
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-x86/isel.c) is                          ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main/vex_util.h"
#include "main/vex_globals.h"
#include "host-generic/h_generic_regs.h"
#include "host-x86/hdefs.h"


/*---------------------------------------------------------*/
/*--- Stuff for pattern matching on IR.  This isn't     ---*/
/*--- x86 specific, and should be moved elsewhere.      ---*/
/*---------------------------------------------------------*/

#define DECLARE_PATTERN(_patt) \
   static IRExpr* _patt = NULL

#define DEFINE_PATTERN(_patt,_expr)                            \
   do {                                                        \
      if (!(_patt)) {                                          \
         vassert(LibVEX_GetAllocMode() == AllocModeTEMPORARY); \
         LibVEX_SetAllocMode(AllocModePERMANENT);              \
         _patt = (_expr);                                      \
         LibVEX_SetAllocMode(AllocModeTEMPORARY);              \
         vassert(LibVEX_GetAllocMode() == AllocModeTEMPORARY); \
      }                                                        \
   } while (0)


#define N_MATCH_BINDERS 4
typedef
   struct {
      IRExpr* bindee[N_MATCH_BINDERS];
   }
   MatchInfo;


static void setBindee ( MatchInfo* mi, Int n, IRExpr* bindee )
{
   if (n < 0 || n >= N_MATCH_BINDERS)
      vpanic("setBindee: out of range index");
   if (mi->bindee[n] != NULL)
      vpanic("setBindee: bindee already set");
   mi->bindee[n] = bindee;
}

static Bool matchWrk ( MatchInfo* mi, IRExpr* p/*attern*/, IRExpr* e/*xpr*/ )
{
   switch (p->tag) {
      case Iex_Binder: /* aha, what we were looking for. */
         setBindee(mi, p->Iex.Binder.binder, e);
         return True;
      case Iex_Unop:
         if (e->tag != Iex_Unop) return False;
         if (p->Iex.Unop.op != e->Iex.Unop.op) return False;
         if (!matchWrk(mi, p->Iex.Unop.arg, e->Iex.Unop.arg))
            return False;
         return True;
      case Iex_Binop:
         if (e->tag != Iex_Binop) return False;
         if (p->Iex.Binop.op != e->Iex.Binop.op) return False;
	 if (!matchWrk(mi, p->Iex.Binop.arg1, e->Iex.Binop.arg1))
            return False;
	 if (!matchWrk(mi, p->Iex.Binop.arg2, e->Iex.Binop.arg2))
            return False;
         return True;
      case Iex_Const:
	if (e->tag != Iex_Const) return False;
	switch (p->Iex.Const.con->tag) {
           case Ico_U8: return e->Iex.Const.con->tag==Ico_U8 
                                  ? (p->Iex.Const.con->Ico.U8 
                                     == e->Iex.Const.con->Ico.U8) 
                                  : False;
           case Ico_U16: return e->Iex.Const.con->tag==Ico_U16 
                                   ? (p->Iex.Const.con->Ico.U16 
                                      == e->Iex.Const.con->Ico.U16) 
                                   : False;
           case Ico_U32: return e->Iex.Const.con->tag==Ico_U32 
                                   ? (p->Iex.Const.con->Ico.U32 
                                      == e->Iex.Const.con->Ico.U32) 
                                   : False;
           case Ico_U64: return e->Iex.Const.con->tag==Ico_U64 
                                   ? (p->Iex.Const.con->Ico.U64 
                                      == e->Iex.Const.con->Ico.U64) 
                                   : False;
           case Ico_F64: return e->Iex.Const.con->tag==Ico_F64 
                                   ? (p->Iex.Const.con->Ico.F64 
                                      == e->Iex.Const.con->Ico.F64) 
                                   : False;
	}
        vpanic("matchIRExpr.Iex_Const");
        /*NOTREACHED*/
      default: 
         ppIRExpr(p);
         vpanic("match");
   }
}

static Bool matchIRExpr ( MatchInfo* mi, IRExpr* p/*attern*/, IRExpr* e/*xpr*/ )
{
   Int i;
   for (i = 0; i < N_MATCH_BINDERS; i++)
      mi->bindee[i] = NULL;
   return matchWrk(mi, p, e);
}

/*-----*/
/* These are duplicated in x86toIR.c */
static IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

static IRExpr* binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   return IRExpr_Binop(op, a1, a2);
}

static IRExpr* bind ( Int binder )
{
   return IRExpr_Binder(binder);
}




/*---------------------------------------------------------*/
/*--- ISelEnv                                           ---*/
/*---------------------------------------------------------*/

/* This carries around:

   - A function for looking up the address of helper functions.

   - A mapping from IRTemp to IRType, giving the type of any IRTemp we
     might encounter.  This is computed before insn selection starts,
     and does not change.

   - A mapping from IRTemp to HReg.  This tells the insn selector
     which virtual register(s) are associated with each IRTemp
     temporary.  This is computed before insn selection starts, and
     does not change.  We expect this mapping to map precisely the
     same set of IRTemps as the type mapping does.

        - vregmap   holds the primary register for the IRTemp.
        - vregmapHI is only used for 64-bit integer-typed
             IRTemps.  It holds the identity of a second
             32-bit virtual HReg, which holds the high half
             of the value.

   - The code array, that is, the insns selected so far.

   - A counter, for generating new virtual registers.

   Note, this is all host-independent.  */

typedef
   struct {
      Addr64       (*find_helper)(Char*);

      IRTypeEnv*   type_env;

      HReg*        vregmap;
      HReg*        vregmapHI;
      Int          n_vregmap;

      HInstrArray* code;

      Int          vreg_ctr;
   }
   ISelEnv;


static HReg lookupIRTemp ( ISelEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   return env->vregmap[tmp];
}

static void lookupIRTemp64 ( HReg* vrHI, HReg* vrLO, ISelEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   vassert(env->vregmapHI[tmp] != INVALID_HREG);
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr ( ISelEnv* env, X86Instr* instr )
{
   addHInstr(env->code, instr);
   if (vex_verbosity > 0) {
      ppX86Instr(instr);
      vex_printf("\n");
   }
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(env->vreg_ctr, HRcInt, True/*virtual reg*/);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF ( ISelEnv* env )
{
   HReg reg = mkHReg(env->vreg_ctr, HRcFloat, True/*virtual reg*/);
   env->vreg_ctr++;
   return reg;
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (32/16/8 bit)           ---*/
/*---------------------------------------------------------*/

/* forwards ... */
static X86RMI*   iselIntExpr_RMI   ( ISelEnv* env, IRExpr* e );
static X86RM*    iselIntExpr_RM    ( ISelEnv* env, IRExpr* e );
static X86AMode* iselIntExpr_AMode ( ISelEnv* env, IRExpr* e );
static void      iselIntExpr64     ( HReg* rHi, HReg* rLo, 
                                     ISelEnv* env, IRExpr* e );
static X86CondCode iselCondCode ( ISelEnv* env, IRExpr* e );


static X86Instr* mk_MOVsd_RR ( HReg src, HReg dst )
{
   vassert(hregClass(src) == HRcInt);
   vassert(hregClass(dst) == HRcInt);
   return X86Instr_Alu32R(Xalu_MOV, X86RMI_Reg(src), dst);
}


/* Select insns for an integer-typed expression, and add them to the
   code list.  Return a reg holding the result.  This reg may be
   either a real or virtual reg; you get no guarantees.  THE RETURNED
   REG MUST NOT BE MODIFIED.  If you want to modify it, ask for a new
   vreg, copy it in there, and modify the copy.  The register
   allocator will do its best to map both vregs to the same real
   register, so the copies will often disappear later in the game.

   This should handle expressions of 32, 16 and 8-bit type.  All
   results are returned in a 32-bit register.  For 16- and 8-bit
   expressions, the upper 16/24 bits are arbitrary, so you should
   mask or sign extend partial values if necessary.  
*/
static HReg iselIntExpr_R ( ISelEnv* env, IRExpr* e )
{
   MatchInfo mi;
   DECLARE_PATTERN(p_32to1_then_1Uto8);

   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || Ity_I16 || Ity_I8);

   switch (e->tag) {

   /* --------- TEMP --------- */
   case Iex_Tmp: {
      return lookupIRTemp(env, e->Iex.Tmp.tmp);
   }

   /* --------- LOAD --------- */
   case Iex_LDle: {
      HReg dst = newVRegI(env);
      X86AMode* amode = iselIntExpr_AMode ( env, e->Iex.LDle.addr );
      if (ty == Ity_I32) {
         addInstr(env, X86Instr_Alu32R(Xalu_MOV,
                                       X86RMI_Mem(amode), dst) );
         return dst;
      }
      if (ty == Ity_I16) {
         addInstr(env, X86Instr_LoadEX(2,False,amode,dst));
         return dst;
      }
      if (ty == Ity_I8) {
         addInstr(env, X86Instr_LoadEX(1,False,amode,dst));
         return dst;
      }
      break;
   }

   /* --------- BINARY OP --------- */
   case Iex_Binop: {
      X86AluOp   aluOp;
      X86ShiftOp shOp;

#if 0
      { DECLARE_PATTERN(p_rol32);
        DEFINE_PATTERN(p_rol32,
           binop(Iop_Or32,
                 binop(Iop_Shl32,bind(0),bind(1)),
                 binop(Iop_Shr32,
                       bind(2),
                       binop(Iop_Sub8,IRConst_U8(32),bind(3)))));
        if (matchIRExpr(&mi,p_rol32,e)
            && eqIRExpr(mi.bindee[0], mi.bindee[2])
            && eqIRExpr(mi.bindee[1], mi.bindee[3])) {
           /* emit roll */
        }
      }
#endif

      /* Is it an addition or logical style op? */
      switch (e->Iex.Binop.op) {
         case Iop_Add8: case Iop_Add16: case Iop_Add32:
            aluOp = Xalu_ADD; break;
         case Iop_Sub8: case Iop_Sub16: case Iop_Sub32: 
            aluOp = Xalu_SUB; break;
         case Iop_And8: case Iop_And16: case Iop_And32: 
            aluOp = Xalu_AND; break;
         case Iop_Or8: case Iop_Or16: case Iop_Or32:  
            aluOp = Xalu_OR; break;
         case Iop_Xor8: case Iop_Xor16: case Iop_Xor32: 
            aluOp = Xalu_XOR; break;
         case Iop_Mul16: case Iop_Mul32: 
            aluOp = Xalu_MUL; break;
         default:
            aluOp = Xalu_INVALID; break;
      }
      /* For commutative ops we assume any literal
         values are on the second operand. */
      if (aluOp != Xalu_INVALID) {
         HReg dst    = newVRegI(env);
         HReg reg    = iselIntExpr_R(env, e->Iex.Binop.arg1);
         X86RMI* rmi = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         addInstr(env, mk_MOVsd_RR(reg,dst));
         addInstr(env, X86Instr_Alu32R(aluOp, rmi, dst));
         return dst;
      }

      /* Perhaps a shift op? */
      switch (e->Iex.Binop.op) {
         case Iop_Shl32: case Iop_Shl16: case Iop_Shl8:
            shOp = Xsh_SHL; break;
         case Iop_Shr32: case Iop_Shr16: case Iop_Shr8: 
            shOp = Xsh_SHR; break;
         case Iop_Sar32: case Iop_Sar16: case Iop_Sar8: 
            shOp = Xsh_SAR; break;
         default:
            shOp = Xsh_INVALID; break;
      }
      if (shOp != Xsh_INVALID) {
         HReg dst = newVRegI(env);

         /* regL = the value to be shifted */
         HReg regL   = iselIntExpr_R(env, e->Iex.Binop.arg1);
         addInstr(env, mk_MOVsd_RR(regL,dst));

         /* Do any necessary widening for 16/8 bit operands */
         switch (e->Iex.Binop.op) {
            case Iop_Shr8:
               addInstr(env, X86Instr_Alu32R(
                                Xalu_AND, X86RMI_Imm(0xFF), dst));
               break;
            case Iop_Shr16:
               addInstr(env, X86Instr_Alu32R(
                                Xalu_AND, X86RMI_Imm(0xFFFF), dst));
               break;
            case Iop_Sar8:
               addInstr(env, X86Instr_Sh32(Xsh_SHL, 24, X86RM_Reg(dst)));
               addInstr(env, X86Instr_Sh32(Xsh_SAR, 24, X86RM_Reg(dst)));
               break;
            case Iop_Sar16:
               addInstr(env, X86Instr_Sh32(Xsh_SHL, 16, X86RM_Reg(dst)));
               addInstr(env, X86Instr_Sh32(Xsh_SAR, 16, X86RM_Reg(dst)));
               break;
            default: break;
         }

         /* Now consider the shift amount.  If it's a literal, we
            can do a much better job than the general case. */
         if (e->Iex.Binop.arg2->tag == Iex_Const) {
            /* assert that the IR is well-typed */
            Int nshift;
            vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
            nshift = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
	    vassert(nshift >= 0);
	    if (nshift > 0)
               /* Can't allow nshift==0 since that means %cl */
               addInstr(env, X86Instr_Sh32(
                                shOp, 
                                nshift,
                                X86RM_Reg(dst)));
         } else {
            /* General case; we have to force the amount into %cl. */
            HReg regR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, mk_MOVsd_RR(regR,hregX86_ECX()));
            addInstr(env, X86Instr_Sh32(shOp, 0/* %cl */, X86RM_Reg(dst)));
         }
         return dst;
      }

      /* Handle misc other ops. */
      if (e->Iex.Binop.op == Iop_16HLto32) {
         HReg hi16  = newVRegI(env);
         HReg lo16  = newVRegI(env);
         HReg hi16s = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg lo16s = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_MOVsd_RR(hi16s, hi16));
         addInstr(env, mk_MOVsd_RR(lo16s, lo16));
         addInstr(env, X86Instr_Sh32(Xsh_SHL, 16, X86RM_Reg(hi16)));
         addInstr(env, X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(0xFFFF), lo16));
         addInstr(env, X86Instr_Alu32R(Xalu_OR, X86RMI_Reg(lo16), hi16));
         return hi16;
      }

      if (e->Iex.Binop.op == Iop_MullS16 || e->Iex.Binop.op == Iop_MullS8
          || e->Iex.Binop.op == Iop_MullU16 || e->Iex.Binop.op == Iop_MullU8) {
         HReg a16   = newVRegI(env);
         HReg b16   = newVRegI(env);
         HReg a16s  = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg b16s  = iselIntExpr_R(env, e->Iex.Binop.arg2);
         Int  shift = (e->Iex.Binop.op == Iop_MullS8 
                       || e->Iex.Binop.op == Iop_MullU8)
                         ? 24 : 16;
         X86ShiftOp shr_op = (e->Iex.Binop.op == Iop_MullS8 
                              || e->Iex.Binop.op == Iop_MullS16)
                                ? Xsh_SAR : Xsh_SHR;

         addInstr(env, mk_MOVsd_RR(a16s, a16));
         addInstr(env, mk_MOVsd_RR(b16s, b16));
         addInstr(env, X86Instr_Sh32(Xsh_SHL, shift, X86RM_Reg(a16)));
         addInstr(env, X86Instr_Sh32(Xsh_SHL, shift, X86RM_Reg(b16)));
         addInstr(env, X86Instr_Sh32(shr_op,  shift, X86RM_Reg(a16)));
         addInstr(env, X86Instr_Sh32(shr_op,  shift, X86RM_Reg(b16)));
         addInstr(env, X86Instr_Alu32R(Xalu_MUL, X86RMI_Reg(a16), b16));
         return b16;
      }

      break;
   }

   /* --------- UNARY OP --------- */
   case Iex_Unop: {
      /* 1Uto8(32to1(expr32)) */
      DEFINE_PATTERN(p_32to1_then_1Uto8,
                     unop(Iop_1Uto8,unop(Iop_32to1,bind(0))));
      if (matchIRExpr(&mi,p_32to1_then_1Uto8,e)) {
         IRExpr* expr32 = mi.bindee[0];
         HReg dst = newVRegI(env);
         HReg src = iselIntExpr_R(env, expr32);
         addInstr(env, mk_MOVsd_RR(src,dst) );
         addInstr(env, X86Instr_Alu32R(Xalu_AND,
                                       X86RMI_Imm(1), dst));
         return dst;
      }

      switch (e->Iex.Unop.op) {
         case Iop_8Uto32:
         case Iop_16Uto32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            UInt mask = e->Iex.Unop.op==Iop_8Uto32 ? 0xFF : 0xFFFF;
            addInstr(env, mk_MOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Alu32R(Xalu_AND,
                                          X86RMI_Imm(mask), dst));
            return dst;
         }
         case Iop_8Sto32:
         case Iop_16Sto32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            UInt amt = e->Iex.Unop.op==Iop_8Sto32 ? 24 : 16;
            addInstr(env, mk_MOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Sh32(Xsh_SHL, amt, X86RM_Reg(dst)));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, amt, X86RM_Reg(dst)));
            return dst;
         }
	 case Iop_Not8:
	 case Iop_Not16:
         case Iop_Not32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_MOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Unary32(Xun_Not,X86RM_Reg(dst)));
            return dst;
         }
         case Iop_64HIto32: {
            HReg rHi, rLo;
            iselIntExpr64(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rHi; /* and abandon rLo .. poor wee thing :-) */
         }
         case Iop_64to32: {
            HReg rHi, rLo;
            iselIntExpr64(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rLo; /* similar stupid comment to the above ... */
         }
         case Iop_16HIto8:
         case Iop_32HIto16: {
            HReg dst  = newVRegI(env);
            HReg src  = iselIntExpr_R(env, e->Iex.Unop.arg);
            Int shift = e->Iex.Unop.op == Iop_16HIto8 ? 8 : 16;
            addInstr(env, mk_MOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Sh32(Xsh_SHR, shift, X86RM_Reg(dst)));
            return dst;
         }
         case Iop_1Uto32:
         case Iop_1Uto8: {
            HReg dst         = newVRegI(env);
            X86CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Set32(cond,dst));
            return dst;
         }
         case Iop_16to8:
         case Iop_32to8:
         case Iop_32to16:
            /* These are no-ops. */
            return iselIntExpr_R(env, e->Iex.Unop.arg);

         default: 
            break;
      }
      break;
   }

   /* --------- GET --------- */
   case Iex_Get: {
      if (ty == Ity_I32) {
         HReg dst = newVRegI(env);
         addInstr(env, X86Instr_Alu32R(
                          Xalu_MOV, 
                          X86RMI_Mem(X86AMode_IR(e->Iex.Get.offset,
                                                 hregX86_EBP())),
                          dst));
         return dst;
      }
      if (ty == Ity_I8 || ty == Ity_I16) {
         HReg dst = newVRegI(env);
         addInstr(env, X86Instr_LoadEX(
                          ty==Ity_I8 ? 1 : 2,
                          False,
                          X86AMode_IR(e->Iex.Get.offset,hregX86_EBP()),
                          dst));
         return dst;
      }
      break;
   }

   /* --------- CCALL --------- */
   case Iex_CCall: {
      Addr64 helper;
      Int    i, nargs;
      UInt   target;
      IRExpr* arg;
      vassert(ty == Ity_I32);
      /* be very restrictive for now.  Only 32-bit ints allowed
         for args and return type. */
      if (e->Iex.CCall.retty != Ity_I32)
         break;
      /* push args on the stack, right to left. */
      nargs = 0;
      while (e->Iex.CCall.args[nargs]) nargs++;
      for (i = nargs-1; i >= 0; i--) {
         arg = e->Iex.CCall.args[i];
         if (typeOfIRExpr(env->type_env,arg) != Ity_I32)
            goto irreducible;
         addInstr(env, X86Instr_Push(iselIntExpr_RMI(env, arg)));
      }
      /* Find the function to call.  Since the host -- for which we
         are generating code -- is a 32-bit machine (x86) -- the upper
         32 bit of the helper address should be zero. */
      helper = env->find_helper(e->Iex.CCall.name);
      vassert((helper & 0xFFFFFFFF00000000LL) == 0);
      target = helper & 0xFFFFFFFF;
      addInstr(env, X86Instr_Alu32R(
                       Xalu_MOV,
                       X86RMI_Imm(target),
                       hregX86_EAX()));
      addInstr(env, X86Instr_Call(hregX86_EAX()));
      if (nargs > 0)
         addInstr(env, X86Instr_Alu32R(Xalu_ADD,
                          X86RMI_Imm(4*nargs),
                          hregX86_ESP()));
      return hregX86_EAX();
   }

   /* --------- LITERAL --------- */
   /* 32/16/8-bit literals */
   case Iex_Const: {
      X86RMI* rmi = iselIntExpr_RMI ( env, e );
      HReg    r   = newVRegI(env);
      addInstr(env, X86Instr_Alu32R(Xalu_MOV, rmi, r));
      return r;
   }

   /* --------- MULTIPLEX --------- */
   case Iex_Mux0X: {
      if (ty == Ity_I32 
         && typeOfIRExpr(env->type_env,e->Iex.Mux0X.cond) == Ity_I8) {
        HReg r8;
        HReg rX   = iselIntExpr_R(env, e->Iex.Mux0X.exprX);
        X86RM* r0 = iselIntExpr_RM(env, e->Iex.Mux0X.expr0);
        HReg dst = newVRegI(env);
        addInstr(env, mk_MOVsd_RR(rX,dst));
        r8 = iselIntExpr_R(env, e->Iex.Mux0X.cond);
        addInstr(env, X86Instr_Test32(X86RI_Imm(0xFF), X86RM_Reg(r8)));
        addInstr(env, X86Instr_CMov32(Xcc_Z,r0,dst));
        return dst;
      }
      break;
   }

   default: 
   break;
   } /* switch (e->tag) */

   /* We get here if no pattern matched. */
  irreducible:
   ppIRExpr(e);
   vpanic("iselIntExpr_R: cannot reduce tree");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expression auxiliaries              ---*/
/*---------------------------------------------------------*/

/* --------------- AMODEs --------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a 32-bit one.
*/
static X86AMode* iselIntExpr_AMode ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32);

   /* Add32(expr1, Shl32(expr2, imm)) */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg2->tag == Iex_Binop
       && e->Iex.Binop.arg2->Iex.Binop.op == Iop_Shl32
       && e->Iex.Binop.arg2->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32) {
      UInt shift = e->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
      if (shift == 2 || shift == 4 || shift == 8) {
         HReg r1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg r2 = iselIntExpr_R(env, e->Iex.Binop.arg2->Iex.Binop.arg1 );
         return X86AMode_IRRS(0, r1, r2, shift);
      }
   }

   /* Add32(expr,i) */
   if (e->tag == Iex_Binop 
       && e->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32) {
      HReg r1 = iselIntExpr_R(env,  e->Iex.Binop.arg1);
      return X86AMode_IR(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32, r1);
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg r1 = iselIntExpr_R(env, e);
      return X86AMode_IR(0, r1);
   }
}


/* --------------- RMIs --------------- */

/* Similarly, calculate an expression into an X86RMI operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits.  */

static X86RMI* iselIntExpr_RMI ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      UInt u;
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & (e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:  u = 0xFF   & (e->Iex.Const.con->Ico.U8); break;
         default: vpanic("iselIntExpr_RMI.Iex_Const(x86h)");
      }
      return X86RMI_Imm(u);
   }

   /* special case: 32-bit GET */
   if (e->tag == Iex_Get && ty == Ity_I32) {
      return X86RMI_Mem(X86AMode_IR(e->Iex.Get.offset,
                                    hregX86_EBP()));
   }

   /* special case: load from memory */

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return X86RMI_Reg(r);
   }
}


/* --------------- RIs --------------- */

/* Calculate an expression into an X86RI operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits. */

static X86RI* iselIntExpr_RI ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      UInt u;
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & (e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:  u = 0xFF   & (e->Iex.Const.con->Ico.U8); break;
         default: vpanic("iselIntExpr_RMI.Iex_Const(x86h)");
      }
      return X86RI_Imm(u);
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return X86RI_Reg(r);
   }
}


/* --------------- RMs --------------- */

/* Similarly, calculate an expression into an X86RM operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits.  */

static X86RM* iselIntExpr_RM ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   /* special case: 32-bit GET */
   if (e->tag == Iex_Get && ty == Ity_I32) {
      return X86RM_Mem(X86AMode_IR(e->Iex.Get.offset,
                                   hregX86_EBP()));
   }

   /* special case: load from memory */

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return X86RM_Reg(r);
   }
}


/* --------------- CONDCODE --------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static X86CondCode iselCondCode ( ISelEnv* env, IRExpr* e )
{
   MatchInfo mi;
   DECLARE_PATTERN(p_32to1);
   DECLARE_PATTERN(p_eq32_literal);
   DECLARE_PATTERN(p_ne32_zero);
   DECLARE_PATTERN(p_1Uto32_then_32to1);

   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_Bit);

   /* 32to1(1Uto32(expr1)) -- the casts are pointless, ignore them */
   DEFINE_PATTERN(p_1Uto32_then_32to1,
                  unop(Iop_32to1,unop(Iop_1Uto32,bind(0))));
   if (matchIRExpr(&mi,p_1Uto32_then_32to1,e)) {
      IRExpr* expr1 = mi.bindee[0];
      return iselCondCode(env, expr1);
   }

   /* pattern: 32to1(expr32) */
   DEFINE_PATTERN(p_32to1, 
      unop(Iop_32to1,bind(0))
   );
   if (matchIRExpr(&mi,p_32to1,e)) {
      X86RM* rm = iselIntExpr_RM(env, mi.bindee[0]);
      addInstr(env, X86Instr_Test32(X86RI_Imm(1),rm));
      return Xcc_NZ;
   }

   /* CmpEQ8 / CmpNE8 */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ8
           || e->Iex.Binop.op == Iop_CmpNE8)) {
      HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      X86RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      HReg    r    = newVRegI(env);
      addInstr(env, mk_MOVsd_RR(r1,r));
      addInstr(env, X86Instr_Alu32R(Xalu_XOR,rmi2,r));
      addInstr(env, X86Instr_Alu32R(Xalu_AND,X86RMI_Imm(0xFF),r));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ8:  return Xcc_Z;
         case Iop_CmpNE8:  return Xcc_NZ;
         default: vpanic("iselCondCode(x86): CmpXX8");
      }
   }

   /* CmpEQ16 / CmpNE16 */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ16
           || e->Iex.Binop.op == Iop_CmpNE16)) {
      HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      X86RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      HReg    r    = newVRegI(env);
      addInstr(env, mk_MOVsd_RR(r1,r));
      addInstr(env, X86Instr_Alu32R(Xalu_XOR,rmi2,r));
      addInstr(env, X86Instr_Alu32R(Xalu_AND,X86RMI_Imm(0xFFFF),r));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ16:  return Xcc_Z;
         case Iop_CmpNE16:  return Xcc_NZ;
         default: vpanic("iselCondCode(x86): CmpXX8");
      }
   }

   /* Cmp*32*(x,y) */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ32
           || e->Iex.Binop.op == Iop_CmpNE32
           || e->Iex.Binop.op == Iop_CmpLT32S
           || e->Iex.Binop.op == Iop_CmpLT32U
           || e->Iex.Binop.op == Iop_CmpLE32S
           || e->Iex.Binop.op == Iop_CmpLE32U)) {
      HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      X86RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      addInstr(env, X86Instr_Alu32R(Xalu_CMP,rmi2,r1));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ32:  return Xcc_Z;
         case Iop_CmpNE32:  return Xcc_NZ;
         case Iop_CmpLT32S: return Xcc_L;
         case Iop_CmpLT32U: return Xcc_B;
         case Iop_CmpLE32S: return Xcc_LE;
         case Iop_CmpLE32U: return Xcc_BE;
         default: vpanic("iselCondCode(x86): CmpXX32");
      }
   }

   /* var */
   if (e->tag == Iex_Tmp) {
      HReg r32 = lookupIRTemp(env, e->Iex.Tmp.tmp);
      HReg dst = newVRegI(env);
      addInstr(env, mk_MOVsd_RR(r32,dst));
      addInstr(env, X86Instr_Alu32R(Xalu_AND,X86RMI_Imm(1),dst));
      return Xcc_NZ;
   }

   ppIRExpr(e);
   vpanic("iselCondCode");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64 bit)                ---*/
/*---------------------------------------------------------*/

/* Compute a 64-bit value into a register pair, which is returned as
   the first two parameters.  As with iselIntExpr_R, these may be
   either real or virtual regs; in any case they must not be changed
   by subsequent code emitted by the caller.  */

static void iselIntExpr64 ( HReg* rHi, HReg* rLo, ISelEnv* env, IRExpr* e )
{
  //   MatchInfo mi;
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I64);

   if (e->tag == Iex_Const) {
      ULong w64 = e->Iex.Const.con->Ico.U64;
      UInt  wHi = ((UInt)(w64 >> 32)) & 0xFFFFFFFF;
      UInt  wLo = ((UInt)w64) & 0xFFFFFFFF;
      HReg  tLo = newVRegI(env);
      HReg  tHi = newVRegI(env);
      vassert(e->Iex.Const.con->tag == Ico_U64);
      addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(wHi), tHi));
      addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(wLo), tLo));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* read 64-bit IRTemp */
   if (e->tag == Iex_Tmp) {
      lookupIRTemp64( rHi, rLo, env, e->Iex.Tmp.tmp);
      return;
   }

   /* 32 x 32 -> 64 multiply */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_MullU32
           || e->Iex.Binop.op == Iop_MullS32)) {
      /* get one operand into %eax, and the other into a R/M.  Need to
         make an educated guess about which is better in which. */
      Bool   syned  = e->Iex.Binop.op == Iop_MullS32;
      X86RM* rmLeft = iselIntExpr_RM(env, e->Iex.Binop.arg1);
      HReg   rRight = iselIntExpr_R(env, e->Iex.Binop.arg2);
      addInstr(env, mk_MOVsd_RR(rRight, hregX86_EAX()));
      addInstr(env, X86Instr_MulL(syned, Xss_32, rmLeft));
      /* Result is now in EDX:EAX.  Tell the caller. */
      *rHi = hregX86_EDX();
      *rLo = hregX86_EAX();
      return;
   }

   /* 64 x 32 -> (32(rem),32(div)) division */
   if (e->tag == Iex_Binop
      && (e->Iex.Binop.op == Iop_DivModU64to32
          || e->Iex.Binop.op == Iop_DivModS64to32)) {
      /* Get the 64-bit operand into edx:eax, and the other
         into any old R/M. */
      HReg sHi, sLo;
      Bool   syned   = e->Iex.Binop.op == Iop_DivModS64to32;
      X86RM* rmRight = iselIntExpr_RM(env, e->Iex.Binop.arg2);
      iselIntExpr64(&sHi,&sLo, env, e->Iex.Binop.arg1);
      addInstr(env, mk_MOVsd_RR(sHi, hregX86_EDX()));
      addInstr(env, mk_MOVsd_RR(sLo, hregX86_EAX()));
      addInstr(env, X86Instr_Div(syned, Xss_32, rmRight));
      *rHi = hregX86_EDX();
      *rLo = hregX86_EAX();
      return;
   }

   /* 32HLto64(e1,e2) */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_32HLto64) {
      *rHi = iselIntExpr_R(env, e->Iex.Binop.arg1);
      *rLo = iselIntExpr_R(env, e->Iex.Binop.arg2);
      return;
   }

   /* 32Sto64(e) */
   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_32Sto64) {
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, mk_MOVsd_RR(src,tHi));
      addInstr(env, mk_MOVsd_RR(src,tLo));
      addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, X86RM_Reg(tHi)));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit shifts */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Shl64) {
      /* We use the same ingenious scheme as gcc.  Put the value
         to be shifted into %hi:%lo, and the shift amount into %cl.
         Then (dsts on right, a la ATT syntax):
 
            shldl %cl, %lo, %hi   -- make %hi be right for the shift amt
                                  -- %cl % 32
            shll  %cl, %lo        -- make %lo be right for the shift amt
                                  -- %cl % 32

         Now, if (shift amount % 64) is in the range 32 .. 63, we have 
         to do a fixup, which puts the result low half into the result
         high half, and zeroes the low half:

            testl $32, %ecx

            cmovnz %lo, %hi
            movl $0, %tmp         -- sigh; need yet another reg
            cmovnz %tmp, %lo
      */
      HReg rAmt, sHi, sLo, tHi, tLo, tTemp;
      tLo = newVRegI(env);
      tHi = newVRegI(env);
      tTemp = newVRegI(env);
      rAmt = iselIntExpr_R(env, e->Iex.Binop.arg2);
      iselIntExpr64(&sHi,&sLo, env, e->Iex.Binop.arg1);
      addInstr(env, mk_MOVsd_RR(rAmt, hregX86_ECX()));
      addInstr(env, mk_MOVsd_RR(sHi, tHi));
      addInstr(env, mk_MOVsd_RR(sLo, tLo));
      /* Ok.  Now shift amt is in %ecx, and value is in tHi/tLo and
         those regs are legitimately modifiable. */
      addInstr(env, X86Instr_Sh3232(Xsh_SHL, 0/*%cl*/, tLo, tHi));
      addInstr(env, X86Instr_Sh32(Xsh_SHL, 0/*%cl*/, X86RM_Reg(tLo)));
      addInstr(env, X86Instr_Test32(X86RI_Imm(32), X86RM_Reg(hregX86_ECX())));
      addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tLo), tHi));
      addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tTemp));
      addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tTemp), tLo));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Shr64) {
      /* We use the same ingenious scheme as gcc.  Put the value
         to be shifted into %hi:%lo, and the shift amount into %cl.
         Then:
 
            shrdl %cl, %hi, %lo   -- make %lo be right for the shift amt
                                  -- %cl % 32
            shrl  %cl, %hi        -- make %hi be right for the shift amt
                                  -- %cl % 32

         Now, if (shift amount % 64) is in the range 32 .. 63, we have 
         to do a fixup, which puts the result high half into the result
         low half, and zeroes the high half:

            testl $32, %ecx

            cmovnz %hi, %lo
            movl $0, %tmp         -- sigh; need yet another reg
            cmovnz %tmp, %hi
      */
      HReg rAmt, sHi, sLo, tHi, tLo, tTemp;
      tLo = newVRegI(env);
      tHi = newVRegI(env);
      tTemp = newVRegI(env);
      rAmt = iselIntExpr_R(env, e->Iex.Binop.arg2);
      iselIntExpr64(&sHi,&sLo, env, e->Iex.Binop.arg1);
      addInstr(env, mk_MOVsd_RR(rAmt, hregX86_ECX()));
      addInstr(env, mk_MOVsd_RR(sHi, tHi));
      addInstr(env, mk_MOVsd_RR(sLo, tLo));
      /* Ok.  Now shift amt is in %ecx, and value is in tHi/tLo and
         those regs are legitimately modifiable. */
      addInstr(env, X86Instr_Sh3232(Xsh_SHR, 0/*%cl*/, tHi, tLo));
      addInstr(env, X86Instr_Sh32(Xsh_SHR, 0/*%cl*/, X86RM_Reg(tHi)));
      addInstr(env, X86Instr_Test32(X86RI_Imm(32), X86RM_Reg(hregX86_ECX())));
      addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tHi), tLo));
      addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tTemp));
      addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tTemp), tHi));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   ppIRExpr(e);
   vpanic("iselIntExpr64");
}



/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (64 bit)         ---*/
/*---------------------------------------------------------*/

/* Compute a 64-bit floating point value into a register, the identity
   of which is returned.  As with iselIntExpr_R, the reg may be either
   real or virtual; in any case it must not be changed by subsequent
   code emitted by the caller.  */

static HReg iselDblExpr ( ISelEnv* env, IRExpr* e )
{
  //   MatchInfo mi;
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_F64);

   if (e->tag == Iex_Tmp) {
      return lookupIRTemp(env, e->Iex.Tmp.tmp);
   }

   if (e->tag == Iex_Const) {
      union { UInt i64[2]; Double f64; } u;
      HReg freg = newVRegF(env);
      vassert(sizeof(u) == 8);
      vassert(sizeof(u.i64) == 8);
      vassert(sizeof(u.f64) == 8);
      vassert(e->Iex.Const.con->tag == Ico_F64);
      u.f64 = e->Iex.Const.con->Ico.F64;
      addInstr(env, X86Instr_Push(X86RMI_Imm(u.i64[1])));
      addInstr(env, X86Instr_Push(X86RMI_Imm(u.i64[0])));
      addInstr(env, X86Instr_FpLdSt(True/*load*/, 8, freg, 
                                    X86AMode_IR(0, hregX86_ESP())));
      addInstr(env, X86Instr_Alu32R(Xalu_ADD,
                                    X86RMI_Imm(8),
                                    hregX86_ESP()));
      return freg;
   }

   if (e->tag == Iex_LDle) {
      X86AMode* am;
      HReg res = newVRegF(env);
      vassert(e->Iex.LDle.ty == Ity_F64);
      am = iselIntExpr_AMode(env, e->Iex.LDle.addr);
      addInstr(env, X86Instr_FpLdSt(True/*load*/, 8, res, am));
      return res;
   }

   if (e->tag == Iex_GetI) {
      /* First off, compute the index expression into an integer reg.
         The written address will then be 0 + ebp + reg*1, that is, an
         X86AMode_IRRS. */
      HReg idx = iselIntExpr_R(env, e->Iex.GetI.offset);
      HReg res = newVRegF(env);
      addInstr(env, 
               X86Instr_FpLdSt( True/*load*/, 8, res,
                                X86AMode_IRRS(0, hregX86_EBP(), idx, 0)) );
      return res;
   }

   if (e->tag == Iex_Binop) {
      X86FpOp fpop = Xfp_INVALID;
      switch (e->Iex.Binop.op) {
         case Iop_AddF64: fpop = Xfp_ADD; break;
         case Iop_MulF64: fpop = Xfp_MUL; break;
         default: break;
      }
      if (fpop != Xfp_INVALID) {
         HReg res  = newVRegF(env);
         HReg srcL = iselDblExpr(env, e->Iex.Binop.arg1);
         HReg srcR = iselDblExpr(env, e->Iex.Binop.arg2);
         addInstr(env, X86Instr_FpBinary(fpop,srcL,srcR,res));
         return res;
      }
   }

   if (e->tag == Iex_Unop) {
      if (e->Iex.Unop.op == Iop_I64toF64) {
         HReg iHi, iLo;
         HReg dst = newVRegF(env);
         iselIntExpr64(&iHi, &iLo, env, e->Iex.Unop.arg);
         addInstr(env, X86Instr_FpI64(False/*i->f*/,dst,iHi,iLo));
         return dst;
      }
   }

   ppIRExpr(e);
   vpanic("iselDblExpr");
}



/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void iselStmt ( ISelEnv* env, IRStmt* stmt )
{
   if (vex_verbosity > 0) {
      vex_printf("-- ");
      ppIRStmt(stmt);
      vex_printf("\n");
   }

   switch (stmt->tag) {

   /* --------- STORE --------- */
   case Ist_STle: {
      X86AMode* am;
      IRType tya = typeOfIRExpr(env->type_env, stmt->Ist.STle.addr);
      IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.STle.data);
      vassert(tya == Ity_I32);
      am = iselIntExpr_AMode(env, stmt->Ist.STle.addr);
      if (tyd == Ity_I32) {
         X86RI* ri = iselIntExpr_RI(env, stmt->Ist.STle.data);
         addInstr(env, X86Instr_Alu32M(Xalu_MOV,ri,am));
         return;
      }
      if (tyd == Ity_I8 || tyd == Ity_I16) {
         HReg r = iselIntExpr_R(env, stmt->Ist.STle.data);
         addInstr(env, X86Instr_Store(tyd==Ity_I8 ? 1 : 2,
                                      r,am));
         return;
      }
      if (tyd == Ity_F64) {
         HReg r = iselDblExpr(env, stmt->Ist.STle.data);
         addInstr(env, X86Instr_FpLdSt(False/*store*/, 8, r, am));
         return;
      }
      break;
   }

   /* --------- PUT --------- */
   case Ist_Put: {
      IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.expr);
      if (ty == Ity_I32) {
         /* We're going to write to memory, so compute the RHS into an
            X86RI. */
         X86RI* ri  = iselIntExpr_RI(env, stmt->Ist.Put.expr);
         addInstr(env,
                  X86Instr_Alu32M(
                     Xalu_MOV,
                     ri,
                     X86AMode_IR(stmt->Ist.Put.offset,hregX86_EBP())
                 ));
         return;
      }
      if (ty == Ity_I8 || ty == Ity_I16) {
         HReg r = iselIntExpr_R(env, stmt->Ist.Put.expr);
         addInstr(env, X86Instr_Store(
                          ty==Ity_I8 ? 1 : 2,
                          r,
                          X86AMode_IR(stmt->Ist.Put.offset,
                                      hregX86_EBP())));
         return;
      }
      break;
   }

   /* --------- Indexed PUT --------- */
   case Ist_PutI: {
      /* First off, compute the index expression into an integer reg.
         The written address will then be 0 + ebp + reg*1, that is, an
         X86AMode_IRRS. */
      HReg idx = iselIntExpr_R(env, stmt->Ist.PutI.offset);

      IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.PutI.expr);
      if (ty == Ity_F64) {
         HReg val = iselDblExpr(env, stmt->Ist.PutI.expr);
         addInstr(env, 
            X86Instr_FpLdSt( False/*store*/, 8, val,
                             X86AMode_IRRS(0, hregX86_EBP(), idx, 0)) );
         return;
      }
      break;
   }

   /* --------- TMP --------- */
   case Ist_Tmp: {
      IRTemp tmp = stmt->Ist.Tmp.tmp;
      IRType ty = lookupIRTypeEnv(env->type_env, tmp);
      if (ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
         X86RMI* rmi = iselIntExpr_RMI(env, stmt->Ist.Tmp.expr);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, X86Instr_Alu32R(Xalu_MOV,rmi,dst));
         return;
      }
      if (ty == Ity_I64) {
         HReg rHi, rLo, dstHi, dstLo;
         iselIntExpr64(&rHi,&rLo, env, stmt->Ist.Tmp.expr);
         lookupIRTemp64( &dstHi, &dstLo, env, tmp);
         addInstr(env, mk_MOVsd_RR(rHi,dstHi) );
         addInstr(env, mk_MOVsd_RR(rLo,dstLo) );
         return;
      }
      if (ty == Ity_Bit) {
         X86CondCode cond = iselCondCode(env, stmt->Ist.Tmp.expr);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, X86Instr_Set32(cond, dst));
         return;
      }
      if (ty == Ity_F64) {
         HReg dst = lookupIRTemp(env, tmp);
         HReg src = iselDblExpr(env, stmt->Ist.Tmp.expr);
         addInstr(env, X86Instr_FpUnary(Xfp_MOV,src,dst));
         return;
      }
      break;
   }

   /* --------- EXIT --------- */
   case Ist_Exit: {
      X86RI*      dst;
      X86CondCode cc;
      if (stmt->Ist.Exit.dst->tag != Ico_U32)
         vpanic("isel_x86: Ist_Exit: dst is not a 32-bit value");
      dst = iselIntExpr_RI(env, IRExpr_Const(stmt->Ist.Exit.dst));
      cc  = iselCondCode(env,stmt->Ist.Exit.cond);
      addInstr(env, X86Instr_Goto(Ijk_Boring, cc, dst));
      return;
   }

   default: break;
   }
   ppIRStmt(stmt);
   vpanic("iselStmt");
}


/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

static void iselNext ( ISelEnv* env, IRExpr* next, IRJumpKind jk )
{
   X86RI* ri;
   if (vex_verbosity > 0) {
      vex_printf("-- goto {");
      ppIRJumpKind(jk);
      vex_printf("} ");
      ppIRExpr(next);
      vex_printf("\n");
   }
   ri = iselIntExpr_RI(env, next);
   addInstr(env, X86Instr_Goto(jk, Xcc_ALWAYS,ri));
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire BB to x86 code. */

HInstrArray* iselBB_X86 ( IRBB* bb, Addr64(*find_helper)(Char*) )
{
   Int     i, j;
   HReg    hreg, hregHI;

   /* Make up an initial environment to use. */
   ISelEnv* env = LibVEX_Alloc(sizeof(ISelEnv));
   env->vreg_ctr = 0;

   /* Register helper-function-finder. */
   env->find_helper = find_helper;

   /* Set up output code array. */
   env->code = newHInstrArray();

   /* Copy BB's type env. */
   env->type_env = bb->tyenv;

   /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
      change as we go along. */
   env->n_vregmap = bb->tyenv->types_used;
   env->vregmap   = LibVEX_Alloc(env->n_vregmap * sizeof(HReg));
   env->vregmapHI = LibVEX_Alloc(env->n_vregmap * sizeof(HReg));

   /* For each IR temporary, allocate a suitably-kinded virtual
      register. */
   j = 0;
   for (i = 0; i < env->n_vregmap; i++) {
      hregHI = hreg = INVALID_HREG;
      switch (bb->tyenv->types[i]) {
         case Ity_Bit:
         case Ity_I8:
         case Ity_I16:
         case Ity_I32: hreg   = mkHReg(j++, HRcInt, True); break;
         case Ity_I64: hreg   = mkHReg(j++, HRcInt, True);
                       hregHI = mkHReg(j++, HRcInt, True); break;
         case Ity_F64: hreg   = mkHReg(j++, HRcFloat, True); break;
         default: ppIRType(bb->tyenv->types[i]);
                  vpanic("iselBB: IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* Ok, finally we can iterate over the statements. */
   for (i = 0; i < bb->stmts_used; i++)
      if (bb->stmts[i])
         iselStmt(env,bb->stmts[i]);

   iselNext(env,bb->next,bb->jumpkind);

   /* record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;
   return env->code;
}


/*---------------------------------------------------------------*/
/*--- end                                     host-x86/isel.c ---*/
/*---------------------------------------------------------------*/
