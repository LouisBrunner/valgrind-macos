
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (isel_x86.c) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "vex_util.h"
#include "host_regs.h"
#include "x86h_defs.h"


/*---------------------------------------------------------*/
/*--- ISelEnv                                           ---*/
/*---------------------------------------------------------*/

/* This carries around:

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

static void lookupIRTemp64 ( ISelEnv* env, IRTemp tmp, HReg* vrHI, HReg* vrLO )
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr ( ISelEnv* env, X86Instr* instr )
{
   addHInstr(env->code, instr);
   ppX86Instr(instr);
   vex_printf("\n");
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(env->vreg_ctr, HRcInt, True/*virtual reg*/);
   env->vreg_ctr++;
   return reg;
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions                         ---*/
/*---------------------------------------------------------*/

/* forwards ... */
static X86RMI* iselIntExpr_RMI ( ISelEnv* env, IRExpr* e );
static X86AMode* iselIntExpr_AMode ( ISelEnv* env, IRExpr* e );


static X86Instr* mk_MOV_RR ( HReg src, HReg dst )
{
   vassert(hregClass(src) == HRcInt);
   vassert(hregClass(dst) == HRcInt);
   return X86Instr_Alu32R(Xalu_MOV, X86RMI_Reg(src), dst);
}


/* Select insns for an integer-typed expression, and add them to the
   code list.  Return a vreg holding the result.  The vreg MUST NOT BE
   MODIFIED.  If you want to modify it, ask for a new vreg, copy it in
   there, and modify the copy.  The register allocator will do its
   best to map both vregs to the same real register, so the copies
   will often disappear later in the game.
*/
static HReg iselIntExpr_R ( ISelEnv* env, IRExpr* e )
{
   vassert(e);
   IRType ty = typeOfIRExpr(env->type_env,e);

   switch (e->tag) {

   case Iex_Tmp: {
      vassert(ty == Ity_I32);
      return lookupIRTemp(env, e->Iex.Tmp.tmp);
   }

   case Iex_LDle: {
      X86AMode* amode = iselIntExpr_AMode ( env, e->Iex.LDle.addr );
      HReg dst = newVRegI(env);
      vassert(ty == Ity_I32);
      addInstr(env, X86Instr_Alu32R(Xalu_MOV,
				    X86RMI_Mem(amode), dst) );
      return dst;
   }

   case Iex_Binop: {
      X86AluOp   aluOp;
      X86ShiftOp shOp;
      vassert(ty == Ity_I32);
      switch (e->Iex.Binop.op) {
         case Iop_Add32: aluOp = Xalu_ADD; break;
         case Iop_Sub32: aluOp = Xalu_SUB; break;
         case Iop_And32: aluOp = Xalu_AND; break;
         case Iop_Or32:  aluOp = Xalu_OR;  break;
         case Iop_Xor32: aluOp = Xalu_XOR; break;
         default:        aluOp = Xalu_INVALID; break;
      }
      /* For commutative ops we assume any literal
         values are on the second operand. */
      if (aluOp != Xalu_INVALID) {
         HReg dst    = newVRegI(env);
         HReg reg    = iselIntExpr_R(env, e->Iex.Binop.arg1);
         X86RMI* rmi = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         addInstr(env, mk_MOV_RR(reg,dst));
         addInstr(env, X86Instr_Alu32R(aluOp, rmi, dst));
         return dst;
      }
      switch (e->Iex.Binop.op) {
         case Iop_Shl32: shOp = Xsh_SHL; break;
         case Iop_Shr32: shOp = Xsh_SHR; break;
         case Iop_Sar32: shOp = Xsh_SAR; break;
         default:        shOp = Xsh_INVALID; break;
      }
      if (shOp != Xsh_INVALID) {
         HReg dst    = newVRegI(env);
         HReg regL   = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg regR   = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_MOV_RR(regL,dst));
         addInstr(env, mk_MOV_RR(regR,hregX86_ECX()));
         addInstr(env, X86Instr_Sh32(shOp, 0/* %cl */, X86RM_Reg(dst)));
         return dst;
      }
      break;
   }

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
      break;
   }

   case Iex_CCall: {
      Int i, nargs;
      UInt target;
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
     target = 0x12345678; //FIND_HELPER(e->Iex.CCall.name);
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

   /* 32/16/8-bit literals */
   case Iex_Const: {
      UInt u;
      vassert(ty == Ity_I32 || ty == Ity_I8);
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & (e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:  u = 0xFF   & (e->Iex.Const.con->Ico.U8); break;
         default: vpanic("iselIntExpr_R.Iex_Const(x86h)");
      }
      HReg r = newVRegI(env);
      addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(u), r));
      return r;
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

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  
*/
static X86AMode* iselIntExpr_AMode ( ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I32);

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


/* Similarly, calculate an expression into an X86RMI operand. */

static X86RMI* iselIntExpr_RMI ( ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I32);

   /* special case: immediate */
   if (e->tag == Iex_Const 
       && e->Iex.Const.con->tag == Ico_U32) {
      return X86RMI_Imm(e->Iex.Const.con->Ico.U32);
   }

   /* special case: 32-bit GET */
   if (e->tag == Iex_Get && e->Iex.Get.ty ==Ity_I32) {
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


/* Calculate an expression into an X86RI operand. */

static X86RI* iselIntExpr_RI ( ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I32);

   /* special case: immediate */
   if (e->tag == Iex_Const 
       && e->Iex.Const.con->tag == Ico_U32) {
      return X86RI_Imm(e->Iex.Const.con->Ico.U32);
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return X86RI_Reg(r);
   }
}


/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void iselStmt ( ISelEnv* env, IRStmt* stmt )
{
   vex_printf("-- ");
   ppIRStmt(stmt);
   vex_printf("\n");

   switch (stmt->tag) {

   case Ist_STle: {
      IRType tya = typeOfIRExpr(env->type_env, stmt->Ist.STle.addr);
      IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.STle.data);
      vassert(tya == Ity_I32);
      if (tyd == Ity_I32) {
         X86AMode* am = iselIntExpr_AMode(env, stmt->Ist.STle.addr);
         X86RI* ri = iselIntExpr_RI(env, stmt->Ist.STle.data);
         addInstr(env, X86Instr_Alu32M(Xalu_MOV,ri,am));
         return;
      }
      break;
   }

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
      break;
   }

   case Ist_Tmp: {
      IRTemp tmp = stmt->Ist.Tmp.tmp;
      IRType ty = lookupIRTypeEnv(env->type_env, tmp);
      if (ty == Ity_I32) {
         X86RMI* rmi = iselIntExpr_RMI(env, stmt->Ist.Tmp.expr);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, X86Instr_Alu32R(Xalu_MOV,rmi,dst));
         return;
      }
      break;
   }

   case Ist_Exit: {
     if (stmt->Ist.Exit.dst->tag != Ico_U32)
        vpanic("isel_x86: Ist_Exit: dst is not a 32-bit value");
     /* For the moment, only handle conditions of the form
        32to1(...). */
     IRExpr* cond = stmt->Ist.Exit.cond;
     if (cond->tag == Iex_Unop && cond->Iex.Unop.op == Iop_32to1) {
        cond = cond->Iex.Unop.arg;
     } else {
        break; /* give up */
     }
     HReg reg = iselIntExpr_R( env, cond );
     /* Set the Z flag -- as the inverse of the lowest bit of cond */
     addInstr(env, X86Instr_Alu32R(Xalu_AND,X86RMI_Imm(1),reg));
     addInstr(env, X86Instr_GotoNZ(
                      True, X86RI_Imm(stmt->Ist.Exit.dst->Ico.U32)));
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
   vex_printf("-- goto ");
   ppIRExpr(next);
   vex_printf("\n");

   ri = iselIntExpr_RI(env, next);
   addInstr(env, X86Instr_GotoNZ(False,ri));
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire BB to x86 code. */

HInstrArray* iselBB_X86 ( IRBB* bb )
{
   Int     i;
   HReg    hreg, hregHI;
   IRStmt* stmt;

   /* Make up an initial environment to use. */
   ISelEnv* env = LibVEX_Alloc(sizeof(ISelEnv));
   env->vreg_ctr = 0;

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
   for (i = 0; i < env->n_vregmap; i++) {
      hregHI = hreg = INVALID_HREG;
      switch (bb->tyenv->types[i]) {
         case Ity_Bit:
         case Ity_I8:
         case Ity_I32: hreg   = mkHReg(i, HRcInt, True); break;
         case Ity_I64: hreg   = mkHReg(i, HRcInt, True);
                       hregHI = mkHReg(i, HRcInt, True); break;
         default: ppIRType(bb->tyenv->types[i]);
                  vpanic("iselBB: IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = env->n_vregmap;

   /* Ok, finally we can iterate over the statements. */
   for (stmt = bb->stmts; stmt; stmt=stmt->link)
      iselStmt(env,stmt);

   iselNext(env,bb->next,bb->jumpkind);

   /* record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;
   return env->code;
}


/*---------------------------------------------------------------*/
/*--- end                                          isel_x86.c ---*/
/*---------------------------------------------------------------*/
