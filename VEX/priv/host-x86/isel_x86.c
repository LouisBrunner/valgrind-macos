
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
     which virtual register is associated with each IRTemp temporary.
     This is computed before insn selection starts, and does not
     change.  We expect this mapping to map precisely the same set
     of IRTemps as the type mapping does.

   - The code array, that is, the insns selected so far.

   - A counter, for generating new virtual registers.

   Note, this is all host-independent.  
*/

typedef
   struct {
      IRTemp ir_name;
      HReg   vreg;
   }
   VRegMaplet;

typedef
   struct {
      IRTypeEnv*   type_env;

      VRegMaplet*  vregmap;
      Int          n_vregmap;

      HInstrArray* code;

      Int          vreg_ctr;
   }
   ISelEnv;


static HReg lookupIRTemp ( ISelEnv* env, IRTemp tmp )
{
   Int i;
   for (i = 0; i < env->n_vregmap; i++)
      if (env->vregmap[i].ir_name == tmp)
         return env->vregmap[i].vreg;
   vpanic("lookupIRTemp");
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
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I32);

   switch (e->tag) {

   case Iex_Tmp:
   return lookupIRTemp(env, e->Iex.Tmp.tmp);

   case Iex_Binop:
      /* Add32(x,y).  For commutative ops we assume any literal
         values are on the second operand. */
      if (e->Iex.Binop.op == Iop_Add32) {
         HReg dst    = newVRegI(env);
         HReg reg    = iselIntExpr_R(env, e->Iex.Binop.arg1);
         X86RMI* rmi = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         addInstr(env, mk_MOV_RR(reg,dst));
         addInstr(env, X86Instr_Alu32R(Xalu_ADD, rmi, dst));
         return dst;
      }
      if (e->Iex.Binop.op == Iop_Shl32) {
         HReg dst    = newVRegI(env);
         HReg regL   = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg regR   = iselIntExpr_R(env, e->Iex.Binop.arg2);
	 addInstr(env, mk_MOV_RR(regL,dst));
	 addInstr(env, mk_MOV_RR(regR,hregX86_ECX()));
	 addInstr(env, X86Instr_Sh32(Xsh_SHL, 0/* %cl */, X86RM_Reg(dst)));
	 return dst;
      }

#if 0
   /* 32-bit literals */
   case Iex_Const: {
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: {
            HReg r = newVRegI(env);
            addInstr(env,
               X86Instr_Mov32(X86Operand_Imm(e->Iex.Const.con->Ico.U32),
			      X86Operand_Reg(r)));
            return r;
         }
         default: break;
      }
   }
#endif

   default: 
   break;
   } /* switch (e->tag) */

   /* We get here if no pattern matched. */
   ppIRExpr(e);
   vpanic("iselExprI: cannot reduce tree");
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
      return X86AMode_IR(r1, e->Iex.Binop.arg2->Iex.Const.con->Ico.U32);
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg r1 = iselIntExpr_R(env, e);
      return X86AMode_IR(r1, 0);
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

   case Ist_Put:
     if (stmt->Ist.Put.size == 4) {
       /* We're going to write to memory, so compute the
	  RHS into an X86RI. */
       X86RI* ri  = iselIntExpr_RI(env, stmt->Ist.Put.expr);
       addInstr(env,
		X86Instr_Alu32M(
                   Xalu_MOV,
                   ri,
		   X86AMode_IR(stmt->Ist.Put.offset,hregX86_EBP())
	       ));
       return;
     }

   case Ist_Tmp: {
     IRTemp tmp = stmt->Ist.Tmp.tmp;
     IRType ty = lookupIRTypeEnv(env->type_env, tmp);
     if (ty == Ity_I32) {
       X86RMI* rmi = iselIntExpr_RMI(env, stmt->Ist.Tmp.expr);
       HReg dst = lookupIRTemp(env, tmp);
       addInstr(env, 
		X86Instr_Alu32R(Xalu_MOV,rmi,dst));
       return;
   }

  }

   default: break;
   }
   ppIRStmt(stmt);
   vpanic("iselStmt");
}


/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

static void iselNext ( ISelEnv* env, IRNext* next )
{
   vex_printf("-- ");
   ppIRNext(next);
   vex_printf("\n");

   switch (next->tag) {
   case Inx_UJump: {
      vassert(next->Inx.UJump.dst->tag == Ico_U32);
      addInstr(env, X86Instr_Alu32R(
                       Xalu_MOV, 
                       X86RMI_Imm(next->Inx.UJump.dst->Ico.U32),
                       hregX86_EAX()));
      addInstr(env, X86Instr_RET());
      return;
   }
   default:
      ppIRNext(next);
      vpanic("iselNext");
   }
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire BB to x86 code. */

HInstrArray* iselBB_X86 ( IRBB* bb )
{
   Int     i;
   HReg    hreg;
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
   env->n_vregmap = bb->tyenv->map_used;
   env->vregmap   = LibVEX_Alloc(env->n_vregmap * sizeof(VRegMaplet));

   /* For each IR temporary, allocate a suitably-kinded virtual
      register. */
   for (i = 0; i < env->n_vregmap; i++) {
      env->vregmap[i].ir_name = bb->tyenv->map[i].name;
      switch (bb->tyenv->map[i].type) {
         case Ity_I32: hreg = mkHReg(i, HRcInt, True); break;
         default: vpanic("iselBB: IRTemp type");
      }
      env->vregmap[i].vreg = hreg;
   }
   env->vreg_ctr = env->n_vregmap;

   /* Ok, finally we can iterate over the statements. */
   for (stmt = bb->stmts; stmt; stmt=stmt->link)
      iselStmt(env,stmt);

   iselNext(env,bb->next);

   /* record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;
   return env->code;
}


/*---------------------------------------------------------------*/
/*--- end                                          isel_x86.c ---*/
/*---------------------------------------------------------------*/
