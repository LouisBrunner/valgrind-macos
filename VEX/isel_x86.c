
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (isel_x86.c) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>
#include <malloc.h>

#include "basictypes.h"
#include "ir_defs.h"
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
      IRTypeEnv* type_env;

      VRegMaplet* vregmap;
      Int n_vregmap;

      void** code;  /* really, X86Instr**, or ArmInstr**, or whatever */
      Int    code_size;
      Int    code_used;

      Int    ctr;
   }
   ISelEnv;


static HReg lookupIRTemp ( ISelEnv* env, IRTemp tmp )
{
   Int i;
   for (i = 0; i < env->n_vregmap; i++)
      if (env->vregmap[i].ir_name == tmp)
         return env->vregmap[i].vreg;
   panic("lookupIRTemp");
}

static void addInstr ( ISelEnv* env, void* instr )
{
   assert(env->code_used < env->code_size);
   env->code[env->code_used++] = instr;
   ppX86Instr(stdout, instr);
   printf("\n");
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(env->ctr, HRcInt, True/*virtual reg*/);
   env->ctr++;
   return reg;
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions                         ---*/
/*---------------------------------------------------------*/

/* Select insns for an integer-typed expression, and add them to the
   code list.  Return a vreg holding the result.  The vreg MUST NOT BE
   MODIFIED.  If you want to modify it, ask for a new vreg, copy it in
   there, and modify the copy.  The register allocator will do its
   best to map both vregs to the same real register, so the copies
   will often disappear later in the game.
*/
HReg iselExprI ( ISelEnv* env, IRExpr* e )
{
   assert(e);
   assert(typeOfIRExpr(env->type_env,e) == Ity_I32);

   switch (e->tag) {

   case Iex_Tmp:
   return lookupIRTemp(env, e->Iex.Tmp.tmp);

   case Iex_Binop:
      /* Add32(x,y) */
      if (e->Iex.Binop.op == Iop_Add32) {
         HReg res = newVRegI(env);
         HReg src = iselExprI(env, e->Iex.Binop.arg1);
         HReg dst = iselExprI(env, e->Iex.Binop.arg2);
         addInstr(env, X86Instr_Mov32(
                          X86Operand_Reg(dst),
                          X86Operand_Reg(res)));
         addInstr(env, X86Instr_Alu32(
                          Xalu_ADD, X86Operand_Reg(src),
                                    X86Operand_Reg(res)) );
         return res;
      }

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

   default: 
   break;
   } /* switch (e->tag) */

   /* We get here if no pattern matched. */
   ppIRExpr(stderr, e);
   panic("iselExprI: cannot reduce tree");
}


/* Similarly, return an AMode which computes the value of the
   specified expression, possibly also adding insns to the code list
   as a result.  
*/
X86AMode* iselAMode ( ISelEnv* env, IRExpr* e )
{
   assert(e);
   assert(typeOfIRExpr(env->type_env,e) == Ity_I32);

   /* Add32(expr1, Shl32(expr2, imm)) */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg2->tag == Iex_Binop
       && e->Iex.Binop.arg2->Iex.Binop.op == Iop_Shl32
       && e->Iex.Binop.arg2->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32) {
      UInt shift = e->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
      if (shift == 2 || shift == 4 || shift == 8) {
         HReg r1 = iselExprI(env, e->Iex.Binop.arg1);
         HReg r2 = iselExprI(env, e->Iex.Binop.arg2->Iex.Binop.arg1 );
         return X86AMode_IRRS(0, r1, r2, shift);
      }
   }

   /* Add32(expr,i) */
   if (e->tag == Iex_Binop 
       && e->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32) {
      HReg r1 = iselExprI(env,  e->Iex.Binop.arg1);
      return X86AMode_IR(r1, e->Iex.Binop.arg2->Iex.Const.con->Ico.U32);
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg r1 = iselExprI(env, e);
      return X86AMode_IR(r1, 0);
   }
}


/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

void iselStmt ( ISelEnv* env, IRStmt* stmt )
{
   switch (stmt->tag) {

   case Ist_Put:
     if (stmt->Ist.Put.size == 4) {
       HReg ebp = mkHReg(5, HRcInt, False);
       HReg reg = iselExprI(env, stmt->Ist.Put.expr);
       addInstr(env,
		X86Instr_ST32(reg,
			      X86AMode_IR(stmt->Ist.Put.offset,
					  ebp)
			      ));
       return;
     }

   default: break;
   }
   ppIRStmt(stderr, stmt);
   panic("iselStmt");
}


/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

void iselNext ( ISelEnv* env, IRNext* next )
{
   switch (next->tag) {
   default: 
      ppIRNext(stderr, next);
      panic("iselNext");
   }
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

void /* not really, but for the time being ... */
     iselBB ( IRBB* bb )
{
   Int     i;
   HReg    hreg;
   IRStmt* stmt;

   /* Make up an initial environment to use. */
   ISelEnv* env = malloc(sizeof(ISelEnv));
   env->ctr = 0;

   /* Set up output code array. */
   env->code_used = 0;
   env->code_size = 40;
   env->code = malloc(env->code_size * sizeof(void*));

   /* Copy BB's type env. */
   env->type_env = bb->tyenv;

   /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
      change as we go along. */
   env->n_vregmap = bb->tyenv->map_used;
   env->vregmap   = malloc(env->n_vregmap * sizeof(VRegMaplet));

   /* For each IR temporary, allocate a suitably-kinded virtual
      register. */
   for (i = 0; i < env->n_vregmap; i++) {
      env->vregmap[i].ir_name = bb->tyenv->map[i].name;
      switch (bb->tyenv->map[i].type) {
         case Ity_I32: hreg = mkHReg(i, HRcInt, True); break;
         default: panic("iselBB: IRTemp type");
      }
      env->vregmap[i].vreg = hreg;
   }

   /* Ok, finally we can iterate over the statements. */
   for (stmt = bb->stmts; stmt; stmt=stmt->link)
      iselStmt(env,stmt);

   iselNext(env,bb->next);
}
