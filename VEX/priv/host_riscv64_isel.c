
/*--------------------------------------------------------------------*/
/*--- begin                                    host_riscv64_isel.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2020-2023 Petr Pavlu
      petr.pavlu@dagobah.cz

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "host_riscv64_defs.h"
#include "main_globals.h"
#include "main_util.h"

/*------------------------------------------------------------*/
/*--- ISelEnv                                              ---*/
/*------------------------------------------------------------*/

/* This carries around:

   - A mapping from IRTemp to IRType, giving the type of any IRTemp we might
     encounter. This is computed before insn selection starts, and does not
     change.

   - A mapping from IRTemp to HReg. This tells the insn selector which virtual
     register is associated with each IRTemp temporary. This is computed before
     insn selection starts, and does not change. We expect this mapping to map
     precisely the same set of IRTemps as the type mapping does.

     - vregmap   holds the primary register for the IRTemp.
     - vregmapHI is only used for 128-bit integer-typed IRTemps. It holds the
                 identity of a second 64-bit virtual HReg, which holds the high
                 half of the value.

   - The code array, that is, the insns selected so far.

   - A counter, for generating new virtual registers.

   - The host hardware capabilities word. This is set at the start and does not
     change.

   - A Bool for indicating whether we may generate chain-me instructions for
     control flow transfers, or whether we must use XAssisted.

   - The maximum guest address of any guest insn in this block. Actually, the
     address of the highest-addressed byte from any insn in this block. Is set
     at the start and does not change. This is used for detecting jumps which
     are definitely forward-edges from this block, and therefore can be made
     (chained) to the fast entry point of the destination, thereby avoiding the
     destination's event check.

   - An IRExpr*, which may be NULL, holding the IR expression (an
     IRRoundingMode-encoded value) to which the FPU's rounding mode was most
     recently set. Setting to NULL is always safe. Used to avoid redundant
     settings of the FPU's rounding mode, as described in
     set_fcsr_rounding_mode() below.

   Note, this is all (well, mostly) host-independent.
*/

typedef struct {
   /* Constant -- are set at the start and do not change. */
   IRTypeEnv* type_env;

   HReg* vregmap;
   HReg* vregmapHI;
   Int   n_vregmap;

   UInt hwcaps;

   Bool   chainingAllowed;
   Addr64 max_ga;

   /* These are modified as we go along. */
   HInstrArray* code;
   Int          vreg_ctr;

   IRExpr* previous_rm;
} ISelEnv;

static HReg lookupIRTemp(ISelEnv* env, IRTemp tmp)
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   return env->vregmap[tmp];
}

static void addInstr(ISelEnv* env, RISCV64Instr* instr)
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppRISCV64Instr(instr, True /*mode64*/);
      vex_printf("\n");
   }
}

static HReg newVRegI(ISelEnv* env)
{
   HReg reg = mkHReg(True /*virtual*/, HRcInt64, 0, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF(ISelEnv* env)
{
   HReg reg = mkHReg(True /*virtual*/, HRcFlt64, 0, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

/*------------------------------------------------------------*/
/*--- ISEL: Forward declarations                           ---*/
/*------------------------------------------------------------*/

/* These are organised as iselXXX and iselXXX_wrk pairs. The iselXXX_wrk do the
   real work, but are not to be called directly. For each XXX, iselXXX calls its
   iselXXX_wrk counterpart, then checks that all returned registers are virtual.
   You should not call the _wrk version directly. */

static HReg iselIntExpr_R(ISelEnv* env, IRExpr* e);
static void iselInt128Expr(HReg* rHi, HReg* rLo, ISelEnv* env, IRExpr* e);
static HReg iselFltExpr(ISelEnv* env, IRExpr* e);

/*------------------------------------------------------------*/
/*--- ISEL: FP rounding mode helpers                       ---*/
/*------------------------------------------------------------*/

/* Set the FP rounding mode: 'mode' is an I32-typed expression denoting a value
   of IRRoundingMode. Set the fcsr RISC-V register to have the same rounding.

   All attempts to set the rounding mode have to be routed through this
   function for things to work properly. Refer to the comment in the AArch64
   backend for set_FPCR_rounding_mode() how the mechanism relies on the SSA
   property of IR and CSE.
*/
static void set_fcsr_rounding_mode(ISelEnv* env, IRExpr* mode)
{
   vassert(typeOfIRExpr(env->type_env, mode) == Ity_I32);

   /* Do we need to do anything? */
   if (env->previous_rm && env->previous_rm->tag == Iex_RdTmp &&
       mode->tag == Iex_RdTmp &&
       env->previous_rm->Iex.RdTmp.tmp == mode->Iex.RdTmp.tmp) {
      /* No - setting it to what it was before.  */
      vassert(typeOfIRExpr(env->type_env, env->previous_rm) == Ity_I32);
      return;
   }

   /* No luck - we better set it, and remember what we set it to. */
   env->previous_rm = mode;

   /*
      rounding mode                 |  IR  | RISC-V
      ---------------------------------------------
      to nearest, ties to even      | 0000 |   000
      to -infinity                  | 0001 |   011
      to +infinity                  | 0010 |   010
      to zero                       | 0011 |   001
      to nearest, ties away from 0  | 0100 |   100
      prepare for shorter precision | 0101 |   111
      to away from 0                | 0110 |   111
      to nearest, ties towards 0    | 0111 |   111
      invalid                       | 1000 |   111

      All rounding modes not supported on RISC-V are mapped to 111 which is the
      dynamic mode that is always invalid in fcsr and raises an illegal
      instruction exception.

      The mapping can be implemented using the following transformation:
         t0 = 30 >> rm_IR
         t1 = t0 & 19
         t2 = t0 + 7
         t3 = t1 + t2
         fcsr_rm_RISCV = t3 >> t1
   */
   HReg rm_IR  = iselIntExpr_R(env, mode);
   HReg imm_30 = newVRegI(env);
   addInstr(env, RISCV64Instr_LI(imm_30, 30));
   HReg t0 = newVRegI(env);
   addInstr(env, RISCV64Instr_ALU(RISCV64op_SRL, t0, imm_30, rm_IR));
   HReg t1 = newVRegI(env);
   addInstr(env, RISCV64Instr_ALUImm(RISCV64op_ANDI, t1, t0, 19));
   HReg t2 = newVRegI(env);
   addInstr(env, RISCV64Instr_ALUImm(RISCV64op_ADDI, t2, t0, 7));
   HReg t3 = newVRegI(env);
   addInstr(env, RISCV64Instr_ALU(RISCV64op_ADD, t3, t1, t2));
   HReg fcsr_rm_RISCV = newVRegI(env);
   addInstr(env, RISCV64Instr_ALU(RISCV64op_SRL, fcsr_rm_RISCV, t3, t1));
   addInstr(env,
            RISCV64Instr_CSRRW(hregRISCV64_x0(), fcsr_rm_RISCV, 0x002 /*frm*/));
}

/*------------------------------------------------------------*/
/*--- ISEL: Function call helpers                          ---*/
/*------------------------------------------------------------*/

/* Used only in doHelperCall(). See the big comment in doHelperCall() regarding
   handling of register-parameter arguments. This function figures out whether
   evaluation of an expression might require use of a fixed register. If in
   doubt return True (safe but suboptimal).
*/
static Bool mightRequireFixedRegs(IRExpr* e)
{
   if (UNLIKELY(is_IRExpr_VECRET_or_GSPTR(e))) {
      /* These are always "safe" -- either a copy of x2/sp in some arbitrary
         vreg, or a copy of x8/s0, respectively. */
      return False;
   }
   /* Else it's a "normal" expression. */
   switch (e->tag) {
   case Iex_RdTmp:
   case Iex_Const:
   case Iex_Get:
      return False;
   default:
      return True;
   }
}

/* Do a complete function call. |guard| is a Ity_Bit expression indicating
   whether or not the call happens. If guard==NULL, the call is unconditional.
   |retloc| is set to indicate where the return value is after the call. The
   caller (of this fn) must generate code to add |stackAdjustAfterCall| to the
   stack pointer after the call is done. Returns True iff it managed to handle
   this combination of arg/return types, else returns False. */
static Bool doHelperCall(/*OUT*/ UInt*   stackAdjustAfterCall,
                         /*OUT*/ RetLoc* retloc,
                         ISelEnv*        env,
                         IRExpr*         guard,
                         IRCallee*       cee,
                         IRType          retTy,
                         IRExpr**        args)
{
   /* Set default returns. We'll update them later if needed. */
   *stackAdjustAfterCall = 0;
   *retloc               = mk_RetLoc_INVALID();

   /* Marshal args for a call and do the call.

      This function only deals with a limited set of possibilities, which cover
      all helpers in practice. The restrictions are that only the following
      arguments are supported:
      * RISCV64_N_REGPARMS x Ity_I32/Ity_I64 values, passed in x10/a0 .. x17/a7,
      * RISCV64_N_FREGPARMS x Ity_F32/Ity_F64 values, passed in f10/fa0 ..
        f17/fa7.

      Note that the cee->regparms field is meaningless on riscv64 hosts (since
      we only implement one calling convention) and so we always ignore it.

      The return type can be I{8,16,32,64} or V128. In the V128 case, it is
      expected that |args| will contain the special node IRExpr_VECRET(), in
      which case this routine generates code to allocate space on the stack for
      the vector return value.  Since we are not passing any scalars on the
      stack, it is enough to preallocate the return space before marshalling any
      arguments, in this case.

      |args| may also contain IRExpr_GSPTR(), in which case the value in the
      guest state pointer register minus BASEBLOCK_OFFSET_ADJUSTMENT is passed
      as the corresponding argument.

      Generating code which is both efficient and correct when parameters are to
      be passed in registers is difficult, for the reasons elaborated in detail
      in comments attached to doHelperCall() in VEX/priv/host_x86_isel.c. Here,
      we use a variant of the method described in those comments.

      The problem is split into two cases: the fast scheme and the slow scheme.
      In the fast scheme, arguments are computed directly into the target (real)
      registers. This is only safe when we can be sure that computation of each
      argument will not trash any real registers set by computation of any other
      argument.

      In the slow scheme, all args are first computed into vregs, and once they
      are all done, they are moved to the relevant real regs. This always gives
      correct code, but it also gives a bunch of vreg-to-rreg moves which are
      usually redundant but are hard for the register allocator to get rid of.

      To decide which scheme to use, all argument expressions are first
      examined. If they are all so simple that it is clear they will be
      evaluated without use of any fixed registers, use the fast scheme, else
      use the slow scheme. Note also that only unconditional calls may use the
      fast scheme, since having to compute a condition expression could itself
      trash real registers.

      Note this requires being able to examine an expression and determine
      whether or not evaluation of it might use a fixed register. That requires
      knowledge of how the rest of this insn selector works. Currently just the
      following 3 are regarded as safe -- hopefully they cover the majority of
      arguments in practice: IRExpr_RdTmp, IRExpr_Const, IRExpr_Get.
   */

   /* These are used for cross-checking that IR-level constraints on the use of
      IRExpr_VECRET() and IRExpr_GSPTR() are observed. */
   UInt nVECRETs = 0;
   UInt nGSPTRs  = 0;

   UInt n_args = 0;
   for (UInt i = 0; args[i] != NULL; i++) {
      IRExpr* arg = args[i];
      if (UNLIKELY(arg->tag == Iex_VECRET))
         nVECRETs++;
      else if (UNLIKELY(arg->tag == Iex_GSPTR))
         nGSPTRs++;
      n_args++;
   }

   /* If this fails, the IR is ill-formed. */
   vassert(nGSPTRs == 0 || nGSPTRs == 1);

   /* If we have a VECRET, allocate space on the stack for the return value, and
      record the stack pointer after that. */
   HReg r_vecRetAddr = INVALID_HREG;
   if (nVECRETs == 1) {
      vassert(retTy == Ity_V128 || retTy == Ity_V256);
      r_vecRetAddr = newVRegI(env);
      addInstr(env, RISCV64Instr_ALUImm(RISCV64op_ADDI, hregRISCV64_x2(),
                                        hregRISCV64_x2(),
                                        retTy == Ity_V128 ? -16 : -32));
      addInstr(env, RISCV64Instr_MV(r_vecRetAddr, hregRISCV64_x2()));
   } else {
      /* If either of these fail, the IR is ill-formed. */
      vassert(retTy != Ity_V128 && retTy != Ity_V256);
      vassert(nVECRETs == 0);
   }

   /* First decide which scheme (slow or fast) is to be used. First assume the
      fast scheme, and select slow if any contraindications (wow) appear. */
   Bool go_fast = True;

   /* We'll need space on the stack for the return value. Avoid possible
      complications with nested calls by using the slow scheme. */
   if (retTy == Ity_V128 || retTy == Ity_V256)
      go_fast = False;

   if (go_fast && guard != NULL) {
      if (guard->tag == Iex_Const && guard->Iex.Const.con->tag == Ico_U1 &&
          guard->Iex.Const.con->Ico.U1 == True) {
         /* Unconditional. */
      } else {
         /* Not manifestly unconditional -- be conservative. */
         go_fast = False;
      }
   }

   if (go_fast)
      for (UInt i = 0; i < n_args; i++) {
         if (mightRequireFixedRegs(args[i])) {
            go_fast = False;
            break;
         }
      }

   /* At this point the scheme to use has been established. Generate code to get
      the arg values into the argument regs. If we run out of arg regs, give up.
    */

   HReg argregs[RISCV64_N_ARGREGS];
   HReg fargregs[RISCV64_N_FARGREGS];

   vassert(RISCV64_N_ARGREGS == 8);
   vassert(RISCV64_N_FARGREGS == 8);

   argregs[0] = hregRISCV64_x10();
   argregs[1] = hregRISCV64_x11();
   argregs[2] = hregRISCV64_x12();
   argregs[3] = hregRISCV64_x13();
   argregs[4] = hregRISCV64_x14();
   argregs[5] = hregRISCV64_x15();
   argregs[6] = hregRISCV64_x16();
   argregs[7] = hregRISCV64_x17();

   fargregs[0] = hregRISCV64_f10();
   fargregs[1] = hregRISCV64_f11();
   fargregs[2] = hregRISCV64_f12();
   fargregs[3] = hregRISCV64_f13();
   fargregs[4] = hregRISCV64_f14();
   fargregs[5] = hregRISCV64_f15();
   fargregs[6] = hregRISCV64_f16();
   fargregs[7] = hregRISCV64_f17();

   HReg tmpregs[RISCV64_N_ARGREGS];
   HReg ftmpregs[RISCV64_N_FARGREGS];
   Int  nextArgReg = 0, nextFArgReg = 0;
   HReg cond;

   if (go_fast) {
      /* FAST SCHEME */
      for (UInt i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];

         IRType aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, args[i]);

         if (aTy == Ity_I32 || aTy == Ity_I64) {
            if (nextArgReg >= RISCV64_N_ARGREGS)
               return False; /* Out of argregs. */
            addInstr(env, RISCV64Instr_MV(argregs[nextArgReg],
                                          iselIntExpr_R(env, args[i])));
            nextArgReg++;
         } else if (aTy == Ity_F32 || aTy == Ity_F64) {
            if (nextFArgReg >= RISCV64_N_FARGREGS)
               return False; /* Out of fargregs. */
            addInstr(env,
                     RISCV64Instr_FpMove(RISCV64op_FMV_D, fargregs[nextFArgReg],
                                         iselFltExpr(env, args[i])));
            nextFArgReg++;
         } else if (arg->tag == Iex_GSPTR) {
            if (nextArgReg >= RISCV64_N_ARGREGS)
               return False; /* Out of argregs. */
            addInstr(env,
                     RISCV64Instr_MV(argregs[nextArgReg], hregRISCV64_x8()));
            nextArgReg++;
         } else if (arg->tag == Iex_VECRET) {
            /* Because of the go_fast logic above, we can't get here, since
               vector return values make us use the slow path instead. */
            vassert(0);
         } else
            return False; /* Unhandled arg type. */
      }

      /* Fast scheme only applies for unconditional calls. Hence: */
      cond = INVALID_HREG;

   } else {
      /* SLOW SCHEME; move via temporaries. */
      for (UInt i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];

         IRType aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, args[i]);

         if (aTy == Ity_I32 || aTy == Ity_I64) {
            if (nextArgReg >= RISCV64_N_ARGREGS)
               return False; /* Out of argregs. */
            tmpregs[nextArgReg] = iselIntExpr_R(env, args[i]);
            nextArgReg++;
         } else if (aTy == Ity_F32 || aTy == Ity_F64) {
            if (nextFArgReg >= RISCV64_N_FARGREGS)
               return False; /* Out of fargregs. */
            ftmpregs[nextFArgReg] = iselFltExpr(env, args[i]);
            nextFArgReg++;
         } else if (arg->tag == Iex_GSPTR) {
            if (nextArgReg >= RISCV64_N_ARGREGS)
               return False; /* Out of argregs. */
            tmpregs[nextArgReg] = hregRISCV64_x8();
            nextArgReg++;
         } else if (arg->tag == Iex_VECRET) {
            vassert(!hregIsInvalid(r_vecRetAddr));
            tmpregs[nextArgReg] = r_vecRetAddr;
            nextArgReg++;
         } else
            return False; /* Unhandled arg type. */
      }

      /* Compute the condition. Be a bit clever to handle the common case where
         the guard is 1:Bit. */
      cond = INVALID_HREG;
      if (guard) {
         if (guard->tag == Iex_Const && guard->Iex.Const.con->tag == Ico_U1 &&
             guard->Iex.Const.con->Ico.U1 == True) {
            /* Unconditional -- do nothing. */
         } else {
            cond = iselIntExpr_R(env, guard);
         }
      }

      /* Move the args to their final destinations. */
      for (UInt i = 0; i < nextArgReg; i++) {
         vassert(!(hregIsInvalid(tmpregs[i])));
         addInstr(env, RISCV64Instr_MV(argregs[i], tmpregs[i]));
      }
      for (UInt i = 0; i < nextFArgReg; i++) {
         vassert(!(hregIsInvalid(ftmpregs[i])));
         addInstr(env, RISCV64Instr_FpMove(RISCV64op_FMV_D, fargregs[i],
                                           ftmpregs[i]));
      }
   }

   /* Should be assured by checks above. */
   vassert(nextArgReg <= RISCV64_N_ARGREGS);
   vassert(nextFArgReg <= RISCV64_N_FARGREGS);

   /* Do final checks, set the return values, and generate the call instruction
      proper. */
   vassert(nGSPTRs == 0 || nGSPTRs == 1);
   vassert(nVECRETs == ((retTy == Ity_V128 || retTy == Ity_V256) ? 1 : 0));
   vassert(*stackAdjustAfterCall == 0);
   vassert(is_RetLoc_INVALID(*retloc));
   switch (retTy) {
   case Ity_INVALID:
      /* Function doesn't return a value. */
      *retloc = mk_RetLoc_simple(RLPri_None);
      break;
   case Ity_I8:
   case Ity_I16:
   case Ity_I32:
   case Ity_I64:
      *retloc = mk_RetLoc_simple(RLPri_Int);
      break;
   case Ity_V128:
      *retloc               = mk_RetLoc_spRel(RLPri_V128SpRel, 0);
      *stackAdjustAfterCall = 16;
      break;
   case Ity_V256:
      *retloc               = mk_RetLoc_spRel(RLPri_V256SpRel, 0);
      *stackAdjustAfterCall = 32;
      break;
   default:
      /* IR can denote other possible return types, but we don't handle those
         here. */
      return False;
   }

   /* Finally, generate the call itself. This needs the *retloc value set in the
      switch above, which is why it's at the end. */

   /* nextArgReg doles out argument registers. Since these are assigned in the
      order x10/a0 .. x17/a7, its numeric value at this point, which must be
      between 0 and 8 inclusive, is going to be equal to the number of arg regs
      in use for the call. Hence bake that number into the call (we'll need to
      know it when doing register allocation, to know what regs the call reads.)

      The same applies to nextFArgReg which records a number of used
      floating-point registers f10/fa0 .. f17/fa7.
    */
   addInstr(env, RISCV64Instr_Call(*retloc, (Addr64)(HWord)cee->addr, cond, nextArgReg,
                                   nextFArgReg));

   return True;
}

/*------------------------------------------------------------*/
/*--- ISEL: Integer expressions (64/32/16/8/1 bit)         ---*/
/*------------------------------------------------------------*/

/* Select insns for an integer-typed expression, and add them to the code list.
   Return a reg holding the result. This reg will be a virtual register. THE
   RETURNED REG MUST NOT BE MODIFIED. If you want to modify it, ask for a new
   vreg, copy it in there, and modify the copy. The register allocator will do
   its best to map both vregs to the same real register, so the copies will
   often disappear later in the game.

   This should handle expressions of 64, 32, 16, 8 and 1-bit type. All results
   are returned in a 64-bit register. For an N-bit expression, the upper 64-N
   bits are arbitrary, so you should mask or sign-extend partial values if
   necessary.

   The riscv64 backend however internally always extends the values as follows:
   * a 32/16/8-bit integer result is sign-extended to 64 bits,
   * a 1-bit logical result is zero-extended to 64 bits.

   This schema follows the approach taken by the RV64 ISA which by default
   sign-extends any 32/16/8-bit operation result to 64 bits. Matching the isel
   with the ISA generally results in requiring less instructions. For instance,
   it allows that any Ico_U32 immediate can be always materialized at maximum
   using two instructions (LUI+ADDIW).

   An important consequence of this design is that any Iop_<N>Sto64 extension is
   a no-op. On the other hand, any Iop_64to<N> operation must additionally
   perform an N-bit sign-extension. This is the opposite situation than in most
   other VEX backends.
*/

/* -------------------------- Reg --------------------------- */

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselIntExpr_R_wrk(ISelEnv* env, IRExpr* e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I64 || ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8 ||
           ty == Ity_I1);

   switch (e->tag) {
   /* ------------------------ TEMP ------------------------- */
   case Iex_RdTmp: {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   /* ------------------------ LOAD ------------------------- */
   case Iex_Load: {
      if (e->Iex.Load.end != Iend_LE)
         goto irreducible;

      HReg dst = newVRegI(env);
      /* TODO Optimize the cases with small imm Add64/Sub64. */
      HReg addr = iselIntExpr_R(env, e->Iex.Load.addr);

      if (ty == Ity_I64)
         addInstr(env, RISCV64Instr_Load(RISCV64op_LD, dst, addr, 0));
      else if (ty == Ity_I32)
         addInstr(env, RISCV64Instr_Load(RISCV64op_LW, dst, addr, 0));
      else if (ty == Ity_I16)
         addInstr(env, RISCV64Instr_Load(RISCV64op_LH, dst, addr, 0));
      else if (ty == Ity_I8)
         addInstr(env, RISCV64Instr_Load(RISCV64op_LB, dst, addr, 0));
      else
         goto irreducible;
      return dst;
   }

   /* ---------------------- BINARY OP ---------------------- */
   case Iex_Binop: {
      /* TODO Optimize for small imms by generating <instr>i. */
      switch (e->Iex.Binop.op) {
      case Iop_Add64:
      case Iop_Add32:
      case Iop_Sub64:
      case Iop_Sub32:
      case Iop_Xor64:
      case Iop_Xor32:
      case Iop_Or64:
      case Iop_Or32:
      case Iop_Or1:
      case Iop_And64:
      case Iop_And32:
      case Iop_And1:
      case Iop_Shl64:
      case Iop_Shl32:
      case Iop_Shr64:
      case Iop_Shr32:
      case Iop_Sar64:
      case Iop_Sar32:
      case Iop_Mul64:
      case Iop_Mul32:
      case Iop_DivU64:
      case Iop_DivU32:
      case Iop_DivS64:
      case Iop_DivS32: {
         RISCV64ALUOp op;
         switch (e->Iex.Binop.op) {
         case Iop_Add64:
            op = RISCV64op_ADD;
            break;
         case Iop_Add32:
            op = RISCV64op_ADDW;
            break;
         case Iop_Sub64:
            op = RISCV64op_SUB;
            break;
         case Iop_Sub32:
            op = RISCV64op_SUBW;
            break;
         case Iop_Xor64:
         case Iop_Xor32:
            op = RISCV64op_XOR;
            break;
         case Iop_Or64:
         case Iop_Or32:
         case Iop_Or1:
            op = RISCV64op_OR;
            break;
         case Iop_And64:
         case Iop_And32:
         case Iop_And1:
            op = RISCV64op_AND;
            break;
         case Iop_Shl64:
            op = RISCV64op_SLL;
            break;
         case Iop_Shl32:
            op = RISCV64op_SLLW;
            break;
         case Iop_Shr64:
            op = RISCV64op_SRL;
            break;
         case Iop_Shr32:
            op = RISCV64op_SRLW;
            break;
         case Iop_Sar64:
            op = RISCV64op_SRA;
            break;
         case Iop_Sar32:
            op = RISCV64op_SRAW;
            break;
         case Iop_Mul64:
            op = RISCV64op_MUL;
            break;
         case Iop_Mul32:
            op = RISCV64op_MULW;
            break;
         case Iop_DivU64:
            op = RISCV64op_DIVU;
            break;
         case Iop_DivU32:
            op = RISCV64op_DIVUW;
            break;
         case Iop_DivS64:
            op = RISCV64op_DIV;
            break;
         case Iop_DivS32:
            op = RISCV64op_DIVW;
            break;
         default:
            vassert(0);
         }
         HReg dst  = newVRegI(env);
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, RISCV64Instr_ALU(op, dst, argL, argR));
         return dst;
      }
      case Iop_CmpEQ64:
      case Iop_CmpEQ32:
      case Iop_CasCmpEQ64:
      case Iop_CasCmpEQ32: {
         HReg tmp  = newVRegI(env);
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_SUB, tmp, argL, argR));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLTIU, dst, tmp, 1));
         return dst;
      }
      case Iop_CmpNE64:
      case Iop_CmpNE32:
      case Iop_CasCmpNE64:
      case Iop_CasCmpNE32: {
         HReg tmp  = newVRegI(env);
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_SUB, tmp, argL, argR));
         HReg dst = newVRegI(env);
         addInstr(env,
                  RISCV64Instr_ALU(RISCV64op_SLTU, dst, hregRISCV64_x0(), tmp));
         return dst;
      }
      case Iop_CmpLT64S:
      case Iop_CmpLT32S: {
         HReg dst  = newVRegI(env);
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_SLT, dst, argL, argR));
         return dst;
      }
      case Iop_CmpLE64S:
      case Iop_CmpLE32S: {
         HReg tmp  = newVRegI(env);
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_SLT, tmp, argR, argL));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLTIU, dst, tmp, 1));
         return dst;
      }
      case Iop_CmpLT64U:
      case Iop_CmpLT32U: {
         HReg dst  = newVRegI(env);
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_SLTU, dst, argL, argR));
         return dst;
      }
      case Iop_CmpLE64U:
      case Iop_CmpLE32U: {
         HReg tmp  = newVRegI(env);
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_SLTU, tmp, argR, argL));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLTIU, dst, tmp, 1));
         return dst;
      }
      case Iop_Max32U: {
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg cond = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_SLTU, cond, argL, argR));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_CSEL(dst, argR, argL, cond));
         return dst;
      }
      case Iop_32HLto64: {
         HReg hi32s = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg lo32s = iselIntExpr_R(env, e->Iex.Binop.arg2);

         HReg lo32_tmp = newVRegI(env);
         addInstr(env,
                  RISCV64Instr_ALUImm(RISCV64op_SLLI, lo32_tmp, lo32s, 32));
         HReg lo32 = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SRLI, lo32, lo32_tmp, 32));

         HReg hi32 = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLLI, hi32, hi32s, 32));

         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_OR, dst, hi32, lo32));
         return dst;
      }
      case Iop_DivModS32to32: {
         /* TODO Improve in conjunction with Iop_64HIto32. */
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);

         HReg remw = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_REMW, remw, argL, argR));
         HReg remw_hi = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLLI, remw_hi, remw, 32));

         HReg divw = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_DIVW, divw, argL, argR));
         HReg divw_hi = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLLI, divw_hi, divw, 32));
         HReg divw_lo = newVRegI(env);
         addInstr(env,
                  RISCV64Instr_ALUImm(RISCV64op_SRLI, divw_lo, divw_hi, 32));

         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_OR, dst, remw_hi, divw_lo));
         return dst;
      }
      case Iop_DivModU32to32: {
         /* TODO Improve in conjunction with Iop_64HIto32. */
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);

         HReg remuw = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_REMUW, remuw, argL, argR));
         HReg remuw_hi = newVRegI(env);
         addInstr(env,
                  RISCV64Instr_ALUImm(RISCV64op_SLLI, remuw_hi, remuw, 32));

         HReg divuw = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_DIVUW, divuw, argL, argR));
         HReg divuw_hi = newVRegI(env);
         addInstr(env,
                  RISCV64Instr_ALUImm(RISCV64op_SLLI, divuw_hi, divuw, 32));
         HReg divuw_lo = newVRegI(env);
         addInstr(env,
                  RISCV64Instr_ALUImm(RISCV64op_SRLI, divuw_lo, divuw_hi, 32));

         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_OR, dst, remuw_hi, divuw_lo));
         return dst;
      }
      case Iop_F32toI32S:
      case Iop_F32toI32U:
      case Iop_F32toI64S:
      case Iop_F32toI64U: {
         RISCV64FpConvertOp op;
         switch (e->Iex.Binop.op) {
         case Iop_F32toI32S:
            op = RISCV64op_FCVT_W_S;
            break;
         case Iop_F32toI32U:
            op = RISCV64op_FCVT_WU_S;
            break;
         case Iop_F32toI64S:
            op = RISCV64op_FCVT_L_S;
            break;
         case Iop_F32toI64U:
            op = RISCV64op_FCVT_LU_S;
            break;
         default:
            vassert(0);
         }
         HReg dst = newVRegI(env);
         HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
         set_fcsr_rounding_mode(env, e->Iex.Binop.arg1);
         addInstr(env, RISCV64Instr_FpConvert(op, dst, src));
         return dst;
      }
      case Iop_CmpF32:
      case Iop_CmpF64: {
         HReg argL = iselFltExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselFltExpr(env, e->Iex.Binop.arg2);

         HReg lt = newVRegI(env);
         HReg gt = newVRegI(env);
         HReg eq = newVRegI(env);
         if (e->Iex.Binop.op == Iop_CmpF32) {
            addInstr(env,
                     RISCV64Instr_FpCompare(RISCV64op_FLT_S, lt, argL, argR));
            addInstr(env,
                     RISCV64Instr_FpCompare(RISCV64op_FLT_S, gt, argR, argL));
            addInstr(env,
                     RISCV64Instr_FpCompare(RISCV64op_FEQ_S, eq, argL, argR));
         } else {
            addInstr(env,
                     RISCV64Instr_FpCompare(RISCV64op_FLT_D, lt, argL, argR));
            addInstr(env,
                     RISCV64Instr_FpCompare(RISCV64op_FLT_D, gt, argR, argL));
            addInstr(env,
                     RISCV64Instr_FpCompare(RISCV64op_FEQ_D, eq, argL, argR));
         }

         /*
            t0 = Ircr_UN
            t1 = Ircr_LT
            t2 = csel t1, t0, lt
            t3 = Ircr_GT
            t4 = csel t3, t2, gt
            t5 = Ircr_EQ
            dst = csel t5, t4, eq
         */
         HReg t0 = newVRegI(env);
         addInstr(env, RISCV64Instr_LI(t0, Ircr_UN));
         HReg t1 = newVRegI(env);
         addInstr(env, RISCV64Instr_LI(t1, Ircr_LT));
         HReg t2 = newVRegI(env);
         addInstr(env, RISCV64Instr_CSEL(t2, t1, t0, lt));
         HReg t3 = newVRegI(env);
         addInstr(env, RISCV64Instr_LI(t3, Ircr_GT));
         HReg t4 = newVRegI(env);
         addInstr(env, RISCV64Instr_CSEL(t4, t3, t2, gt));
         HReg t5 = newVRegI(env);
         addInstr(env, RISCV64Instr_LI(t5, Ircr_EQ));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_CSEL(dst, t5, t4, eq));
         return dst;
      }
      case Iop_F64toI32S:
      case Iop_F64toI32U:
      case Iop_F64toI64S:
      case Iop_F64toI64U: {
         RISCV64FpConvertOp op;
         switch (e->Iex.Binop.op) {
         case Iop_F64toI32S:
            op = RISCV64op_FCVT_W_D;
            break;
         case Iop_F64toI32U:
            op = RISCV64op_FCVT_WU_D;
            break;
         case Iop_F64toI64S:
            op = RISCV64op_FCVT_L_D;
            break;
         case Iop_F64toI64U:
            op = RISCV64op_FCVT_LU_D;
            break;
         default:
            vassert(0);
         }
         HReg dst = newVRegI(env);
         HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
         set_fcsr_rounding_mode(env, e->Iex.Binop.arg1);
         addInstr(env, RISCV64Instr_FpConvert(op, dst, src));
         return dst;
      }
      default:
         break;
      }

      break;
   }

   /* ---------------------- UNARY OP ----------------------- */
   case Iex_Unop: {
      switch (e->Iex.Unop.op) {
      case Iop_Not64:
      case Iop_Not32: {
         HReg dst = newVRegI(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_XORI, dst, src, -1));
         return dst;
      }
      case Iop_Not1: {
         HReg dst = newVRegI(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLTIU, dst, src, 1));
         return dst;
      }
      case Iop_8Uto32:
      case Iop_8Uto64:
      case Iop_16Uto64:
      case Iop_32Uto64: {
         UInt shift =
            64 - 8 * sizeofIRType(typeOfIRExpr(env->type_env, e->Iex.Unop.arg));
         HReg tmp = newVRegI(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLLI, tmp, src, shift));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SRLI, dst, tmp, shift));
         return dst;
      }
      case Iop_1Sto32:
      case Iop_1Sto64: {
         HReg tmp = newVRegI(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLLI, tmp, src, 63));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SRAI, dst, tmp, 63));
         return dst;
      }
      case Iop_1Uto64:
      case Iop_8Sto64:
      case Iop_16Sto64:
      case Iop_32Sto64:
         /* These are no-ops. */
         return iselIntExpr_R(env, e->Iex.Unop.arg);
      case Iop_32to8:
      case Iop_32to16:
      case Iop_64to8:
      case Iop_64to16:
      case Iop_64to32: {
         UInt shift = 64 - 8 * sizeofIRType(ty);
         HReg tmp   = newVRegI(env);
         HReg src   = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLLI, tmp, src, shift));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SRAI, dst, tmp, shift));
         return dst;
      }
      case Iop_128HIto64: {
         HReg rHi, rLo;
         iselInt128Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
         return rHi; /* and abandon rLo */
      }
      case Iop_64HIto32: {
         HReg dst = newVRegI(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SRAI, dst, src, 32));
         return dst;
      }
      case Iop_ReinterpF32asI32: {
         HReg dst = newVRegI(env);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_FpMove(RISCV64op_FMV_X_W, dst, src));
         return dst;
      }
      case Iop_ReinterpF64asI64: {
         HReg dst = newVRegI(env);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_FpMove(RISCV64op_FMV_X_D, dst, src));
         return dst;
      }
      case Iop_CmpNEZ8:
      case Iop_CmpNEZ32:
      case Iop_CmpNEZ64: {
         HReg dst = newVRegI(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env,
                  RISCV64Instr_ALU(RISCV64op_SLTU, dst, hregRISCV64_x0(), src));
         return dst;
      }
      case Iop_CmpwNEZ32:
      case Iop_CmpwNEZ64: {
         /* Use the fact that x | -x == 0 iff x == 0. Otherwise, either X or -X
            will have a 1 in the MSB. */
         HReg neg = newVRegI(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env,
                  RISCV64Instr_ALU(RISCV64op_SUB, neg, hregRISCV64_x0(), src));
         HReg or = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_OR, or, src, neg));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SRAI, dst, or, 63));
         return dst;
      }
      case Iop_Left32:
      case Iop_Left64: {
         /* Left32/64(src) = src | -src. */
         HReg neg = newVRegI(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env,
                  RISCV64Instr_ALU(RISCV64op_SUB, neg, hregRISCV64_x0(), src));
         HReg dst = newVRegI(env);
         addInstr(env, RISCV64Instr_ALU(RISCV64op_OR, dst, src, neg));
         return dst;
      }
      default:
         break;
      }

      break;
   }

   /* ------------------------- GET ------------------------- */
   case Iex_Get: {
      HReg dst  = newVRegI(env);
      HReg base = get_baseblock_register();
      Int  off  = e->Iex.Get.offset - BASEBLOCK_OFFSET_ADJUSTMENT;
      vassert(off >= -2048 && off < 2048);

      if (ty == Ity_I64)
         addInstr(env, RISCV64Instr_Load(RISCV64op_LD, dst, base, off));
      else if (ty == Ity_I32)
         addInstr(env, RISCV64Instr_Load(RISCV64op_LW, dst, base, off));
      else if (ty == Ity_I16)
         addInstr(env, RISCV64Instr_Load(RISCV64op_LH, dst, base, off));
      else if (ty == Ity_I8)
         addInstr(env, RISCV64Instr_Load(RISCV64op_LB, dst, base, off));
      else
         goto irreducible;
      return dst;
   }

   /* ------------------------ CCALL ------------------------ */
   case Iex_CCall: {
      vassert(ty == e->Iex.CCall.retty);

      /* Be very restrictive for now. Only 32 and 64-bit ints are allowed for
         the return type. */
      if (e->Iex.CCall.retty != Ity_I32 && e->Iex.CCall.retty != Ity_I64)
         goto irreducible;

      /* Marshal args and do the call. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      Bool   ok =
         doHelperCall(&addToSp, &rloc, env, NULL /*guard*/, e->Iex.CCall.cee,
                      e->Iex.CCall.retty, e->Iex.CCall.args);
      if (!ok)
         goto irreducible;
      vassert(is_sane_RetLoc(rloc));
      vassert(rloc.pri == RLPri_Int);
      vassert(addToSp == 0);

      HReg dst = newVRegI(env);
      switch (e->Iex.CCall.retty) {
      case Ity_I32:
         /* Sign-extend the value returned from the helper as is expected by the
            rest of the backend. */
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_ADDIW, dst,
                                           hregRISCV64_x10(), 0));
         break;
      case Ity_I64:
         addInstr(env, RISCV64Instr_MV(dst, hregRISCV64_x10()));
         break;
      default:
         vassert(0);
      }
      return dst;
   }

   /* ----------------------- LITERAL ----------------------- */
   /* 64/32/16/8-bit literals. */
   case Iex_Const: {
      ULong u;
      HReg  dst = newVRegI(env);
      switch (e->Iex.Const.con->tag) {
      case Ico_U64:
         u = e->Iex.Const.con->Ico.U64;
         break;
      case Ico_U32:
         vassert(ty == Ity_I32);
         u = vex_sx_to_64(e->Iex.Const.con->Ico.U32, 32);
         break;
      case Ico_U16:
         vassert(ty == Ity_I16);
         u = vex_sx_to_64(e->Iex.Const.con->Ico.U16, 16);
         break;
      case Ico_U8:
         vassert(ty == Ity_I8);
         u = vex_sx_to_64(e->Iex.Const.con->Ico.U8, 8);
         break;
      case Ico_U1:
         vassert(ty == Ity_I1);
         u = e->Iex.Const.con->Ico.U1 ? 1 : 0;
         break;
      default:
         goto irreducible;
      }
      addInstr(env, RISCV64Instr_LI(dst, u));
      return dst;
   }

   /* ---------------------- MULTIPLEX ---------------------- */
   case Iex_ITE: {
      /* ITE(ccexpr, iftrue, iffalse) */
      if (ty == Ity_I64 || ty == Ity_I32) {
         HReg dst     = newVRegI(env);
         HReg iftrue  = iselIntExpr_R(env, e->Iex.ITE.iftrue);
         HReg iffalse = iselIntExpr_R(env, e->Iex.ITE.iffalse);
         HReg cond    = iselIntExpr_R(env, e->Iex.ITE.cond);
         addInstr(env, RISCV64Instr_CSEL(dst, iftrue, iffalse, cond));
         return dst;
      }
      break;
   }

   default:
      break;
   }

   /* We get here if no pattern matched. */
irreducible:
   ppIRExpr(e);
   vpanic("iselIntExpr_R(riscv64)");
}

static HReg iselIntExpr_R(ISelEnv* env, IRExpr* e)
{
   HReg r = iselIntExpr_R_wrk(env, e);

   /* Sanity checks ... */
   vassert(hregClass(r) == HRcInt64);
   vassert(hregIsVirtual(r));

   return r;
}

/*------------------------------------------------------------*/
/*--- ISEL: Integer expressions (128 bit)                  ---*/
/*------------------------------------------------------------*/

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt128Expr_wrk(HReg* rHi, HReg* rLo, ISelEnv* env, IRExpr* e)
{
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I128);

   /* ---------------------- BINARY OP ---------------------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
      /* 64 x 64 -> 128 multiply */
      case Iop_MullS64:
      case Iop_MullU64: {
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         *rHi      = newVRegI(env);
         *rLo      = newVRegI(env);
         if (e->Iex.Binop.op == Iop_MullS64)
            addInstr(env, RISCV64Instr_ALU(RISCV64op_MULH, *rHi, argL, argR));
         else
            addInstr(env, RISCV64Instr_ALU(RISCV64op_MULHU, *rHi, argL, argR));
         addInstr(env, RISCV64Instr_ALU(RISCV64op_MUL, *rLo, argL, argR));
         return;
      }

      /* 64 x 64 -> (64(rem),64(div)) division */
      case Iop_DivModS64to64:
      case Iop_DivModU64to64: {
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         *rHi      = newVRegI(env);
         *rLo      = newVRegI(env);
         if (e->Iex.Binop.op == Iop_DivModS64to64) {
            addInstr(env, RISCV64Instr_ALU(RISCV64op_REM, *rHi, argL, argR));
            addInstr(env, RISCV64Instr_ALU(RISCV64op_DIV, *rLo, argL, argR));
         } else {
            addInstr(env, RISCV64Instr_ALU(RISCV64op_REMU, *rHi, argL, argR));
            addInstr(env, RISCV64Instr_ALU(RISCV64op_DIVU, *rLo, argL, argR));
         }
         return;
      }

      /* 64HLto128(e1,e2) */
      case Iop_64HLto128:
         *rHi = iselIntExpr_R(env, e->Iex.Binop.arg1);
         *rLo = iselIntExpr_R(env, e->Iex.Binop.arg2);
         return;

      default:
         break;
      }
   }

   ppIRExpr(e);
   vpanic("iselInt128Expr(riscv64)");
}

/* Compute a 128-bit value into a register pair, which is returned as the first
   two parameters. As with iselIntExpr_R, these will be virtual registers and
   they must not be changed by subsequent code emitted by the caller. */
static void iselInt128Expr(HReg* rHi, HReg* rLo, ISelEnv* env, IRExpr* e)
{
   iselInt128Expr_wrk(rHi, rLo, env, e);

   /* Sanity checks ... */
   vassert(hregClass(*rHi) == HRcInt64);
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcInt64);
   vassert(hregIsVirtual(*rLo));
}

/*------------------------------------------------------------*/
/*--- ISEL: Floating point expressions                     ---*/
/*------------------------------------------------------------*/

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselFltExpr_wrk(ISelEnv* env, IRExpr* e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_F32 || ty == Ity_F64);

   switch (e->tag) {
   /* ------------------------ TEMP ------------------------- */
   case Iex_RdTmp: {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   /* ------------------------ LOAD ------------------------- */
   case Iex_Load: {
      if (e->Iex.Load.end != Iend_LE)
         goto irreducible;

      HReg dst = newVRegF(env);
      /* TODO Optimize the cases with small imm Add64/Sub64. */
      HReg addr = iselIntExpr_R(env, e->Iex.Load.addr);

      if (ty == Ity_F32)
         addInstr(env, RISCV64Instr_FpLdSt(RISCV64op_FLW, dst, addr, 0));
      else if (ty == Ity_F64)
         addInstr(env, RISCV64Instr_FpLdSt(RISCV64op_FLD, dst, addr, 0));
      else
         vassert(0);
      return dst;
   }

   /* -------------------- QUATERNARY OP -------------------- */
   case Iex_Qop: {
      switch (e->Iex.Qop.details->op) {
      case Iop_MAddF32: {
         HReg dst  = newVRegF(env);
         HReg argN = iselFltExpr(env, e->Iex.Qop.details->arg2);
         HReg argM = iselFltExpr(env, e->Iex.Qop.details->arg3);
         HReg argA = iselFltExpr(env, e->Iex.Qop.details->arg4);
         set_fcsr_rounding_mode(env, e->Iex.Qop.details->arg1);
         addInstr(env, RISCV64Instr_FpTernary(RISCV64op_FMADD_S, dst, argN,
                                              argM, argA));
         return dst;
      }
      case Iop_MAddF64: {
         HReg dst  = newVRegF(env);
         HReg argN = iselFltExpr(env, e->Iex.Qop.details->arg2);
         HReg argM = iselFltExpr(env, e->Iex.Qop.details->arg3);
         HReg argA = iselFltExpr(env, e->Iex.Qop.details->arg4);
         set_fcsr_rounding_mode(env, e->Iex.Qop.details->arg1);
         addInstr(env, RISCV64Instr_FpTernary(RISCV64op_FMADD_D, dst, argN,
                                              argM, argA));
         return dst;
      }
      default:
         break;
      }

      break;
   }

   /* --------------------- TERNARY OP ---------------------- */
   case Iex_Triop: {
      RISCV64FpBinaryOp op;
      switch (e->Iex.Triop.details->op) {
      case Iop_AddF32:
         op = RISCV64op_FADD_S;
         break;
      case Iop_MulF32:
         op = RISCV64op_FMUL_S;
         break;
      case Iop_DivF32:
         op = RISCV64op_FDIV_S;
         break;
      case Iop_AddF64:
         op = RISCV64op_FADD_D;
         break;
      case Iop_SubF64:
         op = RISCV64op_FSUB_D;
         break;
      case Iop_MulF64:
         op = RISCV64op_FMUL_D;
         break;
      case Iop_DivF64:
         op = RISCV64op_FDIV_D;
         break;
      default:
         goto irreducible;
      }
      HReg dst  = newVRegF(env);
      HReg src1 = iselFltExpr(env, e->Iex.Triop.details->arg2);
      HReg src2 = iselFltExpr(env, e->Iex.Triop.details->arg3);
      set_fcsr_rounding_mode(env, e->Iex.Triop.details->arg1);
      addInstr(env, RISCV64Instr_FpBinary(op, dst, src1, src2));
      return dst;
   }

   /* ---------------------- BINARY OP ---------------------- */
   case Iex_Binop: {
      switch (e->Iex.Binop.op) {
      case Iop_SqrtF32: {
         HReg dst = newVRegF(env);
         HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
         set_fcsr_rounding_mode(env, e->Iex.Binop.arg1);
         addInstr(env, RISCV64Instr_FpUnary(RISCV64op_FSQRT_S, dst, src));
         return dst;
      }
      case Iop_SqrtF64: {
         HReg dst = newVRegF(env);
         HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
         set_fcsr_rounding_mode(env, e->Iex.Binop.arg1);
         addInstr(env, RISCV64Instr_FpUnary(RISCV64op_FSQRT_D, dst, src));
         return dst;
      }
      case Iop_I32StoF32:
      case Iop_I32UtoF32:
      case Iop_I64StoF32:
      case Iop_I64UtoF32:
      case Iop_I64StoF64:
      case Iop_I64UtoF64: {
         RISCV64FpConvertOp op;
         switch (e->Iex.Binop.op) {
         case Iop_I32StoF32:
            op = RISCV64op_FCVT_S_W;
            break;
         case Iop_I32UtoF32:
            op = RISCV64op_FCVT_S_WU;
            break;
         case Iop_I64StoF32:
            op = RISCV64op_FCVT_S_L;
            break;
         case Iop_I64UtoF32:
            op = RISCV64op_FCVT_S_LU;
            break;
         case Iop_I64StoF64:
            op = RISCV64op_FCVT_D_L;
            break;
         case Iop_I64UtoF64:
            op = RISCV64op_FCVT_D_LU;
            break;
         default:
            vassert(0);
         }
         HReg dst = newVRegF(env);
         HReg src = iselIntExpr_R(env, e->Iex.Binop.arg2);
         set_fcsr_rounding_mode(env, e->Iex.Binop.arg1);
         addInstr(env, RISCV64Instr_FpConvert(op, dst, src));
         return dst;
      }
      case Iop_F64toF32: {
         HReg dst = newVRegF(env);
         HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
         set_fcsr_rounding_mode(env, e->Iex.Binop.arg1);
         addInstr(env, RISCV64Instr_FpConvert(RISCV64op_FCVT_S_D, dst, src));
         return dst;
      }
      case Iop_MinNumF32:
      case Iop_MaxNumF32:
      case Iop_MinNumF64:
      case Iop_MaxNumF64: {
         RISCV64FpBinaryOp op;
         switch (e->Iex.Binop.op) {
         case Iop_MinNumF32:
            op = RISCV64op_FMIN_S;
            break;
         case Iop_MaxNumF32:
            op = RISCV64op_FMAX_S;
            break;
         case Iop_MinNumF64:
            op = RISCV64op_FMIN_D;
            break;
         case Iop_MaxNumF64:
            op = RISCV64op_FMAX_D;
            break;
         default:
            vassert(0);
         }
         HReg dst  = newVRegF(env);
         HReg src1 = iselFltExpr(env, e->Iex.Binop.arg1);
         HReg src2 = iselFltExpr(env, e->Iex.Binop.arg2);
         addInstr(env, RISCV64Instr_FpBinary(op, dst, src1, src2));
         return dst;
      }
      default:
         break;
      }

      break;
   }

   /* ---------------------- UNARY OP ----------------------- */
   case Iex_Unop: {
      switch (e->Iex.Unop.op) {
      case Iop_NegF32:
      case Iop_AbsF32:
      case Iop_NegF64:
      case Iop_AbsF64: {
         RISCV64FpBinaryOp op;
         switch (e->Iex.Unop.op) {
         case Iop_NegF32:
            op = RISCV64op_FSGNJN_S;
            break;
         case Iop_AbsF32:
            op = RISCV64op_FSGNJX_S;
            break;
         case Iop_NegF64:
            op = RISCV64op_FSGNJN_D;
            break;
         case Iop_AbsF64:
            op = RISCV64op_FSGNJX_D;
            break;
         default:
            vassert(0);
         }
         HReg dst = newVRegF(env);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_FpBinary(op, dst, src, src));
         return dst;
      }
      case Iop_I32StoF64: {
         HReg dst = newVRegF(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_FpConvert(RISCV64op_FCVT_D_W, dst, src));
         return dst;
      }
      case Iop_I32UtoF64: {
         HReg dst = newVRegF(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_FpConvert(RISCV64op_FCVT_D_WU, dst, src));
         return dst;
      }
      case Iop_F32toF64: {
         HReg dst = newVRegF(env);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_FpConvert(RISCV64op_FCVT_D_S, dst, src));
         return dst;
      }
      case Iop_ReinterpI32asF32: {
         HReg dst = newVRegF(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_FpMove(RISCV64op_FMV_W_X, dst, src));
         return dst;
      }
      case Iop_ReinterpI64asF64: {
         HReg dst = newVRegF(env);
         HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, RISCV64Instr_FpMove(RISCV64op_FMV_D_X, dst, src));
         return dst;
      }
      default:
         break;
      }

      break;
   }

   /* ------------------------- GET ------------------------- */
   case Iex_Get: {
      HReg dst  = newVRegF(env);
      HReg base = get_baseblock_register();
      Int  off  = e->Iex.Get.offset - BASEBLOCK_OFFSET_ADJUSTMENT;
      vassert(off >= -2048 && off < 2048);

      if (ty == Ity_F32)
         addInstr(env, RISCV64Instr_FpLdSt(RISCV64op_FLW, dst, base, off));
      else if (ty == Ity_F64)
         addInstr(env, RISCV64Instr_FpLdSt(RISCV64op_FLD, dst, base, off));
      else
         vassert(0);
      return dst;
   }

   /* ---------------------- MULTIPLEX ---------------------- */
   case Iex_ITE: {
      /* ITE(ccexpr, iftrue, iffalse) */
      HReg cond    = iselIntExpr_R(env, e->Iex.ITE.cond);
      HReg iftrue  = iselFltExpr(env, e->Iex.ITE.iftrue);
      HReg iffalse = iselFltExpr(env, e->Iex.ITE.iffalse);
      HReg dst     = newVRegF(env);
      addInstr(env, RISCV64Instr_FpCSEL(dst, iftrue, iffalse, cond));
      return dst;
   }

   default:
      break;
   }

irreducible:
   ppIRExpr(e);
   vpanic("iselFltExpr(riscv64)");
}

/* Compute a floating-point value into a register, the identity of which is
   returned. As with iselIntExpr_R, the register will be virtual and must not be
   changed by subsequent code emitted by the caller. */
static HReg iselFltExpr(ISelEnv* env, IRExpr* e)
{
   HReg r = iselFltExpr_wrk(env, e);

   /* Sanity checks ... */
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));

   return r;
}

/*------------------------------------------------------------*/
/*--- ISEL: Statements                                     ---*/
/*------------------------------------------------------------*/

static void iselStmt(ISelEnv* env, IRStmt* stmt)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- ");
      ppIRStmt(stmt);
      vex_printf("\n");
   }

   switch (stmt->tag) {
   /* ------------------------ STORE ------------------------ */
   /* Little-endian write to memory. */
   case Ist_Store: {
      IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);
      if (tyd == Ity_I64 || tyd == Ity_I32 || tyd == Ity_I16 || tyd == Ity_I8) {
         HReg src = iselIntExpr_R(env, stmt->Ist.Store.data);
         /* TODO Optimize the cases with small imm Add64/Sub64. */
         HReg addr = iselIntExpr_R(env, stmt->Ist.Store.addr);

         if (tyd == Ity_I64)
            addInstr(env, RISCV64Instr_Store(RISCV64op_SD, src, addr, 0));
         else if (tyd == Ity_I32)
            addInstr(env, RISCV64Instr_Store(RISCV64op_SW, src, addr, 0));
         else if (tyd == Ity_I16)
            addInstr(env, RISCV64Instr_Store(RISCV64op_SH, src, addr, 0));
         else if (tyd == Ity_I8)
            addInstr(env, RISCV64Instr_Store(RISCV64op_SB, src, addr, 0));
         else
            vassert(0);
         return;
      }
      if (tyd == Ity_F32 || tyd == Ity_F64) {
         HReg src  = iselFltExpr(env, stmt->Ist.Store.data);
         HReg addr = iselIntExpr_R(env, stmt->Ist.Store.addr);

         if (tyd == Ity_F32)
            addInstr(env, RISCV64Instr_FpLdSt(RISCV64op_FSW, src, addr, 0));
         else if (tyd == Ity_F64)
            addInstr(env, RISCV64Instr_FpLdSt(RISCV64op_FSD, src, addr, 0));
         else
            vassert(0);
         return;
      }
      break;
   }

   /* ------------------------- PUT ------------------------- */
   /* Write guest state, fixed offset. */
   case Ist_Put: {
      IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
      if (tyd == Ity_I64 || tyd == Ity_I32 || tyd == Ity_I16 || tyd == Ity_I8) {
         HReg src  = iselIntExpr_R(env, stmt->Ist.Put.data);
         HReg base = get_baseblock_register();
         Int  off  = stmt->Ist.Put.offset - BASEBLOCK_OFFSET_ADJUSTMENT;
         vassert(off >= -2048 && off < 2048);

         if (tyd == Ity_I64)
            addInstr(env, RISCV64Instr_Store(RISCV64op_SD, src, base, off));
         else if (tyd == Ity_I32)
            addInstr(env, RISCV64Instr_Store(RISCV64op_SW, src, base, off));
         else if (tyd == Ity_I16)
            addInstr(env, RISCV64Instr_Store(RISCV64op_SH, src, base, off));
         else if (tyd == Ity_I8)
            addInstr(env, RISCV64Instr_Store(RISCV64op_SB, src, base, off));
         else
            vassert(0);
         return;
      }
      if (tyd == Ity_F32 || tyd == Ity_F64) {
         HReg src  = iselFltExpr(env, stmt->Ist.Put.data);
         HReg base = get_baseblock_register();
         Int  off  = stmt->Ist.Put.offset - BASEBLOCK_OFFSET_ADJUSTMENT;
         vassert(off >= -2048 && off < 2048);

         if (tyd == Ity_F32)
            addInstr(env, RISCV64Instr_FpLdSt(RISCV64op_FSW, src, base, off));
         else if (tyd == Ity_F64)
            addInstr(env, RISCV64Instr_FpLdSt(RISCV64op_FSD, src, base, off));
         else
            vassert(0);
         return;
      }
      break;
   }

   /* ------------------------- TMP ------------------------- */
   /* Assign value to temporary. */
   case Ist_WrTmp: {
      IRType ty = typeOfIRTemp(env->type_env, stmt->Ist.WrTmp.tmp);
      if (ty == Ity_I64 || ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8 ||
          ty == Ity_I1) {
         HReg dst = lookupIRTemp(env, stmt->Ist.WrTmp.tmp);
         HReg src = iselIntExpr_R(env, stmt->Ist.WrTmp.data);
         addInstr(env, RISCV64Instr_MV(dst, src));
         return;
      }
      if (ty == Ity_F32 || ty == Ity_F64) {
         HReg dst = lookupIRTemp(env, stmt->Ist.WrTmp.tmp);
         HReg src = iselFltExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, RISCV64Instr_FpMove(RISCV64op_FMV_D, dst, src));
         return;
      }
      break;
   }

   /* ---------------- Call to DIRTY helper ----------------- */
   /* Call complex ("dirty") helper function. */
   case Ist_Dirty: {
      IRDirty* d = stmt->Ist.Dirty.details;

      /* Figure out the return type, if any. */
      IRType retty = Ity_INVALID;
      if (d->tmp != IRTemp_INVALID)
         retty = typeOfIRTemp(env->type_env, d->tmp);

      if (retty != Ity_INVALID && retty != Ity_I8 && retty != Ity_I16 &&
          retty != Ity_I32 && retty != Ity_I64)
         goto stmt_fail;

      /* Marshal args and do the call. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      Bool   ok =
         doHelperCall(&addToSp, &rloc, env, d->guard, d->cee, retty, d->args);
      if (!ok)
         goto stmt_fail;
      vassert(is_sane_RetLoc(rloc));
      vassert(addToSp == 0);

      /* Now figure out what to do with the returned value, if any. */
      switch (retty) {
      case Ity_INVALID: {
         /* No return value. Nothing to do. */
         vassert(d->tmp == IRTemp_INVALID);
         vassert(rloc.pri == RLPri_None);
         return;
      }
      /* The returned value is for Ity_I<x> in x10/a0. Park it in the register
         associated with tmp. */
      case Ity_I8:
      case Ity_I16: {
         vassert(rloc.pri == RLPri_Int);
         /* Sign-extend the value returned from the helper as is expected by the
            rest of the backend. */
         HReg dst   = lookupIRTemp(env, d->tmp);
         UInt shift = 64 - 8 * sizeofIRType(retty);
         HReg tmp   = newVRegI(env);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SLLI, tmp,
                                           hregRISCV64_x10(), shift));
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_SRAI, dst, tmp, shift));
         return;
      }
      case Ity_I32: {
         vassert(rloc.pri == RLPri_Int);
         HReg dst = lookupIRTemp(env, d->tmp);
         addInstr(env, RISCV64Instr_ALUImm(RISCV64op_ADDIW, dst,
                                           hregRISCV64_x10(), 0));
         return;
      }
      case Ity_I64: {
         vassert(rloc.pri == RLPri_Int);
         HReg dst = lookupIRTemp(env, d->tmp);
         addInstr(env, RISCV64Instr_MV(dst, hregRISCV64_x10()));
         return;
      }
      default:
         vassert(0);
      }
      break;
   }

   /* ---------- Load Linked and Store Conditional ---------- */
   case Ist_LLSC: {
      if (stmt->Ist.LLSC.storedata == NULL) {
         /* LL */
         IRTemp res = stmt->Ist.LLSC.result;
         IRType ty  = typeOfIRTemp(env->type_env, res);
         if (ty == Ity_I32) {
            HReg r_dst  = lookupIRTemp(env, res);
            HReg r_addr = iselIntExpr_R(env, stmt->Ist.LLSC.addr);
            addInstr(env, RISCV64Instr_LoadR(RISCV64op_LR_W, r_dst, r_addr));
            return;
         }
      } else {
         /* SC */
         IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.storedata);
         if (tyd == Ity_I32) {
            HReg r_tmp  = newVRegI(env);
            HReg r_src  = iselIntExpr_R(env, stmt->Ist.LLSC.storedata);
            HReg r_addr = iselIntExpr_R(env, stmt->Ist.LLSC.addr);
            addInstr(env,
                     RISCV64Instr_StoreC(RISCV64op_SC_W, r_tmp, r_src, r_addr));

            /* Now r_tmp is non-zero if failed, 0 if success. Change to IR
               conventions (0 is fail, 1 is success). */
            IRTemp res   = stmt->Ist.LLSC.result;
            HReg   r_res = lookupIRTemp(env, res);
            IRType ty    = typeOfIRTemp(env->type_env, res);
            vassert(ty == Ity_I1);
            addInstr(env,
                     RISCV64Instr_ALUImm(RISCV64op_SLTIU, r_res, r_tmp, 1));
            return;
         }
      }
      break;
   }

   /* ------------------------ ACAS ------------------------- */
   case Ist_CAS: {
      if (stmt->Ist.CAS.details->oldHi == IRTemp_INVALID) {
         /* "Normal" singleton CAS. */
         IRCAS* cas = stmt->Ist.CAS.details;
         IRType tyd = typeOfIRTemp(env->type_env, cas->oldLo);
         if (tyd == Ity_I64 || tyd == Ity_I32) {
            HReg old  = lookupIRTemp(env, cas->oldLo);
            HReg addr = iselIntExpr_R(env, cas->addr);
            HReg expd = iselIntExpr_R(env, cas->expdLo);
            HReg data = iselIntExpr_R(env, cas->dataLo);
            if (tyd == Ity_I64)
               addInstr(env, RISCV64Instr_CAS(RISCV64op_CAS_D, old, addr, expd,
                                              data));
            else
               addInstr(env, RISCV64Instr_CAS(RISCV64op_CAS_W, old, addr, expd,
                                              data));
            return;
         }
      }
      break;
   }

   /* ---------------------- MEM FENCE ---------------------- */
   case Ist_MBE:
      switch (stmt->Ist.MBE.event) {
      case Imbe_Fence:
         addInstr(env, RISCV64Instr_FENCE());
         return;
      default:
         break;
      }
      break;

   /* --------------------- INSTR MARK ---------------------- */
   /* Doesn't generate any executable code ... */
   case Ist_IMark:
      return;

   /* ---------------------- ABI HINT ----------------------- */
   /* These have no meaning (denotation in the IR) and so we ignore them ... if
      any actually made it this far. */
   case Ist_AbiHint:
       return;

   /* ------------------------ NO-OP ------------------------ */
   case Ist_NoOp:
      return;

   /* ------------------------ EXIT ------------------------- */
   case Ist_Exit: {
      if (stmt->Ist.Exit.dst->tag != Ico_U64)
         vpanic("iselStmt(riscv64): Ist_Exit: dst is not a 64-bit value");

      HReg cond   = iselIntExpr_R(env, stmt->Ist.Exit.guard);
      HReg base   = get_baseblock_register();
      Int  soff12 = stmt->Ist.Exit.offsIP - BASEBLOCK_OFFSET_ADJUSTMENT;
      vassert(soff12 >= -2048 && soff12 < 2048);

      /* Case: boring transfer to known address. */
      if (stmt->Ist.Exit.jk == Ijk_Boring) {
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards edge. */
            Bool toFastEP = (Addr64)stmt->Ist.Exit.dst->Ico.U64 > env->max_ga;
            if (0)
               vex_printf("%s", toFastEP ? "Y" : ",");
            addInstr(env, RISCV64Instr_XDirect(stmt->Ist.Exit.dst->Ico.U64,
                                               base, soff12, cond, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer, as
               that's the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env,
                     RISCV64Instr_XAssisted(r, base, soff12, cond, Ijk_Boring));
         }
         return;
      }

      /* Case: assisted transfer to arbitrary address. */
      switch (stmt->Ist.Exit.jk) {
      /* Keep this list in sync with that for iselNext below. */
      case Ijk_ClientReq:
      case Ijk_NoDecode:
      case Ijk_NoRedir:
      case Ijk_Sys_syscall:
      case Ijk_InvalICache:
      case Ijk_SigTRAP: {
         HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
         addInstr(env, RISCV64Instr_XAssisted(r, base, soff12, cond,
                                              stmt->Ist.Exit.jk));
         return;
      }
      default:
         break;
      }

      /* Do we ever expect to see any other kind? */
      goto stmt_fail;
   }

   default:
      break;
   }

stmt_fail:
   ppIRStmt(stmt);
   vpanic("iselStmt");
}

/*------------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)                ---*/
/*------------------------------------------------------------*/

static void iselNext(ISelEnv* env, IRExpr* next, IRJumpKind jk, Int offsIP)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- PUT(%d) = ", offsIP);
      ppIRExpr(next);
      vex_printf("; exit-");
      ppIRJumpKind(jk);
      vex_printf("\n");
   }

   HReg base   = get_baseblock_register();
   Int  soff12 = offsIP - BASEBLOCK_OFFSET_ADJUSTMENT;
   vassert(soff12 >= -2048 && soff12 < 2048);

   /* Case: boring transfer to known address. */
   if (next->tag == Iex_Const) {
      IRConst* cdst = next->Iex.Const.con;
      vassert(cdst->tag == Ico_U64);
      if (jk == Ijk_Boring || jk == Ijk_Call) {
         /* Boring transfer to known address. */
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards edge. */
            Bool toFastEP = (Addr64)cdst->Ico.U64 > env->max_ga;
            if (0)
               vex_printf("%s", toFastEP ? "X" : ".");
            addInstr(env, RISCV64Instr_XDirect(cdst->Ico.U64, base, soff12,
                                               INVALID_HREG, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer, as that's
               the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, next);
            addInstr(env, RISCV64Instr_XAssisted(r, base, soff12, INVALID_HREG,
                                                 Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address. */
   switch (jk) {
   case Ijk_Boring:
   case Ijk_Ret:
   case Ijk_Call: {
      HReg r = iselIntExpr_R(env, next);
      if (env->chainingAllowed)
         addInstr(env, RISCV64Instr_XIndir(r, base, soff12, INVALID_HREG));
      else
         addInstr(env, RISCV64Instr_XAssisted(r, base, soff12, INVALID_HREG,
                                              Ijk_Boring));
      return;
   }
   default:
      break;
   }

   /* Case: assisted transfer to arbitrary address. */
   switch (jk) {
   /* Keep this list in sync with that for Ist_Exit above. */
   case Ijk_ClientReq:
   case Ijk_NoDecode:
   case Ijk_NoRedir:
   case Ijk_Sys_syscall:
   case Ijk_InvalICache:
   case Ijk_SigTRAP: {
      HReg r = iselIntExpr_R(env, next);
      addInstr(env, RISCV64Instr_XAssisted(r, base, soff12, INVALID_HREG, jk));
      return;
   }
   default:
      break;
   }

   vex_printf("\n-- PUT(%d) = ", offsIP);
   ppIRExpr(next);
   vex_printf("; exit-");
   ppIRJumpKind(jk);
   vex_printf("\n");
   vassert(0); /* Are we expecting any other kind? */
}

/*------------------------------------------------------------*/
/*--- Insn selector top-level                              ---*/
/*------------------------------------------------------------*/

/* Translate an entire SB to riscv64 code. */

HInstrArray* iselSB_RISCV64(const IRSB*        bb,
                            VexArch            arch_host,
                            const VexArchInfo* archinfo_host,
                            const VexAbiInfo*  vbi /*UNUSED*/,
                            Int                offs_Host_EvC_Counter,
                            Int                offs_Host_EvC_FailAddr,
                            Bool               chainingAllowed,
                            Bool               addProfInc,
                            Addr               max_ga)
{
   Int      i, j;
   HReg     hreg, hregHI;
   ISelEnv *env, envmem;

   /* Do some sanity checks. */
   vassert(arch_host == VexArchRISCV64);

   /* Check that the host's endianness is as expected. */
   vassert(archinfo_host->endness == VexEndnessLE);

   /* Guard against unexpected space regressions. */
   vassert(sizeof(RISCV64Instr) <= 32);

   /* Make up an initial environment to use. */
   env           = &envmem;
   env->vreg_ctr = 0;

   /* Set up output code array. */
   env->code = newHInstrArray();

   /* Copy BB's type env. */
   env->type_env = bb->tyenv;

   /* Make up an IRTemp -> virtual HReg mapping. This doesn't change as we go
      along. */
   env->n_vregmap = bb->tyenv->types_used;
   env->vregmap   = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
   env->vregmapHI = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));

   /* and finally ... */
   env->chainingAllowed = chainingAllowed;
   env->hwcaps          = archinfo_host->hwcaps;
   env->previous_rm     = NULL;
   env->max_ga          = max_ga;

   /* For each IR temporary, allocate a suitably-kinded virtual register. */
   j = 0;
   for (i = 0; i < env->n_vregmap; i++) {
      hregHI = hreg = INVALID_HREG;
      switch (bb->tyenv->types[i]) {
      case Ity_I1:
      case Ity_I8:
      case Ity_I16:
      case Ity_I32:
      case Ity_I64:
         hreg = mkHReg(True, HRcInt64, 0, j++);
         break;
      case Ity_I128:
         hreg   = mkHReg(True, HRcInt64, 0, j++);
         hregHI = mkHReg(True, HRcInt64, 0, j++);
         break;
      case Ity_F32:
      case Ity_F64:
         hreg = mkHReg(True, HRcFlt64, 0, j++);
         break;
      default:
         ppIRType(bb->tyenv->types[i]);
         vpanic("iselBB(riscv64): IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   HReg base             = get_baseblock_register();
   Int  soff12_amCounter = offs_Host_EvC_Counter - BASEBLOCK_OFFSET_ADJUSTMENT;
   vassert(soff12_amCounter >= -2048 && soff12_amCounter < 2048);
   Int soff12_amFailAddr = offs_Host_EvC_FailAddr - BASEBLOCK_OFFSET_ADJUSTMENT;
   vassert(soff12_amFailAddr >= -2048 && soff12_amFailAddr < 2048);
   addInstr(env, RISCV64Instr_EvCheck(base, soff12_amCounter, base,
                                      soff12_amFailAddr));

   /* Possibly a block counter increment (for profiling). At this point we don't
      know the address of the counter, so just pretend it is zero. It will have
      to be patched later, but before this translation is used, by a call to
      LibVEX_PatchProfInc(). */
   if (addProfInc)
      addInstr(env, RISCV64Instr_ProfInc());

   /* Ok, finally we can iterate over the statements. */
   for (i = 0; i < bb->stmts_used; i++)
      iselStmt(env, bb->stmts[i]);

   iselNext(env, bb->next, bb->jumpkind, bb->offsIP);

   /* Record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;
   return env->code;
}

/*--------------------------------------------------------------------*/
/*--- end                                      host_riscv64_isel.c ---*/
/*--------------------------------------------------------------------*/
