
/*---------------------------------------------------------------*/
/*--- begin                              host_nanomips_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2018 RT-RK

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_nanomips_defs.h"

/*---------------------------------------------------------*/
/*--- Register Usage Conventions                        ---*/
/*---------------------------------------------------------*/

/* Integer Regs
   ------------
   ZERO0       Reserved
   GPR12:22    Allocateable
   23          GuestStatePointer
   SP          StackFramePointer
   RA          LinkRegister */

/* Host hwcaps */
static UInt hwcaps_host = 0;

/* GPR register class for NANOMIPS */
#define HRcGPR  HRcInt32

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

   - The host subarchitecture we are selecting insns for.
     This is set at the start and does not change.

   - A Bool for indicating whether we may generate chain-me
     instructions for control flow transfers, or whether we must use
     XAssisted.

   - The maximum guest address of any guest insn in this block.
     Actually, the address of the highest-addressed byte from any insn
     in this block.  Is set at the start and does not change.  This is
     used for detecting jumps which are definitely forward-edges from
     this block, and therefore can be made (chained) to the fast entry
     point of the destination, thereby avoiding the destination's
     event check.

   Note, this is all (well, mostly) host-independent.
*/

typedef
struct {
   /* Constant -- are set at the start and do not change. */
   IRTypeEnv*   type_env;

   HReg*        vregmap;
   HReg*        vregmapHI;
   Int          n_vregmap;

   UInt         hwcaps;

   Bool         chainingAllowed;
   Addr64       max_ga;

   /* These are modified as we go along. */
   HInstrArray* code;
   Int          vreg_ctr;
} ISelEnv;

static HReg lookupIRTemp(ISelEnv* env, IRTemp tmp)
{
   vassert(tmp < env->n_vregmap);
   return env->vregmap[tmp];
}

static void lookupIRTemp64(HReg* vrHI, HReg* vrLO, ISelEnv* env, IRTemp tmp)
{
   vassert(tmp < env->n_vregmap);
   vassert(!hregIsInvalid(env->vregmapHI[tmp]));
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr(ISelEnv* env, NANOMIPSInstr* instr)
{
   addHInstr(env->code, instr);

   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppNANOMIPSInstr(instr);
      vex_printf("\n");
   }
}

static HReg newVRegI(ISelEnv* env)
{
   HReg reg = mkHReg(True /* virtual reg */,
                     HRcGPR, 0 /* enc */, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

/*---------------------------------------------------------*/
/*--- ISEL: Forward declarations                        ---*/
/*---------------------------------------------------------*/

/* These are organised as iselXXX and iselXXX_wrk pairs.  The
   iselXXX_wrk do the real work, but are not to be called directly.
   For each XXX, iselXXX calls its iselXXX_wrk counterpart, then
   checks that all returned registers are virtual.  You should not
   call the _wrk version directly.
*/

/* Compute an I1/I8/I16/I32 into a GPR. */
static HReg iselWordExpr_R_wrk(ISelEnv* env, IRExpr* e);
static HReg iselWordExpr_R(ISelEnv* env, IRExpr* e);

/* Compute an I64 into a pair of GPRs. */
static void iselInt64Expr_wrk(HReg* rHi, HReg* rLo, ISelEnv* env, IRExpr* e);
static void iselInt64Expr(HReg* rHi, HReg* rLo, ISelEnv* env, IRExpr* e);

/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

/* Make an int reg-reg move. */
static inline NANOMIPSInstr *mk_iMOVds_RR(HReg r_dst, HReg r_src)
{
   vassert(hregClass(r_dst) == hregClass(r_src));
   vassert(hregClass(r_src) == HRcInt32);
   return NANOMIPSInstr_Alu(NMalu_OR, r_dst, r_src, r_src);
}

/* Extract sign-extended value from IRConst */
static inline Int extractConst(IRConst *c)
{
   switch (c->tag) {
      case Ico_U32:
         return c->Ico.U32;

      case Ico_U16:
         return (Int)(Short)c->Ico.U16;

      case Ico_U8:
         return (Int)(Char)c->Ico.U8;

      case Ico_U1:
         return !!c->Ico.U1;

      default:
         vpanic("NANOMIPSisel_extractConst() fails");
   }
}

/*---------------------------------------------------------*/
/*--- ISEL: Function call helpers                       ---*/
/*---------------------------------------------------------*/

/* Used only in doHelperCall.  See big comment in doHelperCall re
   handling of register-parameter args.  This function figures out
   whether evaluation of an expression might require use of a fixed
   register.  If in doubt return True (safe but suboptimal).
*/
static Bool mightRequireFixedRegs(IRExpr* e)
{
   switch (e->tag) {
      case Iex_RdTmp:
      case Iex_Const:
      case Iex_Get:
         return False;

      default:
         return True;
   }
}

/* Do a complete function call.  |guard| is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional.  |retloc| is set to indicate where the
   return value is after the call.  The caller (of this fn) must
   generate code to add |stackAdjustAfterCall| to the stack pointer
   after the call is done. */

static void doHelperCall(/*OUT*/ RetLoc* retloc,
                                 ISelEnv* env,
                                 IRExpr* guard,
                                 IRCallee* cee,
                                 IRType retty,
                                 IRExpr** args )
{
   HReg argregs[8];
   HReg tmpregs[8];
   Bool go_fast;
   UInt n_args, i, argreg, nGSPTRs, argiregs;
   HReg cond = INVALID_HREG;

   vassert((retty == Ity_INVALID) ||
           (retty == Ity_I32) ||
           (retty == Ity_I64) ||
           (retty == Ity_I8) ||
           (retty == Ity_I16));
   /* NANOMIPS P32 calling convention: up to eight registers ($a0 ... $a7)
      are allowed to be used for passing integer arguments. */

   /* The return type can be I{32,16,8}.
      |args| may contain IRExpr_GSPTR(), in which case the value
      in the guest state pointer register is passed as the
      corresponding argument. */

   *retloc = mk_RetLoc_INVALID();
   n_args = 0;
   nGSPTRs = 0;

   for (i = 0; args[i]; i++) {
      IRExpr* arg = args[i];

      if (UNLIKELY(arg->tag == Iex_GSPTR)) {
         nGSPTRs++;
      }

      n_args++;
   }

   vassert(nGSPTRs <= 1);
   vassert(n_args <= NANOMIPS_N_REGPARMS);

   argregs[0] = hregNANOMIPS_GPR4();
   argregs[1] = hregNANOMIPS_GPR5();
   argregs[2] = hregNANOMIPS_GPR6();
   argregs[3] = hregNANOMIPS_GPR7();
   argregs[4] = hregNANOMIPS_GPR8();
   argregs[5] = hregNANOMIPS_GPR9();
   argregs[6] = hregNANOMIPS_GPR10();
   argregs[7] = hregNANOMIPS_GPR11();
   argiregs = 0;
   tmpregs[0] = tmpregs[1] = tmpregs[2] =
                                tmpregs[3] = tmpregs[4] = tmpregs[5] =
                                         tmpregs[6] = tmpregs[7] = INVALID_HREG;

   /* First decide which scheme (slow or fast) is to be used. First assume the
      fast scheme, and select slow if any contraindications (wow) appear. */
   go_fast = True;

   if (guard) {
      vassert(typeOfIRExpr(env->type_env, guard) == Ity_I1);

      if (guard->tag != Iex_Const || !guard->Iex.Const.con->Ico.U1) {
         go_fast = False;
         cond = iselWordExpr_R(env, guard);
      }
   }

   if (go_fast) {
      for (i = 0; i < n_args; i++) {
         if (mightRequireFixedRegs(args[i])) {
            go_fast = False;
            break;
         }
      }
   }

   /* At this point the scheme to use has been established.  Generate
      code to get the arg values into the argument rregs. */
   if (go_fast) {
      argreg = 0;

      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];
         IRType aTy = Ity_INVALID;
         vassert(argreg < NANOMIPS_N_REGPARMS);

         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, arg);

         switch (aTy) {
            case Ity_I1:
            case Ity_I8:
            case Ity_I16:
            case Ity_I32:
               argiregs |= (1 << (argreg + 4));
               addInstr(env, mk_iMOVds_RR(argregs[argreg],
                                          iselWordExpr_R(env, arg)));
               argreg++;
               break;

            case Ity_I64:
               if (argreg & 1) {
                  argreg++;
                  argiregs |= (1 << (argreg + 4));
               }

               vassert(argreg + 1 < NANOMIPS_N_REGPARMS);

               HReg rHi, rLo;
               iselInt64Expr(&rHi, &rLo, env, arg);
               argiregs |= (1 << (argreg + 4));
               addInstr(env, mk_iMOVds_RR(argregs[argreg++], rHi));
               argiregs |= (1 << (argreg + 4));
               addInstr(env, mk_iMOVds_RR(argregs[argreg], rLo));
               argreg++;
               break;

            case Ity_INVALID:
            default:
               vassert(arg->tag == Iex_GSPTR);
               addInstr(env, mk_iMOVds_RR(argregs[argreg], GuestStatePointer));
               argreg++;
               break;
         }
      }
   } else {
      argreg = 0;

      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];
         IRType  aTy = Ity_INVALID;
         vassert(argreg < NANOMIPS_N_REGPARMS);

         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, arg);

         switch (aTy) {
            case Ity_I1:
            case Ity_I8:
            case Ity_I16:
            case Ity_I32:
               tmpregs[argreg] = iselWordExpr_R(env, arg);
               argreg++;
               break;

            case Ity_I64: {
               HReg raHi, raLo;

               if (argreg & 1) {
                  argreg++;
               }

               vassert(argreg + 1 < NANOMIPS_N_REGPARMS);

               iselInt64Expr(&raHi, &raLo, env, arg);
               tmpregs[argreg] = raLo;
               argreg++;
               tmpregs[argreg] = raHi;
               argreg++;
               break;
            }

            case Ity_INVALID:
            default:
               vassert(arg->tag == Iex_GSPTR);
               tmpregs[argreg] = GuestStatePointer;
               argreg++;
               break;
         }

         for (i = 0; i < argreg; i++) {
            if (hregIsInvalid(tmpregs[i]))
               continue;

            /* None of these insns, including any spill code that might
               be generated, may alter the condition codes. */
            argiregs |= (1 << (i + 4));
            addInstr(env, mk_iMOVds_RR(argregs[i], tmpregs[i]));
         }
      }
   }

   switch (retty) {
      case Ity_INVALID:
         *retloc = mk_RetLoc_simple(RLPri_None);
         break;

      case Ity_I64:
         *retloc = mk_RetLoc_simple(RLPri_2Int);
         break;

      case Ity_I32:
      case Ity_I16:
      case Ity_I8:
         *retloc = mk_RetLoc_simple(RLPri_Int);
         break;

      default:
         vassert(0);
   }

   addInstr(env, NANOMIPSInstr_Call((Addr)cee->addr, argiregs, cond, *retloc));
}

/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64/32/16/8 bit)        ---*/
/*---------------------------------------------------------*/

/* Select insns for an integer-typed expression, and add them to the
   code list.  Return a reg holding the result.  This reg will be a
   virtual register.  THE RETURNED REG MUST NOT BE MODIFIED.  If you
   want to modify it, ask for a new vreg, copy it in there, and modify
   the copy.  The register allocator will do its best to map both
   vregs to the same real register, so the copies will often disappear
   later in the game.

   This should handle expressions of 64, 32, 16 and 8-bit type.
   All results are returned in a (mode64 ? 64bit : 32bit) register.
   For 16- and 8-bit expressions, the upper (32/48/56 : 16/24) bits
   are arbitrary, so you should mask or sign extend partial values
   if necessary.
*/
static HReg iselWordExpr_R(ISelEnv * env, IRExpr * e)
{
   HReg r = iselWordExpr_R_wrk(env, e);
   /* sanity checks ... */
   vassert(hregClass(r) == HRcGPR);
   vassert(hregIsVirtual(r));
   return r;
}

static HReg iselWordExpr_R_wrk(ISelEnv * env, IRExpr * e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I1 || ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);

   switch (e->tag) {
      case Iex_RdTmp:
         return lookupIRTemp(env, e->Iex.RdTmp.tmp);

      case Iex_Load: {
         HReg r_dst = newVRegI(env);
         HReg r_addr = iselWordExpr_R(env, e->Iex.Load.addr);
         addInstr(env, NANOMIPSInstr_Load(sizeofIRType(ty), r_dst, r_addr, 0));
         return r_dst;
      }

      case Iex_Get: {
         vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);
         HReg r_dst = newVRegI(env);
         vassert((e->Iex.Get.offset < 0x1000) && (e->Iex.Get.offset >= 0));
         addInstr(env, NANOMIPSInstr_Load(sizeofIRType(ty), r_dst,
                                          GuestStatePointer,
                                          e->Iex.Get.offset));
         return r_dst;
      }

      case Iex_Binop: {
         NANOMIPSAluOp aluOp;
         NANOMIPSCondCode ccOp;

         switch (e->Iex.Binop.op) {
            case Iop_Add8:
            case Iop_Add16:
            case Iop_Add32:
               aluOp = NMalu_ADD;
               break;

            case Iop_Sub8:
            case Iop_Sub16:
            case Iop_Sub32:
               aluOp = NMalu_SUB;
               break;

            case Iop_And1:
            case Iop_And8:
            case Iop_And16:
            case Iop_And32:
               aluOp = NMalu_AND;
               break;

            case Iop_Or1:
            case Iop_Or8:
            case Iop_Or16:
            case Iop_Or32:
               aluOp = NMalu_OR;
               break;

            case Iop_Xor8:
            case Iop_Xor16:
            case Iop_Xor32:
               aluOp = NMalu_XOR;
               break;

            case Iop_Shl32:
               aluOp = NMalu_SLL;
               break;

            case Iop_Shr32:
               aluOp = NMalu_SRL;
               break;

            case Iop_Sar32:
               aluOp = NMalu_SRA;
               break;

            case Iop_Mul32:
               aluOp = NMalu_MULU;
               break;

            case Iop_MullS8:
            case Iop_MullS16:
               aluOp = NMalu_MUL;
               break;

            case Iop_DivS32:
               aluOp = NMalu_DIV;
               break;

            case Iop_DivU32:
               aluOp = NMalu_DIVU;
               break;

            default:
               aluOp = NMalu_INVALID;
               break;
         }

         if (aluOp != NMalu_INVALID) {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);

            /* Optimization: If seccond argument is Const and
               ALU operation can be converted to IMM operation */
            if ((aluOp <= NMalu_AND) &&
                  (e->Iex.Binop.arg2->tag == Iex_Const)) {

               UInt val = extractConst(e->Iex.Binop.arg2->Iex.Const.con);

               if ((val < 0x20) ||
                     ((val < 0x1000) && (aluOp >= NMalu_OR))) {
                  NANOMIPSImmOp immOp = (NANOMIPSImmOp)aluOp;
                  addInstr(env, NANOMIPSInstr_Imm(immOp, r_dst, r_srcL,
                                                  val));
                  return r_dst;
               }
            }

            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, NANOMIPSInstr_Alu(aluOp, r_dst, r_srcL, r_srcR));
            return r_dst;
         }

         switch (e->Iex.Binop.op) {
            case Iop_CmpEQ32:
            case Iop_CasCmpEQ32:
               ccOp = NMcc_EQ;
               break;

            case Iop_CmpNE32:
               ccOp = NMcc_NE;
               break;

            case Iop_CmpLT32S:
               ccOp = NMcc_LTS;
               break;

            case Iop_CmpLT32U:
               ccOp = NMcc_LTU;
               break;

            case Iop_CmpLE32S:
               ccOp = NMcc_LES;
               break;

            case Iop_CmpLE32U:
               ccOp = NMcc_LEU;
               break;

            default:
               ccOp = NMcc_INVALID;
               break;
         }

         if (ccOp != NMcc_INVALID) {
            HReg dst = newVRegI(env);
            HReg r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r2 = iselWordExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, NANOMIPSInstr_Cmp(ccOp, dst, r1, r2));
            return dst;
         }

         switch (e->Iex.Binop.op) {
            case Iop_MullU8: {
               HReg r_dst = newVRegI(env);
               HReg r_tmp = newVRegI(env);
               HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
               HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_ANDI, r_dst, r_srcL, 0xFF));
               addInstr(env, NANOMIPSInstr_Imm(NMimm_ANDI, r_tmp, r_srcR, 0xFF));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_MULU, r_dst, r_dst, r_tmp));
               return r_dst;
            }

            case Iop_MullU16: {
               HReg r_dst = newVRegI(env);
               HReg r_tmp = newVRegI(env);
               HReg r_mask = newVRegI(env);
               HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
               HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_LI, r_mask, INVALID_HREG,
                                               0xFFFF));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_AND, r_dst, r_srcL, r_mask));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_AND, r_tmp, r_srcR, r_mask));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_MULU, r_dst, r_dst, r_tmp));
               return r_dst;
            }

            case Iop_8HLto16:
            case Iop_16HLto32: {
               HReg r_dst = newVRegI(env);
               HReg r_tmp = newVRegI(env);
               HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
               HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);

               switch (e->Iex.Binop.op) {
                  case Iop_8HLto16:
                     addInstr(env, NANOMIPSInstr_Imm(NMimm_SLL, r_tmp, r_srcL, 8));
                     addInstr(env, NANOMIPSInstr_Imm(NMimm_ANDI, r_dst, r_srcR,
                                                     0xFF));
                     break;

                  case Iop_16HLto32: {
                     HReg r_mask = newVRegI(env);
                     addInstr(env, NANOMIPSInstr_Imm(NMimm_LI, r_mask,
                                                     INVALID_HREG, 0xFFFF));
                     addInstr(env, NANOMIPSInstr_Imm(NMimm_SLL, r_tmp,
                                                     r_srcL, 16));
                     addInstr(env, NANOMIPSInstr_Alu(NMalu_AND, r_dst, r_srcR,
                                                     r_mask));
                  }
                  break;

                  default:
                     vassert(0);
               }

               addInstr(env, NANOMIPSInstr_Alu(NMalu_OR, r_dst, r_dst, r_tmp));
               return r_dst;
            }

            default:
               break;
         }

         vex_printf("Unimplemented binop ");
         ppIROp(e->Iex.Binop.op);
         vpanic("\n");

         break;
      }

      case Iex_Unop: {
         IROp op_unop = e->Iex.Unop.op;

         switch (op_unop) {
            case Iop_1Sto8:
            case Iop_1Sto16:
            case Iop_1Sto32: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_SGN, r_dst, r_src, 1));
               return r_dst;
            }

            case Iop_16to8:
            case Iop_32to8: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_SGN, r_dst, r_src, 8));
               return r_dst;
            }

            case Iop_32to16: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_SGN, r_dst, r_src, 16));
               return r_dst;
            }

            case Iop_1Uto8:
            case Iop_1Uto32:
            case Iop_8Sto16:
            case Iop_8Sto32:
            case Iop_16Sto32:
               return iselWordExpr_R(env, e->Iex.Unop.arg);

            case Iop_64to32: {
               HReg rHi, rLo;
               iselInt64Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
               return rLo;
            }

            case Iop_64HIto32: {
               HReg rHi, rLo;
               iselInt64Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
               return rHi;
            }

            case Iop_32to1: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_ANDI, r_dst, r_src, 1));
               return r_dst;
            }

            case Iop_8Uto16:
            case Iop_8Uto32: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_ANDI, r_dst, r_src,
                                               0xFF));
               return r_dst;
            }

            case Iop_16Uto32: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_LI, r_dst, INVALID_HREG,
                                               0xFFFF));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_AND, r_dst, r_dst, r_src));
               return r_dst;
            }

            case Iop_Not1: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_XORI, r_dst, r_src, 1));
               return r_dst;
            }

            case Iop_Not8:
            case Iop_Not16:
            case Iop_Not32: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Alu(NMalu_NOR, r_dst, r_src, r_src));
               return r_dst;
            }

            case Iop_32HIto16: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_SRA, r_dst, r_src, 16));
               return r_dst;
            }

            case Iop_16HIto8: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_SRA, r_dst, r_src, 8));
               return r_dst;
            }

            case Iop_CmpNEZ8:
            case Iop_CmpNEZ16:
            case Iop_CmpNEZ32: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Cmp(NMcc_NE, r_dst, r_src,
                                               Zero));
               return r_dst;
            }

            case Iop_CmpwNEZ32: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Cmp(NMcc_NE, r_dst, r_src,
                                               Zero));
               addInstr(env, NANOMIPSInstr_Imm(NMimm_SGN, r_dst, r_dst, 1));
               return r_dst;
            }

            case Iop_Clz32: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Unary(NMun_CLZ, r_dst, r_src));
               return r_dst;
            }

            case Iop_Left8:
            case Iop_Left16:
            case Iop_Left32: {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Alu(NMalu_SUB, r_dst, Zero, r_src));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_OR, r_dst, r_dst,
                                               r_src));
               return r_dst;
            }

            default:
               break;
         }

         vex_printf("Unimplemented unop ");
         ppIROp(e->Iex.Unop.op);
         vpanic("\n");
      }

      case Iex_Qop: {
         HReg dst = newVRegI(env);
         HReg src1 = iselWordExpr_R(env, e->Iex.Qop.details->arg1);
         UChar src2 = e->Iex.Qop.details->arg2->Iex.Const.con->Ico.U8;
         UChar src3 = e->Iex.Qop.details->arg3->Iex.Const.con->Ico.U8;
         UChar src4 = e->Iex.Qop.details->arg4->Iex.Const.con->Ico.U8;
         UInt imm = (src3 << 6) | (src4 << 6) | src2;
         switch (e->Iex.Qop.details->op) {
           case Iop_Rotx32:
             addInstr(env, NANOMIPSInstr_Imm(NMimm_ROTX, dst, src1, imm));
             return dst;
           default:
             break;
          }
          break;
      }

      case Iex_ITE: {
         vassert(typeOfIRExpr(env->type_env, e->Iex.ITE.cond) == Ity_I1);
         HReg r0     = iselWordExpr_R(env, e->Iex.ITE.iffalse);
         HReg r1     = iselWordExpr_R(env, e->Iex.ITE.iftrue);
         HReg r_cond = iselWordExpr_R(env, e->Iex.ITE.cond);
         HReg r_dst = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(r_dst, r0));
         addInstr(env, NANOMIPSInstr_MoveCond(NMMoveCond_movn, r_dst,
                                              r1, r_cond));
         return r_dst;
      }

      case Iex_Const: {
         HReg r_dst = newVRegI(env);
         addInstr(env, NANOMIPSInstr_Imm(NMimm_LI, r_dst, INVALID_HREG,
                                         extractConst(e->Iex.Const.con)));
         return r_dst;
      }

      case Iex_CCall: {
         HReg     r_dst = newVRegI(env);
         UInt   addToSp = 0;
         RetLoc rloc    = mk_RetLoc_INVALID();

         /* Be very restrictive for now. Only 32-bit ints allowed for
            args, and 32 bits for return type. Don't forget to change
            the RetLoc if more return types are allowed in future. */
         vassert(Ity_I32 == e->Iex.CCall.retty);

         /* Marshal args, do the call, clear stack. */
         doHelperCall(&rloc, env, NULL /*guard*/, e->Iex.CCall.cee,
                      e->Iex.CCall.retty, e->Iex.CCall.args);
         vassert(is_sane_RetLoc(rloc));
         vassert(rloc.pri == RLPri_Int);
         vassert(addToSp == 0);
         addInstr(env, mk_iMOVds_RR(r_dst, hregNANOMIPS_GPR4()));
         return r_dst;
      }

      default:
         break;
   }

   ppIRExpr(e);
   vpanic("iselWordExpr_R(NANOMIPS): cannot reduce tree");
}

/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64 bit)                ---*/
/*---------------------------------------------------------*/

/* Compute a 64-bit value into the register pair HI, LO.
   HI and LO must not be changed by subsequent code emitted
   by the caller. */
static void iselInt64Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e)
{
   iselInt64Expr_wrk(rHi, rLo, env, e);
   vassert(hregClass(*rHi) == HRcInt32);
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcInt32);
   vassert(hregIsVirtual(*rLo));
}

static void iselInt64Expr_wrk(HReg * rHi, HReg * rLo, ISelEnv * env,
                              IRExpr * e)
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I64);

   switch (e->tag) {
      case Iex_RdTmp:
         lookupIRTemp64(rHi, rLo, env, e->Iex.RdTmp.tmp);
         return;

      case Iex_Load: {
         HReg tLo = newVRegI(env);
         HReg tHi = newVRegI(env);
         HReg r_addr = iselWordExpr_R(env, e->Iex.Load.addr);
         addInstr(env, NANOMIPSInstr_Load(4, tLo, r_addr, 0));
         addInstr(env, NANOMIPSInstr_Load(4, tHi, r_addr, 4));
         *rHi = tHi;
         *rLo = tLo;
         return;
      }

      case Iex_Get: {
         HReg tLo = newVRegI(env);
         HReg tHi = newVRegI(env);
         vassert((e->Iex.Get.offset < 0x1000 - 4) && (e->Iex.Get.offset >= 0));
         addInstr(env, NANOMIPSInstr_Load(4, tLo, GuestStatePointer,
                                          e->Iex.Get.offset));
         addInstr(env, NANOMIPSInstr_Load(4, tHi, GuestStatePointer,
                                          e->Iex.Get.offset + 4));
         *rHi = tHi;
         *rLo = tLo;
         return;
      }

      case Iex_Binop: {
         switch (e->Iex.Binop.op) {
            case Iop_DivModS32to32: {
               HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
               HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
               HReg tLo = newVRegI(env);
               HReg tHi = newVRegI(env);
               addInstr(env, NANOMIPSInstr_Alu(NMalu_DIV, tLo, r_srcL, r_srcR));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_MOD, tHi, r_srcL, r_srcR));
               *rHi = tHi;
               *rLo = tLo;
               return;
            }

            case Iop_DivModU32to32: {
               HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
               HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
               HReg tLo = newVRegI(env);
               HReg tHi = newVRegI(env);
               addInstr(env, NANOMIPSInstr_Alu(NMalu_DIVU, tLo, r_srcL, r_srcR));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_MODU, tHi, r_srcL, r_srcR));
               *rHi = tHi;
               *rLo = tLo;
               return;
            }

            case Iop_MullS32: {
               HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
               HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
               HReg tLo = newVRegI(env);
               HReg tHi = newVRegI(env);
               addInstr(env, NANOMIPSInstr_Alu(NMalu_MUL, tLo, r_srcL, r_srcR));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_MUH, tHi, r_srcL, r_srcR));
               *rHi = tHi;
               *rLo = tLo;
               return;
            }

            case Iop_MullU32: {
               HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
               HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
               HReg tLo = newVRegI(env);
               HReg tHi = newVRegI(env);
               addInstr(env, NANOMIPSInstr_Alu(NMalu_MULU, tLo, r_srcL, r_srcR));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_MUHU, tHi, r_srcL, r_srcR));
               *rHi = tHi;
               *rLo = tLo;
               return;
            }

            case Iop_Shr64: {
#if defined (_MIPSEL)
               HReg a0, a1, sa;
               HReg a0tmp = newVRegI(env);
               HReg a1tmp = newVRegI(env);
               HReg a2 = newVRegI(env);
               HReg a3 = newVRegI(env);
               HReg a4 = newVRegI(env);

               iselInt64Expr(&a1, &a0, env, e->Iex.Binop.arg1);
               sa = iselWordExpr_R(env, e->Iex.Binop.arg2);

               /* andi a2, %sa, 0x3f */
               addInstr(env, NANOMIPSInstr_Imm(NMimm_ANDI, a2, sa, 0x3f));
               /* nor a4, zero, a2 */
               addInstr(env, NANOMIPSInstr_Alu(NMalu_NOR, a4, Zero, a2));
               /* sll a3, a1, 1 */
               addInstr(env, NANOMIPSInstr_Imm(NMimm_SLL, a3, a1, 0x1));
               /* sllv a3, a3, a4 */
               addInstr(env, NANOMIPSInstr_Alu(NMalu_SLL, a3, a3, a4));
               /* srlv a0, a0, a2 */
               addInstr(env, NANOMIPSInstr_Alu(NMalu_SRL, a0tmp, a0, a2));
               /* andi a4, a2, 0x20 */
               addInstr(env, NANOMIPSInstr_Imm(NMimm_ANDI, a4, a2, 0x20));
               /* srlv a2, a1, a2 */
               addInstr(env, NANOMIPSInstr_Alu(NMalu_SRL, a2, a1, a2));
               /* or a0, a0, a3 */
               addInstr(env, NANOMIPSInstr_Alu(NMalu_OR, a0tmp, a0tmp, a3));
               /* move a1, a2 */
               addInstr(env, mk_iMOVds_RR(a1tmp, a2));
               /* movn a1, zero, a4 */
               addInstr(env, NANOMIPSInstr_MoveCond(NMMoveCond_movn, a1tmp,
                                                    Zero, a4));
               /* movn a0, a2, a4 */
               addInstr(env, NANOMIPSInstr_MoveCond(NMMoveCond_movn, a0tmp,
                                                    a2, a4));

               *rHi = a1tmp;
               *rLo = a0tmp;
               return;
#elif defined (_MIPSEB)
               /* 64-bit logical shift right based on what gcc generates:
                  <shift>:
                  nor  v0, zero, a2
                  sll  a3, a0, 0x1
                  sllv a3, a3, v0
                  srlv v1, a1, a2
                  andi v0, a2, 0x20
                  or   v1, a3, v1
                  srlv a2, a0, a2
                  movn v1, a2, v0
                  movn a2, zero, v0
                  jr   ra
                  move v0, a2
               */
               /* unimplemented yet */
               vassert(0);

#endif
            }

            case Iop_32HLto64:
               *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
               *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2);

               return;

            case Iop_Or64: {
               HReg rHi_srcL, rLo_srcL;
               HReg rHi_srcR, rLo_srcR;
               HReg rHi_dst = newVRegI(env);
               HReg rLo_dst = newVRegI(env);
               iselInt64Expr(&rHi_srcL, &rLo_srcL, env, e->Iex.Binop.arg1);
               iselInt64Expr(&rHi_srcR, &rLo_srcR, env, e->Iex.Binop.arg2);
               addInstr(env, NANOMIPSInstr_Alu(NMalu_OR, rHi_dst, rHi_srcL,
                                               rHi_srcR));
               addInstr(env, NANOMIPSInstr_Alu(NMalu_OR, rLo_dst, rLo_srcL,
                                               rLo_srcR));
               *rHi = rHi_dst;
               *rLo = rLo_dst;

               return;
            }

            default:
               break;
         }

         vex_printf("Unimplemented binop ");
         ppIROp(e->Iex.Binop.op);
         vpanic("\n");

         break;
      }

      case Iex_Unop: {
         switch (e->Iex.Unop.op) {
            case Iop_1Sto64: {
               HReg rHi_dst = newVRegI(env);
               HReg rLo_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_SGN, rLo_dst, r_src, 1));
               addInstr(env, mk_iMOVds_RR(rHi_dst, rLo_dst));
               *rHi = rHi_dst;
               *rLo = rLo_dst;
               return;
            }

            default:
               break;
         }

         vex_printf("Unimplemented unop ");
         ppIROp(e->Iex.Unop.op);
         vpanic("\n");

         break;
      }

      default:
         break;
   }

   ppIRExpr(e);
   vpanic("iselInt64Expr(NANOMIPS): cannot reduce tree");
}

/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/
static void iselStmt(ISelEnv * env, IRStmt * stmt)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- ");
      ppIRStmt(stmt);
      vex_printf("\n");
   }

   switch (stmt->tag) {
      case Ist_Store: {
         IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);
         HReg r_addr = iselWordExpr_R(env, stmt->Ist.Store.addr);

         if (tyd == Ity_I8 || tyd == Ity_I16 || tyd == Ity_I32) {
            HReg r_src = iselWordExpr_R(env, stmt->Ist.Store.data);
            addInstr(env, NANOMIPSInstr_Store(sizeofIRType(tyd),
                                              r_addr, 0, r_src));
            return;
         } else if (tyd == Ity_I64) {
            HReg vHi, vLo;
            iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Store.data);
            addInstr(env, NANOMIPSInstr_Store(4, r_addr, 0, vLo));
            addInstr(env, NANOMIPSInstr_Store(4, r_addr, 4, vHi));
            return;
         }

         break;
      }

      case Ist_Put: {
         IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
         vassert(stmt->Ist.Put.offset >= 0);

         if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32) {
            HReg r_src = iselWordExpr_R(env, stmt->Ist.Put.data);
            vassert(stmt->Ist.Put.offset < 0x1000);
            addInstr(env, NANOMIPSInstr_Store(sizeofIRType(ty),
                                              GuestStatePointer,
                                              stmt->Ist.Put.offset, r_src));
            return;
         } else if (ty == Ity_I64) {
            HReg vHi, vLo;
            vassert(stmt->Ist.Put.offset < 0x1000 - 4);
            iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Put.data);
            addInstr(env, NANOMIPSInstr_Store(4, GuestStatePointer,
                                              stmt->Ist.Put.offset, vLo));
            addInstr(env, NANOMIPSInstr_Store(4, GuestStatePointer,
                                              stmt->Ist.Put.offset + 4,
                                              vHi));
            return;
         }

         break;
      }

      case Ist_WrTmp: {
         IRTemp tmp = stmt->Ist.WrTmp.tmp;
         IRType ty = typeOfIRTemp(env->type_env, tmp);

         if (ty == Ity_I1 || ty == Ity_I8 || ty == Ity_I16 ||
               ty == Ity_I32) {
            HReg r_dst = lookupIRTemp(env, tmp);
            HReg r_src = iselWordExpr_R(env, stmt->Ist.WrTmp.data);
            addInstr(env, mk_iMOVds_RR(r_dst, r_src));
            return;
         } else if (ty == Ity_I64) {
            HReg rHi, rLo, dstHi, dstLo;
            iselInt64Expr(&rHi, &rLo, env, stmt->Ist.WrTmp.data);
            lookupIRTemp64(&dstHi, &dstLo, env, tmp);
            addInstr(env, mk_iMOVds_RR(dstHi, rHi));
            addInstr(env, mk_iMOVds_RR(dstLo, rLo));
            return;
         }

         break;
      }

      case Ist_Dirty: {
         IRDirty *d = stmt->Ist.Dirty.details;
         IRType retty = Ity_INVALID;

         if (d->tmp != IRTemp_INVALID)
            retty = typeOfIRTemp(env->type_env, d->tmp);

         vassert((retty == Ity_INVALID) ||
                 (retty == Ity_I32) ||
                 (retty == Ity_I64) ||
                 (retty == Ity_I8) ||
                 (retty == Ity_I16));

         /* Marshal args, do the call, clear stack, set the return value
           to 0x555..555 if this is a conditional call that returns a
           value and the call is skipped. */
         RetLoc rloc = mk_RetLoc_INVALID();
         doHelperCall(&rloc, env, d->guard, d->cee, retty, d->args);
         vassert(is_sane_RetLoc(rloc));

         /* Now figure out what to do with the returned value, if any. */
         switch (retty) {
            case Ity_INVALID: {
               vassert(d->tmp == IRTemp_INVALID);
               vassert(rloc.pri == RLPri_None);
               return;
            }

            case Ity_I32:
            case Ity_I16:
            case Ity_I8: {
               HReg r_dst = lookupIRTemp(env, d->tmp);
               vassert(rloc.pri == RLPri_Int);
               addInstr(env, mk_iMOVds_RR(r_dst, hregNANOMIPS_GPR4()));
               return;
            }

            case Ity_I64: {
               HReg rHi = newVRegI(env);
               HReg rLo = newVRegI(env);
               HReg dstHi, dstLo;
               vassert(rloc.pri == RLPri_2Int);
               addInstr(env, mk_iMOVds_RR(rLo, hregNANOMIPS_GPR4()));
               addInstr(env, mk_iMOVds_RR(rHi, hregNANOMIPS_GPR5()));
               lookupIRTemp64(&dstHi, &dstLo, env, d->tmp);
               addInstr(env, mk_iMOVds_RR(dstHi, rHi));
               addInstr(env, mk_iMOVds_RR(dstLo, rLo));
               return;
            }

            default:
               vassert(0);
         }

         break;
      }

      case Ist_LLSC: {
         IRTemp res = stmt->Ist.LLSC.result;
         IRType tyAddr = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.addr);

         if (tyAddr != Ity_I32)
            break;

         if (stmt->Ist.LLSC.storedata == NULL) {
            /* LL */
            HReg r_addr = iselWordExpr_R(env, stmt->Ist.LLSC.addr);
            HReg r_dst = lookupIRTemp(env, res);

            addInstr(env, NANOMIPSInstr_LoadL(4, r_dst, r_addr, 0));
            return;
         } else {
            /* SC */
            HReg r_addr = iselWordExpr_R(env, stmt->Ist.LLSC.addr);
            HReg r_src = iselWordExpr_R(env, stmt->Ist.LLSC.storedata);
            HReg r_dst = lookupIRTemp(env, res);

            addInstr(env, mk_iMOVds_RR(r_dst, r_src));
            addInstr(env, NANOMIPSInstr_StoreC(4, r_addr, 0, r_dst));
            return;
         }
         break;
       /* NOTREACHED */}
      case Ist_CAS:
         if (stmt->Ist.CAS.details->oldHi == IRTemp_INVALID) {
            IRCAS *cas = stmt->Ist.CAS.details;
            HReg old   = lookupIRTemp(env, cas->oldLo);
            HReg addr  = iselWordExpr_R(env, cas->addr);
            HReg expd  = iselWordExpr_R(env, cas->expdLo);
            HReg data  = iselWordExpr_R(env, cas->dataLo);
            vassert(typeOfIRTemp(env->type_env, cas->oldLo) == Ity_I32);
            addInstr(env, NANOMIPSInstr_Cas(4, old, old, addr, expd, expd, data, data));
         }
         else {
            IRCAS *cas = stmt->Ist.CAS.details;
            HReg oldHi   = lookupIRTemp(env, cas->oldHi);
            HReg oldLo   = lookupIRTemp(env, cas->oldLo);
            HReg addr  = iselWordExpr_R(env, cas->addr);
            HReg expdHi  = iselWordExpr_R(env, cas->expdHi);
            HReg expdLo  = iselWordExpr_R(env, cas->expdLo);
            HReg dataHi  = iselWordExpr_R(env, cas->dataHi);
            HReg dataLo  = iselWordExpr_R(env, cas->dataLo);
            vassert(typeOfIRTemp(env->type_env, cas->oldLo) == Ity_I32);
            addInstr(env, NANOMIPSInstr_Cas(8, oldLo, oldHi, addr,
                                            expdLo, expdHi, dataLo, dataHi));
         }
         return;

      case Ist_IMark:
      case Ist_AbiHint:
      case Ist_NoOp:
         return;

      case Ist_Exit: {
         Addr dst = extractConst(stmt->Ist.Exit.dst);
         HReg cond = iselWordExpr_R(env, stmt->Ist.Exit.guard);

         switch (stmt->Ist.Exit.jk) {
            /* case Ijk_Ret: */
            case Ijk_Boring:
            case Ijk_Call: {
               vassert(stmt->Ist.Exit.offsIP >= 0);
               vassert(stmt->Ist.Exit.offsIP <= 0x1000);

               if (env->chainingAllowed) {
                  Bool toFastEP = (dst > (Addr)env->max_ga);
                  addInstr(env, NANOMIPSInstr_XDirect(dst, GuestStatePointer,
                                                      stmt->Ist.Exit.offsIP,
                                                      cond, toFastEP));
               } else {
                  HReg r = newVRegI(env);
                  addInstr(env, NANOMIPSInstr_Imm(NMimm_LI, r, INVALID_HREG,
                                                  dst));
                  addInstr(env, NANOMIPSInstr_XAssisted(r, GuestStatePointer,
                                                        stmt->Ist.Exit.offsIP,
                                                        cond, Ijk_Boring));
               }

               return;
            }

            case Ijk_ClientReq:
            case Ijk_EmFail:
            case Ijk_EmWarn:
            case Ijk_NoDecode:
            case Ijk_NoRedir:
            case Ijk_SigBUS:
            case Ijk_Yield:
            case Ijk_SigTRAP:
            case Ijk_SigFPE_IntDiv:
            case Ijk_SigFPE_IntOvf:
            case Ijk_Sys_syscall:
            case Ijk_InvalICache: {
               HReg r = newVRegI(env);
               addInstr(env, NANOMIPSInstr_Imm(NMimm_LI, r, INVALID_HREG,
                                               dst));
               vassert(stmt->Ist.Exit.offsIP >= 0);
               vassert(stmt->Ist.Exit.offsIP <= 0x1000);
               addInstr(env, NANOMIPSInstr_XAssisted(r, GuestStatePointer,
                                                     stmt->Ist.Exit.offsIP,
                                                     cond, stmt->Ist.Exit.jk));
               return;
            }

            default:
               vassert(0);
         };

         break;
      }

      default:
         break;
   }

   vex_printf("stmt_fail tag: 0x%x\n", stmt->tag);
   ppIRStmt(stmt);
   vpanic("iselStmt:\n");
}


/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/
static void iselNext(ISelEnv * env,
                     IRExpr * next, IRJumpKind jk, Int offsIP)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf( "\n-- PUT(%d) = ", offsIP);
      ppIRExpr( next );
      vex_printf( "; exit-");
      ppIRJumpKind(jk);
      vex_printf( "\n");
   }

   /* Case: boring transfer to known address */
   if (next->tag == Iex_Const) {
      IRConst* cdst = next->Iex.Const.con;
      vassert(cdst->tag == Ico_U32);

      if (jk == Ijk_Boring || jk == Ijk_Call) {
         vassert(offsIP >= 0);
         vassert(offsIP < 0x1000);

         /* Boring transfer to known address */
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = (((Addr32)cdst->Ico.U32) > (Addr32)env->max_ga);
            addInstr(env, NANOMIPSInstr_XDirect((Addr)cdst->Ico.U32,
                                                GuestStatePointer, offsIP,
                                                INVALID_HREG, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselWordExpr_R(env, next);
            addInstr(env, NANOMIPSInstr_XAssisted(r, GuestStatePointer, offsIP,
                                                  INVALID_HREG, Ijk_Boring));
         }

         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
      case Ijk_Boring:
      case Ijk_Ret:
      case Ijk_Call: {
         HReg r = iselWordExpr_R(env, next);
         vassert(offsIP >= 0);
         vassert(offsIP < 0x1000);

         if (env->chainingAllowed) {
            addInstr(env, NANOMIPSInstr_XIndir(r, GuestStatePointer, offsIP,
                                               INVALID_HREG));
         } else {
            addInstr(env, NANOMIPSInstr_XAssisted(r, GuestStatePointer, offsIP,
                                                  INVALID_HREG, Ijk_Boring));
         }

         return;
      }

      default:
         break;
   }

   /* Case: assisted transfer to arbitrary address */
   switch (jk) {
      /* Keep this list in sync with that for Ist_Exit above */
      case Ijk_ClientReq:
      case Ijk_EmFail:
      case Ijk_EmWarn:
      case Ijk_NoDecode:
      case Ijk_NoRedir:
      case Ijk_SigBUS:
      case Ijk_SigILL:
      case Ijk_SigTRAP:
      case Ijk_SigFPE_IntDiv:
      case Ijk_SigFPE_IntOvf:
      case Ijk_Sys_syscall:
      case Ijk_InvalICache: {
         HReg r = iselWordExpr_R(env, next);
         vassert(offsIP >= 0);
         vassert(offsIP < 0x1000);
         addInstr(env, NANOMIPSInstr_XAssisted(r, GuestStatePointer,
                                               offsIP, INVALID_HREG, jk));
         return;
      }

      default:
         break;
   }

   vex_printf("\n-- PUT(%d) = ", offsIP);
   ppIRExpr(next );
   vex_printf("; exit-");
   ppIRJumpKind(jk);
   vex_printf("\n");
   vassert(0);  /* are we expecting any other kind? */
}

/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire BB to NANOMIPS code. */
HInstrArray *iselSB_NANOMIPS(const IRSB * bb,
                             VexArch arch_host,
                             const VexArchInfo * archinfo_host,
                             const VexAbiInfo * vbi,
                             Int offs_Host_EvC_Counter,
                             Int offs_Host_EvC_FailAddr,
                             Bool chainingAllowed,
                             Bool addProfInc,
                             Addr max_ga)
{
   Int      i, j;
   HReg     hreg, hregHI;
   ISelEnv *env;
   hwcaps_host = archinfo_host->hwcaps;
   /* sanity ... */
   vassert(arch_host == VexArchNANOMIPS);
   /* Check that the host's endianness is as expected. */
   vassert(archinfo_host->endness == VexEndnessLE
           || archinfo_host->endness == VexEndnessBE);
   /* Make up an initial environment to use. */
   env = LibVEX_Alloc_inline(sizeof(ISelEnv));
   env->vreg_ctr = 0;
   /* Set up output code array. */
   env->code = newHInstrArray();
   /* Copy BB's type env. */
   env->type_env = bb->tyenv;
   /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
      change as we go along. */
   env->n_vregmap = bb->tyenv->types_used;
   env->vregmap   = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
   env->vregmapHI = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
   env->hwcaps          = hwcaps_host;
   env->chainingAllowed = chainingAllowed;
   env->max_ga          = max_ga;
   /* For each IR temporary, allocate a suitably-kinded virtual
      register. */
   j = 0;

   for (i = 0; i < env->n_vregmap; i++) {
      hregHI = hreg = INVALID_HREG;

      switch (bb->tyenv->types[i]) {
         case Ity_I1:
         case Ity_I8:
         case Ity_I16:
         case Ity_I32:
            hreg = mkHReg(True, HRcInt32, 0, j++);
            break;

         case Ity_I64:
            hreg   = mkHReg(True, HRcInt32, 0, j++);
            hregHI = mkHReg(True, HRcInt32, 0, j++);
            break;

         default:
            ppIRType(bb->tyenv->types[i]);
            vpanic("iselBB(nanomips): IRTemp type");
            break;
      }

      env->vregmap[i] = hreg;
      env->vregmapHI[i] = hregHI;
   }

   env->vreg_ctr = j;
   /* The very first instruction must be an event check. */
   vassert(offs_Host_EvC_Counter >= 0);
   vassert(offs_Host_EvC_FailAddr >= 0);
   vassert(offs_Host_EvC_Counter < 0x1000);
   vassert(offs_Host_EvC_FailAddr < 0x1000);
   addInstr(env, NANOMIPSInstr_EvCheck(GuestStatePointer,
                                       offs_Host_EvC_Counter,
                                       GuestStatePointer,
                                       offs_Host_EvC_FailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, NANOMIPSInstr_ProfInc());
   }

   /* Ok, finally we can iterate over the statements. */
   for (i = 0; i < bb->stmts_used; i++)
      iselStmt(env, bb->stmts[i]);

   iselNext(env, bb->next, bb->jumpkind, bb->offsIP);
   /* record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;
   return env->code;
}

/*---------------------------------------------------------------*/
/*--- end                                host_nanomips_isel.c ---*/
/*---------------------------------------------------------------*/
