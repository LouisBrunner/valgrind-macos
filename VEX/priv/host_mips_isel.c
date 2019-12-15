
/*---------------------------------------------------------------*/
/*--- begin                                  host_mips_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2017 RT-RK
      mips-valgrind@rt-rk.com

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_generic_simd64.h"  /* for 64-bit SIMD helpers */
#include "host_mips_defs.h"

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

static Bool mode64 = False;

/* Host CPU has FPU and 32 dbl. prec. FP registers. */
static Bool fp_mode64 = False;

/* Host hwcaps */
static UInt hwcaps_host = 0;

/* Host CPU has MSA ASE */
static Bool has_msa = False;

/* GPR register class for mips32/64 */
#define HRcGPR(_mode64) ((_mode64) ? HRcInt64 : HRcInt32)

/* FPR register class for mips32/64 */
#define HRcFPR(_mode64) ((_mode64) ? HRcFlt64 : HRcFlt32)

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
      Bool         mode64;
      Bool         fp_mode64;

      Bool         chainingAllowed;
      Addr64       max_ga;

      /* These are modified as we go along. */
      HInstrArray* code;
      Int          vreg_ctr;
   }
   ISelEnv;

static HReg lookupIRTemp(ISelEnv * env, IRTemp tmp)
{
   vassert(tmp < env->n_vregmap);
   return env->vregmap[tmp];
}

static void lookupIRTemp64(HReg * vrHI, HReg * vrLO, ISelEnv * env, IRTemp tmp)
{
   vassert(tmp < env->n_vregmap);
   vassert(! hregIsInvalid(env->vregmapHI[tmp]));
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void
lookupIRTempPair(HReg * vrHI, HReg * vrLO, ISelEnv * env, IRTemp tmp)
{
   vassert(env->mode64);
   vassert(tmp < env->n_vregmap);
   vassert(! hregIsInvalid(env->vregmapHI[tmp]));
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr(ISelEnv * env, MIPSInstr * instr)
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppMIPSInstr(instr, mode64);
      vex_printf("\n");
   }
}

static HReg newVRegI(ISelEnv * env)
{
   HReg reg = mkHReg(True/*virtual reg*/,
                     HRcGPR(env->mode64), 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegD(ISelEnv * env)
{
   HReg reg = mkHReg(True/*virtual reg*/,
                     HRcFlt64, 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF(ISelEnv * env)
{
   HReg reg = mkHReg(True/*virtual reg*/,
                     HRcFPR(env->mode64), 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegV ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcVec128, 0, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static void add_to_sp(ISelEnv * env, UInt n)
{
   HReg sp = StackPointer(mode64);
   vassert(n < 256 && (n % 8) == 0);
   if (mode64)
      addInstr(env, MIPSInstr_Alu(Malu_DADD, sp, sp, MIPSRH_Imm(True,
                                                                toUShort(n))));
   else
      addInstr(env, MIPSInstr_Alu(Malu_ADD, sp, sp, MIPSRH_Imm(True,
                                                               toUShort(n))));
}

static void sub_from_sp(ISelEnv * env, UInt n)
{
   HReg sp = StackPointer(mode64);
   vassert(n < 256 && (n % 8) == 0);
   if (mode64)
      addInstr(env, MIPSInstr_Alu(Malu_DSUB, sp, sp,
                                  MIPSRH_Imm(True, toUShort(n))));
   else
      addInstr(env, MIPSInstr_Alu(Malu_SUB, sp, sp,
                                  MIPSRH_Imm(True, toUShort(n))));
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
/* 32-bit mode: Compute an I8/I16/I32 into a RH
                (reg-or-halfword-immediate).
   It's important to specify whether the immediate is to be regarded
   as signed or not.  If yes, this will never return -32768 as an
   immediate; this guaranteed that all signed immediates that are
   return can have their sign inverted if need be.
*/
static MIPSRH *iselWordExpr_RH_wrk(ISelEnv * env, Bool syned, IRExpr * e);
static MIPSRH *iselWordExpr_RH(ISelEnv * env, Bool syned, IRExpr * e);

/* Compute an I8 into a reg-or-5-bit-unsigned-immediate, the latter being an
   immediate in the range 1 .. 31 inclusive.  Used for doing shift amounts. */
static MIPSRH *iselWordExpr_RH5u_wrk(ISelEnv * env, IRExpr * e);
static MIPSRH *iselWordExpr_RH5u(ISelEnv * env, IRExpr * e);

/* Compute an I8 into a reg-or-6-bit-unsigned-immediate, the latter being an
   immediate in the range 1 .. 63 inclusive.  Used for doing shift amounts. */
static MIPSRH *iselWordExpr_RH6u_wrk(ISelEnv * env, IRExpr * e);
static MIPSRH *iselWordExpr_RH6u(ISelEnv * env, IRExpr * e);

/* Compute an I8 into a reg-or-7-bit-unsigned-immediate, the latter being an
   immediate in the range 1 .. 127 inclusive.  Used for doing shift amounts. */
static MIPSRH *iselWordExpr_RH7u_wrk(ISelEnv * env, IRExpr * e);
static MIPSRH *iselWordExpr_RH7u(ISelEnv * env, IRExpr * e);

/* compute an I8/I16/I32 into a GPR*/
static HReg iselWordExpr_R_wrk(ISelEnv * env, IRExpr * e);
static HReg iselWordExpr_R(ISelEnv * env, IRExpr * e);

/* compute an I32 into an AMode. */
static MIPSAMode *iselWordExpr_AMode_wrk(ISelEnv * env, IRExpr * e,
                                         IRType xferTy);
static MIPSAMode *iselWordExpr_AMode(ISelEnv * env, IRExpr * e, IRType xferTy);

static void iselInt64Expr_wrk(HReg * rHi, HReg * rLo, ISelEnv * env,
                              IRExpr * e);
static void iselInt64Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e);

/* 64-bit mode ONLY: compute an I128 into a GPR64 pair. */
static void iselInt128Expr_wrk(HReg * rHi, HReg * rLo,
                               ISelEnv * env, IRExpr * e);
static void iselInt128Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e);

static HReg iselV128Expr( ISelEnv* env, IRExpr* e );
static HReg iselV128Expr_wrk( ISelEnv* env, IRExpr* e );

static MIPSCondCode iselCondCode_wrk(ISelEnv * env, IRExpr * e);
static MIPSCondCode iselCondCode(ISelEnv * env, IRExpr * e);

static HReg iselDblExpr_wrk(ISelEnv * env, IRExpr * e);
static HReg iselDblExpr(ISelEnv * env, IRExpr * e);

static HReg iselFltExpr_wrk(ISelEnv * env, IRExpr * e);
static HReg iselFltExpr(ISelEnv * env, IRExpr * e);

static void set_MIPS_rounding_mode(ISelEnv * env, IRExpr * mode)
{
   /*
      rounding mode | MIPS | IR
      ------------------------
      to nearest    | 00  | 00
      to zero       | 01  | 11
      to +infinity  | 10  | 10
      to -infinity  | 11  | 01
    */
   /* rm_MIPS32  = XOR(rm_IR , (rm_IR << 1)) & 3 */
   HReg irrm = iselWordExpr_R(env, mode);
   HReg tmp = newVRegI(env);
   HReg fcsr_old = newVRegI(env);
   MIPSAMode *am_addr;

   addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp, irrm,
                                MIPSRH_Imm(False, 1)));
   addInstr(env, MIPSInstr_Alu(Malu_XOR, tmp, irrm, MIPSRH_Reg(tmp)));
   addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp, MIPSRH_Imm(False, 3)));
   /* save old value of FCSR */
   addInstr(env, MIPSInstr_MfFCSR(fcsr_old));
   sub_from_sp(env, 8); /*  Move SP down 8 bytes */
   am_addr = MIPSAMode_IR(0, StackPointer(mode64));

   /* store old FCSR to stack */
   addInstr(env, MIPSInstr_Store(4, am_addr, fcsr_old, mode64));

   /* set new value of FCSR */
   addInstr(env, MIPSInstr_MtFCSR(tmp));
}

static void set_MIPS_rounding_mode_MSA(ISelEnv * env, IRExpr * mode) {
   /*
      rounding mode | MIPS | IR
      ------------------------
      to nearest    | 00  | 00
      to zero       | 01  | 11
      to +infinity  | 10  | 10
      to -infinity  | 11  | 01
    */
   /* rm_MIPS32  = XOR(rm_IR , (rm_IR << 1)) & 3 */
   HReg irrm = iselWordExpr_R(env, mode);
   HReg tmp = newVRegI(env);
   HReg msacsr_old = newVRegI(env);
   MIPSAMode *am_addr;
   addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp, irrm,
                                MIPSRH_Imm(False, 1)));
   addInstr(env, MIPSInstr_Alu(Malu_XOR, tmp, irrm, MIPSRH_Reg(tmp)));
   addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp, MIPSRH_Imm(False, 3)));
   /* save old value of MSACSR */
   addInstr(env, MIPSInstr_MsaElm(MSA_CFCMSA, hregMIPS_GPR0(mode64), msacsr_old,
                                  MSA_DFN_W));
   sub_from_sp(env, 8); /*  Move SP down 8 bytes */
   am_addr = MIPSAMode_IR(0, StackPointer(mode64));
   /* store old MSACSR to stack */
   addInstr(env, MIPSInstr_Store(4, am_addr, msacsr_old, mode64));
   /* set new value of MSACSR */
   addInstr(env, MIPSInstr_MsaElm(MSA_CTCMSA, tmp, hregMIPS_GPR0(mode64),
                                  MSA_DFN_W));
}


static void set_guest_MIPS_rounding_mode_MSA(ISelEnv * env) {
   /*
      rounding mode | MIPS | IR
      ------------------------
      to nearest    | 00  | 00
      to zero       | 01  | 11
      to +infinity  | 10  | 10
      to -infinity  | 11  | 01
    */
   /* rm_MIPS32  = XOR(rm_IR , (rm_IR << 1)) & 3 */
   HReg irrm =  newVRegI(env);
   HReg msacsr_old = newVRegI(env);
   MIPSAMode *am_addr;
   MIPSAMode *rm_addr = MIPSAMode_IR(MSACSR_OFFSET(mode64),
                                     GuestStatePointer(mode64));
   addInstr(env, MIPSInstr_Load(4, irrm, rm_addr, mode64));
   /* save old value of MSACSR */
   addInstr(env, MIPSInstr_MsaElm(MSA_CFCMSA, hregMIPS_GPR0(mode64), msacsr_old,
                                  MSA_DFN_W));
   sub_from_sp(env, 8); /*  Move SP down 8 bytes */
   am_addr = MIPSAMode_IR(0, StackPointer(mode64));
   /* store old MSACSR to stack */
   addInstr(env, MIPSInstr_Store(4, am_addr, msacsr_old, mode64));
   /* set new value of MSACSR */
   addInstr(env, MIPSInstr_MsaElm(MSA_CTCMSA, irrm, hregMIPS_GPR0(mode64),
                                  MSA_DFN_W));
}


static void set_MIPS_rounding_default(ISelEnv * env)
{
   HReg fcsr = newVRegI(env);
   /* load as float */
   MIPSAMode *am_addr;
   am_addr = MIPSAMode_IR(0, StackPointer(mode64));

   addInstr(env, MIPSInstr_Load(4, fcsr, am_addr, mode64));

   add_to_sp(env, 8);  /* Reset SP */

   /* set new value of FCSR*/
   addInstr(env, MIPSInstr_MtFCSR(fcsr));
}

static void set_MIPS_rounding_default_MSA(ISelEnv * env) {
   HReg msacsr = newVRegI(env);
   /* load as float */
   MIPSAMode *am_addr;
   am_addr = MIPSAMode_IR(0, StackPointer(mode64));
   addInstr(env, MIPSInstr_Load(4, msacsr, am_addr, mode64));
   add_to_sp(env, 8);  /* Reset SP */
   /* set new value of FCSR*/
   addInstr(env, MIPSInstr_MsaElm(MSA_CTCMSA, msacsr, hregMIPS_GPR0(mode64),
                                  MSA_DFN_W));
}

/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

/* Make an int reg-reg move. */
static MIPSInstr *mk_iMOVds_RR(HReg r_dst, HReg r_src)
{
   vassert(hregClass(r_dst) == hregClass(r_src));
   vassert(hregClass(r_src) == HRcInt32 || hregClass(r_src) == HRcInt64);
   return MIPSInstr_Alu(Malu_OR, r_dst, r_src, MIPSRH_Reg(r_src));
}

/*---------------------------------------------------------*/
/*--- ISEL: Function call helpers                       ---*/
/*---------------------------------------------------------*/

/* Used only in doHelperCall.  See big comment in doHelperCall re
   handling of register-parameter args.  This function figures out
   whether evaluation of an expression might require use of a fixed
   register.  If in doubt return True (safe but suboptimal).
*/
static Bool mightRequireFixedRegs(IRExpr * e)
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

/* Load 2*I32 regs to fp reg */
static HReg mk_LoadRR32toFPR(ISelEnv * env, HReg r_srcHi, HReg r_srcLo)
{
   HReg fr_dst = newVRegD(env);
   MIPSAMode *am_addr0, *am_addr1;

   vassert(hregClass(r_srcHi) == HRcInt32);
   vassert(hregClass(r_srcLo) == HRcInt32);

   sub_from_sp(env, 16);  /* Move SP down 16 bytes */
   am_addr0 = MIPSAMode_IR(0, StackPointer(mode64));
   am_addr1 = MIPSAMode_IR(4, StackPointer(mode64));

   /* store hi,lo as Ity_I32's */
#if defined (_MIPSEL)
   addInstr(env, MIPSInstr_Store(4, am_addr0, r_srcLo, mode64));
   addInstr(env, MIPSInstr_Store(4, am_addr1, r_srcHi, mode64));
#elif defined (_MIPSEB)
   addInstr(env, MIPSInstr_Store(4, am_addr0, r_srcHi, mode64));
   addInstr(env, MIPSInstr_Store(4, am_addr1, r_srcLo, mode64));
#else
   /* Stop gcc on other platforms complaining about am_addr1 being set
      but not used. */
   (void)am_addr1;
#endif

   /* load as float */
   addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, fr_dst, am_addr0));

   add_to_sp(env, 16);  /* Reset SP */
   return fr_dst;
}

/* Do a complete function call.  |guard| is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional.  |retloc| is set to indicate where the
   return value is after the call.  The caller (of this fn) must
   generate code to add |stackAdjustAfterCall| to the stack pointer
   after the call is done. */

static void doHelperCall(/*OUT*/UInt*   stackAdjustAfterCall,
                         /*OUT*/RetLoc* retloc,
                         ISelEnv* env,
                         IRExpr* guard,
                         IRCallee* cee, IRType retTy, IRExpr** args )
{
   MIPSCondCode cc;
   HReg argregs[8];
   HReg tmpregs[8];
   Bool go_fast;
   Int n_args, i, argreg;
   UInt argiregs;
   HReg src = INVALID_HREG;

   /* Set default returns.  We'll update them later if needed. */
   *stackAdjustAfterCall = 0;
   *retloc               = mk_RetLoc_INVALID();

   /* These are used for cross-checking that IR-level constraints on
      the use of IRExpr_VECRET() and IRExpr_GSPTR() are observed. */
   UInt nVECRETs = 0;
   UInt nGSPTRs  = 0;

   /* MIPS O32 calling convention: up to four registers ($a0 ... $a3)
      are allowed to be used for passing integer arguments. They correspond
      to regs GPR4 ... GPR7. Note that the cee->regparms field is meaningless
      on MIPS host (since we only implement one calling convention) and so we
      always ignore it. */

   /* MIPS 64 calling convention: up to four registers ($a0 ... $a7)
      are allowed to be used for passing integer arguments. They correspond
      to regs GPR4 ... GPR11. Note that the cee->regparms field is meaningless
      on MIPS host (since we only implement one calling convention) and so we
      always ignore it. */

   /* The return type can be I{64,32,16,8} or V{128,256}.  In the
      latter two cases, it is expected that |args| will contain the
      special node IRExpr_VECRET(), in which case this routine
      generates code to allocate space on the stack for the vector
      return value.  Since we are not passing any scalars on the
      stack, it is enough to preallocate the return space before
      marshalling any arguments, in this case.

      |args| may also contain IRExpr_GSPTR(), in which case the value
      in the guest state pointer register is passed as the
      corresponding argument. */

   n_args = 0;
   for (i = 0; args[i]; i++) {
      IRExpr* arg = args[i];
      if (UNLIKELY(arg->tag == Iex_VECRET)) {
         nVECRETs++;
      } else if (UNLIKELY(arg->tag == Iex_GSPTR)) {
         nGSPTRs++;
      }
      n_args++;
   }

   if (n_args > MIPS_N_REGPARMS) {
      vpanic("doHelperCall(MIPS): cannot currently handle > 4 or 8 args");
   }
   if (mode64) {
      argregs[0] = hregMIPS_GPR4(mode64);
      argregs[1] = hregMIPS_GPR5(mode64);
      argregs[2] = hregMIPS_GPR6(mode64);
      argregs[3] = hregMIPS_GPR7(mode64);
      argregs[4] = hregMIPS_GPR8(mode64);
      argregs[5] = hregMIPS_GPR9(mode64);
      argregs[6] = hregMIPS_GPR10(mode64);
      argregs[7] = hregMIPS_GPR11(mode64);
      argiregs = 0;
      tmpregs[0] = tmpregs[1] = tmpregs[2] =
      tmpregs[3] = tmpregs[4] = tmpregs[5] =
      tmpregs[6] = tmpregs[7] = INVALID_HREG;
   } else {
      argregs[0] = hregMIPS_GPR4(mode64);
      argregs[1] = hregMIPS_GPR5(mode64);
      argregs[2] = hregMIPS_GPR6(mode64);
      argregs[3] = hregMIPS_GPR7(mode64);
      argiregs = 0;
      tmpregs[0] = tmpregs[1] = tmpregs[2] = tmpregs[3] = INVALID_HREG;
   }

   /* First decide which scheme (slow or fast) is to be used. First assume the
      fast scheme, and select slow if any contraindications (wow) appear. */

   go_fast = True;

   /* We'll need space on the stack for the return value.  Avoid
      possible complications with nested calls by using the slow
      scheme. */
   if (retTy == Ity_V128 || retTy == Ity_V256)
      go_fast = False;

   if (go_fast && guard) {
      if (guard->tag == Iex_Const && guard->Iex.Const.con->tag == Ico_U1
          && guard->Iex.Const.con->Ico.U1 == True) {
         /* unconditional */
      } else {
         /* Not manifestly unconditional -- be conservative. */
         go_fast = False;
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
      /* FAST SCHEME */
      argreg = 0;

      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];
         vassert(argreg < MIPS_N_REGPARMS);

         IRType  aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, arg);

         if (aTy == Ity_I32 || (mode64 && aTy != Ity_INVALID)) {
            argiregs |= (1 << (argreg + 4));
            addInstr(env, mk_iMOVds_RR(argregs[argreg],
                                       iselWordExpr_R(env, arg)));
            argreg++;
         } else if (aTy == Ity_I64) {  /* Ity_I64 */
            if (argreg & 1) {
               argreg++;
               argiregs |= (1 << (argreg + 4));
            }
            HReg rHi, rLo;
            iselInt64Expr(&rHi, &rLo, env, arg);
            argiregs |= (1 << (argreg + 4));
            addInstr(env, mk_iMOVds_RR( argregs[argreg++], rLo ));
            argiregs |= (1 << (argreg + 4));
            addInstr(env, mk_iMOVds_RR( argregs[argreg], rHi));
            argreg++;
         } else if (arg->tag == Iex_GSPTR) {
            vassert(0);  // ATC
            addInstr(env, mk_iMOVds_RR(argregs[argreg],
                                       GuestStatePointer(mode64)));
            argreg++;
         } else if (arg->tag == Iex_VECRET) {
            // If this happens, it denotes ill-formed IR.
            vassert(0);
         }
      }
      /* Fast scheme only applies for unconditional calls.  Hence: */
      cc = MIPScc_AL;
   } else {
      /* SLOW SCHEME; move via temporaries */
      argreg = 0;

      for (i = 0; i < n_args; i++) {
         vassert(argreg < MIPS_N_REGPARMS);
         IRExpr* arg = args[i];

         IRType  aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy  = typeOfIRExpr(env->type_env, arg);

         if (aTy == Ity_I32 || (mode64 && aTy != Ity_INVALID)) {
            tmpregs[argreg] = iselWordExpr_R(env, arg);
            argreg++;
         } else if (aTy == Ity_I64) {  /* Ity_I64 */
            if (argreg & 1)
               argreg++;
            if (argreg + 1 >= MIPS_N_REGPARMS)
               vassert(0);  /* out of argregs */
            HReg raHi, raLo;
            iselInt64Expr(&raHi, &raLo, env, arg);
            tmpregs[argreg] = raLo;
            argreg++;
            tmpregs[argreg] = raHi;
            argreg++;
         } else if (arg->tag == Iex_GSPTR) {
            tmpregs[argreg] = GuestStatePointer(mode64);
            argreg++;
         }
         else if (arg->tag == Iex_VECRET) {
            tmpregs[argreg++] = StackPointer(mode64);
            sub_from_sp(env, 16); /*  Move SP down 16 bytes */
         }
      }

      /* Now we can compute the condition.  We can't do it earlier
         because the argument computations could trash the condition
         codes.  Be a bit clever to handle the common case where the
         guard is 1:Bit. */
      cc = MIPScc_AL;
      if (guard) {
         if (guard->tag == Iex_Const && guard->Iex.Const.con->tag == Ico_U1
             && guard->Iex.Const.con->Ico.U1 == True) {
            /* unconditional -- do nothing */
         } else {
            cc = iselCondCode(env, guard);
            src = iselWordExpr_R(env, guard);
         }
      }
      /* Move the args to their final destinations. */
      for (i = 0; i < argreg; i++) {
         if (hregIsInvalid(tmpregs[i]))  /* Skip invalid regs */
            continue;
         /* None of these insns, including any spill code that might
            be generated, may alter the condition codes. */
         argiregs |= (1 << (i + 4));
         addInstr(env, mk_iMOVds_RR(argregs[i], tmpregs[i]));
      }
   }

   /* Do final checks, set the return values, and generate the call
      instruction proper. */
   vassert(nGSPTRs == 0 || nGSPTRs == 1);
   vassert(nVECRETs == ((retTy == Ity_V128 || retTy == Ity_V256) ? 1 : 0));
   vassert(*stackAdjustAfterCall == 0);
   vassert(is_RetLoc_INVALID(*retloc));
   switch (retTy) {
      case Ity_INVALID:
         /* Function doesn't return a value. */
         *retloc = mk_RetLoc_simple(RLPri_None);
         break;
      case Ity_I64:
         *retloc = mk_RetLoc_simple(mode64 ? RLPri_Int : RLPri_2Int);
         break;
      case Ity_I32: case Ity_I16: case Ity_I8:
         *retloc = mk_RetLoc_simple(RLPri_Int);
         break;
      case Ity_V128:
         *retloc = mk_RetLoc_spRel(RLPri_V128SpRel, 0);
         *stackAdjustAfterCall = 16;
         break;
      case Ity_V256:
         vassert(0); // ATC
         *retloc = mk_RetLoc_spRel(RLPri_V256SpRel, 0);
         *stackAdjustAfterCall = 32;
         break;
      default:
         /* IR can denote other possible return types, but we don't
            handle those here. */
        vassert(0);
   }

   Addr64 target = mode64 ? (Addr)cee->addr :
                            toUInt((Addr)cee->addr);

   /* Finally, generate the call itself.  This needs the *retloc value
      set in the switch above, which is why it's at the end. */
   if (cc == MIPScc_AL)
      addInstr(env, MIPSInstr_CallAlways(cc, target, argiregs,
                                         *retloc));
   else
      addInstr(env, MIPSInstr_Call(cc, target, argiregs, src, *retloc));
}

/*---------------------------------------------------------*/
/*--- ISEL: Integer expression auxiliaries              ---*/
/*---------------------------------------------------------*/

/* --------------------- AMODEs --------------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a word-size one.
*/

static Bool uInt_fits_in_16_bits(UInt u)
{
   Int i = u & 0xFFFF;
   i <<= 16;
   i >>= 16;
   return toBool(u == (UInt) i);
}

static Bool uLong_fits_in_16_bits ( ULong u )
{
   Long i = u & 0xFFFFULL;
   i <<= 48;
   i >>= 48;
   return toBool(u == (ULong) i);
}

static Bool uLong_is_4_aligned ( ULong u )
{
   return toBool((u & 3ULL) == 0);
}

static Bool sane_AMode(ISelEnv * env, MIPSAMode * am)
{
   switch (am->tag) {
      case Mam_IR:
         return toBool(hregClass(am->Mam.IR.base) == HRcGPR(mode64) &&
                  hregIsVirtual(am->Mam.IR.base) &&
                  uInt_fits_in_16_bits(am->Mam.IR.index));
      case Mam_RR:
         return toBool(hregClass(am->Mam.RR.base) == HRcGPR(mode64) &&
                  hregIsVirtual(am->Mam.RR.base) &&
                  hregClass(am->Mam.RR.index) == HRcGPR(mode64) &&
                  hregIsVirtual(am->Mam.RR.index));
      default:
         vpanic("sane_AMode: unknown mips amode tag");
   }
}

static MIPSAMode *iselWordExpr_AMode(ISelEnv * env, IRExpr * e, IRType xferTy)
{
   MIPSAMode *am = iselWordExpr_AMode_wrk(env, e, xferTy);
   vassert(sane_AMode(env, am));
   return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSAMode *iselWordExpr_AMode_wrk(ISelEnv * env, IRExpr * e,
                                         IRType xferTy)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   if (env->mode64) {
      Bool aligned4imm = toBool(xferTy == Ity_I32 || xferTy == Ity_I64);
      vassert(ty == Ity_I64);

      /* Add64(expr,i), where i == sign-extend of (i & 0xFFFF) */
      if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_Add64
          && e->Iex.Binop.arg2->tag == Iex_Const
          && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U64
          && (aligned4imm ?
          uLong_is_4_aligned(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64) : True)
          && uLong_fits_in_16_bits(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)) {
         return MIPSAMode_IR((Int) e->Iex.Binop.arg2->Iex.Const.con->Ico.U64,
                                   iselWordExpr_R(env, e->Iex.Binop.arg1));
      }

      /* Add64(expr,expr) */
      if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_Add64) {
         HReg r_base = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_idx = iselWordExpr_R(env, e->Iex.Binop.arg2);
         return MIPSAMode_RR(r_idx, r_base);
      }
   } else {
      vassert(ty == Ity_I32);

      /* Add32(expr,i), where i == sign-extend of (i & 0xFFFF) */
      if (e->tag == Iex_Binop
          && e->Iex.Binop.op == Iop_Add32
          && e->Iex.Binop.arg2->tag == Iex_Const
          && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32
          && uInt_fits_in_16_bits(e->Iex.Binop.arg2->Iex.Const.con-> Ico.U32)) {
         return MIPSAMode_IR((Int) e->Iex.Binop.arg2->Iex.Const.con->Ico.U32,
                              iselWordExpr_R(env, e->Iex.Binop.arg1));
      }

      /* Add32(expr,expr) */
      if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_Add32) {
         HReg r_base = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_idx = iselWordExpr_R(env, e->Iex.Binop.arg2);

         return MIPSAMode_RR(r_idx, r_base);
      }
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   return MIPSAMode_IR(0, iselWordExpr_R(env, e));
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

   vassert(hregClass(r) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselWordExpr_R_wrk(ISelEnv * env, IRExpr * e)
{
   UInt argiregs = 0;
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I1
           || ty == Ity_F32 || (ty == Ity_I64 && mode64)
           || (ty == Ity_I128 && mode64));

   switch (e->tag) {
      /* --------- TEMP --------- */
      case Iex_RdTmp:
         return lookupIRTemp(env, e->Iex.RdTmp.tmp);

      /* --------- LOAD --------- */
      case Iex_Load: {
         HReg r_dst = newVRegI(env);
         MIPSAMode *am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, ty);

         if (e->Iex.Load.end != Iend_LE
             && e->Iex.Load.end != Iend_BE)
            goto irreducible;

         addInstr(env, MIPSInstr_Load(toUChar(sizeofIRType(ty)),
                                      r_dst, am_addr, mode64));
         return r_dst;
      }

      /* --------- BINARY OP --------- */
      case Iex_Binop: {
         MIPSAluOp aluOp;
         MIPSShftOp shftOp;

         /* Is it an addition or logical style op? */
         switch (e->Iex.Binop.op) {
            case Iop_Add8:
            case Iop_Add16:
            case Iop_Add32:
               aluOp = Malu_ADD;
               break;

            case Iop_Sub8:
            case Iop_Sub16:
            case Iop_Sub32:
               aluOp = Malu_SUB;
               break;

            case Iop_Sub64:
               aluOp = Malu_DSUB;
               break;

            case Iop_And8:
            case Iop_And16:
            case Iop_And32:
            case Iop_And64:
               aluOp = Malu_AND;
               break;

            case Iop_Or8:
            case Iop_Or16:
            case Iop_Or32:
            case Iop_Or64:
               aluOp = Malu_OR;
               break;

            case Iop_Xor8:
            case Iop_Xor16:
            case Iop_Xor32:
            case Iop_Xor64:
               aluOp = Malu_XOR;
               break;

            case Iop_Add64:
               aluOp = Malu_DADD;
               break;

            default:
               aluOp = Malu_INVALID;
               break;
         }

         /* For commutative ops we assume any literal
            values are on the second operand. */
         if (aluOp != Malu_INVALID) {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            MIPSRH *ri_srcR = NULL;
            /* get right arg into an RH, in the appropriate way */
            switch (aluOp) {
               case Malu_ADD:
               case Malu_SUB:
               case Malu_DADD:
               case Malu_DSUB:
                  ri_srcR = iselWordExpr_RH(env, True /*signed */ ,
                                            e->Iex.Binop.arg2);
                  break;
               case Malu_AND:
               case Malu_OR:
               case Malu_XOR:
                  ri_srcR = iselWordExpr_RH(env, False /*unsigned */,
                                            e->Iex.Binop.arg2);
                  break;
               default:
                  vpanic("iselWordExpr_R_wrk-aluOp-arg2");
            }
            addInstr(env, MIPSInstr_Alu(aluOp, r_dst, r_srcL, ri_srcR));
            return r_dst;
         }

         /* a shift? */
         switch (e->Iex.Binop.op) {
            case Iop_Shl32:
            case Iop_Shl64:
               shftOp = Mshft_SLL;
               break;
            case Iop_Shr16:
            case Iop_Shr32:
            case Iop_Shr64:
               shftOp = Mshft_SRL;
               break;
            case Iop_Sar16:
            case Iop_Sar32:
            case Iop_Sar64:
               shftOp = Mshft_SRA;
               break;
            default:
               shftOp = Mshft_INVALID;
               break;
         }

         /* we assume any literal values are on the second operand. */
         if (shftOp != Mshft_INVALID) {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            MIPSRH *ri_srcR;
            if (mode64)
               ri_srcR = iselWordExpr_RH6u(env, e->Iex.Binop.arg2);
            else
               ri_srcR = iselWordExpr_RH5u(env, e->Iex.Binop.arg2);

            if (ty == Ity_I8) {
               vassert(0);
            } else if (ty == Ity_I16) {
               if (shftOp == Mshft_SRA) {
                  HReg tmp = newVRegI(env);
                  HReg r_srcL_se = newVRegI(env);
                  addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp,
                                               r_srcL, MIPSRH_Imm(False, 16)));
                  addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, r_srcL_se,
                                               tmp, MIPSRH_Imm(False, 16)));
                  addInstr(env, MIPSInstr_Shft(shftOp, True,
                                               r_dst, r_srcL_se, ri_srcR));
               } else if (shftOp == Mshft_SRL) {
                  HReg r_srcL_se = newVRegI(env);
                  addInstr(env, MIPSInstr_Alu(Malu_AND, r_srcL_se, r_srcL,
                                              MIPSRH_Imm(False, 0xFFFF)));
                  addInstr(env, MIPSInstr_Shft(shftOp, True,
                                               r_dst, r_srcL_se, ri_srcR));
               } else {
                  vassert(0);
               }
            } else if (ty == Ity_I32) {
               if (mode64 && (shftOp == Mshft_SRA || shftOp == Mshft_SRL)) {
                  HReg tmp = newVRegI(env);
                  HReg r_srcL_se = newVRegI(env);
                  /* SRA, SRAV, SRL, SRLV: On 64-bit processors, if GPR rt does
                     not contain a sign-extended 32-bit value (bits 63..31
                     equal), then the result of the operation is UNPREDICTABLE.
                     So we need to sign-extend r_srcL:
                     DSLLV tmp, r_srcL, 32
                     DSRAV r_srcL_se, tmp, 32
                  */
                  addInstr(env, MIPSInstr_Shft(Mshft_SLL, False, tmp,
                                               r_srcL, MIPSRH_Imm(False, 32)));
                  addInstr(env, MIPSInstr_Shft(Mshft_SRA, False, r_srcL_se,
                                               tmp, MIPSRH_Imm(False, 32)));
                  /* And finally do the shift. */
                  addInstr(env, MIPSInstr_Shft(shftOp, True /*32bit shift */,
                                               r_dst, r_srcL_se, ri_srcR));
               } else
                  addInstr(env, MIPSInstr_Shft(shftOp, True /*32bit shift */,
                                               r_dst, r_srcL, ri_srcR));
            } else if (ty == Ity_I64) {
               vassert(mode64);
               addInstr(env, MIPSInstr_Shft(shftOp, False/*64bit shift */,
                                            r_dst, r_srcL, ri_srcR));
            } else
               goto irreducible;
            return r_dst;
         }

          if (!mode64 && (e->Iex.Binop.op == Iop_CasCmpEQ64
              || e->Iex.Binop.op == Iop_CmpEQ64)) {
             HReg tmp1, tmp2, tmp3, tmp4;
             HReg dst1 = newVRegI(env);
             HReg dst2 = newVRegI(env);
             iselInt64Expr(&tmp1, &tmp2, env, e->Iex.Binop.arg1);
             iselInt64Expr(&tmp3, &tmp4, env, e->Iex.Binop.arg2);
             addInstr(env, MIPSInstr_Cmp(False, True, dst1, tmp1, tmp3, MIPScc_EQ));
             addInstr(env, MIPSInstr_Cmp(False, True, dst2, tmp2, tmp4, MIPScc_EQ));
             addInstr(env, MIPSInstr_Alu(Malu_AND, dst1, dst1, MIPSRH_Reg(dst2)));
             return dst1;
          }

         /* Cmp*32*(x,y) ? */
         if (e->Iex.Binop.op == Iop_CmpEQ32
             || e->Iex.Binop.op == Iop_CmpEQ8
             || e->Iex.Binop.op == Iop_CmpEQ16
             || e->Iex.Binop.op == Iop_CmpNE32
             || e->Iex.Binop.op == Iop_CmpNE64
             || e->Iex.Binop.op == Iop_CmpLT32S
             || e->Iex.Binop.op == Iop_CmpLT32U
             || e->Iex.Binop.op == Iop_CmpLT64U
             || e->Iex.Binop.op == Iop_CmpLE32U
             || e->Iex.Binop.op == Iop_CmpLE32S
             || e->Iex.Binop.op == Iop_CmpLE64S
             || e->Iex.Binop.op == Iop_CmpLT64S
             || e->Iex.Binop.op == Iop_CmpEQ64
             || e->Iex.Binop.op == Iop_CasCmpEQ32
             || e->Iex.Binop.op == Iop_CasCmpEQ64) {

            Bool syned = (e->Iex.Binop.op == Iop_CmpLT32S
                         || e->Iex.Binop.op == Iop_CmpLE32S
                         || e->Iex.Binop.op == Iop_CmpLT64S
                         || e->Iex.Binop.op == Iop_CmpLE64S);
            Bool size32;
            HReg dst = newVRegI(env);
            HReg r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r2 = iselWordExpr_R(env, e->Iex.Binop.arg2);

            MIPSCondCode cc;

            switch (e->Iex.Binop.op) {
               case Iop_CmpEQ32:
               case Iop_CasCmpEQ32:
                  cc = MIPScc_EQ;
                  size32 = True;
                  break;
               case Iop_CmpEQ8:
               case Iop_CmpEQ16:
                  cc = MIPScc_EQ;
                  size32 = True;
                  break;
               case Iop_CmpNE32:
                  cc = MIPScc_NE;
                  size32 = True;
                  break;
               case Iop_CmpNE64:
                  cc = MIPScc_NE;
                  size32 = False;
                  break;
               case Iop_CmpLT32S:
                  cc = MIPScc_LT;
                  size32 = True;
                  break;
               case Iop_CmpLT32U:
                  cc = MIPScc_LO;
                  size32 = True;
                  break;
               case Iop_CmpLT64U:
                  cc = MIPScc_LO;
                  size32 = False;
                  break;
               case Iop_CmpLE32U:
                  cc = MIPScc_LE;
                  size32 = True;
                  break;
               case Iop_CmpLE32S:
                  cc = MIPScc_LE;
                  size32 = True;
                  break;
               case Iop_CmpLE64S:
                  cc = MIPScc_LE;
                  size32 = False;
                  break;
               case Iop_CmpLT64S:
                  cc = MIPScc_LT;
                  size32 = False;
                  break;
               case Iop_CmpEQ64:
               case Iop_CasCmpEQ64:
                  cc = MIPScc_EQ;
                  size32 = False;
                  break;
               default:
                  vpanic("iselCondCode(mips): CmpXX32 or CmpXX64");
            }

            addInstr(env, MIPSInstr_Cmp(syned, size32, dst, r1, r2, cc));
            return dst;
         }

         if (e->Iex.Binop.op == Iop_Max32U) {
            HReg tmp = newVRegI(env);
            HReg r_dst = newVRegI(env);
            HReg argL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg argR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            MIPSRH *argRH = MIPSRH_Reg(argR);
            /* max (v0, s0)
               ------------
               slt v1, v0, s0
               movn v0, s0, v1 */

            addInstr(env, MIPSInstr_Alu(Malu_SLT, tmp, argL, argRH));
#if (__mips_isa_rev >= 6)
            {
              HReg r_temp  = newVRegI(env);
              addInstr(env, MIPSInstr_MoveCond(MSeleqz, r_dst, argL, tmp));
              addInstr(env, MIPSInstr_MoveCond(MSelnez, r_temp, argR, tmp));
              addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, r_dst,
                                MIPSRH_Reg(r_temp)));
            }

#else
            addInstr(env, mk_iMOVds_RR(r_dst, argL));
            addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, r_dst, argR, tmp));
#endif
            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_Mul32) {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Mulr6(False, True, True,
                                          r_dst, r_srcL, r_srcR));
#else
            addInstr(env, MIPSInstr_Mul(r_dst, r_srcL, r_srcR));
#endif
            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_Mul64 ||
             e->Iex.Binop.op == Iop_MullS32) {
            vassert(mode64);
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Mulr6(False, False, True,
                                          r_dst, r_srcL, r_srcR));
#else
            addInstr(env, MIPSInstr_Mult(True, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mflo(r_dst));
#endif
            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_MullU32) {
            vassert(mode64);
            HReg r_tmpL = newVRegI(env);
            HReg r_tmpR = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Ext(r_tmpL, r_srcL, 0, 32));
            addInstr(env, MIPSInstr_Ext(r_tmpR, r_srcR, 0, 32));
            addInstr(env, MIPSInstr_Mulr6(True, False, True,
                                          r_tmpR, r_tmpL, r_tmpR));
#else
            if (VEX_MIPS_CPU_HAS_MIPS64R2(hwcaps_host)) {
               addInstr(env, MIPSInstr_Ext(r_tmpL, r_srcL, 0, 32));
               addInstr(env, MIPSInstr_Ext(r_tmpR, r_srcR, 0, 32));
            } else {
               addInstr(env, MIPSInstr_LI(r_tmpL, 0xFFFFFFFF));
               addInstr(env, MIPSInstr_Alu(Malu_AND, r_tmpR, r_srcR,
                                           MIPSRH_Reg(r_tmpL)));
               addInstr(env, MIPSInstr_Alu(Malu_AND, r_tmpL, r_srcL,
                                           MIPSRH_Reg(r_tmpL)));
            }
            addInstr(env, MIPSInstr_Mult(False, r_tmpL, r_tmpR));
            addInstr(env, MIPSInstr_Mflo(r_tmpR));
#endif
            return r_tmpR;
         }

         if (e->Iex.Binop.op == Iop_MullU8 ||
             e->Iex.Binop.op == Iop_MullS8 ||
             e->Iex.Binop.op == Iop_MullU16 ||
             e->Iex.Binop.op == Iop_MullS16) {
            Bool syned = toBool((e->Iex.Binop.op == Iop_MullS8) ||
                                (e->Iex.Binop.op == Iop_MullS16));
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
#if (__mips_isa_rev >= 6)
             if (syned) {
               Int no_bits = (e->Iex.Binop.op == Iop_MullS16) ? 16 : 24;
               addInstr(env, MIPSInstr_Shft(Mshft_SLL, True,
                                            r_srcL, r_srcL,
                                            MIPSRH_Imm(False, no_bits)));
               addInstr(env, MIPSInstr_Shft(Mshft_SRA, True,
                                            r_srcL, r_srcL,
                                            MIPSRH_Imm(False, no_bits)));
               addInstr(env, MIPSInstr_Shft(Mshft_SLL, True,
                                            r_srcR, r_srcR,
                                            MIPSRH_Imm(False, no_bits)));
               addInstr(env, MIPSInstr_Shft(Mshft_SRA, True,
                                            r_srcR, r_srcR,
                                            MIPSRH_Imm(False, no_bits)));
            }
            addInstr(env, MIPSInstr_Mulr6(syned, True, True,
                                          r_dst, r_srcL, r_srcR));
#else
            if (syned) {
               Int no_bits = (e->Iex.Binop.op == Iop_MullS16) ? 16 : 24;
               addInstr(env, MIPSInstr_Shft(Mshft_SLL, True,
                                            r_srcL, r_srcL,
                                            MIPSRH_Imm(False, no_bits)));
               addInstr(env, MIPSInstr_Shft(Mshft_SRA, True,
                                            r_srcL, r_srcL,
                                            MIPSRH_Imm(False, no_bits)));
               addInstr(env, MIPSInstr_Shft(Mshft_SLL, True,
                                            r_srcR, r_srcR,
                                            MIPSRH_Imm(False, no_bits)));
               addInstr(env, MIPSInstr_Shft(Mshft_SRA, True,
                                            r_srcR, r_srcR,
                                            MIPSRH_Imm(False, no_bits)));
               addInstr(env, MIPSInstr_Mul(r_dst, r_srcL, r_srcR));

            } else {
               addInstr(env, MIPSInstr_Mult(syned, r_srcL, r_srcR));
               addInstr(env, MIPSInstr_Mflo(r_dst));
            }
#endif
            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_CmpF64) {
            HReg r_srcL, r_srcR;
            if (mode64) {
               r_srcL = iselFltExpr(env, e->Iex.Binop.arg1);
               r_srcR = iselFltExpr(env, e->Iex.Binop.arg2);
            } else {
               r_srcL = iselDblExpr(env, e->Iex.Binop.arg1);
               r_srcR = iselDblExpr(env, e->Iex.Binop.arg2);
            }
#if (__mips_isa_rev >= 6)
            HReg tmp = newVRegI(env);
            HReg tmpf;
            HReg result = newVRegI(env);
            if (mode64) tmpf = newVRegF(env);
            else tmpf = newVRegD(env);
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_UN, tmpf, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, tmp, tmpf));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp,
                                        MIPSRH_Imm(False, 0x45)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, result,
                                        hregMIPS_GPR0(env->mode64),
                                        MIPSRH_Reg(tmp)));
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_LT, tmpf, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, tmp, tmpf));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp,
                                        MIPSRH_Imm(False, 0x1)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, result, result,
                                        MIPSRH_Reg(tmp)));
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_EQ, tmpf, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, tmp, tmpf));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp,
                                        MIPSRH_Imm(False, 0x40)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, result, result,
                                        MIPSRH_Reg(tmp)));
            return result;
#else
            HReg tmp = newVRegI(env);
            HReg r_ccMIPS = newVRegI(env);
            HReg r_ccIR = newVRegI(env);
            HReg r_ccIR_b0 = newVRegI(env);
            HReg r_ccIR_b2 = newVRegI(env);
            HReg r_ccIR_b6 = newVRegI(env);

            /* Create in dst, the IRCmpF64Result encoded result. */
            /* chech for EQ */
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_EQ, tmp, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, r_ccMIPS, tmp,
                                         MIPSRH_Imm(False, 1)));
            /* chech for UN */
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_UN, tmp, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccMIPS, r_ccMIPS,
                                        MIPSRH_Reg(tmp)));
            /* chech for LT */
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_LT, tmp, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp,
                                         tmp, MIPSRH_Imm(False, 2)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccMIPS, r_ccMIPS,
                                        MIPSRH_Reg(tmp)));
            /* chech for GT */
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_NGT,
                                              tmp, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp, tmp,
                                         MIPSRH_Imm(False, 3)));

            addInstr(env, MIPSInstr_Alu(Malu_NOR, tmp, tmp, MIPSRH_Reg(tmp)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp,
                                        MIPSRH_Imm(False, 8)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccMIPS, r_ccMIPS,
                                        MIPSRH_Reg(tmp)));
            /* Map compare result from MIPS to IR,
               conforming to CmpF64 definition.
               FP cmp result | MIPS | IR
               --------------------------
               UN            | 0x1 | 0x45
               EQ            | 0x2 | 0x40
               GT            | 0x4 | 0x00
               LT            | 0x8 | 0x01
             */

            /* r_ccIR_b0 = r_ccMIPS[0] | r_ccMIPS[3] */
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True, r_ccIR_b0, r_ccMIPS,
                          MIPSRH_Imm(False, 0x3)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccIR_b0, r_ccMIPS,
                          MIPSRH_Reg(r_ccIR_b0)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_ccIR_b0, r_ccIR_b0,
                          MIPSRH_Imm(False, 0x1)));

            /* r_ccIR_b2 = r_ccMIPS[0] */
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, r_ccIR_b2, r_ccMIPS,
                          MIPSRH_Imm(False, 0x2)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_ccIR_b2, r_ccIR_b2,
                          MIPSRH_Imm(False, 0x4)));

            /* r_ccIR_b6 = r_ccMIPS[0] | r_ccMIPS[1] */
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True, r_ccIR_b6,
                          r_ccMIPS, MIPSRH_Imm(False, 0x1)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccIR_b6, r_ccMIPS,
                          MIPSRH_Reg(r_ccIR_b6)));
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, r_ccIR_b6, r_ccIR_b6,
                          MIPSRH_Imm(False, 0x6)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_ccIR_b6, r_ccIR_b6,
                          MIPSRH_Imm(False, 0x40)));

            /* r_ccIR = r_ccIR_b0 | r_ccIR_b2 | r_ccIR_b6 */
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccIR, r_ccIR_b0,
                          MIPSRH_Reg(r_ccIR_b2)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccIR, r_ccIR,
                          MIPSRH_Reg(r_ccIR_b6)));
            return r_ccIR;
#endif
         }

         if (e->Iex.Binop.op == Iop_CmpF32) {
#if (__mips_isa_rev >= 6)
            HReg r_srcL = iselFltExpr(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg tmp = newVRegI(env);
            HReg tmpf;
            HReg result = newVRegI(env);
            if (mode64) tmpf = newVRegF(env);
            else tmpf = newVRegD(env);
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_UN_S, tmpf, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, tmp, tmpf));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp,
                                        MIPSRH_Imm(False, 0x45)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, result,
                                        hregMIPS_GPR0(env->mode64),
                                        MIPSRH_Reg(tmp)));
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_LT_S, tmpf, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, tmp, tmpf));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp,
                                        MIPSRH_Imm(False, 0x1)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, result, result,
                                        MIPSRH_Reg(tmp)));
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP_EQ_S, tmpf, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, tmp, tmpf));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp,
                                        MIPSRH_Imm(False, 0x40)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, result, result,
                                        MIPSRH_Reg(tmp)));
            return result;
#endif
         }

         if (e->Iex.Binop.op == Iop_DivModU32to32 ||
             e->Iex.Binop.op == Iop_DivModS32to32) {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg mask = newVRegI(env);
            HReg tLo_1 = newVRegI(env);
            HReg tHi_1 = newVRegI(env);
            HReg r_dst = newVRegI(env);
            Bool syned = toBool(e->Iex.Binop.op == Iop_DivModS32to32);

            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Divr6(syned /* Unsigned or Signed */ ,
                                          True  /* 32bit or 64bit div */ ,
                                          False /* mod */,
                                          tLo, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Divr6(syned /* Unsigned or Signed */ ,
                                          True /*3 2bit or 64bit div */ ,
                                          True /* mod */,
                                          tHi, r_srcL, r_srcR));
#else
            addInstr(env, MIPSInstr_Div(syned, True, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
#endif
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, False, tHi_1, tHi,
                                         MIPSRH_Imm(False, 32)));

            addInstr(env, MIPSInstr_LI(mask, 0xffffffff));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tLo_1, tLo,
                          MIPSRH_Reg(mask)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, tHi_1,
                          MIPSRH_Reg(tLo_1)));

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_DivS32 ||
             e->Iex.Binop.op == Iop_DivU32 ||
             (e->Iex.Binop.op == Iop_DivS64 && mode64) ||
             (e->Iex.Binop.op == Iop_DivU64 && mode64)) {
            HReg r_dst = newVRegI(env);
            Bool syned = toBool(e->Iex.Binop.op == Iop_DivS32 ||
                                e->Iex.Binop.op == Iop_DivS64);
            Bool div32 = toBool(e->Iex.Binop.op == Iop_DivS32 ||
                                e->Iex.Binop.op == Iop_DivU32);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Divr6(syned, div32, False,
                                          r_dst, r_srcL, r_srcR));
#else
            addInstr(env, MIPSInstr_Div(syned, div32, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mflo(r_dst));
#endif
            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_8HLto16
             || e->Iex.Binop.op == Iop_16HLto32) {
            HReg tHi   = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg tLo   = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tLo_1 = newVRegI(env);
            HReg tHi_1 = newVRegI(env);
            HReg r_dst = newVRegI(env);
            UInt shift = 0;
            UInt mask  = 0;
            switch (e->Iex.Binop.op) {
               case Iop_8HLto16:
                  shift = 8;
                  mask  = 0xff;
                  break;
               case Iop_16HLto32:
                  shift = 16;
                  mask  = 0xffff;
                  break;
               default:
                  break;
            }

            /* sll tHi_1, tHi,   shift
               and tLo_1, tLo,   mask
               or  r_dst, tHi_1, tLo_1 */
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tHi_1, tHi,
                                         MIPSRH_Imm(False, shift)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tLo_1, tLo,
                          MIPSRH_Imm(False, mask)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, tHi_1,
                          MIPSRH_Reg(tLo_1)));
            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_32HLto64) {
            vassert(mode64);
            HReg tHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg tLo = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tLo_1 = newVRegI(env);
            HReg tHi_1 = newVRegI(env);
            HReg r_dst = newVRegI(env);
            HReg mask = newVRegI(env);

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, False, tHi_1, tHi,
                                         MIPSRH_Imm(False, 32)));

            addInstr(env, MIPSInstr_LI(mask, 0xffffffff));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tLo_1, tLo,
                          MIPSRH_Reg(mask)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, tHi_1,
                          MIPSRH_Reg(tLo_1)));

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_F32toI64S) {
            vassert(mode64);
            HReg valS = newVRegI(env);
            HReg tmpF = newVRegF(env);
            HReg valF = iselFltExpr(env, e->Iex.Binop.arg2);

            /* CVTLS tmpF, valF */
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTLS, tmpF, valF));
            set_MIPS_rounding_default(env);

            /* Doubleword Move from Floating Point
               dmfc1 valS, tmpF */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_dmfc1, valS, tmpF));

            return valS;
         }

         if (e->Iex.Binop.op == Iop_F64toI64S) {
            vassert(mode64);
            HReg valS = newVRegI(env);
            HReg tmpF = newVRegF(env);
            HReg valF = iselFltExpr(env, e->Iex.Binop.arg2);

            /* CVTLS tmpF, valF */
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTLD, tmpF, valF));
            set_MIPS_rounding_default(env);

            /* Doubleword Move from Floating Point
               dmfc1 valS, tmpF */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_dmfc1, valS, tmpF));

            return valS;
         }

         if (e->Iex.Binop.op == Iop_F64toI32S) {
            HReg valD;
            if (mode64)
               valD = iselFltExpr(env, e->Iex.Binop.arg2);
            else
               valD = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg valS = newVRegF(env);
            HReg r_dst = newVRegI(env);

            /* CVTWD valS, valD */
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTWD, valS, valD));
            set_MIPS_rounding_default(env);

            /* Move Word From Floating Point
               mfc1 r_dst, valS */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, r_dst, valS));

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_F32toI32U) {
            HReg valF = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg tmpD = newVRegD(env);
            HReg r_dst  = newVRegI(env);
            MIPSAMode *am_addr;

            /* CVTLS tmpD, valF */
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTLS, tmpD, valF));
            set_MIPS_rounding_default(env);

            sub_from_sp(env, 16);  /* Move SP down 16 bytes */
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            /* store as F64 */
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, tmpD,
                                           am_addr));
            /* load as 2xI32 */
#if defined (_MIPSEL)
            addInstr(env, MIPSInstr_Load(4, r_dst, am_addr, mode64));
#elif defined (_MIPSEB)
            addInstr(env, MIPSInstr_Load(4, r_dst, nextMIPSAModeFloat(am_addr),
                                         mode64));
#endif

            /* Reset SP */
            add_to_sp(env, 16);

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_F64toI64U) {
            HReg r_src;
            HReg tmp = newVRegV(env);
            vassert(has_msa);
            r_src = iselFltExpr( env, e->Iex.Binop.arg2);
            set_MIPS_rounding_mode_MSA(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_Msa2RF(MSA_FTINT_U, MSA_F_DW, tmp, r_src));
            HReg r_dst = newVRegI(env);
            addInstr(env,
                     MIPSInstr_MsaElm(MSA_COPY_S, tmp, r_dst, MSA_DFN_D | 0));
            set_MIPS_rounding_default_MSA(env);
            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_GetElem8x16) {
            HReg v_src = iselV128Expr(env, e->Iex.Binop.arg1);
            HReg r_dst = newVRegI(env);
            MIPSRH *tmp = iselWordExpr_RH(env, False, e->Iex.Binop.arg2);
            vassert(has_msa);
            switch (tmp->tag) {
               case Mrh_Imm:
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_COPY_U, v_src, r_dst,
                                            MSA_DFN_B |
                                            (tmp->Mrh.Imm.imm16 & 0x0f)));
                  break;

               case Mrh_Reg: {
                     HReg v_tmp = newVRegV(env);
                     addInstr(env,
                              MIPSInstr_Msa3R(MSA_SPLAT, MSA_B, v_tmp, v_src,
                                              tmp->Mrh.Reg.reg));
                     addInstr(env,
                              MIPSInstr_MsaElm(MSA_COPY_U, v_tmp, r_dst,
                                               MSA_DFN_B));
                     break;
                  }
            }

            return r_dst;
         }


         if (e->Iex.Binop.op == Iop_GetElem16x8) {
            HReg v_src = iselV128Expr(env, e->Iex.Binop.arg1);
            HReg r_dst = newVRegI(env);
            MIPSRH *tmp = iselWordExpr_RH(env, False, e->Iex.Binop.arg2);
            vassert(has_msa);
            switch (tmp->tag) {
               case Mrh_Imm:
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_COPY_U, v_src, r_dst,
                                            MSA_DFN_H |
                                            (tmp->Mrh.Imm.imm16 & 0x07)));
                  break;

               case Mrh_Reg: {
                     HReg v_tmp = newVRegV(env);
                     addInstr(env,
                              MIPSInstr_Msa3R(MSA_SPLAT, MSA_H, v_tmp, v_src,
                                              tmp->Mrh.Reg.reg));
                     addInstr(env,
                              MIPSInstr_MsaElm(MSA_COPY_U, v_tmp, r_dst,
                                               MSA_DFN_H));
                     break;
                  }
            }

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_GetElem32x4) {
            HReg v_src = iselV128Expr(env, e->Iex.Binop.arg1);
            HReg r_dst = newVRegI(env);
            MIPSRH *tmp = iselWordExpr_RH(env, False, e->Iex.Binop.arg2);
            vassert(has_msa);
            switch (tmp->tag) {
               case Mrh_Imm:
                  addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, v_src, r_dst,
                                                 MSA_DFN_W |
                                                 (tmp->Mrh.Imm.imm16 & 0x03)));
                  break;

               case Mrh_Reg: {
                     HReg v_tmp = newVRegV(env);
                     addInstr(env,
                              MIPSInstr_Msa3R(MSA_SPLAT, MSA_W, v_tmp, v_src,
                                              tmp->Mrh.Reg.reg));
                     addInstr(env,
                              MIPSInstr_MsaElm(MSA_COPY_S, v_tmp, r_dst,
                                               MSA_DFN_W));
                     break;
                  }
            }

            return r_dst;
         }
         if (e->Iex.Binop.op == Iop_GetElem64x2) {
            vassert(mode64);
            HReg v_src = iselV128Expr(env, e->Iex.Binop.arg1);
            HReg r_dst = newVRegI(env);
            MIPSRH *tmp = iselWordExpr_RH(env, False, e->Iex.Binop.arg2);
            vassert(has_msa);
            switch (tmp->tag) {
               case Mrh_Imm:
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_COPY_S, v_src, r_dst,
                                            MSA_DFN_D |
                                            (tmp->Mrh.Imm.imm16 & 0x01)));
                  break;

               case Mrh_Reg: {
                     HReg v_tmp = newVRegV(env);
                     addInstr(env,
                              MIPSInstr_Msa3R(MSA_SPLAT, MSA_D, v_tmp, v_src,
                                              tmp->Mrh.Reg.reg));
                     addInstr(env,
                              MIPSInstr_MsaElm(MSA_COPY_S, v_tmp, r_dst,
                                               MSA_DFN_D));
                     break;
                  }
            }

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_F32toI32S) {
            HReg valS = newVRegF(env);
            HReg valF = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg r_dst = newVRegI(env);

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTWS, valS, valF));
            set_MIPS_rounding_default(env);

            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, r_dst, valS));

            return r_dst;
         }

         /* -------- DSP ASE -------- */
         /* All used cases involving host-side helper calls. */
         void* fn = NULL;
         switch (e->Iex.Binop.op) {
            case Iop_HAdd8Ux4:
               fn = &h_generic_calc_HAdd8Ux4; break;
            case Iop_HSub8Ux4:
               fn = &h_generic_calc_HSub8Ux4; break;
            case Iop_HSub16Sx2:
               fn = &h_generic_calc_HSub16Sx2; break;
            case Iop_QSub8Ux4:
               fn = &h_generic_calc_QSub8Ux4; break;
            default:
                  break;
         }

         /* What's the retloc? */
         RetLoc rloc = mk_RetLoc_INVALID();
         if (ty == Ity_I32) {
            rloc = mk_RetLoc_simple(RLPri_Int);
         }
         else if (ty == Ity_I64) {
            rloc = mode64 ? mk_RetLoc_simple(RLPri_Int) :
                            mk_RetLoc_simple(RLPri_2Int);
         }
         else {
            goto irreducible;
         }

         if (fn) {
            HReg regL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg regR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg res  = newVRegI(env);
            addInstr(env, mk_iMOVds_RR(hregMIPS_GPR4(env->mode64), regL));
            addInstr(env, mk_iMOVds_RR(hregMIPS_GPR5(env->mode64), regR));
            argiregs |= (1 << 4);
            argiregs |= (1 << 5);
            addInstr(env, MIPSInstr_CallAlways( MIPScc_AL,
                                                (Addr)fn,
                                                argiregs, rloc));
            addInstr(env, mk_iMOVds_RR(res, hregMIPS_GPR2(env->mode64)));
            return res;
         }
      break;
   }

   /* --------- UNARY OP --------- */
   case Iex_Unop: {
      IROp op_unop = e->Iex.Unop.op;

      switch (op_unop) {
         case Iop_1Sto8:
         case Iop_1Sto16:
         case Iop_1Sto32:
         case Iop_8Sto16:
         case Iop_8Sto32:
         case Iop_16Sto32:
         case Iop_16Sto64:
         case Iop_8Sto64:
         case Iop_1Sto64: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            Bool sz32;
            UShort amt;
            switch (op_unop) {
               case Iop_1Sto8:
                  amt = 31;
                  sz32 = True;
                  break;
               case Iop_1Sto16:
                  amt = 31;
                  sz32 = True;
                  break;
               case Iop_1Sto32:
                  amt = 31;
                  sz32 = True;
                  break;
               case Iop_16Sto32:
                  amt = 16;
                  sz32 = True;
                  break;
               case Iop_16Sto64:
                  amt = 48;
                  sz32 = False;
                  break;
               case Iop_8Sto16:
                  amt = 24;
                  sz32 = True;
                  break;
               case Iop_8Sto32:
                  amt = 24;
                  sz32 = True;
                  break;
               case Iop_8Sto64:
                  amt = 56;
                  sz32 = False;
                  break;
               case Iop_1Sto64:
                  amt = 63;
                  sz32 = False;
                  break;
               default:
                  vassert(0);
            }

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, sz32, r_dst, r_src,
                                         MIPSRH_Imm(False, amt)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, sz32, r_dst, r_dst,
                                         MIPSRH_Imm(False, amt)));
            return r_dst;
         }

         /* not(x) = nor(x,x) */
         case Iop_Not1: {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
            MIPSRH *r_srcR = MIPSRH_Reg(r_srcL);

            addInstr(env, MIPSInstr_LI(r_dst, 0x1));
            addInstr(env, MIPSInstr_Alu(Malu_SUB, r_dst, r_dst, r_srcR));
            return r_dst;
         }

         case Iop_Not8:
         case Iop_Not16:
         case Iop_Not32:
         case Iop_Not64: {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
            MIPSRH *r_srcR = MIPSRH_Reg(r_srcL);

            addInstr(env, MIPSInstr_Alu(Malu_NOR, r_dst, r_srcL, r_srcR));
            return r_dst;
         }

         case Iop_ReinterpF32asI32: {
            HReg fr_src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg r_dst = newVRegI(env);

            /* Move Word From Floating Point
               mfc1 r_dst, fr_src */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, r_dst, fr_src));

            return r_dst;
         }

         case Iop_ReinterpF64asI64: {
            vassert(mode64);
            HReg fr_src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg r_dst = newVRegI(env);

            /* Doubleword Move from Floating Point
               mfc1 r_dst, fr_src */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_dmfc1, r_dst, fr_src));

            return r_dst;
         }

         case Iop_F64toI32S: {
            HReg valD;
            if (mode64)
               valD = iselFltExpr(env, e->Iex.Binop.arg2);
            else
               valD = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg valS = newVRegF(env);
            HReg r_dst = newVRegI(env);

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTWD, valS, valD));
            set_MIPS_rounding_default(env);

            /* Move Word From Floating Point
               mfc1 r_dst, valS */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mfc1, r_dst, valS));

            return r_dst;
         }

         case Iop_16to8:
         case Iop_32to1:
         case Iop_32to8:
         case Iop_32to16:
            return iselWordExpr_R(env, e->Iex.Unop.arg);

         case Iop_32HIto16: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /* 32bit shift */,
                                         r_dst, r_src, MIPSRH_Imm(False, 16)));
            return r_dst;
         }

         case Iop_64to1:
         case Iop_64to8: {
            HReg r_src, r_dst;
            UShort mask = (op_unop == Iop_64to1) ? 0x1 : 0xFF;
            r_dst = newVRegI(env);
            if (mode64)
               r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            else {
              HReg tmp;
              iselInt64Expr(&tmp, &r_src, env, e->Iex.Unop.arg);
            }
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_dst, r_src,
                          MIPSRH_Imm(False, mask)));
            return r_dst;
         }

         case Iop_16HIto8: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /* 32bit shift */,
                                         r_dst, r_src, MIPSRH_Imm(False, 8)));
            return r_dst;
         }

         case Iop_1Uto8:
         case Iop_1Uto32:
         case Iop_1Uto64:
         case Iop_8Uto16:
         case Iop_8Uto32:
         case Iop_8Uto64:
         case Iop_16Uto32:
         case Iop_16Uto64: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            UShort mask = 0;
            switch (op_unop) {
               case Iop_1Uto64:
                  vassert(mode64);
                  /* fallthrough */
               case Iop_1Uto8:
               case Iop_1Uto32:
                  mask = toUShort(0x1);
                  break;
               case Iop_8Uto64:
                  vassert(mode64);
                  /* fallthrough */
               case Iop_8Uto16:
               case Iop_8Uto32:
                  mask = toUShort(0xFF);
                  break;
               case Iop_16Uto64:
                  vassert(mode64);
                  /* fallthrough */
               case Iop_16Uto32:
                  mask = toUShort(0xFFFF);
                  break;
               default:
                  vassert(0);
                  break;
            }
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_dst, r_src,
                          MIPSRH_Imm(False, mask)));
            return r_dst;
         }

         case Iop_32Uto64: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            vassert(mode64);
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, False /*!32bit shift */,
                                         r_dst, r_src, MIPSRH_Imm(False, 32)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, False /*!32bit shift */,
                                         r_dst, r_dst, MIPSRH_Imm(False, 32)));
            return r_dst;
         }

         case Iop_64HIto32: {
            if (env->mode64) {
               HReg r_dst = newVRegI(env);
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, MIPSInstr_Shft(Mshft_SRA, False /*64bit shift */,
                       r_dst, r_src, MIPSRH_Imm(True, 32)));
               return r_dst;
            } else {
               HReg rHi, rLo;
               iselInt64Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
               return rHi;
            }
         }

         case Iop_64to32: {
            if (env->mode64) {
               HReg r_dst = newVRegI(env);
               r_dst = iselWordExpr_R(env, e->Iex.Unop.arg);
               return r_dst;
            } else {
               HReg rHi, rLo;
               iselInt64Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
               return rLo;
            }
         }

         case Iop_64to16: {
            vassert(env->mode64);
            HReg r_dst = newVRegI(env);
            r_dst = iselWordExpr_R(env, e->Iex.Unop.arg);
            return r_dst;
         }

         case Iop_32Sto64: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            vassert(mode64);
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True /*!32bit shift */,
                                         r_dst, r_src, MIPSRH_Imm(True, 0)));
            return r_dst;
         }

         case Iop_CmpNEZ8:
         case Iop_CmpNEZ16: {
            HReg r_dst = newVRegI(env);
            HReg tmp = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            UShort mask = (op_unop == Iop_CmpNEZ8) ? 0xFF : 0xFFFF;

            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, r_src,
                                        MIPSRH_Imm(False, mask)));
            addInstr(env, MIPSInstr_Cmp(False, True, r_dst, tmp,
                                        hregMIPS_GPR0(mode64), MIPScc_NE));
            return r_dst;
         }

         case Iop_CmpNEZ32: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

            addInstr(env, MIPSInstr_Cmp(False, True, r_dst, r_src,
                                        hregMIPS_GPR0(mode64), MIPScc_NE));
            return r_dst;
         }

         case Iop_CmpwNEZ32: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

            addInstr(env, MIPSInstr_Alu(Malu_SUB, r_dst, hregMIPS_GPR0(mode64),
                          MIPSRH_Reg(r_src)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, r_dst,
                                        MIPSRH_Reg(r_src)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, r_dst, r_dst,
                                         MIPSRH_Imm(False, 31)));
            return r_dst;
         }

         case Iop_Left8:
         case Iop_Left16:
         case Iop_Left32:
         case Iop_Left64: {
            if (op_unop == Iop_Left64 && !mode64)
               goto irreducible;
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            MIPSAluOp op = (op_unop == Iop_Left64) ? Malu_DSUB : Malu_SUB;
            addInstr(env, MIPSInstr_Alu(op, r_dst,
                                        hregMIPS_GPR0(mode64),
                                        MIPSRH_Reg(r_src)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, r_dst,
                          MIPSRH_Reg(r_src)));
            return r_dst;
         }

         case Iop_Clz64:
            vassert(mode64);
	    /* fallthrough */
         case Iop_Clz32: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            MIPSUnaryOp op = (op_unop == Iop_Clz64) ? Mun_DCLZ : Mun_CLZ;
            addInstr(env, MIPSInstr_Unary(op, r_dst, r_src));
            return r_dst;
         }

         case Iop_CmpNEZ64: {
            HReg hi, lo;
            HReg r_dst = newVRegI(env);
            HReg r_src;
            if (env->mode64) {
               r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            } else {
               r_src = newVRegI(env);
               iselInt64Expr(&hi, &lo, env, e->Iex.Unop.arg);
               addInstr(env, MIPSInstr_Alu(Malu_OR, r_src, lo, MIPSRH_Reg(hi)));
            }
            addInstr(env, MIPSInstr_Cmp(False, !(env->mode64), r_dst, r_src,
                                        hregMIPS_GPR0(mode64), MIPScc_NE));
            return r_dst;
         }

         case Iop_CmpwNEZ64: {
            HReg tmp1;
            HReg tmp2 = newVRegI(env);
            vassert(env->mode64);
            tmp1 = iselWordExpr_R(env, e->Iex.Unop.arg);

            addInstr(env, MIPSInstr_Alu(Malu_DSUB, tmp2, hregMIPS_GPR0(mode64),
                          MIPSRH_Reg(tmp1)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, tmp2, tmp2, MIPSRH_Reg(tmp1)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, False, tmp2, tmp2,
                                         MIPSRH_Imm (False, 63)));
            return tmp2;
         }

         case Iop_128HIto64: {
            vassert(mode64);
            HReg rHi, rLo;
            iselInt128Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
            return rHi;  /* and abandon rLo .. poor wee thing :-) */
         }

         case Iop_128to64: {
            vassert(mode64);
            HReg rHi, rLo;
            iselInt128Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
            return rLo;  /* and abandon rLo .. poor wee thing :-) */
         }

         case Iop_V128to32: {
            HReg i_dst = newVRegI(env);
            HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
            vassert(has_msa);
            addInstr(env,
                     MIPSInstr_MsaElm(MSA_COPY_S, v_src, i_dst, MSA_DFN_W));
            return i_dst;
         }

         case Iop_V128HIto64: {
            vassert(mode64);
            vassert(has_msa);
            HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
            HReg reg = newVRegI(env);
            addInstr(env,
                     MIPSInstr_MsaElm(MSA_COPY_S, v_src, reg, MSA_DFN_D | 1));
            return reg;
         }

         case Iop_V128to64: {
            vassert(mode64);
            vassert(has_msa);
            HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
            HReg reg = newVRegI(env);
            addInstr(env,
                     MIPSInstr_MsaElm(MSA_COPY_S, v_src, reg, MSA_DFN_D | 0));
            return reg;
         }

         case Iop_F32toF16x4_DEP: {
            vassert(mode64);
            vassert(has_msa);
            HReg v_arg = iselV128Expr(env, e->Iex.Unop.arg);
            HReg v_src = newVRegV(env);
            set_guest_MIPS_rounding_mode_MSA(env);
            addInstr(env,
                     MIPSInstr_Msa3RF(MSA_FEXDO, MSA_F_WH,
                                      v_src, v_arg, v_arg));
            set_MIPS_rounding_default_MSA(env);
            HReg reg = newVRegI(env);
            addInstr(env,
                     MIPSInstr_MsaElm(MSA_COPY_S, v_src, reg, MSA_DFN_D | 0));
            return reg;
         }


         default:
            break;
      }

      /* -------- DSP ASE -------- */
      /* All Unop cases involving host-side helper calls. */
      void* fn = NULL;
      switch (e->Iex.Unop.op) {
         case Iop_CmpNEZ16x2:
            fn = &h_generic_calc_CmpNEZ16x2; break;
         case Iop_CmpNEZ8x4:
            fn = &h_generic_calc_CmpNEZ8x4; break;
         default:
            break;
      }

      RetLoc rloc = mk_RetLoc_INVALID();
      if (ty == Ity_I32) {
         rloc = mk_RetLoc_simple(RLPri_Int);
      }
      else if (ty == Ity_I64) {
         rloc = mode64 ? mk_RetLoc_simple(RLPri_Int) :
                         mk_RetLoc_simple(RLPri_2Int);
      }
      else {
         goto irreducible;
      }

      if (fn) {
         HReg regL = iselWordExpr_R(env, e->Iex.Unop.arg);
         HReg res  = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(hregMIPS_GPR4(env->mode64), regL));
         argiregs |= (1 << 4);
         addInstr(env, MIPSInstr_CallAlways( MIPScc_AL,
                                             (Addr)fn,
                                             argiregs, rloc));
         addInstr(env, mk_iMOVds_RR(res, hregMIPS_GPR2(env->mode64)));
         return res;
      }

      break;
   }

   /* --------- GET --------- */
   case Iex_Get: {
      if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32
          || ((ty == Ity_I64) && mode64)) {
         HReg r_dst = newVRegI(env);

         MIPSAMode *am_addr = MIPSAMode_IR(e->Iex.Get.offset,
                                           GuestStatePointer(mode64));
         addInstr(env, MIPSInstr_Load(toUChar(sizeofIRType(ty)), r_dst, am_addr,
                                      mode64));
         return r_dst;
      }
      break;
   }

   /* --------- ITE --------- */
   case Iex_ITE: {
      if ((ty == Ity_I8 || ty == Ity_I16 ||
           ty == Ity_I32 || ((ty == Ity_I64))) &&
           typeOfIRExpr(env->type_env, e->Iex.ITE.cond) == Ity_I1) {
         HReg r0     = iselWordExpr_R(env, e->Iex.ITE.iffalse);
         HReg r1     = iselWordExpr_R(env, e->Iex.ITE.iftrue);
         HReg r_cond = iselWordExpr_R(env, e->Iex.ITE.cond);
         HReg r_dst = newVRegI(env);
         /*
          * r_dst = r0
          * movn r_dst, r1, r_cond
          */
#if (__mips_isa_rev >= 6)
         HReg r_temp  = newVRegI(env);
         addInstr(env, MIPSInstr_MoveCond(MSeleqz, r_dst, r0, r_cond));
         addInstr(env, MIPSInstr_MoveCond(MSelnez, r_temp, r1, r_cond));
         addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, r_dst,
                          MIPSRH_Reg(r_temp)));

#else
         addInstr(env, mk_iMOVds_RR(r_dst, r0));
         addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, r_dst, r1, r_cond));
#endif
         return r_dst;
      }
      break;
   }

   /* --------- LITERAL --------- */
   /* 32/16/8-bit literals */
   case Iex_Const: {
      Long l;
      HReg r_dst = newVRegI(env);
      IRConst *con = e->Iex.Const.con;
      switch (con->tag) {
         case Ico_U64:
            if (!mode64)
               goto irreducible;
            l = (Long) con->Ico.U64;
            break;
         case Ico_U32:
            l = (Long) (Int) con->Ico.U32;
            break;
         case Ico_U16:
            l = (Long) (Int) (Short) con->Ico.U16;
            break;
         case Ico_U8:
            l = (Long) (Int) (Char) con->Ico.U8;
            break;
#if 0
         // Not needed until chasing cond branches in bb_to_IR is enabled on
         // MIPS.  See comment on And1/Or1 below.
         case Ico_U1:
            l = con->Ico.U1 ? 1 : 0;
            break;
#endif
         default:
            vpanic("iselIntExpr_R.const(mips)");
      }
      addInstr(env, MIPSInstr_LI(r_dst, (ULong) l));
      return r_dst;
   }

   /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg r_dst = newVRegI(env);
      vassert(ty == e->Iex.CCall.retty);

      /* be very restrictive for now.  Only 32/64-bit ints allowed for
         args, and 64 and 32 bits for return type.  Don't forget to change
         the RetLoc if more return types are allowed in future. */
      if (e->Iex.CCall.retty != Ity_I64 && e->Iex.CCall.retty != Ity_I32)
         goto irreducible;

      /* Marshal args, do the call, clear stack. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      doHelperCall(&addToSp, &rloc, env, NULL/*guard*/, e->Iex.CCall.cee,
                   e->Iex.CCall.retty, e->Iex.CCall.args );

      vassert(is_sane_RetLoc(rloc));
      vassert(rloc.pri == RLPri_Int);
      vassert(addToSp == 0);
      addInstr(env, mk_iMOVds_RR(r_dst, hregMIPS_GPR2(mode64)));
      return r_dst;
   }

#if (__mips_isa_rev >= 6)
   case Iex_Qop: {
      HReg dst = newVRegI(env);
      HReg src1 = iselWordExpr_R(env, e->Iex.Qop.details->arg1);
      HReg src2 = iselWordExpr_R(env, e->Iex.Qop.details->arg2);
      HReg src3 = iselWordExpr_R(env, e->Iex.Qop.details->arg3);
      HReg src4 = iselWordExpr_R(env, e->Iex.Qop.details->arg4);
      switch (e->Iex.Qop.details->op) {
        case Iop_Rotx32:
          addInstr(env, MIPSInstr_Bitswap(Rotx32, dst, src1, src2, src3, src4));
          break;
        case Iop_Rotx64:
          addInstr(env, MIPSInstr_Bitswap(Rotx64, dst, src1, src2, src3, src4));
          break;
        default:
          break;
      }
      return dst;
   }
#endif

   default:
      break;
   }  /* end switch(e->tag) */

   /* We get here if no pattern matched. */
   irreducible:
      vex_printf("--------------->\n");
      if (e->tag == Iex_RdTmp)
         vex_printf("Iex_RdTmp \n");
      ppIRExpr(e);

      vpanic("iselWordExpr_R(mips): cannot reduce tree");
}

/* --------------------- RH --------------------- */

/* Compute an I8/I16/I32 (and I64, in 64-bit mode) into a RH
   (reg-or-halfword-immediate).  It's important to specify whether the
   immediate is to be regarded as signed or not.  If yes, this will
   never return -32768 as an immediate; this guaranteed that all
   signed immediates that are return can have their sign inverted if
   need be. */

static MIPSRH *iselWordExpr_RH(ISelEnv * env, Bool syned, IRExpr * e)
{
   MIPSRH *ri = iselWordExpr_RH_wrk(env, syned, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Mrh_Imm:
         vassert(ri->Mrh.Imm.syned == syned);
         if (syned)
            vassert(ri->Mrh.Imm.imm16 != 0x8000);
         return ri;
      case Mrh_Reg:
         vassert(hregClass(ri->Mrh.Reg.reg) == HRcGPR(env->mode64));
         vassert(hregIsVirtual(ri->Mrh.Reg.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RH: unknown mips RH tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSRH *iselWordExpr_RH_wrk(ISelEnv * env, Bool syned, IRExpr * e)
{
   ULong u;
   Long l;
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 ||
          ((ty == Ity_I64) && env->mode64));

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      IRConst *con = e->Iex.Const.con;
      /* What value are we aiming to generate? */
      switch (con->tag) {
         /* Note: Not sign-extending - we carry 'syned' around */
         case Ico_U64:
            vassert(env->mode64);
            u = con->Ico.U64;
            break;
         case Ico_U32:
            u = 0xFFFFFFFF & con->Ico.U32;
            break;
         case Ico_U16:
            u = 0x0000FFFF & con->Ico.U16;
            break;
         case Ico_U8:
            u = 0x000000FF & con->Ico.U8;
            break;
         default:
            vpanic("iselIntExpr_RH.Iex_Const(mips)");
      }
      l = (Long) u;
      /* Now figure out if it's representable. */
      if (!syned && u <= 65535) {
         return MIPSRH_Imm(False /*unsigned */ , toUShort(u & 0xFFFF));
      }
      if (syned && l >= -32767 && l <= 32767) {
         return MIPSRH_Imm(True /*signed */ , toUShort(u & 0xFFFF));
      }
      /* no luck; use the Slow Way. */
   }
   /* default case: calculate into a register and return that */
   return MIPSRH_Reg(iselWordExpr_R(env, e));
}

/* --------------------- RH5u --------------------- */

/* Compute an I8 into a reg-or-5-bit-unsigned-immediate, the latter
   being an immediate in the range 1 .. 31 inclusive.  Used for doing
   shift amounts. */

static MIPSRH *iselWordExpr_RH5u(ISelEnv * env, IRExpr * e)
{
   MIPSRH *ri;
   ri = iselWordExpr_RH5u_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Mrh_Imm:
         vassert(ri->Mrh.Imm.imm16 >= 1 && ri->Mrh.Imm.imm16 <= 31);
         vassert(!ri->Mrh.Imm.syned);
         return ri;
      case Mrh_Reg:
         vassert(hregClass(ri->Mrh.Reg.reg) == HRcInt32);
         vassert(hregIsVirtual(ri->Mrh.Reg.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RH5u: unknown mips RH tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSRH *iselWordExpr_RH5u_wrk(ISelEnv * env, IRExpr * e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const
       && e->Iex.Const.con->tag == Ico_U8
       && e->Iex.Const.con->Ico.U8 >= 1 && e->Iex.Const.con->Ico.U8 <= 31) {
      return MIPSRH_Imm(False /*unsigned */ , e->Iex.Const.con->Ico.U8);
   }

   /* default case: calculate into a register and return that */
   return MIPSRH_Reg(iselWordExpr_R(env, e));
}

/* --------------------- RH6u --------------------- */

static MIPSRH *iselWordExpr_RH6u ( ISelEnv * env, IRExpr * e )
{
   MIPSRH *ri;
   ri = iselWordExpr_RH6u_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
   case Mrh_Imm:
      vassert(ri->Mrh.Imm.imm16 >= 1 && ri->Mrh.Imm.imm16 <= 63);
      vassert(!ri->Mrh.Imm.syned);
      return ri;
   case Mrh_Reg:
      vassert(hregClass(ri->Mrh.Reg.reg) == HRcGPR(env->mode64));
      vassert(hregIsVirtual(ri->Mrh.Reg.reg));
      return ri;
   default:
      vpanic("iselIntExpr_RH6u: unknown RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSRH *iselWordExpr_RH6u_wrk ( ISelEnv * env, IRExpr * e )
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const
       && e->Iex.Const.con->tag == Ico_U8
       && e->Iex.Const.con->Ico.U8 >= 1 && e->Iex.Const.con->Ico.U8 <= 63)
   {
      return MIPSRH_Imm(False /*unsigned */ ,
              e->Iex.Const.con->Ico.U8);
   }

   /* default case: calculate into a register and return that */
   return MIPSRH_Reg(iselWordExpr_R(env, e));
}
/* --------------------- RH7u --------------------- */

static MIPSRH *iselWordExpr_RH7u ( ISelEnv * env, IRExpr * e )
{
   MIPSRH *ri;
   ri = iselWordExpr_RH7u_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
   case Mrh_Imm:
      vassert(ri->Mrh.Imm.imm16 >= 1 && ri->Mrh.Imm.imm16 <= 127);
      vassert(!ri->Mrh.Imm.syned);
      return ri;
   case Mrh_Reg:
      vassert(hregClass(ri->Mrh.Reg.reg) == HRcGPR(env->mode64));
      vassert(hregIsVirtual(ri->Mrh.Reg.reg));
      return ri;
   default:
      vpanic("iselIntExpr_RH7u: unknown RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSRH *iselWordExpr_RH7u_wrk ( ISelEnv * env, IRExpr * e )
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const
       && e->Iex.Const.con->tag == Ico_U8
       && e->Iex.Const.con->Ico.U8 >= 1 && e->Iex.Const.con->Ico.U8 <= 127)
   {
      return MIPSRH_Imm(False /*unsigned */ ,
              e->Iex.Const.con->Ico.U8);
   }

   /* default case: calculate into a register and return that */
   return MIPSRH_Reg(iselWordExpr_R(env, e));
}


/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static MIPSCondCode iselCondCode(ISelEnv * env, IRExpr * e)
{
   MIPSCondCode cc = iselCondCode_wrk(env,e);
   vassert(cc != MIPScc_NV);
   return cc;
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSCondCode iselCondCode_wrk(ISelEnv * env, IRExpr * e)
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I1);
   /* Cmp*32*(x,y) ? */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_CmpEQ32
           || e->Iex.Binop.op == Iop_CmpNE32
           || e->Iex.Binop.op == Iop_CmpNE64
           || e->Iex.Binop.op == Iop_CmpLT32S
           || e->Iex.Binop.op == Iop_CmpLT32U
           || e->Iex.Binop.op == Iop_CmpLT64U
           || e->Iex.Binop.op == Iop_CmpLE32S
           || e->Iex.Binop.op == Iop_CmpLE64S
           || e->Iex.Binop.op == Iop_CmpLT64S
           || e->Iex.Binop.op == Iop_CmpEQ64
           || e->Iex.Binop.op == Iop_CasCmpEQ32
           || e->Iex.Binop.op == Iop_CasCmpEQ64)) {

      Bool syned = (e->Iex.Binop.op == Iop_CmpLT32S
                   || e->Iex.Binop.op == Iop_CmpLE32S
                   || e->Iex.Binop.op == Iop_CmpLT64S
                   || e->Iex.Binop.op == Iop_CmpLE64S);
      Bool size32;
      HReg dst = newVRegI(env);
      HReg r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
      HReg r2 = iselWordExpr_R(env, e->Iex.Binop.arg2);

      MIPSCondCode cc;

      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ32:
         case Iop_CasCmpEQ32:
            cc = MIPScc_EQ;
            size32 = True;
            break;
         case Iop_CmpNE32:
            cc = MIPScc_NE;
            size32 = True;
            break;
         case Iop_CmpNE64:
            cc = MIPScc_NE;
            size32 = False;
            break;
         case Iop_CmpLT32S:
            cc = MIPScc_LT;
            size32 = True;
            break;
         case Iop_CmpLT32U:
            cc = MIPScc_LO;
            size32 = True;
            break;
         case Iop_CmpLT64U:
            cc = MIPScc_LO;
            size32 = False;
            break;
         case Iop_CmpLE32S:
            cc = MIPScc_LE;
            size32 = True;
            break;
         case Iop_CmpLE64S:
            cc = MIPScc_LE;
            size32 = False;
            break;
         case Iop_CmpLT64S:
            cc = MIPScc_LT;
            size32 = False;
            break;
         case Iop_CmpEQ64:
         case Iop_CasCmpEQ64:
            cc = MIPScc_EQ;
            size32 = False;
            break;
         default:
            vpanic("iselCondCode(mips): CmpXX32 or CmpXX64");
            break;
      }

      addInstr(env, MIPSInstr_Cmp(syned, size32, dst, r1, r2, cc));
      /* Store result to guest_COND */
      MIPSAMode *am_addr = MIPSAMode_IR(0, GuestStatePointer(mode64));

      addInstr(env, MIPSInstr_Store(4,
               MIPSAMode_IR(am_addr->Mam.IR.index + COND_OFFSET(mode64),
                            am_addr->Mam.IR.base),
               dst, mode64));
      return cc;
   }
   if (e->tag == Iex_Unop && e->Iex.Binop.op == Iop_Not1) {
      HReg r_dst = newVRegI(env);
      HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
      MIPSRH *r_srcR = MIPSRH_Reg(r_srcL);

      addInstr(env, MIPSInstr_LI(r_dst, 0x1));
      addInstr(env, MIPSInstr_Alu(Malu_SUB, r_dst, r_dst, r_srcR));
      /* Store result to guest_COND */
      MIPSAMode *am_addr = MIPSAMode_IR(0, GuestStatePointer(mode64));

      addInstr(env, MIPSInstr_Store(4,
               MIPSAMode_IR(am_addr->Mam.IR.index + COND_OFFSET(mode64),
                            am_addr->Mam.IR.base),
               r_dst, mode64));
      return MIPScc_NE;
   }
#if 0
   // sewardj 2019Dec14: this is my best attempt at And1/Or1, but I am not
   // sure if it is correct.  In any case it is not needed until chasing cond
   // branches is enabled on MIPS.  Currently it is disabled, in function bb_to_IR
   // (see comments there).
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_And1 || e->Iex.Binop.op == Iop_Or1)) {
      HReg r_argL = iselWordExpr_R(env, e->Iex.Binop.arg1);
      HReg r_argR = iselWordExpr_R(env, e->Iex.Binop.arg2);
      HReg r_dst  = newVRegI(env);
      addInstr(env, MIPSInstr_Alu(e->Iex.Binop.op == Iop_And1 ? Malu_AND : Malu_OR,
                                  r_dst, r_argL, MIPSRH_Reg(r_argR)));
      addInstr(env, MIPSInstr_Alu(Malu_AND, r_dst, r_dst, MIPSRH_Imm(False, 1)));
      /* Store result to guest_COND */
      /* sewardj 2019Dec13: this seems wrong to me.  The host-side instruction
         selector shouldn't touch the guest-side state, except in response to
         Iex_Get and Ist_Put. */
      MIPSAMode *am_addr = MIPSAMode_IR(0, GuestStatePointer(mode64));

      addInstr(env, MIPSInstr_Store(4,
               MIPSAMode_IR(am_addr->Mam.IR.index + COND_OFFSET(mode64),
                            am_addr->Mam.IR.base),
               r_dst, mode64));
      return MIPScc_EQ;
   }
#endif
   if (e->tag == Iex_RdTmp) {
      HReg r_dst = iselWordExpr_R_wrk(env, e);
      /* Store result to guest_COND */
      MIPSAMode *am_addr = MIPSAMode_IR(0, GuestStatePointer(mode64));

      addInstr(env, MIPSInstr_Store(4,
               MIPSAMode_IR(am_addr->Mam.IR.index + COND_OFFSET(mode64),
                            am_addr->Mam.IR.base),
               r_dst, mode64));
      return MIPScc_EQ;
   }

   vex_printf("iselCondCode(mips): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselCondCode(mips)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Vector expressions (128 bit - SIMD)         ---*/
/*---------------------------------------------------------*/

/* Compute a vector value into vector register.            */
static HReg iselV128Expr (ISelEnv* env, IRExpr* e) {
   vassert(has_msa);
   HReg r = iselV128Expr_wrk(env, e);
   vassert(hregClass(r) == HRcVec128);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselV128Expr_wrk(ISelEnv* env, IRExpr* e) {
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(e);
   vassert(ty == Ity_V128);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Load) {
      vassert (e->Iex.Load.ty == Ity_V128);
      HReg v_dst = newVRegV(env);
      addInstr(env, MIPSInstr_MsaMi10(MSA_LD, 0, iselWordExpr_R(env,
                                      e->Iex.Load.addr), v_dst, MSA_B));
      return v_dst;
   }

   if (e->tag == Iex_Get) {
      HReg v_dst = newVRegV(env);
#if defined(_MIPSEB)
      HReg r_addr = newVRegI(env);
      addInstr(env, MIPSInstr_Alu(mode64 ? Malu_DADD : Malu_ADD, r_addr, GuestStatePointer(mode64),
                                  MIPSRH_Imm(False, e->Iex.Get.offset)));
      addInstr(env, MIPSInstr_MsaMi10(MSA_LD, 0, r_addr, v_dst, MSA_B));
#else
      vassert(!(e->Iex.Get.offset & 7));
      addInstr(env, MIPSInstr_MsaMi10(MSA_LD, e->Iex.Get.offset >> 3,
                                      GuestStatePointer(mode64), v_dst, MSA_D));
#endif
      return v_dst;
   }

   if (e->tag == Iex_Unop) {
      IROp op_unop = e->Iex.Unop.op;

      switch (op_unop) {
         case Iop_Abs64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_help = newVRegV(env);
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_D, v_help, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADD_A, MSA_D,
                                        v_dst, v_src, v_help));
               return v_dst;
            }

         case Iop_Abs32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_help = newVRegV(env);
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_W, v_help, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADD_A, MSA_W,
                                        v_dst, v_src, v_help));
               return v_dst;
            }

         case Iop_Abs16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_help = newVRegV(env);
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_H, v_help, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADD_A, MSA_H,
                                        v_dst, v_src, v_help));
               return v_dst;
            }

         case Iop_Abs8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_help = newVRegV(env);
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_B, v_help, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADD_A, MSA_B,
                                        v_dst, v_src, v_help));
               return v_dst;
            }

         case Iop_Cnt8x16: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg res = newVRegV(env);
               addInstr(env, MIPSInstr_Msa2R(MSA_PCNT, MSA_B, v_src, res));
               return res;
            }

         case Iop_NotV128: {
               HReg v_dst = newVRegV(env);
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               addInstr(env, MIPSInstr_MsaVec(MSA_NORV, v_dst, v_src, v_src));
               return v_dst;
            }

         case Iop_Reverse8sIn16_x8: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_tmp = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_B, v_tmp, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_B, v_src, v_tmp, v_src));
               return v_src;
            }

         case Iop_Reverse8sIn32_x4: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_tmp = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_H, v_tmp, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_H, v_src, v_tmp, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_B, v_tmp, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_B, v_src, v_tmp, v_src));
               return v_src;
            }

         case Iop_Reverse8sIn64_x2: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_tmp = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_W, v_tmp, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_W, v_src, v_tmp, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_H, v_tmp, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_H, v_src, v_tmp, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_B, v_tmp, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_B, v_src, v_tmp, v_src));
               return v_src;
            }

         case Iop_Cls8x16: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env, MIPSInstr_Msa2R(MSA_NLOC, MSA_B, v_src, v_dst));
               return v_dst;
            }

         case Iop_Cls16x8: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env, MIPSInstr_Msa2R(MSA_NLOC, MSA_H, v_src, v_dst));
               return v_dst;
            }

         case Iop_Cls32x4: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env, MIPSInstr_Msa2R(MSA_NLOC, MSA_W, v_src, v_dst));
               return v_dst;
            }

         case Iop_Clz8x16: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env, MIPSInstr_Msa2R(MSA_NLZC, MSA_B, v_src, v_dst));
               return v_dst;
            }

         case Iop_Clz16x8: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env, MIPSInstr_Msa2R(MSA_NLZC, MSA_H, v_src, v_dst));
               return v_dst;
            }

         case Iop_Clz32x4: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env, MIPSInstr_Msa2R(MSA_NLZC, MSA_W, v_src, v_dst));
               return v_dst;
            }

         case Iop_Clz64x2: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env, MIPSInstr_Msa2R(MSA_NLZC, MSA_D, v_src, v_dst));
               return v_dst;
            }

         case Iop_Abs32Fx4: {
               HReg v_src  = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst  = newVRegV(env);
               HReg v_help = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FMUL, MSA_F_WH,
                                         v_help, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FSQRT, MSA_F_WH, v_dst, v_help));
               return v_dst;
            }

         case Iop_Abs64Fx2: {
               HReg v_src  = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst  = newVRegV(env);
               HReg v_help = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FMUL, MSA_F_DW,
                                         v_help, v_src, v_src));
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FSQRT, MSA_F_DW, v_dst, v_help));
               return v_dst;
            }

         case Iop_RecipEst32Fx4: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               set_guest_MIPS_rounding_mode_MSA(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FRCP, MSA_F_WH, v_dst, v_src));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_RecipEst64Fx2: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               set_guest_MIPS_rounding_mode_MSA(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FRCP, MSA_F_DW, v_dst, v_src));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_RSqrtEst32Fx4: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               set_guest_MIPS_rounding_mode_MSA(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FRSQRT, MSA_F_WH, v_dst, v_src));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_RSqrtEst64Fx2: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               set_guest_MIPS_rounding_mode_MSA(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FRSQRT, MSA_F_DW, v_dst, v_src));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_F16toF32x4: {
               HReg v_dst = newVRegV(env);

               if (mode64) {
                  HReg r_src;
                  r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
                  addInstr(env,
                           MIPSInstr_Msa2R(MSA_FILL, MSA_D, r_src, v_dst));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_INSERT, r_src, v_dst,
                                            MSA_DFN_D | 1));
               } else {
                  HReg r_srch, r_srcl;
                  iselInt64Expr(&r_srch, &r_srcl, env, e->Iex.Unop.arg);
                  addInstr(env,
                           MIPSInstr_Msa2R(MSA_FILL, MSA_W, r_srcl, v_dst));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_INSERT, r_srch, v_dst,
                                            MSA_DFN_W | 1));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_INSERT, r_srcl, v_dst,
                                            MSA_DFN_W | 2));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_INSERT, r_srch, v_dst,
                                            MSA_DFN_W | 3));
               }

               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FEXUPR, MSA_F_WH, v_dst, v_dst));
               return v_dst;
            }

         case Iop_I32UtoF32x4_DEP: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               set_guest_MIPS_rounding_mode_MSA(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FFINT_U, MSA_F_WH, v_dst, v_src));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_F32toI32Sx4_RZ: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FTRUNC_S, MSA_F_WH, v_dst, v_src));
               return v_dst;
            }

         case Iop_F32toI32Ux4_RZ: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FTRUNC_U, MSA_F_WH, v_dst, v_src));
               return v_dst;
            }

         case Iop_Log2_32Fx4: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FLOG2, MSA_F_WH, v_dst, v_src));
               return v_dst;
            }

         case Iop_Log2_64Fx2: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FLOG2, MSA_F_DW, v_dst, v_src));
               return v_dst;
            }
         case Iop_CmpNEZ8x16: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               HReg zero = Zero(mode64);
               addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, zero, v_dst));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CEQ, MSA_B, v_dst, v_src, v_dst));
               addInstr(env, MIPSInstr_MsaVec(MSA_NORV, v_dst, v_dst, v_dst));
               return v_dst;
            }
         case Iop_CmpNEZ16x8: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               HReg zero = Zero(mode64);
               addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, zero, v_dst));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CEQ, MSA_H, v_dst, v_src, v_dst));
               addInstr(env, MIPSInstr_MsaVec(MSA_NORV, v_dst, v_dst, v_dst));
               return v_dst;
            }
          case Iop_CmpNEZ32x4: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               HReg zero = Zero(mode64);
               addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, zero, v_dst));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CEQ, MSA_W, v_dst, v_src, v_dst));
               addInstr(env, MIPSInstr_MsaVec(MSA_NORV, v_dst, v_dst, v_dst));
               return v_dst;
            }
          case Iop_CmpNEZ64x2: {
               HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
               HReg v_dst = newVRegV(env);
               HReg zero = Zero(mode64);
               addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, zero, v_dst));
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CEQ, MSA_D, v_dst, v_src, v_dst));
               addInstr(env, MIPSInstr_MsaVec(MSA_NORV, v_dst, v_dst, v_dst));
               return v_dst;
            }
         default:
            vex_printf("iselV128Expr_wrk: Unsupported unop: %u\n", op_unop);
      }
   }

   if (e->tag == Iex_Binop) {
      IROp op_binop = e->Iex.Binop.op;

      switch (op_binop) {
         case Iop_Add8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDV, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Add16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDV, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Add32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDV, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Add64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDV, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sub8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sub16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sub32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sub64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QAdd8Sx16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDS_S, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QAdd16Sx8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDS_S, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QAdd32Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDS_S, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QAdd64Sx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDS_S, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QAdd8Ux16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDS_U, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QAdd16Ux8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDS_U, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QAdd32Ux4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDS_U, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QAdd64Ux2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ADDS_U, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QSub8Sx16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBS_S, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QSub16Sx8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBS_S, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QSub32Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBS_S, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QSub64Sx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBS_S, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QSub8Ux16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBS_U, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QSub16Ux8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBS_U, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QSub32Ux4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBS_U, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QSub64Ux2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBS_U, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QDMulHi32Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_MUL_Q, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QDMulHi16Sx8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_MUL_Q, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QRDMulHi32Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_MULR_Q, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_QRDMulHi16Sx8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_MULR_Q, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max8Sx16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MAX_S, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max16Sx8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MAX_S, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max32Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MAX_S, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max64Sx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MAX_S, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max8Ux16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MAX_U, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max16Ux8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MAX_U, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max32Ux4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MAX_U, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max64Ux2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MAX_U, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min8Sx16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MIN_S, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min16Sx8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MIN_S, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min32Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MIN_S, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min64Sx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MIN_S, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min8Ux16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MIN_U, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min16Ux8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MIN_U, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min32Ux4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MIN_U, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min64Ux2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MIN_U, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Shl8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SLL, MSA_B, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Shl16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SLL, MSA_H, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Shl32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SLL, MSA_W, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Shl64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SLL, MSA_D, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Shr8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SRL, MSA_B, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Shr16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SRL, MSA_H,  v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Shr32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SRL, MSA_W, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Shr64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SRL, MSA_D, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sar8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SRA, MSA_B, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sar16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SRA, MSA_H, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sar32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SRA, MSA_W, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sar64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SRA, MSA_D, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveHI8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVL, MSA_B, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveHI16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVL, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveHI32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVL, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveHI64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVL, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveLO8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVR, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveLO16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVR, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveLO32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVR, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveLO64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVR, MSA_D,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveEvenLanes8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveEvenLanes16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveEvenLanes32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVEV, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveOddLanes8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveOddLanes16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_InterleaveOddLanes32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_ILVOD, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

          case Iop_PackEvenLanes8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_PCKEV, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_PackEvenLanes16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_PCKEV, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_PackEvenLanes32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_PCKEV, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_PackOddLanes8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_PCKOD, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_PackOddLanes16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_PCKOD, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_PackOddLanes32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_PCKOD, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpEQ8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CEQ, MSA_B, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpEQ16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CEQ, MSA_H, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpEQ32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CEQ, MSA_W, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpEQ64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CEQ, MSA_D, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpGT8Sx16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CLT_S, MSA_B,
                                        v_dst, v_src2, v_src1));
               return v_dst;
            }

         case Iop_CmpGT16Sx8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CLT_S, MSA_H,
                                        v_dst, v_src2, v_src1));
               return v_dst;
            }

         case Iop_CmpGT32Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CLT_S, MSA_W,
                                        v_dst, v_src2, v_src1));
               return v_dst;
            }

         case Iop_CmpGT64Sx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CLT_S, MSA_D,
                                        v_dst, v_src2, v_src1));
               return v_dst;
            }

         case Iop_CmpGT8Ux16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CLT_U, MSA_B,
                                        v_dst, v_src2, v_src1));
               return v_dst;
            }

         case Iop_CmpGT16Ux8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CLT_U, MSA_H,
                                        v_dst, v_src2, v_src1));
               return v_dst;
            }

         case Iop_CmpGT32Ux4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CLT_U, MSA_W,
                                        v_dst, v_src2, v_src1));
               return v_dst;
            }

         case Iop_CmpGT64Ux2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_CLT_U, MSA_D,
                                        v_dst, v_src2, v_src1));
               return v_dst;
            }

         case Iop_Avg8Sx16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_AVER_S, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Avg16Sx8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_AVER_S, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Avg32Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_AVER_S, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Avg8Ux16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_AVER_U, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Avg16Ux8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_AVER_U, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Avg32Ux4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_AVER_U, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Mul8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MULV, MSA_B,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Mul16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MULV, MSA_H,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Mul32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_MULV, MSA_W,
                                        v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_AndV128: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env, MIPSInstr_MsaVec(MSA_ANDV, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_OrV128: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env, MIPSInstr_MsaVec(MSA_ORV, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_XorV128: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env, MIPSInstr_MsaVec(MSA_XORV, v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_ShrV128: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               MIPSRH *sm;
               sm = iselWordExpr_RH7u(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_B,
                                        v_dst, v_src1, v_src1));

               if (sm->tag == Mrh_Imm) {
                  int n = (sm->Mrh.Imm.imm16) >> 3;
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_SLDI, v_src1, v_dst,
                                            MSA_DFN_B | n));
               } else {
                  HReg v_src2 = sm->Mrh.Reg.reg;
                  MIPSRH *ri = MIPSRH_Imm(False, 3);
                  HReg r_dst = newVRegI(env);
                  addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /*32bit shift */,
                                               r_dst, v_src2, ri));
                  addInstr(env,
                           MIPSInstr_Msa3R(MSA_SLD, MSA_B,
                                           v_dst, v_src1, r_dst));
               }

               return v_dst;
            }

         case Iop_ShlV128: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               MIPSRH *sm;
               sm = iselWordExpr_RH7u(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3R(MSA_SUBV, MSA_B,
                                        v_dst, v_src1, v_src1));

               if (sm->tag == Mrh_Imm) {
                  int n = 16 - ((sm->Mrh.Imm.imm16) >> 3);

                  if (n == 16) n = 0;

                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_SLDI, v_dst, v_src1,
                                            MSA_DFN_B | n));
               } else {
                  HReg v_src2 = sm->Mrh.Reg.reg;
                  MIPSRH *ri = MIPSRH_Imm(False, 3);
                  HReg r_dst = newVRegI(env);
                  HReg help = newVRegI(env);
                  addInstr(env, MIPSInstr_Alu(Malu_XOR, help, v_src2, sm));
                  addInstr(env, MIPSInstr_Alu(Malu_SUB, help, help, sm));
                  addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /*32bit shift */,
                                               r_dst, help, ri));
                  addInstr(env,
                           MIPSInstr_Msa3R(MSA_SLD, MSA_B,
                                           v_src1, v_dst, r_dst));
               }

               return v_src1;
            }

         case Iop_ShlN8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SLLI, MSA_B,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_ShlN16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SLLI, MSA_H,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_ShlN32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SLLI, MSA_W,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_ShlN64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SLLI, MSA_D,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_SarN8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRAI, MSA_B,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_SarN16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRAI, MSA_H,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_SarN32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRAI, MSA_W,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_SarN64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRAI, MSA_D,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_ShrN8x16: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRLI, MSA_B,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_ShrN16x8: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRLI, MSA_H,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_ShrN32x4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRLI, MSA_W,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_ShrN64x2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRLI, MSA_D,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               return v_dst;
            }

         case Iop_QandQSarNnarrow64Sto32Sx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRAI, MSA_D,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               addInstr(env, MIPSInstr_MsaBit(MSA_SAT_S, MSA_D, 31, v_dst, v_dst));
               return v_dst;
            }

         case Iop_QandQSarNnarrow32Sto16Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRAI, MSA_W,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SAT_S, MSA_W, 15, v_dst, v_dst));
               return v_dst;
            }

         case Iop_QandQRSarNnarrow64Sto32Sx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRARI, MSA_D,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SAT_S, MSA_D, 31, v_dst, v_dst));
               return v_dst;
            }

         case Iop_QandQRSarNnarrow32Sto16Sx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               vassert(e->Iex.Binop.arg2->tag == Iex_Const);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               vassert(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 <= 63);
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SRARI, MSA_W,
                                         e->Iex.Binop.arg2->Iex.Const.con->Ico.U8,
                                         v_src1, v_dst));
               addInstr(env,
                        MIPSInstr_MsaBit(MSA_SAT_S, MSA_W, 15, v_dst, v_dst));
               return v_dst;
            }

         case Iop_CmpEQ32Fx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FCEQ, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpEQ64Fx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FCEQ, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpLT32Fx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FCLT, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpLT64Fx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FCLT, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpLE32Fx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FCLE, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpLE64Fx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FCLE, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpUN32Fx4: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FCUN, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_CmpUN64Fx2: {
               HReg v_dst = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FCUN, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_64HLtoV128: {
               HReg v_dst = newVRegV(env);

               if (mode64) {
                  HReg r_src1;
                  HReg r_src2;
                  r_src1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
                  r_src2 = iselWordExpr_R(env, e->Iex.Binop.arg2);
                  addInstr(env,
                           MIPSInstr_Msa2R(MSA_FILL, MSA_D, r_src2, v_dst));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_INSERT, r_src1, v_dst,
                                            MSA_DFN_D | 1));
               } else {
                  HReg r_src1h, r_src1l;
                  HReg r_src2h, r_src2l;
                  iselInt64Expr(&r_src1h, &r_src1l, env, e->Iex.Binop.arg1);
                  iselInt64Expr(&r_src2h, &r_src2l, env, e->Iex.Binop.arg2);
                  addInstr(env,
                           MIPSInstr_Msa2R(MSA_FILL, MSA_W, r_src2l, v_dst));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_INSERT, r_src2h, v_dst,
                                            MSA_DFN_W | 1));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_INSERT, r_src1l, v_dst,
                                            MSA_DFN_W | 2));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_INSERT, r_src1h, v_dst,
                                            MSA_DFN_W | 3));
               }

               return v_dst;
            }

         case Iop_Min32Fx4: {
               HReg v_src1  = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2  = iselV128Expr(env, e->Iex.Binop.arg2);
               HReg v_dst = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FMIN, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Min64Fx2: {
               HReg v_src1  = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2  = iselV128Expr(env, e->Iex.Binop.arg2);
               HReg v_dst = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FMIN, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max32Fx4: {
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               HReg v_dst  = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FMAX, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Max64Fx2: {
               HReg v_src1 = iselV128Expr(env, e->Iex.Binop.arg1);
               HReg v_src2 = iselV128Expr(env, e->Iex.Binop.arg2);
               HReg v_dst  = newVRegV(env);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FMAX, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               return v_dst;
            }

         case Iop_Sqrt32Fx4: {
               HReg v_src = iselV128Expr(env, e->Iex.Binop.arg2);
               HReg v_dst = newVRegV(env);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Binop.arg1);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FSQRT, MSA_F_WH, v_dst, v_src));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_Sqrt64Fx2: {
               HReg v_src = iselV128Expr(env, e->Iex.Binop.arg2);
               HReg v_dst = newVRegV(env);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Binop.arg1);
               addInstr(env,
                        MIPSInstr_Msa2RF(MSA_FSQRT, MSA_F_DW, v_dst, v_src));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         default:
            vex_printf("iselV128Expr_wrk: unsupported binop: %x\n", op_binop);
      }
   }

   if (e->tag == Iex_Triop) {
      IROp op_triop = e->Iex.Triop.details->op;

      switch (op_triop) {
         case Iop_Add32Fx4: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FADD, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_Add64Fx2: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FADD, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_Sub32Fx4: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FSUB, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_Sub64Fx2: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FSUB, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_Mul32Fx4: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FMUL, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_Mul64Fx2: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FMUL, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_Div32Fx4: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FDIV, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_Div64Fx2: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FDIV, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_F32x4_2toQ16x8: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FTQ, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         case Iop_F64x2_2toQ32x4: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FTQ, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

          case Iop_Scale2_32Fx4: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FEXP2, MSA_F_WH,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

          case Iop_Scale2_64Fx2: {
               HReg v_dst  = newVRegV(env);
               HReg v_src1 = iselV128Expr(env, e->Iex.Triop.details->arg2);
               HReg v_src2 = iselV128Expr(env, e->Iex.Triop.details->arg3);
               set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
               addInstr(env,
                        MIPSInstr_Msa3RF(MSA_FEXP2, MSA_F_DW,
                                         v_dst, v_src1, v_src2));
               set_MIPS_rounding_default_MSA(env);
               return v_dst;
            }

         default:
            vex_printf("iselV128Expr_wrk: unsupported triop: %x\n", op_triop);
      }
   }

   if (e->tag == Iex_Const) {
      IRConst *con = e->Iex.Const.con;

      if (con->tag != Ico_V128) {
         vpanic("iselV128Expr.const(mips)");
      } else {
         HReg v_dst = newVRegV(env);
         UShort val = con->Ico.V128;
         HReg zero = Zero(mode64);

         switch (val) {
            case 0:  /* likely */
               addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, zero, v_dst));
               break;

            default: {
                  HReg r_tmp = newVRegI(env);
                  UInt i;
                  addInstr(env, MIPSInstr_LI(r_tmp, 0xfful));

                  if (val & 1) {
                     addInstr(env,
                              MIPSInstr_Msa2R(MSA_FILL, MSA_B, r_tmp, v_dst));
                  } else {
                     addInstr(env,
                              MIPSInstr_Msa2R(MSA_FILL, MSA_B, zero, v_dst));
                  }

                  for (i = 1; i < 16; i++) {
                     val >>= 1;

                     if (val & 1) {
                        addInstr(env,
                                 MIPSInstr_MsaElm(MSA_INSERT, r_tmp, v_dst,
                                                  MSA_DFN_B | i));
                     } else {
                        addInstr(env,
                                 MIPSInstr_MsaElm(MSA_INSERT, zero, v_dst,
                                                  MSA_DFN_B | i));
                     }
                  }

                  break;
               }
         }

         return v_dst;
      }
   }

   if (e->tag == Iex_ITE) {
      HReg v_dst  = newVRegV(env);
      HReg iff    = iselV128Expr(env, e->Iex.ITE.iffalse);
      HReg ift    = iselV128Expr(env, e->Iex.ITE.iftrue);
      HReg r_cond = iselWordExpr_R(env, e->Iex.ITE.cond);
      addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, r_cond, r_cond,
                                   MIPSRH_Imm(False, 1)));
      addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, r_cond, v_dst));
      addInstr(env,
               MIPSInstr_Alu(Malu_ADD, r_cond, r_cond, MIPSRH_Imm(True, 1)));
      addInstr(env, MIPSInstr_MsaElm(MSA_INSERT, r_cond, v_dst, MSA_DFN_W | 2));
      addInstr(env, MIPSInstr_Msa3R(MSA_VSHF, MSA_D, v_dst, ift, iff));
      return v_dst;
   }

   vex_printf("iselV128Expr_wrk: Unsupported tag: %x\n", e->tag);
   ppIRExpr(e);
   vpanic("iselV128Expr(mips)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (128 bit)               ---*/
/*---------------------------------------------------------*/

/* 64-bit mode ONLY: compute a 128-bit value into a register pair,
   which is returned as the first two parameters.  As with
   iselWordExpr_R, these may be either real or virtual regs; in any
   case they must not be changed by subsequent code emitted by the
   caller.  */

static void iselInt128Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e)
{
   vassert(env->mode64);
   iselInt128Expr_wrk(rHi, rLo, env, e);
   vassert(hregClass(*rHi) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt128Expr_wrk(HReg * rHi, HReg * rLo, ISelEnv * env,
                               IRExpr * e)
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I128);

   /* read 128-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair(rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         /* 64 x 64 -> 128 multiply */
         case Iop_MullU64:
         case Iop_MullS64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            Bool syned = toBool(e->Iex.Binop.op == Iop_MullS64);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Mulr6(syned, False, True,
                                          tLo, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mulr6(syned, False, False,
                                          tHi, r_srcL, r_srcR));
#else
            addInstr(env, MIPSInstr_Mult(syned, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
#endif
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 64HLto128(e1,e2) */
         case Iop_64HLto128:
            *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
            *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2);
            return;

         case Iop_DivModU64to64:
         case Iop_DivModS64to64: {
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            Bool syned = toBool(e->Iex.Binop.op == Iop_DivModS64to64);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Divr6(syned/*Unsigned or Signed */ ,
                                          False /*32bit or 64bit div */ ,
                                          False /*mod*/,
                                          tLo, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Divr6(syned/*Unsigned or Signed */ ,
                                          False /*32bit or 64bit div */ ,
                                          True /*mod*/,
                                          tHi, r_srcL, r_srcR));
#else
            addInstr(env, MIPSInstr_Div(syned, False, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
#endif
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         default:
            break;
      }
   }
   vex_printf("iselInt128Expr(mips64): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselInt128Expr(mips64)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64 bit)                ---*/
/*---------------------------------------------------------*/

/* 32-bit mode ONLY. Compute a 64-bit value into the register
 * pair HI, LO. HI and LO must not be changed by subsequent
 *  code emitted by the caller. */

static void iselInt64Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e)
{
   vassert(!env->mode64);
   iselInt64Expr_wrk(rHi, rLo, env, e);
   vassert(hregClass(*rHi) == HRcInt32);
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcInt32);
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt64Expr_wrk(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e)
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I64);

   /* read 64-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTemp64(rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }
   /* 64-bit load */
   if (e->tag == Iex_Load) {
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      HReg r_addr = iselWordExpr_R(env, e->Iex.Load.addr);
      addInstr(env, MIPSInstr_Load(4, tHi, MIPSAMode_IR(0, r_addr), mode64));
      addInstr(env, MIPSInstr_Load(4, tLo, MIPSAMode_IR(4, r_addr), mode64));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit literal */
   if (e->tag == Iex_Const) {
      ULong w64 = e->Iex.Const.con->Ico.U64;
      UInt wHi = toUInt(w64 >> 32);
      UInt wLo = toUInt(w64);
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      vassert(e->Iex.Const.con->tag == Ico_U64);

      if (wLo == wHi) {
         /* Save a precious Int register in this special case. */
         addInstr(env, MIPSInstr_LI(tLo, (ULong) wLo));
         *rHi = tLo;
         *rLo = tLo;
      } else {
         addInstr(env, MIPSInstr_LI(tHi, (ULong) wHi));
         addInstr(env, MIPSInstr_LI(tLo, (ULong) wLo));
         *rHi = tHi;
         *rLo = tLo;
      }

      return;
   }

   /* 64-bit GET */
   if (e->tag == Iex_Get) {
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);

      MIPSAMode *am_addr = MIPSAMode_IR(e->Iex.Get.offset,
                                        GuestStatePointer(mode64));
      addInstr(env, MIPSInstr_Load(4, tLo, am_addr, mode64));
      addInstr(env, MIPSInstr_Load(4, tHi, nextMIPSAModeInt(am_addr), mode64));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit ITE */
   if (e->tag == Iex_ITE) {
      vassert(typeOfIRExpr(env->type_env, e->Iex.ITE.cond) == Ity_I1);
      HReg expr0Lo, expr0Hi;
      HReg expr1Lo, expr1Hi;
      HReg desLo  = newVRegI(env);
      HReg desHi  = newVRegI(env);
      HReg cond = iselWordExpr_R(env, e->Iex.ITE.cond);

      /* expr0Hi:expr0Lo = iffalse */
      /* expr1Hi:expr1Lo = iftrue */
      iselInt64Expr(&expr0Hi, &expr0Lo, env, e->Iex.ITE.iffalse);
      iselInt64Expr(&expr1Hi, &expr1Lo, env, e->Iex.ITE.iftrue);

      /* move desLo, expr0Lo
       * move desHi, expr0Hi
       * movn desLo, expr1Lo, cond
       * movn desHi, expr1Hi, cond */
#if (__mips_isa_rev >= 6)
      {
        HReg r_temp = newVRegI(env);
        addInstr(env, MIPSInstr_MoveCond(MSeleqz, desLo, expr0Lo, cond));
        addInstr(env, MIPSInstr_MoveCond(MSelnez, r_temp, expr1Lo, cond));
        addInstr(env, MIPSInstr_Alu(Malu_OR, desLo, desLo, MIPSRH_Reg(r_temp)));

        addInstr(env, MIPSInstr_MoveCond(MSeleqz, desHi, expr0Hi, cond));
        addInstr(env, MIPSInstr_MoveCond(MSelnez, r_temp, expr1Hi, cond));
        addInstr(env, MIPSInstr_Alu(Malu_OR, desHi, desHi, MIPSRH_Reg(r_temp)));
      }
#else
      addInstr(env, mk_iMOVds_RR(desLo, expr0Lo));
      addInstr(env, mk_iMOVds_RR(desHi, expr0Hi));
      addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, desLo, expr1Lo, cond));
      addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, desHi, expr1Hi, cond));
#endif

      *rHi = desHi;
      *rLo = desLo;
      return;
   }

   if (e->tag == Iex_CCall) {
      HReg r_dstH = newVRegI(env);
      HReg r_dstL = newVRegI(env);
      vassert(e->Iex.CCall.retty == Ity_I64);

      /* Marshal args, do the call, clear stack. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      doHelperCall(&addToSp, &rloc, env, NULL/*guard*/, e->Iex.CCall.cee,
                   e->Iex.CCall.retty, e->Iex.CCall.args );

      vassert(is_sane_RetLoc(rloc));
      vassert(rloc.pri == RLPri_2Int);
      vassert(addToSp == 0);
      addInstr(env, mk_iMOVds_RR(r_dstL, hregMIPS_GPR2(False)));
      addInstr(env, mk_iMOVds_RR(r_dstH, hregMIPS_GPR3(False)));
      *rHi = r_dstH;
      *rLo = r_dstL;
      return;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      IROp op_binop = e->Iex.Binop.op;
      switch (op_binop) {
         /* 32 x 32 -> 64 multiply */
         /* Add64 */
         case Iop_Add64: {
            HReg xLo, xHi, yLo, yHi, carryBit;

            HReg tHi = newVRegI(env);
            HReg tHi1 = newVRegI(env);
            HReg tLo = newVRegI(env);

            carryBit = newVRegI(env);

            Bool size32 = True;
            MIPSCondCode cc = MIPScc_LO;

            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, MIPSInstr_Alu(Malu_ADD, tLo, xLo, MIPSRH_Reg(yLo)));

            /* Check carry. */
            addInstr(env, MIPSInstr_Cmp(False, size32, carryBit, tLo, xLo, cc));

            addInstr(env, MIPSInstr_Alu(Malu_ADD, tHi1, xHi, MIPSRH_Reg(yHi)));
            addInstr(env, MIPSInstr_Alu(Malu_ADD, tHi, tHi1,
                                        MIPSRH_Reg(carryBit)));

            *rHi = tHi;
            *rLo = tLo;
            return;
         }
         case Iop_Sub64: {
            HReg xLo, xHi, yLo, yHi, borrow;
            Bool size32 = True;
            MIPSCondCode cc = MIPScc_LO;

            HReg tHi = newVRegI(env);
            HReg tLo = newVRegI(env);

            borrow = newVRegI(env);

            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);

            addInstr(env, MIPSInstr_Alu(Malu_SUB, tLo, xLo, MIPSRH_Reg(yLo)));

            /* Check if borrow is nedded. */
            addInstr(env, MIPSInstr_Cmp(False, size32, borrow, xLo, yLo, cc));

            addInstr(env, MIPSInstr_Alu(Malu_ADD, yHi, yHi,
                                        MIPSRH_Reg(borrow)));
            addInstr(env, MIPSInstr_Alu(Malu_SUB, tHi, xHi, MIPSRH_Reg(yHi)));

            *rHi = tHi;
            *rLo = tLo;
            return;
         }
         case Iop_MullU32:
         case Iop_MullS32: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            Bool syned = toBool(op_binop == Iop_MullS32);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Mulr6(syned, True, True,
                                          tLo, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mulr6(syned, True, False,
                                          tHi, r_srcL, r_srcR));
#else
            addInstr(env, MIPSInstr_Mult(syned, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
#endif
            *rHi = tHi;
            *rLo = tLo;

            return;
         }

         case Iop_DivModU32to32:
         case Iop_DivModS32to32: {
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            Bool syned = toBool(e->Iex.Binop.op == Iop_DivModS32to32);

#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Divr6(syned /*Unsigned or Signed */ ,
                                          True /*32bit or 64bit div */ ,
                                          False /*mod*/,
                                          tLo, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Divr6(syned /*Unsigned or Signed */ ,
                                          True /*32bit or 64bit div */ ,
                                          True /*mod*/,
                                          tHi, r_srcL, r_srcR));
#else
            addInstr(env, MIPSInstr_Div(syned, True, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
#endif
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32HLto64(e1,e2) */
         case Iop_32HLto64:
            *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
            *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2);

            return;
         /* Or64/And64/Xor64 */
         case Iop_Or64:
         case Iop_And64:
         case Iop_Xor64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            MIPSAluOp op = (op_binop == Iop_Or64) ? Malu_OR :
                           (op_binop == Iop_And64) ? Malu_AND : Malu_XOR;
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, MIPSInstr_Alu(op, tHi, xHi, MIPSRH_Reg(yHi)));
            addInstr(env, MIPSInstr_Alu(op, tLo, xLo, MIPSRH_Reg(yLo)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Shr64: {
            /* 64-bit logical shift right based on what gcc generates:
               <shift>:
               nor  v0, zero, a2
               sll  a3, a1, 0x1
               sllv a3, a3, v0
               srlv v0, a0, a2
               srlv v1, a1, a2
               andi a0, a2, 0x20
               or   v0, a3, v0
               movn v0, v1, a0
               jr   ra
               movn v1, zero, a0
            */
            HReg r_srcLo, r_srcHi;
            HReg r_srcLotmp = newVRegI(env);
            HReg shift = newVRegI(env);
            HReg a3 = newVRegI(env);
            HReg r_dstLo = newVRegI(env);
            HReg r_dstHi = newVRegI(env);
            HReg zero = hregMIPS_GPR0(env->mode64);
            MIPSRH *sa = NULL;

            iselInt64Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg1);
            sa = iselWordExpr_RH6u(env, e->Iex.Binop.arg2);

            if (sa->tag == Mrh_Imm) {
               addInstr(env, MIPSInstr_LI(shift, sa->Mrh.Imm.imm16));
            }
            else {
               addInstr(env, MIPSInstr_Alu(Malu_AND, shift, sa->Mrh.Reg.reg,
                                           MIPSRH_Imm(False, 0x3f)));
            }
            /* nor  r_dstLo, zero, shift */
            addInstr(env, MIPSInstr_Alu(Malu_NOR, r_dstLo, zero, MIPSRH_Reg(shift)));
            /* sll  a3, r_srcHi, 0x1 */
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True /* 32bit shift */,
                                         a3, r_srcHi, MIPSRH_Imm(False, 0x1)));
            /* sllv a3, a3, r_dstLo */
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True /* 32bit shift */,
                                         a3, a3, MIPSRH_Reg(r_dstLo)));
            /* srlv r_dstLo, r_srcLo, shift */
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /* 32bit shift */,
                                         r_dstLo, r_srcLo, MIPSRH_Reg(shift)));
            /* srlv r_dstHi, r_srcHi, shift */
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /* 32bit shift */,
                                         r_dstHi, r_srcHi, MIPSRH_Reg(shift)));
            /* andi r_srcLo, shift, 0x20 */
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_srcLotmp, shift,
                                        MIPSRH_Imm(False, 0x20)));
            /* or   r_dstLo, a3, r_dstLo */
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dstLo, a3, MIPSRH_Reg(r_dstLo)));
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_MoveCond(MSeleqz, r_dstLo, r_dstLo, r_srcLotmp));
            addInstr(env, MIPSInstr_MoveCond(MSelnez, a3, r_dstHi, r_srcLotmp));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dstLo, r_dstLo, MIPSRH_Reg(a3)));

            addInstr(env, MIPSInstr_MoveCond(MSeleqz, r_dstHi, r_dstHi, r_srcLotmp));
#else
            /* movn    r_dstLo, r_dstHi, r_srcLo */
            addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, r_dstLo, r_dstHi, r_srcLotmp));
            /* movn    r_dstHi, zero, r_srcLo */
            addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, r_dstHi, zero, r_srcLotmp));
#endif
            *rHi = r_dstHi;
            *rLo = r_dstLo;
            return;
         }

         case Iop_Shl64: {
            /* 64-bit shift left based on what gcc generates:
               <shift>:
               nor  v0,zero,a2
               srl  a3,a0,0x1
               srlv a3,a3,v0
               sllv v1,a1,a2
               andi v0,a2,0x20
               or   v1,a3,v1
               sllv a2,a0,a2
               movn v1,a2,v0
               movn a2,zero,v0
               jr   ra
               move v0,a2
            */
            HReg r_srcLo, r_srcHi;
            HReg r_shift = newVRegI(env);
            HReg a3 = newVRegI(env);
            HReg r_dstLo = newVRegI(env);
            HReg r_dstHi = newVRegI(env);
            HReg zero = hregMIPS_GPR0(env->mode64);
            MIPSRH *sa = NULL;

            iselInt64Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg1);
            sa = iselWordExpr_RH6u(env, e->Iex.Binop.arg2);

            if (sa->tag == Mrh_Imm) {
               addInstr(env, MIPSInstr_LI(r_shift, sa->Mrh.Imm.imm16));
            }
            else {
               addInstr(env, MIPSInstr_Alu(Malu_AND, r_shift, sa->Mrh.Reg.reg,
                                           MIPSRH_Imm(False, 0x3f)));
            }
            /* nor r_dstLo, zero, r_shift */
            addInstr(env, MIPSInstr_Alu(Malu_NOR, r_dstLo, zero, MIPSRH_Reg(r_shift)));
            /* srl a3, r_srcLo, 0x1 */
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /* 32bit shift */,
                                         a3, r_srcLo, MIPSRH_Imm(False, 0x1)));
            /* srlv a3, a3, r_dstLo */
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /* 32bit shift */,
                                         a3, a3, MIPSRH_Reg(r_dstLo)));
            /* sllv r_dstHi, r_srcHi, r_shift */
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True /* 32bit shift */,
                                         r_dstHi, r_srcHi, MIPSRH_Reg(r_shift)));
            /* or r_dstHi, a3, r_dstHi */
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dstHi, a3, MIPSRH_Reg(r_dstHi)));
            /* andi a3, r_shift, 0x20 */
            addInstr(env, MIPSInstr_Alu(Malu_AND, a3, r_shift,
                                        MIPSRH_Imm(False, 0x20)));
            /* sllv r_dstLo, r_srcLo, r_shift */
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True /* 32bit shift */,
                                         r_dstLo, r_srcLo, MIPSRH_Reg(r_shift)));
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_MoveCond(MSeleqz, r_dstHi, r_dstHi, a3));
            addInstr(env, MIPSInstr_MoveCond(MSelnez, r_shift, r_dstLo, a3));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dstHi, r_dstHi, MIPSRH_Reg(r_shift)));

            addInstr(env, MIPSInstr_MoveCond(MSeleqz, r_dstLo, r_dstLo, a3));
#else
            /* movn r_dstHi, r_dstLo, a3 */
            addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, r_dstHi, r_dstLo, a3));
            /* movn r_dstLo, zero, a3 */
            addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, r_dstLo, zero, a3));
#endif
            *rHi = r_dstHi;
            *rLo = r_dstLo;
            return;
         }

         case Iop_Sar64: {
            /* 64-bit arithmetic shift right based on what gcc generates:
               <shift>:
               nor  v0, zero, a2
               sll  a3, a1, 0x1
               sllv a3, a3, v0
               srlv v0, a0, a2
               srav v1, a1, a2
               andi a0, a2, 0x20
               sra  a1, a1, 0x1f
               or   v0, a3, v0
               movn v0, v1, a0
               jr   ra
               movn v1, a1, a0
            */
            HReg r_srcHi, r_srcLo;
            HReg r_srcHitmp = newVRegI(env);
            HReg r_srcLotmp = newVRegI(env);
            HReg r_shift = newVRegI(env);
            HReg a3 = newVRegI(env);
            HReg r_dstLo = newVRegI(env);
            HReg r_dstHi = newVRegI(env);
            HReg zero = hregMIPS_GPR0(env->mode64);
            MIPSRH *sa = NULL;

            iselInt64Expr(&r_srcLo, &r_srcHi, env, e->Iex.Binop.arg1);
            sa = iselWordExpr_RH6u(env, e->Iex.Binop.arg2);

            if (sa->tag == Mrh_Imm) {
               addInstr(env, MIPSInstr_LI(r_shift, sa->Mrh.Imm.imm16));
            }
            else {
               addInstr(env, MIPSInstr_Alu(Malu_AND, r_shift, sa->Mrh.Reg.reg,
                                           MIPSRH_Imm(False, 0x3f)));
            }
            /* nor  r_dstLo, zero, r_shift */
            addInstr(env, MIPSInstr_Alu(Malu_NOR, r_dstLo, zero, MIPSRH_Reg(r_shift)));
            /* sll  a3, r_srcLo, 0x1 */
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True /* 32bit shift */,
                                         a3, r_srcLo, MIPSRH_Imm(False, 0x1)));
            /* sllv a3, a3, r_dstLo */
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True /* 32bit shift */,
                                         a3, a3, MIPSRH_Reg(r_dstLo)));
            /* srlv r_dstLo, r_srcHi, r_shift */
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True /* 32bit shift */,
                                         r_dstLo, r_srcHi, MIPSRH_Reg(r_shift)));
            /* srav r_dstHi, r_srcLo, r_shift */
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True /* 32bit shift */,
                                         r_dstHi, r_srcLo, MIPSRH_Reg(r_shift)));
            /* andi r_srcHi, r_shift, 0x20 */
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_srcHitmp, r_shift,
                                        MIPSRH_Imm(False, 0x20)));
            /* sra r_srcLo, r_srcLo, 0x1f */
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True /* 32bit shift */,
                                         r_srcLotmp, r_srcLo, MIPSRH_Imm(False, 0x1f)));
            /* or   r_dstLo, a3, r_dstLo */
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dstLo, a3, MIPSRH_Reg(r_dstLo)));
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_MoveCond(MSeleqz, r_dstLo, r_dstLo, r_srcHitmp));
            addInstr(env, MIPSInstr_MoveCond(MSelnez, a3, r_dstHi, r_srcHitmp));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dstLo, r_dstLo, MIPSRH_Reg(a3)));

            addInstr(env, MIPSInstr_MoveCond(MSeleqz, r_dstHi, r_dstHi, r_srcHitmp));
            addInstr(env, MIPSInstr_MoveCond(MSelnez, a3, r_srcLotmp, r_srcHitmp));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dstHi, r_dstHi, MIPSRH_Reg(a3)));
#else
            /* movn    r_dstLo, r_dstHi, r_srcHi */
            addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, r_dstLo, r_dstHi, r_srcHitmp));
            /* movn    r_dstHi, r_srcLo, r_srcHi */
            addInstr(env, MIPSInstr_MoveCond(MMoveCond_movn, r_dstHi, r_srcLotmp, r_srcHitmp));
#endif
            *rHi = r_dstHi;
            *rLo = r_dstLo;
            return;
         }

         case Iop_F32toI64S: {
            HReg tmpD = newVRegD(env);
            HReg valF = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg tLo  = newVRegI(env);
            HReg tHi  = newVRegI(env);
            MIPSAMode *am_addr;

            /* CVTLS tmpD, valF */
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTLS, tmpD, valF));
            set_MIPS_rounding_default(env);

            sub_from_sp(env, 16);  /* Move SP down 16 bytes */
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            /* store as F64 */
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, tmpD,
                                           am_addr));
            /* load as 2xI32 */
#if defined (_MIPSEL)
            addInstr(env, MIPSInstr_Load(4, tLo, am_addr, mode64));
            addInstr(env, MIPSInstr_Load(4, tHi, nextMIPSAModeFloat(am_addr),
                                         mode64));
#elif defined (_MIPSEB)
            addInstr(env, MIPSInstr_Load(4, tHi, am_addr, mode64));
            addInstr(env, MIPSInstr_Load(4, tLo, nextMIPSAModeFloat(am_addr),
                                         mode64));
#endif

            /* Reset SP */
            add_to_sp(env, 16);

            *rHi = tHi;
            *rLo = tLo;

            return;
         }
         case Iop_F64toI64U: {
            HReg r_src;
            HReg tmp = newVRegV(env);
            vassert(has_msa);
            r_src = iselDblExpr( env, e->Iex.Binop.arg2);
            set_MIPS_rounding_mode_MSA(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_Msa2RF(MSA_FTINT_U, MSA_F_DW, tmp, r_src));
            HReg r_dsth = newVRegI(env);
            HReg r_dstl = newVRegI(env);
            addInstr(env,
                     MIPSInstr_MsaElm(MSA_COPY_S, tmp, r_dstl, MSA_DFN_W | 0));
            addInstr(env,
                     MIPSInstr_MsaElm(MSA_COPY_S, tmp, r_dsth, MSA_DFN_W | 1));
            *rHi = r_dsth;
            *rLo = r_dstl;
            set_MIPS_rounding_default_MSA(env);
            return;
         }

         case Iop_GetElem64x2: {
            vassert(has_msa);
            HReg v_src = iselV128Expr(env, e->Iex.Binop.arg1);
            HReg r_dstHI = newVRegI(env);
            HReg r_dstLO = newVRegI(env);
            MIPSRH *tmp = iselWordExpr_RH(env, False, e->Iex.Binop.arg2);

            switch (tmp->tag) {
               case Mrh_Imm:
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_COPY_S, v_src, r_dstHI,
                                            MSA_DFN_W |
                                            (((tmp->Mrh.Imm.imm16 & 0x01) << 1)
                                            + 1)));
                  addInstr(env,
                           MIPSInstr_MsaElm(MSA_COPY_S, v_src, r_dstLO,
                                            MSA_DFN_W |
                                            ((tmp->Mrh.Imm.imm16 & 0x01) << 1)));
                  break;

               case Mrh_Reg: {
                     HReg v_tmp = newVRegV(env);
                     addInstr(env,
                              MIPSInstr_Msa3R(MSA_SPLAT, MSA_D, v_tmp, v_src,
                                              tmp->Mrh.Reg.reg));
                     addInstr(env,
                              MIPSInstr_MsaElm(MSA_COPY_S, v_tmp, r_dstHI,
                                               MSA_DFN_W | 1));
                     addInstr(env,
                              MIPSInstr_MsaElm(MSA_COPY_S, v_tmp, r_dstLO,
                                               MSA_DFN_W));
                     break;
                  }
            }

            *rHi = r_dstHI;
            *rLo = r_dstLO;
            return;
         }

         case Iop_Mul64: {
            HReg a_L, a_H, b_L, b_H;
            HReg dst_L = newVRegI(env);
            HReg dst_H = newVRegI(env);

            iselInt64Expr(&a_H, &a_L, env, e->Iex.Binop.arg1);
            iselInt64Expr(&b_H, &b_L, env, e->Iex.Binop.arg2);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_Mulr6(True, True, True,
                                          dst_H, a_H, b_L));
            addInstr(env, MIPSInstr_Mulr6(True, True, True,
                                          dst_L, b_H, a_L));
            addInstr(env, MIPSInstr_Alu(Malu_ADD, dst_H, dst_H,
                                        MIPSRH_Reg(dst_L)));
            addInstr(env, MIPSInstr_Mulr6(False, True, False,
                                          dst_L, a_L, b_L));

            addInstr(env, MIPSInstr_Alu(Malu_ADD, dst_H, dst_H,
                                        MIPSRH_Reg(dst_L)));
            addInstr(env, MIPSInstr_Mulr6(False, True, True,
                                          dst_L, a_L, b_L));
#else
            addInstr(env, MIPSInstr_Mul(dst_H, a_H, b_L));
            addInstr(env, MIPSInstr_Mult(True, b_H, a_L));
            addInstr(env, MIPSInstr_Mflo(dst_L));
            addInstr(env, MIPSInstr_Alu(Malu_ADD, dst_H, dst_H,
                                        MIPSRH_Reg(dst_L)));
            addInstr(env, MIPSInstr_Mult(False, a_L, b_L));
            addInstr(env, MIPSInstr_Mfhi(dst_L));

            addInstr(env, MIPSInstr_Alu(Malu_ADD, dst_H, dst_H,
                                        MIPSRH_Reg(dst_L)));
            addInstr(env, MIPSInstr_Mflo(dst_L));
#endif
            *rHi = dst_H;
            *rLo = dst_L;
            return;
         }

         case Iop_DivS64: {
            HReg src1_L, src1_H, src2_L, src2_H;
            HReg dst_L = newVRegI(env);
            HReg dst_H = newVRegI(env);
            HReg tmp1 = newVRegV(env);
            HReg tmp2 = newVRegV(env);
            vassert(has_msa);
            iselInt64Expr(&src1_H, &src1_L, env, e->Iex.Binop.arg1);
            iselInt64Expr(&src2_H, &src2_L, env, e->Iex.Binop.arg2);
            addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, src1_L, tmp1));
            addInstr(env, MIPSInstr_MsaElm(MSA_INSERT, src1_H, tmp1, MSA_DFN_W | 1));
            addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, src2_L, tmp2));
            addInstr(env, MIPSInstr_MsaElm(MSA_INSERT, src2_H, tmp2, MSA_DFN_W | 1));
            addInstr(env, MIPSInstr_Msa3R(MSA_DIVS, MSA_D, tmp1, tmp1, tmp2));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, tmp1, dst_H, MSA_DFN_W | 1));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, tmp1, dst_L, MSA_DFN_W | 0));
            *rHi = dst_H;
            *rLo = dst_L;
            return;
         }

         case Iop_DivU64: {
            HReg src1_L, src1_H, src2_L, src2_H;
            HReg dst_L = newVRegI(env);
            HReg dst_H = newVRegI(env);
            HReg tmp1 = newVRegV(env);
            HReg tmp2 = newVRegV(env);
            vassert(has_msa);
            iselInt64Expr(&src1_H, &src1_L, env, e->Iex.Binop.arg1);
            iselInt64Expr(&src2_H, &src2_L, env, e->Iex.Binop.arg2);
            addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, src1_L, tmp1));
            addInstr(env, MIPSInstr_MsaElm(MSA_INSERT, src1_H, tmp1, MSA_DFN_W | 1));
            addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, src2_L, tmp2));
            addInstr(env, MIPSInstr_MsaElm(MSA_INSERT, src2_H, tmp2, MSA_DFN_W | 1));
            addInstr(env, MIPSInstr_Msa3R(MSA_DIVU, MSA_D, tmp1, tmp1, tmp2));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, tmp1, dst_H, MSA_DFN_W | 1));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, tmp1, dst_L, MSA_DFN_W | 0));
            *rHi = dst_H;
            *rLo = dst_L;
            return;
         }

         case Iop_F64toI64S: {
            HReg tmpD = newVRegD(env);
            HReg valF;
            HReg tLo  = newVRegI(env);
            HReg tHi  = newVRegI(env);
            MIPSAMode *am_addr;

            if(mode64){
              valF = iselFltExpr(env, e->Iex.Binop.arg2);
            } else {
              valF = iselDblExpr(env, e->Iex.Binop.arg2);
            }

            /* CVTLS tmpD, valF */
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTLD, tmpD, valF));
            set_MIPS_rounding_default(env);

            sub_from_sp(env, 16);  /* Move SP down 16 bytes */
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            /* store as F64 */
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, tmpD,
                                           am_addr));
            /* load as 2xI32 */
#if defined (_MIPSEL)
            addInstr(env, MIPSInstr_Load(4, tLo, am_addr, mode64));
            addInstr(env, MIPSInstr_Load(4, tHi, nextMIPSAModeFloat(am_addr),
                                         mode64));
#elif defined (_MIPSEB)
            addInstr(env, MIPSInstr_Load(4, tHi, am_addr, mode64));
            addInstr(env, MIPSInstr_Load(4, tLo, nextMIPSAModeFloat(am_addr),
                                         mode64));
#endif

            /* Reset SP */
            add_to_sp(env, 16);

            *rHi = tHi;
            *rLo = tLo;

            return;
         }

         default:
            break;
      }
   }

   /* --------- UNARY ops --------- */
   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
         case Iop_1Sto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
            HReg tmp = newVRegI(env);

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp, src,
                          MIPSRH_Imm(False, 31)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tmp, tmp,
                          MIPSRH_Imm(False, 31)));

            addInstr(env, mk_iMOVds_RR(tHi, tmp));
            addInstr(env, mk_iMOVds_RR(tLo, tmp));

            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_8Sto64:
         case Iop_16Sto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
            UInt no_bits = (e->Iex.Unop.op == Iop_8Sto64) ? 24 : 16;
            addInstr(env, mk_iMOVds_RR(tLo, src));
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tLo, tLo,
                          MIPSRH_Imm(False, no_bits)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tHi, tLo,
                          MIPSRH_Imm(False, 31)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tLo, tLo,
                          MIPSRH_Imm(False, no_bits)));
            addInstr(env, mk_iMOVds_RR(tHi, tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32Sto64(e) */
         case Iop_32Sto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVds_RR(tHi, src));
            addInstr(env, mk_iMOVds_RR(tLo, src));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tHi, tHi,
                          MIPSRH_Imm(False, 31)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_8Uto64:
         case Iop_16Uto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
            UInt mask = (e->Iex.Unop.op == Iop_8Sto64) ? 0xFF : 0xFFFF;
            addInstr(env, MIPSInstr_Alu(Malu_AND, tLo, src,
                                        MIPSRH_Imm(False, mask)));
            addInstr(env, MIPSInstr_Alu(Malu_ADD, tHi, hregMIPS_GPR0(mode64),
                                        MIPSRH_Reg(hregMIPS_GPR0(mode64))));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32Uto64(e) */
         case Iop_32Uto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVds_RR(tLo, src));
            addInstr(env, MIPSInstr_Alu(Malu_ADD, tHi, hregMIPS_GPR0(mode64),
                          MIPSRH_Reg(hregMIPS_GPR0(mode64))));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Left64: {
            HReg yHi, yLo;
            HReg tHi  = newVRegI(env);
            HReg tLo  = newVRegI(env);
            HReg tmp  = newVRegI(env);
            HReg tmp1  = newVRegI(env);
            HReg tmp2  = newVRegI(env);
            HReg zero = newVRegI(env);
            MIPSCondCode cc = MIPScc_LO;

            /* yHi:yLo = arg */
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Unop.arg);
            /* zero = 0 */
            addInstr(env, MIPSInstr_LI(zero, 0x00000000));

            /* tmp2:tmp1 = 0 - (yHi:yLo)*/
            addInstr(env, MIPSInstr_Alu(Malu_SUB, tmp2, zero, MIPSRH_Reg(yLo)));
            addInstr(env, MIPSInstr_Cmp(False, True, tmp1, zero, tmp2, cc));
            addInstr(env, MIPSInstr_Alu(Malu_SUB, tmp, zero, MIPSRH_Reg(yHi)));
            addInstr(env, MIPSInstr_Alu(Malu_SUB, tmp1, tmp, MIPSRH_Reg(tmp1)));

            /* So now we have tmp2:tmp1 = -arg.  To finish off, or 'arg'
               back in, so as to give the final result
               tHi:tLo = arg | -arg. */
            addInstr(env, MIPSInstr_Alu(Malu_OR, tHi, yHi, MIPSRH_Reg(tmp1)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, tLo, yLo, MIPSRH_Reg(tmp2)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_CmpwNEZ64: {
            HReg srcLo, srcHi;
            HReg tmp1 = newVRegI(env);
            HReg tmp2 = newVRegI(env);
            /* srcHi:srcLo = arg */
            iselInt64Expr(&srcHi, &srcLo, env, e->Iex.Unop.arg);
            /* tmp1 = srcHi | srcLo */
            addInstr(env, MIPSInstr_Alu(Malu_OR, tmp1, srcLo,
                                        MIPSRH_Reg(srcHi)));
            /* tmp2 = (tmp1 | -tmp1) >>s 31 */

            addInstr(env, MIPSInstr_Alu(Malu_SUB, tmp2, hregMIPS_GPR0(mode64),
                                        MIPSRH_Reg(tmp1)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, tmp2, tmp2, MIPSRH_Reg(tmp1)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tmp2, tmp2,
                          MIPSRH_Imm(False, 31)));
            *rHi = tmp2;
            *rLo = tmp2;
            return;

         }
         case Iop_ReinterpF64asI64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            MIPSAMode *am_addr;
            HReg fr_src = iselDblExpr(env, e->Iex.Unop.arg);

            sub_from_sp(env, 16);  /* Move SP down 16 bytes */
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            /* store as F64 */
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, fr_src,
                                           am_addr));
            /* load as 2xI32 */
#if defined (_MIPSEL)
            addInstr(env, MIPSInstr_Load(4, tLo, am_addr, mode64));
            addInstr(env, MIPSInstr_Load(4, tHi, nextMIPSAModeFloat(am_addr),
                                         mode64));
#elif defined (_MIPSEB)
            addInstr(env, MIPSInstr_Load(4, tHi, am_addr, mode64));
            addInstr(env, MIPSInstr_Load(4, tLo, nextMIPSAModeFloat(am_addr),
                                         mode64));
#endif

            /* Reset SP */
            add_to_sp(env, 16);

            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Not64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            iselInt64Expr(&tHi, &tLo, env, e->Iex.Unop.arg);
            addInstr(env, MIPSInstr_Alu(Malu_NOR, tLo, tLo, MIPSRH_Reg(tLo)));
            addInstr(env, MIPSInstr_Alu(Malu_NOR, tHi, tHi, MIPSRH_Reg(tHi)));

            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_V128HIto64: {
            vassert(has_msa);
            HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, v_src, tLo, MSA_DFN_W | 2));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, v_src, tHi, MSA_DFN_W | 3));
            *rLo = tLo;
            *rHi = tHi;
            return;
         }

         case Iop_V128to64: {
            vassert(has_msa);
            HReg v_src = iselV128Expr(env, e->Iex.Unop.arg);
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, v_src, tLo, MSA_DFN_W | 0));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, v_src, tHi, MSA_DFN_W | 1));
            *rLo = tLo;
            *rHi = tHi;
            return;
         }

         case Iop_F32toF16x4_DEP: {
            vassert(has_msa);
            HReg v_arg = iselV128Expr(env, e->Iex.Unop.arg);
            HReg v_src = newVRegV(env);
            set_guest_MIPS_rounding_mode_MSA(env);
            addInstr(env, MIPSInstr_Msa3RF(MSA_FEXDO, MSA_F_WH, v_src, v_arg, v_arg));
            set_MIPS_rounding_default_MSA(env);
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, v_src, tLo, MSA_DFN_W | 0));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, v_src, tHi, MSA_DFN_W | 1));
            *rLo = tLo;
            *rHi = tHi;
            return;
         }

         default:
            vex_printf("UNARY: No such op: ");
            ppIROp(e->Iex.Unop.op);
            vex_printf("\n");
            break;
      }
   }

   vex_printf("iselInt64Expr(mips): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselInt64Expr(mips)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (32 bit)         ---*/
/*---------------------------------------------------------*/

/* Nothing interesting here; really just wrappers for
   64-bit stuff. */
static HReg iselFltExpr(ISelEnv * env, IRExpr * e)
{
    HReg r;
    IRType ty = typeOfIRExpr(env->type_env, e);
    if (ty == Ity_F32 || (ty == Ity_F64 && fp_mode64)) {
      r = iselFltExpr_wrk(env, e);
    } else {
      r = iselDblExpr_wrk(env, e);
      vassert(hregClass(r) == HRcFlt64);
    }
    return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselFltExpr_wrk(ISelEnv * env, IRExpr * e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_F32 || (ty == Ity_F64 && fp_mode64));

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Load) {
      vassert(e->Iex.Load.ty == Ity_F32
              || (e->Iex.Load.ty == Ity_F64 && fp_mode64));
      HReg r_dst;
      MIPSAMode *am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, ty);
      if (e->Iex.Load.ty == Ity_F64) {
         r_dst = newVRegD(env);
         addInstr(env, MIPSInstr_FpLdSt(True /*load */, 8, r_dst, am_addr));
      } else {
         r_dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpLdSt(True /*load */, 4, r_dst, am_addr));
      }
      return r_dst;
   }

   if (e->tag == Iex_Get) {
      MIPSAMode *am_addr = MIPSAMode_IR(e->Iex.Get.offset,
                                        GuestStatePointer(mode64));
      HReg r_dst;
      if (e->Iex.Load.ty == Ity_F64) {
         r_dst = newVRegD(env);
         addInstr(env, MIPSInstr_FpLdSt(True /*load */, 8, r_dst, am_addr));
      } else {
         r_dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpLdSt(True /*load */, 4, r_dst, am_addr));
      }
      return r_dst;
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
      case Iop_ReinterpI32asF32: {
         HReg fr_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         HReg r_dst = newVRegF(env);

         /* Move Word to Floating Point
            mtc1 r_dst, valS */
         addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mtc1, r_dst, fr_src));

         return r_dst;
      }
      case Iop_F32toF64: {
         vassert(fp_mode64);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegD(env);

         addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDS, dst, src));
         return dst;
      }
      case Iop_ReinterpI64asF64: {
         HReg r_dst;
         if (mode64) {
            HReg fr_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            r_dst = newVRegF(env);
            /* Move Doubleword to Floating Point
               dmtc1 r_dst, fr_src */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_dmtc1, r_dst, fr_src));
         } else {
             HReg Hi, Lo;
             r_dst = newVRegD(env);
             iselInt64Expr(&Hi, &Lo, env, e->Iex.Unop.arg);
             r_dst = mk_LoadRR32toFPR(env, Hi, Lo);  /* 2*I32 -> F64 */
         }
         return r_dst;
      }
      case Iop_I32StoF64: {
         vassert(fp_mode64);
         HReg dst = newVRegF(env);
         HReg tmp = newVRegF(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

         /* Move Word to Floating Point
            mtc1 tmp, r_src */
         addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mtc1, tmp, r_src));

         /* and do convert */
         addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDW, dst, tmp));

         return dst;
      }
      case Iop_AbsF32:
      case Iop_AbsF64: {
         Bool sz32 = e->Iex.Unop.op == Iop_AbsF32;
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpUnary(sz32 ? Mfp_ABSS : Mfp_ABSD, dst, src));
         return dst;
      }
      case Iop_NegF32:
      case Iop_NegF64: {
         Bool sz32 = e->Iex.Unop.op == Iop_NegF32;
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpUnary(sz32 ? Mfp_NEGS : Mfp_NEGD, dst, src));
         return dst;
      }
      case Iop_RoundF64toF64_ZERO: {
         vassert(mode64);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpConvert(Mfp_TRULD, dst, src));
         return dst;
      }
      case Iop_RoundF64toF64_NEAREST: {
         vassert(mode64);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpConvert(Mfp_ROUNDLD, dst, src));
         return dst;
      }
      case Iop_RoundF64toF64_NegINF: {
         vassert(mode64);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpConvert(Mfp_FLOORLD, dst, src));
         return dst;
      }
      case Iop_RoundF64toF64_PosINF: {
         vassert(mode64);
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpConvert(Mfp_CEILLD, dst, src));
         return dst;
      }

      default:
         break;
      }
   }

   if (e->tag == Iex_Triop) {
      switch (e->Iex.Triop.details->op) {
         case Iop_DivF32:
         case Iop_DivF64:
         case Iop_MulF32:
         case Iop_MulF64:
         case Iop_AddF32:
         case Iop_AddF64:
         case Iop_SubF32:
         case Iop_SubF64: {
            MIPSFpOp op = 0;
            HReg argL = iselFltExpr(env, e->Iex.Triop.details->arg2);
            HReg argR = iselFltExpr(env, e->Iex.Triop.details->arg3);
            HReg dst = newVRegF(env);
            switch (e->Iex.Triop.details->op) {
               case Iop_DivF32:
                  op = Mfp_DIVS;
                  break;
               case Iop_DivF64:
                  vassert(fp_mode64);
                  op = Mfp_DIVD;
                  break;
               case Iop_MulF32:
                  op = Mfp_MULS;
                  break;
               case Iop_MulF64:
                  vassert(fp_mode64);
                  op = Mfp_MULD;
                  break;
               case Iop_AddF32:
                  op = Mfp_ADDS;
                  break;
               case Iop_AddF64:
                  vassert(fp_mode64);
                  op = Mfp_ADDD;
                  break;
               case Iop_SubF32:
                  op = Mfp_SUBS;
                  break;
               case Iop_SubF64:
                  vassert(fp_mode64);
                  op = Mfp_SUBD;
                  break;
               default:
                  vassert(0);
            }
            set_MIPS_rounding_mode(env, e->Iex.Triop.details->arg1);
            addInstr(env, MIPSInstr_FpBinary(op, dst, argL, argR));
            set_MIPS_rounding_default(env);
            return dst;
         }
         case Iop_ScaleF64: {
            HReg src1   = iselFltExpr(env, e->Iex.Triop.details->arg2);
            HReg src2   = iselFltExpr(env, e->Iex.Triop.details->arg3);
            HReg v_help = newVRegV(env);
            HReg dst    = newVRegF(env);
            vassert(has_msa);
            set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
            addInstr(env, MIPSInstr_Msa2RF(MSA_FTINT_S, MSA_F_DW, v_help, src2));
            addInstr(env, MIPSInstr_Msa3RF(MSA_FEXP2, MSA_F_DW, dst, src1, v_help));
            set_MIPS_rounding_default_MSA(env);

            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_F64toF32: {
            HReg valD;
            if (mode64)
               valD = iselFltExpr(env, e->Iex.Binop.arg2);
            else
               valD = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg valS = newVRegF(env);

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTSD, valS, valD));
            set_MIPS_rounding_default(env);
            return valS;
         }

         case Iop_RoundF32toInt: {
               HReg valS = newVRegF(env);
               HReg valF = iselFltExpr(env, e->Iex.Binop.arg2);

               set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
#if (__mips_isa_rev >= 6)
               addInstr(env, MIPSInstr_FpConvert(Mfp_RINTS, valS, valF));
#else
               addInstr(env, MIPSInstr_FpConvert(Mfp_CVTWS, valS, valF));
               addInstr(env, MIPSInstr_FpConvert(Mfp_CVTSW, valS, valS));
#endif
               set_MIPS_rounding_default(env);
               return valS;
            }

         case Iop_RoundF64toInt: {
            HReg valS = newVRegF(env);
            HReg valF = iselFltExpr(env, e->Iex.Binop.arg2);

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_FpConvert(Mfp_RINTD, valS, valF));
#else
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTLD, valS, valF));
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDL, valS, valS));

#endif
            set_MIPS_rounding_default(env);
            return valS;
         }

         case Iop_I32StoF32: {
            HReg r_dst = newVRegF(env);
            HReg fr_src = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tmp = newVRegF(env);

            /* Move Word to Floating Point
               mtc1 tmp, fr_src */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mtc1, tmp, fr_src));

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTSW, r_dst, tmp));
            set_MIPS_rounding_default(env);

            return r_dst;
         }

         case Iop_I64StoF64: {
            HReg r_dst = newVRegF(env);
            MIPSAMode *am_addr;
            HReg tmp, fr_src;
            if (mode64) {
               tmp = newVRegF(env);
               fr_src = iselWordExpr_R(env, e->Iex.Binop.arg2);
               /* Move SP down 8 bytes */
               sub_from_sp(env, 8);
               am_addr = MIPSAMode_IR(0, StackPointer(mode64));

               /* store as I64 */
               addInstr(env, MIPSInstr_Store(8, am_addr, fr_src, mode64));

               /* load as Ity_F64 */
               addInstr(env, MIPSInstr_FpLdSt(True /*load */, 8, tmp, am_addr));

               /* Reset SP */
               add_to_sp(env, 8);
            } else {
               HReg Hi, Lo;
               tmp = newVRegD(env);
               iselInt64Expr(&Hi, &Lo, env, e->Iex.Binop.arg2);
               tmp = mk_LoadRR32toFPR(env, Hi, Lo);  /* 2*I32 -> F64 */
            }

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDL, r_dst, tmp));
            set_MIPS_rounding_default(env);

            return r_dst;
         }

         case Iop_I64StoF32: {
            HReg r_dst = newVRegF(env);
            MIPSAMode *am_addr;
            HReg fr_src, tmp;
            if (mode64) {
               tmp = newVRegF(env);
               fr_src = iselWordExpr_R(env, e->Iex.Binop.arg2);
               /* Move SP down 8 bytes */
               sub_from_sp(env, 8);
               am_addr = MIPSAMode_IR(0, StackPointer(mode64));

               /* store as I64 */
               addInstr(env, MIPSInstr_Store(8, am_addr, fr_src, mode64));

               /* load as Ity_F64 */
               addInstr(env, MIPSInstr_FpLdSt(True /*load */, 8, tmp, am_addr));

               /* Reset SP */
               add_to_sp(env, 8);
            } else {
               HReg Hi, Lo;
               tmp = newVRegD(env);
               iselInt64Expr(&Hi, &Lo, env, e->Iex.Binop.arg2);
               tmp = mk_LoadRR32toFPR(env, Hi, Lo);  /* 2*I32 -> F64 */
            }

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTSL, r_dst, tmp));
            set_MIPS_rounding_default(env);

            return r_dst;
         }

         case Iop_SqrtF32:
         case Iop_SqrtF64: {
            Bool sz32 = e->Iex.Binop.op == Iop_SqrtF32;
            HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegF(env);
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpUnary(sz32 ? Mfp_SQRTS : Mfp_SQRTD, dst,
                                            src));
            set_MIPS_rounding_default(env);
            return dst;
         }

         case Iop_I64UtoF64: {
            vassert(mode64);
            HReg r_dst = newVRegF(env);
            HReg tmp = newVRegV(env);
            HReg r_src;
            vassert(has_msa);
            r_src = iselWordExpr_R(env, e->Iex.Binop.arg2);
            set_MIPS_rounding_mode_MSA(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_D, r_src, tmp));
            HReg r_srch = newVRegI(env);
            addInstr(env, MIPSInstr_Msa2RF(MSA_FFINT_U, MSA_F_DW, tmp, tmp));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, tmp, r_srch, MSA_DFN_D | 0));
            sub_from_sp(env, 8);
            MIPSAMode *am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            /* store as I64 */
            addInstr(env, MIPSInstr_Store(8, am_addr, r_srch, mode64));

            /* load as Ity_F64 */
            addInstr(env, MIPSInstr_FpLdSt(True /*load */, 8, r_dst, am_addr));

            /* Reset SP */
            add_to_sp(env, 8);
            set_MIPS_rounding_default_MSA(env);
            return r_dst;
          }

#if (__mips_isa_rev >= 6)
         case Iop_MaxNumF32: {
            HReg src1 = iselFltExpr(env, e->Iex.Binop.arg1);
            HReg src2 = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegF(env);
            addInstr(env, MIPSInstr_FpMinMax(Mfp_MAXS, dst,
                                            src1, src2));
            return dst;
         }

         case Iop_MaxNumF64: {
            HReg src1 = iselFltExpr(env, e->Iex.Binop.arg1);
            HReg src2 = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegF(env);
            addInstr(env, MIPSInstr_FpMinMax(Mfp_MAXD, dst,
                                            src1, src2));
            return dst;
         }

         case Iop_MinNumF32: {
            HReg src1 = iselFltExpr(env, e->Iex.Binop.arg1);
            HReg src2 = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegF(env);
            addInstr(env, MIPSInstr_FpMinMax(Mfp_MINS, dst,
                                            src1, src2));
            return dst;
         }

         case Iop_MinNumF64: {
            HReg src1 = iselFltExpr(env, e->Iex.Binop.arg1);
            HReg src2 = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegF(env);
            addInstr(env, MIPSInstr_FpMinMax(Mfp_MIND, dst,
                                            src1, src2));
            return dst;
         }
#endif
         default:
            break;
      }
   }

   if (e->tag == Iex_Qop) {
      switch (e->Iex.Qop.details->op) {
         case Iop_MAddF32:
         case Iop_MAddF64:
         case Iop_MSubF32:
         case Iop_MSubF64: {
            Int op = 0;
#if (__mips_isa_rev < 6)
            MSADFFlx type = 0;
#endif
            switch (e->Iex.Qop.details->op) {
#if (__mips_isa_rev >= 6)
              case Iop_MAddF32:
                  op = Mfp_MADDS;
                  break;
               case Iop_MAddF64:
                  op = Mfp_MADDD;
                  break;
               case Iop_MSubF32:
                  op = Mfp_MSUBS;
                  break;
               case Iop_MSubF64:
                  op = Mfp_MSUBD;
                  break;
#else
               case Iop_MAddF32:
                  op = has_msa ? MSA_FMADD : Mfp_MADDS;
                  type = MSA_F_WH;
                  break;
               case Iop_MAddF64:
                  op = has_msa ? MSA_FMADD : Mfp_MADDD;
                  type = MSA_F_DW;
                  break;
               case Iop_MSubF32:
                  op = has_msa ? MSA_FMSUB : Mfp_MSUBS;
                  type = MSA_F_WH;
                  break;
               case Iop_MSubF64:
                  op = has_msa ? MSA_FMSUB : Mfp_MSUBD;
                  type = MSA_F_DW;
                  break;
#endif
               default:
                  vassert(0);
            }

            HReg dst = newVRegF(env);
            HReg src1 = iselFltExpr(env, e->Iex.Qop.details->arg2);
            HReg src2 = iselFltExpr(env, e->Iex.Qop.details->arg3);
            HReg src3 = iselFltExpr(env, e->Iex.Qop.details->arg4);
#if (__mips_isa_rev >= 6)
            set_MIPS_rounding_mode(env, e->Iex.Qop.details->arg1);
            addInstr(env, MIPSInstr_FpTernary(op, dst,
                                              src3, src1, src2));
            set_MIPS_rounding_default(env);
#else
            if (has_msa) {
               addInstr(env, MIPSInstr_MsaElm(MSA_MOVE, src3, dst, 0));
               set_MIPS_rounding_mode_MSA(env, e->Iex.Qop.details->arg1);
               addInstr(env, MIPSInstr_Msa3RF(op, type, dst, src1, src2));
               set_MIPS_rounding_default_MSA(env);
            } else {
               set_MIPS_rounding_mode(env, e->Iex.Qop.details->arg1);
               addInstr(env, MIPSInstr_FpTernary(op, dst,
                                                 src1, src2, src3));
               set_MIPS_rounding_default(env);
            }
#endif
            return dst;
         }

         default:
         break;
      }
   }

   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_TruncF64asF32) {
      /* This is quite subtle.  The only way to do the relevant
         truncation is to do a single-precision store and then a
         double precision load to get it back into a register.  The
         problem is, if the data is then written to memory a second
         time, as in

         STbe(...) = TruncF64asF32(...)

         then will the second truncation further alter the value?  The
         answer is no: flds (as generated here) followed by fsts
         (generated for the STbe) is the identity function on 32-bit
         floats, so we are safe.

         Another upshot of this is that if iselStmt can see the
         entirety of

         STbe(...) = TruncF64asF32(arg)

         then it can short circuit having to deal with TruncF64asF32
         individually; instead just compute arg into a 64-bit FP
         register and do 'fsts' (since that itself does the
         truncation).

         We generate pretty poor code here (should be ok both for
         32-bit and 64-bit mode); but it is expected that for the most
         part the latter optimisation will apply and hence this code
         will not often be used.
       */
      HReg fsrc = iselDblExpr(env, e->Iex.Unop.arg);
      HReg fdst = newVRegF(env);
      MIPSAMode *zero_r1 = MIPSAMode_IR(0, StackPointer(mode64));

      sub_from_sp(env, 16);
      /* store as F32, hence truncating */
      addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 4, fsrc, zero_r1));
      /* and reload.  Good huh?! (sigh) */
      addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 4, fdst, zero_r1));
      add_to_sp(env, 16);
      return fdst;
   }

   /* --------- ITE --------- */
   if (e->tag == Iex_ITE) {
      vassert(typeOfIRExpr(env->type_env, e->Iex.ITE.cond) == Ity_I1);
      HReg r0 = iselFltExpr(env, e->Iex.ITE.iffalse);
      HReg r1 = iselFltExpr(env, e->Iex.ITE.iftrue);
      HReg r_cond = iselWordExpr_R(env, e->Iex.ITE.cond);
      HReg r_dst = newVRegF(env);
#if (__mips_isa_rev >= 6)
         addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mtc1, r_dst, r_cond));
         addInstr(env, MIPSInstr_MoveCond(MFpSeld, r_dst, r0, r1));
#else
      addInstr(env, MIPSInstr_FpUnary((ty == Ity_F64) ? Mfp_MOVD : Mfp_MOVS,
                                      r_dst, r0));
      addInstr(env, MIPSInstr_MoveCond((ty == Ity_F64) ? MFpMoveCond_movnd :
                                                         MFpMoveCond_movns,
                                       r_dst, r1, r_cond));
#endif
      return r_dst;
   }

   vex_printf("iselFltExpr(mips): No such tag(0x%x)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselFltExpr_wrk(mips)");
}

static HReg iselDblExpr(ISelEnv * env, IRExpr * e)
{
   HReg r = iselDblExpr_wrk(env, e);
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselDblExpr_wrk(ISelEnv * env, IRExpr * e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(e);
   vassert(ty == Ity_F64);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   /* --------- LOAD --------- */
   if (e->tag == Iex_Load) {
      HReg r_dst = newVRegD(env);
      MIPSAMode *am_addr;
      vassert(e->Iex.Load.ty == Ity_F64);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, ty);
      addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, r_dst, am_addr));
      return r_dst;
   }

   /* --------- GET --------- */
   if (e->tag == Iex_Get) {

      HReg r_dst = newVRegD(env);
      MIPSAMode *am_addr = MIPSAMode_IR(e->Iex.Get.offset,
                                        GuestStatePointer(mode64));
      addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, r_dst, am_addr));
      return r_dst;
   }

   if (e->tag == Iex_Unop) {
      MIPSFpOp fpop = Mfp_INVALID;
      switch (e->Iex.Unop.op) {
         case Iop_NegF64:
            fpop = Mfp_NEGD;
            break;
         case Iop_AbsF64:
            fpop = Mfp_ABSD;
            break;
         case Iop_F32toF64: {
            vassert(!mode64);
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);

            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDS, dst, src));
            return dst;
         }
         case Iop_ReinterpI64asF64: {
            HReg Hi, Lo;
            HReg dst = newVRegD(env);

            iselInt64Expr(&Hi, &Lo, env, e->Iex.Unop.arg);

            dst = mk_LoadRR32toFPR(env, Hi, Lo);  /* 2*I32 -> F64 */
            return dst;
         }
         case Iop_I32StoF64: {
            vassert(!mode64);
            HReg dst = newVRegD(env);
            HReg tmp = newVRegF(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

            /* Move Word to Floating Point
               mtc1 tmp, r_src */
            addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mtc1, tmp, r_src));

            /* and do convert */
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDW, dst, tmp));

            return dst;
         }
         default:
            break;
      }

      if (fpop != Mfp_INVALID) {
         HReg src = iselDblExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegD(env);
         addInstr(env, MIPSInstr_FpUnary(fpop, dst, src));
         return dst;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_RoundF64toInt: {
            HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegD(env);

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
#if (__mips_isa_rev >= 6)
            addInstr(env, MIPSInstr_FpConvert(Mfp_RINTD, dst, src));
#else
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTLD, dst, src));
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDL, dst, dst));

#endif
            set_MIPS_rounding_default(env);

            return dst; 
         }

         case Iop_SqrtF64: {
            HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegD(env);
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpUnary(Mfp_SQRTD, dst, src));
            set_MIPS_rounding_default(env);
            return dst;
         }

         case Iop_I64StoF64: {
            HReg r_dst = newVRegD(env);
            MIPSAMode *am_addr;
            HReg tmp, fr_src;
            if (mode64) {
               tmp = newVRegD(env);
               fr_src = iselDblExpr(env, e->Iex.Binop.arg2);
               /* Move SP down 8 bytes */
               sub_from_sp(env, 8);
               am_addr = MIPSAMode_IR(0, StackPointer(mode64));

               /* store as I64 */
               addInstr(env, MIPSInstr_Store(8, am_addr, fr_src, mode64));

               /* load as Ity_F64 */
               addInstr(env, MIPSInstr_FpLdSt(True /*load */, 8, tmp, am_addr));

               /* Reset SP */
               add_to_sp(env, 8);
            } else {
               HReg Hi, Lo;
               tmp = newVRegD(env);
               iselInt64Expr(&Hi, &Lo, env, e->Iex.Binop.arg2);
               tmp = mk_LoadRR32toFPR(env, Hi, Lo);  /* 2*I32 -> F64 */
            }

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDL, r_dst, tmp));
            set_MIPS_rounding_default(env);

            return r_dst;
         }

         case Iop_I64UtoF64: {
            HReg r_dst;
            HReg tmp = newVRegV(env);
            HReg r_src2h, r_src2l;
            vassert(has_msa);
            iselInt64Expr(&r_src2h, &r_src2l, env, e->Iex.Binop.arg2);
            set_MIPS_rounding_mode_MSA(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_Msa2R(MSA_FILL, MSA_W, r_src2l, tmp));
            addInstr(env, MIPSInstr_MsaElm(MSA_INSERT, r_src2h, tmp, MSA_DFN_W | 1));
            addInstr(env, MIPSInstr_MsaElm(MSA_INSERT, r_src2l, tmp, MSA_DFN_W | 2));
            addInstr(env, MIPSInstr_MsaElm(MSA_INSERT, r_src2h, tmp, MSA_DFN_W | 3));
            HReg r_srchh = newVRegI(env);
            HReg r_srchl = newVRegI(env);
            addInstr(env, MIPSInstr_Msa2RF(MSA_FFINT_U, MSA_F_DW, tmp, tmp));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, tmp, r_srchl, MSA_DFN_W | 0));
            addInstr(env, MIPSInstr_MsaElm(MSA_COPY_S, tmp, r_srchh, MSA_DFN_W | 1));
            r_dst = mk_LoadRR32toFPR(env, r_srchh, r_srchl);
            set_MIPS_rounding_default_MSA(env);
            return r_dst;
          }
#if (__mips_isa_rev >= 6)
         case Iop_MaxNumF64: {
            HReg src1 = iselDblExpr(env, e->Iex.Binop.arg1);
            HReg src2 = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegD(env);
            addInstr(env, MIPSInstr_FpMinMax(Mfp_MAXD, dst,
                                            src1, src2));
            return dst;
         }

         case Iop_MinNumF64: {
            HReg src1 = iselDblExpr(env, e->Iex.Binop.arg1);
            HReg src2 = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegD(env);
            addInstr(env, MIPSInstr_FpMinMax(Mfp_MIND, dst,
                                            src1, src2));
            return dst;
         }
#endif

         default:
            break;

      }
   }

   if (e->tag == Iex_Triop) {
      switch (e->Iex.Triop.details->op) {
         case Iop_DivF64:
         case Iop_DivF32:
         case Iop_MulF64:
         case Iop_AddF64:
         case Iop_SubF64: {
            MIPSFpOp op = 0;
            HReg argL = iselDblExpr(env, e->Iex.Triop.details->arg2);
            HReg argR = iselDblExpr(env, e->Iex.Triop.details->arg3);
            HReg dst = newVRegD(env);
            switch (e->Iex.Triop.details->op) {
               case Iop_DivF64:
                  op = Mfp_DIVD;
                  break;
               case Iop_DivF32:
                  op = Mfp_DIVS;
                  break;
               case Iop_MulF64:
                  op = Mfp_MULD;
                  break;
               case Iop_AddF64:
                  op = Mfp_ADDD;
                  break;
               case Iop_SubF64:
                  op = Mfp_SUBD;
                  break;
               default:
                  vassert(0);
            }
            set_MIPS_rounding_mode(env, e->Iex.Triop.details->arg1);
            addInstr(env, MIPSInstr_FpBinary(op, dst, argL, argR));
            set_MIPS_rounding_default(env);
            return dst;
         }

         case Iop_ScaleF64: {
            HReg src1   = iselDblExpr(env, e->Iex.Triop.details->arg2);
            HReg src2   = iselDblExpr(env, e->Iex.Triop.details->arg3);
            HReg v_help = newVRegV(env);
            HReg dst    = newVRegD(env);
            vassert(has_msa);
            set_MIPS_rounding_mode_MSA(env, e->Iex.Triop.details->arg1);
            addInstr(env, MIPSInstr_Msa2RF(MSA_FTINT_S, MSA_F_DW, v_help, src2));
            addInstr(env, MIPSInstr_Msa3RF(MSA_FEXP2, MSA_F_DW, dst, src1, v_help));
            set_MIPS_rounding_default_MSA(env);
            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Qop) {
      switch (e->Iex.Qop.details->op) {
         case Iop_MAddF64:
         case Iop_MSubF64: {
            MSA3RFOp op = 0;
            switch (e->Iex.Qop.details->op) {
#if (__mips_isa_rev >= 6)
              case Iop_MAddF64:
                  op = Mfp_MADDD;
                  break;
               case Iop_MSubF64:
                  op = Mfp_MSUBD;
                  break;
#else
               case Iop_MAddF64:
                  op = MSA_FMADD;
                  break;
               case Iop_MSubF64:
                  op = MSA_FMSUB;
                  break;
#endif
               default:
                  vassert(0);
            }
            HReg dst = newVRegD(env);
            HReg src1 = iselDblExpr(env, e->Iex.Qop.details->arg2);
            HReg src2 = iselDblExpr(env, e->Iex.Qop.details->arg3);
            HReg src3 = iselDblExpr(env, e->Iex.Qop.details->arg4);
#if (__mips_isa_rev >= 6)
            set_MIPS_rounding_mode(env, e->Iex.Qop.details->arg1);
            addInstr(env, MIPSInstr_FpTernary(op, dst,
                                              src3, src1, src2));
            set_MIPS_rounding_default(env);
#else
            vassert(has_msa);
            addInstr(env, MIPSInstr_MsaElm(MSA_MOVE, src3, dst, 0));
            set_MIPS_rounding_mode_MSA(env, e->Iex.Qop.details->arg1);
            addInstr(env, MIPSInstr_Msa3RF(op, MSA_F_DW, dst, src1, src2));
            set_MIPS_rounding_default_MSA(env);
#endif
            return dst;
         }
         case Iop_I64StoF64: {
            HReg r_dst = newVRegD(env);
            MIPSAMode *am_addr;
            HReg tmp, fr_src;
            if (mode64) {
               tmp = newVRegF(env);
               fr_src = iselWordExpr_R(env, e->Iex.Binop.arg2);
               /* Move SP down 8 bytes */
               sub_from_sp(env, 8);
               am_addr = MIPSAMode_IR(0, StackPointer(mode64));

               /* store as I64 */
               addInstr(env, MIPSInstr_Store(8, am_addr, fr_src, mode64));

               /* load as Ity_F64 */
               addInstr(env, MIPSInstr_FpLdSt(True /*load */, 8, tmp, am_addr));

               /* Reset SP */
               add_to_sp(env, 8);
            } else {
               HReg Hi, Lo;
               tmp = newVRegD(env);
               iselInt64Expr(&Hi, &Lo, env, e->Iex.Binop.arg2);
               tmp = mk_LoadRR32toFPR(env, Hi, Lo);  /* 2*I32 -> F64 */
            }

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDL, r_dst, tmp));
            set_MIPS_rounding_default(env);

            return r_dst;
         }

         default:
         break;
      }
   }

   /* --------- ITE --------- */
   if (e->tag == Iex_ITE) {
      if (ty == Ity_F64
          && typeOfIRExpr(env->type_env, e->Iex.ITE.cond) == Ity_I1) {
         HReg r0 = iselDblExpr(env, e->Iex.ITE.iffalse);
         HReg r1 = iselDblExpr(env, e->Iex.ITE.iftrue);
         HReg r_cond = iselWordExpr_R(env, e->Iex.ITE.cond);
         HReg r_dst = newVRegD(env);
#if (__mips_isa_rev >= 6)
         addInstr(env, MIPSInstr_FpGpMove(MFpGpMove_mtc1, r_dst, r_cond));
         addInstr(env, MIPSInstr_MoveCond(MFpSeld, r_dst, r0, r1));
#else
         addInstr(env, MIPSInstr_FpUnary(Mfp_MOVD, r_dst, r0));
         addInstr(env, MIPSInstr_MoveCond(MFpMoveCond_movnd, r_dst, r1,
                                            r_cond));
#endif
         return r_dst;
      }
   }

   vex_printf("iselDblExpr(mips): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselDblExpr_wrk(mips)");
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
      /* --------- STORE --------- */
      case Ist_Store: {
         MIPSAMode *am_addr;
         IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);

         if (tyd == Ity_V128) {
            vassert(has_msa);
            HReg res = iselV128Expr(env, stmt->Ist.Store.data);
            HReg addr = iselWordExpr_R(env, stmt->Ist.Store.addr);
            addInstr(env, MIPSInstr_MsaMi10(MSA_ST, 0, addr, res, MSA_B));
            return;
         }

         /*constructs addressing mode from address provided */
         am_addr = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd);

         if (tyd == Ity_I8 || tyd == Ity_I16 || tyd == Ity_I32 ||
             (mode64 && (tyd == Ity_I64))) {
            HReg r_src = iselWordExpr_R(env, stmt->Ist.Store.data);
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(tyd)),
                     am_addr, r_src, mode64));
            return;
         }
         if (!mode64 && (tyd == Ity_I64)) {
            HReg vHi, vLo;
            HReg r_addr = iselWordExpr_R(env, stmt->Ist.Store.addr);

            iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Store.data);

            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(Ity_I32)),
                          MIPSAMode_IR(0, r_addr), vHi, mode64));
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(Ity_I32)),
                          MIPSAMode_IR(4, r_addr), vLo, mode64));
            return;
         }
         if (tyd == Ity_F32) {
            HReg fr_src = iselFltExpr(env, stmt->Ist.Store.data);
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 4, fr_src,
                                           am_addr));
            return;
         }
         if (tyd == Ity_F64 && mode64) {
            HReg fr_src = iselFltExpr(env, stmt->Ist.Store.data);
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, fr_src,
                                           am_addr));
            return;
         }
         if (!mode64 && (tyd == Ity_F64)) {
            HReg fr_src = iselDblExpr(env, stmt->Ist.Store.data);
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, fr_src,
                                           am_addr));
            return;
         }

         break;
      }

      /* --------- PUT --------- */
      case Ist_Put: {
         IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);

         if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 ||
             (ty == Ity_I64 && mode64)) {
            HReg r_src = iselWordExpr_R(env, stmt->Ist.Put.data);
            MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                              GuestStatePointer(mode64));
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(ty)),
                                          am_addr, r_src, mode64));
            return;
         }

         if (ty == Ity_I64 && !mode64) {
            HReg vHi, vLo;
            MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                              GuestStatePointer(mode64));
            MIPSAMode *am_addr4 = MIPSAMode_IR(stmt->Ist.Put.offset + 4,
                                               GuestStatePointer(mode64));
            iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Put.data);
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(Ity_I32)),
                                          am_addr, vLo, mode64));
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(Ity_I32)),
                                          am_addr4, vHi, mode64));
            return;

         }

         if (ty == Ity_F32) {
            HReg fr_src = iselFltExpr(env, stmt->Ist.Put.data);
            MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                              GuestStatePointer(mode64));
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 4, fr_src,
                                           am_addr));
            return;
         }

         if (ty == Ity_F64) {
            if (mode64) {
              HReg fr_src = iselFltExpr(env, stmt->Ist.Put.data);
              MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                                GuestStatePointer(mode64));
              addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, fr_src,
                                             am_addr));
            } else {
              HReg fr_src = iselDblExpr(env, stmt->Ist.Put.data);
              MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                                GuestStatePointer(mode64));
              addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, fr_src,
                                             am_addr));
            }
            return;
         }
         if (ty == Ity_V128) {
            vassert(has_msa);
            HReg v_src = iselV128Expr(env, stmt->Ist.Put.data);
#if defined(_MIPSEB)
            HReg r_addr = newVRegI(env);
            addInstr(env, MIPSInstr_Alu(mode64 ? Malu_DADD : Malu_ADD, r_addr, GuestStatePointer(mode64),
                                        MIPSRH_Imm(False, stmt->Ist.Put.offset)));
            addInstr(env, MIPSInstr_MsaMi10(MSA_ST, 0, r_addr, v_src, MSA_B));
#else
            vassert(!(stmt->Ist.Put.offset & 7));
            addInstr(env, MIPSInstr_MsaMi10(MSA_ST, stmt->Ist.Put.offset >> 3,
                                            GuestStatePointer(mode64), v_src, MSA_D));
#endif
            return;
         }
         break;
      }

      /* --------- TMP --------- */
      case Ist_WrTmp: {
         IRTemp tmp = stmt->Ist.WrTmp.tmp;
         IRType ty = typeOfIRTemp(env->type_env, tmp);

         if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I1) {
            HReg r_dst = lookupIRTemp(env, tmp);
            HReg r_src = iselWordExpr_R(env, stmt->Ist.WrTmp.data);
            addInstr(env, mk_iMOVds_RR(r_dst, r_src));
            return;
         }

         if (ty == Ity_I64) {
            if (mode64) {
               HReg r_dst = lookupIRTemp(env, tmp);
               HReg r_src = iselWordExpr_R(env, stmt->Ist.WrTmp.data);
               addInstr(env, mk_iMOVds_RR(r_dst, r_src));
               return;
            } else {
               HReg rHi, rLo, dstHi, dstLo;
               iselInt64Expr(&rHi, &rLo, env, stmt->Ist.WrTmp.data);
               lookupIRTemp64(&dstHi, &dstLo, env, tmp);
               addInstr(env, mk_iMOVds_RR(dstHi, rHi));
               addInstr(env, mk_iMOVds_RR(dstLo, rLo));
               return;
            }
         }

         if (mode64 && ty == Ity_I128) {
            HReg rHi, rLo, dstHi, dstLo;
            iselInt128Expr(&rHi, &rLo, env, stmt->Ist.WrTmp.data);
            lookupIRTempPair(&dstHi, &dstLo, env, tmp);
            addInstr(env, mk_iMOVds_RR(dstHi, rHi));
            addInstr(env, mk_iMOVds_RR(dstLo, rLo));
            return;
         }

         if (ty == Ity_F32) {
            HReg fr_dst = lookupIRTemp(env, tmp);
            HReg fr_src = iselFltExpr(env, stmt->Ist.WrTmp.data);
            addInstr(env, MIPSInstr_FpUnary(Mfp_MOVS, fr_dst, fr_src));
            return;
         }

         if (ty == Ity_F64) {
            if (mode64) {
               HReg src = iselFltExpr(env, stmt->Ist.WrTmp.data);
               HReg dst = lookupIRTemp(env, tmp);
               addInstr(env, MIPSInstr_FpUnary(Mfp_MOVD, dst, src));
               return;
            } else {
               HReg src = iselDblExpr(env, stmt->Ist.WrTmp.data);
               HReg dst = lookupIRTemp(env, tmp);
               addInstr(env, MIPSInstr_FpUnary(Mfp_MOVD, dst, src));
               return;
            }
         }

         if (ty == Ity_V128) {
            vassert(has_msa);
            HReg v_dst = lookupIRTemp(env, tmp);
            HReg v_src = iselV128Expr(env, stmt->Ist.WrTmp.data);
            addInstr(env, MIPSInstr_MsaElm(MSA_MOVE, v_src, v_dst, 0));
            return;
          }
         break;
      }

      /* --------- Call to DIRTY helper --------- */
      case Ist_Dirty: {
         IRDirty *d = stmt->Ist.Dirty.details;

         /* Figure out the return type, if any. */
         IRType retty = Ity_INVALID;
         if (d->tmp != IRTemp_INVALID)
            retty = typeOfIRTemp(env->type_env, d->tmp);

         /* Throw out any return types we don't know about. */
         Bool retty_ok = False;
         switch (retty) {
            case Ity_INVALID: /* Function doesn't return anything. */
            case Ity_V128:
            case Ity_I64: case Ity_I32: case Ity_I16: case Ity_I8:
               retty_ok = True; break;
            default:
               break;
         }

         if (!retty_ok)
            break; /* will go to stmt_fail: */

         /* Marshal args, do the call, clear stack, set the return value
            to 0x555..555 if this is a conditional call that returns a
            value and the call is skipped. */
         UInt   addToSp = 0;
         RetLoc rloc    = mk_RetLoc_INVALID();
         doHelperCall( &addToSp, &rloc, env, d->guard, d->cee, retty, d->args );
         vassert(is_sane_RetLoc(rloc));

         /* Now figure out what to do with the returned value, if any. */
         switch (retty) {
            case Ity_INVALID: {
               /* No return value.  Nothing to do. */
               vassert(d->tmp == IRTemp_INVALID);
               vassert(rloc.pri == RLPri_None);
               vassert(addToSp == 0);
               return;
            }
            case Ity_I32: case Ity_I16: case Ity_I8: {
               /* The returned value is in $v0.  Park it in the register
                  associated with tmp. */
               HReg r_dst = lookupIRTemp(env, d->tmp);
               addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, r_dst,
                                            hregMIPS_GPR2(mode64),
                                            MIPSRH_Imm(False, 0)));
               vassert(rloc.pri == RLPri_Int);
               vassert(addToSp == 0);
               return;
            }
            case Ity_I64: {
               if (mode64) {
                  /* The returned value is in $v0.  Park it in the register
                     associated with tmp. */
                  HReg r_dst = lookupIRTemp(env, d->tmp);
                  addInstr(env, mk_iMOVds_RR(r_dst, hregMIPS_GPR2(mode64)));
                  vassert(rloc.pri == RLPri_Int);
                  vassert(addToSp == 0);
                  return;
               } else {
                  HReg rHi = newVRegI(env);
                  HReg rLo = newVRegI(env);
                  HReg dstHi, dstLo;
                  addInstr(env, mk_iMOVds_RR(rLo, hregMIPS_GPR2(mode64)));
                  addInstr(env, mk_iMOVds_RR(rHi, hregMIPS_GPR3(mode64)));
                  lookupIRTemp64(&dstHi, &dstLo, env, d->tmp);
                  addInstr(env, mk_iMOVds_RR(dstHi, rHi));
                  addInstr(env, mk_iMOVds_RR(dstLo, rLo));
                  return;
               }
            }
            case Ity_V128: {
               vassert(has_msa);
               vassert(rloc.pri == RLPri_V128SpRel);
               vassert((rloc.spOff < 512) && (rloc.spOff > -512));
               vassert(addToSp >= 16);
               HReg       dst = lookupIRTemp(env, d->tmp);
               addInstr(env, MIPSInstr_MsaMi10(MSA_LD, rloc.spOff, StackPointer(mode64), dst, MSA_B));
               add_to_sp(env, addToSp);
               return;

            }
            default:
               /*NOTREACHED*/
               vassert(0);
         }
      }

      /* --------- Load Linked or Store Conditional --------- */
      case Ist_LLSC: {
         /* Temporary solution; this need to be rewritten again for MIPS.
            On MIPS you can not read from address that is locked with LL
            before SC. If you read from address that is locked than SC will
            fall. */
         IRTemp res = stmt->Ist.LLSC.result;
         IRType tyRes = typeOfIRTemp(env->type_env, res);
         IRType tyAddr = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.addr);

         if (!mode64 && (tyAddr != Ity_I32))
            goto stmt_fail;

         if (stmt->Ist.LLSC.storedata == NULL) {
            /* LL */
            MIPSAMode *r_addr;
            /* constructs addressing mode from address provided */
            r_addr = iselWordExpr_AMode(env, stmt->Ist.LLSC.addr, tyAddr);

            HReg r_dst = lookupIRTemp(env, res);
            if (tyRes == Ity_I32) {
               addInstr(env, MIPSInstr_LoadL(4, r_dst, r_addr, mode64));
               return;
            } else if (tyRes == Ity_I64 && mode64) {
               addInstr(env, MIPSInstr_LoadL(8, r_dst, r_addr, mode64));
               return;
            }
         } else {
            /* SC */
            MIPSAMode *r_addr;
            r_addr = iselWordExpr_AMode(env, stmt->Ist.LLSC.addr, tyAddr);
            HReg r_src = iselWordExpr_R(env, stmt->Ist.LLSC.storedata);
            HReg r_dst = lookupIRTemp(env, res);
            IRType tyData = typeOfIRExpr(env->type_env,
                                         stmt->Ist.LLSC.storedata);

            if (tyData == Ity_I32) {
               addInstr(env, mk_iMOVds_RR(r_dst, r_src));
               addInstr(env, MIPSInstr_StoreC(4, r_addr, r_dst, mode64));
               return;
            } else if (tyData == Ity_I64 && mode64) {
               addInstr(env, mk_iMOVds_RR(r_dst, r_src));
               addInstr(env, MIPSInstr_StoreC(8, r_addr, r_dst, mode64));
               return;
            }
         }
         goto stmt_fail;
       /* NOTREACHED */}

   case Ist_CAS:
      if (stmt->Ist.CAS.details->oldHi == IRTemp_INVALID) {
         IRCAS *cas = stmt->Ist.CAS.details;
         HReg old   = lookupIRTemp(env, cas->oldLo);
         HReg addr  = iselWordExpr_R(env, cas->addr);
         HReg expd  = iselWordExpr_R(env, cas->expdLo);
         HReg data  = iselWordExpr_R(env, cas->dataLo);
         if (typeOfIRTemp(env->type_env, cas->oldLo) == Ity_I64) {
            addInstr(env, MIPSInstr_Cas(8, old, addr, expd, data, mode64));
         } else if (typeOfIRTemp(env->type_env, cas->oldLo) == Ity_I32) {
            addInstr(env, MIPSInstr_Cas(4, old, addr, expd, data, mode64));
         }
      }
      return;

   /* --------- INSTR MARK --------- */
   /* Doesn't generate any executable code ... */
   case Ist_IMark:
      return;

   /* --------- ABI HINT --------- */
   /* These have no meaning (denotation in the IR) and so we ignore
      them ... if any actually made it this far. */
   case Ist_AbiHint:
      return;

   /* --------- NO-OP --------- */
   /* Fairly self-explanatory, wouldn't you say? */
   case Ist_NoOp:
      return;

   /* --------- EXIT --------- */
   case Ist_Exit: {
      IRConst* dst = stmt->Ist.Exit.dst;
      if (!mode64 && dst->tag != Ico_U32)
         vpanic("iselStmt(mips32): Ist_Exit: dst is not a 32-bit value");
      if (mode64 && dst->tag != Ico_U64)
         vpanic("iselStmt(mips64): Ist_Exit: dst is not a 64-bit value");

      MIPSCondCode cc   = iselCondCode(env, stmt->Ist.Exit.guard);
      MIPSAMode*   amPC = MIPSAMode_IR(stmt->Ist.Exit.offsIP,
                                      GuestStatePointer(mode64));

      /* Case: boring transfer to known address */
      if (stmt->Ist.Exit.jk == Ijk_Boring
          || stmt->Ist.Exit.jk == Ijk_Call
          /* || stmt->Ist.Exit.jk == Ijk_Ret */) {
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = mode64
               ? (((Addr64)stmt->Ist.Exit.dst->Ico.U64) > (Addr64)env->max_ga)
               : (((Addr32)stmt->Ist.Exit.dst->Ico.U32) > (Addr32)env->max_ga);
            if (0) vex_printf("%s", toFastEP ? "Y" : ",");
            addInstr(env, MIPSInstr_XDirect(
                             mode64 ? (Addr64)stmt->Ist.Exit.dst->Ico.U64
                                    : (Addr64)stmt->Ist.Exit.dst->Ico.U32,
                             amPC, cc, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselWordExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, MIPSInstr_XAssisted(r, amPC, cc, Ijk_Boring));
         }
         return;
      }

      /* Case: assisted transfer to arbitrary address */
      switch (stmt->Ist.Exit.jk) {
         /* Keep this list in sync with that in iselNext below */
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
         case Ijk_InvalICache:
         {
            HReg r = iselWordExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, MIPSInstr_XAssisted(r, amPC, cc,
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
      vex_printf("stmt_fail tag: 0x%x\n", stmt->tag);
      ppIRStmt(stmt);
      vpanic("iselStmt:\n");
}

/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

static void iselNext ( ISelEnv* env,
                       IRExpr* next, IRJumpKind jk, Int offsIP )
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
      vassert(cdst->tag == (env->mode64 ? Ico_U64 :Ico_U32));
      if (jk == Ijk_Boring || jk == Ijk_Call) {
         /* Boring transfer to known address */
         MIPSAMode* amPC = MIPSAMode_IR(offsIP, GuestStatePointer(env->mode64));
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = env->mode64
               ? (((Addr64)cdst->Ico.U64) > (Addr64)env->max_ga)
               : (((Addr32)cdst->Ico.U32) > (Addr32)env->max_ga);
            if (0) vex_printf("%s", toFastEP ? "X" : ".");
            addInstr(env, MIPSInstr_XDirect(
                             env->mode64 ? (Addr64)cdst->Ico.U64
                                         : (Addr64)cdst->Ico.U32,
                             amPC, MIPScc_AL, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselWordExpr_R(env, next);
            addInstr(env, MIPSInstr_XAssisted(r, amPC, MIPScc_AL,
                                              Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
      case Ijk_Boring: case Ijk_Ret: case Ijk_Call: {
         HReg       r     = iselWordExpr_R(env, next);
         MIPSAMode*  amPC = MIPSAMode_IR(offsIP,
                                         GuestStatePointer(env->mode64));
         if (env->chainingAllowed) {
            addInstr(env, MIPSInstr_XIndir(r, amPC, MIPScc_AL));
         } else {
            addInstr(env, MIPSInstr_XAssisted(r, amPC, MIPScc_AL,
                                              Ijk_Boring));
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
         HReg      r     = iselWordExpr_R(env, next);
         MIPSAMode* amPC = MIPSAMode_IR(offsIP, GuestStatePointer(env->mode64));
         addInstr(env, MIPSInstr_XAssisted(r, amPC, MIPScc_AL, jk));
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

/* Translate an entire BB to mips code. */
HInstrArray *iselSB_MIPS ( const IRSB* bb,
                           VexArch arch_host,
                           const VexArchInfo* archinfo_host,
                           const VexAbiInfo* vbi,
                           Int offs_Host_EvC_Counter,
                           Int offs_Host_EvC_FailAddr,
                           Bool chainingAllowed,
                           Bool addProfInc,
                           Addr max_ga )
{
   Int      i, j;
   HReg     hreg, hregHI;
   ISelEnv* env;
   MIPSAMode *amCounter, *amFailAddr;

   hwcaps_host = archinfo_host->hwcaps;

   /* sanity ... */
   vassert(arch_host == VexArchMIPS32 || arch_host == VexArchMIPS64);
   vassert(VEX_PRID_COMP_MIPS == VEX_MIPS_COMP_ID(hwcaps_host)
           || VEX_PRID_COMP_CAVIUM == VEX_MIPS_COMP_ID(hwcaps_host)
           || VEX_PRID_COMP_BROADCOM == VEX_MIPS_COMP_ID(hwcaps_host)
           || VEX_PRID_COMP_NETLOGIC == VEX_MIPS_COMP_ID(hwcaps_host)
           || VEX_PRID_COMP_INGENIC_E1 == VEX_MIPS_COMP_ID(hwcaps_host)
           || VEX_PRID_COMP_LEGACY == VEX_MIPS_COMP_ID(hwcaps_host));

   /* Check that the host's endianness is as expected. */
   vassert(archinfo_host->endness == VexEndnessLE
           || archinfo_host->endness == VexEndnessBE);

   mode64 = arch_host != VexArchMIPS32;
   fp_mode64 = VEX_MIPS_HOST_FP_MODE(hwcaps_host);
   has_msa = VEX_MIPS_PROC_MSA(archinfo_host->hwcaps);

   /* Make up an initial environment to use. */
   env = LibVEX_Alloc_inline(sizeof(ISelEnv));
   env->vreg_ctr = 0;
   env->mode64 = mode64;
   env->fp_mode64 = fp_mode64;

   /* Set up output code array. */
   env->code = newHInstrArray();

   /* Copy BB's type env. */
   env->type_env = bb->tyenv;

   /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
      change as we go along. */
   env->n_vregmap = bb->tyenv->types_used;
   env->vregmap = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
   env->vregmapHI = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));

   /* and finally ... */
   env->hwcaps          = hwcaps_host;
   env->chainingAllowed = chainingAllowed;
   env->hwcaps          = hwcaps_host;
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
            if (mode64) {
               hreg = mkHReg(True, HRcInt64, 0, j++);
               break;
            } else {
               hreg = mkHReg(True, HRcInt32, 0, j++);
               break;
            }
         case Ity_I64:
            if (mode64) {
               hreg = mkHReg(True, HRcInt64, 0, j++);
               break;
            } else {
               hreg   = mkHReg(True, HRcInt32, 0, j++);
               hregHI = mkHReg(True, HRcInt32, 0, j++);
               break;
            }
         case Ity_I128:
            vassert(mode64);
            hreg   = mkHReg(True, HRcInt64, 0, j++);
            hregHI = mkHReg(True, HRcInt64, 0, j++);
            break;
         case Ity_F32:
            if (mode64) {
               hreg = mkHReg(True, HRcFlt64, 0, j++);
               break;
            } else {
               hreg = mkHReg(True, HRcFlt32, 0, j++);
               break;
            }
         case Ity_F64:
            hreg = mkHReg(True, HRcFlt64, 0, j++);
            break;
         case Ity_V128:
            hreg = mkHReg(True, HRcVec128, 0, j++);
            break;
         default:
            ppIRType(bb->tyenv->types[i]);
            vpanic("iselBB(mips): IRTemp type");
            break;
      }
      env->vregmap[i] = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   amCounter = MIPSAMode_IR(offs_Host_EvC_Counter, GuestStatePointer(mode64));
   amFailAddr = MIPSAMode_IR(offs_Host_EvC_FailAddr, GuestStatePointer(mode64));
   addInstr(env, MIPSInstr_EvCheck(amCounter, amFailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, MIPSInstr_ProfInc());
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
/*--- end                                    host_mips_isel.c ---*/
/*---------------------------------------------------------------*/
