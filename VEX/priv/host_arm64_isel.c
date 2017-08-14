
/*---------------------------------------------------------------*/
/*--- begin                                 host_arm64_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2015 OpenWorks
      info@open-works.net

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
#include "ir_match.h"

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_generic_simd64.h"  // for 32-bit SIMD helpers
#include "host_arm64_defs.h"


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
     change.  We expect this mapping to map precisely the same set of
     IRTemps as the type mapping does.

     |vregmap|   holds the primary register for the IRTemp.
     |vregmapHI| is only used for 128-bit integer-typed
                 IRTemps.  It holds the identity of a second
                 64-bit virtual HReg, which holds the high half
                 of the value.

   - The code array, that is, the insns selected so far.

   - A counter, for generating new virtual registers.

   - The host hardware capabilities word.  This is set at the start
     and does not change.

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

    - An IRExpr*, which may be NULL, holding the IR expression (an
      IRRoundingMode-encoded value) to which the FPU's rounding mode
      was most recently set.  Setting to NULL is always safe.  Used to
      avoid redundant settings of the FPU's rounding mode, as
      described in set_FPCR_rounding_mode below.

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

      IRExpr*      previous_rm;
   }
   ISelEnv;

static HReg lookupIRTemp ( ISelEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   return env->vregmap[tmp];
}

static void lookupIRTempPair ( HReg* vrHI, HReg* vrLO, 
                               ISelEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   vassert(! hregIsInvalid(env->vregmapHI[tmp]));
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr ( ISelEnv* env, ARM64Instr* instr )
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppARM64Instr(instr);
      vex_printf("\n");
   }
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcInt64, 0, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegD ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcFlt64, 0, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegV ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcVec128, 0, env->vreg_ctr);
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

   Because some forms of ARM64 memory amodes are implicitly scaled by
   the access size, iselIntExpr_AMode takes an IRType which tells it
   the type of the access for which the amode is to be used.  This
   type needs to be correct, else you'll get incorrect code.
*/
static ARM64AMode* iselIntExpr_AMode_wrk ( ISelEnv* env,
                                           IRExpr* e, IRType dty );
static ARM64AMode* iselIntExpr_AMode     ( ISelEnv* env,
                                           IRExpr* e, IRType dty );

static ARM64RIA*   iselIntExpr_RIA_wrk   ( ISelEnv* env, IRExpr* e );
static ARM64RIA*   iselIntExpr_RIA       ( ISelEnv* env, IRExpr* e );

static ARM64RIL*   iselIntExpr_RIL_wrk   ( ISelEnv* env, IRExpr* e );
static ARM64RIL*   iselIntExpr_RIL       ( ISelEnv* env, IRExpr* e );

static ARM64RI6*   iselIntExpr_RI6_wrk   ( ISelEnv* env, IRExpr* e );
static ARM64RI6*   iselIntExpr_RI6       ( ISelEnv* env, IRExpr* e );

static ARM64CondCode iselCondCode_wrk    ( ISelEnv* env, IRExpr* e );
static ARM64CondCode iselCondCode        ( ISelEnv* env, IRExpr* e );

static HReg        iselIntExpr_R_wrk     ( ISelEnv* env, IRExpr* e );
static HReg        iselIntExpr_R         ( ISelEnv* env, IRExpr* e );

static void        iselInt128Expr_wrk    ( /*OUT*/HReg* rHi, HReg* rLo, 
                                           ISelEnv* env, IRExpr* e );
static void        iselInt128Expr        ( /*OUT*/HReg* rHi, HReg* rLo, 
                                           ISelEnv* env, IRExpr* e );

static HReg        iselDblExpr_wrk        ( ISelEnv* env, IRExpr* e );
static HReg        iselDblExpr            ( ISelEnv* env, IRExpr* e );

static HReg        iselFltExpr_wrk        ( ISelEnv* env, IRExpr* e );
static HReg        iselFltExpr            ( ISelEnv* env, IRExpr* e );

static HReg        iselF16Expr_wrk        ( ISelEnv* env, IRExpr* e );
static HReg        iselF16Expr            ( ISelEnv* env, IRExpr* e );

static HReg        iselV128Expr_wrk       ( ISelEnv* env, IRExpr* e );
static HReg        iselV128Expr           ( ISelEnv* env, IRExpr* e );

static void        iselV256Expr_wrk       ( /*OUT*/HReg* rHi, HReg* rLo, 
                                            ISelEnv* env, IRExpr* e );
static void        iselV256Expr           ( /*OUT*/HReg* rHi, HReg* rLo, 
                                            ISelEnv* env, IRExpr* e );

static ARM64RIL* mb_mkARM64RIL_I ( ULong imm64 );


/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

/* Generate an amode suitable for a 64-bit sized access relative to
   the baseblock register (X21).  This generates an RI12 amode, which
   means its scaled by the access size, which is why the access size
   -- 64 bit -- is stated explicitly here.  Consequently |off| needs
   to be divisible by 8. */
static ARM64AMode* mk_baseblock_64bit_access_amode ( UInt off )
{
   vassert(off < (8 << 12)); /* otherwise it's unrepresentable */
   vassert((off & 7) == 0);  /* ditto */
   return ARM64AMode_RI12(hregARM64_X21(), off >> 3, 8/*scale*/);
}

/* Ditto, for 32 bit accesses. */
static ARM64AMode* mk_baseblock_32bit_access_amode ( UInt off )
{
   vassert(off < (4 << 12)); /* otherwise it's unrepresentable */
   vassert((off & 3) == 0);  /* ditto */
   return ARM64AMode_RI12(hregARM64_X21(), off >> 2, 4/*scale*/);
}

/* Ditto, for 16 bit accesses. */
static ARM64AMode* mk_baseblock_16bit_access_amode ( UInt off )
{
   vassert(off < (2 << 12)); /* otherwise it's unrepresentable */
   vassert((off & 1) == 0);  /* ditto */
   return ARM64AMode_RI12(hregARM64_X21(), off >> 1, 2/*scale*/);
}

/* Ditto, for 8 bit accesses. */
static ARM64AMode* mk_baseblock_8bit_access_amode ( UInt off )
{
   vassert(off < (1 << 12)); /* otherwise it's unrepresentable */
   return ARM64AMode_RI12(hregARM64_X21(), off >> 0, 1/*scale*/);
}

static HReg mk_baseblock_128bit_access_addr ( ISelEnv* env, UInt off )
{
   vassert(off < (1<<12));
   HReg r = newVRegI(env);
   addInstr(env, ARM64Instr_Arith(r, hregARM64_X21(),
                                     ARM64RIA_I12(off,0), True/*isAdd*/));
   return r;
}

static HReg get_baseblock_register ( void )
{
   return hregARM64_X21();
}

/* Generate code to zero extend a 32 bit value in 'src' to 64 bits, in
   a new register, and return the new register. */
static HReg widen_z_32_to_64 ( ISelEnv* env, HReg src )
{
   HReg      dst  = newVRegI(env);
   ARM64RIL* mask = ARM64RIL_I13(1, 0, 31); /* encodes 0xFFFFFFFF */
   addInstr(env, ARM64Instr_Logic(dst, src, mask, ARM64lo_AND));
   return dst;
}

/* Generate code to sign extend a 16 bit value in 'src' to 64 bits, in
   a new register, and return the new register. */
static HReg widen_s_16_to_64 ( ISelEnv* env, HReg src )
{
   HReg      dst = newVRegI(env);
   ARM64RI6* n48 = ARM64RI6_I6(48);
   addInstr(env, ARM64Instr_Shift(dst, src, n48, ARM64sh_SHL));
   addInstr(env, ARM64Instr_Shift(dst, dst, n48, ARM64sh_SAR));
   return dst;
}

/* Generate code to zero extend a 16 bit value in 'src' to 64 bits, in
   a new register, and return the new register. */
static HReg widen_z_16_to_64 ( ISelEnv* env, HReg src )
{
   HReg      dst = newVRegI(env);
   ARM64RI6* n48 = ARM64RI6_I6(48);
   addInstr(env, ARM64Instr_Shift(dst, src, n48, ARM64sh_SHL));
   addInstr(env, ARM64Instr_Shift(dst, dst, n48, ARM64sh_SHR));
   return dst;
}

/* Generate code to sign extend a 32 bit value in 'src' to 64 bits, in
   a new register, and return the new register. */
static HReg widen_s_32_to_64 ( ISelEnv* env, HReg src )
{
   HReg      dst = newVRegI(env);
   ARM64RI6* n32 = ARM64RI6_I6(32);
   addInstr(env, ARM64Instr_Shift(dst, src, n32, ARM64sh_SHL));
   addInstr(env, ARM64Instr_Shift(dst, dst, n32, ARM64sh_SAR));
   return dst;
}

/* Generate code to sign extend a 8 bit value in 'src' to 64 bits, in
   a new register, and return the new register. */
static HReg widen_s_8_to_64 ( ISelEnv* env, HReg src )
{
   HReg      dst = newVRegI(env);
   ARM64RI6* n56 = ARM64RI6_I6(56);
   addInstr(env, ARM64Instr_Shift(dst, src, n56, ARM64sh_SHL));
   addInstr(env, ARM64Instr_Shift(dst, dst, n56, ARM64sh_SAR));
   return dst;
}

static HReg widen_z_8_to_64 ( ISelEnv* env, HReg src )
{
   HReg      dst = newVRegI(env);
   ARM64RI6* n56 = ARM64RI6_I6(56);
   addInstr(env, ARM64Instr_Shift(dst, src, n56, ARM64sh_SHL));
   addInstr(env, ARM64Instr_Shift(dst, dst, n56, ARM64sh_SHR));
   return dst;
}

/* Is this IRExpr_Const(IRConst_U64(0)) ? */
static Bool isZeroU64 ( IRExpr* e ) {
   if (e->tag != Iex_Const) return False;
   IRConst* con = e->Iex.Const.con;
   vassert(con->tag == Ico_U64);
   return con->Ico.U64 == 0;
}


/*---------------------------------------------------------*/
/*--- ISEL: FP rounding mode helpers                    ---*/
/*---------------------------------------------------------*/

/* Set the FP rounding mode: 'mode' is an I32-typed expression
   denoting a value in the range 0 .. 3, indicating a round mode
   encoded as per type IRRoundingMode -- the first four values only
   (Irrm_NEAREST, Irrm_NegINF, Irrm_PosINF, Irrm_ZERO).  Set the ARM64
   FSCR to have the same rounding.

   For speed & simplicity, we're setting the *entire* FPCR here.

   Setting the rounding mode is expensive.  So this function tries to
   avoid repeatedly setting the rounding mode to the same thing by
   first comparing 'mode' to the 'mode' tree supplied in the previous
   call to this function, if any.  (The previous value is stored in
   env->previous_rm.)  If 'mode' is a single IR temporary 't' and
   env->previous_rm is also just 't', then the setting is skipped.

   This is safe because of the SSA property of IR: an IR temporary can
   only be defined once and so will have the same value regardless of
   where it appears in the block.  Cool stuff, SSA.

   A safety condition: all attempts to set the RM must be aware of
   this mechanism - by being routed through the functions here.

   Of course this only helps if blocks where the RM is set more than
   once and it is set to the same value each time, *and* that value is
   held in the same IR temporary each time.  In order to assure the
   latter as much as possible, the IR optimiser takes care to do CSE
   on any block with any sign of floating point activity.
*/
static
void set_FPCR_rounding_mode ( ISelEnv* env, IRExpr* mode )
{
   vassert(typeOfIRExpr(env->type_env,mode) == Ity_I32);
   
   /* Do we need to do anything? */
   if (env->previous_rm
       && env->previous_rm->tag == Iex_RdTmp
       && mode->tag == Iex_RdTmp
       && env->previous_rm->Iex.RdTmp.tmp == mode->Iex.RdTmp.tmp) {
      /* no - setting it to what it was before.  */
      vassert(typeOfIRExpr(env->type_env, env->previous_rm) == Ity_I32);
      return;
   }

   /* No luck - we better set it, and remember what we set it to. */
   env->previous_rm = mode;

   /* Only supporting the rounding-mode bits - the rest of FPCR is set
      to zero - so we can set the whole register at once (faster). */

   /* This isn't simple, because 'mode' carries an IR rounding
      encoding, and we need to translate that to an ARM64 FP one:
      The IR encoding:
         00  to nearest (the default)
         10  to +infinity
         01  to -infinity
         11  to zero
      The ARM64 FP encoding:
         00  to nearest
         01  to +infinity
         10  to -infinity
         11  to zero
      Easy enough to do; just swap the two bits.
   */
   HReg irrm = iselIntExpr_R(env, mode);
   HReg tL   = newVRegI(env);
   HReg tR   = newVRegI(env);
   HReg t3   = newVRegI(env);
   /* tL = irrm << 1;
      tR = irrm >> 1;  if we're lucky, these will issue together
      tL &= 2;
      tR &= 1;         ditto
      t3 = tL | tR;
      t3 <<= 22;
      fmxr fpscr, t3
   */
   ARM64RIL* ril_one = mb_mkARM64RIL_I(1);
   ARM64RIL* ril_two = mb_mkARM64RIL_I(2);
   vassert(ril_one && ril_two);
   addInstr(env, ARM64Instr_Shift(tL, irrm, ARM64RI6_I6(1), ARM64sh_SHL));
   addInstr(env, ARM64Instr_Shift(tR, irrm, ARM64RI6_I6(1), ARM64sh_SHR));
   addInstr(env, ARM64Instr_Logic(tL, tL, ril_two, ARM64lo_AND));
   addInstr(env, ARM64Instr_Logic(tR, tR, ril_one, ARM64lo_AND));
   addInstr(env, ARM64Instr_Logic(t3, tL, ARM64RIL_R(tR), ARM64lo_OR));
   addInstr(env, ARM64Instr_Shift(t3, t3, ARM64RI6_I6(22), ARM64sh_SHL));
   addInstr(env, ARM64Instr_FPCR(True/*toFPCR*/, t3));
}


/*---------------------------------------------------------*/
/*--- ISEL: Function call helpers                       ---*/
/*---------------------------------------------------------*/

/* Used only in doHelperCall.  See big comment in doHelperCall re
   handling of register-parameter args.  This function figures out
   whether evaluation of an expression might require use of a fixed
   register.  If in doubt return True (safe but suboptimal).
*/
static
Bool mightRequireFixedRegs ( IRExpr* e )
{
   if (UNLIKELY(is_IRExpr_VECRET_or_BBPTR(e))) {
      // These are always "safe" -- either a copy of SP in some
      // arbitrary vreg, or a copy of x21, respectively.
      return False;
   }
   /* Else it's a "normal" expression. */
   switch (e->tag) {
      case Iex_RdTmp: case Iex_Const: case Iex_Get:
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
   after the call is done.  Returns True iff it managed to handle this
   combination of arg/return types, else returns False. */

static
Bool doHelperCall ( /*OUT*/UInt*   stackAdjustAfterCall,
                    /*OUT*/RetLoc* retloc,
                    ISelEnv* env,
                    IRExpr* guard,
                    IRCallee* cee, IRType retTy, IRExpr** args )
{
   ARM64CondCode cc;
   HReg          argregs[ARM64_N_ARGREGS];
   HReg          tmpregs[ARM64_N_ARGREGS];
   Bool          go_fast;
   Int           n_args, i, nextArgReg;
   Addr64        target;

   vassert(ARM64_N_ARGREGS == 8);

   /* Set default returns.  We'll update them later if needed. */
   *stackAdjustAfterCall = 0;
   *retloc               = mk_RetLoc_INVALID();

   /* These are used for cross-checking that IR-level constraints on
      the use of IRExpr_VECRET() and IRExpr_BBPTR() are observed. */
   UInt nVECRETs = 0;
   UInt nBBPTRs  = 0;

   /* Marshal args for a call and do the call.

      This function only deals with a tiny set of possibilities, which
      cover all helpers in practice.  The restrictions are that only
      arguments in registers are supported, hence only
      ARM64_N_REGPARMS x 64 integer bits in total can be passed.  In
      fact the only supported arg type is I64.

      The return type can be I{64,32} or V128.  In the V128 case, it
      is expected that |args| will contain the special node
      IRExpr_VECRET(), in which case this routine generates code to
      allocate space on the stack for the vector return value.  Since
      we are not passing any scalars on the stack, it is enough to
      preallocate the return space before marshalling any arguments,
      in this case.

      |args| may also contain IRExpr_BBPTR(), in which case the
      value in x21 is passed as the corresponding argument.

      Generating code which is both efficient and correct when
      parameters are to be passed in registers is difficult, for the
      reasons elaborated in detail in comments attached to
      doHelperCall() in priv/host-x86/isel.c.  Here, we use a variant
      of the method described in those comments.

      The problem is split into two cases: the fast scheme and the
      slow scheme.  In the fast scheme, arguments are computed
      directly into the target (real) registers.  This is only safe
      when we can be sure that computation of each argument will not
      trash any real registers set by computation of any other
      argument.

      In the slow scheme, all args are first computed into vregs, and
      once they are all done, they are moved to the relevant real
      regs.  This always gives correct code, but it also gives a bunch
      of vreg-to-rreg moves which are usually redundant but are hard
      for the register allocator to get rid of.

      To decide which scheme to use, all argument expressions are
      first examined.  If they are all so simple that it is clear they
      will be evaluated without use of any fixed registers, use the
      fast scheme, else use the slow scheme.  Note also that only
      unconditional calls may use the fast scheme, since having to
      compute a condition expression could itself trash real
      registers.

      Note this requires being able to examine an expression and
      determine whether or not evaluation of it might use a fixed
      register.  That requires knowledge of how the rest of this insn
      selector works.  Currently just the following 3 are regarded as
      safe -- hopefully they cover the majority of arguments in
      practice: IRExpr_Tmp IRExpr_Const IRExpr_Get.
   */

   /* Note that the cee->regparms field is meaningless on ARM64 hosts
      (since there is only one calling convention) and so we always
      ignore it. */

   n_args = 0;
   for (i = 0; args[i]; i++) {
      IRExpr* arg = args[i];
      if (UNLIKELY(arg->tag == Iex_VECRET)) {
         nVECRETs++;
      } else if (UNLIKELY(arg->tag == Iex_BBPTR)) {
         nBBPTRs++;
      }
      n_args++;
   }

   /* If this fails, the IR is ill-formed */
   vassert(nBBPTRs == 0 || nBBPTRs == 1);

   /* If we have a VECRET, allocate space on the stack for the return
      value, and record the stack pointer after that. */
   HReg r_vecRetAddr = INVALID_HREG;
   if (nVECRETs == 1) {
      vassert(retTy == Ity_V128 || retTy == Ity_V256);
      vassert(retTy != Ity_V256); // we don't handle that yet (if ever)
      r_vecRetAddr = newVRegI(env);
      addInstr(env, ARM64Instr_AddToSP(-16));
      addInstr(env, ARM64Instr_FromSP(r_vecRetAddr));
   } else {
      // If either of these fail, the IR is ill-formed
      vassert(retTy != Ity_V128 && retTy != Ity_V256);
      vassert(nVECRETs == 0);
   }

   argregs[0] = hregARM64_X0();
   argregs[1] = hregARM64_X1();
   argregs[2] = hregARM64_X2();
   argregs[3] = hregARM64_X3();
   argregs[4] = hregARM64_X4();
   argregs[5] = hregARM64_X5();
   argregs[6] = hregARM64_X6();
   argregs[7] = hregARM64_X7();

   tmpregs[0] = tmpregs[1] = tmpregs[2] = tmpregs[3] = INVALID_HREG;
   tmpregs[4] = tmpregs[5] = tmpregs[6] = tmpregs[7] = INVALID_HREG;

   /* First decide which scheme (slow or fast) is to be used.  First
      assume the fast scheme, and select slow if any contraindications
      (wow) appear. */

   go_fast = True;

   if (guard) {
      if (guard->tag == Iex_Const
          && guard->Iex.Const.con->tag == Ico_U1
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

   if (go_fast) {
      if (retTy == Ity_V128 || retTy == Ity_V256)
         go_fast = False;
   }

   /* At this point the scheme to use has been established.  Generate
      code to get the arg values into the argument rregs.  If we run
      out of arg regs, give up. */

   if (go_fast) {

      /* FAST SCHEME */
      nextArgReg = 0;

      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];

         IRType  aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, args[i]);

         if (nextArgReg >= ARM64_N_ARGREGS)
            return False; /* out of argregs */

         if (aTy == Ity_I64) {
            addInstr(env, ARM64Instr_MovI( argregs[nextArgReg],
                                           iselIntExpr_R(env, args[i]) ));
            nextArgReg++;
         }
         else if (arg->tag == Iex_BBPTR) {
            vassert(0); //ATC
            addInstr(env, ARM64Instr_MovI( argregs[nextArgReg],
                                           hregARM64_X21() ));
            nextArgReg++;
         }
         else if (arg->tag == Iex_VECRET) {
            // because of the go_fast logic above, we can't get here,
            // since vector return values makes us use the slow path
            // instead.
            vassert(0);
         }
         else
            return False; /* unhandled arg type */
      }

      /* Fast scheme only applies for unconditional calls.  Hence: */
      cc = ARM64cc_AL;

   } else {

      /* SLOW SCHEME; move via temporaries */
      nextArgReg = 0;

      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];

         IRType  aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, args[i]);

         if (nextArgReg >= ARM64_N_ARGREGS)
            return False; /* out of argregs */

         if (aTy == Ity_I64) {
            tmpregs[nextArgReg] = iselIntExpr_R(env, args[i]);
            nextArgReg++;
         }
         else if (arg->tag == Iex_BBPTR) {
            vassert(0); //ATC
            tmpregs[nextArgReg] = hregARM64_X21();
            nextArgReg++;
         }
         else if (arg->tag == Iex_VECRET) {
            vassert(!hregIsInvalid(r_vecRetAddr));
            tmpregs[nextArgReg] = r_vecRetAddr;
            nextArgReg++;
         }
         else
            return False; /* unhandled arg type */
      }

      /* Now we can compute the condition.  We can't do it earlier
         because the argument computations could trash the condition
         codes.  Be a bit clever to handle the common case where the
         guard is 1:Bit. */
      cc = ARM64cc_AL;
      if (guard) {
         if (guard->tag == Iex_Const
             && guard->Iex.Const.con->tag == Ico_U1
             && guard->Iex.Const.con->Ico.U1 == True) {
            /* unconditional -- do nothing */
         } else {
            cc = iselCondCode( env, guard );
         }
      }

      /* Move the args to their final destinations. */
      for (i = 0; i < nextArgReg; i++) {
         vassert(!(hregIsInvalid(tmpregs[i])));
         /* None of these insns, including any spill code that might
            be generated, may alter the condition codes. */
         addInstr( env, ARM64Instr_MovI( argregs[i], tmpregs[i] ) );
      }

   }

   /* Should be assured by checks above */
   vassert(nextArgReg <= ARM64_N_ARGREGS);

   /* Do final checks, set the return values, and generate the call
      instruction proper. */
   vassert(nBBPTRs == 0 || nBBPTRs == 1);
   vassert(nVECRETs == (retTy == Ity_V128 || retTy == Ity_V256) ? 1 : 0);
   vassert(*stackAdjustAfterCall == 0);
   vassert(is_RetLoc_INVALID(*retloc));
   switch (retTy) {
      case Ity_INVALID:
         /* Function doesn't return a value. */
         *retloc = mk_RetLoc_simple(RLPri_None);
         break;
      case Ity_I64: case Ity_I32: case Ity_I16: case Ity_I8:
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

   /* Finally, generate the call itself.  This needs the *retloc value
      set in the switch above, which is why it's at the end. */

   /* nextArgReg doles out argument registers.  Since these are
      assigned in the order x0 .. x7, its numeric value at this point,
      which must be between 0 and 8 inclusive, is going to be equal to
      the number of arg regs in use for the call.  Hence bake that
      number into the call (we'll need to know it when doing register
      allocation, to know what regs the call reads.) */

   target = (Addr)cee->addr;
   addInstr(env, ARM64Instr_Call( cc, target, nextArgReg, *retloc ));

   return True; /* success */
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64/32 bit)             ---*/
/*---------------------------------------------------------*/

/* Select insns for an integer-typed expression, and add them to the
   code list.  Return a reg holding the result.  This reg will be a
   virtual register.  THE RETURNED REG MUST NOT BE MODIFIED.  If you
   want to modify it, ask for a new vreg, copy it in there, and modify
   the copy.  The register allocator will do its best to map both
   vregs to the same real register, so the copies will often disappear
   later in the game.

   This should handle expressions of 64- and 32-bit type.  All results
   are returned in a 64-bit register.  For 32-bit expressions, the
   upper 32 bits are arbitrary, so you should mask or sign extend
   partial values if necessary.
*/

/* --------------------- AMode --------------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a 64-bit one.
*/

static Bool isValidScale ( UChar scale )
{
   switch (scale) {
      case 1: case 2: case 4: case 8: /* case 16: ??*/ return True;
      default: return False;
   }
}

static Bool sane_AMode ( ARM64AMode* am )
{
   switch (am->tag) {
      case ARM64am_RI9:
         return
            toBool( hregClass(am->ARM64am.RI9.reg) == HRcInt64
                    && (hregIsVirtual(am->ARM64am.RI9.reg)
                        /* || sameHReg(am->ARM64am.RI9.reg, 
                                       hregARM64_X21()) */ )
                    && am->ARM64am.RI9.simm9 >= -256
                    && am->ARM64am.RI9.simm9 <= 255 );
      case ARM64am_RI12:
         return
            toBool( hregClass(am->ARM64am.RI12.reg) == HRcInt64
                    && (hregIsVirtual(am->ARM64am.RI12.reg)
                        /* || sameHReg(am->ARM64am.RI12.reg, 
                                       hregARM64_X21()) */ )
                    && am->ARM64am.RI12.uimm12 < 4096
                    && isValidScale(am->ARM64am.RI12.szB) );
      case ARM64am_RR:
         return
            toBool( hregClass(am->ARM64am.RR.base) == HRcInt64
                    && hregIsVirtual(am->ARM64am.RR.base)
                    && hregClass(am->ARM64am.RR.index) == HRcInt64
                    && hregIsVirtual(am->ARM64am.RR.index) );
      default:
         vpanic("sane_AMode: unknown ARM64 AMode1 tag");
   }
}

static
ARM64AMode* iselIntExpr_AMode ( ISelEnv* env, IRExpr* e, IRType dty )
{
   ARM64AMode* am = iselIntExpr_AMode_wrk(env, e, dty);
   vassert(sane_AMode(am));
   return am;
}

static
ARM64AMode* iselIntExpr_AMode_wrk ( ISelEnv* env, IRExpr* e, IRType dty )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64);

   ULong szBbits = 0;
   switch (dty) {
      case Ity_I64: szBbits = 3; break;
      case Ity_I32: szBbits = 2; break;
      case Ity_I16: szBbits = 1; break;
      case Ity_I8:  szBbits = 0; break;
      default: vassert(0);
   }

   /* {Add64,Sub64}(expr,simm9).  We don't care about |dty| here since
      we're going to create an amode suitable for LDU* or STU*
      instructions, which use unscaled immediate offsets.  */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_Add64 || e->Iex.Binop.op == Iop_Sub64)
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U64) {
      Long simm = (Long)e->Iex.Binop.arg2->Iex.Const.con->Ico.U64;
      if (simm >= -255 && simm <= 255) {
         /* Although the gating condition might seem to be 
               simm >= -256 && simm <= 255
            we will need to negate simm in the case where the op is Sub64.
            Hence limit the lower value to -255 in order that its negation
            is representable. */
         HReg reg = iselIntExpr_R(env, e->Iex.Binop.arg1);
         if (e->Iex.Binop.op == Iop_Sub64) simm = -simm;
         return ARM64AMode_RI9(reg, (Int)simm);
      }
   }

   /* Add64(expr, uimm12 * transfer-size) */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Add64
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U64) {
      ULong uimm = e->Iex.Binop.arg2->Iex.Const.con->Ico.U64;
      ULong szB  = 1 << szBbits;
      if (0 == (uimm & (szB-1)) /* "uimm is szB-aligned" */
          && (uimm >> szBbits) < 4096) {
         HReg reg = iselIntExpr_R(env, e->Iex.Binop.arg1);
         return ARM64AMode_RI12(reg, (UInt)(uimm >> szBbits), (UChar)szB);
      }
   }

   /* Add64(expr1, expr2) */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Add64) {
      HReg reg1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
      HReg reg2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
      return ARM64AMode_RR(reg1, reg2);
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   HReg reg = iselIntExpr_R(env, e);
   return ARM64AMode_RI9(reg, 0);
}


/* --------------------- RIA --------------------- */

/* Select instructions to generate 'e' into a RIA. */

static ARM64RIA* iselIntExpr_RIA ( ISelEnv* env, IRExpr* e )
{
   ARM64RIA* ri = iselIntExpr_RIA_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case ARM64riA_I12:
         vassert(ri->ARM64riA.I12.imm12 < 4096);
         vassert(ri->ARM64riA.I12.shift == 0 || ri->ARM64riA.I12.shift == 12);
         return ri;
      case ARM64riA_R:
         vassert(hregClass(ri->ARM64riA.R.reg) == HRcInt64);
         vassert(hregIsVirtual(ri->ARM64riA.R.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RIA: unknown arm RIA tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARM64RIA* iselIntExpr_RIA_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64 || ty == Ity_I32);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      ULong u = 0xF000000ULL; /* invalid */
      switch (e->Iex.Const.con->tag) {
         case Ico_U64: u = e->Iex.Const.con->Ico.U64; break;
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         default: vpanic("iselIntExpr_RIA.Iex_Const(arm64)");
      }
      if (0 == (u & ~(0xFFFULL << 0)))
         return ARM64RIA_I12((UShort)((u >> 0) & 0xFFFULL), 0);
      if (0 == (u & ~(0xFFFULL << 12)))
         return ARM64RIA_I12((UShort)((u >> 12) & 0xFFFULL), 12);
      /* else fail, fall through to default case */
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return ARM64RIA_R(r);
   }
}


/* --------------------- RIL --------------------- */

/* Select instructions to generate 'e' into a RIL.  At this point we
   have to deal with the strange bitfield-immediate encoding for logic
   instructions. */


// The following four functions
//    CountLeadingZeros CountTrailingZeros CountSetBits isImmLogical
// are copied, with modifications, from
// https://github.com/armvixl/vixl/blob/master/src/a64/assembler-a64.cc
// which has the following copyright notice:
/*
   Copyright 2013, ARM Limited
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:
   
   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.
   * Neither the name of ARM Limited nor the names of its contributors may be
     used to endorse or promote products derived from this software without
     specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS CONTRIBUTORS "AS IS" AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

static Int CountLeadingZeros(ULong value, Int width)
{
   vassert(width == 32 || width == 64);
   Int count = 0;
   ULong bit_test = 1ULL << (width - 1);
   while ((count < width) && ((bit_test & value) == 0)) {
      count++;
      bit_test >>= 1;
   }
   return count;
}

static Int CountTrailingZeros(ULong value, Int width)
{
   vassert(width == 32 || width == 64);
   Int count = 0;
   while ((count < width) && (((value >> count) & 1) == 0)) {
      count++;
   }
   return count;
}

static Int CountSetBits(ULong value, Int width)
{
   // TODO: Other widths could be added here, as the implementation already
   // supports them.
   vassert(width == 32 || width == 64);

   // Mask out unused bits to ensure that they are not counted.
   value &= (0xffffffffffffffffULL >> (64-width));

   // Add up the set bits.
   // The algorithm works by adding pairs of bit fields together iteratively,
   // where the size of each bit field doubles each time.
   // An example for an 8-bit value:
   // Bits: h g f e d c b a
   // \ | \ | \ | \ |
   // value = h+g f+e d+c b+a
   // \ | \ |
   // value = h+g+f+e d+c+b+a
   // \ |
   // value = h+g+f+e+d+c+b+a
   value = ((value >>  1) & 0x5555555555555555ULL)
                 + (value & 0x5555555555555555ULL);
   value = ((value >>  2) & 0x3333333333333333ULL)
                 + (value & 0x3333333333333333ULL);
   value = ((value >>  4) & 0x0f0f0f0f0f0f0f0fULL)
                 + (value & 0x0f0f0f0f0f0f0f0fULL);
   value = ((value >>  8) & 0x00ff00ff00ff00ffULL)
                 + (value & 0x00ff00ff00ff00ffULL);
   value = ((value >> 16) & 0x0000ffff0000ffffULL)
                 + (value & 0x0000ffff0000ffffULL);
   value = ((value >> 32) & 0x00000000ffffffffULL)
                 + (value & 0x00000000ffffffffULL);

   return value;
}

static Bool isImmLogical ( /*OUT*/UInt* n,
                           /*OUT*/UInt* imm_s, /*OUT*/UInt* imm_r,
                           ULong value, UInt width )
{
  // Test if a given value can be encoded in the immediate field of a
  // logical instruction.

  // If it can be encoded, the function returns true, and values
  // pointed to by n, imm_s and imm_r are updated with immediates
  // encoded in the format required by the corresponding fields in the
  // logical instruction.  If it can not be encoded, the function
  // returns false, and the values pointed to by n, imm_s and imm_r
  // are undefined.
  vassert(n != NULL && imm_s != NULL && imm_r != NULL);
  vassert(width == 32 || width == 64);

  // Logical immediates are encoded using parameters n, imm_s and imm_r using
  // the following table:
  //
  // N imms immr size S R
  // 1 ssssss rrrrrr 64 UInt(ssssss) UInt(rrrrrr)
  // 0 0sssss xrrrrr 32 UInt(sssss) UInt(rrrrr)
  // 0 10ssss xxrrrr 16 UInt(ssss) UInt(rrrr)
  // 0 110sss xxxrrr 8 UInt(sss) UInt(rrr)
  // 0 1110ss xxxxrr 4 UInt(ss) UInt(rr)
  // 0 11110s xxxxxr 2 UInt(s) UInt(r)
  // (s bits must not be all set)
  //
  // A pattern is constructed of size bits, where the least significant S+1
  // bits are set. The pattern is rotated right by R, and repeated across a
  // 32 or 64-bit value, depending on destination register width.
  //
  // To test if an arbitrary immediate can be encoded using this scheme, an
  // iterative algorithm is used.
  //
  // TODO: This code does not consider using X/W register overlap to support
  // 64-bit immediates where the top 32-bits are zero, and the bottom 32-bits
  // are an encodable logical immediate.

  // 1. If the value has all set or all clear bits, it can't be encoded.
  if ((value == 0) || (value == 0xffffffffffffffffULL) ||
      ((width == 32) && (value == 0xffffffff))) {
    return False;
  }

  UInt lead_zero = CountLeadingZeros(value, width);
  UInt lead_one = CountLeadingZeros(~value, width);
  UInt trail_zero = CountTrailingZeros(value, width);
  UInt trail_one = CountTrailingZeros(~value, width);
  UInt set_bits = CountSetBits(value, width);

  // The fixed bits in the immediate s field.
  // If width == 64 (X reg), start at 0xFFFFFF80.
  // If width == 32 (W reg), start at 0xFFFFFFC0, as the iteration for 64-bit
  // widths won't be executed.
  Int imm_s_fixed = (width == 64) ? -128 : -64;
  Int imm_s_mask = 0x3F;

  for (;;) {
    // 2. If the value is two bits wide, it can be encoded.
    if (width == 2) {
      *n = 0;
      *imm_s = 0x3C;
      *imm_r = (value & 3) - 1;
      return True;
    }

    *n = (width == 64) ? 1 : 0;
    *imm_s = ((imm_s_fixed | (set_bits - 1)) & imm_s_mask);
    if ((lead_zero + set_bits) == width) {
      *imm_r = 0;
    } else {
      *imm_r = (lead_zero > 0) ? (width - trail_zero) : lead_one;
    }

    // 3. If the sum of leading zeros, trailing zeros and set bits is equal to
    // the bit width of the value, it can be encoded.
    if (lead_zero + trail_zero + set_bits == width) {
      return True;
    }

    // 4. If the sum of leading ones, trailing ones and unset bits in the
    // value is equal to the bit width of the value, it can be encoded.
    if (lead_one + trail_one + (width - set_bits) == width) {
      return True;
    }

    // 5. If the most-significant half of the bitwise value is equal to the
    // least-significant half, return to step 2 using the least-significant
    // half of the value.
    ULong mask = (1ULL << (width >> 1)) - 1;
    if ((value & mask) == ((value >> (width >> 1)) & mask)) {
      width >>= 1;
      set_bits >>= 1;
      imm_s_fixed >>= 1;
      continue;
    }

    // 6. Otherwise, the value can't be encoded.
    return False;
  }
}


/* Create a RIL for the given immediate, if it is representable, or
   return NULL if not. */

static ARM64RIL* mb_mkARM64RIL_I ( ULong imm64 )
{
   UInt n = 0, imm_s = 0, imm_r = 0;
   Bool ok = isImmLogical(&n, &imm_s, &imm_r, imm64, 64);
   if (!ok) return NULL;
   vassert(n < 2 && imm_s < 64 && imm_r < 64);
   return ARM64RIL_I13(n, imm_r, imm_s);
}

/* So, finally .. */

static ARM64RIL* iselIntExpr_RIL ( ISelEnv* env, IRExpr* e )
{
   ARM64RIL* ri = iselIntExpr_RIL_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case ARM64riL_I13:
         vassert(ri->ARM64riL.I13.bitN < 2);
         vassert(ri->ARM64riL.I13.immR < 64);
         vassert(ri->ARM64riL.I13.immS < 64);
         return ri;
      case ARM64riL_R:
         vassert(hregClass(ri->ARM64riL.R.reg) == HRcInt64);
         vassert(hregIsVirtual(ri->ARM64riL.R.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RIL: unknown arm RIL tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARM64RIL* iselIntExpr_RIL_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64 || ty == Ity_I32);
   
   /* special case: immediate */
   if (e->tag == Iex_Const) {
      ARM64RIL* maybe = NULL;
      if (ty == Ity_I64) {
         vassert(e->Iex.Const.con->tag == Ico_U64);
         maybe = mb_mkARM64RIL_I(e->Iex.Const.con->Ico.U64);
      } else {
         vassert(ty == Ity_I32);
         vassert(e->Iex.Const.con->tag == Ico_U32);
         UInt  u32 = e->Iex.Const.con->Ico.U32;
         ULong u64 = (ULong)u32;
         /* First try with 32 leading zeroes. */
         maybe = mb_mkARM64RIL_I(u64);
         /* If that doesn't work, try with 2 copies, since it doesn't
            matter what winds up in the upper 32 bits. */
         if (!maybe) {
            maybe = mb_mkARM64RIL_I((u64 << 32) | u64);
         }
      }
      if (maybe) return maybe;
      /* else fail, fall through to default case */
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return ARM64RIL_R(r);
   }
}


/* --------------------- RI6 --------------------- */

/* Select instructions to generate 'e' into a RI6. */

static ARM64RI6* iselIntExpr_RI6 ( ISelEnv* env, IRExpr* e )
{
   ARM64RI6* ri = iselIntExpr_RI6_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case ARM64ri6_I6:
         vassert(ri->ARM64ri6.I6.imm6 < 64);
         vassert(ri->ARM64ri6.I6.imm6 > 0);
         return ri;
      case ARM64ri6_R:
         vassert(hregClass(ri->ARM64ri6.R.reg) == HRcInt64);
         vassert(hregIsVirtual(ri->ARM64ri6.R.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RI6: unknown arm RI6 tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARM64RI6* iselIntExpr_RI6_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64 || ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      switch (e->Iex.Const.con->tag) {
         case Ico_U8: {
            UInt u = e->Iex.Const.con->Ico.U8;
            if (u > 0 && u < 64)
              return ARM64RI6_I6(u);
            break;
         default:
            break;
         }
      }
      /* else fail, fall through to default case */
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return ARM64RI6_R(r);
   }
}


/* ------------------- CondCode ------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static ARM64CondCode iselCondCode ( ISelEnv* env, IRExpr* e )
{
   ARM64CondCode cc = iselCondCode_wrk(env,e);
   vassert(cc != ARM64cc_NV);
   return cc;
}

static ARM64CondCode iselCondCode_wrk ( ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I1);

   /* var */
   if (e->tag == Iex_RdTmp) {
      HReg rTmp = lookupIRTemp(env, e->Iex.RdTmp.tmp);
      /* Cmp doesn't modify rTmp; so this is OK. */
      ARM64RIL* one = mb_mkARM64RIL_I(1);
      vassert(one);
      addInstr(env, ARM64Instr_Test(rTmp, one));
      return ARM64cc_NE;
   }

   /* Not1(e) */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_Not1) {
      /* Generate code for the arg, and negate the test condition */
      ARM64CondCode cc = iselCondCode(env, e->Iex.Unop.arg);
      if (cc == ARM64cc_AL || cc == ARM64cc_NV) {
        return ARM64cc_AL;
      } else {
        return 1 ^ cc;
      }
   }

   /* --- patterns rooted at: 64to1 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_64to1) {
      HReg      rTmp = iselIntExpr_R(env, e->Iex.Unop.arg);
      ARM64RIL* one  = mb_mkARM64RIL_I(1);
      vassert(one); /* '1' must be representable */
      addInstr(env, ARM64Instr_Test(rTmp, one));
      return ARM64cc_NE;
   }

   /* --- patterns rooted at: CmpNEZ8 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ8) {
      HReg      r1  = iselIntExpr_R(env, e->Iex.Unop.arg);
      ARM64RIL* xFF = mb_mkARM64RIL_I(0xFF);
      addInstr(env, ARM64Instr_Test(r1, xFF));
      return ARM64cc_NE;
   }

   /* --- patterns rooted at: CmpNEZ16 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ16) {
      HReg      r1    = iselIntExpr_R(env, e->Iex.Unop.arg);
      ARM64RIL* xFFFF = mb_mkARM64RIL_I(0xFFFF);
      addInstr(env, ARM64Instr_Test(r1, xFFFF));
      return ARM64cc_NE;
   }

   /* --- patterns rooted at: CmpNEZ64 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ64) {
      HReg      r1   = iselIntExpr_R(env, e->Iex.Unop.arg);
      ARM64RIA* zero = ARM64RIA_I12(0,0);
      addInstr(env, ARM64Instr_Cmp(r1, zero, True/*is64*/));
      return ARM64cc_NE;
   }

   /* --- patterns rooted at: CmpNEZ32 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ32) {
      HReg      r1   = iselIntExpr_R(env, e->Iex.Unop.arg);
      ARM64RIA* zero = ARM64RIA_I12(0,0);
      addInstr(env, ARM64Instr_Cmp(r1, zero, False/*!is64*/));
      return ARM64cc_NE;
   }

   /* --- Cmp*64*(x,y) --- */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_CmpEQ64
           || e->Iex.Binop.op == Iop_CmpNE64
           || e->Iex.Binop.op == Iop_CmpLT64S
           || e->Iex.Binop.op == Iop_CmpLT64U
           || e->Iex.Binop.op == Iop_CmpLE64S
           || e->Iex.Binop.op == Iop_CmpLE64U)) {
      HReg      argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
      ARM64RIA* argR = iselIntExpr_RIA(env, e->Iex.Binop.arg2);
      addInstr(env, ARM64Instr_Cmp(argL, argR, True/*is64*/));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ64:  return ARM64cc_EQ;
         case Iop_CmpNE64:  return ARM64cc_NE;
         case Iop_CmpLT64S: return ARM64cc_LT;
         case Iop_CmpLT64U: return ARM64cc_CC;
         case Iop_CmpLE64S: return ARM64cc_LE;
         case Iop_CmpLE64U: return ARM64cc_LS;
         default: vpanic("iselCondCode(arm64): CmpXX64");
      }
   }

   /* --- Cmp*32*(x,y) --- */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_CmpEQ32
           || e->Iex.Binop.op == Iop_CmpNE32
           || e->Iex.Binop.op == Iop_CmpLT32S
           || e->Iex.Binop.op == Iop_CmpLT32U
           || e->Iex.Binop.op == Iop_CmpLE32S
           || e->Iex.Binop.op == Iop_CmpLE32U)) {
      HReg      argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
      ARM64RIA* argR = iselIntExpr_RIA(env, e->Iex.Binop.arg2);
      addInstr(env, ARM64Instr_Cmp(argL, argR, False/*!is64*/));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ32:  return ARM64cc_EQ;
         case Iop_CmpNE32:  return ARM64cc_NE;
         case Iop_CmpLT32S: return ARM64cc_LT;
         case Iop_CmpLT32U: return ARM64cc_CC;
         case Iop_CmpLE32S: return ARM64cc_LE;
         case Iop_CmpLE32U: return ARM64cc_LS;
         default: vpanic("iselCondCode(arm64): CmpXX32");
      }
   }

   ppIRExpr(e);
   vpanic("iselCondCode");
}


/* --------------------- Reg --------------------- */

static HReg iselIntExpr_R ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselIntExpr_R_wrk(env, e);
   /* sanity checks ... */
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcInt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselIntExpr_R_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64 || ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   switch (e->tag) {

   /* --------- TEMP --------- */
   case Iex_RdTmp: {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   /* --------- LOAD --------- */
   case Iex_Load: {
      HReg dst  = newVRegI(env);

      if (e->Iex.Load.end != Iend_LE)
         goto irreducible;

      if (ty == Ity_I64) {
         ARM64AMode* amode = iselIntExpr_AMode ( env, e->Iex.Load.addr, ty );
         addInstr(env, ARM64Instr_LdSt64(True/*isLoad*/, dst, amode));
         return dst;
      }
      if (ty == Ity_I32) {
         ARM64AMode* amode = iselIntExpr_AMode ( env, e->Iex.Load.addr, ty );
         addInstr(env, ARM64Instr_LdSt32(True/*isLoad*/, dst, amode));
         return dst;
      }
      if (ty == Ity_I16) {
         ARM64AMode* amode = iselIntExpr_AMode ( env, e->Iex.Load.addr, ty );
         addInstr(env, ARM64Instr_LdSt16(True/*isLoad*/, dst, amode));
         return dst;
      }
      if (ty == Ity_I8) {
         ARM64AMode* amode = iselIntExpr_AMode ( env, e->Iex.Load.addr, ty );
         addInstr(env, ARM64Instr_LdSt8(True/*isLoad*/, dst, amode));
         return dst;
      }
      break;
   }

   /* --------- BINARY OP --------- */
   case Iex_Binop: {

      ARM64LogicOp lop = 0; /* invalid */
      ARM64ShiftOp sop = 0; /* invalid */

      /* Special-case 0-x into a Neg instruction.  Not because it's
         particularly useful but more so as to give value flow using
         this instruction, so as to check its assembly correctness for
         implementation of Left32/Left64. */
      switch (e->Iex.Binop.op) {
         case Iop_Sub64:
            if (isZeroU64(e->Iex.Binop.arg1)) {
               HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
               HReg dst  = newVRegI(env);
               addInstr(env, ARM64Instr_Unary(dst, argR, ARM64un_NEG));
               return dst;
            }
            break;
         default:
            break;
      }

      /* ADD/SUB */
      switch (e->Iex.Binop.op) {
         case Iop_Add64: case Iop_Add32:
         case Iop_Sub64: case Iop_Sub32: {
            Bool      isAdd = e->Iex.Binop.op == Iop_Add64
                              || e->Iex.Binop.op == Iop_Add32;
            HReg      dst   = newVRegI(env);
            HReg      argL  = iselIntExpr_R(env, e->Iex.Binop.arg1);
            ARM64RIA* argR  = iselIntExpr_RIA(env, e->Iex.Binop.arg2);
            addInstr(env, ARM64Instr_Arith(dst, argL, argR, isAdd));
            return dst;
         }
         default:
            break;
      }

      /* AND/OR/XOR */
      switch (e->Iex.Binop.op) {
         case Iop_And64: case Iop_And32: lop = ARM64lo_AND; goto log_binop;
         case Iop_Or64:  case Iop_Or32:  lop = ARM64lo_OR;  goto log_binop;
         case Iop_Xor64: case Iop_Xor32: lop = ARM64lo_XOR; goto log_binop;
         log_binop: {
            HReg      dst  = newVRegI(env);
            HReg      argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
            ARM64RIL* argR = iselIntExpr_RIL(env, e->Iex.Binop.arg2);
            addInstr(env, ARM64Instr_Logic(dst, argL, argR, lop));
            return dst;
         }
         default:
            break;
      }

      /* SHL/SHR/SAR */
      switch (e->Iex.Binop.op) {
         case Iop_Shr64:                 sop = ARM64sh_SHR; goto sh_binop;
         case Iop_Sar64:                 sop = ARM64sh_SAR; goto sh_binop;
         case Iop_Shl64: case Iop_Shl32: sop = ARM64sh_SHL; goto sh_binop;
         sh_binop: {
            HReg      dst  = newVRegI(env);
            HReg      argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
            ARM64RI6* argR = iselIntExpr_RI6(env, e->Iex.Binop.arg2);
            addInstr(env, ARM64Instr_Shift(dst, argL, argR, sop));
            return dst;
         }
         case Iop_Shr32:
         case Iop_Sar32: {
            Bool      zx   = e->Iex.Binop.op == Iop_Shr32;
            HReg      argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
            ARM64RI6* argR = iselIntExpr_RI6(env, e->Iex.Binop.arg2);
            HReg      dst  = zx ? widen_z_32_to_64(env, argL)
                                : widen_s_32_to_64(env, argL);
            addInstr(env, ARM64Instr_Shift(dst, dst, argR, ARM64sh_SHR));
            return dst;
         }
         default: break;
      }

      /* MUL */
      if (e->Iex.Binop.op == Iop_Mul64 || e->Iex.Binop.op == Iop_Mul32) {
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegI(env);
         addInstr(env, ARM64Instr_Mul(dst, argL, argR, ARM64mul_PLAIN));
         return dst;
      }

      /* MULL */
      if (e->Iex.Binop.op == Iop_MullU32 || e->Iex.Binop.op == Iop_MullS32) {
         Bool isS  = e->Iex.Binop.op == Iop_MullS32;
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg extL = (isS ? widen_s_32_to_64 : widen_z_32_to_64)(env, argL);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg extR = (isS ? widen_s_32_to_64 : widen_z_32_to_64)(env, argR);
         HReg dst  = newVRegI(env);
         addInstr(env, ARM64Instr_Mul(dst, extL, extR, ARM64mul_PLAIN));
         return dst;
      }

      /* Handle misc other ops. */

      if (e->Iex.Binop.op == Iop_Max32U) {
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegI(env);
         addInstr(env, ARM64Instr_Cmp(argL, ARM64RIA_R(argR), False/*!is64*/));
         addInstr(env, ARM64Instr_CSel(dst, argL, argR, ARM64cc_CS));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_32HLto64) {
         HReg hi32s = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg lo32s = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg lo32  = widen_z_32_to_64(env, lo32s);
         HReg hi32  = newVRegI(env);
         addInstr(env, ARM64Instr_Shift(hi32, hi32s, ARM64RI6_I6(32),
                                        ARM64sh_SHL));
         addInstr(env, ARM64Instr_Logic(hi32, hi32, ARM64RIL_R(lo32),
                                        ARM64lo_OR));
         return hi32;
      }

      if (e->Iex.Binop.op == Iop_CmpF64 || e->Iex.Binop.op == Iop_CmpF32) {
         Bool isD = e->Iex.Binop.op == Iop_CmpF64;
         HReg dL  = (isD ? iselDblExpr : iselFltExpr)(env, e->Iex.Binop.arg1);
         HReg dR  = (isD ? iselDblExpr : iselFltExpr)(env, e->Iex.Binop.arg2);
         HReg dst = newVRegI(env);
         HReg imm = newVRegI(env);
         /* Do the compare (FCMP), which sets NZCV in PSTATE.  Then
            create in dst, the IRCmpF64Result encoded result. */
         addInstr(env, (isD ? ARM64Instr_VCmpD : ARM64Instr_VCmpS)(dL, dR));
         addInstr(env, ARM64Instr_Imm64(dst, 0));
         addInstr(env, ARM64Instr_Imm64(imm, 0x40)); // 0x40 = Ircr_EQ
         addInstr(env, ARM64Instr_CSel(dst, imm, dst, ARM64cc_EQ));
         addInstr(env, ARM64Instr_Imm64(imm, 0x01)); // 0x01 = Ircr_LT
         addInstr(env, ARM64Instr_CSel(dst, imm, dst, ARM64cc_MI));
         addInstr(env, ARM64Instr_Imm64(imm, 0x00)); // 0x00 = Ircr_GT
         addInstr(env, ARM64Instr_CSel(dst, imm, dst, ARM64cc_GT));
         addInstr(env, ARM64Instr_Imm64(imm, 0x45)); // 0x45 = Ircr_UN
         addInstr(env, ARM64Instr_CSel(dst, imm, dst, ARM64cc_VS));
         return dst;
      }

      { /* local scope */
        ARM64CvtOp cvt_op = ARM64cvt_INVALID;
        Bool       srcIsD = False;
        switch (e->Iex.Binop.op) {
           case Iop_F64toI64S:
              cvt_op = ARM64cvt_F64_I64S; srcIsD = True; break;
           case Iop_F64toI64U:
              cvt_op = ARM64cvt_F64_I64U; srcIsD = True; break;
           case Iop_F64toI32S:
              cvt_op = ARM64cvt_F64_I32S; srcIsD = True; break;
           case Iop_F64toI32U:
              cvt_op = ARM64cvt_F64_I32U; srcIsD = True; break;
           case Iop_F32toI32S:
              cvt_op = ARM64cvt_F32_I32S; srcIsD = False; break;
           case Iop_F32toI32U:
              cvt_op = ARM64cvt_F32_I32U; srcIsD = False; break;
           case Iop_F32toI64S:
              cvt_op = ARM64cvt_F32_I64S; srcIsD = False; break;
           case Iop_F32toI64U:
              cvt_op = ARM64cvt_F32_I64U; srcIsD = False; break;
           default:
              break;
        }
        if (cvt_op != ARM64cvt_INVALID) {
           /* This is all a bit dodgy, because we can't handle a
              non-constant (not-known-at-JIT-time) rounding mode
              indication.  That's because there's no instruction
              AFAICS that does this conversion but rounds according to
              FPCR.RM, so we have to bake the rounding mode into the
              instruction right now.  But that should be OK because
              (1) the front end attaches a literal Irrm_ value to the
              conversion binop, and (2) iropt will never float that
              off via CSE, into a literal.  Hence we should always
              have an Irrm_ value as the first arg. */
           IRExpr* arg1 = e->Iex.Binop.arg1;
           if (arg1->tag != Iex_Const) goto irreducible;
           IRConst* arg1con = arg1->Iex.Const.con;
           vassert(arg1con->tag == Ico_U32); // else ill-typed IR
           UInt irrm = arg1con->Ico.U32;
           /* Find the ARM-encoded equivalent for |irrm|. */
           UInt armrm = 4; /* impossible */
           switch (irrm) {
              case Irrm_NEAREST: armrm = 0; break;
              case Irrm_NegINF:  armrm = 2; break;
              case Irrm_PosINF:  armrm = 1; break;
              case Irrm_ZERO:    armrm = 3; break;
              default: goto irreducible;
           }
           HReg src = (srcIsD ? iselDblExpr : iselFltExpr)
                         (env, e->Iex.Binop.arg2);
           HReg dst = newVRegI(env);
           addInstr(env, ARM64Instr_VCvtF2I(cvt_op, dst, src, armrm));
           return dst;
        }
      } /* local scope */

      /* All cases involving host-side helper calls. */
      void* fn = NULL;
      switch (e->Iex.Binop.op) {
         case Iop_DivU32:
            fn = &h_calc_udiv32_w_arm_semantics; break;
         case Iop_DivS32:
            fn = &h_calc_sdiv32_w_arm_semantics; break;
         case Iop_DivU64:
            fn = &h_calc_udiv64_w_arm_semantics; break;
         case Iop_DivS64:
            fn = &h_calc_sdiv64_w_arm_semantics; break;
         default:
            break;
      }

      if (fn) {
         HReg regL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg regR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg res  = newVRegI(env);
         addInstr(env, ARM64Instr_MovI(hregARM64_X0(), regL));
         addInstr(env, ARM64Instr_MovI(hregARM64_X1(), regR));
         addInstr(env, ARM64Instr_Call( ARM64cc_AL, (Addr)fn,
                                        2, mk_RetLoc_simple(RLPri_Int) ));
         addInstr(env, ARM64Instr_MovI(res, hregARM64_X0()));
         return res;
      }

      break;
   }

   /* --------- UNARY OP --------- */
   case Iex_Unop: {

      switch (e->Iex.Unop.op) {
         case Iop_16Uto64: {
            /* This probably doesn't occur often enough to be worth
               rolling the extension into the load. */
            IRExpr* arg = e->Iex.Unop.arg;
            HReg    src = iselIntExpr_R(env, arg);
            HReg    dst = widen_z_16_to_64(env, src);
            return dst;
         }
         case Iop_32Uto64: {
            IRExpr* arg = e->Iex.Unop.arg;
            if (arg->tag == Iex_Load) {
               /* This correctly zero extends because _LdSt32 is
                  defined to do a zero extending load. */
               HReg dst = newVRegI(env);
               ARM64AMode* am
                  = iselIntExpr_AMode(env, arg->Iex.Load.addr, Ity_I32);
               addInstr(env, ARM64Instr_LdSt32(True/*isLoad*/, dst, am));
               return dst;
            }
            /* else be lame and mask it  */
            HReg src  = iselIntExpr_R(env, arg);
            HReg dst  = widen_z_32_to_64(env, src);
            return dst;
         }
         case Iop_8Uto32: /* Just freeload on the 8Uto64 case */
         case Iop_8Uto64: {
            IRExpr* arg = e->Iex.Unop.arg;
            if (arg->tag == Iex_Load) {
               /* This correctly zero extends because _LdSt8 is
                  defined to do a zero extending load. */
               HReg dst = newVRegI(env);
               ARM64AMode* am
                  = iselIntExpr_AMode(env, arg->Iex.Load.addr, Ity_I8);
               addInstr(env, ARM64Instr_LdSt8(True/*isLoad*/, dst, am));
               return dst;
            }
            /* else be lame and mask it  */
            HReg src = iselIntExpr_R(env, arg);
            HReg dst = widen_z_8_to_64(env, src);
            return dst;
         }
         case Iop_128HIto64: {
            HReg rHi, rLo;
            iselInt128Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rHi; /* and abandon rLo */
         }
         case Iop_8Sto32: case Iop_8Sto64: {
            IRExpr* arg = e->Iex.Unop.arg;
            HReg    src = iselIntExpr_R(env, arg);
            HReg    dst = widen_s_8_to_64(env, src);
            return dst;
         }
         case Iop_16Sto32: case Iop_16Sto64: {
            IRExpr* arg = e->Iex.Unop.arg;
            HReg    src = iselIntExpr_R(env, arg);
            HReg    dst = widen_s_16_to_64(env, src);
            return dst;
         }
         case Iop_32Sto64: {
            IRExpr* arg = e->Iex.Unop.arg;
            HReg    src = iselIntExpr_R(env, arg);
            HReg    dst = widen_s_32_to_64(env, src);
            return dst;
         }
         case Iop_Not32:
         case Iop_Not64: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_Unary(dst, src, ARM64un_NOT));
            return dst;
         }
         case Iop_Clz64: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_Unary(dst, src, ARM64un_CLZ));
            return dst;
         }
         case Iop_Left32:
         case Iop_Left64: {
            /* Left64(src) = src | -src.  Left32 can use the same
               implementation since in that case we don't care what
               the upper 32 bits become. */
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_Unary(dst, src, ARM64un_NEG));
            addInstr(env, ARM64Instr_Logic(dst, dst, ARM64RIL_R(src),
                                           ARM64lo_OR));
            return dst;
         }
         case Iop_CmpwNEZ64: {
           /* CmpwNEZ64(src) = (src == 0) ? 0...0 : 1...1
                             = Left64(src) >>s 63 */
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_Unary(dst, src, ARM64un_NEG));
            addInstr(env, ARM64Instr_Logic(dst, dst, ARM64RIL_R(src),
                                           ARM64lo_OR));
            addInstr(env, ARM64Instr_Shift(dst, dst, ARM64RI6_I6(63),
                                           ARM64sh_SAR));
            return dst;
         }
         case Iop_CmpwNEZ32: {
            /* CmpwNEZ32(src) = CmpwNEZ64(src & 0xFFFFFFFF)
                              = Left64(src & 0xFFFFFFFF) >>s 63 */
            HReg dst = newVRegI(env);
            HReg pre = iselIntExpr_R(env, e->Iex.Unop.arg);
            HReg src = widen_z_32_to_64(env, pre);
            addInstr(env, ARM64Instr_Unary(dst, src, ARM64un_NEG));
            addInstr(env, ARM64Instr_Logic(dst, dst, ARM64RIL_R(src),
                                           ARM64lo_OR));
            addInstr(env, ARM64Instr_Shift(dst, dst, ARM64RI6_I6(63),
                                           ARM64sh_SAR));
            return dst;
         }
         case Iop_V128to64: case Iop_V128HIto64: {
            HReg dst    = newVRegI(env);
            HReg src    = iselV128Expr(env, e->Iex.Unop.arg);
            UInt laneNo = (e->Iex.Unop.op == Iop_V128HIto64) ? 1 : 0;
            addInstr(env, ARM64Instr_VXfromQ(dst, src, laneNo));
            return dst;
         }
         case Iop_ReinterpF64asI64: {
            HReg dst = newVRegI(env);
            HReg src = iselDblExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_VXfromDorS(dst, src, True/*fromD*/));
            return dst;
         }
         case Iop_ReinterpF32asI32: {
            HReg dst = newVRegI(env);
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_VXfromDorS(dst, src, False/*!fromD*/));
            return dst;
         }
         case Iop_1Sto16:
         case Iop_1Sto32:
         case Iop_1Sto64: {
            /* As with the iselStmt case for 'tmp:I1 = expr', we could
               do a lot better here if it ever became necessary. */
            HReg zero = newVRegI(env);
            HReg one  = newVRegI(env);
            HReg dst  = newVRegI(env);
            addInstr(env, ARM64Instr_Imm64(zero, 0));
            addInstr(env, ARM64Instr_Imm64(one,  1));
            ARM64CondCode cc = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_CSel(dst, one, zero, cc));
            addInstr(env, ARM64Instr_Shift(dst, dst, ARM64RI6_I6(63),
                                           ARM64sh_SHL));
            addInstr(env, ARM64Instr_Shift(dst, dst, ARM64RI6_I6(63),
                                           ARM64sh_SAR));
            return dst;
         }
         case Iop_NarrowUn16to8x8:
         case Iop_NarrowUn32to16x4:
         case Iop_NarrowUn64to32x2:
         case Iop_QNarrowUn16Sto8Sx8:
         case Iop_QNarrowUn32Sto16Sx4:
         case Iop_QNarrowUn64Sto32Sx2:
         case Iop_QNarrowUn16Uto8Ux8:
         case Iop_QNarrowUn32Uto16Ux4:
         case Iop_QNarrowUn64Uto32Ux2:
         case Iop_QNarrowUn16Sto8Ux8:
         case Iop_QNarrowUn32Sto16Ux4:
         case Iop_QNarrowUn64Sto32Ux2:
         {
            HReg src = iselV128Expr(env, e->Iex.Unop.arg);
            HReg tmp = newVRegV(env);
            HReg dst = newVRegI(env);
            UInt dszBlg2 = 3; /* illegal */
            ARM64VecNarrowOp op = ARM64vecna_INVALID;
            switch (e->Iex.Unop.op) {
               case Iop_NarrowUn16to8x8:
                  dszBlg2 = 0; op = ARM64vecna_XTN; break;
               case Iop_NarrowUn32to16x4:
                  dszBlg2 = 1; op = ARM64vecna_XTN; break;
               case Iop_NarrowUn64to32x2:
                  dszBlg2 = 2; op = ARM64vecna_XTN; break;
               case Iop_QNarrowUn16Sto8Sx8:
                  dszBlg2 = 0; op = ARM64vecna_SQXTN; break;
               case Iop_QNarrowUn32Sto16Sx4:
                  dszBlg2 = 1; op = ARM64vecna_SQXTN; break;
               case Iop_QNarrowUn64Sto32Sx2:
                  dszBlg2 = 2; op = ARM64vecna_SQXTN; break;
               case Iop_QNarrowUn16Uto8Ux8:
                  dszBlg2 = 0; op = ARM64vecna_UQXTN; break;
               case Iop_QNarrowUn32Uto16Ux4:
                  dszBlg2 = 1; op = ARM64vecna_UQXTN; break;
               case Iop_QNarrowUn64Uto32Ux2:
                  dszBlg2 = 2; op = ARM64vecna_UQXTN; break;
               case Iop_QNarrowUn16Sto8Ux8:
                  dszBlg2 = 0; op = ARM64vecna_SQXTUN; break;
               case Iop_QNarrowUn32Sto16Ux4:
                  dszBlg2 = 1; op = ARM64vecna_SQXTUN; break;
               case Iop_QNarrowUn64Sto32Ux2:
                  dszBlg2 = 2; op = ARM64vecna_SQXTUN; break;
               default:
                  vassert(0);
            }
            addInstr(env, ARM64Instr_VNarrowV(op, dszBlg2, tmp, src));
            addInstr(env, ARM64Instr_VXfromQ(dst, tmp, 0/*laneNo*/));
            return dst;
         }
         case Iop_1Uto64: {
            /* 1Uto64(tmp). */
            HReg dst = newVRegI(env);
            if (e->Iex.Unop.arg->tag == Iex_RdTmp) {
               ARM64RIL* one = mb_mkARM64RIL_I(1);
               HReg src = lookupIRTemp(env, e->Iex.Unop.arg->Iex.RdTmp.tmp);
               vassert(one);
               addInstr(env, ARM64Instr_Logic(dst, src, one, ARM64lo_AND));
            } else {
               /* CLONE-01 */
               HReg zero = newVRegI(env);
               HReg one  = newVRegI(env);
               addInstr(env, ARM64Instr_Imm64(zero, 0));
               addInstr(env, ARM64Instr_Imm64(one,  1));
               ARM64CondCode cc = iselCondCode(env, e->Iex.Unop.arg);
               addInstr(env, ARM64Instr_CSel(dst, one, zero, cc));
            }
            return dst;
         }
         case Iop_64to32:
         case Iop_64to16:
         case Iop_64to8:
            /* These are no-ops. */
            return iselIntExpr_R(env, e->Iex.Unop.arg);

         default:
            break;
      }

      break;
   }

   /* --------- GET --------- */
   case Iex_Get: {
      if (ty == Ity_I64
          && 0 == (e->Iex.Get.offset & 7) && e->Iex.Get.offset < (8<<12)-8) {
         HReg        dst = newVRegI(env);
         ARM64AMode* am
            = mk_baseblock_64bit_access_amode(e->Iex.Get.offset);
         addInstr(env, ARM64Instr_LdSt64(True/*isLoad*/, dst, am));
         return dst;
      }
      if (ty == Ity_I32
          && 0 == (e->Iex.Get.offset & 3) && e->Iex.Get.offset < (4<<12)-4) {
         HReg        dst = newVRegI(env);
         ARM64AMode* am
            = mk_baseblock_32bit_access_amode(e->Iex.Get.offset);
         addInstr(env, ARM64Instr_LdSt32(True/*isLoad*/, dst, am));
         return dst;
      }
      if (ty == Ity_I16
          && 0 == (e->Iex.Get.offset & 1) && e->Iex.Get.offset < (2<<12)-2) {
         HReg        dst = newVRegI(env);
         ARM64AMode* am
            = mk_baseblock_16bit_access_amode(e->Iex.Get.offset);
         addInstr(env, ARM64Instr_LdSt16(True/*isLoad*/, dst, am));
         return dst;
      }
      if (ty == Ity_I8
          /* && no alignment check */ && e->Iex.Get.offset < (1<<12)-1) {
         HReg        dst = newVRegI(env);
         ARM64AMode* am
            = mk_baseblock_8bit_access_amode(e->Iex.Get.offset);
         addInstr(env, ARM64Instr_LdSt8(True/*isLoad*/, dst, am));
         return dst;
      }
      break;
   }

   /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg    dst = newVRegI(env);
      vassert(ty == e->Iex.CCall.retty);

      /* be very restrictive for now.  Only 64-bit ints allowed for
         args, and 64 bits for return type.  Don't forget to change
         the RetLoc if more types are allowed in future. */
      if (e->Iex.CCall.retty != Ity_I64)
         goto irreducible;

      /* Marshal args, do the call, clear stack. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      Bool   ok      = doHelperCall( &addToSp, &rloc, env, NULL/*guard*/,
                                     e->Iex.CCall.cee, e->Iex.CCall.retty,
                                     e->Iex.CCall.args );
      /* */
      if (ok) {
         vassert(is_sane_RetLoc(rloc));
         vassert(rloc.pri == RLPri_Int);
         vassert(addToSp == 0);
         addInstr(env, ARM64Instr_MovI(dst, hregARM64_X0()));
         return dst;
      }
      /* else fall through; will hit the irreducible: label */
   }

   /* --------- LITERAL --------- */
   /* 64-bit literals */
   case Iex_Const: {
      ULong u   = 0;
      HReg  dst = newVRegI(env);
      switch (e->Iex.Const.con->tag) {
         case Ico_U64: u = e->Iex.Const.con->Ico.U64; break;
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = e->Iex.Const.con->Ico.U16; break;
         case Ico_U8:  u = e->Iex.Const.con->Ico.U8;  break;
         default: ppIRExpr(e); vpanic("iselIntExpr_R.Iex_Const(arm64)");
      }
      addInstr(env, ARM64Instr_Imm64(dst, u));
      return dst;
   }

   /* --------- MULTIPLEX --------- */
   case Iex_ITE: {
      /* ITE(ccexpr, iftrue, iffalse) */
      if (ty == Ity_I64 || ty == Ity_I32) {
         ARM64CondCode cc;
         HReg r1  = iselIntExpr_R(env, e->Iex.ITE.iftrue);
         HReg r0  = iselIntExpr_R(env, e->Iex.ITE.iffalse);
         HReg dst = newVRegI(env);
         cc = iselCondCode(env, e->Iex.ITE.cond);
         addInstr(env, ARM64Instr_CSel(dst, r1, r0, cc));
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
/*--- ISEL: Integer expressions (128 bit)               ---*/
/*---------------------------------------------------------*/

/* Compute a 128-bit value into a register pair, which is returned as
   the first two parameters.  As with iselIntExpr_R, these may be
   either real or virtual regs; in any case they must not be changed
   by subsequent code emitted by the caller.  */

static void iselInt128Expr ( HReg* rHi, HReg* rLo, 
                             ISelEnv* env, IRExpr* e )
{
   iselInt128Expr_wrk(rHi, rLo, env, e);
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(*rHi) == HRcInt64);
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcInt64);
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt128Expr_wrk ( HReg* rHi, HReg* rLo, 
                                 ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I128);

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         /* 64 x 64 -> 128 multiply */
         case Iop_MullU64:
         case Iop_MullS64: {
            Bool syned = toBool(e->Iex.Binop.op == Iop_MullS64);
            HReg argL  = iselIntExpr_R(env, e->Iex.Binop.arg1);
            HReg argR  = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg dstLo = newVRegI(env);
            HReg dstHi = newVRegI(env);
            addInstr(env, ARM64Instr_Mul(dstLo, argL, argR,
                                         ARM64mul_PLAIN));
            addInstr(env, ARM64Instr_Mul(dstHi, argL, argR,
                                         syned ? ARM64mul_SX : ARM64mul_ZX));
            *rHi = dstHi;
            *rLo = dstLo;
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
   } /* if (e->tag == Iex_Binop) */

   ppIRExpr(e);
   vpanic("iselInt128Expr(arm64)");
}


/*---------------------------------------------------------*/
/*--- ISEL: Vector expressions (128 bit)                ---*/
/*---------------------------------------------------------*/

static HReg iselV128Expr ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselV128Expr_wrk( env, e );
   vassert(hregClass(r) == HRcVec128);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselV128Expr_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(e);
   vassert(ty == Ity_V128);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Const) {
      /* Only a very limited range of constants is handled. */
      vassert(e->Iex.Const.con->tag == Ico_V128);
      UShort con = e->Iex.Const.con->Ico.V128;
      HReg   res = newVRegV(env);
      switch (con) {
         case 0x0000: case 0x000F: case 0x003F: case 0x00FF: case 0xFFFF:
            addInstr(env, ARM64Instr_VImmQ(res, con));
            return res;
         case 0x00F0:
            addInstr(env, ARM64Instr_VImmQ(res, 0x000F));
            addInstr(env, ARM64Instr_VExtV(res, res, res, 12));
            return res;
         case 0x0F00:
            addInstr(env, ARM64Instr_VImmQ(res, 0x000F));
            addInstr(env, ARM64Instr_VExtV(res, res, res, 8));
            return res;
         case 0x0FF0:
            addInstr(env, ARM64Instr_VImmQ(res, 0x00FF));
            addInstr(env, ARM64Instr_VExtV(res, res, res, 12));
            return res;
         case 0x0FFF:
            addInstr(env, ARM64Instr_VImmQ(res, 0x000F));
            addInstr(env, ARM64Instr_VExtV(res, res, res, 4));
            addInstr(env, ARM64Instr_VUnaryV(ARM64vecu_NOT, res, res));
            return res;
         case 0xF000:
            addInstr(env, ARM64Instr_VImmQ(res, 0x000F));
            addInstr(env, ARM64Instr_VExtV(res, res, res, 4));
            return res;
         case 0xFF00:
            addInstr(env, ARM64Instr_VImmQ(res, 0x00FF));
            addInstr(env, ARM64Instr_VExtV(res, res, res, 8));
            return res;
         default: 
            break;
      }
      /* Unhandled */
      goto v128_expr_bad;
   }

   if (e->tag == Iex_Load) {
      HReg res = newVRegV(env);
      HReg rN  = iselIntExpr_R(env, e->Iex.Load.addr);
      vassert(ty == Ity_V128);
      addInstr(env, ARM64Instr_VLdStQ(True/*isLoad*/, res, rN));
      return res;
   }

   if (e->tag == Iex_Get) {
      UInt offs = (UInt)e->Iex.Get.offset;
      if (offs < (1<<12)) {
         HReg addr = mk_baseblock_128bit_access_addr(env, offs);
         HReg res  = newVRegV(env);
         vassert(ty == Ity_V128);
         addInstr(env, ARM64Instr_VLdStQ(True/*isLoad*/, res, addr));
         return res;
      }
      goto v128_expr_bad;
   }

   if (e->tag == Iex_Unop) {

      /* Iop_ZeroHIXXofV128 cases */
      UShort imm16 = 0;
      switch (e->Iex.Unop.op) {
         case Iop_ZeroHI64ofV128:  imm16 = 0x00FF; break;
         case Iop_ZeroHI96ofV128:  imm16 = 0x000F; break;
         case Iop_ZeroHI112ofV128: imm16 = 0x0003; break;
         case Iop_ZeroHI120ofV128: imm16 = 0x0001; break;
         default: break;
      }
      if (imm16 != 0) {
         HReg src = iselV128Expr(env, e->Iex.Unop.arg);
         HReg imm = newVRegV(env);
         HReg res = newVRegV(env);
         addInstr(env, ARM64Instr_VImmQ(imm, imm16));
         addInstr(env, ARM64Instr_VBinV(ARM64vecb_AND, res, src, imm));
         return res;
      }

      /* Other cases */
      switch (e->Iex.Unop.op) {
         case Iop_NotV128:
         case Iop_Abs64Fx2: case Iop_Abs32Fx4:
         case Iop_Neg64Fx2: case Iop_Neg32Fx4:
         case Iop_Abs64x2:  case Iop_Abs32x4:
         case Iop_Abs16x8:  case Iop_Abs8x16:
         case Iop_Cls32x4:  case Iop_Cls16x8:  case Iop_Cls8x16:
         case Iop_Clz32x4:  case Iop_Clz16x8:  case Iop_Clz8x16:
         case Iop_Cnt8x16:
         case Iop_Reverse1sIn8_x16:
         case Iop_Reverse8sIn16_x8:
         case Iop_Reverse8sIn32_x4: case Iop_Reverse16sIn32_x4:
         case Iop_Reverse8sIn64_x2: case Iop_Reverse16sIn64_x2:
         case Iop_Reverse32sIn64_x2:
         case Iop_RecipEst32Ux4:
         case Iop_RSqrtEst32Ux4:
         case Iop_RecipEst64Fx2: case Iop_RecipEst32Fx4:
         case Iop_RSqrtEst64Fx2: case Iop_RSqrtEst32Fx4:
         {
            HReg res   = newVRegV(env);
            HReg arg   = iselV128Expr(env, e->Iex.Unop.arg);
            Bool setRM = False;
            ARM64VecUnaryOp op = ARM64vecu_INVALID;
            switch (e->Iex.Unop.op) {
               case Iop_NotV128:           op = ARM64vecu_NOT;         break;
               case Iop_Abs64Fx2:          op = ARM64vecu_FABS64x2;    break;
               case Iop_Abs32Fx4:          op = ARM64vecu_FABS32x4;    break;
               case Iop_Neg64Fx2:          op = ARM64vecu_FNEG64x2;    break;
               case Iop_Neg32Fx4:          op = ARM64vecu_FNEG32x4;    break;
               case Iop_Abs64x2:           op = ARM64vecu_ABS64x2;     break;
               case Iop_Abs32x4:           op = ARM64vecu_ABS32x4;     break;
               case Iop_Abs16x8:           op = ARM64vecu_ABS16x8;     break;
               case Iop_Abs8x16:           op = ARM64vecu_ABS8x16;     break;
               case Iop_Cls32x4:           op = ARM64vecu_CLS32x4;     break;
               case Iop_Cls16x8:           op = ARM64vecu_CLS16x8;     break;
               case Iop_Cls8x16:           op = ARM64vecu_CLS8x16;     break;
               case Iop_Clz32x4:           op = ARM64vecu_CLZ32x4;     break;
               case Iop_Clz16x8:           op = ARM64vecu_CLZ16x8;     break;
               case Iop_Clz8x16:           op = ARM64vecu_CLZ8x16;     break;
               case Iop_Cnt8x16:           op = ARM64vecu_CNT8x16;     break;
               case Iop_Reverse1sIn8_x16:  op = ARM64vecu_RBIT;        break;
               case Iop_Reverse8sIn16_x8:  op = ARM64vecu_REV1616B;    break;
               case Iop_Reverse8sIn32_x4:  op = ARM64vecu_REV3216B;    break;
               case Iop_Reverse16sIn32_x4: op = ARM64vecu_REV328H;     break;
               case Iop_Reverse8sIn64_x2:  op = ARM64vecu_REV6416B;    break;
               case Iop_Reverse16sIn64_x2: op = ARM64vecu_REV648H;     break;
               case Iop_Reverse32sIn64_x2: op = ARM64vecu_REV644S;     break;
               case Iop_RecipEst32Ux4:     op = ARM64vecu_URECPE32x4;  break;
               case Iop_RSqrtEst32Ux4:     op = ARM64vecu_URSQRTE32x4; break;
               case Iop_RecipEst64Fx2:     setRM = True;
                                           op = ARM64vecu_FRECPE64x2;  break;
               case Iop_RecipEst32Fx4:     setRM = True;
                                           op = ARM64vecu_FRECPE32x4;  break;
               case Iop_RSqrtEst64Fx2:     setRM = True;
                                           op = ARM64vecu_FRSQRTE64x2; break;
               case Iop_RSqrtEst32Fx4:     setRM = True;
                                           op = ARM64vecu_FRSQRTE32x4; break;
               default: vassert(0);
            }
            if (setRM) {
               // This is a bit of a kludge.  We should do rm properly for
               // these recip-est insns, but that would require changing the
               // primop's type to take an rmode.
               set_FPCR_rounding_mode(env, IRExpr_Const(
                                              IRConst_U32(Irrm_NEAREST)));
            }
            addInstr(env, ARM64Instr_VUnaryV(op, res, arg));
            return res;
         }
         case Iop_CmpNEZ8x16:
         case Iop_CmpNEZ16x8:
         case Iop_CmpNEZ32x4:
         case Iop_CmpNEZ64x2: {
            HReg arg  = iselV128Expr(env, e->Iex.Unop.arg);
            HReg zero = newVRegV(env);
            HReg res  = newVRegV(env);
            ARM64VecBinOp cmp = ARM64vecb_INVALID;
            switch (e->Iex.Unop.op) {
               case Iop_CmpNEZ64x2: cmp = ARM64vecb_CMEQ64x2; break;
               case Iop_CmpNEZ32x4: cmp = ARM64vecb_CMEQ32x4; break;
               case Iop_CmpNEZ16x8: cmp = ARM64vecb_CMEQ16x8; break;
               case Iop_CmpNEZ8x16: cmp = ARM64vecb_CMEQ8x16; break;
               default: vassert(0);
            }
            // This is pretty feeble.  Better: use CMP against zero
            // and avoid the extra instruction and extra register.
            addInstr(env, ARM64Instr_VImmQ(zero, 0x0000));
            addInstr(env, ARM64Instr_VBinV(cmp, res, arg, zero));
            addInstr(env, ARM64Instr_VUnaryV(ARM64vecu_NOT, res, res));
            return res;
         }
         case Iop_V256toV128_0:
         case Iop_V256toV128_1: {
            HReg vHi, vLo;
            iselV256Expr(&vHi, &vLo, env, e->Iex.Unop.arg);
            return (e->Iex.Unop.op == Iop_V256toV128_1) ? vHi : vLo;
         }
         case Iop_64UtoV128: {
            HReg res = newVRegV(env);
            HReg arg = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_VQfromX(res, arg));
            return res;
         }
         case Iop_Widen8Sto16x8: {
            HReg res = newVRegV(env);
            HReg arg = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_VQfromX(res, arg));
            addInstr(env, ARM64Instr_VBinV(ARM64vecb_ZIP18x16, res, res, res));
            addInstr(env, ARM64Instr_VShiftImmV(ARM64vecshi_SSHR16x8,
                                                res, res, 8));
            return res;
         }
         case Iop_Widen16Sto32x4: {
            HReg res = newVRegV(env);
            HReg arg = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_VQfromX(res, arg));
            addInstr(env, ARM64Instr_VBinV(ARM64vecb_ZIP116x8, res, res, res));
            addInstr(env, ARM64Instr_VShiftImmV(ARM64vecshi_SSHR32x4,
                                                res, res, 16));
            return res;
         }
         case Iop_Widen32Sto64x2: {
            HReg res = newVRegV(env);
            HReg arg = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARM64Instr_VQfromX(res, arg));
            addInstr(env, ARM64Instr_VBinV(ARM64vecb_ZIP132x4, res, res, res));
            addInstr(env, ARM64Instr_VShiftImmV(ARM64vecshi_SSHR64x2,
                                                res, res, 32));
            return res;
         }
         /* ... */
         default:
            break;
      } /* switch on the unop */
   } /* if (e->tag == Iex_Unop) */

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_Sqrt32Fx4:
         case Iop_Sqrt64Fx2: {
            HReg arg = iselV128Expr(env, e->Iex.Binop.arg2);
            HReg res = newVRegV(env);
            set_FPCR_rounding_mode(env, e->Iex.Binop.arg1);
            ARM64VecUnaryOp op 
               = e->Iex.Binop.op == Iop_Sqrt32Fx4
                    ? ARM64vecu_FSQRT32x4 : ARM64vecu_FSQRT64x2;
            addInstr(env, ARM64Instr_VUnaryV(op, res, arg));
            return res;
         }
         case Iop_64HLtoV128: {
            HReg res  = newVRegV(env);
            HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
            HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, ARM64Instr_VQfromXX(res, argL, argR));
            return res;
         }
         /* -- Cases where we can generate a simple three-reg instruction. -- */
         case Iop_AndV128:
         case Iop_OrV128:
         case Iop_XorV128:
         case Iop_Max32Ux4: case Iop_Max16Ux8: case Iop_Max8Ux16:
         case Iop_Min32Ux4: case Iop_Min16Ux8: case Iop_Min8Ux16:
         case Iop_Max32Sx4: case Iop_Max16Sx8: case Iop_Max8Sx16:
         case Iop_Min32Sx4: case Iop_Min16Sx8: case Iop_Min8Sx16:
         case Iop_Add64x2: case Iop_Add32x4:
         case Iop_Add16x8: case Iop_Add8x16:
         case Iop_Sub64x2: case Iop_Sub32x4:
         case Iop_Sub16x8: case Iop_Sub8x16:
         case Iop_Mul32x4: case Iop_Mul16x8: case Iop_Mul8x16:
         case Iop_CmpEQ64x2: case Iop_CmpEQ32x4:
         case Iop_CmpEQ16x8:  case Iop_CmpEQ8x16:
         case Iop_CmpGT64Ux2: case Iop_CmpGT32Ux4:
         case Iop_CmpGT16Ux8: case Iop_CmpGT8Ux16:
         case Iop_CmpGT64Sx2: case Iop_CmpGT32Sx4:
         case Iop_CmpGT16Sx8: case Iop_CmpGT8Sx16:
         case Iop_CmpEQ64Fx2: case Iop_CmpEQ32Fx4:
         case Iop_CmpLE64Fx2: case Iop_CmpLE32Fx4:
         case Iop_CmpLT64Fx2: case Iop_CmpLT32Fx4:
         case Iop_Perm8x16:
         case Iop_InterleaveLO64x2: case Iop_CatEvenLanes32x4:
         case Iop_CatEvenLanes16x8: case Iop_CatEvenLanes8x16:
         case Iop_InterleaveHI64x2: case Iop_CatOddLanes32x4:
         case Iop_CatOddLanes16x8:  case Iop_CatOddLanes8x16:
         case Iop_InterleaveHI32x4:
         case Iop_InterleaveHI16x8: case Iop_InterleaveHI8x16:
         case Iop_InterleaveLO32x4:
         case Iop_InterleaveLO16x8: case Iop_InterleaveLO8x16:
         case Iop_PolynomialMul8x16:
         case Iop_QAdd64Sx2: case Iop_QAdd32Sx4:
         case Iop_QAdd16Sx8: case Iop_QAdd8Sx16:
         case Iop_QAdd64Ux2: case Iop_QAdd32Ux4:
         case Iop_QAdd16Ux8: case Iop_QAdd8Ux16:
         case Iop_QSub64Sx2: case Iop_QSub32Sx4:
         case Iop_QSub16Sx8: case Iop_QSub8Sx16:
         case Iop_QSub64Ux2: case Iop_QSub32Ux4:
         case Iop_QSub16Ux8: case Iop_QSub8Ux16:
         case Iop_QDMulHi32Sx4:  case Iop_QDMulHi16Sx8:
         case Iop_QRDMulHi32Sx4: case Iop_QRDMulHi16Sx8:
         case Iop_Sh8Sx16:  case Iop_Sh16Sx8:
         case Iop_Sh32Sx4:  case Iop_Sh64Sx2:
         case Iop_Sh8Ux16:  case Iop_Sh16Ux8:
         case Iop_Sh32Ux4:  case Iop_Sh64Ux2:
         case Iop_Rsh8Sx16: case Iop_Rsh16Sx8:
         case Iop_Rsh32Sx4: case Iop_Rsh64Sx2:
         case Iop_Rsh8Ux16: case Iop_Rsh16Ux8:
         case Iop_Rsh32Ux4: case Iop_Rsh64Ux2:
         case Iop_Max64Fx2: case Iop_Max32Fx4:
         case Iop_Min64Fx2: case Iop_Min32Fx4:
         case Iop_RecipStep64Fx2: case Iop_RecipStep32Fx4:
         case Iop_RSqrtStep64Fx2: case Iop_RSqrtStep32Fx4:
         {
            HReg res   = newVRegV(env);
            HReg argL  = iselV128Expr(env, e->Iex.Binop.arg1);
            HReg argR  = iselV128Expr(env, e->Iex.Binop.arg2);
            Bool sw    = False;
            Bool setRM = False;
            ARM64VecBinOp op = ARM64vecb_INVALID;
            switch (e->Iex.Binop.op) {
               case Iop_AndV128:    op = ARM64vecb_AND; break;
               case Iop_OrV128:     op = ARM64vecb_ORR; break;
               case Iop_XorV128:    op = ARM64vecb_XOR; break;
               case Iop_Max32Ux4:   op = ARM64vecb_UMAX32x4; break;
               case Iop_Max16Ux8:   op = ARM64vecb_UMAX16x8; break;
               case Iop_Max8Ux16:   op = ARM64vecb_UMAX8x16; break;
               case Iop_Min32Ux4:   op = ARM64vecb_UMIN32x4; break;
               case Iop_Min16Ux8:   op = ARM64vecb_UMIN16x8; break;
               case Iop_Min8Ux16:   op = ARM64vecb_UMIN8x16; break;
               case Iop_Max32Sx4:   op = ARM64vecb_SMAX32x4; break;
               case Iop_Max16Sx8:   op = ARM64vecb_SMAX16x8; break;
               case Iop_Max8Sx16:   op = ARM64vecb_SMAX8x16; break;
               case Iop_Min32Sx4:   op = ARM64vecb_SMIN32x4; break;
               case Iop_Min16Sx8:   op = ARM64vecb_SMIN16x8; break;
               case Iop_Min8Sx16:   op = ARM64vecb_SMIN8x16; break;
               case Iop_Add64x2:    op = ARM64vecb_ADD64x2; break;
               case Iop_Add32x4:    op = ARM64vecb_ADD32x4; break;
               case Iop_Add16x8:    op = ARM64vecb_ADD16x8; break;
               case Iop_Add8x16:    op = ARM64vecb_ADD8x16; break;
               case Iop_Sub64x2:    op = ARM64vecb_SUB64x2; break;
               case Iop_Sub32x4:    op = ARM64vecb_SUB32x4; break;
               case Iop_Sub16x8:    op = ARM64vecb_SUB16x8; break;
               case Iop_Sub8x16:    op = ARM64vecb_SUB8x16; break;
               case Iop_Mul32x4:    op = ARM64vecb_MUL32x4; break;
               case Iop_Mul16x8:    op = ARM64vecb_MUL16x8; break;
               case Iop_Mul8x16:    op = ARM64vecb_MUL8x16; break;
               case Iop_CmpEQ64x2:  op = ARM64vecb_CMEQ64x2; break;
               case Iop_CmpEQ32x4:  op = ARM64vecb_CMEQ32x4; break;
               case Iop_CmpEQ16x8:  op = ARM64vecb_CMEQ16x8; break;
               case Iop_CmpEQ8x16:  op = ARM64vecb_CMEQ8x16; break;
               case Iop_CmpGT64Ux2: op = ARM64vecb_CMHI64x2; break;
               case Iop_CmpGT32Ux4: op = ARM64vecb_CMHI32x4; break;
               case Iop_CmpGT16Ux8: op = ARM64vecb_CMHI16x8; break;
               case Iop_CmpGT8Ux16: op = ARM64vecb_CMHI8x16; break;
               case Iop_CmpGT64Sx2: op = ARM64vecb_CMGT64x2; break;
               case Iop_CmpGT32Sx4: op = ARM64vecb_CMGT32x4; break;
               case Iop_CmpGT16Sx8: op = ARM64vecb_CMGT16x8; break;
               case Iop_CmpGT8Sx16: op = ARM64vecb_CMGT8x16; break;
               case Iop_CmpEQ64Fx2: op = ARM64vecb_FCMEQ64x2; break;
               case Iop_CmpEQ32Fx4: op = ARM64vecb_FCMEQ32x4; break;
               case Iop_CmpLE64Fx2: op = ARM64vecb_FCMGE64x2; sw = True; break;
               case Iop_CmpLE32Fx4: op = ARM64vecb_FCMGE32x4; sw = True; break;
               case Iop_CmpLT64Fx2: op = ARM64vecb_FCMGT64x2; sw = True; break;
               case Iop_CmpLT32Fx4: op = ARM64vecb_FCMGT32x4; sw = True; break;
               case Iop_Perm8x16:   op = ARM64vecb_TBL1; break;
               case Iop_InterleaveLO64x2: op = ARM64vecb_UZP164x2; sw = True;
                                          break;
               case Iop_CatEvenLanes32x4: op = ARM64vecb_UZP132x4; sw = True;
                                          break;
               case Iop_CatEvenLanes16x8: op = ARM64vecb_UZP116x8; sw = True;
                                          break;
               case Iop_CatEvenLanes8x16: op = ARM64vecb_UZP18x16; sw = True;
                                          break;
               case Iop_InterleaveHI64x2: op = ARM64vecb_UZP264x2; sw = True;
                                          break;
               case Iop_CatOddLanes32x4:  op = ARM64vecb_UZP232x4; sw = True;
                                          break;
               case Iop_CatOddLanes16x8:  op = ARM64vecb_UZP216x8; sw = True;
                                          break;
               case Iop_CatOddLanes8x16:  op = ARM64vecb_UZP28x16; sw = True;
                                          break;
               case Iop_InterleaveHI32x4: op = ARM64vecb_ZIP232x4; sw = True;
                                          break;
               case Iop_InterleaveHI16x8: op = ARM64vecb_ZIP216x8; sw = True;
                                          break;
               case Iop_InterleaveHI8x16: op = ARM64vecb_ZIP28x16; sw = True;
                                          break;
               case Iop_InterleaveLO32x4: op = ARM64vecb_ZIP132x4; sw = True;
                                          break;
               case Iop_InterleaveLO16x8: op = ARM64vecb_ZIP116x8; sw = True;
                                          break;
               case Iop_InterleaveLO8x16: op = ARM64vecb_ZIP18x16; sw = True;
                                          break;
               case Iop_PolynomialMul8x16: op = ARM64vecb_PMUL8x16; break;
               case Iop_QAdd64Sx2:      op = ARM64vecb_SQADD64x2; break;
               case Iop_QAdd32Sx4:      op = ARM64vecb_SQADD32x4; break;
               case Iop_QAdd16Sx8:      op = ARM64vecb_SQADD16x8; break;
               case Iop_QAdd8Sx16:      op = ARM64vecb_SQADD8x16; break;
               case Iop_QAdd64Ux2:      op = ARM64vecb_UQADD64x2; break;
               case Iop_QAdd32Ux4:      op = ARM64vecb_UQADD32x4; break;
               case Iop_QAdd16Ux8:      op = ARM64vecb_UQADD16x8; break;
               case Iop_QAdd8Ux16:      op = ARM64vecb_UQADD8x16; break;
               case Iop_QSub64Sx2:      op = ARM64vecb_SQSUB64x2; break;
               case Iop_QSub32Sx4:      op = ARM64vecb_SQSUB32x4; break;
               case Iop_QSub16Sx8:      op = ARM64vecb_SQSUB16x8; break;
               case Iop_QSub8Sx16:      op = ARM64vecb_SQSUB8x16; break;
               case Iop_QSub64Ux2:      op = ARM64vecb_UQSUB64x2; break;
               case Iop_QSub32Ux4:      op = ARM64vecb_UQSUB32x4; break;
               case Iop_QSub16Ux8:      op = ARM64vecb_UQSUB16x8; break;
               case Iop_QSub8Ux16:      op = ARM64vecb_UQSUB8x16; break;
               case Iop_QDMulHi32Sx4:   op = ARM64vecb_SQDMULH32x4; break;
               case Iop_QDMulHi16Sx8:   op = ARM64vecb_SQDMULH16x8; break;
               case Iop_QRDMulHi32Sx4:  op = ARM64vecb_SQRDMULH32x4; break;
               case Iop_QRDMulHi16Sx8:  op = ARM64vecb_SQRDMULH16x8; break;
               case Iop_Sh8Sx16:        op = ARM64vecb_SSHL8x16; break;
               case Iop_Sh16Sx8:        op = ARM64vecb_SSHL16x8; break;
               case Iop_Sh32Sx4:        op = ARM64vecb_SSHL32x4; break;
               case Iop_Sh64Sx2:        op = ARM64vecb_SSHL64x2; break;
               case Iop_Sh8Ux16:        op = ARM64vecb_USHL8x16; break;
               case Iop_Sh16Ux8:        op = ARM64vecb_USHL16x8; break;
               case Iop_Sh32Ux4:        op = ARM64vecb_USHL32x4; break;
               case Iop_Sh64Ux2:        op = ARM64vecb_USHL64x2; break;
               case Iop_Rsh8Sx16:       op = ARM64vecb_SRSHL8x16; break;
               case Iop_Rsh16Sx8:       op = ARM64vecb_SRSHL16x8; break;
               case Iop_Rsh32Sx4:       op = ARM64vecb_SRSHL32x4; break;
               case Iop_Rsh64Sx2:       op = ARM64vecb_SRSHL64x2; break;
               case Iop_Rsh8Ux16:       op = ARM64vecb_URSHL8x16; break;
               case Iop_Rsh16Ux8:       op = ARM64vecb_URSHL16x8; break;
               case Iop_Rsh32Ux4:       op = ARM64vecb_URSHL32x4; break;
               case Iop_Rsh64Ux2:       op = ARM64vecb_URSHL64x2; break;
               case Iop_Max64Fx2:       op = ARM64vecb_FMAX64x2; break;
               case Iop_Max32Fx4:       op = ARM64vecb_FMAX32x4; break;
               case Iop_Min64Fx2:       op = ARM64vecb_FMIN64x2; break;
               case Iop_Min32Fx4:       op = ARM64vecb_FMIN32x4; break;
               case Iop_RecipStep64Fx2: setRM = True;
                                        op = ARM64vecb_FRECPS64x2; break;
               case Iop_RecipStep32Fx4: setRM = True;
                                        op = ARM64vecb_FRECPS32x4; break;
               case Iop_RSqrtStep64Fx2: setRM = True;
                                        op = ARM64vecb_FRSQRTS64x2; break;
               case Iop_RSqrtStep32Fx4: setRM = True;
                                        op = ARM64vecb_FRSQRTS32x4; break;
               default: vassert(0);
            }
            if (setRM) {
               // This is a bit of a kludge.  We should do rm properly for
               // these recip-step insns, but that would require changing the
               // primop's type to take an rmode.
               set_FPCR_rounding_mode(env, IRExpr_Const(
                                              IRConst_U32(Irrm_NEAREST)));
            }
            if (sw) {
               addInstr(env, ARM64Instr_VBinV(op, res, argR, argL));
            } else {
               addInstr(env, ARM64Instr_VBinV(op, res, argL, argR));
            }
            return res;
         }
         /* -- These only have 2 operand instructions, so we have to first move
            the first argument into a new register, for modification. -- */
         case Iop_QAddExtUSsatSS8x16: case Iop_QAddExtUSsatSS16x8:
         case Iop_QAddExtUSsatSS32x4: case Iop_QAddExtUSsatSS64x2:
         case Iop_QAddExtSUsatUU8x16: case Iop_QAddExtSUsatUU16x8:
         case Iop_QAddExtSUsatUU32x4: case Iop_QAddExtSUsatUU64x2:
         {
            HReg res  = newVRegV(env);
            HReg argL = iselV128Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselV128Expr(env, e->Iex.Binop.arg2);
            ARM64VecModifyOp op = ARM64vecmo_INVALID;
            switch (e->Iex.Binop.op) {
               /* In the following 8 cases, the US - SU switching is intended.
                  See comments on the libvex_ir.h for details.  Also in the 
                  ARM64 front end, where used these primops are generated. */
               case Iop_QAddExtUSsatSS8x16: op = ARM64vecmo_SUQADD8x16; break;
               case Iop_QAddExtUSsatSS16x8: op = ARM64vecmo_SUQADD16x8; break;
               case Iop_QAddExtUSsatSS32x4: op = ARM64vecmo_SUQADD32x4; break;
               case Iop_QAddExtUSsatSS64x2: op = ARM64vecmo_SUQADD64x2; break;
               case Iop_QAddExtSUsatUU8x16: op = ARM64vecmo_USQADD8x16; break;
               case Iop_QAddExtSUsatUU16x8: op = ARM64vecmo_USQADD16x8; break;
               case Iop_QAddExtSUsatUU32x4: op = ARM64vecmo_USQADD32x4; break;
               case Iop_QAddExtSUsatUU64x2: op = ARM64vecmo_USQADD64x2; break;
               default: vassert(0);
            }
            /* The order of the operands is important.  Although this is
               basically addition, the two operands are extended differently,
               making it important to get them into the correct registers in
               the instruction. */
            addInstr(env, ARM64Instr_VMov(16, res, argR));
            addInstr(env, ARM64Instr_VModifyV(op, res, argL));
            return res;
         }
         /* -- Shifts by an immediate. -- */
         case Iop_ShrN64x2: case Iop_ShrN32x4:
         case Iop_ShrN16x8: case Iop_ShrN8x16:
         case Iop_SarN64x2: case Iop_SarN32x4:
         case Iop_SarN16x8: case Iop_SarN8x16:
         case Iop_ShlN64x2: case Iop_ShlN32x4:
         case Iop_ShlN16x8: case Iop_ShlN8x16:
         case Iop_QShlNsatUU64x2: case Iop_QShlNsatUU32x4:
         case Iop_QShlNsatUU16x8: case Iop_QShlNsatUU8x16:
         case Iop_QShlNsatSS64x2: case Iop_QShlNsatSS32x4:
         case Iop_QShlNsatSS16x8: case Iop_QShlNsatSS8x16:
         case Iop_QShlNsatSU64x2: case Iop_QShlNsatSU32x4:
         case Iop_QShlNsatSU16x8: case Iop_QShlNsatSU8x16:
         {
            IRExpr* argL = e->Iex.Binop.arg1;
            IRExpr* argR = e->Iex.Binop.arg2;
            if (argR->tag == Iex_Const && argR->Iex.Const.con->tag == Ico_U8) {
               UInt amt   = argR->Iex.Const.con->Ico.U8;
               UInt limLo = 0;
               UInt limHi = 0;
               ARM64VecShiftImmOp op = ARM64vecshi_INVALID;
               /* Establish the instruction to use. */
               switch (e->Iex.Binop.op) {
                  case Iop_ShrN64x2:       op = ARM64vecshi_USHR64x2;   break;
                  case Iop_ShrN32x4:       op = ARM64vecshi_USHR32x4;   break;
                  case Iop_ShrN16x8:       op = ARM64vecshi_USHR16x8;   break;
                  case Iop_ShrN8x16:       op = ARM64vecshi_USHR8x16;   break;
                  case Iop_SarN64x2:       op = ARM64vecshi_SSHR64x2;   break;
                  case Iop_SarN32x4:       op = ARM64vecshi_SSHR32x4;   break;
                  case Iop_SarN16x8:       op = ARM64vecshi_SSHR16x8;   break;
                  case Iop_SarN8x16:       op = ARM64vecshi_SSHR8x16;   break;
                  case Iop_ShlN64x2:       op = ARM64vecshi_SHL64x2;    break;
                  case Iop_ShlN32x4:       op = ARM64vecshi_SHL32x4;    break;
                  case Iop_ShlN16x8:       op = ARM64vecshi_SHL16x8;    break;
                  case Iop_ShlN8x16:       op = ARM64vecshi_SHL8x16;    break;
                  case Iop_QShlNsatUU64x2: op = ARM64vecshi_UQSHL64x2;  break;
                  case Iop_QShlNsatUU32x4: op = ARM64vecshi_UQSHL32x4;  break;
                  case Iop_QShlNsatUU16x8: op = ARM64vecshi_UQSHL16x8;  break;
                  case Iop_QShlNsatUU8x16: op = ARM64vecshi_UQSHL8x16;  break;
                  case Iop_QShlNsatSS64x2: op = ARM64vecshi_SQSHL64x2;  break;
                  case Iop_QShlNsatSS32x4: op = ARM64vecshi_SQSHL32x4;  break;
                  case Iop_QShlNsatSS16x8: op = ARM64vecshi_SQSHL16x8;  break;
                  case Iop_QShlNsatSS8x16: op = ARM64vecshi_SQSHL8x16;  break;
                  case Iop_QShlNsatSU64x2: op = ARM64vecshi_SQSHLU64x2; break;
                  case Iop_QShlNsatSU32x4: op = ARM64vecshi_SQSHLU32x4; break;
                  case Iop_QShlNsatSU16x8: op = ARM64vecshi_SQSHLU16x8; break;
                  case Iop_QShlNsatSU8x16: op = ARM64vecshi_SQSHLU8x16; break;
                  default: vassert(0);
               }
               /* Establish the shift limits, for sanity check purposes only. */
               switch (e->Iex.Binop.op) {
                  case Iop_ShrN64x2:       limLo = 1; limHi = 64; break;
                  case Iop_ShrN32x4:       limLo = 1; limHi = 32; break;
                  case Iop_ShrN16x8:       limLo = 1; limHi = 16; break;
                  case Iop_ShrN8x16:       limLo = 1; limHi = 8;  break;
                  case Iop_SarN64x2:       limLo = 1; limHi = 64; break;
                  case Iop_SarN32x4:       limLo = 1; limHi = 32; break;
                  case Iop_SarN16x8:       limLo = 1; limHi = 16; break;
                  case Iop_SarN8x16:       limLo = 1; limHi = 8;  break;
                  case Iop_ShlN64x2:       limLo = 0; limHi = 63; break;
                  case Iop_ShlN32x4:       limLo = 0; limHi = 31; break;
                  case Iop_ShlN16x8:       limLo = 0; limHi = 15; break;
                  case Iop_ShlN8x16:       limLo = 0; limHi = 7;  break;
                  case Iop_QShlNsatUU64x2: limLo = 0; limHi = 63; break;
                  case Iop_QShlNsatUU32x4: limLo = 0; limHi = 31; break;
                  case Iop_QShlNsatUU16x8: limLo = 0; limHi = 15; break;
                  case Iop_QShlNsatUU8x16: limLo = 0; limHi = 7;  break;
                  case Iop_QShlNsatSS64x2: limLo = 0; limHi = 63; break;
                  case Iop_QShlNsatSS32x4: limLo = 0; limHi = 31; break;
                  case Iop_QShlNsatSS16x8: limLo = 0; limHi = 15; break;
                  case Iop_QShlNsatSS8x16: limLo = 0; limHi = 7;  break;
                  case Iop_QShlNsatSU64x2: limLo = 0; limHi = 63; break;
                  case Iop_QShlNsatSU32x4: limLo = 0; limHi = 31; break;
                  case Iop_QShlNsatSU16x8: limLo = 0; limHi = 15; break;
                  case Iop_QShlNsatSU8x16: limLo = 0; limHi = 7;  break;
                  default: vassert(0);
               }
               /* For left shifts, the allowable amt values are
                  0 .. lane_bits-1.  For right shifts the allowable
                  values are 1 .. lane_bits. */
               if (op != ARM64vecshi_INVALID && amt >= limLo && amt <= limHi) {
                  HReg src = iselV128Expr(env, argL);
                  HReg dst = newVRegV(env);
                  addInstr(env, ARM64Instr_VShiftImmV(op, dst, src, amt));
                  return dst;
               }
               /* Special case some no-op shifts that the arm64 front end
                  throws at us.  We can't generate any instructions for these,
                  but we don't need to either. */
               switch (e->Iex.Binop.op) {
                  case Iop_ShrN64x2: case Iop_ShrN32x4:
                  case Iop_ShrN16x8: case Iop_ShrN8x16:
                     if (amt == 0) {
                        return iselV128Expr(env, argL);
                     }
                     break;
                  default:
                     break;
               }
               /* otherwise unhandled */
            }
            /* else fall out; this is unhandled */
            break;
         }
         /* -- Saturating narrowing by an immediate -- */
         /* uu */
         case Iop_QandQShrNnarrow16Uto8Ux8:
         case Iop_QandQShrNnarrow32Uto16Ux4:
         case Iop_QandQShrNnarrow64Uto32Ux2:
         /* ss */
         case Iop_QandQSarNnarrow16Sto8Sx8:
         case Iop_QandQSarNnarrow32Sto16Sx4:
         case Iop_QandQSarNnarrow64Sto32Sx2:
         /* su */
         case Iop_QandQSarNnarrow16Sto8Ux8:
         case Iop_QandQSarNnarrow32Sto16Ux4:
         case Iop_QandQSarNnarrow64Sto32Ux2:
         /* ruu */
         case Iop_QandQRShrNnarrow16Uto8Ux8:
         case Iop_QandQRShrNnarrow32Uto16Ux4:
         case Iop_QandQRShrNnarrow64Uto32Ux2:
         /* rss */
         case Iop_QandQRSarNnarrow16Sto8Sx8:
         case Iop_QandQRSarNnarrow32Sto16Sx4:
         case Iop_QandQRSarNnarrow64Sto32Sx2:
         /* rsu */
         case Iop_QandQRSarNnarrow16Sto8Ux8:
         case Iop_QandQRSarNnarrow32Sto16Ux4:
         case Iop_QandQRSarNnarrow64Sto32Ux2:
         {
            IRExpr* argL = e->Iex.Binop.arg1;
            IRExpr* argR = e->Iex.Binop.arg2;
            if (argR->tag == Iex_Const && argR->Iex.Const.con->tag == Ico_U8) {
               UInt amt   = argR->Iex.Const.con->Ico.U8;
               UInt limit = 0;
               ARM64VecShiftImmOp op = ARM64vecshi_INVALID;
               switch (e->Iex.Binop.op) {
                  /* uu */
                  case Iop_QandQShrNnarrow64Uto32Ux2:
                     op = ARM64vecshi_UQSHRN2SD; limit = 64; break;
                  case Iop_QandQShrNnarrow32Uto16Ux4:
                     op = ARM64vecshi_UQSHRN4HS; limit = 32; break;
                  case Iop_QandQShrNnarrow16Uto8Ux8:
                     op = ARM64vecshi_UQSHRN8BH; limit = 16; break;
                  /* ss */
                  case Iop_QandQSarNnarrow64Sto32Sx2:
                     op = ARM64vecshi_SQSHRN2SD; limit = 64; break;
                  case Iop_QandQSarNnarrow32Sto16Sx4:
                     op = ARM64vecshi_SQSHRN4HS; limit = 32; break;
                  case Iop_QandQSarNnarrow16Sto8Sx8:
                     op = ARM64vecshi_SQSHRN8BH; limit = 16; break;
                  /* su */
                  case Iop_QandQSarNnarrow64Sto32Ux2:
                     op = ARM64vecshi_SQSHRUN2SD; limit = 64; break;
                  case Iop_QandQSarNnarrow32Sto16Ux4:
                     op = ARM64vecshi_SQSHRUN4HS; limit = 32; break;
                  case Iop_QandQSarNnarrow16Sto8Ux8:
                     op = ARM64vecshi_SQSHRUN8BH; limit = 16; break;
                  /* ruu */
                  case Iop_QandQRShrNnarrow64Uto32Ux2:
                     op = ARM64vecshi_UQRSHRN2SD; limit = 64; break;
                  case Iop_QandQRShrNnarrow32Uto16Ux4:
                     op = ARM64vecshi_UQRSHRN4HS; limit = 32; break;
                  case Iop_QandQRShrNnarrow16Uto8Ux8:
                     op = ARM64vecshi_UQRSHRN8BH; limit = 16; break;
                  /* rss */
                  case Iop_QandQRSarNnarrow64Sto32Sx2:
                     op = ARM64vecshi_SQRSHRN2SD; limit = 64; break;
                  case Iop_QandQRSarNnarrow32Sto16Sx4:
                     op = ARM64vecshi_SQRSHRN4HS; limit = 32; break;
                  case Iop_QandQRSarNnarrow16Sto8Sx8:
                     op = ARM64vecshi_SQRSHRN8BH; limit = 16; break;
                  /* rsu */
                  case Iop_QandQRSarNnarrow64Sto32Ux2:
                     op = ARM64vecshi_SQRSHRUN2SD; limit = 64; break;
                  case Iop_QandQRSarNnarrow32Sto16Ux4:
                     op = ARM64vecshi_SQRSHRUN4HS; limit = 32; break;
                  case Iop_QandQRSarNnarrow16Sto8Ux8:
                     op = ARM64vecshi_SQRSHRUN8BH; limit = 16; break;
                  /**/
                  default:
                     vassert(0);
               }
               if (op != ARM64vecshi_INVALID && amt >= 1 && amt <= limit) {
                  HReg src  = iselV128Expr(env, argL);
                  HReg dst  = newVRegV(env);
                  HReg fpsr = newVRegI(env);
                  /* Clear FPSR.Q, do the operation, and return both its
                     result and the new value of FPSR.Q.  We can simply
                     zero out FPSR since all the other bits have no relevance
                     in VEX generated code. */
                  addInstr(env, ARM64Instr_Imm64(fpsr, 0));
                  addInstr(env, ARM64Instr_FPSR(True/*toFPSR*/, fpsr));
                  addInstr(env, ARM64Instr_VShiftImmV(op, dst, src, amt));
                  addInstr(env, ARM64Instr_FPSR(False/*!toFPSR*/, fpsr));
                  addInstr(env, ARM64Instr_Shift(fpsr, fpsr, ARM64RI6_I6(27),
                                                             ARM64sh_SHR));
                  ARM64RIL* ril_one = mb_mkARM64RIL_I(1);
                  vassert(ril_one);
                  addInstr(env, ARM64Instr_Logic(fpsr,
                                                 fpsr, ril_one, ARM64lo_AND));
                  /* Now we have: the main (shift) result in the bottom half
                     of |dst|, and the Q bit at the bottom of |fpsr|.  
                     Combining them with a "InterleaveLO64x2" style operation 
                     produces a 128 bit value, dst[63:0]:fpsr[63:0], 
                     which is what we want. */
                  HReg scratch = newVRegV(env);
                  addInstr(env, ARM64Instr_VQfromX(scratch, fpsr));
                  addInstr(env, ARM64Instr_VBinV(ARM64vecb_UZP164x2,
                                                 dst, dst, scratch));
                  return dst;
               }
            }
            /* else fall out; this is unhandled */
            break;
         }

         // Use Iop_SliceV128 in preference to Iop_ShlV128 and Iop_ShrV128,
         // as it is in some ways more general and often leads to better
         // code overall. 
         case Iop_ShlV128:
         case Iop_ShrV128: {
            Bool isSHR = e->Iex.Binop.op == Iop_ShrV128;
            /* This is tricky.  Generate an EXT instruction with zeroes in
               the high operand (shift right) or low operand (shift left).
               Note that we can only slice in the EXT instruction at a byte
               level of granularity, so the shift amount needs careful
               checking. */
            IRExpr* argL = e->Iex.Binop.arg1;
            IRExpr* argR = e->Iex.Binop.arg2;
            if (argR->tag == Iex_Const && argR->Iex.Const.con->tag == Ico_U8) {
               UInt amt   = argR->Iex.Const.con->Ico.U8;
               Bool amtOK = False;
               switch (amt) {
                  case 0x08: case 0x10: case 0x18: case 0x20: case 0x28:
                  case 0x30: case 0x38: case 0x40: case 0x48: case 0x50:
                  case 0x58: case 0x60: case 0x68: case 0x70: case 0x78:
                     amtOK = True; break;
               }
               /* We could also deal with amt==0 by copying the source to
                  the destination, but there's no need for that so far. */
               if (amtOK) {
                  HReg src  = iselV128Expr(env, argL);
                  HReg srcZ = newVRegV(env);
                  addInstr(env, ARM64Instr_VImmQ(srcZ, 0x0000));
                  UInt immB = amt / 8;
                  vassert(immB >= 1 && immB <= 15);
                  HReg dst = newVRegV(env);
                  if (isSHR) {
                    addInstr(env, ARM64Instr_VExtV(dst, src/*lo*/, srcZ/*hi*/,
                                                         immB));
                  } else {
                    addInstr(env, ARM64Instr_VExtV(dst, srcZ/*lo*/, src/*hi*/,
                                                         16 - immB));
                  }
                  return dst;
               }
            }
            /* else fall out; this is unhandled */
            break;
         }

         case Iop_PolynomialMull8x8:
         case Iop_Mull32Ux2:
         case Iop_Mull16Ux4:
         case Iop_Mull8Ux8:
         case Iop_Mull32Sx2:
         case Iop_Mull16Sx4:
         case Iop_Mull8Sx8:
         case Iop_QDMull32Sx2:
         case Iop_QDMull16Sx4:
         {
            HReg iSrcL = iselIntExpr_R(env, e->Iex.Binop.arg1);
            HReg iSrcR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg vSrcL = newVRegV(env);
            HReg vSrcR = newVRegV(env);
            HReg dst   = newVRegV(env);
            ARM64VecBinOp op = ARM64vecb_INVALID;
            switch (e->Iex.Binop.op) {
               case Iop_PolynomialMull8x8: op = ARM64vecb_PMULL8x8;    break;
               case Iop_Mull32Ux2:         op = ARM64vecb_UMULL2DSS;   break;
               case Iop_Mull16Ux4:         op = ARM64vecb_UMULL4SHH;   break;
               case Iop_Mull8Ux8:          op = ARM64vecb_UMULL8HBB;   break;
               case Iop_Mull32Sx2:         op = ARM64vecb_SMULL2DSS;   break;
               case Iop_Mull16Sx4:         op = ARM64vecb_SMULL4SHH;   break;
               case Iop_Mull8Sx8:          op = ARM64vecb_SMULL8HBB;   break;
               case Iop_QDMull32Sx2:       op = ARM64vecb_SQDMULL2DSS; break;
               case Iop_QDMull16Sx4:       op = ARM64vecb_SQDMULL4SHH; break;
               default: vassert(0);
            }
            addInstr(env, ARM64Instr_VQfromXX(vSrcL, iSrcL, iSrcL));
            addInstr(env, ARM64Instr_VQfromXX(vSrcR, iSrcR, iSrcR));
            addInstr(env, ARM64Instr_VBinV(op, dst, vSrcL, vSrcR));
            return dst;
         }

         /* ... */
         default:
            break;
      } /* switch on the binop */
   } /* if (e->tag == Iex_Binop) */

   if (e->tag == Iex_Triop) {
      IRTriop*      triop  = e->Iex.Triop.details;
      ARM64VecBinOp vecbop = ARM64vecb_INVALID;
      switch (triop->op) {
         case Iop_Add64Fx2: vecbop = ARM64vecb_FADD64x2; break;
         case Iop_Sub64Fx2: vecbop = ARM64vecb_FSUB64x2; break;
         case Iop_Mul64Fx2: vecbop = ARM64vecb_FMUL64x2; break;
         case Iop_Div64Fx2: vecbop = ARM64vecb_FDIV64x2; break;
         case Iop_Add32Fx4: vecbop = ARM64vecb_FADD32x4; break;
         case Iop_Sub32Fx4: vecbop = ARM64vecb_FSUB32x4; break;
         case Iop_Mul32Fx4: vecbop = ARM64vecb_FMUL32x4; break;
         case Iop_Div32Fx4: vecbop = ARM64vecb_FDIV32x4; break;
         default: break;
      }
      if (vecbop != ARM64vecb_INVALID) {
         HReg argL = iselV128Expr(env, triop->arg2);
         HReg argR = iselV128Expr(env, triop->arg3);
         HReg dst  = newVRegV(env);
         set_FPCR_rounding_mode(env, triop->arg1);
         addInstr(env, ARM64Instr_VBinV(vecbop, dst, argL, argR));
         return dst;
      }

      if (triop->op == Iop_SliceV128) {
         /* Note that, compared to ShlV128/ShrV128 just above, the shift
            amount here is in bytes, not bits. */
         IRExpr* argHi  = triop->arg1;
         IRExpr* argLo  = triop->arg2;
         IRExpr* argAmt = triop->arg3;
         if (argAmt->tag == Iex_Const && argAmt->Iex.Const.con->tag == Ico_U8) {
            UInt amt   = argAmt->Iex.Const.con->Ico.U8;
            Bool amtOK = amt >= 1 && amt <= 15;
            /* We could also deal with amt==0 by copying argLO to
               the destination, but there's no need for that so far. */
            if (amtOK) {
               HReg srcHi = iselV128Expr(env, argHi);
               HReg srcLo = iselV128Expr(env, argLo);
               HReg dst = newVRegV(env);
              addInstr(env, ARM64Instr_VExtV(dst, srcLo, srcHi, amt));
               return dst;
            }
         }
         /* else fall out; this is unhandled */
      }

   } /* if (e->tag == Iex_Triop) */

  v128_expr_bad:
   ppIRExpr(e);
   vpanic("iselV128Expr_wrk");
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
   HReg r = iselDblExpr_wrk( env, e );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselDblExpr_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_F64);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Const) {
      IRConst* con = e->Iex.Const.con;
      if (con->tag == Ico_F64i) {
         HReg src = newVRegI(env);
         HReg dst = newVRegD(env);
         addInstr(env, ARM64Instr_Imm64(src, con->Ico.F64i));
         addInstr(env, ARM64Instr_VDfromX(dst, src));
         return dst;
      }
      if (con->tag == Ico_F64) {
         HReg src = newVRegI(env);
         HReg dst = newVRegD(env);
         union { Double d64; ULong u64; } u;
         vassert(sizeof(u) == 8);
         u.d64 = con->Ico.F64;
         addInstr(env, ARM64Instr_Imm64(src, u.u64));
         addInstr(env, ARM64Instr_VDfromX(dst, src));
         return dst;
      }
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      vassert(e->Iex.Load.ty == Ity_F64);
      HReg addr = iselIntExpr_R(env, e->Iex.Load.addr);
      HReg res  = newVRegD(env);
      addInstr(env, ARM64Instr_VLdStD(True/*isLoad*/, res, addr, 0));
      return res;
   }

   if (e->tag == Iex_Get) {
      Int offs = e->Iex.Get.offset;
      if (offs >= 0 && offs < 32768 && 0 == (offs & 7)) {
         HReg rD = newVRegD(env);
         HReg rN = get_baseblock_register();
         addInstr(env, ARM64Instr_VLdStD(True/*isLoad*/, rD, rN, offs));
         return rD;
      }
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
         case Iop_NegF64: {
            HReg src = iselDblExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARM64Instr_VUnaryD(ARM64fpu_NEG, dst, src));
            return dst;
         }
         case Iop_AbsF64: {
            HReg src = iselDblExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARM64Instr_VUnaryD(ARM64fpu_ABS, dst, src));
            return dst;
         }
         case Iop_F32toF64: {
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARM64Instr_VCvtSD(True/*sToD*/, dst, src));
            return dst;
         }
         case Iop_F16toF64: {
            HReg src = iselF16Expr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARM64Instr_VCvtHD(True/*hToD*/, dst, src));
            return dst;
         }
         case Iop_I32UtoF64:
         case Iop_I32StoF64: {
            /* Rounding mode is not involved here, since the
               conversion can always be done without loss of
               precision. */
            HReg src   = iselIntExpr_R(env, e->Iex.Unop.arg);
            HReg dst   = newVRegD(env);
            Bool syned = e->Iex.Unop.op == Iop_I32StoF64;
            ARM64CvtOp cvt_op = syned ? ARM64cvt_F64_I32S : ARM64cvt_F64_I32U;
            addInstr(env, ARM64Instr_VCvtI2F(cvt_op, dst, src));
            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_RoundF64toInt:
         case Iop_SqrtF64:
         case Iop_RecpExpF64: {
            HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegD(env);
            set_FPCR_rounding_mode(env, e->Iex.Binop.arg1);
            ARM64FpUnaryOp op = ARM64fpu_INVALID;
            switch (e->Iex.Binop.op) {
               case Iop_RoundF64toInt: op = ARM64fpu_RINT;  break;
               case Iop_SqrtF64:       op = ARM64fpu_SQRT;  break;
               case Iop_RecpExpF64:    op = ARM64fpu_RECPX; break;
               default: vassert(0);
            }
            addInstr(env, ARM64Instr_VUnaryD(op, dst, src));
            return dst;
         }
         case Iop_I64StoF64:
         case Iop_I64UtoF64: {
            ARM64CvtOp cvt_op = e->Iex.Binop.op == Iop_I64StoF64
                                   ? ARM64cvt_F64_I64S : ARM64cvt_F64_I64U;
            HReg srcI = iselIntExpr_R(env, e->Iex.Binop.arg2);
            set_FPCR_rounding_mode(env, e->Iex.Binop.arg1);
            HReg dstS = newVRegD(env);
            addInstr(env, ARM64Instr_VCvtI2F(cvt_op, dstS, srcI));
            return dstS;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Triop) {
      IRTriop*     triop = e->Iex.Triop.details;
      ARM64FpBinOp dblop = ARM64fpb_INVALID;
      switch (triop->op) {
         case Iop_DivF64: dblop = ARM64fpb_DIV; break;
         case Iop_MulF64: dblop = ARM64fpb_MUL; break;
         case Iop_SubF64: dblop = ARM64fpb_SUB; break;
         case Iop_AddF64: dblop = ARM64fpb_ADD; break;
         default: break;
      }
      if (dblop != ARM64fpb_INVALID) {
         HReg argL = iselDblExpr(env, triop->arg2);
         HReg argR = iselDblExpr(env, triop->arg3);
         HReg dst  = newVRegD(env);
         set_FPCR_rounding_mode(env, triop->arg1);
         addInstr(env, ARM64Instr_VBinD(dblop, dst, argL, argR));
         return dst;
      }
   }

   if (e->tag == Iex_ITE) {
      /* ITE(ccexpr, iftrue, iffalse) */
      ARM64CondCode cc;
      HReg r1  = iselDblExpr(env, e->Iex.ITE.iftrue);
      HReg r0  = iselDblExpr(env, e->Iex.ITE.iffalse);
      HReg dst = newVRegD(env);
      cc = iselCondCode(env, e->Iex.ITE.cond);
      addInstr(env, ARM64Instr_VFCSel(dst, r1, r0, cc, True/*64-bit*/));
      return dst;
   }

   ppIRExpr(e);
   vpanic("iselDblExpr_wrk");
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (32 bit)         ---*/
/*---------------------------------------------------------*/

/* Compute a 32-bit floating point value into a register, the identity
   of which is returned.  As with iselIntExpr_R, the reg may be either
   real or virtual; in any case it must not be changed by subsequent
   code emitted by the caller.  Values are generated into HRcFlt64
   registers despite the values themselves being Ity_F32s. */

static HReg iselFltExpr ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselFltExpr_wrk( env, e );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselFltExpr_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_F32);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Const) {
      /* This is something of a kludge.  Since a 32 bit floating point
         zero is just .. all zeroes, just create a 64 bit zero word
         and transfer it.  This avoids having to create a SfromW
         instruction for this specific case. */
      IRConst* con = e->Iex.Const.con;
      if (con->tag == Ico_F32i && con->Ico.F32i == 0) {
         HReg src = newVRegI(env);
         HReg dst = newVRegD(env);
         addInstr(env, ARM64Instr_Imm64(src, 0));
         addInstr(env, ARM64Instr_VDfromX(dst, src));
         return dst;
      }
      if (con->tag == Ico_F32) {
         HReg src = newVRegI(env);
         HReg dst = newVRegD(env);
         union { Float f32; UInt u32; } u;
         vassert(sizeof(u) == 4);
         u.f32 = con->Ico.F32;
         addInstr(env, ARM64Instr_Imm64(src, (ULong)u.u32));
         addInstr(env, ARM64Instr_VDfromX(dst, src));
         return dst;
      }
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      vassert(e->Iex.Load.ty == Ity_F32);
      HReg addr = iselIntExpr_R(env, e->Iex.Load.addr);
      HReg res  = newVRegD(env);
      addInstr(env, ARM64Instr_VLdStS(True/*isLoad*/, res, addr, 0));
      return res;
   }

   if (e->tag == Iex_Get) {
      Int offs = e->Iex.Get.offset;
      if (offs >= 0 && offs < 16384 && 0 == (offs & 3)) {
         HReg rD = newVRegD(env);
         HReg rN = get_baseblock_register();
         addInstr(env, ARM64Instr_VLdStS(True/*isLoad*/, rD, rN, offs));
         return rD;
      }
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
         case Iop_NegF32: {
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARM64Instr_VUnaryS(ARM64fpu_NEG, dst, src));
            return dst;
         }
         case Iop_AbsF32: {
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARM64Instr_VUnaryS(ARM64fpu_ABS, dst, src));
            return dst;
         }
         case Iop_F16toF32: {
            HReg src = iselF16Expr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARM64Instr_VCvtHS(True/*hToS*/, dst, src));
            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_RoundF32toInt:
         case Iop_SqrtF32:
         case Iop_RecpExpF32: {
            HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegD(env);
            set_FPCR_rounding_mode(env, e->Iex.Binop.arg1);
            ARM64FpUnaryOp op = ARM64fpu_INVALID;
            switch (e->Iex.Binop.op) {
               case Iop_RoundF32toInt: op = ARM64fpu_RINT;  break;
               case Iop_SqrtF32:       op = ARM64fpu_SQRT;  break;
               case Iop_RecpExpF32:    op = ARM64fpu_RECPX; break;
               default: vassert(0);
            }
            addInstr(env, ARM64Instr_VUnaryS(op, dst, src));
            return dst;
         }
         case Iop_F64toF32: {
            HReg srcD = iselDblExpr(env, e->Iex.Binop.arg2);
            set_FPCR_rounding_mode(env, e->Iex.Binop.arg1);
            HReg dstS = newVRegD(env);
            addInstr(env, ARM64Instr_VCvtSD(False/*!sToD*/, dstS, srcD));
            return dstS;
         }
         case Iop_I32UtoF32:
         case Iop_I32StoF32:
         case Iop_I64UtoF32:
         case Iop_I64StoF32: {
            ARM64CvtOp cvt_op = ARM64cvt_INVALID;
            switch (e->Iex.Binop.op) {
               case Iop_I32UtoF32: cvt_op = ARM64cvt_F32_I32U; break;
               case Iop_I32StoF32: cvt_op = ARM64cvt_F32_I32S; break;
               case Iop_I64UtoF32: cvt_op = ARM64cvt_F32_I64U; break;
               case Iop_I64StoF32: cvt_op = ARM64cvt_F32_I64S; break;
               default: vassert(0);
            }
            HReg srcI = iselIntExpr_R(env, e->Iex.Binop.arg2);
            set_FPCR_rounding_mode(env, e->Iex.Binop.arg1);
            HReg dstS = newVRegD(env);
            addInstr(env, ARM64Instr_VCvtI2F(cvt_op, dstS, srcI));
            return dstS;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Triop) {
      IRTriop*     triop = e->Iex.Triop.details;
      ARM64FpBinOp sglop = ARM64fpb_INVALID;
      switch (triop->op) {
         case Iop_DivF32: sglop = ARM64fpb_DIV; break;
         case Iop_MulF32: sglop = ARM64fpb_MUL; break;
         case Iop_SubF32: sglop = ARM64fpb_SUB; break;
         case Iop_AddF32: sglop = ARM64fpb_ADD; break;
         default: break;
      }
      if (sglop != ARM64fpb_INVALID) {
         HReg argL = iselFltExpr(env, triop->arg2);
         HReg argR = iselFltExpr(env, triop->arg3);
         HReg dst  = newVRegD(env);
         set_FPCR_rounding_mode(env, triop->arg1);
         addInstr(env, ARM64Instr_VBinS(sglop, dst, argL, argR));
         return dst;
      }
   }

   if (e->tag == Iex_ITE) {
      /* ITE(ccexpr, iftrue, iffalse) */
      ARM64CondCode cc;
      HReg r1  = iselFltExpr(env, e->Iex.ITE.iftrue);
      HReg r0  = iselFltExpr(env, e->Iex.ITE.iffalse);
      HReg dst = newVRegD(env);
      cc = iselCondCode(env, e->Iex.ITE.cond);
      addInstr(env, ARM64Instr_VFCSel(dst, r1, r0, cc, False/*!64-bit*/));
      return dst;
   }

   ppIRExpr(e);
   vpanic("iselFltExpr_wrk");
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (16 bit)         ---*/
/*---------------------------------------------------------*/

/* Compute a 16-bit floating point value into a register, the identity
   of which is returned.  As with iselIntExpr_R, the reg may be either
   real or virtual; in any case it must not be changed by subsequent
   code emitted by the caller.  Values are generated into HRcFlt64
   registers despite the values themselves being Ity_F16s. */

static HReg iselF16Expr ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselF16Expr_wrk( env, e );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselF16Expr_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_F16);

   if (e->tag == Iex_Get) {
      Int offs = e->Iex.Get.offset;
      if (offs >= 0 && offs < 8192 && 0 == (offs & 1)) {
         HReg rD = newVRegD(env);
         HReg rN = get_baseblock_register();
         addInstr(env, ARM64Instr_VLdStH(True/*isLoad*/, rD, rN, offs));
         return rD;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_F32toF16: {
            HReg srcS = iselFltExpr(env, e->Iex.Binop.arg2);
            set_FPCR_rounding_mode(env, e->Iex.Binop.arg1);
            HReg dstH = newVRegD(env);
            addInstr(env, ARM64Instr_VCvtHS(False/*!hToS*/, dstH, srcS));
            return dstH;
         }
         case Iop_F64toF16: {
            HReg srcD = iselDblExpr(env, e->Iex.Binop.arg2);
            set_FPCR_rounding_mode(env, e->Iex.Binop.arg1);
            HReg dstH = newVRegD(env);
            addInstr(env, ARM64Instr_VCvtHD(False/*!hToD*/, dstH, srcD));
            return dstH;
         }
         default:
            break;
      }
   }

   ppIRExpr(e);
   vpanic("iselF16Expr_wrk");
}


/*---------------------------------------------------------*/
/*--- ISEL: Vector expressions (256 bit)                ---*/
/*---------------------------------------------------------*/

static void iselV256Expr ( /*OUT*/HReg* rHi, HReg* rLo, 
                           ISelEnv* env, IRExpr* e )
{
   iselV256Expr_wrk( rHi, rLo, env, e );
   vassert(hregClass(*rHi) == HRcVec128);
   vassert(hregClass(*rLo) == HRcVec128);
   vassert(hregIsVirtual(*rHi));
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY */
static void iselV256Expr_wrk ( /*OUT*/HReg* rHi, /*OUT*/HReg* rLo, 
                               ISelEnv* env, IRExpr* e )
{
   vassert(e);
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_V256);

   /* read 256-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair( rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }
 
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_V128HLtoV256: {
            *rHi = iselV128Expr(env, e->Iex.Binop.arg1);
            *rLo = iselV128Expr(env, e->Iex.Binop.arg2);
            return;
         }
         case Iop_QandSQsh64x2:
         case Iop_QandSQsh32x4:
         case Iop_QandSQsh16x8:
         case Iop_QandSQsh8x16:
         case Iop_QandUQsh64x2:
         case Iop_QandUQsh32x4:
         case Iop_QandUQsh16x8:
         case Iop_QandUQsh8x16:
         case Iop_QandSQRsh64x2:
         case Iop_QandSQRsh32x4:
         case Iop_QandSQRsh16x8:
         case Iop_QandSQRsh8x16:
         case Iop_QandUQRsh64x2:
         case Iop_QandUQRsh32x4:
         case Iop_QandUQRsh16x8:
         case Iop_QandUQRsh8x16:
         {
            HReg argL  = iselV128Expr(env, e->Iex.Binop.arg1);
            HReg argR  = iselV128Expr(env, e->Iex.Binop.arg2);
            HReg fpsr  = newVRegI(env);
            HReg resHi = newVRegV(env);
            HReg resLo = newVRegV(env);
            ARM64VecBinOp op = ARM64vecb_INVALID;
            switch (e->Iex.Binop.op) {
               case Iop_QandSQsh64x2:  op = ARM64vecb_SQSHL64x2;  break;
               case Iop_QandSQsh32x4:  op = ARM64vecb_SQSHL32x4;  break;
               case Iop_QandSQsh16x8:  op = ARM64vecb_SQSHL16x8;  break;
               case Iop_QandSQsh8x16:  op = ARM64vecb_SQSHL8x16;  break;
               case Iop_QandUQsh64x2:  op = ARM64vecb_UQSHL64x2;  break;
               case Iop_QandUQsh32x4:  op = ARM64vecb_UQSHL32x4;  break;
               case Iop_QandUQsh16x8:  op = ARM64vecb_UQSHL16x8;  break;
               case Iop_QandUQsh8x16:  op = ARM64vecb_UQSHL8x16;  break;
               case Iop_QandSQRsh64x2: op = ARM64vecb_SQRSHL64x2; break;
               case Iop_QandSQRsh32x4: op = ARM64vecb_SQRSHL32x4; break;
               case Iop_QandSQRsh16x8: op = ARM64vecb_SQRSHL16x8; break;
               case Iop_QandSQRsh8x16: op = ARM64vecb_SQRSHL8x16; break;
               case Iop_QandUQRsh64x2: op = ARM64vecb_UQRSHL64x2; break;
               case Iop_QandUQRsh32x4: op = ARM64vecb_UQRSHL32x4; break;
               case Iop_QandUQRsh16x8: op = ARM64vecb_UQRSHL16x8; break;
               case Iop_QandUQRsh8x16: op = ARM64vecb_UQRSHL8x16; break;
               default: vassert(0);
            }
            /* Clear FPSR.Q, do the operation, and return both its result
               and the new value of FPSR.Q.  We can simply zero out FPSR
               since all the other bits have no relevance in VEX generated
               code. */
            addInstr(env, ARM64Instr_Imm64(fpsr, 0));
            addInstr(env, ARM64Instr_FPSR(True/*toFPSR*/, fpsr));
            addInstr(env, ARM64Instr_VBinV(op, resLo, argL, argR));
            addInstr(env, ARM64Instr_FPSR(False/*!toFPSR*/, fpsr));
            addInstr(env, ARM64Instr_Shift(fpsr, fpsr, ARM64RI6_I6(27),
                                                       ARM64sh_SHR));
            ARM64RIL* ril_one = mb_mkARM64RIL_I(1);
            vassert(ril_one);
            addInstr(env, ARM64Instr_Logic(fpsr, fpsr, ril_one, ARM64lo_AND));
            /* Now we have: the main (shift) result in |resLo|, and the
               Q bit at the bottom of |fpsr|. */
            addInstr(env, ARM64Instr_VQfromX(resHi, fpsr));
            *rHi = resHi;
            *rLo = resLo;
            return;
         }

         /* ... */
         default:
            break;
      } /* switch on the binop */
   } /* if (e->tag == Iex_Binop) */

   ppIRExpr(e);
   vpanic("iselV256Expr_wrk");
}


/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void iselStmt ( ISelEnv* env, IRStmt* stmt )
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- ");
      ppIRStmt(stmt);
      vex_printf("\n");
   }
   switch (stmt->tag) {

   /* --------- STORE --------- */
   /* little-endian write to memory */
   case Ist_Store: {
      IRType    tya  = typeOfIRExpr(env->type_env, stmt->Ist.Store.addr);
      IRType    tyd  = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);
      IREndness end  = stmt->Ist.Store.end;

      if (tya != Ity_I64 || end != Iend_LE) 
         goto stmt_fail;

      if (tyd == Ity_I64) {
         HReg        rD = iselIntExpr_R(env, stmt->Ist.Store.data);
         ARM64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr, tyd);
         addInstr(env, ARM64Instr_LdSt64(False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I32) {
         HReg        rD = iselIntExpr_R(env, stmt->Ist.Store.data);
         ARM64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr, tyd);
         addInstr(env, ARM64Instr_LdSt32(False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I16) {
         HReg        rD = iselIntExpr_R(env, stmt->Ist.Store.data);
         ARM64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr, tyd);
         addInstr(env, ARM64Instr_LdSt16(False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I8) {
         HReg        rD = iselIntExpr_R(env, stmt->Ist.Store.data);
         ARM64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr, tyd);
         addInstr(env, ARM64Instr_LdSt8(False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_V128) {
         HReg qD   = iselV128Expr(env, stmt->Ist.Store.data);
         HReg addr = iselIntExpr_R(env, stmt->Ist.Store.addr);
         addInstr(env, ARM64Instr_VLdStQ(False/*!isLoad*/, qD, addr));
         return;
      }
      if (tyd == Ity_F64) {
         HReg dD   = iselDblExpr(env, stmt->Ist.Store.data);
         HReg addr = iselIntExpr_R(env, stmt->Ist.Store.addr);
         addInstr(env, ARM64Instr_VLdStD(False/*!isLoad*/, dD, addr, 0));
         return;
      }
      if (tyd == Ity_F32) {
         HReg sD   = iselFltExpr(env, stmt->Ist.Store.data);
         HReg addr = iselIntExpr_R(env, stmt->Ist.Store.addr);
         addInstr(env, ARM64Instr_VLdStS(False/*!isLoad*/, sD, addr, 0));
         return;
      }
      break;
   }

   /* --------- PUT --------- */
   /* write guest state, fixed offset */
   case Ist_Put: {
      IRType tyd  = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
      UInt   offs = (UInt)stmt->Ist.Put.offset;
      if (tyd == Ity_I64 && 0 == (offs & 7) && offs < (8<<12)) {
         HReg        rD = iselIntExpr_R(env, stmt->Ist.Put.data);
         ARM64AMode* am = mk_baseblock_64bit_access_amode(offs);
         addInstr(env, ARM64Instr_LdSt64(False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I32 && 0 == (offs & 3) && offs < (4<<12)) {
         HReg        rD = iselIntExpr_R(env, stmt->Ist.Put.data);
         ARM64AMode* am = mk_baseblock_32bit_access_amode(offs);
         addInstr(env, ARM64Instr_LdSt32(False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I16 && 0 == (offs & 1) && offs < (2<<12)) {
         HReg        rD = iselIntExpr_R(env, stmt->Ist.Put.data);
         ARM64AMode* am = mk_baseblock_16bit_access_amode(offs);
         addInstr(env, ARM64Instr_LdSt16(False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I8 && offs < (1<<12)) {
         HReg        rD = iselIntExpr_R(env, stmt->Ist.Put.data);
         ARM64AMode* am = mk_baseblock_8bit_access_amode(offs);
         addInstr(env, ARM64Instr_LdSt8(False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_V128 && offs < (1<<12)) {
         HReg qD   = iselV128Expr(env, stmt->Ist.Put.data);
         HReg addr = mk_baseblock_128bit_access_addr(env, offs);
         addInstr(env, ARM64Instr_VLdStQ(False/*!isLoad*/, qD, addr));
         return;
      }
      if (tyd == Ity_F64 && 0 == (offs & 7) && offs < (8<<12)) {
         HReg dD   = iselDblExpr(env, stmt->Ist.Put.data);
         HReg bbp  = get_baseblock_register();
         addInstr(env, ARM64Instr_VLdStD(False/*!isLoad*/, dD, bbp, offs));
         return;
      }
      if (tyd == Ity_F32 && 0 == (offs & 3) && offs < (4<<12)) {
         HReg sD   = iselFltExpr(env, stmt->Ist.Put.data);
         HReg bbp  = get_baseblock_register();
         addInstr(env, ARM64Instr_VLdStS(False/*!isLoad*/, sD, bbp, offs));
         return;
      }
      if (tyd == Ity_F16 && 0 == (offs & 1) && offs < (2<<12)) {
         HReg hD   = iselF16Expr(env, stmt->Ist.Put.data);
         HReg bbp  = get_baseblock_register();
         addInstr(env, ARM64Instr_VLdStH(False/*!isLoad*/, hD, bbp, offs));
         return;
      }

      break;
   }

   /* --------- TMP --------- */
   /* assign value to temporary */
   case Ist_WrTmp: {
      IRTemp tmp = stmt->Ist.WrTmp.tmp;
      IRType ty  = typeOfIRTemp(env->type_env, tmp);

      if (ty == Ity_I64 || ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
         /* We could do a lot better here.  But for the time being: */
         HReg dst = lookupIRTemp(env, tmp);
         HReg rD  = iselIntExpr_R(env, stmt->Ist.WrTmp.data);
         addInstr(env, ARM64Instr_MovI(dst, rD));
         return;
      }
      if (ty == Ity_I1) {
         /* Here, we are generating a I1 value into a 64 bit register.
            Make sure the value in the register is only zero or one,
            but no other.  This allows optimisation of the
            1Uto64(tmp:I1) case, by making it simply a copy of the
            register holding 'tmp'.  The point being that the value in
            the register holding 'tmp' can only have been created
            here.  LATER: that seems dangerous; safer to do 'tmp & 1'
            in that case.  Also, could do this just with a single CINC
            insn. */
         /* CLONE-01 */
         HReg zero = newVRegI(env);
         HReg one  = newVRegI(env);
         HReg dst  = lookupIRTemp(env, tmp);
         addInstr(env, ARM64Instr_Imm64(zero, 0));
         addInstr(env, ARM64Instr_Imm64(one,  1));
         ARM64CondCode cc = iselCondCode(env, stmt->Ist.WrTmp.data);
         addInstr(env, ARM64Instr_CSel(dst, one, zero, cc));
         return;
      }
      if (ty == Ity_F64) {
         HReg src = iselDblExpr(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, ARM64Instr_VMov(8, dst, src));
         return;
      }
      if (ty == Ity_F32) {
         HReg src = iselFltExpr(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, ARM64Instr_VMov(8/*yes, really*/, dst, src));
         return;
      }
      if (ty == Ity_V128) {
         HReg src = iselV128Expr(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, ARM64Instr_VMov(16, dst, src));
         return;
      }
      if (ty == Ity_V256) {
         HReg srcHi, srcLo, dstHi, dstLo;
         iselV256Expr(&srcHi,&srcLo, env, stmt->Ist.WrTmp.data);
         lookupIRTempPair( &dstHi, &dstLo, env, tmp);
         addInstr(env, ARM64Instr_VMov(16, dstHi, srcHi));
         addInstr(env, ARM64Instr_VMov(16, dstLo, srcLo));
         return;
      }
      break;
   }

   /* --------- Call to DIRTY helper --------- */
   /* call complex ("dirty") helper function */
   case Ist_Dirty: {
      IRDirty* d = stmt->Ist.Dirty.details;

      /* Figure out the return type, if any. */
      IRType retty = Ity_INVALID;
      if (d->tmp != IRTemp_INVALID)
         retty = typeOfIRTemp(env->type_env, d->tmp);

      Bool retty_ok = False;
      switch (retty) {
         case Ity_INVALID: /* function doesn't return anything */
         case Ity_I64: case Ity_I32: case Ity_I16: case Ity_I8:
         case Ity_V128:
            retty_ok = True; break;
         default:
            break;
      }
      if (!retty_ok)
         break; /* will go to stmt_fail: */

      /* Marshal args, do the call, and set the return value to 0x555..555
         if this is a conditional call that returns a value and the
         call is skipped. */
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
         case Ity_I64: case Ity_I32: case Ity_I16: case Ity_I8: {
            vassert(rloc.pri == RLPri_Int);
            vassert(addToSp == 0);
            /* The returned value is in x0.  Park it in the register
               associated with tmp. */
            HReg dst = lookupIRTemp(env, d->tmp);
            addInstr(env, ARM64Instr_MovI(dst, hregARM64_X0()) );
            return;
         }
         case Ity_V128: {
            /* The returned value is on the stack, and *retloc tells
               us where.  Fish it off the stack and then move the
               stack pointer upwards to clear it, as directed by
               doHelperCall. */
            vassert(rloc.pri == RLPri_V128SpRel);
            vassert(rloc.spOff < 256); // stay sane
            vassert(addToSp >= 16); // ditto
            vassert(addToSp < 256); // ditto
            HReg dst = lookupIRTemp(env, d->tmp);
            HReg tmp = newVRegI(env); // the address of the returned value
            addInstr(env, ARM64Instr_FromSP(tmp)); // tmp = SP
            addInstr(env, ARM64Instr_Arith(tmp, tmp,
                                           ARM64RIA_I12((UShort)rloc.spOff, 0),
                                           True/*isAdd*/ ));
            addInstr(env, ARM64Instr_VLdStQ(True/*isLoad*/, dst, tmp));
            addInstr(env, ARM64Instr_AddToSP(addToSp));
            return;
         }
         default:
            /*NOTREACHED*/
            vassert(0);
      }
      break;
   }

   /* --------- Load Linked and Store Conditional --------- */
   case Ist_LLSC: {
      if (stmt->Ist.LLSC.storedata == NULL) {
         /* LL */
         IRTemp res = stmt->Ist.LLSC.result;
         IRType ty  = typeOfIRTemp(env->type_env, res);
         if (ty == Ity_I64 || ty == Ity_I32 
             || ty == Ity_I16 || ty == Ity_I8) {
            Int  szB   = 0;
            HReg r_dst = lookupIRTemp(env, res);
            HReg raddr = iselIntExpr_R(env, stmt->Ist.LLSC.addr);
            switch (ty) {
               case Ity_I8:  szB = 1; break;
               case Ity_I16: szB = 2; break;
               case Ity_I32: szB = 4; break;
               case Ity_I64: szB = 8; break;
               default:      vassert(0);
            }
            addInstr(env, ARM64Instr_MovI(hregARM64_X4(), raddr));
            addInstr(env, ARM64Instr_LdrEX(szB));
            addInstr(env, ARM64Instr_MovI(r_dst, hregARM64_X2()));
            return;
         }
         goto stmt_fail;
      } else {
         /* SC */
         IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.storedata);
         if (tyd == Ity_I64 || tyd == Ity_I32
             || tyd == Ity_I16 || tyd == Ity_I8) {
            Int  szB = 0;
            HReg rD  = iselIntExpr_R(env, stmt->Ist.LLSC.storedata);
            HReg rA  = iselIntExpr_R(env, stmt->Ist.LLSC.addr);
            switch (tyd) {
               case Ity_I8:  szB = 1; break;
               case Ity_I16: szB = 2; break;
               case Ity_I32: szB = 4; break;
               case Ity_I64: szB = 8; break;
               default:      vassert(0);
            }
            addInstr(env, ARM64Instr_MovI(hregARM64_X2(), rD));
            addInstr(env, ARM64Instr_MovI(hregARM64_X4(), rA));
            addInstr(env, ARM64Instr_StrEX(szB));
         } else {
            goto stmt_fail;
         }
         /* now r0 is 1 if failed, 0 if success.  Change to IR
            conventions (0 is fail, 1 is success).  Also transfer
            result to r_res. */
         IRTemp    res   = stmt->Ist.LLSC.result;
         IRType    ty    = typeOfIRTemp(env->type_env, res);
         HReg      r_res = lookupIRTemp(env, res);
         ARM64RIL* one   = mb_mkARM64RIL_I(1);
         vassert(ty == Ity_I1);
         vassert(one);
         addInstr(env, ARM64Instr_Logic(r_res, hregARM64_X0(), one,
                                        ARM64lo_XOR));
         /* And be conservative -- mask off all but the lowest bit. */
         addInstr(env, ARM64Instr_Logic(r_res, r_res, one,
                                        ARM64lo_AND));
         return;
      }
      break;
   }

   /* --------- MEM FENCE --------- */
   case Ist_MBE:
      switch (stmt->Ist.MBE.event) {
         case Imbe_Fence:
            addInstr(env, ARM64Instr_MFence());
            return;
         default:
            break;
      }
      break;

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
   case Ist_NoOp:
       return;

   /* --------- EXIT --------- */
   case Ist_Exit: {
      if (stmt->Ist.Exit.dst->tag != Ico_U64)
         vpanic("isel_arm: Ist_Exit: dst is not a 64-bit value");

      ARM64CondCode cc 
         = iselCondCode(env, stmt->Ist.Exit.guard);
      ARM64AMode* amPC
         = mk_baseblock_64bit_access_amode(stmt->Ist.Exit.offsIP);

      /* Case: boring transfer to known address */
      if (stmt->Ist.Exit.jk == Ijk_Boring) {
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = ((Addr64)stmt->Ist.Exit.dst->Ico.U64) > env->max_ga;
            if (0) vex_printf("%s", toFastEP ? "Y" : ",");
            addInstr(env, ARM64Instr_XDirect(stmt->Ist.Exit.dst->Ico.U64,
                                             amPC, cc, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, ARM64Instr_XAssisted(r, amPC, cc, Ijk_Boring));
         }
         return;
      }

      /* Case: assisted transfer to arbitrary address */
      switch (stmt->Ist.Exit.jk) {
         /* Keep this list in sync with that for iselNext below */
         case Ijk_ClientReq:
         case Ijk_NoDecode:
         case Ijk_NoRedir:
         case Ijk_Sys_syscall:
         case Ijk_InvalICache:
         case Ijk_FlushDCache:
         case Ijk_SigTRAP:
         case Ijk_Yield: {
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, ARM64Instr_XAssisted(r, amPC, cc,
                                               stmt->Ist.Exit.jk));
            return;
         }
         default:
            break;
      }

      /* Do we ever expect to see any other kind? */
      goto stmt_fail;
   }

   default: break;
   }
  stmt_fail:
   ppIRStmt(stmt);
   vpanic("iselStmt");
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
      vassert(cdst->tag == Ico_U64);
      if (jk == Ijk_Boring || jk == Ijk_Call) {
         /* Boring transfer to known address */
         ARM64AMode* amPC = mk_baseblock_64bit_access_amode(offsIP);
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = ((Addr64)cdst->Ico.U64) > env->max_ga;
            if (0) vex_printf("%s", toFastEP ? "X" : ".");
            addInstr(env, ARM64Instr_XDirect(cdst->Ico.U64,
                                             amPC, ARM64cc_AL, 
                                             toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, next);
            addInstr(env, ARM64Instr_XAssisted(r, amPC, ARM64cc_AL,
                                               Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
      case Ijk_Boring: case Ijk_Ret: case Ijk_Call: {
         HReg        r    = iselIntExpr_R(env, next);
         ARM64AMode* amPC = mk_baseblock_64bit_access_amode(offsIP);
         if (env->chainingAllowed) {
            addInstr(env, ARM64Instr_XIndir(r, amPC, ARM64cc_AL));
         } else {
            addInstr(env, ARM64Instr_XAssisted(r, amPC, ARM64cc_AL,
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
      case Ijk_NoDecode:
      case Ijk_NoRedir:
      case Ijk_Sys_syscall:
      case Ijk_InvalICache:
      case Ijk_FlushDCache:
      case Ijk_SigTRAP:
      case Ijk_Yield:
      {
         HReg        r    = iselIntExpr_R(env, next);
         ARM64AMode* amPC = mk_baseblock_64bit_access_amode(offsIP);
         addInstr(env, ARM64Instr_XAssisted(r, amPC, ARM64cc_AL, jk));
         return;
      }
      default:
         break;
   }

   vex_printf( "\n-- PUT(%d) = ", offsIP);
   ppIRExpr( next );
   vex_printf( "; exit-");
   ppIRJumpKind(jk);
   vex_printf( "\n");
   vassert(0); // are we expecting any other kind?
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire SB to arm64 code. */

HInstrArray* iselSB_ARM64 ( const IRSB* bb,
                            VexArch      arch_host,
                            const VexArchInfo* archinfo_host,
                            const VexAbiInfo*  vbi/*UNUSED*/,
                            Int offs_Host_EvC_Counter,
                            Int offs_Host_EvC_FailAddr,
                            Bool chainingAllowed,
                            Bool addProfInc,
                            Addr max_ga )
{
   Int        i, j;
   HReg       hreg, hregHI;
   ISelEnv*   env;
   UInt       hwcaps_host = archinfo_host->hwcaps;
   ARM64AMode *amCounter, *amFailAddr;

   /* sanity ... */
   vassert(arch_host == VexArchARM64);

   /* Check that the host's endianness is as expected. */
   vassert(archinfo_host->endness == VexEndnessLE);

   /* guard against unexpected space regressions */
   vassert(sizeof(ARM64Instr) <= 32);

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

   /* and finally ... */
   env->chainingAllowed = chainingAllowed;
   env->hwcaps          = hwcaps_host;
   env->previous_rm     = NULL;
   env->max_ga          = max_ga;

   /* For each IR temporary, allocate a suitably-kinded virtual
      register. */
   j = 0;
   for (i = 0; i < env->n_vregmap; i++) {
      hregHI = hreg = INVALID_HREG;
      switch (bb->tyenv->types[i]) {
         case Ity_I1:
         case Ity_I8: case Ity_I16: case Ity_I32: case Ity_I64:
            hreg = mkHReg(True, HRcInt64, 0, j++);
            break;
         case Ity_I128:
            hreg   = mkHReg(True, HRcInt64, 0, j++);
            hregHI = mkHReg(True, HRcInt64, 0, j++);
            break;
         case Ity_F16: // we'll use HRcFlt64 regs for F16 too
         case Ity_F32: // we'll use HRcFlt64 regs for F32 too
         case Ity_F64:
            hreg = mkHReg(True, HRcFlt64, 0, j++);
            break;
         case Ity_V128:
            hreg = mkHReg(True, HRcVec128, 0, j++);
            break;
         case Ity_V256:
            hreg   = mkHReg(True, HRcVec128, 0, j++);
            hregHI = mkHReg(True, HRcVec128, 0, j++);
            break;
         default:
            ppIRType(bb->tyenv->types[i]);
            vpanic("iselBB(arm64): IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   amCounter  = ARM64AMode_RI9(hregARM64_X21(), offs_Host_EvC_Counter);
   amFailAddr = ARM64AMode_RI9(hregARM64_X21(), offs_Host_EvC_FailAddr);
   addInstr(env, ARM64Instr_EvCheck(amCounter, amFailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, ARM64Instr_ProfInc());
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
/*--- end                                   host_arm64_isel.c ---*/
/*---------------------------------------------------------------*/
