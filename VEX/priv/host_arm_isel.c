
/*---------------------------------------------------------------*/
/*--- begin                                   host_arm_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
      info@open-works.net

   NEON support is
   Copyright (C) 2010-2017 Samsung Electronics
   contributed by Dmitry Zhurikhin <zhur@ispras.ru>
              and Kirill Batuzov <batuzovk@ispras.ru>

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
#include "ir_match.h"

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_generic_simd64.h"  // for 32-bit SIMD helpers
#include "host_arm_defs.h"


/*---------------------------------------------------------*/
/*--- ARMvfp control word stuff                         ---*/
/*---------------------------------------------------------*/

/* Vex-generated code expects to run with the FPU set as follows: all
   exceptions masked, round-to-nearest, non-vector mode, with the NZCV
   flags cleared, and FZ (flush to zero) disabled.  Curiously enough,
   this corresponds to a FPSCR value of zero.

   fpscr should therefore be zero on entry to Vex-generated code, and
   should be unchanged at exit.  (Or at least the bottom 28 bits
   should be zero).
*/

#define DEFAULT_FPSCR 0


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
      Addr32       max_ga;

      /* These are modified as we go along. */
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
   vassert(! hregIsInvalid(env->vregmapHI[tmp]));
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr ( ISelEnv* env, ARMInstr* instr )
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppARMInstr(instr);
      vex_printf("\n");
   }
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcInt32, 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegD ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcFlt64, 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcFlt32, 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegV ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcVec128, 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

/* These are duplicated in guest_arm_toIR.c */
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
/*--- ISEL: Forward declarations                        ---*/
/*---------------------------------------------------------*/

/* These are organised as iselXXX and iselXXX_wrk pairs.  The
   iselXXX_wrk do the real work, but are not to be called directly.
   For each XXX, iselXXX calls its iselXXX_wrk counterpart, then
   checks that all returned registers are virtual.  You should not
   call the _wrk version directly.
*/
static ARMAMode1*  iselIntExpr_AMode1_wrk ( ISelEnv* env, IRExpr* e );
static ARMAMode1*  iselIntExpr_AMode1     ( ISelEnv* env, IRExpr* e );

static ARMAMode2*  iselIntExpr_AMode2_wrk ( ISelEnv* env, IRExpr* e );
static ARMAMode2*  iselIntExpr_AMode2     ( ISelEnv* env, IRExpr* e );

static ARMAModeV*  iselIntExpr_AModeV_wrk ( ISelEnv* env, IRExpr* e );
static ARMAModeV*  iselIntExpr_AModeV     ( ISelEnv* env, IRExpr* e );

static ARMAModeN*  iselIntExpr_AModeN_wrk ( ISelEnv* env, IRExpr* e );
static ARMAModeN*  iselIntExpr_AModeN     ( ISelEnv* env, IRExpr* e );

static ARMRI84*    iselIntExpr_RI84_wrk
        ( /*OUT*/Bool* didInv, Bool mayInv, ISelEnv* env, IRExpr* e );
static ARMRI84*    iselIntExpr_RI84
        ( /*OUT*/Bool* didInv, Bool mayInv, ISelEnv* env, IRExpr* e );

static ARMRI5*     iselIntExpr_RI5_wrk    ( ISelEnv* env, IRExpr* e );
static ARMRI5*     iselIntExpr_RI5        ( ISelEnv* env, IRExpr* e );

static ARMCondCode iselCondCode_wrk       ( ISelEnv* env, IRExpr* e );
static ARMCondCode iselCondCode           ( ISelEnv* env, IRExpr* e );

static HReg        iselIntExpr_R_wrk      ( ISelEnv* env, IRExpr* e );
static HReg        iselIntExpr_R          ( ISelEnv* env, IRExpr* e );

static void        iselInt64Expr_wrk      ( HReg* rHi, HReg* rLo, 
                                            ISelEnv* env, const IRExpr* e );
static void        iselInt64Expr          ( HReg* rHi, HReg* rLo, 
                                            ISelEnv* env, const IRExpr* e );

static HReg        iselDblExpr_wrk        ( ISelEnv* env, IRExpr* e );
static HReg        iselDblExpr            ( ISelEnv* env, IRExpr* e );

static HReg        iselFltExpr_wrk        ( ISelEnv* env, IRExpr* e );
static HReg        iselFltExpr            ( ISelEnv* env, IRExpr* e );

static HReg        iselNeon64Expr_wrk     ( ISelEnv* env, const IRExpr* e );
static HReg        iselNeon64Expr         ( ISelEnv* env, const IRExpr* e );

static HReg        iselNeonExpr_wrk       ( ISelEnv* env, const IRExpr* e );
static HReg        iselNeonExpr           ( ISelEnv* env, const IRExpr* e );

/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

static UInt ROR32 ( UInt x, UInt sh ) {
   vassert(sh >= 0 && sh < 32);
   if (sh == 0)
      return x;
   else
      return (x << (32-sh)) | (x >> sh);
}

/* Figure out if 'u' fits in the special shifter-operand 8x4 immediate
   form, and if so return the components. */
static Bool fitsIn8x4 ( /*OUT*/UInt* u8, /*OUT*/UInt* u4, UInt u )
{
   UInt i;
   for (i = 0; i < 16; i++) {
      if (0 == (u & 0xFFFFFF00)) {
         *u8 = u;
         *u4 = i;
         return True;
      }
      u = ROR32(u, 30);
   }
   vassert(i == 16);
   return False;
}

/* Make a int reg-reg move. */
static ARMInstr* mk_iMOVds_RR ( HReg dst, HReg src )
{
   vassert(hregClass(src) == HRcInt32);
   vassert(hregClass(dst) == HRcInt32);
   return ARMInstr_Mov(dst, ARMRI84_R(src));
}

/* Set the VFP unit's rounding mode to default (round to nearest). */
static void set_VFP_rounding_default ( ISelEnv* env )
{
   /* mov rTmp, #DEFAULT_FPSCR
      fmxr fpscr, rTmp
   */
   HReg rTmp = newVRegI(env);
   addInstr(env, ARMInstr_Imm32(rTmp, DEFAULT_FPSCR));
   addInstr(env, ARMInstr_FPSCR(True/*toFPSCR*/, rTmp));
}

/* Mess with the VFP unit's rounding mode: 'mode' is an I32-typed
   expression denoting a value in the range 0 .. 3, indicating a round
   mode encoded as per type IRRoundingMode.  Set FPSCR to have the
   same rounding.
*/
static
void set_VFP_rounding_mode ( ISelEnv* env, IRExpr* mode )
{
   /* This isn't simple, because 'mode' carries an IR rounding
      encoding, and we need to translate that to an ARMvfp one:
      The IR encoding:
         00  to nearest (the default)
         10  to +infinity
         01  to -infinity
         11  to zero
      The ARMvfp encoding:
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
   addInstr(env, ARMInstr_Shift(ARMsh_SHL, tL, irrm, ARMRI5_I5(1)));
   addInstr(env, ARMInstr_Shift(ARMsh_SHR, tR, irrm, ARMRI5_I5(1)));
   addInstr(env, ARMInstr_Alu(ARMalu_AND, tL, tL, ARMRI84_I84(2,0)));
   addInstr(env, ARMInstr_Alu(ARMalu_AND, tR, tR, ARMRI84_I84(1,0)));
   addInstr(env, ARMInstr_Alu(ARMalu_OR, t3, tL, ARMRI84_R(tR)));
   addInstr(env, ARMInstr_Shift(ARMsh_SHL, t3, t3, ARMRI5_I5(22)));
   addInstr(env, ARMInstr_FPSCR(True/*toFPSCR*/, t3));
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
   if (UNLIKELY(is_IRExpr_VECRET_or_GSPTR(e))) {
      // These are always "safe" -- either a copy of r13(sp) in some
      // arbitrary vreg, or a copy of r8, respectively.
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


static
Bool doHelperCallWithArgsOnStack ( /*OUT*/UInt*   stackAdjustAfterCall,
                                   /*OUT*/RetLoc* retloc,
                                   ISelEnv* env,
                                   IRExpr* guard,
                                   IRCallee* cee, IRType retTy, IRExpr** args )
{
   /* This function deals just with the case where the arg sequence is:
      VECRET followed by between 4 and 12 Ity_I32 values.  So far no other
      cases are necessary or supported. */

   /* Check this matches the required format. */
   if (args[0] == NULL || args[0]->tag != Iex_VECRET)
      goto no_match;

   UInt i;
   UInt n_real_args = 0;
   for (i = 1; args[i]; i++) {
      IRExpr* arg = args[i];
      if (UNLIKELY(is_IRExpr_VECRET_or_GSPTR(arg)))
         goto no_match;
      IRType argTy = typeOfIRExpr(env->type_env, arg);
      if (UNLIKELY(argTy != Ity_I32))
         goto no_match;
      n_real_args++;
   }

   /* We expect to pass at least some args on the stack. */
   if (n_real_args <= 3)
      goto no_match;

   /* But not too many. */
   if (n_real_args > 12)
      goto no_match;

   /* General rules for a call:

      Args 1 .. 4 go in R0 .. R3.  The rest are pushed R to L on the
      stack; that is, arg 5 is at the lowest address, arg 6 at the
      next lowest, etc.

      The stack is to be kept 8 aligned.

      It appears (for unclear reasons) that the highest 3 words made
      available when moving SP downwards are not to be used.  For
      example, if 5 args are to go on the stack, then SP must be moved
      down 32 bytes, and the area at SP+20 .. SP+31 is not to be used
      by the caller.
   */

   /* For this particular case, we use the following layout:

        ------ original SP
        112 bytes
        ------
        return value
        ------ original SP - 128
        space
        args words, between 1 and 11
        ------ new SP = original_SP - 256

      Using 256 bytes is overkill, but it is simple and good enough.
   */

   /* This should really be
        HReg argVRegs[n_real_args];
      but that makes it impossible to do 'goto's forward past.
      Hence the following kludge. */
   vassert(n_real_args <= 12);
   HReg argVRegs[12];
   for (i = 0; i < 12; i++)
      argVRegs[i] = INVALID_HREG;

   /* Compute args into vregs. */
   for (i = 0; i < n_real_args; i++) {
      argVRegs[i] = iselIntExpr_R(env, args[i+1]);
   }

   /* Now we can compute the condition.  We can't do it earlier
      because the argument computations could trash the condition
      codes.  Be a bit clever to handle the common case where the
      guard is 1:Bit. */
   ARMCondCode cc = ARMcc_AL;
   if (guard) {
      if (guard->tag == Iex_Const
          && guard->Iex.Const.con->tag == Ico_U1
          && guard->Iex.Const.con->Ico.U1 == True) {
         /* unconditional -- do nothing */
      } else {
         goto no_match; //ATC
         cc = iselCondCode( env, guard );
      }
   }

   HReg r0 = hregARM_R0();
   HReg sp = hregARM_R13();

   ARMRI84* c256 = ARMRI84_I84(64, 15); // 64 `ror` (15 * 2)

   addInstr(env, ARMInstr_Alu(ARMalu_SUB, r0, sp, ARMRI84_I84(128, 0)));

   addInstr(env, mk_iMOVds_RR(hregARM_R1(), argVRegs[0]));
   addInstr(env, mk_iMOVds_RR(hregARM_R2(), argVRegs[1]));
   addInstr(env, mk_iMOVds_RR(hregARM_R3(), argVRegs[2]));

   addInstr(env, ARMInstr_Alu(ARMalu_SUB, sp, sp, c256));

   for (i = 3; i < n_real_args; i++) {
      addInstr(env, ARMInstr_LdSt32(ARMcc_AL, False/*store*/, argVRegs[i],
                                    ARMAMode1_RI(sp, (i-3) * 4)));
   }

   vassert(*stackAdjustAfterCall == 0);
   vassert(is_RetLoc_INVALID(*retloc));

   *stackAdjustAfterCall = 256;
   *retloc = mk_RetLoc_spRel(RLPri_V128SpRel, 128);

   Addr32 target = (Addr)cee->addr;
   addInstr(env, ARMInstr_Call( cc, target, 4, *retloc ));

   return True; /* success */

  no_match:
   return False;
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
   ARMCondCode cc;
   HReg        argregs[ARM_N_ARGREGS];
   HReg        tmpregs[ARM_N_ARGREGS];
   Bool        go_fast;
   Int         n_args, i, nextArgReg;
   Addr32      target;

   vassert(ARM_N_ARGREGS == 4);

   /* Set default returns.  We'll update them later if needed. */
   *stackAdjustAfterCall = 0;
   *retloc               = mk_RetLoc_INVALID();

   /* These are used for cross-checking that IR-level constraints on
      the use of IRExpr_VECRET() and IRExpr_GSPTR() are observed. */
   UInt nVECRETs = 0;
   UInt nGSPTRs  = 0;

   /* Marshal args for a call and do the call.

      This function only deals with a tiny set of possibilities, which
      cover all helpers in practice.  The restrictions are that only
      arguments in registers are supported, hence only ARM_N_REGPARMS
      x 32 integer bits in total can be passed.  In fact the only
      supported arg types are I32 and I64.

      The return type can be I{64,32} or V128.  In the V128 case, it
      is expected that |args| will contain the special node
      IRExpr_VECRET(), in which case this routine generates code to
      allocate space on the stack for the vector return value.  Since
      we are not passing any scalars on the stack, it is enough to
      preallocate the return space before marshalling any arguments,
      in this case.

      |args| may also contain IRExpr_GSPTR(), in which case the
      value in r8 is passed as the corresponding argument.

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

   /* Note that the cee->regparms field is meaningless on ARM hosts
      (since there is only one calling convention) and so we always
      ignore it. */

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

   /* If there are more than 4 args, we are going to have to pass
      some via memory.  Use a different function to (possibly) deal with
      that; dealing with it here is too complex. */
   if (n_args > ARM_N_ARGREGS) {
      return doHelperCallWithArgsOnStack(stackAdjustAfterCall, retloc,
                                         env, guard, cee, retTy, args );

   }

   /* After this point we make no attempt to pass args on the stack,
      and just give up if that case (which is OK because it never
      happens).  Even if there are for example only 3 args, it might
      still be necessary to pass some of them on the stack if for example
      two or more of them are 64-bit integers. */

   argregs[0] = hregARM_R0();
   argregs[1] = hregARM_R1();
   argregs[2] = hregARM_R2();
   argregs[3] = hregARM_R3();

   tmpregs[0] = tmpregs[1] = tmpregs[2] =
   tmpregs[3] = INVALID_HREG;

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
         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, arg);

         if (nextArgReg >= ARM_N_ARGREGS)
            return False; /* out of argregs */

         if (aTy == Ity_I32) {
            addInstr(env, mk_iMOVds_RR( argregs[nextArgReg],
                                        iselIntExpr_R(env, arg) ));
            nextArgReg++;
         }
         else if (aTy == Ity_I64) {
            /* 64-bit args must be passed in an a reg-pair of the form
               n:n+1, where n is even.  Hence either r0:r1 or r2:r3.
               On a little-endian host, the less significant word is
               passed in the lower-numbered register. */
            if (nextArgReg & 1) {
               if (nextArgReg >= ARM_N_ARGREGS)
                  return False; /* out of argregs */
               addInstr(env, ARMInstr_Imm32( argregs[nextArgReg], 0xAA ));
               nextArgReg++;
            }
            if (nextArgReg >= ARM_N_ARGREGS)
               return False; /* out of argregs */
            HReg raHi, raLo;
            iselInt64Expr(&raHi, &raLo, env, arg);
            addInstr(env, mk_iMOVds_RR( argregs[nextArgReg], raLo ));
            nextArgReg++;
            addInstr(env, mk_iMOVds_RR( argregs[nextArgReg], raHi ));
            nextArgReg++;
         }
         else if (arg->tag == Iex_GSPTR) {
            vassert(0); //ATC
            addInstr(env, mk_iMOVds_RR( argregs[nextArgReg],
                                        hregARM_R8() ));
            nextArgReg++;
         }
         else if (arg->tag == Iex_VECRET) {
            // If this happens, it denotes ill-formed IR
            vassert(0);
         }
         else
            return False; /* unhandled arg type */
      }

      /* Fast scheme only applies for unconditional calls.  Hence: */
      cc = ARMcc_AL;

   } else {

      /* SLOW SCHEME; move via temporaries */
      nextArgReg = 0;

      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];

         IRType  aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy  = typeOfIRExpr(env->type_env, arg);

         if (nextArgReg >= ARM_N_ARGREGS)
            return False; /* out of argregs */

         if (aTy == Ity_I32) {
            tmpregs[nextArgReg] = iselIntExpr_R(env, args[i]);
            nextArgReg++;
         }
         else if (aTy == Ity_I64) {
            /* Same comment applies as in the Fast-scheme case. */
            if (nextArgReg & 1)
               nextArgReg++;
            if (nextArgReg + 1 >= ARM_N_ARGREGS)
               return False; /* out of argregs */
            HReg raHi, raLo;
            iselInt64Expr(&raHi, &raLo, env, args[i]);
            tmpregs[nextArgReg] = raLo;
            nextArgReg++;
            tmpregs[nextArgReg] = raHi;
            nextArgReg++;
         }
         else if (arg->tag == Iex_GSPTR) {
            vassert(0); //ATC
            tmpregs[nextArgReg] = hregARM_R8();
            nextArgReg++;
         }
         else if (arg->tag == Iex_VECRET) {
            // If this happens, it denotes ill-formed IR
            vassert(0);
         }
         else
            return False; /* unhandled arg type */
      }

      /* Now we can compute the condition.  We can't do it earlier
         because the argument computations could trash the condition
         codes.  Be a bit clever to handle the common case where the
         guard is 1:Bit. */
      cc = ARMcc_AL;
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
         if (hregIsInvalid(tmpregs[i])) { // Skip invalid regs
            addInstr(env, ARMInstr_Imm32( argregs[i], 0xAA ));
            continue;
         }
         /* None of these insns, including any spill code that might
            be generated, may alter the condition codes. */
         addInstr( env, mk_iMOVds_RR( argregs[i], tmpregs[i] ) );
      }

   }

   /* Should be assured by checks above */
   vassert(nextArgReg <= ARM_N_ARGREGS);

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
         *retloc = mk_RetLoc_simple(RLPri_2Int);
         break;
      case Ity_I32: case Ity_I16: case Ity_I8:
         *retloc = mk_RetLoc_simple(RLPri_Int);
         break;
      case Ity_V128:
         vassert(0); // ATC
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
      assigned in the order r0, r1, r2, r3, its numeric value at this
      point, which must be between 0 and 4 inclusive, is going to be
      equal to the number of arg regs in use for the call.  Hence bake
      that number into the call (we'll need to know it when doing
      register allocation, to know what regs the call reads.)

      There is a bit of a twist -- harmless but worth recording.
      Suppose the arg types are (Ity_I32, Ity_I64).  Then we will have
      the first arg in r0 and the second in r3:r2, but r1 isn't used.
      We nevertheless have nextArgReg==4 and bake that into the call
      instruction.  This will mean the register allocator wil believe
      this insn reads r1 when in fact it doesn't.  But that's
      harmless; it just artificially extends the live range of r1
      unnecessarily.  The best fix would be to put into the
      instruction, a bitmask indicating which of r0/1/2/3 carry live
      values.  But that's too much hassle. */

   target = (Addr)cee->addr;
   addInstr(env, ARMInstr_Call( cc, target, nextArgReg, *retloc ));

   return True; /* success */
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (32/16/8 bit)           ---*/
/*---------------------------------------------------------*/

/* Select insns for an integer-typed expression, and add them to the
   code list.  Return a reg holding the result.  This reg will be a
   virtual register.  THE RETURNED REG MUST NOT BE MODIFIED.  If you
   want to modify it, ask for a new vreg, copy it in there, and modify
   the copy.  The register allocator will do its best to map both
   vregs to the same real register, so the copies will often disappear
   later in the game.

   This should handle expressions of 32, 16 and 8-bit type.  All
   results are returned in a 32-bit register.  For 16- and 8-bit
   expressions, the upper 16/24 bits are arbitrary, so you should mask
   or sign extend partial values if necessary.
*/

/* --------------------- AMode1 --------------------- */

/* Return an AMode1 which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a 32-bit one.
*/

static Bool sane_AMode1 ( ARMAMode1* am )
{
   switch (am->tag) {
      case ARMam1_RI:
         return
            toBool( hregClass(am->ARMam1.RI.reg) == HRcInt32
                    && (hregIsVirtual(am->ARMam1.RI.reg)
                        || sameHReg(am->ARMam1.RI.reg, hregARM_R8()))
                    && am->ARMam1.RI.simm13 >= -4095
                    && am->ARMam1.RI.simm13 <= 4095 );
      case ARMam1_RRS:
         return
            toBool( hregClass(am->ARMam1.RRS.base) == HRcInt32
                    && hregIsVirtual(am->ARMam1.RRS.base)
                    && hregClass(am->ARMam1.RRS.index) == HRcInt32
                    && hregIsVirtual(am->ARMam1.RRS.index)
                    && am->ARMam1.RRS.shift >= 0
                    && am->ARMam1.RRS.shift <= 3 );
      default:
         vpanic("sane_AMode: unknown ARM AMode1 tag");
   }
}

static ARMAMode1* iselIntExpr_AMode1 ( ISelEnv* env, IRExpr* e )
{
   ARMAMode1* am = iselIntExpr_AMode1_wrk(env, e);
   vassert(sane_AMode1(am));
   return am;
}

static ARMAMode1* iselIntExpr_AMode1_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32);

   /* FIXME: add RRS matching */

   /* {Add32,Sub32}(expr,simm13) */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_Add32 || e->Iex.Binop.op == Iop_Sub32)
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32) {
      Int simm = (Int)e->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
      if (simm >= -4095 && simm <= 4095) {
         HReg reg;
         if (e->Iex.Binop.op == Iop_Sub32)
            simm = -simm;
         reg = iselIntExpr_R(env, e->Iex.Binop.arg1);
         return ARMAMode1_RI(reg, simm);
      }
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg reg = iselIntExpr_R(env, e);
      return ARMAMode1_RI(reg, 0);
   }

}


/* --------------------- AMode2 --------------------- */

/* Return an AMode2 which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a 32-bit one.
*/

static Bool sane_AMode2 ( ARMAMode2* am )
{
   switch (am->tag) {
      case ARMam2_RI:
         return
            toBool( hregClass(am->ARMam2.RI.reg) == HRcInt32
                    && hregIsVirtual(am->ARMam2.RI.reg)
                    && am->ARMam2.RI.simm9 >= -255
                    && am->ARMam2.RI.simm9 <= 255 );
      case ARMam2_RR:
         return
            toBool( hregClass(am->ARMam2.RR.base) == HRcInt32
                    && hregIsVirtual(am->ARMam2.RR.base)
                    && hregClass(am->ARMam2.RR.index) == HRcInt32
                    && hregIsVirtual(am->ARMam2.RR.index) );
      default:
         vpanic("sane_AMode: unknown ARM AMode2 tag");
   }
}

static ARMAMode2* iselIntExpr_AMode2 ( ISelEnv* env, IRExpr* e )
{
   ARMAMode2* am = iselIntExpr_AMode2_wrk(env, e);
   vassert(sane_AMode2(am));
   return am;
}

static ARMAMode2* iselIntExpr_AMode2_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32);

   /* FIXME: add RR matching */

   /* {Add32,Sub32}(expr,simm8) */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_Add32 || e->Iex.Binop.op == Iop_Sub32)
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32) {
      Int simm = (Int)e->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
      if (simm >= -255 && simm <= 255) {
         HReg reg;
         if (e->Iex.Binop.op == Iop_Sub32)
            simm = -simm;
         reg = iselIntExpr_R(env, e->Iex.Binop.arg1);
         return ARMAMode2_RI(reg, simm);
      }
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg reg = iselIntExpr_R(env, e);
      return ARMAMode2_RI(reg, 0);
   }

}


/* --------------------- AModeV --------------------- */

/* Return an AModeV which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a 32-bit one.
*/

static Bool sane_AModeV ( ARMAModeV* am )
{
  return toBool( hregClass(am->reg) == HRcInt32
                 && hregIsVirtual(am->reg)
                 && am->simm11 >= -1020 && am->simm11 <= 1020
                 && 0 == (am->simm11 & 3) );
}

static ARMAModeV* iselIntExpr_AModeV ( ISelEnv* env, IRExpr* e )
{
   ARMAModeV* am = iselIntExpr_AModeV_wrk(env, e);
   vassert(sane_AModeV(am));
   return am;
}

static ARMAModeV* iselIntExpr_AModeV_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32);

   /* {Add32,Sub32}(expr, simm8 << 2) */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_Add32 || e->Iex.Binop.op == Iop_Sub32)
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32) {
      Int simm = (Int)e->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
      if (simm >= -1020 && simm <= 1020 && 0 == (simm & 3)) {
         HReg reg;
         if (e->Iex.Binop.op == Iop_Sub32)
            simm = -simm;
         reg = iselIntExpr_R(env, e->Iex.Binop.arg1);
         return mkARMAModeV(reg, simm);
      }
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg reg = iselIntExpr_R(env, e);
      return mkARMAModeV(reg, 0);
   }

}

/* -------------------- AModeN -------------------- */

static ARMAModeN* iselIntExpr_AModeN ( ISelEnv* env, IRExpr* e )
{
   return iselIntExpr_AModeN_wrk(env, e);
}

static ARMAModeN* iselIntExpr_AModeN_wrk ( ISelEnv* env, IRExpr* e )
{
   HReg reg = iselIntExpr_R(env, e);
   return mkARMAModeN_R(reg);
}


/* --------------------- RI84 --------------------- */

/* Select instructions to generate 'e' into a RI84.  If mayInv is
   true, then the caller will also accept an I84 form that denotes
   'not e'.  In this case didInv may not be NULL, and *didInv is set
   to True.  This complication is so as to allow generation of an RI84
   which is suitable for use in either an AND or BIC instruction,
   without knowing (before this call) which one.
*/
static ARMRI84* iselIntExpr_RI84 ( /*OUT*/Bool* didInv, Bool mayInv,
                                   ISelEnv* env, IRExpr* e )
{
   ARMRI84* ri;
   if (mayInv)
      vassert(didInv != NULL);
   ri = iselIntExpr_RI84_wrk(didInv, mayInv, env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case ARMri84_I84:
         return ri;
      case ARMri84_R:
         vassert(hregClass(ri->ARMri84.R.reg) == HRcInt32);
         vassert(hregIsVirtual(ri->ARMri84.R.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RI84: unknown arm RI84 tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARMRI84* iselIntExpr_RI84_wrk ( /*OUT*/Bool* didInv, Bool mayInv,
                                       ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   if (didInv) *didInv = False;

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      UInt u, u8 = 0x100, u4 = 0x10; /* both invalid */
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & (e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:  u = 0xFF   & (e->Iex.Const.con->Ico.U8); break;
         default: vpanic("iselIntExpr_RI84.Iex_Const(armh)");
      }
      if (fitsIn8x4(&u8, &u4, u)) {
         return ARMRI84_I84( (UShort)u8, (UShort)u4 );
      }
      if (mayInv && fitsIn8x4(&u8, &u4, ~u)) {
         vassert(didInv);
         *didInv = True;
         return ARMRI84_I84( (UShort)u8, (UShort)u4 );
      }
      /* else fail, fall through to default case */
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return ARMRI84_R(r);
   }
}


/* --------------------- RI5 --------------------- */

/* Select instructions to generate 'e' into a RI5. */

static ARMRI5* iselIntExpr_RI5 ( ISelEnv* env, IRExpr* e )
{
   ARMRI5* ri = iselIntExpr_RI5_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case ARMri5_I5:
         return ri;
      case ARMri5_R:
         vassert(hregClass(ri->ARMri5.R.reg) == HRcInt32);
         vassert(hregIsVirtual(ri->ARMri5.R.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RI5: unknown arm RI5 tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARMRI5* iselIntExpr_RI5_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      UInt u; /* both invalid */
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & (e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:  u = 0xFF   & (e->Iex.Const.con->Ico.U8); break;
         default: vpanic("iselIntExpr_RI5.Iex_Const(armh)");
      }
      if (u >= 1 && u <= 31) {
         return ARMRI5_I5(u);
      }
      /* else fail, fall through to default case */
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return ARMRI5_R(r);
   }
}


/* ------------------- CondCode ------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static ARMCondCode iselCondCode ( ISelEnv* env, IRExpr* e )
{
   ARMCondCode cc = iselCondCode_wrk(env,e);
   vassert(cc != ARMcc_NV);
   return cc;
}

static ARMCondCode iselCondCode_wrk ( ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I1);

   /* var */
   if (e->tag == Iex_RdTmp) {
      HReg rTmp = lookupIRTemp(env, e->Iex.RdTmp.tmp);
      /* CmpOrTst doesn't modify rTmp; so this is OK. */
      ARMRI84* one  = ARMRI84_I84(1,0);
      addInstr(env, ARMInstr_CmpOrTst(False/*test*/, rTmp, one));
      return ARMcc_NE;
   }

   /* Not1(e) */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_Not1) {
      /* Generate code for the arg, and negate the test condition */
      return 1 ^ iselCondCode(env, e->Iex.Unop.arg);
   }

   /* --- patterns rooted at: 32to1 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_32to1) {
      HReg     rTmp = iselIntExpr_R(env, e->Iex.Unop.arg);
      ARMRI84* one  = ARMRI84_I84(1,0);
      addInstr(env, ARMInstr_CmpOrTst(False/*test*/, rTmp, one));
      return ARMcc_NE;
   }

   /* --- patterns rooted at: CmpNEZ8 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ8) {
      HReg     r1   = iselIntExpr_R(env, e->Iex.Unop.arg);
      ARMRI84* xFF  = ARMRI84_I84(0xFF,0);
      addInstr(env, ARMInstr_CmpOrTst(False/*!isCmp*/, r1, xFF));
      return ARMcc_NE;
   }

   /* --- patterns rooted at: CmpNEZ32 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ32) {
      HReg     r1   = iselIntExpr_R(env, e->Iex.Unop.arg);
      ARMRI84* zero = ARMRI84_I84(0,0);
      addInstr(env, ARMInstr_CmpOrTst(True/*isCmp*/, r1, zero));
      return ARMcc_NE;
   }

   /* --- patterns rooted at: CmpNEZ64 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ64) {
      HReg     tHi, tLo;
      HReg     tmp  = newVRegI(env);
      ARMRI84* zero = ARMRI84_I84(0,0);
      iselInt64Expr(&tHi, &tLo, env, e->Iex.Unop.arg);
      addInstr(env, ARMInstr_Alu(ARMalu_OR, tmp, tHi, ARMRI84_R(tLo)));
      addInstr(env, ARMInstr_CmpOrTst(True/*isCmp*/, tmp, zero));
      return ARMcc_NE;
   }

   /* --- Cmp*32*(x,y) --- */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_CmpEQ32
           || e->Iex.Binop.op == Iop_CmpNE32
           || e->Iex.Binop.op == Iop_CmpLT32S
           || e->Iex.Binop.op == Iop_CmpLT32U
           || e->Iex.Binop.op == Iop_CmpLE32S
           || e->Iex.Binop.op == Iop_CmpLE32U)) {
      HReg     argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
      ARMRI84* argR = iselIntExpr_RI84(NULL,False, 
                                       env, e->Iex.Binop.arg2);
      addInstr(env, ARMInstr_CmpOrTst(True/*isCmp*/, argL, argR));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ32:  return ARMcc_EQ;
         case Iop_CmpNE32:  return ARMcc_NE;
         case Iop_CmpLT32S: return ARMcc_LT;
         case Iop_CmpLT32U: return ARMcc_LO;
         case Iop_CmpLE32S: return ARMcc_LE;
         case Iop_CmpLE32U: return ARMcc_LS;
         default: vpanic("iselCondCode(arm): CmpXX32");
      }
   }

   /* const */
   /* Constant 1:Bit */
   if (e->tag == Iex_Const) {
      HReg r;
      vassert(e->Iex.Const.con->tag == Ico_U1);
      vassert(e->Iex.Const.con->Ico.U1 == True 
              || e->Iex.Const.con->Ico.U1 == False);
      r = newVRegI(env);
      addInstr(env, ARMInstr_Imm32(r, 0));
      addInstr(env, ARMInstr_CmpOrTst(True/*isCmp*/, r, ARMRI84_R(r)));
      return e->Iex.Const.con->Ico.U1 ? ARMcc_EQ : ARMcc_NE;
   }

   /* --- And1(x,y), Or1(x,y) --- */
   /* FIXME: We could (and probably should) do a lot better here, by using the
      iselCondCode_C/_R scheme used in the amd64 insn selector. */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_And1 || e->Iex.Binop.op == Iop_Or1)) {
      HReg x_as_32 = newVRegI(env);
      ARMCondCode cc_x = iselCondCode(env, e->Iex.Binop.arg1);
      addInstr(env, ARMInstr_Mov(x_as_32, ARMRI84_I84(0,0)));
      addInstr(env, ARMInstr_CMov(cc_x, x_as_32, ARMRI84_I84(1,0)));

      HReg y_as_32 = newVRegI(env);
      ARMCondCode cc_y = iselCondCode(env, e->Iex.Binop.arg2);
      addInstr(env, ARMInstr_Mov(y_as_32, ARMRI84_I84(0,0)));
      addInstr(env, ARMInstr_CMov(cc_y, y_as_32, ARMRI84_I84(1,0)));

      HReg tmp = newVRegI(env);
      ARMAluOp aop = e->Iex.Binop.op == Iop_And1 ? ARMalu_AND : ARMalu_OR;
      addInstr(env, ARMInstr_Alu(aop, tmp, x_as_32, ARMRI84_R(y_as_32)));

      ARMRI84* one  = ARMRI84_I84(1,0);
      addInstr(env, ARMInstr_CmpOrTst(False/*test*/, tmp, one));
      return ARMcc_NE;
   }

   // JRS 2013-Jan-03: this seems completely nonsensical
   /* --- CasCmpEQ* --- */
   /* Ist_Cas has a dummy argument to compare with, so comparison is
      always true. */
   //if (e->tag == Iex_Binop
   //    && (e->Iex.Binop.op == Iop_CasCmpEQ32
   //        || e->Iex.Binop.op == Iop_CasCmpEQ16
   //        || e->Iex.Binop.op == Iop_CasCmpEQ8)) {
   //   return ARMcc_AL;
   //}

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
   vassert(hregClass(r) == HRcInt32);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselIntExpr_R_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

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

      if (ty == Ity_I32) {
         ARMAMode1* amode = iselIntExpr_AMode1 ( env, e->Iex.Load.addr );
         addInstr(env, ARMInstr_LdSt32(ARMcc_AL, True/*isLoad*/, dst, amode));
         return dst;
      }
      if (ty == Ity_I16) {
         ARMAMode2* amode = iselIntExpr_AMode2 ( env, e->Iex.Load.addr );
         addInstr(env, ARMInstr_LdSt16(ARMcc_AL,
                                       True/*isLoad*/, False/*!signedLoad*/,
                                       dst, amode));
         return dst;
      }
      if (ty == Ity_I8) {
         ARMAMode1* amode = iselIntExpr_AMode1 ( env, e->Iex.Load.addr );
         addInstr(env, ARMInstr_LdSt8U(ARMcc_AL, True/*isLoad*/, dst, amode));
         return dst;
      }
      break;
   }

//zz   /* --------- TERNARY OP --------- */
//zz   case Iex_Triop: {
//zz      IRTriop *triop = e->Iex.Triop.details;
//zz      /* C3210 flags following FPU partial remainder (fprem), both
//zz         IEEE compliant (PREM1) and non-IEEE compliant (PREM). */
//zz      if (triop->op == Iop_PRemC3210F64
//zz          || triop->op == Iop_PRem1C3210F64) {
//zz         HReg junk = newVRegF(env);
//zz         HReg dst  = newVRegI(env);
//zz         HReg srcL = iselDblExpr(env, triop->arg2);
//zz         HReg srcR = iselDblExpr(env, triop->arg3);
//zz         /* XXXROUNDINGFIXME */
//zz         /* set roundingmode here */
//zz         addInstr(env, X86Instr_FpBinary(
//zz                           e->Iex.Binop.op==Iop_PRemC3210F64 
//zz                              ? Xfp_PREM : Xfp_PREM1,
//zz                           srcL,srcR,junk
//zz                 ));
//zz         /* The previous pseudo-insn will have left the FPU's C3210
//zz            flags set correctly.  So bag them. */
//zz         addInstr(env, X86Instr_FpStSW_AX());
//zz         addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), dst));
//zz         addInstr(env, X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(0x4700), dst));
//zz         return dst;
//zz      }
//zz
//zz      break;
//zz   }

   /* --------- BINARY OP --------- */
   case Iex_Binop: {

      ARMAluOp   aop = 0; /* invalid */
      ARMShiftOp sop = 0; /* invalid */

      /* ADD/SUB/AND/OR/XOR */
      switch (e->Iex.Binop.op) {
         case Iop_And32: {
            Bool     didInv = False;
            HReg     dst    = newVRegI(env);
            HReg     argL   = iselIntExpr_R(env, e->Iex.Binop.arg1);
            ARMRI84* argR   = iselIntExpr_RI84(&didInv, True/*mayInv*/,
                                               env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_Alu(didInv ? ARMalu_BIC : ARMalu_AND,
                                       dst, argL, argR));
            return dst;
         }
         case Iop_Or32:  aop = ARMalu_OR;  goto std_binop;
         case Iop_Xor32: aop = ARMalu_XOR; goto std_binop;
         case Iop_Sub32: aop = ARMalu_SUB; goto std_binop;
         case Iop_Add32: aop = ARMalu_ADD; goto std_binop;
         std_binop: {
            HReg     dst  = newVRegI(env);
            HReg     argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
            ARMRI84* argR = iselIntExpr_RI84(NULL, False/*mayInv*/,
                                             env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_Alu(aop, dst, argL, argR));
            return dst;
         }
         default: break;
      }

      /* SHL/SHR/SAR */
      switch (e->Iex.Binop.op) {
         case Iop_Shl32: sop = ARMsh_SHL; goto sh_binop;
         case Iop_Shr32: sop = ARMsh_SHR; goto sh_binop;
         case Iop_Sar32: sop = ARMsh_SAR; goto sh_binop;
         sh_binop: {
            HReg    dst  = newVRegI(env);
            HReg    argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
            ARMRI5* argR = iselIntExpr_RI5(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_Shift(sop, dst, argL, argR));
            vassert(ty == Ity_I32); /* else the IR is ill-typed */
            return dst;
         }
         default: break;
      }

      /* MUL */
      if (e->Iex.Binop.op == Iop_Mul32) {
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(hregARM_R2(), argL));
         addInstr(env, mk_iMOVds_RR(hregARM_R3(), argR));
         addInstr(env, ARMInstr_Mul(ARMmul_PLAIN));
         addInstr(env, mk_iMOVds_RR(dst, hregARM_R0()));
         return dst;
      }

      /* Handle misc other ops. */

      if (e->Iex.Binop.op == Iop_Max32U) {
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegI(env);
         addInstr(env, ARMInstr_CmpOrTst(True/*isCmp*/, argL,
                                         ARMRI84_R(argR)));
         addInstr(env, mk_iMOVds_RR(dst, argL));
         addInstr(env, ARMInstr_CMov(ARMcc_LO, dst, ARMRI84_R(argR)));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_CmpF64) {
         HReg dL = iselDblExpr(env, e->Iex.Binop.arg1);
         HReg dR = iselDblExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegI(env);
         /* Do the compare (FCMPD) and set NZCV in FPSCR.  Then also do
            FMSTAT, so we can examine the results directly. */
         addInstr(env, ARMInstr_VCmpD(dL, dR));
         /* Create in dst, the IRCmpF64Result encoded result. */
         addInstr(env, ARMInstr_Imm32(dst, 0));
         addInstr(env, ARMInstr_CMov(ARMcc_EQ, dst, ARMRI84_I84(0x40,0))); //EQ
         addInstr(env, ARMInstr_CMov(ARMcc_MI, dst, ARMRI84_I84(0x01,0))); //LT
         addInstr(env, ARMInstr_CMov(ARMcc_GT, dst, ARMRI84_I84(0x00,0))); //GT
         addInstr(env, ARMInstr_CMov(ARMcc_VS, dst, ARMRI84_I84(0x45,0))); //UN
         return dst;
      }

      if (e->Iex.Binop.op == Iop_F64toI32S
          || e->Iex.Binop.op == Iop_F64toI32U) {
         /* Wretched uglyness all round, due to having to deal
            with rounding modes.  Oh well. */
         /* FIXME: if arg1 is a constant indicating round-to-zero,
            then we could skip all this arsing around with FPSCR and
            simply emit FTO{S,U}IZD. */
         Bool syned = e->Iex.Binop.op == Iop_F64toI32S;
         HReg valD  = iselDblExpr(env, e->Iex.Binop.arg2);
         set_VFP_rounding_mode(env, e->Iex.Binop.arg1);
         /* FTO{S,U}ID valF, valD */
         HReg valF = newVRegF(env);
         addInstr(env, ARMInstr_VCvtID(False/*!iToD*/, syned,
                                       valF, valD));
         set_VFP_rounding_default(env);
         /* VMOV dst, valF */
         HReg dst = newVRegI(env);
         addInstr(env, ARMInstr_VXferS(False/*!toS*/, valF, dst));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_GetElem8x8
          || e->Iex.Binop.op == Iop_GetElem16x4
          || e->Iex.Binop.op == Iop_GetElem32x2) {
         if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
            HReg res = newVRegI(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Binop.arg1);
            UInt index, size;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
               vpanic("ARM target supports GetElem with constant "
                      "second argument only (neon)\n");
            }
            index = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch (e->Iex.Binop.op) {
               case Iop_GetElem8x8: vassert(index < 8); size = 0; break;
               case Iop_GetElem16x4: vassert(index < 4); size = 1; break;
               case Iop_GetElem32x2: vassert(index < 2); size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnaryS(ARMneon_GETELEMS,
                                           mkARMNRS(ARMNRS_Reg, res, 0),
                                           mkARMNRS(ARMNRS_Scalar, arg, index),
                                           size, False));
            return res;
         }
      }

      if (e->Iex.Binop.op == Iop_GetElem32x2
          && e->Iex.Binop.arg2->tag == Iex_Const
          && !(env->hwcaps & VEX_HWCAPS_ARM_NEON)) {
         /* We may have to do GetElem32x2 on a non-NEON capable
            target. */
         IRConst* con = e->Iex.Binop.arg2->Iex.Const.con;
         vassert(con->tag == Ico_U8); /* else IR is ill-typed */
         UInt index = con->Ico.U8;
         if (index >= 0 && index <= 1) {
            HReg rHi, rLo;
            iselInt64Expr(&rHi, &rLo, env, e->Iex.Binop.arg1);
            return index == 0 ? rLo : rHi;
         }
      }

      if (e->Iex.Binop.op == Iop_GetElem8x16
          || e->Iex.Binop.op == Iop_GetElem16x8
          || e->Iex.Binop.op == Iop_GetElem32x4) {
         if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
            HReg res = newVRegI(env);
            HReg arg = iselNeonExpr(env, e->Iex.Binop.arg1);
            UInt index, size;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
               vpanic("ARM target supports GetElem with constant "
                      "second argument only (neon)\n");
            }
            index = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch (e->Iex.Binop.op) {
               case Iop_GetElem8x16: vassert(index < 16); size = 0; break;
               case Iop_GetElem16x8: vassert(index < 8); size = 1; break;
               case Iop_GetElem32x4: vassert(index < 4); size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnaryS(ARMneon_GETELEMS,
                                           mkARMNRS(ARMNRS_Reg, res, 0),
                                           mkARMNRS(ARMNRS_Scalar, arg, index),
                                           size, True));
            return res;
         }
      }

      /* All cases involving host-side helper calls. */
      void* fn = NULL;
      switch (e->Iex.Binop.op) {
         case Iop_Add16x2:
            fn = &h_generic_calc_Add16x2; break;
         case Iop_Sub16x2:
            fn = &h_generic_calc_Sub16x2; break;
         case Iop_HAdd16Ux2:
            fn = &h_generic_calc_HAdd16Ux2; break;
         case Iop_HAdd16Sx2:
            fn = &h_generic_calc_HAdd16Sx2; break;
         case Iop_HSub16Ux2:
            fn = &h_generic_calc_HSub16Ux2; break;
         case Iop_HSub16Sx2:
            fn = &h_generic_calc_HSub16Sx2; break;
         case Iop_QAdd16Sx2:
            fn = &h_generic_calc_QAdd16Sx2; break;
         case Iop_QAdd16Ux2:
            fn = &h_generic_calc_QAdd16Ux2; break;
         case Iop_QSub16Sx2:
            fn = &h_generic_calc_QSub16Sx2; break;
         case Iop_Add8x4:
            fn = &h_generic_calc_Add8x4; break;
         case Iop_Sub8x4:
            fn = &h_generic_calc_Sub8x4; break;
         case Iop_HAdd8Ux4:
            fn = &h_generic_calc_HAdd8Ux4; break;
         case Iop_HAdd8Sx4:
            fn = &h_generic_calc_HAdd8Sx4; break;
         case Iop_HSub8Ux4:
            fn = &h_generic_calc_HSub8Ux4; break;
         case Iop_HSub8Sx4:
            fn = &h_generic_calc_HSub8Sx4; break;
         case Iop_QAdd8Sx4:
            fn = &h_generic_calc_QAdd8Sx4; break;
         case Iop_QAdd8Ux4:
            fn = &h_generic_calc_QAdd8Ux4; break;
         case Iop_QSub8Sx4:
            fn = &h_generic_calc_QSub8Sx4; break;
         case Iop_QSub8Ux4:
            fn = &h_generic_calc_QSub8Ux4; break;
         case Iop_Sad8Ux4:
            fn = &h_generic_calc_Sad8Ux4; break;
         case Iop_QAdd32S:
            fn = &h_generic_calc_QAdd32S; break;
         case Iop_QSub32S:
            fn = &h_generic_calc_QSub32S; break;
         case Iop_QSub16Ux2:
            fn = &h_generic_calc_QSub16Ux2; break;
         case Iop_DivU32:
            fn = &h_calc_udiv32_w_arm_semantics; break;
         case Iop_DivS32:
            fn = &h_calc_sdiv32_w_arm_semantics; break;
         default:
            break;
      }

      if (fn) {
         HReg regL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg regR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg res  = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(hregARM_R0(), regL));
         addInstr(env, mk_iMOVds_RR(hregARM_R1(), regR));
         addInstr(env, ARMInstr_Call( ARMcc_AL, (Addr)fn,
                                      2, mk_RetLoc_simple(RLPri_Int) ));
         addInstr(env, mk_iMOVds_RR(res, hregARM_R0()));
         return res;
      }

      break;
   }

   /* --------- UNARY OP --------- */
   case Iex_Unop: {

//zz      /* 1Uto8(32to1(expr32)) */
//zz      if (e->Iex.Unop.op == Iop_1Uto8) { 
//zz         DECLARE_PATTERN(p_32to1_then_1Uto8);
//zz         DEFINE_PATTERN(p_32to1_then_1Uto8,
//zz                        unop(Iop_1Uto8,unop(Iop_32to1,bind(0))));
//zz         if (matchIRExpr(&mi,p_32to1_then_1Uto8,e)) {
//zz            const IRExpr* expr32 = mi.bindee[0];
//zz            HReg dst = newVRegI(env);
//zz            HReg src = iselIntExpr_R(env, expr32);
//zz            addInstr(env, mk_iMOVsd_RR(src,dst) );
//zz            addInstr(env, X86Instr_Alu32R(Xalu_AND,
//zz                                          X86RMI_Imm(1), dst));
//zz            return dst;
//zz         }
//zz      }
//zz
//zz      /* 8Uto32(LDle(expr32)) */
//zz      if (e->Iex.Unop.op == Iop_8Uto32) {
//zz         DECLARE_PATTERN(p_LDle8_then_8Uto32);
//zz         DEFINE_PATTERN(p_LDle8_then_8Uto32,
//zz                        unop(Iop_8Uto32,
//zz                             IRExpr_Load(Iend_LE,Ity_I8,bind(0))) );
//zz         if (matchIRExpr(&mi,p_LDle8_then_8Uto32,e)) {
//zz            HReg dst = newVRegI(env);
//zz            X86AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
//zz            addInstr(env, X86Instr_LoadEX(1,False,amode,dst));
//zz            return dst;
//zz         }
//zz      }
//zz
//zz      /* 8Sto32(LDle(expr32)) */
//zz      if (e->Iex.Unop.op == Iop_8Sto32) {
//zz         DECLARE_PATTERN(p_LDle8_then_8Sto32);
//zz         DEFINE_PATTERN(p_LDle8_then_8Sto32,
//zz                        unop(Iop_8Sto32,
//zz                             IRExpr_Load(Iend_LE,Ity_I8,bind(0))) );
//zz         if (matchIRExpr(&mi,p_LDle8_then_8Sto32,e)) {
//zz            HReg dst = newVRegI(env);
//zz            X86AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
//zz            addInstr(env, X86Instr_LoadEX(1,True,amode,dst));
//zz            return dst;
//zz         }
//zz      }
//zz
//zz      /* 16Uto32(LDle(expr32)) */
//zz      if (e->Iex.Unop.op == Iop_16Uto32) {
//zz         DECLARE_PATTERN(p_LDle16_then_16Uto32);
//zz         DEFINE_PATTERN(p_LDle16_then_16Uto32,
//zz                        unop(Iop_16Uto32,
//zz                             IRExpr_Load(Iend_LE,Ity_I16,bind(0))) );
//zz         if (matchIRExpr(&mi,p_LDle16_then_16Uto32,e)) {
//zz            HReg dst = newVRegI(env);
//zz            X86AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
//zz            addInstr(env, X86Instr_LoadEX(2,False,amode,dst));
//zz            return dst;
//zz         }
//zz      }
//zz
//zz      /* 8Uto32(GET:I8) */
//zz      if (e->Iex.Unop.op == Iop_8Uto32) {
//zz         if (e->Iex.Unop.arg->tag == Iex_Get) {
//zz            HReg      dst;
//zz            X86AMode* amode;
//zz            vassert(e->Iex.Unop.arg->Iex.Get.ty == Ity_I8);
//zz            dst = newVRegI(env);
//zz            amode = X86AMode_IR(e->Iex.Unop.arg->Iex.Get.offset,
//zz                                hregX86_EBP());
//zz            addInstr(env, X86Instr_LoadEX(1,False,amode,dst));
//zz            return dst;
//zz         }
//zz      }
//zz
//zz      /* 16to32(GET:I16) */
//zz      if (e->Iex.Unop.op == Iop_16Uto32) {
//zz         if (e->Iex.Unop.arg->tag == Iex_Get) {
//zz            HReg      dst;
//zz            X86AMode* amode;
//zz            vassert(e->Iex.Unop.arg->Iex.Get.ty == Ity_I16);
//zz            dst = newVRegI(env);
//zz            amode = X86AMode_IR(e->Iex.Unop.arg->Iex.Get.offset,
//zz                                hregX86_EBP());
//zz            addInstr(env, X86Instr_LoadEX(2,False,amode,dst));
//zz            return dst;
//zz         }
//zz      }

      switch (e->Iex.Unop.op) {
         case Iop_8Uto32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_Alu(ARMalu_AND,
                                       dst, src, ARMRI84_I84(0xFF,0)));
            return dst;
         }
//zz         case Iop_8Uto16:
//zz         case Iop_8Uto32:
//zz         case Iop_16Uto32: {
//zz            HReg dst = newVRegI(env);
//zz            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
//zz            UInt mask = e->Iex.Unop.op==Iop_16Uto32 ? 0xFFFF : 0xFF;
//zz            addInstr(env, mk_iMOVsd_RR(src,dst) );
//zz            addInstr(env, X86Instr_Alu32R(Xalu_AND,
//zz                                          X86RMI_Imm(mask), dst));
//zz            return dst;
//zz         }
//zz         case Iop_8Sto16:
//zz         case Iop_8Sto32:
         case Iop_16Uto32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            ARMRI5* amt = ARMRI5_I5(16);
            addInstr(env, ARMInstr_Shift(ARMsh_SHL, dst, src, amt));
            addInstr(env, ARMInstr_Shift(ARMsh_SHR, dst, dst, amt));
            return dst;
         }
         case Iop_8Sto32:
         case Iop_16Sto32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            ARMRI5* amt = ARMRI5_I5(e->Iex.Unop.op==Iop_16Sto32 ? 16 : 24);
            addInstr(env, ARMInstr_Shift(ARMsh_SHL, dst, src, amt));
            addInstr(env, ARMInstr_Shift(ARMsh_SAR, dst, dst, amt));
            return dst;
         }
//zz         case Iop_Not8:
//zz         case Iop_Not16:
         case Iop_Not32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_Unary(ARMun_NOT, dst, src));
            return dst;
         }
         case Iop_64HIto32: {
            HReg rHi, rLo;
            iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rHi; /* and abandon rLo .. poor wee thing :-) */
         }
         case Iop_64to32: {
            HReg rHi, rLo;
            iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rLo; /* similar stupid comment to the above ... */
         }
         case Iop_64to8: {
            HReg rHi, rLo;
            if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
               HReg tHi = newVRegI(env);
               HReg tLo = newVRegI(env);
               HReg tmp = iselNeon64Expr(env, e->Iex.Unop.arg);
               addInstr(env, ARMInstr_VXferD(False, tmp, tHi, tLo));
               rHi = tHi;
               rLo = tLo;
            } else {
               iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            }
            return rLo;
         }

         case Iop_1Uto32:
            /* 1Uto32(tmp).  Since I1 values generated into registers
               are guaranteed to have value either only zero or one,
               we can simply return the value of the register in this
               case. */
            if (e->Iex.Unop.arg->tag == Iex_RdTmp) {
               HReg dst = lookupIRTemp(env, e->Iex.Unop.arg->Iex.RdTmp.tmp);
               return dst;
            }
            /* else fall through */
         case Iop_1Uto8: {
            HReg        dst  = newVRegI(env);
            ARMCondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_Mov(dst, ARMRI84_I84(0,0)));
            addInstr(env, ARMInstr_CMov(cond, dst, ARMRI84_I84(1,0)));
            return dst;
         }

         case Iop_1Sto32: {
            HReg        dst  = newVRegI(env);
            ARMCondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            ARMRI5*     amt  = ARMRI5_I5(31);
            /* This is really rough.  We could do much better here;
               perhaps mvn{cond} dst, #0 as the second insn?
               (same applies to 1Sto64) */
            addInstr(env, ARMInstr_Mov(dst, ARMRI84_I84(0,0)));
            addInstr(env, ARMInstr_CMov(cond, dst, ARMRI84_I84(1,0)));
            addInstr(env, ARMInstr_Shift(ARMsh_SHL, dst, dst, amt));
            addInstr(env, ARMInstr_Shift(ARMsh_SAR, dst, dst, amt));
            return dst;
         }


//zz         case Iop_1Sto8:
//zz         case Iop_1Sto16:
//zz         case Iop_1Sto32: {
//zz            /* could do better than this, but for now ... */
//zz            HReg dst         = newVRegI(env);
//zz            X86CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
//zz            addInstr(env, X86Instr_Set32(cond,dst));
//zz            addInstr(env, X86Instr_Sh32(Xsh_SHL, 31, dst));
//zz            addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, dst));
//zz            return dst;
//zz         }
//zz         case Iop_Ctz32: {
//zz            /* Count trailing zeroes, implemented by x86 'bsfl' */
//zz            HReg dst = newVRegI(env);
//zz            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
//zz            addInstr(env, X86Instr_Bsfr32(True,src,dst));
//zz            return dst;
//zz         }
         case Iop_Clz32: {
            /* Count leading zeroes; easy on ARM. */
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_Unary(ARMun_CLZ, dst, src));
            return dst;
         }

         case Iop_CmpwNEZ32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_Unary(ARMun_NEG, dst, src));
            addInstr(env, ARMInstr_Alu(ARMalu_OR, dst, dst, ARMRI84_R(src)));
            addInstr(env, ARMInstr_Shift(ARMsh_SAR, dst, dst, ARMRI5_I5(31)));
            return dst;
         }

         case Iop_Left32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_Unary(ARMun_NEG, dst, src));
            addInstr(env, ARMInstr_Alu(ARMalu_OR, dst, dst, ARMRI84_R(src)));
            return dst;
         }

//zz         case Iop_V128to32: {
//zz            HReg      dst  = newVRegI(env);
//zz            HReg      vec  = iselVecExpr(env, e->Iex.Unop.arg);
//zz            X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
//zz            sub_from_esp(env, 16);
//zz            addInstr(env, X86Instr_SseLdSt(False/*store*/, vec, esp0));
//zz            addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(esp0), dst ));
//zz            add_to_esp(env, 16);
//zz            return dst;
//zz         }
//zz
         case Iop_ReinterpF32asI32: {
            HReg dst = newVRegI(env);
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_VXferS(False/*!toS*/, src, dst));
            return dst;
         }

//zz
//zz         case Iop_16to8:
         case Iop_32to8:
         case Iop_32to16:
            /* These are no-ops. */
            return iselIntExpr_R(env, e->Iex.Unop.arg);

         default:
            break;
      }

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

      if (fn) {
         HReg arg = iselIntExpr_R(env, e->Iex.Unop.arg);
         HReg res = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(hregARM_R0(), arg));
         addInstr(env, ARMInstr_Call( ARMcc_AL, (Addr)fn,
                                      1, mk_RetLoc_simple(RLPri_Int) ));
         addInstr(env, mk_iMOVds_RR(res, hregARM_R0()));
         return res;
      }

      break;
   }

   /* --------- GET --------- */
   case Iex_Get: {
      if (ty == Ity_I32 
          && 0 == (e->Iex.Get.offset & 3)
          && e->Iex.Get.offset < 4096-4) {
         HReg dst = newVRegI(env);
         addInstr(env, ARMInstr_LdSt32(
                          ARMcc_AL, True/*isLoad*/,
                          dst,
                          ARMAMode1_RI(hregARM_R8(), e->Iex.Get.offset)));
         return dst;
      }
//zz      if (ty == Ity_I8 || ty == Ity_I16) {
//zz         HReg dst = newVRegI(env);
//zz         addInstr(env, X86Instr_LoadEX(
//zz                          toUChar(ty==Ity_I8 ? 1 : 2),
//zz                          False,
//zz                          X86AMode_IR(e->Iex.Get.offset,hregX86_EBP()),
//zz                          dst));
//zz         return dst;
//zz      }
      break;
   }

//zz   case Iex_GetI: {
//zz      X86AMode* am 
//zz         = genGuestArrayOffset(
//zz              env, e->Iex.GetI.descr, 
//zz                   e->Iex.GetI.ix, e->Iex.GetI.bias );
//zz      HReg dst = newVRegI(env);
//zz      if (ty == Ity_I8) {
//zz         addInstr(env, X86Instr_LoadEX( 1, False, am, dst ));
//zz         return dst;
//zz      }
//zz      if (ty == Ity_I32) {
//zz         addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(am), dst));
//zz         return dst;
//zz      }
//zz      break;
//zz   }

   /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg    dst = newVRegI(env);
      vassert(ty == e->Iex.CCall.retty);

      /* be very restrictive for now.  Only 32/64-bit ints allowed for
         args, and 32 bits for return type.  Don't forget to change
         the RetLoc if more types are allowed in future. */
      if (e->Iex.CCall.retty != Ity_I32)
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
         addInstr(env, mk_iMOVds_RR(dst, hregARM_R0()));
         return dst;
      }
      goto irreducible;
   }

   /* --------- LITERAL --------- */
   /* 32 literals */
   case Iex_Const: {
      UInt u   = 0;
      HReg dst = newVRegI(env);
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & (e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:  u = 0xFF   & (e->Iex.Const.con->Ico.U8); break;
         default: ppIRExpr(e); vpanic("iselIntExpr_R.Iex_Const(arm)");
      }
      addInstr(env, ARMInstr_Imm32(dst, u));
      return dst;
   }

   /* --------- MULTIPLEX --------- */
   case Iex_ITE: { // VFD
      /* ITE(ccexpr, iftrue, iffalse) */
      if (ty == Ity_I32) {
         ARMCondCode cc;
         HReg     r1  = iselIntExpr_R(env, e->Iex.ITE.iftrue);
         ARMRI84* r0  = iselIntExpr_RI84(NULL, False, env, e->Iex.ITE.iffalse);
         HReg     dst = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(dst, r1));
         cc = iselCondCode(env, e->Iex.ITE.cond);
         addInstr(env, ARMInstr_CMov(cc ^ 1, dst, r0));
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


/* -------------------- 64-bit -------------------- */

/* Compute a 64-bit value into a register pair, which is returned as
   the first two parameters.  As with iselIntExpr_R, these may be
   either real or virtual regs; in any case they must not be changed
   by subsequent code emitted by the caller.  */

static void iselInt64Expr ( HReg* rHi, HReg* rLo, ISelEnv* env,
                            const IRExpr* e )
{
   iselInt64Expr_wrk(rHi, rLo, env, e);
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(*rHi) == HRcInt32);
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcInt32);
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt64Expr_wrk ( HReg* rHi, HReg* rLo, ISelEnv* env,
                                const IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I64);

   /* 64-bit literal */
   if (e->tag == Iex_Const) {
      ULong   w64 = e->Iex.Const.con->Ico.U64;
      UInt    wHi = toUInt(w64 >> 32);
      UInt    wLo = toUInt(w64);
      HReg    tHi = newVRegI(env);
      HReg    tLo = newVRegI(env);
      vassert(e->Iex.Const.con->tag == Ico_U64);
      addInstr(env, ARMInstr_Imm32(tHi, wHi));
      addInstr(env, ARMInstr_Imm32(tLo, wLo));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* read 64-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
         HReg tHi = newVRegI(env);
         HReg tLo = newVRegI(env);
         HReg tmp = iselNeon64Expr(env, e);
         addInstr(env, ARMInstr_VXferD(False, tmp, tHi, tLo));
         *rHi = tHi;
         *rLo = tLo;
      } else {
         lookupIRTemp64( rHi, rLo, env, e->Iex.RdTmp.tmp);
      }
      return;
   }

   /* 64-bit load */
   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      HReg      tLo, tHi, rA;
      vassert(e->Iex.Load.ty == Ity_I64);
      rA  = iselIntExpr_R(env, e->Iex.Load.addr);
      tHi = newVRegI(env);
      tLo = newVRegI(env);
      addInstr(env, ARMInstr_LdSt32(ARMcc_AL, True/*isLoad*/,
                                    tHi, ARMAMode1_RI(rA, 4)));
      addInstr(env, ARMInstr_LdSt32(ARMcc_AL, True/*isLoad*/,
                                    tLo, ARMAMode1_RI(rA, 0)));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit GET */
   if (e->tag == Iex_Get) {
      ARMAMode1* am0 = ARMAMode1_RI(hregARM_R8(), e->Iex.Get.offset + 0);
      ARMAMode1* am4 = ARMAMode1_RI(hregARM_R8(), e->Iex.Get.offset + 4);
      HReg tHi = newVRegI(env);
      HReg tLo = newVRegI(env);
      addInstr(env, ARMInstr_LdSt32(ARMcc_AL, True/*isLoad*/, tHi, am4));
      addInstr(env, ARMInstr_LdSt32(ARMcc_AL, True/*isLoad*/, tLo, am0));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {

         /* 32 x 32 -> 64 multiply */
         case Iop_MullS32:
         case Iop_MullU32: {
            HReg     argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
            HReg     argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg     tHi  = newVRegI(env);
            HReg     tLo  = newVRegI(env);
            ARMMulOp mop  = e->Iex.Binop.op == Iop_MullS32
                               ? ARMmul_SX : ARMmul_ZX;
            addInstr(env, mk_iMOVds_RR(hregARM_R2(), argL));
            addInstr(env, mk_iMOVds_RR(hregARM_R3(), argR));
            addInstr(env, ARMInstr_Mul(mop));
            addInstr(env, mk_iMOVds_RR(tHi, hregARM_R1()));
            addInstr(env, mk_iMOVds_RR(tLo, hregARM_R0()));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Or64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tHi = newVRegI(env);
            HReg tLo = newVRegI(env);
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_Alu(ARMalu_OR, tHi, xHi, ARMRI84_R(yHi)));
            addInstr(env, ARMInstr_Alu(ARMalu_OR, tLo, xLo, ARMRI84_R(yLo)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Add64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tHi = newVRegI(env);
            HReg tLo = newVRegI(env);
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_Alu(ARMalu_ADDS, tLo, xLo, ARMRI84_R(yLo)));
            addInstr(env, ARMInstr_Alu(ARMalu_ADC,  tHi, xHi, ARMRI84_R(yHi)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32HLto64(e1,e2) */
         case Iop_32HLto64: {
            *rHi = iselIntExpr_R(env, e->Iex.Binop.arg1);
            *rLo = iselIntExpr_R(env, e->Iex.Binop.arg2);
            return;
         }

         default:
            break;
      }
   }

   /* --------- UNARY ops --------- */
   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {

         /* ReinterpF64asI64 */
         case Iop_ReinterpF64asI64: {
            HReg dstHi = newVRegI(env);
            HReg dstLo = newVRegI(env);
            HReg src   = iselDblExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_VXferD(False/*!toD*/, src, dstHi, dstLo));
            *rHi = dstHi;
            *rLo = dstLo;
            return;
         }

         /* Left64(e) */
         case Iop_Left64: {
            HReg yLo, yHi;
            HReg tHi  = newVRegI(env);
            HReg tLo  = newVRegI(env);
            HReg zero = newVRegI(env);
            /* yHi:yLo = arg */
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Unop.arg);
            /* zero = 0 */
            addInstr(env, ARMInstr_Imm32(zero, 0));
            /* tLo = 0 - yLo, and set carry */
            addInstr(env, ARMInstr_Alu(ARMalu_SUBS,
                                       tLo, zero, ARMRI84_R(yLo)));
            /* tHi = 0 - yHi - carry */
            addInstr(env, ARMInstr_Alu(ARMalu_SBC,
                                       tHi, zero, ARMRI84_R(yHi)));
            /* So now we have tHi:tLo = -arg.  To finish off, or 'arg'
               back in, so as to give the final result 
               tHi:tLo = arg | -arg. */
            addInstr(env, ARMInstr_Alu(ARMalu_OR, tHi, tHi, ARMRI84_R(yHi)));
            addInstr(env, ARMInstr_Alu(ARMalu_OR, tLo, tLo, ARMRI84_R(yLo)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* CmpwNEZ64(e) */
         case Iop_CmpwNEZ64: {
            HReg srcLo, srcHi;
            HReg tmp1 = newVRegI(env);
            HReg tmp2 = newVRegI(env);
            /* srcHi:srcLo = arg */
            iselInt64Expr(&srcHi, &srcLo, env, e->Iex.Unop.arg);
            /* tmp1 = srcHi | srcLo */
            addInstr(env, ARMInstr_Alu(ARMalu_OR,
                                       tmp1, srcHi, ARMRI84_R(srcLo)));
            /* tmp2 = (tmp1 | -tmp1) >>s 31 */
            addInstr(env, ARMInstr_Unary(ARMun_NEG, tmp2, tmp1));
            addInstr(env, ARMInstr_Alu(ARMalu_OR,
                                       tmp2, tmp2, ARMRI84_R(tmp1)));
            addInstr(env, ARMInstr_Shift(ARMsh_SAR,
                                         tmp2, tmp2, ARMRI5_I5(31)));
            *rHi = tmp2;
            *rLo = tmp2;
            return;
         }

         case Iop_1Sto64: {
            HReg        dst  = newVRegI(env);
            ARMCondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            ARMRI5*     amt  = ARMRI5_I5(31);
            /* This is really rough.  We could do much better here;
               perhaps mvn{cond} dst, #0 as the second insn?
               (same applies to 1Sto32) */
            addInstr(env, ARMInstr_Mov(dst, ARMRI84_I84(0,0)));
            addInstr(env, ARMInstr_CMov(cond, dst, ARMRI84_I84(1,0)));
            addInstr(env, ARMInstr_Shift(ARMsh_SHL, dst, dst, amt));
            addInstr(env, ARMInstr_Shift(ARMsh_SAR, dst, dst, amt));
            *rHi = dst;
            *rLo = dst;
            return;
         }

         default: 
            break;
      }
   } /* if (e->tag == Iex_Unop) */

   /* --------- MULTIPLEX --------- */
   if (e->tag == Iex_ITE) { // VFD
      IRType tyC;
      HReg   r1hi, r1lo, r0hi, r0lo, dstHi, dstLo;
      ARMCondCode cc;
      tyC = typeOfIRExpr(env->type_env,e->Iex.ITE.cond);
      vassert(tyC == Ity_I1);
      iselInt64Expr(&r1hi, &r1lo, env, e->Iex.ITE.iftrue);
      iselInt64Expr(&r0hi, &r0lo, env, e->Iex.ITE.iffalse);
      dstHi = newVRegI(env);
      dstLo = newVRegI(env);
      addInstr(env, mk_iMOVds_RR(dstHi, r1hi));
      addInstr(env, mk_iMOVds_RR(dstLo, r1lo));
      cc = iselCondCode(env, e->Iex.ITE.cond);
      addInstr(env, ARMInstr_CMov(cc ^ 1, dstHi, ARMRI84_R(r0hi)));
      addInstr(env, ARMInstr_CMov(cc ^ 1, dstLo, ARMRI84_R(r0lo)));
      *rHi = dstHi;
      *rLo = dstLo;
      return;
   }

   /* It is convenient sometimes to call iselInt64Expr even when we
      have NEON support (e.g. in do_helper_call we need 64-bit
      arguments as 2 x 32 regs). */
   if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
      HReg tHi = newVRegI(env);
      HReg tLo = newVRegI(env);
      HReg tmp = iselNeon64Expr(env, e);
      addInstr(env, ARMInstr_VXferD(False, tmp, tHi, tLo));
      *rHi = tHi;
      *rLo = tLo;
      return ;
   }

   ppIRExpr(e);
   vpanic("iselInt64Expr");
}


/*---------------------------------------------------------*/
/*--- ISEL: Vector (NEON) expressions (64 or 128 bit)   ---*/
/*---------------------------------------------------------*/

static HReg iselNeon64Expr ( ISelEnv* env, const IRExpr* e )
{
   HReg r;
   vassert(env->hwcaps & VEX_HWCAPS_ARM_NEON);
   r = iselNeon64Expr_wrk( env, e );
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselNeon64Expr_wrk ( ISelEnv* env, const IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   MatchInfo mi;
   vassert(e);
   vassert(ty == Ity_I64);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Const) {
      HReg rLo, rHi;
      HReg res = newVRegD(env);
      iselInt64Expr(&rHi, &rLo, env, e);
      addInstr(env, ARMInstr_VXferD(True/*toD*/, res, rHi, rLo));
      return res;
   }

   /* 64-bit load */
   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      HReg res = newVRegD(env);
      ARMAModeN* am = iselIntExpr_AModeN(env, e->Iex.Load.addr);
      vassert(ty == Ity_I64);
      addInstr(env, ARMInstr_NLdStD(True, res, am));
      return res;
   }

   /* 64-bit GET */
   if (e->tag == Iex_Get) {
      HReg addr = newVRegI(env);
      HReg res = newVRegD(env);
      vassert(ty == Ity_I64);
      addInstr(env, ARMInstr_Add32(addr, hregARM_R8(), e->Iex.Get.offset));
      addInstr(env, ARMInstr_NLdStD(True, res, mkARMAModeN_R(addr)));
      return res;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {

         /* 32 x 32 -> 64 multiply */
         case Iop_MullS32:
         case Iop_MullU32: {
            HReg rLo, rHi;
            HReg res = newVRegD(env);
            iselInt64Expr(&rHi, &rLo, env, e);
            addInstr(env, ARMInstr_VXferD(True/*toD*/, res, rHi, rLo));
            return res;
         }

         case Iop_And64: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VAND,
                                           res, argL, argR, 4, False));
            return res;
         }
         case Iop_Or64: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VORR,
                                           res, argL, argR, 4, False));
            return res;
         }
         case Iop_Xor64: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VXOR,
                                           res, argL, argR, 4, False));
            return res;
         }

         /* 32HLto64(e1,e2) */
         case Iop_32HLto64: {
            HReg rHi = iselIntExpr_R(env, e->Iex.Binop.arg1);
            HReg rLo = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg res = newVRegD(env);
            addInstr(env, ARMInstr_VXferD(True/*toD*/, res, rHi, rLo));
            return res;
         }

         case Iop_Add8x8:
         case Iop_Add16x4:
         case Iop_Add32x2:
         case Iop_Add64: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Add8x8: size = 0; break;
               case Iop_Add16x4: size = 1; break;
               case Iop_Add32x2: size = 2; break;
               case Iop_Add64: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VADD,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Add32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VADDFP,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_RecipStep32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VRECPS,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_RSqrtStep32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VRSQRTS,
                                           res, argL, argR, size, False));
            return res;
         }

         // These 6 verified 18 Apr 2013
         case Iop_InterleaveHI32x2:
         case Iop_InterleaveLO32x2:
         case Iop_InterleaveOddLanes8x8:
         case Iop_InterleaveEvenLanes8x8:
         case Iop_InterleaveOddLanes16x4:
         case Iop_InterleaveEvenLanes16x4: {
            HReg rD   = newVRegD(env);
            HReg rM   = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            Bool resRd;  // is the result in rD or rM ?
            switch (e->Iex.Binop.op) {
               case Iop_InterleaveOddLanes8x8:   resRd = False; size = 0; break;
               case Iop_InterleaveEvenLanes8x8:  resRd = True;  size = 0; break;
               case Iop_InterleaveOddLanes16x4:  resRd = False; size = 1; break;
               case Iop_InterleaveEvenLanes16x4: resRd = True;  size = 1; break;
               case Iop_InterleaveHI32x2:        resRd = False; size = 2; break;
               case Iop_InterleaveLO32x2:        resRd = True;  size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rM, argL, 4, False));
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rD, argR, 4, False));
            addInstr(env, ARMInstr_NDual(ARMneon_TRN, rD, rM, size, False));
            return resRd ? rD : rM;
         }

         // These 4 verified 18 Apr 2013
         case Iop_InterleaveHI8x8:
         case Iop_InterleaveLO8x8:
         case Iop_InterleaveHI16x4:
         case Iop_InterleaveLO16x4: {
            HReg rD   = newVRegD(env);
            HReg rM   = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            Bool resRd;  // is the result in rD or rM ?
            switch (e->Iex.Binop.op) {
               case Iop_InterleaveHI8x8:  resRd = False; size = 0; break;
               case Iop_InterleaveLO8x8:  resRd = True;  size = 0; break;
               case Iop_InterleaveHI16x4: resRd = False; size = 1; break;
               case Iop_InterleaveLO16x4: resRd = True;  size = 1; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rM, argL, 4, False));
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rD, argR, 4, False));
            addInstr(env, ARMInstr_NDual(ARMneon_ZIP, rD, rM, size, False));
            return resRd ? rD : rM;
         }

         // These 4 verified 18 Apr 2013
         case Iop_CatOddLanes8x8:
         case Iop_CatEvenLanes8x8:
         case Iop_CatOddLanes16x4:
         case Iop_CatEvenLanes16x4: {
            HReg rD   = newVRegD(env);
            HReg rM   = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            Bool resRd;  // is the result in rD or rM ?
            switch (e->Iex.Binop.op) {
               case Iop_CatOddLanes8x8:   resRd = False; size = 0; break;
               case Iop_CatEvenLanes8x8:  resRd = True;  size = 0; break;
               case Iop_CatOddLanes16x4:  resRd = False; size = 1; break;
               case Iop_CatEvenLanes16x4: resRd = True;  size = 1; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rM, argL, 4, False));
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rD, argR, 4, False));
            addInstr(env, ARMInstr_NDual(ARMneon_UZP, rD, rM, size, False));
            return resRd ? rD : rM;
         }

         case Iop_QAdd8Ux8:
         case Iop_QAdd16Ux4:
         case Iop_QAdd32Ux2:
         case Iop_QAdd64Ux1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QAdd8Ux8: size = 0; break;
               case Iop_QAdd16Ux4: size = 1; break;
               case Iop_QAdd32Ux2: size = 2; break;
               case Iop_QAdd64Ux1: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQADDU,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_QAdd8Sx8:
         case Iop_QAdd16Sx4:
         case Iop_QAdd32Sx2:
         case Iop_QAdd64Sx1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QAdd8Sx8: size = 0; break;
               case Iop_QAdd16Sx4: size = 1; break;
               case Iop_QAdd32Sx2: size = 2; break;
               case Iop_QAdd64Sx1: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQADDS,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Sub8x8:
         case Iop_Sub16x4:
         case Iop_Sub32x2:
         case Iop_Sub64: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Sub8x8: size = 0; break;
               case Iop_Sub16x4: size = 1; break;
               case Iop_Sub32x2: size = 2; break;
               case Iop_Sub64: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUB,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Sub32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUBFP,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_QSub8Ux8:
         case Iop_QSub16Ux4:
         case Iop_QSub32Ux2:
         case Iop_QSub64Ux1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QSub8Ux8: size = 0; break;
               case Iop_QSub16Ux4: size = 1; break;
               case Iop_QSub32Ux2: size = 2; break;
               case Iop_QSub64Ux1: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQSUBU,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_QSub8Sx8:
         case Iop_QSub16Sx4:
         case Iop_QSub32Sx2:
         case Iop_QSub64Sx1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QSub8Sx8: size = 0; break;
               case Iop_QSub16Sx4: size = 1; break;
               case Iop_QSub32Sx2: size = 2; break;
               case Iop_QSub64Sx1: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQSUBS,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Max8Ux8:
         case Iop_Max16Ux4:
         case Iop_Max32Ux2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Max8Ux8: size = 0; break;
               case Iop_Max16Ux4: size = 1; break;
               case Iop_Max32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMAXU,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Max8Sx8:
         case Iop_Max16Sx4:
         case Iop_Max32Sx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Max8Sx8: size = 0; break;
               case Iop_Max16Sx4: size = 1; break;
               case Iop_Max32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMAXS,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Min8Ux8:
         case Iop_Min16Ux4:
         case Iop_Min32Ux2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Min8Ux8: size = 0; break;
               case Iop_Min16Ux4: size = 1; break;
               case Iop_Min32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMINU,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Min8Sx8:
         case Iop_Min16Sx4:
         case Iop_Min32Sx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Min8Sx8: size = 0; break;
               case Iop_Min16Sx4: size = 1; break;
               case Iop_Min32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMINS,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Sar8x8:
         case Iop_Sar16x4:
         case Iop_Sar32x2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            HReg argR2 = newVRegD(env);
            HReg zero = newVRegD(env);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Sar8x8: size = 0; break;
               case Iop_Sar16x4: size = 1; break;
               case Iop_Sar32x2: size = 2; break;
               case Iop_Sar64: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NeonImm(zero, ARMNImm_TI(0,0)));
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUB,
                                           argR2, zero, argR, size, False));
            addInstr(env, ARMInstr_NShift(ARMneon_VSAL,
                                          res, argL, argR2, size, False));
            return res;
         }
         case Iop_Sal8x8:
         case Iop_Sal16x4:
         case Iop_Sal32x2:
         case Iop_Sal64x1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Sal8x8: size = 0; break;
               case Iop_Sal16x4: size = 1; break;
               case Iop_Sal32x2: size = 2; break;
               case Iop_Sal64x1: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NShift(ARMneon_VSAL,
                                          res, argL, argR, size, False));
            return res;
         }
         case Iop_Shr8x8:
         case Iop_Shr16x4:
         case Iop_Shr32x2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            HReg argR2 = newVRegD(env);
            HReg zero = newVRegD(env);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Shr8x8: size = 0; break;
               case Iop_Shr16x4: size = 1; break;
               case Iop_Shr32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NeonImm(zero, ARMNImm_TI(0,0)));
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUB,
                                           argR2, zero, argR, size, False));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          res, argL, argR2, size, False));
            return res;
         }
         case Iop_Shl8x8:
         case Iop_Shl16x4:
         case Iop_Shl32x2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Shl8x8: size = 0; break;
               case Iop_Shl16x4: size = 1; break;
               case Iop_Shl32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          res, argL, argR, size, False));
            return res;
         }
         case Iop_QShl8x8:
         case Iop_QShl16x4:
         case Iop_QShl32x2:
         case Iop_QShl64x1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QShl8x8: size = 0; break;
               case Iop_QShl16x4: size = 1; break;
               case Iop_QShl32x2: size = 2; break;
               case Iop_QShl64x1: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NShift(ARMneon_VQSHL,
                                          res, argL, argR, size, False));
            return res;
         }
         case Iop_QSal8x8:
         case Iop_QSal16x4:
         case Iop_QSal32x2:
         case Iop_QSal64x1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QSal8x8: size = 0; break;
               case Iop_QSal16x4: size = 1; break;
               case Iop_QSal32x2: size = 2; break;
               case Iop_QSal64x1: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NShift(ARMneon_VQSAL,
                                          res, argL, argR, size, False));
            return res;
         }
         case Iop_QShlNsatUU8x8:
         case Iop_QShlNsatUU16x4:
         case Iop_QShlNsatUU32x2:
         case Iop_QShlNsatUU64x1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            UInt size, imm;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
               vpanic("ARM target supports Iop_QShlNsatUUAxB with constant "
                      "second argument only\n");
            }
            imm = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch (e->Iex.Binop.op) {
               case Iop_QShlNsatUU8x8: size = 8 | imm; break;
               case Iop_QShlNsatUU16x4: size = 16 | imm; break;
               case Iop_QShlNsatUU32x2: size = 32 | imm; break;
               case Iop_QShlNsatUU64x1: size = 64 | imm; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_VQSHLNUU,
                                          res, argL, size, False));
            return res;
         }
         case Iop_QShlNsatSU8x8:
         case Iop_QShlNsatSU16x4:
         case Iop_QShlNsatSU32x2:
         case Iop_QShlNsatSU64x1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            UInt size, imm;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
               vpanic("ARM target supports Iop_QShlNsatSUAxB with constant "
                      "second argument only\n");
            }
            imm = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch (e->Iex.Binop.op) {
               case Iop_QShlNsatSU8x8: size = 8 | imm; break;
               case Iop_QShlNsatSU16x4: size = 16 | imm; break;
               case Iop_QShlNsatSU32x2: size = 32 | imm; break;
               case Iop_QShlNsatSU64x1: size = 64 | imm; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_VQSHLNUS,
                                          res, argL, size, False));
            return res;
         }
         case Iop_QShlNsatSS8x8:
         case Iop_QShlNsatSS16x4:
         case Iop_QShlNsatSS32x2:
         case Iop_QShlNsatSS64x1: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            UInt size, imm;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
               vpanic("ARM target supports Iop_QShlNsatSSAxB with constant "
                      "second argument only\n");
            }
            imm = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch (e->Iex.Binop.op) {
               case Iop_QShlNsatSS8x8: size = 8 | imm; break;
               case Iop_QShlNsatSS16x4: size = 16 | imm; break;
               case Iop_QShlNsatSS32x2: size = 32 | imm; break;
               case Iop_QShlNsatSS64x1: size = 64 | imm; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_VQSHLNSS,
                                          res, argL, size, False));
            return res;
         }
         case Iop_ShrN8x8:
         case Iop_ShrN16x4:
         case Iop_ShrN32x2:
         case Iop_Shr64: {
            HReg res = newVRegD(env);
            HReg tmp = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg argR2 = newVRegI(env);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_ShrN8x8: size = 0; break;
               case Iop_ShrN16x4: size = 1; break;
               case Iop_ShrN32x2: size = 2; break;
               case Iop_Shr64: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_Unary(ARMun_NEG, argR2, argR));
            addInstr(env, ARMInstr_NUnary(ARMneon_DUP, tmp, argR2, 0, False));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          res, argL, tmp, size, False));
            return res;
         }
         case Iop_ShlN8x8:
         case Iop_ShlN16x4:
         case Iop_ShlN32x2:
         case Iop_Shl64: {
            HReg res = newVRegD(env);
            HReg tmp = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            /* special-case Shl64(x, imm8) since the Neon front
               end produces a lot of those for V{LD,ST}{1,2,3,4}. */
            if (e->Iex.Binop.op == Iop_Shl64 
                && e->Iex.Binop.arg2->tag == Iex_Const) {
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               Int nshift = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
               if (nshift >= 1 && nshift <= 63) {
                  addInstr(env, ARMInstr_NShl64(res, argL, nshift));
                  return res;
               }
               /* else fall through to general case */
            }
            HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_ShlN8x8:  size = 0; break;
               case Iop_ShlN16x4: size = 1; break;
               case Iop_ShlN32x2: size = 2; break;
               case Iop_Shl64:    size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_DUP,
                                          tmp, argR, 0, False));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          res, argL, tmp, size, False));
            return res;
         }
         case Iop_SarN8x8:
         case Iop_SarN16x4:
         case Iop_SarN32x2:
         case Iop_Sar64: {
            HReg res = newVRegD(env);
            HReg tmp = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg argR2 = newVRegI(env);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_SarN8x8: size = 0; break;
               case Iop_SarN16x4: size = 1; break;
               case Iop_SarN32x2: size = 2; break;
               case Iop_Sar64: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_Unary(ARMun_NEG, argR2, argR));
            addInstr(env, ARMInstr_NUnary(ARMneon_DUP, tmp, argR2, 0, False));
            addInstr(env, ARMInstr_NShift(ARMneon_VSAL,
                                          res, argL, tmp, size, False));
            return res;
         }
         case Iop_CmpGT8Ux8:
         case Iop_CmpGT16Ux4:
         case Iop_CmpGT32Ux2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_CmpGT8Ux8: size = 0; break;
               case Iop_CmpGT16Ux4: size = 1; break;
               case Iop_CmpGT32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VCGTU,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_CmpGT8Sx8:
         case Iop_CmpGT16Sx4:
         case Iop_CmpGT32Sx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_CmpGT8Sx8: size = 0; break;
               case Iop_CmpGT16Sx4: size = 1; break;
               case Iop_CmpGT32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VCGTS,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_CmpEQ8x8:
         case Iop_CmpEQ16x4:
         case Iop_CmpEQ32x2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_CmpEQ8x8: size = 0; break;
               case Iop_CmpEQ16x4: size = 1; break;
               case Iop_CmpEQ32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VCEQ,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Mul8x8:
         case Iop_Mul16x4:
         case Iop_Mul32x2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Mul8x8: size = 0; break;
               case Iop_Mul16x4: size = 1; break;
               case Iop_Mul32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMUL,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Mul32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VMULFP,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_QDMulHi16Sx4:
         case Iop_QDMulHi32Sx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_QDMulHi16Sx4: size = 1; break;
               case Iop_QDMulHi32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQDMULH,
                                           res, argL, argR, size, False));
            return res;
         }

         case Iop_QRDMulHi16Sx4:
         case Iop_QRDMulHi32Sx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_QRDMulHi16Sx4: size = 1; break;
               case Iop_QRDMulHi32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQRDMULH,
                                           res, argL, argR, size, False));
            return res;
         }

         case Iop_PwAdd8x8:
         case Iop_PwAdd16x4:
         case Iop_PwAdd32x2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwAdd8x8: size = 0; break;
               case Iop_PwAdd16x4: size = 1; break;
               case Iop_PwAdd32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VPADD,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_PwAdd32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VPADDFP,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_PwMin8Ux8:
         case Iop_PwMin16Ux4:
         case Iop_PwMin32Ux2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwMin8Ux8: size = 0; break;
               case Iop_PwMin16Ux4: size = 1; break;
               case Iop_PwMin32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VPMINU,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_PwMin8Sx8:
         case Iop_PwMin16Sx4:
         case Iop_PwMin32Sx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwMin8Sx8: size = 0; break;
               case Iop_PwMin16Sx4: size = 1; break;
               case Iop_PwMin32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VPMINS,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_PwMax8Ux8:
         case Iop_PwMax16Ux4:
         case Iop_PwMax32Ux2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwMax8Ux8: size = 0; break;
               case Iop_PwMax16Ux4: size = 1; break;
               case Iop_PwMax32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VPMAXU,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_PwMax8Sx8:
         case Iop_PwMax16Sx4:
         case Iop_PwMax32Sx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwMax8Sx8: size = 0; break;
               case Iop_PwMax16Sx4: size = 1; break;
               case Iop_PwMax32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VPMAXS,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Perm8x8: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VTBL,
                                           res, argL, argR, 0, False));
            return res;
         }
         case Iop_PolynomialMul8x8: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VMULP,
                                           res, argL, argR, size, False));
            return res;
         }
         case Iop_Max32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VMAXF,
                                           res, argL, argR, 2, False));
            return res;
         }
         case Iop_Min32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VMINF,
                                           res, argL, argR, 2, False));
            return res;
         }
         case Iop_PwMax32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VPMAXF,
                                           res, argL, argR, 2, False));
            return res;
         }
         case Iop_PwMin32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VPMINF,
                                           res, argL, argR, 2, False));
            return res;
         }
         case Iop_CmpGT32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VCGTF,
                                           res, argL, argR, 2, False));
            return res;
         }
         case Iop_CmpGE32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VCGEF,
                                           res, argL, argR, 2, False));
            return res;
         }
         case Iop_CmpEQ32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VCEQF,
                                           res, argL, argR, 2, False));
            return res;
         }
         case Iop_F32ToFixed32Ux2_RZ:
         case Iop_F32ToFixed32Sx2_RZ:
         case Iop_Fixed32UToF32x2_RN:
         case Iop_Fixed32SToF32x2_RN: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Binop.arg1);
            ARMNeonUnOp op;
            UInt imm6;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
               typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
                  vpanic("ARM supports FP <-> Fixed conversion with constant "
                         "second argument less than 33 only\n");
            }
            imm6 = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            vassert(imm6 <= 32 && imm6 > 0);
            imm6 = 64 - imm6;
            switch(e->Iex.Binop.op) {
               case Iop_F32ToFixed32Ux2_RZ: op = ARMneon_VCVTFtoFixedU; break;
               case Iop_F32ToFixed32Sx2_RZ: op = ARMneon_VCVTFtoFixedS; break;
               case Iop_Fixed32UToF32x2_RN: op = ARMneon_VCVTFixedUtoF; break;
               case Iop_Fixed32SToF32x2_RN: op = ARMneon_VCVTFixedStoF; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(op, res, arg, imm6, False));
            return res;
         }
         /*
         FIXME: is this here or not?
         case Iop_VDup8x8:
         case Iop_VDup16x4:
         case Iop_VDup32x2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            UInt index;
            UInt imm4;
            UInt size = 0;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
               typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
                  vpanic("ARM supports Iop_VDup with constant "
                         "second argument less than 16 only\n");
            }
            index = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch(e->Iex.Binop.op) {
               case Iop_VDup8x8: imm4 = (index << 1) + 1; break;
               case Iop_VDup16x4: imm4 = (index << 2) + 2; break;
               case Iop_VDup32x2: imm4 = (index << 3) + 4; break;
               default: vassert(0);
            }
            if (imm4 >= 16) {
               vpanic("ARM supports Iop_VDup with constant "
                      "second argument less than 16 only\n");
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_VDUP,
                                          res, argL, imm4, False));
            return res;
         }
         */
         default:
            break;
      }
   }

   /* --------- UNARY ops --------- */
   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {

         /* 32Uto64 */
         case Iop_32Uto64: {
            HReg rLo = iselIntExpr_R(env, e->Iex.Unop.arg);
            HReg rHi = newVRegI(env);
            HReg res = newVRegD(env);
            addInstr(env, ARMInstr_Imm32(rHi, 0));
            addInstr(env, ARMInstr_VXferD(True/*toD*/, res, rHi, rLo));
            return res;
         }

         /* 32Sto64 */
         case Iop_32Sto64: {
            HReg rLo = iselIntExpr_R(env, e->Iex.Unop.arg);
            HReg rHi = newVRegI(env);
            addInstr(env, mk_iMOVds_RR(rHi, rLo));
            addInstr(env, ARMInstr_Shift(ARMsh_SAR, rHi, rHi, ARMRI5_I5(31)));
            HReg res = newVRegD(env);
            addInstr(env, ARMInstr_VXferD(True/*toD*/, res, rHi, rLo));
            return res;
         }

         /* The next 3 are pass-throughs */
         /* ReinterpF64asI64 */
         case Iop_ReinterpF64asI64:
         /* Left64(e) */
         case Iop_Left64:
         /* CmpwNEZ64(e) */
         case Iop_1Sto64: {
            HReg rLo, rHi;
            HReg res = newVRegD(env);
            iselInt64Expr(&rHi, &rLo, env, e);
            addInstr(env, ARMInstr_VXferD(True/*toD*/, res, rHi, rLo));
            return res;
         }

         case Iop_Not64: {
            DECLARE_PATTERN(p_veqz_8x8);
            DECLARE_PATTERN(p_veqz_16x4);
            DECLARE_PATTERN(p_veqz_32x2);
            DECLARE_PATTERN(p_vcge_8sx8);
            DECLARE_PATTERN(p_vcge_16sx4);
            DECLARE_PATTERN(p_vcge_32sx2);
            DECLARE_PATTERN(p_vcge_8ux8);
            DECLARE_PATTERN(p_vcge_16ux4);
            DECLARE_PATTERN(p_vcge_32ux2);
            DEFINE_PATTERN(p_veqz_8x8,
                  unop(Iop_Not64, unop(Iop_CmpNEZ8x8, bind(0))));
            DEFINE_PATTERN(p_veqz_16x4,
                  unop(Iop_Not64, unop(Iop_CmpNEZ16x4, bind(0))));
            DEFINE_PATTERN(p_veqz_32x2,
                  unop(Iop_Not64, unop(Iop_CmpNEZ32x2, bind(0))));
            DEFINE_PATTERN(p_vcge_8sx8,
                  unop(Iop_Not64, binop(Iop_CmpGT8Sx8, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_16sx4,
                  unop(Iop_Not64, binop(Iop_CmpGT16Sx4, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_32sx2,
                  unop(Iop_Not64, binop(Iop_CmpGT32Sx2, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_8ux8,
                  unop(Iop_Not64, binop(Iop_CmpGT8Ux8, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_16ux4,
                  unop(Iop_Not64, binop(Iop_CmpGT16Ux4, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_32ux2,
                  unop(Iop_Not64, binop(Iop_CmpGT32Ux2, bind(1), bind(0))));
            if (matchIRExpr(&mi, p_veqz_8x8, e)) {
               HReg res = newVRegD(env);
               HReg arg = iselNeon64Expr(env, mi.bindee[0]);
               addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, res, arg, 0, False));
               return res;
            } else if (matchIRExpr(&mi, p_veqz_16x4, e)) {
               HReg res = newVRegD(env);
               HReg arg = iselNeon64Expr(env, mi.bindee[0]);
               addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, res, arg, 1, False));
               return res;
            } else if (matchIRExpr(&mi, p_veqz_32x2, e)) {
               HReg res = newVRegD(env);
               HReg arg = iselNeon64Expr(env, mi.bindee[0]);
               addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, res, arg, 2, False));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_8sx8, e)) {
               HReg res = newVRegD(env);
               HReg argL = iselNeon64Expr(env, mi.bindee[0]);
               HReg argR = iselNeon64Expr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGES,
                                              res, argL, argR, 0, False));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_16sx4, e)) {
               HReg res = newVRegD(env);
               HReg argL = iselNeon64Expr(env, mi.bindee[0]);
               HReg argR = iselNeon64Expr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGES,
                                              res, argL, argR, 1, False));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_32sx2, e)) {
               HReg res = newVRegD(env);
               HReg argL = iselNeon64Expr(env, mi.bindee[0]);
               HReg argR = iselNeon64Expr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGES,
                                              res, argL, argR, 2, False));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_8ux8, e)) {
               HReg res = newVRegD(env);
               HReg argL = iselNeon64Expr(env, mi.bindee[0]);
               HReg argR = iselNeon64Expr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGEU,
                                              res, argL, argR, 0, False));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_16ux4, e)) {
               HReg res = newVRegD(env);
               HReg argL = iselNeon64Expr(env, mi.bindee[0]);
               HReg argR = iselNeon64Expr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGEU,
                                              res, argL, argR, 1, False));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_32ux2, e)) {
               HReg res = newVRegD(env);
               HReg argL = iselNeon64Expr(env, mi.bindee[0]);
               HReg argR = iselNeon64Expr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGEU,
                                              res, argL, argR, 2, False));
               return res;
            } else {
               HReg res = newVRegD(env);
               HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
               addInstr(env, ARMInstr_NUnary(ARMneon_NOT, res, arg, 4, False));
               return res;
            }
         }
         case Iop_Dup8x8:
         case Iop_Dup16x4:
         case Iop_Dup32x2: {
            HReg res, arg;
            UInt size;
            DECLARE_PATTERN(p_vdup_8x8);
            DECLARE_PATTERN(p_vdup_16x4);
            DECLARE_PATTERN(p_vdup_32x2);
            DEFINE_PATTERN(p_vdup_8x8,
                  unop(Iop_Dup8x8, binop(Iop_GetElem8x8, bind(0), bind(1))));
            DEFINE_PATTERN(p_vdup_16x4,
                  unop(Iop_Dup16x4, binop(Iop_GetElem16x4, bind(0), bind(1))));
            DEFINE_PATTERN(p_vdup_32x2,
                  unop(Iop_Dup32x2, binop(Iop_GetElem32x2, bind(0), bind(1))));
            if (matchIRExpr(&mi, p_vdup_8x8, e)) {
               UInt index;
               UInt imm4;
               if (mi.bindee[1]->tag == Iex_Const &&
                  typeOfIRExpr(env->type_env, mi.bindee[1]) == Ity_I8) {
                  index = mi.bindee[1]->Iex.Const.con->Ico.U8;
                  imm4 = (index << 1) + 1;
                  if (index < 8) {
                     res = newVRegD(env);
                     arg = iselNeon64Expr(env, mi.bindee[0]);
                     addInstr(env, ARMInstr_NUnaryS(
                                      ARMneon_VDUP,
                                      mkARMNRS(ARMNRS_Reg, res, 0),
                                      mkARMNRS(ARMNRS_Scalar, arg, index),
                                      imm4, False
                             ));
                     return res;
                  }
               }
            } else if (matchIRExpr(&mi, p_vdup_16x4, e)) {
               UInt index;
               UInt imm4;
               if (mi.bindee[1]->tag == Iex_Const &&
                  typeOfIRExpr(env->type_env, mi.bindee[1]) == Ity_I8) {
                  index = mi.bindee[1]->Iex.Const.con->Ico.U8;
                  imm4 = (index << 2) + 2;
                  if (index < 4) {
                     res = newVRegD(env);
                     arg = iselNeon64Expr(env, mi.bindee[0]);
                     addInstr(env, ARMInstr_NUnaryS(
                                      ARMneon_VDUP,
                                      mkARMNRS(ARMNRS_Reg, res, 0),
                                      mkARMNRS(ARMNRS_Scalar, arg, index),
                                      imm4, False
                             ));
                     return res;
                  }
               }
            } else if (matchIRExpr(&mi, p_vdup_32x2, e)) {
               UInt index;
               UInt imm4;
               if (mi.bindee[1]->tag == Iex_Const &&
                  typeOfIRExpr(env->type_env, mi.bindee[1]) == Ity_I8) {
                  index = mi.bindee[1]->Iex.Const.con->Ico.U8;
                  imm4 = (index << 3) + 4;
                  if (index < 2) {
                     res = newVRegD(env);
                     arg = iselNeon64Expr(env, mi.bindee[0]);
                     addInstr(env, ARMInstr_NUnaryS(
                                      ARMneon_VDUP,
                                      mkARMNRS(ARMNRS_Reg, res, 0),
                                      mkARMNRS(ARMNRS_Scalar, arg, index),
                                      imm4, False
                             ));
                     return res;
                  }
               }
            }
            arg = iselIntExpr_R(env, e->Iex.Unop.arg);
            res = newVRegD(env);
            switch (e->Iex.Unop.op) {
               case Iop_Dup8x8: size = 0; break;
               case Iop_Dup16x4: size = 1; break;
               case Iop_Dup32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_DUP, res, arg, size, False));
            return res;
         }
         case Iop_Abs8x8:
         case Iop_Abs16x4:
         case Iop_Abs32x2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Abs8x8: size = 0; break;
               case Iop_Abs16x4: size = 1; break;
               case Iop_Abs32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_ABS, res, arg, size, False));
            return res;
         }
         case Iop_Reverse8sIn64_x1:
         case Iop_Reverse16sIn64_x1:
         case Iop_Reverse32sIn64_x1: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Reverse8sIn64_x1: size = 0; break;
               case Iop_Reverse16sIn64_x1: size = 1; break;
               case Iop_Reverse32sIn64_x1: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_REV64,
                                          res, arg, size, False));
            return res;
         }
         case Iop_Reverse8sIn32_x2:
         case Iop_Reverse16sIn32_x2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Reverse8sIn32_x2: size = 0; break;
               case Iop_Reverse16sIn32_x2: size = 1; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_REV32,
                                          res, arg, size, False));
            return res;
         }
         case Iop_Reverse8sIn16_x4: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            addInstr(env, ARMInstr_NUnary(ARMneon_REV16,
                                          res, arg, size, False));
            return res;
         }
         case Iop_CmpwNEZ64: {
            HReg x_lsh = newVRegD(env);
            HReg x_rsh = newVRegD(env);
            HReg lsh_amt = newVRegD(env);
            HReg rsh_amt = newVRegD(env);
            HReg zero = newVRegD(env);
            HReg tmp = newVRegD(env);
            HReg tmp2 = newVRegD(env);
            HReg res = newVRegD(env);
            HReg x = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, tmp2, arg, 2, False));
            addInstr(env, ARMInstr_NUnary(ARMneon_NOT, x, tmp2, 4, False));
            addInstr(env, ARMInstr_NeonImm(lsh_amt, ARMNImm_TI(0, 32)));
            addInstr(env, ARMInstr_NeonImm(zero, ARMNImm_TI(0, 0)));
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUB,
                                           rsh_amt, zero, lsh_amt, 2, False));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          x_lsh, x, lsh_amt, 3, False));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          x_rsh, x, rsh_amt, 3, False));
            addInstr(env, ARMInstr_NBinary(ARMneon_VORR,
                                           tmp, x_lsh, x_rsh, 0, False));
            addInstr(env, ARMInstr_NBinary(ARMneon_VORR,
                                           res, tmp, x, 0, False));
            return res;
         }
         case Iop_CmpNEZ8x8:
         case Iop_CmpNEZ16x4:
         case Iop_CmpNEZ32x2: {
            HReg res = newVRegD(env);
            HReg tmp = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size;
            switch (e->Iex.Unop.op) {
               case Iop_CmpNEZ8x8: size = 0; break;
               case Iop_CmpNEZ16x4: size = 1; break;
               case Iop_CmpNEZ32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, tmp, arg, size, False));
            addInstr(env, ARMInstr_NUnary(ARMneon_NOT, res, tmp, 4, False));
            return res;
         }
         case Iop_NarrowUn16to8x8:
         case Iop_NarrowUn32to16x4:
         case Iop_NarrowUn64to32x2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_NarrowUn16to8x8:  size = 0; break;
               case Iop_NarrowUn32to16x4: size = 1; break;
               case Iop_NarrowUn64to32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPYN,
                                          res, arg, size, False));
            return res;
         }
         case Iop_QNarrowUn16Sto8Sx8:
         case Iop_QNarrowUn32Sto16Sx4:
         case Iop_QNarrowUn64Sto32Sx2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_QNarrowUn16Sto8Sx8:  size = 0; break;
               case Iop_QNarrowUn32Sto16Sx4: size = 1; break;
               case Iop_QNarrowUn64Sto32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPYQNSS,
                                          res, arg, size, False));
            return res;
         }
         case Iop_QNarrowUn16Sto8Ux8:
         case Iop_QNarrowUn32Sto16Ux4:
         case Iop_QNarrowUn64Sto32Ux2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_QNarrowUn16Sto8Ux8:  size = 0; break;
               case Iop_QNarrowUn32Sto16Ux4: size = 1; break;
               case Iop_QNarrowUn64Sto32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPYQNUS,
                                          res, arg, size, False));
            return res;
         }
         case Iop_QNarrowUn16Uto8Ux8:
         case Iop_QNarrowUn32Uto16Ux4:
         case Iop_QNarrowUn64Uto32Ux2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_QNarrowUn16Uto8Ux8:  size = 0; break;
               case Iop_QNarrowUn32Uto16Ux4: size = 1; break;
               case Iop_QNarrowUn64Uto32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPYQNUU,
                                          res, arg, size, False));
            return res;
         }
         case Iop_PwAddL8Sx8:
         case Iop_PwAddL16Sx4:
         case Iop_PwAddL32Sx2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwAddL8Sx8: size = 0; break;
               case Iop_PwAddL16Sx4: size = 1; break;
               case Iop_PwAddL32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_PADDLS,
                                          res, arg, size, False));
            return res;
         }
         case Iop_PwAddL8Ux8:
         case Iop_PwAddL16Ux4:
         case Iop_PwAddL32Ux2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwAddL8Ux8: size = 0; break;
               case Iop_PwAddL16Ux4: size = 1; break;
               case Iop_PwAddL32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_PADDLU,
                                          res, arg, size, False));
            return res;
         }
         case Iop_Cnt8x8: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            addInstr(env, ARMInstr_NUnary(ARMneon_CNT,
                                          res, arg, size, False));
            return res;
         }
         case Iop_Clz8x8:
         case Iop_Clz16x4:
         case Iop_Clz32x2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Clz8x8: size = 0; break;
               case Iop_Clz16x4: size = 1; break;
               case Iop_Clz32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_CLZ,
                                          res, arg, size, False));
            return res;
         }
         case Iop_Cls8x8:
         case Iop_Cls16x4:
         case Iop_Cls32x2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Cls8x8: size = 0; break;
               case Iop_Cls16x4: size = 1; break;
               case Iop_Cls32x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_CLS,
                                          res, arg, size, False));
            return res;
         }
         case Iop_F32toI32Sx2_RZ: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTFtoS,
                                          res, arg, 2, False));
            return res;
         }
         case Iop_F32toI32Ux2_RZ: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTFtoU,
                                          res, arg, 2, False));
            return res;
         }
         case Iop_I32StoF32x2_DEP: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTStoF,
                                          res, arg, 2, False));
            return res;
         }
         case Iop_I32UtoF32x2_DEP: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTUtoF,
                                          res, arg, 2, False));
            return res;
         }
         case Iop_F32toF16x4_DEP: {
            HReg res = newVRegD(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTF32toF16,
                                          res, arg, 2, False));
            return res;
         }
         case Iop_RecipEst32Fx2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            addInstr(env, ARMInstr_NUnary(ARMneon_VRECIPF,
                                          res, argL, 0, False));
            return res;
         }
         case Iop_RecipEst32Ux2: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            addInstr(env, ARMInstr_NUnary(ARMneon_VRECIP,
                                          res, argL, 0, False));
            return res;
         }
         case Iop_Abs32Fx2: {
            DECLARE_PATTERN(p_vabd_32fx2);
            DEFINE_PATTERN(p_vabd_32fx2,
                           unop(Iop_Abs32Fx2,
                                binop(Iop_Sub32Fx2,
                                      bind(0),
                                      bind(1))));
            if (matchIRExpr(&mi, p_vabd_32fx2, e)) {
               HReg res = newVRegD(env);
               HReg argL = iselNeon64Expr(env, mi.bindee[0]);
               HReg argR = iselNeon64Expr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VABDFP,
                                              res, argL, argR, 0, False));
               return res;
            } else {
               HReg res = newVRegD(env);
               HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
               addInstr(env, ARMInstr_NUnary(ARMneon_VABSFP,
                                             res, arg, 0, False));
               return res;
            }
         }
         case Iop_RSqrtEst32Fx2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VRSQRTEFP,
                                          res, arg, 0, False));
            return res;
         }
         case Iop_RSqrtEst32Ux2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VRSQRTE,
                                          res, arg, 0, False));
            return res;
         }
         case Iop_Neg32Fx2: {
            HReg res = newVRegD(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VNEGF,
                                          res, arg, 0, False));
            return res;
         }
         case Iop_V128to64:
         case Iop_V128HIto64: {
            HReg src   = iselNeonExpr(env, e->Iex.Unop.arg);
            HReg resLo = newVRegD(env);
            HReg resHi = newVRegD(env);
            addInstr(env, ARMInstr_VXferQ(False/*!toQ*/, src, resHi, resLo));
            return e->Iex.Unop.op == Iop_V128HIto64 ? resHi : resLo;
         }
         default:
            break;
      }
   } /* if (e->tag == Iex_Unop) */

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;

      switch (triop->op) {
         case Iop_Slice64: {
            HReg res = newVRegD(env);
            HReg argL = iselNeon64Expr(env, triop->arg2);
            HReg argR = iselNeon64Expr(env, triop->arg1);
            UInt imm4;
            if (triop->arg3->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, triop->arg3) != Ity_I8) {
               vpanic("ARM target supports Iop_Extract64 with constant "
                      "third argument less than 16 only\n");
            }
            imm4 = triop->arg3->Iex.Const.con->Ico.U8;
            if (imm4 >= 8) {
               vpanic("ARM target supports Iop_Extract64 with constant "
                      "third argument less than 16 only\n");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VEXT,
                                           res, argL, argR, imm4, False));
            return res;
         }
         case Iop_SetElem8x8:
         case Iop_SetElem16x4:
         case Iop_SetElem32x2: {
            HReg res = newVRegD(env);
            HReg dreg = iselNeon64Expr(env, triop->arg1);
            HReg arg = iselIntExpr_R(env, triop->arg3);
            UInt index, size;
            if (triop->arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, triop->arg2) != Ity_I8) {
               vpanic("ARM target supports SetElem with constant "
                      "second argument only\n");
            }
            index = triop->arg2->Iex.Const.con->Ico.U8;
            switch (triop->op) {
               case Iop_SetElem8x8: vassert(index < 8); size = 0; break;
               case Iop_SetElem16x4: vassert(index < 4); size = 1; break;
               case Iop_SetElem32x2: vassert(index < 2); size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, res, dreg, 4, False));
            addInstr(env, ARMInstr_NUnaryS(ARMneon_SETELEM,
                                           mkARMNRS(ARMNRS_Scalar, res, index),
                                           mkARMNRS(ARMNRS_Reg, arg, 0),
                                           size, False));
            return res;
         }
         default:
            break;
      }
   }

   /* --------- MULTIPLEX --------- */
   if (e->tag == Iex_ITE) { // VFD
      HReg rLo, rHi;
      HReg res = newVRegD(env);
      iselInt64Expr(&rHi, &rLo, env, e);
      addInstr(env, ARMInstr_VXferD(True/*toD*/, res, rHi, rLo));
      return res;
   }

   ppIRExpr(e);
   vpanic("iselNeon64Expr");
}


static HReg iselNeonExpr ( ISelEnv* env, const IRExpr* e )
{
   HReg r;
   vassert(env->hwcaps & VEX_HWCAPS_ARM_NEON);
   r = iselNeonExpr_wrk( env, e );
   vassert(hregClass(r) == HRcVec128);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselNeonExpr_wrk ( ISelEnv* env, const IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   MatchInfo mi;
   vassert(e);
   vassert(ty == Ity_V128);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Const) {
      /* At the moment there should be no 128-bit constants in IR for ARM
         generated during disassemble. They are represented as Iop_64HLtoV128
         binary operation and are handled among binary ops. */
      /* But zero can be created by valgrind internal optimizer */
      if (e->Iex.Const.con->Ico.V128 == 0x0000) {
         HReg res = newVRegV(env);
         addInstr(env, ARMInstr_NeonImm(res, ARMNImm_TI(6, 0)));
         return res;
      }
      if (e->Iex.Const.con->Ico.V128 == 0xFFFF) {
         HReg res = newVRegV(env);
         addInstr(env, ARMInstr_NeonImm(res, ARMNImm_TI(6, 255)));
         return res;
      }
      ppIRExpr(e);
      vpanic("128-bit constant is not implemented");
   }

   if (e->tag == Iex_Load) {
      HReg res = newVRegV(env);
      ARMAModeN* am = iselIntExpr_AModeN(env, e->Iex.Load.addr);
      vassert(ty == Ity_V128);
      addInstr(env, ARMInstr_NLdStQ(True, res, am));
      return res;
   }

   if (e->tag == Iex_Get) {
      HReg addr = newVRegI(env);
      HReg res = newVRegV(env);
      vassert(ty == Ity_V128);
      addInstr(env, ARMInstr_Add32(addr, hregARM_R8(), e->Iex.Get.offset));
      addInstr(env, ARMInstr_NLdStQ(True, res, mkARMAModeN_R(addr)));
      return res;
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
         case Iop_NotV128: {
            DECLARE_PATTERN(p_veqz_8x16);
            DECLARE_PATTERN(p_veqz_16x8);
            DECLARE_PATTERN(p_veqz_32x4);
            DECLARE_PATTERN(p_vcge_8sx16);
            DECLARE_PATTERN(p_vcge_16sx8);
            DECLARE_PATTERN(p_vcge_32sx4);
            DECLARE_PATTERN(p_vcge_8ux16);
            DECLARE_PATTERN(p_vcge_16ux8);
            DECLARE_PATTERN(p_vcge_32ux4);
            DEFINE_PATTERN(p_veqz_8x16,
                  unop(Iop_NotV128, unop(Iop_CmpNEZ8x16, bind(0))));
            DEFINE_PATTERN(p_veqz_16x8,
                  unop(Iop_NotV128, unop(Iop_CmpNEZ16x8, bind(0))));
            DEFINE_PATTERN(p_veqz_32x4,
                  unop(Iop_NotV128, unop(Iop_CmpNEZ32x4, bind(0))));
            DEFINE_PATTERN(p_vcge_8sx16,
                  unop(Iop_NotV128, binop(Iop_CmpGT8Sx16, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_16sx8,
                  unop(Iop_NotV128, binop(Iop_CmpGT16Sx8, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_32sx4,
                  unop(Iop_NotV128, binop(Iop_CmpGT32Sx4, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_8ux16,
                  unop(Iop_NotV128, binop(Iop_CmpGT8Ux16, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_16ux8,
                  unop(Iop_NotV128, binop(Iop_CmpGT16Ux8, bind(1), bind(0))));
            DEFINE_PATTERN(p_vcge_32ux4,
                  unop(Iop_NotV128, binop(Iop_CmpGT32Ux4, bind(1), bind(0))));
            if (matchIRExpr(&mi, p_veqz_8x16, e)) {
               HReg res = newVRegV(env);
               HReg arg = iselNeonExpr(env, mi.bindee[0]);
               addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, res, arg, 0, True));
               return res;
            } else if (matchIRExpr(&mi, p_veqz_16x8, e)) {
               HReg res = newVRegV(env);
               HReg arg = iselNeonExpr(env, mi.bindee[0]);
               addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, res, arg, 1, True));
               return res;
            } else if (matchIRExpr(&mi, p_veqz_32x4, e)) {
               HReg res = newVRegV(env);
               HReg arg = iselNeonExpr(env, mi.bindee[0]);
               addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, res, arg, 2, True));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_8sx16, e)) {
               HReg res = newVRegV(env);
               HReg argL = iselNeonExpr(env, mi.bindee[0]);
               HReg argR = iselNeonExpr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGES,
                                              res, argL, argR, 0, True));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_16sx8, e)) {
               HReg res = newVRegV(env);
               HReg argL = iselNeonExpr(env, mi.bindee[0]);
               HReg argR = iselNeonExpr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGES,
                                              res, argL, argR, 1, True));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_32sx4, e)) {
               HReg res = newVRegV(env);
               HReg argL = iselNeonExpr(env, mi.bindee[0]);
               HReg argR = iselNeonExpr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGES,
                                              res, argL, argR, 2, True));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_8ux16, e)) {
               HReg res = newVRegV(env);
               HReg argL = iselNeonExpr(env, mi.bindee[0]);
               HReg argR = iselNeonExpr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGEU,
                                              res, argL, argR, 0, True));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_16ux8, e)) {
               HReg res = newVRegV(env);
               HReg argL = iselNeonExpr(env, mi.bindee[0]);
               HReg argR = iselNeonExpr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGEU,
                                              res, argL, argR, 1, True));
               return res;
            } else if (matchIRExpr(&mi, p_vcge_32ux4, e)) {
               HReg res = newVRegV(env);
               HReg argL = iselNeonExpr(env, mi.bindee[0]);
               HReg argR = iselNeonExpr(env, mi.bindee[1]);
               addInstr(env, ARMInstr_NBinary(ARMneon_VCGEU,
                                              res, argL, argR, 2, True));
               return res;
            } else {
               HReg res = newVRegV(env);
               HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
               addInstr(env, ARMInstr_NUnary(ARMneon_NOT, res, arg, 4, True));
               return res;
            }
         }
         case Iop_Dup8x16:
         case Iop_Dup16x8:
         case Iop_Dup32x4: {
            HReg res, arg;
            UInt size;
            DECLARE_PATTERN(p_vdup_8x16);
            DECLARE_PATTERN(p_vdup_16x8);
            DECLARE_PATTERN(p_vdup_32x4);
            DEFINE_PATTERN(p_vdup_8x16,
                  unop(Iop_Dup8x16, binop(Iop_GetElem8x8, bind(0), bind(1))));
            DEFINE_PATTERN(p_vdup_16x8,
                  unop(Iop_Dup16x8, binop(Iop_GetElem16x4, bind(0), bind(1))));
            DEFINE_PATTERN(p_vdup_32x4,
                  unop(Iop_Dup32x4, binop(Iop_GetElem32x2, bind(0), bind(1))));
            if (matchIRExpr(&mi, p_vdup_8x16, e)) {
               UInt index;
               UInt imm4;
               if (mi.bindee[1]->tag == Iex_Const &&
                  typeOfIRExpr(env->type_env, mi.bindee[1]) == Ity_I8) {
                  index = mi.bindee[1]->Iex.Const.con->Ico.U8;
                  imm4 = (index << 1) + 1;
                  if (index < 8) {
                     res = newVRegV(env);
                     arg = iselNeon64Expr(env, mi.bindee[0]);
                     addInstr(env, ARMInstr_NUnaryS(
                                      ARMneon_VDUP,
                                      mkARMNRS(ARMNRS_Reg, res, 0),
                                      mkARMNRS(ARMNRS_Scalar, arg, index),
                                      imm4, True
                             ));
                     return res;
                  }
               }
            } else if (matchIRExpr(&mi, p_vdup_16x8, e)) {
               UInt index;
               UInt imm4;
               if (mi.bindee[1]->tag == Iex_Const &&
                  typeOfIRExpr(env->type_env, mi.bindee[1]) == Ity_I8) {
                  index = mi.bindee[1]->Iex.Const.con->Ico.U8;
                  imm4 = (index << 2) + 2;
                  if (index < 4) {
                     res = newVRegV(env);
                     arg = iselNeon64Expr(env, mi.bindee[0]);
                     addInstr(env, ARMInstr_NUnaryS(
                                      ARMneon_VDUP,
                                      mkARMNRS(ARMNRS_Reg, res, 0),
                                      mkARMNRS(ARMNRS_Scalar, arg, index),
                                      imm4, True
                             ));
                     return res;
                  }
               }
            } else if (matchIRExpr(&mi, p_vdup_32x4, e)) {
               UInt index;
               UInt imm4;
               if (mi.bindee[1]->tag == Iex_Const &&
                  typeOfIRExpr(env->type_env, mi.bindee[1]) == Ity_I8) {
                  index = mi.bindee[1]->Iex.Const.con->Ico.U8;
                  imm4 = (index << 3) + 4;
                  if (index < 2) {
                     res = newVRegV(env);
                     arg = iselNeon64Expr(env, mi.bindee[0]);
                     addInstr(env, ARMInstr_NUnaryS(
                                      ARMneon_VDUP,
                                      mkARMNRS(ARMNRS_Reg, res, 0),
                                      mkARMNRS(ARMNRS_Scalar, arg, index),
                                      imm4, True
                             ));
                     return res;
                  }
               }
            }
            arg = iselIntExpr_R(env, e->Iex.Unop.arg);
            res = newVRegV(env);
            switch (e->Iex.Unop.op) {
               case Iop_Dup8x16: size = 0; break;
               case Iop_Dup16x8: size = 1; break;
               case Iop_Dup32x4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_DUP, res, arg, size, True));
            return res;
         }
         case Iop_Abs8x16:
         case Iop_Abs16x8:
         case Iop_Abs32x4: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Abs8x16: size = 0; break;
               case Iop_Abs16x8: size = 1; break;
               case Iop_Abs32x4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_ABS, res, arg, size, True));
            return res;
         }
         case Iop_Reverse8sIn64_x2:
         case Iop_Reverse16sIn64_x2:
         case Iop_Reverse32sIn64_x2: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Reverse8sIn64_x2: size = 0; break;
               case Iop_Reverse16sIn64_x2: size = 1; break;
               case Iop_Reverse32sIn64_x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_REV64,
                                          res, arg, size, True));
            return res;
         }
         case Iop_Reverse8sIn32_x4:
         case Iop_Reverse16sIn32_x4: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Reverse8sIn32_x4: size = 0; break;
               case Iop_Reverse16sIn32_x4: size = 1; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_REV32,
                                          res, arg, size, True));
            return res;
         }
         case Iop_Reverse8sIn16_x8: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            addInstr(env, ARMInstr_NUnary(ARMneon_REV16,
                                          res, arg, size, True));
            return res;
         }
         case Iop_CmpNEZ64x2: {
            HReg x_lsh = newVRegV(env);
            HReg x_rsh = newVRegV(env);
            HReg lsh_amt = newVRegV(env);
            HReg rsh_amt = newVRegV(env);
            HReg zero = newVRegV(env);
            HReg tmp = newVRegV(env);
            HReg tmp2 = newVRegV(env);
            HReg res = newVRegV(env);
            HReg x = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, tmp2, arg, 2, True));
            addInstr(env, ARMInstr_NUnary(ARMneon_NOT, x, tmp2, 4, True));
            addInstr(env, ARMInstr_NeonImm(lsh_amt, ARMNImm_TI(0, 32)));
            addInstr(env, ARMInstr_NeonImm(zero, ARMNImm_TI(0, 0)));
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUB,
                                           rsh_amt, zero, lsh_amt, 2, True));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          x_lsh, x, lsh_amt, 3, True));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          x_rsh, x, rsh_amt, 3, True));
            addInstr(env, ARMInstr_NBinary(ARMneon_VORR,
                                           tmp, x_lsh, x_rsh, 0, True));
            addInstr(env, ARMInstr_NBinary(ARMneon_VORR,
                                           res, tmp, x, 0, True));
            return res;
         }
         case Iop_CmpNEZ8x16:
         case Iop_CmpNEZ16x8:
         case Iop_CmpNEZ32x4: {
            HReg res = newVRegV(env);
            HReg tmp = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size;
            switch (e->Iex.Unop.op) {
               case Iop_CmpNEZ8x16: size = 0; break;
               case Iop_CmpNEZ16x8: size = 1; break;
               case Iop_CmpNEZ32x4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_EQZ, tmp, arg, size, True));
            addInstr(env, ARMInstr_NUnary(ARMneon_NOT, res, tmp, 4, True));
            return res;
         }
         case Iop_Widen8Uto16x8:
         case Iop_Widen16Uto32x4:
         case Iop_Widen32Uto64x2: {
            HReg res = newVRegV(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size;
            switch (e->Iex.Unop.op) {
               case Iop_Widen8Uto16x8:  size = 0; break;
               case Iop_Widen16Uto32x4: size = 1; break;
               case Iop_Widen32Uto64x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPYLU,
                                          res, arg, size, True));
            return res;
         }
         case Iop_Widen8Sto16x8:
         case Iop_Widen16Sto32x4:
         case Iop_Widen32Sto64x2: {
            HReg res = newVRegV(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            UInt size;
            switch (e->Iex.Unop.op) {
               case Iop_Widen8Sto16x8:  size = 0; break;
               case Iop_Widen16Sto32x4: size = 1; break;
               case Iop_Widen32Sto64x2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPYLS,
                                          res, arg, size, True));
            return res;
         }
         case Iop_PwAddL8Sx16:
         case Iop_PwAddL16Sx8:
         case Iop_PwAddL32Sx4: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwAddL8Sx16: size = 0; break;
               case Iop_PwAddL16Sx8: size = 1; break;
               case Iop_PwAddL32Sx4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_PADDLS,
                                          res, arg, size, True));
            return res;
         }
         case Iop_PwAddL8Ux16:
         case Iop_PwAddL16Ux8:
         case Iop_PwAddL32Ux4: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwAddL8Ux16: size = 0; break;
               case Iop_PwAddL16Ux8: size = 1; break;
               case Iop_PwAddL32Ux4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_PADDLU,
                                          res, arg, size, True));
            return res;
         }
         case Iop_Cnt8x16: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            addInstr(env, ARMInstr_NUnary(ARMneon_CNT, res, arg, size, True));
            return res;
         }
         case Iop_Clz8x16:
         case Iop_Clz16x8:
         case Iop_Clz32x4: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Clz8x16: size = 0; break;
               case Iop_Clz16x8: size = 1; break;
               case Iop_Clz32x4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_CLZ, res, arg, size, True));
            return res;
         }
         case Iop_Cls8x16:
         case Iop_Cls16x8:
         case Iop_Cls32x4: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Cls8x16: size = 0; break;
               case Iop_Cls16x8: size = 1; break;
               case Iop_Cls32x4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_CLS, res, arg, size, True));
            return res;
         }
         case Iop_F32toI32Sx4_RZ: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTFtoS,
                                          res, arg, 2, True));
            return res;
         }
         case Iop_F32toI32Ux4_RZ: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTFtoU,
                                          res, arg, 2, True));
            return res;
         }
         case Iop_I32StoF32x4_DEP: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTStoF,
                                          res, arg, 2, True));
            return res;
         }
         case Iop_I32UtoF32x4_DEP: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTUtoF,
                                          res, arg, 2, True));
            return res;
         }
         case Iop_F16toF32x4: {
            HReg res = newVRegV(env);
            HReg arg = iselNeon64Expr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VCVTF16toF32,
                                          res, arg, 2, True));
            return res;
         }
         case Iop_RecipEst32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VRECIPF,
                                          res, argL, 0, True));
            return res;
         }
         case Iop_RecipEst32Ux4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VRECIP,
                                          res, argL, 0, True));
            return res;
         }
         case Iop_Abs32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VABSFP,
                                          res, argL, 0, True));
            return res;
         }
         case Iop_RSqrtEst32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VRSQRTEFP,
                                          res, argL, 0, True));
            return res;
         }
         case Iop_RSqrtEst32Ux4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VRSQRTE,
                                          res, argL, 0, True));
            return res;
         }
         case Iop_Neg32Fx4: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_NUnary(ARMneon_VNEGF,
                                          res, arg, 0, True));
            return res;
         }
         /* ... */
         default:
            break;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_64HLtoV128: {
            /* Try to match into single "VMOV reg, imm" instruction */
            if (e->Iex.Binop.arg1->tag == Iex_Const &&
                e->Iex.Binop.arg2->tag == Iex_Const &&
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg1) == Ity_I64 &&
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) == Ity_I64 &&
                e->Iex.Binop.arg1->Iex.Const.con->Ico.U64 ==
                           e->Iex.Binop.arg2->Iex.Const.con->Ico.U64) {
               ULong imm64 = e->Iex.Binop.arg2->Iex.Const.con->Ico.U64;
               ARMNImm *imm = Imm64_to_ARMNImm(imm64);
               if (imm) {
                  HReg res = newVRegV(env);
                  addInstr(env, ARMInstr_NeonImm(res, imm));
                  return res;
               }
               if ((imm64 >> 32) == 0LL &&
                   (imm = Imm64_to_ARMNImm(imm64 | (imm64 << 32))) != NULL) {
                  HReg tmp1 = newVRegV(env);
                  HReg tmp2 = newVRegV(env);
                  HReg res = newVRegV(env);
                  if (imm->type < 10) {
                     addInstr(env, ARMInstr_NeonImm(tmp1, ARMNImm_TI(9,0x0f)));
                     addInstr(env, ARMInstr_NeonImm(tmp2, imm));
                     addInstr(env, ARMInstr_NBinary(ARMneon_VAND,
                                                    res, tmp1, tmp2, 4, True));
                     return res;
                  }
               }
               if ((imm64 & 0xFFFFFFFFLL) == 0LL &&
                   (imm = Imm64_to_ARMNImm(imm64 | (imm64 >> 32))) != NULL) {
                  HReg tmp1 = newVRegV(env);
                  HReg tmp2 = newVRegV(env);
                  HReg res = newVRegV(env);
                  if (imm->type < 10) {
                     addInstr(env, ARMInstr_NeonImm(tmp1, ARMNImm_TI(9,0xf0)));
                     addInstr(env, ARMInstr_NeonImm(tmp2, imm));
                     addInstr(env, ARMInstr_NBinary(ARMneon_VAND,
                                                    res, tmp1, tmp2, 4, True));
                     return res;
                  }
               }
            }
            /* Does not match "VMOV Reg, Imm" form.  We'll have to do
               it the slow way. */
            HReg dHi = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg dLo = iselNeon64Expr(env, e->Iex.Binop.arg2);
            HReg res = newVRegV(env);
            addInstr(env, ARMInstr_VXferQ(True/*toQ*/, res, dHi, dLo));
            return res;
         }
         case Iop_AndV128: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VAND,
                                           res, argL, argR, 4, True));
            return res;
         }
         case Iop_OrV128: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VORR,
                                           res, argL, argR, 4, True));
            return res;
         }
         case Iop_XorV128: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VXOR,
                                           res, argL, argR, 4, True));
            return res;
         }
         case Iop_Add8x16:
         case Iop_Add16x8:
         case Iop_Add32x4:
         case Iop_Add64x2: {
            /* 
            FIXME: remove this if not used
            DECLARE_PATTERN(p_vrhadd_32sx4);
            ULong one = (1LL << 32) | 1LL;
            DEFINE_PATTERN(p_vrhadd_32sx4,
                  binop(Iop_Add32x4,
                        binop(Iop_Add32x4,
                              binop(Iop_SarN32x4,
                                    bind(0),
                                    mkU8(1)),
                              binop(Iop_SarN32x4,
                                    bind(1),
                                    mkU8(1))),
                        binop(Iop_SarN32x4,
                              binop(Iop_Add32x4,
                                    binop(Iop_Add32x4,
                                          binop(Iop_AndV128,
                                                bind(0),
                                                mkU128(one)),
                                          binop(Iop_AndV128,
                                                bind(1),
                                                mkU128(one))),
                                    mkU128(one)),
                              mkU8(1))));
            */
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Add8x16: size = 0; break;
               case Iop_Add16x8: size = 1; break;
               case Iop_Add32x4: size = 2; break;
               case Iop_Add64x2: size = 3; break;
               default:
                  ppIROp(e->Iex.Binop.op);
                  vpanic("Illegal element size in VADD");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VADD,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_RecipStep32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VRECPS,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_RSqrtStep32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VRSQRTS,
                                           res, argL, argR, size, True));
            return res;
         }

         // These 6 verified 18 Apr 2013
         case Iop_InterleaveEvenLanes8x16:
         case Iop_InterleaveOddLanes8x16:
         case Iop_InterleaveEvenLanes16x8:
         case Iop_InterleaveOddLanes16x8:
         case Iop_InterleaveEvenLanes32x4:
         case Iop_InterleaveOddLanes32x4: {
            HReg rD   = newVRegV(env);
            HReg rM   = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            Bool resRd;  // is the result in rD or rM ?
            switch (e->Iex.Binop.op) {
               case Iop_InterleaveOddLanes8x16:  resRd = False; size = 0; break;
               case Iop_InterleaveEvenLanes8x16: resRd = True;  size = 0; break;
               case Iop_InterleaveOddLanes16x8:  resRd = False; size = 1; break;
               case Iop_InterleaveEvenLanes16x8: resRd = True;  size = 1; break;
               case Iop_InterleaveOddLanes32x4:  resRd = False; size = 2; break;
               case Iop_InterleaveEvenLanes32x4: resRd = True;  size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rM, argL, 4, True));
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rD, argR, 4, True));
            addInstr(env, ARMInstr_NDual(ARMneon_TRN, rD, rM, size, True));
            return resRd ? rD : rM;
         }

         // These 6 verified 18 Apr 2013
         case Iop_InterleaveHI8x16:
         case Iop_InterleaveLO8x16:
         case Iop_InterleaveHI16x8:
         case Iop_InterleaveLO16x8:
         case Iop_InterleaveHI32x4:
         case Iop_InterleaveLO32x4: {
            HReg rD   = newVRegV(env);
            HReg rM   = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            Bool resRd;  // is the result in rD or rM ?
            switch (e->Iex.Binop.op) {
               case Iop_InterleaveHI8x16: resRd = False; size = 0; break;
               case Iop_InterleaveLO8x16: resRd = True;  size = 0; break;
               case Iop_InterleaveHI16x8: resRd = False; size = 1; break;
               case Iop_InterleaveLO16x8: resRd = True;  size = 1; break;
               case Iop_InterleaveHI32x4: resRd = False; size = 2; break;
               case Iop_InterleaveLO32x4: resRd = True;  size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rM, argL, 4, True));
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rD, argR, 4, True));
            addInstr(env, ARMInstr_NDual(ARMneon_ZIP, rD, rM, size, True));
            return resRd ? rD : rM;
         }

         // These 6 verified 18 Apr 2013
         case Iop_CatOddLanes8x16:
         case Iop_CatEvenLanes8x16:
         case Iop_CatOddLanes16x8:
         case Iop_CatEvenLanes16x8:
         case Iop_CatOddLanes32x4:
         case Iop_CatEvenLanes32x4: {
            HReg rD   = newVRegV(env);
            HReg rM   = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            Bool resRd;  // is the result in rD or rM ?
            switch (e->Iex.Binop.op) {
               case Iop_CatOddLanes8x16:  resRd = False; size = 0; break;
               case Iop_CatEvenLanes8x16: resRd = True;  size = 0; break;
               case Iop_CatOddLanes16x8:  resRd = False; size = 1; break;
               case Iop_CatEvenLanes16x8: resRd = True;  size = 1; break;
               case Iop_CatOddLanes32x4:  resRd = False; size = 2; break;
               case Iop_CatEvenLanes32x4: resRd = True;  size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rM, argL, 4, True));
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, rD, argR, 4, True));
            addInstr(env, ARMInstr_NDual(ARMneon_UZP, rD, rM, size, True));
            return resRd ? rD : rM;
         }

         case Iop_QAdd8Ux16:
         case Iop_QAdd16Ux8:
         case Iop_QAdd32Ux4:
         case Iop_QAdd64Ux2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QAdd8Ux16: size = 0; break;
               case Iop_QAdd16Ux8: size = 1; break;
               case Iop_QAdd32Ux4: size = 2; break;
               case Iop_QAdd64Ux2: size = 3; break;
               default:
                  ppIROp(e->Iex.Binop.op);
                  vpanic("Illegal element size in VQADDU");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQADDU,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_QAdd8Sx16:
         case Iop_QAdd16Sx8:
         case Iop_QAdd32Sx4:
         case Iop_QAdd64Sx2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QAdd8Sx16: size = 0; break;
               case Iop_QAdd16Sx8: size = 1; break;
               case Iop_QAdd32Sx4: size = 2; break;
               case Iop_QAdd64Sx2: size = 3; break;
               default:
                  ppIROp(e->Iex.Binop.op);
                  vpanic("Illegal element size in VQADDS");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQADDS,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Sub8x16:
         case Iop_Sub16x8:
         case Iop_Sub32x4:
         case Iop_Sub64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Sub8x16: size = 0; break;
               case Iop_Sub16x8: size = 1; break;
               case Iop_Sub32x4: size = 2; break;
               case Iop_Sub64x2: size = 3; break;
               default:
                  ppIROp(e->Iex.Binop.op);
                  vpanic("Illegal element size in VSUB");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUB,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_QSub8Ux16:
         case Iop_QSub16Ux8:
         case Iop_QSub32Ux4:
         case Iop_QSub64Ux2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QSub8Ux16: size = 0; break;
               case Iop_QSub16Ux8: size = 1; break;
               case Iop_QSub32Ux4: size = 2; break;
               case Iop_QSub64Ux2: size = 3; break;
               default:
                  ppIROp(e->Iex.Binop.op);
                  vpanic("Illegal element size in VQSUBU");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQSUBU,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_QSub8Sx16:
         case Iop_QSub16Sx8:
         case Iop_QSub32Sx4:
         case Iop_QSub64Sx2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QSub8Sx16: size = 0; break;
               case Iop_QSub16Sx8: size = 1; break;
               case Iop_QSub32Sx4: size = 2; break;
               case Iop_QSub64Sx2: size = 3; break;
               default:
                  ppIROp(e->Iex.Binop.op);
                  vpanic("Illegal element size in VQSUBS");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQSUBS,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Max8Ux16:
         case Iop_Max16Ux8:
         case Iop_Max32Ux4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Max8Ux16: size = 0; break;
               case Iop_Max16Ux8: size = 1; break;
               case Iop_Max32Ux4: size = 2; break;
               default: vpanic("Illegal element size in VMAXU");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMAXU,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Max8Sx16:
         case Iop_Max16Sx8:
         case Iop_Max32Sx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Max8Sx16: size = 0; break;
               case Iop_Max16Sx8: size = 1; break;
               case Iop_Max32Sx4: size = 2; break;
               default: vpanic("Illegal element size in VMAXU");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMAXS,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Min8Ux16:
         case Iop_Min16Ux8:
         case Iop_Min32Ux4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Min8Ux16: size = 0; break;
               case Iop_Min16Ux8: size = 1; break;
               case Iop_Min32Ux4: size = 2; break;
               default: vpanic("Illegal element size in VMAXU");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMINU,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Min8Sx16:
         case Iop_Min16Sx8:
         case Iop_Min32Sx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Min8Sx16: size = 0; break;
               case Iop_Min16Sx8: size = 1; break;
               case Iop_Min32Sx4: size = 2; break;
               default: vpanic("Illegal element size in VMAXU");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMINS,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Sar8x16:
         case Iop_Sar16x8:
         case Iop_Sar32x4:
         case Iop_Sar64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            HReg argR2 = newVRegV(env);
            HReg zero = newVRegV(env);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Sar8x16: size = 0; break;
               case Iop_Sar16x8: size = 1; break;
               case Iop_Sar32x4: size = 2; break;
               case Iop_Sar64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NeonImm(zero, ARMNImm_TI(0,0)));
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUB,
                                           argR2, zero, argR, size, True));
            addInstr(env, ARMInstr_NShift(ARMneon_VSAL,
                                          res, argL, argR2, size, True));
            return res;
         }
         case Iop_Sal8x16:
         case Iop_Sal16x8:
         case Iop_Sal32x4:
         case Iop_Sal64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Sal8x16: size = 0; break;
               case Iop_Sal16x8: size = 1; break;
               case Iop_Sal32x4: size = 2; break;
               case Iop_Sal64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NShift(ARMneon_VSAL,
                                          res, argL, argR, size, True));
            return res;
         }
         case Iop_Shr8x16:
         case Iop_Shr16x8:
         case Iop_Shr32x4:
         case Iop_Shr64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            HReg argR2 = newVRegV(env);
            HReg zero = newVRegV(env);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Shr8x16: size = 0; break;
               case Iop_Shr16x8: size = 1; break;
               case Iop_Shr32x4: size = 2; break;
               case Iop_Shr64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NeonImm(zero, ARMNImm_TI(0,0)));
            addInstr(env, ARMInstr_NBinary(ARMneon_VSUB,
                                           argR2, zero, argR, size, True));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          res, argL, argR2, size, True));
            return res;
         }
         case Iop_Shl8x16:
         case Iop_Shl16x8:
         case Iop_Shl32x4:
         case Iop_Shl64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_Shl8x16: size = 0; break;
               case Iop_Shl16x8: size = 1; break;
               case Iop_Shl32x4: size = 2; break;
               case Iop_Shl64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          res, argL, argR, size, True));
            return res;
         }
         case Iop_QShl8x16:
         case Iop_QShl16x8:
         case Iop_QShl32x4:
         case Iop_QShl64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QShl8x16: size = 0; break;
               case Iop_QShl16x8: size = 1; break;
               case Iop_QShl32x4: size = 2; break;
               case Iop_QShl64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NShift(ARMneon_VQSHL,
                                          res, argL, argR, size, True));
            return res;
         }
         case Iop_QSal8x16:
         case Iop_QSal16x8:
         case Iop_QSal32x4:
         case Iop_QSal64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_QSal8x16: size = 0; break;
               case Iop_QSal16x8: size = 1; break;
               case Iop_QSal32x4: size = 2; break;
               case Iop_QSal64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NShift(ARMneon_VQSAL,
                                          res, argL, argR, size, True));
            return res;
         }
         case Iop_QShlNsatUU8x16:
         case Iop_QShlNsatUU16x8:
         case Iop_QShlNsatUU32x4:
         case Iop_QShlNsatUU64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            UInt size, imm;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
               vpanic("ARM target supports Iop_QShlNsatUUAxB with constant "
                      "second argument only\n");
            }
            imm = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch (e->Iex.Binop.op) {
               case Iop_QShlNsatUU8x16: size = 8 | imm; break;
               case Iop_QShlNsatUU16x8: size = 16 | imm; break;
               case Iop_QShlNsatUU32x4: size = 32 | imm; break;
               case Iop_QShlNsatUU64x2: size = 64 | imm; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_VQSHLNUU,
                                          res, argL, size, True));
            return res;
         }
         case Iop_QShlNsatSU8x16:
         case Iop_QShlNsatSU16x8:
         case Iop_QShlNsatSU32x4:
         case Iop_QShlNsatSU64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            UInt size, imm;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
               vpanic("ARM target supports Iop_QShlNsatSUAxB with constant "
                      "second argument only\n");
            }
            imm = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch (e->Iex.Binop.op) {
               case Iop_QShlNsatSU8x16: size = 8 | imm; break;
               case Iop_QShlNsatSU16x8: size = 16 | imm; break;
               case Iop_QShlNsatSU32x4: size = 32 | imm; break;
               case Iop_QShlNsatSU64x2: size = 64 | imm; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_VQSHLNUS,
                                          res, argL, size, True));
            return res;
         }
         case Iop_QShlNsatSS8x16:
         case Iop_QShlNsatSS16x8:
         case Iop_QShlNsatSS32x4:
         case Iop_QShlNsatSS64x2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            UInt size, imm;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
               vpanic("ARM target supports Iop_QShlNsatSSAxB with constant "
                      "second argument only\n");
            }
            imm = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch (e->Iex.Binop.op) {
               case Iop_QShlNsatSS8x16: size = 8 | imm; break;
               case Iop_QShlNsatSS16x8: size = 16 | imm; break;
               case Iop_QShlNsatSS32x4: size = 32 | imm; break;
               case Iop_QShlNsatSS64x2: size = 64 | imm; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_VQSHLNSS,
                                          res, argL, size, True));
            return res;
         }
         case Iop_ShrN8x16:
         case Iop_ShrN16x8:
         case Iop_ShrN32x4:
         case Iop_ShrN64x2: {
            HReg res = newVRegV(env);
            HReg tmp = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg argR2 = newVRegI(env);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_ShrN8x16: size = 0; break;
               case Iop_ShrN16x8: size = 1; break;
               case Iop_ShrN32x4: size = 2; break;
               case Iop_ShrN64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_Unary(ARMun_NEG, argR2, argR));
            addInstr(env, ARMInstr_NUnary(ARMneon_DUP,
                                          tmp, argR2, 0, True));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          res, argL, tmp, size, True));
            return res;
         }
         case Iop_ShlN8x16:
         case Iop_ShlN16x8:
         case Iop_ShlN32x4:
         case Iop_ShlN64x2: {
            HReg res = newVRegV(env);
            HReg tmp = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_ShlN8x16: size = 0; break;
               case Iop_ShlN16x8: size = 1; break;
               case Iop_ShlN32x4: size = 2; break;
               case Iop_ShlN64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_DUP, tmp, argR, 0, True));
            addInstr(env, ARMInstr_NShift(ARMneon_VSHL,
                                          res, argL, tmp, size, True));
            return res;
         }
         case Iop_SarN8x16:
         case Iop_SarN16x8:
         case Iop_SarN32x4:
         case Iop_SarN64x2: {
            HReg res = newVRegV(env);
            HReg tmp = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg argR2 = newVRegI(env);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_SarN8x16: size = 0; break;
               case Iop_SarN16x8: size = 1; break;
               case Iop_SarN32x4: size = 2; break;
               case Iop_SarN64x2: size = 3; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_Unary(ARMun_NEG, argR2, argR));
            addInstr(env, ARMInstr_NUnary(ARMneon_DUP, tmp, argR2, 0, True));
            addInstr(env, ARMInstr_NShift(ARMneon_VSAL,
                                          res, argL, tmp, size, True));
            return res;
         }
         case Iop_CmpGT8Ux16:
         case Iop_CmpGT16Ux8:
         case Iop_CmpGT32Ux4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_CmpGT8Ux16: size = 0; break;
               case Iop_CmpGT16Ux8: size = 1; break;
               case Iop_CmpGT32Ux4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VCGTU,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_CmpGT8Sx16:
         case Iop_CmpGT16Sx8:
         case Iop_CmpGT32Sx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_CmpGT8Sx16: size = 0; break;
               case Iop_CmpGT16Sx8: size = 1; break;
               case Iop_CmpGT32Sx4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VCGTS,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_CmpEQ8x16:
         case Iop_CmpEQ16x8:
         case Iop_CmpEQ32x4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size;
            switch (e->Iex.Binop.op) {
               case Iop_CmpEQ8x16: size = 0; break;
               case Iop_CmpEQ16x8: size = 1; break;
               case Iop_CmpEQ32x4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VCEQ,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Mul8x16:
         case Iop_Mul16x8:
         case Iop_Mul32x4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Mul8x16: size = 0; break;
               case Iop_Mul16x8: size = 1; break;
               case Iop_Mul32x4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMUL,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Mull8Ux8:
         case Iop_Mull16Ux4:
         case Iop_Mull32Ux2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Mull8Ux8: size = 0; break;
               case Iop_Mull16Ux4: size = 1; break;
               case Iop_Mull32Ux2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMULLU,
                                           res, argL, argR, size, True));
            return res;
         }

         case Iop_Mull8Sx8:
         case Iop_Mull16Sx4:
         case Iop_Mull32Sx2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_Mull8Sx8: size = 0; break;
               case Iop_Mull16Sx4: size = 1; break;
               case Iop_Mull32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VMULLS,
                                           res, argL, argR, size, True));
            return res;
         }

         case Iop_QDMulHi16Sx8:
         case Iop_QDMulHi32Sx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_QDMulHi16Sx8: size = 1; break;
               case Iop_QDMulHi32Sx4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQDMULH,
                                           res, argL, argR, size, True));
            return res;
         }

         case Iop_QRDMulHi16Sx8:
         case Iop_QRDMulHi32Sx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_QRDMulHi16Sx8: size = 1; break;
               case Iop_QRDMulHi32Sx4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQRDMULH,
                                           res, argL, argR, size, True));
            return res;
         }

         case Iop_QDMull16Sx4:
         case Iop_QDMull32Sx2: {
            HReg res = newVRegV(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_QDMull16Sx4: size = 1; break;
               case Iop_QDMull32Sx2: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VQDMULL,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_PolynomialMul8x16: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VMULP,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_Max32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VMAXF,
                                           res, argL, argR, 2, True));
            return res;
         }
         case Iop_Min32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VMINF,
                                           res, argL, argR, 2, True));
            return res;
         }
         case Iop_PwMax32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VPMAXF,
                                           res, argL, argR, 2, True));
            return res;
         }
         case Iop_PwMin32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VPMINF,
                                           res, argL, argR, 2, True));
            return res;
         }
         case Iop_CmpGT32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VCGTF,
                                           res, argL, argR, 2, True));
            return res;
         }
         case Iop_CmpGE32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VCGEF,
                                           res, argL, argR, 2, True));
            return res;
         }
         case Iop_CmpEQ32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            addInstr(env, ARMInstr_NBinary(ARMneon_VCEQF,
                                           res, argL, argR, 2, True));
            return res;
         }

         case Iop_PolynomialMull8x8: {
            HReg res = newVRegV(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeon64Expr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            addInstr(env, ARMInstr_NBinary(ARMneon_VMULLP,
                                           res, argL, argR, size, True));
            return res;
         }
         case Iop_F32ToFixed32Ux4_RZ:
         case Iop_F32ToFixed32Sx4_RZ:
         case Iop_Fixed32UToF32x4_RN:
         case Iop_Fixed32SToF32x4_RN: {
            HReg res = newVRegV(env);
            HReg arg = iselNeonExpr(env, e->Iex.Binop.arg1);
            ARMNeonUnOp op;
            UInt imm6;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
               typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
                  vpanic("ARM supports FP <-> Fixed conversion with constant "
                         "second argument less than 33 only\n");
            }
            imm6 = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            vassert(imm6 <= 32 && imm6 > 0);
            imm6 = 64 - imm6;
            switch(e->Iex.Binop.op) {
               case Iop_F32ToFixed32Ux4_RZ: op = ARMneon_VCVTFtoFixedU; break;
               case Iop_F32ToFixed32Sx4_RZ: op = ARMneon_VCVTFtoFixedS; break;
               case Iop_Fixed32UToF32x4_RN: op = ARMneon_VCVTFixedUtoF; break;
               case Iop_Fixed32SToF32x4_RN: op = ARMneon_VCVTFixedStoF; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NUnary(op, res, arg, imm6, True));
            return res;
         }
         /*
         FIXME remove if not used
         case Iop_VDup8x16:
         case Iop_VDup16x8:
         case Iop_VDup32x4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeon64Expr(env, e->Iex.Binop.arg1);
            UInt imm4;
            UInt index;
            if (e->Iex.Binop.arg2->tag != Iex_Const ||
               typeOfIRExpr(env->type_env, e->Iex.Binop.arg2) != Ity_I8) {
                  vpanic("ARM supports Iop_VDup with constant "
                         "second argument less than 16 only\n");
            }
            index = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            switch(e->Iex.Binop.op) {
               case Iop_VDup8x16: imm4 = (index << 1) + 1; break;
               case Iop_VDup16x8: imm4 = (index << 2) + 2; break;
               case Iop_VDup32x4: imm4 = (index << 3) + 4; break;
               default: vassert(0);
            }
            if (imm4 >= 16) {
               vpanic("ARM supports Iop_VDup with constant "
                      "second argument less than 16 only\n");
            }
            addInstr(env, ARMInstr_NUnary(ARMneon_VDUP,
                                          res, argL, imm4, True));
            return res;
         }
         */
         case Iop_PwAdd8x16:
         case Iop_PwAdd16x8:
         case Iop_PwAdd32x4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, e->Iex.Binop.arg1);
            HReg argR = iselNeonExpr(env, e->Iex.Binop.arg2);
            UInt size = 0;
            switch(e->Iex.Binop.op) {
               case Iop_PwAdd8x16: size = 0; break;
               case Iop_PwAdd16x8: size = 1; break;
               case Iop_PwAdd32x4: size = 2; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VPADD,
                                           res, argL, argR, size, True));
            return res;
         }
         /* ... */
         default:
            break;
      }
   }

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;

      switch (triop->op) {
         case Iop_SliceV128: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, triop->arg2);
            HReg argR = iselNeonExpr(env, triop->arg1);
            UInt imm4;
            if (triop->arg3->tag != Iex_Const ||
                typeOfIRExpr(env->type_env, triop->arg3) != Ity_I8) {
               vpanic("ARM target supports Iop_ExtractV128 with constant "
                      "third argument less than 16 only\n");
            }
            imm4 = triop->arg3->Iex.Const.con->Ico.U8;
            if (imm4 >= 16) {
               vpanic("ARM target supports Iop_ExtractV128 with constant "
                      "third argument less than 16 only\n");
            }
            addInstr(env, ARMInstr_NBinary(ARMneon_VEXT,
                                           res, argL, argR, imm4, True));
            return res;
         }
         case Iop_Mul32Fx4:
         case Iop_Sub32Fx4:
         case Iop_Add32Fx4: {
            HReg res = newVRegV(env);
            HReg argL = iselNeonExpr(env, triop->arg2);
            HReg argR = iselNeonExpr(env, triop->arg3);
            UInt size = 0;
            ARMNeonBinOp op = ARMneon_INVALID;
            switch (triop->op) {
               case Iop_Mul32Fx4: op = ARMneon_VMULFP; break;
               case Iop_Sub32Fx4: op = ARMneon_VSUBFP; break;
               case Iop_Add32Fx4: op = ARMneon_VADDFP; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_NBinary(op, res, argL, argR, size, True));
            return res;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_ITE) { // VFD
      ARMCondCode cc;
      HReg r1  = iselNeonExpr(env, e->Iex.ITE.iftrue);
      HReg r0  = iselNeonExpr(env, e->Iex.ITE.iffalse);
      HReg dst = newVRegV(env);
      addInstr(env, ARMInstr_NUnary(ARMneon_COPY, dst, r1, 4, True));
      cc = iselCondCode(env, e->Iex.ITE.cond);
      addInstr(env, ARMInstr_NCMovQ(cc ^ 1, dst, r0));
      return dst;
   }

  /* neon_expr_bad: */
   ppIRExpr(e);
   vpanic("iselNeonExpr_wrk");
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
      /* Just handle the zero case. */
      IRConst* con = e->Iex.Const.con;
      if (con->tag == Ico_F64i && con->Ico.F64i == 0ULL) {
         HReg z32 = newVRegI(env);
         HReg dst = newVRegD(env);
         addInstr(env, ARMInstr_Imm32(z32, 0));
         addInstr(env, ARMInstr_VXferD(True/*toD*/, dst, z32, z32));
         return dst;
      }
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      ARMAModeV* am;
      HReg res = newVRegD(env);
      vassert(e->Iex.Load.ty == Ity_F64);
      am = iselIntExpr_AModeV(env, e->Iex.Load.addr);
      addInstr(env, ARMInstr_VLdStD(True/*isLoad*/, res, am));
      return res;
   }

   if (e->tag == Iex_Get) {
      // XXX This won't work if offset > 1020 or is not 0 % 4.
      // In which case we'll have to generate more longwinded code.
      ARMAModeV* am  = mkARMAModeV(hregARM_R8(), e->Iex.Get.offset);
      HReg       res = newVRegD(env);
      addInstr(env, ARMInstr_VLdStD(True/*isLoad*/, res, am));
      return res;
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
         case Iop_ReinterpI64asF64: {
            if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
               return iselNeon64Expr(env, e->Iex.Unop.arg);
            } else {
               HReg srcHi, srcLo;
               HReg dst = newVRegD(env);
               iselInt64Expr(&srcHi, &srcLo, env, e->Iex.Unop.arg);
               addInstr(env, ARMInstr_VXferD(True/*toD*/, dst, srcHi, srcLo));
               return dst;
            }
         }
         case Iop_NegF64: {
            HReg src = iselDblExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARMInstr_VUnaryD(ARMvfpu_NEG, dst, src));
            return dst;
         }
         case Iop_AbsF64: {
            HReg src = iselDblExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARMInstr_VUnaryD(ARMvfpu_ABS, dst, src));
            return dst;
         }
         case Iop_F32toF64: {
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);
            addInstr(env, ARMInstr_VCvtSD(True/*sToD*/, dst, src));
            return dst;
         }
         case Iop_I32UtoF64:
         case Iop_I32StoF64: {
            HReg src   = iselIntExpr_R(env, e->Iex.Unop.arg);
            HReg f32   = newVRegF(env);
            HReg dst   = newVRegD(env);
            Bool syned = e->Iex.Unop.op == Iop_I32StoF64;
            /* VMOV f32, src */
            addInstr(env, ARMInstr_VXferS(True/*toS*/, f32, src));
            /* FSITOD dst, f32 */
            addInstr(env, ARMInstr_VCvtID(True/*iToD*/, syned,
                                          dst, f32));
            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_SqrtF64: {
            /* first arg is rounding mode; we ignore it. */
            HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegD(env);
            addInstr(env, ARMInstr_VUnaryD(ARMvfpu_SQRT, dst, src));
            return dst;
         }
         case Iop_RoundF64toInt: {
            /* We can only generate this on a >= V8 capable target.  But
               that's OK since we should only be asked to generate for V8
               capable guests, and we assume here that host == guest. */
            if (VEX_ARM_ARCHLEVEL(env->hwcaps) >= 8) {
               HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
               HReg dst = newVRegD(env);
               set_VFP_rounding_mode(env, e->Iex.Binop.arg1);
               addInstr(env, ARMInstr_VRIntR(True/*isF64*/, dst, src));
               set_VFP_rounding_default(env);
               return dst;
            }
            /* not a V8 target, so we can't select insns for this. */
            break;
         }
         case Iop_MaxNumF64:
         case Iop_MinNumF64: {
            /* Same comments regarding V8 support as for Iop_RoundF64toInt. */
            if (VEX_ARM_ARCHLEVEL(env->hwcaps) >= 8) {
               HReg srcL  = iselDblExpr(env, e->Iex.Binop.arg1);
               HReg srcR  = iselDblExpr(env, e->Iex.Binop.arg2);
               HReg dst   = newVRegD(env);
               Bool isMax = e->Iex.Binop.op == Iop_MaxNumF64;
               addInstr(env, ARMInstr_VMinMaxNum(
                                True/*isF64*/, isMax, dst, srcL, srcR));
               return dst;
            }
            /* not a V8 target, so we can't select insns for this. */
            break;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;

      switch (triop->op) {
         case Iop_DivF64:
         case Iop_MulF64:
         case Iop_AddF64:
         case Iop_SubF64: {
            ARMVfpOp op = 0; /*INVALID*/
            HReg argL = iselDblExpr(env, triop->arg2);
            HReg argR = iselDblExpr(env, triop->arg3);
            HReg dst  = newVRegD(env);
            switch (triop->op) {
               case Iop_DivF64: op = ARMvfp_DIV; break;
               case Iop_MulF64: op = ARMvfp_MUL; break;
               case Iop_AddF64: op = ARMvfp_ADD; break;
               case Iop_SubF64: op = ARMvfp_SUB; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_VAluD(op, dst, argL, argR));
            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_ITE) { // VFD
      if (ty == Ity_F64
          && typeOfIRExpr(env->type_env,e->Iex.ITE.cond) == Ity_I1) {
         HReg r1  = iselDblExpr(env, e->Iex.ITE.iftrue);
         HReg r0  = iselDblExpr(env, e->Iex.ITE.iffalse);
         HReg dst = newVRegD(env);
         addInstr(env, ARMInstr_VUnaryD(ARMvfpu_COPY, dst, r1));
         ARMCondCode cc = iselCondCode(env, e->Iex.ITE.cond);
         addInstr(env, ARMInstr_VCMovD(cc ^ 1, dst, r0));
         return dst;
      }
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
   code emitted by the caller.  */

static HReg iselFltExpr ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselFltExpr_wrk( env, e );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcFlt32);
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

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      ARMAModeV* am;
      HReg res = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_F32);
      am = iselIntExpr_AModeV(env, e->Iex.Load.addr);
      addInstr(env, ARMInstr_VLdStS(True/*isLoad*/, res, am));
      return res;
   }

   if (e->tag == Iex_Get) {
      // XXX This won't work if offset > 1020 or is not 0 % 4.
      // In which case we'll have to generate more longwinded code.
      ARMAModeV* am  = mkARMAModeV(hregARM_R8(), e->Iex.Get.offset);
      HReg       res = newVRegF(env);
      addInstr(env, ARMInstr_VLdStS(True/*isLoad*/, res, am));
      return res;
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
         case Iop_ReinterpI32asF32: {
            HReg dst = newVRegF(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, ARMInstr_VXferS(True/*toS*/, dst, src));
            return dst;
         }
         case Iop_NegF32: {
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegF(env);
            addInstr(env, ARMInstr_VUnaryS(ARMvfpu_NEG, dst, src));
            return dst;
         }
         case Iop_AbsF32: {
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegF(env);
            addInstr(env, ARMInstr_VUnaryS(ARMvfpu_ABS, dst, src));
            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_SqrtF32: {
            /* first arg is rounding mode; we ignore it. */
            HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegF(env);
            addInstr(env, ARMInstr_VUnaryS(ARMvfpu_SQRT, dst, src));
            return dst;
         }
         case Iop_F64toF32: {
            HReg valD = iselDblExpr(env, e->Iex.Binop.arg2);
            set_VFP_rounding_mode(env, e->Iex.Binop.arg1);
            HReg valS = newVRegF(env);
            /* FCVTSD valS, valD */
            addInstr(env, ARMInstr_VCvtSD(False/*!sToD*/, valS, valD));
            set_VFP_rounding_default(env);
            return valS;
         }
         case Iop_RoundF32toInt: {
            /* We can only generate this on a >= V8 capable target.  But
               that's OK since we should only be asked to generate for V8
               capable guests, and we assume here that host == guest. */
            if (VEX_ARM_ARCHLEVEL(env->hwcaps) >= 8) {
               HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
               HReg dst = newVRegF(env);
               set_VFP_rounding_mode(env, e->Iex.Binop.arg1);
               addInstr(env, ARMInstr_VRIntR(False/*!isF64*/, dst, src));
               set_VFP_rounding_default(env);
               return dst;
            }
            /* not a V8 target, so we can't select insns for this. */
            break;
         }
         case Iop_MaxNumF32:
         case Iop_MinNumF32: {
            /* Same comments regarding V8 support as for Iop_RoundF32toInt. */
            if (VEX_ARM_ARCHLEVEL(env->hwcaps) >= 8) {
               HReg srcL  = iselFltExpr(env, e->Iex.Binop.arg1);
               HReg srcR  = iselFltExpr(env, e->Iex.Binop.arg2);
               HReg dst   = newVRegF(env);
               Bool isMax = e->Iex.Binop.op == Iop_MaxNumF32;
               addInstr(env, ARMInstr_VMinMaxNum(
                                False/*!isF64*/, isMax, dst, srcL, srcR));
               return dst;
            }
            /* not a V8 target, so we can't select insns for this. */
            break;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;

      switch (triop->op) {
         case Iop_DivF32:
         case Iop_MulF32:
         case Iop_AddF32:
         case Iop_SubF32: {
            ARMVfpOp op = 0; /*INVALID*/
            HReg argL = iselFltExpr(env, triop->arg2);
            HReg argR = iselFltExpr(env, triop->arg3);
            HReg dst  = newVRegF(env);
            switch (triop->op) {
               case Iop_DivF32: op = ARMvfp_DIV; break;
               case Iop_MulF32: op = ARMvfp_MUL; break;
               case Iop_AddF32: op = ARMvfp_ADD; break;
               case Iop_SubF32: op = ARMvfp_SUB; break;
               default: vassert(0);
            }
            addInstr(env, ARMInstr_VAluS(op, dst, argL, argR));
            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_ITE) { // VFD
      if (ty == Ity_F32
          && typeOfIRExpr(env->type_env,e->Iex.ITE.cond) == Ity_I1) {
         ARMCondCode cc;
         HReg r1  = iselFltExpr(env, e->Iex.ITE.iftrue);
         HReg r0  = iselFltExpr(env, e->Iex.ITE.iffalse);
         HReg dst = newVRegF(env);
         addInstr(env, ARMInstr_VUnaryS(ARMvfpu_COPY, dst, r1));
         cc = iselCondCode(env, e->Iex.ITE.cond);
         addInstr(env, ARMInstr_VCMovS(cc ^ 1, dst, r0));
         return dst;
      }
   }

   ppIRExpr(e);
   vpanic("iselFltExpr_wrk");
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

      if (tya != Ity_I32 || end != Iend_LE) 
         goto stmt_fail;

      if (tyd == Ity_I32) {
         HReg       rD = iselIntExpr_R(env, stmt->Ist.Store.data);
         ARMAMode1* am = iselIntExpr_AMode1(env, stmt->Ist.Store.addr);
         addInstr(env, ARMInstr_LdSt32(ARMcc_AL, False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I16) {
         HReg       rD = iselIntExpr_R(env, stmt->Ist.Store.data);
         ARMAMode2* am = iselIntExpr_AMode2(env, stmt->Ist.Store.addr);
         addInstr(env, ARMInstr_LdSt16(ARMcc_AL,
                                       False/*!isLoad*/,
                                       False/*!isSignedLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I8) {
         HReg       rD = iselIntExpr_R(env, stmt->Ist.Store.data);
         ARMAMode1* am = iselIntExpr_AMode1(env, stmt->Ist.Store.addr);
         addInstr(env, ARMInstr_LdSt8U(ARMcc_AL, False/*!isLoad*/, rD, am));
         return;
      }
      if (tyd == Ity_I64) {
         if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
            HReg dD = iselNeon64Expr(env, stmt->Ist.Store.data);
            ARMAModeN* am = iselIntExpr_AModeN(env, stmt->Ist.Store.addr);
            addInstr(env, ARMInstr_NLdStD(False, dD, am));
         } else {
            HReg rDhi, rDlo, rA;
            iselInt64Expr(&rDhi, &rDlo, env, stmt->Ist.Store.data);
            rA = iselIntExpr_R(env, stmt->Ist.Store.addr);
            addInstr(env, ARMInstr_LdSt32(ARMcc_AL, False/*!load*/, rDhi,
                                          ARMAMode1_RI(rA,4)));
            addInstr(env, ARMInstr_LdSt32(ARMcc_AL, False/*!load*/, rDlo,
                                          ARMAMode1_RI(rA,0)));
         }
         return;
      }
      if (tyd == Ity_F64) {
         HReg       dD = iselDblExpr(env, stmt->Ist.Store.data);
         ARMAModeV* am = iselIntExpr_AModeV(env, stmt->Ist.Store.addr);
         addInstr(env, ARMInstr_VLdStD(False/*!isLoad*/, dD, am));
         return;
      }
      if (tyd == Ity_F32) {
         HReg       fD = iselFltExpr(env, stmt->Ist.Store.data);
         ARMAModeV* am = iselIntExpr_AModeV(env, stmt->Ist.Store.addr);
         addInstr(env, ARMInstr_VLdStS(False/*!isLoad*/, fD, am));
         return;
      }
      if (tyd == Ity_V128) {
         HReg       qD = iselNeonExpr(env, stmt->Ist.Store.data);
         ARMAModeN* am = iselIntExpr_AModeN(env, stmt->Ist.Store.addr);
         addInstr(env, ARMInstr_NLdStQ(False, qD, am));
         return;
      }

      break;
   }

   /* --------- CONDITIONAL STORE --------- */
   /* conditional little-endian write to memory */
   case Ist_StoreG: {
      IRStoreG* sg   = stmt->Ist.StoreG.details;
      IRType    tya  = typeOfIRExpr(env->type_env, sg->addr);
      IRType    tyd  = typeOfIRExpr(env->type_env, sg->data);
      IREndness end  = sg->end;

      if (tya != Ity_I32 || end != Iend_LE) 
         goto stmt_fail;

      switch (tyd) {
         case Ity_I8:
         case Ity_I32: {
            HReg        rD = iselIntExpr_R(env, sg->data);
            ARMAMode1*  am = iselIntExpr_AMode1(env, sg->addr);
            ARMCondCode cc = iselCondCode(env, sg->guard);
            addInstr(env, (tyd == Ity_I32 ? ARMInstr_LdSt32 : ARMInstr_LdSt8U)
                             (cc, False/*!isLoad*/, rD, am));
            return;
         }
         case Ity_I16: {
            HReg        rD = iselIntExpr_R(env, sg->data);
            ARMAMode2*  am = iselIntExpr_AMode2(env, sg->addr);
            ARMCondCode cc = iselCondCode(env, sg->guard);
            addInstr(env, ARMInstr_LdSt16(cc, 
                                          False/*!isLoad*/,
                                          False/*!isSignedLoad*/, rD, am));
            return;
         }
         default:
            break;
      }
      break;
   }

   /* --------- CONDITIONAL LOAD --------- */
   /* conditional little-endian load from memory */
   case Ist_LoadG: {
      IRLoadG*  lg   = stmt->Ist.LoadG.details;
      IRType    tya  = typeOfIRExpr(env->type_env, lg->addr);
      IREndness end  = lg->end;

      if (tya != Ity_I32 || end != Iend_LE) 
         goto stmt_fail;

      switch (lg->cvt) {
         case ILGop_8Uto32:
         case ILGop_Ident32: {
            HReg        rAlt = iselIntExpr_R(env, lg->alt);
            ARMAMode1*  am   = iselIntExpr_AMode1(env, lg->addr);
            HReg        rD   = lookupIRTemp(env, lg->dst);
            addInstr(env, mk_iMOVds_RR(rD, rAlt));
            ARMCondCode cc   = iselCondCode(env, lg->guard);
            addInstr(env, (lg->cvt == ILGop_Ident32 ? ARMInstr_LdSt32
                                                    : ARMInstr_LdSt8U)
                             (cc, True/*isLoad*/, rD, am));
            return;
         }
         case ILGop_16Sto32:
         case ILGop_16Uto32:
         case ILGop_8Sto32: {
            HReg        rAlt = iselIntExpr_R(env, lg->alt);
            ARMAMode2*  am   = iselIntExpr_AMode2(env, lg->addr);
            HReg        rD   = lookupIRTemp(env, lg->dst);
            addInstr(env, mk_iMOVds_RR(rD, rAlt));
            ARMCondCode cc   = iselCondCode(env, lg->guard);
            if (lg->cvt == ILGop_8Sto32) {
               addInstr(env, ARMInstr_Ld8S(cc, rD, am));
            } else {
               vassert(lg->cvt == ILGop_16Sto32 || lg->cvt == ILGop_16Uto32);
               Bool sx = lg->cvt == ILGop_16Sto32;
               addInstr(env, ARMInstr_LdSt16(cc, True/*isLoad*/, sx, rD, am));
            }
            return;
         }
         default:
            break;
      }
      break;
   }

   /* --------- PUT --------- */
   /* write guest state, fixed offset */
   case Ist_Put: {
       IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);

       if (tyd == Ity_I32) {
           HReg       rD = iselIntExpr_R(env, stmt->Ist.Put.data);
           ARMAMode1* am = ARMAMode1_RI(hregARM_R8(), stmt->Ist.Put.offset);
           addInstr(env, ARMInstr_LdSt32(ARMcc_AL, False/*!isLoad*/, rD, am));
           return;
       }
       if (tyd == Ity_I64) {
          if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
             HReg addr = newVRegI(env);
             HReg qD = iselNeon64Expr(env, stmt->Ist.Put.data);
             addInstr(env, ARMInstr_Add32(addr, hregARM_R8(),
                                                stmt->Ist.Put.offset));
             addInstr(env, ARMInstr_NLdStD(False, qD, mkARMAModeN_R(addr)));
          } else {
             HReg rDhi, rDlo;
             ARMAMode1* am0 = ARMAMode1_RI(hregARM_R8(),
                                           stmt->Ist.Put.offset + 0);
             ARMAMode1* am4 = ARMAMode1_RI(hregARM_R8(),
                                           stmt->Ist.Put.offset + 4);
             iselInt64Expr(&rDhi, &rDlo, env, stmt->Ist.Put.data);
             addInstr(env, ARMInstr_LdSt32(ARMcc_AL, False/*!isLoad*/,
                                           rDhi, am4));
             addInstr(env, ARMInstr_LdSt32(ARMcc_AL, False/*!isLoad*/,
                                           rDlo, am0));
          }
          return;
       }
       if (tyd == Ity_F64) {
          // XXX This won't work if offset > 1020 or is not 0 % 4.
          // In which case we'll have to generate more longwinded code.
          ARMAModeV* am = mkARMAModeV(hregARM_R8(), stmt->Ist.Put.offset);
          HReg       rD = iselDblExpr(env, stmt->Ist.Put.data);
          addInstr(env, ARMInstr_VLdStD(False/*!isLoad*/, rD, am));
          return;
       }
       if (tyd == Ity_F32) {
          // XXX This won't work if offset > 1020 or is not 0 % 4.
          // In which case we'll have to generate more longwinded code.
          ARMAModeV* am = mkARMAModeV(hregARM_R8(), stmt->Ist.Put.offset);
          HReg       rD = iselFltExpr(env, stmt->Ist.Put.data);
          addInstr(env, ARMInstr_VLdStS(False/*!isLoad*/, rD, am));
          return;
       }
       if (tyd == Ity_V128) {
          HReg addr = newVRegI(env);
          HReg qD = iselNeonExpr(env, stmt->Ist.Put.data);
          addInstr(env, ARMInstr_Add32(addr, hregARM_R8(),
                                       stmt->Ist.Put.offset));
          addInstr(env, ARMInstr_NLdStQ(False, qD, mkARMAModeN_R(addr)));
          return;
       }
       break;
   }

   /* --------- TMP --------- */
   /* assign value to temporary */
   case Ist_WrTmp: {
      IRTemp tmp = stmt->Ist.WrTmp.tmp;
      IRType ty = typeOfIRTemp(env->type_env, tmp);

      if (ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
         ARMRI84* ri84 = iselIntExpr_RI84(NULL, False,
                                          env, stmt->Ist.WrTmp.data);
         HReg     dst  = lookupIRTemp(env, tmp);
         addInstr(env, ARMInstr_Mov(dst,ri84));
         return;
      }
      if (ty == Ity_I1) {
         /* Here, we are generating a I1 value into a 32 bit register.
            Make sure the value in the register is only zero or one,
            but no other.  This allows optimisation of the
            1Uto32(tmp:I1) case, by making it simply a copy of the
            register holding 'tmp'.  The point being that the value in
            the register holding 'tmp' can only have been created
            here. */
         HReg        dst  = lookupIRTemp(env, tmp);
         ARMCondCode cond = iselCondCode(env, stmt->Ist.WrTmp.data);
         addInstr(env, ARMInstr_Mov(dst, ARMRI84_I84(0,0)));
         addInstr(env, ARMInstr_CMov(cond, dst, ARMRI84_I84(1,0)));
         return;
      }
      if (ty == Ity_I64) {
         if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
            HReg src = iselNeon64Expr(env, stmt->Ist.WrTmp.data);
            HReg dst = lookupIRTemp(env, tmp);
            addInstr(env, ARMInstr_NUnary(ARMneon_COPY, dst, src, 4, False));
         } else {
            HReg rHi, rLo, dstHi, dstLo;
            iselInt64Expr(&rHi,&rLo, env, stmt->Ist.WrTmp.data);
            lookupIRTemp64( &dstHi, &dstLo, env, tmp);
            addInstr(env, mk_iMOVds_RR(dstHi, rHi) );
            addInstr(env, mk_iMOVds_RR(dstLo, rLo) );
         }
         return;
      }
      if (ty == Ity_F64) {
         HReg src = iselDblExpr(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, ARMInstr_VUnaryD(ARMvfpu_COPY, dst, src));
         return;
      }
      if (ty == Ity_F32) {
         HReg src = iselFltExpr(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, ARMInstr_VUnaryS(ARMvfpu_COPY, dst, src));
         return;
      }
      if (ty == Ity_V128) {
         HReg src = iselNeonExpr(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, ARMInstr_NUnary(ARMneon_COPY, dst, src, 4, True));
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
      Bool   ok      = doHelperCall( &addToSp, &rloc, env,
                                     d->guard, d->cee, retty, d->args );
      if (!ok) goto stmt_fail;
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
         case Ity_I64: {
            vassert(rloc.pri == RLPri_2Int);
            vassert(addToSp == 0);
            if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
               HReg tmp = lookupIRTemp(env, d->tmp);
               addInstr(env, ARMInstr_VXferD(True, tmp, hregARM_R1(),
                                                        hregARM_R0()));
            } else {
               HReg dstHi, dstLo;
               /* The returned value is in r1:r0.  Park it in the
                  register-pair associated with tmp. */
               lookupIRTemp64( &dstHi, &dstLo, env, d->tmp);
               addInstr(env, mk_iMOVds_RR(dstHi, hregARM_R1()) );
               addInstr(env, mk_iMOVds_RR(dstLo, hregARM_R0()) );
            }
            return;
         }
         case Ity_I32: case Ity_I16: case Ity_I8: {
            vassert(rloc.pri == RLPri_Int);
            vassert(addToSp == 0);
            /* The returned value is in r0.  Park it in the register
               associated with tmp. */
            HReg dst = lookupIRTemp(env, d->tmp);
            addInstr(env, mk_iMOVds_RR(dst, hregARM_R0()) );
            return;
         }
         case Ity_V128: {
            /* The returned value is on the stack, and *retloc tells
               us where.  Fish it off the stack and then move the
               stack pointer upwards to clear it, as directed by
               doHelperCall. */
            vassert(rloc.pri == RLPri_V128SpRel);
            vassert(rloc.spOff < 256); // else ARMRI84_I84(_,0) can't encode it
            vassert(addToSp >= 16);
            vassert(addToSp <= 256);
            /* Both the stack delta and the offset must be at least 8-aligned.
               If that isn't so, doHelperCall() has generated bad code. */
            vassert(0 == (rloc.spOff % 8));
            vassert(0 == (addToSp % 8));
            HReg dst = lookupIRTemp(env, d->tmp);
            HReg tmp = newVRegI(env);
            HReg sp  = hregARM_R13();
            addInstr(env, ARMInstr_Alu(ARMalu_ADD,
                                       tmp, sp, ARMRI84_I84(rloc.spOff,0)));
            ARMAModeN* am = mkARMAModeN_R(tmp);
            /* This load could be done with its effective address 0 % 8,
               because that's the best stack alignment that we can be
               assured of. */
            addInstr(env, ARMInstr_NLdStQ(True/*load*/, dst, am));

            ARMRI84* spAdj
               = addToSp == 256 ? ARMRI84_I84(64, 15) // 64 `ror` (15 * 2)
                                : ARMRI84_I84(addToSp, 0);
            addInstr(env, ARMInstr_Alu(ARMalu_ADD, sp, sp, spAdj));
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
         if (ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
            Int  szB   = 0;
            HReg r_dst = lookupIRTemp(env, res);
            HReg raddr = iselIntExpr_R(env, stmt->Ist.LLSC.addr);
            switch (ty) {
               case Ity_I8:  szB = 1; break;
               case Ity_I16: szB = 2; break;
               case Ity_I32: szB = 4; break;
               default:      vassert(0);
            }
            addInstr(env, mk_iMOVds_RR(hregARM_R4(), raddr));
            addInstr(env, ARMInstr_LdrEX(szB));
            addInstr(env, mk_iMOVds_RR(r_dst, hregARM_R2()));
            return;
         }
         if (ty == Ity_I64) {
            HReg raddr = iselIntExpr_R(env, stmt->Ist.LLSC.addr);
            addInstr(env, mk_iMOVds_RR(hregARM_R4(), raddr));
            addInstr(env, ARMInstr_LdrEX(8));
            /* Result is in r3:r2.  On a non-NEON capable CPU, we must
               move it into a result register pair.  On a NEON capable
               CPU, the result register will be a 64 bit NEON
               register, so we must move it there instead. */
            if (env->hwcaps & VEX_HWCAPS_ARM_NEON) {
               HReg dst = lookupIRTemp(env, res);
               addInstr(env, ARMInstr_VXferD(True, dst, hregARM_R3(),
                                                        hregARM_R2()));
            } else {
               HReg r_dst_hi, r_dst_lo;
               lookupIRTemp64(&r_dst_hi, &r_dst_lo, env, res);
               addInstr(env, mk_iMOVds_RR(r_dst_lo, hregARM_R2()));
               addInstr(env, mk_iMOVds_RR(r_dst_hi, hregARM_R3()));
            }
            return;
         }
         /*NOTREACHED*/
         vassert(0); 
      } else {
         /* SC */
         IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.storedata);
         if (tyd == Ity_I32 || tyd == Ity_I16 || tyd == Ity_I8) {
            Int  szB = 0;
            HReg rD  = iselIntExpr_R(env, stmt->Ist.LLSC.storedata);
            HReg rA  = iselIntExpr_R(env, stmt->Ist.LLSC.addr);
            switch (tyd) {
               case Ity_I8:  szB = 1; break;
               case Ity_I16: szB = 2; break;
               case Ity_I32: szB = 4; break;
               default:      vassert(0);
            }
            addInstr(env, mk_iMOVds_RR(hregARM_R2(), rD));
            addInstr(env, mk_iMOVds_RR(hregARM_R4(), rA));
            addInstr(env, ARMInstr_StrEX(szB));
         } else {
            vassert(tyd == Ity_I64);
            /* This is really ugly.  There is no is/is-not NEON
               decision akin to the case for LL, because iselInt64Expr
               fudges this for us, and always gets the result into two
               GPRs even if this means moving it from a NEON
               register. */
            HReg rDhi, rDlo;
            iselInt64Expr(&rDhi, &rDlo, env, stmt->Ist.LLSC.storedata);
            HReg rA = iselIntExpr_R(env, stmt->Ist.LLSC.addr);
            addInstr(env, mk_iMOVds_RR(hregARM_R2(), rDlo));
            addInstr(env, mk_iMOVds_RR(hregARM_R3(), rDhi));
            addInstr(env, mk_iMOVds_RR(hregARM_R4(), rA));
            addInstr(env, ARMInstr_StrEX(8));
         }
         /* now r0 is 1 if failed, 0 if success.  Change to IR
            conventions (0 is fail, 1 is success).  Also transfer
            result to r_res. */
         IRTemp   res   = stmt->Ist.LLSC.result;
         IRType   ty    = typeOfIRTemp(env->type_env, res);
         HReg     r_res = lookupIRTemp(env, res);
         ARMRI84* one   = ARMRI84_I84(1,0);
         vassert(ty == Ity_I1);
         addInstr(env, ARMInstr_Alu(ARMalu_XOR, r_res, hregARM_R0(), one));
         /* And be conservative -- mask off all but the lowest bit */
         addInstr(env, ARMInstr_Alu(ARMalu_AND, r_res, r_res, one));
         return;
      }
      break;
   }

   /* --------- MEM FENCE --------- */
   case Ist_MBE:
      switch (stmt->Ist.MBE.event) {
         case Imbe_Fence:
            addInstr(env, ARMInstr_MFence());
            return;
         case Imbe_CancelReservation:
            addInstr(env, ARMInstr_CLREX());
            return;
         default:
            break;
      }
      break;

   /* --------- INSTR MARK --------- */
   /* Doesn't generate any executable code ... */
   case Ist_IMark:
       return;

   /* --------- NO-OP --------- */
   case Ist_NoOp:
       return;

   /* --------- EXIT --------- */
   case Ist_Exit: {
      if (stmt->Ist.Exit.dst->tag != Ico_U32)
         vpanic("isel_arm: Ist_Exit: dst is not a 32-bit value");

      ARMCondCode cc     = iselCondCode(env, stmt->Ist.Exit.guard);
      ARMAMode1*  amR15T = ARMAMode1_RI(hregARM_R8(),
                                        stmt->Ist.Exit.offsIP);

      /* Case: boring transfer to known address */
      if (stmt->Ist.Exit.jk == Ijk_Boring
          || stmt->Ist.Exit.jk == Ijk_Call
          || stmt->Ist.Exit.jk == Ijk_Ret) {
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = stmt->Ist.Exit.dst->Ico.U32 > env->max_ga;
            if (0) vex_printf("%s", toFastEP ? "Y" : ",");
            addInstr(env, ARMInstr_XDirect(stmt->Ist.Exit.dst->Ico.U32,
                                           amR15T, cc, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, ARMInstr_XAssisted(r, amR15T, cc, Ijk_Boring));
         }
         return;
      }

      /* Case: assisted transfer to arbitrary address */
      switch (stmt->Ist.Exit.jk) {
         /* Keep this list in sync with that in iselNext below */
         case Ijk_ClientReq:
         case Ijk_NoDecode:
         case Ijk_NoRedir:
         case Ijk_Sys_syscall:
         case Ijk_InvalICache:
         case Ijk_Yield:
         {
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, ARMInstr_XAssisted(r, amR15T, cc,
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
      vassert(cdst->tag == Ico_U32);
      if (jk == Ijk_Boring || jk == Ijk_Call) {
         /* Boring transfer to known address */
         ARMAMode1* amR15T = ARMAMode1_RI(hregARM_R8(), offsIP);
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = cdst->Ico.U32 > env->max_ga;
            if (0) vex_printf("%s", toFastEP ? "X" : ".");
            addInstr(env, ARMInstr_XDirect(cdst->Ico.U32,
                                           amR15T, ARMcc_AL, 
                                           toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, next);
            addInstr(env, ARMInstr_XAssisted(r, amR15T, ARMcc_AL,
                                             Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
      case Ijk_Boring: case Ijk_Ret: case Ijk_Call: {
         HReg       r      = iselIntExpr_R(env, next);
         ARMAMode1* amR15T = ARMAMode1_RI(hregARM_R8(), offsIP);
         if (env->chainingAllowed) {
            addInstr(env, ARMInstr_XIndir(r, amR15T, ARMcc_AL));
         } else {
            addInstr(env, ARMInstr_XAssisted(r, amR15T, ARMcc_AL,
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
      case Ijk_Yield:
      {
         HReg       r      = iselIntExpr_R(env, next);
         ARMAMode1* amR15T = ARMAMode1_RI(hregARM_R8(), offsIP);
         addInstr(env, ARMInstr_XAssisted(r, amR15T, ARMcc_AL, jk));
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

/* Translate an entire SB to arm code. */

HInstrArray* iselSB_ARM ( const IRSB* bb,
                          VexArch      arch_host,
                          const VexArchInfo* archinfo_host,
                          const VexAbiInfo*  vbi/*UNUSED*/,
                          Int offs_Host_EvC_Counter,
                          Int offs_Host_EvC_FailAddr,
                          Bool chainingAllowed,
                          Bool addProfInc,
                          Addr max_ga )
{
   Int       i, j;
   HReg      hreg, hregHI;
   ISelEnv*  env;
   UInt      hwcaps_host = archinfo_host->hwcaps;
   ARMAMode1 *amCounter, *amFailAddr;

   /* sanity ... */
   vassert(arch_host == VexArchARM);

   /* Check that the host's endianness is as expected. */
   vassert(archinfo_host->endness == VexEndnessLE);

   /* guard against unexpected space regressions */
   vassert(sizeof(ARMInstr) <= 28);

   /* hwcaps should not change from one ISEL call to another. */
   arm_hwcaps = hwcaps_host; // JRS 2012 Mar 31: FIXME (RM)

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
         case Ity_I32:  hreg   = mkHReg(True, HRcInt32, 0, j++); break;
         case Ity_I64:
            if (hwcaps_host & VEX_HWCAPS_ARM_NEON) {
               hreg = mkHReg(True, HRcFlt64, 0, j++);
            } else {
               hregHI = mkHReg(True, HRcInt32, 0, j++);
               hreg   = mkHReg(True, HRcInt32, 0, j++);
            }
            break;
         case Ity_F32:  hreg   = mkHReg(True, HRcFlt32,  0, j++); break;
         case Ity_F64:  hreg   = mkHReg(True, HRcFlt64,  0, j++); break;
         case Ity_V128: hreg   = mkHReg(True, HRcVec128, 0, j++); break;
         default: ppIRType(bb->tyenv->types[i]);
                  vpanic("iselBB: IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   amCounter  = ARMAMode1_RI(hregARM_R8(), offs_Host_EvC_Counter);
   amFailAddr = ARMAMode1_RI(hregARM_R8(), offs_Host_EvC_FailAddr);
   addInstr(env, ARMInstr_EvCheck(amCounter, amFailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, ARMInstr_ProfInc());
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
/*--- end                                     host_arm_isel.c ---*/
/*---------------------------------------------------------------*/

