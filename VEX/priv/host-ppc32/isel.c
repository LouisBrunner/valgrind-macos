
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-ppc32/isel.c) is                        ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "ir/irmatch.h"
#include "main/vex_util.h"
#include "main/vex_globals.h"
#include "host-generic/h_generic_regs.h"
#include "host-ppc32/hdefs.h"

/*---------------------------------------------------------*/
/*--- Register Usage Conventions                        ---*/
/*---------------------------------------------------------*/
/*
  Integer Regs
  ------------
  GPR0       Reserved
  GPR1       Stack Pointer
  GPR2       TOC pointer - not used
  GPR3:12    Allocateable
  GPR13      Thread-specific pointer - not used
  GPR14:29   Allocateable
  GPR30      AltiVec temp spill register
  GPR31      GuestStatePointer

  Of Allocateable regs:
  GPR3:12    Caller-saved regs
  GPR14:29   Callee-saved regs

  GPR3       [Return | Parameter] - carrying reg
  GPR4:10    Parameter-carrying regs


  Floating Point Regs
  -------------------
  FPR0:31    Allocateable

  FPR0:13    Caller-saved regs
  FPR14:31   Callee-saved regs  
*/


/*---------------------------------------------------------*/
/*--- PPC32 FP Status & Control Register Conventions    ---*/
/*---------------------------------------------------------*/
/*
  Vex-generated code expects to run with the FPU set as follows: all
  exceptions masked, round-to-nearest.
  This corresponds to a FPU control word value of 0x0.
  
  %fpscr should have this value on entry to Vex-generated code,
  and those values should be unchanged at exit.
  
  Warning: For simplicity, set_FPU_rounding_* assumes this is 0x0
  - if this is changed, update those functions.
*/


/*---------------------------------------------------------*/
/*--- misc helpers                                      ---*/
/*---------------------------------------------------------*/

/* These are duplicated in guest-ppc32/toIR.c */
static IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

#if 0
static IRExpr* binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   return IRExpr_Binop(op, a1, a2);
}
#endif

//.. static IRExpr* mkU64 ( ULong i )
//.. {
//..    return IRExpr_Const(IRConst_U64(i));
//.. }

static IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static IRExpr* bind ( Int binder )
{
   return IRExpr_Binder(binder);
}


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

    - A copy of the link reg, so helper functions don't kill it.

    - The code array, that is, the insns selected so far.
 
    - A counter, for generating new virtual registers.
 
    - The host subarchitecture we are selecting insns for.  
      This is set at the start and does not change.
 
    Note, this is all host-independent.  (JRS 20050201: well, kinda
   ... not completely.  Compare with ISelEnv for amd64.)
*/
 
typedef
   struct {
      IRTypeEnv*   type_env;
 
      HReg*        vregmap;
      HReg*        vregmapHI;
      Int          n_vregmap;
 
      HReg         savedLR;

      HInstrArray* code;
 
      Int          vreg_ctr;
 
      VexSubArch   subarch;
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

static void addInstr ( ISelEnv* env, PPC32Instr* instr )
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppPPC32Instr(instr);
      vex_printf("\n");
   }
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(env->vreg_ctr, HRcInt32, True/*virtual reg*/);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF ( ISelEnv* env )
{
   HReg reg = mkHReg(env->vreg_ctr, HRcFlt64, True/*virtual reg*/);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegV ( ISelEnv* env )
{
   HReg reg = mkHReg(env->vreg_ctr, HRcVec128, True/*virtual reg*/);
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
/* Compute an I8/I16/I32 into a GPR. */
static HReg          iselIntExpr_R_wrk ( ISelEnv* env, IRExpr* e );
static HReg          iselIntExpr_R     ( ISelEnv* env, IRExpr* e );

/* Compute an I8/I16/I32 into a RH (reg-or-halfword-immediate).  It's
   important to specify whether the immediate is to be regarded as
   signed or not.  If yes, this will never return -32768 as an
   immediate; this guaranteed that all signed immediates that are
   return can have their sign inverted if need be. */
static PPC32RH*      iselIntExpr_RH_wrk ( ISelEnv* env, 
                                          Bool syned, IRExpr* e );
static PPC32RH*      iselIntExpr_RH     ( ISelEnv* env, 
                                          Bool syned, IRExpr* e );

/* Compute an I32 into a RI (reg or 32-bit immediate). */
static PPC32RI*      iselIntExpr_RI_wrk ( ISelEnv* env, IRExpr* e );
static PPC32RI*      iselIntExpr_RI     ( ISelEnv* env, IRExpr* e );

/* Compute an I8 into a reg-or-5-bit-unsigned-immediate, the latter
   being an immediate in the range 1 .. 31 inclusive.  Used for doing
   shift amounts. */
static PPC32RH*      iselIntExpr_RH5u_wrk ( ISelEnv* env, IRExpr* e );
static PPC32RH*      iselIntExpr_RH5u     ( ISelEnv* env, IRExpr* e );

/* Compute an I32 into an AMode. */
static PPC32AMode*   iselIntExpr_AMode_wrk ( ISelEnv* env, IRExpr* e );
static PPC32AMode*   iselIntExpr_AMode     ( ISelEnv* env, IRExpr* e );

/* Compute an I64 into a GPR pair. */
static void          iselInt64Expr_wrk ( HReg* rHi, HReg* rLo, 
                                         ISelEnv* env, IRExpr* e );
static void          iselInt64Expr     ( HReg* rHi, HReg* rLo, 
                                         ISelEnv* env, IRExpr* e );

static PPC32CondCode iselCondCode_wrk ( ISelEnv* env, IRExpr* e );
static PPC32CondCode iselCondCode     ( ISelEnv* env, IRExpr* e );

static HReg          iselDblExpr_wrk ( ISelEnv* env, IRExpr* e );
static HReg          iselDblExpr     ( ISelEnv* env, IRExpr* e );

static HReg          iselFltExpr_wrk ( ISelEnv* env, IRExpr* e );
static HReg          iselFltExpr     ( ISelEnv* env, IRExpr* e );

static HReg          iselVecExpr_wrk ( ISelEnv* env, IRExpr* e );
static HReg          iselVecExpr     ( ISelEnv* env, IRExpr* e );


/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

//.. /* Is this a 32-bit zero expression? */
//.. 
//.. static Bool isZero32 ( IRExpr* e )
//.. {
//..    return e->tag == Iex_Const
//..           && e->Iex.Const.con->tag == Ico_U32
//..           && e->Iex.Const.con->Ico.U32 == 0;
//.. }

/* Make an int reg-reg move. */

static PPC32Instr* mk_iMOVds_RR ( HReg r_dst, HReg r_src )
{
   vassert(hregClass(r_dst) == HRcInt32);
   vassert(hregClass(r_src) == HRcInt32);
   return PPC32Instr_Alu32(Palu_OR, r_dst, r_src, PPC32RH_Reg(r_src));
}

//.. /* Make a vector reg-reg move. */
//.. 
//.. static X86Instr* mk_vMOVsd_RR ( HReg src, HReg dst )
//.. {
//..    vassert(hregClass(src) == HRcVec128);
//..    vassert(hregClass(dst) == HRcVec128);
//..    return X86Instr_SseReRg(Xsse_MOV, src, dst);
//.. }

/* Advance/retreat %sp by n. */

static void add_to_sp ( ISelEnv* env, Int n )
{
   HReg sp = StackFramePtr;
   vassert(n > 0 && n < 256 && (n%16) == 0);
   addInstr(env, PPC32Instr_Alu32(
                    Palu_ADD, sp, sp, PPC32RH_Imm(True,toUShort(n))));
}

static void sub_from_sp ( ISelEnv* env, Int n )
{
   HReg sp = StackFramePtr;
   vassert(n > 0 && n < 256 && (n%16) == 0);
   addInstr(env, PPC32Instr_Alu32(
                    Palu_SUB, sp, sp, PPC32RH_Imm(True,toUShort(n))));
}

/*
  returns a quadword aligned address on the stack
   - copies SP, adds 16bytes, aligns to quadword.
  use sub_from_sp(32) before calling this,
  as expects to have 32 bytes to play with.
*/
static HReg get_sp_aligned16 ( ISelEnv* env )
{
   HReg       r = newVRegI(env);
   HReg align16 = newVRegI(env);
   addInstr(env, mk_iMOVds_RR(r, StackFramePtr));
   // add 16
   addInstr(env, PPC32Instr_Alu32(
                    Palu_ADD, r, r, PPC32RH_Imm(True,toUShort(16))));
   // mask to quadword
   addInstr(env, PPC32Instr_LI32(align16, (UInt)0xFFFFFFF0));
   addInstr(env, PPC32Instr_Alu32(Palu_AND, r,r, PPC32RH_Reg(align16)));
   return r;
}



/* Load 2*I32 regs to fp reg */
static HReg mk_LoadRRtoFPR ( ISelEnv* env, HReg r_srcHi, HReg r_srcLo )
{
   HReg fr_dst = newVRegF(env);
   PPC32AMode *am_addr0, *am_addr1;

   sub_from_sp( env, 16 );        // Move SP down 16 bytes
   am_addr0 = PPC32AMode_IR(0, StackFramePtr);
   am_addr1 = PPC32AMode_IR(4, StackFramePtr);

   // store hi,lo as Ity_I32's
   addInstr(env, PPC32Instr_Store( 4, am_addr0, r_srcHi ));
   addInstr(env, PPC32Instr_Store( 4, am_addr1, r_srcLo ));

   // load as float
   addInstr(env, PPC32Instr_FpLdSt(True/*load*/, 8, fr_dst, am_addr0));
   
   add_to_sp( env, 16 );          // Reset SP
   return fr_dst;
}


/* Given an amode, return one which references 4 bytes further
   along. */

static PPC32AMode* advance4 ( ISelEnv* env, PPC32AMode* am )
{
   PPC32AMode* am4 = dopyPPC32AMode(am);
   if (am4->tag == Pam_IR 
       && am4->Pam.IR.index + 4 <= 32767) {
      am4->Pam.IR.index += 4;
   } else {
      vpanic("advance4(ppc32,host)");
   }
   return am4;
}

/* Used only in doHelperCall.  See big comment in doHelperCall re
   handling of register-parameter args.  This function figures out
   whether evaluation of an expression might require use of a fixed
   register.  If in doubt return True (safe but suboptimal).
*/
static
Bool mightRequireFixedRegs ( IRExpr* e )
{
   switch (e->tag) {
   case Iex_Tmp: case Iex_Const: case Iex_Get: 
      return False;
   default:
      return True;
   }
}


/* Do a complete function call.  guard is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional. */

static
void doHelperCall ( ISelEnv* env, 
                    Bool passBBP, 
                    IRExpr* guard, IRCallee* cee, IRExpr** args )
{
   PPC32CondCode cc;
   HReg          argregs[PPC32_N_REGPARMS];
   HReg          tmpregs[PPC32_N_REGPARMS];
   Bool          go_fast;
   Int           n_args, i, argreg;
   UInt          argiregs;

   /* Marshal args for a call and do the call.

      If passBBP is True, %rbp (the baseblock pointer) is to be passed
      as the first arg.

      This function only deals with a tiny set of possibilities, which
      cover all helpers in practice.  The restrictions are that only
      arguments in registers are supported, hence only PPC32_N_REGPARMSx32
      integer bits in total can be passed.  In fact the only supported
      arg type is I32.

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

   /* Note that the cee->regparms field is meaningless on PPC32 host
      (since there is only one calling convention) and so we always
      ignore it. */

   n_args = 0;
   for (i = 0; args[i]; i++)
      n_args++;

   if (PPC32_N_REGPARMS < n_args + (passBBP ? 1 : 0)) {
      vpanic("doHelperCall(PPC32): cannot currently handle > 8 args");
      // PPC32_N_REGPARMS
   }
   
   argregs[0] = hregPPC32_GPR3();
   argregs[1] = hregPPC32_GPR4();
   argregs[2] = hregPPC32_GPR5();
   argregs[3] = hregPPC32_GPR6();
   argregs[4] = hregPPC32_GPR7();
   argregs[5] = hregPPC32_GPR8();
   argregs[6] = hregPPC32_GPR9();
   argregs[7] = hregPPC32_GPR10();
   argiregs = 0;

   tmpregs[0] = tmpregs[1] = tmpregs[2] =
   tmpregs[3] = tmpregs[4] = tmpregs[5] =
   tmpregs[6] = tmpregs[7] = INVALID_HREG;

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

   /* At this point the scheme to use has been established.  Generate
      code to get the arg values into the argument rregs. */

   if (go_fast) {

      /* FAST SCHEME */
      argreg = 0;
      if (passBBP) {
         argiregs |= (1 << (argreg+3));
         addInstr(env, mk_iMOVds_RR( argregs[argreg], GuestStatePtr ));
         argreg++;
      }

      for (i = 0; i < n_args; i++) {
         vassert(argreg < PPC32_N_REGPARMS);
         vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32 ||
                 typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
         if (typeOfIRExpr(env->type_env, args[i]) == Ity_I32) { 
            argiregs |= (1 << (argreg+3));
            addInstr(env, mk_iMOVds_RR( argregs[argreg],
                                        iselIntExpr_R(env, args[i]) ));
         } else { // Ity_I64
            HReg rHi, rLo;
            if (argreg%2 == 1) // ppc32 abi spec for passing a LONG_LONG
               argreg++;       // XXX: odd argreg => even rN
            vassert(argreg < PPC32_N_REGPARMS-1);
            iselInt64Expr(&rHi,&rLo, env, args[i]);
            argiregs |= (1 << (argreg+3));
            addInstr(env, mk_iMOVds_RR( argregs[argreg++], rHi ));
            argiregs |= (1 << (argreg+3));
            addInstr(env, mk_iMOVds_RR( argregs[argreg], rLo));
         }
         argreg++;
      }

      /* Fast scheme only applies for unconditional calls.  Hence: */
      cc.test = Pct_ALWAYS;

   } else {

      /* SLOW SCHEME; move via temporaries */
      argreg = 0;

      if (passBBP) {
         /* This is pretty stupid; better to move directly to r3
            after the rest of the args are done. */
         tmpregs[argreg] = newVRegI(env);
         addInstr(env, mk_iMOVds_RR( tmpregs[argreg], GuestStatePtr ));
         argreg++;
      }

      for (i = 0; i < n_args; i++) {
         vassert(argreg < PPC32_N_REGPARMS);
         vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32 ||
                 typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
         if (typeOfIRExpr(env->type_env, args[i]) == Ity_I32) { 
            tmpregs[argreg] = iselIntExpr_R(env, args[i]);
         } else { // Ity_I64
            HReg rHi, rLo;
            if (argreg%2 == 1) // ppc32 abi spec for passing a LONG_LONG
               argreg++;       // XXX: odd argreg => even rN
            vassert(argreg < PPC32_N_REGPARMS-1);
            iselInt64Expr(&rHi,&rLo, env, args[i]);
            tmpregs[argreg++] = rHi;
            tmpregs[argreg]   = rLo;
         }
         argreg++;
      }

      /* Now we can compute the condition.  We can't do it earlier
         because the argument computations could trash the condition
         codes.  Be a bit clever to handle the common case where the
         guard is 1:Bit. */
      cc.test = Pct_ALWAYS;
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
      for (i = 0; i < argreg; i++) {
         if (tmpregs[i] == INVALID_HREG)  // Skip invalid regs
            continue;
         /* None of these insns, including any spill code that might
            be generated, may alter the condition codes. */
         argiregs |= (1 << (i+3));
         addInstr( env, mk_iMOVds_RR( argregs[i], tmpregs[i] ) );
      }

   }

   /* Finally, the call itself. */
   addInstr(env, PPC32Instr_Call( cc,
                                  (Addr32)toUInt(Ptr_to_ULong(cee->addr)),
                                  argiregs ));
}


/* Set FPU's rounding mode to the default */
static 
void set_FPU_rounding_default ( ISelEnv* env )
{
   HReg fr_src  = newVRegF(env);
   HReg r_srcHi = newVRegI(env);
   HReg r_srcLo = newVRegI(env);

   /* Default rounding mode = 0x0
      Only supporting the rounding-mode bits - the rest of FPSCR is 0x0
       - so we can set the whole register at once (faster)
   */
   addInstr(env, PPC32Instr_LI32(r_srcLo, 0x0));
   // r_srcHi = 0: upper 32 bits ignored by FpLdFPSCR
   addInstr(env, PPC32Instr_LI32(r_srcHi, 0x0));

   fr_src = mk_LoadRRtoFPR( env, r_srcHi, r_srcLo );
   addInstr(env, PPC32Instr_FpLdFPSCR( fr_src ));
}

/* Convert IR rounding mode to PPC32 encoding */
static HReg roundModeIRtoPPC32 ( ISelEnv* env, HReg r_rmIR )
{
/* 
   rounding mode | PPC | IR
   ------------------------
   to nearest    | 00  | 00
   to zero       | 01  | 11
   to +infinity  | 10  | 10
   to -infinity  | 11  | 01
*/
   HReg r_rmPPC32 = newVRegI(env);
   HReg r_tmp     = newVRegI(env);

   // AND r_rmRI,3   -- shouldn't be needed; paranoia
   addInstr(env, 
      PPC32Instr_Alu32(Palu_AND, r_rmIR, r_rmIR, PPC32RH_Imm(False,3)));

   // r_rmPPC32 = XOR( r_rmIR, (r_rmIR << 1) & 2)
   addInstr(env, 
      PPC32Instr_Alu32(Palu_SHL, r_tmp, r_rmIR, PPC32RH_Imm(False,1)));
   addInstr(env, 
      PPC32Instr_Alu32(Palu_AND, r_tmp, r_tmp, PPC32RH_Imm(False,2)));
   addInstr(env, 
      PPC32Instr_Alu32(Palu_XOR, r_rmPPC32, r_rmIR, PPC32RH_Reg(r_tmp)));
   return r_rmPPC32;
}


/* Mess with the FPU's rounding mode: 'mode' is an I32-typed
   expression denoting a value in the range 0 .. 3, indicating a round
   mode encoded as per type IRRoundingMode.  Set the PPC32 FPSCR to have
   the same rounding.
   For speed & simplicity, we're setting the *entire* FPSCR here.
*/
static
void set_FPU_rounding_mode ( ISelEnv* env, IRExpr* mode )
{
   HReg fr_src  = newVRegF(env);
   HReg r_srcHi = newVRegI(env);

   /* Only supporting the rounding-mode bits - the rest of FPSCR is 0x0
       - so we can set the whole register at once (faster)
   */

   // Resolve rounding mode and convert to PPC32 representation
   HReg r_srcLo = roundModeIRtoPPC32( env, iselIntExpr_R(env, mode) );

   // srcHi = 0: upper 32 bits ignored by FpLdFPSCR
   addInstr(env, PPC32Instr_LI32(r_srcHi, 0));

   // Load 2*I32 regs to fp reg:
   fr_src = mk_LoadRRtoFPR( env, r_srcHi, r_srcLo );

   // Move to FPSCR
   addInstr(env, PPC32Instr_FpLdFPSCR( fr_src ));
}


//.. /* Generate !src into a new vector register, and be sure that the code
//..    is SSE1 compatible.  Amazing that Intel doesn't offer a less crappy
//..    way to do this. 
//.. */
//.. static HReg do_sse_Not128 ( ISelEnv* env, HReg src )
//.. {
//..    HReg dst = newVRegV(env);
//..    /* Set dst to zero.  Not strictly necessary, but the idea of doing
//..       a FP comparison on whatever junk happens to be floating around
//..       in it is just too scary. */
//..    addInstr(env, X86Instr_SseReRg(Xsse_XOR, dst, dst));
//..    /* And now make it all 1s ... */
//..    addInstr(env, X86Instr_Sse32Fx4(Xsse_CMPEQF, dst, dst));
//..    /* Finally, xor 'src' into it. */
//..    addInstr(env, X86Instr_SseReRg(Xsse_XOR, src, dst));
//..    return dst;
//.. }


//.. /* Round an x87 FPU value to 53-bit-mantissa precision, to be used
//..    after most non-simple FPU operations (simple = +, -, *, / and
//..    sqrt).
//.. 
//..    This could be done a lot more efficiently if needed, by loading
//..    zero and adding it to the value to be rounded (fldz ; faddp?).
//.. */
//.. static void roundToF64 ( ISelEnv* env, HReg reg )
//.. {
//..    X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());
//..    sub_from_esp(env, 8);
//..    addInstr(env, X86Instr_FpLdSt(False/*store*/, 8, reg, zero_esp));
//..    addInstr(env, X86Instr_FpLdSt(True/*load*/, 8, reg, zero_esp));
//..    add_to_esp(env, 8);
//.. }

/*
  Generates code for AvSplat
  - takes in IRExpr* of type 8|16|32
    returns vector reg of duplicated lanes of input
  - uses AvSplat(imm) for imms up to simm6.
    otherwise must use store reg & load vector
*/
static HReg mk_AvDuplicateRI( ISelEnv* env, IRExpr* e )
{
   HReg     r_src;
   HReg     dst = newVRegV(env);
   PPC32RI* ri  = iselIntExpr_RI(env, e);
   IRType   ty  = typeOfIRExpr(env->type_env,e);
   UInt     sz  = (ty == Ity_I8) ? 8 : (ty == Ity_I16) ? 16 : 32;
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);

   /* special case: immediate */
   if (ri->tag == Pri_Imm) {
      Int simm32 = (Int)ri->Pri.Imm;

      /* figure out if it's do-able with imm splats. */
      if (simm32 >= -32 && simm32 <= 31) {
         Char simm6 = (Char)simm32;
         if (simm6 > 15) {           /* 16:31 inclusive */
            HReg v1 = newVRegV(env);
            HReg v2 = newVRegV(env);
            addInstr(env, PPC32Instr_AvSplat(sz, v1, PPC32VI5s_Imm(-16)));
            addInstr(env, PPC32Instr_AvSplat(sz, v2, PPC32VI5s_Imm(simm6-16)));
            addInstr(env,
               (sz== 8) ? PPC32Instr_AvBin8x16(Pav_SUBU, dst, v2, v1) :
               (sz==16) ? PPC32Instr_AvBin16x8(Pav_SUBU, dst, v2, v1)
                        : PPC32Instr_AvBin32x4(Pav_SUBU, dst, v2, v1) );
            return dst;
         }
         if (simm6 < -16) {          /* -32:-17 inclusive */
            HReg v1 = newVRegV(env);
            HReg v2 = newVRegV(env);
            addInstr(env, PPC32Instr_AvSplat(sz, v1, PPC32VI5s_Imm(-16)));
            addInstr(env, PPC32Instr_AvSplat(sz, v2, PPC32VI5s_Imm(simm6+16)));
            addInstr(env,
               (sz== 8) ? PPC32Instr_AvBin8x16(Pav_ADDU, dst, v2, v1) :
               (sz==16) ? PPC32Instr_AvBin16x8(Pav_ADDU, dst, v2, v1)
                        : PPC32Instr_AvBin32x4(Pav_ADDU, dst, v2, v1) );
            return dst;
         }
         /* simplest form:              -16:15 inclusive */
         addInstr(env, PPC32Instr_AvSplat(sz, dst, PPC32VI5s_Imm(simm6)));
         return dst;
      }

      /* no luck; use the Slow way. */
      r_src = newVRegI(env);
      addInstr(env, PPC32Instr_LI32(r_src, (UInt)simm32));
   }
   else {
      r_src = ri->Pri.Reg;
   }

   /* default case: store r_src in lowest lane of 16-aligned mem,
      load vector, splat lowest lane to dst */
   {
      /* CAB: Perhaps faster to store r_src multiple times (sz dependent),
              and simply load the vector? */
      HReg r_aligned16;
      HReg v_src = newVRegV(env);
      PPC32AMode *am_off12;

      sub_from_sp( env, 32 );     // Move SP down
      /* Get a 16-aligned address within our stack space */
      r_aligned16 = get_sp_aligned16( env );
      am_off12 = PPC32AMode_IR( 12, r_aligned16);

      /* Store r_src in low word of 16-aligned mem */
      addInstr(env, PPC32Instr_Store( 4, am_off12, r_src ));

      /* Load src to vector[low lane] */
      addInstr(env, PPC32Instr_AvLdSt( True/*load*/, 4, v_src, am_off12 ));
      add_to_sp( env, 32 );       // Reset SP

      /* Finally, splat v_src[low_lane] to dst */
      addInstr(env, PPC32Instr_AvSplat(sz, dst, PPC32VI5s_Reg(v_src)));
      return dst;
   }
}


/* for each lane of vSrc: lane == nan ? laneX = all 1's : all 0's */
static HReg isNan ( ISelEnv* env, HReg vSrc )
{
   HReg zeros, msk_exp, msk_mnt, expt, mnts, vIsNan;
 
   vassert(hregClass(vSrc) == HRcVec128);

   zeros   = mk_AvDuplicateRI(env, mkU32(0));
   msk_exp = mk_AvDuplicateRI(env, mkU32(0x7F800000));
   msk_mnt = mk_AvDuplicateRI(env, mkU32(0x7FFFFF));
   expt    = newVRegV(env);
   mnts    = newVRegV(env);
   vIsNan  = newVRegV(env); 

   /* 32bit float => sign(1) | expontent(8) | mantissa(23)
      nan => exponent all ones, mantissa > 0 */

   addInstr(env, PPC32Instr_AvBinary(Pav_AND, expt, vSrc, msk_exp));
   addInstr(env, PPC32Instr_AvBin32x4(Pav_CMPEQU, expt, expt, msk_exp));
   addInstr(env, PPC32Instr_AvBinary(Pav_AND, mnts, vSrc, msk_mnt));
   addInstr(env, PPC32Instr_AvBin32x4(Pav_CMPGTU, mnts, mnts, zeros));
   addInstr(env, PPC32Instr_AvBinary(Pav_AND, vIsNan, expt, mnts));
   return vIsNan;
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
   MatchInfo mi;
   DECLARE_PATTERN(p_32to1_then_1Uto8);

   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || Ity_I16 || Ity_I8);

   switch (e->tag) {

   /* --------- TEMP --------- */
   case Iex_Tmp:
      return lookupIRTemp(env, e->Iex.Tmp.tmp);

   /* --------- LOAD --------- */
   case Iex_Load: {
      HReg        r_dst   = newVRegI(env);
      PPC32AMode* am_addr = iselIntExpr_AMode(env, e->Iex.Load.addr);
      if (e->Iex.Load.end != Iend_BE)
         goto irreducible;
      if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32) {
         addInstr(env, PPC32Instr_Load( toUChar(sizeofIRType(ty)), 
                                        False, r_dst, am_addr ));
         return r_dst;
      }
      break;
   }

   /* --------- BINARY OP --------- */
   case Iex_Binop: {
      PPC32AluOp   aluOp;

//..       /* Pattern: Sub32(0,x) */
//..       if (e->Iex.Binop.op == Iop_Sub32 && isZero32(e->Iex.Binop.arg1)) {
//..          HReg dst = newVRegI(env);
//..          HReg reg = iselIntExpr_R(env, e->Iex.Binop.arg2);
//..          addInstr(env, mk_iMOVsd_RR(reg,dst));
//..          addInstr(env, PPC32Instr_Unary32(Xun_NEG,PPC32RM_Reg(dst)));
//..          return dst;
//..       }

      /* Is it an addition or logical style op? */
      switch (e->Iex.Binop.op) {
         case Iop_Add8:  case Iop_Add16: case Iop_Add32:
            aluOp = Palu_ADD; break;
         case Iop_Sub8:  case Iop_Sub16: case Iop_Sub32:
            aluOp = Palu_SUB; break;
         case Iop_And8:  case Iop_And16: case Iop_And32: 
            aluOp = Palu_AND; break;
         case Iop_Or8:   case Iop_Or16:  case Iop_Or32:  
            aluOp = Palu_OR; break;
         case Iop_Xor8:  case Iop_Xor16: case Iop_Xor32: 
            aluOp = Palu_XOR; break;
         case Iop_Shl32: case Iop_Shl16: case Iop_Shl8:
            aluOp = Palu_SHL; break;
         case Iop_Shr32: case Iop_Shr16: case Iop_Shr8: 
            aluOp = Palu_SHR; break;
         case Iop_Sar32: case Iop_Sar16: case Iop_Sar8: 
            aluOp = Palu_SAR; break;
         default:
            aluOp = Palu_INVALID; break;
      }
      /* For commutative ops we assume any literal
         values are on the second operand. */
      if (aluOp != Palu_INVALID) {
         HReg     r_dst, r_srcL;
         PPC32RH* ri_srcR = NULL;
         r_dst  = newVRegI(env);
         /* get left arg into a reg */
         r_srcL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         /* get right arg into an RH, in the appropriate way */
         switch (aluOp) {
            case Palu_ADD: case Palu_SUB:
               ri_srcR = iselIntExpr_RH(env, True/*signed*/, 
                                             e->Iex.Binop.arg2);
               break;
            case Palu_AND: case Palu_OR: case Palu_XOR:
               ri_srcR = iselIntExpr_RH(env, False/*signed*/,
                                             e->Iex.Binop.arg2);
               break;
            case Palu_SHL: case Palu_SHR: case Palu_SAR:
               ri_srcR = iselIntExpr_RH5u(env, e->Iex.Binop.arg2);
               break;
            default:
               vpanic("iselIntExpr_R_wrk-aluOp-arg2");
         }
	 /* widen the left arg if needed */
         if ((aluOp == Palu_SHR || aluOp == Palu_SAR)
             && (ty == Ity_I8 || ty == Ity_I16)) {
            PPC32RH* amt = PPC32RH_Imm(False, toUShort(ty == Ity_I8 ? 24 : 16));
            HReg     tmp = newVRegI(env);
            addInstr(env, PPC32Instr_Alu32(Palu_SHL, tmp, r_srcL, amt));
            addInstr(env, PPC32Instr_Alu32(aluOp,    tmp, tmp, amt));
            r_srcL = tmp;
            vassert(0); /* AWAITING TEST CASE */
	 }
         addInstr(env, PPC32Instr_Alu32(aluOp, r_dst, r_srcL, ri_srcR));
         return r_dst;
      }

      /* How about a div? */
      if (e->Iex.Binop.op == Iop_DivS32 || 
          e->Iex.Binop.op == Iop_DivU32) {
         HReg r_dst  = newVRegI(env);
         HReg r_srcL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg r_srcR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_Div(toBool(e->Iex.Binop.op == Iop_DivS32), 
                                      r_dst, r_srcL, r_srcR));
         return r_dst;
      }

      /* No? Anyone for a mul? */
      if (e->Iex.Binop.op == Iop_Mul16 ||
          e->Iex.Binop.op == Iop_Mul32) {
         Bool syned       = True;
         HReg r_dst       = newVRegI(env);
         HReg r_srcL      = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg r_srcR      = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_MulL(syned, False/*lo32*/, 
                                       r_dst, r_srcL, r_srcR));
         return r_dst;
      }      

      /* El-mutanto 3-way compare? */
      if (e->Iex.Binop.op == Iop_CmpORD32S
          || e->Iex.Binop.op == Iop_CmpORD32U) {
         Bool     syned = toBool(e->Iex.Binop.op == Iop_CmpORD32S);
         HReg     dst   = newVRegI(env);
         HReg     srcL  = iselIntExpr_R(env, e->Iex.Binop.arg1);
         PPC32RH* srcR  = iselIntExpr_RH(env, syned, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_Cmp32(syned, /*cr*/7, srcL, srcR));
         addInstr(env, PPC32Instr_MfCR(dst));
         addInstr(env, PPC32Instr_Alu32(Palu_AND, dst, dst,
                                        PPC32RH_Imm(False,7<<1)));
         return dst;
      }

//zz       /* Handle misc other ops. */
//zz       if (e->Iex.Binop.op == Iop_8HLto16) {
//zz          HReg hi8  = newVRegI(env);
//zz          HReg lo8  = newVRegI(env);
//zz          HReg hi8s = iselIntExpr_R(env, e->Iex.Binop.arg1);
//zz          HReg lo8s = iselIntExpr_R(env, e->Iex.Binop.arg2);
//zz          addInstr(env, 
//zz             PPC32Instr_Alu32(Palu_SHL, hi8, hi8s, PPC32RH_Imm(False,8)));
//zz          addInstr(env, 
//zz             PPC32Instr_Alu32(Palu_AND, lo8, lo8s, PPC32RH_Imm(False,0xFF)));
//zz          addInstr(env, 
//zz             PPC32Instr_Alu32(Palu_OR, hi8, hi8, PPC32RI_Reg(lo8)));
//zz          return hi8;
//zz       }
//zz 
//zz       if (e->Iex.Binop.op == Iop_16HLto32) {
//zz          HReg hi16  = newVRegI(env);
//zz          HReg lo16  = newVRegI(env);
//zz          HReg hi16s = iselIntExpr_R(env, e->Iex.Binop.arg1);
//zz          HReg lo16s = iselIntExpr_R(env, e->Iex.Binop.arg2);
//zz          addInstr(env, mk_sh32(env, Psh_SHL, hi16, hi16s, PPC32RI_Imm(16)));
//zz          addInstr(env, PPC32Instr_Alu32(Palu_AND, lo16, lo16s, PPC32RI_Imm(0xFFFF)));
//zz          addInstr(env, PPC32Instr_Alu32(Palu_OR, hi16, hi16, PPC32RI_Reg(lo16)));
//zz          return hi16;
//zz       }

//..       if (e->Iex.Binop.op == Iop_MullS16 || e->Iex.Binop.op == Iop_MullS8
//..           || e->Iex.Binop.op == Iop_MullU16 || e->Iex.Binop.op == Iop_MullU8) {
//..          HReg a16   = newVRegI(env);
//..          HReg b16   = newVRegI(env);
//..          HReg a16s  = iselIntExpr_R(env, e->Iex.Binop.arg1);
//..          HReg b16s  = iselIntExpr_R(env, e->Iex.Binop.arg2);
//..          Int  shift = (e->Iex.Binop.op == Iop_MullS8 
//..                        || e->Iex.Binop.op == Iop_MullU8)
//..                          ? 24 : 16;
//..          X86ShiftOp shr_op = (e->Iex.Binop.op == Iop_MullS8 
//..                               || e->Iex.Binop.op == Iop_MullS16)
//..                                 ? Xsh_SAR : Xsh_SHR;
//.. 
//..          addInstr(env, mk_iMOVsd_RR(a16s, a16));
//..          addInstr(env, mk_iMOVsd_RR(b16s, b16));
//..          addInstr(env, X86Instr_Sh32(Xsh_SHL, shift, X86RM_Reg(a16)));
//..          addInstr(env, X86Instr_Sh32(Xsh_SHL, shift, X86RM_Reg(b16)));
//..          addInstr(env, X86Instr_Sh32(shr_op,  shift, X86RM_Reg(a16)));
//..          addInstr(env, X86Instr_Sh32(shr_op,  shift, X86RM_Reg(b16)));
//..          addInstr(env, X86Instr_Alu32R(Xalu_MUL, X86RMI_Reg(a16), b16));
//..          return b16;
//..       }

      if (e->Iex.Binop.op == Iop_CmpF64) {
         HReg fr_srcL    = iselDblExpr(env, e->Iex.Binop.arg1);
         HReg fr_srcR    = iselDblExpr(env, e->Iex.Binop.arg2);

         HReg r_ccPPC32 = newVRegI(env);
         HReg r_ccIR    = newVRegI(env);
         HReg r_ccIR_b0 = newVRegI(env);
         HReg r_ccIR_b2 = newVRegI(env);
         HReg r_ccIR_b6 = newVRegI(env);

         addInstr(env, PPC32Instr_FpCmp(r_ccPPC32, fr_srcL, fr_srcR));

         /* Map compare result from PPC32 to IR,
            conforming to CmpF64 definition. */
         /*
           FP cmp result | PPC | IR
           --------------------------
           UN            | 0x1 | 0x45
           EQ            | 0x2 | 0x40
           GT            | 0x4 | 0x00
           LT            | 0x8 | 0x01
         */

         // r_ccIR_b0 = r_ccPPC32[0] | r_ccPPC32[3]
         addInstr(env, PPC32Instr_Alu32(Palu_SHR, r_ccIR_b0, r_ccPPC32, PPC32RH_Imm(False,0x3)));
         addInstr(env, PPC32Instr_Alu32(Palu_OR,  r_ccIR_b0, r_ccPPC32, PPC32RH_Reg(r_ccIR_b0)));
         addInstr(env, PPC32Instr_Alu32(Palu_AND, r_ccIR_b0, r_ccIR_b0, PPC32RH_Imm(False,0x1)));
         
         // r_ccIR_b2 = r_ccPPC32[0]
         addInstr(env, PPC32Instr_Alu32(Palu_SHL, r_ccIR_b2, r_ccPPC32, PPC32RH_Imm(False,0x2)));
         addInstr(env, PPC32Instr_Alu32(Palu_AND, r_ccIR_b2, r_ccIR_b2, PPC32RH_Imm(False,0x4)));

         // r_ccIR_b6 = r_ccPPC32[0] | r_ccPPC32[1]
         addInstr(env, PPC32Instr_Alu32(Palu_SHR, r_ccIR_b6, r_ccPPC32, PPC32RH_Imm(False,0x1)));
         addInstr(env, PPC32Instr_Alu32(Palu_OR,  r_ccIR_b6, r_ccPPC32, PPC32RH_Reg(r_ccIR_b6)));
         addInstr(env, PPC32Instr_Alu32(Palu_SHL, r_ccIR_b6, r_ccIR_b6, PPC32RH_Imm(False,0x6)));
         addInstr(env, PPC32Instr_Alu32(Palu_AND, r_ccIR_b6, r_ccIR_b6, PPC32RH_Imm(False,0x40)));

         // r_ccIR = r_ccIR_b0 | r_ccIR_b2 | r_ccIR_b6
         addInstr(env, PPC32Instr_Alu32(Palu_OR, r_ccIR, r_ccIR_b0, PPC32RH_Reg(r_ccIR_b2)));
         addInstr(env, PPC32Instr_Alu32(Palu_OR, r_ccIR, r_ccIR,    PPC32RH_Reg(r_ccIR_b6)));
         return r_ccIR;
      }

      if (e->Iex.Binop.op == Iop_F64toI32) {
         HReg fr_src = iselDblExpr(env, e->Iex.Binop.arg2);
         HReg r_dst = newVRegI(env);         
         /* Set host rounding mode */
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

         sub_from_sp( env, 16 );
         addInstr(env, PPC32Instr_FpF64toI32(r_dst, fr_src));
         add_to_sp( env, 16 );

         /* Restore default FPU rounding. */
         set_FPU_rounding_default( env );
         return r_dst;
      }


//..       /* C3210 flags following FPU partial remainder (fprem), both
//..          IEEE compliant (PREM1) and non-IEEE compliant (PREM). */
//..       if (e->Iex.Binop.op == Iop_PRemC3210F64
//..           || e->Iex.Binop.op == Iop_PRem1C3210F64) {
//..          HReg junk = newVRegF(env);
//..          HReg dst  = newVRegI(env);
//..          HReg srcL = iselDblExpr(env, e->Iex.Binop.arg1);
//..          HReg srcR = iselDblExpr(env, e->Iex.Binop.arg2);
//..          addInstr(env, X86Instr_FpBinary(
//..                            e->Iex.Binop.op==Iop_PRemC3210F64 
//..                               ? Xfp_PREM : Xfp_PREM1,
//..                            srcL,srcR,junk
//..                  ));
//..          /* The previous pseudo-insn will have left the FPU's C3210
//..             flags set correctly.  So bag them. */
//..          addInstr(env, X86Instr_FpStSW_AX());
//..          addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), dst));
//..          addInstr(env, X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(0x4700), dst));
//..          return dst;
//..       }

      break;
   }

   /* --------- UNARY OP --------- */
   case Iex_Unop: {
      /* 1Uto8(32to1(expr32)) */
      DEFINE_PATTERN(p_32to1_then_1Uto8,
                     unop(Iop_1Uto8,unop(Iop_32to1,bind(0))));
      if (matchIRExpr(&mi,p_32to1_then_1Uto8,e)) {
         IRExpr* expr32 = mi.bindee[0];
         HReg r_dst = newVRegI(env);
         HReg r_src = iselIntExpr_R(env, expr32);
         addInstr(env, PPC32Instr_Alu32(Palu_AND, r_dst, r_src, PPC32RH_Imm(False,1)));
         return r_dst;
      }

      /* 16Uto32(LDbe:I16(expr32)) */
      {
         DECLARE_PATTERN(p_LDbe16_then_16Uto32);
         DEFINE_PATTERN(p_LDbe16_then_16Uto32,
                        unop(Iop_16Uto32,
                             IRExpr_Load(Iend_BE,Ity_I16,bind(0))) );
         if (matchIRExpr(&mi,p_LDbe16_then_16Uto32,e)) {
            HReg r_dst = newVRegI(env);
            PPC32AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
            addInstr(env, PPC32Instr_Load(2,False,r_dst,amode));
            return r_dst;
         }
      }

      switch (e->Iex.Unop.op) {
      case Iop_8Uto16:
      case Iop_8Uto32:
      case Iop_16Uto32: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselIntExpr_R(env, e->Iex.Unop.arg);
         UShort mask  = toUShort(e->Iex.Unop.op==Iop_16Uto32 ? 0xFFFF : 0xFF);
         addInstr(env, PPC32Instr_Alu32(Palu_AND,r_dst,r_src,
                                                 PPC32RH_Imm(False,mask)));
         return r_dst;
      }
      case Iop_8Sto16:
      case Iop_8Sto32:
      case Iop_16Sto32: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselIntExpr_R(env, e->Iex.Unop.arg);
         UShort amt   = toUShort(e->Iex.Unop.op==Iop_16Sto32 ? 16 : 24);
         addInstr(env, PPC32Instr_Alu32(Palu_SHL, r_dst, r_src, 
                                                  PPC32RH_Imm(False,amt)));
         addInstr(env, PPC32Instr_Alu32(Palu_SAR, r_dst, r_dst, 
                                                  PPC32RH_Imm(False,amt)));
         return r_dst;
      }
      case Iop_Not8:
      case Iop_Not16:
      case Iop_Not32: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPC32Instr_Unary32(Pun_NOT,r_dst,r_src));
         return r_dst;
      }
      case Iop_64HIto32: {
         HReg rHi, rLo;
         iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
         return rHi; /* and abandon rLo .. poor wee thing :-) */
      }
      case Iop_64to32: {

//::          /* 64to32(MullS32(expr,expr)) */
//::          {
//::             DECLARE_PATTERN(p_MullS32_then_64to32);
//::             DEFINE_PATTERN(p_MullS32_then_64to32,
//::                            unop(Iop_64to32,
//::                                 binop(Iop_MullS32, bind(0), bind(1))));
//::             if (matchIRExpr(&mi,p_MullS32_then_64to32,e)) {
//::                HReg r_dst         = newVRegI(env);
//::                HReg r_srcL      = iselIntExpr_R( env, mi.bindee[0] );
//::                PPC32RI* ri_srcR = mk_FitRI16_S(env, iselIntExpr_RI( env, mi.bindee[1] ));
//::                addInstr(env, PPC32Instr_MulL(True, 0, r_dst, r_srcL, ri_srcR));
//::                return r_dst;
//::             }
//::          }
//:: 
//::          /* 64to32(MullU32(expr,expr)) */
//::          {
//::             DECLARE_PATTERN(p_MullU32_then_64to32);
//::             DEFINE_PATTERN(p_MullU32_then_64to32,
//::                            unop(Iop_64to32,
//::                                 binop(Iop_MullU32, bind(0), bind(1))));
//::             if (matchIRExpr(&mi,p_MullU32_then_64to32,e)) {
//::                HReg r_dst         = newVRegI(env);
//::                HReg r_srcL      = iselIntExpr_R( env, mi.bindee[0] );
//::                PPC32RI* ri_srcR = mk_FitRI16_S(env, iselIntExpr_RI( env, mi.bindee[1] ));
//::                addInstr(env, PPC32Instr_MulL(False, 0, r_dst, r_srcL, ri_srcR));
//::                return r_dst;
//::             }
//::          }
//:: 
//::          // CAB: Also: 64HIto32(MullU32(expr,expr))
//::          // CAB: Also: 64HIto32(MullS32(expr,expr))

         HReg rHi, rLo;
         iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
         return rLo; /* similar stupid comment to the above ... */
      }
      case Iop_16HIto8:
      case Iop_32HIto16: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselIntExpr_R(env, e->Iex.Unop.arg);
         UShort shift = toUShort(e->Iex.Unop.op == Iop_16HIto8 ? 8 : 16);
         addInstr(env, PPC32Instr_Alu32(Palu_SHR, r_dst, r_src, 
                                                  PPC32RH_Imm(False,shift)));
         return r_dst;
      }
      case Iop_1Uto32:
      case Iop_1Uto8: {
         HReg          r_dst = newVRegI(env);
         PPC32CondCode cond  = iselCondCode(env, e->Iex.Unop.arg);
         addInstr(env, PPC32Instr_Set32(cond,r_dst));
         return r_dst;
      }
      case Iop_1Sto8:
      case Iop_1Sto16:
      case Iop_1Sto32: {
         /* could do better than this, but for now ... */
         HReg          r_dst = newVRegI(env);
         PPC32CondCode cond  = iselCondCode(env, e->Iex.Unop.arg);
         addInstr(env, PPC32Instr_Set32(cond,r_dst));
         addInstr(env, PPC32Instr_Alu32(Palu_SHL, r_dst, r_dst, 
                                                  PPC32RH_Imm(False,31)));
         addInstr(env, PPC32Instr_Alu32(Palu_SAR, r_dst, r_dst, 
                                                  PPC32RH_Imm(False,31)));
         return r_dst;
      }

//..          case Iop_Ctz32: {
//..             /* Count trailing zeroes, implemented by x86 'bsfl' */
//..             HReg dst = newVRegI(env);
//..             HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
//..             addInstr(env, X86Instr_Bsfr32(True,src,dst));
//..             return dst;
//..          }
      case Iop_Clz32: {
         /* Count leading zeroes. */
         HReg r_dst = newVRegI(env);
         HReg r_src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPC32Instr_Unary32(Pun_CLZ,r_dst,r_src));
         return r_dst;
      }
      case Iop_Neg8:
      case Iop_Neg16:
      case Iop_Neg32: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPC32Instr_Unary32(Pun_NEG,r_dst,r_src));
         return r_dst;
      }

      case Iop_V128to32: {
         HReg        r_aligned16;
         HReg        dst  = newVRegI(env);
         HReg        vec  = iselVecExpr(env, e->Iex.Unop.arg);
         PPC32AMode *am_off0, *am_off12;
         sub_from_sp( env, 32 );     // Move SP down 32 bytes

         // get a quadword aligned address within our stack space
         r_aligned16 = get_sp_aligned16( env );
         am_off0  = PPC32AMode_IR( 0, r_aligned16 );
         am_off12 = PPC32AMode_IR( 12,r_aligned16 );

         // store vec, load low word to dst
         addInstr(env, PPC32Instr_AvLdSt( False/*store*/, 16, vec, am_off0 ));
         addInstr(env, PPC32Instr_Load( 4, False, dst, am_off12 ));

         add_to_sp( env, 32 );       // Reset SP
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
      if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32) {
         HReg r_dst = newVRegI(env);
         PPC32AMode* am_addr = PPC32AMode_IR(e->Iex.Get.offset, GuestStatePtr );
         addInstr(env, PPC32Instr_Load( toUChar(sizeofIRType(ty)), 
                                        False, r_dst, am_addr ));
         return r_dst;
      }
      break;
   }

//..    case Iex_GetI: {
//..       X86AMode* am 
//..          = genGuestArrayOffset(
//..               env, e->Iex.GetI.descr, 
//..                    e->Iex.GetI.ix, e->Iex.GetI.bias );
//..       HReg dst = newVRegI(env);
//..       if (ty == Ity_I8) {
//..          addInstr(env, X86Instr_Load( 1, False, am, dst ));
//..          return dst;
//..       }
//..       break;
//..    }

   /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg    r_dst = newVRegI(env);
      vassert(ty == Ity_I32);

      /* be very restrictive for now.  Only 32/64-bit ints allowed
         for args, and 32 bits for return type. */
      if (e->Iex.CCall.retty != Ity_I32)
         goto irreducible;
      
      /* Marshal args, do the call, clear stack. */
      doHelperCall( env, False, NULL, e->Iex.CCall.cee, e->Iex.CCall.args );

      /* GPR3 now holds the destination address from Pin_Goto */
      addInstr(env, mk_iMOVds_RR(r_dst, hregPPC32_GPR3()));
      return r_dst;
   }
      
   /* --------- LITERAL --------- */
   /* 32/16/8-bit literals */
   case Iex_Const: {
      Int  i;
      HReg r_dst = newVRegI(env);
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: i = (Int)e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: i = (Int)(Short)e->Iex.Const.con->Ico.U16; break;
         case Ico_U8:  i = (Int)(Char)e->Iex.Const.con->Ico.U8; break;
         default:      vpanic("iselIntExpr_R.const(ppc32)");
      }
      addInstr(env, PPC32Instr_LI32(r_dst, (UInt)i));
      return r_dst;
   }

   /* --------- MULTIPLEX --------- */
   case Iex_Mux0X: {
      if ((ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8)
          && typeOfIRExpr(env->type_env,e->Iex.Mux0X.cond) == Ity_I8) {
         PPC32CondCode cc = mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
         HReg     r_cond = iselIntExpr_R(env, e->Iex.Mux0X.cond);
         HReg     rX     = iselIntExpr_R(env, e->Iex.Mux0X.exprX);
         PPC32RI* r0     = iselIntExpr_RI(env, e->Iex.Mux0X.expr0);
         HReg     r_dst  = newVRegI(env);
         HReg     r_tmp  = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(r_dst,rX));
         addInstr(env, PPC32Instr_Alu32(Palu_AND, r_tmp, r_cond, PPC32RH_Imm(False,0xFF)));
         addInstr(env, PPC32Instr_Cmp32(False/*unsigned*/, 7/*cr*/, r_tmp, PPC32RH_Imm(False,0)));
         addInstr(env, PPC32Instr_CMov32(cc,r_dst,r0));
         return r_dst;
      }
      break;
   }
      
   default: 
      break;
   } /* switch (e->tag) */


   /* We get here if no pattern matched. */
 irreducible:
   ppIRExpr(e);
   vpanic("iselIntExpr_R(ppc32): cannot reduce tree");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expression auxiliaries              ---*/
/*---------------------------------------------------------*/

/* --------------------- AMODEs --------------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a 32-bit one.
*/

static Bool fits16bits ( UInt u ) 
{
   /* Is u the same as the sign-extend of its lower 16 bits? */
   Int i = u & 0xFFFF;
   i <<= 16;
   i >>= 16;
   return toBool(u == (UInt)i);
}

static Bool sane_AMode ( PPC32AMode* am )
{
   switch (am->tag) {
      case Pam_IR:
         return toBool(
                   hregClass(am->Pam.IR.base) == HRcInt32
                   && hregIsVirtual(am->Pam.IR.base)
                   && fits16bits(am->Pam.IR.index)
                );
      case Pam_RR:
         return toBool(
                   hregClass(am->Pam.RR.base) == HRcInt32
                   && hregIsVirtual(am->Pam.IR.base)
                   && hregClass(am->Pam.RR.base) == HRcInt32
                   && hregIsVirtual(am->Pam.IR.base)
                );
      default:
         vpanic("sane_AMode: unknown ppc32 amode tag");
   }
}

static PPC32AMode* iselIntExpr_AMode ( ISelEnv* env, IRExpr* e )
{
   PPC32AMode* am = iselIntExpr_AMode_wrk(env, e);
   vassert(sane_AMode(am));
   return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPC32AMode* iselIntExpr_AMode_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32);
   
   /* Add32(expr,i), where i == sign-extend of (i & 0xFFFF) */
   if (e->tag == Iex_Binop 
       && e->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32
       && fits16bits(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)) {
      return PPC32AMode_IR(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32,
                           iselIntExpr_R(env, e->Iex.Binop.arg1));
   }
      
   /* Add32(expr,expr) */
   if (e->tag == Iex_Binop 
       && e->Iex.Binop.op == Iop_Add32) {
      HReg r_base = iselIntExpr_R(env,  e->Iex.Binop.arg1);
      HReg r_idx  = iselIntExpr_R(env,  e->Iex.Binop.arg2);
      return PPC32AMode_RR(r_idx, r_base);
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg r1 = iselIntExpr_R(env, e);
      return PPC32AMode_IR(0, r1);
   }
}


/* --------------------- RH --------------------- */

/* Compute an I8/I16/I32 into a RH (reg-or-halfword-immediate).  It's
   important to specify whether the immediate is to be regarded as
   signed or not.  If yes, this will never return -32768 as an
   immediate; this guaranteed that all signed immediates that are
   return can have their sign inverted if need be. */

static PPC32RH* iselIntExpr_RH ( ISelEnv* env, Bool syned, IRExpr* e )
{
   PPC32RH* ri = iselIntExpr_RH_wrk(env, syned, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Prh_Imm:
         vassert(ri->Prh.Imm.syned == syned);
         if (syned)
            vassert(ri->Prh.Imm.imm16 != 0x8000);
         return ri;
      case Prh_Reg:
         vassert(hregClass(ri->Prh.Reg.reg) == HRcInt32);
         vassert(hregIsVirtual(ri->Prh.Reg.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RH: unknown ppc32 RH tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPC32RH* iselIntExpr_RH_wrk ( ISelEnv* env, Bool syned, IRExpr* e )
{
   UInt u;
   Int  i;
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      /* What value are we aiming to generate? */
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & e->Iex.Const.con->Ico.U16; break;
         case Ico_U8:  u = 0xFF & e->Iex.Const.con->Ico.U8; break;
         default:      vpanic("iselIntExpr_RH.Iex_Const(ppc32h)");
      }
      i = (Int)u;
      /* Now figure out if it's representable. */
      if (!syned && u <= 65535) {
         return PPC32RH_Imm(False/*unsigned*/, toUShort(u & 0xFFFF));
      }
      if (syned && i >= -32767 && i <= 32767) {
         return PPC32RH_Imm(True/*signed*/, toUShort(u & 0xFFFF));
      }
      /* no luck; use the Slow Way. */
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return PPC32RH_Reg(r);
   }
}


/* --------------------- RIs --------------------- */

/* Calculate an expression into an PPC32RI operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits. */

static PPC32RI* iselIntExpr_RI ( ISelEnv* env, IRExpr* e )
{
   PPC32RI* ri = iselIntExpr_RI_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Pri_Imm:
         return ri;
      case Pri_Reg:
         vassert(hregClass(ri->Pri.Reg) == HRcInt32);
         vassert(hregIsVirtual(ri->Pri.Reg));
         return ri;
      default:
         vpanic("iselIntExpr_RI: unknown ppc32 RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPC32RI* iselIntExpr_RI_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      UInt u;
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u =          e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & e->Iex.Const.con->Ico.U16; break;
         case Ico_U8:  u = 0xFF   & e->Iex.Const.con->Ico.U8;  break;
         default:      vpanic("iselIntExpr_RI.Iex_Const(ppc32h)");
      }
      return PPC32RI_Imm(u);
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return PPC32RI_Reg(r);
   }
}


/* --------------------- RH5u --------------------- */

/* Compute an I8 into a reg-or-5-bit-unsigned-immediate, the latter
   being an immediate in the range 1 .. 31 inclusive.  Used for doing
   shift amounts. */

static PPC32RH* iselIntExpr_RH5u ( ISelEnv* env, IRExpr* e )
{
   PPC32RH* ri = iselIntExpr_RH5u_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Prh_Imm:
         vassert(ri->Prh.Imm.imm16 >= 1 && ri->Prh.Imm.imm16 <= 31);
         vassert(!ri->Prh.Imm.syned);
         return ri;
      case Prh_Reg:
         vassert(hregClass(ri->Prh.Reg.reg) == HRcInt32);
         vassert(hregIsVirtual(ri->Prh.Reg.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RH5u: unknown ppc32 RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPC32RH* iselIntExpr_RH5u_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const
       && e->Iex.Const.con->tag == Ico_U8
       && e->Iex.Const.con->Ico.U8 >= 1
       && e->Iex.Const.con->Ico.U8 <= 31) {
      return PPC32RH_Imm(False/*unsigned*/, e->Iex.Const.con->Ico.U8);
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return PPC32RH_Reg(r);
   }
}


/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static PPC32CondCode iselCondCode ( ISelEnv* env, IRExpr* e )
{
   /* Uh, there's nothing we can sanity check here, unfortunately. */
   return iselCondCode_wrk(env,e);
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPC32CondCode iselCondCode_wrk ( ISelEnv* env, IRExpr* e )
{
//   MatchInfo mi;
//   DECLARE_PATTERN(p_32to1);
//..    DECLARE_PATTERN(p_1Uto32_then_32to1);
//..    DECLARE_PATTERN(p_1Sto32_then_32to1);

   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I1);

   /* Constant 1:Bit */
   if (e->tag == Iex_Const && e->Iex.Const.con->Ico.U1 == True) {
      // Make a compare that will always be true:
      HReg r_zero = newVRegI(env);
      addInstr(env, PPC32Instr_LI32(r_zero, 0));
      addInstr(env, PPC32Instr_Cmp32(False/*unsigned*/, /*cr*/7, 
                                     r_zero, PPC32RH_Reg(r_zero)));
      return mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
   }

   /* Not1(...) */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_Not1) {
      /* Generate code for the arg, and negate the test condition */
      PPC32CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
      cond.test = invertCondTest(cond.test);
      return cond;
   }

   /* --- patterns rooted at: 32to1 --- */

//..    /* 32to1(1Uto32(expr1)) -- the casts are pointless, ignore them */
//..    DEFINE_PATTERN(p_1Uto32_then_32to1,
//..                   unop(Iop_32to1,unop(Iop_1Uto32,bind(0))));
//..    if (matchIRExpr(&mi,p_1Uto32_then_32to1,e)) {
//..       IRExpr* expr1 = mi.bindee[0];
//..       return iselCondCode(env, expr1);
//..    }
//.. 
//..    /* 32to1(1Sto32(expr1)) -- the casts are pointless, ignore them */
//..    DEFINE_PATTERN(p_1Sto32_then_32to1,
//..                   unop(Iop_32to1,unop(Iop_1Sto32,bind(0))));
//..    if (matchIRExpr(&mi,p_1Sto32_then_32to1,e)) {
//..       IRExpr* expr1 = mi.bindee[0];
//..       return iselCondCode(env, expr1);
//..    }

   /* 32to1 */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_32to1) {
      HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
      HReg tmp = newVRegI(env);
      /* could do better, probably -- andi. */
      addInstr(env, PPC32Instr_Alu32(
                       Palu_AND, tmp, src, PPC32RH_Imm(False,1)));
      addInstr(env, PPC32Instr_Cmp32(
                       False/*unsigned*/, 7/*cr*/, 
                       tmp, PPC32RH_Imm(False,1)));
      return mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
   }

   /* --- patterns rooted at: CmpNEZ8 --- */

   /* CmpNEZ8(x) */
   /* could do better -- andi. */
   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ8) {
      HReg r_32 = iselIntExpr_R(env, e->Iex.Unop.arg);
      HReg r_l  = newVRegI(env);
      addInstr(env, PPC32Instr_Alu32(Palu_AND, r_l, r_32, PPC32RH_Imm(False,0xFF)));
      addInstr(env, PPC32Instr_Cmp32(False/*unsigned*/, 7/*cr*/, 
r_l, PPC32RH_Imm(False,0)));
      return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
   }

   /* --- patterns rooted at: CmpNEZ32 --- */

   /* CmpNEZ32(x) */
   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ32) {
      HReg r1 = iselIntExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, PPC32Instr_Cmp32(False/*unsigned*/, 7, r1, PPC32RH_Imm(False,0)));
      return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
   }

   /* --- patterns rooted at: Cmp{EQ,NE}{8,16} --- */

//..    /* CmpEQ8 / CmpNE8 */
//..    if (e->tag == Iex_Binop 
//..        && (e->Iex.Binop.op == Iop_CmpEQ8
//..            || e->Iex.Binop.op == Iop_CmpNE8)) {
//..       HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
//..       X86RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
//..       HReg    r    = newVRegI(env);
//..       addInstr(env, mk_iMOVsd_RR(r1,r));
//..       addInstr(env, X86Instr_Alu32R(Xalu_XOR,rmi2,r));
//..       addInstr(env, X86Instr_Alu32R(Xalu_AND,X86RMI_Imm(0xFF),r));
//..       switch (e->Iex.Binop.op) {
//..          case Iop_CmpEQ8:  return Xcc_Z;
//..          case Iop_CmpNE8:  return Xcc_NZ;
//..          default: vpanic("iselCondCode(x86): CmpXX8");
//..       }
//..    }
//.. 
//..    /* CmpEQ16 / CmpNE16 */
//..    if (e->tag == Iex_Binop 
//..        && (e->Iex.Binop.op == Iop_CmpEQ16
//..            || e->Iex.Binop.op == Iop_CmpNE16)) {
//..       HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
//..       X86RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
//..       HReg    r    = newVRegI(env);
//..       addInstr(env, mk_iMOVsd_RR(r1,r));
//..       addInstr(env, X86Instr_Alu32R(Xalu_XOR,rmi2,r));
//..       addInstr(env, X86Instr_Alu32R(Xalu_AND,X86RMI_Imm(0xFFFF),r));
//..       switch (e->Iex.Binop.op) {
//..          case Iop_CmpEQ16:  return Xcc_Z;
//..          case Iop_CmpNE16:  return Xcc_NZ;
//..          default: vpanic("iselCondCode(x86): CmpXX16");
//..       }
//..    }
//.. 
//..    /* CmpNE32(1Sto32(b), 0) ==> b */
//..    {
//..       DECLARE_PATTERN(p_CmpNE32_1Sto32);
//..       DEFINE_PATTERN(
//..          p_CmpNE32_1Sto32,
//..          binop(Iop_CmpNE32, unop(Iop_1Sto32,bind(0)), mkU32(0)));
//..       if (matchIRExpr(&mi, p_CmpNE32_1Sto32, e)) {
//..          return iselCondCode(env, mi.bindee[0]);
//..       }
//..    }

   /* Cmp*32*(x,y) */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ32
           || e->Iex.Binop.op == Iop_CmpNE32
           || e->Iex.Binop.op == Iop_CmpLT32S
           || e->Iex.Binop.op == Iop_CmpLT32U
           || e->Iex.Binop.op == Iop_CmpLE32S
           || e->Iex.Binop.op == Iop_CmpLE32U)) {
      PPC32RH* ri2;
      HReg r1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
      Bool syned = False;
      if (e->Iex.Binop.op == Iop_CmpLT32S ||
          e->Iex.Binop.op == Iop_CmpLE32S) {
         syned = True;
      }
      ri2 = iselIntExpr_RH(env, syned, e->Iex.Binop.arg2);
      addInstr(env, PPC32Instr_Cmp32(syned,7,r1,ri2));

      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ32:  return mk_PPCCondCode( Pct_TRUE,  Pcf_7EQ );
         case Iop_CmpNE32:  return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
//      case Iop_CmpLT32S: return mk_PPCCondCode( Pct_TRUE,  Pcf_LT );
         case Iop_CmpLT32U: return mk_PPCCondCode( Pct_TRUE,  Pcf_7LT );
//      case Iop_CmpLE32S: return mk_PPCCondCode( Pct_FALSE, Pcf_GT );
         case Iop_CmpLE32U: return mk_PPCCondCode( Pct_FALSE, Pcf_7GT );
      default: vpanic("iselCondCode(ppc32): CmpXX32");
      }
   }

//..    /* CmpNE64(1Sto64(b), 0) ==> b */
//..    {
//..       DECLARE_PATTERN(p_CmpNE64_1Sto64);
//..       DEFINE_PATTERN(
//..          p_CmpNE64_1Sto64,
//..          binop(Iop_CmpNE64, unop(Iop_1Sto64,bind(0)), mkU64(0)));
//..       if (matchIRExpr(&mi, p_CmpNE64_1Sto64, e)) {
//..          return iselCondCode(env, mi.bindee[0]);
//..       }
//..    }
//.. 
//..    /* CmpNE64(x, 0) */
//..    {
//..       DECLARE_PATTERN(p_CmpNE64_x_zero);
//..       DEFINE_PATTERN(
//..          p_CmpNE64_x_zero,
//..          binop(Iop_CmpNE64, bind(0), mkU64(0)) );
//..       if (matchIRExpr(&mi, p_CmpNE64_x_zero, e)) {
//..          HReg hi, lo;
//..          IRExpr* x   = mi.bindee[0];
//..          HReg    tmp = newVRegI(env);
//..          iselInt64Expr( &hi, &lo, env, x );
//..          addInstr(env, mk_iMOVsd_RR(hi, tmp));
//..          addInstr(env, X86Instr_Alu32R(Xalu_OR,X86RMI_Reg(lo), tmp));
//..          return Xcc_NZ;
//..       }
//..    }
//.. 
//..    /* CmpNE64 */
//..    if (e->tag == Iex_Binop 
//..        && e->Iex.Binop.op == Iop_CmpNE64) {
//..       HReg hi1, hi2, lo1, lo2;
//..       HReg tHi = newVRegI(env);
//..       HReg tLo = newVRegI(env);
//..       iselInt64Expr( &hi1, &lo1, env, e->Iex.Binop.arg1 );
//..       iselInt64Expr( &hi2, &lo2, env, e->Iex.Binop.arg2 );
//..       addInstr(env, mk_iMOVsd_RR(hi1, tHi));
//..       addInstr(env, X86Instr_Alu32R(Xalu_XOR,X86RMI_Reg(hi2), tHi));
//..       addInstr(env, mk_iMOVsd_RR(lo1, tLo));
//..       addInstr(env, X86Instr_Alu32R(Xalu_XOR,X86RMI_Reg(lo2), tLo));
//..       addInstr(env, X86Instr_Alu32R(Xalu_OR,X86RMI_Reg(tHi), tLo));
//..       switch (e->Iex.Binop.op) {
//..          case Iop_CmpNE64:  return Xcc_NZ;
//..          default: vpanic("iselCondCode(x86): CmpXX64");
//..       }
//..    }


   /* CmpNEZ64 */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ64) {
      HReg hi, lo;
      HReg tmp = newVRegI(env);
      iselInt64Expr( &hi, &lo, env, e->Iex.Unop.arg );
      addInstr(env, mk_iMOVds_RR(tmp, lo));
      addInstr(env, PPC32Instr_Alu32(Palu_OR, tmp, tmp, PPC32RH_Reg(hi)));
      addInstr(env, PPC32Instr_Cmp32(False/*sign*/,7/*cr*/,tmp,PPC32RH_Imm(False,0)));
      return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
   }

   /* var */
   if (e->tag == Iex_Tmp) {
      HReg r_src      = lookupIRTemp(env, e->Iex.Tmp.tmp);
      HReg src_masked = newVRegI(env);
      addInstr(env, PPC32Instr_Alu32(Palu_AND, src_masked, r_src, PPC32RH_Imm(False,1)));
      addInstr(env, PPC32Instr_Cmp32(False/*unsigned*/, 7/*cr*/, src_masked, PPC32RH_Imm(False,1)));
      return mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
   }

   ppIRExpr(e);
   vpanic("iselCondCode(ppc32)");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64 bit)                ---*/
/*---------------------------------------------------------*/

/* Compute a 64-bit value into a register pair, which is returned as
   the first two parameters.  As with iselIntExpr_R, these may be
   either real or virtual regs; in any case they must not be changed
   by subsequent code emitted by the caller.  */

static void iselInt64Expr ( HReg* rHi, HReg* rLo, ISelEnv* env, IRExpr* e )
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
static void iselInt64Expr_wrk ( HReg* rHi, HReg* rLo, ISelEnv* env, IRExpr* e )
{
//   HWord fn = 0; /* helper fn for most SIMD64 stuff */
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I64);

   /* 64-bit literal */
   if (e->tag == Iex_Const) {
      ULong w64 = e->Iex.Const.con->Ico.U64;
      UInt  wHi = ((UInt)(w64 >> 32)) & 0xFFFFFFFF;
      UInt  wLo = ((UInt)w64) & 0xFFFFFFFF;
      HReg  tLo = newVRegI(env);
      HReg  tHi = newVRegI(env);
      vassert(e->Iex.Const.con->tag == Ico_U64);
      addInstr(env, PPC32Instr_LI32(tHi, wHi));
      addInstr(env, PPC32Instr_LI32(tLo, wLo));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* read 64-bit IRTemp */
   if (e->tag == Iex_Tmp) {
      lookupIRTemp64( rHi, rLo, env, e->Iex.Tmp.tmp);
      return;
   }

//..    /* 64-bit load */
//..    if (e->tag == Iex_LDle) {
//..       HReg     tLo, tHi;
//..       X86AMode *am0, *am4;
//..       vassert(e->Iex.LDle.ty == Ity_I64);
//..       tLo = newVRegI(env);
//..       tHi = newVRegI(env);
//..       am0 = iselIntExpr_AMode(env, e->Iex.LDle.addr);
//..       am4 = advance4(am0);
//..       addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am0), tLo ));
//..       addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am4), tHi ));
//..       *rHi = tHi;
//..       *rLo = tLo;
//..       return;
//..    }

   /* 64-bit GET */
   if (e->tag == Iex_Get) {
      PPC32AMode* am_addr = PPC32AMode_IR(e->Iex.Get.offset, GuestStatePtr );
      PPC32AMode* am_addr4 = advance4(env, am_addr);
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      addInstr(env, PPC32Instr_Load( 4, False, tHi, am_addr ));
      addInstr(env, PPC32Instr_Load( 4, False, tLo, am_addr4 ));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

//..    /* 64-bit GETI */
//..    if (e->tag == Iex_GetI) {
//..       X86AMode* am 
//..          = genGuestArrayOffset( env, e->Iex.GetI.descr, 
//..                                      e->Iex.GetI.ix, e->Iex.GetI.bias );
//..       X86AMode* am4 = advance4(am);
//..       HReg tLo = newVRegI(env);
//..       HReg tHi = newVRegI(env);
//..       addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am), tLo ));
//..       addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am4), tHi ));
//..       *rHi = tHi;
//..       *rLo = tLo;
//..       return;
//..    }

   /* 64-bit Mux0X */
   if (e->tag == Iex_Mux0X) {
      HReg e0Lo, e0Hi, eXLo, eXHi;
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);

      PPC32CondCode cc = mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
      HReg r_cond = iselIntExpr_R(env, e->Iex.Mux0X.cond);
      HReg r_tmp  = newVRegI(env);

      iselInt64Expr(&e0Hi, &e0Lo, env, e->Iex.Mux0X.expr0);
      iselInt64Expr(&eXHi, &eXLo, env, e->Iex.Mux0X.exprX);
      addInstr(env, mk_iMOVds_RR(tHi,eXHi));
      addInstr(env, mk_iMOVds_RR(tLo,eXLo));

      addInstr(env, PPC32Instr_Alu32(Palu_AND, 
                                     r_tmp, r_cond, PPC32RH_Imm(False,0xFF)));
      addInstr(env, PPC32Instr_Cmp32(False/*unsigned*/, 
                                     7/*cr*/, r_tmp, PPC32RH_Imm(False,0)));

      addInstr(env, PPC32Instr_CMov32(cc,tHi,PPC32RI_Reg(e0Hi)));
      addInstr(env, PPC32Instr_CMov32(cc,tLo,PPC32RI_Reg(e0Lo)));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
      /* 32 x 32 -> 64 multiply */
      case Iop_MullU32:
      case Iop_MullS32: {
         HReg     tLo     = newVRegI(env);
         HReg     tHi     = newVRegI(env);
         Bool     syned   = toBool(e->Iex.Binop.op == Iop_MullS32);
         HReg     r_srcL  = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg     r_srcR  = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_MulL(False/*signedness irrelevant*/, 
                                       False/*lo32*/, tLo, r_srcL, r_srcR));
         addInstr(env, PPC32Instr_MulL(syned,
                                       True/*hi32*/, tHi, r_srcL, r_srcR));
         *rHi = tHi;
         *rLo = tLo;
         return;
      }

//..          /* 64 x 32 -> (32(rem),32(div)) division */
//..          case Iop_DivModU64to32:
//..          case Iop_DivModS64to32: {
//..             /* Get the 64-bit operand into edx:eax, and the other into
//..                any old R/M. */
//..             HReg sHi, sLo;
//..             HReg   tLo     = newVRegI(env);
//..             HReg   tHi     = newVRegI(env);
//..             Bool   syned   = e->Iex.Binop.op == Iop_DivModS64to32;
//..             X86RM* rmRight = iselIntExpr_RM(env, e->Iex.Binop.arg2);
//..             iselInt64Expr(&sHi,&sLo, env, e->Iex.Binop.arg1);
//..             addInstr(env, mk_iMOVsd_RR(sHi, hregX86_EDX()));
//..             addInstr(env, mk_iMOVsd_RR(sLo, hregX86_EAX()));
//..             addInstr(env, X86Instr_Div(syned, Xss_32, rmRight));
//..             addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
//..             addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
//..             *rHi = tHi;
//..             *rLo = tLo;
//..             return;
//..          }

         /* Or64/And64/Xor64 */
         case Iop_Or64:
         case Iop_And64:
         case Iop_Xor64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            PPC32AluOp op = e->Iex.Binop.op==Iop_Or64 ? Palu_OR
                            : e->Iex.Binop.op==Iop_And64 ? Palu_AND
                            : Palu_XOR;
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, PPC32Instr_Alu32(op, tHi, xHi, PPC32RH_Reg(yHi)));
            addInstr(env, PPC32Instr_Alu32(op, tLo, xLo, PPC32RH_Reg(yLo)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* Add64/Sub64 */
         case Iop_Add64: {
//..          case Iop_Sub64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
//..             if (e->Iex.Binop.op==Iop_Add64) {
            addInstr(env, PPC32Instr_AddSubC32( True/*add*/, True /*set carry*/,
                                                tLo, xLo, yLo));
            addInstr(env, PPC32Instr_AddSubC32( True/*add*/, False/*read carry*/,
                                                tHi, xHi, yHi));
//..             } else { // Sub64
//..             }
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32HLto64(e1,e2) */
         case Iop_32HLto64:
            *rHi = iselIntExpr_R(env, e->Iex.Binop.arg1);
            *rLo = iselIntExpr_R(env, e->Iex.Binop.arg2);
            return;

//..          /* 64-bit shifts */
//..          case Iop_Shl64: {
//..             /* We use the same ingenious scheme as gcc.  Put the value
//..                to be shifted into %hi:%lo, and the shift amount into
//..                %cl.  Then (dsts on right, a la ATT syntax):
//..  
//..                shldl %cl, %lo, %hi   -- make %hi be right for the
//..                                      -- shift amt %cl % 32
//..                shll  %cl, %lo        -- make %lo be right for the
//..                                      -- shift amt %cl % 32
//.. 
//..                Now, if (shift amount % 64) is in the range 32 .. 63,
//..                we have to do a fixup, which puts the result low half
//..                into the result high half, and zeroes the low half:
//.. 
//..                testl $32, %ecx
//.. 
//..                cmovnz %lo, %hi
//..                movl $0, %tmp         -- sigh; need yet another reg
//..                cmovnz %tmp, %lo 
//..             */
//..             HReg rAmt, sHi, sLo, tHi, tLo, tTemp;
//..             tLo = newVRegI(env);
//..             tHi = newVRegI(env);
//..             tTemp = newVRegI(env);
//..             rAmt = iselIntExpr_R(env, e->Iex.Binop.arg2);
//..             iselInt64Expr(&sHi,&sLo, env, e->Iex.Binop.arg1);
//..             addInstr(env, mk_iMOVsd_RR(rAmt, hregX86_ECX()));
//..             addInstr(env, mk_iMOVsd_RR(sHi, tHi));
//..             addInstr(env, mk_iMOVsd_RR(sLo, tLo));
//..             /* Ok.  Now shift amt is in %ecx, and value is in tHi/tLo
//..                and those regs are legitimately modifiable. */
//..             addInstr(env, X86Instr_Sh3232(Xsh_SHL, 0/*%cl*/, tLo, tHi));
//..             addInstr(env, X86Instr_Sh32(Xsh_SHL, 0/*%cl*/, X86RM_Reg(tLo)));
//..             addInstr(env, X86Instr_Test32(X86RI_Imm(32), 
//..                           X86RM_Reg(hregX86_ECX())));
//..             addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tLo), tHi));
//..             addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tTemp));
//..             addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tTemp), tLo));
//..             *rHi = tHi;
//..             *rLo = tLo;
//..             return;
//..          }
//.. 
//..          case Iop_Shr64: {
//..             /* We use the same ingenious scheme as gcc.  Put the value
//..                to be shifted into %hi:%lo, and the shift amount into
//..                %cl.  Then:
//..  
//..                shrdl %cl, %hi, %lo   -- make %lo be right for the
//..                                      -- shift amt %cl % 32
//..                shrl  %cl, %hi        -- make %hi be right for the
//..                                      -- shift amt %cl % 32
//.. 
//..                Now, if (shift amount % 64) is in the range 32 .. 63,
//..                we have to do a fixup, which puts the result high half
//..                into the result low half, and zeroes the high half:
//.. 
//..                testl $32, %ecx
//.. 
//..                cmovnz %hi, %lo
//..                movl $0, %tmp         -- sigh; need yet another reg
//..                cmovnz %tmp, %hi
//..             */
//..             HReg rAmt, sHi, sLo, tHi, tLo, tTemp;
//..             tLo = newVRegI(env);
//..             tHi = newVRegI(env);
//..             tTemp = newVRegI(env);
//..             rAmt = iselIntExpr_R(env, e->Iex.Binop.arg2);
//..             iselInt64Expr(&sHi,&sLo, env, e->Iex.Binop.arg1);
//..             addInstr(env, mk_iMOVsd_RR(rAmt, hregX86_ECX()));
//..             addInstr(env, mk_iMOVsd_RR(sHi, tHi));
//..             addInstr(env, mk_iMOVsd_RR(sLo, tLo));
//..             /* Ok.  Now shift amt is in %ecx, and value is in tHi/tLo
//..                and those regs are legitimately modifiable. */
//..             addInstr(env, X86Instr_Sh3232(Xsh_SHR, 0/*%cl*/, tHi, tLo));
//..             addInstr(env, X86Instr_Sh32(Xsh_SHR, 0/*%cl*/, X86RM_Reg(tHi)));
//..             addInstr(env, X86Instr_Test32(X86RI_Imm(32), 
//..                           X86RM_Reg(hregX86_ECX())));
//..             addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tHi), tLo));
//..             addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tTemp));
//..             addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tTemp), tHi));
//..             *rHi = tHi;
//..             *rLo = tLo;
//..             return;
//..          }
//.. 
//..          /* F64 -> I64 */
//..          /* Sigh, this is an almost exact copy of the F64 -> I32/I16
//..             case.  Unfortunately I see no easy way to avoid the
//..             duplication. */
//..          case Iop_F64toI64: {
//..             HReg rf  = iselDblExpr(env, e->Iex.Binop.arg2);
//..             HReg tLo = newVRegI(env);
//..             HReg tHi = newVRegI(env);
//.. 
//..             /* Used several times ... */
//..             /* Careful ... this sharing is only safe because
//..             zero_esp/four_esp do not hold any registers which the
//..             register allocator could attempt to swizzle later. */
//..             X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());
//..             X86AMode* four_esp = X86AMode_IR(4, hregX86_ESP());
//.. 
//..             /* rf now holds the value to be converted, and rrm holds
//..                the rounding mode value, encoded as per the
//..                IRRoundingMode enum.  The first thing to do is set the
//..                FPU's rounding mode accordingly. */
//.. 
//..             /* Create a space for the format conversion. */
//..             /* subl $8, %esp */
//..             sub_from_esp(env, 8);
//.. 
//..             /* Set host rounding mode */
//..             set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );
//.. 
//..             /* gistll %rf, 0(%esp) */
//..             addInstr(env, X86Instr_FpLdStI(False/*store*/, 8, rf, zero_esp));
//.. 
//..             /* movl 0(%esp), %dstLo */
//..             /* movl 4(%esp), %dstHi */
//..             addInstr(env, X86Instr_Alu32R(
//..                              Xalu_MOV, X86RMI_Mem(zero_esp), tLo));
//..             addInstr(env, X86Instr_Alu32R(
//..                              Xalu_MOV, X86RMI_Mem(four_esp), tHi));
//.. 
//..             /* Restore default FPU rounding. */
//..             set_FPU_rounding_default( env );
//.. 
//..             /* addl $8, %esp */
//..             add_to_esp(env, 8);
//.. 
//..             *rHi = tHi;
//..             *rLo = tLo;
//..             return;
//..          }
//.. 
//..          case Iop_Add8x8:
//..             fn = (HWord)h_generic_calc_Add8x8; goto binnish;
//..          case Iop_Add16x4:
//..             fn = (HWord)h_generic_calc_Add16x4; goto binnish;
//..          case Iop_Add32x2:
//..             fn = (HWord)h_generic_calc_Add32x2; goto binnish;
//.. 
//..          case Iop_Avg8Ux8:
//..             fn = (HWord)h_generic_calc_Avg8Ux8; goto binnish;
//..          case Iop_Avg16Ux4:
//..             fn = (HWord)h_generic_calc_Avg16Ux4; goto binnish;
//.. 
//..          case Iop_CmpEQ8x8:
//..             fn = (HWord)h_generic_calc_CmpEQ8x8; goto binnish;
//..          case Iop_CmpEQ16x4:
//..             fn = (HWord)h_generic_calc_CmpEQ16x4; goto binnish;
//..          case Iop_CmpEQ32x2:
//..             fn = (HWord)h_generic_calc_CmpEQ32x2; goto binnish;
//.. 
//..          case Iop_CmpGT8Sx8:
//..             fn = (HWord)h_generic_calc_CmpGT8Sx8; goto binnish;
//..          case Iop_CmpGT16Sx4:
//..             fn = (HWord)h_generic_calc_CmpGT16Sx4; goto binnish;
//..          case Iop_CmpGT32Sx2:
//..             fn = (HWord)h_generic_calc_CmpGT32Sx2; goto binnish;
//.. 
//..          case Iop_InterleaveHI8x8:
//..             fn = (HWord)h_generic_calc_InterleaveHI8x8; goto binnish;
//..          case Iop_InterleaveLO8x8:
//..             fn = (HWord)h_generic_calc_InterleaveLO8x8; goto binnish;
//..          case Iop_InterleaveHI16x4:
//..             fn = (HWord)h_generic_calc_InterleaveHI16x4; goto binnish;
//..          case Iop_InterleaveLO16x4:
//..             fn = (HWord)h_generic_calc_InterleaveLO16x4; goto binnish;
//..          case Iop_InterleaveHI32x2:
//..             fn = (HWord)h_generic_calc_InterleaveHI32x2; goto binnish;
//..          case Iop_InterleaveLO32x2:
//..             fn = (HWord)h_generic_calc_InterleaveLO32x2; goto binnish;
//.. 
//..          case Iop_Max8Ux8:
//..             fn = (HWord)h_generic_calc_Max8Ux8; goto binnish;
//..          case Iop_Max16Sx4:
//..             fn = (HWord)h_generic_calc_Max16Sx4; goto binnish;
//..          case Iop_Min8Ux8:
//..             fn = (HWord)h_generic_calc_Min8Ux8; goto binnish;
//..          case Iop_Min16Sx4:
//..             fn = (HWord)h_generic_calc_Min16Sx4; goto binnish;
//.. 
//..          case Iop_Mul16x4:
//..             fn = (HWord)h_generic_calc_Mul16x4; goto binnish;
//..          case Iop_MulHi16Sx4:
//..             fn = (HWord)h_generic_calc_MulHi16Sx4; goto binnish;
//..          case Iop_MulHi16Ux4:
//..             fn = (HWord)h_generic_calc_MulHi16Ux4; goto binnish;
//.. 
//..          case Iop_QAdd8Sx8:
//..             fn = (HWord)h_generic_calc_QAdd8Sx8; goto binnish;
//..          case Iop_QAdd16Sx4:
//..             fn = (HWord)h_generic_calc_QAdd16Sx4; goto binnish;
//..          case Iop_QAdd8Ux8:
//..             fn = (HWord)h_generic_calc_QAdd8Ux8; goto binnish;
//..          case Iop_QAdd16Ux4:
//..             fn = (HWord)h_generic_calc_QAdd16Ux4; goto binnish;
//.. 
//..          case Iop_QNarrow32Sx2:
//..             fn = (HWord)h_generic_calc_QNarrow32Sx2; goto binnish;
//..          case Iop_QNarrow16Sx4:
//..             fn = (HWord)h_generic_calc_QNarrow16Sx4; goto binnish;
//..          case Iop_QNarrow16Ux4:
//..             fn = (HWord)h_generic_calc_QNarrow16Ux4; goto binnish;
//.. 
//..          case Iop_QSub8Sx8:
//..             fn = (HWord)h_generic_calc_QSub8Sx8; goto binnish;
//..          case Iop_QSub16Sx4:
//..             fn = (HWord)h_generic_calc_QSub16Sx4; goto binnish;
//..          case Iop_QSub8Ux8:
//..             fn = (HWord)h_generic_calc_QSub8Ux8; goto binnish;
//..          case Iop_QSub16Ux4:
//..             fn = (HWord)h_generic_calc_QSub16Ux4; goto binnish;
//.. 
//..          case Iop_Sub8x8:
//..             fn = (HWord)h_generic_calc_Sub8x8; goto binnish;
//..          case Iop_Sub16x4:
//..             fn = (HWord)h_generic_calc_Sub16x4; goto binnish;
//..          case Iop_Sub32x2:
//..             fn = (HWord)h_generic_calc_Sub32x2; goto binnish;
//.. 
//..          binnish: {
//..             /* Note: the following assumes all helpers are of
//..                signature 
//..                   ULong fn ( ULong, ULong ), and they are
//..                not marked as regparm functions. 
//..             */
//..             HReg xLo, xHi, yLo, yHi;
//..             HReg tLo = newVRegI(env);
//..             HReg tHi = newVRegI(env);
//..             iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(yHi)));
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(yLo)));
//..             iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(xHi)));
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(xLo)));
//..             addInstr(env, X86Instr_Call( Xcc_ALWAYS, (UInt)fn, 0 ));
//..             add_to_esp(env, 4*4);
//..             addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
//..             addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
//..             *rHi = tHi;
//..             *rLo = tLo;
//..             return;
//..          }
//.. 
//..          case Iop_ShlN32x2:
//..             fn = (HWord)h_generic_calc_ShlN32x2; goto shifty;
//..          case Iop_ShlN16x4:
//..             fn = (HWord)h_generic_calc_ShlN16x4; goto shifty;
//..          case Iop_ShrN32x2:
//..             fn = (HWord)h_generic_calc_ShrN32x2; goto shifty;
//..          case Iop_ShrN16x4:
//..             fn = (HWord)h_generic_calc_ShrN16x4; goto shifty;
//..          case Iop_SarN32x2:
//..             fn = (HWord)h_generic_calc_SarN32x2; goto shifty;
//..          case Iop_SarN16x4:
//..             fn = (HWord)h_generic_calc_SarN16x4; goto shifty;
//..          shifty: {
//..             /* Note: the following assumes all helpers are of
//..                signature 
//..                   ULong fn ( ULong, UInt ), and they are
//..                not marked as regparm functions. 
//..             */
//..             HReg xLo, xHi;
//..             HReg tLo = newVRegI(env);
//..             HReg tHi = newVRegI(env);
//..             X86RMI* y = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
//..             addInstr(env, X86Instr_Push(y));
//..             iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(xHi)));
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(xLo)));
//..             addInstr(env, X86Instr_Call( Xcc_ALWAYS, (UInt)fn, 0 ));
//..             add_to_esp(env, 3*4);
//..             addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
//..             addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
//..             *rHi = tHi;
//..             *rLo = tLo;
//..             return;
//..          }

      default: 
         break;
      }
   } /* if (e->tag == Iex_Binop) */


   /* --------- UNARY ops --------- */
   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {

         /* 32Sto64(e) */
         case Iop_32Sto64: {
            HReg tHi = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, PPC32Instr_Alu32(Palu_SAR, tHi, src, PPC32RH_Imm(False,31)));
            *rHi = tHi;
            *rLo = src;
            return;
         }

         /* 32Uto64(e) */
         case Iop_32Uto64: {
            HReg tHi = newVRegI(env);
            HReg tLo = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, PPC32Instr_LI32(tHi, 0));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* V128{HI}to64 */
         case Iop_V128HIto64:
         case Iop_V128to64: {
            HReg r_aligned16;
            Int  off = e->Iex.Unop.op==Iop_V128HIto64 ? 0 : 8;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg vec = iselVecExpr(env, e->Iex.Unop.arg);
            PPC32AMode *am_off0, *am_offLO, *am_offHI;
            sub_from_sp( env, 32 );     // Move SP down 32 bytes

            // get a quadword aligned address within our stack space
            r_aligned16 = get_sp_aligned16( env );
            am_off0  = PPC32AMode_IR( 0,     r_aligned16 );
            am_offHI = PPC32AMode_IR( off,   r_aligned16 );
            am_offLO = PPC32AMode_IR( off+4, r_aligned16 );

            // store as Vec128
            addInstr(env, PPC32Instr_AvLdSt( False/*store*/, 16, vec, am_off0 ));

            // load hi,lo words (of hi/lo half of vec) as Ity_I32's
            addInstr(env, PPC32Instr_Load( 4, False, tHi, am_offHI ));
            addInstr(env, PPC32Instr_Load( 4, False, tLo, am_offLO ));

            add_to_sp( env, 32 );       // Reset SP
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* could do better than this, but for now ... */
         case Iop_1Sto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            PPC32CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, PPC32Instr_Set32(cond,tLo));
            addInstr(env, PPC32Instr_Alu32(Palu_SHL, tLo, tLo, PPC32RH_Imm(False,31)));
            addInstr(env, PPC32Instr_Alu32(Palu_SAR, tLo, tLo, PPC32RH_Imm(False,31)));
            addInstr(env, mk_iMOVds_RR(tHi, tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Neg64: {
            HReg yLo, yHi;
            HReg zero = newVRegI(env);
            HReg tLo  = newVRegI(env);
            HReg tHi  = newVRegI(env);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Unop.arg);
            addInstr(env, PPC32Instr_LI32(zero, 0));
            addInstr(env, PPC32Instr_AddSubC32( False/*sub*/, True /*set carry*/,
                                                tLo, zero, yLo));
            addInstr(env, PPC32Instr_AddSubC32( False/*sub*/, False/*read carry*/,
                                                tHi, zero, yHi));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

//..          /* Not64(e) */
//..          case Iop_Not64: {
//..             HReg tLo = newVRegI(env);
//..             HReg tHi = newVRegI(env);
//..             HReg sHi, sLo;
//..             iselInt64Expr(&sHi, &sLo, env, e->Iex.Unop.arg);
//..             addInstr(env, mk_iMOVsd_RR(sHi, tHi));
//..             addInstr(env, mk_iMOVsd_RR(sLo, tLo));
//..             addInstr(env, X86Instr_Unary32(Xun_NOT,X86RM_Reg(tHi)));
//..             addInstr(env, X86Instr_Unary32(Xun_NOT,X86RM_Reg(tLo)));
//..             *rHi = tHi;
//..             *rLo = tLo;
//..             return;
//..          }

         /* ReinterpF64asI64(e) */
         /* Given an IEEE754 double, produce an I64 with the same bit
            pattern. */
         case Iop_ReinterpF64asI64: {
            PPC32AMode *am_addr0, *am_addr1;
            HReg fr_src  = iselDblExpr(env, e->Iex.Unop.arg);
            HReg r_dstLo = newVRegI(env);
            HReg r_dstHi = newVRegI(env);

            sub_from_sp( env, 16 );     // Move SP down 16 bytes
            am_addr0 = PPC32AMode_IR(0, StackFramePtr);
            am_addr1 = PPC32AMode_IR(4, StackFramePtr);

            // store as F64
            addInstr(env, PPC32Instr_FpLdSt( False/*store*/, 8, fr_src, am_addr0 ));

            // load hi,lo as Ity_I32's
            addInstr(env, PPC32Instr_Load( 4, False, r_dstHi, am_addr0 ));
            addInstr(env, PPC32Instr_Load( 4, False, r_dstLo, am_addr1 ));
            *rHi = r_dstHi;
            *rLo = r_dstLo;

            add_to_sp( env, 16 );       // Reset SP
            return;
         }

//..          case Iop_CmpNEZ32x2:
//..             fn = (HWord)h_generic_calc_CmpNEZ32x2; goto unish;
//..          case Iop_CmpNEZ16x4:
//..             fn = (HWord)h_generic_calc_CmpNEZ16x4; goto unish;
//..          case Iop_CmpNEZ8x8:
//..             fn = (HWord)h_generic_calc_CmpNEZ8x8; goto unish;
//..          unish: {
//..             /* Note: the following assumes all helpers are of
//..                signature 
//..                   ULong fn ( ULong ), and they are
//..                not marked as regparm functions. 
//..             */
//..             HReg xLo, xHi;
//..             HReg tLo = newVRegI(env);
//..             HReg tHi = newVRegI(env);
//..             iselInt64Expr(&xHi, &xLo, env, e->Iex.Unop.arg);
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(xHi)));
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(xLo)));
//..             addInstr(env, X86Instr_Call( Xcc_ALWAYS, (UInt)fn, 0 ));
//..             add_to_esp(env, 2*4);
//..             addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
//..             addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
//..             *rHi = tHi;
//..             *rLo = tLo;
//..             return;
//..          }

         default:
            break;
      }
   } /* if (e->tag == Iex_Unop) */


//..    /* --------- CCALL --------- */
//..    if (e->tag == Iex_CCall) {
//..       HReg tLo = newVRegI(env);
//..       HReg tHi = newVRegI(env);
//.. 
//..       /* Marshal args, do the call, clear stack. */
//..       doHelperCall( env, False, NULL, e->Iex.CCall.cee, e->Iex.CCall.args );
//.. 
//..       addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
//..       addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
//..       *rHi = tHi;
//..       *rLo = tLo;
//..       return;
//..    }

   vex_printf("iselInt64Expr(ppc32): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselInt64Expr(ppc32)");
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (32 bit)         ---*/
/*---------------------------------------------------------*/

/* Nothing interesting here; really just wrappers for
   64-bit stuff. */

static HReg iselFltExpr ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselFltExpr_wrk( env, e );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcFlt64); /* yes, really Flt64 */
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselFltExpr_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_F32);

   if (e->tag == Iex_Tmp) {
      return lookupIRTemp(env, e->Iex.Tmp.tmp);
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_BE) {
      PPC32AMode* am_addr;
      HReg r_dst = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_F32);
      am_addr = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, PPC32Instr_FpLdSt(True/*load*/, 4, r_dst, am_addr));
      return r_dst;
   }

   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_F64toF32) {
      /* Although the result is still held in a standard FPU register,
         we need to round it to reflect the loss of accuracy/range
         entailed in casting it to a 32-bit float. */
      HReg r_dst = newVRegF(env);
      HReg r_src = iselDblExpr(env, e->Iex.Binop.arg2);
      set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );
      addInstr(env, PPC32Instr_FpF64toF32(r_dst, r_src));
      set_FPU_rounding_default( env );
      return r_dst;
   }

   if (e->tag == Iex_Get) {
      HReg r_dst = newVRegF(env);
      PPC32AMode* am_addr = PPC32AMode_IR(e->Iex.Get.offset, GuestStatePtr );
      addInstr(env, PPC32Instr_FpLdSt( True/*load*/, 4, r_dst, am_addr ));
      return r_dst;
   }

//..    if (e->tag == Iex_Unop
//..        && e->Iex.Unop.op == Iop_ReinterpI32asF32) {
//..        /* Given an I32, produce an IEEE754 float with the same bit
//..           pattern. */
//..       HReg    dst = newVRegF(env);
//..       X86RMI* rmi = iselIntExpr_RMI(env, e->Iex.Unop.arg);
//..       /* paranoia */
//..       addInstr(env, X86Instr_Push(rmi));
//..       addInstr(env, X86Instr_FpLdSt(
//..                        True/*load*/, 4, dst,
//..                        X86AMode_IR(0, hregX86_ESP())));
//..       add_to_esp(env, 4);
//..       return dst;
//..    }

   vex_printf("iselFltExpr(ppc32): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselFltExpr_wrk(ppc32)");
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (64 bit)         ---*/
/*---------------------------------------------------------*/

/* Compute a 64-bit floating point value into a register, the identity
   of which is returned.  As with iselIntExpr_R, the reg may be either
   real or virtual; in any case it must not be changed by subsequent
   code emitted by the caller.  */

/* IEEE 754 formats.  From http://www.freesoft.org/CIE/RFC/1832/32.htm:

    Type                  S (1 bit)   E (11 bits)   F (52 bits)
    ----                  ---------   -----------   -----------
    signalling NaN        u           2047 (max)    .0uuuuu---u
                                                    (with at least
                                                     one 1 bit)
    quiet NaN             u           2047 (max)    .1uuuuu---u

    negative infinity     1           2047 (max)    .000000---0

    positive infinity     0           2047 (max)    .000000---0

    negative zero         1           0             .000000---0

    positive zero         0           0             .000000---0
*/

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

   if (e->tag == Iex_Tmp) {
      return lookupIRTemp(env, e->Iex.Tmp.tmp);
   }

   /* --------- LITERAL --------- */
   if (e->tag == Iex_Const) {
      HReg r_srcHi, r_srcLo;
      union { UInt u32x2[2]; ULong u64; Double f64; } u;
      vassert(sizeof(u) == 8);
      vassert(sizeof(u.u64) == 8);
      vassert(sizeof(u.f64) == 8);
      vassert(sizeof(u.u32x2) == 8);

      if (e->Iex.Const.con->tag == Ico_F64) {
         u.f64 = e->Iex.Const.con->Ico.F64;
      }
      else if (e->Iex.Const.con->tag == Ico_F64i) {
         u.u64 = e->Iex.Const.con->Ico.F64i;
      }
      else
         vpanic("iselDblExpr(ppc32): const");

      r_srcHi = newVRegI(env);
      r_srcLo = newVRegI(env);
      addInstr(env, PPC32Instr_LI32(r_srcHi, u.u32x2[1]));
      addInstr(env, PPC32Instr_LI32(r_srcLo, u.u32x2[0]));
      return mk_LoadRRtoFPR( env, r_srcHi, r_srcLo );
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_BE) {
      HReg r_dst = newVRegF(env);
      PPC32AMode* am_addr;
      vassert(e->Iex.Load.ty == Ity_F64);
      am_addr = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, PPC32Instr_FpLdSt(True/*load*/, 8, r_dst, am_addr));
      return r_dst;
   }

   if (e->tag == Iex_Get) {
      HReg r_dst = newVRegF(env);
      PPC32AMode* am_addr = PPC32AMode_IR(e->Iex.Get.offset, GuestStatePtr );
      addInstr(env, PPC32Instr_FpLdSt( True/*load*/, 8, r_dst, am_addr ));
      return r_dst;
   }

//..    if (e->tag == Iex_GetI) {
//..       X86AMode* am 
//..          = genGuestArrayOffset(
//..               env, e->Iex.GetI.descr, 
//..                    e->Iex.GetI.ix, e->Iex.GetI.bias );
//..       HReg res = newVRegF(env);
//..       addInstr(env, X86Instr_FpLdSt( True/*load*/, 8, res, am ));
//..       return res;
//..    }

   if (e->tag == Iex_Binop) {
      PPC32FpOp fpop = Pfp_INVALID;
      switch (e->Iex.Binop.op) {
         case Iop_AddF64:    fpop = Pfp_ADD; break;
         case Iop_SubF64:    fpop = Pfp_SUB; break;
         case Iop_MulF64:    fpop = Pfp_MUL; break;
         case Iop_DivF64:    fpop = Pfp_DIV; break;
         default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg r_dst  = newVRegF(env);
         HReg r_srcL = iselDblExpr(env, e->Iex.Binop.arg1);
         HReg r_srcR = iselDblExpr(env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_FpBinary(fpop, r_dst, r_srcL, r_srcR));
         return r_dst;
      }
   }

//..    if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_RoundF64) {
//..       HReg rf  = iselDblExpr(env, e->Iex.Binop.arg2);
//..       HReg dst = newVRegF(env);
//.. 
//..       /* rf now holds the value to be rounded.  The first thing to do
//..          is set the FPU's rounding mode accordingly. */
//.. 
//..       /* Set host rounding mode */
//..       set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );
//.. 
//..       /* grndint %rf, %dst */
//..       addInstr(env, X86Instr_FpUnary(Xfp_ROUND, rf, dst));
//.. 
//..       /* Restore default FPU rounding. */
//..       set_FPU_rounding_default( env );
//.. 
//..       return dst;
//..    }

//..    if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_I64toF64) {
//..       HReg fr_dst = newVRegF(env);
//..       HReg rHi,rLo;
//..       iselInt64Expr( &rHi, &rLo, env, e->Iex.Binop.arg2);
//..       addInstr(env, PPC32Instr_Push(PPC32RMI_Reg(rHi)));
//..       addInstr(env, PPC32Instr_Push(PPC32RMI_Reg(rLo)));
//.. 
//..       /* Set host rounding mode */
//..       set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );
//.. 
//..       PPC32AMode* am_addr = ...
//..       addInstr(env, PPC32Instr_FpLdSt( True/*load*/, 8, r_dst,
//..                                        PPC32AMode_IR(0, GuestStatePtr ) ));
//.. 
//.. 
//..       addInstr(env, PPC32Instr_FpLdStI(
//..                        True/*load*/, 8, fr_dst, 
//..                        PPC32AMode_IR(0, hregPPC32_ESP())));
//.. 
//..       /* Restore default FPU rounding. */
//..       set_FPU_rounding_default( env );
//.. 
//..       add_to_esp(env, 8);
//..       return fr_dst;
//..    }

   if (e->tag == Iex_Unop) {
      PPC32FpOp fpop = Pfp_INVALID;
      switch (e->Iex.Unop.op) {
         case Iop_NegF64:  fpop = Pfp_NEG; break;
         case Iop_AbsF64:  fpop = Pfp_ABS; break;
         case Iop_SqrtF64: fpop = Pfp_SQRT; break;
//..          case Iop_SinF64:  fpop = Xfp_SIN; break;
//..          case Iop_CosF64:  fpop = Xfp_COS; break;
//..          case Iop_TanF64:  fpop = Xfp_TAN; break;
//..          case Iop_2xm1F64: fpop = Xfp_2XM1; break;
         default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg fr_dst = newVRegF(env);
         HReg fr_src = iselDblExpr(env, e->Iex.Unop.arg);
         addInstr(env, PPC32Instr_FpUnary(fpop, fr_dst, fr_src));
//..          if (fpop != Pfp_SQRT && fpop != Xfp_NEG && fpop != Xfp_ABS)
//..             roundToF64(env, fr_dst);
         return fr_dst;
      }
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
//..          case Iop_I32toF64: {
//..             HReg dst = newVRegF(env);
//..             HReg ri  = iselIntExpr_R(env, e->Iex.Unop.arg);
//..             addInstr(env, X86Instr_Push(X86RMI_Reg(ri)));
//..             set_FPU_rounding_default(env);
//..             addInstr(env, X86Instr_FpLdStI(
//..                              True/*load*/, 4, dst, 
//..                              X86AMode_IR(0, hregX86_ESP())));
//..             add_to_esp(env, 4);
//..             return dst;
//..          }
         case Iop_ReinterpI64asF64: {
            /* Given an I64, produce an IEEE754 double with the same
               bit pattern. */
            HReg r_srcHi, r_srcLo;
            iselInt64Expr( &r_srcHi, &r_srcLo, env, e->Iex.Unop.arg);
            return mk_LoadRRtoFPR( env, r_srcHi, r_srcLo );
         }
         case Iop_F32toF64: {
            /* this is a no-op */
            HReg res = iselFltExpr(env, e->Iex.Unop.arg);
            return res;
         }
         default: 
            break;
      }
   }

   /* --------- MULTIPLEX --------- */
   if (e->tag == Iex_Mux0X) {
      if (ty == Ity_F64
          && typeOfIRExpr(env->type_env,e->Iex.Mux0X.cond) == Ity_I8) {
         PPC32CondCode cc = mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
         HReg r_cond = iselIntExpr_R(env, e->Iex.Mux0X.cond);
         HReg frX    = iselDblExpr(env, e->Iex.Mux0X.exprX);
         HReg fr0    = iselDblExpr(env, e->Iex.Mux0X.expr0);
         HReg fr_dst = newVRegF(env);
         HReg r_tmp  = newVRegI(env);
         addInstr(env, PPC32Instr_Alu32(Palu_AND, r_tmp, r_cond, PPC32RH_Imm(False,0xFF)));
         addInstr(env, PPC32Instr_FpUnary( Pfp_MOV, fr_dst, frX ));
         addInstr(env, PPC32Instr_Cmp32(False/*unsigned*/, 7/*cr*/, r_tmp, PPC32RH_Imm(False,0)));
         addInstr(env, PPC32Instr_FpCMov( cc, fr_dst, fr0 ));
         return fr_dst;
      }
   }

   vex_printf("iselDblExpr(ppc32): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselDblExpr_wrk(ppc32)");
}


/*---------------------------------------------------------*/
/*--- ISEL: SIMD (Vector) expressions, 128 bit.         ---*/
/*---------------------------------------------------------*/

static HReg iselVecExpr ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselVecExpr_wrk( env, e );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcVec128);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselVecExpr_wrk ( ISelEnv* env, IRExpr* e )
{
//..    Bool     arg1isEReg = False;
   PPC32AvOp op = Pav_INVALID;
   IRType   ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_V128);

   if (e->tag == Iex_Tmp) {
      return lookupIRTemp(env, e->Iex.Tmp.tmp);
   }

   if (e->tag == Iex_Get) {
      /* Guest state vectors are 16byte aligned, so don't need to worry here */
      HReg dst = newVRegV(env);
      addInstr(env,
               PPC32Instr_AvLdSt( True/*load*/, 16, dst,
                                  PPC32AMode_IR(e->Iex.Get.offset, GuestStatePtr)));
      return dst;
   }

   if (e->tag == Iex_Load) {
      PPC32AMode* am_addr;
      HReg v_dst = newVRegV(env);
      vassert(e->Iex.Load.ty == Ity_V128);
      am_addr = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, PPC32Instr_AvLdSt( True/*load*/, 16, v_dst, am_addr));
      return v_dst;
   }

//..    if (e->tag == Iex_Const) {
//..       HReg dst = newVRegV(env);
//..       vassert(e->Iex.Const.con->tag == Ico_V128);
//..       addInstr(env, X86Instr_SseConst(e->Iex.Const.con->Ico.V128, dst));
//..       return dst;
//..    }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {

      case Iop_NotV128: {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, PPC32Instr_AvUnary(Pav_NOT, dst, arg));
         return dst;
      }

//..       case Iop_CmpNEZ64x2: {
//..          /* We can use SSE2 instructions for this. */
//..          /* Ideally, we want to do a 64Ix2 comparison against zero of
//..             the operand.  Problem is no such insn exists.  Solution
//..             therefore is to do a 32Ix4 comparison instead, and bitwise-
//..             negate (NOT) the result.  Let a,b,c,d be 32-bit lanes, and 
//..             let the not'd result of this initial comparison be a:b:c:d.
//..             What we need to compute is (a|b):(a|b):(c|d):(c|d).  So, use
//..             pshufd to create a value b:a:d:c, and OR that with a:b:c:d,
//..             giving the required result.
//.. 
//..             The required selection sequence is 2,3,0,1, which
//..             according to Intel's documentation means the pshufd
//..             literal value is 0xB1, that is, 
//..             (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0) 
//..          */
//..          HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
//..          HReg tmp  = newVRegV(env);
//..          HReg dst  = newVRegV(env);
//..          REQUIRE_SSE2;
//..          addInstr(env, X86Instr_SseReRg(Xsse_XOR, tmp, tmp));
//..          addInstr(env, X86Instr_SseReRg(Xsse_CMPEQ32, arg, tmp));
//..          tmp = do_sse_Not128(env, tmp);
//..          addInstr(env, X86Instr_SseShuf(0xB1, tmp, dst));
//..          addInstr(env, X86Instr_SseReRg(Xsse_OR, tmp, dst));
//..          return dst;
//..       }

      case Iop_CmpNEZ8x16: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPC32Instr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPC32Instr_AvBin8x16(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPC32Instr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_CmpNEZ16x8: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPC32Instr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPC32Instr_AvBin16x8(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPC32Instr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_CmpNEZ32x4: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPC32Instr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPC32Instr_AvBin32x4(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPC32Instr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

//..       case Iop_CmpNEZ16x8: {
//..          /* We can use SSE2 instructions for this. */
//..          HReg arg;
//..          HReg vec0 = newVRegV(env);
//..          HReg vec1 = newVRegV(env);
//..          HReg dst  = newVRegV(env);
//..          X86SseOp cmpOp 
//..             = e->Iex.Unop.op==Iop_CmpNEZ16x8 ? Xsse_CMPEQ16
//..                                              : Xsse_CMPEQ8;
//..          REQUIRE_SSE2;
//..          addInstr(env, X86Instr_SseReRg(Xsse_XOR, vec0, vec0));
//..          addInstr(env, mk_vMOVsd_RR(vec0, vec1));
//..          addInstr(env, X86Instr_Sse32Fx4(Xsse_CMPEQF, vec1, vec1));
//..          /* defer arg computation to here so as to give CMPEQF as long
//..             as possible to complete */
//..          arg = iselVecExpr(env, e->Iex.Unop.arg);
//..          /* vec0 is all 0s; vec1 is all 1s */
//..          addInstr(env, mk_vMOVsd_RR(arg, dst));
//..          /* 16x8 or 8x16 comparison == */
//..          addInstr(env, X86Instr_SseReRg(cmpOp, vec0, dst));
//..          /* invert result */
//..          addInstr(env, X86Instr_SseReRg(Xsse_XOR, vec1, dst));
//..          return dst;
//..       }

      case Iop_Recip32Fx4: op = Pavfp_RCPF;   goto do_32Fx4_unary;
      case Iop_RSqrt32Fx4: op = Pavfp_RSQRTF; goto do_32Fx4_unary;
//..       case Iop_Sqrt32Fx4:  op = Xsse_SQRTF;  goto do_32Fx4_unary;
      case Iop_I32UtoFx4:     op = Pavfp_CVTU2F;  goto do_32Fx4_unary;
      case Iop_I32StoFx4:     op = Pavfp_CVTS2F;  goto do_32Fx4_unary;
      case Iop_QFtoI32Ux4_RZ: op = Pavfp_QCVTF2U; goto do_32Fx4_unary;
      case Iop_QFtoI32Sx4_RZ: op = Pavfp_QCVTF2S; goto do_32Fx4_unary;
      case Iop_RoundF32x4_RM: op = Pavfp_ROUNDM;    goto do_32Fx4_unary;
      case Iop_RoundF32x4_RP: op = Pavfp_ROUNDP;    goto do_32Fx4_unary;
      case Iop_RoundF32x4_RN: op = Pavfp_ROUNDN;    goto do_32Fx4_unary;
      case Iop_RoundF32x4_RZ: op = Pavfp_ROUNDZ;    goto do_32Fx4_unary;
      do_32Fx4_unary:
      {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, PPC32Instr_AvUn32Fx4(op, dst, arg));
         return dst;
      }

//..       case Iop_Recip64Fx2: op = Xsse_RCPF;   goto do_64Fx2_unary;
//..       case Iop_RSqrt64Fx2: op = Xsse_RSQRTF; goto do_64Fx2_unary;
//..       case Iop_Sqrt64Fx2:  op = Xsse_SQRTF;  goto do_64Fx2_unary;
//..       do_64Fx2_unary:
//..       {
//..          HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
//..          HReg dst = newVRegV(env);
//..          REQUIRE_SSE2;
//..          addInstr(env, X86Instr_Sse64Fx2(op, arg, dst));
//..          return dst;
//..       }
//.. 
//..       case Iop_Recip32F0x4: op = Xsse_RCPF;   goto do_32F0x4_unary;
//..       case Iop_RSqrt32F0x4: op = Xsse_RSQRTF; goto do_32F0x4_unary;
//..       case Iop_Sqrt32F0x4:  op = Xsse_SQRTF;  goto do_32F0x4_unary;
//..       do_32F0x4_unary:
//..       {
//..          /* A bit subtle.  We have to copy the arg to the result
//..             register first, because actually doing the SSE scalar insn
//..             leaves the upper 3/4 of the destination register
//..             unchanged.  Whereas the required semantics of these
//..             primops is that the upper 3/4 is simply copied in from the
//..             argument. */
//..          HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
//..          HReg dst = newVRegV(env);
//..          addInstr(env, mk_vMOVsd_RR(arg, dst));
//..          addInstr(env, X86Instr_Sse32FLo(op, arg, dst));
//..          return dst;
//..       }
//.. 
//..       case Iop_Recip64F0x2: op = Xsse_RCPF;   goto do_64F0x2_unary;
//..       case Iop_RSqrt64F0x2: op = Xsse_RSQRTF; goto do_64F0x2_unary;
//..       case Iop_Sqrt64F0x2:  op = Xsse_SQRTF;  goto do_64F0x2_unary;
//..       do_64F0x2_unary:
//..       {
//..          /* A bit subtle.  We have to copy the arg to the result
//..             register first, because actually doing the SSE scalar insn
//..             leaves the upper half of the destination register
//..             unchanged.  Whereas the required semantics of these
//..             primops is that the upper half is simply copied in from the
//..             argument. */
//..          HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
//..          HReg dst = newVRegV(env);
//..          REQUIRE_SSE2;
//..          addInstr(env, mk_vMOVsd_RR(arg, dst));
//..          addInstr(env, X86Instr_Sse64FLo(op, arg, dst));
//..          return dst;
//..       }

      case Iop_32UtoV128: {
         HReg r_aligned16, r_zeros;
         HReg r_src = iselIntExpr_R(env, e->Iex.Unop.arg);
         HReg   dst = newVRegV(env);
         PPC32AMode *am_off0, *am_off4, *am_off8, *am_off12;
         sub_from_sp( env, 32 );     // Move SP down

         /* Get a quadword aligned address within our stack space */
         r_aligned16 = get_sp_aligned16( env );
         am_off0  = PPC32AMode_IR( 0,  r_aligned16);
         am_off4  = PPC32AMode_IR( 4,  r_aligned16);
         am_off8  = PPC32AMode_IR( 8,  r_aligned16);
         am_off12 = PPC32AMode_IR( 12, r_aligned16);

         /* Store zeros */
         r_zeros = newVRegI(env);
         addInstr(env, PPC32Instr_LI32(r_zeros, 0x0));
         addInstr(env, PPC32Instr_Store( 4, am_off0, r_zeros ));
         addInstr(env, PPC32Instr_Store( 4, am_off4, r_zeros ));
         addInstr(env, PPC32Instr_Store( 4, am_off8, r_zeros ));

         /* Store r_src in low word of quadword-aligned mem */
         addInstr(env, PPC32Instr_Store( 4, am_off12, r_src ));

         /* Load word into low word of quadword vector reg */
         addInstr(env, PPC32Instr_AvLdSt( True/*load*/, 4, dst, am_off12 ));

         add_to_sp( env, 32 );       // Reset SP
         return dst;
      }

//..       case Iop_64UtoV128: {
//..          HReg      rHi, rLo;
//..          HReg      dst  = newVRegV(env);
//..          X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
//..          iselInt64Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
//..          addInstr(env, X86Instr_Push(X86RMI_Reg(rHi)));
//..          addInstr(env, X86Instr_Push(X86RMI_Reg(rLo)));
//..          addInstr(env, X86Instr_SseLdzLO(8, dst, esp0));
//..          add_to_esp(env, 8);
//..          return dst;
//..       }

      case Iop_Dup8x16:
      case Iop_Dup16x8:
      case Iop_Dup32x4:
         return mk_AvDuplicateRI(env, e->Iex.Binop.arg1);

      default:
         break;
      } /* switch (e->Iex.Unop.op) */
   } /* if (e->tag == Iex_Unop) */

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {

//..       case Iop_SetV128lo32: {
//..          HReg dst = newVRegV(env);
//..          HReg srcV = iselVecExpr(env, e->Iex.Binop.arg1);
//..          HReg srcI = iselIntExpr_R(env, e->Iex.Binop.arg2);
//..          X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
//..          sub_from_esp(env, 16);
//..          addInstr(env, X86Instr_SseLdSt(False/*store*/, srcV, esp0));
//..          addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(srcI), esp0));
//..          addInstr(env, X86Instr_SseLdSt(True/*load*/, dst, esp0));
//..          add_to_esp(env, 16);
//..          return dst;
//..       }
//.. 
//..       case Iop_SetV128lo64: {
//..          HReg dst = newVRegV(env);
//..          HReg srcV = iselVecExpr(env, e->Iex.Binop.arg1);
//..          HReg srcIhi, srcIlo;
//..          X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
//..          X86AMode* esp4 = advance4(esp0);
//..          iselInt64Expr(&srcIhi, &srcIlo, env, e->Iex.Binop.arg2);
//..          sub_from_esp(env, 16);
//..          addInstr(env, X86Instr_SseLdSt(False/*store*/, srcV, esp0));
//..          addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(srcIlo), esp0));
//..          addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(srcIhi), esp4));
//..          addInstr(env, X86Instr_SseLdSt(True/*load*/, dst, esp0));
//..          add_to_esp(env, 16);
//..          return dst;
//..       }
//.. 
      case Iop_64HLtoV128: {
         HReg r3, r2, r1, r0, r_aligned16;
         PPC32AMode *am_off0, *am_off4, *am_off8, *am_off12;
         HReg        dst = newVRegV(env);
         /* do this via the stack (easy, convenient, etc) */
         sub_from_sp( env, 32 );        // Move SP down

         // get a quadword aligned address within our stack space
         r_aligned16 = get_sp_aligned16( env );
         am_off0  = PPC32AMode_IR( 0,  r_aligned16);
         am_off4  = PPC32AMode_IR( 4,  r_aligned16);
         am_off8  = PPC32AMode_IR( 8,  r_aligned16);
         am_off12 = PPC32AMode_IR( 12, r_aligned16);

         /* Do the less significant 64 bits */
         iselInt64Expr(&r1, &r0, env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_Store( 4, am_off12, r0 ));
         addInstr(env, PPC32Instr_Store( 4, am_off8,  r1 ));
         /* Do the more significant 64 bits */
         iselInt64Expr(&r3, &r2, env, e->Iex.Binop.arg1);
         addInstr(env, PPC32Instr_Store( 4, am_off4, r2 ));
         addInstr(env, PPC32Instr_Store( 4, am_off0, r3 ));

         /* Fetch result back from stack. */
         addInstr(env, PPC32Instr_AvLdSt(True/*load*/, 16, dst, am_off0));

         add_to_sp( env, 32 );          // Reset SP
         return dst;
      }

      case Iop_Add32Fx4:   op = Pavfp_ADDF;   goto do_32Fx4;
      case Iop_Sub32Fx4:   op = Pavfp_SUBF;   goto do_32Fx4;
      case Iop_Max32Fx4:   op = Pavfp_MAXF;   goto do_32Fx4;
      case Iop_Min32Fx4:   op = Pavfp_MINF;   goto do_32Fx4;
      case Iop_Mul32Fx4:   op = Pavfp_MULF;   goto do_32Fx4;
//..       case Iop_Div32Fx4:   op = Xsse_DIVF;   goto do_32Fx4;
      case Iop_CmpEQ32Fx4: op = Pavfp_CMPEQF; goto do_32Fx4;
      case Iop_CmpGT32Fx4: op = Pavfp_CMPGTF; goto do_32Fx4;
      case Iop_CmpGE32Fx4: op = Pavfp_CMPGEF; goto do_32Fx4;
//..       case Iop_CmpLT32Fx4:
      do_32Fx4:
      {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         addInstr(env, PPC32Instr_AvBin32Fx4(op, dst, argL, argR));
         return dst;
      }

      case Iop_CmpLE32Fx4: {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         
         /* stay consistent with native ppc compares:
            if a left/right lane holds a nan, return zeros for that lane
            so: le == NOT(gt OR isNan)
          */
         HReg isNanLR = newVRegV(env);
         HReg isNanL = isNan(env, argL);
         HReg isNanR = isNan(env, argR);
         addInstr(env, PPC32Instr_AvBinary(Pav_OR, isNanLR, isNanL, isNanR));

         addInstr(env, PPC32Instr_AvBin32Fx4(Pavfp_CMPGTF, dst, argL, argR));
         addInstr(env, PPC32Instr_AvBinary(Pav_OR, dst, dst, isNanLR));
         addInstr(env, PPC32Instr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

//..       case Iop_CmpEQ64Fx2: op = Xsse_CMPEQF; goto do_64Fx2;
//..       case Iop_CmpLT64Fx2: op = Xsse_CMPLTF; goto do_64Fx2;
//..       case Iop_CmpLE64Fx2: op = Xsse_CMPLEF; goto do_64Fx2;
//..       case Iop_Add64Fx2:   op = Xsse_ADDF;   goto do_64Fx2;
//..       case Iop_Div64Fx2:   op = Xsse_DIVF;   goto do_64Fx2;
//..       case Iop_Max64Fx2:   op = Xsse_MAXF;   goto do_64Fx2;
//..       case Iop_Min64Fx2:   op = Xsse_MINF;   goto do_64Fx2;
//..       case Iop_Mul64Fx2:   op = Xsse_MULF;   goto do_64Fx2;
//..       case Iop_Sub64Fx2:   op = Xsse_SUBF;   goto do_64Fx2;
//..       do_64Fx2:
//..       {
//..          HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
//..          HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
//..          HReg dst = newVRegV(env);
//..          REQUIRE_SSE2;
//..          addInstr(env, mk_vMOVsd_RR(argL, dst));
//..          addInstr(env, X86Instr_Sse64Fx2(op, argR, dst));
//..          return dst;
//..       }

//..       case Iop_CmpEQ32F0x4: op = Xsse_CMPEQF; goto do_32F0x4;
//..       case Iop_CmpLT32F0x4: op = Xsse_CMPLTF; goto do_32F0x4;
//..       case Iop_CmpLE32F0x4: op = Xsse_CMPLEF; goto do_32F0x4;
//..       case Iop_Add32F0x4:   op = Xsse_ADDF;   goto do_32F0x4;
//..       case Iop_Div32F0x4:   op = Xsse_DIVF;   goto do_32F0x4;
//..       case Iop_Max32F0x4:   op = Xsse_MAXF;   goto do_32F0x4;
//..       case Iop_Min32F0x4:   op = Xsse_MINF;   goto do_32F0x4;
//..       case Iop_Mul32F0x4:   op = Xsse_MULF;   goto do_32F0x4;
//..       case Iop_Sub32F0x4:   op = Xsse_SUBF;   goto do_32F0x4;
//..       do_32F0x4: {
//..          HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
//..          HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
//..          HReg dst = newVRegV(env);
//..          addInstr(env, mk_vMOVsd_RR(argL, dst));
//..          addInstr(env, X86Instr_Sse32FLo(op, argR, dst));
//..          return dst;
//..       }

//..       case Iop_CmpEQ64F0x2: op = Xsse_CMPEQF; goto do_64F0x2;
//..       case Iop_CmpLT64F0x2: op = Xsse_CMPLTF; goto do_64F0x2;
//..       case Iop_CmpLE64F0x2: op = Xsse_CMPLEF; goto do_64F0x2;
//..       case Iop_Add64F0x2:   op = Xsse_ADDF;   goto do_64F0x2;
//..       case Iop_Div64F0x2:   op = Xsse_DIVF;   goto do_64F0x2;
//..       case Iop_Max64F0x2:   op = Xsse_MAXF;   goto do_64F0x2;
//..       case Iop_Min64F0x2:   op = Xsse_MINF;   goto do_64F0x2;
//..       case Iop_Mul64F0x2:   op = Xsse_MULF;   goto do_64F0x2;
//..       case Iop_Sub64F0x2:   op = Xsse_SUBF;   goto do_64F0x2;
//..       do_64F0x2: {
//..          HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
//..          HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
//..          HReg dst = newVRegV(env);
//..          REQUIRE_SSE2;
//..          addInstr(env, mk_vMOVsd_RR(argL, dst));
//..          addInstr(env, X86Instr_Sse64FLo(op, argR, dst));
//..          return dst;
//..       }

      case Iop_AndV128:    op = Pav_AND;      goto do_AvBin;
      case Iop_OrV128:     op = Pav_OR;       goto do_AvBin;
      case Iop_XorV128:    op = Pav_XOR;      goto do_AvBin;
      do_AvBin: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegV(env);
         addInstr(env, PPC32Instr_AvBinary(op, dst, arg1, arg2));
         return dst;
      }

//..       case Iop_Mul16x8:    op = Xsse_MUL16;    goto do_SseReRg;

      case Iop_Shl8x16:    op = Pav_SHL;    goto do_AvBin8x16;
      case Iop_Shr8x16:    op = Pav_SHR;    goto do_AvBin8x16;
      case Iop_Sar8x16:    op = Pav_SAR;    goto do_AvBin8x16;
      case Iop_Rol8x16:    op = Pav_ROTL;   goto do_AvBin8x16;
      case Iop_InterleaveHI8x16: op = Pav_MRGHI;  goto do_AvBin8x16;
      case Iop_InterleaveLO8x16: op = Pav_MRGLO;  goto do_AvBin8x16;
      case Iop_Add8x16:    op = Pav_ADDU;   goto do_AvBin8x16;
      case Iop_QAdd8Ux16:  op = Pav_QADDU;  goto do_AvBin8x16;
      case Iop_QAdd8Sx16:  op = Pav_QADDS;  goto do_AvBin8x16;
      case Iop_Sub8x16:    op = Pav_SUBU;   goto do_AvBin8x16;
      case Iop_QSub8Ux16:  op = Pav_QSUBU;  goto do_AvBin8x16;
      case Iop_QSub8Sx16:  op = Pav_QSUBS;  goto do_AvBin8x16;
      case Iop_Avg8Ux16:   op = Pav_AVGU;   goto do_AvBin8x16;
      case Iop_Avg8Sx16:   op = Pav_AVGS;   goto do_AvBin8x16;
      case Iop_Max8Ux16:   op = Pav_MAXU;   goto do_AvBin8x16;
      case Iop_Max8Sx16:   op = Pav_MAXS;   goto do_AvBin8x16;
      case Iop_Min8Ux16:   op = Pav_MINU;   goto do_AvBin8x16;
      case Iop_Min8Sx16:   op = Pav_MINS;   goto do_AvBin8x16;
      case Iop_MullEven8Ux16: op = Pav_OMULU;  goto do_AvBin8x16;
      case Iop_MullEven8Sx16: op = Pav_OMULS;  goto do_AvBin8x16;
      case Iop_CmpEQ8x16:  op = Pav_CMPEQU; goto do_AvBin8x16;
      case Iop_CmpGT8Ux16: op = Pav_CMPGTU; goto do_AvBin8x16;
      case Iop_CmpGT8Sx16: op = Pav_CMPGTS; goto do_AvBin8x16;
      do_AvBin8x16: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegV(env);
         addInstr(env, PPC32Instr_AvBin8x16(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_Shl16x8:    op = Pav_SHL;    goto do_AvBin16x8;
      case Iop_Shr16x8:    op = Pav_SHR;    goto do_AvBin16x8;
      case Iop_Sar16x8:    op = Pav_SAR;    goto do_AvBin16x8;
      case Iop_Rol16x8:    op = Pav_ROTL;   goto do_AvBin16x8;
      case Iop_Narrow16x8:       op = Pav_PACKUU;  goto do_AvBin16x8;
      case Iop_QNarrow16Ux8:     op = Pav_QPACKUU; goto do_AvBin16x8;
      case Iop_QNarrow16Sx8:     op = Pav_QPACKSS; goto do_AvBin16x8;
      case Iop_InterleaveHI16x8: op = Pav_MRGHI;  goto do_AvBin16x8;
      case Iop_InterleaveLO16x8: op = Pav_MRGLO;  goto do_AvBin16x8;
      case Iop_Add16x8:    op = Pav_ADDU;   goto do_AvBin16x8;
      case Iop_QAdd16Ux8:  op = Pav_QADDU;  goto do_AvBin16x8;
      case Iop_QAdd16Sx8:  op = Pav_QADDS;  goto do_AvBin16x8;
      case Iop_Sub16x8:    op = Pav_SUBU;   goto do_AvBin16x8;
      case Iop_QSub16Ux8:  op = Pav_QSUBU;  goto do_AvBin16x8;
      case Iop_QSub16Sx8:  op = Pav_QSUBS;  goto do_AvBin16x8;
      case Iop_Avg16Ux8:   op = Pav_AVGU;   goto do_AvBin16x8;
      case Iop_Avg16Sx8:   op = Pav_AVGS;   goto do_AvBin16x8;
      case Iop_Max16Ux8:   op = Pav_MAXU;   goto do_AvBin16x8;
      case Iop_Max16Sx8:   op = Pav_MAXS;   goto do_AvBin16x8;
      case Iop_Min16Ux8:   op = Pav_MINU;   goto do_AvBin16x8;
      case Iop_Min16Sx8:   op = Pav_MINS;   goto do_AvBin16x8;
      case Iop_MullEven16Ux8: op = Pav_OMULU;  goto do_AvBin16x8;
      case Iop_MullEven16Sx8: op = Pav_OMULS;  goto do_AvBin16x8;
      case Iop_CmpEQ16x8:  op = Pav_CMPEQU; goto do_AvBin16x8;
      case Iop_CmpGT16Ux8: op = Pav_CMPGTU; goto do_AvBin16x8;
      case Iop_CmpGT16Sx8: op = Pav_CMPGTS; goto do_AvBin16x8;
      do_AvBin16x8: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegV(env);
         addInstr(env, PPC32Instr_AvBin16x8(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_Shl32x4:    op = Pav_SHL;    goto do_AvBin32x4;
      case Iop_Shr32x4:    op = Pav_SHR;    goto do_AvBin32x4;
      case Iop_Sar32x4:    op = Pav_SAR;    goto do_AvBin32x4;
      case Iop_Rol32x4:    op = Pav_ROTL;   goto do_AvBin32x4;
      case Iop_Narrow32x4:       op = Pav_PACKUU;  goto do_AvBin32x4;
      case Iop_QNarrow32Ux4:     op = Pav_QPACKUU; goto do_AvBin32x4;
      case Iop_QNarrow32Sx4:     op = Pav_QPACKSS; goto do_AvBin32x4;
      case Iop_InterleaveHI32x4: op = Pav_MRGHI;  goto do_AvBin32x4;
      case Iop_InterleaveLO32x4: op = Pav_MRGLO;  goto do_AvBin32x4;
      case Iop_Add32x4:    op = Pav_ADDU;   goto do_AvBin32x4;
      case Iop_QAdd32Ux4:  op = Pav_QADDU;  goto do_AvBin32x4;
      case Iop_QAdd32Sx4:  op = Pav_QADDS;  goto do_AvBin32x4;
      case Iop_Sub32x4:    op = Pav_SUBU;   goto do_AvBin32x4;
      case Iop_QSub32Ux4:  op = Pav_QSUBU;  goto do_AvBin32x4;
      case Iop_QSub32Sx4:  op = Pav_QSUBS;  goto do_AvBin32x4;
      case Iop_Avg32Ux4:   op = Pav_AVGU;   goto do_AvBin32x4;
      case Iop_Avg32Sx4:   op = Pav_AVGS;   goto do_AvBin32x4;
      case Iop_Max32Ux4:   op = Pav_MAXU;   goto do_AvBin32x4;
      case Iop_Max32Sx4:   op = Pav_MAXS;   goto do_AvBin32x4;
      case Iop_Min32Ux4:   op = Pav_MINU;   goto do_AvBin32x4;
      case Iop_Min32Sx4:   op = Pav_MINS;   goto do_AvBin32x4;
      case Iop_CmpEQ32x4:  op = Pav_CMPEQU; goto do_AvBin32x4;
      case Iop_CmpGT32Ux4: op = Pav_CMPGTU; goto do_AvBin32x4;
      case Iop_CmpGT32Sx4: op = Pav_CMPGTS; goto do_AvBin32x4;
      do_AvBin32x4: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegV(env);
         addInstr(env, PPC32Instr_AvBin32x4(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_ShlN8x16: op = Pav_SHL; goto do_AvShift8x16;
      case Iop_SarN8x16: op = Pav_SAR; goto do_AvShift8x16;
      do_AvShift8x16: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_AvBin8x16(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShlN16x8: op = Pav_SHL; goto do_AvShift16x8;
      case Iop_ShrN16x8: op = Pav_SHR; goto do_AvShift16x8;
      case Iop_SarN16x8: op = Pav_SAR; goto do_AvShift16x8;
      do_AvShift16x8: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_AvBin16x8(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShlN32x4: op = Pav_SHL; goto do_AvShift32x4;
      case Iop_ShrN32x4: op = Pav_SHR; goto do_AvShift32x4;
      case Iop_SarN32x4: op = Pav_SAR; goto do_AvShift32x4;
      do_AvShift32x4: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_AvBin32x4(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShrV128: op = Pav_SHR; goto do_AvShiftV128;
      case Iop_ShlV128: op = Pav_SHL; goto do_AvShiftV128;
      do_AvShiftV128: {
         HReg dst    = newVRegV(env);
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2);
         /* Note: shift value gets masked by 127 */
         addInstr(env, PPC32Instr_AvBinary(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_Perm8x16: {
         HReg dst   = newVRegV(env);
         HReg v_src = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg v_ctl = iselVecExpr(env, e->Iex.Binop.arg2);
         addInstr(env, PPC32Instr_AvPerm(dst, v_src, v_src, v_ctl));
         return dst;
      }

      default:
         break;
      } /* switch (e->Iex.Binop.op) */
   } /* if (e->tag == Iex_Binop) */

//..    if (e->tag == Iex_Mux0X) {
//..       HReg r8  = iselIntExpr_R(env, e->Iex.Mux0X.cond);
//..       HReg rX  = iselVecExpr(env, e->Iex.Mux0X.exprX);
//..       HReg r0  = iselVecExpr(env, e->Iex.Mux0X.expr0);
//..       HReg dst = newVRegV(env);
//..       addInstr(env, mk_vMOVsd_RR(rX,dst));
//..       addInstr(env, X86Instr_Test32(X86RI_Imm(0xFF), X86RM_Reg(r8)));
//..       addInstr(env, X86Instr_SseCMov(Xcc_Z,r0,dst));
//..       return dst;
//..    }

   // unused:   vec_fail:
   vex_printf("iselVecExpr(ppc32) (subarch = %s): can't reduce\n",
              LibVEX_ppVexSubArch(env->subarch));
   ppIRExpr(e);
   vpanic("iselVecExpr_wrk(ppc32)");
}


/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void iselStmt ( ISelEnv* env, IRStmt* stmt )
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n -- ");
      ppIRStmt(stmt);
      vex_printf("\n");
   }

   switch (stmt->tag) {

   /* --------- STORE --------- */
   case Ist_Store: {
      PPC32AMode* am_addr;
      IRType    tya = typeOfIRExpr(env->type_env, stmt->Ist.Store.addr);
      IRType    tyd = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);
      IREndness end = stmt->Ist.Store.end;

      if (tya != Ity_I32 || end != Iend_BE) 
         goto stmt_fail;

      am_addr = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
      if (tyd == Ity_I8 || tyd == Ity_I16 || tyd == Ity_I32) {
         HReg r_src = iselIntExpr_R(env, stmt->Ist.Store.data);
         addInstr(env, PPC32Instr_Store( toUChar(sizeofIRType(tyd)), 
                                         am_addr, r_src));
         return;
      }
      if (tyd == Ity_F64) {
         HReg fr_src = iselDblExpr(env, stmt->Ist.Store.data);
         addInstr(env, PPC32Instr_FpLdSt(False/*store*/, 8, fr_src, am_addr));
         return;
      }
      if (tyd == Ity_F32) {
         HReg fr_src = iselFltExpr(env, stmt->Ist.Store.data);
         addInstr(env, PPC32Instr_FpLdSt(False/*store*/, 4, fr_src, am_addr));
         return;
      }
//..       if (tyd == Ity_I64) {
//..          HReg vHi, vLo, rA;
//..          iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Store.data);
//..          rA = iselIntExpr_R(env, stmt->Ist.Store.addr);
//..          addInstr(env, X86Instr_Alu32M(
//..                           Xalu_MOV, X86RI_Reg(vLo), X86AMode_IR(0, rA)));
//..          addInstr(env, X86Instr_Alu32M(
//..                           Xalu_MOV, X86RI_Reg(vHi), X86AMode_IR(4, rA)));
//..          return;
//..       }
      if (tyd == Ity_V128) {
         HReg v_src = iselVecExpr(env, stmt->Ist.Store.data);
         addInstr(env, PPC32Instr_AvLdSt(False/*store*/, 16, v_src, am_addr));
         return;
      }
      break;
   }

   /* --------- PUT --------- */
   case Ist_Put: {
      IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
      if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32) {
         HReg r_src = iselIntExpr_R(env, stmt->Ist.Put.data);
         PPC32AMode* am_addr = PPC32AMode_IR(stmt->Ist.Put.offset, GuestStatePtr);
         addInstr(env, PPC32Instr_Store( toUChar(sizeofIRType(ty)), 
                                         am_addr, r_src ));
         return;
      }
      if (ty == Ity_I64) {
         HReg rHi, rLo;
         PPC32AMode* am_addr  = PPC32AMode_IR(stmt->Ist.Put.offset, GuestStatePtr);
         PPC32AMode* am_addr4 = advance4(env, am_addr);
         iselInt64Expr(&rHi,&rLo, env, stmt->Ist.Put.data);
         addInstr(env, PPC32Instr_Store( 4, am_addr,  rHi ));
         addInstr(env, PPC32Instr_Store( 4, am_addr4, rLo ));
         return;
     }
     if (ty == Ity_V128) {
         /* Guest state vectors are 16byte aligned, so don't need to worry here */
         HReg v_src = iselVecExpr(env, stmt->Ist.Put.data);
         PPC32AMode* am_addr  = PPC32AMode_IR(stmt->Ist.Put.offset, GuestStatePtr);
         addInstr(env, PPC32Instr_AvLdSt(False/*store*/, 16, v_src, am_addr));
         return;
      }
//..       if (ty == Ity_F32) {
//..          HReg f32 = iselFltExpr(env, stmt->Ist.Put.data);
//..          X86AMode* am  = X86AMode_IR(stmt->Ist.Put.offset, hregX86_EBP());
//..          set_FPU_rounding_default(env); /* paranoia */
//..          addInstr(env, X86Instr_FpLdSt( False/*store*/, 4, f32, am ));
//..          return;
//..       }
      if (ty == Ity_F64) {
         HReg fr_src = iselDblExpr(env, stmt->Ist.Put.data);
         PPC32AMode* am_addr = PPC32AMode_IR(stmt->Ist.Put.offset, GuestStatePtr);
         addInstr(env, PPC32Instr_FpLdSt( False/*store*/, 8, fr_src, am_addr ));
         return;
      }
      break;
   }
      
//..    /* --------- Indexed PUT --------- */
//..    case Ist_PutI: {
//..       X86AMode* am 
//..          = genGuestArrayOffset(
//..               env, stmt->Ist.PutI.descr, 
//..                    stmt->Ist.PutI.ix, stmt->Ist.PutI.bias );
//.. 
//..       IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.PutI.data);
//..       if (ty == Ity_F64) {
//..          HReg val = iselDblExpr(env, stmt->Ist.PutI.data);
//..          addInstr(env, X86Instr_FpLdSt( False/*store*/, 8, val, am ));
//..          return;
//..       }
//..       if (ty == Ity_I8) {
//..          HReg r = iselIntExpr_R(env, stmt->Ist.PutI.data);
//..          addInstr(env, X86Instr_Store( 1, r, am ));
//..          return;
//..       }
//..       if (ty == Ity_I64) {
//..          HReg rHi, rLo;
//..          X86AMode* am4 = advance4(am);
//..          iselInt64Expr(&rHi, &rLo, env, stmt->Ist.PutI.data);
//..          addInstr(env, X86Instr_Alu32M( Xalu_MOV, X86RI_Reg(rLo), am ));
//..          addInstr(env, X86Instr_Alu32M( Xalu_MOV, X86RI_Reg(rHi), am4 ));
//..          return;
//..       }
//..       break;
//..    }

   /* --------- TMP --------- */
   case Ist_Tmp: {
      IRTemp tmp = stmt->Ist.Tmp.tmp;
      IRType ty = typeOfIRTemp(env->type_env, tmp);
      if (ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
         HReg r_dst = lookupIRTemp(env, tmp);
         HReg r_src = iselIntExpr_R(env, stmt->Ist.Tmp.data);
         addInstr(env, mk_iMOVds_RR( r_dst, r_src ));
         return;
      }
      if (ty == Ity_I64) {
         HReg r_srcHi, r_srcLo, r_dstHi, r_dstLo;
         iselInt64Expr(&r_srcHi,&r_srcLo, env, stmt->Ist.Tmp.data);
         lookupIRTemp64( &r_dstHi, &r_dstLo, env, tmp);
         addInstr(env, mk_iMOVds_RR(r_dstHi, r_srcHi) );
         addInstr(env, mk_iMOVds_RR(r_dstLo, r_srcLo) );
         return;
      }
      if (ty == Ity_I1) {
         PPC32CondCode cond = iselCondCode(env, stmt->Ist.Tmp.data);
         HReg r_dst = lookupIRTemp(env, tmp);
         addInstr(env, PPC32Instr_Set32(cond, r_dst));
         return;
      }
      if (ty == Ity_F64) {
         HReg fr_dst = lookupIRTemp(env, tmp);
         HReg fr_src = iselDblExpr(env, stmt->Ist.Tmp.data);
         addInstr(env, PPC32Instr_FpUnary(Pfp_MOV, fr_dst, fr_src));
         return;
      }
      if (ty == Ity_F32) {
         HReg fr_dst = lookupIRTemp(env, tmp);
         HReg fr_src = iselFltExpr(env, stmt->Ist.Tmp.data);
         addInstr(env, PPC32Instr_FpUnary(Pfp_MOV, fr_dst, fr_src));
         return;
      }
      if (ty == Ity_V128) {
         HReg v_dst = lookupIRTemp(env, tmp);
         HReg v_src = iselVecExpr(env, stmt->Ist.Tmp.data);
         addInstr(env, PPC32Instr_AvUnary(Pav_MOV, v_dst, v_src));
         return;
      }
      break;
   }

   /* --------- Call to DIRTY helper --------- */
   case Ist_Dirty: {
      IRType   retty;
      IRDirty* d = stmt->Ist.Dirty.details;
      Bool     passBBP = False;

      if (d->nFxState == 0)
         vassert(!d->needsBBP);
      passBBP = toBool(d->nFxState > 0 && d->needsBBP);

      /* Marshal args, do the call, clear stack. */
      doHelperCall( env, passBBP, d->guard, d->cee, d->args );

      /* Now figure out what to do with the returned value, if any. */
      if (d->tmp == IRTemp_INVALID)
         /* No return value.  Nothing to do. */
         return;

      retty = typeOfIRTemp(env->type_env, d->tmp);
      if (retty == Ity_I64) {
         HReg r_dstHi, r_dstLo;
         /* The returned value is in %r3:%r4.  Park it in the
            register-pair associated with tmp. */
         lookupIRTemp64( &r_dstHi, &r_dstLo, env, d->tmp);
         addInstr(env, mk_iMOVds_RR(r_dstHi, hregPPC32_GPR3()));
         addInstr(env, mk_iMOVds_RR(r_dstLo, hregPPC32_GPR4()));
         return;
      }
      if (retty == Ity_I32 || retty == Ity_I16 || retty == Ity_I8) {
         /* The returned value is in %r3.  Park it in the register
            associated with tmp. */
         HReg r_dst = lookupIRTemp(env, d->tmp);
         addInstr(env, mk_iMOVds_RR(r_dst, hregPPC32_GPR3()));
         return;
      }
      break;
   }

   /* --------- MEM FENCE --------- */
   case Ist_MFence:
      addInstr(env, PPC32Instr_MFence());
      return;

   /* --------- INSTR MARK --------- */
   /* Doesn't generate any executable code ... */
   case Ist_IMark:
       return;

   /* --------- NO-OP --------- */
   /* Fairly self-explanatory, wouldn't you say? */
   case Ist_NoOp:
       return;

   /* --------- EXIT --------- */
   case Ist_Exit: {
      PPC32RI*      ri_dst;
      PPC32CondCode cc;
      if (stmt->Ist.Exit.dst->tag != Ico_U32)
         vpanic("isel_ppc32: Ist_Exit: dst is not a 32-bit value");
      ri_dst = iselIntExpr_RI(env, IRExpr_Const(stmt->Ist.Exit.dst));
      cc     = iselCondCode(env,stmt->Ist.Exit.guard);
      addInstr(env, PPC32Instr_RdWrLR(True, env->savedLR));
      addInstr(env, PPC32Instr_Goto(stmt->Ist.Exit.jk, cc, ri_dst));
      return;
   }

   default: break;
   }
  stmt_fail:
   ppIRStmt(stmt);
   vpanic("iselStmt(ppc32)");
}
 

/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

static void iselNext ( ISelEnv* env, IRExpr* next, IRJumpKind jk )
{
   PPC32CondCode cond;
   PPC32RI* ri;
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- goto {");
      ppIRJumpKind(jk);
      vex_printf("} ");
      ppIRExpr(next);
      vex_printf("\n");
   }
   cond = mk_PPCCondCode( Pct_ALWAYS, Pcf_7EQ );
   ri = iselIntExpr_RI(env, next);
   addInstr(env, PPC32Instr_RdWrLR(True, env->savedLR));
   addInstr(env, PPC32Instr_Goto(jk, cond, ri));
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire BB to ppc32 code. */

HInstrArray* iselBB_PPC32 ( IRBB* bb, VexArchInfo* archinfo_host )
{
   Int        i, j;
   HReg       hreg, hregHI;
   ISelEnv*   env;
   VexSubArch subarch_host = archinfo_host->subarch;

   /* sanity ... */
   vassert(subarch_host == VexSubArchPPC32_I
           || subarch_host == VexSubArchPPC32_FI
           || subarch_host == VexSubArchPPC32_VFI);

   /* Make up an initial environment to use. */
   env = LibVEX_Alloc(sizeof(ISelEnv));
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

   /* and finally ... */
   env->subarch = subarch_host;

   /* For each IR temporary, allocate a suitably-kinded virtual
      register. */
   j = 0;
   for (i = 0; i < env->n_vregmap; i++) {
      hregHI = hreg = INVALID_HREG;
      switch (bb->tyenv->types[i]) {
      case Ity_I1:
      case Ity_I8:
      case Ity_I16:
      case Ity_I32:  hreg   = mkHReg(j++, HRcInt32,  True); break;
      case Ity_I64:  hreg   = mkHReg(j++, HRcInt32,  True);
                     hregHI = mkHReg(j++, HRcInt32,  True); break;
      case Ity_F32:
      case Ity_F64:  hreg   = mkHReg(j++, HRcFlt64,  True); break;
      case Ity_V128: hreg   = mkHReg(j++, HRcVec128, True); break;
      default:
         ppIRType(bb->tyenv->types[i]);
         vpanic("iselBB(ppc32): IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* Keep a copy of the link reg, so helper functions don't kill it. */
   env->savedLR = newVRegI(env);
   addInstr(env, PPC32Instr_RdWrLR(False, env->savedLR));

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
/*--- end                                   host-ppc32/isel.c ---*/
/*---------------------------------------------------------------*/
