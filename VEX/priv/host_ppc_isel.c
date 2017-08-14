
/*---------------------------------------------------------------*/
/*--- begin                                   host_ppc_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "ir_match.h"
#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_ppc_defs.h"

/* GPR register class for ppc32/64 */
#define HRcGPR(__mode64) (__mode64 ? HRcInt64 : HRcInt32)


/*---------------------------------------------------------*/
/*--- Register Usage Conventions                        ---*/
/*---------------------------------------------------------*/
/*
  Integer Regs
  ------------
  GPR0       Reserved
  GPR1       Stack Pointer
  GPR2       not used - TOC pointer
  GPR3:10    Allocateable
  GPR11      if mode64: not used - calls by ptr / env ptr for some langs
  GPR12      if mode64: not used - exceptions / global linkage code
  GPR13      not used - Thread-specific pointer
  GPR14:28   Allocateable
  GPR29      Unused by us (reserved for the dispatcher)
  GPR30      AltiVec temp spill register
  GPR31      GuestStatePointer

  Of Allocateable regs:
  if (mode64)
    GPR3:10  Caller-saved regs
  else
    GPR3:12  Caller-saved regs
  GPR14:29   Callee-saved regs

  GPR3       [Return | Parameter] - carrying reg
  GPR4:10    Parameter-carrying regs


  Floating Point Regs
  -------------------
  FPR0:31    Allocateable

  FPR0       Caller-saved - scratch reg
  if (mode64)
    FPR1:13  Caller-saved - param & return regs
  else
    FPR1:8   Caller-saved - param & return regs
    FPR9:13  Caller-saved regs
  FPR14:31   Callee-saved regs


  Vector Regs (on processors with the VMX feature)
  -----------
  VR0-VR1    Volatile scratch registers
  VR2-VR13   Volatile vector parameters registers
  VR14-VR19  Volatile scratch registers
  VR20-VR31  Non-volatile registers
  VRSAVE     Non-volatile 32-bit register
*/


/*---------------------------------------------------------*/
/*--- PPC FP Status & Control Register Conventions      ---*/
/*---------------------------------------------------------*/
/*
  Vex-generated code expects to run with the FPU set as follows: all
  exceptions masked.  The rounding mode is set appropriately before
  each floating point insn emitted (or left unchanged if known to be
  correct already).  There are a few fp insns (fmr,fneg,fabs,fnabs),
  which are unaffected by the rm and so the rounding mode is not set
  prior to them.  

  At least on MPC7447A (Mac Mini), frsqrte is also not affected by
  rounding mode.  At some point the ppc docs get sufficiently vague
  that the only way to find out is to write test programs.
*/
/* Notes on the FP instruction set, 6 Feb 06.

What                 exns -> CR1 ?   Sets FPRF ?   Observes RM ?
-------------------------------------------------------------

fmr[.]                   if .             n             n
fneg[.]                  if .             n             n
fabs[.]                  if .             n             n
fnabs[.]                 if .             n             n

fadd[.]                  if .             y             y
fadds[.]                 if .             y             y
fcfid[.] (i64->dbl)      if .             y             y
fcmpo (cmp, result       n                n             n
fcmpu  to crfD)          n                n             n
fctid[.]  (dbl->i64)     if .       ->undef             y
fctidz[.] (dbl->i64)     if .       ->undef    rounds-to-zero
fctiw[.]  (dbl->i32)     if .       ->undef             y
fctiwz[.] (dbl->i32)     if .       ->undef    rounds-to-zero
fdiv[.]                  if .             y             y
fdivs[.]                 if .             y             y
fmadd[.]                 if .             y             y
fmadds[.]                if .             y             y
fmsub[.]                 if .             y             y
fmsubs[.]                if .             y             y
fmul[.]                  if .             y             y
fmuls[.]                 if .             y             y

(note: for fnm*, rounding happens before final negation)
fnmadd[.]                if .             y             y
fnmadds[.]               if .             y             y
fnmsub[.]                if .             y             y
fnmsubs[.]               if .             y             y

fre[.]                   if .             y             y
fres[.]                  if .             y             y

frsqrte[.]               if .             y       apparently not

fsqrt[.]                 if .             y             y
fsqrts[.]                if .             y             y
fsub[.]                  if .             y             y
fsubs[.]                 if .             y             y


fpscr: bits 30-31 (ibm) is RM
            24-29 (ibm) are exnmasks/non-IEEE bit, all zero
	    15-19 (ibm) is FPRF: class, <, =, >, UNord

ppc fe(guest) makes fpscr read as all zeros except RM (and maybe FPRF
in future) 

mcrfs     - move fpscr field to CR field
mtfsfi[.] - 4 bit imm moved to fpscr field
mtfsf[.]  - move frS[low 1/2] to fpscr but using 8-bit field mask
mtfsb1[.] - set given fpscr bit
mtfsb0[.] - clear given fpscr bit
mffs[.]   - move all fpscr to frD[low 1/2]

For [.] presumably cr1 is set with exn summary bits, as per 
main FP insns

A single precision store truncates/denormalises the in-register value,
but does not round it.  This is so that flds followed by fsts is
always the identity.
*/


/*---------------------------------------------------------*/
/*--- misc helpers                                      ---*/
/*---------------------------------------------------------*/

/* These are duplicated in guest-ppc/toIR.c */
static IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

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
         - vregmapHI holds the secondary register for the IRTemp,
              if any is needed.  That's only for Ity_I64 temps
              in 32 bit mode or Ity_I128 temps in 64-bit mode.

    - The name of the vreg in which we stash a copy of the link reg,
      so helper functions don't kill it.

    - The code array, that is, the insns selected so far.
 
    - A counter, for generating new virtual registers.
 
    - The host subarchitecture we are selecting insns for.  
      This is set at the start and does not change.
 
    - A Bool to tell us if the host is 32 or 64bit.
      This is set at the start and does not change.
 
    - An IRExpr*, which may be NULL, holding the IR expression (an
      IRRoundingMode-encoded value) to which the FPU's rounding mode
      was most recently set.  Setting to NULL is always safe.  Used to
      avoid redundant settings of the FPU's rounding mode, as
      described in set_FPU_rounding_mode below.

    - A VexMiscInfo*, needed for knowing how to generate
      function calls for this target
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
 
      /* 27 Jan 06: Not currently used, but should be */
      UInt         hwcaps;

      Bool         mode64;

      IRExpr*      previous_rm;

      VexAbiInfo*  vbi;
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
   vassert(!env->mode64);
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   vassert(env->vregmapHI[tmp] != INVALID_HREG);
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr ( ISelEnv* env, PPCInstr* instr )
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppPPCInstr(instr, env->mode64);
      vex_printf("\n");
   }
}

static HReg newVRegI ( ISelEnv* env )
{   
   HReg reg = mkHReg(env->vreg_ctr, HRcGPR(env->mode64),
                     True/*virtual reg*/);
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

   'Word' refers to the size of the native machine word, that is,
   32-bit int in 32-bit mode and 64-bit int in 64-bit mode.  '2Word'
   therefore refers to a double-width (64/128-bit) quantity in two
   integer registers.
*/
/* 32-bit mode: compute an I8/I16/I32 into a GPR.
   64-bit mode: compute an I8/I16/I32/I64 into a GPR. */
static HReg          iselWordExpr_R_wrk ( ISelEnv* env, IRExpr* e );
static HReg          iselWordExpr_R     ( ISelEnv* env, IRExpr* e );

/* 32-bit mode: Compute an I8/I16/I32 into a RH
                (reg-or-halfword-immediate).
   64-bit mode: Compute an I8/I16/I32/I64 into a RH
                (reg-or-halfword-immediate).
   It's important to specify whether the immediate is to be regarded
   as signed or not.  If yes, this will never return -32768 as an
   immediate; this guaranteed that all signed immediates that are
   return can have their sign inverted if need be. 
*/
static PPCRH*        iselWordExpr_RH_wrk ( ISelEnv* env, 
                                           Bool syned, IRExpr* e );
static PPCRH*        iselWordExpr_RH     ( ISelEnv* env, 
                                           Bool syned, IRExpr* e );

/* 32-bit mode: compute an I32 into a RI (reg or 32-bit immediate).
   64-bit mode: compute an I64 into a RI (reg or 64-bit immediate). */
static PPCRI*        iselWordExpr_RI_wrk ( ISelEnv* env, IRExpr* e );
static PPCRI*        iselWordExpr_RI     ( ISelEnv* env, IRExpr* e );

/* In 32 bit mode ONLY, compute an I8 into a
   reg-or-5-bit-unsigned-immediate, the latter being an immediate in
   the range 1 .. 31 inclusive.  Used for doing shift amounts. */
static PPCRH*        iselWordExpr_RH5u_wrk ( ISelEnv* env, IRExpr* e );
static PPCRH*        iselWordExpr_RH5u     ( ISelEnv* env, IRExpr* e );

/* In 64-bit mode ONLY, compute an I8 into a
   reg-or-6-bit-unsigned-immediate, the latter being an immediate in
   the range 1 .. 63 inclusive.  Used for doing shift amounts. */
static PPCRH*        iselWordExpr_RH6u_wrk ( ISelEnv* env, IRExpr* e );
static PPCRH*        iselWordExpr_RH6u     ( ISelEnv* env, IRExpr* e );

/* 32-bit mode: compute an I32 into an AMode.
   64-bit mode: compute an I64 into an AMode.

   Requires to know (xferTy) the type of data to be loaded/stored
   using this amode.  That is so that, for 64-bit code generation, any
   PPCAMode_IR returned will have an index (immediate offset) field
   that is guaranteed to be 4-aligned, if there is any chance that the
   amode is to be used in ld/ldu/lda/std/stdu.

   Since there are no such restrictions on 32-bit insns, xferTy is
   ignored for 32-bit code generation. */
static PPCAMode*     iselWordExpr_AMode_wrk ( ISelEnv* env, IRExpr* e, IRType xferTy );
static PPCAMode*     iselWordExpr_AMode     ( ISelEnv* env, IRExpr* e, IRType xferTy );

/* 32-bit mode ONLY: compute an I64 into a GPR pair. */
static void          iselInt64Expr_wrk ( HReg* rHi, HReg* rLo, 
                                         ISelEnv* env, IRExpr* e );
static void          iselInt64Expr     ( HReg* rHi, HReg* rLo, 
                                         ISelEnv* env, IRExpr* e );

/* 64-bit mode ONLY: compute an I128 into a GPR64 pair. */
static void          iselInt128Expr_wrk ( HReg* rHi, HReg* rLo, 
                                          ISelEnv* env, IRExpr* e );
static void          iselInt128Expr     ( HReg* rHi, HReg* rLo, 
                                          ISelEnv* env, IRExpr* e );

static PPCCondCode   iselCondCode_wrk ( ISelEnv* env, IRExpr* e );
static PPCCondCode   iselCondCode     ( ISelEnv* env, IRExpr* e );

static HReg          iselDblExpr_wrk ( ISelEnv* env, IRExpr* e );
static HReg          iselDblExpr     ( ISelEnv* env, IRExpr* e );

static HReg          iselFltExpr_wrk ( ISelEnv* env, IRExpr* e );
static HReg          iselFltExpr     ( ISelEnv* env, IRExpr* e );

static HReg          iselVecExpr_wrk ( ISelEnv* env, IRExpr* e );
static HReg          iselVecExpr     ( ISelEnv* env, IRExpr* e );


/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

/* Make an int reg-reg move. */

static PPCInstr* mk_iMOVds_RR ( HReg r_dst, HReg r_src )
{
   vassert(hregClass(r_dst) == hregClass(r_src));
   vassert(hregClass(r_src) ==  HRcInt32 ||
           hregClass(r_src) ==  HRcInt64);
   return PPCInstr_Alu(Palu_OR, r_dst, r_src, PPCRH_Reg(r_src));
}

/* Advance/retreat %r1 by n. */

static void add_to_sp ( ISelEnv* env, UInt n )
{
   HReg sp = StackFramePtr(env->mode64);
   vassert(n < 256 && (n%16) == 0);
   addInstr(env, PPCInstr_Alu( Palu_ADD, sp, sp,
                               PPCRH_Imm(True,toUShort(n)) ));
}

static void sub_from_sp ( ISelEnv* env, UInt n )
{
   HReg sp = StackFramePtr(env->mode64);
   vassert(n < 256 && (n%16) == 0);
   addInstr(env, PPCInstr_Alu( Palu_SUB, sp, sp,
                               PPCRH_Imm(True,toUShort(n)) ));
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
   addInstr(env, mk_iMOVds_RR(r, StackFramePtr(env->mode64)));
   // add 16
   addInstr(env, PPCInstr_Alu( Palu_ADD, r, r,
                               PPCRH_Imm(True,toUShort(16)) ));
   // mask to quadword
   addInstr(env,
            PPCInstr_LI(align16, 0xFFFFFFFFFFFFFFF0ULL, env->mode64));
   addInstr(env, PPCInstr_Alu(Palu_AND, r,r, PPCRH_Reg(align16)));
   return r;
}



/* Load 2*I32 regs to fp reg */
static HReg mk_LoadRR32toFPR ( ISelEnv* env,
                               HReg r_srcHi, HReg r_srcLo )
{
   HReg fr_dst = newVRegF(env);
   PPCAMode *am_addr0, *am_addr1;

   vassert(!env->mode64);
   vassert(hregClass(r_srcHi) == HRcInt32);
   vassert(hregClass(r_srcLo) == HRcInt32);

   sub_from_sp( env, 16 );        // Move SP down 16 bytes
   am_addr0 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
   am_addr1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );

   // store hi,lo as Ity_I32's
   addInstr(env, PPCInstr_Store( 4, am_addr0, r_srcHi, env->mode64 ));
   addInstr(env, PPCInstr_Store( 4, am_addr1, r_srcLo, env->mode64 ));

   // load as float
   addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fr_dst, am_addr0));
   
   add_to_sp( env, 16 );          // Reset SP
   return fr_dst;
}

/* Load I64 reg to fp reg */
static HReg mk_LoadR64toFPR ( ISelEnv* env, HReg r_src )
{
   HReg fr_dst = newVRegF(env);
   PPCAMode *am_addr0;

   vassert(env->mode64);
   vassert(hregClass(r_src) == HRcInt64);

   sub_from_sp( env, 16 );        // Move SP down 16 bytes
   am_addr0 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );

   // store as Ity_I64
   addInstr(env, PPCInstr_Store( 8, am_addr0, r_src, env->mode64 ));

   // load as float
   addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fr_dst, am_addr0));
   
   add_to_sp( env, 16 );          // Reset SP
   return fr_dst;
}


/* Given an amode, return one which references 4 bytes further
   along. */

static PPCAMode* advance4 ( ISelEnv* env, PPCAMode* am )
{
   PPCAMode* am4 = dopyPPCAMode( am );
   if (am4->tag == Pam_IR 
       && am4->Pam.IR.index + 4 <= 32767) {
      am4->Pam.IR.index += 4;
   } else {
      vpanic("advance4(ppc,host)");
   }
   return am4;
}


/* Given a guest-state array descriptor, an index expression and a
   bias, generate a PPCAMode pointing at the relevant piece of 
   guest state.  */
static
PPCAMode* genGuestArrayOffset ( ISelEnv* env, IRRegArray* descr,
                                IRExpr* off, Int bias )
{
   HReg rtmp, roff;
   Int  elemSz = sizeofIRType(descr->elemTy);
   Int  nElems = descr->nElems;
   Int  shift  = 0;

   /* Throw out any cases we don't need.  In theory there might be a
      day where we need to handle others, but not today. */

   if (nElems != 16 && nElems != 32)
      vpanic("genGuestArrayOffset(ppc host)(1)");

   switch (elemSz) {
      case 4:  shift = 2; break;
      case 8:  shift = 3; break;
      default: vpanic("genGuestArrayOffset(ppc host)(2)");
   }

   if (bias < -100 || bias > 100) /* somewhat arbitrarily */
      vpanic("genGuestArrayOffset(ppc host)(3)");
   if (descr->base < 0 || descr->base > 4000) /* somewhat arbitrarily */
      vpanic("genGuestArrayOffset(ppc host)(4)");

   /* Compute off into a reg, %off.  Then return:

         addi %tmp, %off, bias (if bias != 0)
         andi %tmp, nElems-1
         sldi %tmp, shift
         addi %tmp, %tmp, base
         ... Baseblockptr + %tmp ...
   */
   roff = iselWordExpr_R(env, off);
   rtmp = newVRegI(env);
   addInstr(env, PPCInstr_Alu(
                    Palu_ADD, 
                    rtmp, roff, 
                    PPCRH_Imm(True/*signed*/, toUShort(bias))));
   addInstr(env, PPCInstr_Alu(
                    Palu_AND, 
                    rtmp, rtmp, 
                    PPCRH_Imm(False/*unsigned*/, toUShort(nElems-1))));
   addInstr(env, PPCInstr_Shft(
                    Pshft_SHL, 
                    env->mode64 ? False : True/*F:64-bit, T:32-bit shift*/,
                    rtmp, rtmp, 
                    PPCRH_Imm(False/*unsigned*/, toUShort(shift))));
   addInstr(env, PPCInstr_Alu(
                    Palu_ADD, 
                    rtmp, rtmp, 
                    PPCRH_Imm(True/*signed*/, toUShort(descr->base))));
   return
      PPCAMode_RR( GuestStatePtr(env->mode64), rtmp );
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
   switch (e->tag) {
   case Iex_RdTmp: case Iex_Const: case Iex_Get: 
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
   PPCCondCode cc;
   HReg        argregs[PPC_N_REGPARMS];
   HReg        tmpregs[PPC_N_REGPARMS];
   Bool        go_fast;
   Int         n_args, i, argreg;
   UInt        argiregs;
   ULong       target;
   Bool        mode64 = env->mode64;

   /* Do we need to force use of an odd-even reg pair for 64-bit
      args? */
   Bool regalign_int64s
      = (!mode64) && env->vbi->host_ppc32_regalign_int64_args;

   /* Marshal args for a call and do the call.

      If passBBP is True, %rbp (the baseblock pointer) is to be passed
      as the first arg.

      This function only deals with a tiny set of possibilities, which
      cover all helpers in practice.  The restrictions are that only
      arguments in registers are supported, hence only PPC_N_REGPARMS x
      (mode32:32 | mode64:64) integer bits in total can be passed.
      In fact the only supported arg type is (mode32:I32 | mode64:I64).

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

   /* Note that the cee->regparms field is meaningless on PPC32/64 host
      (since there is only one calling convention) and so we always
      ignore it. */

   n_args = 0;
   for (i = 0; args[i]; i++)
      n_args++;

   if (PPC_N_REGPARMS < n_args + (passBBP ? 1 : 0)) {
      vpanic("doHelperCall(PPC): cannot currently handle > 8 args");
      // PPC_N_REGPARMS
   }
   
   argregs[0] = hregPPC_GPR3(mode64);
   argregs[1] = hregPPC_GPR4(mode64);
   argregs[2] = hregPPC_GPR5(mode64);
   argregs[3] = hregPPC_GPR6(mode64);
   argregs[4] = hregPPC_GPR7(mode64);
   argregs[5] = hregPPC_GPR8(mode64);
   argregs[6] = hregPPC_GPR9(mode64);
   argregs[7] = hregPPC_GPR10(mode64);
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
         addInstr(env, mk_iMOVds_RR( argregs[argreg],
                                     GuestStatePtr(mode64) ));
         argreg++;
      }

      for (i = 0; i < n_args; i++) {
         vassert(argreg < PPC_N_REGPARMS);
         vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32 ||
                 typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
         if (!mode64) {
            if (typeOfIRExpr(env->type_env, args[i]) == Ity_I32) { 
               argiregs |= (1 << (argreg+3));
               addInstr(env,
                        mk_iMOVds_RR( argregs[argreg],
                                      iselWordExpr_R(env, args[i]) ));
            } else { // Ity_I64
               HReg rHi, rLo;
               if (regalign_int64s && (argreg%2) == 1) 
                              // ppc32 ELF abi spec for passing LONG_LONG
                  argreg++;   // XXX: odd argreg => even rN
               vassert(argreg < PPC_N_REGPARMS-1);
               iselInt64Expr(&rHi,&rLo, env, args[i]);
               argiregs |= (1 << (argreg+3));
               addInstr(env, mk_iMOVds_RR( argregs[argreg++], rHi ));
               argiregs |= (1 << (argreg+3));
               addInstr(env, mk_iMOVds_RR( argregs[argreg], rLo));
            }
         } else { // mode64
            argiregs |= (1 << (argreg+3));
            addInstr(env, mk_iMOVds_RR( argregs[argreg],
                                        iselWordExpr_R(env, args[i]) ));
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
         addInstr(env, mk_iMOVds_RR( tmpregs[argreg],
                                     GuestStatePtr(mode64) ));
         argreg++;
      }

      for (i = 0; i < n_args; i++) {
         vassert(argreg < PPC_N_REGPARMS);
         vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32 ||
                 typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
         if (!mode64) {
            if (typeOfIRExpr(env->type_env, args[i]) == Ity_I32) { 
               tmpregs[argreg] = iselWordExpr_R(env, args[i]);
            } else { // Ity_I64
               HReg rHi, rLo;
               if (regalign_int64s && (argreg%2) == 1)
                             // ppc32 ELF abi spec for passing LONG_LONG
                  argreg++;  // XXX: odd argreg => even rN
               vassert(argreg < PPC_N_REGPARMS-1);
               iselInt64Expr(&rHi,&rLo, env, args[i]);
               tmpregs[argreg++] = rHi;
               tmpregs[argreg]   = rLo;
            }
         } else { // mode64
            tmpregs[argreg] = iselWordExpr_R(env, args[i]);
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

   target = mode64 ? Ptr_to_ULong(cee->addr) :
                     toUInt(Ptr_to_ULong(cee->addr));

   /* Finally, the call itself. */
   addInstr(env, PPCInstr_Call( cc, (Addr64)target, argiregs ));
}


/*---------------------------------------------------------*/
/*--- ISEL: FP rounding mode helpers                    ---*/
/*---------------------------------------------------------*/

///* Set FPU's rounding mode to the default */
//static 
//void set_FPU_rounding_default ( ISelEnv* env )
//{
//   HReg fr_src = newVRegF(env);
//   HReg r_src  = newVRegI(env);
//
//   /* Default rounding mode = 0x0
//      Only supporting the rounding-mode bits - the rest of FPSCR is 0x0
//       - so we can set the whole register at once (faster)
//      note: upper 32 bits ignored by FpLdFPSCR
//   */
//   addInstr(env, PPCInstr_LI(r_src, 0x0, env->mode64));
//   if (env->mode64) {
//      fr_src = mk_LoadR64toFPR( env, r_src );         // 1*I64 -> F64
//   } else {
//      fr_src = mk_LoadRR32toFPR( env, r_src, r_src ); // 2*I32 -> F64
//   }
//   addInstr(env, PPCInstr_FpLdFPSCR( fr_src ));
//}

/* Convert IR rounding mode to PPC encoding */
static HReg roundModeIRtoPPC ( ISelEnv* env, HReg r_rmIR )
{
   /* 
   rounding mode | PPC | IR
   ------------------------
   to nearest    | 00  | 00
   to zero       | 01  | 11
   to +infinity  | 10  | 10
   to -infinity  | 11  | 01
   */
   HReg r_rmPPC = newVRegI(env);
   HReg r_tmp1  = newVRegI(env);

   vassert(hregClass(r_rmIR) == HRcGPR(env->mode64));

   // r_rmPPC = XOR(r_rmIR, r_rmIR << 1) & 3
   //
   // slwi  tmp1,    r_rmIR, 1
   // xor   tmp1,    r_rmIR, tmp1
   // andi  r_rmPPC, tmp1, 3

   addInstr(env, PPCInstr_Shft(Pshft_SHL, True/*32bit shift*/,
                               r_tmp1, r_rmIR, PPCRH_Imm(False,1)));

   addInstr(env, PPCInstr_Alu( Palu_XOR, r_tmp1, r_rmIR,
                               PPCRH_Reg(r_tmp1) ));

   addInstr(env, PPCInstr_Alu( Palu_AND, r_rmPPC, r_tmp1,
                               PPCRH_Imm(False,3) ));

   return r_rmPPC;
}


/* Set the FPU's rounding mode: 'mode' is an I32-typed expression
   denoting a value in the range 0 .. 3, indicating a round mode
   encoded as per type IRRoundingMode.  Set the PPC FPSCR to have the
   same rounding.

   For speed & simplicity, we're setting the *entire* FPSCR here.

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
void set_FPU_rounding_mode ( ISelEnv* env, IRExpr* mode )
{
   HReg fr_src = newVRegF(env);
   HReg r_src;

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

   /* Only supporting the rounding-mode bits - the rest of FPSCR is
      0x0 - so we can set the whole register at once (faster). */

   // Resolve rounding mode and convert to PPC representation
   r_src = roundModeIRtoPPC( env, iselWordExpr_R(env, mode) );
   // gpr -> fpr
   if (env->mode64) {
      fr_src = mk_LoadR64toFPR( env, r_src );         // 1*I64 -> F64
   } else {
      fr_src = mk_LoadRR32toFPR( env, r_src, r_src ); // 2*I32 -> F64
   }

   // Move to FPSCR
   addInstr(env, PPCInstr_FpLdFPSCR( fr_src ));
}


/*---------------------------------------------------------*/
/*--- ISEL: vector helpers                              ---*/
/*---------------------------------------------------------*/

/* Generate all-zeroes into a new vector register.
*/
static HReg generate_zeroes_V128 ( ISelEnv* env )
{
   HReg dst = newVRegV(env);
   addInstr(env, PPCInstr_AvBinary(Pav_XOR, dst, dst, dst));
   return dst;
}

/* Generate all-ones into a new vector register.
*/
static HReg generate_ones_V128 ( ISelEnv* env )
{
   HReg dst = newVRegV(env);
   PPCVI5s * src = PPCVI5s_Imm(-1);
   addInstr(env, PPCInstr_AvSplat(8, dst, src));
   return dst;
}


/*
  Generates code for AvSplat
  - takes in IRExpr* of type 8|16|32
    returns vector reg of duplicated lanes of input
  - uses AvSplat(imm) for imms up to simm6.
    otherwise must use store reg & load vector
*/
static HReg mk_AvDuplicateRI( ISelEnv* env, IRExpr* e )
{
   HReg   r_src;
   HReg   dst = newVRegV(env);
   PPCRI* ri  = iselWordExpr_RI(env, e);
   IRType ty  = typeOfIRExpr(env->type_env,e);
   UInt   sz  = (ty == Ity_I8) ? 8 : (ty == Ity_I16) ? 16 : 32;
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
            addInstr(env, PPCInstr_AvSplat(sz, v1, PPCVI5s_Imm(-16)));
            addInstr(env, PPCInstr_AvSplat(sz, v2, PPCVI5s_Imm(simm6-16)));
            addInstr(env,
               (sz== 8) ? PPCInstr_AvBin8x16(Pav_SUBU, dst, v2, v1) :
               (sz==16) ? PPCInstr_AvBin16x8(Pav_SUBU, dst, v2, v1)
                        : PPCInstr_AvBin32x4(Pav_SUBU, dst, v2, v1) );
            return dst;
         }
         if (simm6 < -16) {          /* -32:-17 inclusive */
            HReg v1 = newVRegV(env);
            HReg v2 = newVRegV(env);
            addInstr(env, PPCInstr_AvSplat(sz, v1, PPCVI5s_Imm(-16)));
            addInstr(env, PPCInstr_AvSplat(sz, v2, PPCVI5s_Imm(simm6+16)));
            addInstr(env,
               (sz== 8) ? PPCInstr_AvBin8x16(Pav_ADDU, dst, v2, v1) :
               (sz==16) ? PPCInstr_AvBin16x8(Pav_ADDU, dst, v2, v1)
                        : PPCInstr_AvBin32x4(Pav_ADDU, dst, v2, v1) );
            return dst;
         }
         /* simplest form:              -16:15 inclusive */
         addInstr(env, PPCInstr_AvSplat(sz, dst, PPCVI5s_Imm(simm6)));
         return dst;
      }

      /* no luck; use the Slow way. */
      r_src = newVRegI(env);
      addInstr(env, PPCInstr_LI(r_src, (Long)simm32, env->mode64));
   }
   else {
      r_src = ri->Pri.Reg;
   }

   /* default case: store r_src in lowest lane of 16-aligned mem,
      load vector, splat lowest lane to dst */
   {
      /* CAB: Maybe faster to store r_src multiple times (sz dependent),
              and simply load the vector? */
      HReg r_aligned16;
      HReg v_src = newVRegV(env);
      PPCAMode *am_off12;

      sub_from_sp( env, 32 );     // Move SP down
      /* Get a 16-aligned address within our stack space */
      r_aligned16 = get_sp_aligned16( env );
      am_off12 = PPCAMode_IR( 12, r_aligned16 );

      /* Store r_src in low word of 16-aligned mem */
      addInstr(env, PPCInstr_Store( 4, am_off12, r_src, env->mode64 ));

      /* Load src to vector[low lane] */
      addInstr(env, PPCInstr_AvLdSt( True/*ld*/, 4, v_src, am_off12 ) );
      add_to_sp( env, 32 );       // Reset SP

      /* Finally, splat v_src[low_lane] to dst */
      addInstr(env, PPCInstr_AvSplat(sz, dst, PPCVI5s_Reg(v_src)));
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

   /* 32bit float => sign(1) | exponent(8) | mantissa(23)
      nan => exponent all ones, mantissa > 0 */

   addInstr(env, PPCInstr_AvBinary(Pav_AND, expt, vSrc, msk_exp));
   addInstr(env, PPCInstr_AvBin32x4(Pav_CMPEQU, expt, expt, msk_exp));
   addInstr(env, PPCInstr_AvBinary(Pav_AND, mnts, vSrc, msk_mnt));
   addInstr(env, PPCInstr_AvBin32x4(Pav_CMPGTU, mnts, mnts, zeros));
   addInstr(env, PPCInstr_AvBinary(Pav_AND, vIsNan, expt, mnts));
   return vIsNan;
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

static HReg iselWordExpr_R ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselWordExpr_R_wrk(env, e);
   /* sanity checks ... */
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif

   vassert(hregClass(r) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselWordExpr_R_wrk ( ISelEnv* env, IRExpr* e )
{
   Bool mode64 = env->mode64;
   MatchInfo mi;
   DECLARE_PATTERN(p_32to1_then_1Uto8);

   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I8 || ty == Ity_I16 ||
           ty == Ity_I32 || ((ty == Ity_I64) && mode64));

   switch (e->tag) {

   /* --------- TEMP --------- */
   case Iex_RdTmp:
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);

   /* --------- LOAD --------- */
   case Iex_Load: {
      HReg      r_dst;
      PPCAMode* am_addr;
      if (e->Iex.Load.end != Iend_BE)
         goto irreducible;
      r_dst   = newVRegI(env);
      am_addr = iselWordExpr_AMode( env, e->Iex.Load.addr, ty/*of xfer*/ );
      addInstr(env, PPCInstr_Load( toUChar(sizeofIRType(ty)), 
                                   r_dst, am_addr, mode64 ));
      return r_dst;
      /*NOTREACHED*/
   }

   /* --------- BINARY OP --------- */
   case Iex_Binop: {
      PPCAluOp  aluOp;
      PPCShftOp shftOp;

      /* Is it an addition or logical style op? */
      switch (e->Iex.Binop.op) {
      case Iop_Add8: case Iop_Add16: case Iop_Add32: case Iop_Add64:
         aluOp = Palu_ADD; break;
      case Iop_Sub8: case Iop_Sub16: case Iop_Sub32: case Iop_Sub64:
         aluOp = Palu_SUB; break;
      case Iop_And8: case Iop_And16: case Iop_And32: case Iop_And64:
         aluOp = Palu_AND; break;
      case Iop_Or8:  case Iop_Or16:  case Iop_Or32:  case Iop_Or64:
         aluOp = Palu_OR; break;
      case Iop_Xor8: case Iop_Xor16: case Iop_Xor32: case Iop_Xor64:
         aluOp = Palu_XOR; break;
      default:
         aluOp = Palu_INVALID; break;
      }
      /* For commutative ops we assume any literal
         values are on the second operand. */
      if (aluOp != Palu_INVALID) {
         HReg   r_dst   = newVRegI(env);
         HReg   r_srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1);
         PPCRH* ri_srcR = NULL;
         /* get right arg into an RH, in the appropriate way */
         switch (aluOp) {
         case Palu_ADD: case Palu_SUB:
            ri_srcR = iselWordExpr_RH(env, True/*signed*/, 
                                      e->Iex.Binop.arg2);
            break;
         case Palu_AND: case Palu_OR: case Palu_XOR:
            ri_srcR = iselWordExpr_RH(env, False/*signed*/,
                                      e->Iex.Binop.arg2);
            break;
         default:
            vpanic("iselWordExpr_R_wrk-aluOp-arg2");
         }
         addInstr(env, PPCInstr_Alu(aluOp, r_dst, r_srcL, ri_srcR));
         return r_dst;
      }

      /* a shift? */
      switch (e->Iex.Binop.op) {
      case Iop_Shl8: case Iop_Shl16: case Iop_Shl32: case Iop_Shl64:
         shftOp = Pshft_SHL; break;
      case Iop_Shr8: case Iop_Shr16: case Iop_Shr32: case Iop_Shr64:
         shftOp = Pshft_SHR; break;
      case Iop_Sar8: case Iop_Sar16: case Iop_Sar32: case Iop_Sar64:
         shftOp = Pshft_SAR; break;
      default:
         shftOp = Pshft_INVALID; break;
      }
      /* we assume any literal values are on the second operand. */
      if (shftOp != Pshft_INVALID) {
         HReg   r_dst   = newVRegI(env);
         HReg   r_srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1);
         PPCRH* ri_srcR = NULL;
         /* get right arg into an RH, in the appropriate way */
         switch (shftOp) {
         case Pshft_SHL: case Pshft_SHR: case Pshft_SAR:
            if (!mode64)
               ri_srcR = iselWordExpr_RH5u(env, e->Iex.Binop.arg2);
            else
               ri_srcR = iselWordExpr_RH6u(env, e->Iex.Binop.arg2);
            break;
         default:
            vpanic("iselIntExpr_R_wrk-shftOp-arg2");
         }
         /* widen the left arg if needed */
         if (shftOp == Pshft_SHR || shftOp == Pshft_SAR) {
            if (ty == Ity_I8 || ty == Ity_I16) {
               PPCRH* amt = PPCRH_Imm(False,
                                      toUShort(ty == Ity_I8 ? 24 : 16));
               HReg   tmp = newVRegI(env);
               addInstr(env, PPCInstr_Shft(Pshft_SHL,
                                           True/*32bit shift*/,
                                           tmp, r_srcL, amt));
               addInstr(env, PPCInstr_Shft(shftOp,
                                           True/*32bit shift*/,
                                           tmp, tmp,    amt));
               r_srcL = tmp;
               vassert(0); /* AWAITING TEST CASE */
            }
         }
         /* Only 64 expressions need 64bit shifts,
            32bit shifts are fine for all others */
         if (ty == Ity_I64) {
            vassert(mode64);
            addInstr(env, PPCInstr_Shft(shftOp, False/*64bit shift*/,
                                        r_dst, r_srcL, ri_srcR));
         } else {
            addInstr(env, PPCInstr_Shft(shftOp, True/*32bit shift*/,
                                        r_dst, r_srcL, ri_srcR));
         }
         return r_dst;
      }

      /* How about a div? */
      if (e->Iex.Binop.op == Iop_DivS32 || 
          e->Iex.Binop.op == Iop_DivU32) {
         Bool syned  = toBool(e->Iex.Binop.op == Iop_DivS32);
         HReg r_dst  = newVRegI(env);
         HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_Div(syned, True/*32bit div*/,
                                    r_dst, r_srcL, r_srcR));
         return r_dst;
      }
      if (e->Iex.Binop.op == Iop_DivS64 || 
          e->Iex.Binop.op == Iop_DivU64) {
         Bool syned  = toBool(e->Iex.Binop.op == Iop_DivS64);
         HReg r_dst  = newVRegI(env);
         HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
         vassert(mode64);
         addInstr(env, PPCInstr_Div(syned, False/*64bit div*/,
                                    r_dst, r_srcL, r_srcR));
         return r_dst;
      }

      /* No? Anyone for a mul? */
      if (e->Iex.Binop.op == Iop_Mul32
          || e->Iex.Binop.op == Iop_Mul64) {
         Bool syned       = False;
         Bool sz32        = (e->Iex.Binop.op != Iop_Mul64);
         HReg r_dst       = newVRegI(env);
         HReg r_srcL      = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_srcR      = iselWordExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_MulL(syned, False/*lo32*/, sz32,
                                     r_dst, r_srcL, r_srcR));
         return r_dst;
      }      

      /* 32 x 32 -> 64 multiply */
      if (mode64
          && (e->Iex.Binop.op == Iop_MullU32
              || e->Iex.Binop.op == Iop_MullS32)) {
         HReg tLo    = newVRegI(env);
         HReg tHi    = newVRegI(env);
         HReg r_dst  = newVRegI(env);
         Bool syned  = toBool(e->Iex.Binop.op == Iop_MullS32);
         HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_MulL(False/*signedness irrelevant*/, 
                                     False/*lo32*/, True/*32bit mul*/,
                                     tLo, r_srcL, r_srcR));
         addInstr(env, PPCInstr_MulL(syned,
                                     True/*hi32*/, True/*32bit mul*/,
                                     tHi, r_srcL, r_srcR));
         addInstr(env, PPCInstr_Shft(Pshft_SHL, False/*64bit shift*/,
                                     r_dst, tHi, PPCRH_Imm(False,32)));
         addInstr(env, PPCInstr_Alu(Palu_OR,
                                    r_dst, r_dst, PPCRH_Reg(tLo)));
         return r_dst;
      }

      /* El-mutanto 3-way compare? */
      if (e->Iex.Binop.op == Iop_CmpORD32S
          || e->Iex.Binop.op == Iop_CmpORD32U) {
         Bool   syned = toBool(e->Iex.Binop.op == Iop_CmpORD32S);
         HReg   dst   = newVRegI(env);
         HReg   srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1);
         PPCRH* srcR  = iselWordExpr_RH(env, syned, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_Cmp(syned, True/*32bit cmp*/,
                                    7/*cr*/, srcL, srcR));
         addInstr(env, PPCInstr_MfCR(dst));
         addInstr(env, PPCInstr_Alu(Palu_AND, dst, dst,
                                    PPCRH_Imm(False,7<<1)));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_CmpORD64S
          || e->Iex.Binop.op == Iop_CmpORD64U) {
         Bool   syned = toBool(e->Iex.Binop.op == Iop_CmpORD64S);
         HReg   dst   = newVRegI(env);
         HReg   srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1);
         PPCRH* srcR  = iselWordExpr_RH(env, syned, e->Iex.Binop.arg2);
         vassert(mode64);
         addInstr(env, PPCInstr_Cmp(syned, False/*64bit cmp*/,
                                    7/*cr*/, srcL, srcR));
         addInstr(env, PPCInstr_MfCR(dst));
         addInstr(env, PPCInstr_Alu(Palu_AND, dst, dst,
                                    PPCRH_Imm(False,7<<1)));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_Max32U) {
         HReg        r1   = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg        r2   = iselWordExpr_R(env, e->Iex.Binop.arg2);
         HReg        rdst = newVRegI(env);
         PPCCondCode cc   = mk_PPCCondCode( Pct_TRUE, Pcf_7LT );
         addInstr(env, mk_iMOVds_RR(rdst, r1));
         addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                    7/*cr*/, rdst, PPCRH_Reg(r2)));
         addInstr(env, PPCInstr_CMov(cc, rdst, PPCRI_Reg(r2)));
         return rdst;
      }

      if (e->Iex.Binop.op == Iop_32HLto64) {
         HReg   r_Hi  = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg   r_Lo  = iselWordExpr_R(env, e->Iex.Binop.arg2);
         HReg   r_dst = newVRegI(env);
         HReg   msk   = newVRegI(env);
         vassert(mode64);
         /* r_dst = OR( r_Hi<<32, r_Lo ) */
         addInstr(env, PPCInstr_Shft(Pshft_SHL, False/*64bit shift*/,
                                     r_dst, r_Hi, PPCRH_Imm(False,32)));
         addInstr(env, PPCInstr_LI(msk, 0xFFFFFFFF, mode64));
         addInstr(env, PPCInstr_Alu( Palu_AND, r_Lo, r_Lo,
                                     PPCRH_Reg(msk) ));
         addInstr(env, PPCInstr_Alu( Palu_OR, r_dst, r_dst,
                                     PPCRH_Reg(r_Lo) ));
         return r_dst;
      }

      if (e->Iex.Binop.op == Iop_CmpF64) {
         HReg fr_srcL    = iselDblExpr(env, e->Iex.Binop.arg1);
         HReg fr_srcR    = iselDblExpr(env, e->Iex.Binop.arg2);

         HReg r_ccPPC   = newVRegI(env);
         HReg r_ccIR    = newVRegI(env);
         HReg r_ccIR_b0 = newVRegI(env);
         HReg r_ccIR_b2 = newVRegI(env);
         HReg r_ccIR_b6 = newVRegI(env);

         addInstr(env, PPCInstr_FpCmp(r_ccPPC, fr_srcL, fr_srcR));

         /* Map compare result from PPC to IR,
            conforming to CmpF64 definition. */
         /*
           FP cmp result | PPC | IR
           --------------------------
           UN            | 0x1 | 0x45
           EQ            | 0x2 | 0x40
           GT            | 0x4 | 0x00
           LT            | 0x8 | 0x01
         */

         // r_ccIR_b0 = r_ccPPC[0] | r_ccPPC[3]
         addInstr(env, PPCInstr_Shft(Pshft_SHR, True/*32bit shift*/,
                                     r_ccIR_b0, r_ccPPC,
                                     PPCRH_Imm(False,0x3)));
         addInstr(env, PPCInstr_Alu(Palu_OR,  r_ccIR_b0,
                                    r_ccPPC,   PPCRH_Reg(r_ccIR_b0)));
         addInstr(env, PPCInstr_Alu(Palu_AND, r_ccIR_b0,
                                    r_ccIR_b0, PPCRH_Imm(False,0x1)));
         
         // r_ccIR_b2 = r_ccPPC[0]
         addInstr(env, PPCInstr_Shft(Pshft_SHL, True/*32bit shift*/,
                                     r_ccIR_b2, r_ccPPC,
                                     PPCRH_Imm(False,0x2)));
         addInstr(env, PPCInstr_Alu(Palu_AND, r_ccIR_b2,
                                    r_ccIR_b2, PPCRH_Imm(False,0x4)));

         // r_ccIR_b6 = r_ccPPC[0] | r_ccPPC[1]
         addInstr(env, PPCInstr_Shft(Pshft_SHR, True/*32bit shift*/,
                                     r_ccIR_b6, r_ccPPC,
                                     PPCRH_Imm(False,0x1)));
         addInstr(env, PPCInstr_Alu(Palu_OR,  r_ccIR_b6,
                                    r_ccPPC, PPCRH_Reg(r_ccIR_b6)));
         addInstr(env, PPCInstr_Shft(Pshft_SHL, True/*32bit shift*/,
                                     r_ccIR_b6, r_ccIR_b6,
                                     PPCRH_Imm(False,0x6)));
         addInstr(env, PPCInstr_Alu(Palu_AND, r_ccIR_b6,
                                    r_ccIR_b6, PPCRH_Imm(False,0x40)));

         // r_ccIR = r_ccIR_b0 | r_ccIR_b2 | r_ccIR_b6
         addInstr(env, PPCInstr_Alu(Palu_OR, r_ccIR,
                                    r_ccIR_b0, PPCRH_Reg(r_ccIR_b2)));
         addInstr(env, PPCInstr_Alu(Palu_OR, r_ccIR,
                                    r_ccIR,    PPCRH_Reg(r_ccIR_b6)));
         return r_ccIR;
      }

      if (e->Iex.Binop.op == Iop_F64toI32S) {
         /* This works in both mode64 and mode32. */
         HReg      r1      = StackFramePtr(env->mode64);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
         HReg      fsrc    = iselDblExpr(env, e->Iex.Binop.arg2);
         HReg      ftmp    = newVRegF(env);
         HReg      idst    = newVRegI(env);

         /* Set host rounding mode */
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

         sub_from_sp( env, 16 );
         addInstr(env, PPCInstr_FpCftI(False/*F->I*/, True/*int32*/, 
                                       ftmp, fsrc));
         addInstr(env, PPCInstr_FpSTFIW(r1, ftmp));
         addInstr(env, PPCInstr_Load(4, idst, zero_r1, mode64));

         /* in 64-bit mode we need to sign-widen idst. */
         if (mode64)
            addInstr(env, PPCInstr_Unary(Pun_EXTSW, idst, idst));

         add_to_sp( env, 16 );

         ///* Restore default FPU rounding. */
         //set_FPU_rounding_default( env );
         return idst;
      }

      if (e->Iex.Binop.op == Iop_F64toI64S) {
         if (mode64) {
            HReg      r1      = StackFramePtr(env->mode64);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
            HReg      fsrc    = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg      idst    = newVRegI(env);         
            HReg      ftmp    = newVRegF(env);

            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

            sub_from_sp( env, 16 );
            addInstr(env, PPCInstr_FpCftI(False/*F->I*/, False/*int64*/,
                                          ftmp, fsrc));
            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, ftmp, zero_r1));
            addInstr(env, PPCInstr_Load(8, idst, zero_r1, True/*mode64*/));
            add_to_sp( env, 16 );

            ///* Restore default FPU rounding. */
            //set_FPU_rounding_default( env );
            return idst;
         }
      }

      break;
   }

   /* --------- UNARY OP --------- */
   case Iex_Unop: {
      IROp op_unop = e->Iex.Unop.op;

      /* 1Uto8(32to1(expr32)) */
      DEFINE_PATTERN(p_32to1_then_1Uto8,
                     unop(Iop_1Uto8,unop(Iop_32to1,bind(0))));
      if (matchIRExpr(&mi,p_32to1_then_1Uto8,e)) {
         IRExpr* expr32 = mi.bindee[0];
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, expr32);
         addInstr(env, PPCInstr_Alu(Palu_AND, r_dst,
                                    r_src, PPCRH_Imm(False,1)));
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
            PPCAMode* amode
               = iselWordExpr_AMode( env, mi.bindee[0], Ity_I16/*xfer*/ );
            addInstr(env, PPCInstr_Load(2,r_dst,amode, mode64));
            return r_dst;
         }
      }

      switch (op_unop) {
      case Iop_8Uto16:
      case Iop_8Uto32:
      case Iop_8Uto64:
      case Iop_16Uto32:
      case Iop_16Uto64: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         UShort mask  = toUShort(op_unop==Iop_16Uto64 ? 0xFFFF :
                                 op_unop==Iop_16Uto32 ? 0xFFFF : 0xFF);
         addInstr(env, PPCInstr_Alu(Palu_AND,r_dst,r_src,
                                    PPCRH_Imm(False,mask)));
         return r_dst;
      }
      case Iop_32Uto64: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         vassert(mode64);
         addInstr(env,
                  PPCInstr_Shft(Pshft_SHL, False/*64bit shift*/,
                                r_dst, r_src, PPCRH_Imm(False,32)));
         addInstr(env,
                  PPCInstr_Shft(Pshft_SHR, False/*64bit shift*/,
                                r_dst, r_dst, PPCRH_Imm(False,32)));
         return r_dst;
      }
      case Iop_8Sto16:
      case Iop_8Sto32:
      case Iop_16Sto32: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         UShort amt   = toUShort(op_unop==Iop_16Sto32 ? 16 : 24);
         addInstr(env,
                  PPCInstr_Shft(Pshft_SHL, True/*32bit shift*/,
                                r_dst, r_src, PPCRH_Imm(False,amt)));
         addInstr(env,
                  PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/,
                                r_dst, r_dst, PPCRH_Imm(False,amt)));
         return r_dst;
      }
      case Iop_8Sto64:
      case Iop_16Sto64: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         UShort amt   = toUShort(op_unop==Iop_8Sto64  ? 56 :
                                 op_unop==Iop_16Sto64 ? 48 : 32);
         vassert(mode64);
         addInstr(env,
                  PPCInstr_Shft(Pshft_SHL, False/*64bit shift*/,
                                r_dst, r_src, PPCRH_Imm(False,amt)));
         addInstr(env,
                  PPCInstr_Shft(Pshft_SAR, False/*64bit shift*/,
                                r_dst, r_dst, PPCRH_Imm(False,amt)));
         return r_dst;
      }
      case Iop_32Sto64: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
	 vassert(mode64);
         /* According to the IBM docs, in 64 bit mode, srawi r,r,0
            sign extends the lower 32 bits into the upper 32 bits. */
         addInstr(env,
                  PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/,
                                r_dst, r_src, PPCRH_Imm(False,0)));
         return r_dst;
      }
      case Iop_Not8:
      case Iop_Not16:
      case Iop_Not32:
      case Iop_Not64: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_Unary(Pun_NOT,r_dst,r_src));
         return r_dst;
      }
      case Iop_64HIto32: {
         if (!mode64) {
            HReg rHi, rLo;
            iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rHi; /* and abandon rLo .. poor wee thing :-) */
         } else {
            HReg   r_dst = newVRegI(env);
            HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env,
                     PPCInstr_Shft(Pshft_SHR, False/*64bit shift*/,
                                   r_dst, r_src, PPCRH_Imm(False,32)));
            return r_dst;
         }
      }
      case Iop_64to32: {
         if (!mode64) {
            HReg rHi, rLo;
            iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rLo; /* similar stupid comment to the above ... */
         } else {
            /* This is a no-op. */
            return iselWordExpr_R(env, e->Iex.Unop.arg);
         }
      }
      case Iop_64to16: {
         if (mode64) { /* This is a no-op. */
            return iselWordExpr_R(env, e->Iex.Unop.arg);
         }
         break; /* evidently not used in 32-bit mode */
      }
      case Iop_16HIto8:
      case Iop_32HIto16: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         UShort shift = toUShort(op_unop == Iop_16HIto8 ? 8 : 16);
         addInstr(env,
                  PPCInstr_Shft(Pshft_SHR, True/*32bit shift*/,
                                r_dst, r_src, PPCRH_Imm(False,shift)));
         return r_dst;
      }
      case Iop_128HIto64: 
         if (mode64) {
            HReg rHi, rLo;
            iselInt128Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rHi; /* and abandon rLo .. poor wee thing :-) */
         }
         break;
      case Iop_128to64:
         if (mode64) {
            HReg rHi, rLo;
            iselInt128Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rLo; /* similar stupid comment to the above ... */
         }
         break;
      case Iop_1Uto32:
      case Iop_1Uto8: {
         HReg        r_dst = newVRegI(env);
         PPCCondCode cond  = iselCondCode(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_Set(cond,r_dst));
         return r_dst;
      }
      case Iop_1Sto8:
      case Iop_1Sto16:
      case Iop_1Sto32: {
         /* could do better than this, but for now ... */
         HReg        r_dst = newVRegI(env);
         PPCCondCode cond  = iselCondCode(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_Set(cond,r_dst));
         addInstr(env,
                  PPCInstr_Shft(Pshft_SHL, True/*32bit shift*/,
                                r_dst, r_dst, PPCRH_Imm(False,31)));
         addInstr(env,
                  PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/,
                                r_dst, r_dst, PPCRH_Imm(False,31)));
         return r_dst;
      }
      case Iop_1Sto64: 
         if (mode64) {
            /* could do better than this, but for now ... */
            HReg        r_dst = newVRegI(env);
            PPCCondCode cond  = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, PPCInstr_Set(cond,r_dst));
            addInstr(env, PPCInstr_Shft(Pshft_SHL, False/*64bit shift*/,
                                        r_dst, r_dst, PPCRH_Imm(False,63)));
            addInstr(env, PPCInstr_Shft(Pshft_SAR, False/*64bit shift*/,
                                        r_dst, r_dst, PPCRH_Imm(False,63)));
            return r_dst;
         }
         break;
      case Iop_Clz32:
      case Iop_Clz64: {
         HReg r_src, r_dst;
         PPCUnaryOp op_clz = (op_unop == Iop_Clz32) ? Pun_CLZ32 :
                                                      Pun_CLZ64;
         if (op_unop == Iop_Clz64 && !mode64)
            goto irreducible;
         /* Count leading zeroes. */
         r_dst = newVRegI(env);
         r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_Unary(op_clz,r_dst,r_src));
         return r_dst;
      }

      case Iop_Left8:
      case Iop_Left32: 
      case Iop_Left64: {
         HReg r_src, r_dst;
         if (op_unop == Iop_Left64 && !mode64)
            goto irreducible;
         r_dst = newVRegI(env);
         r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_Unary(Pun_NEG,r_dst,r_src));
         addInstr(env, PPCInstr_Alu(Palu_OR, r_dst, r_dst, PPCRH_Reg(r_src)));
         return r_dst;
      }

      case Iop_CmpwNEZ32: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_Unary(Pun_NEG,r_dst,r_src));
         addInstr(env, PPCInstr_Alu(Palu_OR, r_dst, r_dst, PPCRH_Reg(r_src)));
         addInstr(env, PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/, 
                                     r_dst, r_dst, PPCRH_Imm(False, 31)));
         return r_dst;
      }

      case Iop_CmpwNEZ64: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         if (!mode64) goto irreducible;
         addInstr(env, PPCInstr_Unary(Pun_NEG,r_dst,r_src));
         addInstr(env, PPCInstr_Alu(Palu_OR, r_dst, r_dst, PPCRH_Reg(r_src)));
         addInstr(env, PPCInstr_Shft(Pshft_SAR, False/*64bit shift*/, 
                                     r_dst, r_dst, PPCRH_Imm(False, 63)));
         return r_dst;
      }

      case Iop_V128to32: {
         HReg        r_aligned16;
         HReg        dst  = newVRegI(env);
         HReg        vec  = iselVecExpr(env, e->Iex.Unop.arg);
         PPCAMode *am_off0, *am_off12;
         sub_from_sp( env, 32 );     // Move SP down 32 bytes

         // get a quadword aligned address within our stack space
         r_aligned16 = get_sp_aligned16( env );
         am_off0  = PPCAMode_IR( 0, r_aligned16 );
         am_off12 = PPCAMode_IR( 12,r_aligned16 );

         // store vec, load low word to dst
         addInstr(env,
                  PPCInstr_AvLdSt( False/*store*/, 16, vec, am_off0 ));
         addInstr(env,
                  PPCInstr_Load( 4, dst, am_off12, mode64 ));

         add_to_sp( env, 32 );       // Reset SP
         return dst;
      }

      case Iop_V128to64:
      case Iop_V128HIto64: 
         if (mode64) {
            HReg     r_aligned16;
            HReg     dst = newVRegI(env);
            HReg     vec = iselVecExpr(env, e->Iex.Unop.arg);
            PPCAMode *am_off0, *am_off8;
            sub_from_sp( env, 32 );     // Move SP down 32 bytes

            // get a quadword aligned address within our stack space
            r_aligned16 = get_sp_aligned16( env );
            am_off0 = PPCAMode_IR( 0, r_aligned16 );
            am_off8 = PPCAMode_IR( 8 ,r_aligned16 );

            // store vec, load low word (+8) or high (+0) to dst
            addInstr(env,
                     PPCInstr_AvLdSt( False/*store*/, 16, vec, am_off0 ));
            addInstr(env,
                     PPCInstr_Load( 
                        8, dst, 
                        op_unop == Iop_V128HIto64 ? am_off0 : am_off8, 
                        mode64 ));

            add_to_sp( env, 32 );       // Reset SP
            return dst;
         }
         break;
      case Iop_16to8:
      case Iop_32to8:
      case Iop_32to16:
      case Iop_64to8:
         /* These are no-ops. */
         return iselWordExpr_R(env, e->Iex.Unop.arg);
         
      /* ReinterpF64asI64(e) */
      /* Given an IEEE754 double, produce an I64 with the same bit
         pattern. */
      case Iop_ReinterpF64asI64: 
         if (mode64) {
            PPCAMode *am_addr;
            HReg fr_src = iselDblExpr(env, e->Iex.Unop.arg);
            HReg r_dst  = newVRegI(env);

            sub_from_sp( env, 16 );     // Move SP down 16 bytes
            am_addr = PPCAMode_IR( 0, StackFramePtr(mode64) );

            // store as F64
            addInstr(env, PPCInstr_FpLdSt( False/*store*/, 8,
                                           fr_src, am_addr ));
            // load as Ity_I64
            addInstr(env, PPCInstr_Load( 8, r_dst, am_addr, mode64 ));

            add_to_sp( env, 16 );       // Reset SP
            return r_dst;
         }
         break;

      /* ReinterpF32asI32(e) */
      /* Given an IEEE754 float, produce an I32 with the same bit
         pattern. */
      case Iop_ReinterpF32asI32: {
         /* I believe this generates correct code for both 32- and
            64-bit hosts. */
         PPCAMode *am_addr;
         HReg fr_src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg r_dst  = newVRegI(env);

         sub_from_sp( env, 16 );     // Move SP down 16 bytes
         am_addr = PPCAMode_IR( 0, StackFramePtr(mode64) );

         // store as F32
         addInstr(env, PPCInstr_FpLdSt( False/*store*/, 4,
                                        fr_src, am_addr ));
         // load as Ity_I32
         addInstr(env, PPCInstr_Load( 4, r_dst, am_addr, mode64 ));

         add_to_sp( env, 16 );       // Reset SP
         return r_dst;
      }

      default: 
         break;
      }
      break;
   }

   /* --------- GET --------- */
   case Iex_Get: {
      if (ty == Ity_I8  || ty == Ity_I16 ||
          ty == Ity_I32 || ((ty == Ity_I64) && mode64)) {
         HReg r_dst = newVRegI(env);
         PPCAMode* am_addr = PPCAMode_IR( e->Iex.Get.offset,
                                          GuestStatePtr(mode64) );
         addInstr(env, PPCInstr_Load( toUChar(sizeofIRType(ty)), 
                                      r_dst, am_addr, mode64 ));
         return r_dst;
      }
      break;
   }

   case Iex_GetI: {
      PPCAMode* src_am
         = genGuestArrayOffset( env, e->Iex.GetI.descr,
                                     e->Iex.GetI.ix, e->Iex.GetI.bias );
      HReg r_dst = newVRegI(env);
      if (mode64 && ty == Ity_I64) {
         addInstr(env, PPCInstr_Load( toUChar(8),
                                      r_dst, src_am, mode64 ));
         return r_dst;
      }
      if ((!mode64) && ty == Ity_I32) {
         addInstr(env, PPCInstr_Load( toUChar(4),
                                      r_dst, src_am, mode64 ));
         return r_dst;
      }
      break;
   }

   /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg    r_dst = newVRegI(env);
      vassert(ty == Ity_I32);

      /* be very restrictive for now.  Only 32/64-bit ints allowed
         for args, and 32 bits for return type. */
      if (e->Iex.CCall.retty != Ity_I32)
         goto irreducible;
      
      /* Marshal args, do the call, clear stack. */
      doHelperCall( env, False, NULL,
                    e->Iex.CCall.cee, e->Iex.CCall.args );

      /* GPR3 now holds the destination address from Pin_Goto */
      addInstr(env, mk_iMOVds_RR(r_dst, hregPPC_GPR3(mode64)));
      return r_dst;
   }
      
   /* --------- LITERAL --------- */
   /* 32/16/8-bit literals */
   case Iex_Const: {
      Long l;
      HReg r_dst = newVRegI(env);
      IRConst* con = e->Iex.Const.con;
      switch (con->tag) {
         case Ico_U64: if (!mode64) goto irreducible;
                       l = (Long)            con->Ico.U64; break;
         case Ico_U32: l = (Long)(Int)       con->Ico.U32; break;
         case Ico_U16: l = (Long)(Int)(Short)con->Ico.U16; break;
         case Ico_U8:  l = (Long)(Int)(Char )con->Ico.U8;  break;
         default:      vpanic("iselIntExpr_R.const(ppc)");
      }
      addInstr(env, PPCInstr_LI(r_dst, (ULong)l, mode64));
      return r_dst;
   }

   /* --------- MULTIPLEX --------- */
   case Iex_Mux0X: {
      if ((ty == Ity_I8  || ty == Ity_I16 ||
           ty == Ity_I32 || ((ty == Ity_I64) && mode64)) &&
          typeOfIRExpr(env->type_env,e->Iex.Mux0X.cond) == Ity_I8) {
         PPCCondCode  cc = mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
         HReg   r_cond = iselWordExpr_R(env, e->Iex.Mux0X.cond);
         HReg   rX     = iselWordExpr_R(env, e->Iex.Mux0X.exprX);
         PPCRI* r0     = iselWordExpr_RI(env, e->Iex.Mux0X.expr0);
         HReg   r_dst  = newVRegI(env);
         HReg   r_tmp  = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(r_dst,rX));
         addInstr(env, PPCInstr_Alu(Palu_AND, r_tmp,
                                    r_cond, PPCRH_Imm(False,0xFF)));
         addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                    7/*cr*/, r_tmp, PPCRH_Imm(False,0)));
         addInstr(env, PPCInstr_CMov(cc,r_dst,r0));
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
   vpanic("iselIntExpr_R(ppc): cannot reduce tree");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expression auxiliaries              ---*/
/*---------------------------------------------------------*/

/* --------------------- AMODEs --------------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a word-size one.
*/

static Bool uInt_fits_in_16_bits ( UInt u ) 
{
   /* Is u the same as the sign-extend of its lower 16 bits? */
   Int i = u & 0xFFFF;
   i <<= 16;
   i >>= 16;
   return toBool(u == (UInt)i);
}

static Bool uLong_fits_in_16_bits ( ULong u ) 
{
   /* Is u the same as the sign-extend of its lower 16 bits? */
   Long i = u & 0xFFFFULL;
   i <<= 48;
   i >>= 48;
   return toBool(u == (ULong)i);
}

static Bool uLong_is_4_aligned ( ULong u )
{
   return toBool((u & 3ULL) == 0);
}

static Bool sane_AMode ( ISelEnv* env, PPCAMode* am )
{
   Bool mode64 = env->mode64;
   switch (am->tag) {
   case Pam_IR:
      /* Using uInt_fits_in_16_bits in 64-bit mode seems a bit bogus,
         somehow, but I think it's OK. */
      return toBool( hregClass(am->Pam.IR.base) == HRcGPR(mode64) && 
                     hregIsVirtual(am->Pam.IR.base) && 
                     uInt_fits_in_16_bits(am->Pam.IR.index) );
   case Pam_RR:
      return toBool( hregClass(am->Pam.RR.base) == HRcGPR(mode64) && 
                     hregIsVirtual(am->Pam.RR.base) &&
                     hregClass(am->Pam.RR.index) == HRcGPR(mode64) &&
                     hregIsVirtual(am->Pam.IR.index) );
   default:
      vpanic("sane_AMode: unknown ppc amode tag");
   }
}

static 
PPCAMode* iselWordExpr_AMode ( ISelEnv* env, IRExpr* e, IRType xferTy )
{
   PPCAMode* am = iselWordExpr_AMode_wrk(env, e, xferTy);
   vassert(sane_AMode(env, am));
   return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPCAMode* iselWordExpr_AMode_wrk ( ISelEnv* env, IRExpr* e, IRType xferTy )
{
   IRType ty = typeOfIRExpr(env->type_env,e);

   if (env->mode64) {

      /* If the data load/store type is I32 or I64, this amode might
         be destined for use in ld/ldu/lwa/st/stu.  In which case
         insist that if it comes out as an _IR, the immediate must
         have its bottom two bits be zero.  This does assume that for
         any other type (I8/I16/I128/F32/F64/V128) the amode will not
         be parked in any such instruction.  But that seems a
         reasonable assumption.  */
      Bool aligned4imm = toBool(xferTy == Ity_I32 || xferTy == Ity_I64);

      vassert(ty == Ity_I64);
   
      /* Add64(expr,i), where i == sign-extend of (i & 0xFFFF) */
      if (e->tag == Iex_Binop 
          && e->Iex.Binop.op == Iop_Add64
          && e->Iex.Binop.arg2->tag == Iex_Const
          && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U64
          && (aligned4imm  ? uLong_is_4_aligned(e->Iex.Binop.arg2
                                                 ->Iex.Const.con->Ico.U64)
                           : True)
          && uLong_fits_in_16_bits(e->Iex.Binop.arg2
                                    ->Iex.Const.con->Ico.U64)) {
         return PPCAMode_IR( (Int)e->Iex.Binop.arg2->Iex.Const.con->Ico.U64,
                             iselWordExpr_R(env, e->Iex.Binop.arg1) );
      }
      
      /* Add64(expr,expr) */
      if (e->tag == Iex_Binop 
          && e->Iex.Binop.op == Iop_Add64) {
         HReg r_base = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_idx  = iselWordExpr_R(env, e->Iex.Binop.arg2);
         return PPCAMode_RR( r_idx, r_base );
      }

   } else {

      vassert(ty == Ity_I32);
   
      /* Add32(expr,i), where i == sign-extend of (i & 0xFFFF) */
      if (e->tag == Iex_Binop 
          && e->Iex.Binop.op == Iop_Add32
          && e->Iex.Binop.arg2->tag == Iex_Const
          && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32
          && uInt_fits_in_16_bits(e->Iex.Binop.arg2
                                   ->Iex.Const.con->Ico.U32)) {
         return PPCAMode_IR( (Int)e->Iex.Binop.arg2->Iex.Const.con->Ico.U32,
                             iselWordExpr_R(env, e->Iex.Binop.arg1) );
      }
      
      /* Add32(expr,expr) */
      if (e->tag == Iex_Binop 
          && e->Iex.Binop.op == Iop_Add32) {
         HReg r_base = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_idx  = iselWordExpr_R(env, e->Iex.Binop.arg2);
         return PPCAMode_RR( r_idx, r_base );
      }

   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   return PPCAMode_IR( 0, iselWordExpr_R(env,e) );
}


/* --------------------- RH --------------------- */

/* Compute an I8/I16/I32 (and I64, in 64-bit mode) into a RH
   (reg-or-halfword-immediate).  It's important to specify whether the
   immediate is to be regarded as signed or not.  If yes, this will
   never return -32768 as an immediate; this guaranteed that all
   signed immediates that are return can have their sign inverted if
   need be. */

static PPCRH* iselWordExpr_RH ( ISelEnv* env, Bool syned, IRExpr* e )
{
   PPCRH* ri = iselWordExpr_RH_wrk(env, syned, e);
   /* sanity checks ... */
   switch (ri->tag) {
   case Prh_Imm:
      vassert(ri->Prh.Imm.syned == syned);
      if (syned)
         vassert(ri->Prh.Imm.imm16 != 0x8000);
      return ri;
   case Prh_Reg:
      vassert(hregClass(ri->Prh.Reg.reg) == HRcGPR(env->mode64));
      vassert(hregIsVirtual(ri->Prh.Reg.reg));
      return ri;
   default:
      vpanic("iselIntExpr_RH: unknown ppc RH tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPCRH* iselWordExpr_RH_wrk ( ISelEnv* env, Bool syned, IRExpr* e )
{
   ULong u;
   Long  l;
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I8  || ty == Ity_I16 ||
           ty == Ity_I32 || ((ty == Ity_I64) && env->mode64));

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      IRConst* con = e->Iex.Const.con;
      /* What value are we aiming to generate? */
      switch (con->tag) {
      /* Note: Not sign-extending - we carry 'syned' around */
      case Ico_U64: vassert(env->mode64);
                    u =              con->Ico.U64; break;
      case Ico_U32: u = 0xFFFFFFFF & con->Ico.U32; break;
      case Ico_U16: u = 0x0000FFFF & con->Ico.U16; break;
      case Ico_U8:  u = 0x000000FF & con->Ico.U8; break;
      default:      vpanic("iselIntExpr_RH.Iex_Const(ppch)");
      }
      l = (Long)u;
      /* Now figure out if it's representable. */
      if (!syned && u <= 65535) {
         return PPCRH_Imm(False/*unsigned*/, toUShort(u & 0xFFFF));
      }
      if (syned && l >= -32767 && l <= 32767) {
         return PPCRH_Imm(True/*signed*/, toUShort(u & 0xFFFF));
      }
      /* no luck; use the Slow Way. */
   }

   /* default case: calculate into a register and return that */
   return PPCRH_Reg( iselWordExpr_R ( env, e ) );
}


/* --------------------- RIs --------------------- */

/* Calculate an expression into an PPCRI operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits, or,
   in 64-bit mode, 64 bits. */

static PPCRI* iselWordExpr_RI ( ISelEnv* env, IRExpr* e )
{
   PPCRI* ri = iselWordExpr_RI_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
   case Pri_Imm:
      return ri;
   case Pri_Reg:
      vassert(hregClass(ri->Pri.Reg) == HRcGPR(env->mode64));
      vassert(hregIsVirtual(ri->Pri.Reg));
      return ri;
   default:
      vpanic("iselIntExpr_RI: unknown ppc RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPCRI* iselWordExpr_RI_wrk ( ISelEnv* env, IRExpr* e )
{
   Long  l;
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I8  || ty == Ity_I16 ||
           ty == Ity_I32 || ((ty == Ity_I64) && env->mode64));

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      IRConst* con = e->Iex.Const.con;
      switch (con->tag) {
      case Ico_U64: vassert(env->mode64);
                    l = (Long)            con->Ico.U64; break;
      case Ico_U32: l = (Long)(Int)       con->Ico.U32; break;
      case Ico_U16: l = (Long)(Int)(Short)con->Ico.U16; break;
      case Ico_U8:  l = (Long)(Int)(Char )con->Ico.U8;  break;
      default:      vpanic("iselIntExpr_RI.Iex_Const(ppch)");
      }
      return PPCRI_Imm((ULong)l);
   }

   /* default case: calculate into a register and return that */
   return PPCRI_Reg( iselWordExpr_R ( env, e ) );
}


/* --------------------- RH5u --------------------- */

/* Compute an I8 into a reg-or-5-bit-unsigned-immediate, the latter
   being an immediate in the range 1 .. 31 inclusive.  Used for doing
   shift amounts.  Only used in 32-bit mode. */

static PPCRH* iselWordExpr_RH5u ( ISelEnv* env, IRExpr* e )
{
   PPCRH* ri;
   vassert(!env->mode64);
   ri = iselWordExpr_RH5u_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
   case Prh_Imm:
      vassert(ri->Prh.Imm.imm16 >= 1 && ri->Prh.Imm.imm16 <= 31);
      vassert(!ri->Prh.Imm.syned);
      return ri;
   case Prh_Reg:
      vassert(hregClass(ri->Prh.Reg.reg) == HRcGPR(env->mode64));
      vassert(hregIsVirtual(ri->Prh.Reg.reg));
      return ri;
   default:
      vpanic("iselIntExpr_RH5u: unknown ppc RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPCRH* iselWordExpr_RH5u_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const
       && e->Iex.Const.con->tag == Ico_U8
       && e->Iex.Const.con->Ico.U8 >= 1
       && e->Iex.Const.con->Ico.U8 <= 31) {
      return PPCRH_Imm(False/*unsigned*/, e->Iex.Const.con->Ico.U8);
   }

   /* default case: calculate into a register and return that */
   return PPCRH_Reg( iselWordExpr_R ( env, e ) );
}


/* --------------------- RH6u --------------------- */

/* Compute an I8 into a reg-or-6-bit-unsigned-immediate, the latter
   being an immediate in the range 1 .. 63 inclusive.  Used for doing
   shift amounts.  Only used in 64-bit mode. */

static PPCRH* iselWordExpr_RH6u ( ISelEnv* env, IRExpr* e )
{
   PPCRH* ri; 
   vassert(env->mode64);
   ri = iselWordExpr_RH6u_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
   case Prh_Imm:
      vassert(ri->Prh.Imm.imm16 >= 1 && ri->Prh.Imm.imm16 <= 63);
      vassert(!ri->Prh.Imm.syned);
      return ri;
   case Prh_Reg:
      vassert(hregClass(ri->Prh.Reg.reg) == HRcGPR(env->mode64));
      vassert(hregIsVirtual(ri->Prh.Reg.reg));
      return ri;
   default:
      vpanic("iselIntExpr_RH6u: unknown ppc64 RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPCRH* iselWordExpr_RH6u_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const
       && e->Iex.Const.con->tag == Ico_U8
       && e->Iex.Const.con->Ico.U8 >= 1
       && e->Iex.Const.con->Ico.U8 <= 63) {
      return PPCRH_Imm(False/*unsigned*/, e->Iex.Const.con->Ico.U8);
   }

   /* default case: calculate into a register and return that */
   return PPCRH_Reg( iselWordExpr_R ( env, e ) );
}


/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static PPCCondCode iselCondCode ( ISelEnv* env, IRExpr* e )
{
   /* Uh, there's nothing we can sanity check here, unfortunately. */
   return iselCondCode_wrk(env,e);
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPCCondCode iselCondCode_wrk ( ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I1);

   /* Constant 1:Bit */
   if (e->tag == Iex_Const && e->Iex.Const.con->Ico.U1 == True) {
      // Make a compare that will always be true:
      HReg r_zero = newVRegI(env);
      addInstr(env, PPCInstr_LI(r_zero, 0, env->mode64));
      addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                 7/*cr*/, r_zero, PPCRH_Reg(r_zero)));
      return mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
   }

   /* Not1(...) */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_Not1) {
      /* Generate code for the arg, and negate the test condition */
      PPCCondCode cond = iselCondCode(env, e->Iex.Unop.arg);
      cond.test = invertCondTest(cond.test);
      return cond;
   }

   /* --- patterns rooted at: 32to1 or 64to1 --- */

   /* 32to1, 64to1 */
   if (e->tag == Iex_Unop &&
       (e->Iex.Unop.op == Iop_32to1 || e->Iex.Unop.op == Iop_64to1)) {
      HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
      HReg tmp = newVRegI(env);
      /* could do better, probably -- andi. */
      addInstr(env, PPCInstr_Alu(Palu_AND, tmp,
                                 src, PPCRH_Imm(False,1)));
      addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                 7/*cr*/, tmp, PPCRH_Imm(False,1)));
      return mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
   }

   /* --- patterns rooted at: CmpNEZ8 --- */

   /* CmpNEZ8(x) */
   /* could do better -- andi. */
   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ8) {
      HReg arg = iselWordExpr_R(env, e->Iex.Unop.arg);
      HReg tmp = newVRegI(env);
      addInstr(env, PPCInstr_Alu(Palu_AND, tmp, arg,
                                 PPCRH_Imm(False,0xFF)));
      addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                 7/*cr*/, tmp, PPCRH_Imm(False,0)));
      return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
   }

   /* --- patterns rooted at: CmpNEZ32 --- */

   /* CmpNEZ32(x) */
   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ32) {
      HReg r1 = iselWordExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                 7/*cr*/, r1, PPCRH_Imm(False,0)));
      return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
   }

   /* --- patterns rooted at: Cmp*32* --- */

   /* Cmp*32*(x,y) */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ32
           || e->Iex.Binop.op == Iop_CmpNE32
           || e->Iex.Binop.op == Iop_CmpLT32S
           || e->Iex.Binop.op == Iop_CmpLT32U
           || e->Iex.Binop.op == Iop_CmpLE32S
           || e->Iex.Binop.op == Iop_CmpLE32U)) {
      Bool syned = (e->Iex.Binop.op == Iop_CmpLT32S ||
                    e->Iex.Binop.op == Iop_CmpLE32S);
      HReg   r1  = iselWordExpr_R(env, e->Iex.Binop.arg1);
      PPCRH* ri2 = iselWordExpr_RH(env, syned, e->Iex.Binop.arg2);
      addInstr(env, PPCInstr_Cmp(syned, True/*32bit cmp*/,
                                 7/*cr*/, r1, ri2));

      switch (e->Iex.Binop.op) {
      case Iop_CmpEQ32:  return mk_PPCCondCode( Pct_TRUE,  Pcf_7EQ );
      case Iop_CmpNE32:  return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
      case Iop_CmpLT32U: return mk_PPCCondCode( Pct_TRUE,  Pcf_7LT );
      case Iop_CmpLE32U: return mk_PPCCondCode( Pct_FALSE, Pcf_7GT );
      default: vpanic("iselCondCode(ppc): CmpXX32");
      }
   }

   /* --- patterns rooted at: CmpNEZ64 --- */

   /* CmpNEZ64 */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ64) {
      if (!env->mode64) {
         HReg hi, lo;
         HReg tmp = newVRegI(env);
         iselInt64Expr( &hi, &lo, env, e->Iex.Unop.arg );
         addInstr(env, PPCInstr_Alu(Palu_OR, tmp, lo, PPCRH_Reg(hi)));
         addInstr(env, PPCInstr_Cmp(False/*sign*/, True/*32bit cmp*/,
                                    7/*cr*/, tmp,PPCRH_Imm(False,0)));
         return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
      } else {  // mode64
         HReg r_src = iselWordExpr_R(env, e->Iex.Binop.arg1);
         addInstr(env, PPCInstr_Cmp(False/*sign*/, False/*64bit cmp*/,
                                    7/*cr*/, r_src,PPCRH_Imm(False,0)));
         return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
      }
   }

   /* --- patterns rooted at: Cmp*64* --- */

   /* Cmp*64*(x,y) */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ64
           || e->Iex.Binop.op == Iop_CmpNE64
           || e->Iex.Binop.op == Iop_CmpLT64S
           || e->Iex.Binop.op == Iop_CmpLT64U
           || e->Iex.Binop.op == Iop_CmpLE64S
           || e->Iex.Binop.op == Iop_CmpLE64U)) {
      Bool   syned = (e->Iex.Binop.op == Iop_CmpLT64S ||
                      e->Iex.Binop.op == Iop_CmpLE64S);
      HReg    r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
      PPCRH* ri2 = iselWordExpr_RH(env, syned, e->Iex.Binop.arg2);
      vassert(env->mode64);
      addInstr(env, PPCInstr_Cmp(syned, False/*64bit cmp*/,
                                 7/*cr*/, r1, ri2));

      switch (e->Iex.Binop.op) {
      case Iop_CmpEQ64:  return mk_PPCCondCode( Pct_TRUE,  Pcf_7EQ );
      case Iop_CmpNE64:  return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
      case Iop_CmpLT64U: return mk_PPCCondCode( Pct_TRUE,  Pcf_7LT );
      case Iop_CmpLE64U: return mk_PPCCondCode( Pct_FALSE, Pcf_7GT );
      default: vpanic("iselCondCode(ppc): CmpXX64");
      }
   }

   /* var */
   if (e->tag == Iex_RdTmp) {
      HReg r_src      = lookupIRTemp(env, e->Iex.RdTmp.tmp);
      HReg src_masked = newVRegI(env);
      addInstr(env,
               PPCInstr_Alu(Palu_AND, src_masked,
                            r_src, PPCRH_Imm(False,1)));
      addInstr(env,
               PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                            7/*cr*/, src_masked, PPCRH_Imm(False,1)));
      return mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
   }

   vex_printf("iselCondCode(ppc): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselCondCode(ppc)");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (128 bit)               ---*/
/*---------------------------------------------------------*/

/* 64-bit mode ONLY: compute a 128-bit value into a register pair,
   which is returned as the first two parameters.  As with
   iselWordExpr_R, these may be either real or virtual regs; in any
   case they must not be changed by subsequent code emitted by the
   caller.  */

static void iselInt128Expr ( HReg* rHi, HReg* rLo,
                             ISelEnv* env, IRExpr* e )
{
   vassert(env->mode64);
   iselInt128Expr_wrk(rHi, rLo, env, e);
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(*rHi) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt128Expr_wrk ( HReg* rHi, HReg* rLo,
                                 ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I128);

   /* read 128-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair( rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
      /* 64 x 64 -> 128 multiply */
      case Iop_MullU64:
      case Iop_MullS64: {
         HReg     tLo     = newVRegI(env);
         HReg     tHi     = newVRegI(env);
         Bool     syned   = toBool(e->Iex.Binop.op == Iop_MullS64);
         HReg     r_srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg     r_srcR  = iselWordExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_MulL(False/*signedness irrelevant*/, 
                                     False/*lo64*/, False/*64bit mul*/,
                                     tLo, r_srcL, r_srcR));
         addInstr(env, PPCInstr_MulL(syned,
                                     True/*hi64*/, False/*64bit mul*/,
                                     tHi, r_srcL, r_srcR));
         *rHi = tHi;
         *rLo = tLo;
         return;
      }

      /* 64HLto128(e1,e2) */
      case Iop_64HLto128:
         *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
         *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2);
         return;

      default: 
         break;
      }
   } /* if (e->tag == Iex_Binop) */


   /* --------- UNARY ops --------- */
   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
      default:
         break;
      }
   } /* if (e->tag == Iex_Unop) */

   vex_printf("iselInt128Expr(ppc64): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselInt128Expr(ppc64)");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64 bit)                ---*/
/*---------------------------------------------------------*/

/* 32-bit mode ONLY: compute a 64-bit value into a register pair,
   which is returned as the first two parameters.  As with
   iselIntExpr_R, these may be either real or virtual regs; in any
   case they must not be changed by subsequent code emitted by the
   caller.  */

static void iselInt64Expr ( HReg* rHi, HReg* rLo,
                            ISelEnv* env, IRExpr* e )
{
   vassert(!env->mode64);
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
static void iselInt64Expr_wrk ( HReg* rHi, HReg* rLo,
                                ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I64);

   /* 64-bit load */
   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_BE) {
      HReg tLo    = newVRegI(env);
      HReg tHi    = newVRegI(env);
      HReg r_addr = iselWordExpr_R(env, e->Iex.Load.addr);
      vassert(!env->mode64);
      addInstr(env, PPCInstr_Load( 4/*byte-load*/,
                                   tHi, PPCAMode_IR( 0, r_addr ), 
                                   False/*32-bit insn please*/) );
      addInstr(env, PPCInstr_Load( 4/*byte-load*/, 
                                   tLo, PPCAMode_IR( 4, r_addr ), 
                                   False/*32-bit insn please*/) );
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit literal */
   if (e->tag == Iex_Const) {
      ULong w64 = e->Iex.Const.con->Ico.U64;
      UInt  wHi = ((UInt)(w64 >> 32)) & 0xFFFFFFFF;
      UInt  wLo = ((UInt)w64) & 0xFFFFFFFF;
      HReg  tLo = newVRegI(env);
      HReg  tHi = newVRegI(env);
      vassert(e->Iex.Const.con->tag == Ico_U64);
      addInstr(env, PPCInstr_LI(tHi, (Long)(Int)wHi, False/*mode32*/));
      addInstr(env, PPCInstr_LI(tLo, (Long)(Int)wLo, False/*mode32*/));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* read 64-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair( rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }

   /* 64-bit GET */
   if (e->tag == Iex_Get) {
      PPCAMode* am_addr = PPCAMode_IR( e->Iex.Get.offset,
                                       GuestStatePtr(False/*mode32*/) );
      PPCAMode* am_addr4 = advance4(env, am_addr);
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      addInstr(env, PPCInstr_Load( 4, tHi, am_addr,  False/*mode32*/ ));
      addInstr(env, PPCInstr_Load( 4, tLo, am_addr4, False/*mode32*/ ));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit Mux0X */
   if (e->tag == Iex_Mux0X) {
      HReg e0Lo, e0Hi, eXLo, eXHi;
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      
      PPCCondCode cc = mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
      HReg r_cond = iselWordExpr_R(env, e->Iex.Mux0X.cond);
      HReg r_tmp  = newVRegI(env);
      
      iselInt64Expr(&e0Hi, &e0Lo, env, e->Iex.Mux0X.expr0);
      iselInt64Expr(&eXHi, &eXLo, env, e->Iex.Mux0X.exprX);
      addInstr(env, mk_iMOVds_RR(tHi,eXHi));
      addInstr(env, mk_iMOVds_RR(tLo,eXLo));
      
      addInstr(env, PPCInstr_Alu(Palu_AND, 
                                 r_tmp, r_cond, PPCRH_Imm(False,0xFF)));
      addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/, 
                                 7/*cr*/, r_tmp, PPCRH_Imm(False,0)));
      
      addInstr(env, PPCInstr_CMov(cc,tHi,PPCRI_Reg(e0Hi)));
      addInstr(env, PPCInstr_CMov(cc,tLo,PPCRI_Reg(e0Lo)));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      IROp op_binop = e->Iex.Binop.op;
      switch (op_binop) {
         /* 32 x 32 -> 64 multiply */
         case Iop_MullU32:
         case Iop_MullS32: {
            HReg     tLo     = newVRegI(env);
            HReg     tHi     = newVRegI(env);
            Bool     syned   = toBool(op_binop == Iop_MullS32);
            HReg     r_srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg     r_srcR  = iselWordExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, PPCInstr_MulL(False/*signedness irrelevant*/, 
                                        False/*lo32*/, True/*32bit mul*/,
                                        tLo, r_srcL, r_srcR));
            addInstr(env, PPCInstr_MulL(syned,
                                        True/*hi32*/, True/*32bit mul*/,
                                        tHi, r_srcL, r_srcR));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* Or64/And64/Xor64 */
         case Iop_Or64:
         case Iop_And64:
         case Iop_Xor64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            PPCAluOp op = (op_binop == Iop_Or64) ? Palu_OR :
                          (op_binop == Iop_And64) ? Palu_AND : Palu_XOR;
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, PPCInstr_Alu(op, tHi, xHi, PPCRH_Reg(yHi)));
            addInstr(env, PPCInstr_Alu(op, tLo, xLo, PPCRH_Reg(yLo)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* Add64 */
         case Iop_Add64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, PPCInstr_AddSubC( True/*add*/, True /*set carry*/,
                                            tLo, xLo, yLo));
            addInstr(env, PPCInstr_AddSubC( True/*add*/, False/*read carry*/,
                                            tHi, xHi, yHi));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32HLto64(e1,e2) */
         case Iop_32HLto64:
            *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
            *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2);
            return;

         /* F64toI64S */
         case Iop_F64toI64S: {
            HReg      tLo     = newVRegI(env);
            HReg      tHi     = newVRegI(env);
            HReg      r1      = StackFramePtr(env->mode64);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
            PPCAMode* four_r1 = PPCAMode_IR( 4, r1 );
            HReg      fsrc    = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg      ftmp    = newVRegF(env);

            vassert(!env->mode64);
            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

            sub_from_sp( env, 16 );
            addInstr(env, PPCInstr_FpCftI(False/*F->I*/, False/*int64*/,
                                          ftmp, fsrc));
            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, ftmp, zero_r1));
            addInstr(env, PPCInstr_Load(4, tHi, zero_r1, False/*mode32*/));
            addInstr(env, PPCInstr_Load(4, tLo, four_r1, False/*mode32*/));
            add_to_sp( env, 16 );

            ///* Restore default FPU rounding. */
            //set_FPU_rounding_default( env );
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         default: 
            break;
      }
   } /* if (e->tag == Iex_Binop) */


   /* --------- UNARY ops --------- */
   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {

      /* CmpwNEZ64(e) */
      case Iop_CmpwNEZ64: {
         HReg argHi, argLo;
         HReg tmp1  = newVRegI(env);
         HReg tmp2  = newVRegI(env);
         iselInt64Expr(&argHi, &argLo, env, e->Iex.Unop.arg);
         /* tmp1 = argHi | argLo */
         addInstr(env, PPCInstr_Alu(Palu_OR, tmp1, argHi, PPCRH_Reg(argLo)));
         /* tmp2 = (tmp1 | -tmp1) >>s 31 */
         addInstr(env, PPCInstr_Unary(Pun_NEG,tmp2,tmp1));
         addInstr(env, PPCInstr_Alu(Palu_OR, tmp2, tmp2, PPCRH_Reg(tmp1)));
         addInstr(env, PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/, 
                                     tmp2, tmp2, PPCRH_Imm(False, 31)));
         *rHi = tmp2;
         *rLo = tmp2; /* yes, really tmp2 */
         return;
      }

      /* Left64 */
      case Iop_Left64: {
         HReg argHi, argLo;
         HReg zero32 = newVRegI(env);
         HReg resHi  = newVRegI(env);
         HReg resLo  = newVRegI(env);
         iselInt64Expr(&argHi, &argLo, env, e->Iex.Unop.arg);
         vassert(env->mode64 == False);
         addInstr(env, PPCInstr_LI(zero32, 0, env->mode64));
         /* resHi:resLo = - argHi:argLo */
         addInstr(env, PPCInstr_AddSubC( False/*sub*/, True/*set carry*/,
                                         resLo, zero32, argLo ));
         addInstr(env, PPCInstr_AddSubC( False/*sub*/, False/*read carry*/,
                                         resHi, zero32, argHi ));
         /* resHi:resLo |= srcHi:srcLo */
         addInstr(env, PPCInstr_Alu(Palu_OR, resLo, resLo, PPCRH_Reg(argLo)));
         addInstr(env, PPCInstr_Alu(Palu_OR, resHi, resHi, PPCRH_Reg(argHi)));
         *rHi = resHi;
         *rLo = resLo;
         return;
      }

      /* 32Sto64(e) */
      case Iop_32Sto64: {
         HReg tHi = newVRegI(env);
         HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/,
                                     tHi, src, PPCRH_Imm(False,31)));
         *rHi = tHi;
         *rLo = src;
         return;
      }

      /* 32Uto64(e) */
      case Iop_32Uto64: {
         HReg tHi = newVRegI(env);
         HReg tLo = iselWordExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_LI(tHi, 0, False/*mode32*/));
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
         PPCAMode *am_off0, *am_offLO, *am_offHI;
         sub_from_sp( env, 32 );     // Move SP down 32 bytes
         
         // get a quadword aligned address within our stack space
         r_aligned16 = get_sp_aligned16( env );
         am_off0  = PPCAMode_IR( 0,     r_aligned16 );
         am_offHI = PPCAMode_IR( off,   r_aligned16 );
         am_offLO = PPCAMode_IR( off+4, r_aligned16 );
         
         // store as Vec128
         addInstr(env,
                  PPCInstr_AvLdSt( False/*store*/, 16, vec, am_off0 ));
         
         // load hi,lo words (of hi/lo half of vec) as Ity_I32's
         addInstr(env,
                  PPCInstr_Load( 4, tHi, am_offHI, False/*mode32*/ ));
         addInstr(env,
                  PPCInstr_Load( 4, tLo, am_offLO, False/*mode32*/ ));
         
         add_to_sp( env, 32 );       // Reset SP
         *rHi = tHi;
         *rLo = tLo;
         return;
      }

      /* could do better than this, but for now ... */
      case Iop_1Sto64: {
         HReg tLo = newVRegI(env);
         HReg tHi = newVRegI(env);
         PPCCondCode cond = iselCondCode(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_Set(cond,tLo));
         addInstr(env, PPCInstr_Shft(Pshft_SHL, True/*32bit shift*/,
                                     tLo, tLo, PPCRH_Imm(False,31)));
         addInstr(env, PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/,
                                     tLo, tLo, PPCRH_Imm(False,31)));
         addInstr(env, mk_iMOVds_RR(tHi, tLo));
         *rHi = tHi;
         *rLo = tLo;
         return;
      }

      /* ReinterpF64asI64(e) */
      /* Given an IEEE754 double, produce an I64 with the same bit
         pattern. */
      case Iop_ReinterpF64asI64: {
         PPCAMode *am_addr0, *am_addr1;
         HReg fr_src  = iselDblExpr(env, e->Iex.Unop.arg);
         HReg r_dstLo = newVRegI(env);
         HReg r_dstHi = newVRegI(env);
         
         sub_from_sp( env, 16 );     // Move SP down 16 bytes
         am_addr0 = PPCAMode_IR( 0, StackFramePtr(False/*mode32*/) );
         am_addr1 = PPCAMode_IR( 4, StackFramePtr(False/*mode32*/) );

         // store as F64
         addInstr(env, PPCInstr_FpLdSt( False/*store*/, 8,
                                        fr_src, am_addr0 ));
         
         // load hi,lo as Ity_I32's
         addInstr(env, PPCInstr_Load( 4, r_dstHi,
                                      am_addr0, False/*mode32*/ ));
         addInstr(env, PPCInstr_Load( 4, r_dstLo,
                                      am_addr1, False/*mode32*/ ));
         *rHi = r_dstHi;
         *rLo = r_dstLo;
         
         add_to_sp( env, 16 );       // Reset SP
         return;
      }

      default:
         break;
      }
   } /* if (e->tag == Iex_Unop) */

   vex_printf("iselInt64Expr(ppc): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselInt64Expr(ppc)");
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

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_BE) {
      PPCAMode* am_addr;
      HReg r_dst = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_F32);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, Ity_F32/*xfer*/);
      addInstr(env, PPCInstr_FpLdSt(True/*load*/, 4, r_dst, am_addr));
      return r_dst;
   }

   if (e->tag == Iex_Get) {
      HReg r_dst = newVRegF(env);
      PPCAMode* am_addr = PPCAMode_IR( e->Iex.Get.offset,
                                       GuestStatePtr(env->mode64) );
      addInstr(env, PPCInstr_FpLdSt( True/*load*/, 4, r_dst, am_addr ));
      return r_dst;
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
      HReg      fsrc    = iselDblExpr(env, e->Iex.Unop.arg);
      HReg      fdst    = newVRegF(env);
      PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );

      sub_from_sp( env, 16 );
      // store as F32, hence truncating
      addInstr(env, PPCInstr_FpLdSt( False/*store*/, 4,
                                     fsrc, zero_r1 ));
      // and reload.  Good huh?! (sigh)
      addInstr(env, PPCInstr_FpLdSt( True/*load*/, 4,
                                     fdst, zero_r1 ));
      add_to_sp( env, 16 );
      return fdst;
   }

   vex_printf("iselFltExpr(ppc): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselFltExpr_wrk(ppc)");
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
   Bool mode64 = env->mode64;
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_F64);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   /* --------- LITERAL --------- */
   if (e->tag == Iex_Const) {
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
         vpanic("iselDblExpr(ppc): const");

      if (!mode64) {
         HReg r_srcHi = newVRegI(env);
         HReg r_srcLo = newVRegI(env);
         addInstr(env, PPCInstr_LI(r_srcHi, u.u32x2[0], mode64));
         addInstr(env, PPCInstr_LI(r_srcLo, u.u32x2[1], mode64));
         return mk_LoadRR32toFPR( env, r_srcHi, r_srcLo );
      } else { // mode64
         HReg r_src = newVRegI(env);
         addInstr(env, PPCInstr_LI(r_src, u.u64, mode64));
         return mk_LoadR64toFPR( env, r_src );         // 1*I64 -> F64
      }
   }

   /* --------- LOAD --------- */
   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_BE) {
      HReg r_dst = newVRegF(env);
      PPCAMode* am_addr;
      vassert(e->Iex.Load.ty == Ity_F64);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, Ity_F64/*xfer*/);
      addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, r_dst, am_addr));
      return r_dst;
   }

   /* --------- GET --------- */
   if (e->tag == Iex_Get) {
      HReg r_dst = newVRegF(env);
      PPCAMode* am_addr = PPCAMode_IR( e->Iex.Get.offset,
                                       GuestStatePtr(mode64) );
      addInstr(env, PPCInstr_FpLdSt( True/*load*/, 8, r_dst, am_addr ));
      return r_dst;
   }

   /* --------- OPS --------- */
   if (e->tag == Iex_Qop) {
      PPCFpOp fpop = Pfp_INVALID;
      switch (e->Iex.Qop.op) {
         case Iop_MAddF64:    fpop = Pfp_MADDD; break;
         case Iop_MAddF64r32: fpop = Pfp_MADDS; break;
         case Iop_MSubF64:    fpop = Pfp_MSUBD; break;
         case Iop_MSubF64r32: fpop = Pfp_MSUBS; break;
         default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg r_dst  = newVRegF(env);
         HReg r_srcML  = iselDblExpr(env, e->Iex.Qop.arg2);
         HReg r_srcMR  = iselDblExpr(env, e->Iex.Qop.arg3);
         HReg r_srcAcc = iselDblExpr(env, e->Iex.Qop.arg4);
         set_FPU_rounding_mode( env, e->Iex.Qop.arg1 );
         addInstr(env, PPCInstr_FpMulAcc(fpop, r_dst, 
                                               r_srcML, r_srcMR, r_srcAcc));
         return r_dst;
      }
   }

   if (e->tag == Iex_Triop) {
      PPCFpOp fpop = Pfp_INVALID;
      switch (e->Iex.Triop.op) {
         case Iop_AddF64:    fpop = Pfp_ADDD; break;
         case Iop_SubF64:    fpop = Pfp_SUBD; break;
         case Iop_MulF64:    fpop = Pfp_MULD; break;
         case Iop_DivF64:    fpop = Pfp_DIVD; break;
         case Iop_AddF64r32: fpop = Pfp_ADDS; break;
         case Iop_SubF64r32: fpop = Pfp_SUBS; break;
         case Iop_MulF64r32: fpop = Pfp_MULS; break;
         case Iop_DivF64r32: fpop = Pfp_DIVS; break;
         default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg r_dst  = newVRegF(env);
         HReg r_srcL = iselDblExpr(env, e->Iex.Triop.arg2);
         HReg r_srcR = iselDblExpr(env, e->Iex.Triop.arg3);
         set_FPU_rounding_mode( env, e->Iex.Triop.arg1 );
         addInstr(env, PPCInstr_FpBinary(fpop, r_dst, r_srcL, r_srcR));
         return r_dst;
      }
   }

   if (e->tag == Iex_Binop) {
      PPCFpOp fpop = Pfp_INVALID;
      switch (e->Iex.Binop.op) {
         case Iop_SqrtF64: fpop = Pfp_SQRT; break;
         default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg fr_dst = newVRegF(env);
         HReg fr_src = iselDblExpr(env, e->Iex.Binop.arg2);
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );
         addInstr(env, PPCInstr_FpUnary(fpop, fr_dst, fr_src));
         return fr_dst;
      }
   }

   if (e->tag == Iex_Binop) {

      if (e->Iex.Binop.op == Iop_RoundF64toF32) {
         HReg r_dst = newVRegF(env);
         HReg r_src = iselDblExpr(env, e->Iex.Binop.arg2);
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );
         addInstr(env, PPCInstr_FpRSP(r_dst, r_src));
         //set_FPU_rounding_default( env );
         return r_dst;
      }

      if (e->Iex.Binop.op == Iop_I64StoF64) {
         if (mode64) {
            HReg fdst = newVRegF(env);
            HReg isrc = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg r1   = StackFramePtr(env->mode64);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );

            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

            sub_from_sp( env, 16 );

            addInstr(env, PPCInstr_Store(8, zero_r1, isrc, True/*mode64*/));
            addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fdst, zero_r1));
            addInstr(env, PPCInstr_FpCftI(True/*I->F*/, False/*int64*/, 
                                          fdst, fdst));

            add_to_sp( env, 16 );

            ///* Restore default FPU rounding. */
            //set_FPU_rounding_default( env );
            return fdst;
         } else {
            /* 32-bit mode */
            HReg fdst = newVRegF(env);
            HReg isrcHi, isrcLo;
            HReg r1   = StackFramePtr(env->mode64);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
            PPCAMode* four_r1 = PPCAMode_IR( 4, r1 );

            iselInt64Expr(&isrcHi, &isrcLo, env, e->Iex.Binop.arg2);

            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

            sub_from_sp( env, 16 );

            addInstr(env, PPCInstr_Store(4, zero_r1, isrcHi, False/*mode32*/));
            addInstr(env, PPCInstr_Store(4, four_r1, isrcLo, False/*mode32*/));
            addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fdst, zero_r1));
            addInstr(env, PPCInstr_FpCftI(True/*I->F*/, False/*int64*/, 
                                          fdst, fdst));

            add_to_sp( env, 16 );

            ///* Restore default FPU rounding. */
            //set_FPU_rounding_default( env );
            return fdst;
         }
      }

   }

   if (e->tag == Iex_Unop) {
      PPCFpOp fpop = Pfp_INVALID;
      switch (e->Iex.Unop.op) {
         case Iop_NegF64:     fpop = Pfp_NEG; break;
         case Iop_AbsF64:     fpop = Pfp_ABS; break;
         case Iop_Est5FRSqrt: fpop = Pfp_RSQRTE; break;
         case Iop_RoundF64toF64_NegINF:  fpop = Pfp_FRIM; break;
         case Iop_RoundF64toF64_PosINF:  fpop = Pfp_FRIP; break;
         case Iop_RoundF64toF64_NEAREST: fpop = Pfp_FRIN; break;
         case Iop_RoundF64toF64_ZERO:    fpop = Pfp_FRIZ; break;
         default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg fr_dst = newVRegF(env);
         HReg fr_src = iselDblExpr(env, e->Iex.Unop.arg);
         addInstr(env, PPCInstr_FpUnary(fpop, fr_dst, fr_src));
         return fr_dst;
      }
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
         case Iop_ReinterpI64asF64: {
            /* Given an I64, produce an IEEE754 double with the same
               bit pattern. */
            if (!mode64) {
               HReg r_srcHi, r_srcLo;
               iselInt64Expr( &r_srcHi, &r_srcLo, env, e->Iex.Unop.arg);
               return mk_LoadRR32toFPR( env, r_srcHi, r_srcLo );
            } else {
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
               return mk_LoadR64toFPR( env, r_src );
            }
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
         PPCCondCode cc = mk_PPCCondCode( Pct_TRUE, Pcf_7EQ );
         HReg r_cond = iselWordExpr_R(env, e->Iex.Mux0X.cond);
         HReg frX    = iselDblExpr(env, e->Iex.Mux0X.exprX);
         HReg fr0    = iselDblExpr(env, e->Iex.Mux0X.expr0);
         HReg fr_dst = newVRegF(env);
         HReg r_tmp  = newVRegI(env);
         addInstr(env, PPCInstr_Alu(Palu_AND, r_tmp,
                                    r_cond, PPCRH_Imm(False,0xFF)));
         addInstr(env, PPCInstr_FpUnary( Pfp_MOV, fr_dst, frX ));
         addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                    7/*cr*/, r_tmp, PPCRH_Imm(False,0)));
         addInstr(env, PPCInstr_FpCMov( cc, fr_dst, fr0 ));
         return fr_dst;
      }
   }

   vex_printf("iselDblExpr(ppc): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselDblExpr_wrk(ppc)");
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
   Bool mode64 = env->mode64;
   PPCAvOp op = Pav_INVALID;
   IRType  ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_V128);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Get) {
      /* Guest state vectors are 16byte aligned,
         so don't need to worry here */
      HReg dst = newVRegV(env);
      addInstr(env,
               PPCInstr_AvLdSt( True/*load*/, 16, dst,
                                PPCAMode_IR( e->Iex.Get.offset,
                                             GuestStatePtr(mode64) )));
      return dst;
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_BE) {
      PPCAMode* am_addr;
      HReg v_dst = newVRegV(env);
      vassert(e->Iex.Load.ty == Ity_V128);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, Ity_V128/*xfer*/);
      addInstr(env, PPCInstr_AvLdSt( True/*load*/, 16, v_dst, am_addr));
      return v_dst;
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {

      case Iop_NotV128: {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, arg));
         return dst;
      }

      case Iop_CmpNEZ8x16: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPCInstr_AvBin8x16(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_CmpNEZ16x8: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPCInstr_AvBin16x8(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_CmpNEZ32x4: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPCInstr_AvBin32x4(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_Recip32Fx4:    op = Pavfp_RCPF;    goto do_32Fx4_unary;
      case Iop_RSqrt32Fx4:    op = Pavfp_RSQRTF;  goto do_32Fx4_unary;
      case Iop_I32UtoFx4:     op = Pavfp_CVTU2F;  goto do_32Fx4_unary;
      case Iop_I32StoFx4:     op = Pavfp_CVTS2F;  goto do_32Fx4_unary;
      case Iop_QFtoI32Ux4_RZ: op = Pavfp_QCVTF2U; goto do_32Fx4_unary;
      case Iop_QFtoI32Sx4_RZ: op = Pavfp_QCVTF2S; goto do_32Fx4_unary;
      case Iop_RoundF32x4_RM: op = Pavfp_ROUNDM;  goto do_32Fx4_unary;
      case Iop_RoundF32x4_RP: op = Pavfp_ROUNDP;  goto do_32Fx4_unary;
      case Iop_RoundF32x4_RN: op = Pavfp_ROUNDN;  goto do_32Fx4_unary;
      case Iop_RoundF32x4_RZ: op = Pavfp_ROUNDZ;  goto do_32Fx4_unary;
      do_32Fx4_unary:
      {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, PPCInstr_AvUn32Fx4(op, dst, arg));
         return dst;
      }

      case Iop_32UtoV128: {
         HReg r_aligned16, r_zeros;
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         HReg   dst = newVRegV(env);
         PPCAMode *am_off0, *am_off4, *am_off8, *am_off12;
         sub_from_sp( env, 32 );     // Move SP down

         /* Get a quadword aligned address within our stack space */
         r_aligned16 = get_sp_aligned16( env );
         am_off0  = PPCAMode_IR( 0,  r_aligned16 );
         am_off4  = PPCAMode_IR( 4,  r_aligned16 );
         am_off8  = PPCAMode_IR( 8,  r_aligned16 );
         am_off12 = PPCAMode_IR( 12, r_aligned16 );

         /* Store zeros */
         r_zeros = newVRegI(env);
         addInstr(env, PPCInstr_LI(r_zeros, 0x0, mode64));
         addInstr(env, PPCInstr_Store( 4, am_off0, r_zeros, mode64 ));
         addInstr(env, PPCInstr_Store( 4, am_off4, r_zeros, mode64 ));
         addInstr(env, PPCInstr_Store( 4, am_off8, r_zeros, mode64 ));

         /* Store r_src in low word of quadword-aligned mem */
         addInstr(env, PPCInstr_Store( 4, am_off12, r_src, mode64 ));

         /* Load word into low word of quadword vector reg */
         addInstr(env, PPCInstr_AvLdSt( True/*ld*/, 4, dst, am_off12 ));

         add_to_sp( env, 32 );       // Reset SP
         return dst;
      }

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

      case Iop_64HLtoV128: {
         if (!mode64) {
            HReg     r3, r2, r1, r0, r_aligned16;
            PPCAMode *am_off0, *am_off4, *am_off8, *am_off12;
            HReg     dst = newVRegV(env);
            /* do this via the stack (easy, convenient, etc) */
            sub_from_sp( env, 32 );        // Move SP down
            
            // get a quadword aligned address within our stack space
            r_aligned16 = get_sp_aligned16( env );
            am_off0  = PPCAMode_IR( 0,  r_aligned16 );
            am_off4  = PPCAMode_IR( 4,  r_aligned16 );
            am_off8  = PPCAMode_IR( 8,  r_aligned16 );
            am_off12 = PPCAMode_IR( 12, r_aligned16 );
            
            /* Do the less significant 64 bits */
            iselInt64Expr(&r1, &r0, env, e->Iex.Binop.arg2);
            addInstr(env, PPCInstr_Store( 4, am_off12, r0, mode64 ));
            addInstr(env, PPCInstr_Store( 4, am_off8,  r1, mode64 ));
            /* Do the more significant 64 bits */
            iselInt64Expr(&r3, &r2, env, e->Iex.Binop.arg1);
            addInstr(env, PPCInstr_Store( 4, am_off4, r2, mode64 ));
            addInstr(env, PPCInstr_Store( 4, am_off0, r3, mode64 ));
            
            /* Fetch result back from stack. */
            addInstr(env, PPCInstr_AvLdSt(True/*ld*/, 16, dst, am_off0));
            
            add_to_sp( env, 32 );          // Reset SP
            return dst;
         } else {
            HReg     rHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg     rLo = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg     dst = newVRegV(env);
            HReg     r_aligned16;
            PPCAMode *am_off0, *am_off8;
            /* do this via the stack (easy, convenient, etc) */
            sub_from_sp( env, 32 );        // Move SP down
            
            // get a quadword aligned address within our stack space
            r_aligned16 = get_sp_aligned16( env );
            am_off0  = PPCAMode_IR( 0,  r_aligned16 );
            am_off8  = PPCAMode_IR( 8,  r_aligned16 );
            
            /* Store 2*I64 to stack */
            addInstr(env, PPCInstr_Store( 8, am_off0, rHi, mode64 ));
            addInstr(env, PPCInstr_Store( 8, am_off8, rLo, mode64 ));

            /* Fetch result back from stack. */
            addInstr(env, PPCInstr_AvLdSt(True/*ld*/, 16, dst, am_off0));
            
            add_to_sp( env, 32 );          // Reset SP
            return dst;
         }
      }

      case Iop_Add32Fx4:   op = Pavfp_ADDF;   goto do_32Fx4;
      case Iop_Sub32Fx4:   op = Pavfp_SUBF;   goto do_32Fx4;
      case Iop_Max32Fx4:   op = Pavfp_MAXF;   goto do_32Fx4;
      case Iop_Min32Fx4:   op = Pavfp_MINF;   goto do_32Fx4;
      case Iop_Mul32Fx4:   op = Pavfp_MULF;   goto do_32Fx4;
      case Iop_CmpEQ32Fx4: op = Pavfp_CMPEQF; goto do_32Fx4;
      case Iop_CmpGT32Fx4: op = Pavfp_CMPGTF; goto do_32Fx4;
      case Iop_CmpGE32Fx4: op = Pavfp_CMPGEF; goto do_32Fx4;
      do_32Fx4:
      {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         addInstr(env, PPCInstr_AvBin32Fx4(op, dst, argL, argR));
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
         addInstr(env, PPCInstr_AvBinary(Pav_OR, isNanLR,
                                         isNanL, isNanR));

         addInstr(env, PPCInstr_AvBin32Fx4(Pavfp_CMPGTF, dst,
                                           argL, argR));
         addInstr(env, PPCInstr_AvBinary(Pav_OR, dst, dst, isNanLR));
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_AndV128:    op = Pav_AND;      goto do_AvBin;
      case Iop_OrV128:     op = Pav_OR;       goto do_AvBin;
      case Iop_XorV128:    op = Pav_XOR;      goto do_AvBin;
      do_AvBin: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBinary(op, dst, arg1, arg2));
         return dst;
      }

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
         addInstr(env, PPCInstr_AvBin8x16(op, dst, arg1, arg2));
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
         addInstr(env, PPCInstr_AvBin16x8(op, dst, arg1, arg2));
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
         addInstr(env, PPCInstr_AvBin32x4(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_ShlN8x16: op = Pav_SHL; goto do_AvShift8x16;
      case Iop_SarN8x16: op = Pav_SAR; goto do_AvShift8x16;
      do_AvShift8x16: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_AvBin8x16(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShlN16x8: op = Pav_SHL; goto do_AvShift16x8;
      case Iop_ShrN16x8: op = Pav_SHR; goto do_AvShift16x8;
      case Iop_SarN16x8: op = Pav_SAR; goto do_AvShift16x8;
      do_AvShift16x8: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_AvBin16x8(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShlN32x4: op = Pav_SHL; goto do_AvShift32x4;
      case Iop_ShrN32x4: op = Pav_SHR; goto do_AvShift32x4;
      case Iop_SarN32x4: op = Pav_SAR; goto do_AvShift32x4;
      do_AvShift32x4: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_AvBin32x4(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShrV128: op = Pav_SHR; goto do_AvShiftV128;
      case Iop_ShlV128: op = Pav_SHL; goto do_AvShiftV128;
      do_AvShiftV128: {
         HReg dst    = newVRegV(env);
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2);
         /* Note: shift value gets masked by 127 */
         addInstr(env, PPCInstr_AvBinary(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_Perm8x16: {
         HReg dst   = newVRegV(env);
         HReg v_src = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg v_ctl = iselVecExpr(env, e->Iex.Binop.arg2);
         addInstr(env, PPCInstr_AvPerm(dst, v_src, v_src, v_ctl));
         return dst;
      }

      default:
         break;
      } /* switch (e->Iex.Binop.op) */
   } /* if (e->tag == Iex_Binop) */

   if (e->tag == Iex_Const ) {
      vassert(e->Iex.Const.con->tag == Ico_V128);
      if (e->Iex.Const.con->Ico.V128 == 0x0000) {
         return generate_zeroes_V128(env);
      } 
      else if (e->Iex.Const.con->Ico.V128 == 0xffff) {
         return generate_ones_V128(env);
      }
   }

   vex_printf("iselVecExpr(ppc) (subarch = %s): can't reduce\n",
              LibVEX_ppVexHwCaps(mode64 ? VexArchPPC64 : VexArchPPC32,
                                 env->hwcaps));
   ppIRExpr(e);
   vpanic("iselVecExpr_wrk(ppc)");
}


/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void iselStmt ( ISelEnv* env, IRStmt* stmt )
{
   Bool mode64 = env->mode64;
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n -- ");
      ppIRStmt(stmt);
      vex_printf("\n");
   }

   switch (stmt->tag) {

   /* --------- STORE --------- */
   case Ist_Store: {
      IRType    tya   = typeOfIRExpr(env->type_env, stmt->Ist.Store.addr);
      IRType    tyd   = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);
      IREndness end   = stmt->Ist.Store.end;

      if (end != Iend_BE)
         goto stmt_fail;
      if (!mode64 && (tya != Ity_I32))
         goto stmt_fail;
      if (mode64 && (tya != Ity_I64))
         goto stmt_fail;

      if (tyd == Ity_I8 || tyd == Ity_I16 || tyd == Ity_I32 ||
          (mode64 && (tyd == Ity_I64))) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/);
         HReg r_src = iselWordExpr_R(env, stmt->Ist.Store.data);
         addInstr(env, PPCInstr_Store( toUChar(sizeofIRType(tyd)), 
                                       am_addr, r_src, mode64 ));
         return;
      }
      if (tyd == Ity_F64) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/);
         HReg fr_src = iselDblExpr(env, stmt->Ist.Store.data);
         addInstr(env,
                  PPCInstr_FpLdSt(False/*store*/, 8, fr_src, am_addr));
         return;
      }
      if (tyd == Ity_F32) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/);
         HReg fr_src = iselFltExpr(env, stmt->Ist.Store.data);
         addInstr(env,
                  PPCInstr_FpLdSt(False/*store*/, 4, fr_src, am_addr));
         return;
      }
      if (tyd == Ity_V128) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/);
         HReg v_src = iselVecExpr(env, stmt->Ist.Store.data);
         addInstr(env,
                  PPCInstr_AvLdSt(False/*store*/, 16, v_src, am_addr));
         return;
      }
      if (tyd == Ity_I64 && !mode64) {
         /* Just calculate the address in the register.  Life is too
            short to arse around trying and possibly failing to adjust
            the offset in a 'reg+offset' style amode. */
         HReg rHi32, rLo32;
         HReg r_addr = iselWordExpr_R(env, stmt->Ist.Store.addr);
         iselInt64Expr( &rHi32, &rLo32, env, stmt->Ist.Store.data );
         addInstr(env, PPCInstr_Store( 4/*byte-store*/,
                                       PPCAMode_IR( 0, r_addr ), 
                                       rHi32,
                                       False/*32-bit insn please*/) );
         addInstr(env, PPCInstr_Store( 4/*byte-store*/, 
                                       PPCAMode_IR( 4, r_addr ), 
                                       rLo32,
                                       False/*32-bit insn please*/) );
         return;
      }
      break;
   }

   /* --------- PUT --------- */
   case Ist_Put: {
      IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
      if (ty == Ity_I8  || ty == Ity_I16 ||
          ty == Ity_I32 || ((ty == Ity_I64) && mode64)) {
         HReg r_src = iselWordExpr_R(env, stmt->Ist.Put.data);
         PPCAMode* am_addr = PPCAMode_IR( stmt->Ist.Put.offset,
                                          GuestStatePtr(mode64) );
         addInstr(env, PPCInstr_Store( toUChar(sizeofIRType(ty)), 
                                       am_addr, r_src, mode64 ));
         return;
      }
      if (!mode64 && ty == Ity_I64) {
         HReg rHi, rLo;
         PPCAMode* am_addr  = PPCAMode_IR( stmt->Ist.Put.offset,
                                           GuestStatePtr(mode64) );
         PPCAMode* am_addr4 = advance4(env, am_addr);
         iselInt64Expr(&rHi,&rLo, env, stmt->Ist.Put.data);
         addInstr(env, PPCInstr_Store( 4, am_addr,  rHi, mode64 ));
         addInstr(env, PPCInstr_Store( 4, am_addr4, rLo, mode64 ));
         return;
     }
     if (ty == Ity_V128) {
         /* Guest state vectors are 16byte aligned,
            so don't need to worry here */
         HReg v_src = iselVecExpr(env, stmt->Ist.Put.data);
         PPCAMode* am_addr  = PPCAMode_IR( stmt->Ist.Put.offset,
                                           GuestStatePtr(mode64) );
         addInstr(env,
                  PPCInstr_AvLdSt(False/*store*/, 16, v_src, am_addr));
         return;
      }
      if (ty == Ity_F64) {
         HReg fr_src = iselDblExpr(env, stmt->Ist.Put.data);
         PPCAMode* am_addr = PPCAMode_IR( stmt->Ist.Put.offset,
                                          GuestStatePtr(mode64) );
         addInstr(env, PPCInstr_FpLdSt( False/*store*/, 8,
                                        fr_src, am_addr ));
         return;
      }
      break;
   }
      
   /* --------- Indexed PUT --------- */
   case Ist_PutI: {
      PPCAMode* dst_am
         = genGuestArrayOffset(
              env, stmt->Ist.PutI.descr, 
                   stmt->Ist.PutI.ix, stmt->Ist.PutI.bias );
      IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.PutI.data);
      if (mode64 && ty == Ity_I64) {
         HReg r_src = iselWordExpr_R(env, stmt->Ist.PutI.data);
         addInstr(env, PPCInstr_Store( toUChar(8),
                                       dst_am, r_src, mode64 ));
         return;
      }
      if ((!mode64) && ty == Ity_I32) {
         HReg r_src = iselWordExpr_R(env, stmt->Ist.PutI.data);
         addInstr(env, PPCInstr_Store( toUChar(4),
                                       dst_am, r_src, mode64 ));
         return;
      }
      break;
   }

   /* --------- TMP --------- */
   case Ist_WrTmp: {
      IRTemp tmp = stmt->Ist.WrTmp.tmp;
      IRType ty = typeOfIRTemp(env->type_env, tmp);
      if (ty == Ity_I8  || ty == Ity_I16 ||
          ty == Ity_I32 || ((ty == Ity_I64) && mode64)) {
         HReg r_dst = lookupIRTemp(env, tmp);
         HReg r_src = iselWordExpr_R(env, stmt->Ist.WrTmp.data);
         addInstr(env, mk_iMOVds_RR( r_dst, r_src ));
         return;
      }
      if (!mode64 && ty == Ity_I64) {
         HReg r_srcHi, r_srcLo, r_dstHi, r_dstLo;
         iselInt64Expr(&r_srcHi,&r_srcLo, env, stmt->Ist.WrTmp.data);
         lookupIRTempPair( &r_dstHi, &r_dstLo, env, tmp);
         addInstr(env, mk_iMOVds_RR(r_dstHi, r_srcHi) );
         addInstr(env, mk_iMOVds_RR(r_dstLo, r_srcLo) );
         return;
      }
      if (mode64 && ty == Ity_I128) {
         HReg r_srcHi, r_srcLo, r_dstHi, r_dstLo;
         iselInt128Expr(&r_srcHi,&r_srcLo, env, stmt->Ist.WrTmp.data);
         lookupIRTempPair( &r_dstHi, &r_dstLo, env, tmp);
         addInstr(env, mk_iMOVds_RR(r_dstHi, r_srcHi) );
         addInstr(env, mk_iMOVds_RR(r_dstLo, r_srcLo) );
         return;
      }
      if (ty == Ity_I1) {
         PPCCondCode cond = iselCondCode(env, stmt->Ist.WrTmp.data);
         HReg r_dst = lookupIRTemp(env, tmp);
         addInstr(env, PPCInstr_Set(cond, r_dst));
         return;
      }
      if (ty == Ity_F64) {
         HReg fr_dst = lookupIRTemp(env, tmp);
         HReg fr_src = iselDblExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, PPCInstr_FpUnary(Pfp_MOV, fr_dst, fr_src));
         return;
      }
      if (ty == Ity_F32) {
         HReg fr_dst = lookupIRTemp(env, tmp);
         HReg fr_src = iselFltExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, PPCInstr_FpUnary(Pfp_MOV, fr_dst, fr_src));
         return;
      }
      if (ty == Ity_V128) {
         HReg v_dst = lookupIRTemp(env, tmp);
         HReg v_src = iselVecExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, PPCInstr_AvUnary(Pav_MOV, v_dst, v_src));
         return;
      }
      break;
   }

   /* --------- Load Linked or Store Conditional --------- */
   case Ist_LLSC: {
      IRTemp res    = stmt->Ist.LLSC.result;
      IRType tyRes  = typeOfIRTemp(env->type_env, res);
      IRType tyAddr = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.addr);

      if (stmt->Ist.LLSC.end != Iend_BE)
         goto stmt_fail;
      if (!mode64 && (tyAddr != Ity_I32))
         goto stmt_fail;
      if (mode64 && (tyAddr != Ity_I64))
         goto stmt_fail;

      if (stmt->Ist.LLSC.storedata == NULL) {
         /* LL */
         HReg r_addr = iselWordExpr_R( env, stmt->Ist.LLSC.addr );
         HReg r_dst  = lookupIRTemp(env, res);
         if (tyRes == Ity_I32) {
            addInstr(env, PPCInstr_LoadL( 4, r_dst, r_addr, mode64 ));
            return;
         }
         if (tyRes == Ity_I64 && mode64) {
            addInstr(env, PPCInstr_LoadL( 8, r_dst, r_addr, mode64 ));
            return;
         }
         /* fallthru */;
      } else {
         /* SC */
         HReg   r_res  = lookupIRTemp(env, res); /* :: Ity_I1 */
         HReg   r_a    = iselWordExpr_R(env, stmt->Ist.LLSC.addr);
         HReg   r_src  = iselWordExpr_R(env, stmt->Ist.LLSC.storedata);
         HReg   r_tmp  = newVRegI(env);
         IRType tyData = typeOfIRExpr(env->type_env,
                                      stmt->Ist.LLSC.storedata);
         vassert(tyRes == Ity_I1);
         if (tyData == Ity_I32 || (tyData == Ity_I64 && mode64)) {
            addInstr(env, PPCInstr_StoreC( tyData==Ity_I32 ? 4 : 8,
                                           r_a, r_src, mode64 ));
            addInstr(env, PPCInstr_MfCR( r_tmp ));
            addInstr(env, PPCInstr_Shft(
                             Pshft_SHR,
                             env->mode64 ? False : True
                                /*F:64-bit, T:32-bit shift*/,
                             r_tmp, r_tmp, 
                             PPCRH_Imm(False/*unsigned*/, 29)));
            /* Probably unnecessary, since the IR dest type is Ity_I1,
               and so we are entitled to leave whatever junk we like
               drifting round in the upper 31 or 63 bits of r_res.
               However, for the sake of conservativeness .. */
            addInstr(env, PPCInstr_Alu(
                             Palu_AND, 
                             r_res, r_tmp, 
                             PPCRH_Imm(False/*signed*/, 1)));
            return;
         }
         /* fallthru */
      }
      goto stmt_fail;
      /*NOTREACHED*/
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
      if (!mode64 && retty == Ity_I64) {
         HReg r_dstHi, r_dstLo;
         /* The returned value is in %r3:%r4.  Park it in the
            register-pair associated with tmp. */
         lookupIRTempPair( &r_dstHi, &r_dstLo, env, d->tmp);
         addInstr(env, mk_iMOVds_RR(r_dstHi, hregPPC_GPR3(mode64)));
         addInstr(env, mk_iMOVds_RR(r_dstLo, hregPPC_GPR4(mode64)));
         return;
      }
      if (retty == Ity_I8  || retty == Ity_I16 ||
          retty == Ity_I32 || ((retty == Ity_I64) && mode64)) {
         /* The returned value is in %r3.  Park it in the register
            associated with tmp. */
         HReg r_dst = lookupIRTemp(env, d->tmp);
         addInstr(env, mk_iMOVds_RR(r_dst, hregPPC_GPR3(mode64)));
         return;
      }
      break;
   }

   /* --------- MEM FENCE --------- */
   case Ist_MBE:
      switch (stmt->Ist.MBE.event) {
         case Imbe_Fence:
            addInstr(env, PPCInstr_MFence());
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
   /* Fairly self-explanatory, wouldn't you say? */
   case Ist_NoOp:
       return;

   /* --------- EXIT --------- */
   case Ist_Exit: {
      PPCRI*      ri_dst;
      PPCCondCode cc;
      IRConstTag tag = stmt->Ist.Exit.dst->tag;
      if (!mode64 && (tag != Ico_U32))
         vpanic("iselStmt(ppc): Ist_Exit: dst is not a 32-bit value");
      if (mode64 && (tag != Ico_U64))
         vpanic("iselStmt(ppc64): Ist_Exit: dst is not a 64-bit value");
      ri_dst = iselWordExpr_RI(env, IRExpr_Const(stmt->Ist.Exit.dst));
      cc     = iselCondCode(env,stmt->Ist.Exit.guard);
      addInstr(env, PPCInstr_RdWrLR(True, env->savedLR));
      addInstr(env, PPCInstr_Goto(stmt->Ist.Exit.jk, cc, ri_dst));
      return;
   }

   default: break;
   }
  stmt_fail:
   ppIRStmt(stmt);
   vpanic("iselStmt(ppc)");
}
 

/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

static void iselNext ( ISelEnv* env, IRExpr* next, IRJumpKind jk )
{
   PPCCondCode cond;
   PPCRI* ri;
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- goto {");
      ppIRJumpKind(jk);
      vex_printf("} ");
      ppIRExpr(next);
      vex_printf("\n");
   }
   cond = mk_PPCCondCode( Pct_ALWAYS, Pcf_7EQ );
   ri = iselWordExpr_RI(env, next);
   addInstr(env, PPCInstr_RdWrLR(True, env->savedLR));
   addInstr(env, PPCInstr_Goto(jk, cond, ri));
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire BS to ppc code. */

HInstrArray* iselSB_PPC ( IRSB* bb, VexArch      arch_host,
                                    VexArchInfo* archinfo_host,
                                    VexAbiInfo*  vbi )
{
   Int      i, j;
   HReg     hreg, hregHI;
   ISelEnv* env;
   UInt     hwcaps_host = archinfo_host->hwcaps;
   Bool     mode64 = False;
   UInt     mask32, mask64;

   vassert(arch_host == VexArchPPC32 || arch_host == VexArchPPC64);
   mode64 = arch_host == VexArchPPC64;

   /* do some sanity checks */
   mask32 = VEX_HWCAPS_PPC32_F | VEX_HWCAPS_PPC32_V
            | VEX_HWCAPS_PPC32_FX | VEX_HWCAPS_PPC32_GX;

   mask64 = VEX_HWCAPS_PPC64_V
            | VEX_HWCAPS_PPC64_FX | VEX_HWCAPS_PPC64_GX;

   if (mode64) {
      vassert((hwcaps_host & mask32) == 0);
   } else {
      vassert((hwcaps_host & mask64) == 0);
   }

   /* Make up an initial environment to use. */
   env = LibVEX_Alloc(sizeof(ISelEnv));
   env->vreg_ctr = 0;

   /* Are we being ppc32 or ppc64? */
   env->mode64 = mode64;

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
   env->hwcaps      = hwcaps_host;
   env->previous_rm = NULL;
   env->vbi         = vbi;

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
         if (mode64) { hreg   = mkHReg(j++, HRcInt64,  True); break;
         } else {      hreg   = mkHReg(j++, HRcInt32,  True); break;
         }
      case Ity_I64:  
         if (mode64) { hreg   = mkHReg(j++, HRcInt64,  True); break;
         } else {      hreg   = mkHReg(j++, HRcInt32,  True);
                       hregHI = mkHReg(j++, HRcInt32,  True); break;
         }
      case Ity_I128:   vassert(mode64);
                       hreg   = mkHReg(j++, HRcInt64,  True);
                       hregHI = mkHReg(j++, HRcInt64,  True); break;
      case Ity_F32:
      case Ity_F64:    hreg   = mkHReg(j++, HRcFlt64,  True); break;
      case Ity_V128:   hreg   = mkHReg(j++, HRcVec128, True); break;
      default:
         ppIRType(bb->tyenv->types[i]);
         vpanic("iselBB(ppc): IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* Keep a copy of the link reg, so helper functions don't kill it. */
   env->savedLR = newVRegI(env);
   addInstr(env, PPCInstr_RdWrLR(False, env->savedLR));

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
/*--- end                                     host_ppc_isel.c ---*/
/*---------------------------------------------------------------*/
