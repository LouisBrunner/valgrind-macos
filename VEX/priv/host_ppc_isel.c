
/*---------------------------------------------------------------*/
/*--- begin                                   host_ppc_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

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
#include "host_generic_simd64.h"
#include "host_ppc_defs.h"

/* GPR register class for ppc32/64 */
#define HRcGPR(_mode64) ((_mode64) ? HRcInt64 : HRcInt32)


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
fcfid[.] (Si64->dbl)     if .             y             y
fcfidU[.] (Ui64->dbl)    if .             y             y
fcfids[.] (Si64->sngl)   if .             Y             Y
fcfidus[.] (Ui64->sngl)  if .             Y             Y
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

static Bool isZeroU8 ( IRExpr* e )
{
   return e->tag == Iex_Const
          && e->Iex.Const.con->tag == Ico_U8
          && e->Iex.Const.con->Ico.U8 == 0;
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
 
         - vregmapLo    holds the primary register for the IRTemp.
         - vregmapMedLo holds the secondary register for the IRTemp,
              if any is needed.  That's only for Ity_I64 temps
              in 32 bit mode or Ity_I128 temps in 64-bit mode.
         - vregmapMedHi is only for dealing with Ity_I128 temps in
              32 bit mode.  It holds bits 95:64 (Intel numbering)
              of the IRTemp.
         - vregmapHi is also only for dealing with Ity_I128 temps
              in 32 bit mode.  It holds the most significant bits
              (127:96 in Intel numbering) of the IRTemp.

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
      function calls for this target.

    - The maximum guest address of any guest insn in this block.
      Actually, the address of the highest-addressed byte from any
      insn in this block.  Is set at the start and does not change.
      This is used for detecting jumps which are definitely
      forward-edges from this block, and therefore can be made
      (chained) to the fast entry point of the destination, thereby
      avoiding the destination's event check.
*/
 
typedef
   struct {
      /* Constant -- are set at the start and do not change. */
      IRTypeEnv* type_env;
                              //    64-bit mode              32-bit mode
      HReg*    vregmapLo;     // Low 64-bits [63:0]    Low 32-bits     [31:0]
      HReg*    vregmapMedLo;  // high 64-bits[127:64]  Next 32-bits    [63:32]
      HReg*    vregmapMedHi;  // unused                Next 32-bits    [95:64]
      HReg*    vregmapHi;     // unused                highest 32-bits [127:96]
      Int      n_vregmap;

      /* 27 Jan 06: Not currently used, but should be */
      UInt         hwcaps;

      Bool         mode64;

      const VexAbiInfo*  vbi;   // unused

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
   return env->vregmapLo[tmp];
}

static void lookupIRTempPair ( HReg* vrHI, HReg* vrLO,
                               ISelEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   vassert(! hregIsInvalid(env->vregmapMedLo[tmp]));
   *vrLO = env->vregmapLo[tmp];
   *vrHI = env->vregmapMedLo[tmp];
}

/* Only for used in 32-bit mode */
static void lookupIRTempQuad ( HReg* vrHi, HReg* vrMedHi, HReg* vrMedLo,
                               HReg* vrLo, ISelEnv* env, IRTemp tmp )
{
   vassert(!env->mode64);
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   vassert(! hregIsInvalid(env->vregmapMedLo[tmp]));
   *vrHi    = env->vregmapHi[tmp];
   *vrMedHi = env->vregmapMedHi[tmp];
   *vrMedLo = env->vregmapMedLo[tmp];
   *vrLo    = env->vregmapLo[tmp];
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
   HReg reg
      = mkHReg(True/*vreg*/, HRcGPR(env->mode64), 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*vreg*/, HRcFlt64, 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegV ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*vreg*/, HRcVec128, 0/*enc*/, env->vreg_ctr);
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
static HReg          iselWordExpr_R_wrk ( ISelEnv* env, const IRExpr* e,
                                          IREndness IEndianess );
static HReg          iselWordExpr_R     ( ISelEnv* env, const IRExpr* e,
                                          IREndness IEndianess );

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
                                           Bool syned, const IRExpr* e,
                                           IREndness IEndianess );
static PPCRH*        iselWordExpr_RH     ( ISelEnv* env, 
                                           Bool syned, const IRExpr* e,
                                           IREndness IEndianess );

/* 32-bit mode: compute an I32 into a RI (reg or 32-bit immediate).
   64-bit mode: compute an I64 into a RI (reg or 64-bit immediate). */
static PPCRI*        iselWordExpr_RI_wrk ( ISelEnv* env, const IRExpr* e,
                                           IREndness IEndianess );
static PPCRI*        iselWordExpr_RI     ( ISelEnv* env, const IRExpr* e,
                                           IREndness IEndianess );

/* In 32 bit mode ONLY, compute an I8 into a
   reg-or-5-bit-unsigned-immediate, the latter being an immediate in
   the range 1 .. 31 inclusive.  Used for doing shift amounts. */
static PPCRH*        iselWordExpr_RH5u_wrk ( ISelEnv* env, const IRExpr* e,
                                             IREndness IEndianess );
static PPCRH*        iselWordExpr_RH5u     ( ISelEnv* env, const IRExpr* e,
                                             IREndness IEndianess );

/* In 64-bit mode ONLY, compute an I8 into a
   reg-or-6-bit-unsigned-immediate, the latter being an immediate in
   the range 1 .. 63 inclusive.  Used for doing shift amounts. */
static PPCRH*        iselWordExpr_RH6u_wrk ( ISelEnv* env, const IRExpr* e,
                                             IREndness IEndianess );
static PPCRH*        iselWordExpr_RH6u     ( ISelEnv* env, const IRExpr* e,
                                             IREndness IEndianess );

/* 32-bit mode: compute an I32 into an AMode.
   64-bit mode: compute an I64 into an AMode.

   Requires to know (xferTy) the type of data to be loaded/stored
   using this amode.  That is so that, for 64-bit code generation, any
   PPCAMode_IR returned will have an index (immediate offset) field
   that is guaranteed to be 4-aligned, if there is any chance that the
   amode is to be used in ld/ldu/lda/std/stdu.

   Since there are no such restrictions on 32-bit insns, xferTy is
   ignored for 32-bit code generation. */
static PPCAMode*     iselWordExpr_AMode_wrk ( ISelEnv* env, const IRExpr* e,
                                              IRType xferTy,
                                              IREndness IEndianess );
static PPCAMode*     iselWordExpr_AMode     ( ISelEnv* env, const IRExpr* e,
                                              IRType xferTy,
                                              IREndness IEndianess );

static void iselInt128Expr_to_32x4_wrk ( HReg* rHi, HReg* rMedHi,
                                         HReg* rMedLo, HReg* rLo,
                                         ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess );
static void iselInt128Expr_to_32x4     ( HReg* rHi, HReg* rMedHi,
                                         HReg* rMedLo, HReg* rLo,
                                         ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess );


/* 32-bit mode ONLY: compute an I64 into a GPR pair. */
static void          iselInt64Expr_wrk ( HReg* rHi, HReg* rLo,
                                         ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess );
static void          iselInt64Expr     ( HReg* rHi, HReg* rLo,
                                         ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess );

/* 64-bit mode ONLY: compute an I128 into a GPR64 pair. */
static void          iselInt128Expr_wrk ( HReg* rHi, HReg* rLo, 
                                          ISelEnv* env, const IRExpr* e,
                                          IREndness IEndianess );

static void          iselInt128Expr     ( HReg* rHi, HReg* rLo, 
                                          ISelEnv* env, const IRExpr* e,
                                          IREndness IEndianess );

static PPCCondCode   iselCondCode_wrk ( ISelEnv* env, const IRExpr* e,
                                        IREndness IEndianess );
static PPCCondCode   iselCondCode     ( ISelEnv* env, const IRExpr* e,
                                        IREndness IEndianess );

static HReg          iselDblExpr_wrk ( ISelEnv* env, const IRExpr* e,
                                       IREndness IEndianess );
static HReg          iselDblExpr     ( ISelEnv* env, const IRExpr* e,
                                       IREndness IEndianess );

static HReg          iselFltExpr_wrk ( ISelEnv* env, const IRExpr* e,
                                       IREndness IEndianess );
static HReg          iselFltExpr     ( ISelEnv* env, const IRExpr* e,
                                       IREndness IEndianess );

static HReg          iselVecExpr_wrk ( ISelEnv* env, const IRExpr* e,
                                       IREndness IEndianess );
static HReg          iselVecExpr     ( ISelEnv* env, const IRExpr* e,
                                       IREndness IEndianess );

/* 64-bit mode ONLY. */
static HReg          iselDfp32Expr_wrk ( ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess );
static HReg          iselDfp32Expr     ( ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess );
static HReg          iselDfp64Expr_wrk ( ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess );
static HReg          iselDfp64Expr     ( ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess );
static HReg iselFp128Expr_wrk ( ISelEnv* env, const IRExpr* e,
                                IREndness IEndianess);
static HReg iselFp128Expr     ( ISelEnv* env, const IRExpr* e,
                                IREndness IEndianess);

/* 64-bit mode ONLY: compute an D128 into a GPR64 pair. */
static void iselDfp128Expr_wrk ( HReg* rHi, HReg* rLo, ISelEnv* env,
                                 const IRExpr* e, IREndness IEndianess );
static void iselDfp128Expr     ( HReg* rHi, HReg* rLo, ISelEnv* env,
                                 const IRExpr* e, IREndness IEndianess );

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
   vassert(n <= 1024 && (n%16) == 0);
   addInstr(env, PPCInstr_Alu( Palu_ADD, sp, sp,
                               PPCRH_Imm(True,toUShort(n)) ));
}

static void sub_from_sp ( ISelEnv* env, UInt n )
{
   HReg sp = StackFramePtr(env->mode64);
   vassert(n <= 1024 && (n%16) == 0);
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
                                IRExpr* off, Int bias, IREndness IEndianess )
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
   if (descr->base < 0 || descr->base > 5000) /* somewhat arbitrarily */
      vpanic("genGuestArrayOffset(ppc host)(4)");

   /* Compute off into a reg, %off.  Then return:

         addi %tmp, %off, bias (if bias != 0)
         andi %tmp, nElems-1
         sldi %tmp, shift
         addi %tmp, %tmp, base
         ... Baseblockptr + %tmp ...
   */
   roff = iselWordExpr_R(env, off, IEndianess);
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


/* Do a complete function call.  |guard| is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional.  |retloc| is set to indicate where the
   return value is after the call.  The caller (of this fn) must
   generate code to add |stackAdjustAfterCall| to the stack pointer
   after the call is done. */

static
void doHelperCall ( /*OUT*/UInt*   stackAdjustAfterCall,
                    /*OUT*/RetLoc* retloc,
                    ISelEnv* env,
                    IRExpr* guard,
                    IRCallee* cee, IRType retTy, IRExpr** args,
                    IREndness IEndianess)
{
   PPCCondCode cc;
   HReg        argregs[PPC_N_REGPARMS];
   HReg        tmpregs[PPC_N_REGPARMS];
   Bool        go_fast;
   Int         n_args, i, argreg;
   UInt        argiregs;
   Bool        mode64 = env->mode64;

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
      arguments in registers are supported, hence only PPC_N_REGPARMS x
      (mode32:32 | mode64:64) integer bits in total can be passed.
      In fact the only supported arg type is (mode32:I32 | mode64:I64).

      The return type can be I{64,32,16,8} or V{128,256}.  In the
      latter two cases, it is expected that |args| will contain the
      special node IRExpr_VECRET(), in which case this routine
      generates code to allocate space on the stack for the vector
      return value.  Since we are not passing any scalars on the
      stack, it is enough to preallocate the return space before
      marshalling any arguments, in this case.

      |args| may also contain IRExpr_GSPTR(), in which case the value
      in the guest state pointer register is passed as the
      corresponding argument.

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

   if (n_args > PPC_N_REGPARMS) {
      vpanic("doHelperCall(PPC): cannot currently handle > 8 args");
      // PPC_N_REGPARMS
   }

   /* This is kind of stupid .. the arrays are sized as PPC_N_REGPARMS
      but we then assume that that value is 8. */
   vassert(PPC_N_REGPARMS == 8);
   
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

   /* We'll need space on the stack for the return value.  Avoid
      possible complications with nested calls by using the slow
      scheme. */
   if (retTy == Ity_V128 || retTy == Ity_V256)
      go_fast = False;

   if (go_fast && guard) {
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
         IRExpr* arg = args[i];
         if (UNLIKELY(arg->tag == Iex_GSPTR)) {
            /* that's OK */
         } 
         else if (UNLIKELY(arg->tag == Iex_VECRET)) {
            /* This implies ill-formed IR, since if the IR was
               well-formed, the return-type test above would have
               filtered it out. */
            vpanic("doHelperCall(PPC): invalid IR");
         }
         else if (mightRequireFixedRegs(arg)) {
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
         vassert(argreg < PPC_N_REGPARMS);

         if (arg->tag == Iex_GSPTR) {
            argiregs |= (1 << (argreg+3));
            addInstr(env, mk_iMOVds_RR( argregs[argreg],
                                        GuestStatePtr(mode64) ));
            argreg++;
         } else {
            vassert(arg->tag != Iex_VECRET);
            IRType ty = typeOfIRExpr(env->type_env, arg);
            vassert(ty == Ity_I32 || ty == Ity_I64);
            if (!mode64) {
               if (ty == Ity_I32) { 
                  argiregs |= (1 << (argreg+3));
                  addInstr(env,
                           mk_iMOVds_RR( argregs[argreg],
                                         iselWordExpr_R(env, arg,
							IEndianess) ));
               } else { // Ity_I64 in 32-bit mode
                  HReg rHi, rLo;
                  if ((argreg%2) == 1)
                                 // ppc32 ELF abi spec for passing LONG_LONG
                     argreg++;   // XXX: odd argreg => even rN
                  vassert(argreg < PPC_N_REGPARMS-1);
                  iselInt64Expr(&rHi,&rLo, env, arg, IEndianess);
                  argiregs |= (1 << (argreg+3));
                  addInstr(env, mk_iMOVds_RR( argregs[argreg++], rHi ));
                  argiregs |= (1 << (argreg+3));
                  addInstr(env, mk_iMOVds_RR( argregs[argreg], rLo));
               }
            } else { // mode64
               argiregs |= (1 << (argreg+3));
               addInstr(env, mk_iMOVds_RR( argregs[argreg],
                                           iselWordExpr_R(env, arg,
                                                          IEndianess) ));
            }
            argreg++;
         } /* if (arg == IRExprP__BBPR) */
      }

      /* Fast scheme only applies for unconditional calls.  Hence: */
      cc = mk_PPCCondCode( Pct_ALWAYS, Pcf_NONE );

   } else {

      /* SLOW SCHEME; move via temporaries */
      argreg = 0;

      /* If we have a vector return type, allocate a place for it on
         the stack and record its address.  Rather than figure out the
         complexities of PPC{32,64} ELF ABI stack frame layout, simply
         drop the SP by 1024 and allocate the return point in the
         middle.  I think this should comfortably clear any ABI
         mandated register save areas.  Note that it doesn't maintain
         the backchain as it should, since we're not doing st{d,w}u to
         adjust the SP, but .. that doesn't seem to be a big deal.
         Since we're not expecting to have to unwind out of here. */
      HReg r_vecRetAddr = INVALID_HREG;
      if (retTy == Ity_V128) {
         r_vecRetAddr = newVRegI(env);
         sub_from_sp(env, 512);
         addInstr(env, mk_iMOVds_RR( r_vecRetAddr, StackFramePtr(mode64) ));
         sub_from_sp(env, 512);
      }
      else if (retTy == Ity_V256) {
         vassert(0); //ATC
         r_vecRetAddr = newVRegI(env);
         sub_from_sp(env, 512);
         addInstr(env, mk_iMOVds_RR( r_vecRetAddr, StackFramePtr(mode64) ));
         sub_from_sp(env, 512);
      }

      vassert(n_args >= 0 && n_args <= 8);
      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];
         vassert(argreg < PPC_N_REGPARMS);
         if (UNLIKELY(arg->tag == Iex_GSPTR)) {
            tmpregs[argreg] = newVRegI(env);
            addInstr(env, mk_iMOVds_RR( tmpregs[argreg],
                                        GuestStatePtr(mode64) ));
            nGSPTRs++;
         }
         else if (UNLIKELY(arg->tag == Iex_VECRET)) {
            /* We stashed the address of the return slot earlier, so just
               retrieve it now. */
            vassert(!hregIsInvalid(r_vecRetAddr));
            tmpregs[i] = r_vecRetAddr;
            nVECRETs++;
         }
         else {
            IRType ty = typeOfIRExpr(env->type_env, arg);
            vassert(ty == Ity_I32 || ty == Ity_I64);
            if (!mode64) {
               if (ty == Ity_I32) { 
                  tmpregs[argreg] = iselWordExpr_R(env, arg, IEndianess);
               } else { // Ity_I64 in 32-bit mode
                  HReg rHi, rLo;
                  if ((argreg%2) == 1)
                                // ppc32 ELF abi spec for passing LONG_LONG
                     argreg++;  // XXX: odd argreg => even rN
                  vassert(argreg < PPC_N_REGPARMS-1);
                  iselInt64Expr(&rHi,&rLo, env, arg, IEndianess);
                  tmpregs[argreg++] = rHi;
                  tmpregs[argreg]   = rLo;
               }
            } else { // mode64
               tmpregs[argreg] = iselWordExpr_R(env, arg, IEndianess);
            }
         }
         argreg++;
      }

      /* Now we can compute the condition.  We can't do it earlier
         because the argument computations could trash the condition
         codes.  Be a bit clever to handle the common case where the
         guard is 1:Bit. */
      cc = mk_PPCCondCode( Pct_ALWAYS, Pcf_NONE );
      if (guard) {
         if (guard->tag == Iex_Const 
             && guard->Iex.Const.con->tag == Ico_U1
             && guard->Iex.Const.con->Ico.U1 == True) {
            /* unconditional -- do nothing */
         } else {
            cc = iselCondCode( env, guard, IEndianess );
         }
      }

      /* Move the args to their final destinations. */
      for (i = 0; i < argreg; i++) {
         if (hregIsInvalid(tmpregs[i]))  // Skip invalid regs
            continue;
         /* None of these insns, including any spill code that might
            be generated, may alter the condition codes. */
         argiregs |= (1 << (i+3));
         addInstr( env, mk_iMOVds_RR( argregs[i], tmpregs[i] ) );
      }

   }

   /* Do final checks, set the return values, and generate the call
      instruction proper. */
   if (retTy == Ity_V128 || retTy == Ity_V256) {
      vassert(nVECRETs == 1);
   } else {
      vassert(nVECRETs == 0);
   }

   vassert(nGSPTRs == 0 || nGSPTRs == 1);

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
         /* Result is 512 bytes up the stack, and after it has been
            retrieved, adjust SP upwards by 1024. */
         *retloc = mk_RetLoc_spRel(RLPri_V128SpRel, 512);
         *stackAdjustAfterCall = 1024;
         break;
      case Ity_V256:
         vassert(0); // ATC
         /* Ditto */
         *retloc = mk_RetLoc_spRel(RLPri_V256SpRel, 512);
         *stackAdjustAfterCall = 1024;
         break;
      default:
         /* IR can denote other possible return types, but we don't
            handle those here. */
         vassert(0);
   }

   /* Finally, generate the call itself.  This needs the *retloc value
      set in the switch above, which is why it's at the end. */

   Addr64 target = mode64 ? (Addr)cee->addr
                          : toUInt((Addr)(cee->addr));
   addInstr(env, PPCInstr_Call( cc, target, argiregs, *retloc ));
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
   rounding mode                     | PPC  |  IR
   -----------------------------------------------
   to nearest, ties to even          | 000  | 000
   to zero                           | 001  | 011
   to +infinity                      | 010  | 010
   to -infinity                      | 011  | 001
   +++++ Below are the extended rounding modes for decimal floating point +++++
   to nearest, ties away from 0      | 100  | 100
   to nearest, ties toward 0         | 101  | 111
   to away from 0                    | 110  | 110
   to prepare for shorter precision  | 111  | 101
   */
   HReg r_rmPPC = newVRegI(env);
   HReg r_tmp1  = newVRegI(env);
   HReg r_tmp2  = newVRegI(env);

   vassert(hregClass(r_rmIR) == HRcGPR(env->mode64));

   // r_rmPPC = XOR(r_rmIR, r_rmIR << 1) & 3
   //
   // slwi  tmp1,    r_rmIR, 1
   // xor   tmp1,    r_rmIR, tmp1
   // andi  r_rmPPC, tmp1, 3

   addInstr(env, PPCInstr_Shft(Pshft_SHL, True/*32bit shift*/,
                               r_tmp1, r_rmIR, PPCRH_Imm(False,1)));

   addInstr( env, PPCInstr_Alu( Palu_AND,
                                r_tmp2, r_tmp1, PPCRH_Imm( False, 3 ) ) );

   addInstr( env, PPCInstr_Alu( Palu_XOR,
                                r_rmPPC, r_rmIR, PPCRH_Reg( r_tmp2 ) ) );

   return r_rmPPC;
}


/* Set the FPU's rounding mode: 'mode' is an I32-typed expression
   denoting a value in the range 0 .. 7, indicating a round mode
   encoded as per type IRRoundingMode.  Set the PPC FPSCR to have the
   same rounding.  When the dfp_rm arg is True, set the decimal
   floating point rounding mode bits (29:31); otherwise, set the
   binary floating point rounding mode bits (62:63).

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
void _set_FPU_rounding_mode ( ISelEnv* env, IRExpr* mode, Bool dfp_rm,
                              IREndness IEndianess )
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
   r_src = roundModeIRtoPPC( env, iselWordExpr_R(env, mode, IEndianess) );

   // gpr -> fpr
   if (env->mode64) {
      if (dfp_rm) {
         HReg r_tmp1 = newVRegI( env );
         addInstr( env,
                   PPCInstr_Shft( Pshft_SHL, False/*64bit shift*/,
                                  r_tmp1, r_src, PPCRH_Imm( False, 32 ) ) );
         fr_src = mk_LoadR64toFPR( env, r_tmp1 );
      } else {
         fr_src = mk_LoadR64toFPR( env, r_src ); // 1*I64 -> F64
      }
   } else {
      if (dfp_rm) {
         HReg r_zero = newVRegI( env );
         addInstr( env, PPCInstr_LI( r_zero, 0, env->mode64 ) );
         fr_src = mk_LoadRR32toFPR( env, r_src, r_zero );
      } else {
         fr_src = mk_LoadRR32toFPR( env, r_src, r_src ); // 2*I32 -> F64
      }
   }

   // Move to FPSCR
   addInstr(env, PPCInstr_FpLdFPSCR( fr_src, dfp_rm ));
}

static void set_FPU_rounding_mode ( ISelEnv* env, IRExpr* mode,
                                    IREndness IEndianess )
{
   _set_FPU_rounding_mode(env, mode, False, IEndianess);
}

static void set_FPU_DFP_rounding_mode ( ISelEnv* env, IRExpr* mode,
                                        IREndness IEndianess )
{
   _set_FPU_rounding_mode(env, mode, True, IEndianess);
}

static
Bool FPU_rounding_mode_isOdd (IRExpr* mode) {
   /* If the rounding mode is set to odd, the the expr must be a constant U8
    * value equal to 8.  Otherwise, it must be a bin op expressiong that
    * calculates the value.
    */

   if (mode->tag != Iex_Const)
      return False;

   vassert(mode->Iex.Const.con->tag == Ico_U32);
   vassert(mode->Iex.Const.con->Ico.U32 == 0x8);
   return True;
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
static HReg mk_AvDuplicateRI( ISelEnv* env, IRExpr* e, IREndness IEndianess )
{
   HReg   r_src;
   HReg   dst = newVRegV(env);
   PPCRI* ri  = iselWordExpr_RI(env, e, IEndianess);
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

   {
      /* Store r_src multiple times (sz dependent); then load the dest vector. */
      HReg r_aligned16;
      PPCAMode *am_offset, *am_offset_zero;

      sub_from_sp( env, 32 );     // Move SP down
      /* Get a 16-aligned address within our stack space */
      r_aligned16 = get_sp_aligned16( env );

      Int i;
      Int stride = (sz == 8) ? 1 : (sz == 16) ? 2 : 4;
      UChar num_bytes_to_store = stride;
      am_offset_zero = PPCAMode_IR( 0, r_aligned16 );
      am_offset = am_offset_zero;
      for (i = 0; i < 16; i+=stride, am_offset = PPCAMode_IR( i, r_aligned16)) {
         addInstr(env, PPCInstr_Store( num_bytes_to_store, am_offset, r_src, env->mode64 ));
      }

      /* Effectively splat the r_src value to dst */
      addInstr(env, PPCInstr_AvLdSt( True/*ld*/, 16, dst, am_offset_zero ) );
      add_to_sp( env, 32 );       // Reset SP

      return dst;
   }
}


/* for each lane of vSrc: lane == nan ? laneX = all 1's : all 0's */
static HReg isNan ( ISelEnv* env, HReg vSrc, IREndness IEndianess )
{
   HReg zeros, msk_exp, msk_mnt, expt, mnts, vIsNan;
 
   vassert(hregClass(vSrc) == HRcVec128);

   zeros   = mk_AvDuplicateRI(env, mkU32(0), IEndianess);
   msk_exp = mk_AvDuplicateRI(env, mkU32(0x7F800000), IEndianess);
   msk_mnt = mk_AvDuplicateRI(env, mkU32(0x7FFFFF), IEndianess);
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

static HReg iselWordExpr_R ( ISelEnv* env, const IRExpr* e,
                             IREndness IEndianess )
{
   HReg r = iselWordExpr_R_wrk(env, e, IEndianess);
   /* sanity checks ... */
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif

   vassert(hregClass(r) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselWordExpr_R_wrk ( ISelEnv* env, const IRExpr* e,
                                 IREndness IEndianess )
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
      if (e->Iex.Load.end != IEndianess)
         goto irreducible;
      r_dst   = newVRegI(env);
      am_addr = iselWordExpr_AMode( env, e->Iex.Load.addr, ty/*of xfer*/,
                                    IEndianess );
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
         HReg   r_srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         PPCRH* ri_srcR = NULL;
         /* get right arg into an RH, in the appropriate way */
         switch (aluOp) {
         case Palu_ADD: case Palu_SUB:
            ri_srcR = iselWordExpr_RH(env, True/*signed*/, 
                                      e->Iex.Binop.arg2, IEndianess);
            break;
         case Palu_AND: case Palu_OR: case Palu_XOR:
            ri_srcR = iselWordExpr_RH(env, False/*signed*/,
                                      e->Iex.Binop.arg2, IEndianess);
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
         HReg   r_srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         PPCRH* ri_srcR = NULL;
         /* get right arg into an RH, in the appropriate way */
         switch (shftOp) {
         case Pshft_SHL: case Pshft_SHR: case Pshft_SAR:
            if (!mode64)
               ri_srcR = iselWordExpr_RH5u(env, e->Iex.Binop.arg2, IEndianess);
            else
               ri_srcR = iselWordExpr_RH6u(env, e->Iex.Binop.arg2, IEndianess);
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
          e->Iex.Binop.op == Iop_DivU32 ||
          e->Iex.Binop.op == Iop_DivS32E ||
          e->Iex.Binop.op == Iop_DivU32E) {
         Bool syned  = toBool((e->Iex.Binop.op == Iop_DivS32) || (e->Iex.Binop.op == Iop_DivS32E));
         HReg r_dst  = newVRegI(env);
         HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
         addInstr( env,
                      PPCInstr_Div( ( ( e->Iex.Binop.op == Iop_DivU32E )
                                             || ( e->Iex.Binop.op == Iop_DivS32E ) ) ? True
                                                                                     : False,
                                    syned,
                                    True/*32bit div*/,
                                    r_dst,
                                    r_srcL,
                                    r_srcR ) );
         return r_dst;
      }
      if (e->Iex.Binop.op == Iop_DivS64 || 
          e->Iex.Binop.op == Iop_DivU64 || e->Iex.Binop.op == Iop_DivS64E
          || e->Iex.Binop.op == Iop_DivU64E ) {
         Bool syned  = toBool((e->Iex.Binop.op == Iop_DivS64) ||(e->Iex.Binop.op == Iop_DivS64E));
         HReg r_dst  = newVRegI(env);
         HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
         vassert(mode64);
         addInstr( env,
                      PPCInstr_Div( ( ( e->Iex.Binop.op == Iop_DivS64E )
                                             || ( e->Iex.Binop.op
                                                      == Iop_DivU64E ) ) ? True
                                                                         : False,
                                    syned,
                                    False/*64bit div*/,
                                    r_dst,
                                    r_srcL,
                                    r_srcR ) );
         return r_dst;
      }

      /* No? Anyone for a mul? */
      if (e->Iex.Binop.op == Iop_Mul32
          || e->Iex.Binop.op == Iop_Mul64) {
         Bool syned       = False;
         Bool sz32        = (e->Iex.Binop.op != Iop_Mul64);
         HReg r_dst       = newVRegI(env);
         HReg r_srcL      = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg r_srcR      = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
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
         HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
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
         HReg   srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         PPCRH* srcR  = iselWordExpr_RH(env, syned, e->Iex.Binop.arg2,
                                        IEndianess);
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
         HReg   srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         PPCRH* srcR  = iselWordExpr_RH(env, syned, e->Iex.Binop.arg2,
                                        IEndianess);
         vassert(mode64);
         addInstr(env, PPCInstr_Cmp(syned, False/*64bit cmp*/,
                                    7/*cr*/, srcL, srcR));
         addInstr(env, PPCInstr_MfCR(dst));
         addInstr(env, PPCInstr_Alu(Palu_AND, dst, dst,
                                    PPCRH_Imm(False,7<<1)));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_Max32U) {
         HReg        r1   = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg        r2   = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
         HReg        rdst = newVRegI(env);
         PPCCondCode cc   = mk_PPCCondCode( Pct_TRUE, Pcf_7LT );
         addInstr(env, mk_iMOVds_RR(rdst, r1));
         addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                    7/*cr*/, rdst, PPCRH_Reg(r2)));
         addInstr(env, PPCInstr_CMov(cc, rdst, PPCRI_Reg(r2)));
         return rdst;
      }

      if (e->Iex.Binop.op == Iop_32HLto64) {
         HReg   r_Hi  = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg   r_Lo  = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
         HReg   r_Tmp = newVRegI(env);
         HReg   r_dst = newVRegI(env);
         HReg   msk   = newVRegI(env);
         vassert(mode64);
         /* r_dst = OR( r_Hi<<32, r_Lo ) */
         addInstr(env, PPCInstr_Shft(Pshft_SHL, False/*64bit shift*/,
                                     r_dst, r_Hi, PPCRH_Imm(False,32)));
         addInstr(env, PPCInstr_LI(msk, 0xFFFFFFFF, mode64));
         addInstr(env, PPCInstr_Alu( Palu_AND, r_Tmp, r_Lo,
                                     PPCRH_Reg(msk) ));
         addInstr(env, PPCInstr_Alu( Palu_OR, r_dst, r_dst,
                                     PPCRH_Reg(r_Tmp) ));
         return r_dst;
      }

      if ((e->Iex.Binop.op == Iop_CmpF64) ||
          (e->Iex.Binop.op == Iop_CmpD64) ||
          (e->Iex.Binop.op == Iop_CmpD128)) {
         HReg fr_srcL;
         HReg fr_srcL_lo;
         HReg fr_srcR;
         HReg fr_srcR_lo;

         HReg r_ccPPC   = newVRegI(env);
         HReg r_ccIR    = newVRegI(env);
         HReg r_ccIR_b0 = newVRegI(env);
         HReg r_ccIR_b2 = newVRegI(env);
         HReg r_ccIR_b6 = newVRegI(env);

         if (e->Iex.Binop.op == Iop_CmpF64) {
            fr_srcL = iselDblExpr(env, e->Iex.Binop.arg1, IEndianess);
            fr_srcR = iselDblExpr(env, e->Iex.Binop.arg2, IEndianess);
            addInstr(env, PPCInstr_FpCmp(r_ccPPC, fr_srcL, fr_srcR));

         } else if (e->Iex.Binop.op == Iop_CmpD64) {
            fr_srcL = iselDfp64Expr(env, e->Iex.Binop.arg1, IEndianess);
            fr_srcR = iselDfp64Expr(env, e->Iex.Binop.arg2, IEndianess);
            addInstr(env, PPCInstr_Dfp64Cmp(r_ccPPC, fr_srcL, fr_srcR));

         } else {    //  e->Iex.Binop.op == Iop_CmpD128
            iselDfp128Expr(&fr_srcL, &fr_srcL_lo, env, e->Iex.Binop.arg1,
                           IEndianess);
            iselDfp128Expr(&fr_srcR, &fr_srcR_lo, env, e->Iex.Binop.arg2,
                           IEndianess);
            addInstr(env, PPCInstr_Dfp128Cmp(r_ccPPC, fr_srcL, fr_srcL_lo,
                                             fr_srcR, fr_srcR_lo));
         }

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

      if ( e->Iex.Binop.op == Iop_F64toI32S ||
               e->Iex.Binop.op == Iop_F64toI32U ) {
         /* This works in both mode64 and mode32. */
         HReg      r1      = StackFramePtr(env->mode64);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
         HReg      fsrc    = iselDblExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg      ftmp    = newVRegF(env);
         HReg      idst    = newVRegI(env);

         /* Set host rounding mode */
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );

         sub_from_sp( env, 16 );
         addInstr(env, PPCInstr_FpCftI(False/*F->I*/, True/*int32*/,
                                       e->Iex.Binop.op == Iop_F64toI32S ? True/*syned*/
                                                                     : False,
                                       True/*flt64*/,
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

      if (e->Iex.Binop.op == Iop_F64toI64S || e->Iex.Binop.op == Iop_F64toI64U ) {
         if (mode64) {
            HReg      r1      = StackFramePtr(env->mode64);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
            HReg      fsrc    = iselDblExpr(env, e->Iex.Binop.arg2,
                                            IEndianess);
            HReg      idst    = newVRegI(env);         
            HReg      ftmp    = newVRegF(env);

            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );

            sub_from_sp( env, 16 );
            addInstr(env, PPCInstr_FpCftI(False/*F->I*/, False/*int64*/,
                                          ( e->Iex.Binop.op == Iop_F64toI64S ) ? True
                                                                            : False,
                                          True, ftmp, fsrc));
            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, ftmp, zero_r1));
            addInstr(env, PPCInstr_Load(8, idst, zero_r1, True/*mode64*/));
            add_to_sp( env, 16 );

            ///* Restore default FPU rounding. */
            //set_FPU_rounding_default( env );
            return idst;
         }
      }

      if (e->Iex.Binop.op == Iop_D64toI64S ) {
         HReg      r1      = StackFramePtr(env->mode64);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
         HReg      fr_src  = iselDfp64Expr(env, e->Iex.Binop.arg2, IEndianess);
         HReg      idst    = newVRegI(env);
         HReg      ftmp    = newVRegF(env);

         /* Set host rounding mode */
         set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         addInstr(env, PPCInstr_Dfp64Unary(Pfp_DCTFIX, ftmp, fr_src));
         sub_from_sp( env, 16 );
         addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, ftmp, zero_r1));
         addInstr(env, PPCInstr_Load(8, idst, zero_r1, mode64));

         add_to_sp( env, 16 );

         ///* Restore default FPU rounding. */
         //set_FPU_rounding_default( env );
         return idst;
      }

      if (e->Iex.Binop.op == Iop_D128toI64S ) {
         PPCFpOp fpop = Pfp_DCTFIXQ;
         HReg r_srcHi = newVRegF(env);
         HReg r_srcLo = newVRegF(env);
         HReg idst    = newVRegI(env);
         HReg ftmp    = newVRegF(env);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );

         set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg2,
                        IEndianess);
         addInstr(env, PPCInstr_DfpD128toD64(fpop, ftmp, r_srcHi, r_srcLo));

         // put the D64 result into an integer register
         sub_from_sp( env, 16 );
         addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, ftmp, zero_r1));
         addInstr(env, PPCInstr_Load(8, idst, zero_r1, True/*mode64*/));
         add_to_sp( env, 16 );
         return idst;
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
         const IRExpr* expr32 = mi.bindee[0];
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, expr32, IEndianess);
         addInstr(env, PPCInstr_Alu(Palu_AND, r_dst,
                                    r_src, PPCRH_Imm(False,1)));
         return r_dst;
      }

      /* 16Uto32(LDbe:I16(expr32)) */
      {
         DECLARE_PATTERN(p_LDbe16_then_16Uto32);
         DEFINE_PATTERN(p_LDbe16_then_16Uto32,
                        unop(Iop_16Uto32,
                             IRExpr_Load(IEndianess,Ity_I16,bind(0))) );
         if (matchIRExpr(&mi,p_LDbe16_then_16Uto32,e)) {
            HReg r_dst = newVRegI(env);
            PPCAMode* amode
               = iselWordExpr_AMode( env, mi.bindee[0], Ity_I16/*xfer*/,
                                     IEndianess );
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
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         UShort mask  = toUShort(op_unop==Iop_16Uto64 ? 0xFFFF :
                                 op_unop==Iop_16Uto32 ? 0xFFFF : 0xFF);
         addInstr(env, PPCInstr_Alu(Palu_AND,r_dst,r_src,
                                    PPCRH_Imm(False,mask)));
         return r_dst;
      }
      case Iop_32Uto64: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         UShort amt   = toUShort(op_unop==Iop_8Sto64  ? 56 : 48);
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
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
         if (op_unop == Iop_Not64) vassert(mode64);
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_Unary(Pun_NOT,r_dst,r_src));
         return r_dst;
      }
      case Iop_64HIto32: {
         if (!mode64) {
            HReg rHi, rLo;
            iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg, IEndianess);
            return rHi; /* and abandon rLo .. poor wee thing :-) */
         } else {
            HReg   r_dst = newVRegI(env);
            HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
            addInstr(env,
                     PPCInstr_Shft(Pshft_SHR, False/*64bit shift*/,
                                   r_dst, r_src, PPCRH_Imm(False,32)));
            return r_dst;
         }
      }
      case Iop_64to32: {
         if (!mode64) {
            HReg rHi, rLo;
            iselInt64Expr(&rHi,&rLo, env, e->Iex.Unop.arg, IEndianess);
            return rLo; /* similar stupid comment to the above ... */
         } else {
            /* This is a no-op. */
            return iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         }
      }
      case Iop_64to16: {
         if (mode64) { /* This is a no-op. */
            return iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         }
         break; /* evidently not used in 32-bit mode */
      }
      case Iop_16HIto8:
      case Iop_32HIto16: {
         HReg   r_dst = newVRegI(env);
         HReg   r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         UShort shift = toUShort(op_unop == Iop_16HIto8 ? 8 : 16);
         addInstr(env,
                  PPCInstr_Shft(Pshft_SHR, True/*32bit shift*/,
                                r_dst, r_src, PPCRH_Imm(False,shift)));
         return r_dst;
      }
      case Iop_128HIto64: 
         if (mode64) {
            HReg rHi, rLo;
            iselInt128Expr(&rHi,&rLo, env, e->Iex.Unop.arg, IEndianess);
            return rHi; /* and abandon rLo .. poor wee thing :-) */
         }
         break;
      case Iop_128to64:
         if (mode64) {
            HReg rHi, rLo;
            iselInt128Expr(&rHi,&rLo, env, e->Iex.Unop.arg, IEndianess);
            return rLo; /* similar stupid comment to the above ... */
         }
         break;
      case Iop_1Uto64:
      case Iop_1Uto32:
      case Iop_1Uto8:
         if ((op_unop != Iop_1Uto64) || mode64) {
            HReg        r_dst = newVRegI(env);
            PPCCondCode cond  = iselCondCode(env, e->Iex.Unop.arg, IEndianess);
            addInstr(env, PPCInstr_Set(cond,r_dst));
            return r_dst;
         }
         break;
      case Iop_1Sto8:
      case Iop_1Sto16:
      case Iop_1Sto32: {
         /* could do better than this, but for now ... */
         HReg        r_dst = newVRegI(env);
         PPCCondCode cond  = iselCondCode(env, e->Iex.Unop.arg, IEndianess);
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
            PPCCondCode cond  = iselCondCode(env, e->Iex.Unop.arg, IEndianess);
            addInstr(env, PPCInstr_Set(cond,r_dst));
            addInstr(env, PPCInstr_Shft(Pshft_SHL, False/*64bit shift*/,
                                        r_dst, r_dst, PPCRH_Imm(False,63)));
            addInstr(env, PPCInstr_Shft(Pshft_SAR, False/*64bit shift*/,
                                        r_dst, r_dst, PPCRH_Imm(False,63)));
            return r_dst;
         }
         break;

      case Iop_Clz32: case Iop_ClzNat32:
      case Iop_Clz64: case Iop_ClzNat64: {
         // cntlz is available even in the most basic (earliest) ppc
         // variants, so it's safe to generate it unconditionally.
         HReg r_src, r_dst;
         PPCUnaryOp op_clz = (op_unop == Iop_Clz32 || op_unop == Iop_ClzNat32)
                                ? Pun_CLZ32 : Pun_CLZ64;
         if ((op_unop == Iop_Clz64 || op_unop == Iop_ClzNat64) && !mode64)
            goto irreducible;
         /* Count leading zeroes. */
         r_dst = newVRegI(env);
         r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_Unary(op_clz,r_dst,r_src));
         return r_dst;
      }

      //case Iop_Ctz32:
      case Iop_CtzNat32:
      //case Iop_Ctz64:
      case Iop_CtzNat64:
      {
         // Generate code using Clz, because we can't assume the host has
         // Ctz.  In particular, part of the fix for bug 386945 involves
         // creating a Ctz in ir_opt.c from smaller fragments.
         PPCUnaryOp op_clz = Pun_CLZ64;
         Int WS = 64;
         if (op_unop == Iop_Ctz32 || op_unop == Iop_CtzNat32) {
            op_clz = Pun_CLZ32;
            WS = 32;
         }
         /* Compute ctz(arg) = wordsize - clz(~arg & (arg - 1)), thusly:
            t1 = arg - 1
            t2 = not arg
            t2 = t2 & t1
            t2 = clz t2
            t1 = WS
            t2 = t1 - t2
            // result in t2
         */
         HReg arg = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         HReg t1 = newVRegI(env);
         HReg t2 = newVRegI(env);
         addInstr(env, PPCInstr_Alu(Palu_SUB, t1, arg, PPCRH_Imm(True, 1)));
         addInstr(env, PPCInstr_Unary(Pun_NOT, t2, arg));
         addInstr(env, PPCInstr_Alu(Palu_AND, t2, t2, PPCRH_Reg(t1)));
         addInstr(env, PPCInstr_Unary(op_clz, t2, t2));
         addInstr(env, PPCInstr_LI(t1, WS, False/*!64-bit imm*/));
         addInstr(env, PPCInstr_Alu(Palu_SUB, t2, t1, PPCRH_Reg(t2)));
         return t2;
      }

      case Iop_PopCount64: {
         // popcnt{x,d} is only available in later arch revs (ISA 3.0,
         // maybe) so it's not really correct to emit it here without a caps
         // check for the host.
         if (mode64) {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
            addInstr(env, PPCInstr_Unary(Pun_POP64, r_dst, r_src));
            return r_dst;
         }
         // We don't expect to be required to handle this in 32-bit mode.
         break;
      }

      case Iop_PopCount32: {
         // Similar comment as for Ctz just above applies -- we really
         // should have a caps check here.

        HReg r_dst = newVRegI(env);
        // This actually generates popcntw, which in 64 bit mode does a
        // 32-bit count individually for both low and high halves of the
        // word.  Per the comment at the top of iselIntExpr_R, in the 64
        // bit mode case, the user of this result is required to ignore
        // the upper 32 bits of the result.  In 32 bit mode this is all
        // moot.  It is however unclear from the PowerISA 3.0 docs that
        // the instruction exists in 32 bit mode; however our own front
        // end (guest_ppc_toIR.c) accepts it, so I guess it does exist.
        HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
        addInstr(env, PPCInstr_Unary(Pun_POP32, r_dst, r_src));
        return r_dst;
      }

      case Iop_Reverse8sIn32_x1: {
         // A bit of a mouthful, but simply .. 32-bit byte swap.
         // This is pretty rubbish code.  We could do vastly better if
         // rotates, and better, rotate-inserts, were allowed.  Note that
         // even on a 64 bit target, the right shifts must be done as 32-bit
         // so as to introduce zero bits in the right places.  So it seems
         // simplest to do the whole sequence in 32-bit insns.
         /*
            r     = <argument>  // working temporary, initial byte order ABCD
            Mask  = 00FF00FF
            nMask = not Mask
            tHi   = and r, Mask
            tHi   = shl tHi, 8
            tLo   = and r, nMask
            tLo   = shr tLo, 8
            r     = or tHi, tLo  // now r has order BADC
            and repeat for 16 bit chunks ..
            Mask  = 0000FFFF
            nMask = not Mask
            tHi   = and r, Mask
            tHi   = shl tHi, 16
            tLo   = and r, nMask
            tLo   = shr tLo, 16
            r     = or tHi, tLo  // now r has order DCBA
         */
         HReg r_src  = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         HReg rr     = newVRegI(env);
         HReg rMask  = newVRegI(env);
         HReg rnMask = newVRegI(env);
         HReg rtHi   = newVRegI(env);
         HReg rtLo   = newVRegI(env);
         // Copy r_src since we need to modify it
         addInstr(env, mk_iMOVds_RR(rr, r_src));
         // Swap within 16-bit lanes
         addInstr(env, PPCInstr_LI(rMask, 0x00FF00FFULL,
                                   False/* !64bit imm*/));
         addInstr(env, PPCInstr_Unary(Pun_NOT, rnMask, rMask));
         addInstr(env, PPCInstr_Alu(Palu_AND, rtHi, rr, PPCRH_Reg(rMask)));
         addInstr(env, PPCInstr_Shft(Pshft_SHL, True/*32 bit shift*/,
                                     rtHi, rtHi,
                                     PPCRH_Imm(False/*!signed imm*/, 8)));
         addInstr(env, PPCInstr_Alu(Palu_AND, rtLo, rr, PPCRH_Reg(rnMask)));
         addInstr(env, PPCInstr_Shft(Pshft_SHR, True/*32 bit shift*/,
                                     rtLo, rtLo,
                                     PPCRH_Imm(False/*!signed imm*/, 8)));
         addInstr(env, PPCInstr_Alu(Palu_OR, rr, rtHi, PPCRH_Reg(rtLo)));
         // And now swap the two 16-bit chunks
         addInstr(env, PPCInstr_LI(rMask, 0x0000FFFFULL,
                                   False/* !64bit imm*/));
         addInstr(env, PPCInstr_Unary(Pun_NOT, rnMask, rMask));
         addInstr(env, PPCInstr_Alu(Palu_AND, rtHi, rr, PPCRH_Reg(rMask)));
         addInstr(env, PPCInstr_Shft(Pshft_SHL, True/*32 bit shift*/,
                                     rtHi, rtHi,
                                     PPCRH_Imm(False/*!signed imm*/, 16)));
         addInstr(env, PPCInstr_Alu(Palu_AND, rtLo, rr, PPCRH_Reg(rnMask)));
         addInstr(env, PPCInstr_Shft(Pshft_SHR, True/*32 bit shift*/,
                                     rtLo, rtLo,
                                     PPCRH_Imm(False/*!signed imm*/, 16)));
         addInstr(env, PPCInstr_Alu(Palu_OR, rr, rtHi, PPCRH_Reg(rtLo)));
         return rr;
      }

      case Iop_Reverse8sIn64_x1: {
	 /* See Iop_Reverse8sIn32_x1, but extended to 64bit.
            Can only be used in 64bit mode.  */
         vassert (mode64);

         HReg r_src  = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         HReg rr     = newVRegI(env);
         HReg rMask  = newVRegI(env);
         HReg rnMask = newVRegI(env);
         HReg rtHi   = newVRegI(env);
         HReg rtLo   = newVRegI(env);

         // Copy r_src since we need to modify it
         addInstr(env, mk_iMOVds_RR(rr, r_src));

         // r = (r & 0x00FF00FF00FF00FF) << 8 | (r & 0xFF00FF00FF00FF00) >> 8
         addInstr(env, PPCInstr_LI(rMask, 0x00FF00FF00FF00FFULL,
                                   True/* 64bit imm*/));
         addInstr(env, PPCInstr_Unary(Pun_NOT, rnMask, rMask));
         addInstr(env, PPCInstr_Alu(Palu_AND, rtHi, rr, PPCRH_Reg(rMask)));
         addInstr(env, PPCInstr_Shft(Pshft_SHL, False/*64 bit shift*/,
                                     rtHi, rtHi,
                                     PPCRH_Imm(False/*!signed imm*/, 8)));
         addInstr(env, PPCInstr_Alu(Palu_AND, rtLo, rr, PPCRH_Reg(rnMask)));
         addInstr(env, PPCInstr_Shft(Pshft_SHR, False/*64 bit shift*/,
                                     rtLo, rtLo,
                                     PPCRH_Imm(False/*!signed imm*/, 8)));
         addInstr(env, PPCInstr_Alu(Palu_OR, rr, rtHi, PPCRH_Reg(rtLo)));

         // r = (r & 0x0000FFFF0000FFFF) << 16 | (r & 0xFFFF0000FFFF0000) >> 16
         addInstr(env, PPCInstr_LI(rMask, 0x0000FFFF0000FFFFULL,
                                   True/* !64bit imm*/));
         addInstr(env, PPCInstr_Unary(Pun_NOT, rnMask, rMask));
         addInstr(env, PPCInstr_Alu(Palu_AND, rtHi, rr, PPCRH_Reg(rMask)));
         addInstr(env, PPCInstr_Shft(Pshft_SHL, False/*64 bit shift*/,
                                     rtHi, rtHi,
                                     PPCRH_Imm(False/*!signed imm*/, 16)));
         addInstr(env, PPCInstr_Alu(Palu_AND, rtLo, rr, PPCRH_Reg(rnMask)));
         addInstr(env, PPCInstr_Shft(Pshft_SHR, False/*64 bit shift*/,
                                     rtLo, rtLo,
                                     PPCRH_Imm(False/*!signed imm*/, 16)));
         addInstr(env, PPCInstr_Alu(Palu_OR, rr, rtHi, PPCRH_Reg(rtLo)));

         // r = (r & 0x00000000FFFFFFFF) << 32 | (r & 0xFFFFFFFF00000000) >> 32
         /* We don't need to mask anymore, just two more shifts and an or.  */
         addInstr(env, mk_iMOVds_RR(rtLo, rr));
         addInstr(env, PPCInstr_Shft(Pshft_SHL, False/*64 bit shift*/,
                                     rtLo, rtLo,
                                     PPCRH_Imm(False/*!signed imm*/, 32)));
         addInstr(env, PPCInstr_Shft(Pshft_SHR, False/*64 bit shift*/,
                                     rr, rr,
                                     PPCRH_Imm(False/*!signed imm*/, 32)));
         addInstr(env, PPCInstr_Alu(Palu_OR, rr, rr, PPCRH_Reg(rtLo)));

         return rr;
      }

      case Iop_Left8:
      case Iop_Left16:
      case Iop_Left32: 
      case Iop_Left64: {
         HReg r_src, r_dst;
         if (op_unop == Iop_Left64 && !mode64)
            goto irreducible;
         r_dst = newVRegI(env);
         r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_Unary(Pun_NEG,r_dst,r_src));
         addInstr(env, PPCInstr_Alu(Palu_OR, r_dst, r_dst, PPCRH_Reg(r_src)));
         return r_dst;
      }

      case Iop_CmpwNEZ32: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_Unary(Pun_NEG,r_dst,r_src));
         addInstr(env, PPCInstr_Alu(Palu_OR, r_dst, r_dst, PPCRH_Reg(r_src)));
         addInstr(env, PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/, 
                                     r_dst, r_dst, PPCRH_Imm(False, 31)));
         return r_dst;
      }

      case Iop_CmpwNEZ64: {
         HReg r_dst = newVRegI(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
         HReg        vec  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         PPCAMode *am_off0, *am_off_word0;
         sub_from_sp( env, 32 );     // Move SP down 32 bytes

         // get a quadword aligned address within our stack space
         r_aligned16 = get_sp_aligned16( env );
         am_off0  = PPCAMode_IR( 0, r_aligned16 );

         /* Note that the store below (done via PPCInstr_AvLdSt) uses
          * stvx, which stores the vector in proper LE format,
          * with byte zero (far right byte of the register in LE format)
          * stored at the lowest memory address.  Therefore, to obtain
          * integer word zero, we need to use that lowest memory address
          * as the base for the load.
          */
         if (IEndianess == Iend_LE)
            am_off_word0 = am_off0;
         else
            am_off_word0 = PPCAMode_IR( 12,r_aligned16 );

         // store vec, load low word to dst
         addInstr(env,
                  PPCInstr_AvLdSt( False/*store*/, 16, vec, am_off0 ));
         addInstr(env,
                  PPCInstr_Load( 4, dst, am_off_word0, mode64 ));

         add_to_sp( env, 32 );       // Reset SP
         return dst;
      }

      case Iop_V128to64:
      case Iop_V128HIto64: 
         if (mode64) {
            HReg     r_aligned16;
            HReg     dst = newVRegI(env);
            HReg     vec = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
            PPCAMode *am_off0, *am_off8, *am_off_arg;
            sub_from_sp( env, 32 );     // Move SP down 32 bytes

            // get a quadword aligned address within our stack space
            r_aligned16 = get_sp_aligned16( env );
            am_off0 = PPCAMode_IR( 0, r_aligned16 );
            am_off8 = PPCAMode_IR( 8 ,r_aligned16 );

            // store vec, load low word or high to dst
            addInstr(env,
                     PPCInstr_AvLdSt( False/*store*/, 16, vec, am_off0 ));
            if (IEndianess == Iend_LE) {
               if (op_unop == Iop_V128HIto64)
                  am_off_arg = am_off8;
               else
                  am_off_arg = am_off0;
            } else {
               if (op_unop == Iop_V128HIto64)
                  am_off_arg = am_off0;
               else
                  am_off_arg = am_off8;
            }
            addInstr(env,
                     PPCInstr_Load( 
                        8, dst, 
                        am_off_arg,
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
         return iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         
      /* ReinterpF64asI64(e) */
      /* Given an IEEE754 double, produce an I64 with the same bit
         pattern. */
      case Iop_ReinterpF64asI64: 
         if (mode64) {
            PPCAMode *am_addr;
            HReg fr_src = iselDblExpr(env, e->Iex.Unop.arg, IEndianess);
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
         HReg fr_src = iselFltExpr(env, e->Iex.Unop.arg, IEndianess);
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
      break;

      case Iop_ReinterpD64asI64:
         if (mode64) {
            PPCAMode *am_addr;
            HReg fr_src = iselDfp64Expr(env, e->Iex.Unop.arg, IEndianess);
            HReg r_dst  = newVRegI(env);

            sub_from_sp( env, 16 );     // Move SP down 16 bytes
            am_addr = PPCAMode_IR( 0, StackFramePtr(mode64) );

            // store as D64
            addInstr(env, PPCInstr_FpLdSt( False/*store*/, 8,
                                           fr_src, am_addr ));
            // load as Ity_I64
            addInstr(env, PPCInstr_Load( 8, r_dst, am_addr, mode64 ));
            add_to_sp( env, 16 );       // Reset SP
            return r_dst;
         } 
         break;

      case Iop_BCDtoDPB: {
         /* the following is only valid in 64 bit mode */
         if (!mode64) break;

         PPCCondCode cc;
         UInt        argiregs;
         HReg        argregs[1];
         HReg        r_dst  = newVRegI(env);
         Int         argreg;

         argiregs = 0;
         argreg = 0;
         argregs[0] = hregPPC_GPR3(mode64);

         argiregs |= (1 << (argreg+3));
         addInstr(env, mk_iMOVds_RR( argregs[argreg++],
                                     iselWordExpr_R(env, e->Iex.Unop.arg,
                                                    IEndianess) ) );

         cc = mk_PPCCondCode( Pct_ALWAYS, Pcf_NONE );
         if (IEndianess == Iend_LE) {
             addInstr(env, PPCInstr_Call( cc, (Addr)h_calc_BCDtoDPB,
                                          argiregs,
                                          mk_RetLoc_simple(RLPri_Int)) );
         } else {
             HWord*      fdescr;
             fdescr = (HWord*)h_calc_BCDtoDPB;
             addInstr(env, PPCInstr_Call( cc, (Addr64)(fdescr[0]),
                                          argiregs,
                                          mk_RetLoc_simple(RLPri_Int)) );
         }

         addInstr(env, mk_iMOVds_RR(r_dst, argregs[0]));
         return r_dst;
      }

      case Iop_DPBtoBCD: {
         /* the following is only valid in 64 bit mode */
         if (!mode64) break;

         PPCCondCode cc;
         UInt        argiregs;
         HReg        argregs[1];
         HReg        r_dst  = newVRegI(env);
         Int         argreg;

         argiregs = 0;
         argreg = 0;
         argregs[0] = hregPPC_GPR3(mode64);

         argiregs |= (1 << (argreg+3));
         addInstr(env, mk_iMOVds_RR( argregs[argreg++],
                                     iselWordExpr_R(env, e->Iex.Unop.arg,
                                                    IEndianess) ) );

         cc = mk_PPCCondCode( Pct_ALWAYS, Pcf_NONE );

        if (IEndianess == Iend_LE) {
            addInstr(env, PPCInstr_Call( cc, (Addr)h_calc_DPBtoBCD,
                                         argiregs, 
                                         mk_RetLoc_simple(RLPri_Int) ) );
        } else {
            HWord*      fdescr;
            fdescr = (HWord*)h_calc_DPBtoBCD;
            addInstr(env, PPCInstr_Call( cc, (Addr64)(fdescr[0]),
                                         argiregs,
                                         mk_RetLoc_simple(RLPri_Int) ) );
         }

         addInstr(env, mk_iMOVds_RR(r_dst, argregs[0]));
         return r_dst;
      }
      case Iop_F32toF16x4_DEP: {
         HReg vdst = newVRegV(env);    /* V128 */
         HReg dst  = newVRegI(env);    /* I64*/
         HReg r0 = newVRegI(env);    /* I16*/
         HReg r1 = newVRegI(env);    /* I16*/
         HReg r2 = newVRegI(env);    /* I16*/
         HReg r3 = newVRegI(env);    /* I16*/
         HReg vsrc  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         PPCAMode *am_off0, *am_off2, *am_off4, *am_off6, *am_off8;
         PPCAMode *am_off10, *am_off12, *am_off14;
         HReg r_aligned16;

         sub_from_sp( env, 32 );     // Move SP down

         /* issue instruction */
         addInstr(env, PPCInstr_AvUnary(Pav_F32toF16x4, vdst, vsrc));

         /* Get a  quadword aligned address within our stack space */
         r_aligned16 = get_sp_aligned16( env );
         am_off0  = PPCAMode_IR( 0, r_aligned16 );
         am_off2  = PPCAMode_IR( 2, r_aligned16 );
         am_off4  = PPCAMode_IR( 4, r_aligned16 );
         am_off6  = PPCAMode_IR( 6, r_aligned16 );
         am_off8  = PPCAMode_IR( 8, r_aligned16 );
         am_off10 = PPCAMode_IR( 10, r_aligned16 );
         am_off12 = PPCAMode_IR( 12, r_aligned16 );
         am_off14 = PPCAMode_IR( 14, r_aligned16 );

         /* Store v128 result to stack. */
         addInstr(env, PPCInstr_AvLdSt(False/*store*/, 16, vdst, am_off0));

         /* fetch four I16 from V128, store into contiguous I64 via stack,  */
         if (IEndianess == Iend_LE) {
            addInstr(env, PPCInstr_Load( 2, r3, am_off12, mode64));
            addInstr(env, PPCInstr_Load( 2, r2, am_off8, mode64));
            addInstr(env, PPCInstr_Load( 2, r1, am_off4, mode64));
            addInstr(env, PPCInstr_Load( 2, r0, am_off0, mode64));
         } else {
            addInstr(env, PPCInstr_Load( 2, r0, am_off14, mode64));
            addInstr(env, PPCInstr_Load( 2, r1, am_off10, mode64));
            addInstr(env, PPCInstr_Load( 2, r2, am_off6, mode64));
            addInstr(env, PPCInstr_Load( 2, r3, am_off2, mode64));
         }

         /* store in contiguous 64-bit values */
         addInstr(env, PPCInstr_Store( 2, am_off6, r3, mode64));
         addInstr(env, PPCInstr_Store( 2, am_off4, r2, mode64));
         addInstr(env, PPCInstr_Store( 2, am_off2, r1, mode64));
         addInstr(env, PPCInstr_Store( 2, am_off0, r0, mode64));

         /* Fetch I64 */
         addInstr(env, PPCInstr_Load(8, dst, am_off0, mode64));

         add_to_sp( env, 32 );          // Reset SP
         return dst;
      }

      default: 
         break;
      }

     switch (e->Iex.Unop.op) {
        case Iop_ExtractExpD64: {

            HReg fr_dst = newVRegI(env);
            HReg fr_src = iselDfp64Expr(env, e->Iex.Unop.arg, IEndianess);
            HReg tmp    = newVRegF(env);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
            addInstr(env, PPCInstr_Dfp64Unary(Pfp_DXEX, tmp, fr_src));

            // put the D64 result into a integer register
            sub_from_sp( env, 16 );
            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, tmp, zero_r1));
            addInstr(env, PPCInstr_Load(8, fr_dst, zero_r1, env->mode64));
            add_to_sp( env, 16 );
            return fr_dst;
         }
         case Iop_ExtractExpD128: {
            HReg fr_dst = newVRegI(env);
            HReg r_srcHi;
            HReg r_srcLo;
            HReg tmp    = newVRegF(env);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );

            iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Unop.arg,
                           IEndianess);
            addInstr(env, PPCInstr_ExtractExpD128(Pfp_DXEXQ, tmp,
                                                  r_srcHi, r_srcLo));

            sub_from_sp( env, 16 );
            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, tmp, zero_r1));
            addInstr(env, PPCInstr_Load(8, fr_dst, zero_r1, env->mode64));
            add_to_sp( env, 16 );
            return fr_dst;
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
                                e->Iex.GetI.ix, e->Iex.GetI.bias,
                                IEndianess );
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
      vassert(ty == e->Iex.CCall.retty); /* well-formedness of IR */

      /* be very restrictive for now.  Only 32/64-bit ints allowed for
         args, and 32 bits or host machine word for return type. */
      if (!(ty == Ity_I32 || (mode64 && ty == Ity_I64)))
         goto irreducible;

      /* Marshal args, do the call, clear stack. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      doHelperCall( &addToSp, &rloc, env, NULL/*guard*/,
                    e->Iex.CCall.cee, e->Iex.CCall.retty, e->Iex.CCall.args,
                    IEndianess );
      vassert(is_sane_RetLoc(rloc));
      vassert(rloc.pri == RLPri_Int);
      vassert(addToSp == 0);

      /* GPR3 now holds the destination address from Pin_Goto */
      HReg r_dst = newVRegI(env);
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
   case Iex_ITE: { // VFD
      if ((ty == Ity_I8  || ty == Ity_I16 ||
           ty == Ity_I32 || ((ty == Ity_I64) && mode64)) &&
          typeOfIRExpr(env->type_env,e->Iex.ITE.cond) == Ity_I1) {
         PPCRI* r1    = iselWordExpr_RI(env, e->Iex.ITE.iftrue, IEndianess);
         HReg   r0    = iselWordExpr_R(env, e->Iex.ITE.iffalse, IEndianess);
         HReg   r_dst = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(r_dst,r0));
         PPCCondCode cc = iselCondCode(env, e->Iex.ITE.cond, IEndianess);
         addInstr(env, PPCInstr_CMov(cc, r_dst, r1));
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
   UInt v = u & 0xFFFF;

   v = (Int)(v << 16) >> 16;   /* sign extend */

   return u == v;
}

static Bool uLong_fits_in_16_bits ( ULong u ) 
{
   /* Is u the same as the sign-extend of its lower 16 bits? */
   ULong v = u & 0xFFFFULL;

   v = (Long)(v << 48) >> 48;   /* sign extend */

   return u == v;
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
                     hregIsVirtual(am->Pam.RR.index) );
   default:
      vpanic("sane_AMode: unknown ppc amode tag");
   }
}

static 
PPCAMode* iselWordExpr_AMode ( ISelEnv* env, const IRExpr* e, IRType xferTy,
                               IREndness IEndianess )
{
   PPCAMode* am = iselWordExpr_AMode_wrk(env, e, xferTy, IEndianess);
   vassert(sane_AMode(env, am));
   return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPCAMode* iselWordExpr_AMode_wrk ( ISelEnv* env, const IRExpr* e,
                                          IRType xferTy, IREndness IEndianess )
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
                             iselWordExpr_R(env, e->Iex.Binop.arg1,
                                            IEndianess) );
      }
      
      /* Add64(expr,expr) */
      if (e->tag == Iex_Binop 
          && e->Iex.Binop.op == Iop_Add64) {
         HReg r_base = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg r_idx  = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
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
                             iselWordExpr_R(env, e->Iex.Binop.arg1,
                                            IEndianess) );
      }
      
      /* Add32(expr,expr) */
      if (e->tag == Iex_Binop 
          && e->Iex.Binop.op == Iop_Add32) {
         HReg r_base = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg r_idx  = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
         return PPCAMode_RR( r_idx, r_base );
      }

   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   return PPCAMode_IR( 0, iselWordExpr_R(env,e,IEndianess) );
}


/* --------------------- RH --------------------- */

/* Compute an I8/I16/I32 (and I64, in 64-bit mode) into a RH
   (reg-or-halfword-immediate).  It's important to specify whether the
   immediate is to be regarded as signed or not.  If yes, this will
   never return -32768 as an immediate; this guaranteed that all
   signed immediates that are return can have their sign inverted if
   need be. */

static PPCRH* iselWordExpr_RH ( ISelEnv* env, Bool syned, const IRExpr* e,
                                IREndness IEndianess )
{
  PPCRH* ri = iselWordExpr_RH_wrk(env, syned, e, IEndianess);
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
static PPCRH* iselWordExpr_RH_wrk ( ISelEnv* env, Bool syned, const IRExpr* e,
                                    IREndness IEndianess )
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
   return PPCRH_Reg( iselWordExpr_R ( env, e, IEndianess ) );
}


/* --------------------- RIs --------------------- */

/* Calculate an expression into an PPCRI operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits, or,
   in 64-bit mode, 64 bits. */

static PPCRI* iselWordExpr_RI ( ISelEnv* env, const IRExpr* e,
                                IREndness IEndianess )
{
   PPCRI* ri = iselWordExpr_RI_wrk(env, e, IEndianess);
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
static PPCRI* iselWordExpr_RI_wrk ( ISelEnv* env, const IRExpr* e,
                                    IREndness IEndianess )
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
   return PPCRI_Reg( iselWordExpr_R ( env, e, IEndianess ) );
}


/* --------------------- RH5u --------------------- */

/* Compute an I8 into a reg-or-5-bit-unsigned-immediate, the latter
   being an immediate in the range 1 .. 31 inclusive.  Used for doing
   shift amounts.  Only used in 32-bit mode. */

static PPCRH* iselWordExpr_RH5u ( ISelEnv* env, const IRExpr* e,
                                  IREndness IEndianess )
{
   PPCRH* ri;
   vassert(!env->mode64);
   ri = iselWordExpr_RH5u_wrk(env, e, IEndianess);
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
static PPCRH* iselWordExpr_RH5u_wrk ( ISelEnv* env, const IRExpr* e,
                                      IREndness IEndianess )
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
   return PPCRH_Reg( iselWordExpr_R ( env, e, IEndianess ) );
}


/* --------------------- RH6u --------------------- */

/* Compute an I8 into a reg-or-6-bit-unsigned-immediate, the latter
   being an immediate in the range 1 .. 63 inclusive.  Used for doing
   shift amounts.  Only used in 64-bit mode. */

static PPCRH* iselWordExpr_RH6u ( ISelEnv* env, const IRExpr* e,
                                  IREndness IEndianess )
{
   PPCRH* ri; 
   vassert(env->mode64);
   ri = iselWordExpr_RH6u_wrk(env, e, IEndianess);
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
static PPCRH* iselWordExpr_RH6u_wrk ( ISelEnv* env, const IRExpr* e,
                                      IREndness IEndianess )
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
   return PPCRH_Reg( iselWordExpr_R ( env, e, IEndianess ) );
}


/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static PPCCondCode iselCondCode ( ISelEnv* env, const IRExpr* e,
                                  IREndness IEndianess )
{
   /* Uh, there's nothing we can sanity check here, unfortunately. */
   return iselCondCode_wrk(env,e, IEndianess);
}

/* DO NOT CALL THIS DIRECTLY ! */
static PPCCondCode iselCondCode_wrk ( ISelEnv* env, const IRExpr* e,
                                      IREndness IEndianess )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I1);

   /* Constant 1:Bit */
   if (e->tag == Iex_Const) {
      // Make a compare that will always be true (or always false):
      vassert(e->Iex.Const.con->Ico.U1 == True || e->Iex.Const.con->Ico.U1 == False);
      HReg r_zero = newVRegI(env);
      addInstr(env, PPCInstr_LI(r_zero, 0, env->mode64));
      addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                 7/*cr*/, r_zero, PPCRH_Reg(r_zero)));
      return mk_PPCCondCode( e->Iex.Const.con->Ico.U1 ? Pct_TRUE : Pct_FALSE,
                             Pcf_7EQ );
   }

   /* Not1(...) */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_Not1) {
      /* Generate code for the arg, and negate the test condition */
      PPCCondCode cond = iselCondCode(env, e->Iex.Unop.arg, IEndianess);
      cond.test = invertCondTest(cond.test);
      return cond;
   }

   /* --- patterns rooted at: 32to1 or 64to1 --- */

   /* 32to1, 64to1 */
   if (e->tag == Iex_Unop &&
       (e->Iex.Unop.op == Iop_32to1 || e->Iex.Unop.op == Iop_64to1)) {
      HReg src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
   /* Note this cloned as CmpNE8(x,0) below. */
   /* could do better -- andi. */
   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_CmpNEZ8) {
      HReg arg = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
      HReg r1 = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
      HReg   r1  = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
      PPCRH* ri2 = iselWordExpr_RH(env, syned, e->Iex.Binop.arg2, IEndianess);
      addInstr(env, PPCInstr_Cmp(syned, True/*32bit cmp*/,
                                 7/*cr*/, r1, ri2));

      switch (e->Iex.Binop.op) {
      case Iop_CmpEQ32:  return mk_PPCCondCode( Pct_TRUE,  Pcf_7EQ );
      case Iop_CmpNE32:  return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
      case Iop_CmpLT32U: case Iop_CmpLT32S:
         return mk_PPCCondCode( Pct_TRUE,  Pcf_7LT );
      case Iop_CmpLE32U: case Iop_CmpLE32S:
         return mk_PPCCondCode( Pct_FALSE, Pcf_7GT );
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
         iselInt64Expr( &hi, &lo, env, e->Iex.Unop.arg, IEndianess );
         addInstr(env, PPCInstr_Alu(Palu_OR, tmp, lo, PPCRH_Reg(hi)));
         addInstr(env, PPCInstr_Cmp(False/*sign*/, True/*32bit cmp*/,
                                    7/*cr*/, tmp,PPCRH_Imm(False,0)));
         return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
      } else {  // mode64
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
      HReg    r1 = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
      PPCRH* ri2 = iselWordExpr_RH(env, syned, e->Iex.Binop.arg2, IEndianess);
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

   /* --- patterns rooted at: CmpNE8 --- */

   /* CmpNE8(x,0) */
   /* Note this is a direct copy of CmpNEZ8 above. */
   /* could do better -- andi. */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_CmpNE8
       && isZeroU8(e->Iex.Binop.arg2)) {
      HReg arg = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
      HReg tmp = newVRegI(env);
      addInstr(env, PPCInstr_Alu(Palu_AND, tmp, arg,
                                 PPCRH_Imm(False,0xFF)));
      addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                 7/*cr*/, tmp, PPCRH_Imm(False,0)));
      return mk_PPCCondCode( Pct_FALSE, Pcf_7EQ );
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

   /* --- And1(x,y), Or1(x,y) --- */
   /* FIXME: We could (and probably should) do a lot better here, by using the
      iselCondCode_C/_R scheme used in the amd64 insn selector. */
   if (e->tag == Iex_Binop
        && (e->Iex.Binop.op == Iop_And1 || e->Iex.Binop.op == Iop_Or1)) {
      HReg x_as_int = newVRegI(env);
      PPCCondCode cc_x = iselCondCode(env, e->Iex.Binop.arg1, IEndianess);
      addInstr(env, PPCInstr_Set(cc_x, x_as_int));

      HReg y_as_int = newVRegI(env);
      PPCCondCode cc_y = iselCondCode(env, e->Iex.Binop.arg2, IEndianess);
      addInstr(env, PPCInstr_Set(cc_y, y_as_int));

      HReg tmp = newVRegI(env);
      PPCAluOp op = e->Iex.Binop.op == Iop_And1 ? Palu_AND : Palu_OR;
      addInstr(env, PPCInstr_Alu(op, tmp, x_as_int, PPCRH_Reg(y_as_int)));

      addInstr(env, PPCInstr_Alu(Palu_AND, tmp, tmp, PPCRH_Imm(False,1)));
      addInstr(env, PPCInstr_Cmp(False/*unsigned*/, True/*32bit cmp*/,
                                 7/*cr*/, tmp, PPCRH_Imm(False,1)));
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

static void iselInt128Expr ( HReg* rHi, HReg* rLo, ISelEnv* env,
                             const IRExpr* e, IREndness IEndianess )
{
   vassert(env->mode64);
   iselInt128Expr_wrk(rHi, rLo, env, e, IEndianess);
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(*rHi) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt128Expr_wrk ( HReg* rHi, HReg* rLo, ISelEnv* env,
                                 const IRExpr* e, IREndness IEndianess )
{
   Bool mode64 = env->mode64;

   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I128);

   /* read 128-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair( rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }

   /* 128-bit GET */
   if (e->tag == Iex_Get) {
      PPCAMode* am_addr = PPCAMode_IR( e->Iex.Get.offset,
                                       GuestStatePtr(mode64) );
      PPCAMode* am_addr4 = advance4(env, am_addr);
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);

      addInstr(env, PPCInstr_Load( 8, tHi, am_addr,  mode64));
      addInstr(env, PPCInstr_Load( 8, tLo, am_addr4, mode64));
      *rHi = tHi;
      *rLo = tLo;
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
         HReg     r_srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         HReg     r_srcR  = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
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
         *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
         *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
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

/* 32-bit mode ONLY: compute a 128-bit value into a register quad */
static void iselInt128Expr_to_32x4 ( HReg* rHi, HReg* rMedHi, HReg* rMedLo,
                                     HReg* rLo, ISelEnv* env, const IRExpr* e,
                                     IREndness IEndianess )
{
   vassert(!env->mode64);
   iselInt128Expr_to_32x4_wrk(rHi, rMedHi, rMedLo, rLo, env, e, IEndianess);
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(*rHi) == HRcInt32);
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rMedHi) == HRcInt32);
   vassert(hregIsVirtual(*rMedHi));
   vassert(hregClass(*rMedLo) == HRcInt32);
   vassert(hregIsVirtual(*rMedLo));
   vassert(hregClass(*rLo) == HRcInt32);
   vassert(hregIsVirtual(*rLo));
}

static void iselInt128Expr_to_32x4_wrk ( HReg* rHi, HReg* rMedHi,
                                         HReg* rMedLo, HReg* rLo,
                                         ISelEnv* env, const IRExpr* e,
                                         IREndness IEndianess )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I128);

   /* read 128-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempQuad( rHi, rMedHi, rMedLo, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }

   if (e->tag == Iex_Binop) {

      IROp op_binop = e->Iex.Binop.op;
      switch (op_binop) {
      case Iop_64HLto128:
         iselInt64Expr(rHi, rMedHi, env, e->Iex.Binop.arg1, IEndianess);
         iselInt64Expr(rMedLo, rLo, env, e->Iex.Binop.arg2, IEndianess);
         return;
      default:
         vex_printf("iselInt128Expr_to_32x4_wrk: Binop case 0x%x not found\n",
                    op_binop);
         break;
      }
   } 

   vex_printf("iselInt128Expr_to_32x4_wrk: e->tag 0x%x not found\n", e->tag);
   return;
}

/* 32-bit mode ONLY: compute a 64-bit value into a register pair,
   which is returned as the first two parameters.  As with
   iselIntExpr_R, these may be either real or virtual regs; in any
   case they must not be changed by subsequent code emitted by the
   caller.  */

static void iselInt64Expr ( HReg* rHi, HReg* rLo,
                            ISelEnv* env, const IRExpr* e,
                            IREndness IEndianess )
{
   vassert(!env->mode64);
   iselInt64Expr_wrk(rHi, rLo, env, e, IEndianess);
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
                                ISelEnv* env, const IRExpr* e,
                                IREndness IEndianess )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I64);

   /* 64-bit load */
   if (e->tag == Iex_Load && e->Iex.Load.end == IEndianess) {
      HReg tLo    = newVRegI(env);
      HReg tHi    = newVRegI(env);
      HReg r_addr = iselWordExpr_R(env, e->Iex.Load.addr, IEndianess);
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

   /* --------- CCALL --------- */
   if(e->tag == Iex_CCall) {
      IRType ty = typeOfIRExpr(env->type_env,e);
      Bool mode64 = env->mode64;

      vassert(ty == e->Iex.CCall.retty); /* well-formedness of IR */

      /* be very restrictive for now.  Only 32-bit ints allowed for
         args, and 32 bits or host machine word for return type. */
      vassert(!(ty == Ity_I32 || (mode64 && ty == Ity_I64)));

      /* Marshal args, do the call, clear stack. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      doHelperCall( &addToSp, &rloc, env, NULL/*guard*/,
                    e->Iex.CCall.cee, e->Iex.CCall.retty, e->Iex.CCall.args,
                    IEndianess );
      vassert(is_sane_RetLoc(rloc));

      vassert(rloc.pri == RLPri_2Int);
      vassert(addToSp == 0);

      /* GPR3 now holds the destination address from Pin_Goto */
      HReg r_dst = newVRegI(env);
      addInstr(env, mk_iMOVds_RR(r_dst, hregPPC_GPR3(mode64)));
      *rHi = r_dst;
      *rLo = r_dst;
      return;
   }

   /* 64-bit ITE */
   if (e->tag == Iex_ITE) { // VFD
      HReg e0Lo, e0Hi, eXLo, eXHi;
      iselInt64Expr(&eXHi, &eXLo, env, e->Iex.ITE.iftrue, IEndianess);
      iselInt64Expr(&e0Hi, &e0Lo, env, e->Iex.ITE.iffalse, IEndianess);
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      addInstr(env, mk_iMOVds_RR(tHi,e0Hi));
      addInstr(env, mk_iMOVds_RR(tLo,e0Lo));
      PPCCondCode cc = iselCondCode(env, e->Iex.ITE.cond, IEndianess);
      addInstr(env, PPCInstr_CMov(cc,tHi,PPCRI_Reg(eXHi)));
      addInstr(env, PPCInstr_CMov(cc,tLo,PPCRI_Reg(eXLo)));
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
            HReg     r_srcL  = iselWordExpr_R(env, e->Iex.Binop.arg1,
                                              IEndianess);
            HReg     r_srcR  = iselWordExpr_R(env, e->Iex.Binop.arg2,
                                              IEndianess);
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
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1, IEndianess);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2, IEndianess);
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
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1, IEndianess);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2, IEndianess);
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
            *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
            *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
            return;

         /* F64toI64[S|U] */
         case Iop_F64toI64S: case Iop_F64toI64U: {
            HReg      tLo     = newVRegI(env);
            HReg      tHi     = newVRegI(env);
            HReg      r1      = StackFramePtr(env->mode64);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
            PPCAMode* four_r1 = PPCAMode_IR( 4, r1 );
            HReg      fsrc    = iselDblExpr(env, e->Iex.Binop.arg2,
                                            IEndianess);
            HReg      ftmp    = newVRegF(env);

            vassert(!env->mode64);
            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );

            sub_from_sp( env, 16 );
            addInstr(env, PPCInstr_FpCftI(False/*F->I*/, False/*int64*/,
                                          (op_binop == Iop_F64toI64S) ? True : False,
                                          True, ftmp, fsrc));
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
         case Iop_D64toI64S: {
            HReg      tLo     = newVRegI(env);
            HReg      tHi     = newVRegI(env);
            HReg      r1      = StackFramePtr(env->mode64);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );
            PPCAMode* four_r1 = PPCAMode_IR( 4, r1 );
            HReg fr_src = iselDfp64Expr(env, e->Iex.Binop.arg2, IEndianess);
            HReg tmp    = newVRegF(env);

            vassert(!env->mode64);
            set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
            addInstr(env, PPCInstr_Dfp64Unary(Pfp_DCTFIX, tmp, fr_src));

            sub_from_sp( env, 16 );
            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, tmp, zero_r1));
            addInstr(env, PPCInstr_Load(4, tHi, zero_r1, False/*mode32*/));
            addInstr(env, PPCInstr_Load(4, tLo, four_r1, False/*mode32*/));
            add_to_sp( env, 16 );
            *rHi = tHi;
            *rLo = tLo;
            return;
         }
         case Iop_D128toI64S: {
            PPCFpOp fpop = Pfp_DCTFIXQ;
            HReg r_srcHi = newVRegF(env);
            HReg r_srcLo = newVRegF(env);
            HReg tLo     = newVRegI(env);
            HReg tHi     = newVRegI(env);
            HReg ftmp    = newVRegF(env);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
            PPCAMode* four_r1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );

            set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
            iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg2,
                           IEndianess);
            addInstr(env, PPCInstr_DfpD128toD64(fpop, ftmp, r_srcHi, r_srcLo));

            // put the D64 result into an integer register pair
            sub_from_sp( env, 16 );
            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, ftmp, zero_r1));
            addInstr(env, PPCInstr_Load(4, tHi, zero_r1, False/*mode32*/));
            addInstr(env, PPCInstr_Load(4, tLo, four_r1, False/*mode32*/));
            add_to_sp( env, 16 );
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
         iselInt64Expr(&argHi, &argLo, env, e->Iex.Unop.arg, IEndianess);
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
         iselInt64Expr(&argHi, &argLo, env, e->Iex.Unop.arg, IEndianess);
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
         HReg src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_Shft(Pshft_SAR, True/*32bit shift*/,
                                     tHi, src, PPCRH_Imm(False,31)));
         *rHi = tHi;
         *rLo = src;
         return;
      }
      case Iop_ExtractExpD64: {
         HReg tmp    = newVRegF(env);
         HReg fr_src = iselDfp64Expr(env, e->Iex.Unop.arg, IEndianess);
         HReg      tLo     = newVRegI(env);
         HReg      tHi     = newVRegI(env);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
         PPCAMode* four_r1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );

         addInstr(env, PPCInstr_Dfp64Unary(Pfp_DXEX, tmp, fr_src));

         // put the D64 result into a integer register pair
         sub_from_sp( env, 16 );
         addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, tmp, zero_r1));
         addInstr(env, PPCInstr_Load(4, tHi, zero_r1, False/*mode32*/));
         addInstr(env, PPCInstr_Load(4, tLo, four_r1, False/*mode32*/));
         add_to_sp( env, 16 );
         *rHi = tHi;
         *rLo = tLo;
         return;
      }
      case Iop_ExtractExpD128: {
         HReg      r_srcHi;
         HReg      r_srcLo;
         HReg      tmp     = newVRegF(env);
         HReg      tLo     = newVRegI(env);
         HReg      tHi     = newVRegI(env);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
         PPCAMode* four_r1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );

         iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_ExtractExpD128(Pfp_DXEXQ, tmp,
                                                  r_srcHi, r_srcLo));

         // put the D64 result into a integer register pair
         sub_from_sp( env, 16 );
         addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, tmp, zero_r1));
         addInstr(env, PPCInstr_Load(4, tHi, zero_r1, False/*mode32*/));
         addInstr(env, PPCInstr_Load(4, tLo, four_r1, False/*mode32*/));
         add_to_sp( env, 16 );
         *rHi = tHi;
         *rLo = tLo;
         return;
      }

      /* 32Uto64(e) */
      case Iop_32Uto64: {
         HReg tHi = newVRegI(env);
         HReg tLo = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_LI(tHi, 0, False/*mode32*/));
         *rHi = tHi;
         *rLo = tLo;
         return;
      }

      case Iop_128to64: {
         /* Narrow, return the low 64-bit half as a 32-bit
          * register pair */
         HReg r_Hi    = INVALID_HREG;
         HReg r_MedHi = INVALID_HREG;
         HReg r_MedLo = INVALID_HREG;
         HReg r_Lo    = INVALID_HREG;

         iselInt128Expr_to_32x4(&r_Hi, &r_MedHi, &r_MedLo, &r_Lo,
                                env, e->Iex.Unop.arg, IEndianess);
         *rHi = r_MedLo;
         *rLo = r_Lo;
         return;
      }

      case Iop_128HIto64: {
         /* Narrow, return the high 64-bit half as a 32-bit
          *  register pair */
         HReg r_Hi    = INVALID_HREG;
         HReg r_MedHi = INVALID_HREG;
         HReg r_MedLo = INVALID_HREG;
         HReg r_Lo    = INVALID_HREG;

         iselInt128Expr_to_32x4(&r_Hi, &r_MedHi, &r_MedLo, &r_Lo,
                                env, e->Iex.Unop.arg, IEndianess);
         *rHi = r_Hi;
         *rLo = r_MedHi;
         return;
      }

      /* V128{HI}to64 */
      case Iop_V128HIto64:
      case Iop_V128to64: {
         HReg r_aligned16;
         Int  off = e->Iex.Unop.op==Iop_V128HIto64 ? 0 : 8;
         HReg tLo = newVRegI(env);
         HReg tHi = newVRegI(env);
         HReg vec = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
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
         PPCCondCode cond = iselCondCode(env, e->Iex.Unop.arg, IEndianess);
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

      case Iop_Not64: {
         HReg xLo, xHi;
         HReg tmpLo = newVRegI(env);
         HReg tmpHi = newVRegI(env);
         iselInt64Expr(&xHi, &xLo, env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_Unary(Pun_NOT,tmpLo,xLo));
         addInstr(env, PPCInstr_Unary(Pun_NOT,tmpHi,xHi));
         *rHi = tmpHi;
         *rLo = tmpLo;
         return;
      }

      /* ReinterpF64asI64(e) */
      /* Given an IEEE754 double, produce an I64 with the same bit
         pattern. */
      case Iop_ReinterpF64asI64: {
         PPCAMode *am_addr0, *am_addr1;
         HReg fr_src  = iselDblExpr(env, e->Iex.Unop.arg, IEndianess);
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

      case Iop_ReinterpD64asI64: {
         HReg fr_src  = iselDfp64Expr(env, e->Iex.Unop.arg, IEndianess);
         PPCAMode *am_addr0, *am_addr1;
         HReg r_dstLo = newVRegI(env);
         HReg r_dstHi = newVRegI(env);


         sub_from_sp( env, 16 );     // Move SP down 16 bytes
         am_addr0 = PPCAMode_IR( 0, StackFramePtr(False/*mode32*/) );
         am_addr1 = PPCAMode_IR( 4, StackFramePtr(False/*mode32*/) );

         // store as D64
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

      case Iop_BCDtoDPB: {
         PPCCondCode cc;
         UInt        argiregs;
         HReg        argregs[2];
         Int         argreg;
         HReg        tLo = newVRegI(env);
         HReg        tHi = newVRegI(env);
         HReg        tmpHi;
         HReg        tmpLo;
         Bool        mode64 = env->mode64;

         argregs[0] = hregPPC_GPR3(mode64);
         argregs[1] = hregPPC_GPR4(mode64);

         argiregs = 0;
         argreg = 0;

         iselInt64Expr( &tmpHi, &tmpLo, env, e->Iex.Unop.arg, IEndianess );

         argiregs |= ( 1 << (argreg+3 ) );
         addInstr( env, mk_iMOVds_RR( argregs[argreg++], tmpHi ) );

         argiregs |= ( 1 << (argreg+3 ) );
         addInstr( env, mk_iMOVds_RR( argregs[argreg], tmpLo ) );

         cc = mk_PPCCondCode( Pct_ALWAYS, Pcf_NONE );

         if (IEndianess == Iend_LE) {
             addInstr( env, PPCInstr_Call( cc, (Addr)h_calc_BCDtoDPB,
                                           argiregs,
                                           mk_RetLoc_simple(RLPri_2Int) ) );
         } else {
             Addr64 target;
             target = mode64 ? (Addr)h_calc_BCDtoDPB :
               toUInt( (Addr)h_calc_BCDtoDPB );
             addInstr( env, PPCInstr_Call( cc, target,
                                           argiregs,
                                           mk_RetLoc_simple(RLPri_2Int) ) );
         }

         addInstr( env, mk_iMOVds_RR( tHi, argregs[argreg-1] ) );
         addInstr( env, mk_iMOVds_RR( tLo, argregs[argreg] ) );

         *rHi = tHi;
         *rLo = tLo;
         return;
      }

      case Iop_DPBtoBCD: {
         PPCCondCode cc;
         UInt        argiregs;
         HReg        argregs[2];
         Int         argreg;
         HReg        tLo = newVRegI(env);
         HReg        tHi = newVRegI(env);
         HReg        tmpHi;
         HReg        tmpLo;
         Bool        mode64 = env->mode64;

         argregs[0] = hregPPC_GPR3(mode64);
         argregs[1] = hregPPC_GPR4(mode64);

         argiregs = 0;
         argreg = 0;

         iselInt64Expr(&tmpHi, &tmpLo, env, e->Iex.Unop.arg, IEndianess);

         argiregs |= (1 << (argreg+3));
         addInstr(env, mk_iMOVds_RR( argregs[argreg++], tmpHi ));

         argiregs |= (1 << (argreg+3));
         addInstr(env, mk_iMOVds_RR( argregs[argreg], tmpLo));

         cc = mk_PPCCondCode( Pct_ALWAYS, Pcf_NONE );

         if (IEndianess == Iend_LE) {
             addInstr(env, PPCInstr_Call( cc, (Addr)h_calc_DPBtoBCD,
                                          argiregs,
                                          mk_RetLoc_simple(RLPri_2Int) ) );
         } else {
             Addr64 target;
             target = mode64 ? (Addr)h_calc_DPBtoBCD :
               toUInt( (Addr)h_calc_DPBtoBCD );
             addInstr(env, PPCInstr_Call( cc, target, argiregs,
                                          mk_RetLoc_simple(RLPri_2Int) ) );
         }

         addInstr(env, mk_iMOVds_RR(tHi, argregs[argreg-1]));
         addInstr(env, mk_iMOVds_RR(tLo, argregs[argreg]));

         *rHi = tHi;
         *rLo = tLo;
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

static HReg iselFltExpr ( ISelEnv* env, const IRExpr* e, IREndness IEndianess )
{
  HReg r = iselFltExpr_wrk( env, e, IEndianess );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcFlt64); /* yes, really Flt64 */
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselFltExpr_wrk ( ISelEnv* env, const IRExpr* e,
                              IREndness IEndianess )
{
   Bool        mode64 = env->mode64;

   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_F32);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == IEndianess) {
      PPCAMode* am_addr;
      HReg r_dst = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_F32);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, Ity_F32/*xfer*/,
                                   IEndianess);
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
      HReg      fsrc    = iselDblExpr(env, e->Iex.Unop.arg, IEndianess);
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

   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_I64UtoF32) {
      if (mode64) {
         HReg fdst = newVRegF(env);
         HReg isrc = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
         HReg r1   = StackFramePtr(env->mode64);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );

         /* Set host rounding mode */
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );

         sub_from_sp( env, 16 );

         addInstr(env, PPCInstr_Store(8, zero_r1, isrc, True/*mode64*/));
         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fdst, zero_r1));
         addInstr(env, PPCInstr_FpCftI(True/*I->F*/, False/*int64*/, 
                                       False, False,
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

         iselInt64Expr(&isrcHi, &isrcLo, env, e->Iex.Binop.arg2, IEndianess);

         /* Set host rounding mode */
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );

         sub_from_sp( env, 16 );

         addInstr(env, PPCInstr_Store(4, zero_r1, isrcHi, False/*mode32*/));
         addInstr(env, PPCInstr_Store(4, four_r1, isrcLo, False/*mode32*/));
         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fdst, zero_r1));
         addInstr(env, PPCInstr_FpCftI(True/*I->F*/, False/*int64*/, 
                                       False, False,
                                       fdst, fdst));

         add_to_sp( env, 16 );

         ///* Restore default FPU rounding. */
         //set_FPU_rounding_default( env );
         return fdst;
      }

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

static HReg iselDblExpr ( ISelEnv* env, const IRExpr* e, IREndness IEndianess )
{
   HReg r = iselDblExpr_wrk( env, e, IEndianess );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselDblExpr_wrk ( ISelEnv* env, const IRExpr* e,
                              IREndness IEndianess )
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
   if (e->tag == Iex_Load && e->Iex.Load.end == IEndianess) {
      HReg r_dst = newVRegF(env);
      PPCAMode* am_addr;
      vassert(e->Iex.Load.ty == Ity_F64);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, Ity_F64/*xfer*/,
                                   IEndianess);
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
      switch (e->Iex.Qop.details->op) {
         case Iop_MAddF64:    fpop = Pfp_MADDD; break;
         case Iop_MAddF64r32: fpop = Pfp_MADDS; break;
         case Iop_MSubF64:    fpop = Pfp_MSUBD; break;
         case Iop_MSubF64r32: fpop = Pfp_MSUBS; break;
         default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg r_dst  = newVRegF(env);
         HReg r_srcML  = iselDblExpr(env, e->Iex.Qop.details->arg2,
                                     IEndianess);
         HReg r_srcMR  = iselDblExpr(env, e->Iex.Qop.details->arg3,
                                     IEndianess);
         HReg r_srcAcc = iselDblExpr(env, e->Iex.Qop.details->arg4,
                                     IEndianess);
         set_FPU_rounding_mode( env, e->Iex.Qop.details->arg1, IEndianess );
         addInstr(env, PPCInstr_FpMulAcc(fpop, r_dst, 
                                               r_srcML, r_srcMR, r_srcAcc));
         return r_dst;
      }
   }

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;
      PPCFpOp fpop = Pfp_INVALID;
      switch (triop->op) {
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
         HReg r_srcL = iselDblExpr(env, triop->arg2, IEndianess);
         HReg r_srcR = iselDblExpr(env, triop->arg3, IEndianess);
         set_FPU_rounding_mode( env, triop->arg1, IEndianess );
         addInstr(env, PPCInstr_FpBinary(fpop, r_dst, r_srcL, r_srcR));
         return r_dst;
      }
   }

   if (e->tag == Iex_Binop) {
      PPCFpOp fpop = Pfp_INVALID;
      switch (e->Iex.Binop.op) {
      case Iop_SqrtF64:   fpop = Pfp_SQRT;   break;
      default: break;
      }
      if (fpop == Pfp_SQRT) {
         HReg fr_dst = newVRegF(env);
         HReg fr_src = iselDblExpr(env, e->Iex.Binop.arg2, IEndianess);
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         addInstr(env, PPCInstr_FpUnary(fpop, fr_dst, fr_src));
         return fr_dst;
      }
   }

   if (e->tag == Iex_Binop) {

      if (e->Iex.Binop.op == Iop_F128toF64) {
         HReg fr_dst = newVRegF(env);
         HReg fr_src = iselFp128Expr(env, e->Iex.Binop.arg2, IEndianess);
         HReg tmp = newVRegV(env);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
         PPCAMode* eight_r1 = PPCAMode_IR( 8, StackFramePtr(env->mode64) );
         PPCFpOp fpop = Pfp_INVALID;

         if (FPU_rounding_mode_isOdd(e->Iex.Binop.arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPQTODRNDODD;
         } else {
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
            fpop = Pfp_FPQTOD;
         }

         addInstr(env, PPCInstr_Fp128Unary(fpop, tmp, fr_src));

         /* result is in a 128-bit vector register, move to 64-bit reg to
          * match the Iop specification.  The result will get moved back
          * to a 128-bit register and stored once the value is returned.
          */
         sub_from_sp( env, 16 );
         addInstr(env, PPCInstr_AvLdSt(False/*store*/, 16, tmp, zero_r1));
         if (IEndianess == Iend_LE)
            addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fr_dst, eight_r1));
         else
            /* High 64-bits stored at lower address */
            addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fr_dst, zero_r1));

         add_to_sp( env, 16 );

         return fr_dst;
      }

      if (e->Iex.Binop.op == Iop_RoundF64toF32) {
         HReg r_dst = newVRegF(env);
         HReg r_src = iselDblExpr(env, e->Iex.Binop.arg2, IEndianess);
         set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         addInstr(env, PPCInstr_FpRSP(r_dst, r_src));
         //set_FPU_rounding_default( env );
         return r_dst;
      }

      if (e->Iex.Binop.op == Iop_I64StoF64 || e->Iex.Binop.op == Iop_I64UtoF64) {
         if (mode64) {
            HReg fdst = newVRegF(env);
            HReg isrc = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
            HReg r1   = StackFramePtr(env->mode64);
            PPCAMode* zero_r1 = PPCAMode_IR( 0, r1 );

            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );

            sub_from_sp( env, 16 );

            addInstr(env, PPCInstr_Store(8, zero_r1, isrc, True/*mode64*/));
            addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fdst, zero_r1));
            addInstr(env, PPCInstr_FpCftI(True/*I->F*/, False/*int64*/, 
                                          e->Iex.Binop.op == Iop_I64StoF64,
                                          True/*fdst is 64 bit*/,
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

            iselInt64Expr(&isrcHi, &isrcLo, env, e->Iex.Binop.arg2,
                          IEndianess);

            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );

            sub_from_sp( env, 16 );

            addInstr(env, PPCInstr_Store(4, zero_r1, isrcHi, False/*mode32*/));
            addInstr(env, PPCInstr_Store(4, four_r1, isrcLo, False/*mode32*/));
            addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fdst, zero_r1));
            addInstr(env, PPCInstr_FpCftI(True/*I->F*/, False/*int64*/, 
                                          e->Iex.Binop.op == Iop_I64StoF64,
                                          True/*fdst is 64 bit*/,
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
         case Iop_RSqrtEst5GoodF64:      fpop = Pfp_RSQRTE; break;
         case Iop_RoundF64toF64_NegINF:  fpop = Pfp_FRIM; break;
         case Iop_RoundF64toF64_PosINF:  fpop = Pfp_FRIP; break;
         case Iop_RoundF64toF64_NEAREST: fpop = Pfp_FRIN; break;
         case Iop_RoundF64toF64_ZERO:    fpop = Pfp_FRIZ; break;
         default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg fr_dst = newVRegF(env);
         HReg fr_src = iselDblExpr(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_FpUnary(fpop, fr_dst, fr_src));
         return fr_dst;
      }
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
      case Iop_F128HItoF64:
      case Iop_F128LOtoF64:
         {
            /* put upper/lower 64-bits of F128 into an F64. */
            HReg     r_aligned16;
            HReg     fdst = newVRegF(env);
            HReg     fsrc = iselFp128Expr(env, e->Iex.Unop.arg, IEndianess);
            PPCAMode *am_off0, *am_off8, *am_off_arg;
            sub_from_sp( env, 32 );     // Move SP down 32 bytes

            // get a quadword aligned address within our stack space
            r_aligned16 = get_sp_aligned16( env );
            am_off0 = PPCAMode_IR( 0, r_aligned16 );
            am_off8 = PPCAMode_IR( 8 ,r_aligned16 );

            /* store 128-bit floating point value to memory, load low word
             * or high to 64-bit destination floating point register
             */
            addInstr(env, PPCInstr_AvLdSt(False/*store*/, 16, fsrc, am_off0));
            if (IEndianess == Iend_LE) {
               if (e->Iex.Binop.op == Iop_F128HItoF64)
                  am_off_arg = am_off8;
               else
                  am_off_arg = am_off0;
            } else {
               if (e->Iex.Binop.op == Iop_F128HItoF64)
                  am_off_arg = am_off0;
               else
                  am_off_arg = am_off8;
            }
            addInstr(env,
                    PPCInstr_FpLdSt( True /*load*/,
                                      8, fdst,
                                      am_off_arg ));
            add_to_sp( env, 32 );       // Reset SP
            return fdst;
         }
         case Iop_ReinterpI64asF64: {
            /* Given an I64, produce an IEEE754 double with the same
               bit pattern. */
            if (!mode64) {
               HReg r_srcHi, r_srcLo;
               iselInt64Expr( &r_srcHi, &r_srcLo, env, e->Iex.Unop.arg,
                               IEndianess);
               return mk_LoadRR32toFPR( env, r_srcHi, r_srcLo );
            } else {
               HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
               return mk_LoadR64toFPR( env, r_src );
            }
         }

         case Iop_F32toF64: {
            if (e->Iex.Unop.arg->tag == Iex_Unop &&
                     e->Iex.Unop.arg->Iex.Unop.op == Iop_ReinterpI32asF32 ) {
               e = e->Iex.Unop.arg;

               HReg src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
               HReg fr_dst = newVRegF(env);
               PPCAMode *am_addr;

               sub_from_sp( env, 16 );        // Move SP down 16 bytes
               am_addr = PPCAMode_IR( 0, StackFramePtr(env->mode64) );

               // store src as Ity_I32's
               addInstr(env, PPCInstr_Store( 4, am_addr, src, env->mode64 ));

               // load single precision float, but the end results loads into a
               // 64-bit FP register -- i.e., F64.
               addInstr(env, PPCInstr_FpLdSt(True/*load*/, 4, fr_dst, am_addr));

               add_to_sp( env, 16 );          // Reset SP
               return fr_dst;
            }


            /* this is a no-op */
            HReg res = iselFltExpr(env, e->Iex.Unop.arg, IEndianess);
            return res;
         }
         default: 
            break;
      }
   }

   /* --------- MULTIPLEX --------- */
   if (e->tag == Iex_ITE) { // VFD
      if (ty == Ity_F64
          && typeOfIRExpr(env->type_env,e->Iex.ITE.cond) == Ity_I1) {
         HReg fr1    = iselDblExpr(env, e->Iex.ITE.iftrue, IEndianess);
         HReg fr0    = iselDblExpr(env, e->Iex.ITE.iffalse, IEndianess);
         HReg fr_dst = newVRegF(env);
         addInstr(env, PPCInstr_FpUnary( Pfp_MOV, fr_dst, fr0 ));
         PPCCondCode cc = iselCondCode(env, e->Iex.ITE.cond, IEndianess);
         addInstr(env, PPCInstr_FpCMov( cc, fr_dst, fr1 ));
         return fr_dst;
      }
   }

   vex_printf("iselDblExpr(ppc): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselDblExpr_wrk(ppc)");
}

static HReg iselDfp32Expr(ISelEnv* env, const IRExpr* e, IREndness IEndianess)
{
   HReg r = iselDfp32Expr_wrk( env, e, IEndianess );
   vassert(hregClass(r) == HRcFlt64);
   vassert( hregIsVirtual(r) );
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselDfp32Expr_wrk(ISelEnv* env, const IRExpr* e,
                              IREndness IEndianess)
{
   Bool mode64 = env->mode64;
   IRType ty = typeOfIRExpr( env->type_env, e );

   vassert( e );
   vassert( ty == Ity_D32 );

   /* --------- GET --------- */
   if (e->tag == Iex_Get) {
      HReg r_dst = newVRegF( env );
      PPCAMode* am_addr = PPCAMode_IR( e->Iex.Get.offset,
                                       GuestStatePtr(mode64) );
      addInstr( env, PPCInstr_FpLdSt( True/*load*/, 8, r_dst, am_addr ) );
      return r_dst;
   }

   /* --------- LOAD --------- */
   if (e->tag == Iex_Load && e->Iex.Load.end == IEndianess) {
      PPCAMode* am_addr;
      HReg r_dst = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_D32);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, Ity_D32/*xfer*/,
                                   IEndianess);
      addInstr(env, PPCInstr_FpLdSt(True/*load*/, 4, r_dst, am_addr));
      return r_dst;
   }

   /* --------- OPS --------- */
   if (e->tag == Iex_Binop) {
      if (e->Iex.Binop.op == Iop_D64toD32) {
         HReg fr_dst = newVRegF(env);
         HReg fr_src = iselDfp64Expr(env, e->Iex.Binop.arg2, IEndianess);
         set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         addInstr(env, PPCInstr_Dfp64Unary(Pfp_DRSP, fr_dst, fr_src));
         return fr_dst;
      }
   }

   ppIRExpr( e );
   vpanic( "iselDfp32Expr_wrk(ppc)" );
}

static HReg iselFp128Expr( ISelEnv* env, const IRExpr* e, IREndness IEndianess )
{
   HReg r = iselFp128Expr_wrk( env, e, IEndianess );
   vassert(hregClass(r) == HRcVec128);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselFp128Expr_wrk( ISelEnv* env, const IRExpr* e,
                               IREndness IEndianess)
{
   Bool mode64 = env->mode64;
   PPCFpOp fpop = Pfp_INVALID;
   IRType  ty = typeOfIRExpr(env->type_env,e);

   vassert(e);
   vassert( ty == Ity_F128 );

   /* read 128-bit IRTemp */
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

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
      case Iop_TruncF128toI64S:
         fpop = Pfp_TRUNCFPQTOISD; goto do_Un_F128;
      case Iop_TruncF128toI32S:
         fpop = Pfp_TRUNCFPQTOISW; goto do_Un_F128;
      case Iop_TruncF128toI64U:
         fpop = Pfp_TRUNCFPQTOIUD; goto do_Un_F128;
      case Iop_TruncF128toI32U:
         fpop = Pfp_TRUNCFPQTOIUW; goto do_Un_F128;

      do_Un_F128: {
         HReg r_dst = newVRegV(env);
         HReg r_src = iselFp128Expr(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_Fp128Unary(fpop, r_dst, r_src));
         return r_dst;
      }

      case Iop_F64toF128: {
         fpop = Pfp_FPDTOQ;
         HReg r_dst = newVRegV(env);
         HReg r_src = iselDblExpr(env, e->Iex.Unop.arg, IEndianess);
         HReg v128tmp = newVRegV(env);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );

         /* value is in 64-bit float reg, need to move to 128-bit vector reg */
         sub_from_sp( env, 16 );
         addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8, r_src, zero_r1));
         addInstr(env, PPCInstr_AvLdSt(True/*load*/, 16, v128tmp, zero_r1));
         add_to_sp( env, 16 );

         addInstr(env, PPCInstr_Fp128Unary(fpop, r_dst, v128tmp));
         return r_dst;
      }

      case Iop_I64StoF128:
         fpop = Pfp_IDSTOQ; goto do_Un_int_F128;
      case Iop_I64UtoF128:
         fpop = Pfp_IDUTOQ; goto do_Un_int_F128;

      do_Un_int_F128: {
         HReg r_dst = newVRegV(env);
         HReg tmp = newVRegV(env);
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
         PPCAMode *am_offhi, *am_offlo;
         HReg r_aligned16;

         /* source is in a 64-bit integer reg, move to 128-bit float reg
          * do this via the stack (easy, convenient, etc).
          */
         sub_from_sp( env, 32 );        // Move SP down

         /* Get a quadword aligned address within our stack space */
         r_aligned16 = get_sp_aligned16( env );

         am_offlo  = PPCAMode_IR( 0,  r_aligned16 );
         am_offhi  = PPCAMode_IR( 8,  r_aligned16 );

         /* Inst only uses the upper 64-bit of the source */
         addInstr(env, PPCInstr_Load(8, r_src, am_offhi, mode64));

         /* Fetch result back from stack. */
         addInstr(env, PPCInstr_AvLdSt(True/*load*/, 16, tmp, am_offlo));

         add_to_sp( env, 32 );          // Reset SP

         addInstr(env, PPCInstr_Fp128Unary(fpop, r_dst, tmp));
         return r_dst;
      }

      default:
         break;
      } /* switch (e->Iex.Unop.op) */
   } /* if (e->tag == Iex_Unop) */

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {

      case Iop_F64HLtoF128:
         {
            HReg dst    = newVRegV(env);
            HReg r_src_hi = iselDblExpr(env, e->Iex.Binop.arg1, IEndianess);
            HReg r_src_lo = iselDblExpr(env, e->Iex.Binop.arg2, IEndianess);
            PPCAMode *am_offhi, *am_offlo;
            HReg r_aligned16;

            /* do this via the stack (easy, convenient, etc) */
            sub_from_sp( env, 16 );        // Move SP down

            /* Get a quadword aligned address within our stack space */
            r_aligned16 = get_sp_aligned16( env );

            am_offlo  = PPCAMode_IR( 0,  r_aligned16 );
            am_offhi  = PPCAMode_IR( 8,  r_aligned16 );

            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8,
                                          r_src_lo, am_offlo));
            addInstr(env, PPCInstr_FpLdSt(False/*store*/, 8,
                                          r_src_hi, am_offhi));

            /* Fetch result back from stack. */
            addInstr(env, PPCInstr_AvLdSt(True/*load*/, 16,
                                          dst, am_offlo));

            add_to_sp( env, 16 );          // Reset SP
            return dst;
         }
      case Iop_F128toI128S:
         {
            HReg dst    = newVRegV(env);
            HReg r_src  = iselFp128Expr(env, e->Iex.Binop.arg2, IEndianess);
            PPCRI* rm = iselWordExpr_RI(env, e->Iex.Binop.arg1, IEndianess);
            /* Note: rm is a set of three bit fields that specify the
             * rounding mode and which of the two instructions to issue.
             */
            addInstr(env, PPCInstr_AvBinaryInt(Pav_F128toI128S, dst,
                                               r_src, rm));
            return dst;
         }
      case Iop_RndF128:
         {
            HReg dst    = newVRegV(env);
            HReg r_src  = iselFp128Expr(env, e->Iex.Binop.arg2, IEndianess);
            PPCRI* rm = iselWordExpr_RI(env, e->Iex.Binop.arg1, IEndianess);
            /* Note: rm is a set of three bit fields that specify the
             * rounding mode and which of the two instructions to issue.
             */
            addInstr(env, PPCInstr_AvBinaryInt(Pav_ROUNDFPQ, dst,
                                               r_src, rm));
            return dst;
         }
      case Iop_SqrtF128:
         if (FPU_rounding_mode_isOdd(e->Iex.Binop.arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPSQRTQRNDODD;
            goto do_Bin_F128;
         } else {
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
            fpop = Pfp_FPSQRTQ;
            goto do_Bin_F128;
         }
      case Iop_F128toF32:
         if (FPU_rounding_mode_isOdd(e->Iex.Binop.arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPQTOWRNDODD;
            goto do_Bin_F128;
         } else {
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
            fpop = Pfp_FPQTOW;
            goto do_Bin_F128;
         }
      do_Bin_F128: {
         HReg r_dst = newVRegV(env);
         HReg r_src = iselFp128Expr(env, e->Iex.Binop.arg2, IEndianess);
         addInstr(env, PPCInstr_Fp128Unary(fpop, r_dst, r_src));
         return r_dst;
      }

      default:
         break;
      } /* switch (e->Iex.Binop.op) */
   } /* if (e->tag == Iex_Binop) */

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;

      switch (triop->op) {
      case Iop_AddF128:
         if (FPU_rounding_mode_isOdd(triop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPADDQRNDODD; goto do_Tri_F128;
         } else {
            set_FPU_rounding_mode( env, triop->arg1, IEndianess );
            fpop = Pfp_FPADDQ; goto do_Tri_F128;
         }
      case Iop_SubF128:
         if (FPU_rounding_mode_isOdd(triop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPSUBQRNDODD; goto do_Tri_F128;
         } else {
            set_FPU_rounding_mode( env, triop->arg1, IEndianess );
            fpop = Pfp_FPSUBQ; goto do_Tri_F128;
         }
      case Iop_MulF128:
         if (FPU_rounding_mode_isOdd(triop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPMULQRNDODD; goto do_Tri_F128;
         } else {
            set_FPU_rounding_mode( env, triop->arg1, IEndianess );
            fpop = Pfp_FPMULQ; goto do_Tri_F128;
         }
      case Iop_DivF128:
         if (FPU_rounding_mode_isOdd(triop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPDIVQRNDODD; goto do_Tri_F128;
         } else {
            set_FPU_rounding_mode( env, triop->arg1, IEndianess );
            fpop = Pfp_FPDIVQ; goto do_Tri_F128;
         }
      case Iop_MAddF128:
         if (FPU_rounding_mode_isOdd(triop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPMULADDQRNDODD; goto do_Tri_F128;
         } else {
            set_FPU_rounding_mode( env, triop->arg1, IEndianess );
            fpop = Pfp_FPMULADDQ; goto do_Tri_F128;
         }

   do_Tri_F128: {
         HReg r_dst  = newVRegV(env);
         HReg r_srcL = iselFp128Expr(env, triop->arg2, IEndianess);
         HReg r_srcR = iselFp128Expr(env, triop->arg3, IEndianess);

         addInstr(env, PPCInstr_Fp128Binary(fpop, r_dst, r_srcL, r_srcR));
         return r_dst;
      }

      default:
         break;
      } /* switch (e->Iex.Triop.op) */

   } /* if (e->tag == Iex_Trinop) */

   if (e->tag == Iex_Qop) {
      IRQop *qop = e->Iex.Qop.details;

      switch (qop->op) {
      case Iop_MAddF128:
         if (FPU_rounding_mode_isOdd(qop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPMULADDQRNDODD; goto do_Quad_F128;
         } else {
            set_FPU_rounding_mode( env, qop->arg1, IEndianess );
            fpop = Pfp_FPMULADDQ; goto do_Quad_F128;
         }
      case Iop_MSubF128:
         if (FPU_rounding_mode_isOdd(qop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPMULSUBQRNDODD; goto do_Quad_F128;
         } else {
            set_FPU_rounding_mode( env, qop->arg1, IEndianess );
            fpop = Pfp_FPMULSUBQ; goto do_Quad_F128;
         }
      case Iop_NegMAddF128:
         if (FPU_rounding_mode_isOdd(qop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPNEGMULADDQRNDODD; goto do_Quad_F128;
         } else {
            set_FPU_rounding_mode( env, qop->arg1, IEndianess );
            fpop = Pfp_FPNEGMULADDQ; goto do_Quad_F128;
         }
      case Iop_NegMSubF128:
         if (FPU_rounding_mode_isOdd(qop->arg1)) {
            /* use rounding mode specified by RN. Issue inst with R0 = 0 */
            fpop = Pfp_FPNEGMULSUBQRNDODD; goto do_Quad_F128;
         } else {
            set_FPU_rounding_mode( env, qop->arg1, IEndianess );
            fpop = Pfp_FPNEGMULSUBQ; goto do_Quad_F128;
         }

      do_Quad_F128: {
         HReg r_dst = iselFp128Expr(env, qop->arg3,
                                    IEndianess);
         HReg r_srcL = iselFp128Expr(env, qop->arg2,
                                     IEndianess);
         HReg r_srcR = iselFp128Expr(env, qop->arg4,
                                     IEndianess);

         addInstr(env, PPCInstr_Fp128Trinary(fpop, r_dst, r_srcL, r_srcR));
         return r_dst;
         }

      default:
         break;
      }
   }   /* if (e->tag == Iex_Qop) */

   ppIRExpr( e );
   vpanic( "iselFp128Expr(ppc64)" );
}

static HReg iselDfp64Expr(ISelEnv* env, const IRExpr* e, IREndness IEndianess)
{
   HReg r = iselDfp64Expr_wrk( env, e, IEndianess );
   vassert(hregClass(r) == HRcFlt64);
   vassert( hregIsVirtual(r) );
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselDfp64Expr_wrk(ISelEnv* env, const IRExpr* e,
                              IREndness IEndianess)
{
   Bool mode64 = env->mode64;
   IRType ty = typeOfIRExpr( env->type_env, e );
   HReg r_dstHi, r_dstLo;

   vassert( e );
   vassert( ty == Ity_D64 );

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp( env, e->Iex.RdTmp.tmp );
   }

   /* --------- GET --------- */
   if (e->tag == Iex_Get) {
      HReg r_dst = newVRegF( env );
      PPCAMode* am_addr = PPCAMode_IR( e->Iex.Get.offset,
                                       GuestStatePtr(mode64) );
      addInstr( env, PPCInstr_FpLdSt( True/*load*/, 8, r_dst, am_addr ) );
      return r_dst;
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == IEndianess) {
      PPCAMode* am_addr;
      HReg r_dst = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_D64);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, Ity_D64/*xfer*/,
                                   IEndianess);
      addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, r_dst, am_addr));
      return r_dst;
   }

   /* --------- OPS --------- */
   if (e->tag == Iex_Qop) {
      HReg r_dst = newVRegF( env );
      return r_dst;
   }

   if (e->tag == Iex_Unop) {
      HReg fr_dst = newVRegF(env);
      switch (e->Iex.Unop.op) {
      case Iop_ReinterpI64asD64: {
         /* Given an I64, produce an IEEE754 DFP with the same
               bit pattern. */
         if (!mode64) {
            HReg r_srcHi, r_srcLo;
            iselInt64Expr( &r_srcHi, &r_srcLo, env, e->Iex.Unop.arg,
                           IEndianess);
            return mk_LoadRR32toFPR( env, r_srcHi, r_srcLo );
         } else {
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
            return mk_LoadR64toFPR( env, r_src );
         }
      }
      case Iop_D32toD64: {
         HReg fr_src = iselDfp32Expr(env, e->Iex.Unop.arg, IEndianess);
         addInstr(env, PPCInstr_Dfp64Unary(Pfp_DCTDP, fr_dst, fr_src));
         return fr_dst;
      }
      case Iop_D128HItoD64:
         iselDfp128Expr( &r_dstHi, &r_dstLo, env, e->Iex.Unop.arg,
                         IEndianess );
         return r_dstHi;
      case Iop_D128LOtoD64:
         iselDfp128Expr( &r_dstHi, &r_dstLo, env, e->Iex.Unop.arg,
                         IEndianess );
         return r_dstLo;
      case Iop_InsertExpD64: {
         HReg fr_srcL = iselDblExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg fr_srcR = iselDblExpr(env, e->Iex.Binop.arg2, IEndianess);

         addInstr(env, PPCInstr_Dfp64Binary(Pfp_DIEX, fr_dst, fr_srcL,
					    fr_srcR));
         return fr_dst;
       }
      default:
         vex_printf( "ERROR: iselDfp64Expr_wrk, UNKNOWN unop case %d\n",
                     (Int)e->Iex.Unop.op );
      }
   }

   if (e->tag == Iex_Binop) {
      PPCFpOp fpop = Pfp_INVALID;
      HReg fr_dst = newVRegF(env);

      switch (e->Iex.Binop.op) {
      case Iop_D128toD64:     fpop = Pfp_DRDPQ;  break;
      case Iop_D64toD32:      fpop = Pfp_DRSP;   break;
      case Iop_I64StoD64:     fpop = Pfp_DCFFIX; break;
      case Iop_RoundD64toInt: fpop = Pfp_DRINTN; break;
      default: break;
      }
      if (fpop == Pfp_DRDPQ) {
         HReg r_srcHi = newVRegF(env);
         HReg r_srcLo = newVRegF(env);

         set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg2,
                        IEndianess);
         addInstr(env, PPCInstr_DfpD128toD64(fpop, fr_dst, r_srcHi, r_srcLo));
         return fr_dst;

      } else if (fpop == Pfp_DRINTN) {
         HReg fr_src = newVRegF(env);
         PPCRI* r_rmc = iselWordExpr_RI(env, e->Iex.Binop.arg1, IEndianess);

         /* NOTE, this IOP takes a DFP value and rounds to the
          * neares floating point integer value, i.e. fractional part
          * is zero.  The result is a decimal floating point number.
          * the INT in the name is a bit misleading.
          */
         fr_src = iselDfp64Expr(env, e->Iex.Binop.arg2, IEndianess);
         addInstr(env, PPCInstr_DfpRound(fr_dst, fr_src, r_rmc));
         return fr_dst;

      } else if (fpop == Pfp_DRSP) {
         HReg fr_src = iselDfp64Expr(env, e->Iex.Binop.arg2, IEndianess);
         set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         addInstr(env, PPCInstr_Dfp64Unary(fpop, fr_dst, fr_src));
         return fr_dst;

      } else if (fpop == Pfp_DCFFIX) {
         HReg fr_src = newVRegF(env);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );

         set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         sub_from_sp( env, 16 );

         // put the I64 value into a floating point register
         if (mode64) {
           HReg tmp = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);

           addInstr(env, PPCInstr_Store(8, zero_r1, tmp, True/*mode64*/));
         } else {
            HReg tmpHi, tmpLo;
            PPCAMode* four_r1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );

            iselInt64Expr(&tmpHi, &tmpLo, env, e->Iex.Binop.arg2,
                          IEndianess);
            addInstr(env, PPCInstr_Store(4, zero_r1, tmpHi, False/*mode32*/));
            addInstr(env, PPCInstr_Store(4, four_r1, tmpLo, False/*mode32*/));
         }

         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8,  fr_src, zero_r1));
         addInstr(env, PPCInstr_Dfp64Unary(fpop, fr_dst, fr_src));
         add_to_sp( env, 16 );
         return fr_dst;
      }

      switch (e->Iex.Binop.op) {
      /* shift instructions D64, I32 -> D64 */
      case Iop_ShlD64: fpop = Pfp_DSCLI; break;
      case Iop_ShrD64: fpop = Pfp_DSCRI; break;
      default: break;
      }
      if (fpop != Pfp_INVALID) {
         HReg fr_src = iselDfp64Expr(env, e->Iex.Binop.arg1, IEndianess);
         PPCRI* shift = iselWordExpr_RI(env, e->Iex.Binop.arg2, IEndianess);

         /* shift value must be an immediate value */
         vassert(shift->tag == Pri_Imm);

         addInstr(env, PPCInstr_DfpShift(fpop, fr_dst, fr_src, shift));
         return fr_dst;
      }

      switch (e->Iex.Binop.op) {
      case Iop_InsertExpD64:
         fpop = Pfp_DIEX;
         break;
      default: 	break;
      }
      if (fpop != Pfp_INVALID) {
         HReg fr_srcL = newVRegF(env);
         HReg fr_srcR = iselDfp64Expr(env, e->Iex.Binop.arg2, IEndianess);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
         sub_from_sp( env, 16 );

         if (env->mode64) {
            // put the I64 value into a floating point reg
            HReg tmp = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);

            addInstr(env, PPCInstr_Store(8, zero_r1, tmp, True/*mode64*/));
         } else {
            // put the I64 register pair into a floating point reg
            HReg tmpHi;
            HReg tmpLo;
            PPCAMode* four_r1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );

            iselInt64Expr(&tmpHi, &tmpLo, env, e->Iex.Binop.arg1,
                          IEndianess);
            addInstr(env, PPCInstr_Store(4, zero_r1, tmpHi, False/*!mode64*/));
            addInstr(env, PPCInstr_Store(4, four_r1, tmpLo, False/*!mode64*/));
         }
         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fr_srcL, zero_r1));
         addInstr(env, PPCInstr_Dfp64Binary(fpop, fr_dst, fr_srcL,
                                            fr_srcR));
         add_to_sp( env, 16 );
         return fr_dst;
      }
   }

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;
      PPCFpOp fpop = Pfp_INVALID;

      switch (triop->op) {
      case Iop_AddD64:
         fpop = Pfp_DFPADD;
         break;
      case Iop_SubD64:
         fpop = Pfp_DFPSUB;
         break;
      case Iop_MulD64:
         fpop = Pfp_DFPMUL;
         break;
      case Iop_DivD64:
         fpop = Pfp_DFPDIV;
         break;
      default:
         break;
      }
      if (fpop != Pfp_INVALID) {
         HReg r_dst = newVRegF( env );
         HReg r_srcL = iselDfp64Expr( env, triop->arg2, IEndianess );
         HReg r_srcR = iselDfp64Expr( env, triop->arg3, IEndianess );

         set_FPU_DFP_rounding_mode( env, triop->arg1, IEndianess );
         addInstr( env, PPCInstr_Dfp64Binary( fpop, r_dst, r_srcL, r_srcR ) );
         return r_dst;
      }

      switch (triop->op) {
      case Iop_QuantizeD64:          fpop = Pfp_DQUA;  break;
      case Iop_SignificanceRoundD64: fpop = Pfp_RRDTR; break;
      default: break;
      }
      if (fpop == Pfp_DQUA) {
         HReg r_dst = newVRegF(env);
         HReg r_srcL = iselDfp64Expr(env, triop->arg2, IEndianess);
         HReg r_srcR = iselDfp64Expr(env, triop->arg3, IEndianess);
         PPCRI* rmc  = iselWordExpr_RI(env, triop->arg1, IEndianess);
         addInstr(env, PPCInstr_DfpQuantize(fpop, r_dst, r_srcL, r_srcR,
                                            rmc));
         return r_dst;

      } else if (fpop == Pfp_RRDTR) {
         HReg r_dst = newVRegF(env);
         HReg r_srcL = newVRegF(env);
         HReg r_srcR = iselDfp64Expr(env, triop->arg3, IEndianess);
         PPCRI* rmc  = iselWordExpr_RI(env, triop->arg1, IEndianess);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
         HReg i8_val = iselWordExpr_R(env, triop->arg2, IEndianess);

         /* Move I8 to float register to issue instruction */
         sub_from_sp( env, 16 );
         if (mode64)
            addInstr(env, PPCInstr_Store(8, zero_r1, i8_val, True/*mode64*/));
         else
            addInstr(env, PPCInstr_Store(4, zero_r1, i8_val, False/*mode32*/));

         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, r_srcL, zero_r1));
         add_to_sp( env, 16 );

         // will set TE and RMC when issuing instruction
         addInstr(env, PPCInstr_DfpQuantize(fpop, r_dst, r_srcL, r_srcR, rmc));
         return r_dst;
      }
   }

   ppIRExpr( e );
   vpanic( "iselDfp64Expr_wrk(ppc)" );
}

static void iselDfp128Expr(HReg* rHi, HReg* rLo, ISelEnv* env, const IRExpr* e,
                           IREndness IEndianess)
{
   iselDfp128Expr_wrk( rHi, rLo, env, e, IEndianess );
   vassert( hregIsVirtual(*rHi) );
   vassert( hregIsVirtual(*rLo) );
}

/* DO NOT CALL THIS DIRECTLY */
static void iselDfp128Expr_wrk(HReg* rHi, HReg *rLo, ISelEnv* env,
                               const IRExpr* e, IREndness IEndianess)
{
   vassert( e );
   vassert( typeOfIRExpr(env->type_env,e) == Ity_D128 );

   /* read 128-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair( rHi, rLo, env, e->Iex.RdTmp.tmp );
      return;
   }

   if (e->tag == Iex_Unop) {
      HReg r_dstHi = newVRegF(env);
      HReg r_dstLo = newVRegF(env);

      if (e->Iex.Unop.op == Iop_I64StoD128) {
         HReg fr_src = newVRegF(env);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );

         // put the I64 value into a floating point reg
         if (env->mode64) {
            HReg tmp   = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
            addInstr(env, PPCInstr_Store(8, zero_r1, tmp, True/*mode64*/));
         } else {
            HReg tmpHi, tmpLo;
            PPCAMode* four_r1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );

            iselInt64Expr(&tmpHi, &tmpLo, env, e->Iex.Unop.arg,
                          IEndianess);
            addInstr(env, PPCInstr_Store(4, zero_r1, tmpHi, False/*mode32*/));
            addInstr(env, PPCInstr_Store(4, four_r1, tmpLo, False/*mode32*/));
         }

         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, fr_src, zero_r1));
         addInstr(env, PPCInstr_DfpI64StoD128(Pfp_DCFFIXQ, r_dstHi, r_dstLo,
                                              fr_src));
      }

      if (e->Iex.Unop.op == Iop_D64toD128) {
         HReg r_src = iselDfp64Expr(env, e->Iex.Unop.arg, IEndianess);

         /* Source is 64bit, result is 128 bit.  High 64bit source arg,
          * is ignored by the instruction.  Set high arg to r_src just
          * to meet the vassert tests.
          */
         addInstr(env, PPCInstr_Dfp128Unary(Pfp_DCTQPQ, r_dstHi, r_dstLo,
                                            r_src, r_src));
      }
      *rHi = r_dstHi;
      *rLo = r_dstLo;
      return;
   }

   /* --------- OPS --------- */
   if (e->tag == Iex_Binop) {
      HReg r_srcHi;
      HReg r_srcLo;

      switch (e->Iex.Binop.op) {
      case Iop_D64HLtoD128:
         r_srcHi = iselDfp64Expr( env, e->Iex.Binop.arg1, IEndianess );
         r_srcLo = iselDfp64Expr( env, e->Iex.Binop.arg2, IEndianess );
         *rHi = r_srcHi;
         *rLo = r_srcLo;
         return;
         break;
      case Iop_D128toD64: {
         PPCFpOp fpop = Pfp_DRDPQ;
         HReg fr_dst  = newVRegF(env);

         set_FPU_DFP_rounding_mode( env, e->Iex.Binop.arg1, IEndianess );
         iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg2,
                        IEndianess);
         addInstr(env, PPCInstr_DfpD128toD64(fpop, fr_dst, r_srcHi, r_srcLo));

         /* Need to meet the interface spec but the result is
          * just 64-bits so send the result back in both halfs.
          */
         *rHi = fr_dst;
         *rLo = fr_dst;
         return;
      }
      case Iop_ShlD128: 
      case Iop_ShrD128: {
         HReg fr_dst_hi = newVRegF(env);  
         HReg fr_dst_lo = newVRegF(env);
         PPCRI* shift = iselWordExpr_RI(env, e->Iex.Binop.arg2, IEndianess);
         PPCFpOp fpop = Pfp_DSCLIQ;  /* fix later if necessary */

         iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg1,
                        IEndianess);

         if (e->Iex.Binop.op == Iop_ShrD128)
            fpop = Pfp_DSCRIQ;

         addInstr(env, PPCInstr_DfpShift128(fpop, fr_dst_hi, fr_dst_lo,
                                            r_srcHi, r_srcLo, shift));

         *rHi = fr_dst_hi;
         *rLo = fr_dst_lo;
         return;
      }
      case Iop_RoundD128toInt: {
         HReg r_dstHi = newVRegF(env);
         HReg r_dstLo = newVRegF(env);
         PPCRI* r_rmc = iselWordExpr_RI(env, e->Iex.Binop.arg1, IEndianess);

         // will set R and RMC when issuing instruction
         iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg2,
                        IEndianess);

         addInstr(env, PPCInstr_DfpRound128(r_dstHi, r_dstLo,
                                            r_srcHi, r_srcLo, r_rmc));
         *rHi = r_dstHi;
         *rLo = r_dstLo;
         return;
      }
      case Iop_InsertExpD128: {
         HReg r_dstHi = newVRegF(env);
         HReg r_dstLo = newVRegF(env);
         HReg r_srcL  = newVRegF(env);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
         r_srcHi = newVRegF(env);
         r_srcLo = newVRegF(env);

         iselDfp128Expr(&r_srcHi, &r_srcLo, env, e->Iex.Binop.arg2,
                        IEndianess);

         /* Move I64 to float register to issue instruction */
         if (env->mode64) {
            HReg tmp = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
            addInstr(env, PPCInstr_Store(8, zero_r1, tmp, True/*mode64*/));
         } else {
            HReg tmpHi, tmpLo;
            PPCAMode* four_r1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );

            iselInt64Expr(&tmpHi, &tmpLo, env, e->Iex.Unop.arg,
                          IEndianess);
            addInstr(env, PPCInstr_Store(4, zero_r1, tmpHi, False/*mode32*/));
            addInstr(env, PPCInstr_Store(4, four_r1, tmpLo, False/*mode32*/));
         }

         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, r_srcL, zero_r1));
         addInstr(env, PPCInstr_InsertExpD128(Pfp_DIEXQ,
                                              r_dstHi, r_dstLo,
                                              r_srcL, r_srcHi, r_srcLo));
         *rHi = r_dstHi;
         *rLo = r_dstLo;
         return;
      }
      default:
         vex_printf( "ERROR: iselDfp128Expr_wrk, UNKNOWN binop case %d\n",
                     (Int)e->Iex.Binop.op );
         break;
      }
   }

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;
      PPCFpOp fpop = Pfp_INVALID;
      HReg r_dstHi = newVRegF(env);
      HReg r_dstLo = newVRegF(env);

      switch (triop->op) {
      case Iop_AddD128:
         fpop = Pfp_DFPADDQ;
         break;
      case Iop_SubD128:
         fpop = Pfp_DFPSUBQ;
         break;
      case Iop_MulD128:
         fpop = Pfp_DFPMULQ;
         break;
      case Iop_DivD128:
         fpop = Pfp_DFPDIVQ;
         break;
      default:
         break;
      }

      if (fpop != Pfp_INVALID) {
         HReg r_srcRHi = newVRegV( env );
         HReg r_srcRLo = newVRegV( env );

         /* dst will be used to pass in the left operand and get the result. */
         iselDfp128Expr( &r_dstHi, &r_dstLo, env, triop->arg2, IEndianess );
         iselDfp128Expr( &r_srcRHi, &r_srcRLo, env, triop->arg3, IEndianess );
         set_FPU_DFP_rounding_mode( env, triop->arg1, IEndianess );
         addInstr( env,
                   PPCInstr_Dfp128Binary( fpop, r_dstHi, r_dstLo,
                                          r_srcRHi, r_srcRLo ) );
         *rHi = r_dstHi;
         *rLo = r_dstLo;
         return;
      }
      switch (triop->op) {
      case Iop_QuantizeD128:          fpop = Pfp_DQUAQ;  break;
      case Iop_SignificanceRoundD128: fpop = Pfp_DRRNDQ; break;
      default: break;
      }
      if (fpop == Pfp_DQUAQ) {
         HReg r_srcHi = newVRegF(env);
         HReg r_srcLo = newVRegF(env);
         PPCRI* rmc = iselWordExpr_RI(env, triop->arg1, IEndianess);

         /* dst will be used to pass in the left operand and get the result */
         iselDfp128Expr(&r_dstHi, &r_dstLo, env, triop->arg2, IEndianess);
         iselDfp128Expr(&r_srcHi, &r_srcLo, env, triop->arg3, IEndianess);

         // will set RMC when issuing instruction
         addInstr(env, PPCInstr_DfpQuantize128(fpop, r_dstHi, r_dstLo,
                                               r_srcHi, r_srcLo, rmc));
        *rHi = r_dstHi;
        *rLo = r_dstLo;
         return;

      } else if (fpop == Pfp_DRRNDQ) {
         HReg r_srcHi = newVRegF(env);
         HReg r_srcLo = newVRegF(env);
         PPCRI* rmc = iselWordExpr_RI(env, triop->arg1, IEndianess);
         PPCAMode* zero_r1 = PPCAMode_IR( 0, StackFramePtr(env->mode64) );
         PPCAMode* four_r1 = PPCAMode_IR( 4, StackFramePtr(env->mode64) );
         HReg i8_val = iselWordExpr_R(env, triop->arg2, IEndianess);
         HReg r_zero = newVRegI( env );

         iselDfp128Expr(&r_srcHi, &r_srcLo, env, triop->arg3, IEndianess);

         /* dst will be used to pass in the left operand and get the result */
         /* Move I8 to float register to issue instruction.  Note, the
          * instruction only looks at the bottom 6 bits so we really don't
          * have to clear the upper bits since the iselWordExpr_R sets the
          * bottom 8-bits.
          */
         sub_from_sp( env, 16 );

         if (env->mode64)
            addInstr(env, PPCInstr_Store(4, four_r1, i8_val, True/*mode64*/));
         else
            addInstr(env, PPCInstr_Store(4, four_r1, i8_val, False/*mode32*/));

         /* Have to write to the upper bits to ensure they have been
          * initialized. The instruction ignores all but the lower 6-bits.
          */
         addInstr( env, PPCInstr_LI( r_zero, 0, env->mode64 ) );
         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, r_dstHi, zero_r1));
         addInstr(env, PPCInstr_FpLdSt(True/*load*/, 8, r_dstLo, zero_r1));

         add_to_sp( env, 16 );

         // will set RMC when issuing instruction
         addInstr(env, PPCInstr_DfpQuantize128(fpop, r_dstHi, r_dstLo,
                                               r_srcHi, r_srcLo, rmc));
         *rHi = r_dstHi;
         *rLo = r_dstLo;
         return;
      }
 }

   ppIRExpr( e );
   vpanic( "iselDfp128Expr(ppc64)" );
}


/*---------------------------------------------------------*/
/*--- ISEL: SIMD (Vector) expressions, 128 bit.         ---*/
/*---------------------------------------------------------*/

static HReg iselVecExpr ( ISelEnv* env, const IRExpr* e, IREndness IEndianess )
{
   HReg r = iselVecExpr_wrk( env, e, IEndianess );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcVec128);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselVecExpr_wrk ( ISelEnv* env, const IRExpr* e,
                              IREndness IEndianess )
{
   Bool mode64 = env->mode64;
   PPCAvOp op = Pav_INVALID;
   PPCAvFpOp fpop = Pavfp_INVALID;
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

   if (e->tag == Iex_Load && e->Iex.Load.end == IEndianess) {
      /* Need to be able to do V128 unaligned loads. The BE unaligned load
       * can be accomplised using the following code sequece from the ISA.
       * It uses the lvx instruction that does two aligned loads and then
       * permute the data to store the required data as if it had been an
       * unaligned load.
       *
       *   lvx  Vhi,0,Rb        # load MSQ, using the unaligned address in Rb
       *   lvsl Vp, 0,Rb        # Set permute control vector
       *   addi Rb,Rb,15        # Address of LSQ
       *   lvx  Vlo,0,Rb        # load LSQ
       *   vperm Vt,Vhi,Vlo,Vp  # align the data as requested
       */

      HReg Vhi   = newVRegV(env);
      HReg Vlo   = newVRegV(env);
      HReg Vp    = newVRegV(env);
      HReg v_dst = newVRegV(env);
      HReg rB;
      HReg rB_plus_15 = newVRegI(env);

      vassert(e->Iex.Load.ty == Ity_V128);
      rB = iselWordExpr_R( env, e->Iex.Load.addr, IEndianess );

      // lvx  Vhi, 0, Rb
      addInstr(env, PPCInstr_AvLdSt( True/*load*/, 16, Vhi,
                                     PPCAMode_IR(0, rB)) );

      if (IEndianess == Iend_LE)
         // lvsr Vp, 0, Rb
         addInstr(env, PPCInstr_AvSh( False/*right shift*/, Vp,
                                      PPCAMode_IR(0, rB)) );
      else
         // lvsl Vp, 0, Rb
         addInstr(env, PPCInstr_AvSh( True/*left shift*/, Vp,
                                      PPCAMode_IR(0, rB)) );

      // addi Rb_plus_15, Rb, 15
      addInstr(env, PPCInstr_Alu( Palu_ADD, rB_plus_15,
                                  rB, PPCRH_Imm(True, toUShort(15))) );

      // lvx  Vlo, 0, Rb_plus_15
      addInstr(env, PPCInstr_AvLdSt( True/*load*/, 16, Vlo,
                                     PPCAMode_IR(0, rB_plus_15)) );

      if (IEndianess == Iend_LE)
         // vperm Vt, Vhi, Vlo, Vp
         addInstr(env, PPCInstr_AvPerm( v_dst, Vlo, Vhi, Vp ));
      else
         // vperm Vt, Vhi, Vlo, Vp
         addInstr(env, PPCInstr_AvPerm( v_dst, Vhi, Vlo, Vp ));

      return v_dst;
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {

      case Iop_F16toF64x2:
         {
            HReg dst = newVRegV(env);
            HReg arg  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
            /* Note: PPC only coverts the 16-bt value in the upper word
             *       to a 64-bit value stored in the upper word.  The
             *       contents of the lower word is undefined.
             */
            addInstr(env, PPCInstr_AvUnary(Pav_F16toF64x2, dst, arg));
            return dst;
         }

      case Iop_F64toF16x2_DEP:
         {
            HReg dst = newVRegV(env);
            HReg arg  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
            /* Note: PPC only coverts the 64-bt value in the upper 64-bit of V128
             * to a 16-bit value stored in the upper 64-bits of the result
             * V128.  The contents of the lower 64-bits is undefined.
             */
            addInstr(env, PPCInstr_AvUnary(Pav_F64toF16x2, dst, arg));
            return dst;
         }

      case Iop_F16toF32x4:
         {
            HReg src = newVRegV(env);
            HReg dst = newVRegV(env);
            HReg arg = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
            PPCAMode *am_off0, *am_off8;
            HReg r_aligned16;

            vassert(mode64);
            /* need to put I64 src into upper 64-bits of vector register,
               use stack */
            sub_from_sp( env, 32 );     // Move SP down

            /* Get a quadword aligned address within our stack space */
            r_aligned16 = get_sp_aligned16( env );
            am_off0  = PPCAMode_IR( 0, r_aligned16 );
            am_off8  = PPCAMode_IR( 8, r_aligned16 );

            /* Store I64 to stack */

            if (IEndianess == Iend_LE) {
               addInstr(env, PPCInstr_Store( 8, am_off8, arg, mode64 ));
            } else {
               addInstr(env, PPCInstr_Store( 8, am_off0, arg, mode64 ));
            }

            /* Fetch new v128 src back from stack. */
            addInstr(env, PPCInstr_AvLdSt(True/*ld*/, 16, src, am_off0));

            /* issue instruction */
            addInstr(env, PPCInstr_AvUnary(Pav_F16toF32x4, dst, src));
            add_to_sp( env, 32 );          // Reset SP

            return dst;
         }

      case Iop_F32toF16x4_DEP:
         {
            HReg dst = newVRegI(env);
            HReg tmp = newVRegV(env);
            HReg arg  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
            PPCAMode *am_off0, *am_off8;
            HReg r_aligned16;

            /* Instruction returns a V128, the Iop_F32toF16x4 needs to return
             * I64.  Move the upper 64-bits from the instruction to an I64 via
             * the stack and return it.
             */
            sub_from_sp( env, 32 );     // Move SP down

            addInstr(env, PPCInstr_AvUnary(Pav_F32toF16x4, tmp, arg));

            /* Get a quadword aligned address within our stack space */
            r_aligned16 = get_sp_aligned16( env );
            am_off0  = PPCAMode_IR( 0, r_aligned16 );
            am_off8  = PPCAMode_IR( 8, r_aligned16 );

            /* Store v128 tmp to stack. */
            addInstr(env, PPCInstr_AvLdSt(False/*store*/, 16, tmp, am_off0));

            /* Fetch I64 from stack */
            if (IEndianess == Iend_LE) {
               addInstr(env, PPCInstr_Load( 8, dst, am_off8, mode64 ));
            } else {
               addInstr(env, PPCInstr_Load( 8, dst, am_off0, mode64 ));
            }

            add_to_sp( env, 32 );          // Reset SP
            return dst;
         }

      case Iop_NotV128: {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         HReg dst = newVRegV(env);
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, arg));
         return dst;
      }

      case Iop_CmpNEZ8x16: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPCInstr_AvBin8x16(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_CmpNEZ16x8: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPCInstr_AvBin16x8(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_CmpNEZ32x4: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPCInstr_AvBin32x4(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_CmpNEZ64x2: {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         HReg zero = newVRegV(env);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBinary(Pav_XOR, zero, zero, zero));
         addInstr(env, PPCInstr_AvBin64x2(Pav_CMPEQU, dst, arg, zero));
         addInstr(env, PPCInstr_AvUnary(Pav_NOT, dst, dst));
         return dst;
      }

      case Iop_RecipEst32Fx4: fpop = Pavfp_RCPF;    goto do_32Fx4_unary;
      case Iop_RSqrtEst32Fx4: fpop = Pavfp_RSQRTF;  goto do_32Fx4_unary;
      case Iop_Log2_32Fx4:    fpop = Pavfp_Log2;    goto do_32Fx4_unary;
      case Iop_Exp2_32Fx4:    fpop = Pavfp_Exp2;    goto do_32Fx4_unary;
      case Iop_I32UtoF32x4_DEP: fpop = Pavfp_CVTU2F;  goto do_32Fx4_unary;
      case Iop_I32StoF32x4_DEP: fpop = Pavfp_CVTS2F;  goto do_32Fx4_unary;
      case Iop_QF32toI32Ux4_RZ: fpop = Pavfp_QCVTF2U; goto do_32Fx4_unary;
      case Iop_QF32toI32Sx4_RZ: fpop = Pavfp_QCVTF2S; goto do_32Fx4_unary;
      case Iop_RoundF32x4_RM: fpop = Pavfp_ROUNDM;  goto do_32Fx4_unary;
      case Iop_RoundF32x4_RP: fpop = Pavfp_ROUNDP;  goto do_32Fx4_unary;
      case Iop_RoundF32x4_RN: fpop = Pavfp_ROUNDN;  goto do_32Fx4_unary;
      case Iop_RoundF32x4_RZ: fpop = Pavfp_ROUNDZ;  goto do_32Fx4_unary;
      do_32Fx4_unary:
      {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         HReg dst = newVRegV(env);
         addInstr(env, PPCInstr_AvUn32Fx4(fpop, dst, arg));
         return dst;
      }

      case Iop_32UtoV128: {
         HReg r_aligned16, r_zeros;
         HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg, IEndianess);
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
         if (IEndianess == Iend_LE)
            addInstr(env, PPCInstr_Store( 4, am_off0, r_src, mode64 ));
         else
            addInstr(env, PPCInstr_Store( 4, am_off0, r_zeros, mode64 ));
         addInstr(env, PPCInstr_Store( 4, am_off4, r_zeros, mode64 ));
         addInstr(env, PPCInstr_Store( 4, am_off8, r_zeros, mode64 ));

         /* Store r_src in low word of quadword-aligned mem */
         if (IEndianess == Iend_LE)
            addInstr(env, PPCInstr_Store( 4, am_off12, r_zeros, mode64 ));
         else
            addInstr(env, PPCInstr_Store( 4, am_off12, r_src, mode64 ));

         /* Load word into low word of quadword vector reg */
         if (IEndianess == Iend_LE)
            addInstr(env, PPCInstr_AvLdSt( True/*ld*/, 4, dst, am_off0 ));
         else
            addInstr(env, PPCInstr_AvLdSt( True/*ld*/, 4, dst, am_off12 ));

         add_to_sp( env, 32 );       // Reset SP
         return dst;
      }

      case Iop_Dup8x16:
      case Iop_Dup16x8:
      case Iop_Dup32x4:
         return mk_AvDuplicateRI(env, e->Iex.Unop.arg, IEndianess);

      case Iop_CipherSV128: op = Pav_CIPHERSUBV128; goto do_AvCipherV128Un;
      do_AvCipherV128Un: {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
         HReg dst = newVRegV(env);
         addInstr(env, PPCInstr_AvCipherV128Unary(op, dst, arg));
         return dst;
      }

      case Iop_Clz8x16: op = Pav_ZEROCNTBYTE;   goto do_zerocnt;
      case Iop_Clz16x8: op = Pav_ZEROCNTHALF;   goto do_zerocnt;
      case Iop_Clz32x4: op = Pav_ZEROCNTWORD;   goto do_zerocnt;
      case Iop_Clz64x2: op = Pav_ZEROCNTDBL;    goto do_zerocnt;
      case Iop_Ctz8x16: op = Pav_TRAILINGZEROCNTBYTE; goto do_zerocnt;
      case Iop_Ctz16x8: op = Pav_TRAILINGZEROCNTHALF; goto do_zerocnt;
      case Iop_Ctz32x4: op = Pav_TRAILINGZEROCNTWORD; goto do_zerocnt;
      case Iop_Ctz64x2: op = Pav_TRAILINGZEROCNTDBL;  goto do_zerocnt;
      case Iop_PwBitMtxXpose64x2: op = Pav_BITMTXXPOSE;  goto do_zerocnt;
      do_zerocnt:
      {
        HReg arg = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
        HReg dst = newVRegV(env);
        addInstr(env, PPCInstr_AvUnary(op, dst, arg));
        return dst;
      }

      /* BCD Iops */
      case Iop_BCD128toI128S:
         {
            HReg dst  = newVRegV(env);
            HReg arg  = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
            addInstr(env, PPCInstr_AvUnary( Pav_BCD128toI128S, dst, arg ) );
            return dst;
         }

      case Iop_MulI128by10:       op = Pav_MulI128by10;      goto do_MulI128;
      case Iop_MulI128by10Carry:  op = Pav_MulI128by10Carry; goto do_MulI128;
      do_MulI128: {
            HReg dst = newVRegV(env);
            HReg arg = iselVecExpr(env, e->Iex.Unop.arg, IEndianess);
            addInstr(env, PPCInstr_AvUnary(op, dst, arg));
            return dst;
         }

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
            iselInt64Expr(&r1, &r0, env, e->Iex.Binop.arg2, IEndianess);
            addInstr(env, PPCInstr_Store( 4, am_off12, r0, mode64 ));
            addInstr(env, PPCInstr_Store( 4, am_off8,  r1, mode64 ));
            /* Do the more significant 64 bits */
            iselInt64Expr(&r3, &r2, env, e->Iex.Binop.arg1, IEndianess);
            addInstr(env, PPCInstr_Store( 4, am_off4, r2, mode64 ));
            addInstr(env, PPCInstr_Store( 4, am_off0, r3, mode64 ));
            
            /* Fetch result back from stack. */
            addInstr(env, PPCInstr_AvLdSt(True/*ld*/, 16, dst, am_off0));
            
            add_to_sp( env, 32 );          // Reset SP
            return dst;
         } else {
            HReg     rHi = iselWordExpr_R(env, e->Iex.Binop.arg1, IEndianess);
            HReg     rLo = iselWordExpr_R(env, e->Iex.Binop.arg2, IEndianess);
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
            if (IEndianess == Iend_LE) {
               addInstr(env, PPCInstr_Store( 8, am_off0, rLo, mode64 ));
               addInstr(env, PPCInstr_Store( 8, am_off8, rHi, mode64 ));
            } else {
               addInstr(env, PPCInstr_Store( 8, am_off0, rHi, mode64 ));
               addInstr(env, PPCInstr_Store( 8, am_off8, rLo, mode64 ));
            }
            /* Fetch result back from stack. */
            addInstr(env, PPCInstr_AvLdSt(True/*ld*/, 16, dst, am_off0));
            
            add_to_sp( env, 32 );          // Reset SP
            return dst;
         }
      }

      case Iop_Max32Fx4:   fpop = Pavfp_MAXF;   goto do_32Fx4;
      case Iop_Min32Fx4:   fpop = Pavfp_MINF;   goto do_32Fx4;
      case Iop_CmpEQ32Fx4: fpop = Pavfp_CMPEQF; goto do_32Fx4;
      case Iop_CmpGT32Fx4: fpop = Pavfp_CMPGTF; goto do_32Fx4;
      case Iop_CmpGE32Fx4: fpop = Pavfp_CMPGEF; goto do_32Fx4;
      do_32Fx4:
      {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg dst = newVRegV(env);
         addInstr(env, PPCInstr_AvBin32Fx4(fpop, dst, argL, argR));
         return dst;
      }

      case Iop_CmpLE32Fx4: {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg dst = newVRegV(env);
         
         /* stay consistent with native ppc compares:
            if a left/right lane holds a nan, return zeros for that lane
            so: le == NOT(gt OR isNan)
          */
         HReg isNanLR = newVRegV(env);
         HReg isNanL = isNan(env, argL, IEndianess);
         HReg isNanR = isNan(env, argR, IEndianess);
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
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
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
      case Iop_PolynomialMulAdd8x16: op = Pav_POLYMULADD; goto do_AvBin8x16;
      do_AvBin8x16: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBin8x16(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_Shl16x8:    op = Pav_SHL;    goto do_AvBin16x8;
      case Iop_Shr16x8:    op = Pav_SHR;    goto do_AvBin16x8;
      case Iop_Sar16x8:    op = Pav_SAR;    goto do_AvBin16x8;
      case Iop_Rol16x8:    op = Pav_ROTL;   goto do_AvBin16x8;
      case Iop_NarrowBin16to8x16:    op = Pav_PACKUU;  goto do_AvBin16x8;
      case Iop_QNarrowBin16Uto8Ux16: op = Pav_QPACKUU; goto do_AvBin16x8;
      case Iop_QNarrowBin16Sto8Sx16: op = Pav_QPACKSS; goto do_AvBin16x8;
      case Iop_InterleaveHI16x8:  op = Pav_MRGHI;  goto do_AvBin16x8;
      case Iop_InterleaveLO16x8:  op = Pav_MRGLO;  goto do_AvBin16x8;
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
      case Iop_PolynomialMulAdd16x8: op = Pav_POLYMULADD; goto do_AvBin16x8;
      do_AvBin16x8: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBin16x8(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_Shl32x4:    op = Pav_SHL;    goto do_AvBin32x4;
      case Iop_Shr32x4:    op = Pav_SHR;    goto do_AvBin32x4;
      case Iop_Sar32x4:    op = Pav_SAR;    goto do_AvBin32x4;
      case Iop_Rol32x4:    op = Pav_ROTL;   goto do_AvBin32x4;
      case Iop_NarrowBin32to16x8:    op = Pav_PACKUU;  goto do_AvBin32x4;
      case Iop_QNarrowBin32Uto16Ux8: op = Pav_QPACKUU; goto do_AvBin32x4;
      case Iop_QNarrowBin32Sto16Sx8: op = Pav_QPACKSS; goto do_AvBin32x4;
      case Iop_InterleaveHI32x4:  op = Pav_MRGHI;  goto do_AvBin32x4;
      case Iop_InterleaveLO32x4:  op = Pav_MRGLO;  goto do_AvBin32x4;
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
      case Iop_Mul32x4:    op = Pav_MULU;   goto do_AvBin32x4;
      case Iop_MullEven32Ux4: op = Pav_OMULU;  goto do_AvBin32x4;
      case Iop_MullEven32Sx4: op = Pav_OMULS;  goto do_AvBin32x4;
      case Iop_CmpEQ32x4:  op = Pav_CMPEQU; goto do_AvBin32x4;
      case Iop_CmpGT32Ux4: op = Pav_CMPGTU; goto do_AvBin32x4;
      case Iop_CmpGT32Sx4: op = Pav_CMPGTS; goto do_AvBin32x4;
      case Iop_CatOddLanes32x4:  op = Pav_CATODD;  goto do_AvBin32x4;
      case Iop_CatEvenLanes32x4: op = Pav_CATEVEN; goto do_AvBin32x4;
      case Iop_PolynomialMulAdd32x4: op = Pav_POLYMULADD; goto do_AvBin32x4;
      do_AvBin32x4: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBin32x4(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_Shl64x2:    op = Pav_SHL;    goto do_AvBin64x2;
      case Iop_Shr64x2:    op = Pav_SHR;    goto do_AvBin64x2;
      case Iop_Sar64x2:    op = Pav_SAR;    goto do_AvBin64x2;
      case Iop_Rol64x2:    op = Pav_ROTL;   goto do_AvBin64x2;
      case Iop_NarrowBin64to32x4:    op = Pav_PACKUU;  goto do_AvBin64x2;
      case Iop_QNarrowBin64Sto32Sx4: op = Pav_QPACKSS; goto do_AvBin64x2;
      case Iop_QNarrowBin64Uto32Ux4: op = Pav_QPACKUU; goto do_AvBin64x2;
      case Iop_InterleaveHI64x2:  op = Pav_MRGHI;  goto do_AvBin64x2;
      case Iop_InterleaveLO64x2:  op = Pav_MRGLO;  goto do_AvBin64x2;
      case Iop_Add64x2:    op = Pav_ADDU;   goto do_AvBin64x2;
      case Iop_Sub64x2:    op = Pav_SUBU;   goto do_AvBin64x2;
      case Iop_Max64Ux2:   op = Pav_MAXU;   goto do_AvBin64x2;
      case Iop_Max64Sx2:   op = Pav_MAXS;   goto do_AvBin64x2;
      case Iop_Min64Ux2:   op = Pav_MINU;   goto do_AvBin64x2;
      case Iop_Min64Sx2:   op = Pav_MINS;   goto do_AvBin64x2;
      case Iop_CmpEQ64x2:  op = Pav_CMPEQU; goto do_AvBin64x2;
      case Iop_CmpGT64Ux2: op = Pav_CMPGTU; goto do_AvBin64x2;
      case Iop_CmpGT64Sx2: op = Pav_CMPGTS; goto do_AvBin64x2;
      case Iop_PolynomialMulAdd64x2: op = Pav_POLYMULADD; goto do_AvBin64x2;
      do_AvBin64x2: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBin64x2(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_ShlN8x16: op = Pav_SHL; goto do_AvShift8x16;
      case Iop_SarN8x16: op = Pav_SAR; goto do_AvShift8x16;
      do_AvShift8x16: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2, IEndianess);
         addInstr(env, PPCInstr_AvBin8x16(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShlN16x8: op = Pav_SHL; goto do_AvShift16x8;
      case Iop_ShrN16x8: op = Pav_SHR; goto do_AvShift16x8;
      case Iop_SarN16x8: op = Pav_SAR; goto do_AvShift16x8;
      do_AvShift16x8: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2, IEndianess);
         addInstr(env, PPCInstr_AvBin16x8(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShlN32x4: op = Pav_SHL; goto do_AvShift32x4;
      case Iop_ShrN32x4: op = Pav_SHR; goto do_AvShift32x4;
      case Iop_SarN32x4: op = Pav_SAR; goto do_AvShift32x4;
      do_AvShift32x4: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2, IEndianess);
         addInstr(env, PPCInstr_AvBin32x4(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShlN64x2: op = Pav_SHL; goto do_AvShift64x2;
      case Iop_ShrN64x2: op = Pav_SHR; goto do_AvShift64x2;
      case Iop_SarN64x2: op = Pav_SAR; goto do_AvShift64x2;
      do_AvShift64x2: {
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg dst    = newVRegV(env);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2, IEndianess);
         addInstr(env, PPCInstr_AvBin64x2(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_ShrV128: op = Pav_SHR; goto do_AvShiftV128;
      case Iop_ShlV128: op = Pav_SHL; goto do_AvShiftV128;
      do_AvShiftV128: {
         HReg dst    = newVRegV(env);
         HReg r_src  = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg v_shft = mk_AvDuplicateRI(env, e->Iex.Binop.arg2, IEndianess);
         /* Note: shift value gets masked by 127 */
         addInstr(env, PPCInstr_AvBinary(op, dst, r_src, v_shft));
         return dst;
      }

      case Iop_Perm8x16: {
         HReg dst   = newVRegV(env);
         HReg v_src = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg v_ctl = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         addInstr(env, PPCInstr_AvPerm(dst, v_src, v_src, v_ctl));
         return dst;
      }

      case Iop_CipherV128:  op = Pav_CIPHERV128;   goto do_AvCipherV128;
      case Iop_CipherLV128: op = Pav_CIPHERLV128;  goto do_AvCipherV128;
      case Iop_NCipherV128: op = Pav_NCIPHERV128;  goto do_AvCipherV128;
      case Iop_NCipherLV128:op = Pav_NCIPHERLV128; goto do_AvCipherV128;
      do_AvCipherV128: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvCipherV128Binary(op, dst, arg1, arg2));
         return dst;
      }

      case Iop_SHA256:op = Pav_SHA256; goto do_AvHashV128;
      case Iop_SHA512:op = Pav_SHA512; goto do_AvHashV128;
      do_AvHashV128: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg dst  = newVRegV(env);
         PPCRI* s_field = iselWordExpr_RI(env, e->Iex.Binop.arg2, IEndianess);
         addInstr(env, PPCInstr_AvHashV128Binary(op, dst, arg1, s_field));
         return dst;
      }

      /* BCD Iops */
      case Iop_I128StoBCD128:
         {
            HReg dst = newVRegV(env);
            HReg arg = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
            PPCRI* ps = iselWordExpr_RI(env, e->Iex.Binop.arg2, IEndianess);

            addInstr(env, PPCInstr_AvBinaryInt( Pav_I128StoBCD128, dst, arg,
                                                ps ) );
            return dst;
         }

      case Iop_MulI128by10E:       op = Pav_MulI128by10E;      goto do_MulI128E;
      case Iop_MulI128by10ECarry:  op = Pav_MulI128by10ECarry; goto do_MulI128E;
      do_MulI128E: {
            HReg dst  = newVRegV(env);
            HReg argL = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
            HReg argR = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
            addInstr(env, PPCInstr_AvBinary(op, dst, argL, argR));
            return dst;
         }

      case Iop_BCDAdd:op = Pav_BCDAdd; goto do_AvBCDV128;
      case Iop_BCDSub:op = Pav_BCDSub; goto do_AvBCDV128;
      do_AvBCDV128: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1, IEndianess);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2, IEndianess);
         HReg dst  = newVRegV(env);
         addInstr(env, PPCInstr_AvBCDV128Binary(op, dst, arg1, arg2));
         return dst;
      }

      default:
         break;
      } /* switch (e->Iex.Binop.op) */
   } /* if (e->tag == Iex_Binop) */

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;
      switch (triop->op) {
      case Iop_Add32Fx4: fpop = Pavfp_ADDF; goto do_32Fx4_with_rm;
      case Iop_Sub32Fx4: fpop = Pavfp_SUBF; goto do_32Fx4_with_rm;
      case Iop_Mul32Fx4: fpop = Pavfp_MULF; goto do_32Fx4_with_rm;
      do_32Fx4_with_rm:
      {
         HReg argL = iselVecExpr(env, triop->arg2, IEndianess);
         HReg argR = iselVecExpr(env, triop->arg3, IEndianess);
         HReg dst  = newVRegV(env);
         /* FIXME: this is bogus, in the sense that Altivec ignores
            FPSCR.RM, at least for some FP operations.  So setting the
            RM is pointless.  This is only really correct in the case
            where the RM is known, at JIT time, to be Irrm_NEAREST,
            since -- at least for Altivec FP add/sub/mul -- the
            emitted insn is hardwired to round to nearest. */
         set_FPU_rounding_mode(env, triop->arg1, IEndianess);
         addInstr(env, PPCInstr_AvBin32Fx4(fpop, dst, argL, argR));
         return dst;
      }

      default:
         break;
      } /* switch (e->Iex.Triop.op) */
   } /* if (e->tag == Iex_Trinop) */


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

static void iselStmt ( ISelEnv* env, IRStmt* stmt, IREndness IEndianess )
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

      if (end != IEndianess)
         goto stmt_fail;
      if (!mode64 && (tya != Ity_I32))
         goto stmt_fail;
      if (mode64 && (tya != Ity_I64))
         goto stmt_fail;

      if (tyd == Ity_I8 || tyd == Ity_I16 || tyd == Ity_I32 ||
          (mode64 && (tyd == Ity_I64))) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/,
                                 IEndianess);
         HReg r_src = iselWordExpr_R(env, stmt->Ist.Store.data, IEndianess);
         addInstr(env, PPCInstr_Store( toUChar(sizeofIRType(tyd)), 
                                       am_addr, r_src, mode64 ));
         return;
      }
      if (tyd == Ity_F64) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/,
                                 IEndianess);
         HReg fr_src = iselDblExpr(env, stmt->Ist.Store.data, IEndianess);
         addInstr(env,
                  PPCInstr_FpLdSt(False/*store*/, 8, fr_src, am_addr));
         return;
      }
      if (tyd == Ity_F32) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/,
                                 IEndianess);
         HReg fr_src = iselFltExpr(env, stmt->Ist.Store.data, IEndianess);
         addInstr(env,
                  PPCInstr_FpLdSt(False/*store*/, 4, fr_src, am_addr));
         return;
      }
      if (tyd == Ity_D64) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/,
                                 IEndianess);
         HReg fr_src = iselDfp64Expr(env, stmt->Ist.Store.data, IEndianess);
         addInstr(env,
                  PPCInstr_FpLdSt(False/*store*/, 8, fr_src, am_addr));
         return;
      }
      if (tyd == Ity_D32) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/,
                                 IEndianess);
         HReg fr_src = iselDfp32Expr(env, stmt->Ist.Store.data, IEndianess);
         addInstr(env,
                  PPCInstr_FpLdSt(False/*store*/, 4, fr_src, am_addr));
         return;
      }
      if (tyd == Ity_V128) {
         PPCAMode* am_addr
            = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd/*of xfer*/,
                                 IEndianess);
         HReg v_src = iselVecExpr(env, stmt->Ist.Store.data, IEndianess);
         addInstr(env,
                  PPCInstr_AvLdSt(False/*store*/, 16, v_src, am_addr));
         return;
      }
      if (tyd == Ity_I64 && !mode64) {
         /* Just calculate the address in the register.  Life is too
            short to arse around trying and possibly failing to adjust
            the offset in a 'reg+offset' style amode. */
         HReg rHi32, rLo32;
         HReg r_addr = iselWordExpr_R(env, stmt->Ist.Store.addr, IEndianess);
         iselInt64Expr( &rHi32, &rLo32, env, stmt->Ist.Store.data,
                        IEndianess );
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
         HReg r_src = iselWordExpr_R(env, stmt->Ist.Put.data, IEndianess);
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
         iselInt64Expr(&rHi,&rLo, env, stmt->Ist.Put.data, IEndianess);
         addInstr(env, PPCInstr_Store( 4, am_addr,  rHi, mode64 ));
         addInstr(env, PPCInstr_Store( 4, am_addr4, rLo, mode64 ));
         return;
      }
      if (ty == Ity_I128) {
         HReg rHi, rLo;
         PPCAMode* am_addr  = PPCAMode_IR( stmt->Ist.Put.offset,
                                           GuestStatePtr(mode64) );
         PPCAMode* am_addr4 = advance4(env, am_addr);

         iselInt128Expr(&rHi,&rLo, env, stmt->Ist.Put.data, IEndianess);
         addInstr(env, PPCInstr_Store( 4, am_addr,  rHi, mode64 ));
         addInstr(env, PPCInstr_Store( 4, am_addr4, rLo, mode64 ));
         return;
      }
      if (ty == Ity_F128) {
         /* Guest state vectors are 16byte aligned,
            so don't need to worry here */
         HReg v_src = iselFp128Expr(env, stmt->Ist.Put.data, IEndianess);

         PPCAMode* am_addr  = PPCAMode_IR( stmt->Ist.Put.offset,
                                           GuestStatePtr(mode64) );
         addInstr(env,
                  PPCInstr_AvLdSt(False/*store*/, 16, v_src, am_addr));
         return;
      }
      if (ty == Ity_V128) {
         /* Guest state vectors are 16byte aligned,
            so don't need to worry here */
         HReg v_src = iselVecExpr(env, stmt->Ist.Put.data, IEndianess);
         PPCAMode* am_addr  = PPCAMode_IR( stmt->Ist.Put.offset,
                                           GuestStatePtr(mode64) );
         addInstr(env,
                  PPCInstr_AvLdSt(False/*store*/, 16, v_src, am_addr));
         return;
      }
      if (ty == Ity_F64) {
         HReg fr_src = iselDblExpr(env, stmt->Ist.Put.data, IEndianess);
         PPCAMode* am_addr = PPCAMode_IR( stmt->Ist.Put.offset,
                                          GuestStatePtr(mode64) );
         addInstr(env, PPCInstr_FpLdSt( False/*store*/, 8,
                                        fr_src, am_addr ));
         return;
      }
      if (ty == Ity_D32) {
         /* The 32-bit value is stored in a 64-bit register */
         HReg fr_src = iselDfp32Expr( env, stmt->Ist.Put.data, IEndianess );
         PPCAMode* am_addr = PPCAMode_IR( stmt->Ist.Put.offset,
                                          GuestStatePtr(mode64) );
         addInstr( env, PPCInstr_FpLdSt( False/*store*/, 8,
                                         fr_src, am_addr ) );
         return;
      }
      if (ty == Ity_D64) {
         HReg fr_src = iselDfp64Expr( env, stmt->Ist.Put.data, IEndianess );
         PPCAMode* am_addr = PPCAMode_IR( stmt->Ist.Put.offset,
                                          GuestStatePtr(mode64) );
         addInstr( env, PPCInstr_FpLdSt( False/*store*/, 8, fr_src, am_addr ) );
         return;
      }
      break;
   }
      
   /* --------- Indexed PUT --------- */
   case Ist_PutI: {
      IRPutI *puti = stmt->Ist.PutI.details;

      PPCAMode* dst_am
         = genGuestArrayOffset(
              env, puti->descr, 
              puti->ix, puti->bias,
              IEndianess );
      IRType ty = typeOfIRExpr(env->type_env, puti->data);
      if (mode64 && ty == Ity_I64) {
         HReg r_src = iselWordExpr_R(env, puti->data, IEndianess);
         addInstr(env, PPCInstr_Store( toUChar(8),
                                       dst_am, r_src, mode64 ));
         return;
      }
      if ((!mode64) && ty == Ity_I32) {
         HReg r_src = iselWordExpr_R(env, puti->data, IEndianess);
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
         HReg r_src = iselWordExpr_R(env, stmt->Ist.WrTmp.data, IEndianess);
         addInstr(env, mk_iMOVds_RR( r_dst, r_src ));
         return;
      }
      if (!mode64 && ty == Ity_I64) {
         HReg r_srcHi, r_srcLo, r_dstHi, r_dstLo;

         iselInt64Expr(&r_srcHi,&r_srcLo, env, stmt->Ist.WrTmp.data,
                       IEndianess);
         lookupIRTempPair( &r_dstHi, &r_dstLo, env, tmp);
         addInstr(env, mk_iMOVds_RR(r_dstHi, r_srcHi) );
         addInstr(env, mk_iMOVds_RR(r_dstLo, r_srcLo) );
         return;
      }
      if (mode64 && ty == Ity_I128) {
         HReg r_srcHi, r_srcLo, r_dstHi, r_dstLo;
         iselInt128Expr(&r_srcHi,&r_srcLo, env, stmt->Ist.WrTmp.data,
                        IEndianess);
         lookupIRTempPair( &r_dstHi, &r_dstLo, env, tmp);
         addInstr(env, mk_iMOVds_RR(r_dstHi, r_srcHi) );
         addInstr(env, mk_iMOVds_RR(r_dstLo, r_srcLo) );
         return;
      }
      if (!mode64 && ty == Ity_I128) {
         HReg r_srcHi    = INVALID_HREG;
         HReg r_srcMedHi = INVALID_HREG;
         HReg r_srcMedLo = INVALID_HREG;
         HReg r_srcLo    = INVALID_HREG;
         HReg r_dstHi, r_dstMedHi, r_dstMedLo, r_dstLo;

         iselInt128Expr_to_32x4(&r_srcHi, &r_srcMedHi,
                                &r_srcMedLo, &r_srcLo,
                                env, stmt->Ist.WrTmp.data, IEndianess);

         lookupIRTempQuad( &r_dstHi, &r_dstMedHi, &r_dstMedLo,
                           &r_dstLo, env, tmp);

         addInstr(env, mk_iMOVds_RR(r_dstHi,    r_srcHi) );
         addInstr(env, mk_iMOVds_RR(r_dstMedHi, r_srcMedHi) );
         addInstr(env, mk_iMOVds_RR(r_dstMedLo, r_srcMedLo) );
         addInstr(env, mk_iMOVds_RR(r_dstLo,    r_srcLo) );
         return;
      }
      if (ty == Ity_I1) {
         PPCCondCode cond = iselCondCode(env, stmt->Ist.WrTmp.data,
                                         IEndianess);
         HReg r_dst = lookupIRTemp(env, tmp);
         addInstr(env, PPCInstr_Set(cond, r_dst));
         return;
      }
      if (ty == Ity_F64) {
         HReg fr_dst = lookupIRTemp(env, tmp);
         HReg fr_src = iselDblExpr(env, stmt->Ist.WrTmp.data, IEndianess);
         addInstr(env, PPCInstr_FpUnary(Pfp_MOV, fr_dst, fr_src));
         return;
      }
      if (ty == Ity_F32) {
         HReg fr_dst = lookupIRTemp(env, tmp);
         HReg fr_src = iselFltExpr(env, stmt->Ist.WrTmp.data, IEndianess);
         addInstr(env, PPCInstr_FpUnary(Pfp_MOV, fr_dst, fr_src));
         return;
      }
      if (ty == Ity_D32) {
         HReg fr_dst = lookupIRTemp(env, tmp);
         HReg fr_src = iselDfp32Expr(env, stmt->Ist.WrTmp.data, IEndianess);
         addInstr(env, PPCInstr_Dfp64Unary(Pfp_MOV, fr_dst, fr_src));
         return;
      }
      if (ty == Ity_F128) {
         HReg v_dst = lookupIRTemp(env, tmp);
         HReg v_src = iselFp128Expr(env, stmt->Ist.WrTmp.data, IEndianess);
         addInstr(env, PPCInstr_AvUnary(Pav_MOV, v_dst, v_src));
         return;
      }
      if (ty == Ity_V128) {
         HReg v_dst = lookupIRTemp(env, tmp);
         HReg v_src = iselVecExpr(env, stmt->Ist.WrTmp.data, IEndianess);
         addInstr(env, PPCInstr_AvUnary(Pav_MOV, v_dst, v_src));
         return;
      }
      if (ty == Ity_D64) {
         HReg fr_dst = lookupIRTemp( env, tmp );
         HReg fr_src = iselDfp64Expr( env, stmt->Ist.WrTmp.data, IEndianess );
         addInstr( env, PPCInstr_Dfp64Unary( Pfp_MOV, fr_dst, fr_src ) );
         return;
      }
      if (ty == Ity_D128) {
         HReg fr_srcHi, fr_srcLo, fr_dstHi, fr_dstLo;
	 //         lookupDfp128IRTempPair( &fr_dstHi, &fr_dstLo, env, tmp );
         lookupIRTempPair( &fr_dstHi, &fr_dstLo, env, tmp );
         iselDfp128Expr( &fr_srcHi, &fr_srcLo, env, stmt->Ist.WrTmp.data,
                         IEndianess );
         addInstr( env, PPCInstr_Dfp64Unary( Pfp_MOV, fr_dstHi, fr_srcHi ) );
         addInstr( env, PPCInstr_Dfp64Unary( Pfp_MOV, fr_dstLo, fr_srcLo ) );
         return;
      }
      break;
   }

   /* --------- Load Linked or Store Conditional --------- */
   case Ist_LLSC: {
      IRTemp res    = stmt->Ist.LLSC.result;
      IRType tyRes  = typeOfIRTemp(env->type_env, res);
      IRType tyAddr = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.addr);

      if (stmt->Ist.LLSC.end != IEndianess)
         goto stmt_fail;
      if (!mode64 && (tyAddr != Ity_I32))
         goto stmt_fail;
      if (mode64 && (tyAddr != Ity_I64))
         goto stmt_fail;

      if (stmt->Ist.LLSC.storedata == NULL) {
         /* LL */
         HReg r_addr = iselWordExpr_R( env, stmt->Ist.LLSC.addr, IEndianess );
         HReg r_dst  = lookupIRTemp(env, res);
         if (tyRes == Ity_I8) {
            addInstr(env, PPCInstr_LoadL( 1, r_dst, r_addr, mode64 ));
            return;
         }
         if (tyRes == Ity_I16) {
            addInstr(env, PPCInstr_LoadL( 2, r_dst, r_addr, mode64 ));
            return;
         }
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
         HReg   r_a    = iselWordExpr_R(env, stmt->Ist.LLSC.addr, IEndianess);
         HReg   r_src  = iselWordExpr_R(env, stmt->Ist.LLSC.storedata,
                                        IEndianess);
         HReg   r_tmp  = newVRegI(env);
         IRType tyData = typeOfIRExpr(env->type_env,
                                      stmt->Ist.LLSC.storedata);
         vassert(tyRes == Ity_I1);
         if (tyData == Ity_I8 || tyData == Ity_I16 || tyData == Ity_I32 ||
            (tyData == Ity_I64 && mode64)) {
            int size = 0;

            if (tyData == Ity_I64)
               size = 8;
            else if (tyData == Ity_I32)
               size = 4;
            else if (tyData == Ity_I16)
               size = 2;
            else if (tyData == Ity_I8)
               size = 1;

            addInstr(env, PPCInstr_StoreC( size,
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
      IRDirty* d = stmt->Ist.Dirty.details;

      /* Figure out the return type, if any. */
      IRType retty = Ity_INVALID;
      if (d->tmp != IRTemp_INVALID)
         retty = typeOfIRTemp(env->type_env, d->tmp);

      /* Throw out any return types we don't know about.  The set of
         acceptable return types is the same in both 32- and 64-bit
         mode, so we don't need to inspect mode64 to make a
         decision. */
      Bool retty_ok = False;
      switch (retty) {
         case Ity_INVALID: /* function doesn't return anything */
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
      doHelperCall( &addToSp, &rloc, env, d->guard, d->cee, retty, d->args,
                    IEndianess );
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
            /* The returned value is in %r3.  Park it in the register
               associated with tmp. */
            HReg r_dst = lookupIRTemp(env, d->tmp);
            addInstr(env, mk_iMOVds_RR(r_dst, hregPPC_GPR3(mode64)));
            vassert(rloc.pri == RLPri_Int);
            vassert(addToSp == 0);
            return;
         }
         case Ity_I64:
            if (mode64) {
               /* The returned value is in %r3.  Park it in the register
                  associated with tmp. */
               HReg r_dst = lookupIRTemp(env, d->tmp);
               addInstr(env, mk_iMOVds_RR(r_dst, hregPPC_GPR3(mode64)));
               vassert(rloc.pri == RLPri_Int);
               vassert(addToSp == 0);
            } else {
               /* The returned value is in %r3:%r4.  Park it in the
                  register-pair associated with tmp. */
               HReg r_dstHi = INVALID_HREG;
               HReg r_dstLo = INVALID_HREG;
               lookupIRTempPair( &r_dstHi, &r_dstLo, env, d->tmp);
               addInstr(env, mk_iMOVds_RR(r_dstHi, hregPPC_GPR3(mode64)));
               addInstr(env, mk_iMOVds_RR(r_dstLo, hregPPC_GPR4(mode64)));
               vassert(rloc.pri == RLPri_2Int);
               vassert(addToSp == 0);
            }
            return;
         case Ity_V128: {
            /* The returned value is on the stack, and *retloc tells
               us where.  Fish it off the stack and then move the
               stack pointer upwards to clear it, as directed by
               doHelperCall. */
            vassert(rloc.pri == RLPri_V128SpRel);
            vassert(addToSp >= 16);
            HReg      dst = lookupIRTemp(env, d->tmp);
            PPCAMode* am  = PPCAMode_IR(rloc.spOff, StackFramePtr(mode64));
            addInstr(env, PPCInstr_AvLdSt( True/*load*/, 16, dst, am ));
            add_to_sp(env, addToSp);
            return;
         }
         default:
            /*NOTREACHED*/
            vassert(0);
      }
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
      IRConst* dst = stmt->Ist.Exit.dst;
      if (!mode64 && dst->tag != Ico_U32)
         vpanic("iselStmt(ppc): Ist_Exit: dst is not a 32-bit value");
      if (mode64 && dst->tag != Ico_U64)
         vpanic("iselStmt(ppc64): Ist_Exit: dst is not a 64-bit value");

      PPCCondCode cc    = iselCondCode(env, stmt->Ist.Exit.guard, IEndianess);
      PPCAMode*   amCIA = PPCAMode_IR(stmt->Ist.Exit.offsIP,
                                      hregPPC_GPR31(mode64));

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
            addInstr(env, PPCInstr_XDirect(
                             mode64 ? (Addr64)stmt->Ist.Exit.dst->Ico.U64
                                    : (Addr64)stmt->Ist.Exit.dst->Ico.U32,
                             amCIA, cc, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselWordExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst),
                                    IEndianess);
            addInstr(env, PPCInstr_XAssisted(r, amCIA, cc, Ijk_Boring));
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
         case Ijk_SigTRAP:
         case Ijk_Sys_syscall:
         case Ijk_InvalICache:
         {
            HReg r = iselWordExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst),
                                    IEndianess);
            addInstr(env, PPCInstr_XAssisted(r, amCIA, cc,
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
   vpanic("iselStmt(ppc)");
}
 

/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

static void iselNext ( ISelEnv* env,
                       IRExpr* next, IRJumpKind jk, Int offsIP,
                       IREndness IEndianess)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf( "\n-- PUT(%d) = ", offsIP);
      ppIRExpr( next );
      vex_printf( "; exit-");
      ppIRJumpKind(jk);
      vex_printf( "\n");
   }

   PPCCondCode always = mk_PPCCondCode( Pct_ALWAYS, Pcf_NONE );

   /* Case: boring transfer to known address */
   if (next->tag == Iex_Const) {
      IRConst* cdst = next->Iex.Const.con;
      vassert(cdst->tag == (env->mode64 ? Ico_U64 :Ico_U32));
      if (jk == Ijk_Boring || jk == Ijk_Call) {
         /* Boring transfer to known address */
         PPCAMode* amCIA = PPCAMode_IR(offsIP, hregPPC_GPR31(env->mode64));
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = env->mode64
               ? (((Addr64)cdst->Ico.U64) > (Addr64)env->max_ga)
               : (((Addr32)cdst->Ico.U32) > (Addr32)env->max_ga);
            if (0) vex_printf("%s", toFastEP ? "X" : ".");
            addInstr(env, PPCInstr_XDirect(
                             env->mode64 ? (Addr64)cdst->Ico.U64
                                         : (Addr64)cdst->Ico.U32,
                             amCIA, always, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselWordExpr_R(env, next, IEndianess);
            addInstr(env, PPCInstr_XAssisted(r, amCIA, always,
                                             Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
      case Ijk_Boring: case Ijk_Ret: case Ijk_Call: {
         HReg       r     = iselWordExpr_R(env, next, IEndianess);
         PPCAMode*  amCIA = PPCAMode_IR(offsIP, hregPPC_GPR31(env->mode64));
         if (env->chainingAllowed) {
            addInstr(env, PPCInstr_XIndir(r, amCIA, always));
         } else {
            addInstr(env, PPCInstr_XAssisted(r, amCIA, always,
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
      case Ijk_SigTRAP:
      case Ijk_Sys_syscall:
      case Ijk_InvalICache:
      {
         HReg      r     = iselWordExpr_R(env, next, IEndianess);
         PPCAMode* amCIA = PPCAMode_IR(offsIP, hregPPC_GPR31(env->mode64));
         addInstr(env, PPCInstr_XAssisted(r, amCIA, always, jk));
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

/* Translate an entire SB to ppc code. */
HInstrArray* iselSB_PPC ( const IRSB* bb,
                          VexArch      arch_host,
                          const VexArchInfo* archinfo_host,
                          const VexAbiInfo*  vbi,
                          Int offs_Host_EvC_Counter,
                          Int offs_Host_EvC_FailAddr,
                          Bool chainingAllowed,
                          Bool addProfInc,
                          Addr max_ga)

{
   Int       i, j;
   HReg      hregLo, hregMedLo, hregMedHi, hregHi;
   ISelEnv*  env;
   UInt      hwcaps_host = archinfo_host->hwcaps;
   Bool      mode64 = False;
   UInt      mask32, mask64;
   PPCAMode *amCounter, *amFailAddr;
   IREndness IEndianess;

   vassert(arch_host == VexArchPPC32 || arch_host == VexArchPPC64);
   mode64 = arch_host == VexArchPPC64;

   /* do some sanity checks,
    * Note: no 32-bit support for ISA 3.0
    */
   mask32 = VEX_HWCAPS_PPC32_F | VEX_HWCAPS_PPC32_V
            | VEX_HWCAPS_PPC32_FX | VEX_HWCAPS_PPC32_GX | VEX_HWCAPS_PPC32_VX
            | VEX_HWCAPS_PPC32_DFP | VEX_HWCAPS_PPC32_ISA2_07;

   mask64 = VEX_HWCAPS_PPC64_V | VEX_HWCAPS_PPC64_FX
            | VEX_HWCAPS_PPC64_GX | VEX_HWCAPS_PPC64_VX | VEX_HWCAPS_PPC64_DFP
            | VEX_HWCAPS_PPC64_ISA2_07 | VEX_HWCAPS_PPC64_ISA3_0;

   if (mode64) {
      vassert((hwcaps_host & mask32) == 0);
   } else {
      vassert((hwcaps_host & mask64) == 0);
   }

   /* Check that the host's endianness is as expected. */
   vassert((archinfo_host->endness == VexEndnessBE) ||
	   (archinfo_host->endness == VexEndnessLE));

   if (archinfo_host->endness == VexEndnessBE)
     IEndianess = Iend_BE;
   else
     IEndianess = Iend_LE;

   /* Make up an initial environment to use. */
   env = LibVEX_Alloc_inline(sizeof(ISelEnv));
   env->vreg_ctr = 0;

   /* Are we being ppc32 or ppc64? */
   env->mode64 = mode64;

   /* Set up output code array. */
   env->code = newHInstrArray();

   /* Copy BB's type env. */
   env->type_env = bb->tyenv;

   /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
    * change as we go along. 
    *
    * vregmap2 and vregmap3 are only used in 32 bit mode 
    * for supporting I128 in 32-bit mode
    */
   env->n_vregmap = bb->tyenv->types_used;
   env->vregmapLo    = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
   env->vregmapMedLo = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
   if (mode64) {
      env->vregmapMedHi = NULL;
      env->vregmapHi    = NULL;
   } else {
      env->vregmapMedHi = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
      env->vregmapHi    = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
   }

   /* and finally ... */
   env->chainingAllowed = chainingAllowed;
   env->max_ga          = max_ga;
   env->hwcaps          = hwcaps_host;
   env->previous_rm     = NULL;
   env->vbi             = vbi;

   /* For each IR temporary, allocate a suitably-kinded virtual
      register. */
   j = 0;
   for (i = 0; i < env->n_vregmap; i++) {
      hregLo = hregMedLo = hregMedHi = hregHi = INVALID_HREG;
      switch (bb->tyenv->types[i]) {
      case Ity_I1:
      case Ity_I8:
      case Ity_I16:
      case Ity_I32:
         if (mode64) {
            hregLo = mkHReg(True, HRcInt64, 0, j++);
         } else {
            hregLo = mkHReg(True, HRcInt32, 0, j++);
         }
         break;
      case Ity_I64:  
         if (mode64) {
            hregLo    = mkHReg(True, HRcInt64, 0, j++);
         } else {
            hregLo    = mkHReg(True, HRcInt32, 0, j++);
            hregMedLo = mkHReg(True, HRcInt32, 0, j++);
         }
         break;
      case Ity_I128:
         if (mode64) {
            hregLo    = mkHReg(True, HRcInt64, 0, j++);
            hregMedLo = mkHReg(True, HRcInt64, 0, j++);
         } else {
            hregLo    = mkHReg(True, HRcInt32, 0, j++);
            hregMedLo = mkHReg(True, HRcInt32, 0, j++);
            hregMedHi = mkHReg(True, HRcInt32, 0, j++);
            hregHi    = mkHReg(True, HRcInt32, 0, j++);
         }
         break;
      case Ity_F32:
      case Ity_F64:
         hregLo = mkHReg(True, HRcFlt64, 0, j++);
         break;
      case Ity_F128:
      case Ity_V128:
         hregLo = mkHReg(True, HRcVec128, 0, j++);
         break;
      case Ity_D32:
      case Ity_D64:
         hregLo = mkHReg(True, HRcFlt64, 0, j++);
         break;
      case Ity_D128:
         hregLo    = mkHReg(True, HRcFlt64, 0, j++);
         hregMedLo = mkHReg(True, HRcFlt64, 0, j++);
         break;
      default:
         ppIRType(bb->tyenv->types[i]);
         vpanic("iselBB(ppc): IRTemp type");
      }
      env->vregmapLo[i]    = hregLo;
      env->vregmapMedLo[i] = hregMedLo;
      if (!mode64) {
         env->vregmapMedHi[i] = hregMedHi;
         env->vregmapHi[i]    = hregHi;
      }
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   amCounter  = PPCAMode_IR(offs_Host_EvC_Counter, hregPPC_GPR31(mode64));
   amFailAddr = PPCAMode_IR(offs_Host_EvC_FailAddr, hregPPC_GPR31(mode64));
   addInstr(env, PPCInstr_EvCheck(amCounter, amFailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, PPCInstr_ProfInc());
   }

   /* Ok, finally we can iterate over the statements. */
   for (i = 0; i < bb->stmts_used; i++)
      iselStmt(env, bb->stmts[i], IEndianess);

   iselNext(env, bb->next, bb->jumpkind, bb->offsIP, IEndianess);

   /* record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;
   return env->code;
}


/*---------------------------------------------------------------*/
/*--- end                                     host_ppc_isel.c ---*/
/*---------------------------------------------------------------*/
