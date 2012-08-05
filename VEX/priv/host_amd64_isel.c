
/*---------------------------------------------------------------*/
/*--- begin                                 host_amd64_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2012 OpenWorks LLP
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
#include "host_generic_simd64.h"
#include "host_generic_simd128.h"
#include "host_amd64_defs.h"


/*---------------------------------------------------------*/
/*--- x87/SSE control word stuff                        ---*/
/*---------------------------------------------------------*/

/* Vex-generated code expects to run with the FPU set as follows: all
   exceptions masked, round-to-nearest, precision = 53 bits.  This
   corresponds to a FPU control word value of 0x027F.

   Similarly the SSE control word (%mxcsr) should be 0x1F80.

   %fpucw and %mxcsr should have these values on entry to
   Vex-generated code, and should those values should be
   unchanged at exit.
*/

#define DEFAULT_FPUCW 0x027F

#define DEFAULT_MXCSR 0x1F80

/* debugging only, do not use */
/* define DEFAULT_FPUCW 0x037F */


/*---------------------------------------------------------*/
/*--- misc helpers                                      ---*/
/*---------------------------------------------------------*/

/* These are duplicated in guest-amd64/toIR.c */
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
/*--- ISelEnv                                           ---*/
/*---------------------------------------------------------*/

/* This carries around:

   - A mapping from IRTemp to IRType, giving the type of any IRTemp we
     might encounter.  This is computed before insn selection starts,
     and does not change.

   - A mapping from IRTemp to HReg.  This tells the insn selector
     which virtual register is associated with each IRTemp
     temporary.  This is computed before insn selection starts, and
     does not change.  We expect this mapping to map precisely the
     same set of IRTemps as the type mapping does.

        - vregmap   holds the primary register for the IRTemp.
        - vregmapHI is only used for 128-bit integer-typed
             IRTemps.  It holds the identity of a second
             64-bit virtual HReg, which holds the high half
             of the value.

   - The host subarchitecture we are selecting insns for.  
     This is set at the start and does not change.

   - The code array, that is, the insns selected so far.

   - A counter, for generating new virtual registers.

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

   Note, this is all host-independent.  (JRS 20050201: well, kinda
   ... not completely.  Compare with ISelEnv for X86.)
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
   vassert(env->vregmapHI[tmp] != INVALID_HREG);
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr ( ISelEnv* env, AMD64Instr* instr )
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppAMD64Instr(instr, True);
      vex_printf("\n");
   }
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(env->vreg_ctr, HRcInt64, True/*virtual reg*/);
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
static AMD64RMI*     iselIntExpr_RMI_wrk ( ISelEnv* env, IRExpr* e );
static AMD64RMI*     iselIntExpr_RMI     ( ISelEnv* env, IRExpr* e );

static AMD64RI*      iselIntExpr_RI_wrk  ( ISelEnv* env, IRExpr* e );
static AMD64RI*      iselIntExpr_RI      ( ISelEnv* env, IRExpr* e );

static AMD64RM*      iselIntExpr_RM_wrk  ( ISelEnv* env, IRExpr* e );
static AMD64RM*      iselIntExpr_RM      ( ISelEnv* env, IRExpr* e );

static HReg          iselIntExpr_R_wrk   ( ISelEnv* env, IRExpr* e );
static HReg          iselIntExpr_R       ( ISelEnv* env, IRExpr* e );

static AMD64AMode*   iselIntExpr_AMode_wrk ( ISelEnv* env, IRExpr* e );
static AMD64AMode*   iselIntExpr_AMode     ( ISelEnv* env, IRExpr* e );

static void          iselInt128Expr_wrk ( /*OUT*/HReg* rHi, HReg* rLo, 
                                          ISelEnv* env, IRExpr* e );
static void          iselInt128Expr     ( /*OUT*/HReg* rHi, HReg* rLo, 
                                          ISelEnv* env, IRExpr* e );

static AMD64CondCode iselCondCode_wrk    ( ISelEnv* env, IRExpr* e );
static AMD64CondCode iselCondCode        ( ISelEnv* env, IRExpr* e );

static HReg          iselDblExpr_wrk     ( ISelEnv* env, IRExpr* e );
static HReg          iselDblExpr         ( ISelEnv* env, IRExpr* e );

static HReg          iselFltExpr_wrk     ( ISelEnv* env, IRExpr* e );
static HReg          iselFltExpr         ( ISelEnv* env, IRExpr* e );

static HReg          iselVecExpr_wrk     ( ISelEnv* env, IRExpr* e );
static HReg          iselVecExpr         ( ISelEnv* env, IRExpr* e );

static void          iselDVecExpr_wrk ( /*OUT*/HReg* rHi, HReg* rLo, 
                                        ISelEnv* env, IRExpr* e );
static void          iselDVecExpr     ( /*OUT*/HReg* rHi, HReg* rLo, 
                                        ISelEnv* env, IRExpr* e );


/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

static Bool sane_AMode ( AMD64AMode* am )
{
   switch (am->tag) {
      case Aam_IR:
         return 
            toBool( hregClass(am->Aam.IR.reg) == HRcInt64
                    && (hregIsVirtual(am->Aam.IR.reg)
                        || am->Aam.IR.reg == hregAMD64_RBP()) );
      case Aam_IRRS:
         return 
            toBool( hregClass(am->Aam.IRRS.base) == HRcInt64
                    && hregIsVirtual(am->Aam.IRRS.base)
                    && hregClass(am->Aam.IRRS.index) == HRcInt64
                    && hregIsVirtual(am->Aam.IRRS.index) );
      default:
        vpanic("sane_AMode: unknown amd64 amode tag");
   }
}


/* Can the lower 32 bits be signedly widened to produce the whole
   64-bit value?  In other words, are the top 33 bits either all 0 or
   all 1 ? */
static Bool fitsIn32Bits ( ULong x )
{
   Long y0 = (Long)x;
   Long y1 = y0;
   y1 <<= 32;
   y1 >>=/*s*/ 32;
   return toBool(x == y1);
}

/* Is this a 64-bit zero expression? */

static Bool isZeroU64 ( IRExpr* e )
{
   return e->tag == Iex_Const
          && e->Iex.Const.con->tag == Ico_U64
          && e->Iex.Const.con->Ico.U64 == 0ULL;
}

static Bool isZeroU32 ( IRExpr* e )
{
   return e->tag == Iex_Const
          && e->Iex.Const.con->tag == Ico_U32
          && e->Iex.Const.con->Ico.U32 == 0;
}

/* Make a int reg-reg move. */

static AMD64Instr* mk_iMOVsd_RR ( HReg src, HReg dst )
{
   vassert(hregClass(src) == HRcInt64);
   vassert(hregClass(dst) == HRcInt64);
   return AMD64Instr_Alu64R(Aalu_MOV, AMD64RMI_Reg(src), dst);
}

/* Make a vector (128 bit) reg-reg move. */

static AMD64Instr* mk_vMOVsd_RR ( HReg src, HReg dst )
{
   vassert(hregClass(src) == HRcVec128);
   vassert(hregClass(dst) == HRcVec128);
   return AMD64Instr_SseReRg(Asse_MOV, src, dst);
}

/* Advance/retreat %rsp by n. */

static void add_to_rsp ( ISelEnv* env, Int n )
{
   vassert(n > 0 && n < 256 && (n%8) == 0);
   addInstr(env, 
            AMD64Instr_Alu64R(Aalu_ADD, AMD64RMI_Imm(n), 
                                        hregAMD64_RSP()));
}

static void sub_from_rsp ( ISelEnv* env, Int n )
{
   vassert(n > 0 && n < 256 && (n%8) == 0);
   addInstr(env, 
            AMD64Instr_Alu64R(Aalu_SUB, AMD64RMI_Imm(n), 
                                        hregAMD64_RSP()));
}

/* Push 64-bit constants on the stack. */
static void push_uimm64( ISelEnv* env, ULong uimm64 )
{
   /* If uimm64 can be expressed as the sign extension of its
      lower 32 bits, we can do it the easy way. */
   Long simm64 = (Long)uimm64;
   if ( simm64 == ((simm64 << 32) >> 32) ) {
      addInstr( env, AMD64Instr_Push(AMD64RMI_Imm( (UInt)uimm64 )) );
   } else {
      HReg tmp = newVRegI(env);
      addInstr( env, AMD64Instr_Imm64(uimm64, tmp) );
      addInstr( env, AMD64Instr_Push(AMD64RMI_Reg(tmp)) );
   }
}


/* Used only in doHelperCall.  If possible, produce a single
   instruction which computes 'e' into 'dst'.  If not possible, return
   NULL. */

static AMD64Instr* iselIntExpr_single_instruction ( ISelEnv* env,
                                                    HReg     dst,
                                                    IRExpr*  e )
{
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I64);

   if (e->tag == Iex_Const) {
      vassert(e->Iex.Const.con->tag == Ico_U64);
      if (fitsIn32Bits(e->Iex.Const.con->Ico.U64)) {
         return AMD64Instr_Alu64R(
                   Aalu_MOV,
                   AMD64RMI_Imm(toUInt(e->Iex.Const.con->Ico.U64)),
                   dst
                );
      } else {
         return AMD64Instr_Imm64(e->Iex.Const.con->Ico.U64, dst);
      }
   }

   if (e->tag == Iex_RdTmp) {
      HReg src = lookupIRTemp(env, e->Iex.RdTmp.tmp);
      return mk_iMOVsd_RR(src, dst);
   }

   if (e->tag == Iex_Get) {
      vassert(e->Iex.Get.ty == Ity_I64);
      return AMD64Instr_Alu64R(
                Aalu_MOV,
                AMD64RMI_Mem(
                   AMD64AMode_IR(e->Iex.Get.offset,
                                 hregAMD64_RBP())),
                dst);
   }

   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_32Uto64 
       && e->Iex.Unop.arg->tag == Iex_RdTmp) {
      HReg src = lookupIRTemp(env, e->Iex.Unop.arg->Iex.RdTmp.tmp);
      return AMD64Instr_MovxLQ(False, src, dst);
   }

   if (0) { ppIRExpr(e); vex_printf("\n"); }

   return NULL;
}


/* Do a complete function call.  guard is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional. */

static
void doHelperCall ( ISelEnv* env, 
                    Bool passBBP, 
                    IRExpr* guard, IRCallee* cee, IRExpr** args )
{
   AMD64CondCode cc;
   HReg          argregs[6];
   HReg          tmpregs[6];
   AMD64Instr*   fastinstrs[6];
   Int           n_args, i, argreg;

   /* Marshal args for a call and do the call.

      If passBBP is True, %rbp (the baseblock pointer) is to be passed
      as the first arg.

      This function only deals with a tiny set of possibilities, which
      cover all helpers in practice.  The restrictions are that only
      arguments in registers are supported, hence only 6x64 integer
      bits in total can be passed.  In fact the only supported arg
      type is I64.

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

   /* Note that the cee->regparms field is meaningless on AMD64 host
      (since there is only one calling convention) and so we always
      ignore it. */

   n_args = 0;
   for (i = 0; args[i]; i++)
      n_args++;

   if (6 < n_args + (passBBP ? 1 : 0))
      vpanic("doHelperCall(AMD64): cannot currently handle > 6 args");

   argregs[0] = hregAMD64_RDI();
   argregs[1] = hregAMD64_RSI();
   argregs[2] = hregAMD64_RDX();
   argregs[3] = hregAMD64_RCX();
   argregs[4] = hregAMD64_R8();
   argregs[5] = hregAMD64_R9();

   tmpregs[0] = tmpregs[1] = tmpregs[2] =
   tmpregs[3] = tmpregs[4] = tmpregs[5] = INVALID_HREG;

   fastinstrs[0] = fastinstrs[1] = fastinstrs[2] =
   fastinstrs[3] = fastinstrs[4] = fastinstrs[5] = NULL;

   /* First decide which scheme (slow or fast) is to be used.  First
      assume the fast scheme, and select slow if any contraindications
      (wow) appear. */

   if (guard) {
      if (guard->tag == Iex_Const 
          && guard->Iex.Const.con->tag == Ico_U1
          && guard->Iex.Const.con->Ico.U1 == True) {
         /* unconditional */
      } else {
         /* Not manifestly unconditional -- be conservative. */
         goto slowscheme;
      }
   }

   /* Ok, let's try for the fast scheme.  If it doesn't pan out, we'll
      use the slow scheme.  Because this is tentative, we can't call
      addInstr (that is, commit to) any instructions until we're
      handled all the arguments.  So park the resulting instructions
      in a buffer and emit that if we're successful. */

   /* FAST SCHEME */
   argreg = 0;
   if (passBBP) {
      fastinstrs[argreg] = mk_iMOVsd_RR( hregAMD64_RBP(), argregs[argreg]);
      argreg++;
   }

   for (i = 0; i < n_args; i++) {
      vassert(argreg < 6);
      vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
      fastinstrs[argreg] 
         = iselIntExpr_single_instruction( env, argregs[argreg], args[i] );
      if (fastinstrs[argreg] == NULL)
         goto slowscheme;
      argreg++;
   }

   /* Looks like we're in luck.  Emit the accumulated instructions and
      move on to doing the call itself. */
   vassert(argreg <= 6);
   for (i = 0; i < argreg; i++)
      addInstr(env, fastinstrs[i]);

   /* Fast scheme only applies for unconditional calls.  Hence: */
   cc = Acc_ALWAYS;

   goto handle_call;


   /* SLOW SCHEME; move via temporaries */
  slowscheme:
#  if 0 /* debug only */
   if (n_args > 0) {for (i = 0; args[i]; i++) {
   ppIRExpr(args[i]); vex_printf(" "); }
   vex_printf("\n");}
#  endif
   argreg = 0;

   if (passBBP) {
      /* This is pretty stupid; better to move directly to rdi
         after the rest of the args are done. */
      tmpregs[argreg] = newVRegI(env);
      addInstr(env, mk_iMOVsd_RR( hregAMD64_RBP(), tmpregs[argreg]));
      argreg++;
   }

   for (i = 0; i < n_args; i++) {
      vassert(argreg < 6);
      vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
      tmpregs[argreg] = iselIntExpr_R(env, args[i]);
      argreg++;
   }

   /* Now we can compute the condition.  We can't do it earlier
      because the argument computations could trash the condition
      codes.  Be a bit clever to handle the common case where the
      guard is 1:Bit. */
   cc = Acc_ALWAYS;
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
      /* None of these insns, including any spill code that might
         be generated, may alter the condition codes. */
      addInstr( env, mk_iMOVsd_RR( tmpregs[i], argregs[i] ) );
   }


   /* Finally, the call itself. */
  handle_call:
   addInstr(env, AMD64Instr_Call( 
                    cc, 
                    Ptr_to_ULong(cee->addr), 
                    n_args + (passBBP ? 1 : 0) 
                 )
   );
}


/* Given a guest-state array descriptor, an index expression and a
   bias, generate an AMD64AMode holding the relevant guest state
   offset. */

static
AMD64AMode* genGuestArrayOffset ( ISelEnv* env, IRRegArray* descr, 
                                  IRExpr* off, Int bias )
{
   HReg tmp, roff;
   Int  elemSz = sizeofIRType(descr->elemTy);
   Int  nElems = descr->nElems;

   /* Throw out any cases not generated by an amd64 front end.  In
      theory there might be a day where we need to handle them -- if
      we ever run non-amd64-guest on amd64 host. */

   if (nElems != 8 || (elemSz != 1 && elemSz != 8))
      vpanic("genGuestArrayOffset(amd64 host)");

   /* Compute off into a reg, %off.  Then return:

         movq %off, %tmp
         addq $bias, %tmp  (if bias != 0)
         andq %tmp, 7
         ... base(%rbp, %tmp, shift) ...
   */
   tmp  = newVRegI(env);
   roff = iselIntExpr_R(env, off);
   addInstr(env, mk_iMOVsd_RR(roff, tmp));
   if (bias != 0) {
      /* Make sure the bias is sane, in the sense that there are
         no significant bits above bit 30 in it. */
      vassert(-10000 < bias && bias < 10000);
      addInstr(env, 
               AMD64Instr_Alu64R(Aalu_ADD, AMD64RMI_Imm(bias), tmp));
   }
   addInstr(env, 
            AMD64Instr_Alu64R(Aalu_AND, AMD64RMI_Imm(7), tmp));
   vassert(elemSz == 1 || elemSz == 8);
   return
      AMD64AMode_IRRS( descr->base, hregAMD64_RBP(), tmp,
                                    elemSz==8 ? 3 : 0);
}


/* Set the SSE unit's rounding mode to default (%mxcsr = 0x1F80) */
static
void set_SSE_rounding_default ( ISelEnv* env )
{
   /* pushq $DEFAULT_MXCSR 
      ldmxcsr 0(%rsp)
      addq $8, %rsp
   */
   AMD64AMode* zero_rsp = AMD64AMode_IR(0, hregAMD64_RSP());
   addInstr(env, AMD64Instr_Push(AMD64RMI_Imm(DEFAULT_MXCSR)));
   addInstr(env, AMD64Instr_LdMXCSR(zero_rsp));
   add_to_rsp(env, 8);
}

/* Mess with the FPU's rounding mode: set to the default rounding mode
   (DEFAULT_FPUCW). */
static 
void set_FPU_rounding_default ( ISelEnv* env )
{
   /* movq $DEFAULT_FPUCW, -8(%rsp)
      fldcw -8(%esp)
   */
   AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
   addInstr(env, AMD64Instr_Alu64M(
                    Aalu_MOV, AMD64RI_Imm(DEFAULT_FPUCW), m8_rsp));
   addInstr(env, AMD64Instr_A87LdCW(m8_rsp));
}


/* Mess with the SSE unit's rounding mode: 'mode' is an I32-typed
   expression denoting a value in the range 0 .. 3, indicating a round
   mode encoded as per type IRRoundingMode.  Set the SSE machinery to
   have the same rounding.
*/
static
void set_SSE_rounding_mode ( ISelEnv* env, IRExpr* mode )
{
   /* Note: this sequence only makes sense because DEFAULT_MXCSR has
      both rounding bits == 0.  If that wasn't the case, we couldn't
      create a new rounding field simply by ORing the new value into
      place. */

   /* movq $3, %reg
      andq [[mode]], %reg  -- shouldn't be needed; paranoia
      shlq $13, %reg
      orq $DEFAULT_MXCSR, %reg
      pushq %reg
      ldmxcsr 0(%esp)
      addq $8, %rsp
   */      
   HReg        reg      = newVRegI(env);
   AMD64AMode* zero_rsp = AMD64AMode_IR(0, hregAMD64_RSP());
   addInstr(env, AMD64Instr_Alu64R(Aalu_MOV, AMD64RMI_Imm(3), reg));
   addInstr(env, AMD64Instr_Alu64R(Aalu_AND,
                                   iselIntExpr_RMI(env, mode), reg));
   addInstr(env, AMD64Instr_Sh64(Ash_SHL, 13, reg));
   addInstr(env, AMD64Instr_Alu64R(
                    Aalu_OR, AMD64RMI_Imm(DEFAULT_MXCSR), reg));
   addInstr(env, AMD64Instr_Push(AMD64RMI_Reg(reg)));
   addInstr(env, AMD64Instr_LdMXCSR(zero_rsp));
   add_to_rsp(env, 8);
}


/* Mess with the FPU's rounding mode: 'mode' is an I32-typed
   expression denoting a value in the range 0 .. 3, indicating a round
   mode encoded as per type IRRoundingMode.  Set the x87 FPU to have
   the same rounding.
*/
static
void set_FPU_rounding_mode ( ISelEnv* env, IRExpr* mode )
{
   HReg rrm  = iselIntExpr_R(env, mode);
   HReg rrm2 = newVRegI(env);
   AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());

   /* movq  %rrm, %rrm2
      andq  $3, %rrm2   -- shouldn't be needed; paranoia
      shlq  $10, %rrm2
      orq   $DEFAULT_FPUCW, %rrm2
      movq  %rrm2, -8(%rsp)
      fldcw -8(%esp)
   */
   addInstr(env, mk_iMOVsd_RR(rrm, rrm2));
   addInstr(env, AMD64Instr_Alu64R(Aalu_AND, AMD64RMI_Imm(3), rrm2));
   addInstr(env, AMD64Instr_Sh64(Ash_SHL, 10, rrm2));
   addInstr(env, AMD64Instr_Alu64R(Aalu_OR, 
                                   AMD64RMI_Imm(DEFAULT_FPUCW), rrm2));
   addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, 
                                   AMD64RI_Reg(rrm2), m8_rsp));
   addInstr(env, AMD64Instr_A87LdCW(m8_rsp));
}


/* Generate all-zeroes into a new vector register.
*/
static HReg generate_zeroes_V128 ( ISelEnv* env )
{
   HReg dst = newVRegV(env);
   addInstr(env, AMD64Instr_SseReRg(Asse_XOR, dst, dst));
   return dst;
}

/* Generate all-ones into a new vector register.
*/
static HReg generate_ones_V128 ( ISelEnv* env )
{
   HReg dst = newVRegV(env);
   addInstr(env, AMD64Instr_SseReRg(Asse_CMPEQ32, dst, dst));
   return dst;
}


/* Generate !src into a new vector register.  Amazing that there isn't
   a less crappy way to do this.
*/
static HReg do_sse_NotV128 ( ISelEnv* env, HReg src )
{
   HReg dst = generate_ones_V128(env);
   addInstr(env, AMD64Instr_SseReRg(Asse_XOR, src, dst));
   return dst;
}


/* Expand the given byte into a 64-bit word, by cloning each bit
   8 times. */
static ULong bitmask8_to_bytemask64 ( UShort w8 )
{
   vassert(w8 == (w8 & 0xFF));
   ULong w64 = 0;
   Int i;
   for (i = 0; i < 8; i++) {
      if (w8 & (1<<i))
         w64 |= (0xFFULL << (8 * i));
   }
   return w64;
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

   This should handle expressions of 64, 32, 16 and 8-bit type.  All
   results are returned in a 64-bit register.  For 32-, 16- and 8-bit
   expressions, the upper 32/16/24 bits are arbitrary, so you should
   mask or sign extend partial values if necessary.
*/

static HReg iselIntExpr_R ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselIntExpr_R_wrk(env, e);
   /* sanity checks ... */
#  if 0
   vex_printf("\niselIntExpr_R: "); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(r) == HRcInt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselIntExpr_R_wrk ( ISelEnv* env, IRExpr* e )
{
   /* Used for unary/binary SIMD64 ops. */
   HWord fn = 0;
   Bool second_is_UInt;

   MatchInfo mi;
   DECLARE_PATTERN(p_1Uto8_64to1);
   DECLARE_PATTERN(p_LDle8_then_8Uto64);
   DECLARE_PATTERN(p_LDle16_then_16Uto64);

   IRType ty = typeOfIRExpr(env->type_env,e);
   switch (ty) {
      case Ity_I64: case Ity_I32: case Ity_I16: case Ity_I8: break;
      default: vassert(0);
   }

   switch (e->tag) {

   /* --------- TEMP --------- */
   case Iex_RdTmp: {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   /* --------- LOAD --------- */
   case Iex_Load: {
      HReg dst = newVRegI(env);
      AMD64AMode* amode = iselIntExpr_AMode ( env, e->Iex.Load.addr );

      /* We can't handle big-endian loads, nor load-linked. */
      if (e->Iex.Load.end != Iend_LE)
         goto irreducible;

      if (ty == Ity_I64) {
         addInstr(env, AMD64Instr_Alu64R(Aalu_MOV,
                                         AMD64RMI_Mem(amode), dst) );
         return dst;
      }
      if (ty == Ity_I32) {
         addInstr(env, AMD64Instr_LoadEX(4,False,amode,dst));
         return dst;
      }
      if (ty == Ity_I16) {
         addInstr(env, AMD64Instr_LoadEX(2,False,amode,dst));
         return dst;
      }
      if (ty == Ity_I8) {
         addInstr(env, AMD64Instr_LoadEX(1,False,amode,dst));
         return dst;
      }
      break;
   }

   /* --------- BINARY OP --------- */
   case Iex_Binop: {
      AMD64AluOp   aluOp;
      AMD64ShiftOp shOp;

      /* Pattern: Sub64(0,x) */
      /*     and: Sub32(0,x) */
      if ((e->Iex.Binop.op == Iop_Sub64 && isZeroU64(e->Iex.Binop.arg1))
          || (e->Iex.Binop.op == Iop_Sub32 && isZeroU32(e->Iex.Binop.arg1))) {
         HReg dst = newVRegI(env);
         HReg reg = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(reg,dst));
         addInstr(env, AMD64Instr_Unary64(Aun_NEG,dst));
         return dst;
      }

      /* Is it an addition or logical style op? */
      switch (e->Iex.Binop.op) {
         case Iop_Add8: case Iop_Add16: case Iop_Add32: case Iop_Add64: 
            aluOp = Aalu_ADD; break;
         case Iop_Sub8: case Iop_Sub16: case Iop_Sub32: case Iop_Sub64:
            aluOp = Aalu_SUB; break;
         case Iop_And8: case Iop_And16: case Iop_And32: case Iop_And64: 
            aluOp = Aalu_AND; break;
         case Iop_Or8:  case Iop_Or16:  case Iop_Or32:  case Iop_Or64: 
            aluOp = Aalu_OR; break;
         case Iop_Xor8: case Iop_Xor16: case Iop_Xor32: case Iop_Xor64: 
            aluOp = Aalu_XOR; break;
         case Iop_Mul16: case Iop_Mul32: case Iop_Mul64:
            aluOp = Aalu_MUL; break;
         default:
            aluOp = Aalu_INVALID; break;
      }
      /* For commutative ops we assume any literal
         values are on the second operand. */
      if (aluOp != Aalu_INVALID) {
         HReg dst      = newVRegI(env);
         HReg reg      = iselIntExpr_R(env, e->Iex.Binop.arg1);
         AMD64RMI* rmi = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(reg,dst));
         addInstr(env, AMD64Instr_Alu64R(aluOp, rmi, dst));
         return dst;
      }

      /* Perhaps a shift op? */
      switch (e->Iex.Binop.op) {
         case Iop_Shl64: case Iop_Shl32: case Iop_Shl16: case Iop_Shl8:
            shOp = Ash_SHL; break;
         case Iop_Shr64: case Iop_Shr32: case Iop_Shr16: case Iop_Shr8: 
            shOp = Ash_SHR; break;
         case Iop_Sar64: case Iop_Sar32: case Iop_Sar16: case Iop_Sar8: 
            shOp = Ash_SAR; break;
         default:
            shOp = Ash_INVALID; break;
      }
      if (shOp != Ash_INVALID) {
         HReg dst = newVRegI(env);

         /* regL = the value to be shifted */
         HReg regL   = iselIntExpr_R(env, e->Iex.Binop.arg1);
         addInstr(env, mk_iMOVsd_RR(regL,dst));

         /* Do any necessary widening for 32/16/8 bit operands */
         switch (e->Iex.Binop.op) {
            case Iop_Shr64: case Iop_Shl64: case Iop_Sar64: 
               break;
            case Iop_Shl32: case Iop_Shl16: case Iop_Shl8:
               break;
            case Iop_Shr8:
               addInstr(env, AMD64Instr_Alu64R(
                                Aalu_AND, AMD64RMI_Imm(0xFF), dst));
               break;
            case Iop_Shr16:
               addInstr(env, AMD64Instr_Alu64R(
                                Aalu_AND, AMD64RMI_Imm(0xFFFF), dst));
               break;
            case Iop_Shr32:
               addInstr(env, AMD64Instr_MovxLQ(False, dst, dst));
               break;
            case Iop_Sar8:
               addInstr(env, AMD64Instr_Sh64(Ash_SHL, 56, dst));
               addInstr(env, AMD64Instr_Sh64(Ash_SAR, 56, dst));
               break;
            case Iop_Sar16:
               addInstr(env, AMD64Instr_Sh64(Ash_SHL, 48, dst));
               addInstr(env, AMD64Instr_Sh64(Ash_SAR, 48, dst));
               break;
            case Iop_Sar32:
               addInstr(env, AMD64Instr_MovxLQ(True, dst, dst));
               break;
            default: 
               ppIROp(e->Iex.Binop.op);
               vassert(0);
         }

         /* Now consider the shift amount.  If it's a literal, we
            can do a much better job than the general case. */
         if (e->Iex.Binop.arg2->tag == Iex_Const) {
            /* assert that the IR is well-typed */
            Int nshift;
            vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
            nshift = e->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
            vassert(nshift >= 0);
            if (nshift > 0)
               /* Can't allow nshift==0 since that means %cl */
               addInstr(env, AMD64Instr_Sh64(shOp, nshift, dst));
         } else {
            /* General case; we have to force the amount into %cl. */
            HReg regR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, mk_iMOVsd_RR(regR,hregAMD64_RCX()));
            addInstr(env, AMD64Instr_Sh64(shOp, 0/* %cl */, dst));
         }
         return dst;
      }

      /* Deal with 64-bit SIMD binary ops */
      second_is_UInt = False;
      switch (e->Iex.Binop.op) {
         case Iop_Add8x8:
            fn = (HWord)h_generic_calc_Add8x8; break;
         case Iop_Add16x4:
            fn = (HWord)h_generic_calc_Add16x4; break;
         case Iop_Add32x2:
            fn = (HWord)h_generic_calc_Add32x2; break;

         case Iop_Avg8Ux8:
            fn = (HWord)h_generic_calc_Avg8Ux8; break;
         case Iop_Avg16Ux4:
            fn = (HWord)h_generic_calc_Avg16Ux4; break;

         case Iop_CmpEQ8x8:
            fn = (HWord)h_generic_calc_CmpEQ8x8; break;
         case Iop_CmpEQ16x4:
            fn = (HWord)h_generic_calc_CmpEQ16x4; break;
         case Iop_CmpEQ32x2:
            fn = (HWord)h_generic_calc_CmpEQ32x2; break;

         case Iop_CmpGT8Sx8:
            fn = (HWord)h_generic_calc_CmpGT8Sx8; break;
         case Iop_CmpGT16Sx4:
            fn = (HWord)h_generic_calc_CmpGT16Sx4; break;
         case Iop_CmpGT32Sx2:
            fn = (HWord)h_generic_calc_CmpGT32Sx2; break;

         case Iop_InterleaveHI8x8:
            fn = (HWord)h_generic_calc_InterleaveHI8x8; break;
         case Iop_InterleaveLO8x8:
            fn = (HWord)h_generic_calc_InterleaveLO8x8; break;
         case Iop_InterleaveHI16x4:
            fn = (HWord)h_generic_calc_InterleaveHI16x4; break;
         case Iop_InterleaveLO16x4:
            fn = (HWord)h_generic_calc_InterleaveLO16x4; break;
         case Iop_InterleaveHI32x2:
            fn = (HWord)h_generic_calc_InterleaveHI32x2; break;
         case Iop_InterleaveLO32x2:
            fn = (HWord)h_generic_calc_InterleaveLO32x2; break;
         case Iop_CatOddLanes16x4:
            fn = (HWord)h_generic_calc_CatOddLanes16x4; break;
         case Iop_CatEvenLanes16x4:
            fn = (HWord)h_generic_calc_CatEvenLanes16x4; break;
         case Iop_Perm8x8:
            fn = (HWord)h_generic_calc_Perm8x8; break;

         case Iop_Max8Ux8:
            fn = (HWord)h_generic_calc_Max8Ux8; break;
         case Iop_Max16Sx4:
            fn = (HWord)h_generic_calc_Max16Sx4; break;
         case Iop_Min8Ux8:
            fn = (HWord)h_generic_calc_Min8Ux8; break;
         case Iop_Min16Sx4:
            fn = (HWord)h_generic_calc_Min16Sx4; break;

         case Iop_Mul16x4:
            fn = (HWord)h_generic_calc_Mul16x4; break;
         case Iop_Mul32x2:
            fn = (HWord)h_generic_calc_Mul32x2; break;
         case Iop_MulHi16Sx4:
            fn = (HWord)h_generic_calc_MulHi16Sx4; break;
         case Iop_MulHi16Ux4:
            fn = (HWord)h_generic_calc_MulHi16Ux4; break;

         case Iop_QAdd8Sx8:
            fn = (HWord)h_generic_calc_QAdd8Sx8; break;
         case Iop_QAdd16Sx4:
            fn = (HWord)h_generic_calc_QAdd16Sx4; break;
         case Iop_QAdd8Ux8:
            fn = (HWord)h_generic_calc_QAdd8Ux8; break;
         case Iop_QAdd16Ux4:
            fn = (HWord)h_generic_calc_QAdd16Ux4; break;

         case Iop_QNarrowBin32Sto16Sx4:
            fn = (HWord)h_generic_calc_QNarrowBin32Sto16Sx4; break;
         case Iop_QNarrowBin16Sto8Sx8:
            fn = (HWord)h_generic_calc_QNarrowBin16Sto8Sx8; break;
         case Iop_QNarrowBin16Sto8Ux8:
            fn = (HWord)h_generic_calc_QNarrowBin16Sto8Ux8; break;
         case Iop_NarrowBin16to8x8:
            fn = (HWord)h_generic_calc_NarrowBin16to8x8; break;
         case Iop_NarrowBin32to16x4:
            fn = (HWord)h_generic_calc_NarrowBin32to16x4; break;

         case Iop_QSub8Sx8:
            fn = (HWord)h_generic_calc_QSub8Sx8; break;
         case Iop_QSub16Sx4:
            fn = (HWord)h_generic_calc_QSub16Sx4; break;
         case Iop_QSub8Ux8:
            fn = (HWord)h_generic_calc_QSub8Ux8; break;
         case Iop_QSub16Ux4:
            fn = (HWord)h_generic_calc_QSub16Ux4; break;

         case Iop_Sub8x8:
            fn = (HWord)h_generic_calc_Sub8x8; break;
         case Iop_Sub16x4:
            fn = (HWord)h_generic_calc_Sub16x4; break;
         case Iop_Sub32x2:
            fn = (HWord)h_generic_calc_Sub32x2; break;

         case Iop_ShlN32x2:
            fn = (HWord)h_generic_calc_ShlN32x2; 
            second_is_UInt = True;
            break;
         case Iop_ShlN16x4:
            fn = (HWord)h_generic_calc_ShlN16x4;
            second_is_UInt = True;
            break;
         case Iop_ShlN8x8:
            fn = (HWord)h_generic_calc_ShlN8x8;
            second_is_UInt = True;
            break;
         case Iop_ShrN32x2:
            fn = (HWord)h_generic_calc_ShrN32x2; 
            second_is_UInt = True; 
            break;
         case Iop_ShrN16x4:
            fn = (HWord)h_generic_calc_ShrN16x4;
            second_is_UInt = True; 
            break;
         case Iop_SarN32x2:
            fn = (HWord)h_generic_calc_SarN32x2;
            second_is_UInt = True; 
            break;
         case Iop_SarN16x4:
            fn = (HWord)h_generic_calc_SarN16x4;
            second_is_UInt = True; 
            break;
         case Iop_SarN8x8:
            fn = (HWord)h_generic_calc_SarN8x8;
            second_is_UInt = True; 
            break;

         default:
            fn = (HWord)0; break;
      }
      if (fn != (HWord)0) {
         /* Note: the following assumes all helpers are of signature 
               ULong fn ( ULong, ULong ), and they are
            not marked as regparm functions. 
         */
         HReg dst  = newVRegI(env);
         HReg argL = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         if (second_is_UInt)
            addInstr(env, AMD64Instr_MovxLQ(False, argR, argR));
         addInstr(env, mk_iMOVsd_RR(argL, hregAMD64_RDI()) );
         addInstr(env, mk_iMOVsd_RR(argR, hregAMD64_RSI()) );
         addInstr(env, AMD64Instr_Call( Acc_ALWAYS, (ULong)fn, 2 ));
         addInstr(env, mk_iMOVsd_RR(hregAMD64_RAX(), dst));
         return dst;
      }

      /* Handle misc other ops. */

      if (e->Iex.Binop.op == Iop_Max32U) {
         HReg src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg dst  = newVRegI(env);
         HReg src2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(src1, dst));
         addInstr(env, AMD64Instr_Alu32R(Aalu_CMP, AMD64RMI_Reg(src2), dst));
         addInstr(env, AMD64Instr_CMov64(Acc_B, AMD64RM_Reg(src2), dst));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_DivModS64to32
          || e->Iex.Binop.op == Iop_DivModU64to32) {
         /* 64 x 32 -> (32(rem),32(div)) division */
         /* Get the 64-bit operand into edx:eax, and the other into
            any old R/M. */
         HReg      rax     = hregAMD64_RAX();
         HReg      rdx     = hregAMD64_RDX();
         HReg      dst     = newVRegI(env);
         Bool      syned   = toBool(e->Iex.Binop.op == Iop_DivModS64to32);
         AMD64RM*  rmRight = iselIntExpr_RM(env, e->Iex.Binop.arg2);
         /* Compute the left operand into a reg, and then 
            put the top half in edx and the bottom in eax. */
         HReg left64 = iselIntExpr_R(env, e->Iex.Binop.arg1);
         addInstr(env, mk_iMOVsd_RR(left64, rdx));
         addInstr(env, mk_iMOVsd_RR(left64, rax));
         addInstr(env, AMD64Instr_Sh64(Ash_SHR, 32, rdx));
         addInstr(env, AMD64Instr_Div(syned, 4, rmRight));
	 addInstr(env, AMD64Instr_MovxLQ(False, rdx, rdx));
	 addInstr(env, AMD64Instr_MovxLQ(False, rax, rax));
         addInstr(env, AMD64Instr_Sh64(Ash_SHL, 32, rdx));
         addInstr(env, mk_iMOVsd_RR(rax, dst));
         addInstr(env, AMD64Instr_Alu64R(Aalu_OR, AMD64RMI_Reg(rdx), dst));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_32HLto64) {
         HReg hi32  = newVRegI(env);
         HReg lo32  = newVRegI(env);
         HReg hi32s = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg lo32s = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(hi32s, hi32));
         addInstr(env, mk_iMOVsd_RR(lo32s, lo32));
         addInstr(env, AMD64Instr_Sh64(Ash_SHL, 32, hi32));
	 addInstr(env, AMD64Instr_MovxLQ(False, lo32, lo32));
         addInstr(env, AMD64Instr_Alu64R(
                          Aalu_OR, AMD64RMI_Reg(lo32), hi32));
         return hi32;
      }

      if (e->Iex.Binop.op == Iop_16HLto32) {
         HReg hi16  = newVRegI(env);
         HReg lo16  = newVRegI(env);
         HReg hi16s = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg lo16s = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(hi16s, hi16));
         addInstr(env, mk_iMOVsd_RR(lo16s, lo16));
         addInstr(env, AMD64Instr_Sh64(Ash_SHL, 16, hi16));
         addInstr(env, AMD64Instr_Alu64R(
                          Aalu_AND, AMD64RMI_Imm(0xFFFF), lo16));
         addInstr(env, AMD64Instr_Alu64R(
                          Aalu_OR, AMD64RMI_Reg(lo16), hi16));
         return hi16;
      }

      if (e->Iex.Binop.op == Iop_8HLto16) {
         HReg hi8  = newVRegI(env);
         HReg lo8  = newVRegI(env);
         HReg hi8s = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg lo8s = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(hi8s, hi8));
         addInstr(env, mk_iMOVsd_RR(lo8s, lo8));
         addInstr(env, AMD64Instr_Sh64(Ash_SHL, 8, hi8));
         addInstr(env, AMD64Instr_Alu64R(
                          Aalu_AND, AMD64RMI_Imm(0xFF), lo8));
         addInstr(env, AMD64Instr_Alu64R(
                          Aalu_OR, AMD64RMI_Reg(lo8), hi8));
         return hi8;
      }

      if (e->Iex.Binop.op == Iop_MullS32
          || e->Iex.Binop.op == Iop_MullS16
          || e->Iex.Binop.op == Iop_MullS8
          || e->Iex.Binop.op == Iop_MullU32 
          || e->Iex.Binop.op == Iop_MullU16 
          || e->Iex.Binop.op == Iop_MullU8) {
         HReg a32   = newVRegI(env);
         HReg b32   = newVRegI(env);
         HReg a32s  = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg b32s  = iselIntExpr_R(env, e->Iex.Binop.arg2);
         Int          shift  = 0;
         AMD64ShiftOp shr_op = Ash_SHR;
         switch (e->Iex.Binop.op) {
            case Iop_MullS32: shr_op = Ash_SAR; shift = 32; break;
            case Iop_MullS16: shr_op = Ash_SAR; shift = 48; break;
            case Iop_MullS8:  shr_op = Ash_SAR; shift = 56; break;
            case Iop_MullU32: shr_op = Ash_SHR; shift = 32; break;
            case Iop_MullU16: shr_op = Ash_SHR; shift = 48; break;
            case Iop_MullU8:  shr_op = Ash_SHR; shift = 56; break;
            default: vassert(0);
         }

         addInstr(env, mk_iMOVsd_RR(a32s, a32));
         addInstr(env, mk_iMOVsd_RR(b32s, b32));
         addInstr(env, AMD64Instr_Sh64(Ash_SHL, shift, a32));
         addInstr(env, AMD64Instr_Sh64(Ash_SHL, shift, b32));
         addInstr(env, AMD64Instr_Sh64(shr_op,  shift, a32));
         addInstr(env, AMD64Instr_Sh64(shr_op,  shift, b32));
         addInstr(env, AMD64Instr_Alu64R(Aalu_MUL, AMD64RMI_Reg(a32), b32));
         return b32;
      }

      if (e->Iex.Binop.op == Iop_CmpF64) {
         HReg fL = iselDblExpr(env, e->Iex.Binop.arg1);
         HReg fR = iselDblExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegI(env);
         addInstr(env, AMD64Instr_SseUComIS(8,fL,fR,dst));
         /* Mask out irrelevant parts of the result so as to conform
            to the CmpF64 definition. */
         addInstr(env, AMD64Instr_Alu64R(Aalu_AND, AMD64RMI_Imm(0x45), dst));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_F64toI32S
          || e->Iex.Binop.op == Iop_F64toI64S) {
         Int  szD = e->Iex.Binop.op==Iop_F64toI32S ? 4 : 8;
         HReg rf  = iselDblExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegI(env);
         set_SSE_rounding_mode( env, e->Iex.Binop.arg1 );
         addInstr(env, AMD64Instr_SseSF2SI( 8, szD, rf, dst ));
         set_SSE_rounding_default(env);
         return dst;
      }

      break;
   }

   /* --------- UNARY OP --------- */
   case Iex_Unop: {

      /* 1Uto8(64to1(expr64)) */
      {
         DEFINE_PATTERN( p_1Uto8_64to1,
                         unop(Iop_1Uto8, unop(Iop_64to1, bind(0))) );
         if (matchIRExpr(&mi,p_1Uto8_64to1,e)) {
            IRExpr* expr64 = mi.bindee[0];
            HReg    dst    = newVRegI(env);
            HReg    src    = iselIntExpr_R(env, expr64);
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, AMD64Instr_Alu64R(Aalu_AND,
                                            AMD64RMI_Imm(1), dst));
            return dst;
         }
      }

      /* 8Uto64(LDle(expr64)) */
      {
         DEFINE_PATTERN(p_LDle8_then_8Uto64,
                        unop(Iop_8Uto64,
                             IRExpr_Load(Iend_LE,Ity_I8,bind(0))) );
         if (matchIRExpr(&mi,p_LDle8_then_8Uto64,e)) {
            HReg dst = newVRegI(env);
            AMD64AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
            addInstr(env, AMD64Instr_LoadEX(1,False,amode,dst));
            return dst;
         }
      }

      /* 16Uto64(LDle(expr64)) */
      {
         DEFINE_PATTERN(p_LDle16_then_16Uto64,
                        unop(Iop_16Uto64,
                             IRExpr_Load(Iend_LE,Ity_I16,bind(0))) );
         if (matchIRExpr(&mi,p_LDle16_then_16Uto64,e)) {
            HReg dst = newVRegI(env);
            AMD64AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
            addInstr(env, AMD64Instr_LoadEX(2,False,amode,dst));
            return dst;
         }
      }

      /* 32Uto64( Add32/Sub32/And32/Or32/Xor32(expr32, expr32) )
         Use 32 bit arithmetic and let the default zero-extend rule
         do the 32Uto64 for free. */
      if (e->Iex.Unop.op == Iop_32Uto64 && e->Iex.Unop.arg->tag == Iex_Binop) {
         IROp    opi  = e->Iex.Unop.arg->Iex.Binop.op; /* inner op */
         IRExpr* argL = e->Iex.Unop.arg->Iex.Binop.arg1;
         IRExpr* argR = e->Iex.Unop.arg->Iex.Binop.arg2;
         AMD64AluOp aluOp = Aalu_INVALID;
         switch (opi) {
            case Iop_Add32: aluOp = Aalu_ADD; break;
            case Iop_Sub32: aluOp = Aalu_SUB; break;
            case Iop_And32: aluOp = Aalu_AND; break;
            case Iop_Or32:  aluOp = Aalu_OR;  break;
            case Iop_Xor32: aluOp = Aalu_XOR; break;
            default: break;
         }
         if (aluOp != Aalu_INVALID) {
            /* For commutative ops we assume any literal values are on
               the second operand. */
            HReg dst      = newVRegI(env);
            HReg reg      = iselIntExpr_R(env, argL);
            AMD64RMI* rmi = iselIntExpr_RMI(env, argR);
            addInstr(env, mk_iMOVsd_RR(reg,dst));
            addInstr(env, AMD64Instr_Alu32R(aluOp, rmi, dst));
            return dst;
         }
         /* just fall through to normal handling for Iop_32Uto64 */
      }

      /* Fallback cases */
      switch (e->Iex.Unop.op) {
         case Iop_32Uto64:
         case Iop_32Sto64: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, AMD64Instr_MovxLQ(e->Iex.Unop.op == Iop_32Sto64,
                                            src, dst) );
            return dst;
         }
         case Iop_128HIto64: {
            HReg rHi, rLo;
            iselInt128Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rHi; /* and abandon rLo */
         }
         case Iop_128to64: {
            HReg rHi, rLo;
            iselInt128Expr(&rHi,&rLo, env, e->Iex.Unop.arg);
            return rLo; /* and abandon rHi */
         }
         case Iop_8Uto16:
         case Iop_8Uto32:
         case Iop_8Uto64:
         case Iop_16Uto64:
         case Iop_16Uto32: {
            HReg dst     = newVRegI(env);
            HReg src     = iselIntExpr_R(env, e->Iex.Unop.arg);
            Bool srcIs16 = toBool( e->Iex.Unop.op==Iop_16Uto32
                                   || e->Iex.Unop.op==Iop_16Uto64 );
            UInt mask    = srcIs16 ? 0xFFFF : 0xFF;
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, AMD64Instr_Alu64R(Aalu_AND,
                                            AMD64RMI_Imm(mask), dst));
            return dst;
         }
         case Iop_8Sto16:
         case Iop_8Sto64:
         case Iop_8Sto32:
         case Iop_16Sto32:
         case Iop_16Sto64: {
            HReg dst     = newVRegI(env);
            HReg src     = iselIntExpr_R(env, e->Iex.Unop.arg);
            Bool srcIs16 = toBool( e->Iex.Unop.op==Iop_16Sto32
                                   || e->Iex.Unop.op==Iop_16Sto64 );
            UInt amt     = srcIs16 ? 48 : 56;
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, AMD64Instr_Sh64(Ash_SHL, amt, dst));
            addInstr(env, AMD64Instr_Sh64(Ash_SAR, amt, dst));
            return dst;
         }
 	 case Iop_Not8:
 	 case Iop_Not16:
         case Iop_Not32:
         case Iop_Not64: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, AMD64Instr_Unary64(Aun_NOT,dst));
            return dst;
         }
         case Iop_16HIto8:
         case Iop_32HIto16:
         case Iop_64HIto32: {
            HReg dst  = newVRegI(env);
            HReg src  = iselIntExpr_R(env, e->Iex.Unop.arg);
            Int shift = 0;
            switch (e->Iex.Unop.op) {
               case Iop_16HIto8:  shift = 8;  break;
               case Iop_32HIto16: shift = 16; break;
               case Iop_64HIto32: shift = 32; break;
               default: vassert(0);
            }
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, AMD64Instr_Sh64(Ash_SHR, shift, dst));
            return dst;
         }
         case Iop_1Uto64:
         case Iop_1Uto32:
         case Iop_1Uto8: {
            HReg dst           = newVRegI(env);
            AMD64CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, AMD64Instr_Set64(cond,dst));
            return dst;
         }
         case Iop_1Sto8:
         case Iop_1Sto16:
         case Iop_1Sto32:
         case Iop_1Sto64: {
            /* could do better than this, but for now ... */
            HReg dst           = newVRegI(env);
            AMD64CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, AMD64Instr_Set64(cond,dst));
            addInstr(env, AMD64Instr_Sh64(Ash_SHL, 63, dst));
            addInstr(env, AMD64Instr_Sh64(Ash_SAR, 63, dst));
            return dst;
         }
         case Iop_Ctz64: {
            /* Count trailing zeroes, implemented by amd64 'bsfq' */
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, AMD64Instr_Bsfr64(True,src,dst));
            return dst;
         }
         case Iop_Clz64: {
            /* Count leading zeroes.  Do 'bsrq' to establish the index
               of the highest set bit, and subtract that value from
               63. */
            HReg tmp = newVRegI(env);
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, AMD64Instr_Bsfr64(False,src,tmp));
            addInstr(env, AMD64Instr_Alu64R(Aalu_MOV, 
                                            AMD64RMI_Imm(63), dst));
            addInstr(env, AMD64Instr_Alu64R(Aalu_SUB,
                                            AMD64RMI_Reg(tmp), dst));
            return dst;
         }

         case Iop_CmpwNEZ64: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src,dst));
            addInstr(env, AMD64Instr_Unary64(Aun_NEG,dst));
            addInstr(env, AMD64Instr_Alu64R(Aalu_OR,
                                            AMD64RMI_Reg(src), dst));
            addInstr(env, AMD64Instr_Sh64(Ash_SAR, 63, dst));
            return dst;
         }

         case Iop_CmpwNEZ32: {
            HReg src = newVRegI(env);
            HReg dst = newVRegI(env);
            HReg pre = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(pre,src));
            addInstr(env, AMD64Instr_MovxLQ(False, src, src));
            addInstr(env, mk_iMOVsd_RR(src,dst));
            addInstr(env, AMD64Instr_Unary64(Aun_NEG,dst));
            addInstr(env, AMD64Instr_Alu64R(Aalu_OR,
                                            AMD64RMI_Reg(src), dst));
            addInstr(env, AMD64Instr_Sh64(Ash_SAR, 63, dst));
            return dst;
         }

         case Iop_Left8:
         case Iop_Left16:
         case Iop_Left32:
         case Iop_Left64: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src, dst));
            addInstr(env, AMD64Instr_Unary64(Aun_NEG, dst));
            addInstr(env, AMD64Instr_Alu64R(Aalu_OR, AMD64RMI_Reg(src), dst));
            return dst;
         }

         case Iop_V128to32: {
            HReg        dst     = newVRegI(env);
            HReg        vec     = iselVecExpr(env, e->Iex.Unop.arg);
            AMD64AMode* rsp_m16 = AMD64AMode_IR(-16, hregAMD64_RSP());
            addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, vec, rsp_m16));
            addInstr(env, AMD64Instr_LoadEX(4, False/*z-widen*/, rsp_m16, dst));
            return dst;
         }

         /* V128{HI}to64 */
         case Iop_V128HIto64:
         case Iop_V128to64: {
            HReg dst = newVRegI(env);
            Int  off = e->Iex.Unop.op==Iop_V128HIto64 ? -8 : -16;
            HReg rsp = hregAMD64_RSP();
            HReg vec = iselVecExpr(env, e->Iex.Unop.arg);
            AMD64AMode* m16_rsp = AMD64AMode_IR(-16, rsp);
            AMD64AMode* off_rsp = AMD64AMode_IR(off, rsp);
            addInstr(env, AMD64Instr_SseLdSt(False/*store*/,
                                             16, vec, m16_rsp));
            addInstr(env, AMD64Instr_Alu64R( Aalu_MOV, 
                                             AMD64RMI_Mem(off_rsp), dst ));
            return dst;
         }

         case Iop_V256to64_0: case Iop_V256to64_1:
         case Iop_V256to64_2: case Iop_V256to64_3: {
            HReg vHi, vLo, vec;
            iselDVecExpr(&vHi, &vLo, env, e->Iex.Unop.arg);
            /* Do the first part of the selection by deciding which of
               the 128 bit registers do look at, and second part using
               the same scheme as for V128{HI}to64 above. */
            Int off = 0;
            switch (e->Iex.Unop.op) {
               case Iop_V256to64_0: vec = vLo; off = -16; break;
               case Iop_V256to64_1: vec = vLo; off =  -8; break;
               case Iop_V256to64_2: vec = vHi; off = -16; break;
               case Iop_V256to64_3: vec = vHi; off =  -8; break;
               default: vassert(0);
            }
            HReg        dst     = newVRegI(env);
            HReg        rsp     = hregAMD64_RSP();
            AMD64AMode* m16_rsp = AMD64AMode_IR(-16, rsp);
            AMD64AMode* off_rsp = AMD64AMode_IR(off, rsp);
            addInstr(env, AMD64Instr_SseLdSt(False/*store*/,
                                             16, vec, m16_rsp));
            addInstr(env, AMD64Instr_Alu64R( Aalu_MOV, 
                                             AMD64RMI_Mem(off_rsp), dst ));
            return dst;
         }

         /* ReinterpF64asI64(e) */
         /* Given an IEEE754 double, produce an I64 with the same bit
            pattern. */
         case Iop_ReinterpF64asI64: {
            AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
            HReg        dst    = newVRegI(env);
            HReg        src    = iselDblExpr(env, e->Iex.Unop.arg);
            /* paranoia */
            set_SSE_rounding_default(env);
            addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 8, src, m8_rsp));
            addInstr(env, AMD64Instr_Alu64R(
                             Aalu_MOV, AMD64RMI_Mem(m8_rsp), dst));
            return dst;
         }

         /* ReinterpF32asI32(e) */
         /* Given an IEEE754 single, produce an I64 with the same bit
            pattern in the lower half. */
         case Iop_ReinterpF32asI32: {
            AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
            HReg        dst    = newVRegI(env);
            HReg        src    = iselFltExpr(env, e->Iex.Unop.arg);
            /* paranoia */
            set_SSE_rounding_default(env);
            addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 4, src, m8_rsp));
            addInstr(env, AMD64Instr_LoadEX(4, False/*unsigned*/, m8_rsp, dst ));
            return dst;
         }

         case Iop_16to8:
         case Iop_32to8:
         case Iop_64to8:
         case Iop_32to16:
         case Iop_64to16:
         case Iop_64to32:
            /* These are no-ops. */
            return iselIntExpr_R(env, e->Iex.Unop.arg);

         default: 
            break;
      }

      /* Deal with unary 64-bit SIMD ops. */
      switch (e->Iex.Unop.op) {
         case Iop_CmpNEZ32x2:
            fn = (HWord)h_generic_calc_CmpNEZ32x2; break;
         case Iop_CmpNEZ16x4:
            fn = (HWord)h_generic_calc_CmpNEZ16x4; break;
         case Iop_CmpNEZ8x8:
            fn = (HWord)h_generic_calc_CmpNEZ8x8; break;
         default:
            fn = (HWord)0; break;
      }
      if (fn != (HWord)0) {
         /* Note: the following assumes all helpers are of
            signature 
               ULong fn ( ULong ), and they are
            not marked as regparm functions. 
         */
         HReg dst = newVRegI(env);
         HReg arg = iselIntExpr_R(env, e->Iex.Unop.arg);
         addInstr(env, mk_iMOVsd_RR(arg, hregAMD64_RDI()) );
         addInstr(env, AMD64Instr_Call( Acc_ALWAYS, (ULong)fn, 1 ));
         addInstr(env, mk_iMOVsd_RR(hregAMD64_RAX(), dst));
         return dst;
      }

      break;
   }

   /* --------- GET --------- */
   case Iex_Get: {
      if (ty == Ity_I64) {
         HReg dst = newVRegI(env);
         addInstr(env, AMD64Instr_Alu64R(
                          Aalu_MOV, 
                          AMD64RMI_Mem(
                             AMD64AMode_IR(e->Iex.Get.offset,
                                           hregAMD64_RBP())),
                          dst));
         return dst;
      }
      if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32) {
         HReg dst = newVRegI(env);
         addInstr(env, AMD64Instr_LoadEX(
                          toUChar(ty==Ity_I8 ? 1 : (ty==Ity_I16 ? 2 : 4)),
                          False,
                          AMD64AMode_IR(e->Iex.Get.offset,hregAMD64_RBP()),
                          dst));
         return dst;
      }
      break;
   }

   case Iex_GetI: {
      AMD64AMode* am 
         = genGuestArrayOffset(
              env, e->Iex.GetI.descr, 
                   e->Iex.GetI.ix, e->Iex.GetI.bias );
      HReg dst = newVRegI(env);
      if (ty == Ity_I8) {
         addInstr(env, AMD64Instr_LoadEX( 1, False, am, dst ));
         return dst;
      }
      if (ty == Ity_I64) {
         addInstr(env, AMD64Instr_Alu64R( Aalu_MOV, AMD64RMI_Mem(am), dst ));
         return dst;
      }
      break;
   }

   /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg    dst = newVRegI(env);
      vassert(ty == e->Iex.CCall.retty);

      /* be very restrictive for now.  Only 64-bit ints allowed
         for args, and 64 or 32 bits for return type. */
      if (e->Iex.CCall.retty != Ity_I64 && e->Iex.CCall.retty != Ity_I32)
         goto irreducible;

      /* Marshal args, do the call. */
      doHelperCall( env, False, NULL, e->Iex.CCall.cee, e->Iex.CCall.args );

      /* Move to dst, and zero out the top 32 bits if the result type is
         Ity_I32.  Probably overkill, but still .. */
      if (e->Iex.CCall.retty == Ity_I64)
         addInstr(env, mk_iMOVsd_RR(hregAMD64_RAX(), dst));
      else
         addInstr(env, AMD64Instr_MovxLQ(False, hregAMD64_RAX(), dst));

      return dst;
   }

   /* --------- LITERAL --------- */
   /* 64/32/16/8-bit literals */
   case Iex_Const:
      if (ty == Ity_I64) {
         HReg r = newVRegI(env);
         addInstr(env, AMD64Instr_Imm64(e->Iex.Const.con->Ico.U64, r));
         return r;
      } else {
         AMD64RMI* rmi = iselIntExpr_RMI ( env, e );
         HReg      r   = newVRegI(env);
         addInstr(env, AMD64Instr_Alu64R(Aalu_MOV, rmi, r));
         return r;
      }

   /* --------- MULTIPLEX --------- */
   case Iex_Mux0X: {
     if ((ty == Ity_I64 || ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8)
         && typeOfIRExpr(env->type_env,e->Iex.Mux0X.cond) == Ity_I8) {
        HReg     r8;
        HReg     rX  = iselIntExpr_R(env, e->Iex.Mux0X.exprX);
        AMD64RM* r0  = iselIntExpr_RM(env, e->Iex.Mux0X.expr0);
        HReg dst = newVRegI(env);
        addInstr(env, mk_iMOVsd_RR(rX,dst));
        r8 = iselIntExpr_R(env, e->Iex.Mux0X.cond);
        addInstr(env, AMD64Instr_Test64(0xFF, r8));
        addInstr(env, AMD64Instr_CMov64(Acc_Z,r0,dst));
        return dst;
      }
      break;
   }

   /* --------- TERNARY OP --------- */
   case Iex_Triop: {
      IRTriop *triop = e->Iex.Triop.details;
      /* C3210 flags following FPU partial remainder (fprem), both
         IEEE compliant (PREM1) and non-IEEE compliant (PREM). */
      if (triop->op == Iop_PRemC3210F64
          || triop->op == Iop_PRem1C3210F64) {
         AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
         HReg        arg1   = iselDblExpr(env, triop->arg2);
         HReg        arg2   = iselDblExpr(env, triop->arg3);
         HReg        dst    = newVRegI(env);
         addInstr(env, AMD64Instr_A87Free(2));

         /* one arg -> top of x87 stack */
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 8, arg2, m8_rsp));
         addInstr(env, AMD64Instr_A87PushPop(m8_rsp, True/*push*/, 8));

         /* other arg -> top of x87 stack */
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 8, arg1, m8_rsp));
         addInstr(env, AMD64Instr_A87PushPop(m8_rsp, True/*push*/, 8));

         switch (triop->op) {
            case Iop_PRemC3210F64:
               addInstr(env, AMD64Instr_A87FpOp(Afp_PREM));
               break;
            case Iop_PRem1C3210F64:
               addInstr(env, AMD64Instr_A87FpOp(Afp_PREM1));
               break;
            default: 
               vassert(0);
         }
         /* Ignore the result, and instead make off with the FPU's
	    C3210 flags (in the status word). */
         addInstr(env, AMD64Instr_A87StSW(m8_rsp));
         addInstr(env, AMD64Instr_Alu64R(Aalu_MOV,AMD64RMI_Mem(m8_rsp),dst));
         addInstr(env, AMD64Instr_Alu64R(Aalu_AND,AMD64RMI_Imm(0x4700),dst));
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
   vpanic("iselIntExpr_R(amd64): cannot reduce tree");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expression auxiliaries              ---*/
/*---------------------------------------------------------*/

/* --------------------- AMODEs --------------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a 32-bit one.
*/

static AMD64AMode* iselIntExpr_AMode ( ISelEnv* env, IRExpr* e )
{
   AMD64AMode* am = iselIntExpr_AMode_wrk(env, e);
   vassert(sane_AMode(am));
   return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static AMD64AMode* iselIntExpr_AMode_wrk ( ISelEnv* env, IRExpr* e )
{
   MatchInfo mi;
   DECLARE_PATTERN(p_complex);
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64);

   /* Add64( Add64(expr1, Shl64(expr2, imm8)), simm32 ) */
   /*              bind0        bind1  bind2   bind3   */
   DEFINE_PATTERN(p_complex,
      binop( Iop_Add64,
             binop( Iop_Add64, 
                    bind(0), 
                    binop(Iop_Shl64, bind(1), bind(2))
                  ),
             bind(3)
           )
   );
   if (matchIRExpr(&mi, p_complex, e)) {
      IRExpr* expr1  = mi.bindee[0];
      IRExpr* expr2  = mi.bindee[1];
      IRExpr* imm8   = mi.bindee[2];
      IRExpr* simm32 = mi.bindee[3];
      if (imm8->tag == Iex_Const 
          && imm8->Iex.Const.con->tag == Ico_U8
          && imm8->Iex.Const.con->Ico.U8 < 4
          /* imm8 is OK, now check simm32 */
          && simm32->tag == Iex_Const
          && simm32->Iex.Const.con->tag == Ico_U64
          && fitsIn32Bits(simm32->Iex.Const.con->Ico.U64)) {
         UInt shift = imm8->Iex.Const.con->Ico.U8;
         UInt offset = toUInt(simm32->Iex.Const.con->Ico.U64);
         HReg r1 = iselIntExpr_R(env, expr1);
         HReg r2 = iselIntExpr_R(env, expr2);
         vassert(shift == 0 || shift == 1 || shift == 2 || shift == 3);
         return AMD64AMode_IRRS(offset, r1, r2, shift);
      }
   }

   /* Add64(expr1, Shl64(expr2, imm)) */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Add64
       && e->Iex.Binop.arg2->tag == Iex_Binop
       && e->Iex.Binop.arg2->Iex.Binop.op == Iop_Shl64
       && e->Iex.Binop.arg2->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8) {
      UInt shift = e->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
      if (shift == 1 || shift == 2 || shift == 3) {
         HReg r1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg r2 = iselIntExpr_R(env, e->Iex.Binop.arg2->Iex.Binop.arg1 );
         return AMD64AMode_IRRS(0, r1, r2, shift);
      }
   }

   /* Add64(expr,i) */
   if (e->tag == Iex_Binop 
       && e->Iex.Binop.op == Iop_Add64
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U64
       && fitsIn32Bits(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)) {
      HReg r1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
      return AMD64AMode_IR(
                toUInt(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64), 
                r1
             );
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg r1 = iselIntExpr_R(env, e);
      return AMD64AMode_IR(0, r1);
   }
}


/* --------------------- RMIs --------------------- */

/* Similarly, calculate an expression into an X86RMI operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits.  */

static AMD64RMI* iselIntExpr_RMI ( ISelEnv* env, IRExpr* e )
{
   AMD64RMI* rmi = iselIntExpr_RMI_wrk(env, e);
   /* sanity checks ... */
   switch (rmi->tag) {
      case Armi_Imm:
         return rmi;
      case Armi_Reg:
         vassert(hregClass(rmi->Armi.Reg.reg) == HRcInt64);
         vassert(hregIsVirtual(rmi->Armi.Reg.reg));
         return rmi;
      case Armi_Mem:
         vassert(sane_AMode(rmi->Armi.Mem.am));
         return rmi;
      default:
         vpanic("iselIntExpr_RMI: unknown amd64 RMI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static AMD64RMI* iselIntExpr_RMI_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64 || ty == Ity_I32 
           || ty == Ity_I16 || ty == Ity_I8);

   /* special case: immediate 64/32/16/8 */
   if (e->tag == Iex_Const) {
      switch (e->Iex.Const.con->tag) {
        case Ico_U64:
           if (fitsIn32Bits(e->Iex.Const.con->Ico.U64)) {
              return AMD64RMI_Imm(toUInt(e->Iex.Const.con->Ico.U64));
           }
           break;
         case Ico_U32:
            return AMD64RMI_Imm(e->Iex.Const.con->Ico.U32); break;
         case Ico_U16:
            return AMD64RMI_Imm(0xFFFF & e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:
            return AMD64RMI_Imm(0xFF & e->Iex.Const.con->Ico.U8); break;
         default:
            vpanic("iselIntExpr_RMI.Iex_Const(amd64)");
      }
   }

   /* special case: 64-bit GET */
   if (e->tag == Iex_Get && ty == Ity_I64) {
      return AMD64RMI_Mem(AMD64AMode_IR(e->Iex.Get.offset,
                                        hregAMD64_RBP()));
   }

   /* special case: 64-bit load from memory */
   if (e->tag == Iex_Load && ty == Ity_I64
       && e->Iex.Load.end == Iend_LE) {
      AMD64AMode* am = iselIntExpr_AMode(env, e->Iex.Load.addr);
      return AMD64RMI_Mem(am);
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return AMD64RMI_Reg(r);
   }
}


/* --------------------- RIs --------------------- */

/* Calculate an expression into an AMD64RI operand.  As with
   iselIntExpr_R, the expression can have type 64, 32, 16 or 8
   bits. */

static AMD64RI* iselIntExpr_RI ( ISelEnv* env, IRExpr* e )
{
   AMD64RI* ri = iselIntExpr_RI_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Ari_Imm:
         return ri;
      case Ari_Reg:
         vassert(hregClass(ri->Ari.Reg.reg) == HRcInt64);
         vassert(hregIsVirtual(ri->Ari.Reg.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RI: unknown amd64 RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static AMD64RI* iselIntExpr_RI_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64 || ty == Ity_I32 
           || ty == Ity_I16 || ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      switch (e->Iex.Const.con->tag) {
        case Ico_U64:
           if (fitsIn32Bits(e->Iex.Const.con->Ico.U64)) {
              return AMD64RI_Imm(toUInt(e->Iex.Const.con->Ico.U64));
           }
           break;
         case Ico_U32:
            return AMD64RI_Imm(e->Iex.Const.con->Ico.U32);
         case Ico_U16: 
            return AMD64RI_Imm(0xFFFF & e->Iex.Const.con->Ico.U16);
         case Ico_U8:
            return AMD64RI_Imm(0xFF & e->Iex.Const.con->Ico.U8);
         default:
            vpanic("iselIntExpr_RMI.Iex_Const(amd64)");
      }
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return AMD64RI_Reg(r);
   }
}


/* --------------------- RMs --------------------- */

/* Similarly, calculate an expression into an AMD64RM operand.  As
   with iselIntExpr_R, the expression can have type 64, 32, 16 or 8
   bits.  */

static AMD64RM* iselIntExpr_RM ( ISelEnv* env, IRExpr* e )
{
   AMD64RM* rm = iselIntExpr_RM_wrk(env, e);
   /* sanity checks ... */
   switch (rm->tag) {
      case Arm_Reg:
         vassert(hregClass(rm->Arm.Reg.reg) == HRcInt64);
         vassert(hregIsVirtual(rm->Arm.Reg.reg));
         return rm;
      case Arm_Mem:
         vassert(sane_AMode(rm->Arm.Mem.am));
         return rm;
      default:
         vpanic("iselIntExpr_RM: unknown amd64 RM tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static AMD64RM* iselIntExpr_RM_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I64 || ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   /* special case: 64-bit GET */
   if (e->tag == Iex_Get && ty == Ity_I64) {
      return AMD64RM_Mem(AMD64AMode_IR(e->Iex.Get.offset,
                                       hregAMD64_RBP()));
   }

   /* special case: load from memory */

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return AMD64RM_Reg(r);
   }
}


/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static AMD64CondCode iselCondCode ( ISelEnv* env, IRExpr* e )
{
   /* Uh, there's nothing we can sanity check here, unfortunately. */
   return iselCondCode_wrk(env,e);
}

/* DO NOT CALL THIS DIRECTLY ! */
static AMD64CondCode iselCondCode_wrk ( ISelEnv* env, IRExpr* e )
{
   MatchInfo mi;

   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I1);

   /* var */
   if (e->tag == Iex_RdTmp) {
      HReg r64 = lookupIRTemp(env, e->Iex.RdTmp.tmp);
      HReg dst = newVRegI(env);
      addInstr(env, mk_iMOVsd_RR(r64,dst));
      addInstr(env, AMD64Instr_Alu64R(Aalu_AND,AMD64RMI_Imm(1),dst));
      return Acc_NZ;
   }

   /* Constant 1:Bit */
   if (e->tag == Iex_Const) {
      HReg r;
      vassert(e->Iex.Const.con->tag == Ico_U1);
      vassert(e->Iex.Const.con->Ico.U1 == True 
              || e->Iex.Const.con->Ico.U1 == False);
      r = newVRegI(env);
      addInstr(env, AMD64Instr_Alu64R(Aalu_MOV,AMD64RMI_Imm(0),r));
      addInstr(env, AMD64Instr_Alu64R(Aalu_XOR,AMD64RMI_Reg(r),r));
      return e->Iex.Const.con->Ico.U1 ? Acc_Z : Acc_NZ;
   }

   /* Not1(...) */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_Not1) {
      /* Generate code for the arg, and negate the test condition */
      return 1 ^ iselCondCode(env, e->Iex.Unop.arg);
   }

   /* --- patterns rooted at: 64to1 --- */

   /* 64to1 */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_64to1) {
      HReg reg = iselIntExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, AMD64Instr_Test64(1,reg));
      return Acc_NZ;
   }

   /* --- patterns rooted at: 32to1 --- */

   /* 32to1 */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_32to1) {
      HReg reg = iselIntExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, AMD64Instr_Test64(1,reg));
      return Acc_NZ;
   }

   /* --- patterns rooted at: CmpNEZ8 --- */

   /* CmpNEZ8(x) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ8) {
      HReg r = iselIntExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, AMD64Instr_Test64(0xFF,r));
      return Acc_NZ;
   }

   /* --- patterns rooted at: CmpNEZ16 --- */

   /* CmpNEZ16(x) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ16) {
      HReg r = iselIntExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, AMD64Instr_Test64(0xFFFF,r));
      return Acc_NZ;
   }

   /* --- patterns rooted at: CmpNEZ32 --- */

   /* CmpNEZ32(x) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ32) {
      HReg      r1   = iselIntExpr_R(env, e->Iex.Unop.arg);
      AMD64RMI* rmi2 = AMD64RMI_Imm(0);
      addInstr(env, AMD64Instr_Alu32R(Aalu_CMP,rmi2,r1));
      return Acc_NZ;
   }

   /* --- patterns rooted at: CmpNEZ64 --- */

   /* CmpNEZ64(Or64(x,y)) */
   {
      DECLARE_PATTERN(p_CmpNEZ64_Or64);
      DEFINE_PATTERN(p_CmpNEZ64_Or64,
                     unop(Iop_CmpNEZ64, binop(Iop_Or64, bind(0), bind(1))));
      if (matchIRExpr(&mi, p_CmpNEZ64_Or64, e)) {
         HReg      r0   = iselIntExpr_R(env, mi.bindee[0]);
         AMD64RMI* rmi1 = iselIntExpr_RMI(env, mi.bindee[1]);
         HReg      tmp  = newVRegI(env);
         addInstr(env, mk_iMOVsd_RR(r0, tmp));
         addInstr(env, AMD64Instr_Alu64R(Aalu_OR,rmi1,tmp));
         return Acc_NZ;
      }
   }

   /* CmpNEZ64(x) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ64) {
      HReg      r1   = iselIntExpr_R(env, e->Iex.Unop.arg);
      AMD64RMI* rmi2 = AMD64RMI_Imm(0);
      addInstr(env, AMD64Instr_Alu64R(Aalu_CMP,rmi2,r1));
      return Acc_NZ;
   }

   /* --- patterns rooted at: Cmp{EQ,NE}{8,16,32} --- */

   /* CmpEQ8 / CmpNE8 */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ8
           || e->Iex.Binop.op == Iop_CmpNE8
           || e->Iex.Binop.op == Iop_CasCmpEQ8
           || e->Iex.Binop.op == Iop_CasCmpNE8)) {
      HReg      r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      AMD64RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      HReg      r    = newVRegI(env);
      addInstr(env, mk_iMOVsd_RR(r1,r));
      addInstr(env, AMD64Instr_Alu64R(Aalu_XOR,rmi2,r));
      addInstr(env, AMD64Instr_Alu64R(Aalu_AND,AMD64RMI_Imm(0xFF),r));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ8: case Iop_CasCmpEQ8: return Acc_Z;
         case Iop_CmpNE8: case Iop_CasCmpNE8: return Acc_NZ;
         default: vpanic("iselCondCode(amd64): CmpXX8");
      }
   }

   /* CmpEQ16 / CmpNE16 */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ16
           || e->Iex.Binop.op == Iop_CmpNE16
           || e->Iex.Binop.op == Iop_CasCmpEQ16
           || e->Iex.Binop.op == Iop_CasCmpNE16)) {
      HReg      r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      AMD64RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      HReg      r    = newVRegI(env);
      addInstr(env, mk_iMOVsd_RR(r1,r));
      addInstr(env, AMD64Instr_Alu64R(Aalu_XOR,rmi2,r));
      addInstr(env, AMD64Instr_Alu64R(Aalu_AND,AMD64RMI_Imm(0xFFFF),r));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ16: case Iop_CasCmpEQ16: return Acc_Z;
         case Iop_CmpNE16: case Iop_CasCmpNE16: return Acc_NZ;
         default: vpanic("iselCondCode(amd64): CmpXX16");
      }
   }

   /* CmpNE64(ccall, 64-bit constant) (--smc-check=all optimisation).
      Saves a "movq %rax, %tmp" compared to the default route. */
   if (e->tag == Iex_Binop 
       && e->Iex.Binop.op == Iop_CmpNE64
       && e->Iex.Binop.arg1->tag == Iex_CCall
       && e->Iex.Binop.arg2->tag == Iex_Const) {
      IRExpr* cal = e->Iex.Binop.arg1;
      IRExpr* con = e->Iex.Binop.arg2;
      HReg    tmp = newVRegI(env);
      /* clone & partial-eval of generic Iex_CCall and Iex_Const cases */
      vassert(cal->Iex.CCall.retty == Ity_I64); /* else ill-typed IR */
      vassert(con->Iex.Const.con->tag == Ico_U64);
      /* Marshal args, do the call. */
      doHelperCall( env, False, NULL, cal->Iex.CCall.cee, cal->Iex.CCall.args );
      addInstr(env, AMD64Instr_Imm64(con->Iex.Const.con->Ico.U64, tmp));
      addInstr(env, AMD64Instr_Alu64R(Aalu_CMP,
                                      AMD64RMI_Reg(hregAMD64_RAX()), tmp));
      return Acc_NZ;
   }

   /* Cmp*64*(x,y) */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ64
           || e->Iex.Binop.op == Iop_CmpNE64
           || e->Iex.Binop.op == Iop_CmpLT64S
           || e->Iex.Binop.op == Iop_CmpLT64U
           || e->Iex.Binop.op == Iop_CmpLE64S
           || e->Iex.Binop.op == Iop_CmpLE64U
           || e->Iex.Binop.op == Iop_CasCmpEQ64
           || e->Iex.Binop.op == Iop_CasCmpNE64)) {
      HReg      r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      AMD64RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      addInstr(env, AMD64Instr_Alu64R(Aalu_CMP,rmi2,r1));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ64: case Iop_CasCmpEQ64: return Acc_Z;
         case Iop_CmpNE64: case Iop_CasCmpNE64: return Acc_NZ;
	 case Iop_CmpLT64S: return Acc_L;
	 case Iop_CmpLT64U: return Acc_B;
	 case Iop_CmpLE64S: return Acc_LE;
         case Iop_CmpLE64U: return Acc_BE;
         default: vpanic("iselCondCode(amd64): CmpXX64");
      }
   }

   /* Cmp*32*(x,y) */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ32
           || e->Iex.Binop.op == Iop_CmpNE32
           || e->Iex.Binop.op == Iop_CmpLT32S
           || e->Iex.Binop.op == Iop_CmpLT32U
           || e->Iex.Binop.op == Iop_CmpLE32S
           || e->Iex.Binop.op == Iop_CmpLE32U
           || e->Iex.Binop.op == Iop_CasCmpEQ32
           || e->Iex.Binop.op == Iop_CasCmpNE32)) {
      HReg      r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      AMD64RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      addInstr(env, AMD64Instr_Alu32R(Aalu_CMP,rmi2,r1));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ32: case Iop_CasCmpEQ32: return Acc_Z;
         case Iop_CmpNE32: case Iop_CasCmpNE32: return Acc_NZ;
	 case Iop_CmpLT32S: return Acc_L;
	 case Iop_CmpLT32U: return Acc_B;
	 case Iop_CmpLE32S: return Acc_LE;
         case Iop_CmpLE32U: return Acc_BE;
         default: vpanic("iselCondCode(amd64): CmpXX32");
      }
   }

   ppIRExpr(e);
   vpanic("iselCondCode(amd64)");
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
            /* get one operand into %rax, and the other into a R/M.
               Need to make an educated guess about which is better in
               which. */
            HReg     tLo    = newVRegI(env);
            HReg     tHi    = newVRegI(env);
            Bool     syned  = toBool(e->Iex.Binop.op == Iop_MullS64);
            AMD64RM* rmLeft = iselIntExpr_RM(env, e->Iex.Binop.arg1);
            HReg     rRight = iselIntExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, mk_iMOVsd_RR(rRight, hregAMD64_RAX()));
            addInstr(env, AMD64Instr_MulL(syned, rmLeft));
            /* Result is now in RDX:RAX.  Tell the caller. */
            addInstr(env, mk_iMOVsd_RR(hregAMD64_RDX(), tHi));
            addInstr(env, mk_iMOVsd_RR(hregAMD64_RAX(), tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 128 x 64 -> (64(rem),64(div)) division */
         case Iop_DivModU128to64:
         case Iop_DivModS128to64: {
            /* Get the 128-bit operand into rdx:rax, and the other into
               any old R/M. */
            HReg sHi, sLo;
            HReg     tLo     = newVRegI(env);
            HReg     tHi     = newVRegI(env);
            Bool     syned   = toBool(e->Iex.Binop.op == Iop_DivModS128to64);
            AMD64RM* rmRight = iselIntExpr_RM(env, e->Iex.Binop.arg2);
            iselInt128Expr(&sHi,&sLo, env, e->Iex.Binop.arg1);
            addInstr(env, mk_iMOVsd_RR(sHi, hregAMD64_RDX()));
            addInstr(env, mk_iMOVsd_RR(sLo, hregAMD64_RAX()));
            addInstr(env, AMD64Instr_Div(syned, 8, rmRight));
            addInstr(env, mk_iMOVsd_RR(hregAMD64_RDX(), tHi));
            addInstr(env, mk_iMOVsd_RR(hregAMD64_RAX(), tLo));
            *rHi = tHi;
            *rLo = tLo;
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
   vpanic("iselInt128Expr");
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
   vassert(hregClass(r) == HRcVec128);
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

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      AMD64AMode* am;
      HReg res = newVRegV(env);
      vassert(e->Iex.Load.ty == Ity_F32);
      am = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 4, res, am));
      return res;
   }

   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_F64toF32) {
      /* Although the result is still held in a standard SSE register,
         we need to round it to reflect the loss of accuracy/range
         entailed in casting it to a 32-bit float. */
      HReg dst = newVRegV(env);
      HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
      set_SSE_rounding_mode( env, e->Iex.Binop.arg1 );
      addInstr(env, AMD64Instr_SseSDSS(True/*D->S*/,src,dst));
      set_SSE_rounding_default( env );
      return dst;
   }

   if (e->tag == Iex_Get) {
      AMD64AMode* am = AMD64AMode_IR( e->Iex.Get.offset,
                                       hregAMD64_RBP() );
      HReg res = newVRegV(env);
      addInstr(env, AMD64Instr_SseLdSt( True/*load*/, 4, res, am ));
      return res;
   }

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_ReinterpI32asF32) {
       /* Given an I32, produce an IEEE754 float with the same bit
          pattern. */
       HReg        dst    = newVRegV(env);
       HReg        src    = iselIntExpr_R(env, e->Iex.Unop.arg);
       AMD64AMode* m4_rsp = AMD64AMode_IR(-4, hregAMD64_RSP());
       addInstr(env, AMD64Instr_Store(4, src, m4_rsp));
       addInstr(env, AMD64Instr_SseLdSt( True/*load*/, 4, dst, m4_rsp ));
       return dst;
   }

   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_RoundF32toInt) {
      AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
      HReg        arg    = iselFltExpr(env, e->Iex.Binop.arg2);
      HReg        dst    = newVRegV(env);

      /* rf now holds the value to be rounded.  The first thing to do
         is set the FPU's rounding mode accordingly. */

      /* Set host x87 rounding mode */
      set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

      addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 4, arg, m8_rsp));
      addInstr(env, AMD64Instr_A87Free(1));
      addInstr(env, AMD64Instr_A87PushPop(m8_rsp, True/*push*/, 4));
      addInstr(env, AMD64Instr_A87FpOp(Afp_ROUND));
      addInstr(env, AMD64Instr_A87PushPop(m8_rsp, False/*pop*/, 4));
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 4, dst, m8_rsp));

      /* Restore default x87 rounding. */
      set_FPU_rounding_default( env );

      return dst;
   }

   ppIRExpr(e);
   vpanic("iselFltExpr_wrk");
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (64 bit)         ---*/
/*---------------------------------------------------------*/

/* Compute a 64-bit floating point value into the lower half of an xmm
   register, the identity of which is returned.  As with
   iselIntExpr_R, the returned reg will be virtual, and it must not be
   changed by subsequent code emitted by the caller.
*/

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
   vassert(hregClass(r) == HRcVec128);
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
      union { ULong u64; Double f64; } u;
      HReg res = newVRegV(env);
      HReg tmp = newVRegI(env);
      vassert(sizeof(u) == 8);
      vassert(sizeof(u.u64) == 8);
      vassert(sizeof(u.f64) == 8);

      if (e->Iex.Const.con->tag == Ico_F64) {
         u.f64 = e->Iex.Const.con->Ico.F64;
      }
      else if (e->Iex.Const.con->tag == Ico_F64i) {
         u.u64 = e->Iex.Const.con->Ico.F64i;
      }
      else
         vpanic("iselDblExpr(amd64): const");

      addInstr(env, AMD64Instr_Imm64(u.u64, tmp));
      addInstr(env, AMD64Instr_Push(AMD64RMI_Reg(tmp)));
      addInstr(env, AMD64Instr_SseLdSt(
                       True/*load*/, 8, res, 
                       AMD64AMode_IR(0, hregAMD64_RSP())
              ));
      add_to_rsp(env, 8);
      return res;
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      AMD64AMode* am;
      HReg res = newVRegV(env);
      vassert(e->Iex.Load.ty == Ity_F64);
      am = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, AMD64Instr_SseLdSt( True/*load*/, 8, res, am ));
      return res;
   }

   if (e->tag == Iex_Get) {
      AMD64AMode* am = AMD64AMode_IR( e->Iex.Get.offset,
                                      hregAMD64_RBP() );
      HReg res = newVRegV(env);
      addInstr(env, AMD64Instr_SseLdSt( True/*load*/, 8, res, am ));
      return res;
   }

   if (e->tag == Iex_GetI) {
      AMD64AMode* am 
         = genGuestArrayOffset(
              env, e->Iex.GetI.descr, 
                   e->Iex.GetI.ix, e->Iex.GetI.bias );
      HReg res = newVRegV(env);
      addInstr(env, AMD64Instr_SseLdSt( True/*load*/, 8, res, am ));
      return res;
   }

   if (e->tag == Iex_Triop) {
      IRTriop *triop = e->Iex.Triop.details;
      AMD64SseOp op = Asse_INVALID;
      switch (triop->op) {
         case Iop_AddF64: op = Asse_ADDF; break;
         case Iop_SubF64: op = Asse_SUBF; break;
         case Iop_MulF64: op = Asse_MULF; break;
         case Iop_DivF64: op = Asse_DIVF; break;
         default: break;
      }
      if (op != Asse_INVALID) {
         HReg dst  = newVRegV(env);
         HReg argL = iselDblExpr(env, triop->arg2);
         HReg argR = iselDblExpr(env, triop->arg3);
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         /* XXXROUNDINGFIXME */
         /* set roundingmode here */
         addInstr(env, AMD64Instr_Sse64FLo(op, argR, dst));
         return dst;
      }
   }

   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_RoundF64toInt) {
      AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
      HReg        arg    = iselDblExpr(env, e->Iex.Binop.arg2);
      HReg        dst    = newVRegV(env);

      /* rf now holds the value to be rounded.  The first thing to do
         is set the FPU's rounding mode accordingly. */

      /* Set host x87 rounding mode */
      set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

      addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 8, arg, m8_rsp));
      addInstr(env, AMD64Instr_A87Free(1));
      addInstr(env, AMD64Instr_A87PushPop(m8_rsp, True/*push*/, 8));
      addInstr(env, AMD64Instr_A87FpOp(Afp_ROUND));
      addInstr(env, AMD64Instr_A87PushPop(m8_rsp, False/*pop*/, 8));
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 8, dst, m8_rsp));

      /* Restore default x87 rounding. */
      set_FPU_rounding_default( env );

      return dst;
   }

   IRTriop *triop = e->Iex.Triop.details;
   if (e->tag == Iex_Triop 
       && (triop->op == Iop_ScaleF64
           || triop->op == Iop_AtanF64
           || triop->op == Iop_Yl2xF64
           || triop->op == Iop_Yl2xp1F64
           || triop->op == Iop_PRemF64
           || triop->op == Iop_PRem1F64)
      ) {
      AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
      HReg        arg1   = iselDblExpr(env, triop->arg2);
      HReg        arg2   = iselDblExpr(env, triop->arg3);
      HReg        dst    = newVRegV(env);
      Bool     arg2first = toBool(triop->op == Iop_ScaleF64 
                                  || triop->op == Iop_PRemF64
                                  || triop->op == Iop_PRem1F64);
      addInstr(env, AMD64Instr_A87Free(2));

      /* one arg -> top of x87 stack */
      addInstr(env, AMD64Instr_SseLdSt(
                       False/*store*/, 8, arg2first ? arg2 : arg1, m8_rsp));
      addInstr(env, AMD64Instr_A87PushPop(m8_rsp, True/*push*/, 8));

      /* other arg -> top of x87 stack */
      addInstr(env, AMD64Instr_SseLdSt(
                       False/*store*/, 8, arg2first ? arg1 : arg2, m8_rsp));
      addInstr(env, AMD64Instr_A87PushPop(m8_rsp, True/*push*/, 8));

      /* do it */
      /* XXXROUNDINGFIXME */
      /* set roundingmode here */
      switch (triop->op) {
         case Iop_ScaleF64: 
            addInstr(env, AMD64Instr_A87FpOp(Afp_SCALE));
            break;
         case Iop_AtanF64: 
            addInstr(env, AMD64Instr_A87FpOp(Afp_ATAN));
            break;
         case Iop_Yl2xF64: 
            addInstr(env, AMD64Instr_A87FpOp(Afp_YL2X));
            break;
         case Iop_Yl2xp1F64: 
            addInstr(env, AMD64Instr_A87FpOp(Afp_YL2XP1));
            break;
         case Iop_PRemF64:
            addInstr(env, AMD64Instr_A87FpOp(Afp_PREM));
            break;
         case Iop_PRem1F64:
            addInstr(env, AMD64Instr_A87FpOp(Afp_PREM1));
            break;
         default: 
            vassert(0);
      }

      /* save result */
      addInstr(env, AMD64Instr_A87PushPop(m8_rsp, False/*pop*/, 8));
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 8, dst, m8_rsp));
      return dst;
   }

   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_I64StoF64) {
      HReg dst = newVRegV(env);
      HReg src = iselIntExpr_R(env, e->Iex.Binop.arg2);
      set_SSE_rounding_mode( env, e->Iex.Binop.arg1 );
      addInstr(env, AMD64Instr_SseSI2SF( 8, 8, src, dst ));
      set_SSE_rounding_default( env );
      return dst;
   }

   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_I32StoF64) {
      HReg dst = newVRegV(env);
      HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
      set_SSE_rounding_default( env );
      addInstr(env, AMD64Instr_SseSI2SF( 4, 8, src, dst ));
      return dst;
   }

   if (e->tag == Iex_Unop 
       && (e->Iex.Unop.op == Iop_NegF64
           || e->Iex.Unop.op == Iop_AbsF64)) {
      /* Sigh ... very rough code.  Could do much better. */
      /* Get the 128-bit literal 00---0 10---0 into a register
         and xor/nand it with the value to be negated. */
      HReg r1  = newVRegI(env);
      HReg dst = newVRegV(env);
      HReg tmp = newVRegV(env);
      HReg src = iselDblExpr(env, e->Iex.Unop.arg);
      AMD64AMode* rsp0 = AMD64AMode_IR(0, hregAMD64_RSP());
      addInstr(env, mk_vMOVsd_RR(src,tmp));
      addInstr(env, AMD64Instr_Push(AMD64RMI_Imm(0)));
      addInstr(env, AMD64Instr_Imm64( 1ULL<<63, r1 ));
      addInstr(env, AMD64Instr_Push(AMD64RMI_Reg(r1)));
      addInstr(env, AMD64Instr_SseLdSt(True, 16, dst, rsp0));

      if (e->Iex.Unop.op == Iop_NegF64)
         addInstr(env, AMD64Instr_SseReRg(Asse_XOR, tmp, dst));
      else
         addInstr(env, AMD64Instr_SseReRg(Asse_ANDN, tmp, dst));

      add_to_rsp(env, 16);
      return dst;
   }

   if (e->tag == Iex_Binop) {
      A87FpOp fpop = Afp_INVALID;
      switch (e->Iex.Binop.op) {
         case Iop_SqrtF64: fpop = Afp_SQRT; break;
         case Iop_SinF64:  fpop = Afp_SIN;  break;
         case Iop_CosF64:  fpop = Afp_COS;  break;
         case Iop_TanF64:  fpop = Afp_TAN;  break;
         case Iop_2xm1F64: fpop = Afp_2XM1; break;
         default: break;
      }
      if (fpop != Afp_INVALID) {
         AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
         HReg        arg    = iselDblExpr(env, e->Iex.Binop.arg2);
         HReg        dst    = newVRegV(env);
         Int     nNeeded    = e->Iex.Binop.op==Iop_TanF64 ? 2 : 1;
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 8, arg, m8_rsp));
         addInstr(env, AMD64Instr_A87Free(nNeeded));
         addInstr(env, AMD64Instr_A87PushPop(m8_rsp, True/*push*/, 8));
         /* XXXROUNDINGFIXME */
         /* set roundingmode here */
         addInstr(env, AMD64Instr_A87FpOp(fpop));
         if (e->Iex.Binop.op==Iop_TanF64) {
            /* get rid of the extra 1.0 that fptan pushes */
            addInstr(env, AMD64Instr_A87PushPop(m8_rsp, False/*pop*/, 8));
         }
         addInstr(env, AMD64Instr_A87PushPop(m8_rsp, False/*pop*/, 8));
         addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 8, dst, m8_rsp));
         return dst;
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
            AMD64AMode* m8_rsp = AMD64AMode_IR(-8, hregAMD64_RSP());
            HReg        dst    = newVRegV(env);
            AMD64RI*    src    = iselIntExpr_RI(env, e->Iex.Unop.arg);
            /* paranoia */
            set_SSE_rounding_default(env);
            addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, src, m8_rsp));
            addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 8, dst, m8_rsp));
            return dst;
         }
         case Iop_F32toF64: {
            HReg f32;
            HReg f64 = newVRegV(env);
            /* this shouldn't be necessary, but be paranoid ... */
            set_SSE_rounding_default(env);
            f32 = iselFltExpr(env, e->Iex.Unop.arg);
            addInstr(env, AMD64Instr_SseSDSS(False/*S->D*/, f32, f64));
            return f64;
         }
         default: 
            break;
      }
   }

   /* --------- MULTIPLEX --------- */
   if (e->tag == Iex_Mux0X) {
      HReg r8, rX, r0, dst;
      vassert(ty == Ity_F64);
      vassert(typeOfIRExpr(env->type_env,e->Iex.Mux0X.cond) == Ity_I8);
      r8  = iselIntExpr_R(env, e->Iex.Mux0X.cond);
      rX  = iselDblExpr(env, e->Iex.Mux0X.exprX);
      r0  = iselDblExpr(env, e->Iex.Mux0X.expr0);
      dst = newVRegV(env);
      addInstr(env, mk_vMOVsd_RR(rX,dst));
      addInstr(env, AMD64Instr_Test64(0xFF, r8));
      addInstr(env, AMD64Instr_SseCMov(Acc_Z,r0,dst));
      return dst;
   }

   ppIRExpr(e);
   vpanic("iselDblExpr_wrk");
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
   HWord      fn = 0; /* address of helper fn, if required */
   Bool       arg1isEReg = False;
   AMD64SseOp op = Asse_INVALID;
   IRType     ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_V128);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Get) {
      HReg dst = newVRegV(env);
      addInstr(env, AMD64Instr_SseLdSt(
                       True/*load*/, 
                       16,
                       dst,
                       AMD64AMode_IR(e->Iex.Get.offset, hregAMD64_RBP())
                    )
              );
      return dst;
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      HReg        dst = newVRegV(env);
      AMD64AMode* am  = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, AMD64Instr_SseLdSt( True/*load*/, 16, dst, am ));
      return dst;
   }

   if (e->tag == Iex_Const) {
      HReg dst = newVRegV(env);
      vassert(e->Iex.Const.con->tag == Ico_V128);
      switch (e->Iex.Const.con->Ico.V128) {
         case 0x0000:
            dst = generate_zeroes_V128(env);
            break;
         case 0xFFFF:
            dst = generate_ones_V128(env);
            break;
         default: {
            AMD64AMode* rsp0 = AMD64AMode_IR(0, hregAMD64_RSP());
            /* do push_uimm64 twice, first time for the high-order half. */
            push_uimm64(env, bitmask8_to_bytemask64(
                                (e->Iex.Const.con->Ico.V128 >> 8) & 0xFF
                       ));
            push_uimm64(env, bitmask8_to_bytemask64(
                                (e->Iex.Const.con->Ico.V128 >> 0) & 0xFF
                       ));
            addInstr(env, AMD64Instr_SseLdSt( True/*load*/, 16, dst, rsp0 ));
            add_to_rsp(env, 16);
            break;
         }
      }
      return dst;
   }

   if (e->tag == Iex_Unop) {
   switch (e->Iex.Unop.op) {

      case Iop_NotV128: {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         return do_sse_NotV128(env, arg);
      }

      case Iop_CmpNEZ64x2: {
         /* We can use SSE2 instructions for this. */
         /* Ideally, we want to do a 64Ix2 comparison against zero of
            the operand.  Problem is no such insn exists.  Solution
            therefore is to do a 32Ix4 comparison instead, and bitwise-
            negate (NOT) the result.  Let a,b,c,d be 32-bit lanes, and 
            let the not'd result of this initial comparison be a:b:c:d.
            What we need to compute is (a|b):(a|b):(c|d):(c|d).  So, use
            pshufd to create a value b:a:d:c, and OR that with a:b:c:d,
            giving the required result.

            The required selection sequence is 2,3,0,1, which
            according to Intel's documentation means the pshufd
            literal value is 0xB1, that is, 
            (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0) 
         */
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg tmp  = generate_zeroes_V128(env);
         HReg dst  = newVRegV(env);
         addInstr(env, AMD64Instr_SseReRg(Asse_CMPEQ32, arg, tmp));
         tmp = do_sse_NotV128(env, tmp);
         addInstr(env, AMD64Instr_SseShuf(0xB1, tmp, dst));
         addInstr(env, AMD64Instr_SseReRg(Asse_OR, tmp, dst));
         return dst;
      }

      case Iop_CmpNEZ32x4: op = Asse_CMPEQ32; goto do_CmpNEZ_vector;
      case Iop_CmpNEZ16x8: op = Asse_CMPEQ16; goto do_CmpNEZ_vector;
      case Iop_CmpNEZ8x16: op = Asse_CMPEQ8;  goto do_CmpNEZ_vector;
      do_CmpNEZ_vector:
      {
         HReg arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg tmp  = newVRegV(env);
         HReg zero = generate_zeroes_V128(env);
         HReg dst;
         addInstr(env, mk_vMOVsd_RR(arg, tmp));
         addInstr(env, AMD64Instr_SseReRg(op, zero, tmp));
         dst = do_sse_NotV128(env, tmp);
         return dst;
      }

      case Iop_Recip32Fx4: op = Asse_RCPF;   goto do_32Fx4_unary;
      case Iop_RSqrt32Fx4: op = Asse_RSQRTF; goto do_32Fx4_unary;
      case Iop_Sqrt32Fx4:  op = Asse_SQRTF;  goto do_32Fx4_unary;
      do_32Fx4_unary:
      {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, AMD64Instr_Sse32Fx4(op, arg, dst));
         return dst;
      }

      case Iop_Sqrt64Fx2:  op = Asse_SQRTF;  goto do_64Fx2_unary;
      do_64Fx2_unary:
      {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, AMD64Instr_Sse64Fx2(op, arg, dst));
         return dst;
      }

      case Iop_Recip32F0x4: op = Asse_RCPF;   goto do_32F0x4_unary;
      case Iop_RSqrt32F0x4: op = Asse_RSQRTF; goto do_32F0x4_unary;
      case Iop_Sqrt32F0x4:  op = Asse_SQRTF;  goto do_32F0x4_unary;
      do_32F0x4_unary:
      {
         /* A bit subtle.  We have to copy the arg to the result
            register first, because actually doing the SSE scalar insn
            leaves the upper 3/4 of the destination register
            unchanged.  Whereas the required semantics of these
            primops is that the upper 3/4 is simply copied in from the
            argument. */
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(arg, dst));
         addInstr(env, AMD64Instr_Sse32FLo(op, arg, dst));
         return dst;
      }

      case Iop_Sqrt64F0x2:  op = Asse_SQRTF;  goto do_64F0x2_unary;
      do_64F0x2_unary:
      {
         /* A bit subtle.  We have to copy the arg to the result
            register first, because actually doing the SSE scalar insn
            leaves the upper half of the destination register
            unchanged.  Whereas the required semantics of these
            primops is that the upper half is simply copied in from the
            argument. */
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(arg, dst));
         addInstr(env, AMD64Instr_Sse64FLo(op, arg, dst));
         return dst;
      }

      case Iop_32UtoV128: {
         HReg        dst     = newVRegV(env);
         AMD64AMode* rsp_m32 = AMD64AMode_IR(-32, hregAMD64_RSP());
         AMD64RI*    ri      = iselIntExpr_RI(env, e->Iex.Unop.arg);
         addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, ri, rsp_m32));
         addInstr(env, AMD64Instr_SseLdzLO(4, dst, rsp_m32));
         return dst;
      }

      case Iop_64UtoV128: {
         HReg        dst  = newVRegV(env);
         AMD64AMode* rsp0 = AMD64AMode_IR(0, hregAMD64_RSP());
         AMD64RMI*   rmi  = iselIntExpr_RMI(env, e->Iex.Unop.arg);
         addInstr(env, AMD64Instr_Push(rmi));
         addInstr(env, AMD64Instr_SseLdzLO(8, dst, rsp0));
         add_to_rsp(env, 8);
         return dst;
      }

      case Iop_V256toV128_0:
      case Iop_V256toV128_1: {
         HReg vHi, vLo;
         iselDVecExpr(&vHi, &vLo, env, e->Iex.Unop.arg);
         return (e->Iex.Unop.op == Iop_V256toV128_1) ? vHi : vLo;
      }

      default:
         break;
   } /* switch (e->Iex.Unop.op) */
   } /* if (e->tag == Iex_Unop) */

   if (e->tag == Iex_Binop) {
   switch (e->Iex.Binop.op) {

      /* FIXME: could we generate MOVQ here? */
      case Iop_SetV128lo64: {
         HReg dst  = newVRegV(env);
         HReg srcV = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg srcI = iselIntExpr_R(env, e->Iex.Binop.arg2);
         AMD64AMode* rsp_m16 = AMD64AMode_IR(-16, hregAMD64_RSP());
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, srcV, rsp_m16));
         addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, AMD64RI_Reg(srcI), rsp_m16));
         addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, dst, rsp_m16));
         return dst;
      }

      /* FIXME: could we generate MOVD here? */
      case Iop_SetV128lo32: {
         HReg dst  = newVRegV(env);
         HReg srcV = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg srcI = iselIntExpr_R(env, e->Iex.Binop.arg2);
         AMD64AMode* rsp_m16 = AMD64AMode_IR(-16, hregAMD64_RSP());
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, srcV, rsp_m16));
         addInstr(env, AMD64Instr_Store(4, srcI, rsp_m16));
         addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, dst, rsp_m16));
         return dst;
      }

      case Iop_64HLtoV128: {
         HReg        rsp     = hregAMD64_RSP();
         AMD64AMode* m8_rsp  = AMD64AMode_IR(-8, rsp);
         AMD64AMode* m16_rsp = AMD64AMode_IR(-16, rsp);
         AMD64RI*    qHi = iselIntExpr_RI(env, e->Iex.Binop.arg1);
         AMD64RI*    qLo = iselIntExpr_RI(env, e->Iex.Binop.arg2);
         addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, qHi, m8_rsp));
         addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, qLo, m16_rsp));
         HReg        dst = newVRegV(env);
         /* One store-forwarding stall coming up, oh well :-( */
         addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, dst, m16_rsp));
         return dst;
      }

      case Iop_CmpEQ32Fx4: op = Asse_CMPEQF; goto do_32Fx4;
      case Iop_CmpLT32Fx4: op = Asse_CMPLTF; goto do_32Fx4;
      case Iop_CmpLE32Fx4: op = Asse_CMPLEF; goto do_32Fx4;
      case Iop_CmpUN32Fx4: op = Asse_CMPUNF; goto do_32Fx4;
      case Iop_Add32Fx4:   op = Asse_ADDF;   goto do_32Fx4;
      case Iop_Div32Fx4:   op = Asse_DIVF;   goto do_32Fx4;
      case Iop_Max32Fx4:   op = Asse_MAXF;   goto do_32Fx4;
      case Iop_Min32Fx4:   op = Asse_MINF;   goto do_32Fx4;
      case Iop_Mul32Fx4:   op = Asse_MULF;   goto do_32Fx4;
      case Iop_Sub32Fx4:   op = Asse_SUBF;   goto do_32Fx4;
      do_32Fx4:
      {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         addInstr(env, AMD64Instr_Sse32Fx4(op, argR, dst));
         return dst;
      }

      case Iop_CmpEQ64Fx2: op = Asse_CMPEQF; goto do_64Fx2;
      case Iop_CmpLT64Fx2: op = Asse_CMPLTF; goto do_64Fx2;
      case Iop_CmpLE64Fx2: op = Asse_CMPLEF; goto do_64Fx2;
      case Iop_CmpUN64Fx2: op = Asse_CMPUNF; goto do_64Fx2;
      case Iop_Add64Fx2:   op = Asse_ADDF;   goto do_64Fx2;
      case Iop_Div64Fx2:   op = Asse_DIVF;   goto do_64Fx2;
      case Iop_Max64Fx2:   op = Asse_MAXF;   goto do_64Fx2;
      case Iop_Min64Fx2:   op = Asse_MINF;   goto do_64Fx2;
      case Iop_Mul64Fx2:   op = Asse_MULF;   goto do_64Fx2;
      case Iop_Sub64Fx2:   op = Asse_SUBF;   goto do_64Fx2;
      do_64Fx2:
      {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         addInstr(env, AMD64Instr_Sse64Fx2(op, argR, dst));
         return dst;
      }

      case Iop_CmpEQ32F0x4: op = Asse_CMPEQF; goto do_32F0x4;
      case Iop_CmpLT32F0x4: op = Asse_CMPLTF; goto do_32F0x4;
      case Iop_CmpLE32F0x4: op = Asse_CMPLEF; goto do_32F0x4;
      case Iop_CmpUN32F0x4: op = Asse_CMPUNF; goto do_32F0x4;
      case Iop_Add32F0x4:   op = Asse_ADDF;   goto do_32F0x4;
      case Iop_Div32F0x4:   op = Asse_DIVF;   goto do_32F0x4;
      case Iop_Max32F0x4:   op = Asse_MAXF;   goto do_32F0x4;
      case Iop_Min32F0x4:   op = Asse_MINF;   goto do_32F0x4;
      case Iop_Mul32F0x4:   op = Asse_MULF;   goto do_32F0x4;
      case Iop_Sub32F0x4:   op = Asse_SUBF;   goto do_32F0x4;
      do_32F0x4: {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         addInstr(env, AMD64Instr_Sse32FLo(op, argR, dst));
         return dst;
      }

      case Iop_CmpEQ64F0x2: op = Asse_CMPEQF; goto do_64F0x2;
      case Iop_CmpLT64F0x2: op = Asse_CMPLTF; goto do_64F0x2;
      case Iop_CmpLE64F0x2: op = Asse_CMPLEF; goto do_64F0x2;
      case Iop_CmpUN64F0x2: op = Asse_CMPUNF; goto do_64F0x2;
      case Iop_Add64F0x2:   op = Asse_ADDF;   goto do_64F0x2;
      case Iop_Div64F0x2:   op = Asse_DIVF;   goto do_64F0x2;
      case Iop_Max64F0x2:   op = Asse_MAXF;   goto do_64F0x2;
      case Iop_Min64F0x2:   op = Asse_MINF;   goto do_64F0x2;
      case Iop_Mul64F0x2:   op = Asse_MULF;   goto do_64F0x2;
      case Iop_Sub64F0x2:   op = Asse_SUBF;   goto do_64F0x2;
      do_64F0x2: {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         addInstr(env, AMD64Instr_Sse64FLo(op, argR, dst));
         return dst;
      }

      case Iop_QNarrowBin32Sto16Sx8: 
         op = Asse_PACKSSD; arg1isEReg = True; goto do_SseReRg;
      case Iop_QNarrowBin16Sto8Sx16: 
         op = Asse_PACKSSW; arg1isEReg = True; goto do_SseReRg;
      case Iop_QNarrowBin16Sto8Ux16: 
         op = Asse_PACKUSW; arg1isEReg = True; goto do_SseReRg;

      case Iop_InterleaveHI8x16: 
         op = Asse_UNPCKHB; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveHI16x8: 
         op = Asse_UNPCKHW; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveHI32x4: 
         op = Asse_UNPCKHD; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveHI64x2: 
         op = Asse_UNPCKHQ; arg1isEReg = True; goto do_SseReRg;

      case Iop_InterleaveLO8x16: 
         op = Asse_UNPCKLB; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveLO16x8: 
         op = Asse_UNPCKLW; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveLO32x4: 
         op = Asse_UNPCKLD; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveLO64x2: 
         op = Asse_UNPCKLQ; arg1isEReg = True; goto do_SseReRg;

      case Iop_AndV128:    op = Asse_AND;      goto do_SseReRg;
      case Iop_OrV128:     op = Asse_OR;       goto do_SseReRg;
      case Iop_XorV128:    op = Asse_XOR;      goto do_SseReRg;
      case Iop_Add8x16:    op = Asse_ADD8;     goto do_SseReRg;
      case Iop_Add16x8:    op = Asse_ADD16;    goto do_SseReRg;
      case Iop_Add32x4:    op = Asse_ADD32;    goto do_SseReRg;
      case Iop_Add64x2:    op = Asse_ADD64;    goto do_SseReRg;
      case Iop_QAdd8Sx16:  op = Asse_QADD8S;   goto do_SseReRg;
      case Iop_QAdd16Sx8:  op = Asse_QADD16S;  goto do_SseReRg;
      case Iop_QAdd8Ux16:  op = Asse_QADD8U;   goto do_SseReRg;
      case Iop_QAdd16Ux8:  op = Asse_QADD16U;  goto do_SseReRg;
      case Iop_Avg8Ux16:   op = Asse_AVG8U;    goto do_SseReRg;
      case Iop_Avg16Ux8:   op = Asse_AVG16U;   goto do_SseReRg;
      case Iop_CmpEQ8x16:  op = Asse_CMPEQ8;   goto do_SseReRg;
      case Iop_CmpEQ16x8:  op = Asse_CMPEQ16;  goto do_SseReRg;
      case Iop_CmpEQ32x4:  op = Asse_CMPEQ32;  goto do_SseReRg;
      case Iop_CmpGT8Sx16: op = Asse_CMPGT8S;  goto do_SseReRg;
      case Iop_CmpGT16Sx8: op = Asse_CMPGT16S; goto do_SseReRg;
      case Iop_CmpGT32Sx4: op = Asse_CMPGT32S; goto do_SseReRg;
      case Iop_Max16Sx8:   op = Asse_MAX16S;   goto do_SseReRg;
      case Iop_Max8Ux16:   op = Asse_MAX8U;    goto do_SseReRg;
      case Iop_Min16Sx8:   op = Asse_MIN16S;   goto do_SseReRg;
      case Iop_Min8Ux16:   op = Asse_MIN8U;    goto do_SseReRg;
      case Iop_MulHi16Ux8: op = Asse_MULHI16U; goto do_SseReRg;
      case Iop_MulHi16Sx8: op = Asse_MULHI16S; goto do_SseReRg;
      case Iop_Mul16x8:    op = Asse_MUL16;    goto do_SseReRg;
      case Iop_Sub8x16:    op = Asse_SUB8;     goto do_SseReRg;
      case Iop_Sub16x8:    op = Asse_SUB16;    goto do_SseReRg;
      case Iop_Sub32x4:    op = Asse_SUB32;    goto do_SseReRg;
      case Iop_Sub64x2:    op = Asse_SUB64;    goto do_SseReRg;
      case Iop_QSub8Sx16:  op = Asse_QSUB8S;   goto do_SseReRg;
      case Iop_QSub16Sx8:  op = Asse_QSUB16S;  goto do_SseReRg;
      case Iop_QSub8Ux16:  op = Asse_QSUB8U;   goto do_SseReRg;
      case Iop_QSub16Ux8:  op = Asse_QSUB16U;  goto do_SseReRg;
      do_SseReRg: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         if (arg1isEReg) {
            addInstr(env, mk_vMOVsd_RR(arg2, dst));
            addInstr(env, AMD64Instr_SseReRg(op, arg1, dst));
         } else {
            addInstr(env, mk_vMOVsd_RR(arg1, dst));
            addInstr(env, AMD64Instr_SseReRg(op, arg2, dst));
         }
         return dst;
      }

      case Iop_ShlN16x8: op = Asse_SHL16; goto do_SseShift;
      case Iop_ShlN32x4: op = Asse_SHL32; goto do_SseShift;
      case Iop_ShlN64x2: op = Asse_SHL64; goto do_SseShift;
      case Iop_SarN16x8: op = Asse_SAR16; goto do_SseShift;
      case Iop_SarN32x4: op = Asse_SAR32; goto do_SseShift;
      case Iop_ShrN16x8: op = Asse_SHR16; goto do_SseShift;
      case Iop_ShrN32x4: op = Asse_SHR32; goto do_SseShift;
      case Iop_ShrN64x2: op = Asse_SHR64; goto do_SseShift;
      do_SseShift: {
         HReg        greg = iselVecExpr(env, e->Iex.Binop.arg1);
         AMD64RMI*   rmi  = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         AMD64AMode* rsp0 = AMD64AMode_IR(0, hregAMD64_RSP());
         HReg        ereg = newVRegV(env);
         HReg        dst  = newVRegV(env);
         addInstr(env, AMD64Instr_Push(AMD64RMI_Imm(0)));
         addInstr(env, AMD64Instr_Push(rmi));
         addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, ereg, rsp0));
         addInstr(env, mk_vMOVsd_RR(greg, dst));
         addInstr(env, AMD64Instr_SseReRg(op, ereg, dst));
         add_to_rsp(env, 16);
         return dst;
      }

      case Iop_Mul32x4:    fn = (HWord)h_generic_calc_Mul32x4;
                           goto do_SseAssistedBinary;
      case Iop_Max32Sx4:   fn = (HWord)h_generic_calc_Max32Sx4;
                           goto do_SseAssistedBinary;
      case Iop_Min32Sx4:   fn = (HWord)h_generic_calc_Min32Sx4;
                           goto do_SseAssistedBinary;
      case Iop_Max32Ux4:   fn = (HWord)h_generic_calc_Max32Ux4;
                           goto do_SseAssistedBinary;
      case Iop_Min32Ux4:   fn = (HWord)h_generic_calc_Min32Ux4;
                           goto do_SseAssistedBinary;
      case Iop_Max16Ux8:   fn = (HWord)h_generic_calc_Max16Ux8;
                           goto do_SseAssistedBinary;
      case Iop_Min16Ux8:   fn = (HWord)h_generic_calc_Min16Ux8;
                           goto do_SseAssistedBinary;
      case Iop_Max8Sx16:   fn = (HWord)h_generic_calc_Max8Sx16;
                           goto do_SseAssistedBinary;
      case Iop_Min8Sx16:   fn = (HWord)h_generic_calc_Min8Sx16;
                           goto do_SseAssistedBinary;
      case Iop_CmpEQ64x2:  fn = (HWord)h_generic_calc_CmpEQ64x2;
                           goto do_SseAssistedBinary;
      case Iop_CmpGT64Sx2: fn = (HWord)h_generic_calc_CmpGT64Sx2;
                           goto do_SseAssistedBinary;
      case Iop_Perm32x4:   fn = (HWord)h_generic_calc_Perm32x4;
                           goto do_SseAssistedBinary;
      case Iop_QNarrowBin32Sto16Ux8:
                           fn = (HWord)h_generic_calc_QNarrowBin32Sto16Ux8;
                           goto do_SseAssistedBinary;
      case Iop_NarrowBin16to8x16:
                           fn = (HWord)h_generic_calc_NarrowBin16to8x16;
                           goto do_SseAssistedBinary;
      case Iop_NarrowBin32to16x8:
                           fn = (HWord)h_generic_calc_NarrowBin32to16x8;
                           goto do_SseAssistedBinary;
      do_SseAssistedBinary: {
         /* RRRufff!  RRRufff code is what we're generating here.  Oh
            well. */
         vassert(fn != 0);
         HReg dst = newVRegV(env);
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg argp = newVRegI(env);
         /* subq $112, %rsp         -- make a space*/
         sub_from_rsp(env, 112);
         /* leaq 48(%rsp), %r_argp  -- point into it */
         addInstr(env, AMD64Instr_Lea64(AMD64AMode_IR(48, hregAMD64_RSP()),
                                        argp));
         /* andq $-16, %r_argp      -- 16-align the pointer */
         addInstr(env, AMD64Instr_Alu64R(Aalu_AND,
                                         AMD64RMI_Imm( ~(UInt)15 ), 
                                         argp));
         /* Prepare 3 arg regs:
            leaq 0(%r_argp), %rdi
            leaq 16(%r_argp), %rsi
            leaq 32(%r_argp), %rdx
         */
         addInstr(env, AMD64Instr_Lea64(AMD64AMode_IR(0, argp),
                                        hregAMD64_RDI()));
         addInstr(env, AMD64Instr_Lea64(AMD64AMode_IR(16, argp),
                                        hregAMD64_RSI()));
         addInstr(env, AMD64Instr_Lea64(AMD64AMode_IR(32, argp),
                                        hregAMD64_RDX()));
         /* Store the two args, at (%rsi) and (%rdx):
            movupd  %argL, 0(%rsi)
            movupd  %argR, 0(%rdx)
         */
         addInstr(env, AMD64Instr_SseLdSt(False/*!isLoad*/, 16, argL,
                                          AMD64AMode_IR(0, hregAMD64_RSI())));
         addInstr(env, AMD64Instr_SseLdSt(False/*!isLoad*/, 16, argR,
                                          AMD64AMode_IR(0, hregAMD64_RDX())));
         /* call the helper */
         addInstr(env, AMD64Instr_Call( Acc_ALWAYS, (ULong)fn, 3 ));
         /* fetch the result from memory, using %r_argp, which the
            register allocator will keep alive across the call. */
         addInstr(env, AMD64Instr_SseLdSt(True/*isLoad*/, 16, dst,
                                          AMD64AMode_IR(0, argp)));
         /* and finally, clear the space */
         add_to_rsp(env, 112);
         return dst;
      }

      case Iop_SarN64x2: fn = (HWord)h_generic_calc_SarN64x2;
                         goto do_SseAssistedVectorAndScalar;
      case Iop_SarN8x16: fn = (HWord)h_generic_calc_SarN8x16;
                         goto do_SseAssistedVectorAndScalar;
      do_SseAssistedVectorAndScalar: {
         /* RRRufff!  RRRufff code is what we're generating here.  Oh
            well. */
         vassert(fn != 0);
         HReg dst = newVRegV(env);
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselIntExpr_R(env, e->Iex.Binop.arg2);
         HReg argp = newVRegI(env);
         /* subq $112, %rsp         -- make a space*/
         sub_from_rsp(env, 112);
         /* leaq 48(%rsp), %r_argp  -- point into it */
         addInstr(env, AMD64Instr_Lea64(AMD64AMode_IR(48, hregAMD64_RSP()),
                                        argp));
         /* andq $-16, %r_argp      -- 16-align the pointer */
         addInstr(env, AMD64Instr_Alu64R(Aalu_AND,
                                         AMD64RMI_Imm( ~(UInt)15 ), 
                                         argp));
         /* Prepare 2 vector arg regs:
            leaq 0(%r_argp), %rdi
            leaq 16(%r_argp), %rsi
         */
         addInstr(env, AMD64Instr_Lea64(AMD64AMode_IR(0, argp),
                                        hregAMD64_RDI()));
         addInstr(env, AMD64Instr_Lea64(AMD64AMode_IR(16, argp),
                                        hregAMD64_RSI()));
         /* Store the vector arg, at (%rsi):
            movupd  %argL, 0(%rsi)
         */
         addInstr(env, AMD64Instr_SseLdSt(False/*!isLoad*/, 16, argL,
                                          AMD64AMode_IR(0, hregAMD64_RSI())));
         /* And get the scalar value into rdx */
         addInstr(env, mk_iMOVsd_RR(argR, hregAMD64_RDX()));

         /* call the helper */
         addInstr(env, AMD64Instr_Call( Acc_ALWAYS, (ULong)fn, 3 ));
         /* fetch the result from memory, using %r_argp, which the
            register allocator will keep alive across the call. */
         addInstr(env, AMD64Instr_SseLdSt(True/*isLoad*/, 16, dst,
                                          AMD64AMode_IR(0, argp)));
         /* and finally, clear the space */
         add_to_rsp(env, 112);
         return dst;
      }

      default:
         break;
   } /* switch (e->Iex.Binop.op) */
   } /* if (e->tag == Iex_Binop) */

   if (e->tag == Iex_Mux0X) {
      HReg r8  = iselIntExpr_R(env, e->Iex.Mux0X.cond);
      HReg rX  = iselVecExpr(env, e->Iex.Mux0X.exprX);
      HReg r0  = iselVecExpr(env, e->Iex.Mux0X.expr0);
      HReg dst = newVRegV(env);
      addInstr(env, mk_vMOVsd_RR(rX,dst));
      addInstr(env, AMD64Instr_Test64(0xFF, r8));
      addInstr(env, AMD64Instr_SseCMov(Acc_Z,r0,dst));
      return dst;
   }

   //vec_fail:
   vex_printf("iselVecExpr (amd64, subarch = %s): can't reduce\n",
              LibVEX_ppVexHwCaps(VexArchAMD64, env->hwcaps));
   ppIRExpr(e);
   vpanic("iselVecExpr_wrk");
}


/*---------------------------------------------------------*/
/*--- ISEL: SIMD (V256) expressions, into 2 XMM regs.    --*/
/*---------------------------------------------------------*/

static void iselDVecExpr ( /*OUT*/HReg* rHi, /*OUT*/HReg* rLo, 
                           ISelEnv* env, IRExpr* e )
{
   iselDVecExpr_wrk( rHi, rLo, env, e );
#  if 0
   vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
#  endif
   vassert(hregClass(*rHi) == HRcVec128);
   vassert(hregClass(*rLo) == HRcVec128);
   vassert(hregIsVirtual(*rHi));
   vassert(hregIsVirtual(*rLo));
}


/* DO NOT CALL THIS DIRECTLY */
static void iselDVecExpr_wrk ( /*OUT*/HReg* rHi, /*OUT*/HReg* rLo, 
                               ISelEnv* env, IRExpr* e )
{
   vassert(e);
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_V256);

   AMD64SseOp op = Asse_INVALID;

   /* read 256-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair( rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }
 
   if (e->tag == Iex_Get) {
      HReg        vHi  = newVRegV(env);
      HReg        vLo  = newVRegV(env);
      HReg        rbp  = hregAMD64_RBP();
      AMD64AMode* am0  = AMD64AMode_IR(e->Iex.Get.offset + 0,  rbp);
      AMD64AMode* am16 = AMD64AMode_IR(e->Iex.Get.offset + 16, rbp);
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, vLo, am0));
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, vHi, am16));
      *rHi = vHi;
      *rLo = vLo;
      return;
   }

   if (e->tag == Iex_Load) {
      HReg        vHi  = newVRegV(env);
      HReg        vLo  = newVRegV(env);
      HReg        rA   = iselIntExpr_R(env, e->Iex.Load.addr);
      AMD64AMode* am0  = AMD64AMode_IR(0,  rA);
      AMD64AMode* am16 = AMD64AMode_IR(16, rA);
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, vLo, am0));
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, vHi, am16));
      *rHi = vHi;
      *rLo = vLo;
      return;
   }

   if (e->tag == Iex_Const) {
      vassert(e->Iex.Const.con->tag == Ico_V256);
      switch (e->Iex.Const.con->Ico.V256) {
         case 0x00000000: {
            HReg vHi = generate_zeroes_V128(env);
            HReg vLo = newVRegV(env);
            addInstr(env, mk_vMOVsd_RR(vHi, vLo));
            *rHi = vHi;
            *rLo = vLo;
            return;
         }
         default:
            break; /* give up.   Until such time as is necessary. */
      }
   }

   if (e->tag == Iex_Unop) {
   switch (e->Iex.Unop.op) {

      case Iop_NotV256: {
         HReg argHi, argLo;
         iselDVecExpr(&argHi, &argLo, env, e->Iex.Unop.arg);
         *rHi = do_sse_NotV128(env, argHi);
         *rLo = do_sse_NotV128(env, argLo);
         return;
      }

      case Iop_Recip32Fx8: op = Asse_RCPF;   goto do_32Fx8_unary;
      case Iop_Sqrt32Fx8:  op = Asse_SQRTF;  goto do_32Fx8_unary;
      case Iop_RSqrt32Fx8: op = Asse_RSQRTF; goto do_32Fx8_unary;
      do_32Fx8_unary:
      {
         HReg argHi, argLo;
         iselDVecExpr(&argHi, &argLo, env, e->Iex.Unop.arg);
         HReg dstHi = newVRegV(env);
         HReg dstLo = newVRegV(env);
         addInstr(env, AMD64Instr_Sse32Fx4(op, argHi, dstHi));
         addInstr(env, AMD64Instr_Sse32Fx4(op, argLo, dstLo));
         *rHi = dstHi;
         *rLo = dstLo;
         return;
      }

      case Iop_Sqrt64Fx4:  op = Asse_SQRTF;  goto do_64Fx4_unary;
      do_64Fx4_unary:
      {
         HReg argHi, argLo;
         iselDVecExpr(&argHi, &argLo, env, e->Iex.Unop.arg);
         HReg dstHi = newVRegV(env);
         HReg dstLo = newVRegV(env);
         addInstr(env, AMD64Instr_Sse64Fx2(op, argHi, dstHi));
         addInstr(env, AMD64Instr_Sse64Fx2(op, argLo, dstLo));
         *rHi = dstHi;
         *rLo = dstLo;
         return;
      }

      case Iop_CmpNEZ64x4: {
         /* We can use SSE2 instructions for this. */
         /* Same scheme as Iop_CmpNEZ64x2, except twice as wide
            (obviously).  See comment on Iop_CmpNEZ64x2 for
            explanation of what's going on here. */
         HReg argHi, argLo;
         iselDVecExpr(&argHi, &argLo, env, e->Iex.Unop.arg);
         HReg tmpHi  = generate_zeroes_V128(env);
         HReg tmpLo  = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(tmpHi, tmpLo));
         HReg dstHi  = newVRegV(env);
         HReg dstLo  = newVRegV(env);
         addInstr(env, AMD64Instr_SseReRg(Asse_CMPEQ32, argHi, tmpHi));
         addInstr(env, AMD64Instr_SseReRg(Asse_CMPEQ32, argLo, tmpLo));
         tmpHi = do_sse_NotV128(env, tmpHi);
         tmpLo = do_sse_NotV128(env, tmpLo);
         addInstr(env, AMD64Instr_SseShuf(0xB1, tmpHi, dstHi));
         addInstr(env, AMD64Instr_SseShuf(0xB1, tmpLo, dstLo));
         addInstr(env, AMD64Instr_SseReRg(Asse_OR, tmpHi, dstHi));
         addInstr(env, AMD64Instr_SseReRg(Asse_OR, tmpLo, dstLo));
         *rHi = dstHi;
         *rLo = dstLo;
         return;
      }

      case Iop_CmpNEZ32x8: op = Asse_CMPEQ32; goto do_CmpNEZ_vector;
      do_CmpNEZ_vector:
      {
         HReg argHi, argLo;
         iselDVecExpr(&argHi, &argLo, env, e->Iex.Unop.arg);
         HReg tmpHi = newVRegV(env);
         HReg tmpLo = newVRegV(env);
         HReg zero  = generate_zeroes_V128(env);
         HReg dstHi, dstLo;
         addInstr(env, mk_vMOVsd_RR(argHi, tmpHi));
         addInstr(env, mk_vMOVsd_RR(argLo, tmpLo));
         addInstr(env, AMD64Instr_SseReRg(op, zero, tmpHi));
         addInstr(env, AMD64Instr_SseReRg(op, zero, tmpLo));
         dstHi = do_sse_NotV128(env, tmpHi);
         dstLo = do_sse_NotV128(env, tmpLo);
         *rHi = dstHi;
         *rLo = dstLo;
         return;
      }

      default:
         break;
   } /* switch (e->Iex.Unop.op) */
   } /* if (e->tag == Iex_Unop) */

   if (e->tag == Iex_Binop) {
   switch (e->Iex.Binop.op) {

      case Iop_Add64Fx4:   op = Asse_ADDF;   goto do_64Fx4;
      case Iop_Sub64Fx4:   op = Asse_SUBF;   goto do_64Fx4;
      case Iop_Mul64Fx4:   op = Asse_MULF;   goto do_64Fx4;
      case Iop_Div64Fx4:   op = Asse_DIVF;   goto do_64Fx4;
      case Iop_Max64Fx4:   op = Asse_MAXF;   goto do_64Fx4;
      case Iop_Min64Fx4:   op = Asse_MINF;   goto do_64Fx4;
      do_64Fx4:
      {
         HReg argLhi, argLlo, argRhi, argRlo;
         iselDVecExpr(&argLhi, &argLlo, env, e->Iex.Binop.arg1);
         iselDVecExpr(&argRhi, &argRlo, env, e->Iex.Binop.arg2);
         HReg dstHi = newVRegV(env);
         HReg dstLo = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argLhi, dstHi));
         addInstr(env, mk_vMOVsd_RR(argLlo, dstLo));
         addInstr(env, AMD64Instr_Sse64Fx2(op, argRhi, dstHi));
         addInstr(env, AMD64Instr_Sse64Fx2(op, argRlo, dstLo));
         *rHi = dstHi;
         *rLo = dstLo;
         return;
      }

      case Iop_Add32Fx8:   op = Asse_ADDF;   goto do_32Fx8;
      case Iop_Sub32Fx8:   op = Asse_SUBF;   goto do_32Fx8;
      case Iop_Mul32Fx8:   op = Asse_MULF;   goto do_32Fx8;
      case Iop_Div32Fx8:   op = Asse_DIVF;   goto do_32Fx8;
      case Iop_Max32Fx8:   op = Asse_MAXF;   goto do_32Fx8;
      case Iop_Min32Fx8:   op = Asse_MINF;   goto do_32Fx8;
      do_32Fx8:
      {
         HReg argLhi, argLlo, argRhi, argRlo;
         iselDVecExpr(&argLhi, &argLlo, env, e->Iex.Binop.arg1);
         iselDVecExpr(&argRhi, &argRlo, env, e->Iex.Binop.arg2);
         HReg dstHi = newVRegV(env);
         HReg dstLo = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argLhi, dstHi));
         addInstr(env, mk_vMOVsd_RR(argLlo, dstLo));
         addInstr(env, AMD64Instr_Sse32Fx4(op, argRhi, dstHi));
         addInstr(env, AMD64Instr_Sse32Fx4(op, argRlo, dstLo));
         *rHi = dstHi;
         *rLo = dstLo;
         return;
      }

      case Iop_AndV256:    op = Asse_AND;      goto do_SseReRg;
      case Iop_OrV256:     op = Asse_OR;       goto do_SseReRg;
      case Iop_XorV256:    op = Asse_XOR;      goto do_SseReRg;
      do_SseReRg:
      {
         HReg argLhi, argLlo, argRhi, argRlo;
         iselDVecExpr(&argLhi, &argLlo, env, e->Iex.Binop.arg1);
         iselDVecExpr(&argRhi, &argRlo, env, e->Iex.Binop.arg2);
         HReg dstHi = newVRegV(env);
         HReg dstLo = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argLhi, dstHi));
         addInstr(env, mk_vMOVsd_RR(argLlo, dstLo));
         addInstr(env, AMD64Instr_SseReRg(op, argRhi, dstHi));
         addInstr(env, AMD64Instr_SseReRg(op, argRlo, dstLo));
         *rHi = dstHi;
         *rLo = dstLo;
         return;
      }

      case Iop_V128HLtoV256: {
         *rHi = iselVecExpr(env, e->Iex.Binop.arg1);
         *rLo = iselVecExpr(env, e->Iex.Binop.arg2);
         return;
      }

      default:
         break;
   } /* switch (e->Iex.Binop.op) */
   } /* if (e->tag == Iex_Binop) */

   if (e->tag == Iex_Qop && e->Iex.Qop.details->op == Iop_64x4toV256) {
      HReg        rsp     = hregAMD64_RSP();
      HReg        vHi     = newVRegV(env);
      HReg        vLo     = newVRegV(env);
      AMD64AMode* m8_rsp  = AMD64AMode_IR(-8, rsp);
      AMD64AMode* m16_rsp = AMD64AMode_IR(-16, rsp);
      /* arg1 is the most significant (Q3), arg4 the least (Q0) */
      /* Get all the args into regs, before messing with the stack. */
      AMD64RI* q3  = iselIntExpr_RI(env, e->Iex.Qop.details->arg1);
      AMD64RI* q2  = iselIntExpr_RI(env, e->Iex.Qop.details->arg2);
      AMD64RI* q1  = iselIntExpr_RI(env, e->Iex.Qop.details->arg3);
      AMD64RI* q0  = iselIntExpr_RI(env, e->Iex.Qop.details->arg4);
      /* less significant lane (Q2) at the lower address (-16(rsp)) */
      addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, q3, m8_rsp));
      addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, q2, m16_rsp));
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, vHi, m16_rsp));
      /* and then the lower half .. */
      addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, q1, m8_rsp));
      addInstr(env, AMD64Instr_Alu64M(Aalu_MOV, q0, m16_rsp));
      addInstr(env, AMD64Instr_SseLdSt(True/*load*/, 16, vLo, m16_rsp));
      *rHi = vHi;
      *rLo = vLo;
      return;
   }

   //avx_fail:
   vex_printf("iselDVecExpr (amd64, subarch = %s): can't reduce\n",
              LibVEX_ppVexHwCaps(VexArchAMD64, env->hwcaps));
   ppIRExpr(e);
   vpanic("iselDVecExpr_wrk");
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
   case Ist_Store: {
      IRType    tya   = typeOfIRExpr(env->type_env, stmt->Ist.Store.addr);
      IRType    tyd   = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);
      IREndness end   = stmt->Ist.Store.end;

      if (tya != Ity_I64 || end != Iend_LE)
         goto stmt_fail;

      if (tyd == Ity_I64) {
         AMD64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         AMD64RI* ri = iselIntExpr_RI(env, stmt->Ist.Store.data);
         addInstr(env, AMD64Instr_Alu64M(Aalu_MOV,ri,am));
         return;
      }
      if (tyd == Ity_I8 || tyd == Ity_I16 || tyd == Ity_I32) {
         AMD64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         HReg r = iselIntExpr_R(env, stmt->Ist.Store.data);
         addInstr(env, AMD64Instr_Store(
                          toUChar(tyd==Ity_I8 ? 1 : (tyd==Ity_I16 ? 2 : 4)),
                          r,am));
         return;
      }
      if (tyd == Ity_F64) {
         AMD64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         HReg r = iselDblExpr(env, stmt->Ist.Store.data);
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 8, r, am));
         return;
      }
      if (tyd == Ity_F32) {
         AMD64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         HReg r = iselFltExpr(env, stmt->Ist.Store.data);
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 4, r, am));
         return;
      }
      if (tyd == Ity_V128) {
         AMD64AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         HReg r = iselVecExpr(env, stmt->Ist.Store.data);
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, r, am));
         return;
      }
      if (tyd == Ity_V256) {
         HReg        rA   = iselIntExpr_R(env, stmt->Ist.Store.addr);
         AMD64AMode* am0  = AMD64AMode_IR(0,  rA);
         AMD64AMode* am16 = AMD64AMode_IR(16, rA);
         HReg vHi, vLo;
         iselDVecExpr(&vHi, &vLo, env, stmt->Ist.Store.data);
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, vLo, am0));
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, vHi, am16));
         return;
      }
      break;
   }

   /* --------- PUT --------- */
   case Ist_Put: {
      IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
      if (ty == Ity_I64) {
         /* We're going to write to memory, so compute the RHS into an
            AMD64RI. */
         AMD64RI* ri = iselIntExpr_RI(env, stmt->Ist.Put.data);
         addInstr(env,
                  AMD64Instr_Alu64M(
                     Aalu_MOV,
                     ri,
                     AMD64AMode_IR(stmt->Ist.Put.offset,
                                   hregAMD64_RBP())
                 ));
         return;
      }
      if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32) {
         HReg r = iselIntExpr_R(env, stmt->Ist.Put.data);
         addInstr(env, AMD64Instr_Store(
                          toUChar(ty==Ity_I8 ? 1 : (ty==Ity_I16 ? 2 : 4)),
                          r,
                          AMD64AMode_IR(stmt->Ist.Put.offset,
                                        hregAMD64_RBP())));
         return;
      }
      if (ty == Ity_F32) {
         HReg f32 = iselFltExpr(env, stmt->Ist.Put.data);
         AMD64AMode* am = AMD64AMode_IR(stmt->Ist.Put.offset, hregAMD64_RBP());
         set_SSE_rounding_default(env); /* paranoia */
         addInstr(env, AMD64Instr_SseLdSt( False/*store*/, 4, f32, am ));
         return;
      }
      if (ty == Ity_F64) {
         HReg f64 = iselDblExpr(env, stmt->Ist.Put.data);
         AMD64AMode* am = AMD64AMode_IR( stmt->Ist.Put.offset, 
                                         hregAMD64_RBP() );
         addInstr(env, AMD64Instr_SseLdSt( False/*store*/, 8, f64, am ));
         return;
      }
      if (ty == Ity_V128) {
         HReg        vec = iselVecExpr(env, stmt->Ist.Put.data);
         AMD64AMode* am  = AMD64AMode_IR(stmt->Ist.Put.offset, 
                                         hregAMD64_RBP());
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, vec, am));
         return;
      }
      if (ty == Ity_V256) {
         HReg vHi, vLo;
         iselDVecExpr(&vHi, &vLo, env, stmt->Ist.Put.data);
         HReg        rbp  = hregAMD64_RBP();
         AMD64AMode* am0  = AMD64AMode_IR(stmt->Ist.Put.offset + 0,  rbp);
         AMD64AMode* am16 = AMD64AMode_IR(stmt->Ist.Put.offset + 16, rbp);
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, vLo, am0));
         addInstr(env, AMD64Instr_SseLdSt(False/*store*/, 16, vHi, am16));
         return;
      }
      break;
   }

   /* --------- Indexed PUT --------- */
   case Ist_PutI: {
      IRPutI *puti = stmt->Ist.PutI.details;

      AMD64AMode* am 
         = genGuestArrayOffset(
              env, puti->descr, 
                   puti->ix, puti->bias );

      IRType ty = typeOfIRExpr(env->type_env, puti->data);
      if (ty == Ity_F64) {
         HReg val = iselDblExpr(env, puti->data);
         addInstr(env, AMD64Instr_SseLdSt( False/*store*/, 8, val, am ));
         return;
      }
      if (ty == Ity_I8) {
         HReg r = iselIntExpr_R(env, puti->data);
         addInstr(env, AMD64Instr_Store( 1, r, am ));
         return;
      }
      if (ty == Ity_I64) {
         AMD64RI* ri = iselIntExpr_RI(env, puti->data);
         addInstr(env, AMD64Instr_Alu64M( Aalu_MOV, ri, am ));
         return;
      }
      break;
   }

   /* --------- TMP --------- */
   case Ist_WrTmp: {
      IRTemp tmp = stmt->Ist.WrTmp.tmp;
      IRType ty = typeOfIRTemp(env->type_env, tmp);

      /* optimisation: if stmt->Ist.WrTmp.data is Add64(..,..),
         compute it into an AMode and then use LEA.  This usually
         produces fewer instructions, often because (for memcheck
         created IR) we get t = address-expression, (t is later used
         twice) and so doing this naturally turns address-expression
         back into an AMD64 amode. */
      if (ty == Ity_I64 
          && stmt->Ist.WrTmp.data->tag == Iex_Binop
          && stmt->Ist.WrTmp.data->Iex.Binop.op == Iop_Add64) {
         AMD64AMode* am = iselIntExpr_AMode(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         if (am->tag == Aam_IR && am->Aam.IR.imm == 0) {
            /* Hmm, iselIntExpr_AMode wimped out and just computed the
               value into a register.  Just emit a normal reg-reg move
               so reg-alloc can coalesce it away in the usual way. */
            HReg src = am->Aam.IR.reg;
            addInstr(env, AMD64Instr_Alu64R(Aalu_MOV, AMD64RMI_Reg(src), dst));
         } else {
            addInstr(env, AMD64Instr_Lea64(am,dst));
         }
         return;
      }

      if (ty == Ity_I64 || ty == Ity_I32 
          || ty == Ity_I16 || ty == Ity_I8) {
         AMD64RMI* rmi = iselIntExpr_RMI(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, AMD64Instr_Alu64R(Aalu_MOV,rmi,dst));
         return;
      }
      if (ty == Ity_I128) {
         HReg rHi, rLo, dstHi, dstLo;
         iselInt128Expr(&rHi,&rLo, env, stmt->Ist.WrTmp.data);
         lookupIRTempPair( &dstHi, &dstLo, env, tmp);
         addInstr(env, mk_iMOVsd_RR(rHi,dstHi) );
         addInstr(env, mk_iMOVsd_RR(rLo,dstLo) );
         return;
      }
      if (ty == Ity_I1) {
         AMD64CondCode cond = iselCondCode(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, AMD64Instr_Set64(cond, dst));
         return;
      }
      if (ty == Ity_F64) {
         HReg dst = lookupIRTemp(env, tmp);
         HReg src = iselDblExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, mk_vMOVsd_RR(src, dst));
         return;
      }
      if (ty == Ity_F32) {
         HReg dst = lookupIRTemp(env, tmp);
         HReg src = iselFltExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, mk_vMOVsd_RR(src, dst));
         return;
      }
      if (ty == Ity_V128) {
         HReg dst = lookupIRTemp(env, tmp);
         HReg src = iselVecExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, mk_vMOVsd_RR(src, dst));
         return;
      }
      if (ty == Ity_V256) {
         HReg rHi, rLo, dstHi, dstLo;
         iselDVecExpr(&rHi,&rLo, env, stmt->Ist.WrTmp.data);
         lookupIRTempPair( &dstHi, &dstLo, env, tmp);
         addInstr(env, mk_vMOVsd_RR(rHi,dstHi) );
         addInstr(env, mk_vMOVsd_RR(rLo,dstLo) );
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
      if (retty == Ity_I64 || retty == Ity_I32 
          || retty == Ity_I16 || retty == Ity_I8) {
         /* The returned value is in %rax.  Park it in the register
            associated with tmp. */
         HReg dst = lookupIRTemp(env, d->tmp);
         addInstr(env, mk_iMOVsd_RR(hregAMD64_RAX(),dst) );
         return;
      }
      break;
   }

   /* --------- MEM FENCE --------- */
   case Ist_MBE:
      switch (stmt->Ist.MBE.event) {
         case Imbe_Fence:
            addInstr(env, AMD64Instr_MFence());
            return;
         default:
            break;
      }
      break;

   /* --------- ACAS --------- */
   case Ist_CAS:
      if (stmt->Ist.CAS.details->oldHi == IRTemp_INVALID) {
         /* "normal" singleton CAS */
         UChar  sz;
         IRCAS* cas = stmt->Ist.CAS.details;
         IRType ty  = typeOfIRExpr(env->type_env, cas->dataLo);
         /* get: cas->expd into %rax, and cas->data into %rbx */
         AMD64AMode* am = iselIntExpr_AMode(env, cas->addr);
         HReg rData = iselIntExpr_R(env, cas->dataLo);
         HReg rExpd = iselIntExpr_R(env, cas->expdLo);
         HReg rOld  = lookupIRTemp(env, cas->oldLo);
         vassert(cas->expdHi == NULL);
         vassert(cas->dataHi == NULL);
         addInstr(env, mk_iMOVsd_RR(rExpd, rOld));
         addInstr(env, mk_iMOVsd_RR(rExpd, hregAMD64_RAX()));
         addInstr(env, mk_iMOVsd_RR(rData, hregAMD64_RBX()));
         switch (ty) { 
            case Ity_I64: sz = 8; break;
            case Ity_I32: sz = 4; break;
            case Ity_I16: sz = 2; break;
            case Ity_I8:  sz = 1; break; 
            default: goto unhandled_cas;
         }
         addInstr(env, AMD64Instr_ACAS(am, sz));
         addInstr(env, AMD64Instr_CMov64(
                          Acc_NZ, AMD64RM_Reg(hregAMD64_RAX()), rOld));
         return;
      } else {
         /* double CAS */
         UChar  sz;
         IRCAS* cas = stmt->Ist.CAS.details;
         IRType ty  = typeOfIRExpr(env->type_env, cas->dataLo);
         /* only 32-bit and 64-bit allowed in this case */
         /* get: cas->expdLo into %rax, and cas->dataLo into %rbx */
         /* get: cas->expdHi into %rdx, and cas->dataHi into %rcx */
         AMD64AMode* am = iselIntExpr_AMode(env, cas->addr);
         HReg rDataHi = iselIntExpr_R(env, cas->dataHi);
         HReg rDataLo = iselIntExpr_R(env, cas->dataLo);
         HReg rExpdHi = iselIntExpr_R(env, cas->expdHi);
         HReg rExpdLo = iselIntExpr_R(env, cas->expdLo);
         HReg rOldHi  = lookupIRTemp(env, cas->oldHi);
         HReg rOldLo  = lookupIRTemp(env, cas->oldLo);
         switch (ty) { 
            case Ity_I64:
               if (!(env->hwcaps & VEX_HWCAPS_AMD64_CX16))
                  goto unhandled_cas; /* we'd have to generate
                                         cmpxchg16b, but the host
                                         doesn't support that */
               sz = 8;
               break;
            case Ity_I32:
               sz = 4;
               break;
            default:
               goto unhandled_cas;
         }
         addInstr(env, mk_iMOVsd_RR(rExpdHi, rOldHi));
         addInstr(env, mk_iMOVsd_RR(rExpdLo, rOldLo));
         addInstr(env, mk_iMOVsd_RR(rExpdHi, hregAMD64_RDX()));
         addInstr(env, mk_iMOVsd_RR(rExpdLo, hregAMD64_RAX()));
         addInstr(env, mk_iMOVsd_RR(rDataHi, hregAMD64_RCX()));
         addInstr(env, mk_iMOVsd_RR(rDataLo, hregAMD64_RBX()));
         addInstr(env, AMD64Instr_DACAS(am, sz));
         addInstr(env,
                  AMD64Instr_CMov64(
                     Acc_NZ, AMD64RM_Reg(hregAMD64_RDX()), rOldHi));
         addInstr(env,
                  AMD64Instr_CMov64(
                     Acc_NZ, AMD64RM_Reg(hregAMD64_RAX()), rOldLo));
         return;
      }
      unhandled_cas:
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
         vpanic("iselStmt(amd64): Ist_Exit: dst is not a 64-bit value");

      AMD64CondCode cc    = iselCondCode(env, stmt->Ist.Exit.guard);
      AMD64AMode*   amRIP = AMD64AMode_IR(stmt->Ist.Exit.offsIP,
                                          hregAMD64_RBP());

      /* Case: boring transfer to known address */
      if (stmt->Ist.Exit.jk == Ijk_Boring) {
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = ((Addr64)stmt->Ist.Exit.dst->Ico.U64) > env->max_ga;
            if (0) vex_printf("%s", toFastEP ? "Y" : ",");
            addInstr(env, AMD64Instr_XDirect(stmt->Ist.Exit.dst->Ico.U64,
                                             amRIP, cc, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, AMD64Instr_XAssisted(r, amRIP, cc, Ijk_Boring));
         }
         return;
      }

      /* Case: assisted transfer to arbitrary address */
      switch (stmt->Ist.Exit.jk) {
         /* Keep this list in sync with that in iselNext below */
         case Ijk_ClientReq:
         case Ijk_EmWarn:
         case Ijk_NoDecode:
         case Ijk_NoRedir:
         case Ijk_SigSEGV:
         case Ijk_SigTRAP:
         case Ijk_Sys_syscall:
         case Ijk_TInval:
         case Ijk_Yield:
         {
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, AMD64Instr_XAssisted(r, amRIP, cc, stmt->Ist.Exit.jk));
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
   vpanic("iselStmt(amd64)");
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
         AMD64AMode* amRIP = AMD64AMode_IR(offsIP, hregAMD64_RBP());
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = ((Addr64)cdst->Ico.U64) > env->max_ga;
            if (0) vex_printf("%s", toFastEP ? "X" : ".");
            addInstr(env, AMD64Instr_XDirect(cdst->Ico.U64, 
                                             amRIP, Acc_ALWAYS, 
                                             toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an indirect transfer,
               as that's the cheapest alternative that is
               allowable. */
            HReg r = iselIntExpr_R(env, next);
            addInstr(env, AMD64Instr_XAssisted(r, amRIP, Acc_ALWAYS,
                                               Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
      case Ijk_Boring: case Ijk_Ret: case Ijk_Call: {
         HReg        r     = iselIntExpr_R(env, next);
         AMD64AMode* amRIP = AMD64AMode_IR(offsIP, hregAMD64_RBP());
         if (env->chainingAllowed) {
            addInstr(env, AMD64Instr_XIndir(r, amRIP, Acc_ALWAYS));
         } else {
            addInstr(env, AMD64Instr_XAssisted(r, amRIP, Acc_ALWAYS,
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
      case Ijk_EmWarn:
      case Ijk_NoDecode:
      case Ijk_NoRedir:
      case Ijk_SigSEGV:
      case Ijk_SigTRAP:
      case Ijk_Sys_syscall:
      case Ijk_TInval:
      case Ijk_Yield: {
         HReg        r     = iselIntExpr_R(env, next);
         AMD64AMode* amRIP = AMD64AMode_IR(offsIP, hregAMD64_RBP());
         addInstr(env, AMD64Instr_XAssisted(r, amRIP, Acc_ALWAYS, jk));
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

/* Translate an entire SB to amd64 code. */

HInstrArray* iselSB_AMD64 ( IRSB* bb,
                            VexArch      arch_host,
                            VexArchInfo* archinfo_host,
                            VexAbiInfo*  vbi/*UNUSED*/,
                            Int offs_Host_EvC_Counter,
                            Int offs_Host_EvC_FailAddr,
                            Bool chainingAllowed,
                            Bool addProfInc,
                            Addr64 max_ga )
{
   Int        i, j;
   HReg       hreg, hregHI;
   ISelEnv*   env;
   UInt       hwcaps_host = archinfo_host->hwcaps;
   AMD64AMode *amCounter, *amFailAddr;

   /* sanity ... */
   vassert(arch_host == VexArchAMD64);
   vassert(0 == (hwcaps_host
                 & ~(VEX_HWCAPS_AMD64_SSE3
                     | VEX_HWCAPS_AMD64_CX16
                     | VEX_HWCAPS_AMD64_LZCNT
                     | VEX_HWCAPS_AMD64_AVX)));

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
         case Ity_I8: case Ity_I16: case Ity_I32: case Ity_I64:
            hreg = mkHReg(j++, HRcInt64, True);
            break;
         case Ity_I128:
            hreg   = mkHReg(j++, HRcInt64, True);
            hregHI = mkHReg(j++, HRcInt64, True);
            break;
         case Ity_F32:
         case Ity_F64:
         case Ity_V128:
            hreg = mkHReg(j++, HRcVec128, True);
            break;
         case Ity_V256:
            hreg   = mkHReg(j++, HRcVec128, True);
            hregHI = mkHReg(j++, HRcVec128, True);
            break;
         default:
            ppIRType(bb->tyenv->types[i]);
            vpanic("iselBB(amd64): IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   amCounter  = AMD64AMode_IR(offs_Host_EvC_Counter,  hregAMD64_RBP());
   amFailAddr = AMD64AMode_IR(offs_Host_EvC_FailAddr, hregAMD64_RBP());
   addInstr(env, AMD64Instr_EvCheck(amCounter, amFailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, AMD64Instr_ProfInc());
   }

   /* Ok, finally we can iterate over the statements. */
   for (i = 0; i < bb->stmts_used; i++)
      if (bb->stmts[i])
         iselStmt(env, bb->stmts[i]);

   iselNext(env, bb->next, bb->jumpkind, bb->offsIP);

   /* record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;
   return env->code;
}


/*---------------------------------------------------------------*/
/*--- end                                   host_amd64_isel.c ---*/
/*---------------------------------------------------------------*/
