
/*---------------------------------------------------------------*/
/*--- begin                                   host_x86_isel.c ---*/
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
#include "host_generic_simd128.h"
#include "host_x86_defs.h"

/* TODO 21 Apr 2005:

   -- (Really an assembler issue) don't emit CMov32 as a cmov
      insn, since that's expensive on P4 and conditional branch
      is cheaper if (as we expect) the condition is highly predictable

   -- preserve xmm registers across function calls (by declaring them
      as trashed by call insns)

   -- preserve x87 ST stack discipline across function calls.  Sigh.

   -- Check doHelperCall: if a call is conditional, we cannot safely
      compute any regparm args directly to registers.  Hence, the
      fast-regparm marshalling should be restricted to unconditional
      calls only.
*/

/*---------------------------------------------------------*/
/*--- x87 control word stuff                            ---*/
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

/* debugging only, do not use */
/* define DEFAULT_FPUCW 0x037F */


/*---------------------------------------------------------*/
/*--- misc helpers                                      ---*/
/*---------------------------------------------------------*/

/* These are duplicated in guest-x86/toIR.c */
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

static Bool isZeroU8 ( IRExpr* e )
{
   return e->tag == Iex_Const
          && e->Iex.Const.con->tag == Ico_U8
          && e->Iex.Const.con->Ico.U8 == 0;
}

static Bool isZeroU32 ( IRExpr* e )
{
   return e->tag == Iex_Const
          && e->Iex.Const.con->tag == Ico_U32
          && e->Iex.Const.con->Ico.U32 == 0;
}

//static Bool isZeroU64 ( IRExpr* e )
//{
//   return e->tag == Iex_Const
//          && e->Iex.Const.con->tag == Ico_U64
//          && e->Iex.Const.con->Ico.U64 == 0ULL;
//}


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
      Addr32       max_ga;

      /* These are modified as we go along. */
      HInstrArray* code;
      Int          vreg_ctr;
   }
   ISelEnv;


static HReg lookupIRTemp ( ISelEnv* env, IRTemp tmp )
{
   vassert(tmp < env->n_vregmap);
   return env->vregmap[tmp];
}

static void lookupIRTemp64 ( HReg* vrHI, HReg* vrLO, ISelEnv* env, IRTemp tmp )
{
   vassert(tmp < env->n_vregmap);
   vassert(! hregIsInvalid(env->vregmapHI[tmp]));
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr ( ISelEnv* env, X86Instr* instr )
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppX86Instr(instr, False);
      vex_printf("\n");
   }
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcInt32, 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcFlt64, 0/*enc*/, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegV ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcVec128, 0/*enc*/, env->vreg_ctr);
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
static X86RMI*     iselIntExpr_RMI_wrk ( ISelEnv* env, const IRExpr* e );
static X86RMI*     iselIntExpr_RMI     ( ISelEnv* env, const IRExpr* e );

static X86RI*      iselIntExpr_RI_wrk ( ISelEnv* env, const IRExpr* e );
static X86RI*      iselIntExpr_RI     ( ISelEnv* env, const IRExpr* e );

static X86RM*      iselIntExpr_RM_wrk ( ISelEnv* env, const IRExpr* e );
static X86RM*      iselIntExpr_RM     ( ISelEnv* env, const IRExpr* e );

static HReg        iselIntExpr_R_wrk ( ISelEnv* env, const IRExpr* e );
static HReg        iselIntExpr_R     ( ISelEnv* env, const IRExpr* e );

static X86AMode*   iselIntExpr_AMode_wrk ( ISelEnv* env, const IRExpr* e );
static X86AMode*   iselIntExpr_AMode     ( ISelEnv* env, const IRExpr* e );

static void        iselInt64Expr_wrk ( HReg* rHi, HReg* rLo, 
                                       ISelEnv* env, const IRExpr* e );
static void        iselInt64Expr     ( HReg* rHi, HReg* rLo, 
                                       ISelEnv* env, const IRExpr* e );

static X86CondCode iselCondCode_wrk ( ISelEnv* env, const IRExpr* e );
static X86CondCode iselCondCode     ( ISelEnv* env, const IRExpr* e );

static HReg        iselDblExpr_wrk ( ISelEnv* env, const IRExpr* e );
static HReg        iselDblExpr     ( ISelEnv* env, const IRExpr* e );

static HReg        iselFltExpr_wrk ( ISelEnv* env, const IRExpr* e );
static HReg        iselFltExpr     ( ISelEnv* env, const IRExpr* e );

static HReg        iselVecExpr_wrk ( ISelEnv* env, const IRExpr* e );
static HReg        iselVecExpr     ( ISelEnv* env, const IRExpr* e );


/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

/* Make a int reg-reg move. */

static X86Instr* mk_iMOVsd_RR ( HReg src, HReg dst )
{
   vassert(hregClass(src) == HRcInt32);
   vassert(hregClass(dst) == HRcInt32);
   return X86Instr_Alu32R(Xalu_MOV, X86RMI_Reg(src), dst);
}


/* Make a vector reg-reg move. */

static X86Instr* mk_vMOVsd_RR ( HReg src, HReg dst )
{
   vassert(hregClass(src) == HRcVec128);
   vassert(hregClass(dst) == HRcVec128);
   return X86Instr_SseReRg(Xsse_MOV, src, dst);
}

/* Advance/retreat %esp by n. */

static void add_to_esp ( ISelEnv* env, Int n )
{
   vassert(n > 0 && n < 256 && (n%4) == 0);
   addInstr(env, 
            X86Instr_Alu32R(Xalu_ADD, X86RMI_Imm(n), hregX86_ESP()));
}

static void sub_from_esp ( ISelEnv* env, Int n )
{
   vassert(n > 0 && n < 256 && (n%4) == 0);
   addInstr(env, 
            X86Instr_Alu32R(Xalu_SUB, X86RMI_Imm(n), hregX86_ESP()));
}


/* Given an amode, return one which references 4 bytes further
   along. */

static X86AMode* advance4 ( X86AMode* am )
{
   X86AMode* am4 = dopyX86AMode(am);
   switch (am4->tag) {
      case Xam_IRRS:
         am4->Xam.IRRS.imm += 4; break;
      case Xam_IR:
         am4->Xam.IR.imm += 4; break;
      default:
         vpanic("advance4(x86,host)");
   }
   return am4;
}


/* Push an arg onto the host stack, in preparation for a call to a
   helper function of some kind.  Returns the number of 32-bit words
   pushed.  If we encounter an IRExpr_VECRET() then we expect that
   r_vecRetAddr will be a valid register, that holds the relevant
   address. 
*/
static Int pushArg ( ISelEnv* env, IRExpr* arg, HReg r_vecRetAddr )
{
   if (UNLIKELY(arg->tag == Iex_VECRET)) {
      vassert(0); //ATC
      vassert(!hregIsInvalid(r_vecRetAddr));
      addInstr(env, X86Instr_Push(X86RMI_Reg(r_vecRetAddr)));
      return 1;
   }
   if (UNLIKELY(arg->tag == Iex_GSPTR)) {
      addInstr(env, X86Instr_Push(X86RMI_Reg(hregX86_EBP())));
      return 1;
   }
   /* Else it's a "normal" expression. */
   IRType arg_ty = typeOfIRExpr(env->type_env, arg);
   if (arg_ty == Ity_I32) {
      addInstr(env, X86Instr_Push(iselIntExpr_RMI(env, arg)));
      return 1;
   } else 
   if (arg_ty == Ity_I64) {
      HReg rHi, rLo;
      iselInt64Expr(&rHi, &rLo, env, arg);
      addInstr(env, X86Instr_Push(X86RMI_Reg(rHi)));
      addInstr(env, X86Instr_Push(X86RMI_Reg(rLo)));
      return 2;
   }
   ppIRExpr(arg);
   vpanic("pushArg(x86): can't handle arg of this type");
}


/* Complete the call to a helper function, by calling the 
   helper and clearing the args off the stack. */

static 
void callHelperAndClearArgs ( ISelEnv* env, X86CondCode cc, 
                              IRCallee* cee, Int n_arg_ws,
                              RetLoc rloc )
{
   /* Complication.  Need to decide which reg to use as the fn address
      pointer, in a way that doesn't trash regparm-passed
      parameters. */
   vassert(sizeof(void*) == 4);

   addInstr(env, X86Instr_Call( cc, (Addr)cee->addr,
                                cee->regparms, rloc));
   if (n_arg_ws > 0)
      add_to_esp(env, 4*n_arg_ws);
}


/* Used only in doHelperCall.  See big comment in doHelperCall re
   handling of regparm args.  This function figures out whether
   evaluation of an expression might require use of a fixed register.
   If in doubt return True (safe but suboptimal).  
*/
static
Bool mightRequireFixedRegs ( IRExpr* e )
{
   if (UNLIKELY(is_IRExpr_VECRET_or_GSPTR(e))) {
      // These are always "safe" -- either a copy of %esp in some
      // arbitrary vreg, or a copy of %ebp, respectively.
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
   after the call is done. */

static
void doHelperCall ( /*OUT*/UInt*   stackAdjustAfterCall,
                    /*OUT*/RetLoc* retloc,
                    ISelEnv* env,
                    IRExpr* guard,
                    IRCallee* cee, IRType retTy, IRExpr** args )
{
   X86CondCode cc;
   HReg        argregs[3];
   HReg        tmpregs[3];
   Bool        danger;
   Int         not_done_yet, n_args, n_arg_ws, stack_limit, 
               i, argreg, argregX;

   /* Set default returns.  We'll update them later if needed. */
   *stackAdjustAfterCall = 0;
   *retloc               = mk_RetLoc_INVALID();

   /* These are used for cross-checking that IR-level constraints on
      the use of Iex_VECRET and Iex_GSPTR are observed. */
   UInt nVECRETs = 0;
   UInt nGSPTRs  = 0;

   /* Marshal args for a call, do the call, and clear the stack.
      Complexities to consider:

      * The return type can be I{64,32,16,8} or V128.  In the V128
        case, it is expected that |args| will contain the special
        node IRExpr_VECRET(), in which case this routine generates
        code to allocate space on the stack for the vector return
        value.  Since we are not passing any scalars on the stack, it
        is enough to preallocate the return space before marshalling
        any arguments, in this case.

        |args| may also contain IRExpr_GSPTR(), in which case the
        value in %ebp is passed as the corresponding argument.

      * If the callee claims regparmness of 1, 2 or 3, we must pass the
        first 1, 2 or 3 args in registers (EAX, EDX, and ECX
        respectively).  To keep things relatively simple, only args of
        type I32 may be passed as regparms -- just bomb out if anything
        else turns up.  Clearly this depends on the front ends not
        trying to pass any other types as regparms.  
   */

   /* 16 Nov 2004: the regparm handling is complicated by the
      following problem.

      Consider a call two a function with two regparm parameters:
      f(e1,e2).  We need to compute e1 into %eax and e2 into %edx.
      Suppose code is first generated to compute e1 into %eax.  Then,
      code is generated to compute e2 into %edx.  Unfortunately, if
      the latter code sequence uses %eax, it will trash the value of
      e1 computed by the former sequence.  This could happen if (for
      example) e2 itself involved a function call.  In the code below,
      args are evaluated right-to-left, not left-to-right, but the
      principle and the problem are the same.

      One solution is to compute all regparm-bound args into vregs
      first, and once they are all done, move them to the relevant
      real regs.  This always gives correct code, but it also gives
      a bunch of vreg-to-rreg moves which are usually redundant but 
      are hard for the register allocator to get rid of.

      A compromise is to first examine all regparm'd argument 
      expressions.  If they are all so simple that it is clear 
      they will be evaluated without use of any fixed registers,
      use the old compute-directly-to-fixed-target scheme.  If not,
      be safe and use the via-vregs scheme.

      Note this requires being able to examine an expression and
      determine whether or not evaluation of it might use a fixed
      register.  That requires knowledge of how the rest of this
      insn selector works.  Currently just the following 3 are 
      regarded as safe -- hopefully they cover the majority of
      arguments in practice: IRExpr_Tmp IRExpr_Const IRExpr_Get.
   */
   vassert(cee->regparms >= 0 && cee->regparms <= 3);

   /* Count the number of args and also the VECRETs */
   n_args = n_arg_ws = 0;
   while (args[n_args]) {
      IRExpr* arg = args[n_args];
      n_args++;
      if (UNLIKELY(arg->tag == Iex_VECRET)) {
         nVECRETs++;
      } else if (UNLIKELY(arg->tag == Iex_GSPTR)) {
         nGSPTRs++;
      }
   }

   /* If this fails, the IR is ill-formed */
   vassert(nGSPTRs == 0 || nGSPTRs == 1);

   /* If we have a VECRET, allocate space on the stack for the return
      value, and record the stack pointer after that. */
   HReg r_vecRetAddr = INVALID_HREG;
   if (nVECRETs == 1) {
      vassert(retTy == Ity_V128 || retTy == Ity_V256);
      vassert(retTy != Ity_V256); // we don't handle that yet (if ever)
      r_vecRetAddr = newVRegI(env);
      sub_from_esp(env, 16);
      addInstr(env, mk_iMOVsd_RR( hregX86_ESP(), r_vecRetAddr ));
   } else {
      // If either of these fail, the IR is ill-formed
      vassert(retTy != Ity_V128 && retTy != Ity_V256);
      vassert(nVECRETs == 0);
   }

   not_done_yet = n_args;

   stack_limit = cee->regparms;

   /* ------ BEGIN marshall all arguments ------ */

   /* Push (R to L) the stack-passed args, [n_args-1 .. stack_limit] */
   for (i = n_args-1; i >= stack_limit; i--) {
      n_arg_ws += pushArg(env, args[i], r_vecRetAddr);
      not_done_yet--;
   }

   /* args [stack_limit-1 .. 0] and possibly %ebp are to be passed in
      registers. */

   if (cee->regparms > 0) {

      /* ------ BEGIN deal with regparms ------ */

      /* deal with regparms, not forgetting %ebp if needed. */
      argregs[0] = hregX86_EAX();
      argregs[1] = hregX86_EDX();
      argregs[2] = hregX86_ECX();
      tmpregs[0] = tmpregs[1] = tmpregs[2] = INVALID_HREG;

      argreg = cee->regparms;

      /* In keeping with big comment above, detect potential danger
         and use the via-vregs scheme if needed. */
      danger = False;
      for (i = stack_limit-1; i >= 0; i--) {
         if (mightRequireFixedRegs(args[i])) {
            danger = True;
            break;
         }
      }

      if (danger) {

         /* Move via temporaries */
         argregX = argreg;
         for (i = stack_limit-1; i >= 0; i--) {

            if (0) {
               vex_printf("x86 host: register param is complex: ");
               ppIRExpr(args[i]);
               vex_printf("\n");
            }

            IRExpr* arg = args[i];
            argreg--;
            vassert(argreg >= 0);
            if (UNLIKELY(arg->tag == Iex_VECRET)) {
               vassert(0); //ATC
            }
            else if (UNLIKELY(arg->tag == Iex_GSPTR)) {
               vassert(0); //ATC
            } else {
               vassert(typeOfIRExpr(env->type_env, arg) == Ity_I32);
               tmpregs[argreg] = iselIntExpr_R(env, arg);
            }
            not_done_yet--;
         }
         for (i = stack_limit-1; i >= 0; i--) {
            argregX--;
            vassert(argregX >= 0);
            addInstr( env, mk_iMOVsd_RR( tmpregs[argregX], argregs[argregX] ) );
         }

      } else {
         /* It's safe to compute all regparm args directly into their
            target registers. */
         for (i = stack_limit-1; i >= 0; i--) {
            IRExpr* arg = args[i];
            argreg--;
            vassert(argreg >= 0);
            if (UNLIKELY(arg->tag == Iex_VECRET)) {
               vassert(!hregIsInvalid(r_vecRetAddr));
               addInstr(env, X86Instr_Alu32R(Xalu_MOV,
                                             X86RMI_Reg(r_vecRetAddr),
                                             argregs[argreg]));
            }
            else if (UNLIKELY(arg->tag == Iex_GSPTR)) {
               vassert(0); //ATC
            } else {
               vassert(typeOfIRExpr(env->type_env, arg) == Ity_I32);
               addInstr(env, X86Instr_Alu32R(Xalu_MOV, 
                                             iselIntExpr_RMI(env, arg),
                                             argregs[argreg]));
            }
            not_done_yet--;
         }

      }

      /* ------ END deal with regparms ------ */

   }

   vassert(not_done_yet == 0);

   /* ------ END marshall all arguments ------ */

   /* Now we can compute the condition.  We can't do it earlier
      because the argument computations could trash the condition
      codes.  Be a bit clever to handle the common case where the
      guard is 1:Bit. */
   cc = Xcc_ALWAYS;
   if (guard) {
      if (guard->tag == Iex_Const 
          && guard->Iex.Const.con->tag == Ico_U1
          && guard->Iex.Const.con->Ico.U1 == True) {
         /* unconditional -- do nothing */
      } else {
         cc = iselCondCode( env, guard );
      }
   }

   /* Do final checks, set the return values, and generate the call
      instruction proper. */
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
   callHelperAndClearArgs( env, cc, cee, n_arg_ws, *retloc );
}


/* Given a guest-state array descriptor, an index expression and a
   bias, generate an X86AMode holding the relevant guest state
   offset. */

static
X86AMode* genGuestArrayOffset ( ISelEnv* env, IRRegArray* descr, 
                                IRExpr* off, Int bias )
{
   HReg tmp, roff;
   Int  elemSz = sizeofIRType(descr->elemTy);
   Int  nElems = descr->nElems;
   Int  shift  = 0;

   /* throw out any cases not generated by an x86 front end.  In
      theory there might be a day where we need to handle them -- if
      we ever run non-x86-guest on x86 host. */

   if (nElems != 8) 
      vpanic("genGuestArrayOffset(x86 host)(1)");

   switch (elemSz) {
      case 1:  shift = 0; break;
      case 4:  shift = 2; break;
      case 8:  shift = 3; break;
      default: vpanic("genGuestArrayOffset(x86 host)(2)");
   }

   /* Compute off into a reg, %off.  Then return:

         movl %off, %tmp
         addl $bias, %tmp  (if bias != 0)
         andl %tmp, 7
         ... base(%ebp, %tmp, shift) ...
   */
   tmp  = newVRegI(env);
   roff = iselIntExpr_R(env, off);
   addInstr(env, mk_iMOVsd_RR(roff, tmp));
   if (bias != 0) {
      addInstr(env, 
               X86Instr_Alu32R(Xalu_ADD, X86RMI_Imm(bias), tmp));
   }
   addInstr(env, 
            X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(7), tmp));
   return
      X86AMode_IRRS( descr->base, hregX86_EBP(), tmp, shift );
}


/* Mess with the FPU's rounding mode: set to the default rounding mode
   (DEFAULT_FPUCW). */
static 
void set_FPU_rounding_default ( ISelEnv* env )
{
   /* pushl $DEFAULT_FPUCW
      fldcw 0(%esp)
      addl $4, %esp 
   */
   X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());
   addInstr(env, X86Instr_Push(X86RMI_Imm(DEFAULT_FPUCW)));
   addInstr(env, X86Instr_FpLdCW(zero_esp));
   add_to_esp(env, 4);
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
   X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());

   /* movl  %rrm, %rrm2
      andl  $3, %rrm2   -- shouldn't be needed; paranoia
      shll  $10, %rrm2
      orl   $DEFAULT_FPUCW, %rrm2
      pushl %rrm2
      fldcw 0(%esp)
      addl  $4, %esp
   */
   addInstr(env, mk_iMOVsd_RR(rrm, rrm2));
   addInstr(env, X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(3), rrm2));
   addInstr(env, X86Instr_Sh32(Xsh_SHL, 10, rrm2));
   addInstr(env, X86Instr_Alu32R(Xalu_OR, X86RMI_Imm(DEFAULT_FPUCW), rrm2));
   addInstr(env, X86Instr_Push(X86RMI_Reg(rrm2)));
   addInstr(env, X86Instr_FpLdCW(zero_esp));
   add_to_esp(env, 4);
}


/* Generate !src into a new vector register, and be sure that the code
   is SSE1 compatible.  Amazing that Intel doesn't offer a less crappy
   way to do this. 
*/
static HReg do_sse_Not128 ( ISelEnv* env, HReg src )
{
   HReg dst = newVRegV(env);
   /* Set dst to zero.  If dst contains a NaN then all hell might
      break loose after the comparison.  So, first zero it. */
   addInstr(env, X86Instr_SseReRg(Xsse_XOR, dst, dst));
   /* And now make it all 1s ... */
   addInstr(env, X86Instr_Sse32Fx4(Xsse_CMPEQF, dst, dst));
   /* Finally, xor 'src' into it. */
   addInstr(env, X86Instr_SseReRg(Xsse_XOR, src, dst));
   /* Doesn't that just totally suck? */
   return dst;
}


/* Round an x87 FPU value to 53-bit-mantissa precision, to be used
   after most non-simple FPU operations (simple = +, -, *, / and
   sqrt).

   This could be done a lot more efficiently if needed, by loading
   zero and adding it to the value to be rounded (fldz ; faddp?).
*/
static void roundToF64 ( ISelEnv* env, HReg reg )
{
   X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());
   sub_from_esp(env, 8);
   addInstr(env, X86Instr_FpLdSt(False/*store*/, 8, reg, zero_esp));
   addInstr(env, X86Instr_FpLdSt(True/*load*/, 8, reg, zero_esp));
   add_to_esp(env, 8);
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

static HReg iselIntExpr_R ( ISelEnv* env, const IRExpr* e )
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
static HReg iselIntExpr_R_wrk ( ISelEnv* env, const IRExpr* e )
{
   MatchInfo mi;

   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   switch (e->tag) {

   /* --------- TEMP --------- */
   case Iex_RdTmp: {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   /* --------- LOAD --------- */
   case Iex_Load: {
      HReg dst = newVRegI(env);
      X86AMode* amode = iselIntExpr_AMode ( env, e->Iex.Load.addr );

      /* We can't handle big-endian loads, nor load-linked. */
      if (e->Iex.Load.end != Iend_LE)
         goto irreducible;

      if (ty == Ity_I32) {
         addInstr(env, X86Instr_Alu32R(Xalu_MOV,
                                       X86RMI_Mem(amode), dst) );
         return dst;
      }
      if (ty == Ity_I16) {
         addInstr(env, X86Instr_LoadEX(2,False,amode,dst));
         return dst;
      }
      if (ty == Ity_I8) {
         addInstr(env, X86Instr_LoadEX(1,False,amode,dst));
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
         HReg junk = newVRegF(env);
         HReg dst  = newVRegI(env);
         HReg srcL = iselDblExpr(env, triop->arg2);
         HReg srcR = iselDblExpr(env, triop->arg3);
         /* XXXROUNDINGFIXME */
         /* set roundingmode here */
         addInstr(env, X86Instr_FpBinary(
                           e->Iex.Binop.op==Iop_PRemC3210F64 
                              ? Xfp_PREM : Xfp_PREM1,
                           srcL,srcR,junk
                 ));
         /* The previous pseudo-insn will have left the FPU's C3210
            flags set correctly.  So bag them. */
         addInstr(env, X86Instr_FpStSW_AX());
         addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), dst));
         addInstr(env, X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(0x4700), dst));
         return dst;
      }

      break;
   }

   /* --------- BINARY OP --------- */
   case Iex_Binop: {
      X86AluOp   aluOp;
      X86ShiftOp shOp;

      /* Pattern: Sub32(0,x) */
      if (e->Iex.Binop.op == Iop_Sub32 && isZeroU32(e->Iex.Binop.arg1)) {
         HReg dst = newVRegI(env);
         HReg reg = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(reg,dst));
         addInstr(env, X86Instr_Unary32(Xun_NEG,dst));
         return dst;
      }

      /* Is it an addition or logical style op? */
      switch (e->Iex.Binop.op) {
         case Iop_Add8: case Iop_Add16: case Iop_Add32:
            aluOp = Xalu_ADD; break;
         case Iop_Sub8: case Iop_Sub16: case Iop_Sub32: 
            aluOp = Xalu_SUB; break;
         case Iop_And8: case Iop_And16: case Iop_And32: 
            aluOp = Xalu_AND; break;
         case Iop_Or8: case Iop_Or16: case Iop_Or32:  
            aluOp = Xalu_OR; break;
         case Iop_Xor8: case Iop_Xor16: case Iop_Xor32: 
            aluOp = Xalu_XOR; break;
         case Iop_Mul16: case Iop_Mul32: 
            aluOp = Xalu_MUL; break;
         default:
            aluOp = Xalu_INVALID; break;
      }
      /* For commutative ops we assume any literal
         values are on the second operand. */
      if (aluOp != Xalu_INVALID) {
         HReg dst    = newVRegI(env);
         HReg reg    = iselIntExpr_R(env, e->Iex.Binop.arg1);
         X86RMI* rmi = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(reg,dst));
         addInstr(env, X86Instr_Alu32R(aluOp, rmi, dst));
         return dst;
      }
      /* Could do better here; forcing the first arg into a reg
         isn't always clever.
         -- t70 = Xor32(And32(Xor32(LDle:I32(Add32(t41,0xFFFFFFA0:I32)),
                        LDle:I32(Add32(t41,0xFFFFFFA4:I32))),LDle:I32(Add32(
                        t41,0xFFFFFFA8:I32))),LDle:I32(Add32(t41,0xFFFFFFA0:I32)))
            movl 0xFFFFFFA0(%vr41),%vr107
            movl 0xFFFFFFA4(%vr41),%vr108
            movl %vr107,%vr106
            xorl %vr108,%vr106
            movl 0xFFFFFFA8(%vr41),%vr109
            movl %vr106,%vr105
            andl %vr109,%vr105
            movl 0xFFFFFFA0(%vr41),%vr110
            movl %vr105,%vr104
            xorl %vr110,%vr104
            movl %vr104,%vr70
      */

      /* Perhaps a shift op? */
      switch (e->Iex.Binop.op) {
         case Iop_Shl32: case Iop_Shl16: case Iop_Shl8:
            shOp = Xsh_SHL; break;
         case Iop_Shr32: case Iop_Shr16: case Iop_Shr8: 
            shOp = Xsh_SHR; break;
         case Iop_Sar32: case Iop_Sar16: case Iop_Sar8: 
            shOp = Xsh_SAR; break;
         default:
            shOp = Xsh_INVALID; break;
      }
      if (shOp != Xsh_INVALID) {
         HReg dst = newVRegI(env);

         /* regL = the value to be shifted */
         HReg regL   = iselIntExpr_R(env, e->Iex.Binop.arg1);
         addInstr(env, mk_iMOVsd_RR(regL,dst));

         /* Do any necessary widening for 16/8 bit operands */
         switch (e->Iex.Binop.op) {
            case Iop_Shr8:
               addInstr(env, X86Instr_Alu32R(
                                Xalu_AND, X86RMI_Imm(0xFF), dst));
               break;
            case Iop_Shr16:
               addInstr(env, X86Instr_Alu32R(
                                Xalu_AND, X86RMI_Imm(0xFFFF), dst));
               break;
            case Iop_Sar8:
               addInstr(env, X86Instr_Sh32(Xsh_SHL, 24, dst));
               addInstr(env, X86Instr_Sh32(Xsh_SAR, 24, dst));
               break;
            case Iop_Sar16:
               addInstr(env, X86Instr_Sh32(Xsh_SHL, 16, dst));
               addInstr(env, X86Instr_Sh32(Xsh_SAR, 16, dst));
               break;
            default: break;
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
               addInstr(env, X86Instr_Sh32( shOp, nshift, dst ));
         } else {
            /* General case; we have to force the amount into %cl. */
            HReg regR = iselIntExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, mk_iMOVsd_RR(regR,hregX86_ECX()));
            addInstr(env, X86Instr_Sh32(shOp, 0/* %cl */, dst));
         }
         return dst;
      }

      /* Handle misc other ops. */

      if (e->Iex.Binop.op == Iop_Max32U) {
         HReg src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg dst  = newVRegI(env);
         HReg src2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(src1,dst));
         addInstr(env, X86Instr_Alu32R(Xalu_CMP, X86RMI_Reg(src2), dst));
         addInstr(env, X86Instr_CMov32(Xcc_B, X86RM_Reg(src2), dst));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_8HLto16) {
         HReg hi8  = newVRegI(env);
         HReg lo8  = newVRegI(env);
         HReg hi8s = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg lo8s = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(hi8s, hi8));
         addInstr(env, mk_iMOVsd_RR(lo8s, lo8));
         addInstr(env, X86Instr_Sh32(Xsh_SHL, 8, hi8));
         addInstr(env, X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(0xFF), lo8));
         addInstr(env, X86Instr_Alu32R(Xalu_OR, X86RMI_Reg(lo8), hi8));
         return hi8;
      }

      if (e->Iex.Binop.op == Iop_16HLto32) {
         HReg hi16  = newVRegI(env);
         HReg lo16  = newVRegI(env);
         HReg hi16s = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg lo16s = iselIntExpr_R(env, e->Iex.Binop.arg2);
         addInstr(env, mk_iMOVsd_RR(hi16s, hi16));
         addInstr(env, mk_iMOVsd_RR(lo16s, lo16));
         addInstr(env, X86Instr_Sh32(Xsh_SHL, 16, hi16));
         addInstr(env, X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(0xFFFF), lo16));
         addInstr(env, X86Instr_Alu32R(Xalu_OR, X86RMI_Reg(lo16), hi16));
         return hi16;
      }

      if (e->Iex.Binop.op == Iop_MullS16 || e->Iex.Binop.op == Iop_MullS8
          || e->Iex.Binop.op == Iop_MullU16 || e->Iex.Binop.op == Iop_MullU8) {
         HReg a16   = newVRegI(env);
         HReg b16   = newVRegI(env);
         HReg a16s  = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg b16s  = iselIntExpr_R(env, e->Iex.Binop.arg2);
         Int  shift = (e->Iex.Binop.op == Iop_MullS8 
                       || e->Iex.Binop.op == Iop_MullU8)
                         ? 24 : 16;
         X86ShiftOp shr_op = (e->Iex.Binop.op == Iop_MullS8 
                              || e->Iex.Binop.op == Iop_MullS16)
                                ? Xsh_SAR : Xsh_SHR;

         addInstr(env, mk_iMOVsd_RR(a16s, a16));
         addInstr(env, mk_iMOVsd_RR(b16s, b16));
         addInstr(env, X86Instr_Sh32(Xsh_SHL, shift, a16));
         addInstr(env, X86Instr_Sh32(Xsh_SHL, shift, b16));
         addInstr(env, X86Instr_Sh32(shr_op,  shift, a16));
         addInstr(env, X86Instr_Sh32(shr_op,  shift, b16));
         addInstr(env, X86Instr_Alu32R(Xalu_MUL, X86RMI_Reg(a16), b16));
         return b16;
      }

      if (e->Iex.Binop.op == Iop_CmpF64) {
         HReg fL = iselDblExpr(env, e->Iex.Binop.arg1);
         HReg fR = iselDblExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegI(env);
         addInstr(env, X86Instr_FpCmp(fL,fR,dst));
         /* shift this right 8 bits so as to conform to CmpF64
            definition. */
         addInstr(env, X86Instr_Sh32(Xsh_SHR, 8, dst));
         return dst;
      }

      if (e->Iex.Binop.op == Iop_F64toI32S
          || e->Iex.Binop.op == Iop_F64toI16S) {
         Int  sz  = e->Iex.Binop.op == Iop_F64toI16S ? 2 : 4;
         HReg rf  = iselDblExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegI(env);

         /* Used several times ... */
         X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());

	 /* rf now holds the value to be converted, and rrm holds the
	    rounding mode value, encoded as per the IRRoundingMode
	    enum.  The first thing to do is set the FPU's rounding
	    mode accordingly. */

         /* Create a space for the format conversion. */
         /* subl $4, %esp */
         sub_from_esp(env, 4);

	 /* Set host rounding mode */
	 set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

         /* gistw/l %rf, 0(%esp) */
         addInstr(env, X86Instr_FpLdStI(False/*store*/, 
                                        toUChar(sz), rf, zero_esp));

         if (sz == 2) {
            /* movzwl 0(%esp), %dst */
            addInstr(env, X86Instr_LoadEX(2,False,zero_esp,dst));
         } else {
            /* movl 0(%esp), %dst */
            vassert(sz == 4);
            addInstr(env, X86Instr_Alu32R(
                             Xalu_MOV, X86RMI_Mem(zero_esp), dst));
         }

	 /* Restore default FPU rounding. */
         set_FPU_rounding_default( env );

         /* addl $4, %esp */
	 add_to_esp(env, 4);
         return dst;
      }

      break;
   }

   /* --------- UNARY OP --------- */
   case Iex_Unop: {

      /* 1Uto8(32to1(expr32)) */
      if (e->Iex.Unop.op == Iop_1Uto8) { 
         DECLARE_PATTERN(p_32to1_then_1Uto8);
         DEFINE_PATTERN(p_32to1_then_1Uto8,
                        unop(Iop_1Uto8,unop(Iop_32to1,bind(0))));
         if (matchIRExpr(&mi,p_32to1_then_1Uto8,e)) {
            const IRExpr* expr32 = mi.bindee[0];
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, expr32);
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Alu32R(Xalu_AND,
                                          X86RMI_Imm(1), dst));
            return dst;
         }
      }

      /* 8Uto32(LDle(expr32)) */
      if (e->Iex.Unop.op == Iop_8Uto32) {
         DECLARE_PATTERN(p_LDle8_then_8Uto32);
         DEFINE_PATTERN(p_LDle8_then_8Uto32,
                        unop(Iop_8Uto32,
                             IRExpr_Load(Iend_LE,Ity_I8,bind(0))) );
         if (matchIRExpr(&mi,p_LDle8_then_8Uto32,e)) {
            HReg dst = newVRegI(env);
            X86AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
            addInstr(env, X86Instr_LoadEX(1,False,amode,dst));
            return dst;
         }
      }

      /* 8Sto32(LDle(expr32)) */
      if (e->Iex.Unop.op == Iop_8Sto32) {
         DECLARE_PATTERN(p_LDle8_then_8Sto32);
         DEFINE_PATTERN(p_LDle8_then_8Sto32,
                        unop(Iop_8Sto32,
                             IRExpr_Load(Iend_LE,Ity_I8,bind(0))) );
         if (matchIRExpr(&mi,p_LDle8_then_8Sto32,e)) {
            HReg dst = newVRegI(env);
            X86AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
            addInstr(env, X86Instr_LoadEX(1,True,amode,dst));
            return dst;
         }
      }

      /* 16Uto32(LDle(expr32)) */
      if (e->Iex.Unop.op == Iop_16Uto32) {
         DECLARE_PATTERN(p_LDle16_then_16Uto32);
         DEFINE_PATTERN(p_LDle16_then_16Uto32,
                        unop(Iop_16Uto32,
                             IRExpr_Load(Iend_LE,Ity_I16,bind(0))) );
         if (matchIRExpr(&mi,p_LDle16_then_16Uto32,e)) {
            HReg dst = newVRegI(env);
            X86AMode* amode = iselIntExpr_AMode ( env, mi.bindee[0] );
            addInstr(env, X86Instr_LoadEX(2,False,amode,dst));
            return dst;
         }
      }

      /* 8Uto32(GET:I8) */
      if (e->Iex.Unop.op == Iop_8Uto32) {
         if (e->Iex.Unop.arg->tag == Iex_Get) {
            HReg      dst;
            X86AMode* amode;
            vassert(e->Iex.Unop.arg->Iex.Get.ty == Ity_I8);
            dst = newVRegI(env);
            amode = X86AMode_IR(e->Iex.Unop.arg->Iex.Get.offset,
                                hregX86_EBP());
            addInstr(env, X86Instr_LoadEX(1,False,amode,dst));
            return dst;
         }
      }

      /* 16to32(GET:I16) */
      if (e->Iex.Unop.op == Iop_16Uto32) {
         if (e->Iex.Unop.arg->tag == Iex_Get) {
            HReg      dst;
            X86AMode* amode;
            vassert(e->Iex.Unop.arg->Iex.Get.ty == Ity_I16);
            dst = newVRegI(env);
            amode = X86AMode_IR(e->Iex.Unop.arg->Iex.Get.offset,
                                hregX86_EBP());
            addInstr(env, X86Instr_LoadEX(2,False,amode,dst));
            return dst;
         }
      }

      switch (e->Iex.Unop.op) {
         case Iop_8Uto16:
         case Iop_8Uto32:
         case Iop_16Uto32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            UInt mask = e->Iex.Unop.op==Iop_16Uto32 ? 0xFFFF : 0xFF;
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Alu32R(Xalu_AND,
                                          X86RMI_Imm(mask), dst));
            return dst;
         }
         case Iop_8Sto16:
         case Iop_8Sto32:
         case Iop_16Sto32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            UInt amt = e->Iex.Unop.op==Iop_16Sto32 ? 16 : 24;
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Sh32(Xsh_SHL, amt, dst));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, amt, dst));
            return dst;
         }
	 case Iop_Not8:
	 case Iop_Not16:
         case Iop_Not32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Unary32(Xun_NOT,dst));
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
         case Iop_16HIto8:
         case Iop_32HIto16: {
            HReg dst  = newVRegI(env);
            HReg src  = iselIntExpr_R(env, e->Iex.Unop.arg);
            Int shift = e->Iex.Unop.op == Iop_16HIto8 ? 8 : 16;
            addInstr(env, mk_iMOVsd_RR(src,dst) );
            addInstr(env, X86Instr_Sh32(Xsh_SHR, shift, dst));
            return dst;
         }
         case Iop_1Uto32:
         case Iop_1Uto8: {
            HReg dst         = newVRegI(env);
            X86CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Set32(cond,dst));
            return dst;
         }
         case Iop_1Sto8:
         case Iop_1Sto16:
         case Iop_1Sto32: {
            /* could do better than this, but for now ... */
            HReg dst         = newVRegI(env);
            X86CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Set32(cond,dst));
            addInstr(env, X86Instr_Sh32(Xsh_SHL, 31, dst));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, dst));
            return dst;
         }
         case Iop_Ctz32: {
            /* Count trailing zeroes, implemented by x86 'bsfl' */
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Bsfr32(True,src,dst));
            return dst;
         }
         case Iop_Clz32: {
            /* Count leading zeroes.  Do 'bsrl' to establish the index
               of the highest set bit, and subtract that value from
               31. */
            HReg tmp = newVRegI(env);
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Bsfr32(False,src,tmp));
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, 
                                          X86RMI_Imm(31), dst));
            addInstr(env, X86Instr_Alu32R(Xalu_SUB,
                                          X86RMI_Reg(tmp), dst));
            return dst;
         }

         case Iop_CmpwNEZ32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src,dst));
            addInstr(env, X86Instr_Unary32(Xun_NEG,dst));
            addInstr(env, X86Instr_Alu32R(Xalu_OR,
                                          X86RMI_Reg(src), dst));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, dst));
            return dst;
         }
         case Iop_Left8:
         case Iop_Left16:
         case Iop_Left32: {
            HReg dst = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src, dst));
            addInstr(env, X86Instr_Unary32(Xun_NEG, dst));
            addInstr(env, X86Instr_Alu32R(Xalu_OR, X86RMI_Reg(src), dst));
            return dst;
         }

         case Iop_V128to32: {
            HReg      dst  = newVRegI(env);
            HReg      vec  = iselVecExpr(env, e->Iex.Unop.arg);
            X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
            sub_from_esp(env, 16);
            addInstr(env, X86Instr_SseLdSt(False/*store*/, vec, esp0));
            addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(esp0), dst ));
            add_to_esp(env, 16);
            return dst;
         }

         /* ReinterpF32asI32(e) */
         /* Given an IEEE754 single, produce an I32 with the same bit
            pattern.  Keep stack 8-aligned even though only using 4
            bytes. */
         case Iop_ReinterpF32asI32: {
            HReg rf   = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst  = newVRegI(env);
            X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());
            /* paranoia */
            set_FPU_rounding_default(env);
            /* subl $8, %esp */
            sub_from_esp(env, 8);
            /* gstF %rf, 0(%esp) */
            addInstr(env,
                     X86Instr_FpLdSt(False/*store*/, 4, rf, zero_esp));
            /* movl 0(%esp), %dst */
            addInstr(env, 
                     X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(zero_esp), dst));
            /* addl $8, %esp */
            add_to_esp(env, 8);
            return dst;
         }

         case Iop_16to8:
         case Iop_32to8:
         case Iop_32to16:
            /* These are no-ops. */
            return iselIntExpr_R(env, e->Iex.Unop.arg);

         case Iop_GetMSBs8x8: {
            /* Note: the following assumes the helper is of
               signature
                  UInt fn ( ULong ), and is not a regparm fn.
            */
            HReg  xLo, xHi;
            HReg  dst = newVRegI(env);
            Addr fn = (Addr)h_generic_calc_GetMSBs8x8;
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Push(X86RMI_Reg(xHi)));
            addInstr(env, X86Instr_Push(X86RMI_Reg(xLo)));
            addInstr(env, X86Instr_Call( Xcc_ALWAYS, (Addr32)fn,
                                         0, mk_RetLoc_simple(RLPri_Int) ));
            add_to_esp(env, 2*4);
            addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), dst));
            return dst;
         }

         default: 
            break;
      }
      break;
   }

   /* --------- GET --------- */
   case Iex_Get: {
      if (ty == Ity_I32) {
         HReg dst = newVRegI(env);
         addInstr(env, X86Instr_Alu32R(
                          Xalu_MOV, 
                          X86RMI_Mem(X86AMode_IR(e->Iex.Get.offset,
                                                 hregX86_EBP())),
                          dst));
         return dst;
      }
      if (ty == Ity_I8 || ty == Ity_I16) {
         HReg dst = newVRegI(env);
         addInstr(env, X86Instr_LoadEX(
                          toUChar(ty==Ity_I8 ? 1 : 2),
                          False,
                          X86AMode_IR(e->Iex.Get.offset,hregX86_EBP()),
                          dst));
         return dst;
      }
      break;
   }

   case Iex_GetI: {
      X86AMode* am 
         = genGuestArrayOffset(
              env, e->Iex.GetI.descr, 
                   e->Iex.GetI.ix, e->Iex.GetI.bias );
      HReg dst = newVRegI(env);
      if (ty == Ity_I8) {
         addInstr(env, X86Instr_LoadEX( 1, False, am, dst ));
         return dst;
      }
      if (ty == Ity_I32) {
         addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(am), dst));
         return dst;
      }
      break;
   }

   /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg    dst = newVRegI(env);
      vassert(ty == e->Iex.CCall.retty);

      /* be very restrictive for now.  Only 32/64-bit ints allowed for
         args, and 32 bits for return type.  Don't forget to change
         the RetLoc if more return types are allowed in future. */
      if (e->Iex.CCall.retty != Ity_I32)
         goto irreducible;

      /* Marshal args, do the call, clear stack. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      doHelperCall( &addToSp, &rloc, env, NULL/*guard*/,
                    e->Iex.CCall.cee, e->Iex.CCall.retty, e->Iex.CCall.args );
      vassert(is_sane_RetLoc(rloc));
      vassert(rloc.pri == RLPri_Int);
      vassert(addToSp == 0);

      addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), dst));
      return dst;
   }

   /* --------- LITERAL --------- */
   /* 32/16/8-bit literals */
   case Iex_Const: {
      X86RMI* rmi = iselIntExpr_RMI ( env, e );
      HReg    r   = newVRegI(env);
      addInstr(env, X86Instr_Alu32R(Xalu_MOV, rmi, r));
      return r;
   }

   /* --------- MULTIPLEX --------- */
   case Iex_ITE: { // VFD
     if ((ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8)
         && typeOfIRExpr(env->type_env,e->Iex.ITE.cond) == Ity_I1) {
        HReg   r1  = iselIntExpr_R(env, e->Iex.ITE.iftrue);
        X86RM* r0  = iselIntExpr_RM(env, e->Iex.ITE.iffalse);
        HReg   dst = newVRegI(env);
        addInstr(env, mk_iMOVsd_RR(r1,dst));
        X86CondCode cc = iselCondCode(env, e->Iex.ITE.cond);
        addInstr(env, X86Instr_CMov32(cc ^ 1, r0, dst));
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
/*--- ISEL: Integer expression auxiliaries              ---*/
/*---------------------------------------------------------*/

/* --------------------- AMODEs --------------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a 32-bit one.
*/

static Bool sane_AMode ( X86AMode* am )
{
   switch (am->tag) {
      case Xam_IR:
         return 
            toBool( hregClass(am->Xam.IR.reg) == HRcInt32
                    && (hregIsVirtual(am->Xam.IR.reg)
                        || sameHReg(am->Xam.IR.reg, hregX86_EBP())) );
      case Xam_IRRS:
         return 
            toBool( hregClass(am->Xam.IRRS.base) == HRcInt32
                    && hregIsVirtual(am->Xam.IRRS.base)
                    && hregClass(am->Xam.IRRS.index) == HRcInt32
                    && hregIsVirtual(am->Xam.IRRS.index) );
      default:
        vpanic("sane_AMode: unknown x86 amode tag");
   }
}

static X86AMode* iselIntExpr_AMode ( ISelEnv* env, const IRExpr* e )
{
   X86AMode* am = iselIntExpr_AMode_wrk(env, e);
   vassert(sane_AMode(am));
   return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static X86AMode* iselIntExpr_AMode_wrk ( ISelEnv* env, const IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32);

   /* Add32( Add32(expr1, Shl32(expr2, simm)), imm32 ) */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32
       && e->Iex.Binop.arg1->tag == Iex_Binop
       && e->Iex.Binop.arg1->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg1->Iex.Binop.arg2->tag == Iex_Binop
       && e->Iex.Binop.arg1->Iex.Binop.arg2->Iex.Binop.op == Iop_Shl32
       && e->Iex.Binop.arg1
           ->Iex.Binop.arg2->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg1
           ->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8) {
      UInt shift = e->Iex.Binop.arg1
                    ->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
      UInt imm32 = e->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
      if (shift == 1 || shift == 2 || shift == 3) {
         HReg r1 = iselIntExpr_R(env, e->Iex.Binop.arg1->Iex.Binop.arg1);
         HReg r2 = iselIntExpr_R(env, e->Iex.Binop.arg1
                                       ->Iex.Binop.arg2->Iex.Binop.arg1 );
         return X86AMode_IRRS(imm32, r1, r2, shift);
      }
   }

   /* Add32(expr1, Shl32(expr2, imm)) */
   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg2->tag == Iex_Binop
       && e->Iex.Binop.arg2->Iex.Binop.op == Iop_Shl32
       && e->Iex.Binop.arg2->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8) {
      UInt shift = e->Iex.Binop.arg2->Iex.Binop.arg2->Iex.Const.con->Ico.U8;
      if (shift == 1 || shift == 2 || shift == 3) {
         HReg r1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
         HReg r2 = iselIntExpr_R(env, e->Iex.Binop.arg2->Iex.Binop.arg1 );
         return X86AMode_IRRS(0, r1, r2, shift);
      }
   }

   /* Add32(expr,i) */
   if (e->tag == Iex_Binop 
       && e->Iex.Binop.op == Iop_Add32
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32) {
      HReg r1 = iselIntExpr_R(env,  e->Iex.Binop.arg1);
      return X86AMode_IR(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32, r1);
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   {
      HReg r1 = iselIntExpr_R(env, e);
      return X86AMode_IR(0, r1);
   }
}


/* --------------------- RMIs --------------------- */

/* Similarly, calculate an expression into an X86RMI operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits.  */

static X86RMI* iselIntExpr_RMI ( ISelEnv* env, const IRExpr* e )
{
   X86RMI* rmi = iselIntExpr_RMI_wrk(env, e);
   /* sanity checks ... */
   switch (rmi->tag) {
      case Xrmi_Imm:
         return rmi;
      case Xrmi_Reg:
         vassert(hregClass(rmi->Xrmi.Reg.reg) == HRcInt32);
         vassert(hregIsVirtual(rmi->Xrmi.Reg.reg));
         return rmi;
      case Xrmi_Mem:
         vassert(sane_AMode(rmi->Xrmi.Mem.am));
         return rmi;
      default:
         vpanic("iselIntExpr_RMI: unknown x86 RMI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static X86RMI* iselIntExpr_RMI_wrk ( ISelEnv* env, const IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      UInt u;
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & (e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:  u = 0xFF   & (e->Iex.Const.con->Ico.U8); break;
         default: vpanic("iselIntExpr_RMI.Iex_Const(x86h)");
      }
      return X86RMI_Imm(u);
   }

   /* special case: 32-bit GET */
   if (e->tag == Iex_Get && ty == Ity_I32) {
      return X86RMI_Mem(X86AMode_IR(e->Iex.Get.offset,
                                    hregX86_EBP()));
   }

   /* special case: 32-bit load from memory */
   if (e->tag == Iex_Load && ty == Ity_I32 
       && e->Iex.Load.end == Iend_LE) {
      X86AMode* am = iselIntExpr_AMode(env, e->Iex.Load.addr);
      return X86RMI_Mem(am);
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return X86RMI_Reg(r);
   }
}


/* --------------------- RIs --------------------- */

/* Calculate an expression into an X86RI operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits. */

static X86RI* iselIntExpr_RI ( ISelEnv* env, const IRExpr* e )
{
   X86RI* ri = iselIntExpr_RI_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Xri_Imm:
         return ri;
      case Xri_Reg:
         vassert(hregClass(ri->Xri.Reg.reg) == HRcInt32);
         vassert(hregIsVirtual(ri->Xri.Reg.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RI: unknown x86 RI tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static X86RI* iselIntExpr_RI_wrk ( ISelEnv* env, const IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      UInt u;
      switch (e->Iex.Const.con->tag) {
         case Ico_U32: u = e->Iex.Const.con->Ico.U32; break;
         case Ico_U16: u = 0xFFFF & (e->Iex.Const.con->Ico.U16); break;
         case Ico_U8:  u = 0xFF   & (e->Iex.Const.con->Ico.U8); break;
         default: vpanic("iselIntExpr_RMI.Iex_Const(x86h)");
      }
      return X86RI_Imm(u);
   }

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return X86RI_Reg(r);
   }
}


/* --------------------- RMs --------------------- */

/* Similarly, calculate an expression into an X86RM operand.  As with
   iselIntExpr_R, the expression can have type 32, 16 or 8 bits.  */

static X86RM* iselIntExpr_RM ( ISelEnv* env, const IRExpr* e )
{
   X86RM* rm = iselIntExpr_RM_wrk(env, e);
   /* sanity checks ... */
   switch (rm->tag) {
      case Xrm_Reg:
         vassert(hregClass(rm->Xrm.Reg.reg) == HRcInt32);
         vassert(hregIsVirtual(rm->Xrm.Reg.reg));
         return rm;
      case Xrm_Mem:
         vassert(sane_AMode(rm->Xrm.Mem.am));
         return rm;
      default:
         vpanic("iselIntExpr_RM: unknown x86 RM tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static X86RM* iselIntExpr_RM_wrk ( ISelEnv* env, const IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8);

   /* special case: 32-bit GET */
   if (e->tag == Iex_Get && ty == Ity_I32) {
      return X86RM_Mem(X86AMode_IR(e->Iex.Get.offset,
                                   hregX86_EBP()));
   }

   /* special case: load from memory */

   /* default case: calculate into a register and return that */
   {
      HReg r = iselIntExpr_R ( env, e );
      return X86RM_Reg(r);
   }
}


/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static X86CondCode iselCondCode ( ISelEnv* env, const IRExpr* e )
{
   /* Uh, there's nothing we can sanity check here, unfortunately. */
   return iselCondCode_wrk(env,e);
}

/* DO NOT CALL THIS DIRECTLY ! */
static X86CondCode iselCondCode_wrk ( ISelEnv* env, const IRExpr* e )
{
   MatchInfo mi;

   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I1);

   /* var */
   if (e->tag == Iex_RdTmp) {
      HReg r32 = lookupIRTemp(env, e->Iex.RdTmp.tmp);
      /* Test32 doesn't modify r32; so this is OK. */
      addInstr(env, X86Instr_Test32(1,X86RM_Reg(r32)));
      return Xcc_NZ;
   }

   /* Constant 1:Bit */
   if (e->tag == Iex_Const) {
      HReg r;
      vassert(e->Iex.Const.con->tag == Ico_U1);
      vassert(e->Iex.Const.con->Ico.U1 == True 
              || e->Iex.Const.con->Ico.U1 == False);
      r = newVRegI(env);
      addInstr(env, X86Instr_Alu32R(Xalu_MOV,X86RMI_Imm(0),r));
      addInstr(env, X86Instr_Alu32R(Xalu_XOR,X86RMI_Reg(r),r));
      return e->Iex.Const.con->Ico.U1 ? Xcc_Z : Xcc_NZ;
   }

   /* Not1(e) */
   if (e->tag == Iex_Unop && e->Iex.Unop.op == Iop_Not1) {
      /* Generate code for the arg, and negate the test condition */
      return 1 ^ iselCondCode(env, e->Iex.Unop.arg);
   }

   /* --- patterns rooted at: 32to1 --- */

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_32to1) {
      X86RM* rm = iselIntExpr_RM(env, e->Iex.Unop.arg);
      addInstr(env, X86Instr_Test32(1,rm));
      return Xcc_NZ;
   }

   /* --- patterns rooted at: CmpNEZ8 --- */

   /* CmpNEZ8(x) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ8) {
      X86RM* rm = iselIntExpr_RM(env, e->Iex.Unop.arg);
      addInstr(env, X86Instr_Test32(0xFF,rm));
      return Xcc_NZ;
   }

   /* --- patterns rooted at: CmpNEZ16 --- */

   /* CmpNEZ16(x) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ16) {
      X86RM* rm = iselIntExpr_RM(env, e->Iex.Unop.arg);
      addInstr(env, X86Instr_Test32(0xFFFF,rm));
      return Xcc_NZ;
   }

   /* --- patterns rooted at: CmpNEZ32 --- */

   /* CmpNEZ32(And32(x,y)) */
   {
      DECLARE_PATTERN(p_CmpNEZ32_And32);
      DEFINE_PATTERN(p_CmpNEZ32_And32,
                     unop(Iop_CmpNEZ32, binop(Iop_And32, bind(0), bind(1))));
      if (matchIRExpr(&mi, p_CmpNEZ32_And32, e)) {
         HReg    r0   = iselIntExpr_R(env, mi.bindee[0]);
         X86RMI* rmi1 = iselIntExpr_RMI(env, mi.bindee[1]);
         HReg    tmp  = newVRegI(env);
         addInstr(env, mk_iMOVsd_RR(r0, tmp));
         addInstr(env, X86Instr_Alu32R(Xalu_AND,rmi1,tmp));
         return Xcc_NZ;
      }
   }

   /* CmpNEZ32(Or32(x,y)) */
   {
      DECLARE_PATTERN(p_CmpNEZ32_Or32);
      DEFINE_PATTERN(p_CmpNEZ32_Or32,
                     unop(Iop_CmpNEZ32, binop(Iop_Or32, bind(0), bind(1))));
      if (matchIRExpr(&mi, p_CmpNEZ32_Or32, e)) {
         HReg    r0   = iselIntExpr_R(env, mi.bindee[0]);
         X86RMI* rmi1 = iselIntExpr_RMI(env, mi.bindee[1]);
         HReg    tmp  = newVRegI(env);
         addInstr(env, mk_iMOVsd_RR(r0, tmp));
         addInstr(env, X86Instr_Alu32R(Xalu_OR,rmi1,tmp));
         return Xcc_NZ;
      }
   }

   /* CmpNEZ32(GET(..):I32) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ32
       && e->Iex.Unop.arg->tag == Iex_Get) {
      X86AMode* am = X86AMode_IR(e->Iex.Unop.arg->Iex.Get.offset, 
                                 hregX86_EBP());
      addInstr(env, X86Instr_Alu32M(Xalu_CMP, X86RI_Imm(0), am));
      return Xcc_NZ;
   }

   /* CmpNEZ32(x) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ32) {
      HReg    r1   = iselIntExpr_R(env, e->Iex.Unop.arg);
      X86RMI* rmi2 = X86RMI_Imm(0);
      addInstr(env, X86Instr_Alu32R(Xalu_CMP,rmi2,r1));
      return Xcc_NZ;
   }

   /* --- patterns rooted at: CmpNEZ64 --- */

   /* CmpNEZ64(Or64(x,y)) */
   {
      DECLARE_PATTERN(p_CmpNEZ64_Or64);
      DEFINE_PATTERN(p_CmpNEZ64_Or64,
                     unop(Iop_CmpNEZ64, binop(Iop_Or64, bind(0), bind(1))));
      if (matchIRExpr(&mi, p_CmpNEZ64_Or64, e)) {
         HReg    hi1, lo1, hi2, lo2;
         HReg    tmp  = newVRegI(env);
         iselInt64Expr( &hi1, &lo1, env, mi.bindee[0] );
         addInstr(env, mk_iMOVsd_RR(hi1, tmp));
         addInstr(env, X86Instr_Alu32R(Xalu_OR,X86RMI_Reg(lo1),tmp));
         iselInt64Expr( &hi2, &lo2, env, mi.bindee[1] );
         addInstr(env, X86Instr_Alu32R(Xalu_OR,X86RMI_Reg(hi2),tmp));
         addInstr(env, X86Instr_Alu32R(Xalu_OR,X86RMI_Reg(lo2),tmp));
         return Xcc_NZ;
      }
   }

   /* CmpNEZ64(x) */
   if (e->tag == Iex_Unop 
       && e->Iex.Unop.op == Iop_CmpNEZ64) {
      HReg hi, lo;
      HReg tmp = newVRegI(env);
      iselInt64Expr( &hi, &lo, env, e->Iex.Unop.arg );
      addInstr(env, mk_iMOVsd_RR(hi, tmp));
      addInstr(env, X86Instr_Alu32R(Xalu_OR,X86RMI_Reg(lo), tmp));
      return Xcc_NZ;
   }

   /* --- patterns rooted at: Cmp{EQ,NE}{8,16} --- */

   /* CmpEQ8 / CmpNE8 */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ8
           || e->Iex.Binop.op == Iop_CmpNE8
           || e->Iex.Binop.op == Iop_CasCmpEQ8
           || e->Iex.Binop.op == Iop_CasCmpNE8)) {
      if (isZeroU8(e->Iex.Binop.arg2)) {
         HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
         addInstr(env, X86Instr_Test32(0xFF,X86RM_Reg(r1)));
         switch (e->Iex.Binop.op) {
            case Iop_CmpEQ8: case Iop_CasCmpEQ8: return Xcc_Z;
            case Iop_CmpNE8: case Iop_CasCmpNE8: return Xcc_NZ;
            default: vpanic("iselCondCode(x86): CmpXX8(expr,0:I8)");
         }
      } else {
         HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
         X86RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         HReg    r    = newVRegI(env);
         addInstr(env, mk_iMOVsd_RR(r1,r));
         addInstr(env, X86Instr_Alu32R(Xalu_XOR,rmi2,r));
         addInstr(env, X86Instr_Test32(0xFF,X86RM_Reg(r)));
         switch (e->Iex.Binop.op) {
            case Iop_CmpEQ8: case Iop_CasCmpEQ8: return Xcc_Z;
            case Iop_CmpNE8: case Iop_CasCmpNE8: return Xcc_NZ;
            default: vpanic("iselCondCode(x86): CmpXX8(expr,expr)");
         }
      }
   }

   /* CmpEQ16 / CmpNE16 */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpEQ16
           || e->Iex.Binop.op == Iop_CmpNE16
           || e->Iex.Binop.op == Iop_CasCmpEQ16
           || e->Iex.Binop.op == Iop_CasCmpNE16
           || e->Iex.Binop.op == Iop_ExpCmpNE16)) {
      HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      X86RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      HReg    r    = newVRegI(env);
      addInstr(env, mk_iMOVsd_RR(r1,r));
      addInstr(env, X86Instr_Alu32R(Xalu_XOR,rmi2,r));
      addInstr(env, X86Instr_Test32(0xFFFF,X86RM_Reg(r)));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ16: case Iop_CasCmpEQ16:
            return Xcc_Z;
         case Iop_CmpNE16: case Iop_CasCmpNE16: case Iop_ExpCmpNE16:
            return Xcc_NZ;
         default:
            vpanic("iselCondCode(x86): CmpXX16");
      }
   }

   /* CmpNE32(ccall, 32-bit constant) (--smc-check=all optimisation).
      Saves a "movl %eax, %tmp" compared to the default route. */
   if (e->tag == Iex_Binop 
       && e->Iex.Binop.op == Iop_CmpNE32
       && e->Iex.Binop.arg1->tag == Iex_CCall
       && e->Iex.Binop.arg2->tag == Iex_Const) {
      IRExpr* cal = e->Iex.Binop.arg1;
      IRExpr* con = e->Iex.Binop.arg2;
      /* clone & partial-eval of generic Iex_CCall and Iex_Const cases */
      vassert(cal->Iex.CCall.retty == Ity_I32); /* else ill-typed IR */
      vassert(con->Iex.Const.con->tag == Ico_U32);
      /* Marshal args, do the call. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      doHelperCall( &addToSp, &rloc, env, NULL/*guard*/,
                    cal->Iex.CCall.cee,
                    cal->Iex.CCall.retty, cal->Iex.CCall.args );
      vassert(is_sane_RetLoc(rloc));
      vassert(rloc.pri == RLPri_Int);
      vassert(addToSp == 0);
      /* */
      addInstr(env, X86Instr_Alu32R(Xalu_CMP,
                                    X86RMI_Imm(con->Iex.Const.con->Ico.U32),
                                    hregX86_EAX()));
      return Xcc_NZ;
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
           || e->Iex.Binop.op == Iop_CasCmpNE32
           || e->Iex.Binop.op == Iop_ExpCmpNE32)) {
      HReg    r1   = iselIntExpr_R(env, e->Iex.Binop.arg1);
      X86RMI* rmi2 = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
      addInstr(env, X86Instr_Alu32R(Xalu_CMP,rmi2,r1));
      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ32: case Iop_CasCmpEQ32: return Xcc_Z;
         case Iop_CmpNE32:
         case Iop_CasCmpNE32: case Iop_ExpCmpNE32: return Xcc_NZ;
         case Iop_CmpLT32S: return Xcc_L;
         case Iop_CmpLT32U: return Xcc_B;
         case Iop_CmpLE32S: return Xcc_LE;
         case Iop_CmpLE32U: return Xcc_BE;
         default: vpanic("iselCondCode(x86): CmpXX32");
      }
   }

   /* CmpNE64 */
   if (e->tag == Iex_Binop 
       && (e->Iex.Binop.op == Iop_CmpNE64
           || e->Iex.Binop.op == Iop_CmpEQ64)) {
      HReg hi1, hi2, lo1, lo2;
      HReg tHi = newVRegI(env);
      HReg tLo = newVRegI(env);
      iselInt64Expr( &hi1, &lo1, env, e->Iex.Binop.arg1 );
      iselInt64Expr( &hi2, &lo2, env, e->Iex.Binop.arg2 );
      addInstr(env, mk_iMOVsd_RR(hi1, tHi));
      addInstr(env, X86Instr_Alu32R(Xalu_XOR,X86RMI_Reg(hi2), tHi));
      addInstr(env, mk_iMOVsd_RR(lo1, tLo));
      addInstr(env, X86Instr_Alu32R(Xalu_XOR,X86RMI_Reg(lo2), tLo));
      addInstr(env, X86Instr_Alu32R(Xalu_OR,X86RMI_Reg(tHi), tLo));
      switch (e->Iex.Binop.op) {
         case Iop_CmpNE64: return Xcc_NZ;
         case Iop_CmpEQ64: return Xcc_Z;
         default: vpanic("iselCondCode(x86): CmpXX64");
      }
   }

   /* And1(x,y), Or1(x,y) */
   /* FIXME: We could (and probably should) do a lot better here.  If both args
      are in temps already then we can just emit a reg-reg And/Or directly,
      followed by the final Test. */
   if (e->tag == Iex_Binop
       && (e->Iex.Binop.op == Iop_And1 || e->Iex.Binop.op == Iop_Or1)) {
      // We could probably be cleverer about this.  In the meantime ..
      HReg x_as_32 = newVRegI(env);
      X86CondCode cc_x = iselCondCode(env, e->Iex.Binop.arg1);
      addInstr(env, X86Instr_Set32(cc_x, x_as_32));
      HReg y_as_32 = newVRegI(env);
      X86CondCode cc_y = iselCondCode(env, e->Iex.Binop.arg2);
      addInstr(env, X86Instr_Set32(cc_y, y_as_32));
      X86AluOp aop = e->Iex.Binop.op == Iop_And1 ? Xalu_AND : Xalu_OR;
      addInstr(env, X86Instr_Alu32R(aop, X86RMI_Reg(x_as_32), y_as_32));
      addInstr(env, X86Instr_Test32(1, X86RM_Reg(y_as_32)));
      return Xcc_NZ;
   }

   ppIRExpr(e);
   vpanic("iselCondCode");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64 bit)                ---*/
/*---------------------------------------------------------*/

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
   MatchInfo mi;
   HWord fn = 0; /* helper fn for most SIMD64 stuff */
   vassert(e);
   vassert(typeOfIRExpr(env->type_env,e) == Ity_I64);

   /* 64-bit literal */
   if (e->tag == Iex_Const) {
      ULong w64 = e->Iex.Const.con->Ico.U64;
      UInt  wHi = toUInt(w64 >> 32);
      UInt  wLo = toUInt(w64);
      HReg  tLo = newVRegI(env);
      HReg  tHi = newVRegI(env);
      vassert(e->Iex.Const.con->tag == Ico_U64);
      if (wLo == wHi) {
         /* Save a precious Int register in this special case. */
         addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(wLo), tLo));
         *rHi = tLo;
         *rLo = tLo;
      } else {
         addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(wHi), tHi));
         addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(wLo), tLo));
         *rHi = tHi;
         *rLo = tLo;
      }
      return;
   }

   /* read 64-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTemp64( rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }

   /* 64-bit load */
   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      HReg     tLo, tHi;
      X86AMode *am0, *am4;
      vassert(e->Iex.Load.ty == Ity_I64);
      tLo = newVRegI(env);
      tHi = newVRegI(env);
      am0 = iselIntExpr_AMode(env, e->Iex.Load.addr);
      am4 = advance4(am0);
      addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am0), tLo ));
      addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am4), tHi ));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit GET */
   if (e->tag == Iex_Get) {
      X86AMode* am  = X86AMode_IR(e->Iex.Get.offset, hregX86_EBP());
      X86AMode* am4 = advance4(am);
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am), tLo ));
      addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am4), tHi ));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit GETI */
   if (e->tag == Iex_GetI) {
      X86AMode* am 
         = genGuestArrayOffset( env, e->Iex.GetI.descr, 
                                     e->Iex.GetI.ix, e->Iex.GetI.bias );
      X86AMode* am4 = advance4(am);
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am), tLo ));
      addInstr(env, X86Instr_Alu32R( Xalu_MOV, X86RMI_Mem(am4), tHi ));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit ITE: ITE(g, expr, expr) */ // VFD
   if (e->tag == Iex_ITE) {
      HReg e0Lo, e0Hi, e1Lo, e1Hi;
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      iselInt64Expr(&e0Hi, &e0Lo, env, e->Iex.ITE.iffalse);
      iselInt64Expr(&e1Hi, &e1Lo, env, e->Iex.ITE.iftrue);
      addInstr(env, mk_iMOVsd_RR(e1Hi, tHi));
      addInstr(env, mk_iMOVsd_RR(e1Lo, tLo));
      X86CondCode cc = iselCondCode(env, e->Iex.ITE.cond);
      /* This assumes the first cmov32 doesn't trash the condition
         codes, so they are still available for the second cmov32 */
      addInstr(env, X86Instr_CMov32(cc ^ 1, X86RM_Reg(e0Hi), tHi));
      addInstr(env, X86Instr_CMov32(cc ^ 1, X86RM_Reg(e0Lo), tLo));
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
            /* get one operand into %eax, and the other into a R/M.
               Need to make an educated guess about which is better in
               which. */
            HReg   tLo    = newVRegI(env);
            HReg   tHi    = newVRegI(env);
            Bool   syned  = toBool(e->Iex.Binop.op == Iop_MullS32);
            X86RM* rmLeft = iselIntExpr_RM(env, e->Iex.Binop.arg1);
            HReg   rRight = iselIntExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, mk_iMOVsd_RR(rRight, hregX86_EAX()));
            addInstr(env, X86Instr_MulL(syned, rmLeft));
            /* Result is now in EDX:EAX.  Tell the caller. */
            addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
            addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 64 x 32 -> (32(rem),32(div)) division */
         case Iop_DivModU64to32:
         case Iop_DivModS64to32: {
            /* Get the 64-bit operand into edx:eax, and the other into
               any old R/M. */
            HReg sHi, sLo;
            HReg   tLo     = newVRegI(env);
            HReg   tHi     = newVRegI(env);
            Bool   syned   = toBool(e->Iex.Binop.op == Iop_DivModS64to32);
            X86RM* rmRight = iselIntExpr_RM(env, e->Iex.Binop.arg2);
            iselInt64Expr(&sHi,&sLo, env, e->Iex.Binop.arg1);
            addInstr(env, mk_iMOVsd_RR(sHi, hregX86_EDX()));
            addInstr(env, mk_iMOVsd_RR(sLo, hregX86_EAX()));
            addInstr(env, X86Instr_Div(syned, rmRight));
            addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
            addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
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
            X86AluOp op = e->Iex.Binop.op==Iop_Or64 ? Xalu_OR
                          : e->Iex.Binop.op==Iop_And64 ? Xalu_AND
                          : Xalu_XOR;
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, mk_iMOVsd_RR(xHi, tHi));
            addInstr(env, X86Instr_Alu32R(op, X86RMI_Reg(yHi), tHi));
            addInstr(env, mk_iMOVsd_RR(xLo, tLo));
            addInstr(env, X86Instr_Alu32R(op, X86RMI_Reg(yLo), tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* Add64/Sub64 */
         case Iop_Add64:
            if (e->Iex.Binop.arg2->tag == Iex_Const) {
               /* special case Add64(e, const) */
               ULong w64 = e->Iex.Binop.arg2->Iex.Const.con->Ico.U64;
               UInt  wHi = toUInt(w64 >> 32);
               UInt  wLo = toUInt(w64);
               HReg  tLo = newVRegI(env);
               HReg  tHi = newVRegI(env);
               HReg  xLo, xHi;
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U64);
               iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
               addInstr(env, mk_iMOVsd_RR(xHi, tHi));
               addInstr(env, mk_iMOVsd_RR(xLo, tLo));
               addInstr(env, X86Instr_Alu32R(Xalu_ADD, X86RMI_Imm(wLo), tLo));
               addInstr(env, X86Instr_Alu32R(Xalu_ADC, X86RMI_Imm(wHi), tHi));
               *rHi = tHi;
               *rLo = tLo;
               return;
            }
            /* else fall through to the generic case */
         case Iop_Sub64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            addInstr(env, mk_iMOVsd_RR(xHi, tHi));
            addInstr(env, mk_iMOVsd_RR(xLo, tLo));
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            if (e->Iex.Binop.op==Iop_Add64) {
               addInstr(env, X86Instr_Alu32R(Xalu_ADD, X86RMI_Reg(yLo), tLo));
               addInstr(env, X86Instr_Alu32R(Xalu_ADC, X86RMI_Reg(yHi), tHi));
            } else {
               addInstr(env, X86Instr_Alu32R(Xalu_SUB, X86RMI_Reg(yLo), tLo));
               addInstr(env, X86Instr_Alu32R(Xalu_SBB, X86RMI_Reg(yHi), tHi));
            }
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32HLto64(e1,e2) */
         case Iop_32HLto64:
            *rHi = iselIntExpr_R(env, e->Iex.Binop.arg1);
            *rLo = iselIntExpr_R(env, e->Iex.Binop.arg2);
            return;

         /* 64-bit shifts */
         case Iop_Shl64: {
            /* We use the same ingenious scheme as gcc.  Put the value
               to be shifted into %hi:%lo, and the shift amount into
               %cl.  Then (dsts on right, a la ATT syntax):
 
               shldl %cl, %lo, %hi   -- make %hi be right for the
                                     -- shift amt %cl % 32
               shll  %cl, %lo        -- make %lo be right for the
                                     -- shift amt %cl % 32

               Now, if (shift amount % 64) is in the range 32 .. 63,
               we have to do a fixup, which puts the result low half
               into the result high half, and zeroes the low half:

               testl $32, %ecx

               cmovnz %lo, %hi
               movl $0, %tmp         -- sigh; need yet another reg
               cmovnz %tmp, %lo 
            */
            HReg rAmt, sHi, sLo, tHi, tLo, tTemp;
            tLo = newVRegI(env);
            tHi = newVRegI(env);
            tTemp = newVRegI(env);
            rAmt = iselIntExpr_R(env, e->Iex.Binop.arg2);
            iselInt64Expr(&sHi,&sLo, env, e->Iex.Binop.arg1);
            addInstr(env, mk_iMOVsd_RR(rAmt, hregX86_ECX()));
            addInstr(env, mk_iMOVsd_RR(sHi, tHi));
            addInstr(env, mk_iMOVsd_RR(sLo, tLo));
            /* Ok.  Now shift amt is in %ecx, and value is in tHi/tLo
               and those regs are legitimately modifiable. */
            addInstr(env, X86Instr_Sh3232(Xsh_SHL, 0/*%cl*/, tLo, tHi));
            addInstr(env, X86Instr_Sh32(Xsh_SHL, 0/*%cl*/, tLo));
            addInstr(env, X86Instr_Test32(32, X86RM_Reg(hregX86_ECX())));
            addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tLo), tHi));
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tTemp));
            addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tTemp), tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Shr64: {
            /* We use the same ingenious scheme as gcc.  Put the value
               to be shifted into %hi:%lo, and the shift amount into
               %cl.  Then:
 
               shrdl %cl, %hi, %lo   -- make %lo be right for the
                                     -- shift amt %cl % 32
               shrl  %cl, %hi        -- make %hi be right for the
                                     -- shift amt %cl % 32

               Now, if (shift amount % 64) is in the range 32 .. 63,
               we have to do a fixup, which puts the result high half
               into the result low half, and zeroes the high half:

               testl $32, %ecx

               cmovnz %hi, %lo
               movl $0, %tmp         -- sigh; need yet another reg
               cmovnz %tmp, %hi
            */
            HReg rAmt, sHi, sLo, tHi, tLo, tTemp;
            tLo = newVRegI(env);
            tHi = newVRegI(env);
            tTemp = newVRegI(env);
            rAmt = iselIntExpr_R(env, e->Iex.Binop.arg2);
            iselInt64Expr(&sHi,&sLo, env, e->Iex.Binop.arg1);
            addInstr(env, mk_iMOVsd_RR(rAmt, hregX86_ECX()));
            addInstr(env, mk_iMOVsd_RR(sHi, tHi));
            addInstr(env, mk_iMOVsd_RR(sLo, tLo));
            /* Ok.  Now shift amt is in %ecx, and value is in tHi/tLo
               and those regs are legitimately modifiable. */
            addInstr(env, X86Instr_Sh3232(Xsh_SHR, 0/*%cl*/, tHi, tLo));
            addInstr(env, X86Instr_Sh32(Xsh_SHR, 0/*%cl*/, tHi));
            addInstr(env, X86Instr_Test32(32, X86RM_Reg(hregX86_ECX())));
            addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tHi), tLo));
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tTemp));
            addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(tTemp), tHi));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Sar64: {
            /* gcc -O2 does the following.  I don't know how it works, but it
               does work.  Don't mess with it.  This is hard to test because the
               x86 front end doesn't create Iop_Sar64 for any x86 instruction,
               so it's impossible to write a test program that feeds values
               through Iop_Sar64 and prints their results.  The implementation
               here was tested by using psrlq on mmx registers -- that generates
               Iop_Shr64 -- and temporarily hacking the front end to generate
               Iop_Sar64 for that instruction instead.

               movl  %amount, %ecx
               movl  %srcHi,  %r1
               movl  %srcLo,  %r2

               movl   %r1, %r3
               sarl   %cl, %r3
               movl   %r2, %r4
               shrdl  %cl, %r1, %r4
               movl   %r3, %r2
               sarl   $31, %r2
               andl   $32, %ecx
               cmovne %r3, %r4   // = resLo
               cmovne %r2, %r3   // = resHi
            */
            HReg amount = iselIntExpr_R(env, e->Iex.Binop.arg2);
            HReg srcHi = INVALID_HREG, srcLo = INVALID_HREG;
            iselInt64Expr(&srcHi, &srcLo, env, e->Iex.Binop.arg1);
            HReg r1 = newVRegI(env);
            HReg r2 = newVRegI(env);
            HReg r3 = newVRegI(env);
            HReg r4 = newVRegI(env);
            addInstr(env, mk_iMOVsd_RR(amount, hregX86_ECX()));
            addInstr(env, mk_iMOVsd_RR(srcHi, r1));
            addInstr(env, mk_iMOVsd_RR(srcLo, r2));

            addInstr(env, mk_iMOVsd_RR(r1, r3));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, 0/*%cl*/, r3));
            addInstr(env, mk_iMOVsd_RR(r2, r4));
            addInstr(env, X86Instr_Sh3232(Xsh_SHR, 0/*%cl*/, r1, r4));
            addInstr(env, mk_iMOVsd_RR(r3, r2));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, r2));
            addInstr(env, X86Instr_Alu32R(Xalu_AND, X86RMI_Imm(32),
                                                    hregX86_ECX()));
            addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(r3), r4));
            addInstr(env, X86Instr_CMov32(Xcc_NZ, X86RM_Reg(r2), r3));
            *rHi = r3;
            *rLo = r4;
            return;
         }

         /* F64 -> I64 */
         /* Sigh, this is an almost exact copy of the F64 -> I32/I16
            case.  Unfortunately I see no easy way to avoid the
            duplication. */
         case Iop_F64toI64S: {
            HReg rf  = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);

            /* Used several times ... */
            /* Careful ... this sharing is only safe because
	       zero_esp/four_esp do not hold any registers which the
	       register allocator could attempt to swizzle later. */
            X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());
            X86AMode* four_esp = X86AMode_IR(4, hregX86_ESP());

            /* rf now holds the value to be converted, and rrm holds
               the rounding mode value, encoded as per the
               IRRoundingMode enum.  The first thing to do is set the
               FPU's rounding mode accordingly. */

            /* Create a space for the format conversion. */
            /* subl $8, %esp */
            sub_from_esp(env, 8);

            /* Set host rounding mode */
            set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

            /* gistll %rf, 0(%esp) */
            addInstr(env, X86Instr_FpLdStI(False/*store*/, 8, rf, zero_esp));

            /* movl 0(%esp), %dstLo */
            /* movl 4(%esp), %dstHi */
            addInstr(env, X86Instr_Alu32R(
                             Xalu_MOV, X86RMI_Mem(zero_esp), tLo));
            addInstr(env, X86Instr_Alu32R(
                             Xalu_MOV, X86RMI_Mem(four_esp), tHi));

            /* Restore default FPU rounding. */
            set_FPU_rounding_default( env );

            /* addl $8, %esp */
            add_to_esp(env, 8);

            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_Add8x8:
            fn = (HWord)h_generic_calc_Add8x8; goto binnish;
         case Iop_Add16x4:
            fn = (HWord)h_generic_calc_Add16x4; goto binnish;
         case Iop_Add32x2:
            fn = (HWord)h_generic_calc_Add32x2; goto binnish;

         case Iop_Avg8Ux8:
            fn = (HWord)h_generic_calc_Avg8Ux8; goto binnish;
         case Iop_Avg16Ux4:
            fn = (HWord)h_generic_calc_Avg16Ux4; goto binnish;

         case Iop_CmpEQ8x8:
            fn = (HWord)h_generic_calc_CmpEQ8x8; goto binnish;
         case Iop_CmpEQ16x4:
            fn = (HWord)h_generic_calc_CmpEQ16x4; goto binnish;
         case Iop_CmpEQ32x2:
            fn = (HWord)h_generic_calc_CmpEQ32x2; goto binnish;

         case Iop_CmpGT8Sx8:
            fn = (HWord)h_generic_calc_CmpGT8Sx8; goto binnish;
         case Iop_CmpGT16Sx4:
            fn = (HWord)h_generic_calc_CmpGT16Sx4; goto binnish;
         case Iop_CmpGT32Sx2:
            fn = (HWord)h_generic_calc_CmpGT32Sx2; goto binnish;

         case Iop_InterleaveHI8x8:
            fn = (HWord)h_generic_calc_InterleaveHI8x8; goto binnish;
         case Iop_InterleaveLO8x8:
            fn = (HWord)h_generic_calc_InterleaveLO8x8; goto binnish;
         case Iop_InterleaveHI16x4:
            fn = (HWord)h_generic_calc_InterleaveHI16x4; goto binnish;
         case Iop_InterleaveLO16x4:
            fn = (HWord)h_generic_calc_InterleaveLO16x4; goto binnish;
         case Iop_InterleaveHI32x2:
            fn = (HWord)h_generic_calc_InterleaveHI32x2; goto binnish;
         case Iop_InterleaveLO32x2:
            fn = (HWord)h_generic_calc_InterleaveLO32x2; goto binnish;
         case Iop_CatOddLanes16x4:
            fn = (HWord)h_generic_calc_CatOddLanes16x4; goto binnish;
         case Iop_CatEvenLanes16x4:
            fn = (HWord)h_generic_calc_CatEvenLanes16x4; goto binnish;
         case Iop_Perm8x8:
            fn = (HWord)h_generic_calc_Perm8x8; goto binnish;

         case Iop_Max8Ux8:
            fn = (HWord)h_generic_calc_Max8Ux8; goto binnish;
         case Iop_Max16Sx4:
            fn = (HWord)h_generic_calc_Max16Sx4; goto binnish;
         case Iop_Min8Ux8:
            fn = (HWord)h_generic_calc_Min8Ux8; goto binnish;
         case Iop_Min16Sx4:
            fn = (HWord)h_generic_calc_Min16Sx4; goto binnish;

         case Iop_Mul16x4:
            fn = (HWord)h_generic_calc_Mul16x4; goto binnish;
         case Iop_Mul32x2:
            fn = (HWord)h_generic_calc_Mul32x2; goto binnish;
         case Iop_MulHi16Sx4:
            fn = (HWord)h_generic_calc_MulHi16Sx4; goto binnish;
         case Iop_MulHi16Ux4:
            fn = (HWord)h_generic_calc_MulHi16Ux4; goto binnish;

         case Iop_QAdd8Sx8:
            fn = (HWord)h_generic_calc_QAdd8Sx8; goto binnish;
         case Iop_QAdd16Sx4:
            fn = (HWord)h_generic_calc_QAdd16Sx4; goto binnish;
         case Iop_QAdd8Ux8:
            fn = (HWord)h_generic_calc_QAdd8Ux8; goto binnish;
         case Iop_QAdd16Ux4:
            fn = (HWord)h_generic_calc_QAdd16Ux4; goto binnish;

         case Iop_QNarrowBin32Sto16Sx4:
            fn = (HWord)h_generic_calc_QNarrowBin32Sto16Sx4; goto binnish;
         case Iop_QNarrowBin16Sto8Sx8:
            fn = (HWord)h_generic_calc_QNarrowBin16Sto8Sx8; goto binnish;
         case Iop_QNarrowBin16Sto8Ux8:
            fn = (HWord)h_generic_calc_QNarrowBin16Sto8Ux8; goto binnish;
         case Iop_NarrowBin16to8x8:
            fn = (HWord)h_generic_calc_NarrowBin16to8x8; goto binnish;
         case Iop_NarrowBin32to16x4:
            fn = (HWord)h_generic_calc_NarrowBin32to16x4; goto binnish;

         case Iop_QSub8Sx8:
            fn = (HWord)h_generic_calc_QSub8Sx8; goto binnish;
         case Iop_QSub16Sx4:
            fn = (HWord)h_generic_calc_QSub16Sx4; goto binnish;
         case Iop_QSub8Ux8:
            fn = (HWord)h_generic_calc_QSub8Ux8; goto binnish;
         case Iop_QSub16Ux4:
            fn = (HWord)h_generic_calc_QSub16Ux4; goto binnish;

         case Iop_Sub8x8:
            fn = (HWord)h_generic_calc_Sub8x8; goto binnish;
         case Iop_Sub16x4:
            fn = (HWord)h_generic_calc_Sub16x4; goto binnish;
         case Iop_Sub32x2:
            fn = (HWord)h_generic_calc_Sub32x2; goto binnish;

         binnish: {
            /* Note: the following assumes all helpers are of
               signature 
                  ULong fn ( ULong, ULong ), and they are
               not marked as regparm functions. 
            */
            HReg xLo, xHi, yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, X86Instr_Push(X86RMI_Reg(yHi)));
            addInstr(env, X86Instr_Push(X86RMI_Reg(yLo)));
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            addInstr(env, X86Instr_Push(X86RMI_Reg(xHi)));
            addInstr(env, X86Instr_Push(X86RMI_Reg(xLo)));
            addInstr(env, X86Instr_Call( Xcc_ALWAYS, (Addr32)fn,
                                         0, mk_RetLoc_simple(RLPri_2Int) ));
            add_to_esp(env, 4*4);
            addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
            addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_ShlN32x2:
            fn = (HWord)h_generic_calc_ShlN32x2; goto shifty;
         case Iop_ShlN16x4:
            fn = (HWord)h_generic_calc_ShlN16x4; goto shifty;
         case Iop_ShlN8x8:
            fn = (HWord)h_generic_calc_ShlN8x8;  goto shifty;
         case Iop_ShrN32x2:
            fn = (HWord)h_generic_calc_ShrN32x2; goto shifty;
         case Iop_ShrN16x4:
            fn = (HWord)h_generic_calc_ShrN16x4; goto shifty;
         case Iop_SarN32x2:
            fn = (HWord)h_generic_calc_SarN32x2; goto shifty;
         case Iop_SarN16x4:
            fn = (HWord)h_generic_calc_SarN16x4; goto shifty;
         case Iop_SarN8x8:
            fn = (HWord)h_generic_calc_SarN8x8;  goto shifty;
         shifty: {
            /* Note: the following assumes all helpers are of
               signature 
                  ULong fn ( ULong, UInt ), and they are
               not marked as regparm functions. 
            */
            HReg xLo, xHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            X86RMI* y = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
            addInstr(env, X86Instr_Push(y));
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            addInstr(env, X86Instr_Push(X86RMI_Reg(xHi)));
            addInstr(env, X86Instr_Push(X86RMI_Reg(xLo)));
            addInstr(env, X86Instr_Call( Xcc_ALWAYS, (Addr32)fn,
                                         0, mk_RetLoc_simple(RLPri_2Int) ));
            add_to_esp(env, 3*4);
            addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
            addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
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

         /* 32Sto64(e) */
         case Iop_32Sto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src,tHi));
            addInstr(env, mk_iMOVsd_RR(src,tLo));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, tHi));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32Uto64(e) */
         case Iop_32Uto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src,tLo));
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tHi));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 16Uto64(e) */
         case Iop_16Uto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(src,tLo));
            addInstr(env, X86Instr_Alu32R(Xalu_AND,
                                          X86RMI_Imm(0xFFFF), tLo));
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tHi));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* V128{HI}to64 */
         case Iop_V128HIto64:
         case Iop_V128to64: {
            Int  off = e->Iex.Unop.op==Iop_V128HIto64 ? 8 : 0;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg vec = iselVecExpr(env, e->Iex.Unop.arg);
            X86AMode* esp0  = X86AMode_IR(0,     hregX86_ESP());
            X86AMode* espLO = X86AMode_IR(off,   hregX86_ESP());
            X86AMode* espHI = X86AMode_IR(off+4, hregX86_ESP());
            sub_from_esp(env, 16);
            addInstr(env, X86Instr_SseLdSt(False/*store*/, vec, esp0));
            addInstr(env, X86Instr_Alu32R( Xalu_MOV, 
                                           X86RMI_Mem(espLO), tLo ));
            addInstr(env, X86Instr_Alu32R( Xalu_MOV, 
                                           X86RMI_Mem(espHI), tHi ));
            add_to_esp(env, 16);
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* could do better than this, but for now ... */
         case Iop_1Sto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            X86CondCode cond = iselCondCode(env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Set32(cond,tLo));
            addInstr(env, X86Instr_Sh32(Xsh_SHL, 31, tLo));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, tLo));
            addInstr(env, mk_iMOVsd_RR(tLo, tHi));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* Not64(e) */
         case Iop_Not64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg sHi, sLo;
            iselInt64Expr(&sHi, &sLo, env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVsd_RR(sHi, tHi));
            addInstr(env, mk_iMOVsd_RR(sLo, tLo));
            addInstr(env, X86Instr_Unary32(Xun_NOT,tHi));
            addInstr(env, X86Instr_Unary32(Xun_NOT,tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* Left64(e) */
         case Iop_Left64: {
            HReg yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            /* yHi:yLo = arg */
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Unop.arg);
            /* tLo = 0 - yLo, and set carry */
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tLo));
            addInstr(env, X86Instr_Alu32R(Xalu_SUB, X86RMI_Reg(yLo), tLo));
            /* tHi = 0 - yHi - carry */
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Imm(0), tHi));
            addInstr(env, X86Instr_Alu32R(Xalu_SBB, X86RMI_Reg(yHi), tHi));
            /* So now we have tHi:tLo = -arg.  To finish off, or 'arg'
               back in, so as to give the final result 
               tHi:tLo = arg | -arg. */
            addInstr(env, X86Instr_Alu32R(Xalu_OR, X86RMI_Reg(yLo), tLo));
            addInstr(env, X86Instr_Alu32R(Xalu_OR, X86RMI_Reg(yHi), tHi));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* --- patterns rooted at: CmpwNEZ64 --- */

         /* CmpwNEZ64(e) */
         case Iop_CmpwNEZ64: {

         DECLARE_PATTERN(p_CmpwNEZ64_Or64);
         DEFINE_PATTERN(p_CmpwNEZ64_Or64,
                        unop(Iop_CmpwNEZ64,binop(Iop_Or64,bind(0),bind(1))));
         if (matchIRExpr(&mi, p_CmpwNEZ64_Or64, e)) {
            /* CmpwNEZ64(Or64(x,y)) */
            HReg xHi,xLo,yHi,yLo;
            HReg xBoth = newVRegI(env);
            HReg merged = newVRegI(env);
            HReg tmp2 = newVRegI(env);

            iselInt64Expr(&xHi,&xLo, env, mi.bindee[0]);
            addInstr(env, mk_iMOVsd_RR(xHi,xBoth));
            addInstr(env, X86Instr_Alu32R(Xalu_OR,
                                          X86RMI_Reg(xLo),xBoth));

            iselInt64Expr(&yHi,&yLo, env, mi.bindee[1]);
            addInstr(env, mk_iMOVsd_RR(yHi,merged));
            addInstr(env, X86Instr_Alu32R(Xalu_OR,
                                          X86RMI_Reg(yLo),merged));
            addInstr(env, X86Instr_Alu32R(Xalu_OR,
                                             X86RMI_Reg(xBoth),merged));

            /* tmp2 = (merged | -merged) >>s 31 */
            addInstr(env, mk_iMOVsd_RR(merged,tmp2));
            addInstr(env, X86Instr_Unary32(Xun_NEG,tmp2));
            addInstr(env, X86Instr_Alu32R(Xalu_OR,
                                          X86RMI_Reg(merged), tmp2));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, tmp2));
            *rHi = tmp2;
            *rLo = tmp2;
            return;
         } else {
            /* CmpwNEZ64(e) */
            HReg srcLo, srcHi;
            HReg tmp1  = newVRegI(env);
            HReg tmp2  = newVRegI(env);
            /* srcHi:srcLo = arg */
            iselInt64Expr(&srcHi, &srcLo, env, e->Iex.Unop.arg);
            /* tmp1 = srcHi | srcLo */
            addInstr(env, mk_iMOVsd_RR(srcHi,tmp1));
            addInstr(env, X86Instr_Alu32R(Xalu_OR,
                                          X86RMI_Reg(srcLo), tmp1));
            /* tmp2 = (tmp1 | -tmp1) >>s 31 */
            addInstr(env, mk_iMOVsd_RR(tmp1,tmp2));
            addInstr(env, X86Instr_Unary32(Xun_NEG,tmp2));
            addInstr(env, X86Instr_Alu32R(Xalu_OR,
                                          X86RMI_Reg(tmp1), tmp2));
            addInstr(env, X86Instr_Sh32(Xsh_SAR, 31, tmp2));
            *rHi = tmp2;
            *rLo = tmp2;
            return;
         }
         }

         /* ReinterpF64asI64(e) */
         /* Given an IEEE754 double, produce an I64 with the same bit
            pattern. */
         case Iop_ReinterpF64asI64: {
            HReg rf   = iselDblExpr(env, e->Iex.Unop.arg);
            HReg tLo  = newVRegI(env);
            HReg tHi  = newVRegI(env);
            X86AMode* zero_esp = X86AMode_IR(0, hregX86_ESP());
            X86AMode* four_esp = X86AMode_IR(4, hregX86_ESP());
            /* paranoia */
            set_FPU_rounding_default(env);
            /* subl $8, %esp */
            sub_from_esp(env, 8);
            /* gstD %rf, 0(%esp) */
            addInstr(env,
                     X86Instr_FpLdSt(False/*store*/, 8, rf, zero_esp));
            /* movl 0(%esp), %tLo */
            addInstr(env, 
                     X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(zero_esp), tLo));
            /* movl 4(%esp), %tHi */
            addInstr(env, 
                     X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(four_esp), tHi));
            /* addl $8, %esp */
            add_to_esp(env, 8);
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         case Iop_CmpNEZ32x2:
            fn = (HWord)h_generic_calc_CmpNEZ32x2; goto unish;
         case Iop_CmpNEZ16x4:
            fn = (HWord)h_generic_calc_CmpNEZ16x4; goto unish;
         case Iop_CmpNEZ8x8:
            fn = (HWord)h_generic_calc_CmpNEZ8x8; goto unish;
         unish: {
            /* Note: the following assumes all helpers are of
               signature 
                  ULong fn ( ULong ), and they are
               not marked as regparm functions. 
            */
            HReg xLo, xHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Push(X86RMI_Reg(xHi)));
            addInstr(env, X86Instr_Push(X86RMI_Reg(xLo)));
            addInstr(env, X86Instr_Call( Xcc_ALWAYS, (Addr32)fn,
                                         0, mk_RetLoc_simple(RLPri_2Int) ));
            add_to_esp(env, 2*4);
            addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
            addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         default: 
            break;
      }
   } /* if (e->tag == Iex_Unop) */


   /* --------- CCALL --------- */
   if (e->tag == Iex_CCall) {
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);

      /* Marshal args, do the call, clear stack. */
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();
      doHelperCall( &addToSp, &rloc, env, NULL/*guard*/,
                    e->Iex.CCall.cee,
                    e->Iex.CCall.retty, e->Iex.CCall.args );
      vassert(is_sane_RetLoc(rloc));
      vassert(rloc.pri == RLPri_2Int);
      vassert(addToSp == 0);
      /* */

      addInstr(env, mk_iMOVsd_RR(hregX86_EDX(), tHi));
      addInstr(env, mk_iMOVsd_RR(hregX86_EAX(), tLo));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   ppIRExpr(e);
   vpanic("iselInt64Expr");
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (32 bit)         ---*/
/*---------------------------------------------------------*/

/* Nothing interesting here; really just wrappers for
   64-bit stuff. */

static HReg iselFltExpr ( ISelEnv* env, const IRExpr* e )
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
static HReg iselFltExpr_wrk ( ISelEnv* env, const IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(ty == Ity_F32);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      X86AMode* am;
      HReg res = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_F32);
      am = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, X86Instr_FpLdSt(True/*load*/, 4, res, am));
      return res;
   }

   if (e->tag == Iex_Binop
       && e->Iex.Binop.op == Iop_F64toF32) {
      /* Although the result is still held in a standard FPU register,
         we need to round it to reflect the loss of accuracy/range
         entailed in casting it to a 32-bit float. */
      HReg dst = newVRegF(env);
      HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
      set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );
      addInstr(env, X86Instr_Fp64to32(src,dst));
      set_FPU_rounding_default( env );
      return dst;
   }

   if (e->tag == Iex_Get) {
      X86AMode* am = X86AMode_IR( e->Iex.Get.offset,
                                  hregX86_EBP() );
      HReg res = newVRegF(env);
      addInstr(env, X86Instr_FpLdSt( True/*load*/, 4, res, am ));
      return res;
   }

   if (e->tag == Iex_Unop
       && e->Iex.Unop.op == Iop_ReinterpI32asF32) {
       /* Given an I32, produce an IEEE754 float with the same bit
          pattern. */
      HReg    dst = newVRegF(env);
      X86RMI* rmi = iselIntExpr_RMI(env, e->Iex.Unop.arg);
      /* paranoia */
      addInstr(env, X86Instr_Push(rmi));
      addInstr(env, X86Instr_FpLdSt(
                       True/*load*/, 4, dst, 
                       X86AMode_IR(0, hregX86_ESP())));
      add_to_esp(env, 4);
      return dst;
   }

   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_RoundF32toInt) {
      HReg rf  = iselFltExpr(env, e->Iex.Binop.arg2);
      HReg dst = newVRegF(env);

      /* rf now holds the value to be rounded.  The first thing to do
         is set the FPU's rounding mode accordingly. */

      /* Set host rounding mode */
      set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

      /* grndint %rf, %dst */
      addInstr(env, X86Instr_FpUnary(Xfp_ROUND, rf, dst));

      /* Restore default FPU rounding. */
      set_FPU_rounding_default( env );

      return dst;
   }

   ppIRExpr(e);
   vpanic("iselFltExpr_wrk");
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

static HReg iselDblExpr ( ISelEnv* env, const IRExpr* e )
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
static HReg iselDblExpr_wrk ( ISelEnv* env, const IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_F64);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Const) {
      union { UInt u32x2[2]; ULong u64; Double f64; } u;
      HReg freg = newVRegF(env);
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
         vpanic("iselDblExpr(x86): const");

      addInstr(env, X86Instr_Push(X86RMI_Imm(u.u32x2[1])));
      addInstr(env, X86Instr_Push(X86RMI_Imm(u.u32x2[0])));
      addInstr(env, X86Instr_FpLdSt(True/*load*/, 8, freg, 
                                    X86AMode_IR(0, hregX86_ESP())));
      add_to_esp(env, 8);
      return freg;
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      X86AMode* am;
      HReg res = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_F64);
      am = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, X86Instr_FpLdSt(True/*load*/, 8, res, am));
      return res;
   }

   if (e->tag == Iex_Get) {
      X86AMode* am = X86AMode_IR( e->Iex.Get.offset,
                                  hregX86_EBP() );
      HReg res = newVRegF(env);
      addInstr(env, X86Instr_FpLdSt( True/*load*/, 8, res, am ));
      return res;
   }

   if (e->tag == Iex_GetI) {
      X86AMode* am 
         = genGuestArrayOffset(
              env, e->Iex.GetI.descr, 
                   e->Iex.GetI.ix, e->Iex.GetI.bias );
      HReg res = newVRegF(env);
      addInstr(env, X86Instr_FpLdSt( True/*load*/, 8, res, am ));
      return res;
   }

   if (e->tag == Iex_Triop) {
      X86FpOp fpop = Xfp_INVALID;
      IRTriop *triop = e->Iex.Triop.details;
      switch (triop->op) {
         case Iop_AddF64:    fpop = Xfp_ADD; break;
         case Iop_SubF64:    fpop = Xfp_SUB; break;
         case Iop_MulF64:    fpop = Xfp_MUL; break;
         case Iop_DivF64:    fpop = Xfp_DIV; break;
         case Iop_ScaleF64:  fpop = Xfp_SCALE; break;
         case Iop_Yl2xF64:   fpop = Xfp_YL2X; break;
         case Iop_Yl2xp1F64: fpop = Xfp_YL2XP1; break;
         case Iop_AtanF64:   fpop = Xfp_ATAN; break;
         case Iop_PRemF64:   fpop = Xfp_PREM; break;
         case Iop_PRem1F64:  fpop = Xfp_PREM1; break;
         default: break;
      }
      if (fpop != Xfp_INVALID) {
         HReg res  = newVRegF(env);
         HReg srcL = iselDblExpr(env, triop->arg2);
         HReg srcR = iselDblExpr(env, triop->arg3);
         /* XXXROUNDINGFIXME */
         /* set roundingmode here */
         addInstr(env, X86Instr_FpBinary(fpop,srcL,srcR,res));
	 if (fpop != Xfp_ADD && fpop != Xfp_SUB 
	     && fpop != Xfp_MUL && fpop != Xfp_DIV)
            roundToF64(env, res);
         return res;
      }
   }

   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_RoundF64toInt) {
      HReg rf  = iselDblExpr(env, e->Iex.Binop.arg2);
      HReg dst = newVRegF(env);

      /* rf now holds the value to be rounded.  The first thing to do
         is set the FPU's rounding mode accordingly. */

      /* Set host rounding mode */
      set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

      /* grndint %rf, %dst */
      addInstr(env, X86Instr_FpUnary(Xfp_ROUND, rf, dst));

      /* Restore default FPU rounding. */
      set_FPU_rounding_default( env );

      return dst;
   }

   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_I64StoF64) {
      HReg dst = newVRegF(env);
      HReg rHi,rLo;
      iselInt64Expr( &rHi, &rLo, env, e->Iex.Binop.arg2);
      addInstr(env, X86Instr_Push(X86RMI_Reg(rHi)));
      addInstr(env, X86Instr_Push(X86RMI_Reg(rLo)));

      /* Set host rounding mode */
      set_FPU_rounding_mode( env, e->Iex.Binop.arg1 );

      addInstr(env, X86Instr_FpLdStI(
                       True/*load*/, 8, dst, 
                       X86AMode_IR(0, hregX86_ESP())));

      /* Restore default FPU rounding. */
      set_FPU_rounding_default( env );

      add_to_esp(env, 8);
      return dst;
   }

   if (e->tag == Iex_Binop) {
      X86FpOp fpop = Xfp_INVALID;
      switch (e->Iex.Binop.op) {
         case Iop_SinF64:  fpop = Xfp_SIN; break;
         case Iop_CosF64:  fpop = Xfp_COS; break;
         case Iop_TanF64:  fpop = Xfp_TAN; break;
         case Iop_2xm1F64: fpop = Xfp_2XM1; break;
         case Iop_SqrtF64: fpop = Xfp_SQRT; break;
         default: break;
      }
      if (fpop != Xfp_INVALID) {
         HReg res = newVRegF(env);
         HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
         /* XXXROUNDINGFIXME */
         /* set roundingmode here */
         /* Note that X86Instr_FpUnary(Xfp_TAN,..) sets the condition
            codes.  I don't think that matters, since this insn
            selector never generates such an instruction intervening
            between an flag-setting instruction and a flag-using
            instruction. */
         addInstr(env, X86Instr_FpUnary(fpop,src,res));
	 if (fpop != Xfp_SQRT
             && fpop != Xfp_NEG && fpop != Xfp_ABS)
            roundToF64(env, res);
         return res;
      }
   }

   if (e->tag == Iex_Unop) {
      X86FpOp fpop = Xfp_INVALID;
      switch (e->Iex.Unop.op) {
         case Iop_NegF64:  fpop = Xfp_NEG; break;
         case Iop_AbsF64:  fpop = Xfp_ABS; break;
         default: break;
      }
      if (fpop != Xfp_INVALID) {
         HReg res = newVRegF(env);
         HReg src = iselDblExpr(env, e->Iex.Unop.arg);
         addInstr(env, X86Instr_FpUnary(fpop,src,res));
         /* No need to do roundToF64(env,res) for Xfp_NEG or Xfp_ABS,
            but might need to do that for other unary ops. */
         return res;
      }
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
         case Iop_I32StoF64: {
            HReg dst = newVRegF(env);
            HReg ri  = iselIntExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, X86Instr_Push(X86RMI_Reg(ri)));
            set_FPU_rounding_default(env);
            addInstr(env, X86Instr_FpLdStI(
                             True/*load*/, 4, dst, 
                             X86AMode_IR(0, hregX86_ESP())));
	    add_to_esp(env, 4);
            return dst;
         }
         case Iop_ReinterpI64asF64: {
            /* Given an I64, produce an IEEE754 double with the same
               bit pattern. */
            HReg dst = newVRegF(env);
            HReg rHi, rLo;
	    iselInt64Expr( &rHi, &rLo, env, e->Iex.Unop.arg);
            /* paranoia */
            set_FPU_rounding_default(env);
            addInstr(env, X86Instr_Push(X86RMI_Reg(rHi)));
            addInstr(env, X86Instr_Push(X86RMI_Reg(rLo)));
            addInstr(env, X86Instr_FpLdSt(
                             True/*load*/, 8, dst, 
                             X86AMode_IR(0, hregX86_ESP())));
	    add_to_esp(env, 8);
            return dst;
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
   if (e->tag == Iex_ITE) { // VFD
     if (ty == Ity_F64
         && typeOfIRExpr(env->type_env,e->Iex.ITE.cond) == Ity_I1) {
        HReg r1  = iselDblExpr(env, e->Iex.ITE.iftrue);
        HReg r0  = iselDblExpr(env, e->Iex.ITE.iffalse);
        HReg dst = newVRegF(env);
        addInstr(env, X86Instr_FpUnary(Xfp_MOV,r1,dst));
        X86CondCode cc = iselCondCode(env, e->Iex.ITE.cond);
        addInstr(env, X86Instr_FpCMov(cc ^ 1, r0, dst));
        return dst;
      }
   }

   ppIRExpr(e);
   vpanic("iselDblExpr_wrk");
}


/*---------------------------------------------------------*/
/*--- ISEL: SIMD (Vector) expressions, 128 bit.         ---*/
/*---------------------------------------------------------*/

static HReg iselVecExpr ( ISelEnv* env, const IRExpr* e )
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
static HReg iselVecExpr_wrk ( ISelEnv* env, const IRExpr* e )
{

#  define REQUIRE_SSE1                                    \
      do { if (env->hwcaps == 0/*baseline, no sse*/       \
               ||  env->hwcaps == VEX_HWCAPS_X86_MMXEXT /*Integer SSE*/) \
              goto vec_fail;                              \
      } while (0)

#  define REQUIRE_SSE2                                    \
      do { if (0 == (env->hwcaps & VEX_HWCAPS_X86_SSE2))  \
              goto vec_fail;                              \
      } while (0)

#  define SSE2_OR_ABOVE                                   \
       (env->hwcaps & VEX_HWCAPS_X86_SSE2)

   HWord     fn = 0; /* address of helper fn, if required */
   MatchInfo mi;
   Bool      arg1isEReg = False;
   X86SseOp  op = Xsse_INVALID;
   IRType    ty = typeOfIRExpr(env->type_env,e);
   vassert(e);
   vassert(ty == Ity_V128);

   REQUIRE_SSE1;

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Get) {
      HReg dst = newVRegV(env);
      addInstr(env, X86Instr_SseLdSt(
                       True/*load*/, 
                       dst,
                       X86AMode_IR(e->Iex.Get.offset, hregX86_EBP())
                    )
              );
      return dst;
   }

   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      HReg      dst = newVRegV(env);
      X86AMode* am  = iselIntExpr_AMode(env, e->Iex.Load.addr);
      addInstr(env, X86Instr_SseLdSt( True/*load*/, dst, am ));
      return dst;
   }

   if (e->tag == Iex_Const) {
      HReg dst = newVRegV(env);
      vassert(e->Iex.Const.con->tag == Ico_V128);
      addInstr(env, X86Instr_SseConst(e->Iex.Const.con->Ico.V128, dst));
      return dst;
   }

   if (e->tag == Iex_Unop) {

   if (SSE2_OR_ABOVE) { 
      /* 64UtoV128(LDle:I64(addr)) */
      DECLARE_PATTERN(p_zwiden_load64);
      DEFINE_PATTERN(p_zwiden_load64,
                     unop(Iop_64UtoV128, 
                          IRExpr_Load(Iend_LE,Ity_I64,bind(0))));
      if (matchIRExpr(&mi, p_zwiden_load64, e)) {
         X86AMode* am = iselIntExpr_AMode(env, mi.bindee[0]);
         HReg dst = newVRegV(env);
         addInstr(env, X86Instr_SseLdzLO(8, dst, am));
         return dst;
      }
   }

   switch (e->Iex.Unop.op) {

      case Iop_NotV128: {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         return do_sse_Not128(env, arg);
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
         HReg tmp  = newVRegV(env);
         HReg dst  = newVRegV(env);
         REQUIRE_SSE2;
         addInstr(env, X86Instr_SseReRg(Xsse_XOR, tmp, tmp));
         addInstr(env, X86Instr_SseReRg(Xsse_CMPEQ32, arg, tmp));
         tmp = do_sse_Not128(env, tmp);
         addInstr(env, X86Instr_SseShuf(0xB1, tmp, dst));
         addInstr(env, X86Instr_SseReRg(Xsse_OR, tmp, dst));
         return dst;
      }

      case Iop_CmpNEZ32x4: {
         /* Sigh, we have to generate lousy code since this has to
            work on SSE1 hosts */
         /* basically, the idea is: for each lane:
               movl lane, %r ; negl %r   (now CF = lane==0 ? 0 : 1)
               sbbl %r, %r               (now %r = 1Sto32(CF))
               movl %r, lane
         */
         Int       i;
         X86AMode* am;
         X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
         HReg      arg  = iselVecExpr(env, e->Iex.Unop.arg);
         HReg      dst  = newVRegV(env);
         HReg      r32  = newVRegI(env);
         sub_from_esp(env, 16);
         addInstr(env, X86Instr_SseLdSt(False/*store*/, arg, esp0));
         for (i = 0; i < 4; i++) {
            am = X86AMode_IR(i*4, hregX86_ESP());
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Mem(am), r32));
            addInstr(env, X86Instr_Unary32(Xun_NEG, r32));
            addInstr(env, X86Instr_Alu32R(Xalu_SBB, X86RMI_Reg(r32), r32));
            addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(r32), am));
         }
         addInstr(env, X86Instr_SseLdSt(True/*load*/, dst, esp0));
         add_to_esp(env, 16);
         return dst;
      }

      case Iop_CmpNEZ8x16:
      case Iop_CmpNEZ16x8: {
         /* We can use SSE2 instructions for this. */
         HReg arg;
         HReg vec0 = newVRegV(env);
         HReg vec1 = newVRegV(env);
         HReg dst  = newVRegV(env);
         X86SseOp cmpOp 
            = e->Iex.Unop.op==Iop_CmpNEZ16x8 ? Xsse_CMPEQ16
                                             : Xsse_CMPEQ8;
         REQUIRE_SSE2;
         addInstr(env, X86Instr_SseReRg(Xsse_XOR, vec0, vec0));
         addInstr(env, mk_vMOVsd_RR(vec0, vec1));
         addInstr(env, X86Instr_Sse32Fx4(Xsse_CMPEQF, vec1, vec1));
         /* defer arg computation to here so as to give CMPEQF as long
            as possible to complete */
         arg = iselVecExpr(env, e->Iex.Unop.arg);
         /* vec0 is all 0s; vec1 is all 1s */
         addInstr(env, mk_vMOVsd_RR(arg, dst));
         /* 16x8 or 8x16 comparison == */
         addInstr(env, X86Instr_SseReRg(cmpOp, vec0, dst));
         /* invert result */
         addInstr(env, X86Instr_SseReRg(Xsse_XOR, vec1, dst));
         return dst;
      }

      case Iop_RecipEst32Fx4: op = Xsse_RCPF;   goto do_32Fx4_unary;
      case Iop_RSqrtEst32Fx4: op = Xsse_RSQRTF; goto do_32Fx4_unary;
      do_32Fx4_unary:
      {
         HReg arg = iselVecExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegV(env);
         addInstr(env, X86Instr_Sse32Fx4(op, arg, dst));
         return dst;
      }

      case Iop_RecipEst32F0x4: op = Xsse_RCPF;   goto do_32F0x4_unary;
      case Iop_RSqrtEst32F0x4: op = Xsse_RSQRTF; goto do_32F0x4_unary;
      case Iop_Sqrt32F0x4:     op = Xsse_SQRTF;  goto do_32F0x4_unary;
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
         addInstr(env, X86Instr_Sse32FLo(op, arg, dst));
         return dst;
      }

      case Iop_Sqrt64F0x2:  op = Xsse_SQRTF;  goto do_64F0x2_unary;
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
         REQUIRE_SSE2;
         addInstr(env, mk_vMOVsd_RR(arg, dst));
         addInstr(env, X86Instr_Sse64FLo(op, arg, dst));
         return dst;
      }

      case Iop_32UtoV128: {
         HReg      dst  = newVRegV(env);
         X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
         X86RMI*   rmi  = iselIntExpr_RMI(env, e->Iex.Unop.arg);
         addInstr(env, X86Instr_Push(rmi));
	 addInstr(env, X86Instr_SseLdzLO(4, dst, esp0));
         add_to_esp(env, 4);
         return dst;
      }

      case Iop_64UtoV128: {
         HReg      rHi, rLo;
         HReg      dst  = newVRegV(env);
         X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
         iselInt64Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
         addInstr(env, X86Instr_Push(X86RMI_Reg(rHi)));
         addInstr(env, X86Instr_Push(X86RMI_Reg(rLo)));
	 addInstr(env, X86Instr_SseLdzLO(8, dst, esp0));
         add_to_esp(env, 8);
         return dst;
      }

      default:
         break;
   } /* switch (e->Iex.Unop.op) */
   } /* if (e->tag == Iex_Unop) */

   if (e->tag == Iex_Binop) {
   switch (e->Iex.Binop.op) {

      case Iop_Sqrt64Fx2:
         REQUIRE_SSE2;
         /* fallthrough */
      case Iop_Sqrt32Fx4: {
         /* :: (rmode, vec) -> vec */
         HReg arg = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         /* XXXROUNDINGFIXME */
         /* set roundingmode here */
         addInstr(env, (e->Iex.Binop.op == Iop_Sqrt64Fx2 
                           ? X86Instr_Sse64Fx2 : X86Instr_Sse32Fx4)
                       (Xsse_SQRTF, arg, dst));
         return dst;
      }

      case Iop_SetV128lo32: {
         HReg dst = newVRegV(env);
         HReg srcV = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg srcI = iselIntExpr_R(env, e->Iex.Binop.arg2);
         X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
         sub_from_esp(env, 16);
         addInstr(env, X86Instr_SseLdSt(False/*store*/, srcV, esp0));
         addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(srcI), esp0));
         addInstr(env, X86Instr_SseLdSt(True/*load*/, dst, esp0));
         add_to_esp(env, 16);
         return dst;
      }

      case Iop_SetV128lo64: {
         HReg dst = newVRegV(env);
         HReg srcV = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg srcIhi, srcIlo;
         X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
         X86AMode* esp4 = advance4(esp0);
         iselInt64Expr(&srcIhi, &srcIlo, env, e->Iex.Binop.arg2);
         sub_from_esp(env, 16);
         addInstr(env, X86Instr_SseLdSt(False/*store*/, srcV, esp0));
         addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(srcIlo), esp0));
         addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(srcIhi), esp4));
         addInstr(env, X86Instr_SseLdSt(True/*load*/, dst, esp0));
         add_to_esp(env, 16);
         return dst;
      }

      case Iop_64HLtoV128: {
         HReg r3, r2, r1, r0;
         X86AMode* esp0  = X86AMode_IR(0, hregX86_ESP());
         X86AMode* esp4  = advance4(esp0);
         X86AMode* esp8  = advance4(esp4);
         X86AMode* esp12 = advance4(esp8);
         HReg dst = newVRegV(env);
	 /* do this via the stack (easy, convenient, etc) */
         sub_from_esp(env, 16);
         /* Do the less significant 64 bits */
         iselInt64Expr(&r1, &r0, env, e->Iex.Binop.arg2);
         addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(r0), esp0));
         addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(r1), esp4));
         /* Do the more significant 64 bits */
         iselInt64Expr(&r3, &r2, env, e->Iex.Binop.arg1);
         addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(r2), esp8));
         addInstr(env, X86Instr_Alu32M(Xalu_MOV, X86RI_Reg(r3), esp12));
	 /* Fetch result back from stack. */
         addInstr(env, X86Instr_SseLdSt(True/*load*/, dst, esp0));
         add_to_esp(env, 16);
         return dst;
      }

      case Iop_CmpEQ32Fx4: op = Xsse_CMPEQF; goto do_32Fx4;
      case Iop_CmpLT32Fx4: op = Xsse_CMPLTF; goto do_32Fx4;
      case Iop_CmpLE32Fx4: op = Xsse_CMPLEF; goto do_32Fx4;
      case Iop_CmpUN32Fx4: op = Xsse_CMPUNF; goto do_32Fx4;
      case Iop_Max32Fx4:   op = Xsse_MAXF;   goto do_32Fx4;
      case Iop_Min32Fx4:   op = Xsse_MINF;   goto do_32Fx4;
      do_32Fx4:
      {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         addInstr(env, X86Instr_Sse32Fx4(op, argR, dst));
         return dst;
      }

      case Iop_CmpEQ64Fx2: op = Xsse_CMPEQF; goto do_64Fx2;
      case Iop_CmpLT64Fx2: op = Xsse_CMPLTF; goto do_64Fx2;
      case Iop_CmpLE64Fx2: op = Xsse_CMPLEF; goto do_64Fx2;
      case Iop_CmpUN64Fx2: op = Xsse_CMPUNF; goto do_64Fx2;
      case Iop_Max64Fx2:   op = Xsse_MAXF;   goto do_64Fx2;
      case Iop_Min64Fx2:   op = Xsse_MINF;   goto do_64Fx2;
      do_64Fx2:
      {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         REQUIRE_SSE2;
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         addInstr(env, X86Instr_Sse64Fx2(op, argR, dst));
         return dst;
      }

      case Iop_CmpEQ32F0x4: op = Xsse_CMPEQF; goto do_32F0x4;
      case Iop_CmpLT32F0x4: op = Xsse_CMPLTF; goto do_32F0x4;
      case Iop_CmpLE32F0x4: op = Xsse_CMPLEF; goto do_32F0x4;
      case Iop_CmpUN32F0x4: op = Xsse_CMPUNF; goto do_32F0x4;
      case Iop_Add32F0x4:   op = Xsse_ADDF;   goto do_32F0x4;
      case Iop_Div32F0x4:   op = Xsse_DIVF;   goto do_32F0x4;
      case Iop_Max32F0x4:   op = Xsse_MAXF;   goto do_32F0x4;
      case Iop_Min32F0x4:   op = Xsse_MINF;   goto do_32F0x4;
      case Iop_Mul32F0x4:   op = Xsse_MULF;   goto do_32F0x4;
      case Iop_Sub32F0x4:   op = Xsse_SUBF;   goto do_32F0x4;
      do_32F0x4: {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         addInstr(env, X86Instr_Sse32FLo(op, argR, dst));
         return dst;
      }

      case Iop_CmpEQ64F0x2: op = Xsse_CMPEQF; goto do_64F0x2;
      case Iop_CmpLT64F0x2: op = Xsse_CMPLTF; goto do_64F0x2;
      case Iop_CmpLE64F0x2: op = Xsse_CMPLEF; goto do_64F0x2;
      case Iop_CmpUN64F0x2: op = Xsse_CMPUNF; goto do_64F0x2;
      case Iop_Add64F0x2:   op = Xsse_ADDF;   goto do_64F0x2;
      case Iop_Div64F0x2:   op = Xsse_DIVF;   goto do_64F0x2;
      case Iop_Max64F0x2:   op = Xsse_MAXF;   goto do_64F0x2;
      case Iop_Min64F0x2:   op = Xsse_MINF;   goto do_64F0x2;
      case Iop_Mul64F0x2:   op = Xsse_MULF;   goto do_64F0x2;
      case Iop_Sub64F0x2:   op = Xsse_SUBF;   goto do_64F0x2;
      do_64F0x2: {
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         REQUIRE_SSE2;
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         addInstr(env, X86Instr_Sse64FLo(op, argR, dst));
         return dst;
      }

      case Iop_QNarrowBin32Sto16Sx8: 
         op = Xsse_PACKSSD; arg1isEReg = True; goto do_SseReRg;
      case Iop_QNarrowBin16Sto8Sx16: 
         op = Xsse_PACKSSW; arg1isEReg = True; goto do_SseReRg;
      case Iop_QNarrowBin16Sto8Ux16: 
         op = Xsse_PACKUSW; arg1isEReg = True; goto do_SseReRg;

      case Iop_InterleaveHI8x16: 
         op = Xsse_UNPCKHB; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveHI16x8: 
         op = Xsse_UNPCKHW; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveHI32x4: 
         op = Xsse_UNPCKHD; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveHI64x2: 
         op = Xsse_UNPCKHQ; arg1isEReg = True; goto do_SseReRg;

      case Iop_InterleaveLO8x16: 
         op = Xsse_UNPCKLB; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveLO16x8: 
         op = Xsse_UNPCKLW; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveLO32x4: 
         op = Xsse_UNPCKLD; arg1isEReg = True; goto do_SseReRg;
      case Iop_InterleaveLO64x2: 
         op = Xsse_UNPCKLQ; arg1isEReg = True; goto do_SseReRg;

      case Iop_AndV128:    op = Xsse_AND;      goto do_SseReRg;
      case Iop_OrV128:     op = Xsse_OR;       goto do_SseReRg;
      case Iop_XorV128:    op = Xsse_XOR;      goto do_SseReRg;
      case Iop_Add8x16:    op = Xsse_ADD8;     goto do_SseReRg;
      case Iop_Add16x8:    op = Xsse_ADD16;    goto do_SseReRg;
      case Iop_Add32x4:    op = Xsse_ADD32;    goto do_SseReRg;
      case Iop_Add64x2:    op = Xsse_ADD64;    goto do_SseReRg;
      case Iop_QAdd8Sx16:  op = Xsse_QADD8S;   goto do_SseReRg;
      case Iop_QAdd16Sx8:  op = Xsse_QADD16S;  goto do_SseReRg;
      case Iop_QAdd8Ux16:  op = Xsse_QADD8U;   goto do_SseReRg;
      case Iop_QAdd16Ux8:  op = Xsse_QADD16U;  goto do_SseReRg;
      case Iop_Avg8Ux16:   op = Xsse_AVG8U;    goto do_SseReRg;
      case Iop_Avg16Ux8:   op = Xsse_AVG16U;   goto do_SseReRg;
      case Iop_CmpEQ8x16:  op = Xsse_CMPEQ8;   goto do_SseReRg;
      case Iop_CmpEQ16x8:  op = Xsse_CMPEQ16;  goto do_SseReRg;
      case Iop_CmpEQ32x4:  op = Xsse_CMPEQ32;  goto do_SseReRg;
      case Iop_CmpGT8Sx16: op = Xsse_CMPGT8S;  goto do_SseReRg;
      case Iop_CmpGT16Sx8: op = Xsse_CMPGT16S; goto do_SseReRg;
      case Iop_CmpGT32Sx4: op = Xsse_CMPGT32S; goto do_SseReRg;
      case Iop_Max16Sx8:   op = Xsse_MAX16S;   goto do_SseReRg;
      case Iop_Max8Ux16:   op = Xsse_MAX8U;    goto do_SseReRg;
      case Iop_Min16Sx8:   op = Xsse_MIN16S;   goto do_SseReRg;
      case Iop_Min8Ux16:   op = Xsse_MIN8U;    goto do_SseReRg;
      case Iop_MulHi16Ux8: op = Xsse_MULHI16U; goto do_SseReRg;
      case Iop_MulHi16Sx8: op = Xsse_MULHI16S; goto do_SseReRg;
      case Iop_Mul16x8:    op = Xsse_MUL16;    goto do_SseReRg;
      case Iop_Sub8x16:    op = Xsse_SUB8;     goto do_SseReRg;
      case Iop_Sub16x8:    op = Xsse_SUB16;    goto do_SseReRg;
      case Iop_Sub32x4:    op = Xsse_SUB32;    goto do_SseReRg;
      case Iop_Sub64x2:    op = Xsse_SUB64;    goto do_SseReRg;
      case Iop_QSub8Sx16:  op = Xsse_QSUB8S;   goto do_SseReRg;
      case Iop_QSub16Sx8:  op = Xsse_QSUB16S;  goto do_SseReRg;
      case Iop_QSub8Ux16:  op = Xsse_QSUB8U;   goto do_SseReRg;
      case Iop_QSub16Ux8:  op = Xsse_QSUB16U;  goto do_SseReRg;
      do_SseReRg: {
         HReg arg1 = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg arg2 = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg dst = newVRegV(env);
         if (op != Xsse_OR && op != Xsse_AND && op != Xsse_XOR)
            REQUIRE_SSE2;
         if (arg1isEReg) {
            addInstr(env, mk_vMOVsd_RR(arg2, dst));
            addInstr(env, X86Instr_SseReRg(op, arg1, dst));
         } else {
            addInstr(env, mk_vMOVsd_RR(arg1, dst));
            addInstr(env, X86Instr_SseReRg(op, arg2, dst));
         }
         return dst;
      }

      case Iop_ShlN8x16: {
         /* This instruction doesn't exist so we need to fake it using
            Xsse_SHL16 and Xsse_SHR16.

            We'd like to shift every byte in the 16-byte register to the left by
            some amount.

            Instead, we will make a copy and shift all the 16-bit words to the
            *right* by 8 and then to the left by 8 plus the shift amount.  That
            will get us the correct answer for the upper 8 bits of each 16-bit
            word and zero elsewhere.

            Then we will shift all the 16-bit words in the original to the left
            by 8 plus the shift amount and then to the right by 8.  This will
            get the correct answer for the lower 8 bits of each 16-bit word and
            zero elsewhere.

            Finally, we will OR those two results together.

            Because we don't have a shift by constant in x86, we store the
            constant 8 into a register and shift by that as needed.
         */
         HReg      greg  = iselVecExpr(env, e->Iex.Binop.arg1);
         X86RMI*   rmi   = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         X86AMode* esp0  = X86AMode_IR(0, hregX86_ESP());
         HReg      ereg  = newVRegV(env);
         HReg      eight = newVRegV(env); // To store the constant value 8.
         HReg      dst   = newVRegV(env);
         HReg      hi    = newVRegV(env);
         REQUIRE_SSE2;
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(rmi));
         addInstr(env, X86Instr_SseLdSt(True/*load*/, ereg, esp0));
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(X86RMI_Imm(8)));
         addInstr(env, X86Instr_SseLdSt(True/*load*/, eight, esp0));

         op = Xsse_SHL16;
         X86SseOp reverse_op = Xsse_SHR16;
         addInstr(env, mk_vMOVsd_RR(greg, hi));
         addInstr(env, X86Instr_SseReRg(reverse_op, eight, hi));
         addInstr(env, X86Instr_SseReRg(op, eight, hi));
         addInstr(env, X86Instr_SseReRg(op, ereg, hi));
         addInstr(env, mk_vMOVsd_RR(greg, dst));
         addInstr(env, X86Instr_SseReRg(op, eight, dst));
         addInstr(env, X86Instr_SseReRg(op, ereg, dst));
         addInstr(env, X86Instr_SseReRg(reverse_op, eight, dst));
         addInstr(env, X86Instr_SseReRg(Xsse_OR, hi, dst));

         add_to_esp(env, 32);
         return dst;
      }
      case Iop_ShlN16x8: op = Xsse_SHL16; goto do_SseShift;
      case Iop_ShlN32x4: op = Xsse_SHL32; goto do_SseShift;
      case Iop_ShlN64x2: op = Xsse_SHL64; goto do_SseShift;
      case Iop_SarN16x8: op = Xsse_SAR16; goto do_SseShift;
      case Iop_SarN32x4: op = Xsse_SAR32; goto do_SseShift;
      case Iop_ShrN16x8: op = Xsse_SHR16; goto do_SseShift;
      case Iop_ShrN32x4: op = Xsse_SHR32; goto do_SseShift;
      case Iop_ShrN64x2: op = Xsse_SHR64; goto do_SseShift;
      do_SseShift: {
         HReg      greg = iselVecExpr(env, e->Iex.Binop.arg1);
         X86RMI*   rmi  = iselIntExpr_RMI(env, e->Iex.Binop.arg2);
         X86AMode* esp0 = X86AMode_IR(0, hregX86_ESP());
         HReg      ereg = newVRegV(env);
         HReg      dst  = newVRegV(env);
         REQUIRE_SSE2;
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(X86RMI_Imm(0)));
         addInstr(env, X86Instr_Push(rmi));
         addInstr(env, X86Instr_SseLdSt(True/*load*/, ereg, esp0));
	 addInstr(env, mk_vMOVsd_RR(greg, dst));
         addInstr(env, X86Instr_SseReRg(op, ereg, dst));
         add_to_esp(env, 16);
         return dst;
      }

      case Iop_NarrowBin32to16x8:
         fn = (HWord)h_generic_calc_NarrowBin32to16x8;
         goto do_SseAssistedBinary;
      case Iop_NarrowBin16to8x16:
         fn = (HWord)h_generic_calc_NarrowBin16to8x16;
         goto do_SseAssistedBinary;
      do_SseAssistedBinary: {
         /* As with the amd64 case (where this is copied from) we
            generate pretty bad code. */
         vassert(fn != 0);
         HReg dst = newVRegV(env);
         HReg argL = iselVecExpr(env, e->Iex.Binop.arg1);
         HReg argR = iselVecExpr(env, e->Iex.Binop.arg2);
         HReg argp = newVRegI(env);
         /* subl $112, %esp         -- make a space */
         sub_from_esp(env, 112);
         /* leal 48(%esp), %r_argp  -- point into it */
         addInstr(env, X86Instr_Lea32(X86AMode_IR(48, hregX86_ESP()),
                                      argp));
         /* andl $-16, %r_argp      -- 16-align the pointer */
         addInstr(env, X86Instr_Alu32R(Xalu_AND,
                                       X86RMI_Imm( ~(UInt)15 ), 
                                       argp));
         /* Prepare 3 arg regs:
            leal  0(%r_argp), %eax
            leal 16(%r_argp), %edx
            leal 32(%r_argp), %ecx
         */
         addInstr(env, X86Instr_Lea32(X86AMode_IR(0, argp),
                                      hregX86_EAX()));
         addInstr(env, X86Instr_Lea32(X86AMode_IR(16, argp),
                                      hregX86_EDX()));
         addInstr(env, X86Instr_Lea32(X86AMode_IR(32, argp),
                                      hregX86_ECX()));
         /* Store the two args, at (%edx) and (%ecx):
            movupd  %argL, 0(%edx)
            movupd  %argR, 0(%ecx)
         */
         addInstr(env, X86Instr_SseLdSt(False/*!isLoad*/, argL,
                                        X86AMode_IR(0, hregX86_EDX())));
         addInstr(env, X86Instr_SseLdSt(False/*!isLoad*/, argR,
                                        X86AMode_IR(0, hregX86_ECX())));
         /* call the helper */
         addInstr(env, X86Instr_Call( Xcc_ALWAYS, (Addr32)fn,
                                      3, mk_RetLoc_simple(RLPri_None) ));
         /* fetch the result from memory, using %r_argp, which the
            register allocator will keep alive across the call. */
         addInstr(env, X86Instr_SseLdSt(True/*isLoad*/, dst,
                                        X86AMode_IR(0, argp)));
         /* and finally, clear the space */
         add_to_esp(env, 112);
         return dst;
      }

      default:
         break;
   } /* switch (e->Iex.Binop.op) */
   } /* if (e->tag == Iex_Binop) */


   if (e->tag == Iex_Triop) {
   IRTriop *triop = e->Iex.Triop.details;
   switch (triop->op) {

      case Iop_Add32Fx4: op = Xsse_ADDF; goto do_32Fx4_w_rm;
      case Iop_Sub32Fx4: op = Xsse_SUBF; goto do_32Fx4_w_rm;
      case Iop_Mul32Fx4: op = Xsse_MULF; goto do_32Fx4_w_rm;
      case Iop_Div32Fx4: op = Xsse_DIVF; goto do_32Fx4_w_rm;
      do_32Fx4_w_rm:
      {
         HReg argL = iselVecExpr(env, triop->arg2);
         HReg argR = iselVecExpr(env, triop->arg3);
         HReg dst = newVRegV(env);
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         /* XXXROUNDINGFIXME */
         /* set roundingmode here */
         addInstr(env, X86Instr_Sse32Fx4(op, argR, dst));
         return dst;
      }

      case Iop_Add64Fx2: op = Xsse_ADDF; goto do_64Fx2_w_rm;
      case Iop_Sub64Fx2: op = Xsse_SUBF; goto do_64Fx2_w_rm;
      case Iop_Mul64Fx2: op = Xsse_MULF; goto do_64Fx2_w_rm;
      case Iop_Div64Fx2: op = Xsse_DIVF; goto do_64Fx2_w_rm;
      do_64Fx2_w_rm:
      {
         HReg argL = iselVecExpr(env, triop->arg2);
         HReg argR = iselVecExpr(env, triop->arg3);
         HReg dst = newVRegV(env);
         REQUIRE_SSE2;
         addInstr(env, mk_vMOVsd_RR(argL, dst));
         /* XXXROUNDINGFIXME */
         /* set roundingmode here */
         addInstr(env, X86Instr_Sse64Fx2(op, argR, dst));
         return dst;
      }

      default:
         break;
   } /* switch (triop->op) */
   } /* if (e->tag == Iex_Triop) */


   if (e->tag == Iex_ITE) { // VFD
      HReg r1  = iselVecExpr(env, e->Iex.ITE.iftrue);
      HReg r0  = iselVecExpr(env, e->Iex.ITE.iffalse);
      HReg dst = newVRegV(env);
      addInstr(env, mk_vMOVsd_RR(r1,dst));
      X86CondCode cc = iselCondCode(env, e->Iex.ITE.cond);
      addInstr(env, X86Instr_SseCMov(cc ^ 1, r0, dst));
      return dst;
   }

   vec_fail:
   vex_printf("iselVecExpr (hwcaps = %s): can't reduce\n",
              LibVEX_ppVexHwCaps(VexArchX86,env->hwcaps));
   ppIRExpr(e);
   vpanic("iselVecExpr_wrk");

#  undef REQUIRE_SSE1
#  undef REQUIRE_SSE2
#  undef SSE2_OR_ABOVE
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

      if (tya != Ity_I32 || end != Iend_LE) 
         goto stmt_fail;

      if (tyd == Ity_I32) {
         X86AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         X86RI* ri = iselIntExpr_RI(env, stmt->Ist.Store.data);
         addInstr(env, X86Instr_Alu32M(Xalu_MOV,ri,am));
         return;
      }
      if (tyd == Ity_I8 || tyd == Ity_I16) {
         X86AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         HReg r = iselIntExpr_R(env, stmt->Ist.Store.data);
         addInstr(env, X86Instr_Store( toUChar(tyd==Ity_I8 ? 1 : 2),
                                       r,am ));
         return;
      }
      if (tyd == Ity_F64) {
         X86AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         HReg r = iselDblExpr(env, stmt->Ist.Store.data);
         addInstr(env, X86Instr_FpLdSt(False/*store*/, 8, r, am));
         return;
      }
      if (tyd == Ity_F32) {
         X86AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         HReg r = iselFltExpr(env, stmt->Ist.Store.data);
         addInstr(env, X86Instr_FpLdSt(False/*store*/, 4, r, am));
         return;
      }
      if (tyd == Ity_I64) {
         HReg vHi, vLo, rA;
         iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Store.data);
         rA = iselIntExpr_R(env, stmt->Ist.Store.addr);
         addInstr(env, X86Instr_Alu32M(
                          Xalu_MOV, X86RI_Reg(vLo), X86AMode_IR(0, rA)));
         addInstr(env, X86Instr_Alu32M(
                          Xalu_MOV, X86RI_Reg(vHi), X86AMode_IR(4, rA)));
         return;
      }
      if (tyd == Ity_V128) {
         X86AMode* am = iselIntExpr_AMode(env, stmt->Ist.Store.addr);
         HReg r = iselVecExpr(env, stmt->Ist.Store.data);
         addInstr(env, X86Instr_SseLdSt(False/*store*/, r, am));
         return;
      }
      break;
   }

   /* --------- PUT --------- */
   case Ist_Put: {
      IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
      if (ty == Ity_I32) {
         /* We're going to write to memory, so compute the RHS into an
            X86RI. */
         X86RI* ri = iselIntExpr_RI(env, stmt->Ist.Put.data);
         addInstr(env,
                  X86Instr_Alu32M(
                     Xalu_MOV,
                     ri,
                     X86AMode_IR(stmt->Ist.Put.offset,hregX86_EBP())
                 ));
         return;
      }
      if (ty == Ity_I8 || ty == Ity_I16) {
         HReg r = iselIntExpr_R(env, stmt->Ist.Put.data);
         addInstr(env, X86Instr_Store(
                          toUChar(ty==Ity_I8 ? 1 : 2),
                          r,
                          X86AMode_IR(stmt->Ist.Put.offset,
                                      hregX86_EBP())));
         return;
      }
      if (ty == Ity_I64) {
         HReg vHi, vLo;
         X86AMode* am  = X86AMode_IR(stmt->Ist.Put.offset, hregX86_EBP());
         X86AMode* am4 = advance4(am);
         iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Put.data);
         addInstr(env, X86Instr_Alu32M( Xalu_MOV, X86RI_Reg(vLo), am ));
         addInstr(env, X86Instr_Alu32M( Xalu_MOV, X86RI_Reg(vHi), am4 ));
         return;
      }
      if (ty == Ity_V128) {
         HReg      vec = iselVecExpr(env, stmt->Ist.Put.data);
         X86AMode* am  = X86AMode_IR(stmt->Ist.Put.offset, hregX86_EBP());
         addInstr(env, X86Instr_SseLdSt(False/*store*/, vec, am));
         return;
      }
      if (ty == Ity_F32) {
         HReg f32 = iselFltExpr(env, stmt->Ist.Put.data);
         X86AMode* am  = X86AMode_IR(stmt->Ist.Put.offset, hregX86_EBP());
         set_FPU_rounding_default(env); /* paranoia */
         addInstr(env, X86Instr_FpLdSt( False/*store*/, 4, f32, am ));
         return;
      }
      if (ty == Ity_F64) {
         HReg f64 = iselDblExpr(env, stmt->Ist.Put.data);
         X86AMode* am  = X86AMode_IR(stmt->Ist.Put.offset, hregX86_EBP());
         set_FPU_rounding_default(env); /* paranoia */
         addInstr(env, X86Instr_FpLdSt( False/*store*/, 8, f64, am ));
         return;
      }
      break;
   }

   /* --------- Indexed PUT --------- */
   case Ist_PutI: {
      IRPutI *puti = stmt->Ist.PutI.details;

      X86AMode* am 
         = genGuestArrayOffset(
              env, puti->descr, 
                   puti->ix, puti->bias );

      IRType ty = typeOfIRExpr(env->type_env, puti->data);
      if (ty == Ity_F64) {
         HReg val = iselDblExpr(env, puti->data);
         addInstr(env, X86Instr_FpLdSt( False/*store*/, 8, val, am ));
         return;
      }
      if (ty == Ity_I8) {
         HReg r = iselIntExpr_R(env, puti->data);
         addInstr(env, X86Instr_Store( 1, r, am ));
         return;
      }
      if (ty == Ity_I32) {
         HReg r = iselIntExpr_R(env, puti->data);
         addInstr(env, X86Instr_Alu32M( Xalu_MOV, X86RI_Reg(r), am ));
         return;
      }
      if (ty == Ity_I64) {
         HReg rHi, rLo;
         X86AMode* am4 = advance4(am);
         iselInt64Expr(&rHi, &rLo, env, puti->data);
         addInstr(env, X86Instr_Alu32M( Xalu_MOV, X86RI_Reg(rLo), am ));
         addInstr(env, X86Instr_Alu32M( Xalu_MOV, X86RI_Reg(rHi), am4 ));
         return;
      }
      break;
   }

   /* --------- TMP --------- */
   case Ist_WrTmp: {
      IRTemp tmp = stmt->Ist.WrTmp.tmp;
      IRType ty = typeOfIRTemp(env->type_env, tmp);

      /* optimisation: if stmt->Ist.WrTmp.data is Add32(..,..),
         compute it into an AMode and then use LEA.  This usually
         produces fewer instructions, often because (for memcheck
         created IR) we get t = address-expression, (t is later used
         twice) and so doing this naturally turns address-expression
         back into an X86 amode. */
      if (ty == Ity_I32 
          && stmt->Ist.WrTmp.data->tag == Iex_Binop
          && stmt->Ist.WrTmp.data->Iex.Binop.op == Iop_Add32) {
         X86AMode* am = iselIntExpr_AMode(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         if (am->tag == Xam_IR && am->Xam.IR.imm == 0) {
            /* Hmm, iselIntExpr_AMode wimped out and just computed the
               value into a register.  Just emit a normal reg-reg move
               so reg-alloc can coalesce it away in the usual way. */
            HReg src = am->Xam.IR.reg;
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, X86RMI_Reg(src), dst));
         } else {
            addInstr(env, X86Instr_Lea32(am,dst));
         }
         return;
      }

      if (ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
         X86RMI* rmi = iselIntExpr_RMI(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, X86Instr_Alu32R(Xalu_MOV,rmi,dst));
         return;
      }
      if (ty == Ity_I64) {
         HReg rHi, rLo, dstHi, dstLo;
         iselInt64Expr(&rHi,&rLo, env, stmt->Ist.WrTmp.data);
         lookupIRTemp64( &dstHi, &dstLo, env, tmp);
         addInstr(env, mk_iMOVsd_RR(rHi,dstHi) );
         addInstr(env, mk_iMOVsd_RR(rLo,dstLo) );
         return;
      }
      if (ty == Ity_I1) {
         X86CondCode cond = iselCondCode(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, X86Instr_Set32(cond, dst));
         return;
      }
      if (ty == Ity_F64) {
         HReg dst = lookupIRTemp(env, tmp);
         HReg src = iselDblExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, X86Instr_FpUnary(Xfp_MOV,src,dst));
         return;
      }
      if (ty == Ity_F32) {
         HReg dst = lookupIRTemp(env, tmp);
         HReg src = iselFltExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, X86Instr_FpUnary(Xfp_MOV,src,dst));
         return;
      }
      if (ty == Ity_V128) {
         HReg dst = lookupIRTemp(env, tmp);
         HReg src = iselVecExpr(env, stmt->Ist.WrTmp.data);
         addInstr(env, mk_vMOVsd_RR(src,dst));
         return;
      }
      break;
   }

   /* --------- Call to DIRTY helper --------- */
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

      /* Marshal args, do the call, and set the return value to
         0x555..555 if this is a conditional call that returns a value
         and the call is skipped. */
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
            /* The returned value is in %eax.  Park it in the register
               associated with tmp. */
            vassert(rloc.pri == RLPri_Int);
            vassert(addToSp == 0);
            HReg dst = lookupIRTemp(env, d->tmp);
            addInstr(env, mk_iMOVsd_RR(hregX86_EAX(),dst) );
            return;
         }
         case Ity_I64: {
            /* The returned value is in %edx:%eax.  Park it in the
               register-pair associated with tmp. */
            vassert(rloc.pri == RLPri_2Int);
            vassert(addToSp == 0);
            HReg dstHi, dstLo;
            lookupIRTemp64( &dstHi, &dstLo, env, d->tmp);
            addInstr(env, mk_iMOVsd_RR(hregX86_EDX(),dstHi) );
            addInstr(env, mk_iMOVsd_RR(hregX86_EAX(),dstLo) );
            return;
         }
         case Ity_V128: {
            /* The returned value is on the stack, and *retloc tells
               us where.  Fish it off the stack and then move the
               stack pointer upwards to clear it, as directed by
               doHelperCall. */
            vassert(rloc.pri == RLPri_V128SpRel);
            vassert(addToSp >= 16);
            HReg      dst = lookupIRTemp(env, d->tmp);
            X86AMode* am  = X86AMode_IR(rloc.spOff, hregX86_ESP());
            addInstr(env, X86Instr_SseLdSt( True/*load*/, dst, am ));
            add_to_esp(env, addToSp);
            return;
         }
         default:
            /*NOTREACHED*/
            vassert(0);
      }
      break;
   }

   /* --------- MEM FENCE --------- */
   case Ist_MBE:
      switch (stmt->Ist.MBE.event) {
         case Imbe_Fence:
            addInstr(env, X86Instr_MFence(env->hwcaps));
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
         /* get: cas->expdLo into %eax, and cas->dataLo into %ebx */
         X86AMode* am = iselIntExpr_AMode(env, cas->addr);
         HReg rDataLo = iselIntExpr_R(env, cas->dataLo);
         HReg rExpdLo = iselIntExpr_R(env, cas->expdLo);
         HReg rOldLo  = lookupIRTemp(env, cas->oldLo);
         vassert(cas->expdHi == NULL);
         vassert(cas->dataHi == NULL);
         addInstr(env, mk_iMOVsd_RR(rExpdLo, rOldLo));
         addInstr(env, mk_iMOVsd_RR(rExpdLo, hregX86_EAX()));
         addInstr(env, mk_iMOVsd_RR(rDataLo, hregX86_EBX()));
         switch (ty) { 
            case Ity_I32: sz = 4; break;
            case Ity_I16: sz = 2; break;
            case Ity_I8:  sz = 1; break; 
            default: goto unhandled_cas;
         }
         addInstr(env, X86Instr_ACAS(am, sz));
         addInstr(env,
                  X86Instr_CMov32(Xcc_NZ,
                                  X86RM_Reg(hregX86_EAX()), rOldLo));
         return;
      } else {
         /* double CAS */
         IRCAS* cas = stmt->Ist.CAS.details;
         IRType ty  = typeOfIRExpr(env->type_env, cas->dataLo);
         /* only 32-bit allowed in this case */
         /* get: cas->expdLo into %eax, and cas->dataLo into %ebx */
         /* get: cas->expdHi into %edx, and cas->dataHi into %ecx */
         X86AMode* am = iselIntExpr_AMode(env, cas->addr);
         HReg rDataHi = iselIntExpr_R(env, cas->dataHi);
         HReg rDataLo = iselIntExpr_R(env, cas->dataLo);
         HReg rExpdHi = iselIntExpr_R(env, cas->expdHi);
         HReg rExpdLo = iselIntExpr_R(env, cas->expdLo);
         HReg rOldHi  = lookupIRTemp(env, cas->oldHi);
         HReg rOldLo  = lookupIRTemp(env, cas->oldLo);
         if (ty != Ity_I32)
            goto unhandled_cas;
         addInstr(env, mk_iMOVsd_RR(rExpdHi, rOldHi));
         addInstr(env, mk_iMOVsd_RR(rExpdLo, rOldLo));
         addInstr(env, mk_iMOVsd_RR(rExpdHi, hregX86_EDX()));
         addInstr(env, mk_iMOVsd_RR(rExpdLo, hregX86_EAX()));
         addInstr(env, mk_iMOVsd_RR(rDataHi, hregX86_ECX()));
         addInstr(env, mk_iMOVsd_RR(rDataLo, hregX86_EBX()));
         addInstr(env, X86Instr_DACAS(am));
         addInstr(env,
                  X86Instr_CMov32(Xcc_NZ,
                                  X86RM_Reg(hregX86_EDX()), rOldHi));
         addInstr(env,
                  X86Instr_CMov32(Xcc_NZ,
                                  X86RM_Reg(hregX86_EAX()), rOldLo));
         return;
      }
      unhandled_cas:
      break;

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
      if (stmt->Ist.Exit.dst->tag != Ico_U32)
         vpanic("iselStmt(x86): Ist_Exit: dst is not a 32-bit value");

      X86CondCode cc    = iselCondCode(env, stmt->Ist.Exit.guard);
      X86AMode*   amEIP = X86AMode_IR(stmt->Ist.Exit.offsIP,
                                      hregX86_EBP());

      /* Case: boring transfer to known address */
      if (stmt->Ist.Exit.jk == Ijk_Boring) {
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = ((Addr32)stmt->Ist.Exit.dst->Ico.U32) > env->max_ga;
            if (0) vex_printf("%s", toFastEP ? "Y" : ",");
            addInstr(env, X86Instr_XDirect(stmt->Ist.Exit.dst->Ico.U32,
                                           amEIP, cc, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, X86Instr_XAssisted(r, amEIP, cc, Ijk_Boring));
         }
         return;
      }

      /* Case: assisted transfer to arbitrary address */
      switch (stmt->Ist.Exit.jk) {
         /* Keep this list in sync with that in iselNext below */
         case Ijk_ClientReq:
         case Ijk_EmWarn:
         case Ijk_MapFail:
         case Ijk_NoDecode:
         case Ijk_NoRedir:
         case Ijk_SigSEGV:
         case Ijk_SigTRAP:
         case Ijk_Sys_int128:
         case Ijk_Sys_int129:
         case Ijk_Sys_int130:
         case Ijk_Sys_int145:
         case Ijk_Sys_int210:
         case Ijk_Sys_syscall:
         case Ijk_Sys_sysenter:
         case Ijk_InvalICache:
         case Ijk_Yield:
         {
            HReg r = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, X86Instr_XAssisted(r, amEIP, cc, stmt->Ist.Exit.jk));
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
         X86AMode* amEIP = X86AMode_IR(offsIP, hregX86_EBP());
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = ((Addr32)cdst->Ico.U32) > env->max_ga;
            if (0) vex_printf("%s", toFastEP ? "X" : ".");
            addInstr(env, X86Instr_XDirect(cdst->Ico.U32,
                                           amEIP, Xcc_ALWAYS, 
                                           toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselIntExpr_R(env, next);
            addInstr(env, X86Instr_XAssisted(r, amEIP, Xcc_ALWAYS,
                                             Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
      case Ijk_Boring: case Ijk_Ret: case Ijk_Call: {
         HReg      r     = iselIntExpr_R(env, next);
         X86AMode* amEIP = X86AMode_IR(offsIP, hregX86_EBP());
         if (env->chainingAllowed) {
            addInstr(env, X86Instr_XIndir(r, amEIP, Xcc_ALWAYS));
         } else {
            addInstr(env, X86Instr_XAssisted(r, amEIP, Xcc_ALWAYS,
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
      case Ijk_MapFail:
      case Ijk_NoDecode:
      case Ijk_NoRedir:
      case Ijk_SigSEGV:
      case Ijk_SigTRAP:
      case Ijk_Sys_int128:
      case Ijk_Sys_int129:
      case Ijk_Sys_int130:
      case Ijk_Sys_int145:
      case Ijk_Sys_int210:
      case Ijk_Sys_syscall:
      case Ijk_Sys_sysenter:
      case Ijk_InvalICache:
      case Ijk_Yield:
      {
         HReg      r     = iselIntExpr_R(env, next);
         X86AMode* amEIP = X86AMode_IR(offsIP, hregX86_EBP());
         addInstr(env, X86Instr_XAssisted(r, amEIP, Xcc_ALWAYS, jk));
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

/* Translate an entire SB to x86 code. */

HInstrArray* iselSB_X86 ( const IRSB* bb,
                          VexArch      arch_host,
                          const VexArchInfo* archinfo_host,
                          const VexAbiInfo*  vbi/*UNUSED*/,
                          Int offs_Host_EvC_Counter,
                          Int offs_Host_EvC_FailAddr,
                          Bool chainingAllowed,
                          Bool addProfInc,
                          Addr max_ga )
{
   Int      i, j;
   HReg     hreg, hregHI;
   ISelEnv* env;
   UInt     hwcaps_host = archinfo_host->hwcaps;
   X86AMode *amCounter, *amFailAddr;

   /* sanity ... */
   vassert(arch_host == VexArchX86);
   vassert(0 == (hwcaps_host
                 & ~(VEX_HWCAPS_X86_MMXEXT
                     | VEX_HWCAPS_X86_SSE1
                     | VEX_HWCAPS_X86_SSE2
                     | VEX_HWCAPS_X86_SSE3
                     | VEX_HWCAPS_X86_LZCNT)));

   /* Check that the host's endianness is as expected. */
   vassert(archinfo_host->endness == VexEndnessLE);

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
         case Ity_I32:  hreg   = mkHReg(True, HRcInt32,  0, j++); break;
         case Ity_I64:  hreg   = mkHReg(True, HRcInt32,  0, j++);
                        hregHI = mkHReg(True, HRcInt32,  0, j++); break;
         case Ity_F32:
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
   amCounter  = X86AMode_IR(offs_Host_EvC_Counter,  hregX86_EBP());
   amFailAddr = X86AMode_IR(offs_Host_EvC_FailAddr, hregX86_EBP());
   addInstr(env, X86Instr_EvCheck(amCounter, amFailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, X86Instr_ProfInc());
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
/*--- end                                     host_x86_isel.c ---*/
/*---------------------------------------------------------------*/
