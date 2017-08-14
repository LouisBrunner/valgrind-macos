
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-x86/isel.c) is                          ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

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

#include "main/vex_util.h"
#include "main/vex_globals.h"
#include "host-generic/h_generic_regs.h"
#include "host-arm/hdefs.h"


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

   - The code array, that is, the insns selected so far.

   - A counter, for generating new virtual registers.

   Note, this is all host-independent.  */

typedef
   struct {
      IRTypeEnv*   type_env;

      HReg*        vregmap;
      Int          n_vregmap;

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
   HReg reg = mkHReg(env->vreg_ctr, HRcInt32, True/*virtual reg*/);
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
static ARMAMode1*     iselIntExpr_AMode1_wrk ( ISelEnv* env, IRExpr* e );
static ARMAMode1*     iselIntExpr_AMode1     ( ISelEnv* env, IRExpr* e );

/* static ARMAMode2*     iselIntExpr_AMode2_wrk ( ISelEnv* env, IRExpr* e ); */
static ARMAMode2*     iselIntExpr_AMode2     ( ISelEnv* env, IRExpr* e );

static ARMAMode3*     iselIntExpr_AMode3_wrk ( ISelEnv* env, IRExpr* e );
static ARMAMode3*     iselIntExpr_AMode3     ( ISelEnv* env, IRExpr* e );

static ARMBranchDest* iselIntExpr_BD_wrk ( ISelEnv* env, IRExpr* e );
static ARMBranchDest* iselIntExpr_BD     ( ISelEnv* env, IRExpr* e );

static ARMCondCode    iselCondCode_wrk ( ISelEnv* env, IRExpr* e );
static ARMCondCode    iselCondCode     ( ISelEnv* env, IRExpr* e );

static HReg           iselIntExpr_R_wrk ( ISelEnv* env, IRExpr* e );
static HReg           iselIntExpr_R     ( ISelEnv* env, IRExpr* e );

#if 0
static void        iselInt64Expr_wrk ( HReg* rHi, HReg* rLo, 
                                       ISelEnv* env, IRExpr* e );
static void        iselInt64Expr     ( HReg* rHi, HReg* rLo, 
                                       ISelEnv* env, IRExpr* e );
#endif


/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/
#if 0
/* Is this a 32-bit zero expression? */
static Bool isZero32 ( IRExpr* e )
{
   return e->tag == Iex_Const
          && e->Iex.Const.con->tag == Ico_U32
          && e->Iex.Const.con->Ico.U32 == 0;
}
#endif

/* Make a int reg-reg move. */
static ARMInstr* mk_iMOVsd_RR ( HReg src, HReg dst )
{
   vassert(hregClass(src) == HRcInt32);
   vassert(hregClass(dst) == HRcInt32);
   return ARMInstr_DPInstr1(ARMalu_MOV, dst, ARMAMode1_ShlI(src, 0));
}

#if 0
/* Advance/retreat stack pointer by n. */

static void add_to_sp ( ISelEnv* env, Int n )
{
    HReg tmp;
    ARMImm12A imm12a;
    vassert(n > 0 && n < 256 && (n%4) == 0);

    if ( mk_ARMImm12A( (UInt)n, &imm12a ) ) {
	addInstr(env, ARMInstr_DPInstr2(ARMalu_ADD,
					GET_SP_REG(), GET_SP_REG(),
					ARMAMode1_I12A( imm12a )));
    } else {
        tmp  = newVRegI(env);
        addInstr(env, ARMInstr_Literal( tmp, (UInt)n ));
	addInstr(env, ARMInstr_DPInstr2(ARMalu_ADD,
					GET_SP_REG(), GET_SP_REG(),
					ARMAMode1_ShlI( tmp, 0 )));
    }    
}

static void sub_from_sp ( ISelEnv* env, Int n )
{
    HReg tmp;
    ARMImm12A imm12a;
    vassert(n > 0 && n < 256 && (n%4) == 0);

    if ( mk_ARMImm12A( (UInt)n, &imm12a ) ) {
	addInstr(env, ARMInstr_DPInstr2(ARMalu_SUB,
					GET_SP_REG(), GET_SP_REG(),
					ARMAMode1_I12A( imm12a )));
    } else {
        tmp  = newVRegI(env);
        addInstr(env, ARMInstr_Literal( tmp, (UInt)n ));
	addInstr(env, ARMInstr_DPInstr2(ARMalu_SUB,
					GET_SP_REG(), GET_SP_REG(),
					ARMAMode1_ShlI( tmp, 0 )));
    }    
}
#endif

#if 0
/* Push an arg onto the host stack, in preparation for a call to a
   helper function of some kind.  Returns the number of 32-bit words
   pushed. */

static Int pushArg ( ISelEnv* env, IRExpr* arg )
{
    IRType arg_ty = typeOfIRExpr(env->type_env, arg);
    if (arg_ty == Ity_I32) {
	
	// CAB: This right?
	addInstr(env, ARMInstr_StoreW( GET_SP_REG(), iselIntExpr_AMode2(env, arg) ) );
	return 1;
    }

#if 0 
   else 
   if (arg_ty == Ity_I64) {
      HReg rHi, rLo;
      iselInt64Expr(&rHi, &rLo, env, arg);
      addInstr(env, X86Instr_Push(X86RMI_Reg(rHi)));
      addInstr(env, X86Instr_Push(X86RMI_Reg(rLo)));
      return 2;
   }
#endif
   ppIRExpr(arg);
   vpanic("pushArg(arm): can't handle arg of this type");
}
#endif

#if 0
/* Complete the call to a helper function, by calling the 
   helper and clearing the args off the stack. */

static 
void callHelperAndClearArgs ( ISelEnv* env, ARMCondCode cc, 
                              IRCallee* cee, Int n_arg_ws )
{
   /* Complication.  Need to decide which reg to use as the fn address
      pointer, in a way that doesn't trash regparm-passed
      parameters. */
   vassert(sizeof(void*) == 4);

// CAB: cee->regparms ?

//   addInstr(env, X86Instr_Call( cc, (UInt)cee->addr, cee->regparms));
   ARMBranchDest* dst = ARMBranchDest_Imm( (UInt)cee->addr );
   addInstr(env, ARMInstr_BranchL(cc, dst));

   if (n_arg_ws > 0)
      add_to_sp(env, 4*n_arg_ws);
}
#endif

#if 0
/* Used only in doHelperCall.  See big comment in doHelperCall re
   handling of regparm args.  This function figures out whether
   evaluation of an expression might require use of a fixed register.
   If in doubt return True (safe but suboptimal).  
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
#endif

/* Do a complete function call.  guard is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional. */

static
void doHelperCall ( ISelEnv* env, 
                    Bool passBBP, 
                    IRExpr* guard, IRCallee* cee, IRExpr** args )
{
#if 0
   ARMCondCode cc;
   HReg        argregs[3];
   HReg        tmpregs[3];
   Bool        danger;
   Int         not_done_yet, n_args, n_arg_ws, stack_limit, 
               i, argreg, argregX;

   /* Marshal args for a call, do the call, and clear the stack.
      Complexities to consider:

      * if passBBP is True, %ebp (the baseblock pointer) is to be
        passed as the first arg.

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

   n_args = n_arg_ws = 0;
   while (args[n_args]) n_args++;

   not_done_yet = n_args;
   if (passBBP)
      not_done_yet++;

   stack_limit = cee->regparms;
   if (cee->regparms > 0 && passBBP) stack_limit--;

   /* ------ BEGIN marshall all arguments ------ */

   /* Push (R to L) the stack-passed args, [n_args-1 .. stack_limit] */
   for (i = n_args-1; i >= stack_limit; i--) {
      n_arg_ws += pushArg(env, args[i]);
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

            argreg--;
            vassert(argreg >= 0);
            vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32);
            tmpregs[argreg] = iselIntExpr_R(env, args[i]);
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
            argreg--;
            vassert(argreg >= 0);
            vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32);
            addInstr(env, X86Instr_Alu32R(Xalu_MOV, 
                                          iselIntExpr_RMI(env, args[i]),
                                          argregs[argreg]));
            not_done_yet--;
         }

      }

      /* Not forgetting %ebp if needed. */
      if (passBBP) {
         vassert(argreg == 1);
         addInstr(env, mk_iMOVsd_RR( hregX86_EBP(), argregs[0]));
         not_done_yet--;
      }

      /* ------ END deal with regparms ------ */

   } else {

      /* No regparms.  Heave %ebp on the stack if needed. */
      if (passBBP) {
         addInstr(env, X86Instr_Push(X86RMI_Reg(hregX86_EBP())));
         n_arg_ws++;
         not_done_yet--;
      }

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

   /* call the helper, and get the args off the stack afterwards. */
   callHelperAndClearArgs( env, cc, cee, n_arg_ws );
#endif
}



// CAB: Do we need to deal with elemSz != 8 ?

/* Given a guest-state array descriptor, an index expression and a
   bias, generate an ARMAMode holding the relevant guest state
   offset. */

static
ARMAMode2* genGuestArrayOffset ( ISelEnv* env, IRRegArray* descr, 
				 IRExpr* off, Int bias )
{
   HReg tmp, tmp2, roff;
   Int  elemSz = sizeofIRType(descr->elemTy);
   Int  nElems = descr->nElems;
   ARMImm12A imm12a;

   /* throw out any cases not generated by an x86 front end.  In
      theory there might be a day where we need to handle them -- if
      we ever run non-x86-guest on x86 host. */

   if (nElems != 8 || (elemSz != 1 && elemSz != 8))
      vpanic("genGuestArrayOffset(arm host)");

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
       if ( mk_ARMImm12A( (UInt)bias, &imm12a ) ) {
	   addInstr(env, ARMInstr_DPInstr2(ARMalu_ADD, tmp, tmp,
					   ARMAMode1_I12A( imm12a )));
       } else {
	   HReg tmp3 = newVRegI(env);
	   addInstr(env, ARMInstr_Literal( tmp, (UInt)bias ));
	   addInstr(env, ARMInstr_DPInstr2(ARMalu_ADD, tmp, tmp,
					   ARMAMode1_ShlI( tmp3, 0 )));
       }
   }

   mk_ARMImm12A( (UInt)7, &imm12a );
   addInstr(env, ARMInstr_DPInstr2(ARMalu_AND, tmp, tmp,
				   ARMAMode1_I12A( imm12a )));
   vassert(elemSz == 1 || elemSz == 8);



// CAB: This anywhere near correct?

// X86AMode_IRRS: Immediate + Reg1 + (Reg2 << Shift)
// return X86AMode_IRRS( descr->base, hregX86_EBP(), tmp, elemSz==8 ? 3 : 0);

   tmp2 = newVRegI(env);  // tmp2 = GET_BP_REG + (tmp << 3|0)
   addInstr(env, ARMInstr_DPInstr2(ARMalu_ADD, tmp2, GET_BP_REG(),
				   ARMAMode1_ShlI(tmp, elemSz==8 ? 3 : 0)));
   return ARMAMode2_RI( tmp2, descr->base );
}


/*---------------------------------------------------------*/
/*--- ISEL ...                                           ---*/
/*---------------------------------------------------------*/

/* --------------------- AMODEs --------------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.
*/

/* ---------------- Addressing Mode 1 ---------------- */

static Bool sane_AMode1 ( ARMAMode1* am )
{
    switch (am->tag) {
    default:
	vpanic("sane_AMode1: unknown arm amode tag");
    }
}

static ARMAMode1* iselIntExpr_AMode1 ( ISelEnv* env, IRExpr* e )
{
    ARMAMode1* am = iselIntExpr_AMode1_wrk(env, e);
    vassert(sane_AMode1(am));
    return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARMAMode1* iselIntExpr_AMode1_wrk ( ISelEnv* env, IRExpr* e )
{
    IRType ty = typeOfIRExpr(env->type_env,e);
    vassert(ty == Ity_I32);
 
    // ARMam1_I12A,    /* Imm12A: extended (rotated) immedate */
    // ARMam1_ShlI,    /* ShlI  reg  Imm5 */
    // ARMam1_ShrI,    /* ShrI  reg  Imm5 */
    // ARMam1_SarI,    /* SarI  reg  Imm5 */
    // ARMam1_ShlR,    /* ShlR  reg  reg */
    // ARMam1_ShrR,    /* ShrR  reg  reg */
    // ARMam1_SarR,    /* SarR  reg  reg */

    // ALU ops:
    /*
      ARMalu_And, ARMalu_Orr, ARMalu_Eor, ARMalu_Bic, // Logic
      ARMalu_Sub, ARMalu_Rsb, ARMalu_Add, ARMalu_Adc, ARMalu_Sbc, ARMalu_Rsc,  // Arith
      ARMalu_Tst, ARMalu_Teq, ARMalu_Cmp, ARMalu_Cmn,  // test
      ARMalu_Mov, ARMalu_Mvn  // Move
    */


    return NULL; 
}



/* ---------------- Addressing Mode 2 ---------------- */

__attribute__((unused))
static Bool sane_AMode2 ( ARMAMode2* am )
{
   switch (am->tag) {
   default:
       vpanic("sane_AMode2: unknown arm amode tag");
   }
}

/* Apparently unused
static ARMAMode2* iselIntExpr_AMode2_wrk ( ISelEnv* env, IRExpr* e )
{
    ARMAMode2* am = iselIntExpr_AMode2_wrk(env, e);
    vassert(sane_AMode2(am));
    return am;
}
*/

/* DO NOT CALL THIS DIRECTLY ! */
static ARMAMode2* iselIntExpr_AMode2 ( ISelEnv* env, IRExpr* e )
{   
    IRType ty = typeOfIRExpr(env->type_env,e);
    vassert(ty == Ity_I32);

    // ARMam2_RI,      /* Reg +/- Imm12 */
    // ARMam2_RR,       /* Reg +/- Reg */
    // ARMam2_RRS,       /* Reg +/- (Reg << Imm5) */

    return NULL; 
}



/* ---------------- Addressing Mode 3 ---------------- */

static Bool sane_AMode3 ( ARMAMode3* am )
{
   switch (am->tag) {
   default:
       vpanic("sane_AMode3: unknown arm amode tag");
   }
}

static ARMAMode3* iselIntExpr_AMode3 ( ISelEnv* env, IRExpr* e )
{
    ARMAMode3* am = iselIntExpr_AMode3_wrk(env, e);
    vassert(sane_AMode3(am));
    return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARMAMode3* iselIntExpr_AMode3_wrk ( ISelEnv* env, IRExpr* e )
{   
    IRType ty = typeOfIRExpr(env->type_env,e);
    vassert(ty == Ity_I32);

    // ARMam3_RI,       /* Reg +/- Imm8 */
    // ARMam3_RR,       /* Reg +/- Reg */

    return NULL; 
}



/* ---------------- Branch Destination ---------------- */

static ARMBranchDest* iselIntExpr_BD ( ISelEnv* env, IRExpr* e )
{
    ARMBranchDest* bd = iselIntExpr_BD_wrk(env, e);
    /* sanity checks ... */
    switch (bd->tag) {
    case ARMbdImm:
	return bd;
    case ARMbdReg:
	vassert(hregClass(bd->ARMbd.Reg.reg) == HRcInt32);
//      vassert(hregIsVirtual(bd->ARMbd.Reg.reg));      // CAB ?
	return bd;
    default:
	vpanic("iselIntExpr_BD: unknown arm BD tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARMBranchDest* iselIntExpr_BD_wrk ( ISelEnv* env, IRExpr* e )
{
    /*
      ARMbdImm,
      ARMbdReg
    */

    return NULL;
}





/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static ARMCondCode iselCondCode ( ISelEnv* env, IRExpr* e )
{
    /* Uh, there's nothing we can sanity check here, unfortunately. */
    return iselCondCode_wrk(env, e);
}

/* DO NOT CALL THIS DIRECTLY ! */
static ARMCondCode iselCondCode_wrk ( ISelEnv* env, IRExpr* e )
{
#if 0
    MatchInfo mi;
    DECLARE_PATTERN(p_32to1);
    DECLARE_PATTERN(p_1Uto32_then_32to1);
#endif

    return 0;
}



static HReg iselIntExpr_R ( ISelEnv* env, IRExpr* e )
{
    return iselIntExpr_R_wrk(env, e);
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselIntExpr_R_wrk ( ISelEnv* env, IRExpr* e )
{
    return 0;
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
       HReg   reg;
       IRType tya = typeOfIRExpr(env->type_env, stmt->Ist.Store.addr);
       IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);
       IREndness end = stmt->Ist.Store.end;

       if (tya != Ity_I32 || end != Iend_LE) 
          goto stmt_fail;

       reg = iselIntExpr_R(env, stmt->Ist.Store.data);

       if (tyd == Ity_I8) {
	   ARMAMode2* am2 = iselIntExpr_AMode2(env, stmt->Ist.Store.addr);
	   addInstr(env, ARMInstr_StoreB(reg,am2));
	   return;
       }
       if (tyd == Ity_I16) {
	   ARMAMode3* am3 = iselIntExpr_AMode3(env, stmt->Ist.Store.addr);
	   addInstr(env, ARMInstr_StoreH(reg,am3));
	   return;
       }
       if (tyd == Ity_I32) {
	   ARMAMode2* am2 = iselIntExpr_AMode2(env, stmt->Ist.Store.addr);
	   addInstr(env, ARMInstr_StoreW(reg,am2));
	   return;
       }       
   }

   /* --------- PUT --------- */
   /* write guest state, fixed offset */
   case Ist_Put: {
       IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
       HReg reg = iselIntExpr_R(env, stmt->Ist.Put.data);

       // CAB: This anywhere near right?!
       if (tyd == Ity_I32) {
	   ARMAMode2* am2 = ARMAMode2_RI(GET_BP_REG(), stmt->Ist.Put.offset);
	   addInstr(env, ARMInstr_StoreW(reg, am2));
	   return;
       }
       if (tyd == Ity_I16) {
	   ARMAMode3* am3 = ARMAMode3_RI(GET_BP_REG(), stmt->Ist.Put.offset);
	   addInstr(env, ARMInstr_StoreH(reg, am3));
	   return;
       }
       if (tyd == Ity_I8) {
	   ARMAMode2* am2 = ARMAMode2_RI(GET_BP_REG(), stmt->Ist.Put.offset);
	   addInstr(env, ARMInstr_StoreB(reg, am2));
	   return;
       }
// CAB: Ity_I32, Ity_I16 ?
       break;
   }

   /* --------- Indexed PUT --------- */
   /* write guest state, run-time offset */
   case Ist_PutI: {
      ARMAMode2* am2
	   = genGuestArrayOffset(
	       env, stmt->Ist.PutI.descr, 
	       stmt->Ist.PutI.ix, stmt->Ist.PutI.bias );
       
       IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.PutI.data);
       
       if (tyd == Ity_I8) {
	   HReg reg = iselIntExpr_R(env, stmt->Ist.PutI.data);
	   addInstr(env, ARMInstr_StoreB(reg, am2));
	   return;
       }
// CAB: Ity_I32, Ity_I16 ?
       break;
   }

   /* --------- TMP --------- */
   /* assign value to temporary */
   case Ist_WrTmp: {
      IRTemp tmp = stmt->Ist.WrTmp.tmp;
      IRType ty = typeOfIRTemp(env->type_env, tmp);

      if (ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
         ARMAMode1* am = iselIntExpr_AMode1(env, stmt->Ist.WrTmp.data);
         HReg dst = lookupIRTemp(env, tmp);
         addInstr(env, ARMInstr_DPInstr1(ARMalu_MOV,dst,am));
         return;
      }

// CAB: Ity_I1 ?

      break;
   }

   /* --------- Call to DIRTY helper --------- */
   /* call complex ("dirty") helper function */
   case Ist_Dirty: {
     //IRType   retty;
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
      
      //retty = typeOfIRTemp(env->type_env, d->tmp);

// CAB: ?     if (retty == Ity_I64) {

#if 0
      if (retty == Ity_I32 || retty == Ity_I16 || retty == Ity_I8) {
         /* The returned value is in %eax.  Park it in the register
            associated with tmp. */
         HReg dst = lookupIRTemp(env, d->tmp);
         addInstr(env, mk_iMOVsd_RR(hregX86_EAX(),dst) );
         return;
      }
#endif
      break;
   }

   /* --------- EXIT --------- */
   /* conditional exit from BB */
   case Ist_Exit: {
      ARMBranchDest* dst;
      ARMCondCode cc;
      if (stmt->Ist.Exit.dst->tag != Ico_U32)
         vpanic("isel_arm: Ist_Exit: dst is not a 32-bit value");

      // CAB: Where does jumpkind fit in ?
      // stmt->Ist.Exit.jk

      dst = iselIntExpr_BD(env, IRExpr_Const(stmt->Ist.Exit.dst));
      cc  = iselCondCode(env,stmt->Ist.Exit.guard);
      addInstr(env, ARMInstr_Branch(cc, dst));
      return;
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

static void iselNext ( ISelEnv* env, IRExpr* next, IRJumpKind jk )
{
    ARMBranchDest* bd;
    if (vex_traceflags & VEX_TRACE_VCODE) {
	vex_printf("\n-- goto {");
	ppIRJumpKind(jk);
	vex_printf("} ");
	ppIRExpr(next);
	vex_printf("\n");
    }
    bd = iselIntExpr_BD(env, next);
    
    // CAB: jk ?

    addInstr( env, ARMInstr_Branch(ARMccAL, bd) );
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire SB to arm code. */

HInstrArray* iselSB_ARM ( IRSB* bb )
{
    Int     i, j;

    /* Make up an initial environment to use. */
    ISelEnv* env = LibVEX_Alloc(sizeof(ISelEnv));
    env->vreg_ctr = 0;

    /* Set up output code array. */
    env->code = newHInstrArray();
    
    /* Copy BB's type env. */
    env->type_env = bb->tyenv;

    /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
       change as we go along. */
    env->n_vregmap = bb->tyenv->types_used;
    env->vregmap   = LibVEX_Alloc(env->n_vregmap * sizeof(HReg));

    /* For each IR temporary, allocate a 32bit virtual register. */
    j = 0;
    for (i = 0; i < env->n_vregmap; i++) {
	env->vregmap[i] = mkHReg(j++, HRcInt32, True);
    }
    env->vreg_ctr = j;

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
/*--- end                                     host-x86/isel.c ---*/
/*---------------------------------------------------------------*/
