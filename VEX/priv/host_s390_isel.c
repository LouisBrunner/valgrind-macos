/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                  host_s390_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2013
   Copyright (C) 2012-2013  Florian Krohm   (britzel@acm.org)

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

/* Contributed by Florian Krohm */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_s390x_common.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_s390_defs.h"   /* S390X_GUEST_OFFSET */
#include "host_generic_regs.h"
#include "host_s390_defs.h"

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

    - Values of certain guest registers which are often assigned constants.
*/

/* Symbolic names for guest registers whose value we're tracking */
enum {
   GUEST_IA,
   GUEST_CC_OP,
   GUEST_CC_DEP1,
   GUEST_CC_DEP2,
   GUEST_CC_NDEP,
   GUEST_SYSNO,
   GUEST_COUNTER,
   GUEST_UNKNOWN    /* must be the last entry */
};

/* Number of registers we're tracking. */
#define NUM_TRACKED_REGS GUEST_UNKNOWN


typedef struct {
   IRTypeEnv   *type_env;

   HInstrArray *code;
   HReg        *vregmap;
   HReg        *vregmapHI;
   UInt         n_vregmap;
   UInt         vreg_ctr;
   UInt         hwcaps;

   IRExpr      *previous_bfp_rounding_mode;
   IRExpr      *previous_dfp_rounding_mode;

   ULong        old_value[NUM_TRACKED_REGS];

   /* The next two are for translation chaining */
   Addr64       max_ga;
   Bool         chaining_allowed;

   Bool         old_value_valid[NUM_TRACKED_REGS];
} ISelEnv;


/* Forward declarations */
static HReg          s390_isel_int_expr(ISelEnv *, IRExpr *);
static s390_amode   *s390_isel_amode(ISelEnv *, IRExpr *);
static s390_cc_t     s390_isel_cc(ISelEnv *, IRExpr *);
static s390_opnd_RMI s390_isel_int_expr_RMI(ISelEnv *, IRExpr *);
static void          s390_isel_int128_expr(HReg *, HReg *, ISelEnv *, IRExpr *);
static HReg          s390_isel_float_expr(ISelEnv *, IRExpr *);
static void          s390_isel_float128_expr(HReg *, HReg *, ISelEnv *, IRExpr *);
static HReg          s390_isel_dfp_expr(ISelEnv *, IRExpr *);
static void          s390_isel_dfp128_expr(HReg *, HReg *, ISelEnv *, IRExpr *);


static Int
get_guest_reg(Int offset)
{
   switch (offset) {
   case S390X_GUEST_OFFSET(guest_IA):        return GUEST_IA;
   case S390X_GUEST_OFFSET(guest_CC_OP):     return GUEST_CC_OP;
   case S390X_GUEST_OFFSET(guest_CC_DEP1):   return GUEST_CC_DEP1;
   case S390X_GUEST_OFFSET(guest_CC_DEP2):   return GUEST_CC_DEP2;
   case S390X_GUEST_OFFSET(guest_CC_NDEP):   return GUEST_CC_NDEP;
   case S390X_GUEST_OFFSET(guest_SYSNO):     return GUEST_SYSNO;
   case S390X_GUEST_OFFSET(guest_counter):   return GUEST_COUNTER;

      /* Also make sure there is never a partial write to one of
         these registers. That would complicate matters. */
   case S390X_GUEST_OFFSET(guest_IA)+1      ... S390X_GUEST_OFFSET(guest_IA)+7:
   case S390X_GUEST_OFFSET(guest_CC_OP)+1   ... S390X_GUEST_OFFSET(guest_CC_OP)+7:
   case S390X_GUEST_OFFSET(guest_CC_DEP1)+1 ... S390X_GUEST_OFFSET(guest_CC_DEP1)+7:
   case S390X_GUEST_OFFSET(guest_CC_DEP2)+1 ... S390X_GUEST_OFFSET(guest_CC_DEP2)+7:
   case S390X_GUEST_OFFSET(guest_CC_NDEP)+1 ... S390X_GUEST_OFFSET(guest_CC_NDEP)+7:
   case S390X_GUEST_OFFSET(guest_SYSNO)+1   ... S390X_GUEST_OFFSET(guest_SYSNO)+7:
      /* counter is used both as 4-byte and as 8-byte entity */
   case S390X_GUEST_OFFSET(guest_counter)+1 ... S390X_GUEST_OFFSET(guest_counter)+3:
   case S390X_GUEST_OFFSET(guest_counter)+5 ... S390X_GUEST_OFFSET(guest_counter)+7:
      vpanic("partial update of this guest state register is not allowed");
      break;

   default: break;
   }

   return GUEST_UNKNOWN;
}

/* Add an instruction */
static void
addInstr(ISelEnv *env, s390_insn *insn)
{
   addHInstr(env->code, insn);

   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("%s\n", s390_insn_as_string(insn));
   }
}


static __inline__ IRExpr *
mkU64(ULong value)
{
   return IRExpr_Const(IRConst_U64(value));
}


/*---------------------------------------------------------*/
/*--- Registers                                         ---*/
/*---------------------------------------------------------*/

/* Return the virtual register to which a given IRTemp is mapped. */
static HReg
lookupIRTemp(ISelEnv *env, IRTemp tmp)
{
   vassert(tmp < env->n_vregmap);
   vassert(! hregIsInvalid(env->vregmap[tmp]));

   return env->vregmap[tmp];
}


/* Return the two virtual registers to which the IRTemp is mapped. */
static void
lookupIRTemp128(HReg *hi, HReg *lo, ISelEnv *env, IRTemp tmp)
{
   vassert(tmp < env->n_vregmap);
   vassert(! hregIsInvalid(env->vregmapHI[tmp]));

   *lo = env->vregmap[tmp];
   *hi = env->vregmapHI[tmp];
}


/* Allocate a new integer register */
static HReg
newVRegI(ISelEnv *env)
{
   HReg reg = mkHReg(env->vreg_ctr, HRcInt64, True /* virtual */ );
   env->vreg_ctr++;

   return reg;
}


/* Allocate a new floating point register */
static HReg
newVRegF(ISelEnv *env)
{
   HReg reg = mkHReg(env->vreg_ctr, HRcFlt64, True /* virtual */ );

   env->vreg_ctr++;

   return reg;
}


/* Construct a non-virtual general purpose register */
static __inline__ HReg
make_gpr(UInt regno)
{
   return mkHReg(regno, HRcInt64, False /* virtual */ );
}


/* Construct a non-virtual floating point register */
static __inline__ HReg
make_fpr(UInt regno)
{
   return mkHReg(regno, HRcFlt64, False /* virtual */ );
}


/*---------------------------------------------------------*/
/*--- Amode                                             ---*/
/*---------------------------------------------------------*/

static __inline__ Bool
ulong_fits_unsigned_12bit(ULong val)
{
   return (val & 0xFFFu) == val;
}


static __inline__ Bool
ulong_fits_signed_20bit(ULong val)
{
   Long v = val & 0xFFFFFu;

   v = (v << 44) >> 44;  /* sign extend */

   return val == (ULong)v;
}


static __inline__ Bool
ulong_fits_signed_8bit(ULong val)
{
   Long v = val & 0xFFu;

   v = (v << 56) >> 56;  /* sign extend */

   return val == (ULong)v;
}

/* EXPR is an expression that is used as an address. Return an s390_amode
   for it. */
static s390_amode *
s390_isel_amode_wrk(ISelEnv *env, IRExpr *expr)
{
   if (expr->tag == Iex_Binop && expr->Iex.Binop.op == Iop_Add64) {
      IRExpr *arg1 = expr->Iex.Binop.arg1;
      IRExpr *arg2 = expr->Iex.Binop.arg2;

      /* Move constant into right subtree */
      if (arg1->tag == Iex_Const) {
         IRExpr *tmp;
         tmp  = arg1;
         arg1 = arg2;
         arg2 = tmp;
      }

      /* r + constant: Check for b12 first, then b20 */
      if (arg2->tag == Iex_Const && arg2->Iex.Const.con->tag == Ico_U64) {
         ULong value = arg2->Iex.Const.con->Ico.U64;

         if (ulong_fits_unsigned_12bit(value)) {
            return s390_amode_b12((Int)value, s390_isel_int_expr(env, arg1));
         }
         /* If long-displacement is not available, do not construct B20 or
            BX20 amodes because code generation cannot handle them. */
         if (s390_host_has_ldisp && ulong_fits_signed_20bit(value)) {
            return s390_amode_b20((Int)value, s390_isel_int_expr(env, arg1));
         }
      }
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   return s390_amode_b12(0, s390_isel_int_expr(env, expr));
}


static s390_amode *
s390_isel_amode(ISelEnv *env, IRExpr *expr)
{
   s390_amode *am;

   /* Address computation should yield a 64-bit value */
   vassert(typeOfIRExpr(env->type_env, expr) == Ity_I64);

   am = s390_isel_amode_wrk(env, expr);

   /* Check post-condition */
   vassert(s390_amode_is_sane(am));

   return am;
}


/*---------------------------------------------------------*/
/*--- Helper functions                                  ---*/
/*---------------------------------------------------------*/

/* Constants and memory accesses should be right operands */
#define order_commutative_operands(left, right)                   \
        do {                                                      \
          if (left->tag == Iex_Const || left->tag == Iex_Load ||  \
              left->tag == Iex_Get) {                             \
            IRExpr *tmp;                                          \
            tmp   = left;                                         \
            left  = right;                                        \
            right = tmp;                                          \
          }                                                       \
        } while (0)


/* Copy an RMI operand to the DST register */
static s390_insn *
s390_opnd_copy(UChar size, HReg dst, s390_opnd_RMI opnd)
{
   switch (opnd.tag) {
   case S390_OPND_AMODE:
      return s390_insn_load(size, dst, opnd.variant.am);

   case S390_OPND_REG:
      return s390_insn_move(size, dst, opnd.variant.reg);

   case S390_OPND_IMMEDIATE:
      return s390_insn_load_immediate(size, dst, opnd.variant.imm);

   default:
      vpanic("s390_opnd_copy");
   }
}


/* Construct a RMI operand for a register */
static __inline__ s390_opnd_RMI
s390_opnd_reg(HReg reg)
{
   s390_opnd_RMI opnd;

   opnd.tag  = S390_OPND_REG;
   opnd.variant.reg = reg;

   return opnd;
}


/* Construct a RMI operand for an immediate constant */
static __inline__ s390_opnd_RMI
s390_opnd_imm(ULong value)
{
   s390_opnd_RMI opnd;

   opnd.tag  = S390_OPND_IMMEDIATE;
   opnd.variant.imm = value;

   return opnd;
}


/* Return 1, if EXPR represents the constant 0 */
static Bool
s390_expr_is_const_zero(IRExpr *expr)
{
   ULong value;

   if (expr->tag == Iex_Const) {
      switch (expr->Iex.Const.con->tag) {
      case Ico_U1:  value = expr->Iex.Const.con->Ico.U1;  break;
      case Ico_U8:  value = expr->Iex.Const.con->Ico.U8;  break;
      case Ico_U16: value = expr->Iex.Const.con->Ico.U16; break;
      case Ico_U32: value = expr->Iex.Const.con->Ico.U32; break;
      case Ico_U64: value = expr->Iex.Const.con->Ico.U64; break;
      default:
         vpanic("s390_expr_is_const_zero");
      }
      return value == 0;
   }

   return 0;
}


/* Return the value of CON as a sign-exteded ULong value */
static ULong
get_const_value_as_ulong(const IRConst *con)
{
   Long value;

   switch (con->tag) {
   case Ico_U1:  value = con->Ico.U1;  return (ULong) ((value << 63) >> 63);
   case Ico_U8:  value = con->Ico.U8;  return (ULong) ((value << 56) >> 56);
   case Ico_U16: value = con->Ico.U16; return (ULong) ((value << 48) >> 48);
   case Ico_U32: value = con->Ico.U32; return (ULong) ((value << 32) >> 32);
   case Ico_U64: return con->Ico.U64;
   default:
      vpanic("get_const_value_as_ulong");
   }
}


/* Call a helper (clean or dirty)
   Arguments must satisfy the following conditions:

   (a) they are expressions yielding an integer result
   (b) there can be no more than S390_NUM_GPRPARMS arguments

   guard is a Ity_Bit expression indicating whether or not the
   call happens.  If guard == NULL, the call is unconditional.

   Calling the helper function proceeds as follows:

   (1) The helper arguments are evaluated and their value stored in
       virtual registers.
   (2) The condition code is evaluated
   (3) The argument values are copied from the virtual registers to the
       registers mandated by the ABI.
   (4) Call the helper function.

   This is not the most efficient way as step 3 generates register-to-register
   moves. But it is the least fragile way as the only hidden dependency here
   is that register-to-register moves (step 3) must not clobber the condition
   code. Other schemes (e.g. VEX r2326) that attempt to avoid the register-
   to-register add more such dependencies. Not good. Besides, it's the job
   of the register allocator to throw out those reg-to-reg moves.
*/
static void
doHelperCall(/*OUT*/UInt *stackAdjustAfterCall,
             /*OUT*/RetLoc *retloc,
             ISelEnv *env, IRExpr *guard,
             IRCallee *callee, IRType retTy, IRExpr **args)
{
   UInt n_args, i, argreg, size;
   ULong target;
   HReg tmpregs[S390_NUM_GPRPARMS];
   s390_cc_t cc;

   /* Set default returns.  We'll update them later if needed. */
   *stackAdjustAfterCall = 0;
   *retloc               = mk_RetLoc_INVALID();

   /* The return type can be I{64,32,16,8} or V{128,256}.  In the
      latter two cases, it is expected that |args| will contain the
      special node IRExpr_VECRET(), in which case this routine
      generates code to allocate space on the stack for the vector
      return value.  Since we are not passing any scalars on the
      stack, it is enough to preallocate the return space before
      marshalling any arguments, in this case.

      |args| may also contain IRExpr_BBPTR(), in which case the value
      in the guest state pointer register is passed as the
      corresponding argument.

      These are used for cross-checking that IR-level constraints on
      the use of IRExpr_VECRET() and IRExpr_BBPTR() are observed. */
   UInt nVECRETs = 0;
   UInt nBBPTRs  = 0;

   n_args = 0;
   for (i = 0; args[i]; i++)
      ++n_args;

   if (n_args > S390_NUM_GPRPARMS) {
      vpanic("doHelperCall: too many arguments");
   }

   /* All arguments must have Ity_I64. For two reasons:
      (1) We do not handle floating point arguments.
      (2) The ABI requires that integer values are sign- or zero-extended
           to 64 bit.
   */
   Int arg_errors = 0;
   for (i = 0; i < n_args; ++i) {
      if (UNLIKELY(args[i]->tag == Iex_VECRET)) {
         nVECRETs++;
      } else if (UNLIKELY(args[i]->tag == Iex_BBPTR)) {
         nBBPTRs++;
      } else {
         IRType type = typeOfIRExpr(env->type_env, args[i]);
         if (type != Ity_I64) {
            ++arg_errors;
            vex_printf("calling %s: argument #%d has type ", callee->name, i);
            ppIRType(type);
            vex_printf("; Ity_I64 is required\n");
         }
      }
   }

   if (arg_errors)
      vpanic("cannot continue due to errors in argument passing");

   /* If this fails, the IR is ill-formed */
   vassert(nBBPTRs == 0 || nBBPTRs == 1);

   /* If we have a VECRET, allocate space on the stack for the return
      value, and record the stack pointer after that. */
   HReg r_vecRetAddr = INVALID_HREG;
   if (nVECRETs == 1) {
      /* we do not handle vector types yet */
      vassert(0);
      HReg sp = make_gpr(S390_REGNO_STACK_POINTER);
      vassert(retTy == Ity_V128 || retTy == Ity_V256);
      vassert(retTy != Ity_V256); // we don't handle that yet (if ever)
      r_vecRetAddr = newVRegI(env);
      addInstr(env, s390_insn_alu(4, S390_ALU_SUB, sp, s390_opnd_imm(16)));
      addInstr(env, s390_insn_move(sizeof(ULong), r_vecRetAddr, sp));

   } else {
      // If either of these fail, the IR is ill-formed
      vassert(retTy != Ity_V128 && retTy != Ity_V256);
      vassert(nVECRETs == 0);
   }

   argreg = 0;

   /* Compute the function arguments into a temporary register each */
   for (i = 0; i < n_args; i++) {
      IRExpr *arg = args[i];
      if(UNLIKELY(arg->tag == Iex_VECRET)) {
         /* we do not handle vector types yet */
         vassert(0);
         addInstr(env, s390_insn_move(sizeof(ULong), tmpregs[argreg],
                                      r_vecRetAddr));
      } else if (UNLIKELY(arg->tag == Iex_BBPTR)) {
         /* If we need the guest state pointer put it in a temporary arg reg */
         tmpregs[argreg] = newVRegI(env);
         addInstr(env, s390_insn_move(sizeof(ULong), tmpregs[argreg],
                                      s390_hreg_guest_state_pointer()));
      } else {
         tmpregs[argreg] = s390_isel_int_expr(env, args[i]);
      }
      argreg++;
   }

   /* Compute the condition */
   cc = S390_CC_ALWAYS;
   if (guard) {
      if (guard->tag == Iex_Const
          && guard->Iex.Const.con->tag == Ico_U1
          && guard->Iex.Const.con->Ico.U1 == True) {
         /* unconditional -- do nothing */
      } else {
         cc = s390_isel_cc(env, guard);
      }
   }

   /* Move the args to the final register. It is paramount, that the
      code to move the registers does not clobber the condition code ! */
   for (i = 0; i < argreg; i++) {
      HReg finalreg;

      finalreg = make_gpr(s390_gprno_from_arg_index(i));
      size = sizeofIRType(Ity_I64);
      addInstr(env, s390_insn_move(size, finalreg, tmpregs[i]));
   }

   target = Ptr_to_ULong(callee->addr);

   /* Do final checks, set the return values, and generate the call
      instruction proper. */
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
      /* we do not handle vector types yet */
      vassert(0);
      *retloc = mk_RetLoc_spRel(RLPri_V128SpRel, 0);
      *stackAdjustAfterCall = 16;
      break;
   case Ity_V256:
      /* we do not handle vector types yet */
      vassert(0);
      *retloc = mk_RetLoc_spRel(RLPri_V256SpRel, 0);
      *stackAdjustAfterCall = 32;
      break;
   default:
      /* IR can denote other possible return types, but we don't
         handle those here. */
      vassert(0);
   }

   /* Finally, the call itself. */
   addInstr(env, s390_insn_helper_call(cc, (Addr64)target, n_args,
                                       callee->name, *retloc));
}


/*---------------------------------------------------------*/
/*--- BFP helper functions                              ---*/
/*---------------------------------------------------------*/

/* Set the BFP rounding mode in the FPC. This function is called for
   all non-conversion BFP instructions as those will always get the
   rounding mode from the FPC. */
static void 
set_bfp_rounding_mode_in_fpc(ISelEnv *env, IRExpr *irrm)
{
   vassert(typeOfIRExpr(env->type_env, irrm) == Ity_I32);

   /* Do we need to do anything? */
   if (env->previous_bfp_rounding_mode &&
       env->previous_bfp_rounding_mode->tag == Iex_RdTmp &&
       irrm->tag == Iex_RdTmp &&
       env->previous_bfp_rounding_mode->Iex.RdTmp.tmp == irrm->Iex.RdTmp.tmp) {
      /* No - new mode is identical to previous mode.  */
      return;
   }

   /* No luck - we better set it, and remember what we set it to. */
   env->previous_bfp_rounding_mode = irrm;

   /* The incoming rounding mode is in VEX IR encoding. Need to change
      to s390.

      rounding mode | s390 | IR
      -------------------------
      to nearest    |  00  | 00
      to zero       |  01  | 11
      to +infinity  |  10  | 10
      to -infinity  |  11  | 01

      So: s390 = (4 - IR) & 3
   */
   HReg ir = s390_isel_int_expr(env, irrm);

   HReg mode = newVRegI(env);

   addInstr(env, s390_insn_load_immediate(4, mode, 4));
   addInstr(env, s390_insn_alu(4, S390_ALU_SUB, mode, s390_opnd_reg(ir)));
   addInstr(env, s390_insn_alu(4, S390_ALU_AND, mode, s390_opnd_imm(3)));

   addInstr(env, s390_insn_set_fpc_bfprm(4, mode));
}


/* This function is invoked for insns that support a specification of
   a rounding mode in the insn itself. In that case there is no need to
   stick the rounding mode into the FPC -- a good thing. However, the
   rounding mode must be known. */
static s390_bfp_round_t
get_bfp_rounding_mode(ISelEnv *env, IRExpr *irrm)
{
   if (irrm->tag == Iex_Const) {          /* rounding mode is known */
      vassert(irrm->Iex.Const.con->tag == Ico_U32);
      IRRoundingMode mode = irrm->Iex.Const.con->Ico.U32;

      switch (mode) {
      case Irrm_NEAREST:  return S390_BFP_ROUND_NEAREST_EVEN;
      case Irrm_ZERO:     return S390_BFP_ROUND_ZERO;
      case Irrm_PosINF:   return S390_BFP_ROUND_POSINF;
      case Irrm_NegINF:   return S390_BFP_ROUND_NEGINF;
      default:
         vpanic("get_bfp_rounding_mode");
      }
   }

   set_bfp_rounding_mode_in_fpc(env, irrm);
   return S390_BFP_ROUND_PER_FPC;
}


/*---------------------------------------------------------*/
/*--- DFP helper functions                              ---*/
/*---------------------------------------------------------*/

/* Set the DFP rounding mode in the FPC. This function is called for
   all non-conversion DFP instructions as those will always get the
   rounding mode from the FPC. */
static void
set_dfp_rounding_mode_in_fpc(ISelEnv *env, IRExpr *irrm)
{
   vassert(typeOfIRExpr(env->type_env, irrm) == Ity_I32);

   /* Do we need to do anything? */
   if (env->previous_dfp_rounding_mode &&
       env->previous_dfp_rounding_mode->tag == Iex_RdTmp &&
       irrm->tag == Iex_RdTmp &&
       env->previous_dfp_rounding_mode->Iex.RdTmp.tmp == irrm->Iex.RdTmp.tmp) {
      /* No - new mode is identical to previous mode.  */
      return;
   }

   /* No luck - we better set it, and remember what we set it to. */
   env->previous_dfp_rounding_mode = irrm;

   /* The incoming rounding mode is in VEX IR encoding. Need to change
      to s390.

      rounding mode                     | S390 |  IR
      -----------------------------------------------
      to nearest, ties to even          | 000  | 000
      to zero                           | 001  | 011
      to +infinity                      | 010  | 010
      to -infinity                      | 011  | 001
      to nearest, ties away from 0      | 100  | 100
      to nearest, ties toward 0         | 101  | 111
      to away from 0                    | 110  | 110
      to prepare for shorter precision  | 111  | 101

      So: s390 = (IR ^ ((IR << 1) & 2))
   */
   HReg ir = s390_isel_int_expr(env, irrm);

   HReg mode = newVRegI(env);

   addInstr(env, s390_insn_move(4, mode, ir));
   addInstr(env, s390_insn_alu(4, S390_ALU_LSH, mode, s390_opnd_imm(1)));
   addInstr(env, s390_insn_alu(4, S390_ALU_AND, mode, s390_opnd_imm(2)));
   addInstr(env, s390_insn_alu(4, S390_ALU_XOR, mode, s390_opnd_reg(ir)));

   addInstr(env, s390_insn_set_fpc_dfprm(4, mode));
}


/* This function is invoked for insns that support a specification of
   a rounding mode in the insn itself. In that case there is no need to
   stick the rounding mode into the FPC -- a good thing. However, the
   rounding mode must be known.

   When mapping an Irrm_XYZ value to an S390_DFP_ROUND_ value there is
   often a choice. For instance, Irrm_ZERO could be mapped to either
   S390_DFP_ROUND_ZERO_5 or S390_DFP_ROUND_ZERO_9. The difference between
   those two is that with S390_DFP_ROUND_ZERO_9 the recognition of the
   quantum exception is suppressed whereas with S390_DFP_ROUND_ZERO_5 it
   is not.  As the quantum exception is not modelled we can choose either
   value. The choice is to use S390_DFP_ROUND_.. values in the range [8:15],
   because values in the range [1:7] have unpredictable rounding behaviour
   when the floating point exception facility is not installed.

   Translation table of
   s390 DFP rounding mode to IRRoundingMode to s390 DFP rounding mode

   s390(S390_DFP_ROUND_)  |  IR(Irrm_)           |  s390(S390_DFP_ROUND_)
   --------------------------------------------------------------------
   NEAREST_TIE_AWAY_0_1   |  NEAREST_TIE_AWAY_0  |  NEAREST_TIE_AWAY_0_12
   NEAREST_TIE_AWAY_0_12  |     "                |     "
   PREPARE_SHORT_3        |  PREPARE_SHORTER     |  PREPARE_SHORT_15
   PREPARE_SHORT_15       |     "                |     "
   NEAREST_EVEN_4         |  NEAREST             |  NEAREST_EVEN_8
   NEAREST_EVEN_8         |     "                |     "
   ZERO_5                 |  ZERO                |  ZERO_9
   ZERO_9                 |     "                |     "
   POSINF_6               |  PosINF              |  POSINF_10
   POSINF_10              |     "                |     "
   NEGINF_7               |  NegINF              |  NEGINF_11
   NEGINF_11              |     "                |     "
   NEAREST_TIE_TOWARD_0   |  NEAREST_TIE_TOWARD_0|  NEAREST_TIE_TOWARD_0
   AWAY_0                 |  AWAY_FROM_ZERO      |  AWAY_0
*/
static s390_dfp_round_t
get_dfp_rounding_mode(ISelEnv *env, IRExpr *irrm)
{
   if (irrm->tag == Iex_Const) {          /* rounding mode is known */
      vassert(irrm->Iex.Const.con->tag == Ico_U32);
      IRRoundingMode mode = irrm->Iex.Const.con->Ico.U32;

      switch (mode) {
      case Irrm_NEAREST:
         return S390_DFP_ROUND_NEAREST_EVEN_8;
      case Irrm_NegINF:
         return S390_DFP_ROUND_NEGINF_11;
      case Irrm_PosINF:
         return S390_DFP_ROUND_POSINF_10;
      case Irrm_ZERO:
         return S390_DFP_ROUND_ZERO_9;
      case Irrm_NEAREST_TIE_AWAY_0:
         return S390_DFP_ROUND_NEAREST_TIE_AWAY_0_12;
      case Irrm_PREPARE_SHORTER:
          return S390_DFP_ROUND_PREPARE_SHORT_15;
      case Irrm_AWAY_FROM_ZERO:
         return S390_DFP_ROUND_AWAY_0;
      case Irrm_NEAREST_TIE_TOWARD_0:
         return S390_DFP_ROUND_NEAREST_TIE_TOWARD_0;
      default:
         vpanic("get_dfp_rounding_mode");
      }
   }

   set_dfp_rounding_mode_in_fpc(env, irrm);
   return S390_DFP_ROUND_PER_FPC_0;
}


/*---------------------------------------------------------*/
/*--- Condition code helper functions                   ---*/
/*---------------------------------------------------------*/

/* CC_S390 holds the condition code in s390 encoding. Convert it to
   VEX encoding (IRCmpFResult)

   s390     VEX              b6 b2 b0   cc.1  cc.0
   0      0x40 EQ             1  0  0     0     0
   1      0x01 LT             0  0  1     0     1
   2      0x00 GT             0  0  0     1     0
   3      0x45 Unordered      1  1  1     1     1

   b0 = cc.0
   b2 = cc.0 & cc.1
   b6 = ~(cc.0 ^ cc.1)   // ((cc.0 - cc.1) + 0x1 ) & 0x1

   VEX = b0 | (b2 << 2) | (b6 << 6);
*/
static HReg
convert_s390_to_vex_bfpcc(ISelEnv *env, HReg cc_s390)
{
   HReg cc0, cc1, b2, b6, cc_vex;

   cc0 = newVRegI(env);
   addInstr(env, s390_insn_move(4, cc0, cc_s390));
   addInstr(env, s390_insn_alu(4, S390_ALU_AND, cc0, s390_opnd_imm(1)));

   cc1 = newVRegI(env);
   addInstr(env, s390_insn_move(4, cc1, cc_s390));
   addInstr(env, s390_insn_alu(4, S390_ALU_RSH, cc1, s390_opnd_imm(1)));

   b2 = newVRegI(env);
   addInstr(env, s390_insn_move(4, b2, cc0));
   addInstr(env, s390_insn_alu(4, S390_ALU_AND, b2, s390_opnd_reg(cc1)));
   addInstr(env, s390_insn_alu(4, S390_ALU_LSH, b2, s390_opnd_imm(2)));

   b6 = newVRegI(env);
   addInstr(env, s390_insn_move(4, b6, cc0));
   addInstr(env, s390_insn_alu(4, S390_ALU_SUB, b6, s390_opnd_reg(cc1)));
   addInstr(env, s390_insn_alu(4, S390_ALU_ADD, b6, s390_opnd_imm(1)));
   addInstr(env, s390_insn_alu(4, S390_ALU_AND, b6, s390_opnd_imm(1)));
   addInstr(env, s390_insn_alu(4, S390_ALU_LSH, b6, s390_opnd_imm(6)));

   cc_vex = newVRegI(env);
   addInstr(env, s390_insn_move(4, cc_vex, cc0));
   addInstr(env, s390_insn_alu(4, S390_ALU_OR, cc_vex, s390_opnd_reg(b2)));
   addInstr(env, s390_insn_alu(4, S390_ALU_OR, cc_vex, s390_opnd_reg(b6)));

   return cc_vex;
}

/* CC_S390 holds the condition code in s390 encoding. Convert it to
   VEX encoding (IRCmpDResult) */
static HReg
convert_s390_to_vex_dfpcc(ISelEnv *env, HReg cc_s390)
{
   /* The encodings for IRCmpFResult and IRCmpDResult are the same/ */
   return convert_s390_to_vex_bfpcc(env, cc_s390);
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (128 bit)               ---*/
/*---------------------------------------------------------*/
static void
s390_isel_int128_expr_wrk(HReg *dst_hi, HReg *dst_lo, ISelEnv *env,
                          IRExpr *expr)
{
   IRType ty = typeOfIRExpr(env->type_env, expr);

   vassert(ty == Ity_I128);

   /* No need to consider the following
      - 128-bit constants (they do not exist in VEX)
      - 128-bit loads from memory (will not be generated)
   */

   /* Read 128-bit IRTemp */
   if (expr->tag == Iex_RdTmp) {
      lookupIRTemp128(dst_hi, dst_lo, env, expr->Iex.RdTmp.tmp);
      return;
   }

   if (expr->tag == Iex_Binop) {
      IRExpr *arg1 = expr->Iex.Binop.arg1;
      IRExpr *arg2 = expr->Iex.Binop.arg2;
      Bool is_signed_multiply, is_signed_divide;

      switch (expr->Iex.Binop.op) {
      case Iop_MullU64:
         is_signed_multiply = False;
         goto do_multiply64;

      case Iop_MullS64:
         is_signed_multiply = True;
         goto do_multiply64;

      case Iop_DivModU128to64:
         is_signed_divide = False;
         goto do_divide64;

      case Iop_DivModS128to64:
         is_signed_divide = True;
         goto do_divide64;

      case Iop_64HLto128:
         *dst_hi = s390_isel_int_expr(env, arg1);
         *dst_lo = s390_isel_int_expr(env, arg2);
         return;

      case Iop_DivModS64to64: {
         HReg r10, r11, h1;
         s390_opnd_RMI op2;

         h1  = s390_isel_int_expr(env, arg1);       /* Process 1st operand */
         op2 = s390_isel_int_expr_RMI(env, arg2);   /* Process 2nd operand */

         /* We use non-virtual registers r10 and r11 as pair */
         r10  = make_gpr(10);
         r11  = make_gpr(11);

         /* Move 1st operand into r11 and */
         addInstr(env, s390_insn_move(8, r11, h1));

         /* Divide */
         addInstr(env, s390_insn_divs(8, r10, r11, op2));

         /* The result is in registers r10 (remainder) and r11 (quotient).
            Move the result into the reg pair that is being returned such
            such that the low 64 bits are the quotient and the upper 64 bits
            are the remainder. (see libvex_ir.h). */
         *dst_hi = newVRegI(env);
         *dst_lo = newVRegI(env);
         addInstr(env, s390_insn_move(8, *dst_hi, r10));
         addInstr(env, s390_insn_move(8, *dst_lo, r11));
         return;
      }

      default:
         break;

      do_multiply64: {
            HReg r10, r11, h1;
            s390_opnd_RMI op2;

            order_commutative_operands(arg1, arg2);

            h1   = s390_isel_int_expr(env, arg1);       /* Process 1st operand */
            op2  = s390_isel_int_expr_RMI(env, arg2);   /* Process 2nd operand */

            /* We use non-virtual registers r10 and r11 as pair */
            r10  = make_gpr(10);
            r11  = make_gpr(11);

            /* Move the first operand to r11 */
            addInstr(env, s390_insn_move(8, r11, h1));

            /* Multiply */
            addInstr(env, s390_insn_mul(8, r10, r11, op2, is_signed_multiply));

            /* The result is in registers r10 and r11. Assign to two virtual regs
               and return. */
            *dst_hi = newVRegI(env);
            *dst_lo = newVRegI(env);
            addInstr(env, s390_insn_move(8, *dst_hi, r10));
            addInstr(env, s390_insn_move(8, *dst_lo, r11));
            return;
         }

      do_divide64: {
         HReg r10, r11, hi, lo;
         s390_opnd_RMI op2;

         s390_isel_int128_expr(&hi, &lo, env, arg1);
         op2  = s390_isel_int_expr_RMI(env, arg2);   /* Process 2nd operand */

         /* We use non-virtual registers r10 and r11 as pair */
         r10  = make_gpr(10);
         r11  = make_gpr(11);

         /* Move high 64 bits of the 1st operand into r10 and
            the low 64 bits into r11. */
         addInstr(env, s390_insn_move(8, r10, hi));
         addInstr(env, s390_insn_move(8, r11, lo));

         /* Divide */
         addInstr(env, s390_insn_div(8, r10, r11, op2, is_signed_divide));

         /* The result is in registers r10 (remainder) and r11 (quotient).
            Move the result into the reg pair that is being returned such
            such that the low 64 bits are the quotient and the upper 64 bits
            are the remainder. (see libvex_ir.h). */
         *dst_hi = newVRegI(env);
         *dst_lo = newVRegI(env);
         addInstr(env, s390_insn_move(8, *dst_hi, r10));
         addInstr(env, s390_insn_move(8, *dst_lo, r11));
         return;
      }
      }
   }

   vpanic("s390_isel_int128_expr");
}


/* Compute a 128-bit value into two 64-bit registers. These may be either
   real or virtual regs; in any case they must not be changed by subsequent
   code emitted by the caller. */
static void
s390_isel_int128_expr(HReg *dst_hi, HReg *dst_lo, ISelEnv *env, IRExpr *expr)
{
   s390_isel_int128_expr_wrk(dst_hi, dst_lo, env, expr);

   /* Sanity checks ... */
   vassert(hregIsVirtual(*dst_hi));
   vassert(hregIsVirtual(*dst_lo));
   vassert(hregClass(*dst_hi) == HRcInt64);
   vassert(hregClass(*dst_lo) == HRcInt64);
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
   All results are returned in a 64bit register.
   For 16- and 8-bit expressions, the upper (32/48/56 : 16/24) bits
   are arbitrary, so you should mask or sign extend partial values
   if necessary.
*/

/* DO NOT CALL THIS DIRECTLY ! */
static HReg
s390_isel_int_expr_wrk(ISelEnv *env, IRExpr *expr)
{
   IRType ty = typeOfIRExpr(env->type_env, expr);
   UChar size;
   s390_bfp_conv_t conv;
   s390_dfp_conv_t dconv;

   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I64);

   size = sizeofIRType(ty);   /* size of the result after evaluating EXPR */

   switch (expr->tag) {

      /* --------- TEMP --------- */
   case Iex_RdTmp:
      /* Return the virtual register that holds the temporary. */
      return lookupIRTemp(env, expr->Iex.RdTmp.tmp);

      /* --------- LOAD --------- */
   case Iex_Load: {
      HReg        dst = newVRegI(env);
      s390_amode *am  = s390_isel_amode(env, expr->Iex.Load.addr);

      if (expr->Iex.Load.end != Iend_BE)
         goto irreducible;

      addInstr(env, s390_insn_load(size, dst, am));

      return dst;
   }

      /* --------- BINARY OP --------- */
   case Iex_Binop: {
      IRExpr *arg1 = expr->Iex.Binop.arg1;
      IRExpr *arg2 = expr->Iex.Binop.arg2;
      HReg h1, res;
      s390_alu_t opkind;
      s390_opnd_RMI op2, value, opnd;
      s390_insn *insn;
      Bool is_commutative, is_signed_multiply, is_signed_divide;

      is_commutative = True;

      switch (expr->Iex.Binop.op) {
      case Iop_MullU8:
      case Iop_MullU16:
      case Iop_MullU32:
         is_signed_multiply = False;
         goto do_multiply;

      case Iop_MullS8:
      case Iop_MullS16:
      case Iop_MullS32:
         is_signed_multiply = True;
         goto do_multiply;

      do_multiply: {
            HReg r10, r11;
            UInt arg_size = size / 2;

            order_commutative_operands(arg1, arg2);

            h1   = s390_isel_int_expr(env, arg1);       /* Process 1st operand */
            op2  = s390_isel_int_expr_RMI(env, arg2);   /* Process 2nd operand */

            /* We use non-virtual registers r10 and r11 as pair */
            r10  = make_gpr(10);
            r11  = make_gpr(11);

            /* Move the first operand to r11 */
            addInstr(env, s390_insn_move(arg_size, r11, h1));

            /* Multiply */
            addInstr(env, s390_insn_mul(arg_size, r10, r11, op2, is_signed_multiply));

            /* The result is in registers r10 and r11. Combine them into a SIZE-bit
               value into the destination register. */
            res  = newVRegI(env);
            addInstr(env, s390_insn_move(arg_size, res, r10));
            value = s390_opnd_imm(arg_size * 8);
            addInstr(env, s390_insn_alu(size, S390_ALU_LSH, res, value));
            value = s390_opnd_imm((((ULong)1) << arg_size * 8) - 1);
            addInstr(env, s390_insn_alu(size, S390_ALU_AND, r11, value));
            opnd = s390_opnd_reg(r11);
            addInstr(env, s390_insn_alu(size, S390_ALU_OR,  res, opnd));
            return res;
         }

      case Iop_DivModS64to32:
         is_signed_divide = True;
         goto do_divide;

      case Iop_DivModU64to32:
         is_signed_divide = False;
         goto do_divide;

      do_divide: {
            HReg r10, r11;

            h1   = s390_isel_int_expr(env, arg1);       /* Process 1st operand */
            op2  = s390_isel_int_expr_RMI(env, arg2);   /* Process 2nd operand */

            /* We use non-virtual registers r10 and r11 as pair */
            r10  = make_gpr(10);
            r11  = make_gpr(11);

            /* Split the first operand and put the high 32 bits into r10 and
               the low 32 bits into r11. */
            addInstr(env, s390_insn_move(8, r10, h1));
            addInstr(env, s390_insn_move(8, r11, h1));
            value = s390_opnd_imm(32);
            addInstr(env, s390_insn_alu(8, S390_ALU_RSH, r10, value));

            /* Divide */
            addInstr(env, s390_insn_div(4, r10, r11, op2, is_signed_divide));

            /* The result is in registers r10 (remainder) and r11 (quotient).
               Combine them into a 64-bit value such that the low 32 bits are
               the quotient and the upper 32 bits are the remainder. (see
               libvex_ir.h). */
            res  = newVRegI(env);
            addInstr(env, s390_insn_move(8, res, r10));
            value = s390_opnd_imm(32);
            addInstr(env, s390_insn_alu(8, S390_ALU_LSH, res, value));
            value = s390_opnd_imm((((ULong)1) << 32) - 1);
            addInstr(env, s390_insn_alu(8, S390_ALU_AND, r11, value));
            opnd = s390_opnd_reg(r11);
            addInstr(env, s390_insn_alu(8, S390_ALU_OR,  res, opnd));
            return res;
         }

      case Iop_F32toI32S:  conv = S390_BFP_F32_TO_I32;  goto do_convert;
      case Iop_F32toI64S:  conv = S390_BFP_F32_TO_I64;  goto do_convert;
      case Iop_F32toI32U:  conv = S390_BFP_F32_TO_U32;  goto do_convert;
      case Iop_F32toI64U:  conv = S390_BFP_F32_TO_U64;  goto do_convert;
      case Iop_F64toI32S:  conv = S390_BFP_F64_TO_I32;  goto do_convert;
      case Iop_F64toI64S:  conv = S390_BFP_F64_TO_I64;  goto do_convert;
      case Iop_F64toI32U:  conv = S390_BFP_F64_TO_U32;  goto do_convert;
      case Iop_F64toI64U:  conv = S390_BFP_F64_TO_U64;  goto do_convert;
      case Iop_F128toI32S: conv = S390_BFP_F128_TO_I32; goto do_convert_128;
      case Iop_F128toI64S: conv = S390_BFP_F128_TO_I64; goto do_convert_128;
      case Iop_F128toI32U: conv = S390_BFP_F128_TO_U32; goto do_convert_128;
      case Iop_F128toI64U: conv = S390_BFP_F128_TO_U64; goto do_convert_128;

      case Iop_D64toI32S:  dconv = S390_DFP_D64_TO_I32;  goto do_convert_dfp;
      case Iop_D64toI64S:  dconv = S390_DFP_D64_TO_I64;  goto do_convert_dfp;
      case Iop_D64toI32U:  dconv = S390_DFP_D64_TO_U32;  goto do_convert_dfp;
      case Iop_D64toI64U:  dconv = S390_DFP_D64_TO_U64;  goto do_convert_dfp;
      case Iop_D128toI32S: dconv = S390_DFP_D128_TO_I32; goto do_convert_dfp128;
      case Iop_D128toI64S: dconv = S390_DFP_D128_TO_I64; goto do_convert_dfp128;
      case Iop_D128toI32U: dconv = S390_DFP_D128_TO_U32; goto do_convert_dfp128;
      case Iop_D128toI64U: dconv = S390_DFP_D128_TO_U64; goto do_convert_dfp128;

      do_convert: {
         s390_bfp_round_t rounding_mode;

         res  = newVRegI(env);
         h1   = s390_isel_float_expr(env, arg2);   /* Process operand */

         rounding_mode = get_bfp_rounding_mode(env, arg1);
         addInstr(env, s390_insn_bfp_convert(size, conv, res, h1,
                                             rounding_mode));
         return res;
      }

      do_convert_128: {
         s390_bfp_round_t rounding_mode;
         HReg op_hi, op_lo, f13, f15;

         res = newVRegI(env);
         s390_isel_float128_expr(&op_hi, &op_lo, env, arg2); /* operand */

         /* We use non-virtual registers r13 and r15 as pair */
         f13 = make_fpr(13);
         f15 = make_fpr(15);

         /* operand --> (f13, f15) */
         addInstr(env, s390_insn_move(8, f13, op_hi));
         addInstr(env, s390_insn_move(8, f15, op_lo));

         rounding_mode = get_bfp_rounding_mode(env, arg1);
         addInstr(env, s390_insn_bfp128_convert_from(size, conv, res,
                                                     INVALID_HREG, f13, f15,
                                                     rounding_mode));
         return res;
      }

      do_convert_dfp: {
            s390_dfp_round_t rounding_mode;

            res  = newVRegI(env);
            h1   = s390_isel_dfp_expr(env, arg2);   /* Process operand */

            rounding_mode = get_dfp_rounding_mode(env, arg1);
            addInstr(env, s390_insn_dfp_convert(size, dconv, res, h1,
                                                rounding_mode));
            return res;
         }

      do_convert_dfp128: {
            s390_dfp_round_t rounding_mode;
            HReg op_hi, op_lo, f13, f15;

            res = newVRegI(env);
            s390_isel_dfp128_expr(&op_hi, &op_lo, env, arg2); /* operand */

            /* We use non-virtual registers r13 and r15 as pair */
            f13 = make_fpr(13);
            f15 = make_fpr(15);

            /* operand --> (f13, f15) */
            addInstr(env, s390_insn_move(8, f13, op_hi));
            addInstr(env, s390_insn_move(8, f15, op_lo));

            rounding_mode = get_dfp_rounding_mode(env, arg1);
            addInstr(env, s390_insn_dfp128_convert_from(size, dconv, res,
                                                        INVALID_HREG, f13,
                                                        f15, rounding_mode));
            return res;
         }

      case Iop_8HLto16:
      case Iop_16HLto32:
      case Iop_32HLto64: {
         HReg h2;
         UInt arg_size = size / 2;

         res  = newVRegI(env);
         h1   = s390_isel_int_expr(env, arg1);   /* Process 1st operand */
         h2   = s390_isel_int_expr(env, arg2);   /* Process 2nd operand */

         addInstr(env, s390_insn_move(arg_size, res, h1));
         value = s390_opnd_imm(arg_size * 8);
         addInstr(env, s390_insn_alu(size, S390_ALU_LSH, res, value));
         value = s390_opnd_imm((((ULong)1) << arg_size * 8) - 1);
         addInstr(env, s390_insn_alu(size, S390_ALU_AND, h2, value));
         opnd = s390_opnd_reg(h2);
         addInstr(env, s390_insn_alu(size, S390_ALU_OR,  res, opnd));
         return res;
      }

      case Iop_Max32U: {
         /* arg1 > arg2 ? arg1 : arg2   using uint32_t arguments */
         res = newVRegI(env);
         h1  = s390_isel_int_expr(env, arg1);
         op2 = s390_isel_int_expr_RMI(env, arg2);

         addInstr(env, s390_insn_move(size, res, h1));
         addInstr(env, s390_insn_compare(size, res, op2, False /* signed */));
         addInstr(env, s390_insn_cond_move(size, S390_CC_L, res, op2));
         return res;
      }

      case Iop_CmpF32:
      case Iop_CmpF64: {
         HReg cc_s390, h2;

         h1 = s390_isel_float_expr(env, arg1);
         h2 = s390_isel_float_expr(env, arg2);
         cc_s390 = newVRegI(env);

         size = (expr->Iex.Binop.op == Iop_CmpF32) ? 4 : 8;

         addInstr(env, s390_insn_bfp_compare(size, cc_s390, h1, h2));

         return convert_s390_to_vex_bfpcc(env, cc_s390);
      }

      case Iop_CmpF128: {
         HReg op1_hi, op1_lo, op2_hi, op2_lo, f12, f13, f14, f15, cc_s390;

         s390_isel_float128_expr(&op1_hi, &op1_lo, env, arg1); /* 1st operand */
         s390_isel_float128_expr(&op2_hi, &op2_lo, env, arg2); /* 2nd operand */
         cc_s390 = newVRegI(env);

         /* We use non-virtual registers as pairs (f13, f15) and (f12, f14)) */
         f12 = make_fpr(12);
         f13 = make_fpr(13);
         f14 = make_fpr(14);
         f15 = make_fpr(15);

         /* 1st operand --> (f12, f14) */
         addInstr(env, s390_insn_move(8, f12, op1_hi));
         addInstr(env, s390_insn_move(8, f14, op1_lo));

         /* 2nd operand --> (f13, f15) */
         addInstr(env, s390_insn_move(8, f13, op2_hi));
         addInstr(env, s390_insn_move(8, f15, op2_lo));

         res = newVRegI(env);
         addInstr(env, s390_insn_bfp128_compare(16, cc_s390, f12, f14, f13, f15));

         return convert_s390_to_vex_bfpcc(env, cc_s390);
      }

      case Iop_CmpD64:
      case Iop_CmpExpD64: {
         HReg cc_s390, h2;
         s390_dfp_cmp_t cmp;

         h1 = s390_isel_dfp_expr(env, arg1);
         h2 = s390_isel_dfp_expr(env, arg2);
         cc_s390 = newVRegI(env);

         switch(expr->Iex.Binop.op) {
         case Iop_CmpD64:    cmp = S390_DFP_COMPARE; break;
         case Iop_CmpExpD64: cmp = S390_DFP_COMPARE_EXP; break;
         default: goto irreducible;
         }
         addInstr(env, s390_insn_dfp_compare(8, cmp, cc_s390, h1, h2));

         return convert_s390_to_vex_dfpcc(env, cc_s390);
      }

      case Iop_CmpD128:
      case Iop_CmpExpD128: {
         HReg op1_hi, op1_lo, op2_hi, op2_lo, f12, f13, f14, f15, cc_s390;
         s390_dfp_cmp_t cmp;

         s390_isel_dfp128_expr(&op1_hi, &op1_lo, env, arg1); /* 1st operand */
         s390_isel_dfp128_expr(&op2_hi, &op2_lo, env, arg2); /* 2nd operand */
         cc_s390 = newVRegI(env);

         /* We use non-virtual registers as pairs (f13, f15) and (f12, f14)) */
         f12 = make_fpr(12);
         f13 = make_fpr(13);
         f14 = make_fpr(14);
         f15 = make_fpr(15);

         /* 1st operand --> (f12, f14) */
         addInstr(env, s390_insn_move(8, f12, op1_hi));
         addInstr(env, s390_insn_move(8, f14, op1_lo));

         /* 2nd operand --> (f13, f15) */
         addInstr(env, s390_insn_move(8, f13, op2_hi));
         addInstr(env, s390_insn_move(8, f15, op2_lo));

         switch(expr->Iex.Binop.op) {
         case Iop_CmpD128:    cmp = S390_DFP_COMPARE; break;
         case Iop_CmpExpD128: cmp = S390_DFP_COMPARE_EXP; break;
         default: goto irreducible;
         }
         addInstr(env, s390_insn_dfp128_compare(16, cmp, cc_s390, f12, f14,
                                                f13, f15));

         return convert_s390_to_vex_dfpcc(env, cc_s390);
      }

      case Iop_Add8:
      case Iop_Add16:
      case Iop_Add32:
      case Iop_Add64:
         opkind = S390_ALU_ADD;
         break;

      case Iop_Sub8:
      case Iop_Sub16:
      case Iop_Sub32:
      case Iop_Sub64:
         opkind = S390_ALU_SUB;
         is_commutative = False;
         break;

      case Iop_And8:
      case Iop_And16:
      case Iop_And32:
      case Iop_And64:
         opkind = S390_ALU_AND;
         break;

      case Iop_Or8:
      case Iop_Or16:
      case Iop_Or32:
      case Iop_Or64:
         opkind = S390_ALU_OR;
         break;

      case Iop_Xor8:
      case Iop_Xor16:
      case Iop_Xor32:
      case Iop_Xor64:
         opkind = S390_ALU_XOR;
         break;

      case Iop_Shl8:
      case Iop_Shl16:
      case Iop_Shl32:
      case Iop_Shl64:
         opkind = S390_ALU_LSH;
         is_commutative = False;
         break;

      case Iop_Shr8:
      case Iop_Shr16:
      case Iop_Shr32:
      case Iop_Shr64:
         opkind = S390_ALU_RSH;
         is_commutative = False;
         break;

      case Iop_Sar8:
      case Iop_Sar16:
      case Iop_Sar32:
      case Iop_Sar64:
         opkind = S390_ALU_RSHA;
         is_commutative = False;
         break;

      default:
         goto irreducible;
      }

      /* Pattern match: 0 - arg1  -->  -arg1   */
      if (opkind == S390_ALU_SUB && s390_expr_is_const_zero(arg1)) {
         res  = newVRegI(env);
         op2  = s390_isel_int_expr_RMI(env, arg2);   /* Process 2nd operand */
         insn = s390_insn_unop(size, S390_NEGATE, res, op2);
         addInstr(env, insn);

         return res;
      }

      if (is_commutative) {
         order_commutative_operands(arg1, arg2);
      }

      h1   = s390_isel_int_expr(env, arg1);       /* Process 1st operand */
      op2  = s390_isel_int_expr_RMI(env, arg2);   /* Process 2nd operand */
      res  = newVRegI(env);

      /* As right shifts of one/two byte opreands are implemented using a
         4-byte shift op, we first need to zero/sign-extend the shiftee. */
      switch (expr->Iex.Binop.op) {
      case Iop_Shr8:
         insn = s390_insn_unop(4, S390_ZERO_EXTEND_8, res, s390_opnd_reg(h1));
         break;
      case Iop_Shr16:
         insn = s390_insn_unop(4, S390_ZERO_EXTEND_16, res, s390_opnd_reg(h1));
         break;
      case Iop_Sar8:
         insn = s390_insn_unop(4, S390_SIGN_EXTEND_8, res, s390_opnd_reg(h1));
         break;
      case Iop_Sar16:
         insn = s390_insn_unop(4, S390_SIGN_EXTEND_16, res, s390_opnd_reg(h1));
         break;
      default:
         insn = s390_insn_move(size, res, h1);
         break;
      }
      addInstr(env, insn);

      insn = s390_insn_alu(size, opkind, res, op2);

      addInstr(env, insn);

      return res;
   }

      /* --------- UNARY OP --------- */
   case Iex_Unop: {
      static s390_opnd_RMI mask  = { S390_OPND_IMMEDIATE };
      static s390_opnd_RMI shift = { S390_OPND_IMMEDIATE };
      s390_opnd_RMI opnd;
      s390_insn    *insn;
      IRExpr *arg;
      HReg    dst, h1;
      IROp    unop, binop;

      arg = expr->Iex.Unop.arg;

      /* Special cases are handled here */

      /* 32-bit multiply with 32-bit result or
         64-bit multiply with 64-bit result */
      unop  = expr->Iex.Unop.op;
      binop = arg->Iex.Binop.op;

      if ((arg->tag == Iex_Binop &&
           ((unop == Iop_64to32 &&
             (binop == Iop_MullS32 || binop == Iop_MullU32)) ||
            (unop == Iop_128to64 &&
             (binop == Iop_MullS64 || binop == Iop_MullU64))))) {
         h1   = s390_isel_int_expr(env, arg->Iex.Binop.arg1);     /* 1st opnd */
         opnd = s390_isel_int_expr_RMI(env, arg->Iex.Binop.arg2); /* 2nd opnd */
         dst  = newVRegI(env);     /* Result goes into a new register */
         addInstr(env, s390_insn_move(size, dst, h1));
         addInstr(env, s390_insn_alu(size, S390_ALU_MUL, dst, opnd));

         return dst;
      }

      if (unop == Iop_ReinterpF64asI64 || unop == Iop_ReinterpF32asI32) {
         dst = newVRegI(env);
         h1  = s390_isel_float_expr(env, arg);     /* Process the operand */
         addInstr(env, s390_insn_move(size, dst, h1));

         return dst;
      }

      if (unop == Iop_ReinterpD64asI64) {
         dst = newVRegI(env);
         h1  = s390_isel_dfp_expr(env, arg);     /* Process the operand */
         addInstr(env, s390_insn_move(size, dst, h1));

         return dst;
      }

      if (unop == Iop_ExtractExpD64 || unop == Iop_ExtractSigD64) {
         s390_dfp_unop_t dfpop;
         switch(unop) {
         case Iop_ExtractExpD64: dfpop = S390_DFP_EXTRACT_EXP_D64; break;
         case Iop_ExtractSigD64: dfpop = S390_DFP_EXTRACT_SIG_D64; break;
         default: goto irreducible;
         }
         dst = newVRegI(env);
         h1  = s390_isel_dfp_expr(env, arg);     /* Process the operand */
         addInstr(env, s390_insn_dfp_unop(size, dfpop, dst, h1));
         return dst;
      }

      if (unop == Iop_ExtractExpD128 || unop == Iop_ExtractSigD128) {
         s390_dfp_unop_t dfpop;
         HReg op_hi, op_lo, f13, f15;

         switch(unop) {
         case Iop_ExtractExpD128: dfpop = S390_DFP_EXTRACT_EXP_D128; break;
         case Iop_ExtractSigD128: dfpop = S390_DFP_EXTRACT_SIG_D128; break;
         default: goto irreducible;
         }
         dst = newVRegI(env);
         s390_isel_dfp128_expr(&op_hi, &op_lo, env, arg); /* Process operand */

         /* We use non-virtual registers r13 and r15 as pair */
         f13 = make_fpr(13);
         f15 = make_fpr(15);

         /* operand --> (f13, f15) */
         addInstr(env, s390_insn_move(8, f13, op_hi));
         addInstr(env, s390_insn_move(8, f15, op_lo));

         addInstr(env, s390_insn_dfp128_unop(size, dfpop, dst, f13, f15));
         return dst;
      }

      /* Expressions whose argument is 1-bit wide */
      if (typeOfIRExpr(env->type_env, arg) == Ity_I1) {
         s390_cc_t cond = s390_isel_cc(env, arg);
         dst = newVRegI(env);     /* Result goes into a new register */
         addInstr(env, s390_insn_cc2bool(dst, cond));

         switch (unop) {
         case Iop_1Uto8:
         case Iop_1Uto32:
            /* Zero extend */
            mask.variant.imm = 1;
            addInstr(env, s390_insn_alu(4, S390_ALU_AND,  dst, mask));
            break;

         case Iop_1Uto64:
            /* Zero extend */
            mask.variant.imm = 1;
            addInstr(env, s390_insn_alu(8, S390_ALU_AND,  dst, mask));
            break;

         case Iop_1Sto8:
         case Iop_1Sto16:
         case Iop_1Sto32:
            shift.variant.imm = 31;
            addInstr(env, s390_insn_alu(4, S390_ALU_LSH,  dst, shift));
            addInstr(env, s390_insn_alu(4, S390_ALU_RSHA, dst, shift));
            break;

         case Iop_1Sto64:
            shift.variant.imm = 63;
            addInstr(env, s390_insn_alu(8, S390_ALU_LSH,  dst, shift));
            addInstr(env, s390_insn_alu(8, S390_ALU_RSHA, dst, shift));
            break;

         default:
            goto irreducible;
         }

         return dst;
      }

      /* Regular processing */

      if (unop == Iop_128to64) {
         HReg dst_hi, dst_lo;

         s390_isel_int128_expr(&dst_hi, &dst_lo, env, arg);
         return dst_lo;
      }

      if (unop == Iop_128HIto64) {
         HReg dst_hi, dst_lo;

         s390_isel_int128_expr(&dst_hi, &dst_lo, env, arg);
         return dst_hi;
      }

      dst  = newVRegI(env);     /* Result goes into a new register */
      opnd = s390_isel_int_expr_RMI(env, arg);     /* Process the operand */

      switch (unop) {
      case Iop_8Uto16:
      case Iop_8Uto32:
      case Iop_8Uto64:
         insn = s390_insn_unop(size, S390_ZERO_EXTEND_8, dst, opnd);
         break;

      case Iop_16Uto32:
      case Iop_16Uto64:
         insn = s390_insn_unop(size, S390_ZERO_EXTEND_16, dst, opnd);
         break;

      case Iop_32Uto64:
         insn = s390_insn_unop(size, S390_ZERO_EXTEND_32, dst, opnd);
         break;

      case Iop_8Sto16:
      case Iop_8Sto32:
      case Iop_8Sto64:
         insn = s390_insn_unop(size, S390_SIGN_EXTEND_8, dst, opnd);
         break;

      case Iop_16Sto32:
      case Iop_16Sto64:
         insn = s390_insn_unop(size, S390_SIGN_EXTEND_16, dst, opnd);
         break;

      case Iop_32Sto64:
         insn = s390_insn_unop(size, S390_SIGN_EXTEND_32, dst, opnd);
         break;

      case Iop_64to8:
      case Iop_64to16:
      case Iop_64to32:
      case Iop_32to8:
      case Iop_32to16:
      case Iop_16to8:
         /* Down-casts are no-ops. Upstream operations will only look at
            the bytes that make up the result of the down-cast. So there
            is no point setting the other bytes to 0. */
         insn = s390_opnd_copy(8, dst, opnd);
         break;

      case Iop_64HIto32:
         addInstr(env, s390_opnd_copy(8, dst, opnd));
         shift.variant.imm = 32;
         insn = s390_insn_alu(8, S390_ALU_RSH, dst, shift);
         break;

      case Iop_32HIto16:
         addInstr(env, s390_opnd_copy(4, dst, opnd));
         shift.variant.imm = 16;
         insn = s390_insn_alu(4, S390_ALU_RSH, dst, shift);
         break;

      case Iop_16HIto8:
         addInstr(env, s390_opnd_copy(2, dst, opnd));
         shift.variant.imm = 8;
         insn = s390_insn_alu(2, S390_ALU_RSH, dst, shift);
         break;

      case Iop_Not8:
      case Iop_Not16:
      case Iop_Not32:
      case Iop_Not64:
         /* XOR with ffff... */
         mask.variant.imm = ~(ULong)0;
         addInstr(env, s390_opnd_copy(size, dst, opnd));
         insn = s390_insn_alu(size, S390_ALU_XOR, dst, mask);
         break;

      case Iop_Left8:
      case Iop_Left16:
      case Iop_Left32:
      case Iop_Left64:
         addInstr(env, s390_insn_unop(size, S390_NEGATE, dst, opnd));
         insn = s390_insn_alu(size, S390_ALU_OR, dst, opnd);
         break;

      case Iop_CmpwNEZ32:
      case Iop_CmpwNEZ64: {
         /* Use the fact that x | -x == 0 iff x == 0. Otherwise, either X
            or -X will have a 1 in the MSB. */
         addInstr(env, s390_insn_unop(size, S390_NEGATE, dst, opnd));
         addInstr(env, s390_insn_alu(size, S390_ALU_OR,  dst, opnd));
         shift.variant.imm = (unop == Iop_CmpwNEZ32) ? 31 : 63;
         addInstr(env, s390_insn_alu(size, S390_ALU_RSHA,  dst, shift));
         return dst;
      }

      case Iop_Clz64: {
         HReg r10, r11;

         /* This will be implemented using FLOGR, if possible. So we need to
            set aside a pair of non-virtual registers. The result (number of
            left-most zero bits) will be in r10. The value in r11 is unspecified
            and must not be used. */
         r10  = make_gpr(10);
         r11  = make_gpr(11);

         addInstr(env, s390_insn_clz(8, r10, r11, opnd));
         addInstr(env, s390_insn_move(8, dst, r10));
         return dst;
      }

      default:
         goto irreducible;
      }

      addInstr(env, insn);

      return dst;
   }

      /* --------- GET --------- */
   case Iex_Get: {
      HReg dst = newVRegI(env);
      s390_amode *am = s390_amode_for_guest_state(expr->Iex.Get.offset);

      /* We never load more than 8 bytes from the guest state, because the
         floating point register pair is not contiguous. */
      vassert(size <= 8);

      addInstr(env, s390_insn_load(size, dst, am));

      return dst;
   }

   case Iex_GetI:
      /* not needed */
      break;

      /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg dst = newVRegI(env);
      HReg ret = make_gpr(S390_REGNO_RETURN_VALUE);
      UInt   addToSp = 0;
      RetLoc rloc    = mk_RetLoc_INVALID();

      doHelperCall(&addToSp, &rloc, env, NULL, expr->Iex.CCall.cee,
                   expr->Iex.CCall.retty, expr->Iex.CCall.args);
      vassert(is_sane_RetLoc(rloc));
      vassert(rloc.pri == RLPri_Int);
      vassert(addToSp == 0);
      addInstr(env, s390_insn_move(sizeof(ULong), dst, ret));

      return dst;
   }

      /* --------- LITERAL --------- */

      /* Load a literal into a register. Create a "load immediate"
         v-insn and return the register. */
   case Iex_Const: {
      ULong value;
      HReg  dst = newVRegI(env);
      const IRConst *con = expr->Iex.Const.con;

      /* Bitwise copy of the value. No sign/zero-extension */
      switch (con->tag) {
      case Ico_U64: value = con->Ico.U64; break;
      case Ico_U32: value = con->Ico.U32; break;
      case Ico_U16: value = con->Ico.U16; break;
      case Ico_U8:  value = con->Ico.U8;  break;
      default:      vpanic("s390_isel_int_expr: invalid constant");
      }

      addInstr(env, s390_insn_load_immediate(size, dst, value));

      return dst;
   }

      /* --------- MULTIPLEX --------- */
   case Iex_ITE: {
      IRExpr *cond_expr;
      HReg dst, r1;
      s390_opnd_RMI r0;

      cond_expr = expr->Iex.ITE.cond;

      vassert(typeOfIRExpr(env->type_env, cond_expr) == Ity_I1);

      dst  = newVRegI(env);
      r0   = s390_isel_int_expr_RMI(env, expr->Iex.ITE.iffalse);
      r1   = s390_isel_int_expr(env, expr->Iex.ITE.iftrue);
      size = sizeofIRType(typeOfIRExpr(env->type_env, expr->Iex.ITE.iftrue));

      s390_cc_t cc = s390_isel_cc(env, cond_expr);

      addInstr(env, s390_insn_move(size, dst, r1));
      addInstr(env, s390_insn_cond_move(size, s390_cc_invert(cc), dst, r0));
      return dst;
   }

   default:
      break;
   }

   /* We get here if no pattern matched. */
 irreducible:
   ppIRExpr(expr);
   vpanic("s390_isel_int_expr: cannot reduce tree");
}


static HReg
s390_isel_int_expr(ISelEnv *env, IRExpr *expr)
{
   HReg dst = s390_isel_int_expr_wrk(env, expr);

   /* Sanity checks ... */
   vassert(hregClass(dst) == HRcInt64);
   vassert(hregIsVirtual(dst));

   return dst;
}


static s390_opnd_RMI
s390_isel_int_expr_RMI(ISelEnv *env, IRExpr *expr)
{
   IRType ty = typeOfIRExpr(env->type_env, expr);
   s390_opnd_RMI dst;

   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 ||
           ty == Ity_I64);

   if (expr->tag == Iex_Load) {
      dst.tag = S390_OPND_AMODE;
      dst.variant.am = s390_isel_amode(env, expr->Iex.Load.addr);
   } else if (expr->tag == Iex_Get) {
      dst.tag = S390_OPND_AMODE;
      dst.variant.am = s390_amode_for_guest_state(expr->Iex.Get.offset);
   } else if (expr->tag == Iex_Const) {
      ULong value;

      /* The bit pattern for the value will be stored as is in the least
         significant bits of VALUE. */
      switch (expr->Iex.Const.con->tag) {
      case Ico_U1:  value = expr->Iex.Const.con->Ico.U1;  break;
      case Ico_U8:  value = expr->Iex.Const.con->Ico.U8;  break;
      case Ico_U16: value = expr->Iex.Const.con->Ico.U16; break;
      case Ico_U32: value = expr->Iex.Const.con->Ico.U32; break;
      case Ico_U64: value = expr->Iex.Const.con->Ico.U64; break;
      default:
         vpanic("s390_isel_int_expr_RMI");
      }

      dst.tag = S390_OPND_IMMEDIATE;
      dst.variant.imm = value;
   } else {
      dst.tag = S390_OPND_REG;
      dst.variant.reg = s390_isel_int_expr(env, expr);
   }

   return dst;
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (128 bit)        ---*/
/*---------------------------------------------------------*/
static void
s390_isel_float128_expr_wrk(HReg *dst_hi, HReg *dst_lo, ISelEnv *env,
                            IRExpr *expr)
{
   IRType ty = typeOfIRExpr(env->type_env, expr);

   vassert(ty == Ity_F128);

   switch (expr->tag) {
   case Iex_RdTmp:
      /* Return the virtual registers that hold the temporary. */
      lookupIRTemp128(dst_hi, dst_lo, env, expr->Iex.RdTmp.tmp);
      return;

      /* --------- LOAD --------- */
   case Iex_Load: {
      IRExpr *addr_hi, *addr_lo;
      s390_amode *am_hi, *am_lo;

      if (expr->Iex.Load.end != Iend_BE)
         goto irreducible;

      addr_hi = expr->Iex.Load.addr;
      addr_lo = IRExpr_Binop(Iop_Add64, addr_hi, mkU64(8));

      am_hi  = s390_isel_amode(env, addr_hi);
      am_lo  = s390_isel_amode(env, addr_lo);

      *dst_hi = newVRegF(env);
      *dst_lo = newVRegF(env);
      addInstr(env, s390_insn_load(8, *dst_hi, am_hi));
      addInstr(env, s390_insn_load(8, *dst_hi, am_lo));
      return;
   }


      /* --------- GET --------- */
   case Iex_Get:
      /* This is not supported because loading 128-bit from the guest
         state is almost certainly wrong. Use get_fpr_pair instead. */
      vpanic("Iex_Get with F128 data");

      /* --------- 4-ary OP --------- */
   case Iex_Qop:
      vpanic("Iex_Qop with F128 data");

      /* --------- TERNARY OP --------- */
   case Iex_Triop: {
      IRTriop *triop = expr->Iex.Triop.details;
      IROp    op     = triop->op;
      IRExpr *left   = triop->arg2;
      IRExpr *right  = triop->arg3;
      s390_bfp_binop_t bfpop;
      HReg op1_hi, op1_lo, op2_hi, op2_lo, f12, f13, f14, f15;

      s390_isel_float128_expr(&op1_hi, &op1_lo, env, left);  /* 1st operand */
      s390_isel_float128_expr(&op2_hi, &op2_lo, env, right); /* 2nd operand */

      /* We use non-virtual registers as pairs (f13, f15) and (f12, f14)) */
      f12 = make_fpr(12);
      f13 = make_fpr(13);
      f14 = make_fpr(14);
      f15 = make_fpr(15);

      /* 1st operand --> (f12, f14) */
      addInstr(env, s390_insn_move(8, f12, op1_hi));
      addInstr(env, s390_insn_move(8, f14, op1_lo));

      /* 2nd operand --> (f13, f15) */
      addInstr(env, s390_insn_move(8, f13, op2_hi));
      addInstr(env, s390_insn_move(8, f15, op2_lo));

      switch (op) {
      case Iop_AddF128: bfpop = S390_BFP_ADD; break;
      case Iop_SubF128: bfpop = S390_BFP_SUB; break;
      case Iop_MulF128: bfpop = S390_BFP_MUL; break;
      case Iop_DivF128: bfpop = S390_BFP_DIV; break;
      default:
         goto irreducible;
      }

      set_bfp_rounding_mode_in_fpc(env, triop->arg1);
      addInstr(env, s390_insn_bfp128_binop(16, bfpop, f12, f14, f13, f15));

      /* Move result to virtual destination register */
      *dst_hi = newVRegF(env);
      *dst_lo = newVRegF(env);
      addInstr(env, s390_insn_move(8, *dst_hi, f12));
      addInstr(env, s390_insn_move(8, *dst_lo, f14));

      return;
   }

      /* --------- BINARY OP --------- */
   case Iex_Binop: {
      switch (expr->Iex.Binop.op) {
      case Iop_SqrtF128: {
         HReg op_hi, op_lo, f12, f13, f14, f15;

         /* We use non-virtual registers as pairs (f13, f15) and (f12, f14)) */
         f12 = make_fpr(12);
         f13 = make_fpr(13);
         f14 = make_fpr(14);
         f15 = make_fpr(15);

         s390_isel_float128_expr(&op_hi, &op_lo, env, expr->Iex.Binop.arg2);

         /* operand --> (f13, f15) */
         addInstr(env, s390_insn_move(8, f13, op_hi));
         addInstr(env, s390_insn_move(8, f15, op_lo));

         set_bfp_rounding_mode_in_fpc(env, expr->Iex.Binop.arg1);
         addInstr(env, s390_insn_bfp128_unop(16, S390_BFP_SQRT, f12, f14,
                                             f13, f15));

         /* Move result to virtual destination registers */
         *dst_hi = newVRegF(env);
         *dst_lo = newVRegF(env);
         addInstr(env, s390_insn_move(8, *dst_hi, f12));
         addInstr(env, s390_insn_move(8, *dst_lo, f14));
         return;
      }

      case Iop_F64HLtoF128:
         *dst_hi = s390_isel_float_expr(env, expr->Iex.Binop.arg1);
         *dst_lo = s390_isel_float_expr(env, expr->Iex.Binop.arg2);
         return;

      case Iop_D32toF128:
      case Iop_D64toF128: {
         IRExpr *irrm;
         IRExpr *left;
         s390_dfp_round_t rm;
         HReg h1; /* virtual reg. to hold source */
         HReg f0, f2, f4, r1; /* real registers used by PFPO */
         s390_fp_conv_t fpconv;

         switch (expr->Iex.Binop.op) {
         case Iop_D32toF128:
            fpconv = S390_FP_D32_TO_F128;
            break;
         case Iop_D64toF128:
            fpconv = S390_FP_D64_TO_F128;
            break;
         default: goto irreducible;
         }

         f4 = make_fpr(4); /* source */
         f0 = make_fpr(0); /* destination */
         f2 = make_fpr(2); /* destination */
         r1 = make_gpr(1); /* GPR #1 clobbered */
         irrm = expr->Iex.Binop.arg1;
         left = expr->Iex.Binop.arg2;
         rm = get_dfp_rounding_mode(env, irrm);
         h1 = s390_isel_dfp_expr(env, left);
         addInstr(env, s390_insn_move(8, f4, h1));
         addInstr(env, s390_insn_fp128_convert(16, fpconv, f0, f2,
                                               f4, INVALID_HREG, r1, rm));
         /* (f0, f2) --> destination */
         *dst_hi = newVRegF(env);
         *dst_lo = newVRegF(env);
         addInstr(env, s390_insn_move(8, *dst_hi, f0));
         addInstr(env, s390_insn_move(8, *dst_lo, f2));

         return;
      }

      case Iop_D128toF128: {
         IRExpr *irrm;
         IRExpr *left;
         s390_dfp_round_t rm;
         HReg op_hi, op_lo;
         HReg f0, f2, f4, f6, r1; /* real registers used by PFPO */

         f4 = make_fpr(4); /* source */
         f6 = make_fpr(6); /* source */
         f0 = make_fpr(0); /* destination */
         f2 = make_fpr(2); /* destination */
         r1 = make_gpr(1); /* GPR #1 clobbered */

         irrm = expr->Iex.Binop.arg1;
         left = expr->Iex.Binop.arg2;
         rm = get_dfp_rounding_mode(env, irrm);
         s390_isel_dfp128_expr(&op_hi, &op_lo, env, left);
         /* operand --> (f4, f6) */
         addInstr(env, s390_insn_move(8, f4, op_hi));
         addInstr(env, s390_insn_move(8, f6, op_lo));
         addInstr(env, s390_insn_fp128_convert(16, S390_FP_D128_TO_F128, f0, f2,
                                               f4, f6, r1, rm));
         /* (f0, f2) --> destination */
         *dst_hi = newVRegF(env);
         *dst_lo = newVRegF(env);
         addInstr(env, s390_insn_move(8, *dst_hi, f0));
         addInstr(env, s390_insn_move(8, *dst_lo, f2));

         return;
      }

      default:
         goto irreducible;
      }
   }

      /* --------- UNARY OP --------- */
   case Iex_Unop: {
      IRExpr *left = expr->Iex.Unop.arg;
      s390_bfp_unop_t bfpop;
      s390_bfp_conv_t conv;
      HReg op_hi, op_lo, op, f12, f13, f14, f15;

      /* We use non-virtual registers as pairs (f13, f15) and (f12, f14)) */
      f12 = make_fpr(12);
      f13 = make_fpr(13);
      f14 = make_fpr(14);
      f15 = make_fpr(15);

      switch (expr->Iex.Unop.op) {
      case Iop_NegF128:
         if (left->tag == Iex_Unop &&
             (left->Iex.Unop.op == Iop_AbsF32 ||
              left->Iex.Unop.op == Iop_AbsF64))
            bfpop = S390_BFP_NABS;
         else
            bfpop = S390_BFP_NEG;
         goto float128_opnd;
      case Iop_AbsF128:     bfpop = S390_BFP_ABS;         goto float128_opnd;
      case Iop_I32StoF128:  conv = S390_BFP_I32_TO_F128;  goto convert_int;
      case Iop_I64StoF128:  conv = S390_BFP_I64_TO_F128;  goto convert_int;
      case Iop_I32UtoF128:  conv = S390_BFP_U32_TO_F128;  goto convert_int;
      case Iop_I64UtoF128:  conv = S390_BFP_U64_TO_F128;  goto convert_int;
      case Iop_F32toF128:   conv = S390_BFP_F32_TO_F128;  goto convert_float;
      case Iop_F64toF128:   conv = S390_BFP_F64_TO_F128;  goto convert_float;
      default:
         goto irreducible;
      }

   float128_opnd:
      s390_isel_float128_expr(&op_hi, &op_lo, env, left);

      /* operand --> (f13, f15) */
      addInstr(env, s390_insn_move(8, f13, op_hi));
      addInstr(env, s390_insn_move(8, f15, op_lo));

      addInstr(env, s390_insn_bfp128_unop(16, bfpop, f12, f14, f13, f15));
      goto move_dst;

   convert_float:
      op  = s390_isel_float_expr(env, left);
      addInstr(env, s390_insn_bfp128_convert_to(16, conv, f12, f14, op));
      goto move_dst;

   convert_int:
      op  = s390_isel_int_expr(env, left);
      addInstr(env, s390_insn_bfp128_convert_to(16, conv, f12, f14, op));
      goto move_dst;

   move_dst:
      /* Move result to virtual destination registers */
      *dst_hi = newVRegF(env);
      *dst_lo = newVRegF(env);
      addInstr(env, s390_insn_move(8, *dst_hi, f12));
      addInstr(env, s390_insn_move(8, *dst_lo, f14));
      return;
   }

   default:
      goto irreducible;
   }

   /* We get here if no pattern matched. */
 irreducible:
   ppIRExpr(expr);
   vpanic("s390_isel_float128_expr: cannot reduce tree");
}

/* Compute a 128-bit value into two 64-bit registers. These may be either
   real or virtual regs; in any case they must not be changed by subsequent
   code emitted by the caller. */
static void
s390_isel_float128_expr(HReg *dst_hi, HReg *dst_lo, ISelEnv *env, IRExpr *expr)
{
   s390_isel_float128_expr_wrk(dst_hi, dst_lo, env, expr);

   /* Sanity checks ... */
   vassert(hregIsVirtual(*dst_hi));
   vassert(hregIsVirtual(*dst_lo));
   vassert(hregClass(*dst_hi) == HRcFlt64);
   vassert(hregClass(*dst_lo) == HRcFlt64);
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (64 bit)         ---*/
/*---------------------------------------------------------*/

static HReg
s390_isel_float_expr_wrk(ISelEnv *env, IRExpr *expr)
{
   IRType ty = typeOfIRExpr(env->type_env, expr);
   UChar size;

   vassert(ty == Ity_F32 || ty == Ity_F64);

   size = sizeofIRType(ty);

   switch (expr->tag) {
   case Iex_RdTmp:
      /* Return the virtual register that holds the temporary. */
      return lookupIRTemp(env, expr->Iex.RdTmp.tmp);

      /* --------- LOAD --------- */
   case Iex_Load: {
      HReg        dst = newVRegF(env);
      s390_amode *am  = s390_isel_amode(env, expr->Iex.Load.addr);

      if (expr->Iex.Load.end != Iend_BE)
         goto irreducible;

      addInstr(env, s390_insn_load(size, dst, am));

      return dst;
   }

      /* --------- GET --------- */
   case Iex_Get: {
      HReg dst = newVRegF(env);
      s390_amode *am = s390_amode_for_guest_state(expr->Iex.Get.offset);

      addInstr(env, s390_insn_load(size, dst, am));

      return dst;
   }

      /* --------- LITERAL --------- */

      /* Load a literal into a register. Create a "load immediate"
         v-insn and return the register. */
   case Iex_Const: {
      ULong value;
      HReg  dst = newVRegF(env);
      const IRConst *con = expr->Iex.Const.con;

      /* Bitwise copy of the value. No sign/zero-extension */
      switch (con->tag) {
      case Ico_F32i: value = con->Ico.F32i; break;
      case Ico_F64i: value = con->Ico.F64i; break;
      default:       vpanic("s390_isel_float_expr: invalid constant");
      }

      if (value != 0) vpanic("cannot load immediate floating point constant");

      addInstr(env, s390_insn_load_immediate(size, dst, value));

      return dst;
   }

      /* --------- 4-ary OP --------- */
   case Iex_Qop: {
      HReg op1, op2, op3, dst;
      s390_bfp_triop_t bfpop;

      op3 = s390_isel_float_expr(env, expr->Iex.Qop.details->arg2);
      op2 = s390_isel_float_expr(env, expr->Iex.Qop.details->arg3);
      op1 = s390_isel_float_expr(env, expr->Iex.Qop.details->arg4);
      dst = newVRegF(env);
      addInstr(env, s390_insn_move(size, dst, op1));

      switch (expr->Iex.Qop.details->op) {
      case Iop_MAddF32:
      case Iop_MAddF64:  bfpop = S390_BFP_MADD; break;
      case Iop_MSubF32:
      case Iop_MSubF64:  bfpop = S390_BFP_MSUB; break;

      default:
         goto irreducible;
      }

      set_bfp_rounding_mode_in_fpc(env, expr->Iex.Qop.details->arg1);
      addInstr(env, s390_insn_bfp_triop(size, bfpop, dst, op2, op3));
      return dst;
   }

      /* --------- TERNARY OP --------- */
   case Iex_Triop: {
      IRTriop *triop = expr->Iex.Triop.details;
      IROp    op     = triop->op;
      IRExpr *left   = triop->arg2;
      IRExpr *right  = triop->arg3;
      s390_bfp_binop_t bfpop;
      HReg h1, op2, dst;

      h1   = s390_isel_float_expr(env, left);  /* Process 1st operand */
      op2  = s390_isel_float_expr(env, right); /* Process 2nd operand */
      dst  = newVRegF(env);
      addInstr(env, s390_insn_move(size, dst, h1));
      switch (op) {
      case Iop_AddF32:
      case Iop_AddF64:  bfpop = S390_BFP_ADD; break;
      case Iop_SubF32:
      case Iop_SubF64:  bfpop = S390_BFP_SUB; break;
      case Iop_MulF32:
      case Iop_MulF64:  bfpop = S390_BFP_MUL; break;
      case Iop_DivF32:
      case Iop_DivF64:  bfpop = S390_BFP_DIV; break;

      default:
         goto irreducible;
      }

      set_bfp_rounding_mode_in_fpc(env, triop->arg1);
      addInstr(env, s390_insn_bfp_binop(size, bfpop, dst, op2));
      return dst;
   }

      /* --------- BINARY OP --------- */
   case Iex_Binop: {
      IROp    op   = expr->Iex.Binop.op;
      IRExpr *irrm = expr->Iex.Binop.arg1;
      IRExpr *left = expr->Iex.Binop.arg2;
      HReg h1, dst;
      s390_bfp_conv_t  conv;
      s390_fp_conv_t fpconv;

      switch (op) {
      case Iop_SqrtF32:
      case Iop_SqrtF64:
         h1  = s390_isel_float_expr(env, left);
         dst = newVRegF(env);
         set_bfp_rounding_mode_in_fpc(env, irrm);
         addInstr(env, s390_insn_bfp_unop(size, S390_BFP_SQRT, dst, h1));
         return dst;

      case Iop_F64toF32:  conv = S390_BFP_F64_TO_F32; goto convert_float;
      case Iop_I32StoF32: conv = S390_BFP_I32_TO_F32; goto convert_int;
      case Iop_I32UtoF32: conv = S390_BFP_U32_TO_F32; goto convert_int;
      case Iop_I64StoF32: conv = S390_BFP_I64_TO_F32; goto convert_int;
      case Iop_I64StoF64: conv = S390_BFP_I64_TO_F64; goto convert_int;
      case Iop_I64UtoF32: conv = S390_BFP_U64_TO_F32; goto convert_int;
      case Iop_I64UtoF64: conv = S390_BFP_U64_TO_F64; goto convert_int;
      case Iop_D32toF32:  fpconv = S390_FP_D32_TO_F32;  goto convert_dfp;
      case Iop_D32toF64:  fpconv = S390_FP_D32_TO_F64;  goto convert_dfp;
      case Iop_D64toF32:  fpconv = S390_FP_D64_TO_F32;  goto convert_dfp;
      case Iop_D64toF64:  fpconv = S390_FP_D64_TO_F64;  goto convert_dfp;
      case Iop_D128toF32: fpconv = S390_FP_D128_TO_F32; goto convert_dfp128;
      case Iop_D128toF64: fpconv = S390_FP_D128_TO_F64; goto convert_dfp128;

      convert_float:
         h1 = s390_isel_float_expr(env, left);
         goto convert;

      convert_int:
         h1 = s390_isel_int_expr(env, left);
         goto convert;

      convert: {
         s390_bfp_round_t rounding_mode;
         /* convert-from-fixed and load-rounded have a rounding mode field
            when the floating point extension facility is installed. */
         dst = newVRegF(env);
         if (s390_host_has_fpext) {
            rounding_mode = get_bfp_rounding_mode(env, irrm);
         } else {
            set_bfp_rounding_mode_in_fpc(env, irrm);
            rounding_mode = S390_BFP_ROUND_PER_FPC;
         }
         addInstr(env, s390_insn_bfp_convert(size, conv, dst, h1,
                                             rounding_mode));
         return dst;
      }

      convert_dfp: {
         s390_dfp_round_t rm;
         HReg f0, f4, r1; /* real registers used by PFPO */

         f4 = make_fpr(4); /* source */
         f0 = make_fpr(0); /* destination */
         r1 = make_gpr(1); /* GPR #1 clobbered */
         h1 = s390_isel_dfp_expr(env, left);
         dst = newVRegF(env);
         rm = get_dfp_rounding_mode(env, irrm);
         /* operand --> f4 */
         addInstr(env, s390_insn_move(8, f4, h1));
         addInstr(env, s390_insn_fp_convert(size, fpconv, f0, f4, r1, rm));
         /* f0 --> destination */
         addInstr(env, s390_insn_move(8, dst, f0));
         return dst;
      }

      convert_dfp128: {
         s390_dfp_round_t rm;
         HReg op_hi, op_lo;
         HReg f0, f4, f6, r1; /* real registers used by PFPO */

         f4 = make_fpr(4); /* source */
         f6 = make_fpr(6); /* source */
         f0 = make_fpr(0); /* destination */
         r1 = make_gpr(1); /* GPR #1 clobbered */
         s390_isel_dfp128_expr(&op_hi, &op_lo, env, left);
         dst = newVRegF(env);
         rm = get_dfp_rounding_mode(env, irrm);
         /* operand --> (f4, f6) */
         addInstr(env, s390_insn_move(8, f4, op_hi));
         addInstr(env, s390_insn_move(8, f6, op_lo));
         addInstr(env, s390_insn_fp128_convert(16, fpconv, f0, INVALID_HREG,
                                               f4, f6, r1, rm));
         /* f0 --> destination */
         addInstr(env, s390_insn_move(8, dst, f0));
         return dst;
      }

      default:
         goto irreducible;

      case Iop_F128toF64:
      case Iop_F128toF32: {
         HReg op_hi, op_lo, f12, f13, f14, f15;
         s390_bfp_round_t rounding_mode;

         conv = op == Iop_F128toF32 ? S390_BFP_F128_TO_F32
                                    : S390_BFP_F128_TO_F64;

         s390_isel_float128_expr(&op_hi, &op_lo, env, left);

         /* We use non-virtual registers as pairs (f13, f15) and (f12, f14)) */
         f12 = make_fpr(12);
         f13 = make_fpr(13);
         f14 = make_fpr(14);
         f15 = make_fpr(15);

         /* operand --> (f13, f15) */
         addInstr(env, s390_insn_move(8, f13, op_hi));
         addInstr(env, s390_insn_move(8, f15, op_lo));

         /* result --> (f12, f14) */

         /* load-rounded has a rounding mode field when the floating point
            extension facility is installed. */
         if (s390_host_has_fpext) {
            rounding_mode = get_bfp_rounding_mode(env, irrm);
         } else {
            set_bfp_rounding_mode_in_fpc(env, irrm);
            rounding_mode = S390_BFP_ROUND_PER_FPC;
         }

         addInstr(env, s390_insn_bfp128_convert_from(size, conv, f12, f14,
                                                     f13, f15, rounding_mode));
         dst = newVRegF(env);
         addInstr(env, s390_insn_move(8, dst, f12));

         return dst;
      }
      }
   }

      /* --------- UNARY OP --------- */
   case Iex_Unop: {
      IROp    op   = expr->Iex.Unop.op;
      IRExpr *left = expr->Iex.Unop.arg;
      s390_bfp_unop_t bfpop;
      s390_bfp_conv_t conv;
      HReg h1, dst;

      if (op == Iop_F128HItoF64 || op == Iop_F128LOtoF64) {
         HReg dst_hi, dst_lo;

         s390_isel_float128_expr(&dst_hi, &dst_lo, env, left);
         return op == Iop_F128LOtoF64 ? dst_lo : dst_hi;
      }

      if (op == Iop_ReinterpI64asF64 || op == Iop_ReinterpI32asF32) {
         dst = newVRegF(env);
         h1  = s390_isel_int_expr(env, left);     /* Process the operand */
         addInstr(env, s390_insn_move(size, dst, h1));

         return dst;
      }

      switch (op) {
      case Iop_NegF32:
      case Iop_NegF64:
         if (left->tag == Iex_Unop &&
             (left->Iex.Unop.op == Iop_AbsF32 ||
              left->Iex.Unop.op == Iop_AbsF64))
            bfpop = S390_BFP_NABS;
         else
            bfpop = S390_BFP_NEG;
         break;

      case Iop_AbsF32:
      case Iop_AbsF64:
         bfpop = S390_BFP_ABS;
         break;

      case Iop_I32StoF64:  conv = S390_BFP_I32_TO_F64;  goto convert_int1;
      case Iop_I32UtoF64:  conv = S390_BFP_U32_TO_F64;  goto convert_int1;
      case Iop_F32toF64:   conv = S390_BFP_F32_TO_F64;  goto convert_float1;

      convert_float1:
         h1 = s390_isel_float_expr(env, left);
         goto convert1;

      convert_int1:
         h1 = s390_isel_int_expr(env, left);
         goto convert1;

      convert1:
         dst = newVRegF(env);
         /* No rounding mode is needed for these conversions. Just stick
            one in. It won't be used later on. */
         addInstr(env, s390_insn_bfp_convert(size, conv, dst, h1,
                                             S390_BFP_ROUND_NEAREST_EVEN));
         return dst;

      default:
         goto irreducible;
      }

      /* Process operand */
      h1  = s390_isel_float_expr(env, left);
      dst = newVRegF(env);
      addInstr(env, s390_insn_bfp_unop(size, bfpop, dst, h1));
      return dst;
   }

   default:
      goto irreducible;
   }

   /* We get here if no pattern matched. */
 irreducible:
   ppIRExpr(expr);
   vpanic("s390_isel_float_expr: cannot reduce tree");
}


static HReg
s390_isel_float_expr(ISelEnv *env, IRExpr *expr)
{
   HReg dst = s390_isel_float_expr_wrk(env, expr);

   /* Sanity checks ... */
   vassert(hregClass(dst) == HRcFlt64);
   vassert(hregIsVirtual(dst));

   return dst;
}


/*---------------------------------------------------------*/
/*--- ISEL: Decimal point expressions (128 bit)         ---*/
/*---------------------------------------------------------*/
static void
s390_isel_dfp128_expr_wrk(HReg *dst_hi, HReg *dst_lo, ISelEnv *env,
                          IRExpr *expr)
{
   IRType ty = typeOfIRExpr(env->type_env, expr);

   vassert(ty == Ity_D128);

   switch (expr->tag) {
   case Iex_RdTmp:
      /* Return the virtual registers that hold the temporary. */
      lookupIRTemp128(dst_hi, dst_lo, env, expr->Iex.RdTmp.tmp);
      return;

      /* --------- LOAD --------- */
   case Iex_Load: {
      IRExpr *addr_hi, *addr_lo;
      s390_amode *am_hi, *am_lo;

      if (expr->Iex.Load.end != Iend_BE)
         goto irreducible;

      addr_hi = expr->Iex.Load.addr;
      addr_lo = IRExpr_Binop(Iop_Add64, addr_hi, mkU64(8));

      am_hi  = s390_isel_amode(env, addr_hi);
      am_lo  = s390_isel_amode(env, addr_lo);

      *dst_hi = newVRegF(env);
      *dst_lo = newVRegF(env);
      addInstr(env, s390_insn_load(8, *dst_hi, am_hi));
      addInstr(env, s390_insn_load(8, *dst_hi, am_lo));
      return;
   }

      /* --------- GET --------- */
   case Iex_Get:
      /* This is not supported because loading 128-bit from the guest
         state is almost certainly wrong. Use get_dpr_pair instead. */
      vpanic("Iex_Get with D128 data");

      /* --------- 4-ary OP --------- */
   case Iex_Qop:
      vpanic("Iex_Qop with D128 data");

      /* --------- TERNARY OP --------- */
   case Iex_Triop: {
      IRTriop *triop = expr->Iex.Triop.details;
      IROp    op     = triop->op;
      IRExpr *irrm   = triop->arg1;
      IRExpr *left   = triop->arg2;
      IRExpr *right  = triop->arg3;
      s390_dfp_round_t rounding_mode;
      s390_dfp_binop_t dfpop;
      HReg op1_hi, op1_lo, op2_hi, op2_lo, f9, f11, f12, f13, f14, f15;

      /* We use non-virtual registers as pairs with (f9, f11) as op1,
         (f12, f14) as op2 and (f13, f15)  as destination) */
      f9  = make_fpr(9);
      f11 = make_fpr(11);
      f12 = make_fpr(12);
      f13 = make_fpr(13);
      f14 = make_fpr(14);
      f15 = make_fpr(15);

      switch (op) {
      case Iop_AddD128:       dfpop = S390_DFP_ADD;      goto evaluate_dfp128;
      case Iop_SubD128:       dfpop = S390_DFP_SUB;      goto evaluate_dfp128;
      case Iop_MulD128:       dfpop = S390_DFP_MUL;      goto evaluate_dfp128;
      case Iop_DivD128:       dfpop = S390_DFP_DIV;      goto evaluate_dfp128;
      case Iop_QuantizeD128:  dfpop = S390_DFP_QUANTIZE; goto evaluate_dfp128;

      evaluate_dfp128: {
         /* Process 1st operand */
         s390_isel_dfp128_expr(&op1_hi, &op1_lo, env, left);
         /* 1st operand --> (f9, f11) */
         addInstr(env, s390_insn_move(8, f9,  op1_hi));
         addInstr(env, s390_insn_move(8, f11, op1_lo));

         /* Process 2nd operand */
         s390_isel_dfp128_expr(&op2_hi, &op2_lo, env, right);
         /* 2nd operand --> (f12, f14) */
         addInstr(env, s390_insn_move(8, f12, op2_hi));
         addInstr(env, s390_insn_move(8, f14, op2_lo));

         /* DFP arithmetic ops take rounding mode only when fpext is
            installed. But, DFP quantize operation takes rm irrespective
            of fpext facility . */
         if (s390_host_has_fpext || op == Iop_QuantizeD128) {
            rounding_mode = get_dfp_rounding_mode(env, irrm);
         } else {
            set_dfp_rounding_mode_in_fpc(env, irrm);
            rounding_mode = S390_DFP_ROUND_PER_FPC_0;
         }
         addInstr(env, s390_insn_dfp128_binop(16, dfpop, f13, f15, f9, f11,
                                              f12, f14, rounding_mode));
         /* Move result to virtual destination register */
         *dst_hi = newVRegF(env);
         *dst_lo = newVRegF(env);
         addInstr(env, s390_insn_move(8, *dst_hi, f13));
         addInstr(env, s390_insn_move(8, *dst_lo, f15));
         return;
      }

      case Iop_SignificanceRoundD128: {
         /* Process 1st operand */
         HReg op1 = s390_isel_int_expr(env, left);
         /* Process 2nd operand */
         s390_isel_dfp128_expr(&op2_hi, &op2_lo, env, right);
         /* 2nd operand --> (f12, f14) */
         addInstr(env, s390_insn_move(8, f12, op2_hi));
         addInstr(env, s390_insn_move(8, f14, op2_lo));

         rounding_mode = get_dfp_rounding_mode(env, irrm);
         addInstr(env, s390_insn_dfp128_reround(16, f13, f15, op1, f12, f14,
                                                rounding_mode));
         /* Move result to virtual destination register */
         *dst_hi = newVRegF(env);
         *dst_lo = newVRegF(env);
         addInstr(env, s390_insn_move(8, *dst_hi, f13));
         addInstr(env, s390_insn_move(8, *dst_lo, f15));
         return;
      }

      default:
         goto irreducible;
      }
   }

      /* --------- BINARY OP --------- */
   case Iex_Binop: {

      switch (expr->Iex.Binop.op) {
      case Iop_D64HLtoD128:
         *dst_hi = s390_isel_dfp_expr(env, expr->Iex.Binop.arg1);
         *dst_lo = s390_isel_dfp_expr(env, expr->Iex.Binop.arg2);
         return;

      case Iop_ShlD128:
      case Iop_ShrD128:
      case Iop_InsertExpD128: {
         HReg op1_hi, op1_lo, op2, f9, f11, f13, f15;
         s390_dfp_intop_t intop;
         IRExpr *dfp_op;
         IRExpr *int_op;

         switch (expr->Iex.Binop.op) {
         case Iop_ShlD128:       /* (D128, I64) -> D128 */
            intop = S390_DFP_SHIFT_LEFT;
            dfp_op = expr->Iex.Binop.arg1;
            int_op = expr->Iex.Binop.arg2;
            break;
         case Iop_ShrD128:       /* (D128, I64) -> D128 */
            intop = S390_DFP_SHIFT_RIGHT;
            dfp_op = expr->Iex.Binop.arg1;
            int_op = expr->Iex.Binop.arg2;
            break;
         case Iop_InsertExpD128: /* (I64, D128) -> D128 */
            intop = S390_DFP_INSERT_EXP;
            int_op = expr->Iex.Binop.arg1;
            dfp_op = expr->Iex.Binop.arg2;
            break;
         default: goto irreducible;
         }

         /* We use non-virtual registers as pairs (f9, f11) and (f13, f15)) */
         f9  = make_fpr(9); /* 128 bit dfp operand */
         f11 = make_fpr(11);

         f13 = make_fpr(13); /* 128 bit dfp destination */
         f15 = make_fpr(15);

         /* Process dfp operand */
         s390_isel_dfp128_expr(&op1_hi, &op1_lo, env, dfp_op);
         /* op1 -> (f9,f11) */
         addInstr(env, s390_insn_move(8, f9,  op1_hi));
         addInstr(env, s390_insn_move(8, f11, op1_lo));

         op2 = s390_isel_int_expr(env, int_op);  /* int operand */

         addInstr(env,
                  s390_insn_dfp128_intop(16, intop, f13, f15, op2, f9, f11));

         /* Move result to virtual destination register */
         *dst_hi = newVRegF(env);
         *dst_lo = newVRegF(env);
         addInstr(env, s390_insn_move(8, *dst_hi, f13));
         addInstr(env, s390_insn_move(8, *dst_lo, f15));
         return;
      }

      case Iop_F32toD128:
      case Iop_F64toD128: {
         IRExpr *irrm;
         IRExpr *left;
         s390_dfp_round_t rm;
         HReg h1; /* virtual reg. to hold source */
         HReg f0, f2, f4, r1; /* real registers used by PFPO */
         s390_fp_conv_t fpconv;

         switch (expr->Iex.Binop.op) {
         case Iop_F32toD128:       /* (D128, I64) -> D128 */
            fpconv = S390_FP_F32_TO_D128;
            break;
         case Iop_F64toD128:       /* (D128, I64) -> D128 */
            fpconv = S390_FP_F64_TO_D128;
            break;
         default: goto irreducible;
         }

         f4 = make_fpr(4); /* source */
         f0 = make_fpr(0); /* destination */
         f2 = make_fpr(2); /* destination */
         r1 = make_gpr(1); /* GPR #1 clobbered */
         irrm = expr->Iex.Binop.arg1;
         left = expr->Iex.Binop.arg2;
         rm = get_dfp_rounding_mode(env, irrm);
         h1 = s390_isel_float_expr(env, left);
         addInstr(env, s390_insn_move(8, f4, h1));
         addInstr(env, s390_insn_fp128_convert(16, fpconv, f0, f2,
                                               f4, INVALID_HREG, r1, rm));
         /* (f0, f2) --> destination */
         *dst_hi = newVRegF(env);
         *dst_lo = newVRegF(env);
         addInstr(env, s390_insn_move(8, *dst_hi, f0));
         addInstr(env, s390_insn_move(8, *dst_lo, f2));

         return;
      }

      case Iop_F128toD128: {
         IRExpr *irrm;
         IRExpr *left;
         s390_dfp_round_t rm;
         HReg op_hi, op_lo;
         HReg f0, f2, f4, f6, r1; /* real registers used by PFPO */

         f4 = make_fpr(4); /* source */
         f6 = make_fpr(6); /* source */
         f0 = make_fpr(0); /* destination */
         f2 = make_fpr(2); /* destination */
         r1 = make_gpr(1); /* GPR #1 clobbered */

         irrm = expr->Iex.Binop.arg1;
         left = expr->Iex.Binop.arg2;
         rm = get_dfp_rounding_mode(env, irrm);
         s390_isel_float128_expr(&op_hi, &op_lo, env, left);
         /* operand --> (f4, f6) */
         addInstr(env, s390_insn_move(8, f4, op_hi));
         addInstr(env, s390_insn_move(8, f6, op_lo));
         addInstr(env, s390_insn_fp128_convert(16, S390_FP_F128_TO_D128, f0, f2,
                                               f4, f6, r1, rm));
         /* (f0, f2) --> destination */
         *dst_hi = newVRegF(env);
         *dst_lo = newVRegF(env);
         addInstr(env, s390_insn_move(8, *dst_hi, f0));
         addInstr(env, s390_insn_move(8, *dst_lo, f2));

         return;
      }

      default:
         goto irreducible;
      }
   }

      /* --------- UNARY OP --------- */
   case Iex_Unop: {
      IRExpr *left = expr->Iex.Unop.arg;
      s390_dfp_conv_t conv;
      HReg op, f12, f14;

      /* We use non-virtual registers as pairs (f12, f14)) */
      f12 = make_fpr(12);
      f14 = make_fpr(14);

      switch (expr->Iex.Unop.op) {
      case Iop_D64toD128:   conv = S390_DFP_D64_TO_D128;  goto convert_dfp;
      case Iop_I32StoD128:  conv = S390_DFP_I32_TO_D128;  goto convert_int;
      case Iop_I64StoD128:  conv = S390_DFP_I64_TO_D128;  goto convert_int;
      case Iop_I32UtoD128:  conv = S390_DFP_U32_TO_D128;  goto convert_int;
      case Iop_I64UtoD128:  conv = S390_DFP_U64_TO_D128;  goto convert_int;
      default:
         goto irreducible;
      }

   convert_dfp:
      op  = s390_isel_dfp_expr(env, left);
      addInstr(env, s390_insn_dfp128_convert_to(16, conv, f12, f14, op));
      goto move_dst;

   convert_int:
      op  = s390_isel_int_expr(env, left);
      addInstr(env, s390_insn_dfp128_convert_to(16, conv, f12, f14, op));
      goto move_dst;

   move_dst:
      /* Move result to virtual destination registers */
      *dst_hi = newVRegF(env);
      *dst_lo = newVRegF(env);
      addInstr(env, s390_insn_move(8, *dst_hi, f12));
      addInstr(env, s390_insn_move(8, *dst_lo, f14));
      return;
   }

   default:
      goto irreducible;
   }

   /* We get here if no pattern matched. */
 irreducible:
   ppIRExpr(expr);
   vpanic("s390_isel_dfp128_expr_wrk: cannot reduce tree");

}


/* Compute a 128-bit value into two 64-bit registers. These may be either
   real or virtual regs; in any case they must not be changed by subsequent
   code emitted by the caller. */
static void
s390_isel_dfp128_expr(HReg *dst_hi, HReg *dst_lo, ISelEnv *env, IRExpr *expr)
{
   s390_isel_dfp128_expr_wrk(dst_hi, dst_lo, env, expr);

   /* Sanity checks ... */
   vassert(hregIsVirtual(*dst_hi));
   vassert(hregIsVirtual(*dst_lo));
   vassert(hregClass(*dst_hi) == HRcFlt64);
   vassert(hregClass(*dst_lo) == HRcFlt64);
}


/*---------------------------------------------------------*/
/*--- ISEL: Decimal point expressions (64 bit)          ---*/
/*---------------------------------------------------------*/

static HReg
s390_isel_dfp_expr_wrk(ISelEnv *env, IRExpr *expr)
{
   IRType ty = typeOfIRExpr(env->type_env, expr);
   UChar size;

   vassert(ty == Ity_D64 || ty == Ity_D32);

   size = sizeofIRType(ty);

   switch (expr->tag) {
   case Iex_RdTmp:
      /* Return the virtual register that holds the temporary. */
      return lookupIRTemp(env, expr->Iex.RdTmp.tmp);

      /* --------- LOAD --------- */
   case Iex_Load: {
      HReg        dst = newVRegF(env);
      s390_amode *am  = s390_isel_amode(env, expr->Iex.Load.addr);

      if (expr->Iex.Load.end != Iend_BE)
         goto irreducible;

      addInstr(env, s390_insn_load(size, dst, am));

      return dst;
   }

      /* --------- GET --------- */
   case Iex_Get: {
      HReg dst = newVRegF(env);
      s390_amode *am = s390_amode_for_guest_state(expr->Iex.Get.offset);

      addInstr(env, s390_insn_load(size, dst, am));

      return dst;
   }

      /* --------- BINARY OP --------- */
   case Iex_Binop: {
      IROp    op   = expr->Iex.Binop.op;
      IRExpr *irrm = expr->Iex.Binop.arg1;
      IRExpr *left = expr->Iex.Binop.arg2;
      HReg h1, dst;
      s390_dfp_conv_t  conv;
      s390_fp_conv_t  fpconv;

      switch (op) {
      case Iop_D64toD32:  conv = S390_DFP_D64_TO_D32; goto convert_dfp;
      case Iop_I64StoD64: conv = S390_DFP_I64_TO_D64; goto convert_int;
      case Iop_I64UtoD64: conv = S390_DFP_U64_TO_D64; goto convert_int;
      case Iop_F32toD32:  fpconv = S390_FP_F32_TO_D32; goto convert_bfp;
      case Iop_F32toD64:  fpconv = S390_FP_F32_TO_D64; goto convert_bfp;
      case Iop_F64toD32:  fpconv = S390_FP_F64_TO_D32; goto convert_bfp;
      case Iop_F64toD64:  fpconv = S390_FP_F64_TO_D64; goto convert_bfp;
      case Iop_F128toD32: fpconv = S390_FP_F128_TO_D32; goto convert_bfp128;
      case Iop_F128toD64: fpconv = S390_FP_F128_TO_D64; goto convert_bfp128;

      convert_dfp:
         h1 = s390_isel_dfp_expr(env, left);
         goto convert;

      convert_int:
         h1 = s390_isel_int_expr(env, left);
         goto convert;

      convert: {
            s390_dfp_round_t rounding_mode;
            /* convert-from-fixed and load-rounded have a rounding mode field
               when the floating point extension facility is installed. */
            dst = newVRegF(env);
            if (s390_host_has_fpext) {
               rounding_mode = get_dfp_rounding_mode(env, irrm);
            } else {
               set_dfp_rounding_mode_in_fpc(env, irrm);
               rounding_mode = S390_DFP_ROUND_PER_FPC_0;
            }
            addInstr(env, s390_insn_dfp_convert(size, conv, dst, h1,
                                                rounding_mode));
            return dst;
         }

      convert_bfp: {
         s390_dfp_round_t rm;
         HReg f0, f4, r1; /* real registers used by PFPO */

         f4 = make_fpr(4); /* source */
         f0 = make_fpr(0); /* destination */
         r1 = make_gpr(1); /* GPR #1 clobbered */
         h1 = s390_isel_float_expr(env, left);
         dst = newVRegF(env);
         rm = get_dfp_rounding_mode(env, irrm);
         /* operand --> f4 */
         addInstr(env, s390_insn_move(8, f4, h1));
         addInstr(env, s390_insn_fp_convert(size, fpconv, f0, f4, r1, rm));
         /* f0 --> destination */
         addInstr(env, s390_insn_move(8, dst, f0));
         return dst;
      }

      convert_bfp128: {
         s390_dfp_round_t rm;
         HReg op_hi, op_lo;
         HReg f0, f4, f6, r1; /* real registers used by PFPO */

         f4 = make_fpr(4); /* source */
         f6 = make_fpr(6); /* source */
         f0 = make_fpr(0); /* destination */
         r1 = make_gpr(1); /* GPR #1 clobbered */
         s390_isel_float128_expr(&op_hi, &op_lo, env, left);
         dst = newVRegF(env);
         rm = get_dfp_rounding_mode(env, irrm);
         /* operand --> (f4, f6) */
         addInstr(env, s390_insn_move(8, f4, op_hi));
         addInstr(env, s390_insn_move(8, f6, op_lo));
         addInstr(env, s390_insn_fp128_convert(16, fpconv, f0, INVALID_HREG,
                                               f4, f6, r1, rm));
         /* f0 --> destination */
         addInstr(env, s390_insn_move(8, dst, f0));
         return dst;
      }

      case Iop_D128toD64: {
         HReg op_hi, op_lo, f12, f13, f14, f15;
         s390_dfp_round_t rounding_mode;

         conv = S390_DFP_D128_TO_D64;

         s390_isel_dfp128_expr(&op_hi, &op_lo, env, left);

         /* We use non-virtual registers as pairs (f13, f15) and (f12, f14) */
         f12 = make_fpr(12);
         f13 = make_fpr(13);
         f14 = make_fpr(14);
         f15 = make_fpr(15);

         /* operand --> (f13, f15) */
         addInstr(env, s390_insn_move(8, f13, op_hi));
         addInstr(env, s390_insn_move(8, f15, op_lo));

         /* result --> (f12, f14) */
 
         /* load-rounded has a rounding mode field when the floating point
            extension facility is installed. */
         if (s390_host_has_fpext) {
            rounding_mode = get_dfp_rounding_mode(env, irrm);
         } else {
            set_dfp_rounding_mode_in_fpc(env, irrm);
            rounding_mode = S390_DFP_ROUND_PER_FPC_0;
         }
         addInstr(env, s390_insn_dfp128_convert_from(size, conv, f12, f14,
                                                     f13, f15, rounding_mode));
         dst = newVRegF(env);
         addInstr(env, s390_insn_move(8, dst, f12));

         return dst;
      }

      case Iop_ShlD64:
      case Iop_ShrD64:
      case Iop_InsertExpD64: {
         HReg op2;
         HReg op3;
         IRExpr *dfp_op;
         IRExpr *int_op;
         s390_dfp_intop_t intop;

         switch (expr->Iex.Binop.op) {
         case Iop_ShlD64:       /* (D64, I64) -> D64 */
            intop = S390_DFP_SHIFT_LEFT;
            dfp_op = expr->Iex.Binop.arg1;
            int_op = expr->Iex.Binop.arg2;
            break;
         case Iop_ShrD64:       /* (D64, I64) -> D64 */
            intop = S390_DFP_SHIFT_RIGHT;
            dfp_op = expr->Iex.Binop.arg1;
            int_op = expr->Iex.Binop.arg2;
            break;
         case Iop_InsertExpD64: /* (I64, D64) -> D64 */
            intop = S390_DFP_INSERT_EXP;
            int_op = expr->Iex.Binop.arg1;
            dfp_op = expr->Iex.Binop.arg2;
            break;
         default: goto irreducible;
         }

         op2 = s390_isel_int_expr(env, int_op);
         op3 = s390_isel_dfp_expr(env, dfp_op);
         dst = newVRegF(env);

         addInstr(env, s390_insn_dfp_intop(size, intop, dst, op2, op3));
         return dst;
      }

      default:
         goto irreducible;
      }
   }

      /* --------- UNARY OP --------- */
   case Iex_Unop: {
      IROp    op   = expr->Iex.Unop.op;
      IRExpr *left = expr->Iex.Unop.arg;
      s390_dfp_conv_t conv;
      HReg h1, dst;

      if (op == Iop_D128HItoD64 || op == Iop_D128LOtoD64) {
         HReg dst_hi, dst_lo;

         s390_isel_dfp128_expr(&dst_hi, &dst_lo, env, left);
         return op == Iop_D128LOtoD64 ? dst_lo : dst_hi;
      }

      if (op == Iop_ReinterpI64asD64) {
         dst = newVRegF(env);
         h1  = s390_isel_int_expr(env, left);     /* Process the operand */
         addInstr(env, s390_insn_move(size, dst, h1));

         return dst;
      }

      switch (op) {
      case Iop_D32toD64:  conv = S390_DFP_D32_TO_D64;  goto convert_dfp1;
      case Iop_I32StoD64: conv = S390_DFP_I32_TO_D64;  goto convert_int1;
      case Iop_I32UtoD64: conv = S390_DFP_U32_TO_D64;  goto convert_int1;

      convert_dfp1:
         h1 = s390_isel_dfp_expr(env, left);
         goto convert1;

      convert_int1:
         h1 = s390_isel_int_expr(env, left);
         goto convert1;

      convert1:
         dst = newVRegF(env);
         /* No rounding mode is needed for these conversions. Just stick
            one in. It won't be used later on. */
         addInstr(env, s390_insn_dfp_convert(size, conv, dst, h1,
                                             S390_DFP_ROUND_NEAREST_EVEN_4));
         return dst;

      default:
         goto irreducible;
      }
   }

      /* --------- TERNARY OP --------- */
   case Iex_Triop: {
      IRTriop *triop = expr->Iex.Triop.details;
      IROp    op     = triop->op;
      IRExpr *irrm   = triop->arg1;
      IRExpr *left   = triop->arg2;
      IRExpr *right  = triop->arg3;
      s390_dfp_round_t rounding_mode;
      s390_dfp_binop_t dfpop;
      HReg op2, op3, dst;

      switch (op) {
      case Iop_AddD64:      dfpop = S390_DFP_ADD;      goto evaluate_dfp;
      case Iop_SubD64:      dfpop = S390_DFP_SUB;      goto evaluate_dfp;
      case Iop_MulD64:      dfpop = S390_DFP_MUL;      goto evaluate_dfp;
      case Iop_DivD64:      dfpop = S390_DFP_DIV;      goto evaluate_dfp;
      case Iop_QuantizeD64: dfpop = S390_DFP_QUANTIZE; goto evaluate_dfp;

      evaluate_dfp: {
         op2  = s390_isel_dfp_expr(env, left);  /* Process 1st operand */
         op3  = s390_isel_dfp_expr(env, right); /* Process 2nd operand */
         dst  = newVRegF(env);
         /* DFP arithmetic ops take rounding mode only when fpext is
            installed. But, DFP quantize operation takes rm irrespective
            of fpext facility . */
         if (s390_host_has_fpext || dfpop == S390_DFP_QUANTIZE) {
            rounding_mode = get_dfp_rounding_mode(env, irrm);
         } else {
            set_dfp_rounding_mode_in_fpc(env, irrm);
            rounding_mode = S390_DFP_ROUND_PER_FPC_0;
         }
         addInstr(env, s390_insn_dfp_binop(size, dfpop, dst, op2, op3,
                                           rounding_mode));
         return dst;
      }

      case Iop_SignificanceRoundD64:
         op2  = s390_isel_int_expr(env, left);  /* Process 1st operand */
         op3  = s390_isel_dfp_expr(env, right); /* Process 2nd operand */
         dst  = newVRegF(env);
         rounding_mode = get_dfp_rounding_mode(env, irrm);
         addInstr(env, s390_insn_dfp_reround(size, dst, op2, op3,
                                             rounding_mode));
         return dst;

      default:
         goto irreducible;
      }
   }

   default:
      goto irreducible;
   }

   /* We get here if no pattern matched. */
 irreducible:
   ppIRExpr(expr);
   vpanic("s390_isel_dfp_expr: cannot reduce tree");
}

static HReg
s390_isel_dfp_expr(ISelEnv *env, IRExpr *expr)
{
   HReg dst = s390_isel_dfp_expr_wrk(env, expr);

   /* Sanity checks ... */
   vassert(hregClass(dst) == HRcFlt64);
   vassert(hregIsVirtual(dst));

   return dst;
}


/*---------------------------------------------------------*/
/*--- ISEL: Condition Code                              ---*/
/*---------------------------------------------------------*/

/* This function handles all operators that produce a 1-bit result */
static s390_cc_t
s390_isel_cc(ISelEnv *env, IRExpr *cond)
{
   UChar size;

   vassert(typeOfIRExpr(env->type_env, cond) == Ity_I1);

   /* Constant: either 1 or 0 */
   if (cond->tag == Iex_Const) {
      vassert(cond->Iex.Const.con->tag == Ico_U1);
      vassert(cond->Iex.Const.con->Ico.U1 == True
              || cond->Iex.Const.con->Ico.U1 == False);

      return cond->Iex.Const.con->Ico.U1 == True ? S390_CC_ALWAYS : S390_CC_NEVER;
   }

   /* Variable: values are 1 or 0 */
   if (cond->tag == Iex_RdTmp) {
      IRTemp tmp = cond->Iex.RdTmp.tmp;
      HReg   reg = lookupIRTemp(env, tmp);

      /* Load-and-test does not modify REG; so this is OK. */
      if (typeOfIRTemp(env->type_env, tmp) == Ity_I1)
         size = 4;
      else
         size = sizeofIRType(typeOfIRTemp(env->type_env, tmp));
      addInstr(env, s390_insn_test(size, s390_opnd_reg(reg)));
      return S390_CC_NE;
   }

   /* Unary operators */
   if (cond->tag == Iex_Unop) {
      IRExpr *arg = cond->Iex.Unop.arg;

      switch (cond->Iex.Unop.op) {
      case Iop_Not1:  /* Not1(cond) */
         /* Generate code for EXPR, and negate the test condition */
         return s390_cc_invert(s390_isel_cc(env, arg));

         /* Iop_32/64to1  select the LSB from their operand */
      case Iop_32to1:
      case Iop_64to1: {
         HReg dst = newVRegI(env);
         HReg h1  = s390_isel_int_expr(env, arg);

         size = sizeofIRType(typeOfIRExpr(env->type_env, arg));

         addInstr(env, s390_insn_move(size, dst, h1));
         addInstr(env, s390_insn_alu(size, S390_ALU_AND, dst, s390_opnd_imm(1)));
         addInstr(env, s390_insn_test(size, s390_opnd_reg(dst)));
         return S390_CC_NE;
      }

      case Iop_CmpNEZ8:
      case Iop_CmpNEZ16: {
         s390_opnd_RMI src;
         s390_unop_t   op;
         HReg dst;

         op  = (cond->Iex.Unop.op == Iop_CmpNEZ8) ? S390_ZERO_EXTEND_8
            : S390_ZERO_EXTEND_16;
         dst = newVRegI(env);
         src = s390_isel_int_expr_RMI(env, arg);
         addInstr(env, s390_insn_unop(4, op, dst, src));
         addInstr(env, s390_insn_test(4, s390_opnd_reg(dst)));
         return S390_CC_NE;
      }

      case Iop_CmpNEZ32:
      case Iop_CmpNEZ64: {
         s390_opnd_RMI src;

         src = s390_isel_int_expr_RMI(env, arg);
         size = sizeofIRType(typeOfIRExpr(env->type_env, arg));
         addInstr(env, s390_insn_test(size, src));
         return S390_CC_NE;
      }

      default:
         goto fail;
      }
   }

   /* Binary operators */
   if (cond->tag == Iex_Binop) {
      IRExpr *arg1 = cond->Iex.Binop.arg1;
      IRExpr *arg2 = cond->Iex.Binop.arg2;
      HReg reg1, reg2;

      size = sizeofIRType(typeOfIRExpr(env->type_env, arg1));

      switch (cond->Iex.Binop.op) {
         s390_unop_t op;
         s390_cc_t   result;

      case Iop_CmpEQ8:
      case Iop_CasCmpEQ8:
         op     = S390_ZERO_EXTEND_8;
         result = S390_CC_E;
         goto do_compare_ze;

      case Iop_CmpNE8:
      case Iop_CasCmpNE8:
         op     = S390_ZERO_EXTEND_8;
         result = S390_CC_NE;
         goto do_compare_ze;

      case Iop_CmpEQ16:
      case Iop_CasCmpEQ16:
         op     = S390_ZERO_EXTEND_16;
         result = S390_CC_E;
         goto do_compare_ze;

      case Iop_CmpNE16:
      case Iop_CasCmpNE16:
         op     = S390_ZERO_EXTEND_16;
         result = S390_CC_NE;
         goto do_compare_ze;

      do_compare_ze: {
            s390_opnd_RMI op1, op2;

            op1  = s390_isel_int_expr_RMI(env, arg1);
            reg1 = newVRegI(env);
            addInstr(env, s390_insn_unop(4, op, reg1, op1));

            op2  = s390_isel_int_expr_RMI(env, arg2);
            reg2 = newVRegI(env);
            addInstr(env, s390_insn_unop(4, op, reg2, op2));  /* zero extend */

            op2 = s390_opnd_reg(reg2);
            addInstr(env, s390_insn_compare(4, reg1, op2, False));

            return result;
         }

      case Iop_CmpEQ32:
      case Iop_CmpEQ64:
      case Iop_CasCmpEQ32:
      case Iop_CasCmpEQ64:
         result = S390_CC_E;
         goto do_compare;

      case Iop_CmpNE32:
      case Iop_CmpNE64:
      case Iop_CasCmpNE32:
      case Iop_CasCmpNE64:
         result = S390_CC_NE;
         goto do_compare;

      do_compare: {
            HReg op1;
            s390_opnd_RMI op2;

            order_commutative_operands(arg1, arg2);

            op1 = s390_isel_int_expr(env, arg1);
            op2 = s390_isel_int_expr_RMI(env, arg2);

            addInstr(env, s390_insn_compare(size, op1, op2, False));

            return result;
         }

      case Iop_CmpLT32S:
      case Iop_CmpLE32S:
      case Iop_CmpLT64S:
      case Iop_CmpLE64S: {
         HReg op1;
         s390_opnd_RMI op2;

         op1 = s390_isel_int_expr(env, arg1);
         op2 = s390_isel_int_expr_RMI(env, arg2);

         addInstr(env, s390_insn_compare(size, op1, op2, True));

         return (cond->Iex.Binop.op == Iop_CmpLT32S ||
                 cond->Iex.Binop.op == Iop_CmpLT64S) ? S390_CC_L : S390_CC_LE;
      }

      case Iop_CmpLT32U:
      case Iop_CmpLE32U:
      case Iop_CmpLT64U:
      case Iop_CmpLE64U: {
         HReg op1;
         s390_opnd_RMI op2;

         op1 = s390_isel_int_expr(env, arg1);
         op2 = s390_isel_int_expr_RMI(env, arg2);

         addInstr(env, s390_insn_compare(size, op1, op2, False));

         return (cond->Iex.Binop.op == Iop_CmpLT32U ||
                 cond->Iex.Binop.op == Iop_CmpLT64U) ? S390_CC_L : S390_CC_LE;
      }

      default:
         goto fail;
      }
   }

 fail:
   ppIRExpr(cond);
   vpanic("s390_isel_cc: unexpected operator");
}


/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void
s390_isel_stmt(ISelEnv *env, IRStmt *stmt)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n -- ");
      ppIRStmt(stmt);
      vex_printf("\n");
   }

   switch (stmt->tag) {

      /* --------- STORE --------- */
   case Ist_Store: {
      IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);
      s390_amode *am;
      HReg src;

      if (stmt->Ist.Store.end != Iend_BE) goto stmt_fail;

      am = s390_isel_amode(env, stmt->Ist.Store.addr);

      switch (tyd) {
      case Ity_I8:
      case Ity_I16:
      case Ity_I32:
      case Ity_I64:
         /* fixs390: We could check for INSN_MADD here. */
         if (am->tag == S390_AMODE_B12 &&
             stmt->Ist.Store.data->tag == Iex_Const) {
            ULong value =
               get_const_value_as_ulong(stmt->Ist.Store.data->Iex.Const.con);
            addInstr(env, s390_insn_mimm(sizeofIRType(tyd), am, value));
            return;
         }
         /* Check whether we can use a memcpy here. Currently, the restriction
            is that both amodes need to be B12, so MVC can be emitted.
            We do not consider a store whose data expression is a load because
            we don't want to deal with overlapping locations. */
         /* store(get) never overlaps*/
         if (am->tag == S390_AMODE_B12 &&
             stmt->Ist.Store.data->tag == Iex_Get) {
            UInt offset = stmt->Ist.Store.data->Iex.Get.offset;
            s390_amode *from = s390_amode_for_guest_state(offset);
            addInstr(env, s390_insn_memcpy(sizeofIRType(tyd), am, from));
            return;
         }
         /* General case: compile data into a register */
         src = s390_isel_int_expr(env, stmt->Ist.Store.data);
         break;

      case Ity_F32:
      case Ity_F64:
         src = s390_isel_float_expr(env, stmt->Ist.Store.data);
         break;

      case Ity_D32:
      case Ity_D64:
         src = s390_isel_dfp_expr(env, stmt->Ist.Store.data);
         break;

      case Ity_F128:
      case Ity_D128:
         /* Cannot occur. No such instruction */
         vpanic("Ist_Store with 128-bit floating point data");

      default:
         goto stmt_fail;
      }

      addInstr(env, s390_insn_store(sizeofIRType(tyd), am, src));
      return;
   }

      /* --------- PUT --------- */
   case Ist_Put: {
      IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
      HReg src;
      s390_amode *am;
      ULong new_value, old_value, difference;

      /* Detect updates to certain guest registers. We track the contents
         of those registers as long as they contain constants. If the new
         constant is either zero or in the 8-bit neighbourhood of the
         current value we can use a memory-to-memory insn to do the update. */

      Int offset = stmt->Ist.Put.offset;

      /* Check necessary conditions:
         (1) must be one of the registers we care about
         (2) assigned value must be a constant */
      Int guest_reg = get_guest_reg(offset);

      if (guest_reg == GUEST_UNKNOWN) goto not_special;

      if (stmt->Ist.Put.data->tag != Iex_Const) {
         /* Invalidate guest register contents */
         env->old_value_valid[guest_reg] = False;
         goto not_special;
      }

      /* We can only handle Ity_I64, but the CC_DEPS field can have floats */
      if (tyd != Ity_I64)
         goto not_special;

      /* OK. Necessary conditions are satisfied. */

      old_value = env->old_value[guest_reg];
      new_value = stmt->Ist.Put.data->Iex.Const.con->Ico.U64;
      env->old_value[guest_reg] = new_value;

      Bool old_value_is_valid = env->old_value_valid[guest_reg];
      env->old_value_valid[guest_reg] = True;

      /* If the register already contains the new value, there is nothing
         to do here. */
      if (old_value_is_valid && new_value == old_value) {
         return;
      }

      if (old_value_is_valid == False) goto not_special;

      /* If the new value is in the neighbourhood of the old value
         we can use a memory-to-memory insn */
      difference = new_value - old_value;

      if (s390_host_has_gie && ulong_fits_signed_8bit(difference)) {
         am = s390_amode_for_guest_state(offset);
         addInstr(env, s390_insn_madd(sizeofIRType(tyd), am,
                                      (difference & 0xFF), new_value));
         return;
      }

      /* If the high word is the same it is sufficient to load the low word. */
      if ((old_value >> 32) == (new_value >> 32)) {
         am = s390_amode_for_guest_state(offset + 4);
         addInstr(env, s390_insn_mimm(4, am, new_value & 0xFFFFFFFF));
         return;
      }

      /* No special case applies... fall through */

   not_special:
      am = s390_amode_for_guest_state(offset);

      switch (tyd) {
      case Ity_I8:
      case Ity_I16:
      case Ity_I32:
      case Ity_I64:
         if (am->tag == S390_AMODE_B12 &&
             stmt->Ist.Put.data->tag == Iex_Const) {
            ULong value =
               get_const_value_as_ulong(stmt->Ist.Put.data->Iex.Const.con);
            addInstr(env, s390_insn_mimm(sizeofIRType(tyd), am, value));
            return;
         }
         /* Check whether we can use a memcpy here. Currently, the restriction
            is that both amodes need to be B12, so MVC can be emitted. */
         /* put(load) never overlaps */
         if (am->tag == S390_AMODE_B12 &&
             stmt->Ist.Put.data->tag == Iex_Load) {
            if (stmt->Ist.Put.data->Iex.Load.end != Iend_BE) goto stmt_fail;
            IRExpr *data = stmt->Ist.Put.data->Iex.Load.addr;
            s390_amode *from = s390_isel_amode(env, data);
            UInt size = sizeofIRType(tyd);

            if (from->tag == S390_AMODE_B12) {
               /* Source can be compiled into a B12 amode. */
               addInstr(env, s390_insn_memcpy(size, am, from));
               return;
            }

            src = newVRegI(env);
            addInstr(env, s390_insn_load(size, src, from));
            break;
         }
         /* put(get) */
         if (am->tag == S390_AMODE_B12 &&
             stmt->Ist.Put.data->tag == Iex_Get) {
            UInt put_offset = am->d;
            UInt get_offset = stmt->Ist.Put.data->Iex.Get.offset;
            UInt size = sizeofIRType(tyd);
            /* don't memcpy in case of overlap */
            if (put_offset + size <= get_offset ||
                get_offset + size <= put_offset) {
               s390_amode *from = s390_amode_for_guest_state(get_offset);
               addInstr(env, s390_insn_memcpy(size, am, from));
               return;
            }
            goto no_memcpy_put;
         }
         /* General case: compile data into a register */
no_memcpy_put:
         src = s390_isel_int_expr(env, stmt->Ist.Put.data);
         break;

      case Ity_F32:
      case Ity_F64:
         src = s390_isel_float_expr(env, stmt->Ist.Put.data);
         break;

      case Ity_F128:
      case Ity_D128:
         /* Does not occur. See function put_(f|d)pr_pair. */
         vpanic("Ist_Put with 128-bit floating point data");

      case Ity_D32:
      case Ity_D64:
         src = s390_isel_dfp_expr(env, stmt->Ist.Put.data);
         break;

      default:
         goto stmt_fail;
      }

      addInstr(env, s390_insn_store(sizeofIRType(tyd), am, src));
      return;
   }

      /* --------- TMP --------- */
   case Ist_WrTmp: {
      IRTemp tmp = stmt->Ist.WrTmp.tmp;
      IRType tyd = typeOfIRTemp(env->type_env, tmp);
      HReg src, dst;

      switch (tyd) {
      case Ity_I128: {
         HReg dst_hi, dst_lo, res_hi, res_lo;

         s390_isel_int128_expr(&res_hi, &res_lo, env, stmt->Ist.WrTmp.data);
         lookupIRTemp128(&dst_hi, &dst_lo, env, tmp);

         addInstr(env, s390_insn_move(8, dst_hi, res_hi));
         addInstr(env, s390_insn_move(8, dst_lo, res_lo));
         return;
      }

      case Ity_I8:
      case Ity_I16:
      case Ity_I32:
      case Ity_I64:
         src = s390_isel_int_expr(env, stmt->Ist.WrTmp.data);
         dst = lookupIRTemp(env, tmp);
         break;

      case Ity_I1: {
         s390_cc_t cond = s390_isel_cc(env, stmt->Ist.WrTmp.data);
         dst = lookupIRTemp(env, tmp);
         addInstr(env, s390_insn_cc2bool(dst, cond));
         return;
      }

      case Ity_F32:
      case Ity_F64:
         src = s390_isel_float_expr(env, stmt->Ist.WrTmp.data);
         dst = lookupIRTemp(env, tmp);
         break;

      case Ity_F128: {
         HReg dst_hi, dst_lo, res_hi, res_lo;

         s390_isel_float128_expr(&res_hi, &res_lo, env, stmt->Ist.WrTmp.data);
         lookupIRTemp128(&dst_hi, &dst_lo, env, tmp);

         addInstr(env, s390_insn_move(8, dst_hi, res_hi));
         addInstr(env, s390_insn_move(8, dst_lo, res_lo));
         return;
      }

      case Ity_D32:
      case Ity_D64:
         src = s390_isel_dfp_expr(env, stmt->Ist.WrTmp.data);
         dst = lookupIRTemp(env, tmp);
         break;

      case Ity_D128: {
         HReg dst_hi, dst_lo, res_hi, res_lo;

         s390_isel_dfp128_expr(&res_hi, &res_lo, env, stmt->Ist.WrTmp.data);
         lookupIRTemp128(&dst_hi, &dst_lo, env, tmp);

         addInstr(env, s390_insn_move(8, dst_hi, res_hi));
         addInstr(env, s390_insn_move(8, dst_lo, res_lo));
         return;
      }

      default:
         goto stmt_fail;
      }

      addInstr(env, s390_insn_move(sizeofIRType(tyd), dst, src));
      return;
   }

      /* --------- Call to DIRTY helper --------- */
   case Ist_Dirty: {
      IRType   retty;
      IRDirty* d = stmt->Ist.Dirty.details;
      HReg dst;
      RetLoc rloc    = mk_RetLoc_INVALID();
      UInt   addToSp = 0;
      Int i;

      /* Invalidate tracked values of those guest state registers that are
         modified by this helper. */
      for (i = 0; i < d->nFxState; ++i) {
         /* JRS 1 June 2012: AFAICS, s390 guest doesn't use 'repeat'
            descriptors in guest state effect descriptions.  Hence: */
         vassert(d->fxState[i].nRepeats == 0 && d->fxState[i].repeatLen == 0);
         if ((d->fxState[i].fx == Ifx_Write || d->fxState[i].fx == Ifx_Modify)) {
            Int guest_reg = get_guest_reg(d->fxState[i].offset);
            if (guest_reg != GUEST_UNKNOWN)
               env->old_value_valid[guest_reg] = False;
         }
      }

      if (d->tmp == IRTemp_INVALID) {
         /* No return value. */
         retty = Ity_INVALID;
         doHelperCall(&addToSp, &rloc, env, d->guard,  d->cee, retty,
                      d->args);
         vassert(is_sane_RetLoc(rloc));
         vassert(rloc.pri == RLPri_None);
         vassert(addToSp == 0);

         return;
      }

      retty = typeOfIRTemp(env->type_env, d->tmp);
      if (retty == Ity_I64 || retty == Ity_I32
          || retty == Ity_I16 || retty == Ity_I8) {
         /* Move the returned value to the destination register */
         HReg ret = make_gpr(S390_REGNO_RETURN_VALUE);

         dst = lookupIRTemp(env, d->tmp);
         doHelperCall(&addToSp, &rloc, env, d->guard,  d->cee, retty,
                      d->args);
         vassert(is_sane_RetLoc(rloc));
         vassert(rloc.pri == RLPri_Int);
         vassert(addToSp == 0);
         addInstr(env, s390_insn_move(sizeof(ULong), dst, ret));

         return;
      }
      if (retty == Ity_V128) {
         /* we do not handle vector types yet */
         vassert(0);
         HReg sp = make_gpr(S390_REGNO_STACK_POINTER);
         s390_amode *am;

         dst = lookupIRTemp(env, d->tmp);
         doHelperCall(&addToSp, &rloc, env, d->guard,  d->cee, retty,
                      d->args);
         vassert(is_sane_RetLoc(rloc));
         vassert(rloc.pri == RLPri_V128SpRel);
         vassert(addToSp >= 16);

         /* rloc.spOff should be zero for s390 */
         /* cannot use fits_unsigned_12bit(rloc.spOff), so doing
            it explicitly */
         vassert((rloc.spOff & 0xFFF) == rloc.spOff);
         am = s390_amode_b12(rloc.spOff, sp);
         // JRS 2013-Aug-08: is this correct?  Looks like we're loading
         // only 64 bits from memory, when in fact we should be loading 128.
         addInstr(env, s390_insn_load(8, dst, am));
         addInstr(env, s390_insn_alu(4, S390_ALU_ADD, sp,
                                     s390_opnd_imm(addToSp)));
         return;
      } else {/* if (retty == Ity_V256) */
         /* we do not handle vector types yet */
         vassert(0);
      }
      break;
   }

   case Ist_CAS:
      if (stmt->Ist.CAS.details->oldHi == IRTemp_INVALID) {
         IRCAS *cas = stmt->Ist.CAS.details;
         s390_amode *op2 = s390_isel_amode(env, cas->addr);
         HReg op3 = s390_isel_int_expr(env, cas->dataLo);  /* new value */
         HReg op1 = s390_isel_int_expr(env, cas->expdLo);  /* expected value */
         HReg old = lookupIRTemp(env, cas->oldLo);

         if (typeOfIRTemp(env->type_env, cas->oldLo) == Ity_I32) {
            addInstr(env, s390_insn_cas(4, op1, op2, op3, old));
         } else {
            addInstr(env, s390_insn_cas(8, op1, op2, op3, old));
         }
         return;
      } else {
         IRCAS *cas = stmt->Ist.CAS.details;
         s390_amode *op2 = s390_isel_amode(env,  cas->addr);
         HReg r8, r9, r10, r11, r1;
         HReg op3_high = s390_isel_int_expr(env, cas->dataHi);  /* new value */
         HReg op3_low  = s390_isel_int_expr(env, cas->dataLo);  /* new value */
         HReg op1_high = s390_isel_int_expr(env, cas->expdHi);  /* expected value */
         HReg op1_low  = s390_isel_int_expr(env, cas->expdLo);  /* expected value */
         HReg old_low  = lookupIRTemp(env, cas->oldLo);
         HReg old_high = lookupIRTemp(env, cas->oldHi);

         /* Use non-virtual registers r8 and r9 as pair for op1
            and move op1 there */
         r8 = make_gpr(8);
         r9 = make_gpr(9);
         addInstr(env, s390_insn_move(8, r8, op1_high));
         addInstr(env, s390_insn_move(8, r9, op1_low));

         /* Use non-virtual registers r10 and r11 as pair for op3
            and move op3 there */
         r10 = make_gpr(10);
         r11 = make_gpr(11);
         addInstr(env, s390_insn_move(8, r10, op3_high));
         addInstr(env, s390_insn_move(8, r11, op3_low));

         /* Register r1 is used as a scratch register */
         r1 = make_gpr(1);

         if (typeOfIRTemp(env->type_env, cas->oldLo) == Ity_I32) {
            addInstr(env, s390_insn_cdas(4, r8, r9, op2, r10, r11,
                                         old_high, old_low, r1));
         } else {
            addInstr(env, s390_insn_cdas(8, r8, r9, op2, r10, r11,
                                         old_high, old_low, r1));
         }
         addInstr(env, s390_insn_move(8, op1_high, r8));
         addInstr(env, s390_insn_move(8, op1_low,  r9));
         addInstr(env, s390_insn_move(8, op3_high, r10));
         addInstr(env, s390_insn_move(8, op3_low,  r11));
         return;
      }
      break;

      /* --------- EXIT --------- */
   case Ist_Exit: {
      s390_cc_t cond;
      IRConstTag tag = stmt->Ist.Exit.dst->tag;

      if (tag != Ico_U64)
         vpanic("s390_isel_stmt: Ist_Exit: dst is not a 64-bit value");

      s390_amode *guest_IA = s390_amode_for_guest_state(stmt->Ist.Exit.offsIP);
      cond = s390_isel_cc(env, stmt->Ist.Exit.guard);

      /* Case: boring transfer to known address */
      if (stmt->Ist.Exit.jk == Ijk_Boring) {
         if (env->chaining_allowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool to_fast_entry
               = ((Addr64)stmt->Ist.Exit.dst->Ico.U64) > env->max_ga;
            if (0) vex_printf("%s", to_fast_entry ? "Y" : ",");
            addInstr(env, s390_insn_xdirect(cond, stmt->Ist.Exit.dst->Ico.U64,
                                            guest_IA, to_fast_entry));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg dst = s390_isel_int_expr(env,
                                          IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, s390_insn_xassisted(cond, dst, guest_IA, Ijk_Boring));
         }
         return;
      }

      /* Case: assisted transfer to arbitrary address */
      switch (stmt->Ist.Exit.jk) {
      case Ijk_EmFail:
      case Ijk_EmWarn:
      case Ijk_NoDecode:
      case Ijk_InvalICache:
      case Ijk_Sys_syscall:
      case Ijk_ClientReq:
      case Ijk_NoRedir:
      case Ijk_Yield:
      case Ijk_SigTRAP: {
         HReg dst = s390_isel_int_expr(env, IRExpr_Const(stmt->Ist.Exit.dst));
         addInstr(env, s390_insn_xassisted(cond, dst, guest_IA,
                                           stmt->Ist.Exit.jk));
         return;
      }
      default:
         break;
      }

      /* Do we ever expect to see any other kind? */
      goto stmt_fail;
   }

      /* --------- MEM FENCE --------- */
   case Ist_MBE:
      switch (stmt->Ist.MBE.event) {
         case Imbe_Fence:
            addInstr(env, s390_insn_mfence());
            return;
         default:
            break;
      }
      break;

      /* --------- Miscellaneous --------- */

   case Ist_PutI:    /* Not needed */
   case Ist_IMark:   /* Doesn't generate any executable code */
   case Ist_NoOp:    /* Doesn't generate any executable code */
   case Ist_AbiHint: /* Meaningless in IR */
      return;

   default:
      break;
   }

 stmt_fail:
   ppIRStmt(stmt);
   vpanic("s390_isel_stmt");
}


/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

static void
iselNext(ISelEnv *env, IRExpr *next, IRJumpKind jk, Int offsIP)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- PUT(%d) = ", offsIP);
      ppIRExpr(next);
      vex_printf("; exit-");
      ppIRJumpKind(jk);
      vex_printf("\n");
   }

   s390_amode *guest_IA = s390_amode_for_guest_state(offsIP);

   /* Case: boring transfer to known address */
   if (next->tag == Iex_Const) {
      IRConst *cdst = next->Iex.Const.con;
      vassert(cdst->tag == Ico_U64);
      if (jk == Ijk_Boring || jk == Ijk_Call) {
         /* Boring transfer to known address */
         if (env->chaining_allowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool to_fast_entry
               = ((Addr64)cdst->Ico.U64) > env->max_ga;
            if (0) vex_printf("%s", to_fast_entry ? "X" : ".");
            addInstr(env, s390_insn_xdirect(S390_CC_ALWAYS, cdst->Ico.U64,
                                            guest_IA, to_fast_entry));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an indirect transfer,
               as that's the cheapest alternative that is allowable. */
            HReg dst = s390_isel_int_expr(env, next);
            addInstr(env, s390_insn_xassisted(S390_CC_ALWAYS, dst, guest_IA,
                                              Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
   case Ijk_Boring:
   case Ijk_Ret:
   case Ijk_Call: {
      HReg dst = s390_isel_int_expr(env, next);
      if (env->chaining_allowed) {
         addInstr(env, s390_insn_xindir(S390_CC_ALWAYS, dst, guest_IA));
      } else {
         addInstr(env, s390_insn_xassisted(S390_CC_ALWAYS, dst, guest_IA,
                                           Ijk_Boring));
      }
      return;
   }
   default:
      break;
   }

   /* Case: some other kind of transfer to any address */
   switch (jk) {
   case Ijk_EmFail:
   case Ijk_EmWarn:
   case Ijk_NoDecode:
   case Ijk_InvalICache:
   case Ijk_Sys_syscall:
   case Ijk_ClientReq:
   case Ijk_NoRedir:
   case Ijk_Yield:
   case Ijk_SigTRAP: {
      HReg dst = s390_isel_int_expr(env, next);
      addInstr(env, s390_insn_xassisted(S390_CC_ALWAYS, dst, guest_IA, jk));
      return;
   }
   default:
      break;
   }

   vpanic("iselNext");
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire SB to s390 code.
   Note: archinfo_host is a pointer to a stack-allocated variable.
   Do not assign it to a global variable! */

HInstrArray *
iselSB_S390(IRSB *bb, VexArch arch_host, VexArchInfo *archinfo_host,
            VexAbiInfo *vbi, Int offset_host_evcheck_counter,
            Int offset_host_evcheck_fail_addr, Bool chaining_allowed,
            Bool add_profinc, Addr64 max_ga)
{
   UInt     i, j;
   HReg     hreg, hregHI;
   ISelEnv *env;
   UInt     hwcaps_host = archinfo_host->hwcaps;

   /* KLUDGE: export hwcaps. */
   s390_host_hwcaps = hwcaps_host;

   /* Do some sanity checks */
   vassert((VEX_HWCAPS_S390X(hwcaps_host) & ~(VEX_HWCAPS_S390X_ALL)) == 0);

   /* Make up an initial environment to use. */
   env = LibVEX_Alloc(sizeof(ISelEnv));
   env->vreg_ctr = 0;

   /* Set up output code array. */
   env->code = newHInstrArray();

   /* Copy BB's type env. */
   env->type_env = bb->tyenv;

   /* Set up data structures for tracking guest register values. */
   for (i = 0; i < NUM_TRACKED_REGS; ++i) {
      env->old_value[i] = 0;  /* just something to have a defined value */
      env->old_value_valid[i] = False;
   }

   /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
      change as we go along. For some reason types_used has Int type -- but
      it should be unsigned. Internally we use an unsigned type; so we
      assert it here. */
   vassert(bb->tyenv->types_used >= 0);

   env->n_vregmap = bb->tyenv->types_used;
   env->vregmap   = LibVEX_Alloc(env->n_vregmap * sizeof(HReg));
   env->vregmapHI = LibVEX_Alloc(env->n_vregmap * sizeof(HReg));

   env->previous_bfp_rounding_mode = NULL;
   env->previous_dfp_rounding_mode = NULL;

   /* and finally ... */
   env->hwcaps    = hwcaps_host;

   env->max_ga = max_ga;
   env->chaining_allowed = chaining_allowed;

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
         hreg = mkHReg(j++, HRcInt64, True);
         break;

      case Ity_I64:
         hreg   = mkHReg(j++, HRcInt64, True);
         break;

      case Ity_I128:
         hreg   = mkHReg(j++, HRcInt64, True);
         hregHI = mkHReg(j++, HRcInt64, True);
         break;

      case Ity_F32:
      case Ity_F64:
      case Ity_D32:
      case Ity_D64:
         hreg = mkHReg(j++, HRcFlt64, True);
         break;

      case Ity_F128:
      case Ity_D128:
         hreg   = mkHReg(j++, HRcFlt64, True);
         hregHI = mkHReg(j++, HRcFlt64, True);
         break;

      case Ity_V128: /* fall through */
      default:
         ppIRType(bb->tyenv->types[i]);
         vpanic("iselSB_S390: IRTemp type");
      }

      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   s390_amode *counter, *fail_addr;
   counter   = s390_amode_for_guest_state(offset_host_evcheck_counter);
   fail_addr = s390_amode_for_guest_state(offset_host_evcheck_fail_addr);
   addInstr(env, s390_insn_evcheck(counter, fail_addr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfInc. */
   if (add_profinc) {
      addInstr(env, s390_insn_profinc());
   }

   /* Ok, finally we can iterate over the statements. */
   for (i = 0; i < bb->stmts_used; i++)
      if (bb->stmts[i])
         s390_isel_stmt(env, bb->stmts[i]);

   iselNext(env, bb->next, bb->jumpkind, bb->offsIP);

   /* Record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;

   return env->code;
}

/*---------------------------------------------------------------*/
/*--- end                                    host_s390_isel.c ---*/
/*---------------------------------------------------------------*/
