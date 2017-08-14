
/*---------------------------------------------------------------*/
/*--- begin                                host_tilegx_isel.c ---*/
/*---------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2010-2015 Tilera Corp.

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

 /* Contributed by Zhi-Gang Liu <zliu at tilera dot com> */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_tilegx_defs.h"
#include "tilegx_disasm.h"

/*---------------------------------------------------------*/
/*--- Register Usage Conventions                        ---*/
/*---------------------------------------------------------*/

/* GPR register class for tilegx */
#define HRcGPR()     HRcInt64

/* guest_COND offset. */
#define COND_OFFSET()    (608)

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
typedef struct {
  IRTypeEnv *type_env;

  HReg *vregmap;

  Int n_vregmap;

  HInstrArray *code;

  Int vreg_ctr;

  UInt hwcaps;

  Bool mode64;

  Bool   chainingAllowed;

  Addr64 max_ga;

  IRExpr *previous_rm;

  VexAbiInfo *vbi;
} ISelEnv;

static HReg lookupIRTemp ( ISelEnv * env, IRTemp tmp )
{
  vassert(tmp >= 0);
  vassert(tmp < env->n_vregmap);
  return env->vregmap[tmp];
}

static void addInstr ( ISelEnv * env, TILEGXInstr * instr )
{
  addHInstr(env->code, instr);
  if (vex_traceflags & VEX_TRACE_VCODE) {
    ppTILEGXInstr(instr);
    vex_printf("\n");
  }
}

static HReg newVRegI ( ISelEnv * env )
{
  HReg reg = mkHReg(True /*virtual R*/, HRcGPR(), 0, env->vreg_ctr);
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
/* Compute an I8/I16/I32/I64 into a RH (reg-or-halfword-immediate).
   It's important to specify whether the immediate is to be regarded
   as signed or not.  If yes, this will never return -32768 as an
   immediate; this guaranteed that all signed immediates that are
   return can have their sign inverted if need be.
*/
static TILEGXRH *iselWordExpr_RH_wrk ( ISelEnv * env, Bool syned, IRExpr * e );
static TILEGXRH *iselWordExpr_RH ( ISelEnv * env, Bool syned, IRExpr * e );

static TILEGXRH *iselWordExpr_RH6u_wrk ( ISelEnv * env, IRExpr * e );
static TILEGXRH *iselWordExpr_RH6u ( ISelEnv * env, IRExpr * e );

/* compute an I8/I16/I32/I64 into a GPR*/
static HReg iselWordExpr_R_wrk ( ISelEnv * env, IRExpr * e );
static HReg iselWordExpr_R ( ISelEnv * env, IRExpr * e );

/* compute an I64 into an AMode. */
static TILEGXAMode *iselWordExpr_AMode_wrk ( ISelEnv * env, IRExpr * e,
                                             IRType xferTy );
static TILEGXAMode *iselWordExpr_AMode ( ISelEnv * env, IRExpr * e,
                                         IRType xferTy );

static TILEGXCondCode iselCondCode_wrk ( ISelEnv * env, IRExpr * e );
static TILEGXCondCode iselCondCode ( ISelEnv * env, IRExpr * e );

/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

/* Make an int reg-reg move. */
static TILEGXInstr *mk_iMOVds_RR ( HReg r_dst, HReg r_src )
{
  vassert(hregClass(r_dst) == hregClass(r_src));
  vassert(hregClass(r_src) == HRcInt32 || hregClass(r_src) == HRcInt64);
  return TILEGXInstr_Alu(GXalu_OR, r_dst, r_src, TILEGXRH_Reg(r_src));
}

/*---------------------------------------------------------*/
/*--- ISEL: Function call helpers                       ---*/
/*---------------------------------------------------------*/

/* Used only in doHelperCall. See big comment in doHelperCall
   handling of register-parameter args.  This function figures out
   whether evaluation of an expression might require use of a fixed
   register.
*/
static Bool mightRequireFixedRegs ( IRExpr * e )
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

/* Do a complete function call.  guard is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional. */

static void doHelperCall ( ISelEnv * env, IRExpr * guard, IRCallee * cee,
                           IRExpr ** args, IRType retTy )
{
  TILEGXCondCode cc;
  HReg argregs[TILEGX_N_REGPARMS];
  HReg tmpregs[TILEGX_N_REGPARMS];
  Bool go_fast;
  Long  n_args, i, argreg;
  ULong argiregs;
  ULong target;
  HReg src = INVALID_HREG;


  UInt nVECRETs = 0;
  UInt nBBPTRs  = 0;

  /* TILEGX calling convention: up to 10 registers (r0 ... r9)
     are allowed to be used for passing integer arguments. They correspond
     to regs GPR0 ... GPR9. */

  /* Note that the cee->regparms field is meaningless on ARM64 hosts
     (since there is only one calling convention) and so we always
     ignore it. */

  n_args = 0;
  for (i = 0; args[i]; i++) {
    n_args++;
    IRExpr* arg = args[i];
    if (UNLIKELY(arg->tag == Iex_VECRET)) {
      nVECRETs++;
    } else if (UNLIKELY(arg->tag == Iex_BBPTR)) {
      nBBPTRs++;
    }
  }

  if (nVECRETs || nBBPTRs)
    vex_printf("nVECRETs=%u, nBBPTRs=%u\n",
               nVECRETs, nBBPTRs);

  if (TILEGX_N_REGPARMS < n_args) {
    vpanic("doHelperCall(TILEGX): cannot currently handle > 10 args");
  }
  argregs[0] = hregTILEGX_R0();
  argregs[1] = hregTILEGX_R1();
  argregs[2] = hregTILEGX_R2();
  argregs[3] = hregTILEGX_R3();
  argregs[4] = hregTILEGX_R4();
  argregs[5] = hregTILEGX_R5();
  argregs[6] = hregTILEGX_R6();
  argregs[7] = hregTILEGX_R7();
  argregs[8] = hregTILEGX_R8();
  argregs[9] = hregTILEGX_R9();
  argiregs = 0;

  for (i = 0; i < TILEGX_N_REGPARMS; i++)
    tmpregs[i] = INVALID_HREG;

  /* First decide which scheme (slow or fast) is to be used.  First
     assume the fast scheme, and select slow if any contraindications
     (wow) appear. */

  go_fast = True;

  if (guard) {
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
      vassert(argreg < TILEGX_N_REGPARMS);
      vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32 ||
              typeOfIRExpr(env->type_env, args[i]) == Ity_I64);

      argiregs |= (1 << (argreg));
      addInstr(env, mk_iMOVds_RR(argregs[argreg],
                                 iselWordExpr_R(env,
                                                args[i])));
      argreg++;
    }
    /* Fast scheme only applies for unconditional calls.  Hence: */
    cc = TILEGXcc_AL;
  } else {
    /* SLOW SCHEME; move via temporaries */
    argreg = 0;

    for (i = 0; i < n_args; i++) {
      vassert(argreg < TILEGX_N_REGPARMS);
      vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32
              || typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
      tmpregs[argreg] = iselWordExpr_R(env, args[i]);
      argreg++;
    }

    /* Now we can compute the condition.  We can't do it earlier
       because the argument computations could trash the condition
       codes.  Be a bit clever to handle the common case where the
       guard is 1:Bit. */
    cc = TILEGXcc_AL;
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
      if (hregIsInvalid(tmpregs[i]))  // Skip invalid regs
        continue;
      /* None of these insns, including any spill code that might
         be generated, may alter the condition codes. */
      argiregs |= (1 << (i));
      addInstr(env, mk_iMOVds_RR(argregs[i], tmpregs[i]));
    }
  }

  target = (Addr)(cee->addr);

  /* Finally, the call itself. */
  if (cc == TILEGXcc_AL)
    addInstr(env, TILEGXInstr_CallAlways(cc, target, argiregs));
  else
    addInstr(env, TILEGXInstr_Call(cc, target, argiregs, src));
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
   UInt v = u & 0xFFFF;

   v = (Int)(v << 16) >> 16;   /* sign extend */

   return u == v;
}

static Bool sane_AMode ( ISelEnv * env, TILEGXAMode * am )
{
  if (am->tag == GXam_IR)
    return toBool(hregClass(am->GXam.IR.base) == HRcGPR() &&
                  hregIsVirtual(am->GXam.IR.base) &&
                  uInt_fits_in_16_bits(am->GXam.IR.index));

  vpanic("sane_AMode: unknown tilegx amode tag");
}

static TILEGXAMode *iselWordExpr_AMode ( ISelEnv * env, IRExpr * e,
                                         IRType xferTy )
{
  TILEGXAMode *am = iselWordExpr_AMode_wrk(env, e, xferTy);
  vassert(sane_AMode(env, am));
  return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static TILEGXAMode *iselWordExpr_AMode_wrk ( ISelEnv * env, IRExpr * e,
                                             IRType xferTy )
{
  IRType ty = typeOfIRExpr(env->type_env, e);

  vassert(ty == Ity_I64);
  /* Add64(expr,i), where i == sign-extend of (i & 0xFFFF) */
  if (e->tag == Iex_Binop
      && e->Iex.Binop.op == Iop_Add64
      && e->Iex.Binop.arg2->tag == Iex_Const
      && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U64
      && uInt_fits_in_16_bits(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)) {

    return TILEGXAMode_IR((Long) e->Iex.Binop.arg2->Iex.Const.con->Ico.U64,
                          iselWordExpr_R(env, e->Iex.Binop.arg1));
  }

  /* Doesn't match anything in particular.  Generate it into
     a register and use that. */
  return TILEGXAMode_IR(0, iselWordExpr_R(env, e));
}





/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64/32/16/8 bit)        ---*/
/*---------------------------------------------------------*/

/* Select insns for an integer-typed expression, and add them to the
   code list.  Return a reg holding the result.  This reg will be a
   virtual register.  THE RETURNED REG MUST NOT BE MODIFIED.  If you
   want to modify it, ask for a new vreg, copy it in there, and modify
   the copy.  The register allocator will do its best to map both
   add vregs to the same real register, so the copies will often disappear
   later in the game.

   This should handle expressions of 64, 32, 16 and 8-bit type.
   All results are returned in a  64bit register.
*/
static HReg iselWordExpr_R ( ISelEnv * env, IRExpr * e )
{
  HReg r = iselWordExpr_R_wrk(env, e);
  /* sanity checks ... */

  vassert(hregClass(r) == HRcGPR());
  vassert(hregIsVirtual(r));
  return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselWordExpr_R_wrk ( ISelEnv * env, IRExpr * e )
{
  IRType ty = typeOfIRExpr(env->type_env, e);
  vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 ||
          ty == Ity_I1 || ty == Ity_I64);

  switch (e->tag) {
    /* --------- TEMP --------- */
  case Iex_RdTmp:
    return lookupIRTemp(env, e->Iex.RdTmp.tmp);

    /* --------- LOAD --------- */
  case Iex_Load: {
    HReg r_dst = newVRegI(env);
    TILEGXAMode *am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, ty);

    if (e->Iex.Load.end != Iend_LE
        && e->Iex.Load.end != Iend_BE)
      goto irreducible;

    addInstr(env, TILEGXInstr_Load(toUChar(sizeofIRType(ty)),
                                   r_dst, am_addr));
    return r_dst;
    break;
  }
    /* --------- BINARY OP --------- */
  case Iex_Binop: {
    TILEGXAluOp aluOp;
    TILEGXShftOp shftOp;

    switch (e->Iex.Binop.op) {

    case Iop_Add8:
    case Iop_Add16:
    case Iop_Add32:
    case Iop_Add64:
      aluOp = GXalu_ADD;
      break;

    case Iop_Sub8:
    case Iop_Sub16:
    case Iop_Sub32:
    case Iop_Sub64:
      aluOp = GXalu_SUB;
      break;

    case Iop_And8:
    case Iop_And16:
    case Iop_And32:
    case Iop_And64:
      aluOp = GXalu_AND;
      break;

    case Iop_Or8:
    case Iop_Or16:
    case Iop_Or32:
    case Iop_Or64:
      aluOp = GXalu_OR;
      break;

    case Iop_Xor8:
    case Iop_Xor16:
    case Iop_Xor32:
    case Iop_Xor64:
      aluOp = GXalu_XOR;
      break;

    default:
      aluOp = GXalu_INVALID;
      break;
    }

    /* For commutative ops we assume any literal
       values are on the second operand. */
    if (aluOp != GXalu_INVALID) {
      HReg r_dst = newVRegI(env);
      HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
      TILEGXRH *ri_srcR = NULL;
      /* get right arg into an RH, in the appropriate way */
      switch (aluOp) {
      case GXalu_ADD:
      case GXalu_SUB:
        ri_srcR = iselWordExpr_RH(env, True /*signed */ ,
                                  e->Iex.Binop.arg2);
        break;
      case GXalu_AND:
      case GXalu_OR:
      case GXalu_XOR:
        ri_srcR = iselWordExpr_RH(env, True /*signed */,
                                  e->Iex.Binop.arg2);
        break;
      default:
        vpanic("iselWordExpr_R_wrk-aluOp-arg2");
      }
      addInstr(env, TILEGXInstr_Alu(aluOp, r_dst, r_srcL, ri_srcR));
      return r_dst;
    }

    /* a shift? */
    switch (e->Iex.Binop.op) {
    case Iop_Shl32:
    case Iop_Shl64:
      shftOp = GXshft_SLL;
      break;
    case Iop_Shr32:
    case Iop_Shr64:
      shftOp = GXshft_SRL;
      break;
    case Iop_Sar64:
      shftOp = GXshft_SRA;
      break;
    case Iop_Shl8x8:
      shftOp = GXshft_SLL8x8;
      break;
    case Iop_Shr8x8:
      shftOp = GXshft_SRL8x8;
      break;
    default:
      shftOp = GXshft_INVALID;
      break;
    }

    /* we assume any literal values are on the second operand. */
    if (shftOp != GXshft_INVALID) {
      HReg r_dst = newVRegI(env);
      HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
      TILEGXRH *ri_srcR = NULL;
      /* get right arg into an RH, in the appropriate way */
      switch (shftOp) {
      case GXshft_SLL:
      case GXshft_SRL:
      case GXshft_SRA:
        //ri_srcR = iselWordExpr_RH6u(env, e->Iex.Binop.arg2);
        //break;
      case GXshft_SLL8x8:
      case GXshft_SRL8x8:
        //if (e->Iex.Binop.arg2->tag == GXrh_Imm)
        //{
        // ri_srcR = iselWordExpr_RH6u(env, e->Iex.Binop.arg2);
        //  break;
        //}
        ri_srcR = iselWordExpr_RH6u(env, e->Iex.Binop.arg2);
        break;
      default:
        vpanic("iselIntExpr_R_wrk-shftOp-arg2");
      }
      /* widen the left arg if needed */
      /*TODO do we need this? */
      if (ty == Ity_I8 || ty == Ity_I16)
        goto irreducible;
      if (ty == Ity_I64) {
        addInstr(env, TILEGXInstr_Shft(shftOp, False/*64bit shift */,
                                       r_dst, r_srcL, ri_srcR));
      } else {
        addInstr(env, TILEGXInstr_Shft(shftOp, True /*32bit shift */,
                                       r_dst, r_srcL, ri_srcR));
      }
      return r_dst;
    }

    /* Cmp*32*(x,y) ? */
    if (e->Iex.Binop.op == Iop_CasCmpEQ32
        || e->Iex.Binop.op == Iop_CmpEQ32
        || e->Iex.Binop.op == Iop_CasCmpNE32
        || e->Iex.Binop.op == Iop_CmpNE32
        || e->Iex.Binop.op == Iop_CmpNE64
        || e->Iex.Binop.op == Iop_CmpLT32S
        || e->Iex.Binop.op == Iop_CmpLT32U
        || e->Iex.Binop.op == Iop_CmpLT64U
        || e->Iex.Binop.op == Iop_CmpLE32S
        || e->Iex.Binop.op == Iop_CmpLE64S
        || e->Iex.Binop.op == Iop_CmpLE64U
        || e->Iex.Binop.op == Iop_CmpLT64S
        || e->Iex.Binop.op == Iop_CmpEQ64
        || e->Iex.Binop.op == Iop_CasCmpEQ64
        || e->Iex.Binop.op == Iop_CasCmpNE64) {

      Bool syned = (e->Iex.Binop.op == Iop_CmpLT32S
                    || e->Iex.Binop.op == Iop_CmpLE32S
                    || e->Iex.Binop.op == Iop_CmpLT64S
                    || e->Iex.Binop.op == Iop_CmpLE64S);
      Bool size32;
      HReg dst = newVRegI(env);
      HReg r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
      HReg r2 = iselWordExpr_R(env, e->Iex.Binop.arg2);
      TILEGXCondCode cc;

      switch (e->Iex.Binop.op) {
      case Iop_CasCmpEQ32:
      case Iop_CmpEQ32:
        cc = TILEGXcc_EQ;
        size32 = True;
        break;
      case Iop_CasCmpNE32:
      case Iop_CmpNE32:
        cc = TILEGXcc_NE;
        size32 = True;
        break;
      case Iop_CasCmpNE64:
      case Iop_CmpNE64:
        cc = TILEGXcc_NE;
        size32 = True;
        break;
      case Iop_CmpLT32S:
        cc = TILEGXcc_LT;
        size32 = True;
        break;
      case Iop_CmpLT32U:
        cc = TILEGXcc_LO;
        size32 = True;
        break;
      case Iop_CmpLT64U:
        cc = TILEGXcc_LT;
        size32 = False;
        break;
      case Iop_CmpLE32S:
        cc = TILEGXcc_LE;
        size32 = True;
        break;
      case Iop_CmpLE64S:
        cc = TILEGXcc_LE;
        size32 = False;
        break;
      case Iop_CmpLE64U:
        cc = TILEGXcc_LE;
        size32 = False;
        break;
      case Iop_CmpLT64S:
        cc = TILEGXcc_LT;
        size32 = False;
        break;
      case Iop_CasCmpEQ64:
      case Iop_CmpEQ64:
        cc = TILEGXcc_EQ;
        size32 = False;
        break;
      default:
        vpanic
          ("iselCondCode(tilegx): CmpXX32 or CmpXX64");
      }

      addInstr(env, TILEGXInstr_Cmp(syned, size32, dst, r1, r2, cc));
      return dst;

      break;

    }

    if (e->Iex.Binop.op == Iop_CmpEQ8x8) {

      Bool syned = False;

      Bool size32;
      HReg dst = newVRegI(env);
      HReg r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
      TILEGXRH *r2 = iselWordExpr_RH(env, True, e->Iex.Binop.arg2);
      TILEGXCondCode cc;

      switch (e->Iex.Binop.op) {
      case Iop_CmpEQ8x8:
        cc = TILEGXcc_EQ8x8;
        size32 = False;
        break;

      default:
        vassert(0);
      }

      addInstr(env, TILEGXInstr_CmpI(syned, size32, dst, r1, r2, cc));
      return dst;

      break;
    }

    if (e->Iex.Binop.op == Iop_Max32U) {
      /*
        tmp = argL - argR;
        tmp &= (1<<31)
        dst = (tmp) ? (argL) ? (argR)
      */
      HReg argL = iselWordExpr_R(env, e->Iex.Binop.arg1);
      TILEGXRH *argR = iselWordExpr_RH(env, False /*signed */ ,
                                       e->Iex.Binop.arg2);
      HReg dst = newVRegI(env);
      HReg tmp = newVRegI(env);
      // temp = argL - argR
      addInstr(env, TILEGXInstr_Alu(GXalu_SUB, tmp, argL, argR));
      // tmp &= bit31
      addInstr(env, TILEGXInstr_Bf(GXbf_EXTU, tmp, tmp , 31, 31));
      // (tmp == 0) ? (argL) : (argR)
      addInstr(env, TILEGXInstr_MovCond(dst, argL, argR, tmp, TILEGXcc_EZ));
      return dst;
    }

    if (e->Iex.Binop.op == Iop_MullS32 || e->Iex.Binop.op == Iop_MullU32) {
      Bool syned = (e->Iex.Binop.op == Iop_MullS32);
      Bool sz32 = (e->Iex.Binop.op == Iop_Mul32);
      HReg r_dst = newVRegI(env);
      HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
      HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
      addInstr(env, TILEGXInstr_Mul(syned /*Unsigned or Signed */ ,
                                    True /*widen */ ,
                                    sz32 /*32bit or 64bit */,
                                    r_dst, r_srcL, r_srcR));
      return r_dst;
    }

    if (e->Iex.Binop.op == Iop_32HLto64) {
      HReg tHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
      HReg tLo = iselWordExpr_R(env, e->Iex.Binop.arg2);
      HReg tLo_1 = newVRegI(env);
      HReg tHi_1 = newVRegI(env);
      HReg r_dst = newVRegI(env);
      HReg mask = newVRegI(env);

      addInstr(env, TILEGXInstr_Shft(GXshft_SLL, False, tHi_1, tHi,
                                     TILEGXRH_Imm(False, 32)));

      addInstr(env, TILEGXInstr_LI(mask, 0xffffffff));
      addInstr(env, TILEGXInstr_Alu(GXalu_AND, tLo_1, tLo,
                                    TILEGXRH_Reg(mask)));
      addInstr(env, TILEGXInstr_Alu(GXalu_OR, r_dst, tHi_1,
                                    TILEGXRH_Reg(tLo_1)));

      return r_dst;
    }

    /* Anything reached here !*/
    goto irreducible;
  }

    /* --------- UNARY OP --------- */
  case Iex_Unop: {

    IROp op_unop = e->Iex.Unop.op;

    switch (op_unop) {
    case Iop_Not1: {
      HReg r_dst = newVRegI(env);
      HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
      TILEGXRH *r_srcR = TILEGXRH_Reg(r_srcL);

      addInstr(env, TILEGXInstr_LI(r_dst, 0x1));
      addInstr(env, TILEGXInstr_Alu(GXalu_SUB, r_dst, r_dst, r_srcR));
      return r_dst;
    }

    case Iop_Not8:
    case Iop_Not16:
    case Iop_Not32:
    case Iop_Not64: {
      /* not x = nor x, x */
      HReg r_dst = newVRegI(env);
      HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
      TILEGXRH *r_srcR = TILEGXRH_Reg(r_srcL);

      addInstr(env, TILEGXInstr_Alu(GXalu_NOR, r_dst, r_srcL, r_srcR));
      return r_dst;
    }

    case Iop_CmpNEZ8x8: {

      Bool syned = False;
      Bool size32;
      HReg dst = newVRegI(env);
      HReg r1;
      TILEGXCondCode cc =  TILEGXcc_NE8x8;
      size32 = False;
      r1 = iselWordExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, TILEGXInstr_CmpI(syned, size32, dst, hregTILEGX_R63(),
                                     TILEGXRH_Reg(r1), cc));

      return dst;
      break;
    }

    case Iop_16to8:
    case Iop_32to8:
    case Iop_64to8:
    case Iop_32to16:
    case Iop_64to16:
    case Iop_64to32:
    case Iop_128to64:

      return iselWordExpr_R(env, e->Iex.Unop.arg);

    case Iop_1Uto64:
    case Iop_1Uto32:
    case Iop_1Uto8: {
      HReg dst = newVRegI(env);
      HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, TILEGXInstr_Alu(GXalu_AND, dst, src, TILEGXRH_Imm(False, 1)));
      return dst;
    }
    case Iop_8Uto16:
    case Iop_8Uto32:
    case Iop_8Uto64:
    case Iop_16Uto32:
    case Iop_16Uto64: {

      HReg dst     = newVRegI(env);
      HReg src     = iselWordExpr_R(env, e->Iex.Unop.arg);
      Bool srcIs16 = toBool( e->Iex.Unop.op==Iop_16Uto32
                             || e->Iex.Unop.op==Iop_16Uto64 );

      addInstr(env, TILEGXInstr_Bf(GXbf_EXTU, dst, src,
                                   0,
                                   srcIs16 ? 15 : 7));

      return dst;
    }

    case Iop_32to1:
    case Iop_64to1:
      {
        HReg r_dst = newVRegI(env);
        HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

        addInstr(env, TILEGXInstr_Bf(GXbf_EXTU, r_dst, r_src, 0, 0));
        return r_dst;
      }
    case Iop_1Sto32:
    case Iop_1Sto64:
      {
        HReg r_dst = newVRegI(env);
        HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

        addInstr(env, TILEGXInstr_Bf(GXbf_EXTS, r_dst, r_src, 0, 0));
        return r_dst;
      }
    case Iop_8Sto16:
    case Iop_8Sto32:
    case Iop_8Sto64:
      {
        HReg r_dst = newVRegI(env);
        HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

        addInstr(env, TILEGXInstr_Bf(GXbf_EXTS, r_dst, r_src, 0, 7));
        return r_dst;
      }
    case Iop_16Sto32:
    case Iop_16Sto64:
      {
        HReg r_dst = newVRegI(env);
        HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

        addInstr(env, TILEGXInstr_Bf(GXbf_EXTS, r_dst, r_src, 0, 15));
        return r_dst;
      }
    case Iop_32Uto64:
      {
        HReg r_dst = newVRegI(env);
        HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

        addInstr(env, TILEGXInstr_Bf(GXbf_EXTU, r_dst, r_src, 0, 31));
        return r_dst;
      }
    case Iop_32Sto64:
      {
        HReg r_dst = newVRegI(env);
        HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

        addInstr(env, TILEGXInstr_Bf(GXbf_EXTS, r_dst, r_src, 0, 31));
        return r_dst;
      }

    case Iop_CmpNEZ8: {
      HReg r_dst = newVRegI(env);
      HReg tmp = newVRegI(env);
      HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

      TILEGXCondCode cc;

      cc = TILEGXcc_NE;
      addInstr(env, TILEGXInstr_Alu(GXalu_AND, tmp, r_src,
                                    TILEGXRH_Imm(False, 0xFF)));
      addInstr(env, TILEGXInstr_Cmp(False, True, r_dst, tmp,
                                    hregTILEGX_R63(), cc));
      return r_dst;
    }

    case Iop_CmpNEZ32: {
      HReg r_dst = newVRegI(env);
      HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

      TILEGXCondCode cc;

      cc = TILEGXcc_NE;

      addInstr(env, TILEGXInstr_Cmp(False, True, r_dst, r_src,
                                    hregTILEGX_R63(), cc));
      return r_dst;
    }

    case Iop_CmpwNEZ32: {
      HReg r_dst = newVRegI(env);
      HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

      addInstr(env, TILEGXInstr_Alu(GXalu_SUB, r_dst, hregTILEGX_R63(),
                                    TILEGXRH_Reg(r_src)));

      addInstr(env, TILEGXInstr_Alu(GXalu_OR, r_dst, r_dst,
                                    TILEGXRH_Reg(r_src)));
      addInstr(env, TILEGXInstr_Shft(GXshft_SRA, True, r_dst, r_dst,
                                     TILEGXRH_Imm(False, 31)));
      return r_dst;
    }

    case Iop_Left8:
    case Iop_Left16:
    case Iop_Left32:
    case Iop_Left64: {

      HReg r_dst = newVRegI(env);
      HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, TILEGXInstr_Alu(GXalu_SUB, r_dst, hregTILEGX_R63(),
                                    TILEGXRH_Reg(r_src)));
      addInstr(env, TILEGXInstr_Alu(GXalu_OR, r_dst, r_dst,
                                    TILEGXRH_Reg(r_src)));
      return r_dst;
    }

    case Iop_Ctz64:
    case Iop_Clz64: {
      HReg r_dst = newVRegI(env);
      HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
      if (op_unop == Iop_Clz64)
        addInstr(env, TILEGXInstr_Unary(GXun_CLZ, r_dst, r_src));
      else
        addInstr(env, TILEGXInstr_Unary(GXun_CTZ, r_dst, r_src));
      return r_dst;
    }

    case Iop_CmpNEZ64: {

      HReg r_dst = newVRegI(env);
      HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

      TILEGXCondCode cc;

      cc = TILEGXcc_NE;

      addInstr(env, TILEGXInstr_Cmp(False, False, r_dst, r_src,
                                    hregTILEGX_R63(), cc));
      return r_dst;
    }

    case Iop_CmpwNEZ64: {
      HReg tmp1;
      HReg tmp2 = newVRegI(env);

      tmp1 = iselWordExpr_R(env, e->Iex.Unop.arg);

      addInstr(env, TILEGXInstr_Alu(GXalu_SUB, tmp2, hregTILEGX_R63(),
                                    TILEGXRH_Reg(tmp1)));

      addInstr(env, TILEGXInstr_Alu(GXalu_OR, tmp2, tmp2, TILEGXRH_Reg(tmp1)));
      addInstr(env, TILEGXInstr_Shft(GXshft_SRA, False, tmp2, tmp2,
                                     TILEGXRH_Imm (False, 63)));
      return tmp2;
    }

    default:
      goto irreducible;
      break;
    }
    break;
  }

    /* --------- GET --------- */
  case Iex_Get: {
    if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32
        || ((ty == Ity_I64))) {
      HReg r_dst;
      TILEGXAMode *am_addr;
      r_dst = newVRegI(env);
      am_addr = TILEGXAMode_IR(e->Iex.Get.offset,
                               TILEGXGuestStatePointer());
      addInstr(env, TILEGXInstr_Load(toUChar(sizeofIRType(ty)),
                                     r_dst, am_addr));
      return r_dst;
    }
  }

    /* --------- ITE --------- */
  case Iex_ITE: {
    if ((ty == Ity_I8 || ty == Ity_I16 ||
         ty == Ity_I32 || ((ty == Ity_I64))) &&
        typeOfIRExpr(env->type_env, e->Iex.ITE.cond) == Ity_I1) {

      HReg r0 = iselWordExpr_R(env, e->Iex.ITE.iffalse);
      HReg r1 = iselWordExpr_R(env, e->Iex.ITE.iftrue);
      HReg r_cond = iselWordExpr_R(env, e->Iex.ITE.cond);
      HReg r_dst = newVRegI(env);

      /* r_dst = (r_cond) ? r1 : r0 */

      addInstr(env, TILEGXInstr_MovCond(r_dst, r0, TILEGXRH_Reg(r1),
                                        r_cond, TILEGXcc_EZ));

      return r_dst;
    }
  }

    /* --------- LITERAL --------- */
    /* 32/16/8-bit literals */
  case Iex_Const: {
    Long l;
    HReg r_dst = newVRegI(env);
    IRConst *con = e->Iex.Const.con;
    switch (con->tag) {
    case Ico_U64:

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
    default:
      vpanic("iselIntExpr_R.const(tilegx)");
    }
    addInstr(env, TILEGXInstr_LI(r_dst, (ULong) l));
    return r_dst;
  }

    /* --------- CCALL --------- */
  case Iex_CCall: {
    HReg r_dst = newVRegI(env);
    vassert(ty == e->Iex.CCall.retty);

    /* Marshal args, do the call, clear stack. */
    doHelperCall(env, NULL, e->Iex.CCall.cee, e->Iex.CCall.args,
                 e->Iex.CCall.retty);

    /* r0 is the return value. */
    addInstr(env, mk_iMOVds_RR(r_dst, hregTILEGX_R0()));

    return r_dst;
  }

  default:
    goto irreducible;
    break;
  }        /* end switch(e->tag) */

  /* We get here if no pattern matched. */
 irreducible:
  vex_printf("--------------->\n");
  if (e->tag == Iex_RdTmp)
    vex_printf("Iex_RdTmp \n");
  ppIRExpr(e);

  vpanic("iselWordExpr_R(tilegx): cannot reduce tree");
}

/* --------------------- RH --------------------- */

/* Compute an I8/I16/I32/I64 into a RH
   (reg-or-halfword-immediate).  It's important to specify whether the
   immediate is to be regarded as signed or not.  If yes, this will
   never return -32768 as an immediate; this guaranteed that all
   signed immediates that are return can have their sign inverted if
   need be. */

static TILEGXRH *iselWordExpr_RH ( ISelEnv * env, Bool syned, IRExpr * e )
{
  TILEGXRH *ri = iselWordExpr_RH_wrk(env, syned, e);
  /* sanity checks ... */
  switch (ri->tag) {
  case GXrh_Imm:
    vassert(ri->GXrh.Imm.syned == syned);
    if (syned)
      vassert(ri->GXrh.Imm.imm16 != 0x8000);
    return ri;
  case GXrh_Reg:
    vassert(hregClass(ri->GXrh.Reg.reg) == HRcGPR());
    vassert(hregIsVirtual(ri->GXrh.Reg.reg));
    return ri;
  default:
    vpanic("iselIntExpr_RH: unknown tilegx RH tag");
  }
}

/* DO NOT CALL THIS DIRECTLY ! */
static TILEGXRH *iselWordExpr_RH_wrk ( ISelEnv * env, Bool syned, IRExpr * e )
{
  ULong u;
  Long l;
  IRType ty = typeOfIRExpr(env->type_env, e);
  vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 ||
          ((ty == Ity_I64)));

  /* special case: immediate */
  if (e->tag == Iex_Const) {
    IRConst *con = e->Iex.Const.con;
    /* What value are we aiming to generate? */
    switch (con->tag) {
      /* Note: Not sign-extending - we carry 'syned' around */
    case Ico_U64:
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
      vpanic("iselIntExpr_RH.Iex_Const(tilegx)");
    }
    l = (Long) u;
    /* Now figure out if it's representable. */
    if (!syned && u <= 255) {
      return TILEGXRH_Imm(False /*unsigned */ , toUShort(u & 0xFFFF));
    }
    if (syned && l >= -127 && l <= 127) {
      return TILEGXRH_Imm(True /*signed */ , toUShort(u & 0xFFFF));
    }
    /* no luck; use the Slow Way. */
  }
  /* default case: calculate into a register and return that */
  return TILEGXRH_Reg(iselWordExpr_R(env, e));
}

/* --------------------- RH6u --------------------- */

/* Compute an I8 into a reg-or-6-bit-unsigned-immediate, the latter
   being an immediate in the range 0 .. 63 inclusive.  Used for doing
   shift amounts. */

static TILEGXRH *iselWordExpr_RH6u ( ISelEnv * env, IRExpr * e )
{
  TILEGXRH *ri;
  ri = iselWordExpr_RH6u_wrk(env, e);
  /* sanity checks ... */
  switch (ri->tag) {
  case GXrh_Imm:
    vassert(ri->GXrh.Imm.imm16 >= 1 && ri->GXrh.Imm.imm16 <= 63);
    vassert(!ri->GXrh.Imm.syned);
    return ri;
  case GXrh_Reg:
    vassert(hregClass(ri->GXrh.Reg.reg) == HRcInt64);
    vassert(hregIsVirtual(ri->GXrh.Reg.reg));
    return ri;
  default:
    vpanic("iselIntExpr_RH6u: unknown tilegx RH tag");
  }
}

/* DO NOT CALL THIS DIRECTLY ! */
static TILEGXRH *iselWordExpr_RH6u_wrk ( ISelEnv * env, IRExpr * e )
{
  IRType ty = typeOfIRExpr(env->type_env, e);

  /* special case: immediate */
  if (e->tag == Iex_Const)
  {
    if (ty == Ity_I8)
    {
      if(e->Iex.Const.con->tag == Ico_U8
         && e->Iex.Const.con->Ico.U8 >= 1 && e->Iex.Const.con->Ico.U8 <= 63)
        return TILEGXRH_Imm(False /*unsigned */ , e->Iex.Const.con->Ico.U8);
    }
    else if (ty == Ity_I64)
    {
      if(e->Iex.Const.con->tag == Ico_U64
         && e->Iex.Const.con->Ico.U64 >= 1
         && e->Iex.Const.con->Ico.U64 <= 63)
        return TILEGXRH_Imm(False /*unsigned */, e->Iex.Const.con->Ico.U64);
    }
  }

  /* default case: calculate into a register and return that */
  return TILEGXRH_Reg(iselWordExpr_R(env, e));
}

/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static TILEGXCondCode iselCondCode(ISelEnv * env, IRExpr * e)
{
  TILEGXCondCode cc = iselCondCode_wrk(env,e);
  vassert(cc != TILEGXcc_NV);
  return cc;
}

/* DO NOT CALL THIS DIRECTLY ! */
static TILEGXCondCode iselCondCode_wrk ( ISelEnv * env, IRExpr * e )
{
  vassert(e);
  vassert(typeOfIRExpr(env->type_env, e) == Ity_I1);

  /* Cmp*(x,y) ? */
  if (e->Iex.Binop.op == Iop_CmpEQ32
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
      || e->Iex.Binop.op == Iop_CasCmpEQ64) {

    Bool syned = (e->Iex.Binop.op == Iop_CmpLT32S
                  || e->Iex.Binop.op == Iop_CmpLE32S
                  || e->Iex.Binop.op == Iop_CmpLT64S
                  || e->Iex.Binop.op == Iop_CmpLE64S);
    Bool size32;
    HReg dst = newVRegI(env);
    HReg r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
    HReg r2 = iselWordExpr_R(env, e->Iex.Binop.arg2);

    TILEGXCondCode cc;

    switch (e->Iex.Binop.op) {
    case Iop_CmpEQ32:
    case Iop_CasCmpEQ32:
      cc = TILEGXcc_EQ;
      size32 = True;
      break;
    case Iop_CmpNE32:
      cc = TILEGXcc_NE;
      size32 = True;
      break;
    case Iop_CmpNE64:
      cc = TILEGXcc_NE;
      size32 = True;
      break;
    case Iop_CmpLT32S:
      cc = TILEGXcc_LT;
      size32 = True;
      break;
    case Iop_CmpLT32U:
      cc = TILEGXcc_LO;
      size32 = True;
      break;
    case Iop_CmpLT64U:
      cc = TILEGXcc_LO;
      size32 = False;
      break;
    case Iop_CmpLE32S:
      cc = TILEGXcc_LE;
      size32 = True;
      break;
    case Iop_CmpLE64S:
      cc = TILEGXcc_LE;
      size32 = False;
      break;
    case Iop_CmpLT64S:
      cc = TILEGXcc_LT;
      size32 = False;
      break;
    case Iop_CmpEQ64:
    case Iop_CasCmpEQ64:
      cc = TILEGXcc_EQ;
      size32 = False;
      break;
    default:
      vpanic("iselCondCode(tilegx): CmpXX32 or CmpXX64");
      break;
    }

    addInstr(env, TILEGXInstr_Cmp(syned, size32, dst, r1, r2, cc));
    /* Store result to guest_COND */
    TILEGXAMode *am_addr = TILEGXAMode_IR(0, TILEGXGuestStatePointer());

    addInstr(env, TILEGXInstr_Store(8,
                                    TILEGXAMode_IR(am_addr->GXam.IR.index +
                                                   COND_OFFSET(),
                                                   am_addr->GXam.IR.base),
                                    dst));
    return cc;
  }

  if (e->Iex.Binop.op == Iop_Not1) {
    HReg r_dst = newVRegI(env);
    HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
    TILEGXRH *r_srcR = TILEGXRH_Reg(r_srcL);

    addInstr(env, TILEGXInstr_LI(r_dst, 0x1));
    addInstr(env, TILEGXInstr_Alu(GXalu_SUB, r_dst, r_dst, r_srcR));

   /* Store result to guest_COND */
    TILEGXAMode *am_addr = TILEGXAMode_IR(0, TILEGXGuestStatePointer());

    addInstr(env, TILEGXInstr_Store(8,
                                    TILEGXAMode_IR(am_addr->GXam.IR.index +
                                                   COND_OFFSET(),
                                                   am_addr->GXam.IR.base),
                                    r_dst));
    return TILEGXcc_NE;
  }

  if (e->tag == Iex_RdTmp || e->tag == Iex_Unop) {
    HReg r_dst = iselWordExpr_R_wrk(env, e);
    /* Store result to guest_COND */
    TILEGXAMode *am_addr = TILEGXAMode_IR(0, TILEGXGuestStatePointer());

    addInstr(env, TILEGXInstr_Store(8,
                                    TILEGXAMode_IR(am_addr->GXam.IR.index +
                                                   COND_OFFSET(),
                                                   am_addr->GXam.IR.base),
                                    r_dst));
    return TILEGXcc_EQ;
  }

  vex_printf("iselCondCode(tilegx): No such tag(%u)\n", e->tag);
  ppIRExpr(e);
  vpanic("iselCondCode(tilegx)");

  /* Constant 1:Bit */
  if (e->tag == Iex_Const && e->Iex.Const.con->Ico.U1 == True)
    return TILEGXcc_AL;

  if (e->tag == Iex_RdTmp)
    return TILEGXcc_EQ;

  if (e->tag == Iex_Binop)
    return TILEGXcc_EQ;

  if (e->tag == Iex_Unop)
    return TILEGXcc_EQ;

  vex_printf("iselCondCode(tilegx): No such tag(%u)\n", e->tag);
  ppIRExpr(e);
  vpanic("iselCondCode(tilegx)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void iselStmt ( ISelEnv * env, IRStmt * stmt )
{
  if (vex_traceflags & VEX_TRACE_VCODE) {
    vex_printf("\n-- ");
    ppIRStmt(stmt);
    vex_printf("\n");
  }

  switch (stmt->tag) {
    /* --------- STORE --------- */
  case Ist_Store: {
    TILEGXAMode *am_addr;
    IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);

    /*constructs addressing mode from address provided */
    am_addr = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd);

    if (tyd == Ity_I8 || tyd == Ity_I16 || tyd == Ity_I32 ||
        (tyd == Ity_I64)) {
      HReg r_src = iselWordExpr_R(env, stmt->Ist.Store.data);
      addInstr(env, TILEGXInstr_Store(toUChar(sizeofIRType(tyd)),
                                      am_addr, r_src));
      return;
    }
    break;
  }

    /* --------- PUT --------- */
  case Ist_Put: {
    IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);

    if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 ||
        (ty == Ity_I64)) {
      HReg r_src = iselWordExpr_R(env, stmt->Ist.Put.data);
      TILEGXAMode *am_addr = TILEGXAMode_IR(stmt->Ist.Put.offset,
                                            TILEGXGuestStatePointer());
      addInstr(env, TILEGXInstr_Store(toUChar(sizeofIRType(ty)),
                                      am_addr, r_src));
      return;
    }
    break;
  }

    /* --------- TMP --------- */
  case Ist_WrTmp: {
    IRTemp tmp = stmt->Ist.WrTmp.tmp;
    IRType ty = typeOfIRTemp(env->type_env, tmp);
    HReg r_dst = lookupIRTemp(env, tmp);
    HReg r_src = iselWordExpr_R(env, stmt->Ist.WrTmp.data);
    IRType dty = typeOfIRExpr(env->type_env, stmt->Ist.WrTmp.data);

    if (ty == Ity_I64 || ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8 ||
        (ty == dty))
    {
      addInstr(env, mk_iMOVds_RR(r_dst, r_src));
      return;
    }
    else if (ty == Ity_I1) {
      switch (dty)
      {
      case Ity_I32:
        addInstr(env, TILEGXInstr_Bf(GXbf_EXTU, r_src, r_src, 0, 31));
        break;
      case Ity_I16:
        addInstr(env, TILEGXInstr_Bf(GXbf_EXTU, r_src, r_src, 0, 15));
        break;
      case Ity_I8:
        addInstr(env, TILEGXInstr_Bf(GXbf_EXTU, r_src, r_src, 0, 7));
        break;
      default:
        vassert(0);
      }

      addInstr(env, TILEGXInstr_MovCond(r_dst,
                                        hregTILEGX_R63(),
                                        TILEGXRH_Imm(False, 1),
                                        r_src,
                                        TILEGXcc_EZ));
      return;
    }
    break;
  }

    /* --------- Call to DIRTY helper --------- */
  case Ist_Dirty: {
    IRType retty;
    IRDirty *d = stmt->Ist.Dirty.details;

    /* Marshal args, do the call, clear stack. */
    doHelperCall(env, d->guard, d->cee, d->args, -1);

    /* Now figure out what to do with the returned value, if any. */
    if (d->tmp == IRTemp_INVALID)
      /* No return value.  Nothing to do. */
      return;

    retty = typeOfIRTemp(env->type_env, d->tmp);

    if (retty == Ity_I8 || retty == Ity_I16 || retty == Ity_I32
        || (retty == Ity_I64)) {
      /* The returned value is in r0.  Park it in the register
         associated with tmp. */
      HReg r_dst = lookupIRTemp(env, d->tmp);
      addInstr(env, mk_iMOVds_RR(r_dst, hregTILEGX_R0()));
      return;
    }
    break;
  }


    /* --------- ACAS --------- */
  case Ist_CAS:
    {
      UChar  sz;
      IRCAS* cas = stmt->Ist.CAS.details;
      IRType ty  = typeOfIRExpr(env->type_env, cas->dataLo);

      TILEGXAMode *r_addr = iselWordExpr_AMode(env, cas->addr, Ity_I64);
      HReg r_new  = iselWordExpr_R(env, cas->dataLo);
      HReg r_old  = lookupIRTemp(env,   cas->oldLo);
      HReg r_exp =  INVALID_HREG;

      vassert(cas->expdHi == NULL);
      vassert(cas->dataHi == NULL);
      vassert(r_addr->tag == GXam_IR);
      vassert(r_addr->GXam.IR.index == 0);

      switch (ty)
      {
      case Ity_I64: sz = 8; break;
      case Ity_I32: sz = 4; break;
      default: vassert(0);
      }

      if (cas->expdLo->tag != Iex_Const)
      {
        r_exp = iselWordExpr_R(env, cas->expdLo);
        addInstr(env, TILEGXInstr_Acas(GXacas_CMPEXCH, r_old,
                                       r_addr->GXam.IR.base, r_exp,
                                       r_new, sz));
      }
      else
      {
        if((sz == 8 && cas->expdLo->Iex.Const.con->Ico.U64 == 0) ||
           (sz == 4 && cas->expdLo->Iex.Const.con->Ico.U32 == 0))
        {
          addInstr(env, TILEGXInstr_Acas(GXacas_EXCH, r_old,
                                         r_addr->GXam.IR.base,
                                         r_exp, r_new, sz));
        }
        else if((sz == 8 && cas->expdLo->Iex.Const.con->Ico.U64 == 2) ||
                (sz == 4 && cas->expdLo->Iex.Const.con->Ico.U32 == 2))
        {
          addInstr(env, TILEGXInstr_Acas(GXacas_FetchAnd, r_old,
                                         r_addr->GXam.IR.base, r_exp,
                                         r_new, sz));
        }
        else if((sz == 8 && cas->expdLo->Iex.Const.con->Ico.U64 == 3) ||
                (sz == 4 && cas->expdLo->Iex.Const.con->Ico.U32 == 3))
        {
          addInstr(env, TILEGXInstr_Acas(GXacas_FetchAdd, r_old,
                                         r_addr->GXam.IR.base,
                                         r_exp, r_new, sz));
        }
        else if((sz == 8 && cas->expdLo->Iex.Const.con->Ico.U64 == 4) ||
                (sz == 4 && cas->expdLo->Iex.Const.con->Ico.U32 == 4))
        {
          addInstr(env, TILEGXInstr_Acas(GXacas_FetchOr, r_old,
                                         r_addr->GXam.IR.base, r_exp,
                                         r_new, sz));
        }
        else if((sz == 8 && cas->expdLo->Iex.Const.con->Ico.U64 == 5) ||
                (sz == 4 && cas->expdLo->Iex.Const.con->Ico.U32 == 5))
        {
          addInstr(env, TILEGXInstr_Acas(GXacas_FetchAddgez, r_old,
                                         r_addr->GXam.IR.base, r_exp,
                                         r_new, sz));
        }
        else
        {
          vassert(0);
        }
      }
      return;
    }

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

    TILEGXCondCode cc   = iselCondCode(env, stmt->Ist.Exit.guard);
    TILEGXAMode*   amPC = TILEGXAMode_IR(stmt->Ist.Exit.offsIP,
                                         TILEGXGuestStatePointer());

    /* Case: boring transfer to known address */
    if (stmt->Ist.Exit.jk == Ijk_Boring
        || stmt->Ist.Exit.jk == Ijk_Call
        /* || stmt->Ist.Exit.jk == Ijk_Ret */) {
      if (env->chainingAllowed) {
        /* .. almost always true .. */
        /* Skip the event check at the dst if this is a forwards
           edge. */
        Bool toFastEP  =
          ((Addr64)stmt->Ist.Exit.dst->Ico.U64) > ((Addr64)env->max_ga);

        if (0) vex_printf("%s", toFastEP ? "Y" : ",");
        addInstr(env, TILEGXInstr_XDirect(
                   (Addr64)stmt->Ist.Exit.dst->Ico.U64,
                   amPC, cc, toFastEP));
      } else {
        /* .. very occasionally .. */
        /* We can't use chaining, so ask for an assisted transfer,
           as that's the only alternative that is allowable. */
        HReg r = iselWordExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
        addInstr(env, TILEGXInstr_XAssisted(r, amPC, cc, Ijk_Boring));
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
    case Ijk_Ret:
      {
        HReg r = iselWordExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
        addInstr(env, TILEGXInstr_XAssisted(r, amPC, cc,
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

static void iselNext ( ISelEnv * env, IRExpr * next, IRJumpKind jk,
                       Int offsIP )
{

  if (vex_traceflags & VEX_TRACE_VCODE) {
    vex_printf("\n-- PUT(%d) = ", offsIP);
    ppIRExpr(next);
    vex_printf( "; exit-");
    ppIRJumpKind(jk);
    vex_printf( "\n");
  }

  /* Case: boring transfer to known address */
  if (next->tag == Iex_Const) {
    IRConst* cdst = next->Iex.Const.con;
    if (jk == Ijk_Boring || jk == Ijk_Call) {
      /* Boring transfer to known address */
      TILEGXAMode* amPC = TILEGXAMode_IR(offsIP, TILEGXGuestStatePointer());
      if (env->chainingAllowed) {
        /* .. almost always true .. */
        /* Skip the event check at the dst if this is a forwards
           edge. */
        Bool toFastEP = ((Addr64)cdst->Ico.U64) > ((Addr64)env->max_ga);

        if (0) vex_printf("%s", toFastEP ? "X" : ".");
        addInstr(env, TILEGXInstr_XDirect((Addr64)cdst->Ico.U64,
                                          amPC, TILEGXcc_AL, toFastEP));
      } else {
        /* .. very occasionally .. */
        /* We can't use chaining, so ask for an assisted transfer,
           as that's the only alternative that is allowable. */
        HReg r = iselWordExpr_R(env, next);
        addInstr(env, TILEGXInstr_XAssisted(r, amPC, TILEGXcc_AL,
                                            Ijk_Boring));
      }
      return;
    }
  }

  /* Case: call/return (==boring) transfer to any address */
  switch (jk) {
  case Ijk_Boring: case Ijk_Call: {
    HReg       r     = iselWordExpr_R(env, next);
    TILEGXAMode*  amPC = TILEGXAMode_IR(offsIP,
                                        TILEGXGuestStatePointer());
    if (env->chainingAllowed)
      addInstr(env, TILEGXInstr_XIndir(r, amPC, TILEGXcc_AL));
    else
      addInstr(env, TILEGXInstr_XAssisted(r, amPC, TILEGXcc_AL,
                                          Ijk_Boring));
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
  case Ijk_InvalICache:
  case Ijk_Ret: {
    HReg  r = iselWordExpr_R(env, next);
    TILEGXAMode* amPC = TILEGXAMode_IR(offsIP, TILEGXGuestStatePointer());
    addInstr(env, TILEGXInstr_XAssisted(r, amPC, TILEGXcc_AL, jk));
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

/* Translate an entire BB to tilegx code. */
HInstrArray *iselSB_TILEGX ( const IRSB* bb,
                             VexArch arch_host,
                             const VexArchInfo* archinfo_host,
                             const VexAbiInfo* vbi,
                             Int offs_Host_EvC_Counter,
                             Int offs_Host_EvC_FailAddr,
                             Bool chainingAllowed,
                             Bool addProfInc,
                             Addr max_ga )
{
  Int i, j;
  HReg hreg;
  ISelEnv *env;
  UInt hwcaps_host = archinfo_host->hwcaps;
  TILEGXAMode *amCounter, *amFailAddr;

  /* sanity ... */
  vassert(arch_host == VexArchTILEGX);

  /* Make up an initial environment to use. */
  env = LibVEX_Alloc(sizeof(ISelEnv));
  env->vreg_ctr = 0;
  env->mode64 = True;

  /* Set up output code array. */
  env->code = newHInstrArray();

  /* Copy BB's type env. */
  env->type_env = bb->tyenv;

  /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
     change as we go along. */
  env->n_vregmap = bb->tyenv->types_used;
  env->vregmap = LibVEX_Alloc(env->n_vregmap * sizeof(HReg));

  /* and finally ... */
  env->hwcaps = hwcaps_host;
  env->chainingAllowed = chainingAllowed;
  env->hwcaps          = hwcaps_host;
  env->max_ga          = max_ga;

  /* For each IR temporary, allocate a suitably-kinded virtual
     register. */
  j = 0;

  for (i = 0; i < env->n_vregmap; i++) {
    hreg = INVALID_HREG;
    switch (bb->tyenv->types[i]) {
    case Ity_I1:
    case Ity_I8:
    case Ity_I16:
    case Ity_I32:
      hreg = mkHReg(True, HRcInt64, 0, j++);
      break;
    case Ity_I64:
      hreg = mkHReg(True, HRcInt64, 0, j++);
      break;
    default:
      ppIRType(bb->tyenv->types[i]);
      vpanic("iselBB(tilegx): IRTemp type");
    }
    env->vregmap[i] = hreg;
  }
  env->vreg_ctr = j;

  /* The very first instruction must be an event check. */
  amCounter = TILEGXAMode_IR(offs_Host_EvC_Counter,
                             TILEGXGuestStatePointer());
  amFailAddr = TILEGXAMode_IR(offs_Host_EvC_FailAddr,
                              TILEGXGuestStatePointer());
  addInstr(env, TILEGXInstr_EvCheck(amCounter, amFailAddr));

  /* Possibly a block counter increment (for profiling).  At this
     point we don't know the address of the counter, so just pretend
     it is zero.  It will have to be patched later, but before this
     translation is used, by a call to LibVEX_patchProfCtr. */
  if (addProfInc) {
    addInstr(env, TILEGXInstr_ProfInc());
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
/*--- end                                  host_tilegx_isel.c ---*/
/*---------------------------------------------------------------*/
