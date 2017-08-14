
/*---------------------------------------------------------------*/
/*--- begin                                   host_mips_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2012 RT-RK
      mips-valgrind@rt-rk.com

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

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_mips_defs.h"

/*---------------------------------------------------------*/
/*--- Register Usage Conventions                        ---*/
/*---------------------------------------------------------*/
/*

Integer Regs
------------
ZERO0       Reserved
GPR1:9      Allocateable
10          GuestStatePointer
GPR1:9      Allocateable
SP          StackFramePointer
RA          LinkRegister

*/

static Bool mode64 = False;

/* GPR register class for mips32/64 */
#define HRcGPR(__mode64) (__mode64 ? HRcInt64 : HRcInt32)

/* FPR register class for mips32/64 */
#define HRcFPR(__mode64) (__mode64 ? HRcFlt64 : HRcFlt32)

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
      Bool         mode64;

      Bool         chainingAllowed;
      Addr64       max_ga;

      /* These are modified as we go along. */
      HInstrArray* code;
      Int          vreg_ctr;
   }
   ISelEnv;

static HReg lookupIRTemp(ISelEnv * env, IRTemp tmp)
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   return env->vregmap[tmp];
}

static void lookupIRTemp64(HReg * vrHI, HReg * vrLO, ISelEnv * env, IRTemp tmp)
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   vassert(env->vregmapHI[tmp] != INVALID_HREG);
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void
lookupIRTempPair(HReg * vrHI, HReg * vrLO, ISelEnv * env, IRTemp tmp)
{
   vassert(env->mode64);
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   vassert(env->vregmapHI[tmp] != INVALID_HREG);
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr(ISelEnv * env, MIPSInstr * instr)
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppMIPSInstr(instr, mode64);
      vex_printf("\n");
   }
}

static HReg newVRegI(ISelEnv * env)
{
   HReg reg = mkHReg(env->vreg_ctr, HRcGPR(env->mode64), 
                     True /*virtual reg */ );
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegD(ISelEnv * env)
{
   HReg reg = mkHReg(env->vreg_ctr, HRcFlt64, True /*virtual reg */ );
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF(ISelEnv * env)
{
   HReg reg = mkHReg(env->vreg_ctr, HRcFPR(env->mode64), 
                     True /*virtual reg */ );
   env->vreg_ctr++;
   return reg;
}

static void add_to_sp(ISelEnv * env, UInt n)
{
   HReg sp = StackPointer(mode64);
   vassert(n < 256 && (n % 8) == 0);
   addInstr(env, MIPSInstr_Alu(Malu_ADD, sp, sp, MIPSRH_Imm(True,
                                                            toUShort(n))));
}

static void sub_from_sp(ISelEnv * env, UInt n)
{
   HReg sp = StackPointer(mode64);
   vassert(n < 256 && (n % 8) == 0);
   addInstr(env, MIPSInstr_Alu(Malu_SUB, sp, sp,
                               MIPSRH_Imm(True, toUShort(n))));
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
/* 32-bit mode: Compute an I8/I16/I32 into a RH
                (reg-or-halfword-immediate).
   It's important to specify whether the immediate is to be regarded
   as signed or not.  If yes, this will never return -32768 as an
   immediate; this guaranteed that all signed immediates that are
   return can have their sign inverted if need be. 
*/
static MIPSRH *iselWordExpr_RH_wrk(ISelEnv * env, Bool syned, IRExpr * e);
static MIPSRH *iselWordExpr_RH(ISelEnv * env, Bool syned, IRExpr * e);

/* Compute an I8 into a reg-or-5-bit-unsigned-immediate, the latter being an immediate in
   the range 1 .. 31 inclusive.  Used for doing shift amounts. */
static MIPSRH *iselWordExpr_RH5u_wrk(ISelEnv * env, IRExpr * e);
static MIPSRH *iselWordExpr_RH5u(ISelEnv * env, IRExpr * e);

/* compute an I8/I16/I32 into a GPR*/
static HReg iselWordExpr_R_wrk(ISelEnv * env, IRExpr * e);
static HReg iselWordExpr_R(ISelEnv * env, IRExpr * e);

/* compute an I32 into an AMode. */
static MIPSAMode *iselWordExpr_AMode_wrk(ISelEnv * env, IRExpr * e,
                                         IRType xferTy);
static MIPSAMode *iselWordExpr_AMode(ISelEnv * env, IRExpr * e, IRType xferTy);

static void iselInt64Expr_wrk(HReg * rHi, HReg * rLo, ISelEnv * env,
                              IRExpr * e);
static void iselInt64Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e);

/* 64-bit mode ONLY: compute an I128 into a GPR64 pair. */
static void iselInt128Expr_wrk(HReg * rHi, HReg * rLo,
                               ISelEnv * env, IRExpr * e);
static void iselInt128Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e);

static MIPSCondCode iselCondCode_wrk(ISelEnv * env, IRExpr * e);
static MIPSCondCode iselCondCode(ISelEnv * env, IRExpr * e);

static HReg iselDblExpr_wrk(ISelEnv * env, IRExpr * e);
static HReg iselDblExpr(ISelEnv * env, IRExpr * e);

static HReg iselFltExpr_wrk(ISelEnv * env, IRExpr * e);
static HReg iselFltExpr(ISelEnv * env, IRExpr * e);

static void set_MIPS_rounding_mode(ISelEnv * env, IRExpr * mode)
{
   /*
      rounding mode | MIPS | IR
      ------------------------
      to nearest    | 00  | 00
      to zero       | 01  | 11
      to +infinity  | 10  | 10
      to -infinity  | 11  | 01
    */
   // rm_MIPS32  = XOR(rm_IR , (rm_IR << 1)) & 2
   HReg irrm = iselWordExpr_R(env, mode);
   HReg tmp = newVRegI(env);
   HReg fcsr_old = newVRegI(env);
   MIPSAMode *am_addr;

   addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp, irrm,
                                MIPSRH_Imm(False, 1)));
   addInstr(env, MIPSInstr_Alu(Malu_XOR, tmp, irrm, MIPSRH_Reg(tmp)));
   addInstr(env, MIPSInstr_Alu(Malu_AND, irrm, tmp, MIPSRH_Imm(False, 3)));
   /* save old value of FCSR */
   addInstr(env, MIPSInstr_MfFCSR(fcsr_old));
   sub_from_sp(env, 8); // Move SP down 4 bytes
   am_addr = MIPSAMode_IR(0, StackPointer(mode64));

   //store old FCSR to stack
   addInstr(env, MIPSInstr_Store(4, am_addr, fcsr_old, mode64));

   //set new value of FCSR
   addInstr(env, MIPSInstr_MtFCSR(irrm));
}

static void set_MIPS_rounding_default(ISelEnv * env)
{
   HReg fcsr = newVRegI(env);
   // load as float
   MIPSAMode *am_addr;
   am_addr = MIPSAMode_IR(0, StackPointer(mode64));

   addInstr(env, MIPSInstr_Load(4, fcsr, am_addr, mode64));

   add_to_sp(env, 8);   // Reset SP

   //set new value of FCSR
   addInstr(env, MIPSInstr_MtFCSR(fcsr));
}

/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

/* Make an int reg-reg move. */
static MIPSInstr *mk_iMOVds_RR(HReg r_dst, HReg r_src)
{
   vassert(hregClass(r_dst) == hregClass(r_src));
   vassert(hregClass(r_src) == HRcInt32 || hregClass(r_src) == HRcInt64);
   return MIPSInstr_Alu(Malu_OR, r_dst, r_src, MIPSRH_Reg(r_src));
}

/*---------------------------------------------------------*/
/*--- ISEL: Function call helpers                       ---*/
/*---------------------------------------------------------*/

/* Used only in doHelperCall.  See big comment in doHelperCall re
   handling of register-parameter args.  This function figures out
   whether evaluation of an expression might require use of a fixed
   register.  If in doubt return True (safe but suboptimal).
*/
static Bool mightRequireFixedRegs(IRExpr * e)
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

/* Load 2*I32 regs to fp reg */
static HReg mk_LoadRR32toFPR(ISelEnv * env, HReg r_srcHi, HReg r_srcLo)
{
   HReg fr_dst = newVRegD(env);
   MIPSAMode *am_addr0, *am_addr1;

   vassert(hregClass(r_srcHi) == HRcInt32);
   vassert(hregClass(r_srcLo) == HRcInt32);

   sub_from_sp(env, 16);   // Move SP down 16 bytes
   am_addr0 = MIPSAMode_IR(0, StackPointer(mode64));
   am_addr1 = MIPSAMode_IR(8, StackPointer(mode64));

   // store hi,lo as Ity_I32's
   addInstr(env, MIPSInstr_Store(4, am_addr0, r_srcLo, mode64));
   addInstr(env, MIPSInstr_Store(4, am_addr1, r_srcHi, mode64));

   // load as float
   addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, fr_dst, am_addr0));

   add_to_sp(env, 16);  // Reset SP
   return fr_dst;
}

/* Do a complete function call.  guard is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional. */

static void doHelperCall(ISelEnv * env, Bool passBBP, IRExpr * guard,
                         IRCallee * cee, IRExpr ** args)
{
   MIPSCondCode cc;
   HReg argregs[MIPS_N_REGPARMS];
   HReg tmpregs[MIPS_N_REGPARMS];
   Bool go_fast;
   Int n_args, i, argreg;
   UInt argiregs;
   ULong target;
   HReg src = 0;

   /* MIPS O32 calling convention: up to four registers ($a0 ... $a3)
      are allowed to be used for passing integer arguments. They correspond
      to regs GPR4 ... GPR7. Note that the cee->regparms field is meaningless 
      on MIPS host (since we only implement one calling convention) and so we 
      always ignore it. */

   /* MIPS 64 calling convention: up to four registers ($a0 ... $a7)
      are allowed to be used for passing integer arguments. They correspond
      to regs GPR4 ... GPR11. Note that the cee->regparms field is meaningless 
      on MIPS host (since we only implement one calling convention) and so we 
      always ignore it. */
   n_args = 0;
   for (i = 0; args[i]; i++)
      n_args++;

   if (MIPS_N_REGPARMS < n_args + (passBBP ? 1 : 0)) {
      vpanic("doHelperCall(MIPS): cannot currently handle > 4 args");
   }
   argregs[0] = hregMIPS_GPR4(mode64);
   argregs[1] = hregMIPS_GPR5(mode64);
   argregs[2] = hregMIPS_GPR6(mode64);
   argregs[3] = hregMIPS_GPR7(mode64);
   argiregs = 0;

   tmpregs[0] = tmpregs[1] = tmpregs[2] = tmpregs[3] = INVALID_HREG;

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

   /* save GuestStatePointer on the stack */
   sub_from_sp(env, 8);   // Move SP down 4 bytes
   addInstr(env, MIPSInstr_Store(4, MIPSAMode_IR(0, StackPointer(mode64)),
                                    GuestStatePointer(mode64), mode64));

   /* At this point the scheme to use has been established.  Generate
      code to get the arg values into the argument rregs. */
   if (go_fast) {
      /* FAST SCHEME */
      argreg = 0;
      if (passBBP) {
         argiregs |= (1 << (argreg + 4));
         addInstr(env, mk_iMOVds_RR(argregs[argreg],
                  GuestStatePointer(mode64)));
         argreg++;
      }

      for (i = 0; i < n_args; i++) {
         vassert(argreg < MIPS_N_REGPARMS);
         vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32
                 || typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
         if (typeOfIRExpr(env->type_env, args[i]) == Ity_I32) {
            argiregs |= (1 << (argreg + 4));
            addInstr(env, mk_iMOVds_RR(argregs[argreg], iselWordExpr_R(env,
                     args[i])));
         } else { // Ity_I64
            vassert(mode64);
            argiregs |= (1 << (argreg + 4));
            addInstr(env, mk_iMOVds_RR(argregs[argreg], iselWordExpr_R(env,
                     args[i])));
         }
         argreg++;
      }
      /* Fast scheme only applies for unconditional calls.  Hence: */
      cc = MIPScc_AL;
   } else {
      /* SLOW SCHEME; move via temporaries */
      argreg = 0;
      if (passBBP) {
         /* This is pretty stupid; better to move directly to r3
            after the rest of the args are done. */
         tmpregs[argreg] = newVRegI(env);
         addInstr(env, mk_iMOVds_RR(tmpregs[argreg],
                  GuestStatePointer(mode64)));
         argreg++;
      }
      for (i = 0; i < n_args; i++) {
         vassert(argreg < MIPS_N_REGPARMS);
         vassert(typeOfIRExpr(env->type_env, args[i]) == Ity_I32
                 || typeOfIRExpr(env->type_env, args[i]) == Ity_I64);
         if (typeOfIRExpr(env->type_env, args[i]) == Ity_I32) {
            tmpregs[argreg] = iselWordExpr_R(env, args[i]);
         } else { // Ity_I64
            vassert(mode64);
            tmpregs[argreg] = iselWordExpr_R(env, args[i]);
         }
         argreg++;
      }

      /* Now we can compute the condition.  We can't do it earlier
         because the argument computations could trash the condition
         codes.  Be a bit clever to handle the common case where the
         guard is 1:Bit. */
      cc = MIPScc_AL;
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
         if (tmpregs[i] == INVALID_HREG)  // Skip invalid regs
            continue;
         /* None of these insns, including any spill code that might
            be generated, may alter the condition codes. */
         argiregs |= (1 << (i + 4));
         addInstr(env, mk_iMOVds_RR(argregs[i], tmpregs[i]));
      }
   }

   target = toUInt(Ptr_to_ULong(cee->addr));

   /* Finally, the call itself. */
   if (mode64)
      if (cc == MIPScc_AL) {
         addInstr(env, MIPSInstr_CallAlways(cc, target, argiregs));
      } else {
         addInstr(env, MIPSInstr_Call(cc, target, argiregs, src));
   } else if (cc == MIPScc_AL) {
      addInstr(env, MIPSInstr_CallAlways(cc, (Addr32) target, argiregs));
   } else {
      addInstr(env, MIPSInstr_Call(cc, (Addr32) target, argiregs, src));
   }
   /* restore GuestStatePointer */
   addInstr(env, MIPSInstr_Load(4, GuestStatePointer(mode64),
            MIPSAMode_IR(0, StackPointer(mode64)), mode64));
   add_to_sp(env, 8);  // Reset SP
}

/*---------------------------------------------------------*/
/*--- ISEL: Integer expression auxiliaries              ---*/
/*---------------------------------------------------------*/

/* --------------------- AMODEs --------------------- */

/* Return an AMode which computes the value of the specified
   expression, possibly also adding insns to the code list as a
   result.  The expression may only be a word-size one.
*/

static Bool uInt_fits_in_16_bits(UInt u)
{
   Int i = u & 0xFFFF;
   i <<= 16;
   i >>= 16;
   return toBool(u == (UInt) i);
}

static Bool sane_AMode(ISelEnv * env, MIPSAMode * am)
{
   switch (am->tag) {
      case Mam_IR:
         return toBool(hregClass(am->Mam.IR.base) == HRcGPR(mode64) &&
                  hregIsVirtual(am->Mam.IR.base) &&
                  uInt_fits_in_16_bits(am->Mam.IR.index));
      case Mam_RR:
         return toBool(hregClass(am->Mam.RR.base) == HRcGPR(mode64) &&
                  hregIsVirtual(am->Mam.RR.base) &&
                  hregClass(am->Mam.RR.index) == HRcGPR(mode64) &&
                  hregIsVirtual(am->Mam.IR.index));
      default:
         vpanic("sane_AMode: unknown mips amode tag");
   }
}

static MIPSAMode *iselWordExpr_AMode(ISelEnv * env, IRExpr * e, IRType xferTy)
{
   MIPSAMode *am = iselWordExpr_AMode_wrk(env, e, xferTy);
   vassert(sane_AMode(env, am));
   return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSAMode *iselWordExpr_AMode_wrk(ISelEnv * env, IRExpr * e,
                IRType xferTy)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   {
      vassert(ty == Ity_I32);

      /* Add32(expr,i), where i == sign-extend of (i & 0xFFFF) */
      if (e->tag == Iex_Binop
          && e->Iex.Binop.op == Iop_Add32
          && e->Iex.Binop.arg2->tag == Iex_Const
          && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U32
          && uInt_fits_in_16_bits(e->Iex.Binop.arg2->Iex.Const.con-> Ico.U32)) {
         return MIPSAMode_IR((Int) e->Iex.Binop.arg2->Iex.Const.con->Ico.U32,
                              iselWordExpr_R(env, e->Iex.Binop.arg1));
      }

      /* Add32(expr,expr) */
      if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_Add32) {
         HReg r_base = iselWordExpr_R(env, e->Iex.Binop.arg1);
         HReg r_idx = iselWordExpr_R(env, e->Iex.Binop.arg2);

         return MIPSAMode_RR(r_idx, r_base);
      }
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   return MIPSAMode_IR(0, iselWordExpr_R(env, e));
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
static HReg iselWordExpr_R(ISelEnv * env, IRExpr * e)
{
   HReg r = iselWordExpr_R_wrk(env, e);
   /* sanity checks ... */

   vassert(hregClass(r) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselWordExpr_R_wrk(ISelEnv * env, IRExpr * e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I1
           || ty == Ity_F32 || (ty == Ity_I64 && mode64)
           || (ty == Ity_I128 && mode64));

   switch (e->tag) {
      /* --------- TEMP --------- */
      case Iex_RdTmp:
         return lookupIRTemp(env, e->Iex.RdTmp.tmp);

      /* --------- LOAD --------- */
      case Iex_Load: {
         HReg r_dst = newVRegI(env);
         MIPSAMode *am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, ty);

         if (e->Iex.Load.end != Iend_LE
             && e->Iex.Load.end != Iend_BE)
            goto irreducible;

         addInstr(env, MIPSInstr_Load(toUChar(sizeofIRType(ty)),
                                      r_dst, am_addr, mode64));
         return r_dst;
      }

      /* --------- BINARY OP --------- */
      case Iex_Binop: {
         MIPSAluOp aluOp;
         MIPSShftOp shftOp;

         /* Is it an addition or logical style op? */
         switch (e->Iex.Binop.op) {
            case Iop_Add32:
               aluOp = Malu_ADD;
               break;
   
            case Iop_Sub8:
            case Iop_Sub16:
            case Iop_Sub32:
               aluOp = Malu_SUB;
               break;
   
            case Iop_And32:
            case Iop_And64:
               aluOp = Malu_AND;
               break;
   
            case Iop_Or32:
            case Iop_Or64:
               aluOp = Malu_OR;
               break;
   
            case Iop_Xor32:
            case Iop_Xor64:
               aluOp = Malu_XOR;
               break;
   
            default:
               aluOp = Malu_INVALID;
               break;
         }

         /* For commutative ops we assume any literal
            values are on the second operand. */
         if (aluOp != Malu_INVALID) {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            MIPSRH *ri_srcR = NULL;
            /* get right arg into an RH, in the appropriate way */
            switch (aluOp) {
               case Malu_ADD:
               case Malu_SUB:
                  ri_srcR = iselWordExpr_RH(env, True /*signed */ ,
                                            e->Iex.Binop.arg2);
                  break;
               case Malu_AND:
               case Malu_OR:
               case Malu_XOR:
                  ri_srcR = iselWordExpr_RH(env, False /*unsigned */,
                                            e->Iex.Binop.arg2);
                  break;
               default:
                  vpanic("iselWordExpr_R_wrk-aluOp-arg2");
            }
            addInstr(env, MIPSInstr_Alu(aluOp, r_dst, r_srcL, ri_srcR));
            return r_dst;
         }

         /* a shift? */
         switch (e->Iex.Binop.op) {
            case Iop_Shl32:
            case Iop_Shl64:
               shftOp = Mshft_SLL;
               break;
            case Iop_Shr32:
            case Iop_Shr64:
               shftOp = Mshft_SRL;
               break;
            case Iop_Sar32:
            case Iop_Sar64:
               shftOp = Mshft_SRA;
               break;
            default:
               shftOp = Mshft_INVALID;
               break;
         }

         /* we assume any literal values are on the second operand. */
         if (shftOp != Mshft_INVALID) {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            MIPSRH *ri_srcR = NULL;
            /* get right arg into an RH, in the appropriate way */
            switch (shftOp) {
               case Mshft_SLL:
               case Mshft_SRL:
               case Mshft_SRA:
                  ri_srcR = iselWordExpr_RH5u(env, e->Iex.Binop. arg2);
                  break;
               default:
                  vpanic("iselIntExpr_R_wrk-shftOp-arg2");
            }
            /* widen the left arg if needed */
            /*TODO do we need this? */
            if (ty == Ity_I8 || ty == Ity_I16)
               goto irreducible;
            if (ty == Ity_I64) {
               vassert(mode64);
               addInstr(env, MIPSInstr_Shft(shftOp, False/*64bit shift */,
                                            r_dst, r_srcL, ri_srcR));
            } else {
               addInstr(env, MIPSInstr_Shft(shftOp, True /*32bit shift */,
                                            r_dst, r_srcL, ri_srcR));
            }
            return r_dst;
         }

         /* Cmp*32*(x,y) ? */
         if (e->Iex.Binop.op == Iop_CmpEQ32
             || e->Iex.Binop.op == Iop_CmpNE32
             || e->Iex.Binop.op == Iop_CmpNE64
             || e->Iex.Binop.op == Iop_CmpLT32S
             || e->Iex.Binop.op == Iop_CmpLT32U
             || e->Iex.Binop.op == Iop_CmpLT64U
             || e->Iex.Binop.op == Iop_CmpLE32S
             || e->Iex.Binop.op == Iop_CmpLE64S
             || e->Iex.Binop.op == Iop_CmpLT64S
             || e->Iex.Binop.op == Iop_CmpEQ64) {

            Bool syned = (e->Iex.Binop.op == Iop_CmpLT32S
                         || e->Iex.Binop.op == Iop_CmpLE32S
                         || e->Iex.Binop.op == Iop_CmpLT64S
                         || e->Iex.Binop.op == Iop_CmpLE64S);
            Bool size32;
            HReg dst = newVRegI(env);
            HReg r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r2 = iselWordExpr_R(env, e->Iex.Binop.arg2);

            MIPSCondCode cc;

            switch (e->Iex.Binop.op) {
               case Iop_CmpEQ32:
                  cc = MIPScc_EQ;
                  size32 = True;
                  break;
               case Iop_CmpNE32:
                  cc = MIPScc_NE;
                  size32 = True;
                  break;
               case Iop_CmpNE64:
                  cc = MIPScc_NE;
                  size32 = True;
                  break;
               case Iop_CmpLT32S:
                  cc = MIPScc_LT;
                  size32 = True;
                  break;
               case Iop_CmpLT32U:
                  cc = MIPScc_LO;
                  size32 = True;
                  break;
               case Iop_CmpLT64U:
                  cc = MIPScc_LO;
                  size32 = False;
                  break;
               case Iop_CmpLE32S:
                  cc = MIPScc_LE;
                  size32 = True;
                  break;
               case Iop_CmpLE64S:
                  cc = MIPScc_LE;
                  size32 = False;
                  break;
               case Iop_CmpLT64S:
                  cc = MIPScc_LT;
                  size32 = False;
                  break;
               case Iop_CmpEQ64:
                  cc = MIPScc_EQ;
                  size32 = False;
                  break;
               default:
                  vpanic
                      ("iselCondCode(mips): CmpXX32 or CmpXX64");
            }

            addInstr(env, MIPSInstr_Cmp(syned, size32, dst, r1, r2, cc));
            return dst;
         }

         if (e->Iex.Binop.op == Iop_Max32U) {
            /*
               tmp = argR - argL;
               dst = argL;
               bltz tmp,2;
               dst = argR;

             */
            HReg argL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            MIPSRH *argR = iselWordExpr_RH(env, False /*signed */ ,
                                           e->Iex.Binop.arg2);
            HReg dst = newVRegI(env);
            HReg tmp = newVRegI(env);
            addInstr(env, MIPSInstr_Alu(Malu_SUB, tmp, argL, argR));
            addInstr(env, MIPSInstr_MovCond(dst, argL, argR, tmp, MIPScc_MI));

            return dst;
         }

         if (e->Iex.Binop.op == Iop_Mul32 || e->Iex.Binop.op == Iop_Mul64) {
            Bool sz32 = (e->Iex.Binop.op == Iop_Mul32);
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, MIPSInstr_Mul(False/*Unsigned or Signed */ ,
                                       False /*widen */ ,
                                       sz32 /*32bit or 64bit */,
                                       r_dst, r_srcL, r_srcR));
            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_MullU32 || e->Iex.Binop.op == Iop_MullS32) {
            HReg r_dst = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg tLo = newVRegI(env);
            HReg tLo_1 = newVRegI(env);
            HReg tHi_1 = newVRegI(env);
            HReg mask = newVRegI(env);

            Bool syned = toBool(e->Iex.Binop.op == Iop_MullS32);
            Bool size = toBool(e->Iex.Binop.op == Iop_MullS32)
                        || toBool(e->Iex.Binop.op == Iop_MullU32);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            addInstr(env, MIPSInstr_Mul(syned /*Unsigned or Signed */ ,
                                        True /*widen */ ,
                                        size /*32bit or 64bit mul */ ,
                                        r_dst, r_srcL, r_srcR));

            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, False, tHi_1,
                          tHi, MIPSRH_Imm(False, 32)));

            addInstr(env, MIPSInstr_LI(mask, 0xffffffff));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tLo_1, tLo,
                          MIPSRH_Reg(mask)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, tHi_1,
                          MIPSRH_Reg(tLo_1)));

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_CmpF64) {
            HReg r_srcL, r_srcR;
            {
               r_srcL = iselDblExpr(env, e->Iex.Binop.arg1);
               r_srcR = iselDblExpr(env, e->Iex.Binop.arg2);
            }
            HReg tmp = newVRegI(env);
            HReg r_ccMIPS = newVRegI(env);
            HReg r_ccIR = newVRegI(env);
            HReg r_ccIR_b0 = newVRegI(env);
            HReg r_ccIR_b2 = newVRegI(env);
            HReg r_ccIR_b6 = newVRegI(env);

            /* Create in dst, the IRCmpF64Result encoded result. */
            // chech for EQ
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP, tmp, r_srcL, r_srcR,
                                              toUChar(2)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, r_ccMIPS, tmp,
                                         MIPSRH_Imm(False, 22)));
            // chech for UN
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP, tmp, r_srcL, r_srcR,
                                              toUChar(1)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tmp, tmp,
                                        MIPSRH_Imm(False, 23)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccMIPS, r_ccMIPS,
                                        MIPSRH_Reg(tmp)));
            // chech for LT
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP, tmp, r_srcL, r_srcR,
                                              toUChar(12)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tmp,
                                         tmp, MIPSRH_Imm(False, 21)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccMIPS, r_ccMIPS,
                                        MIPSRH_Reg(tmp)));
            // chech for GT
            addInstr(env, MIPSInstr_FpCompare(Mfp_CMP, tmp, r_srcL, r_srcR,
                                              toUChar(15)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tmp, tmp,
                                         MIPSRH_Imm(False, 20)));

            addInstr(env, MIPSInstr_Alu(Malu_NOR, tmp, tmp, MIPSRH_Reg(tmp)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, tmp,
                                        MIPSRH_Imm(False, 8)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccMIPS, r_ccMIPS,
                                        MIPSRH_Reg(tmp)));
            /* Map compare result from PPC to IR,
               conforming to CmpF64 definition. */
            /*
               FP cmp result | MIPS | IR
               --------------------------
               UN            | 0x1 | 0x45
               EQ            | 0x2 | 0x40
               GT            | 0x4 | 0x00
               LT            | 0x8 | 0x01
             */

            // r_ccIR_b0 = r_ccPPC[0] | r_ccPPC[3]
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True, r_ccIR_b0, r_ccMIPS,
                          MIPSRH_Imm(False, 0x3)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccIR_b0, r_ccMIPS,
                          MIPSRH_Reg(r_ccIR_b0)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_ccIR_b0, r_ccIR_b0,
                          MIPSRH_Imm(False, 0x1)));

            // r_ccIR_b2 = r_ccPPC[0]
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, r_ccIR_b2, r_ccMIPS,
                          MIPSRH_Imm(False, 0x2)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_ccIR_b2, r_ccIR_b2,
                          MIPSRH_Imm(False, 0x4)));

            // r_ccIR_b6 = r_ccPPC[0] | r_ccPPC[1]
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True, r_ccIR_b6,
                          r_ccMIPS, MIPSRH_Imm(False, 0x1)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccIR_b6, r_ccMIPS,
                          MIPSRH_Reg(r_ccIR_b6)));
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, r_ccIR_b6, r_ccIR_b6,
                          MIPSRH_Imm(False, 0x6)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_ccIR_b6, r_ccIR_b6,
                          MIPSRH_Imm(False, 0x40)));

            // r_ccIR = r_ccIR_b0 | r_ccIR_b2 | r_ccIR_b6
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccIR, r_ccIR_b0,
                          MIPSRH_Reg(r_ccIR_b2)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_ccIR, r_ccIR,
                          MIPSRH_Reg(r_ccIR_b6)));
            return r_ccIR;
         }

         if (e->Iex.Binop.op == Iop_DivModU64to32 ||
             e->Iex.Binop.op == Iop_DivModS64to32) {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg mask = newVRegI(env);
            HReg tLo_1 = newVRegI(env);
            HReg tHi_1 = newVRegI(env);
            HReg r_dst = newVRegI(env);
            Bool syned = toBool(e->Iex.Binop.op == Iop_DivModS64to32);

            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);

            addInstr(env, MIPSInstr_Div(syned, True, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, False, tHi_1, tHi,
                                         MIPSRH_Imm(False, 32)));

            addInstr(env, MIPSInstr_LI(mask, 0xffffffff));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tLo_1, tLo,
                          MIPSRH_Reg(mask)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, tHi_1,
                          MIPSRH_Reg(tLo_1)));

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_32HLto64) {
            vassert(mode64);
            HReg tHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg tLo = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tLo_1 = newVRegI(env);
            HReg tHi_1 = newVRegI(env);
            HReg r_dst = newVRegI(env);
            HReg mask = newVRegI(env);

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, False, tHi_1, tHi,
                                         MIPSRH_Imm(False, 32)));

            addInstr(env, MIPSInstr_LI(mask, 0xffffffff));
            addInstr(env, MIPSInstr_Alu(Malu_AND, tLo_1, tLo,
                          MIPSRH_Reg(mask)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, tHi_1,
                          MIPSRH_Reg(tLo_1)));

            return r_dst;
         }

         if (e->Iex.Binop.op == Iop_F64toI32S) {
            HReg valD = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg valS = newVRegF(env);
            HReg r_dst = newVRegI(env);
            MIPSAMode *am_addr;

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTWD, valS, valD));
            set_MIPS_rounding_default(env);

            sub_from_sp(env, 16);   // Move SP down 16 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            // store as F32
            addInstr(env, MIPSInstr_FpLdSt(False/*store */ , 4, valS, am_addr));
            // load as I32                              
            addInstr(env, MIPSInstr_Load(4, r_dst, am_addr, mode64));

            add_to_sp(env, 16);  // Reset SP

            return r_dst;
         }

      break;
   }

      /* --------- UNARY OP --------- */
   case Iex_Unop: {
      IROp op_unop = e->Iex.Unop.op;

      switch (op_unop) {
         case Iop_1Sto32:
         case Iop_8Sto32:
         case Iop_16Sto32:
         case Iop_16Sto64:
         case Iop_8Sto64:
         case Iop_1Sto64: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            Bool sz32;
            UShort amt;
            switch (op_unop) {
               case Iop_1Sto32:
                  amt = 31;
                  sz32 = True;
                  break;
               case Iop_16Sto32:
                  amt = 16;
                  sz32 = True;
                  break;
               case Iop_16Sto64:
                  amt = 48;
                  sz32 = False;
                  break;
               case Iop_8Sto32:
                  amt = 24;
                  sz32 = True;
                  break;
               case Iop_8Sto64:
                  amt = 56;
                  sz32 = False;
                  break;
               case Iop_1Sto64:
                  amt = 63;
                  sz32 = False;
                  break;
               default:
                  vassert(0);
            }

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, sz32, r_dst, r_src,
                                         MIPSRH_Imm(False, amt)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, sz32, r_dst, r_dst,
                                         MIPSRH_Imm(False, amt)));
            return r_dst;
         }

         /*not(x) = nor(x,x) */
         case Iop_Not1: {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
            MIPSRH *r_srcR = MIPSRH_Reg(r_srcL);

            addInstr(env, MIPSInstr_LI(r_dst, 0x1));
            addInstr(env, MIPSInstr_Alu(Malu_SUB, r_dst, r_dst, r_srcR));
            return r_dst;
         }

         case Iop_Not32:
         case Iop_Not64: {
            HReg r_dst = newVRegI(env);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
            MIPSRH *r_srcR = MIPSRH_Reg(r_srcL);

            addInstr(env, MIPSInstr_Alu(Malu_NOR, r_dst, r_srcL, r_srcR));
            return r_dst;
         }

         case Iop_ReinterpF32asI32: {
            MIPSAMode *am_addr;
            HReg fr_src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg r_dst = newVRegI(env);

            sub_from_sp(env, 16);   // Move SP down 16 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            // store as F32
            addInstr(env, MIPSInstr_FpLdSt(False/*store */ , 4, fr_src,
                                           am_addr));
            // load as Ity_I32
            addInstr(env, MIPSInstr_Load(4, r_dst, am_addr, mode64));

            add_to_sp(env, 16);  // Reset SP
            return r_dst;
         }

         case Iop_ReinterpF64asI64: {
            vassert(mode64);
            MIPSAMode *am_addr;
            HReg fr_src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg r_dst = newVRegI(env);

            sub_from_sp(env, 16);   // Move SP down 16 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            // store as F64
            addInstr(env, MIPSInstr_FpLdSt(False/*store */ , 8, fr_src,
                                           am_addr));
            // load as Ity_I64
            addInstr(env, MIPSInstr_Load(8, r_dst, am_addr, mode64));

            add_to_sp(env, 16);  // Reset SP
            return r_dst;
         }

         case Iop_32to8:
         case Iop_32to16:
            return iselWordExpr_R(env, e->Iex.Unop.arg);

         case Iop_64to8: {
            vassert(mode64);
            HReg r_src, r_dst;
            r_dst = newVRegI(env);
            r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_dst, r_src,
                          MIPSRH_Imm(False, 0xFF)));
            return r_dst;
         }
   
         case Iop_16Uto32:
         case Iop_8Uto32:
         case Iop_1Uto32: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            UShort amt;
            switch (op_unop) {
               case Iop_1Uto32:
               case Iop_1Uto8:
                  amt = 31;
                  break;
   
               case Iop_16Uto32:
                  amt = 16;
                  break;
   
               default:
                  amt = 24;
                  break;
            }

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, r_dst, r_src,
                          MIPSRH_Imm(False, amt)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, True, r_dst, r_dst,
                          MIPSRH_Imm(False, amt)));
            return r_dst;
         }

         case Iop_8Uto16:
         case Iop_8Uto64:
         case Iop_16Uto64: {
            vassert(mode64);
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env,  e->Iex.Unop.arg);
            UShort mask = toUShort(op_unop == Iop_16Uto64 ? 0xFFFF :
                                   op_unop == Iop_16Uto32 ? 0xFFFF : 0xFF);
            addInstr(env, MIPSInstr_Alu(Malu_AND, r_dst, r_src,
                          MIPSRH_Imm(False, mask)));
            return r_dst;
         }

         case Iop_32Uto64: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            vassert(mode64);
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, False/*!32bit shift */,
                                         r_dst, r_src, MIPSRH_Imm(False, 32)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRL, False/*!32bit shift */,
                                         r_dst, r_dst, MIPSRH_Imm(False, 32)));
            return r_dst;
         }

         case Iop_1Uto64:
            vassert(mode64);
            return iselWordExpr_R(env, e->Iex.Unop.arg);

         case Iop_64HIto32: {
            HReg rHi, rLo;
            iselInt64Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
            return rHi;
         }

         case Iop_64to32: {
            HReg rHi, rLo;
            iselInt64Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
            return rLo;
         }
   
         case Iop_64to16: {
            vassert(env->mode64);
            HReg r_dst = newVRegI(env);
            r_dst = iselWordExpr_R(env, e->Iex.Unop.arg);
            return r_dst;
         }
   
         case Iop_32Sto64: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            vassert(mode64);
            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True/*!32bit shift */,
                                         r_dst, r_src, MIPSRH_Imm(True, 0)));
            return r_dst;
         }
   
         case Iop_CmpNEZ8: {
            HReg r_dst = newVRegI(env);
            HReg tmp = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

            MIPSCondCode cc;

            cc = MIPScc_NE;
            addInstr(env, MIPSInstr_Alu(Malu_AND, tmp, r_src,
                                        MIPSRH_Imm(False, 0xFF)));
            addInstr(env, MIPSInstr_Cmp(False, True, r_dst, tmp,
                                        hregMIPS_GPR0(mode64), cc));
            return r_dst;
         }

         case Iop_CmpNEZ32: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

            MIPSCondCode cc;

            cc = MIPScc_NE;

            addInstr(env, MIPSInstr_Cmp(False, True, r_dst, r_src,
                                        hregMIPS_GPR0(mode64), cc));
            return r_dst;
         }

         case Iop_CmpwNEZ32: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);

            addInstr(env, MIPSInstr_Alu(Malu_SUB, r_dst, hregMIPS_GPR0(mode64),
                          MIPSRH_Reg(r_src)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, r_dst,
                                        MIPSRH_Reg(r_src)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, r_dst, r_dst,
                                         MIPSRH_Imm(False, 31)));
            return r_dst;
         }

         case Iop_Left8:
         case Iop_Left32:
         case Iop_Left64: {
            if (op_unop == Iop_Left64 && !mode64)
               goto irreducible;
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, MIPSInstr_Alu(Malu_SUB, r_dst, hregMIPS_GPR0(mode64),
                          MIPSRH_Reg(r_src)));
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_dst, r_dst,
                          MIPSRH_Reg(r_src)));
            return r_dst;
         }

         case Iop_Clz32: {
            HReg r_dst = newVRegI(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, MIPSInstr_Unary(Mun_CLZ, r_dst, r_src));
            return r_dst;
         }

         case Iop_CmpNEZ64: {
            HReg hi, lo;
            HReg r_dst = newVRegI(env);
            HReg r_src;
            r_src = newVRegI(env);
            iselInt64Expr(&hi, &lo, env, e->Iex.Unop.arg);
            addInstr(env, MIPSInstr_Alu(Malu_OR, r_src, lo, MIPSRH_Reg(hi)));
            MIPSCondCode cc;

            cc = MIPScc_NE;

            addInstr(env, MIPSInstr_Cmp(False, !(env->mode64), r_dst, r_src,
                                        hregMIPS_GPR0(mode64), cc));
            return r_dst;
         }

         case Iop_CmpwNEZ64: {
            HReg tmp1;
            HReg tmp2 = newVRegI(env);
            vassert(env->mode64);
            tmp1 = iselWordExpr_R(env, e->Iex.Unop.arg);

            addInstr(env, MIPSInstr_Alu(Malu_SUB, tmp2, hregMIPS_GPR0(mode64),
                          MIPSRH_Reg(tmp1)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, tmp2, tmp2, MIPSRH_Reg(tmp1)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, False, tmp2, tmp2,
                                         MIPSRH_Imm (False, 63)));
            return tmp2;
         }

         case Iop_128HIto64: {
            vassert(mode64);
            HReg rHi, rLo;
            iselInt128Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
            return rHi; /* and abandon rLo .. poor wee thing :-) */
         }

         case Iop_128to64: {
            vassert(mode64);
            HReg rHi, rLo;
            iselInt128Expr(&rHi, &rLo, env, e->Iex.Unop.arg);
            return rLo; /* and abandon rLo .. poor wee thing :-) */
         }

         default:
            break;
      }
      break;
   }

      /* --------- GET --------- */
   case Iex_Get: {
      if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32
          || ((ty == Ity_I64) && mode64)) {
         HReg r_dst = newVRegI(env);

         MIPSAMode *am_addr = MIPSAMode_IR(e->Iex.Get.offset,
                                           GuestStatePointer(mode64));
         addInstr(env, MIPSInstr_Load(toUChar(sizeofIRType(ty)), r_dst, am_addr,
                                      mode64));
         return r_dst;
      }
      break;
   }

      /* --------- MULTIPLEX --------- */
   case Iex_Mux0X: {
      if ((ty == Ity_I8 || ty == Ity_I16 ||
           ty == Ity_I32 || ((ty == Ity_I64))) &&
           typeOfIRExpr(env->type_env, e->Iex.Mux0X.cond) == Ity_I8) {
         /*
          * r_dst = cond && rX
          * cond = not(cond)
          * tmp = cond && r0
          * r_dst = tmp + r_dst
          */
         HReg r0 = iselWordExpr_R(env, e->Iex.Mux0X.expr0);
         HReg rX = iselWordExpr_R(env, e->Iex.Mux0X.exprX);
         HReg r_cond = iselWordExpr_R(env, e->Iex.Mux0X.cond);
         HReg r_dst = newVRegI(env);
         HReg r_tmp = newVRegI(env);
         HReg r_tmp1 = newVRegI(env);
         HReg r_cond_neg = newVRegI(env);

         addInstr(env, MIPSInstr_Alu(Malu_AND, r_tmp, r_cond, MIPSRH_Reg(rX)));
         addInstr(env, MIPSInstr_Alu(Malu_NOR, r_cond_neg, r_cond,
                       MIPSRH_Reg(r_cond)));
         addInstr(env, MIPSInstr_Alu(Malu_AND, r_tmp1, r_cond_neg,
                       MIPSRH_Reg(r0)));
         addInstr(env, MIPSInstr_Alu(Malu_ADD, r_dst, r_tmp,
                       MIPSRH_Reg(r_tmp1)));

         return r_dst;
      }
      break;
   }

      /* --------- LITERAL --------- */
      /* 32/16/8-bit literals */
   case Iex_Const: {
      Long l;
      HReg r_dst = newVRegI(env);
      IRConst *con = e->Iex.Const.con;
      switch (con->tag) {
         case Ico_U64:
            if (!mode64)
               goto irreducible;
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
            vpanic("iselIntExpr_R.const(mips)");
      }
      addInstr(env, MIPSInstr_LI(r_dst, (ULong) l));
      return r_dst;
   }

      /* --------- CCALL --------- */
   case Iex_CCall: {
      HReg r_dst = newVRegI(env);
      vassert(ty == e->Iex.CCall.retty);

      /* be very restrictive for now.  Only 32/64-bit ints allowed
         for args, and 32 bits for return type. */
      if (e->Iex.CCall.retty != Ity_I32 && !mode64)
         goto irreducible;

      /* Marshal args, do the call, clear stack. */
      doHelperCall(env, False, NULL, e->Iex.CCall.cee, e->Iex.CCall.args);
      addInstr(env, mk_iMOVds_RR(r_dst, hregMIPS_GPR2(mode64)));
      return r_dst;
   }

   default:
      break;
   }        /* end switch(e->tag) */

   /* We get here if no pattern matched. */
   irreducible:
      vex_printf("--------------->\n");
      if (e->tag == Iex_RdTmp)
         vex_printf("Iex_RdTmp \n");
      ppIRExpr(e);

      vpanic("iselWordExpr_R(mips): cannot reduce tree");
}

/* --------------------- RH --------------------- */

/* Compute an I8/I16/I32 (and I64, in 64-bit mode) into a RH
   (reg-or-halfword-immediate).  It's important to specify whether the
   immediate is to be regarded as signed or not.  If yes, this will
   never return -32768 as an immediate; this guaranteed that all
   signed immediates that are return can have their sign inverted if
   need be. */

static MIPSRH *iselWordExpr_RH(ISelEnv * env, Bool syned, IRExpr * e)
{
   MIPSRH *ri = iselWordExpr_RH_wrk(env, syned, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Mrh_Imm:
         vassert(ri->Mrh.Imm.syned == syned);
         if (syned)
            vassert(ri->Mrh.Imm.imm16 != 0x8000);
         return ri;
      case Mrh_Reg:
         vassert(hregClass(ri->Mrh.Reg.reg) == HRcGPR(env->mode64));
         vassert(hregIsVirtual(ri->Mrh.Reg.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RH: unknown mips RH tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSRH *iselWordExpr_RH_wrk(ISelEnv * env, Bool syned, IRExpr * e)
{
   ULong u;
   Long l;
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 ||
          ((ty == Ity_I64) && env->mode64));

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      IRConst *con = e->Iex.Const.con;
      /* What value are we aiming to generate? */
      switch (con->tag) {
         /* Note: Not sign-extending - we carry 'syned' around */
         case Ico_U64:
            vassert(env->mode64);
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
            vpanic("iselIntExpr_RH.Iex_Const(mips)");
      }
      l = (Long) u;
      /* Now figure out if it's representable. */
      if (!syned && u <= 65535) {
         return MIPSRH_Imm(False /*unsigned */ , toUShort(u & 0xFFFF));
      }
      if (syned && l >= -32767 && l <= 32767) {
         return MIPSRH_Imm(True /*signed */ , toUShort(u & 0xFFFF));
      }
      /* no luck; use the Slow Way. */
   }
   /* default case: calculate into a register and return that */
   return MIPSRH_Reg(iselWordExpr_R(env, e));
}

/* --------------------- RH5u --------------------- */

/* Compute an I8 into a reg-or-5-bit-unsigned-immediate, the latter
   being an immediate in the range 1 .. 31 inclusive.  Used for doing
   shift amounts. */

static MIPSRH *iselWordExpr_RH5u(ISelEnv * env, IRExpr * e)
{
   MIPSRH *ri;
   ri = iselWordExpr_RH5u_wrk(env, e);
   /* sanity checks ... */
   switch (ri->tag) {
      case Mrh_Imm:
         vassert(ri->Mrh.Imm.imm16 >= 1 && ri->Mrh.Imm.imm16 <= 31);
         vassert(!ri->Mrh.Imm.syned);
         return ri;
      case Mrh_Reg:
         vassert(hregClass(ri->Mrh.Reg.reg) == HRcInt32);
         vassert(hregIsVirtual(ri->Mrh.Reg.reg));
         return ri;
      default:
         vpanic("iselIntExpr_RH5u: unknown mips RH tag");
   }
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSRH *iselWordExpr_RH5u_wrk(ISelEnv * env, IRExpr * e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_I8);

   /* special case: immediate */
   if (e->tag == Iex_Const
       && e->Iex.Const.con->tag == Ico_U8
       && e->Iex.Const.con->Ico.U8 >= 1 && e->Iex.Const.con->Ico.U8 <= 31) {
      return MIPSRH_Imm(False /*unsigned */ , e->Iex.Const.con->Ico.U8);
   }

   /* default case: calculate into a register and return that */
   return MIPSRH_Reg(iselWordExpr_R(env, e));
}

/* --------------------- CONDCODE --------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static MIPSCondCode iselCondCode(ISelEnv * env, IRExpr * e)
{
   MIPSCondCode cc = iselCondCode_wrk(env,e);
   vassert(cc != MIPScc_NV);
   return cc;
}

/* DO NOT CALL THIS DIRECTLY ! */
static MIPSCondCode iselCondCode_wrk(ISelEnv * env, IRExpr * e)
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I1);
   /* Cmp*32*(x,y) ? */
   if (e->Iex.Binop.op == Iop_CmpEQ32
       || e->Iex.Binop.op == Iop_CmpNE32
       || e->Iex.Binop.op == Iop_CmpNE64
       || e->Iex.Binop.op == Iop_CmpLT32S
       || e->Iex.Binop.op == Iop_CmpLT32U
       || e->Iex.Binop.op == Iop_CmpLT64U
       || e->Iex.Binop.op == Iop_CmpLE32S
       || e->Iex.Binop.op == Iop_CmpLE64S
       || e->Iex.Binop.op == Iop_CmpLT64S
       || e->Iex.Binop.op == Iop_CmpEQ64) {

      Bool syned = (e->Iex.Binop.op == Iop_CmpLT32S
                   || e->Iex.Binop.op == Iop_CmpLE32S
                   || e->Iex.Binop.op == Iop_CmpLT64S
                   || e->Iex.Binop.op == Iop_CmpLE64S);
      Bool size32;
      HReg dst = newVRegI(env);
      HReg r1 = iselWordExpr_R(env, e->Iex.Binop.arg1);
      HReg r2 = iselWordExpr_R(env, e->Iex.Binop.arg2);

      MIPSCondCode cc;

      switch (e->Iex.Binop.op) {
         case Iop_CmpEQ32:
            cc = MIPScc_EQ;
            size32 = True;
            break;
         case Iop_CmpNE32:
            cc = MIPScc_NE;
            size32 = True;
            break;
         case Iop_CmpNE64:
            cc = MIPScc_NE;
            size32 = True;
            break;
         case Iop_CmpLT32S:
            cc = MIPScc_LT;
            size32 = True;
            break;
         case Iop_CmpLT32U:
            cc = MIPScc_LO;
            size32 = True;
            break;
         case Iop_CmpLT64U:
            cc = MIPScc_LO;
            size32 = False;
            break;
         case Iop_CmpLE32S:
            cc = MIPScc_LE;
            size32 = True;
            break;
         case Iop_CmpLE64S:
            cc = MIPScc_LE;
            size32 = False;
            break;
         case Iop_CmpLT64S:
            cc = MIPScc_LT;
            size32 = False;
            break;
         case Iop_CmpEQ64:
            cc = MIPScc_EQ;
            size32 = False;
            break;
         default:
            vpanic
                ("iselCondCode(mips): CmpXX32 or CmpXX64");
      }

      addInstr(env, MIPSInstr_Cmp(syned, size32, dst, r1, r2, cc));
      // Store result to guest_COND
      MIPSAMode *am_addr = MIPSAMode_IR(0, GuestStatePointer(mode64));

      addInstr(env, MIPSInstr_Store(4,
               MIPSAMode_IR(am_addr->Mam.IR.index + 316, am_addr->Mam.IR.base),
               dst, mode64));
      return cc;
   }
   if (e->Iex.Binop.op == Iop_Not1) {
      HReg r_dst = newVRegI(env);
      HReg r_srcL = iselWordExpr_R(env, e->Iex.Unop.arg);
      MIPSRH *r_srcR = MIPSRH_Reg(r_srcL);

      addInstr(env, MIPSInstr_LI(r_dst, 0x1));
      addInstr(env, MIPSInstr_Alu(Malu_SUB, r_dst, r_dst, r_srcR));
      // Store result to guest_COND
      MIPSAMode *am_addr = MIPSAMode_IR(0, GuestStatePointer(mode64));

      addInstr(env, MIPSInstr_Store(4,
               MIPSAMode_IR(am_addr->Mam.IR.index + 316, am_addr->Mam.IR.base),
               r_dst, mode64));
      return MIPScc_NE;
   }
   if (e->tag == Iex_RdTmp || e->tag == Iex_Unop) {
      HReg r_dst = iselWordExpr_R_wrk(env, e);
      // Store result to guest_COND
      MIPSAMode *am_addr = MIPSAMode_IR(0, GuestStatePointer(mode64));

      addInstr(env, MIPSInstr_Store(4,
               MIPSAMode_IR(am_addr->Mam.IR.index + 316, am_addr->Mam.IR.base),
               r_dst, mode64));
      return MIPScc_EQ;
   }

   vex_printf("iselCondCode(mips): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselCondCode(mips)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (128 bit)               ---*/
/*---------------------------------------------------------*/

/* 64-bit mode ONLY: compute a 128-bit value into a register pair,
   which is returned as the first two parameters.  As with
   iselWordExpr_R, these may be either real or virtual regs; in any
   case they must not be changed by subsequent code emitted by the
   caller.  */

static void iselInt128Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e)
{
   vassert(env->mode64);
   iselInt128Expr_wrk(rHi, rLo, env, e);
#  if 0
   vex_printf("\n");
   ppIRExpr(e);
   vex_printf("\n");
#  endif
   vassert(hregClass(*rHi) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcGPR(env->mode64));
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt128Expr_wrk(HReg * rHi, HReg * rLo, ISelEnv * env,
                               IRExpr * e)
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I128);

   /* read 128-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair(rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         /* 64 x 64 -> 128 multiply */
         case Iop_MullU64:
         case Iop_MullS64:
            {
               HReg tLo = newVRegI(env);
               HReg tHi = newVRegI(env);
               Bool syned = toBool(e->Iex.Binop.op == Iop_MullS64);
               HReg r_dst = newVRegI(env);
               HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
               HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
               addInstr(env, MIPSInstr_Mul(syned, True, False /*64bit mul */ ,
                                           r_dst, r_srcL, r_srcR));
               addInstr(env, MIPSInstr_Mfhi(tHi));
               addInstr(env, MIPSInstr_Mflo(tLo));
               *rHi = tHi;
               *rLo = tLo;
               return;
            }
   
         /* 64HLto128(e1,e2) */
         case Iop_64HLto128:
            *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
            *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2);
            return;
   
         case Iop_DivModS64to64: {
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            Bool syned = toBool(e->Iex.Binop.op == Iop_DivModS64to64);

            addInstr(env, MIPSInstr_Div(syned, False, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }
   
         case Iop_DivModU128to64: {
            vassert(mode64);
            HReg rHi1, rLo1;
            iselInt128Expr(&rHi1, &rLo1, env, e->Iex.Binop.arg1);

            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            Bool syned = toBool(e->Iex.Binop.op == Iop_DivModS128to64);

            addInstr(env, MIPSInstr_Div(syned, False, rLo1, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }
   
         default:
            break;
      }
   }
   vex_printf("iselInt128Expr(mips64): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselInt128Expr(mips64)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64 bit)                ---*/
/*---------------------------------------------------------*/

/* 32-bit mode ONLY. Compute a 64-bit value into the register 
 * pair HI, LO. HI and LO must not be changed by subsequent
 *  code emitted by the caller. */

static void iselInt64Expr(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e)
{
   vassert(!env->mode64);
   iselInt64Expr_wrk(rHi, rLo, env, e);
   vassert(hregClass(*rHi) == HRcInt32);
   vassert(hregIsVirtual(*rHi));
   vassert(hregClass(*rLo) == HRcInt32);
   vassert(hregIsVirtual(*rLo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt64Expr_wrk(HReg * rHi, HReg * rLo, ISelEnv * env, IRExpr * e)
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I64);

   /* read 64-bit IRTemp */
   if (e->tag == Iex_RdTmp) {
      lookupIRTemp64(rHi, rLo, env, e->Iex.RdTmp.tmp);
      return;
   }
   /* 64-bit load */
   if (e->tag == Iex_Load) {
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      HReg r_addr = iselWordExpr_R(env, e->Iex.Load.addr);
      addInstr(env, MIPSInstr_Load(4, tHi, MIPSAMode_IR(0, r_addr), mode64));
      addInstr(env, MIPSInstr_Load(4, tLo, MIPSAMode_IR(4, r_addr), mode64));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit literal */
   if (e->tag == Iex_Const) {
      ULong w64 = e->Iex.Const.con->Ico.U64;
      UInt wHi = toUInt(w64 >> 32);
      UInt wLo = toUInt(w64);
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);
      vassert(e->Iex.Const.con->tag == Ico_U64);

      if (wLo == wHi) {
         /* Save a precious Int register in this special case. */
         addInstr(env, MIPSInstr_LI(tLo, (ULong) wLo));
         *rHi = tLo;
         *rLo = tLo;
      } else {
         addInstr(env, MIPSInstr_LI(tHi, (ULong) wHi));
         addInstr(env, MIPSInstr_LI(tLo, (ULong) wLo));
         *rHi = tHi;
         *rLo = tLo;
      }

      return;
   }

   /* 64-bit GET */
   if (e->tag == Iex_Get) {
      HReg tLo = newVRegI(env);
      HReg tHi = newVRegI(env);

      MIPSAMode *am_addr = MIPSAMode_IR(e->Iex.Get.offset,
                                        GuestStatePointer(mode64));
      addInstr(env, MIPSInstr_Load(4, tLo, am_addr, mode64));
      addInstr(env, MIPSInstr_Load(4, tHi, nextMIPSAModeInt(am_addr), mode64));
      *rHi = tHi;
      *rLo = tLo;
      return;
   }

   /* 64-bit Mux0X */
   if (e->tag == Iex_Mux0X) {
      HReg expr0Lo, expr0Hi;
      HReg exprXLo, exprXHi;
      HReg tmpHi = newVRegI(env);
      HReg tmpLo = newVRegI(env);
      HReg tmp1Hi = newVRegI(env);
      HReg tmp1Lo = newVRegI(env);
      HReg r_cond = iselWordExpr_R(env, e->Iex.Mux0X.cond);
      HReg r_cond_neg = newVRegI(env);
      HReg desLo = newVRegI(env);
      HReg desHi = newVRegI(env);

      /* expr0Hi:expr0Lo = expr0 */
      /* exprXHi:exprXLo = exprX */
      iselInt64Expr(&expr0Hi, &expr0Lo, env, e->Iex.Mux0X.expr0);
      iselInt64Expr(&exprXHi, &exprXLo, env, e->Iex.Mux0X.exprX);

      addInstr(env, MIPSInstr_Alu(Malu_AND, tmpLo, r_cond,
                                  MIPSRH_Reg(exprXLo)));
      addInstr(env, MIPSInstr_Alu(Malu_AND, tmpHi, r_cond,
                                  MIPSRH_Reg(exprXHi)));
      addInstr(env, MIPSInstr_Alu(Malu_NOR, r_cond_neg, r_cond,
                                  MIPSRH_Reg(r_cond)));
      addInstr(env, MIPSInstr_Alu(Malu_AND, tmp1Lo, r_cond_neg,
                                  MIPSRH_Reg(exprXLo)));
      addInstr(env, MIPSInstr_Alu(Malu_AND, tmp1Hi, r_cond_neg,
                                  MIPSRH_Reg(exprXHi)));
      addInstr(env, MIPSInstr_Alu(Malu_ADD, desLo, tmpLo,
                                  MIPSRH_Reg(tmp1Lo)));
      addInstr(env, MIPSInstr_Alu(Malu_ADD, desHi, tmpHi,
                                  MIPSRH_Reg(tmp1Hi)));
      *rHi = desHi;
      *rLo = desLo;
      return;
   }

   /* --------- BINARY ops --------- */
   if (e->tag == Iex_Binop) {
      IROp op_binop = e->Iex.Binop.op;
      switch (op_binop) {
         /* 32 x 32 -> 64 multiply */
         /* Add64 */
         case Iop_Add64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tHi = newVRegI(env);
            HReg tLo = newVRegI(env);
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, MIPSInstr_Alu(Malu_ADD, tHi, xHi, MIPSRH_Reg(yHi)));
            addInstr(env, MIPSInstr_Alu(Malu_ADD, tLo, xLo, MIPSRH_Reg(yLo)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }
         case Iop_MullU32:
         case Iop_MullS32: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg r_dst = newVRegI(env);
            Bool syned = toBool(op_binop == Iop_MullS32);
            HReg r_srcL = iselWordExpr_R(env, e->Iex.Binop.arg1);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);

            addInstr(env, MIPSInstr_Mul(syned/*Unsigned or Signed */ ,
                                        True /*widen */ , True,
                                        r_dst, r_srcL, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
            *rHi = tHi;
            *rLo = tLo;

            return;
         }
         case Iop_DivModS64to32:
         case Iop_DivModU64to32: {
            HReg r_sHi, r_sLo;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            Bool syned = toBool(op_binop == Iop_DivModS64to32);
            HReg r_srcR = iselWordExpr_R(env, e->Iex.Binop.arg2);

            iselInt64Expr(&r_sHi, &r_sLo, env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_Div(syned, True, r_sLo, r_srcR));
            addInstr(env, MIPSInstr_Mfhi(tHi));
            addInstr(env, MIPSInstr_Mflo(tLo));
            *rHi = tHi;
            *rLo = tLo;

            return;
         }

            /* 32HLto64(e1,e2) */
         case Iop_32HLto64:
            *rHi = iselWordExpr_R(env, e->Iex.Binop.arg1);
            *rLo = iselWordExpr_R(env, e->Iex.Binop.arg2);

            return;
            /* Or64/And64/Xor64 */
         case Iop_Or64:
         case Iop_And64:
         case Iop_Xor64: {
            HReg xLo, xHi, yLo, yHi;
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            MIPSAluOp op = (op_binop == Iop_Or64) ? Malu_OR :
                           (op_binop == Iop_And64) ? Malu_AND : Malu_XOR;
            iselInt64Expr(&xHi, &xLo, env, e->Iex.Binop.arg1);
            iselInt64Expr(&yHi, &yLo, env, e->Iex.Binop.arg2);
            addInstr(env, MIPSInstr_Alu(op, tHi, xHi, MIPSRH_Reg(yHi)));
            addInstr(env, MIPSInstr_Alu(op, tLo, xLo, MIPSRH_Reg(yLo)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         default:
            break;
      }
   }

   /* --------- UNARY ops --------- */
   if (e->tag == Iex_Unop) {

      switch (e->Iex.Unop.op) {
         case Iop_1Sto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
            HReg tmp = newVRegI(env);

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp, src,
                          MIPSRH_Imm(False, 31)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tmp, src,
                          MIPSRH_Imm(False, 31)));

            addInstr(env, mk_iMOVds_RR(tHi, tmp));
            addInstr(env, mk_iMOVds_RR(tLo, tmp));

            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32Sto64(e) */
         case Iop_32Sto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVds_RR(tHi, src));
            addInstr(env, mk_iMOVds_RR(tLo, src));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tHi, tHi,
                          MIPSRH_Imm(False, 31)));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }

         /* 32Uto64(e) */
         case Iop_32Uto64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            HReg src = iselWordExpr_R(env, e->Iex.Unop.arg);
            addInstr(env, mk_iMOVds_RR(tLo, src));
            addInstr(env, MIPSInstr_Alu(Malu_ADD, tHi, hregMIPS_GPR0(mode64),
                          MIPSRH_Reg(hregMIPS_GPR0(mode64))));
            *rHi = tHi;
            *rLo = tLo;
            return;
         }
   
         case Iop_CmpwNEZ64: {
            HReg srcLo, srcHi;
            HReg tmp1 = newVRegI(env);
            HReg tmp2 = newVRegI(env);
            /* srcHi:srcLo = arg */
            iselInt64Expr(&srcHi, &srcLo, env, e->Iex.Unop.arg);
            /* tmp1 = srcHi | srcLo */
            addInstr(env, MIPSInstr_Alu(Malu_OR, tmp1, srcLo,
                                        MIPSRH_Reg(srcHi)));
            /* tmp2 = (tmp1 | -tmp1) >>s 31 */

            addInstr(env, MIPSInstr_Alu(Malu_SUB, tmp2, hregMIPS_GPR0(mode64),
                                        MIPSRH_Reg(tmp1)));

            addInstr(env, MIPSInstr_Alu(Malu_OR, tmp2, tmp2, MIPSRH_Reg(tmp1)));
            addInstr(env, MIPSInstr_Shft(Mshft_SRA, True, tmp2, tmp2,
                          MIPSRH_Imm(False, 31)));
            *rHi = tmp2;
            *rLo = tmp2;
            return;

         }
         case Iop_ReinterpF64asI64: {
            HReg tLo = newVRegI(env);
            HReg tHi = newVRegI(env);
            MIPSAMode *am_addr;
            HReg fr_src = iselDblExpr(env, e->Iex.Unop.arg);

            sub_from_sp(env, 16);   // Move SP down 16 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            // store as F64
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, fr_src,
                                           am_addr));
            // load as 2xI32                              
            addInstr(env, MIPSInstr_Load(4, tLo, am_addr, mode64));
            addInstr(env, MIPSInstr_Load(4, tHi, nextMIPSAModeFloat(am_addr),
                                         mode64));

            add_to_sp(env, 16);  // Reset SP

            *rHi = tHi;
            *rLo = tLo;
            return;
         }
   
         default:
            vex_printf("UNARY: No such op: ");
            ppIROp(e->Iex.Unop.op);
            vex_printf("\n");
            break;
      }
   }

   vex_printf("iselInt64Expr(mips): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselInt64Expr(mips)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (32 bit)         ---*/
/*---------------------------------------------------------*/

/* Nothing interesting here; really just wrappers for
   64-bit stuff. */

static HReg iselFltExpr(ISelEnv * env, IRExpr * e)
{
   HReg r = iselFltExpr_wrk(env, e);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselFltExpr_wrk(ISelEnv * env, IRExpr * e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(ty == Ity_F32 || (ty == Ity_F64 && mode64));

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   if (e->tag == Iex_Load) {
      MIPSAMode *am_addr;
      HReg r_dst = newVRegF(env);
      vassert(e->Iex.Load.ty == Ity_F32
             || (e->Iex.Load.ty == Ity_F64 && mode64));
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, ty);
      addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 4, r_dst, am_addr));
      return r_dst;
   }

   if (e->tag == Iex_Get) {
      HReg r_dst = newVRegF(env);
      MIPSAMode *am_addr = MIPSAMode_IR(e->Iex.Get.offset,
                                        GuestStatePointer(mode64));
      addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 4, r_dst, am_addr));
      return r_dst;
   }

   if (e->tag == Iex_Unop) {
      switch (e->Iex.Unop.op) {
      case Iop_ReinterpI32asF32: {
         MIPSAMode *am_addr;
         HReg fr_src = iselWordExpr_R(env, e->Iex.Unop.arg);
         HReg r_dst = newVRegF(env);

         sub_from_sp(env, 16);   // Move SP down 16 bytes
         am_addr = MIPSAMode_IR(0, StackPointer(mode64));

         // store as I32                                 
         addInstr(env, MIPSInstr_Store(4, am_addr, fr_src, mode64));

         // load as Ity_F32
         addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 4, r_dst, am_addr));

         add_to_sp(env, 16);  // Reset SP
         return r_dst;

      }
      case Iop_F32toF64: {
         /* first arg is rounding mode; we ignore it. */
         MIPSAMode *am_addr;
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);

         sub_from_sp(env, 16);   // Move SP down 16 bytes
         am_addr = MIPSAMode_IR(0, StackPointer(mode64));

         addInstr(env, MIPSInstr_Store(4,
                                       MIPSAMode_IR(am_addr->Mam.IR.index + 4,
                                       am_addr->Mam.IR.base),
                                       hregMIPS_GPR0(mode64), mode64));
         addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 4, src, am_addr));

         // load as Ity_F32
         addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, dst, am_addr));
         add_to_sp(env, 16);  // Reset SP

         return dst;
      }
      case Iop_ReinterpI64asF64:
         {
            vassert(mode64);
            MIPSAMode *am_addr;
            HReg fr_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            HReg r_dst = newVRegF(env);

            sub_from_sp(env, 16);   // Move SP down 16 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            // store as I32                                 
            addInstr(env, MIPSInstr_Store(8, am_addr, fr_src, mode64));

            // load as Ity_F32
            addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, r_dst, am_addr));

            add_to_sp(env, 16);  // Reset SP
            return r_dst;
         }
      case Iop_AbsF32:
      case Iop_AbsF64: {
         Bool sz32 = e->Iex.Unop.op == Iop_AbsF32;
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpUnary(sz32 ? Mfp_ABSS : Mfp_ABSD, dst, src));
         return dst;
      }
      case Iop_NegF32:
      case Iop_NegF64: {
         Bool sz32 = e->Iex.Unop.op == Iop_NegF32;
         HReg src = iselFltExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegF(env);
         addInstr(env, MIPSInstr_FpUnary(sz32 ? Mfp_NEGS : Mfp_NEGD, dst, src));
         return dst;
      }
      default:
         break;
      }
   }

   if (e->tag == Iex_Triop) {
      switch (e->Iex.Triop.details->op) {
         case Iop_DivF32:
         case Iop_DivF64:
         case Iop_MulF32:
         case Iop_MulF64:
         case Iop_AddF32:
         case Iop_AddF64:
         case Iop_SubF32:
         case Iop_SubF64: {
            MIPSFpOp op = 0;
            /*INVALID*/ HReg argL = iselFltExpr(env, e->Iex.Triop.details->arg2);
            HReg argR = iselFltExpr(env, e->Iex.Triop.details->arg3);
            HReg dst = newVRegF(env);
            switch (e->Iex.Triop.details->op) {
               case Iop_DivF32:
                  op = Mfp_DIVS;
                  break;
               case Iop_MulF32:
                  op = Mfp_MULS;
                  break;
               case Iop_AddF32:
                  op = Mfp_ADDS;
                  break;
               case Iop_SubF32:
                  op = Mfp_SUBS;
                  break;
               default:
                  vassert(0);
            }
            addInstr(env, MIPSInstr_FpBinary(op, dst, argL, argR));
            return dst;
         }
         default:
            break;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_F64toF32: {
            HReg valD = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg valS = newVRegF(env);

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTSD, valS, valD));
            set_MIPS_rounding_default(env);
            return valS;
         }

         case Iop_RoundF32toInt: {
               HReg valS = newVRegF(env);
               HReg valF = iselFltExpr(env, e->Iex.Binop.arg2);

               set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
               addInstr(env, MIPSInstr_FpConvert(Mfp_CVTWS, valS, valF));

               set_MIPS_rounding_default(env);
               return valS;
            }

         case Iop_I32StoF32: {
            HReg r_dst = newVRegF(env);

            MIPSAMode *am_addr;
            HReg fr_src = iselWordExpr_R(env, e->Iex.Binop.arg2);
            HReg tmp = newVRegF(env);

            sub_from_sp(env, 16);   // Move SP down 16 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            // store as I32
            addInstr(env, MIPSInstr_Store(4, am_addr, fr_src, mode64));

            // load as Ity_F32
            addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 4, tmp, am_addr));

            add_to_sp(env, 16);  // Reset SP

            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTSW, r_dst, tmp));
            set_MIPS_rounding_default(env);

            return r_dst;
         }

         case Iop_SqrtF32:
         case Iop_SqrtF64: {
            /* first arg is rounding mode; we ignore it. */
            Bool sz32 = e->Iex.Binop.op == Iop_SqrtF32;
            HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegF(env);
            set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpUnary(sz32 ? Mfp_SQRTS : Mfp_SQRTD, dst,
                                            src));
            set_MIPS_rounding_default(env);
            return dst;
         }
   
         default:
            break;
      }
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
      HReg fsrc = iselDblExpr(env, e->Iex.Unop.arg);
      HReg fdst = newVRegF(env);
      MIPSAMode *zero_r1 = MIPSAMode_IR(0, StackPointer(mode64));

      sub_from_sp(env, 16);
      // store as F32, hence truncating
      addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 4, fsrc, zero_r1));
      // and reload.  Good huh?! (sigh)
      addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 4, fdst, zero_r1));
      add_to_sp(env, 16);
      return fdst;
   }

   vex_printf("iselFltExpr(mips): No such tag(0x%x)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselFltExpr_wrk(mips)");
}

static HReg iselDblExpr(ISelEnv * env, IRExpr * e)
{
   HReg r = iselDblExpr_wrk(env, e);
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));
   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselDblExpr_wrk(ISelEnv * env, IRExpr * e)
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(e);
   vassert(ty == Ity_F64);

   if (e->tag == Iex_RdTmp) {
      return lookupIRTemp(env, e->Iex.RdTmp.tmp);
   }

   /* --------- LOAD --------- */
   if (e->tag == Iex_Load && e->Iex.Load.end == Iend_LE) {
      HReg r_dst = newVRegD(env);
      MIPSAMode *am_addr;
      vassert(e->Iex.Load.ty == Ity_F64);
      am_addr = iselWordExpr_AMode(env, e->Iex.Load.addr, ty);
      addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, r_dst, am_addr));
      return r_dst;
   }

   /* --------- GET --------- */
   if (e->tag == Iex_Get) {

      HReg r_dst = newVRegD(env);
      MIPSAMode *am_addr = MIPSAMode_IR(e->Iex.Get.offset,
                                        GuestStatePointer(mode64));
      addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, r_dst, am_addr));
      return r_dst;
   }

   if (e->tag == Iex_Unop) {
      MIPSFpOp fpop = Mfp_INVALID;
      switch (e->Iex.Unop.op) {
         case Iop_NegF64:
            fpop = Mfp_NEGD;
            break;
         case Iop_AbsF64:
            fpop = Mfp_ABSD;
            break;
         case Iop_F32toF64: {
            HReg src = iselFltExpr(env, e->Iex.Unop.arg);
            HReg dst = newVRegD(env);

            HReg irrm = newVRegI(env);

            MIPSAMode *am_addr1 = MIPSAMode_IR(284, GuestStatePointer(mode64));

            addInstr(env, MIPSInstr_Load(4, irrm, am_addr1, mode64));

            // set new FCSR
            HReg tmp = newVRegI(env);
            HReg fcsr_old = newVRegI(env);
            MIPSAMode *am_addr;

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp, irrm,
                                        MIPSRH_Imm(False, 1)));
            addInstr(env, MIPSInstr_Alu(Malu_XOR, tmp, irrm, MIPSRH_Reg(tmp)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, irrm, tmp,
                                        MIPSRH_Imm(False, 3)));
            /* save old value of FCSR */
            addInstr(env, MIPSInstr_MfFCSR(fcsr_old));
            sub_from_sp(env, 8); // Move SP down 4 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            //store old FCSR to stack
            addInstr(env, MIPSInstr_Store(4, am_addr, fcsr_old, mode64));

            //set new value of FCSR
            addInstr(env, MIPSInstr_MtFCSR(irrm));

            //set_MIPS_rounding_mode(env, e->Iex.Binop.arg1);
            addInstr(env, MIPSInstr_FpUnary(Mfp_CVTD, dst, src));
            set_MIPS_rounding_default(env);
            return dst;
         }
         case Iop_ReinterpI64asF64: {
            HReg Hi;
            HReg Lo;
            HReg dst = newVRegD(env);

            iselInt64Expr(&Hi, &Lo, env, e->Iex.Unop.arg);

            dst = mk_LoadRR32toFPR(env, Hi, Lo);   // 2*I32 -> F64
            return dst;
         }
         case Iop_I32StoF64: {
            HReg dst = newVRegD(env);
            HReg tmp1 = newVRegF(env);
            HReg r_src = iselWordExpr_R(env, e->Iex.Unop.arg);
            MIPSAMode *am_addr;
            sub_from_sp(env, 16);   // Move SP down 16 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            // store as I32
            addInstr(env, MIPSInstr_Store(4, am_addr, r_src, mode64));

            // load as Ity_F32
            addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 4, tmp1, am_addr));

            add_to_sp(env, 16);  // Reset SP

            HReg irrm = newVRegI(env);

            MIPSAMode *am_addr1 = MIPSAMode_IR(284, GuestStatePointer(mode64));

            addInstr(env, MIPSInstr_Load(4, irrm, am_addr1, mode64));

            //set rounding mode
            HReg tmp = newVRegI(env);
            HReg fcsr_old = newVRegI(env);

            addInstr(env, MIPSInstr_Shft(Mshft_SLL, True, tmp, irrm,
                                         MIPSRH_Imm(False, 1)));
            addInstr(env, MIPSInstr_Alu(Malu_XOR, tmp, irrm, MIPSRH_Reg(tmp)));
            addInstr(env, MIPSInstr_Alu(Malu_AND, irrm, tmp,
                                        MIPSRH_Imm(False, 3)));
            /* save old value of FCSR */
            addInstr(env, MIPSInstr_MfFCSR(fcsr_old));
            sub_from_sp(env, 8); // Move SP down 4 bytes
            am_addr = MIPSAMode_IR(0, StackPointer(mode64));

            //store old FCSR to stack
            addInstr(env, MIPSInstr_Store(4, am_addr, fcsr_old, mode64));

            //set new value of FCSR
            addInstr(env, MIPSInstr_MtFCSR(irrm));

            // and do convert
            addInstr(env, MIPSInstr_FpConvert(Mfp_CVTDW, dst, tmp1));
            set_MIPS_rounding_default(env);

            return dst;
         }
         default:
            break;
      }

      if (fpop != Mfp_INVALID) {
         HReg src = iselDblExpr(env, e->Iex.Unop.arg);
         HReg dst = newVRegD(env);
         addInstr(env, MIPSInstr_FpUnary(fpop, dst, src));
         return dst;
      }
   }

   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_RoundF64toInt: {
            HReg valD = iselDblExpr(env, e->Iex.Binop.arg2);
            MIPSRH *fmt = iselWordExpr_RH(env, False, e->Iex.Binop.arg1);
            HReg valD1 = newVRegD(env);

            if (fmt->Mrh.Imm.imm16 == 0x3)
               addInstr(env, MIPSInstr_FpConvert(Mfp_TRULD, valD1, valD));
            else if (fmt->Mrh.Imm.imm16 == 0x2)
               addInstr(env, MIPSInstr_FpConvert(Mfp_CEILLD, valD1, valD));
            else
               vassert(0);
            return valD1;
         }

         case Iop_SqrtF64:{
            /* first arg is rounding mode; we ignore it. */
            HReg src = iselDblExpr(env, e->Iex.Binop.arg2);
            HReg dst = newVRegD(env);
            addInstr(env, MIPSInstr_FpUnary(Mfp_SQRTD, dst, src));
            return dst;
         }

         default:
            break;

      }
   }

   if (e->tag == Iex_Triop) {
      switch (e->Iex.Triop.details->op) {
         case Iop_DivF64:
         case Iop_DivF32:
         case Iop_MulF64:
         case Iop_AddF64:
         case Iop_SubF64: {
            MIPSFpOp op = 0;
            /*INVALID*/ HReg argL = iselDblExpr(env, e->Iex.Triop.details->arg2);
            HReg argR = iselDblExpr(env, e->Iex.Triop.details->arg3);
            HReg dst = newVRegD(env);
            switch (e->Iex.Triop.details->op) {
               case Iop_DivF64:
                  op = Mfp_DIVD;
                  break;
               case Iop_MulF64:
                  op = Mfp_MULD;
                  break;
               case Iop_AddF64:
                  op = Mfp_ADDD;
                  break;
               case Iop_SubF64:
                  op = Mfp_SUBD;
                  break;
               default:
                  vassert(0);
            }
            addInstr(env, MIPSInstr_FpBinary(op, dst, argL, argR));
            return dst;
         }
         default:
            break;
      }
   }

   /* --------- MULTIPLEX --------- */
   if (e->tag == Iex_Mux0X) {
      if (ty == Ity_F64
          && typeOfIRExpr(env->type_env, e->Iex.Mux0X.cond) == Ity_I8) {
         HReg r0 = iselDblExpr(env, e->Iex.Mux0X.expr0);
         HReg rX = iselDblExpr(env, e->Iex.Mux0X.exprX);
         HReg r_cond = iselWordExpr_R(env, e->Iex.Mux0X.cond);
         HReg r_cond_neg = newVRegI(env);
         HReg r_dst = newVRegD(env);
         HReg r_tmp_lo = newVRegI(env);
         HReg r_tmp_hi = newVRegI(env);
         HReg r_tmp1_lo = newVRegI(env);
         HReg r_tmp1_hi = newVRegI(env);
         HReg r_r0_lo = newVRegI(env);
         HReg r_r0_hi = newVRegI(env);
         HReg r_rX_lo = newVRegI(env);
         HReg r_rX_hi = newVRegI(env);
         HReg r_dst_lo = newVRegI(env);
         HReg r_dst_hi = newVRegI(env);

         sub_from_sp(env, 16);   // Move SP down 16 bytes
         MIPSAMode *am_addr = MIPSAMode_IR(0, StackPointer(mode64));

         // store as Ity_F64
         addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, r0, am_addr));

         // load as 2xI32                              
         addInstr(env, MIPSInstr_Load(4, r_r0_lo, am_addr, mode64));
         addInstr(env, MIPSInstr_Load(4, r_r0_hi, nextMIPSAModeFloat(am_addr),
                                      mode64));

         add_to_sp(env, 16);  // Reset SP

         addInstr(env, MIPSInstr_Alu(Malu_AND, r_tmp_lo, r_cond,
                                     MIPSRH_Reg(r_r0_lo)));
         addInstr(env, MIPSInstr_Alu(Malu_AND, r_tmp_hi, r_cond,
                       MIPSRH_Reg(r_r0_hi)));

         addInstr(env, MIPSInstr_Alu(Malu_NOR, r_cond_neg, r_cond,
                       MIPSRH_Reg(r_cond)));

         sub_from_sp(env, 16);   // Move SP down 16 bytes
         am_addr = MIPSAMode_IR(0, StackPointer(mode64));

         // store as Ity_F64
         addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, rX, am_addr));

         // load as 2xI32                              
         addInstr(env, MIPSInstr_Load(4, r_rX_lo, am_addr, mode64));
         addInstr(env, MIPSInstr_Load(4, r_rX_hi, nextMIPSAModeFloat(am_addr),
                                      mode64));

         add_to_sp(env, 16);  // Reset SP

         addInstr(env, MIPSInstr_Alu(Malu_AND, r_tmp1_lo, r_cond_neg,
                                     MIPSRH_Reg(r_rX_lo)));
         addInstr(env, MIPSInstr_Alu(Malu_AND, r_tmp1_hi, r_cond_neg,
                                     MIPSRH_Reg(r_rX_hi)));

         addInstr(env, MIPSInstr_Alu(Malu_ADD, r_dst_lo, r_tmp_lo,
                                     MIPSRH_Reg(r_tmp1_lo)));
         addInstr(env, MIPSInstr_Alu(Malu_ADD, r_dst_hi, r_tmp_hi,
                                     MIPSRH_Reg(r_tmp1_hi)));

         sub_from_sp(env, 16);   // Move SP down 16 bytes
         am_addr = MIPSAMode_IR(0, StackPointer(mode64));

         // store as I32
         addInstr(env, MIPSInstr_Store(4, am_addr, r_dst_lo, mode64));
         addInstr(env, MIPSInstr_Store(4, nextMIPSAModeFloat(am_addr),
                  r_dst_hi, mode64));

         // load as Ity_F32
         addInstr(env, MIPSInstr_FpLdSt(True /*load */ , 8, r_dst, am_addr));

         add_to_sp(env, 16);  // Reset SP      

         return r_dst;
      }
   }

   vex_printf("iselDblExpr(mips): No such tag(%u)\n", e->tag);
   ppIRExpr(e);
   vpanic("iselDblExpr_wrk(mips)");
}

/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void iselStmt(ISelEnv * env, IRStmt * stmt)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- ");

      ppIRStmt(stmt);
      vex_printf("\n");
   }

   switch (stmt->tag) {
      /* --------- STORE --------- */
      case Ist_Store: {
         MIPSAMode *am_addr;
         IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);

         /*constructs addressing mode from address provided */
         am_addr = iselWordExpr_AMode(env, stmt->Ist.Store.addr, tyd);

         if (tyd == Ity_I8 || tyd == Ity_I16 || tyd == Ity_I32 ||
             (mode64 && (tyd == Ity_I64))) {
            HReg r_src = iselWordExpr_R(env, stmt->Ist.Store.data);
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(tyd)),
                     am_addr, r_src, mode64));
            return;
         }
         if (!mode64 && (tyd == Ity_I64)) {
            HReg vHi, vLo;
            HReg r_addr = iselWordExpr_R(env, stmt->Ist.Store.addr);

            iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Store.data);

            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(Ity_I32)),
                          MIPSAMode_IR(0, r_addr), vHi, mode64));
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(Ity_I32)),
                          MIPSAMode_IR(4, r_addr), vLo, mode64));
            return;
         }
         if (tyd == Ity_F32) {
            HReg fr_src = iselFltExpr(env, stmt->Ist.Store.data);
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 4, fr_src,
                                           am_addr));
            return;
         }

         break;
      }

      /* --------- PUT --------- */
      case Ist_Put: {
         IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);
   
         if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 ||
             (ty == Ity_I64 && mode64)) {
            HReg r_src = iselWordExpr_R(env, stmt->Ist.Put.data);
            MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                              GuestStatePointer(mode64));
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(ty)),
                                          am_addr, r_src, mode64));
            return;
         }
   
         if (ty == Ity_I64 && !mode64) {
            HReg vHi, vLo;
            MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                              GuestStatePointer(mode64));
            MIPSAMode *am_addr4 = MIPSAMode_IR(stmt->Ist.Put.offset + 4,
                                               GuestStatePointer(mode64));
            iselInt64Expr(&vHi, &vLo, env, stmt->Ist.Put.data);
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(Ity_I32)),
                                          am_addr, vLo, mode64));
            addInstr(env, MIPSInstr_Store(toUChar(sizeofIRType(Ity_I32)),
                                          am_addr4, vHi, mode64));
            return;
   
         }
   
         if (ty == Ity_F32) {
            HReg fr_src = iselFltExpr(env, stmt->Ist.Put.data);
            MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                              GuestStatePointer(mode64));
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 4, fr_src,
                                           am_addr));
            return;
         }
   
         if (ty == Ity_F64) {
            HReg fr_src;
            fr_src = iselDblExpr(env, stmt->Ist.Put.data);
            MIPSAMode *am_addr = MIPSAMode_IR(stmt->Ist.Put.offset,
                                              GuestStatePointer(mode64));
            addInstr(env, MIPSInstr_FpLdSt(False /*store */ , 8, fr_src,
                                           am_addr));
            return;
         }
         break;
      }

      /* --------- TMP --------- */
      case Ist_WrTmp: {
         IRTemp tmp = stmt->Ist.WrTmp.tmp;
         IRType ty = typeOfIRTemp(env->type_env, tmp);

         if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I1) {
            HReg r_dst = lookupIRTemp(env, tmp);
            HReg r_src = iselWordExpr_R(env, stmt->Ist.WrTmp.data);
            addInstr(env, mk_iMOVds_RR(r_dst, r_src));
            return;
         }

         if (ty == Ity_I64) {
             HReg rHi, rLo, dstHi, dstLo;
             iselInt64Expr(&rHi, &rLo, env, stmt->Ist.WrTmp.data);
             lookupIRTemp64(&dstHi, &dstLo, env, tmp);
             addInstr(env, mk_iMOVds_RR(dstHi, rHi));
             addInstr(env, mk_iMOVds_RR(dstLo, rLo));
             return;
         }

         if (ty == Ity_F32) {
            HReg fr_dst = lookupIRTemp(env, tmp);
            HReg fr_src = iselFltExpr(env, stmt->Ist.WrTmp.data);
            addInstr(env, MIPSInstr_FpUnary(Mfp_MOVS, fr_dst, fr_src));
            return;
         }

         if (ty == Ity_F64) {
             HReg src = iselDblExpr(env, stmt->Ist.WrTmp.data);
             HReg dst = lookupIRTemp(env, tmp);
             addInstr(env, MIPSInstr_FpUnary(Mfp_MOVD, dst, src));
             return;
         }
         break;
      }

      /* --------- Call to DIRTY helper --------- */
      case Ist_Dirty: {
         IRType retty;
         IRDirty *d = stmt->Ist.Dirty.details;
         Bool passBBP = False;

         if (d->nFxState == 0)
            vassert(!d->needsBBP);
         passBBP = toBool(d->nFxState > 0 && d->needsBBP);

         /* Marshal args, do the call, clear stack. */
         doHelperCall(env, passBBP, d->guard, d->cee, d->args);

         /* Now figure out what to do with the returned value, if any. */
         if (d->tmp == IRTemp_INVALID)
            /* No return value.  Nothing to do. */
            return;

         retty = typeOfIRTemp(env->type_env, d->tmp);
         if (retty == Ity_I64 && !mode64) {
            vex_printf
                ("Dirty! Return 64 bits. Not implemented (yet!)\n");
            return;
         }
         if (retty == Ity_I8 || retty == Ity_I16 || retty == Ity_I32
             || (retty == Ity_I64 && mode64)) {
            /* The returned value is in %r2.  Park it in the register
               associated with tmp. */
            HReg r_dst = lookupIRTemp(env, d->tmp);
            addInstr(env, mk_iMOVds_RR(r_dst, hregMIPS_GPR2(mode64)));
            return;
         }
         break;
      }

      /* --------- Load Linked or Store Conditional --------- */
      case Ist_LLSC: {
         //Temporary solution; this need to be rewritten again for MIPS.
         //On MIPS you can not read from address that is locked with LL before SC.
         // If you read from address that is locked than SC will fall.
         IRTemp res = stmt->Ist.LLSC.result;
         IRType tyRes = typeOfIRTemp(env->type_env, res);
         IRType tyAddr = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.addr);

         if (!mode64 && (tyAddr != Ity_I32))
            goto stmt_fail;

         if (stmt->Ist.LLSC.storedata == NULL) {
            /* LL */
            MIPSAMode *r_addr;
            /*constructs addressing mode from address provided */
            r_addr = iselWordExpr_AMode(env, stmt->Ist.LLSC.addr, tyAddr);

            HReg r_dst = lookupIRTemp(env, res);
            if (tyRes == Ity_I32) {
               addInstr(env, MIPSInstr_Load(4, r_dst, r_addr, mode64));
               return;
            } else if (tyRes == Ity_I64 && mode64) {
               addInstr(env, MIPSInstr_Load(8, r_dst, r_addr, mode64));
               return;
            }
            /* fallthru */ ;
         } else {
            /* SC */
            MIPSAMode *r_addr;
            r_addr = iselWordExpr_AMode(env, stmt->Ist.LLSC.addr, tyAddr);
            HReg r_src = iselWordExpr_R(env, stmt->Ist.LLSC.storedata);
            HReg r_dst = lookupIRTemp(env, res);
            IRType tyData = typeOfIRExpr(env->type_env, 
                                         stmt->Ist.LLSC.storedata);

            if (tyData == Ity_I32) {
               addInstr(env, MIPSInstr_Store(4, r_addr, r_src, mode64));
               addInstr(env, MIPSInstr_LI(r_dst, 0x1));
               return;
            } else if (tyData == Ity_I64 && mode64) {
               addInstr(env, MIPSInstr_Store(8, r_addr, r_src, mode64));
               addInstr(env, MIPSInstr_LI(r_dst, 0x1));
               return;
            }
            /* fallthru */
         }
         goto stmt_fail;
       /*NOTREACHED*/}

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
         vpanic("iselStmt(mips32): Ist_Exit: dst is not a 32-bit value");
      if (mode64 && dst->tag != Ico_U64)
         vpanic("iselStmt(mips64): Ist_Exit: dst is not a 64-bit value");

      MIPSCondCode cc   = iselCondCode(env, stmt->Ist.Exit.guard);
      MIPSAMode*   amPC = MIPSAMode_IR(stmt->Ist.Exit.offsIP,
                                      hregMIPS_GPR10(mode64));

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
            addInstr(env, MIPSInstr_XDirect(
                             mode64 ? (Addr64)stmt->Ist.Exit.dst->Ico.U64
                                    : (Addr64)stmt->Ist.Exit.dst->Ico.U32,
                             amPC, cc, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselWordExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, MIPSInstr_XAssisted(r, amPC, cc, Ijk_Boring));
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
         case Ijk_TInval:
         {
            HReg r = iselWordExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
            addInstr(env, MIPSInstr_XAssisted(r, amPC, cc,
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
      vassert(cdst->tag == (env->mode64 ? Ico_U64 :Ico_U32));
      if (jk == Ijk_Boring || jk == Ijk_Call) {
         /* Boring transfer to known address */
         MIPSAMode* amPC = MIPSAMode_IR(offsIP, hregMIPS_GPR10(env->mode64));
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards
               edge. */
            Bool toFastEP
               = env->mode64
               ? (((Addr64)cdst->Ico.U64) > (Addr64)env->max_ga)
               : (((Addr32)cdst->Ico.U32) > (Addr32)env->max_ga);
            if (0) vex_printf("%s", toFastEP ? "X" : ".");
            addInstr(env, MIPSInstr_XDirect(
                             env->mode64 ? (Addr64)cdst->Ico.U64
                                         : (Addr64)cdst->Ico.U32,
                             amPC, MIPScc_AL, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg r = iselWordExpr_R(env, next);
            addInstr(env, MIPSInstr_XAssisted(r, amPC, MIPScc_AL,
                                              Ijk_Boring));
         }
         return;
      }
   }

   /* Case: call/return (==boring) transfer to any address */
   switch (jk) {
      case Ijk_Boring: case Ijk_Ret: case Ijk_Call: {

         HReg       r     = iselWordExpr_R(env, next);
         MIPSAMode*  amPC = MIPSAMode_IR(offsIP, hregMIPS_GPR10(env->mode64));
         if (env->chainingAllowed) {
            addInstr(env, MIPSInstr_XIndir(r, amPC, MIPScc_AL));
         } else {
            addInstr(env, MIPSInstr_XAssisted(r, amPC, MIPScc_AL,
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
      case Ijk_TInval: {
         HReg      r     = iselWordExpr_R(env, next);
         MIPSAMode* amPC = MIPSAMode_IR(offsIP, hregMIPS_GPR10(env->mode64));
         addInstr(env, MIPSInstr_XAssisted(r, amPC, MIPScc_AL, jk));
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

/* Translate an entire BB to mips code. */
HInstrArray *iselSB_MIPS ( IRSB* bb,
                           VexArch arch_host,
                           VexArchInfo* archinfo_host,
                           VexAbiInfo* vbi,
                           Int offs_Host_EvC_Counter,
                           Int offs_Host_EvC_FailAddr,
                           Bool chainingAllowed,
                           Bool addProfInc,
                           Addr64 max_ga )
{
   Int      i, j;
   HReg     hreg, hregHI;
   ISelEnv* env;
   UInt     hwcaps_host = archinfo_host->hwcaps;
   MIPSAMode *amCounter, *amFailAddr;

   /* sanity ... */
   vassert(arch_host == VexArchMIPS32);
   vassert(VEX_PRID_COMP_MIPS == hwcaps_host
           || VEX_PRID_COMP_BROADCOM == hwcaps_host);

   mode64 = arch_host != VexArchMIPS32;

   /* Make up an initial environment to use. */
   env = LibVEX_Alloc(sizeof(ISelEnv));
   env->vreg_ctr = 0;
   env->mode64 = mode64;

   /* Set up output code array. */
   env->code = newHInstrArray();

   /* Copy BB's type env. */
   env->type_env = bb->tyenv;

   /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
      change as we go along. */
   env->n_vregmap = bb->tyenv->types_used;
   env->vregmap = LibVEX_Alloc(env->n_vregmap * sizeof(HReg));
   env->vregmapHI = LibVEX_Alloc(env->n_vregmap * sizeof(HReg));

   /* and finally ... */
   env->hwcaps          = hwcaps_host;
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
         case Ity_I32: {
            hreg = mkHReg(j++, HRcInt32, True);
            break;
         }
         case Ity_I64: {
            hreg = mkHReg(j++, HRcInt32, True);
            hregHI = mkHReg(j++, HRcInt32, True);
            break;
         }
         case Ity_I128:
            vassert(mode64);
            hreg = mkHReg(j++, HRcInt64, True);
            hregHI = mkHReg(j++, HRcInt64, True);
            break;
         case Ity_F32: {
            hreg = mkHReg(j++, HRcFlt32, True);
            break;
         }
         case Ity_F64:
            hreg = mkHReg(j++, HRcFlt64, True);
            break;
         default:
            ppIRType(bb->tyenv->types[i]);
            vpanic("iselBB(mips): IRTemp type");
      }
      env->vregmap[i] = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   amCounter = MIPSAMode_IR(offs_Host_EvC_Counter, hregMIPS_GPR10(mode64));
   amFailAddr = MIPSAMode_IR(offs_Host_EvC_FailAddr, hregMIPS_GPR10(mode64));
   addInstr(env, MIPSInstr_EvCheck(amCounter, amFailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, MIPSInstr_ProfInc());
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
/*--- end                                    host_mips_isel.c ---*/
/*---------------------------------------------------------------*/
