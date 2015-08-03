
/*---------------------------------------------------------------*/
/*--- begin                                  host_mips_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2013 RT-RK
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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_mips_defs.h"

/* guest_COND offset. */
#define COND_OFFSET(__mode64) (__mode64 ? 612 : 448)

/* Register number for guest state pointer in host code. */
#define GuestSP 23


/*---------------- Registers ----------------*/

const RRegUniverse* getRRegUniverse_MIPS ( Bool mode64 )
{
   /* The real-register universe is a big constant, so we just want to
      initialise it once.  rRegUniverse_MIPS_initted values: 0=not initted,
      1=initted for 32-bit-mode, 2=initted for 64-bit-mode */
   static RRegUniverse rRegUniverse_MIPS;
   static UInt         rRegUniverse_MIPS_initted = 0;

   /* Handy shorthand, nothing more */
   RRegUniverse* ru = &rRegUniverse_MIPS;

   /* This isn't thread-safe.  Sigh. */
   UInt howNeeded = mode64 ? 2 : 1;
   if (LIKELY(rRegUniverse_MIPS_initted == howNeeded))
      return ru;

   RRegUniverse__init(ru);

   /* Add the registers.  The initial segment of this array must be
      those available for allocation by reg-alloc, and those that
      follow are not available for allocation. */
   ru->regs[ru->size++] = hregMIPS_GPR16(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR17(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR18(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR19(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR20(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR21(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR22(mode64);

   ru->regs[ru->size++] = hregMIPS_GPR12(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR13(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR14(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR15(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR24(mode64);
   /* s7  (=guest_state) */
   ru->regs[ru->size++] = hregMIPS_F16(mode64);
   ru->regs[ru->size++] = hregMIPS_F18(mode64);
   ru->regs[ru->size++] = hregMIPS_F20(mode64);
   ru->regs[ru->size++] = hregMIPS_F22(mode64);
   ru->regs[ru->size++] = hregMIPS_F24(mode64);
   ru->regs[ru->size++] = hregMIPS_F26(mode64);
   ru->regs[ru->size++] = hregMIPS_F28(mode64);
   ru->regs[ru->size++] = hregMIPS_F30(mode64);
   if (!mode64) {
      /* Fake double floating point */
      ru->regs[ru->size++] = hregMIPS_D0(mode64);
      ru->regs[ru->size++] = hregMIPS_D1(mode64);
      ru->regs[ru->size++] = hregMIPS_D2(mode64);
      ru->regs[ru->size++] = hregMIPS_D3(mode64);
      ru->regs[ru->size++] = hregMIPS_D4(mode64);
      ru->regs[ru->size++] = hregMIPS_D5(mode64);
      ru->regs[ru->size++] = hregMIPS_D6(mode64);
      ru->regs[ru->size++] = hregMIPS_D7(mode64);
   }

   ru->allocable = ru->size;
   /* And other regs, not available to the allocator. */

   ru->regs[ru->size++] = hregMIPS_HI(mode64);
   ru->regs[ru->size++] = hregMIPS_LO(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR0(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR1(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR2(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR3(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR4(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR5(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR6(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR7(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR8(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR9(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR10(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR11(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR23(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR25(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR29(mode64);
   ru->regs[ru->size++] = hregMIPS_GPR31(mode64);

   rRegUniverse_MIPS_initted = howNeeded;

   RRegUniverse__check_is_sane(ru);
   return ru;
}


void ppHRegMIPS(HReg reg, Bool mode64)
{
   Int r;
   static const HChar *ireg32_names[35]
       = { "$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7",
      "$8", "$9", "$10", "$11", "$12", "$13", "$14", "$15",
      "$16", "$17", "$18", "$19", "$20", "$21", "$22", "$23",
      "$24", "$25", "$26", "$27", "$28", "$29", "$30", "$31",
      "%32", "%33", "%34",
   };

   static const HChar *freg32_names[32]
       = { "$f0", "$f1", "$f2", "$f3", "$f4", "$f5", "$f6", "$f7",
      "$f8", "$f9", "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
      "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
      "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "f30", "$f31"
   };

   static const HChar *freg64_names[32]
       = { "$d0", "$d1", "$d2", "$d3", "$d4", "$d5", "$d6", "$d7",
      "$d8", "$d9", "$d10", "$d11", "$d12", "$d13", "$d14", "$d15",
   };

   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }

   /* But specific for real regs. */
   vassert(hregClass(reg) == HRcInt32 || hregClass(reg) == HRcInt64 ||
           hregClass(reg) == HRcFlt32 || hregClass(reg) == HRcFlt64);

   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt32:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 32);
         vex_printf("%s", ireg32_names[r]);
         return;
      case HRcInt64:
         r = hregEncoding (reg);
         vassert (r >= 0 && r < 32);
         vex_printf ("%s", ireg32_names[r]);
         return;
      case HRcFlt32:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 32);
         vex_printf("%s", freg32_names[r]);
         return;
      case HRcFlt64:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 32);
         vex_printf("%s", freg64_names[r]);
         return;
      default:
         vpanic("ppHRegMIPS");
         break;
   }

   return;
}


/*----------------- Condition Codes ----------------------*/

const HChar *showMIPSCondCode(MIPSCondCode cond)
{
   const HChar* ret;
   switch (cond) {
      case MIPScc_EQ:
         ret = "EQ";  /* equal */
         break;
      case MIPScc_NE:
         ret = "NEQ";  /* not equal */
         break;
      case MIPScc_HS:
         ret = "GE";  /* >=u (Greater Than or Equal) */
         break;
      case MIPScc_LO:
         ret = "LT";  /* <u  (lower) */
         break;
      case MIPScc_MI:
         ret = "MI";  /* minus (negative) */
         break;
      case MIPScc_PL:
         ret = "PL";  /* plus (zero or +ve) */
         break;
      case MIPScc_VS:
         ret = "VS";  /* overflow */
         break;
      case MIPScc_VC:
         ret = "VC";  /* no overflow */
         break;
      case MIPScc_HI:
         ret = "HI";  /* >u   (higher) */
         break;
      case MIPScc_LS:
         ret = "LS";  /* <=u  (lower or same) */
         break;
      case MIPScc_GE:
         ret = "GE";  /* >=s (signed greater or equal) */
         break;
      case MIPScc_LT:
         ret = "LT";  /* <s  (signed less than) */
         break;
      case MIPScc_GT:
         ret = "GT";  /* >s  (signed greater) */
         break;
      case MIPScc_LE:
         ret = "LE";  /* <=s (signed less or equal) */
         break;
      case MIPScc_AL:
         ret = "AL";  /* always (unconditional) */
         break;
      case MIPScc_NV:
         ret = "NV";  /* never (unconditional): */
         break;
      default:
         vpanic("showMIPSCondCode");
         break;
   }
   return ret;
}

const HChar *showMIPSFpOp(MIPSFpOp op)
{
   const HChar *ret;
   switch (op) {
      case Mfp_ADDD:
         ret = "add.d";
         break;
      case Mfp_SUBD:
         ret = "sub.d";
         break;
      case Mfp_MULD:
         ret = "mul.d";
         break;
      case Mfp_DIVD:
         ret = "div.d";
         break;
      case Mfp_MADDD:
         ret = "madd.d";
         break;
      case Mfp_MSUBD:
         ret = "msub.d";
         break;
      case Mfp_MADDS:
         ret = "madd.s";
         break;
      case Mfp_MSUBS:
         ret = "msub.s";
         break;
      case Mfp_ADDS:
         ret = "add.s";
         break;
      case Mfp_SUBS:
         ret = "sub.s";
         break;
      case Mfp_MULS:
         ret = "mul.s";
         break;
      case Mfp_DIVS:
         ret = "div.s";
         break;
      case Mfp_SQRTS:
         ret = "sqrt.s";
         break;
      case Mfp_SQRTD:
         ret = "sqrt.d";
         break;
      case Mfp_ABSS:
         ret = "abs.s";
         break;
      case Mfp_ABSD:
         ret = "abs.d";
         break;
      case Mfp_NEGS:
         ret = "neg.s";
         break;
      case Mfp_NEGD:
         ret = "neg.d";
         break;
      case Mfp_MOVS:
         ret = "mov.s";
         break;
      case Mfp_MOVD:
         ret = "mov.d";
         break;
      case Mfp_ROUNDWS:
         ret = "round.w.s";
         break;
      case Mfp_ROUNDWD:
         ret = "round.w.d";
         break;
      case Mfp_ROUNDLD:
         ret = "round.l.d";
         break;
      case Mfp_FLOORWS:
         ret = "floor.w.s";
         break;
      case Mfp_FLOORWD:
         ret = "floor.w.d";
         break;
      case Mfp_CVTDW:
         ret = "cvt.d.w";
         break;
      case Mfp_CVTDL:
         ret = "cvt.d.l";
         break;
      case Mfp_CVTDS:
         ret = "cvt.d.s";
         break;
      case Mfp_CVTSD:
         ret = "cvt.s.d";
         break;
      case Mfp_CVTSW:
         ret = "cvt.s.w";
         break;
      case Mfp_CVTWS:
         ret = "cvt.w.s";
         break;
      case Mfp_CVTWD:
         ret = "cvt.w.d";
         break;
      case Mfp_CVTLD:
         ret = "cvt.l.d";
         break;
      case Mfp_CVTLS:
         ret = "cvt.l.s";
         break;
      case Mfp_TRUWD:
         ret = "trunc.w.d";
         break;
      case Mfp_TRUWS:
         ret = "trunc.w.s";
         break;
      case Mfp_TRULD:
         ret = "trunc.l.d";
         break;
      case Mfp_TRULS:
         ret = "trunc.l.s";
         break;
      case Mfp_CEILWS:
         ret = "ceil.w.s";
         break;
      case Mfp_CEILWD:
         ret = "ceil.w.d";
         break;
      case Mfp_CEILLS:
         ret = "ceil.l.s";
         break;
      case Mfp_CEILLD:
         ret = "ceil.l.d";
         break;
      case Mfp_CMP_UN:
         ret = "c.un.d";
         break;
      case Mfp_CMP_EQ:
         ret = "c.eq.d";
         break;
      case Mfp_CMP_LT:
         ret = "c.lt.d";
         break;
      case Mfp_CMP_NGT:
         ret = "c.ngt.d";
         break;
      default:
         vex_printf("Unknown op: %d", (Int)op);
         vpanic("showMIPSFpOp");
         break;
   }
   return ret;
}

/* Show move from/to fpr to/from gpr */
const HChar* showMIPSFpGpMoveOp ( MIPSFpGpMoveOp op )
{
   const HChar *ret;
   switch (op) {
      case MFpGpMove_mfc1:
         ret = "mfc1";
         break;
      case MFpGpMove_dmfc1:
         ret = "dmfc1";
         break;
      case MFpGpMove_mtc1:
         ret = "mtc1";
         break;
      case MFpGpMove_dmtc1:
         ret = "dmtc1";
         break;
      default:
         vpanic("showMIPSFpGpMoveOp");
         break;
   }
   return ret;
}

/* Show floating point move conditional */
const HChar* showMIPSMoveCondOp ( MIPSMoveCondOp op )
{
   const HChar *ret;
   switch (op) {
      case MFpMoveCond_movns:
         ret = "movn.s";
         break;
      case MFpMoveCond_movnd:
         ret = "movn.d";
         break;
      case MMoveCond_movn:
         ret = "movn";
         break;
      default:
         vpanic("showMIPSFpMoveCondOp");
         break;
   }
   return ret;
}

/* --------- MIPSAMode: memory address expressions. --------- */

MIPSAMode *MIPSAMode_IR(Int idx, HReg base)
{
   MIPSAMode *am = LibVEX_Alloc_inline(sizeof(MIPSAMode));
   am->tag = Mam_IR;
   am->Mam.IR.base = base;
   am->Mam.IR.index = idx;

   return am;
}

MIPSAMode *MIPSAMode_RR(HReg idx, HReg base)
{
   MIPSAMode *am = LibVEX_Alloc_inline(sizeof(MIPSAMode));
   am->tag = Mam_RR;
   am->Mam.RR.base = base;
   am->Mam.RR.index = idx;

   return am;
}

MIPSAMode *dopyMIPSAMode(MIPSAMode * am)
{
   MIPSAMode* ret;
   switch (am->tag) {
      case Mam_IR:
         ret = MIPSAMode_IR(am->Mam.IR.index, am->Mam.IR.base);
         break;
      case Mam_RR:
         ret = MIPSAMode_RR(am->Mam.RR.index, am->Mam.RR.base);
         break;
      default:
         vpanic("dopyMIPSAMode");
         break;
   }
   return ret;
}

MIPSAMode *nextMIPSAModeFloat(MIPSAMode * am)
{
   MIPSAMode* ret;
   switch (am->tag) {
      case Mam_IR:
         ret = MIPSAMode_IR(am->Mam.IR.index + 4, am->Mam.IR.base);
         break;
      case Mam_RR:
         /* We can't do anything with the RR case, so if it appears
            we simply have to give up. */
         /* fallthrough */
      default:
         vpanic("nextMIPSAModeFloat");
         break;
   }
   return ret;
}

MIPSAMode *nextMIPSAModeInt(MIPSAMode * am)
{
   MIPSAMode* ret;
   switch (am->tag) {
      case Mam_IR:
         ret = MIPSAMode_IR(am->Mam.IR.index + 4, am->Mam.IR.base);
         break;
      case Mam_RR:
         /* We can't do anything with the RR case, so if it appears
            we simply have to give up. */
         /* fallthrough */
      default:
         vpanic("nextMIPSAModeInt");
         break;
   }
   return ret;
}

void ppMIPSAMode(MIPSAMode * am, Bool mode64)
{
   switch (am->tag) {
      case Mam_IR:
         if (am->Mam.IR.index == 0)
            vex_printf("0(");
         else
            vex_printf("%d(", (Int) am->Mam.IR.index);
         ppHRegMIPS(am->Mam.IR.base, mode64);
         vex_printf(")");
         return;
      case Mam_RR:
         ppHRegMIPS(am->Mam.RR.base, mode64);
         vex_printf(", ");
         ppHRegMIPS(am->Mam.RR.index, mode64);
         return;
      default:
         vpanic("ppMIPSAMode");
         break;
   }
}

static void addRegUsage_MIPSAMode(HRegUsage * u, MIPSAMode * am)
{
   switch (am->tag) {
      case Mam_IR:
         addHRegUse(u, HRmRead, am->Mam.IR.base);
         return;
      case Mam_RR:
         addHRegUse(u, HRmRead, am->Mam.RR.base);
         addHRegUse(u, HRmRead, am->Mam.RR.index);
         return;
      default:
         vpanic("addRegUsage_MIPSAMode");
         break;
   }
}

static void mapRegs_MIPSAMode(HRegRemap * m, MIPSAMode * am)
{
   switch (am->tag) {
      case Mam_IR:
         am->Mam.IR.base = lookupHRegRemap(m, am->Mam.IR.base);
         return;
      case Mam_RR:
         am->Mam.RR.base = lookupHRegRemap(m, am->Mam.RR.base);
         am->Mam.RR.index = lookupHRegRemap(m, am->Mam.RR.index);
         return;
      default:
         vpanic("mapRegs_MIPSAMode");
         break;
   }
}

/* --------- Operand, which can be a reg or a u16/s16. --------- */

MIPSRH *MIPSRH_Imm(Bool syned, UShort imm16)
{
   MIPSRH *op = LibVEX_Alloc_inline(sizeof(MIPSRH));
   op->tag = Mrh_Imm;
   op->Mrh.Imm.syned = syned;
   op->Mrh.Imm.imm16 = imm16;
   /* If this is a signed value, ensure it's not -32768, so that we
      are guaranteed always to be able to negate if needed. */
   if (syned)
      vassert(imm16 != 0x8000);
   vassert(syned == True || syned == False);
   return op;
}

MIPSRH *MIPSRH_Reg(HReg reg)
{
   MIPSRH *op = LibVEX_Alloc_inline(sizeof(MIPSRH));
   op->tag = Mrh_Reg;
   op->Mrh.Reg.reg = reg;
   return op;
}

void ppMIPSRH(MIPSRH * op, Bool mode64)
{
   MIPSRHTag tag = op->tag;
   switch (tag) {
      case Mrh_Imm:
         if (op->Mrh.Imm.syned)
            vex_printf("%d", (Int) (Short) op->Mrh.Imm.imm16);
         else
            vex_printf("%u", (UInt) (UShort) op->Mrh.Imm.imm16);
         return;
      case Mrh_Reg:
         ppHRegMIPS(op->Mrh.Reg.reg, mode64);
         return;
      default:
         vpanic("ppMIPSRH");
         break;
   }
}

/* An MIPSRH can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_MIPSRH(HRegUsage * u, MIPSRH * op)
{
   switch (op->tag) {
      case Mrh_Imm:
         return;
      case Mrh_Reg:
         addHRegUse(u, HRmRead, op->Mrh.Reg.reg);
         return;
      default:
         vpanic("addRegUsage_MIPSRH");
         break;
   }
}

static void mapRegs_MIPSRH(HRegRemap * m, MIPSRH * op)
{
   switch (op->tag) {
      case Mrh_Imm:
         return;
      case Mrh_Reg:
         op->Mrh.Reg.reg = lookupHRegRemap(m, op->Mrh.Reg.reg);
         return;
      default:
         vpanic("mapRegs_MIPSRH");
         break;
   }
}

/* --------- Instructions. --------- */

const HChar *showMIPSUnaryOp(MIPSUnaryOp op)
{
   const HChar* ret;
   switch (op) {
      case Mun_CLO:
         ret = "clo";
         break;
      case Mun_CLZ:
         ret = "clz";
         break;
      case Mun_NOP:
         ret = "nop";
         break;
      case Mun_DCLO:
         ret = "dclo";
         break;
      case Mun_DCLZ:
         ret = "dclz";
         break;
      default:
         vpanic("showMIPSUnaryOp");
         break;
   }
   return ret;
}

const HChar *showMIPSAluOp(MIPSAluOp op, Bool immR)
{
   const HChar* ret;
   switch (op) {
      case Malu_ADD:
         ret = immR ? "addiu" : "addu";
         break;
      case Malu_SUB:
         ret = "subu";
         break;
      case Malu_AND:
         ret = immR ? "andi" : "and";
         break;
      case Malu_OR:
         ret = immR ? "ori" : "or";
         break;
      case Malu_NOR:
         vassert(immR == False); /*there's no nor with an immediate operand!? */
         ret = "nor";
         break;
      case Malu_XOR:
         ret = immR ? "xori" : "xor";
         break;
      case Malu_DADD:
         ret = immR ? "daddi" : "dadd";
         break;
      case Malu_DSUB:
         ret = immR ? "dsubi" : "dsub";
         break;
      case Malu_SLT:
         ret = immR ? "slti" : "slt";
         break;
      default:
         vpanic("showMIPSAluOp");
         break;
   }
   return ret;
}

const HChar *showMIPSShftOp(MIPSShftOp op, Bool immR, Bool sz32)
{
   const HChar *ret;
   switch (op) {
      case Mshft_SRA:
         ret = immR ? (sz32 ? "sra" : "dsra") : (sz32 ? "srav" : "dsrav");
         break;
      case Mshft_SLL:
         ret = immR ? (sz32 ? "sll" : "dsll") : (sz32 ? "sllv" : "dsllv");
         break;
      case Mshft_SRL:
         ret = immR ? (sz32 ? "srl" : "dsrl") : (sz32 ? "srlv" : "dsrlv");
         break;
      default:
         vpanic("showMIPSShftOp");
         break;
   }
   return ret;
}

const HChar *showMIPSMaccOp(MIPSMaccOp op, Bool variable)
{
   const HChar *ret;
   switch (op) {
      case Macc_ADD:
         ret = variable ? "madd" : "maddu";
         break;
      case Macc_SUB:
         ret = variable ? "msub" : "msubu";
         break;
      default:
         vpanic("showMIPSAccOp");
         break;
   }
   return ret;
}

MIPSInstr *MIPSInstr_LI(HReg dst, ULong imm)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_LI;
   i->Min.LI.dst = dst;
   i->Min.LI.imm = imm;
   return i;
}

MIPSInstr *MIPSInstr_Alu(MIPSAluOp op, HReg dst, HReg srcL, MIPSRH * srcR)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Alu;
   i->Min.Alu.op = op;
   i->Min.Alu.dst = dst;
   i->Min.Alu.srcL = srcL;
   i->Min.Alu.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_Shft(MIPSShftOp op, Bool sz32, HReg dst, HReg srcL,
                          MIPSRH * srcR)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Shft;
   i->Min.Shft.op = op;
   i->Min.Shft.sz32 = sz32;
   i->Min.Shft.dst = dst;
   i->Min.Shft.srcL = srcL;
   i->Min.Shft.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_Unary(MIPSUnaryOp op, HReg dst, HReg src)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Unary;
   i->Min.Unary.op = op;
   i->Min.Unary.dst = dst;
   i->Min.Unary.src = src;
   return i;
}

MIPSInstr *MIPSInstr_Cmp(Bool syned, Bool sz32, HReg dst, HReg srcL, HReg srcR,
                         MIPSCondCode cond)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Cmp;
   i->Min.Cmp.syned = syned;
   i->Min.Cmp.sz32 = sz32;
   i->Min.Cmp.dst = dst;
   i->Min.Cmp.srcL = srcL;
   i->Min.Cmp.srcR = srcR;
   i->Min.Cmp.cond = cond;
   return i;
}

/* multiply */
MIPSInstr *MIPSInstr_Mul(Bool syned, Bool wid, Bool sz32, HReg dst, HReg srcL,
                         HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Mul;
   i->Min.Mul.syned = syned;
   i->Min.Mul.widening = wid; /* widen=True else False */
   i->Min.Mul.sz32 = sz32; /* True = 32 bits */
   i->Min.Mul.dst = dst;
   i->Min.Mul.srcL = srcL;
   i->Min.Mul.srcR = srcR;
   return i;
}

/* msub */
MIPSInstr *MIPSInstr_Msub(Bool syned, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Macc;

   i->Min.Macc.op = Macc_SUB;
   i->Min.Macc.syned = syned;
   i->Min.Macc.srcL = srcL;
   i->Min.Macc.srcR = srcR;
   return i;
}

/* madd */
MIPSInstr *MIPSInstr_Madd(Bool syned, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Macc;

   i->Min.Macc.op = Macc_ADD;
   i->Min.Macc.syned = syned;
   i->Min.Macc.srcL = srcL;
   i->Min.Macc.srcR = srcR;
   return i;
}

/* div */
MIPSInstr *MIPSInstr_Div(Bool syned, Bool sz32, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Div;
   i->Min.Div.syned = syned;
   i->Min.Div.sz32 = sz32; /* True = 32 bits */
   i->Min.Div.srcL = srcL;
   i->Min.Div.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_Call ( MIPSCondCode cond, Addr64 target, UInt argiregs,
                            HReg src, RetLoc rloc )
{
   UInt mask;
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Call;
   i->Min.Call.cond = cond;
   i->Min.Call.target = target;
   i->Min.Call.argiregs = argiregs;
   i->Min.Call.src = src;
   i->Min.Call.rloc = rloc;
   /* Only $4 .. $7/$11 inclusive may be used as arg regs. */
   mask = (1 << 4) | (1 << 5) | (1 << 6) | (1 << 7) | (1 << 8) | (1 << 9)
          | (1 << 10) | (1 << 11);
   vassert(0 == (argiregs & ~mask));
   vassert(is_sane_RetLoc(rloc));
   return i;
}

MIPSInstr *MIPSInstr_CallAlways ( MIPSCondCode cond, Addr64 target,
                                  UInt argiregs, RetLoc rloc )
{
   UInt mask;
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Call;
   i->Min.Call.cond = cond;
   i->Min.Call.target = target;
   i->Min.Call.argiregs = argiregs;
   i->Min.Call.rloc = rloc;
   /* Only $4 .. $7/$11 inclusive may be used as arg regs. */
   mask = (1 << 4) | (1 << 5) | (1 << 6) | (1 << 7) | (1 << 8) | (1 << 9)
          | (1 << 10) | (1 << 11);
   vassert(0 == (argiregs & ~mask));
   vassert(is_sane_RetLoc(rloc));
   return i;
}

MIPSInstr *MIPSInstr_XDirect ( Addr64 dstGA, MIPSAMode* amPC,
                               MIPSCondCode cond, Bool toFastEP ) {
   MIPSInstr* i               = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag                     = Min_XDirect;
   i->Min.XDirect.dstGA       = dstGA;
   i->Min.XDirect.amPC        = amPC;
   i->Min.XDirect.cond        = cond;
   i->Min.XDirect.toFastEP    = toFastEP;
   return i;
}

MIPSInstr *MIPSInstr_XIndir ( HReg dstGA, MIPSAMode* amPC,
                              MIPSCondCode cond ) {
   MIPSInstr* i            = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag                  = Min_XIndir;
   i->Min.XIndir.dstGA     = dstGA;
   i->Min.XIndir.amPC      = amPC;
   i->Min.XIndir.cond      = cond;
   return i;
}

MIPSInstr *MIPSInstr_XAssisted ( HReg dstGA, MIPSAMode* amPC,
                                 MIPSCondCode cond, IRJumpKind jk ) {
   MIPSInstr* i               = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag                     = Min_XAssisted;
   i->Min.XAssisted.dstGA     = dstGA;
   i->Min.XAssisted.amPC      = amPC;
   i->Min.XAssisted.cond      = cond;
   i->Min.XAssisted.jk        = jk;
   return i;
}

MIPSInstr *MIPSInstr_Load(UChar sz, HReg dst, MIPSAMode * src, Bool mode64)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Load;
   i->Min.Load.sz = sz;
   i->Min.Load.src = src;
   i->Min.Load.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_Store(UChar sz, MIPSAMode * dst, HReg src, Bool mode64)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Store;
   i->Min.Store.sz = sz;
   i->Min.Store.src = src;
   i->Min.Store.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_LoadL(UChar sz, HReg dst, MIPSAMode * src, Bool mode64)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_LoadL;
   i->Min.LoadL.sz  = sz;
   i->Min.LoadL.src = src;
   i->Min.LoadL.dst = dst;
   vassert(sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_Cas(UChar sz, HReg old, HReg addr,
                         HReg expd, HReg data, Bool mode64)
{
   MIPSInstr *i    = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag          = Min_Cas;
   i->Min.Cas.sz   = sz;
   i->Min.Cas.old  = old;
   i->Min.Cas.addr = addr;
   i->Min.Cas.expd = expd;
   i->Min.Cas.data = data;
   vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_StoreC(UChar sz, MIPSAMode * dst, HReg src, Bool mode64)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_StoreC;
   i->Min.StoreC.sz  = sz;
   i->Min.StoreC.src = src;
   i->Min.StoreC.dst = dst;
   vassert(sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_Mthi(HReg src)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Mthi;
   i->Min.MtHL.src = src;
   return i;
}

MIPSInstr *MIPSInstr_Mtlo(HReg src)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Mtlo;
   i->Min.MtHL.src = src;
   return i;
}

MIPSInstr *MIPSInstr_Mfhi(HReg dst)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Mfhi;
   i->Min.MfHL.dst = dst;
   return i;
}

MIPSInstr *MIPSInstr_Mflo(HReg dst)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_Mflo;
   i->Min.MfHL.dst = dst;
   return i;
}

/* Read/Write Link Register */
MIPSInstr *MIPSInstr_RdWrLR(Bool wrLR, HReg gpr)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_RdWrLR;
   i->Min.RdWrLR.wrLR = wrLR;
   i->Min.RdWrLR.gpr = gpr;
   return i;
}

MIPSInstr *MIPSInstr_FpLdSt(Bool isLoad, UChar sz, HReg reg, MIPSAMode * addr)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_FpLdSt;
   i->Min.FpLdSt.isLoad = isLoad;
   i->Min.FpLdSt.sz = sz;
   i->Min.FpLdSt.reg = reg;
   i->Min.FpLdSt.addr = addr;
   vassert(sz == 4 || sz == 8);
   return i;
}

MIPSInstr *MIPSInstr_FpUnary(MIPSFpOp op, HReg dst, HReg src)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_FpUnary;
   i->Min.FpUnary.op = op;
   i->Min.FpUnary.dst = dst;
   i->Min.FpUnary.src = src;
   return i;
}

MIPSInstr *MIPSInstr_FpBinary(MIPSFpOp op, HReg dst, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_FpBinary;
   i->Min.FpBinary.op = op;
   i->Min.FpBinary.dst = dst;
   i->Min.FpBinary.srcL = srcL;
   i->Min.FpBinary.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_FpTernary ( MIPSFpOp op, HReg dst, HReg src1, HReg src2,
                                 HReg src3 )
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_FpTernary;
   i->Min.FpTernary.op = op;
   i->Min.FpTernary.dst = dst;
   i->Min.FpTernary.src1 = src1;
   i->Min.FpTernary.src2 = src2;
   i->Min.FpTernary.src3 = src3;
   return i;
}

MIPSInstr *MIPSInstr_FpConvert(MIPSFpOp op, HReg dst, HReg src)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_FpConvert;
   i->Min.FpConvert.op = op;
   i->Min.FpConvert.dst = dst;
   i->Min.FpConvert.src = src;
   return i;

}

MIPSInstr *MIPSInstr_FpCompare(MIPSFpOp op, HReg dst, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_FpCompare;
   i->Min.FpCompare.op = op;
   i->Min.FpCompare.dst = dst;
   i->Min.FpCompare.srcL = srcL;
   i->Min.FpCompare.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_MtFCSR(HReg src)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_MtFCSR;
   i->Min.MtFCSR.src = src;
   return i;
}

MIPSInstr *MIPSInstr_MfFCSR(HReg dst)
{
   MIPSInstr *i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag = Min_MfFCSR;
   i->Min.MfFCSR.dst = dst;
   return i;
}

MIPSInstr *MIPSInstr_FpGpMove ( MIPSFpGpMoveOp op, HReg dst, HReg src )
{
   MIPSInstr *i        = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag              = Min_FpGpMove;
   i->Min.FpGpMove.op  = op;
   i->Min.FpGpMove.dst = dst;
   i->Min.FpGpMove.src = src;
   return i;
}

MIPSInstr *MIPSInstr_MoveCond ( MIPSMoveCondOp op, HReg dst, HReg src,
                                HReg cond )
{
   MIPSInstr *i        = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag              = Min_MoveCond;
   i->Min.MoveCond.op  = op;
   i->Min.MoveCond.dst = dst;
   i->Min.MoveCond.src = src;
   i->Min.MoveCond.cond = cond;
   return i;
}

MIPSInstr *MIPSInstr_EvCheck ( MIPSAMode* amCounter,
                            MIPSAMode* amFailAddr ) {
   MIPSInstr* i                 = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag                       = Min_EvCheck;
   i->Min.EvCheck.amCounter     = amCounter;
   i->Min.EvCheck.amFailAddr    = amFailAddr;
   return i;
}

MIPSInstr* MIPSInstr_ProfInc ( void ) {
   MIPSInstr* i = LibVEX_Alloc_inline(sizeof(MIPSInstr));
   i->tag       = Min_ProfInc;
   return i;
}

/* -------- Pretty Print instructions ------------- */
static void ppLoadImm(HReg dst, ULong imm, Bool mode64)
{
   vex_printf("li ");
   ppHRegMIPS(dst, mode64);
   vex_printf(",0x%016llx", imm);
}

void ppMIPSInstr(const MIPSInstr * i, Bool mode64)
{
   switch (i->tag) {
      case Min_LI:
         ppLoadImm(i->Min.LI.dst, i->Min.LI.imm, mode64);
         break;
      case Min_Alu: {
         HReg r_srcL = i->Min.Alu.srcL;
         MIPSRH *rh_srcR = i->Min.Alu.srcR;
         /* generic */
         vex_printf("%s ", showMIPSAluOp(i->Min.Alu.op,
                                         toBool(rh_srcR->tag == Mrh_Imm)));
         ppHRegMIPS(i->Min.Alu.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(r_srcL, mode64);
         vex_printf(",");
         ppMIPSRH(rh_srcR, mode64);
         return;
      }
      case Min_Shft: {
         HReg r_srcL = i->Min.Shft.srcL;
         MIPSRH *rh_srcR = i->Min.Shft.srcR;
         vex_printf("%s ", showMIPSShftOp(i->Min.Shft.op,
                                          toBool(rh_srcR->tag == Mrh_Imm),
                                          i->Min.Shft.sz32));
         ppHRegMIPS(i->Min.Shft.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(r_srcL, mode64);
         vex_printf(",");
         ppMIPSRH(rh_srcR, mode64);
         return;
      }
      case Min_Unary: {
         vex_printf("%s ", showMIPSUnaryOp(i->Min.Unary.op));
         ppHRegMIPS(i->Min.Unary.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.Unary.src, mode64);
         return;
      }
      case Min_Cmp: {
         vex_printf("word_compare ");
         ppHRegMIPS(i->Min.Cmp.dst, mode64);
         vex_printf(" = %s ( ", showMIPSCondCode(i->Min.Cmp.cond));
         ppHRegMIPS(i->Min.Cmp.srcL, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Cmp.srcR, mode64);
         vex_printf(" )");

         return;
      }
      case Min_Mul: {
         switch (i->Min.Mul.widening) {
            case False:
               vex_printf("mul ");
               ppHRegMIPS(i->Min.Mul.dst, mode64);
               vex_printf(", ");
               ppHRegMIPS(i->Min.Mul.srcL, mode64);
               vex_printf(", ");
               ppHRegMIPS(i->Min.Mul.srcR, mode64);
               return;
            case True:
               vex_printf("%s%s ", i->Min.Mul.sz32 ? "mult" : "dmult",
                                   i->Min.Mul.syned ? "" : "u");
               ppHRegMIPS(i->Min.Mul.dst, mode64);
               vex_printf(", ");
               ppHRegMIPS(i->Min.Mul.srcL, mode64);
               vex_printf(", ");
               ppHRegMIPS(i->Min.Mul.srcR, mode64);
               return;
            }
         break;
      }
      case Min_Mthi: {
         vex_printf("mthi ");
         ppHRegMIPS(i->Min.MtHL.src, mode64);
         return;
      }
      case Min_Mtlo: {
         vex_printf("mtlo ");
         ppHRegMIPS(i->Min.MtHL.src, mode64);
         return;
      }
      case Min_Mfhi: {
         vex_printf("mfhi ");
         ppHRegMIPS(i->Min.MfHL.dst, mode64);
         return;
      }
      case Min_Mflo: {
         vex_printf("mflo ");
         ppHRegMIPS(i->Min.MfHL.dst, mode64);
         return;
      }
      case Min_Macc: {
         vex_printf("%s ", showMIPSMaccOp(i->Min.Macc.op, i->Min.Macc.syned));
         ppHRegMIPS(i->Min.Macc.srcL, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Macc.srcR, mode64);
         return;
      }
      case Min_Div: {
         if (!i->Min.Div.sz32)
            vex_printf("d");
         vex_printf("div");
         vex_printf("%s ", i->Min.Div.syned ? "s" : "u");
         ppHRegMIPS(i->Min.Div.srcL, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Div.srcR, mode64);
         return;
      }
      case Min_Call: {
         Int n;
         vex_printf("call: ");
         if (i->Min.Call.cond != MIPScc_AL) {
            vex_printf("if (%s) ", showMIPSCondCode(i->Min.Call.cond));
         }
         vex_printf(" {");
         if (!mode64)
            vex_printf(" addiu $29, $29, -16");

         ppLoadImm(hregMIPS_GPR25(mode64), i->Min.Call.target, mode64);

         vex_printf(" ; jarl $31, $25; # args [");
         for (n = 0; n < 32; n++) {
            if (i->Min.Call.argiregs & (1 << n)) {
               vex_printf("$%d", n);
               if ((i->Min.Call.argiregs >> n) > 1)
                  vex_printf(",");
            }
         }
         vex_printf("] nop; ");
         if (!mode64)
            vex_printf("addiu $29, $29, 16; ]");

         break;
      }
      case Min_XDirect:
         vex_printf("(xDirect) ");
         vex_printf("if (guest_COND.%s) { ",
                    showMIPSCondCode(i->Min.XDirect.cond));
         vex_printf("move $9, 0x%x,", (UInt)i->Min.XDirect.dstGA);
         vex_printf("; sw $9, ");
         ppMIPSAMode(i->Min.XDirect.amPC, mode64);
         vex_printf("; move $9, $disp_cp_chain_me_to_%sEP; jalr $9; nop}",
                    i->Min.XDirect.toFastEP ? "fast" : "slow");
         return;
      case Min_XIndir:
         vex_printf("(xIndir) ");
         vex_printf("if (guest_COND.%s) { sw ",
                    showMIPSCondCode(i->Min.XIndir.cond));
         ppHRegMIPS(i->Min.XIndir.dstGA, mode64);
         vex_printf(", ");
         ppMIPSAMode(i->Min.XIndir.amPC, mode64);
         vex_printf("; move $9, $disp_indir; jalr $9; nop}");
         return;
      case Min_XAssisted:
         vex_printf("(xAssisted) ");
         vex_printf("if (guest_COND.%s) { ",
                    showMIPSCondCode(i->Min.XAssisted.cond));
         vex_printf("sw ");
         ppHRegMIPS(i->Min.XAssisted.dstGA, mode64);
         vex_printf(", ");
         ppMIPSAMode(i->Min.XAssisted.amPC, mode64);
         vex_printf("; move $9, $IRJumpKind_to_TRCVAL(%d)",
                    (Int)i->Min.XAssisted.jk);
         vex_printf("; move $9, $disp_assisted; jalr $9; nop; }");
         return;
      case Min_Load: {
         Bool idxd = toBool(i->Min.Load.src->tag == Mam_RR);
         UChar sz = i->Min.Load.sz;
         HChar c_sz = sz == 1 ? 'b' : sz == 2 ? 'h' : sz == 4 ? 'w' : 'd';
         vex_printf("l%c%s ", c_sz, idxd ? "x" : "");
         ppHRegMIPS(i->Min.Load.dst, mode64);
         vex_printf(",");
         ppMIPSAMode(i->Min.Load.src, mode64);
         return;
      }
      case Min_Store: {
         UChar sz = i->Min.Store.sz;
         Bool idxd = toBool(i->Min.Store.dst->tag == Mam_RR);
         HChar c_sz = sz == 1 ? 'b' : sz == 2 ? 'h' : sz == 4 ? 'w' : 'd';
         vex_printf("s%c%s ", c_sz, idxd ? "x" : "");
         ppHRegMIPS(i->Min.Store.src, mode64);
         vex_printf(",");
         ppMIPSAMode(i->Min.Store.dst, mode64);
         return;
      }
      case Min_LoadL: {
         vex_printf("ll ");
         ppHRegMIPS(i->Min.LoadL.dst, mode64);
         vex_printf(",");
         ppMIPSAMode(i->Min.LoadL.src, mode64);
         return;
      }
      case Min_Cas: {
          Bool sz8  = toBool(i->Min.Cas.sz == 8);
          /*
           * ll(d)    old,  0(addr)
           * bne      old,  expd, end
           * nop
           * (d)addiu old,  old,  1
           * sc(d)    data, 0(addr)
           * movn     old,  expd, data
           * end:
           */
          // ll(d) old, 0(addr)
         vex_printf("cas: ");

         vex_printf("%s ", sz8 ? "lld" : "ll");
         ppHRegMIPS(i->Min.Cas.old , mode64);
         vex_printf(", 0(");
         ppHRegMIPS(i->Min.Cas.addr , mode64);
         vex_printf(")\n");

         vex_printf("bne ");
         ppHRegMIPS(i->Min.Cas.old , mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Cas.expd , mode64);
         vex_printf(", end\n");

         vex_printf("nop\n");

         vex_printf("%s ", sz8 ? "daddiu" : "addiu");
         ppHRegMIPS(i->Min.Cas.old , mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Cas.old , mode64);
         vex_printf(", 1\n");

         vex_printf("%s ", sz8 ? "scd" : "sc");
         ppHRegMIPS(i->Min.Cas.data , mode64);
         vex_printf(", 0(");
         ppHRegMIPS(i->Min.Cas.addr , mode64);
         vex_printf(")\n");

         vex_printf("movn ");
         ppHRegMIPS(i->Min.Cas.old , mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Cas.expd , mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Cas.data , mode64);
         vex_printf("\nend:");
         return;
      }
      case Min_StoreC: {
         vex_printf("sc ");
         ppHRegMIPS(i->Min.StoreC.src, mode64);
         vex_printf(",");
         ppMIPSAMode(i->Min.StoreC.dst, mode64);
         return;
      }
      case Min_RdWrLR: {
         vex_printf("%s ", i->Min.RdWrLR.wrLR ? "mtlr" : "mflr");
         ppHRegMIPS(i->Min.RdWrLR.gpr, mode64);
         return;
      }
      case Min_FpUnary:
         vex_printf("%s ", showMIPSFpOp(i->Min.FpUnary.op));
         ppHRegMIPS(i->Min.FpUnary.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpUnary.src, mode64);
         return;
      case Min_FpBinary:
         vex_printf("%s", showMIPSFpOp(i->Min.FpBinary.op));
         ppHRegMIPS(i->Min.FpBinary.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpBinary.srcL, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpBinary.srcR, mode64);
         return;
      case Min_FpTernary:
         vex_printf("%s", showMIPSFpOp(i->Min.FpTernary.op));
         ppHRegMIPS(i->Min.FpTernary.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpTernary.src1, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpTernary.src2, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpTernary.src3, mode64);
         return;
      case Min_FpConvert:
         vex_printf("%s", showMIPSFpOp(i->Min.FpConvert.op));
         ppHRegMIPS(i->Min.FpConvert.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpConvert.src, mode64);
         return;
      case Min_FpCompare:
         vex_printf("%s ", showMIPSFpOp(i->Min.FpCompare.op));
         ppHRegMIPS(i->Min.FpCompare.srcL, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpCompare.srcR, mode64);
         return;
      case Min_FpMulAcc:
         vex_printf("%s ", showMIPSFpOp(i->Min.FpMulAcc.op));
         ppHRegMIPS(i->Min.FpMulAcc.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpMulAcc.srcML, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpMulAcc.srcMR, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpMulAcc.srcAcc, mode64);
         return;
      case Min_FpLdSt: {
         if (i->Min.FpLdSt.sz == 4) {
            if (i->Min.FpLdSt.isLoad) {
               vex_printf("lwc1 ");
               ppHRegMIPS(i->Min.FpLdSt.reg, mode64);
               vex_printf(",");
               ppMIPSAMode(i->Min.FpLdSt.addr, mode64);
            } else {
               vex_printf("swc1 ");
               ppHRegMIPS(i->Min.FpLdSt.reg, mode64);
               vex_printf(",");
               ppMIPSAMode(i->Min.FpLdSt.addr, mode64);
            }
         } else if (i->Min.FpLdSt.sz == 8) {
            if (i->Min.FpLdSt.isLoad) {
               vex_printf("ldc1 ");
               ppHRegMIPS(i->Min.FpLdSt.reg, mode64);
               vex_printf(",");
               ppMIPSAMode(i->Min.FpLdSt.addr, mode64);
            } else {
               vex_printf("sdc1 ");
               ppHRegMIPS(i->Min.FpLdSt.reg, mode64);
               vex_printf(",");
               ppMIPSAMode(i->Min.FpLdSt.addr, mode64);
            }
         }
         return;
      }
      case Min_MtFCSR: {
         vex_printf("ctc1 ");
         ppHRegMIPS(i->Min.MtFCSR.src, mode64);
         vex_printf(", $31");
         return;
      }
      case Min_MfFCSR: {
         vex_printf("ctc1 ");
         ppHRegMIPS(i->Min.MfFCSR.dst, mode64);
         vex_printf(", $31");
         return;
      }
      case Min_FpGpMove: {
         vex_printf("%s ", showMIPSFpGpMoveOp(i->Min.FpGpMove.op));
         ppHRegMIPS(i->Min.FpGpMove.dst, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.FpGpMove.src, mode64);
         return;
      }
      case Min_MoveCond: {
         vex_printf("%s", showMIPSMoveCondOp(i->Min.MoveCond.op));
         ppHRegMIPS(i->Min.MoveCond.dst, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.MoveCond.src, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.MoveCond.cond, mode64);
         return;
      }
      case Min_EvCheck:
         vex_printf("(evCheck) lw $9, ");
         ppMIPSAMode(i->Min.EvCheck.amCounter, mode64);
         vex_printf("; addiu $9, $9, -1");
         vex_printf("; sw $9, ");
         ppMIPSAMode(i->Min.EvCheck.amCounter, mode64);
         vex_printf("; bgez $t9, nofail; jalr *");
         ppMIPSAMode(i->Min.EvCheck.amFailAddr, mode64);
         vex_printf("; nofail:");
         return;
      case Min_ProfInc:
         if (mode64)
            vex_printf("(profInc) move $9, ($NotKnownYet); "
                       "ld $8, 0($9); "
                       "daddiu $8, $8, 1; "
                       "sd $8, 0($9); " );
         else
            vex_printf("(profInc) move $9, ($NotKnownYet); "
                       "lw $8, 0($9); "
                       "addiu $8, $8, 1; "
                       "sw $8, 0($9); "
                       "sltiu $1, $8, 1; "
                       "lw $8, 4($9); "
                       "addu $8, $8, $1; "
                       "sw $8, 4($9); " );
         return;
      default:
         vpanic("ppMIPSInstr");
         break;
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_MIPSInstr(HRegUsage * u, const MIPSInstr * i, Bool mode64)
{
   initHRegUsage(u);
   switch (i->tag) {
      case Min_LI:
         addHRegUse(u, HRmWrite, i->Min.LI.dst);
         break;
      case Min_Alu:
         addHRegUse(u, HRmRead, i->Min.Alu.srcL);
         addRegUsage_MIPSRH(u, i->Min.Alu.srcR);
         addHRegUse(u, HRmWrite, i->Min.Alu.dst);
         return;
      case Min_Shft:
         addHRegUse(u, HRmRead, i->Min.Shft.srcL);
         addRegUsage_MIPSRH(u, i->Min.Shft.srcR);
         addHRegUse(u, HRmWrite, i->Min.Shft.dst);
         return;
      case Min_Cmp:
         addHRegUse(u, HRmRead, i->Min.Cmp.srcL);
         addHRegUse(u, HRmRead, i->Min.Cmp.srcR);
         addHRegUse(u, HRmWrite, i->Min.Cmp.dst);
         return;
      case Min_Unary:
         addHRegUse(u, HRmRead, i->Min.Unary.src);
         addHRegUse(u, HRmWrite, i->Min.Unary.dst);
         return;
      case Min_Mul:
         addHRegUse(u, HRmWrite, i->Min.Mul.dst);
         addHRegUse(u, HRmRead, i->Min.Mul.srcL);
         addHRegUse(u, HRmRead, i->Min.Mul.srcR);
         return;
      case Min_Mthi:
      case Min_Mtlo:
         addHRegUse(u, HRmWrite, hregMIPS_HI(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_LO(mode64));
         addHRegUse(u, HRmRead, i->Min.MtHL.src);
         return;
      case Min_Mfhi:
      case Min_Mflo:
         addHRegUse(u, HRmRead, hregMIPS_HI(mode64));
         addHRegUse(u, HRmRead, hregMIPS_LO(mode64));
         addHRegUse(u, HRmWrite, i->Min.MfHL.dst);
         return;
      case Min_MtFCSR:
         addHRegUse(u, HRmRead, i->Min.MtFCSR.src);
         return;
      case Min_MfFCSR:
         addHRegUse(u, HRmWrite, i->Min.MfFCSR.dst);
         return;
      case Min_Macc:
         addHRegUse(u, HRmModify, hregMIPS_HI(mode64));
         addHRegUse(u, HRmModify, hregMIPS_LO(mode64));
         addHRegUse(u, HRmRead, i->Min.Macc.srcL);
         addHRegUse(u, HRmRead, i->Min.Macc.srcR);
         return;
      case Min_Div:
         addHRegUse(u, HRmWrite, hregMIPS_HI(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_LO(mode64));
         addHRegUse(u, HRmRead, i->Min.Div.srcL);
         addHRegUse(u, HRmRead, i->Min.Div.srcR);
         return;
      case Min_Call: {
         /* Logic and comments copied/modified from x86, ppc and arm back end.
            First off, claim it trashes all the caller-saved regs
            which fall within the register allocator's jurisdiction. */
         if (i->Min.Call.cond != MIPScc_AL)
            addHRegUse(u, HRmRead, i->Min.Call.src);
         UInt argir;
         addHRegUse(u, HRmWrite, hregMIPS_GPR1(mode64));

         addHRegUse(u, HRmWrite, hregMIPS_GPR2(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR3(mode64));

         addHRegUse(u, HRmWrite, hregMIPS_GPR4(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR5(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR6(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR7(mode64));

         addHRegUse(u, HRmWrite, hregMIPS_GPR8(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR9(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR10(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR11(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR12(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR13(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR14(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR15(mode64));

         addHRegUse(u, HRmWrite, hregMIPS_GPR24(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR25(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR31(mode64));

         /* Now we have to state any parameter-carrying registers
            which might be read. This depends on the argiregs field. */
         argir = i->Min.Call.argiregs;
         if (argir & (1<<11)) addHRegUse(u, HRmRead, hregMIPS_GPR11(mode64));
         if (argir & (1<<10)) addHRegUse(u, HRmRead, hregMIPS_GPR10(mode64));
         if (argir & (1<<9)) addHRegUse(u, HRmRead, hregMIPS_GPR9(mode64));
         if (argir & (1<<8)) addHRegUse(u, HRmRead, hregMIPS_GPR8(mode64));
         if (argir & (1<<7)) addHRegUse(u, HRmRead, hregMIPS_GPR7(mode64));
         if (argir & (1<<6)) addHRegUse(u, HRmRead, hregMIPS_GPR6(mode64));
         if (argir & (1<<5)) addHRegUse(u, HRmRead, hregMIPS_GPR5(mode64));
         if (argir & (1<<4)) addHRegUse(u, HRmRead, hregMIPS_GPR4(mode64));

         vassert(0 == (argir & ~((1 << 4) | (1 << 5) | (1 << 6)
                                 | (1 << 7) | (1 << 8) | (1 << 9) | (1 << 10)
                                 | (1 << 11))));

         return;
      }
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case Min_XDirect:
         addRegUsage_MIPSAMode(u, i->Min.XDirect.amPC);
         return;
      case Min_XIndir:
         addHRegUse(u, HRmRead, i->Min.XIndir.dstGA);
         addRegUsage_MIPSAMode(u, i->Min.XIndir.amPC);
         return;
      case Min_XAssisted:
         addHRegUse(u, HRmRead, i->Min.XAssisted.dstGA);
         addRegUsage_MIPSAMode(u, i->Min.XAssisted.amPC);
         return;
      case Min_Load:
         addRegUsage_MIPSAMode(u, i->Min.Load.src);
         addHRegUse(u, HRmWrite, i->Min.Load.dst);
         return;
      case Min_Store:
         addHRegUse(u, HRmRead, i->Min.Store.src);
         addRegUsage_MIPSAMode(u, i->Min.Store.dst);
         return;
      case Min_LoadL:
         addRegUsage_MIPSAMode(u, i->Min.LoadL.src);
         addHRegUse(u, HRmWrite, i->Min.LoadL.dst);
         return;
      case Min_Cas:
         addHRegUse(u, HRmWrite, i->Min.Cas.old);
         addHRegUse(u, HRmRead, i->Min.Cas.addr);
         addHRegUse(u, HRmRead, i->Min.Cas.expd);
         addHRegUse(u, HRmModify, i->Min.Cas.data);
         return;
      case Min_StoreC:
         addHRegUse(u, HRmWrite, i->Min.StoreC.src);
         addHRegUse(u, HRmRead, i->Min.StoreC.src);
         addRegUsage_MIPSAMode(u, i->Min.StoreC.dst);
         return;
      case Min_RdWrLR:
         addHRegUse(u, (i->Min.RdWrLR.wrLR ? HRmRead : HRmWrite),
                        i->Min.RdWrLR.gpr);
         return;
      case Min_FpLdSt:
         if (i->Min.FpLdSt.sz == 4) {
            addHRegUse(u, (i->Min.FpLdSt.isLoad ? HRmWrite : HRmRead),
                           i->Min.FpLdSt.reg);
            addRegUsage_MIPSAMode(u, i->Min.FpLdSt.addr);
            return;
         } else if (i->Min.FpLdSt.sz == 8) {
            addHRegUse(u, (i->Min.FpLdSt.isLoad ? HRmWrite : HRmRead),
                           i->Min.FpLdSt.reg);
            addRegUsage_MIPSAMode(u, i->Min.FpLdSt.addr);
            return;
         }
         break;
      case Min_FpUnary:
         addHRegUse(u, HRmWrite, i->Min.FpUnary.dst);
         addHRegUse(u, HRmRead, i->Min.FpUnary.src);
         return;
      case Min_FpBinary:
         addHRegUse(u, HRmWrite, i->Min.FpBinary.dst);
         addHRegUse(u, HRmRead, i->Min.FpBinary.srcL);
         addHRegUse(u, HRmRead, i->Min.FpBinary.srcR);
         return;
      case Min_FpTernary:
         addHRegUse(u, HRmWrite, i->Min.FpTernary.dst);
         addHRegUse(u, HRmRead, i->Min.FpTernary.src1);
         addHRegUse(u, HRmRead, i->Min.FpTernary.src2);
         addHRegUse(u, HRmRead, i->Min.FpTernary.src3);
         return;
      case Min_FpConvert:
         addHRegUse(u, HRmWrite, i->Min.FpConvert.dst);
         addHRegUse(u, HRmRead, i->Min.FpConvert.src);
         return;
      case Min_FpCompare:
         addHRegUse(u, HRmWrite, i->Min.FpCompare.dst);
         addHRegUse(u, HRmRead, i->Min.FpCompare.srcL);
         addHRegUse(u, HRmRead, i->Min.FpCompare.srcR);
         return;
      case Min_FpGpMove:
         addHRegUse(u, HRmWrite, i->Min.FpGpMove.dst);
         addHRegUse(u, HRmRead, i->Min.FpGpMove.src);
         return;
      case Min_MoveCond:
         addHRegUse(u, HRmModify, i->Min.MoveCond.dst);
         addHRegUse(u, HRmRead, i->Min.MoveCond.src);
         addHRegUse(u, HRmRead, i->Min.MoveCond.cond);
         return;
      case Min_EvCheck:
         /* We expect both amodes only to mention %ebp, so this is in
            fact pointless, since %ebp isn't allocatable, but anyway.. */
         addRegUsage_MIPSAMode(u, i->Min.EvCheck.amCounter);
         addRegUsage_MIPSAMode(u, i->Min.EvCheck.amFailAddr);
         return;
      case Min_ProfInc:
         /* does not use any registers. */
         return;
      default:
         ppMIPSInstr(i, mode64);
         vpanic("getRegUsage_MIPSInstr");
         break;
   }
}

/* local helper */
static void mapReg(HRegRemap * m, HReg * r)
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_MIPSInstr(HRegRemap * m, MIPSInstr * i, Bool mode64)
{
   switch (i->tag) {
      case Min_LI:
         mapReg(m, &i->Min.LI.dst);
         break;
      case Min_Alu:
         mapReg(m, &i->Min.Alu.srcL);
         mapRegs_MIPSRH(m, i->Min.Alu.srcR);
         mapReg(m, &i->Min.Alu.dst);
         return;
      case Min_Shft:
         mapReg(m, &i->Min.Shft.srcL);
         mapRegs_MIPSRH(m, i->Min.Shft.srcR);
         mapReg(m, &i->Min.Shft.dst);
         return;
      case Min_Cmp:
         mapReg(m, &i->Min.Cmp.srcL);
         mapReg(m, &i->Min.Cmp.srcR);
         mapReg(m, &i->Min.Cmp.dst);
         return;
      case Min_Unary:
         mapReg(m, &i->Min.Unary.src);
         mapReg(m, &i->Min.Unary.dst);
         return;
      case Min_Mul:
         mapReg(m, &i->Min.Mul.dst);
         mapReg(m, &i->Min.Mul.srcL);
         mapReg(m, &i->Min.Mul.srcR);
         return;
      case Min_Mthi:
      case Min_Mtlo:
         mapReg(m, &i->Min.MtHL.src);
         return;
      case Min_Mfhi:
      case Min_Mflo:
         mapReg(m, &i->Min.MfHL.dst);
         return;
      case Min_Macc:
         mapReg(m, &i->Min.Macc.srcL);
         mapReg(m, &i->Min.Macc.srcR);
         return;
      case Min_Div:
         mapReg(m, &i->Min.Div.srcL);
         mapReg(m, &i->Min.Div.srcR);
         return;
      case Min_Call:
         {
            if (i->Min.Call.cond != MIPScc_AL)
               mapReg(m, &i->Min.Call.src);
            return;
         }
      case Min_XDirect:
         mapRegs_MIPSAMode(m, i->Min.XDirect.amPC);
         return;
      case Min_XIndir:
         mapReg(m, &i->Min.XIndir.dstGA);
         mapRegs_MIPSAMode(m, i->Min.XIndir.amPC);
         return;
      case Min_XAssisted:
         mapReg(m, &i->Min.XAssisted.dstGA);
         mapRegs_MIPSAMode(m, i->Min.XAssisted.amPC);
         return;
      case Min_Load:
         mapRegs_MIPSAMode(m, i->Min.Load.src);
         mapReg(m, &i->Min.Load.dst);
         return;
      case Min_Store:
         mapReg(m, &i->Min.Store.src);
         mapRegs_MIPSAMode(m, i->Min.Store.dst);
         return;
      case Min_LoadL:
         mapRegs_MIPSAMode(m, i->Min.LoadL.src);
         mapReg(m, &i->Min.LoadL.dst);
         return;
      case Min_Cas:
         mapReg(m, &i->Min.Cas.old);
         mapReg(m, &i->Min.Cas.addr);
         mapReg(m, &i->Min.Cas.expd);
         mapReg(m, &i->Min.Cas.data);
         return;
      case Min_StoreC:
         mapReg(m, &i->Min.StoreC.src);
         mapRegs_MIPSAMode(m, i->Min.StoreC.dst);
         return;
      case Min_RdWrLR:
         mapReg(m, &i->Min.RdWrLR.gpr);
         return;
      case Min_FpLdSt:
         if (i->Min.FpLdSt.sz == 4) {
            mapReg(m, &i->Min.FpLdSt.reg);
            mapRegs_MIPSAMode(m, i->Min.FpLdSt.addr);
            return;
         } else if (i->Min.FpLdSt.sz == 8) {
            mapReg(m, &i->Min.FpLdSt.reg);
            mapRegs_MIPSAMode(m, i->Min.FpLdSt.addr);
            return;
         }
         break;
      case Min_FpUnary:
         mapReg(m, &i->Min.FpUnary.dst);
         mapReg(m, &i->Min.FpUnary.src);
         return;
      case Min_FpBinary:
         mapReg(m, &i->Min.FpBinary.dst);
         mapReg(m, &i->Min.FpBinary.srcL);
         mapReg(m, &i->Min.FpBinary.srcR);
         return;
      case Min_FpTernary:
         mapReg(m, &i->Min.FpTernary.dst);
         mapReg(m, &i->Min.FpTernary.src1);
         mapReg(m, &i->Min.FpTernary.src2);
         mapReg(m, &i->Min.FpTernary.src3);
         return;
      case Min_FpConvert:
         mapReg(m, &i->Min.FpConvert.dst);
         mapReg(m, &i->Min.FpConvert.src);
         return;
      case Min_FpCompare:
         mapReg(m, &i->Min.FpCompare.dst);
         mapReg(m, &i->Min.FpCompare.srcL);
         mapReg(m, &i->Min.FpCompare.srcR);
         return;
      case Min_MtFCSR:
         mapReg(m, &i->Min.MtFCSR.src);
         return;
      case Min_MfFCSR:
         mapReg(m, &i->Min.MfFCSR.dst);
         return;
      case Min_FpGpMove:
         mapReg(m, &i->Min.FpGpMove.dst);
         mapReg(m, &i->Min.FpGpMove.src);
         return;
      case Min_MoveCond:
         mapReg(m, &i->Min.MoveCond.dst);
         mapReg(m, &i->Min.MoveCond.src);
         mapReg(m, &i->Min.MoveCond.cond);
         return;
      case Min_EvCheck:
         /* We expect both amodes only to mention %ebp, so this is in
            fact pointless, since %ebp isn't allocatable, but anyway.. */
         mapRegs_MIPSAMode(m, i->Min.EvCheck.amCounter);
         mapRegs_MIPSAMode(m, i->Min.EvCheck.amFailAddr);
         return;
      case Min_ProfInc:
         /* does not use any registers. */
         return;
      default:
         ppMIPSInstr(i, mode64);
         vpanic("mapRegs_MIPSInstr");
         break;
   }

}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing.
*/
Bool isMove_MIPSInstr(const MIPSInstr * i, HReg * src, HReg * dst)
{
   /* Moves between integer regs */
   if (i->tag == Min_Alu) {
      /* or Rd,Rs,Rs == mr Rd,Rs */
      if (i->Min.Alu.op != Malu_OR)
         return False;
      if (i->Min.Alu.srcR->tag != Mrh_Reg)
         return False;
      if (!sameHReg(i->Min.Alu.srcR->Mrh.Reg.reg, i->Min.Alu.srcL))
         return False;
      *src = i->Min.Alu.srcL;
      *dst = i->Min.Alu.dst;
      return True;
   }
   return False;
}

/* Generate mips spill/reload instructions under the direction of the
   register allocator. */
void genSpill_MIPS( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2, HReg rreg,
                    Int offsetB, Bool mode64)
{
   MIPSAMode *am;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   *i1 = *i2 = NULL;
   am = MIPSAMode_IR(offsetB, GuestStatePointer(mode64));

   switch (hregClass(rreg)) {
      case HRcInt64:
         vassert(mode64);
         *i1 = MIPSInstr_Store(8, am, rreg, mode64);
         break;
      case HRcInt32:
         vassert(!mode64);
         *i1 = MIPSInstr_Store(4, am, rreg, mode64);
         break;
      case HRcFlt32:
         vassert(!mode64);
         *i1 = MIPSInstr_FpLdSt(False /*Store */ , 4, rreg, am);
         break;
      case HRcFlt64:
         *i1 = MIPSInstr_FpLdSt(False /*Store */ , 8, rreg, am);
         break;
      default:
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_MIPS: unimplemented regclass");
         break;
   }
}

void genReload_MIPS( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2, HReg rreg,
                     Int offsetB, Bool mode64)
{
   MIPSAMode *am;
   vassert(!hregIsVirtual(rreg));
   am = MIPSAMode_IR(offsetB, GuestStatePointer(mode64));

   switch (hregClass(rreg)) {
      case HRcInt64:
         vassert(mode64);
         *i1 = MIPSInstr_Load(8, rreg, am, mode64);
         break;
      case HRcInt32:
         vassert(!mode64);
         *i1 = MIPSInstr_Load(4, rreg, am, mode64);
         break;
      case HRcFlt32:
         if (mode64)
            *i1 = MIPSInstr_FpLdSt(True /*Load */ , 8, rreg, am);
         else
            *i1 = MIPSInstr_FpLdSt(True /*Load */ , 4, rreg, am);
         break;
      case HRcFlt64:
         *i1 = MIPSInstr_FpLdSt(True /*Load */ , 8, rreg, am);
         break;
      default:
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_MIPS: unimplemented regclass");
         break;
   }
}

/* --------- The mips assembler --------- */

inline static UInt iregNo(HReg r, Bool mode64)
{
   UInt n;
   vassert(hregClass(r) == (mode64 ? HRcInt64 : HRcInt32));
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 32);
   return n;
}

inline static UInt fregNo(HReg r, Bool mode64)
{
   UInt n;
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 31);
   return n;
}

inline static UInt dregNo(HReg r)
{
   UInt n;
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 31);
   return n;
}

/* Emit 32bit instruction */
static UChar *emit32(UChar * p, UInt w32)
{
#if defined (_MIPSEL)
   *p++ = toUChar(w32 & 0x000000FF);
   *p++ = toUChar((w32 >> 8) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
/* HACK !!!!
   MIPS endianess is decided at compile time using gcc defined
   symbols _MIPSEL or _MIPSEB. When compiling libvex in a cross-arch
   setup, then none of these is defined. We just choose here by default
   mips Big Endian to allow libvexmultiarch_test to work when using
   a mips host architecture.
   A cleaner way would be to either have mips using 'dynamic endness'
   (like ppc64be or le, decided at runtime) or at least defining
   by default _MIPSEB when compiling on a non mips system.
#elif defined (_MIPSEB).
*/
#else
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >> 8) & 0x000000FF);
   *p++ = toUChar(w32 & 0x000000FF);
#endif
   return p;
}
/* Fetch an instruction */
static UInt fetch32 ( UChar* p )
{
   UInt w32 = 0;
#if defined (_MIPSEL)
   w32 |= ((0xFF & (UInt)p[0]) << 0);
   w32 |= ((0xFF & (UInt)p[1]) << 8);
   w32 |= ((0xFF & (UInt)p[2]) << 16);
   w32 |= ((0xFF & (UInt)p[3]) << 24);
#elif defined (_MIPSEB)
   w32 |= ((0xFF & (UInt)p[0]) << 24);
   w32 |= ((0xFF & (UInt)p[1]) << 16);
   w32 |= ((0xFF & (UInt)p[2]) <<  8);
   w32 |= ((0xFF & (UInt)p[3]) <<  0);
#endif
   return w32;
}

/* physical structure of mips instructions */
/* type I : opcode    - 6 bits
         rs         - 5 bits
         rt         - 5 bits
         immediate - 16 bits
*/
static UChar *mkFormI(UChar * p, UInt opc, UInt rs, UInt rt, UInt imm)
{
   UInt theInstr;
   vassert(opc < 0x40);
   vassert(rs < 0x20);
   vassert(rt < 0x20);
   imm = imm & 0xFFFF;
   theInstr = ((opc << 26) | (rs << 21) | (rt << 16) | (imm));
   return emit32(p, theInstr);
}

/* type R: opcode    - 6 bits
         rs    - 5 bits
         rt    - 5 bits
         rd    - 5 bits
         sa    - 5 bits
         func  - 6 bits
*/
static UChar *mkFormR(UChar * p, UInt opc, UInt rs, UInt rt, UInt rd, UInt sa,
            UInt func)
{
   if (rs >= 0x20)
      vex_printf("rs = %u\n", rs);
   UInt theInstr;
   vassert(opc < 0x40);
   vassert(rs < 0x20);
   vassert(rt < 0x20);
   vassert(rd < 0x20);
   vassert(sa < 0x20);
   func = func & 0xFFFF;
   theInstr = ((opc << 26) | (rs << 21) | (rt << 16) | (rd << 11) | (sa << 6) |
               (func));

   return emit32(p, theInstr);
}

static UChar *mkFormS(UChar * p, UInt opc1, UInt rRD, UInt rRS, UInt rRT,
                      UInt sa, UInt opc2)
{
   UInt theInstr;
   vassert(opc1 <= 0x3F);
   vassert(rRD < 0x20);
   vassert(rRS < 0x20);
   vassert(rRT < 0x20);
   vassert(opc2 <= 0x3F);
   vassert(sa >= 0 && sa <= 0x3F);

   theInstr = ((opc1 << 26) | (rRS << 21) | (rRT << 16) | (rRD << 11) |
              ((sa & 0x1F) << 6) | (opc2));

   return emit32(p, theInstr);
}

static UChar *doAMode_IR(UChar * p, UInt opc1, UInt rSD, MIPSAMode * am,
                         Bool mode64)
{
   UInt rA, idx, r_dst;
   vassert(am->tag == Mam_IR);
   vassert(am->Mam.IR.index < 0x10000);

   rA = iregNo(am->Mam.IR.base, mode64);
   idx = am->Mam.IR.index;

   if (rSD == 33 || rSD == 34)
      r_dst = 24;
   else
      r_dst = rSD;

   if (opc1 < 40) {
      /* load */
      if (rSD == 33)
         /* mfhi */
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 16);
      else if (rSD == 34)
         /* mflo */
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 18);
   }

   p = mkFormI(p, opc1, rA, r_dst, idx);

   if (opc1 >= 40) {
      /* store */
      if (rSD == 33)
         /* mthi */
         p = mkFormR(p, 0, r_dst, 0, 0, 0, 17);
      else if (rSD == 34)
         /* mtlo */
         p = mkFormR(p, 0, r_dst, 0, 0, 0, 19);
   }

   return p;
}

static UChar *doAMode_RR(UChar * p, UInt opc1, UInt rSD, MIPSAMode * am,
                         Bool mode64)
{
   UInt rA, rB, r_dst;
   vassert(am->tag == Mam_RR);

   rA = iregNo(am->Mam.RR.base, mode64);
   rB = iregNo(am->Mam.RR.index, mode64);

   if (rSD == 33 || rSD == 34)
      r_dst = 24;
   else
      r_dst = rSD;

   if (opc1 < 40) {
      /* load */
      if (rSD == 33)
         /* mfhi */
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 16);
      else if (rSD == 34)
         /* mflo */
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 18);
   }

   if (mode64) {
      /* daddu rA, rA, rB$
         sd/ld r_dst, 0(rA)$
         dsubu rA, rA, rB */
      p = mkFormR(p, 0, rA, rB, rA, 0, 45);
      p = mkFormI(p, opc1, rA, r_dst, 0);
      p = mkFormR(p, 0, rA, rB, rA, 0, 47);
   } else {
      /* addu rA, rA, rB
         sw/lw r_dst, 0(rA)
         subu rA, rA, rB */
      p = mkFormR(p, 0, rA, rB, rA, 0, 33);
      p = mkFormI(p, opc1, rA, r_dst, 0);
      p = mkFormR(p, 0, rA, rB, rA, 0, 35);
   }
   if (opc1 >= 40) {
      /* store */
      if (rSD == 33)
         /* mthi */
         p = mkFormR(p, 0, r_dst, 0, 0, 0, 17);
      else if (rSD == 34)
         /* mtlo */
         p = mkFormR(p, 0, r_dst, 0, 0, 0, 19);
   }

   return p;
}

/* Load imm to r_dst */
static UChar *mkLoadImm(UChar * p, UInt r_dst, ULong imm, Bool mode64)
{
   if (!mode64) {
      vassert(r_dst < 0x20);
      UInt u32 = (UInt) imm;
      Int s32 = (Int) u32;
      Long s64 = (Long) s32;
      imm = (ULong) s64;
   }

   if (imm >= 0xFFFFFFFFFFFF8000ULL || imm < 0x8000) {
      /* sign-extendable from 16 bits
         addiu r_dst, 0, imm  => li r_dst, imm */
      p = mkFormI(p, 9, 0, r_dst, imm & 0xFFFF);
   } else {
      if (imm >= 0xFFFFFFFF80000000ULL || imm < 0x80000000ULL) {
         /* sign-extendable from 32 bits
            addiu r_dst, r0, (imm >> 16) => lis r_dst, (imm >> 16)
            lui r_dst, (imm >> 16) */
         p = mkFormI(p, 15, 0, r_dst, (imm >> 16) & 0xFFFF);
         /* ori r_dst, r_dst, (imm & 0xFFFF) */
         p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
      } else {
         vassert(mode64);
         /* lui load in upper half of low word */
         p = mkFormI(p, 15, 0, r_dst, (imm >> 48) & 0xFFFF);
         /* ori */
         p = mkFormI(p, 13, r_dst, r_dst, (imm >> 32) & 0xFFFF);
         /* shift */
         p = mkFormS(p, 0, r_dst, 0, r_dst, 16, 56);
         /* ori */
         p = mkFormI(p, 13, r_dst, r_dst, (imm >> 16) & 0xFFFF);
         /* shift */
         p = mkFormS(p, 0, r_dst, 0, r_dst, 16, 56);
         /* ori */
         p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
      }
   }
   return p;
}

/* A simplified version of mkLoadImm that always generates 2 or 6
   instructions (32 or 64 bits respectively) even if it could generate
   fewer.  This is needed for generating fixed sized patchable
   sequences. */
static UChar* mkLoadImm_EXACTLY2or6 ( UChar* p,
                                      UInt r_dst, ULong imm, Bool mode64)
{
   vassert(r_dst < 0x20);

   if (!mode64) {
      /* In 32-bit mode, make sure the top 32 bits of imm are a sign
         extension of the bottom 32 bits. (Probably unnecessary.) */
      UInt u32 = (UInt)imm;
      Int  s32 = (Int)u32;
      Long s64 = (Long)s32;
      imm = (ULong)s64;
   }

   if (!mode64) {
      /* sign-extendable from 32 bits
         addiu r_dst, r0, (imm >> 16) => lis r_dst, (imm >> 16)
         lui r_dst, (imm >> 16) */
      p = mkFormI(p, 15, 0, r_dst, (imm >> 16) & 0xFFFF);
      /* ori r_dst, r_dst, (imm & 0xFFFF) */
      p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
   } else {
      /* full 64bit immediate load: 6 (six!) insns. */
      vassert(mode64);
      /* lui load in upper half of low word */
      p = mkFormI(p, 15, 0, r_dst, (imm >> 48) & 0xFFFF);
      /* ori */
      p = mkFormI(p, 13, r_dst, r_dst, (imm >> 32) & 0xFFFF);
      /* shift */
      p = mkFormS(p, 0, r_dst, 0, r_dst, 16, 56);
      /* ori */
      p = mkFormI(p, 13, r_dst, r_dst, (imm >> 16) & 0xFFFF);
      /* shift */
      p = mkFormS(p, 0, r_dst, 0, r_dst, 16, 56);
      /* ori */
      p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
   }
   return p;
}

/* Checks whether the sequence of bytes at p was indeed created
   by mkLoadImm_EXACTLY2or6 with the given parameters. */
static Bool isLoadImm_EXACTLY2or6 ( UChar* p_to_check,
                                    UInt r_dst, ULong imm, Bool mode64 )
{
   vassert(r_dst < 0x20);
   Bool ret;
   if (!mode64) {
      /* In 32-bit mode, make sure the top 32 bits of imm are a sign
         extension of the bottom 32 bits.  (Probably unnecessary.) */
      UInt u32 = (UInt)imm;
      Int  s32 = (Int)u32;
      Long s64 = (Long)s32;
      imm = (ULong)s64;
   }

   if (!mode64) {
      UInt   expect[2] = { 0, 0 };
      UChar* p         = (UChar*)&expect[0];
      /* lui r_dst, (immi >> 16) */
      p = mkFormI(p, 15, 0, r_dst, (imm >> 16) & 0xFFFF);
      /* ori r_dst, r_dst, (imm & 0xFFFF) */
      p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
      vassert(p == (UChar*)&expect[2]);

      ret = fetch32(p_to_check + 0) == expect[0]
            && fetch32(p_to_check + 4) == expect[1];
   } else {
      UInt   expect[6] = { 0, 0, 0, 0, 0, 0};
      UChar* p         = (UChar*)&expect[0];
      /* lui load in upper half of low word */
      p = mkFormI(p, 15, 0, r_dst, (imm >> 48) & 0xFFFF);
      /* ori */
      p = mkFormI(p, 13, r_dst, r_dst, (imm >> 32) & 0xFFFF);
      /* shift */
      p = mkFormS(p, 0, r_dst, 0, r_dst, 16, 56);
      /* ori */
      p = mkFormI(p, 13, r_dst, r_dst, (imm >> 16) & 0xFFFF);
      /* shift */
      p = mkFormS(p, 0, r_dst, 0, r_dst, 16, 56);
      /* ori */
      p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
      vassert(p == (UChar*)&expect[6]);

      ret = fetch32(p_to_check + 0) == expect[0]
            && fetch32(p_to_check + 4) == expect[1]
            && fetch32(p_to_check + 8) == expect[2]
            && fetch32(p_to_check + 12) == expect[3]
            && fetch32(p_to_check + 16) == expect[4]
            && fetch32(p_to_check + 20) == expect[5];
   }
   return ret;
}

/* Generate a machine-word sized load or store. Simplified version of
   the Min_Load and Min_Store cases below.
   This will generate 32-bit load/store on MIPS32, and 64-bit load/store on
   MIPS64 platforms.
*/
static UChar* do_load_or_store_machine_word ( UChar* p, Bool isLoad, UInt reg,
                                              MIPSAMode* am, Bool mode64 )
{
   if (isLoad) { /* load */
      switch (am->tag) {
         case Mam_IR:
            if (mode64) {
               vassert(0 == (am->Mam.IR.index & 3));
            }
            p = doAMode_IR(p, mode64 ? 55 : 35, reg, am, mode64);
            break;
         case Mam_RR:
            /* we could handle this case, but we don't expect to ever
               need to. */
            vassert(0);
            break;
         default:
            vassert(0);
            break;
      }
   } else /* store */ {
      switch (am->tag) {
         case Mam_IR:
            if (mode64) {
               vassert(0 == (am->Mam.IR.index & 3));
            }
            p = doAMode_IR(p, mode64 ? 63 : 43, reg, am, mode64);
            break;
         case Mam_RR:
            /* we could handle this case, but we don't expect to ever
               need to. */
            vassert(0);
            break;
         default:
            vassert(0);
            break;
      }
   }
   return p;
}

/* Generate a 32-bit sized load or store. Simplified version of
   do_load_or_store_machine_word above. */
static UChar* do_load_or_store_word32 ( UChar* p, Bool isLoad, UInt reg,
                                        MIPSAMode* am, Bool mode64 )
{
   if (isLoad) { /* load */
      switch (am->tag) {
         case Mam_IR:
            if (mode64) {
               vassert(0 == (am->Mam.IR.index & 3));
            }
            p = doAMode_IR(p, 35, reg, am, mode64);
            break;
         case Mam_RR:
            /* we could handle this case, but we don't expect to ever
               need to. */
            vassert(0);
            break;
         default:
            vassert(0);
            break;
      }
   } else /* store */ {
      switch (am->tag) {
         case Mam_IR:
            if (mode64) {
               vassert(0 == (am->Mam.IR.index & 3));
            }
            p = doAMode_IR(p, 43, reg, am, mode64);
            break;
         case Mam_RR:
            /* we could handle this case, but we don't expect to ever
               need to. */
            vassert(0);
            break;
         default:
            vassert(0);
            break;
      }
   }
   return p;
}

/* Move r_dst to r_src */
static UChar *mkMoveReg(UChar * p, UInt r_dst, UInt r_src)
{
   vassert(r_dst < 0x20);
   vassert(r_src < 0x20);

   if (r_dst != r_src) {
      /* or r_dst, r_src, r_src */
      p = mkFormR(p, 0, r_src, r_src, r_dst, 0, 37);
   }
   return p;
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code.  If the emitted
   instruction was a profiler inc, set *is_profInc to True, else
   leave it unchanged. */
Int emit_MIPSInstr ( /*MB_MOD*/Bool* is_profInc,
                     UChar* buf, Int nbuf, const MIPSInstr* i,
                     Bool mode64,
                     VexEndness endness_host,
                     const void* disp_cp_chain_me_to_slowEP,
                     const void* disp_cp_chain_me_to_fastEP,
                     const void* disp_cp_xindir,
                     const void* disp_cp_xassisted )
{
   UChar *p = &buf[0];
   UChar *ptmp = p;
   vassert(nbuf >= 32);

   switch (i->tag) {
      case Min_LI:
         p = mkLoadImm(p, iregNo(i->Min.LI.dst, mode64), i->Min.LI.imm, mode64);
         goto done;

      case Min_Alu: {
         MIPSRH *srcR = i->Min.Alu.srcR;
         Bool immR = toBool(srcR->tag == Mrh_Imm);
         UInt r_dst = iregNo(i->Min.Alu.dst, mode64);
         UInt r_srcL = iregNo(i->Min.Alu.srcL, mode64);
         UInt r_srcR = immR ? (-1) /*bogus */ : iregNo(srcR->Mrh.Reg.reg,
                                                       mode64);
         switch (i->Min.Alu.op) {
            /* Malu_ADD, Malu_SUB, Malu_AND, Malu_OR, Malu_NOR, Malu_XOR, Malu_SLT */
            case Malu_ADD:
               if (immR) {
                  vassert(srcR->Mrh.Imm.syned);
                  /* addiu */
                  p = mkFormI(p, 9, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  /* addu */
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 33);
               }
               break;
            case Malu_SUB:
               if (immR) {
                  /* addiu , but with negated imm */
                  vassert(srcR->Mrh.Imm.syned);
                  vassert(srcR->Mrh.Imm.imm16 != 0x8000);
                  p = mkFormI(p, 9, r_srcL, r_dst, (-srcR->Mrh.Imm.imm16));
               } else {
                  /* subu */
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 35);
               }
               break;
            case Malu_AND:
               if (immR) {
                  /* andi */
                  vassert(!srcR->Mrh.Imm.syned);
                  p = mkFormI(p, 12, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  /* and */
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 36);
               }
               break;
            case Malu_OR:
               if (immR) {
                  /* ori */
                  vassert(!srcR->Mrh.Imm.syned);
                  p = mkFormI(p, 13, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  /* or */
                  if (r_srcL == 33)
                     /* MFHI */
                     p = mkFormR(p, 0, 0, 0, r_dst, 0, 16);
                  else if (r_srcL == 34)
                     /* MFLO */
                     p = mkFormR(p, 0, 0, 0, r_dst, 0, 18);
                  else if (r_dst == 33)
                     /* MTHI */
                     p = mkFormR(p, 0, r_srcL, 0, 0, 0, 17);
                  else if (r_dst == 34)
                     /* MTLO */
                     p = mkFormR(p, 0, r_srcL, 0, 0, 0, 19);
                  else
                     p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 37);
               }
               break;
            case Malu_NOR:
               /* nor */
               vassert(!immR);
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 39);
               break;
            case Malu_XOR:
               if (immR) {
                  /* xori */
                  vassert(!srcR->Mrh.Imm.syned);
                  p = mkFormI(p, 14, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  /* xor */
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 38);
               }
               break;
            case Malu_DADD:
               if (immR) {
                  vassert(srcR->Mrh.Imm.syned);
                  vassert(srcR->Mrh.Imm.imm16 != 0x8000);
                  p = mkFormI(p, 25, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 45);
               }
               break;
            case Malu_DSUB:
               if (immR) {
                  p = mkFormI(p, 25, r_srcL, r_dst, (-srcR->Mrh.Imm.imm16));
               } else {
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 47);
               }
               break;
            case Malu_SLT:
               if (immR) {
                  goto bad;
               } else {
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 42);
               }
               break;

            default:
               goto bad;
         }
         goto done;
      }

      case Min_Shft: {
         MIPSRH *srcR = i->Min.Shft.srcR;
         Bool sz32 = i->Min.Shft.sz32;
         Bool immR = toBool(srcR->tag == Mrh_Imm);
         UInt r_dst = iregNo(i->Min.Shft.dst, mode64);
         UInt r_srcL = iregNo(i->Min.Shft.srcL, mode64);
         UInt r_srcR = immR ? (-1) /*bogus */ : iregNo(srcR->Mrh.Reg.reg,
                                                       mode64);
         if (!mode64)
            vassert(sz32);
         switch (i->Min.Shft.op) {
            case Mshft_SLL:
               if (sz32) {
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert(n >= 0 && n <= 32);
                     p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 0);
                  } else {
                     /* shift variable */
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 4);
                  }
               } else {
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert((n >= 0 && n < 32) || (n > 31 && n < 64));
                     if (n >= 0 && n < 32) {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 56);
                     } else {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n - 32, 60);
                     }
                  } else {
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 20);
                  }
               }
               break;

            case Mshft_SRL:
               if (sz32) {
                  /* SRL, SRLV */
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert(n >= 0 && n < 32);
                     p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 2);
                  } else {
                     /* shift variable */
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 6);
                  }
               } else {
                  /* DSRL, DSRL32, DSRLV */
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert((n >= 0 && n < 32) || (n > 31 && n < 64));
                     if (n >= 0 && n < 32) {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 58);
                     } else {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n - 32, 62);
                     }
                  } else {
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 22);
                  }
               }
               break;

            case Mshft_SRA:
               if (sz32) {
                  /* SRA, SRAV */
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert(n >= 0 && n < 32);
                     p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 3);
                  } else {
                     /* shift variable */
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 7);
                  }
               } else {
                  /* DSRA, DSRA32, DSRAV */
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert((n >= 0 && n < 32) || (n > 31 && n < 64));
                     if (n >= 0 && n < 32) {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 59);
                     } else {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n - 32, 63);
                     }
                  } else {
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 23);
                  }
               }
               break;

            default:
               goto bad;
         }

         goto done;
      }

      case Min_Unary: {
         UInt r_dst = iregNo(i->Min.Unary.dst, mode64);
         UInt r_src = iregNo(i->Min.Unary.src, mode64);

         switch (i->Min.Unary.op) {
            /* Mun_CLO, Mun_CLZ, Mun_NOP, Mun_DCLO, Mun_DCLZ */
            case Mun_CLO:  /* clo */
               p = mkFormR(p, 28, r_src, r_dst , r_dst, 0, 33);
               break;
            case Mun_CLZ:  /* clz */
               p = mkFormR(p, 28, r_src, r_dst , r_dst, 0, 32);
               break;
            case Mun_NOP:  /* nop (sll r0,r0,0) */
               p = mkFormR(p, 0, 0, 0, 0, 0, 0);
               break;
            case Mun_DCLO:  /* clo */
               p = mkFormR(p, 28, r_src, r_dst , r_dst, 0, 37);
               break;
            case Mun_DCLZ:  /* clz */
               p = mkFormR(p, 28, r_src, r_dst , r_dst, 0, 36);
               break;
         }
         goto done;
      }

      case Min_Cmp: {
         UInt r_srcL = iregNo(i->Min.Cmp.srcL, mode64);
         UInt r_srcR = iregNo(i->Min.Cmp.srcR, mode64);
         UInt r_dst = iregNo(i->Min.Cmp.dst, mode64);

         switch (i->Min.Cmp.cond) {
            case MIPScc_EQ:
               /* xor r_dst, r_srcL, r_srcR
                  sltiu r_dst, r_dst, 1 */
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 38);
               p = mkFormI(p, 11, r_dst, r_dst, 1);
               break;
            case MIPScc_NE:
               /* xor r_dst, r_srcL, r_srcR
                  sltu r_dst, zero, r_dst */
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 38);
               p = mkFormR(p, 0, 0, r_dst, r_dst, 0, 43);
               break;
            case MIPScc_LT:
               /* slt r_dst, r_srcL, r_srcR */
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 42);
               break;
            case MIPScc_LO:
               /* sltu r_dst, r_srcL, r_srcR */
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 43);
               break;
            case MIPScc_LE:
               /* slt r_dst, r_srcR, r_srcL
                  xori r_dst, r_dst, 1 */
               p = mkFormR(p, 0, r_srcR, r_srcL, r_dst, 0, 42);
               p = mkFormI(p, 14, r_dst, r_dst, 1);
               break;
            case MIPScc_LS:
               /* sltu r_dst, rsrcR, r_srcL
                  xori r_dsr, r_dst, 1 */
               p = mkFormR(p, 0, r_srcR, r_srcL, r_dst, 0, 43);
               p = mkFormI(p, 14, r_dst, r_dst, 1);
               break;
            default:
               goto bad;
         }
         goto done;
      }

      case Min_Mul: {
         Bool syned = i->Min.Mul.syned;
         Bool widening = i->Min.Mul.widening;
         Bool sz32 = i->Min.Mul.sz32;
         UInt r_srcL = iregNo(i->Min.Mul.srcL, mode64);
         UInt r_srcR = iregNo(i->Min.Mul.srcR, mode64);
         UInt r_dst = iregNo(i->Min.Mul.dst, mode64);
         if (widening) {
            if (sz32) {
               if (syned)
                  /* mult */
                  p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 24);
               else
                  /* multu */
                  p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 25);
            } else {
               if (syned)  /* DMULT  r_dst,r_srcL,r_srcR */
                  p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 28);
               else  /* DMULTU r_dst,r_srcL,r_srcR */
                  p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 29);
            }
         } else {
            if (sz32)
               /* mul */
               p = mkFormR(p, 28, r_srcL, r_srcR, r_dst, 0, 2);
            else if (mode64 && !sz32)
               p = mkFormR(p, 28, r_srcL, r_srcR, r_dst, 0, 2);
            else
               goto bad;
         }
         goto done;
      }

      case Min_Macc: {
         Bool syned = i->Min.Macc.syned;
         UInt r_srcL = iregNo(i->Min.Macc.srcL, mode64);
         UInt r_srcR = iregNo(i->Min.Macc.srcR, mode64);

         if (syned) {
            switch (i->Min.Macc.op) {
               case Macc_ADD:
                  /* madd */
                  p = mkFormR(p, 28, r_srcL, r_srcR, 0, 0, 0);
                  break;
               case Macc_SUB:
                  /* msub */
                  p = mkFormR(p, 28, r_srcL, r_srcR, 0, 0,
                         4);
                  break;
               default:
                  goto bad;
            }
         } else {
            switch (i->Min.Macc.op) {
               case Macc_ADD:
                  /* maddu */
                  p = mkFormR(p, 28, r_srcL, r_srcR, 0, 0,
                         1);
                  break;
               case Macc_SUB:
                  /* msubu */
                  p = mkFormR(p, 28, r_srcL, r_srcR, 0, 0,
                         5);
                  break;
               default:
                  goto bad;
            }
         }

         goto done;
      }

      case Min_Div: {
         Bool syned = i->Min.Div.syned;
         Bool sz32 = i->Min.Div.sz32;
         UInt r_srcL = iregNo(i->Min.Div.srcL, mode64);
         UInt r_srcR = iregNo(i->Min.Div.srcR, mode64);
         if (sz32) {
            if (syned) {
               /* div */
               p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 26);
            } else
               /* divu */
               p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 27);
            goto done;
         } else {
            if (syned) {
               /* ddiv */
               p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 30);
            } else
               /* ddivu */
               p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 31);
            goto done;
         }
      }

      case Min_Mthi: {
         UInt r_src = iregNo(i->Min.MtHL.src, mode64);
         p = mkFormR(p, 0, r_src, 0, 0, 0, 17);
         goto done;
      }

      case Min_Mtlo: {
         UInt r_src = iregNo(i->Min.MtHL.src, mode64);
         p = mkFormR(p, 0, r_src, 0, 0, 0, 19);
         goto done;
      }

      case Min_Mfhi: {
         UInt r_dst = iregNo(i->Min.MfHL.dst, mode64);
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 16);
         goto done;
      }

      case Min_Mflo: {
         UInt r_dst = iregNo(i->Min.MfHL.dst, mode64);
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 18);
         goto done;
      }

      case Min_MtFCSR: {
         UInt r_src = iregNo(i->Min.MtFCSR.src, mode64);
         /* ctc1 */
         p = mkFormR(p, 17, 6, r_src, 31, 0, 0);
         goto done;
      }

      case Min_MfFCSR: {
         UInt r_dst = iregNo(i->Min.MfFCSR.dst, mode64);
         /* cfc1 */
         p = mkFormR(p, 17, 2, r_dst, 31, 0, 0);
         goto done;
      }

      case Min_Call: {
         if (i->Min.Call.cond != MIPScc_AL
             && i->Min.Call.rloc.pri != RLPri_None) {
            /* The call might not happen (it isn't unconditional) and
               it returns a result.  In this case we will need to
               generate a control flow diamond to put 0x555..555 in
               the return register(s) in the case where the call
               doesn't happen.  If this ever becomes necessary, maybe
               copy code from the ARM equivalent.  Until that day,
               just give up. */
            goto bad;
         }
         MIPSCondCode cond = i->Min.Call.cond;
         UInt r_dst = 25;  /* using %r25 as address temporary -
                              see getRegUsage_MIPSInstr */

         /* jump over the following insns if condition does not hold */
         if (cond != MIPScc_AL) {
            /* jmp fwds if !condition */
            /* don't know how many bytes to jump over yet...
               make space for a jump instruction + nop!!! and fill in later. */
            ptmp = p;  /* fill in this bit later */
            p += 8;    /* p += 8 */
         }

         if (!mode64) {
            /* addiu $29, $29, -16 */
            p = mkFormI(p, 9, 29, 29, 0xFFF0);
         }

         /* load target to r_dst; p += 4|8 */
         p = mkLoadImm(p, r_dst, i->Min.Call.target, mode64);

         /* jalr r_dst */
         p = mkFormR(p, 0, r_dst, 0, 31, 0, 9);  /* p += 4 */
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);       /* p += 4 */

         if (!mode64) {
            /* addiu $29, $29, 16 */
            p = mkFormI(p, 9, 29, 29, 0x0010);
         }

         /* Fix up the conditional jump, if there was one. */
         if (cond != MIPScc_AL) {
            UInt r_src = iregNo(i->Min.Call.src, mode64);
            Int delta = p - ptmp;

            vassert(delta >= 20 && delta <= 32);
            /* blez r_src, delta/4-1
               nop */
            ptmp = mkFormI(ptmp, 6, r_src, 0, delta / 4 - 1);
            mkFormR(ptmp, 0, 0, 0, 0, 0, 0);
         }
         goto done;
      }

      case Min_XDirect: {
         /* NB: what goes on here has to be very closely coordinated
            with the chainXDirect_MIPS and unchainXDirect_MIPS below. */
         /* We're generating chain-me requests here, so we need to be
            sure this is actually allowed -- no-redir translations
            can't use chain-me's.  Hence: */
         vassert(disp_cp_chain_me_to_slowEP != NULL);
         vassert(disp_cp_chain_me_to_fastEP != NULL);

         /* Use ptmp for backpatching conditional jumps. */
         ptmp = NULL;

         /* First off, if this is conditional, create a conditional
            jump over the rest of it.  Or at least, leave a space for
            it that we will shortly fill in. */
         if (i->Min.XDirect.cond != MIPScc_AL) {
            vassert(i->Min.XDirect.cond != MIPScc_NV);
            ptmp = p;
            p += 12;
         }

         /* Update the guest PC. */
         /* move r9, dstGA */
         /* sw/sd r9, amPC */
         p = mkLoadImm_EXACTLY2or6(p, /*r*/ 9, (ULong)i->Min.XDirect.dstGA,
                                   mode64);
         p = do_load_or_store_machine_word(p, False /*!isLoad*/ , /*r*/ 9,
                                           i->Min.XDirect.amPC, mode64);

         /* --- FIRST PATCHABLE BYTE follows --- */
         /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're
            calling to) backs up the return address, so as to find the
            address of the first patchable byte.  So: don't change the
            number of instructions (3) below. */
         /* move r9, VG_(disp_cp_chain_me_to_{slowEP,fastEP}) */
         /* jr  r9  */
         const void* disp_cp_chain_me
                  = i->Min.XDirect.toFastEP ? disp_cp_chain_me_to_fastEP
                                              : disp_cp_chain_me_to_slowEP;
         p = mkLoadImm_EXACTLY2or6(p, /*r*/ 9,
                                   (Addr)disp_cp_chain_me, mode64);
         /* jalr $9 */
         /* nop */
         p = mkFormR(p, 0, 9, 0, 31, 0, 9);  /* p += 4 */
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   /* p += 4 */
         /* --- END of PATCHABLE BYTES --- */

         /* Fix up the conditional jump, if there was one. */
         if (i->Min.XDirect.cond != MIPScc_AL) {
            Int delta = p - ptmp;
            delta = delta / 4 - 3;
            vassert(delta > 0 && delta < 40);

            /* lw $9, COND_OFFSET(GuestSP)
               beq $9, $0, 2
               nop */
            ptmp = mkFormI(ptmp, 35, GuestSP, 9, COND_OFFSET(mode64));
            ptmp = mkFormI(ptmp, 4, 0, 9, (delta));
            mkFormR(ptmp, 0, 0, 0, 0, 0, 0);
         }
         goto done;
      }

      case Min_XIndir: {
         /* We're generating transfers that could lead indirectly to a
            chain-me, so we need to be sure this is actually allowed --
            no-redir translations are not allowed to reach normal
            translations without going through the scheduler.  That means
            no XDirects or XIndirs out from no-redir translations.
            Hence: */
         vassert(disp_cp_xindir != NULL);

         /* Use ptmp for backpatching conditional jumps. */
         ptmp = NULL;

         /* First off, if this is conditional, create a conditional
            jump over the rest of it. */
         if (i->Min.XIndir.cond != MIPScc_AL) {
            vassert(i->Min.XIndir.cond != MIPScc_NV);
            ptmp = p;
            p += 12;
         }

         /* Update the guest PC. */
         /* sw/sd r-dstGA, amPC */
         p = do_load_or_store_machine_word(p, False /*!isLoad*/ ,
                                           iregNo(i->Min.XIndir.dstGA, mode64),
                                           i->Min.XIndir.amPC, mode64);

         /* move r9, VG_(disp_cp_xindir) */
         /* jalr   r9 */
         /* nop */
         p = mkLoadImm_EXACTLY2or6(p, /*r*/ 9,
                                   (Addr)disp_cp_xindir, mode64);
         p = mkFormR(p, 0, 9, 0, 31, 0, 9);  /* p += 4 */
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   /* p += 4 */

         /* Fix up the conditional jump, if there was one. */
         if (i->Min.XIndir.cond != MIPScc_AL) {
            Int delta = p - ptmp;
            delta = delta / 4 - 3;
            vassert(delta > 0 && delta < 40);

            /* lw $9, COND_OFFSET($GuestSP)
               beq $9, $0, 2
               nop */
            ptmp = mkFormI(ptmp, 35, GuestSP, 9, COND_OFFSET(mode64));
            ptmp = mkFormI(ptmp, 4, 0, 9, (delta));
            mkFormR(ptmp, 0, 0, 0, 0, 0, 0);
         }
         goto done;
      }

      case Min_XAssisted: {
         /* First off, if this is conditional, create a conditional jump
            over the rest of it.  Or at least, leave a space for it that
            we will shortly fill in. */
         ptmp = NULL;
         if (i->Min.XAssisted.cond != MIPScc_AL) {
            vassert(i->Min.XAssisted.cond != MIPScc_NV);
            ptmp = p;
            p += 12;
         }

         /* Update the guest PC. */
         /* sw/sd r-dstGA, amPC */
         p = do_load_or_store_machine_word(p, False /*!isLoad*/ ,
                                           iregNo(i->Min.XIndir.dstGA, mode64),
                                           i->Min.XIndir.amPC, mode64);

         /* imm32/64 r31, $magic_number */
         UInt trcval = 0;
         switch (i->Min.XAssisted.jk) {
            case Ijk_ClientReq:     trcval = VEX_TRC_JMP_CLIENTREQ;     break;
            case Ijk_Sys_syscall:   trcval = VEX_TRC_JMP_SYS_SYSCALL;   break;
            /* case Ijk_Sys_int128: trcval = VEX_TRC_JMP_SYS_INT128;   break; */
            case Ijk_Yield:         trcval = VEX_TRC_JMP_YIELD;       break;
            case Ijk_EmWarn:        trcval = VEX_TRC_JMP_EMWARN;        break;
            case Ijk_EmFail:        trcval = VEX_TRC_JMP_EMFAIL;        break;
            /* case Ijk_MapFail:   trcval = VEX_TRC_JMP_MAPFAIL;       break; */
            case Ijk_NoDecode:      trcval = VEX_TRC_JMP_NODECODE;      break;
            case Ijk_InvalICache:   trcval = VEX_TRC_JMP_INVALICACHE;   break;
            case Ijk_NoRedir:       trcval = VEX_TRC_JMP_NOREDIR;       break;
            case Ijk_SigILL:        trcval = VEX_TRC_JMP_SIGILL;        break;
            case Ijk_SigTRAP:       trcval = VEX_TRC_JMP_SIGTRAP;       break;
            /* case Ijk_SigSEGV:   trcval = VEX_TRC_JMP_SIGSEGV;       break; */
            case Ijk_SigBUS:        trcval = VEX_TRC_JMP_SIGBUS;        break;
            case Ijk_SigFPE_IntDiv: trcval = VEX_TRC_JMP_SIGFPE_INTDIV; break;
            case Ijk_SigFPE_IntOvf: trcval = VEX_TRC_JMP_SIGFPE_INTOVF; break;
            case Ijk_Boring:        trcval = VEX_TRC_JMP_BORING;        break;
            /* We don't expect to see the following being assisted.
               case Ijk_Ret:
               case Ijk_Call:
               fallthrough */
            default:
               ppIRJumpKind(i->Min.XAssisted.jk);
               vpanic("emit_MIPSInstr.Min_XAssisted: unexpected jump kind");
         }
         vassert(trcval != 0);
         p = mkLoadImm_EXACTLY2or6(p, /*r*/ GuestSP, trcval, mode64);

         /* move r9, VG_(disp_cp_xassisted) */
         p = mkLoadImm_EXACTLY2or6(p, /*r*/ 9,
                                   (ULong)(Addr)disp_cp_xassisted, mode64);
         /* jalr $9
             nop */
         p = mkFormR(p, 0, 9, 0, 31, 0, 9);  /* p += 4 */
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   /* p += 4 */

         /* Fix up the conditional jump, if there was one. */
         if (i->Min.XAssisted.cond != MIPScc_AL) {
            Int delta = p - ptmp;
            delta = delta / 4 - 3;
            vassert(delta > 0 && delta < 40);

            /* lw $9, COND_OFFSET($GuestSP)
               beq $9, $0, 2
               nop */
            ptmp = mkFormI(ptmp, 35, GuestSP, 9, COND_OFFSET(mode64));
            ptmp = mkFormI(ptmp, 4, 0, 9, (delta));
            mkFormR(ptmp, 0, 0, 0, 0, 0, 0);
         }
         goto done;
      }

      case Min_Load: {
         MIPSAMode *am_addr = i->Min.Load.src;
         if (am_addr->tag == Mam_IR) {
            UInt r_dst = iregNo(i->Min.Load.dst, mode64);
            UInt opc, sz = i->Min.Load.sz;
            if (mode64 && (sz == 4 || sz == 8)) {
               /* should be guaranteed to us by iselWordExpr_AMode */
               vassert(0 == (am_addr->Mam.IR.index & 3));
            }
            switch (sz) {
               case 1:
                  opc = 32;
                  break;
               case 2:
                  opc = 33;
                  break;
               case 4:
                  opc = 35;
                  break;
               case 8:
                  opc = 55;
                  vassert(mode64);
                  break;
               default:
                  goto bad;
            }

            p = doAMode_IR(p, opc, r_dst, am_addr, mode64);
            goto done;
         } else if (am_addr->tag == Mam_RR) {
            UInt r_dst = iregNo(i->Min.Load.dst, mode64);
            UInt opc, sz = i->Min.Load.sz;

            switch (sz) {
               case 1:
                  opc = 32;
                  break;
               case 2:
                  opc = 33;
                  break;
               case 4:
                  opc = 35;
                  break;
               case 8:
                  opc = 55;
                  vassert(mode64);
                  break;
               default:
                  goto bad;
            }

            p = doAMode_RR(p, opc, r_dst, am_addr, mode64);
            goto done;
         }
         break;
      }

      case Min_Store: {
         MIPSAMode *am_addr = i->Min.Store.dst;
         if (am_addr->tag == Mam_IR) {
            UInt r_src = iregNo(i->Min.Store.src, mode64);
            UInt opc, sz = i->Min.Store.sz;
            if (mode64 && (sz == 4 || sz == 8)) {
               /* should be guaranteed to us by iselWordExpr_AMode */
               vassert(0 == (am_addr->Mam.IR.index & 3));
            }
            switch (sz) {
               case 1:
                  opc = 40;
                  break;
               case 2:
                  opc = 41;
                  break;
               case 4:
                  opc = 43;
                  break;
               case 8:
                  vassert(mode64);
                  opc = 63;
                  break;
               default:
                  goto bad;
            }

            p = doAMode_IR(p, opc, r_src, am_addr, mode64);
            goto done;
         } else if (am_addr->tag == Mam_RR) {
            UInt r_src = iregNo(i->Min.Store.src, mode64);
            UInt opc, sz = i->Min.Store.sz;

            switch (sz) {
               case 1:
                  opc = 40;
                  break;
               case 2:
                  opc = 41;
                  break;
               case 4:
                  opc = 43;
                  break;
               case 8:
                  vassert(mode64);
                  opc = 63;
                  break;
               default:
                  goto bad;
            }

            p = doAMode_RR(p, opc, r_src, am_addr, mode64);
            goto done;
         }
         break;
      }
      case Min_LoadL: {
         MIPSAMode *am_addr = i->Min.LoadL.src;
         UInt r_src = iregNo(am_addr->Mam.IR.base, mode64);
         UInt idx = am_addr->Mam.IR.index;
         UInt r_dst = iregNo(i->Min.LoadL.dst, mode64);

         if (i->Min.LoadL.sz == 4)
            p = mkFormI(p, 0x30, r_src, r_dst, idx);
         else
            p = mkFormI(p, 0x34, r_src, r_dst, idx);
         goto done;
      }
      case Min_StoreC: {
         MIPSAMode *am_addr = i->Min.StoreC.dst;
         UInt r_src = iregNo(i->Min.StoreC.src, mode64);
         UInt idx = am_addr->Mam.IR.index;
         UInt r_dst = iregNo(am_addr->Mam.IR.base, mode64);

         if (i->Min.StoreC.sz == 4)
            p = mkFormI(p, 0x38, r_dst, r_src, idx);
         else
            p = mkFormI(p, 0x3C, r_dst, r_src, idx);
         goto done;
      }
      case Min_Cas: {
         if (i->Min.Cas.sz != 8 && i->Min.Cas.sz != 4)
            goto bad;
         UInt old  = iregNo(i->Min.Cas.old, mode64);
         UInt addr = iregNo(i->Min.Cas.addr, mode64);
         UInt expd = iregNo(i->Min.Cas.expd, mode64);
         UInt data = iregNo(i->Min.Cas.data, mode64);
         Bool sz8  = toBool(i->Min.Cas.sz == 8);

         /*
          * ll(d)    old,  0(addr)
          * bne      old,  expd, end
          * nop
          * (d)addiu old,  old,  1
          * sc(d)    data, 0(addr)
          * movn     old,  expd, data
          * end:
          */
         // ll(d) old, 0(addr)
         p = mkFormI(p, sz8 ? 0x34 : 0x30, addr, old, 0);
         // bne  old,  expd, end
         p = mkFormI(p, 5, old, expd, 4);
         // nop
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);
         // (d)addiu old,  old,  1
         p = mkFormI(p, sz8 ? 25 : 9, old, old, 1);
         // sc(d)  data, 0(addr)
         p = mkFormI(p, sz8 ? 0x3C : 0x38, addr, data, 0);
         // movn old,  expd, data
         p = mkFormR(p, 0, expd, data, old, 0, 0xb);

         goto done;
      }
      case Min_RdWrLR: {
         UInt reg = iregNo(i->Min.RdWrLR.gpr, mode64);
         Bool wrLR = i->Min.RdWrLR.wrLR;
         if (wrLR)
            p = mkMoveReg(p, 31, reg);
         else
            p = mkMoveReg(p, reg, 31);
         goto done;
      }

      /* Floating point */
      case Min_FpLdSt: {
         MIPSAMode *am_addr = i->Min.FpLdSt.addr;
         UChar sz = i->Min.FpLdSt.sz;
         vassert(sz == 4 || sz == 8);
         if (sz == 4) {
            UInt f_reg = fregNo(i->Min.FpLdSt.reg, mode64);
            if (i->Min.FpLdSt.isLoad) {
               if (am_addr->tag == Mam_IR)
                  p = doAMode_IR(p, 0x31, f_reg, am_addr, mode64);
               else if (am_addr->tag == Mam_RR)
                  p = doAMode_RR(p, 0x31, f_reg, am_addr, mode64);
            } else {
               if (am_addr->tag == Mam_IR)
                  p = doAMode_IR(p, 0x39, f_reg, am_addr, mode64);
               else if (am_addr->tag == Mam_RR)
                  p = doAMode_RR(p, 0x39, f_reg, am_addr, mode64);
            }
         } else if (sz == 8) {
            UInt f_reg = dregNo(i->Min.FpLdSt.reg);
            if (i->Min.FpLdSt.isLoad) {
               if (am_addr->tag == Mam_IR) {
                  p = doAMode_IR(p, 0x35, f_reg, am_addr, mode64);
               } else if (am_addr->tag == Mam_RR) {
                  p = doAMode_RR(p, 0x35, f_reg, am_addr, mode64);
               }
            } else {
               if (am_addr->tag == Mam_IR) {
                  p = doAMode_IR(p, 0x3d, f_reg, am_addr, mode64);
               } else if (am_addr->tag == Mam_RR) {
                  p = doAMode_RR(p, 0x3d, f_reg, am_addr, mode64);
               }
            }
         }
         goto done;
      }

      case Min_FpUnary: {
         switch (i->Min.FpUnary.op) {
            case Mfp_MOVS: {  /* FP move */
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x6);
               break;
            }
            case Mfp_MOVD: {  /* FP move */
                UInt fr_dst = dregNo(i->Min.FpUnary.dst);
                UInt fr_src = dregNo(i->Min.FpUnary.src);
                p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x6);
                break;
             }
            case Mfp_ABSS: {  /* ABS.S */
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x5);
               break;
            }
            case Mfp_ABSD: {  /* ABS.D */
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = dregNo(i->Min.FpUnary.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x5);
               break;
            }
            case Mfp_NEGS: {  /* NEG.S */
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x7);
               break;
            }
            case Mfp_NEGD: {  /* NEG.D */
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = dregNo(i->Min.FpUnary.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x7);
               break;
            }
            case Mfp_SQRTS: {  /* SQRT.S */
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x04);
               break;
            }
            case Mfp_SQRTD: {  /* SQRT.D */
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = dregNo(i->Min.FpUnary.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x04);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }

      case Min_FpBinary: {
         switch (i->Min.FpBinary.op) {
            case Mfp_ADDS: {
               UInt fr_dst = fregNo(i->Min.FpBinary.dst, mode64);
               UInt fr_srcL = fregNo(i->Min.FpBinary.srcL, mode64);
               UInt fr_srcR = fregNo(i->Min.FpBinary.srcR, mode64);
               p = mkFormR(p, 0x11, 0x10, fr_srcR, fr_srcL, fr_dst, 0);
               break;
            }
            case Mfp_SUBS: {
               UInt fr_dst = fregNo(i->Min.FpBinary.dst, mode64);
               UInt fr_srcL = fregNo(i->Min.FpBinary.srcL, mode64);
               UInt fr_srcR = fregNo(i->Min.FpBinary.srcR, mode64);
               p = mkFormR(p, 0x11, 0x10, fr_srcR, fr_srcL, fr_dst, 1);
               break;
            }
            case Mfp_MULS: {
               UInt fr_dst = fregNo(i->Min.FpBinary.dst, mode64);
               UInt fr_srcL = fregNo(i->Min.FpBinary.srcL, mode64);
               UInt fr_srcR = fregNo(i->Min.FpBinary.srcR, mode64);
               p = mkFormR(p, 0x11, 0x10, fr_srcR, fr_srcL, fr_dst, 2);
               break;
            }
            case Mfp_DIVS: {
               UInt fr_dst = fregNo(i->Min.FpBinary.dst, mode64);
               UInt fr_srcL = fregNo(i->Min.FpBinary.srcL, mode64);
               UInt fr_srcR = fregNo(i->Min.FpBinary.srcR, mode64);
               p = mkFormR(p, 0x11, 0x10, fr_srcR, fr_srcL, fr_dst, 3);
               break;
            }
            case Mfp_ADDD: {
               UInt fr_dst = dregNo(i->Min.FpBinary.dst);
               UInt fr_srcL = dregNo(i->Min.FpBinary.srcL);
               UInt fr_srcR = dregNo(i->Min.FpBinary.srcR);
               p = mkFormR(p, 0x11, 0x11, fr_srcR, fr_srcL, fr_dst, 0);
               break;
            }
            case Mfp_SUBD: {
               UInt fr_dst = dregNo(i->Min.FpBinary.dst);
               UInt fr_srcL = dregNo(i->Min.FpBinary.srcL);
               UInt fr_srcR = dregNo(i->Min.FpBinary.srcR);
               p = mkFormR(p, 0x11, 0x11, fr_srcR, fr_srcL, fr_dst, 1);
               break;
            }
            case Mfp_MULD: {
               UInt fr_dst = dregNo(i->Min.FpBinary.dst);
               UInt fr_srcL = dregNo(i->Min.FpBinary.srcL);
               UInt fr_srcR = dregNo(i->Min.FpBinary.srcR);
               p = mkFormR(p, 0x11, 0x11, fr_srcR, fr_srcL, fr_dst, 2);
               break;
            }
            case Mfp_DIVD: {
               UInt fr_dst = dregNo(i->Min.FpBinary.dst);
               UInt fr_srcL = dregNo(i->Min.FpBinary.srcL);
               UInt fr_srcR = dregNo(i->Min.FpBinary.srcR);
               p = mkFormR(p, 0x11, 0x11, fr_srcR, fr_srcL, fr_dst, 3);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }

      case Min_FpTernary: {
         switch (i->Min.FpTernary.op) {
            case Mfp_MADDS: {
               UInt fr_dst = fregNo(i->Min.FpTernary.dst, mode64);
               UInt fr_src1 = fregNo(i->Min.FpTernary.src1, mode64);
               UInt fr_src2 = fregNo(i->Min.FpTernary.src2, mode64);
               UInt fr_src3 = fregNo(i->Min.FpTernary.src3, mode64);
               p = mkFormR(p, 0x13, fr_src1, fr_src2, fr_src3, fr_dst, 0x20);
               break;
            }
            case Mfp_MADDD: {
               UInt fr_dst = dregNo(i->Min.FpTernary.dst);
               UInt fr_src1 = dregNo(i->Min.FpTernary.src1);
               UInt fr_src2 = dregNo(i->Min.FpTernary.src2);
               UInt fr_src3 = dregNo(i->Min.FpTernary.src3);
               p = mkFormR(p, 0x13, fr_src1, fr_src2, fr_src3, fr_dst, 0x21);
               break;
            }
            case Mfp_MSUBS: {
               UInt fr_dst = fregNo(i->Min.FpTernary.dst, mode64);
               UInt fr_src1 = fregNo(i->Min.FpTernary.src1, mode64);
               UInt fr_src2 = fregNo(i->Min.FpTernary.src2, mode64);
               UInt fr_src3 = fregNo(i->Min.FpTernary.src3, mode64);
               p = mkFormR(p, 0x13, fr_src1, fr_src2, fr_src3, fr_dst, 0x28);
               break;
            }
            case Mfp_MSUBD: {
               UInt fr_dst = dregNo(i->Min.FpTernary.dst);
               UInt fr_src1 = dregNo(i->Min.FpTernary.src1);
               UInt fr_src2 = dregNo(i->Min.FpTernary.src2);
               UInt fr_src3 = dregNo(i->Min.FpTernary.src3);
               p = mkFormR(p, 0x13, fr_src1, fr_src2, fr_src3, fr_dst, 0x29);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }

      case Min_FpConvert: {
         switch (i->Min.FpConvert.op) {
            UInt fr_dst, fr_src;
            case Mfp_CVTSD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x20);
               break;
            case Mfp_CVTSW:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x14, 0, fr_src, fr_dst, 0x20);
               break;
            case Mfp_CVTWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x24);
               break;
            case Mfp_CVTWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x24);
               break;
            case Mfp_CVTDW:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x14, 0, fr_src, fr_dst, 0x21);
               break;
            case Mfp_CVTDL:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x15, 0, fr_src, fr_dst, 0x21);
               break;
            case Mfp_CVTDS:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x21);
               break;
            case Mfp_CVTSL:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x15, 0, fr_src, fr_dst, 0x20);
               break;
            case Mfp_CVTLS:
               if (mode64) {
                  fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
                  fr_src = dregNo(i->Min.FpConvert.src);
               } else {
                  fr_dst = dregNo(i->Min.FpConvert.dst);
                  fr_src = fregNo(i->Min.FpConvert.src, mode64);
               }
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x25);
               break;
            case Mfp_CVTLD:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x25);
               break;
            case Mfp_TRUWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0D);
               break;
            case Mfp_TRUWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0D);
               break;
            case Mfp_TRULS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x09);
               break;
            case Mfp_TRULD:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x09);
               break;
            case Mfp_CEILWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0E);
               break;
            case Mfp_CEILWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0E);
               break;
            case Mfp_CEILLS:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0A);
               break;
            case Mfp_CEILLD:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0A);
               break;
            case Mfp_ROUNDWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0C);
               break;
            case Mfp_ROUNDWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0C);
               break;
            case Mfp_ROUNDLD:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x08);
               break;
            case Mfp_FLOORWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0F);
               break;
            case Mfp_FLOORWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0F);
               break;
            case Mfp_FLOORLD:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0B);
               break;

            default:
               goto bad;
         }
         goto done;
      }

      case Min_FpCompare: {
         UInt r_dst   = iregNo(i->Min.FpCompare.dst, mode64);
         UInt fr_srcL = dregNo(i->Min.FpCompare.srcL);
         UInt fr_srcR = dregNo(i->Min.FpCompare.srcR);

         UInt op;
         switch (i->Min.FpConvert.op) {
            case Mfp_CMP_UN:
               op = 1;
               break;
            case Mfp_CMP_EQ:
               op = 2;
               break;
            case Mfp_CMP_LT:
               op = 12;
               break;
            case Mfp_CMP_NGT:
               op = 15;
               break;               
            default:
               goto bad;
         }
         /* c.cond.d fr_srcL, fr_srcR
            cfc1     r_dst,   $31
            srl      r_dst,   r_dst, 23
            andi     r_dst,   r_dst, 1 */
         p = mkFormR(p, 0x11, 0x11, fr_srcL, fr_srcR, 0, op + 48);
         p = mkFormR(p, 0x11, 0x2, r_dst, 31, 0, 0);
         p = mkFormS(p, 0, r_dst, 0, r_dst, 23, 2);
         p = mkFormI(p, 12, r_dst, r_dst, 1);
         goto done;
      }

      case Min_FpGpMove: {
         switch (i->Min.FpGpMove.op) {
            UInt rt, fs;
            case MFpGpMove_mfc1: {
               rt = iregNo(i->Min.FpGpMove.dst, mode64);
               fs = fregNo(i->Min.FpGpMove.src, mode64);
               p = mkFormR(p, 0x11, 0x0, rt, fs, 0x0, 0x0);
               break;
            }
            case MFpGpMove_dmfc1: {
               vassert(mode64);
               rt = iregNo(i->Min.FpGpMove.dst, mode64);
               fs = fregNo(i->Min.FpGpMove.src, mode64);
               p = mkFormR(p, 0x11, 0x1, rt, fs, 0x0, 0x0);
               break;
            }
            case MFpGpMove_mtc1: {
               rt = iregNo(i->Min.FpGpMove.src, mode64);
               fs = fregNo(i->Min.FpGpMove.dst, mode64);
               p = mkFormR(p, 0x11, 0x4, rt, fs, 0x0, 0x0);
               break;
            }
            case MFpGpMove_dmtc1: {
               vassert(mode64);
               rt = iregNo(i->Min.FpGpMove.src, mode64);
               fs = fregNo(i->Min.FpGpMove.dst, mode64);
               p = mkFormR(p, 0x11, 0x5, rt, fs, 0x0, 0x0);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }

      case Min_MoveCond: {
         switch (i->Min.MoveCond.op) {
            UInt d, s, t;
            case MFpMoveCond_movns: {
               d = fregNo(i->Min.MoveCond.dst, mode64);
               s = fregNo(i->Min.MoveCond.src, mode64);
               t = iregNo(i->Min.MoveCond.cond, mode64);
               p = mkFormR(p, 0x11, 0x10, t, s, d, 0x13);
               break;
            }
            case MFpMoveCond_movnd: {
               d = dregNo(i->Min.MoveCond.dst);
               s = dregNo(i->Min.MoveCond.src);
               t = iregNo(i->Min.MoveCond.cond, mode64);
               p = mkFormR(p, 0x11, 0x11, t, s, d, 0x13);
               break;
            }
            case MMoveCond_movn: {
               d = iregNo(i->Min.MoveCond.dst, mode64);
               s = iregNo(i->Min.MoveCond.src, mode64);
               t = iregNo(i->Min.MoveCond.cond, mode64);
               p = mkFormR(p, 0, s, t, d, 0, 0xb);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }

      case Min_EvCheck: {
         /* This requires a 32-bit dec/test in 32 mode. */
         /* We generate:
               lw      r9, amCounter
               addiu   r9, r9, -1
               sw      r9, amCounter
               bgez    r9, nofail
               lw      r9, amFailAddr
               jalr    r9
               nop
              nofail:
         */
         UChar* p0 = p;
         /* lw  r9, amCounter */
         p = do_load_or_store_word32(p, True /*isLoad*/ , /*r*/ 9,
                                     i->Min.EvCheck.amCounter, mode64);
         /* addiu r9,r9,-1 */
         p = mkFormI(p, 9, 9, 9, 0xFFFF);
         /* sw r30, amCounter */
         p = do_load_or_store_word32(p, False /*!isLoad*/ , /*r*/ 9,
                                     i->Min.EvCheck.amCounter, mode64);
         /* bgez t9, nofail */
         p = mkFormI(p, 1, 9, 1, 3);
         /* lw/ld r9, amFailAddr */
         p = do_load_or_store_machine_word(p, True /*isLoad*/ , /*r*/ 9,
                                           i->Min.EvCheck.amFailAddr, mode64);
         /* jalr $9 */
         p = mkFormR(p, 0, 9, 0, 31, 0, 9);  /* p += 4 */
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   /* p += 4 */
         /* nofail: */

         /* Crosscheck */
         vassert(evCheckSzB_MIPS() == (UChar*)p - (UChar*)p0);
         goto done;
      }

      case Min_ProfInc: {
         /* Generate a code template to increment a memory location whose
            address will be known later as an immediate value. This code
            template will be patched once the memory location is known.
            For now we do this with address == 0x65556555. */
         if (mode64) {
            /* 64-bit:
               move r9, 0x6555655565556555ULL
               ld r8, 0(r9)
               daddiu r8, r8, 1
               sd r8, 0(r9) */

            /* move r9, 0x6555655565556555ULL */
            p = mkLoadImm_EXACTLY2or6(p, /*r*/ 9, 0x6555655565556555ULL,
                                      True /*mode64*/);
            /* ld r8, 0(r9) */
            p = mkFormI(p, 55, 9, 8, 0);

            /* daddiu r8, r8, 1 */
            p = mkFormI(p, 25, 8, 8, 1);

            /* sd r8, 0(r9) */
            p = mkFormI(p, 63, 9, 8, 0);
         } else {
            /* 32-bit:
               move r9, 0x65556555
               lw r8, 0(r9)
               addiu r8, r8, 1         # add least significant word
               sw r8, 0(r9)
               sltiu r1, r8, 1         # set carry-in bit
               lw r8, 4(r9)
               addu r8, r8, r1
               sw r8, 4(r9) */

            /* move r9, 0x65556555 */
            p = mkLoadImm_EXACTLY2or6(p, /*r*/ 9, 0x65556555ULL,
                                      False /*!mode64*/);
            /* lw r8, 0(r9) */
            p = mkFormI(p, 35, 9, 8, 0);

            /* addiu r8, r8, 1         # add least significant word */
            p = mkFormI(p, 9, 8, 8, 1);

            /* sw r8, 0(r9) */
            p = mkFormI(p, 43, 9, 8, 0);

            /* sltiu r1, r8, 1         # set carry-in bit */
            p = mkFormI(p, 11, 8, 1, 1);

            /* lw r8, 4(r9) */
            p = mkFormI(p, 35, 9, 8, 4);

            /* addu r8, r8, r1 */
            p = mkFormR(p, 0, 8, 1, 8, 0, 33);

            /*  sw r8, 4(r9) */
            p = mkFormI(p, 43, 9, 8, 4);

         }
         /* Tell the caller .. */
         vassert(!(*is_profInc));
         *is_profInc = True;
         goto done;
      }

      default:
         goto bad;

   }

   bad:
      vex_printf("\n=> ");
      ppMIPSInstr(i, mode64);
      vpanic("emit_MIPSInstr");
      /* NOTREACHED */ done:
      vassert(p - &buf[0] <= 128);
      return p - &buf[0];
}

/* How big is an event check?  See case for Min_EvCheck in
   emit_MIPSInstr just above.  That crosschecks what this returns, so
   we can tell if we're inconsistent. */
Int evCheckSzB_MIPS (void)
{
  UInt kInstrSize = 4;
  return 7*kInstrSize;
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_MIPS ( VexEndness endness_host,
                                  void* place_to_chain,
                                  const void* disp_cp_chain_me_EXPECTED,
                                  const void* place_to_jump_to,
                                  Bool  mode64 )
{
   vassert(endness_host == VexEndnessLE || endness_host == VexEndnessBE);
   /* What we're expecting to see is:
        move r9, disp_cp_chain_me_to_EXPECTED
        jalr r9
        nop
      viz
        <8 or 24 bytes generated by mkLoadImm_EXACTLY2or6>
        0x120F809   # jalr r9
        0x00000000  # nop
   */
   UChar* p = (UChar*)place_to_chain;
   vassert(0 == (3 & (HWord)p));
   vassert(isLoadImm_EXACTLY2or6(p, /*r*/9,
                                 (UInt)(Addr)disp_cp_chain_me_EXPECTED,
                                 mode64));
   vassert(fetch32(p + (mode64 ? 24 : 8) + 0) == 0x120F809);
   vassert(fetch32(p + (mode64 ? 24 : 8) + 4) == 0x00000000);
   /* And what we want to change it to is either:
          move r9, place_to_jump_to
          jalr r9
          nop
        viz
          <8 bytes generated by mkLoadImm_EXACTLY2or6>
          0x120F809   # jalr r9
          0x00000000  # nop

      The replacement has the same length as the original.
   */

   p = mkLoadImm_EXACTLY2or6(p, /*r*/9,
                             (Addr)place_to_jump_to, mode64);
   p = emit32(p, 0x120F809);
   p = emit32(p, 0x00000000);

   Int len = p - (UChar*)place_to_chain;
   vassert(len == (mode64 ? 32 : 16)); /* stay sane */
   VexInvalRange vir = {(HWord)place_to_chain, len};
   return vir;
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_MIPS ( VexEndness endness_host,
                                    void* place_to_unchain,
                                    const void* place_to_jump_to_EXPECTED,
                                    const void* disp_cp_chain_me,
                                    Bool  mode64 )
{
   vassert(endness_host == VexEndnessLE || endness_host == VexEndnessBE);
   /* What we're expecting to see is:
        move r9, place_to_jump_to_EXPECTED
        jalr r9
        nop
      viz
        <8 or 24 bytes generated by mkLoadImm_EXACTLY2or6>
        0x120F809   # jalr r9
        0x00000000  # nop
   */
   UChar* p = (UChar*)place_to_unchain;
   vassert(0 == (3 & (HWord)p));
   vassert(isLoadImm_EXACTLY2or6(p, /*r*/ 9,
                                 (Addr)place_to_jump_to_EXPECTED,
                                 mode64));
   vassert(fetch32(p + (mode64 ? 24 : 8) + 0) == 0x120F809);
   vassert(fetch32(p + (mode64 ? 24 : 8) + 4) == 0x00000000);
   /* And what we want to change it to is:
        move r9, disp_cp_chain_me
        jalr r9
        nop
      viz
        <8 or 24 bytes generated by mkLoadImm_EXACTLY2or6>
        0x120F809   # jalr r9
        0x00000000  # nop
      The replacement has the same length as the original.
   */
   p = mkLoadImm_EXACTLY2or6(p, /*r*/ 9,
                             (Addr)disp_cp_chain_me, mode64);
   p = emit32(p, 0x120F809);
   p = emit32(p, 0x00000000);

   Int len = p - (UChar*)place_to_unchain;
   vassert(len == (mode64 ? 32 : 16)); /* stay sane */
   VexInvalRange vir = {(HWord)place_to_unchain, len};
   return vir;
}

/* Patch the counter address into a profile inc point, as previously
   created by the Min_ProfInc case for emit_MIPSInstr. */
VexInvalRange patchProfInc_MIPS ( VexEndness endness_host,
                                  void*  place_to_patch,
                                  const ULong* location_of_counter,
                                  Bool mode64 )
{
   vassert(endness_host == VexEndnessLE || endness_host == VexEndnessBE);
   if (mode64) {
      vassert(sizeof(ULong*) == 8);
   } else {
      vassert(sizeof(ULong*) == 4);
   }
   UChar* p = (UChar*)place_to_patch;
   vassert(0 == (3 & (HWord)p));
   vassert(isLoadImm_EXACTLY2or6((UChar *)p, /*r*/9,
                                 mode64 ? 0x6555655565556555ULL : 0x65556555,
                                 mode64));

   if (mode64) {
      vassert(fetch32(p + 24 + 0) == 0xDD280000);
      vassert(fetch32(p + 24 + 4) == 0x65080001);
      vassert(fetch32(p + 24 + 8) == 0xFD280000);
   } else {
      vassert(fetch32(p + 8 + 0) == 0x8D280000);
      vassert(fetch32(p + 8 + 4) == 0x25080001);
      vassert(fetch32(p + 8 + 8) == 0xAD280000);
      vassert(fetch32(p + 8 + 12) == 0x2d010001);
      vassert(fetch32(p + 8 + 16) == 0x8d280004);
      vassert(fetch32(p + 8 + 20) == 0x01014021);
      vassert(fetch32(p + 8 + 24) == 0xad280004);
   }

   p = mkLoadImm_EXACTLY2or6(p, /*r*/9,
                             (Addr)location_of_counter, mode64);

   VexInvalRange vir = {(HWord)p, 8};
   return vir;
}


/*---------------------------------------------------------------*/
/*--- end                                    host_mips_defs.c ---*/
/*---------------------------------------------------------------*/
