
/*---------------------------------------------------------------*/
/*--- begin                                host_tilegx_defs.c ---*/
/*---------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2010-2013 Tilera Corp.

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

/* Contributed by Zhi-Gang Liu <zliu at tilera dot com> */

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_tilegx_defs.h"
#include "tilegx_disasm.h"

/* Contributed by Zhi-Gang Liu <zliu at tilera dot com> */

/* Register number for guest state pointer in host code, r50 */
#define GuestSP     ( 50)
/* CONTEXT_EX0 offset */
#define OFFSET_EX0  (576)
/* CONTEXT_EX1 offset */
#define OFFSET_EX1  (584)
/* COND offset */
#define OFFSET_COND (608)
/* PC offset */
#define OFFSET_PC   (512)

/* guest_COND offset. */
#define COND_OFFSET() OFFSET_COND

/*---------------- Registers ----------------*/

void ppHRegTILEGX ( HReg reg )
{
  Int r;
  static const HChar *ireg_names[64] = {
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",  "r8",  "r9",
    "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19",
    "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29",
    "r30", "r31", "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39",
    "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47", "r48", "r49",
    "r50", "r51", "r52", "r53", "r54", "r55",
    "sn",  "idn0", "idn1", "udn0", "udn1", "udn2", "udn3", "zero"
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
  case HRcInt64:
    r = hregEncoding(reg);
    vassert(r >= 0 && r < 64);
    vex_printf("%s", ireg_names[r]);
    return;
  case HRcFlt32:
    r = hregEncoding(reg);
    vassert(r >= 0 && r < 64);
    vex_printf("%s", ireg_names[r]);
    return;
  case HRcFlt64:
    r = hregEncoding(reg);
    vassert(r >= 0 && r < 64);
    vex_printf("%s", ireg_names[r]);
    return;
  default:
    vpanic("ppHRegTILEGX");
  }

  return;
}

static const HChar* tilegxUnaryOp [] =
  {
    "clz ",
    "ctz ",
    "nop "
  };

static const HChar* tilegxAluOp [] =
  {  "Alu_invalid",
     "Add ",
     "Sub ",
     "And ",
     "Or  ",
     "Nor ",
     "Xor "
  };

static const HChar* tilegxShftOp [] =
  {
    "Shft_invalid",
    "Sll    ",
    "Srl    ",
    "Sra    ",
    "Sll8x8 ",
    "Srl8x8 ",
  };

static const HChar* tilegxBfOp [] =
  {
    "BfExts ",
    "BfEtxu ",
    "BfIns  "
  };


static const HChar* tilegxAcasOp [] =
  {
    "CmpExch    ",
    "Exch       ",
    "FetchAnd   ",
    "FetchAdd   ",
    "FetchAddgez",
    "FetchOr    "
  };

static const HChar* tilegxInstrTag [] =
  {
    "Imm      ",
    "ALU      ",
    "Shift    ",
    "Unary    ",
    "Cmp      ",
    "CmpI     ",
    "Mul      ",
    "Call     ",
    "XDirect  ",
    "XIndir   ",
    "XAssisted",
    "EvCheck  ",
    "ProfInc  ",
    "RdWrLR   ",
    "Load     ",
    "Store    ",
    "MovCond  ",
    "BitField ",
    "ACAS     "
  };

/* -------- Pretty Print instructions ------------- */
static void ppLoadImm ( HReg dst, ULong imm )
{
  vex_printf("li ");
  ppHRegTILEGX(dst);
  vex_printf(",0x%016lx", (unsigned long)imm);
}

void ppTILEGXInstr ( const TILEGXInstr * instr )
{
  vex_printf("%s ", tilegxInstrTag[instr->tag]);
  switch (instr->tag) {
  case GXin_LI:  {
    ppHRegTILEGX(instr->GXin.LI.dst);
    vex_printf(",0x%016llx", instr->GXin.LI.imm);
  }
    break;

  case GXin_Alu: {
    HReg r_srcL = instr->GXin.Alu.srcL;
    TILEGXRH *rh_srcR = instr->GXin.Alu.srcR;
    /* generic */
    vex_printf("%s ", tilegxAluOp[instr->GXin.Alu.op]);
    ppHRegTILEGX(instr->GXin.Alu.dst);
    vex_printf(",");
    ppHRegTILEGX(r_srcL);
    vex_printf(",");
    ppTILEGXRH(rh_srcR);
  }
    break;

  case GXin_Shft: {
    HReg r_srcL = instr->GXin.Shft.srcL;
    TILEGXRH *rh_srcR = instr->GXin.Shft.srcR;
    vex_printf("%s ", tilegxShftOp[instr->GXin.Shft.op]);
    ppHRegTILEGX(instr->GXin.Shft.dst);
    vex_printf(",");
    ppHRegTILEGX(r_srcL);
    vex_printf(",");
    ppTILEGXRH(rh_srcR);
  }
    break;

  case GXin_Unary: {
    vex_printf("%s ", tilegxUnaryOp[instr->GXin.Unary.op]);
    ppHRegTILEGX(instr->GXin.Unary.dst);
    vex_printf(",");
    ppHRegTILEGX(instr->GXin.Unary.src);
  }
    break;

  case GXin_Cmp: {
    ppHRegTILEGX(instr->GXin.Cmp.dst);
    vex_printf(" = %s ( ", showTILEGXCondCode(instr->GXin.Cmp.cond));
    ppHRegTILEGX(instr->GXin.Cmp.srcL);
    vex_printf(", ");
    ppHRegTILEGX(instr->GXin.Cmp.srcR);
    vex_printf(" )");
  }
    break;

  case GXin_CmpI: {
    ppHRegTILEGX(instr->GXin.CmpI.dst);
    vex_printf(" = %s ( ", showTILEGXCondCode(instr->GXin.CmpI.cond));
    ppHRegTILEGX(instr->GXin.CmpI.srcL);
    vex_printf(", ");
    ppTILEGXRH(instr->GXin.CmpI.srcR);
    vex_printf(" )");
  }
    break;

  case GXin_Mul: {
    if (instr->GXin.Mul.widening == False) {
      vex_printf("mul ");
      ppHRegTILEGX(instr->GXin.Mul.dst);
      vex_printf(", ");
      ppHRegTILEGX(instr->GXin.Mul.srcL);
      vex_printf(", ");
      ppHRegTILEGX(instr->GXin.Mul.srcR);

    } else {
      vex_printf("%s ", instr->GXin.Mul.syned ? "mull32s" : "mull32u");
      ppHRegTILEGX(instr->GXin.Mul.dst);
      vex_printf(", ");
      ppHRegTILEGX(instr->GXin.Mul.srcL);
      vex_printf(", ");
      ppHRegTILEGX(instr->GXin.Mul.srcR);
    }
  }
    break;

  case GXin_Call: {
    Int n;
    if (instr->GXin.Call.cond != TILEGXcc_AL) {
      vex_printf("if (%s (", showTILEGXCondCode(instr->GXin.Call.cond));
      ppHRegTILEGX(instr->GXin.Call.src);
      vex_printf(",zero))");
    }
    else
      vex_printf("(always) ");

    vex_printf("{ ");
    ppLoadImm(hregTILEGX_R11(), instr->GXin.Call.target);

    vex_printf(" ; [");
    for (n = 0; n < 56; n++) {
      if (instr->GXin.Call.argiregs & (1ULL << n)) {
        vex_printf("r%d", n);
        if ((instr->GXin.Call.argiregs >> n) > 1)
          vex_printf(",");
      }
    }
    vex_printf("] }");
  }
    break;

  case GXin_XDirect:
    vex_printf("(xDirect) ");
    vex_printf("if (guest_COND.%s) { ",
               showTILEGXCondCode(instr->GXin.XDirect.cond));
    vex_printf("move r11, 0x%x,", (UInt)instr->GXin.XDirect.dstGA);
    vex_printf("; st r11, ");
    ppTILEGXAMode(instr->GXin.XDirect.amPC);
    vex_printf("; move r11, $disp_cp_chain_me_to_%sEP; jalr r11; nop}",
               instr->GXin.XDirect.toFastEP ? "fast" : "slow");
    return;
  case GXin_XIndir:
    vex_printf("(xIndir) ");
    vex_printf("if (guest_COND.%s) { st ",
               showTILEGXCondCode(instr->GXin.XIndir.cond));
    ppHRegTILEGX(instr->GXin.XIndir.dstGA);
    vex_printf(", ");
    ppTILEGXAMode(instr->GXin.XIndir.amPC);
    vex_printf("; move r11, $disp_indir; jalr r11; nop}");
    return;
  case GXin_XAssisted:
    vex_printf("(xAssisted) ");
    vex_printf("if (guest_COND.%s) { ",
               showTILEGXCondCode(instr->GXin.XAssisted.cond));
    vex_printf("st ");
    ppHRegTILEGX(instr->GXin.XAssisted.dstGA);
    vex_printf(", ");
    ppTILEGXAMode(instr->GXin.XAssisted.amPC);
    vex_printf("; move r50, $IRJumpKind_to_TRCVAL(%d)",
               (Int)instr->GXin.XAssisted.jk);
    vex_printf("; move r11, $disp_assisted; jalr r11; nop; }");
    return;

  case GXin_EvCheck:
    vex_printf("(evCheck) ld r11, ");
    ppTILEGXAMode(instr->GXin.EvCheck.amCounter);
    vex_printf("; addli r11, r11, -1");
    vex_printf("; st r11, ");
    ppTILEGXAMode(instr->GXin.EvCheck.amCounter);
    vex_printf("; bgez r11, nofail; jalr *");
    ppTILEGXAMode(instr->GXin.EvCheck.amFailAddr);
    vex_printf("; nofail:");
    return;
  case GXin_ProfInc:
    vex_printf("(profInc) move r11, ($NotKnownYet); "
               "ld r8, r11; "
               "addi r8, r8, 1; "
               "st r11, r8; " );
    return;
  case GXin_Load: {
    UChar sz = instr->GXin.Load.sz;
    UChar c_sz = sz == 1 ? '1' : sz == 2 ? '2' : sz == 4 ? '4' : '8';
    vex_printf("ld%c ", c_sz);
    ppHRegTILEGX(instr->GXin.Load.dst);
    vex_printf(",");
    ppTILEGXAMode(instr->GXin.Load.src);
  }
    break;

  case GXin_Store: {
    UChar sz = instr->GXin.Store.sz;
    UChar c_sz = sz == 1 ? '1' : sz == 2 ? '2' : sz == 4 ? '4' : '8';
    vex_printf("st%c ", c_sz);
    ppTILEGXAMode(instr->GXin.Store.dst);
    vex_printf(",");
    ppHRegTILEGX(instr->GXin.Store.src);
  }
    break;

  case GXin_MovCond: {
    ppHRegTILEGX(instr->GXin.MovCond.dst);
    vex_printf("=");
    showTILEGXCondCode(instr->GXin.MovCond.cond);
    vex_printf("?");
    ppHRegTILEGX(instr->GXin.MovCond.srcL);
    vex_printf(":");
    ppTILEGXRH(instr->GXin.MovCond.srcR);
  }
    break;

  case GXin_Acas: {
    vex_printf("%s ",  tilegxAcasOp[instr->GXin.Acas.op]);
    ppHRegTILEGX(instr->GXin.Acas.old);
    vex_printf(",");
    if (instr->GXin.Acas.op == GXacas_CMPEXCH) {
      ppHRegTILEGX(instr->GXin.Acas.exp);
      vex_printf(",");
    }
    ppHRegTILEGX(instr->GXin.Acas.new);
  }
    break;

  case GXin_Bf: {
    vex_printf("%s ",  tilegxBfOp[instr->GXin.Bf.op]);
    ppHRegTILEGX(instr->GXin.Bf.dst);
    vex_printf(",");
    ppHRegTILEGX(instr->GXin.Bf.src);
    vex_printf(",");
    vex_printf("%d,%d", (Int)instr->GXin.Bf.Start, (Int)instr->GXin.Bf.End);
  }
    break;

  default:
    vassert(0);
  }
}


const RRegUniverse* getRRegUniverse_TILEGX ( void )
{
  /* The 'universe' is constant and BIG, do it statically. */
  static RRegUniverse rRegUniverse_TILEGX;
  static UInt         rRegUniverse_TILEGX_initted = False;

  /* Get a pointer of the 'universe' */
  RRegUniverse* ru = &rRegUniverse_TILEGX;

  if (LIKELY(rRegUniverse_TILEGX_initted))
    return ru;

  RRegUniverse__init(ru);

  /* Callee saves ones are listed first, since we prefer them
     if they're available */

  ru->regs[ru->size++] = hregTILEGX_R30();
  ru->regs[ru->size++] = hregTILEGX_R31();
  ru->regs[ru->size++] = hregTILEGX_R32();
  ru->regs[ru->size++] = hregTILEGX_R33();
  ru->regs[ru->size++] = hregTILEGX_R34();
  ru->regs[ru->size++] = hregTILEGX_R35();
  ru->regs[ru->size++] = hregTILEGX_R36();
  ru->regs[ru->size++] = hregTILEGX_R37();
  ru->regs[ru->size++] = hregTILEGX_R38();
  ru->regs[ru->size++] = hregTILEGX_R39();

  ru->regs[ru->size++] = hregTILEGX_R40();
  ru->regs[ru->size++] = hregTILEGX_R41();
  ru->regs[ru->size++] = hregTILEGX_R42();
  ru->regs[ru->size++] = hregTILEGX_R43();
  ru->regs[ru->size++] = hregTILEGX_R44();
  ru->regs[ru->size++] = hregTILEGX_R45();
  ru->regs[ru->size++] = hregTILEGX_R46();
  ru->regs[ru->size++] = hregTILEGX_R47();
  ru->regs[ru->size++] = hregTILEGX_R48();
  ru->regs[ru->size++] = hregTILEGX_R49();

  /* GPR 50 is reserved as Guest state */
  /* GPR 51 is reserved register, mainly used to do memory
     load and store since TileGx has no pre-displacement
     addressing mode */

  ru->regs[ru->size++] = hregTILEGX_R10();

  /* GPR 11 is reserved as next guest address */

  ru->regs[ru->size++] = hregTILEGX_R13();
  ru->regs[ru->size++] = hregTILEGX_R14();
  ru->regs[ru->size++] = hregTILEGX_R15();
  ru->regs[ru->size++] = hregTILEGX_R16();
  ru->regs[ru->size++] = hregTILEGX_R17();
  ru->regs[ru->size++] = hregTILEGX_R18();
  ru->regs[ru->size++] = hregTILEGX_R19();
  ru->regs[ru->size++] = hregTILEGX_R20();
  ru->regs[ru->size++] = hregTILEGX_R21();
  ru->regs[ru->size++] = hregTILEGX_R22();
  ru->regs[ru->size++] = hregTILEGX_R23();
  ru->regs[ru->size++] = hregTILEGX_R24();
  ru->regs[ru->size++] = hregTILEGX_R25();
  ru->regs[ru->size++] = hregTILEGX_R26();
  ru->regs[ru->size++] = hregTILEGX_R27();
  ru->regs[ru->size++] = hregTILEGX_R28();
  ru->regs[ru->size++] = hregTILEGX_R29();

  ru->allocable = ru->size;

  /* And other unallocable registers. */
  ru->regs[ru->size++] = hregTILEGX_R0();
  ru->regs[ru->size++] = hregTILEGX_R1();
  ru->regs[ru->size++] = hregTILEGX_R2();
  ru->regs[ru->size++] = hregTILEGX_R3();
  ru->regs[ru->size++] = hregTILEGX_R4();
  ru->regs[ru->size++] = hregTILEGX_R5();
  ru->regs[ru->size++] = hregTILEGX_R6();
  ru->regs[ru->size++] = hregTILEGX_R7();
  ru->regs[ru->size++] = hregTILEGX_R8();
  ru->regs[ru->size++] = hregTILEGX_R9();
  ru->regs[ru->size++] = hregTILEGX_R11();
  ru->regs[ru->size++] = hregTILEGX_R12();
  ru->regs[ru->size++] = hregTILEGX_R50();
  ru->regs[ru->size++] = hregTILEGX_R51();
  ru->regs[ru->size++] = hregTILEGX_R52();
  ru->regs[ru->size++] = hregTILEGX_R53();
  ru->regs[ru->size++] = hregTILEGX_R54();
  ru->regs[ru->size++] = hregTILEGX_R55();
  ru->regs[ru->size++] = hregTILEGX_R63();

  rRegUniverse_TILEGX_initted = True;

  RRegUniverse__check_is_sane(ru);

  return ru;
}

/*----------------- Condition Codes ----------------------*/

const HChar *showTILEGXCondCode ( TILEGXCondCode cond )
{
  switch (cond) {
  case TILEGXcc_EQ:
    return "e"; /* equal */
  case TILEGXcc_EQ8x8:
    return "e8x8"; /* equal */

  case TILEGXcc_NE:
    return "ne";   /* not equal */
  case TILEGXcc_NE8x8:
    return "ne8x8";   /* not equal */

  case TILEGXcc_HS:
    return "hs";   /* >=u (higher or same) */
  case TILEGXcc_LO:
    return "lo";   /* <u  (lower) */

  case TILEGXcc_MI:
    return "mi";   /* minus (negative) */
  case TILEGXcc_PL:
    return "pl";   /* plus (zero or +ve) */

  case TILEGXcc_VS:
    return "vs";   /* overflow */
  case TILEGXcc_VC:
    return "vc";   /* no overflow */

  case TILEGXcc_HI:
    return "hi";   /* >u   (higher) */
  case TILEGXcc_LS:
    return "ls";   /* <=u  (lower or same) */

  case TILEGXcc_GE:
    return "ge";   /* >=s (signed greater or equal) */
  case TILEGXcc_LT:
    return "lt";   /* <s  (signed less than) */

  case TILEGXcc_GT:
    return "gt";   /* >s  (signed greater) */
  case TILEGXcc_LE:
    return "le";   /* <=s (signed less or equal) */

  case TILEGXcc_AL:
    return "al";   /* always (unconditional) */
  case TILEGXcc_NV:
    return "nv";   /* never (unconditional): */
  case TILEGXcc_EZ:
    return "ez"; /* equal 0 */
  case TILEGXcc_NZ:
    return "nz"; /* not equal 0 */

  default:
    vpanic("showTILEGXCondCode");
  }
}


/* --------- TILEGXAMode: memory address expressions. --------- */

TILEGXAMode *TILEGXAMode_IR ( Int idx, HReg base )
{
  TILEGXAMode *am = LibVEX_Alloc(sizeof(TILEGXAMode));
  am->tag = GXam_IR;
  am->GXam.IR.base = base;
  am->GXam.IR.index = idx;

  return am;
}

TILEGXAMode *nextTILEGXAModeInt ( TILEGXAMode * am )
{
  if (am->tag == GXam_IR)
    return TILEGXAMode_IR(am->GXam.IR.index + 4, am->GXam.IR.base);

  vpanic("dopyTILEGXAMode");
}

void ppTILEGXAMode ( const TILEGXAMode * am )
{
  if (am->tag == GXam_IR)
  {
    if (am->GXam.IR.index == 0)
      vex_printf("(");
    else
      vex_printf("%d(", (Int) am->GXam.IR.index);
    ppHRegTILEGX(am->GXam.IR.base);
    vex_printf(")");
    return;
  }
  vpanic("ppTILEGXAMode");
}

static void addRegUsage_TILEGXAMode ( HRegUsage * u, TILEGXAMode * am )
{
  if (am->tag == GXam_IR)
  {
    addHRegUse(u, HRmRead, am->GXam.IR.base);
    return;
  }

  vpanic("addRegUsage_TILEGXAMode");
}

static void mapRegs_TILEGXAMode ( HRegRemap * m, TILEGXAMode * am )
{
  if (am->tag == GXam_IR)
  {
    am->GXam.IR.base = lookupHRegRemap(m, am->GXam.IR.base);
    return;
  }

  vpanic("mapRegs_TILEGXAMode");
}

/* --------- Operand, which can be a reg or a u16/s16. --------- */

TILEGXRH *TILEGXRH_Imm ( Bool syned, UShort imm16 )
{
  TILEGXRH *op = LibVEX_Alloc(sizeof(TILEGXRH));
  op->tag = GXrh_Imm;
  op->GXrh.Imm.syned = syned;
  op->GXrh.Imm.imm16 = imm16;
  /* If this is a signed value, ensure it's not -32768, so that we
     are guaranteed always to be able to negate if needed. */
  if (syned)
    vassert(imm16 != 0x8000);
  vassert(syned == True || syned == False);
  return op;
}

TILEGXRH *TILEGXRH_Reg ( HReg reg )
{
  TILEGXRH *op = LibVEX_Alloc(sizeof(TILEGXRH));
  op->tag = GXrh_Reg;
  op->GXrh.Reg.reg = reg;
  return op;
}

void ppTILEGXRH ( const TILEGXRH * op )
{
  TILEGXRHTag tag = op->tag;
  switch (tag) {
  case GXrh_Imm:
    if (op->GXrh.Imm.syned)
      vex_printf("%d", (Int) (Short) op->GXrh.Imm.imm16);
    else
      vex_printf("%u", (UInt) (UShort) op->GXrh.Imm.imm16);
    return;
  case GXrh_Reg:
    ppHRegTILEGX(op->GXrh.Reg.reg);
    return;
  default:
    vpanic("ppTILEGXRH");
  }
}

/* An TILEGXRH can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_TILEGXRH ( HRegUsage * u, TILEGXRH * op )
{
  switch (op->tag) {
  case GXrh_Imm:
    return;
  case GXrh_Reg:
    addHRegUse(u, HRmRead, op->GXrh.Reg.reg);
    return;
  default:
    vpanic("addRegUsage_TILEGXRH");
  }
}

static void mapRegs_TILEGXRH ( HRegRemap * m, TILEGXRH * op )
{
  switch (op->tag) {
  case GXrh_Imm:
    return;
  case GXrh_Reg:
    op->GXrh.Reg.reg = lookupHRegRemap(m, op->GXrh.Reg.reg);
    return;
  default:
    vpanic("mapRegs_TILEGXRH");
  }
}

TILEGXInstr *TILEGXInstr_LI ( HReg dst, ULong imm )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_LI;
  i->GXin.LI.dst = dst;
  i->GXin.LI.imm = imm;
  return i;
}

TILEGXInstr *TILEGXInstr_Alu ( TILEGXAluOp op, HReg dst, HReg srcL,
                               TILEGXRH * srcR )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Alu;
  i->GXin.Alu.op = op;
  i->GXin.Alu.dst = dst;
  i->GXin.Alu.srcL = srcL;
  i->GXin.Alu.srcR = srcR;
  return i;
}

TILEGXInstr *TILEGXInstr_Shft ( TILEGXShftOp op, Bool sz32, HReg dst, HReg srcL,
                                TILEGXRH * srcR )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Shft;
  i->GXin.Shft.op = op;
  i->GXin.Shft.sz32 = sz32;
  i->GXin.Shft.dst = dst;
  i->GXin.Shft.srcL = srcL;
  i->GXin.Shft.srcR = srcR;
  return i;
}

TILEGXInstr *TILEGXInstr_Unary ( TILEGXUnaryOp op, HReg dst, HReg src )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Unary;
  i->GXin.Unary.op = op;
  i->GXin.Unary.dst = dst;
  i->GXin.Unary.src = src;
  return i;
}

TILEGXInstr *TILEGXInstr_Cmp ( Bool syned, Bool sz32, HReg dst,
                               HReg srcL, HReg srcR, TILEGXCondCode cond )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Cmp;
  i->GXin.Cmp.syned = syned;
  i->GXin.Cmp.sz32 = sz32;
  i->GXin.Cmp.dst = dst;
  i->GXin.Cmp.srcL = srcL;
  i->GXin.Cmp.srcR = srcR;
  i->GXin.Cmp.cond = cond;
  return i;
}

TILEGXInstr *TILEGXInstr_CmpI ( Bool syned, Bool sz32, HReg dst,
                                HReg srcL, TILEGXRH * srcR,
                                TILEGXCondCode cond )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_CmpI;
  i->GXin.CmpI.syned = syned;
  i->GXin.CmpI.sz32 = sz32;
  i->GXin.CmpI.dst = dst;
  i->GXin.CmpI.srcL = srcL;
  i->GXin.CmpI.srcR = srcR;
  i->GXin.CmpI.cond = cond;
  return i;
}

TILEGXInstr *TILEGXInstr_Bf ( TILEGXBfOp op, HReg dst, HReg src,
                              UInt Start, UInt End )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Bf;
  i->GXin.Bf.op = op;
  i->GXin.Bf.dst = dst;
  i->GXin.Bf.src = src;
  i->GXin.Bf.Start = Start;
  i->GXin.Bf.End = End;
  return i;
}

TILEGXInstr *TILEGXInstr_Acas ( TILEGXAcasOp op, HReg old,
                                HReg addr, HReg exp, HReg new, UInt sz )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Acas;
  i->GXin.Acas.op = op;
  i->GXin.Acas.old = old;
  i->GXin.Acas.addr = addr;
  i->GXin.Acas.exp = exp;
  i->GXin.Acas.new = new;
  i->GXin.Acas.sz = sz;
  return i;
}

/* multiply */
TILEGXInstr *TILEGXInstr_Mul ( Bool syned, Bool wid, Bool sz32,
                               HReg dst, HReg srcL,
                               HReg srcR )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Mul;
  i->GXin.Mul.syned = syned;
  i->GXin.Mul.widening = wid; /* widen=True else False */
  i->GXin.Mul.sz32 = sz32;    /* True = 32 bits */
  i->GXin.Mul.dst = dst;
  i->GXin.Mul.srcL = srcL;
  i->GXin.Mul.srcR = srcR;
  return i;
}

TILEGXInstr *TILEGXInstr_Call ( TILEGXCondCode cond, Addr64 target,
                                ULong argiregs,
                                HReg src )
{
  ULong mask;
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Call;
  i->GXin.Call.cond = cond;
  i->GXin.Call.target = target;
  i->GXin.Call.argiregs = argiregs;
  i->GXin.Call.src = src;

  /* Only r0 .. r9 inclusive may be used as arg regs. Hence: */
  mask = (1ULL << 10) - 1;
  vassert(0 == (argiregs & ~mask));
  return i;
}

TILEGXInstr *TILEGXInstr_CallAlways ( TILEGXCondCode cond, Addr64 target,
                                      ULong argiregs )
{
  ULong mask;
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Call;
  i->GXin.Call.cond = cond;
  i->GXin.Call.target = target;
  i->GXin.Call.argiregs = argiregs;

  /* Only r0 .. r9 inclusive may be used as arg regs. Hence: */
  mask = (1ULL << 10) - 1;
  vassert(0 == (argiregs & ~mask));
  return i;
}

TILEGXInstr *TILEGXInstr_XDirect ( Addr64 dstGA, TILEGXAMode* amPC,
                                   TILEGXCondCode cond, Bool toFastEP )
{
  TILEGXInstr* i             = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag                     = GXin_XDirect;
  i->GXin.XDirect.dstGA      = dstGA;
  i->GXin.XDirect.amPC       = amPC;
  i->GXin.XDirect.cond       = cond;
  i->GXin.XDirect.toFastEP   = toFastEP;
  return i;
}

TILEGXInstr *TILEGXInstr_XIndir ( HReg dstGA, TILEGXAMode* amPC,
                                  TILEGXCondCode cond )
{
  TILEGXInstr* i           = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag                   = GXin_XIndir;
  i->GXin.XIndir.dstGA     = dstGA;
  i->GXin.XIndir.amPC      = amPC;
  i->GXin.XIndir.cond      = cond;
  return i;
}

TILEGXInstr *TILEGXInstr_XAssisted ( HReg dstGA, TILEGXAMode* amPC,
                                     TILEGXCondCode cond, IRJumpKind jk )
{
  TILEGXInstr* i              = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag                      = GXin_XAssisted;
  i->GXin.XAssisted.dstGA     = dstGA;
  i->GXin.XAssisted.amPC      = amPC;
  i->GXin.XAssisted.cond      = cond;
  i->GXin.XAssisted.jk        = jk;
  return i;
}

TILEGXInstr *TILEGXInstr_EvCheck ( TILEGXAMode* amCounter,
                                   TILEGXAMode* amFailAddr ) {
  TILEGXInstr* i               = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag                       = GXin_EvCheck;
  i->GXin.EvCheck.amCounter     = amCounter;
  i->GXin.EvCheck.amFailAddr    = amFailAddr;
  return i;
}

TILEGXInstr* TILEGXInstr_ProfInc ( void ) {
  TILEGXInstr* i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag       = GXin_ProfInc;
  return i;
}

TILEGXInstr *TILEGXInstr_Load ( UChar sz, HReg dst, TILEGXAMode * src )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Load;
  i->GXin.Load.sz = sz;
  i->GXin.Load.src = src;
  i->GXin.Load.dst = dst;
  vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);
  return i;
}

TILEGXInstr *TILEGXInstr_Store(UChar sz, TILEGXAMode * dst, HReg src)
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_Store;
  i->GXin.Store.sz = sz;
  i->GXin.Store.src = src;
  i->GXin.Store.dst = dst;
  vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);
  return i;
}

/* Read/Write Link Register */
TILEGXInstr *TILEGXInstr_RdWrLR ( Bool wrLR, HReg gpr )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_RdWrLR;
  i->GXin.RdWrLR.wrLR = wrLR;
  i->GXin.RdWrLR.gpr = gpr;
  return i;
}

TILEGXInstr *TILEGXInstr_MovCond ( HReg dst, HReg argL, TILEGXRH * argR,
                                   HReg condR, TILEGXCondCode cond )
{
  TILEGXInstr *i = LibVEX_Alloc(sizeof(TILEGXInstr));
  i->tag = GXin_MovCond;
  i->GXin.MovCond.dst = dst;
  i->GXin.MovCond.srcL = argL;
  i->GXin.MovCond.srcR = argR;
  i->GXin.MovCond.condR = condR;
  i->GXin.MovCond.cond = cond;
  return i;
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_TILEGXInstr ( HRegUsage * u, TILEGXInstr * i )
{
  initHRegUsage(u);
  switch (i->tag) {
  case GXin_LI:
    addHRegUse(u, HRmWrite, i->GXin.LI.dst);
    break;
  case GXin_Alu:
    addHRegUse(u, HRmRead, i->GXin.Alu.srcL);
    addRegUsage_TILEGXRH(u, i->GXin.Alu.srcR);
    addHRegUse(u, HRmWrite, i->GXin.Alu.dst);
    return;
  case GXin_CmpI:
    addHRegUse(u, HRmRead, i->GXin.CmpI.srcL);
    addRegUsage_TILEGXRH(u, i->GXin.CmpI.srcR);
    addHRegUse(u, HRmWrite, i->GXin.CmpI.dst);
    return;
  case GXin_Shft:
    addHRegUse(u, HRmRead, i->GXin.Shft.srcL);
    addRegUsage_TILEGXRH(u, i->GXin.Shft.srcR);
    addHRegUse(u, HRmWrite, i->GXin.Shft.dst);
    return;
  case GXin_Cmp:
    addHRegUse(u, HRmRead, i->GXin.Cmp.srcL);
    addHRegUse(u, HRmRead, i->GXin.Cmp.srcR);
    addHRegUse(u, HRmWrite, i->GXin.Cmp.dst);
    return;
  case GXin_Bf:
    addHRegUse(u, HRmRead, i->GXin.Bf.src);
    addHRegUse(u, HRmWrite, i->GXin.Bf.dst);
    return;
  case GXin_Acas:
    addHRegUse(u, HRmRead, i->GXin.Acas.addr);
    addHRegUse(u, HRmRead, i->GXin.Acas.new);
    if (i->GXin.Acas.op == GXacas_CMPEXCH)
      addHRegUse(u, HRmRead, i->GXin.Acas.exp);
    addHRegUse(u, HRmWrite, i->GXin.Acas.old);
    return;
  case GXin_Unary:
    addHRegUse(u, HRmRead, i->GXin.Unary.src);
    addHRegUse(u, HRmWrite, i->GXin.Unary.dst);
    return;
  case GXin_Mul:
    addHRegUse(u, HRmWrite, i->GXin.Mul.dst);
    addHRegUse(u, HRmRead, i->GXin.Mul.srcL);
    addHRegUse(u, HRmRead, i->GXin.Mul.srcR);
    return;
  case GXin_Call: {
    if (i->GXin.Call.cond != TILEGXcc_AL)
      addHRegUse(u, HRmRead, i->GXin.Call.src);
    ULong argir;

    // Only need save r10-r29, and r0-r9 is not allocable.
    addHRegUse(u, HRmWrite, hregTILEGX_R10());
    addHRegUse(u, HRmWrite, hregTILEGX_R11());
    addHRegUse(u, HRmWrite, hregTILEGX_R12());
    addHRegUse(u, HRmWrite, hregTILEGX_R13());
    addHRegUse(u, HRmWrite, hregTILEGX_R14());
    addHRegUse(u, HRmWrite, hregTILEGX_R15());

    addHRegUse(u, HRmWrite, hregTILEGX_R16());
    addHRegUse(u, HRmWrite, hregTILEGX_R17());
    addHRegUse(u, HRmWrite, hregTILEGX_R18());
    addHRegUse(u, HRmWrite, hregTILEGX_R19());
    addHRegUse(u, HRmWrite, hregTILEGX_R20());
    addHRegUse(u, HRmWrite, hregTILEGX_R21());
    addHRegUse(u, HRmWrite, hregTILEGX_R22());
    addHRegUse(u, HRmWrite, hregTILEGX_R23());

    addHRegUse(u, HRmWrite, hregTILEGX_R24());
    addHRegUse(u, HRmWrite, hregTILEGX_R25());
    addHRegUse(u, HRmWrite, hregTILEGX_R26());
    addHRegUse(u, HRmWrite, hregTILEGX_R27());

    addHRegUse(u, HRmWrite, hregTILEGX_R28());
    addHRegUse(u, HRmWrite, hregTILEGX_R29());

    /* Now we have to state any parameter-carrying registers
       which might be read.  This depends on the argiregs field. */
    argir = i->GXin.Call.argiregs;
    if (argir & (1 << 9))
      addHRegUse(u, HRmRead, hregTILEGX_R9());
    if (argir & (1 << 8))
      addHRegUse(u, HRmRead, hregTILEGX_R8());
    if (argir & (1 << 7))
      addHRegUse(u, HRmRead, hregTILEGX_R7());
    if (argir & (1 << 6))
      addHRegUse(u, HRmRead, hregTILEGX_R6());
    if (argir & (1 << 5))
      addHRegUse(u, HRmRead, hregTILEGX_R5());
    if (argir & (1 << 4))
      addHRegUse(u, HRmRead, hregTILEGX_R4());
    if (argir & (1 << 3))
      addHRegUse(u, HRmRead, hregTILEGX_R3());
    if (argir & (1 << 2))
      addHRegUse(u, HRmRead, hregTILEGX_R2());
    if (argir & (1 << 1))
      addHRegUse(u, HRmRead, hregTILEGX_R1());
    if (argir & (1 << 0))
      addHRegUse(u, HRmRead, hregTILEGX_R0());

    vassert(0 == (argir & ~((1ULL << 10) - 1)));
    return;
  }
  case GXin_XDirect:
    addRegUsage_TILEGXAMode(u, i->GXin.XDirect.amPC);
    return;
  case GXin_XIndir:
    addHRegUse(u, HRmRead, i->GXin.XIndir.dstGA);
    addRegUsage_TILEGXAMode(u, i->GXin.XIndir.amPC);
    return;
  case GXin_XAssisted:
    addHRegUse(u, HRmRead, i->GXin.XAssisted.dstGA);
    addRegUsage_TILEGXAMode(u, i->GXin.XAssisted.amPC);
    return;

  case GXin_EvCheck:
    addRegUsage_TILEGXAMode(u, i->GXin.EvCheck.amCounter);
    addRegUsage_TILEGXAMode(u, i->GXin.EvCheck.amFailAddr);
    return;
  case GXin_ProfInc:
    return;
  case GXin_Load:
    addRegUsage_TILEGXAMode(u, i->GXin.Load.src);
    addHRegUse(u, HRmWrite, i->GXin.Load.dst);
    return;
  case GXin_Store:
    addHRegUse(u, HRmRead, i->GXin.Store.src);
    addRegUsage_TILEGXAMode(u, i->GXin.Store.dst);
    return;
  case GXin_RdWrLR:
    addHRegUse(u, (i->GXin.RdWrLR.wrLR ? HRmRead : HRmWrite),
               i->GXin.RdWrLR.gpr);
    return;
  case GXin_MovCond:
    if (i->GXin.MovCond.srcR->tag == GXrh_Reg) {
      addHRegUse(u, HRmRead, i->GXin.MovCond.srcR->GXrh.Reg.reg);
    }
    addHRegUse(u, HRmRead, i->GXin.MovCond.srcL);
    addHRegUse(u, HRmRead, i->GXin.MovCond.condR);
    addHRegUse(u, HRmWrite, i->GXin.MovCond.dst);
    return;
  default:
    vpanic("getRegUsage_TILEGXInstr");
  }
}

/* local helper */
static void mapReg ( HRegRemap * m, HReg * r )
{
  *r = lookupHRegRemap(m, *r);
}

void mapRegs_TILEGXInstr ( HRegRemap * m, TILEGXInstr * i )
{
  switch (i->tag) {
  case GXin_LI:
    mapReg(m, &i->GXin.LI.dst);
    break;
  case GXin_Alu:
    mapReg(m, &i->GXin.Alu.srcL);
    mapRegs_TILEGXRH(m, i->GXin.Alu.srcR);
    mapReg(m, &i->GXin.Alu.dst);
    return;
  case GXin_CmpI:
    mapReg(m, &i->GXin.CmpI.srcL);
    mapRegs_TILEGXRH(m, i->GXin.CmpI.srcR);
    mapReg(m, &i->GXin.CmpI.dst);
    return;
  case GXin_Shft:
    mapReg(m, &i->GXin.Shft.srcL);
    mapRegs_TILEGXRH(m, i->GXin.Shft.srcR);
    mapReg(m, &i->GXin.Shft.dst);
    return;
  case GXin_Cmp:
    mapReg(m, &i->GXin.Cmp.srcL);
    mapReg(m, &i->GXin.Cmp.srcR);
    mapReg(m, &i->GXin.Cmp.dst);
    return;
  case GXin_Acas:
    mapReg(m, &i->GXin.Acas.old);
    mapReg(m, &i->GXin.Acas.addr);
    mapReg(m, &i->GXin.Acas.new);
    if (i->GXin.Acas.op == GXacas_CMPEXCH)
      mapReg(m, &i->GXin.Acas.exp);
    return;
  case GXin_Bf:
    mapReg(m, &i->GXin.Bf.src);
    mapReg(m, &i->GXin.Bf.dst);
    return;
  case GXin_Unary:
    mapReg(m, &i->GXin.Unary.src);
    mapReg(m, &i->GXin.Unary.dst);
    return;
  case GXin_Mul:
    mapReg(m, &i->GXin.Mul.dst);
    mapReg(m, &i->GXin.Mul.srcL);
    mapReg(m, &i->GXin.Mul.srcR);
    return;
  case GXin_Call:
    {
      if (i->GXin.Call.cond != TILEGXcc_AL)
        mapReg(m, &i->GXin.Call.src);
      return;
    }
  case GXin_XDirect:
    mapRegs_TILEGXAMode(m, i->GXin.XDirect.amPC);
    return;
  case GXin_XIndir:
    mapReg(m, &i->GXin.XIndir.dstGA);
    mapRegs_TILEGXAMode(m, i->GXin.XIndir.amPC);
    return;
  case GXin_XAssisted:
    mapReg(m, &i->GXin.XAssisted.dstGA);
    mapRegs_TILEGXAMode(m, i->GXin.XAssisted.amPC);
    return;
  case GXin_EvCheck:
    mapRegs_TILEGXAMode(m, i->GXin.EvCheck.amCounter);
    mapRegs_TILEGXAMode(m, i->GXin.EvCheck.amFailAddr);
    return;
  case GXin_ProfInc:
    return;
  case GXin_Load:
    mapRegs_TILEGXAMode(m, i->GXin.Load.src);
    mapReg(m, &i->GXin.Load.dst);
    return;
  case GXin_Store:
    mapReg(m, &i->GXin.Store.src);
    mapRegs_TILEGXAMode(m, i->GXin.Store.dst);
    return;
  case GXin_RdWrLR:
    mapReg(m, &i->GXin.RdWrLR.gpr);
    return;
  case GXin_MovCond:
    if (i->GXin.MovCond.srcR->tag == GXrh_Reg) {
      mapReg(m, &(i->GXin.MovCond.srcR->GXrh.Reg.reg));
    }
    mapReg(m, &i->GXin.MovCond.srcL);
    mapReg(m, &i->GXin.MovCond.condR);
    mapReg(m, &i->GXin.MovCond.dst);

    return;
  default:
    vpanic("mapRegs_TILEGXInstr");
  }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing.
*/
Bool isMove_TILEGXInstr ( TILEGXInstr * i, HReg * src, HReg * dst )
{
  /* Moves between integer regs */
  if (i->tag == GXin_Alu) {
    // or Rd,Rs,Rs == mov Rd, Rs
    if (i->GXin.Alu.op != GXalu_OR)
      return False;
    if (i->GXin.Alu.srcR->tag != GXrh_Reg)
      return False;
    if (!sameHReg(i->GXin.Alu.srcR->GXrh.Reg.reg, i->GXin.Alu.srcL))
      return False;
    *src = i->GXin.Alu.srcL;
    *dst = i->GXin.Alu.dst;
    return True;
  }
  return False;
}

/* Generate tilegx spill/reload instructions under the direction of the
   register allocator.
*/
void genSpill_TILEGX ( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2, HReg rreg,
                       Int offsetB )
{
  TILEGXAMode *am;
  vassert(offsetB >= 0);
  vassert(!hregIsVirtual(rreg));
  *i1 = *i2 = NULL;
  am = TILEGXAMode_IR(offsetB, TILEGXGuestStatePointer());

  switch (hregClass(rreg)) {
  case HRcInt64:
    *i1 = TILEGXInstr_Store(8, am, rreg);
    break;
  case HRcInt32:
    *i1 = TILEGXInstr_Store(4, am, rreg);
    break;
  default:
    ppHRegClass(hregClass(rreg));
    vpanic("genSpill_TILEGX: unimplemented regclass");
  }
}

void genReload_TILEGX ( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2, HReg rreg,
                        Int offsetB )
{
  TILEGXAMode *am;
  vassert(!hregIsVirtual(rreg));
  am = TILEGXAMode_IR(offsetB, TILEGXGuestStatePointer());

  switch (hregClass(rreg)) {
  case HRcInt64:
    *i1 = TILEGXInstr_Load(8, rreg, am);
    break;
  case HRcInt32:
    *i1 = TILEGXInstr_Load(4, rreg, am);
    break;
  default:
    ppHRegClass(hregClass(rreg));
    vpanic("genReload_TILEGX: unimplemented regclass");
    break;
  }
}

/* --------- The tilegx assembler --------- */

static UChar *mkInsnBin ( UChar * p, ULong insn )
{
  vassert(insn != (ULong)(-1));
  if (((Addr)p) & 7) {
    vex_printf("p=%p\n", p);
    vassert((((Addr)p) & 7) == 0);
  }
  *((ULong *)(Addr)p) = insn;
  p += 8;
  return p;
}

static Int display_insn ( struct tilegx_decoded_instruction
                          decoded[1] )
{
  Int i;
  for (i = 0;
       decoded[i].opcode && (i < 1);
       i++) {
    Int n;
    vex_printf("%s ", decoded[i].opcode->name);

    for (n = 0; n < decoded[i].opcode->num_operands; n++) {
      const struct tilegx_operand *op = decoded[i].operands[n];

      if (op->type == TILEGX_OP_TYPE_REGISTER)
        vex_printf("r%d", (Int) decoded[i].operand_values[n]);
      else
        vex_printf("%llu", (ULong)decoded[i].operand_values[n]);

      if (n != (decoded[i].opcode->num_operands - 1))
        vex_printf(", ");
    }
    vex_printf(" ");
  }
  return i;
}


Int decode_and_display ( tilegx_bundle_bits *p, Int count, ULong pc )
{
  struct tilegx_decoded_instruction
    decode[TILEGX_MAX_INSTRUCTIONS_PER_BUNDLE];
  Int i;

#ifdef TILEGX_DEBUG
  vex_printf("Insn@0x%lx\n", (ULong)p);
#endif

  if (count > 0x1000) {
    vex_printf("insn count: %d", count);
    vassert(0);
  }

  for (i = 0 ; i < count ; i++) {
    if (pc) {
      vex_printf("%012llx %016llx  ", pc, (ULong)p[i]);
      pc += 8;
    }
    parse_insn_tilegx(p[i], 0, decode);

    Int n, k, bundled = 0;

    for(k = 0; (k < TILEGX_MAX_INSTRUCTIONS_PER_BUNDLE) && decode[k].opcode;
        k++) {
      if (decode[k].opcode->mnemonic != TILEGX_OPC_FNOP)
        bundled++;
    }

    /* Print "{", ";" and "}" only if multiple instructions are bundled. */
    if (bundled > 1)
      vex_printf("{ ");

    n = bundled;
    for(k = 0; (k < TILEGX_MAX_INSTRUCTIONS_PER_BUNDLE) && decode[k].opcode;
        k++) {
      if (decode[k].opcode->mnemonic == TILEGX_OPC_FNOP)
        continue;

      display_insn(&decode[k]);

      if (--n > 0)
        vex_printf("; ");
    }

    if (bundled > 1)
      vex_printf(" }");

    vex_printf("\n");
  }
  return count;
}

static UInt iregNo ( HReg r )
{
  UInt n;
  vassert(hregClass(r) == HRcInt64);
  vassert(!hregIsVirtual(r));
  n = hregEncoding(r);
  vassert(n <= 63);
  return n;
}

static UChar *doAMode_IR ( UChar * p, UInt opc1, UInt rSD, TILEGXAMode * am )
{
  UInt rA;
  vassert(am->tag == GXam_IR);

  rA = iregNo(am->GXam.IR.base);

  if (opc1 == TILEGX_OPC_ST1 || opc1 == TILEGX_OPC_ST2 ||
      opc1 == TILEGX_OPC_ST4 || opc1 == TILEGX_OPC_ST) {
    if ( am->GXam.IR.index ) {
      /* r51 is reserved scratch registers. */
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                    51, rA, am->GXam.IR.index));
      /* store rSD to address in r51 */
      p = mkInsnBin(p, mkTileGxInsn(opc1, 2, 51, rSD));
    } else {
      /* store rSD to address in rA */
      p = mkInsnBin(p, mkTileGxInsn(opc1, 2, rA, rSD));
    }
  } else {
    if ( am->GXam.IR.index ) {
      /* r51 is reserved scratch registers. */
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                    51, rA, am->GXam.IR.index));
      /* load from address in r51 to rSD. */
      p = mkInsnBin(p, mkTileGxInsn(opc1, 2, rSD, 51));
    } else {
      /* load from address in rA to rSD. */
      p = mkInsnBin(p, mkTileGxInsn(opc1, 2, rSD, rA));
    }
  }
  return p;
}

/* Generate a machine-word sized load or store using exact 2 bundles.
   Simplified version of the GXin_Load and GXin_Store cases below. */
static UChar* do_load_or_store_machine_word ( UChar* p, Bool isLoad, UInt reg,
                                              TILEGXAMode* am )
{
  UInt rA = iregNo(am->GXam.IR.base);

  if (am->tag != GXam_IR)
    vpanic(__func__);

  if (isLoad) /* load */ {
     /* r51 is reserved scratch registers. */
     p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
				   51, rA, am->GXam.IR.index));
     /* load from address in r51 to rSD. */
     p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_LD, 2, reg, 51));
  } else /* store */ {
     /* r51 is reserved scratch registers. */
     p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
				   51, rA, am->GXam.IR.index));
     /* store rSD to address in r51 */
     p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ST, 2, 51, reg));
  }
  return p;
}

/* Load imm to r_dst */
static UChar *mkLoadImm ( UChar * p, UInt r_dst, ULong imm )
{
  vassert(r_dst < 0x40);

  if (imm == 0)
  {
    /* A special case, use r63 - zero register. */
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MOVE, 2, r_dst, 63));
  }
  else if (imm >= 0xFFFFFFFFFFFF8000ULL || imm < 0x8000)
  {
    /* only need one 16-bit sign-extendable movli instructon. */
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MOVELI, 2,
                                  r_dst, imm & 0xFFFF));

  }
  else if (imm >= 0xFFFFFFFF80000000ULL || imm < 0x80000000ULL)
  {
    /* Sign-extendable moveli and a shl16insli */
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MOVELI, 2,
                                  r_dst,
                                  (imm >> 16) & 0xFFFF));

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL16INSLI, 3,
                                  r_dst, r_dst,
                                  (imm & 0xFFFF)));

  }
  else
  {
    /* A general slower and rare case, use 4 instructions/bundles:
       moveli     r_dst, imm[63:48]
       shl16insli r_dst, imm[47:32]
       shl16insli r_dst, imm[31:16]
       shl16insli r_dst, imm[15: 0]
    */
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MOVELI, 2,
                                  r_dst,
                                  (imm >> 48) & 0xFFFF));

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL16INSLI, 3,
                                  r_dst, r_dst,
                                  (imm >> 32) & 0xFFFF));

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL16INSLI, 3,
                                  r_dst, r_dst,
                                  (imm >> 16) & 0xFFFF));

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL16INSLI, 3,
                                  r_dst, r_dst,
                                  imm & 0xFFFF));
  }
  return p;
}

/* Load imm to r_dst using exact 4 bundles. A special case of above
   mkLoadImm(...). */
static UChar *mkLoadImm_EXACTLY4 ( UChar * p, UInt r_dst, ULong imm )
{
  p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MOVELI, 2,
                                r_dst,
                                (imm >> 48) & 0xFFFF));

  p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL16INSLI, 3,
                                r_dst, r_dst,
                                (imm >> 32) & 0xFFFF));

  p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL16INSLI, 3,
                                r_dst, r_dst,
                                (imm >> 16) & 0xFFFF));

  p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL16INSLI, 3,
                                r_dst, r_dst,
                                (imm) & 0xFFFF));
  return p;
}

/* Move r_dst to r_src */
static UChar *mkMoveReg ( UChar * p, UInt r_dst, UInt r_src )
{
  vassert(r_dst < 0x40);
  vassert(r_src < 0x40);

  if (r_dst != r_src) {
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MOVE, 2,
                                  r_dst, r_src));
  }
  return p;
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code. */
Int emit_TILEGXInstr ( Bool*  is_profInc,
                       UChar* buf,
                       Int    nbuf,
                       TILEGXInstr* i,
                       Bool   mode64,
                       VexEndness endness_host,
                       void*  disp_cp_chain_me_to_slowEP,
                       void*  disp_cp_chain_me_to_fastEP,
                       void*  disp_cp_xindir,
                       void*  disp_cp_xassisted )
{
  Int instr_bytes = 0;
  UChar *p = &buf[0];
  UChar *ptmp = p;
  vassert(nbuf >= 32);
  vassert(!((Addr)p & 0x7));
  vassert (mode64);

  switch (i->tag) {
  case GXin_MovCond: {

    TILEGXRH *srcR = i->GXin.MovCond.srcR;
    UInt condR = iregNo(i->GXin.MovCond.condR);
    UInt dst = iregNo(i->GXin.MovCond.dst);

    UInt srcL = iregNo(i->GXin.MovCond.srcL);

    if (i->GXin.MovCond.cond == TILEGXcc_EZ) {
      if (srcR->tag == GXrh_Reg) {
        p = mkMoveReg(p, dst, iregNo(srcR->GXrh.Reg.reg));
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMOVEQZ, 3,
                                      dst, condR, srcL));
      } else {
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MOVELI, 2,
                                      dst, srcR->GXrh.Imm.imm16));
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMOVEQZ, 3,
                                      dst, condR, srcL));
      }
    } else {
      vassert(0);
    }

    goto done;
  }
  case GXin_LI:

    // Tilegx, load literal
    p = mkLoadImm(p, iregNo(i->GXin.LI.dst), i->GXin.LI.imm);
    goto done;

  case GXin_Alu: {
    TILEGXRH *srcR = i->GXin.Alu.srcR;
    Bool immR = toBool(srcR->tag == GXrh_Imm);
    UInt r_dst = iregNo(i->GXin.Alu.dst);
    UInt r_srcL = iregNo(i->GXin.Alu.srcL);
    UInt r_srcR = immR ? (-1) /*bogus */ : iregNo(srcR->GXrh.Reg.reg);

    switch (i->GXin.Alu.op) {
      /*GXalu_ADD, GXalu_SUB, GXalu_AND, GXalu_OR, GXalu_NOR, GXalu_XOR */
    case GXalu_ADD:
      if (immR) {
        vassert(srcR->GXrh.Imm.imm16 != 0x8000);
        if (srcR->GXrh.Imm.syned)
          /* addi */
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                        r_dst, r_srcL,
                                        srcR->GXrh.Imm.imm16));
        else
          /* addiu, use shil16insli for tilegx  */
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL16INSLI, 3,
                                        r_dst, 63,
                                        srcR->GXrh.Imm.imm16));
      } else {
        /* addu */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADD, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      }
      break;
    case GXalu_SUB:
      if (immR) {
        /* addi , but with negated imm */
        vassert(srcR->GXrh.Imm.syned);
        vassert(srcR->GXrh.Imm.imm16 != 0x8000);
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                      r_dst, r_srcL,
                                      -srcR->GXrh.Imm.imm16));
      } else {
        /* subu */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SUB, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      }
      break;
    case GXalu_AND:
      if (immR) {
        /* andi */
        vassert((srcR->GXrh.Imm.imm16 >> 8 == 0) ||
                (srcR->GXrh.Imm.imm16 >> 8 == 0xFF));

        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ANDI, 3,
                                      r_dst, r_srcL,
                                      srcR->GXrh.Imm.imm16));

      } else {
        /* and */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_AND, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      }
      break;
    case GXalu_OR:
      if (immR) {
        /* ori */
        vassert((srcR->GXrh.Imm.imm16 >> 8 == 0) ||
                (srcR->GXrh.Imm.imm16 >> 8 == 0xFF));

        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ORI, 3,
                                      r_dst, r_srcL,
                                      srcR->GXrh.Imm.imm16));
      } else {
        /* or */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_OR, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      }
      break;
    case GXalu_NOR:
      /* nor */
      vassert(!immR);
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_NOR, 3,
                                    r_dst, r_srcL,
                                    r_srcR));
      break;
    case GXalu_XOR:
      if (immR) {
        /* xori */
        vassert(srcR->GXrh.Imm.syned);
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_XORI, 3,
                                      r_dst, r_srcL,
                                      srcR->GXrh.Imm.imm16));
      } else {
        /* xor */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_XOR, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      }
      break;

    default:
      goto bad;
    }
    goto done;
  }

  case GXin_Shft: {
    TILEGXRH *srcR = i->GXin.Shft.srcR;
    Bool sz32 = i->GXin.Shft.sz32;
    Bool immR = toBool(srcR->tag == GXrh_Imm);
    UInt r_dst = iregNo(i->GXin.Shft.dst);
    UInt r_srcL = iregNo(i->GXin.Shft.srcL);
    UInt r_srcR = immR ? (-1) /*bogus */ : iregNo(srcR->GXrh.Reg.reg);

    switch (i->GXin.Shft.op) {
    case GXshft_SLL:
      if (sz32) {
        if (immR) {
          UInt n = srcR->GXrh.Imm.imm16;
          vassert(n >= 0 && n < 64);
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHLXI, 3,
                                        r_dst, r_srcL,
                                        srcR->GXrh.Imm.imm16));
        } else {
          /* shift variable */
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHLX, 3,
                                        r_dst, r_srcL,
                                        r_srcR));
        }
      } else {
        if (immR) {
          UInt n = srcR->GXrh.Imm.imm16;
          vassert(n >= 0 && n < 64);
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHLI, 3,
                                        r_dst, r_srcL,
                                        srcR->GXrh.Imm.imm16));
        } else {
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHL, 3,
                                        r_dst, r_srcL,
                                        r_srcR));
        }
      }
      break;

    case GXshft_SLL8x8:
      if (immR) {
        UInt n = srcR->GXrh.Imm.imm16;
        vassert(n >= 0 && n < 64);
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_V1SHLI, 3,
                                      r_dst, r_srcL,
                                      srcR->GXrh.Imm.imm16));
      } else {
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_V1SHL, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      }
      break;

    case GXshft_SRL8x8:
      if (immR) {
        UInt n = srcR->GXrh.Imm.imm16;
        vassert(n >= 0 && n < 64);
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_V1SHRUI, 3,
                                      r_dst, r_srcL,
                                      srcR->GXrh.Imm.imm16));
      } else {
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_V1SHRU, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      }
      break;

    case GXshft_SRL:
      if (sz32) {
        // SRL, SRLV
        if (immR) {
          UInt n = srcR->GXrh.Imm.imm16;
          vassert(n >= 0 && n < 32);
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHRUXI, 3,
                                        r_dst, r_srcL,
                                        srcR->GXrh.Imm.imm16));
        } else {
          /* shift variable */
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHRUX, 3,
                                        r_dst, r_srcL,
                                        r_srcR));
        }
      } else {
        // DSRL, DSRL32, DSRLV
        if (immR) {
          UInt n = srcR->GXrh.Imm.imm16;
          vassert((n >= 0 && n < 64));
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHRUI, 3,
                                        r_dst, r_srcL,
                                        srcR->GXrh.Imm.imm16));
        } else {
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHRU, 3,
                                        r_dst, r_srcL,
                                        r_srcR));
        }
      }
      break;

    case GXshft_SRA:
      if (sz32) {
        // SRA, SRAV
        if (immR) {
          UInt n = srcR->GXrh.Imm.imm16;
          vassert(n >= 0 && n < 64);
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHRSI, 3,
                                        r_dst, r_srcL,
                                        srcR->GXrh.Imm.imm16));

        } else {
          /* shift variable */
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHRS, 3,
                                        r_dst, r_srcL,
                                        r_srcR));
        }
      } else {
        // DSRA, DSRA32, DSRAV
        if (immR) {

          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHRSI, 3,
                                        r_dst, r_srcL,
                                        srcR->GXrh.Imm.imm16));
        } else {
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_SHRS, 3,
                                        r_dst, r_srcL,
                                        r_srcR));
        }
      }
      break;

    default:
      goto bad;
    }

    goto done;
  }

  case GXin_Unary: {
    UInt r_dst = iregNo(i->GXin.Unary.dst);
    UInt r_src = iregNo(i->GXin.Unary.src);

    switch (i->GXin.Unary.op) {
      /* GXun_CLZ, GXun_NOP */
    case GXun_CLZ:  //clz

      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CLZ, 2,
                                    r_dst, r_src));
      break;
    case GXun_CTZ:  //ctz

      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CTZ, 2,
                                    r_dst, r_src));
      break;

    case GXun_NOP:
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_NOP, 0));
      break;
    }
    goto done;
  }

  case GXin_Cmp: {

    Bool syned = i->GXin.Cmp.syned;
    UInt r_srcL = iregNo(i->GXin.Cmp.srcL);
    UInt r_srcR = iregNo(i->GXin.Cmp.srcR);
    UInt r_dst = iregNo(i->GXin.Cmp.dst);

    switch (i->GXin.Cmp.cond) {
    case TILEGXcc_EQ:

      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPEQ, 3,
                                    r_dst, r_srcL,
                                    r_srcR));

      break;

    case TILEGXcc_NE:
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPNE, 3,
                                    r_dst, r_srcL,
                                    r_srcR));

      break;
    case TILEGXcc_LT:
      /*  slt r_dst, r_srcL, r_srcR */

      if (syned)
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPLTS, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      else
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPLTU, 3,
                                      r_dst, r_srcL,
                                      r_srcR));

      break;
    case TILEGXcc_LO:
      /*  sltu r_dst, r_srcL, r_srcR */

      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPLTU, 3,
                                    r_dst, r_srcL,
                                    r_srcR));

      break;
    case TILEGXcc_LE:
      if (syned)
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPLES, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      else
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPLEU, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      break;
    case TILEGXcc_LS:

      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPLTU, 3,
                                    r_dst, r_srcL,
                                    r_srcR));
      break;
    default:
      goto bad;
    }
    goto done;
  }

  case GXin_CmpI: {

    TILEGXRH *srcR = i->GXin.CmpI.srcR;
    Bool immR = toBool(srcR->tag == GXrh_Imm);
    UInt r_dst = iregNo(i->GXin.CmpI.dst);
    UInt r_srcL = iregNo(i->GXin.CmpI.srcL);
    UInt r_srcR = immR ? (-1) /*bogus */ : iregNo(srcR->GXrh.Reg.reg);

    switch (i->GXin.CmpI.cond) {
    case TILEGXcc_EQ8x8:
      if (immR) {
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_V1CMPEQI, 3,
                                      r_dst, r_srcL,
                                      srcR->GXrh.Imm.imm16));
      } else {
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_V1CMPEQ, 3,
                                      r_dst, r_srcL,
                                      r_srcR));
      }
      break;

    case TILEGXcc_NE8x8:
      if (immR) {
        vassert(0);
      } else {
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_V1CMPNE, 3,
                                      r_dst, r_srcR,
                                      r_srcL));
      }
      break;
    default:
      vassert(0);
    }
    goto done;
    break;
  }

  case GXin_Bf: {

    /* Bit Field */
    UInt r_dst = iregNo(i->GXin.Bf.dst);
    UInt r_src = iregNo(i->GXin.Bf.src);
    UInt Start = i->GXin.Bf.Start;
    UInt End   = i->GXin.Bf.End;

    switch (i->GXin.Bf.op) {
    case GXbf_EXTS:
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_BFEXTS, 4,
                                    r_dst, r_src,
                                    Start, End));

      break;
    case GXbf_EXTU:
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_BFEXTU, 4,
                                    r_dst, r_src,
                                    Start, End));

      break;
    case GXbf_INS:
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_BFINS, 4,
                                    r_dst, r_src,
                                    Start, End));

      break;
    default:
      vassert(0);
    }
    goto done;
    break;
  }

  case GXin_Acas: {

    /* Atomic */
    UInt sz =  i->GXin.Acas.sz;
    UInt old = iregNo(i->GXin.Acas.old);
    UInt addr= iregNo(i->GXin.Acas.addr);
    UInt new = iregNo(i->GXin.Acas.new);

    switch (i->GXin.Acas.op) {
    case GXacas_CMPEXCH:
      {
        UInt exp = iregNo(i->GXin.Acas.exp);
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MTSPR, 2,
                                      0x2780, exp));
        if (sz == 8)
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPEXCH, 3,
                                        old, addr, new));
        else
          p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_CMPEXCH4, 3,
                                        old, addr, new));
      }
      break;

    case GXacas_EXCH:
      if (sz == 8)
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_EXCH, 3,
                                      old, addr, new));
      else
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_EXCH4, 3,
                                      old, addr, new));
      break;

    case GXacas_FetchAnd:
      if (sz == 8)
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_FETCHAND, 3,
                                      old, addr, new));
      else
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_FETCHAND4, 3,
                                      old, addr, new));
      break;

    case GXacas_FetchAdd:
      if (sz == 8)
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_FETCHADD, 3,
                                      old, addr, new));
      else
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_FETCHADD4, 3,
                                      old, addr, new));
      break;

    case GXacas_FetchAddgez:
      if (sz == 8)
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_FETCHADDGEZ, 3,
                                      old, addr, new));
      else
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_FETCHADDGEZ4, 3,
                                      old, addr, new));
      break;

    case GXacas_FetchOr:
      if (sz == 8)
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_FETCHOR, 3,
                                      old, addr, new));
      else
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_FETCHOR4, 3,
                                      old, addr, new));
      break;

    default: vassert(0);
    }
    goto done;
    break;
  }

  case GXin_Mul: {

    /* Multiplication */
    Bool syned = i->GXin.Mul.syned;
    Bool widening = i->GXin.Mul.widening;
    Bool sz32 = i->GXin.Mul.sz32;
    UInt r_srcL = iregNo(i->GXin.Mul.srcL);
    UInt r_srcR = iregNo(i->GXin.Mul.srcR);
    UInt r_dst = iregNo(i->GXin.Mul.dst);

    vassert(widening);  // always widen.
    vassert(!sz32);   // always be 64 bits.

    if (syned) {
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MUL_LS_LS, 3,
                                    r_dst, r_srcL, r_srcR));
    } else {
      p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MUL_LU_LU, 3,
                                    r_dst, r_srcL, r_srcR));
    }
    goto done;
  }

  case GXin_Call: {

    /* Function Call. */
    TILEGXCondCode cond = i->GXin.Call.cond;
    UInt r_dst = 11;  /* using r11 as address temporary */

    /* jump over the following insns if conditional. */
    if (cond != TILEGXcc_AL) {
      /* jmp fwds if !condition */
      /* don't know how many bytes to jump over yet...
         make space for a jump instruction + nop!!! and fill in later. */
      ptmp = p;   /* fill in this bit later */
      p += 8;
    }

    /* load target to r_dst */
    p = mkLoadImm(p, r_dst, i->GXin.Call.target);

    /* jalr %r_dst */
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_JALRP, 1,
                                  r_dst));

    /* Fix up the conditional jump, if there was one. */
    if (cond != TILEGXcc_AL) {
      UInt r_src = iregNo(i->GXin.Call.src);
      Int delta = p - ptmp;

      vassert(cond == TILEGXcc_EQ);

      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_BEQZ, 2,
                                          r_src, delta / 8));
   }
    goto done;
  }

  case GXin_XDirect: {
    /* NB: what goes on here has to be very closely coordinated
       with the chainXDirect_TILEGX and unchainXDirect_TILEGX below. */
    /* We're generating chain-me requests here, so we need to be
       sure this is actually allowed -- no-redir translations
       can't use chain-me's.  Hence: */
    vassert(disp_cp_chain_me_to_slowEP != NULL);
    vassert(disp_cp_chain_me_to_fastEP != NULL);

    /* Use ptmp for backpatching conditional jumps. */
    ptmp = NULL;

    /* First, if this is conditional, create a conditional
       jump over the rest of it.  Or at least, leave a space for
       it that we will shortly fill in. */
    if (i->GXin.XDirect.cond != TILEGXcc_AL) {
      vassert(i->GXin.XDirect.cond != TILEGXcc_NV);
      ptmp = p;
      p += 24;
    }

    /* Update the guest PC. */
    /* move r11, dstGA */
    /* st   amPC, r11  */
    p = mkLoadImm_EXACTLY4(p, /*r*/ 11, (ULong)i->GXin.XDirect.dstGA);

    p = do_load_or_store_machine_word(p, False /*!isLoad*/ , /*r*/ 11,
                                      i->GXin.XDirect.amPC);

    /* --- FIRST PATCHABLE BYTE follows --- */
    /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're
       calling to) backs up the return address, so as to find the
       address of the first patchable byte.  So: don't change the
       number of instructions (3) below. */
    /* move r9, VG_(disp_cp_chain_me_to_{slowEP,fastEP}) */
    /* jr  r11  */
    void* disp_cp_chain_me
      = i->GXin.XDirect.toFastEP ? disp_cp_chain_me_to_fastEP
      : disp_cp_chain_me_to_slowEP;
    p = mkLoadImm_EXACTLY4(p, /*r*/ 11,
                           (Addr)disp_cp_chain_me);
    /* jalr r11 */
    /* nop */
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_JALR, 1, 11));

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_NOP, 0));

    /* --- END of PATCHABLE BYTES --- */

    /* Fix up the conditional jump, if there was one. */
    if (i->GXin.XDirect.cond != TILEGXcc_AL) {
      Int delta = p - ptmp;
      delta = delta / 8 - 3;

      /* ld r11, COND_OFFSET(GuestSP=r50)
         beqz r11, delta
      */
      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                          11, 50, COND_OFFSET()));
      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_LD, 2,
                                          11, 11));

      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_BEQZ, 2,
                                          11, delta));

    }
    goto done;
  }

  case GXin_XIndir: {
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
    if (i->GXin.XIndir.cond != TILEGXcc_AL) {
      vassert(i->GXin.XIndir.cond != TILEGXcc_NV);
      ptmp = p;
      p += 24;
    }

    /* Update the guest PC. */
    /* st amPC, dstGA */
    p = do_load_or_store_machine_word(p, False /*!isLoad*/ ,
                                      iregNo(i->GXin.XIndir.dstGA),
                                      i->GXin.XIndir.amPC);

    /* move r11, VG_(disp_cp_xindir), 4 bundles. */
    /* jalr r11 */
    /* nop */
    p = mkLoadImm_EXACTLY4(p, /*r*/ 11,
                           (Addr)disp_cp_xindir);

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_JALR, 1, 11));

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_NOP, 0));

    /* Fix up the conditional jump, if there was one. */
    if (i->GXin.XIndir.cond != TILEGXcc_AL) {
      Int delta = p - ptmp;
      delta = delta / 8 - 3;
      vassert(delta > 0 && delta < 40);

      /* ld r11, COND_OFFSET($GuestSP)
         beqz r11, delta  */

      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                          11, 50, COND_OFFSET()));
      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_LD, 2,
                                          11, 11));
      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_BEQZ, 2,
                                          11, delta));
    }
    goto done;
  }

  case GXin_XAssisted: {
    /* First off, if this is conditional, create a conditional jump
       over the rest of it.  Or at least, leave a space for it that
       we will shortly fill in. */
    ptmp = NULL;
    if (i->GXin.XAssisted.cond != TILEGXcc_AL) {
      vassert(i->GXin.XAssisted.cond != TILEGXcc_NV);
      ptmp = p;
      p += 24;
    }

    /* Update the guest PC. */
    /* st amPC, dstGA */
    p = do_load_or_store_machine_word(p, False /*!isLoad*/ ,
                                      iregNo(i->GXin.XIndir.dstGA),
                                      i->GXin.XIndir.amPC);

    UInt trcval = 0;
    switch (i->GXin.XAssisted.jk) {
    case Ijk_ClientReq:     trcval = VEX_TRC_JMP_CLIENTREQ;     break;
    case Ijk_Sys_syscall:   trcval = VEX_TRC_JMP_SYS_SYSCALL;   break;
    case Ijk_Yield:         trcval = VEX_TRC_JMP_YIELD;         break;
    case Ijk_EmWarn:        trcval = VEX_TRC_JMP_EMWARN;        break;
    case Ijk_EmFail:        trcval = VEX_TRC_JMP_EMFAIL;        break;
    case Ijk_NoDecode:      trcval = VEX_TRC_JMP_NODECODE;      break;
    case Ijk_InvalICache:   trcval = VEX_TRC_JMP_INVALICACHE;   break;
    case Ijk_NoRedir:       trcval = VEX_TRC_JMP_NOREDIR;       break;
    case Ijk_SigILL:        trcval = VEX_TRC_JMP_SIGILL;        break;
    case Ijk_SigTRAP:       trcval = VEX_TRC_JMP_SIGTRAP;       break;
    case Ijk_SigBUS:        trcval = VEX_TRC_JMP_SIGBUS;        break;
    case Ijk_SigFPE_IntDiv: trcval = VEX_TRC_JMP_SIGFPE_INTDIV; break;
    case Ijk_SigFPE_IntOvf: trcval = VEX_TRC_JMP_SIGFPE_INTOVF; break;
    case Ijk_Boring:        trcval = VEX_TRC_JMP_BORING;        break;
    case Ijk_Ret:
      {
        /* Tilegx "iret" instruction. */
        trcval = VEX_TRC_JMP_BORING;
        /* Interrupt return "iret", setup the jump address into EX_CONTRXT_0_0.
           Read context_0_1 from guest_state */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                      51, 50, OFFSET_EX1));
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_LD, 2,
                                      11, 51));
        /* Write into host cpu's context_0_1 spr. */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MTSPR, 2,
                                      0x2581, 11));
        /* Read context_0_0 from guest_state */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                      51, 50, OFFSET_EX0));
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_LD, 2,
                                      11, 51));
        /* Write into host cpu's context_0_0 spr */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_MTSPR, 2,
                                      0x2580, 11));
        /* Update the guest PC  so branch to the iret target address
           in EX_CONTEXT_0. */
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                      51, 50, 512));
        p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ST, 2,
                                      51, 11));
      }
      break;
      /* We don't expect to see the following being assisted.
         case Ijk_Call:
         fallthrough */
    default:
      ppIRJumpKind(i->GXin.XAssisted.jk);
      vpanic("emit_TILEGXInstr.GXin_XAssisted: unexpected jump kind");
    }
    vassert(trcval != 0);

    /* moveli r50, trcval */

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDLI, 3, 50, 63, trcval));

    /* move r11, VG_(disp_cp_xassisted) */

    p = mkLoadImm_EXACTLY4(p, /*r*/ 11,
                           (Addr)disp_cp_xassisted);
    /* jalr r11
       nop  */

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_JALR, 1, 11));
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_NOP, 0));

    /* Fix up the conditional jump, if there was one. */
    if (i->GXin.XAssisted.cond != TILEGXcc_AL) {
      Int delta = p - ptmp;
      delta = delta / 8 - 3;
      vassert(delta > 0 && delta < 40);

      /* ld  r11, COND_OFFSET($GuestSP)
         beqz r11, delta
         nop  */

      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_ADDLI, 3,
                                          11, 50, COND_OFFSET()));
      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_LD, 2,
                                          11, 11));
      ptmp = mkInsnBin(ptmp, mkTileGxInsn(TILEGX_OPC_BEQZ, 2,
                                          11, delta));
    }
    goto done;
  }

  case GXin_EvCheck: {
    /* We generate:
       ld      r11, amCounter
       addi    r11, r11, -1
       st      amCounter, r11
       bgez    r11, nofail
       ld      r11, amFailAddr
       jalr    r11
       nop
       nofail:
    */
    UChar* p0 = p;
    /* ld  r11, amCounter */
    p = do_load_or_store_machine_word(p, True /*isLoad*/ , /*r*/ 11,
                                      i->GXin.EvCheck.amCounter);

    /* addi r11,r11,-1 */

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDI, 3,
                                  11, 11, -1));

    /* st amCounter, 11 */
    p = do_load_or_store_machine_word(p, False /*!isLoad*/ , /*r*/ 11,
                                      i->GXin.EvCheck.amCounter);

    /* Reserve a bundle, fill it after the do_load_or_store_machine_word.
       since we are not sure how many bundles it takes. */
    UChar* p1 = p;
    p += 8;
    /* bgez t9, nofail */

    /* lw/ld r9, amFailAddr */
    p = do_load_or_store_machine_word(p, True /*isLoad*/ , /*r*/ 11,
                                      i->GXin.EvCheck.amFailAddr);

    mkInsnBin(p1, mkTileGxInsn(TILEGX_OPC_BGEZ, 2,
                               11, 2 + (p - p1) / 8));

    /* jalr r11 */

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_JALR, 1, 11));

    /* nop */
    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_NOP, 0));

    /* nofail: */

    /* Crosscheck */
    vassert(evCheckSzB_TILEGX() == (UChar*)p - (UChar*)p0);
    goto done;
  }

  case GXin_ProfInc: {
    /* Generate a code template to increment a memory location whose
       address will be known later as an immediate value. This code
       template will be patched once the memory location is known.
       For now we do this with address == 0x65556555. */
    /* 64-bit:
       move r11, 0x6555655565556555ULL
       ld r51, r11
       addi r51, r51, 1
       st  r11, r51
    */

    /* move r11, 0x6555655565556555ULL */
    p = mkLoadImm_EXACTLY4(p, /*r*/ 11, 0x6555655565556555ULL);

    /* ld r51, r11 */

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_LD, 2, 51, 11));

    /* addi r51, r51, 1 */

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ADDI, 3, 51, 51, 1));

    /* st r11, r51 */

    p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_ST, 2, 11, 51));

    /* Tell the caller .. */
    vassert(!(*is_profInc));
    *is_profInc = True;
    goto done;
  }

  case GXin_Load: {
    TILEGXAMode *am_addr = i->GXin.Load.src;
    if (am_addr->tag == GXam_IR) {
      UInt r_dst = iregNo(i->GXin.Load.dst);
      UInt opc, sz = i->GXin.Load.sz;
      if ((sz == 4 || sz == 8)) {
        /* should be guaranteed to us by iselWordExpr_AMode */
        vassert(0 == (am_addr->GXam.IR.index & 3));
      }

      // Note: Valgrind memory load has no sign-extend. We extend explicitly.
      switch (sz) {
      case 1:
        opc = TILEGX_OPC_LD1U;
        break;
      case 2:
        opc = TILEGX_OPC_LD2U;
        break;
      case 4:
        opc = TILEGX_OPC_LD4U;
        break;
      case 8:
        opc = TILEGX_OPC_LD;
        break;
      default:
        goto bad;
      }

      p = doAMode_IR(p, opc, r_dst, am_addr);
      goto done;

    }
  }

  case GXin_Store: {
    TILEGXAMode *am_addr = i->GXin.Store.dst;
    if (am_addr->tag == GXam_IR) {
      UInt r_src = iregNo(i->GXin.Store.src);
      UInt opc, sz = i->GXin.Store.sz;
      switch (sz) {
      case 1:
        opc = TILEGX_OPC_ST1;
        break;
      case 2:
        opc = TILEGX_OPC_ST2;
        break;
      case 4:
        opc = TILEGX_OPC_ST4;
        break;
      case 8:
        opc = TILEGX_OPC_ST;
        break;
      default:
        goto bad;
      }

      p = doAMode_IR(p, opc, r_src, am_addr);
      goto done;
    } else {
      vassert(0);
    }
  }

  case GXin_RdWrLR: {
    UInt reg = iregNo(i->GXin.RdWrLR.gpr);
    Bool wrLR = i->GXin.RdWrLR.wrLR;
    if (wrLR)
      p = mkMoveReg(p, 55, reg);
    else
      p = mkMoveReg(p, reg, 55);
    goto done;
  }

  default:
    goto bad;
  }

 bad:
  vex_printf("\n=> ");
  vpanic("emit_TILEGXInstr");
  /*NOTREACHED*/

 done:
  instr_bytes = p - &buf[0];
  /* Instr byte count must be modular of 8. */
  vassert(0 == (instr_bytes & 0x7));

  if (  0) {
    Int k;
    for (k = 0; k < instr_bytes; k += 8)
      decode_and_display((ULong *)(Addr)&buf[k], 1, 0);
  }

  /* Limit the JIT size. */
  vassert(instr_bytes <= 256);
  return instr_bytes;
}


Int evCheckSzB_TILEGX ( void )
{
  UInt kInstrSize = 8;
  return 10*kInstrSize;
}

VexInvalRange chainXDirect_TILEGX ( VexEndness endness_host,
                                    void* place_to_chain,
                                    const void* disp_cp_chain_me_EXPECTED,
                                    const void* place_to_jump_to,
                                    Bool  mode64 )
{
  vassert(mode64);
  vassert(endness_host == VexEndnessLE);
  /* What we're expecting to see is:
     move r11, disp_cp_chain_me_to_EXPECTED
     jalr r11
     nop
     viz
     <32 bytes generated by mkLoadImm_EXACTLY4>
     jalr r11
     nop
  */
  UChar* p = (UChar*)place_to_chain;
  vassert(0 == (7 & (HWord)p));

#ifdef TILEGX_DEBUG
  vex_printf("chainXDirect_TILEGX: disp_cp_chain_me_EXPECTED=%p\n",
             disp_cp_chain_me_EXPECTED);
  decode_and_display(p, 6, p);

  vex_printf("chainXDirect_TILEGX: place_to_jump_to=%p\n",
             place_to_jump_to);
#endif

  /* And what we want to change it to is either:
     move r11, place_to_jump_to
     jalr r11
     nop
     viz
     <32 bytes generated by mkLoadImm_EXACTLY4>
     jalr r11
     nop

     The replacement has the same length as the original.
  */

  p = mkLoadImm_EXACTLY4(p, /*r*/ 11,
                         (Addr)place_to_jump_to);


  p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_JALR, 1, 11));

  p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_NOP, 0));

#ifdef TILEGX_DEBUG
  decode_and_display((UChar*)place_to_chain, 8, place_to_chain);
#endif

  Int len = p - (UChar*)place_to_chain;
  vassert(len == 48); /* stay sane */
  VexInvalRange vir = {(HWord)place_to_chain, len};
  return vir;
}

VexInvalRange unchainXDirect_TILEGX ( VexEndness endness_host,
                                      void* place_to_unchain,
                                      const void* place_to_jump_to_EXPECTED,
                                      const void* disp_cp_chain_me,
                                      Bool  mode64 )
{
  vassert(mode64);
  vassert(endness_host == VexEndnessLE);
  /* What we're expecting to see is:
     move r11, place_to_jump_to_EXPECTED
     jalr r11
     nop
     viz
     <32 bytes generated by mkLoadImm_EXACTLY4>
     jalr r11
     nop
  */
  UChar* p = (UChar*)place_to_unchain;
  vassert(0 == (7 & (HWord)p));

  /* And what we want to change it to is:
     move r11, disp_cp_chain_me
     jalr r11
     nop
     viz
     <32 bytes generated by mkLoadImm_EXACTLY4>
     jalr r11
     nop
     The replacement has the same length as the original.
  */
  p = mkLoadImm_EXACTLY4(p, /*r*/ 11,
                         (Addr)disp_cp_chain_me);


  p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_JALR, 1, 11));

  p = mkInsnBin(p, mkTileGxInsn(TILEGX_OPC_NOP, 0));

  Int len = p - (UChar*)place_to_unchain;
  vassert(len == 48); /* stay sane */
  VexInvalRange vir = {(HWord)place_to_unchain, len};
  return vir;
}

/* Patch the counter address into a profile inc point, as previously
   created by the GXin_ProfInc case for emit_TILEGXInstr. */
VexInvalRange patchProfInc_TILEGX ( VexEndness endness_host,
                                    void*  place_to_patch,
                                    const ULong* location_of_counter,
                                    Bool mode64 )
{
  vassert(mode64);
  vassert(endness_host == VexEndnessLE);
  UChar* p = (UChar*)place_to_patch;
  vassert(0 == (7 & (HWord)p));

  p = mkLoadImm_EXACTLY4(p, /*r*/ 11,
                         (Addr)location_of_counter);

  VexInvalRange vir = {(HWord)p, 32};
  return vir;
}

/*---------------------------------------------------------------*/
/*--- end                                    host_tilegx_defs.c ---*/
/*---------------------------------------------------------------*/
