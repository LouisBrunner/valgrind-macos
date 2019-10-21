
/*--------------------------------------------------------------------*/
/*--- begin                                  guest_nanomips_toIR.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2018 RT-RK
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

/* Translates nanoMIPS code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_mips32.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_nanomips_defs.h"

#define P16 0x4

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

#define OFFB_PC offsetof(VexGuestMIPS32State, guest_PC)

#define ILLEGAL_INSTRUCTON          \
   dres->jk_StopHere = Ijk_SigILL;  \
   dres->whatNext    = Dis_StopHere;

#define LLADDR_INVALID (mkU32(0xFFFFFFFF))

/* MOD: The IRSB* into which we're generating code. */
static IRSB *irsb;

/* CONST: The guest address for the instruction currently being
   translated. */
static Addr32 guest_PC_curr_instr;

/* Do a endian load of a 16-bit word, regardless of the endianness of the
   underlying host. */
static inline UShort getUShort(const UChar * p)
{
   UShort w = 0;
#if defined (_MIPSEL)
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
#elif defined (_MIPSEB)
   w = (w << 8) | p[0];
   w = (w << 8) | p[1];
#endif
   return w;
}

/* Do a endian load of a 32-bit code word. */
static inline UInt getUInt(const UChar * p)
{
   return (getUShort(p) << 16) | getUShort(p + 2);
}

const UChar GPR3_list[] = { 16, 17, 18, 19, 4, 5, 6, 7 };
const UChar GPR4_list[] = { 8, 9, 10, 11, 4, 5, 6, 7, 16, 17, 18, 19,
                            20, 21, 22, 23
                          };
const UChar GPR4_zero_list[] = { 8, 9, 10, 0, 4, 5, 6, 7, 16, 17, 18,
                                 19, 20, 21, 22, 23
                               };
const UChar GPR3_src_store_list[] = { 0, 17, 18, 19, 4, 5, 6, 7 };
const UChar GPR2_reg1_list[] = { 4, 5, 6, 7 };
const UChar GPR2_reg2_list[] = { 5, 6, 7, 8 };

static UInt integerGuestRegOffset(UInt iregNo)
{
   /* Maybe we should use formula ??? */
   switch (iregNo) {
      case 0:
         return offsetof(VexGuestMIPS32State, guest_r0);

      case 1:
         return offsetof(VexGuestMIPS32State, guest_r1);

      case 2:
         return offsetof(VexGuestMIPS32State, guest_r2);

      case 3:
         return offsetof(VexGuestMIPS32State, guest_r3);

      case 4:
         return offsetof(VexGuestMIPS32State, guest_r4);

      case 5:
         return offsetof(VexGuestMIPS32State, guest_r5);

      case 6:
         return offsetof(VexGuestMIPS32State, guest_r6);

      case 7:
         return offsetof(VexGuestMIPS32State, guest_r7);

      case 8:
         return offsetof(VexGuestMIPS32State, guest_r8);

      case 9:
         return offsetof(VexGuestMIPS32State, guest_r9);

      case 10:
         return offsetof(VexGuestMIPS32State, guest_r10);

      case 11:
         return offsetof(VexGuestMIPS32State, guest_r11);

      case 12:
         return offsetof(VexGuestMIPS32State, guest_r12);

      case 13:
         return offsetof(VexGuestMIPS32State, guest_r13);

      case 14:
         return offsetof(VexGuestMIPS32State, guest_r14);

      case 15:
         return offsetof(VexGuestMIPS32State, guest_r15);

      case 16:
         return offsetof(VexGuestMIPS32State, guest_r16);

      case 17:
         return offsetof(VexGuestMIPS32State, guest_r17);

      case 18:
         return offsetof(VexGuestMIPS32State, guest_r18);

      case 19:
         return offsetof(VexGuestMIPS32State, guest_r19);

      case 20:
         return offsetof(VexGuestMIPS32State, guest_r20);

      case 21:
         return offsetof(VexGuestMIPS32State, guest_r21);

      case 22:
         return offsetof(VexGuestMIPS32State, guest_r22);

      case 23:
         return offsetof(VexGuestMIPS32State, guest_r23);

      case 24:
         return offsetof(VexGuestMIPS32State, guest_r24);

      case 25:
         return offsetof(VexGuestMIPS32State, guest_r25);

      case 26:
         return offsetof(VexGuestMIPS32State, guest_r26);

      case 27:
         return offsetof(VexGuestMIPS32State, guest_r27);

      case 28:
         return offsetof(VexGuestMIPS32State, guest_r28);

      case 29:
         return offsetof(VexGuestMIPS32State, guest_r29);

      case 30:
         return offsetof(VexGuestMIPS32State, guest_r30);

      case 31:
         return offsetof(VexGuestMIPS32State, guest_r31);
   }

   vassert(0);
   return 0;
}

/* Add a statement to the list held by "irsb". */
static void stmt(IRStmt * st)
{
   addStmtToIRSB(irsb, st);
}

static IRExpr *mkU8(UInt i)
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8((UChar) i));
}

/* Create an expression node for a 32-bit integer constant. */
static IRExpr *mkU32(UInt i)
{
   return IRExpr_Const(IRConst_U32(i));
}

static IRExpr *mkU64(ULong i)
{
   return IRExpr_Const(IRConst_U64(i));
}

static void putPC(IRExpr * e)
{
   stmt(IRStmt_Put(OFFB_PC, e));
}

static void putIReg(UInt archreg, IRExpr * e)
{
   vassert(archreg < 32);

   if (archreg != 0)
      stmt(IRStmt_Put(integerGuestRegOffset(archreg), e));
}

static IRExpr *getIReg(UInt iregNo)
{
   if (0 == iregNo) {
      return mkU32(0x0);
   } else {
      IRType ty = Ity_I32;
      vassert(iregNo < 32);
      return IRExpr_Get(integerGuestRegOffset(iregNo), ty);
   }
}

static void putLLaddr(IRExpr * e)
{
   stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_LLaddr), e));
}

static IRExpr *getLLaddr(void)
{
   return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_LLaddr), Ity_I32);
}

static void putLLdata(IRExpr * e)
{
   stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_LLdata), e));
}

static IRExpr *getLLdata(void)
{
   return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_LLdata), Ity_I32);
}

static void putLLdata64(IRExpr * e)
{
   stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_LLdata64), e));
}

static IRExpr *getLLdata64(void)
{
   return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_LLdata64), Ity_I64);
}

static IRExpr *unop(IROp op, IRExpr * a)
{
   return IRExpr_Unop(op, a);
}

static IRExpr *binop(IROp op, IRExpr * a1, IRExpr * a2)
{
   return IRExpr_Binop(op, a1, a2);
}

/* Generate a new temporary of the given type. */
static IRTemp newTemp(IRType ty)
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp(irsb->tyenv, ty);
}

static void assign(IRTemp dst, IRExpr * e)
{
   stmt(IRStmt_WrTmp(dst, e));
}

static void store(IRExpr * addr, IRExpr * data)
{
#if defined (_MIPSEL)
   stmt(IRStmt_Store(Iend_LE, addr, data));
#elif defined (_MIPSEB)
   stmt(IRStmt_Store(Iend_BE, addr, data));
#endif
}

static IRExpr *load(IRType ty, IRExpr * addr)
{
   IRExpr *load1 = NULL;
#if defined (_MIPSEL)
   load1 = IRExpr_Load(Iend_LE, ty, addr);
#elif defined (_MIPSEB)
   load1 = IRExpr_Load(Iend_BE, ty, addr);
#endif
   return load1;
}

static IRExpr *mkexpr(IRTemp tmp)
{
   return IRExpr_RdTmp(tmp);
}

static UInt extend_sign(UInt value, UChar from_nbits)
{
   UChar shift = 32 - from_nbits;
   return (UInt)((((Int) value) << shift) >> shift);
}

static void ir_for_branch(DisResult *dres, IRExpr *guard, UChar length,
                          Int offset)
{
   dres->whatNext = Dis_StopHere;
   dres->jk_StopHere = Ijk_Boring;
   stmt(IRStmt_Exit(guard, Ijk_Boring,
                    IRConst_U32(guest_PC_curr_instr + length + offset),
                    OFFB_PC));
   putPC(mkU32(guest_PC_curr_instr + length));
}

static void nano_plsu12(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   UShort u = cins & 0x0FFF;

   switch ((cins >> 12) & 0x0F) {
      case PLSU12_LB: {  /* lb[u12] */
         DIP("lb[u12] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_8Sto32,
                          load(Ity_I8, binop(Iop_Add32, getIReg(rs), mkU32(u)))));
         break;
      }

      case PLSU12_LH: {  /* lh[u12] */
         DIP("lh[u12] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_16Sto32,
                          load(Ity_I16, binop(Iop_Add32, getIReg(rs), mkU32(u)))));
         break;
      }

      case PLSU12_LW: {  /* lw[u12] */
         DIP("lw[u12] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, load(Ity_I32, binop(Iop_Add32, getIReg(rs), mkU32(u))));
         break;
      }

      case PLSU12_LD: {  /* ld[u12] */
         DIP("ld[u12] r%u %u(r%u)", rt, u, rs);
         vassert(0);
         break;
      }

      case PLSU12_SB: {   /* sb[u12] */
         DIP("sb[12] r%u %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), unop(Iop_32to8, getIReg(rt)));
         break;
      }

      case PLSU12_SH: {  /* sh[u12] */
         DIP("sh[u12] r%u %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), unop(Iop_32to16, getIReg(rt)));
         break;
      }

      case PLSU12_SW: {  /* sw[u12] */
         DIP("sw[u12] r%u, %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), getIReg(rt));
         break;
      }

      case PLSU12_SD: {  /* sd[u12] */
         DIP("sd[u12] r%u, %u(r%u)", rt, u, rs);
         vassert(0);
      }

      case PLSU12_LBU: {  /* lbu[u12] */
         DIP("lbu r%u, %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_8Uto32,
                          load(Ity_I8, binop(Iop_Add32, getIReg(rs), mkU32(u)))));
         break;
      }

      case PLSU12_LHU: {  /* lhu[u12] */
         DIP("lhu[u12] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_16Uto32,
                          load(Ity_I16, binop(Iop_Add32, getIReg(rs), mkU32(u)))));
         break;
      }

      case PLSU12_LWC1: {  /* lwc1[u12] */
         DIP("lwc1[u12] r%u %u(r%u)", rt, u, rs);
         vassert(0);
         break;
      }

      case PLSU12_LDC1: {  /* ldc1[u12] */
         DIP("ldc1[u12] r%u %u(r%u)", rt, u, rs);
         vassert(0);
         break;
      }

      case PLSU12_PREF: {  /* pref[u12] */
         DIP("pref[u12] r%u %u(r%u)", rt, u, rs);
         break;
      }

      case PLSU12_LWU: {  /* lwu[u12] */
         DIP("lwu[u12] r%u %u(r%u)", rt, u, rs);
         vassert(0);
         break;
      }

      case PLSU12_SWC1: {  /* swc1[u12] */
         DIP("swc1[u12] r%u %u(r%u)", rt, u, rs);
         vassert(0);
         break;
      }

      case PLSU12_SDC1: {  /* sdc1[u12] */
         DIP("sdc1[u12] r%u %u(r%u)", rt, u, rs);
         vassert(0);
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_pl32a0(DisResult *dres, UInt cins)
{
   UChar rd = (cins >> 11) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   IRTemp t1 = newTemp(Ity_I32);
   IRTemp t2 = newTemp(Ity_I64);

   switch ((cins >> 3) & 0x07F) {
      case _POOL32A0_PTRAP: {
         if ((cins >> 10) & 0x01) {  /* tne */
            DIP("tne r%u, r%u", rs, rt);
            stmt(IRStmt_Exit(binop(Iop_CmpNE32, getIReg(rs),
                                   getIReg(rt)), Ijk_SigTRAP,
                             IRConst_U32(guest_PC_curr_instr + 4),
                             OFFB_PC));
         } else {  /* teq */
            DIP("teq r%u, r%u", rs, rt);
            stmt(IRStmt_Exit(binop(Iop_CmpEQ32, getIReg(rs),
                                   getIReg(rt)), Ijk_SigTRAP,
                             IRConst_U32(guest_PC_curr_instr + 4),
                             OFFB_PC));
         }

         break;
      }

      case _POOL32A0_SEB: {  /* seb */
         DIP("seb r%u, r%u", rs, rt);
         putIReg(rt, unop(Iop_8Sto32, unop(Iop_32to8, getIReg(rs))));
         break;
      }

      case _POOL32A0_SEH: {  /* seh */
         DIP("seh r%u, r%u", rs, rt);
         putIReg(rt, unop(Iop_16Sto32, unop(Iop_32to16, getIReg(rs))));
         break;
      }

      case _POOL32A0_SLLV: {  /* sllv */
         DIP("sllv r%u, r%u, r%u", rd, rs, rt);
         assign(t1, binop(Iop_And32, getIReg(rt), mkU32(0x1f)));
         putIReg(rd, binop(Iop_Shl32, getIReg(rs), unop(Iop_32to8,
                           mkexpr(t1))));
         break;
      }

      case _POOL32A0_MUL32: {  /* mul */
         DIP("mul[32] r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, unop(Iop_64to32, binop(Iop_MullS32, getIReg(rt),
                                            getIReg(rs))));
         break;
      }

      case _POOL32A0_MUH: {  /* muh */
         DIP("muh r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, unop(Iop_64HIto32, binop(Iop_MullS32, getIReg(rt),
                                              getIReg(rs))));
         break;
      }

      case _POOL32A0_MULU: {  /* mulu */
         DIP("mulu r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, unop(Iop_64to32, binop(Iop_MullU32, getIReg(rt),
                                            getIReg(rs))));
         break;
      }

      case _POOL32A0_MUHU: {  /* muhu */
         DIP("muhu r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, unop(Iop_64HIto32, binop(Iop_MullU32, getIReg(rt),
                                              getIReg(rs))));
         break;
      }

      case _POOL32A0_DIV: {  /* div */
         DIP("div r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_DivS32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_MOD: {  /* mod */
         DIP("mod r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, unop(Iop_64HIto32, binop(Iop_DivModS32to32, getIReg(rs),
                                              getIReg(rt))));
         break;
      }

      case _POOL32A0_DIVU: {  /* divu */
         DIP("divu r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_DivU32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_MODU: {  /* modu */
         DIP("modu r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, unop(Iop_64HIto32, binop(Iop_DivModU32to32, getIReg(rs),
                                              getIReg(rt))));
         break;
      }

      case _POOL32A0_SRLV: {  /* srlv */
         DIP("srlv r%u, r%u, r%u", rd, rs, rt);
         assign(t1, binop(Iop_And32, getIReg(rt), mkU32(0x1f)));
         putIReg(rd, binop(Iop_Shr32,
                           getIReg(rs), unop(Iop_32to8, mkexpr(t1))));
         break;
      }

      case _POOL32A0_SRAV: {  /* srav */
         DIP("srav r%u, r%u, r%u", rd, rs, rt);
         assign(t1, binop(Iop_And32, getIReg(rt), mkU32(0x1f)));
         putIReg(rd, binop(Iop_Sar32,
                           getIReg(rs), unop(Iop_32to8, mkexpr(t1))));
         break;
      }

      case _POOL32A0_ROTRV: {  /* rotrv */
         DIP("rotv r%u, r%u, r%u", rd, rs, rt);
         assign(t1, binop(Iop_And32, getIReg(rt), mkU32(0x1f)));
         assign(t2, binop(Iop_32HLto64, getIReg(rs), getIReg(rs)));
         putIReg(rd, unop(Iop_64to32,
                          binop(Iop_Shr64,
                                mkexpr(t2), unop(Iop_32to8, mkexpr(t1)))));
         break;
      }

      case _POOL32A0_AND32: {  /* and[32] */
         DIP("and[32] r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_And32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_ADD: {  /* add */
         DIP("add r%u, r%u, r%u", rd, rs, rt);
         // if overflows(sum, nbits=32): raise exception('OV')
         putIReg(rd, binop(Iop_Add32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_ADDU32: {  /* addu[32] */
         DIP("addu[32] r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_Add32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_SUB: {  /* sub */
         DIP("sub r%u, r%u, r%u", rd, rs, rt);
         // if overflows(result, nbits=32): raise exception('OV')
         putIReg(rd, binop(Iop_Sub32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_SUBU32: {  /* subu[32] */
         DIP("subu[32] r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_Sub32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_OR32: {  /* or[32] */
         DIP("or[32] r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_Or32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_NOR: {  /* nor */
         DIP("nor r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, unop(Iop_Not32, binop(Iop_Or32, getIReg(rs),
                                           getIReg(rt))));
         break;
      }

      case _POOL32A0_XOR32: {  /* xor[32] */
         DIP("xor[32] r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_Xor32, getIReg(rs), getIReg(rt)));
         break;
      }

      case _POOL32A0_SLT: {  /* slt */
         DIP("slt r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, unop(Iop_1Uto32, binop(Iop_CmpLT32S, getIReg(rs),
                                            getIReg(rt))));
         break;
      }

      case _POOL32A0_PSLTU: {  /* p.sltu */
         if (rd == 0) {
            vassert(0);
         } else {  /* sltu */
            DIP("sltu r%u, r%u, r%u", rd, rs, rt);
            putIReg(rd, unop(Iop_1Uto32, binop(Iop_CmpLT32U, getIReg(rs),
                                               getIReg(rt))));
         }

         break;
      }

      case _POOL32A0_SOV: {  /* sov */
         IRTemp t33 = newTemp(Ity_I32);
         IRTemp t0 = newTemp(Ity_I32);
         DIP("sov r%u, r%u, r%u", rd, rs, rt);
         assign(t1, binop(Iop_Add32, getIReg(rs), getIReg(rt)));
         assign(t33, binop(Iop_Add32,
                           binop(Iop_Sar32, getIReg(rs), mkU8(1)),
                           binop(Iop_Sar32, getIReg(rt), mkU8(1))));
         assign(t0, binop(Iop_And32,
                          binop(Iop_And32,
                                getIReg(rs), getIReg(rt)), mkU32(1)));
         putIReg(rd, unop(Iop_1Uto32,
                          binop(Iop_CmpNE32,
                                binop(Iop_Sar32, mkexpr(t1), mkU8(1)),
                                binop(Iop_Add32, mkexpr(t33), mkexpr(t0)))));
         // GPR[rd] = 1 if overflows(sum, nbits=32) else 0
         break;
      }

      case _POOL32A0_PCMOVE: {  /* p.cmove */
         if (cins & 0x400) {  /* movn */
            DIP("movn r%u, r%u, r%u", rd, rs, rt);
            putIReg(rd, IRExpr_ITE(binop(Iop_CmpNE32, getIReg(rt), mkU32(0x00)),
                                   getIReg(rs), getIReg(rd)));
         } else {  /* movz */
            DIP("movz r%u, r%u, r%u", rd, rs, rt);
            putIReg(rd, IRExpr_ITE(binop(Iop_CmpEQ32, getIReg(rt), mkU32(0x00)),
                                   getIReg(rs), getIReg(rd)));
         }

         break;
      }

      case _POOL32A0_RDHWR: /* RDHWR */
         DIP("rdhwr r%u, r%u", rt, rs);

         if (rs == 29) {
            putIReg(rt, IRExpr_Get(offsetof(VexGuestMIPS32State, guest_ULR),
                                   Ity_I32));
            break;
         } else if (rs <= 3) {
            IRExpr** arg = mkIRExprVec_1(mkU32(rs));
            IRTemp   val  = newTemp(Ity_I32);
            IRDirty *d = unsafeIRDirty_1_N(val,
                                           0,
                                           "nanomips_dirtyhelper_rdhwr",
                                           &nanomips_dirtyhelper_rdhwr,
                                           arg);
            stmt(IRStmt_Dirty(d));
            putIReg(rt, mkexpr(val));
            break;
         } else {
            vex_printf("Unsupported RDHWR variant");
            vassert(0);
         }

      default:
         vex_printf("Unrecognized _POOL32A0 instruction %08X",
                    (cins >> 3) & 0x07F);
         vassert(0);
   }
}

static void nano_pplsx(DisResult *dres, UInt cins)
{
   UChar rd = (cins >> 11) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;

   switch ((cins >> 7) & 0x0F) {
      case LBX: {  /* lbx */
         DIP("lbx r%u, %u(r%u)", rd, rs, rt);
         putIReg(rd, unop(Iop_8Sto32,
                          load(Ity_I8,
                          binop(Iop_Add32, getIReg(rs), getIReg(rt)))));
         break;
      }

      case SBX: {  /* sbx */
         DIP("sbx r%u %u(r%u)", rd, rs, rt);
         store(binop(Iop_Add32, getIReg(rs), getIReg(rt)),
                     unop(Iop_32to8, getIReg(rd)));
         break;
      }

      case LBUX: {  /* lbux */
         DIP("lbux r%u, %u(r%u)", rd, rs, rt);
         putIReg(rd, unop(Iop_8Uto32,
                          load(Ity_I8,
                               binop(Iop_Add32, getIReg(rs), getIReg(rt)))));
         break;
      }

      case LHX: {
         DIP("lhx r%u, %u(r%u)", rd, rs, rt);
         putIReg(rd, unop(Iop_16Sto32,
                          load(Ity_I16,
                               binop(Iop_Add32, getIReg(rs), getIReg(rt)))));
         break;
      }

      case SHX: {
         DIP("shx r%u %u(r%u)", rd, rs, rt);
         store(binop(Iop_Add32, getIReg(rs), getIReg(rt)), unop(Iop_32to16,
               getIReg(rd)));
         break;
      }

      case LHUX: {
         DIP("lbux r%u, %u(r%u)", rd, rs, rt);
         putIReg(rd, unop(Iop_16Uto32,
                          load(Ity_I16,
                               binop(Iop_Add32, getIReg(rs), getIReg(rt)))));
         break;
      }

      case LWX: {
         DIP("lwx r%u, %u(r%u)", rd, rs, rt);
         putIReg(rd, load(Ity_I32, binop(Iop_Add32, getIReg(rs), getIReg(rt))));
         break;
      }

      case SWX: {
         DIP("swx r%u %u(r%u)", rd, rs, rt);
         store(binop(Iop_Add32, getIReg(rs), getIReg(rt)), getIReg(rd));
         break;
      }

      default:
         vassert(0);
         break;
   }
}

static void nano_pplsxs(DisResult *dres, UInt cins)
{
   UChar rd = (cins >> 11) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;

   switch ((cins >> 7) & 0x0F) {
      case LHXS: {
         DIP("lhxs r%u, %u(r%u)", rd, rs, rt);
         putIReg(rd, unop(Iop_16Sto32,
                          load(Ity_I16,
                               binop(Iop_Add32,
                                     binop(Iop_Shl32, getIReg(rs), mkU8(0x01)),
                                     getIReg(rt)))));
         break;
      }

      case SHXS: {
         DIP("shxs r%u %u(r%u)", rd, rs, rt);
         store(binop(Iop_Add32,
                     binop(Iop_Shl32, getIReg(rs), mkU8(0x01)),
                     getIReg(rt)),
               unop(Iop_32to16, getIReg(rd)));
         break;
      }

      case LHUXS: {
         DIP("lbuxs r%u, %u(r%u)", rd, rs, rt);
         putIReg(rd, unop(Iop_16Uto32,
                          load(Ity_I16,
                               binop(Iop_Add32,
                                     binop(Iop_Shl32, getIReg(rs), mkU8(0x01)),
                                     getIReg(rt)))));
         break;
      }

      case LWXS32: {
         DIP("lwxs[32] r%u, r%u(r%u)", rd, rs, rt);
         putIReg(rd, load(Ity_I32,
                          binop(Iop_Add32,
                                binop(Iop_Shl32, getIReg(rs), mkU8(0x02)),
                                getIReg(rt))));
         break;
      }

      case SWXS: {
         DIP("swxs r%u %u(r%u)", rd, rs, rt);
         store(binop(Iop_Add32,
                     binop(Iop_Shl32, getIReg(rs), mkU8(0x02)),
                     getIReg(rt)),
               getIReg(rd));
         break;
      }

      default:
         vassert(0);
         break;
   }
}

static void nano_plsx(DisResult *dres, UInt cins)
{
   if ((cins >> 6) & 0x01) {
      nano_pplsxs(dres, cins);
   } else {
      nano_pplsx(dres, cins);
   }
}

static void nano_pool32Axf_4(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   IRTemp t1;

   switch ((cins >> 9) & 0x7F) {
      case nano_POOL32Axf4_CLO: {  /* clo */
         DIP("clo r%u, r%u", rt, rs);
         t1 = newTemp(Ity_I1);
         assign(t1, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0xffffffff)));
         putIReg(rt, IRExpr_ITE(mkexpr(t1),
                                mkU32(0x00000020),
                                unop(Iop_Clz32,
                                     unop(Iop_Not32, getIReg(rs)))));
         break;
      }

      case nano_POOL32Axf4_CLZ: {  /* clz */
         DIP("clz r%u, r%u", rt, rs);
         putIReg(rt, unop(Iop_Clz32, getIReg(rs)));
         break;
      }
   }
}

static void nano_p32Axf(DisResult *dres, UInt cins)
{
   switch ((cins >> 6) & 0x7) {
      case POOL32aXF_4:
         nano_pool32Axf_4(dres, cins);
         break;

      case POOL32aXF_5:
         vassert(0);
         break;

      default:
         vex_printf("Unrecognized pool32Axf instruction %08X\n", cins);
         vassert(0);
         break;
   }
}

static void nano_pool32a7(DisResult *dres, UInt cins)
{
   UChar rd = (cins >> 11) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   UChar u2  = (cins >> 9) & 0x03;
   UChar shift = (cins >> 6) & 0x1F;

   switch ((cins >> 3) & 7) {
      case _POOL32A7_PLSX:
         nano_plsx(dres, cins);
         break;

      case _POOL32A7_LSA: {  /* lsa */
         DIP("lsa r%u r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_Add32, binop(Iop_Shl32, getIReg(rs), mkU8(u2)),
                           getIReg(rt)));
         break;
      }

      case _POOL32A7_EXTW: {  /*extw*/
         DIP("extw r%u r%u, r%u, %u", rd, rs, rt, shift);
         IRTemp t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_32HLto64, getIReg(rt), getIReg(rs)));
         putIReg(rd, unop(Iop_64to32, binop(Iop_Shr64, mkexpr(t1),
                                            mkU8(shift))));
         break;
      }

      case _POOL32A7_P32Axf: {
         nano_p32Axf(dres, cins);
         break;
      }

      default:
         vex_printf("Unrecognized _POOL32A7 instruction %08X", cins);
         vassert(0);
   }
}

static void nano_p32a(DisResult *dres, UInt cins)
{
   switch (cins & 0x7) {
      case P32A_POOL32A0:
         nano_pl32a0(dres, cins);
         break;

      case P32A_POOL32A7:
         nano_pool32a7(dres, cins);
         break;

      default:
         vex_printf("Unrecognized P32A instruction %08X", cins);
         vassert(0);
   }
}

static void nano_pbal(DisResult *dres, UInt cins)
{
   Int s = extend_sign((cins & 0x1FFFFFE) | ((cins & 1) << 25), 26);

   if (cins & 0x2000000) { /* BALC[32] */
      DIP("balc %0X", guest_PC_curr_instr + 4 + s);
      putIReg(31, mkU32(guest_PC_curr_instr + 4));
      dres->jk_StopHere = Ijk_Call;
   } else { /* BC[32] */
      DIP("bc %0X", guest_PC_curr_instr + 4 + s);
      dres->jk_StopHere = Ijk_Boring;
   }

   putPC(mkU32(guest_PC_curr_instr + 4 + s));
   dres->whatNext = Dis_StopHere;
}

static void nano_ppsr(DisResult *dres, UInt cins)
{
   UInt u = cins & 0xFF8;
   UChar count = (cins >> 16) & 0x0F;
   UChar rt = (cins >> 21) & 0x1F;
   UChar counter = 0;
   Bool jr = True;

   switch (cins & 0x03) {
      case 0x00: {  /* save[32] */
         DIP("save %u, r%u-r%u", u, (rt & 0x1fu) | (rt & 0x10u),
             ((rt + count - 1) & 0x1fu) | (rt & 0x10u));

         while (counter != count) {
            Bool use_gp = (cins & 0x04) && (counter + 1 == count);
            UChar this_rt = use_gp ? 28 : (UChar)((rt + counter) & 0x1f)
                                          | (rt & 0x10);
            Int offset = -((counter + 1) << 2);
            store(binop(Iop_Add32, getIReg(29), mkU32(offset)),
                  getIReg(this_rt));
            counter++;
         }

         putIReg(29, binop(Iop_Sub32, getIReg(29), mkU32(u)));
         break;
      }

      case 0x02: /* restore[32] */
         jr = False; //falls through common restore(.jrc) implementation

      case 0x03: {  /* restore.jrc[32] */
         DIP("restore%s %u, r%u-r%u", jr ? ".jrc" : "", u,
             ((rt + count - 1) & 0x1fu) | (rt & 0x10u),
             (rt & 0x1fu) | (rt & 0x10u));

         while (counter != count) {
            Bool use_gp = (cins & 0x04) && (counter + 1 == count);
            UChar this_rt = use_gp ? 28 : ((rt + counter) & 0x1F) | (rt & 0x10);
            Int offset = u - ((counter + 1) << 2);
            putIReg(this_rt,load(Ity_I32, binop(Iop_Add32,
                                                getIReg(29), mkU32(offset))));
            // if this_rt == 29: raise UNPREDICTABLE()
            counter++;
         }

         putIReg(29, binop(Iop_Add32, getIReg(29), mkU32(u)));

         if (jr) {
            putPC(getIReg(31));
            dres->whatNext = Dis_StopHere;
            dres->jk_StopHere = Ijk_Ret;
         }

         break;
      }

      default:
         vassert(0);
   }
}

static void nano_psrf(UInt cins)
{
   switch (cins & 0x03) {
      case 0x00: {  /* savef */
         vex_printf("Instruction savef is missing documentation.\n");
         vassert(0);
         break;
      }

      case 0x02: {  /* restoref */
         vex_printf("Instruction restoref is missing documentation.\n");
         vassert(0);
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_psr(DisResult *dres, UInt cins)
{
   switch ((cins >> 20) & 0x1) {
      case 0x00:  /* pp.sr */
         nano_ppsr(dres, cins);
         break;

      case 0x01:  /* p.sr.f */
         nano_psrf(cins);
         break;

      default:
         vassert(0);
         break;
   }
}

static void nano_pri(DisResult *dres, UInt cins)
{
   switch ((cins >> 19) & 3) {
      case PRI_SIGRIE:
         ILLEGAL_INSTRUCTON
         break;

      case PRI_PSYSCALL:
         if (cins & 0x40000) { /* HYPCALL */
            vex_printf("Instruction HYPCALL is missing documentation.\n");
            vassert(0);
         } else { /* SYSCALL[32] */
            DIP("syscall %u", cins & 0x3FFFF);
            dres->jk_StopHere = Ijk_Sys_syscall;
            dres->whatNext    = Dis_StopHere;
         }

         break;

      case PRI_BREAK: /* BREAK[32] */
         DIP("break %u", cins & 0x7FFFF);
         dres->jk_StopHere = Ijk_SigTRAP;
         dres->whatNext    = Dis_StopHere;
         break;

      case PRI_SDBBP:
         vex_printf("Instruction SDBBP is not supported.\n");
         vassert(0);
         break;

      default:
         vassert(0);
   }
}

static void nano_psll(UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar shift = cins & 0x1F;

   if (rt == 0 && shift == 0) {  /* nop[32] */
      DIP("nop[32]");
      return;
   }

   if (rt == 0 && shift == 3) {  /* ehb */
      DIP("ehb");
      vassert(0);
      return;
   }

   if (rt == 0 && shift == 5) {  /* pause */
      DIP("pause");
      vassert(0);
      // pause_until_llbit_clears();
      return;
   }

   if (rt == 0 && shift == 6) {  /* sync */
      DIP("sync 0x%x", rs);
      /* Just ignore it. */
      return;
   }

   DIP("sll r%u, r%u, %u", rt, rs, shift);
   putIReg(rt, binop(Iop_Shl32, getIReg(rs), mkU8(shift)));
   return;
}

static void nano_pshift(UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar shift = cins & 0x1F;

   switch ((cins >> 5) & 0xF) {
      case PSLL: {  /* p.sll */
         nano_psll(cins);
         break;
      }

      case SRL32:  /* srl[32] */
         DIP("srl[32] r%u, r%u, %u", rt, rs, shift);
         putIReg(rt, binop(Iop_Shr32, getIReg(rs), mkU8(shift)));
         break;

      case SRA:  /* sra */
         DIP("sra r%u, r%u, %u", rt, rs, shift);
         putIReg(rt, binop(Iop_Sar32, getIReg(rs), mkU8(shift)));
         break;

      case ROTR:  /* rotr */
         DIP("rotr r%u, r%u, %u", rt, rs, shift);
         IRTemp t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_32HLto64, getIReg(rs), getIReg(rs)));
         putIReg(rt, unop(Iop_64to32, binop(Iop_Shr64, mkexpr(t1),
                                            mkU8(shift))));
         break;

      case DSLL: {  /* dsll */
         DIP("dsll r%u, r%u, %u", rt, rs, shift);
         vassert(0);
         break;
      }

      case DSLL32: {  /* dsll32 */
         DIP("dsll32 r%u, r%u, %u", rt, rs, shift);
         vassert(0);
         break;
      }

      case DSRL: {  /* dsrl */
         DIP("dsrl r%u, r%u, %u", rt, rs, shift);
         vassert(0);
         break;
      }

      case DSRL32: {  /* dsrl32 */
         DIP("dsrl32 r%u, r%u, %u", rt, rs, shift);
         vassert(0);
         break;
      }

      case DSRA: {  /* dsra */
         DIP("dsra r%u, r%u, %u", rt, rs, shift);
         vassert(0);
         break;
      }

      case DSRA32: {  /* dsra32 */
         DIP("dsra32 r%u, r%u, %u", rt, rs, shift);
         vassert(0);
         break;
      }

      case DROTR: {  /* drotr */
         DIP("drotr r%u, r%u, %u", rt, rs, shift);
         vassert(0);
         break;
      }

      case DROTR32: {  /* drotr32 */
         DIP("drotr32 r%u, r%u, %u", rt, rs, shift);
         vassert(0);
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_protx(UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar shift = cins & 0x1F;
   UChar shiftx = ((cins >> 7) & 0xF) << 1;
   UChar stripe = (cins & 0x40) ? 1 : 0;

   switch ((cins >> 5) & 0x41) {
      case 0x00: {  /* rotx */
         int i;
         IRTemp t0  = newTemp(Ity_I64);
         IRTemp t1  = newTemp(Ity_I64);
         IRTemp t2  = newTemp(Ity_I64);
         IRTemp t3  = newTemp(Ity_I64);
         IRTemp t4  = newTemp(Ity_I64);
         IRTemp t5  = newTemp(Ity_I64);
         IRTemp tmp = newTemp(Ity_I64);
         IRTemp s   = newTemp(Ity_I32);
         DIP("rotx r%u, r%u, %u, %u, %u", rt, rs, shift, shiftx, stripe);
         assign(t0, binop(Iop_Or64, getIReg(rs), binop(Iop_Shl64,
                          getIReg(rs), mkU8(32))));
         assign(t1, mkexpr(t0));

         for (i = 0; i < 46; i++) {
            assign(s, IRExpr_ITE(binop(Iop_And32, mkU32(i), mkU32(0x08)),
                                 mkU32(shift), mkU32(shiftx)));
            assign(s, IRExpr_ITE(binop(Iop_And32, mkU32(stripe),
                                       binop(Iop_CmpNE32, mkU32(0x0),
                                             binop(Iop_And32,
                                                   mkU32(i), mkU32(0x04)))),
                                 unop(Iop_Not32, mkU32(s)), mkexpr(s)));
            assign(tmp, binop(Iop_Or64, binop(Iop_And64,
                                              binop(Iop_Shr64, mkexpr(t0),
                                                    mkU8(0x10)),
                                              binop(Iop_Shl64, mkU64(0x01),
                                                    mkU8(i))),
                              binop(Iop_And64, mkexpr(t1),
                                    unop(Iop_Not64,
                                         binop(Iop_Shl64, mkU64(0x01),
                                               mkU8(i))))));
            assign(t1, IRExpr_ITE(binop(Iop_And32, mkexpr(s), mkU32(0x10)),
                                  mkexpr(tmp),
                                  mkexpr(t1)));

         }

         assign(t2, mkexpr(t1));

         for (i = 0; i < 38; i++) {
            assign(s, IRExpr_ITE(binop(Iop_And32, mkU32(i), mkU32(0x04)),
                                 mkU32(shift), mkU32(shiftx)));
            assign(tmp, binop(Iop_Or64,
                              binop(Iop_And64,
                                    binop(Iop_Shr64, mkexpr(t1), mkU8(0x08)),
                                    binop(Iop_Shl64, mkU64(0x01), mkU8(i))),
                              binop(Iop_And64, mkexpr(t2),
                                    unop(Iop_Not64, binop(Iop_Shl64,
                                                          mkU64(0x01),
                                                          mkU8(i))))));
            assign(t2, IRExpr_ITE(binop(Iop_And32, mkexpr(s), mkU32(0x08)),
                                  mkexpr(tmp),
                                  mkexpr(t2)));

         }

         assign(t3, mkexpr(t2));

         for (i = 0; i < 34; i++) {
            assign(s, IRExpr_ITE(binop(Iop_And32, mkU32(i), mkU32(0x02)),
                                 mkU32(shift), mkU32(shiftx)));
            assign(tmp, binop(Iop_Or64,
                              binop(Iop_And64,
                                    binop(Iop_Shr64, mkexpr(t2), mkU8(0x04)),
                                    binop(Iop_Shl64, mkU64(0x01), mkU8(i))),
                              binop(Iop_And64, mkexpr(t3),
                                    unop(Iop_Not64, binop(Iop_Shl64,
                                                          mkU64(0x01),
                                                          mkU8(i))))));
            assign(t3, IRExpr_ITE(binop(Iop_And32, mkexpr(s), mkU32(0x04)),
                                  mkexpr(tmp),
                                  mkexpr(t3)));

         }

         assign(t4, mkexpr(t3));

         for (i = 0; i < 32; i++) {
            assign(s, IRExpr_ITE(binop(Iop_And32, mkU32(i), mkU32(0x01)),
                                 mkU32(shift), mkU32(shiftx)));
            assign(tmp, binop(Iop_Or64,
                              binop(Iop_And64,
                                    binop(Iop_Shr64, mkexpr(t3), mkU8(0x02)),
                                    binop(Iop_Shl64, mkU64(0x01), mkU8(i))),
                              binop(Iop_And64, mkexpr(t4),
                                    unop(Iop_Not64, binop(Iop_Shl64,
                                                          mkU64(0x01),
                                                          mkU8(i))))));
            assign(t4, IRExpr_ITE(binop(Iop_And32, mkexpr(s), mkU32(0x02)),
                                  mkexpr(tmp),
                                  mkexpr(t4)));

         }

         assign(t5, mkexpr(t4));

         for (i = 0; i < 32; i++) {
            assign(tmp, binop(Iop_Or64,
                              binop(Iop_And64,
                                    binop(Iop_Shr64, mkexpr(t4), mkU8(0x01)),
                                    binop(Iop_Shl64, mkU64(0x01), mkU8(i))),
                              binop(Iop_And64, mkexpr(t5),
                                    unop(Iop_Not64, binop(Iop_Shl64,
                                                          mkU64(0x01),
                                                          mkU8(i))))));
            assign(t4, IRExpr_ITE(binop(Iop_And32, mkexpr(shift), mkU32(0x02)),
                                  mkexpr(tmp),
                                  mkexpr(t5)));

         }

         putIReg(rt, mkexpr(t5));
         break;
      }

      default:
         vassert(0);
         break;
   }

}

static void nano_pins(UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar lsb = cins & 0x1F;
   UChar msbd = (cins >> 6) & 0x1F;

   switch ((cins >> 5) & 0x41) {
      case 0x00: { /* ins */
         UChar size = 1 + msbd - lsb;
         DIP("ins r%u, r%u, %u, %u", rt, rs, lsb, size);
         UInt mask = ((1 << size) - 1) << lsb;
         putIReg(rt, binop(Iop_Or32, binop(Iop_And32, getIReg(rt),
                                           mkU32(~mask)),
                           binop(Iop_And32,
                                 binop(Iop_Shl32,
                                       getIReg(rs), mkU8(lsb)),
                                 mkU32(mask))));
         break;
      }

      case 0x01: { /* dins */
         vassert(0);
         break;
      }

      case 0x40: { /* dinsm */
         vassert(0);
         break;
      }

      case 0x41: { /* dins */
         vassert(0);
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_pext(UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar lsb = cins & 0x1F;
   UChar msbd = (cins >> 6) & 0x1F;

   switch ((cins >> 5) & 0x41) {
      case 0x00: { /* ext */
         DIP("ext r%u, r%u, %u, %u", rt, rs, lsb, msbd + 1u);

         if (msbd + 1 + lsb > 32) vassert(0);

         putIReg(rt, binop(Iop_And32, binop(Iop_Shr32, getIReg(rs), mkU8(lsb)),
                           mkU32((1 << (msbd + 1)) - 1)));
         break;
      }

      case 0x01: { /* dextu */
         vassert(0);
         break;
      }

      case 0x40: { /* dextm */
         vassert(0);
         break;
      }

      case 0x41: { /* dext */
         vassert(0);
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_pool16c00(UInt cins)
{
   UChar rs = GPR3_list[(cins >> 4) & 0x07];
   UChar rt = GPR3_list[(cins >> 7) & 0x07];

   switch (cins & 0x0C) {
      case POOL16C00_NOT: {  /* not[16] */
         DIP("not[16] r%u, r%u", rt, rs);
         putIReg(rt, unop(Iop_Not32, getIReg(rs)));
         break;
      }

      case POOL16C00_XOR: {  /* xor[16] */
         DIP("xor[16] r%u, r%u", rt, rs);
         putIReg(rt, binop(Iop_Xor32, getIReg(rs), getIReg(rt)));
         break;
      }

      case POOL16C00_AND: {  /* and[16] */
         DIP("and[16] r%u, r%u", rt, rs);
         putIReg(rt, binop(Iop_And32, getIReg(rs), getIReg(rt)));
         break;
      }

      case POOL16C00_OR: {  /* or[16] */
         DIP("or[16] r%u, r%u", rt, rs);
         putIReg(rt, binop(Iop_Or32, getIReg(rs), getIReg(rt)));
         break;
      }
   }
}

static void nano_pu12(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   UShort u = cins & 0x0FFF;

   switch ((cins >> 12) & 0x0F) {
      case PU12_ORI: {  /* ori */
         DIP("ori r%u, r%u, %u", rt, rs, u);
         putIReg(rt, binop(Iop_Or32, getIReg(rs), mkU32(u)));
         break;
      }

      case PU12_XORI: {  /* xori */
         DIP("xori r%u, r%u, %u", rt, rs, u);
         putIReg(rt, binop(Iop_Xor32, getIReg(rs), mkU32(u)));
         break;
      }

      case PU12_ANDI: {  /* andi */
         DIP("andi r%u, r%u, %u", rt, rs, u);
         putIReg(rt, binop(Iop_And32, getIReg(rs), mkU32(u)));
         break;
      }

      case PU12_PSR:  /* p.sr */
         nano_psr(dres, cins);
         break;

      case PU12_SLTI: {  /* slti */
         DIP("slti r%u, r%u, %u", rt, rs, u);
         putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpLT32S, getIReg(rs),
                                            mkU32(u))));
         break;
      }

      case PU12_SLTIU: {  /* sltiu */
         DIP("sltiu r%u, r%u, %u", rt, rs, u);
         putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpLT32U, getIReg(rs),
                                            mkU32(u))));
         break;
      }

      case PU12_SEQI: {  /* seqi */
         DIP("seqi r%u, r%u, %u", rt, rs, u);
         putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpEQ32, getIReg(rs),
                                            mkU32(u))));
         break;
      }

      case PU12_ADDIU_NEG: {  /* addiu[neg] */
         DIP("addiu[neg] r%u, r%u, %u", rt, rs, u);
         putIReg(rt, binop(Iop_Sub32, getIReg(rs), mkU32(u)));
         break;
      }

      case PU12_PSHIFT:  /* p.shift */
         nano_pshift(cins);
         break;

      case PU12_PROTX:  /* p.rotx */
         nano_protx(cins);
         break;

      case PU12_PINS: /* p.ins */
         nano_pins(cins);
         break;

      case PU12_PEXT: /* p.ext */
         nano_pext(cins);
         break;

      default:
         vassert(0);
   }
}

static void nano_pbr1(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   Short s = (Short)((cins & 0x3FFE) |
                     ((cins & 1) << 14) | ((cins & 1) << 15));

   switch ((cins >> 14) & 0x3) {
      case PBR1_BEQC32: {  /* BEQC[32] */
         DIP("beqc[32] r%u, r%u, %X", rt, rs, guest_PC_curr_instr + 4 + (Int)s);
         ir_for_branch(dres, binop(Iop_CmpEQ32, getIReg(rt), getIReg(rs)),
                       4, (Int)s);
         break;
      }

      case PBR1_PBR3A: {  /* P.BR3A */
         vassert(0);
         break;
      }

      case PBR1_BGEC: {  /* BGEC */
         DIP("bgec r%u, r%u, %X", rs, rt, guest_PC_curr_instr + 4 + (Int)s);
         ir_for_branch(dres, binop(Iop_CmpLE32S, getIReg(rt), getIReg(rs)),
                       4, (Int)s);
         break;
      }

      case PBR1_BGEUC: {  /* bgeuc */
         DIP("bgeuc r%u, r%u, %X", rs, rt, guest_PC_curr_instr + 4 + (Int)s);
         ir_for_branch(dres, binop(Iop_CmpLE32U, getIReg(rt), getIReg(rs)),
                       4, (Int)s);
         break;
      }

      default:
         vex_printf("Unsupported p.br1 instruction %08X", cins);
         vassert(0);
   }
}

static void nano_pbr2(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   Short s = (Short)((cins & 0x3FFE) |
                     ((cins & 1) << 14) | ((cins & 1) << 15));

   switch ((cins >> 14) & 0x3) {
      case PBR2_BNEC32: {  /* BNEC[32] */
         DIP("bnec r%u, r%u, %X", rt, rs, guest_PC_curr_instr + 4 + (Int)s);
         ir_for_branch(dres, binop(Iop_CmpNE32, getIReg(rt), getIReg(rs)),
                       4, (Int)s);
         break;
      }

      case PBR2_BLTC: {  /* BLTC */
         DIP("bltc r%u, r%u, %X", rt, rs, guest_PC_curr_instr + 4 + (Int)s);
         ir_for_branch(dres, binop(Iop_CmpLT32S, getIReg(rs), getIReg(rt)),
                       4, (Int)s);
         break;
      }

      case PBR2_BLTUC: {  /* BLTUC */
         DIP("bltuc r%u, r%u, %X", rt, rs, guest_PC_curr_instr + 4 + (Int)s);
         ir_for_branch(dres, binop(Iop_CmpLT32U, getIReg(rs), getIReg(rt)),
                       4, (Int)s);
         break;
      }

      default:
         vex_printf("Unsupported p.br2 instruction %08X", cins);
         vassert(0);
   }
}

static void nano_pbri(DisResult *dres, UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   Int s = extend_sign((cins & 0x7FE) | ((cins & 0x01) << 11), 12);
   UChar bit = (cins >> 11) & 0x3F;
   UInt u =  (cins >> 11) & 0x7F;

   switch ((cins >> 18) & 0x07) {
      case PBRI_BEQIC: {
         DIP("beqic r%u, %u, %0X", rt, u, guest_PC_curr_instr + 4 + s);
         ir_for_branch(dres, binop(Iop_CmpEQ32, getIReg(rt), mkU32(u)),
                       4, (Int)s);
         break;
      }

      case PBRI_BBEQZC: {
         DIP("bbeqzc r%u, %u, %0X", rt, bit, guest_PC_curr_instr + 4 + s);

         if (bit >= 32) {
            ILLEGAL_INSTRUCTON
            return;
         }

         ir_for_branch(dres,
                       binop(Iop_CmpEQ32,
                             binop(Iop_And32,
                                   binop(Iop_Shr32, getIReg(rt), mkU8(bit)),
                                   mkU32(1)),
                             mkU32(0)), 4, s);
         break;
      }

      case PBRI_BGEIC: {  /* bgeic */
         DIP("bgeic r%u, %u, %0X", rt, u, guest_PC_curr_instr + 4 + s);
         ir_for_branch(dres, binop(Iop_CmpLE32S, mkU32(u), getIReg(rt)),
                       4, (Int)s);
         break;
      }

      case PBRI_BGEIUC: {  /* bgeiuc */
         DIP("bgeiuc r%u, %u, %0X", rt, u, guest_PC_curr_instr + 4 + s);
         ir_for_branch(dres, binop(Iop_CmpLE32U, mkU32(u), getIReg(rt)),
                       4, (Int)s);
         break;
      }

      case PBRI_BNEIC: {
         DIP("bneic r%u, %u, %0X", rt, u, guest_PC_curr_instr + 4 + s);
         ir_for_branch(dres,
                       binop(Iop_CmpNE32, getIReg(rt), mkU32(u)), 4, s);
         break;
      }

      case PBRI_BBNEZC: {
         DIP("bbnezc r%u, %u, %0X", rt, bit, guest_PC_curr_instr + 4 + s);

         if (bit >= 32) {
            ILLEGAL_INSTRUCTON
            return;
         }

         ir_for_branch(dres,
                       binop(Iop_CmpNE32,
                             binop(Iop_And32,
                                   binop(Iop_Shr32, getIReg(rt), mkU8(bit)),
                                   mkU32(1)),
                             mkU32(0)), 4, s);
         break;
      }

      case PBRI_BLTIC: {
         DIP("bltic r%u, %u, %0X", rt, u, guest_PC_curr_instr + 4 + s);
         ir_for_branch(dres, binop(Iop_CmpLT32S, getIReg(rt), mkU32(u)),
                       4, (Int)s);
         break;
      }

      case PBRI_BLTIUC: {
         DIP("bltiuc r%u, %u, %0X", rt, u, guest_PC_curr_instr + 4 + s);
         ir_for_branch(dres, binop(Iop_CmpLT32U, getIReg(rt), mkU32(u)),
                       4, (Int)s);
         break;
      }

      default:
         vex_printf("Unsupported p.bri instruction %08X", cins);
         vassert(0);
   }
}

static void nano_pprefs9(DisResult *dres, UInt cins)
{
   UChar hint = (cins >> 21) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;
   UChar s = extend_sign((cins & 0xFF) | ((cins >> 7) & 0x100), 9);

   if (hint == 31) { /* synci */
      DIP("synci %u(r%u)", s, rs);
      vassert(0);
   } else { /* pref[s9] */
      DIP("pref[s9] %u, %u(r%u)", hint, s, rs);
      vassert(0);
   }
}

static void nano_plss0(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   Int s  = extend_sign(((cins >> 7) & 0x100) | (cins & 0xFF), 9);

   switch ((cins >> 11) & 0xf) {
      case LBS9: {  /* lb[s9] */
         DIP("lb[s9] r%u %d(r%u)", rt, s, rs);
         putIReg(rt, unop(Iop_8Sto32,
                          load(Ity_I8, binop(Iop_Add32, getIReg(rs),
                                             mkU32(s)))));
         break;
      }

      case LHS9: {  /* lh[s9] */
         DIP("lh[s9] r%u %d(r%u)", rt, s, rs);
         putIReg(rt, unop(Iop_16Sto32,
                          load(Ity_I16, binop(Iop_Add32, getIReg(rs),
                                              mkU32(s)))));
         break;
      }

      case LWS9: {  /* lw[s9] */
         DIP("lw[s9] r%u %d(r%u)", rt, s, rs);
         putIReg(rt, load(Ity_I32, binop(Iop_Add32, getIReg(rs), mkU32(s))));
         break;
      }

      case LDS9: {  /* ld[s9] */
         DIP("ld[s9] r%u %d(r%u)", rt, s, rs);
         vassert(0);
         break;
      }

      case SBS9: {  /* sb[s9] */
         DIP("sb[s9] r%u %d(r%u)", rt, s, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(s)), unop(Iop_32to8,
                                                             getIReg(rt)));
         break;
      }

      case SHS9: {  /* sh[s9] */
         DIP("sh[s9] r%u %d(r%u)", rt, s, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(s)), unop(Iop_32to16,
                                                             getIReg(rt)));
         break;
      }

      case SWS9: {  /* sw[s9] */
         DIP("sw[s9] r%u %d(r%u)", rt, s, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(s)), getIReg(rt));
         break;
      }

      case SDS9: {  /* sd[s9] */
         DIP("sd[s9] r%u %d(r%u)", rt, s, rs);
         vassert(0);
         break;
      }

      case LBUS9: { /* lbu[s9] */
         DIP("lbu[s9] r%u %d(r%u)", rt, s, rs);
         putIReg(rt, unop(Iop_8Uto32,
                          load(Ity_I8, binop(Iop_Add32, getIReg(rs),
                                             mkU32(s)))));
         break;
      }

      case LHUS9: { /* lhu[s9] */
         DIP("lhu[s9] r%u %d(r%u)", rt, s, rs);
         putIReg(rt, unop(Iop_16Uto32,
                          load(Ity_I16, binop(Iop_Add32, getIReg(rs),
                                              mkU32(s)))));
         break;
      }

      case LWC1S9: { /* lwc1[s9] */
         DIP("lwc1[s9] r%u %d(r%u)", rt, s, rs);
         vassert(0);
         break;
      }

      case LDC1S9: { /* ldc1[s9] */
         DIP("ldc1[s9] r%u %d(r%u)", rt, s, rs);
         vassert(0);
         break;
      }

      case PPREFS9: { /* p.pref[s9] pool */
         nano_pprefs9(dres, cins);
         break;
      }

      case LWUS9: { /* lwu[s9] */
         DIP("lwu[s9] r%u %d(r%u)", rt, s, rs);
         vassert(0);
         break;
      }

      case SWC1S9: { /* swc1[s9] */
         DIP("swc1[s9] r%u %d(r%u)", rt, s, rs);
         vassert(0);
         break;
      }

      case SDC1S9: { /* sdc1[s9] */
         DIP("sdc1[s9] r%u %d(r%u)", rt, s, rs);
         vassert(0);
         break;
      }
   }
}

static void nano_pll(DisResult *dres, UInt cins)
{
   IRTemp t1, t2;
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   UInt s  = extend_sign(((cins >> 7) & 0x100) | (cins & 0xFC), 9);

   switch (cins & 0x03) {
      case LL: {
         DIP("ll r%u %u(r%u)", rt, s, rs);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(s)));
         assign(t2, load(Ity_I32, mkexpr(t1)));
         putLLaddr(mkexpr(t1));
         putLLdata(mkexpr(t2));
         putIReg(rt, mkexpr(t2));
         break;
      }

      case LLWP: {
         UChar ru = (cins >> 3) & 0x1F;
         DIP("llwp r%u %u(r%u)", rt, s, rs);
         if (rt == ru) vassert(0);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I64);
         assign(t1, getIReg(rs));
         assign(t2, load(Ity_I64, mkexpr(t1)));
         putLLaddr(mkexpr(t1));
         putLLdata64(mkexpr(t2));
         putIReg(rt, unop(Iop_64to32, mkexpr(t2)));
         putIReg(ru, unop(Iop_64HIto32, mkexpr(t2)));
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_psc(DisResult *dres, UInt cins)
{
   IRTemp t1, t2, t3, t4, t5;
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   UInt s  = extend_sign(((cins >> 7) & 0x100) | (cins & 0xFC), 9);

   switch (cins & 0x03) {
      case SC: {
         DIP("sc r%u %u(r%u)", rt, s, rs);

         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);
         t3 = newTemp(Ity_I32);

         assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(s)));

         assign(t2, binop(Iop_CmpNE32,
                          mkexpr(t1), getLLaddr()));
         assign(t3, getIReg(rt));
         putLLaddr(LLADDR_INVALID);
         putIReg(rt, getIReg(0));

         stmt(IRStmt_Exit(mkexpr(t2), Ijk_Boring,
                          IRConst_U32(guest_PC_curr_instr + 4),
              OFFB_PC));

         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);

         assign(t5, getLLdata());

         stmt(IRStmt_CAS(mkIRCAS(IRTemp_INVALID, t4, /* old_mem */
              MIPS_IEND, mkexpr(t1),                 /* addr */
              NULL, mkexpr(t5),                      /* expected value */
              NULL, mkexpr(t3)                       /* new value */)));

         putIReg(rt, unop(Iop_1Uto32,
                          binop(Iop_CmpEQ32, mkexpr(t4), mkexpr(t5))));
         break;
      }

      case SCWP: {
         UChar ru = (cins >> 3) & 0x1F;
         DIP("scwp r%u %u(r%u)", rt, s, rs);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I1);

         IRTemp oldHi, oldLo, expHi, expLo;
         oldHi = newTemp(Ity_I32);
         oldLo = newTemp(Ity_I32);
         expHi = newTemp(Ity_I32);
         expLo = newTemp(Ity_I32);

         assign(t2, binop(Iop_CmpNE32,
                          getIReg(rs), getLLaddr()));

         putLLaddr(LLADDR_INVALID);
         putIReg(rt, getIReg(0));

         stmt(IRStmt_Exit(mkexpr(t2), Ijk_Boring,
                          IRConst_U32(guest_PC_curr_instr + 4),
              OFFB_PC));

         assign(expHi, unop(Iop_64HIto32, getLLdata64()));
         assign(expLo, unop(Iop_64to32, getLLdata64()));


         stmt(IRStmt_CAS(mkIRCAS(oldHi, oldLo, /* old_mem */
              MIPS_IEND, getIReg(rs),          /* addr */
              mkexpr(expHi),  mkexpr(expLo),   /* expected value */
              getIReg(ru), getIReg(rt)         /* new value */)));

         putIReg(rt, binop(Iop_And32,
                           unop(Iop_1Uto32,
                                binop(Iop_CmpEQ32, mkexpr(oldHi), mkexpr(expHi))),
                           unop(Iop_1Uto32,
                                binop(Iop_CmpEQ32, mkexpr(oldLo), mkexpr(expLo)))));
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_plss1(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   UInt s  = extend_sign(((cins >> 7) & 0x100) | (cins & 0xFF), 9);

   switch ((cins >> 11) & 0x0F) {
      case ASET_ACLER: {
         vassert(0);
         break;
      }

      case UALH: {  /* ualh */
         DIP("ualh r%u %u(r%u)", rt, s, rs);
         putIReg(rt, unop(Iop_16Sto32,
                          load(Ity_I16, binop(Iop_Add32, getIReg(rs), mkU32(s)))));
         break;
      }

      case UASH: {  /* uash */
         DIP("uash r%u %u(r%u)", rt, s, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(s)), unop(Iop_32to16, getIReg(rt)));
         break;
      }

      case CACHE: {  /* cache */
         vassert(0);
         break;
      }

      case LWC2: {
         vassert(0);
         break;
      }

      case SWC2: {
         vassert(0);
         break;
      }

      case LDC2: {
         vassert(0);
         break;
      }

      case SDC2: {
         vassert(0);
         break;
      }

      case PLL: {
         nano_pll(dres, cins);
         break;
      }

      case PSC: {
         nano_psc(dres, cins);
         break;
      }

      case PLLD: {
         vassert(0);
         break;
      }

      case PSCD: {
         vassert(0);
         break;
      }

      default:
         vex_printf("Unrecognized P.LS.S1 instruction %08X", cins);
         vassert(0);
   }
}

static void nano_plswm(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   Int offset  = extend_sign(((cins >> 7) & 0x100) | (cins & 0xFF), 9);
   UChar count3 = (cins >> 12) & 0x07;
   UChar count = count3 ? count3 : 8;
   UChar counter = 0;
   UChar rt_tmp;
   Int offset_tmp;

   if ((cins >> 11) & 0x01) {  /* swm */
      DIP("swm r%u, %d(r%u), %u", rt, offset, rs, count);

      while (counter != count) {
         rt_tmp = rt ? (rt & 0x10) | ((rt + counter) & 0x1F) : 0;
         offset_tmp = offset + (counter << 2);
         store(binop(Iop_Add32, getIReg(rs), mkU32(offset_tmp)),
               getIReg(rt_tmp));
         counter++;
      }
   } else {  /* lwm */
      DIP("lwm r%u, %d(r%u), %u", rt, offset, rs, count);

      while (counter != count) {
         rt_tmp = (rt & 0x10) | ((rt + counter) & 0x1F);
         offset_tmp = offset + (counter << 2);
         putIReg(rt_tmp, load(Ity_I32, binop(Iop_Add32, getIReg(rs),
                                             mkU32(offset_tmp))));

         if ((rt_tmp == rs) && (counter != count - 1)) {
            // raise UNPREDICTABLE()
         }

         counter++;
      }

   }
}

static void nano_plsuawm(DisResult *dres, UInt cins)
{
   UChar rs = (cins >> 16) & 0x1F;
   UChar rt = (cins >> 21) & 0x1F;
   UInt s  = extend_sign(((cins >> 7) & 0x100) | (cins & 0xFF), 9);
   UChar count3 = (cins >> 12) & 0x07;
   UChar count = count3 ? count3 : 8;
   UChar counter = 0;
   UInt offset = extend_sign(s, 9);
   UChar rt_tmp, offset_tmp;

   if ((cins >> 11) & 0x01) {  /* swm */
      while (counter++ != count) {
         rt_tmp = rt ? (rt & 0x10) | ((rt + counter) & 0x1F) : 0;
         offset_tmp = offset + (counter << 2);
         store(binop(Iop_Add32, getIReg(rs), mkU32(offset_tmp)),
               getIReg(rt_tmp));
      }
   } else {  /* lwm */
      while (counter++ != count) {
         rt_tmp = (rt & 0x10) | (rt + counter);
         offset_tmp = offset + (counter << 2);
         putIReg(rt_tmp, load(Ity_I32, binop(Iop_Add32, getIReg(rs),
                                             mkU32(offset_tmp))));

         if ((rt_tmp == rs) && (counter != count - 1)) {
            vassert(0);
            // raise UNPREDICTABLE()
         }
      }

   }
}

static void nano_plss9(DisResult *dres, UInt cins)
{
   switch ((cins >> 8) & 0x7) {
      case PLSS0: {  /* p.ls.s0 */
         nano_plss0(dres, cins);
         break;
      }

      case PLSS1: {
         nano_plss1(dres, cins);
         break;
      }

      case PLSE0: {
         vassert(0);
         break;
      }

      case PLSWM: {
         nano_plswm(dres, cins);
         break;
      }

      case PLSUAWM: {
         nano_plsuawm(dres, cins);
         break;
      }

      case PLSDM: {
         vassert(0);
         break;
      }

      case PLSUADM: {
         vassert(0);
         break;
      }
   }
}

static void nano_p16a1(DisResult *dres, UShort cins)
{
   if (cins & 0x40) { /* ADDIU[R1.SP] */
      UChar rs = 29;
      UChar rt = GPR3_list[(cins >> 7) & 0x07];
      UChar u = (cins & 0x3F) << 2;
      DIP("ADDIU[R1.SP] r%u, %u", rt, u);
      putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(u)));
   } else {
      vassert(0);
   }
}

static void nano_p16a2(DisResult *dres, UShort cins)
{
   if (cins & 0x08) { /* P.ADDIU[RS5] */
      UChar rt = (cins >> 5) & 0x1F;

      if (rt != 0) { /* ADDIU[RS5] */
         Int s = extend_sign((cins & 0x07) | ((cins >> 1) & 0x08), 4);
         DIP("addiu r%u, r%u, %d", rt, rt, s);
         putIReg(rt, binop(Iop_Add32, getIReg(rt), mkU32(s)));
      } else {
         DIP("nop");
      }
   } else { /* ADDIU[R2] */
      UChar rs = GPR3_list[(cins >> 4) & 0x07];
      UChar rt = GPR3_list[(cins >> 7) & 0x07];
      UChar u = (cins & 0x07) << 2;
      DIP("addiu r%u, r%u, 0x%X", rt, rs, u);
      putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(u)));
   }
}

static void nano_p16ri(DisResult *dres, UShort cins)
{
   switch ((cins >> 3) & 0x03) {
      case RI_PSYSCALL: {
         if (cins & 0x4) { /* HYPCALL[16] */
            vex_printf("Instruction HYPCALL is missing documentation.\n");
            vassert(0);
         } else { /* SYSCALL[16] */
            DIP("syscall %u", cins & 0x3u);
            dres->jk_StopHere = Ijk_Sys_syscall;
            dres->whatNext    = Dis_StopHere;
         }

         break;
      }

      case RI_BREAK: {  /* BREAK[16] */
         DIP("break %u", cins & 0x7u);
         dres->jk_StopHere = Ijk_SigTRAP;
         dres->whatNext    = Dis_StopHere;
         break;
      }

      case RI_SDBBP: {
         vex_printf("Instruction SDBBP is not supported.\n");
         vassert(0);
      }
   }
}

static void nano_p16mv(DisResult *dres, UShort cins)
{
   UChar rs = cins & 0x1F;
   UChar rt = (cins >> 5) & 0x1f;

   if (rt != 0) {
      DIP("move r%u, r%u", rt, rs);
      putIReg(rt, getIReg(rs));
   } else {
      nano_p16ri(dres, cins);
   }
}

static void nano_p16shift(DisResult *dres, UShort cins)
{
   UChar rs = GPR3_list[(cins >> 4) & 0x07];
   UChar rt = GPR3_list[(cins >> 7) & 0x07];
   UChar shift = cins & 0x07;

   if (cins & 0x08) { /* slr[16] */
      DIP("slr r%u, r%u, %u ", rt, rs, shift);
      putIReg(rt, binop(Iop_Shr32, getIReg(rs), mkU8(shift != 0 ? shift : 8)));
   } else { /* sll[16] */
      DIP("sll r%u, r%u, %u ", rt, rs, shift);
      putIReg(rt, binop(Iop_Shl32, getIReg(rs), mkU8(shift != 0 ? shift : 8)));
   }
}

static void nano_p16c(DisResult *dres, UShort cins)
{
   switch (cins & 0x03) {
      case 0x00: {  /* POOL16C_0 */
         nano_pool16c00(cins);
         break;
      }

      case 0x01:
      case 0x03: { /* LWXS[16] */
         UChar rt = GPR3_list[(cins >> 7) & 0x07];
         UChar rs = GPR3_list[(cins >> 4) & 0x07];
         UChar rd = GPR3_list[(cins >> 1) & 0x07];
         DIP("lwxs[32] r%u, %u(r%u)", rd, rs, rt);
         putIReg(rd, load(Ity_I32,
                          binop(Iop_Add32, binop(Iop_Shl32, getIReg(rs),
                                                 mkU8(0x02)),
                          getIReg(rt))));
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_p16br(DisResult *dres, UShort cins)
{
   UChar u = (cins & 0x0f) << 1;

   if (0 == u) {
      UChar rt = (cins >> 5) & 0x1F;

      if (cins & 0x10) { /* JALRC[16] */
         DIP("jalrc r%u", rt);
         putIReg(31, mkU32(guest_PC_curr_instr + 2));
         dres->jk_StopHere = Ijk_Call;
      } else { /* JRC */
         DIP("jrc r%u", rt);
         dres->jk_StopHere = rt != 31 ? Ijk_Boring : Ijk_Ret;
      }

      putPC(getIReg(rt));
      dres->whatNext = Dis_StopHere;
   } else {
      UChar rt = GPR3_list[(cins >> 7) & 0x07];
      UChar rs = GPR3_list[(cins >> 4) & 0x07];

      if (rs < rt) {  /* beqc[16] */
         DIP("beqc r%u, r%u, %X", rt, rs, guest_PC_curr_instr + 2 + u);
         ir_for_branch(dres, binop(Iop_CmpEQ32, getIReg(rt), getIReg(rs)),
                       2, (Int)u);
      } else {  /* bnec[16] */
         DIP("bnec r%u, r%u, %X", rt, rs, guest_PC_curr_instr + 2 + u);
         ir_for_branch(dres, binop(Iop_CmpNE32, getIReg(rt), getIReg(rs)),
                       2, (Int)u);
      }
   }
}

static void nano_p16sr(DisResult *dres, UShort cins)
{
   UChar u = cins & 0xF0;
   UChar count = cins & 0x0F;
   UChar counter = 0;
   UChar rt = cins & 0x200 ? 31 : 30;

   if (cins & 0x100) { /* RESTORE.JRC[16] */
      DIP("restore.jrc %u, r%u-r%u", u, (rt & 0x1fu) | (rt & 0x10u),
          ((rt + count - 1) & 0x1fu) | (rt & 0x10u));

      while (counter != count) {
         UChar this_rt = ((rt + counter) & 0x1F) | (rt & 0x10);
         Int offset = u - ((counter + 1) << 2);
         putIReg(this_rt, load(Ity_I32, binop(Iop_Add32, getIReg(29),
                                              mkU32(offset))));
         // if this_rt == 29: raise UNPREDICTABLE()
         counter++;
      }

      putIReg(29, binop(Iop_Add32, getIReg(29), mkU32(u)));
      putPC(getIReg(31));
      dres->whatNext = Dis_StopHere;
      dres->jk_StopHere = Ijk_Ret;
   } else { /* SAVE[16] */
      DIP("save %u, r%u-r%u", u, (rt & 0x1fu) | (rt & 0x10u),
          ((rt + count - 1) & 0x1fu) | (rt & 0x10u));

      while (counter != count) {
         UChar this_rt = ((rt + counter) & 0x1f) | (rt & 0x10);
         Int offset = -((counter + 1) << 2);
         store(binop(Iop_Add32, getIReg(29), mkU32(offset)), getIReg(this_rt));
         counter++;
      }

      putIReg(29, binop(Iop_Sub32, getIReg(29), mkU32(u)));
   }
}

static void nano_p16lb(DisResult *dres, UShort cins)
{
   UChar rt = GPR3_list[(cins >> 7) & 0x7];
   UChar rs = GPR3_list[(cins >> 4) & 0x7];
   UChar u = cins & 0x3;

   switch ((cins >> 2) & 0x3) {
      case 0x0: /* LB[16] */
         DIP("lb[16] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_8Sto32,
                          load(Ity_I8, binop(Iop_Add32, getIReg(rs),
                                             mkU32(u)))));
         break;

      case 0x1: /* SB[16] */
         rt = GPR3_src_store_list[(cins >> 7) & 0x7];
         DIP("sb[16] r%u %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), unop(Iop_32to8,
                                                             getIReg(rt)));
         break;

      case 0x2: /* LBU[16] */
         DIP("lbu[16] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_8Uto32,
                          load(Ity_I8, binop(Iop_Add32, getIReg(rs),
                                             mkU32(u)))));
         break;

      default:
         vex_printf("Unrecognized bytes %04x\n", cins);
         vassert(0);
   }
}

static void nano_p16lh(DisResult *dres, UShort cins)
{
   UChar rt = GPR3_list[(cins >> 7) & 0x7];
   UChar rs = GPR3_list[(cins >> 4) & 0x7];
   UChar u = cins & 0x06;

   switch (cins & 0x09) {
      case 0x0: /* LH[16] */
         DIP("lh[16] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_16Sto32,
                          load(Ity_I16, binop(Iop_Add32, getIReg(rs),
                                              mkU32(u)))));
         break;

      case 0x1: /* SH[16] */
         rt = GPR3_src_store_list[(cins >> 7) & 0x7];
         DIP("sh[16] r%u %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), unop(Iop_32to16,
                                                             getIReg(rt)));
         break;

      case 0x8: /* LHU[16] */
         DIP("lhu[16] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_16Uto32,
                          load(Ity_I16, binop(Iop_Add32, getIReg(rs),
                                              mkU32(u)))));
         break;

      default:
         vex_printf("Unrecognized bytes %04x\n", cins);
         vassert(0);
   }
}

static void nano_p164x4(DisResult *dres, UShort cins)
{
   UChar rt = GPR4_list[((cins >> 6) & 0x08) | ((cins >> 5) & 0x07)];
   UChar rs = GPR4_list[((cins >> 1) & 0x08) | (cins & 0x07)];
   UChar rd = rt;

   switch (cins & 0x108) {
      case 0x00: {  /* ADDU[4x4] */
         DIP("addu[4x4] r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_Add32, getIReg(rs), getIReg(rt)));
         break;
      }

      case 0x08: {  /* MUL[4x4] */
         DIP("mul[4x4] r%u, r%u, r%u", rd, rs, rt);
         putIReg(rd, binop(Iop_Mul32, getIReg(rs), getIReg(rt)));
         break;
      }

      default:
         vassert(0);
   }
}

static void nano_pgpbh(DisResult *dres, UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   UChar rs = 28;
   UInt  u  = cins & 0x3FFFF;

   switch ((cins >> 18) & 7) {
      case LBGP: {  /* lb[gp] */
         DIP("lb[gp] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_8Sto32,
                          load(Ity_I8, binop(Iop_Add32, getIReg(rs),
                                             mkU32(u)))));
         break;
      }

      case SBGP: {  /* sb[gp] */
         DIP("sb[gp] r%u %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), unop(Iop_32to8,
                                                             getIReg(rt)));
         break;
      }

      case LBUGP: {  /* lbu[gp] */
         DIP("lbu[gp] r%u %u(r%u)", rt, u, rs);
         putIReg(rt, unop(Iop_8Uto32,
                          load(Ity_I8, binop(Iop_Add32, getIReg(rs),
                                             mkU32(u)))));
         break;
      }

      case ADDIUGPB: { /* addiu[gp.b] */
         DIP("addiu r%u, r%u, 0x%X", rt, rs, u);
         putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(u)));
         break;
      }

      case PGPLH: {
         if (cins & 0x01) {  /* lhu[gp] */
            DIP("lhu[gp] r%u %u(r%u)", rt, u, rs);
            putIReg(rt, unop(Iop_16Uto32,
                             load(Ity_I16, binop(Iop_Add32, getIReg(rs),
                                                 mkU32(u & 0x3FFFE)))));
         } else {  /* lh[gp] */
            DIP("lh[gp] r%u %u(r%u)", rt, u, rs);
            putIReg(rt, unop(Iop_16Sto32,
                             load(Ity_I16, binop(Iop_Add32, getIReg(rs),
                                                 mkU32(u & 0x3FFFE)))));
         }

         break;
      }

      case PGPSH: {
         if (cins & 0x01) {
            vassert(0);
         } else {  /* sh[gp] */
            DIP("sh[gp] r%u %u(r%u)", rt, u, rs);
            store(binop(Iop_Add32, getIReg(rs), mkU32(u & 0x3FFFE)),
                  unop(Iop_32to16, getIReg(rt)));
         }

         break;
      }

      case PGPCP1: {
         vassert(0);
         break;
      }

      case PGPM64: {
         vassert(0);
         break;
      }
   }
}

static void nano_pj(DisResult *dres, UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   UChar rs = (cins >> 16) & 0x1F;

   switch ((cins >> 12) & 0x0F) {
      case JALRC32: {  /* JARLC[32] */
         DIP("jalrc[32] r%u, r%u", rt, rs);
         putIReg(rt, mkU32(guest_PC_curr_instr + 4));
         putPC(getIReg(rs));
         dres->jk_StopHere = Ijk_Call;
         dres->whatNext = Dis_StopHere;
         break;
      }

      case JALRCHB: {
         DIP("jalrc.hb r%u r%u", rt, rs);
         putIReg(rt, mkU32(guest_PC_curr_instr + 4));
         putPC(getIReg(rs));
         dres->jk_StopHere = Ijk_Call;
         dres->whatNext = Dis_StopHere;
         // clear_hazards()
         break;
      }

      case PBALRSC: {
         if (rt == 0) {  /* brsc */
            DIP("brsc r%u", rs);
            IRTemp t1 = newTemp(Ity_I32);
            assign(t1, binop(Iop_Add32, mkU32(guest_PC_curr_instr + 4),
                             binop(Iop_Shl32, getIReg(rs), mkU8(0x01))));
            putPC(mkexpr(t1));
         } else {  /* balrsc */
            DIP("balrsc r%u, r%u", rs, rt);
            IRTemp t1 = newTemp(Ity_I32);
            assign(t1, binop(Iop_Add32, mkU32(guest_PC_curr_instr + 4),
                             binop(Iop_Shl32, getIReg(rs), mkU8(0x01))));
            putIReg(rt, mkU32(guest_PC_curr_instr + 4));
            putPC(mkexpr(t1));
         }
      }
      break;
   }
}

static void nano_pgpw(DisResult *dres, UInt cins)
{
   UChar rt = (cins >> 21) & 0x1F;
   UChar rs = 28;
   UInt  u  = cins & 0x1FFFFC;

   switch (cins & 0x03) {
      case PGPW_ADDIU: {  /* addiu[gp.w] */
         DIP("addiu[gp.w] r%u, r%u, %u", rt, rs, u);
         putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(u)));
         break;
      }

      case PGPW_LW: {  /* lw[gp] */
         DIP("lw[gp] r%u, %u(r%u)", rt, u, rs);
         putIReg(rt, load(Ity_I32, binop(Iop_Add32, getIReg(rs), mkU32(u))));
         break;
      }

      case PGPW_SW: {  /* sw[gp] */
         DIP("sw[gp] r%u, %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), getIReg(rt));
         break;
      }
   }
}

static void dis_nanoMIPS16(DisResult *dres, UShort cins)
{
   switch (cins >> 10) {
      case P16A2:
         nano_p16a2(dres, cins);
         break;

      case BC16: {
         Int s = extend_sign((cins & 0x3FE) | ((cins & 1) << 10), 11);
         DIP("bc %0X", guest_PC_curr_instr + 2 + s);
         putPC(mkU32(guest_PC_curr_instr + 2 + s));
         dres->jk_StopHere = Ijk_Boring;
         dres->whatNext = Dis_StopHere;
         break;
      }

      case P16MV:
         nano_p16mv(dres, cins);
         break;

      case P16SHIFT:
         nano_p16shift(dres, cins);
         break;

      case P16C:
         nano_p16c(dres, cins);
         break;

      case P16BR:
         nano_p16br(dres, cins);
         break;
         break;

      case P16ADDU: {
         UChar rt = GPR3_list[(cins >> 7) & 0x7];
         UChar rs = GPR3_list[(cins >> 4) & 0x7];
         UChar rd = GPR3_list[(cins >> 1) & 0x7];

         if (cins & 1) { /* SUBU[16] */
            DIP("subu r%u, r%u, r%u", rd, rs, rt);
            putIReg(rd, binop(Iop_Sub32, getIReg(rs), getIReg(rt)));
         } else { /* ADDU[16] */
            DIP("addu r%u, r%u, r%u", rd, rs, rt);
            putIReg(rd, binop(Iop_Add32, getIReg(rs), getIReg(rt)));
         }

         break;
      }

      case LI16: {
         UChar rt = GPR3_list[(cins >> 7) & 0x07];
         UChar eu = cins & 0x7F;
         Int s;

         if (eu == 127) s = -1;
         else s = eu;

         DIP("li r%u, %d", rt, s);
         putIReg(rt, mkU32(s));
         break;
      }

      case BNEZC16: {
         UChar rt = GPR3_list[(cins >> 7) & 0x7];
         Int s = (Char)((cins & 0x7E) | (cins << 7));
         DIP("bnezc r%u, %X", rt, guest_PC_curr_instr + 2 + s);
         ir_for_branch(dres, binop(Iop_CmpNE32, getIReg(rt), mkU32(0)), 2, s);
         break;
      }

      case BEQZC16: {
         UChar rt = GPR3_list[(cins >> 7) & 0x7];
         Int s = (Char)((cins & 0x7E) | (cins << 7));
         DIP("beqzc r%u, %X", rt, guest_PC_curr_instr + 2 + s);
         ir_for_branch(dres, binop(Iop_CmpEQ32, getIReg(rt), mkU32(0)), 2, s);
         break;
      }

      case P16LB:
         nano_p16lb(dres, cins);
         break;

      case P16LH:
         nano_p16lh(dres, cins);
         break;

      case P16SR:
         nano_p16sr(dres, cins);
         break;

      case P16A1:
         nano_p16a1(dres, cins);
         break;

      case ANDI16: { /* ANDI[16] */
         UChar rt = GPR3_list[(cins >> 7) & 0x7];
         UChar rs = GPR3_list[(cins >> 4) & 0x7];
         UChar eu = cins & 0xF;
         UInt u;

         if (eu == 12) u = 0x00FF;
         else if (eu == 13) u = 0xFFFF;
         else u = (UInt)eu;

         DIP("andi[16] r%u, r%u, %u", rt, rs, u);
         putIReg(rt, binop(Iop_And32, getIReg(rs), mkU32(u)));
         break;
      }

      case LW16: { /* LW[16] */
         UChar rt = GPR3_list[(cins >> 7) & 0x7];
         UChar rs = GPR3_list[(cins >> 4) & 0x7];
         UChar u = (cins & 0x0F) << 2;
         DIP("lw[16] r%u, %u(r%u)", rt, u, rs);
         putIReg(rt, load(Ity_I32, binop(Iop_Add32, getIReg(rs), mkU32(u))));
         break;
      }

      case LWSP: { /* LW[SP] */
         UChar rt = (cins >> 5) & 0x1F;
         UChar rs = 29;
         UChar u = (cins & 0x1F) << 2;
         DIP("lw[SP] r%u, %u(r%u)", rt, u, rs);
         putIReg(rt, load(Ity_I32, binop(Iop_Add32, getIReg(rs), mkU32(u))));
         break;
      }

      case LWGP16: { /* LW[GP16] */
         UChar rt = GPR3_list[(cins >> 7) & 0x07];
         UChar rs = 28;
         UInt u = (cins & 0x7F) << 2;
         DIP("lw[GP16] r%u, %u(r%u)", rt, u, rs);
         putIReg(rt, load(Ity_I32, binop(Iop_Add32, getIReg(rs), mkU32(u))));
         break;
      }

      case LW4X4: { /* LW[4x4] */
         UChar rt = GPR4_list[((cins >> 6) & 0x08) | ((cins >> 5) & 0x07)];
         UChar rs = GPR4_list[((cins >> 1) & 0x08) | (cins & 0x07)];
         UChar u = (cins & 0x08) | ((cins >> 6) & 0x04);
         DIP("lw[4x4] r%u, %u(r%u)", rt, u, rs);
         putIReg(rt, load(Ity_I32, binop(Iop_Add32, getIReg(rs), mkU32(u))));
         break;
      }

      case SW16: { /* SW[16] */
         UChar rt = GPR3_src_store_list[(cins >> 7) & 0x7];
         UChar rs = GPR3_list[(cins >> 4) & 0x7];
         UChar u = (cins & 0x0F) << 2;
         DIP("sw[16] r%u, %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), getIReg(rt));
         break;
      }

      case SWSP: { /* SW[SP] */
         UChar rt = (cins >> 5) & 0x1F;
         UChar rs = 29;
         UChar u = (cins & 0x1F) << 2;
         DIP("sw[SP] r%u, %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), getIReg(rt));
         break;
      }

      case SWGP16: { /* SW[GP16] */
         UChar rt = GPR3_src_store_list[(cins >> 7) & 0x07];
         UChar rs = 28;
         UInt u = (cins & 0x7F) << 2;
         DIP("sw[GP16] r%u, %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), getIReg(rt));
         break;
      }

      case SW4X4: { /* SW[4x4] */
         UChar rt = GPR4_zero_list[((cins >> 6) & 0x08) | ((cins >> 5) & 0x07)];
         UChar rs = GPR4_list[((cins >> 1) & 0x08) | (cins & 0x07)];
         UChar u = (cins & 0x08) | ((cins >> 6) & 0x04);
         DIP("sw[4x4] r%u, %u(r%u)", rt, u, rs);
         store(binop(Iop_Add32, getIReg(rs), mkU32(u)), getIReg(rt));
         break;
      }

      case P164X4: { /* P16.4X4 pool */
         nano_p164x4(dres, cins);
         break;
      }

      case MOVEP: { /* MOVEP */
         UChar rd1 = GPR2_reg1_list[((cins >> 2) & 0x02) | ((cins >> 8) & 0x01)];
         UChar rd2 = GPR2_reg2_list[((cins >> 2) & 0x02) | ((cins >> 8) & 0x01)];
         UChar rs1 = GPR4_zero_list[((cins >> 1) & 0x08) | (cins & 0x07)];
         UChar rs2 = GPR4_zero_list[((cins >> 6) & 0x08) | ((cins >> 5) & 0x07)];
         DIP("MOVEP r%u, r%u, r%u, r%u", rd1, rd2, rs1, rs2);
         putIReg(rd1, getIReg(rs1));
         putIReg(rd2, getIReg(rs2));
         break;
      }

      case MOVEPREV: { /* MOVEP[REV] */
         UChar rd1 = GPR4_list[((cins >> 1) & 0x08) | (cins & 0x07)];
         UChar rd2 = GPR4_list[((cins >> 6) & 0x08) | ((cins >> 5) & 0x07)];
         UChar rs1 = GPR2_reg1_list[((cins >> 2) & 0x02) | ((cins >> 8) & 0x01)];
         UChar rs2 = GPR2_reg2_list[((cins >> 2) & 0x02) | ((cins >> 8) & 0x01)];
         DIP("MOVEP r%u, r%u, r%u, r%u", rd1, rd2, rs1, rs2);
         putIReg(rd1, getIReg(rs1));
         putIReg(rd2, getIReg(rs2));
         break;
      }

      case BALC16: {
         Int s = extend_sign((cins & 0x3FE) | ((cins & 0x1) << 10), 11);
         DIP("balc %0X", guest_PC_curr_instr + 2 + s);
         putIReg(31, mkU32(guest_PC_curr_instr + 2));
         putPC(mkU32(guest_PC_curr_instr + 2 + s));
         dres->whatNext = Dis_StopHere;
         dres->jk_StopHere = Ijk_Call;
         break;
      }

      default:
         vex_printf("Unsupported 16bit: %04X\n", cins);
         vassert(0);
         break;
   }

   dres->len = 2;
}

static void dis_nanoMIPS32(DisResult *dres, UInt cins)
{

   switch (cins >> 26) {
      case P_ADDIURI: {
         UChar rt = (cins >> 21) & 0x1F;

         if (rt != 0) {
            UChar rs = (cins >> 16) & 0x1F;
            UInt u = cins & 0xFFFF;
            DIP("addiu r%u, r%u, %u", rt, rs, u);
            putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(u)));
         } else {
            nano_pri(dres, cins);
         }

         break;
      }

      case ADDIUPC32: {
         UChar rt = (cins >> 21) & 0x1F;
         Int s = extend_sign((cins & 0x1FFFFE) | ((cins & 1) << 21), 22);
         DIP("addiupc r%u, 0x%X", rt, guest_PC_curr_instr + 4 + s);
         putIReg(rt, mkU32(guest_PC_curr_instr + 4 + s));
         break;
      }

      case MOVE_BALC: {
         Int s = extend_sign((cins & 0x1FFFFE) | ((cins & 1) << 21), 22);
         UChar rt = GPR4_zero_list[((cins >> 21) & 0x07)
                                    | ((cins >> 22) & 0x08)];
         UChar rd = (cins & 0x1000000) ? 5 : 4; /* GPR1_list */
         DIP("move.balc r%u, r%u, %0X", rd, rt, guest_PC_curr_instr + 4 + s);
         putIReg(rd, getIReg(rt));
         putIReg(31, mkU32(guest_PC_curr_instr + 4));
         putPC(mkU32(guest_PC_curr_instr + 4 + s));
         dres->jk_StopHere = Ijk_Call;
         dres->whatNext = Dis_StopHere;
         break;
      }

      case PLSU12:
         nano_plsu12(dres, cins);
         break;

      case PGPW:
         nano_pgpw(dres, cins);
         break;

      case PU12:
         nano_pu12(dres, cins);
         break;

      case P32A:
         nano_p32a(dres, cins);
         break;

      case PGPBH:
         nano_pgpbh(dres, cins);
         break;

      case PBAL:
         nano_pbal(dres, cins);
         break;

      case PBR1:
         nano_pbr1(dres, cins);
         break;

      case PBR2:
         nano_pbr2(dres, cins);
         break;

      case PBRI:
         nano_pbri(dres, cins);
         break;

      case PLSS9:
         nano_plss9(dres, cins);
         break;

      case P_LUI:
         if (cins & 0x02) {  /* ALUIPC */
            UChar rt = (cins >> 21) & 0x1F;
            UInt s = (cins & 0x1FF000) | ((cins & 0xFFC) << 19) | (cins << 31);
            DIP("aluipc r%u %08X", rt, s);
            putIReg(rt, binop(Iop_And32, mkU32(guest_PC_curr_instr + s + 4),
                              unop(Iop_Not32, mkU32(0xFFF))));
         } else { /* LUI */
            UChar rt = (cins >> 21) & 0x1F;
            UInt s = (cins & 0x1FF000) | ((cins & 0xFFC) << 19) | (cins << 31);
            DIP("lui r%u %08X", rt, s);
            putIReg(rt, mkU32(s));
         }

         break;

      case PJ: {
         nano_pj(dres, cins);
         break;
      }

      default:
         vex_printf("Unsupported 32bit: %08X\n", cins);
         vassert(0);
         break;
   }

   dres->len = 4;
}

static void dis_nanoMIPS48(DisResult *dres, ULong cins)
{

   UChar rt = (cins >> 37) & 0x1F;
   UInt x = (UInt)cins;

   switch ((cins >> 32) & 0x1F) {
      case P48I_LI:
         DIP("li r%u, 0x%X", rt, x);
         putIReg(rt, mkU32(x));
         break;

      case P48I_ADDIU:
         DIP("addiu r%u, r%u, 0x%X", rt, rt, x);
         putIReg(rt, binop(Iop_Add32, getIReg(rt), mkU32(x)));
         break;

      case P48I_ADDIU_GP:
         DIP("addiu r%u, r28, 0x%X", rt, x);
         putIReg(rt, binop(Iop_Add32, getIReg(28), mkU32(x)));
         break;

      case P48I_ADDIUPC:
         DIP("addiupc r%u, 0x%X", rt, x);
         putIReg(rt, mkU32(guest_PC_curr_instr + x + 6));
         break;

      case P48I_LWPC:
         DIP("lwpc r%u, 0x%X", rt, x);
         putIReg(rt, load(Ity_I32, mkU32(guest_PC_curr_instr + 6 + x)));
         break;

      case P48I_SWPC:
         DIP("swpc r%u, 0x%X", rt, x);
         store(mkU32(guest_PC_curr_instr + 6 + x), getIReg(rt));
         break;

      default:
         vex_printf("Unsupported 48bit: %012llX\n", cins);
         vassert(0);
         break;
   }

   dres->len = 6;
}

static Bool check_for_special_requests_nanoMIPS(DisResult *dres,
                                                const UChar *code)
{
/* 8000 c04d  srl  zero, zero, 13
   8000 c05d  srl  zero, zero, 29
   8000 c043  srl  zero, zero,  3
   8000 c053  srl  zero, zero, 19  */
   const UInt word1 = 0x8000C04D;
   const UInt word2 = 0x8000C05D;
   const UInt word3 = 0x8000C043;
   const UInt word4 = 0x8000C053;
   if (getUInt(code + 0) == word1 && getUInt(code + 4) == word2 &&
       getUInt(code + 8) == word3 && getUInt(code + 12) == word4) {
      /* Got a "Special" instruction preamble. Which one is it? */
      if (getUInt(code + 16) == 0x218C6290 /* or t0, t0, t0 */ ) {
         /* $a0 = client_request ( $a1 ) */
         DIP("a0 = client_request(a1)");
         dres->jk_StopHere = Ijk_ClientReq;
         dres->whatNext    = Dis_StopHere;
         dres->len = 20;
         return True;
      } else if (getUInt(code + 16) == 0x21AD6A90 /* or t1, t1, t1 */ ) {
         /*  $a0 = guest_NRADDR */
         DIP("a0 = guest_NRADDR");
         putIReg(11, IRExpr_Get(offsetof(VexGuestMIPS32State, guest_NRADDR),
                                Ity_I32));
         dres->len = 20;
         return True;
      } else if (getUInt(code + 16) == 0x21CE7290 /* or t2, t2, t2 */ ) {
         /*  branch-and-link-to-noredir $25 */
         DIP("branch-and-link-to-noredir t9");
         putIReg(31, mkU32(guest_PC_curr_instr + 20));
         putPC(getIReg(25));
         dres->jk_StopHere = Ijk_NoRedir;
         dres->whatNext    = Dis_StopHere;
         dres->len = 20;
         return True;
      } else if (getUInt(code + 16) == 0x21EF7A90 /* or t3, t3, t3 */ ) {
        /* IR injection */
         DIP("IR injection");
#if defined (_MIPSEL)
         vex_inject_ir(irsb, Iend_LE);
#elif defined (_MIPSEB)
         vex_inject_ir(irsb, Iend_BE);
#endif

         stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_CMSTART),
                         mkU32(guest_PC_curr_instr)));
         stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_CMLEN),
                         mkU32(20)));

         dres->whatNext    = Dis_StopHere;
         dres->jk_StopHere = Ijk_InvalICache;
         dres->len = 20;
         return True;
      }
   }
   return False;
}


/*------------------------------------------------------------*/
/*---          Disassemble a single instruction            ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR. The instruction is
   located in host memory at guest_instr, and has guest IP of
   guest_PC_curr_instr, which will have been set before the call
   here. */


/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */
DisResult disInstr_nanoMIPS( IRSB*        irsb_IN,
                             const UChar* guest_code_IN,
                             Long         delta,
                             Addr         guest_IP,
                             VexArch      guest_arch,
                             const VexArchInfo* archinfo,
                             const VexAbiInfo*  abiinfo,
                             VexEndness   host_endness_IN,
                             Bool         sigill_diag_IN )
{
   DisResult dres;
   const UChar *code;
   vassert(guest_arch == VexArchNANOMIPS);

   /* Set result defaults. */
   dres.whatNext = Dis_Continue;
   dres.len = 0;
   dres.jk_StopHere = Ijk_INVALID;
   dres.hint        = Dis_HintNone;

   irsb = irsb_IN;
   guest_PC_curr_instr = (Addr32)guest_IP;

   code = guest_code_IN + delta;

   if (!check_for_special_requests_nanoMIPS(&dres, code)) {
      UShort cins = getUShort(code);
      nanoMIPSopcodes opcode = cins >> 10;

      if (opcode & P16) dis_nanoMIPS16(&dres, cins);
      else if (opcode == P48I) {
         ULong cinsl = (((ULong) cins ) << 32) |
                       (((ULong) getUShort(code + 4)) << 16) |
                       getUShort(code + 2);
         dis_nanoMIPS48(&dres, cinsl);
      } else {
         UInt cinsi = (((UInt) cins ) << 16) | getUShort(code + 2);
         dis_nanoMIPS32(&dres, cinsi);
      }
   }

   if ((dres.whatNext == Dis_Continue)         ||
         (dres.jk_StopHere == Ijk_Sys_syscall) ||
         (dres.jk_StopHere == Ijk_SigTRAP)     ||
         (dres.jk_StopHere == Ijk_SigILL)      ||
         (dres.jk_StopHere == Ijk_ClientReq)   ||
         (dres.jk_StopHere == Ijk_NoRedir)     ||
         (dres.jk_StopHere == Ijk_InvalICache)) {
      putPC(mkU32(guest_PC_curr_instr + dres.len));
   }

   return dres;
}


/*--------------------------------------------------------------------*/
/*--- end                                    guest_nanomips_toIR.c ---*/
/*--------------------------------------------------------------------*/
