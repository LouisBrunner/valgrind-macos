
/*---------------------------------------------------------------*/
/*--- begin                                       mips_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2019 RT-RK

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
*/
#ifndef __VEX_MIPS_DEFS_H
#define __VEX_MIPS_DEFS_H

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

/* MOD: The IRSB* into which we're generating code. */
extern IRSB *irsb;

/* Is our guest binary 32 or 64bit? Set at each call to
   disInstr_MIPS below. */
extern Bool mode64;

/* Pointer to the guest code area. */
extern const UChar *guest_code;

/*------------------------------------------------------------*/
/*---              DSP to IR function                      ---*/
/*------------------------------------------------------------*/

UInt disDSPInstr_MIPS_WRK ( UInt );

/*------------------------------------------------------------*/
/*---                  Debugging output                    ---*/
/*------------------------------------------------------------*/

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

/* ------------ MIPS32 DSP ASE(r2) accumulators ------------- */

extern UInt accumulatorGuestRegOffset( UInt );

/*------------------------------------------------------------*/
/*--- Helper bits and pieces for creating IR fragments.    ---*/
/*------------------------------------------------------------*/

static inline IRExpr *mkU8(UInt i)
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8((UChar) i));
}

/* Create an expression node for a 16-bit integer constant. */
static inline IRExpr *mkU16(UInt i)
{
   return IRExpr_Const(IRConst_U16(i));
}

/* Create an expression node for a 32-bit integer constant. */
static inline IRExpr *mkU32(UInt i)
{
   return IRExpr_Const(IRConst_U32(i));
}

/* Create an expression node for a 64-bit integer constant. */
static inline IRExpr *mkU64(ULong i)
{
   return IRExpr_Const(IRConst_U64(i));
}

static inline IRExpr *mkexpr(IRTemp tmp)
{
   return IRExpr_RdTmp(tmp);
}

static inline IRExpr *unop(IROp op, IRExpr * a)
{
   return IRExpr_Unop(op, a);
}

static inline IRExpr *binop(IROp op, IRExpr * a1, IRExpr * a2)
{
   return IRExpr_Binop(op, a1, a2);
}

static inline IRExpr *triop(IROp op, IRExpr * a1, IRExpr * a2, IRExpr * a3)
{
   return IRExpr_Triop(op, a1, a2, a3);
}

static inline IRExpr *qop ( IROp op, IRExpr * a1, IRExpr * a2, IRExpr * a3,
                     IRExpr * a4 )
{
   return IRExpr_Qop(op, a1, a2, a3, a4);
}

static inline IRExpr *load(IRType ty, IRExpr * addr)
{
   IRExpr *load1 = NULL;
#if defined (_MIPSEL)
   load1 = IRExpr_Load(Iend_LE, ty, addr);
#elif defined (_MIPSEB)
   load1 = IRExpr_Load(Iend_BE, ty, addr);
#endif
   return load1;
}

/* Add a statement to the list held by "irsb". */
static inline void stmt(IRStmt * st)
{
   addStmtToIRSB(irsb, st);
}

static inline void assign(IRTemp dst, IRExpr * e)
{
   stmt(IRStmt_WrTmp(dst, e));
}

static inline void store(IRExpr * addr, IRExpr * data)
{
#if defined (_MIPSEL)
   stmt(IRStmt_Store(Iend_LE, addr, data));
#elif defined (_MIPSEB)
   stmt(IRStmt_Store(Iend_BE, addr, data));
#endif
}

/* Generate a new temporary of the given type. */
static inline IRTemp newTemp(IRType ty)
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp(irsb->tyenv, ty);
}

static inline UShort extend_s_9to16(UInt x)
{
   return (UShort) ((((Int) x) << 23) >> 23);
}

static inline UShort extend_s_10to16(UInt x)
{
   return (UShort) ((((Int) x) << 22) >> 22);
}

static inline UInt extend_s_10to32(UInt x)
{
   return (UInt)((((Int) x) << 22) >> 22);
}

static inline ULong extend_s_10to64(UInt x)
{
   return (ULong)((((Long) x) << 54) >> 54);
}

static inline UInt extend_s_16to32(UInt x)
{
   return (UInt) ((((Int) x) << 16) >> 16);
}

static inline UInt extend_s_18to32(UInt x)
{
   return (UInt) ((((Int) x) << 14) >> 14);
}

static inline UInt extend_s_19to32(UInt x)
{
   return (UInt) ((((Int) x) << 13) >> 13);
}

static inline UInt extend_s_23to32(UInt x)
{
   return (UInt) ((((Int) x) << 9) >> 9);
}

static inline UInt extend_s_26to32(UInt x)
{
   return (UInt) ((((Int) x) << 6) >> 6);
}

static inline ULong extend_s_16to64 ( UInt x )
{
   return (ULong) ((((Long) x) << 48) >> 48);
}

static inline ULong extend_s_18to64 ( UInt x )
{
   return (ULong) ((((Long) x) << 46) >> 46);
}

static inline ULong extend_s_19to64(UInt x)
{
   return (ULong) ((((Long) x) << 45) >> 45);
}

static inline ULong extend_s_23to64(UInt x)
{
   return (ULong) ((((Long) x) << 41) >> 41);
}

static inline ULong extend_s_26to64(UInt x)
{
   return (ULong) ((((Long) x) << 38) >> 38);
}

static inline ULong extend_s_32to64 ( UInt x )
{
   return (ULong) ((((Long) x) << 32) >> 32);
}

extern IRExpr *getIReg(UInt);

extern void putIReg(UInt, IRExpr *);

/* Get value from accumulator (helper function for MIPS32 DSP ASE instructions).
   This function should be called before any other operation if widening
   multiplications are used. */
extern IRExpr *getAcc(UInt acNo);

extern IRExpr *getDSPControl(void);

extern IRExpr *mkNarrowTo32(IRType, IRExpr *);

extern void putLO(IRExpr *);

extern void putHI(IRExpr *);

/*------------------------------------------------------------*/
/*---                  Field helpers                       ---*/
/*------------------------------------------------------------*/

static inline UInt get_opcode(UInt mipsins)
{
   return (0xFC000000 & mipsins) >> 26;
}

static inline UInt get_rs(UInt mipsins)
{
   return (0x03E00000 & mipsins) >> 21;
}

static inline UInt get_rt(UInt mipsins)
{
   return (0x001F0000 & mipsins) >> 16;
}

static inline UInt get_imm(UInt mipsins)
{
   return (0x0000FFFF & mipsins);
}

static inline UInt get_instr_index(UInt mipsins)
{
   return (0x03FFFFFF & mipsins);
}

static inline UInt get_rd(UInt mipsins)
{
   return (0x0000F800 & mipsins) >> 11;
}

static inline UInt get_sa(UInt mipsins)
{
   return (0x000007C0 & mipsins) >> 6;
}

static inline UInt get_function(UInt mipsins)
{
   return (0x0000003F & mipsins);
}

static inline UInt get_ft(UInt mipsins)
{
   return (0x001F0000 & mipsins) >> 16;
}

static inline UInt get_fs(UInt mipsins)
{
   return (0x0000F800 & mipsins) >> 11;
}

static inline UInt get_fd(UInt mipsins)
{
   return (0x000007C0 & mipsins) >> 6;
}

static inline UInt get_mov_cc(UInt mipsins)
{
   return (0x001C0000 & mipsins) >> 18;
}

static inline UInt get_bc1_cc(UInt mipsins)
{
   return (0x001C0000 & mipsins) >> 18;
}

static inline UInt get_fpc_cc(UInt mipsins)
{
   return (0x00000700 & mipsins) >> 8;
}

static inline UInt get_tf(UInt mipsins)
{
   return (0x00010000 & mipsins) >> 16;
}

static inline UInt get_nd(UInt mipsins)
{
   return (0x00020000 & mipsins) >> 17;
}

static inline UInt get_fmt(UInt mipsins)
{
   return (0x03E00000 & mipsins) >> 21;
}

static inline UInt get_FC(UInt mipsins)
{
   return (0x000000F0 & mipsins) >> 4;
}

static inline UInt get_cond(UInt mipsins)
{
   return (0x0000000F & mipsins);
}

/* for break & syscall */
static inline UInt get_code(UInt mipsins)
{
   return (0xFFC0 & mipsins) >> 6;
}

static inline UInt get_lsb(UInt mipsins)
{
   return (0x7C0 & mipsins) >> 6;
}

static inline UInt get_msb(UInt mipsins)
{
   return (0x0000F800 & mipsins) >> 11;
}

static inline UInt get_rot(UInt mipsins)
{
   return (0x00200000 & mipsins) >> 21;
}

static inline UInt get_rotv(UInt mipsins)
{
   return (0x00000040 & mipsins) >> 6;
}

static inline UInt get_sel(UInt mipsins)
{
   return (0x00000007 & mipsins);
}

/* Get acc number for all MIPS32 DSP ASE(r2) instructions that use them,
   except for MFHI and MFLO. */
static inline UInt get_acNo(UInt mipsins)
{
   return (0x00001800 & mipsins) >> 11;
}

/* Get accumulator number for MIPS32 DSP ASEr2 MFHI and MFLO instructions. */
static inline UInt get_acNo_mfhilo(UInt mipsins)
{
   return (0x00600000 & mipsins) >> 21;
}

/* Get mask field (helper function for wrdsp instruction). */
static inline UInt get_wrdspMask(UInt mipsins)
{
   return (0x001ff800 & mipsins) >> 11;
}

/* Get mask field (helper function for rddsp instruction). */
static inline UInt get_rddspMask(UInt mipsins)
{
   return (0x03ff0000 & mipsins) >> 16;
}

/* Get shift field (helper function for DSP ASE instructions). */
static inline UInt get_shift(UInt mipsins)
{
   return (0x03f00000 & mipsins) >> 20;
}

/* Get immediate field for DSP ASE instructions. */
static inline UInt get_dspImm(UInt mipsins)
{
   return (0x03ff0000 & mipsins) >> 16;
}


#endif

/*---------------------------------------------------------------*/
/*--- end                                         mips_defs.h ---*/
/*---------------------------------------------------------------*/
