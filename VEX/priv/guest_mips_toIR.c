
/*--------------------------------------------------------------------*/
/*--- begin                                      guest_mips_toIR.c ---*/
/*--------------------------------------------------------------------*/

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

/* Translates MIPS code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_mips32.h"
#include "libvex_guest_mips64.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_mips_defs.h"

/*------------------------------------------------------------*/
/*---                      Globals                         ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of a instruction, so
   that we don't have to pass them around endlessly. CONST means does
   not change during translation of the instruction. */

/* CONST: is the host bigendian?  This has to do with float vs double
   register accesses on VFP, but it's complex and not properly thought
   out. */
static Bool host_is_bigendian;

/* Pointer to the guest code area. */
static UChar *guest_code;

/* CONST: The guest address for the instruction currently being
   translated. */
#if defined(VGP_mips32_linux)
static Addr32 guest_PC_curr_instr;
#else
static Addr64 guest_PC_curr_instr;
#endif

/* MOD: The IRSB* into which we're generating code. */
static IRSB *irsb;

/* Is our guest binary 32 or 64bit? Set at each call to
   disInstr_MIPS below. */
static Bool mode64 = False;

/* CPU has FPU and 32 dbl. prec. FP registers. */
static Bool fp_mode64 = False;

/* Define 1.0 in single and double precision. */
#define ONE_SINGLE 0x3F800000
#define ONE_DOUBLE 0x3FF0000000000000ULL

/*------------------------------------------------------------*/
/*---                  Debugging output                    ---*/
/*------------------------------------------------------------*/

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- mips insn stream.                                    ---*/
/*------------------------------------------------------------*/

/* ---------------- Integer registers ---------------- */

static UInt integerGuestRegOffset(UInt iregNo)
{
   /* Do we care about endianness here?  We do if sub-parts of integer
      registers are accessed, but I don't think that ever happens on
      MIPS. */
   UInt ret;
   if (!mode64)
      switch (iregNo) {
         case 0:
            ret = offsetof(VexGuestMIPS32State, guest_r0); break;
         case 1:
            ret = offsetof(VexGuestMIPS32State, guest_r1); break;
         case 2:
            ret = offsetof(VexGuestMIPS32State, guest_r2); break;
         case 3:
            ret = offsetof(VexGuestMIPS32State, guest_r3); break;
         case 4:
            ret = offsetof(VexGuestMIPS32State, guest_r4); break;
         case 5:
            ret = offsetof(VexGuestMIPS32State, guest_r5); break;
         case 6:
            ret = offsetof(VexGuestMIPS32State, guest_r6); break;
         case 7:
            ret = offsetof(VexGuestMIPS32State, guest_r7); break;
         case 8:
            ret = offsetof(VexGuestMIPS32State, guest_r8); break;
         case 9:
            ret = offsetof(VexGuestMIPS32State, guest_r9); break;
         case 10:
            ret = offsetof(VexGuestMIPS32State, guest_r10); break;
         case 11:
            ret = offsetof(VexGuestMIPS32State, guest_r11); break;
         case 12:
            ret = offsetof(VexGuestMIPS32State, guest_r12); break;
         case 13:
            ret = offsetof(VexGuestMIPS32State, guest_r13); break;
         case 14:
            ret = offsetof(VexGuestMIPS32State, guest_r14); break;
         case 15:
            ret = offsetof(VexGuestMIPS32State, guest_r15); break;
         case 16:
            ret = offsetof(VexGuestMIPS32State, guest_r16); break;
         case 17:
            ret = offsetof(VexGuestMIPS32State, guest_r17); break;
         case 18:
            ret = offsetof(VexGuestMIPS32State, guest_r18); break;
         case 19:
            ret = offsetof(VexGuestMIPS32State, guest_r19); break;
         case 20:
            ret = offsetof(VexGuestMIPS32State, guest_r20); break;
         case 21:
            ret = offsetof(VexGuestMIPS32State, guest_r21); break;
         case 22:
            ret = offsetof(VexGuestMIPS32State, guest_r22); break;
         case 23:
            ret = offsetof(VexGuestMIPS32State, guest_r23); break;
         case 24:
            ret = offsetof(VexGuestMIPS32State, guest_r24); break;
         case 25:
            ret = offsetof(VexGuestMIPS32State, guest_r25); break;
         case 26:
            ret = offsetof(VexGuestMIPS32State, guest_r26); break;
         case 27:
            ret = offsetof(VexGuestMIPS32State, guest_r27); break;
         case 28:
            ret = offsetof(VexGuestMIPS32State, guest_r28); break;
         case 29:
            ret = offsetof(VexGuestMIPS32State, guest_r29); break;
         case 30:
            ret = offsetof(VexGuestMIPS32State, guest_r30); break;
         case 31:
            ret = offsetof(VexGuestMIPS32State, guest_r31); break;
         default:
            vassert(0);
            break;
      }
   else
      switch (iregNo) {
         case 0:
            ret = offsetof(VexGuestMIPS64State, guest_r0); break;
         case 1:
            ret = offsetof(VexGuestMIPS64State, guest_r1); break;
         case 2:
            ret = offsetof(VexGuestMIPS64State, guest_r2); break;
         case 3:
            ret = offsetof(VexGuestMIPS64State, guest_r3); break;
         case 4:
            ret = offsetof(VexGuestMIPS64State, guest_r4); break;
         case 5:
            ret = offsetof(VexGuestMIPS64State, guest_r5); break;
         case 6:
            ret = offsetof(VexGuestMIPS64State, guest_r6); break;
         case 7:
            ret = offsetof(VexGuestMIPS64State, guest_r7); break;
         case 8:
            ret = offsetof(VexGuestMIPS64State, guest_r8); break;
         case 9:
            ret = offsetof(VexGuestMIPS64State, guest_r9); break;
         case 10:
            ret = offsetof(VexGuestMIPS64State, guest_r10); break;
         case 11:
            ret = offsetof(VexGuestMIPS64State, guest_r11); break;
         case 12:
            ret = offsetof(VexGuestMIPS64State, guest_r12); break;
         case 13:
            ret = offsetof(VexGuestMIPS64State, guest_r13); break;
         case 14:
            ret = offsetof(VexGuestMIPS64State, guest_r14); break;
         case 15:
            ret = offsetof(VexGuestMIPS64State, guest_r15); break;
         case 16:
            ret = offsetof(VexGuestMIPS64State, guest_r16); break;
         case 17:
            ret = offsetof(VexGuestMIPS64State, guest_r17); break;
         case 18:
            ret = offsetof(VexGuestMIPS64State, guest_r18); break;
         case 19:
            ret = offsetof(VexGuestMIPS64State, guest_r19); break;
         case 20:
            ret = offsetof(VexGuestMIPS64State, guest_r20); break;
         case 21:
            ret = offsetof(VexGuestMIPS64State, guest_r21); break;
         case 22:
            ret = offsetof(VexGuestMIPS64State, guest_r22); break;
         case 23:
            ret = offsetof(VexGuestMIPS64State, guest_r23); break;
         case 24:
            ret = offsetof(VexGuestMIPS64State, guest_r24); break;
         case 25:
            ret = offsetof(VexGuestMIPS64State, guest_r25); break;
         case 26:
            ret = offsetof(VexGuestMIPS64State, guest_r26); break;
         case 27:
            ret = offsetof(VexGuestMIPS64State, guest_r27); break;
         case 28:
            ret = offsetof(VexGuestMIPS64State, guest_r28); break;
         case 29:
            ret = offsetof(VexGuestMIPS64State, guest_r29); break;
         case 30:
            ret = offsetof(VexGuestMIPS64State, guest_r30); break;
         case 31:
            ret = offsetof(VexGuestMIPS64State, guest_r31); break;
         default:
            vassert(0);
            break;
      }
   return ret;
}

#if defined(VGP_mips32_linux)
#define OFFB_PC     offsetof(VexGuestMIPS32State, guest_PC)
#else
#define OFFB_PC     offsetof(VexGuestMIPS64State, guest_PC)
#endif

/* ---------------- Floating point registers ---------------- */

static UInt floatGuestRegOffset(UInt fregNo)
{
   vassert(fregNo < 32);
   UInt ret;
   if (!mode64)
      switch (fregNo) {
         case 0:
            ret = offsetof(VexGuestMIPS32State, guest_f0); break;
         case 1:
            ret = offsetof(VexGuestMIPS32State, guest_f1); break;
         case 2:
            ret = offsetof(VexGuestMIPS32State, guest_f2); break;
         case 3:
            ret = offsetof(VexGuestMIPS32State, guest_f3); break;
         case 4:
            ret = offsetof(VexGuestMIPS32State, guest_f4); break;
         case 5:
            ret = offsetof(VexGuestMIPS32State, guest_f5); break;
         case 6:
            ret = offsetof(VexGuestMIPS32State, guest_f6); break;
         case 7:
            ret = offsetof(VexGuestMIPS32State, guest_f7); break;
         case 8:
            ret = offsetof(VexGuestMIPS32State, guest_f8); break;
         case 9:
            ret = offsetof(VexGuestMIPS32State, guest_f9); break;
         case 10:
            ret = offsetof(VexGuestMIPS32State, guest_f10); break;
         case 11:
            ret = offsetof(VexGuestMIPS32State, guest_f11); break;
         case 12:
            ret = offsetof(VexGuestMIPS32State, guest_f12); break;
         case 13:
            ret = offsetof(VexGuestMIPS32State, guest_f13); break;
         case 14:
            ret = offsetof(VexGuestMIPS32State, guest_f14); break;
         case 15:
            ret = offsetof(VexGuestMIPS32State, guest_f15); break;
         case 16:
            ret = offsetof(VexGuestMIPS32State, guest_f16); break;
         case 17:
            ret = offsetof(VexGuestMIPS32State, guest_f17); break;
         case 18:
            ret = offsetof(VexGuestMIPS32State, guest_f18); break;
         case 19:
            ret = offsetof(VexGuestMIPS32State, guest_f19); break;
         case 20:
            ret = offsetof(VexGuestMIPS32State, guest_f20); break;
         case 21:
            ret = offsetof(VexGuestMIPS32State, guest_f21); break;
         case 22:
            ret = offsetof(VexGuestMIPS32State, guest_f22); break;
         case 23:
            ret = offsetof(VexGuestMIPS32State, guest_f23); break;
         case 24:
            ret = offsetof(VexGuestMIPS32State, guest_f24); break;
         case 25:
            ret = offsetof(VexGuestMIPS32State, guest_f25); break;
         case 26:
            ret = offsetof(VexGuestMIPS32State, guest_f26); break;
         case 27:
            ret = offsetof(VexGuestMIPS32State, guest_f27); break;
         case 28:
            ret = offsetof(VexGuestMIPS32State, guest_f28); break;
         case 29:
            ret = offsetof(VexGuestMIPS32State, guest_f29); break;
         case 30:
            ret = offsetof(VexGuestMIPS32State, guest_f30); break;
         case 31:
            ret = offsetof(VexGuestMIPS32State, guest_f31); break;
         default:
            vassert(0);
            break;
      }
   else
      switch (fregNo) {
         case 0:
            ret = offsetof(VexGuestMIPS64State, guest_f0); break;
         case 1:
            ret = offsetof(VexGuestMIPS64State, guest_f1); break;
         case 2:
            ret = offsetof(VexGuestMIPS64State, guest_f2); break;
         case 3:
            ret = offsetof(VexGuestMIPS64State, guest_f3); break;
         case 4:
            ret = offsetof(VexGuestMIPS64State, guest_f4); break;
         case 5:
            ret = offsetof(VexGuestMIPS64State, guest_f5); break;
         case 6:
            ret = offsetof(VexGuestMIPS64State, guest_f6); break;
         case 7:
            ret = offsetof(VexGuestMIPS64State, guest_f7); break;
         case 8:
            ret = offsetof(VexGuestMIPS64State, guest_f8); break;
         case 9:
            ret = offsetof(VexGuestMIPS64State, guest_f9); break;
         case 10:
            ret = offsetof(VexGuestMIPS64State, guest_f10); break;
         case 11:
            ret = offsetof(VexGuestMIPS64State, guest_f11); break;
         case 12:
            ret = offsetof(VexGuestMIPS64State, guest_f12); break;
         case 13:
            ret = offsetof(VexGuestMIPS64State, guest_f13); break;
         case 14:
            ret = offsetof(VexGuestMIPS64State, guest_f14); break;
         case 15:
            ret = offsetof(VexGuestMIPS64State, guest_f15); break;
         case 16:
            ret = offsetof(VexGuestMIPS64State, guest_f16); break;
         case 17:
            ret = offsetof(VexGuestMIPS64State, guest_f17); break;
         case 18:
            ret = offsetof(VexGuestMIPS64State, guest_f18); break;
         case 19:
            ret = offsetof(VexGuestMIPS64State, guest_f19); break;
         case 20:
            ret = offsetof(VexGuestMIPS64State, guest_f20); break;
         case 21:
            ret = offsetof(VexGuestMIPS64State, guest_f21); break;
         case 22:
            ret = offsetof(VexGuestMIPS64State, guest_f22); break;
         case 23:
            ret = offsetof(VexGuestMIPS64State, guest_f23); break;
         case 24:
            ret = offsetof(VexGuestMIPS64State, guest_f24); break;
         case 25:
            ret = offsetof(VexGuestMIPS64State, guest_f25); break;
         case 26:
            ret = offsetof(VexGuestMIPS64State, guest_f26); break;
         case 27:
            ret = offsetof(VexGuestMIPS64State, guest_f27); break;
         case 28:
            ret = offsetof(VexGuestMIPS64State, guest_f28); break;
         case 29:
            ret = offsetof(VexGuestMIPS64State, guest_f29); break;
         case 30:
            ret = offsetof(VexGuestMIPS64State, guest_f30); break;
         case 31:
            ret = offsetof(VexGuestMIPS64State, guest_f31); break;
         default:
            vassert(0);
            break;
      }
   return ret;
}

/* ---------------- MIPS32 DSP ASE(r2) accumulators ---------------- */

static UInt accumulatorGuestRegOffset(UInt acNo)
{
   vassert(!mode64);
   vassert(acNo <= 3);
   UInt ret;
   switch (acNo) {
      case 0:
         ret = offsetof(VexGuestMIPS32State, guest_ac0); break;
      case 1:
         ret = offsetof(VexGuestMIPS32State, guest_ac1); break;
      case 2:
         ret = offsetof(VexGuestMIPS32State, guest_ac2); break;
      case 3:
         ret = offsetof(VexGuestMIPS32State, guest_ac3); break;
      default:
         vassert(0);
    break;
   }
   return ret;
}

/* Do a endian load of a 32-bit word, regardless of the endianness of the
   underlying host. */
static inline UInt getUInt(UChar * p)
{
   UInt w = 0;
#if defined (_MIPSEL)
   w = (w << 8) | p[3];
   w = (w << 8) | p[2];
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
#elif defined (_MIPSEB)
   w = (w << 8) | p[0];
   w = (w << 8) | p[1];
   w = (w << 8) | p[2];
   w = (w << 8) | p[3];
#endif
   return w;
}

#define BITS2(_b1,_b0) \
   (((_b1) << 1) | (_b0))

#define BITS3(_b2,_b1,_b0)                      \
  (((_b2) << 2) | ((_b1) << 1) | (_b0))

#define BITS4(_b3,_b2,_b1,_b0) \
   (((_b3) << 3) | ((_b2) << 2) | ((_b1) << 1) | (_b0))

#define BITS5(_b4,_b3,_b2,_b1,_b0)  \
   (((_b4) << 4) | BITS4((_b3),(_b2),(_b1),(_b0)))

#define BITS6(_b5,_b4,_b3,_b2,_b1,_b0)  \
   ((BITS2((_b5),(_b4)) << 4) \
    | BITS4((_b3),(_b2),(_b1),(_b0)))

#define BITS8(_b7,_b6,_b5,_b4,_b3,_b2,_b1,_b0)  \
   ((BITS4((_b7),(_b6),(_b5),(_b4)) << 4) \
    | BITS4((_b3),(_b2),(_b1),(_b0)))

#define LOAD_STORE_PATTERN \
   t1 = newTemp(mode64 ? Ity_I64 : Ity_I32); \
      if(!mode64) \
         assign(t1, binop(Iop_Add32, getIReg(rs), \
                                     mkU32(extend_s_16to32(imm)))); \
      else \
         assign(t1, binop(Iop_Add64, getIReg(rs), \
                                     mkU64(extend_s_16to64(imm)))); \

#define LOADX_STORE_PATTERN \
   t1 = newTemp(mode64 ? Ity_I64 : Ity_I32); \
      if(!mode64) \
         assign(t1, binop(Iop_Add32, getIReg(regRs), getIReg(regRt))); \
      else \
         assign(t1, binop(Iop_Add64, getIReg(regRs), getIReg(regRt)));

#define LWX_SWX_PATTERN64 \
   t2 = newTemp(Ity_I64); \
   assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFFCULL))); \
   t4 = newTemp(Ity_I32); \
   assign(t4, mkNarrowTo32( ty, binop(Iop_And64, \
                                      mkexpr(t1), mkU64(0x3))));

#define LWX_SWX_PATTERN64_1 \
   t2 = newTemp(Ity_I64); \
   assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFF8ULL))); \
   t4 = newTemp(Ity_I64); \
   assign(t4, binop(Iop_And64, mkexpr(t1), mkU64(0x7)));

#define LWX_SWX_PATTERN \
   t2 = newTemp(Ity_I32); \
   assign(t2, binop(Iop_And32, mkexpr(t1), mkU32(0xFFFFFFFC))); \
   t4 = newTemp(Ity_I32); \
   assign(t4, binop(Iop_And32, mkexpr(t1), mkU32(0x00000003)))

#define SXXV_PATTERN(op) \
   putIReg(rd, binop(op, \
         getIReg(rt), \
            unop(Iop_32to8, \
               binop(Iop_And32, \
                  getIReg(rs), \
                  mkU32(0x0000001F) \
               ) \
            ) \
         ) \
      )

#define SXXV_PATTERN64(op) \
   putIReg(rd, mkWidenFrom32(ty, binop(op, \
           mkNarrowTo32(ty, getIReg(rt)), \
             unop(Iop_32to8, \
                binop(Iop_And32, \
                   mkNarrowTo32(ty, getIReg(rs)), \
                   mkU32(0x0000001F) \
                ) \
             ) \
          ), True \
       ))

#define SXX_PATTERN(op) \
   putIReg(rd, binop(op, getIReg(rt), mkU8(sa)));

#define ALU_PATTERN(op) \
   putIReg(rd, binop(op, getIReg(rs), getIReg(rt)));

#define ALUI_PATTERN(op) \
   putIReg(rt, binop(op, getIReg(rs), mkU32(imm)));

#define ALUI_PATTERN64(op) \
   putIReg(rt, binop(op, getIReg(rs), mkU64(imm)));

#define ALU_PATTERN64(op) \
   putIReg(rd, mkWidenFrom32(ty, binop(op, \
                             mkNarrowTo32(ty, getIReg(rs)), \
                             mkNarrowTo32(ty, getIReg(rt))), True));

#define FP_CONDITIONAL_CODE \
   t3 = newTemp(Ity_I32);   \
   assign(t3, binop(Iop_And32, \
                 IRExpr_ITE( binop(Iop_CmpEQ32, mkU32(cc), mkU32(0)), \
                             binop(Iop_Shr32, getFCSR(), mkU8(23)), \
                             binop(Iop_Shr32, getFCSR(), mkU8(24+cc))), \
                 mkU32(0x1)));

#define ILLEGAL_INSTRUCTON \
   putPC(mkU32(guest_PC_curr_instr + 4)); \
   dres.jk_StopHere = Ijk_SigILL; \
   dres.whatNext    = Dis_StopHere;

/*------------------------------------------------------------*/
/*---                  Field helpers                       ---*/
/*------------------------------------------------------------*/

static UInt get_opcode(UInt mipsins)
{
   return (0xFC000000 & mipsins) >> 26;
}

static UInt get_rs(UInt mipsins)
{
   return (0x03E00000 & mipsins) >> 21;
}

static UInt get_rt(UInt mipsins)
{
   return (0x001F0000 & mipsins) >> 16;
}

static UInt get_imm(UInt mipsins)
{
   return (0x0000FFFF & mipsins);
}

static UInt get_instr_index(UInt mipsins)
{
   return (0x03FFFFFF & mipsins);
}

static UInt get_rd(UInt mipsins)
{
   return (0x0000F800 & mipsins) >> 11;
}

static UInt get_sa(UInt mipsins)
{
   return (0x000007C0 & mipsins) >> 6;
}

static UInt get_function(UInt mipsins)
{
   return (0x0000003F & mipsins);
}

static UInt get_ft(UInt mipsins)
{
   return (0x001F0000 & mipsins) >> 16;
}

static UInt get_fs(UInt mipsins)
{
   return (0x0000F800 & mipsins) >> 11;
}

static UInt get_fd(UInt mipsins)
{
   return (0x000007C0 & mipsins) >> 6;
}

static UInt get_mov_cc(UInt mipsins)
{
   return (0x001C0000 & mipsins) >> 18;
}

static UInt get_bc1_cc(UInt mipsins)
{
   return (0x001C0000 & mipsins) >> 18;
}

static UInt get_fpc_cc(UInt mipsins)
{
   return (0x00000700 & mipsins) >> 8;
}

static UInt get_tf(UInt mipsins)
{
   return (0x00010000 & mipsins) >> 16;
}

static UInt get_nd(UInt mipsins)
{
   return (0x00020000 & mipsins) >> 17;
}

static UInt get_fmt(UInt mipsins)
{
   return (0x03E00000 & mipsins) >> 21;
}

static UInt get_FC(UInt mipsins)
{
   return (0x000000F0 & mipsins) >> 4;
}

static UInt get_cond(UInt mipsins)
{
   return (0x0000000F & mipsins);
}

/* for break & syscall */
static UInt get_code(UInt mipsins)
{
   return (0xFFC0 & mipsins) >> 6;
}

static UInt get_lsb(UInt mipsins)
{
   return (0x7C0 & mipsins) >> 6;
}

static UInt get_msb(UInt mipsins)
{
   return (0x0000F800 & mipsins) >> 11;
}

static UInt get_rot(UInt mipsins)
{
   return (0x00200000 & mipsins) >> 21;
}

static UInt get_rotv(UInt mipsins)
{
   return (0x00000040 & mipsins) >> 6;
}

static UInt get_sel(UInt mipsins)
{
   return (0x00000007 & mipsins);
}

/* Get acc number for all MIPS32 DSP ASE(r2) instructions that use them,
   except for MFHI and MFLO. */
static UInt get_acNo(UInt mipsins)
{
   return (0x00001800 & mipsins) >> 11;
}

/* Get accumulator number for MIPS32 DSP ASEr2 MFHI and MFLO instructions. */
static UInt get_acNo_mfhilo(UInt mipsins)
{
   return (0x00600000 & mipsins) >> 21;
}

/* Get mask field (helper function for wrdsp instruction). */
static UInt get_wrdspMask(UInt mipsins)
{
   return (0x001ff800 & mipsins) >> 11;
}

/* Get mask field (helper function for rddsp instruction). */
static UInt get_rddspMask(UInt mipsins)
{
   return (0x03ff0000 & mipsins) >> 16;
}

/* Get shift field (helper function for DSP ASE instructions). */
static UInt get_shift(UInt mipsins)
{
   return (0x03f00000 & mipsins) >> 20;
}

/* Get immediate field for DSP ASE instructions. */
static UInt get_dspImm(UInt mipsins)
{
   return (0x03ff0000 & mipsins) >> 16;
}

static Bool branch_or_jump(UChar * addr)
{
   UInt fmt;
   UInt cins = getUInt(addr);

   UInt opcode = get_opcode(cins);
   UInt rt = get_rt(cins);
   UInt function = get_function(cins);

   /* bgtz, blez, bne, beq, jal */
   if (opcode == 0x07 || opcode == 0x06 || opcode == 0x05 || opcode == 0x04
       || opcode == 0x03 || opcode == 0x02) {
      return True;
   }

   /* bgez */
   if (opcode == 0x01 && rt == 0x01) {
      return True;
   }

   /* bgezal */
   if (opcode == 0x01 && rt == 0x11) {
      return True;
   }

   /* bltzal */
   if (opcode == 0x01 && rt == 0x10) {
      return True;
   }

   /* bltz */
   if (opcode == 0x01 && rt == 0x00) {
      return True;
   }

   /* jalr */
   if (opcode == 0x00 && function == 0x09) {
      return True;
   }

   /* jr */
   if (opcode == 0x00 && function == 0x08) {
      return True;
   }

   if (opcode == 0x11) {
      /*bc1f & bc1t */
      fmt = get_fmt(cins);
      if (fmt == 0x08) {
         return True;
      }
   }

   /* bposge32 */
   if (opcode == 0x01 && rt == 0x1c) {
      return True;
   }

   return False;
}

static Bool is_Branch_or_Jump_and_Link(UChar * addr)
{
   UInt cins = getUInt(addr);

   UInt opcode = get_opcode(cins);
   UInt rt = get_rt(cins);
   UInt function = get_function(cins);

   /* jal */
   if (opcode == 0x02) {
      return True;
   }

   /* bgezal */
   if (opcode == 0x01 && rt == 0x11) {
      return True;
   }

   /* bltzal */
   if (opcode == 0x01 && rt == 0x10) {
      return True;
   }

   /* jalr */
   if (opcode == 0x00 && function == 0x09) {
      return True;
   }

   return False;
}

static Bool branch_or_link_likely(UChar * addr)
{
   UInt cins = getUInt(addr);
   UInt opcode = get_opcode(cins);
   UInt rt = get_rt(cins);

   /* bgtzl, blezl, bnel, beql */
   if (opcode == 0x17 || opcode == 0x16 || opcode == 0x15 || opcode == 0x14)
      return True;

   /* bgezl */
   if (opcode == 0x01 && rt == 0x03)
      return True;

   /* bgezall */
   if (opcode == 0x01 && rt == 0x13)
      return True;

   /* bltzall */
   if (opcode == 0x01 && rt == 0x12)
      return True;

   /* bltzl */
   if (opcode == 0x01 && rt == 0x02)
      return True;

   return False;
}

/*------------------------------------------------------------*/
/*--- Helper bits and pieces for creating IR fragments.    ---*/
/*------------------------------------------------------------*/

static IRExpr *mkU8(UInt i)
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8((UChar) i));
}

/* Create an expression node for a 16-bit integer constant. */
static IRExpr *mkU16(UInt i)
{
   return IRExpr_Const(IRConst_U16(i));
}

/* Create an expression node for a 32-bit integer constant. */
static IRExpr *mkU32(UInt i)
{
   return IRExpr_Const(IRConst_U32(i));
}

/* Create an expression node for a 64-bit integer constant. */
static IRExpr *mkU64(ULong i)
{
   return IRExpr_Const(IRConst_U64(i));
}

static IRExpr *mkexpr(IRTemp tmp)
{
   return IRExpr_RdTmp(tmp);
}

static IRExpr *unop(IROp op, IRExpr * a)
{
   return IRExpr_Unop(op, a);
}

static IRExpr *binop(IROp op, IRExpr * a1, IRExpr * a2)
{
   return IRExpr_Binop(op, a1, a2);
}

static IRExpr *triop(IROp op, IRExpr * a1, IRExpr * a2, IRExpr * a3)
{
   return IRExpr_Triop(op, a1, a2, a3);
}

static IRExpr *qop ( IROp op, IRExpr * a1, IRExpr * a2, IRExpr * a3,
                     IRExpr * a4 )
{
   return IRExpr_Qop(op, a1, a2, a3, a4);
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

/* Add a statement to the list held by "irsb". */
static void stmt(IRStmt * st)
{
   addStmtToIRSB(irsb, st);
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

/* Generate a new temporary of the given type. */
static IRTemp newTemp(IRType ty)
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp(irsb->tyenv, ty);
}

/* Generate an expression for SRC rotated right by ROT. */
static IRExpr *genROR32(IRExpr * src, Int rot)
{
   vassert(rot >= 0 && rot < 32);
   if (rot == 0)
      return src;
   return binop(Iop_Or32, binop(Iop_Shl32, src, mkU8(32 - rot)),
                          binop(Iop_Shr32, src, mkU8(rot)));
}

static IRExpr *genRORV32(IRExpr * src, IRExpr * rs)
{
   IRTemp t0 = newTemp(Ity_I8);
   IRTemp t1 = newTemp(Ity_I8);

   assign(t0, unop(Iop_32to8, binop(Iop_And32, rs, mkU32(0x0000001F))));
   assign(t1, binop(Iop_Sub8, mkU8(32), mkexpr(t0)));
   return binop(Iop_Or32, binop(Iop_Shl32, src, mkexpr(t1)),
                          binop(Iop_Shr32, src, mkexpr(t0)));
}

static UShort extend_s_10to16(UInt x)
{
   return (UShort) ((((Int) x) << 22) >> 22);
}

static ULong extend_s_10to32(UInt x)
{
   return (ULong)((((Long) x) << 22) >> 22);
}

static ULong extend_s_10to64(UInt x)
{
   return (ULong)((((Long) x) << 54) >> 54);
}

static UInt extend_s_16to32(UInt x)
{
   return (UInt) ((((Int) x) << 16) >> 16);
}

static UInt extend_s_18to32(UInt x)
{
   return (UInt) ((((Int) x) << 14) >> 14);
}

static ULong extend_s_16to64 ( UInt x )
{
   return (ULong) ((((Long) x) << 48) >> 48);
}

static ULong extend_s_18to64 ( UInt x )
{
   return (ULong) ((((Long) x) << 46) >> 46);
}

static ULong extend_s_32to64 ( UInt x )
{
   return (ULong) ((((Long) x) << 32) >> 32);
}

static void jmp_lit32 ( /*MOD*/ DisResult* dres, IRJumpKind kind, Addr32 d32 )
{
   vassert(dres->whatNext    == Dis_Continue);
   vassert(dres->len         == 0);
   vassert(dres->continueAt  == 0);
   vassert(dres->jk_StopHere == Ijk_INVALID);
   dres->whatNext    = Dis_StopHere;
   dres->jk_StopHere = kind;
   stmt( IRStmt_Put( OFFB_PC, mkU32(d32) ) );
}

static void jmp_lit64 ( /*MOD*/ DisResult* dres, IRJumpKind kind, Addr64 d64 )
{
   vassert(dres->whatNext    == Dis_Continue);
   vassert(dres->len         == 0);
   vassert(dres->continueAt  == 0);
   vassert(dres->jk_StopHere == Ijk_INVALID);
   dres->whatNext    = Dis_StopHere;
   dres->jk_StopHere = kind;
   stmt(IRStmt_Put(OFFB_PC, mkU64(d64)));
}

/* Get value from accumulator (helper function for MIPS32 DSP ASE instructions).
   This function should be called before any other operation if widening
   multiplications are used. */
static IRExpr *getAcc(UInt acNo)
{
   vassert(!mode64);
   vassert(acNo <= 3);
   return IRExpr_Get(accumulatorGuestRegOffset(acNo), Ity_I64);
}

/* Get value from DSPControl register (helper function for MIPS32 DSP ASE
   instructions). */
static IRExpr *getDSPControl(void)
{
   vassert(!mode64);
   return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_DSPControl), Ity_I32);
}

/* Put value to DSPControl register. Expression e is written to DSPControl as
   is. If only certain bits of DSPControl need to be changed, it should be done
   before calling putDSPControl(). It could be done by reading DSPControl and
   ORing it with appropriate mask. */
static void putDSPControl(IRExpr * e)
{
   vassert(!mode64);
   stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_DSPControl), e));
}

/* Fetch a byte from the guest insn stream. */
static UChar getIByte(Int delta)
{
   return guest_code[delta];
}

static IRExpr *getIReg(UInt iregNo)
{
   if (0 == iregNo) {
      return mode64 ? mkU64(0x0) : mkU32(0x0);
   } else {
      IRType ty = mode64 ? Ity_I64 : Ity_I32;
      vassert(iregNo < 32);
      return IRExpr_Get(integerGuestRegOffset(iregNo), ty);
   }
}

static IRExpr *getHI(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_HI), Ity_I64);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_HI), Ity_I32);
}

static IRExpr *getLO(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_LO), Ity_I64);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_LO), Ity_I32);
}

static IRExpr *getFCSR(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_FCSR), Ity_I32);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_FCSR), Ity_I32);
}

/* Get byte from register reg, byte pos from 0 to 3 (or 7 for MIPS64) . */
static IRExpr *getByteFromReg(UInt reg, UInt byte_pos)
{
  UInt pos = byte_pos * 8;
  if (mode64)
      return unop(Iop_64to8, binop(Iop_And64,
                                   binop(Iop_Shr64, getIReg(reg), mkU8(pos)),
                                   mkU64(0xFF)));
   else
      return unop(Iop_32to8, binop(Iop_And32,
                                   binop(Iop_Shr32, getIReg(reg), mkU8(pos)),
                                   mkU32(0xFF)));
}

static void putFCSR(IRExpr * e)
{
   if (mode64)
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_FCSR), e));
   else
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_FCSR), e));
}

/* fs   - fpu source register number.
   inst - fpu instruction that needs to be executed.
   sz32 - size of source register.
   opN  - number of operads:
          1 - unary operation.
          2 - binary operation. */
static void calculateFCSR(UInt fs, UInt ft, UInt inst, Bool sz32, UInt opN)
{
   IRDirty *d;
   IRTemp fcsr = newTemp(Ity_I32);
   /* IRExpr_BBPTR() => Need to pass pointer to guest state to helper. */
   if (fp_mode64)
      d = unsafeIRDirty_1_N(fcsr, 0,
                            "mips_dirtyhelper_calculate_FCSR_fp64",
                            &mips_dirtyhelper_calculate_FCSR_fp64,
                            mkIRExprVec_4(IRExpr_BBPTR(),
                                          mkU32(fs),
                                          mkU32(ft),
                                          mkU32(inst)));
   else
      d = unsafeIRDirty_1_N(fcsr, 0,
                            "mips_dirtyhelper_calculate_FCSR_fp32",
                            &mips_dirtyhelper_calculate_FCSR_fp32,
                            mkIRExprVec_4(IRExpr_BBPTR(),
                                          mkU32(fs),
                                          mkU32(ft),
                                          mkU32(inst)));

   if (opN == 1) {  /* Unary operation. */
      /* Declare we're reading guest state. */
      if (sz32 || fp_mode64)
         d->nFxState = 2;
      else
         d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));

      d->fxState[0].fx     = Ifx_Read;  /* read */
      if (mode64)
         d->fxState[0].offset = offsetof(VexGuestMIPS64State, guest_FCSR);
      else
         d->fxState[0].offset = offsetof(VexGuestMIPS32State, guest_FCSR);
      d->fxState[0].size   = sizeof(UInt);
      d->fxState[1].fx     = Ifx_Read;  /* read */
      d->fxState[1].offset = floatGuestRegOffset(fs);
      d->fxState[1].size   = sizeof(ULong);

      if (!(sz32 || fp_mode64)) {
         d->fxState[2].fx     = Ifx_Read;  /* read */
         d->fxState[2].offset = floatGuestRegOffset(fs+1);
         d->fxState[2].size   = sizeof(ULong);
      }
   } else if (opN == 2) {  /* Binary operation. */
      /* Declare we're reading guest state. */
      if (sz32 || fp_mode64)
         d->nFxState = 3;
      else
         d->nFxState = 5;
      vex_bzero(&d->fxState, sizeof(d->fxState));

      d->fxState[0].fx     = Ifx_Read;  /* read */
      if (mode64)
         d->fxState[0].offset = offsetof(VexGuestMIPS64State, guest_FCSR);
      else
         d->fxState[0].offset = offsetof(VexGuestMIPS32State, guest_FCSR);
      d->fxState[0].size   = sizeof(UInt);
      d->fxState[1].fx     = Ifx_Read;  /* read */
      d->fxState[1].offset = floatGuestRegOffset(fs);
      d->fxState[1].size   = sizeof(ULong);
      d->fxState[2].fx     = Ifx_Read;  /* read */
      d->fxState[2].offset = floatGuestRegOffset(ft);
      d->fxState[2].size   = sizeof(ULong);

      if (!(sz32 || fp_mode64)) {
         d->fxState[3].fx     = Ifx_Read;  /* read */
         d->fxState[3].offset = floatGuestRegOffset(fs+1);
         d->fxState[3].size   = sizeof(ULong);
         d->fxState[4].fx     = Ifx_Read;  /* read */
         d->fxState[4].offset = floatGuestRegOffset(ft+1);
         d->fxState[4].size   = sizeof(ULong);
      }
   }

   stmt(IRStmt_Dirty(d));

   putFCSR(mkexpr(fcsr));
}

static IRExpr *getULR(void)
{
   if (mode64)
      return IRExpr_Get(offsetof(VexGuestMIPS64State, guest_ULR), Ity_I64);
   else
      return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_ULR), Ity_I32);
}

static void putIReg(UInt archreg, IRExpr * e)
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irsb->tyenv, e) == ty);
   if (archreg != 0)
      stmt(IRStmt_Put(integerGuestRegOffset(archreg), e));
}

static IRExpr *mkNarrowTo32(IRType ty, IRExpr * src)
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to32, src) : src;
}

static void putLO(IRExpr * e)
{
   if (mode64) {
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_LO), e));
   } else {
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_LO), e));
   /* Add value to lower 32 bits of ac0 to maintain compatibility between
      regular MIPS32 instruction set and MIPS DSP ASE. Keep higher 32bits
      unchanged. */
      IRTemp t_lo = newTemp(Ity_I32);
      IRTemp t_hi = newTemp(Ity_I32);
      assign(t_lo, e);
      assign(t_hi, unop(Iop_64HIto32, getAcc(0)));
      stmt(IRStmt_Put(accumulatorGuestRegOffset(0),
           binop(Iop_32HLto64, mkexpr(t_hi), mkexpr(t_lo))));
   }
}

static void putHI(IRExpr * e)
{
   if (mode64) {
      stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_HI), e));
   } else {
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_HI), e));
   /* Add value to higher 32 bits of ac0 to maintain compatibility between
      regular MIPS32 instruction set and MIPS DSP ASE. Keep lower 32bits
      unchanged. */
      IRTemp t_lo = newTemp(Ity_I32);
      IRTemp t_hi = newTemp(Ity_I32);
      assign(t_hi, e);
      assign(t_lo, unop(Iop_64to32, getAcc(0)));
      stmt(IRStmt_Put(accumulatorGuestRegOffset(0),
           binop(Iop_32HLto64, mkexpr(t_hi), mkexpr(t_lo))));
   }
}

/* Put value to accumulator(helper function for MIPS32 DSP ASE instructions). */
static void putAcc(UInt acNo, IRExpr * e)
{
   vassert(!mode64);
   vassert(acNo <= 3);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I64);
   stmt(IRStmt_Put(accumulatorGuestRegOffset(acNo), e));
/* If acNo = 0, split value to HI and LO regs in order to maintain compatibility
   between MIPS32 and MIPS DSP ASE insn sets. */
   if (0 == acNo) {
     putLO(unop(Iop_64to32, e));
     putHI(unop(Iop_64HIto32, e));
   }
}

static IRExpr *mkNarrowTo8 ( IRType ty, IRExpr * src )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to8, src) : unop(Iop_32to8, src);
}

static void putPC(IRExpr * e)
{
   stmt(IRStmt_Put(OFFB_PC, e));
}

static IRExpr *mkWidenFrom32(IRType ty, IRExpr * src, Bool sined)
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   if (ty == Ity_I32)
      return src;
   return (sined) ? unop(Iop_32Sto64, src) : unop(Iop_32Uto64, src);
}

/* Narrow 8/16/32 bit int expr to 8/16/32.  Clearly only some
   of these combinations make sense. */
static IRExpr *narrowTo(IRType dst_ty, IRExpr * e)
{
   IRType src_ty = typeOfIRExpr(irsb->tyenv, e);
   if (src_ty == dst_ty)
      return e;
   if (src_ty == Ity_I32 && dst_ty == Ity_I16)
      return unop(Iop_32to16, e);
   if (src_ty == Ity_I32 && dst_ty == Ity_I8)
      return unop(Iop_32to8, e);
   if (src_ty == Ity_I64 && dst_ty == Ity_I8) {
      vassert(mode64);
      return unop(Iop_64to8, e);
   }
   if (src_ty == Ity_I64 && dst_ty == Ity_I16) {
      vassert(mode64);
      return unop(Iop_64to16, e);
   }
   vpanic("narrowTo(mips)");
   return 0;
}

static IRExpr *getLoFromF64(IRType ty, IRExpr * src)
{
   vassert(ty == Ity_F32 || ty == Ity_F64);
   if (ty == Ity_F64) {
      IRTemp t0, t1;
      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I32);
      assign(t0, unop(Iop_ReinterpF64asI64, src));
      assign(t1, unop(Iop_64to32, mkexpr(t0)));
      return unop(Iop_ReinterpI32asF32, mkexpr(t1));
   } else
      return src;
}

static IRExpr *mkWidenFromF32(IRType ty, IRExpr * src)
{
   vassert(ty == Ity_F32 || ty == Ity_F64);
   if (ty == Ity_F64) {
      IRTemp t0 = newTemp(Ity_I32);
      IRTemp t1 = newTemp(Ity_I64);
      assign(t0, unop(Iop_ReinterpF32asI32, src));
      assign(t1, binop(Iop_32HLto64, mkU32(0x0), mkexpr(t0)));
      return unop(Iop_ReinterpI64asF64, mkexpr(t1));
   } else
      return src;
}

static IRExpr *dis_branch_likely(IRExpr * guard, UInt imm)
{
   ULong branch_offset;
   IRTemp t0;

   /* PC = PC + (SignExtend(signed_immed_24) << 2)
      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits)
      is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form
      a PC-relative effective target address. */
   if (mode64)
      branch_offset = extend_s_18to64(imm << 2);
   else
      branch_offset = extend_s_18to32(imm << 2);

   t0 = newTemp(Ity_I1);
   assign(t0, guard);

   if (mode64)
      stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                       IRConst_U64(guest_PC_curr_instr + 8), OFFB_PC));
   else
      stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring,
                       IRConst_U32(guest_PC_curr_instr + 8), OFFB_PC));

   irsb->jumpkind = Ijk_Boring;

   if (mode64)
      return mkU64(guest_PC_curr_instr + 4 + branch_offset);
   else
      return mkU32(guest_PC_curr_instr + 4 + branch_offset);
}

static void dis_branch(Bool link, IRExpr * guard, UInt imm, IRStmt ** set)
{
   ULong branch_offset;
   IRTemp t0;

   if (link) {  /* LR (GPR31) = addr of the 2nd instr after branch instr */
      if (mode64)
         putIReg(31, mkU64(guest_PC_curr_instr + 8));
      else
         putIReg(31, mkU32(guest_PC_curr_instr + 8));
   }

   /* PC = PC + (SignExtend(signed_immed_24) << 2)
      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits)
      is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form
      a PC-relative effective target address. */

   if (mode64)
      branch_offset = extend_s_18to64(imm << 2);
   else
      branch_offset = extend_s_18to32(imm << 2);

   t0 = newTemp(Ity_I1);
   assign(t0, guard);
   if (mode64)
      *set = IRStmt_Exit(mkexpr(t0), link ? Ijk_Call : Ijk_Boring,
                         IRConst_U64(guest_PC_curr_instr + 4 + branch_offset),
                         OFFB_PC);
   else
      *set = IRStmt_Exit(mkexpr(t0), link ? Ijk_Call : Ijk_Boring,
                         IRConst_U32(guest_PC_curr_instr + 4 +
                                     (UInt) branch_offset), OFFB_PC);
}

static IRExpr *getFReg(UInt fregNo)
{
   vassert(fregNo < 32);
   IRType ty = fp_mode64 ? Ity_F64 : Ity_F32;
   return IRExpr_Get(floatGuestRegOffset(fregNo), ty);
}

static IRExpr *getDReg(UInt dregNo)
{
   vassert(dregNo < 32);
   if (fp_mode64) {
      return IRExpr_Get(floatGuestRegOffset(dregNo), Ity_F64);
   } else {
      /* Read a floating point register pair and combine their contents into a
         64-bit value */
      IRTemp t0 = newTemp(Ity_F32);
      IRTemp t1 = newTemp(Ity_F32);
      IRTemp t2 = newTemp(Ity_F64);
      IRTemp t3 = newTemp(Ity_I32);
      IRTemp t4 = newTemp(Ity_I32);
      IRTemp t5 = newTemp(Ity_I64);

      assign(t0, getFReg(dregNo));
      assign(t1, getFReg(dregNo + 1));

      assign(t3, unop(Iop_ReinterpF32asI32, mkexpr(t0)));
      assign(t4, unop(Iop_ReinterpF32asI32, mkexpr(t1)));
      assign(t5, binop(Iop_32HLto64, mkexpr(t4), mkexpr(t3)));
      assign(t2, unop(Iop_ReinterpI64asF64, mkexpr(t5)));

      return mkexpr(t2);
   }
}

static void putFReg(UInt dregNo, IRExpr * e)
{
   vassert(dregNo < 32);
   IRType ty = fp_mode64 ? Ity_F64 : Ity_F32;
   vassert(typeOfIRExpr(irsb->tyenv, e) == ty);
   stmt(IRStmt_Put(floatGuestRegOffset(dregNo), e));
}

static void putDReg(UInt dregNo, IRExpr * e)
{
   if (fp_mode64) {
      vassert(dregNo < 32);
      IRType ty = Ity_F64;
      vassert(typeOfIRExpr(irsb->tyenv, e) == ty);
      stmt(IRStmt_Put(floatGuestRegOffset(dregNo), e));
   } else {
      vassert(dregNo < 32);
      vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_F64);
      IRTemp t1 = newTemp(Ity_F64);
      IRTemp t4 = newTemp(Ity_I32);
      IRTemp t5 = newTemp(Ity_I32);
      IRTemp t6 = newTemp(Ity_I64);
      assign(t1, e);
      assign(t6, unop(Iop_ReinterpF64asI64, mkexpr(t1)));
      assign(t4, unop(Iop_64HIto32, mkexpr(t6)));  /* hi */
      assign(t5, unop(Iop_64to32, mkexpr(t6)));    /* lo */
      putFReg(dregNo, unop(Iop_ReinterpI32asF32, mkexpr(t5)));
      putFReg(dregNo + 1, unop(Iop_ReinterpI32asF32, mkexpr(t4)));
   }
}

static void setFPUCondCode(IRExpr * e, UInt cc)
{
   if (cc == 0) {
      putFCSR(binop(Iop_And32, getFCSR(), mkU32(0xFF7FFFFF)));
      putFCSR(binop(Iop_Or32, getFCSR(), binop(Iop_Shl32, e, mkU8(23))));
   } else {
      putFCSR(binop(Iop_And32, getFCSR(), unop(Iop_Not32,
                               binop(Iop_Shl32, mkU32(0x01000000), mkU8(cc)))));
      putFCSR(binop(Iop_Or32, getFCSR(), binop(Iop_Shl32, e, mkU8(24 + cc))));
   }
}

static IRExpr* get_IR_roundingmode ( void )
{
/*
   rounding mode | MIPS | IR
   ------------------------
   to nearest    | 00  | 00
   to zero       | 01  | 11
   to +infinity  | 10  | 10
   to -infinity  | 11  | 01
*/
   IRTemp rm_MIPS = newTemp(Ity_I32);
   /* Last two bits in FCSR are rounding mode. */

   if (mode64)
      assign(rm_MIPS, binop(Iop_And32, IRExpr_Get(offsetof(VexGuestMIPS64State,
                                       guest_FCSR), Ity_I32), mkU32(3)));
   else
      assign(rm_MIPS, binop(Iop_And32, IRExpr_Get(offsetof(VexGuestMIPS32State,
                                       guest_FCSR), Ity_I32), mkU32(3)));

   /* rm_IR = XOR( rm_MIPS32, (rm_MIPS32 << 1) & 2) */

   return binop(Iop_Xor32, mkexpr(rm_MIPS), binop(Iop_And32,
                binop(Iop_Shl32, mkexpr(rm_MIPS), mkU8(1)), mkU32(2)));
}

/* sz, ULong -> IRExpr */
static IRExpr *mkSzImm ( IRType ty, ULong imm64 )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? mkU64(imm64) : mkU32((UInt) imm64);
}

static IRConst *mkSzConst ( IRType ty, ULong imm64 )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return (ty == Ity_I64 ? IRConst_U64(imm64) : IRConst_U32((UInt) imm64));
}

/* Make sure we get valid 32 and 64bit addresses */
static Addr64 mkSzAddr ( IRType ty, Addr64 addr )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return (ty == Ity_I64 ? (Addr64) addr :
                           (Addr64) extend_s_32to64(toUInt(addr)));
}

/* Shift and Rotate instructions for MIPS64 */
static Bool dis_instr_shrt ( UInt theInstr )
{
   UInt opc2 = get_function(theInstr);
   UChar regRs = get_rs(theInstr);
   UChar regRt = get_rt(theInstr);
   UChar regRd = get_rd(theInstr);
   UChar uImmsa = get_sa(theInstr);
   Long sImmsa = extend_s_16to64(uImmsa);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IRTemp tmp = newTemp(ty);
   IRTemp tmpOr = newTemp(ty);
   IRTemp tmpRt = newTemp(ty);
   IRTemp tmpRs = newTemp(ty);
   IRTemp tmpRd = newTemp(ty);

   assign(tmpRs, getIReg(regRs));
   assign(tmpRt, getIReg(regRt));

   switch (opc2) {
      case 0x3A:
         if ((regRs & 0x01) == 0) {
            /* Doubleword Shift Right Logical - DSRL; MIPS64 */
            DIP("dsrl r%u, r%u, %d", regRd, regRt, (Int)sImmsa);
            assign(tmpRd, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(uImmsa)));
            putIReg(regRd, mkexpr(tmpRd));
         } else if ((regRs & 0x01) == 1) {
            /* Doubleword Rotate Right - DROTR; MIPS64r2 */
            vassert(mode64);
            DIP("drotr r%u, r%u, %d", regRd, regRt, (Int)sImmsa);
            IRTemp tmpL = newTemp(ty);
            IRTemp tmpR = newTemp(ty);
            assign(tmpR, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(uImmsa)));
            assign(tmp, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(63 - uImmsa)));
            assign(tmpL, binop(Iop_Shl64, mkexpr(tmp), mkU8(1)));
            assign(tmpRd, binop(Iop_Or64, mkexpr(tmpL), mkexpr(tmpR)));
            putIReg(regRd, mkexpr(tmpRd));
         } else
            return False;
         break;

      case 0x3E:
         if ((regRs & 0x01) == 0) {
            /* Doubleword Shift Right Logical Plus 32 - DSRL32; MIPS64 */
            DIP("dsrl32 r%u, r%u, %d", regRd, regRt, (Int)(sImmsa + 32));
            assign(tmpRd, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(uImmsa + 32)));
            putIReg(regRd, mkexpr(tmpRd));
         } else if ((regRs & 0x01) == 1) {
            /* Doubleword Rotate Right Plus 32 - DROTR32; MIPS64r2 */
            DIP("drotr32 r%u, r%u, %d", regRd, regRt, (Int)sImmsa);
            vassert(mode64);
            IRTemp tmpL = newTemp(ty);
            IRTemp tmpR = newTemp(ty);
            /* (tmpRt >> sa) | (tmpRt << (64 - sa)) */
            assign(tmpR, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(uImmsa + 32)));
            assign(tmp, binop(Iop_Shl64, mkexpr(tmpRt),
                              mkU8(63 - (uImmsa + 32))));
            assign(tmpL, binop(Iop_Shl64, mkexpr(tmp), mkU8(1)));
            assign(tmpRd, binop(Iop_Or64, mkexpr(tmpL), mkexpr(tmpR)));
            putIReg(regRd, mkexpr(tmpRd));
         } else
            return False;
         break;

      case 0x16:
         if ((uImmsa & 0x01) == 0) {
            /* Doubleword Shift Right Logical Variable - DSRLV; MIPS64 */
            DIP("dsrlv r%u, r%u, r%u", regRd, regRt, regRs);
            IRTemp tmpRs8 = newTemp(Ity_I8);
            /* s = tmpRs[5..0] */
            assign(tmp, binop(Iop_And64, mkexpr(tmpRs), mkU64(63)));
            assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp)));
            assign(tmpRd, binop(Iop_Shr64, mkexpr(tmpRt), mkexpr(tmpRs8)));
            putIReg(regRd, mkexpr(tmpRd));
         } else if ((uImmsa & 0x01) == 1) {
            /* Doubleword Rotate Right Variable - DROTRV; MIPS64r2 */
            DIP("drotrv r%u, r%u, r%u", regRd, regRt, regRs);
            IRTemp tmpL = newTemp(ty);
            IRTemp tmpR = newTemp(ty);
            IRTemp tmpRs8 = newTemp(Ity_I8);
            IRTemp tmpLs8 = newTemp(Ity_I8);
            IRTemp tmp64 = newTemp(ty);
            /* s = tmpRs[5...0]
               m = 64 - s
               (tmpRt << s) | (tmpRt >> m) */

            assign(tmp64, binop(Iop_And64, mkexpr(tmpRs), mkSzImm(ty, 63)));
            assign(tmp, binop(Iop_Sub64, mkU64(63), mkexpr(tmp64)));

            assign(tmpLs8, mkNarrowTo8(ty, mkexpr(tmp)));
            assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp64)));

            assign(tmpR, binop(Iop_Shr64, mkexpr(tmpRt), mkexpr(tmpRs8)));
            assign(tmpL, binop(Iop_Shl64, mkexpr(tmpRt), mkexpr(tmpLs8)));
            assign(tmpRd, binop(Iop_Shl64, mkexpr(tmpL), mkU8(1)));
            assign(tmpOr, binop(Iop_Or64, mkexpr(tmpRd), mkexpr(tmpR)));

            putIReg(regRd, mkexpr(tmpOr));
         } else
            return False;
         break;

      case 0x38:  /* Doubleword Shift Left Logical - DSLL; MIPS64 */
         DIP("dsll r%u, r%u, %d", regRd, regRt, (Int)sImmsa);
         vassert(mode64);
         assign(tmpRd, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(uImmsa)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      case 0x3C:  /* Doubleword Shift Left Logical Plus 32 - DSLL32; MIPS64 */
         DIP("dsll32 r%u, r%u, %d", regRd, regRt, (Int)sImmsa);
         assign(tmpRd, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(uImmsa + 32)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      case 0x14: {  /* Doubleword Shift Left Logical Variable - DSLLV; MIPS64 */
         DIP("dsllv r%u, r%u, r%u", regRd, regRt, regRs);
         IRTemp tmpRs8 = newTemp(Ity_I8);

         assign(tmp, binop(Iop_And64, mkexpr(tmpRs), mkSzImm(ty, 63)));
         assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp)));
         assign(tmpRd, binop(Iop_Shl64, mkexpr(tmpRt), mkexpr(tmpRs8)));
         putIReg(regRd, mkexpr(tmpRd));
         break;
      }

      case 0x3B:  /* Doubleword Shift Right Arithmetic - DSRA; MIPS64 */
         DIP("dsra r%u, r%u, %d", regRd, regRt, (Int)sImmsa);
         assign(tmpRd, binop(Iop_Sar64, mkexpr(tmpRt), mkU8(uImmsa)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      case 0x3F:  /* Doubleword Shift Right Arithmetic Plus 32 - DSRA32;
                     MIPS64 */
         DIP("dsra32 r%u, r%u, %d", regRd, regRt, (Int)sImmsa);
         assign(tmpRd, binop(Iop_Sar64, mkexpr(tmpRt), mkU8(uImmsa + 32)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      case 0x17: {  /* Doubleword Shift Right Arithmetic Variable - DSRAV;
                       MIPS64 */
         DIP("dsrav r%u, r%u, r%u", regRd, regRt, regRs);
         IRTemp tmpRs8 = newTemp(Ity_I8);
         assign(tmp, binop(Iop_And64, mkexpr(tmpRs), mkSzImm(ty, 63)));
         assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp)));
         assign(tmpRd, binop(Iop_Sar64, mkexpr(tmpRt), mkexpr(tmpRs8)));
         putIReg(regRd, mkexpr(tmpRd));
         break;

      }

      default:
         return False;

   }
   return True;
}

static IROp mkSzOp ( IRType ty, IROp op8 )
{
   Int adj;
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I64);
   vassert(op8 == Iop_Add8 || op8 == Iop_Sub8 || op8 == Iop_Mul8
           || op8 == Iop_Or8 || op8 == Iop_And8 || op8 == Iop_Xor8
           || op8 == Iop_Shl8 || op8 == Iop_Shr8 || op8 == Iop_Sar8
           || op8 == Iop_CmpEQ8 || op8 == Iop_CmpNE8 || op8 == Iop_Not8);
   adj = ty == Ity_I8 ? 0 : (ty == Ity_I16 ? 1 : (ty == Ity_I32 ? 2 : 3));
   return adj + op8;
}

/*********************************************************/
/*---             Floating Point Compare              ---*/
/*********************************************************/
/* Function that returns a string that represent mips cond
   mnemonic for the input code. */
static const HChar* showCondCode(UInt code) {
   const HChar* ret;
   switch (code) {
      case 0: ret = "f"; break;
      case 1: ret = "un"; break;
      case 2: ret = "eq"; break;
      case 3: ret = "ueq"; break;
      case 4: ret = "olt"; break;
      case 5: ret = "ult"; break;
      case 6: ret = "ole"; break;
      case 7: ret = "ule"; break;
      case 8: ret = "sf"; break;
      case 9: ret = "ngle"; break;
      case 10: ret = "seq"; break;
      case 11: ret = "ngl"; break;
      case 12: ret = "lt"; break;
      case 13: ret = "nge"; break;
      case 14: ret = "le"; break;
      case 15: ret = "ngt"; break;
      default: vpanic("showCondCode"); break;
   }
   return ret;
}

static Bool dis_instr_CCondFmt ( UInt cins )
{
   IRTemp t0, t1, t2, t3, tmp5, tmp6;
   IRTemp ccIR = newTemp(Ity_I32);
   IRTemp ccMIPS = newTemp(Ity_I32);
   UInt FC = get_FC(cins);
   UInt fmt = get_fmt(cins);
   UInt fs = get_fs(cins);
   UInt ft = get_ft(cins);
   UInt cond = get_cond(cins);

   if (FC == 0x3) {  /* C.cond.fmt */
      UInt fpc_cc = get_fpc_cc(cins);
      switch (fmt) {
         case 0x10: {  /* C.cond.S */
            DIP("c.%s.s %d, f%d, f%d", showCondCode(cond), fpc_cc, fs, ft);
            if (fp_mode64) {
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);

               tmp5 = newTemp(Ity_F64);
               tmp6 = newTemp(Ity_F64);

               assign(tmp5, unop(Iop_F32toF64, getLoFromF64(Ity_F64,
                                 getFReg(fs))));
               assign(tmp6, unop(Iop_F32toF64, getLoFromF64(Ity_F64,
                                 getFReg(ft))));

               assign(ccIR, binop(Iop_CmpF64, mkexpr(tmp5), mkexpr(tmp6)));
               putHI(mkWidenFrom32(mode64 ? Ity_I64: Ity_I32,
                                   mkexpr(ccIR), True));
               /* Map compare result from IR to MIPS
                  FP cmp result | MIPS | IR
                  --------------------------
                  UN            | 0x1 | 0x45
                  EQ            | 0x2 | 0x40
                  GT            | 0x4 | 0x00
                  LT            | 0x8 | 0x01
                */

               /* ccMIPS = Shl(1, (~(ccIR>>5) & 2) | ((ccIR ^ (ccIR>>6)) & 1) */
               assign(ccMIPS, binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,
                              binop(Iop_Or32, binop(Iop_And32, unop(Iop_Not32,
                              binop(Iop_Shr32, mkexpr(ccIR),mkU8(5))),mkU32(2)),
                              binop(Iop_And32, binop(Iop_Xor32, mkexpr(ccIR),
                              binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))),
                              mkU32(1))))));
               putLO(mkWidenFrom32(mode64 ? Ity_I64: Ity_I32,
                                   mkexpr(ccMIPS), True));

               /* UN */
               assign(t0, binop(Iop_And32, mkexpr(ccMIPS), mkU32(0x1)));
               /* EQ */
               assign(t1, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                                                 mkU8(0x1)), mkU32(0x1)));
               /* NGT */
               assign(t2, binop(Iop_And32, unop(Iop_Not32, binop(Iop_Shr32,
                                 mkexpr(ccMIPS), mkU8(0x2))),mkU32(0x1)));
               /* LT */
               assign(t3, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                                                 mkU8(0x3)), mkU32(0x1)));
               switch (cond) {
                  case 0x0:
                     setFPUCondCode(mkU32(0), fpc_cc);
                     break;
                  case 0x1:
                     setFPUCondCode(mkexpr(t0), fpc_cc);
                     break;
                  case 0x2:
                     setFPUCondCode(mkexpr(t1), fpc_cc);
                     break;
                  case 0x3:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                          fpc_cc);
                     break;
                  case 0x4:
                     setFPUCondCode(mkexpr(t3), fpc_cc);
                     break;
                  case 0x5:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                          fpc_cc);
                     break;
                  case 0x6:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                          fpc_cc);
                     break;
                  case 0x7:
                     setFPUCondCode(mkexpr(t2), fpc_cc);
                     break;
                  case 0x8:
                     setFPUCondCode(mkU32(0), fpc_cc);
                     break;
                  case 0x9:
                     setFPUCondCode(mkexpr(t0), fpc_cc);
                     break;
                  case 0xA:
                     setFPUCondCode(mkexpr(t1), fpc_cc);
                     break;
                  case 0xB:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                          fpc_cc);
                     break;
                  case 0xC:
                     setFPUCondCode(mkexpr(t3), fpc_cc);
                     break;
                  case 0xD:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                          fpc_cc);
                     break;
                  case 0xE:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                          fpc_cc);
                     break;
                  case 0xF:
                     setFPUCondCode(mkexpr(t2), fpc_cc);
                     break;

                  default:
                     return False;
               }

            } else {
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);

               assign(ccIR, binop(Iop_CmpF64, unop(Iop_F32toF64, getFReg(fs)),
                                  unop(Iop_F32toF64, getFReg(ft))));
               /* Map compare result from IR to MIPS
                  FP cmp result | MIPS | IR
                  --------------------------
                  UN            | 0x1 | 0x45
                  EQ            | 0x2 | 0x40
                  GT            | 0x4 | 0x00
                  LT            | 0x8 | 0x01
                */

               /* ccMIPS = Shl(1, (~(ccIR>>5) & 2) | ((ccIR ^ (ccIR>>6)) & 1) */
               assign(ccMIPS, binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,
                              binop(Iop_Or32, binop(Iop_And32, unop(Iop_Not32,
                              binop(Iop_Shr32, mkexpr(ccIR), mkU8(5))),
                                    mkU32(2)), binop(Iop_And32,
                              binop(Iop_Xor32, mkexpr(ccIR),
                              binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))),
                              mkU32(1))))));
               /* UN */
               assign(t0, binop(Iop_And32, mkexpr(ccMIPS), mkU32(0x1)));
               /* EQ */
               assign(t1, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                      mkU8(0x1)), mkU32(0x1)));
               /* NGT */
               assign(t2, binop(Iop_And32, unop(Iop_Not32, binop(Iop_Shr32,
                      mkexpr(ccMIPS), mkU8(0x2))), mkU32(0x1)));
               /* LT */
               assign(t3, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                      mkU8(0x3)), mkU32(0x1)));

               switch (cond) {
                  case 0x0:
                     setFPUCondCode(mkU32(0), fpc_cc);
                     break;
                  case 0x1:
                     setFPUCondCode(mkexpr(t0), fpc_cc);
                     break;
                  case 0x2:
                     setFPUCondCode(mkexpr(t1), fpc_cc);
                     break;
                  case 0x3:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                          fpc_cc);
                     break;
                  case 0x4:
                     setFPUCondCode(mkexpr(t3), fpc_cc);
                     break;
                  case 0x5:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                          fpc_cc);
                     break;
                  case 0x6:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                          fpc_cc);
                     break;
                  case 0x7:
                     setFPUCondCode(mkexpr(t2), fpc_cc);
                     break;
                  case 0x8:
                     setFPUCondCode(mkU32(0), fpc_cc);
                     break;
                  case 0x9:
                     setFPUCondCode(mkexpr(t0), fpc_cc);
                     break;
                  case 0xA:
                     setFPUCondCode(mkexpr(t1), fpc_cc);
                     break;
                  case 0xB:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                          fpc_cc);
                     break;
                  case 0xC:
                     setFPUCondCode(mkexpr(t3), fpc_cc);
                     break;
                  case 0xD:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                          fpc_cc);
                     break;
                  case 0xE:
                     setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                          fpc_cc);
                     break;
                  case 0xF:
                     setFPUCondCode(mkexpr(t2), fpc_cc);
                     break;

                  default:
                     return False;
               }
            }
         }
            break;

         case 0x11: {  /* C.cond.D */
            DIP("c.%s.d %d, f%d, f%d", showCondCode(cond), fpc_cc, fs, ft);
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I32);
            assign(ccIR, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
            /* Map compare result from IR to MIPS
               FP cmp result | MIPS | IR
               --------------------------
               UN            | 0x1 | 0x45
               EQ            | 0x2 | 0x40
               GT            | 0x4 | 0x00
               LT            | 0x8 | 0x01
             */

            /* ccMIPS = Shl(1, (~(ccIR>>5) & 2) | ((ccIR ^ (ccIR>>6)) & 1) */
            assign(ccMIPS, binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,
                           binop(Iop_Or32, binop(Iop_And32, unop(Iop_Not32,
                           binop(Iop_Shr32, mkexpr(ccIR), mkU8(5))), mkU32(2)),
                           binop(Iop_And32, binop(Iop_Xor32, mkexpr(ccIR),
                           binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))),
                           mkU32(1))))));

            /* UN */
            assign(t0, binop(Iop_And32, mkexpr(ccMIPS), mkU32(0x1)));
            /* EQ */
            assign(t1, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                   mkU8(0x1)), mkU32(0x1)));
            /* NGT */
            assign(t2, binop(Iop_And32, unop(Iop_Not32, binop(Iop_Shr32,
                   mkexpr(ccMIPS), mkU8(0x2))), mkU32(0x1)));
            /* LT */
            assign(t3, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                   mkU8(0x3)), mkU32(0x1)));

            switch (cond) {
               case 0x0:
                  setFPUCondCode(mkU32(0), fpc_cc);
                  break;
               case 0x1:
                  setFPUCondCode(mkexpr(t0), fpc_cc);
                  break;
               case 0x2:
                  setFPUCondCode(mkexpr(t1), fpc_cc);
                  break;
               case 0x3:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                       fpc_cc);
                  break;
               case 0x4:
                  setFPUCondCode(mkexpr(t3), fpc_cc);
                  break;
               case 0x5:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                       fpc_cc);
                  break;
               case 0x6:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                       fpc_cc);
                  break;
               case 0x7:
                  setFPUCondCode(mkexpr(t2), fpc_cc);
                  break;
               case 0x8:
                  setFPUCondCode(mkU32(0), fpc_cc);
                  break;
               case 0x9:
                  setFPUCondCode(mkexpr(t0), fpc_cc);
                  break;
               case 0xA:
                  setFPUCondCode(mkexpr(t1), fpc_cc);
                  break;
               case 0xB:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t1)),
                                       fpc_cc);
                  break;
               case 0xC:
                  setFPUCondCode(mkexpr(t3), fpc_cc);
                  break;
               case 0xD:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t0), mkexpr(t3)),
                                       fpc_cc);
                  break;
               case 0xE:
                  setFPUCondCode(binop(Iop_Or32, mkexpr(t3), mkexpr(t1)),
                                       fpc_cc);
                  break;
               case 0xF:
                  setFPUCondCode(mkexpr(t2), fpc_cc);
                  break;
               default:
                  return False;
            }
         }
         break;

         default:
            return False;
      }
   } else {
      return False;
   }

   return True;
}

/*********************************************************/
/*---        Branch Instructions for mips64           ---*/
/*********************************************************/
static Bool dis_instr_branch ( UInt theInstr, DisResult * dres,
                               Bool(*resteerOkFn) (void *, Addr64),
                               void *callback_opaque, IRStmt ** set )
{
   UInt jmpKind = 0;
   UChar opc1 = get_opcode(theInstr);
   UChar regRs = get_rs(theInstr);
   UChar regRt = get_rt(theInstr);
   UInt offset = get_imm(theInstr);
   Long sOffset = extend_s_16to64(offset);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IROp opSlt = mode64 ? Iop_CmpLT64S : Iop_CmpLT32S;

   IRTemp tmp = newTemp(ty);
   IRTemp tmpRs = newTemp(ty);
   IRTemp tmpRt = newTemp(ty);
   IRTemp tmpLt = newTemp(ty);
   IRTemp tmpReg0 = newTemp(ty);

   UChar regLnk = 31;   /* reg 31 is link reg in MIPS */
   Addr64 addrTgt = 0;
   Addr64 cia = guest_PC_curr_instr;

   IRExpr *eConst0 = mkSzImm(ty, (UInt) 0);
   IRExpr *eNia = mkSzImm(ty, cia + 8);
   IRExpr *eCond = NULL;

   assign(tmpRs, getIReg(regRs));
   assign(tmpRt, getIReg(regRt));
   assign(tmpReg0, getIReg(0));

   eCond = binop(mkSzOp(ty, Iop_CmpNE8), mkexpr(tmpReg0), mkexpr(tmpReg0));

   switch (opc1) {
      case 0x01:
         switch (regRt) {
            case 0x00: {  /* BLTZ rs, offset */
               addrTgt = mkSzAddr(ty, cia + 4 + (sOffset << 2));
               IRTemp tmpLtRes = newTemp(Ity_I1);

               assign(tmp, eConst0);
               assign(tmpLtRes, binop(opSlt, mkexpr(tmpRs), mkexpr(tmp)));
               assign(tmpLt, mode64 ? unop(Iop_1Uto64, mkexpr(tmpLtRes)) :
                      unop(Iop_1Uto32, mkexpr(tmpLtRes)));

               eCond = binop(mkSzOp(ty, Iop_CmpNE8), mkexpr(tmpLt),
                             mkexpr(tmpReg0));

               jmpKind = Ijk_Call;
               break;
            }

            case 0x01: {  /* BGEZ rs, offset */
               IRTemp tmpLtRes = newTemp(Ity_I1);
               addrTgt = mkSzAddr(ty, cia + 4 + (sOffset << 2));

               assign(tmp, eConst0);
               assign(tmpLtRes, binop(opSlt, mkexpr(tmpRs), mkexpr(tmp)));
               assign(tmpLt, mode64 ? unop(Iop_1Uto64, mkexpr(tmpLtRes)) :
                                      unop(Iop_1Uto32, mkexpr(tmpLtRes)));
               eCond = binop(mkSzOp(ty, Iop_CmpEQ8), mkexpr(tmpLt),
                                    mkexpr(tmpReg0));

               jmpKind = Ijk_Call;
               break;
            }

            case 0x11: {  /* BGEZAL rs, offset */
               addrTgt = mkSzAddr(ty, cia + 4 + (sOffset << 2));
               putIReg(regLnk, eNia);
               IRTemp tmpLtRes = newTemp(Ity_I1);

               assign(tmpLtRes, binop(opSlt, mkexpr(tmpRs), eConst0));
               assign(tmpLt, mode64 ? unop(Iop_1Uto64, mkexpr(tmpLtRes)) :
                                      unop(Iop_1Uto32, mkexpr(tmpLtRes)));

               eCond = binop(mkSzOp(ty, Iop_CmpEQ8), mkexpr(tmpLt),
                                    mkexpr(tmpReg0));

               jmpKind = Ijk_Call;
               break;
            }

            case 0x10: {  /* BLTZAL rs, offset */
               IRTemp tmpLtRes = newTemp(Ity_I1);
               IRTemp tmpRes = newTemp(ty);

               addrTgt = mkSzAddr(ty, cia + 4 + (sOffset << 2));
               putIReg(regLnk, eNia);

               assign(tmp, eConst0);
               assign(tmpLtRes, binop(opSlt, mkexpr(tmpRs), mkexpr(tmp)));
               assign(tmpRes, mode64 ? unop(Iop_1Uto64,
                      mkexpr(tmpLtRes)) : unop(Iop_1Uto32, mkexpr(tmpLtRes)));
               eCond = binop(mkSzOp(ty, Iop_CmpNE8), mkexpr(tmpRes),
                                                     mkexpr(tmpReg0));

               jmpKind = Ijk_Call;
               break;
            }

         }
         break;
      default:
         return False;
      }
   *set = IRStmt_Exit(eCond, jmpKind, mkSzConst(ty, addrTgt), OFFB_PC);
   return True;
}

/*********************************************************/
/*---         Cavium Specific Instructions            ---*/
/*********************************************************/
static Bool dis_instr_CVM ( UInt theInstr )
{
   UChar  opc2     = get_function(theInstr);
   UChar  opc1     = get_opcode(theInstr);
   UChar  regRs    = get_rs(theInstr);
   UChar  regRt    = get_rt(theInstr);
   UChar  regRd    = get_rd(theInstr);
   UInt   imm 	   = get_imm(theInstr);
   UChar  lenM1    = get_msb(theInstr);
   UChar  p        = get_lsb(theInstr);
   IRType ty       = mode64? Ity_I64 : Ity_I32;
   IRTemp tmp      = newTemp(ty);
   IRTemp tmpRs    = newTemp(ty);
   IRTemp tmpRt    = newTemp(ty);
   IRTemp t1       = newTemp(ty);
   UInt size;
   assign(tmpRs, getIReg(regRs));

   switch(opc1){
      case 0x1C:  {
         switch(opc2) { 
            case 0x03: {  /* DMUL rd, rs, rt */
               DIP("dmul r%d, r%d, r%d", regRd, regRs, regRt);
               IRType t0 = newTemp(Ity_I128);
               assign(t0, binop(Iop_MullU64, getIReg(regRs), getIReg(regRt)));
               putIReg(regRd, unop(Iop_128to64, mkexpr(t0)));
               break;
            }

            case 0x32:  /* 5. CINS rd, rs, p, lenm1 */
               DIP("cins r%u, r%u, %d, %d\n", regRt, regRs, p, lenM1); 
               assign ( tmp  , binop(Iop_Shl64, mkexpr(tmpRs),
                                     mkU8(64-( lenM1+1 ))));
               assign ( tmpRt, binop(Iop_Shr64, mkexpr( tmp ),
                                     mkU8(64-(p+lenM1+1))));
               putIReg( regRt, mkexpr(tmpRt));
               break;

            case 0x33:  /* 6. CINS32 rd, rs, p+32, lenm1 */
               DIP("cins32 r%u, r%u, %d, %d\n", regRt, regRs, p+32, lenM1);
               assign ( tmp  , binop(Iop_Shl64, mkexpr(tmpRs),
                                     mkU8(64-( lenM1+1 ))));
               assign ( tmpRt, binop(Iop_Shr64, mkexpr( tmp ),
                                     mkU8(32-(p+lenM1+1))));
               putIReg( regRt, mkexpr(tmpRt));
               break;

            case 0x3A:  /* 3. EXTS rt, rs, p len */
               DIP("exts r%u, r%u, %d, %d\n", regRt, regRs, p, lenM1); 
               size = lenM1 + 1;  /* lenm1+1 */
               UChar lsAmt = 64 - (p + size);  /* p+lenm1+1 */
               UChar rsAmt = 64 - size;  /* lenm1+1 */
               tmp = newTemp(Ity_I64);
               assign(tmp, binop(Iop_Shl64, mkexpr(tmpRs), mkU8(lsAmt)));
               putIReg(regRt, binop(Iop_Sar64, mkexpr(tmp), mkU8(rsAmt)));
               break;

            case 0x3B:  /* 4. EXTS32 rt, rs, p len */
               DIP("exts32 r%u, r%u, %d, %d\n", regRt, regRs, p, lenM1); 
               assign ( tmp  , binop(Iop_Shl64, mkexpr(tmpRs),
                                     mkU8(32-(p+lenM1+1))));
               assign ( tmpRt, binop(Iop_Sar64, mkexpr(tmp),
                                     mkU8(64-(lenM1+1))) );
               putIReg( regRt, mkexpr(tmpRt));
               break;

            case 0x2B:  /* 20. SNE rd, rs, rt */
               DIP("sne r%d, r%d, r%d", regRd,regRs, regRt);
               if (mode64)
                  putIReg(regRd, unop(Iop_1Uto64, binop(Iop_CmpNE64,
                                                        getIReg(regRs),
                                                        getIReg(regRt))));
               else
                  putIReg(regRd,unop(Iop_1Uto32, binop(Iop_CmpNE32,
                                                       getIReg(regRs),
                                                       getIReg(regRt))));
               break;

            case 0x2A:  /* Set Equals - SEQ; Cavium OCTEON */
               DIP("seq r%d, r%d, %d", regRd, regRs, regRt);
               if (mode64)
                  putIReg(regRd, unop(Iop_1Uto64,
                                      binop(Iop_CmpEQ64, getIReg(regRs),
                                            getIReg(regRt))));
               else
                  putIReg(regRd, unop(Iop_1Uto32,
                                      binop(Iop_CmpEQ32, getIReg(regRs),
                                            getIReg(regRt))));
               break;

            case 0x2E:  /* Set Equals Immediate - SEQI; Cavium OCTEON */
               DIP("seqi r%d, r%d, %d", regRt, regRs, imm);
               if (mode64)
                  putIReg(regRt, unop(Iop_1Uto64,
                                      binop(Iop_CmpEQ64, getIReg(regRs),
                                            mkU64(extend_s_10to64(imm)))));
               else
                  putIReg(regRt, unop(Iop_1Uto32,
                                      binop(Iop_CmpEQ32, getIReg(regRs),
                                            mkU32(extend_s_10to32(imm)))));
               break;

            case 0x2F:  /* Set Not Equals Immediate - SNEI; Cavium OCTEON */
               DIP("snei r%d, r%d, %d", regRt, regRs, imm);
               if (mode64)
                  putIReg(regRt, unop(Iop_1Uto64,
                                   binop(Iop_CmpNE64,
                                         getIReg(regRs),
                                         mkU64(extend_s_10to64(imm)))));
               else
                  putIReg(regRt, unop(Iop_1Uto32,
                                   binop(Iop_CmpNE32,
                                         getIReg(regRs),
                                         mkU32(extend_s_10to32(imm)))));
               break;

            default:
               return False;
         }
         break;
      } /* opc1 0x1C ends here*/
      case 0x1F:{
         switch(opc2) {
            case 0x0A: {  // lx - Load indexed instructions
               switch (get_sa(theInstr)) {
                  case 0x00: {  // LWX rd, index(base)
                     DIP("lwx r%d, r%d(r%d)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN;
                     putIReg(regRd, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)),
                                                  True));
                     break;
                  }
                  case 0x08: {  // LDX rd, index(base)
                     DIP("ldx r%d, r%d(r%d)", regRd, regRt, regRs);
                     vassert(mode64); /* Currently Implemented only for n64 */
                     LOADX_STORE_PATTERN;
                     putIReg(regRd, load(Ity_I64, mkexpr(t1)));
                     break;
                  }
                  case 0x06: {  // LBUX rd, index(base)
                     DIP("lbux r%d, r%d(r%d)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN;
                     if (mode64)
                        putIReg(regRd, unop(Iop_8Uto64, load(Ity_I8,
                                                             mkexpr(t1))));
                     else
                        putIReg(regRd, unop(Iop_8Uto32, load(Ity_I8,
                                                             mkexpr(t1))));
                     break;
                  }
                  case 0x10: {  // LWUX rd, index(base) (Cavium OCTEON)
                     DIP("lwux r%d, r%d(r%d)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN; /* same for both 32 and 64 modes*/
                     putIReg(regRd, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)),
                                                  False));
                     break;
                  }
                  case 0x14: {  // LHUX rd, index(base) (Cavium OCTEON)
                     DIP("lhux r%d, r%d(r%d)", regRd, regRt, regRs);
                     LOADX_STORE_PATTERN;
                     if (mode64)
                        putIReg(regRd,
                                unop(Iop_16Uto64, load(Ity_I16, mkexpr(t1))));
                     else
                        putIReg(regRd,
                                unop(Iop_16Uto32, load(Ity_I16, mkexpr(t1))));
                     break;
                  }
                  case 0x16: {  // LBX rd, index(base) (Cavium OCTEON)
                     DIP("lbx r%d, r%d(r%d)", regRd, regRs, regRt);
                     LOADX_STORE_PATTERN;
                     if (mode64)
                        putIReg(regRd,
                                unop(Iop_8Sto64, load(Ity_I8, mkexpr(t1))));
                     else
                        putIReg(regRd,
                                unop(Iop_8Sto32, load(Ity_I8, mkexpr(t1))));
                     break;
                  }
                  default:
                     vex_printf("\nUnhandled LX instruction opc3 = %x\n",
                                get_sa(theInstr));
                     return False;
               }
               break;
            }
         } /* opc1 = 0x1F & opc2 = 0xA (LX) ends here*/
         break;
      } /* opc1 = 0x1F ends here*/
      default:
         return False; 
   } /* main opc1 switch ends here */
   return True;
}

/*------------------------------------------------------------*/
/*---       Disassemble a single DSP ASE instruction       ---*/
/*------------------------------------------------------------*/

static UInt disDSPInstr_MIPS_WRK ( UInt cins )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14,
          t15, t16, t17;
   UInt opcode, rs, rt, rd, sa, function, ac, ac_mfhilo, rddsp_mask,
        wrdsp_mask, dsp_imm, shift;

   opcode = get_opcode(cins);
   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);
   function = get_function(cins);
   ac = get_acNo(cins);
   ac_mfhilo = get_acNo_mfhilo(cins);
   rddsp_mask = get_rddspMask(cins);
   wrdsp_mask = get_wrdspMask(cins);
   dsp_imm = get_dspImm(cins);
   shift = get_shift(cins);

   switch (opcode) {
      case 0x00: {  /* Special */
         switch (function) {
            case 0x10: {  /* MFHI */
               DIP("mfhi ac%d r%d", ac_mfhilo, rd);
               putIReg(rd, unop(Iop_64HIto32, getAcc(ac_mfhilo)));
               break;
            }

            case 0x11: {  /* MTHI */
               DIP("mthi ac%d r%d", ac, rs);
               t1 = newTemp(Ity_I32);
               assign(t1, unop(Iop_64to32, getAcc(ac)));
               putAcc(ac, binop(Iop_32HLto64, getIReg(rs), mkexpr(t1)));
               break;
            }

            case 0x12: {  /* MFLO */
               DIP("mflo ac%d r%d", ac_mfhilo, rd);
               putIReg(rd, unop(Iop_64to32, getAcc(ac_mfhilo)));
               break;
            }

            case 0x13: {  /* MTLO */
               DIP("mtlo ac%d r%d", ac, rs);
               t1 = newTemp(Ity_I32);
               assign(t1, unop(Iop_64HIto32, getAcc(ac)));
               putAcc(ac, binop(Iop_32HLto64, mkexpr(t1), getIReg(rs)));
               break;
            }

            case 0x18: {  /* MULT */
               DIP("mult ac%d r%d, r%d", ac, rs, rt);
               t1 = newTemp(Ity_I64);
               assign(t1, binop(Iop_MullS32, mkNarrowTo32(Ity_I32, getIReg(rs)),
                                mkNarrowTo32(Ity_I32, getIReg(rt))));
               putAcc(ac, mkexpr(t1));
               break;
            }

            case 0x19: {  /* MULTU */
               DIP("multu ac%d r%d, r%d", ac, rs, rt);
               t1 = newTemp(Ity_I64);
               assign(t1, binop(Iop_MullU32, mkNarrowTo32(Ity_I32, getIReg(rs)),
                                             mkNarrowTo32(Ity_I32,
                                                          getIReg(rt))));
               putAcc(ac, mkexpr(t1));
            break;
            }
         }
         break;
      }
      case 0x1C: {  /* Special2 */
         switch (function) {
            case 0x00: {  /* MADD */
               DIP("madd ac%d, r%d, r%d", ac, rs, rt);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_I64);

               assign(t1, getAcc(ac));
               assign(t2, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));
               assign(t3, binop(Iop_Add64, mkexpr(t1), mkexpr(t2)));

               putAcc(ac, mkexpr(t3));
               break;
            }
            case 0x01: {  /* MADDU */
               DIP("maddu ac%d r%d, r%d", ac, rs, rt);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_I64);

               assign(t1, getAcc(ac));
               assign(t2, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));
               assign(t3, binop(Iop_Add64, mkexpr(t2), mkexpr(t1)));

               putAcc(ac, mkexpr(t3));
               break;
            }
            case 0x04: {  /* MSUB */
               DIP("msub ac%d r%d, r%d", ac, rs, rt);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_I64);

               assign(t1, getAcc(ac));
               assign(t2, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));
               assign(t3, binop(Iop_Sub64, mkexpr(t1), mkexpr(t2)));

               putAcc(ac, mkexpr(t3));
               break;
            }
            case 0x05: {  /* MSUBU */
               DIP("msubu ac%d r%d, r%d", ac, rs, rt);
               t1 = newTemp(Ity_I64);
               t2 = newTemp(Ity_I64);
               t3 = newTemp(Ity_I64);

               assign(t1, getAcc(ac));
               assign(t2, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));
               assign(t3, binop(Iop_Sub64, mkexpr(t1), mkexpr(t2)));

               putAcc(ac, mkexpr(t3));
               break;
            }
         }
         break;
      }
      case 0x1F: {  /* Special3 */
         switch (function) {
            case 0x12: {  /* ABSQ_S.PH */
               switch (sa) {
                  case 0x1: {  /* ABSQ_S.QB */
                     DIP("absq_s.qb r%d, r%d", rd, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I8);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I8);
                     t4 = newTemp(Ity_I8);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I8);
                     t8 = newTemp(Ity_I8);
                     t9 = newTemp(Ity_I1);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I8);
                     t12 = newTemp(Ity_I8);
                     t13 = newTemp(Ity_I1);
                     t14 = newTemp(Ity_I1);
                     t15 = newTemp(Ity_I8);
                     t16 = newTemp(Ity_I32);
                     t17 = newTemp(Ity_I32);

                     /* Absolute value of the rightmost byte (bits 7-0). */
                     /* t0 - rightmost byte. */
                     assign(t0, unop(Iop_16to8, unop(Iop_32to16, getIReg(rt))));
                     /* t1 holds 1 if t0 is equal to 0x80, or 0 otherwise. */
                     assign(t1, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32, mkexpr(t0)),
                                      mkU32(0x00000080)));
                     /* t2 holds 1 if value in t0 is negative, 0 otherwise. */
                     assign(t2, unop(Iop_32to1,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x00000080)),
                                           mkU8(0x7))));
                     /* t3 holds abs(t0). */
                     assign(t3, IRExpr_ITE(mkexpr(t1),
                                           mkU8(0x7F),
                                           IRExpr_ITE(mkexpr(t2),
                                                      binop(Iop_Add8,
                                                            unop(Iop_Not8,
                                                                 mkexpr(t0)),
                                                            mkU8(0x1)),
                                                      mkexpr(t0))));

                     /* Absolute value of bits 15-8. */
                     /* t4 - input byte. */
                     assign(t4,
                            unop(Iop_16HIto8, unop(Iop_32to16, getIReg(rt))));
                     /* t5 holds 1 if t4 is equal to 0x80, or 0 otherwise. */
                     assign(t5, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32, mkexpr(t4)),
                                      mkU32(0x00000080)));
                     /* t6 holds 1 if value in t4 is negative, 0 otherwise. */
                     assign(t6, unop(Iop_32to1,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x00008000)),
                                           mkU8(15))));
                     /* t3 holds abs(t4). */
                     assign(t7, IRExpr_ITE(mkexpr(t5),
                                           mkU8(0x7F),
                                           IRExpr_ITE(mkexpr(t6),
                                                      binop(Iop_Add8,
                                                            unop(Iop_Not8,
                                                                 mkexpr(t4)),
                                                            mkU8(0x1)),
                                                      mkexpr(t4))));

                     /* Absolute value of bits 23-15. */
                     /* t8 - input byte. */
                     assign(t8,
                            unop(Iop_16to8, unop(Iop_32HIto16, getIReg(rt))));
                     /* t9 holds 1 if t8 is equal to 0x80, or 0 otherwise. */
                     assign(t9, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32, mkexpr(t8)),
                                      mkU32(0x00000080)));
                     /* t6 holds 1 if value in t8 is negative, 0 otherwise. */
                     assign(t10, unop(Iop_32to1,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  getIReg(rt),
                                                  mkU32(0x00800000)),
                                            mkU8(23))));
                     /* t3 holds abs(t8). */
                     assign(t11, IRExpr_ITE(mkexpr(t9),
                                            mkU8(0x7F),
                                            IRExpr_ITE(mkexpr(t10),
                                                       binop(Iop_Add8,
                                                             unop(Iop_Not8,
                                                                  mkexpr(t8)),
                                                             mkU8(0x1)),
                                                       mkexpr(t8))));

                     /* Absolute value of bits 31-24. */
                     /* t12 - input byte. */
                     assign(t12,
                            unop(Iop_16HIto8, unop(Iop_32HIto16, getIReg(rt))));
                     /* t13 holds 1 if t12 is equal to 0x80, or 0 otherwise. */
                     assign(t13, binop(Iop_CmpEQ32,
                                       unop(Iop_8Uto32, mkexpr(t12)),
                                       mkU32(0x00000080)));
                     /* t14 holds 1 if value in t12 is negative, 0 otherwise. */
                     assign(t14, unop(Iop_32to1,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  getIReg(rt),
                                                  mkU32(0x80000000)),
                                            mkU8(31))));
                     /* t15 holds abs(t12). */
                     assign(t15, IRExpr_ITE(mkexpr(t13),
                                            mkU8(0x7F),
                                            IRExpr_ITE(mkexpr(t14),
                                                       binop(Iop_Add8,
                                                             unop(Iop_Not8,
                                                                  mkexpr(t12)),
                                                             mkU8(0x1)),
                                                       mkexpr(t12))));

                     /* t16 holds !0 if any of input bytes is 0x80 or 0
                        otherwise. */
                     assign(t16,
                            binop(Iop_Or32,
                                  binop(Iop_Or32,
                                        binop(Iop_Or32,
                                              unop(Iop_1Sto32, mkexpr(t13)),
                                              unop(Iop_1Sto32, mkexpr(t9))),
                                        unop(Iop_1Sto32, mkexpr(t5))),
                                  unop(Iop_1Sto32, mkexpr(t1))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    mkexpr(t16),
                                                    mkU32(0x0)),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000))));

                     /* t17 = t15|t11|t7|t3 */
                     assign(t17,
                            binop(Iop_16HLto32,
                                  binop(Iop_8HLto16, mkexpr(t15), mkexpr(t11)),
                                  binop(Iop_8HLto16, mkexpr(t7), mkexpr(t3))));

                     putIReg(rd, mkexpr(t17));
                     break;
                  }
                  case 0x2: {  /* REPL.QB */
                     DIP("repl.qb r%d, %d", rd, dsp_imm);
                     vassert(!mode64);

                     putIReg(rd, mkU32((dsp_imm << 24) | (dsp_imm << 16) |
                                       (dsp_imm << 8) | (dsp_imm)));
                     break;
                  }
                  case 0x3: {  /* REPLV.QB */
                     DIP("replv.qb r%d, r%d", rd, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I8);

                     assign(t0, unop(Iop_32to8,
                                binop(Iop_And32, getIReg(rt), mkU32(0xff))));
                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   binop(Iop_8HLto16, mkexpr(t0), mkexpr(t0)),
                                   binop(Iop_8HLto16, mkexpr(t0), mkexpr(t0))));
                     break;
                  }
                  case 0x4: {  /* PRECEQU.PH.QBL */
                     DIP("precequ.ph.qbl r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0xff000000)),
                                             mkU8(1)),
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x00ff0000)),
                                             mkU8(9))));
                     break;
                  }
                  case 0x5: {  /* PRECEQU.PH.QBR */
                     DIP("precequ.ph.qbr r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Shl32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x0000ff00)),
                                             mkU8(15)),
                                       binop(Iop_Shl32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x000000ff)),
                                             mkU8(7))));
                     break;
                  }
                  case 0x6: {  /* PRECEQU.PH.QBLA */
                     DIP("precequ.ph.qbla r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0xff000000)),
                                             mkU8(1)),
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x0000ff00)),
                                             mkU8(1))));
                     break;
                  }
                  case 0x7: {  /* PRECEQU.PH.QBRA */
                     DIP("precequ.ph.qbra r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Shl32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x00ff0000)),
                                             mkU8(7)),
                                       binop(Iop_Shl32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x000000ff)),
                                             mkU8(7))));
                     break;
                  }
                  case 0x9: {  /* ABSQ_S.PH */
                     DIP("absq_s.ph r%d, r%d", rd, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I16);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I16);
                     t4 = newTemp(Ity_I16);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I16);
                     t8 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);

                     /* t0 holds lower 16 bits of value in rt. */
                     assign(t0, unop(Iop_32to16, getIReg(rt)));
                     /* t1 holds 1 if t0 is equal to 0x8000. */
                     assign(t1, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32, mkexpr(t0)),
                                      mkU32(0x00008000)));
                     /* t2 holds 1 if value in t0 is negative, 0 otherwise. */
                     assign(t2, unop(Iop_32to1,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x00008000)),
                                           mkU8(15))));
                     /* t3 holds abs(t0). */
                     assign(t3, IRExpr_ITE(mkexpr(t1),
                                           mkU16(0x7FFF),
                                           IRExpr_ITE(mkexpr(t2),
                                                      binop(Iop_Add16,
                                                            unop(Iop_Not16,
                                                                 mkexpr(t0)),
                                                            mkU16(0x1)),
                                                      mkexpr(t0))));

                     /* t4 holds lower 16 bits of value in rt. */
                     assign(t4, unop(Iop_32HIto16, getIReg(rt)));
                     /* t5 holds 1 if t4 is equal to 0x8000. */
                     assign(t5, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32, mkexpr(t4)),
                                      mkU32(0x00008000)));
                     /* t6 holds 1 if value in t4 is negative, 0 otherwise. */
                     assign(t6, unop(Iop_32to1,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 getIReg(rt),
                                                 mkU32(0x80000000)),
                                           mkU8(31))));
                     /* t7 holds abs(t4). */
                     assign(t7, IRExpr_ITE(mkexpr(t5),
                                           mkU16(0x7FFF),
                                           IRExpr_ITE(mkexpr(t6),
                                                      binop(Iop_Add16,
                                                            unop(Iop_Not16,
                                                                 mkexpr(t4)),
                                                            mkU16(0x1)),
                                                      mkexpr(t4))));
                     /* If any of the two input halfwords is equal 0x8000,
                        set bit 20 in DSPControl register. */
                     assign(t8, binop(Iop_Or32,
                                      unop(Iop_1Sto32, mkexpr(t5)),
                                      unop(Iop_1Sto32, mkexpr(t1))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    mkexpr(t8),
                                                    mkU32(0x0)),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000))));

                     /* t9 = t7|t3 */
                     assign(t9, binop(Iop_16HLto32, mkexpr(t7), mkexpr(t3)));

                     putIReg(rd, mkexpr(t9));
                     break;
                  }
                  case 0xA: {  /* REPL.PH */
                     DIP("repl.ph r%d, %d", rd, dsp_imm);
                     vassert(!mode64);
                     UShort immediate = extend_s_10to16(dsp_imm);

                     putIReg(rd, mkU32(immediate << 16 | immediate));
                     break;
                  }
                  case 0xB: {  /* REPLV.PH */
                     DIP("replv.ph r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, getIReg(rt)),
                                       unop(Iop_32to16, getIReg(rt))));
                     break;
                  }
                  case 0xC: {  /* PRECEQ.W.PHL */
                     DIP("preceq.w.phl r%d, r%d", rd, rt);
                     vassert(!mode64);
                     putIReg(rd, binop(Iop_And32,
                                       getIReg(rt),
                                       mkU32(0xffff0000)));
                     break;
                  }
                  case 0xD: {  /* PRECEQ.W.PHR */
                     DIP("preceq.w.phr r%d, r%d", rd, rt);
                     vassert(!mode64);
                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, getIReg(rt)),
                                       mkU16(0x0)));
                     break;
                  }
                  case 0x11: {  /* ABSQ_S.W */
                     DIP("absq_s.w r%d, r%d", rd, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I1);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);

                     assign(t0,
                            binop(Iop_CmpEQ32, getIReg(rt), mkU32(0x80000000)));

                     putDSPControl(IRExpr_ITE(mkexpr(t0),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     assign(t1, binop(Iop_CmpLT32S, getIReg(rt), mkU32(0x0)));

                     assign(t2, IRExpr_ITE(mkexpr(t0),
                                           mkU32(0x7FFFFFFF),
                                           IRExpr_ITE(mkexpr(t1),
                                                      binop(Iop_Add32,
                                                            unop(Iop_Not32,
                                                                 getIReg(rt)),
                                                            mkU32(0x1)),
                                                      getIReg(rt))));
                     putIReg(rd, mkexpr(t2));
                     break;
                  }
                  case 0x1B: {  /* BITREV */
                     DIP("bitrev r%d, r%d", rd, rt);
                     vassert(!mode64);
                     /* 32bit reversal as seen on Bit Twiddling Hacks site
                        http://graphics.stanford.edu/~seander/bithacks.html
                        section ReverseParallel */
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);

                     assign(t1, binop(Iop_Or32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  getIReg(rt),
                                                  mkU32(0xaaaaaaaa)),
                                            mkU8(0x1)),
                                      binop(Iop_Shl32,
                                            binop(Iop_And32,
                                                  getIReg(rt),
                                                  mkU32(0x55555555)),
                                            mkU8(0x1))));
                     assign(t2, binop(Iop_Or32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t1),
                                                  mkU32(0xcccccccc)),
                                            mkU8(0x2)),
                                      binop(Iop_Shl32,
                                            binop(Iop_And32,
                                                  mkexpr(t1),
                                                  mkU32(0x33333333)),
                                            mkU8(0x2))));
                     assign(t3, binop(Iop_Or32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t2),
                                                  mkU32(0xf0f0f0f0)),
                                            mkU8(0x4)),
                                      binop(Iop_Shl32,
                                            binop(Iop_And32,
                                                  mkexpr(t2),
                                                  mkU32(0x0f0f0f0f)),
                                            mkU8(0x4))));
                     assign(t4, binop(Iop_Or32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t3),
                                                  mkU32(0xff00ff00)),
                                            mkU8(0x8)),
                                      binop(Iop_Shl32,
                                            binop(Iop_And32,
                                                  mkexpr(t3),
                                                  mkU32(0x00ff00ff)),
                                            mkU8(0x8))));
                     assign(t5, binop(Iop_Or32,
                                      binop(Iop_Shr32,
                                            mkexpr(t4),
                                            mkU8(0x10)),
                                      binop(Iop_Shl32,
                                            mkexpr(t4),
                                            mkU8(0x10))));
                     putIReg(rd, binop(Iop_Shr32,
                                       mkexpr(t5),
                                       mkU8(16)));
                     break;
                  }
                  case 0x1C: {  /* PRECEU.PH.QBL */
                     DIP("preceu.ph.qbl r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0xff000000)),
                                             mkU8(8)),
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x00ff0000)),
                                             mkU8(16))));
                     break;
                  }
                  case 0x1E: {  /* PRECEU.PH.QBLA */
                     DIP("preceu.ph.qbla r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0xff000000)),
                                             mkU8(8)),
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x0000ff00)),
                                             mkU8(8))));
                     break;
                  }
                  case 0x1D: {  /* PRECEU.PH.QBR */
                     DIP("preceu.ph.qbr r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Shl32,
                                             binop(Iop_And32,
                                                   getIReg(rt),
                                                   mkU32(0x0000ff00)),
                                             mkU8(8)),
                                       binop(Iop_And32,
                                             getIReg(rt),
                                             mkU32(0x000000ff))));
                     break;
                  }
                  case 0x1F: {  /* PRECEU.PH.QBRA */
                     DIP("preceu.ph.qbra r%d, r%d", rd, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_And32,
                                             getIReg(rt),
                                             mkU32(0x00ff0000)),
                                       binop(Iop_And32,
                                             getIReg(rt),
                                             mkU32(0x000000ff))));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of ABSQ_S.PH */
            }
            case 0x38: {  /* EXTR.W */
               switch(sa) {
                  case 0x0: {  /* EXTR.W */
                     DIP("extr.w r%d, ac%d, %d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I1);
                     t12 = newTemp(Ity_I1);
                     t13 = newTemp(Ity_I1);
                     t14 = newTemp(Ity_I32);

                     assign(t0, getAcc(ac));
                     if (0 == rs) {
                        assign(t1, mkexpr(t0));
                     } else {
                        assign(t1, binop(Iop_Sar64, mkexpr(t0), mkU8(rs)));
                     }
                     /* Check if bits 63..31 of the result in t1 aren't 0. */
                     assign(t3, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0)));
                     assign(t4, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));
                     /* Check if bits 63..31 of the result in t1 aren't
                        0x1ffffffff. */
                     assign(t5, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0xffffffff)));
                     assign(t6, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));
                     /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
                        control register. */
                     assign(t7, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t3)),
                                            unop(Iop_1Sto32, mkexpr(t4))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t5)),
                                            unop(Iop_1Sto32, mkexpr(t6)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t7),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));

                     /* If the last discarded bit is 1, there would be carry
                        when rounding, otherwise there wouldn't. We use that
                        fact and just add the value of the last discarded bit
                        to the least sifgnificant bit of the shifted value
                        from acc. */
                     if (0 == rs) {
                        assign(t8, mkU64(0x0ULL));
                     } else {
                        assign(t8, binop(Iop_And64,
                                         binop(Iop_Shr64,
                                               mkexpr(t0),
                                               mkU8(rs-1)),
                                         mkU64(0x1ULL)));
                     }
                     assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));

                     /* Repeat previous steps for the rounded value. */
                     assign(t10, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0)));
                     assign(t11, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));

                     assign(t12, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0xffffffff)));
                     assign(t13, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));

                     assign(t14, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t10)),
                                            unop(Iop_1Sto32, mkexpr(t11))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t12)),
                                            unop(Iop_1Sto32, mkexpr(t13)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t14),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));
                     if (0 == rs) {
                        putIReg(rt, unop(Iop_64to32, mkexpr(t0)));
                     } else {
                        putIReg(rt, unop(Iop_64to32, mkexpr(t1)));
                     }
                     break;
                  }
                  case 0x1: {  /* EXTRV.W */
                     DIP("extrv.w r%d, ac%d, r%d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I1);
                     t12 = newTemp(Ity_I1);
                     t13 = newTemp(Ity_I1);
                     t14 = newTemp(Ity_I32);
                     t15 = newTemp(Ity_I8);

                     assign(t15, unop(Iop_32to8,
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x1f))));
                     assign(t0, getAcc(ac));
                     assign(t1, binop(Iop_Sar64, mkexpr(t0), mkexpr(t15)));
                     putIReg(rt, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                        unop(Iop_8Uto32,
                                                             mkexpr(t15)),
                                                        mkU32(0)),
                                                  unop(Iop_64to32, mkexpr(t0)),
                                                  unop(Iop_64to32, mkexpr(t1))));

                     /* Check if bits 63..31 of the result in t1 aren't 0. */
                     assign(t3, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0)));
                     assign(t4, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));
                     /* Check if bits 63..31 of the result in t1 aren't
                        0x1ffffffff. */
                     assign(t5, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0xffffffff)));
                     assign(t6, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));
                     /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
                        control register. */
                     assign(t7, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t3)),
                                            unop(Iop_1Sto32, mkexpr(t4))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t5)),
                                            unop(Iop_1Sto32, mkexpr(t6)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t7),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));

                     /* If the last discarded bit is 1, there would be carry
                        when rounding, otherwise there wouldn't. We use that
                        fact and just add the value of the last discarded bit
                        to the least sifgnificant bit of the shifted value
                        from acc. */
                     assign(t8,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             unop(Iop_8Uto32,
                                                  mkexpr(t15)),
                                             mkU32(0)),
                                       mkU64(0x0ULL),
                                       binop(Iop_And64,
                                             binop(Iop_Shr64,
                                                   mkexpr(t0),
                                                   unop(Iop_32to8,
                                                        binop(Iop_Sub32,
                                                              unop(Iop_8Uto32,
                                                                   mkexpr(t15)),
                                                                   mkU32(1)))),
                                             mkU64(0x1ULL))));

                     assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));

                     /* Repeat previous steps for the rounded value. */
                     assign(t10, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0)));
                     assign(t11, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));

                     assign(t12, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0xffffffff)));
                     assign(t13, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));

                     assign(t14, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t10)),
                                            unop(Iop_1Sto32, mkexpr(t11))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t12)),
                                            unop(Iop_1Sto32, mkexpr(t13)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t14),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));
                     break;
                  }
                  case 0x2: {  /* EXTP */
                     DIP("extp r%d, ac%d, %d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I8);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I32);

                     assign(t0, getAcc(ac));
                     /* Extract pos field of DSPControl register. */
                     assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));

                     /* Check if (pos - size) >= 0 [size <= pos]
                        if (pos < size)
                           put 1 to EFI field of DSPControl register
                        else
                           extract bits from acc and put 0 to EFI field of
                           DSPCtrl */
                     assign(t2, binop(Iop_CmpLT32U, mkexpr(t1), mkU32(rs)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    binop(Iop_And32,
                                                          getDSPControl(),
                                                          mkU32(0xffffbfff)),
                                                    mkU32(0x4000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xffffbfff))));

                     /* If pos <= 31, shift right the value from the acc
                        (pos-size) times and take (size+1) bits from the least
                        significant positions. Otherwise, shift left the value
                        (63-pos) times, take (size+1) bits from the most
                        significant positions and shift right (31-size) times.*/
                     assign(t3, binop(Iop_CmpLE32U, mkexpr(t1), mkU32(31)));

                     assign(t4,
                           IRExpr_ITE(mkexpr(t3),
                                      unop(Iop_32to8,
                                           binop(Iop_Sub32,
                                                 mkexpr(t1), mkU32(rs))),
                                      unop(Iop_32to8,
                                           binop(Iop_Sub32,
                                                 mkU32(63), mkexpr(t1)))));

                     assign(t5, IRExpr_ITE(mkexpr(t3),
                                           binop(Iop_Shr64,
                                                 mkexpr(t0), mkexpr(t4)),
                                           binop(Iop_Shl64,
                                                 mkexpr(t0), mkexpr(t4))));

                     /* t6 holds a mask for bit extraction */
                     assign(t6,
                            IRExpr_ITE(mkexpr(t3),
                                       unop(Iop_Not64,
                                            binop(Iop_Shl64,
                                                  mkU64(0xffffffffffffffffULL),
                                                  mkU8(rs+1))),
                                       unop(Iop_Not64,
                                            binop(Iop_Shr64,
                                                  mkU64(0xffffffffffffffffULL),
                                                  mkU8(rs+1)))));

                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           unop(Iop_64to32,
                                                binop(Iop_And64,
                                                      mkexpr(t5),
                                                      mkexpr(t6))),
                                           binop(Iop_Shr32,
                                                 unop(Iop_64HIto32,
                                                      binop(Iop_And64,
                                                            mkexpr(t5),
                                                            mkexpr(t6))),
                                                 mkU8(31-rs))));

                     putIReg(rt, mkexpr(t7));
                     break;
                  }
                  case 0x3: {  /* EXTPV */
                     DIP("extpv r%d, ac%d, r%d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I8);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t8, binop(Iop_And32, getIReg(rs), mkU32(0x1f)));
                     assign(t0, getAcc(ac));
                     /* Extract pos field of DSPControl register. */
                     assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));

                     /* Check if (pos - size) >= 0 [size <= pos]
                        if (pos < size)
                           put 1 to EFI field of DSPControl register
                        else
                           extract bits from acc and put 0 to EFI field of
                           DSPCtrl */
                     assign(t2, binop(Iop_CmpLT32U, mkexpr(t1), mkexpr(t8)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    binop(Iop_And32,
                                                          getDSPControl(),
                                                          mkU32(0xffffbfff)),
                                                    mkU32(0x4000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xffffbfff))));

                     /* If pos <= 31, shift right the value from the acc
                        (pos-size) times and take (size+1) bits from the least
                        significant positions. Otherwise, shift left the value
                        (63-pos) times, take (size+1) bits from the most
                        significant positions and shift right (31-size)
                        times. */
                     assign(t3, binop(Iop_CmpLE32U, mkexpr(t1), mkU32(31)));

                     assign(t4,
                           IRExpr_ITE(mkexpr(t3),
                                      unop(Iop_32to8,
                                           binop(Iop_Sub32,
                                                 mkexpr(t1), mkexpr(t8))),
                                      unop(Iop_32to8,
                                           binop(Iop_Sub32,
                                                 mkU32(63), mkexpr(t1)))));

                     assign(t5, IRExpr_ITE(mkexpr(t3),
                                           binop(Iop_Shr64,
                                                 mkexpr(t0), mkexpr(t4)),
                                           binop(Iop_Shl64,
                                                 mkexpr(t0), mkexpr(t4))));

                     /* t6 holds a mask for bit extraction. */
                     assign(t6,
                            IRExpr_ITE(mkexpr(t3),
                                       unop(Iop_Not64,
                                            binop(Iop_Shl64,
                                                  mkU64(0xffffffffffffffffULL),
                                                  unop(Iop_32to8,
                                                       binop(Iop_Add32,
                                                             mkexpr(t8),
                                                             mkU32(1))))),
                                       unop(Iop_Not64,
                                            binop(Iop_Shr64,
                                                  mkU64(0xffffffffffffffffULL),
                                                  unop(Iop_32to8,
                                                       binop(Iop_Add32,
                                                             mkexpr(t8),
                                                             mkU32(1)))))));

                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           unop(Iop_64to32,
                                                binop(Iop_And64,
                                                      mkexpr(t5),
                                                      mkexpr(t6))),
                                           binop(Iop_Shr32,
                                                 unop(Iop_64HIto32,
                                                      binop(Iop_And64,
                                                            mkexpr(t5),
                                                            mkexpr(t6))),
                                                 unop(Iop_32to8,
                                                      binop(Iop_Sub32,
                                                            mkU32(31),
                                                            mkexpr(t8))))));

                     putIReg(rt, mkexpr(t7));
                     break;
                  }
                  case 0x4: {  /* EXTR_R.W */
                     DIP("extr_r.w r%d, ac%d, %d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I1);
                     t12 = newTemp(Ity_I1);
                     t13 = newTemp(Ity_I1);
                     t14 = newTemp(Ity_I32);
                     t15 = newTemp(Ity_I64);
                     t16 = newTemp(Ity_I1);

                     assign(t0, getAcc(ac));
                     assign(t16, binop(Iop_CmpEQ32,
                                       mkU32(rs),
                                       mkU32(0)));
                     assign(t1, IRExpr_ITE(mkexpr(t16),
                                           mkexpr(t0),
                                           binop(Iop_Sar64,
                                                 mkexpr(t0),
                                                 mkU8(rs))));
                     /* If the last discarded bit is 1, there would be carry
                        when rounding, otherwise there wouldn't. We use that
                        fact and just add the value of the last discarded bit
                        to the least significant bit of the shifted value
                        from acc. */
                     assign(t15, binop(Iop_Shr64,
                                       mkexpr(t0),
                                       unop(Iop_32to8,
                                            binop(Iop_Sub32,
                                                  binop(Iop_And32,
                                                        mkU32(rs),
                                                        mkU32(0x1f)),
                                                  mkU32(1)))));

                     assign(t8,
                            IRExpr_ITE(mkexpr(t16),
                                       mkU64(0x0ULL),
                                       binop(Iop_And64,
                                             mkexpr(t15),
                                             mkU64(0x0000000000000001ULL))));
                     assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));
                     putIReg(rt, unop(Iop_64to32, mkexpr(t9)));

                     /* Check if bits 63..31 of the result in t1 aren't 0. */
                     assign(t3, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0)));
                     assign(t4, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));

                     /* Check if bits 63..31 of the result in t1 aren't
                        0x1ffffffff. */
                     assign(t5, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0xffffffff)));
                     assign(t6, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));
                     /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
                        control register. */
                     assign(t7, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t3)),
                                            unop(Iop_1Sto32, mkexpr(t4))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t5)),
                                            unop(Iop_1Sto32, mkexpr(t6)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t7),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));

                     /* Repeat previous steps for the rounded value. */
                     assign(t10, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0)));
                     assign(t11, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));

                     assign(t12, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0xffffffff)));
                     assign(t13, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));

                     assign(t14, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t10)),
                                            unop(Iop_1Sto32, mkexpr(t11))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t12)),
                                            unop(Iop_1Sto32, mkexpr(t13)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t14),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));
                     break;
                  }
                  case 0x5: {  /* EXTRV_R.W */
                     DIP("extrv_r.w r%d, ac%d, r%d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I1);
                     t12 = newTemp(Ity_I1);
                     t13 = newTemp(Ity_I1);
                     t14 = newTemp(Ity_I32);
                     t15 = newTemp(Ity_I8);

                     assign(t15, unop(Iop_32to8,
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x1f))));
                     assign(t0, getAcc(ac));
                     assign(t1, binop(Iop_Sar64, mkexpr(t0), mkexpr(t15)));

                     /* Check if bits 63..31 of the result in t1 aren't 0. */
                     assign(t3, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0)));
                     assign(t4, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));
                     /* Check if bits 63..31 of the result in t1 aren't
                        0x1ffffffff. */
                     assign(t5, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0xffffffff)));
                     assign(t6, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));
                     /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
                        control register. */
                     assign(t7, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t3)),
                                            unop(Iop_1Sto32, mkexpr(t4))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t5)),
                                            unop(Iop_1Sto32, mkexpr(t6)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t7),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));

                     /* If the last discarded bit is 1, there would be carry
                        when rounding, otherwise there wouldn't. We use that
                        fact and just add the value of the last discarded bit
                        to the least sifgnificant bit of the shifted value
                        from acc. */
                     assign(t8,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             unop(Iop_8Uto32,
                                                  mkexpr(t15)),
                                             mkU32(0)),
                                       mkU64(0x0ULL),
                                       binop(Iop_And64,
                                             binop(Iop_Shr64,
                                                   mkexpr(t0),
                                                   unop(Iop_32to8,
                                                        binop(Iop_Sub32,
                                                              unop(Iop_8Uto32,
                                                                   mkexpr(t15)),
                                                                   mkU32(1)))),
                                             mkU64(0x1ULL))));

                     assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));
                     /* Put rounded value in destination register. */
                     putIReg(rt, unop(Iop_64to32, mkexpr(t9)));

                     /* Repeat previous steps for the rounded value. */
                     assign(t10, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0)));
                     assign(t11, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));

                     assign(t12, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0xffffffff)));
                     assign(t13, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));

                     assign(t14, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t10)),
                                            unop(Iop_1Sto32, mkexpr(t11))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t12)),
                                            unop(Iop_1Sto32, mkexpr(t13)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t14),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));
                     break;
                  }
                  case 0x6: {  /* EXTR_RS.W */
                     DIP("extr_rs.w r%d, ac%d, %d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I1);
                     t12 = newTemp(Ity_I1);
                     t13 = newTemp(Ity_I1);
                     t14 = newTemp(Ity_I32);
                     t16 = newTemp(Ity_I32);

                     assign(t0, getAcc(ac));
                     if (0 == rs) {
                        assign(t1, mkexpr(t0));
                     } else {
                        assign(t1, binop(Iop_Sar64, mkexpr(t0), mkU8(rs)));
                     }

                     /* Check if bits 63..31 of the result in t1 aren't 0. */
                     assign(t3, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0)));
                     assign(t4, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));
                     /* Check if bits 63..31 of the result in t1 aren't
                        0x1ffffffff. */
                     assign(t5, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0xffffffff)));
                     assign(t6, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));
                     /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
                        control register. */
                     assign(t7, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t3)),
                                            unop(Iop_1Sto32, mkexpr(t4))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t5)),
                                            unop(Iop_1Sto32, mkexpr(t6)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t7),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));

                     /* If the last discarded bit is 1, there would be carry
                        when rounding, otherwise there wouldn't. We use that
                        fact and just add the value of the last discarded bit
                        to the least sifgnificant bit of the shifted value
                        from acc. */
                     if (0 == rs) {
                        assign(t8, mkU64(0x0ULL));
                     } else {
                        assign(t8, binop(Iop_And64,
                                         binop(Iop_Shr64,
                                               mkexpr(t0),
                                               mkU8(rs-1)),
                                         mkU64(0x1ULL)));
                     }

                     assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));

                     /* Repeat previous steps for the rounded value. */
                     assign(t10, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0)));
                     assign(t11, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));

                     assign(t12, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0xffffffff)));
                     assign(t13, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));

                     assign(t14, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t10)),
                                            unop(Iop_1Sto32, mkexpr(t11))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t12)),
                                            unop(Iop_1Sto32, mkexpr(t13)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t14),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));

                     assign(t16, binop(Iop_And32,
                                       unop(Iop_64HIto32,
                                            mkexpr(t9)),
                                       mkU32(0x80000000)));
                     putIReg(rt, IRExpr_ITE(binop(Iop_CmpNE32,
                                                  mkexpr(t14),
                                                  mkU32(0)),
                                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                                             mkexpr(t16),
                                                             mkU32(0)),
                                                       mkU32(0x7fffffff),
                                                       mkU32(0x80000000)),
                                            unop(Iop_64to32, mkexpr(t9))));
                     break;
                  }
                  case 0x7: {  /* EXTRV_RS.W */
                     DIP("extrv_rs.w r%d, ac%d, r%d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I1);
                     t12 = newTemp(Ity_I1);
                     t13 = newTemp(Ity_I1);
                     t14 = newTemp(Ity_I32);
                     t15 = newTemp(Ity_I32);
                     t16 = newTemp(Ity_I32);
                     t17 = newTemp(Ity_I1);

                     assign(t15, binop(Iop_And32,
                                       getIReg(rs),
                                       mkU32(0x1f)));
                     assign(t17, binop(Iop_CmpEQ32,
                                       mkexpr(t15),
                                       mkU32(0)));
                     assign(t0, getAcc(ac));
                     assign(t1, IRExpr_ITE(mkexpr(t17),
                                           mkexpr(t0),
                                           binop(Iop_Sar64,
                                                 mkexpr(t0),
                                                 unop(Iop_32to8,
                                                      mkexpr(t15)))));

                     /* Check if bits 63..31 of the result in t1 aren't 0. */
                     assign(t3, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0)));
                     assign(t4, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));
                     /* Check if bits 63..31 of the result in t1 aren't
                        0x1ffffffff. */
                     assign(t5, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t1)),
                                      mkU32(0xffffffff)));
                     assign(t6, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));
                     /* If bits 63..31 aren't 0 nor 0x1ffffffff, set DSP
                        control register. */
                     assign(t7, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t3)),
                                            unop(Iop_1Sto32, mkexpr(t4))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t5)),
                                            unop(Iop_1Sto32, mkexpr(t6)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t7),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));

                     /* If the last discarded bit is 1, there would be carry
                        when rounding, otherwise there wouldn't. We use that
                        fact and just add the value of the last discarded bit
                        to the least sifgnificant bit of the shifted value
                        from acc. */
                     assign(t8,
                            IRExpr_ITE(mkexpr(t17),
                                       mkU64(0x0ULL),
                                       binop(Iop_And64,
                                             binop(Iop_Shr64,
                                                   mkexpr(t0),
                                                   unop(Iop_32to8,
                                                        binop(Iop_Sub32,
                                                              mkexpr(t15),
                                                              mkU32(1)))),
                                             mkU64(0x1ULL))));

                     assign(t9, binop(Iop_Add64, mkexpr(t1), mkexpr(t8)));

                     /* Repeat previous steps for the rounded value. */
                     assign(t10, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0)));
                     assign(t11, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0)));

                     assign(t12, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32,
                                           mkexpr(t9)),
                                      mkU32(0xffffffff)));
                     assign(t13, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32,
                                                 mkexpr(t9)),
                                            mkU32(0x80000000)),
                                      mkU32(0x80000000)));

                     assign(t14, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t10)),
                                            unop(Iop_1Sto32, mkexpr(t11))),
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32, mkexpr(t12)),
                                            unop(Iop_1Sto32, mkexpr(t13)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t14),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));

                     assign(t16, binop(Iop_And32,
                                       unop(Iop_64HIto32,
                                            mkexpr(t9)),
                                       mkU32(0x80000000)));
                     putIReg(rt, IRExpr_ITE(binop(Iop_CmpNE32,
                                                  mkexpr(t14),
                                                  mkU32(0)),
                                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                                             mkexpr(t16),
                                                             mkU32(0)),
                                                       mkU32(0x7fffffff),
                                                       mkU32(0x80000000)),
                                            unop(Iop_64to32, mkexpr(t9))));
                     break;
                  }
                  case 0xA: {  /* EXTPDP */
                     DIP("extpdp r%d, ac%d, %d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I8);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t0, getAcc(ac));
                     /* Extract pos field of DSPControl register. */
                     assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));

                     /* Check if (pos - size) >= 0 [size <= pos]
                        if (pos < size)
                           put 1 to EFI field of DSPControl register
                        else
                           extract bits from acc and put 0 to EFI field of
                           DSPCtrl */
                     assign(t2, binop(Iop_CmpLT32U, mkexpr(t1), mkU32(rs)));

                     assign(t8, binop(Iop_Or32,
                                      binop(Iop_And32,
                                            getDSPControl(),
                                            mkU32(0xffffbfc0)),
                                      binop(Iop_And32,
                                            binop(Iop_Sub32,
                                                  binop(Iop_And32,
                                                        getDSPControl(),
                                                        mkU32(0x3f)),
                                                  mkU32(rs+1)),
                                            mkU32(0x3f))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                     binop(Iop_And32,
                                                           getDSPControl(),
                                                           mkU32(0xffffbfff)),
                                                     mkU32(0x4000)),
                                              mkexpr(t8)));

                     /* If pos <= 31, shift right the value from the acc
                        (pos-size) times and take (size+1) bits from the least
                        significant positions. Otherwise, shift left the value
                        (63-pos) times, take (size+1) bits from the most
                        significant positions and shift right (31-size) times.
                     */
                     assign(t3, binop(Iop_CmpLE32U, mkexpr(t1), mkU32(31)));

                     assign(t4,
                            IRExpr_ITE(mkexpr(t3),
                                       unop(Iop_32to8,
                                            binop(Iop_Sub32,
                                                  mkexpr(t1), mkU32(rs))),
                                       unop(Iop_32to8,
                                            binop(Iop_Sub32,
                                                  mkU32(63), mkexpr(t1)))));

                     assign(t5, IRExpr_ITE(mkexpr(t3),
                                           binop(Iop_Shr64,
                                                 mkexpr(t0), mkexpr(t4)),
                                           binop(Iop_Shl64,
                                                 mkexpr(t0), mkexpr(t4))));

                     /* t6 holds a mask for bit extraction. */
                     assign(t6,
                            IRExpr_ITE(mkexpr(t3),
                                       unop(Iop_Not64,
                                            binop(Iop_Shl64,
                                                  mkU64(0xffffffffffffffffULL),
                                                  mkU8(rs+1))),
                                       unop(Iop_Not64,
                                            binop(Iop_Shr64,
                                                  mkU64(0xffffffffffffffffULL),
                                                  mkU8(rs+1)))));

                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           unop(Iop_64to32,
                                                binop(Iop_And64,
                                                      mkexpr(t5),
                                                      mkexpr(t6))),
                                           binop(Iop_Shr32,
                                                 unop(Iop_64HIto32,
                                                      binop(Iop_And64,
                                                            mkexpr(t5),
                                                            mkexpr(t6))),
                                                 mkU8(31-rs))));

                     putIReg(rt, mkexpr(t7));
                     break;
                  }
                  case 0xB: {  /* EXTPDPV */
                     DIP("extpdpv r%d, ac%d, r%d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I8);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);

                     assign(t8, binop(Iop_And32, getIReg(rs), mkU32(0x1f)));
                     assign(t0, getAcc(ac));
                     /* Extract pos field of DSPControl register. */
                     assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));

                     /* Check if (pos - size) >= 0 [size <= pos]
                        if (pos < size)
                           put 1 to EFI field of DSPControl register
                        else
                           extract bits from acc and put 0 to EFI field of
                           DSPCtrl */
                     assign(t2, binop(Iop_CmpLT32U, mkexpr(t1), mkexpr(t8)));

                     assign(t9, binop(Iop_Or32,
                                      binop(Iop_And32,
                                            getDSPControl(),
                                            mkU32(0xffffbfc0)),
                                      binop(Iop_And32,
                                            binop(Iop_Sub32,
                                                  binop(Iop_And32,
                                                        getDSPControl(),
                                                        mkU32(0x3f)),
                                                  binop(Iop_Add32,
                                                        mkexpr(t8),
                                                        mkU32(0x1))),
                                            mkU32(0x3f))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    binop(Iop_And32,
                                                          getDSPControl(),
                                                          mkU32(0xffffbfff)),
                                                    mkU32(0x4000)),
                                              mkexpr(t9)));

                     /* If pos <= 31, shift right the value from the acc
                        (pos-size) times and take (size+1) bits from the least
                        significant positions. Otherwise, shift left the value
                        (63-pos) times, take (size+1) bits from the most
                        significant positions and shift right (31-size) times.
                     */
                     assign(t3, binop(Iop_CmpLE32U, mkexpr(t1), mkU32(31)));

                     assign(t4,
                            IRExpr_ITE(mkexpr(t3),
                                      unop(Iop_32to8,
                                           binop(Iop_Sub32,
                                                 mkexpr(t1), mkexpr(t8))),
                                      unop(Iop_32to8,
                                           binop(Iop_Sub32,
                                                 mkU32(63), mkexpr(t1)))));

                     assign(t5, IRExpr_ITE(mkexpr(t3),
                                           binop(Iop_Shr64,
                                                 mkexpr(t0), mkexpr(t4)),
                                           binop(Iop_Shl64,
                                                 mkexpr(t0), mkexpr(t4))));

                     /* t6 holds a mask for bit extraction. */
                     assign(t6,
                            IRExpr_ITE(mkexpr(t3),
                                       unop(Iop_Not64,
                                            binop(Iop_Shl64,
                                                  mkU64(0xffffffffffffffffULL),
                                                  unop(Iop_32to8,
                                                       binop(Iop_Add32,
                                                             mkexpr(t8),
                                                             mkU32(1))))),
                                       unop(Iop_Not64,
                                            binop(Iop_Shr64,
                                                  mkU64(0xffffffffffffffffULL),
                                                  unop(Iop_32to8,
                                                       binop(Iop_Add32,
                                                             mkexpr(t8),
                                                             mkU32(1)))))));

                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           unop(Iop_64to32,
                                                binop(Iop_And64,
                                                      mkexpr(t5),
                                                      mkexpr(t6))),
                                           binop(Iop_Shr32,
                                                 unop(Iop_64HIto32,
                                                      binop(Iop_And64,
                                                            mkexpr(t5),
                                                            mkexpr(t6))),
                                                 unop(Iop_32to8,
                                                      binop(Iop_Sub32,
                                                            mkU32(31),
                                                            mkexpr(t8))))));

                     putIReg(rt, mkexpr(t7));
                     break;
                  }
                  case 0xE: {  /* EXTR_S.H */
                     DIP("extr_s.h r%d, ac%d, %d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I64);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);

                     assign(t0, getAcc(ac));

                     assign(t1, binop(Iop_Sar64, mkexpr(t0), mkU8(rs)));

                     assign(t2, binop(Iop_Or32,
                                      getDSPControl(), mkU32(0x00800000)));

                     assign(t9, binop(Iop_And32,
                                      unop(Iop_64to32,
                                           mkexpr(t1)),
                                      mkU32(0x80000000)));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t9),
                                                    binop(Iop_And32,
                                                          unop(Iop_64HIto32,
                                                               mkexpr(t0)),
                                                          mkU32(0x80000000))),
                                              mkexpr(t2),
                                              getDSPControl()));

                     /* Check if t1 > 0x7fff ((t1 - 0x7fff) > 0)
                        1. subtract 0x7fff from t1
                        2. if the resulting number is positive (sign bit = 0)
                           and any of the other bits is 1, the value is > 0. */
                     assign(t3, binop(Iop_Sub64,
                                      mkexpr(t1),
                                      mkU64(0x0000000000007fffULL)));
                     assign(t4, binop(Iop_And32,
                                       binop(Iop_Or32,
                                            unop(Iop_1Sto32,
                                                 binop(Iop_CmpNE32,
                                                       mkU32(0),
                                                       binop(Iop_And32,
                                                             unop(Iop_64HIto32,
                                                                  mkexpr(t3)),
                                                             mkU32(0x7fffffff)))),
                                            unop(Iop_1Sto32,
                                                 binop(Iop_CmpNE32,
                                                       mkU32(0),
                                                       unop(Iop_64to32,
                                                            mkexpr(t3))))),
                                       unop(Iop_1Sto32,
                                            binop(Iop_CmpEQ32,
                                                  binop(Iop_And32,
                                                        unop(Iop_64HIto32,
                                                                  mkexpr(t3)),
                                                             mkU32(0x80000000)),
                                                  mkU32(0)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkU32(0),
                                                    mkexpr(t4)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));
                     /* Check if t1<0xffffffffffff8000 (0xffffffffffff8000-t1)>0
                        1. subtract t1 from 0xffffffffffff8000
                        2. if the resulting number is positive (sign bit = 0)
                            and any of the other bits is 1, the value is > 0 */
                     assign(t6, binop(Iop_Sub64,
                                       mkU64(0xffffffffffff8000ULL),
                                       mkexpr(t1)));
                     assign(t7, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32,
                                                 binop(Iop_CmpNE32,
                                                       mkU32(0),
                                                       binop(Iop_And32,
                                                             unop(Iop_64HIto32,
                                                                  mkexpr(t6)),
                                                             mkU32(0x7fffffff)))),
                                            unop(Iop_1Sto32,
                                                 binop(Iop_CmpNE32,
                                                       mkU32(0),
                                                       unop(Iop_64to32,
                                                            mkexpr(t6))))),
                                      unop(Iop_1Sto32,
                                            binop(Iop_CmpEQ32,
                                                  binop(Iop_And32,
                                                        unop(Iop_64HIto32,
                                                                  mkexpr(t6)),
                                                             mkU32(0x80000000)),
                                                  mkU32(0)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkU32(0),
                                                    mkexpr(t7)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));
                     putIReg(rt, IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkU32(0),
                                                    mkexpr(t4)),
                                            mkU32(0x00007fff),
                                            IRExpr_ITE(binop(Iop_CmpNE32,
                                                             mkU32(0),
                                                             mkexpr(t7)),
                                                       mkU32(0xffff8000),
                                                       unop(Iop_64to32,
                                                            mkexpr(t1)))));
                     break;
                  }
                  case 0xF: {  /* EXTRV_S.H */
                     DIP("extrv_s.h r%d, ac%d, %d", rt, ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I64);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);

                     assign(t0, getAcc(ac));

                     assign(t1, binop(Iop_Sar64,
                                      mkexpr(t0),
                                      unop(Iop_32to8,
                                           binop(Iop_And32,
                                                 getIReg(rs),
                                                 mkU32(0x1f)))));

                     assign(t2, binop(Iop_Or32,
                                      getDSPControl(), mkU32(0x00800000)));

                     assign(t9, binop(Iop_And32,
                                      unop(Iop_64to32,
                                           mkexpr(t1)),
                                      mkU32(0x80000000)));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t9),
                                                    binop(Iop_And32,
                                                          unop(Iop_64HIto32,
                                                               mkexpr(t0)),
                                                          mkU32(0x80000000))),
                                              mkexpr(t2),
                                              getDSPControl()));

                     /* Check if t1 > 0x7fff ((t1 - 0x7fff) > 0)
                        1. subtract 0x7fff from t1
                        2. if the resulting number is positive (sign bit = 0)
                           and any of the other bits is 1, the value is > 0. */
                     assign(t3, binop(Iop_Sub64,
                                      mkexpr(t1),
                                      mkU64(0x0000000000007fffULL)));
                     assign(t4, binop(Iop_And32,
                                       binop(Iop_Or32,
                                            unop(Iop_1Sto32,
                                                 binop(Iop_CmpNE32,
                                                       mkU32(0),
                                                       binop(Iop_And32,
                                                             unop(Iop_64HIto32,
                                                                  mkexpr(t3)),
                                                             mkU32(0x7fffffff)))),
                                            unop(Iop_1Sto32,
                                                 binop(Iop_CmpNE32,
                                                       mkU32(0),
                                                       unop(Iop_64to32,
                                                            mkexpr(t3))))),
                                       unop(Iop_1Sto32,
                                            binop(Iop_CmpEQ32,
                                                  binop(Iop_And32,
                                                        unop(Iop_64HIto32,
                                                                  mkexpr(t3)),
                                                             mkU32(0x80000000)),
                                                  mkU32(0)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkU32(0),
                                                    mkexpr(t4)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));
                     /* Check if t1<0xffffffffffff8000 (0xffffffffffff8000-t1)>0
                        1. subtract t1 from 0xffffffffffff8000
                        2. if the resulting number is positive (sign bit = 0)
                            and any of the other bits is 1, the value is > 0 */
                     assign(t6, binop(Iop_Sub64,
                                       mkU64(0xffffffffffff8000ULL),
                                       mkexpr(t1)));
                     assign(t7, binop(Iop_And32,
                                      binop(Iop_Or32,
                                            unop(Iop_1Sto32,
                                                 binop(Iop_CmpNE32,
                                                       mkU32(0),
                                                       binop(Iop_And32,
                                                             unop(Iop_64HIto32,
                                                                  mkexpr(t6)),
                                                             mkU32(0x7fffffff)))),
                                            unop(Iop_1Sto32,
                                                 binop(Iop_CmpNE32,
                                                       mkU32(0),
                                                       unop(Iop_64to32,
                                                            mkexpr(t6))))),
                                      unop(Iop_1Sto32,
                                            binop(Iop_CmpEQ32,
                                                  binop(Iop_And32,
                                                        unop(Iop_64HIto32,
                                                                  mkexpr(t6)),
                                                             mkU32(0x80000000)),
                                                  mkU32(0)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkU32(0),
                                                    mkexpr(t7)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00800000)),
                                              getDSPControl()));
                     putIReg(rt, IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkU32(0),
                                                    mkexpr(t4)),
                                            mkU32(0x00007fff),
                                            IRExpr_ITE(binop(Iop_CmpNE32,
                                                             mkU32(0),
                                                             mkexpr(t7)),
                                                       mkU32(0xffff8000),
                                                       unop(Iop_64to32,
                                                            mkexpr(t1)))));
                     break;
                  }
                  case 0x12: {  /* RDDSP*/
                     DIP("rddsp r%d, mask 0x%x", rd, rddsp_mask);
                     vassert(!mode64);

                     putIReg(rd, mkU32(0x0));

                     if ((rddsp_mask & 0x1) == 0x1) {
                        /* Read pos field (bits 5-0) of DSPControl register. */
                        putIReg(rd, binop(Iop_Or32,
                                          getIReg(rd),
                                          binop(Iop_And32,
                                                getDSPControl(),
                                                mkU32(0x0000003F))));
                     }

                     if ((rddsp_mask & 0x2) == 0x2) {
                        /* Read scount field (bits 12-7) of DSPControl
                           register. */
                        putIReg(rd, binop(Iop_Or32,
                                          getIReg(rd),
                                          binop(Iop_And32,
                                                getDSPControl(),
                                                mkU32(0x00001F80))));
                     }

                     if ((rddsp_mask & 0x4) == 0x4) {
                        /* Read C field (bit 13) of DSPControl register. */
                        putIReg(rd, binop(Iop_Or32,
                                          getIReg(rd),
                                          binop(Iop_And32,
                                                getDSPControl(),
                                                mkU32(0x00002000))));
                     }

                     if ((rddsp_mask & 0x8) == 0x8) {
                        /* Read outflag field (bit s 23-16) of DSPControl
                           register. */
                        putIReg(rd, binop(Iop_Or32,
                                          getIReg(rd),
                                          binop(Iop_And32,
                                                getDSPControl(),
                                                mkU32(0x00FF0000))));
                     }

                     if ((rddsp_mask & 0x10) == 0x10) {
                        /* Read ccond field (bits 31-24) of DSPControl
                           register. */
                        putIReg(rd, binop(Iop_Or32,
                                          getIReg(rd),
                                          binop(Iop_And32,
                                                getDSPControl(),
                                                mkU32(0xFF000000))));
                     }

                     if ((rddsp_mask & 0x20) == 0x20) {
                        /* Read EFI field (bit 14) of DSPControl register. */
                        putIReg(rd, binop(Iop_Or32,
                                          getIReg(rd),
                                          binop(Iop_And32,
                                                getDSPControl(),
                                                mkU32(0x00004000))));
                     }

                     if ((rddsp_mask & 0x3f) == 0x3f) {
                        /* Read all fields of DSPControl register. */
                        putIReg(rd, getDSPControl());
                     }
                     break;
                  }
                  case 0x13: {  /* WRDSP */
                     DIP("wrdsp r%d, mask 0x%x", rs, wrdsp_mask);
                     vassert(!mode64);

                     if ((wrdsp_mask & 0x3f) == 0x3f) {
                        /* If mips64 put all fields of rs, except bit 15 and bit
                           6, to DSPControl register, otherwise put all except
                           bits 15, 6 and bits 31..28. */
                        putDSPControl(mode64 ?
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0xffff7fbf)) :
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x0fff7fbf)));
                     } else {
                        if ((wrdsp_mask & 0x1) == 0x1) {
                           /* Put bits 5-0 of rs to DSPControl register pos
                              field. */
                           putDSPControl(binop(Iop_Or32,
                                               binop(Iop_And32,
                                                     getDSPControl(),
                                                     mkU32(0xFFFF7F40)),
                                               binop(Iop_And32,
                                                     getIReg(rs),
                                                     mkU32(0x0000003F))));
                        }

                        if ((wrdsp_mask & 0x2) == 0x2) {
                           /* Put bits 12-7 of rs to DSPControl scount field. */
                           putDSPControl(binop(Iop_Or32,
                                               binop(Iop_And32,
                                                     getDSPControl(),
                                                     mkU32(0xFFFFE03F)),
                                               binop(Iop_And32,
                                                     getIReg(rs),
                                                     mkU32(0x00001F80))));
                        }

                        if ((wrdsp_mask & 0x4) == 0x4) {
                           /* Put bit 13 of rs to DSPControl register C
                              field. */
                           putDSPControl(binop(Iop_Or32,
                                               binop(Iop_And32,
                                                     getDSPControl(),
                                                     mkU32(0xFFFF5FBF)),
                                               binop(Iop_And32,
                                                     getIReg(rs),
                                                     mkU32(0x00002000))));
                        }

                        if ((wrdsp_mask & 0x8) == 0x8) {
                           /* Put bits 23-16 of rs to DSPControl reg outflag
                              field. */
                           putDSPControl(binop(Iop_Or32,
                                               binop(Iop_And32,
                                                     getDSPControl(),
                                                     mkU32(0xFF007FBF)),
                                               binop(Iop_And32,
                                                     getIReg(rs),
                                                     mkU32(0x00FF0000))));
                        }

                        if ((wrdsp_mask & 0x10) == 0x10) {
                           /* Put bits 31-24 of rs to DSPControl reg ccond
                              field. */
                           putDSPControl(binop(Iop_Or32,
                                               binop(Iop_And32,
                                                     getDSPControl(),
                                                     mkU32(0x00FF7FBF)),
                                               binop(Iop_And32,
                                                     getIReg(rs),
                                                     mode64 ? mkU32(0xFF000000)
                                                            : mkU32(0x0F000000))
                                               )
                                        );
                        }

                        if ((wrdsp_mask & 0x20) == 0x20) {
                           /* Put bit 14 of rs to DSPControl register EFI
                              field. */
                           putDSPControl(binop(Iop_Or32,
                                               binop(Iop_And32,
                                                     getDSPControl(),
                                                     mkU32(0xFFFF3FBF)),
                                               binop(Iop_And32,
                                                     getIReg(rs),
                                                     mkU32(0x00004000))));
                        }
                     }
                     break;
                  }
                  case 0x1A: {  /* SHILO */
                     DIP("shilo ac%d, %d", ac, shift);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);

                     assign(t0, getAcc(ac));

                     putAcc(ac, mkexpr(t0));

                     if (0x20 == (shift & 0x3f)) {
                        putAcc(ac, binop(Iop_32HLto64,
                                         unop(Iop_64to32, mkexpr(t0)),
                                         mkU32(0x0)));
                     } else if (0x20 == (shift & 0x20)) {
                        assign(t1, binop(Iop_Shl64,
                                         mkexpr(t0),
                                         unop(Iop_32to8,
                                              binop(Iop_Add32,
                                                    unop(Iop_Not32,
                                                         mkU32(shift)),
                                                    mkU32(0x1)))));

                        putAcc(ac, mkexpr(t1));
                     } else {
                        assign(t1, binop(Iop_Shr64, mkexpr(t0), mkU8(shift)));

                        putAcc(ac, mkexpr(t1));
                     }
                     break;
                  }
                  case 0x1B: {  /* SHILOV */
                     DIP("shilov ac%d, r%d", ac, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I64);
                     t4 = newTemp(Ity_I64);

                     assign(t0, getAcc(ac));
                     assign(t1, binop(Iop_And32, getIReg(rs), mkU32(0x3f)));
                     assign(t2, binop(Iop_CmpEQ32, mkexpr(t1), mkU32(0x20)));
                     assign(t3, binop(Iop_Shl64,
                                      mkexpr(t0),
                                      unop(Iop_32to8,
                                           binop(Iop_Add32,
                                                 unop(Iop_Not32,
                                                      mkexpr(t1)),
                                                 mkU32(0x1)))));
                     assign(t4, binop(Iop_Shr64,
                                      mkexpr(t0),
                                      unop(Iop_32to8,
                                           mkexpr(t1))));

                     putAcc(ac,
                            IRExpr_ITE(mkexpr(t2),
                                       binop(Iop_32HLto64,
                                             unop(Iop_64to32, mkexpr(t0)),
                                             mkU32(0x0)),
                                       IRExpr_ITE(binop(Iop_CmpEQ32,
                                                        binop(Iop_And32,
                                                              mkexpr(t1),
                                                              mkU32(0x20)),
                                                        mkU32(0x20)),
                                                  mkexpr(t3),
                                                  mkexpr(t4))));
                     break;
                  }
                  case 0x1F: {  /* MTHLIP */
                     DIP("mthlip r%d, ac%d", rs, ac);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);

                     assign(t0, getAcc(ac));
                     putAcc(ac, binop(Iop_32HLto64,
                                      unop(Iop_64to32, mkexpr(t0)),
                                      getIReg(rs)));
                     assign(t1, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpLE32U,
                                                    mkU32(32),
                                                    mkexpr(t1)),
                                              binop(Iop_Or32,
                                                    binop(Iop_Sub32,
                                                          mkexpr(t1),
                                                          mkU32(32)),
                                                   binop(Iop_And32,
                                                         getDSPControl(),
                                                         mkU32(0xffffffc0))),
                                              binop(Iop_Or32,
                                                    binop(Iop_Add32,
                                                          mkexpr(t1),
                                                          mkU32(32)),
                                                    binop(Iop_And32,
                                                          getDSPControl(),
                                                          mkU32(0xffffffc0)))));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of EXTR.W */
            }
            case 0xA: {  /* LX */
               switch(sa) {
                  case 0x0: {  /* LWX */
                     DIP("lwx r%d, r%d(r%d)", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_Add32, getIReg(rt), getIReg(rs)));

                     putIReg(rd, load(Ity_I32, mkexpr(t0)));
                     break;
                  }
                  case 0x4: {  /* LHX */
                     DIP("lhx r%d, r%d(r%d)", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_Add32, getIReg(rt), getIReg(rs)));

                     putIReg(rd, unop(Iop_16Sto32, load(Ity_I16, mkexpr(t0))));
                     break;
                  }
                  case 0x6: {  /* LBUX */
                     DIP("lbux r%d, r%d(r%d)", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_Add32, getIReg(rt), getIReg(rs)));

                     putIReg(rd, unop(Iop_8Uto32, load(Ity_I8, mkexpr(t0))));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of LX */
            }
            case 0xC: {  /* INSV */
               switch(sa) {
                  case 0x0: {  /* INSV */
                     DIP("insv r%d, r%d", rt, rs);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I8);
                     t3 = newTemp(Ity_I8);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);

                     /* t0 <- pos field of DSPControl register. */
                     assign(t0, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));
                     /* t1 <- scount field of DSPControl register. */
                     assign(t1, binop(Iop_Shr32,
                                      binop(Iop_And32,
                                            getDSPControl(),
                                            mkU32(0x1f80)),
                                      mkU8(7)));

                     assign(t2, unop(Iop_32to8,
                                     binop(Iop_Add32,
                                           mkexpr(t1),
                                           mkexpr(t0))));

                     /* 32-(pos+size) most significant bits of rt. */
                     assign(t6, binop(Iop_Shl32,
                                      binop(Iop_Shr32,
                                            getIReg(rt),
                                            mkexpr(t2)),
                                      mkexpr(t2)));

                     assign(t3, unop(Iop_32to8,
                                     binop(Iop_Sub32,
                                           mkU32(32),
                                           mkexpr(t0))));
                     /* Pos least significant bits of rt. */
                     assign(t7, binop(Iop_Shr32,
                                      binop(Iop_Shl32,
                                            getIReg(rt),
                                            mkexpr(t3)),
                                      mkexpr(t3)));

                     /* Size least significant bits of rs,
                        shifted to appropriate position. */
                     assign(t8, binop(Iop_Shl32,
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            unop(Iop_Not32,
                                                 binop(Iop_Shl32,
                                                       mkU32(0xffffffff),
                                                       unop(Iop_32to8,
                                                            mkexpr(t1))))),
                                      unop(Iop_32to8,
                                           mkexpr(t0))));

                     putIReg(rt, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                  mkexpr(t0),
                                                  mkU32(0)),
                                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                                             mkexpr(t1),
                                                             mkU32(32)),
                                                       getIReg(rs),
                                                       binop(Iop_Or32,
                                                             mkexpr(t6),
                                                             mkexpr(t8))),
                                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                                             unop(Iop_8Uto32,
                                                                  mkexpr(t2)),
                                                             mkU32(32)),
                                                       binop(Iop_Or32,
                                                             mkexpr(t7),
                                                             mkexpr(t8)),
                                                       binop(Iop_Or32,
                                                             binop(Iop_Or32,
                                                                   mkexpr(t6),
                                                                   mkexpr(t7)),
                                                             mkexpr(t8)))));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* enf of INSV */
            }
            case 0x10: {  /* ADDU.QB */
               switch(sa) {
                  case 0x00: {  /* ADDU.QB */
                     DIP("addu.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I32);

                     /* Add rightmost bytes of rs and rt. */
                     assign(t0,
                            binop(Iop_Add32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     /* t1 will be 1 if there is overflow, 0 otherwise. */
                     assign(t1, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t0),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));

                     /* Add bits 15-8 of rs and rt. */
                     assign(t2,
                            binop(Iop_Add32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     /* t3 will be 1 if there is overflow, 0 otherwise. */
                     assign(t3, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t2),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));

                     /* Add bits 15-8 of rs and rt. */
                     assign(t4,
                            binop(Iop_Add32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     /* t5 will be 1 if there is overflow, 0 otherwise. */
                     assign(t5, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t4),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));

                     /* Add bits 15-8 of rs and rt. */
                     assign(t6,
                            binop(Iop_Add32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     /* t7 will be 1 if there is overflow, 0 otherwise. */
                     assign(t7, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t6),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));

                     assign(t8,
                            binop(Iop_Or32,
                                  binop(Iop_Or32,
                                        binop(Iop_Or32,
                                              unop(Iop_1Sto32, mkexpr(t7)),
                                              unop(Iop_1Sto32,  mkexpr(t5))),
                                        unop(Iop_1Sto32, mkexpr(t3))),
                                  unop(Iop_1Sto32, mkexpr(t1))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    mkexpr(t8),
                                                    mkU32(0x0)),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000))));

                     putIReg(rd, binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             unop(Iop_32to8, mkexpr(t6)),
                                             unop(Iop_32to8, mkexpr(t4))),
                                       binop(Iop_8HLto16,
                                             unop(Iop_32to8, mkexpr(t2)),
                                             unop(Iop_32to8, mkexpr(t0)))));
                     break;
                  }
                  case 0x1: {  /* SUBU.QB */
                     DIP("subu.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I32);

                     /* Subtract rightmost bytes of rs and rt. */
                     assign(t0,
                            binop(Iop_Sub32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     /* t1 will be 1 if there is overflow, 0 otherwise. */
                     assign(t1, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t0),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));

                     /* Subtract bits 15-8 of rs and rt. */
                     assign(t2,
                            binop(Iop_Sub32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     /* t3 will be 1 if there is overflow, 0 otherwise. */
                     assign(t3, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t2),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));

                     /* Subtract bits 15-8 of rs and rt. */
                     assign(t4,
                            binop(Iop_Sub32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     /* t5 will be 1 if there is overflow, 0 otherwise. */
                     assign(t5, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t4),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));

                     /* Subtract bits 15-8 of rs and rt. */
                     assign(t6,
                            binop(Iop_Sub32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     /* t7 will be 1 if there is overflow, 0 otherwise. */
                     assign(t7, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t6),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));

                     assign(t8, binop(Iop_Or32,
                                      binop(Iop_Or32,
                                            binop(Iop_Or32,
                                                  unop(Iop_1Sto32, mkexpr(t7)),
                                                  unop(Iop_1Sto32, mkexpr(t5))),
                                            unop(Iop_1Sto32, mkexpr(t3))),
                                      unop(Iop_1Sto32, mkexpr(t1))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                     mkexpr(t8),
                                                     mkU32(0x0)),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000))));

                     putIReg(rd, binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             unop(Iop_32to8, mkexpr(t6)),
                                             unop(Iop_32to8, mkexpr(t4))),
                                       binop(Iop_8HLto16,
                                             unop(Iop_32to8, mkexpr(t2)),
                                             unop(Iop_32to8, mkexpr(t0)))));
                     break;
                  }
                  case 0x04: {  /* ADDU_S.QB */
                     DIP("addu_s.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I8);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I8);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I8);
                     t9 = newTemp(Ity_I32);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I8);
                     t12 = newTemp(Ity_I32);

                     /* Add rightmost bytes of rs and rt. */
                     assign(t0,
                            binop(Iop_Add32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     /* t1 will be 1 if there is overflow, 0 otherwise. */
                     assign(t1, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t0),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));
                     /* Saturate if necessary. */
                     assign(t2, IRExpr_ITE(mkexpr(t1),
                                           mkU8(0xff),
                                           unop(Iop_32to8, mkexpr(t0))));

                     /* Add bits 15-8 of rs and rt. */
                     assign(t3,
                            binop(Iop_Add32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     /* t4 will be 1 if there is overflow, 0 otherwise. */
                     assign(t4, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t3),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));
                     /* Saturate if necessary. */
                     assign(t5, IRExpr_ITE(mkexpr(t4),
                                           mkU8(0xff),
                                           unop(Iop_32to8, mkexpr(t3))));

                     /* Add bits 15-8 of rs and rt. */
                     assign(t6,
                            binop(Iop_Add32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     /* t7 will be 1 if there is overflow, 0 otherwise. */
                     assign(t7, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t6),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));
                     /* Saturate if necessary. */
                     assign(t8, IRExpr_ITE(mkexpr(t7),
                                           mkU8(0xff),
                                           unop(Iop_32to8, mkexpr(t6))));

                     /* Add bits 15-8 of rs and rt. */
                     assign(t9,
                            binop(Iop_Add32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     /* t10 will be 1 if there is overflow, 0 otherwise. */
                     assign(t10, binop(Iop_CmpEQ32,
                                       binop(Iop_And32,
                                             mkexpr(t9),
                                             mkU32(0x00000100)),
                                       mkU32(0x00000100)));
                     /* Saturate if necessary. */
                     assign(t11, IRExpr_ITE(mkexpr(t10),
                                            mkU8(0xff),
                                            unop(Iop_32to8, mkexpr(t9))));

                     assign(t12,
                            binop(Iop_Or32,
                                  binop(Iop_Or32,
                                        binop(Iop_Or32,
                                              unop(Iop_1Sto32, mkexpr(t10)),
                                              unop(Iop_1Sto32, mkexpr(t7))),
                                        unop(Iop_1Sto32, mkexpr(t4))),
                                  unop(Iop_1Sto32, mkexpr(t1))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    mkexpr(t12),
                                                    mkU32(0x0)),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000))));

                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   binop(Iop_8HLto16, mkexpr(t11), mkexpr(t8)),
                                   binop(Iop_8HLto16, mkexpr(t5), mkexpr(t2))));
                     break;
                  }
                  case 0x05: {  /* SUBU_S.QB */
                     DIP("subu_s.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);

                     /* Use C function to easily calculate the result
                        and write it in the register more conveniently
                        Underflow is checked using step by step subtraction. */
                     assign(t1, binop(Iop_QSub8Ux4, getIReg(rs), getIReg(rt)));

                     /* Subtract each byte of rs and rt. */
                     assign(t6,
                            binop(Iop_Sub32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t7,
                            binop(Iop_Sub32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t8,
                            binop(Iop_Sub32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t9,
                            binop(Iop_Sub32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rt))))));

                     /* Put 1 to bit 20 in DSPControl if there is underflow
                        in either byte. */
                     assign(t2, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t6),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     assign(t3, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t7),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));
                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     assign(t4, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t8),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));
                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     assign(t5, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            mkexpr(t9),
                                            mkU32(0x00000100)),
                                      mkU32(0x00000100)));
                     putDSPControl(IRExpr_ITE(mkexpr(t5),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     putIReg(rd, mkexpr(t1));
                     break;
                  }
                  case 0x6: {  /* MULEU_S.PH.QBL */
                     DIP("muleu_s.ph.qbl r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);

                     assign(t0,
                            unop(Iop_64to32,
                                 binop(Iop_MullU32,
                                       unop(Iop_8Uto32,
                                            unop(Iop_16HIto8,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rs)))),
                                       unop(Iop_16Uto32,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t1,
                            unop(Iop_64to32,
                                 binop(Iop_MullU32,
                                       unop(Iop_8Uto32,
                                            unop(Iop_16to8,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rs)))),
                                       unop(Iop_16Uto32,
                                            unop(Iop_32to16, getIReg(rt))))));

                     assign(t2, binop(Iop_CmpNE32,
                                      mkU32(0x0),
                                      binop(Iop_And32,
                                            mkexpr(t0),
                                            mkU32(0x03ff0000))));
                     assign(t3, binop(Iop_CmpNE32,
                                      mkU32(0x0),
                                      binop(Iop_And32,
                                            mkexpr(t1),
                                            mkU32(0x03ff0000))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x200000)),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x200000)),
                                                         getDSPControl())));
                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   IRExpr_ITE(mkexpr(t2),
                                              mkU16(0xffff),
                                              unop(Iop_32to16, mkexpr(t0))),
                                   IRExpr_ITE(mkexpr(t3),
                                              mkU16(0xffff),
                                              unop(Iop_32to16, mkexpr(t1)))));
                     break;
                  }
                  case 0x7: {  /* MULEU_S.PH.QBR */
                     DIP("muleu_s.ph.qbr r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);

                     assign(t0, unop(Iop_64to32,
                                     binop(Iop_MullU32,
                                           unop(Iop_8Uto32,
                                                unop(Iop_16HIto8,
                                                     unop(Iop_32to16,
                                                          getIReg(rs)))),
                                           unop(Iop_16Uto32,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t1, unop(Iop_64to32,
                                     binop(Iop_MullU32,
                                           unop(Iop_8Uto32,
                                                unop(Iop_16to8,
                                                     unop(Iop_32to16,
                                                          getIReg(rs)))),
                                           unop(Iop_16Uto32,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));

                     assign(t2, binop(Iop_CmpNE32,
                                      mkU32(0x0),
                                      binop(Iop_And32,
                                            mkexpr(t0),
                                            mkU32(0x03ff0000))));
                     assign(t3, binop(Iop_CmpNE32,
                                      mkU32(0x0),
                                      binop(Iop_And32,
                                            mkexpr(t1),
                                            mkU32(0x03ff0000))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x200000)),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x200000)),
                                                         getDSPControl())));
                     putIReg(rd, binop(Iop_16HLto32,
                                       IRExpr_ITE(mkexpr(t2),
                                                  mkU16(0xffff),
                                                  unop(Iop_32to16,
                                                       mkexpr(t0))),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU16(0xffff),
                                                  unop(Iop_32to16,
                                                       mkexpr(t1)))));
                     break;
                  }
                  case 0x08: {  /* ADDU.PH */
                     DIP("addu.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);

                     /* Add lower halves. */
                     assign(t0, binop(Iop_Add32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     /* Detect overflow. */
                     assign(t1, binop(Iop_CmpLT32U,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, mkexpr(t0))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs)))));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     /* Add higher halves. */
                     assign(t2, binop(Iop_Add32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));

                     /* Detect overflow. */
                     assign(t3, binop(Iop_CmpLT32U,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, mkexpr(t2))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16,
                                                getIReg(rs)))));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, mkexpr(t2)),
                                       unop(Iop_32to16, mkexpr(t0))));
                     break;
                  }
                  case 0x9: {  /* SUBU.PH */
                     DIP("subu.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);

                     /* Substract lower halves. */
                     assign(t0, binop(Iop_Sub32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     /* Detect underflow. */
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            mkexpr(t0),
                                            mkU32(0x00010000)),
                                      mkU32(0x0)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     /* Subtract higher halves. */
                     assign(t2, binop(Iop_Sub32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));

                     /* Detect underflow. */
                     assign(t3, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            mkexpr(t2),
                                            mkU32(0x00010000)),
                                      mkU32(0x0)));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, mkexpr(t2)),
                                       unop(Iop_32to16, mkexpr(t0))));
                     break;
                  }
                  case 0xA: {  /* ADDQ.PH */
                     DIP("addq.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);

                     /* Add lower halves. */
                     assign(t0, binop(Iop_Add32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     /* Bit 16 of the result. */
                     assign(t6, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, mkexpr(t0))),
                                      mkU32(0x1)));
                     /* Detect overflow. */
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t0),
                                                  mkU32(0x8000)),
                                            mkU8(15)),
                                      mkexpr(t6)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     /* Add higher halves. */
                     assign(t2, binop(Iop_Add32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));

                     /* Bit 16 of the result. */
                     assign(t7, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, mkexpr(t2))),
                                      mkU32(0x1)));
                     /* Detect overflow. */
                     assign(t3, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t2),
                                                  mkU32(0x00008000)),
                                            mkU8(15)),
                                      mkexpr(t7)));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, mkexpr(t2)),
                                       unop(Iop_32to16, mkexpr(t0))));
                     break;
                  }
                  case 0xB: {  /* SUBQ.PH */
                     DIP("subq.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);

                     /* Subtract lower halves. */
                     assign(t0, binop(Iop_Sub32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     /* Bit 16 of the result. */
                     assign(t6, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, mkexpr(t0))),
                                      mkU32(0x1)));
                     /* Compare the signs of input value and the result. */
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t0),
                                                  mkU32(0x8000)),
                                            mkU8(15)),
                                      mkexpr(t6)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     /* Subtract higher halves. */
                     assign(t2, binop(Iop_Sub32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));

                     /* Bit 16 of the result. */
                     assign(t7, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, mkexpr(t2))),
                                      mkU32(0x1)));
                     /* Compare the signs of input value and the result. */
                     assign(t3, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t2),
                                                  mkU32(0x00008000)),
                                            mkU8(15)),
                                      mkexpr(t7)));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, mkexpr(t2)),
                                       unop(Iop_32to16, mkexpr(t0))));
                     break;
                  }
                  case 0xC: {  /* ADDU_S.PH */
                     DIP("addu_s.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);

                     /* Add lower halves. */
                     assign(t0, binop(Iop_Add32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     /* Detect overflow. */
                     assign(t1, binop(Iop_CmpLT32U,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, mkexpr(t0))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs)))));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     /* Add higher halves. */
                     assign(t2, binop(Iop_Add32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));

                     /* Detect overflow. */
                     assign(t3, binop(Iop_CmpLT32U,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, mkexpr(t2))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs)))));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     putIReg(rd, binop(Iop_16HLto32,
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU16(0xffff),
                                                  unop(Iop_32to16,
                                                       mkexpr(t2))),
                                       IRExpr_ITE(mkexpr(t1),
                                                  mkU16(0xffff),
                                                  unop(Iop_32to16,
                                                       mkexpr(t0)))));
                     break;
                  }
                  case 0xD: {  /* SUBU_S.PH */
                     DIP("subu_s.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);

                     /* Subtract lower halves. */
                     assign(t0, binop(Iop_Sub32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     /* Detect underflow. */
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            mkexpr(t0), mkU32(0x00010000)),
                                      mkU32(0x0)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     /* Subtract higher halves. */
                     assign(t2, binop(Iop_Sub32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));

                     /* Detect underflow. */
                     assign(t3, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            mkexpr(t2), mkU32(0x00010000)),
                                      mkU32(0x0)));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   IRExpr_ITE(mkexpr(t3),
                                              mkU16(0x0000),
                                              unop(Iop_32to16, mkexpr(t2))),
                                   IRExpr_ITE(mkexpr(t1),
                                              mkU16(0x0000),
                                              unop(Iop_32to16, mkexpr(t0)))));
                     break;
                  }
                  case 0xE: {  /* ADDQ_S.PH */
                     DIP("addq_s.ph r%d r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I16);
                     t5 = newTemp(Ity_I16);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);

                     /* Add lower halves. */
                     assign(t0, binop(Iop_Add32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     /* Bit 16 of the result. */
                     assign(t6, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, mkexpr(t0))),
                                      mkU32(0x1)));
                     /* Detect overflow. */
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t0),
                                                  mkU32(0x8000)),
                                            mkU8(15)),
                                      mkexpr(t6)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     /* Saturate if needed. */
                     assign(t4, IRExpr_ITE(mkexpr(t1),
                                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                                            mkexpr(t6),
                                                            mkU32(0x0)),
                                                      mkU16(0x7fff),
                                                      mkU16(0x8000)),
                                           unop(Iop_32to16, mkexpr(t0))));

                     /* Add higher halves. */
                     assign(t2, binop(Iop_Add32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));

                     /* Bit 16 of the result. */
                     assign(t7, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, mkexpr(t2))),
                                      mkU32(0x1)));
                     /* Detect overflow. */
                     assign(t3, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t2),
                                                  mkU32(0x00008000)),
                                            mkU8(15)),
                                      mkexpr(t7)));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     /* Saturate if needed. */
                     assign(t5, IRExpr_ITE(mkexpr(t3),
                                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                                            mkexpr(t7),
                                                            mkU32(0x0)),
                                                      mkU16(0x7fff),
                                                      mkU16(0x8000)),
                                           unop(Iop_32to16, mkexpr(t2))));

                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t5), mkexpr(t4)));
                     break;
                  }
                  case 0xF: {  /* SUBQ_S.PH */
                     DIP("subq_s.ph r%d r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I16);
                     t5 = newTemp(Ity_I16);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);

                     /* Subtract lower halves. */
                     assign(t0, binop(Iop_Sub32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     /* Bit 16 of the result. */
                     assign(t6, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, mkexpr(t0))),
                                      mkU32(0x1)));
                     /* Detect overflow or underflow. */
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t0),
                                                  mkU32(0x8000)),
                                            mkU8(15)),
                                      mkexpr(t6)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     /* Saturate if needed. */
                     assign(t4, IRExpr_ITE(mkexpr(t1),
                                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                                            mkexpr(t6),
                                                            mkU32(0x0)),
                                                      mkU16(0x7fff),
                                                      mkU16(0x8000)),
                                           unop(Iop_32to16, mkexpr(t0))));

                     /* Subtract higher halves. */
                     assign(t2, binop(Iop_Sub32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));

                     /* Bit 16 of the result. */
                     assign(t7, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, mkexpr(t2))),
                                      mkU32(0x1)));
                     /* Detect overflow or underflow. */
                     assign(t3, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  mkexpr(t2),
                                                  mkU32(0x00008000)),
                                            mkU8(15)),
                                      mkexpr(t7)));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     /* Saturate if needed. */
                     assign(t5, IRExpr_ITE(mkexpr(t3),
                                           IRExpr_ITE(binop(Iop_CmpEQ32,
                                                            mkexpr(t7),
                                                            mkU32(0x0)),
                                                      mkU16(0x7fff),
                                                      mkU16(0x8000)),
                                           unop(Iop_32to16, mkexpr(t2))));

                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t5), mkexpr(t4)));
                     break;
                  }
                  case 0x10: {  /* ADDSC */
                     DIP("addsc r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I1);

                     /* The carry bit result out of the addition operation is
                        written to bit 13(the c field) of the DSPControl reg. */
                     assign(t0, binop(Iop_Add64,
                                      unop(Iop_32Uto64, getIReg(rs)),
                                      unop(Iop_32Uto64, getIReg(rt))));

                     assign(t1, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32, mkexpr(t0)),
                                            mkU32(0x1)),
                                      mkU32(0x1)));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x2000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xffffdfff))));

                     putIReg(rd, unop(Iop_64to32, mkexpr(t0)));
                     break;
                  }
                  case 0x11: {  /* ADDWC */
                     DIP("addwc r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I1);

                     /* Get carry bit from DSPControl register. */
                     assign(t0, binop(Iop_Shr32,
                                       binop(Iop_And32,
                                             getDSPControl(),
                                             mkU32(0x2000)),
                                       mkU8(0xd)));
                     assign(t1, binop(Iop_Add64,
                                      unop(Iop_32Sto64, getIReg(rs)),
                                      unop(Iop_32Sto64,
                                           binop(Iop_Add32,
                                                 getIReg(rt),
                                                 mkexpr(t0)))));

                     /* Extract bits 32 and 31. */
                     assign(t2, binop(Iop_And32,
                                      unop(Iop_64HIto32, mkexpr(t1)),
                                      mkU32(0x1)));
                     assign(t3, binop(Iop_Shr32,
                                      binop(Iop_And32,
                                            unop(Iop_64to32, mkexpr(t1)),
                                            mkU32(0x80000000)),
                                      mkU8(31)));
                     assign(t4, binop(Iop_CmpNE32, mkexpr(t2), mkexpr(t3)));

                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));
                     putIReg(rd, unop(Iop_64to32, mkexpr(t1)));
                     break;
                  }
                  case 0x12: {  /* MODSUB */
                     DIP("modsub r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);

                     /* decr_7..0 */
                     assign(t0,
                            unop(Iop_8Uto32,
                                 unop(Iop_16to8,
                                      unop(Iop_32to16, getIReg(rt)))));

                     /* lastindex_15..0 */
                     assign(t1,
                            unop(Iop_16Uto32,
                                 binop(Iop_8HLto16,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rt))),
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     /* temp_15..0 */
                     assign(t2,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             getIReg(rs),
                                             mkU32(0x00000000)),
                                       mkexpr(t1),
                                       binop(Iop_Sub32,
                                             getIReg(rs), mkexpr(t0))));
                     putIReg(rd, mkexpr(t2));
                     break;
                  }
                  case 0x14: {  /* RADDU.W.QB */
                     DIP("raddu.w.qb r%d, r%d", rd, rs);
                     vassert(!mode64);
                     putIReg(rd, binop(Iop_Add32,
                                       binop(Iop_Add32,
                                             unop(Iop_8Uto32,
                                                  unop(Iop_16to8,
                                                       unop(Iop_32to16,
                                                            getIReg(rs)))),
                                             unop(Iop_8Uto32,
                                                  unop(Iop_16HIto8,
                                                       unop(Iop_32to16,
                                                            getIReg(rs))))),
                                       binop(Iop_Add32,
                                             unop(Iop_8Uto32,
                                                  unop(Iop_16to8,
                                                       unop(Iop_32HIto16,
                                                            getIReg(rs)))),
                                             unop(Iop_8Uto32,
                                                  unop(Iop_16HIto8,
                                                       unop(Iop_32HIto16,
                                                            getIReg(rs)))))));
                     break;
                  }
                  case 0x16: {  /* ADDQ_S.W */
                     DIP("addq_s.w r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_Add64,
                                      unop(Iop_32Sto64, getIReg(rs)),
                                      unop(Iop_32Sto64, getIReg(rt))));

                     assign(t3, binop(Iop_And32,
                                      unop(Iop_64HIto32, mkexpr(t0)),
                                      mkU32(0x1)));
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  unop(Iop_64to32, mkexpr(t0)),
                                                  mkU32(0x80000000)),
                                            mkU8(31)),
                                      mkexpr(t3)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                                             mkexpr(t3),
                                                             mkU32(0x0)),
                                                       mkU32(0x7fffffff),
                                                       mkU32(0x80000000)),
                                            unop(Iop_64to32, mkexpr(t0))));
                     break;
                  }
                  case 0x17: {  /* SUBQ_S.W */
                     DIP("subq_s.w r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_Sub64,
                                      unop(Iop_32Sto64, getIReg(rs)),
                                      unop(Iop_32Sto64, getIReg(rt))));

                     assign(t3, binop(Iop_And32,
                                      unop(Iop_64HIto32, mkexpr(t0)),
                                      mkU32(0x1)));
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  unop(Iop_64to32, mkexpr(t0)),
                                                  mkU32(0x80000000)),
                                            mkU8(31)),
                                      mkexpr(t3)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00100000)),
                                              getDSPControl()));

                     putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                                             mkexpr(t3),
                                                             mkU32(0x0)),
                                                       mkU32(0x7fffffff),
                                                       mkU32(0x80000000)),
                                            unop(Iop_64to32, mkexpr(t0))));
                     break;
                  }
                  case 0x1C: {  /* MULEQ_S.W.PHL */
                     DIP("muleq_s.w.phl r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I32);

                     assign(t0,
                            binop(Iop_Shl32,
                                  binop(Iop_Mul32,
                                        unop(Iop_16Sto32,
                                             unop(Iop_32HIto16, getIReg(rt))),
                                        unop(Iop_16Sto32,
                                             unop(Iop_32HIto16, getIReg(rs)))),
                                  mkU8(0x1)));
                     assign(t1, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0xffff0000)),
                                      mkU32(0x80000000)));
                     assign(t2, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0xffff0000)),
                                      mkU32(0x80000000)));
                     assign(t3, IRExpr_ITE(mkexpr(t1),
                                           IRExpr_ITE(mkexpr(t2),
                                                      binop(Iop_Or32,
                                                            getDSPControl(),
                                                            mkU32(0x00200000)),
                                                      getDSPControl()),
                                           getDSPControl()));
                     putDSPControl(mkexpr(t3));

                     putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                            IRExpr_ITE(mkexpr(t2),
                                                       mkU32(0x7fffffff),
                                                       mkexpr(t0)),
                                            mkexpr(t0)));
                     break;
                  }
                  case 0x1D: {  /* MULEQ_S.W.PHR */
                     DIP("muleq_s.w.phr r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);

                     assign(t0,
                            binop(Iop_Shl32,
                                  binop(Iop_Mul32,
                                        unop(Iop_16Sto32,
                                             unop(Iop_32to16, getIReg(rt))),
                                        unop(Iop_16Sto32,
                                             unop(Iop_32to16, getIReg(rs)))),
                                  mkU8(0x1)));
                     assign(t1, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0xffff)),
                                      mkU32(0x8000)));
                     assign(t2, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0xffff)),
                                      mkU32(0x8000)));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              IRExpr_ITE(mkexpr(t2),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00200000)
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));
                     putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                            IRExpr_ITE(mkexpr(t2),
                                                       mkU32(0x7fffffff),
                                                       mkexpr(t0)),
                                            mkexpr(t0)));
                     break;
                  }
                  case 0x1E: {  /* MULQ_S.PH */
                     DIP("mulq_s.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I16);
                     t3 = newTemp(Ity_I16);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t5,
                            unop(Iop_16Sto32, unop(Iop_32to16, getIReg(rs))));
                     assign(t6,
                            unop(Iop_16Sto32, unop(Iop_32to16, getIReg(rt))));

                     assign(t7,
                            unop(Iop_16Sto32, unop(Iop_32HIto16, getIReg(rs))));
                     assign(t8,
                            unop(Iop_16Sto32, unop(Iop_32HIto16, getIReg(rt))));

                     assign(t0, binop(Iop_And32,
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       mkexpr(t5),
                                                       mkU32(0xffff)),
                                                 mkU32(0x8000))),
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       mkexpr(t6),
                                                       mkU32(0xffff)),
                                                 mkU32(0x8000)))));
                     assign(t1, binop(Iop_And32,
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       mkexpr(t7),
                                                       mkU32(0xffff)),
                                                 mkU32(0x8000))),
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       mkexpr(t8),
                                                       mkU32(0xffff)),
                                                 mkU32(0x8000)))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    binop(Iop_Or32,
                                                          mkexpr(t0),
                                                          mkexpr(t1)),
                                                    mkU32(0x0)),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x200000))));

                     assign(t2, unop(Iop_32HIto16,
                                     binop(Iop_Shl32,
                                           unop(Iop_64to32,
                                                binop(Iop_MullS32,
                                                      mkexpr(t7),
                                                      mkexpr(t8))),
                                           mkU8(0x1))));
                     assign(t3, unop(Iop_32HIto16,
                                     binop(Iop_Shl32,
                                           unop(Iop_64to32,
                                                binop(Iop_MullS32,
                                                      mkexpr(t5),
                                                      mkexpr(t6))),
                                           mkU8(0x1))));
                     putIReg(rd, binop(Iop_16HLto32,
                                       IRExpr_ITE(binop(Iop_CmpEQ32,
                                                        mkexpr(t1),
                                                        mkU32(0x0)),
                                                  mkexpr(t2),
                                                  mkU16(0x7fff)),
                                       IRExpr_ITE(binop(Iop_CmpEQ32,
                                                        mkexpr(t0),
                                                        mkU32(0x0)),
                                                  mkexpr(t3),
                                                  mkU16(0x7fff))));
                     break;
                  }
                  case 0x1F: {  /* MULQ_RS.PH */
                     DIP("mulq_rs.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I16);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I16);

                     /* Multiply and round lower halfwords. */
                     assign(t0, binop(Iop_Add32,
                                      binop(Iop_Shl32,
                                            binop(Iop_Mul32,
                                                  unop(Iop_16Sto32,
                                                       unop(Iop_32to16,
                                                            getIReg(rt))),
                                                  unop(Iop_16Sto32,
                                                       unop(Iop_32to16,
                                                            getIReg(rs)))),
                                            mkU8(0x1)),
                                      mkU32(0x00008000)));
                     assign(t1, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            getIReg(rt), mkU32(0xffff)),
                                      mkU32(0x8000)));
                     assign(t2, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            getIReg(rs), mkU32(0xffff)),
                                      mkU32(0x8000)));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              IRExpr_ITE(mkexpr(t2),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00200000)
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));
                     assign(t3, IRExpr_ITE(mkexpr(t1),
                                           IRExpr_ITE(mkexpr(t2),
                                                      mkU16(0x7fff),
                                                      unop(Iop_32HIto16,
                                                           mkexpr(t0))),
                                           unop(Iop_32HIto16, mkexpr(t0))));

                     /* Multiply and round higher halfwords. */
                     assign(t4, binop(Iop_Add32,
                                      binop(Iop_Shl32,
                                            binop(Iop_Mul32,
                                                  unop(Iop_16Sto32,
                                                       unop(Iop_32HIto16,
                                                            getIReg(rt))),
                                                  unop(Iop_16Sto32,
                                                       unop(Iop_32HIto16,
                                                            getIReg(rs)))),
                                            mkU8(0x1)),
                                      mkU32(0x00008000)));
                     assign(t5, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0xffff0000)),
                                      mkU32(0x80000000)));
                     assign(t6, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0xffff0000)),
                                      mkU32(0x80000000)));
                     putDSPControl(IRExpr_ITE(mkexpr(t5),
                                             IRExpr_ITE(mkexpr(t6),
                                                        binop(Iop_Or32,
                                                             getDSPControl(),
                                                             mkU32(0x00200000)),
                                                        getDSPControl()),
                                             getDSPControl()));
                     assign(t7, IRExpr_ITE(mkexpr(t5),
                                           IRExpr_ITE(mkexpr(t6),
                                                      mkU16(0x7fff),
                                                      unop(Iop_32HIto16,
                                                           mkexpr(t4))),
                                           unop(Iop_32HIto16, mkexpr(t4))));

                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t7), mkexpr(t3)));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of ADDU.QB */
            }
            case 0x11: {  /* CMPU.EQ.QB */
               switch(sa) {
                  case 0x0: {  /* CMPU.EQ.QB */
                     DIP("cmpu.eq.qb r%d, r%d", rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);

                     assign(t1,
                            binop(Iop_CmpEQ32,
                                  binop(Iop_And32, getIReg(rs), mkU32(0xff)),
                                  binop(Iop_And32, getIReg(rt), mkU32(0xff))));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));

                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));

                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x04000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfbffffff))));

                     assign(t4, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x08000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xf7ffffff))));
                     break;
                  }
                  case 0x1: {  /* CMPU.LT.QB */
                     DIP("cmpu.lt.qb r%d, r%d", rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);

                     assign(t1, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));

                     assign(t2, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));

                     assign(t3, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x04000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfbffffff))));

                     assign(t4, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x08000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xf7ffffff))));
                     break;
                  }
                  case 0x2: {  /* CMPU.LE.QB */
                     DIP("cmpu.le.qb r%d, r%d", rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);

                     assign(t1, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));

                     assign(t2, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));

                     assign(t3, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x04000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfbffffff))));

                     assign(t4, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x08000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xf7ffffff))));
                     break;
                  }
                  case 0x3: {  /* PICK.QB */
                     DIP("pick.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I8);
                     t2 = newTemp(Ity_I8);
                     t3 = newTemp(Ity_I8);
                     t4 = newTemp(Ity_I8);

                     assign(t0, getDSPControl());
                     assign(t1, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       mkexpr(t0),
                                                       mkU32(0x01000000)),
                                                 mkU32(0x0)),
                                           unop(Iop_16to8,
                                                 unop(Iop_32to16,
                                                      getIReg(rs))),
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rt)))));
                     assign(t2, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       mkexpr(t0),
                                                       mkU32(0x02000000)),
                                                 mkU32(0x0)),
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16, getIReg(rs))),
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt)))));
                     assign(t3, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       mkexpr(t0),
                                                       mkU32(0x04000000)),
                                                 mkU32(0x0)),
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs))),
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt)))));
                     assign(t4, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       mkexpr(t0),
                                                       mkU32(0x08000000)),
                                                 mkU32(0x0)),
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs))),
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt)))));
                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   binop(Iop_8HLto16, mkexpr(t4), mkexpr(t3)),
                                   binop(Iop_8HLto16, mkexpr(t2), mkexpr(t1))));
                     break;
                  }
                  case 0x4: {  /* CMPGU.EQ.QB */
                     DIP("cmpgu.eq.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t1, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t5, IRExpr_ITE(mkexpr(t1),
                                           mkU32(0x00000001), mkU32(0)));

                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t6, IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x00000002), mkU32(0)));

                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           mkU32(0x00000004), mkU32(0)));

                     assign(t4, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t8, IRExpr_ITE(mkexpr(t4),
                                           mkU32(0x00000008), mkU32(0)));

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Or32,
                                             binop(Iop_Or32,
                                                   mkexpr(t5), mkexpr(t6)),
                                             mkexpr(t7)),
                                       mkexpr(t8)));
                     break;
                  }
                  case 0x5: {  /* CMPGU.LT.QB */
                     DIP("cmpgu.lt.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t1, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t5, IRExpr_ITE(mkexpr(t1),
                                           mkU32(0x00000001), mkU32(0)));

                     assign(t2, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t6, IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x00000002), mkU32(0)));

                     assign(t3, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           mkU32(0x00000004), mkU32(0)));

                     assign(t4, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t8, IRExpr_ITE(mkexpr(t4),
                                           mkU32(0x00000008), mkU32(0)));
                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Or32,
                                             binop(Iop_Or32,
                                                   mkexpr(t5), mkexpr(t6)),
                                             mkexpr(t7)),
                                       mkexpr(t8)));
                     break;
                  }
                  case 0x6: {  /* CMPGU.LE.QB */
                     DIP("cmpgu.le.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t1, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t5, IRExpr_ITE(mkexpr(t1),
                                           mkU32(0x00000001), mkU32(0)));

                     assign(t2, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t6, IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x00000002), mkU32(0)));

                     assign(t3, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           mkU32(0x00000004), mkU32(0)));

                     assign(t4, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t8, IRExpr_ITE(mkexpr(t4),
                                           mkU32(0x00000008), mkU32(0)));
                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Or32,
                                             binop(Iop_Or32,
                                                   mkexpr(t5), mkexpr(t6)),
                                             mkexpr(t7)),
                                       mkexpr(t8)));
                     break;
                  }
                  case 0x8: {  /* CMP.EQ.PH */
                     DIP("cmp.eq.ph r%d, r%d", rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);

                     assign(t1, binop(Iop_CmpEQ16,
                                      unop(Iop_32to16, getIReg(rs)),
                                      unop(Iop_32to16, getIReg(rt))));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));
                     assign(t2, binop(Iop_CmpEQ16,
                                      unop(Iop_32HIto16, getIReg(rs)),
                                      unop(Iop_32HIto16, getIReg(rt))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));
                     break;
                  }
                  case 0x9: {  /* CMP.LT.PH */
                     DIP("cmp.lt.ph r%d, r%d", rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);

                     assign(t1, binop(Iop_CmpLT32S,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));

                     assign(t2, binop(Iop_CmpLT32S,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));
                     break;
                  }
                  case 0xA: {  /* CMP.LE.PH */
                     DIP("cmp.le.ph r%d, r%d", rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);

                     assign(t1, binop(Iop_CmpLE32S,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));

                     assign(t2, binop(Iop_CmpLE32S,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));
                     break;
                  }
                  case 0xB: {  /* PICK.PH */
                     DIP("pick.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I16);
                     t2 = newTemp(Ity_I16);

                     assign(t0, getDSPControl());

                     assign(t1, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       mkexpr(t0),
                                                       mkU32(0x01000000)),
                                                 mkU32(0x0)),
                                           unop(Iop_32to16, getIReg(rs)),
                                           unop(Iop_32to16, getIReg(rt))));

                     assign(t2, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       mkexpr(t0),
                                                       mkU32(0x02000000)),
                                                 mkU32(0x0)),
                                           unop(Iop_32HIto16, getIReg(rs)),
                                           unop(Iop_32HIto16, getIReg(rt))));

                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t2), mkexpr(t1)));
                     break;
                  }
                  case 0xC: {  /* PRECRQ.QB.PH */
                     DIP("precrq.qb.ph r%d, r%d, %d", rd, rs, rt);
                     vassert(!mode64);
                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   binop(Iop_8HLto16,
                                         unop(Iop_16HIto8,
                                              unop(Iop_32HIto16, getIReg(rs))),
                                         unop(Iop_16HIto8,
                                              unop(Iop_32to16, getIReg(rs)))),
                                   binop(Iop_8HLto16,
                                         unop(Iop_16HIto8,
                                              unop(Iop_32HIto16, getIReg(rt))),
                                         unop(Iop_16HIto8,
                                              unop(Iop_32to16, getIReg(rt))))));
                     break;
                  }
                  case 0xD: {  /* PRECR.QB.PH */
                     DIP("precr.qb.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);

                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   binop(Iop_8HLto16,
                                         unop(Iop_16to8,
                                              unop(Iop_32HIto16, getIReg(rs))),
                                         unop(Iop_16to8,
                                              unop(Iop_32to16, getIReg(rs)))),
                                   binop(Iop_8HLto16,
                                         unop(Iop_16to8,
                                              unop(Iop_32HIto16, getIReg(rt))),
                                         unop(Iop_16to8,
                                              unop(Iop_32to16, getIReg(rt))))));
                     break;
                  }
                  case 0xF: {  /* PRECRQU_S.QB.PH */
                     DIP("precrqu_s.qb.ph r%d, r%d, %d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I8);
                     t1 = newTemp(Ity_I8);
                     t2 = newTemp(Ity_I8);
                     t3 = newTemp(Ity_I8);
                     t4 = newTemp(Ity_I8);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I8);
                     t8 = newTemp(Ity_I1);
                     t9 = newTemp(Ity_I32);
                     t10 = newTemp(Ity_I8);
                     t11 = newTemp(Ity_I1);
                     t12 = newTemp(Ity_I32);
                     t13 = newTemp(Ity_I8);
                     t14 = newTemp(Ity_I1);
                     t15 = newTemp(Ity_I32);

                     assign(t4, IRExpr_ITE(binop(Iop_CmpLT32U,
                                                 mkU32(0x7f80),
                                                 binop(Iop_And32,
                                                       unop(Iop_16Uto32,
                                                            unop(Iop_32to16,
                                                            getIReg(rs))),
                                                       mkU32(0x7fff))),
                                           mkU8(0xff),
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     binop(Iop_Shl32,
                                                           getIReg(rs),
                                                           mkU8(1))))));
                     assign(t0, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       unop(Iop_16Uto32,
                                                            unop(Iop_32to16,
                                                                 getIReg(rs))),
                                                       mkU32(0x00008000)),
                                                 mkU32(0x0)),
                                           mkexpr(t4),
                                           mkU8(0x0)));
                     assign(t5, binop(Iop_And32,
                                      unop(Iop_16Uto32,
                                            unop(Iop_32to16,
                                                 getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t6, binop(Iop_CmpLT32U,
                                      mkU32(0x7f80),
                                      binop(Iop_And32,
                                            unop(Iop_16Uto32,
                                                 unop(Iop_32to16,
                                                 getIReg(rs))),
                                            mkU32(0x7fff))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    mkexpr(t5),
                                                    mkU32(0x0)),
                                              IRExpr_ITE(mkexpr(t6),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00400000)
                                                              ),
                                                         getDSPControl()),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00400000))));

                     assign(t7, IRExpr_ITE(binop(Iop_CmpLT32U,
                                                 mkU32(0x7f80),
                                                 binop(Iop_And32,
                                                       unop(Iop_16Uto32,
                                                            unop(Iop_32HIto16,
                                                                 getIReg(rs))),
                                                       mkU32(0x7fff))),
                                           mkU8(0xff),
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     binop(Iop_Shl32,
                                                           getIReg(rs),
                                                           mkU8(1))))));
                     assign(t1, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       unop(Iop_16Uto32,
                                                            unop(Iop_32HIto16,
                                                                 getIReg(rs))),
                                                       mkU32(0x00008000)),
                                                 mkU32(0x0)),
                                           mkexpr(t7),
                                           mkU8(0x0)));
                     assign(t8, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            unop(Iop_16Uto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rs))),
                                            mkU32(0x00008000)),
                                      mkU32(0x0)));
                     assign(t9, IRExpr_ITE(binop(Iop_CmpLT32U,
                                                 mkU32(0x7f80),
                                                 binop(Iop_And32,
                                                       unop(Iop_16Uto32,
                                                            unop(Iop_32HIto16,
                                                                 getIReg(rs))),
                                                       mkU32(0x7fff))),
                                           binop(Iop_Or32,
                                                 getDSPControl(),
                                                 mkU32(0x00400000)),
                                           getDSPControl()));
                     putDSPControl(IRExpr_ITE(mkexpr(t8),
                                              mkexpr(t9),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00400000))));

                     assign(t10, IRExpr_ITE(binop(Iop_CmpLT32U,
                                                  mkU32(0x7f80),
                                                  binop(Iop_And32,
                                                        unop(Iop_16Uto32,
                                                             unop(Iop_32to16,
                                                             getIReg(rt))),
                                                        mkU32(0x7fff))),
                                            mkU8(0xff),
                                            unop(Iop_16HIto8,
                                                 unop(Iop_32to16,
                                                      binop(Iop_Shl32,
                                                            getIReg(rt),
                                                            mkU8(1))))));
                     assign(t2, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       unop(Iop_16Uto32,
                                                            unop(Iop_32to16,
                                                                 getIReg(rt))),
                                                       mkU32(0x00008000)),
                                                 mkU32(0x0)),
                                           mkexpr(t10),
                                           mkU8(0x0)));
                     assign(t11, binop(Iop_CmpEQ32,
                                       binop(Iop_And32,
                                             unop(Iop_16Uto32,
                                                  unop(Iop_32to16,
                                                       getIReg(rt))),
                                             mkU32(0x00008000)),
                                       mkU32(0x0)));
                     assign(t12, IRExpr_ITE(binop(Iop_CmpLT32U,
                                                  mkU32(0x7f80),
                                                  binop(Iop_And32,
                                                        unop(Iop_16Uto32,
                                                             unop(Iop_32to16,
                                                             getIReg(rt))),
                                                        mkU32(0x7fff))),
                                            binop(Iop_Or32,
                                                  getDSPControl(),
                                                  mkU32(0x00400000)),
                                            getDSPControl()));
                     putDSPControl(IRExpr_ITE(mkexpr(t11),
                                              mkexpr(t12),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00400000))));

                     assign(t13, IRExpr_ITE(binop(Iop_CmpLT32U,
                                                  mkU32(0x7f80),
                                                  binop(Iop_And32,
                                                        unop(Iop_16Uto32,
                                                             unop(Iop_32HIto16,
                                                                  getIReg(rt))),
                                                        mkU32(0x7fff))),
                                            mkU8(0xff),
                                            unop(Iop_16HIto8,
                                                 unop(Iop_32HIto16,
                                                      binop(Iop_Shl32,
                                                            getIReg(rt),
                                                            mkU8(1))))));
                     assign(t3, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       unop(Iop_16Uto32,
                                                            unop(Iop_32HIto16,
                                                                 getIReg(rt))),
                                                       mkU32(0x00008000)),
                                                 mkU32(0x0)),
                                           mkexpr(t13),
                                           mkU8(0x0)));
                     assign(t14, binop(Iop_CmpEQ32,
                                       binop(Iop_And32,
                                             unop(Iop_16Uto32,
                                                  unop(Iop_32HIto16,
                                                       getIReg(rt))),
                                             mkU32(0x00008000)),
                                       mkU32(0x0)));
                     assign(t15, IRExpr_ITE(binop(Iop_CmpLT32U,
                                                  mkU32(0x7f80),
                                                  binop(Iop_And32,
                                                        unop(Iop_16Uto32,
                                                             unop(Iop_32HIto16,
                                                                  getIReg(rt))),
                                                        mkU32(0x7fff))),
                                            binop(Iop_Or32,
                                                  getDSPControl(),
                                                  mkU32(0x00400000)),
                                            getDSPControl()));
                     putDSPControl(IRExpr_ITE(mkexpr(t14),
                                              mkexpr(t15),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00400000))));

                     putIReg(rd, binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             mkexpr(t1), mkexpr(t0)),
                                       binop(Iop_8HLto16,
                                             mkexpr(t3), mkexpr(t2))));
                     break;
                  }
                  case 0x14: {  /* PRECRQ.PH.W */
                     DIP("precrq.ph.w r%d, r%d, %d", rd, rs, rt);
                     vassert(!mode64);
                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32HIto16, getIReg(rs)),
                                       unop(Iop_32HIto16, getIReg(rt))));
                     break;
                  }
                  case 0x15: {  /* PRECRQ_RS.PH.W */
                     DIP("precrq_rs.ph.w r%d, r%d, %d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I64);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_Add64,
                                      binop(Iop_32HLto64,
                                            binop(Iop_Shr32,
                                                  binop(Iop_And32,
                                                        getIReg(rs),
                                                        mkU32(0x80000000)),
                                                  mkU8(31)),
                                            getIReg(rs)),
                                      mkU64(0x0000000000008000ULL)));
                     assign(t1, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32, mkexpr(t0)),
                                            mkU32(0x1)),
                                      binop(Iop_And32,
                                            binop(Iop_Shr32,
                                                  unop(Iop_64to32, mkexpr(t0)),
                                                  mkU8(31)),
                                            mkU32(0x1))));
                     assign(t2, IRExpr_ITE(mkexpr(t1),
                                           mkU32(0x7fffffff),
                                           unop(Iop_64to32, mkexpr(t0))));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x400000)),
                                              getDSPControl()));
                     assign(t3, binop(Iop_Add64,
                                      binop(Iop_32HLto64,
                                            binop(Iop_Shr32,
                                                  binop(Iop_And32,
                                                        getIReg(rt),
                                                        mkU32(0x80000000)),
                                                  mkU8(31)),
                                            getIReg(rt)),
                                      mkU64(0x0000000000008000ULL)));
                     assign(t4, binop(Iop_CmpNE32,
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32, mkexpr(t3)),
                                            mkU32(0x1)),
                                      binop(Iop_And32,
                                            binop(Iop_Shr32,
                                                  unop(Iop_64to32, mkexpr(t3)),
                                                  mkU8(31)),
                                            mkU32(0x1))));
                     assign(t5, IRExpr_ITE(mkexpr(t4),
                                           mkU32(0x7fffffff),
                                           unop(Iop_64to32, mkexpr(t3))));
                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x400000)),
                                              getDSPControl()));
                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32HIto16, mkexpr(t2)),
                                       unop(Iop_32HIto16, mkexpr(t5))));
                     break;
                  }
                  case 0x1E: {  /* PRECR_SRA.PH.W */
                     DIP("precr_sra.ph.w r%d, r%d, %d", rt, rs, rd);
                     vassert(!mode64);

                     if (0 == rd) {
                        putIReg(rt, binop(Iop_16HLto32,
                                          unop(Iop_32to16, getIReg(rt)),
                                          unop(Iop_32to16, getIReg(rs))));
                     } else {
                        putIReg(rt, binop(Iop_16HLto32,
                                          unop(Iop_32to16, binop(Iop_Sar32,
                                                                 getIReg(rt),
                                                                 mkU8(rd))),
                                          unop(Iop_32to16, binop(Iop_Sar32,
                                                                 getIReg(rs),
                                                                 mkU8(rd)))));
                     }
                     break;
                  }
                  case 0x1F: {  /* PRECR_SRA_R.PH.W */
                     DIP("precr_sra_r.ph.w r%d, r%d, %d", rt, rs, rd);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);

                     if (0 == rd) {
                        putIReg(rt, binop(Iop_16HLto32,
                                          unop(Iop_32to16, getIReg(rt)),
                                          unop(Iop_32to16, getIReg(rs))));
                     } else {
                        assign(t0, binop(Iop_Shr32,
                                         binop(Iop_Add32,
                                               binop(Iop_Sar32,
                                                     getIReg(rt),
                                                     mkU8(rd-1)),
                                               mkU32(0x1)),
                                         mkU8(0x1)));
                        assign(t1, binop(Iop_Shr32,
                                         binop(Iop_Add32,
                                               binop(Iop_Sar32,
                                                     getIReg(rs),
                                                     mkU8(rd-1)),
                                               mkU32(0x1)),
                                         mkU8(0x1)));
                        putIReg(rt, binop(Iop_16HLto32,
                                          unop(Iop_32to16, mkexpr(t0)),
                                          unop(Iop_32to16, mkexpr(t1))));
                     };
                     break;
                  }
                  case 0xE: {  /* PACKRL.PH */
                     DIP("packrl.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, getIReg(rs)),
                                       unop(Iop_32HIto16, getIReg(rt))));
                     break;
                  }
                  case 0x18: {  /* CMPGDU.EQ.QB */
                     DIP("cmpgdu.eq.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t1,
                            binop(Iop_CmpEQ32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t5, IRExpr_ITE(mkexpr(t1),
                                           mkU32(0x00000001), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));

                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t6, IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x00000002), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));

                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           mkU32(0x00000004), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x04000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfbffffff))));

                     assign(t4, binop(Iop_CmpEQ32,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t8, IRExpr_ITE(mkexpr(t4),
                                           mkU32(0x00000008), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x08000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xf7ffffff))));

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Or32,
                                             binop(Iop_Or32,
                                                   mkexpr(t5), mkexpr(t6)),
                                             mkexpr(t7)),
                                       mkexpr(t8)));
                     break;
                  }
                  case 0x19: {  /* CMPGDU.LT.QB */
                     DIP("cmpgdu.lt.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t1, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t5, IRExpr_ITE(mkexpr(t1),
                                           mkU32(0x00000001), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));

                     assign(t2, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t6, IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x00000002), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));

                     assign(t3, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           mkU32(0x00000004), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x04000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfbffffff))));

                     assign(t4, binop(Iop_CmpLT32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t8, IRExpr_ITE(mkexpr(t4),
                                           mkU32(0x00000008), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x08000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xf7ffffff))));

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Or32,
                                             binop(Iop_Or32,
                                                   mkexpr(t5), mkexpr(t6)),
                                             mkexpr(t7)),
                                       mkexpr(t8)));
                     break;
                  }
                  case 0x1A: {  /* CMPGDU.LE.QB */
                     DIP("cmpgdu.le.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     assign(t1, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t5, IRExpr_ITE(mkexpr(t1),
                                           mkU32(0x00000001),
                                           mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x01000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfeffffff))));

                     assign(t2, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16, getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32to16,
                                                     getIReg(rt))))));
                     assign(t6, IRExpr_ITE(mkexpr(t2),
                                           mkU32(0x00000002), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x02000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfdffffff))));

                     assign(t3, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16to8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t7, IRExpr_ITE(mkexpr(t3),
                                           mkU32(0x00000004), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x04000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xfbffffff))));

                     assign(t4, binop(Iop_CmpLE32U,
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rs)))),
                                      unop(Iop_8Uto32,
                                           unop(Iop_16HIto8,
                                                unop(Iop_32HIto16,
                                                     getIReg(rt))))));
                     assign(t8, IRExpr_ITE(mkexpr(t4),
                                           mkU32(0x00000008), mkU32(0)));
                     putDSPControl(IRExpr_ITE(mkexpr(t4),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x08000000)),
                                              binop(Iop_And32,
                                                    getDSPControl(),
                                                    mkU32(0xf7ffffff))));

                     putIReg(rd, binop(Iop_Or32,
                                       binop(Iop_Or32,
                                             binop(Iop_Or32,
                                                   mkexpr(t5), mkexpr(t6)),
                                             mkexpr(t7)),
                                       mkexpr(t8)));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of CMPU.EQ.QB */
            }
            case 0x13: {  /* SHLL.QB */
               switch(sa) {
                  case 0x0: {  /* SHLL.QB */
                     DIP("shll.qb r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I1);
                     t9 = newTemp(Ity_I1);
                     t10 = newTemp(Ity_I1);

                     if (0 == rs) {
                        putIReg(rd, getIReg(rt));
                     } else {
                        /* Shift bits 7..0 and 23..16. */
                        assign(t0, binop(Iop_Shl32,
                                         binop(Iop_And32,
                                               getIReg(rt),
                                               mkU32(0x00ff00ff)),
                                         mkU8(rs)));
                        assign(t1, binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              mkexpr(t0),
                                              mkU32(0xff000000)),
                                        mkU32(0x00000000)));
                        assign(t2, binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              mkexpr(t0),
                                              mkU32(0xff000000)),
                                        mkU32(0xff000000)));
                        assign(t7, binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              mkexpr(t0),
                                              mkU32(0x0000ff00)),
                                        mkU32(0x00000000)));
                        assign(t8, binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              mkexpr(t0),
                                              mkU32(0x0000ff00)),
                                        mkU32(0x000ff00)));
                        /* Shift bits 15..8 and 31..24. */
                        assign(t3, binop(Iop_Shl32,
                                         binop(Iop_Shr32,
                                               binop(Iop_And32,
                                                     getIReg(rt),
                                                     mkU32(0xff00ff00)),
                                               mkU8(8)),
                                         mkU8(rs)));
                        assign(t4, binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              mkexpr(t3),
                                              mkU32(0xff000000)),
                                        mkU32(0x00000000)));
                        assign(t5, binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              mkexpr(t3),
                                              mkU32(0xff000000)),
                                        mkU32(0xff000000)));
                        assign(t9, binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              mkexpr(t3),
                                              mkU32(0x0000ff00)),
                                        mkU32(0x00000000)));
                        assign(t10, binop(Iop_CmpNE32,
                                        binop(Iop_And32,
                                              mkexpr(t3),
                                              mkU32(0x0000ff00)),
                                        mkU32(0x0000ff00)));

                        assign(t6, binop(Iop_Or32,
                                         binop(Iop_Or32,
                                               binop(Iop_And32,
                                                     unop(Iop_1Uto32,
                                                          mkexpr(t1)),
                                                     unop(Iop_1Uto32,
                                                          mkexpr(t2))),
                                               binop(Iop_And32,
                                                     unop(Iop_1Uto32,
                                                          mkexpr(t7)),
                                                     unop(Iop_1Uto32,
                                                          mkexpr(t8)))),
                                         binop(Iop_Or32,
                                               binop(Iop_And32,
                                                     unop(Iop_1Uto32,
                                                          mkexpr(t4)),
                                                     unop(Iop_1Uto32,
                                                          mkexpr(t5))),
                                               binop(Iop_And32,
                                                     unop(Iop_1Uto32,
                                                          mkexpr(t9)),
                                                     unop(Iop_1Uto32,
                                                          mkexpr(t10))))));

                        putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                       mkexpr(t6),
                                                       mkU32(0x0)),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000)),
                                                 getDSPControl()));
                        putIReg(rd, binop(Iop_Or32,
                                          binop(Iop_Shl32,
                                                binop(Iop_And32,
                                                      mkexpr(t3),
                                                      mkU32(0x00ff00ff)),
                                                mkU8(8)),
                                          binop(Iop_And32,
                                                mkexpr(t0),
                                                mkU32(0x00ff00ff))));
                     }
                     break;
                  }
                  case 0x3: {  /* SHRL.QB */
                     DIP("shrl.qb r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I8);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I8);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I8);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I8);
                     t9 = newTemp(Ity_I32);

                     assign(t9, binop(Iop_And32, getIReg(rs), mkU32(0x7)));
                     assign(t0, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32to16, getIReg(rt)))));
                     assign(t1, unop(Iop_32to8,
                                     binop(Iop_Shr32,
                                           mkexpr(t0),
                                           unop(Iop_32to8, mkexpr(t9)))));

                     assign(t2, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32to16, getIReg(rt)))));
                     assign(t3, unop(Iop_32to8,
                                     binop(Iop_Shr32,
                                           mkexpr(t2),
                                           unop(Iop_32to8, mkexpr(t9)))));

                     assign(t4, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t5, unop(Iop_32to8,
                                     binop(Iop_Shr32,
                                           mkexpr(t4),
                                           unop(Iop_32to8, mkexpr(t9)))));

                     assign(t6, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t7, unop(Iop_32to8,
                                     binop(Iop_Shr32,
                                           mkexpr(t6),
                                           unop(Iop_32to8, mkexpr(t9)))));
                     putIReg(rd, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                  mkexpr(t9),
                                                  mkU32(0x0)),
                                            getIReg(rt),
                                            binop(Iop_16HLto32,
                                                  binop(Iop_8HLto16,
                                                        mkexpr(t7),
                                                        mkexpr(t5)),
                                                  binop(Iop_8HLto16,
                                                        mkexpr(t3),
                                                        mkexpr(t1)))));
                     break;
                  }
                  case 0x2: {  /* SHLLV.QB */
                     DIP("shllv.qb r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I1);
                     t9 = newTemp(Ity_I1);
                     t10 = newTemp(Ity_I1);
                     t11 = newTemp(Ity_I8);

                     assign(t11, unop(Iop_32to8,
                                      binop(Iop_And32,
                                            getIReg(rs),
                                            mkU32(0x7))));
                     /* Shift bits 7..0 and 23..16. */
                     assign(t0, binop(Iop_Shl32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0x00ff00ff)),
                                      mkexpr(t11)));
                     assign(t1, binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0xff000000)),
                                     mkU32(0x00000000)));
                     assign(t2, binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0xff000000)),
                                     mkU32(0xff000000)));
                     assign(t7, binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0x0000ff00)),
                                     mkU32(0x00000000)));
                     assign(t8, binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t0),
                                           mkU32(0x0000ff00)),
                                     mkU32(0x000ff00)));
                     /* Shift bits 15..8 and 31..24. */
                     assign(t3, binop(Iop_Shl32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  getIReg(rt),
                                                  mkU32(0xff00ff00)),
                                            mkU8(8)),
                                      mkexpr(t11)));
                     assign(t4, binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t3),
                                           mkU32(0xff000000)),
                                     mkU32(0x00000000)));
                     assign(t5, binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t3),
                                           mkU32(0xff000000)),
                                     mkU32(0xff000000)));
                     assign(t9, binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t3),
                                           mkU32(0x0000ff00)),
                                     mkU32(0x00000000)));
                     assign(t10, binop(Iop_CmpNE32,
                                     binop(Iop_And32,
                                           mkexpr(t3),
                                           mkU32(0x0000ff00)),
                                     mkU32(0x0000ff00)));

                     assign(t6, binop(Iop_Or32,
                                      binop(Iop_Or32,
                                            binop(Iop_And32,
                                                  unop(Iop_1Uto32,
                                                       mkexpr(t1)),
                                                  unop(Iop_1Uto32,
                                                       mkexpr(t2))),
                                            binop(Iop_And32,
                                                  unop(Iop_1Uto32,
                                                       mkexpr(t7)),
                                                  unop(Iop_1Uto32,
                                                       mkexpr(t8)))),
                                      binop(Iop_Or32,
                                            binop(Iop_And32,
                                                  unop(Iop_1Uto32,
                                                       mkexpr(t4)),
                                                  unop(Iop_1Uto32,
                                                       mkexpr(t5))),
                                            binop(Iop_And32,
                                                  unop(Iop_1Uto32,
                                                       mkexpr(t9)),
                                                  unop(Iop_1Uto32,
                                                       mkexpr(t10))))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t6),
                                                    mkU32(0x0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x400000)),
                                              getDSPControl()));
                     putIReg(rd, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                  unop(Iop_8Uto32, mkexpr(t11)),
                                                  mkU32(0)),
                                            getIReg(rt),
                                            binop(Iop_Or32,
                                                  binop(Iop_Shl32,
                                                        binop(Iop_And32,
                                                              mkexpr(t3),
                                                              mkU32(0xff00ff)),
                                                        mkU8(8)),
                                                  binop(Iop_And32,
                                                        mkexpr(t0),
                                                        mkU32(0x00ff00ff)))));
                     break;
                  }
                  case 0x1: {  /* SHRLV.QB */
                     DIP("shrlv.qb r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I8);
                     t1 = newTemp(Ity_I8);
                     t2 = newTemp(Ity_I8);
                     t3 = newTemp(Ity_I8);

                     assign(t0, unop(Iop_32to8,
                                     binop(Iop_Shr32,
                                           unop(Iop_8Uto32,
                                                unop(Iop_32to8, getIReg(rt))),
                                           mkU8(rs))));
                     assign(t1, unop(Iop_32to8,
                                     binop(Iop_Shr32,
                                           unop(Iop_8Uto32,
                                                unop(Iop_16HIto8,
                                                     unop(Iop_32to16,
                                                          getIReg(rt)))),
                                           mkU8(rs))));
                     assign(t2, unop(Iop_32to8,
                                      binop(Iop_Shr32,
                                            unop(Iop_8Uto32,
                                                 unop(Iop_16to8,
                                                      unop(Iop_32HIto16,
                                                           getIReg(rt)))),
                                            mkU8(rs))));
                     assign(t3, unop(Iop_32to8,
                                     binop(Iop_Shr32,
                                           unop(Iop_8Uto32,
                                                unop(Iop_16HIto8,
                                                     unop(Iop_32HIto16,
                                                          getIReg(rt)))),
                                           mkU8(rs))));
                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   binop(Iop_8HLto16, mkexpr(t3), mkexpr(t2)),
                                   binop(Iop_8HLto16, mkexpr(t1), mkexpr(t0))));
                     break;
                  }
                  case 0x4: {  /* SHRA.QB */
                     DIP("shra.qb r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);
                     t10 = newTemp(Ity_I32);
                     t11 = newTemp(Ity_I32);

                     /* ========== GPR[rt]_31..24 ========== */
                     assign(t1,
                            unop(Iop_8Uto32,
                                 unop(Iop_16HIto8,
                                      unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t2,
                            binop(Iop_Shr32, mkexpr(t1), mkU8(rs)));
                     /* tempD_7..0 */
                     assign(t0,
                            binop(Iop_Or32,
                                  mkexpr(t2),
                                  binop(Iop_Shl32,
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         binop(Iop_And32,
                                                               mkexpr(t1),
                                                               mkU32(0x00000080)
                                                              ),
                                                         mkU32(0x00000080)),
                                                   mkU32(0xFFFFFFFF),
                                                   mkU32(0x00000000)),
                                        binop(Iop_Sub8, mkU8(0x8), mkU8(rs)))));

                     /* ========== GPR[rt]_23..16 ========== */
                     assign(t4,
                            unop(Iop_8Uto32,
                                 unop(Iop_16to8,
                                      unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t5, binop(Iop_Shr32, mkexpr(t4), mkU8(rs)));
                     /* tempC_7..0 */
                     assign(t3,
                            binop(Iop_Or32,
                                  mkexpr(t5),
                                  binop(Iop_Shl32,
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         binop(Iop_And32,
                                                               mkexpr(t4),
                                                               mkU32(0x00000080)
                                                              ),
                                                         mkU32(0x00000080)),
                                                   mkU32(0xFFFFFFFF),
                                                   mkU32(0x00000000)),
                                        binop(Iop_Sub8, mkU8(0x8), mkU8(rs)))));

                     /* ========== GPR[rt]_15..8 ========== */
                     assign(t7,
                            unop(Iop_8Uto32,
                                 unop(Iop_16HIto8,
                                      unop(Iop_32to16, getIReg(rt)))));
                     assign(t8, binop(Iop_Shr32, mkexpr(t7), mkU8(rs)));
                     /* tempB_7..0 */
                     assign(t6,
                            binop(Iop_Or32,
                                  mkexpr(t8),
                                  binop(Iop_Shl32,
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         binop(Iop_And32,
                                                               mkexpr(t7),
                                                               mkU32(0x00000080)
                                                              ),
                                                         mkU32(0x00000080)),
                                                   mkU32(0xFFFFFFFF),
                                                   mkU32(0x00000000)),
                                        binop(Iop_Sub8, mkU8(0x8), mkU8(rs)))));

                     /* ========== GPR[rt]_7..0 ========== */
                     assign(t10,
                            unop(Iop_8Uto32,
                                 unop(Iop_16to8,
                                      unop(Iop_32to16, getIReg(rt)))));
                     assign(t11, binop(Iop_Shr32, mkexpr(t10), mkU8(rs)));
                     /* tempB_7..0 */
                     assign(t9,
                            binop(Iop_Or32,
                                  mkexpr(t11),
                                  binop(Iop_Shl32,
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         binop(Iop_And32,
                                                               mkexpr(t10),
                                                               mkU32(0x00000080)
                                                              ),
                                                         mkU32(0x00000080)),
                                                   mkU32(0xFFFFFFFF),
                                                   mkU32(0x00000000)),
                                        binop(Iop_Sub8, mkU8(0x8), mkU8(rs)))));

                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   binop(Iop_8HLto16,
                                         unop(Iop_32to8, mkexpr(t0)),
                                         unop(Iop_32to8, mkexpr(t3))),
                                   binop(Iop_8HLto16,
                                         unop(Iop_32to8, mkexpr(t6)),
                                         unop(Iop_32to8, mkexpr(t9)))));
                     break;
                  }
                  case 0x5: {  /* SHRA_R.QB */
                     DIP("shra_r.qb r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I8);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I8);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I8);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I8);

                     if (0 == rs) {
                        putIReg(rd, getIReg(rt));
                     } else {
                        assign(t0, unop(Iop_8Sto32,
                                        unop(Iop_16to8,
                                             unop(Iop_32to16, getIReg(rt)))));
                        assign(t1, unop(Iop_32to8,
                                        binop(Iop_Sar32,
                                              binop(Iop_Add32,
                                                    mkexpr(t0),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(rs-1))),
                                              mkU8(rs))));

                        assign(t2, unop(Iop_8Sto32,
                                        unop(Iop_16HIto8,
                                             unop(Iop_32to16, getIReg(rt)))));
                        assign(t3, unop(Iop_32to8,
                                        binop(Iop_Sar32,
                                              binop(Iop_Add32,
                                                    mkexpr(t2),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(rs-1))),
                                              mkU8(rs))));

                        assign(t4, unop(Iop_8Sto32,
                                        unop(Iop_16to8,
                                             unop(Iop_32HIto16, getIReg(rt)))));
                        assign(t5, unop(Iop_32to8,
                                        binop(Iop_Sar32,
                                              binop(Iop_Add32,
                                                    mkexpr(t4),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(rs-1))),
                                              mkU8(rs))));

                        assign(t6, unop(Iop_8Sto32,
                                        unop(Iop_16HIto8,
                                             unop(Iop_32HIto16, getIReg(rt)))));
                        assign(t7, unop(Iop_32to8,
                                        binop(Iop_Sar32,
                                              binop(Iop_Add32,
                                                    mkexpr(t6),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(rs-1))),
                                              mkU8(rs))));
                        putIReg(rd, binop(Iop_16HLto32,
                                         binop(Iop_8HLto16,
                                               mkexpr(t7), mkexpr(t5)),
                                         binop(Iop_8HLto16,
                                               mkexpr(t3), mkexpr(t1))));
                     }
                     break;
                  }
                  case 0x6: {  /* SHRAV.QB */
                     DIP("shrav.qb r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);

                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);

                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);

                     t9 = newTemp(Ity_I32);
                     t10 = newTemp(Ity_I32);
                     t11 = newTemp(Ity_I32);

                     /* ========== GPR[rt]_31..24 ========== */
                     assign(t1,
                            unop(Iop_8Uto32,
                                 unop(Iop_16HIto8,
                                      unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t2,
                            binop(Iop_Shr32,
                                  mkexpr(t1),
                                  unop(Iop_32to8, binop(Iop_And32,
                                                        getIReg(rs),
                                                        mkU32(0x7)))));
                     /* tempD_7..0 */
                     assign(t0,
                            binop(Iop_Or32,
                                  mkexpr(t2),
                                  binop(Iop_Shl32,
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         binop(Iop_And32,
                                                               mkexpr(t1),
                                                               mkU32(0x00000080)
                                                              ),
                                                         mkU32(0x00000080)),
                                                   mkU32(0xFFFFFFFF),
                                                   mkU32(0x00000000)),
                                        binop(Iop_Sub8,
                                              mkU8(0x8),
                                              unop(Iop_32to8, binop(Iop_And32,
                                                                    getIReg(rs),
                                                                    mkU32(0x7)))
                                              ))));

                     /* ========== GPR[rt]_23..16 ========== */
                     assign(t4,
                            unop(Iop_8Uto32,
                                 unop(Iop_16to8,
                                      unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t5,
                            binop(Iop_Shr32,
                                  mkexpr(t4),
                                  unop(Iop_32to8, binop(Iop_And32,
                                                        getIReg(rs),
                                                        mkU32(0x7)))));
                     /* tempC_7..0 */
                     assign(t3,
                            binop(Iop_Or32,
                                  mkexpr(t5),
                                  binop(Iop_Shl32,
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         binop(Iop_And32,
                                                               mkexpr(t4),
                                                               mkU32(0x00000080)
                                                              ),
                                                         mkU32(0x00000080)),
                                                   mkU32(0xFFFFFFFF),
                                                   mkU32(0x00000000)),
                                        binop(Iop_Sub8,
                                              mkU8(0x8),
                                              unop(Iop_32to8, binop(Iop_And32,
                                                                    getIReg(rs),
                                                                    mkU32(0x7)))
                                              ))));

                     /* ========== GPR[rt]_15..8 ========== */
                     assign(t7,
                            unop(Iop_8Uto32,
                                 unop(Iop_16HIto8,
                                      unop(Iop_32to16, getIReg(rt)))));
                     assign(t8,
                            binop(Iop_Shr32,
                                  mkexpr(t7),
                                  unop(Iop_32to8, binop(Iop_And32,
                                                        getIReg(rs),
                                                        mkU32(0x7)))));
                     /* tempB_7..0 */
                     assign(t6,
                            binop(Iop_Or32,
                                  mkexpr(t8),
                                  binop(Iop_Shl32,
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         binop(Iop_And32,
                                                               mkexpr(t7),
                                                               mkU32(0x00000080)
                                                              ),
                                                         mkU32(0x00000080)),
                                                   mkU32(0xFFFFFFFF),
                                                   mkU32(0x00000000)),
                                        binop(Iop_Sub8,
                                              mkU8(0x8),
                                              unop(Iop_32to8, binop(Iop_And32,
                                                                    getIReg(rs),
                                                                    mkU32(0x7)))
                                              ))));

                     /* ========== GPR[rt]_7..0 ========== */
                     assign(t10,
                            unop(Iop_8Uto32,
                                 unop(Iop_16to8,
                                      unop(Iop_32to16, getIReg(rt)))));
                     assign(t11,
                            binop(Iop_Shr32,
                                  mkexpr(t10),
                                  unop(Iop_32to8, binop(Iop_And32,
                                                        getIReg(rs),
                                                        mkU32(0x7)))));
                     /* tempB_7..0 */
                     assign(t9,
                            binop(Iop_Or32,
                                  mkexpr(t11),
                                  binop(Iop_Shl32,
                                        IRExpr_ITE(binop(Iop_CmpEQ32,
                                                         binop(Iop_And32,
                                                               mkexpr(t10),
                                                               mkU32(0x00000080)
                                                              ),
                                                         mkU32(0x00000080)),
                                                   mkU32(0xFFFFFFFF),
                                                   mkU32(0x00000000)),
                                        binop(Iop_Sub8,
                                              mkU8(0x8),
                                              unop(Iop_32to8, binop(Iop_And32,
                                                                    getIReg(rs),
                                                                    mkU32(0x7)))
                                              ))));

                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   binop(Iop_8HLto16,
                                         unop(Iop_32to8,
                                              IRExpr_ITE(binop(Iop_CmpEQ32,
                                                               binop(Iop_And32,
                                                                     mkU32(rs),
                                                                     mkU32(0x7)
                                                                    ),
                                                               mkU32(0x0)),
                                                         mkexpr(t1),
                                                         mkexpr(t0))),
                                         unop(Iop_32to8,
                                              IRExpr_ITE(binop(Iop_CmpEQ32,
                                                               binop(Iop_And32,
                                                                     mkU32(rs),
                                                                     mkU32(0x7)
                                                                    ),
                                                               mkU32(0x0)),
                                                         mkexpr(t2),
                                                         mkexpr(t3)))),
                                   binop(Iop_8HLto16,
                                         unop(Iop_32to8,
                                              IRExpr_ITE(binop(Iop_CmpEQ32,
                                                               binop(Iop_And32,
                                                                     mkU32(rs),
                                                                     mkU32(0x7)
                                                                    ),
                                                               mkU32(0x0)),
                                                         mkexpr(t5),
                                                         mkexpr(t6))),
                                         unop(Iop_32to8,
                                              IRExpr_ITE(binop(Iop_CmpEQ32,
                                                               binop(Iop_And32,
                                                                     mkU32(rs),
                                                                     mkU32(0x7)
                                                                    ),
                                                               mkU32(0x0)),
                                                         mkexpr(t8),
                                                         mkexpr(t9))))));
                     break;
                  }
                  case 0x7: {  /* SHRAV_R.QB */
                     DIP("shrav_r.qb r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I8);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I8);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I8);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I8);
                     t8 = newTemp(Ity_I8);
                     t9 = newTemp(Ity_I32);

                     assign(t9, binop(Iop_And32, getIReg(rs), mkU32(0x7)));
                     assign(t8, unop(Iop_32to8,
                                     binop(Iop_Sub32, mkexpr(t9), mkU32(0x1))));
                     assign(t0, unop(Iop_8Sto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32to16, getIReg(rt)))));
                     assign(t1, unop(Iop_32to8,
                                     binop(Iop_Sar32,
                                           binop(Iop_Add32,
                                                 mkexpr(t0),
                                                 binop(Iop_Shl32,
                                                       mkU32(0x1),
                                                       mkexpr(t8))),
                                           unop(Iop_32to8,
                                                mkexpr(t9)))));

                     assign(t2, unop(Iop_8Sto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32to16, getIReg(rt)))));
                     assign(t3, unop(Iop_32to8,
                                     binop(Iop_Sar32,
                                           binop(Iop_Add32,
                                                 mkexpr(t2),
                                                 binop(Iop_Shl32,
                                                       mkU32(0x1),
                                                       mkexpr(t8))),
                                           unop(Iop_32to8, mkexpr(t9)))));

                     assign(t4, unop(Iop_8Sto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t5, unop(Iop_32to8,
                                     binop(Iop_Sar32,
                                           binop(Iop_Add32,
                                                 mkexpr(t4),
                                                 binop(Iop_Shl32,
                                                       mkU32(0x1),
                                                       mkexpr(t8))),
                                           unop(Iop_32to8, mkexpr(t9)))));

                     assign(t6, unop(Iop_8Sto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t7, unop(Iop_32to8,
                                     binop(Iop_Sar32,
                                           binop(Iop_Add32,
                                                 mkexpr(t6),
                                                 binop(Iop_Shl32,
                                                       mkU32(0x1),
                                                       mkexpr(t8))),
                                           unop(Iop_32to8, mkexpr(t9)))));
                     putIReg(rd, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                  mkexpr(t9),
                                                  mkU32(0x0)),
                                            getIReg(rt),
                                            binop(Iop_16HLto32,
                                                  binop(Iop_8HLto16,
                                                        mkexpr(t7),
                                                        mkexpr(t5)),
                                                  binop(Iop_8HLto16,
                                                        mkexpr(t3),
                                                        mkexpr(t1)))));
                     break;
                  }
                  case 0x8: {  /* SHLL.PH */
                     DIP("shll.ph r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);

                     if (0 == rs) {
                        putIReg(rd, getIReg(rt));
                     } else {
                        /* Shift lower 16 bits. */
                        assign(t0, binop(Iop_Shl32,
                                         unop(Iop_16Sto32,
                                              unop(Iop_32to16, getIReg(rt))),
                                         mkU8(rs)));

                        assign(t1, unop(Iop_1Uto32,
                                        binop(Iop_CmpNE32,
                                               binop(Iop_Sar32,
                                                     mkexpr(t0),
                                                     mkU8(16)),
                                               mkU32(0))));
                        assign(t2, unop(Iop_1Uto32,
                                        binop(Iop_CmpNE32,
                                              binop(Iop_Sar32,
                                                    mkexpr(t0),
                                                    mkU8(16)),
                                              mkU32(0xffffffff))));
                        assign(t3, binop(Iop_And32,
                                         mkexpr(t1),
                                         mkexpr(t2)));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       mkexpr(t3),
                                                       mkU32(0x1)),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000)),
                                                 getDSPControl()));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       binop(Iop_And32,
                                                             getIReg(rt),
                                                             mkU32(0x00008000)),
                                                       binop(Iop_And32,
                                                             mkexpr(t0),
                                                             mkU32(0x00008000))
                                                      ),
                                                 getDSPControl(),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000))));
                        /* Shift higher 16 bits. */
                        assign(t4, binop(Iop_Shl32,
                                         unop(Iop_16Sto32,
                                              unop(Iop_32HIto16, getIReg(rt))),
                                         mkU8(rs)));

                        assign(t5, unop(Iop_1Uto32,
                                        binop(Iop_CmpNE32,
                                               binop(Iop_Sar32,
                                                     mkexpr(t4),
                                                     mkU8(16)),
                                               mkU32(0))));
                        assign(t6, unop(Iop_1Uto32,
                                        binop(Iop_CmpNE32,
                                              binop(Iop_Sar32,
                                                    mkexpr(t4),
                                                    mkU8(16)),
                                              mkU32(0xffffffff))));
                        assign(t7, binop(Iop_And32,
                                         mkexpr(t5),
                                         mkexpr(t6)));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       mkexpr(t7),
                                                       mkU32(0x1)),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000)),
                                                 getDSPControl()));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       mkexpr(t7),
                                                       mkU32(0x1)),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000)),
                                                 getDSPControl()));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       binop(Iop_And32,
                                                             getIReg(rt),
                                                             mkU32(0x80000000)),
                                                       binop(Iop_Shl32,
                                                             binop(Iop_And32,
                                                                   mkexpr(t4),
                                                                   mkU32(0x00008000)),
                                                             mkU8(16))
                                                      ),
                                                 getDSPControl(),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000))));
                        putIReg(rd, binop(Iop_16HLto32,
                                          unop(Iop_32to16, mkexpr(t4)),
                                          unop(Iop_32to16, mkexpr(t0))));
                     }
                     break;
                  }
                  case 0x9: {  /* SHRA.PH */
                     DIP("shra.ph r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     if (0 == rs) {
                        putIReg(rd, getIReg(rt));
                     } else {
                        assign(t0, binop(Iop_Sar32,
                                         unop(Iop_16Sto32,
                                              unop(Iop_32to16, getIReg(rt))),
                                         mkU8(rs)));
                        assign(t1, binop(Iop_Sar32,
                                         unop(Iop_16Sto32,
                                              unop(Iop_32HIto16, getIReg(rt))),
                                         mkU8(rs)));
                        putIReg(rd, binop(Iop_16HLto32,
                                          unop(Iop_32to16, mkexpr(t1)),
                                          unop(Iop_32to16, mkexpr(t0))));
                     }
                     break;
                  }
                  case 0xA: {  /* SHLLV.PH */
                     DIP("shllv.ph r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I1);
                     t9 = newTemp(Ity_I32);
                     t10 = newTemp(Ity_I32);
                     t11 = newTemp(Ity_I32);
                     t12 = newTemp(Ity_I1);
                     t13 = newTemp(Ity_I1);

                     assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));

                     /* Shift lower 16 bits. */
                     assign(t2, binop(Iop_Shl32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      unop(Iop_32to8, mkexpr(t0))));

                     assign(t3, binop(Iop_CmpNE32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, mkexpr(t2))),
                                      mkU32(0x00000000)));
                     assign(t4, binop(Iop_CmpNE32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, mkexpr(t2))),
                                      mkU32(0xffffffff)));
                     assign(t10, binop(Iop_And32,
                                       unop(Iop_1Sto32, mkexpr(t3)),
                                       unop(Iop_1Sto32, mkexpr(t4))));
                     assign(t5, binop(Iop_Shr32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0x00008000)),
                                      mkU8(15)));
                     assign(t12, binop(Iop_CmpEQ32,
                                       mkexpr(t5),
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   mkexpr(t2),
                                                   mkU32(0x00008000)),
                                             mkU8(15))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t10),
                                                    mkU32(0x0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x400000)),
                                              IRExpr_ITE(mkexpr(t12),
                                                         getDSPControl(),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x400000)))
                                             ));
                     /* Shift higher 16 bits. */
                     assign(t6, binop(Iop_Shl32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      unop(Iop_32to8, mkexpr(t0))));

                     assign(t7, binop(Iop_CmpNE32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, mkexpr(t6))),
                                      mkU32(0x00000000)));
                     assign(t8, binop(Iop_CmpNE32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, mkexpr(t6))),
                                      mkU32(0xffffffff)));
                     assign(t11, binop(Iop_And32,
                                       unop(Iop_1Sto32, mkexpr(t7)),
                                       unop(Iop_1Sto32, mkexpr(t8))));

                     assign(t9, binop(Iop_Shr32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0x80000000)),
                                      mkU8(31)));
                     assign(t13, binop(Iop_CmpEQ32,
                                       mkexpr(t9),
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   mkexpr(t6),
                                                   mkU32(0x00008000)),
                                             mkU8(15))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t11),
                                                    mkU32(0x0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x400000)),
                                              IRExpr_ITE(mkexpr(t13),
                                                         getDSPControl(),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x400000)))
                                             ));

                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, mkexpr(t6)),
                                       unop(Iop_32to16, mkexpr(t2))));
                     break;
                  }
                  case 0xB: {  /* SHRAV.PH */
                     DIP("shrav.ph r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));
                     assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));
                     assign(t2, binop(Iop_Sar32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      unop(Iop_32to8, mkexpr(t0))));
                     assign(t3, binop(Iop_Sar32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      unop(Iop_32to8, mkexpr(t0))));
                     putIReg(rd,
                             binop(Iop_16HLto32,
                                   IRExpr_ITE(mkexpr(t1),
                                              unop(Iop_32HIto16, getIReg(rt)),
                                              unop(Iop_32to16, mkexpr(t3))),
                                   IRExpr_ITE(mkexpr(t1),
                                              unop(Iop_32to16, getIReg(rt)),
                                              unop(Iop_32to16, mkexpr(t2)))));
                     break;
                  }
                  case 0xC: {  /* SHLL_S.PH */
                     DIP("shll_s.ph r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);
                     t10 = newTemp(Ity_I32);
                     t11 = newTemp(Ity_I32);
                     t12 = newTemp(Ity_I32);
                     t13 = newTemp(Ity_I32);
                     t14 = newTemp(Ity_I32);

                     if (0 == rs) {
                        putIReg(rd, getIReg(rt));
                     } else {
                        /* Shift lower 16 bits. */
                        assign(t0, binop(Iop_Shl32,
                                         unop(Iop_16Sto32,
                                              unop(Iop_32to16, getIReg(rt))),
                                         mkU8(rs)));

                        assign(t1, unop(Iop_1Uto32,
                                        binop(Iop_CmpNE32,
                                               binop(Iop_Sar32,
                                                     mkexpr(t0),
                                                     mkU8(16)),
                                               mkU32(0))));
                        assign(t2, unop(Iop_1Uto32,
                                        binop(Iop_CmpNE32,
                                              binop(Iop_Sar32,
                                                    mkexpr(t0),
                                                    mkU8(16)),
                                              mkU32(0xffffffff))));
                        assign(t3, binop(Iop_And32,
                                         mkexpr(t1),
                                         mkexpr(t2)));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       mkexpr(t3),
                                                       mkU32(0x1)),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000)),
                                                 getDSPControl()));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       binop(Iop_And32,
                                                             getIReg(rt),
                                                             mkU32(0x00008000)),
                                                       binop(Iop_And32,
                                                             mkexpr(t0),
                                                             mkU32(0x00008000))
                                                      ),
                                                 getDSPControl(),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000))));
                        assign(t8,
                               IRExpr_ITE(binop(Iop_CmpEQ32,
                                                mkexpr(t3),
                                                mkU32(0x1)),
                                          IRExpr_ITE(binop(Iop_CmpEQ32,
                                                           binop(Iop_And32,
                                                                 getIReg(rt),
                                                                 mkU32(0x8000)),
                                                           mkU32(0)),
                                                     mkU32(0x00007fff),
                                                     mkU32(0x00008000)),
                                          binop(Iop_And32,
                                                mkexpr(t0),
                                                mkU32(0x0000ffff))));
                        assign(t10,
                               IRExpr_ITE(binop(Iop_CmpEQ32,
                                                binop(Iop_And32,
                                                      getIReg(rt),
                                                      mkU32(0x00008000)),
                                                binop(Iop_And32,
                                                      mkexpr(t0),
                                                      mkU32(0x00008000))),
                                          mkexpr(t8),
                                          IRExpr_ITE(binop(Iop_CmpEQ32,
                                                           binop(Iop_And32,
                                                                 getIReg(rt),
                                                                 mkU32(0x8000)),
                                                           mkU32(0)),
                                                     mkU32(0x00007fff),
                                                     mkU32(0x00008000))));
                        /* Shift higher 16 bits. */
                        assign(t4, binop(Iop_Shl32,
                                         unop(Iop_16Sto32,
                                              unop(Iop_32HIto16, getIReg(rt))),
                                         mkU8(rs)));

                        assign(t5, unop(Iop_1Uto32,
                                        binop(Iop_CmpNE32,
                                               binop(Iop_Sar32,
                                                     mkexpr(t4),
                                                     mkU8(16)),
                                               mkU32(0))));
                        assign(t6, unop(Iop_1Uto32,
                                        binop(Iop_CmpNE32,
                                              binop(Iop_Sar32,
                                                    mkexpr(t4),
                                                    mkU8(16)),
                                              mkU32(0xffffffff))));
                        assign(t7, binop(Iop_And32,
                                         mkexpr(t5),
                                         mkexpr(t6)));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       mkexpr(t7),
                                                       mkU32(0x1)),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000)),
                                                 getDSPControl()));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       mkexpr(t7),
                                                       mkU32(0x1)),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000)),
                                                 getDSPControl()));
                        assign(t12, binop(Iop_Shl32,
                                          binop(Iop_And32,
                                                mkexpr(t4),
                                                mkU32(0x8000)),
                                          mkU8(16)));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       binop(Iop_And32,
                                                             getIReg(rt),
                                                             mkU32(0x80000000)),
                                                       mkexpr(t12)),
                                                 getDSPControl(),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000))));
                        assign(t13, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                     binop(Iop_And32,
                                                           getIReg(rt),
                                                           mkU32(0x80000000)),
                                                     mkU32(0)),
                                               mkU32(0x7fff0000),
                                               mkU32(0x80000000)));
                        assign(t9,
                               IRExpr_ITE(binop(Iop_CmpEQ32,
                                                mkexpr(t7),
                                                mkU32(0x1)),
                                          mkexpr(t13),
                                          binop(Iop_Shl32,
                                                binop(Iop_And32,
                                                      mkexpr(t4),
                                                      mkU32(0x0000ffff)),
                                                mkU8(16))));
                        assign(t14, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                     binop(Iop_And32,
                                                           getIReg(rt),
                                                           mkU32(0x80000000)),
                                                     mkU32(0)),
                                               mkU32(0x7fff0000),
                                               mkU32(0x80000000)));
                        assign(t11,
                               IRExpr_ITE(binop(Iop_CmpEQ32,
                                                binop(Iop_And32,
                                                      getIReg(rt),
                                                      mkU32(0x80000000)),
                                                binop(Iop_Shl32,
                                                      binop(Iop_And32,
                                                            mkexpr(t4),
                                                            mkU32(0x00008000)),
                                                      mkU8(16))),
                                          mkexpr(t9),
                                          mkexpr(t14)));
                        putIReg(rd, binop(Iop_Or32,
                                          mkexpr(t10),
                                          mkexpr(t11)));
                     }
                     break;
                  }
                  case 0xD: {  /* SHRA_R.PH */
                     DIP("shra.ph r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     if (0 == rs) {
                        putIReg(rd, getIReg(rt));
                     } else {
                        assign(t0, binop(Iop_Sar32,
                                         binop(Iop_Add32,
                                               unop(Iop_16Sto32,
                                                    unop(Iop_32to16,
                                                         getIReg(rt))),
                                               binop(Iop_Shl32,
                                                     mkU32(0x1),
                                                     mkU8(rs-1))),
                                         mkU8(rs)));
                        assign(t1, binop(Iop_Sar32,
                                         binop(Iop_Add32,
                                               unop(Iop_16Sto32,
                                                    unop(Iop_32HIto16,
                                                         getIReg(rt))),
                                               binop(Iop_Shl32,
                                                     mkU32(0x1),
                                                     mkU8(rs-1))),
                                         mkU8(rs)));
                        putIReg(rd, binop(Iop_16HLto32,
                                          unop(Iop_32to16, mkexpr(t1)),
                                          unop(Iop_32to16, mkexpr(t0))));
                     }
                     break;
                  }
                  case 0xE: {  /* SHLLV_S.PH */
                     DIP("shllv_s.ph r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I1);
                     t9 = newTemp(Ity_I32);
                     t10 = newTemp(Ity_I32);
                     t11 = newTemp(Ity_I32);
                     t12 = newTemp(Ity_I1);
                     t13 = newTemp(Ity_I1);
                     t14 = newTemp(Ity_I16);
                     t15 = newTemp(Ity_I16);
                     t16 = newTemp(Ity_I16);
                     t17 = newTemp(Ity_I16);

                     assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));

                     /* Shift lower 16 bits. */
                     assign(t2, binop(Iop_Shl32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      unop(Iop_32to8, mkexpr(t0))));

                     assign(t3, binop(Iop_CmpNE32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, mkexpr(t2))),
                                      mkU32(0x00000000)));
                     assign(t4, binop(Iop_CmpNE32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, mkexpr(t2))),
                                      mkU32(0xffffffff)));
                     assign(t10, binop(Iop_And32,
                                       unop(Iop_1Sto32, mkexpr(t3)),
                                       unop(Iop_1Sto32, mkexpr(t4))));
                     assign(t5, binop(Iop_Shr32,
                                       binop(Iop_And32,
                                             getIReg(rt),
                                             mkU32(0x00008000)),
                                       mkU8(15)));
                     assign(t12, binop(Iop_CmpEQ32,
                                       mkexpr(t5),
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   mkexpr(t2),
                                                   mkU32(0x00008000)),
                                             mkU8(15))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t10),
                                                    mkU32(0x0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x400000)),
                                              IRExpr_ITE(mkexpr(t12),
                                                         getDSPControl(),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x400000)))
                                             ));
                     assign(t14, IRExpr_ITE(binop(Iop_CmpNE32,
                                                  mkexpr(t5),
                                                  mkU32(0x0)),
                                            mkU16(0x8000),
                                            mkU16(0x7fff)));
                     assign(t15, IRExpr_ITE(binop(Iop_CmpNE32,
                                                  mkexpr(t10),
                                                  mkU32(0x0)),
                                            mkexpr(t14),
                                            IRExpr_ITE(mkexpr(t12),
                                                       unop(Iop_32to16,
                                                            mkexpr(t2)),
                                                       mkexpr(t14))));
                     /* Shift higher 16 bits. */
                     assign(t6, binop(Iop_Shl32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      unop(Iop_32to8, mkexpr(t0))));

                     assign(t7, binop(Iop_CmpNE32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, mkexpr(t6))),
                                      mkU32(0x00000000)));
                     assign(t8, binop(Iop_CmpNE32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, mkexpr(t6))),
                                      mkU32(0xffffffff)));
                     assign(t11, binop(Iop_And32,
                                       unop(Iop_1Sto32, mkexpr(t7)),
                                       unop(Iop_1Sto32, mkexpr(t8))));

                     assign(t9, binop(Iop_Shr32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0x80000000)),
                                      mkU8(31)));
                     assign(t13, binop(Iop_CmpEQ32,
                                       mkexpr(t9),
                                       binop(Iop_Shr32,
                                             binop(Iop_And32,
                                                   mkexpr(t6),
                                                   mkU32(0x00008000)),
                                             mkU8(15))));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t11),
                                                    mkU32(0x0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x400000)),
                                              IRExpr_ITE(mkexpr(t13),
                                                         getDSPControl(),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x400000)))
                                             ));

                     assign(t16, IRExpr_ITE(binop(Iop_CmpNE32,
                                                  mkexpr(t9),
                                                  mkU32(0x0)),
                                            mkU16(0x8000),
                                            mkU16(0x7fff)));
                     assign(t17, IRExpr_ITE(binop(Iop_CmpNE32,
                                                  mkexpr(t11),
                                                  mkU32(0x0)),
                                            mkexpr(t16),
                                            IRExpr_ITE(mkexpr(t13),
                                                       unop(Iop_32to16,
                                                            mkexpr(t6)),
                                                       mkexpr(t16))));

                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t17), mkexpr(t15)));
                     break;
                  }
                  case 0xF: {  /* SHRAV_R.PH */
                     DIP("shrav_r.ph r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I8);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));
                     assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));
                     assign(t2, unop(Iop_32to8,
                                     binop(Iop_Sub32, mkexpr(t0), mkU32(1))));

                     assign(t3, binop(Iop_Sar32,
                                      binop(Iop_Add32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32to16, getIReg(rt))),
                                            binop(Iop_Shl32,
                                                  mkU32(0x1),
                                                  mkexpr(t2))),
                                      unop(Iop_32to8, mkexpr(t0))));
                     assign(t4, binop(Iop_Sar32,
                                      binop(Iop_Add32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rt))),
                                            binop(Iop_Shl32,
                                                  mkU32(0x1),
                                                  mkexpr(t2))),
                                      unop(Iop_32to8, mkexpr(t0))));

                     putIReg(rd, binop(Iop_16HLto32,
                                       IRExpr_ITE(mkexpr(t1),
                                                  unop(Iop_32HIto16,
                                                       getIReg(rt)),
                                                  unop(Iop_32to16,
                                                       mkexpr(t4))),
                                       IRExpr_ITE(mkexpr(t1),
                                                  unop(Iop_32to16, getIReg(rt)),
                                                  unop(Iop_32to16,
                                                       mkexpr(t3)))));
                     break;
                  }
                  case 0x14: {  /* SHLL_S.W */
                     DIP("shll_s.w r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);

                     if (0 == rs) {
                        putIReg(rd, getIReg(rt));
                     } else {
                        /* t0-bits that will be discarded, sign extended to
                           32bits. */
                        assign(t0, binop(Iop_Sar32,
                                         binop(Iop_And32,
                                               getIReg(rt),
                                               binop(Iop_Sar32,
                                                     mkU32(0x80000000),
                                                     mkU8(rs-1))),
                                         mkU8(32-rs)));

                        assign(t1, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    binop(Iop_And32,
                                                          getIReg(rt),
                                                          mkU32(0x80000000)),
                                                    mkU32(0x0)),
                                              mkU32(0x7fffffff),
                                              mkU32(0x80000000)));

                        assign(t2, binop(Iop_Shl32, getIReg(rt), mkU8(rs)));
                        assign(t3, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    binop(Iop_And32,
                                                          getIReg(rt),
                                                          mkU32(0x80000000)),
                                                    binop(Iop_And32,
                                                          mkexpr(t2),
                                                          mkU32(0x80000000))),
                                              mkexpr(t2),
                                              mkexpr(t1)));

                        assign(t4, IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t0),
                                                    mkU32(0x0)),
                                              IRExpr_ITE(binop(Iop_CmpNE32,
                                                               mkexpr(t0),
                                                               mkU32(0xffffffff)
                                                              ),
                                                         mkexpr(t1),
                                                         mkexpr(t3)),
                                              mkexpr(t3)));
                        assign(t5, IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t0),
                                                    mkU32(0xffffffff)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x400000)),
                                              getDSPControl()));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                       mkexpr(t0),
                                                       mkU32(0x0)),
                                                 mkexpr(t5),
                                                 getDSPControl()));
                        putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                       binop(Iop_And32,
                                                             getIReg(rt),
                                                             mkU32(0x80000000)),
                                                       binop(Iop_And32,
                                                             mkexpr(t2),
                                                             mkU32(0x80000000))
                                                            ),
                                                 getDSPControl(),
                                                 binop(Iop_Or32,
                                                       getDSPControl(),
                                                       mkU32(0x400000))));
                        putIReg(rd, mkexpr(t4));
                     }
                     break;
                  }
                  case 0x15: {  /* SHRA_R.W */
                     DIP("shra_r.w r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     if (0 == rs) {
                        putIReg(rd, getIReg(rt));
                     } else {
                        putIReg(rd, binop(Iop_Add32,
                                          binop(Iop_Sar32,
                                                getIReg(rt), mkU8(rs)),
                                          binop(Iop_Shr32,
                                                binop(Iop_And32,
                                                      getIReg(rt),
                                                      binop(Iop_Shl32,
                                                            mkU32(0x1),
                                                            mkU8(rs-1))),
                                                mkU8(rs-1))));
                     }
                     break;
                  }
                  case 0x16: {  /* SHLLV_S.W */
                     DIP("shllv_s.w r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I64);
                     t4 = newTemp(Ity_I1);
                     t5 = newTemp(Ity_I1);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I32);

                     /* Check if shift amount is zero. */
                     assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x1f)));
                     assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));

                     /* t2 = sign of the input value. */
                     assign(t2, binop(Iop_Shr32,
                                      binop(Iop_And32,
                                            getIReg(rt),
                                            mkU32(0x80000000)),
                                      mkU8(31)));
                     /* Shift left input value and check for overflow. */
                     assign(t3, binop(Iop_Shl64,
                                      unop(Iop_32Sto64, getIReg(rt)),
                                      unop(Iop_32to8, mkexpr(t0))));
                     assign(t4, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32, mkexpr(t3)),
                                      mkU32(0x00000000)));
                     assign(t5, binop(Iop_CmpNE32,
                                      unop(Iop_64HIto32, mkexpr(t3)),
                                      mkU32(0xffffffff)));
                     assign(t6, binop(Iop_And32,
                                      unop(Iop_1Uto32, mkexpr(t4)),
                                      unop(Iop_1Uto32, mkexpr(t5))));
                     assign(t7, binop(Iop_CmpEQ32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  getIReg(rt),
                                                  mkU32(0x80000000)),
                                            mkU8(31)),
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  unop(Iop_64to32, mkexpr(t3)),
                                                  mkU32(0x80000000)),
                                            mkU8(31))));

                     putDSPControl(IRExpr_ITE(unop(Iop_32to1, mkexpr(t6)),
                                                   binop(Iop_Or32,
                                                         getDSPControl(),
                                                         mkU32(0x400000)),
                                              IRExpr_ITE(mkexpr(t7),
                                                         getDSPControl(),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x400000)))
                                             ));

                     assign(t8, IRExpr_ITE(unop(Iop_32to1,
                                                mkexpr(t2)),
                                           mkU32(0x80000000),
                                           mkU32(0x7fffffff)));
                     putIReg(rd, IRExpr_ITE(unop(Iop_32to1, mkexpr(t6)),
                                            IRExpr_ITE(unop(Iop_32to1,
                                                            mkexpr(t2)),
                                                       mkU32(0x80000000),
                                                       mkU32(0x7fffffff)),
                                            IRExpr_ITE(mkexpr(t7),
                                                       unop(Iop_64to32,
                                                            mkexpr(t3)),
                                                       mkexpr(t8))));
                     break;
                  }
                  case 0x17: {  /* SHRAV_R.W */
                     DIP("shrav_r.w r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I8);
                     t3 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x1f)));
                     assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));
                     assign(t2, unop(Iop_32to8,
                                     binop(Iop_Sub32, mkexpr(t0), mkU32(1))));

                     putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                            getIReg(rt),
                                            binop(Iop_Sar32,
                                                  binop(Iop_Add32,
                                                        binop(Iop_Sar32,
                                                              getIReg(rt),
                                                              mkexpr(t2)),
                                                        mkU32(0x1)),
                                                  mkU8(1))));
                     break;
                  }
                  case 0x19: {  /* SHRL.PH */
                     DIP("shrl.ph r%d, r%d, %d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     assign(t0, binop(Iop_Shr32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      mkU8(rs)));
                     assign(t1, binop(Iop_Shr32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      mkU8(rs)));
                     putIReg(rd, binop(Iop_16HLto32,
                                       unop(Iop_32to16, mkexpr(t1)),
                                       unop(Iop_32to16, mkexpr(t0))));
                     break;
                  }
                  case 0x1B: {  /* SHRLV.PH */
                     DIP("shrlv.ph r%d, r%d, r%d", rd, rt, rs);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I16);
                     t5 = newTemp(Ity_I16);

                     /* Get shift amount from lower 5 bits of rs
                        and check if it is zero. */
                     assign(t0, binop(Iop_And32, getIReg(rs), mkU32(0x0f)));
                     assign(t1, binop(Iop_CmpEQ32, mkexpr(t0), mkU32(0x0)));

                     assign(t2, binop(Iop_Shr32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      unop(Iop_32to8, mkexpr(t0))));
                     assign(t3, binop(Iop_Shr32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      unop(Iop_32to8, mkexpr(t0))));

                     assign(t4, IRExpr_ITE(mkexpr(t1),
                                           unop(Iop_32HIto16, getIReg(rt)),
                                           unop(Iop_32to16, mkexpr(t3))));
                     assign(t5, IRExpr_ITE(mkexpr(t1),
                                           unop(Iop_32to16, getIReg(rt)),
                                           unop(Iop_32to16, mkexpr(t2))));
                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t4), mkexpr(t5)));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of SHLL.QB */
            }
            case 0x18: {  /* ADDUH.QB/MUL.PH */
               switch(sa) {
                  case 0x00: {  /* ADDUH.QB */
                     DIP("adduh.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_HAdd8Ux4, getIReg(rs), getIReg(rt)));

                     putIReg(rd, mkexpr(t0));
                     break;
                  }
                  case 0x1: {  /* SUBUH.QB */
                     DIP("subuh.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_HSub8Ux4, getIReg(rs), getIReg(rt)));

                     putIReg(rd, mkexpr(t0));
                     break;
                  }
                  case 0x02: {  /* ADDUH_R.QB */
                     DIP("adduh_r.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I8);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I8);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I8);
                     t9 = newTemp(Ity_I32);
                     t10 = newTemp(Ity_I32);
                     t11 = newTemp(Ity_I8);

                     /* Extract input bytes, add values, add 1 and half the
                        result. */
                     assign(t0, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32to16, getIReg(rs)))));
                     assign(t1, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32to16, getIReg(rt)))));
                     assign(t2, unop(Iop_16to8,
                                     unop(Iop_32to16,
                                          binop(Iop_Shr32,
                                                binop(Iop_Add32,
                                                      binop(Iop_Add32,
                                                            mkexpr(t0),
                                                            mkexpr(t1)),
                                                      mkU32(0x00000001)),
                                                mkU8(0x01)))));

                     assign(t3, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32to16, getIReg(rs)))));
                     assign(t4, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32to16, getIReg(rt)))));
                     assign(t5, unop(Iop_16to8,
                                     unop(Iop_32to16,
                                          binop(Iop_Shr32,
                                                binop(Iop_Add32,
                                                      binop(Iop_Add32,
                                                            mkexpr(t3),
                                                            mkexpr(t4)),
                                                      mkU32(0x00000001)),
                                                mkU8(0x01)))));

                     assign(t6, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32HIto16, getIReg(rs)))));
                     assign(t7, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t8, unop(Iop_16to8,
                                     unop(Iop_32to16,
                                          binop(Iop_Shr32,
                                                binop(Iop_Add32,
                                                      binop(Iop_Add32,
                                                            mkexpr(t7),
                                                            mkexpr(t6)),
                                                      mkU32(0x00000001)),
                                                mkU8(0x01)))));

                     assign(t9, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32HIto16, getIReg(rs)))));
                     assign(t10, unop(Iop_8Uto32,
                                      unop(Iop_16HIto8,
                                           unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t11, unop(Iop_16to8,
                                      unop(Iop_32to16,
                                           binop(Iop_Shr32,
                                                 binop(Iop_Add32,
                                                       binop(Iop_Add32,
                                                             mkexpr(t9),
                                                             mkexpr(t10)),
                                                       mkU32(0x00000001)),
                                                 mkU8(0x01)))));

                     putIReg(rd, binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             mkexpr(t11), mkexpr(t8)),
                                       binop(Iop_8HLto16,
                                             mkexpr(t5), mkexpr(t2))));
                     break;
                  }
                  case 0x3: {  /* SUBUH_R.QB */
                     DIP("subuh_r.qb r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I32);
                     t8 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I8);
                     t10 = newTemp(Ity_I8);
                     t11 = newTemp(Ity_I8);
                     t12 = newTemp(Ity_I8);

                     /* Extract each byte of rs and rt. */
                     assign(t1, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32to16, getIReg(rs)))));
                     assign(t2, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32to16, getIReg(rs)))));
                     assign(t3, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32HIto16, getIReg(rs)))));
                     assign(t4, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32HIto16, getIReg(rs)))));

                     assign(t5, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32to16, getIReg(rt)))));
                     assign(t6, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32to16, getIReg(rt)))));
                     assign(t7, unop(Iop_8Uto32,
                                     unop(Iop_16to8,
                                          unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t8, unop(Iop_8Uto32,
                                     unop(Iop_16HIto8,
                                          unop(Iop_32HIto16, getIReg(rt)))));

                     /* Add 1 to each resulting byte and half the results. */
                     assign(t9, unop(Iop_16to8,
                                     unop(Iop_32to16,
                                          binop(Iop_Shr32,
                                                binop(Iop_Add32,
                                                      binop(Iop_Sub32,
                                                            mkexpr(t1),
                                                            mkexpr(t5)),
                                                      mkU32(0x00000001)),
                                                mkU8(0x01)))));
                     assign(t10, unop(Iop_16to8,
                                      unop(Iop_32to16,
                                           binop(Iop_Shr32,
                                                 binop(Iop_Add32,
                                                       binop(Iop_Sub32,
                                                             mkexpr(t2),
                                                             mkexpr(t6)),
                                                       mkU32(0x00000001)),
                                                 mkU8(0x01)))));
                     assign(t11, unop(Iop_16to8,
                                      unop(Iop_32to16,
                                            binop(Iop_Shr32,
                                                  binop(Iop_Add32,
                                                        binop(Iop_Sub32,
                                                              mkexpr(t3),
                                                              mkexpr(t7)),
                                                        mkU32(0x00000001)),
                                                  mkU8(0x01)))));
                     assign(t12, unop(Iop_16to8,
                                      unop(Iop_32to16,
                                           binop(Iop_Shr32,
                                                 binop(Iop_Add32,
                                                       binop(Iop_Sub32,
                                                             mkexpr(t4),
                                                             mkexpr(t8)),
                                                       mkU32(0x00000001)),
                                                 mkU8(0x01)))));

                     putIReg(rd, binop(Iop_16HLto32,
                                       binop(Iop_8HLto16,
                                             mkexpr(t12), mkexpr(t11)),
                                       binop(Iop_8HLto16,
                                             mkexpr(t10), mkexpr(t9))));
                     break;
                  }
                  case 0x8: {  /* ADDQH.PH */
                     DIP("addqh.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I16);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I16);

                     /* Add lower halfs of rs and rt
                        and right shift the result by 1. */
                     assign(t0, binop(Iop_Add32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));
                     assign(t1, unop(Iop_32to16,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 mkexpr(t0),
                                                 mkU32(0x0001fffe)),
                                           mkU8(0x1))));
                     /* Add higher halfs of rs and rt
                        and right shift the result by 1. */
                     assign(t2, binop(Iop_Add32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t3, unop(Iop_32to16,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 mkexpr(t2),
                                                 mkU32(0x0001fffe)),
                                           mkU8(0x1))));
                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t3), mkexpr(t1)));
                     break;
                  }
                  case 0x9: {  /* SUBQH.PH */
                     DIP("subqh.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);

                     putIReg(rd, binop(Iop_HSub16Sx2,
                                       getIReg(rs), getIReg(rt)));
                     break;
                  }
                  case 0xA: {/* ADDQH_R.PH */
                     DIP("addqh_r.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I16);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I16);

                     /* Add lower halfs of rs and rt, add 1
                        and right shift the result by 1. */
                     assign(t0, binop(Iop_Add32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));
                     assign(t1, unop(Iop_32to16,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 binop(Iop_Add32,
                                                       mkexpr(t0),
                                                       mkU32(0x1)),
                                                 mkU32(0x0001fffe)),
                                           mkU8(0x1))));
                     /* Add higher halfs of rs and rt, add 1
                        and right shift the result by 1. */
                     assign(t2, binop(Iop_Add32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t3, unop(Iop_32to16,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 binop(Iop_Add32,
                                                       mkexpr(t2),
                                                       mkU32(0x1)),
                                                 mkU32(0x0001fffe)),
                                           mkU8(0x1))));

                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t3), mkexpr(t1)));
                     break;
                  }
                  case 0xB: {  /* SUBQH_R.PH */
                     DIP("subqh_r.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I16);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I16);

                     /* Sub lower halfs of rs and rt, add 1
                        and right shift the result by 1. */
                     assign(t0, binop(Iop_Sub32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));
                     assign(t1, unop(Iop_32to16,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 binop(Iop_Add32,
                                                       mkexpr(t0),
                                                       mkU32(0x1)),
                                                 mkU32(0x0001fffe)),
                                           mkU8(0x1))));
                     /* Sub higher halfs of rs and rt, add 1
                        and right shift the result by 1. */
                     assign(t2, binop(Iop_Sub32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt)))));
                     assign(t3, unop(Iop_32to16,
                                     binop(Iop_Shr32,
                                           binop(Iop_And32,
                                                 binop(Iop_Add32,
                                                       mkexpr(t2),
                                                       mkU32(0x1)),
                                                 mkU32(0x0001fffe)),
                                           mkU8(0x1))));

                     putIReg(rd, binop(Iop_16HLto32, mkexpr(t3), mkexpr(t1)));
                     break;
                  }
                  case 0xC: {  /* MUL.PH */
                     DIP("mul.ph r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);

                     assign(t0,
                            binop(Iop_Mul32,
                                  unop(Iop_16Sto32,
                                       unop(Iop_32HIto16, getIReg(rs))),
                                  unop(Iop_16Sto32,
                                       unop(Iop_32HIto16, getIReg(rt)))));
                     /* DSP Control flag. */
                     putDSPControl(IRExpr_ITE(unop(Iop_Not1,
                                                   binop(Iop_CmpLE32S,
                                                         mkexpr(t0),
                                                         mkU32(0x7FFF))),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00200000)),
                                              IRExpr_ITE(binop(Iop_CmpLT32S,
                                                               mkexpr(t0),
                                                               mkU32(0xFFFF8000)
                                                             ),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00200000)
                                                              ),
                                                         getDSPControl())));

                     assign(t1,
                            binop(Iop_Mul32,
                                  unop(Iop_16Sto32,
                                       unop(Iop_32to16, getIReg(rs))),
                                  unop(Iop_16Sto32,
                                       unop(Iop_32to16, getIReg(rt)))));
                     /* DSP Control flag. */
                     putDSPControl(IRExpr_ITE(unop(Iop_Not1,
                                                   binop(Iop_CmpLE32S,
                                                         mkexpr(t1),
                                                         mkU32(0x7FFF))),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00200000)),
                                              IRExpr_ITE(binop(Iop_CmpLT32S,
                                                               mkexpr(t1),
                                                               mkU32(0xFFFF8000)
                                                              ),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00200000)
                                                              ),
                                                         getDSPControl())));

                     assign(t2, binop(Iop_16HLto32,
                                      unop(Iop_32to16, mkexpr(t0)),
                                      unop(Iop_32to16, mkexpr(t1))));
                     putIReg(rd, mkexpr(t2));
                     break;
                  }
                  case 0xE: {  /* MUL_S.PH */
                     DIP("mul_s.ph r%d r%d, r%d", rd, rs, rt);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);

                     /* t0 - signed intermediate result. */
                     assign(t0,
                           binop(Iop_Mul32,
                                 unop(Iop_16Sto32,
                                      unop(Iop_32HIto16, getIReg(rs))),
                                 unop(Iop_16Sto32,
                                      unop(Iop_32HIto16, getIReg(rt)))));

                     assign(t1,
                            IRExpr_ITE(unop(Iop_Not1,
                                            binop(Iop_CmpLE32S,
                                                  mkexpr(t0),
                                                  mkU32(0x7FFF))),
                                       mkU32(0x00007FFF),
                                       IRExpr_ITE(binop(Iop_CmpLT32S,
                                                        mkexpr(t0),
                                                        mkU32(0xFFFF8000)),
                                                  mkU32(0xFFFF8000),
                                                  mkexpr(t0))));

                     /* DSP Control flag. */
                     putDSPControl(IRExpr_ITE(unop(Iop_Not1,
                                                   binop(Iop_CmpLE32S,
                                                         mkexpr(t0),
                                                         mkU32(0x7FFF))),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00200000)),
                                              IRExpr_ITE(binop(Iop_CmpLT32S,
                                                               mkexpr(t0),
                                                               mkU32(0xFFFF8000)
                                                              ),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00200000)
                                                              ),
                                                         getDSPControl())));

                     /* t2 - signed intermediate result. */
                     assign(t2, binop(Iop_Mul32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt)))));

                     assign(t3, IRExpr_ITE(unop(Iop_Not1,
                                                binop(Iop_CmpLE32S,
                                                      mkexpr(t2),
                                                      mkU32(0x7FFF))),
                                           mkU32(0x00007FFF),
                                           IRExpr_ITE(binop(Iop_CmpLT32S,
                                                            mkexpr(t2),
                                                            mkU32(0xFFFF8000)),
                                                      mkU32(0xFFFF8000),
                                                      mkexpr(t2))));

                     /* DSP Control flag. */
                     putDSPControl(IRExpr_ITE(unop(Iop_Not1,
                                                   binop(Iop_CmpLE32S,
                                                         mkexpr(t2),
                                                         mkU32(0x7FFF))),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    mkU32(0x00200000)),
                                              IRExpr_ITE(binop(Iop_CmpLT32S,
                                                               mkexpr(t2),
                                                               mkU32(0xFFFF8000)
                                                              ),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00200000)
                                                              ),
                                                         getDSPControl())));

                     assign(t4, binop(Iop_16HLto32,
                                      unop(Iop_32to16, mkexpr(t1)),
                                      unop(Iop_32to16, mkexpr(t3))));
                     putIReg(rd, mkexpr(t4));
                     break;
                  }
                  case 0x10: {  /* ADDQH.W */
                     DIP("addqh.w r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);

                     assign(t0, binop(Iop_Add64,
                                      unop(Iop_32Sto64, getIReg(rs)),
                                      unop(Iop_32Sto64, getIReg(rt))));
                     assign(t1, binop(Iop_And64,
                                      mkexpr(t0),
                                      mkU64(0x00000001fffffffeULL)));
                     putIReg(rd, unop(Iop_64to32,
                                      binop(Iop_Shr64, mkexpr(t1), mkU8(0x1))));
                     break;
                  }
                  case 0x11: {  /* SUBQH.W */
                     DIP("subqh.w r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);

                     assign(t0, binop(Iop_Sub64,
                                      unop(Iop_32Sto64, getIReg(rs)),
                                      unop(Iop_32Sto64, getIReg(rt))));
                     assign(t1, binop(Iop_And64,
                                      mkexpr(t0),
                                      mkU64(0x00000001fffffffeULL)));
                     putIReg(rd, unop(Iop_64to32,
                                      binop(Iop_Shr64, mkexpr(t1), mkU8(0x1))));
                     break;
                  }
                  case 0x12: {  /* ADDQH_R.W */
                     DIP("addqh_r.w r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I64);

                     assign(t0, binop(Iop_Add64,
                                      unop(Iop_32Sto64, getIReg(rs)),
                                      unop(Iop_32Sto64, getIReg(rt))));
                     assign(t1, binop(Iop_Add64,
                                      mkexpr(t0),
                                      mkU64(0x0000000000000001ULL)));
                     assign(t2, binop(Iop_And64,
                                      mkexpr(t1),
                                      mkU64(0x00000001fffffffeULL)));
                     putIReg(rd, unop(Iop_64to32,
                                      binop(Iop_Shr64, mkexpr(t2), mkU8(0x1))));
                     break;
                  }
                  case 0x13: {  /* SUBQH_R.W */
                     DIP("subqh_r.w r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I64);

                     assign(t0, binop(Iop_Sub64,
                                      unop(Iop_32Sto64, getIReg(rs)),
                                      unop(Iop_32Sto64, getIReg(rt))));
                     assign(t1, binop(Iop_Add64,
                                      mkexpr(t0),
                                      mkU64(0x0000000000000001ULL)));
                     assign(t2, binop(Iop_And64,
                                      mkexpr(t1),
                                      mkU64(0x00000001fffffffeULL)));
                     putIReg(rd, unop(Iop_64to32,
                                      binop(Iop_Shr64, mkexpr(t2), mkU8(0x1))));
                     break;
                  }
                  case 0x16: {  /* MULQ_S.W */
                     DIP("mulq_s.w r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);

                     assign(t0, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            getIReg(rt), getIReg(rs)),
                                      mkU8(0x1)));
                     assign(t1, binop(Iop_CmpEQ32,
                                      getIReg(rt), mkU32(0x80000000)));
                     assign(t2, binop(Iop_CmpEQ32,
                                      getIReg(rs), mkU32(0x80000000)));

                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              IRExpr_ITE(mkexpr(t2),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00200000)
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));
                     putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                            IRExpr_ITE(mkexpr(t2),
                                                       mkU32(0x7fffffff),
                                                       unop(Iop_64HIto32,
                                                            mkexpr(t0))),
                                            unop(Iop_64HIto32, mkexpr(t0))));
                     break;
                  }
                  case 0x17: {  /* MULQ_RS.W */
                     DIP("mulq_rs.w r%d, r%d, r%d", rd, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I1);

                     assign(t0, binop(Iop_Add64,
                                      binop(Iop_Shl64,
                                            binop(Iop_MullS32,
                                                  getIReg(rt),
                                                  getIReg(rs)),
                                            mkU8(0x1)),
                                      mkU64(0x0000000080000000ULL)));
                     assign(t1,
                            binop(Iop_CmpEQ32, getIReg(rt), mkU32(0x80000000)));
                     assign(t2,
                            binop(Iop_CmpEQ32, getIReg(rs), mkU32(0x80000000)));
                     putDSPControl(IRExpr_ITE(mkexpr(t1),
                                              IRExpr_ITE(mkexpr(t2),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               mkU32(0x00200000)
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));
                     putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                            IRExpr_ITE(mkexpr(t2),
                                                       mkU32(0x7fffffff),
                                                       unop(Iop_64HIto32,
                                                            mkexpr(t0))),
                                            unop(Iop_64HIto32, mkexpr(t0))));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of ADDUH.QB/MUL.PH */
            }
            case 0x30: {  /* DPAQ.W.PH */
               switch(sa) {
                  case 0x0: {  /* DPA.W.PH */
                     DIP("dpa.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I64);

                     assign(t0,
                            unop(Iop_32Sto64,
                                 binop(Iop_Mul32,
                                       unop(Iop_16Sto32,
                                            unop(Iop_32HIto16, getIReg(rs))),
                                       unop(Iop_16Sto32,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t1,
                            unop(Iop_32Sto64,
                                 binop(Iop_Mul32,
                                       unop(Iop_16Sto32,
                                            unop(Iop_32to16, getIReg(rs))),
                                       unop(Iop_16Sto32,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t2,
                            binop(Iop_Add64,
                                  getAcc(ac),
                                  binop(Iop_Add64, mkexpr(t0), mkexpr(t1))));
                     putAcc(ac, mkexpr(t2));
                     break;
                  }
                  case 0x1: {  /* DPS.W.PH */
                     DIP("dps.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I64);

                     assign(t0,
                            unop(Iop_32Sto64,
                                 binop(Iop_Mul32,
                                       unop(Iop_16Sto32,
                                            unop(Iop_32HIto16, getIReg(rs))),
                                       unop(Iop_16Sto32,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t1,
                            unop(Iop_32Sto64,
                                 binop(Iop_Mul32,
                                       unop(Iop_16Sto32,
                                            unop(Iop_32to16, getIReg(rs))),
                                       unop(Iop_16Sto32,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t2,
                            binop(Iop_Sub64,
                                  getAcc(ac),
                                  binop(Iop_Add64, mkexpr(t0), mkexpr(t1))));
                     putAcc(ac, mkexpr(t2));
                     break;
                  }
                  case 0x2: {  /* MULSA.W.PH */
                     DIP("mulsa.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);

                     assign(t4, getAcc(ac));
                     assign(t0, binop(Iop_Mul32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32to16, getIReg(rs)))));
                     assign(t1, binop(Iop_Mul32,
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      unop(Iop_16Sto32,
                                           unop(Iop_32HIto16, getIReg(rs)))));
                     assign(t2, binop(Iop_Sub32, mkexpr(t1), mkexpr(t0)));
                     putAcc(ac, binop(Iop_Add64,
                                      mkexpr(t4),
                                      unop(Iop_32Sto64, mkexpr(t2))));
                     break;
                  }
                  case 0x3: {  /* DPAU.H.QBL */
                     DIP("dpau.h.qbl ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I64);
                     t3 = newTemp(Ity_I64);

                     assign(t0,
                            binop(Iop_Mul32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t1,
                            binop(Iop_Mul32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t2,
                            unop(Iop_32Uto64,
                                 binop(Iop_Add32,
                                       mkexpr(t0),
                                       mkexpr(t1))));
                     assign(t3,
                            binop(Iop_Add64, getAcc(ac), mkexpr(t2)));
                     putAcc(ac, mkexpr(t3));
                     break;
                  }
                  case 0x4: {  /* DPAQ_S.W.PH */
                     DIP("dpaq_s.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);

                     assign(t0, getAcc(ac));

                     assign(t1, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rs))),
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rt)))),
                                      mkU8(0x1)));
                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      mkU32(0x00008000)));
                     assign(t4,
                            IRExpr_ITE(mkexpr(t2),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t1)),
                                       mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t5, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32to16, getIReg(rs))),
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32to16, getIReg(rt)))
                                           ),
                                      mkU8(0x1)));
                     assign(t6, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t7, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      mkU32(0x00008000)));
                     assign(t8,
                            IRExpr_ITE(mkexpr(t6),
                                       IRExpr_ITE(mkexpr(t7),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t5)),
                                       mkexpr(t5)));

                     putDSPControl(IRExpr_ITE(mkexpr(t6),
                                              IRExpr_ITE(mkexpr(t7),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t9, binop(Iop_Add64,
                                      binop(Iop_Add64, mkexpr(t4), mkexpr(t8)),
                                      mkexpr(t0)));
                     putAcc(ac, mkexpr(t9));
                     break;
                  }
                  case 0x5: {  /* DPSQ_S.W.PH */
                     DIP("dpsq_s.w.ph ac%d r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);

                     assign(t0, getAcc(ac));

                     assign(t1, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rs))),
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rt)))),
                                      mkU8(0x1)));
                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      mkU32(0x00008000)));
                     assign(t4,
                            IRExpr_ITE(mkexpr(t2),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t1)),
                                       mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t5,
                            binop(Iop_Shl64,
                                  binop(Iop_MullS32,
                                        unop(Iop_16Sto32,
                                             unop(Iop_32to16, getIReg(rs))),
                                        unop(Iop_16Sto32,
                                             unop(Iop_32to16, getIReg(rt)))),
                                  mkU8(0x1)));
                     assign(t6, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t7, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      mkU32(0x00008000)));
                     assign(t8,
                            IRExpr_ITE(mkexpr(t6),
                                       IRExpr_ITE(mkexpr(t7),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t5)),
                                       mkexpr(t5)));

                     putDSPControl(IRExpr_ITE(mkexpr(t6),
                                              IRExpr_ITE(mkexpr(t7),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t9,
                            binop(Iop_Sub64,
                                  mkexpr(t0),
                                  binop(Iop_Add64, mkexpr(t4), mkexpr(t8))));
                     putAcc(ac, mkexpr(t9));
                     break;
                  }
                  case 0x6: {  /* MULSAQ_S.W.PH */
                     DIP("mulsaq_s.w.ph ac%d r%d, r%d", ac, rs, rt);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I32);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I64);
                     t8 = newTemp(Ity_I32);
                     t9 = newTemp(Ity_I32);

                     assign(t0, unop(Iop_16Sto32,
                                     unop(Iop_32HIto16, getIReg(rs))));
                     assign(t1, unop(Iop_16Sto32,
                                     unop(Iop_32HIto16, getIReg(rt))));

                     assign(t8, binop(Iop_And32,
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 unop(Iop_16Uto32,
                                                      unop(Iop_32HIto16,
                                                           getIReg(rs))),
                                                 mkU32(0x8000))),
                                    unop(Iop_1Sto32,
                                         binop(Iop_CmpEQ32,
                                               unop(Iop_16Uto32,
                                                    unop(Iop_32HIto16,
                                                         getIReg(rt))),
                                               mkU32(0x8000)))));
                     /* DSPControl_outflag:16+acc <- 1 */
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t8),
                                                    mkU32(0x0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x00010000),
                                                          mkU8(ac))),
                                              getDSPControl()));

                     /* tempB_31..0 */
                     assign(t2,
                            IRExpr_ITE(binop(Iop_CmpNE32,
                                             mkexpr(t8), mkU32(0x0)),
                                       mkU32(0x7FFFFFFF),
                                       binop(Iop_Shl32,
                                             binop(Iop_Mul32,
                                                   mkexpr(t0), mkexpr(t1)),
                                             mkU8(1))));

                     assign(t3, unop(Iop_16Sto32,
                                     unop(Iop_32to16, getIReg(rs))));
                     assign(t4, unop(Iop_16Sto32,
                                     unop(Iop_32to16, getIReg(rt))));

                     assign(t9, binop(Iop_And32,
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 unop(Iop_16Uto32,
                                                      unop(Iop_32to16,
                                                           getIReg(rs))),
                                                 mkU32(0x8000))),
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 unop(Iop_16Uto32,
                                                      unop(Iop_32to16,
                                                           getIReg(rt))),
                                                 mkU32(0x8000)))));
                     /* DSPControl_outflag:16+acc <- 1 */
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    mkexpr(t9),
                                                    mkU32(0x0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x00010000),
                                                          mkU8(ac))),
                                              getDSPControl()));
                     /* tempA_31..0 */
                     assign(t5,
                            IRExpr_ITE(binop(Iop_CmpNE32,
                                             mkexpr(t9),
                                             mkU32(0x0)),
                                       mkU32(0x7FFFFFFF),
                                       binop(Iop_Shl32,
                                             binop(Iop_Mul32,
                                                   mkexpr(t3),
                                                   mkexpr(t4)),
                                             mkU8(1))));
                     /* dotp_63..0 */
                     assign(t6,
                            binop(Iop_Sub64,
                                  unop(Iop_32Sto64, mkexpr(t2)),
                                  unop(Iop_32Sto64, mkexpr(t5))));
                     /* tempC_63..0 */
                     assign(t7, binop(Iop_Add64, getAcc(ac), mkexpr(t6)));

                     putAcc(ac, mkexpr(t7));
                     break;
                  }
                  case 0x7: {  /* DPAU.H.QBR */
                     DIP("dpau.h.qbr ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I64);
                     t3 = newTemp(Ity_I64);

                     assign(t0,
                            binop(Iop_Mul32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t1,
                            binop(Iop_Mul32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t2, unop(Iop_32Uto64,
                                     binop(Iop_Add32, mkexpr(t0), mkexpr(t1))));
                     assign(t3, binop(Iop_Add64, getAcc(ac), mkexpr(t2)));
                     putAcc(ac, mkexpr(t3));
                     break;
                  }
                  case 0x8: {  /* DPAX.W.PH */
                     DIP("dpax.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I64);

                     assign(t0,
                            unop(Iop_32Sto64,
                                 binop(Iop_Mul32,
                                       unop(Iop_16Sto32,
                                            unop(Iop_32HIto16, getIReg(rs))),
                                       unop(Iop_16Sto32,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t1,
                            unop(Iop_32Sto64,
                                 binop(Iop_Mul32,
                                       unop(Iop_16Sto32,
                                            unop(Iop_32to16, getIReg(rs))),
                                       unop(Iop_16Sto32,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t2,
                            binop(Iop_Add64,
                                  getAcc(ac),
                                  binop(Iop_Add64, mkexpr(t0), mkexpr(t1))));
                     putAcc(ac, mkexpr(t2));
                     break;
                  }
                  case 0x9: {  /* DPSX.W.PH */
                     DIP("dpsx.w.ph ac%d r%d, r%d", ac, rs, rt);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I64);

                     assign(t0,
                            unop(Iop_32Sto64,
                                 binop(Iop_Mul32,
                                       unop(Iop_16Sto32,
                                            unop(Iop_32HIto16, getIReg(rs))),
                                       unop(Iop_16Sto32,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t1,
                            unop(Iop_32Sto64,
                                 binop(Iop_Mul32,
                                       unop(Iop_16Sto32,
                                            unop(Iop_32to16, getIReg(rs))),
                                       unop(Iop_16Sto32,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t2,
                            binop(Iop_Sub64,
                                  getAcc(ac),
                                  binop(Iop_Add64, mkexpr(t0), mkexpr(t1))));
                     putAcc(ac, mkexpr(t2));
                     break;
                  }
                  case 0xB: {  /* DPSU.H.QBL */
                     DIP("dpsu.h.qbl ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I64);
                     t3 = newTemp(Ity_I64);

                     assign(t0,
                            binop(Iop_Mul32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t1,
                            binop(Iop_Mul32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32HIto16, getIReg(rt))))));
                     assign(t2,
                            unop(Iop_32Uto64,
                                 binop(Iop_Add32, mkexpr(t0), mkexpr(t1))));
                     assign(t3,
                            binop(Iop_Sub64, getAcc(ac), mkexpr(t2)));
                     putAcc(ac, mkexpr(t3));
                     break;
                  }
                  case 0xC: {  /* DPAQ_SA.L.W */
                     DIP("dpaq_sa.l.w ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I64);
                     t8 = newTemp(Ity_I1);
                     t9 = newTemp(Ity_I1);

                     assign(t0, getAcc(ac));

                     assign(t1, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            getIReg(rs), getIReg(rt)),
                                      mkU8(0x1)));

                     assign(t2, binop(Iop_CmpEQ32,
                                      getIReg(rs),
                                      mkU32(0x80000000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      getIReg(rt),
                                      mkU32(0x80000000)));

                     assign(t4,
                            IRExpr_ITE(mkexpr(t2),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU64(0x7fffffffffffffffULL),
                                                  mkexpr(t1)),
                                       mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t5, binop(Iop_Add64,
                                      unop(Iop_32Uto64,
                                           unop(Iop_64to32, mkexpr(t0))),
                                      unop(Iop_32Uto64,
                                           unop(Iop_64to32, mkexpr(t4)))));
                     assign(t6,
                            binop(Iop_Add64,
                                  binop(Iop_Add64,
                                        unop(Iop_32Sto64,
                                             unop(Iop_64HIto32, mkexpr(t0))),
                                        unop(Iop_32Sto64,
                                             unop(Iop_64HIto32, mkexpr(t4)))),
                                  unop(Iop_32Uto64,
                                       binop(Iop_And32,
                                             unop(Iop_64HIto32, mkexpr(t5)),
                                             mkU32(0x1)))));
                     assign(t7, binop(Iop_32HLto64,
                                      unop(Iop_64to32, mkexpr(t6)),
                                      unop(Iop_64to32, mkexpr(t5))));
                     assign(t8, binop(Iop_CmpEQ32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  unop(Iop_64to32, mkexpr(t6)),
                                                  mkU32(0x80000000)),
                                            mkU8(31)),
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32, mkexpr(t6)),
                                            mkU32(0x00000001))));
                     assign(t9, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32,
                                                 mkexpr(t6)),
                                            mkU32(0x00000001)),
                                      mkU32(0x1)));
                     putDSPControl(IRExpr_ITE(mkexpr(t8),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16)))));
                     putAcc(ac,
                            IRExpr_ITE(mkexpr(t8),
                                       mkexpr(t7),
                                       IRExpr_ITE(mkexpr(t9),
                                                  mkU64(0x8000000000000000ULL),
                                                  mkU64(0x7fffffffffffffffULL)))
                           );
                     break;
                  }
                  case 0xD: {  /* DPSQ_SA.L.W */
                     DIP("dpsq_sa.l.w ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I64);
                     t7 = newTemp(Ity_I64);
                     t8 = newTemp(Ity_I1);
                     t9 = newTemp(Ity_I1);

                     assign(t0, getAcc(ac));

                     assign(t1, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            getIReg(rs), getIReg(rt)),
                                      mkU8(0x1)));

                     assign(t2, binop(Iop_CmpEQ32,
                                      getIReg(rs),
                                      mkU32(0x80000000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      getIReg(rt),
                                      mkU32(0x80000000)));

                     assign(t4,
                            IRExpr_ITE(mkexpr(t2),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU64(0x7fffffffffffffffULL),
                                                  mkexpr(t1)),
                                       mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t5, binop(Iop_Sub64,
                                      unop(Iop_32Uto64,
                                           unop(Iop_64to32, mkexpr(t0))),
                                      unop(Iop_32Uto64,
                                           unop(Iop_64to32, mkexpr(t4)))));
                     assign(t6, binop(Iop_Sub64,
                                      binop(Iop_Add64,
                                            unop(Iop_32Sto64,
                                                 unop(Iop_64HIto32, mkexpr(t0))
                                                ),
                                            unop(Iop_32Sto64,
                                                 unop(Iop_1Sto32,
                                                      binop(Iop_CmpLT32U,
                                                            unop(Iop_64to32,
                                                                 mkexpr(t0)),
                                                            unop(Iop_64to32,
                                                                mkexpr(t4)))))),
                                      unop(Iop_32Sto64,
                                           unop(Iop_64HIto32, mkexpr(t4)))));
                     assign(t7, binop(Iop_32HLto64,
                                      unop(Iop_64to32, mkexpr(t6)),
                                      unop(Iop_64to32, mkexpr(t5))));
                     assign(t8, binop(Iop_CmpEQ32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  unop(Iop_64to32, mkexpr(t6)),
                                                  mkU32(0x80000000)),
                                            mkU8(31)),
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32, mkexpr(t6)),
                                            mkU32(0x00000001))));
                     assign(t9, binop(Iop_CmpEQ32,
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32, mkexpr(t6)),
                                            mkU32(0x00000001)),
                                      mkU32(0x1)));
                     putDSPControl(IRExpr_ITE(mkexpr(t8),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16)))));
                     putAcc(ac,
                            IRExpr_ITE(mkexpr(t8),
                                       mkexpr(t7),
                                       IRExpr_ITE(mkexpr(t9),
                                                  mkU64(0x8000000000000000ULL),
                                                  mkU64(0x7fffffffffffffffULL)))
                           );
                     break;
                  }
                  case 0xF: {  /* DPSU.H.QBR */
                     DIP("dpsu.h.qbr ac%d r%d, r%d", ac, rs, rt);
                     vassert(!mode64);

                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I64);
                     t3 = newTemp(Ity_I64);

                     assign(t0,
                            binop(Iop_Mul32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16HIto8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t1,
                            binop(Iop_Mul32,
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rs)))),
                                  unop(Iop_8Uto32,
                                       unop(Iop_16to8,
                                            unop(Iop_32to16, getIReg(rt))))));
                     assign(t2, unop(Iop_32Uto64,
                                     binop(Iop_Add32, mkexpr(t0), mkexpr(t1))));
                     assign(t3, binop(Iop_Sub64, getAcc(ac), mkexpr(t2)));
                     putAcc(ac, mkexpr(t3));

                     break;
                  }
                  case 0x10: {  /* MAQ_SA.W.PHL */
                     DIP("maq_sa.w.phl ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I64);

                     assign(t0, getAcc(ac));
                     assign(t1, unop(Iop_32Sto64,
                                     binop(Iop_Shl32,
                                           binop(Iop_Mul32,
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32HIto16,
                                                           getIReg(rs))),
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32HIto16,
                                                           getIReg(rt)))),
                                           mkU8(0x1))));

                     /* If both input arguments are equal 0x8000, saturate
                        intermediate product and write to DSPControl register.
                     */
                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      mkU32(0x00008000)));

                     assign(t4,
                            IRExpr_ITE(mkexpr(t2),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t1)),
                                       mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));
                     /* Add intermediate product and value in the
                        accumulator. */
                     assign(t5, binop(Iop_Add64, mkexpr(t0), mkexpr(t4)));

                     /* Compare bits 31 and 32 of the value in t5. */
                     assign(t6, binop(Iop_CmpEQ32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  unop(Iop_64to32, mkexpr(t5)),
                                                  mkU32(0x80000000)),
                                            mkU8(31)),
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32, mkexpr(t5)),
                                            mkU32(1))));
                     putDSPControl(IRExpr_ITE(mkexpr(t6),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16)))));
                     assign(t7,
                            IRExpr_ITE(mkexpr(t6),
                                       mkexpr(t5),
                                       IRExpr_ITE(binop(Iop_CmpEQ32,
                                                        binop(Iop_And32,
                                                              unop(Iop_64HIto32,
                                                                   mkexpr(t5)),
                                                              mkU32(1)),
                                                        mkU32(0x0)),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkU64(0xffffffff80000000ULL)))
                           );
                     putAcc(ac, mkexpr(t7));
                     break;
                  }
                  case 0x12: {  /* MAQ_SA.W.PHR */
                     DIP("maq_sa.w.phr ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I64);

                     assign(t0, getAcc(ac));
                     assign(t1, unop(Iop_32Sto64,
                                     binop(Iop_Shl32,
                                           binop(Iop_Mul32,
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32to16,
                                                           getIReg(rs))),
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32to16,
                                                           getIReg(rt)))),
                                           mkU8(0x1))));

                     /* If both input arguments are equal 0x8000, saturate
                        intermediate product and write to DSPControl
                        register. */
                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      mkU32(0x00008000)));

                     assign(t4,
                            IRExpr_ITE(mkexpr(t2),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t1)),
                                       mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));
                     /* Add intermediate product and value in the
                        accumulator. */
                     assign(t5, binop(Iop_Add64, mkexpr(t0), mkexpr(t4)));

                     /* Compare bits 31 and 32 of the value in t5. */
                     assign(t6, binop(Iop_CmpEQ32,
                                      binop(Iop_Shr32,
                                            binop(Iop_And32,
                                                  unop(Iop_64to32, mkexpr(t5)),
                                                  mkU32(0x80000000)),
                                            mkU8(31)),
                                      binop(Iop_And32,
                                            unop(Iop_64HIto32, mkexpr(t5)),
                                            mkU32(1))));
                     putDSPControl(IRExpr_ITE(mkexpr(t6),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16)))));
                     assign(t7,
                            IRExpr_ITE(mkexpr(t6),
                                       mkexpr(t5),
                                       IRExpr_ITE(binop(Iop_CmpEQ32,
                                                        binop(Iop_And32,
                                                              unop(Iop_64HIto32,
                                                                   mkexpr(t5)),
                                                              mkU32(1)),
                                                        mkU32(0x0)),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkU64(0xffffffff80000000ULL)))
                           );
                     putAcc(ac, mkexpr(t7));
                     break;
                  }
                  case 0x14: {  /* MAQ_S.W.PHL */
                     DIP("maq_s.w.phl ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I64);

                     assign(t5, getAcc(ac));

                     assign(t0, unop(Iop_16Sto32,
                                     unop(Iop_32HIto16, getIReg(rs))));
                     assign(t1, unop(Iop_16Sto32,
                                     unop(Iop_32HIto16, getIReg(rt))));

                     assign(t2, binop(Iop_And32,
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       mkexpr(t0),
                                                       mkU32(0xffff)),
                                                 mkU32(0x8000))),
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       mkexpr(t1),
                                                       mkU32(0xffff)),
                                                 mkU32(0x8000)))));

                     assign(t3, binop(Iop_CmpEQ32, mkexpr(t2), mkU32(0x0)));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16)))));

                     assign(t4, unop(Iop_64to32,
                                     binop(Iop_MullS32,
                                           mkexpr(t0), mkexpr(t1))));
                     putAcc(ac, IRExpr_ITE(mkexpr(t3),
                                           binop(Iop_Add64,
                                                 unop(Iop_32Sto64,
                                                      binop(Iop_Shl32,
                                                            mkexpr(t4),
                                                            mkU8(0x1))),
                                                 mkexpr(t5)),
                                           binop(Iop_Add64,
                                                 mkexpr(t5),
                                                 unop(Iop_32Sto64,
                                                      mkU32(0x7fffffff)))));
                     break;
                  }
                  case 0x16: {  /* MAQ_S.W.PHR */
                     DIP("maq_s.w.phr ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I64);

                     assign(t5, getAcc(ac));

                     assign(t0, unop(Iop_16Sto32,
                                     unop(Iop_32to16, getIReg(rs))));
                     assign(t1, unop(Iop_16Sto32,
                                     unop(Iop_32to16, getIReg(rt))));

                     assign(t2, binop(Iop_And32,
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       mkexpr(t0),
                                                       mkU32(0xffff)),
                                                 mkU32(0x8000))),
                                      unop(Iop_1Sto32,
                                           binop(Iop_CmpEQ32,
                                                 binop(Iop_And32,
                                                       mkexpr(t1),
                                                       mkU32(0xffff)),
                                                 mkU32(0x8000)))));

                     assign(t3, binop(Iop_CmpEQ32, mkexpr(t2), mkU32(0x0)));

                     putDSPControl(IRExpr_ITE(mkexpr(t3),
                                              getDSPControl(),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16)))));

                     assign(t4, unop(Iop_64to32,
                                     binop(Iop_MullS32,
                                           mkexpr(t0), mkexpr(t1))));
                     putAcc(ac, IRExpr_ITE(mkexpr(t3),
                                           binop(Iop_Add64,
                                                 unop(Iop_32Sto64,
                                                      binop(Iop_Shl32,
                                                            mkexpr(t4),
                                                            mkU8(0x1))),
                                                 mkexpr(t5)),
                                           binop(Iop_Add64,
                                                 mkexpr(t5),
                                                 unop(Iop_32Sto64,
                                                      mkU32(0x7fffffff)))));
                     break;
                  }
                  case 0x18: {  /* DPAQX_S.W.PH */
                     DIP("dpaqx_s.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);

                     assign(t0, getAcc(ac));

                     assign(t1, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rs))),
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32to16,
                                                      getIReg(rt)))),
                                      mkU8(0x1)));
                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      mkU32(0x00008000)));
                     assign(t4,
                            IRExpr_ITE(mkexpr(t2),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t1)),
                                       mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                  mkU32(0x1),
                                                                  mkU8(ac+16))),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t5, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32to16,
                                                      getIReg(rs))),
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rt)))),
                                      mkU8(0x1)));
                     assign(t6, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t7, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      mkU32(0x00008000)));
                     assign(t8,
                            IRExpr_ITE(mkexpr(t6),
                                       IRExpr_ITE(mkexpr(t7),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t5)),
                                       mkexpr(t5)));

                     putDSPControl(IRExpr_ITE(mkexpr(t6),
                                              IRExpr_ITE(mkexpr(t7),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t9, binop(Iop_Add64,
                                      binop(Iop_Add64, mkexpr(t4), mkexpr(t8)),
                                      mkexpr(t0)));
                     putAcc(ac, mkexpr(t9));
                     break;
                  }
                  case 0x19: {  /* DPSQX_S.W.PH */
                     DIP("dpsqx_s.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);

                     assign(t0, getAcc(ac));

                     assign(t1, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rs))),
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32to16,
                                                      getIReg(rt)))),
                                      mkU8(0x1)));
                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      mkU32(0x00008000)));
                     assign(t4,
                            IRExpr_ITE(mkexpr(t2),
                                       IRExpr_ITE(mkexpr(t3),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t1)),
                                       mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(mkexpr(t2),
                                              IRExpr_ITE(mkexpr(t3),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t5, binop(Iop_Shl64,
                                      binop(Iop_MullS32,
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32to16,
                                                      getIReg(rs))),
                                            unop(Iop_16Sto32,
                                                 unop(Iop_32HIto16,
                                                      getIReg(rt)))),
                                      mkU8(0x1)));
                     assign(t6, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t7, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      mkU32(0x00008000)));
                     assign(t8,
                            IRExpr_ITE(mkexpr(t6),
                                       IRExpr_ITE(mkexpr(t7),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t5)),
                                       mkexpr(t5)));

                     putDSPControl(IRExpr_ITE(mkexpr(t6),
                                              IRExpr_ITE(mkexpr(t7),
                                                         binop(Iop_Or32,
                                                               getDSPControl(),
                                                               binop(Iop_Shl32,
                                                                     mkU32(0x1),
                                                                     mkU8(ac+16)
                                                                    )
                                                              ),
                                                         getDSPControl()),
                                              getDSPControl()));

                     assign(t9, binop(Iop_Sub64,
                                     mkexpr(t0),
                                     binop(Iop_Add64, mkexpr(t4), mkexpr(t8))));
                     putAcc(ac, mkexpr(t9));
                     break;
                  }
                  case 0x1A: {  /* DPAQX_SA.W.PH */
                     DIP("dpaqx_sa.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);
                     t10 = newTemp(Ity_I32);

                     assign(t0, getAcc(ac));
                     /* Calculate the first cross dot product and saturate if
                        needed. */
                     assign(t1, unop(Iop_32Sto64,
                                     binop(Iop_Shl32,
                                           binop(Iop_Mul32,
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32HIto16,
                                                           getIReg(rs))),
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32to16,
                                                           getIReg(rt)))),
                                           mkU8(0x1))));

                     /* If both input arguments are equal 0x8000, saturate
                        intermediate product and write to DSPControl
                        register. */
                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      mkU32(0x00008000)));

                     assign(t4, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       unop(Iop_1Sto32,
                                                            mkexpr(t2)),
                                                       unop(Iop_1Sto32,
                                                            mkexpr(t3))),
                                                 mkU32(0)),
                                           mkU64(0x000000007fffffffULL),
                                           mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    binop(Iop_And32,
                                                          unop(Iop_1Sto32,
                                                               mkexpr(t2)),
                                                          unop(Iop_1Sto32,
                                                               mkexpr(t3))),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16))),
                                              getDSPControl()));
                     /* Calculate second cross dot product and saturate if
                        needed. */
                     assign(t5, unop(Iop_32Sto64,
                                     binop(Iop_Shl32,
                                           binop(Iop_Mul32,
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32to16,
                                                           getIReg(rs))),
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32HIto16,
                                                           getIReg(rt)))),
                                           mkU8(0x1))));

                     /* If both input arguments are equal 0x8000, saturate
                        intermediate product and write to DSPControl
                        register. */
                     assign(t6, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t7, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      mkU32(0x00008000)));

                     assign(t8, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       unop(Iop_1Sto32,
                                                            mkexpr(t6)),
                                                       unop(Iop_1Sto32,
                                                            mkexpr(t7))),
                                                 mkU32(0)),
                                           mkU64(0x000000007fffffffULL),
                                           mkexpr(t5)));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    binop(Iop_And32,
                                                          unop(Iop_1Sto32,
                                                               mkexpr(t6)),
                                                          unop(Iop_1Sto32,
                                                               mkexpr(t7))),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16))),
                                              getDSPControl()));
                     /* Subtract intermediate products from value in the
                        accumulator. */
                     assign(t9,
                            binop(Iop_Add64,
                                  mkexpr(t0),
                                  binop(Iop_Add64, mkexpr(t8), mkexpr(t4))));

                     putAcc(ac,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   unop(Iop_64HIto32,
                                                        mkexpr(t9)),
                                                   mkU32(0x80000000)),
                                             mkU32(0x0)),
                                       IRExpr_ITE(binop(Iop_CmpNE32,
                                                        unop(Iop_64HIto32,
                                                             binop(Iop_Shl64,
                                                                   mkexpr(t9),
                                                                   mkU8(1))),
                                                        mkU32(0x0)),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t9)),
                                       IRExpr_ITE(binop(Iop_CmpNE32,
                                                        unop(Iop_64HIto32,
                                                             binop(Iop_Shl64,
                                                                   mkexpr(t9),
                                                                   mkU8(1))),
                                                        mkU32(0xffffffff)),
                                                  mkU64(0xffffffff80000000ULL),
                                                  mkexpr(t9))));
                     assign(t10, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                  unop(Iop_64to32,
                                                       mkexpr(t9)),
                                                  unop(Iop_64to32,
                                                       getAcc(ac))),
                                           getDSPControl(),
                                           binop(Iop_Or32,
                                                 getDSPControl(),
                                                 binop(Iop_Shl32,
                                                       mkU32(0x1),
                                                       mkU8(ac+16)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    unop(Iop_64HIto32,
                                                         mkexpr(t9)),
                                                    unop(Iop_64HIto32,
                                                         getAcc(ac))),
                                              mkexpr(t10),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16)))));
                     break;
                  }
                  case 0x1B: {  /* DPSQX_SA.W.PH */
                     DIP("dpsqx_sa.w.ph ac%d, r%d, r%d", ac, rs, rt);
                     vassert(!mode64);
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I64);
                     t2 = newTemp(Ity_I1);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_I64);
                     t5 = newTemp(Ity_I64);
                     t6 = newTemp(Ity_I1);
                     t7 = newTemp(Ity_I1);
                     t8 = newTemp(Ity_I64);
                     t9 = newTemp(Ity_I64);
                     t10 = newTemp(Ity_I32);

                     assign(t0, getAcc(ac));
                     /* Calculate the first cross dot product and saturate if
                        needed. */
                     assign(t1, unop(Iop_32Sto64,
                                     binop(Iop_Shl32,
                                           binop(Iop_Mul32,
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32HIto16,
                                                           getIReg(rs))),
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32to16,
                                                           getIReg(rt)))),
                                           mkU8(0x1))));

                     /* If both input arguments are equal 0x8000, saturate
                        intermediate product and write to DSPControl
                        register. */
                     assign(t2, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t3, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rt))),
                                      mkU32(0x00008000)));

                     assign(t4, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       unop(Iop_1Sto32,
                                                            mkexpr(t2)),
                                                       unop(Iop_1Sto32,
                                                            mkexpr(t3))),
                                                 mkU32(0)),
                                           mkU64(0x000000007fffffffULL),
                                           mkexpr(t1)));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    binop(Iop_And32,
                                                          unop(Iop_1Sto32,
                                                               mkexpr(t2)),
                                                          unop(Iop_1Sto32,
                                                               mkexpr(t3))),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16))),
                                              getDSPControl()));
                     /* Calculate second cross dot product and saturate if
                        needed. */
                     assign(t5, unop(Iop_32Sto64,
                                     binop(Iop_Shl32,
                                           binop(Iop_Mul32,
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32to16,
                                                           getIReg(rs))),
                                                 unop(Iop_16Sto32,
                                                      unop(Iop_32HIto16,
                                                           getIReg(rt)))),
                                           mkU8(0x1))));

                     /* If both input arguments are equal 0x8000, saturate
                        intermediate product and write to DSPControl
                        register. */
                     assign(t6, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32to16, getIReg(rs))),
                                      mkU32(0x00008000)));
                     assign(t7, binop(Iop_CmpEQ32,
                                      unop(Iop_16Uto32,
                                           unop(Iop_32HIto16, getIReg(rt))),
                                      mkU32(0x00008000)));

                     assign(t8, IRExpr_ITE(binop(Iop_CmpNE32,
                                                 binop(Iop_And32,
                                                       unop(Iop_1Sto32,
                                                            mkexpr(t6)),
                                                       unop(Iop_1Sto32,
                                                            mkexpr(t7))),
                                                 mkU32(0)),
                                           mkU64(0x000000007fffffffULL),
                                           mkexpr(t5)));

                     putDSPControl(IRExpr_ITE(binop(Iop_CmpNE32,
                                                    binop(Iop_And32,
                                                          unop(Iop_1Sto32,
                                                               mkexpr(t6)),
                                                          unop(Iop_1Sto32,
                                                               mkexpr(t7))),
                                                    mkU32(0)),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16))),
                                              getDSPControl()));
                     /* Subtract intermediate products from value in the
                        accumulator. */
                     assign(t9,
                            binop(Iop_Sub64,
                                  mkexpr(t0),
                                  binop(Iop_Add64, mkexpr(t8), mkexpr(t4))));

                     putAcc(ac,
                            IRExpr_ITE(binop(Iop_CmpEQ32,
                                             binop(Iop_And32,
                                                   unop(Iop_64HIto32,
                                                        mkexpr(t9)),
                                                   mkU32(0x80000000)),
                                             mkU32(0x0)),
                                       IRExpr_ITE(binop(Iop_CmpNE32,
                                                        unop(Iop_64HIto32,
                                                             binop(Iop_Shl64,
                                                                   mkexpr(t9),
                                                                   mkU8(1))),
                                                        mkU32(0x0)),
                                                  mkU64(0x000000007fffffffULL),
                                                  mkexpr(t9)),
                                       IRExpr_ITE(binop(Iop_CmpNE32,
                                                        unop(Iop_64HIto32,
                                                             binop(Iop_Shl64,
                                                                   mkexpr(t9),
                                                                   mkU8(1))),
                                                        mkU32(0xffffffff)),
                                                  mkU64(0xffffffff80000000ULL),
                                                  mkexpr(t9))));
                     assign(t10, IRExpr_ITE(binop(Iop_CmpEQ32,
                                                  unop(Iop_64to32,
                                                       mkexpr(t9)),
                                                  unop(Iop_64to32,
                                                       getAcc(ac))),
                                           getDSPControl(),
                                           binop(Iop_Or32,
                                                 getDSPControl(),
                                                 binop(Iop_Shl32,
                                                       mkU32(0x1),
                                                       mkU8(ac+16)))));
                     putDSPControl(IRExpr_ITE(binop(Iop_CmpEQ32,
                                                    unop(Iop_64HIto32,
                                                         mkexpr(t9)),
                                                    unop(Iop_64HIto32,
                                                         getAcc(ac))),
                                              mkexpr(t10),
                                              binop(Iop_Or32,
                                                    getDSPControl(),
                                                    binop(Iop_Shl32,
                                                          mkU32(0x1),
                                                          mkU8(ac+16)))));
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of DPAQ.W.PH */
            }
            case 0x31: {  /* APPEND */
               switch(sa) {
                  case 0x0: {  /* APPEND */
                     DIP("append r%d, r%d, %d", rt, rs, rd);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);

                     assign(t1, binop(Iop_Shl32, getIReg(rt), mkU8(rd)));

                     if (31 == rd) {
                        putIReg(rt, binop(Iop_Or32,
                                          mkexpr(t1),
                                          binop(Iop_And32,
                                                getIReg(rs),
                                                mkU32(0x7fffffff))));
                     } else if (1 == rd) {
                        putIReg(rt,
                                binop(Iop_Or32,
                                      mkexpr(t1),
                                      binop(Iop_And32,
                                            getIReg(rs), mkU32(0x1))));
                     } else {
                        assign(t2,
                               unop(Iop_Not32,
                                    binop(Iop_Shl32,
                                          mkU32(0xffffffff), mkU8(rd))));

                        putIReg(rt, binop(Iop_Or32,
                                          mkexpr(t1),
                                          binop(Iop_And32,
                                                getIReg(rs), mkexpr(t2))));
                     }
                     break;
                  }
                  case 0x1: {  /* PREPEND */
                     DIP("prepend r%d, r%d, %d", rt, rs, rd);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);

                     if (0 != rd) {
                        assign(t1, binop(Iop_Shr32, getIReg(rt), mkU8(rd)));

                        if (31 == rd) {
                           putIReg(rt, binop(Iop_Or32,
                                             mkexpr(t1),
                                             binop(Iop_Shl32,
                                                   binop(Iop_And32,
                                                         getIReg(rs),
                                                         mkU32(0x7fffffff)),
                                                   mkU8(1))));
                        } else if (1 == rd) {
                           putIReg(rt, binop(Iop_Or32,
                                             mkexpr(t1),
                                             binop(Iop_Shl32,
                                                   binop(Iop_And32,
                                                         getIReg(rs),
                                                         mkU32(0x1)),
                                                   mkU8(31))));
                        } else {
                           assign(t2, binop(Iop_Add32, mkU32(rd), mkU32(0x1)));

                           assign(t3, unop(Iop_Not32,
                                           binop(Iop_Shl32,
                                                 mkU32(0xffffffff),
                                                 unop(Iop_32to8, mkexpr(t2)))));

                           putIReg(rt, binop(Iop_Or32,
                                             mkexpr(t1),
                                             binop(Iop_Shl32,
                                                   binop(Iop_And32,
                                                         getIReg(rs),
                                                         mkexpr(t3)),
                                                   mkU8(32-rd))));
                        }
                     }
                     break;
                  }
                  case 0x10: {  /* BALIGN */
                     DIP("balign r%d, r%d, %d", rt, rs, rd);
                     vassert(!mode64);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);

                     if ((2 != rd) && (0 != rd)) {
                        assign(t1, binop(Iop_Shl32,
                                         binop(Iop_And32,
                                               mkU32(rd), mkU32(0x3)),
                                         mkU8(0x3)));
                        assign(t2, binop(Iop_Shl32,
                                         getIReg(rt),
                                         unop(Iop_32to8, mkexpr(t1))));
                        assign(t3, binop(Iop_Shr32,
                                         getIReg(rs),
                                         unop(Iop_32to8,
                                              binop(Iop_Shl32,
                                                    binop(Iop_Sub32,
                                                          mkU32(0x4),
                                                          binop(Iop_And32,
                                                                mkU32(rd),
                                                                mkU32(0x3))),
                                                    mkU8(0x3)))));
                        putIReg(rt, binop(Iop_Or32, mkexpr(t2), mkexpr(t3)));
                     }
                     break;
                  }
                  default:
                     return -1;
               }
               break;  /* end of APPEND */
            }
            default:
               return -1;
         }
         break;
      }
      default:
            return -1;
   }
   return 0;
}

/*------------------------------------------------------------*/
/*---          Disassemble a single instruction            ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR. The instruction is
   located in host memory at guest_instr, and has guest IP of
   guest_PC_curr_instr, which will have been set before the call
   here. */

static DisResult disInstr_MIPS_WRK ( Bool(*resteerOkFn) (/*opaque */void *,
                                                                    Addr64),
                                     Bool         resteerCisOk,
                                     void*        callback_opaque,
                                     Long         delta64,
                                     VexArchInfo* archinfo,
                                     VexAbiInfo*  abiinfo,
                                     Bool         sigill_diag )
{
   IRTemp t0, t1 = 0, t2, t3, t4, t5, t6, t7;

   UInt opcode, cins, rs, rt, rd, sa, ft, fs, fd, fmt, tf, nd, function,
        trap_code, imm, instr_index, p, msb, lsb, size, rot, sel;
   /* Additional variables for instruction fields in DSP ASE insructions */
   UInt ac;

   DisResult dres;

   static IRExpr *lastn = NULL;  /* last jump addr */
   static IRStmt *bstmt = NULL;  /* branch (Exit) stmt */

   /* The running delta */
   Int delta = (Int) delta64;

   /* Holds eip at the start of the insn, so that we can print
      consistent error messages for unimplemented insns. */
   Int delta_start = delta;

   /* Are we in a delay slot ? */
   Bool delay_slot_branch, likely_delay_slot, delay_slot_jump;

   /* Set result defaults. */
   dres.whatNext = Dis_Continue;
   dres.len = 0;
   dres.continueAt = 0;
   dres.jk_StopHere = Ijk_INVALID;

   delay_slot_branch = likely_delay_slot = delay_slot_jump = False;

   UChar *code = (UChar *) (guest_code + delta);
   cins = getUInt(code);
   DIP("\t0x%lx:\t0x%08x\t", (long)guest_PC_curr_instr, cins);

   if (delta != 0) {
      if (branch_or_jump(guest_code + delta - 4)) {
         if (lastn == NULL && bstmt == NULL) {
            vassert(0);
         } else {
            dres.whatNext = Dis_StopHere;
            if (lastn != NULL) {
               delay_slot_jump = True;
            } else if (bstmt != NULL) {
               delay_slot_branch = True;
            }
         }
      }

      if (branch_or_link_likely(guest_code + delta - 4)) {
         likely_delay_slot = True;
      }
   }

   /* Spot "Special" instructions (see comment at top of file). */
   {
      /* Spot the 16-byte preamble:
       ****mips32****
       "srl $0, $0, 13
       "srl $0, $0, 29
       "srl $0, $0, 3
       "srl $0, $0, 19

       ****mips64****
       dsll $0, $0, 3
       dsll $0, $0, 13
       dsll $0, $0, 29
       dsll $0, $0, 19 */

      UInt word1 = mode64 ? 0xF8  : 0x342;
      UInt word2 = mode64 ? 0x378 : 0x742;
      UInt word3 = mode64 ? 0x778 : 0xC2;
      UInt word4 = mode64 ? 0x4F8 : 0x4C2;
      if (getUInt(code + 0) == word1 && getUInt(code + 4) == word2 &&
          getUInt(code + 8) == word3 && getUInt(code + 12) == word4) {
         /* Got a "Special" instruction preamble. Which one is it? */
         if (getUInt(code + 16) == 0x01ad6825 /* or $13, $13, $13 */ ) {
            /* $11 = client_request ( $12 ) */
            DIP("$11 = client_request ( $12 )");
            if (mode64)
               putPC(mkU64(guest_PC_curr_instr + 20));
            else
               putPC(mkU32(guest_PC_curr_instr + 20));
            dres.jk_StopHere = Ijk_ClientReq;
            dres.whatNext    = Dis_StopHere;

            goto decode_success;
         } else if (getUInt(code + 16) == 0x01ce7025 /* or $14, $14, $14 */ ) {
            /* $11 = guest_NRADDR */
            DIP("$11 = guest_NRADDR");
            dres.len = 20;
            delta += 20;
            if (mode64)
               putIReg(11, IRExpr_Get(offsetof(VexGuestMIPS64State,
                                               guest_NRADDR), Ity_I64));
            else
               putIReg(11, IRExpr_Get(offsetof(VexGuestMIPS32State,
                                               guest_NRADDR), Ity_I32));
            goto decode_success;
         } else if (getUInt(code + 16) == 0x01ef7825 /* or $15, $15, $15 */ ) {
            /*  branch-and-link-to-noredir $25 */
            DIP("branch-and-link-to-noredir $25");
            if (mode64)
               putIReg(31, mkU64(guest_PC_curr_instr + 20));
            else
               putIReg(31, mkU32(guest_PC_curr_instr + 20));
            putPC(getIReg(25));
            dres.jk_StopHere = Ijk_NoRedir;
            dres.whatNext    = Dis_StopHere;
            goto decode_success;
         } else if (getUInt(code + 16) == 0x016b5825 /* or $11,$11,$11 */ ) {
           /* IR injection */
            DIP("IR injection");
#if defined (_MIPSEL)
            vex_inject_ir(irsb, Iend_LE);
#elif defined (_MIPSEB)
            vex_inject_ir(irsb, Iend_BE);
#endif
            if (mode64) {
               stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_CMSTART),
                               mkU64(guest_PC_curr_instr)));
               stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_CMLEN),
                               mkU64(20)));

               putPC(mkU64(guest_PC_curr_instr + 20));
            } else {
               stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_CMSTART),
                               mkU32(guest_PC_curr_instr)));
               stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_CMLEN),
                               mkU32(20)));

               putPC(mkU32(guest_PC_curr_instr + 20));
            }
            dres.whatNext    = Dis_StopHere;
            dres.jk_StopHere = Ijk_InvalICache;
            dres.len = 20;
            delta += 20;
            goto decode_success;
         }

         /* We don't know what it is.  Set opc1/opc2 so decode_failure
            can print the insn following the Special-insn preamble. */
         delta += 16;
         goto decode_failure;
       /*NOTREACHED*/}
   }

   opcode = get_opcode(cins);
   imm = get_imm(cins);
   rs = get_rs(cins);
   rt = get_rt(cins);
   rd = get_rd(cins);
   sa = get_sa(cins);
   fs = get_fs(cins);
   fd = get_fd(cins);
   ft = get_ft(cins);
   tf = get_tf(cins);
   nd = get_nd(cins);
   sel = get_sel(cins);
   fmt = get_fmt(cins);
   instr_index = get_instr_index(cins);
   trap_code = get_code(cins);
   function = get_function(cins);
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IRType tyF = fp_mode64 ? Ity_F64 : Ity_F32;

   ac = get_acNo(cins);

   switch (opcode) {

   case 0x03:     /* JAL */
      DIP("jal 0x%x", instr_index);
      if (mode64) {
         putIReg(31, mkU64(guest_PC_curr_instr + 8));
         t0 = newTemp(ty);
         assign(t0, mkU64((guest_PC_curr_instr & 0xFFFFFFFFF0000000ULL) |
                          (instr_index << 2)));
      } else {
         putIReg(31, mkU32(guest_PC_curr_instr + 8));
         t0 = newTemp(ty);
         assign(t0, mkU32((guest_PC_curr_instr & 0xF0000000) |
                          (instr_index << 2)));
      }
      lastn = mkexpr(t0);
      break;
   case 0x02:     /* J */
      DIP("j 0x%x", instr_index);
      t0 = newTemp(ty);
      if (mode64)
         assign(t0, mkU64((guest_PC_curr_instr & 0xFFFFFFFFF0000000ULL) |
                          (instr_index << 2)));
      else
         assign(t0, mkU32((guest_PC_curr_instr & 0xF0000000) |
                          (instr_index << 2)));
      lastn = mkexpr(t0);
      break;

   case 0x11: {  /* COP1 */
      if (fmt == 0x3 && fd == 0 && function == 0) {  /* MFHC1 */
         DIP("mfhc1 r%d, f%d", rt, fs);
         if (fp_mode64) {
            t0 = newTemp(Ity_I64);
            t1 = newTemp(Ity_I32);
            assign(t0, unop(Iop_ReinterpF64asI64, getDReg(fs)));
            assign(t1, unop(Iop_64HIto32, mkexpr(t0)));
            putIReg(rt, mkWidenFrom32(ty, mkexpr(t1), True));
         } else {
            ILLEGAL_INSTRUCTON;
         }
         break;
      } else if (fmt == 0x7 && fd == 0 && function == 0) {  /* MTHC1 */
         DIP("mthc1 r%d, f%d", rt, fs);
         if (fp_mode64) {
            t0 = newTemp(Ity_I64);
            assign(t0, binop(Iop_32HLto64, getIReg(rt),
                             unop(Iop_ReinterpF32asI32,
                                  getLoFromF64(Ity_F64 /* 32FPR mode. */,
                                               getDReg(fs)))));
            putDReg(fs, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
         } else {
            ILLEGAL_INSTRUCTON;
         }
         break;
      } else if (fmt == 0x8) {  /* BC */
         /* FcConditionalCode(bc1_cc) */
         UInt bc1_cc = get_bc1_cc(cins);
         t1 = newTemp(Ity_I1);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I1);

         assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(bc1_cc)));
         assign(t2, IRExpr_ITE(mkexpr(t1),
                               binop(Iop_And32,
                                     binop(Iop_Shr32, getFCSR(), mkU8(23)),
                                     mkU32(0x1)),
                               binop(Iop_And32,
                                     binop(Iop_Shr32, getFCSR(),
                                           mkU8(24 + bc1_cc)),
                                     mkU32(0x1))));

         if (tf == 1 && nd == 0) {
            /* branch on true */
            DIP("bc1t %d, %d", bc1_cc, imm);
            assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
            dis_branch(False, mkexpr(t3), imm, &bstmt);
            break;
         } else if (tf == 0 && nd == 0) {
            /* branch on false */
            DIP("bc1f %d, %d", bc1_cc, imm);
            assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
            dis_branch(False, mkexpr(t3), imm, &bstmt);
            break;
         } else if (nd == 1 && tf == 0) {
            DIP("bc1fl %d, %d", bc1_cc, imm);
            lastn = dis_branch_likely(binop(Iop_CmpNE32, mkexpr(t2),
                                            mkU32(0x0)), imm);
            break;
         } else if (nd == 1 && tf == 1) {
            DIP("bc1tl %d, %d", bc1_cc, imm);
            lastn = dis_branch_likely(binop(Iop_CmpEQ32, mkexpr(t2),
                                            mkU32(0x0)), imm);
            break;
         } else
            goto decode_failure;
      } else {
         switch (function) {
            case 0x4: {  /* SQRT.fmt */
               switch (fmt) {
                  case 0x10: {  /* S */
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, binop(Iop_SqrtF32, rm,
                                 getLoFromF64(tyF, getFReg(fs)))));
                     break;
                  }
                  case 0x11: {  /* D */
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, binop(Iop_SqrtF64, rm, getDReg(fs)));
                     break;
                  }
                  default:
                     goto decode_failure;
                  }
               }
               break;
            case 0x5:  /* abs.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("abs.s f%d, f%d", fd, fs);
                     putFReg(fd, mkWidenFromF32(tyF, unop(Iop_AbsF32,
                                 getLoFromF64(tyF, getFReg(fs)))));
                     break;
                  case 0x11:  /* D  */
                     DIP("abs.d f%d, f%d", fd, fs);
                     putDReg(fd, unop(Iop_AbsF64, getDReg(fs)));
                     break;
                  default:
                     goto decode_failure;
               }
               break;  /* case 0x5 */

            case 0x02:  /* MUL.fmt */
               switch (fmt) {
                  case 0x11: {  /* D */
                     DIP("mul.d f%d, f%d, f%d", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_MulF64, rm, getDReg(fs),
                                       getDReg(ft)));
                     break;
                  }
                  case 0x10: {  /* S */
                     DIP("mul.s f%d, f%d, f%d", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_MulF32, rm,
                                 getLoFromF64(tyF, getFReg(fs)),
                                 getLoFromF64(tyF, getFReg(ft)))));
                     break;
                  }
                  default:
                     goto decode_failure;
               }
               break;  /* MUL.fmt */

            case 0x03:  /* DIV.fmt */
               switch (fmt) {
                  case 0x11: {  /* D */
                     DIP("div.d f%d, f%d, f%d", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_DivF64, rm, getDReg(fs),
                                 getDReg(ft)));
                     break;
                  }
                  case 0x10: {  /* S */
                     DIP("div.s f%d, f%d, f%d", fd, fs, ft);
                     calculateFCSR(fs, ft, DIVS, False, 2);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32, rm,
                                 getLoFromF64(tyF, getFReg(fs)),
                                 getLoFromF64(tyF, getFReg(ft)))));
                     break;
                  }
                  default:
                     goto decode_failure;
               }
               break;  /* DIV.fmt */

            case 0x01:  /* SUB.fmt */
               switch (fmt) {
                  case 0x11: {  /* D */
                     DIP("sub.d f%d, f%d, f%d", fd, fs, ft);
                     calculateFCSR(fs, ft, SUBD, False, 2);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_SubF64, rm, getDReg(fs),
                                       getDReg(ft)));
                     break;
                  }
                  case 0x10: {  /* S */
                     DIP("sub.s f%d, f%d, f%d", fd, fs, ft);
                     calculateFCSR(fs, ft, SUBS, True, 2);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_SubF32, rm,
                                 getLoFromF64(tyF, getFReg(fs)),
                                 getLoFromF64(tyF, getFReg(ft)))));
                     break;
                  }
                  default:
                     goto decode_failure;
               }
               break;  /* SUB.fmt */

            case 0x06:  /* MOV.fmt */
               switch (fmt) {
                  case 0x11:  /* D */
                     DIP("mov.d f%d, f%d", fd, fs);
                     if (fp_mode64) {
                        putDReg(fd, getDReg(fs));
                     } else {
                        putFReg(fd, getFReg(fs));
                        putFReg(fd + 1, getFReg(fs + 1));
                     }
                     break;
                  case 0x10:  /* S */
                     DIP("mov.s f%d, f%d", fd, fs);
                     putFReg(fd, getFReg(fs));
                     break;
                  default:
                     goto decode_failure;
               }
               break;  /* MOV.fmt */

            case 0x7:  /* neg.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("neg.s f%d, f%d", fd, fs);
                     putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32,
                                 getLoFromF64(tyF, getFReg(fs)))));
                     break;
                  case 0x11:  /* D */
                     DIP("neg.d f%d, f%d", fd, fs);
                     putDReg(fd, unop(Iop_NegF64, getDReg(fs)));
                     break;
                  default:
                     goto decode_failure;
               }
               break;  /* case 0x7 */

            case 0x08:  /* ROUND.L.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("round.l.s f%d, f%d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, ROUNDLS, True, 1);
                        t0 = newTemp(Ity_I64);

                        assign(t0, binop(Iop_F32toI64S, mkU32(0x0),
                                         getLoFromF64(Ity_F64, getFReg(fs))));

                        putDReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;
                  case 0x11:  /* D */
                     DIP("round.l.d f%d, f%d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, ROUNDLD, False, 1);
                        putDReg(fd, binop(Iop_RoundF64toInt, mkU32(0x0),
                                          getDReg(fs)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;
                  default:
                    goto decode_failure;

               }
               break;  /* ROUND.L.fmt */

            case 0x09:  /* TRUNC.L.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("trunc.l.s f%d, f%d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, TRUNCLS, True, 1);
                        t0 = newTemp(Ity_I64);
                        assign(t0, binop(Iop_F32toI64S, mkU32(0x3),
                                         getLoFromF64(Ity_F64, getFReg(fs))));

                        putDReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;
                  case 0x11:  /* D */
                     DIP("trunc.l.d f%d, f%d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, TRUNCLD, False, 1);
                        putDReg(fd, binop(Iop_RoundF64toInt, mkU32(0x3),
                                          getDReg(fs)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;
                  default:
                     goto decode_failure;
                 }
              break;  /* TRUNC.L.fmt */

            case 0x15:  /* RECIP.fmt */
               switch (fmt) {
                  case 0x10: {  /* S */
                     DIP("recip.s f%d, f%d", fd, fs);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32,
                                 rm, unop(Iop_ReinterpI32asF32,
                                 mkU32(ONE_SINGLE)), getLoFromF64(tyF,
                                 getFReg(fs)))));
                     break;
                  }
                  case 0x11: {  /* D */
                     DIP("recip.d f%d, f%d", fd, fs);
                     IRExpr *rm = get_IR_roundingmode();
                     /* putDReg(fd, 1.0/getDreg(fs)); */
                     putDReg(fd, triop(Iop_DivF64, rm,
                                 unop(Iop_ReinterpI64asF64,
                                 mkU64(ONE_DOUBLE)), getDReg(fs)));
                     break;
                  }
               default:
                  goto decode_failure;

               }
               break;  /* case 0x15 */

            case 0x13:  /* MOVN.fmt */
               switch (fmt) {
               case 0x10:  /* S */
                  DIP("movn.s f%d, f%d, r%d", fd, fs, rt);
                  t1 = newTemp(Ity_F64);
                  t2 = newTemp(Ity_F64);
                  t3 = newTemp(Ity_I1);
                  t4 = newTemp(Ity_F64);
                  if (mode64) {
                     assign(t1, getFReg(fs));
                     assign(t2, getFReg(fd));
                     assign(t3, binop(Iop_CmpNE64, mkU64(0), getIReg(rt)));
                  } else {
                     if (fp_mode64) {
                        assign(t1, getFReg(fs));
                        assign(t2, getFReg(fd));
                        assign(t3, binop(Iop_CmpNE32, mkU32(0), getIReg(rt)));
                     } else {
                        assign(t1, unop(Iop_F32toF64, getFReg(fs)));
                        assign(t2, unop(Iop_F32toF64, getFReg(fd)));
                        assign(t3, binop(Iop_CmpNE32, mkU32(0), getIReg(rt)));
                     }
                  }

                  assign(t4, IRExpr_ITE(mkexpr(t3), mkexpr(t1), mkexpr(t2)));
                  if (fp_mode64) {
                     IRTemp f = newTemp(Ity_F64);
                     IRTemp fd_hi = newTemp(Ity_I32);
                     t5 = newTemp(Ity_I64);
                     assign(f, getFReg(fd));
                     assign(fd_hi, unop(Iop_64HIto32, unop(Iop_ReinterpF64asI64,
                                        mkexpr(f))));

                     assign(t5, mkWidenFrom32(Ity_I64, unop(Iop_64to32,
                                unop(Iop_ReinterpF64asI64, mkexpr(t4))), True));

                     putFReg(fd, unop (Iop_ReinterpI64asF64, mkexpr(t5)));
                  } else
                     putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                       mkexpr(t4)));
                  break;
               case 0x11:  /* D */
                  DIP("movn.d f%d, f%d, r%d", fd, fs, rt);

                  t3 = newTemp(Ity_I1);
                  t4 = newTemp(Ity_F64);

                  if (mode64)
                     assign(t3, binop(Iop_CmpNE64, mkU64(0), getIReg(rt)));
                  else
                     assign(t3, binop(Iop_CmpNE32, mkU32(0), getIReg(rt)));

                  putDReg(fd, IRExpr_ITE(mkexpr(t3), getDReg(fs), getDReg(fd)));
                  break;
               default:
                  goto decode_failure;
               }
               break;  /* MOVN.fmt */

            case 0x12:  /* MOVZ.fmt */
               switch (fmt) {
               case 0x10:  /* S */
                  DIP("movz.s f%d, f%d, r%d", fd, fs, rt);

                  t1 = newTemp(Ity_F64);
                  t2 = newTemp(Ity_F64);
                  t3 = newTemp(Ity_I1);
                  t4 = newTemp(Ity_F64);
                  if (fp_mode64) {
                     assign(t1, getFReg(fs));
                     assign(t2, getFReg(fd));
                     if (mode64)
                        assign(t3, binop(Iop_CmpEQ64, mkU64(0), getIReg(rt)));
                     else
                        assign(t3, binop(Iop_CmpEQ32, mkU32(0), getIReg(rt)));
                  } else {
                     assign(t1, unop(Iop_F32toF64, getFReg(fs)));
                     assign(t2, unop(Iop_F32toF64, getFReg(fd)));
                     assign(t3, binop(Iop_CmpEQ32, mkU32(0), getIReg(rt)));
                  }
                  assign(t4, IRExpr_ITE(mkexpr(t3), mkexpr(t1), mkexpr(t2)));

                 if (fp_mode64) {
                     IRTemp f = newTemp(Ity_F64);
                     IRTemp fd_hi = newTemp(Ity_I32);
                     t7 = newTemp(Ity_I64);
                     assign(f, getFReg(fd));
                     assign(fd_hi, unop(Iop_64HIto32,
                                   unop(Iop_ReinterpF64asI64, mkexpr(f))));
                     assign(t7, mkWidenFrom32(Ity_I64, unop(Iop_64to32,
                                unop(Iop_ReinterpF64asI64, mkexpr(t4))), True));

                     putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t7)));
                  } else
                     putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                       mkexpr(t4)));

                  break;
               case 0x11:  /* D */
                  DIP("movz.d f%d, f%d, r%d", fd, fs, rt);
                  t3 = newTemp(Ity_I1);
                  t4 = newTemp(Ity_F64);
                  if (mode64)
                     assign(t3, binop(Iop_CmpEQ64, mkU64(0), getIReg(rt)));
                  else
                     assign(t3, binop(Iop_CmpEQ32, mkU32(0), getIReg(rt)));

                  putDReg(fd, IRExpr_ITE(mkexpr(t3), getDReg(fs), getDReg(fd)));
                  break;
               default:
                  goto decode_failure;
               }
               break;  /* MOVZ.fmt */

            case 0x11:  /* MOVT.fmt */
               if (tf == 1) {
                  UInt mov_cc = get_mov_cc(cins);
                  switch (fmt) {  /* MOVCF = 010001 */
                  case 0x11:  /* D */
                     DIP("movt.d f%d, f%d, %d", fd, fs, mov_cc);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_F64);

                     assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
                     assign(t2, IRExpr_ITE(mkexpr(t1),
                                           binop(Iop_And32,
                                                 binop(Iop_Shr32, getFCSR(),
                                                       mkU8(23)),
                                                 mkU32(0x1)),
                                           binop(Iop_And32,
                                                 binop(Iop_Shr32, getFCSR(),
                                                       mkU8(24 + mov_cc)),
                                                 mkU32(0x1))
                                           ));

                     assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
                     assign(t4, IRExpr_ITE(mkexpr(t3),
                                           getDReg(fs), getDReg(fd)));
                     putDReg(fd, mkexpr(t4));
                     break;
                  case 0x10:  /* S */
                     DIP("movt.s f%d, f%d, %d", fd, fs, mov_cc);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_F64);
                     t5 = newTemp(Ity_F64);
                     t6 = newTemp(Ity_F64);
                     t7 = newTemp(Ity_I64);

                     if (fp_mode64) {
                        assign(t5, getFReg(fs));
                        assign(t6, getFReg(fd));
                     } else {
                        assign(t5, unop(Iop_F32toF64, getFReg(fs)));
                        assign(t6, unop(Iop_F32toF64, getFReg(fd)));
                     }

                     assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
                     assign(t2, IRExpr_ITE(mkexpr(t1),
                                           binop(Iop_And32,
                                                 binop(Iop_Shr32, getFCSR(),
                                                       mkU8(23)),
                                                 mkU32(0x1)),
                                           binop(Iop_And32,
                                                 binop(Iop_Shr32, getFCSR(),
                                                       mkU8(24 + mov_cc)),
                                                 mkU32(0x1))
                                           ));

                     assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
                     assign(t4, IRExpr_ITE(mkexpr(t3),
                                           mkexpr(t5), mkexpr(t6)));

                     if (fp_mode64) {
                        IRTemp f = newTemp(Ity_F64);
                        IRTemp fd_hi = newTemp(Ity_I32);
                        assign(f, getFReg(fd));
                        assign(fd_hi, unop(Iop_64HIto32,
                                      unop(Iop_ReinterpF64asI64, mkexpr(f))));
                        assign(t7, mkWidenFrom32(Ity_I64, unop(Iop_64to32,
                                      unop(Iop_ReinterpF64asI64, mkexpr(t4))),
                                      True));

                        putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t7)));
                     } else
                        putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                          mkexpr(t4)));
                     break;
                  default:
                     goto decode_failure;
                  }
               } else if (tf == 0)  /* movf.fmt */
               {
                  UInt mov_cc = get_mov_cc(cins);
                  switch (fmt)  /* MOVCF = 010001 */
                  {
                  case 0x11:  /* D */
                     DIP("movf.d f%d, f%d, %d", fd, fs, mov_cc);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_F64);

                     assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
                     assign(t2, IRExpr_ITE(mkexpr(t1),
                                           binop(Iop_And32,
                                                 binop(Iop_Shr32, getFCSR(),
                                                       mkU8(23)),
                                                 mkU32(0x1)),
                                           binop(Iop_And32,
                                                 binop(Iop_Shr32, getFCSR(),
                                                       mkU8(24 + mov_cc)),
                                                 mkU32(0x1))
                                           ));

                     assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
                     assign(t4, IRExpr_ITE(mkexpr(t3),
                                           getDReg(fs), getDReg(fd)));
                     putDReg(fd, mkexpr(t4));
                     break;
                  case 0x10:  /* S */
                     DIP("movf.s f%d, f%d, %d", fd, fs, mov_cc);
                     t1 = newTemp(Ity_I1);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I1);
                     t4 = newTemp(Ity_F64);
                     t5 = newTemp(Ity_F64);
                     t6 = newTemp(Ity_F64);

                     if (fp_mode64) {
                        assign(t5, getFReg(fs));
                        assign(t6, getFReg(fd));
                     } else {
                        assign(t5, unop(Iop_F32toF64, getFReg(fs)));
                        assign(t6, unop(Iop_F32toF64, getFReg(fd)));
                     }

                     assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
                     assign(t2, IRExpr_ITE(mkexpr(t1),
                                           binop(Iop_And32,
                                                 binop(Iop_Shr32, getFCSR(),
                                                       mkU8(23)),
                                                 mkU32(0x1)),
                                           binop(Iop_And32,
                                                 binop(Iop_Shr32, getFCSR(),
                                                       mkU8(24 + mov_cc)),
                                                 mkU32(0x1))
                                           ));

                     assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
                     assign(t4, IRExpr_ITE(mkexpr(t3),
                                           mkexpr(t5), mkexpr(t6)));

                     if (fp_mode64) {
                        IRTemp f = newTemp(Ity_F64);
                        IRTemp fd_hi = newTemp(Ity_I32);
                        t7 = newTemp(Ity_I64);
                        assign(f, getFReg(fd));
                        assign(fd_hi, unop(Iop_64HIto32,
                                      unop(Iop_ReinterpF64asI64, mkexpr(f))));
                        assign(t7, mkWidenFrom32(Ity_I64, unop(Iop_64to32,
                                   unop(Iop_ReinterpF64asI64, mkexpr(t4))),
                                   True));

                        putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t7)));
                     } else
                        putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                          mkexpr(t4)));
                     break;
                  default:
                     goto decode_failure;
                  }
               }

               break;  /* MOVT.fmt */

            case 0x0:  /* add.fmt */
               switch (fmt) {
               case 0x10: {  /* S */
                  DIP("add.s f%d, f%d, f%d", fd, fs, ft);
                  calculateFCSR(fs, ft, ADDS, True, 2);
                  IRExpr *rm = get_IR_roundingmode();
                  putFReg(fd, mkWidenFromF32(tyF, triop(Iop_AddF32, rm,
                              getLoFromF64(tyF, getFReg(fs)),
                              getLoFromF64(tyF, getFReg(ft)))));
                  break;
               }
               case 0x11: {  /* D */
                  DIP("add.d f%d, f%d, f%d", fd, fs, ft);
                  calculateFCSR(fs, ft, ADDD, False, 2);
                  IRExpr *rm = get_IR_roundingmode();
                  putDReg(fd, triop(Iop_AddF64, rm, getDReg(fs), getDReg(ft)));
                  break;
               }

               case 0x4:  /* MTC1 (Move Word to Floating Point) */
                  DIP("mtc1 r%d, f%d", rt, fs);
                  if (fp_mode64) {
                     t0 = newTemp(Ity_I32);
                     t1 = newTemp(Ity_F32);
                     assign(t0, mkNarrowTo32(ty, getIReg(rt)));
                     assign(t1, unop(Iop_ReinterpI32asF32, mkexpr(t0)));

                     putFReg(fs, mkWidenFromF32(tyF, mkexpr(t1)));
                  } else
                     putFReg(fs, unop(Iop_ReinterpI32asF32, getIReg(rt)));
                  break;

               case 0x5:  /* Doubleword Move to Floating Point DMTC1; MIPS64 */
                  DIP("dmtc1 r%d, f%d", rt, fs);
                  vassert(mode64);
                  putFReg(fs, unop(Iop_ReinterpI64asF64, getIReg(rt)));
                  break;

               case 0x0:  /* MFC1 */
                  DIP("mfc1 r%d, f%d", rt, fs);
                  if (fp_mode64) {
                     t0 = newTemp(Ity_I64);
                     t1 = newTemp(Ity_I32);
                     assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));
                     assign(t1, unop(Iop_64to32, mkexpr(t0)));
                     putIReg(rt, mkWidenFrom32(ty, mkexpr(t1), True));
                  } else
                     putIReg(rt, unop(Iop_ReinterpF32asI32, getFReg(fs)));
                  break;

               case 0x1:  /* Doubleword Move from Floating Point DMFC1;
                             MIPS64 */
                  DIP("dmfc1 r%d, f%d", rt, fs);
                  putIReg(rt, unop(Iop_ReinterpF64asI64, getFReg(fs)));
                  break;

               case 0x6:  /* CTC1 */
                  DIP("ctc1 r%d, f%d", rt, fs);
                  t0 = newTemp(Ity_I32);
                  t1 = newTemp(Ity_I32);
                  t2 = newTemp(Ity_I32);
                  t3 = newTemp(Ity_I32);
                  t4 = newTemp(Ity_I32);
                  t5 = newTemp(Ity_I32);
                  t6 = newTemp(Ity_I32);
                  assign(t0, mkNarrowTo32(ty, getIReg(rt)));
                  if (fs == 25) {  /* FCCR */
                     assign(t1, binop(Iop_Shl32, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x000000FE)), mkU8(24)));
                     assign(t2, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x01000000)));
                     assign(t3, binop(Iop_Shl32, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x00000001)), mkU8(23)));
                     assign(t4, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x007FFFFF)));
                     putFCSR(binop(Iop_Or32, binop(Iop_Or32, mkexpr(t1),
                                   mkexpr(t2)), binop(Iop_Or32, mkexpr(t3),
                                   mkexpr(t4))));
                  } else if (fs == 26) {  /* FEXR */
                     assign(t1, binop(Iop_And32, getFCSR(), mkU32(0xFFFC0000)));
                     assign(t2, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x0003F000)));
                     assign(t3, binop(Iop_And32, getFCSR(), mkU32(0x00000F80)));
                     assign(t4, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x0000007C)));
                     assign(t5, binop(Iop_And32, getFCSR(), mkU32(0x00000003)));
                     putFCSR(binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32,
                                   mkexpr(t1), mkexpr(t2)), binop(Iop_Or32,
                                   mkexpr(t3), mkexpr(t4))), mkexpr(t5)));
                  } else if (fs == 28) {
                     assign(t1, binop(Iop_And32, getFCSR(), mkU32(0xFE000000)));
                     assign(t2, binop(Iop_Shl32, binop(Iop_And32, mkexpr(t0),
                                mkU32(0x00000002)), mkU8(22)));
                     assign(t3, binop(Iop_And32, getFCSR(), mkU32(0x00FFF000)));
                     assign(t4, binop(Iop_And32, mkexpr(t0),
                                mkU32(0x00000F80)));
                     assign(t5, binop(Iop_And32, getFCSR(), mkU32(0x0000007C)));
                     assign(t6, binop(Iop_And32, mkexpr(t0),
                                mkU32(0x00000003)));
                     putFCSR(binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32,
                                   mkexpr(t1), mkexpr(t2)), binop(Iop_Or32,
                                   mkexpr(t3), mkexpr(t4))), binop(Iop_Or32,
                                   mkexpr(t5), mkexpr(t6))));
                  } else if (fs == 31) {
                     putFCSR(mkexpr(t0));
                  }
                  break;
               case 0x2:  /* CFC1 */
                  DIP("cfc1 r%d, f%d", rt, fs);
                  t0 = newTemp(Ity_I32);
                  t1 = newTemp(Ity_I32);
                  t2 = newTemp(Ity_I32);
                  t3 = newTemp(Ity_I32);
                  t4 = newTemp(Ity_I32);
                  t5 = newTemp(Ity_I32);
                  t6 = newTemp(Ity_I32);
                  assign(t0, getFCSR());
                  if (fs == 0) {
                     putIReg(rt, mkWidenFrom32(ty,
                             IRExpr_Get(offsetof(VexGuestMIPS32State,
                                                 guest_FIR),
                                       Ity_I32),
                             False));
                  } else if (fs == 25) {
                     assign(t1, mkU32(0x000000FF));
                     assign(t2, binop(Iop_Shr32, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0xFE000000)), mkU8(25)));
                     assign(t3, binop(Iop_Shr32, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x00800000)), mkU8(23)));
                     putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32,
                                 binop(Iop_Or32, mkexpr(t1), mkexpr(t2)),
                                 mkexpr(t3)), False));
                  } else if (fs == 26) {
                     assign(t1, mkU32(0xFFFFF07C));
                     assign(t2, binop(Iop_And32, mkexpr(t0),
                                mkU32(0x0003F000)));
                     assign(t3, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x0000007C)));
                     putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32,
                                 binop(Iop_Or32, mkexpr(t1), mkexpr(t2)),
                                 mkexpr(t3)), False));
                  } else if (fs == 28) {
                     assign(t1, mkU32(0x00000F87));
                     assign(t2, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x00000F83)));
                     assign(t3, binop(Iop_Shr32, binop(Iop_And32, mkexpr(t0),
                                      mkU32(0x01000000)), mkU8(22)));
                     putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32,
                                 binop(Iop_Or32, mkexpr(t1), mkexpr(t2)),
                                 mkexpr(t3)), False));
                  } else if (fs == 31) {
                     putIReg(rt, mkWidenFrom32(ty, getFCSR(), False));
                  }
                  break;
               default:
                  goto decode_failure;
               }
               break;

            case 0x21:  /* CVT.D */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("cvt.d.s f%d, f%d", fd, fs);
                     calculateFCSR(fs, 0, CVTDS, True, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I64);
                        t1 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_F32);
                        t4 = newTemp(Ity_F32);
                        /* get lo half of FPR */
                        assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                        assign(t1, unop(Iop_64to32, mkexpr(t0)));

                        assign(t3, unop(Iop_ReinterpI32asF32, mkexpr(t1)));

                        putFReg(fd, unop(Iop_F32toF64, mkexpr(t3)));
                     } else
                        putDReg(fd, unop(Iop_F32toF64, getFReg(fs)));
                     break;

                  case 0x14:
                     DIP("cvt.d.w %d, %d", fd, fs);
                     calculateFCSR(fs, 0, CVTDW, True, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I64);
                        t1 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_F32);
                        t4 = newTemp(Ity_F32);
                        /* get lo half of FPR */
                        assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                        assign(t1, unop(Iop_64to32, mkexpr(t0)));
                        putDReg(fd,unop(Iop_I32StoF64, mkexpr(t1)));
                        break;
                     } else {
                        t0 = newTemp(Ity_I32);
                        assign(t0, unop(Iop_ReinterpF32asI32, getFReg(fs)));
                        putDReg(fd, unop(Iop_I32StoF64, mkexpr(t0)));
                        break;
                     }

                  case 0x15: {  /* L */
                     if (fp_mode64) {
                        DIP("cvt.d.l %d, %d", fd, fs);
                        calculateFCSR(fs, 0, CVTDL, False, 1);
                        t0 = newTemp(Ity_I64);
                        assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                        putFReg(fd, binop(Iop_I64StoF64,
                                          get_IR_roundingmode(), mkexpr(t0)));
                        break;
                     } else
                        goto decode_failure;
                  }
                  default:
                     goto decode_failure;
               }
               break;  /* CVT.D */

            case 0x20:  /* cvt.s */
               switch (fmt) {
                  case 0x14:  /* W */
                     DIP("cvt.s.w %d, %d", fd, fs);
                     calculateFCSR(fs, 0, CVTSW, True, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I64);
                        t1 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_F32);
                        t4 = newTemp(Ity_F32);
                        /* get lo half of FPR */
                        assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                        assign(t1, unop(Iop_64to32, mkexpr(t0)));
                        putFReg(fd, mkWidenFromF32(tyF, binop(Iop_I32StoF32,
                                    get_IR_roundingmode(), mkexpr(t1))));
                     } else {
                        t0 = newTemp(Ity_I32);
                        assign(t0, unop(Iop_ReinterpF32asI32, getFReg(fs)));
                        putFReg(fd, binop(Iop_I32StoF32, get_IR_roundingmode(),
                                    mkexpr(t0)));
                     }
                     break;

                  case 0x11:  /* D */
                     DIP("cvt.s.d %d, %d", fd, fs);
                     calculateFCSR(fs, 0, CVTSD, False, 1);
                     t0 = newTemp(Ity_F32);
                     assign(t0, binop(Iop_F64toF32, get_IR_roundingmode(),
                                      getDReg(fs)));
                     putFReg(fd, mkWidenFromF32(tyF, mkexpr(t0)));
                     break;

                  case 0x15:  /* L */
                     DIP("cvt.s.l %d, %d", fd, fs);
                     calculateFCSR(fs, 0, CVTSL, False, 1);
                     t0 = newTemp(Ity_I64);
                     assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                     putFReg(fd, mkWidenFromF32(tyF, binop(Iop_I64StoF32,
                                 get_IR_roundingmode(), mkexpr(t0))));
                     break;

                  default:
                     goto decode_failure;
               }
               break;  /* cvt.s */

            case 0x24:  /* cvt.w */
               switch (fmt) {
               case 0x10:  /* S */
                  DIP("cvt.w.s %d, %d", fd, fs);
                  calculateFCSR(fs, 0, CVTWS, True, 1);
                  putFReg(fd,
                          mkWidenFromF32(tyF,
                                         binop(Iop_RoundF32toInt,
                                               get_IR_roundingmode(),
                                               getLoFromF64(tyF, getFReg(fs))))
                         );
                  break;

               case 0x11:
                  DIP("cvt.w.d %d, %d", fd, fs);
                  calculateFCSR(fs, 0, CVTWD, False, 1);
                  t0 = newTemp(Ity_I32);
                  t1 = newTemp(Ity_F32);
                  assign(t0, binop(Iop_F64toI32S, get_IR_roundingmode(),
                                   getDReg(fs)));
                  assign(t1, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                  putFReg(fd, mkWidenFromF32(tyF, mkexpr(t1)));
                  break;

               default:
                  goto decode_failure;

               }
               break;

            case 0x25:  /* cvt.l */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("cvt.l.s %d, %d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, CVTLS, True, 1);
                        t0 = newTemp(Ity_I64);

                        assign(t0, binop(Iop_F32toI64S, get_IR_roundingmode(),
                                         getLoFromF64(tyF, getFReg(fs))));

                        putDReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;

                  case 0x11: {  /* D */
                     DIP("cvt.l.d %d, %d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, CVTLD, False, 1);
                        putDReg(fd, binop(Iop_RoundF64toInt,
                                get_IR_roundingmode(), getDReg(fs)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;
                  }

                  default:
                     goto decode_failure;
               }
               break;

            case 0x0B:  /* FLOOR.L.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("floor.l.s %d, %d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, FLOORLS, True, 1);
                        t0 = newTemp(Ity_I64);

                        assign(t0, binop(Iop_F32toI64S, mkU32(0x1),
                                         getLoFromF64(tyF, getFReg(fs))));

                        putDReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;

                  case 0x11:  /* D */
                     DIP("floor.l.d %d, %d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, FLOORLD, False, 1);
                        putDReg(fd, binop(Iop_RoundF64toInt, mkU32(0x1),
                                          getDReg(fs)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;
                  default:
                     goto decode_failure;
               }
               break;

            case 0x0C:  /* ROUND.W.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("round.w.s f%d, f%d", fd, fs);
                     calculateFCSR(fs, 0, ROUNDWS, True, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I64);
                        t1 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_F32);
                        t4 = newTemp(Ity_F32);
                        /* get lo half of FPR */
                        assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                        assign(t1, unop(Iop_64to32, mkexpr(t0)));

                        assign(t3, unop(Iop_ReinterpI32asF32, mkexpr(t1)));

                        assign(t4, binop(Iop_RoundF32toInt, mkU32(0x0),
                                         mkexpr(t3)));

                        putFReg(fd, mkWidenFromF32(tyF, mkexpr(t4)));
                     } else
                        putFReg(fd, binop(Iop_RoundF32toInt, mkU32(0x0),
                                          getFReg(fs)));
                     break;

                  case 0x11:  /* D */
                     DIP("round.w.d f%d, f%d", fd, fs);
                     calculateFCSR(fs, 0, ROUNDWD, False, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I32);
                        assign(t0, binop(Iop_F64toI32S, mkU32(0x0),
                                         getDReg(fs)));
                        putFReg(fd, mkWidenFromF32(tyF,
                                    unop(Iop_ReinterpI32asF32, mkexpr(t0))));
                     } else {
                        t0 = newTemp(Ity_I32);

                        assign(t0, binop(Iop_F64toI32S, mkU32(0x0),
                                         getDReg(fs)));

                        putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                     }
                     break;
                  default:
                     goto decode_failure;

                  }
                  break;  /* ROUND.W.fmt */

            case 0x0F:  /* FLOOR.W.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("floor.w.s f%d, f%d", fd, fs);
                     calculateFCSR(fs, 0, FLOORWS, True, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I64);
                        t1 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_F32);
                        t4 = newTemp(Ity_F32);
                        /* get lo half of FPR */
                        assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                        assign(t1, unop(Iop_64to32, mkexpr(t0)));

                        assign(t3, unop(Iop_ReinterpI32asF32, mkexpr(t1)));

                        assign(t4, binop(Iop_RoundF32toInt, mkU32(0x1),
                                         mkexpr(t3)));

                        putFReg(fd, mkWidenFromF32(tyF, mkexpr(t4)));
                     } else
                        putFReg(fd, binop(Iop_RoundF32toInt, mkU32(0x1),
                                         getFReg(fs)));
                     break;

                  case 0x11:  /* D */
                     DIP("floor.w.d f%d, f%d", fd, fs);
                     calculateFCSR(fs, 0, FLOORWD, False, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I32);
                        assign(t0, binop(Iop_F64toI32S, mkU32(0x1),
                                         getDReg(fs)));
                        putFReg(fd, mkWidenFromF32(tyF,
                                    unop(Iop_ReinterpI32asF32, mkexpr(t0))));
                        break;
                     } else {
                        t0 = newTemp(Ity_I32);

                        assign(t0, binop(Iop_F64toI32S, mkU32(0x1),
                                         getDReg(fs)));

                        putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                        break;
                     }
                  default:
                     goto decode_failure;

               }
               break;  /* FLOOR.W.fmt */

            case 0x0D:  /* TRUNC.W */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("trunc.w.s %d, %d", fd, fs);
                     calculateFCSR(fs, 0, TRUNCWS, True, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I64);
                        t1 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_F32);
                        t4 = newTemp(Ity_F32);
                        /* get lo half of FPR */
                        assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                        assign(t1, unop(Iop_64to32, mkexpr(t0)));

                        assign(t3, unop(Iop_ReinterpI32asF32, mkexpr(t1)));

                        assign(t4, binop(Iop_RoundF32toInt, mkU32(0x3),
                                         mkexpr(t3)));

                        putFReg(fd, mkWidenFromF32(tyF, mkexpr(t4)));
                     } else
                        putFReg(fd, binop(Iop_RoundF32toInt, mkU32(0x3),
                                       getFReg(fs)));
                     break;
                  case 0x11:  /* D */
                     DIP("trunc.w.d %d, %d", fd, fs);
                     calculateFCSR(fs, 0, TRUNCWD, False, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I32);

                        assign(t0, binop(Iop_F64toI32S, mkU32(0x3),
                                         getFReg(fs)));

                        putFReg(fd, mkWidenFromF32(tyF,
                                    unop(Iop_ReinterpI32asF32, mkexpr(t0))));
                     } else {
                        t0 = newTemp(Ity_I32);

                        assign(t0, binop(Iop_F64toI32S, mkU32(0x3),
                                         getDReg(fs)));

                        putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                     }
                     break;
                  default:
                     goto decode_failure;

               }
               break;

            case 0x0E:  /* CEIL.W.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("ceil.w.s %d, %d", fd, fs);
                     calculateFCSR(fs, 0, CEILWS, True, 1);
                     if (fp_mode64) {
                        t0 = newTemp(Ity_I64);
                        t1 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_F32);
                        t4 = newTemp(Ity_F32);
                        /* get lo half of FPR */
                        assign(t0, unop(Iop_ReinterpF64asI64, getFReg(fs)));

                        assign(t1, unop(Iop_64to32, mkexpr(t0)));

                        assign(t3, unop(Iop_ReinterpI32asF32, mkexpr(t1)));

                        assign(t4, binop(Iop_RoundF32toInt, mkU32(0x2),
                                         mkexpr(t3)));

                        putFReg(fd, mkWidenFromF32(tyF, mkexpr(t4)));
                     } else
                        putFReg(fd, binop(Iop_RoundF32toInt, mkU32(0x2),
                                          getFReg(fs)));
                     break;

                  case 0x11:  /* D */
                     DIP("ceil.w.d %d, %d", fd, fs);
                     calculateFCSR(fs, 0, CEILWD, False, 1);
                     if (!fp_mode64) {
                        t0 = newTemp(Ity_I32);
                        assign(t0, binop(Iop_F64toI32S, mkU32(0x2),
                                         getDReg(fs)));
                        putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                     } else {
                        t0 = newTemp(Ity_I32);
                        assign(t0, binop(Iop_F64toI32S, mkU32(0x2),
                                         getDReg(fs)));
                        putFReg(fd, mkWidenFromF32(tyF,
                                    unop(Iop_ReinterpI32asF32, mkexpr(t0))));
                     }
                     break;
                  default:
                     goto decode_failure;

               }
               break;

            case 0x0A:  /* CEIL.L.fmt */
               switch (fmt) {
                  case 0x10:  /* S */
                     DIP("ceil.l.s %d, %d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, CEILLS, True, 1);
                        t0 = newTemp(Ity_I64);

                        assign(t0, binop(Iop_F32toI64S, mkU32(0x2),
                                   getLoFromF64(tyF, getFReg(fs))));

                        putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t0)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;

                  case 0x11:  /* D */
                     DIP("ceil.l.d %d, %d", fd, fs);
                     if (fp_mode64) {
                        calculateFCSR(fs, 0, CEILLD, False, 1);
                        putFReg(fd, binop(Iop_RoundF64toInt, mkU32(0x2),
                                          getFReg(fs)));
                     } else {
                        ILLEGAL_INSTRUCTON;
                     }
                     break;

                  default:
                     goto decode_failure;

               }
               break;

            case 0x16:  /* RSQRT.fmt */
               switch (fmt) {
                  case 0x10: {  /* S */
                     DIP("rsqrt.s %d, %d", fd, fs);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32, rm,
                                 unop(Iop_ReinterpI32asF32, mkU32(ONE_SINGLE)),
                                 binop(Iop_SqrtF32, rm, getLoFromF64(tyF,
                                 getFReg(fs))))));
                     break;
                  }
                  case 0x11: {  /* D */
                     DIP("rsqrt.d %d, %d", fd, fs);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_DivF64, rm,
                                 unop(Iop_ReinterpI64asF64,
                                 mkU64(ONE_DOUBLE)),
                                 binop(Iop_SqrtF64, rm, getDReg(fs))));
                     break;
                  }
                  default:
                     goto decode_failure;

               }
               break;

            default:
               if (dis_instr_CCondFmt(cins))
                  break;
               goto decode_failure;

            }

         }
      }
      break;  /* COP1 */
   case 0x10:  /* COP0 */
      if (rs == 0) {  /* MFC0 */
         DIP("mfc0 r%d, r%d, %d", rt, rd, sel);
         IRTemp   val  = newTemp(Ity_I32);
         IRExpr** args = mkIRExprVec_3 (IRExpr_BBPTR(), mkU32(rd), mkU32(sel));
         IRDirty *d = unsafeIRDirty_1_N(val,
                                        0,
                                        "mips32_dirtyhelper_mfc0",
                                        &mips32_dirtyhelper_mfc0,
                                        args);
         stmt(IRStmt_Dirty(d));
         putIReg(rt, mkexpr(val));
      } else if (rs == 1) {
         /* Doubleword Move from Coprocessor 0 - DMFC0; MIPS64 */
         DIP("dmfc0 r%d, r%d, %d", rt, rd, sel);
         IRTemp   val  = newTemp(Ity_I64);
         IRExpr** args = mkIRExprVec_3 (IRExpr_BBPTR(), mkU64(rd), mkU64(sel));
         IRDirty *d = unsafeIRDirty_1_N(val,
                                        0,
                                        "mips64_dirtyhelper_dmfc0",
                                        &mips64_dirtyhelper_dmfc0,
                                        args);
         stmt(IRStmt_Dirty(d));
         putDReg(rt, mkexpr(val));
      } else
         goto decode_failure;
      break;

   case 0x31:  /* LWC1 */
      /* Load Word to Floating Point - LWC1 (MIPS32) */
      DIP("lwc1 f%d, %d(r%d)", ft, imm, rs);
      if (fp_mode64) {
         t1 = newTemp(Ity_F32);
         t2 = newTemp(Ity_I64);
         if (mode64) {
            t0 = newTemp(Ity_I64);
            /* new LO */
            assign(t0, binop(Iop_Add64, getIReg(rs),
                             mkU64(extend_s_16to64(imm))));
         } else {
            t0 = newTemp(Ity_I32);
            /* new LO */
            assign(t0, binop(Iop_Add32, getIReg(rs),
                             mkU32(extend_s_16to32(imm))));
         }
         assign(t1, load(Ity_F32, mkexpr(t0)));
         assign(t2, mkWidenFrom32(Ity_I64, unop(Iop_ReinterpF32asI32,
                                                mkexpr(t1)), True));
         putDReg(ft, unop(Iop_ReinterpI64asF64, mkexpr(t2)));
      } else {
         t0 = newTemp(Ity_I32);
         assign(t0, binop(Iop_Add32, getIReg(rs),
                           mkU32(extend_s_16to32(imm))));
         putFReg(ft, load(Ity_F32, mkexpr(t0)));
      }
      break;

   case 0x39:  /* SWC1 */
      DIP("swc1 f%d, %d(r%d)", ft, imm, rs);
      if (fp_mode64) {
         t0 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I32);
         LOAD_STORE_PATTERN;
         assign(t0, unop(Iop_ReinterpF64asI64, getFReg(ft)));
         assign(t2, unop(Iop_64to32, mkexpr(t0)));
         store(mkexpr(t1), unop(Iop_ReinterpI32asF32, mkexpr(t2)));
      } else {
         LOAD_STORE_PATTERN;
         store(mkexpr(t1), getFReg(ft));
      }
      break;

   case 0x33:  /* PREF */
      DIP("pref");
      break;

   case 0x35:
      /* Load Doubleword to Floating Point - LDC1 (MIPS32) */
      DIP("ldc1 f%d, %d(%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      putDReg(ft, load(Ity_F64, mkexpr(t1)));
      break;

   case 0x3D:
      /* Store Doubleword from Floating Point - SDC1 */
      DIP("sdc1 f%d, %d(%d)", ft, imm, rs);
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), getDReg(ft));
      break;

   case 0x23:  /* LW */
      DIP("lw r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      putIReg(rt, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)), True));
      break;

   case 0x20:  /* LB */
      DIP("lb r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      if (mode64)
         putIReg(rt, unop(Iop_8Sto64, load(Ity_I8, mkexpr(t1))));
      else
         putIReg(rt, unop(Iop_8Sto32, load(Ity_I8, mkexpr(t1))));
      break;

   case 0x24:  /* LBU */
      DIP("lbu r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      if (mode64)
         putIReg(rt, unop(Iop_8Uto64, load(Ity_I8, mkexpr(t1))));
      else
         putIReg(rt, unop(Iop_8Uto32, load(Ity_I8, mkexpr(t1))));
      break;

   case 0x21:  /* LH */
      DIP("lh r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      if (mode64)
         putIReg(rt, unop(Iop_16Sto64, load(Ity_I16, mkexpr(t1))));
      else
         putIReg(rt, unop(Iop_16Sto32, load(Ity_I16, mkexpr(t1))));
      break;

   case 0x25:  /* LHU */
      DIP("lhu r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      if (mode64)
         putIReg(rt, unop(Iop_16Uto64, load(Ity_I16, mkexpr(t1))));
      else
         putIReg(rt, unop(Iop_16Uto32, load(Ity_I16, mkexpr(t1))));
      break;

   case 0x0F:  /* LUI */
      p = (imm << 16);
      DIP("lui r%d, imm: 0x%x", rt, imm);
      if (mode64)
         putIReg(rt, mkU64(extend_s_32to64(p)));
      else
         putIReg(rt, mkU32(p));
      break;

   case 0x13:  /* COP1X */
      switch (function) {
      case 0x0: {  /* LWXC1 */
         /* Load Word  Indexed to Floating Point - LWXC1 (MIPS32r2) */
         DIP("lwxc1 f%d, r%d(r%d)", fd, rt, rs);
         if (fp_mode64) {
            t0 = newTemp(Ity_I64);
            t1 = newTemp(Ity_I32);
            t3 = newTemp(Ity_F32);
            t4 = newTemp(Ity_I64);

            t2 = newTemp(ty);
            /* new LO */
            assign(t2, binop(mode64 ? Iop_Add64 : Iop_Add32, getIReg(rs),
                             getIReg(rt)));
            assign(t3, load(Ity_F32, mkexpr(t2)));

            assign(t4, mkWidenFrom32(Ity_I64, unop(Iop_ReinterpF32asI32,
                                                   mkexpr(t3)), True));

            putFReg(fd, unop(Iop_ReinterpI64asF64, mkexpr(t4)));
         } else {
            t0 = newTemp(Ity_I32);
            assign(t0, binop(Iop_Add32, getIReg(rs), getIReg(rt)));
            putFReg(fd, load(Ity_F32, mkexpr(t0)));
         }
         break;
      }

      case 0x1: {  /* LDXC1 */
         /* Load Doubleword  Indexed to Floating Point
            LDXC1 (MIPS32r2 and MIPS64) */
         if (fp_mode64) {
            DIP("ldxc1 f%d, r%d(r%d)", fd, rt, rs);
            t0 = newTemp(ty);
            assign(t0, binop(mode64 ? Iop_Add64 : Iop_Add32, getIReg(rs),
                             getIReg(rt)));
            putFReg(fd, load(Ity_F64, mkexpr(t0)));
            break;
         } else {
            t0 = newTemp(Ity_I32);
            assign(t0, binop(Iop_Add32, getIReg(rs), getIReg(rt)));

            t1 = newTemp(Ity_I32);
            assign(t1, binop(Iop_Add32, mkexpr(t0), mkU32(4)));

#if defined (_MIPSEL)
            putFReg(fd, load(Ity_F32, mkexpr(t0)));
            putFReg(fd + 1, load(Ity_F32, mkexpr(t1)));
#elif defined (_MIPSEB)
            putFReg(fd + 1, load(Ity_F32, mkexpr(t0)));
            putFReg(fd, load(Ity_F32, mkexpr(t1)));
#endif
            break;
         }
      }

      case 0x5:  /* Load Doubleword Indexed Unaligned to Floating Point - LUXC1;
                    MIPS32r2 */
         DIP("luxc1 f%d, r%d(r%d)", fd, rt, rs);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         assign(t0, binop(Iop_Add64, getIReg(rs), getIReg(rt)));
         assign(t1, binop(Iop_And64, mkexpr(t0),
                                     mkU64(0xfffffffffffffff8ULL)));
         putFReg(fd, load(Ity_F64, mkexpr(t1)));
         break;

      case 0x8: {  /* Store Word Indexed from Floating Point - SWXC1 */
         DIP("swxc1 f%d, r%d(r%d)", ft, rt, rs);
         if (fp_mode64) {
            t0 = newTemp(ty);
            assign(t0, binop(mode64 ? Iop_Add64 : Iop_Add32, getIReg(rs),
                             getIReg(rt)));
            store(mkexpr(t0), getLoFromF64(tyF, getFReg(fs)));

         } else {
            t0 = newTemp(Ity_I32);
            assign(t0, binop(Iop_Add32, getIReg(rs), getIReg(rt)));

            store(mkexpr(t0), getFReg(fs));
         }
         break;
      }
      case 0x9: {  /* Store Doubleword Indexed from Floating Point - SDXC1 */
         DIP("sdc1 f%d, %d(%d)", ft, imm, rs);
         if (fp_mode64) {
            t0 = newTemp(ty);
            assign(t0, binop(mode64 ? Iop_Add64 : Iop_Add32, getIReg(rs),
                             getIReg(rt)));
            store(mkexpr(t0), getFReg(fs));
         } else {
            t0 = newTemp(Ity_I32);
            assign(t0, binop(Iop_Add32, getIReg(rs), getIReg(rt)));

            t1 = newTemp(Ity_I32);
            assign(t1, binop(Iop_Add32, mkexpr(t0), mkU32(4)));

#if defined (_MIPSEL)
            store(mkexpr(t0), getFReg(fs));
            store(mkexpr(t1), getFReg(fs + 1));
#elif defined (_MIPSEB)
            store(mkexpr(t0), getFReg(fs + 1));
            store(mkexpr(t1), getFReg(fs));
#endif
         }
         break;
      }
      case 0xD:  /* Store Doubleword Indexed Unaligned from Floating Point -
                    SUXC1; MIPS64 MIPS32r2 */
         DIP("suxc1 f%d, r%d(r%d)", fd, rt, rs);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         assign(t0, binop(Iop_Add64, getIReg(rs), getIReg(rt)));
         assign(t1, binop(Iop_And64, mkexpr(t0), mkU64(0xfffffffffffffff8ULL)));
         store(mkexpr(t1), getFReg(fs));
         break;

      case 0x0F: {
         DIP("prefx");
         break;
      }
      case 0x20:  {  /* MADD.S */
         DIP("madd.s f%d, f%d, f%d, f%d", fd, fmt, fs, ft);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F32);
         assign(t1, qop(Iop_MAddF32, rm,
                        getLoFromF64(tyF, getFReg(fmt)),
                        getLoFromF64(tyF, getFReg(fs)),
                        getLoFromF64(tyF, getFReg(ft))));
         putFReg(fd, mkWidenFromF32(tyF, mkexpr(t1)));
         break;  /* MADD.S */
      }
      case 0x21: {  /* MADD.D */
         DIP("madd.d f%d, f%d, f%d, f%d", fd, fmt, fs, ft);
         IRExpr *rm = get_IR_roundingmode();
         putDReg(fd, qop(Iop_MAddF64, rm, getDReg(fmt), getDReg(fs),
                         getDReg(ft)));
         break;  /* MADD.D */
      }
      case 0x28: {  /* MSUB.S */
         DIP("msub.s f%d, f%d, f%d, f%d", fd, fmt, fs, ft);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F32);
         assign(t1, qop(Iop_MSubF32, rm,
                        getLoFromF64(tyF, getFReg(fmt)),
                        getLoFromF64(tyF, getFReg(fs)),
                        getLoFromF64(tyF, getFReg(ft))));
         putFReg(fd, mkWidenFromF32(tyF, mkexpr(t1)));
         break;  /* MSUB.S */
      }
      case 0x29: {  /* MSUB.D */
         DIP("msub.d f%d, f%d, f%d, f%d", fd, fmt, fs, ft);
         IRExpr *rm = get_IR_roundingmode();
         putDReg(fd, qop(Iop_MSubF64, rm, getDReg(fmt), getDReg(fs),
                         getDReg(ft)));
         break;  /* MSUB.D */
      }
      case 0x30: {  /* NMADD.S */
         DIP("nmadd.s f%d, f%d, f%d, f%d", fd, fmt, fs, ft);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F32);
         assign(t1, qop(Iop_MAddF32, rm,
                        getLoFromF64(tyF, getFReg(fmt)),
                        getLoFromF64(tyF, getFReg(fs)),
                        getLoFromF64(tyF, getFReg(ft))));

         putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32, mkexpr(t1))));
         break;  /* NMADD.S */
      }
      case 0x31: {  /* NMADD.D */
         DIP("nmadd.d f%d, f%d, f%d, f%d", fd, fmt, fs, ft);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F64);
         assign(t1, qop(Iop_MAddF64, rm, getDReg(fmt), getDReg(fs),
                        getDReg(ft)));
         putDReg(fd, unop(Iop_NegF64, mkexpr(t1)));
         break;  /* NMADD.D */
      }
      case 0x38: {  /* NMSUBB.S */
         DIP("nmsub.s f%d, f%d, f%d, f%d", fd, fmt, fs, ft);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F32);
         assign(t1, qop(Iop_MSubF32, rm,
                        getLoFromF64(tyF, getFReg(fmt)),
                        getLoFromF64(tyF, getFReg(fs)),
                        getLoFromF64(tyF, getFReg(ft))));

         putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32, mkexpr(t1))));
         break;  /* NMSUBB.S */
      }
      case 0x39: {  /* NMSUBB.D */
         DIP("nmsub.d f%d, f%d, f%d, f%d", fd, fmt, fs, ft);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F64);
         assign(t1, qop(Iop_MSubF64, rm, getDReg(fmt), getDReg(fs),
                        getDReg(ft)));
         putDReg(fd, unop(Iop_NegF64, mkexpr(t1)));
         break;  /* NMSUBB.D */
      }

      default:
         goto decode_failure;
      }
      break;

   case 0x22:  /* LWL */
      DIP("lwl r%d, %d(r%d)", rt, imm, rs);
      if (mode64) {
         /* t1 = addr */
         t1 = newTemp(Ity_I64);
#if defined (_MIPSEL)
         assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
         /* t2 = word addr */
         /* t4 = addr mod 4 */
         LWX_SWX_PATTERN64;

         /* t3 = word content - shifted */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_Shl32, mkNarrowTo32(ty, load(Ity_I64,
                          mkexpr(t2))), narrowTo(Ity_I8, binop(Iop_Shl32,
                    binop(Iop_Sub32, mkU32(0x03), mkexpr(t4)), mkU8(3)))));

         /* rt content - adjusted */
         t5 = newTemp(Ity_I32);
         assign(t5, binop(Iop_And32,
                          mkNarrowTo32(ty, getIReg(rt)),
                          binop(Iop_Shr32,
                                mkU32(0x00FFFFFF),
                                      narrowTo(Ity_I8, binop(Iop_Mul32,
                                                             mkU32(0x08),
                                                             mkexpr(t4))))));

         putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t5),
                                             mkexpr(t3)), True));
#elif defined (_MIPSEB)
         assign(t1, binop(Iop_Xor64, mkU64(0x3),
                binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm)))));
         /* t2 = word addr */
         /* t4 = addr mod 4 */
         LWX_SWX_PATTERN64;

         /* t3 = word content - shifted */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_Shl32, unop(Iop_64HIto32, load(Ity_I64,
                          mkexpr(t2))), narrowTo(Ity_I8, binop(Iop_Shl32,
                    binop(Iop_Sub32, mkU32(0x03), mkexpr(t4)), mkU8(3)))));

         /* rt content - adjusted */
         t5 = newTemp(Ity_I32);
         assign(t5, binop(Iop_And32,
                          mkNarrowTo32(ty, getIReg(rt)),
                          binop(Iop_Shr32,
                                mkU32(0x00FFFFFF),
                                      narrowTo(Ity_I8, binop(Iop_Mul32,
                                                             mkU32(0x08),
                                                             mkexpr(t4))))));

         putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t5),
                                             mkexpr(t3)), True));
#endif
      } else {
         /* t1 = addr */
         t1 = newTemp(Ity_I32);
#if defined (_MIPSEL)
         assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));
#elif defined (_MIPSEB)
         assign(t1, binop(Iop_Xor32, mkU32(0x3), binop(Iop_Add32, getIReg(rs),
                                     mkU32(extend_s_16to32(imm)))));
#endif

         /* t2 = word addr */
         /* t4 = addr mod 4 */
         LWX_SWX_PATTERN;

         /* t3 = word content - shifted */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_Shl32, load(Ity_I32, mkexpr(t2)), narrowTo(Ity_I8,
                    binop(Iop_Shl32, binop(Iop_Sub32, mkU32(0x03), mkexpr(t4)),
                    mkU8(3)))));

         /* rt content  - adjusted */
         t5 = newTemp(Ity_I32);
         assign(t5, binop(Iop_And32,
                          getIReg(rt),
                          binop(Iop_Shr32,
                                mkU32(0x00FFFFFF),
                                      narrowTo(Ity_I8, binop(Iop_Mul32,
                                                             mkU32(0x08),
                                                             mkexpr(t4))))));

         putIReg(rt, binop(Iop_Or32, mkexpr(t5), mkexpr(t3)));
      }
      break;

   case 0x26:  /* LWR */
      DIP("lwr r%d, %d(r%d)", rt, imm, rs);
      if (mode64) {
         /* t1 = addr */
         t1 = newTemp(Ity_I64);
#if defined (_MIPSEL)
         assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
         /* t2 = word addr */
         /* t4 = addr mod 8 */
         LWX_SWX_PATTERN64;

         /* t3 = word content - shifted */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_Shr32, mkNarrowTo32(ty, load(Ity_I64,mkexpr(t2))),
                    narrowTo(Ity_I8, binop(Iop_Shl32, mkexpr(t4), mkU8(3)))));

         /* rt content  - adjusted */
         t5 = newTemp(Ity_I32);
         assign(t5, binop(Iop_And32, mkNarrowTo32(ty, getIReg(rt)),
                unop(Iop_Not32, binop(Iop_Shr32, mkU32(0xFFFFFFFF),
                narrowTo(Ity_I8, binop(Iop_Shl32, mkexpr(t4), mkU8(0x3)))))));

         putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t5),
                                       mkexpr(t3)), True));
#elif defined (_MIPSEB)
         assign(t1, binop(Iop_Xor64, mkU64(0x3), binop(Iop_Add64, getIReg(rs),
                          mkU64(extend_s_16to64(imm)))));
         /* t2 = word addr */
         /* t4 = addr mod 4 */
         LWX_SWX_PATTERN64;

         /* t3 = word content - shifted */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_Shr32, unop(Iop_64HIto32, load(Ity_I64,mkexpr(t2))),
                    narrowTo(Ity_I8, binop(Iop_Shl32, mkexpr(t4), mkU8(3)))));

         /* rt content  - adjusted */
         t5 = newTemp(Ity_I32);
         assign(t5, binop(Iop_And32, mkNarrowTo32(ty, getIReg(rt)),
                unop(Iop_Not32, binop(Iop_Shr32, mkU32(0xFFFFFFFF),
                narrowTo(Ity_I8, binop(Iop_Shl32, mkexpr(t4), mkU8(0x3)))))));

         putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t5),
                                       mkexpr(t3)), True));
#endif

      } else {
         /* t1 = addr */
         t1 = newTemp(Ity_I32);
#if defined (_MIPSEL)
         assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));
#elif defined (_MIPSEB)
         assign(t1, binop(Iop_Xor32, mkU32(0x3), binop(Iop_Add32, getIReg(rs),
                                     mkU32(extend_s_16to32(imm)))));
#endif

         /* t2 = word addr */
         /* t4 = addr mod 4 */
         LWX_SWX_PATTERN;

         /* t3 = word content - shifted */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_Shr32, load(Ity_I32, mkexpr(t2)),
                    narrowTo(Ity_I8, binop(Iop_Shl32, mkexpr(t4),
                    mkU8(3)))));

         /* rt content  - adjusted */
         t5 = newTemp(Ity_I32);
         assign(t5, binop(Iop_And32, getIReg(rt), unop(Iop_Not32,
                    binop(Iop_Shr32, mkU32(0xFFFFFFFF), narrowTo(Ity_I8,
                          binop(Iop_Shl32, mkexpr(t4), mkU8(0x3)))))));

         putIReg(rt, binop(Iop_Or32, mkexpr(t5), mkexpr(t3)));
      }
      break;

   case 0x2B:  /* SW */
      DIP("sw r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), mkNarrowTo32(ty, getIReg(rt)));
      break;

   case 0x2C: {  /* SDL rt, offset(base) MIPS64 */
      DIP("sdl r%u, %d(r%u)", rt, (Int) imm, rs);
      vassert(mode64);
      IRTemp A_byte = newTemp(Ity_I8);
      IRTemp B_byte = newTemp(Ity_I8);
      IRTemp C_byte = newTemp(Ity_I8);
      IRTemp D_byte = newTemp(Ity_I8);
      IRTemp E_byte = newTemp(Ity_I8);
      IRTemp F_byte = newTemp(Ity_I8);
      IRTemp G_byte = newTemp(Ity_I8);
      IRTemp H_byte = newTemp(Ity_I8);
      IRTemp B_pos  = newTemp(Ity_I64);
      IRTemp C_pos  = newTemp(Ity_I64);
      IRTemp D_pos  = newTemp(Ity_I64);
      IRTemp E_pos  = newTemp(Ity_I64);
      IRTemp F_pos  = newTemp(Ity_I64);
      IRTemp G_pos  = newTemp(Ity_I64);

      /* H byte */
      assign(H_byte, getByteFromReg(rt, 0));
      /* G byte */
      assign(G_byte, getByteFromReg(rt, 1));
      /* F byte */
      assign(F_byte, getByteFromReg(rt, 2));
      /* E byte */
      assign(E_byte, getByteFromReg(rt, 3));
      /* D byte */
      assign(D_byte, getByteFromReg(rt, 4));
      /* C byte */
      assign(C_byte, getByteFromReg(rt, 5));
      /* B byte */
      assign(B_byte, getByteFromReg(rt, 6));
      /* A byte */
      assign(A_byte, getByteFromReg(rt, 7));

      /* t1 = addr */
      t1 = newTemp(Ity_I64);
      assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));

      /* t2 = word addr */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFF8ULL)));

      /* t3 = addr mod 7 */
      t3 = newTemp(Ity_I64);
      assign(t3, binop(Iop_And64, mkexpr(t1), mkU64(0x7)));

#if defined (_MIPSEL)
      /* Calculate X_byte position. */
      assign(B_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x1)),
                               mkU64(0x0),
                               mkU64(0x1)));

      assign(C_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x2)),
                               mkU64(0x0),
                               mkU64(0x2)));

      assign(D_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x3)),
                               mkU64(0x0),
                               mkU64(0x3)));

      assign(E_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x4)),
                               mkU64(0x0),
                               mkU64(0x4)));

      assign(F_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x5)),
                               mkU64(0x0),
                               mkU64(0x5)));

      assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x7)),
                               mkU64(0x1),
                               mkU64(0x0)));

      /* Store X_byte on the right place. */
      store(mkexpr(t2), mkexpr(H_byte));
      store(binop(Iop_Add64, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(E_pos)), mkexpr(E_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(D_pos)), mkexpr(D_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(C_pos)), mkexpr(C_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(B_pos)), mkexpr(B_byte));
      store(mkexpr(t1), mkexpr(A_byte));

#else /* _MIPSEB */
      /* Calculate X_byte position. */
      assign(B_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x7)),
                               mkU64(0x0),
                               mkU64(0x1)));

      assign(C_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x6)),
                               mkU64(0x2),
                               mkU64(0x0)));

      assign(D_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x5)),
                               mkU64(0x3),
                               mkU64(0x0)));

      assign(E_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x4)),
                               mkU64(0x4),
                               mkU64(0x0)));

      assign(F_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkexpr(t3), mkU64(0x3)),
                               mkU64(0x5),
                               mkU64(0x0)));

      assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                               mkU64(0x6),
                               mkU64(0x7)));

      /* Store X_byte on the right place. */
      store(binop(Iop_Add64, mkexpr(t2), mkU64(0x7)), mkexpr(H_byte));
      store(binop(Iop_Add64, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(E_pos)), mkexpr(E_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(D_pos)), mkexpr(D_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(C_pos)), mkexpr(C_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(B_pos)), mkexpr(B_byte));
      store(mkexpr(t1), mkexpr(A_byte));
#endif

      break;
   }

   case 0x2D: {
      /* SDR rt, offset(base) - MIPS64 */
      vassert(mode64);
      DIP("sdr r%u, %d(r%u)", rt, imm, rs);
      IRTemp A_byte = newTemp(Ity_I8);
      IRTemp B_byte = newTemp(Ity_I8);
      IRTemp C_byte = newTemp(Ity_I8);
      IRTemp D_byte = newTemp(Ity_I8);
      IRTemp E_byte = newTemp(Ity_I8);
      IRTemp F_byte = newTemp(Ity_I8);
      IRTemp G_byte = newTemp(Ity_I8);
      IRTemp H_byte = newTemp(Ity_I8);
      IRTemp B_pos  = newTemp(Ity_I64);
      IRTemp C_pos  = newTemp(Ity_I64);
      IRTemp D_pos  = newTemp(Ity_I64);
      IRTemp E_pos  = newTemp(Ity_I64);
      IRTemp F_pos  = newTemp(Ity_I64);
      IRTemp G_pos  = newTemp(Ity_I64);

      /* H byte */
      assign(H_byte, getByteFromReg(rt, 0));
      /* G byte */
      assign(G_byte, getByteFromReg(rt, 1));
      /* F byte */
      assign(F_byte, getByteFromReg(rt, 2));
      /* E byte */
      assign(E_byte, getByteFromReg(rt, 3));
      /* D byte */
      assign(D_byte, getByteFromReg(rt, 4));
      /* C byte */
      assign(C_byte, getByteFromReg(rt, 5));
      /* B byte */
      assign(B_byte, getByteFromReg(rt, 6));
      /* A byte */
      assign(A_byte, getByteFromReg(rt, 7));

      /* t1 = addr */
      t1 = newTemp(Ity_I64);
      assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));

      /* t2 = word addr */
      t2 = newTemp(Ity_I64);
      assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFF8ULL)));

      /* t3 = addr mod 7 */
      t3 = newTemp(Ity_I64);
      assign(t3, binop(Iop_And64, mkexpr(t1), mkU64(0x7)));

#if defined (_MIPSEL)
      /* Calculate X_byte position. */
      assign(B_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x1), mkexpr(t3)),
                               mkU64(0x0),
                               mkU64(0x6)));

      assign(C_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x2), mkexpr(t3)),
                               mkU64(0x0),
                               mkU64(0x5)));

      assign(D_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x3), mkexpr(t3)),
                               mkU64(0x0),
                               mkU64(0x4)));

      assign(E_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x4), mkexpr(t3)),
                               mkU64(0x0),
                               mkU64(0x3)));

      assign(F_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x5), mkexpr(t3)),
                               mkU64(0x0),
                               mkU64(0x2)));

      assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x7)),
                               mkU64(0x0),
                               mkU64(0x1)));

      /* Store X_byte on the right place. */
      store(binop(Iop_Add64, mkexpr(t2), mkU64(0x7)), mkexpr(A_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(B_pos)), mkexpr(B_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(C_pos)), mkexpr(C_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(D_pos)), mkexpr(D_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(E_pos)), mkexpr(E_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
      store(binop(Iop_Add64, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
      store(mkexpr(t1), mkexpr(H_byte));

#else /* _MIPSEB */
      /* Calculate X_byte position. */
      assign(B_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x5), mkexpr(t3)),
                               mkU64(0x6),
                               mkU64(0x0)));

      assign(C_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x4), mkexpr(t3)),
                               mkU64(0x5),
                               mkU64(0x0)));

      assign(D_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x3), mkexpr(t3)),
                               mkU64(0x4),
                               mkU64(0x0)));

      assign(E_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x2), mkexpr(t3)),
                               mkU64(0x3),
                               mkU64(0x0)));

      assign(F_pos, IRExpr_ITE(binop(Iop_CmpLT64U, mkU64(0x1), mkexpr(t3)),
                               mkU64(0x2),
                               mkU64(0x0)));

      assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                               mkU64(0x0),
                               mkU64(0x1)));

      /* Store X_byte on the right place. */
      store(mkexpr(t2), mkexpr(A_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(B_pos)), mkexpr(B_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(C_pos)), mkexpr(C_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(D_pos)), mkexpr(D_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(E_pos)), mkexpr(E_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
      store(binop(Iop_Sub64, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
      store(mkexpr(t1), mkexpr(H_byte));
#endif
      break;
   }

   case 0x28:  /* SB */
      DIP("sb r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), narrowTo(Ity_I8, getIReg(rt)));
      break;

   case 0x29:  /* SH */
      DIP("sh r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), narrowTo(Ity_I16, getIReg(rt)));
      break;

   case 0x2A:  /* SWL */
      DIP("swl r%d, %d(r%d)", rt, imm, rs);
      if (mode64) {
         IRTemp E_byte = newTemp(Ity_I8);
         IRTemp F_byte = newTemp(Ity_I8);
         IRTemp G_byte = newTemp(Ity_I8);
         IRTemp H_byte = newTemp(Ity_I8);
         IRTemp F_pos  = newTemp(Ity_I64);
         IRTemp G_pos  = newTemp(Ity_I64);

         /* H byte */
         assign(H_byte, getByteFromReg(rt, 0));
         /* G byte */
         assign(G_byte, getByteFromReg(rt, 1));
         /* F byte */
         assign(F_byte, getByteFromReg(rt, 2));
         /* E byte */
         assign(E_byte, getByteFromReg(rt, 3));

         /* t1 = addr */
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));

         /* t2 = word addr */
         t2 = newTemp(Ity_I64);
         assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFFCULL)));

         /* t3 = addr mod 4 */
         t3 = newTemp(Ity_I64);
         assign(t3, binop(Iop_And64, mkexpr(t1), mkU64(0x3)));

#if defined (_MIPSEL)
         /* Calculate X_byte position. */
         assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                  mkU64(0x0),
                                  mkU64(0x1)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x3)),
                                  mkU64(0x1),
                                  mkU64(0x0)));

         /* Store X_byte on the right place. */
         store(mkexpr(t2), mkexpr(H_byte));
         store(binop(Iop_Add64, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
         store(mkexpr(t1), mkexpr(E_byte));

#else    /* _MIPSEB */
         /* Calculate X_byte position. */
         assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x3)),
                                  mkU64(0x0),
                                  mkU64(0x1)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                  mkU64(0x2),
                                  mkU64(0x3)));

         store(binop(Iop_Add64, mkexpr(t2), mkU64(3)), mkexpr(H_byte));
         store(binop(Iop_Add64, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
         store(mkexpr(t1), mkexpr(E_byte));

#endif
      } else {
         IRTemp E_byte = newTemp(Ity_I8);
         IRTemp F_byte = newTemp(Ity_I8);
         IRTemp G_byte = newTemp(Ity_I8);
         IRTemp H_byte = newTemp(Ity_I8);
         IRTemp F_pos  = newTemp(Ity_I32);
         IRTemp G_pos  = newTemp(Ity_I32);

         /* H byte */
         assign(H_byte, getByteFromReg(rt, 0));
         /* G byte */
         assign(G_byte, getByteFromReg(rt, 1));
         /* F byte */
         assign(F_byte, getByteFromReg(rt, 2));
         /* E byte */
         assign(E_byte, getByteFromReg(rt, 3));

         /* t1 = addr */
         t1 = newTemp(Ity_I32);
         assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));

         /* t2 = word addr */
         t2 = newTemp(Ity_I32);
         assign(t2, binop(Iop_And32, mkexpr(t1), mkU32(0xFFFFFFFCULL)));

         /* t3 = addr mod 4 */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_And32, mkexpr(t1), mkU32(0x3)));

#if defined (_MIPSEL)
         /* Calculate X_byte position. */
         assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x0)),
                                  mkU32(0x0),
                                  mkU32(0x1)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x3)),
                                  mkU32(0x1),
                                  mkU32(0x0)));

         /* Store X_byte on the right place. */
         store(mkexpr(t2), mkexpr(H_byte));
         store(binop(Iop_Add32, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
         store(binop(Iop_Sub32, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
         store(mkexpr(t1), mkexpr(E_byte));

#else    /* _MIPSEB */
         /* Calculate X_byte position. */
         assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x3)),
                                  mkU32(0x0),
                                  mkU32(0x1)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x0)),
                                  mkU32(0x2),
                                  mkU32(0x3)));

         store(binop(Iop_Add32, mkexpr(t2), mkU32(3)), mkexpr(H_byte));
         store(binop(Iop_Add32, mkexpr(t2), mkexpr(G_pos)), mkexpr(G_byte));
         store(binop(Iop_Add32, mkexpr(t1), mkexpr(F_pos)), mkexpr(F_byte));
         store(mkexpr(t1), mkexpr(E_byte));

#endif
      }
      break;

   case 0x2E:  /* SWR */
      DIP("swr r%d, %d(r%d)", rt, imm, rs);
      if (mode64) {
         IRTemp E_byte = newTemp(Ity_I8);
         IRTemp F_byte = newTemp(Ity_I8);
         IRTemp G_byte = newTemp(Ity_I8);
         IRTemp H_byte = newTemp(Ity_I8);
         IRTemp F_pos  = newTemp(Ity_I64);
         IRTemp G_pos  = newTemp(Ity_I64);

         /* H byte */
         assign(H_byte, getByteFromReg(rt, 0));
         /* G byte */
         assign(G_byte, getByteFromReg(rt, 1));
         /* F byte */
         assign(F_byte, getByteFromReg(rt, 2));
         /* E byte */
         assign(E_byte, getByteFromReg(rt, 3));

         /* t1 = addr */
         t1 = newTemp(Ity_I64);
         assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));

         /* t2 = word addr */
         t2 = newTemp(Ity_I64);
         assign(t2, binop(Iop_And64, mkexpr(t1), mkU64(0xFFFFFFFFFFFFFFFCULL)));

         /* t3 = addr mod 4 */
         t3 = newTemp(Ity_I64);
         assign(t3, binop(Iop_And64, mkexpr(t1), mkU64(0x3)));

#if defined (_MIPSEL)
         /* Calculate X_byte position. */
         assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                  mkU64(0x2),
                                  mkU64(0x3)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x3)),
                                  mkU64(0x0),
                                  mkU64(0x1)));

         /* Store X_byte on the right place. */
         store(binop(Iop_Add64, mkexpr(t2), mkU64(0x3)), mkexpr(E_byte));
         store(binop(Iop_Add64, mkexpr(t2), mkexpr(F_pos)), mkexpr(F_byte));
         store(binop(Iop_Add64, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
         store(mkexpr(t1), mkexpr(H_byte));

#else    /* _MIPSEB */
         /* Calculate X_byte position. */
         assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x3)),
                                  mkU64(0x1),
                                  mkU64(0x0)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ64, mkexpr(t3), mkU64(0x0)),
                                  mkU64(0x0),
                                  mkU64(0x1)));

         /* Store X_byte on the right place. */
         store(mkexpr(t2), mkexpr(E_byte));
         store(binop(Iop_Add64, mkexpr(t2), mkexpr(F_pos)), mkexpr(F_byte));
         store(binop(Iop_Sub64, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
         store(mkexpr(t1), mkexpr(H_byte));
#endif
      } else {
         IRTemp E_byte = newTemp(Ity_I8);
         IRTemp F_byte = newTemp(Ity_I8);
         IRTemp G_byte = newTemp(Ity_I8);
         IRTemp H_byte = newTemp(Ity_I8);
         IRTemp F_pos  = newTemp(Ity_I32);
         IRTemp G_pos  = newTemp(Ity_I32);

         /* H byte */
         assign(H_byte, getByteFromReg(rt, 0));
         /* G byte */
         assign(G_byte, getByteFromReg(rt, 1));
         /* F byte */
         assign(F_byte, getByteFromReg(rt, 2));
         /* E byte */
         assign(E_byte, getByteFromReg(rt, 3));

         /* t1 = addr */
         t1 = newTemp(Ity_I32);
         assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));

         /* t2 = word addr */
         t2 = newTemp(Ity_I32);
         assign(t2, binop(Iop_And32, mkexpr(t1), mkU32(0xFFFFFFFCULL)));

         /* t3 = addr mod 4 */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_And32, mkexpr(t1), mkU32(0x3)));

#if defined (_MIPSEL)
         /* Calculate X_byte position. */
         assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x0)),
                                  mkU32(0x2),
                                  mkU32(0x3)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x3)),
                                  mkU32(0x0),
                                  mkU32(0x1)));

         /* Store X_byte on the right place. */
         store(binop(Iop_Add32, mkexpr(t2), mkU32(0x3)), mkexpr(E_byte));
         store(binop(Iop_Add32, mkexpr(t2), mkexpr(F_pos)), mkexpr(F_byte));
         store(binop(Iop_Add32, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
         store(mkexpr(t1), mkexpr(H_byte));

#else    /* _MIPSEB */
         /* Calculate X_byte position. */
         assign(F_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x3)),
                                  mkU32(0x1),
                                  mkU32(0x0)));

         assign(G_pos, IRExpr_ITE(binop(Iop_CmpEQ32, mkexpr(t3), mkU32(0x0)),
                                  mkU32(0x0),
                                  mkU32(0x1)));

         /* Store X_byte on the right place. */
         store(mkexpr(t2), mkexpr(E_byte));
         store(binop(Iop_Add32, mkexpr(t2), mkexpr(F_pos)), mkexpr(F_byte));
         store(binop(Iop_Sub32, mkexpr(t1), mkexpr(G_pos)), mkexpr(G_byte));
         store(mkexpr(t1), mkexpr(H_byte));
#endif
      }
      break;

   case 0x1C:  /* Special2 */
      switch (function) {
      /* Cavium Specific instructions */
      case 0x03: case 0x32: case 0x33:  /* DMUL, CINS , CINS32 */
      case 0x3A: case 0x3B: case 0x2B:  /* EXT,  EXT32, SNE    */
      /* CVM Compare Instructions */
      case 0x2A: case 0x2E: case 0x2F:  /* SEQ,  SEQI,  SNEI   */
         if (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_CAVIUM) {
            if (dis_instr_CVM(cins))
               break;
            goto decode_failure;
         } else {
            goto decode_failure;
         }
         break;
      case 0x02: {  /* MUL */
         DIP("mul r%d, r%d, r%d", rd, rs, rt);
         if (mode64) {
            IRTemp tmpRs32 = newTemp(Ity_I32);
            IRTemp tmpRt32 = newTemp(Ity_I32);
            IRTemp tmpRes = newTemp(Ity_I32);

            assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpRes, binop(Iop_Mul32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpRes), True));
         } else
            putIReg(rd, binop(Iop_Mul32, getIReg(rs), getIReg(rt)));
         break;
      }

         case 0x00: {  /* MADD */
            if (mode64) {
               DIP("madd r%d, r%d", rs, rt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I64);
               t5 = newTemp(Ity_I64);
               t6 = newTemp(Ity_I32);

               assign(t1, mkNarrowTo32(ty, getHI()));
               assign(t2, mkNarrowTo32(ty, getLO()));

               assign(t3, binop(Iop_MullS32, mkNarrowTo32(ty, getIReg(rs)),
                                             mkNarrowTo32(ty, getIReg(rt))));

               assign(t4, binop(Iop_32HLto64, mkexpr(t1), mkexpr(t2)));
               assign(t5, binop(Iop_Add64, mkexpr(t3), mkexpr(t4)));

               putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t5)), True));
               putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t5)), True));
            } else {
               if ( (1 <= ac) && ( 3 >= ac) ) {
                  if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                     /* If DSP is present -> DSP ASE MADD */
                     UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                     if (0 != retVal ) {
                        goto decode_failure_dsp;
                     }
                     break;
                  } else {
                     goto decode_failure_dsp;
                  }
               } else {
                  DIP("madd r%d, r%d", rs, rt);
                  t1 = newTemp(Ity_I32);
                  t2 = newTemp(Ity_I32);
                  t3 = newTemp(Ity_I64);
                  t4 = newTemp(Ity_I32);
                  t5 = newTemp(Ity_I32);
                  t6 = newTemp(Ity_I32);

                  assign(t1, getHI());
                  assign(t2, getLO());

                  assign(t3, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));

                  assign(t4, binop(Iop_Add32, mkexpr(t2), unop(Iop_64to32,
                                                               mkexpr(t3))));

                  assign(t5, unop(Iop_1Uto32, binop(Iop_CmpLT32U, mkexpr(t4),
                                              unop(Iop_64to32, mkexpr(t3)))));
                  assign(t6, binop(Iop_Add32, mkexpr(t5), mkexpr(t1)));

                  putHI(binop(Iop_Add32, mkexpr(t6), unop(Iop_64HIto32,
                                                          mkexpr(t3))));
                  putLO(mkexpr(t4));
                  break;
               }
            }
            break;
         }

      case 0x01: {  /* MADDU */
         if (mode64) {
            DIP("maddu r%d, r%d", rs, rt);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I64);
            t5 = newTemp(Ity_I64);
            t6 = newTemp(Ity_I32);

            assign(t1, mkNarrowTo32(ty, getHI()));
            assign(t2, mkNarrowTo32(ty, getLO()));

            assign(t3, binop(Iop_MullU32, mkNarrowTo32(ty, getIReg(rs)),
                                          mkNarrowTo32(ty, getIReg(rt))));

            assign(t4, binop(Iop_32HLto64, mkexpr(t1), mkexpr(t2)));
            assign(t5, binop(Iop_Add64, mkexpr(t3), mkexpr(t4)));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t5)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t5)), True));
         } else {
            if ( (1 <= ac) && ( 3 >= ac) ) {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  /* If DSP is present -> DSP ASE MADDU */
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
            } else {
               DIP("maddu r%d, r%d", rs, rt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I32);
               t5 = newTemp(Ity_I32);
               t6 = newTemp(Ity_I32);

               assign(t1, getHI());
               assign(t2, getLO());

               assign(t3, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));

               assign(t4, binop(Iop_Add32, mkexpr(t2), unop(Iop_64to32,
                                                            mkexpr(t3))));
               assign(t5, unop(Iop_1Uto32, binop(Iop_CmpLT32U, mkexpr(t4),
                                           unop(Iop_64to32, mkexpr(t3)))));
               assign(t6, binop(Iop_Add32, mkexpr(t5), mkexpr(t1)));

               putHI(binop(Iop_Add32, mkexpr(t6), unop(Iop_64HIto32,
                                                      mkexpr(t3))));
               putLO(mkexpr(t4));
               break;
            }
         }
         break;
      }

      case 0x04: {  /* MSUB */
         if (mode64) {
            DIP("msub r%d, r%d", rs, rt);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I64);
            t5 = newTemp(Ity_I64);
            t6 = newTemp(Ity_I32);

            assign(t1, mkNarrowTo32(ty, getHI()));
            assign(t2, mkNarrowTo32(ty, getLO()));

            assign(t3, binop(Iop_MullS32, mkNarrowTo32(ty, getIReg(rs)),
                                          mkNarrowTo32(ty, getIReg(rt))));

            assign(t4, binop(Iop_32HLto64, mkexpr(t1), mkexpr(t2)));
            assign(t5, binop(Iop_Sub64, mkexpr(t4), mkexpr(t3)));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t5)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t5)), True));
         } else {
            if ( (1 <= ac) && ( 3 >= ac) ) {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  /* If DSP is present -> DSP ASE MSUB */
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
            } else {
               DIP("msub r%d, r%d", rs, rt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I32);
               t5 = newTemp(Ity_I1);
               t6 = newTemp(Ity_I32);

               assign(t1, getHI());
               assign(t2, getLO());

               assign(t3, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));
               assign(t4, unop(Iop_64to32, mkexpr(t3)));  /* new lo */

               /* if lo<lo(mul) hi = hi - 1 */
               assign(t5, binop(Iop_CmpLT32U,
                                 mkexpr(t2),
                                 mkexpr(t4)));

               assign(t6, IRExpr_ITE(mkexpr(t5),
                                       binop(Iop_Sub32, mkexpr(t1), mkU32(0x1)),
                                       mkexpr(t1)));

               putHI(binop(Iop_Sub32, mkexpr(t6), unop(Iop_64HIto32,
                                                      mkexpr(t3))));
               putLO(binop(Iop_Sub32, mkexpr(t2), mkexpr(t4)));
               break;
            }
         }
         break;
      }

      case 0x05: {  /* MSUBU */
         if (mode64) {
            DIP("msubu r%d, r%d", rs, rt);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I64);
            t5 = newTemp(Ity_I64);
            t6 = newTemp(Ity_I32);

            assign(t1, mkNarrowTo32(ty, getHI()));
            assign(t2, mkNarrowTo32(ty, getLO()));

            assign(t3, binop(Iop_MullU32, mkNarrowTo32(ty, getIReg(rs)),
                                          mkNarrowTo32(ty, getIReg(rt))));

            assign(t4, binop(Iop_32HLto64, mkexpr(t1), mkexpr(t2)));
            assign(t5, binop(Iop_Sub64, mkexpr(t4), mkexpr(t3)));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t5)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t5)), True));
         } else {
            if ( (1 <= ac) && ( 3 >= ac) ) {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  /* If DSP is present -> DSP ASE MSUBU */
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
            } else {
               DIP("msubu r%d, r%d", rs, rt);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I64);
               t4 = newTemp(Ity_I32);
               t5 = newTemp(Ity_I1);
               t6 = newTemp(Ity_I32);

               assign(t1, getHI());
               assign(t2, getLO());

               assign(t3, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));
               assign(t4, unop(Iop_64to32, mkexpr(t3)));  /* new lo */

               /* if lo<lo(mul) hi = hi - 1 */
               assign(t5, binop(Iop_CmpLT32U,
                                 mkexpr(t2),
                                 mkexpr(t4)));

               assign(t6, IRExpr_ITE(mkexpr(t5),
                                    binop(Iop_Sub32,
                                          mkexpr(t1),
                                          mkU32(0x1)),
                                    mkexpr(t1)));

               putHI(binop(Iop_Sub32, mkexpr(t6), unop(Iop_64HIto32,
                                                      mkexpr(t3))));
               putLO(binop(Iop_Sub32, mkexpr(t2), mkexpr(t4)));
               break;
            }
         }
         break;
      }

      case 0x6:  /* dmul MIPS64 - Netlogic */
         DIP("dmul r%u, r%u, r%u", rd, rs, rt);
         t0 = newTemp(Ity_I128);

         assign(t0, binop(Iop_MullU64, getIReg(rs), getIReg(rt)));

         putIReg(rd, unop(Iop_128to64, mkexpr(t0)));
         break;

      case 0x10:  /* LDADDW - Swap Word - Netlogic */
         DIP("ldaddw r%u, r%u", rt, rs);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);

         /* v = GPR[rt] */
         assign(t0, mkNarrowTo32(ty, getIReg(rt)));

         /* GPR[rt] = memory[base]; */
         assign(t1, load(Ity_I32, getIReg(rs)));
         putIReg(rt, mkWidenFrom32(ty, mkexpr(t1), True));

         /* memory[base] = memory[base] + v; */
         store(getIReg(rs), binop(Iop_Add32, mkexpr(t0), mkexpr(t1)));
         break;

      case 0x12:  /* LDADDD - Swap Word - Netlogic */
         DIP("ldaddw r%u, r%u", rt, rs);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);

         /*  v = GPR[rt] */
         assign(t0, getIReg(rt));

         /* GPR[rt] = memory[base]; */
         assign(t1, load(Ity_I64, getIReg(rs)));
         putIReg(rt, mkexpr(t1));

         /* memory[base] = memory[base] + v; */
         store(getIReg(rs), binop(Iop_Add64, mkexpr(t0), mkexpr(t1)));
         break;

      case 0x14:  /* SWAPW - Swap Word - Netlogic */
         DIP("swapw r%u, r%u", rt, rs);
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         assign(t0, mkNarrowTo32(ty, getIReg(rt)));
         assign(t1, load(Ity_I32, getIReg(rs)));
         putIReg(rt, mkWidenFrom32(ty, mkexpr(t1), True));
         store(getIReg(rs), mkexpr(t0));
         break;

      case 0x16:  /* SWAPD - Swap Double - Netlogic */
         DIP("swapw r%u, r%u", rt, rs);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         assign(t0, getIReg(rt));
         assign(t1, load(Ity_I64, getIReg(rs)));
         putIReg(rt, mkexpr(t1));
         store(getIReg(rs), mkexpr(t0));
         break;

      case 0x20: {  /* CLZ */
         DIP("clz r%d, r%d", rd, rs);
         if (mode64) {
            IRTemp tmpClz32 = newTemp(Ity_I32);
            IRTemp tmpRs32 = newTemp(Ity_I32);

            assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
            assign(tmpClz32, unop(Iop_Clz32, mkexpr(tmpRs32)));
            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpClz32), True));
         } else {
            t1 = newTemp(Ity_I1);
            assign(t1, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0)));
            putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                   mkU32(0x00000020),
                                   unop(Iop_Clz32, getIReg(rs))));
         }
         break;
      }

      case 0x21: {  /* CLO */
         DIP("clo r%d, r%d", rd, rs);
         if (mode64) {
            IRTemp tmpClo32 = newTemp(Ity_I32);
            IRTemp tmpRs32 = newTemp(Ity_I32);
            assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));

            t1 = newTemp(Ity_I1);
            assign(t1, binop(Iop_CmpEQ32, mkexpr(tmpRs32), mkU32(0xffffffff)));
            assign(tmpClo32, IRExpr_ITE(mkexpr(t1),
                      mkU32(0x00000020),
                      unop(Iop_Clz32, unop(Iop_Not32, mkexpr(tmpRs32)))));

            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpClo32), True));
            break;
         } else {
            t1 = newTemp(Ity_I1);
            assign(t1, binop(Iop_CmpEQ32, getIReg(rs), mkU32(0xffffffff)));
            putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                   mkU32(0x00000020),
                                   unop(Iop_Clz32,
                                        unop(Iop_Not32, getIReg(rs)))));
            break;
         }
      }

      case 0x24:  /* Count Leading Zeros in Doubleword - DCLZ; MIPS64 */
         DIP("dclz r%d, r%d", rd, rs);
         t1 = newTemp(Ity_I1);
         assign(t1, binop(Iop_CmpEQ64, getIReg(rs), mkU64(0)));
         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                     mkU64(0x00000040),
                     unop(Iop_Clz64, getIReg(rs))));
         break;

      case 0x25:  /* Count Leading Ones in Doubleword - DCLO; MIPS64 */
         DIP("dclo r%d, r%d", rd, rs);
         t1 = newTemp(Ity_I1);
         assign(t1, binop(Iop_CmpEQ64, getIReg(rs),
                                        mkU64(0xffffffffffffffffULL)));
         putIReg(rd, IRExpr_ITE(mkexpr(t1),
                                mkU64(0x40),
                                unop(Iop_Clz64, unop(Iop_Not64,
                                                     getIReg(rs)))));
         break;

      default:
         goto decode_failure;
      }
      break;

   case 0x1F:  /* Special3 */
      switch (function) {
         case 0x01: {
            /* Doubleword Extract Bit Field - DEXTM; MIPS64r2 */
            msb = get_msb(cins);
            lsb = get_lsb(cins);
            size = msb + 1;
            UInt srcPos = lsb;
            UInt dstSz = msb + 33;
            t1 = newTemp(Ity_I64);
            DIP("dextm r%u, r%u, %d, %d", rt, rs, lsb, msb + 1);

            UChar lsAmt = 64 - (srcPos + dstSz);  /* left shift amount; */
            UChar rsAmt = 64 - dstSz;  /* right shift amount; */

            assign(t1, binop(Iop_Shl64, getIReg(rs), mkU8(lsAmt)));
            putIReg(rt, binop(Iop_Shr64, mkexpr(t1), mkU8(rsAmt)));

            break;
         }
         case 0x02: {
            /* Doubleword Extract Bit Field Upper - DEXTU; MIPS64r2 */
            msb = get_msb(cins);
            lsb = get_lsb(cins);
            size = msb + 1;
            UInt srcPos = lsb + 32;
            UInt dstSz = msb + 1;
            DIP("dextu r%u, r%u, %d, %d", rt, rs, srcPos, dstSz);
            t1 = newTemp(Ity_I64);

            vassert(srcPos >= 32 && srcPos < 64);
            vassert(dstSz > 0 && dstSz <= 32);
            vassert((srcPos + dstSz) > 32 && (srcPos + dstSz) <= 64);

            UChar lsAmt = 64 - (srcPos + dstSz);  /* left shift amount; */
            UChar rsAmt = 64 - dstSz;  /* right shift amount; */

            assign(t1, binop(Iop_Shl64, getIReg(rs), mkU8(lsAmt)));
            putIReg(rt, binop(Iop_Shr64, mkexpr(t1), mkU8(rsAmt)));
            break;
         }
         case 0x05: {
            /* Doubleword Insert Bit Field Middle - DINSM; MIPS64r2 */
            msb = get_msb(cins);
            lsb = get_lsb(cins);
            size = msb + 1;
            UInt dstPos = lsb;
            UInt srcSz = msb - lsb + 33;
            t1 = newTemp(ty);
            t2 = newTemp(ty);
            t3 = newTemp(ty);
            t4 = newTemp(ty);
            IRTemp tmpT1 = newTemp(ty);
            IRTemp tmpT2 = newTemp(ty);
            IRTemp tmpT3 = newTemp(ty);
            IRTemp tmpT4 = newTemp(ty);
            IRTemp tmpT5 = newTemp(ty);
            IRTemp tmpT6 = newTemp(ty);
            IRTemp tmpT7 = newTemp(ty);
            IRTemp tmpRs = newTemp(ty);
            IRTemp tmpRt = newTemp(ty);
            IRTemp tmpRd = newTemp(ty);

            assign(tmpRs, getIReg(rs));
            assign(tmpRt, getIReg(rt));
            DIP("dinsm r%u, r%u, %d, %d", rt, rs, lsb, msb);

            UChar lsAmt = dstPos + srcSz - 1;   /* left shift amount; */
            UChar rsAmt = dstPos + srcSz - 1;   /* right shift amount; */

            assign(t1, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(rsAmt)));
            assign(tmpT1, binop(Iop_Shr64, mkexpr(t1), mkU8(1)));
            assign(t2, binop(Iop_Shl64, mkexpr(tmpT1), mkU8(lsAmt)));
            assign(tmpT2, binop(Iop_Shl64, mkexpr(t2), mkU8(1)));

            lsAmt = 63 - dstPos; /* left shift amount; */
            rsAmt = 63 - dstPos; /* right shift amount; */

            assign(t3, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(lsAmt)));
            assign(tmpT3, binop(Iop_Shl64, mkexpr(t3), mkU8(1)));
            assign(t4, binop(Iop_Shr64, mkexpr(tmpT3), mkU8(rsAmt)));
            assign(tmpT4, binop(Iop_Shr64, mkexpr(t4), mkU8(1)));

            /* extract size from src register */
            lsAmt = 64 - srcSz;  /* left shift amount; */
            rsAmt = 64 - (lsb + srcSz);   /* right shift amount; */

            assign(tmpT5, binop(Iop_Shl64, mkexpr(tmpRs), mkU8(lsAmt)));
            assign(tmpT6, binop(Iop_Shr64, mkexpr(tmpT5), mkU8(rsAmt)));

            assign(tmpT7, binop(Iop_Or64, mkexpr(tmpT2), mkexpr(tmpT4)));
            assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT6), mkexpr(tmpT7)));
            putIReg(rt, mkexpr(tmpRd));
            break;
         }
         case 0x06: {
            /* Doubleword Insert Bit Field Upper - DINSU; MIPS64r2 */
            msb = get_msb(cins);
            lsb = get_lsb(cins);
            size = msb + 1;
            UInt dstPos = lsb + 32;
            UInt srcSz = msb - lsb + 1;
            IRTemp tmpT1 = newTemp(ty);
            IRTemp tmpT2 = newTemp(ty);
            IRTemp tmpT3 = newTemp(ty);
            IRTemp tmpT4 = newTemp(ty);
            IRTemp tmpT5 = newTemp(ty);
            IRTemp tmpT6 = newTemp(ty);
            IRTemp tmpT7 = newTemp(ty);
            IRTemp tmpT8 = newTemp(ty);
            IRTemp tmpT9 = newTemp(ty);
            IRTemp tmpRs = newTemp(ty);
            IRTemp tmpRt = newTemp(ty);
            IRTemp tmpRd = newTemp(ty);

            assign(tmpRs, getIReg(rs));
            assign(tmpRt, getIReg(rt));
            DIP("dinsu r%u, r%u, %d, %d", rt, rs, lsb, msb);

            UChar lsAmt = 64 - srcSz;  /* left shift amount; */
            UChar rsAmt = 64 - (dstPos + srcSz);  /* right shift amount; */
            assign(tmpT1, binop(Iop_Shl64, mkexpr(tmpRs), mkU8(lsAmt)));
            assign(tmpT2, binop(Iop_Shr64, mkexpr(tmpT1), mkU8(rsAmt)));

            lsAmt = 64 - dstPos;  /* left shift amount; */
            rsAmt = 64 - dstPos;  /* right shift amount; */
            assign(tmpT3, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(lsAmt)));
            assign(tmpT4, binop(Iop_Shr64, mkexpr(tmpT3), mkU8(rsAmt)));

            lsAmt = dstPos;  /* left shift amount; */
            rsAmt = srcSz;  /* right shift amount; */
            assign(tmpT5, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(rsAmt)));
            assign(tmpT6, binop(Iop_Shr64, mkexpr(tmpT5), mkU8(lsAmt)));

            assign(tmpT7, binop(Iop_Shl64, mkexpr(tmpT6), mkU8(rsAmt)));
            assign(tmpT8, binop(Iop_Shl64, mkexpr(tmpT7), mkU8(lsAmt)));

            assign(tmpT9, binop(Iop_Or64, mkexpr(tmpT8), mkexpr(tmpT4)));
            assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT2), mkexpr(tmpT9)));
            putIReg(rt, mkexpr(tmpRd));
            break;
         }
         case 0x07: {
            /* Doubleword Insert Bit Field - DINS; MIPS64r2 */
            IRTemp tmp1 = newTemp(ty);
            IRTemp tmpT1 = newTemp(ty);
            IRTemp tmpT2 = newTemp(ty);
            IRTemp tmpT3 = newTemp(ty);
            IRTemp tmpT4 = newTemp(ty);
            IRTemp tmpT5 = newTemp(ty);
            IRTemp tmpT6 = newTemp(ty);
            IRTemp tmpT7 = newTemp(ty);
            IRTemp tmpT8 = newTemp(ty);
            IRTemp tmpT9 = newTemp(ty);
            IRTemp tmp = newTemp(ty);
            IRTemp tmpRs = newTemp(ty);
            IRTemp tmpRt = newTemp(ty);
            IRTemp tmpRd = newTemp(ty);

            assign(tmpRs, getIReg(rs));
            assign(tmpRt, getIReg(rt));

            msb = get_msb(cins);
            lsb = get_lsb(cins);
            size = msb + 1;
            DIP("dins r%u, r%u, %d, %d", rt, rs, lsb,
                msb - lsb + 1);
            UChar lsAmt = 63 - lsb;  /* left shift amount; */
            UChar rsAmt = 63 - lsb;  /* right shift amount; */
            assign(tmp, binop(Iop_Shl64, mkexpr(tmpRt), mkU8(lsAmt)));
            assign(tmpT1, binop(Iop_Shl64, mkexpr(tmp), mkU8(1)));
            assign(tmp1, binop(Iop_Shr64, mkexpr(tmpT1), mkU8(rsAmt)));
            assign(tmpT2, binop(Iop_Shr64, mkexpr(tmp1), mkU8(1)));

            lsAmt = msb;  /* left shift amount; */
            rsAmt = 1;  /*right shift amount; */
            assign(tmpT3, binop(Iop_Shr64, mkexpr(tmpRt), mkU8(rsAmt)));
            assign(tmpT4, binop(Iop_Shr64, mkexpr(tmpT3), mkU8(lsAmt)));
            assign(tmpT5, binop(Iop_Shl64, mkexpr(tmpT4), mkU8(rsAmt)));
            assign(tmpT6, binop(Iop_Shl64, mkexpr(tmpT5), mkU8(lsAmt)));

            lsAmt = 64 - (msb - lsb + 1);  /* left shift amount; */
            rsAmt = 64 - (msb + 1);  /* right shift amount; */
            assign(tmpT7, binop(Iop_Shl64, mkexpr(tmpRs), mkU8(lsAmt)));
            assign(tmpT8, binop(Iop_Shr64, mkexpr(tmpT7), mkU8(rsAmt)));

            assign(tmpT9, binop(Iop_Or64, mkexpr(tmpT2), mkexpr(tmpT8)));
            assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT6), mkexpr(tmpT9)));
            putIReg(rt, mkexpr(tmpRd));
            break;
         }
      case 0x24:  /* DBSHFL */
         lsb = get_lsb(cins);
         IRTemp tmpRs = newTemp(ty);
         IRTemp tmpRt = newTemp(ty);
         IRTemp tmpRd = newTemp(ty);
         assign(tmpRs, getIReg(rs));
         assign(tmpRt, getIReg(rt));
         switch (lsb) {
            case 0x02: {  /* DSBH */
               DIP("dsbh r%u, r%u", rd, rt);
               IRTemp tmpT1 = newTemp(ty);
               IRTemp tmpT2 = newTemp(ty);
               IRTemp tmpT3 = newTemp(ty);
               IRTemp tmpT4 = newTemp(ty);
               IRTemp tmpT5 = newTemp(Ity_I64);
               IRTemp tmpT6 = newTemp(ty);
               assign(tmpT5, mkU64(0xFF00FF00FF00FF00ULL));
               assign(tmpT6, mkU64(0x00FF00FF00FF00FFULL));
               assign(tmpT1, binop(Iop_And64, mkexpr(tmpRt), mkexpr(tmpT5)));
               assign(tmpT2, binop(Iop_Shr64, mkexpr(tmpT1), mkU8(8)));
               assign(tmpT3, binop(Iop_And64, mkexpr(tmpRt), mkexpr(tmpT6)));
               assign(tmpT4, binop(Iop_Shl64, mkexpr(tmpT3), mkU8(8)));
               assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT4), mkexpr(tmpT2)));
               putIReg(rd, mkexpr(tmpRd));
               break;
            }
            case 0x05: {  /* DSHD */
               DIP("dshd r%u, r%u\n", rd, rt);
               IRTemp tmpT1 = newTemp(ty);
               IRTemp tmpT2 = newTemp(ty);
               IRTemp tmpT3 = newTemp(ty);
               IRTemp tmpT4 = newTemp(ty);
               IRTemp tmpT5 = newTemp(Ity_I64);
               IRTemp tmpT6 = newTemp(ty);
               IRTemp tmpT7 = newTemp(ty);
               IRTemp tmpT8 = newTemp(ty);
               IRTemp tmpT9 = newTemp(ty);
               assign(tmpT5, mkU64(0xFFFF0000FFFF0000ULL));
               assign(tmpT6, mkU64(0x0000FFFF0000FFFFULL));
               assign(tmpT1, binop(Iop_And64, mkexpr(tmpRt), mkexpr(tmpT5)));
               assign(tmpT2, binop(Iop_Shr64, mkexpr(tmpT1), mkU8(16)));
               assign(tmpT3, binop(Iop_And64, mkexpr(tmpRt), mkexpr(tmpT6)));
               assign(tmpT4, binop(Iop_Shl64, mkexpr(tmpT3), mkU8(16)));
               assign(tmpT7, binop(Iop_Or64, mkexpr(tmpT4), mkexpr(tmpT2)));
               assign(tmpT8, binop(Iop_Shl64, mkexpr(tmpT7), mkU8(32)));
               assign(tmpT9, binop(Iop_Shr64, mkexpr(tmpT7), mkU8(32)));
               assign(tmpRd, binop(Iop_Or64, mkexpr(tmpT8), mkexpr(tmpT9)));
               putIReg(rd, mkexpr(tmpRd));
               break;
            }
         default:
            vex_printf("\nop6o10 = %d", lsb);
            goto decode_failure;;
         }
         break;
      case 0x3B: {  /* RDHWR */
         DIP("rdhwr r%d, r%d", rt, rd);
            if (rd == 29) {
               putIReg(rt, getULR());
#if defined(__mips__) && ((defined(__mips_isa_rev) && __mips_isa_rev >= 2))
            } else if (rd == 1) {
               if (mode64) {
                  IRTemp   val  = newTemp(Ity_I64);
                  IRExpr** args = mkIRExprVec_2 (mkU64(rt), mkU64(rd));
                  IRDirty *d = unsafeIRDirty_1_N(val,
                                                 0,
                                                 "mips64_dirtyhelper_rdhwr",
                                                 &mips64_dirtyhelper_rdhwr,
                                                 args);
                  stmt(IRStmt_Dirty(d));
                  putIReg(rt, mkexpr(val));
               } else {
                  IRTemp   val  = newTemp(Ity_I32);
                  IRExpr** args = mkIRExprVec_2 (mkU32(rt), mkU32(rd));
                  IRDirty *d = unsafeIRDirty_1_N(val,
                                                 0,
                                                 "mips32_dirtyhelper_rdhwr",
                                                 &mips32_dirtyhelper_rdhwr,
                                                 args);
                  stmt(IRStmt_Dirty(d));
                  putIReg(rt, mkexpr(val));
               }
#endif
            } else
               goto decode_failure;
            break;
         }
      case 0x04:  /* INS */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb - lsb + 1;
         DIP("ins size:%d msb:%d lsb:%d", size, msb, lsb);

         vassert(lsb + size <= 32);
         vassert(lsb + size > 0);

         /* put size bits from rs at the pos in temporary */
         t0 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         /* shift left for 32 - size to clear leading bits and get zeros
            at the end */
         assign(t0, binop(Iop_Shl32, mkNarrowTo32(ty, getIReg(rs)),
                          mkU8(32 - size)));
         /* now set it at pos */
         t1 = newTemp(Ity_I32);
         assign(t1, binop(Iop_Shr32, mkexpr(t0), mkU8(32 - size - lsb)));

         if (lsb > 0) {
            t2 = newTemp(Ity_I32);
            /* clear everything but lower pos bits from rt */
            assign(t2, binop(Iop_Shl32, mkNarrowTo32(ty, getIReg(rt)),
                             mkU8(32 - lsb)));
            assign(t3, binop(Iop_Shr32, mkexpr(t2), mkU8(32 - lsb)));
         } else
            assign(t3, mkU32(0));

         if (msb < 31) {
            t4 = newTemp(Ity_I32);
            /* clear everything but upper msb + 1 bits from rt */
            assign(t4, binop(Iop_Shr32, mkNarrowTo32(ty, getIReg(rt)),
                             mkU8(msb + 1)));
            t5 = newTemp(Ity_I32);
            assign(t5, binop(Iop_Shl32, mkexpr(t4), mkU8(msb + 1)));

            /* now combine these registers */
            if (lsb > 0) {
               t6 = newTemp(Ity_I32);
               assign(t6, binop(Iop_Or32, mkexpr(t5), mkexpr(t1)));
               putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t6),
                                                   mkexpr(t3)), True));
            } else {
               putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t1),
                                                   mkexpr(t5)), True));
            }
         } else {
            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Or32, mkexpr(t1),
                                                mkexpr(t3)), True));
         }
         break;

      case 0x00:  /* EXT */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         DIP("ext size:%d msb:%d lsb:%d", size, msb, lsb);
         vassert(lsb + size <= 32);
         vassert(lsb + size > 0);
         /* put size bits from rs at the top of in temporary */
         if (lsb + size < 32) {
            t0 = newTemp(Ity_I32);
            assign(t0, binop(Iop_Shl32, mkNarrowTo32(ty, getIReg(rs)),
                             mkU8(32 - lsb - size)));

            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Shr32, mkexpr(t0),
                                                mkU8(32 - size)), True));
         } else {
            putIReg(rt, mkWidenFrom32(ty, binop(Iop_Shr32,
                                                mkNarrowTo32(ty, getIReg(rs)),
                                                mkU8(32 - size)), True));
         }
         break;

      case 0x03:  /* Doubleword Extract Bit Field - DEXT; MIPS64r2 */
         msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         DIP("dext r%u, r%u, %d, %d", rt, rs, lsb, msb + 1);
         t1 = newTemp(Ity_I64);
         vassert(lsb >= 0 && lsb < 32);
         vassert(size > 0 && size <= 32);
         vassert((lsb + size) > 0 && (lsb + size) <= 63);

         UChar lsAmt = 63 - (lsb + msb);  /* left shift amount; */
         UChar rsAmt = 63 - msb;  /* right shift amount; */

         assign(t1, binop(Iop_Shl64, getIReg(rs), mkU8(lsAmt)));
         putIReg(rt, binop(Iop_Shr64, mkexpr(t1), mkU8(rsAmt)));

         break;

      case 0x20:  /* BSHFL */
         switch (sa) {
            case 0x02:  /* WSBH */
               DIP("wsbh r%d, r%d", rd, rt);
               t0 = newTemp(Ity_I32);
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               assign(t0, binop(Iop_Shl32, binop(Iop_And32, mkNarrowTo32(ty,
                                           getIReg(rt)), mkU32(0x00FF0000)),
                                           mkU8(0x8)));
               assign(t1, binop(Iop_Shr32, binop(Iop_And32, mkNarrowTo32(ty,
                                getIReg(rt)), mkU32(0xFF000000)), mkU8(0x8)));
               assign(t2, binop(Iop_Shl32, binop(Iop_And32, mkNarrowTo32(ty,
                                getIReg(rt)), mkU32(0x000000FF)), mkU8(0x8)));
               assign(t3, binop(Iop_Shr32, binop(Iop_And32, mkNarrowTo32(ty,
                                getIReg(rt)), mkU32(0x0000FF00)), mkU8(0x8)));
               putIReg(rd, mkWidenFrom32(ty, binop(Iop_Or32, binop(Iop_Or32,
                                         mkexpr(t0), mkexpr(t1)),
                                         binop(Iop_Or32, mkexpr(t2),
                                         mkexpr(t3))), True));
               break;

            case 0x10:  /* SEB */
               DIP("seb r%d, r%d", rd, rt);
               if (mode64)
                  putIReg(rd, unop(Iop_8Sto64, unop(Iop_64to8, getIReg(rt))));
               else
                  putIReg(rd, unop(Iop_8Sto32, unop(Iop_32to8, getIReg(rt))));
               break;

            case 0x18:  /* SEH */
               DIP("seh r%d, r%d", rd, rt);
               if (mode64)
                  putIReg(rd, unop(Iop_16Sto64, unop(Iop_64to16, getIReg(rt))));
               else
                  putIReg(rd, unop(Iop_16Sto32, unop(Iop_32to16, getIReg(rt))));
               break;

            default:
               goto decode_failure;

         }
         break;  /* BSHFL */

      /* --- MIPS32(r2) DSP ASE(r2) / Cavium Specfic (LX) instructions --- */
      case 0xA:  /* LX */
         if (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_CAVIUM) {
            if (dis_instr_CVM(cins))
               break;
            goto decode_failure;
         }
      case 0xC:  /* INSV */
      case 0x38: {  /* EXTR.W */
         if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );
            if (0 != retVal ) {
               goto decode_failure_dsp;
            }
            break;
         } else {
            goto decode_failure_dsp;
         }
         break;
      }
      case 0x10: {  /* ADDU.QB */
         switch(sa) {
            case  0xC:  /* SUBU_S.PH */
            case  0xD:  /* ADDU_S.PH */
            case 0x1E: {  /* MULQ_S.PH */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
         }
         break;
      }
      case 0x11: {  /* CMPU.EQ.QB */
         switch(sa) {
            case 0x18:  /* CMPGDU.EQ.QB */
            case 0x19:  /* CMPGDU.LT.QB */
            case 0x1A:  /* CMPGDU.LE.QB */
            case 0x0D:  /* PRECR.QB.PH */
            case 0x1E:  /* PRECR_SRA.PH.W */
            case 0x1F: {  /* PRECR_SRA_R.PH.W */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
         }
         break;
      }
      case 0x12: {  /* ABSQ_S.PH */
         switch(sa){
            case 0x1: {  /* ABSQ_S.QB */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
         }
         break;
      }
      case 0x13: {  /* SHLL.QB */
         switch(sa) {
            case 0x04:  /* SHRA.QB */
            case 0x05:  /* SHRA_R.QB */
            case 0x06:  /* SHRAV.QB */
            case 0x07:  /* SHRAV_R.QB */
            case 0x19:  /* SHLR.PH */
            case 0x1B: {  /* SHLRV.PH */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
         }
         break;
      }
      case 0x30: {  /* DPAQ.W.PH */
         switch(sa) {
            case  0x0:  /* DPA.W.PH */
            case 0x18:  /* DPAQX_S.W.PH */
            case 0x1A:  /* DPAQX_SA.W.PH */
            case  0x8:  /* DPAX.W.PH */
            case  0x1:  /* DPS.W.PH */
            case 0x19:  /* DPSQX_S.W.PH */
            case 0x1B:  /* DPSQX_SA.W.PH */
            case  0x9:  /* DPSX.W.PH */
            case  0x2: {  /* MULSA.W.PH */
               if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
            default: {
               if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
                  UInt retVal = disDSPInstr_MIPS_WRK ( cins );
                  if (0 != retVal ) {
                     goto decode_failure_dsp;
                  }
                  break;
               } else {
                  goto decode_failure_dsp;
               }
               break;
            }
         }
         break;
      }
      case 0x18:  /* ADDUH.QB/MUL.PH */
      case 0x31: {  /* APPEND */
         if (VEX_MIPS_PROC_DSP2(archinfo->hwcaps)) {
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );
            if (0 != retVal ) {
               goto decode_failure_dsp;
            }
            break;
         } else {
            goto decode_failure_dsp;
         }
      }
      default:
         goto decode_failure;

   }
      break;  /* Special3 */

   case 0x3B:
      if (0x3B == function &&
          (VEX_MIPS_COMP_ID(archinfo->hwcaps) == VEX_PRID_COMP_BROADCOM)) {
         /*RDHWR*/
         DIP("rdhwr r%d, r%d", rt, rd);
         if (rd == 29) {
            putIReg(rt, getULR());
         } else
            goto decode_failure;
         break;
      } else {
         goto decode_failure;
      }

   case 0x00:  /* Special */

      switch (function) {
      case 0x1: {
         UInt mov_cc = get_mov_cc(cins);
         if (tf == 0) {  /* MOVF */
            DIP("movf r%d, r%d, %d", rd, rs, mov_cc);
            t1 = newTemp(Ity_I1);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I1);

            assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
            assign(t2, IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(23)),
                                        mkU32(0x1)),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(24 + mov_cc)),
                                        mkU32(0x1))
                                  ));
            assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
            putIReg(rd, IRExpr_ITE(mkexpr(t3), getIReg(rs), getIReg(rd)));
         } else if (tf == 1) {  /* MOVT */
            DIP("movt r%d, r%d, %d", rd, rs, mov_cc);
            t1 = newTemp(Ity_I1);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I1);

            assign(t1, binop(Iop_CmpEQ32, mkU32(0), mkU32(mov_cc)));
            assign(t2, IRExpr_ITE(mkexpr(t1),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(23)),
                                        mkU32(0x1)),
                                  binop(Iop_And32,
                                        binop(Iop_Shr32, getFCSR(),
                                              mkU8(24 + mov_cc)),
                                        mkU32(0x1))
                                  ));
            assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
            putIReg(rd, IRExpr_ITE(mkexpr(t3), getIReg(rs), getIReg(rd)));
         }
         break;
      }
      case 0x0A: {  /* MOVZ */
         DIP("movz r%d, r%d, r%d", rd, rs, rt);
         t1 = newTemp(ty);
         t2 = newTemp(ty);
         if (mode64) {
            assign(t1, unop(Iop_32Sto64, unop(Iop_1Sto32, binop(Iop_CmpEQ64,
                            getIReg(rt), mkU64(0x0)))));
            assign(t2, unop(Iop_32Sto64, unop(Iop_1Sto32, binop(Iop_CmpNE64,
                            getIReg(rt), mkU64(0x0)))));
            putIReg(rd, binop(Iop_Add64, binop(Iop_And64, getIReg(rs),
                        mkexpr(t1)), binop(Iop_And64, getIReg(rd),mkexpr(t2))));
         } else {
            assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, getIReg(rt),
                                              mkU32(0x0))));
            assign(t2, unop(Iop_1Sto32, binop(Iop_CmpNE32, getIReg(rt),
                                              mkU32(0x0))));
            putIReg(rd, binop(Iop_Add32, binop(Iop_And32, getIReg(rs),
                        mkexpr(t1)), binop(Iop_And32, getIReg(rd),
                        mkexpr(t2))));
         }
         break;
      }

      case 0x0B: {  /* MOVN */
         DIP("movn r%d, r%d, r%d", rd, rs, rt);
         t1 = newTemp(ty);
         t2 = newTemp(ty);
         if (mode64) {
            assign(t1, unop(Iop_32Sto64, unop(Iop_1Sto32, binop(Iop_CmpEQ64,
                            getIReg(rt), mkU64(0x0)))));
            assign(t2, unop(Iop_32Sto64, unop(Iop_1Sto32, binop(Iop_CmpNE64,
                            getIReg(rt), mkU64(0x0)))));
            putIReg(rd, binop(Iop_Add64, binop(Iop_And64, getIReg(rs),
                        mkexpr(t2)), binop(Iop_And64, getIReg(rd),
                                           mkexpr(t1))));
         } else {
            assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, getIReg(rt),
                                              mkU32(0x0))));
            assign(t2, unop(Iop_1Sto32, binop(Iop_CmpNE32, getIReg(rt),
                                              mkU32(0x0))));
            putIReg(rd, binop(Iop_Add32, binop(Iop_And32, getIReg(rs),
                        mkexpr(t2)), binop(Iop_And32, getIReg(rd),
                        mkexpr(t1))));
         }
         break;
      }

      case 0x18:  {  /* MULT */
         if ( (1 <= ac) && ( 3 >= ac) ) {
            if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
               /* If DSP is present -> DSP ASE MULT */
               UInt retVal = disDSPInstr_MIPS_WRK ( cins );
               if (0 != retVal ) {
                  goto decode_failure_dsp;
               }
               break;
            } else {
               goto decode_failure_dsp;
            }
         } else {
            DIP("mult r%d, r%d", rs, rt);
            t2 = newTemp(Ity_I64);

            assign(t2, binop(Iop_MullS32, mkNarrowTo32(ty, getIReg(rs)),
                                          mkNarrowTo32(ty, getIReg(rt))));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
            break;
         }
      }
      case 0x19:  {  /* MULTU */
         if ( (1 <= ac) && ( 3 >= ac) ) {
            if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
               /* If DSP is present -> DSP ASE MULTU */
               UInt retVal = disDSPInstr_MIPS_WRK ( cins );
               if (0 != retVal ) {
                  goto decode_failure_dsp;
               }
               break;
            } else {
               goto decode_failure_dsp;
            }
         } else {
            DIP("multu r%d, r%d", rs, rt);
            t2 = newTemp(Ity_I64);

            assign(t2, binop(Iop_MullU32, mkNarrowTo32(ty, getIReg(rs)),
                                          mkNarrowTo32(ty, getIReg(rt))));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
            break;
         }
      }
      case 0x20: {  /* ADD */
         DIP("add r%d, r%d, r%d", rd, rs, rt);
         IRTemp tmpRs32 = newTemp(Ity_I32);
         IRTemp tmpRt32 = newTemp(Ity_I32);

         assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
         assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));

         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         /* dst = src0 + src1
            if (sign(src0 ) != sign(src1 ))
            goto no overflow;
            if (sign(dst) == sign(src0 ))
            goto no overflow;
            we have overflow! */

         assign(t0, binop(Iop_Add32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
         assign(t1, binop(Iop_Xor32, mkexpr(tmpRs32), mkexpr(tmpRt32)));
         assign(t2, unop(Iop_1Uto32,
                         binop(Iop_CmpEQ32,
                               binop(Iop_And32, mkexpr(t1), mkU32(0x80000000)),
                               mkU32(0x80000000))));

         assign(t3, binop(Iop_Xor32, mkexpr(t0), mkexpr(tmpRs32)));
         assign(t4, unop(Iop_1Uto32,
                         binop(Iop_CmpNE32,
                               binop(Iop_And32, mkexpr(t3), mkU32(0x80000000)),
                               mkU32(0x80000000))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ32,
                                binop(Iop_Or32, mkexpr(t2), mkexpr(t4)),
                                mkU32(0)),
                          Ijk_SigFPE_IntOvf,
                          mode64 ? IRConst_U64(guest_PC_curr_instr + 4) :
                                   IRConst_U32(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rd,  mkWidenFrom32(ty, mkexpr(t0), True));
         break;
      }
      case 0x1A:  /* DIV */
         DIP("div r%d, r%d", rs, rt);
         if (mode64) {
            t2 = newTemp(Ity_I64);

            assign(t2, binop(Iop_DivModS64to32,
                             getIReg(rs), mkNarrowTo32(ty, getIReg(rt))));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
         } else {
            t1 = newTemp(Ity_I64);
            t2 = newTemp(Ity_I64);

            assign(t1, unop(Iop_32Sto64, getIReg(rs)));
            assign(t2, binop(Iop_DivModS64to32, mkexpr(t1), getIReg(rt)));

            putHI(unop(Iop_64HIto32, mkexpr(t2)));
            putLO(unop(Iop_64to32, mkexpr(t2)));
         }
         break;

      case 0x1B:  /* DIVU */
         DIP("divu r%d, r%d", rs, rt);
         if (mode64) {
            t2 = newTemp(Ity_I64);

            assign(t2, binop(Iop_DivModU64to32,
                             getIReg(rs), mkNarrowTo32(ty, getIReg(rt))));

            putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
            putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
         } else {
            t1 = newTemp(Ity_I64);
            t2 = newTemp(Ity_I64);
            assign(t1, unop(Iop_32Uto64, getIReg(rs)));
            assign(t2, binop(Iop_DivModU64to32, mkexpr(t1), getIReg(rt)));
            putHI(unop(Iop_64HIto32, mkexpr(t2)));
            putLO(unop(Iop_64to32, mkexpr(t2)));
         }
         break;

      case 0x1C:  /* Doubleword Multiply - DMULT; MIPS64 */
         DIP("dmult r%u, r%u", rs, rt);
         t0 = newTemp(Ity_I128);

         assign(t0, binop(Iop_MullS64, getIReg(rs), getIReg(rt)));

         putHI(unop(Iop_128HIto64, mkexpr(t0)));
         putLO(unop(Iop_128to64, mkexpr(t0)));
         break;

      case 0x1D:  /* Doubleword Multiply Unsigned - DMULTU; MIPS64 */
         DIP("dmultu r%u, r%u", rs, rt);
         t0 = newTemp(Ity_I128);

         assign(t0, binop(Iop_MullU64, getIReg(rs), getIReg(rt)));

         putHI(unop(Iop_128HIto64, mkexpr(t0)));
         putLO(unop(Iop_128to64, mkexpr(t0)));
         break;

      case 0x1E:  /* Doubleword Divide DDIV; MIPS64 */
         DIP("ddiv r%u, r%u", rs, rt);
         t1 = newTemp(Ity_I128);

         assign(t1, binop(Iop_DivModS64to64, getIReg(rs), getIReg(rt)));

         putHI(unop(Iop_128HIto64, mkexpr(t1)));
         putLO(unop(Iop_128to64, mkexpr(t1)));
         break;

      case 0x1F:  /* Doubleword Divide Unsigned DDIVU; MIPS64 check this */
         DIP("ddivu r%u, r%u", rs, rt);
         t1 = newTemp(Ity_I128);
         t2 = newTemp(Ity_I128);

         assign(t1, binop(Iop_64HLto128, mkU64(0), getIReg(rs)));

         assign(t2, binop(Iop_DivModU128to64, mkexpr(t1), getIReg(rt)));

         putHI(unop(Iop_128HIto64, mkexpr(t2)));
         putLO(unop(Iop_128to64, mkexpr(t2)));
         break;

      case 0x10: {  /* MFHI */
         if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            /* If DSP is present -> DSP ASE MFHI */
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );
            if (0 != retVal ) {
               goto decode_failure;
            }
            break;
         } else {
            DIP("mfhi r%d", rd);
            putIReg(rd, getHI());
            break;
         }
      }

      case 0x11:  {  /* MTHI */
         if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            /* If DSP is present -> DSP ASE MTHI */
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );
            if (0 != retVal ) {
               goto decode_failure;
            }
            break;
         } else {
            DIP("mthi r%d", rs);
            putHI(getIReg(rs));
            break;
         }
      }

      case 0x12:  {  /* MFLO */
         if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            /* If DSP is present -> DSP ASE MFLO */
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );
            if (0 != retVal ) {
               goto decode_failure;
            }
            break;
         } else {
            DIP("mflo r%d", rd);
            putIReg(rd, getLO());
            break;
         }
      }

      case 0x13:  {  /* MTLO */
         if (VEX_MIPS_PROC_DSP(archinfo->hwcaps)) {
            /* If DSP is present -> DSP ASE MTLO */
            UInt retVal = disDSPInstr_MIPS_WRK ( cins );
            if (0 != retVal ) {
               goto decode_failure;
            }
            break;
         } else {
            DIP("mtlo r%d", rs);
            putLO(getIReg(rs));
            break;
         }
      }

      case 0x21:  /* ADDU */
         DIP("addu r%d, r%d, r%d", rd, rs, rt);
         if (mode64) {
            ALU_PATTERN64(Iop_Add32);
         } else {
            ALU_PATTERN(Iop_Add32);
         }
         break;

      case 0x22: {  /* SUB */
         DIP("sub r%d, r%d, r%d", rd, rs, rt);
         IRTemp tmpRs32 = newTemp(Ity_I32);
         IRTemp tmpRt32 = newTemp(Ity_I32);

         assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));
         assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
         t0 = newTemp(Ity_I32);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         /* dst = src0 + (-1 * src1)
            if(sign(src0 ) != sign((-1 * src1) ))
            goto no overflow;
            if(sign(dst) == sign(src0 ))
            goto no overflow;
            we have overflow! */

         assign(t5, binop(Iop_Mul32, mkexpr(tmpRt32), mkU32(-1)));
         assign(t0, binop(Iop_Add32, mkexpr(tmpRs32), mkexpr(t5)));
         assign(t1, binop(Iop_Xor32, mkexpr(tmpRs32), mkexpr(t5)));
         assign(t2, unop(Iop_1Sto32, binop(Iop_CmpEQ32, binop(Iop_And32,
                         mkexpr(t1), mkU32(0x80000000)), mkU32(0x80000000))));

         assign(t3, binop(Iop_Xor32, mkexpr(t0), mkexpr(tmpRs32)));
         assign(t4, unop(Iop_1Sto32, binop(Iop_CmpNE32, binop(Iop_And32,
                         mkexpr(t3), mkU32(0x80000000)), mkU32(0x80000000))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ32, binop(Iop_Or32, mkexpr(t2),
                                mkexpr(t4)), mkU32(0)), Ijk_SigFPE_IntOvf,
                          mode64 ? IRConst_U64(guest_PC_curr_instr + 4) :
                                   IRConst_U32(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rd, mkWidenFrom32(ty, mkexpr(t0), True));
         break;
      }
      case 0x23:  /* SUBU */
         DIP("subu r%d, r%d, r%d", rd, rs, rt);
         if (mode64) {
            ALU_PATTERN64(Iop_Sub32);
         } else {
            ALU_PATTERN(Iop_Sub32);
         }
         break;

      case 0x24:  /* AND */
         DIP("and r%d, r%d, r%d", rd, rs, rt);
         if (mode64) {
            ALU_PATTERN(Iop_And64);
         } else {
            ALU_PATTERN(Iop_And32);
         }
         break;

      case 0x25:  /* OR */
         DIP("or r%d, r%d, r%d", rd, rs, rt);
         if (mode64) {
            ALU_PATTERN(Iop_Or64);
         } else {
            ALU_PATTERN(Iop_Or32);
         }
         break;

      case 0x26:  /* XOR */
         DIP("xor r%d, r%d, r%d", rd, rs, rt);
         if (mode64) {
            ALU_PATTERN(Iop_Xor64);
         } else {
            ALU_PATTERN(Iop_Xor32);
         }
         break;

      case 0x27:  /* NOR */
         DIP("nor r%d, r%d, r%d", rd, rs, rt);
         if (mode64)
            putIReg(rd, unop(Iop_Not64, binop(Iop_Or64, getIReg(rs),
                                              getIReg(rt))));
         else
            putIReg(rd, unop(Iop_Not32, binop(Iop_Or32, getIReg(rs),
                                              getIReg(rt))));
         break;

      case 0x08:  /* JR */
         DIP("jr r%d", rs);
         t0 = newTemp(ty);
         assign(t0, getIReg(rs));
         lastn = mkexpr(t0);
         break;

      case 0x09:  /* JALR */
         DIP("jalr r%d r%d", rd, rs);
         if (mode64) {
            putIReg(rd, mkU64(guest_PC_curr_instr + 8));
            t0 = newTemp(Ity_I64);
            assign(t0, getIReg(rs));
            lastn = mkexpr(t0);
         } else {
            putIReg(rd, mkU32(guest_PC_curr_instr + 8));
            t0 = newTemp(Ity_I32);
            assign(t0, getIReg(rs));
            lastn = mkexpr(t0);
         }
         break;

      case 0x0C:  /* SYSCALL */
         DIP("syscall");
         if (mode64)
            putPC(mkU64(guest_PC_curr_instr + 4));
         else
            putPC(mkU32(guest_PC_curr_instr + 4));
         dres.jk_StopHere = Ijk_Sys_syscall;
         dres.whatNext    = Dis_StopHere;
         break;

      case 0x2A:  /* SLT */
         DIP("slt r%d, r%d, r%d", rd, rs, rt);
         if (mode64)
            putIReg(rd, unop(Iop_1Uto64, binop(Iop_CmpLT64S, getIReg(rs),
                                               getIReg(rt))));
         else
            putIReg(rd, unop(Iop_1Uto32, binop(Iop_CmpLT32S, getIReg(rs),
                                               getIReg(rt))));
         break;

      case 0x2B:  /* SLTU */
         DIP("sltu r%d, r%d, r%d", rd, rs, rt);
         if (mode64)
            putIReg(rd, unop(Iop_1Uto64, binop(Iop_CmpLT64U, getIReg(rs),
                                         getIReg(rt))));
         else
            putIReg(rd, unop(Iop_1Uto32, binop(Iop_CmpLT32U, getIReg(rs),
                                         getIReg(rt))));
         break;

      case 0x00: {  /* SLL */
         DIP("sll r%d, r%d, %d", rd, rt, sa);
         IRTemp tmpRt32 = newTemp(Ity_I32);
         IRTemp tmpSh32 = newTemp(Ity_I32);
         IRTemp tmpRd = newTemp(Ity_I64);
         if (mode64) {
            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpSh32, binop(Iop_Shl32, mkexpr(tmpRt32), mkU8(sa)));
            assign(tmpRd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
            putIReg(rd, mkexpr(tmpRd));
         } else
            SXX_PATTERN(Iop_Shl32);
         break;
      }

      case 0x04: {  /* SLLV */
         DIP("sllv r%d, r%d, r%d", rd, rt, rs);
         if (mode64) {
            IRTemp tmpRs8 = newTemp(Ity_I8);
            IRTemp tmpRt32 = newTemp(Ity_I32);
            IRTemp tmpSh32 = newTemp(Ity_I32);
            IRTemp tmp = newTemp(ty);
            assign(tmp, binop(mkSzOp(ty, Iop_And8), getIReg(rs),
                              mkSzImm(ty, 31)));
            assign(tmpRs8, mkNarrowTo8(ty, mkexpr(tmp)));
            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpSh32, binop(Iop_Shl32, mkexpr(tmpRt32), mkexpr(tmpRs8)));
            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
         } else {
            SXXV_PATTERN(Iop_Shl32);
         }
         break;
      }

      case 0x03:  /* SRA */
         DIP("sra r%d, r%d, %d", rd, rt, sa);
         if (mode64) {
            IRTemp tmpRt32 = newTemp(Ity_I32);
            IRTemp tmpSh32 = newTemp(Ity_I32);

            t1 = newTemp(Ity_I64);
            t2 = newTemp(Ity_I64);
            t3 = newTemp(Ity_I64);

            assign(t1, binop(Iop_And64, getIReg(rt),  /* hi */
                             mkU64(0xFFFFFFFF00000000ULL)));

            assign(t2, binop(Iop_Sar64, mkexpr(t1), mkU8(sa)));

            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpSh32, binop(Iop_Sar32, mkexpr(tmpRt32), mkU8(sa)));

            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
         } else {
            SXX_PATTERN(Iop_Sar32);
         }
         break;

      case 0x07:  /* SRAV */
         DIP("srav r%d, r%d, r%d", rd, rt, rs);
         if (mode64) {
            IRTemp tmpRt32 = newTemp(Ity_I32);
            IRTemp tmpSh32 = newTemp(Ity_I32);

            t1 = newTemp(Ity_I64);
            t2 = newTemp(Ity_I64);
            t3 = newTemp(Ity_I64);
            t4 = newTemp(Ity_I8);

            assign(t4, unop(Iop_32to8, binop(Iop_And32,
                       mkNarrowTo32(ty, getIReg(rs)), mkU32(0x0000001F))));

            assign(t1, binop(Iop_And64, getIReg(rt),  /* hi */
                   mkU64(0xFFFFFFFF00000000ULL)));

            assign(t2, binop(Iop_Sar64, mkexpr(t1), mkexpr(t4)));

            assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
            assign(tmpSh32, binop(Iop_Sar32, mkexpr(tmpRt32), mkexpr(t4)));

            putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
         } else {
            SXXV_PATTERN(Iop_Sar32);
         }
         break;

      case 0x02: {  /* SRL */
         rot = get_rot(cins);
         if (rot) {
            DIP("rotr r%d, r%d, %d", rd, rt, sa);
            putIReg(rd, mkWidenFrom32(ty, genROR32(mkNarrowTo32(ty,
                        getIReg(rt)), sa), True));
         } else {
            DIP("srl r%d, r%d, %d", rd, rt, sa);
            if (mode64) {
               IRTemp tmpSh32 = newTemp(Ity_I32);
               IRTemp tmpRt32 = newTemp(Ity_I32);

               assign(tmpRt32, mkNarrowTo32(ty, getIReg(rt)));
               assign(tmpSh32, binop(Iop_Shr32, mkexpr(tmpRt32), mkU8(sa)));
               putIReg(rd, mkWidenFrom32(ty, mkexpr(tmpSh32), True));
            } else {
               SXX_PATTERN(Iop_Shr32);
            }
         }
      break;
      }

      case 0x06: {
         rot = get_rotv(cins);
         if (rot) {
            DIP("rotrv r%d, r%d, r%d", rd, rt, rs);
            putIReg(rd, mkWidenFrom32(ty, genRORV32(mkNarrowTo32(ty,
                        getIReg(rt)), mkNarrowTo32(ty, getIReg(rs))), True));
            break;
         } else {  /* SRLV */
            DIP("srlv r%d, r%d, r%d", rd, rt, rs);
            if (mode64) {
               SXXV_PATTERN64(Iop_Shr32);
            } else {
               SXXV_PATTERN(Iop_Shr32);
            }
            break;
         }
      }

      case 0x0D:  /* BREAK */
         DIP("break 0x%x", trap_code);
         if (mode64)
            jmp_lit64(&dres, Ijk_SigTRAP, (guest_PC_curr_instr + 4));
         else
            jmp_lit32(&dres, Ijk_SigTRAP, (guest_PC_curr_instr + 4));
         vassert(dres.whatNext == Dis_StopHere);
         break;

      case 0x30: {  /* TGE */
         DIP("tge r%d, r%d %d", rs, rt, trap_code);
         if (mode64) {
            if (trap_code == 7)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntDiv,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else if (trap_code == 6)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntOvf,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32S,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigTRAP,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
         }
         break;
      }
      case 0x31: {  /* TGEU */
         DIP("tgeu r%d, r%d %d", rs, rt, trap_code);
         if (mode64) {
            if (trap_code == 7)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntDiv,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else if (trap_code == 6)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntOvf,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT64U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigTRAP,
                                  IRConst_U64(guest_PC_curr_instr + 4),
                                  OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntDiv,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else if (trap_code == 6)
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigFPE_IntOvf,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
            else
               stmt (IRStmt_Exit (unop (Iop_Not1,
                                        binop (Iop_CmpLT32U,
                                               getIReg (rs),
                                               getIReg (rt))),
                                  Ijk_SigTRAP,
                                  IRConst_U32(guest_PC_curr_instr + 4),
                                  OFFB_PC));
         }
         break;
      }
      case 0x32: {  /* TLT */
         DIP("tlt r%d, r%d %d", rs, rt, trap_code);
         if (mode64) {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpLT64S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpLT64S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpLT64S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpLT32S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpLT32S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpLT32S, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
         }
         break;
      }
      case 0x33: {  /* TLTU */
         DIP("tltu r%d, r%d %d", rs, rt, trap_code);
         if (mode64) {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpLT64U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpLT64U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpLT64U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpLT32U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpLT32U, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpLT32U, getIReg(rs),
                                      getIReg (rt)), Ijk_SigTRAP,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
         }
         break;
      }
      case 0x34: {  /* TEQ */
         DIP("teq r%d, r%d, %d", rs, rt, trap_code);
         if (mode64) {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpEQ64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpEQ64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpEQ64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpEQ32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpEQ32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpEQ32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
         }
         break;
      }
      case 0x36: {  /* TNE */
         DIP("tne r%d, r%d %d", rs, rt, trap_code);
         if (mode64) {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpNE64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpNE64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpNE64, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U64(guest_PC_curr_instr + 4),
                                OFFB_PC));
         } else {
            if (trap_code == 7)
               stmt(IRStmt_Exit(binop(Iop_CmpNE32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntDiv,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else if (trap_code == 6)
               stmt(IRStmt_Exit(binop(Iop_CmpNE32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigFPE_IntOvf,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
            else
               stmt(IRStmt_Exit(binop(Iop_CmpNE32, getIReg(rs),
                                      getIReg(rt)), Ijk_SigTRAP,
                                IRConst_U32(guest_PC_curr_instr + 4),
                                OFFB_PC));
         }
         break;
      }
      case 0x14:
      case 0x16:
      case 0x17:  /* DSLLV, DROTRV:DSRLV, DSRAV */
      case 0x38:
      case 0x3A:
      case 0x3B:  /* DSLL, DROTL:DSRL, DSRA  */
      case 0x3C:
      case 0x3E:
      case 0x3F:  /* DSLL32, DROTR32:DSRL32, DSRA32 */
         if (dis_instr_shrt(cins))
            break;
         goto decode_failure;

      case 0x0F:  /* SYNC */
         DIP("sync 0x%x", sel);
         /* Just ignore it. */
         break;

      case 0x2C: {  /* Doubleword Add - DADD; MIPS64 */
         DIP("dadd r%d, r%d, r%d", rd, rs, rt);
         IRTemp tmpRs64 = newTemp(Ity_I64);
         IRTemp tmpRt64 = newTemp(Ity_I64);

         assign(tmpRs64, getIReg(rs));
         assign(tmpRt64, getIReg(rt));

         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I64);
         /* dst = src0 + src1
            if(sign(src0 ) != sign(src1 ))
            goto no overflow;
            if(sign(dst) == sign(src0 ))
            goto no overflow;
            we have overflow! */

         assign(t0, binop(Iop_Add64, mkexpr(tmpRs64), mkexpr(tmpRt64)));
         assign(t1, binop(Iop_Xor64, mkexpr(tmpRs64), mkexpr(tmpRt64)));
         assign(t2, unop(Iop_1Uto64,
                         binop(Iop_CmpEQ64,
                               binop(Iop_And64, mkexpr(t1),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x8000000000000000ULL))));

         assign(t3, binop(Iop_Xor64, mkexpr(t0), mkexpr(tmpRs64)));
         assign(t4, unop(Iop_1Uto64,
                         binop(Iop_CmpNE64,
                               binop(Iop_And64, mkexpr(t3),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x8000000000000000ULL))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ64,
                                binop(Iop_Or64, mkexpr(t2), mkexpr(t4)),
                                mkU64(0)),
                          Ijk_SigFPE_IntOvf,
                          IRConst_U64(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rd,  mkexpr(t0));
         break;
      }

      case 0x2D:  /* Doubleword Add Unsigned - DADDU; MIPS64 */
         DIP("daddu r%d, r%d, r%d", rd, rs, rt);
         ALU_PATTERN(Iop_Add64);
         break;

      case 0x2E: {  /* Doubleword Subtract - DSUB; MIPS64 */
         DIP("dsub r%u, r%u, r%u", rd, rs, rt);
         IRTemp tmpRs64 = newTemp(Ity_I64);
         IRTemp tmpRt64 = newTemp(Ity_I64);

         assign(tmpRs64, getIReg(rs));
         assign(tmpRt64, getIReg(rt));
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         t2 = newTemp(Ity_I64);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I64);
         t5 = newTemp(Ity_I64);
         /* dst = src0 + (-1 * src1)
            if(sign(src0 ) != sign((-1 * src1) ))
            goto no overflow;
            if(sign(dst) == sign(src0 ))
            goto no overflow;
            we have overflow! */

         assign(t5, binop(Iop_Mul64,
                          mkexpr(tmpRt64),
                          mkU64(0xffffffffffffffffULL)));
         assign(t0, binop(Iop_Add64, mkexpr(tmpRs64), mkexpr(t5)));
         assign(t1, binop(Iop_Xor64, mkexpr(tmpRs64), mkexpr(t5)));
         assign(t2, unop(Iop_1Sto64,
                         binop(Iop_CmpEQ64,
                               binop(Iop_And64,
                                     mkexpr(t1),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x8000000000000000ULL))));

         assign(t3, binop(Iop_Xor64, mkexpr(t0), mkexpr(tmpRs64)));
         assign(t4, unop(Iop_1Sto64,
                         binop(Iop_CmpNE64,
                               binop(Iop_And64,
                                     mkexpr(t3),
                                     mkU64(0x8000000000000000ULL)),
                               mkU64(0x8000000000000000ULL))));

         stmt(IRStmt_Exit(binop(Iop_CmpEQ64, binop(Iop_Or64, mkexpr(t2),
                                mkexpr(t4)), mkU64(0)), Ijk_SigFPE_IntOvf,
                          IRConst_U64(guest_PC_curr_instr + 4),
                          OFFB_PC));

         putIReg(rd, binop(Iop_Sub64, getIReg(rs), getIReg(rt)));
         break;
      }

      case 0x2F:  /* Doubleword Subtract Unsigned - DSUBU; MIPS64 */
         DIP("dsub r%u, r%u,r%u", rd, rt, rt);
         ALU_PATTERN(Iop_Sub64);
         break;

      default:
         goto decode_failure;
      }
      break;

   case 0x01:  /* Regimm */

      switch (rt) {
      case 0x00:  /* BLTZ */
         DIP("bltz r%d, %d", rs, imm);
         if (mode64) {
            if (!dis_instr_branch(cins, &dres, resteerOkFn,
                        callback_opaque, &bstmt))
               goto decode_failure;
         } else
            dis_branch(False, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                       mkU32(0x80000000)), mkU32(0x80000000)), imm, &bstmt);
         break;

      case 0x01:  /* BGEZ */
         DIP("bgez r%d, %d", rs, imm);
         if (mode64) {
            if (!dis_instr_branch(cins, &dres, resteerOkFn,
                                  callback_opaque, &bstmt))
               goto decode_failure;
         } else
            dis_branch(False, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                              mkU32(0x80000000)), mkU32(0x0)), imm, &bstmt);
         break;

      case 0x02:  /* BLTZL */
         DIP("bltzl r%d, %d", rs, imm);
         lastn = dis_branch_likely(binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                     binop(mode64 ? Iop_And64 : Iop_And32, getIReg(rs),
                     mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                     mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                     imm);
         break;

      case 0x03:  /* BGEZL */
         DIP("bgezl r%d, %d", rs, imm);
         lastn = dis_branch_likely(binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                     binop(mode64 ? Iop_And64 : Iop_And32, getIReg(rs),
                     mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                     mode64 ? mkU64(0x0) : mkU32(0x0)), imm);
         break;

      case 0x10:  /* BLTZAL */
         DIP("bltzal r%d, %d", rs, imm);
         if (mode64) {
            if (!dis_instr_branch(cins, &dres, resteerOkFn,
                        callback_opaque, &bstmt))
               goto decode_failure;
         } else
            dis_branch(True, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                       mkU32(0x80000000)), mkU32(0x80000000)), imm, &bstmt);
         break;

      case 0x12:  /* BLTZALL */
         DIP("bltzall r%d, %d", rs, imm);
         putIReg(31, mode64 ? mkU64(guest_PC_curr_instr + 8) :
                              mkU32(guest_PC_curr_instr + 8));
         lastn = dis_branch_likely(binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                     binop(mode64 ? Iop_And64 : Iop_And32, getIReg(rs),
                     mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                     mode64 ? mkU64(0x8000000000000000ULL) : mkU32(0x80000000)),
                     imm);
         break;

      case 0x11:  /* BGEZAL */
         DIP("bgezal r%d, %d", rs, imm);
         if (mode64) {
            if (!dis_instr_branch(cins, &dres, resteerOkFn,
                        callback_opaque, &bstmt))
               goto decode_failure;
         } else
            dis_branch(True, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                       mkU32(0x80000000)), mkU32(0x0)), imm, &bstmt);
         break;

      case 0x13:  /* BGEZALL */
         DIP("bgezall r%d, %d", rs, imm);
         if (mode64) {
            putIReg(31, mkU64(guest_PC_curr_instr + 8));
            lastn = dis_branch_likely(binop(Iop_CmpNE64,
                                            binop(Iop_And64,
                                                  getIReg(rs),
                                                  mkU64(0x8000000000000000ULL)),
                                            mkU64(0x0)),
                                      imm);
         } else {
            putIReg(31, mkU32(guest_PC_curr_instr + 8));
            lastn = dis_branch_likely(binop(Iop_CmpNE32, binop(Iop_And32,
                                      getIReg(rs), mkU32(0x80000000)),
                                      mkU32(0x0)), imm);
         }
         break;

      case 0x08:  /* TGEI */
         DIP("tgei r%d, %d %d", rs, imm, trap_code);
         if (mode64) {
            stmt (IRStmt_Exit (unop (Iop_Not1,
                                     binop (Iop_CmpLT64S,
                                            getIReg (rs),
                                            mkU64 (extend_s_16to64 (imm)))),
                             Ijk_SigTRAP,
                             IRConst_U64(guest_PC_curr_instr + 4),
                             OFFB_PC));
         } else {
            stmt (IRStmt_Exit (unop (Iop_Not1,
                                     binop (Iop_CmpLT32S,
                                     getIReg (rs),
                                     mkU32 (extend_s_16to32 (imm)))),
                             Ijk_SigTRAP,
                             IRConst_U32(guest_PC_curr_instr + 4),
                             OFFB_PC));
         }
         break;

      case 0x09: {  /* TGEIU */
         DIP("tgeiu r%d, %d %d", rs, imm, trap_code);
         if (mode64) {
            stmt (IRStmt_Exit (unop (Iop_Not1,
                                     binop (Iop_CmpLT64U,
                                            getIReg (rs),
                                            mkU64 (extend_s_16to64 (imm)))),
                             Ijk_SigTRAP,
                             IRConst_U64(guest_PC_curr_instr + 4),
                             OFFB_PC));
         } else {
            stmt (IRStmt_Exit (unop (Iop_Not1,
                                     binop (Iop_CmpLT32U,
                                            getIReg (rs),
                                            mkU32 (extend_s_16to32 (imm)))),
                               Ijk_SigTRAP,
                               IRConst_U32(guest_PC_curr_instr + 4),
                               OFFB_PC));
         }
         break;
      }
      case 0x0A: {  /* TLTI */
         DIP("tlti r%d, %d %d", rs, imm, trap_code);
         if (mode64) {
            stmt (IRStmt_Exit (binop (Iop_CmpLT64S, getIReg (rs),
                                      mkU64 (extend_s_16to64 (imm))),
                             Ijk_SigTRAP,
                             IRConst_U64(guest_PC_curr_instr + 4),
                             OFFB_PC));
         } else {
            stmt (IRStmt_Exit (binop (Iop_CmpLT32S, getIReg (rs),
                                      mkU32 (extend_s_16to32 (imm))),
                               Ijk_SigTRAP,
                               IRConst_U32(guest_PC_curr_instr + 4),
                               OFFB_PC));
         }
         break;
      }
      case 0x0B: {  /* TLTIU */
         DIP("tltiu r%d, %d %d", rs, imm, trap_code);
         if (mode64) {
            stmt (IRStmt_Exit (binop (Iop_CmpLT64U, getIReg (rs),
                                      mkU64 (extend_s_16to64 (imm))),
                             Ijk_SigTRAP,
                             IRConst_U64(guest_PC_curr_instr + 4),
                             OFFB_PC));
         } else {
            stmt (IRStmt_Exit (binop (Iop_CmpLT32U, getIReg (rs),
                                      mkU32 (extend_s_16to32 (imm))),
                               Ijk_SigTRAP,
                               IRConst_U32(guest_PC_curr_instr + 4),
                               OFFB_PC));
         }
         break;
      }
      case 0x0C: {  /* TEQI */
          DIP("teqi r%d, %d %d", rs, imm, trap_code);
         if (mode64) {
            stmt (IRStmt_Exit (binop (Iop_CmpEQ64, getIReg (rs),
                                      mkU64 (extend_s_16to64 (imm))),
                               Ijk_SigTRAP,
                               IRConst_U64(guest_PC_curr_instr + 4),
                               OFFB_PC));
         } else {
            stmt (IRStmt_Exit (binop (Iop_CmpEQ32, getIReg (rs),
                                      mkU32 (extend_s_16to32 (imm))),
                               Ijk_SigTRAP,
                               IRConst_U32(guest_PC_curr_instr + 4),
                               OFFB_PC));
         }
         break;
      }
      case 0x0E: {  /* TNEI */
         DIP("tnei r%d, %d %d", rs, imm, trap_code);
         if (mode64) {
            stmt (IRStmt_Exit (binop (Iop_CmpNE64, getIReg (rs),
                                      mkU64 (extend_s_16to64 (imm))),
                               Ijk_SigTRAP,
                               IRConst_U64(guest_PC_curr_instr + 4),
                               OFFB_PC));
         } else {
            stmt (IRStmt_Exit (binop (Iop_CmpNE32, getIReg (rs),
                                      mkU32 (extend_s_16to32 (imm))),
                               Ijk_SigTRAP,
                               IRConst_U32(guest_PC_curr_instr + 4),
                               OFFB_PC));
         }
         break;
      }
      case 0x1C: {  /* BPOSGE32 */
         DIP("bposge32 %d", imm);
         vassert(!mode64);
         t0 = newTemp(Ity_I32);
         /* Get pos field from DSPControl register. */
         assign(t0, binop(Iop_And32, getDSPControl(), mkU32(0x3f)));
         dis_branch(False, unop(Iop_Not1, binop(Iop_CmpLT32U, mkexpr(t0),
                                mkU32(32))), imm, &bstmt);
      }
      case 0x1F:
         /* SYNCI */
         /* Just ignore it */
         break;

      default:
         goto decode_failure;
      }
      break;

   case 0x04:
      DIP("beq r%d, r%d, %d", rs, rt, imm);
      if (mode64)
         dis_branch(False, binop(Iop_CmpEQ64, getIReg(rs), getIReg(rt)),
                                 imm, &bstmt);
      else
         dis_branch(False, binop(Iop_CmpEQ32, getIReg(rs), getIReg(rt)),
                                 imm, &bstmt);
      break;

   case 0x14:
      DIP("beql r%d, r%d, %d", rs, rt, imm);
      lastn = dis_branch_likely(binop(mode64 ? Iop_CmpNE64 : Iop_CmpNE32,
                                getIReg(rs), getIReg(rt)), imm);
      break;

   case 0x05:
      DIP("bne r%d, r%d, %d", rs, rt, imm);
      if (mode64)
         dis_branch(False, binop(Iop_CmpNE64, getIReg(rs), getIReg(rt)),
                                 imm, &bstmt);
      else
         dis_branch(False, binop(Iop_CmpNE32, getIReg(rs), getIReg(rt)),
                                 imm, &bstmt);
      break;

   case 0x15:
      DIP("bnel r%d, r%d, %d", rs, rt, imm);
      lastn = dis_branch_likely(binop(mode64 ? Iop_CmpEQ64 : Iop_CmpEQ32,
                                      getIReg(rs), getIReg(rt)), imm);
      break;

   case 0x07:  /* BGTZ */
      DIP("bgtz r%d, %d", rs, imm);
      if (mode64)
         dis_branch(False, unop(Iop_Not1, binop(Iop_CmpLE64S, getIReg(rs),
                                mkU64(0x00))), imm, &bstmt);
      else
         dis_branch(False, unop(Iop_Not1, binop(Iop_CmpLE32S, getIReg(rs),
                                mkU32(0x00))), imm, &bstmt);
      break;

   case 0x17:  /* BGTZL */
      DIP("bgtzl r%d, %d", rs, imm);
      if (mode64)
         lastn = dis_branch_likely(binop(Iop_CmpLE64S, getIReg(rs),
                                         mkU64(0x00)), imm);
      else
         lastn = dis_branch_likely(binop(Iop_CmpLE32S, getIReg(rs),
                                         mkU32(0x00)), imm);
      break;

   case 0x06:  /* BLEZ */
      DIP("blez r%d, %d", rs, imm);
      if (mode64)
         dis_branch(False, binop(Iop_CmpLE64S, getIReg(rs), mkU64(0x0)),
                                imm, &bstmt);
      else
         dis_branch(False,binop(Iop_CmpLE32S, getIReg(rs), mkU32(0x0)), imm,
                                &bstmt);
      break;

   case 0x16:  /* BLEZL */
      DIP("blezl r%d, %d", rs, imm);
      lastn = dis_branch_likely(unop(Iop_Not1, (binop(mode64 ? Iop_CmpLE64S :
                                     Iop_CmpLE32S, getIReg(rs), mode64 ?
                                     mkU64(0x0) : mkU32(0x0)))), imm);
      break;

   case 0x08: {  /* ADDI */
      DIP("addi r%d, r%d, %d", rt, rs, imm);
      IRTemp tmpRs32 = newTemp(Ity_I32);
      assign(tmpRs32, mkNarrowTo32(ty, getIReg(rs)));

      t0 = newTemp(Ity_I32);
      t1 = newTemp(Ity_I32);
      t2 = newTemp(Ity_I32);
      t3 = newTemp(Ity_I32);
      t4 = newTemp(Ity_I32);
      /* dst = src0 + sign(imm)
         if(sign(src0 ) != sign(imm ))
         goto no overflow;
         if(sign(dst) == sign(src0 ))
         goto no overflow;
         we have overflow! */

      assign(t0, binop(Iop_Add32, mkexpr(tmpRs32),
                       mkU32(extend_s_16to32(imm))));
      assign(t1, binop(Iop_Xor32, mkexpr(tmpRs32),
                       mkU32(extend_s_16to32(imm))));
      assign(t2, unop(Iop_1Sto32, binop(Iop_CmpEQ32, binop(Iop_And32,
                      mkexpr(t1), mkU32(0x80000000)), mkU32(0x80000000))));

      assign(t3, binop(Iop_Xor32, mkexpr(t0), mkexpr(tmpRs32)));
      assign(t4, unop(Iop_1Sto32, binop(Iop_CmpNE32, binop(Iop_And32,
                      mkexpr(t3), mkU32(0x80000000)), mkU32(0x80000000))));

      stmt(IRStmt_Exit(binop(Iop_CmpEQ32, binop(Iop_Or32, mkexpr(t2),
                             mkexpr(t4)), mkU32(0)), Ijk_SigFPE_IntOvf,
                       mode64 ? IRConst_U64(guest_PC_curr_instr + 4) :
                                IRConst_U32(guest_PC_curr_instr + 4),
                       OFFB_PC));

      putIReg(rt,  mkWidenFrom32(ty, mkexpr(t0), True));
      break;
   }
   case 0x09:  /* ADDIU */
      DIP("addiu r%d, r%d, %d", rt, rs, imm);
      if (mode64) {
         putIReg(rt, mkWidenFrom32(ty, binop(Iop_Add32,
                     mkNarrowTo32(ty, getIReg(rs)),mkU32(extend_s_16to32(imm))),
                     True));
      } else
         putIReg(rt, binop(Iop_Add32, getIReg(rs),mkU32(extend_s_16to32(imm))));
      break;

   case 0x0C:  /* ANDI */
      DIP("andi r%d, r%d, %d", rt, rs, imm);
      if (mode64) {
         ALUI_PATTERN64(Iop_And64);
      } else {
         ALUI_PATTERN(Iop_And32);
      }
      break;

   case 0x0E:  /* XORI */
      DIP("xori r%d, r%d, %d", rt, rs, imm);
      if (mode64) {
         ALUI_PATTERN64(Iop_Xor64);
      } else {
         ALUI_PATTERN(Iop_Xor32);
      }
      break;

   case 0x0D:  /* ORI */
      DIP("ori r%d, r%d, %d", rt, rs, imm);
      if (mode64) {
         ALUI_PATTERN64(Iop_Or64);
      } else {
         ALUI_PATTERN(Iop_Or32);
      }
      break;

   case 0x0A:  /* SLTI */
      DIP("slti r%d, r%d, %d", rt, rs, imm);
      if (mode64)
         putIReg(rt, unop(Iop_1Uto64, binop(Iop_CmpLT64S, getIReg(rs),
                                            mkU64(extend_s_16to64(imm)))));
      else
         putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpLT32S, getIReg(rs),
                                            mkU32(extend_s_16to32(imm)))));
      break;

   case 0x0B:  /* SLTIU */
      DIP("sltiu r%d, r%d, %d", rt, rs, imm);
      if (mode64)
         putIReg(rt, unop(Iop_1Uto64, binop(Iop_CmpLT64U, getIReg(rs),
                                            mkU64(extend_s_16to64(imm)))));
      else
         putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpLT32U, getIReg(rs),
                                            mkU32(extend_s_16to32(imm)))));
      break;

   case 0x18: {  /* Doubleword Add Immidiate - DADD; MIPS64 */
      DIP("daddi r%d, r%d, %d", rt, rs, imm);
      IRTemp tmpRs64 = newTemp(Ity_I64);
      assign(tmpRs64, getIReg(rs));

      t0 = newTemp(Ity_I64);
      t1 = newTemp(Ity_I64);
      t2 = newTemp(Ity_I64);
      t3 = newTemp(Ity_I64);
      t4 = newTemp(Ity_I64);
      /* dst = src0 + sign(imm)
         if(sign(src0 ) != sign(imm ))
         goto no overflow;
         if(sign(dst) == sign(src0 ))
         goto no overflow;
         we have overflow! */

      assign(t0, binop(Iop_Add64, mkexpr(tmpRs64),
                       mkU64(extend_s_16to64(imm))));
      assign(t1, binop(Iop_Xor64, mkexpr(tmpRs64),
                       mkU64(extend_s_16to64(imm))));
      assign(t2, unop(Iop_1Sto64, binop(Iop_CmpEQ64, binop(Iop_And64,
                      mkexpr(t1), mkU64(0x8000000000000000ULL)),
                                        mkU64(0x8000000000000000ULL))));

      assign(t3, binop(Iop_Xor64, mkexpr(t0), mkexpr(tmpRs64)));
      assign(t4, unop(Iop_1Sto64, binop(Iop_CmpNE64, binop(Iop_And64,
                      mkexpr(t3), mkU64(0x8000000000000000ULL)),
                                        mkU64(0x8000000000000000ULL))));

      stmt(IRStmt_Exit(binop(Iop_CmpEQ64, binop(Iop_Or64, mkexpr(t2),
                             mkexpr(t4)), mkU64(0)), Ijk_SigFPE_IntOvf,
                       IRConst_U64(guest_PC_curr_instr + 4),
                       OFFB_PC));

      putIReg(rt,  mkexpr(t0));
      break;
   }

   case 0x19:  /* Doubleword Add Immidiate Unsigned - DADDIU; MIPS64 */
      DIP("daddiu r%d, r%d, %d", rt, rs, imm);
      putIReg(rt, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
      break;

   case 0x1A: {
      /* Load Doubleword Left - LDL; MIPS64 */
      vassert(mode64);
      DIP("ldl r%u, %d(r%u)", rt, imm, rs);
      /* t1 = addr */
#if defined (_MIPSEL)
      t1 = newTemp(Ity_I64);
      assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
#elif defined (_MIPSEB)
      t1 = newTemp(Ity_I64);
      assign(t1, binop(Iop_Xor64, mkU64(0x7), binop(Iop_Add64, getIReg(rs),
                                  mkU64(extend_s_16to64(imm)))));
#endif
      /* t2 = word addr */
      /* t4 = addr mod 8 */
      LWX_SWX_PATTERN64_1;

      /* t3 = word content - shifted */
      t3 = newTemp(Ity_I64);
      assign(t3, binop(Iop_Shl64, load(Ity_I64, mkexpr(t2)),
                 narrowTo(Ity_I8, binop(Iop_Shl64, binop(Iop_Sub64, mkU64(0x07),
                 mkexpr(t4)), mkU8(3)))));

      /* rt content  - adjusted */
      t5 = newTemp(Ity_I64);
      t6 = newTemp(Ity_I64);
      t7 = newTemp(Ity_I64);

      assign(t5, binop(Iop_Mul64, mkexpr(t4), mkU64(0x8)));

      assign(t6, binop(Iop_Shr64, mkU64(0x00FFFFFFFFFFFFFFULL),
                       narrowTo(Ity_I8, mkexpr(t5))));

      assign(t7, binop(Iop_And64, getIReg(rt), mkexpr(t6)));

      putIReg(rt, binop(Iop_Or64, mkexpr(t7), mkexpr(t3)));
      break;
   }

   case 0x1B: {
      /* Load Doubleword Right - LDR; MIPS64 */
      vassert(mode64);
      DIP("ldr r%u,%d(r%u)", rt, imm, rs);
      /* t1 = addr */
#if defined (_MIPSEL)
      t1 = newTemp(Ity_I64);
      assign(t1, binop(Iop_Add64, getIReg(rs), mkU64(extend_s_16to64(imm))));
#elif defined (_MIPSEB)
      t1 = newTemp(Ity_I64);
      assign(t1, binop(Iop_Xor64, mkU64(0x7), binop(Iop_Add64, getIReg(rs),
                                  mkU64(extend_s_16to64(imm)))));
#endif
      /* t2 = word addr */
      /* t4 = addr mod 8 */
      LWX_SWX_PATTERN64_1;

      /* t3 = word content - shifted */
      t3 = newTemp(Ity_I64);
      assign(t3, binop(Iop_Shr64, load(Ity_I64, mkexpr(t2)),
                 narrowTo(Ity_I8, binop(Iop_Shl64, mkexpr(t4), mkU8(3)))));

      /* rt content  - adjusted */
      t5 = newTemp(Ity_I64);
      assign(t5, binop(Iop_And64, getIReg(rt), unop(Iop_Not64,
                 binop(Iop_Shr64, mkU64(0xFFFFFFFFFFFFFFFFULL),
                 narrowTo(Ity_I8, binop(Iop_Shl64, mkexpr(t4), mkU8(0x3)))))));

      putIReg(rt, binop(Iop_Or64, mkexpr(t5), mkexpr(t3)));
      break;
   }

   case 0x27:  /* Load Word unsigned - LWU; MIPS64 */
      DIP("lwu r%u,%d(r%u)", rt, imm, rs);
      LOAD_STORE_PATTERN;

      putIReg(rt, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)), False));
      break;

   case 0x30:  /* LL / LWC0 */
      DIP("ll r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;

      t2 = newTemp(Ity_I32);
#if defined (_MIPSEL)
      stmt(IRStmt_LLSC(Iend_LE, t2, mkexpr(t1), NULL /* this is a load */ ));
#elif defined (_MIPSEB)
      stmt(IRStmt_LLSC(Iend_BE, t2, mkexpr(t1), NULL /* this is a load */ ));
#endif
      if (mode64)
         putIReg(rt, unop(Iop_32Sto64, mkexpr(t2)));
      else
         putIReg(rt, mkexpr(t2));
      break;

   case 0x34:  /* Load Linked Doubleword - LLD; MIPS64 */
      DIP("lld r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;

      t2 = newTemp(Ity_I64);
#if defined (_MIPSEL)
      stmt(IRStmt_LLSC
           (Iend_LE, t2, mkexpr(t1), NULL /* this is a load */ ));
#elif defined (_MIPSEB)
      stmt(IRStmt_LLSC
           (Iend_BE, t2, mkexpr(t1), NULL /* this is a load */ ));
#endif

      putIReg(rt, mkexpr(t2));
      break;

   case 0x38:  /* SC / SWC0 */
      DIP("sc r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;

      t2 = newTemp(Ity_I1);
#if defined (_MIPSEL)
      stmt(IRStmt_LLSC(Iend_LE, t2, mkexpr(t1), mkNarrowTo32(ty, getIReg(rt))));
#elif defined (_MIPSEB)
      stmt(IRStmt_LLSC(Iend_BE, t2, mkexpr(t1), mkNarrowTo32(ty, getIReg(rt))));
#endif

      putIReg(rt, unop(mode64 ? Iop_1Uto64 : Iop_1Uto32, mkexpr(t2)));
      break;

   case 0x3C:  /* Store Conditional Doubleword - SCD; MIPS64 */
      DIP("sdc r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;

      t2 = newTemp(Ity_I1);
#if defined (_MIPSEL)
      stmt(IRStmt_LLSC(Iend_LE, t2, mkexpr(t1), getIReg(rt)));
#elif defined (_MIPSEB)
      stmt(IRStmt_LLSC(Iend_BE, t2, mkexpr(t1), getIReg(rt)));
#endif

      putIReg(rt, unop(Iop_1Uto64, mkexpr(t2)));
      break;

   case 0x37:  /* Load Doubleword - LD; MIPS64 */
      DIP("ld r%u, %d(r%u)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      putIReg(rt, load(Ity_I64, mkexpr(t1)));
      break;

   case 0x3F:  /* Store Doubleword - SD; MIPS64 */
      DIP("sd r%u, %d(r%u)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), getIReg(rt));
      break;

   decode_failure_dsp:
      vex_printf("Error occured while trying to decode MIPS32 DSP "
                 "instruction.\nYour platform probably doesn't support "
                 "MIPS32 DSP ASE.\n");
   decode_failure:
      /* All decode failures end up here. */
      if (sigill_diag)
         vex_printf("vex mips->IR: unhandled instruction bytes: "
                    "0x%x 0x%x 0x%x 0x%x\n",
                    (Int) getIByte(delta_start + 0),
                    (Int) getIByte(delta_start + 1),
                    (Int) getIByte(delta_start + 2),
                    (Int) getIByte(delta_start + 3));

      /* Tell the dispatcher that this insn cannot be decoded, and so has
         not been executed, and (is currently) the next to be executed.
         EIP should be up-to-date since it made so at the start bnezof each
         insn, but nevertheless be paranoid and update it again right
         now. */
      if (mode64) {
         stmt(IRStmt_Put(offsetof(VexGuestMIPS64State, guest_PC),
              mkU64(guest_PC_curr_instr)));
         jmp_lit64(&dres, Ijk_NoDecode, guest_PC_curr_instr);
      } else {
         stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_PC),
              mkU32(guest_PC_curr_instr)));
         jmp_lit32(&dres, Ijk_NoDecode, guest_PC_curr_instr);
      }
      dres.whatNext = Dis_StopHere;
      dres.len = 0;
      return dres;
   }  /* switch (opc) for the main (primary) opcode switch. */

   /* All MIPS insn have 4 bytes */

   if (delay_slot_branch) {
      delay_slot_branch = False;
      stmt(bstmt);
      bstmt = NULL;
      if (mode64)
         putPC(mkU64(guest_PC_curr_instr + 4));
      else
         putPC(mkU32(guest_PC_curr_instr + 4));
      dres.jk_StopHere = is_Branch_or_Jump_and_Link(guest_code + delta - 4) ?
                         Ijk_Call : Ijk_Boring;
   }

   if (likely_delay_slot) {
      dres.jk_StopHere = Ijk_Boring;
      dres.whatNext = Dis_StopHere;
      putPC(lastn);
      lastn = NULL;
   }
   if (delay_slot_jump) {
      putPC(lastn);
      lastn = NULL;
      dres.jk_StopHere = is_Branch_or_Jump_and_Link(guest_code + delta - 4) ?
                         Ijk_Call : Ijk_Boring;
   }

 decode_success:
   /* All decode successes end up here. */
   switch (dres.whatNext) {
      case Dis_Continue:
         if (mode64)
            putPC(mkU64(guest_PC_curr_instr + 4));
         else
            putPC(mkU32(guest_PC_curr_instr + 4));
         break;
      case Dis_ResteerU:
      case Dis_ResteerC:
         putPC(mkU32(dres.continueAt));
         break;
      case Dis_StopHere:
         break;
      default:
         vassert(0);
         break;
   }

   /* On MIPS we need to check if the last instruction in block is branch or
      jump. */
   if (((vex_control.guest_max_insns - 1) == (delta + 4) / 4)
       &&  (dres.whatNext != Dis_StopHere))
      if (branch_or_jump(guest_code + delta + 4)) {
         dres.whatNext = Dis_StopHere;
         dres.jk_StopHere = Ijk_Boring;
         if (mode64)
            putPC(mkU64(guest_PC_curr_instr + 4));
         else
            putPC(mkU32(guest_PC_curr_instr + 4));
      }
   dres.len = 4;

   DIP("\n");

   return dres;

}

/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */
DisResult disInstr_MIPS( IRSB*        irsb_IN,
                         Bool         (*resteerOkFn) ( void *, Addr64 ),
                         Bool         resteerCisOk,
                         void*        callback_opaque,
                         UChar*       guest_code_IN,
                         Long         delta,
                         Addr64       guest_IP,
                         VexArch      guest_arch,
                         VexArchInfo* archinfo,
                         VexAbiInfo*  abiinfo,
                         Bool         host_bigendian_IN,
                         Bool         sigill_diag_IN )
{
   DisResult dres;
   /* Set globals (see top of this file) */
   vassert(guest_arch == VexArchMIPS32 || guest_arch == VexArchMIPS64);

   mode64 = guest_arch != VexArchMIPS32;
#if (__mips_fpr==64)
   fp_mode64 = ((VEX_MIPS_REV(archinfo->hwcaps) == VEX_PRID_CPU_32FPR)
                || guest_arch == VexArchMIPS64);
#endif

   guest_code = guest_code_IN;
   irsb = irsb_IN;
   host_is_bigendian = host_bigendian_IN;
#if defined(VGP_mips32_linux)
   guest_PC_curr_instr = (Addr32)guest_IP;
#elif defined(VGP_mips64_linux)
   guest_PC_curr_instr = (Addr64)guest_IP;
#endif

   dres = disInstr_MIPS_WRK(resteerOkFn, resteerCisOk, callback_opaque,
                            delta, archinfo, abiinfo, sigill_diag_IN);

   return dres;
}

/*--------------------------------------------------------------------*/
/*--- end                                        guest_mips_toIR.c ---*/
/*--------------------------------------------------------------------*/
