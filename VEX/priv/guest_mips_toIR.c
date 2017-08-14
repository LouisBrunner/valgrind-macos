
/*--------------------------------------------------------------------*/
/*--- begin                                      guest_mips_toIR.c ---*/
/*--------------------------------------------------------------------*/

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

/* Translates MIPS code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_mips32.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_mips_defs.h"

/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of a instruction, so
   that we don't have to pass them around endlessly.  CONST means does
   not change during translation of the instruction.
*/

/* CONST: is the host bigendian?  This has to do with float vs double
   register accesses on VFP, but it's complex and not properly thought
   out. */
static Bool host_is_bigendian;

/* Pointer to the guest code area. */
static UChar *guest_code;

/* The guest address corresponding to guest_code[0]. */
static Addr32 guest_PC_bbstart;

/* CONST: The guest address for the instruction currently being
   translated. */
static Addr32 guest_PC_curr_instr;

/* MOD: The IRSB* into which we're generating code. */
static IRSB *irsb;

/* Is our guest binary 32 or 64bit?  Set at each call to
   disInstr_MIPS below. */
static Bool mode64 = False;

/*------------------------------------------------------------*/
/*--- Debugging output                                     ---*/
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
   return ret;
}

#define OFFB_PC     offsetof(VexGuestMIPS32State, guest_PC)

/* ---------------- Floating point registers ---------------- */

static UInt floatGuestRegOffset(UInt fregNo)
{
   vassert(fregNo < 32);
   UInt ret;
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
   return ret;
}

/* Do a endian load of a 32-bit word, regardless of the
   endianness of the underlying host. */
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
    t1 = newTemp(Ity_I32); \
    assign(t1, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm)))); \

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

#define SXX_PATTERN(op) \
   putIReg(rd, binop(op, getIReg(rt), mkU8(sa)));

#define ALU_PATTERN(op) \
   putIReg(rd, binop(op, getIReg(rs), getIReg(rt)));

#define ALUI_PATTERN(op) \
   putIReg(rt, binop(op, getIReg(rs), mkU32(imm)));

#define ALUI_PATTERN64(op) \
   putIReg(rt, binop(op, getIReg(rs), mkU64(imm)));

#define FP_CONDITIONAL_CODE \
    t3 = newTemp(Ity_I32);  \
    assign(t3, binop(Iop_And32, IRExpr_Mux0X( unop(Iop_1Uto8, \
               binop(Iop_CmpEQ32, mkU32(cc), mkU32(0))), \
               binop(Iop_Shr32, getFCSR(), mkU8(24+cc)),  \
               binop(Iop_Shr32, getFCSR(), mkU8(23))), mkU32(0x1)));

/*------------------------------------------------------------*/
/*---                           Field helpers              ---*/
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

/* Create an expression node for a 32-bit integer constant */
static IRExpr *mkU32(UInt i)
{
   return IRExpr_Const(IRConst_U32(i));
}

/* Create an expression node for a 64-bit integer constant */
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

static UInt extend_s_16to32(UInt x)
{
   return (UInt) ((((Int) x) << 16) >> 16);
}

static UInt extend_s_18to32(UInt x)
{
   return (UInt) ((((Int) x) << 14) >> 14);
}

static void jmp_lit( /*MOD*/DisResult* dres,
                     IRJumpKind kind, Addr32 d32 )
{
   vassert(dres->whatNext    == Dis_Continue);
   vassert(dres->len         == 0);
   vassert(dres->continueAt  == 0);
   vassert(dres->jk_StopHere == Ijk_INVALID);
   dres->whatNext    = Dis_StopHere;
   dres->jk_StopHere = kind;
   stmt( IRStmt_Put( OFFB_PC, mkU32(d32) ) );
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
   return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_HI), Ity_I32);
}

static IRExpr *getLO(void)
{
   return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_LO), Ity_I32);
}

static IRExpr *getFCSR(void)
{
   return IRExpr_Get(offsetof(VexGuestMIPS32State, guest_FCSR), Ity_I32);
}

static void putFCSR(IRExpr * e)
{
   stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_FCSR), e));
}

static IRExpr *getULR(void)
{
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

static void putLO(IRExpr * e)
{
   stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_LO), e));
}

static void putHI(IRExpr * e)
{
   stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_HI), e));
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

   if (vex_traceflags & VEX_TRACE_FE) {
      vex_printf("\nsrc, dst tys are: ");
      ppIRType(src_ty);
      vex_printf(", ");
      ppIRType(dst_ty);
      vex_printf("\n");
   }

   vpanic("narrowTo(mips)");
   return 0;
}

static IRExpr *mkNarrowTo32(IRType ty, IRExpr * src)
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to32, src) : src;
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
   return ty == Ity_F64 ? unop(Iop_F32toF64, src) : src;
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
   branch_offset = extend_s_18to32(imm << 2);

   t0 = newTemp(Ity_I1);
   assign(t0, guard);

   stmt(IRStmt_Exit(mkexpr(t0), Ijk_Boring, 
                    IRConst_U32(guest_PC_curr_instr + 8), OFFB_PC));

   irsb->jumpkind = Ijk_Boring;

   return mkU32(guest_PC_curr_instr + 4 + branch_offset);
}

static void dis_branch(Bool link, IRExpr * guard, UInt imm, IRStmt ** set)
{
   ULong branch_offset;
   IRTemp t0;

   if (link) {    // LR (GPR31) = addr of the 2nd instr after branch instr
      putIReg(31, mkU32(guest_PC_curr_instr + 8));
   }

   /* PC = PC + (SignExtend(signed_immed_24) << 2)
      An 18-bit signed offset (the 16-bit offset field shifted left 2 bits) 
      is added to the address of the instruction following
      the branch (not the branch itself), in the branch delay slot, to form 
      a PC-relative effective target address. */

   branch_offset = extend_s_18to32(imm << 2);

   t0 = newTemp(Ity_I1);
   assign(t0, guard);
   *set = IRStmt_Exit(mkexpr(t0), link ? Ijk_Call : Ijk_Boring,
                   IRConst_U32(guest_PC_curr_instr + 4 + (UInt) branch_offset),
                   OFFB_PC);
}

static IRExpr *getFReg(UInt dregNo)
{
   vassert(dregNo < 32);
   IRType ty = mode64 ? Ity_F64 : Ity_F32;
   return IRExpr_Get(floatGuestRegOffset(dregNo), ty);
}

static IRExpr *getDReg(UInt dregNo)
{
   vassert(dregNo < 32);
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

static void putFReg(UInt dregNo, IRExpr * e)
{
   vassert(dregNo < 32);
   IRType ty = mode64 ? Ity_F64 : Ity_F32;
   vassert(typeOfIRExpr(irsb->tyenv, e) == ty);
   stmt(IRStmt_Put(floatGuestRegOffset(dregNo), e));
}

static void putDReg(UInt dregNo, IRExpr * e)
{
   vassert(dregNo < 32);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_F64);
   IRTemp t1 = newTemp(Ity_F64);
   IRTemp t4 = newTemp(Ity_I32);
   IRTemp t5 = newTemp(Ity_I32);
   IRTemp t6 = newTemp(Ity_I64);
   assign(t1, e);
   assign(t6, unop(Iop_ReinterpF64asI64, mkexpr(t1)));
   assign(t4, unop(Iop_64HIto32, mkexpr(t6)));  // hi
   assign(t5, unop(Iop_64to32, mkexpr(t6))); //lo
   putFReg(dregNo, unop(Iop_ReinterpI32asF32, mkexpr(t5)));
   putFReg(dregNo + 1, unop(Iop_ReinterpI32asF32, mkexpr(t4)));
}

static void setFPUCondCode(IRExpr * e, UInt cc)
{
   if (cc == 0) {
      DIP("setFpu: %d\n", cc);
      putFCSR(binop(Iop_And32, getFCSR(), mkU32(0xFF7FFFFF)));
      putFCSR(binop(Iop_Or32, getFCSR(), binop(Iop_Shl32, e, mkU8(23))));
   } else {
      DIP("setFpu1: %d\n", cc);
      putFCSR(binop(Iop_And32, getFCSR(), unop(Iop_Not32, 
                               binop(Iop_Shl32, mkU32(0x01000000), mkU8(cc)))));
      putFCSR(binop(Iop_Or32, getFCSR(), binop(Iop_Shl32, e, mkU8(24 + cc))));
   }
}

static IRExpr */* :: Ity_I32 */get_IR_roundingmode(void)
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

   assign(rm_MIPS, binop(Iop_And32, IRExpr_Get(offsetof(VexGuestMIPS32State,
                                    guest_FCSR), Ity_I32), mkU32(3)));

   // rm_IR = XOR( rm_MIPS32, (rm_MIPS32 << 1) & 2)

   return binop(Iop_Xor32, mkexpr(rm_MIPS), binop(Iop_And32,
                binop(Iop_Shl32, mkexpr(rm_MIPS), mkU8(1)), mkU32(2)));
}

/*********************************************************/
/*---             Floating Point Compare              ---*/
/*********************************************************/
static Bool dis_instr_CCondFmt(UInt cins)
{
   IRTemp t0, t1, t2, t3;
   IRTemp ccIR = newTemp(Ity_I32);
   IRTemp ccMIPS = newTemp(Ity_I32);
   UInt FC = get_FC(cins);
   UInt fmt = get_fmt(cins);
   UInt fs = get_fs(cins);
   UInt ft = get_ft(cins);
   UInt cond = get_cond(cins);

   if (FC == 0x3) {  // C.cond.fmt
      UInt fpc_cc = get_fpc_cc(cins);
      switch (fmt) {
         case 0x10: {  //C.cond.S
            DIP("C.cond.S %d f%d, f%d\n", fpc_cc, fs, ft);
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I32);

            assign(ccIR, binop(Iop_CmpF64, unop(Iop_F32toF64, getFReg(fs)),
                                           unop(Iop_F32toF64, getFReg(ft))));
            /* Map compare result from IR to MIPS */
            /*
               FP cmp result | MIPS | IR
               --------------------------
               UN            | 0x1 | 0x45
               EQ            | 0x2 | 0x40
               GT            | 0x4 | 0x00
               LT            | 0x8 | 0x01
             */

            // ccMIPS = Shl(1, (~(ccIR>>5) & 2)
            //                    | ((ccIR ^ (ccIR>>6)) & 1)
            assign(ccMIPS, binop(Iop_Shl32, mkU32(1), unop(Iop_32to8, 
                           binop(Iop_Or32, binop(Iop_And32, unop(Iop_Not32,
                           binop(Iop_Shr32, mkexpr(ccIR), mkU8(5))), mkU32(2)),
                           binop(Iop_And32, binop(Iop_Xor32, mkexpr(ccIR),
                           binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))), 
                           mkU32(1))))));
            assign(t0, binop(Iop_And32, mkexpr(ccMIPS), mkU32(0x1)));   // UN
            assign(t1, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                   mkU8(0x1)), mkU32(0x1))); // EQ
            assign(t2, binop(Iop_And32, unop(Iop_Not32, binop(Iop_Shr32,
                   mkexpr(ccMIPS), mkU8(0x2))), mkU32(0x1)));  // NGT
            assign(t3, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                   mkU8(0x3)), mkU32(0x1))); // LT

            switch (cond) {
               case 0x0:
                  setFPUCondCode(mkU32(0), fpc_cc);
                  break;
               case 0x1:
                  DIP("unorderd: %d\n", fpc_cc);
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

         case 0x11:  //C.cond.D
            DIP("C.%d.D %d f%d, f%d\n", cond, fpc_cc, fs, ft);
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I32);
            assign(ccIR, binop(Iop_CmpF64, getDReg(fs), getDReg(ft)));
            /* Map compare result from IR to MIPS */
            /*
               FP cmp result | MIPS | IR
               --------------------------
               UN            | 0x1 | 0x45
               EQ            | 0x2 | 0x40
               GT            | 0x4 | 0x00
               LT            | 0x8 | 0x01
             */

            // ccMIPS = Shl(1, (~(ccIR>>5) & 2)
            //                    | ((ccIR ^ (ccIR>>6)) & 1)
            assign(ccMIPS, binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,
                           binop(Iop_Or32, binop(Iop_And32, unop(Iop_Not32,
                           binop(Iop_Shr32, mkexpr(ccIR), mkU8(5))), mkU32(2)),
                           binop(Iop_And32, binop(Iop_Xor32, mkexpr(ccIR),
                           binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))),
                           mkU32(1))))));

            assign(t0, binop(Iop_And32, mkexpr(ccMIPS), mkU32(0x1)));   // UN
            assign(t1, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                   mkU8(0x1)), mkU32(0x1))); // EQ
            assign(t2, binop(Iop_And32, unop(Iop_Not32, binop(Iop_Shr32,
                   mkexpr(ccMIPS), mkU8(0x2))), mkU32(0x1)));  // NGT
            assign(t3, binop(Iop_And32, binop(Iop_Shr32, mkexpr(ccMIPS),
                   mkU8(0x3)), mkU32(0x1))); // LT

            switch (cond) {
               case 0x0:
                  setFPUCondCode(mkU32(0), fpc_cc);
                  break;
               case 0x1:
                  DIP("unorderd: %d\n", fpc_cc);
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
            break;

            default:
               return False;
      }
   } else {
      return False;
   }

   return True;
}

/*------------------------------------------------------------*/
/*--- Disassemble a single instruction                     ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction is
   located in host memory at guest_instr, and has guest IP of
   guest_PC_curr_instr, which will have been set before the call
   here. */

static DisResult disInstr_MIPS_WRK ( Bool(*resteerOkFn) (/*opaque */void *,
                                                                    Addr64),
                                     Bool         resteerCisOk,
                                     void*        callback_opaque,
                                     Long         delta64,
                                     VexArchInfo* archinfo,
                                     VexAbiInfo*  abiinfo )
{
   IRTemp t0, t1, t2, t3, t4, t5, t6, t7, t8;
   UInt opcode, cins, rs, rt, rd, sa, ft, fs, fd, fmt, tf, nd, function,
        trap_code, imm, instr_index, p, msb, lsb, size, rot, sel;

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

   if (delta != 0) {
      if (branch_or_jump(guest_code + delta - 4)) {
         if (lastn == NULL && bstmt == NULL) {
            DIP("Info: jump to delay slot insn...\n");
         } else {
            dres.whatNext = Dis_StopHere;

            DIP("lastn = %p bstmt = %p\n", lastn, bstmt);
            if (lastn != NULL) {
               DIP("delay slot jump\n");
               if (vex_traceflags & VEX_TRACE_FE)
                  ppIRExpr(lastn);
               delay_slot_jump = True;
            } else if (bstmt != NULL) {
               DIP("\ndelay slot branch\n");
               delay_slot_branch = True;
            }
            DIP("delay slot\n");
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
       "srl $0, $0, 19 */
      UInt word1 = 0x00000342;
      UInt word2 = 0x00000742;
      UInt word3 = 0x000000C2;
      UInt word4 = 0x000004C2;
      if (getUInt(code + 0) == word1 && getUInt(code + 4) == word2 &&
          getUInt(code + 8) == word3 && getUInt(code + 12) == word4) {
         /* Got a "Special" instruction preamble.  Which one is it? */
         if (getUInt(code + 16) == 0x01ad6825 /* or t5, t5, t5 */ ) {
            /* v0 = client_request ( t9 ) */
            DIP("v0 = client_request ( t9 )\n");
            putPC(mkU32(guest_PC_curr_instr + 20));
            dres.jk_StopHere = Ijk_ClientReq;
            dres.whatNext    = Dis_StopHere;

            goto decode_success;
         } else if (getUInt(code + 16) == 0x01ce7025 /* or t6,t6,t6 */ ) {
            /* t9 = guest_NRADDR */
            DIP("t9 = guest_NRADDR\n");
            dres.len = 20;
            delta += 20;
            putIReg(11, IRExpr_Get(offsetof(VexGuestMIPS32State, guest_NRADDR),
                                   Ity_I32));
            goto decode_success;
         } else if (getUInt(code + 16) == 0x01ef7825/* or t7,t7,t7 */ ) {
            /*  branch-and-link-to-noredir t9 */
            DIP("branch-and-link-to-noredir t9\n");
            putIReg(31, mkU32(guest_PC_curr_instr + 20));
            putPC(getIReg(25));
            dres.jk_StopHere = Ijk_NoRedir;
            dres.whatNext    = Dis_StopHere;
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
   IRType tyF = mode64 ? Ity_F64 : Ity_F32;

   DIP("[cins = 0x%08x] ", cins);

   switch (opcode) {

   case 0x03:     /* JAL */
      DIP("jal 0x%x", instr_index);
      putIReg(31, mkU32(guest_PC_curr_instr + 8));
      t0 = newTemp(ty);
      assign(t0, mkU32((guest_PC_curr_instr & 0xF0000000) |
                       (instr_index << 2)));
      lastn = mkexpr(t0);
      break;
   case 0x02:     /* J */
      DIP("j 0x%x", instr_index);
      t0 = newTemp(ty);
      assign(t0, mkU32((guest_PC_curr_instr & 0xF0000000) |
                       (instr_index << 2)));
      lastn = mkexpr(t0);
      break;

   case 0x11:     /* COP1 */
      {
         UInt bc1_cc = get_bc1_cc(cins);
         if (0x08 == fmt) {
            switch (fmt) {
            case 0x08:  //BC
               {
                  DIP("tf: %d, nd: %d\n", tf, nd);
                  //FcConditionalCode(bc1_cc)
                  t1 = newTemp(Ity_I32);
                  t2 = newTemp(Ity_I32);
                  t3 = newTemp(Ity_I1);

                  assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                                    mkU32(bc1_cc))));
                  assign(t2, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                             binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                             mkU8(24 + bc1_cc)), mkU32(0x1)), binop(Iop_And32,
                             binop(Iop_Shr32, getFCSR(), mkU8(23)),
                                   mkU32(0x1))));

                  if (tf == 1 && nd == 0) {
                     //branch on true
                     DIP("bc1t %d, %d", bc1_cc, imm);
                     assign(t3, binop(Iop_CmpEQ32, mkU32(1), mkexpr(t2)));
                     dis_branch(False, mkexpr(t3), imm, &bstmt);
                     break;
                  } else if (tf == 0 && nd == 0) {
                     //branch on false
                     DIP("bc1f %d, %d", bc1_cc, imm);
                     assign(t3, binop(Iop_CmpEQ32, mkU32(0), mkexpr(t2)));
                     dis_branch(False, mkexpr(t3), imm, &bstmt);
                     break;
                  } else if (nd == 1 && tf == 0) {
                     DIP("bc1fl %d, %d", bc1_cc, imm);
                     lastn = dis_branch_likely(binop(Iop_CmpNE32, mkexpr(t2),
                           mode64 ? mkU64(0x0) : mkU32(0x0)), imm);
                     break;
                  } else if (nd == 1 && tf == 1) {
                     DIP("bc1tl %d, %d", bc1_cc, imm);
                     lastn = dis_branch_likely(binop(Iop_CmpEQ32, mkexpr(t2),
                                               mkU32(0x0)), imm);
                     break;
                  } else
                     goto decode_failure;
               }

            default:
               goto decode_failure;
            }
         } else {
            switch (function) {

            case 0x4:   //SQRT.fmt
               {
                  switch (fmt) {
                  case 0x10:  //S
                     {
                        IRExpr *rm = get_IR_roundingmode();
                        putFReg(fd, mkWidenFromF32(tyF, binop(Iop_SqrtF32, rm,
                                    getLoFromF64(tyF, getFReg(fs)))));
                     }
                     break;
                  case 0x11:  //D
                     {
                        IRExpr *rm = get_IR_roundingmode();
                        putDReg(fd, binop(Iop_SqrtF64, rm, getDReg(fs)));
                     }
                     break;
                  }
               }
               break;
            case 0x5:   //abs.fmt
               switch (fmt) {
               case 0x10:  //S
                  DIP("abs.s f%d, f%d\n", fd, fs);
                  putFReg(fd, mkWidenFromF32(tyF, unop(Iop_AbsF32,
                              getLoFromF64(tyF, getFReg(fs)))));
                  break;
               case 0x11:  //D 
                  DIP("abs.d f%d, f%d\n", fd, fs);
                  putDReg(fd, unop(Iop_AbsF64, getDReg(fs)));
                  break;
               default:
                  goto decode_failure;
               }
               break;   //case 0x5

            case 0x02:  // MUL.fmt
               switch (fmt) {
               case 0x11:  // D
                  {
                     DIP("mul.d f%d, f%d, f%d", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_MulF64, rm, getDReg(fs),
                                       getDReg(ft)));
                     break;
                  }
               case 0x10:  // S
                  {
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
               break;   // MUL.fmt

            case 0x03:  // DIV.fmt
               switch (fmt) {
               case 0x11:  // D
                  {
                     DIP("div.d f%d, f%d, f%d", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_DivF64, rm, getDReg(fs),
                                 getDReg(ft)));
                     break;
                  }
               case 0x10:  // S
                  {
                     DIP("div.s f%d, f%d, f%d", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32, rm,
                                 getLoFromF64(tyF, getFReg(fs)),
                                 getLoFromF64(tyF, getFReg(ft)))));
                     break;
                  }
               default:
                  goto decode_failure;
               }
               break;   // DIV.fmt

            case 0x01:  // SUB.fmt
               switch (fmt) {
               case 0x11:  // D
                  {
                     DIP("sub.d f%d, f%d, f%d", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_SubF64, rm, getDReg(fs), getDReg(ft)));
                     break;
                  }
               case 0x10:  // S
                  {
                     DIP("sub.s f%d, f%d, f%d", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_SubF32, rm,
                                 getLoFromF64(tyF, getFReg(fs)),
                                 getLoFromF64(tyF, getFReg(ft)))));
                     break;
                  }
               default:
                  goto decode_failure;
               }
               break;   // SUB.fmt

            case 0x06:  // MOV.fmt
               switch (fmt) {
               case 0x11:  // D
                  /* TODO: Check this for 64 bit FPU registers. */
                  DIP("mov.d f%d, f%d", fd, fs);
                  putFReg(fd, getFReg(fs));
                  putFReg(fd + 1, getFReg(fs + 1));
                  break;
               case 0x10:  // S
                  DIP("mov.s f%d, f%d", fd, fs);
                  putFReg(fd, getFReg(fs));
                  break;
               default:
                  goto decode_failure;
               }
               break;   // MOV.fmt

            case 0x7:   //neg.fmt
               switch (fmt) {
               case 0x10:  //S
                  DIP("neg.s f%d, f%d", fd, fs);
                  putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32,
                              getLoFromF64(tyF, getFReg(fs)))));
                  break;
               case 0x11:  //D 
                  DIP("neg.d f%d, f%d", fd, fs);
                  putDReg(fd, unop(Iop_NegF64, getDReg(fs)));
                  break;
               default:
                  goto decode_failure;
               }
               break;   //case 0x7

            case 0x15:  //RECIP.fmt
               switch (fmt) {
               case 0x10:
                  {  //S
                     DIP("recip.s f%d, f%d\n", fd, fs);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32,
                                 rm, unop(Iop_ReinterpI32asF32,
                                 mkU32(0x3F800000)), getLoFromF64(tyF,
                                 getFReg(fs)))));
                     break;
                  }
               case 0x11:
                  {  //D
                     DIP("recip.d f%d, f%d\n", fd, fs);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_DivF64, rm, 
                                 unop(Iop_ReinterpI64asF64,
                                 mkU64(0x3FF0000000000000ULL)), getDReg(fs)));
                     break;
                  }
               default:
                  goto decode_failure;

               }
               break;   //case 0x15

            case 0x13:  //MOVN.fmt
               switch (fmt) {
               case 0x10:  // S
                  DIP("movn.s f%d, f%d, r%d", fd, fs, rt);

                  t1 = newTemp(Ity_F64);
                  t2 = newTemp(Ity_F64);
                  t3 = newTemp(Ity_I32);
                  t4 = newTemp(Ity_F64);

                  assign(t1, unop(Iop_F32toF64, getFReg(fs)));
                  assign(t2, unop(Iop_F32toF64, getFReg(fd)));
                  assign(t3, unop(Iop_1Sto32, binop(Iop_CmpNE32, mkU32(0),
                                                    getIReg(rt))));

                  assign(t4, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                                               mkexpr(t2), mkexpr(t1)));

                  putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                    mkexpr(t4)));
                  break;
               case 0x11:  // D
                  DIP("movn.d f%d, f%d, r%d", fd, fs, rt);

                  t3 = newTemp(Ity_I32);
                  t4 = newTemp(Ity_F64);

                  assign(t3, unop(Iop_1Sto32, binop(Iop_CmpNE32, mkU32(0),
                                                    getIReg(rt))));
                  putDReg(fd, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                                                getDReg(fd), getDReg(fs)));
                  break;
               default:
                  goto decode_failure;
               }
               break;   // MOVN.fmt

            case 0x12:  //MOVZ.fmt
               switch (fmt) {
               case 0x10:  // S
                  DIP("movz.s f%d, f%d, r%d", fd, fs, rt);

                  t1 = newTemp(Ity_F64);
                  t2 = newTemp(Ity_F64);
                  t3 = newTemp(Ity_I32);
                  t4 = newTemp(Ity_F64);

                  assign(t1, unop(Iop_F32toF64, getFReg(fs)));
                  assign(t2, unop(Iop_F32toF64, getFReg(fd)));
                  assign(t3, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                                    getIReg(rt))));
                  assign(t4, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                                               mkexpr(t2), mkexpr(t1)));

                  putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                    mkexpr(t4)));

                  break;
               case 0x11:  // D
                  DIP("movz.d f%d, f%d, r%d", fd, fs, rt);

                  t3 = newTemp(Ity_I32);
                  t4 = newTemp(Ity_F64);

                  assign(t3, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                                    getIReg(rt))));
                  putDReg(fd, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                                                getDReg(fd), getDReg(fs)));
                  break;
               default:
                  goto decode_failure;
               }
               break;   // MOVZ.fmt

            case 0x11:  // MOVT.fmt
               if (tf == 1) {
                  UInt mov_cc = get_mov_cc(cins);
                  switch (fmt)   // MOVCF = 010001
                  {
                  case 0x11:  // D
                     DIP("movt.d f%d, f%d, %d", fd, fs, mov_cc);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_F64);

                     assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                                       mkU32(mov_cc))));
                     assign(t2, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                                binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                                 mkU8(24 + mov_cc)), mkU32(0x1)),
                                 binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                                 mkU8(23)), mkU32(0x1))));

                     assign(t3, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(1),
                                mkexpr(t2))));
                     assign(t4, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                                             getDReg(fs), getDReg(fd)));
                     putDReg(fd, mkexpr(t4));
                     break;
                  case 0x10:  // S
                     DIP("movt.s f%d, f%d, %d", fd, fs, mov_cc);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_F64);
                     t5 = newTemp(Ity_F64);
                     t6 = newTemp(Ity_F64);
                     t7 = newTemp(Ity_I64);

                     assign(t5, unop(Iop_F32toF64, getFReg(fs)));
                     assign(t6, unop(Iop_F32toF64, getFReg(fd)));

                     assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                     mkU32(mov_cc))));
                     assign(t2, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                                             binop(Iop_And32, binop(Iop_Shr32,
                                             getFCSR(), mkU8(24 + mov_cc)),
                                             mkU32(0x1)), binop(Iop_And32,
                                             binop(Iop_Shr32, getFCSR(),
                                             mkU8(23)), mkU32(0x1))));

                     assign(t3, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(1),
                                                       mkexpr(t2))));
                     assign(t4, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                                             mkexpr(t5), mkexpr(t6)));

                     putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                       mkexpr(t4)));
                     break;
                  default:
                     goto decode_failure;
                  }
               } else if (tf == 0)  //movf.fmt
               {
                  UInt mov_cc = get_mov_cc(cins);
                  switch (fmt)   // MOVCF = 010001
                  {
                  case 0x11:  // D
                     DIP("movf.d f%d, f%d, %d", fd, fs, mov_cc);
                     t1 = newTemp(Ity_I32);
                     t2 = newTemp(Ity_I32);
                     t3 = newTemp(Ity_I32);
                     t4 = newTemp(Ity_F64);

                     assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32,
                                                 mkU32(0), mkU32(mov_cc))));
                     assign(t2, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                                binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                                mkU8(24 + mov_cc)), mkU32(0x1)),
                                binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                                mkU8(23)), mkU32(0x1))));

                     assign(t3, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(1),
                                                       mkexpr(t2))));
                     assign(t4, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                                             getDReg(fd), getDReg(fs)));
                     putDReg(fd, mkexpr(t4));
                     break;
                  case 0x10:  // S
                     DIP("movf.s f%d, f%d, %d", fd, fs, mov_cc);
                     {
                        t1 = newTemp(Ity_I32);
                        t2 = newTemp(Ity_I32);
                        t3 = newTemp(Ity_I32);
                        t4 = newTemp(Ity_F64);
                        t5 = newTemp(Ity_F64);
                        t6 = newTemp(Ity_F64);

                        assign(t5, unop(Iop_F32toF64, getFReg(fs)));
                        assign(t6, unop(Iop_F32toF64, getFReg(fd)));

                        assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                                          mkU32(mov_cc))));
                        assign(t2, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                                   binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                                   mkU8(24 + mov_cc)), mkU32(0x1)),
                                   binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                                   mkU8(23)), mkU32(0x1))));

                        assign(t3, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(1),
                                                          mkexpr(t2))));
                        assign(t4, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                                                     mkexpr(t6), mkexpr(t5)));
                        putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                          mkexpr(t4)));
                     }
                     break;
                  default:
                     goto decode_failure;
                  }
               }

               break;   // MOVT.fmt

            case 0x0:   //add.fmt
               switch (fmt) {
               case 0x10:  //S
                  {
                     DIP("add.s f%d, f%d, f%d\n", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_AddF32, rm,
                                 getLoFromF64(tyF, getFReg(fs)),
                                 getLoFromF64(tyF, getFReg(ft)))));
                     break;
                  }
               case 0x11:  //D
                  {
                     DIP("add.d f%d, f%d, f%d\n", fd, fs, ft);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_AddF64, rm, getDReg(fs), 
                                       getDReg(ft)));
                     break;
                  }

               case 0x4:   //MTC1 (Move Word to Floating Point)
                  DIP("mtc1 r%d, f%d", rt, fs);
                  putFReg(fs, unop(Iop_ReinterpI32asF32, getIReg(rt)));
                  break;

               case 0x0:   //MFC1
                  DIP("mfc1 r%d, f%d", rt, fs);
                  putIReg(rt, unop(Iop_ReinterpF32asI32, getFReg(fs)));
                  break;

               case 0x6:   //CTC1
                  DIP("ctc1 r%d, f%d", rt, fs);
                  t0 = newTemp(Ity_I32);
                  t1 = newTemp(Ity_I32);
                  t2 = newTemp(Ity_I32);
                  t3 = newTemp(Ity_I32);
                  t4 = newTemp(Ity_I32);
                  t5 = newTemp(Ity_I32);
                  t6 = newTemp(Ity_I32);
                  assign(t0, mkNarrowTo32(ty, getIReg(rt)));
                  if (fs == 25) {   //FCCR
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
                  } else if (fs == 26) {  //FEXR
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
               case 0x2:   //CFC1
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
               break;   //case 0x0: //add.fmt

            case 0x21:  //CVT.D
               switch (fmt) {
               case 0x10:  //S
                  DIP("cvt.d.s f%d, f%d", fd, fs);
                  putDReg(fd, unop(Iop_F32toF64, getFReg(fs)));
                  break;

               case 0x14:
                  {  //W
                     DIP("cvt.d.w %d, %d\n", fd, fs);
                     t0 = newTemp(Ity_I32);
                     assign(t0, unop(Iop_ReinterpF32asI32, getFReg(fs)));
                     putDReg(fd, unop(Iop_I32StoF64, mkexpr(t0)));
                  }
                  break;

               default:
                  goto decode_failure;
               }
               break;   //CVT.D

            case 0x20:  //cvt.s
               switch (fmt) {
               case 0x14:  //W
                  DIP("cvt.s.w %d, %d\n", fd, fs);
                  t0 = newTemp(Ity_I32);
                  assign(t0, unop(Iop_ReinterpF32asI32, getFReg(fs)));
                  putFReg(fd, binop(Iop_I32StoF32, get_IR_roundingmode(),
                              mkexpr(t0)));
                  break;

               case 0x11:  //D
                  DIP("cvt.s.d %d, %d\n", fd, fs);
                  putFReg(fd, binop(Iop_F64toF32, get_IR_roundingmode(),
                                    getDReg(fs)));
                  break;

               default:
                  goto decode_failure;
               }
               break;   //cvt.s

            case 0x24:  //cvt.w
               switch (fmt) {
               case 0x10:  //S
                  DIP("cvt.w.s %d, %d\n", fd, fs);
                  putFReg(fd, binop(Iop_RoundF32toInt, get_IR_roundingmode(),
                                    getFReg(fs)));
                  break;

               case 0x11:
                  {  //D
                     DIP("cvt.w.d %d, %d\n", fd, fs);
                     t0 = newTemp(Ity_I32);

                     assign(t0, binop(Iop_F64toI32S, get_IR_roundingmode(),
                                      getDReg(fs)));

                     putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                  }
                  break;

               default:
                  goto decode_failure;

               }
               break;

            case 0x09:  //TRUNC.L
               switch (fmt) {
               case 0x10:  //S
                  DIP("trunc.l.s %d, %d\n", fd, fs);
                  goto decode_failure;

               case 0x11:  //D
                  DIP("trunc.l.d %d, %d\n", fd, fs);
                  goto decode_failure;

               default:
                  goto decode_failure;

               }
               break;   //trunc.l

            case 0x0C:  //ROUND.W.fmt
               switch (fmt) {
               case 0x10:  //S
                  DIP("round.w.s f%d, f%d\n", fd, fs);
                  putFReg(fd, binop(Iop_RoundF32toInt, mkU32(0x0),
                                    getFReg(fs)));
                  break;

               case 0x11:  //D
                  DIP("round.w.d f%d, f%d\n", fd, fs);
                  t0 = newTemp(Ity_I32);

                  assign(t0, binop(Iop_F64toI32S, mkU32(0x0), getDReg(fs)));

                  putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                  break;

               default:
                  goto decode_failure;

               }
               break;   //ROUND.W.fmt

            case 0x0F:  //FLOOR.W.fmt
               switch (fmt) {
               case 0x10:  //S
                  DIP("floor.w.s f%d, f%d\n", fd, fs);
                  putFReg(fd, binop(Iop_RoundF32toInt, mkU32(0x1),
                                    getFReg(fs)));
                  break;

               case 0x11:  //D
                  DIP("floor.w.d f%d, f%d\n", fd, fs);
                  t0 = newTemp(Ity_I32);

                  assign(t0, binop(Iop_F64toI32S, mkU32(0x1), getDReg(fs)));

                  putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                  break;

               default:
                  goto decode_failure;

               }
               break;   //FLOOR.W.fmt

            case 0x0D:  //TRUNC.W
               switch (fmt) {
               case 0x10:  //S
                  DIP("trunc.w.s %d, %d\n", fd, fs);
                  putFReg(fd, binop(Iop_RoundF32toInt, mkU32(0x3),
                                    getFReg(fs)));
                  break;

               case 0x11:  //D
                  DIP("trunc.w.d %d, %d\n", fd, fs);
                  t0 = newTemp(Ity_I32);

                  assign(t0, binop(Iop_F64toI32S, mkU32(0x3), getDReg(fs)));

                  putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                  break;

               default:
                  goto decode_failure;

               }
               break;
            case 0x0E:  //CEIL.W.fmt
               switch (fmt) {
               case 0x10:  //S
                  DIP("ceil.w.s %d, %d\n", fd, fs);
                  putFReg(fd, binop(Iop_RoundF32toInt, mkU32(0x2),
                                    getFReg(fs)));
                  break;

               case 0x11:  //D
                  DIP("ceil.w.d %d, %d\n", fd, fs);
                  t0 = newTemp(Ity_I32);

                  assign(t0, binop(Iop_F64toI32S, mkU32(0x2), getDReg(fs)));

                  putFReg(fd, unop(Iop_ReinterpI32asF32, mkexpr(t0)));
                  break;

               default:
                  goto decode_failure;

               }
               break;
            case 0x0A:  //CEIL.L.fmt
               switch (fmt) {
               case 0x10:  //S
                  DIP("ceil.l.s %d, %d\n", fd, fs);
                  goto decode_failure;

               case 0x11:  //D
                  DIP("ceil.l.d %d, %d\n", fd, fs);

                  goto decode_failure;

               default:
                  goto decode_failure;

               }
               break;

            case 0x16:  //RSQRT.fmt
               switch (fmt) {
               case 0x10:
                  {  //S
                     DIP("rsqrt.s %d, %d\n", fd, fs);
                     IRExpr *rm = get_IR_roundingmode();
                     putFReg(fd, mkWidenFromF32(tyF, triop(Iop_DivF32, rm,
                                 unop(Iop_ReinterpI32asF32, mkU32(0x3F800000)),
                                 binop(Iop_SqrtF32, rm, getLoFromF64(tyF,
                                 getFReg(fs))))));
                     break;
                  }
               case 0x11:
                  {  //D
                     DIP("rsqrt.d %d, %d\n", fd, fs);
                     IRExpr *rm = get_IR_roundingmode();
                     putDReg(fd, triop(Iop_DivF64, rm,
                                 unop(Iop_ReinterpI64asF64,
                                 mkU64(0x3FF0000000000000ULL)),
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
      break;      /*COP1 */
   case 0x10:     /* COP0 */
      if (rs == 0) { /* MFC0 */
         DIP("mfc0 r%d, r%d, %d", rt, rd, sel);

         IRTemp   val  = newTemp(Ity_I32);
         IRExpr** args = mkIRExprVec_2 (mkU32(rd), mkU32(sel));
         IRDirty *d = unsafeIRDirty_1_N(val,
                                        0,
                                        "mips32_dirtyhelper_mfc0",
                                        &mips32_dirtyhelper_mfc0,
                                        args);

         stmt(IRStmt_Dirty(d));
         putIReg(rt, mkexpr(val));
      } else
         goto decode_failure;
      break;
   case 0x31:     /* LWC1 */
      /* Load Word to Floating Point - LWC1 (MIPS32) */
      LOAD_STORE_PATTERN;
      putFReg(ft, load(Ity_F32, mkexpr(t1)));

      DIP("lwc1 f%d, %d(r%d)", ft, imm, rs);
      break;

   case 0x39:     /* SWC1 */
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), getFReg(ft));
      DIP("swc1 f%d, %d(r%d)", ft, imm, rs);
      break;

   case 0x33:     /* PREF */
      DIP("pref");
      break;

   case 0x35:
      /* Load Doubleword to Floating Point - LDC1 (MIPS32) */
      LOAD_STORE_PATTERN;

      t2 = newTemp(Ity_I32);
      assign(t2, binop(Iop_Add32, getIReg(rs),
                       mkU32(extend_s_16to32(imm + 4))));

#if defined (_MIPSEL)
      putFReg(ft, load(Ity_F32, mkexpr(t1)));
      putFReg(ft + 1, load(Ity_F32, mkexpr(t2)));
#elif defined (_MIPSEB)
      putFReg(ft + 1, load(Ity_F32, mkexpr(t1)));
      putFReg(ft, load(Ity_F32, mkexpr(t2)));
#endif
      DIP("ldc1 f%d, %d(%d) \n", rt, imm, rs);
      break;

   case 0x3D:
      /* Store Doubleword from Floating Point - SDC1 */
      LOAD_STORE_PATTERN;

      t2 = newTemp(Ity_I32);
      assign(t2, binop(Iop_Add32, getIReg(rs),
                       mkU32(extend_s_16to32(imm + 4))));

#if defined (_MIPSEL)
      store(mkexpr(t1), getFReg(ft));
      store(mkexpr(t2), getFReg(ft + 1));
#elif defined (_MIPSEB)
      store(mkexpr(t1), getFReg(ft + 1));
      store(mkexpr(t2), getFReg(ft));
#endif
      DIP("sdc1 f%d, %d(%d)", ft, imm, rs);
      break;

   case 0x23:     /* LW */
      DIP("lw r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      putIReg(rt, mkWidenFrom32(ty, load(Ity_I32, mkexpr(t1)), True));
      break;

   case 0x20:     /* LB */
      DIP("lb r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      putIReg(rt, unop(Iop_8Sto32, load(Ity_I8, mkexpr(t1))));
      break;

   case 0x24:     /* LBU */
      DIP("lbu r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      putIReg(rt, unop(Iop_8Uto32, load(Ity_I8, mkexpr(t1))));
      break;

   case 0x21:     /* LH */
      DIP("lh r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      putIReg(rt, unop(Iop_16Sto32, load(Ity_I16, mkexpr(t1))));
      break;

   case 0x25:     /* LHU */
      DIP("lhu r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      putIReg(rt, unop(Iop_16Uto32, load(Ity_I16, mkexpr(t1))));
      break;

   case 0x0F:     /* LUI */
      p = (imm << 16);
      DIP("lui rt: %d, imm: %d, imm << 16: %d", rt, imm, p);
      if ((vex_traceflags & VEX_TRACE_FE) && !mode64)
         ppIRExpr(mkU32(p));
      putIReg(rt, mkU32(p));
      break;

   case 0x13:     /* COP1X */
      switch (function) {
      case 0x0: { /* LWXC1 */
         /* Load Word  Indexed to Floating Point - LWXC1 (MIPS32r2) */
         DIP("lwxc1 f%d, r%d(r%d) \n", fd, rt, rs);
         t0 = newTemp(Ity_I32);
         assign(t0, binop(Iop_Add32, getIReg(rs), getIReg(rt)));
         putFReg(fd, load(Ity_F32, mkexpr(t0)));
         break;
      }

      case 0x1: { /* LDXC1 */
         /* Load Doubleword  Indexed to Floating Point - LDXC1 (MIPS32r2) */
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
         DIP("ldxc1 f%d, r%d(r%d) \n", fd, rt, rs);
         break;
      }

      case 0x5:   // Load Doubleword Indexed Unaligned 
         // to Floating Point - LUXC1; MIPS32r2
         DIP("luxc1 f%d, r%d(r%d) \n", fd, rt, rs);
         t0 = newTemp(Ity_I64);
         t1 = newTemp(Ity_I64);
         assign(t0, binop(Iop_Add64, getIReg(rs), getIReg(rt)));
         assign(t1, binop(Iop_And64, mkexpr(t0), mkU64(0xfffffffffffffff8ULL)));
         putFReg(fd, load(Ity_F64, mkexpr(t1)));
         break;

      case 0x8: { /* SWXC1 */
         /* Store Word Indexed from Floating Point - SWXC1 */
         t0 = newTemp(Ity_I32);
         assign(t0, binop(Iop_Add32, getIReg(rs), getIReg(rt)));

         store(mkexpr(t0), getFReg(fs));

         DIP("swxc1 f%d, r%d(r%d)", ft, rt, rs);
         break;
      }
      case 0x9: { /* SDXC1 */
         /* Store Doubleword Indexed from Floating Point - SDXC1 */
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

         DIP("sdc1 f%d, %d(%d)", ft, imm, rs);
         break;
      }
      case 0x0F: {
         DIP("prefx");
         break;
      }
      case 0x20:  { /* MADD.S */
         DIP("madd.s f%d, f%d, f%d, f%d", fmt, ft, fs, fd);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F32);
         assign(t1, triop(Iop_MulF32, rm, getLoFromF64(tyF, getFReg(fs)),
                          getLoFromF64(tyF, getFReg(ft))));

         putFReg(fd, mkWidenFromF32(tyF, triop(Iop_AddF32, rm, mkexpr(t1),
                                    getLoFromF64(tyF, getFReg(fmt)))));
         break;   /* MADD.S */
      }
      case 0x21: { /* MADD.D */
         DIP("madd.d f%d, f%d, f%d, f%d", fmt, ft, fs, fd);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F64);
         assign(t1, triop(Iop_MulF64, rm, getDReg(fs), getDReg(ft)));

         putDReg(fd, triop(Iop_AddF64, rm, mkexpr(t1), getDReg(fmt)));
         break;   /* MADD.D */
      }
      case 0x28: { /* MSUB.S */
         DIP("msub.s f%d, f%d, f%d, f%d", fmt, ft, fs, fd);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F32);
         assign(t1, triop(Iop_MulF32, rm, getLoFromF64(tyF, getFReg(fs)),
                          getLoFromF64(tyF, getFReg(ft))));

         putFReg(fd, mkWidenFromF32(tyF, triop(Iop_SubF32, rm,
                     mkexpr(t1), getLoFromF64(tyF, getFReg(fmt)))));
         break;   /* MSUB.S */
      }
      case 0x29: { /* MSUB.D */
         DIP("msub.d f%d, f%d, f%d, f%d", fmt, ft, fs, fd);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F64);
         assign(t1, triop(Iop_MulF64, rm, getDReg(fs), getDReg(ft)));

         putDReg(fd, triop(Iop_SubF64, rm, mkexpr(t1), getDReg(fmt)));
         break;   /* MSUB.D */
      }
      case 0x30: { /* NMADD.S */
         DIP("nmadd.s f%d, f%d, f%d, f%d", fmt, ft, fs, fd);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F32);
         t2 = newTemp(Ity_F32);
         assign(t1, triop(Iop_MulF32, rm, getLoFromF64(tyF, getFReg(fs)),
                getLoFromF64(tyF, getFReg(ft))));

         assign(t2, triop(Iop_AddF32, rm, mkexpr(t1),
                          getLoFromF64(tyF, getFReg(fmt))));

         putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32, mkexpr(t2))));
         break;   /* NMADD.S */
      }
      case 0x31: { /* NMADD.D */
         DIP("nmadd.d f%d, f%d, f%d, f%d", fmt, ft, fs, fd);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F64);
         t2 = newTemp(Ity_F64);
         assign(t1, triop(Iop_MulF64, rm, getDReg(fs), getDReg(ft)));

         assign(t2, triop(Iop_AddF64, rm, mkexpr(t1), getDReg(fmt)));
         putDReg(fd, unop(Iop_NegF64, mkexpr(t2)));
         break;   /* NMADD.D */
      }
      case 0x38: { /* NMSUBB.S */
         DIP("nmsub.s f%d, f%d, f%d, f%d", fmt, ft, fs, fd);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F32);
         t2 = newTemp(Ity_F32);
         assign(t1, triop(Iop_MulF32, rm, getLoFromF64(tyF, getFReg(fs)),
                          getLoFromF64(tyF, getFReg(ft))));

         assign(t2, triop(Iop_SubF32, rm, mkexpr(t1), getLoFromF64(tyF,
                                                      getFReg(fmt))));

         putFReg(fd, mkWidenFromF32(tyF, unop(Iop_NegF32, mkexpr(t2))));
         break;   /* NMSUBB.S */
      }
      case 0x39: { /* NMSUBB.D */
         DIP("nmsub.d f%d, f%d, f%d, f%d", fmt, ft, fs, fd);
         IRExpr *rm = get_IR_roundingmode();
         t1 = newTemp(Ity_F64);
         t2 = newTemp(Ity_F64);
         assign(t1, triop(Iop_MulF64, rm, getDReg(fs), getDReg(ft)));

         assign(t2, triop(Iop_SubF64, rm, mkexpr(t1), getDReg(fmt)));
         putDReg(fd, unop(Iop_NegF64, mkexpr(t2)));
         break;   /* NMSUBB.D */
      }

      default:
         goto decode_failure;
      }
      break;

   case 0x22:     /* LWL */

      DIP("lwl r%d, %d(r%d)", rt, imm, rs);
      {
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
         assign(t5, binop(Iop_And32, getIReg(rt), binop(Iop_Shr32,
                    mkU32(0xFFFFFFFF), narrowTo(Ity_I8, binop(Iop_Shl32,
                    binop(Iop_Add32, mkexpr(t4), mkU32(0x1)), mkU8(0x3))))));

         putIReg(rt, binop(Iop_Or32, mkexpr(t5), mkexpr(t3)));
      }
      break;

   case 0x26:     /* LWR */

      DIP("lwr r%d, %d(r%d)", rt, imm, rs);
      {
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

   case 0x2B:     /* SW */
      DIP("sw r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), mkNarrowTo32(ty, getIReg(rt)));
      break;

   case 0x28:     /* SB */
      DIP("sb r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), narrowTo(Ity_I8, getIReg(rt)));
      break;

   case 0x29:     /* SH */
      DIP("sh r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;
      store(mkexpr(t1), narrowTo(Ity_I16, getIReg(rt)));
      break;

   case 0x2A:     /* SWL */

      DIP("swl r%d, %d(r%d)", rt, imm, rs);
      {
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

         /* t3 = rt content - shifted */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_Shr32, getIReg(rt), narrowTo(Ity_I8,
                    binop(Iop_Shl32, binop(Iop_Sub32, mkU32(0x03), mkexpr(t4)),
                    mkU8(3)))));

         /* word content  - adjusted */
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);
         t7 = newTemp(Ity_I32);
         t8 = newTemp(Ity_I32);

         // neg(shr(0xFFFFFFFF, mul(sub(3,n), 8)))
         assign(t5, binop(Iop_Mul32, binop(Iop_Sub32, mkU32(0x3), mkexpr(t4)),
                          mkU32(0x8)));

         assign(t6, binop(Iop_Shr32, mkU32(0xFFFFFFFF), narrowTo(Ity_I8,
                                                        mkexpr(t5))));
         assign(t7, binop(Iop_Xor32, mkU32(0xFFFFFFFF), mkexpr(t6)));
         assign(t8, binop(Iop_And32, load(Ity_I32, mkexpr(t2)), mkexpr(t7)));
         store(mkexpr(t2), binop(Iop_Or32, mkexpr(t8), mkexpr(t3)));
      }
      break;

   case 0x2E:     /* SWR */

      DIP("swr r%d, %d(r%d)", rt, imm, rs);
      {
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

         /* t3 = rt content - shifted */
         t3 = newTemp(Ity_I32);
         assign(t3, binop(Iop_Shl32, getIReg(rt), narrowTo(Ity_I8,
                    binop(Iop_Shl32, mkexpr(t4), mkU8(3)))));

         /* word content  - adjusted */
         t5 = newTemp(Ity_I32);
         assign(t5, binop(Iop_And32, load(Ity_I32, mkexpr(t2)), unop(Iop_Not32,
                    binop(Iop_Shl32, mkU32(0xFFFFFFFF), narrowTo(Ity_I8,
                          binop(Iop_Shl32, mkexpr(t4), mkU8(0x3)))))));

         store(mkexpr(t2), binop(Iop_Xor32, mkexpr(t5), mkexpr(t3)));
      }
      break;

   case 0x1C:     /*Special2 */
      switch (function) {
      case 0x02: { /* MUL */
         DIP("mul r%d, r%d, r%d", rd, rs, rt);
         putIReg(rd, binop(Iop_Mul32, getIReg(rs), getIReg(rt)));
         break;
      }

      case 0x00: { /* MADD */
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

         putHI(binop(Iop_Add32, mkexpr(t6), unop(Iop_64HIto32, mkexpr(t3))));
         putLO(mkexpr(t4));
         break;
      }

      case 0x01: { /* MADDU */
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

         putHI(binop(Iop_Add32, mkexpr(t6), unop(Iop_64HIto32, mkexpr(t3))));
         putLO(mkexpr(t4));
         break;
      }

      case 0x04: { /* MSUB */
         DIP("msub r%d, r%d", rs, rt);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);

         assign(t1, getHI());
         assign(t2, getLO());

         assign(t3, binop(Iop_MullS32, getIReg(rs), getIReg(rt)));
         assign(t4, unop(Iop_64to32, mkexpr(t3))); //new lo

         //if lo<lo(mul) hi = hi - 1
         assign(t5, unop(Iop_1Sto32, binop(Iop_CmpLT32U, mkexpr(t2),
                                           mkexpr(t4))));

         assign(t6, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t5)), mkexpr(t1),
                                 binop(Iop_Sub32, mkexpr(t1), mkU32(0x1))));

         putHI(binop(Iop_Sub32, mkexpr(t6), unop(Iop_64HIto32, mkexpr(t3))));
         putLO(binop(Iop_Sub32, mkexpr(t2), mkexpr(t4)));
         break;
      }

      case 0x05: { /* MSUBU */
         DIP("msubu r%d, r%d", rs, rt);
         t1 = newTemp(Ity_I32);
         t2 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I64);
         t4 = newTemp(Ity_I32);
         t5 = newTemp(Ity_I32);
         t6 = newTemp(Ity_I32);

         assign(t1, getHI());
         assign(t2, getLO());

         assign(t3, binop(Iop_MullU32, getIReg(rs), getIReg(rt)));
         assign(t4, unop(Iop_64to32, mkexpr(t3))); //new lo

         //if lo<lo(mul) hi = hi - 1
         assign(t5, unop(Iop_1Sto32, binop(Iop_CmpLT32U, mkexpr(t2),
                                           mkexpr(t4))));

         assign(t6, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t5)),
                    mkexpr(t1), binop(Iop_Sub32, mkexpr(t1), mkU32(0x1))));

         putHI(binop(Iop_Sub32, mkexpr(t6), unop(Iop_64HIto32, mkexpr(t3))));
         putLO(binop(Iop_Sub32, mkexpr(t2), mkexpr(t4)));
         break;
      }

      case 0x20: { /* CLZ */
         DIP("clz r%d, r%d", rd, rs);
         t1 = newTemp(Ity_I32);
         assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, getIReg(rs),
                                           mkU32(0))));
         putIReg(rd, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                     unop(Iop_Clz32, getIReg(rs)), mkU32(0x00000020)));
         break;
      }

      case 0x21: { /* CLO */
         DIP("clo r%d, r%d", rd, rs);
         t1 = newTemp(Ity_I32);
         assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, getIReg(rs),
                                           mkU32(0xffffffff))));
         putIReg(rd, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                     unop(Iop_Clz32, unop(Iop_Not32, getIReg(rs))),
                     mkU32(0x00000020)));
         break;
      }

      default:
         goto decode_failure;
      }
      break;

   case 0x1F:     /*Special3 */
      switch (function) {
      case 0x3B:
          /*RDHWR*/ {
            DIP("rdhwr r%d, r%d", rt, rd);
            if (rd == 29) {
               putIReg(rt, getULR());
            } else
               goto decode_failure;
            break;
         }
      case 0x04:
          /*INS*/ msb = get_msb(cins);
         lsb = get_lsb(cins);

         size = msb - lsb + 1;

         vassert(lsb + size <= 32);
         vassert(lsb + size > 0);

         DIP("ins size:%d msb:%d lsb:%d", size, msb, lsb);
         /*put size bits from rs at the pos in temporary */
         t0 = newTemp(Ity_I32);
         t3 = newTemp(Ity_I32);
         /*shift left for 32 - size to clear leading bits and get zeros
           at the end */
         assign(t0, binop(Iop_Shl32, getIReg(rs), mkU8(32 - size)));
         /*now set it at pos */
         t1 = newTemp(Ity_I32);
         assign(t1, binop(Iop_Shr32, mkexpr(t0), mkU8(32 - size - lsb)));

         if (lsb > 0) {
            t2 = newTemp(Ity_I32);
            /*clear everything but lower pos bits from rt */
            assign(t2, binop(Iop_Shl32, getIReg(rt), mkU8(32 - lsb)));
            assign(t3, binop(Iop_Shr32, mkexpr(t2), mkU8(32 - lsb)));
         }

         if (msb < 31) {
            t4 = newTemp(Ity_I32);
            /*clear everything but upper msb + 1 bits from rt */
            assign(t4, binop(Iop_Shr32, getIReg(rt), mkU8(msb + 1)));
            t5 = newTemp(Ity_I32);
            assign(t5, binop(Iop_Shl32, mkexpr(t4), mkU8(msb + 1)));

            /*now combine these registers */
            if (lsb > 0) {
               t6 = newTemp(Ity_I32);
               assign(t6, binop(Iop_Or32, mkexpr(t5), mkexpr(t1)));
               putIReg(rt, binop(Iop_Or32, mkexpr(t6), mkexpr(t3)));
            } else {
               putIReg(rt, binop(Iop_Or32, mkexpr(t1), mkexpr(t5)));
            }
         }

         else {
            putIReg(rt, binop(Iop_Or32, mkexpr(t1), mkexpr(t3)));

         }
         break;

      case 0x00:
         /*EXT*/ msb = get_msb(cins);
         lsb = get_lsb(cins);
         size = msb + 1;
         DIP("ext size:%d msb:%d lsb:%d", size, msb, lsb);
         vassert(lsb + size <= 32);
         vassert(lsb + size > 0);
         /*put size bits from rs at the top of in temporary */
         if (lsb + size < 32) {
            t0 = newTemp(Ity_I32);
            assign(t0, binop(Iop_Shl32, getIReg(rs), mkU8(32 - lsb - size)));
            putIReg(rt, binop(Iop_Shr32, mkexpr(t0), mkU8(32 - size)));
         } else {
            putIReg(rt, binop(Iop_Shr32, getIReg(rs), mkU8(32 - size)));

         }
         break;

      case 0x20:
         /*BSHFL*/ switch (sa) {
         case 0x10:
             /*SEB*/ DIP("seb r%d, r%d", rd, rt);
            putIReg(rd, unop(Iop_8Sto32, unop(Iop_32to8, getIReg(rt))));
            break;

         case 0x18:
             /*SEH*/ DIP("seh r%d, r%d", rd, rt);
            putIReg(rd, unop(Iop_16Sto32, unop(Iop_32to16, getIReg(rt))));
            break;

         case 0x02:
             /*WSBH*/ DIP("wsbh r%d, r%d", rd, rt);
            t0 = newTemp(Ity_I32);
            t1 = newTemp(Ity_I32);
            t2 = newTemp(Ity_I32);
            t3 = newTemp(Ity_I32);
            assign(t0, binop(Iop_Shl32, binop(Iop_And32, getIReg(rt),
                                        mkU32(0x00FF0000)), mkU8(0x8)));
            assign(t1, binop(Iop_Shr32, binop(Iop_And32, getIReg(rt),
                       mkU32(0xFF000000)), mkU8(0x8)));
            assign(t2, binop(Iop_Shl32, binop(Iop_And32, getIReg(rt),
                       mkU32(0x000000FF)), mkU8(0x8)));
            assign(t3, binop(Iop_Shr32, binop(Iop_And32, getIReg(rt),
                       mkU32(0x0000FF00)), mkU8(0x8)));
            putIReg(rd, binop(Iop_Or32, binop(Iop_Or32, mkexpr(t0),
                        mkexpr(t1)), binop(Iop_Or32, mkexpr(t2), mkexpr(t3))));
            break;

         default:
            goto decode_failure;

         }
         break;
       /*BSHFL*/ default:
         goto decode_failure;

      }
      break;      /*Special3 */

   case 0x3B:
      if (0x3B == function && (archinfo->hwcaps & VEX_PRID_COMP_BROADCOM)) {
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

   case 0x00:     /*Special */

      switch (function) {
      case 0x1: {
         UInt mov_cc = get_mov_cc(cins);
         if (tf == 0) { /* MOVF */
            DIP("movf r%d, r%d, %d", rd, rs, mov_cc);
            {
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               t4 = newTemp(Ity_I32);

               assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                                 mkU32(mov_cc))));
               assign(t2, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                          binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                          mkU8(24 + mov_cc)), mkU32(0x1)), binop(Iop_And32,
                          binop(Iop_Shr32, getFCSR(), mkU8(23)),
                          mkU32(0x1))));

               assign(t3, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                                 mkexpr(t2))));
               assign(t4, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                          getIReg(rd), getIReg(rs)));
               putIReg(rd, mkexpr(t4));
            }
         } else if (tf == 1) {   /* MOVT */
            DIP("movt r%d, r%d, %d", rd, rs, mov_cc);
            {
               t1 = newTemp(Ity_I32);
               t2 = newTemp(Ity_I32);
               t3 = newTemp(Ity_I32);
               t4 = newTemp(Ity_I32);

               assign(t1, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(0),
                                                 mkU32(mov_cc))));
               assign(t2, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t1)),
                          binop(Iop_And32, binop(Iop_Shr32, getFCSR(),
                          mkU8(24 + mov_cc)), mkU32(0x1)), binop(Iop_And32,
                          binop(Iop_Shr32, getFCSR(), mkU8(23)),
                          mkU32(0x1))));

               assign(t3, unop(Iop_1Sto32, binop(Iop_CmpEQ32, mkU32(1),
                                                 mkexpr(t2))));
               assign(t4, IRExpr_Mux0X(unop(Iop_32to8, mkexpr(t3)),
                          getIReg(rd), getIReg(rs)));
               putIReg(rd, mkexpr(t4));
            }
         }
         break;
      }
      case 0x0A: {
         /* MOVZ */
         DIP("movz r%d, r%d, r%d", rd, rs, rt);
         t1 = newTemp(ty);
         t2 = newTemp(ty);
         {
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

      case 0x0B: {
         /* MOVN */
         DIP("movn r%d, r%d, r%d", rd, rs, rt);
         t1 = newTemp(ty);
         t2 = newTemp(ty);
         {
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

      case 0x18:  /* MULT */
         DIP("mult r%d, r%d", rs, rt);
         t2 = newTemp(Ity_I64);

         assign(t2, binop(Iop_MullS32, mkNarrowTo32(ty, getIReg(rs)),
                          mkNarrowTo32(ty, getIReg(rt))));

         putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
         putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
         break;

      case 0x19:  /* MULTU */
         DIP("multu r%d, r%d", rs, rt);
         t2 = newTemp(Ity_I64);

         assign(t2, binop(Iop_MullU32, mkNarrowTo32(ty, getIReg(rs)),
                                       mkNarrowTo32(ty, getIReg(rt))));

         putHI(mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(t2)), True));
         putLO(mkWidenFrom32(ty, unop(Iop_64to32, mkexpr(t2)), True));
         break;

      case 0x20:  /* ADD */
         DIP("add r%d, r%d, r%d", rd, rs, rt);
         {
            t2 = newTemp(Ity_I32);

            assign(t2, binop(Iop_Add32, getIReg(rs), getIReg(rt)));
            putIReg(rd, mkexpr(t2));
         }
         break;

      case 0x1A:  /* DIV */
         DIP("div r%d, r%d", rs, rt);
         {
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
         {
            t1 = newTemp(Ity_I64);
            t2 = newTemp(Ity_I64);
            assign(t1, unop(Iop_32Uto64, getIReg(rs)));
            assign(t2, binop(Iop_DivModU64to32, mkexpr(t1), getIReg(rt)));
            putHI(unop(Iop_64HIto32, mkexpr(t2)));
            putLO(unop(Iop_64to32, mkexpr(t2)));
         }
         break;

      case 0x10:  /* MFHI */
         DIP("mfhi r%d", rd);
         putIReg(rd, getHI());
         break;

      case 0x11:  /* MTHI */
         DIP("mthi r%d", rs);
         putHI(getIReg(rs));
         break;

      case 0x12:  /* MFLO */
         DIP("mflo r%d", rd);
         putIReg(rd, getLO());
         break;

      case 0x13:  /* MTLO */
         DIP("mtlo r%d", rs);
         putLO(getIReg(rs));
         break;

      case 0x21:  /* ADDU */
         DIP("addu r%d, r%d, r%d", rd, rs, rt);
         ALU_PATTERN(Iop_Add32);
         break;

      case 0x22:  /* SUB */
         DIP("sub r%d, r%d, r%d", rd, rs, rt);
         ALU_PATTERN(Iop_Sub32);
         break;

      case 0x23:  /* SUBU */
         DIP("subu r%d, r%d, r%d", rd, rs, rt);
         ALU_PATTERN(Iop_Sub32);
         break;

      case 0x24:  /* AND */
         DIP("and r%d, r%d, r%d", rd, rs, rt);
         ALU_PATTERN(Iop_And32);
         break;

      case 0x25:  /* OR */
         DIP("or r%d, r%d, r%d", rd, rs, rt);
         ALU_PATTERN(Iop_Or32);
         break;

      case 0x26:  /* XOR */
         DIP("xor r%d, r%d, r%d", rd, rs, rt);
         ALU_PATTERN(Iop_Xor32);
         break;

      case 0x27:  /* NOR */
         DIP("nor r%d, r%d, r%d", rd, rs, rt);
         putIReg(rd, unop(Iop_Not32, binop(Iop_Or32, getIReg(rs),getIReg(rt))));
         break;

      case 0x08:  /* JR */
         DIP("jr r%d", rs);
         t0 = newTemp(ty);
         assign(t0, getIReg(rs));
         lastn = mkexpr(t0);
         break;

      case 0x09:  /* JALR */
         DIP("jalr r%d r%d", rd, rs);
         putIReg(rd, mkU32(guest_PC_curr_instr + 8));
         t0 = newTemp(Ity_I32);
         assign(t0, getIReg(rs));
         lastn = mkexpr(t0);
         break;

      case 0x0C:  /* SYSCALL */
         DIP("syscall");
         putPC(mkU32(guest_PC_curr_instr + 4));
         dres.jk_StopHere = Ijk_Sys_syscall;
         dres.whatNext    = Dis_StopHere;
         break;

      case 0x2A:  /* SLT */
         DIP("slt r%d, r%d, r%d", rd, rs, rt);
         putIReg(rd, unop(Iop_1Uto32, binop(Iop_CmpLT32S, getIReg(rs),
                                      getIReg(rt))));
         break;

      case 0x2B:  /* SLTU */
         DIP("sltu r%d, r%d, r%d", rd, rs, rt);
         putIReg(rd, unop(Iop_1Uto32, binop(Iop_CmpLT32U, getIReg(rs),
                                      getIReg(rt))));
         break;

      case 0x00:
         /* SLL */
         DIP("sll r%d, r%d, %d", rd, rt, sa);
         SXX_PATTERN(Iop_Shl32);
         break;

      case 0x04:  /* SLLV */
         DIP("sllv r%d, r%d, r%d", rd, rt, rs);
         SXXV_PATTERN(Iop_Shl32);
         break;

      case 0x03:  /* SRA */
         DIP("sra r%d, r%d, %d", rd, rt, sa);
         SXX_PATTERN(Iop_Sar32);
         break;

      case 0x07:  /* SRAV */
         DIP("srav r%d, r%d, r%d", rd, rt, rs);
         SXXV_PATTERN(Iop_Sar32);
         break;

      case 0x02: {  /* SRL */
         rot = get_rot(cins);
         if (rot) {
            DIP("rotr r%d, r%d, %d", rd, rt, sa);
            putIReg(rd, mkWidenFrom32(ty, genROR32(mkNarrowTo32(ty,
                        getIReg(rt)), sa), False));
         } else {
            DIP("srl r%d, r%d, %d", rd, rt, sa);
            SXX_PATTERN(Iop_Shr32);
         }
      break;
      }

      case 0x06: {
         rot = get_rotv(cins);
         if (rot) {
            DIP("rotrv r%d, r%d, r%d", rd, rt, rs);
            putIReg(rd, mkWidenFrom32(ty, genRORV32(mkNarrowTo32(ty,
                        getIReg(rt)), mkNarrowTo32(ty, getIReg(rs))),False));
            break;
         } else {
            /* SRLV */
            DIP("srlv r%d, r%d, r%d", rd, rt, rs);
            SXXV_PATTERN(Iop_Shr32);
            break;
         }
      } 

      case 0x0D:  /* BREAK */
         DIP("Info: Breakpoint...code = %d", trap_code);
         jmp_lit(&dres, Ijk_SigTRAP, (guest_PC_curr_instr + 4));
         vassert(dres.whatNext == Dis_StopHere);
         break;

      case 0x30: { /* TGE */
         /*tge */ DIP("tge r%d, r%d %d", rs, rt, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpLT32S, getIReg (rt), getIReg (rs)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x31: { /* TGEU */
         /*tgeu */ DIP("tgeu r%d, r%d %d", rs, rt, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpLT32U, getIReg (rt), getIReg (rs)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x32: { /* TLT */
         /*tlt */ DIP("tlt r%d, r%d %d", rs, rt, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpLT32S, getIReg (rs), getIReg (rt)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x33: { /* TLTU */
         /*tltu */ DIP("tltu r%d, r%d %d", rs, rt, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpLT32U, getIReg (rs), getIReg (rt)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x34: { /* TEQ */
         /*teq */ DIP("teq r%d, r%d %d", rs, rt, trap_code);
         stmt (IRStmt_Exit(binop (Iop_CmpEQ32, getIReg (rs), getIReg (rt)),
               Ijk_SigTRAP, IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x36: { /* TNE */
         /*tne */ DIP("tne r%d, r%d %d", rs, rt, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpNE32, getIReg (rs), getIReg (rt)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x0F: {
         /*SYNC*/ DIP("sync r%d, r%d, %d", rt, rd, sel);
         lsb = get_lsb(cins);
         IRDirty *d = unsafeIRDirty_0_N(0,
                                        "mips32_dirtyhelper_sync",
                                        &mips32_dirtyhelper_sync,
                                        mkIRExprVec_1
                                        (mkU32(lsb)));

         d->needsBBP = False;
         d->nFxState = 0;

         stmt(IRStmt_Dirty(d));
         break;
      }

      default:
         goto decode_failure;
      }
      break;

   case 0x01:     /* Regimm */

      switch (rt) {
      case 0x01:  /* BGEZ */
         DIP("bgez r%d, %d", rs, imm);
         dis_branch(False, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                           mkU32(0x80000000)), mkU32(0x0)), imm, &bstmt);
         break;

      case 0x03:  /* BGEZL */
         DIP("bgezl r%d, %d", rs, imm);
         lastn = dis_branch_likely(binop(Iop_CmpNE32, binop(Iop_And32,
                                   getIReg(rs), mode64 ?
                                      mkU64(0x8000000000000000ULL)
                                      :mkU32(0x80000000)),
                                   mkU32(0x0)), imm);
         break;

      case 0x00:  /* BLTZ */
         DIP("bltz r%d, %d", rs, imm);
         dis_branch(False, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                    mkU32(0x80000000)), mkU32(0x80000000)), imm, &bstmt);
         break;

      case 0x02:  /* BLTZL */
         DIP("bltzl r%d, %d", rs, imm);
         lastn = dis_branch_likely(binop(Iop_CmpNE32, binop(Iop_And32,
                                   getIReg(rs), mkU32(0x80000000)),
                                   mkU32(0x80000000)), imm);
         break;

      case 0x10:  /* BLTZAL */
         DIP("bltzal r%d, %d", rs, imm);
         dis_branch(True, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                    mkU32(0x80000000)), mkU32(0x80000000)), imm, &bstmt);
         break;

      case 0x12:  /* BLTZALL */
         DIP("bltzall r%d, %d", rs, imm);
         putIReg(31, mkU32(guest_PC_curr_instr + 8));
         lastn = dis_branch_likely(binop(Iop_CmpNE32, binop(Iop_And32,
                                   getIReg(rs), mkU32(0x80000000)),
                                                mkU32(0x80000000)), imm);
         break;

      case 0x11:  /* BGEZAL */
         DIP("bgezal r%d, %d", rs, imm);
         dis_branch(True, binop(Iop_CmpEQ32, binop(Iop_And32, getIReg(rs),
                    mkU32(0x80000000)), mkU32(0x0)), imm, &bstmt);
         break;

      case 0x13:  /* BGEZALL */
         DIP("bgezall r%d, %d", rs, imm);
         putIReg(31, mkU32(guest_PC_curr_instr + 8));
         lastn = dis_branch_likely(binop(Iop_CmpNE32, binop(Iop_And32,
                                   getIReg(rs), mkU32(0x80000000)),
                                   mkU32(0x0)), imm);
         break;

      case 0x08: { /* TGEI */
         /*tgei */ DIP("tgei r%d, %d %d", rs, imm, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpLT32S, mkU32 (imm), getIReg (rs)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x09: { /* TGEIU */
         /*tqeiu */ DIP("tgeiu r%d, %d %d", rs, imm, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpLT32U, mkU32 (imm), getIReg (rs)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x0A: { /* TLTI */
         /*tlti */ DIP("tlti r%d, %d %d", rs, imm, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpLT32S, getIReg (rs), mkU32 (imm)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x0B: { /* TLTIU */
         /*tltiu */ DIP("tltiu r%d, %d %d", rs, imm, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpLT32U, getIReg (rs), mkU32 (imm)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x0C: { /* TEQI */
         /*teqi */ DIP("teqi r%d, %d %d", rs, imm, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpEQ32, getIReg (rs), mkU32 (imm)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x0E: { /* TNEI */
         /*tnei */ DIP("tnei r%d, %d %d", rs, imm, trap_code);
         stmt (IRStmt_Exit (binop (Iop_CmpNE32, getIReg (rs), mkU32 (imm)),
                            Ijk_SigTRAP,
                            IRConst_U32 (guest_PC_curr_instr + 4), OFFB_PC));
         break;
      }
      case 0x1F:
          /*SYNCI*/
             //Just ignore it
             break;

      default:
         goto decode_failure;
      }
      break;

   case 0x04:
      DIP("beq r%d, r%d, %d", rs, rt, imm);
      dis_branch(False, binop(Iop_CmpEQ32, getIReg(rs), getIReg(rt)),
                              imm, &bstmt);
      break;

   case 0x14:
      DIP("beql r%d, r%d, %d", rs, rt, imm);
      lastn = dis_branch_likely(binop(Iop_CmpNE32, getIReg(rs), getIReg(rt)),
                                      imm);
      break;

   case 0x05:
      DIP("bne r%d, r%d, %d", rs, rt, imm);
      dis_branch(False, binop(Iop_CmpNE32, getIReg(rs), getIReg(rt)),
                              imm, &bstmt);
      break;

   case 0x15:
      DIP("bnel r%d, r%d, %d", rs, rt, imm);
      lastn =
          dis_branch_likely(binop(Iop_CmpEQ32, getIReg(rs), getIReg(rt)), imm);
      break;

   case 0x07:     /* BGTZ */
      DIP("bgtz r%d, %d", rs, imm);
      dis_branch(False, unop(Iop_Not1, binop(Iop_CmpLE32S, getIReg(rs),
                             mkU32(0x00))), imm, &bstmt);
      break;

   case 0x17:     /* BGTZL */
      DIP("bgtzl r%d, %d", rs, imm);
      lastn = dis_branch_likely(binop(Iop_CmpLE32S, getIReg(rs), mkU32(0x00)),
                                      imm);
      break;

   case 0x06:     /* BLEZ */
      DIP("blez r%d, %d", rs, imm);
      dis_branch(False,binop(Iop_CmpLE32S, getIReg(rs), mkU32(0x0)), imm,
                             &bstmt);
      break;

   case 0x16:     /* BLEZL */
      DIP("blezl r%d, %d", rs, imm);
      lastn = dis_branch_likely(unop(Iop_Not1, (binop(Iop_CmpLE32S,
                                     getIReg(rs), mkU32(0x0)))), imm);
      break;

   case 0x08:     /* ADDI TODO: Check this */
      DIP("addi r%d, r%d, %d", rt, rs, imm);
      putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));
      break;

   case 0x09:     /* ADDIU */
      DIP("addiu r%d, r%d, %d", rt, rs, imm);
      putIReg(rt, binop(Iop_Add32, getIReg(rs), mkU32(extend_s_16to32(imm))));
      break;

   case 0x0C:     /* ANDI */
      DIP("andi r%d, r%d, %d", rt, rs, imm);
      ALUI_PATTERN(Iop_And32);
      break;

   case 0x0E:     /* XORI */
      DIP("xori r%d, r%d, %d", rt, rs, imm);
      ALUI_PATTERN(Iop_Xor32);
      break;

   case 0x0D:     /* ORI */
      DIP("ori r%d, r%d, %d", rt, rs, imm);
      ALUI_PATTERN(Iop_Or32);
      break;

   case 0x0A:     /* SLTI */
      DIP("slti r%d, r%d, %d", rt, rs, imm);
      putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpLT32S, getIReg(rs),
                                         mkU32(extend_s_16to32(imm)))));
      break;

   case 0x0B:     /* SLTIU */
      DIP("sltiu r%d, r%d, %d", rt, rs, imm);
      putIReg(rt, unop(Iop_1Uto32, binop(Iop_CmpLT32U, getIReg(rs),
                                         mkU32(extend_s_16to32(imm)))));
      break;

   case 0x30:     /* LL / LWC0 */
      DIP("ll r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;

      t2 = newTemp(Ity_I32);
#if defined (_MIPSEL)
      stmt(IRStmt_LLSC(Iend_LE, t2, mkexpr(t1), NULL /*this is a load */ ));
#elif defined (_MIPSEB)
      stmt(IRStmt_LLSC(Iend_BE, t2, mkexpr(t1), NULL /*this is a load */ ));
#endif

      putIReg(rt, mkexpr(t2));
      break;

   case 0x38:     /* SC / SWC0 */
      DIP("sc r%d, %d(r%d)", rt, imm, rs);
      LOAD_STORE_PATTERN;

      t2 = newTemp(Ity_I1);
#if defined (_MIPSEL)
      stmt(IRStmt_LLSC(Iend_LE, t2, mkexpr(t1), mkNarrowTo32(ty, getIReg(rt))));
#elif defined (_MIPSEB)
      stmt(IRStmt_LLSC(Iend_BE, t2, mkexpr(t1), mkNarrowTo32(ty, getIReg(rt))));
#endif

      putIReg(rt, unop(Iop_1Uto32, mkexpr(t2)));
      break;

 decode_failure:
      /* All decode failures end up here. */
      DIP("vex mips->IR: unhandled instruction bytes: "
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
      stmt(IRStmt_Put(offsetof(VexGuestMIPS32State, guest_PC),
           mkU32(guest_PC_curr_instr)));
      jmp_lit(&dres, Ijk_NoDecode, guest_PC_curr_instr);
      dres.whatNext = Dis_StopHere;
      dres.len = 0;
      return dres;
   }        /* switch (opc) for the main (primary) opcode switch. */

   /* All MIPS insn have 4 bytes */

   if (delay_slot_branch) {
      delay_slot_branch = False;
      stmt(bstmt);
      bstmt = NULL;
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

   // On MIPS we need to check if the last instruction
   // in block is branch or jump
   if ((vex_control.guest_max_insns - 1) == (delta+4)/4)
      if (branch_or_jump(guest_code + delta + 4)) {
         dres.whatNext = Dis_StopHere;
         dres.jk_StopHere = Ijk_Boring;
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

DisResult
disInstr_MIPS(IRSB*        irsb_IN,
              Bool         (*resteerOkFn) (void *, Addr64),
              Bool         resteerCisOk,
              void*        callback_opaque,
              UChar*       guest_code_IN,
              Long         delta,
              Addr64       guest_IP,
              VexArch      guest_arch,
              VexArchInfo* archinfo,
              VexAbiInfo*  abiinfo,
              Bool         host_bigendian_IN)
{
   DisResult dres;

   /* Set globals (see top of this file) */
   vassert(guest_arch == VexArchMIPS32);

   mode64 = guest_arch != VexArchMIPS32;

   guest_code = guest_code_IN;
   irsb = irsb_IN;
   host_is_bigendian = host_bigendian_IN;
   guest_PC_curr_instr = (Addr32) guest_IP;
   guest_PC_bbstart = (Addr32) toUInt(guest_IP - delta);

   dres = disInstr_MIPS_WRK(resteerOkFn, resteerCisOk, callback_opaque,
                            delta, archinfo, abiinfo);

   return dres;
}

/*--------------------------------------------------------------------*/
/*--- end                                        guest_mips_toIR.c ---*/
/*--------------------------------------------------------------------*/
