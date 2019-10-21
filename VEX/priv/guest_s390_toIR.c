/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                 guest_s390_toIR.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2017

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

/* Contributed by Florian Krohm and Christian Borntraeger */

/* Translates s390 code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex_emnote.h"
#include "libvex_s390x_common.h"
#include "main_util.h"               /* vassert */
#include "main_globals.h"            /* vex_traceflags */
#include "guest_generic_bb_to_IR.h"  /* DisResult */
#include "guest_s390_defs.h"         /* prototypes for this file's functions */
#include "s390_disasm.h"
#include "s390_defs.h"               /* S390_BFP_ROUND_xyzzy */
#include "host_s390_defs.h"          /* s390_host_has_xyzzy */


/*------------------------------------------------------------*/
/*--- Forward declarations                                 ---*/
/*------------------------------------------------------------*/
static UInt s390_decode_and_irgen(const UChar *, UInt, DisResult *);
static void s390_irgen_xonc(IROp, IRTemp, IRTemp, IRTemp);
static void s390_irgen_CLC_EX(IRTemp, IRTemp, IRTemp);
static const HChar *s390_irgen_BIC(UChar r1, IRTemp op2addr);

/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* The IRSB* into which we're generating code. */
static IRSB *irsb;

/* The guest address for the instruction currently being
   translated. */
static Addr64 guest_IA_curr_instr;

/* The guest address for the instruction following the current instruction. */
static Addr64 guest_IA_next_instr;

/* Result of disassembly step. */
static DisResult *dis_res;

/* Whether to print diagnostics for illegal instructions. */
static Bool sigill_diag;

/* The last seen execute target instruction */
ULong last_execute_target;

/* The possible outcomes of a decoding operation */
typedef enum {
   S390_DECODE_OK,
   S390_DECODE_UNKNOWN_INSN,
   S390_DECODE_UNIMPLEMENTED_INSN,
   S390_DECODE_UNKNOWN_SPECIAL_INSN,
   S390_DECODE_SPECIFICATION_EXCEPTION,
   S390_DECODE_ERROR
} s390_decode_t;


/*------------------------------------------------------------*/
/*--- Instruction formats.                                 ---*/
/*------------------------------------------------------------*/

#define I_i(insn) ((insn) & 0xff)
#define RR_r1(insn) (((insn) >> 4) & 0xf)
#define RR_r2(insn) ((insn) & 0xf)
#define RI_r1(insn) (((insn) >> 20) & 0xf)
#define RI_i2(insn) ((insn) & 0xffff)
#define RRE_r1(insn) (((insn) >> 4) & 0xf)
#define RRE_r2(insn) ((insn) & 0xf)
#define RRF_r1(insn) (((insn) >> 12) & 0xf)
#define RRF_r3(insn) (((insn) >> 4) & 0xf)
#define RRF_r2(insn) ((insn) & 0xf)
#define RRF2_m3(insn) (((insn) >> 12) & 0xf)
#define RRF2_m4(insn) (((insn) >> 8) & 0xf)
#define RRF2_r1(insn) (((insn) >> 4) & 0xf)
#define RRF2_r2(insn) ((insn) & 0xf)
#define RRF3_r3(insn) (((insn) >> 12) & 0xf)
#define RRF3_r1(insn) (((insn) >> 4) & 0xf)
#define RRF3_r2(insn) ((insn) & 0xf)
#define RRF4_r3(insn) (((insn) >> 12) & 0xf)
#define RRF4_m4(insn) (((insn) >> 8) & 0xf)
#define RRF4_r1(insn) (((insn) >> 4) & 0xf)
#define RRF4_r2(insn) ((insn) & 0xf)
#define RRF5_m4(insn) (((insn) >> 8) & 0xf)
#define RRF5_r1(insn) (((insn) >> 4) & 0xf)
#define RRF5_r2(insn) ((insn) & 0xf)
#define RS_r1(insn) (((insn) >> 20) & 0xf)
#define RS_r3(insn) (((insn) >> 16) & 0xf)
#define RS_b2(insn) (((insn) >> 12) & 0xf)
#define RS_d2(insn) ((insn) & 0xfff)
#define RSI_r1(insn) (((insn) >> 20) & 0xf)
#define RSI_r3(insn) (((insn) >> 16) & 0xf)
#define RSI_i2(insn) ((insn) & 0xffff)
#define RX_r1(insn) (((insn) >> 20) & 0xf)
#define RX_x2(insn) (((insn) >> 16) & 0xf)
#define RX_b2(insn) (((insn) >> 12) & 0xf)
#define RX_d2(insn) ((insn) & 0xfff)
#define S_b2(insn) (((insn) >> 12) & 0xf)
#define S_d2(insn) ((insn) & 0xfff)
#define SI_i2(insn) (((insn) >> 16) & 0xff)
#define SI_b1(insn) (((insn) >> 12) & 0xf)
#define SI_d1(insn) ((insn) & 0xfff)
#define RIE_r1(insn) (((insn) >> 52) & 0xf)
#define RIE_r3(insn) (((insn) >> 48) & 0xf)
#define RIE_i2(insn) (((insn) >> 32) & 0xffff)
#define RIE_RRUUU_r1(insn) (((insn) >> 52) & 0xf)
#define RIE_RRUUU_r2(insn) (((insn) >> 48) & 0xf)
#define RIE_RRUUU_i3(insn) (((insn) >> 40) & 0xff)
#define RIE_RRUUU_i4(insn) (((insn) >> 32) & 0xff)
#define RIE_RRUUU_i5(insn) (((insn) >> 24) & 0xff)
#define RIEv1_r1(insn) (((insn) >> 52) & 0xf)
#define RIEv1_i2(insn) (((insn) >> 32) & 0xffff)
#define RIEv1_m3(insn) (((insn) >> 28) & 0xf)
#define RIE_RRPU_r1(insn) (((insn) >> 52) & 0xf)
#define RIE_RRPU_r2(insn) (((insn) >> 48) & 0xf)
#define RIE_RRPU_i4(insn) (((insn) >> 32) & 0xffff)
#define RIE_RRPU_m3(insn) (((insn) >> 28) & 0xf)
#define RIEv3_r1(insn) (((insn) >> 52) & 0xf)
#define RIEv3_m3(insn) (((insn) >> 48) & 0xf)
#define RIEv3_i4(insn) (((insn) >> 32) & 0xffff)
#define RIEv3_i2(insn) (((insn) >> 24) & 0xff)
#define RIL_r1(insn) (((insn) >> 52) & 0xf)
#define RIL_i2(insn) (((insn) >> 16) & 0xffffffff)
#define RIS_r1(insn) (((insn) >> 52) & 0xf)
#define RIS_m3(insn) (((insn) >> 48) & 0xf)
#define RIS_b4(insn) (((insn) >> 44) & 0xf)
#define RIS_d4(insn) (((insn) >> 32) & 0xfff)
#define RIS_i2(insn) (((insn) >> 24) & 0xff)
#define RRS_r1(insn) (((insn) >> 52) & 0xf)
#define RRS_r2(insn) (((insn) >> 48) & 0xf)
#define RRS_b4(insn) (((insn) >> 44) & 0xf)
#define RRS_d4(insn) (((insn) >> 32) & 0xfff)
#define RRS_m3(insn) (((insn) >> 28) & 0xf)
#define RSY_r1(insn) (((insn) >> 52) & 0xf)
#define RSY_r3(insn) (((insn) >> 48) & 0xf)
#define RSY_b2(insn) (((insn) >> 44) & 0xf)
#define RSY_dl2(insn) (((insn) >> 32) & 0xfff)
#define RSY_dh2(insn) (((insn) >> 24) & 0xff)
#define RXE_r1(insn) (((insn) >> 52) & 0xf)
#define RXE_x2(insn) (((insn) >> 48) & 0xf)
#define RXE_b2(insn) (((insn) >> 44) & 0xf)
#define RXE_d2(insn) (((insn) >> 32) & 0xfff)
#define RXE_m3(insn) (((insn) >> 28) & 0xf)
#define RXF_r3(insn) (((insn) >> 52) & 0xf)
#define RXF_x2(insn) (((insn) >> 48) & 0xf)
#define RXF_b2(insn) (((insn) >> 44) & 0xf)
#define RXF_d2(insn) (((insn) >> 32) & 0xfff)
#define RXF_r1(insn) (((insn) >> 28) & 0xf)
#define RXY_r1(insn) (((insn) >> 52) & 0xf)
#define RXY_x2(insn) (((insn) >> 48) & 0xf)
#define RXY_b2(insn) (((insn) >> 44) & 0xf)
#define RXY_dl2(insn) (((insn) >> 32) & 0xfff)
#define RXY_dh2(insn) (((insn) >> 24) & 0xff)
#define SIY_i2(insn) (((insn) >> 48) & 0xff)
#define SIY_b1(insn) (((insn) >> 44) & 0xf)
#define SIY_dl1(insn) (((insn) >> 32) & 0xfff)
#define SIY_dh1(insn) (((insn) >> 24) & 0xff)
#define SS_l(insn) (((insn) >> 48) & 0xff)
#define SS_b1(insn) (((insn) >> 44) & 0xf)
#define SS_d1(insn) (((insn) >> 32) & 0xfff)
#define SS_b2(insn) (((insn) >> 28) & 0xf)
#define SS_d2(insn) (((insn) >> 16) & 0xfff)
#define SIL_b1(insn) (((insn) >> 44) & 0xf)
#define SIL_d1(insn) (((insn) >> 32) & 0xfff)
#define SIL_i2(insn) (((insn) >> 16) & 0xffff)
#define VRX_v1(insn) (((insn) >> 52) & 0xf)
#define VRX_x2(insn) (((insn) >> 48) & 0xf)
#define VRX_b2(insn) (((insn) >> 44) & 0xf)
#define VRX_d2(insn) (((insn) >> 32) & 0xfff)
#define VRX_m3(insn) (((insn) >> 28) & 0xf)
#define VRX_rxb(insn) (((insn) >> 24) & 0xf)
#define VRR_v1(insn) (((insn) >> 52) & 0xf)
#define VRR_v2(insn) (((insn) >> 48) & 0xf)
#define VRR_r3(insn) (((insn) >> 44) & 0xf)
#define VRR_m5(insn) (((insn) >> 36) & 0xf)
#define VRR_m4(insn) (((insn) >> 28) & 0xf)
#define VRR_rxb(insn) (((insn) >> 24) & 0xf)
#define VRRa_v1(insn) (((insn) >> 52) & 0xf)
#define VRRa_v2(insn) (((insn) >> 48) & 0xf)
#define VRRa_v3(insn) (((insn) >> 44) & 0xf)
#define VRRa_m5(insn) (((insn) >> 36) & 0xf)
#define VRRa_m4(insn) (((insn) >> 32) & 0xf)
#define VRRa_m3(insn) (((insn) >> 28) & 0xf)
#define VRRa_rxb(insn) (((insn) >> 24) & 0xf)
#define VRRd_v1(insn) (((insn) >> 52) & 0xf)
#define VRRd_v2(insn) (((insn) >> 48) & 0xf)
#define VRRd_v3(insn) (((insn) >> 44) & 0xf)
#define VRRd_m5(insn) (((insn) >> 40) & 0xf)
#define VRRd_m6(insn) (((insn) >> 36) & 0xf)
#define VRRd_v4(insn) (((insn) >> 28) & 0xf)
#define VRRd_rxb(insn) (((insn) >> 24) & 0xf)
#define VRRe_v1(insn) (((insn) >> 52) & 0xf)
#define VRRe_v2(insn) (((insn) >> 48) & 0xf)
#define VRRe_v3(insn) (((insn) >> 44) & 0xf)
#define VRRe_m6(insn) (((insn) >> 40) & 0xf)
#define VRRe_m5(insn) (((insn) >> 32) & 0xf)
#define VRRe_v4(insn) (((insn) >> 28) & 0xf)
#define VRRe_rxb(insn) (((insn) >> 24) & 0xf)
#define VRI_v1(insn) (((insn) >> 52) & 0xf)
#define VRI_v3(insn) (((insn) >> 48) & 0xf)
#define VRI_i2(insn) (((insn) >> 32) & 0xffff)
#define VRI_m3(insn) (((insn) >> 28) & 0xf)
#define VRI_rxb(insn) (((insn) >> 24) & 0xf)
#define VRId_v1(insn) (((insn) >> 52) & 0xf)
#define VRId_v2(insn) (((insn) >> 48) & 0xf)
#define VRId_v3(insn) (((insn) >> 44) & 0xf)
#define VRId_i4(insn) (((insn) >> 32) & 0xff)
#define VRId_m5(insn) (((insn) >> 28) & 0xf)
#define VRId_rxb(insn) (((insn) >> 24) & 0xf)
#define VRIe_v1(insn) (((insn) >> 52) & 0xf)
#define VRIe_v2(insn) (((insn) >> 48) & 0xf)
#define VRIe_i3(insn) (((insn) >> 36) & 0xfff)
#define VRIe_m5(insn) (((insn) >> 32) & 0xf)
#define VRIe_m4(insn) (((insn) >> 28) & 0xf)
#define VRIe_rxb(insn) (((insn) >> 24) & 0xf)
#define VRS_v1(insn) (((insn) >> 52) & 0xf)
#define VRS_v3(insn) (((insn) >> 48) & 0xf)
#define VRS_b2(insn) (((insn) >> 44) & 0xf)
#define VRS_d2(insn) (((insn) >> 32) & 0xfff)
#define VRS_m4(insn) (((insn) >> 28) & 0xf)
#define VRS_rxb(insn) (((insn) >> 24) & 0xf)


/*------------------------------------------------------------*/
/*--- Helpers for constructing IR.                         ---*/
/*------------------------------------------------------------*/

/* Add a statement to the current irsb. */
static __inline__ void
stmt(IRStmt *st)
{
   addStmtToIRSB(irsb, st);
}

/* Allocate a new temporary of the given type. */
static __inline__ IRTemp
newTemp(IRType type)
{
   vassert(isPlausibleIRType(type));

   return newIRTemp(irsb->tyenv, type);
}

/* Create an expression node for a temporary */
static __inline__ IRExpr *
mkexpr(IRTemp tmp)
{
   return IRExpr_RdTmp(tmp);
}

/* Generate an expression node for an address. */
static __inline__ IRExpr *
mkaddr_expr(Addr64 addr)
{
   return IRExpr_Const(IRConst_U64(addr));
}

/* Add a statement that assigns to a temporary */
static __inline__ void
assign(IRTemp dst, IRExpr *expr)
{
   stmt(IRStmt_WrTmp(dst, expr));
}

/* Write an address into the guest_IA */
static __inline__ void
put_IA(IRExpr *address)
{
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_IA), address));
}

/* Create a temporary of the given type and assign the expression to it */
static __inline__ IRTemp
mktemp(IRType type, IRExpr *expr)
{
   IRTemp temp = newTemp(type);

   assign(temp, expr);

   return temp;
}

/* Create a unary expression */
static __inline__ IRExpr *
unop(IROp kind, IRExpr *op)
{
   return IRExpr_Unop(kind, op);
}

/* Create a binary expression */
static __inline__ IRExpr *
binop(IROp kind, IRExpr *op1, IRExpr *op2)
{
   return IRExpr_Binop(kind, op1, op2);
}

/* Create a ternary expression */
static __inline__ IRExpr *
triop(IROp kind, IRExpr *op1, IRExpr *op2, IRExpr *op3)
{
   return IRExpr_Triop(kind, op1, op2, op3);
}

/* Create a quaternary expression */
static __inline__  IRExpr *
qop(IROp kind, IRExpr *op1, IRExpr *op2, IRExpr *op3, IRExpr *op4)
{
   return IRExpr_Qop(kind, op1, op2, op3, op4);
}

/* Create an expression node for an 8-bit integer constant */
static __inline__ IRExpr *
mkU8(UInt value)
{
   vassert(value < 256);

   return IRExpr_Const(IRConst_U8((UChar)value));
}

/* Create an expression node for a 16-bit integer constant */
static __inline__ IRExpr *
mkU16(UInt value)
{
   vassert(value < 65536);

   return IRExpr_Const(IRConst_U16((UShort)value));
}

/* Create an expression node for a 32-bit integer constant */
static __inline__ IRExpr *
mkU32(UInt value)
{
   return IRExpr_Const(IRConst_U32(value));
}

/* Create an expression node for a 64-bit integer constant */
static __inline__ IRExpr *
mkU64(ULong value)
{
   return IRExpr_Const(IRConst_U64(value));
}

/* Create an expression node for a 32-bit floating point constant
   whose value is given by a bit pattern. */
static __inline__ IRExpr *
mkF32i(UInt value)
{
   return IRExpr_Const(IRConst_F32i(value));
}

/* Create an expression node for a 32-bit floating point constant
   whose value is given by a bit pattern. */
static __inline__ IRExpr *
mkF64i(ULong value)
{
   return IRExpr_Const(IRConst_F64i(value));
}

/* Little helper function for my sanity. ITE = if-then-else */
static IRExpr *
mkite(IRExpr *condition, IRExpr *iftrue, IRExpr *iffalse)
{
   vassert(typeOfIRExpr(irsb->tyenv, condition) == Ity_I1);

   return IRExpr_ITE(condition, iftrue, iffalse);
}

/* Add a statement that stores DATA at ADDR. This is a big-endian machine. */
static __inline__ void
store(IRExpr *addr, IRExpr *data)
{
   stmt(IRStmt_Store(Iend_BE, addr, data));
}

/* Create an expression that loads a TYPE sized value from ADDR.
   This is a big-endian machine. */
static __inline__ IRExpr *
load(IRType type, IRExpr *addr)
{
   return IRExpr_Load(Iend_BE, type, addr);
}

/* Function call */
static void
call_function(IRExpr *callee_address)
{
   put_IA(callee_address);

   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Call;
}

/* Function call with known target. */
static void
call_function_and_chase(Addr64 callee_address)
{
   put_IA(mkaddr_expr(callee_address));

   dis_res->whatNext = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Call;
}

/* Function return sequence */
static void
return_from_function(IRExpr *return_address)
{
   put_IA(return_address);

   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Ret;
}

/* A conditional branch whose target is not known at instrumentation time.

   if (condition) goto computed_target;

   Needs to be represented as:

   if (! condition) goto next_instruction;
   goto computed_target;
*/
static void
if_condition_goto_computed(IRExpr *condition, IRExpr *target)
{
   vassert(typeOfIRExpr(irsb->tyenv, condition) == Ity_I1);

   condition = unop(Iop_Not1, condition);

   stmt(IRStmt_Exit(condition, Ijk_Boring, IRConst_U64(guest_IA_next_instr),
                    S390X_GUEST_OFFSET(guest_IA)));

   put_IA(target);

   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Boring;
}

/* A conditional branch whose target is known at instrumentation time. */
static void
if_condition_goto(IRExpr *condition, Addr64 target)
{
   vassert(typeOfIRExpr(irsb->tyenv, condition) == Ity_I1);

   stmt(IRStmt_Exit(condition, Ijk_Boring, IRConst_U64(target),
                    S390X_GUEST_OFFSET(guest_IA)));

   put_IA(mkaddr_expr(guest_IA_next_instr));

   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Boring;
}

/* An unconditional branch. Target may or may not be known at instrumentation
   time. */
static void
always_goto(IRExpr *target)
{
   put_IA(target);

   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Boring;
}


/* An unconditional branch to a known target. */
// QQQQ fixme this is now the same as always_goto
static void
always_goto_and_chase(Addr64 target)
{
   put_IA(mkaddr_expr(target));

   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Boring;
}

/* A system call */
static void
system_call(IRExpr *sysno)
{
   /* Store the system call number in the pseudo register. */
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_SYSNO), sysno));

   /* Store the current IA into guest_IP_AT_SYSCALL. libvex_ir.h says so. */
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_IP_AT_SYSCALL),
                   mkU64(guest_IA_curr_instr)));

   put_IA(mkaddr_expr(guest_IA_next_instr));

   /* It's important that all ArchRegs carry their up-to-date value
      at this point.  So we declare an end-of-block here, which
      forces any TempRegs caching ArchRegs to be flushed. */
   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Sys_syscall;
}

/* A side exit that branches back to the current insn if CONDITION is
   true. Does not set DisResult. */
static void
iterate_if(IRExpr *condition)
{
   vassert(typeOfIRExpr(irsb->tyenv, condition) == Ity_I1);

   stmt(IRStmt_Exit(condition, Ijk_Boring, IRConst_U64(guest_IA_curr_instr),
                    S390X_GUEST_OFFSET(guest_IA)));
}

/* A side exit that branches back to the current insn.
   Does not set DisResult. */
static __inline__ void
iterate(void)
{
   iterate_if(IRExpr_Const(IRConst_U1(True)));
}

/* A side exit that branches back to the insn immediately following the
   current insn if CONDITION is true. Does not set DisResult. */
static void
next_insn_if(IRExpr *condition)
{
   vassert(typeOfIRExpr(irsb->tyenv, condition) == Ity_I1);

   stmt(IRStmt_Exit(condition, Ijk_Boring, IRConst_U64(guest_IA_next_instr),
                    S390X_GUEST_OFFSET(guest_IA)));
}

/* Convenience function to restart the current insn */
static void
restart_if(IRExpr *condition)
{
   vassert(typeOfIRExpr(irsb->tyenv, condition) == Ity_I1);

   stmt(IRStmt_Exit(condition, Ijk_InvalICache,
                    IRConst_U64(guest_IA_curr_instr),
                    S390X_GUEST_OFFSET(guest_IA)));
}

/* Convenience function to yield to thread scheduler */
static void
yield_if(IRExpr *condition)
{
   stmt(IRStmt_Exit(condition, Ijk_Yield, IRConst_U64(guest_IA_next_instr),
                    S390X_GUEST_OFFSET(guest_IA)));
}

/* Convenience macro to yield a specification exception if the given condition
   is not met.  Used to pass this type of decoding error up through the call
   chain. */
#define s390_insn_assert(mnm, cond)             \
   do {                                         \
      if (!(cond)) {                            \
         dis_res->whatNext = Dis_StopHere;      \
         dis_res->jk_StopHere = Ijk_NoDecode;   \
         return (mnm);                          \
      }                                         \
   } while (0)

/* Convenience function to check for a specification exception. */
static Bool
is_specification_exception(void)
{
   return (dis_res->whatNext == Dis_StopHere &&
           dis_res->jk_StopHere == Ijk_NoDecode);
}

static __inline__ IRExpr *get_fpr_dw0(UInt);
static __inline__ void    put_fpr_dw0(UInt, IRExpr *);
static __inline__ IRExpr *get_dpr_dw0(UInt);
static __inline__ void    put_dpr_dw0(UInt, IRExpr *);

/* Read a floating point register pair and combine their contents into a
   128-bit value */
static IRExpr *
get_fpr_pair(UInt archreg)
{
   IRExpr *high = get_fpr_dw0(archreg);
   IRExpr *low  = get_fpr_dw0(archreg + 2);

   return binop(Iop_F64HLtoF128, high, low);
}

/* Write a 128-bit floating point value into a register pair. */
static void
put_fpr_pair(UInt archreg, IRExpr *expr)
{
   IRExpr *high = unop(Iop_F128HItoF64, expr);
   IRExpr *low  = unop(Iop_F128LOtoF64, expr);

   put_fpr_dw0(archreg,     high);
   put_fpr_dw0(archreg + 2, low);
}

/* Read a floating point register pair cointaining DFP value
   and combine their contents into a 128-bit value */

static IRExpr *
get_dpr_pair(UInt archreg)
{
   IRExpr *high = get_dpr_dw0(archreg);
   IRExpr *low  = get_dpr_dw0(archreg + 2);

   return binop(Iop_D64HLtoD128, high, low);
}

/* Write a 128-bit decimal floating point value into a register pair. */
static void
put_dpr_pair(UInt archreg, IRExpr *expr)
{
   IRExpr *high = unop(Iop_D128HItoD64, expr);
   IRExpr *low  = unop(Iop_D128LOtoD64, expr);

   put_dpr_dw0(archreg,     high);
   put_dpr_dw0(archreg + 2, low);
}

/* Terminate the current IRSB with an emulation failure. */
static void
emulation_failure_with_expr(IRExpr *emfailure)
{
   vassert(typeOfIRExpr(irsb->tyenv, emfailure) == Ity_I32);

   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_EMNOTE), emfailure));
   dis_res->whatNext = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_EmFail;
}

static void
emulation_failure(VexEmNote fail_kind)
{
   emulation_failure_with_expr(mkU32(fail_kind));
}

/* Terminate the current IRSB with an emulation warning. */
static void
emulation_warning_with_expr(IRExpr *emwarning)
{
   vassert(typeOfIRExpr(irsb->tyenv, emwarning) == Ity_I32);

   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_EMNOTE), emwarning));
   dis_res->whatNext = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_EmWarn;
}

static void
emulation_warning(VexEmNote warn_kind)
{
   emulation_warning_with_expr(mkU32(warn_kind));
}

/*------------------------------------------------------------*/
/*--- IR Debugging aids.                                   ---*/
/*------------------------------------------------------------*/
#if 0

static ULong
s390_do_print(HChar *text, ULong value)
{
   vex_printf("%s %llu\n", text, value);
   return 0;
}

static void
s390_print(HChar *text, IRExpr *value)
{
   IRDirty *d;
   
   d = unsafeIRDirty_0_N(0 /* regparms */, "s390_do_print", &s390_do_print,
                         mkIRExprVec_2(mkU64((ULong)text), value));
   stmt(IRStmt_Dirty(d));
}
#endif


/*------------------------------------------------------------*/
/*--- Build the flags thunk.                               ---*/
/*------------------------------------------------------------*/

/* Completely fill the flags thunk. We're always filling all fields.
   Apparently, that is better for redundant PUT elimination. */
static void
s390_cc_thunk_fill(IRExpr *op, IRExpr *dep1, IRExpr *dep2, IRExpr *ndep)
{
   UInt op_off, dep1_off, dep2_off, ndep_off;

   op_off   = S390X_GUEST_OFFSET(guest_CC_OP);
   dep1_off = S390X_GUEST_OFFSET(guest_CC_DEP1);
   dep2_off = S390X_GUEST_OFFSET(guest_CC_DEP2);
   ndep_off = S390X_GUEST_OFFSET(guest_CC_NDEP);

   stmt(IRStmt_Put(op_off,   op));
   stmt(IRStmt_Put(dep1_off, dep1));
   stmt(IRStmt_Put(dep2_off, dep2));
   stmt(IRStmt_Put(ndep_off, ndep));
}


/* Create an expression for V and widen the result to 64 bit. */
static IRExpr *
s390_cc_widen(IRTemp v, Bool sign_extend)
{
   IRExpr *expr;

   expr = mkexpr(v);

   switch (typeOfIRTemp(irsb->tyenv, v)) {
   case Ity_I64:
      break;
   case Ity_I32:
      expr = unop(sign_extend ? Iop_32Sto64 : Iop_32Uto64, expr);
      break;
   case Ity_I16:
      expr = unop(sign_extend ? Iop_16Sto64 : Iop_16Uto64, expr);
      break;
   case Ity_I8:
      expr = unop(sign_extend ? Iop_8Sto64 : Iop_8Uto64, expr);
      break;
   default:
      vpanic("s390_cc_widen");
   }

   return expr;
}

static void
s390_cc_thunk_put1(UInt opc, IRTemp d1, Bool sign_extend)
{
   IRExpr *op, *dep1, *dep2, *ndep;

   op   = mkU64(opc);
   dep1 = s390_cc_widen(d1, sign_extend);
   dep2 = mkU64(0);
   ndep = mkU64(0);

   s390_cc_thunk_fill(op, dep1, dep2, ndep);
}


static void
s390_cc_thunk_put2(UInt opc, IRTemp d1, IRTemp d2, Bool sign_extend)
{
   IRExpr *op, *dep1, *dep2, *ndep;

   op   = mkU64(opc);
   dep1 = s390_cc_widen(d1, sign_extend);
   dep2 = s390_cc_widen(d2, sign_extend);
   ndep = mkU64(0);

   s390_cc_thunk_fill(op, dep1, dep2, ndep);
}


/* memcheck believes that the NDEP field in the flags thunk is always
   defined. But for some flag computations (e.g. add with carry) that is
   just not true. We therefore need to convey to memcheck that the value
   of the ndep field does matter and therefore we make the DEP2 field
   depend on it:

   DEP2 = original_DEP2 ^ NDEP

   In s390_calculate_cc we exploit that  (a^b)^b == a
   I.e. we xor the DEP2 value with the NDEP value to recover the
   original_DEP2 value. */
static void
s390_cc_thunk_put3(UInt opc, IRTemp d1, IRTemp d2, IRTemp nd, Bool sign_extend)
{
   IRExpr *op, *dep1, *dep2, *ndep, *dep2x;

   op   = mkU64(opc);
   dep1 = s390_cc_widen(d1, sign_extend);
   dep2 = s390_cc_widen(d2, sign_extend);
   ndep = s390_cc_widen(nd, sign_extend);

   dep2x = binop(Iop_Xor64, dep2, ndep);

   s390_cc_thunk_fill(op, dep1, dep2x, ndep);
}


/* Write one floating point value into the flags thunk */
static void
s390_cc_thunk_put1f(UInt opc, IRTemp d1)
{
   IRExpr *op, *dep1, *dep2, *ndep;

   /* Make the CC_DEP1 slot appear completely defined.
      Otherwise, assigning a 32-bit value will cause memcheck
      to trigger an undefinedness error.
   */
   if (sizeofIRType(typeOfIRTemp(irsb->tyenv, d1)) == 4) {
      UInt dep1_off = S390X_GUEST_OFFSET(guest_CC_DEP1);
      stmt(IRStmt_Put(dep1_off, mkU64(0)));
   }
   op   = mkU64(opc);
   dep1 = mkexpr(d1);
   dep2 = mkU64(0);
   ndep = mkU64(0);

   s390_cc_thunk_fill(op, dep1, dep2, ndep);
}


/* Write a floating point value and an integer into the flags thunk. The
   integer value is zero-extended first. */
static void
s390_cc_thunk_putFZ(UInt opc, IRTemp d1, IRTemp d2)
{
   IRExpr *op, *dep1, *dep2, *ndep;

   /* Make the CC_DEP1 slot appear completely defined.
      Otherwise, assigning a 32-bit value will cause memcheck
      to trigger an undefinedness error.
   */
   if (sizeofIRType(typeOfIRTemp(irsb->tyenv, d1)) == 4) {
      UInt dep1_off = S390X_GUEST_OFFSET(guest_CC_DEP1);
      stmt(IRStmt_Put(dep1_off, mkU64(0)));
   }
   op   = mkU64(opc);
   dep1 = mkexpr(d1);
   dep2 = s390_cc_widen(d2, False);
   ndep = mkU64(0);

   s390_cc_thunk_fill(op, dep1, dep2, ndep);
}


/* Write a 128-bit floating point value into the flags thunk. This is
   done by splitting the value into two 64-bits values. */
static void
s390_cc_thunk_put1f128(UInt opc, IRTemp d1)
{
   IRExpr *op, *hi, *lo, *ndep;

   op   = mkU64(opc);
   hi   = unop(Iop_F128HItoF64, mkexpr(d1));
   lo   = unop(Iop_F128LOtoF64, mkexpr(d1));
   ndep = mkU64(0);

   s390_cc_thunk_fill(op, hi, lo, ndep);
}


/* Write a 128-bit floating point value and an integer into the flags thunk.
   The integer value is zero-extended first. */
static void
s390_cc_thunk_put1f128Z(UInt opc, IRTemp d1, IRTemp nd)
{
   IRExpr *op, *hi, *lo, *lox, *ndep;

   op   = mkU64(opc);
   hi   = unop(Iop_F128HItoF64, mkexpr(d1));
   lo   = unop(Iop_ReinterpF64asI64, unop(Iop_F128LOtoF64, mkexpr(d1)));
   ndep = s390_cc_widen(nd, False);

   lox = binop(Iop_Xor64, lo, ndep);  /* convey dependency */

   s390_cc_thunk_fill(op, hi, lox, ndep);
}


/* Write a 128-bit decimal floating point value into the flags thunk.
   This is done by splitting the value into two 64-bits values. */
static void
s390_cc_thunk_put1d128(UInt opc, IRTemp d1)
{
   IRExpr *op, *hi, *lo, *ndep;

   op   = mkU64(opc);
   hi   = unop(Iop_D128HItoD64, mkexpr(d1));
   lo   = unop(Iop_D128LOtoD64, mkexpr(d1));
   ndep = mkU64(0);

   s390_cc_thunk_fill(op, hi, lo, ndep);
}


/* Write a 128-bit decimal floating point value and an integer into the flags
   thunk. The integer value is zero-extended first. */
static void
s390_cc_thunk_put1d128Z(UInt opc, IRTemp d1, IRTemp nd)
{
   IRExpr *op, *hi, *lo, *lox, *ndep;

   op   = mkU64(opc);
   hi   = unop(Iop_D128HItoD64, mkexpr(d1));
   lo   = unop(Iop_ReinterpD64asI64, unop(Iop_D128LOtoD64, mkexpr(d1)));
   ndep = s390_cc_widen(nd, False);

   lox = binop(Iop_Xor64, lo, ndep);  /* convey dependency */

   s390_cc_thunk_fill(op, hi, lox, ndep);
}

static void
s390_cc_set(IRTemp cc)
{
   vassert(typeOfIRTemp(irsb->tyenv, cc) == Ity_I64);

   s390_cc_thunk_fill(mkU64(S390_CC_OP_SET), mkexpr(cc), mkU64(0), mkU64(0));
}

static void
s390_cc_set_val(UInt val)
{
   s390_cc_thunk_fill(mkU64(S390_CC_OP_SET), mkU64(val), mkU64(0), mkU64(0));
}

/* Build IR to calculate the condition code from flags thunk.
   Returns an expression of type Ity_I32 */
static IRExpr *
s390_call_calculate_cc(void)
{
   IRExpr **args, *call, *op, *dep1, *dep2, *ndep;

   op   = IRExpr_Get(S390X_GUEST_OFFSET(guest_CC_OP),   Ity_I64);
   dep1 = IRExpr_Get(S390X_GUEST_OFFSET(guest_CC_DEP1), Ity_I64);
   dep2 = IRExpr_Get(S390X_GUEST_OFFSET(guest_CC_DEP2), Ity_I64);
   ndep = IRExpr_Get(S390X_GUEST_OFFSET(guest_CC_NDEP), Ity_I64);

   args = mkIRExprVec_4(op, dep1, dep2, ndep);
   call = mkIRExprCCall(Ity_I32, 0 /*regparm*/,
                        "s390_calculate_cc", &s390_calculate_cc, args);

   /* Exclude OP and NDEP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<3);

   return call;
}

/* Build IR to calculate the internal condition code for a "compare and branch"
   insn. Returns an expression of type Ity_I32 */
static IRExpr *
s390_call_calculate_icc(UInt m, UInt opc, IRTemp op1, IRTemp op2)
{
   IRExpr **args, *call, *op, *dep1, *dep2, *mask;

   switch (opc) {
   case S390_CC_OP_SIGNED_COMPARE:
      dep1 = s390_cc_widen(op1, True);
      dep2 = s390_cc_widen(op2, True);
      break;

   case S390_CC_OP_UNSIGNED_COMPARE:
      dep1 = s390_cc_widen(op1, False);
      dep2 = s390_cc_widen(op2, False);
      break;

   default:
      vpanic("s390_call_calculate_icc");
   }

   mask = mkU64(m);
   op   = mkU64(opc);

   args = mkIRExprVec_5(mask, op, dep1, dep2, mkU64(0) /* unused */);
   call = mkIRExprCCall(Ity_I32, 0 /*regparm*/,
                        "s390_calculate_cond", &s390_calculate_cond, args);

   /* Exclude the requested condition, OP and NDEP from definedness
      checking.  We're only interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<1) | (1<<4);

   return call;
}

/* Build IR to calculate the condition code from flags thunk.
   Returns an expression of type Ity_I32 */
static IRExpr *
s390_call_calculate_cond(UInt m)
{
   IRExpr **args, *call, *op, *dep1, *dep2, *ndep, *mask;

   mask = mkU64(m);
   op   = IRExpr_Get(S390X_GUEST_OFFSET(guest_CC_OP),   Ity_I64);
   dep1 = IRExpr_Get(S390X_GUEST_OFFSET(guest_CC_DEP1), Ity_I64);
   dep2 = IRExpr_Get(S390X_GUEST_OFFSET(guest_CC_DEP2), Ity_I64);
   ndep = IRExpr_Get(S390X_GUEST_OFFSET(guest_CC_NDEP), Ity_I64);

   args = mkIRExprVec_5(mask, op, dep1, dep2, ndep);
   call = mkIRExprCCall(Ity_I32, 0 /*regparm*/,
                        "s390_calculate_cond", &s390_calculate_cond, args);

   /* Exclude the requested condition, OP and NDEP from definedness
      checking.  We're only interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<1) | (1<<4);

   return call;
}

#define s390_cc_thunk_putZ(op,dep1)  s390_cc_thunk_put1(op,dep1,False)
#define s390_cc_thunk_putS(op,dep1)  s390_cc_thunk_put1(op,dep1,True)
#define s390_cc_thunk_putF(op,dep1)  s390_cc_thunk_put1f(op,dep1)
#define s390_cc_thunk_putZZ(op,dep1,dep2) s390_cc_thunk_put2(op,dep1,dep2,False)
#define s390_cc_thunk_putSS(op,dep1,dep2) s390_cc_thunk_put2(op,dep1,dep2,True)
#define s390_cc_thunk_putFF(op,dep1,dep2) s390_cc_thunk_put2f(op,dep1,dep2)
#define s390_cc_thunk_putZZZ(op,dep1,dep2,ndep) \
        s390_cc_thunk_put3(op,dep1,dep2,ndep,False)
#define s390_cc_thunk_putSSS(op,dep1,dep2,ndep) \
        s390_cc_thunk_put3(op,dep1,dep2,ndep,True)




/*------------------------------------------------------------*/
/*--- Guest register access                                ---*/
/*------------------------------------------------------------*/


/*------------------------------------------------------------*/
/*--- ar registers                                         ---*/
/*------------------------------------------------------------*/

/* Return the guest state offset of a ar register. */
static UInt
ar_offset(UInt archreg)
{
   static const UInt offset[16] = {
      S390X_GUEST_OFFSET(guest_a0),
      S390X_GUEST_OFFSET(guest_a1),
      S390X_GUEST_OFFSET(guest_a2),
      S390X_GUEST_OFFSET(guest_a3),
      S390X_GUEST_OFFSET(guest_a4),
      S390X_GUEST_OFFSET(guest_a5),
      S390X_GUEST_OFFSET(guest_a6),
      S390X_GUEST_OFFSET(guest_a7),
      S390X_GUEST_OFFSET(guest_a8),
      S390X_GUEST_OFFSET(guest_a9),
      S390X_GUEST_OFFSET(guest_a10),
      S390X_GUEST_OFFSET(guest_a11),
      S390X_GUEST_OFFSET(guest_a12),
      S390X_GUEST_OFFSET(guest_a13),
      S390X_GUEST_OFFSET(guest_a14),
      S390X_GUEST_OFFSET(guest_a15),
   };

   vassert(archreg < 16);

   return offset[archreg];
}


/* Return the guest state offset of word #0 of a ar register. */
static __inline__ UInt
ar_w0_offset(UInt archreg)
{
   return ar_offset(archreg) + 0;
}

/* Write word #0 of a ar to the guest state. */
static __inline__ void
put_ar_w0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I32);

   stmt(IRStmt_Put(ar_w0_offset(archreg), expr));
}

/* Read word #0 of a ar register. */
static __inline__ IRExpr *
get_ar_w0(UInt archreg)
{
   return IRExpr_Get(ar_w0_offset(archreg), Ity_I32);
}


/*------------------------------------------------------------*/
/*--- fpr registers                                        ---*/
/*------------------------------------------------------------*/

/* Return the guest state offset of a fpr register.
   FPRs are maped to first doubleword of VRs.
*/
static UInt
fpr_offset(UInt archreg)
{
   static const UInt offset[16] = {
      S390X_GUEST_OFFSET(guest_v0),
      S390X_GUEST_OFFSET(guest_v1),
      S390X_GUEST_OFFSET(guest_v2),
      S390X_GUEST_OFFSET(guest_v3),
      S390X_GUEST_OFFSET(guest_v4),
      S390X_GUEST_OFFSET(guest_v5),
      S390X_GUEST_OFFSET(guest_v6),
      S390X_GUEST_OFFSET(guest_v7),
      S390X_GUEST_OFFSET(guest_v8),
      S390X_GUEST_OFFSET(guest_v9),
      S390X_GUEST_OFFSET(guest_v10),
      S390X_GUEST_OFFSET(guest_v11),
      S390X_GUEST_OFFSET(guest_v12),
      S390X_GUEST_OFFSET(guest_v13),
      S390X_GUEST_OFFSET(guest_v14),
      S390X_GUEST_OFFSET(guest_v15),
   };

   vassert(archreg < 16);

   return offset[archreg];
}


/* Return the guest state offset of word #0 of a fpr register. */
static __inline__ UInt
fpr_w0_offset(UInt archreg)
{
   return fpr_offset(archreg) + 0;
}

/* Write word #0 of a fpr to the guest state. */
static __inline__ void
put_fpr_w0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_F32);

   stmt(IRStmt_Put(fpr_w0_offset(archreg), expr));
}

/* Read word #0 of a fpr register. */
static __inline__ IRExpr *
get_fpr_w0(UInt archreg)
{
   return IRExpr_Get(fpr_w0_offset(archreg), Ity_F32);
}

/* Return the guest state offset of double word #0 of a fpr register. */
static __inline__ UInt
fpr_dw0_offset(UInt archreg)
{
   return fpr_offset(archreg) + 0;
}

/* Write double word #0 of a fpr to the guest state. */
static __inline__ void
put_fpr_dw0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_F64);

   stmt(IRStmt_Put(fpr_dw0_offset(archreg), expr));
}

/* Read double word #0 of a fpr register. */
static __inline__ IRExpr *
get_fpr_dw0(UInt archreg)
{
   return IRExpr_Get(fpr_dw0_offset(archreg), Ity_F64);
}

/* Write word #0 of a dpr to the guest state. */
static __inline__ void
put_dpr_w0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_D32);

   stmt(IRStmt_Put(fpr_w0_offset(archreg), expr));
}

/* Read word #0 of a dpr register. */
static __inline__ IRExpr *
get_dpr_w0(UInt archreg)
{
   return IRExpr_Get(fpr_w0_offset(archreg), Ity_D32);
}

/* Write double word #0 of a fpr containg DFP value to the guest state. */
static __inline__ void
put_dpr_dw0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_D64);

   stmt(IRStmt_Put(fpr_dw0_offset(archreg), expr));
}

/* Read double word #0 of a fpr register containing DFP value. */
static __inline__ IRExpr *
get_dpr_dw0(UInt archreg)
{
   return IRExpr_Get(fpr_dw0_offset(archreg), Ity_D64);
}

/*------------------------------------------------------------*/
/*--- gpr registers                                        ---*/
/*------------------------------------------------------------*/

/* Return the guest state offset of a gpr register. */
static UInt
gpr_offset(UInt archreg)
{
   static const UInt offset[16] = {
      S390X_GUEST_OFFSET(guest_r0),
      S390X_GUEST_OFFSET(guest_r1),
      S390X_GUEST_OFFSET(guest_r2),
      S390X_GUEST_OFFSET(guest_r3),
      S390X_GUEST_OFFSET(guest_r4),
      S390X_GUEST_OFFSET(guest_r5),
      S390X_GUEST_OFFSET(guest_r6),
      S390X_GUEST_OFFSET(guest_r7),
      S390X_GUEST_OFFSET(guest_r8),
      S390X_GUEST_OFFSET(guest_r9),
      S390X_GUEST_OFFSET(guest_r10),
      S390X_GUEST_OFFSET(guest_r11),
      S390X_GUEST_OFFSET(guest_r12),
      S390X_GUEST_OFFSET(guest_r13),
      S390X_GUEST_OFFSET(guest_r14),
      S390X_GUEST_OFFSET(guest_r15),
   };

   vassert(archreg < 16);

   return offset[archreg];
}


/* Return the guest state offset of word #0 of a gpr register. */
static __inline__ UInt
gpr_w0_offset(UInt archreg)
{
   return gpr_offset(archreg) + 0;
}

/* Read an integer of given type from a gpr. */
static __inline__ IRExpr *
get_gpr_int(UInt archreg, IRType ty)
{
   return IRExpr_Get(gpr_offset(archreg) + 8 - sizeofIRType(ty), ty);
}

/* Write word #0 of a gpr to the guest state. */
static __inline__ void
put_gpr_w0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I32);

   stmt(IRStmt_Put(gpr_w0_offset(archreg), expr));
}

/* Read word #0 of a gpr register. */
static __inline__ IRExpr *
get_gpr_w0(UInt archreg)
{
   return IRExpr_Get(gpr_w0_offset(archreg), Ity_I32);
}

/* Return the guest state offset of double word #0 of a gpr register. */
static __inline__ UInt
gpr_dw0_offset(UInt archreg)
{
   return gpr_offset(archreg) + 0;
}

/* Write double word #0 of a gpr to the guest state. */
static __inline__ void
put_gpr_dw0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I64);

   stmt(IRStmt_Put(gpr_dw0_offset(archreg), expr));
}

/* Read double word #0 of a gpr register. */
static __inline__ IRExpr *
get_gpr_dw0(UInt archreg)
{
   return IRExpr_Get(gpr_dw0_offset(archreg), Ity_I64);
}

/* Return the guest state offset of half word #1 of a gpr register. */
static __inline__ UInt
gpr_hw1_offset(UInt archreg)
{
   return gpr_offset(archreg) + 2;
}

/* Write half word #1 of a gpr to the guest state. */
static __inline__ void
put_gpr_hw1(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I16);

   stmt(IRStmt_Put(gpr_hw1_offset(archreg), expr));
}

/* Read half word #1 of a gpr register. */
static __inline__ IRExpr *
get_gpr_hw1(UInt archreg)
{
   return IRExpr_Get(gpr_hw1_offset(archreg), Ity_I16);
}

/* Return the guest state offset of byte #6 of a gpr register. */
static __inline__ UInt
gpr_b6_offset(UInt archreg)
{
   return gpr_offset(archreg) + 6;
}

/* Write byte #6 of a gpr to the guest state. */
static __inline__ void
put_gpr_b6(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I8);

   stmt(IRStmt_Put(gpr_b6_offset(archreg), expr));
}

/* Read byte #6 of a gpr register. */
static __inline__ IRExpr *
get_gpr_b6(UInt archreg)
{
   return IRExpr_Get(gpr_b6_offset(archreg), Ity_I8);
}

/* Return the guest state offset of byte #3 of a gpr register. */
static __inline__ UInt
gpr_b3_offset(UInt archreg)
{
   return gpr_offset(archreg) + 3;
}

/* Write byte #3 of a gpr to the guest state. */
static __inline__ void
put_gpr_b3(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I8);

   stmt(IRStmt_Put(gpr_b3_offset(archreg), expr));
}

/* Read byte #3 of a gpr register. */
static __inline__ IRExpr *
get_gpr_b3(UInt archreg)
{
   return IRExpr_Get(gpr_b3_offset(archreg), Ity_I8);
}

/* Return the guest state offset of byte #0 of a gpr register. */
static __inline__ UInt
gpr_b0_offset(UInt archreg)
{
   return gpr_offset(archreg) + 0;
}

/* Write byte #0 of a gpr to the guest state. */
static __inline__ void
put_gpr_b0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I8);

   stmt(IRStmt_Put(gpr_b0_offset(archreg), expr));
}

/* Read byte #0 of a gpr register. */
static __inline__ IRExpr *
get_gpr_b0(UInt archreg)
{
   return IRExpr_Get(gpr_b0_offset(archreg), Ity_I8);
}

/* Return the guest state offset of word #1 of a gpr register. */
static __inline__ UInt
gpr_w1_offset(UInt archreg)
{
   return gpr_offset(archreg) + 4;
}

/* Write word #1 of a gpr to the guest state. */
static __inline__ void
put_gpr_w1(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I32);

   stmt(IRStmt_Put(gpr_w1_offset(archreg), expr));
}

/* Read word #1 of a gpr register. */
static __inline__ IRExpr *
get_gpr_w1(UInt archreg)
{
   return IRExpr_Get(gpr_w1_offset(archreg), Ity_I32);
}

/* Return the guest state offset of half word #3 of a gpr register. */
static __inline__ UInt
gpr_hw3_offset(UInt archreg)
{
   return gpr_offset(archreg) + 6;
}

/* Write half word #3 of a gpr to the guest state. */
static __inline__ void
put_gpr_hw3(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I16);

   stmt(IRStmt_Put(gpr_hw3_offset(archreg), expr));
}

/* Read half word #3 of a gpr register. */
static __inline__ IRExpr *
get_gpr_hw3(UInt archreg)
{
   return IRExpr_Get(gpr_hw3_offset(archreg), Ity_I16);
}

/* Return the guest state offset of byte #7 of a gpr register. */
static __inline__ UInt
gpr_b7_offset(UInt archreg)
{
   return gpr_offset(archreg) + 7;
}

/* Write byte #7 of a gpr to the guest state. */
static __inline__ void
put_gpr_b7(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I8);

   stmt(IRStmt_Put(gpr_b7_offset(archreg), expr));
}

/* Read byte #7 of a gpr register. */
static __inline__ IRExpr *
get_gpr_b7(UInt archreg)
{
   return IRExpr_Get(gpr_b7_offset(archreg), Ity_I8);
}

/* Return the guest state offset of half word #0 of a gpr register. */
static __inline__ UInt
gpr_hw0_offset(UInt archreg)
{
   return gpr_offset(archreg) + 0;
}

/* Write half word #0 of a gpr to the guest state. */
static __inline__ void
put_gpr_hw0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I16);

   stmt(IRStmt_Put(gpr_hw0_offset(archreg), expr));
}

/* Read half word #0 of a gpr register. */
static __inline__ IRExpr *
get_gpr_hw0(UInt archreg)
{
   return IRExpr_Get(gpr_hw0_offset(archreg), Ity_I16);
}

/* Return the guest state offset of byte #4 of a gpr register. */
static __inline__ UInt
gpr_b4_offset(UInt archreg)
{
   return gpr_offset(archreg) + 4;
}

/* Write byte #4 of a gpr to the guest state. */
static __inline__ void
put_gpr_b4(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I8);

   stmt(IRStmt_Put(gpr_b4_offset(archreg), expr));
}

/* Read byte #4 of a gpr register. */
static __inline__ IRExpr *
get_gpr_b4(UInt archreg)
{
   return IRExpr_Get(gpr_b4_offset(archreg), Ity_I8);
}

/* Return the guest state offset of byte #1 of a gpr register. */
static __inline__ UInt
gpr_b1_offset(UInt archreg)
{
   return gpr_offset(archreg) + 1;
}

/* Write byte #1 of a gpr to the guest state. */
static __inline__ void
put_gpr_b1(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I8);

   stmt(IRStmt_Put(gpr_b1_offset(archreg), expr));
}

/* Read byte #1 of a gpr register. */
static __inline__ IRExpr *
get_gpr_b1(UInt archreg)
{
   return IRExpr_Get(gpr_b1_offset(archreg), Ity_I8);
}

/* Return the guest state offset of half word #2 of a gpr register. */
static __inline__ UInt
gpr_hw2_offset(UInt archreg)
{
   return gpr_offset(archreg) + 4;
}

/* Write half word #2 of a gpr to the guest state. */
static __inline__ void
put_gpr_hw2(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I16);

   stmt(IRStmt_Put(gpr_hw2_offset(archreg), expr));
}

/* Read half word #2 of a gpr register. */
static __inline__ IRExpr *
get_gpr_hw2(UInt archreg)
{
   return IRExpr_Get(gpr_hw2_offset(archreg), Ity_I16);
}

/* Return the guest state offset of byte #5 of a gpr register. */
static __inline__ UInt
gpr_b5_offset(UInt archreg)
{
   return gpr_offset(archreg) + 5;
}

/* Write byte #5 of a gpr to the guest state. */
static __inline__ void
put_gpr_b5(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I8);

   stmt(IRStmt_Put(gpr_b5_offset(archreg), expr));
}

/* Read byte #5 of a gpr register. */
static __inline__ IRExpr *
get_gpr_b5(UInt archreg)
{
   return IRExpr_Get(gpr_b5_offset(archreg), Ity_I8);
}

/* Return the guest state offset of byte #2 of a gpr register. */
static __inline__ UInt
gpr_b2_offset(UInt archreg)
{
   return gpr_offset(archreg) + 2;
}

/* Write byte #2 of a gpr to the guest state. */
static __inline__ void
put_gpr_b2(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I8);

   stmt(IRStmt_Put(gpr_b2_offset(archreg), expr));
}

/* Read byte #2 of a gpr register. */
static __inline__ IRExpr *
get_gpr_b2(UInt archreg)
{
   return IRExpr_Get(gpr_b2_offset(archreg), Ity_I8);
}

/* Return the guest state offset of the counter register. */
static UInt
counter_offset(void)
{
   return S390X_GUEST_OFFSET(guest_counter);
}

/* Return the guest state offset of double word #0 of the counter register. */
static __inline__ UInt
counter_dw0_offset(void)
{
   return counter_offset() + 0;
}

/* Write double word #0 of the counter to the guest state. */
static __inline__ void
put_counter_dw0(IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I64);

   stmt(IRStmt_Put(counter_dw0_offset(), expr));
}

/* Read double word #0 of the counter register. */
static __inline__ IRExpr *
get_counter_dw0(void)
{
   return IRExpr_Get(counter_dw0_offset(), Ity_I64);
}

/* Return the guest state offset of word #0 of the counter register. */
static __inline__ UInt
counter_w0_offset(void)
{
   return counter_offset() + 0;
}

/* Return the guest state offset of word #1 of the counter register. */
static __inline__ UInt
counter_w1_offset(void)
{
   return counter_offset() + 4;
}

/* Write word #0 of the counter to the guest state. */
static __inline__ void
put_counter_w0(IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I32);

   stmt(IRStmt_Put(counter_w0_offset(), expr));
}

/* Read word #0 of the counter register. */
static __inline__ IRExpr *
get_counter_w0(void)
{
   return IRExpr_Get(counter_w0_offset(), Ity_I32);
}

/* Write word #1 of the counter to the guest state. */
static __inline__ void
put_counter_w1(IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I32);

   stmt(IRStmt_Put(counter_w1_offset(), expr));
}

/* Read word #1 of the counter register. */
static __inline__ IRExpr *
get_counter_w1(void)
{
   return IRExpr_Get(counter_w1_offset(), Ity_I32);
}

/* Return the guest state offset of the fpc register. */
static UInt
fpc_offset(void)
{
   return S390X_GUEST_OFFSET(guest_fpc);
}

/* Return the guest state offset of word #0 of the fpc register. */
static __inline__ UInt
fpc_w0_offset(void)
{
   return fpc_offset() + 0;
}

/* Write word #0 of the fpc to the guest state. */
static __inline__ void
put_fpc_w0(IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I32);

   stmt(IRStmt_Put(fpc_w0_offset(), expr));
}

/* Read word #0 of the fpc register. */
static __inline__ IRExpr *
get_fpc_w0(void)
{
   return IRExpr_Get(fpc_w0_offset(), Ity_I32);
}


/*------------------------------------------------------------*/
/*--- vr registers                                        ---*/
/*------------------------------------------------------------*/

/* Return the guest state offset of a vr register. */
static UInt
vr_offset(const UInt archreg)
{
   static const UInt offset[32] = {
      S390X_GUEST_OFFSET(guest_v0),
      S390X_GUEST_OFFSET(guest_v1),
      S390X_GUEST_OFFSET(guest_v2),
      S390X_GUEST_OFFSET(guest_v3),
      S390X_GUEST_OFFSET(guest_v4),
      S390X_GUEST_OFFSET(guest_v5),
      S390X_GUEST_OFFSET(guest_v6),
      S390X_GUEST_OFFSET(guest_v7),
      S390X_GUEST_OFFSET(guest_v8),
      S390X_GUEST_OFFSET(guest_v9),
      S390X_GUEST_OFFSET(guest_v10),
      S390X_GUEST_OFFSET(guest_v11),
      S390X_GUEST_OFFSET(guest_v12),
      S390X_GUEST_OFFSET(guest_v13),
      S390X_GUEST_OFFSET(guest_v14),
      S390X_GUEST_OFFSET(guest_v15),
      S390X_GUEST_OFFSET(guest_v16),
      S390X_GUEST_OFFSET(guest_v17),
      S390X_GUEST_OFFSET(guest_v18),
      S390X_GUEST_OFFSET(guest_v19),
      S390X_GUEST_OFFSET(guest_v20),
      S390X_GUEST_OFFSET(guest_v21),
      S390X_GUEST_OFFSET(guest_v22),
      S390X_GUEST_OFFSET(guest_v23),
      S390X_GUEST_OFFSET(guest_v24),
      S390X_GUEST_OFFSET(guest_v25),
      S390X_GUEST_OFFSET(guest_v26),
      S390X_GUEST_OFFSET(guest_v27),
      S390X_GUEST_OFFSET(guest_v28),
      S390X_GUEST_OFFSET(guest_v29),
      S390X_GUEST_OFFSET(guest_v30),
      S390X_GUEST_OFFSET(guest_v31),
   };

   vassert(archreg < 32);

   return offset[archreg];
}

/* Return the guest state offset of quadword of a vr register. */
static UInt
vr_qw_offset(const UInt archreg)
{
   return vr_offset(archreg) + 0;
}

/* Write quadword of a vr to the guest state. */
static void
put_vr_qw(const UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_V128);

   stmt(IRStmt_Put(vr_qw_offset(archreg), expr));
}

/* Read quadword of a vr register. */
static IRExpr *
get_vr_qw(const UInt archreg)
{
   return IRExpr_Get(vr_qw_offset(archreg), Ity_V128);
}

/* Return the guest state offset of double word #0 of a gpr register. */
static UInt
vr_dw0_offset(UInt archreg)
{
   return vr_offset(archreg) + 0;
}

/* Read doubleword #0 of a vr register. */
static IRExpr *
get_vr_dw0(UInt archreg)
{
   return IRExpr_Get(vr_dw0_offset(archreg), Ity_I64);
}

/* Write double word #0 of a vr to the guest state. */
static void
put_vr_dw0(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I64);

   stmt(IRStmt_Put(vr_dw0_offset(archreg), expr));
}

/* Return the guest state offset of double word #1 of a gpr register. */
static UInt
vr_dw1_offset(UInt archreg)
{
   return vr_offset(archreg) + 8;
}

/* Read doubleword #1 of a vr register. */
static IRExpr *
get_vr_dw1(UInt archreg)
{
   return IRExpr_Get(vr_dw1_offset(archreg), Ity_I64);
}

/* Write double word #0 of a vr to the guest state. */
static void
put_vr_dw1(UInt archreg, IRExpr *expr)
{
   vassert(typeOfIRExpr(irsb->tyenv, expr) == Ity_I64);

   stmt(IRStmt_Put(vr_dw1_offset(archreg), expr));
}

/* Return the guest state offset of word #1 of a gpr register. */
static UInt
vr_w1_offset(UInt archreg)
{
   return vr_offset(archreg) + 4;
}

/* Return the guest state offset of word #3 of a gpr register. */
static UInt
vr_w3_offset(UInt archreg)
{
   return vr_offset(archreg) + 12;
}

/* Read word #0 of a vr register. */
static IRExpr *
get_vr_w0(UInt archreg)
{
   return IRExpr_Get(vr_dw0_offset(archreg), Ity_I32);
}

/* Read word #1 of a vr register. */
static IRExpr *
get_vr_w1(UInt archreg)
{
   return IRExpr_Get(vr_w1_offset(archreg), Ity_I32);
}

/* Read word #2 of a vr register. */
static IRExpr *
get_vr_w2(UInt archreg)
{
   return IRExpr_Get(vr_dw1_offset(archreg), Ity_I32);
}

/* Read word #3 of a vr register. */
static IRExpr *
get_vr_w3(UInt archreg)
{
   return IRExpr_Get(vr_w3_offset(archreg), Ity_I32);
}

/* Return the guest state offset of halfword #3 of a gpr register. */
static UInt
vr_hw3_offset(UInt archreg)
{
   return vr_offset(archreg) + 6;
}

/* Read halfword #3 of a vr register. */
static IRExpr *
get_vr_hw3(UInt archreg)
{
   return IRExpr_Get(vr_hw3_offset(archreg), Ity_I16);
}

/* Return the guest state offset of halfword #7 of a gpr register. */
static UInt
vr_hw7_offset(UInt archreg)
{
   return vr_offset(archreg) + 14;
}

/* Read halfword #7 of a vr register. */
static IRExpr *
get_vr_hw7(UInt archreg)
{
   return IRExpr_Get(vr_hw7_offset(archreg), Ity_I16);
}

/* Return the guest state offset of byte #7 of a vr register. */
static UInt
vr_b7_offset(UInt archreg)
{
   return vr_offset(archreg) + 7;
}

/* Read byte #7 of a vr register. */
static IRExpr *
get_vr_b7(UInt archreg)
{
   return IRExpr_Get(vr_b7_offset(archreg), Ity_I8);
}

/* Return the guest state offset of byte #15 of a vr register. */
static UInt
vr_b15_offset(UInt archreg)
{
   return vr_offset(archreg) + 15;
}

/* Read byte #15 of a vr register. */
static IRExpr *
get_vr_b15(UInt archreg)
{
   return IRExpr_Get(vr_b15_offset(archreg), Ity_I8);
}

/* Determine IRType by instruction's m3 field */
static IRType
s390_vr_get_type(const UChar m)
{
   static const IRType results[] = {Ity_I8, Ity_I16, Ity_I32, Ity_I64, Ity_V128};
   if (m > 4) {
      vex_printf("s390_vr_get_type: m=%x\n", m);
      vpanic("s390_vr_get_type: reserved m value");
   }

   return results[m];
}

/* Determine if Condition Code Set (CS) flag is set in m field */
#define s390_vr_is_cs_set(m) (((m) & 0x1) != 0)

/* Determine if Zero Search (ZS) flag is set in m field */
#define s390_vr_is_zs_set(m) (((m) & 0b0010) != 0)

/* Check if the "Single-Element-Control" bit is set.
   Used in vector FP instructions.
 */
#define s390_vr_is_single_element_control_set(m) (((m) & 0x8) != 0)

/* Generates arg1 < arg2 (or arg1 <= arg2 if allow_equal == True) expression.
   Arguments must have V128 type and are treated as unsigned 128-bit numbers.
*/
static IRExpr*
s390_V128_compareLT128x1(IRExpr* arg1, IRExpr* arg2, Bool allow_equal)
{
   /* If high halves are equal
      then we compare lower ones
      otherwise we compare high halves.
    */
   IRExpr* result;
   result = mkite(binop(Iop_CmpEQ64,
                        unop(Iop_V128HIto64, arg1),
                        unop(Iop_V128HIto64, arg2)
                        ),
                  unop(Iop_1Uto64,
                       binop(allow_equal ? Iop_CmpLE64U : Iop_CmpLT64U,
                             unop(Iop_V128to64, arg1),
                             unop(Iop_V128to64, arg2)
                            )
                      ),
                  unop(Iop_1Uto64,
                       binop(Iop_CmpLT64U,
                             unop(Iop_V128HIto64, arg1),
                             unop(Iop_V128HIto64, arg2)
                            )
                      )
                  );

   return result;
}

/* Generates arg1 == 0 expression.
   Argument must have V128 type and is treated as unsigned 128-bit number.
*/
static IRExpr*
s390_V128_isZero(IRExpr* arg)
{
   IRExpr* high_or_low = binop(Iop_Or64,
                               unop(Iop_V128to64, arg),
                               unop(Iop_V128HIto64, arg)
                              );

   return unop(Iop_1Uto64, binop(Iop_CmpEQ64, high_or_low, mkU64(0ULL)));
}

/* Generate the two's complement for arg.
   Arg should be V128.
*/
static IRExpr*
s390_V128_get_complement(IRExpr* arg, IRType type)
{
   IRExpr* notArg = unop(Iop_NotV128, arg);
   IRExpr* ones;
   IRExpr* result;
   switch(type) {
   case Ity_I8:
      ones = unop(Iop_Dup8x16, mkU8(0x01));
      result = binop(Iop_Add8x16, notArg, ones);
      break;
   case Ity_I16:
      ones = unop(Iop_Dup16x8, mkU16(0x0001));
      result = binop(Iop_Add16x8, notArg, ones);
      break;
   case Ity_I32:
      ones = unop(Iop_Dup32x4, mkU32(0x00000001));
      result = binop(Iop_Add32x4, notArg, ones);
      break;
   case Ity_I64:
      ones = binop(Iop_64HLtoV128, mkU64(0x1ULL), mkU64(0x1ULL));
      result = binop(Iop_Add64x2, notArg, ones);
      break;
   case Ity_V128:
      ones = binop(Iop_64HLtoV128, mkU64(0x0ULL), mkU64(0x1ULL));
      result = binop(Iop_Add128x1, notArg, ones);
      break;
   default:
      vpanic("s390_V128_get_complement: unknown type");
   }

   return result;
}

/* # Elements are treated as 128-bit unsigned integers
   For i = 0; i < elemCount; i++ do:
      sum = arg1[i] + arg2[i]
      result[i] = carry_out_bit(sum)
   end
   return result
 */
static IRExpr*
s390_V128_calculate_carry_out(IRExpr* arg1, IRExpr* arg2, IRType type,
                              Bool allow_equal)
{
   IRTemp sum = newTemp(Ity_V128);
   IRExpr* mask;
   IRExpr* comparison;
   IRExpr* result;
   switch(type){
   case Ity_I8:
      assign(sum, binop(Iop_Add8x16, arg1, arg2));
      mask = unop(Iop_Dup8x16, mkU8(0x1));
      comparison = binop(Iop_CmpGT8Ux16, arg1, mkexpr(sum));
      if(allow_equal) {
         comparison = binop(Iop_OrV128, binop(Iop_CmpEQ8x16, arg1, mkexpr(sum)),
                            comparison);
      }
      result = binop(Iop_AndV128, comparison, mask);
      break;
   case Ity_I16:
      assign(sum, binop(Iop_Add16x8, arg1, arg2));
      mask = unop(Iop_Dup16x8, mkU16(0x1));
      comparison = binop(Iop_CmpGT16Ux8, arg1, mkexpr(sum));
      if(allow_equal) {
         comparison = binop(Iop_OrV128, binop(Iop_CmpEQ16x8, arg1, mkexpr(sum)),
                            comparison);
      }
      result = binop(Iop_AndV128, comparison, mask);
      break;
   case Ity_I32:
      assign(sum, binop(Iop_Add32x4, arg1, arg2));
      mask = unop(Iop_Dup32x4, mkU32(0x1));
      comparison = binop(Iop_CmpGT32Ux4, arg1, mkexpr(sum));
      if(allow_equal) {
         comparison = binop(Iop_OrV128, binop(Iop_CmpEQ32x4, arg1, mkexpr(sum)),
                            comparison);
      }
      result = binop(Iop_AndV128, comparison, mask);
      break;
   case Ity_I64:
      assign(sum, binop(Iop_Add64x2, arg1, arg2));
      mask = binop(Iop_64HLtoV128, mkU64(0x1), mkU64(0x1));
      comparison = binop(Iop_CmpGT64Ux2, arg1, mkexpr(sum));
      if(allow_equal) {
         comparison = binop(Iop_OrV128, binop(Iop_CmpEQ64x2, arg1, mkexpr(sum)),
                            comparison);
      }
      result = binop(Iop_AndV128, comparison, mask);
      break;
   case Ity_V128:
      assign(sum, binop(Iop_Add128x1, arg1, arg2));
      comparison = s390_V128_compareLT128x1(mkexpr(sum), arg1, allow_equal);
      result = binop(Iop_64HLtoV128, mkU64(0x0), comparison);
      break;
   default:
      ppIRType(type);
      vpanic("s390_V128_calculate_carry_out: unknown type");
   }

   return result;
}

/* # elemCount = 1 for now (elements are 128-bit unsigned integers)
   For i = 0; i < elemCount; i++ do:
      sum = arg1[i] + arg2[i] + arg3[i] & 0x1
      result[i] = carry_out_bit(sum)
   end
   return result
 */
static IRExpr*
s390_V128_calculate_carry_out_with_carry(IRExpr* arg1, IRExpr* arg2, IRExpr* arg3)
{
   IRTemp sum = newTemp(Ity_V128);
   assign(sum, binop(Iop_Add128x1, arg1, arg2));

   IRTemp overflow_before = newTemp(Ity_I64);
   assign(overflow_before, s390_V128_compareLT128x1(mkexpr(sum), arg1, False));

   IRExpr* mask = binop(Iop_64HLtoV128, mkU64(0), mkU64(1));
   IRTemp carry_in = newTemp(Ity_V128);
   assign(carry_in, binop(Iop_AndV128, arg3, mask));

   IRExpr* carry_is_not_zero = unop(Iop_1Uto64,
                                    binop(Iop_CmpNE64,
                                          unop(Iop_V128to64, mkexpr(carry_in)),
                                          mkU64(0ULL)
                                         )
                                    );

   IRTemp sum_plus_carry = newTemp(Ity_V128);
   assign(sum_plus_carry, binop(Iop_Add128x1, mkexpr(sum), mkexpr(carry_in)));

   IRExpr* overflow_after = binop(Iop_And64,
                                  carry_is_not_zero,
                                  s390_V128_isZero(mkexpr(sum_plus_carry))
                                  );

   IRExpr* result = binop(Iop_Or64, mkexpr(overflow_before), overflow_after);
   result = binop(Iop_64HLtoV128, mkU64(0Ull), result);
   return result;
}

/* Performs "arg1 + arg2 + carry_out_bit(arg1 + arg2)".
   Arguments and result are Ity_I32.
*/
static IRTemp
s390_checksum_add(IRExpr* arg1, IRExpr* arg2)
{
   IRTemp sum = newTemp(Ity_I32);
   IRTemp res = newTemp(Ity_I32);

   assign(sum, binop(Iop_Add32, arg1, arg2));
   assign(res,
          mkite(binop(Iop_CmpLT32U, mkexpr(sum), arg1),
                binop(Iop_Add32, mkexpr(sum), mkU32(1)),
                mkexpr(sum))
               );

   return res;
}

/* Return the guest state offset of element with type's size and given index
   of a vr register.
*/
static UInt
s390_vr_offset_by_index(UInt archreg,IRType type, UChar index)
{
   switch (type) {
   case Ity_I8:
      if(index > 15) {
         goto invalidIndex;
      }
      return vr_offset(archreg) + sizeof(UChar) * index;

   case Ity_I16:
      if(index > 7) {
         goto invalidIndex;
      }
      return vr_offset(archreg) + sizeof(UShort) * index;

   case Ity_I32:
   case Ity_F32:
      if(index > 3) {
         goto invalidIndex;
      }
      return vr_offset(archreg) + sizeof(UInt) * index;

   case Ity_I64:
   case Ity_F64:
      if(index > 1) {
         goto invalidIndex;
      }
      return vr_offset(archreg) + sizeof(ULong) * index;
   case Ity_V128:
      if(index == 0) {
         return vr_qw_offset(archreg);
      } else {
         goto invalidIndex;
      }
   default:
      vpanic("s390_vr_offset_by_index: unknown type");
   }

   invalidIndex:
      vex_printf("s390_vr_offset_by_index: index = %d ; type = ", index);
      ppIRType(type);
      vpanic("s390_vr_offset_by_index: invalid index for given type");
}

/* Write type sized element to indexed part of vr to the guest state. */
static void
put_vr(UInt archreg, IRType type, UChar index, IRExpr *expr)
{
   UInt offset = s390_vr_offset_by_index(archreg, type, index);
   vassert(typeOfIRExpr(irsb->tyenv, expr) == type);

   stmt(IRStmt_Put(offset, expr));
}

/* Read type sized part specified by index of a vr register. */
static IRExpr *
get_vr(UInt archreg, IRType type, UChar index)
{
   UInt offset = s390_vr_offset_by_index(archreg, type, index);
   return IRExpr_Get(offset, type);
}

/* Calculates vr index according to instruction's rxb field
   and position of vr in instruction.
   Index of first argument must be 1 (not zero) */
static UChar
s390_vr_getVRindex(UChar v,UChar argNumber, UChar rxb)
{
   vassert(argNumber > 0 && argNumber <= 4);
   vassert(rxb < 16);
   return v | (((rxb) << argNumber) & 0b00010000);
}

static void
s390_vr_fill(UChar v1, IRExpr *o2)
{
   IRType o2type = typeOfIRExpr(irsb->tyenv, o2);
   switch (o2type) {
   case Ity_I8:
      put_vr_qw(v1, unop(Iop_Dup8x16, o2));
      break;
   case Ity_I16:
      put_vr_qw(v1, unop(Iop_Dup16x8, o2));
      break;
   case Ity_I32:
      put_vr_qw(v1, unop(Iop_Dup32x4, o2));
      break;
   case Ity_I64:
      put_vr_qw(v1, binop(Iop_64HLtoV128, o2, o2));
      break;
   default:
      ppIRType(o2type);
      vpanic("s390_vr_fill: invalid IRType");
   }
}

/* Returns Ity_I32 number of bytes till block boundary specified by m */
static IRExpr*
s390_getCountToBlockBoundary(IRTemp op2addr, UChar m)
{
   IRTemp boundary = newTemp(Ity_I32);
   IRTemp sixteen = newTemp(Ity_I32);
   IRTemp divisionResult = newTemp(Ity_I64);
   IRTemp mod_result = newTemp(Ity_I32);
   IRTemp output = newTemp(Ity_I32);

   switch (m) {
   case 0: assign(boundary, mkU32(64)); break;
   case 1: assign(boundary, mkU32(128)); break;
   case 2: assign(boundary, mkU32(256)); break;
   case 3: assign(boundary, mkU32(512)); break;
   case 4: assign(boundary, mkU32(1024)); break;
   case 5: assign(boundary, mkU32(2048)); break;
   case 6: assign(boundary, mkU32(4096)); break;
   default:
      vex_printf("m = %d\n", m);
      vpanic("s390_getCountToBlockBoundary: invalid m");
   }
   assign(sixteen, mkU32(16));
   assign(divisionResult,
          binop(Iop_DivModU64to32, mkexpr(op2addr), mkexpr(boundary)));
   assign(mod_result,
          binop(Iop_Sub32,mkexpr(boundary),
                unop(Iop_64HIto32, mkexpr(divisionResult))));

   assign(output,
          mkite(binop(Iop_CmpLE32U, mkexpr(sixteen), mkexpr(mod_result)),
          mkexpr(sixteen),
          mkexpr(mod_result)
         ));

   return mkexpr(output);
}

/* Load bytes into v1.
   maxIndex specifies max index to load and must be Ity_I32.
   If maxIndex >= 15, all 16 bytes are loaded.
   All bytes after maxIndex are zeroed. */
static void s390_vr_loadWithLength(UChar v1, IRTemp addr, IRExpr *maxIndex)
{
   IRTemp maxIdx = newTemp(Ity_I32);
   IRTemp cappedMax = newTemp(Ity_I64);
   IRTemp offset = newTemp(Ity_I64);
   IRTemp zeroed = newTemp(Ity_I64);
   IRTemp back = newTemp(Ity_I64);

   /* Implement the insn with a single 16-byte load, to allow memcheck's
      "partial-loads-OK" heuristic to apply.  Ensure that a page boundary is
      crossed if and only if the real insn would have crossed it as well.
      Thus, if the bytes to load are fully contained in an aligned 16-byte
      chunk, load the whole 16-byte aligned chunk, and otherwise load 16 bytes
      from the unaligned address.  Then shift the loaded data left-aligned
      into the target vector register. */

   assign(maxIdx, maxIndex);
   assign(cappedMax, mkite(binop(Iop_CmpLT32U, mkexpr(maxIdx), mkU32(15)),
                           unop(Iop_32Uto64, mkexpr(maxIdx)), mkU64(15)));
   /* 'offset': addr's offset from last 16-byte aligned address
      'zeroed': number of bytes to be zeroed in the target vector
      'back': how much to subtract from addr before loading 16 bytes */
   assign(offset, binop(Iop_And64, mkexpr(addr), mkU64(15)));
   assign(zeroed, binop(Iop_Sub64, mkU64(15), mkexpr(cappedMax)));
   assign(back, mkite(binop(Iop_CmpLE64U, mkexpr(offset), mkexpr(zeroed)),
                      mkexpr(offset), mkU64(0)));

   /* How much to shift the loaded 16-byte vector to the right, and then to
      the left.  Since both 'zeroed' and 'back' range from 0 to 15, the shift
      amounts range from 0 to 120. */
   IRExpr *shrAmount = binop(Iop_Shl64,
                             binop(Iop_Sub64, mkexpr(zeroed), mkexpr(back)),
                             mkU8(3));
   IRExpr *shlAmount = binop(Iop_Shl64, mkexpr(zeroed), mkU8(3));

   put_vr_qw(v1, binop(Iop_ShlV128,
                       binop(Iop_ShrV128,
                             load(Ity_V128,
                                  binop(Iop_Sub64, mkexpr(addr), mkexpr(back))),
                             unop(Iop_64to8, shrAmount)),
                       unop(Iop_64to8, shlAmount)));
}

/* Bitwise vCond ? v1 : v2
   All args are V128.
 */
static IRExpr*
s390_V128_bitwiseITE(IRExpr* vCond, IRExpr* v1, IRExpr* v2)
{
   IRTemp vc = newTemp(Ity_V128);
   assign(vc, vCond);
   /* result = (v1 & vCond) | (v2 & ~vCond) */
   return binop(Iop_OrV128,
                binop(Iop_AndV128, v1, mkexpr(vc)),
                binop(Iop_AndV128, v2, unop(Iop_NotV128, mkexpr(vc))));
}

/*------------------------------------------------------------*/
/*--- Rounding modes                                       ---*/
/*------------------------------------------------------------*/

/* Extract the bfp rounding mode from the guest FPC reg and encode it as an
   IRRoundingMode:

   rounding mode | s390 | IR
   -------------------------
   to nearest    |  00  | 00
   to zero       |  01  | 11
   to +infinity  |  10  | 10
   to -infinity  |  11  | 01

   So:  IR = (4 - s390) & 3
*/
static IRExpr *
get_bfp_rounding_mode_from_fpc(void)
{
   IRTemp fpc_bits = newTemp(Ity_I32);

   /* For z196 and later the bfp rounding mode is stored in bits [29:31].
      Prior to that bits [30:31] contained the bfp rounding mode with
      bit 29 being unused and having a value of 0. So we can always
      extract the least significant 3 bits. */
   assign(fpc_bits, binop(Iop_And32, get_fpc_w0(), mkU32(7)));

   /* fixs390:


      if (! s390_host_has_fpext && rounding_mode > 3) {
         emulation warning @ runtime and
         set fpc to round nearest
      }
   */

   /* For now silently adjust an unsupported rounding mode to "nearest" */
   IRExpr *rm_s390 = mkite(binop(Iop_CmpLE32S, mkexpr(fpc_bits), mkU32(3)),
                           mkexpr(fpc_bits),
                           mkU32(S390_FPC_BFP_ROUND_NEAREST_EVEN));

   // rm_IR = (4 - rm_s390) & 3;
   return binop(Iop_And32, binop(Iop_Sub32, mkU32(4), rm_s390), mkU32(3));
}

/* Encode the s390 rounding mode as it appears in the m3 field of certain
   instructions to VEX's IRRoundingMode. Rounding modes that cannot be
   represented in VEX are converted to Irrm_NEAREST. The rationale is, that
   Irrm_NEAREST refers to IEEE 754's roundTiesToEven which the standard
   considers the default rounding mode (4.3.3). */
static IRTemp
encode_bfp_rounding_mode(UChar mode)
{
   IRExpr *rm;

   switch (mode) {
   case S390_BFP_ROUND_PER_FPC:
      rm = get_bfp_rounding_mode_from_fpc();
      break;
   case S390_BFP_ROUND_NEAREST_AWAY:  rm = mkU32(Irrm_NEAREST_TIE_AWAY_0); break;
   case S390_BFP_ROUND_PREPARE_SHORT: rm = mkU32(Irrm_PREPARE_SHORTER); break;
   case S390_BFP_ROUND_NEAREST_EVEN:  rm = mkU32(Irrm_NEAREST); break;
   case S390_BFP_ROUND_ZERO:          rm = mkU32(Irrm_ZERO);    break;
   case S390_BFP_ROUND_POSINF:        rm = mkU32(Irrm_PosINF);  break;
   case S390_BFP_ROUND_NEGINF:        rm = mkU32(Irrm_NegINF);  break;
   default:
      vpanic("encode_bfp_rounding_mode");
   }

   return mktemp(Ity_I32, rm);
}

/* Extract the DFP rounding mode from the guest FPC reg and encode it as an
   IRRoundingMode:

   rounding mode                     | s390  | IR
   ------------------------------------------------
   to nearest, ties to even          |  000  | 000
   to zero                           |  001  | 011
   to +infinity                      |  010  | 010
   to -infinity                      |  011  | 001
   to nearest, ties away from 0      |  100  | 100
   to nearest, ties toward 0         |  101  | 111
   to away from 0                    |  110  | 110
   to prepare for shorter precision  |  111  | 101

   So:  IR = (s390 ^ ((s390 << 1) & 2))
*/
static IRExpr *
get_dfp_rounding_mode_from_fpc(void)
{
   IRTemp fpc_bits = newTemp(Ity_I32);

   /* The dfp rounding mode is stored in bits [25:27].
      extract the bits at 25:27 and right shift 4 times. */
   assign(fpc_bits, binop(Iop_Shr32,
                          binop(Iop_And32, get_fpc_w0(), mkU32(0x70)),
                          mkU8(4)));

   IRExpr *rm_s390 = mkexpr(fpc_bits);
   // rm_IR = (rm_s390 ^ ((rm_s390 << 1) & 2));

   return binop(Iop_Xor32, rm_s390,
                binop( Iop_And32,
                       binop(Iop_Shl32, rm_s390, mkU8(1)),
                       mkU32(2)));
}

/* Encode the s390 rounding mode as it appears in the m3 field of certain
   instructions to VEX's IRRoundingMode. */
static IRTemp
encode_dfp_rounding_mode(UChar mode)
{
   IRExpr *rm;

   switch (mode) {
   case S390_DFP_ROUND_PER_FPC_0:
   case S390_DFP_ROUND_PER_FPC_2:
      rm = get_dfp_rounding_mode_from_fpc(); break;
   case S390_DFP_ROUND_NEAREST_EVEN_4:
   case S390_DFP_ROUND_NEAREST_EVEN_8:
      rm = mkU32(Irrm_NEAREST); break;
   case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_1:
   case S390_DFP_ROUND_NEAREST_TIE_AWAY_0_12:
      rm = mkU32(Irrm_NEAREST_TIE_AWAY_0); break;
   case S390_DFP_ROUND_PREPARE_SHORT_3:
   case S390_DFP_ROUND_PREPARE_SHORT_15:
      rm = mkU32(Irrm_PREPARE_SHORTER); break;
   case S390_DFP_ROUND_ZERO_5:
   case S390_DFP_ROUND_ZERO_9:
      rm = mkU32(Irrm_ZERO ); break;
   case S390_DFP_ROUND_POSINF_6:
   case S390_DFP_ROUND_POSINF_10:
      rm = mkU32(Irrm_PosINF); break;
   case S390_DFP_ROUND_NEGINF_7:
   case S390_DFP_ROUND_NEGINF_11:
      rm = mkU32(Irrm_NegINF); break;
   case S390_DFP_ROUND_NEAREST_TIE_TOWARD_0:
      rm = mkU32(Irrm_NEAREST_TIE_TOWARD_0); break;
   case S390_DFP_ROUND_AWAY_0:
      rm = mkU32(Irrm_AWAY_FROM_ZERO); break;
   default:
      vpanic("encode_dfp_rounding_mode");
   }

   return mktemp(Ity_I32, rm);
}


/*------------------------------------------------------------*/
/*--- Condition code helpers                               ---*/
/*------------------------------------------------------------*/

/* The result of a Iop_CmpFxx operation is a condition code. It is
   encoded using the values defined in type IRCmpFxxResult.
   Before we can store the condition code into the guest state (or do
   anything else with it for that matter) we need to convert it to
   the encoding that s390 uses. This is what this function does.

   s390     VEX                b6 b2 b0   cc.1  cc.0
   0      0x40 EQ             1  0  0     0     0
   1      0x01 LT             0  0  1     0     1
   2      0x00 GT             0  0  0     1     0
   3      0x45 Unordered      1  1  1     1     1

   The following bits from the VEX encoding are interesting:
   b0, b2, b6  with b0 being the LSB. We observe:

   cc.0 = b0;
   cc.1 = b2 | (~b0 & ~b6)

   with cc being the s390 condition code.
*/
static IRExpr *
convert_vex_bfpcc_to_s390(IRTemp vex_cc)
{
   IRTemp cc0  = newTemp(Ity_I32);
   IRTemp cc1  = newTemp(Ity_I32);
   IRTemp b0   = newTemp(Ity_I32);
   IRTemp b2   = newTemp(Ity_I32);
   IRTemp b6   = newTemp(Ity_I32);

   assign(b0, binop(Iop_And32, mkexpr(vex_cc), mkU32(1)));
   assign(b2, binop(Iop_And32, binop(Iop_Shr32, mkexpr(vex_cc), mkU8(2)),
                    mkU32(1)));
   assign(b6, binop(Iop_And32, binop(Iop_Shr32, mkexpr(vex_cc), mkU8(6)),
                    mkU32(1)));

   assign(cc0, mkexpr(b0));
   assign(cc1, binop(Iop_Or32, mkexpr(b2),
                     binop(Iop_And32,
                           binop(Iop_Sub32, mkU32(1), mkexpr(b0)), /* ~b0 */
                           binop(Iop_Sub32, mkU32(1), mkexpr(b6))  /* ~b6 */
                           )));

   return binop(Iop_Or32, mkexpr(cc0), binop(Iop_Shl32, mkexpr(cc1), mkU8(1)));
}


/* The result of a Iop_CmpDxx operation is a condition code. It is
   encoded using the values defined in type IRCmpDxxResult.
   Before we can store the condition code into the guest state (or do
   anything else with it for that matter) we need to convert it to
   the encoding that s390 uses. This is what this function does. */
static IRExpr *
convert_vex_dfpcc_to_s390(IRTemp vex_cc)
{
   /* The VEX encodings for IRCmpDxxResult and IRCmpFxxResult are the
      same. currently. */
   return convert_vex_bfpcc_to_s390(vex_cc);
}


/*------------------------------------------------------------*/
/*--- Build IR for formats                                 ---*/
/*------------------------------------------------------------*/
static void
s390_format_I(const HChar *(*irgen)(UChar i),
              UChar i)
{
   const HChar *mnm = irgen(i);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC2(MNM, UINT), mnm, i);
}

static void
s390_format_E(const HChar *(*irgen)(void))
{
   const HChar *mnm = irgen();

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC1(MNM), mnm);
}

static void
s390_format_RI(const HChar *(*irgen)(UChar r1, UShort i2),
               UChar r1, UShort i2)
{
   irgen(r1, i2);
}

static void
s390_format_RI_RU(const HChar *(*irgen)(UChar r1, UShort i2),
                  UChar r1, UShort i2)
{
   const HChar *mnm = irgen(r1, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, UINT), mnm, r1, i2);
}

static void
s390_format_RI_RI(const HChar *(*irgen)(UChar r1, UShort i2),
                  UChar r1, UShort i2)
{
   const HChar *mnm = irgen(r1, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, INT), mnm, r1, (Int)(Short)i2);
}

static void
s390_format_RI_RP(const HChar *(*irgen)(UChar r1, UShort i2),
                  UChar r1, UShort i2)
{
   const HChar *mnm = irgen(r1, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, PCREL), mnm, r1, (Int)(Short)i2);
}

static void
s390_format_RIE_RRP(const HChar *(*irgen)(UChar r1, UChar r3, UShort i2),
                    UChar r1, UChar r3, UShort i2)
{
   const HChar *mnm = irgen(r1, r3, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, GPR, PCREL), mnm, r1, r3, (Int)(Short)i2);
}

static void
s390_format_RIE_RRI0(const HChar *(*irgen)(UChar r1, UChar r3, UShort i2),
                     UChar r1, UChar r3, UShort i2)
{
   const HChar *mnm = irgen(r1, r3, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, GPR, INT), mnm, r1, r3, (Int)(Short)i2);
}

static void
s390_format_RIE_RRUUU(const HChar *(*irgen)(UChar r1, UChar r2, UChar i3,
                                            UChar i4, UChar i5),
                      UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   const HChar *mnm = irgen(r1, r2, i3, i4, i5);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC6(MNM, GPR, GPR, UINT, UINT, UINT), mnm, r1, r2, i3, i4,
                  i5);
}

static void
s390_format_RIEv1(const HChar *(*irgen)(UChar r1, UShort i2, UChar m3),
                  UChar r1, UShort i2, UChar m3)
{
   const HChar *mnm = irgen(r1, i2, m3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, UINT, UINT), mnm, r1, i2, m3);
}

static void
s390_format_RIE_RRPU(const HChar *(*irgen)(UChar r1, UChar r2, UShort i4,
                                           UChar m3),
                     UChar r1, UChar r2, UShort i4, UChar m3)
{
   const HChar *mnm = irgen(r1, r2, i4, m3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(XMNM, GPR, GPR, CABM, PCREL), S390_XMNM_CAB, mnm, m3, r1,
                  r2, m3, (Int)(Short)i4);
}

static void
s390_format_RIE_RUPU(const HChar *(*irgen)(UChar r1, UChar m3, UShort i4,
                                           UChar i2),
                     UChar r1, UChar m3, UShort i4, UChar i2)
{
   const HChar *mnm = irgen(r1, m3, i4, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(XMNM, GPR, UINT, CABM, PCREL), S390_XMNM_CAB, mnm, m3,
                  r1, i2, m3, (Int)(Short)i4);
}

static void
s390_format_RIE_RUPI(const HChar *(*irgen)(UChar r1, UChar m3, UShort i4,
                                           UChar i2),
                     UChar r1, UChar m3, UShort i4, UChar i2)
{
   const HChar *mnm = irgen(r1, m3, i4, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(XMNM, GPR, INT, CABM, PCREL), S390_XMNM_CAB, mnm, m3, r1,
                  (Int)(Char)i2, m3, (Int)(Short)i4);
}

static void
s390_format_RIE_RUPIX(const HChar *(*irgen)(UChar r1, UChar m3, UShort i4,
                                           UChar i2),
                     UChar r1, UChar m3, UShort i4, UChar i2, Int xmnm_kind)
{
   const HChar *mnm = irgen(r1, m3, i4, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(XMNM, GPR, INT, CABM, PCREL), xmnm_kind, mnm, m3, r1,
                  (Int)(Char)i2, m3, (Int)(Short)i4);
}

static void
s390_format_RIL(const HChar *(*irgen)(UChar r1, UInt i2),
                UChar r1, UInt i2)
{
   irgen(r1, i2);
}

static void
s390_format_RIL_RU(const HChar *(*irgen)(UChar r1, UInt i2),
                   UChar r1, UInt i2)
{
   const HChar *mnm = irgen(r1, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, UINT), mnm, r1, i2);
}

static void
s390_format_RIL_RI(const HChar *(*irgen)(UChar r1, UInt i2),
                   UChar r1, UInt i2)
{
   const HChar *mnm = irgen(r1, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, INT), mnm, r1, i2);
}

static void
s390_format_RIL_RP(const HChar *(*irgen)(UChar r1, UInt i2),
                   UChar r1, UInt i2)
{
   const HChar *mnm = irgen(r1, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, PCREL), mnm, r1, i2);
}

static void
s390_format_RIL_UP(const HChar *(*irgen)(void),
                   UChar r1, UInt i2)
{
   const HChar *mnm = irgen();

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, UINT, PCREL), mnm, r1, i2);
}

static void
s390_format_RIS_RURDI(const HChar *(*irgen)(UChar r1, UChar m3, UChar i2,
                      IRTemp op4addr),
                      UChar r1, UChar m3, UChar b4, UShort d4, UChar i2)
{
   const HChar *mnm;
   IRTemp op4addr = newTemp(Ity_I64);

   assign(op4addr, binop(Iop_Add64, mkU64(d4), b4 != 0 ? get_gpr_dw0(b4) :
          mkU64(0)));

   mnm = irgen(r1, m3, i2, op4addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(XMNM, GPR, INT, CABM, UDXB), S390_XMNM_CAB, mnm, m3, r1,
                  (Int)(Char)i2, m3, d4, 0, b4);
}

static void
s390_format_RIS_RURDU(const HChar *(*irgen)(UChar r1, UChar m3, UChar i2,
                      IRTemp op4addr),
                      UChar r1, UChar m3, UChar b4, UShort d4, UChar i2)
{
   const HChar *mnm;
   IRTemp op4addr = newTemp(Ity_I64);

   assign(op4addr, binop(Iop_Add64, mkU64(d4), b4 != 0 ? get_gpr_dw0(b4) :
          mkU64(0)));

   mnm = irgen(r1, m3, i2, op4addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(XMNM, GPR, UINT, CABM, UDXB), S390_XMNM_CAB, mnm, m3, r1,
                  i2, m3, d4, 0, b4);
}

static void
s390_format_RR(const HChar *(*irgen)(UChar r1, UChar r2),
               UChar r1, UChar r2)
{
   irgen(r1, r2);
}

static void
s390_format_RR_RR(const HChar *(*irgen)(UChar r1, UChar r2),
                  UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, GPR), mnm, r1, r2);
}

static void
s390_format_RR_FF(const HChar *(*irgen)(UChar r1, UChar r2),
                  UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, FPR, FPR), mnm, r1, r2);
}

static void
s390_format_RRE(const HChar *(*irgen)(UChar r1, UChar r2),
                UChar r1, UChar r2)
{
   irgen(r1, r2);
}

static void
s390_format_RRE_RR(const HChar *(*irgen)(UChar r1, UChar r2),
                   UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, GPR), mnm, r1, r2);
}

static void
s390_format_RRE_FF(const HChar *(*irgen)(UChar r1, UChar r2),
                   UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, FPR, FPR), mnm, r1, r2);
}

static void
s390_format_RRE_RF(const HChar *(*irgen)(UChar, UChar),
                   UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, FPR), mnm, r1, r2);
}

static void
s390_format_RRE_FR(const HChar *(*irgen)(UChar r1, UChar r2),
                   UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, FPR, GPR), mnm, r1, r2);
}

static void
s390_format_RRE_R0(const HChar *(*irgen)(UChar r1),
                   UChar r1)
{
   const HChar *mnm = irgen(r1);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC2(MNM, GPR), mnm, r1);
}

static void
s390_format_RRE_F0(const HChar *(*irgen)(UChar r1),
                   UChar r1)
{
   const HChar *mnm = irgen(r1);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC2(MNM, FPR), mnm, r1);
}

static void
s390_format_RRF_M0RERE(const HChar *(*irgen)(UChar m3, UChar r1, UChar r2),
                       UChar m3, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(m3, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, GPR, UINT), mnm, r1, r2, m3);
}

static void
s390_format_RRF_F0FF(const HChar *(*irgen)(UChar, UChar, UChar),
                     UChar r1, UChar r3, UChar r2)
{
   const HChar *mnm = irgen(r1, r3, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, FPR, FPR, FPR), mnm, r1, r3, r2);
}

static void
s390_format_RRF_F0FR(const HChar *(*irgen)(UChar, UChar, UChar),
                     UChar r3, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r3, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, FPR, FPR, GPR), mnm, r1, r3, r2);
}

static void
s390_format_RRF_UUFF(const HChar *(*irgen)(UChar m3, UChar m4, UChar r1,
                                           UChar r2),
                     UChar m3, UChar m4, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(m3, m4, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, FPR, UINT, FPR, UINT), mnm, r1, m3, r2, m4);
}

static void
s390_format_RRF_0UFF(const HChar *(*irgen)(UChar m4, UChar r1, UChar r2),
                     UChar m4, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(m4, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, FPR, FPR, UINT), mnm, r1, r2, m4);
}

static void
s390_format_RRF_UUFR(const HChar *(*irgen)(UChar m3, UChar m4, UChar r1,
                                           UChar r2),
                     UChar m3, UChar m4, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(m3, m4, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, FPR, UINT, GPR, UINT), mnm, r1, m3, r2, m4);
}

static void
s390_format_RRF_UURF(const HChar *(*irgen)(UChar m3, UChar m4, UChar r1,
                                           UChar r2),
                     UChar m3, UChar m4, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(m3, m4, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, GPR, UINT, FPR, UINT), mnm, r1, m3, r2, m4);
}


static void
s390_format_RRF_U0RR(const HChar *(*irgen)(UChar m3, UChar r1, UChar r2),
                     UChar m3, UChar r1, UChar r2, Int xmnm_kind)
{
   irgen(m3, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(XMNM, GPR, GPR), xmnm_kind, m3, r1, r2);
}

static void
s390_format_RRF_F0FF2(const HChar *(*irgen)(UChar, UChar, UChar),
                      UChar r3, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r3, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, FPR, FPR, FPR), mnm, r1, r3, r2);
}

static void
s390_format_RRF_FFRU(const HChar *(*irgen)(UChar, UChar, UChar, UChar),
                     UChar r3, UChar m4, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r3, m4, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, FPR, FPR, GPR, UINT), mnm, r1, r3, r2, m4);
}

static void
s390_format_RRF_FUFF(const HChar *(*irgen)(UChar, UChar, UChar, UChar),
                     UChar r3, UChar m4, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r3, m4, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), mnm, r1, r3, r2, m4);
}

static void
s390_format_RRF_FUFF2(const HChar *(*irgen)(UChar, UChar, UChar, UChar),
                      UChar r3, UChar m4, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r3, m4, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, FPR, FPR, FPR, UINT), mnm, r1, r2, r3, m4);
}

static void
s390_format_RRF_R0RR2(const HChar *(*irgen)(UChar r3, UChar r1, UChar r2),
                      UChar r3, UChar r1, UChar r2)
{
   const HChar *mnm = irgen(r3, r1, r2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, GPR, GPR), mnm, r1, r2, r3);
}

static void
s390_format_RRS(const HChar *(*irgen)(UChar r1, UChar r2, UChar m3,
                                      IRTemp op4addr),
                UChar r1, UChar r2, UChar b4, UShort d4, UChar m3)
{
   const HChar *mnm;
   IRTemp op4addr = newTemp(Ity_I64);

   assign(op4addr, binop(Iop_Add64, mkU64(d4), b4 != 0 ? get_gpr_dw0(b4) :
          mkU64(0)));

   mnm = irgen(r1, r2, m3, op4addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(XMNM, GPR, GPR, CABM, UDXB), S390_XMNM_CAB, mnm, m3, r1,
                  r2, m3, d4, 0, b4);
}

static void
s390_format_RS_R0RD(const HChar *(*irgen)(UChar r1, IRTemp op2addr),
                    UChar r1, UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(r1, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, UDXB), mnm, r1, d2, 0, b2);
}

static void
s390_format_RS_RRRD(const HChar *(*irgen)(UChar r1, UChar r3, IRTemp op2addr),
                    UChar r1, UChar r3, UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(r1, r3, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, GPR, UDXB), mnm, r1, r3, d2, 0, b2);
}

static void
s390_format_RS_RURD(const HChar *(*irgen)(UChar r1, UChar r3, IRTemp op2addr),
                    UChar r1, UChar r3, UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(r1, r3, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, UINT, UDXB), mnm, r1, r3, d2, 0, b2);
}

static void
s390_format_RS_AARD(const HChar *(*irgen)(UChar, UChar, IRTemp),
                    UChar r1, UChar r3, UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(r1, r3, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, AR, AR, UDXB), mnm, r1, r3, d2, 0, b2);
}

static void
s390_format_RSI_RRP(const HChar *(*irgen)(UChar r1, UChar r3, UShort i2),
                    UChar r1, UChar r3, UShort i2)
{
   const HChar *mnm = irgen(r1, r3, i2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, GPR, PCREL), mnm, r1, r3, (Int)(Short)i2);
}

static void
s390_format_RSY_RRRD(const HChar *(*irgen)(UChar r1, UChar r3, IRTemp op2addr),
                     UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2 = newTemp(Ity_I64);

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, mkexpr(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(r1, r3, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, GPR, SDXB), mnm, r1, r3, dh2, dl2, 0, b2);
}

static void
s390_format_RSY_AARD(const HChar *(*irgen)(UChar, UChar, IRTemp),
                     UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2 = newTemp(Ity_I64);

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, mkexpr(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(r1, r3, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, AR, AR, SDXB), mnm, r1, r3, dh2, dl2, 0, b2);
}

static void
s390_format_RSY_RURD(const HChar *(*irgen)(UChar r1, UChar r3, IRTemp op2addr),
                     UChar r1, UChar r3, UChar b2, UShort dl2, UChar dh2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2 = newTemp(Ity_I64);

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, mkexpr(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(r1, r3, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, GPR, UINT, SDXB), mnm, r1, r3, dh2, dl2, 0, b2);
}

static void
s390_format_RSY_RDRM(const HChar *(*irgen)(UChar r1, IRTemp op2addr),
                     UChar r1, UChar m3, UChar b2, UShort dl2, UChar dh2,
                     Int xmnm_kind)
{
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2 = newTemp(Ity_I64);

   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, mkexpr(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(r1, op2addr);

   vassert(dis_res->whatNext == Dis_Continue);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(XMNM, GPR, SDXB), xmnm_kind, m3, r1, dh2, dl2, 0, b2);
}

static void
s390_format_RX(const HChar *(*irgen)(UChar r1, UChar x2, UChar b2, UShort d2,
               IRTemp op2addr),
               UChar r1, UChar x2, UChar b2, UShort d2)
{
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   irgen(r1, x2, b2, d2, op2addr);
}

static void
s390_format_RX_RRRD(const HChar *(*irgen)(UChar r1, IRTemp op2addr),
                    UChar r1, UChar x2, UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   mnm = irgen(r1, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, UDXB), mnm, r1, d2, x2, b2);
}

static void
s390_format_RX_FRRD(const HChar *(*irgen)(UChar r1, IRTemp op2addr),
                    UChar r1, UChar x2, UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   mnm = irgen(r1, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, FPR, UDXB), mnm, r1, d2, x2, b2);
}

static void
s390_format_RXE_FRRD(const HChar *(*irgen)(UChar r1, IRTemp op2addr),
                     UChar r1, UChar x2, UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   mnm = irgen(r1, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, FPR, UDXB), mnm, r1, d2, x2, b2);
}

static void
s390_format_RXE_RRRDR(const HChar *(*irgen)(UChar r1, IRTemp op2addr, UChar m3),
                     UChar r1, UChar x2, UChar b2, UShort d2, UChar m3)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   mnm = irgen(r1, op2addr, m3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, UDXB), mnm, r1, d2, x2, b2);
}

static void
s390_format_RXF_FRRDF(const HChar *(*irgen)(UChar, IRTemp, UChar),
                      UChar r3, UChar x2, UChar b2, UShort d2, UChar r1)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   mnm = irgen(r3, op2addr, r1);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, FPR, FPR, UDXB), mnm, r1, r3, d2, x2, b2);
}

static void
s390_format_RXY_RRRD(const HChar *(*irgen)(UChar r1, IRTemp op2addr),
                     UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2 = newTemp(Ity_I64);

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkexpr(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   mnm = irgen(r1, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE)) {
      if (irgen == s390_irgen_BIC)
         s390_disasm(ENC2(XMNM, SDXB), S390_XMNM_BIC, r1, dh2, dl2, x2, b2);
      else
         s390_disasm(ENC3(MNM, GPR, SDXB), mnm, r1, dh2, dl2, x2, b2);
   }
}

static void
s390_format_RXY_FRRD(const HChar *(*irgen)(UChar r1, IRTemp op2addr),
                     UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2 = newTemp(Ity_I64);

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkexpr(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   mnm = irgen(r1, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, FPR, SDXB), mnm, r1, dh2, dl2, x2, b2);
}

static void
s390_format_RXY_URRD(const HChar *(*irgen)(void),
                     UChar r1, UChar x2, UChar b2, UShort dl2, UChar dh2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2 = newTemp(Ity_I64);

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkexpr(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   mnm = irgen();

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, UINT, SDXB), mnm, r1, dh2, dl2, x2, b2);
}

static void
s390_format_S_RD(const HChar *(*irgen)(IRTemp op2addr),
                 UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC2(MNM, UDXB), mnm, d2, 0, b2);
}

static void
s390_format_SI_URD(const HChar *(*irgen)(UChar i2, IRTemp op1addr),
                   UChar i2, UChar b1, UShort d1)
{
   const HChar *mnm;
   IRTemp op1addr = newTemp(Ity_I64);

   assign(op1addr, binop(Iop_Add64, mkU64(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));

   mnm = irgen(i2, op1addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, UDXB, UINT), mnm, d1, 0, b1, i2);
}

static void
s390_format_SIY_URD(const HChar *(*irgen)(UChar i2, IRTemp op1addr),
                    UChar i2, UChar b1, UShort dl1, UChar dh1)
{
   const HChar *mnm;
   IRTemp op1addr = newTemp(Ity_I64);
   IRTemp d1 = newTemp(Ity_I64);

   assign(d1, mkU64(((ULong)(Long)(Char)dh1 << 12) | ((ULong)dl1)));
   assign(op1addr, binop(Iop_Add64, mkexpr(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));

   mnm = irgen(i2, op1addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, SDXB, UINT), mnm, dh1, dl1, 0, b1, i2);
}

static void
s390_format_SIY_IRD(const HChar *(*irgen)(UChar i2, IRTemp op1addr),
                    UChar i2, UChar b1, UShort dl1, UChar dh1)
{
   const HChar *mnm;
   IRTemp op1addr = newTemp(Ity_I64);
   IRTemp d1 = newTemp(Ity_I64);

   assign(d1, mkU64(((ULong)(Long)(Char)dh1 << 12) | ((ULong)dl1)));
   assign(op1addr, binop(Iop_Add64, mkexpr(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));

   mnm = irgen(i2, op1addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, SDXB, INT), mnm, dh1, dl1, 0, b1, (Int)(Char)i2);
}

static void
s390_format_SS_L0RDRD(const HChar *(*irgen)(UChar, IRTemp, IRTemp),
                      UChar l, UChar b1, UShort d1, UChar b2, UShort d2)
{
   const HChar *mnm;
   IRTemp op1addr = newTemp(Ity_I64);
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op1addr, binop(Iop_Add64, mkU64(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));
   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   mnm = irgen(l, op1addr, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, UDLB, UDXB), mnm, d1, l, b1, d2, 0, b2);
}

static void
s390_format_SIL_RDI(const HChar *(*irgen)(UShort i2, IRTemp op1addr),
                    UChar b1, UShort d1, UShort i2)
{
   const HChar *mnm;
   IRTemp op1addr = newTemp(Ity_I64);

   assign(op1addr, binop(Iop_Add64, mkU64(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));

   mnm = irgen(i2, op1addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, UDXB, INT), mnm, d1, 0, b1, (Int)(Short)i2);
}

static void
s390_format_SIL_RDU(const HChar *(*irgen)(UShort i2, IRTemp op1addr),
                    UChar b1, UShort d1, UShort i2)
{
   const HChar *mnm;
   IRTemp op1addr = newTemp(Ity_I64);

   assign(op1addr, binop(Iop_Add64, mkU64(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));

   mnm = irgen(i2, op1addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, UDXB, UINT), mnm, d1, 0, b1, i2);
}

static void
s390_format_VRX_VRRD(const HChar *(*irgen)(UChar v1, IRTemp op2addr),
                    UChar v1, UChar x2, UChar b2, UShort d2, UChar rxb)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   mnm = irgen(v1, op2addr);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, VR, UDXB), mnm, v1, d2, x2, b2);
}


static void
s390_format_VRX_VRRDM(const HChar *(*irgen)(UChar v1, IRTemp op2addr, UChar m3),
                    UChar v1, UChar x2, UChar b2, UShort d2, UChar m3, UChar rxb)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   mnm = irgen(v1, op2addr, m3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, VR, UDXB), mnm, v1, d2, x2, b2);
}


static void
s390_format_VRR_VV(const HChar *(*irgen)(UChar v1, UChar v2),
                    UChar v1, UChar v2, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   mnm = irgen(v1, v2);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, VR, VR), mnm, v1, v2);
}


static void
s390_format_VRR_VVV(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3),
                    UChar v1, UChar v2, UChar v3, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   mnm = irgen(v1, v2, v3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, VR, VR, VR), mnm, v1, v2, v3);
}


static void
s390_format_VRR_VVVM(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3, UChar m4),
                    UChar v1, UChar v2, UChar v3, UChar m4, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   mnm = irgen(v1, v2, v3, m4);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), mnm, v1, v2, v3, m4);
}


static void
s390_format_VRR_VVVMM(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5),
                    UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   mnm = irgen(v1, v2, v3, m4, m5);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), mnm, v1, v2, v3, m4, m5);
}


static void
s390_format_VRR_VVVV(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3, UChar v4),
                    UChar v1, UChar v2, UChar v3, UChar v4, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   v4  = s390_vr_getVRindex(v4, 4, rxb);
   mnm = irgen(v1, v2, v3, v4);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, VR, VR, VR, VR), mnm, v1, v2, v3, v4);
}


static void
s390_format_VRR_VRR(const HChar *(*irgen)(UChar v1, UChar r2, UChar r3),
                    UChar v1, UChar r2, UChar r3, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   mnm = irgen(v1, r2, r3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, VR, GPR, GPR), mnm, v1, r2, r3);
}


static void
s390_format_VRR_VVM(const HChar *(*irgen)(UChar v1, UChar v2, UChar m3),
                    UChar v1, UChar v2, UChar m3, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   mnm = irgen(v1, v2, m3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, VR, VR, UINT), mnm, v1, v2, m3);
}


static void
s390_format_VRI_VIM(const HChar *(*irgen)(UChar v1, UShort i2, UChar m3),
                    UChar v1, UShort i2, UChar m3, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   mnm = irgen(v1, i2, m3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, VR, UINT, UINT), mnm, v1, i2, m3);
}


static void
s390_format_VRI_VVIM(const HChar *(*irgen)(UChar v1, UChar v3, UShort i2, UChar m4),
                    UChar v1, UChar v3, UShort i2, UChar m4, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v3  = s390_vr_getVRindex(v3, 2, rxb);
   mnm = irgen(v1, v3, i2, m4);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, VR, VR, UINT, UINT), mnm, v1, v3, i2, m4);
}

static void
s390_format_VRI_VVIMM(const HChar *(*irgen)(UChar v1, UChar v2, UShort i3,
                                            UChar m4, UChar m5),
                      UChar v1, UChar v2, UShort i3, UChar m4, UChar m5,
                      UChar rxb)
{
   const HChar *mnm;

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   mnm = irgen(v1, v2, i3, m4, m5);

   if (vex_traceflags & VEX_TRACE_FE)
      s390_disasm(ENC6(MNM, VR, VR, UINT, UINT, UINT), mnm, v1, v2, i3, m4, m5);
}

static void
s390_format_VRS_RRDVM(const HChar *(*irgen)(UChar r1, IRTemp op2addr, UChar v3,
                      UChar m4), UChar r1, UChar b2, UShort d2, UChar v3,
                      UChar m4, UChar rxb)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v3  = s390_vr_getVRindex(v3, 2, rxb);
   mnm = irgen(r1, op2addr, v3, m4);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, GPR, UDXB, VR, UINT), mnm, r1, d2, 0, b2, v3, m4);
}


static void
s390_format_VRS_VRDVM(const HChar *(*irgen)(UChar v1, IRTemp op2addr, UChar v3,
                      UChar m4), UChar v1, UChar b2, UShort d2, UChar v3,
                      UChar m4, UChar rxb)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v3  = s390_vr_getVRindex(v3, 2, rxb);
   mnm = irgen(v1, op2addr, v3, m4);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, VR, UDXB, VR, UINT), mnm, v1, d2, 0, b2, v3, m4);
}


static void
s390_format_VRS_VRDV(const HChar *(*irgen)(UChar v1, IRTemp op2addr, UChar v3),
                     UChar v1, UChar b2, UShort d2, UChar v3, UChar rxb)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v3  = s390_vr_getVRindex(v3, 2, rxb);
   mnm = irgen(v1, op2addr, v3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, VR, UDXB, VR), mnm, v1, d2, 0, b2, v3);
}


static void
s390_format_VRS_VRRDM(const HChar *(*irgen)(UChar v1, IRTemp op2addr, UChar r3,
                     UChar m4),
                     UChar v1, UChar b2, UShort d2, UChar r3, UChar m4, UChar rxb)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   mnm = irgen(v1, op2addr, r3, m4);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, VR, GPR, UDXB, UINT), mnm, v1, r3, d2, 0, b2, m4);
}


static void
s390_format_VRS_VRRD(const HChar *(*irgen)(UChar v1, IRTemp op2addr, UChar r3),
                     UChar v1, UChar b2, UShort d2, UChar r3, UChar rxb)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   mnm = irgen(v1, op2addr, r3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, VR, GPR, UDXB), mnm, v1, r3, d2, 0, b2);
}


static void
s390_format_VRV_VVRDMT(const HChar *(*irgen)(UChar v1, IRTemp op2addr, UChar m3),
                       UChar v1, UChar v2, UChar b2, UShort d2, UChar m3, UChar rxb,
                       IRType type)
{
   const HChar *mnm;
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);

   vassert(type == Ity_I32 || type == Ity_I64);
   IRExpr *x2;
   if(type == Ity_I32) {
      x2 = unop(Iop_32Uto64, get_vr(v2, type, m3));
   } else {
      x2 = get_vr(v2, type, m3);
   }

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2));

   mnm = irgen(v1, op2addr, m3);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC4(MNM, VR, UDVB, UINT), mnm, v1, d2, v2, b2, m3);
}


static void
s390_format_VRR_VVVVMM(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3,
                                              UChar v4, UChar m5, UChar m6),
                        UChar v1, UChar v2, UChar v3, UChar v4, UChar m5,
                        UChar m6, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   v4  = s390_vr_getVRindex(v4, 4, rxb);
   mnm = irgen(v1, v2, v3, v4, m5, m6);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC7(MNM, VR, VR, VR, VR, UINT, UINT),
                  mnm, v1, v2, v3, v4, m5, m6);
}


static void
s390_format_VRR_VVMM(const HChar *(*irgen)(UChar v1, UChar v2, UChar m3,
                                           UChar m5),
                     UChar v1, UChar v2, UChar m3, UChar m5, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   mnm = irgen(v1, v2, m3, m5);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, VR, VR, UINT, UINT), mnm, v1, v2, m3, m5);
}


static void
s390_format_VRId_VVVIM(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3,
                                             UChar i4, UChar m5),
                       UChar v1, UChar v2, UChar v3, UChar i4, UChar m5,
                       UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   mnm = irgen(v1, v2, v3, i4, m5);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), mnm, v1, v2, v3, i4, m5);
}


static void
s390_format_VRId_VVVI(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3,
                                            UChar i4),
                      UChar v1, UChar v2, UChar v3, UChar i4, UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   mnm = irgen(v1, v2, v3, i4);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC5(MNM, VR, VR, VR, UINT), mnm, v1, v2, v3, i4);
}


static void
s390_format_VRRd_VVVVM(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3,
                                             UChar v4, UChar m5),
                       UChar v1, UChar v2, UChar v3, UChar v4, UChar m5,
                       UChar rxb)
{
   const HChar *mnm;

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   v4  = s390_vr_getVRindex(v4, 4, rxb);
   mnm = irgen(v1, v2, v3, v4, m5);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC6(MNM, VR, VR, VR, VR, UINT), mnm, v1, v2, v3, v4, m5);
}


static void
s390_format_VRRa_VVMMM(const HChar *(*irgen)(UChar v1, UChar v2, UChar m3,
                                             UChar m4, UChar m5),
                       UChar v1, UChar v2, UChar m3, UChar m4, UChar m5,
                       UChar rxb)
{
   const HChar *mnm;

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   mnm = irgen(v1, v2, m3, m4, m5);

   if (vex_traceflags & VEX_TRACE_FE)
      s390_disasm(ENC6(MNM, VR, VR, UINT, UINT, UINT), mnm, v1, v2, m3, m4, m5);
}

static void
s390_format_VRRa_VVVMM(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3,
                                             UChar m4, UChar m5),
                       UChar v1, UChar v2, UChar v3, UChar m4, UChar m5,
                       UChar rxb)
{
   const HChar *mnm;

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   v3 = s390_vr_getVRindex(v3, 3, rxb);
   mnm = irgen(v1, v2, v3, m4, m5);

   if (vex_traceflags & VEX_TRACE_FE)
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT), mnm, v1, v2, v3, m4, m5);
}

static void
s390_format_VRRa_VVMM(const HChar *(*irgen)(UChar v1, UChar v2, UChar m3,
                                            UChar m4),
                       UChar v1, UChar v2, UChar m3, UChar m4, UChar rxb)
{
   const HChar *mnm;

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   mnm = irgen(v1, v2, m3, m4);

   if (vex_traceflags & VEX_TRACE_FE)
      s390_disasm(ENC5(MNM, VR, VR, UINT, UINT), mnm, v1, v2, m3, m4);
}

static void
s390_format_VRRa_VVVMMM(const HChar *(*irgen)(UChar v1, UChar v2, UChar v3,
                                              UChar m4, UChar m5, UChar m6),
                        UChar v1, UChar v2, UChar v3, UChar m4, UChar m5,
                        UChar m6, UChar rxb)
{
   const HChar *mnm;

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   v3 = s390_vr_getVRindex(v3, 3, rxb);
   mnm = irgen(v1, v2, v3, m4, m5, m6);

   if (vex_traceflags & VEX_TRACE_FE)
      s390_disasm(ENC6(MNM, VR, VR, VR, UINT, UINT),
                  mnm, v1, v2, v3, m4, m5, m6);
}

/*------------------------------------------------------------*/
/*--- Build IR for opcodes                                 ---*/
/*------------------------------------------------------------*/

static const HChar *
s390_irgen_AR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "ar";
}

static const HChar *
s390_irgen_AGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "agr";
}

static const HChar *
s390_irgen_AGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "agfr";
}

static const HChar *
s390_irgen_ARK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Add32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op2, op3);
   put_gpr_w1(r1, mkexpr(result));

   return "ark";
}

static const HChar *
s390_irgen_AGRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Add64, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op2, op3);
   put_gpr_dw0(r1, mkexpr(result));

   return "agrk";
}

static const HChar *
s390_irgen_A(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "a";
}

static const HChar *
s390_irgen_AY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "ay";
}

static const HChar *
s390_irgen_AG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "ag";
}

static const HChar *
s390_irgen_AGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "agf";
}

static const HChar *
s390_irgen_AFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   op2 = (Int)i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32((UInt)op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));
   put_gpr_w1(r1, mkexpr(result));

   return "afi";
}

static const HChar *
s390_irgen_AGFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   op2 = (Long)(Int)i2;
   assign(result, binop(Iop_Add64, mkexpr(op1), mkU64((ULong)op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));
   put_gpr_dw0(r1, mkexpr(result));

   return "agfi";
}

static const HChar *
s390_irgen_AHIK(UChar r1, UChar r3, UShort i2)
{
   Int op2;
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   op2 = (Int)(Short)i2;
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Add32, mkU32((UInt)op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, mktemp(Ity_I32, mkU32((UInt)
                       op2)), op3);
   put_gpr_w1(r1, mkexpr(result));

   return "ahik";
}

static const HChar *
s390_irgen_AGH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_16Sto64, load(Ity_I16, mkexpr(op2addr))));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "agh";
}

static const HChar *
s390_irgen_AGHIK(UChar r1, UChar r3, UShort i2)
{
   Long op2;
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   op2 = (Long)(Short)i2;
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Add64, mkU64((ULong)op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, mktemp(Ity_I64, mkU64((ULong)
                       op2)), op3);
   put_gpr_dw0(r1, mkexpr(result));

   return "aghik";
}

static const HChar *
s390_irgen_ASI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, load(Ity_I32, mkexpr(op1addr)));
   op2 = (Int)(Char)i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32((UInt)op2)));
   store(mkexpr(op1addr), mkexpr(result));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));

   return "asi";
}

static const HChar *
s390_irgen_AGSI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;
   IRTemp result = newTemp(Ity_I64);

   assign(op1, load(Ity_I64, mkexpr(op1addr)));
   op2 = (Long)(Char)i2;
   assign(result, binop(Iop_Add64, mkexpr(op1), mkU64((ULong)op2)));
   store(mkexpr(op1addr), mkexpr(result));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));

   return "agsi";
}

static const HChar *
s390_irgen_AH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
   assign(result, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "ah";
}

static const HChar *
s390_irgen_AHY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
   assign(result, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "ahy";
}

static const HChar *
s390_irgen_AHI(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   op2 = (Int)(Short)i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32((UInt)op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));
   put_gpr_w1(r1, mkexpr(result));

   return "ahi";
}

static const HChar *
s390_irgen_AGHI(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   op2 = (Long)(Short)i2;
   assign(result, binop(Iop_Add64, mkexpr(op1), mkU64((ULong)op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));
   put_gpr_dw0(r1, mkexpr(result));

   return "aghi";
}

static const HChar *
s390_irgen_AHHHR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r2));
   assign(op3, get_gpr_w0(r3));
   assign(result, binop(Iop_Add32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));

   return "ahhhr";
}

static const HChar *
s390_irgen_AHHLR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Add32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));

   return "ahhlr";
}

static const HChar *
s390_irgen_AIH(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   op2 = (Int)i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32((UInt)op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));
   put_gpr_w0(r1, mkexpr(result));

   return "aih";
}

static const HChar *
s390_irgen_ALR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "alr";
}

static const HChar *
s390_irgen_ALGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "algr";
}

static const HChar *
s390_irgen_ALGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, get_gpr_w1(r2)));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "algfr";
}

static const HChar *
s390_irgen_ALRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Add32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op2, op3);
   put_gpr_w1(r1, mkexpr(result));

   return "alrk";
}

static const HChar *
s390_irgen_ALGRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Add64, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, op2, op3);
   put_gpr_dw0(r1, mkexpr(result));

   return "algrk";
}

static const HChar *
s390_irgen_AL(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "al";
}

static const HChar *
s390_irgen_ALY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "aly";
}

static const HChar *
s390_irgen_ALG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "alg";
}

static const HChar *
s390_irgen_ALGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));
   assign(result, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "algf";
}

static const HChar *
s390_irgen_ALFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   op2 = i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op1, mktemp(Ity_I32,
                       mkU32(op2)));
   put_gpr_w1(r1, mkexpr(result));

   return "alfi";
}

static const HChar *
s390_irgen_ALGFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   op2 = (ULong)i2;
   assign(result, binop(Iop_Add64, mkexpr(op1), mkU64(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, op1, mktemp(Ity_I64,
                       mkU64(op2)));
   put_gpr_dw0(r1, mkexpr(result));

   return "algfi";
}

static const HChar *
s390_irgen_ALHHHR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r2));
   assign(op3, get_gpr_w0(r3));
   assign(result, binop(Iop_Add32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));

   return "alhhhr";
}

static const HChar *
s390_irgen_ALHHLR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Add32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));

   return "alhhlr";
}

static const HChar *
s390_irgen_ALCR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp carry_in = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(carry_in, binop(Iop_Shr32, s390_call_calculate_cc(), mkU8(1)));
   assign(result, binop(Iop_Add32, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)),
          mkexpr(carry_in)));
   s390_cc_thunk_putZZZ(S390_CC_OP_UNSIGNED_ADDC_32, op1, op2, carry_in);
   put_gpr_w1(r1, mkexpr(result));

   return "alcr";
}

static const HChar *
s390_irgen_ALCGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp carry_in = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(carry_in, unop(Iop_32Uto64, binop(Iop_Shr32, s390_call_calculate_cc(),
          mkU8(1))));
   assign(result, binop(Iop_Add64, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)),
          mkexpr(carry_in)));
   s390_cc_thunk_putZZZ(S390_CC_OP_UNSIGNED_ADDC_64, op1, op2, carry_in);
   put_gpr_dw0(r1, mkexpr(result));

   return "alcgr";
}

static const HChar *
s390_irgen_ALC(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp carry_in = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(carry_in, binop(Iop_Shr32, s390_call_calculate_cc(), mkU8(1)));
   assign(result, binop(Iop_Add32, binop(Iop_Add32, mkexpr(op1), mkexpr(op2)),
          mkexpr(carry_in)));
   s390_cc_thunk_putZZZ(S390_CC_OP_UNSIGNED_ADDC_32, op1, op2, carry_in);
   put_gpr_w1(r1, mkexpr(result));

   return "alc";
}

static const HChar *
s390_irgen_ALCG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp carry_in = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(carry_in, unop(Iop_32Uto64, binop(Iop_Shr32, s390_call_calculate_cc(),
          mkU8(1))));
   assign(result, binop(Iop_Add64, binop(Iop_Add64, mkexpr(op1), mkexpr(op2)),
          mkexpr(carry_in)));
   s390_cc_thunk_putZZZ(S390_CC_OP_UNSIGNED_ADDC_64, op1, op2, carry_in);
   put_gpr_dw0(r1, mkexpr(result));

   return "alcg";
}

static const HChar *
s390_irgen_ALSI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, load(Ity_I32, mkexpr(op1addr)));
   op2 = (UInt)(Int)(Char)i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op1, mktemp(Ity_I32,
                       mkU32(op2)));
   store(mkexpr(op1addr), mkexpr(result));

   return "alsi";
}

static const HChar *
s390_irgen_ALGSI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;
   IRTemp result = newTemp(Ity_I64);

   assign(op1, load(Ity_I64, mkexpr(op1addr)));
   op2 = (ULong)(Long)(Char)i2;
   assign(result, binop(Iop_Add64, mkexpr(op1), mkU64(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, op1, mktemp(Ity_I64,
                       mkU64(op2)));
   store(mkexpr(op1addr), mkexpr(result));

   return "algsi";
}

static const HChar *
s390_irgen_ALHSIK(UChar r1, UChar r3, UShort i2)
{
   UInt op2;
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   op2 = (UInt)(Int)(Short)i2;
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Add32, mkU32(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, mktemp(Ity_I32, mkU32(op2)),
                       op3);
   put_gpr_w1(r1, mkexpr(result));

   return "alhsik";
}

static const HChar *
s390_irgen_ALGHSIK(UChar r1, UChar r3, UShort i2)
{
   ULong op2;
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   op2 = (ULong)(Long)(Short)i2;
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Add64, mkU64(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, mktemp(Ity_I64, mkU64(op2)),
                       op3);
   put_gpr_dw0(r1, mkexpr(result));

   return "alghsik";
}

static const HChar *
s390_irgen_ALSIH(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   op2 = i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op1, mktemp(Ity_I32,
                       mkU32(op2)));
   put_gpr_w0(r1, mkexpr(result));

   return "alsih";
}

static const HChar *
s390_irgen_ALSIHN(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   op2 = i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32(op2)));
   put_gpr_w0(r1, mkexpr(result));

   return "alsihn";
}

static const HChar *
s390_irgen_NR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_And32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "nr";
}

static const HChar *
s390_irgen_NGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_And64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "ngr";
}

static const HChar *
s390_irgen_NRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_And32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "nrk";
}

static const HChar *
s390_irgen_NGRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_And64, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "ngrk";
}

static const HChar *
s390_irgen_N(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_And32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "n";
}

static const HChar *
s390_irgen_NY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_And32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "ny";
}

static const HChar *
s390_irgen_NG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_And64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "ng";
}

static const HChar *
s390_irgen_NI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;
   IRTemp result = newTemp(Ity_I8);

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   assign(result, binop(Iop_And8, mkexpr(op1), mkU8(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   store(mkexpr(op1addr), mkexpr(result));

   return "ni";
}

static const HChar *
s390_irgen_NIY(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;
   IRTemp result = newTemp(Ity_I8);

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   assign(result, binop(Iop_And8, mkexpr(op1), mkU8(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   store(mkexpr(op1addr), mkexpr(result));

   return "niy";
}

static const HChar *
s390_irgen_NIHF(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   op2 = i2;
   assign(result, binop(Iop_And32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w0(r1, mkexpr(result));

   return "nihf";
}

static const HChar *
s390_irgen_NIHH(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;
   IRTemp result = newTemp(Ity_I16);

   assign(op1, get_gpr_hw0(r1));
   op2 = i2;
   assign(result, binop(Iop_And16, mkexpr(op1), mkU16(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_hw0(r1, mkexpr(result));

   return "nihh";
}

static const HChar *
s390_irgen_NIHL(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;
   IRTemp result = newTemp(Ity_I16);

   assign(op1, get_gpr_hw1(r1));
   op2 = i2;
   assign(result, binop(Iop_And16, mkexpr(op1), mkU16(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_hw1(r1, mkexpr(result));

   return "nihl";
}

static const HChar *
s390_irgen_NILF(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   op2 = i2;
   assign(result, binop(Iop_And32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "nilf";
}

static const HChar *
s390_irgen_NILH(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;
   IRTemp result = newTemp(Ity_I16);

   assign(op1, get_gpr_hw2(r1));
   op2 = i2;
   assign(result, binop(Iop_And16, mkexpr(op1), mkU16(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_hw2(r1, mkexpr(result));

   return "nilh";
}

static const HChar *
s390_irgen_NILL(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;
   IRTemp result = newTemp(Ity_I16);

   assign(op1, get_gpr_hw3(r1));
   op2 = i2;
   assign(result, binop(Iop_And16, mkexpr(op1), mkU16(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_hw3(r1, mkexpr(result));

   return "nill";
}

static const HChar *
s390_irgen_BASR(UChar r1, UChar r2)
{
   IRTemp target = newTemp(Ity_I64);

   if (r2 == 0) {
      put_gpr_dw0(r1, mkU64(guest_IA_curr_instr + 2ULL));
   } else {
      if (r1 != r2) {
         put_gpr_dw0(r1, mkU64(guest_IA_curr_instr + 2ULL));
         call_function(get_gpr_dw0(r2));
      } else {
         assign(target, get_gpr_dw0(r2));
         put_gpr_dw0(r1, mkU64(guest_IA_curr_instr + 2ULL));
         call_function(mkexpr(target));
      }
   }

   return "basr";
}

static const HChar *
s390_irgen_BAS(UChar r1, IRTemp op2addr)
{
   IRTemp target = newTemp(Ity_I64);

   put_gpr_dw0(r1, mkU64(guest_IA_curr_instr + 4ULL));
   assign(target, mkexpr(op2addr));
   call_function(mkexpr(target));

   return "bas";
}

static const HChar *
s390_irgen_BCR(UChar r1, UChar r2)
{
   IRTemp cond = newTemp(Ity_I32);

   if (r2 == 0 && (r1 >= 14)) {    /* serialization */
      stmt(IRStmt_MBE(Imbe_Fence));
   }

   if ((r2 == 0) || (r1 == 0)) {
   } else {
      if (r1 == 15) {
         return_from_function(get_gpr_dw0(r2));
      } else {
         assign(cond, s390_call_calculate_cond(r1));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    get_gpr_dw0(r2));
      }
   }
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC2(XMNM, GPR), S390_XMNM_BCR, r1, r2);

   return "bcr";
}

static const HChar *
s390_irgen_BC(UChar r1, UChar x2, UChar b2, UShort d2, IRTemp op2addr)
{
   IRTemp cond = newTemp(Ity_I32);

   if (r1 == 0) {
   } else {
      if (r1 == 15) {
         always_goto(mkexpr(op2addr));
      } else {
         assign(cond, s390_call_calculate_cond(r1));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    mkexpr(op2addr));
      }
   }
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC2(XMNM, UDXB), S390_XMNM_BC, r1, d2, x2, b2);

   return "bc";
}

static const HChar *
s390_irgen_BCTR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, binop(Iop_Sub32, get_gpr_w1(r1), mkU32(1)));
   if (r2 != 0) {
      if_condition_goto_computed(binop(Iop_CmpNE32, get_gpr_w1(r1), mkU32(0)),
                                 get_gpr_dw0(r2));
   }

   return "bctr";
}

static const HChar *
s390_irgen_BCTGR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, binop(Iop_Sub64, get_gpr_dw0(r1), mkU64(1)));
   if (r2 != 0) {
      if_condition_goto_computed(binop(Iop_CmpNE64, get_gpr_dw0(r1), mkU64(0)),
                                 get_gpr_dw0(r2));
   }

   return "bctgr";
}

static const HChar *
s390_irgen_BCT(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, binop(Iop_Sub32, get_gpr_w1(r1), mkU32(1)));
   if_condition_goto_computed(binop(Iop_CmpNE32, get_gpr_w1(r1), mkU32(0)),
                              mkexpr(op2addr));

   return "bct";
}

static const HChar *
s390_irgen_BCTG(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, binop(Iop_Sub64, get_gpr_dw0(r1), mkU64(1)));
   if_condition_goto_computed(binop(Iop_CmpNE64, get_gpr_dw0(r1), mkU64(0)),
                              mkexpr(op2addr));

   return "bctg";
}

static const HChar *
s390_irgen_BIC(UChar r1, IRTemp op2addr)
{
   IRTemp cond = newTemp(Ity_I32);

   if (r1 == 0) {
      /* nothing */
   } else if (r1 == 15) {
      always_goto(load(Ity_I64, mkexpr(op2addr)));
   } else {
      assign(cond, s390_call_calculate_cond(r1));
      if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                 load(Ity_I64, mkexpr(op2addr)));
   }

   return "bic";
}

static const HChar *
s390_irgen_BXH(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_I32);

   assign(value, get_gpr_w1(r3 | 1));
   put_gpr_w1(r1, binop(Iop_Add32, get_gpr_w1(r1), get_gpr_w1(r3)));
   if_condition_goto_computed(binop(Iop_CmpLT32S, mkexpr(value),
                                    get_gpr_w1(r1)), mkexpr(op2addr));

   return "bxh";
}

static const HChar *
s390_irgen_BXHG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_I64);

   assign(value, get_gpr_dw0(r3 | 1));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), get_gpr_dw0(r3)));
   if_condition_goto_computed(binop(Iop_CmpLT64S, mkexpr(value),
                                    get_gpr_dw0(r1)), mkexpr(op2addr));

   return "bxhg";
}

static const HChar *
s390_irgen_BXLE(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_I32);

   assign(value, get_gpr_w1(r3 | 1));
   put_gpr_w1(r1, binop(Iop_Add32, get_gpr_w1(r1), get_gpr_w1(r3)));
   if_condition_goto_computed(binop(Iop_CmpLE32S, get_gpr_w1(r1),
                                    mkexpr(value)), mkexpr(op2addr));

   return "bxle";
}

static const HChar *
s390_irgen_BXLEG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_I64);

   assign(value, get_gpr_dw0(r3 | 1));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), get_gpr_dw0(r3)));
   if_condition_goto_computed(binop(Iop_CmpLE64S, get_gpr_dw0(r1),
                                    mkexpr(value)), mkexpr(op2addr));

   return "bxleg";
}

static const HChar *
s390_irgen_BRAS(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(guest_IA_curr_instr + 4ULL));
   call_function_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

   return "bras";
}

static const HChar *
s390_irgen_BRASL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64(guest_IA_curr_instr + 6ULL));
   call_function_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Int)i2 << 1));

   return "brasl";
}

static const HChar *
s390_irgen_BRC(UChar r1, UShort i2)
{
   IRTemp cond = newTemp(Ity_I32);

   if (r1 == 0) {
   } else {
      if (r1 == 15) {
         always_goto_and_chase(
               guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));
      } else {
         assign(cond, s390_call_calculate_cond(r1));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

      }
   }
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC2(XMNM, PCREL), S390_XMNM_BRC, r1, (Int)(Short)i2);

   return "brc";
}

static const HChar *
s390_irgen_BRCL(UChar r1, UInt i2)
{
   IRTemp cond = newTemp(Ity_I32);

   if (r1 == 0) {
   } else {
      if (r1 == 15) {
         always_goto_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Int)i2 << 1));
      } else {
         assign(cond, s390_call_calculate_cond(r1));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Int)i2 << 1));
      }
   }
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC2(XMNM, PCREL), S390_XMNM_BRCL, r1, i2);

   return "brcl";
}

static const HChar *
s390_irgen_BRCT(UChar r1, UShort i2)
{
   put_gpr_w1(r1, binop(Iop_Sub32, get_gpr_w1(r1), mkU32(1)));
   if_condition_goto(binop(Iop_CmpNE32, get_gpr_w1(r1), mkU32(0)),
                     guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

   return "brct";
}

static const HChar *
s390_irgen_BRCTH(UChar r1, UInt i2)
{
   put_gpr_w0(r1, binop(Iop_Sub32, get_gpr_w0(r1), mkU32(1)));
   if_condition_goto(binop(Iop_CmpNE32, get_gpr_w0(r1), mkU32(0)),
                     guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

   return "brcth";
}

static const HChar *
s390_irgen_BRCTG(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, binop(Iop_Sub64, get_gpr_dw0(r1), mkU64(1)));
   if_condition_goto(binop(Iop_CmpNE64, get_gpr_dw0(r1), mkU64(0)),
                     guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

   return "brctg";
}

static const HChar *
s390_irgen_BRXH(UChar r1, UChar r3, UShort i2)
{
   IRTemp value = newTemp(Ity_I32);

   assign(value, get_gpr_w1(r3 | 1));
   put_gpr_w1(r1, binop(Iop_Add32, get_gpr_w1(r1), get_gpr_w1(r3)));
   if_condition_goto(binop(Iop_CmpLT32S, mkexpr(value), get_gpr_w1(r1)),
                     guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

   return "brxh";
}

static const HChar *
s390_irgen_BRXHG(UChar r1, UChar r3, UShort i2)
{
   IRTemp value = newTemp(Ity_I64);

   assign(value, get_gpr_dw0(r3 | 1));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), get_gpr_dw0(r3)));
   if_condition_goto(binop(Iop_CmpLT64S, mkexpr(value), get_gpr_dw0(r1)),
                     guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

   return "brxhg";
}

static const HChar *
s390_irgen_BRXLE(UChar r1, UChar r3, UShort i2)
{
   IRTemp value = newTemp(Ity_I32);

   assign(value, get_gpr_w1(r3 | 1));
   put_gpr_w1(r1, binop(Iop_Add32, get_gpr_w1(r1), get_gpr_w1(r3)));
   if_condition_goto(binop(Iop_CmpLE32S, get_gpr_w1(r1), mkexpr(value)),
                     guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

   return "brxle";
}

static const HChar *
s390_irgen_BRXLG(UChar r1, UChar r3, UShort i2)
{
   IRTemp value = newTemp(Ity_I64);

   assign(value, get_gpr_dw0(r3 | 1));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), get_gpr_dw0(r3)));
   if_condition_goto(binop(Iop_CmpLE64S, get_gpr_dw0(r1), mkexpr(value)),
                     guest_IA_curr_instr + ((ULong)(Long)(Short)i2 << 1));

   return "brxlg";
}

static const HChar *
s390_irgen_CR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cr";
}

static const HChar *
s390_irgen_CGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cgr";
}

static const HChar *
s390_irgen_CGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cgfr";
}

static const HChar *
s390_irgen_C(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "c";
}

static const HChar *
s390_irgen_CY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cy";
}

static const HChar *
s390_irgen_CG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cg";
}

static const HChar *
s390_irgen_CGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cgf";
}

static const HChar *
s390_irgen_CFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;

   assign(op1, get_gpr_w1(r1));
   op2 = (Int)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));

   return "cfi";
}

static const HChar *
s390_irgen_CGFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;

   assign(op1, get_gpr_dw0(r1));
   op2 = (Long)(Int)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));

   return "cgfi";
}

static const HChar *
s390_irgen_CRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)
          i2 << 1))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "crl";
}

static const HChar *
s390_irgen_CGRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)
          i2 << 1))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cgrl";
}

static const HChar *
s390_irgen_CGFRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkU64(guest_IA_curr_instr +
          ((ULong)(Long)(Int)i2 << 1)))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cgfrl";
}

static const HChar *
s390_irgen_CRB(UChar r1, UChar r2, UChar m3, IRTemp op4addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkexpr(op4addr));
      } else {
         assign(op1, get_gpr_w1(r1));
         assign(op2, get_gpr_w1(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond),
                                          mkU32(0)), mkexpr(op4addr));
      }
   }

   return "crb";
}

static const HChar *
s390_irgen_CGRB(UChar r1, UChar r2, UChar m3, IRTemp op4addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkexpr(op4addr));
      } else {
         assign(op1, get_gpr_dw0(r1));
         assign(op2, get_gpr_dw0(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond),
                                          mkU32(0)), mkexpr(op4addr));
      }
   }

   return "cgrb";
}

static const HChar *
s390_irgen_CRJ(UChar r1, UChar r2, UShort i4, UChar m3)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto_and_chase(
                guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));
      } else {
         assign(op1, get_gpr_w1(r1));
         assign(op2, get_gpr_w1(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));

      }
   }

   return "crj";
}

static const HChar *
s390_irgen_CGRJ(UChar r1, UChar r2, UShort i4, UChar m3)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto_and_chase(
                guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));
      } else {
         assign(op1, get_gpr_dw0(r1));
         assign(op2, get_gpr_dw0(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));

      }
   }

   return "cgrj";
}

static const HChar *
s390_irgen_CIB(UChar r1, UChar m3, UChar i2, IRTemp op4addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkexpr(op4addr));
      } else {
         assign(op1, get_gpr_w1(r1));
         op2 = (Int)(Char)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE, op1,
                                              mktemp(Ity_I32, mkU32((UInt)op2))));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    mkexpr(op4addr));
      }
   }

   return "cib";
}

static const HChar *
s390_irgen_CGIB(UChar r1, UChar m3, UChar i2, IRTemp op4addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkexpr(op4addr));
      } else {
         assign(op1, get_gpr_dw0(r1));
         op2 = (Long)(Char)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE, op1,
                                              mktemp(Ity_I64, mkU64((ULong)op2))));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    mkexpr(op4addr));
      }
   }

   return "cgib";
}

static const HChar *
s390_irgen_CIJ(UChar r1, UChar m3, UShort i4, UChar i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));
      } else {
         assign(op1, get_gpr_w1(r1));
         op2 = (Int)(Char)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE, op1,
                                              mktemp(Ity_I32, mkU32((UInt)op2))));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));

      }
   }

   return "cij";
}

static const HChar *
s390_irgen_CGIJ(UChar r1, UChar m3, UShort i4, UChar i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));
      } else {
         assign(op1, get_gpr_dw0(r1));
         op2 = (Long)(Char)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE, op1,
                                              mktemp(Ity_I64, mkU64((ULong)op2))));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));

      }
   }

   return "cgij";
}

static const HChar *
s390_irgen_CH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "ch";
}

static const HChar *
s390_irgen_CHY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "chy";
}

static const HChar *
s390_irgen_CGH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_16Sto64, load(Ity_I16, mkexpr(op2addr))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cgh";
}

static const HChar *
s390_irgen_CHI(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;

   assign(op1, get_gpr_w1(r1));
   op2 = (Int)(Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));

   return "chi";
}

static const HChar *
s390_irgen_CGHI(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;

   assign(op1, get_gpr_dw0(r1));
   op2 = (Long)(Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));

   return "cghi";
}

static const HChar *
s390_irgen_CHHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I16);
   Short op2;

   assign(op1, load(Ity_I16, mkexpr(op1addr)));
   op2 = (Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I16,
                       mkU16((UShort)op2)));

   return "chhsi";
}

static const HChar *
s390_irgen_CHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;

   assign(op1, load(Ity_I32, mkexpr(op1addr)));
   op2 = (Int)(Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));

   return "chsi";
}

static const HChar *
s390_irgen_CGHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;

   assign(op1, load(Ity_I64, mkexpr(op1addr)));
   op2 = (Long)(Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));

   return "cghsi";
}

static const HChar *
s390_irgen_CHRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkU64(guest_IA_curr_instr +
          ((ULong)(Long)(Int)i2 << 1)))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "chrl";
}

static const HChar *
s390_irgen_CGHRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_16Sto64, load(Ity_I16, mkU64(guest_IA_curr_instr +
          ((ULong)(Long)(Int)i2 << 1)))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "cghrl";
}

static const HChar *
s390_irgen_CHHR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, get_gpr_w0(r2));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "chhr";
}

static const HChar *
s390_irgen_CHLR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, get_gpr_w1(r2));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "chlr";
}

static const HChar *
s390_irgen_CHF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "chf";
}

static const HChar *
s390_irgen_CIH(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;

   assign(op1, get_gpr_w0(r1));
   op2 = (Int)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));

   return "cih";
}

static const HChar *
s390_irgen_CLR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clr";
}

static const HChar *
s390_irgen_CLGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clgr";
}

static const HChar *
s390_irgen_CLGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, get_gpr_w1(r2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clgfr";
}

static const HChar *
s390_irgen_CL(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "cl";
}

static const HChar *
s390_irgen_CLY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "cly";
}

static const HChar *
s390_irgen_CLG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clg";
}

static const HChar *
s390_irgen_CLGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clgf";
}

static const HChar *
s390_irgen_CLFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;

   assign(op1, get_gpr_w1(r1));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32(op2)));

   return "clfi";
}

static const HChar *
s390_irgen_CLGFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;

   assign(op1, get_gpr_dw0(r1));
   op2 = (ULong)i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64(op2)));

   return "clgfi";
}

static const HChar *
s390_irgen_CLI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I8,
                       mkU8(op2)));

   return "cli";
}

static const HChar *
s390_irgen_CLIY(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I8,
                       mkU8(op2)));

   return "cliy";
}

static const HChar *
s390_irgen_CLFHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;

   assign(op1, load(Ity_I32, mkexpr(op1addr)));
   op2 = (UInt)i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32(op2)));

   return "clfhsi";
}

static const HChar *
s390_irgen_CLGHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;

   assign(op1, load(Ity_I64, mkexpr(op1addr)));
   op2 = (ULong)i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64(op2)));

   return "clghsi";
}

static const HChar *
s390_irgen_CLHHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;

   assign(op1, load(Ity_I16, mkexpr(op1addr)));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I16,
                       mkU16(op2)));

   return "clhhsi";
}

static const HChar *
s390_irgen_CLRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)
          i2 << 1))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clrl";
}

static const HChar *
s390_irgen_CLGRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)
          i2 << 1))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clgrl";
}

static const HChar *
s390_irgen_CLGFRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, load(Ity_I32, mkU64(guest_IA_curr_instr +
          ((ULong)(Long)(Int)i2 << 1)))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clgfrl";
}

static const HChar *
s390_irgen_CLHRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Uto32, load(Ity_I16, mkU64(guest_IA_curr_instr +
          ((ULong)(Long)(Int)i2 << 1)))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clhrl";
}

static const HChar *
s390_irgen_CLGHRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_16Uto64, load(Ity_I16, mkU64(guest_IA_curr_instr +
          ((ULong)(Long)(Int)i2 << 1)))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clghrl";
}

static const HChar *
s390_irgen_CLRB(UChar r1, UChar r2, UChar m3, IRTemp op4addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkexpr(op4addr));
      } else {
         assign(op1, get_gpr_w1(r1));
         assign(op2, get_gpr_w1(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    mkexpr(op4addr));
      }
   }

   return "clrb";
}

static const HChar *
s390_irgen_CLGRB(UChar r1, UChar r2, UChar m3, IRTemp op4addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkexpr(op4addr));
      } else {
         assign(op1, get_gpr_dw0(r1));
         assign(op2, get_gpr_dw0(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    mkexpr(op4addr));
      }
   }

   return "clgrb";
}

/* Raise the appropriate signal for a compare-and-trap-instruction data
   exception if the condition is true. */
static void
s390_trap_on_condition(IRExpr *cond)
{
   stmt(IRStmt_Exit(cond, Ijk_SigFPE, IRConst_U64(guest_IA_next_instr),
                    S390X_GUEST_OFFSET(guest_IA)));
}

/* Handle the various flavors of compare (logical) and trap. */
static void
s390_irgen_CxRT(UChar m3, UChar r1, UChar r2, IRType type, UInt opc)
{
   IRExpr *cond;

   if (m3 == 0) {
      /* Trap never (NOP) */
      return;
   } else if (m3 == 14) {
      /* Trap always */
      cond = IRExpr_Const(IRConst_U1 (True));
   } else {
      IRTemp op1 = newTemp(type);
      IRTemp op2 = newTemp(type);

      assign(op1, get_gpr_int(r1, type));
      assign(op2, get_gpr_int(r2, type));
      cond = binop(Iop_CmpNE32,
                   s390_call_calculate_icc(m3, opc, op1, op2), mkU32(0));
   }
   s390_trap_on_condition(cond);
}

static const HChar *
s390_irgen_CGRT(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_CxRT(m3, r1, r2, Ity_I64, S390_CC_OP_SIGNED_COMPARE);
   return "cgrt";
}

static const HChar *
s390_irgen_CRT(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_CxRT(m3, r1, r2, Ity_I32, S390_CC_OP_SIGNED_COMPARE);
   return "crt";
}

static const HChar *
s390_irgen_CLGRT(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_CxRT(m3, r1, r2, Ity_I64, S390_CC_OP_UNSIGNED_COMPARE);
   return "clgrt";
}

static const HChar *
s390_irgen_CLRT(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_CxRT(m3, r1, r2, Ity_I32, S390_CC_OP_UNSIGNED_COMPARE);
   return "clrt";
}

/* Handle the various flavors of compare (logical) immediate and trap. */
static void
s390_irgen_CxIT(UChar m3, UChar r1, UShort i2, IRType type, UInt opc)
{
   IRExpr *cond;

   if (m3 == 0) {
      /* Trap never (NOP) */
      return;
   } else if (m3 == 14) {
      /* Trap always */
      cond = IRExpr_Const(IRConst_U1 (True));
   } else {
      IRTemp op1 = newTemp(type);
      IRTemp op2 = newTemp(type);

      assign(op1, get_gpr_int(r1, type));
      if (opc == S390_CC_OP_SIGNED_COMPARE) {
         assign(op2, type == Ity_I64 ?
                mkU64((ULong)(Short)i2) : mkU32((UInt)(Short)i2));
      } else {
         assign(op2, type == Ity_I64 ?
                mkU64((ULong)i2) : mkU32((UInt)i2));
      }
      cond = binop(Iop_CmpNE32,
                   s390_call_calculate_icc(m3, opc, op1, op2), mkU32(0));
   }
   s390_trap_on_condition(cond);
}

static const HChar *
s390_irgen_CGIT(UChar r1, UShort i2, UChar m3)
{
   s390_irgen_CxIT(m3, r1, i2, Ity_I64, S390_CC_OP_SIGNED_COMPARE);
   return "cgit";
}

static const HChar *
s390_irgen_CIT(UChar r1, UShort i2, UChar m3)
{
   s390_irgen_CxIT(m3, r1, i2, Ity_I32, S390_CC_OP_SIGNED_COMPARE);
   return "cit";
}

static const HChar *
s390_irgen_CLGIT(UChar r1, UShort i2, UChar m3)
{
   s390_irgen_CxIT(m3, r1, i2, Ity_I64, S390_CC_OP_UNSIGNED_COMPARE);
   return "clgit";
}

static const HChar *
s390_irgen_CLFIT(UChar r1, UShort i2, UChar m3)
{
   s390_irgen_CxIT(m3, r1, i2, Ity_I32, S390_CC_OP_UNSIGNED_COMPARE);
   return "clfit";
}

/* Handle the variants of compare logical and trap with memory operand. */
static void
s390_irgen_CLxT(UChar r1, UChar m3, IRTemp op2addr, IRType type, UInt opc)
{
   IRExpr *cond;

   if (m3 == 0) {
      /* Trap never (NOP) */
      return;
   } else if (m3 == 14) {
      /* Trap always */
      cond = IRExpr_Const(IRConst_U1 (True));
   } else {
      IRTemp op1 = newTemp(type);
      IRTemp op2 = newTemp(type);

      assign(op1, get_gpr_int(r1, type));
      assign(op2, load(type, mkexpr(op2addr)));
      cond = binop(Iop_CmpNE32,
                   s390_call_calculate_icc(m3, opc, op1, op2), mkU32(0));
   }
   s390_trap_on_condition(cond);
}

static const HChar *
s390_irgen_CLT(UChar r1, UChar m3, IRTemp op2addr)
{
   s390_irgen_CLxT(r1, m3, op2addr, Ity_I32, S390_CC_OP_UNSIGNED_COMPARE);
   return "clt";
}

static const HChar *
s390_irgen_CLGT(UChar r1, UChar m3, IRTemp op2addr)
{
   s390_irgen_CLxT(r1, m3, op2addr, Ity_I64, S390_CC_OP_UNSIGNED_COMPARE);
   return "clgt";
}

static const HChar *
s390_irgen_LAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I32);
   assign(val, load(Ity_I32, mkexpr(op2addr)));
   put_gpr_w1(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ32, mkexpr(val), mkU32(0)));
   return "lat";
}

static const HChar *
s390_irgen_LGAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I64);
   assign(val, load(Ity_I64, mkexpr(op2addr)));
   put_gpr_dw0(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ64, mkexpr(val), mkU64(0)));
   return "lgat";
}

static const HChar *
s390_irgen_LFHAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I32);
   assign(val, load(Ity_I32, mkexpr(op2addr)));
   put_gpr_w0(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ32, mkexpr(val), mkU32(0)));
   return "lfhat";
}

static const HChar *
s390_irgen_LLGFAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I64);
   assign(val, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));
   put_gpr_dw0(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ64, mkexpr(val), mkU64(0)));
   return "llgfat";
}

static const HChar *
s390_irgen_LLGTAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I64);
   assign(val, binop(Iop_And64, mkU64(0x7fffffff),
                     unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr)))));
   put_gpr_dw0(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ64, mkexpr(val), mkU64(0)));
   return "llgtat";
}

static const HChar *
s390_irgen_CLRJ(UChar r1, UChar r2, UShort i4, UChar m3)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));
      } else {
         assign(op1, get_gpr_w1(r1));
         assign(op2, get_gpr_w1(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));

      }
   }

   return "clrj";
}

static const HChar *
s390_irgen_CLGRJ(UChar r1, UChar r2, UShort i4, UChar m3)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));
      } else {
         assign(op1, get_gpr_dw0(r1));
         assign(op2, get_gpr_dw0(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));

      }
   }

   return "clgrj";
}

static const HChar *
s390_irgen_CLIB(UChar r1, UChar m3, UChar i2, IRTemp op4addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkexpr(op4addr));
      } else {
         assign(op1, get_gpr_w1(r1));
         op2 = (UInt)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE, op1,
                                              mktemp(Ity_I32, mkU32(op2))));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    mkexpr(op4addr));
      }
   }

   return "clib";
}

static const HChar *
s390_irgen_CLGIB(UChar r1, UChar m3, UChar i2, IRTemp op4addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkexpr(op4addr));
      } else {
         assign(op1, get_gpr_dw0(r1));
         op2 = (ULong)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE, op1,
                                              mktemp(Ity_I64, mkU64(op2))));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    mkexpr(op4addr));
      }
   }

   return "clgib";
}

static const HChar *
s390_irgen_CLIJ(UChar r1, UChar m3, UShort i4, UChar i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));
      } else {
         assign(op1, get_gpr_w1(r1));
         op2 = (UInt)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE, op1,
                                              mktemp(Ity_I32, mkU32(op2))));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));

      }
   }

   return "clij";
}

static const HChar *
s390_irgen_CLGIJ(UChar r1, UChar m3, UShort i4, UChar i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto_and_chase(guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));
      } else {
         assign(op1, get_gpr_dw0(r1));
         op2 = (ULong)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE, op1,
                                              mktemp(Ity_I64, mkU64(op2))));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           guest_IA_curr_instr + ((ULong)(Long)(Short)i4 << 1));

      }
   }

   return "clgij";
}

static const HChar *
s390_irgen_CLM(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp b0 = newTemp(Ity_I32);
   IRTemp b1 = newTemp(Ity_I32);
   IRTemp b2 = newTemp(Ity_I32);
   IRTemp b3 = newTemp(Ity_I32);
   IRTemp c0 = newTemp(Ity_I32);
   IRTemp c1 = newTemp(Ity_I32);
   IRTemp c2 = newTemp(Ity_I32);
   IRTemp c3 = newTemp(Ity_I32);
   UChar n;

   n = 0;
   if ((r3 & 8) != 0) {
      assign(b0, unop(Iop_8Uto32, get_gpr_b4(r1)));
      assign(c0, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));
      n = n + 1;
   } else {
      assign(b0, mkU32(0));
      assign(c0, mkU32(0));
   }
   if ((r3 & 4) != 0) {
      assign(b1, unop(Iop_8Uto32, get_gpr_b5(r1)));
      assign(c1, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b1, mkU32(0));
      assign(c1, mkU32(0));
   }
   if ((r3 & 2) != 0) {
      assign(b2, unop(Iop_8Uto32, get_gpr_b6(r1)));
      assign(c2, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b2, mkU32(0));
      assign(c2, mkU32(0));
   }
   if ((r3 & 1) != 0) {
      assign(b3, unop(Iop_8Uto32, get_gpr_b7(r1)));
      assign(c3, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b3, mkU32(0));
      assign(c3, mkU32(0));
   }
   assign(op1, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Shl32,
          mkexpr(b0), mkU8(24)), binop(Iop_Shl32, mkexpr(b1), mkU8(16))),
          binop(Iop_Shl32, mkexpr(b2), mkU8(8))), mkexpr(b3)));
   assign(op2, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Shl32,
          mkexpr(c0), mkU8(24)), binop(Iop_Shl32, mkexpr(c1), mkU8(16))),
          binop(Iop_Shl32, mkexpr(c2), mkU8(8))), mkexpr(c3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clm";
}

static const HChar *
s390_irgen_CLMY(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp b0 = newTemp(Ity_I32);
   IRTemp b1 = newTemp(Ity_I32);
   IRTemp b2 = newTemp(Ity_I32);
   IRTemp b3 = newTemp(Ity_I32);
   IRTemp c0 = newTemp(Ity_I32);
   IRTemp c1 = newTemp(Ity_I32);
   IRTemp c2 = newTemp(Ity_I32);
   IRTemp c3 = newTemp(Ity_I32);
   UChar n;

   n = 0;
   if ((r3 & 8) != 0) {
      assign(b0, unop(Iop_8Uto32, get_gpr_b4(r1)));
      assign(c0, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));
      n = n + 1;
   } else {
      assign(b0, mkU32(0));
      assign(c0, mkU32(0));
   }
   if ((r3 & 4) != 0) {
      assign(b1, unop(Iop_8Uto32, get_gpr_b5(r1)));
      assign(c1, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b1, mkU32(0));
      assign(c1, mkU32(0));
   }
   if ((r3 & 2) != 0) {
      assign(b2, unop(Iop_8Uto32, get_gpr_b6(r1)));
      assign(c2, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b2, mkU32(0));
      assign(c2, mkU32(0));
   }
   if ((r3 & 1) != 0) {
      assign(b3, unop(Iop_8Uto32, get_gpr_b7(r1)));
      assign(c3, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b3, mkU32(0));
      assign(c3, mkU32(0));
   }
   assign(op1, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Shl32,
          mkexpr(b0), mkU8(24)), binop(Iop_Shl32, mkexpr(b1), mkU8(16))),
          binop(Iop_Shl32, mkexpr(b2), mkU8(8))), mkexpr(b3)));
   assign(op2, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Shl32,
          mkexpr(c0), mkU8(24)), binop(Iop_Shl32, mkexpr(c1), mkU8(16))),
          binop(Iop_Shl32, mkexpr(c2), mkU8(8))), mkexpr(c3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clmy";
}

static const HChar *
s390_irgen_CLMH(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp b0 = newTemp(Ity_I32);
   IRTemp b1 = newTemp(Ity_I32);
   IRTemp b2 = newTemp(Ity_I32);
   IRTemp b3 = newTemp(Ity_I32);
   IRTemp c0 = newTemp(Ity_I32);
   IRTemp c1 = newTemp(Ity_I32);
   IRTemp c2 = newTemp(Ity_I32);
   IRTemp c3 = newTemp(Ity_I32);
   UChar n;

   n = 0;
   if ((r3 & 8) != 0) {
      assign(b0, unop(Iop_8Uto32, get_gpr_b0(r1)));
      assign(c0, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));
      n = n + 1;
   } else {
      assign(b0, mkU32(0));
      assign(c0, mkU32(0));
   }
   if ((r3 & 4) != 0) {
      assign(b1, unop(Iop_8Uto32, get_gpr_b1(r1)));
      assign(c1, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b1, mkU32(0));
      assign(c1, mkU32(0));
   }
   if ((r3 & 2) != 0) {
      assign(b2, unop(Iop_8Uto32, get_gpr_b2(r1)));
      assign(c2, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b2, mkU32(0));
      assign(c2, mkU32(0));
   }
   if ((r3 & 1) != 0) {
      assign(b3, unop(Iop_8Uto32, get_gpr_b3(r1)));
      assign(c3, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b3, mkU32(0));
      assign(c3, mkU32(0));
   }
   assign(op1, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Shl32,
          mkexpr(b0), mkU8(24)), binop(Iop_Shl32, mkexpr(b1), mkU8(16))),
          binop(Iop_Shl32, mkexpr(b2), mkU8(8))), mkexpr(b3)));
   assign(op2, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Or32, binop(Iop_Shl32,
          mkexpr(c0), mkU8(24)), binop(Iop_Shl32, mkexpr(c1), mkU8(16))),
          binop(Iop_Shl32, mkexpr(c2), mkU8(8))), mkexpr(c3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clmh";
}

static const HChar *
s390_irgen_CLHHR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, get_gpr_w0(r2));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clhhr";
}

static const HChar *
s390_irgen_CLHLR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, get_gpr_w1(r2));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clhlr";
}

static const HChar *
s390_irgen_CLHF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "clhf";
}

static const HChar *
s390_irgen_CLIH(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;

   assign(op1, get_gpr_w0(r1));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32(op2)));

   return "clih";
}

static const HChar *
s390_irgen_CPYA(UChar r1, UChar r2)
{
   put_ar_w0(r1, get_ar_w0(r2));
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, AR, AR), "cpya", r1, r2);

   return "cpya";
}

static const HChar *
s390_irgen_XR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   if (r1 == r2) {
      assign(result, mkU32(0));
   } else {
      assign(op1, get_gpr_w1(r1));
      assign(op2, get_gpr_w1(r2));
      assign(result, binop(Iop_Xor32, mkexpr(op1), mkexpr(op2)));
   }
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "xr";
}

static const HChar *
s390_irgen_XGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   if (r1 == r2) {
      assign(result, mkU64(0));
   } else {
      assign(op1, get_gpr_dw0(r1));
      assign(op2, get_gpr_dw0(r2));
      assign(result, binop(Iop_Xor64, mkexpr(op1), mkexpr(op2)));
   }
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "xgr";
}

static const HChar *
s390_irgen_XRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Xor32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "xrk";
}

static const HChar *
s390_irgen_XGRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Xor64, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "xgrk";
}

static const HChar *
s390_irgen_X(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Xor32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "x";
}

static const HChar *
s390_irgen_XY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Xor32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "xy";
}

static const HChar *
s390_irgen_XG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_Xor64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "xg";
}

static const HChar *
s390_irgen_XI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;
   IRTemp result = newTemp(Ity_I8);

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   assign(result, binop(Iop_Xor8, mkexpr(op1), mkU8(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   store(mkexpr(op1addr), mkexpr(result));

   return "xi";
}

static const HChar *
s390_irgen_XIY(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;
   IRTemp result = newTemp(Ity_I8);

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   assign(result, binop(Iop_Xor8, mkexpr(op1), mkU8(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   store(mkexpr(op1addr), mkexpr(result));

   return "xiy";
}

static const HChar *
s390_irgen_XIHF(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   op2 = i2;
   assign(result, binop(Iop_Xor32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w0(r1, mkexpr(result));

   return "xihf";
}

static const HChar *
s390_irgen_XILF(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   op2 = i2;
   assign(result, binop(Iop_Xor32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "xilf";
}

static const HChar *
s390_irgen_EAR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, get_ar_w0(r2));
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, GPR, AR), "ear", r1, r2);

   return "ear";
}

static const HChar *
s390_irgen_IC(UChar r1, IRTemp op2addr)
{
   put_gpr_b7(r1, load(Ity_I8, mkexpr(op2addr)));

   return "ic";
}

static const HChar *
s390_irgen_ICY(UChar r1, IRTemp op2addr)
{
   put_gpr_b7(r1, load(Ity_I8, mkexpr(op2addr)));

   return "icy";
}

static const HChar *
s390_irgen_ICM(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar n;
   IRTemp result = newTemp(Ity_I32);
   UInt mask;

   n = 0;
   mask = (UInt)r3;
   if ((mask & 8) != 0) {
      put_gpr_b4(r1, load(Ity_I8, mkexpr(op2addr)));
      n = n + 1;
   }
   if ((mask & 4) != 0) {
      put_gpr_b5(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   if ((mask & 2) != 0) {
      put_gpr_b6(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   if ((mask & 1) != 0) {
      put_gpr_b7(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   assign(result, get_gpr_w1(r1));
   s390_cc_thunk_putZZ(S390_CC_OP_INSERT_CHAR_MASK_32, result, mktemp(Ity_I32,
                       mkU32(mask)));

   return "icm";
}

static const HChar *
s390_irgen_ICMY(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar n;
   IRTemp result = newTemp(Ity_I32);
   UInt mask;

   n = 0;
   mask = (UInt)r3;
   if ((mask & 8) != 0) {
      put_gpr_b4(r1, load(Ity_I8, mkexpr(op2addr)));
      n = n + 1;
   }
   if ((mask & 4) != 0) {
      put_gpr_b5(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   if ((mask & 2) != 0) {
      put_gpr_b6(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   if ((mask & 1) != 0) {
      put_gpr_b7(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   assign(result, get_gpr_w1(r1));
   s390_cc_thunk_putZZ(S390_CC_OP_INSERT_CHAR_MASK_32, result, mktemp(Ity_I32,
                       mkU32(mask)));

   return "icmy";
}

static const HChar *
s390_irgen_ICMH(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar n;
   IRTemp result = newTemp(Ity_I32);
   UInt mask;

   n = 0;
   mask = (UInt)r3;
   if ((mask & 8) != 0) {
      put_gpr_b0(r1, load(Ity_I8, mkexpr(op2addr)));
      n = n + 1;
   }
   if ((mask & 4) != 0) {
      put_gpr_b1(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   if ((mask & 2) != 0) {
      put_gpr_b2(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   if ((mask & 1) != 0) {
      put_gpr_b3(r1, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkU64(n))));

      n = n + 1;
   }
   assign(result, get_gpr_w0(r1));
   s390_cc_thunk_putZZ(S390_CC_OP_INSERT_CHAR_MASK_32, result, mktemp(Ity_I32,
                       mkU32(mask)));

   return "icmh";
}

static const HChar *
s390_irgen_IIHF(UChar r1, UInt i2)
{
   put_gpr_w0(r1, mkU32(i2));

   return "iihf";
}

static const HChar *
s390_irgen_IIHH(UChar r1, UShort i2)
{
   put_gpr_hw0(r1, mkU16(i2));

   return "iihh";
}

static const HChar *
s390_irgen_IIHL(UChar r1, UShort i2)
{
   put_gpr_hw1(r1, mkU16(i2));

   return "iihl";
}

static const HChar *
s390_irgen_IILF(UChar r1, UInt i2)
{
   put_gpr_w1(r1, mkU32(i2));

   return "iilf";
}

static const HChar *
s390_irgen_IILH(UChar r1, UShort i2)
{
   put_gpr_hw2(r1, mkU16(i2));

   return "iilh";
}

static const HChar *
s390_irgen_IILL(UChar r1, UShort i2)
{
   put_gpr_hw3(r1, mkU16(i2));

   return "iill";
}

static const HChar *
s390_irgen_LR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, get_gpr_w1(r2));

   return "lr";
}

static const HChar *
s390_irgen_LGR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, get_gpr_dw0(r2));

   return "lgr";
}

static const HChar *
s390_irgen_LGFR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_32Sto64, get_gpr_w1(r2)));

   return "lgfr";
}

static const HChar *
s390_irgen_L(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, load(Ity_I32, mkexpr(op2addr)));

   return "l";
}

static const HChar *
s390_irgen_LY(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, load(Ity_I32, mkexpr(op2addr)));

   return "ly";
}

static const HChar *
s390_irgen_LG(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, load(Ity_I64, mkexpr(op2addr)));

   return "lg";
}

static const HChar *
s390_irgen_LGF(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));

   return "lgf";
}

static const HChar *
s390_irgen_LGFI(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64((ULong)(Long)(Int)i2));

   return "lgfi";
}

static const HChar *
s390_irgen_LRL(UChar r1, UInt i2)
{
   put_gpr_w1(r1, load(Ity_I32, mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)
              i2 << 1))));

   return "lrl";
}

static const HChar *
s390_irgen_LGRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, load(Ity_I64, mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)
               i2 << 1))));

   return "lgrl";
}

static const HChar *
s390_irgen_LGFRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, unop(Iop_32Sto64, load(Ity_I32, mkU64(guest_IA_curr_instr +
               ((ULong)(Long)(Int)i2 << 1)))));

   return "lgfrl";
}

static const HChar *
s390_irgen_LA(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, mkexpr(op2addr));

   return "la";
}

static const HChar *
s390_irgen_LAY(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, mkexpr(op2addr));

   return "lay";
}

static const HChar *
s390_irgen_LAE(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, mkexpr(op2addr));

   return "lae";
}

static const HChar *
s390_irgen_LAEY(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, mkexpr(op2addr));

   return "laey";
}

static const HChar *
s390_irgen_LARL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)i2 << 1)));

   return "larl";
}

/* The IR representation of LAA and friends is an approximation of what 
   happens natively. Essentially a loop containing a compare-and-swap is
   constructed which will iterate until the CAS succeeds. As a consequence,
   instrumenters may see more memory accesses than happen natively. See also
   discussion here: https://bugs.kde.org/show_bug.cgi?id=306035 */
static void
s390_irgen_load_and_add32(UChar r1, UChar r3, IRTemp op2addr, Bool is_signed)
{
   IRCAS *cas;
   IRTemp old_mem = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Add32, mkexpr(op2), mkexpr(op3)));

   /* Place the addition of second operand and third operand at the
      second-operand location everytime */
   cas = mkIRCAS(IRTemp_INVALID, old_mem,
                 Iend_BE, mkexpr(op2addr),
                 NULL, mkexpr(op2), /* expected value */
                 NULL, mkexpr(result)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* Set CC according to 32-bit addition */
   if (is_signed) {
      s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_32, op2, op3);
   } else {
      s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_32, op2, op3);
   }

   /* If old_mem contains the expected value, then the CAS succeeded.
      Otherwise, it did not */
   yield_if(binop(Iop_CmpNE32, mkexpr(old_mem), mkexpr(op2)));
   put_gpr_w1(r1, mkexpr(old_mem));
}

static void
s390_irgen_load_and_add64(UChar r1, UChar r3, IRTemp op2addr, Bool is_signed)
{
   IRCAS *cas;
   IRTemp old_mem = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Add64, mkexpr(op2), mkexpr(op3)));

   /* Place the addition of second operand and third operand at the
      second-operand location everytime */
   cas = mkIRCAS(IRTemp_INVALID, old_mem,
                 Iend_BE, mkexpr(op2addr),
                 NULL, mkexpr(op2), /* expected value */
                 NULL, mkexpr(result)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* Set CC according to 64-bit addition */
   if (is_signed) {
      s390_cc_thunk_putSS(S390_CC_OP_SIGNED_ADD_64, op2, op3);
   } else {
      s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_ADD_64, op2, op3);
   }

   /* If old_mem contains the expected value, then the CAS succeeded.
      Otherwise, it did not */
   yield_if(binop(Iop_CmpNE64, mkexpr(old_mem), mkexpr(op2)));
   put_gpr_dw0(r1, mkexpr(old_mem));
}

static void
s390_irgen_load_and_bitwise32(UChar r1, UChar r3, IRTemp op2addr, IROp op)
{
   IRCAS *cas;
   IRTemp old_mem = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(op, mkexpr(op2), mkexpr(op3)));

   /* Place the addition of second operand and third operand at the
      second-operand location everytime */
   cas = mkIRCAS(IRTemp_INVALID, old_mem,
                 Iend_BE, mkexpr(op2addr),
                 NULL, mkexpr(op2), /* expected value */
                 NULL, mkexpr(result)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* Set CC according to bitwise operation */
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);

   /* If old_mem contains the expected value, then the CAS succeeded.
      Otherwise, it did not */
   yield_if(binop(Iop_CmpNE32, mkexpr(old_mem), mkexpr(op2)));
   put_gpr_w1(r1, mkexpr(old_mem));
}

static void
s390_irgen_load_and_bitwise64(UChar r1, UChar r3, IRTemp op2addr, IROp op)
{
   IRCAS *cas;
   IRTemp old_mem = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(op, mkexpr(op2), mkexpr(op3)));

   /* Place the addition of second operand and third operand at the
      second-operand location everytime */
   cas = mkIRCAS(IRTemp_INVALID, old_mem,
                 Iend_BE, mkexpr(op2addr),
                 NULL, mkexpr(op2), /* expected value */
                 NULL, mkexpr(result)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* Set CC according to bitwise operation */
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);

   /* If old_mem contains the expected value, then the CAS succeeded.
      Otherwise, it did not */
   yield_if(binop(Iop_CmpNE64, mkexpr(old_mem), mkexpr(op2)));
   put_gpr_dw0(r1, mkexpr(old_mem));
}

static const HChar *
s390_irgen_LAA(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_add32(r1, r3, op2addr, True /* is_signed */);

   return "laa";
}

static const HChar *
s390_irgen_LAAG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_add64(r1, r3, op2addr, True /* is_signed */);

   return "laag";
}

static const HChar *
s390_irgen_LAAL(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_add32(r1, r3, op2addr, False /* is_signed */);

   return "laal";
}

static const HChar *
s390_irgen_LAALG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_add64(r1, r3, op2addr, False /* is_signed */);

   return "laalg";
}

static const HChar *
s390_irgen_LAN(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise32(r1, r3, op2addr, Iop_And32);

   return "lan";
}

static const HChar *
s390_irgen_LANG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise64(r1, r3, op2addr, Iop_And64);

   return "lang";
}

static const HChar *
s390_irgen_LAX(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise32(r1, r3, op2addr, Iop_Xor32);

   return "lax";
}

static const HChar *
s390_irgen_LAXG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise64(r1, r3, op2addr, Iop_Xor64);

   return "laxg";
}

static const HChar *
s390_irgen_LAO(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise32(r1, r3, op2addr, Iop_Or32);

   return "lao";
}

static const HChar *
s390_irgen_LAOG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise64(r1, r3, op2addr, Iop_Or64);

   return "laog";
}

static const HChar *
s390_irgen_LTR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_gpr_w1(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);

   return "ltr";
}

static const HChar *
s390_irgen_LTGR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_gpr_dw0(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);

   return "ltgr";
}

static const HChar *
s390_irgen_LTGFR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   put_gpr_dw0(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);

   return "ltgfr";
}

static const HChar *
s390_irgen_LT(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   put_gpr_w1(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);

   return "lt";
}

static const HChar *
s390_irgen_LTG(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   put_gpr_dw0(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);

   return "ltg";
}

static const HChar *
s390_irgen_LTGF(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));
   put_gpr_dw0(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);

   return "ltgf";
}

static const HChar *
s390_irgen_LBR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, unop(Iop_8Sto32, get_gpr_b7(r2)));

   return "lbr";
}

static const HChar *
s390_irgen_LGBR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_8Sto64, get_gpr_b7(r2)));

   return "lgbr";
}

static const HChar *
s390_irgen_LB(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_8Sto32, load(Ity_I8, mkexpr(op2addr))));

   return "lb";
}

static const HChar *
s390_irgen_LGB(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_8Sto64, load(Ity_I8, mkexpr(op2addr))));

   return "lgb";
}

static const HChar *
s390_irgen_LBH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, unop(Iop_8Sto32, load(Ity_I8, mkexpr(op2addr))));

   return "lbh";
}

static const HChar *
s390_irgen_LCR(UChar r1, UChar r2)
{
   Int op1;
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   op1 = 0;
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_Sub32, mkU32((UInt)op1), mkexpr(op2)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, mktemp(Ity_I32, mkU32((UInt)
                       op1)), op2);

   return "lcr";
}

static const HChar *
s390_irgen_LCGR(UChar r1, UChar r2)
{
   Long op1;
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   op1 = 0ULL;
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_Sub64, mkU64((ULong)op1), mkexpr(op2)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_64, mktemp(Ity_I64, mkU64((ULong)
                       op1)), op2);

   return "lcgr";
}

static const HChar *
s390_irgen_LCGFR(UChar r1, UChar r2)
{
   Long op1;
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   op1 = 0ULL;
   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   assign(result, binop(Iop_Sub64, mkU64((ULong)op1), mkexpr(op2)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_64, mktemp(Ity_I64, mkU64((ULong)
                       op1)), op2);

   return "lcgfr";
}

static const HChar *
s390_irgen_LHR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, unop(Iop_16Sto32, get_gpr_hw3(r2)));

   return "lhr";
}

static const HChar *
s390_irgen_LGHR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_16Sto64, get_gpr_hw3(r2)));

   return "lghr";
}

static const HChar *
s390_irgen_LH(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));

   return "lh";
}

static const HChar *
s390_irgen_LHY(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));

   return "lhy";
}

static const HChar *
s390_irgen_LGH(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_16Sto64, load(Ity_I16, mkexpr(op2addr))));

   return "lgh";
}

static const HChar *
s390_irgen_LHI(UChar r1, UShort i2)
{
   put_gpr_w1(r1, mkU32((UInt)(Int)(Short)i2));

   return "lhi";
}

static const HChar *
s390_irgen_LGHI(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64((ULong)(Long)(Short)i2));

   return "lghi";
}

static const HChar *
s390_irgen_LHRL(UChar r1, UInt i2)
{
   put_gpr_w1(r1, unop(Iop_16Sto32, load(Ity_I16, mkU64(guest_IA_curr_instr +
              ((ULong)(Long)(Int)i2 << 1)))));

   return "lhrl";
}

static const HChar *
s390_irgen_LGHRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, unop(Iop_16Sto64, load(Ity_I16, mkU64(guest_IA_curr_instr +
               ((ULong)(Long)(Int)i2 << 1)))));

   return "lghrl";
}

static const HChar *
s390_irgen_LHH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));

   return "lhh";
}

static const HChar *
s390_irgen_LFH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, load(Ity_I32, mkexpr(op2addr)));

   return "lfh";
}

static const HChar *
s390_irgen_LLGFR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, get_gpr_w1(r2)));

   return "llgfr";
}

static const HChar *
s390_irgen_LLGF(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));

   return "llgf";
}

static const HChar *
s390_irgen_LLGFRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, load(Ity_I32, mkU64(guest_IA_curr_instr +
               ((ULong)(Long)(Int)i2 << 1)))));

   return "llgfrl";
}

static const HChar *
s390_irgen_LLCR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, unop(Iop_8Uto32, get_gpr_b7(r2)));

   return "llcr";
}

static const HChar *
s390_irgen_LLGCR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_8Uto64, get_gpr_b7(r2)));

   return "llgcr";
}

static const HChar *
s390_irgen_LLC(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));

   return "llc";
}

static const HChar *
s390_irgen_LLGC(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_8Uto64, load(Ity_I8, mkexpr(op2addr))));

   return "llgc";
}

static const HChar *
s390_irgen_LLCH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));

   return "llch";
}

static const HChar *
s390_irgen_LLHR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, unop(Iop_16Uto32, get_gpr_hw3(r2)));

   return "llhr";
}

static const HChar *
s390_irgen_LLGHR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_16Uto64, get_gpr_hw3(r2)));

   return "llghr";
}

static const HChar *
s390_irgen_LLH(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_16Uto32, load(Ity_I16, mkexpr(op2addr))));

   return "llh";
}

static const HChar *
s390_irgen_LLGH(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_16Uto64, load(Ity_I16, mkexpr(op2addr))));

   return "llgh";
}

static const HChar *
s390_irgen_LLHRL(UChar r1, UInt i2)
{
   put_gpr_w1(r1, unop(Iop_16Uto32, load(Ity_I16, mkU64(guest_IA_curr_instr +
              ((ULong)(Long)(Int)i2 << 1)))));

   return "llhrl";
}

static const HChar *
s390_irgen_LLGHRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, unop(Iop_16Uto64, load(Ity_I16, mkU64(guest_IA_curr_instr +
               ((ULong)(Long)(Int)i2 << 1)))));

   return "llghrl";
}

static const HChar *
s390_irgen_LLHH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, unop(Iop_16Uto32, load(Ity_I16, mkexpr(op2addr))));

   return "llhh";
}

static const HChar *
s390_irgen_LLIHF(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64(((ULong)i2) << 32));

   return "llihf";
}

static const HChar *
s390_irgen_LLIHH(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(((ULong)i2) << 48));

   return "llihh";
}

static const HChar *
s390_irgen_LLIHL(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(((ULong)i2) << 32));

   return "llihl";
}

static const HChar *
s390_irgen_LLILF(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64(i2));

   return "llilf";
}

static const HChar *
s390_irgen_LLILH(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(((ULong)i2) << 16));

   return "llilh";
}

static const HChar *
s390_irgen_LLILL(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(i2));

   return "llill";
}

static const HChar *
s390_irgen_LLGTR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, binop(Iop_And32, get_gpr_w1(r2),
               mkU32(2147483647))));

   return "llgtr";
}

static const HChar *
s390_irgen_LLGT(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, binop(Iop_And32, load(Ity_I32,
               mkexpr(op2addr)), mkU32(2147483647))));

   return "llgt";
}

static const HChar *
s390_irgen_LNR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(result, mkite(binop(Iop_CmpLE32S, mkexpr(op2), mkU32(0)), mkexpr(op2),
          binop(Iop_Sub32, mkU32(0), mkexpr(op2))));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_BITWISE, result);

   return "lnr";
}

static const HChar *
s390_irgen_LNGR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(result, mkite(binop(Iop_CmpLE64S, mkexpr(op2), mkU64(0)), mkexpr(op2),
          binop(Iop_Sub64, mkU64(0), mkexpr(op2))));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_BITWISE, result);

   return "lngr";
}

static const HChar *
s390_irgen_LNGFR(UChar r1, UChar r2 __attribute__((unused)))
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r1)));
   assign(result, mkite(binop(Iop_CmpLE64S, mkexpr(op2), mkU64(0)), mkexpr(op2),
          binop(Iop_Sub64, mkU64(0), mkexpr(op2))));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_BITWISE, result);

   return "lngfr";
}

static const HChar *
s390_irgen_LOCR(UChar m3, UChar r1, UChar r2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_w1(r1, get_gpr_w1(r2));

   return "locr";
}

static const HChar *
s390_irgen_LOCGR(UChar m3, UChar r1, UChar r2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_dw0(r1, get_gpr_dw0(r2));

   return "locgr";
}

static const HChar *
s390_irgen_LOC(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   put_gpr_w1(r1, load(Ity_I32, mkexpr(op2addr)));

   return "loc";
}

static const HChar *
s390_irgen_LOCG(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   put_gpr_dw0(r1, load(Ity_I64, mkexpr(op2addr)));

   return "locg";
}

static const HChar *
s390_irgen_LPQ(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, load(Ity_I64, mkexpr(op2addr)));
   put_gpr_dw0(r1 + 1, load(Ity_I64, binop(Iop_Add64, mkexpr(op2addr), mkU64(8))
               ));

   return "lpq";
}

static const HChar *
s390_irgen_LPR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(result, mkite(binop(Iop_CmpLT32S, mkexpr(op2), mkU32(0)),
          binop(Iop_Sub32, mkU32(0), mkexpr(op2)), mkexpr(op2)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_POSITIVE_32, op2);

   return "lpr";
}

static const HChar *
s390_irgen_LPGR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(result, mkite(binop(Iop_CmpLT64S, mkexpr(op2), mkU64(0)),
          binop(Iop_Sub64, mkU64(0), mkexpr(op2)), mkexpr(op2)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_POSITIVE_64, op2);

   return "lpgr";
}

static const HChar *
s390_irgen_LPGFR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   assign(result, mkite(binop(Iop_CmpLT64S, mkexpr(op2), mkU64(0)),
          binop(Iop_Sub64, mkU64(0), mkexpr(op2)), mkexpr(op2)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_POSITIVE_64, op2);

   return "lpgfr";
}

static const HChar *
s390_irgen_LRVR(UChar r1, UChar r2)
{
   IRTemp b0 = newTemp(Ity_I8);
   IRTemp b1 = newTemp(Ity_I8);
   IRTemp b2 = newTemp(Ity_I8);
   IRTemp b3 = newTemp(Ity_I8);

   assign(b3, get_gpr_b7(r2));
   assign(b2, get_gpr_b6(r2));
   assign(b1, get_gpr_b5(r2));
   assign(b0, get_gpr_b4(r2));
   put_gpr_b4(r1, mkexpr(b3));
   put_gpr_b5(r1, mkexpr(b2));
   put_gpr_b6(r1, mkexpr(b1));
   put_gpr_b7(r1, mkexpr(b0));

   return "lrvr";
}

static const HChar *
s390_irgen_LRVGR(UChar r1, UChar r2)
{
   IRTemp b0 = newTemp(Ity_I8);
   IRTemp b1 = newTemp(Ity_I8);
   IRTemp b2 = newTemp(Ity_I8);
   IRTemp b3 = newTemp(Ity_I8);
   IRTemp b4 = newTemp(Ity_I8);
   IRTemp b5 = newTemp(Ity_I8);
   IRTemp b6 = newTemp(Ity_I8);
   IRTemp b7 = newTemp(Ity_I8);

   assign(b7, get_gpr_b7(r2));
   assign(b6, get_gpr_b6(r2));
   assign(b5, get_gpr_b5(r2));
   assign(b4, get_gpr_b4(r2));
   assign(b3, get_gpr_b3(r2));
   assign(b2, get_gpr_b2(r2));
   assign(b1, get_gpr_b1(r2));
   assign(b0, get_gpr_b0(r2));
   put_gpr_b0(r1, mkexpr(b7));
   put_gpr_b1(r1, mkexpr(b6));
   put_gpr_b2(r1, mkexpr(b5));
   put_gpr_b3(r1, mkexpr(b4));
   put_gpr_b4(r1, mkexpr(b3));
   put_gpr_b5(r1, mkexpr(b2));
   put_gpr_b6(r1, mkexpr(b1));
   put_gpr_b7(r1, mkexpr(b0));

   return "lrvgr";
}

static const HChar *
s390_irgen_LRVH(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I16);

   assign(op2, load(Ity_I16, mkexpr(op2addr)));
   put_gpr_b6(r1, unop(Iop_16to8, mkexpr(op2)));
   put_gpr_b7(r1, unop(Iop_16HIto8, mkexpr(op2)));

   return "lrvh";
}

static const HChar *
s390_irgen_LRV(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   put_gpr_b4(r1, unop(Iop_32to8, binop(Iop_And32, mkexpr(op2), mkU32(255))));
   put_gpr_b5(r1, unop(Iop_32to8, binop(Iop_And32, binop(Iop_Shr32, mkexpr(op2),
              mkU8(8)), mkU32(255))));
   put_gpr_b6(r1, unop(Iop_32to8, binop(Iop_And32, binop(Iop_Shr32, mkexpr(op2),
              mkU8(16)), mkU32(255))));
   put_gpr_b7(r1, unop(Iop_32to8, binop(Iop_And32, binop(Iop_Shr32, mkexpr(op2),
              mkU8(24)), mkU32(255))));

   return "lrv";
}

static const HChar *
s390_irgen_LRVG(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   put_gpr_b0(r1, unop(Iop_64to8, binop(Iop_And64, mkexpr(op2), mkU64(255))));
   put_gpr_b1(r1, unop(Iop_64to8, binop(Iop_And64, binop(Iop_Shr64, mkexpr(op2),
              mkU8(8)), mkU64(255))));
   put_gpr_b2(r1, unop(Iop_64to8, binop(Iop_And64, binop(Iop_Shr64, mkexpr(op2),
              mkU8(16)), mkU64(255))));
   put_gpr_b3(r1, unop(Iop_64to8, binop(Iop_And64, binop(Iop_Shr64, mkexpr(op2),
              mkU8(24)), mkU64(255))));
   put_gpr_b4(r1, unop(Iop_64to8, binop(Iop_And64, binop(Iop_Shr64, mkexpr(op2),
              mkU8(32)), mkU64(255))));
   put_gpr_b5(r1, unop(Iop_64to8, binop(Iop_And64, binop(Iop_Shr64, mkexpr(op2),
              mkU8(40)), mkU64(255))));
   put_gpr_b6(r1, unop(Iop_64to8, binop(Iop_And64, binop(Iop_Shr64, mkexpr(op2),
              mkU8(48)), mkU64(255))));
   put_gpr_b7(r1, unop(Iop_64to8, binop(Iop_And64, binop(Iop_Shr64, mkexpr(op2),
              mkU8(56)), mkU64(255))));

   return "lrvg";
}

static const HChar *
s390_irgen_MVHHI(UShort i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU16(i2));

   return "mvhhi";
}

static const HChar *
s390_irgen_MVHI(UShort i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU32((UInt)(Int)(Short)i2));

   return "mvhi";
}

static const HChar *
s390_irgen_MVGHI(UShort i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU64((ULong)(Long)(Short)i2));

   return "mvghi";
}

static const HChar *
s390_irgen_MVI(UChar i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU8(i2));

   return "mvi";
}

static const HChar *
s390_irgen_MVIY(UChar i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU8(i2));

   return "mviy";
}

static const HChar *
s390_irgen_MR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));

   return "mr";
}

static const HChar *
s390_irgen_M(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));

   return "m";
}

static const HChar *
s390_irgen_MFY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));

   return "mfy";
}

static const HChar *
s390_irgen_MG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1 + 1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS64, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result)));

   return "mg";
}

static const HChar *
s390_irgen_MGH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I16);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I16, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS64, mkexpr(op1), unop(Iop_16Sto64, mkexpr(op2))
   ));
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "mgh";
}

static const HChar *
s390_irgen_MGRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_MullS64, mkexpr(op2), mkexpr(op3)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result)));

   return "mgrk";
}

static const HChar *
s390_irgen_MH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I16);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I16, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), unop(Iop_16Sto32, mkexpr(op2))
          ));
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "mh";
}

static const HChar *
s390_irgen_MHY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I16);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I16, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), unop(Iop_16Sto32, mkexpr(op2))
          ));
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "mhy";
}

static const HChar *
s390_irgen_MHI(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Short op2;
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1));
   op2 = (Short)i2;
   assign(result, binop(Iop_MullS32, mkexpr(op1), unop(Iop_16Sto32,
          mkU16((UShort)op2))));
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "mhi";
}

static const HChar *
s390_irgen_MGHI(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Short op2;
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1));
   op2 = (Short)i2;
   assign(result, binop(Iop_MullS64, mkexpr(op1), unop(Iop_16Sto64,
          mkU16((UShort)op2))));
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "mghi";
}

static const HChar *
s390_irgen_MLR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_MullU32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));

   return "mlr";
}

static const HChar *
s390_irgen_MLGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1 + 1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_MullU64, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result)));

   return "mlgr";
}

static const HChar *
s390_irgen_ML(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullU32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));

   return "ml";
}

static const HChar *
s390_irgen_MLG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1 + 1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_MullU64, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result)));

   return "mlg";
}

static const HChar *
s390_irgen_MSR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "msr";
}

static const HChar *
s390_irgen_MSGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_MullS64, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "msgr";
}

static const HChar *
s390_irgen_MSGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_MullS64, mkexpr(op1), unop(Iop_32Sto64, mkexpr(op2))
          ));
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "msgfr";
}

static const HChar *
s390_irgen_MS(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "ms";
}

static const HChar *
s390_irgen_MSC(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_MUL_32, op1, op2);
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "msc";
}

static const HChar *
s390_irgen_MSRKC(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_MullS32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_MUL_32, op2, op3);
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "msrkc";
}

static const HChar *
s390_irgen_MSY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "msy";
}

static const HChar *
s390_irgen_MSG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS64, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "msg";
}

static const HChar *
s390_irgen_MSGC(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_MUL_64, op1, op2);
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "msgc";
}

static const HChar *
s390_irgen_MSGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS64, mkexpr(op1), unop(Iop_32Sto64, mkexpr(op2))
          ));
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "msgf";
}

static const HChar *
s390_irgen_MSFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1));
   op2 = (Int)i2;
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkU32((UInt)op2)));
   put_gpr_w1(r1, unop(Iop_64to32, mkexpr(result)));

   return "msfi";
}

static const HChar *
s390_irgen_MSGFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Int op2;
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1));
   op2 = (Int)i2;
   assign(result, binop(Iop_MullS64, mkexpr(op1), unop(Iop_32Sto64, mkU32((UInt)
          op2))));
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "msgfi";
}

static const HChar *
s390_irgen_MSGRKC(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_MullS64, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_MUL_64, op2, op3);
   put_gpr_dw0(r1, unop(Iop_128to64, mkexpr(result)));

   return "msgrkc";
}

static const HChar *
s390_irgen_OR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_Or32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "or";
}

static const HChar *
s390_irgen_OGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_Or64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "ogr";
}

static const HChar *
s390_irgen_ORK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Or32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "ork";
}

static const HChar *
s390_irgen_OGRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Or64, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "ogrk";
}

static const HChar *
s390_irgen_O(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Or32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "o";
}

static const HChar *
s390_irgen_OY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Or32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "oy";
}

static const HChar *
s390_irgen_OG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_Or64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));

   return "og";
}

static const HChar *
s390_irgen_OI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;
   IRTemp result = newTemp(Ity_I8);

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   assign(result, binop(Iop_Or8, mkexpr(op1), mkU8(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   store(mkexpr(op1addr), mkexpr(result));

   return "oi";
}

static const HChar *
s390_irgen_OIY(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;
   IRTemp result = newTemp(Ity_I8);

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   assign(result, binop(Iop_Or8, mkexpr(op1), mkU8(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   store(mkexpr(op1addr), mkexpr(result));

   return "oiy";
}

static const HChar *
s390_irgen_OIHF(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   op2 = i2;
   assign(result, binop(Iop_Or32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w0(r1, mkexpr(result));

   return "oihf";
}

static const HChar *
s390_irgen_OIHH(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;
   IRTemp result = newTemp(Ity_I16);

   assign(op1, get_gpr_hw0(r1));
   op2 = i2;
   assign(result, binop(Iop_Or16, mkexpr(op1), mkU16(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_hw0(r1, mkexpr(result));

   return "oihh";
}

static const HChar *
s390_irgen_OIHL(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;
   IRTemp result = newTemp(Ity_I16);

   assign(op1, get_gpr_hw1(r1));
   op2 = i2;
   assign(result, binop(Iop_Or16, mkexpr(op1), mkU16(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_hw1(r1, mkexpr(result));

   return "oihl";
}

static const HChar *
s390_irgen_OILF(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   op2 = i2;
   assign(result, binop(Iop_Or32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));

   return "oilf";
}

static const HChar *
s390_irgen_OILH(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;
   IRTemp result = newTemp(Ity_I16);

   assign(op1, get_gpr_hw2(r1));
   op2 = i2;
   assign(result, binop(Iop_Or16, mkexpr(op1), mkU16(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_hw2(r1, mkexpr(result));

   return "oilh";
}

static const HChar *
s390_irgen_OILL(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;
   IRTemp result = newTemp(Ity_I16);

   assign(op1, get_gpr_hw3(r1));
   op2 = i2;
   assign(result, binop(Iop_Or16, mkexpr(op1), mkU16(op2)));
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_hw3(r1, mkexpr(result));

   return "oill";
}

static const HChar *
s390_irgen_PFD(void)
{

   return "pfd";
}

static const HChar *
s390_irgen_PFDRL(void)
{

   return "pfdrl";
}

static IRExpr *
get_rounding_mode_from_gr0(void)
{
   IRTemp rm_bits = newTemp(Ity_I32);
   IRExpr *s390rm;
   IRExpr *irrm;

   /* The dfp/bfp rounding mode is stored in bits [60:63] of GR 0
      when PFPO insn is called. So, extract the bits at [60:63] */
   assign(rm_bits, binop(Iop_And32, get_gpr_w1(0), mkU32(0xf)));
   s390rm = mkexpr(rm_bits);
   irrm = mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0x1)),
            mkexpr(encode_bfp_rounding_mode( S390_BFP_ROUND_PER_FPC)),
            mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0x8)),
              mkexpr(encode_dfp_rounding_mode(S390_DFP_ROUND_NEAREST_EVEN_8)),
              mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0x9)),
                mkexpr(encode_dfp_rounding_mode(S390_DFP_ROUND_ZERO_9)),
                mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0xa)),
                  mkexpr(encode_dfp_rounding_mode(S390_DFP_ROUND_POSINF_10)),
                  mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0xb)),
                    mkexpr(encode_dfp_rounding_mode(S390_DFP_ROUND_NEGINF_11)),
                    mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0xc)),
                      mkexpr(encode_dfp_rounding_mode(
                               S390_DFP_ROUND_NEAREST_TIE_AWAY_0_12)),
                      mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0xd)),
                        mkexpr(encode_dfp_rounding_mode(
                                 S390_DFP_ROUND_NEAREST_TIE_TOWARD_0)),
                        mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0xe)),
                          mkexpr(encode_dfp_rounding_mode(
                                   S390_DFP_ROUND_AWAY_0)),
                          mkite(binop(Iop_CmpEQ32, s390rm, mkU32(0xf)),
                            mkexpr(encode_dfp_rounding_mode(
                                     S390_DFP_ROUND_PREPARE_SHORT_15)),
                                /* if rounding mode is 0 or invalid (2-7)
                                   set S390_DFP_ROUND_PER_FPC_0 */
                            mkexpr(encode_dfp_rounding_mode(
                                     S390_DFP_ROUND_PER_FPC_0)))))))))));

   return irrm;
}

static IRExpr *
s390_call_pfpo_helper(IRExpr *gr0)
{
   IRExpr **args, *call;

   args = mkIRExprVec_1(gr0);
   call = mkIRExprCCall(Ity_I32, 0 /*regparm*/,
                        "s390_do_pfpo", &s390_do_pfpo, args);
   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static const HChar *
s390_irgen_PFPO(void)
{
   IRTemp gr0 = newTemp(Ity_I32);     /* word 1 [32:63] of GR 0 */
   IRTemp test_bit = newTemp(Ity_I32); /* bit 32 of GR 0 - test validity */
   IRTemp fn = newTemp(Ity_I32);       /* [33:55] of GR 0 - function code */
   IRTemp ef = newTemp(Ity_I32);       /* Emulation Failure */
   IRTemp src1 = newTemp(Ity_F32);
   IRTemp dst1 = newTemp(Ity_D32);
   IRTemp src2 = newTemp(Ity_F32);
   IRTemp dst2 = newTemp(Ity_D64);
   IRTemp src3 = newTemp(Ity_F32);
   IRTemp dst3 = newTemp(Ity_D128);
   IRTemp src4 = newTemp(Ity_F64);
   IRTemp dst4 = newTemp(Ity_D32);
   IRTemp src5 = newTemp(Ity_F64);
   IRTemp dst5 = newTemp(Ity_D64);
   IRTemp src6 = newTemp(Ity_F64);
   IRTemp dst6 = newTemp(Ity_D128);
   IRTemp src7 = newTemp(Ity_F128);
   IRTemp dst7 = newTemp(Ity_D32);
   IRTemp src8 = newTemp(Ity_F128);
   IRTemp dst8 = newTemp(Ity_D64);
   IRTemp src9 = newTemp(Ity_F128);
   IRTemp dst9 = newTemp(Ity_D128);
   IRTemp src10 = newTemp(Ity_D32);
   IRTemp dst10 = newTemp(Ity_F32);
   IRTemp src11 = newTemp(Ity_D32);
   IRTemp dst11 = newTemp(Ity_F64);
   IRTemp src12 = newTemp(Ity_D32);
   IRTemp dst12 = newTemp(Ity_F128);
   IRTemp src13 = newTemp(Ity_D64);
   IRTemp dst13 = newTemp(Ity_F32);
   IRTemp src14 = newTemp(Ity_D64);
   IRTemp dst14 = newTemp(Ity_F64);
   IRTemp src15 = newTemp(Ity_D64);
   IRTemp dst15 = newTemp(Ity_F128);
   IRTemp src16 = newTemp(Ity_D128);
   IRTemp dst16 = newTemp(Ity_F32);
   IRTemp src17 = newTemp(Ity_D128);
   IRTemp dst17 = newTemp(Ity_F64);
   IRTemp src18 = newTemp(Ity_D128);
   IRTemp dst18 = newTemp(Ity_F128);
   IRExpr *irrm;

   if (! s390_host_has_pfpo) {
      emulation_failure(EmFail_S390X_pfpo);
      goto done;
   }

   assign(gr0, get_gpr_w1(0));
   /* get function code */
   assign(fn, binop(Iop_And32, binop(Iop_Shr32, mkexpr(gr0), mkU8(8)),
                    mkU32(0x7fffff)));
   /* get validity test bit */
   assign(test_bit, binop(Iop_And32, binop(Iop_Shr32, mkexpr(gr0), mkU8(31)),
                          mkU32(0x1)));
   irrm = get_rounding_mode_from_gr0();

   /* test_bit is 1 */
   assign(src1, get_fpr_w0(4)); /* get source from FPR 4,6 */
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_64, src1, gr0);

   /* Return code set in GR1 is usually 0. Non-zero value is set only
      when exceptions are raised. See Programming Notes point 5 in the
      instrcution description of pfpo in POP. Since valgrind does not
      model exception, it might be safe to just set 0 to GR 1. */
   put_gpr_w1(1, mkU32(0x0));
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(test_bit), mkU32(0x1)));

   /* Check validity of function code in GR 0 */
   assign(ef, s390_call_pfpo_helper(unop(Iop_32Uto64, mkexpr(gr0))));
   emulation_failure_with_expr(mkexpr(ef));

   stmt(
        IRStmt_Exit(
                    binop(Iop_CmpNE32, mkexpr(ef), mkU32(EmNote_NONE)),
                    Ijk_EmFail,
                    IRConst_U64(guest_IA_next_instr),
                    S390X_GUEST_OFFSET(guest_IA)
                    )
        );

   /* F32 -> D32 */
   /* get source from FPR 4,6 - already set in src1 */
   assign(dst1, binop(Iop_F32toD32, irrm, mkexpr(src1)));
   put_dpr_w0(0, mkexpr(dst1)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_32, src1, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F32_TO_D32)));

   /* F32 -> D64 */
   assign(src2, get_fpr_w0(4)); /* get source from FPR 4,6 */
   assign(dst2, binop(Iop_F32toD64, irrm, mkexpr(src2)));
   put_dpr_dw0(0, mkexpr(dst2)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_32, src2, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F32_TO_D64)));

   /* F32 -> D128 */
   assign(src3, get_fpr_w0(4)); /* get source from FPR 4,6 */
   assign(dst3, binop(Iop_F32toD128, irrm, mkexpr(src3)));
   put_dpr_pair(0, mkexpr(dst3)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_32, src3, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F32_TO_D128)));

   /* F64 -> D32 */
   assign(src4, get_fpr_dw0(4)); /* get source from FPR 4,6 */
   assign(dst4, binop(Iop_F64toD32, irrm, mkexpr(src4)));
   put_dpr_w0(0, mkexpr(dst4)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_64, src4, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F64_TO_D32)));

   /* F64 -> D64 */
   assign(src5, get_fpr_dw0(4)); /* get source from FPR 4,6 */
   assign(dst5, binop(Iop_F64toD64, irrm, mkexpr(src5)));
   put_dpr_dw0(0, mkexpr(dst5)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_64, src5, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F64_TO_D64)));

   /* F64 -> D128 */
   assign(src6, get_fpr_dw0(4)); /* get source from FPR 4,6 */
   assign(dst6, binop(Iop_F64toD128, irrm, mkexpr(src6)));
   put_dpr_pair(0, mkexpr(dst6)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_64, src6, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F64_TO_D128)));

   /* F128 -> D32 */
   assign(src7, get_fpr_pair(4)); /* get source from FPR 4,6 */
   assign(dst7, binop(Iop_F128toD32, irrm, mkexpr(src7)));
   put_dpr_w0(0, mkexpr(dst7)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_put1f128Z(S390_CC_OP_PFPO_128, src7, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F128_TO_D32)));

   /* F128 -> D64 */
   assign(src8, get_fpr_pair(4)); /* get source from FPR 4,6 */
   assign(dst8, binop(Iop_F128toD64, irrm, mkexpr(src8)));
   put_dpr_dw0(0, mkexpr(dst8)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_put1f128Z(S390_CC_OP_PFPO_128, src8, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F128_TO_D64)));

   /* F128 -> D128 */
   assign(src9, get_fpr_pair(4)); /* get source from FPR 4,6 */
   assign(dst9, binop(Iop_F128toD128, irrm, mkexpr(src9)));
   put_dpr_pair(0, mkexpr(dst9)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_put1f128Z(S390_CC_OP_PFPO_128, src9, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_F128_TO_D128)));

   /* D32 -> F32 */
   assign(src10, get_dpr_w0(4)); /* get source from FPR 4,6 */
   assign(dst10, binop(Iop_D32toF32, irrm, mkexpr(src10)));
   put_fpr_w0(0, mkexpr(dst10)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_32, src10, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D32_TO_F32)));

   /* D32 -> F64 */
   assign(src11, get_dpr_w0(4)); /* get source from FPR 4,6 */
   assign(dst11, binop(Iop_D32toF64, irrm, mkexpr(src11)));
   put_fpr_dw0(0, mkexpr(dst11)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_32, src11, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D32_TO_F64)));

   /* D32 -> F128 */
   assign(src12, get_dpr_w0(4)); /* get source from FPR 4,6 */
   assign(dst12, binop(Iop_D32toF128, irrm, mkexpr(src12)));
   put_fpr_pair(0, mkexpr(dst12)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_32, src12, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D32_TO_F128)));

   /* D64 -> F32 */
   assign(src13, get_dpr_dw0(4)); /* get source from FPR 4,6 */
   assign(dst13, binop(Iop_D64toF32, irrm, mkexpr(src13)));
   put_fpr_w0(0, mkexpr(dst13)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_64, src13, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D64_TO_F32)));

   /* D64 -> F64 */
   assign(src14, get_dpr_dw0(4)); /* get source from FPR 4,6 */
   assign(dst14, binop(Iop_D64toF64, irrm, mkexpr(src14)));
   put_fpr_dw0(0, mkexpr(dst14)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_64, src14, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D64_TO_F64)));

   /* D64 -> F128 */
   assign(src15, get_dpr_dw0(4)); /* get source from FPR 4,6 */
   assign(dst15, binop(Iop_D64toF128, irrm, mkexpr(src15)));
   put_fpr_pair(0, mkexpr(dst15)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_putFZ(S390_CC_OP_PFPO_64, src15, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D64_TO_F128)));

   /* D128 -> F32 */
   assign(src16, get_dpr_pair(4)); /* get source from FPR 4,6 */
   assign(dst16, binop(Iop_D128toF32, irrm, mkexpr(src16)));
   put_fpr_w0(0, mkexpr(dst16)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_put1d128Z(S390_CC_OP_PFPO_128, src16, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D128_TO_F32)));

   /* D128 -> F64 */
   assign(src17, get_dpr_pair(4)); /* get source from FPR 4,6 */
   assign(dst17, binop(Iop_D128toF64, irrm, mkexpr(src17)));
   put_fpr_dw0(0, mkexpr(dst17)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_put1d128Z(S390_CC_OP_PFPO_128, src17, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D128_TO_F64)));

   /* D128 -> F128 */
   assign(src18, get_dpr_pair(4)); /* get source from FPR 4,6 */
   assign(dst18, binop(Iop_D128toF128, irrm, mkexpr(src18)));
   put_fpr_pair(0, mkexpr(dst18)); /* put the result in FPR 0,2 */
   put_gpr_w1(1, mkU32(0x0));
   s390_cc_thunk_put1d128Z(S390_CC_OP_PFPO_128, src18, gr0);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(fn), mkU32(S390_PFPO_D128_TO_F128)));

 done:
   return "pfpo";
}

static const HChar *
s390_irgen_RLL(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp amount = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I32);

   assign(amount, binop(Iop_And64, mkexpr(op2addr), mkU64(31)));
   assign(op, get_gpr_w1(r3));
   put_gpr_w1(r1, binop(Iop_Or32, binop(Iop_Shl32, mkexpr(op), unop(Iop_64to8,
              mkexpr(amount))), binop(Iop_Shr32, mkexpr(op), unop(Iop_64to8,
              binop(Iop_Sub64, mkU64(32), mkexpr(amount))))));

   return "rll";
}

static const HChar *
s390_irgen_RLLG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp amount = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I64);

   assign(amount, binop(Iop_And64, mkexpr(op2addr), mkU64(63)));
   assign(op, get_gpr_dw0(r3));
   put_gpr_dw0(r1, binop(Iop_Or64, binop(Iop_Shl64, mkexpr(op), unop(Iop_64to8,
               mkexpr(amount))), binop(Iop_Shr64, mkexpr(op), unop(Iop_64to8,
               binop(Iop_Sub64, mkU64(64), mkexpr(amount))))));

   return "rllg";
}

static const HChar *
s390_irgen_RNSBG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   UChar from;
   UChar to;
   UChar rot;
   UChar t_bit;
   ULong mask;
   ULong maskc;
   IRTemp result = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   from = i3 & 63;
   to = i4 & 63;
   rot = i5 & 63;
   t_bit = i3 & 128;
   assign(op2, rot == 0 ? get_gpr_dw0(r2) : binop(Iop_Or64, binop(Iop_Shl64,
          get_gpr_dw0(r2), mkU8(rot)), binop(Iop_Shr64, get_gpr_dw0(r2),
          mkU8(64 - rot))));
   if (from <= to) {
      mask = ~0ULL;
      mask = (mask >> from) & (mask << (63 - to));
      maskc = ~mask;
   } else {
      maskc = ~0ULL;
      maskc = (maskc >> (to + 1)) & (maskc << (64 - from));
      mask = ~maskc;
   }
   assign(result, binop(Iop_And64, binop(Iop_And64, get_gpr_dw0(r1), mkexpr(op2)
          ), mkU64(mask)));
   if (t_bit == 0) {
      put_gpr_dw0(r1, binop(Iop_Or64, binop(Iop_And64, get_gpr_dw0(r1),
                  mkU64(maskc)), mkexpr(result)));
   }
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);

   return "rnsbg";
}

static const HChar *
s390_irgen_RXSBG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   UChar from;
   UChar to;
   UChar rot;
   UChar t_bit;
   ULong mask;
   ULong maskc;
   IRTemp result = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   from = i3 & 63;
   to = i4 & 63;
   rot = i5 & 63;
   t_bit = i3 & 128;
   assign(op2, rot == 0 ? get_gpr_dw0(r2) : binop(Iop_Or64, binop(Iop_Shl64,
          get_gpr_dw0(r2), mkU8(rot)), binop(Iop_Shr64, get_gpr_dw0(r2),
          mkU8(64 - rot))));
   if (from <= to) {
      mask = ~0ULL;
      mask = (mask >> from) & (mask << (63 - to));
      maskc = ~mask;
   } else {
      maskc = ~0ULL;
      maskc = (maskc >> (to + 1)) & (maskc << (64 - from));
      mask = ~maskc;
   }
   assign(result, binop(Iop_And64, binop(Iop_Xor64, get_gpr_dw0(r1), mkexpr(op2)
          ), mkU64(mask)));
   if (t_bit == 0) {
      put_gpr_dw0(r1, binop(Iop_Or64, binop(Iop_And64, get_gpr_dw0(r1),
                  mkU64(maskc)), mkexpr(result)));
   }
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);

   return "rxsbg";
}

static const HChar *
s390_irgen_ROSBG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   UChar from;
   UChar to;
   UChar rot;
   UChar t_bit;
   ULong mask;
   ULong maskc;
   IRTemp result = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   from = i3 & 63;
   to = i4 & 63;
   rot = i5 & 63;
   t_bit = i3 & 128;
   assign(op2, rot == 0 ? get_gpr_dw0(r2) : binop(Iop_Or64, binop(Iop_Shl64,
          get_gpr_dw0(r2), mkU8(rot)), binop(Iop_Shr64, get_gpr_dw0(r2),
          mkU8(64 - rot))));
   if (from <= to) {
      mask = ~0ULL;
      mask = (mask >> from) & (mask << (63 - to));
      maskc = ~mask;
   } else {
      maskc = ~0ULL;
      maskc = (maskc >> (to + 1)) & (maskc << (64 - from));
      mask = ~maskc;
   }
   assign(result, binop(Iop_And64, binop(Iop_Or64, get_gpr_dw0(r1), mkexpr(op2)
          ), mkU64(mask)));
   if (t_bit == 0) {
      put_gpr_dw0(r1, binop(Iop_Or64, binop(Iop_And64, get_gpr_dw0(r1),
                  mkU64(maskc)), mkexpr(result)));
   }
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);

   return "rosbg";
}

static const HChar *
s390_irgen_RISBGx(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5,
                  Bool set_cc)
{
   UChar from;
   UChar to;
   UChar rot;
   UChar z_bit;
   ULong mask;
   ULong maskc;
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   from = i3 & 63;
   to = i4 & 63;
   rot = i5 & 63;
   z_bit = i4 & 128;
   assign(op2, rot == 0 ? get_gpr_dw0(r2) : binop(Iop_Or64, binop(Iop_Shl64,
          get_gpr_dw0(r2), mkU8(rot)), binop(Iop_Shr64, get_gpr_dw0(r2),
          mkU8(64 - rot))));
   if (from <= to) {
      mask = ~0ULL;
      mask = (mask >> from) & (mask << (63 - to));
      maskc = ~mask;
   } else {
      maskc = ~0ULL;
      maskc = (maskc >> (to + 1)) & (maskc << (64 - from));
      mask = ~maskc;
   }
   if (z_bit == 0) {
      put_gpr_dw0(r1, binop(Iop_Or64, binop(Iop_And64, get_gpr_dw0(r1),
                  mkU64(maskc)), binop(Iop_And64, mkexpr(op2), mkU64(mask))));
   } else {
      put_gpr_dw0(r1, binop(Iop_And64, mkexpr(op2), mkU64(mask)));
   }
   assign(result, get_gpr_dw0(r1));
   if (set_cc) {
      s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, result);
      return "risbg";
   }

   return "risbgn";
}

static const HChar *
s390_irgen_RISBG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   return s390_irgen_RISBGx(r1, r2, i3, i4, i5, True);
}

static const HChar *
s390_irgen_RISBGN(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   return s390_irgen_RISBGx(r1, r2, i3, i4, i5, False);
}

static IRExpr *
s390_irgen_RISBxG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5,
                  Bool high)
{
   UChar from;
   UChar to;
   UChar rot;
   UChar z_bit;
   UInt mask;
   UInt maskc;
   IRTemp op2 = newTemp(Ity_I32);

   from = i3 & 31;
   to = i4 & 31;
   rot = i5 & 63;
   z_bit = i4 & 128;
   if (rot == 0) {
      assign(op2, high ? get_gpr_w0(r2) : get_gpr_w1(r2));
   } else if (rot == 32) {
      assign(op2, high ? get_gpr_w1(r2) : get_gpr_w0(r2));
   } else {
      assign(op2,
             unop(high ? Iop_64HIto32 : Iop_64to32,
                  binop(Iop_Or64,
                        binop(Iop_Shl64, get_gpr_dw0(r2), mkU8(rot)),
                        binop(Iop_Shr64, get_gpr_dw0(r2), mkU8(64 - rot)))));
   }
   if (from <= to) {
      mask = ~0U;
      mask = (mask >> from) & (mask << (31 - to));
      maskc = ~mask;
   } else {
      maskc = ~0U;
      maskc = (maskc >> (to + 1)) & (maskc << (32 - from));
      mask = ~maskc;
   }
   if (z_bit) {
      return binop(Iop_And32, mkexpr(op2), mkU32(mask));
   }
   return binop(Iop_Or32,
                binop(Iop_And32, high ? get_gpr_w0(r1) : get_gpr_w1(r1),
                      mkU32(maskc)),
                binop(Iop_And32, mkexpr(op2), mkU32(mask)));
}

static const HChar *
s390_irgen_RISBHG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   put_gpr_w0(r1, s390_irgen_RISBxG(r1, r2, i3, i4, i5, True));
   return "risbhg";
}

static const HChar *
s390_irgen_RISBLG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   put_gpr_w1(r1, s390_irgen_RISBxG(r1, r2, i3, i4, i5, False));
   return "risblg";
}

static const HChar *
s390_irgen_SAR(UChar r1, UChar r2)
{
   put_ar_w0(r1, get_gpr_w1(r2));
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, AR, GPR), "sar", r1, r2);

   return "sar";
}

static const HChar *
s390_irgen_SLDA(UChar r1, IRTemp op2addr)
{
   IRTemp p1 = newTemp(Ity_I64);
   IRTemp p2 = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);
   ULong sign_mask;
   IRTemp shift_amount = newTemp(Ity_I64);

   assign(p1, unop(Iop_32Uto64, get_gpr_w1(r1)));
   assign(p2, unop(Iop_32Uto64, get_gpr_w1(r1 + 1)));
   assign(op, binop(Iop_Or64, binop(Iop_Shl64, mkexpr(p1), mkU8(32)), mkexpr(p2)
          ));
   sign_mask = 1ULL << 63;
   assign(shift_amount, binop(Iop_And64, mkexpr(op2addr), mkU64(63)));
   assign(result, binop(Iop_Or64, binop(Iop_And64, binop(Iop_Shl64, mkexpr(op),
          unop(Iop_64to8, mkexpr(shift_amount))), mkU64(~sign_mask)),
          binop(Iop_And64, mkexpr(op), mkU64(sign_mask))));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));
   s390_cc_thunk_putZZ(S390_CC_OP_SHIFT_LEFT_64, op, shift_amount);

   return "slda";
}

static const HChar *
s390_irgen_SLDL(UChar r1, IRTemp op2addr)
{
   IRTemp p1 = newTemp(Ity_I64);
   IRTemp p2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(p1, unop(Iop_32Uto64, get_gpr_w1(r1)));
   assign(p2, unop(Iop_32Uto64, get_gpr_w1(r1 + 1)));
   assign(result, binop(Iop_Shl64, binop(Iop_Or64, binop(Iop_Shl64, mkexpr(p1),
          mkU8(32)), mkexpr(p2)), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));

   return "sldl";
}

static const HChar *
s390_irgen_SLA(UChar r1, IRTemp op2addr)
{
   IRTemp uop = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   UInt sign_mask;
   IRTemp shift_amount = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r1));
   assign(uop, get_gpr_w1(r1));
   sign_mask = 2147483648U;
   assign(shift_amount, binop(Iop_And64, mkexpr(op2addr), mkU64(63)));
   assign(result, binop(Iop_Or32, binop(Iop_And32, binop(Iop_Shl32, mkexpr(uop),
          unop(Iop_64to8, mkexpr(shift_amount))), mkU32(~sign_mask)),
          binop(Iop_And32, mkexpr(uop), mkU32(sign_mask))));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putZZ(S390_CC_OP_SHIFT_LEFT_32, op, shift_amount);

   return "sla";
}

static const HChar *
s390_irgen_SLAK(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp uop = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   UInt sign_mask;
   IRTemp shift_amount = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r3));
   assign(uop, get_gpr_w1(r3));
   sign_mask = 2147483648U;
   assign(shift_amount, binop(Iop_And64, mkexpr(op2addr), mkU64(63)));
   assign(result, binop(Iop_Or32, binop(Iop_And32, binop(Iop_Shl32, mkexpr(uop),
          unop(Iop_64to8, mkexpr(shift_amount))), mkU32(~sign_mask)),
          binop(Iop_And32, mkexpr(uop), mkU32(sign_mask))));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putZZ(S390_CC_OP_SHIFT_LEFT_32, op, shift_amount);

   return "slak";
}

static const HChar *
s390_irgen_SLAG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp uop = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);
   ULong sign_mask;
   IRTemp shift_amount = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I64);

   assign(op, get_gpr_dw0(r3));
   assign(uop, get_gpr_dw0(r3));
   sign_mask = 9223372036854775808ULL;
   assign(shift_amount, binop(Iop_And64, mkexpr(op2addr), mkU64(63)));
   assign(result, binop(Iop_Or64, binop(Iop_And64, binop(Iop_Shl64, mkexpr(uop),
          unop(Iop_64to8, mkexpr(shift_amount))), mkU64(~sign_mask)),
          binop(Iop_And64, mkexpr(uop), mkU64(sign_mask))));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putZZ(S390_CC_OP_SHIFT_LEFT_64, op, shift_amount);

   return "slag";
}

static const HChar *
s390_irgen_SLL(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, binop(Iop_Shl32, get_gpr_w1(r1), unop(Iop_64to8,
              binop(Iop_And64, mkexpr(op2addr), mkU64(63)))));

   return "sll";
}

static const HChar *
s390_irgen_SLLK(UChar r1, UChar r3, IRTemp op2addr)
{
   put_gpr_w1(r1, binop(Iop_Shl32, get_gpr_w1(r3), unop(Iop_64to8,
              binop(Iop_And64, mkexpr(op2addr), mkU64(63)))));

   return "sllk";
}

static const HChar *
s390_irgen_SLLG(UChar r1, UChar r3, IRTemp op2addr)
{
   put_gpr_dw0(r1, binop(Iop_Shl64, get_gpr_dw0(r3), unop(Iop_64to8,
               binop(Iop_And64, mkexpr(op2addr), mkU64(63)))));

   return "sllg";
}

static const HChar *
s390_irgen_SRDA(UChar r1, IRTemp op2addr)
{
   IRTemp p1 = newTemp(Ity_I64);
   IRTemp p2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(p1, unop(Iop_32Uto64, get_gpr_w1(r1)));
   assign(p2, unop(Iop_32Uto64, get_gpr_w1(r1 + 1)));
   assign(result, binop(Iop_Sar64, binop(Iop_Or64, binop(Iop_Shl64, mkexpr(p1),
          mkU8(32)), mkexpr(p2)), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, result);

   return "srda";
}

static const HChar *
s390_irgen_SRDL(UChar r1, IRTemp op2addr)
{
   IRTemp p1 = newTemp(Ity_I64);
   IRTemp p2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(p1, unop(Iop_32Uto64, get_gpr_w1(r1)));
   assign(p2, unop(Iop_32Uto64, get_gpr_w1(r1 + 1)));
   assign(result, binop(Iop_Shr64, binop(Iop_Or64, binop(Iop_Shl64, mkexpr(p1),
          mkU8(32)), mkexpr(p2)), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));

   return "srdl";
}

static const HChar *
s390_irgen_SRA(UChar r1, IRTemp op2addr)
{
   IRTemp result = newTemp(Ity_I32);
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r1));
   assign(result, binop(Iop_Sar32, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, result);

   return "sra";
}

static const HChar *
s390_irgen_SRAK(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp result = newTemp(Ity_I32);
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r3));
   assign(result, binop(Iop_Sar32, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, result);

   return "srak";
}

static const HChar *
s390_irgen_SRAG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp result = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I64);

   assign(op, get_gpr_dw0(r3));
   assign(result, binop(Iop_Sar64, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, result);

   return "srag";
}

static const HChar *
s390_irgen_SRL(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r1));
   put_gpr_w1(r1, binop(Iop_Shr32, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
              mkexpr(op2addr), mkU64(63)))));

   return "srl";
}

static const HChar *
s390_irgen_SRLK(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r3));
   put_gpr_w1(r1, binop(Iop_Shr32, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
              mkexpr(op2addr), mkU64(63)))));

   return "srlk";
}

static const HChar *
s390_irgen_SRLG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_I64);

   assign(op, get_gpr_dw0(r3));
   put_gpr_dw0(r1, binop(Iop_Shr64, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
               mkexpr(op2addr), mkU64(63)))));

   return "srlg";
}

static const HChar *
s390_irgen_ST(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_w1(r1));

   return "st";
}

static const HChar *
s390_irgen_STY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_w1(r1));

   return "sty";
}

static const HChar *
s390_irgen_STG(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_dw0(r1));

   return "stg";
}

static const HChar *
s390_irgen_STRL(UChar r1, UInt i2)
{
   store(mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)i2 << 1)),
         get_gpr_w1(r1));

   return "strl";
}

static const HChar *
s390_irgen_STGRL(UChar r1, UInt i2)
{
   store(mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)i2 << 1)),
         get_gpr_dw0(r1));

   return "stgrl";
}

static const HChar *
s390_irgen_STC(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));

   return "stc";
}

static const HChar *
s390_irgen_STCY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));

   return "stcy";
}

static const HChar *
s390_irgen_STCH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b3(r1));

   return "stch";
}

static const HChar *
s390_irgen_STCM(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar mask;
   UChar n;

   mask = (UChar)r3;
   n = 0;
   if ((mask & 8) != 0) {
      store(mkexpr(op2addr), get_gpr_b4(r1));
      n = n + 1;
   }
   if ((mask & 4) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b5(r1));
      n = n + 1;
   }
   if ((mask & 2) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b6(r1));
      n = n + 1;
   }
   if ((mask & 1) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b7(r1));
   }

   return "stcm";
}

static const HChar *
s390_irgen_STCMY(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar mask;
   UChar n;

   mask = (UChar)r3;
   n = 0;
   if ((mask & 8) != 0) {
      store(mkexpr(op2addr), get_gpr_b4(r1));
      n = n + 1;
   }
   if ((mask & 4) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b5(r1));
      n = n + 1;
   }
   if ((mask & 2) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b6(r1));
      n = n + 1;
   }
   if ((mask & 1) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b7(r1));
   }

   return "stcmy";
}

static const HChar *
s390_irgen_STCMH(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar mask;
   UChar n;

   mask = (UChar)r3;
   n = 0;
   if ((mask & 8) != 0) {
      store(mkexpr(op2addr), get_gpr_b0(r1));
      n = n + 1;
   }
   if ((mask & 4) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b1(r1));
      n = n + 1;
   }
   if ((mask & 2) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b2(r1));
      n = n + 1;
   }
   if ((mask & 1) != 0) {
      store(binop(Iop_Add64, mkexpr(op2addr), mkU64(n)), get_gpr_b3(r1));
   }

   return "stcmh";
}

static const HChar *
s390_irgen_STH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_hw3(r1));

   return "sth";
}

static const HChar *
s390_irgen_STHY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_hw3(r1));

   return "sthy";
}

static const HChar *
s390_irgen_STHRL(UChar r1, UInt i2)
{
   store(mkU64(guest_IA_curr_instr + ((ULong)(Long)(Int)i2 << 1)),
         get_gpr_hw3(r1));

   return "sthrl";
}

static const HChar *
s390_irgen_STHH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_hw1(r1));

   return "sthh";
}

static const HChar *
s390_irgen_STFH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_w0(r1));

   return "stfh";
}

static const HChar *
s390_irgen_STOC(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   store(mkexpr(op2addr), get_gpr_w1(r1));

   return "stoc";
}

static const HChar *
s390_irgen_STOCG(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   store(mkexpr(op2addr), get_gpr_dw0(r1));

   return "stocg";
}

static const HChar *
s390_irgen_STPQ(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_dw0(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(8)), get_gpr_dw0(r1 + 1));

   return "stpq";
}

static const HChar *
s390_irgen_STRVH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(1)), get_gpr_b6(r1));

   return "strvh";
}

static const HChar *
s390_irgen_STRV(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(1)), get_gpr_b6(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(2)), get_gpr_b5(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(3)), get_gpr_b4(r1));

   return "strv";
}

static const HChar *
s390_irgen_STRVG(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(1)), get_gpr_b6(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(2)), get_gpr_b5(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(3)), get_gpr_b4(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(4)), get_gpr_b3(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(5)), get_gpr_b2(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(6)), get_gpr_b1(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(7)), get_gpr_b0(r1));

   return "strvg";
}

static const HChar *
s390_irgen_SR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "sr";
}

static const HChar *
s390_irgen_SGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "sgr";
}

static const HChar *
s390_irgen_SGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "sgfr";
}

static const HChar *
s390_irgen_SRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op2, op3);
   put_gpr_w1(r1, mkexpr(result));

   return "srk";
}

static const HChar *
s390_irgen_SGRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Sub64, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_64, op2, op3);
   put_gpr_dw0(r1, mkexpr(result));

   return "sgrk";
}

static const HChar *
s390_irgen_S(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "s";
}

static const HChar *
s390_irgen_SY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "sy";
}

static const HChar *
s390_irgen_SG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "sg";
}

static const HChar *
s390_irgen_SGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "sgf";
}

static const HChar *
s390_irgen_SGH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_16Sto64, load(Ity_I16, mkexpr(op2addr))));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "sgh";
}

static const HChar *
s390_irgen_SH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "sh";
}

static const HChar *
s390_irgen_SHY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "shy";
}

static const HChar *
s390_irgen_SHHHR(UChar r3 __attribute__((unused)), UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r1));
   assign(op3, get_gpr_w0(r2));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));

   return "shhhr";
}

static const HChar *
s390_irgen_SHHLR(UChar r3 __attribute__((unused)), UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r1));
   assign(op3, get_gpr_w1(r2));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));

   return "shhlr";
}

static const HChar *
s390_irgen_SLR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "slr";
}

static const HChar *
s390_irgen_SLGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "slgr";
}

static const HChar *
s390_irgen_SLGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, get_gpr_w1(r2)));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "slgfr";
}

static const HChar *
s390_irgen_SLRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op2, op3);
   put_gpr_w1(r1, mkexpr(result));

   return "slrk";
}

static const HChar *
s390_irgen_SLGRK(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_Sub64, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_64, op2, op3);
   put_gpr_dw0(r1, mkexpr(result));

   return "slgrk";
}

static const HChar *
s390_irgen_SL(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "sl";
}

static const HChar *
s390_irgen_SLY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op1, op2);
   put_gpr_w1(r1, mkexpr(result));

   return "sly";
}

static const HChar *
s390_irgen_SLG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "slg";
}

static const HChar *
s390_irgen_SLGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_64, op1, op2);
   put_gpr_dw0(r1, mkexpr(result));

   return "slgf";
}

static const HChar *
s390_irgen_SLFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   op2 = i2;
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkU32(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op1, mktemp(Ity_I32,
                       mkU32(op2)));
   put_gpr_w1(r1, mkexpr(result));

   return "slfi";
}

static const HChar *
s390_irgen_SLGFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   op2 = (ULong)i2;
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkU64(op2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_64, op1, mktemp(Ity_I64,
                       mkU64(op2)));
   put_gpr_dw0(r1, mkexpr(result));

   return "slgfi";
}

static const HChar *
s390_irgen_SLHHHR(UChar r3 __attribute__((unused)), UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r1));
   assign(op3, get_gpr_w0(r2));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));

   return "slhhhr";
}

static const HChar *
s390_irgen_SLHHLR(UChar r3 __attribute__((unused)), UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r1));
   assign(op3, get_gpr_w1(r2));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));

   return "slhhlr";
}

static const HChar *
s390_irgen_SLBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp borrow_in = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   assign(borrow_in, binop(Iop_Sub32, mkU32(1), binop(Iop_Shr32,
          s390_call_calculate_cc(), mkU8(1))));
   assign(result, binop(Iop_Sub32, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)),
          mkexpr(borrow_in)));
   s390_cc_thunk_putZZZ(S390_CC_OP_UNSIGNED_SUBB_32, op1, op2, borrow_in);
   put_gpr_w1(r1, mkexpr(result));

   return "slbr";
}

static const HChar *
s390_irgen_SLBGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp borrow_in = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   assign(borrow_in, unop(Iop_32Uto64, binop(Iop_Sub32, mkU32(1),
          binop(Iop_Shr32, s390_call_calculate_cc(), mkU8(1)))));
   assign(result, binop(Iop_Sub64, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)),
          mkexpr(borrow_in)));
   s390_cc_thunk_putZZZ(S390_CC_OP_UNSIGNED_SUBB_64, op1, op2, borrow_in);
   put_gpr_dw0(r1, mkexpr(result));

   return "slbgr";
}

static const HChar *
s390_irgen_SLB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp borrow_in = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(borrow_in, binop(Iop_Sub32, mkU32(1), binop(Iop_Shr32,
          s390_call_calculate_cc(), mkU8(1))));
   assign(result, binop(Iop_Sub32, binop(Iop_Sub32, mkexpr(op1), mkexpr(op2)),
          mkexpr(borrow_in)));
   s390_cc_thunk_putZZZ(S390_CC_OP_UNSIGNED_SUBB_32, op1, op2, borrow_in);
   put_gpr_w1(r1, mkexpr(result));

   return "slb";
}

static const HChar *
s390_irgen_SLBG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp borrow_in = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(borrow_in, unop(Iop_32Uto64, binop(Iop_Sub32, mkU32(1),
          binop(Iop_Shr32, s390_call_calculate_cc(), mkU8(1)))));
   assign(result, binop(Iop_Sub64, binop(Iop_Sub64, mkexpr(op1), mkexpr(op2)),
          mkexpr(borrow_in)));
   s390_cc_thunk_putZZZ(S390_CC_OP_UNSIGNED_SUBB_64, op1, op2, borrow_in);
   put_gpr_dw0(r1, mkexpr(result));

   return "slbg";
}

static const HChar *
s390_irgen_SVC(UChar i)
{
   IRTemp sysno = newTemp(Ity_I64);

   if (i != 0) {
      assign(sysno, mkU64(i));
   } else {
      assign(sysno, unop(Iop_32Uto64, get_gpr_w1(1)));
   }
   system_call(mkexpr(sysno));

   return "svc";
}

static const HChar *
s390_irgen_TM(UChar i2, IRTemp op1addr)
{
   UChar mask;
   IRTemp value = newTemp(Ity_I8);

   mask = i2;
   assign(value, load(Ity_I8, mkexpr(op1addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_TEST_UNDER_MASK_8, value, mktemp(Ity_I8,
                       mkU8(mask)));

   return "tm";
}

static const HChar *
s390_irgen_TMY(UChar i2, IRTemp op1addr)
{
   UChar mask;
   IRTemp value = newTemp(Ity_I8);

   mask = i2;
   assign(value, load(Ity_I8, mkexpr(op1addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_TEST_UNDER_MASK_8, value, mktemp(Ity_I8,
                       mkU8(mask)));

   return "tmy";
}

static const HChar *
s390_irgen_TMHH(UChar r1, UShort i2)
{
   UShort mask;
   IRTemp value = newTemp(Ity_I16);

   mask = i2;
   assign(value, get_gpr_hw0(r1));
   s390_cc_thunk_putZZ(S390_CC_OP_TEST_UNDER_MASK_16, value, mktemp(Ity_I16,
                       mkU16(mask)));

   return "tmhh";
}

static const HChar *
s390_irgen_TMHL(UChar r1, UShort i2)
{
   UShort mask;
   IRTemp value = newTemp(Ity_I16);

   mask = i2;
   assign(value, get_gpr_hw1(r1));
   s390_cc_thunk_putZZ(S390_CC_OP_TEST_UNDER_MASK_16, value, mktemp(Ity_I16,
                       mkU16(mask)));

   return "tmhl";
}

static const HChar *
s390_irgen_TMLH(UChar r1, UShort i2)
{
   UShort mask;
   IRTemp value = newTemp(Ity_I16);

   mask = i2;
   assign(value, get_gpr_hw2(r1));
   s390_cc_thunk_putZZ(S390_CC_OP_TEST_UNDER_MASK_16, value, mktemp(Ity_I16,
                       mkU16(mask)));

   return "tmlh";
}

static const HChar *
s390_irgen_TMLL(UChar r1, UShort i2)
{
   UShort mask;
   IRTemp value = newTemp(Ity_I16);

   mask = i2;
   assign(value, get_gpr_hw3(r1));
   s390_cc_thunk_putZZ(S390_CC_OP_TEST_UNDER_MASK_16, value, mktemp(Ity_I16,
                       mkU16(mask)));

   return "tmll";
}

static const HChar *
s390_irgen_EFPC(UChar r1)
{
   put_gpr_w1(r1, get_fpc_w0());

   return "efpc";
}

static const HChar *
s390_irgen_LER(UChar r1, UChar r2)
{
   put_fpr_w0(r1, get_fpr_w0(r2));

   return "ler";
}

static const HChar *
s390_irgen_LDR(UChar r1, UChar r2)
{
   put_fpr_dw0(r1, get_fpr_dw0(r2));

   return "ldr";
}

static const HChar *
s390_irgen_LDER(UChar r1, UChar r2)
{
   put_fpr_dw0(r1, mkF64i(0x0));
   put_fpr_w0(r1, get_fpr_w0(r2));

   return "lder";
}

static const HChar *
s390_irgen_LXR(UChar r1, UChar r2)
{
   put_fpr_dw0(r1, get_fpr_dw0(r2));
   put_fpr_dw0(r1 + 2, get_fpr_dw0(r2 + 2));

   return "lxr";
}

static const HChar *
s390_irgen_LE(UChar r1, IRTemp op2addr)
{
   put_fpr_w0(r1, load(Ity_F32, mkexpr(op2addr)));

   return "le";
}

static const HChar *
s390_irgen_LD(UChar r1, IRTemp op2addr)
{
   put_fpr_dw0(r1, load(Ity_F64, mkexpr(op2addr)));

   return "ld";
}

static const HChar *
s390_irgen_LDE(UChar r1, IRTemp op2addr)
{
   put_fpr_dw0(r1, mkF64i(0x0));
   put_fpr_w0(r1, load(Ity_F32, mkexpr(op2addr)));

   return "lde";
}

static const HChar *
s390_irgen_LEY(UChar r1, IRTemp op2addr)
{
   put_fpr_w0(r1, load(Ity_F32, mkexpr(op2addr)));

   return "ley";
}

static const HChar *
s390_irgen_LDY(UChar r1, IRTemp op2addr)
{
   put_fpr_dw0(r1, load(Ity_F64, mkexpr(op2addr)));

   return "ldy";
}

static const HChar *
s390_irgen_LFPC(IRTemp op2addr)
{
   put_fpc_w0(load(Ity_I32, mkexpr(op2addr)));

   return "lfpc";
}

static const HChar *
s390_irgen_LZER(UChar r1)
{
   put_fpr_w0(r1, mkF32i(0x0));

   return "lzer";
}

static const HChar *
s390_irgen_LZDR(UChar r1)
{
   put_fpr_dw0(r1, mkF64i(0x0));

   return "lzdr";
}

static const HChar *
s390_irgen_LZXR(UChar r1)
{
   put_fpr_dw0(r1, mkF64i(0x0));
   put_fpr_dw0(r1 + 2, mkF64i(0x0));

   return "lzxr";
}

static const HChar *
s390_irgen_SRNM(IRTemp op2addr)
{
   UInt input_mask, fpc_mask;

   input_mask = 3;
   fpc_mask = s390_host_has_fpext ? 7 : 3;

   put_fpc_w0(binop(Iop_Or32,
                    binop(Iop_And32, get_fpc_w0(), mkU32(~fpc_mask)),
                    binop(Iop_And32, unop(Iop_64to32, mkexpr(op2addr)),
                          mkU32(input_mask))));
   return "srnm";
}

static const HChar *
s390_irgen_SRNMB(IRTemp op2addr)
{
   UInt input_mask, fpc_mask;

   input_mask = 7;
   fpc_mask = 7;

   put_fpc_w0(binop(Iop_Or32,
                    binop(Iop_And32, get_fpc_w0(), mkU32(~fpc_mask)),
                    binop(Iop_And32, unop(Iop_64to32, mkexpr(op2addr)),
                          mkU32(input_mask))));
   return "srnmb";
}

static void
s390_irgen_srnmb_wrapper(UChar b2, UShort d2)
{
   if (b2 == 0) {  /* This is the typical case */
      if (d2 > 3) {
         if (s390_host_has_fpext && d2 == 7) {
            /* ok */
         } else {
            emulation_warning(EmWarn_S390X_invalid_rounding);
            d2 = S390_FPC_BFP_ROUND_NEAREST_EVEN;
         }
      }
   }

   s390_format_S_RD(s390_irgen_SRNMB, b2, d2);
}

/* Wrapper to validate the parameter as in SRNMB is not required, as all
   the 8 values in op2addr[61:63] correspond to a valid DFP rounding mode */
static const HChar *
s390_irgen_SRNMT(IRTemp op2addr)
{
   UInt input_mask, fpc_mask;

   input_mask = 7;
   fpc_mask = 0x70;

   /* fpc[25:27] <- op2addr[61:63]
      fpc = (fpc & ~(0x70)) | ((op2addr & 7) << 4) */
   put_fpc_w0(binop(Iop_Or32, binop(Iop_And32, get_fpc_w0(), mkU32(~fpc_mask)),
                    binop(Iop_Shl32, binop(Iop_And32,
                                           unop(Iop_64to32, mkexpr(op2addr)),
                                           mkU32(input_mask)), mkU8(4))));
   return "srnmt";
}


static const HChar *
s390_irgen_SFPC(UChar r1)
{
   put_fpc_w0(get_gpr_w1(r1));

   return "sfpc";
}

static const HChar *
s390_irgen_STE(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpr_w0(r1));

   return "ste";
}

static const HChar *
s390_irgen_STD(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpr_dw0(r1));

   return "std";
}

static const HChar *
s390_irgen_STEY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpr_w0(r1));

   return "stey";
}

static const HChar *
s390_irgen_STDY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpr_dw0(r1));

   return "stdy";
}

static const HChar *
s390_irgen_STFPC(IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpc_w0());

   return "stfpc";
}

static const HChar *
s390_irgen_AEBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_w0(r1));
   assign(op2, get_fpr_w0(r2));
   assign(result, triop(Iop_AddF32, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_32, result);
   put_fpr_w0(r1, mkexpr(result));

   return "aebr";
}

static const HChar *
s390_irgen_ADBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, get_fpr_dw0(r2));
   assign(result, triop(Iop_AddF64, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_64, result);
   put_fpr_dw0(r1, mkexpr(result));

   return "adbr";
}

static const HChar *
s390_irgen_AEB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_w0(r1));
   assign(op2, load(Ity_F32, mkexpr(op2addr)));
   assign(result, triop(Iop_AddF32, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_32, result);
   put_fpr_w0(r1, mkexpr(result));

   return "aeb";
}

static const HChar *
s390_irgen_ADB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, load(Ity_F64, mkexpr(op2addr)));
   assign(result, triop(Iop_AddF64, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_64, result);
   put_fpr_dw0(r1, mkexpr(result));

   return "adb";
}

static const HChar *
s390_irgen_CEFBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_fpext && m3 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m3 = S390_BFP_ROUND_PER_FPC;
   }
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_w0(r1, binop(Iop_I32StoF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        mkexpr(op2)));

   return "cefbr";
}

static const HChar *
s390_irgen_CDFBR(UChar m3 __attribute__((unused)),
                 UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_dw0(r1, unop(Iop_I32StoF64, mkexpr(op2)));

   return "cdfbr";
}

static const HChar *
s390_irgen_CEGBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_fpext && m3 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m3 = S390_BFP_ROUND_PER_FPC;
   }
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_w0(r1, binop(Iop_I64StoF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        mkexpr(op2)));

   return "cegbr";
}

static const HChar *
s390_irgen_CDGBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_fpext && m3 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m3 = S390_BFP_ROUND_PER_FPC;
   }
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_dw0(r1, binop(Iop_I64StoF64, mkexpr(encode_bfp_rounding_mode(m3)),
                         mkexpr(op2)));

   return "cdgbr";
}

static const HChar *
s390_irgen_CELFBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op2 = newTemp(Ity_I32);

      assign(op2, get_gpr_w1(r2));
      put_fpr_w0(r1, binop(Iop_I32UtoF32, mkexpr(encode_bfp_rounding_mode(m3)),
                           mkexpr(op2)));
   }
   return "celfbr";
}

static const HChar *
s390_irgen_CDLFBR(UChar m3 __attribute__((unused)),
                  UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op2 = newTemp(Ity_I32);

      assign(op2, get_gpr_w1(r2));
      put_fpr_dw0(r1, unop(Iop_I32UtoF64, mkexpr(op2)));
   }
   return "cdlfbr";
}

static const HChar *
s390_irgen_CELGBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op2 = newTemp(Ity_I64);

      assign(op2, get_gpr_dw0(r2));
      put_fpr_w0(r1, binop(Iop_I64UtoF32, mkexpr(encode_bfp_rounding_mode(m3)),
                           mkexpr(op2)));
   }
   return "celgbr";
}

static const HChar *
s390_irgen_CDLGBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op2 = newTemp(Ity_I64);

      assign(op2, get_gpr_dw0(r2));
      put_fpr_dw0(r1, binop(Iop_I64UtoF64,
                            mkexpr(encode_bfp_rounding_mode(m3)),
                            mkexpr(op2)));
   }
   return "cdlgbr";
}

static const HChar *
s390_irgen_CLFEBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op = newTemp(Ity_F32);
      IRTemp result = newTemp(Ity_I32);
      IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

      assign(op, get_fpr_w0(r2));
      assign(result, binop(Iop_F32toI32U, mkexpr(rounding_mode),
                           mkexpr(op)));
      put_gpr_w1(r1, mkexpr(result));
      s390_cc_thunk_putFZ(S390_CC_OP_BFP_32_TO_UINT_32, op, rounding_mode);
   }
   return "clfebr";
}

static const HChar *
s390_irgen_CLFDBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op = newTemp(Ity_F64);
      IRTemp result = newTemp(Ity_I32);
      IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

      assign(op, get_fpr_dw0(r2));
      assign(result, binop(Iop_F64toI32U, mkexpr(rounding_mode),
                           mkexpr(op)));
      put_gpr_w1(r1, mkexpr(result));
      s390_cc_thunk_putFZ(S390_CC_OP_BFP_64_TO_UINT_32, op, rounding_mode);
   }
   return "clfdbr";
}

static const HChar *
s390_irgen_CLGEBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op = newTemp(Ity_F32);
      IRTemp result = newTemp(Ity_I64);
      IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

      assign(op, get_fpr_w0(r2));
      assign(result, binop(Iop_F32toI64U, mkexpr(rounding_mode),
                           mkexpr(op)));
      put_gpr_dw0(r1, mkexpr(result));
      s390_cc_thunk_putFZ(S390_CC_OP_BFP_32_TO_UINT_64, op, rounding_mode);
   }
   return "clgebr";
}

static const HChar *
s390_irgen_CLGDBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op = newTemp(Ity_F64);
      IRTemp result = newTemp(Ity_I64);
      IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

      assign(op, get_fpr_dw0(r2));
      assign(result, binop(Iop_F64toI64U, mkexpr(rounding_mode),
                           mkexpr(op)));
      put_gpr_dw0(r1, mkexpr(result));
      s390_cc_thunk_putFZ(S390_CC_OP_BFP_64_TO_UINT_64, op, rounding_mode);
   }
   return "clgdbr";
}

static const HChar *
s390_irgen_CFEBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_w0(r2));
   assign(result, binop(Iop_F32toI32S, mkexpr(rounding_mode),
          mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_32_TO_INT_32, op, rounding_mode);

   return "cfebr";
}

static const HChar *
s390_irgen_CFDBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_dw0(r2));
   assign(result, binop(Iop_F64toI32S, mkexpr(rounding_mode),
          mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_64_TO_INT_32, op, rounding_mode);

   return "cfdbr";
}

static const HChar *
s390_irgen_CGEBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_w0(r2));
   assign(result, binop(Iop_F32toI64S, mkexpr(rounding_mode),
          mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_32_TO_INT_64, op, rounding_mode);

   return "cgebr";
}

static const HChar *
s390_irgen_CGDBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_dw0(r2));
   assign(result, binop(Iop_F64toI64S, mkexpr(rounding_mode),
          mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_64_TO_INT_64, op, rounding_mode);

   return "cgdbr";
}

static const HChar *
s390_irgen_DEBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_w0(r1));
   assign(op2, get_fpr_w0(r2));
   assign(result, triop(Iop_DivF32, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   put_fpr_w0(r1, mkexpr(result));

   return "debr";
}

static const HChar *
s390_irgen_DDBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, get_fpr_dw0(r2));
   assign(result, triop(Iop_DivF64, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "ddbr";
}

static const HChar *
s390_irgen_DEB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_w0(r1));
   assign(op2, load(Ity_F32, mkexpr(op2addr)));
   assign(result, triop(Iop_DivF32, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   put_fpr_w0(r1, mkexpr(result));

   return "deb";
}

static const HChar *
s390_irgen_DDB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, load(Ity_F64, mkexpr(op2addr)));
   assign(result, triop(Iop_DivF64, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "ddb";
}

static const HChar *
s390_irgen_LTEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, get_fpr_w0(r2));
   put_fpr_w0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_32, result);

   return "ltebr";
}

static const HChar *
s390_irgen_LTDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, get_fpr_dw0(r2));
   put_fpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_64, result);

   return "ltdbr";
}

static const HChar *
s390_irgen_LCEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, unop(Iop_NegF32, get_fpr_w0(r2)));
   put_fpr_w0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_32, result);

   return "lcebr";
}

static const HChar *
s390_irgen_LCDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_NegF64, get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_64, result);

   return "lcdbr";
}

static const HChar *
s390_irgen_LDEBR(UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F32);

   assign(op, get_fpr_w0(r2));
   put_fpr_dw0(r1, unop(Iop_F32toF64, mkexpr(op)));

   return "ldebr";
}

static const HChar *
s390_irgen_LDEB(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_F32);

   assign(op, load(Ity_F32, mkexpr(op2addr)));
   put_fpr_dw0(r1, unop(Iop_F32toF64, mkexpr(op)));

   return "ldeb";
}

static const HChar *
s390_irgen_LEDBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_fpext && m3 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m3 = S390_BFP_ROUND_PER_FPC;
   }
   IRTemp op = newTemp(Ity_F64);

   assign(op, get_fpr_dw0(r2));
   put_fpr_w0(r1, binop(Iop_F64toF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        mkexpr(op)));

   return "ledbr";
}

static const HChar *
s390_irgen_MEEBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_F32);
   IRRoundingMode rounding_mode =
      encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_w0(r1));
   assign(op2, get_fpr_w0(r2));
   assign(result, triop(Iop_MulF32, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   put_fpr_w0(r1, mkexpr(result));

   return "meebr";
}

static const HChar *
s390_irgen_MDBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, get_fpr_dw0(r2));
   assign(result, triop(Iop_MulF64, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "mdbr";
}

static const HChar *
s390_irgen_MEEB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_w0(r1));
   assign(op2, load(Ity_F32, mkexpr(op2addr)));
   assign(result, triop(Iop_MulF32, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   put_fpr_w0(r1, mkexpr(result));

   return "meeb";
}

static const HChar *
s390_irgen_MDB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, load(Ity_F64, mkexpr(op2addr)));
   assign(result, triop(Iop_MulF64, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "mdb";
}

static const HChar *
s390_irgen_SEBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_w0(r1));
   assign(op2, get_fpr_w0(r2));
   assign(result, triop(Iop_SubF32, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_32, result);
   put_fpr_w0(r1, mkexpr(result));

   return "sebr";
}

static const HChar *
s390_irgen_SDBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, get_fpr_dw0(r2));
   assign(result, triop(Iop_SubF64, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_64, result);
   put_fpr_dw0(r1, mkexpr(result));

   return "sdbr";
}

static const HChar *
s390_irgen_SEB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_w0(r1));
   assign(op2, load(Ity_F32, mkexpr(op2addr)));
   assign(result, triop(Iop_SubF32, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_32, result);
   put_fpr_w0(r1, mkexpr(result));

   return "seb";
}

static const HChar *
s390_irgen_SDB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, load(Ity_F64, mkexpr(op2addr)));
   assign(result, triop(Iop_SubF64, mkexpr(rounding_mode), mkexpr(op1),
          mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_64, result);
   put_fpr_dw0(r1, mkexpr(result));

   return "sdb";
}

static const HChar *
s390_irgen_ADTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D64);
      IRTemp op2 = newTemp(Ity_D64);
      IRTemp result = newTemp(Ity_D64);
      IRTemp rounding_mode;

      if (! s390_host_has_fpext && m4 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_dw0(r2));
      assign(op2, get_dpr_dw0(r3));
      assign(result, triop(Iop_AddD64, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      s390_cc_thunk_putF(S390_CC_OP_DFP_RESULT_64, result);
      put_dpr_dw0(r1, mkexpr(result));
   }
   return (m4 == 0) ? "adtr" : "adtra";
}

static const HChar *
s390_irgen_AXTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D128);
      IRTemp op2 = newTemp(Ity_D128);
      IRTemp result = newTemp(Ity_D128);
      IRTemp rounding_mode;

      if (! s390_host_has_fpext && m4 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_pair(r2));
      assign(op2, get_dpr_pair(r3));
      assign(result, triop(Iop_AddD128, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      put_dpr_pair(r1, mkexpr(result));

      s390_cc_thunk_put1d128(S390_CC_OP_DFP_RESULT_128, result);
   }
   return (m4 == 0) ? "axtr" : "axtra";
}

static const HChar *
s390_irgen_CDTR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_D64);
   IRTemp op2 = newTemp(Ity_D64);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_dpr_dw0(r1));
   assign(op2, get_dpr_dw0(r2));
   assign(cc_vex, binop(Iop_CmpD64, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_dfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);

   return "cdtr";
}

static const HChar *
s390_irgen_CXTR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_D128);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_dpr_pair(r1));
   assign(op2, get_dpr_pair(r2));
   assign(cc_vex, binop(Iop_CmpD128, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_dfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);

   return "cxtr";
}

static const HChar *
s390_irgen_CDFTR(UChar m3 __attribute__((unused)),
                 UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op2 = newTemp(Ity_I32);

         assign(op2, get_gpr_w1(r2));
         put_dpr_dw0(r1, unop(Iop_I32StoD64, mkexpr(op2)));
      }
   }
   return "cdftr";
}

static const HChar *
s390_irgen_CXFTR(UChar m3 __attribute__((unused)),
                 UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op2 = newTemp(Ity_I32);

         assign(op2, get_gpr_w1(r2));
         put_dpr_pair(r1, unop(Iop_I32StoD128, mkexpr(op2)));
      }
   }
   return "cxftr";
}

static const HChar *
s390_irgen_CDGTRA(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op2 = newTemp(Ity_I64);

      if (! s390_host_has_fpext && m3 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m3 = S390_DFP_ROUND_PER_FPC_0;
      }

      assign(op2, get_gpr_dw0(r2));
      put_dpr_dw0(r1, binop(Iop_I64StoD64, mkexpr(encode_dfp_rounding_mode(m3)),
                            mkexpr(op2)));
   }
   return (m3 == 0) ? "cdgtr" : "cdgtra";
}

static const HChar *
s390_irgen_CXGTR(UChar m3 __attribute__((unused)),
                 UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op2 = newTemp(Ity_I64);

      /* No emulation warning here about an non-zero m3 on hosts without
         floating point extension facility. No rounding is performed */

      assign(op2, get_gpr_dw0(r2));
      put_dpr_pair(r1, unop(Iop_I64StoD128, mkexpr(op2)));
   }
   return "cxgtr";
}

static const HChar *
s390_irgen_CDLFTR(UChar m3 __attribute__((unused)),
                  UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op2 = newTemp(Ity_I32);

         assign(op2, get_gpr_w1(r2));
         put_dpr_dw0(r1, unop(Iop_I32UtoD64, mkexpr(op2)));
      }
   }
   return "cdlftr";
}

static const HChar *
s390_irgen_CXLFTR(UChar m3 __attribute__((unused)),
                  UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op2 = newTemp(Ity_I32);

         assign(op2, get_gpr_w1(r2));
         put_dpr_pair(r1, unop(Iop_I32UtoD128, mkexpr(op2)));
      }
   }
   return "cxlftr";
}

static const HChar *
s390_irgen_CDLGTR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op2 = newTemp(Ity_I64);

         assign(op2, get_gpr_dw0(r2));
         put_dpr_dw0(r1, binop(Iop_I64UtoD64,
                               mkexpr(encode_dfp_rounding_mode(m3)),
                               mkexpr(op2)));
      }
   }
   return "cdlgtr";
}

static const HChar *
s390_irgen_CXLGTR(UChar m3 __attribute__((unused)),
                  UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op2 = newTemp(Ity_I64);

         assign(op2, get_gpr_dw0(r2));
         put_dpr_pair(r1, unop(Iop_I64UtoD128, mkexpr(op2)));
      }
   }
   return "cxlgtr";
}

static const HChar *
s390_irgen_CFDTR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op = newTemp(Ity_D64);
         IRTemp result = newTemp(Ity_I32);
         IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

         assign(op, get_dpr_dw0(r2));
         assign(result, binop(Iop_D64toI32S, mkexpr(rounding_mode),
                              mkexpr(op)));
         put_gpr_w1(r1, mkexpr(result));
         s390_cc_thunk_putFZ(S390_CC_OP_DFP_64_TO_INT_32, op, rounding_mode);
      }
   }
   return "cfdtr";
}

static const HChar *
s390_irgen_CFXTR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op = newTemp(Ity_D128);
         IRTemp result = newTemp(Ity_I32);
         IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

         assign(op, get_dpr_pair(r2));
         assign(result, binop(Iop_D128toI32S, mkexpr(rounding_mode),
                              mkexpr(op)));
         put_gpr_w1(r1, mkexpr(result));
         s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_128_TO_INT_32, op,
                                 rounding_mode);
      }
   }
   return "cfxtr";
}

static const HChar *
s390_irgen_CGDTR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op = newTemp(Ity_D64);
      IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

      /* If fpext is not installed and m3 is in 1:7,
         rounding mode performed is unpredictable */
      if (! s390_host_has_fpext && m3 > 0 && m3 < 8) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m3 = S390_DFP_ROUND_PER_FPC_0;
      }

      assign(op, get_dpr_dw0(r2));
      put_gpr_dw0(r1, binop(Iop_D64toI64S, mkexpr(rounding_mode), mkexpr(op)));
      s390_cc_thunk_putFZ(S390_CC_OP_DFP_64_TO_INT_64, op, rounding_mode);
   }
   return "cgdtr";
}

static const HChar *
s390_irgen_CGXTR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op = newTemp(Ity_D128);
      IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

      /* If fpext is not installed and m3 is in 1:7,
         rounding mode performed is unpredictable */
      if (! s390_host_has_fpext && m3 > 0 && m3 < 8) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m3 = S390_DFP_ROUND_PER_FPC_0;
      }
      assign(op, get_dpr_pair(r2));
      put_gpr_dw0(r1, binop(Iop_D128toI64S, mkexpr(rounding_mode), mkexpr(op)));
      s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_128_TO_INT_64, op, rounding_mode);
   }
   return "cgxtr";
}

static const HChar *
s390_irgen_CEDTR(UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D64);
      IRTemp op2 = newTemp(Ity_D64);
      IRTemp cc_vex  = newTemp(Ity_I32);
      IRTemp cc_s390 = newTemp(Ity_I32);

      assign(op1, get_dpr_dw0(r1));
      assign(op2, get_dpr_dw0(r2));
      assign(cc_vex, binop(Iop_CmpExpD64, mkexpr(op1), mkexpr(op2)));

      assign(cc_s390, convert_vex_dfpcc_to_s390(cc_vex));
      s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);
   }
   return "cedtr";
}

static const HChar *
s390_irgen_CEXTR(UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D128);
      IRTemp op2 = newTemp(Ity_D128);
      IRTemp cc_vex  = newTemp(Ity_I32);
      IRTemp cc_s390 = newTemp(Ity_I32);

      assign(op1, get_dpr_pair(r1));
      assign(op2, get_dpr_pair(r2));
      assign(cc_vex, binop(Iop_CmpExpD128, mkexpr(op1), mkexpr(op2)));

      assign(cc_s390, convert_vex_dfpcc_to_s390(cc_vex));
      s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);
   }
   return "cextr";
}

static const HChar *
s390_irgen_CLFDTR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op = newTemp(Ity_D64);
         IRTemp result = newTemp(Ity_I32);
         IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

         assign(op, get_dpr_dw0(r2));
         assign(result, binop(Iop_D64toI32U, mkexpr(rounding_mode),
                              mkexpr(op)));
         put_gpr_w1(r1, mkexpr(result));
         s390_cc_thunk_putFZ(S390_CC_OP_DFP_64_TO_UINT_32, op, rounding_mode);
      }
   }
   return "clfdtr";
}

static const HChar *
s390_irgen_CLFXTR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op = newTemp(Ity_D128);
         IRTemp result = newTemp(Ity_I32);
         IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

         assign(op, get_dpr_pair(r2));
         assign(result, binop(Iop_D128toI32U, mkexpr(rounding_mode),
                              mkexpr(op)));
         put_gpr_w1(r1, mkexpr(result));
         s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_128_TO_UINT_32, op,
                                 rounding_mode);
      }
   }
   return "clfxtr";
}

static const HChar *
s390_irgen_CLGDTR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op = newTemp(Ity_D64);
         IRTemp result = newTemp(Ity_I64);
         IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

         assign(op, get_dpr_dw0(r2));
         assign(result, binop(Iop_D64toI64U, mkexpr(rounding_mode),
                              mkexpr(op)));
         put_gpr_dw0(r1, mkexpr(result));
         s390_cc_thunk_putFZ(S390_CC_OP_DFP_64_TO_UINT_64, op, rounding_mode);
      }
   }
   return "clgdtr";
}

static const HChar *
s390_irgen_CLGXTR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      if (! s390_host_has_fpext) {
         emulation_failure(EmFail_S390X_fpext);
      } else {
         IRTemp op = newTemp(Ity_D128);
         IRTemp result = newTemp(Ity_I64);
         IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

         assign(op, get_dpr_pair(r2));
         assign(result, binop(Iop_D128toI64U, mkexpr(rounding_mode),
                              mkexpr(op)));
         put_gpr_dw0(r1, mkexpr(result));
         s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_128_TO_UINT_64, op,
                                 rounding_mode);
      }
   }
   return "clgxtr";
}

static const HChar *
s390_irgen_DDTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D64);
      IRTemp op2 = newTemp(Ity_D64);
      IRTemp result = newTemp(Ity_D64);
      IRTemp rounding_mode;

      if (! s390_host_has_fpext && m4 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_dw0(r2));
      assign(op2, get_dpr_dw0(r3));
      assign(result, triop(Iop_DivD64, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      put_dpr_dw0(r1, mkexpr(result));
   }
   return (m4 == 0) ? "ddtr" : "ddtra";
}

static const HChar *
s390_irgen_DXTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D128);
      IRTemp op2 = newTemp(Ity_D128);
      IRTemp result = newTemp(Ity_D128);
      IRTemp rounding_mode;

      if (! s390_host_has_fpext && m4 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_pair(r2));
      assign(op2, get_dpr_pair(r3));
      assign(result, triop(Iop_DivD128, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      put_dpr_pair(r1, mkexpr(result));
   }
   return (m4 == 0) ? "dxtr" : "dxtra";
}

static const HChar *
s390_irgen_EEDTR(UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      put_gpr_dw0(r1, unop(Iop_ExtractExpD64, get_dpr_dw0(r2)));
   }
   return "eedtr";
}

static const HChar *
s390_irgen_EEXTR(UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      put_gpr_dw0(r1, unop(Iop_ExtractExpD128, get_dpr_pair(r2)));
   }
   return "eextr";
}

static const HChar *
s390_irgen_ESDTR(UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      put_gpr_dw0(r1, unop(Iop_ExtractSigD64, get_dpr_dw0(r2)));
   }
   return "esdtr";
}

static const HChar *
s390_irgen_ESXTR(UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      put_gpr_dw0(r1, unop(Iop_ExtractSigD128, get_dpr_pair(r2)));
   }
   return "esxtr";
}

static const HChar *
s390_irgen_IEDTR(UChar r3, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_I64);
      IRTemp op2 = newTemp(Ity_D64);
      IRTemp result = newTemp(Ity_D64);

      assign(op1, get_gpr_dw0(r2));
      assign(op2, get_dpr_dw0(r3));
      assign(result, binop(Iop_InsertExpD64, mkexpr(op1), mkexpr(op2)));
      put_dpr_dw0(r1, mkexpr(result));
   }
   return "iedtr";
}

static const HChar *
s390_irgen_IEXTR(UChar r3, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_I64);
      IRTemp op2 = newTemp(Ity_D128);
      IRTemp result = newTemp(Ity_D128);

      assign(op1, get_gpr_dw0(r2));
      assign(op2, get_dpr_pair(r3));
      assign(result, binop(Iop_InsertExpD128, mkexpr(op1), mkexpr(op2)));
      put_dpr_pair(r1, mkexpr(result));
   }
   return "iextr";
}

static const HChar *
s390_irgen_LDETR(UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op = newTemp(Ity_D32);

      assign(op, get_dpr_w0(r2));
      put_dpr_dw0(r1, unop(Iop_D32toD64, mkexpr(op)));
   }
   return "ldetr";
}

static const HChar *
s390_irgen_LXDTR(UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_D64);

   assign(op, get_dpr_dw0(r2));
   put_dpr_pair(r1, unop(Iop_D64toD128, mkexpr(op)));

   return "lxdtr";
}

static const HChar *
s390_irgen_LDXTR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      /* If fpext is not installed and m3 is in 1:7,
         rounding mode performed is unpredictable */
      if (! s390_host_has_fpext && m3 > 0 && m3 < 8) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m3 = S390_DFP_ROUND_PER_FPC_0;
      }
      IRTemp result = newTemp(Ity_D64);

      assign(result, binop(Iop_D128toD64, mkexpr(encode_dfp_rounding_mode(m3)),
                           get_dpr_pair(r2)));
      put_dpr_dw0(r1, mkexpr(result));
   }
   return "ldxtr";
}

static const HChar *
s390_irgen_LEDTR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      /* If fpext is not installed and m3 is in 1:7,
         rounding mode performed is unpredictable */
      if (! s390_host_has_fpext && m3 > 0 && m3 < 8) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m3 = S390_DFP_ROUND_PER_FPC_0;
      }
      IRTemp op = newTemp(Ity_D64);

      assign(op, get_dpr_dw0(r2));
      put_dpr_w0(r1, binop(Iop_D64toD32, mkexpr(encode_dfp_rounding_mode(m3)),
                           mkexpr(op)));
   }
   return "ledtr";
}

static const HChar *
s390_irgen_LTDTR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_D64);

   assign(result, get_dpr_dw0(r2));
   put_dpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_DFP_RESULT_64, result);

   return "ltdtr";
}

static const HChar *
s390_irgen_LTXTR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_D128);

   assign(result, get_dpr_pair(r2));
   put_dpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1d128(S390_CC_OP_DFP_RESULT_128, result);

   return "ltxtr";
}

static const HChar *
s390_irgen_MDTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D64);
      IRTemp op2 = newTemp(Ity_D64);
      IRTemp result = newTemp(Ity_D64);
      IRTemp rounding_mode;

      if (! s390_host_has_fpext && m4 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_dw0(r2));
      assign(op2, get_dpr_dw0(r3));
      assign(result, triop(Iop_MulD64, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      put_dpr_dw0(r1, mkexpr(result));
   }
   return (m4 == 0) ? "mdtr" : "mdtra";
}

static const HChar *
s390_irgen_MXTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D128);
      IRTemp op2 = newTemp(Ity_D128);
      IRTemp result = newTemp(Ity_D128);
      IRTemp rounding_mode;

      if (! s390_host_has_fpext && m4 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_pair(r2));
      assign(op2, get_dpr_pair(r3));
      assign(result, triop(Iop_MulD128, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      put_dpr_pair(r1, mkexpr(result));
   }
   return (m4 == 0) ? "mxtr" : "mxtra";
}

static const HChar *
s390_irgen_QADTR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D64);
      IRTemp op2 = newTemp(Ity_D64);
      IRTemp result = newTemp(Ity_D64);
      IRTemp rounding_mode;

      /* If fpext is not installed and m4 is in 1:7,
         rounding mode performed is unpredictable */
      if (! s390_host_has_fpext && m4 > 0 && m4 < 8) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_dw0(r2));
      assign(op2, get_dpr_dw0(r3));
      assign(result, triop(Iop_QuantizeD64, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      put_dpr_dw0(r1, mkexpr(result));
   }
   return "qadtr";
}

static const HChar *
s390_irgen_QAXTR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D128);
      IRTemp op2 = newTemp(Ity_D128);
      IRTemp result = newTemp(Ity_D128);
      IRTemp rounding_mode;

      /* If fpext is not installed and m4 is in 1:7,
         rounding mode performed is unpredictable */
      if (! s390_host_has_fpext && m4 > 0 && m4 < 8) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_pair(r2));
      assign(op2, get_dpr_pair(r3));
      assign(result, triop(Iop_QuantizeD128, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      put_dpr_pair(r1, mkexpr(result));
   }
   return "qaxtr";
}

static const HChar *
s390_irgen_RRDTR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_I8);
      IRTemp op2 = newTemp(Ity_D64);
      IRTemp result = newTemp(Ity_D64);
      IRTemp rounding_mode;

      /* If fpext is not installed and m4 is in 1:7,
         rounding mode performed is unpredictable */
      if (! s390_host_has_fpext && m4 > 0 && m4 < 8) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_gpr_b7(r2));
      assign(op2, get_dpr_dw0(r3));
      assign(result, triop(Iop_SignificanceRoundD64, mkexpr(rounding_mode),
                           mkexpr(op1), mkexpr(op2)));
      put_dpr_dw0(r1, mkexpr(result));
   }
   return "rrdtr";
}

static const HChar *
s390_irgen_RRXTR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_I8);
      IRTemp op2 = newTemp(Ity_D128);
      IRTemp result = newTemp(Ity_D128);
      IRTemp rounding_mode;

      /* If fpext is not installed and m4 is in 1:7,
         rounding mode performed is unpredictable */
      if (! s390_host_has_fpext && m4 > 0 && m4 < 8) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_gpr_b7(r2));
      assign(op2, get_dpr_pair(r3));
      assign(result, triop(Iop_SignificanceRoundD128, mkexpr(rounding_mode),
                           mkexpr(op1), mkexpr(op2)));
      put_dpr_pair(r1, mkexpr(result));
   }
   return "rrxtr";
}

static const HChar *
s390_irgen_SDTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D64);
      IRTemp op2 = newTemp(Ity_D64);
      IRTemp result = newTemp(Ity_D64);
      IRTemp rounding_mode;

      if (! s390_host_has_fpext && m4 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_dw0(r2));
      assign(op2, get_dpr_dw0(r3));
      assign(result, triop(Iop_SubD64, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      s390_cc_thunk_putF(S390_CC_OP_DFP_RESULT_64, result);
      put_dpr_dw0(r1, mkexpr(result));
   }
   return (m4 == 0) ? "sdtr" : "sdtra";
}

static const HChar *
s390_irgen_SXTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op1 = newTemp(Ity_D128);
      IRTemp op2 = newTemp(Ity_D128);
      IRTemp result = newTemp(Ity_D128);
      IRTemp rounding_mode;

      if (! s390_host_has_fpext && m4 != S390_DFP_ROUND_PER_FPC_0) {
         emulation_warning(EmWarn_S390X_fpext_rounding);
         m4 = S390_DFP_ROUND_PER_FPC_0;
      }

      rounding_mode = encode_dfp_rounding_mode(m4);
      assign(op1, get_dpr_pair(r2));
      assign(op2, get_dpr_pair(r3));
      assign(result, triop(Iop_SubD128, mkexpr(rounding_mode), mkexpr(op1),
                           mkexpr(op2)));
      put_dpr_pair(r1, mkexpr(result));

      s390_cc_thunk_put1d128(S390_CC_OP_DFP_RESULT_128, result);
   }
   return (m4 == 0) ? "sxtr" : "sxtra";
}

static const HChar *
s390_irgen_SLDT(UChar r3, IRTemp op2addr, UChar r1)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op = newTemp(Ity_D64);

      assign(op, get_dpr_dw0(r3));
      put_dpr_dw0(r1, binop(Iop_ShlD64, mkexpr(op),
                            unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr),
                                                  mkU64(63)))));
   }
   return "sldt";
}

static const HChar *
s390_irgen_SLXT(UChar r3, IRTemp op2addr, UChar r1)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op = newTemp(Ity_D128);

      assign(op, get_dpr_pair(r3));
      put_dpr_pair(r1, binop(Iop_ShlD128, mkexpr(op),
                             unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr),
                                                   mkU64(63)))));
   }
   return "slxt";
}

static const HChar *
s390_irgen_SRDT(UChar r3, IRTemp op2addr, UChar r1)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op = newTemp(Ity_D64);

      assign(op, get_dpr_dw0(r3));
      put_dpr_dw0(r1, binop(Iop_ShrD64, mkexpr(op),
                            unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr),
                                                  mkU64(63)))));
   }
   return "srdt";
}

static const HChar *
s390_irgen_SRXT(UChar r3, IRTemp op2addr, UChar r1)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp op = newTemp(Ity_D128);

      assign(op, get_dpr_pair(r3));
      put_dpr_pair(r1, binop(Iop_ShrD128, mkexpr(op),
                             unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr),
                                                   mkU64(63)))));
   }
   return "srxt";
}

static const HChar *
s390_irgen_TDCET(UChar r1, IRTemp op2addr)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp value = newTemp(Ity_D32);

      assign(value, get_dpr_w0(r1));

      s390_cc_thunk_putFZ(S390_CC_OP_DFP_TDC_32, value, op2addr);
   }
   return "tdcet";
}

static const HChar *
s390_irgen_TDCDT(UChar r1, IRTemp op2addr)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp value = newTemp(Ity_D64);

      assign(value, get_dpr_dw0(r1));

      s390_cc_thunk_putFZ(S390_CC_OP_DFP_TDC_64, value, op2addr);
   }
   return "tdcdt";
}

static const HChar *
s390_irgen_TDCXT(UChar r1, IRTemp op2addr)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp value = newTemp(Ity_D128);

      assign(value, get_dpr_pair(r1));

      s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_TDC_128, value, op2addr);
   }
   return "tdcxt";
}

static const HChar *
s390_irgen_TDGET(UChar r1, IRTemp op2addr)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp value = newTemp(Ity_D32);

      assign(value, get_dpr_w0(r1));

      s390_cc_thunk_putFZ(S390_CC_OP_DFP_TDG_32, value, op2addr);
   }
   return "tdget";
}

static const HChar *
s390_irgen_TDGDT(UChar r1, IRTemp op2addr)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp value = newTemp(Ity_D64);

      assign(value, get_dpr_dw0(r1));

      s390_cc_thunk_putFZ(S390_CC_OP_DFP_TDG_64, value, op2addr);
   }
   return "tdgdt";
}

static const HChar *
s390_irgen_TDGXT(UChar r1, IRTemp op2addr)
{
   if (! s390_host_has_dfp) {
      emulation_failure(EmFail_S390X_DFP_insn);
   } else {
      IRTemp value = newTemp(Ity_D128);

      assign(value, get_dpr_pair(r1));

      s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_TDG_128, value, op2addr);
   }
   return "tdgxt";
}

static const HChar *
s390_irgen_CLC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I64);

   assign(len, mkU64(length));
   s390_irgen_CLC_EX(len, start1, start2);

   return "clc";
}

static const HChar *
s390_irgen_CLCL(UChar r1, UChar r2)
{
   IRTemp addr1 = newTemp(Ity_I64);
   IRTemp addr2 = newTemp(Ity_I64);
   IRTemp addr1_load = newTemp(Ity_I64);
   IRTemp addr2_load = newTemp(Ity_I64);
   IRTemp len1 = newTemp(Ity_I32);
   IRTemp len2 = newTemp(Ity_I32);
   IRTemp r1p1 = newTemp(Ity_I32);   /* contents of r1 + 1 */
   IRTemp r2p1 = newTemp(Ity_I32);   /* contents of r2 + 1 */
   IRTemp single1 = newTemp(Ity_I8);
   IRTemp single2 = newTemp(Ity_I8);
   IRTemp pad = newTemp(Ity_I8);

   assign(addr1, get_gpr_dw0(r1));
   assign(r1p1, get_gpr_w1(r1 + 1));
   assign(len1, binop(Iop_And32, mkexpr(r1p1), mkU32(0x00ffffff)));
   assign(addr2, get_gpr_dw0(r2));
   assign(r2p1, get_gpr_w1(r2 + 1));
   assign(len2, binop(Iop_And32, mkexpr(r2p1), mkU32(0x00ffffff)));
   assign(pad, get_gpr_b4(r2 + 1));

   /* len1 == 0 and len2 == 0? Exit */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpEQ32, binop(Iop_Or32, mkexpr(len1),
                                         mkexpr(len2)), mkU32(0)));

   /* Because mkite evaluates both the then-clause and the else-clause
      we cannot load directly from addr1 here. If len1 is 0, then adddr1
      may be NULL and loading from there would segfault. So we provide a
      valid dummy address in that case. Loading from there does no harm and
      the value will be discarded at runtime. */
   assign(addr1_load,
          mkite(binop(Iop_CmpEQ32, mkexpr(len1), mkU32(0)),
                mkU64(guest_IA_curr_instr), mkexpr(addr1)));
   assign(single1,
          mkite(binop(Iop_CmpEQ32, mkexpr(len1), mkU32(0)),
                mkexpr(pad), load(Ity_I8, mkexpr(addr1_load))));

   assign(addr2_load,
          mkite(binop(Iop_CmpEQ32, mkexpr(len2), mkU32(0)),
                mkU64(guest_IA_curr_instr), mkexpr(addr2)));
   assign(single2,
          mkite(binop(Iop_CmpEQ32, mkexpr(len2), mkU32(0)),
                mkexpr(pad), load(Ity_I8, mkexpr(addr2_load))));

   s390_cc_thunk_put2(S390_CC_OP_UNSIGNED_COMPARE, single1, single2, False);
   /* Fields differ ? */
   next_insn_if(binop(Iop_CmpNE8, mkexpr(single1), mkexpr(single2)));

   /* Update len1 and addr1, unless len1 == 0. */
   put_gpr_dw0(r1,
               mkite(binop(Iop_CmpEQ32, mkexpr(len1), mkU32(0)),
                     mkexpr(addr1),
                     binop(Iop_Add64, mkexpr(addr1), mkU64(1))));

   /* When updating len1 we must not modify bits (r1+1)[0:39] */
   put_gpr_w1(r1 + 1,
              mkite(binop(Iop_CmpEQ32, mkexpr(len1), mkU32(0)),
                    binop(Iop_And32, mkexpr(r1p1), mkU32(0xFF000000u)),
                    binop(Iop_Sub32, mkexpr(r1p1), mkU32(1))));

   /* Update len2 and addr2, unless len2 == 0. */
   put_gpr_dw0(r2,
               mkite(binop(Iop_CmpEQ32, mkexpr(len2), mkU32(0)),
                     mkexpr(addr2),
                     binop(Iop_Add64, mkexpr(addr2), mkU64(1))));

   /* When updating len2 we must not modify bits (r2+1)[0:39] */
   put_gpr_w1(r2 + 1,
              mkite(binop(Iop_CmpEQ32, mkexpr(len2), mkU32(0)),
                    binop(Iop_And32, mkexpr(r2p1), mkU32(0xFF000000u)),
                    binop(Iop_Sub32, mkexpr(r2p1), mkU32(1))));

   iterate();

   return "clcl";
}

static const HChar *
s390_irgen_CLCLE(UChar r1, UChar r3, IRTemp pad2)
{
   IRTemp addr1, addr3, addr1_load, addr3_load, len1, len3, single1, single3;

   addr1 = newTemp(Ity_I64);
   addr3 = newTemp(Ity_I64);
   addr1_load = newTemp(Ity_I64);
   addr3_load = newTemp(Ity_I64);
   len1 = newTemp(Ity_I64);
   len3 = newTemp(Ity_I64);
   single1 = newTemp(Ity_I8);
   single3 = newTemp(Ity_I8);

   assign(addr1, get_gpr_dw0(r1));
   assign(len1, get_gpr_dw0(r1 + 1));
   assign(addr3, get_gpr_dw0(r3));
   assign(len3, get_gpr_dw0(r3 + 1));

   /* len1 == 0 and len3 == 0? Exit */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpEQ64,binop(Iop_Or64, mkexpr(len1),
                                        mkexpr(len3)), mkU64(0)));

   /* A mux requires both ways to be possible. This is a way to prevent clcle
      from reading from addr1 if it should read from the pad. Since the pad
      has no address, just read from the instruction, we discard that anyway */
   assign(addr1_load,
          mkite(binop(Iop_CmpEQ64, mkexpr(len1), mkU64(0)),
                mkU64(guest_IA_curr_instr), mkexpr(addr1)));

   /* same for addr3 */
   assign(addr3_load,
          mkite(binop(Iop_CmpEQ64, mkexpr(len3), mkU64(0)),
                mkU64(guest_IA_curr_instr), mkexpr(addr3)));

   assign(single1,
          mkite(binop(Iop_CmpEQ64, mkexpr(len1), mkU64(0)),
                unop(Iop_64to8, mkexpr(pad2)),
                load(Ity_I8, mkexpr(addr1_load))));

   assign(single3,
          mkite(binop(Iop_CmpEQ64, mkexpr(len3), mkU64(0)),
                unop(Iop_64to8, mkexpr(pad2)),
                load(Ity_I8, mkexpr(addr3_load))));

   s390_cc_thunk_put2(S390_CC_OP_UNSIGNED_COMPARE, single1, single3, False);
   /* Both fields differ ? */
   next_insn_if(binop(Iop_CmpNE8, mkexpr(single1), mkexpr(single3)));

   /* If a length in 0 we must not change this length and the address */
   put_gpr_dw0(r1,
               mkite(binop(Iop_CmpEQ64, mkexpr(len1), mkU64(0)),
                     mkexpr(addr1),
                     binop(Iop_Add64, mkexpr(addr1), mkU64(1))));

   put_gpr_dw0(r1 + 1,
               mkite(binop(Iop_CmpEQ64, mkexpr(len1), mkU64(0)),
                     mkU64(0), binop(Iop_Sub64, mkexpr(len1), mkU64(1))));

   put_gpr_dw0(r3,
               mkite(binop(Iop_CmpEQ64, mkexpr(len3), mkU64(0)),
                     mkexpr(addr3),
                     binop(Iop_Add64, mkexpr(addr3), mkU64(1))));

   put_gpr_dw0(r3 + 1,
               mkite(binop(Iop_CmpEQ64, mkexpr(len3), mkU64(0)),
                     mkU64(0), binop(Iop_Sub64, mkexpr(len3), mkU64(1))));

   iterate();

   return "clcle";
}


static void
s390_irgen_XC_EX(IRTemp length, IRTemp start1, IRTemp start2)
{
   s390_irgen_xonc(Iop_Xor8, length, start1, start2);
}


static void
s390_irgen_NC_EX(IRTemp length, IRTemp start1, IRTemp start2)
{
   s390_irgen_xonc(Iop_And8, length, start1, start2);
}


static void
s390_irgen_OC_EX(IRTemp length, IRTemp start1, IRTemp start2)
{
   s390_irgen_xonc(Iop_Or8, length, start1, start2);
}


static void
s390_irgen_CLC_EX(IRTemp length, IRTemp start1, IRTemp start2)
{
   IRTemp current1 = newTemp(Ity_I8);
   IRTemp current2 = newTemp(Ity_I8);
   IRTemp counter = newTemp(Ity_I64);

   assign(counter, get_counter_dw0());
   put_counter_dw0(mkU64(0));

   assign(current1, load(Ity_I8, binop(Iop_Add64, mkexpr(start1),
                                       mkexpr(counter))));
   assign(current2, load(Ity_I8, binop(Iop_Add64, mkexpr(start2),
                                       mkexpr(counter))));
   s390_cc_thunk_put2(S390_CC_OP_UNSIGNED_COMPARE, current1, current2,
                      False);

   /* Both fields differ ? */
   next_insn_if(binop(Iop_CmpNE8, mkexpr(current1), mkexpr(current2)));

   /* Check for end of field */
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   iterate_if(binop(Iop_CmpNE64, mkexpr(counter), mkexpr(length)));
   put_counter_dw0(mkU64(0));
}

static void
s390_irgen_MVC_EX(IRTemp length, IRTemp start1, IRTemp start2)
{
   IRTemp counter = newTemp(Ity_I64);

   assign(counter, get_counter_dw0());

   store(binop(Iop_Add64, mkexpr(start1), mkexpr(counter)),
         load(Ity_I8, binop(Iop_Add64, mkexpr(start2), mkexpr(counter))));

   /* Check for end of field */
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   iterate_if(binop(Iop_CmpNE64, mkexpr(counter), mkexpr(length)));
   put_counter_dw0(mkU64(0));
}

static void
s390_irgen_MVCIN_EX(IRTemp length, IRTemp start1, IRTemp start2)
{
   IRTemp counter = newTemp(Ity_I64);

   assign(counter, get_counter_dw0());

   store(binop(Iop_Add64, mkexpr(start1), mkexpr(counter)),
         load(Ity_I8, binop(Iop_Sub64, mkexpr(start2), mkexpr(counter))));

   /* Check for end of field */
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   iterate_if(binop(Iop_CmpNE64, mkexpr(counter), mkexpr(length)));
   put_counter_dw0(mkU64(0));
}

static void
s390_irgen_TR_EX(IRTemp length, IRTemp start1, IRTemp start2)
{
   IRTemp op = newTemp(Ity_I8);
   IRTemp op1 = newTemp(Ity_I8);
   IRTemp result = newTemp(Ity_I64);
   IRTemp counter = newTemp(Ity_I64);

   assign(counter, get_counter_dw0());

   assign(op, load(Ity_I8, binop(Iop_Add64, mkexpr(start1), mkexpr(counter))));

   assign(result, binop(Iop_Add64, unop(Iop_8Uto64, mkexpr(op)), mkexpr(start2)));

   assign(op1, load(Ity_I8, mkexpr(result)));
   store(binop(Iop_Add64, mkexpr(start1), mkexpr(counter)), mkexpr(op1));

   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   iterate_if(binop(Iop_CmpNE64, mkexpr(counter), mkexpr(length)));
   put_counter_dw0(mkU64(0));
}


static void
s390_irgen_EX_SS(UChar r, IRTemp addr2,
                 void (*irgen)(IRTemp length, IRTemp start1, IRTemp start2),
                 UInt lensize)
{
   IRTemp cond;
   IRDirty *d;
   IRTemp torun;
   ULong ovl;

   IRTemp start1 = newTemp(Ity_I64);
   IRTemp start2 = newTemp(Ity_I64);
   IRTemp len = newTemp(lensize == 64 ? Ity_I64 : Ity_I32);
   cond = newTemp(Ity_I1);
   torun = newTemp(Ity_I64);

   assign(torun, load(Ity_I64, mkexpr(addr2)));
   /* Start with a check that the saved code is still correct */
   assign(cond, binop(Iop_CmpNE64, mkexpr(torun), mkU64(last_execute_target)));
   /* If not, save the new value */
   d = unsafeIRDirty_0_N (0, "s390x_dirtyhelper_EX", &s390x_dirtyhelper_EX,
                          mkIRExprVec_1(mkexpr(torun)));
   d->guard = mkexpr(cond);
   stmt(IRStmt_Dirty(d));

   /* and restart */
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMSTART),
                   mkU64(guest_IA_curr_instr)));
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMLEN), mkU64(4)));
   restart_if(mkexpr(cond));

   ovl = last_execute_target;
   assign(start1, binop(Iop_Add64, mkU64(SS_d1(ovl)),
          SS_b1(ovl) != 0 ? get_gpr_dw0(SS_b1(ovl)) : mkU64(0)));
   assign(start2, binop(Iop_Add64, mkU64(SS_d2(ovl)),
          SS_b2(ovl) != 0 ? get_gpr_dw0(SS_b2(ovl)) : mkU64(0)));
   assign(len, unop(lensize == 64 ? Iop_8Uto64 : Iop_8Uto32, binop(Iop_Or8,
          r != 0 ? get_gpr_b7(r): mkU8(0), mkU8(SS_l(ovl)))));
   irgen(len, start1, start2);

   last_execute_target = 0;
}

static const HChar *
s390_irgen_EX(UChar r1, IRTemp addr2)
{
   switch(last_execute_target & 0xff00000000000000ULL) {
   case 0:
   {
      /* no code information yet */
      IRDirty *d;

      /* so safe the code... */
      d = unsafeIRDirty_0_N (0, "s390x_dirtyhelper_EX", &s390x_dirtyhelper_EX,
                             mkIRExprVec_1(load(Ity_I64, mkexpr(addr2))));
      stmt(IRStmt_Dirty(d));
      /* and restart */
      stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMSTART),
                      mkU64(guest_IA_curr_instr)));
      stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMLEN), mkU64(4)));
      restart_if(IRExpr_Const(IRConst_U1(True)));

      /* we know that this will be invalidated */
      put_IA(mkaddr_expr(guest_IA_next_instr));
      dis_res->whatNext = Dis_StopHere;
      dis_res->jk_StopHere = Ijk_InvalICache;
      break;
   }

   case 0xd200000000000000ULL:
      /* special case MVC */
      s390_irgen_EX_SS(r1, addr2, s390_irgen_MVC_EX, 64);
      return "ex@mvc";

   case 0xd500000000000000ULL:
      /* special case CLC */
      s390_irgen_EX_SS(r1, addr2, s390_irgen_CLC_EX, 64);
      return "ex@clc";

   case 0xd700000000000000ULL:
      /* special case XC */
      s390_irgen_EX_SS(r1, addr2, s390_irgen_XC_EX, 32);
      return "ex@xc";

   case 0xd600000000000000ULL:
      /* special case OC */
      s390_irgen_EX_SS(r1, addr2, s390_irgen_OC_EX, 32);
      return "ex@oc";

   case 0xd400000000000000ULL:
      /* special case NC */
      s390_irgen_EX_SS(r1, addr2, s390_irgen_NC_EX, 32);
      return "ex@nc";

   case 0xdc00000000000000ULL:
      /* special case TR */
      s390_irgen_EX_SS(r1, addr2, s390_irgen_TR_EX, 64);
      return "ex@tr";

   case 0xe800000000000000ULL:
      /* special case MVCIN */
      s390_irgen_EX_SS(r1, addr2, s390_irgen_MVCIN_EX, 64);
      return "ex@mvcin";

   default:
   {
      /* everything else will get a self checking prefix that also checks the
         register content */
      IRDirty *d;
      UChar *bytes;
      IRTemp cond;
      IRTemp orperand;
      IRTemp torun;

      cond = newTemp(Ity_I1);
      orperand = newTemp(Ity_I64);
      torun = newTemp(Ity_I64);

      if (r1 == 0)
         assign(orperand, mkU64(0));
      else
         assign(orperand, unop(Iop_8Uto64,get_gpr_b7(r1)));
      /* This code is going to be translated */
      assign(torun, binop(Iop_Or64, load(Ity_I64, mkexpr(addr2)),
             binop(Iop_Shl64, mkexpr(orperand), mkU8(48))));

      /* Start with a check that saved code is still correct */
      assign(cond, binop(Iop_CmpNE64, mkexpr(torun),
             mkU64(last_execute_target)));
      /* If not, save the new value */
      d = unsafeIRDirty_0_N (0, "s390x_dirtyhelper_EX", &s390x_dirtyhelper_EX,
                             mkIRExprVec_1(mkexpr(torun)));
      d->guard = mkexpr(cond);
      stmt(IRStmt_Dirty(d));

      /* and restart */
      stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMSTART), mkU64(guest_IA_curr_instr)));
      stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMLEN), mkU64(4)));
      restart_if(mkexpr(cond));

      /* Now comes the actual translation */
      bytes = (UChar *) &last_execute_target;
      s390_decode_and_irgen(bytes, ((((bytes[0] >> 6) + 1) >> 1) + 1) << 1,
                            dis_res);
      if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
         vex_printf("    which was executed by\n");
      /* dont make useless translations in the next execute */
      last_execute_target = 0;
   }
   }
   return "ex";
}

static const HChar *
s390_irgen_EXRL(UChar r1, UInt offset)
{
   IRTemp addr = newTemp(Ity_I64);
   Addr64 bytes_addr = guest_IA_curr_instr + offset * 2UL;
   UChar *bytes = (UChar *)(HWord)bytes_addr;
   /* we might save one round trip because we know the target */
   if (!last_execute_target)
      last_execute_target = ((ULong)bytes[0] << 56) | ((ULong)bytes[1] << 48) |
                            ((ULong)bytes[2] << 40) | ((ULong)bytes[3] << 32) |
                            ((ULong)bytes[4] << 24) | ((ULong)bytes[5] << 16);
   assign(addr, mkU64(bytes_addr));
   s390_irgen_EX(r1, addr);
   return "exrl";
}

static const HChar *
s390_irgen_IPM(UChar r1)
{
   // As long as we dont support SPM, lets just assume 0 as program mask
   put_gpr_b4(r1, unop(Iop_32to8, binop(Iop_Or32, mkU32(0 /* program mask */),
                       binop(Iop_Shl32, s390_call_calculate_cc(), mkU8(4)))));

   return "ipm";
}


static const HChar *
s390_irgen_SRST(UChar r1, UChar r2)
{
   IRTemp address = newTemp(Ity_I64);
   IRTemp next = newTemp(Ity_I64);
   IRTemp delim = newTemp(Ity_I8);
   IRTemp counter = newTemp(Ity_I64);
   IRTemp byte = newTemp(Ity_I8);

   assign(address, get_gpr_dw0(r2));
   assign(next, get_gpr_dw0(r1));

   assign(counter, get_counter_dw0());
   put_counter_dw0(mkU64(0));

   // start = next?  CC=2 and out r1 and r2 unchanged
   s390_cc_set_val(2);
   put_gpr_dw0(r2, binop(Iop_Sub64, mkexpr(address), mkexpr(counter)));
   next_insn_if(binop(Iop_CmpEQ64, mkexpr(address), mkexpr(next)));

   assign(byte, load(Ity_I8, mkexpr(address)));
   assign(delim, get_gpr_b7(0));

   // byte = delim? CC=1, R1=address
   s390_cc_set_val(1);
   put_gpr_dw0(r1,  mkexpr(address));
   next_insn_if(binop(Iop_CmpEQ8, mkexpr(delim), mkexpr(byte)));

   // else: all equal, no end yet, loop
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   put_gpr_dw0(r1, mkexpr(next));
   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(address), mkU64(1)));

   iterate();

   return "srst";
}

static const HChar *
s390_irgen_CLST(UChar r1, UChar r2)
{
   IRTemp address1 = newTemp(Ity_I64);
   IRTemp address2 = newTemp(Ity_I64);
   IRTemp end = newTemp(Ity_I8);
   IRTemp counter = newTemp(Ity_I64);
   IRTemp byte1 = newTemp(Ity_I8);
   IRTemp byte2 = newTemp(Ity_I8);

   assign(address1, get_gpr_dw0(r1));
   assign(address2, get_gpr_dw0(r2));
   assign(end, get_gpr_b7(0));
   assign(counter, get_counter_dw0());
   put_counter_dw0(mkU64(0));
   assign(byte1, load(Ity_I8, mkexpr(address1)));
   assign(byte2, load(Ity_I8, mkexpr(address2)));

   // end in both? all equal, reset r1 and r2 to start values
   s390_cc_set_val(0);
   put_gpr_dw0(r1, binop(Iop_Sub64, mkexpr(address1), mkexpr(counter)));
   put_gpr_dw0(r2, binop(Iop_Sub64, mkexpr(address2), mkexpr(counter)));
   next_insn_if(binop(Iop_CmpEQ8, mkU8(0),
                      binop(Iop_Or8,
                            binop(Iop_Xor8, mkexpr(byte1), mkexpr(end)),
                            binop(Iop_Xor8, mkexpr(byte2), mkexpr(end)))));

   put_gpr_dw0(r1, mkexpr(address1));
   put_gpr_dw0(r2, mkexpr(address2));

   // End found in string1
   s390_cc_set_val(1);
   next_insn_if(binop(Iop_CmpEQ8, mkexpr(end), mkexpr(byte1)));

   // End found in string2
   s390_cc_set_val(2);
   next_insn_if(binop(Iop_CmpEQ8, mkexpr(end), mkexpr(byte2)));

   // string1 < string2
   s390_cc_set_val(1);
   next_insn_if(binop(Iop_CmpLT32U, unop(Iop_8Uto32, mkexpr(byte1)),
                      unop(Iop_8Uto32, mkexpr(byte2))));

   // string2 < string1
   s390_cc_set_val(2);
   next_insn_if(binop(Iop_CmpLT32U, unop(Iop_8Uto32, mkexpr(byte2)),
                      unop(Iop_8Uto32, mkexpr(byte1))));

   // else: all equal, no end yet, loop
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), mkU64(1)));
   put_gpr_dw0(r2, binop(Iop_Add64, get_gpr_dw0(r2), mkU64(1)));

   iterate();

   return "clst";
}

static void
s390_irgen_load_multiple_32bit(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar reg;
   IRTemp addr = newTemp(Ity_I64);

   assign(addr, mkexpr(op2addr));
   reg = r1;
   do {
      IRTemp old = addr;

      reg %= 16;
      put_gpr_w1(reg, load(Ity_I32, mkexpr(addr)));
      addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, mkexpr(old), mkU64(4)));
      reg++;
   } while (reg != (r3 + 1));
}

static const HChar *
s390_irgen_LM(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_multiple_32bit(r1, r3, op2addr);

   return "lm";
}

static const HChar *
s390_irgen_LMY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_multiple_32bit(r1, r3, op2addr);

   return "lmy";
}

static const HChar *
s390_irgen_LMH(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar reg;
   IRTemp addr = newTemp(Ity_I64);

   assign(addr, mkexpr(op2addr));
   reg = r1;
   do {
      IRTemp old = addr;

      reg %= 16;
      put_gpr_w0(reg, load(Ity_I32, mkexpr(addr)));
      addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, mkexpr(old), mkU64(4)));
      reg++;
   } while (reg != (r3 + 1));

   return "lmh";
}

static const HChar *
s390_irgen_LMG(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar reg;
   IRTemp addr = newTemp(Ity_I64);

   assign(addr, mkexpr(op2addr));
   reg = r1;
   do {
      IRTemp old = addr;

      reg %= 16;
      put_gpr_dw0(reg, load(Ity_I64, mkexpr(addr)));
      addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, mkexpr(old), mkU64(8)));
      reg++;
   } while (reg != (r3 + 1));

   return "lmg";
}

static void
s390_irgen_store_multiple_32bit(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar reg;
   IRTemp addr = newTemp(Ity_I64);

   assign(addr, mkexpr(op2addr));
   reg = r1;
   do {
      IRTemp old = addr;

      reg %= 16;
      store(mkexpr(addr), get_gpr_w1(reg));
      addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, mkexpr(old), mkU64(4)));
      reg++;
   } while( reg != (r3 + 1));
}

static const HChar *
s390_irgen_STM(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_store_multiple_32bit(r1, r3, op2addr);

   return "stm";
}

static const HChar *
s390_irgen_STMY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_store_multiple_32bit(r1, r3, op2addr);

   return "stmy";
}

static const HChar *
s390_irgen_STMH(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar reg;
   IRTemp addr = newTemp(Ity_I64);

   assign(addr, mkexpr(op2addr));
   reg = r1;
   do {
      IRTemp old = addr;

      reg %= 16;
      store(mkexpr(addr), get_gpr_w0(reg));
      addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, mkexpr(old), mkU64(4)));
      reg++;
   } while( reg != (r3 + 1));

   return "stmh";
}

static const HChar *
s390_irgen_STMG(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar reg;
   IRTemp addr = newTemp(Ity_I64);

   assign(addr, mkexpr(op2addr));
   reg = r1;
   do {
      IRTemp old = addr;

      reg %= 16;
      store(mkexpr(addr), get_gpr_dw0(reg));
      addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, mkexpr(old), mkU64(8)));
      reg++;
   } while( reg != (r3 + 1));

   return "stmg";
}

static void
s390_irgen_xonc(IROp op, IRTemp length, IRTemp start1, IRTemp start2)
{
   IRTemp old1 = newTemp(Ity_I8);
   IRTemp old2 = newTemp(Ity_I8);
   IRTemp new1 = newTemp(Ity_I8);
   IRTemp counter = newTemp(Ity_I32);
   IRTemp addr1 = newTemp(Ity_I64);

   assign(counter, get_counter_w0());

   assign(addr1, binop(Iop_Add64, mkexpr(start1),
                       unop(Iop_32Uto64, mkexpr(counter))));

   assign(old1, load(Ity_I8, mkexpr(addr1)));
   assign(old2, load(Ity_I8, binop(Iop_Add64, mkexpr(start2),
                                   unop(Iop_32Uto64,mkexpr(counter)))));
   assign(new1, binop(op, mkexpr(old1), mkexpr(old2)));

   /* Special case: xc is used to zero memory */
   if (op == Iop_Xor8) {
      store(mkexpr(addr1),
            mkite(binop(Iop_CmpEQ64, mkexpr(start1), mkexpr(start2)),
                  mkU8(0), mkexpr(new1)));
   } else
      store(mkexpr(addr1), mkexpr(new1));
   put_counter_w1(binop(Iop_Or32, unop(Iop_8Uto32, mkexpr(new1)),
                        get_counter_w1()));

   /* Check for end of field */
   put_counter_w0(binop(Iop_Add32, mkexpr(counter), mkU32(1)));
   iterate_if(binop(Iop_CmpNE32, mkexpr(counter), mkexpr(length)));
   s390_cc_thunk_put1(S390_CC_OP_BITWISE, mktemp(Ity_I32, get_counter_w1()),
                      False);
   put_counter_dw0(mkU64(0));
}

static const HChar *
s390_irgen_XC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I32);

   assign(len, mkU32(length));
   s390_irgen_xonc(Iop_Xor8, len, start1, start2);

   return "xc";
}

static void
s390_irgen_XC_sameloc(UChar length, UChar b, UShort d)
{
   IRTemp counter = newTemp(Ity_I32);
   IRTemp start = newTemp(Ity_I64);
   IRTemp addr  = newTemp(Ity_I64);

   assign(start,
          binop(Iop_Add64, mkU64(d), b != 0 ? get_gpr_dw0(b) : mkU64(0)));

   if (length < 8) {
      UInt i;

      for (i = 0; i <= length; ++i) {
         store(binop(Iop_Add64, mkexpr(start), mkU64(i)), mkU8(0));
      }
   } else {
     assign(counter, get_counter_w0());

     assign(addr, binop(Iop_Add64, mkexpr(start),
                        unop(Iop_32Uto64, mkexpr(counter))));

     store(mkexpr(addr), mkU8(0));

     /* Check for end of field */
     put_counter_w0(binop(Iop_Add32, mkexpr(counter), mkU32(1)));
     iterate_if(binop(Iop_CmpNE32, mkexpr(counter), mkU32(length)));

     /* Reset counter */
     put_counter_dw0(mkU64(0));
   }

   s390_cc_thunk_put1(S390_CC_OP_BITWISE, mktemp(Ity_I32, mkU32(0)), False);

   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      s390_disasm(ENC3(MNM, UDLB, UDXB), "xc", d, length, b, d, 0, b);
}

static const HChar *
s390_irgen_NC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I32);

   assign(len, mkU32(length));
   s390_irgen_xonc(Iop_And8, len, start1, start2);

   return "nc";
}

static const HChar *
s390_irgen_OC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I32);

   assign(len, mkU32(length));
   s390_irgen_xonc(Iop_Or8, len, start1, start2);

   return "oc";
}


static const HChar *
s390_irgen_MVC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I64);

   assign(len, mkU64(length));
   s390_irgen_MVC_EX(len, start1, start2);

   return "mvc";
}

static const HChar *
s390_irgen_MVCIN(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I64);

   assign(len, mkU64(length));
   s390_irgen_MVCIN_EX(len, start1, start2);

   return "mvcin";
}

static const HChar *
s390_irgen_MVCL(UChar r1, UChar r2)
{
   IRTemp addr1 = newTemp(Ity_I64);
   IRTemp addr2 = newTemp(Ity_I64);
   IRTemp addr2_load = newTemp(Ity_I64);
   IRTemp r1p1 = newTemp(Ity_I32);   /* contents of r1 + 1 */
   IRTemp r2p1 = newTemp(Ity_I32);   /* contents of r2 + 1 */
   IRTemp len1 = newTemp(Ity_I32);
   IRTemp len2 = newTemp(Ity_I32);
   IRTemp pad = newTemp(Ity_I8);
   IRTemp single = newTemp(Ity_I8);

   assign(addr1, get_gpr_dw0(r1));
   assign(r1p1, get_gpr_w1(r1 + 1));
   assign(len1, binop(Iop_And32, mkexpr(r1p1), mkU32(0x00ffffff)));
   assign(addr2, get_gpr_dw0(r2));
   assign(r2p1, get_gpr_w1(r2 + 1));
   assign(len2, binop(Iop_And32, mkexpr(r2p1), mkU32(0x00ffffff)));
   assign(pad, get_gpr_b4(r2 + 1));

   /* len1 == 0 ? */
   s390_cc_thunk_put2(S390_CC_OP_UNSIGNED_COMPARE, len1, len2, False);
   next_insn_if(binop(Iop_CmpEQ32, mkexpr(len1), mkU32(0)));

   /* Check for destructive overlap:
      addr1 > addr2 && addr2 + len1 > addr1 && (addr2 + len2) > addr1 */
   s390_cc_set_val(3);
   IRTemp cond1 = newTemp(Ity_I32);
   assign(cond1, unop(Iop_1Uto32,
                      binop(Iop_CmpLT64U, mkexpr(addr2), mkexpr(addr1))));
   IRTemp cond2 = newTemp(Ity_I32);
   assign(cond2, unop(Iop_1Uto32,
                      binop(Iop_CmpLT64U, mkexpr(addr1),
                            binop(Iop_Add64, mkexpr(addr2),
                                  unop(Iop_32Uto64, mkexpr(len1))))));
   IRTemp cond3 = newTemp(Ity_I32);
   assign(cond3, unop(Iop_1Uto32,
                      binop(Iop_CmpLT64U, 
                            mkexpr(addr1),
                            binop(Iop_Add64, mkexpr(addr2),
                                  unop(Iop_32Uto64, mkexpr(len2))))));

   next_insn_if(binop(Iop_CmpEQ32,
                      binop(Iop_And32,
                            binop(Iop_And32, mkexpr(cond1), mkexpr(cond2)),
                            mkexpr(cond3)),
                      mkU32(1)));

   /* See s390_irgen_CLCL for explanation why we cannot load directly
      and need two steps. */
   assign(addr2_load,
          mkite(binop(Iop_CmpEQ32, mkexpr(len2), mkU32(0)),
                mkU64(guest_IA_curr_instr), mkexpr(addr2)));
   assign(single,
          mkite(binop(Iop_CmpEQ32, mkexpr(len2), mkU32(0)),
                mkexpr(pad), load(Ity_I8, mkexpr(addr2_load))));

   store(mkexpr(addr1), mkexpr(single));

   /* Update addr1 and len1 */
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(addr1), mkU64(1)));
   put_gpr_w1(r1 + 1, binop(Iop_Sub32, mkexpr(r1p1), mkU32(1)));

   /* Update addr2 and len2 */
   put_gpr_dw0(r2,
               mkite(binop(Iop_CmpEQ32, mkexpr(len2), mkU32(0)),
                     mkexpr(addr2),
                     binop(Iop_Add64, mkexpr(addr2), mkU64(1))));

   /* When updating len2 we must not modify bits (r2+1)[0:39] */
   put_gpr_w1(r2 + 1,
              mkite(binop(Iop_CmpEQ32, mkexpr(len2), mkU32(0)),
                    binop(Iop_And32, mkexpr(r2p1), mkU32(0xFF000000u)),
                    binop(Iop_Sub32, mkexpr(r2p1), mkU32(1))));

   s390_cc_thunk_put2(S390_CC_OP_UNSIGNED_COMPARE, len1, len2, False);
   iterate_if(binop(Iop_CmpNE32, mkexpr(len1), mkU32(1)));

   return "mvcl";
}


static const HChar *
s390_irgen_MVCLE(UChar r1, UChar r3, IRTemp pad2)
{
   IRTemp addr1, addr3, addr3_load, len1, len3, single;

   addr1 = newTemp(Ity_I64);
   addr3 = newTemp(Ity_I64);
   addr3_load = newTemp(Ity_I64);
   len1 = newTemp(Ity_I64);
   len3 = newTemp(Ity_I64);
   single = newTemp(Ity_I8);

   assign(addr1, get_gpr_dw0(r1));
   assign(len1, get_gpr_dw0(r1 + 1));
   assign(addr3, get_gpr_dw0(r3));
   assign(len3, get_gpr_dw0(r3 + 1));

   // len1 == 0 ?
   s390_cc_thunk_put2(S390_CC_OP_UNSIGNED_COMPARE, len1, len3, False);
   next_insn_if(binop(Iop_CmpEQ64,mkexpr(len1), mkU64(0)));

   /* This is a hack to prevent mvcle from reading from addr3 if it
      should read from the pad. Since the pad has no address, just
      read from the instruction, we discard that anyway */
   assign(addr3_load,
          mkite(binop(Iop_CmpEQ64, mkexpr(len3), mkU64(0)),
                mkU64(guest_IA_curr_instr), mkexpr(addr3)));

   assign(single,
          mkite(binop(Iop_CmpEQ64, mkexpr(len3), mkU64(0)),
                unop(Iop_64to8, mkexpr(pad2)),
                load(Ity_I8, mkexpr(addr3_load))));
   store(mkexpr(addr1), mkexpr(single));

   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(addr1), mkU64(1)));

   put_gpr_dw0(r1 + 1, binop(Iop_Sub64, mkexpr(len1), mkU64(1)));

   put_gpr_dw0(r3,
               mkite(binop(Iop_CmpEQ64, mkexpr(len3), mkU64(0)),
                     mkexpr(addr3),
                     binop(Iop_Add64, mkexpr(addr3), mkU64(1))));

   put_gpr_dw0(r3 + 1,
               mkite(binop(Iop_CmpEQ64, mkexpr(len3), mkU64(0)),
                     mkU64(0), binop(Iop_Sub64, mkexpr(len3), mkU64(1))));

   s390_cc_thunk_put2(S390_CC_OP_UNSIGNED_COMPARE, len1, len3, False);
   iterate_if(binop(Iop_CmpNE64, mkexpr(len1), mkU64(1)));

   return "mvcle";
}

static const HChar *
s390_irgen_MVST(UChar r1, UChar r2)
{
   IRTemp addr1 = newTemp(Ity_I64);
   IRTemp addr2 = newTemp(Ity_I64);
   IRTemp end = newTemp(Ity_I8);
   IRTemp byte = newTemp(Ity_I8);
   IRTemp counter = newTemp(Ity_I64);

   assign(addr1, get_gpr_dw0(r1));
   assign(addr2, get_gpr_dw0(r2));
   assign(counter, get_counter_dw0());
   assign(end, get_gpr_b7(0));
   assign(byte, load(Ity_I8, binop(Iop_Add64, mkexpr(addr2),mkexpr(counter))));
   store(binop(Iop_Add64,mkexpr(addr1),mkexpr(counter)), mkexpr(byte));

   // We use unlimited as cpu-determined number
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   iterate_if(binop(Iop_CmpNE8, mkexpr(end), mkexpr(byte)));

   // and always set cc=1 at the end + update r1
   s390_cc_set_val(1);
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(addr1), mkexpr(counter)));
   put_counter_dw0(mkU64(0));

   return "mvst";
}

static void
s390_irgen_divide_64to32(IROp op, UChar r1, IRTemp op2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, binop(Iop_32HLto64,
                     get_gpr_w1(r1),         // high 32 bits
                     get_gpr_w1(r1 + 1)));   // low  32 bits
   assign(result, binop(op, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));   // remainder
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result))); // quotient
}

static void
s390_irgen_divide_128to64(IROp op, UChar r1, IRTemp op2)
{
   IRTemp op1 = newTemp(Ity_I128);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, binop(Iop_64HLto128,
                     get_gpr_dw0(r1),         // high 64 bits
                     get_gpr_dw0(r1 + 1)));   // low  64 bits
   assign(result, binop(op, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));   // remainder
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result))); // quotient
}

static void
s390_irgen_divide_64to64(IROp op, UChar r1, IRTemp op2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1 + 1));
   assign(result, binop(op, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));   // remainder
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result))); // quotient
}

static const HChar *
s390_irgen_DR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));

   s390_irgen_divide_64to32(Iop_DivModS64to32, r1, op2);

   return "dr";
}

static const HChar *
s390_irgen_D(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));

   s390_irgen_divide_64to32(Iop_DivModS64to32, r1, op2);

   return "d";
}

static const HChar *
s390_irgen_DLR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));

   s390_irgen_divide_64to32(Iop_DivModU64to32, r1, op2);

   return "dlr";
}

static const HChar *
s390_irgen_DL(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));

   s390_irgen_divide_64to32(Iop_DivModU64to32, r1, op2);

   return "dl";
}

static const HChar *
s390_irgen_DLG(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));

   s390_irgen_divide_128to64(Iop_DivModU128to64, r1, op2);

   return "dlg";
}

static const HChar *
s390_irgen_DLGR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));

   s390_irgen_divide_128to64(Iop_DivModU128to64, r1, op2);

   return "dlgr";
}

static const HChar *
s390_irgen_DSGR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));

   s390_irgen_divide_64to64(Iop_DivModS64to64, r1, op2);

   return "dsgr";
}

static const HChar *
s390_irgen_DSG(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));

   s390_irgen_divide_64to64(Iop_DivModS64to64, r1, op2);

   return "dsg";
}

static const HChar *
s390_irgen_DSGFR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));

   s390_irgen_divide_64to64(Iop_DivModS64to64, r1, op2);

   return "dsgfr";
}

static const HChar *
s390_irgen_DSGF(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));

   s390_irgen_divide_64to64(Iop_DivModS64to64, r1, op2);

   return "dsgf";
}

static void
s390_irgen_load_ar_multiple(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar reg;
   IRTemp addr = newTemp(Ity_I64);

   assign(addr, mkexpr(op2addr));
   reg = r1;
   do {
      IRTemp old = addr;

      reg %= 16;
      put_ar_w0(reg, load(Ity_I32, mkexpr(addr)));
      addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, mkexpr(old), mkU64(4)));
      reg++;
   } while (reg != (r3 + 1));
}

static const HChar *
s390_irgen_LAM(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_ar_multiple(r1, r3, op2addr);

   return "lam";
}

static const HChar *
s390_irgen_LAMY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_ar_multiple(r1, r3, op2addr);

   return "lamy";
}

static void
s390_irgen_store_ar_multiple(UChar r1, UChar r3, IRTemp op2addr)
{
   UChar reg;
   IRTemp addr = newTemp(Ity_I64);

   assign(addr, mkexpr(op2addr));
   reg = r1;
   do {
      IRTemp old = addr;

      reg %= 16;
      store(mkexpr(addr), get_ar_w0(reg));
      addr = newTemp(Ity_I64);
      assign(addr, binop(Iop_Add64, mkexpr(old), mkU64(4)));
      reg++;
   } while (reg != (r3 + 1));
}

static const HChar *
s390_irgen_STAM(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_store_ar_multiple(r1, r3, op2addr);

   return "stam";
}

static const HChar *
s390_irgen_STAMY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_store_ar_multiple(r1, r3, op2addr);

   return "stamy";
}


/* Implementation for 32-bit compare-and-swap */
static void
s390_irgen_cas_32(UChar r1, UChar r3, IRTemp op2addr)
{
   IRCAS *cas;
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp old_mem = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp nequal = newTemp(Ity_I1);

   assign(op1, get_gpr_w1(r1));
   assign(op3, get_gpr_w1(r3));

   /* The first and second operands are compared. If they are equal,
      the third operand is stored at the second- operand location. */
   cas = mkIRCAS(IRTemp_INVALID, old_mem,
                 Iend_BE, mkexpr(op2addr),
                 NULL, mkexpr(op1), /* expected value */
                 NULL, mkexpr(op3)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* Set CC. Operands compared equal -> 0, else 1. */
   assign(result, binop(Iop_Sub32, mkexpr(op1), mkexpr(old_mem)));
   s390_cc_thunk_put1(S390_CC_OP_BITWISE, result, False);

   /* If operands were equal (cc == 0) just store the old value op1 in r1.
      Otherwise, store the old_value from memory in r1 and yield. */
   assign(nequal, binop(Iop_CmpNE32, s390_call_calculate_cc(), mkU32(0)));
   put_gpr_w1(r1, mkite(mkexpr(nequal), mkexpr(old_mem), mkexpr(op1)));
   yield_if(mkexpr(nequal));
}

static const HChar *
s390_irgen_CS(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_cas_32(r1, r3, op2addr);

   return "cs";
}

static const HChar *
s390_irgen_CSY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_cas_32(r1, r3, op2addr);

   return "csy";
}

static const HChar *
s390_irgen_CSG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRCAS *cas;
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp old_mem = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp nequal = newTemp(Ity_I1);

   assign(op1, get_gpr_dw0(r1));
   assign(op3, get_gpr_dw0(r3));

   /* The first and second operands are compared. If they are equal,
      the third operand is stored at the second- operand location. */
   cas = mkIRCAS(IRTemp_INVALID, old_mem,
                 Iend_BE, mkexpr(op2addr),
                 NULL, mkexpr(op1), /* expected value */
                 NULL, mkexpr(op3)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* Set CC. Operands compared equal -> 0, else 1. */
   assign(result, binop(Iop_Sub64, mkexpr(op1), mkexpr(old_mem)));
   s390_cc_thunk_put1(S390_CC_OP_BITWISE, result, False);

   /* If operands were equal (cc == 0) just store the old value op1 in r1.
      Otherwise, store the old_value from memory in r1 and yield. */
   assign(nequal, binop(Iop_CmpNE32, s390_call_calculate_cc(), mkU32(0)));
   put_gpr_dw0(r1, mkite(mkexpr(nequal), mkexpr(old_mem), mkexpr(op1)));
   yield_if(mkexpr(nequal));

   return "csg";
}

/* Implementation for 32-bit compare-double-and-swap */
static void
s390_irgen_cdas_32(UChar r1, UChar r3, IRTemp op2addr)
{
   IRCAS *cas;
   IRTemp op1_high = newTemp(Ity_I32);
   IRTemp op1_low  = newTemp(Ity_I32);
   IRTemp old_mem_high = newTemp(Ity_I32);
   IRTemp old_mem_low  = newTemp(Ity_I32);
   IRTemp op3_high = newTemp(Ity_I32);
   IRTemp op3_low  = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp nequal = newTemp(Ity_I1);

   assign(op1_high, get_gpr_w1(r1));
   assign(op1_low,  get_gpr_w1(r1+1));
   assign(op3_high, get_gpr_w1(r3));
   assign(op3_low,  get_gpr_w1(r3+1));

   /* The first and second operands are compared. If they are equal,
      the third operand is stored at the second-operand location. */
   cas = mkIRCAS(old_mem_high, old_mem_low,
                 Iend_BE, mkexpr(op2addr),
                 mkexpr(op1_high), mkexpr(op1_low), /* expected value */
                 mkexpr(op3_high), mkexpr(op3_low)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* Set CC. Operands compared equal -> 0, else 1. */
   assign(result, unop(Iop_1Uto32,
          binop(Iop_CmpNE32,
                binop(Iop_Or32,
                      binop(Iop_Xor32, mkexpr(op1_high), mkexpr(old_mem_high)),
                      binop(Iop_Xor32, mkexpr(op1_low), mkexpr(old_mem_low))),
                mkU32(0))));

   s390_cc_thunk_put1(S390_CC_OP_BITWISE, result, False);

   /* If operands were equal (cc == 0) just store the old value op1 in r1.
      Otherwise, store the old_value from memory in r1 and yield. */
   assign(nequal, binop(Iop_CmpNE32, s390_call_calculate_cc(), mkU32(0)));
   put_gpr_w1(r1,   mkite(mkexpr(nequal), mkexpr(old_mem_high), mkexpr(op1_high)));
   put_gpr_w1(r1+1, mkite(mkexpr(nequal), mkexpr(old_mem_low),  mkexpr(op1_low)));
   yield_if(mkexpr(nequal));
}

static const HChar *
s390_irgen_CDS(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_cdas_32(r1, r3, op2addr);

   return "cds";
}

static const HChar *
s390_irgen_CDSY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_cdas_32(r1, r3, op2addr);

   return "cdsy";
}

static const HChar *
s390_irgen_CDSG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRCAS *cas;
   IRTemp op1_high = newTemp(Ity_I64);
   IRTemp op1_low  = newTemp(Ity_I64);
   IRTemp old_mem_high = newTemp(Ity_I64);
   IRTemp old_mem_low  = newTemp(Ity_I64);
   IRTemp op3_high = newTemp(Ity_I64);
   IRTemp op3_low  = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp nequal = newTemp(Ity_I1);

   assign(op1_high, get_gpr_dw0(r1));
   assign(op1_low,  get_gpr_dw0(r1+1));
   assign(op3_high, get_gpr_dw0(r3));
   assign(op3_low,  get_gpr_dw0(r3+1));

   /* The first and second operands are compared. If they are equal,
      the third operand is stored at the second-operand location. */
   cas = mkIRCAS(old_mem_high, old_mem_low,
                 Iend_BE, mkexpr(op2addr),
                 mkexpr(op1_high), mkexpr(op1_low), /* expected value */
                 mkexpr(op3_high), mkexpr(op3_low)  /* new value */);
   stmt(IRStmt_CAS(cas));

   /* Set CC. Operands compared equal -> 0, else 1. */
   assign(result, unop(Iop_1Uto64,
          binop(Iop_CmpNE64,
                binop(Iop_Or64,
                      binop(Iop_Xor64, mkexpr(op1_high), mkexpr(old_mem_high)),
                      binop(Iop_Xor64, mkexpr(op1_low), mkexpr(old_mem_low))),
                mkU64(0))));

   s390_cc_thunk_put1(S390_CC_OP_BITWISE, result, False);

   /* If operands were equal (cc == 0) just store the old value op1 in r1.
      Otherwise, store the old_value from memory in r1 and yield. */
   assign(nequal, binop(Iop_CmpNE32, s390_call_calculate_cc(), mkU32(0)));
   put_gpr_dw0(r1,   mkite(mkexpr(nequal), mkexpr(old_mem_high), mkexpr(op1_high)));
   put_gpr_dw0(r1+1, mkite(mkexpr(nequal), mkexpr(old_mem_low),  mkexpr(op1_low)));
   yield_if(mkexpr(nequal));

   return "cdsg";
}


/* Binary floating point */

static const HChar *
s390_irgen_AXBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F128);
   IRTemp op2 = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_F128);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_pair(r1));
   assign(op2, get_fpr_pair(r2));
   assign(result, triop(Iop_AddF128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_fpr_pair(r1, mkexpr(result));

   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);

   return "axbr";
}

static const HChar *
s390_irgen_CEBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_fpr_w0(r1));
   assign(op2, get_fpr_w0(r2));
   assign(cc_vex, binop(Iop_CmpF32, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);

   return "cebr";
}

static const HChar *
s390_irgen_CDBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, get_fpr_dw0(r2));
   assign(cc_vex, binop(Iop_CmpF64, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);

   return "cdbr";
}

static const HChar *
s390_irgen_CXBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F128);
   IRTemp op2 = newTemp(Ity_F128);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_fpr_pair(r1));
   assign(op2, get_fpr_pair(r2));
   assign(cc_vex, binop(Iop_CmpF128, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);

   return "cxbr";
}

static const HChar *
s390_irgen_CEB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F32);
   IRTemp op2 = newTemp(Ity_F32);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_fpr_w0(r1));
   assign(op2, load(Ity_F32, mkexpr(op2addr)));
   assign(cc_vex,  binop(Iop_CmpF32, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);

   return "ceb";
}

static const HChar *
s390_irgen_CDB(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_F64);
   IRTemp op2 = newTemp(Ity_F64);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_fpr_dw0(r1));
   assign(op2, load(Ity_F64, mkexpr(op2addr)));
   assign(cc_vex, binop(Iop_CmpF64, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);

   return "cdb";
}

static const HChar *
s390_irgen_CXFBR(UChar m3 __attribute__((unused)),
                 UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_pair(r1, unop(Iop_I32StoF128, mkexpr(op2)));

   return "cxfbr";
}

static const HChar *
s390_irgen_CXLFBR(UChar m3 __attribute__((unused)),
                  UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op2 = newTemp(Ity_I32);

      assign(op2, get_gpr_w1(r2));
      put_fpr_pair(r1, unop(Iop_I32UtoF128, mkexpr(op2)));
   }
   return "cxlfbr";
}


static const HChar *
s390_irgen_CXGBR(UChar m3 __attribute__((unused)),
                 UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_pair(r1, unop(Iop_I64StoF128, mkexpr(op2)));

   return "cxgbr";
}

static const HChar *
s390_irgen_CXLGBR(UChar m3 __attribute__((unused)),
                  UChar m4 __attribute__((unused)), UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op2 = newTemp(Ity_I64);

      assign(op2, get_gpr_dw0(r2));
      put_fpr_pair(r1, unop(Iop_I64UtoF128, mkexpr(op2)));
   }
   return "cxlgbr";
}

static const HChar *
s390_irgen_CFXBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_pair(r2));
   assign(result, binop(Iop_F128toI32S, mkexpr(rounding_mode),
                        mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_128_TO_INT_32, op, rounding_mode);

   return "cfxbr";
}

static const HChar *
s390_irgen_CLFXBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op = newTemp(Ity_F128);
      IRTemp result = newTemp(Ity_I32);
      IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

      assign(op, get_fpr_pair(r2));
      assign(result, binop(Iop_F128toI32U, mkexpr(rounding_mode),
                           mkexpr(op)));
      put_gpr_w1(r1, mkexpr(result));
      s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_128_TO_UINT_32, op, rounding_mode);
   }
   return "clfxbr";
}


static const HChar *
s390_irgen_CGXBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_pair(r2));
   assign(result, binop(Iop_F128toI64S, mkexpr(rounding_mode),
                        mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_128_TO_INT_64, op, rounding_mode);

   return "cgxbr";
}

static const HChar *
s390_irgen_CLGXBR(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   if (! s390_host_has_fpext) {
      emulation_failure(EmFail_S390X_fpext);
   } else {
      IRTemp op = newTemp(Ity_F128);
      IRTemp result = newTemp(Ity_I64);
      IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

      assign(op, get_fpr_pair(r2));
      assign(result, binop(Iop_F128toI64U, mkexpr(rounding_mode),
                           mkexpr(op)));
      put_gpr_dw0(r1, mkexpr(result));
      s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_128_TO_UINT_64, op,
                              rounding_mode);
   }
   return "clgxbr";
}

static const HChar *
s390_irgen_DXBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F128);
   IRTemp op2 = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_F128);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_pair(r1));
   assign(op2, get_fpr_pair(r2));
   assign(result, triop(Iop_DivF128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_fpr_pair(r1, mkexpr(result));

   return "dxbr";
}

static const HChar *
s390_irgen_LTXBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F128);

   assign(result, get_fpr_pair(r2));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);

   return "ltxbr";
}

static const HChar *
s390_irgen_LCXBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F128);

   assign(result, unop(Iop_NegF128, get_fpr_pair(r2)));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);

   return "lcxbr";
}

static const HChar *
s390_irgen_LXDBR(UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F64);

   assign(op, get_fpr_dw0(r2));
   put_fpr_pair(r1, unop(Iop_F64toF128, mkexpr(op)));

   return "lxdbr";
}

static const HChar *
s390_irgen_LXEBR(UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F32);

   assign(op, get_fpr_w0(r2));
   put_fpr_pair(r1, unop(Iop_F32toF128, mkexpr(op)));

   return "lxebr";
}

static const HChar *
s390_irgen_LXDB(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_F64);

   assign(op, load(Ity_F64, mkexpr(op2addr)));
   put_fpr_pair(r1, unop(Iop_F64toF128, mkexpr(op)));

   return "lxdb";
}

static const HChar *
s390_irgen_LXEB(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_F32);

   assign(op, load(Ity_F32, mkexpr(op2addr)));
   put_fpr_pair(r1, unop(Iop_F32toF128, mkexpr(op)));

   return "lxeb";
}

static const HChar *
s390_irgen_FIEBRA(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, binop(Iop_RoundF32toInt, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_w0(r2)));
   put_fpr_w0(r1, mkexpr(result));

   return "fiebra";
}

static const HChar *
s390_irgen_FIDBRA(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, binop(Iop_RoundF64toInt, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "fidbra";
}

static const HChar *
s390_irgen_FIXBRA(UChar m3, UChar m4 __attribute__((unused)),
                  UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F128);

   assign(result, binop(Iop_RoundF128toInt, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_pair(r2)));
   put_fpr_pair(r1, mkexpr(result));

   return "fixbra";
}

static const HChar *
s390_irgen_LNEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, unop(Iop_NegF32, unop(Iop_AbsF32, get_fpr_w0(r2))));
   put_fpr_w0(r1, mkexpr(result));
   s390_cc_thunk_put1f(S390_CC_OP_BFP_RESULT_32, result);

   return "lnebr";
}

static const HChar *
s390_irgen_LNDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_NegF64, unop(Iop_AbsF64, get_fpr_dw0(r2))));
   put_fpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_put1f(S390_CC_OP_BFP_RESULT_64, result);

   return "lndbr";
}

static const HChar *
s390_irgen_LNXBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F128);

   assign(result, unop(Iop_NegF128, unop(Iop_AbsF128, get_fpr_pair(r2))));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);

   return "lnxbr";
}

static const HChar *
s390_irgen_LPEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, unop(Iop_AbsF32, get_fpr_w0(r2)));
   put_fpr_w0(r1, mkexpr(result));
   s390_cc_thunk_put1f(S390_CC_OP_BFP_RESULT_32, result);

   return "lpebr";
}

static const HChar *
s390_irgen_LPDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_AbsF64, get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_put1f(S390_CC_OP_BFP_RESULT_64, result);

   return "lpdbr";
}

static const HChar *
s390_irgen_LPXBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F128);

   assign(result, unop(Iop_AbsF128, get_fpr_pair(r2)));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);

   return "lpxbr";
}

static const HChar *
s390_irgen_LDXBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_fpext && m3 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m3 = S390_BFP_ROUND_PER_FPC;
   }
   IRTemp result = newTemp(Ity_F64);

   assign(result, binop(Iop_F128toF64, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_pair(r2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "ldxbr";
}

static const HChar *
s390_irgen_LEXBR(UChar m3, UChar m4 __attribute__((unused)),
                 UChar r1, UChar r2)
{
   if (! s390_host_has_fpext && m3 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m3 = S390_BFP_ROUND_PER_FPC;
   }
   IRTemp result = newTemp(Ity_F32);

   assign(result, binop(Iop_F128toF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_pair(r2)));
   put_fpr_w0(r1, mkexpr(result));

   return "lexbr";
}

static const HChar *
s390_irgen_MXBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F128);
   IRTemp op2 = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_F128);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_pair(r1));
   assign(op2, get_fpr_pair(r2));
   assign(result, triop(Iop_MulF128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_fpr_pair(r1, mkexpr(result));

   return "mxbr";
}

static const HChar *
s390_irgen_MAEBR(UChar r1, UChar r3, UChar r2)
{
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_w0(r1, qop(Iop_MAddF32, mkexpr(rounding_mode),
                      get_fpr_w0(r3), get_fpr_w0(r2), get_fpr_w0(r1)));

   return "maebr";
}

static const HChar *
s390_irgen_MADBR(UChar r1, UChar r3, UChar r2)
{
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_dw0(r1, qop(Iop_MAddF64, mkexpr(rounding_mode),
                       get_fpr_dw0(r3), get_fpr_dw0(r2), get_fpr_dw0(r1)));

   return "madbr";
}

static const HChar *
s390_irgen_MAEB(UChar r3, IRTemp op2addr, UChar r1)
{
   IRExpr *op2 = load(Ity_F32, mkexpr(op2addr));
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_w0(r1, qop(Iop_MAddF32, mkexpr(rounding_mode),
                      get_fpr_w0(r3), op2, get_fpr_w0(r1)));

   return "maeb";
}

static const HChar *
s390_irgen_MADB(UChar r3, IRTemp op2addr, UChar r1)
{
   IRExpr *op2 = load(Ity_F64, mkexpr(op2addr));
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_dw0(r1, qop(Iop_MAddF64, mkexpr(rounding_mode),
                       get_fpr_dw0(r3), op2, get_fpr_dw0(r1)));

   return "madb";
}

static const HChar *
s390_irgen_MSEBR(UChar r1, UChar r3, UChar r2)
{
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_w0(r1, qop(Iop_MSubF32, mkexpr(rounding_mode),
                      get_fpr_w0(r3), get_fpr_w0(r2), get_fpr_w0(r1)));

   return "msebr";
}

static const HChar *
s390_irgen_MSDBR(UChar r1, UChar r3, UChar r2)
{
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_dw0(r1, qop(Iop_MSubF64, mkexpr(rounding_mode),
                       get_fpr_dw0(r3), get_fpr_dw0(r2), get_fpr_dw0(r1)));

   return "msdbr";
}

static const HChar *
s390_irgen_MSEB(UChar r3, IRTemp op2addr, UChar r1)
{
   IRExpr *op2 = load(Ity_F32, mkexpr(op2addr));
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_w0(r1, qop(Iop_MSubF32, mkexpr(rounding_mode),
                      get_fpr_w0(r3), op2, get_fpr_w0(r1)));

   return "mseb";
}

static const HChar *
s390_irgen_MSDB(UChar r3, IRTemp op2addr, UChar r1)
{
   IRExpr *op2 = load(Ity_F64, mkexpr(op2addr));
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_dw0(r1, qop(Iop_MSubF64, mkexpr(rounding_mode),
                       get_fpr_dw0(r3), op2, get_fpr_dw0(r1)));

   return "msdb";
}

static const HChar *
s390_irgen_SQEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(result, binop(Iop_SqrtF32, mkexpr(rounding_mode), get_fpr_w0(r2)));
   put_fpr_w0(r1, mkexpr(result));

   return "sqebr";
}

static const HChar *
s390_irgen_SQDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(result, binop(Iop_SqrtF64, mkexpr(rounding_mode), get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "sqdbr";
}

static const HChar *
s390_irgen_SQXBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F128);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(result, binop(Iop_SqrtF128, mkexpr(rounding_mode),
                        get_fpr_pair(r2)));
   put_fpr_pair(r1, mkexpr(result));

   return "sqxbr";
}

static const HChar *
s390_irgen_SQEB(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op, load(Ity_F32, mkexpr(op2addr)));
   put_fpr_w0(r1, binop(Iop_SqrtF32, mkexpr(rounding_mode), mkexpr(op)));

   return "sqeb";
}

static const HChar *
s390_irgen_SQDB(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op, load(Ity_F64, mkexpr(op2addr)));
   put_fpr_dw0(r1, binop(Iop_SqrtF64, mkexpr(rounding_mode), mkexpr(op)));

   return "sqdb";
}

static const HChar *
s390_irgen_SXBR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_F128);
   IRTemp op2 = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_F128);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_pair(r1));
   assign(op2, get_fpr_pair(r2));
   assign(result, triop(Iop_SubF128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);

   return "sxbr";
}

static const HChar *
s390_irgen_TCEB(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_F32);

   assign(value, get_fpr_w0(r1));

   s390_cc_thunk_putFZ(S390_CC_OP_BFP_TDC_32, value, op2addr);

   return "tceb";
}

static const HChar *
s390_irgen_TCDB(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_F64);

   assign(value, get_fpr_dw0(r1));

   s390_cc_thunk_putFZ(S390_CC_OP_BFP_TDC_64, value, op2addr);

   return "tcdb";
}

static const HChar *
s390_irgen_TCXB(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_F128);

   assign(value, get_fpr_pair(r1));

   s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_TDC_128, value, op2addr);

   return "tcxb";
}

static const HChar *
s390_irgen_LCDFR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_NegF64, get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "lcdfr";
}

static const HChar *
s390_irgen_LNDFR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_NegF64, unop(Iop_AbsF64, get_fpr_dw0(r2))));
   put_fpr_dw0(r1, mkexpr(result));

   return "lndfr";
}

static const HChar *
s390_irgen_LPDFR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_AbsF64, get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));

   return "lpdfr";
}

static const HChar *
s390_irgen_LDGR(UChar r1, UChar r2)
{
   put_fpr_dw0(r1, unop(Iop_ReinterpI64asF64, get_gpr_dw0(r2)));

   return "ldgr";
}

static const HChar *
s390_irgen_LGDR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_ReinterpF64asI64, get_fpr_dw0(r2)));

   return "lgdr";
}


static const HChar *
s390_irgen_CPSDR(UChar r3, UChar r1, UChar r2)
{
   IRTemp sign  = newTemp(Ity_I64);
   IRTemp value = newTemp(Ity_I64);

   assign(sign, binop(Iop_And64, unop(Iop_ReinterpF64asI64, get_fpr_dw0(r3)),
                      mkU64(1ULL << 63)));
   assign(value, binop(Iop_And64, unop(Iop_ReinterpF64asI64, get_fpr_dw0(r2)),
                       mkU64((1ULL << 63) - 1)));
   put_fpr_dw0(r1, unop(Iop_ReinterpI64asF64, binop(Iop_Or64, mkexpr(value),
                                                    mkexpr(sign))));

   return "cpsdr";
}


static IRExpr *
s390_call_cvb(IRExpr *in)
{
   IRExpr **args, *call;

   args = mkIRExprVec_1(in);
   call = mkIRExprCCall(Ity_I32, 0 /*regparm*/,
                        "s390_do_cvb", &s390_do_cvb, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static const HChar *
s390_irgen_CVB(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, s390_call_cvb(load(Ity_I64, mkexpr(op2addr))));

   return "cvb";
}

static const HChar *
s390_irgen_CVBY(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, s390_call_cvb(load(Ity_I64, mkexpr(op2addr))));

   return "cvby";
}


static IRExpr *
s390_call_cvd(IRExpr *in)
{
   IRExpr **args, *call;

   args = mkIRExprVec_1(in);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/,
                        "s390_do_cvd", &s390_do_cvd, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static const HChar *
s390_irgen_CVD(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), s390_call_cvd(unop(Iop_32Uto64, get_gpr_w1(r1))));

   return "cvd";
}

static const HChar *
s390_irgen_CVDY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), s390_call_cvd(get_gpr_w1(r1)));

   return "cvdy";
}

static const HChar *
s390_irgen_FLOGR(UChar r1, UChar r2)
{
   IRTemp input    = newTemp(Ity_I64);
   IRTemp not_zero = newTemp(Ity_I64);
   IRTemp tmpnum   = newTemp(Ity_I64);
   IRTemp num      = newTemp(Ity_I64);
   IRTemp shift_amount = newTemp(Ity_I8);

   /* We use the "count leading zeroes" operator because the number of
      leading zeroes is identical with the bit position of the first '1' bit.
      However, that operator does not work when the input value is zero.
      Therefore, we set the LSB of the input value to 1 and use Clz64 on
      the modified value. If input == 0, then the result is 64. Otherwise,
      the result of Clz64 is what we want. */

   assign(input, get_gpr_dw0(r2));
   assign(not_zero, binop(Iop_Or64, mkexpr(input), mkU64(1)));
   assign(tmpnum, unop(Iop_Clz64, mkexpr(not_zero)));

   /* num = (input == 0) ? 64 : tmpnum */
   assign(num, mkite(binop(Iop_CmpEQ64, mkexpr(input), mkU64(0)),
                     /* == 0 */ mkU64(64),
                     /* != 0 */ mkexpr(tmpnum)));

   put_gpr_dw0(r1, mkexpr(num));

   /* Set the leftmost '1' bit of the input value to zero. The general scheme
      is to first shift the input value by NUM + 1 bits to the left which
      causes the leftmost '1' bit to disappear. Then we shift logically to
      the right by NUM + 1 bits. Because the semantics of Iop_Shl64 and
      Iop_Shr64 are undefined if the shift-amount is greater than or equal to
      the width of the value-to-be-shifted, we need to special case
      NUM + 1 >= 64. This is equivalent to INPUT != 0 && INPUT != 1.
      For both such INPUT values the result will be 0. */

   assign(shift_amount, unop(Iop_64to8, binop(Iop_Add64, mkexpr(num),
                          mkU64(1))));

   put_gpr_dw0(r1 + 1,
               mkite(binop(Iop_CmpLE64U, mkexpr(input), mkU64(1)),
                     /* == 0 || == 1*/ mkU64(0),
                     /* otherwise */
                     binop(Iop_Shr64,
                           binop(Iop_Shl64, mkexpr(input),
                                 mkexpr(shift_amount)),
                           mkexpr(shift_amount))));

   /* Compare the original value as an unsigned integer with 0. */
   s390_cc_thunk_put2(S390_CC_OP_UNSIGNED_COMPARE, input,
                      mktemp(Ity_I64, mkU64(0)), False);

   return "flogr";
}

static const HChar *
s390_irgen_POPCNT(UChar r1, UChar r2)
{
   Int i;
   IRTemp val = newTemp(Ity_I64);
   IRTemp mask[3];

   assign(val, get_gpr_dw0(r2));
   for (i = 0; i < 3; i++) {
      mask[i] = newTemp(Ity_I64);
   }
   assign(mask[0], mkU64(0x5555555555555555ULL));
   assign(mask[1], mkU64(0x3333333333333333ULL));
   assign(mask[2], mkU64(0x0F0F0F0F0F0F0F0FULL));
   for (i = 0; i < 3; i++) {
      IRTemp tmp = newTemp(Ity_I64);

      assign(tmp,
             binop(Iop_Add64,
                   binop(Iop_And64,
                         mkexpr(val),
                         mkexpr(mask[i])),
                   binop(Iop_And64,
                         binop(Iop_Shr64, mkexpr(val), mkU8(1 << i)),
                         mkexpr(mask[i]))));
      val = tmp;
   }
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, val);
   put_gpr_dw0(r1, mkexpr(val));
   return "popcnt";
}

static const HChar *
s390_irgen_STCK(IRTemp op2addr)
{
   IRDirty *d;
   IRTemp cc = newTemp(Ity_I64);

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_STCK",
                         &s390x_dirtyhelper_STCK,
                         mkIRExprVec_1(mkexpr(op2addr)));
   d->mFx   = Ifx_Write;
   d->mAddr = mkexpr(op2addr);
   d->mSize = 8;
   stmt(IRStmt_Dirty(d));
   s390_cc_set(cc);
   return "stck";
}

static const HChar *
s390_irgen_STCKF(IRTemp op2addr)
{
   if (! s390_host_has_stckf) {
      emulation_failure(EmFail_S390X_stckf);
   } else {
      IRTemp cc = newTemp(Ity_I64);

      IRDirty *d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_STCKF",
                                     &s390x_dirtyhelper_STCKF,
                                     mkIRExprVec_1(mkexpr(op2addr)));
      d->mFx   = Ifx_Write;
      d->mAddr = mkexpr(op2addr);
      d->mSize = 8;
      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }
   return "stckf";
}

static const HChar *
s390_irgen_STCKE(IRTemp op2addr)
{
   IRDirty *d;
   IRTemp cc = newTemp(Ity_I64);

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_STCKE",
                         &s390x_dirtyhelper_STCKE,
                         mkIRExprVec_1(mkexpr(op2addr)));
   d->mFx   = Ifx_Write;
   d->mAddr = mkexpr(op2addr);
   d->mSize = 16;
   stmt(IRStmt_Dirty(d));
   s390_cc_set(cc);
   return "stcke";
}

static const HChar *
s390_irgen_STFLE(IRTemp op2addr)
{
   if (! s390_host_has_stfle) {
      emulation_failure(EmFail_S390X_stfle);
      return "stfle";
   }

   IRDirty *d;
   IRTemp cc = newTemp(Ity_I64);

   /* IRExpr_GSPTR() => Need to pass pointer to guest state to helper */
   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_STFLE",
                         &s390x_dirtyhelper_STFLE,
                         mkIRExprVec_2(IRExpr_GSPTR(), mkexpr(op2addr)));

   d->nFxState = 1;
   vex_bzero(&d->fxState, sizeof(d->fxState));

   d->fxState[0].fx     = Ifx_Modify;  /* read then write */
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_r0);
   d->fxState[0].size   = sizeof(ULong);

   d->mAddr = mkexpr(op2addr);
   /* Pretend all double words are written */
   d->mSize = S390_NUM_FACILITY_DW * sizeof(ULong);
   d->mFx   = Ifx_Write;

   stmt(IRStmt_Dirty(d));

   s390_cc_set(cc);

   return "stfle";
}

static const HChar *
s390_irgen_CKSM(UChar r1,UChar r2)
{
   IRTemp addr = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I32);
   IRTemp len = newTemp(Ity_I64);
   IRTemp oldval = newTemp(Ity_I32);
   IRTemp mask = newTemp(Ity_I32);
   IRTemp newop = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp result1 = newTemp(Ity_I32);
   IRTemp inc = newTemp(Ity_I64);

   assign(oldval, get_gpr_w1(r1));
   assign(addr, get_gpr_dw0(r2));
   assign(len, get_gpr_dw0(r2+1));

   /* Condition code is always zero. */
   s390_cc_set_val(0);

   /* If length is zero, there is no need to calculate the checksum */
   next_insn_if(binop(Iop_CmpEQ64, mkexpr(len), mkU64(0)));

   /* Assiging the increment variable to adjust address and length
      later on. */
   assign(inc, mkite(binop(Iop_CmpLT64U, mkexpr(len), mkU64(4)),
                           mkexpr(len), mkU64(4)));

   /* If length < 4 the final 4-byte 2nd operand value is computed by 
      appending the remaining bytes to the right with 0. This is done
      by AND'ing the 4 bytes loaded from memory with an appropriate
      mask. If length >= 4, that mask is simply 0xffffffff. */

   assign(mask, mkite(binop(Iop_CmpLT64U, mkexpr(len), mkU64(4)),
                      /* Mask computation when len < 4:
                         0xffffffff << (32 - (len % 4)*8) */
                      binop(Iop_Shl32, mkU32(0xffffffff),
                            unop(Iop_32to8,
                                 binop(Iop_Sub32, mkU32(32),
                                       binop(Iop_Shl32,
                                             unop(Iop_64to32,
                                                  binop(Iop_And64,
                                                        mkexpr(len), mkU64(3))),
                                             mkU8(3))))),
                      mkU32(0xffffffff)));

   assign(op, load(Ity_I32, mkexpr(addr)));
   assign(newop, binop(Iop_And32, mkexpr(op), mkexpr(mask)));
   assign(result, binop(Iop_Add32, mkexpr(newop), mkexpr(oldval)));

   /* Checking for carry */
   assign(result1, mkite(binop(Iop_CmpLT32U, mkexpr(result), mkexpr(newop)),
                         binop(Iop_Add32, mkexpr(result), mkU32(1)),
                         mkexpr(result)));

   put_gpr_w1(r1, mkexpr(result1));
   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(addr), mkexpr(inc)));
   put_gpr_dw0(r2+1, binop(Iop_Sub64, mkexpr(len), mkexpr(inc)));

   iterate_if(binop(Iop_CmpNE64, mkexpr(len), mkU64(0)));

   return "cksm";
}

static const HChar *
s390_irgen_TROO(UChar m3, UChar r1, UChar r2)
{
   IRTemp src_addr, des_addr, tab_addr, src_len, test_byte;
   src_addr = newTemp(Ity_I64);
   des_addr = newTemp(Ity_I64);
   tab_addr = newTemp(Ity_I64);
   test_byte = newTemp(Ity_I8);
   src_len = newTemp(Ity_I64);

   assign(src_addr, get_gpr_dw0(r2));
   assign(des_addr, get_gpr_dw0(r1));
   assign(tab_addr, get_gpr_dw0(1));
   assign(src_len, get_gpr_dw0(r1+1));
   assign(test_byte, get_gpr_b7(0));

   IRTemp op = newTemp(Ity_I8);
   IRTemp op1 = newTemp(Ity_I8);
   IRTemp result = newTemp(Ity_I64);

   /* End of source string? We're done; proceed to next insn */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpEQ64, mkexpr(src_len), mkU64(0)));

   /* Load character from source string, index translation table and
      store translated character in op1. */
   assign(op, load(Ity_I8, mkexpr(src_addr)));

   assign(result, binop(Iop_Add64, unop(Iop_8Uto64, mkexpr(op)),
                        mkexpr(tab_addr)));
   assign(op1, load(Ity_I8, mkexpr(result)));

   if (! s390_host_has_etf2 || (m3 & 0x1) == 0) {
      s390_cc_set_val(1);
      next_insn_if(binop(Iop_CmpEQ8, mkexpr(op1), mkexpr(test_byte)));
   }
   store(get_gpr_dw0(r1), mkexpr(op1));

   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(des_addr), mkU64(1)));
   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(src_addr), mkU64(1)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(1)));

   iterate();

   return "troo";
}

static const HChar *
s390_irgen_TRTO(UChar m3, UChar r1, UChar r2)
{
   IRTemp src_addr, des_addr, tab_addr, src_len, test_byte;
   src_addr = newTemp(Ity_I64);
   des_addr = newTemp(Ity_I64);
   tab_addr = newTemp(Ity_I64);
   test_byte = newTemp(Ity_I8);
   src_len = newTemp(Ity_I64);

   assign(src_addr, get_gpr_dw0(r2));
   assign(des_addr, get_gpr_dw0(r1));
   assign(tab_addr, get_gpr_dw0(1));
   assign(src_len, get_gpr_dw0(r1+1));
   assign(test_byte, get_gpr_b7(0));

   IRTemp op = newTemp(Ity_I16);
   IRTemp op1 = newTemp(Ity_I8);
   IRTemp result = newTemp(Ity_I64);

   /* End of source string? We're done; proceed to next insn */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpEQ64, mkexpr(src_len), mkU64(0)));

   /* Load character from source string, index translation table and
      store translated character in op1. */
   assign(op, load(Ity_I16, mkexpr(src_addr)));

   assign(result, binop(Iop_Add64, unop(Iop_16Uto64, mkexpr(op)),
                        mkexpr(tab_addr)));

   assign(op1, load(Ity_I8, mkexpr(result)));

   if (! s390_host_has_etf2 || (m3 & 0x1) == 0) {
      s390_cc_set_val(1);
      next_insn_if(binop(Iop_CmpEQ8, mkexpr(op1), mkexpr(test_byte)));
   }
   store(get_gpr_dw0(r1), mkexpr(op1));

   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(src_addr), mkU64(2)));
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(des_addr), mkU64(1)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(2)));

   iterate();

   return "trto";
}

static const HChar *
s390_irgen_TROT(UChar m3, UChar r1, UChar r2)
{
   IRTemp src_addr, des_addr, tab_addr, src_len, test_byte;
   src_addr = newTemp(Ity_I64);
   des_addr = newTemp(Ity_I64);
   tab_addr = newTemp(Ity_I64);
   test_byte = newTemp(Ity_I16);
   src_len = newTemp(Ity_I64);

   assign(src_addr, get_gpr_dw0(r2));
   assign(des_addr, get_gpr_dw0(r1));
   assign(tab_addr, get_gpr_dw0(1));
   assign(src_len, get_gpr_dw0(r1+1));
   assign(test_byte, get_gpr_hw3(0));

   IRTemp op = newTemp(Ity_I8);
   IRTemp op1 = newTemp(Ity_I16);
   IRTemp result = newTemp(Ity_I64);

   /* End of source string? We're done; proceed to next insn */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpEQ64, mkexpr(src_len), mkU64(0)));

   /* Load character from source string, index translation table and
      store translated character in op1. */
   assign(op, binop(Iop_Shl8, load(Ity_I8, mkexpr(src_addr)), mkU8(1)));

   assign(result, binop(Iop_Add64, unop(Iop_8Uto64, mkexpr(op)), 
                        mkexpr(tab_addr)));
   assign(op1, load(Ity_I16, mkexpr(result)));

   if (! s390_host_has_etf2 || (m3 & 0x1) == 0) {
      s390_cc_set_val(1);
      next_insn_if(binop(Iop_CmpEQ16, mkexpr(op1), mkexpr(test_byte)));
   }
   store(get_gpr_dw0(r1), mkexpr(op1));

   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(src_addr), mkU64(1)));
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(des_addr), mkU64(2)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(1)));

   iterate();

   return "trot";
}

static const HChar *
s390_irgen_TRTT(UChar m3, UChar r1, UChar r2)
{
   IRTemp src_addr, des_addr, tab_addr, src_len, test_byte;
   src_addr = newTemp(Ity_I64);
   des_addr = newTemp(Ity_I64);
   tab_addr = newTemp(Ity_I64);
   test_byte = newTemp(Ity_I16);
   src_len = newTemp(Ity_I64);

   assign(src_addr, get_gpr_dw0(r2));
   assign(des_addr, get_gpr_dw0(r1));
   assign(tab_addr, get_gpr_dw0(1));
   assign(src_len, get_gpr_dw0(r1+1));
   assign(test_byte, get_gpr_hw3(0));

   IRTemp op = newTemp(Ity_I16);
   IRTemp op1 = newTemp(Ity_I16);
   IRTemp result = newTemp(Ity_I64);

   /* End of source string? We're done; proceed to next insn */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpEQ64, mkexpr(src_len), mkU64(0)));

   /* Load character from source string, index translation table and
      store translated character in op1. */
   assign(op, binop(Iop_Shl16, load(Ity_I16, mkexpr(src_addr)), mkU8(1)));

   assign(result, binop(Iop_Add64, unop(Iop_16Uto64, mkexpr(op)),
                        mkexpr(tab_addr)));
   assign(op1, load(Ity_I16, mkexpr(result)));

   if (! s390_host_has_etf2 || (m3 & 0x1) == 0) {
      s390_cc_set_val(1);
      next_insn_if(binop(Iop_CmpEQ16, mkexpr(op1), mkexpr(test_byte)));
   }

   store(get_gpr_dw0(r1), mkexpr(op1));

   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(src_addr), mkU64(2)));
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(des_addr), mkU64(2)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(2)));

   iterate();

   return "trtt";
}

static const HChar *
s390_irgen_TR(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I64);

   assign(len, mkU64(length));
   s390_irgen_TR_EX(len, start1, start2);

   return "tr";
}

static const HChar *
s390_irgen_TRE(UChar r1,UChar r2)
{
   IRTemp src_addr, tab_addr, src_len, test_byte;
   src_addr = newTemp(Ity_I64);
   tab_addr = newTemp(Ity_I64);
   src_len = newTemp(Ity_I64);
   test_byte = newTemp(Ity_I8);

   assign(src_addr, get_gpr_dw0(r1));
   assign(src_len, get_gpr_dw0(r1+1));
   assign(tab_addr, get_gpr_dw0(r2));
   assign(test_byte, get_gpr_b7(0));

   IRTemp op = newTemp(Ity_I8);
   IRTemp op1 = newTemp(Ity_I8);
   IRTemp result = newTemp(Ity_I64);

   /* End of source string? We're done; proceed to next insn */   
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpEQ64, mkexpr(src_len), mkU64(0)));

   /* Load character from source string and compare with test byte */
   assign(op, load(Ity_I8, mkexpr(src_addr)));
   
   s390_cc_set_val(1);
   next_insn_if(binop(Iop_CmpEQ8, mkexpr(op), mkexpr(test_byte)));

   assign(result, binop(Iop_Add64, unop(Iop_8Uto64, mkexpr(op)), 
			mkexpr(tab_addr)));

   assign(op1, load(Ity_I8, mkexpr(result)));

   store(get_gpr_dw0(r1), mkexpr(op1));
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(src_addr), mkU64(1)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(1)));

   iterate();

   return "tre";
}

static IRExpr *
s390_call_cu21(IRExpr *srcval, IRExpr *low_surrogate)
{
   IRExpr **args, *call;
   args = mkIRExprVec_2(srcval, low_surrogate);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/,
                       "s390_do_cu21", &s390_do_cu21, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static const HChar *
s390_irgen_CU21(UChar m3, UChar r1, UChar r2)
{
   IRTemp addr1 = newTemp(Ity_I64);
   IRTemp addr2 = newTemp(Ity_I64);
   IRTemp len1 = newTemp(Ity_I64);
   IRTemp len2 = newTemp(Ity_I64);

   assign(addr1, get_gpr_dw0(r1));
   assign(addr2, get_gpr_dw0(r2));
   assign(len1, get_gpr_dw0(r1 + 1));
   assign(len2, get_gpr_dw0(r2 + 1));

   /* We're processing the 2nd operand 2 bytes at a time. Therefore, if
      there are less than 2 bytes left, then the 2nd operand is exhausted
      and we're done here. cc = 0 */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len2), mkU64(2)));

   /* There are at least two bytes there. Read them. */
   IRTemp srcval = newTemp(Ity_I32);
   assign(srcval, unop(Iop_16Uto32, load(Ity_I16, mkexpr(addr2))));

   /* Find out whether this is a high surrogate. I.e. SRCVAL lies
      inside the interval [0xd800 - 0xdbff] */
   IRTemp  is_high_surrogate = newTemp(Ity_I32);
   IRExpr *flag1 = mkite(binop(Iop_CmpLE32U, mkU32(0xd800), mkexpr(srcval)),
                         mkU32(1), mkU32(0));
   IRExpr *flag2 = mkite(binop(Iop_CmpLE32U, mkexpr(srcval), mkU32(0xdbff)),
                         mkU32(1), mkU32(0));
   assign(is_high_surrogate, binop(Iop_And32, flag1, flag2));

   /* If SRCVAL is a high surrogate and there are less than 4 bytes left,
      then the 2nd operand is exhausted and we're done here. cc = 0 */
   IRExpr *not_enough_bytes =
      mkite(binop(Iop_CmpLT64U, mkexpr(len2), mkU64(4)), mkU32(1), mkU32(0));

   next_insn_if(binop(Iop_CmpEQ32,
                      binop(Iop_And32, mkexpr(is_high_surrogate),
                            not_enough_bytes), mkU32(1)));

   /* The 2nd operand is not exhausted. If the first 2 bytes are a high
      surrogate, read the next two bytes (low surrogate). */
   IRTemp  low_surrogate = newTemp(Ity_I32);
   IRExpr *low_surrogate_addr = binop(Iop_Add64, mkexpr(addr2), mkU64(2));

   assign(low_surrogate,
          mkite(binop(Iop_CmpEQ32, mkexpr(is_high_surrogate), mkU32(1)),
                unop(Iop_16Uto32, load(Ity_I16, low_surrogate_addr)),
                mkU32(0)));  // any value is fine; it will not be used

   /* Call the helper */
   IRTemp retval = newTemp(Ity_I64);
   assign(retval, s390_call_cu21(unop(Iop_32Uto64, mkexpr(srcval)),
                                 unop(Iop_32Uto64, mkexpr(low_surrogate))));

   /* Before we can test whether the 1st operand is exhausted we need to
      test for an invalid low surrogate. Because cc=2 outranks cc=1. */
   if (s390_host_has_etf3 && (m3 & 0x1) == 1) {
      IRExpr *invalid_low_surrogate =
         binop(Iop_And64, mkexpr(retval), mkU64(0xff));

      s390_cc_set_val(2);
      next_insn_if(binop(Iop_CmpEQ64, invalid_low_surrogate, mkU64(1)));
   }

   /* Now test whether the 1st operand is exhausted */
   IRTemp num_bytes = newTemp(Ity_I64);
   assign(num_bytes, binop(Iop_And64,
                           binop(Iop_Shr64, mkexpr(retval), mkU8(8)),
                           mkU64(0xff)));
   s390_cc_set_val(1);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len1), mkexpr(num_bytes)));

   /* Extract the bytes to be stored at addr1 */
   IRTemp data = newTemp(Ity_I64);
   assign(data, binop(Iop_Shr64, mkexpr(retval), mkU8(16)));

   /* To store the bytes construct 4 dirty helper calls. The helper calls
      are guarded (num_bytes == 1, num_bytes == 2, etc) such that only
      one of them will be called at runtime. */
   UInt i;
   for (i = 1; i <= 4; ++i) {
      IRDirty *d;

      d = unsafeIRDirty_0_N(0 /* regparms */, "s390x_dirtyhelper_CUxy",
                            &s390x_dirtyhelper_CUxy,
                            mkIRExprVec_3(mkexpr(addr1), mkexpr(data),
                                          mkexpr(num_bytes)));
      d->guard = binop(Iop_CmpEQ64, mkexpr(num_bytes), mkU64(i));
      d->mFx   = Ifx_Write;
      d->mAddr = mkexpr(addr1);
      d->mSize = i;
      stmt(IRStmt_Dirty(d));
   }

   /* Update source address and length */
   IRTemp num_src_bytes = newTemp(Ity_I64);
   assign(num_src_bytes,
          mkite(binop(Iop_CmpEQ32, mkexpr(is_high_surrogate), mkU32(1)),
                mkU64(4), mkU64(2)));
   put_gpr_dw0(r2,     binop(Iop_Add64, mkexpr(addr2), mkexpr(num_src_bytes)));
   put_gpr_dw0(r2 + 1, binop(Iop_Sub64, mkexpr(len2),  mkexpr(num_src_bytes)));

   /* Update destination address and length */
   put_gpr_dw0(r1,     binop(Iop_Add64, mkexpr(addr1), mkexpr(num_bytes)));
   put_gpr_dw0(r1 + 1, binop(Iop_Sub64, mkexpr(len1),  mkexpr(num_bytes)));

   iterate();

   return "cu21";
}

static IRExpr *
s390_call_cu24(IRExpr *srcval, IRExpr *low_surrogate)
{
   IRExpr **args, *call;
   args = mkIRExprVec_2(srcval, low_surrogate);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/,
                       "s390_do_cu24", &s390_do_cu24, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static const HChar *
s390_irgen_CU24(UChar m3, UChar r1, UChar r2)
{
   IRTemp addr1 = newTemp(Ity_I64);
   IRTemp addr2 = newTemp(Ity_I64);
   IRTemp len1 = newTemp(Ity_I64);
   IRTemp len2 = newTemp(Ity_I64);

   assign(addr1, get_gpr_dw0(r1));
   assign(addr2, get_gpr_dw0(r2));
   assign(len1, get_gpr_dw0(r1 + 1));
   assign(len2, get_gpr_dw0(r2 + 1));

   /* We're processing the 2nd operand 2 bytes at a time. Therefore, if
      there are less than 2 bytes left, then the 2nd operand is exhausted
      and we're done here. cc = 0 */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len2), mkU64(2)));

   /* There are at least two bytes there. Read them. */
   IRTemp srcval = newTemp(Ity_I32);
   assign(srcval, unop(Iop_16Uto32, load(Ity_I16, mkexpr(addr2))));

   /* Find out whether this is a high surrogate. I.e. SRCVAL lies
      inside the interval [0xd800 - 0xdbff] */
   IRTemp  is_high_surrogate = newTemp(Ity_I32);
   IRExpr *flag1 = mkite(binop(Iop_CmpLE32U, mkU32(0xd800), mkexpr(srcval)),
                         mkU32(1), mkU32(0));
   IRExpr *flag2 = mkite(binop(Iop_CmpLE32U, mkexpr(srcval), mkU32(0xdbff)),
                         mkU32(1), mkU32(0));
   assign(is_high_surrogate, binop(Iop_And32, flag1, flag2));

   /* If SRCVAL is a high surrogate and there are less than 4 bytes left,
      then the 2nd operand is exhausted and we're done here. cc = 0 */
   IRExpr *not_enough_bytes =
      mkite(binop(Iop_CmpLT64U, mkexpr(len2), mkU64(4)), mkU32(1), mkU32(0));

   next_insn_if(binop(Iop_CmpEQ32,
                      binop(Iop_And32, mkexpr(is_high_surrogate),
                            not_enough_bytes),
                      mkU32(1)));

   /* The 2nd operand is not exhausted. If the first 2 bytes are a high
      surrogate, read the next two bytes (low surrogate). */
   IRTemp  low_surrogate = newTemp(Ity_I32);
   IRExpr *low_surrogate_addr = binop(Iop_Add64, mkexpr(addr2), mkU64(2));

   assign(low_surrogate,
          mkite(binop(Iop_CmpEQ32, mkexpr(is_high_surrogate), mkU32(1)),
                unop(Iop_16Uto32, load(Ity_I16, low_surrogate_addr)),
                mkU32(0)));  // any value is fine; it will not be used

   /* Call the helper */
   IRTemp retval = newTemp(Ity_I64);
   assign(retval, s390_call_cu24(unop(Iop_32Uto64, mkexpr(srcval)),
                                 unop(Iop_32Uto64, mkexpr(low_surrogate))));

   /* Before we can test whether the 1st operand is exhausted we need to
      test for an invalid low surrogate. Because cc=2 outranks cc=1. */
   if (s390_host_has_etf3 && (m3 & 0x1) == 1) {
      IRExpr *invalid_low_surrogate =
         binop(Iop_And64, mkexpr(retval), mkU64(0xff));

      s390_cc_set_val(2);
      next_insn_if(binop(Iop_CmpEQ64, invalid_low_surrogate, mkU64(1)));
   }

   /* Now test whether the 1st operand is exhausted */
   s390_cc_set_val(1);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len1), mkU64(4)));

   /* Extract the bytes to be stored at addr1 */
   IRExpr *data = unop(Iop_64to32, binop(Iop_Shr64, mkexpr(retval), mkU8(8)));

   store(mkexpr(addr1), data);

   /* Update source address and length */
   IRTemp num_src_bytes = newTemp(Ity_I64);
   assign(num_src_bytes,
          mkite(binop(Iop_CmpEQ32, mkexpr(is_high_surrogate), mkU32(1)),
                mkU64(4), mkU64(2)));
   put_gpr_dw0(r2,     binop(Iop_Add64, mkexpr(addr2), mkexpr(num_src_bytes)));
   put_gpr_dw0(r2 + 1, binop(Iop_Sub64, mkexpr(len2),  mkexpr(num_src_bytes)));

   /* Update destination address and length */
   put_gpr_dw0(r1,     binop(Iop_Add64, mkexpr(addr1), mkU64(4)));
   put_gpr_dw0(r1 + 1, binop(Iop_Sub64, mkexpr(len1),  mkU64(4)));

   iterate();

   return "cu24";
}

static IRExpr *
s390_call_cu42(IRExpr *srcval)
{
   IRExpr **args, *call;
   args = mkIRExprVec_1(srcval);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/,
                       "s390_do_cu42", &s390_do_cu42, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static const HChar *
s390_irgen_CU42(UChar r1, UChar r2)
{
   IRTemp addr1 = newTemp(Ity_I64);
   IRTemp addr2 = newTemp(Ity_I64);
   IRTemp len1 = newTemp(Ity_I64);
   IRTemp len2 = newTemp(Ity_I64);

   assign(addr1, get_gpr_dw0(r1));
   assign(addr2, get_gpr_dw0(r2));
   assign(len1, get_gpr_dw0(r1 + 1));
   assign(len2, get_gpr_dw0(r2 + 1));

   /* We're processing the 2nd operand 4 bytes at a time. Therefore, if
      there are less than 4 bytes left, then the 2nd operand is exhausted
      and we're done here. cc = 0 */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len2), mkU64(4)));

   /* Read the 2nd operand. */
   IRTemp srcval = newTemp(Ity_I32);
   assign(srcval, load(Ity_I32, mkexpr(addr2)));

   /* Call the helper */
   IRTemp retval = newTemp(Ity_I64);
   assign(retval, s390_call_cu42(unop(Iop_32Uto64, mkexpr(srcval))));

   /* If the UTF-32 character was invalid, set cc=2 and we're done.
      cc=2 outranks cc=1 (1st operand exhausted) */
   IRExpr *invalid_character = binop(Iop_And64, mkexpr(retval), mkU64(0xff));

   s390_cc_set_val(2);
   next_insn_if(binop(Iop_CmpEQ64, invalid_character, mkU64(1)));

   /* Now test whether the 1st operand is exhausted */
   IRTemp num_bytes = newTemp(Ity_I64);
   assign(num_bytes, binop(Iop_And64,
                           binop(Iop_Shr64, mkexpr(retval), mkU8(8)),
                           mkU64(0xff)));
   s390_cc_set_val(1);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len1), mkexpr(num_bytes)));

   /* Extract the bytes to be stored at addr1 */
   IRTemp data = newTemp(Ity_I64);
   assign(data, binop(Iop_Shr64, mkexpr(retval), mkU8(16)));

   /* To store the bytes construct 2 dirty helper calls. The helper calls
      are guarded (num_bytes == 2 and num_bytes == 4, respectively) such
      that only one of them will be called at runtime. */

   Int i;
   for (i = 2; i <= 4; ++i) {
      IRDirty *d;

      if (i == 3) continue;  // skip this one

      d = unsafeIRDirty_0_N(0 /* regparms */, "s390x_dirtyhelper_CUxy",
                            &s390x_dirtyhelper_CUxy,
                            mkIRExprVec_3(mkexpr(addr1), mkexpr(data),
                                          mkexpr(num_bytes)));
      d->guard = binop(Iop_CmpEQ64, mkexpr(num_bytes), mkU64(i));
      d->mFx   = Ifx_Write;
      d->mAddr = mkexpr(addr1);
      d->mSize = i;
      stmt(IRStmt_Dirty(d));
   }

   /* Update source address and length */
   put_gpr_dw0(r2,     binop(Iop_Add64, mkexpr(addr2), mkU64(4)));
   put_gpr_dw0(r2 + 1, binop(Iop_Sub64, mkexpr(len2),  mkU64(4)));

   /* Update destination address and length */
   put_gpr_dw0(r1,     binop(Iop_Add64, mkexpr(addr1), mkexpr(num_bytes)));
   put_gpr_dw0(r1 + 1, binop(Iop_Sub64, mkexpr(len1),  mkexpr(num_bytes)));

   iterate();

   return "cu42";
}

static IRExpr *
s390_call_cu41(IRExpr *srcval)
{
   IRExpr **args, *call;
   args = mkIRExprVec_1(srcval);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/,
                       "s390_do_cu41", &s390_do_cu41, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static const HChar *
s390_irgen_CU41(UChar r1, UChar r2)
{
   IRTemp addr1 = newTemp(Ity_I64);
   IRTemp addr2 = newTemp(Ity_I64);
   IRTemp len1 = newTemp(Ity_I64);
   IRTemp len2 = newTemp(Ity_I64);

   assign(addr1, get_gpr_dw0(r1));
   assign(addr2, get_gpr_dw0(r2));
   assign(len1, get_gpr_dw0(r1 + 1));
   assign(len2, get_gpr_dw0(r2 + 1));

   /* We're processing the 2nd operand 4 bytes at a time. Therefore, if
      there are less than 4 bytes left, then the 2nd operand is exhausted
      and we're done here. cc = 0 */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len2), mkU64(4)));

   /* Read the 2nd operand. */
   IRTemp srcval = newTemp(Ity_I32);
   assign(srcval, load(Ity_I32, mkexpr(addr2)));

   /* Call the helper */
   IRTemp retval = newTemp(Ity_I64);
   assign(retval, s390_call_cu41(unop(Iop_32Uto64, mkexpr(srcval))));

   /* If the UTF-32 character was invalid, set cc=2 and we're done.
      cc=2 outranks cc=1 (1st operand exhausted) */
   IRExpr *invalid_character = binop(Iop_And64, mkexpr(retval), mkU64(0xff));

   s390_cc_set_val(2);
   next_insn_if(binop(Iop_CmpEQ64, invalid_character, mkU64(1)));

   /* Now test whether the 1st operand is exhausted */
   IRTemp num_bytes = newTemp(Ity_I64);
   assign(num_bytes, binop(Iop_And64,
                           binop(Iop_Shr64, mkexpr(retval), mkU8(8)),
                           mkU64(0xff)));
   s390_cc_set_val(1);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len1), mkexpr(num_bytes)));

   /* Extract the bytes to be stored at addr1 */
   IRTemp data = newTemp(Ity_I64);
   assign(data, binop(Iop_Shr64, mkexpr(retval), mkU8(16)));

   /* To store the bytes construct 4 dirty helper calls. The helper calls
      are guarded (num_bytes == 1, num_bytes == 2, etc) such that only
      one of them will be called at runtime. */
   UInt i;
   for (i = 1; i <= 4; ++i) {
      IRDirty *d;

      d = unsafeIRDirty_0_N(0 /* regparms */, "s390x_dirtyhelper_CUxy",
                            &s390x_dirtyhelper_CUxy,
                            mkIRExprVec_3(mkexpr(addr1), mkexpr(data),
                                          mkexpr(num_bytes)));
      d->guard = binop(Iop_CmpEQ64, mkexpr(num_bytes), mkU64(i));
      d->mFx   = Ifx_Write;
      d->mAddr = mkexpr(addr1);
      d->mSize = i;
      stmt(IRStmt_Dirty(d));
   }

   /* Update source address and length */
   put_gpr_dw0(r2,     binop(Iop_Add64, mkexpr(addr2), mkU64(4)));
   put_gpr_dw0(r2 + 1, binop(Iop_Sub64, mkexpr(len2),  mkU64(4)));

   /* Update destination address and length */
   put_gpr_dw0(r1,     binop(Iop_Add64, mkexpr(addr1), mkexpr(num_bytes)));
   put_gpr_dw0(r1 + 1, binop(Iop_Sub64, mkexpr(len1),  mkexpr(num_bytes)));

   iterate();

   return "cu41";
}

static IRExpr *
s390_call_cu12_cu14_helper1(IRExpr *byte1, IRExpr *etf3_and_m3_is_1)
{
   IRExpr **args, *call;
   args = mkIRExprVec_2(byte1, etf3_and_m3_is_1);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/, "s390_do_cu12_cu14_helper1",
                        &s390_do_cu12_cu14_helper1, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static IRExpr *
s390_call_cu12_helper2(IRExpr *byte1, IRExpr *byte2, IRExpr *byte3,
                       IRExpr *byte4, IRExpr *stuff)
{
   IRExpr **args, *call;
   args = mkIRExprVec_5(byte1, byte2, byte3, byte4, stuff);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/,
                        "s390_do_cu12_helper2", &s390_do_cu12_helper2, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static IRExpr *
s390_call_cu14_helper2(IRExpr *byte1, IRExpr *byte2, IRExpr *byte3,
                       IRExpr *byte4, IRExpr *stuff)
{
   IRExpr **args, *call;
   args = mkIRExprVec_5(byte1, byte2, byte3, byte4, stuff);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/,
                        "s390_do_cu14_helper2", &s390_do_cu14_helper2, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static void
s390_irgen_cu12_cu14(UChar m3, UChar r1, UChar r2, Bool is_cu12)
{
   IRTemp addr1 = newTemp(Ity_I64);
   IRTemp addr2 = newTemp(Ity_I64);
   IRTemp len1 = newTemp(Ity_I64);
   IRTemp len2 = newTemp(Ity_I64);

   assign(addr1, get_gpr_dw0(r1));
   assign(addr2, get_gpr_dw0(r2));
   assign(len1, get_gpr_dw0(r1 + 1));
   assign(len2, get_gpr_dw0(r2 + 1));

   UInt extended_checking = s390_host_has_etf3 && (m3 & 0x1) == 1;

   /* We're processing the 2nd operand 1 byte at a time. Therefore, if
      there is less than 1 byte left, then the 2nd operand is exhausted
      and we're done here. cc = 0 */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len2), mkU64(1)));

   /* There is at least one byte there. Read it. */
   IRTemp byte1 = newTemp(Ity_I64);
   assign(byte1, unop(Iop_8Uto64, load(Ity_I8, mkexpr(addr2))));

   /* Call the helper to get number of bytes and invalid byte indicator */
   IRTemp retval1 = newTemp(Ity_I64);
   assign(retval1, s390_call_cu12_cu14_helper1(mkexpr(byte1),
                                               mkU64(extended_checking)));

   /* Check for invalid 1st byte */
   IRExpr *is_invalid = unop(Iop_64to1, mkexpr(retval1));
   s390_cc_set_val(2);
   next_insn_if(is_invalid);

   /* How many bytes do we have to read? */
   IRTemp num_src_bytes = newTemp(Ity_I64);
   assign(num_src_bytes, binop(Iop_Shr64, mkexpr(retval1), mkU8(8)));

   /* Now test whether the 2nd operand is exhausted */
   s390_cc_set_val(0);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len2), mkexpr(num_src_bytes)));

   /* Read the remaining bytes */
   IRExpr *cond, *addr, *byte2, *byte3, *byte4;

   cond  = binop(Iop_CmpLE64U, mkU64(2), mkexpr(num_src_bytes));
   addr  = binop(Iop_Add64, mkexpr(addr2), mkU64(1));
   byte2 = mkite(cond, unop(Iop_8Uto64, load(Ity_I8, addr)), mkU64(0));
   cond  = binop(Iop_CmpLE64U, mkU64(3), mkexpr(num_src_bytes));
   addr  = binop(Iop_Add64, mkexpr(addr2), mkU64(2));
   byte3 = mkite(cond, unop(Iop_8Uto64, load(Ity_I8, addr)), mkU64(0));
   cond  = binop(Iop_CmpLE64U, mkU64(4), mkexpr(num_src_bytes));
   addr  = binop(Iop_Add64, mkexpr(addr2), mkU64(3));
   byte4 = mkite(cond, unop(Iop_8Uto64, load(Ity_I8, addr)), mkU64(0));

   /* Call the helper to get the converted value and invalid byte indicator.
      We can pass at most 5 arguments; therefore some encoding is needed
      here */
   IRExpr *stuff = binop(Iop_Or64,
                         binop(Iop_Shl64, mkexpr(num_src_bytes), mkU8(1)),
                         mkU64(extended_checking));
   IRTemp retval2 = newTemp(Ity_I64);

   if (is_cu12) {
      assign(retval2, s390_call_cu12_helper2(mkexpr(byte1), byte2, byte3,
                                             byte4, stuff));
   } else {
      assign(retval2, s390_call_cu14_helper2(mkexpr(byte1), byte2, byte3,
                                             byte4, stuff));
   }

   /* Check for invalid character */
   s390_cc_set_val(2);
   is_invalid = unop(Iop_64to1, mkexpr(retval2));
   next_insn_if(is_invalid);

   /* Now test whether the 1st operand is exhausted */
   IRTemp num_bytes = newTemp(Ity_I64);
   assign(num_bytes, binop(Iop_And64,
                           binop(Iop_Shr64, mkexpr(retval2), mkU8(8)),
                           mkU64(0xff)));
   s390_cc_set_val(1);
   next_insn_if(binop(Iop_CmpLT64U, mkexpr(len1), mkexpr(num_bytes)));

   /* Extract the bytes to be stored at addr1 */
   IRTemp data = newTemp(Ity_I64);
   assign(data, binop(Iop_Shr64, mkexpr(retval2), mkU8(16)));

   if (is_cu12) {
      /* To store the bytes construct 2 dirty helper calls. The helper calls
         are guarded (num_bytes == 2 and num_bytes == 4, respectively) such
         that only one of them will be called at runtime. */

      Int i;
      for (i = 2; i <= 4; ++i) {
         IRDirty *d;

         if (i == 3) continue;  // skip this one

         d = unsafeIRDirty_0_N(0 /* regparms */, "s390x_dirtyhelper_CUxy",
                               &s390x_dirtyhelper_CUxy,
                               mkIRExprVec_3(mkexpr(addr1), mkexpr(data),
                                             mkexpr(num_bytes)));
         d->guard = binop(Iop_CmpEQ64, mkexpr(num_bytes), mkU64(i));
         d->mFx   = Ifx_Write;
         d->mAddr = mkexpr(addr1);
         d->mSize = i;
         stmt(IRStmt_Dirty(d));
      }
   } else {
      // cu14
      store(mkexpr(addr1), unop(Iop_64to32, mkexpr(data)));
   }

   /* Update source address and length */
   put_gpr_dw0(r2,     binop(Iop_Add64, mkexpr(addr2), mkexpr(num_src_bytes)));
   put_gpr_dw0(r2 + 1, binop(Iop_Sub64, mkexpr(len2),  mkexpr(num_src_bytes)));

   /* Update destination address and length */
   put_gpr_dw0(r1,     binop(Iop_Add64, mkexpr(addr1), mkexpr(num_bytes)));
   put_gpr_dw0(r1 + 1, binop(Iop_Sub64, mkexpr(len1),  mkexpr(num_bytes)));

   iterate();
}

static const HChar *
s390_irgen_CU12(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_cu12_cu14(m3, r1, r2, /* is_cu12 = */ 1);

   return "cu12";
}

static const HChar *
s390_irgen_CU14(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_cu12_cu14(m3, r1, r2, /* is_cu12 = */ 0);

   return "cu14";
}

static IRExpr *
s390_call_ecag(IRExpr *op2addr)
{
   IRExpr **args, *call;

   args = mkIRExprVec_1(op2addr);
   call = mkIRExprCCall(Ity_I64, 0 /*regparm*/,
                        "s390_do_ecag", &s390_do_ecag, args);

   /* Nothing is excluded from definedness checking. */
   call->Iex.CCall.cee->mcx_mask = 0;

   return call;
}

static const HChar *
s390_irgen_ECAG(UChar r1, UChar r3 __attribute__((unused)), IRTemp op2addr)
{
   if (! s390_host_has_gie) {
      emulation_failure(EmFail_S390X_ecag);
   } else {
      put_gpr_dw0(r1, s390_call_ecag(mkexpr(op2addr)));
   }

   return "ecag";
}

static const HChar *
s390_irgen_VL(UChar v1, IRTemp op2addr)
{
   put_vr_qw(v1, load(Ity_V128, mkexpr(op2addr)));

   return "vl";
}

static const HChar *
s390_irgen_VLR(UChar v1, UChar v2)
{
   put_vr_qw(v1, get_vr_qw(v2));

   return "vlr";
}

static const HChar *
s390_irgen_VST(UChar v1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_vr_qw(v1));

   return "vst";
}

static const HChar *
s390_irgen_VLREP(UChar v1, IRTemp op2addr, UChar m3)
{
   IRType o2type = s390_vr_get_type(m3);
   IRExpr* o2 = load(o2type, mkexpr(op2addr));
   s390_vr_fill(v1, o2);
   return "vlrep";
}

static const HChar *
s390_irgen_VLEB(UChar v1, IRTemp op2addr, UChar m3)
{
   IRExpr* o2 = load(Ity_I8, mkexpr(op2addr));
   put_vr(v1, Ity_I8, m3, o2);

   return "vleb";
}

static const HChar *
s390_irgen_VLEH(UChar v1, IRTemp op2addr, UChar m3)
{
   IRExpr* o2 = load(Ity_I16, mkexpr(op2addr));
   put_vr(v1, Ity_I16, m3, o2);

   return "vleh";
}

static const HChar *
s390_irgen_VLEF(UChar v1, IRTemp op2addr, UChar m3)
{
   IRExpr* o2 = load(Ity_I32, mkexpr(op2addr));
   put_vr(v1, Ity_I32, m3, o2);

   return "vlef";
}

static const HChar *
s390_irgen_VLEG(UChar v1, IRTemp op2addr, UChar m3)
{
   IRExpr* o2 = load(Ity_I64, mkexpr(op2addr));
   put_vr(v1, Ity_I64, m3, o2);

   return "vleg";
}

static const HChar *
s390_irgen_VLEIB(UChar v1, UShort i2, UChar m3)
{
   IRExpr* o2 = unop(Iop_16to8, mkU16(i2));
   put_vr(v1, Ity_I8, m3, o2);

   return "vleib";
}

static const HChar *
s390_irgen_VLEIH(UChar v1, UShort i2, UChar m3)
{
   IRExpr* o2 = mkU16(i2);
   put_vr(v1, Ity_I16, m3, o2);

   return "vleih";
}

static const HChar *
s390_irgen_VLEIF(UChar v1, UShort i2, UChar m3)
{
   IRExpr* o2 = unop(Iop_16Sto32, mkU16(i2));
   put_vr(v1, Ity_I32, m3, o2);

   return "vleif";
}

static const HChar *
s390_irgen_VLEIG(UChar v1, UShort i2, UChar m3)
{
   IRExpr* o2 = unop(Iop_16Sto64, mkU16(i2));
   put_vr(v1, Ity_I64, m3, o2);

   return "vleig";
}

static const HChar *
s390_irgen_VLGV(UChar r1, IRTemp op2addr, UChar v3, UChar m4)
{
   IRType o2type = s390_vr_get_type(m4);
   IRExpr* index = unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr), mkU64(0xf)));
   IRExpr* o2;
   IRExpr* result;
   switch (o2type) {
   case Ity_I8:
      o2 = binop(Iop_GetElem8x16, get_vr_qw(v3), index);
      result = unop(Iop_8Uto64, o2);
      break;
   case Ity_I16:
      o2 = binop(Iop_GetElem16x8, get_vr_qw(v3), index);
      result = unop(Iop_16Uto64, o2);
      break;
   case Ity_I32:
      o2 = binop(Iop_GetElem32x4, get_vr_qw(v3), index);
      result = unop(Iop_32Uto64, o2);
      break;
   case Ity_I64:
      result = binop(Iop_GetElem64x2, get_vr_qw(v3), index);
      break;
   default:
      ppIRType(o2type);
      vpanic("s390_irgen_VLGV: unknown o2type");
   }

   put_gpr_dw0(r1, result);
   return "vlgv";
}

static const HChar *
s390_irgen_VGBM(UChar v1, UShort i2, UChar m3 __attribute__((unused)))
{
   put_vr_qw(v1, IRExpr_Const(IRConst_V128(i2)));

   return "vgbm";
}

static const HChar *
s390_irgen_VGM(UChar v1, UShort i2, UChar m3)
{
   UChar from = (i2 & 0xff00) >> 8;
   UChar to   = (i2 & 0x00ff);
   ULong value = 0UL;
   IRType type = s390_vr_get_type(m3);
   vassert(from <= to);

   UChar maxIndex = 0;
   switch (type) {
   case Ity_I8:
      maxIndex = 7;
      break;
   case Ity_I16:
      maxIndex = 15;
      break;
   case Ity_I32:
      maxIndex = 31;
      break;
   case Ity_I64:
      maxIndex = 63;
      break;
   default:
      vpanic("s390_irgen_VGM: unknown type");
   }

   for(UChar index = from; index <= to; index++) {
      value |= (1ULL << (maxIndex - index));
   }

   IRExpr *fillValue;
   switch (type) {
   case Ity_I8:
      fillValue = mkU8(value);
      break;
   case Ity_I16:
      fillValue = mkU16(value);
      break;
   case Ity_I32:
      fillValue = mkU32(value);
      break;
   case Ity_I64:
      fillValue = mkU64(value);
      break;
   default:
      vpanic("s390_irgen_VGM: unknown type");
   }

   s390_vr_fill(v1, fillValue);
   return "vgm";
}

static const HChar *
s390_irgen_VLLEZ(UChar v1, IRTemp op2addr, UChar m3)
{
   IRType type = s390_vr_get_type(m3);
   IRExpr* op2 = load(type, mkexpr(op2addr));
   IRExpr* op2as64bit;
   switch (type) {
   case Ity_I8:
      op2as64bit = unop(Iop_8Uto64, op2);
      break;
   case Ity_I16:
      op2as64bit = unop(Iop_16Uto64, op2);
      break;
   case Ity_I32:
      op2as64bit = unop(Iop_32Uto64, op2);
      break;
   case Ity_I64:
      op2as64bit = op2;
      break;
   default:
      vpanic("s390_irgen_VLLEZ: unknown type");
   }

   put_vr_dw0(v1, op2as64bit);
   put_vr_dw1(v1, mkU64(0));
   return "vllez";
}

static const HChar *
s390_irgen_VGEF(UChar v1, IRTemp op2addr, UChar m3)
{
   put_vr(v1, Ity_I32, m3, load(Ity_I32, mkexpr(op2addr)));
   return "vgef";
}

static const HChar *
s390_irgen_VGEG(UChar v1, IRTemp op2addr, UChar m3)
{
   put_vr(v1, Ity_I64, m3, load(Ity_I64, mkexpr(op2addr)));
   return "vgeg";
}

static const HChar *
s390_irgen_VLM(UChar v1, IRTemp op2addr, UChar v3)
{
   IRExpr* current = mkexpr(op2addr);
   vassert(v3 >= v1);
   vassert(v3 - v1 <= 16);

   for(UChar vr = v1; vr <= v3; vr++) {
         IRExpr* next = binop(Iop_Add64, current, mkU64(16));
         put_vr_qw(vr, load(Ity_V128, current));
         current = next;
   }

   return "vlm";
}

static const HChar *
s390_irgen_VLVGP(UChar v1, UChar r2, UChar r3)
{
   put_vr_qw(v1, binop(Iop_64HLtoV128, get_gpr_dw0(r2), get_gpr_dw0(r3)));

   return "vlvgp";
}

static const HChar *
s390_irgen_VLVG(UChar v1, IRTemp op2addr, UChar r3, UChar m4)
{
   IRType type = s390_vr_get_type(m4);
   IRExpr* index = unop(Iop_64to8, mkexpr(op2addr));
   IRExpr* vr = get_vr_qw(v1);
   IRExpr* operand;
   switch (type) {
   case Ity_I8:
      operand = unop(Iop_64to8, get_gpr_dw0(r3));
      put_vr_qw(v1, triop(Iop_SetElem8x16, vr, index, operand));
      break;
   case Ity_I16:
      operand = unop(Iop_64to16, get_gpr_dw0(r3));
      put_vr_qw(v1, triop(Iop_SetElem16x8, vr, index, operand));
      break;
   case Ity_I32:
      operand = unop(Iop_64to32, get_gpr_dw0(r3));
      put_vr_qw(v1, triop(Iop_SetElem32x4, vr, index, operand));
      break;
   case Ity_I64:
      operand = get_gpr_dw0(r3);
      put_vr_qw(v1, triop(Iop_SetElem64x2, vr, index, operand));
      break;
   default:
      vpanic("s390_irgen_VLVG: unknown type");
   }

   return "vlvg";
}

static const HChar *
s390_irgen_VMRH(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_InterleaveHI8x16, Iop_InterleaveHI16x8,
                        Iop_InterleaveHI32x4, Iop_InterleaveHI64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmrh";
}

static const HChar *
s390_irgen_VMRL(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_InterleaveLO8x16, Iop_InterleaveLO16x8,
                        Iop_InterleaveLO32x4, Iop_InterleaveLO64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmrl";
}

static const HChar *
s390_irgen_VPK(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_NarrowBin16to8x16, Iop_NarrowBin32to16x8,
                        Iop_NarrowBin64to32x4 };
   Char index = m4 - 1;
   vassert((index >= 0) && (index < sizeof(ops) / sizeof(ops[0])));
   put_vr_qw(v1, binop(ops[index], get_vr_qw(v2), get_vr_qw(v3)));
   return "vpk";
}

static const HChar *
s390_irgen_VPERM(UChar v1, UChar v2, UChar v3, UChar v4)
{
   put_vr_qw(v1, triop(Iop_Perm8x16x2,
             get_vr_qw(v2), get_vr_qw(v3), get_vr_qw(v4)));

   return "vperm";
}

static const HChar *
s390_irgen_VSCEF(UChar v1, IRTemp op2addr, UChar m3)
{
   store(mkexpr(op2addr), get_vr(v1, Ity_I32, m3));
   return "vscef";
}

static const HChar *
s390_irgen_VSCEG(UChar v1, IRTemp op2addr, UChar m3)
{
   store(mkexpr(op2addr), get_vr(v1, Ity_I64, m3));
   return "vsceg";
}

static const HChar *
s390_irgen_VPDI(UChar v1, UChar v2, UChar v3, UChar m4)
{
   /* Theese bits are reserved by specification */
   vassert((m4 & 2) == 0);
   vassert((m4 & 8) == 0);

   if((m4 & 4) != 0)
      put_vr_dw0(v1, get_vr_dw1(v2));
   else
      put_vr_dw0(v1, get_vr_dw0(v2));

   if((m4 & 1) != 0)
      put_vr_dw1(v1, get_vr_dw1(v3));
   else
      put_vr_dw1(v1, get_vr_dw0(v3));

   return "vpdi";
}

static const HChar *
s390_irgen_VSEG(UChar v1, UChar v2, UChar m3)
{
   IRType type = s390_vr_get_type(m3);
   switch(type) {
   case Ity_I8:
      put_vr_dw0(v1, unop(Iop_8Sto64, get_vr_b7(v2)));
      put_vr_dw1(v1, unop(Iop_8Sto64, get_vr_b15(v2)));
      break;
   case Ity_I16:
      put_vr_dw0(v1, unop(Iop_16Sto64, get_vr_hw3(v2)));
      put_vr_dw1(v1, unop(Iop_16Sto64, get_vr_hw7(v2)));
      break;
   case Ity_I32:
      put_vr_dw0(v1, unop(Iop_32Sto64, get_vr_w1(v2)));
      put_vr_dw1(v1, unop(Iop_32Sto64, get_vr_w3(v2)));
      break;
   default:
      ppIRType(type);
      vpanic("s390_irgen_VSEG: unknown type");
   }

   return "vseg";
}

static const HChar *
s390_irgen_VSTEB(UChar v1, IRTemp op2addr, UChar m3)
{
   store(mkexpr(op2addr), get_vr(v1, Ity_I8, m3));

   return "vsteb";
}

static const HChar *
s390_irgen_VSTEH(UChar v1, IRTemp op2addr, UChar m3)
{
   store(mkexpr(op2addr), get_vr(v1, Ity_I16, m3));

   return "vsteh";
}

static const HChar *
s390_irgen_VSTEF(UChar v1, IRTemp op2addr, UChar m3)
{
   store(mkexpr(op2addr), get_vr(v1, Ity_I32, m3));

   return "vstef";
}

static const HChar *
s390_irgen_VSTEG(UChar v1, IRTemp op2addr, UChar m3)
{
   store(mkexpr(op2addr), get_vr(v1, Ity_I64, m3));

   return "vsteg";
}

static const HChar *
s390_irgen_VSTM(UChar v1, IRTemp op2addr, UChar v3)
{
   IRExpr* current = mkexpr(op2addr);
   vassert(v3 >= v1);
   vassert(v3 - v1 <= 16);

   for(UChar vr = v1; vr <= v3; vr++) {
         IRExpr* next = binop(Iop_Add64, current, mkU64(16));
         store(current, get_vr_qw(vr));
         current = next;
   }

   return "vstm";
}

static const HChar *
s390_irgen_VUPH(UChar v1, UChar v2, UChar m3)
{
   const IROp ops[] = { Iop_Widen8Sto16x8, Iop_Widen16Sto32x4, Iop_Widen32Sto64x2 };
   vassert(m3 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, unop(ops[m3], get_vr_dw0(v2)));

   return "vuph";
}

static const HChar *
s390_irgen_VUPLH(UChar v1, UChar v2, UChar m3)
{
   const IROp ops[] = { Iop_Widen8Uto16x8, Iop_Widen16Uto32x4, Iop_Widen32Uto64x2 };
   vassert(m3 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, unop(ops[m3], get_vr_dw0(v2)));
   return "vuplh";
}

static const HChar *
s390_irgen_VUPL(UChar v1, UChar v2, UChar m3)
{
   const IROp ops[] = { Iop_Widen8Sto16x8, Iop_Widen16Sto32x4, Iop_Widen32Sto64x2 };
   vassert(m3 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, unop(ops[m3], get_vr_dw1(v2)));

   return "vupl";
}

static const HChar *
s390_irgen_VUPLL(UChar v1, UChar v2, UChar m3)
{
   const IROp ops[] = { Iop_Widen8Uto16x8, Iop_Widen16Uto32x4, Iop_Widen32Uto64x2 };
   vassert(m3 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, unop(ops[m3], get_vr_dw1(v2)));

   return "vupll";
}

static const HChar *
s390_irgen_VREP(UChar v1, UChar v3, UShort i2, UChar m4)
{
   IRType type = s390_vr_get_type(m4);
   IRExpr* arg = get_vr(v3, type, i2);
   s390_vr_fill(v1, arg);

   return "vrep";
}

static const HChar *
s390_irgen_VREPI(UChar v1, UShort i2, UChar m3)
{
   IRType type = s390_vr_get_type(m3);
   IRExpr *value;
   switch (type) {
   case Ity_I8:
      value = mkU8((UChar)i2);
      break;
   case Ity_I16:
      value = mkU16(i2);
      break;
   case Ity_I32:
      value = unop(Iop_16Sto32, mkU16(i2));
      break;
   case Ity_I64:
      value = unop(Iop_16Sto64, mkU16(i2));
      break;
  default:
    ppIRType(type);
    vpanic("s390_irgen_VREPI: unknown type");
  }
  s390_vr_fill(v1, value);

  return "vrepi";
}

static const HChar *
s390_irgen_VPKS(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (!s390_vr_is_cs_set(m5)) {
      const IROp ops[] = { Iop_QNarrowBin16Sto8Sx16, Iop_QNarrowBin32Sto16Sx8,
                           Iop_QNarrowBin64Sto32Sx4 };
      Char index = m4 - 1;
      vassert((index >= 0) && (index < sizeof(ops) / sizeof(ops[0])));
      put_vr_qw(v1, binop(ops[index], get_vr_qw(v2), get_vr_qw(v3)));

   } else {
      IRDirty* d;
      IRTemp cc = newTemp(Ity_I64);

      s390x_vec_op_details_t details = { .serialized = 0ULL };
      details.op = S390_VEC_OP_VPKS;
      details.v1 = v1;
      details.v2 = v2;
      details.v3 = v3;
      details.m4 = m4;
      details.m5 = m5;

      d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                            &s390x_dirtyhelper_vec_op,
                            mkIRExprVec_2(IRExpr_GSPTR(),
                                          mkU64(details.serialized)));

      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx     = Ifx_Read;
      d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
      d->fxState[0].size   = sizeof(V128);
      d->fxState[1].fx     = Ifx_Read;
      d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
      d->fxState[1].size   = sizeof(V128);
      d->fxState[2].fx     = Ifx_Write;
      d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
      d->fxState[2].size   = sizeof(V128);

      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }

   return "vpks";
}

static const HChar *
s390_irgen_VPKLS(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (!s390_vr_is_cs_set(m5)) {
      const IROp ops[] = { Iop_QNarrowBin16Uto8Ux16, Iop_QNarrowBin32Uto16Ux8,
                           Iop_QNarrowBin64Uto32Ux4 };
      Char index = m4 - 1;
      vassert((index >= 0) && (index < sizeof(ops) / sizeof(ops[0])));
      put_vr_qw(v1, binop(ops[index], get_vr_qw(v2), get_vr_qw(v3)));

   } else {
      IRDirty* d;
      IRTemp cc = newTemp(Ity_I64);

      s390x_vec_op_details_t details = { .serialized = 0ULL };
      details.op = S390_VEC_OP_VPKLS;
      details.v1 = v1;
      details.v2 = v2;
      details.v3 = v3;
      details.m4 = m4;
      details.m5 = m5;

      d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                            &s390x_dirtyhelper_vec_op,
                            mkIRExprVec_2(IRExpr_GSPTR(),
                                          mkU64(details.serialized)));

      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx     = Ifx_Read;
      d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
      d->fxState[0].size   = sizeof(V128);
      d->fxState[1].fx     = Ifx_Read;
      d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
      d->fxState[1].size   = sizeof(V128);
      d->fxState[2].fx     = Ifx_Write;
      d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
      d->fxState[2].size   = sizeof(V128);

      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }

   return "vpkls";
}

static const HChar *
s390_irgen_VSEL(UChar v1, UChar v2, UChar v3, UChar v4)
{
   IRExpr* vIfTrue = get_vr_qw(v2);
   IRExpr* vIfFalse = get_vr_qw(v3);
   IRExpr* vCond = get_vr_qw(v4);

   put_vr_qw(v1, s390_V128_bitwiseITE(vCond, vIfTrue, vIfFalse));
   return "vsel";
}

static const HChar *
s390_irgen_VLBB(UChar v1, IRTemp addr, UChar m3)
{
   IRExpr* maxIndex = binop(Iop_Sub32,
                            s390_getCountToBlockBoundary(addr, m3),
                            mkU32(1));

   s390_vr_loadWithLength(v1, addr, maxIndex);

   return "vlbb";
}

static const HChar *
s390_irgen_VLL(UChar v1, IRTemp addr, UChar r3)
{
   s390_vr_loadWithLength(v1, addr, get_gpr_w1(r3));

   return "vll";
}

static const HChar *
s390_irgen_VSTL(UChar v1, IRTemp addr, UChar r3)
{
   IRTemp counter = newTemp(Ity_I64);
   IRTemp maxIndexToStore = newTemp(Ity_I64);
   IRTemp gpr3 = newTemp(Ity_I64);

   assign(gpr3, unop(Iop_32Uto64, get_gpr_w1(r3)));
   assign(maxIndexToStore, mkite(binop(Iop_CmpLE64U,
                                       mkexpr(gpr3),
                                       mkU64(16)
                                       ),
                                 mkexpr(gpr3),
                                 mkU64(16)
                                 )
         );

   assign(counter, get_counter_dw0());

   store(binop(Iop_Add64, mkexpr(addr), mkexpr(counter)),
         binop(Iop_GetElem8x16, get_vr_qw(v1), unop(Iop_64to8, mkexpr(counter))));

   /* Check for end of field */
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   iterate_if(binop(Iop_CmpNE64, mkexpr(counter), mkexpr(maxIndexToStore)));
   put_counter_dw0(mkU64(0));

   return "vstl";
}

static const HChar *
s390_irgen_VX(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, binop(Iop_XorV128, get_vr_qw(v2), get_vr_qw(v3)));

   return "vx";
}

static const HChar *
s390_irgen_VN(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, binop(Iop_AndV128, get_vr_qw(v2), get_vr_qw(v3)));

   return "vn";
}

static const HChar *
s390_irgen_VO(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, binop(Iop_OrV128, get_vr_qw(v2), get_vr_qw(v3)));

   return "vo";
}

static const HChar *
s390_irgen_VNO(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, unop(Iop_NotV128,
                      binop(Iop_OrV128, get_vr_qw(v2), get_vr_qw(v3))));

   return "vno";
}

static const HChar *
s390_irgen_LZRF(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, binop(Iop_And32, load(Ity_I32, mkexpr(op2addr)), mkU32(0xffffff00)));
   put_gpr_w1(r1, mkexpr(op2));

   return "lzrf";
}

static const HChar *
s390_irgen_LZRG(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, binop(Iop_And64, load(Ity_I64, mkexpr(op2addr)), mkU64(0xffffffffffffff00UL)));
   put_gpr_dw0(r1, mkexpr(op2));

   return "lzrg";
}

static const HChar *
s390_irgen_LLZRGF(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, binop(Iop_And32, load(Ity_I32, mkexpr(op2addr)), mkU32(0xffffff00)));
   put_gpr_w1(r1, mkexpr(op2));
   put_gpr_w0(r1, mkU32(0));

   return "llzrgf";
}

static const HChar *
s390_irgen_LOCFH(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   put_gpr_w0(r1, load(Ity_I32, mkexpr(op2addr)));

   return "locfh";
}

static const HChar *
s390_irgen_LOCFHR(UChar m3, UChar r1, UChar r2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_w0(r1, get_gpr_w0(r2));

   return "locfhr";
}

static const HChar *
s390_irgen_LOCHHI(UChar r1, UChar m3, UShort i2, UChar unused)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_w0(r1, mkU32((UInt)(Int)(Short)i2));

   return "lochhi";
}

static const HChar *
s390_irgen_LOCHI(UChar r1, UChar m3, UShort i2, UChar unused)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_w1(r1, mkU32((UInt)(Int)(Short)i2));

   return "lochi";
}

static const HChar *
s390_irgen_LOCGHI(UChar r1, UChar m3, UShort i2, UChar unused)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_dw0(r1, mkU64((ULong)(Long)(Short)i2));

   return "locghi";
}

static const HChar *
s390_irgen_STOCFH(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   store(mkexpr(op2addr), get_gpr_w1(r1));

   return "stocfh";
}

static const HChar *
s390_irgen_LCBB(UChar r1, IRTemp op2addr, UChar m3)
{
   IRTemp op2 = newTemp(Ity_I32);
   assign(op2, s390_getCountToBlockBoundary(op2addr, m3));
   put_gpr_w1(r1, mkexpr(op2));

   IRExpr* cc = mkite(binop(Iop_CmpEQ32, mkexpr(op2), mkU32(16)), mkU64(0), mkU64(3));
   s390_cc_thunk_fill(mkU64(S390_CC_OP_SET), cc, mkU64(0), mkU64(0));

   return "lcbb";
}

/* Regarding the use of 
   // Dummy helper which is used to signal VEX library that memory was loaded
   sha512_loadparam 
     = unsafeIRDirty_0_N(0, "s390x_dirtyhelper_PPNO_sha512_load_param_block",
                             &s390x_dirtyhelper_PPNO_sha512_load_param_block,
                             mkIRExprVec_0());

   in the following function (s390_irgen_PPNO).  This is a workaround to get
   around the fact that IRDirty annotations cannot indicate two memory side
   effects, which are unfortunately necessary here.  It will possibly lead to
   losing undefinedness (undefinedness in some inputs might not be propagated
   to the outputs as it shouod, in Memcheck).  The correct fix would be to
   extend IRDirty to represent two memory side effects, but that's quite a bit
   of work.

   Here's a summary of what this insn does.

   // getReg(RegisterNumber n) returns the value of GPR number 'n'

   // reg1 and reg2 are even
   void ppno(RegisterNumber reg1, RegisterNumber reg2) {

       switch(getReg(0)) {
       case 0x0:
           // Query mode, ignore reg1 and reg2
           // Write 16 bytes                    at  getReg(1)
           break;

       case 0x3:
           // SHA-512 generate mode, ignore reg2

           // Read 240 bytes                    at  getReg(1)
           // Write getReg(reg1 + 1) bytes      at  getReg(reg1)
           // Write some of 240 bytes starting  at  getReg(1)
           break;

       case 0x83:
           // SHA-512 seed mode, ignore reg1

           // Read some of 240 bytes starting  at  getReg(1)
           // Read getReg(reg2 + 1) bytes      at  getReg(reg2)
           // Write 240 bytes                  at  getReg(1)
           break;

       default:
           // Specification exception, abort execution.
       }
   }
*/
/* Also known as "prno"
   If you implement new functions please don't forget to update
   "s390x_dirtyhelper_PPNO_query" function.
 */
static const HChar *
s390_irgen_PPNO(UChar r1, UChar r2)
{
   if (!s390_host_has_msa5) {
      emulation_failure(EmFail_S390X_ppno);
      return "ppno";
   }

   /* Theese conditions lead to specification exception */
   vassert(r1 % 2 == 0);
   vassert(r2 % 2 == 0);
   vassert((r1 != 0) && (r2 != 0));

   IRDirty *query, *sha512_gen, *sha512_seed, *sha512_loadparam;
   IRTemp gpr1num = newTemp(Ity_I64);
   IRTemp gpr2num = newTemp(Ity_I64);

   IRTemp funcCode = newTemp(Ity_I8);
   IRTemp is_query = newTemp(Ity_I1);
   IRTemp is_sha512_gen = newTemp(Ity_I1);
   IRTemp is_sha512_seed = newTemp(Ity_I1);
   IRTemp is_sha512 = newTemp(Ity_I1);

   assign(funcCode, unop(Iop_64to8, binop(Iop_And64, get_gpr_dw0(0),
                                          mkU64(0xffULL))));
   assign(gpr1num, mkU64(r1));
   assign(gpr2num, mkU64(r2));

   assign(is_query, binop(Iop_CmpEQ8, mkexpr(funcCode), mkU8(S390_PPNO_QUERY)));
   assign(is_sha512_gen, binop(Iop_CmpEQ8, mkexpr(funcCode),
                               mkU8(S390_PPNO_SHA512_GEN)));
   assign(is_sha512_seed, binop(Iop_CmpEQ8, mkexpr(funcCode),
                                mkU8(S390_PPNO_SHA512_SEED)));
   assign(is_sha512, binop(Iop_CmpEQ8,
                           mkU8(S390_PPNO_SHA512_GEN),
                           binop(Iop_And8,
                                 mkexpr(funcCode),
                                 mkU8(S390_PPNO_SHA512_GEN)
                                 )
                           ));

   query = unsafeIRDirty_0_N(0, "s390x_dirtyhelper_PPNO_query",
                             &s390x_dirtyhelper_PPNO_query,
                             mkIRExprVec_3(IRExpr_GSPTR(), mkexpr(gpr1num),
                                           mkexpr(gpr2num)));
   query->guard = mkexpr(is_query);
   query->nFxState = 1;
   vex_bzero(&query->fxState, sizeof(query->fxState));
   query->fxState[0].fx     = Ifx_Read;
   query->fxState[0].offset = S390X_GUEST_OFFSET(guest_r0);
   query->fxState[0].size   = 2 * sizeof(ULong); /* gpr0 and gpr1 are read */
   query->mAddr = get_gpr_dw0(1);
   query->mSize = S390_PPNO_PARAM_BLOCK_SIZE_QUERY;
   query->mFx   = Ifx_Write;

   IRTemp gen_cc = newTemp(Ity_I64);
   sha512_gen = unsafeIRDirty_1_N(gen_cc, 0, "s390x_dirtyhelper_PPNO_sha512",
                             &s390x_dirtyhelper_PPNO_sha512,
                             mkIRExprVec_3(IRExpr_GSPTR(), mkexpr(gpr1num),
                                           mkexpr(gpr2num)));
   sha512_gen->guard = mkexpr(is_sha512_gen);
   sha512_gen->nFxState = 3;
   vex_bzero(&sha512_gen->fxState, sizeof(sha512_gen->fxState));
   sha512_gen->fxState[0].fx     = Ifx_Read;
   sha512_gen->fxState[0].offset = S390X_GUEST_OFFSET(guest_r0);
   sha512_gen->fxState[0].size   = 2 * sizeof(ULong); /* gpr0 and gpr1 are read */
   sha512_gen->fxState[1].fx     = Ifx_Read;
   sha512_gen->fxState[1].offset = S390X_GUEST_OFFSET(guest_r0) + r1 * sizeof(ULong);
   sha512_gen->fxState[1].size   = sizeof(ULong);
   sha512_gen->fxState[2].fx     = Ifx_Modify;
   sha512_gen->fxState[2].offset = S390X_GUEST_OFFSET(guest_r0) + (r1 + 1) * sizeof(ULong);
   sha512_gen->fxState[2].size   = sizeof(ULong);
   sha512_gen->mAddr = get_gpr_dw0(r1);
   sha512_gen->mSize = S390_PPNO_MAX_SIZE_SHA512_GEN;
   sha512_gen->mFx   = Ifx_Write;

   IRTemp unused = newTemp(Ity_I64);
   sha512_seed = unsafeIRDirty_1_N(unused, 0, "s390x_dirtyhelper_PPNO_sha512",
                             &s390x_dirtyhelper_PPNO_sha512,
                             mkIRExprVec_3(IRExpr_GSPTR(), mkexpr(gpr1num),
                                           mkexpr(gpr2num)));
   sha512_seed->guard = mkexpr(is_sha512_seed);
   sha512_seed->nFxState = 2;
   vex_bzero(&sha512_seed->fxState, sizeof(sha512_seed->fxState));
   sha512_seed->fxState[0].fx     = Ifx_Read;
   sha512_seed->fxState[0].offset = S390X_GUEST_OFFSET(guest_r0);
   sha512_seed->fxState[0].size   = 2 * sizeof(ULong); /* gpr0 and gpr1 are read */
   sha512_seed->fxState[1].fx     = Ifx_Read;
   sha512_seed->fxState[1].offset = S390X_GUEST_OFFSET(guest_r0) + r2 * sizeof(ULong);
   sha512_seed->fxState[1].size   = 2 * sizeof(ULong); /* r2 and r2 + 1 are read */
   sha512_seed->mAddr = get_gpr_dw0(r2);
   sha512_seed->mSize = S390_PPNO_MAX_SIZE_SHA512_SEED;
   sha512_seed->mFx   = Ifx_Write;

   /* Dummy helper which is used to signal VEX library that memory was loaded */
   sha512_loadparam =
      unsafeIRDirty_0_N(0, "s390x_dirtyhelper_PPNO_sha512_load_param_block",
                        &s390x_dirtyhelper_PPNO_sha512_load_param_block,
                        mkIRExprVec_0());
   sha512_loadparam->guard = mkexpr(is_sha512);
   sha512_loadparam->nFxState = 0;
   vex_bzero(&sha512_loadparam->fxState, sizeof(sha512_loadparam->fxState));
   sha512_loadparam->mAddr = get_gpr_dw0(1);
   sha512_loadparam->mSize = S390_PPNO_PARAM_BLOCK_SIZE_SHA512;
   sha512_loadparam->mFx   = Ifx_Read;

   IRDirty* sha512_saveparam =
      unsafeIRDirty_0_N(0, "s390x_dirtyhelper_PPNO_sha512_save_param_block",
                        &s390x_dirtyhelper_PPNO_sha512_load_param_block,
                        mkIRExprVec_0());
   sha512_saveparam->guard = mkexpr(is_sha512);
   sha512_saveparam->nFxState = 0;
   vex_bzero(&sha512_saveparam->fxState, sizeof(sha512_saveparam->fxState));
   sha512_saveparam->mAddr = get_gpr_dw0(1);
   sha512_saveparam->mSize = S390_PPNO_PARAM_BLOCK_SIZE_SHA512;
   sha512_saveparam->mFx   = Ifx_Write;

   stmt(IRStmt_Dirty(query));
   stmt(IRStmt_Dirty(sha512_loadparam));
   stmt(IRStmt_Dirty(sha512_gen));
   stmt(IRStmt_Dirty(sha512_seed));
   stmt(IRStmt_Dirty(sha512_saveparam));

   IRTemp cc = newTemp(Ity_I64);
   assign(cc,
          mkite(mkexpr(is_sha512_gen),
                mkexpr(gen_cc),
                mkU64(0)
               )
         );

   s390_cc_thunk_fill(mkU64(S390_CC_OP_SET), mkexpr(cc), mkU64(0), mkU64(0));

   return "ppno";
}

static const HChar *
s390_irgen_VFAE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   /* Check for specification exception */
   vassert(m4 < 3);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VFAE;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.m4 = m4;
   details.m5 = m5;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 3;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);
   d->fxState[2].fx     = Ifx_Write;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[2].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));

   if (s390_vr_is_cs_set(m5)) {
      s390_cc_set(cc);
   }

   return "vfae";
}

static const HChar *
s390_irgen_VFEE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   /* Check for specification exception */
   vassert(m4 < 3);
   vassert((m5 & 0b1100) == 0);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VFEE;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.m4 = m4;
   details.m5 = m5;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 3;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);
   d->fxState[2].fx     = Ifx_Write;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[2].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));

   if (s390_vr_is_cs_set(m5)) {
      s390_cc_set(cc);
   }

   return "vfee";
}

static const HChar *
s390_irgen_VFENE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   const Bool negateComparison = True;
   const IRType type = s390_vr_get_type(m4);

   /* Check for specification exception */
   vassert(m4 < 3);
   vassert((m5 & 0b1100) == 0);

   static const IROp elementGetters[] = {
      Iop_GetElem8x16, Iop_GetElem16x8, Iop_GetElem32x4
   };
   IROp getter = elementGetters[m4];

   static const IROp elementComparators[] = {
      Iop_CmpEQ8, Iop_CmpEQ16, Iop_CmpEQ32
   };
   IROp comparator = elementComparators[m4];

   static const IROp resultConverter[] = {Iop_64to8, Iop_64to16, Iop_64to32};
   IROp converter = resultConverter[m4];

   IRTemp isZeroElem;

   IRTemp counter = newTemp(Ity_I64);
   assign(counter, get_counter_dw0());

   IRTemp arg1 = newTemp(type);
   assign(arg1, binop(getter, get_vr_qw(v2), unop(Iop_64to8, mkexpr(counter))));
   IRTemp arg2 = newTemp(type);
   assign(arg2, binop(getter, get_vr_qw(v3), unop(Iop_64to8, mkexpr(counter))));

   IRTemp isGoodPair = newTemp(Ity_I1);
   if(negateComparison) {
      assign(isGoodPair, unop(Iop_Not1, binop(comparator, mkexpr(arg1),
                                              mkexpr(arg2))));
   } else {
      assign(isGoodPair, binop(comparator, mkexpr(arg1), mkexpr(arg2)));
   }

   if(s390_vr_is_zs_set(m5)) {
      isZeroElem = newTemp(Ity_I1);
      assign(isZeroElem, binop(comparator, mkexpr(arg1),
                               unop(converter, mkU64(0))));
   }

   static const UChar invalidIndices[] = {16, 8, 4};
   const UChar invalidIndex = invalidIndices[m4];
   IRTemp endOfVectorIsReached = newTemp(Ity_I1);
   assign(endOfVectorIsReached, binop(Iop_CmpEQ64, mkexpr(counter),
                                      mkU64(invalidIndex)));

   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   IRExpr* shouldBreak = binop(Iop_Or32,
                               unop(Iop_1Uto32, mkexpr(isGoodPair)),
                               unop(Iop_1Uto32, mkexpr(endOfVectorIsReached))
                              );
   if(s390_vr_is_zs_set(m5)) {
      shouldBreak = binop(Iop_Or32,
                          shouldBreak,
                          unop(Iop_1Uto32, mkexpr(isZeroElem)));
   }
   iterate_if(binop(Iop_CmpEQ32, shouldBreak, mkU32(0)));

   IRExpr* foundIndex = binop(Iop_Sub64, get_counter_dw0(), mkU64(1));
   if(m4 > 0) {
      /* We should return index of byte but we found index of element in
         general case.
            if byte elem (m4 == 0) then indexOfByte = indexOfElement
            if halfword elem (m4 == 1) then indexOfByte = 2 * indexOfElement
                                                        = indexOfElement << 1
            if word elem (m4 == 2) then indexOfByte = 4 * indexOfElement
                                                    = indexOfElement << 2
      */
      foundIndex = binop(Iop_Shl64, foundIndex, mkU8(m4));
   }

   IRTemp result = newTemp(Ity_I64);
   assign(result, mkite(mkexpr(endOfVectorIsReached),
                        mkU64(16),
                        foundIndex));
   put_vr_qw(v1, binop(Iop_64HLtoV128, mkexpr(result), mkU64(0)));


   if (s390_vr_is_cs_set(m5)) {
      static const IROp to64Converters[] = {Iop_8Uto64, Iop_16Uto64, Iop_32Uto64};
      IROp to64Converter = to64Converters[m4];

      IRExpr* arg1IsLessThanArg2 = binop(Iop_CmpLT64U,
                                         unop(to64Converter, mkexpr(arg1)),
                                         unop(to64Converter, mkexpr(arg2)));

      IRExpr* ccexp = mkite(binop(Iop_CmpEQ32,
                                  unop(Iop_1Uto32, mkexpr(isGoodPair)),
                                  mkU32(1)),
                            mkite(arg1IsLessThanArg2, mkU64(1), mkU64(2)),
                            mkU64(3));

      if(s390_vr_is_zs_set(m5)) {
         IRExpr* arg2IsZero = binop(comparator, mkexpr(arg2),
                                    unop(converter, mkU64(0)));
         IRExpr* bothArgsAreZero = binop(Iop_And32,
                                         unop(Iop_1Uto32, mkexpr(isZeroElem)),
                                         unop(Iop_1Uto32, arg2IsZero));
         ccexp = mkite(binop(Iop_CmpEQ32, bothArgsAreZero, mkU32(1)),
                       mkU64(0),
                       ccexp);
      }
      IRTemp cc = newTemp(Ity_I64);
      assign(cc, ccexp);

      s390_cc_set(cc);
   }


   put_counter_dw0(mkU64(0));
   return "vfene";
}

static const HChar *
s390_irgen_VISTR(UChar v1, UChar v2, UChar m3, UChar m5)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   /* Check for specification exception */
   vassert(m3 < 3);
   vassert((m5 & 0b1110) == 0);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VISTR;
   details.v1 = v1;
   details.v2 = v2;
   details.m4 = m3;
   details.m5 = m5;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 2;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Write;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));

   if (s390_vr_is_cs_set(m5)) {
      s390_cc_set(cc);
   }

   return "vistr";
}

static const HChar *
s390_irgen_VSTRC(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   /* Check for specification exception */
   vassert(m5 < 3);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VSTRC;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.v4 = v4;
   details.m4 = m5;
   details.m5 = m6;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 4;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);
   d->fxState[2].fx     = Ifx_Read;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v4 * sizeof(V128);
   d->fxState[2].size   = sizeof(V128);
   d->fxState[3].fx     = Ifx_Write;
   d->fxState[3].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[3].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));

   if (s390_vr_is_cs_set(m6)) {
      s390_cc_set(cc);
   }

   return "vstrc";
}

static const HChar *
s390_irgen_VNC(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, binop(Iop_AndV128,
             get_vr_qw(v2), unop(Iop_NotV128, get_vr_qw(v3)))
             );

   return "vnc";
}

static const HChar *
s390_irgen_VA(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Add8x16, Iop_Add16x8, Iop_Add32x4,
                        Iop_Add64x2, Iop_Add128x1 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "va";
}

static const HChar *
s390_irgen_VS(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Sub8x16, Iop_Sub16x8, Iop_Sub32x4,
                        Iop_Sub64x2, Iop_Sub128x1 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vs";
}

static const HChar *
s390_irgen_VMX(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Max8Sx16, Iop_Max16Sx8, Iop_Max32Sx4, Iop_Max64Sx2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmx";
}

static const HChar *
s390_irgen_VMXL(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Max8Ux16, Iop_Max16Ux8, Iop_Max32Ux4, Iop_Max64Ux2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmxl";
}

static const HChar *
s390_irgen_VMN(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Min8Sx16, Iop_Min16Sx8, Iop_Min32Sx4, Iop_Min64Sx2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmn";
}

static const HChar *
s390_irgen_VMNL(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Min8Ux16, Iop_Min16Ux8, Iop_Min32Ux4, Iop_Min64Ux2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmnl";
}

static const HChar *
s390_irgen_VAVG(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Avg8Sx16, Iop_Avg16Sx8, Iop_Avg32Sx4, Iop_Avg64Sx2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vavg";
}

static const HChar *
s390_irgen_VAVGL(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Avg8Ux16, Iop_Avg16Ux8, Iop_Avg32Ux4, Iop_Avg64Ux2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vavgl";
}

static const HChar *
s390_irgen_VLC(UChar v1, UChar v2, UChar m3)
{
   vassert(m3 < 4);
   IRType type = s390_vr_get_type(m3);
   put_vr_qw(v1, s390_V128_get_complement(get_vr_qw(v2), type));
   return "vlc";
}

static const HChar *
s390_irgen_VLP(UChar v1, UChar v2, UChar m3)
{
   const IROp ops[] = { Iop_Abs8x16, Iop_Abs16x8, Iop_Abs32x4, Iop_Abs64x2 };
   vassert(m3 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, unop(ops[m3], get_vr_qw(v2)));

   return "vlp";
}

static const HChar *
s390_irgen_VCH(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (!s390_vr_is_cs_set(m5)) {
      const IROp ops[] = { Iop_CmpGT8Sx16, Iop_CmpGT16Sx8, Iop_CmpGT32Sx4,
                           Iop_CmpGT64Sx2 };
      vassert(m4 < sizeof(ops) / sizeof(ops[0]));
      put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   } else {
      IRDirty* d;
      IRTemp cc = newTemp(Ity_I64);

      s390x_vec_op_details_t details = { .serialized = 0ULL };
      details.op = S390_VEC_OP_VCH;
      details.v1 = v1;
      details.v2 = v2;
      details.v3 = v3;
      details.m4 = m4;
      details.m5 = m5;

      d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                            &s390x_dirtyhelper_vec_op,
                            mkIRExprVec_2(IRExpr_GSPTR(),
                                          mkU64(details.serialized)));

      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx     = Ifx_Read;
      d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
      d->fxState[0].size   = sizeof(V128);
      d->fxState[1].fx     = Ifx_Read;
      d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
      d->fxState[1].size   = sizeof(V128);
      d->fxState[2].fx     = Ifx_Write;
      d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
      d->fxState[2].size   = sizeof(V128);

      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }

   return "vch";
}

static const HChar *
s390_irgen_VCHL(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (!s390_vr_is_cs_set(m5)) {
      const IROp ops[] = { Iop_CmpGT8Ux16, Iop_CmpGT16Ux8, Iop_CmpGT32Ux4,
                           Iop_CmpGT64Ux2 };
      vassert(m4 < sizeof(ops) / sizeof(ops[0]));
      put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   } else {
      IRDirty* d;
      IRTemp cc = newTemp(Ity_I64);

      s390x_vec_op_details_t details = { .serialized = 0ULL };
      details.op = S390_VEC_OP_VCHL;
      details.v1 = v1;
      details.v2 = v2;
      details.v3 = v3;
      details.m4 = m4;
      details.m5 = m5;

      d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                            &s390x_dirtyhelper_vec_op,
                            mkIRExprVec_2(IRExpr_GSPTR(),
                                          mkU64(details.serialized)));

      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx     = Ifx_Read;
      d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
      d->fxState[0].size   = sizeof(V128);
      d->fxState[1].fx     = Ifx_Read;
      d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
      d->fxState[1].size   = sizeof(V128);
      d->fxState[2].fx     = Ifx_Write;
      d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
      d->fxState[2].size   = sizeof(V128);

      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }

   return "vchl";
}

static const HChar *
s390_irgen_VCLZ(UChar v1, UChar v2, UChar m3)
{
   const IROp ops[] = { Iop_Clz8x16, Iop_Clz16x8, Iop_Clz32x4, Iop_Clz64x2 };
   vassert(m3 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, unop(ops[m3], get_vr_qw(v2)));

   return "vclz";
}

static const HChar *
s390_irgen_VCTZ(UChar v1, UChar v2, UChar m3)
{
   const IROp ops[] = { Iop_Ctz8x16, Iop_Ctz16x8, Iop_Ctz32x4, Iop_Ctz64x2 };
   vassert(m3 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, unop(ops[m3], get_vr_qw(v2)));

   return "vctz";
}

static const HChar *
s390_irgen_VPOPCT(UChar v1, UChar v2, UChar m3)
{
   vassert(m3 == 0);

   put_vr_qw(v1, unop(Iop_Cnt8x16, get_vr_qw(v2)));

   return "vpopct";
}

static const HChar *
s390_irgen_VML(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Mul8x16, Iop_Mul16x8, Iop_Mul32x4 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vml";
}

static const HChar *
s390_irgen_VMLH(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_MulHi8Ux16, Iop_MulHi16Ux8, Iop_MulHi32Ux4 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmlh";
}

static const HChar *
s390_irgen_VMH(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_MulHi8Sx16, Iop_MulHi16Sx8, Iop_MulHi32Sx4 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmh";
}

static const HChar *
s390_irgen_VME(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_MullEven8Sx16, Iop_MullEven16Sx8, Iop_MullEven32Sx4 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vme";
}

static const HChar *
s390_irgen_VMLE(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_MullEven8Ux16, Iop_MullEven16Ux8, Iop_MullEven32Ux4 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vmle";
}

static const HChar *
s390_irgen_VESLV(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Shl8x16, Iop_Shl16x8, Iop_Shl32x4, Iop_Shl64x2};
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "veslv";
}

static const HChar *
s390_irgen_VESL(UChar v1, IRTemp op2addr, UChar v3, UChar m4)
{
   IRExpr* shift_amount = unop(Iop_64to8, mkexpr(op2addr));
   const IROp ops[] = { Iop_ShlN8x16, Iop_ShlN16x8, Iop_ShlN32x4, Iop_ShlN64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v3), shift_amount));

   return "vesl";
}

static const HChar *
s390_irgen_VESRAV(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Sar8x16, Iop_Sar16x8, Iop_Sar32x4, Iop_Sar64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vesrav";
}

static const HChar *
s390_irgen_VESRA(UChar v1, IRTemp op2addr, UChar v3, UChar m4)
{
   IRExpr* shift_amount = unop(Iop_64to8, mkexpr(op2addr));
   const IROp ops[] = { Iop_SarN8x16, Iop_SarN16x8, Iop_SarN32x4, Iop_SarN64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v3), shift_amount));

   return "vesra";
}

static const HChar *
s390_irgen_VESRLV(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Shr8x16, Iop_Shr16x8, Iop_Shr32x4, Iop_Shr64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "vesrlv";
}

static const HChar *
s390_irgen_VESRL(UChar v1, IRTemp op2addr, UChar v3, UChar m4)
{
   IRExpr* shift_amount = unop(Iop_64to8, mkexpr(op2addr));
   const IROp ops[] = { Iop_ShrN8x16, Iop_ShrN16x8, Iop_ShrN32x4, Iop_ShrN64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v3), shift_amount));

   return "vesrl";
}

static const HChar *
s390_irgen_VERLLV(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_Rol8x16, Iop_Rol16x8, Iop_Rol32x4, Iop_Rol64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   return "verllv";
}

static const HChar *
s390_irgen_VERLL(UChar v1, IRTemp op2addr, UChar v3, UChar m4)
{
   /*
      There is no Iop_RolN?x?? operations
      so we have to use VECTOR x VECTOR variant.
    */
   IRExpr* shift_vector = unop(Iop_Dup8x16, unop(Iop_64to8, mkexpr(op2addr)));
   const IROp ops[] = { Iop_Rol8x16, Iop_Rol16x8, Iop_Rol32x4, Iop_Rol64x2 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v3), shift_vector));

   return "verll";
}

static const HChar *
s390_irgen_VSL(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b00000111)));

   put_vr_qw(v1, binop(Iop_ShlV128, get_vr_qw(v2), mkexpr(shift_amount)));
   return "vsl";
}

static const HChar *
s390_irgen_VSRL(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b00000111)));

   put_vr_qw(v1, binop(Iop_ShrV128, get_vr_qw(v2), mkexpr(shift_amount)));
   return "vsrl";
}

static const HChar *
s390_irgen_VSRA(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b00000111)));

   put_vr_qw(v1, binop(Iop_SarV128, get_vr_qw(v2), mkexpr(shift_amount)));
   return "vsra";
}

static const HChar *
s390_irgen_VERIM(UChar v1, UChar v2, UChar v3, UChar i4, UChar m5)
{
   /*
      There is no Iop_RolN?x?? operations
      so we have to use VECTOR x VECTOR variant.
    */
   const IROp ops[] = { Iop_Rol8x16, Iop_Rol16x8, Iop_Rol32x4, Iop_Rol64x2 };
   vassert(m5 < sizeof(ops) / sizeof(ops[0]));
   IRExpr* shift_vector = unop(Iop_Dup8x16, mkU8(i4));
   IRExpr* rotated_vector = binop(ops[m5], get_vr_qw(v2), shift_vector);

   /* result = (result & ~mask) | (rotated_vector & mask) */
   IRExpr* mask = get_vr_qw(v3);
   IRExpr* result = get_vr_qw(v1);
   put_vr_qw(v1, s390_V128_bitwiseITE(mask, rotated_vector, result));

   return "verim";
}

static const HChar *
s390_irgen_VEC(UChar v1, UChar v2, UChar m3)
{
   IRType type = s390_vr_get_type(m3);
   IRTemp op1 = newTemp(type);
   IRTemp op2 = newTemp(type);

   switch(type) {
   case Ity_I8:
      assign(op1, get_vr_b7(v1));
      assign(op2, get_vr_b7(v2));
      break;
   case Ity_I16:
      assign(op1, get_vr_hw3(v1));
      assign(op2, get_vr_hw3(v2));
      break;
   case Ity_I32:
      assign(op1, get_vr_w1(v1));
      assign(op2, get_vr_w1(v2));
      break;
   case Ity_I64:
      assign(op1, get_vr_dw0(v1));
      assign(op2, get_vr_dw0(v2));
      break;
   default:
      vpanic("s390_irgen_VEC: unknown type");
   }

   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);

   return "vec";
}

static const HChar *
s390_irgen_VECL(UChar v1, UChar v2, UChar m3)
{
   IRType type = s390_vr_get_type(m3);
   IRTemp op1 = newTemp(type);
   IRTemp op2 = newTemp(type);

   switch(type) {
   case Ity_I8:
      assign(op1, get_vr_b7(v1));
      assign(op2, get_vr_b7(v2));
      break;
   case Ity_I16:
      assign(op1, get_vr_hw3(v1));
      assign(op2, get_vr_hw3(v2));
      break;
   case Ity_I32:
      assign(op1, get_vr_w1(v1));
      assign(op2, get_vr_w1(v2));
      break;
   case Ity_I64:
      assign(op1, get_vr_dw0(v1));
      assign(op2, get_vr_dw0(v2));
      break;
   default:
      vpanic("s390_irgen_VECL: unknown type");
   }

   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);

   return "vecl";
}

static const HChar *
s390_irgen_VCEQ(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (!s390_vr_is_cs_set(m5)) {
      const IROp ops[] = { Iop_CmpEQ8x16, Iop_CmpEQ16x8, Iop_CmpEQ32x4,
                           Iop_CmpEQ64x2 };
      vassert(m4 < sizeof(ops) / sizeof(ops[0]));
      put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));

   } else {
      IRDirty* d;
      IRTemp cc = newTemp(Ity_I64);

      s390x_vec_op_details_t details = { .serialized = 0ULL };
      details.op = S390_VEC_OP_VCEQ;
      details.v1 = v1;
      details.v2 = v2;
      details.v3 = v3;
      details.m4 = m4;
      details.m5 = m5;

      d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                            &s390x_dirtyhelper_vec_op,
                            mkIRExprVec_2(IRExpr_GSPTR(),
                                          mkU64(details.serialized)));

      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx     = Ifx_Read;
      d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
      d->fxState[0].size   = sizeof(V128);
      d->fxState[1].fx     = Ifx_Read;
      d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
      d->fxState[1].size   = sizeof(V128);
      d->fxState[2].fx     = Ifx_Write;
      d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
      d->fxState[2].size   = sizeof(V128);

      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }

   return "vceq";
}

static const HChar *
s390_irgen_VSLB(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b01111000)));

   put_vr_qw(v1, binop(Iop_ShlV128, get_vr_qw(v2), mkexpr(shift_amount)));
   return "vslb";
}

static const HChar *
s390_irgen_VSRLB(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b01111000)));

   put_vr_qw(v1, binop(Iop_ShrV128, get_vr_qw(v2), mkexpr(shift_amount)));
   return "vsrlb";
}

static const HChar *
s390_irgen_VSRAB(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b01111000)));

   put_vr_qw(v1, binop(Iop_SarV128, get_vr_qw(v2), mkexpr(shift_amount)));
   return "vsrab";
}

static const HChar *
s390_irgen_VSLDB(UChar v1, UChar v2, UChar v3, UChar i4)
{
   UChar imm = i4 & 0b00001111;

   if (imm == 0) {
      /* Just copy v2. */
      put_vr_qw(v1, get_vr_qw(v2));
   } else {
      /* Concatenate v2's tail with v3's head. */
      put_vr_qw(v1,
                binop(Iop_OrV128,
                      binop(Iop_ShlV128, get_vr_qw(v2), mkU8(imm * 8)),
                      binop(Iop_ShrV128, get_vr_qw(v3), mkU8((16 - imm) * 8))
                     )
               );
   }

   return "vsldb";
}

static const HChar *
s390_irgen_VMO(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_MullEven8Sx16, Iop_MullEven16Sx8,
                        Iop_MullEven32Sx4 };
   UChar shifts[] = { 8, 16, 32 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   IRExpr* result = binop(ops[m4],
                          binop(Iop_ShlV128, get_vr_qw(v2), mkU8(shifts[m4])),
                          binop(Iop_ShlV128, get_vr_qw(v3), mkU8(shifts[m4]))
                         );
   put_vr_qw(v1, result);

   return "vmo";
}

static const HChar *
s390_irgen_VMLO(UChar v1, UChar v2, UChar v3, UChar m4)
{
   const IROp ops[] = { Iop_MullEven8Ux16, Iop_MullEven16Ux8,
                        Iop_MullEven32Ux4 };
   UChar shifts[] = { 8, 16, 32 };
   vassert(m4 < sizeof(ops) / sizeof(ops[0]));
   IRExpr* result = binop(ops[m4],
                          binop(Iop_ShlV128, get_vr_qw(v2), mkU8(shifts[m4])),
                          binop(Iop_ShlV128, get_vr_qw(v3), mkU8(shifts[m4]))
                         );
   put_vr_qw(v1, result);

   return "vmlo";
}

static const HChar *
s390_irgen_VMAE(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   const IROp mul_ops[] = { Iop_MullEven8Sx16, Iop_MullEven16Sx8,
                            Iop_MullEven32Sx4 };
   const IROp add_ops[] = { Iop_Add16x8, Iop_Add32x4, Iop_Add64x2};
   vassert(m5 < sizeof(mul_ops) / sizeof(mul_ops[0]));

   IRExpr* mul_result = binop(mul_ops[m5], get_vr_qw(v2), get_vr_qw(v3));
   IRExpr* result = binop(add_ops[m5], mul_result, get_vr_qw(v4));
   put_vr_qw(v1, result);

   return "vmae";
}

static const HChar *
s390_irgen_VMALE(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   const IROp mul_ops[] = { Iop_MullEven8Ux16, Iop_MullEven16Ux8,
                            Iop_MullEven32Ux4 };
   const IROp add_ops[] = { Iop_Add16x8, Iop_Add32x4, Iop_Add64x2 };
   vassert(m5 < sizeof(mul_ops) / sizeof(mul_ops[0]));

   IRExpr* mul_result = binop(mul_ops[m5], get_vr_qw(v2), get_vr_qw(v3));
   IRExpr* result = binop(add_ops[m5], mul_result, get_vr_qw(v4));
   put_vr_qw(v1, result);

   return "vmale";
}

static const HChar *
s390_irgen_VMAO(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   const IROp mul_ops[] = { Iop_MullEven8Sx16, Iop_MullEven16Sx8,
                            Iop_MullEven32Sx4 };
   const IROp add_ops[] = { Iop_Add16x8, Iop_Add32x4, Iop_Add64x2 };
   UChar shifts[] = { 8, 16, 32 };
   vassert(m5 < sizeof(mul_ops) / sizeof(mul_ops[0]));

   IRExpr* mul_result =
      binop(mul_ops[m5],
            binop(Iop_ShlV128, get_vr_qw(v2), mkU8(shifts[m5])),
            binop(Iop_ShlV128, get_vr_qw(v3), mkU8(shifts[m5])));
   IRExpr* result = binop(add_ops[m5], mul_result, get_vr_qw(v4));
   put_vr_qw(v1, result);

   return "vmao";
}

static const HChar *
s390_irgen_VMALO(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   const IROp mul_ops[] = { Iop_MullEven8Ux16, Iop_MullEven16Ux8,
                            Iop_MullEven32Ux4 };
   const IROp add_ops[] = { Iop_Add16x8, Iop_Add32x4, Iop_Add64x2 };
   UChar shifts[] = { 8, 16, 32 };
   vassert(m5 < sizeof(mul_ops) / sizeof(mul_ops[0]));

   IRExpr* mul_result = binop(mul_ops[m5],
                              binop(Iop_ShlV128,
                                    get_vr_qw(v2), mkU8(shifts[m5])),
                              binop(Iop_ShlV128,
                                    get_vr_qw(v3), mkU8(shifts[m5]))
   );

   IRExpr* result = binop(add_ops[m5], mul_result, get_vr_qw(v4));
   put_vr_qw(v1, result);

   return "vmalo";
}

static const HChar *
s390_irgen_VMAL(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   const IROp mul_ops[] = { Iop_Mul8x16, Iop_Mul16x8, Iop_Mul32x4 };
   const IROp add_ops[] = { Iop_Add8x16, Iop_Add16x8, Iop_Add32x4 };
   vassert(m5 < sizeof(mul_ops) / sizeof(mul_ops[0]));

   IRExpr* mul_result = binop(mul_ops[m5], get_vr_qw(v2), get_vr_qw(v3));
   IRExpr* result = binop(add_ops[m5], mul_result, get_vr_qw(v4));
   put_vr_qw(v1, result);

   return "vmal";
}

static const HChar *
s390_irgen_VSUM(UChar v1, UChar v2, UChar v3, UChar m4)
{
   IRType type = s390_vr_get_type(m4);
   IRExpr* mask;
   IRExpr* sum;
   switch(type) {
   case Ity_I8:
      sum = unop(Iop_PwAddL16Ux8, unop(Iop_PwAddL8Ux16, get_vr_qw(v2)));
      mask = IRExpr_Const(IRConst_V128(0b0001000100010001));
      break;
   case Ity_I16:
      sum = unop(Iop_PwAddL16Ux8, get_vr_qw(v2));
      mask = IRExpr_Const(IRConst_V128(0b0011001100110011));
      break;
   default:
      vpanic("s390_irgen_VSUM: invalid type ");
   }

   IRExpr* addition = binop(Iop_AndV128, get_vr_qw(v3), mask);
   put_vr_qw(v1, binop(Iop_Add32x4, sum, addition));

   return "vsum";
}

static const HChar *
s390_irgen_VSUMG(UChar v1, UChar v2, UChar v3, UChar m4)
{
   IRType type = s390_vr_get_type(m4);
   IRExpr* mask;
   IRExpr* sum;
   switch(type) {
   case Ity_I16:
      sum = unop(Iop_PwAddL32Ux4, unop(Iop_PwAddL16Ux8, get_vr_qw(v2)));
      mask = IRExpr_Const(IRConst_V128(0b0000001100000011));
      break;
   case Ity_I32:
      sum = unop(Iop_PwAddL32Ux4, get_vr_qw(v2));
      mask = IRExpr_Const(IRConst_V128(0b0000111100001111));
      break;
   default:
      vpanic("s390_irgen_VSUMG: invalid type ");
   }

   IRExpr* addition = binop(Iop_AndV128, get_vr_qw(v3), mask);
   put_vr_qw(v1, binop(Iop_Add64x2, sum, addition));

   return "vsumg";
}

static const HChar *
s390_irgen_VSUMQ(UChar v1, UChar v2, UChar v3, UChar m4)
{
   IRType type = s390_vr_get_type(m4);
   IRExpr* mask;
   IRExpr* sum;
   switch(type) {
   case Ity_I32:
      sum = unop(Iop_PwAddL64Ux2, unop(Iop_PwAddL32Ux4, get_vr_qw(v2)));
      mask = IRExpr_Const(IRConst_V128(0b0000000000001111));
      break;
   case Ity_I64:
      sum = unop(Iop_PwAddL64Ux2, get_vr_qw(v2));
      mask = IRExpr_Const(IRConst_V128(0b0000000011111111));
      break;
   default:
      vpanic("s390_irgen_VSUMQ: invalid type ");
   }

   IRExpr* addition = binop(Iop_AndV128, get_vr_qw(v3), mask);
   put_vr_qw(v1, binop(Iop_Add128x1, sum, addition));

   return "vsumq";
}

static const HChar *
s390_irgen_VTM(UChar v1, UChar v2)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VTM;
   details.v2 = v1;
   details.v3 = v2;
   details.read_only = 1;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 2;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));
   s390_cc_set(cc);

   return "vtm";
}

static const HChar *
s390_irgen_VAC(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   vassert(m5 == 4); /* specification exception otherwise */

   IRTemp sum = newTemp(Ity_V128);
   assign(sum, binop(Iop_Add128x1, get_vr_qw(v2), get_vr_qw(v3)));

   IRExpr* mask = binop(Iop_64HLtoV128, mkU64(0), mkU64(1));
   IRExpr* carry_in = binop(Iop_AndV128, get_vr_qw(v4), mask);
   put_vr_qw(v1, binop(Iop_Add128x1, mkexpr(sum), carry_in));

   return "vac";
}

static const HChar *
s390_irgen_VACC(UChar v1, UChar v2, UChar v3, UChar m4)
{
   IRType type = s390_vr_get_type(m4);
   IRExpr* arg1 = get_vr_qw(v2);
   IRExpr* arg2 = get_vr_qw(v3);

   put_vr_qw(v1, s390_V128_calculate_carry_out(arg1, arg2, type, False));
   return "vacc";
}

static const HChar *
s390_irgen_VACCC(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   vassert(m5 == 4); /* specification exception otherwise */
   IRExpr* result =
         s390_V128_calculate_carry_out_with_carry(get_vr_qw(v2),
                                                  get_vr_qw(v3),
                                                  get_vr_qw(v4)
                                                  );

   put_vr_qw(v1, result);
   return "vaccc";
}

static const HChar*
s390_irgen_VCKSM(UChar v1, UChar v2, UChar v3)
{

   IRTemp sum1 = s390_checksum_add(get_vr_w1(v3), get_vr_w0(v2));
   IRTemp sum2 = s390_checksum_add(mkexpr(sum1), get_vr_w1(v2));
   IRTemp sum3 = s390_checksum_add(mkexpr(sum2), get_vr_w2(v2));
   IRTemp result = s390_checksum_add(mkexpr(sum3), get_vr_w3(v2));

   put_vr_qw(v1, binop(Iop_64HLtoV128,
                       unop(Iop_32Uto64, mkexpr(result)), mkU64(0ULL)));

   return "vcksm";
}

static const HChar *
s390_irgen_VGFM(UChar v1, UChar v2, UChar v3, UChar m4)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VGFM;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.m4 = m4;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 3;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);
   d->fxState[2].fx     = Ifx_Write;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[2].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));
   return "vgfm";
}

static const HChar *
s390_irgen_VGFMA(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VGFMA;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.v4 = v4;
   details.m4 = m5;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 4;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);
   d->fxState[2].fx     = Ifx_Read;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v4 * sizeof(V128);
   d->fxState[2].size   = sizeof(V128);
   d->fxState[3].fx     = Ifx_Write;
   d->fxState[3].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[3].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));
   return "vgfma";
}

static const HChar *
s390_irgen_VSBI(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   vassert(m5 == 4); /* specification exception otherwise */

   IRExpr* mask = binop(Iop_64HLtoV128, mkU64(0ULL), mkU64(1ULL));
   IRExpr* carry_in = binop(Iop_AndV128, get_vr_qw(v4), mask);

   IRTemp sum = newTemp(Ity_V128);
   assign(sum, binop(Iop_Add128x1,
                     get_vr_qw(v2),
                     unop(Iop_NotV128, get_vr_qw(v3))
                     )
         );

   put_vr_qw(v1, binop(Iop_Add128x1, mkexpr(sum), carry_in));
   return "vsbi";
}

static const HChar *
s390_irgen_VSCBI(UChar v1, UChar v2, UChar v3, UChar m4)
{
   IRType type = s390_vr_get_type(m4);
   IRExpr* arg1 = get_vr_qw(v2);
   IRExpr* arg2 = s390_V128_get_complement(get_vr_qw(v3), type);
   IRExpr* result = s390_V128_calculate_carry_out(arg1, arg2, type, True);

   put_vr_qw(v1, result);
   return "vscbi";
}

static const HChar *
s390_irgen_VSBCBI(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   vassert(m5 == 4); /* specification exception otherwise */
   IRExpr* result =
      s390_V128_calculate_carry_out_with_carry(get_vr_qw(v2),
                                               unop(Iop_NotV128, get_vr_qw(v3)),
                                               get_vr_qw(v4));

   put_vr_qw(v1, result);
   return "vsbcbi";
}

static const HChar *
s390_irgen_VMAH(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   /* Check for specification exception */
   vassert(m5 < 3);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VMAH;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.v4 = v4;
   details.m4 = m5;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 4;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);
   d->fxState[2].fx     = Ifx_Read;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v4 * sizeof(V128);
   d->fxState[2].size   = sizeof(V128);
   d->fxState[3].fx     = Ifx_Write;
   d->fxState[3].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[3].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));

   return "vmah";
}

static const HChar *
s390_irgen_VMALH(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   /* Check for specification exception */
   vassert(m5 < 3);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VMALH;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.v4 = v4;
   details.m4 = m5;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 4;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx     = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size   = sizeof(V128);
   d->fxState[1].fx     = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size   = sizeof(V128);
   d->fxState[2].fx     = Ifx_Read;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v4 * sizeof(V128);
   d->fxState[2].size   = sizeof(V128);
   d->fxState[3].fx     = Ifx_Write;
   d->fxState[3].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[3].size   = sizeof(V128);

   stmt(IRStmt_Dirty(d));

   return "vmalh";
}

static void
s390_vector_fp_convert(IROp op, IRType fromType, IRType toType,
                       UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m4);
   UChar maxIndex = isSingleElementOp ? 0 : 1;

   /* For Iop_F32toF64 we do this:
      f32[0] -> f64[0]
      f32[2] -> f64[1]

      For Iop_F64toF32 we do this:
      f64[0] -> f32[0]
      f64[1] -> f32[2]

      The magic below with scaling factors is used to achieve the logic
      described above.
   */
   const UChar sourceIndexScaleFactor = (op == Iop_F32toF64) ? 2 : 1;
   const UChar destinationIndexScaleFactor = (op == Iop_F64toF32) ? 2 : 1;

   const Bool isUnary = (op == Iop_F32toF64);
   for (UChar i = 0; i <= maxIndex; i++) {
      IRExpr* argument = get_vr(v2, fromType, i * sourceIndexScaleFactor);
      IRExpr* result;
      if (!isUnary) {
         result = binop(op,
                        mkexpr(encode_bfp_rounding_mode(m5)),
                        argument);
      } else {
         result = unop(op, argument);
      }
      put_vr(v1, toType, i * destinationIndexScaleFactor, result);
   }

   if (isSingleElementOp) {
      put_vr_dw1(v1, mkU64(0));
   }
}

static const HChar *
s390_irgen_VCDG(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert("vcdg", m3 == 3);

   if (!s390_host_has_fpext && m5 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m5 = S390_BFP_ROUND_PER_FPC;
   }

   s390_vector_fp_convert(Iop_I64StoF64, Ity_I64, Ity_F64, v1, v2, m3, m4, m5);

   return "vcdg";
}

static const HChar *
s390_irgen_VCDLG(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert("vcdlg", m3 == 3);

   if (!s390_host_has_fpext && m5 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m5 = S390_BFP_ROUND_PER_FPC;
   }

   s390_vector_fp_convert(Iop_I64UtoF64, Ity_I64, Ity_F64, v1, v2, m3, m4, m5);

   return "vcdlg";
}

static const HChar *
s390_irgen_VCGD(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert("vcgd", m3 == 3);

   if (!s390_host_has_fpext && m5 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m5 = S390_BFP_ROUND_PER_FPC;
   }

   s390_vector_fp_convert(Iop_F64toI64S, Ity_F64, Ity_I64, v1, v2, m3, m4, m5);

   return "vcgd";
}

static const HChar *
s390_irgen_VCLGD(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert("vclgd", m3 == 3);

   if (!s390_host_has_fpext && m5 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m5 = S390_BFP_ROUND_PER_FPC;
   }

   s390_vector_fp_convert(Iop_F64toI64U, Ity_F64, Ity_I64, v1, v2, m3, m4, m5);

   return "vclgd";
}

static const HChar *
s390_irgen_VFI(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert("vfi", m3 == 3);

   if (!s390_host_has_fpext && m5 != S390_BFP_ROUND_PER_FPC) {
      emulation_warning(EmWarn_S390X_fpext_rounding);
      m5 = S390_BFP_ROUND_PER_FPC;
   }

   s390_vector_fp_convert(Iop_RoundF64toInt, Ity_F64, Ity_F64,
                          v1, v2, m3, m4, m5);

   return "vcgld";
}

static const HChar *
s390_irgen_VLDE(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert("vlde", m3 == 2);

   s390_vector_fp_convert(Iop_F32toF64, Ity_F32, Ity_F64, v1, v2, m3, m4, m5);

   return "vlde";
}

static const HChar *
s390_irgen_VLED(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert("vled", m3 == 3);

   if (!s390_host_has_fpext && m5 != S390_BFP_ROUND_PER_FPC) {
      m5 = S390_BFP_ROUND_PER_FPC;
   }

   s390_vector_fp_convert(Iop_F64toF32, Ity_F64, Ity_F32, v1, v2, m3, m4, m5);

   return "vled";
}

static const HChar *
s390_irgen_VFPSO(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert("vfpso", m3 == 3);

   IRExpr* result;
   switch (m5) {
   case 0: {
      /* Invert sign */
      if (!s390_vr_is_single_element_control_set(m4)) {
         result = unop(Iop_Neg64Fx2, get_vr_qw(v2));
      }
      else {
         result = binop(Iop_64HLtoV128,
                        unop(Iop_ReinterpF64asI64,
                             unop(Iop_NegF64, get_vr(v2, Ity_F64, 0))),
                        mkU64(0));
      }
      break;
   }

   case 1: {
      /* Set sign to negative */
      IRExpr* highHalf = mkU64(0x8000000000000000ULL);
      if (!s390_vr_is_single_element_control_set(m4)) {
         IRExpr* lowHalf = highHalf;
         IRExpr* mask = binop(Iop_64HLtoV128, highHalf, lowHalf);
         result = binop(Iop_OrV128, get_vr_qw(v2), mask);
      }
      else {
         result = binop(Iop_64HLtoV128,
                        binop(Iop_Or64, get_vr_dw0(v2), highHalf),
                        mkU64(0ULL));
      }

      break;
   }

   case 2: {
      /* Set sign to positive */
      if (!s390_vr_is_single_element_control_set(m4)) {
         result = unop(Iop_Abs64Fx2, get_vr_qw(v2));
      }
      else {
         result = binop(Iop_64HLtoV128,
                        unop(Iop_ReinterpF64asI64,
                             unop(Iop_AbsF64, get_vr(v2, Ity_F64, 0))),
                        mkU64(0));
      }

      break;
   }

   default:
      vpanic("s390_irgen_VFPSO: Invalid m5 value");
   }

   put_vr_qw(v1, result);
   if (s390_vr_is_single_element_control_set(m4)) {
      put_vr_dw1(v1, mkU64(0ULL));
   }

   return "vfpso";
}

static void s390x_vec_fp_binary_op(IROp generalOp, IROp singleElementOp,
                                   UChar v1, UChar v2, UChar v3, UChar m4,
                                   UChar m5)
{
   IRExpr* result;
   if (!s390_vr_is_single_element_control_set(m5)) {
      result = triop(generalOp, get_bfp_rounding_mode_from_fpc(),
                     get_vr_qw(v2), get_vr_qw(v3));
   } else {
      IRExpr* highHalf = triop(singleElementOp,
                               get_bfp_rounding_mode_from_fpc(),
                               get_vr(v2, Ity_F64, 0),
                               get_vr(v3, Ity_F64, 0));
      result = binop(Iop_64HLtoV128, unop(Iop_ReinterpF64asI64, highHalf),
                     mkU64(0ULL));
   }

   put_vr_qw(v1, result);
}

static void s390x_vec_fp_unary_op(IROp generalOp, IROp singleElementOp,
                                  UChar v1, UChar v2, UChar m3, UChar m4)
{
   IRExpr* result;
   if (!s390_vr_is_single_element_control_set(m4)) {
      result = binop(generalOp, get_bfp_rounding_mode_from_fpc(),
                     get_vr_qw(v2));
   }
   else {
      IRExpr* highHalf = binop(singleElementOp,
                               get_bfp_rounding_mode_from_fpc(),
                               get_vr(v2, Ity_F64, 0));
      result = binop(Iop_64HLtoV128, unop(Iop_ReinterpF64asI64, highHalf),
                     mkU64(0ULL));
   }

   put_vr_qw(v1, result);
}


static void
s390_vector_fp_mulAddOrSub(IROp singleElementOp,
                           UChar v1, UChar v2, UChar v3, UChar v4,
                           UChar m5, UChar m6)
{
   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m5);
   IRTemp irrm_temp = newTemp(Ity_I32);
   assign(irrm_temp, get_bfp_rounding_mode_from_fpc());
   IRExpr* irrm = mkexpr(irrm_temp);
   IRExpr* result;
   IRExpr* highHalf = qop(singleElementOp,
                          irrm,
                          get_vr(v2, Ity_F64, 0),
                          get_vr(v3, Ity_F64, 0),
                          get_vr(v4, Ity_F64, 0));

   if (isSingleElementOp) {
      result = binop(Iop_64HLtoV128, unop(Iop_ReinterpF64asI64, highHalf),
                     mkU64(0ULL));
   } else {
      IRExpr* lowHalf = qop(singleElementOp,
                            irrm,
                            get_vr(v2, Ity_F64, 1),
                            get_vr(v3, Ity_F64, 1),
                            get_vr(v4, Ity_F64, 1));
      result = binop(Iop_64HLtoV128, unop(Iop_ReinterpF64asI64, highHalf),
                     unop(Iop_ReinterpF64asI64, lowHalf));
   }

   put_vr_qw(v1, result);
}

static const HChar *
s390_irgen_VFA(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert("vfa", m4 == 3);
   s390x_vec_fp_binary_op(Iop_Add64Fx2, Iop_AddF64, v1, v2, v3, m4, m5);
   return "vfa";
}

static const HChar *
s390_irgen_VFS(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert("vfs", m4 == 3);
   s390x_vec_fp_binary_op(Iop_Sub64Fx2, Iop_SubF64, v1, v2, v3, m4, m5);
   return "vfs";
}

static const HChar *
s390_irgen_VFM(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert("vfm", m4 == 3);
   s390x_vec_fp_binary_op(Iop_Mul64Fx2, Iop_MulF64, v1, v2, v3, m4, m5);
   return "vfm";
}

static const HChar *
s390_irgen_VFD(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert("vfd", m4 == 3);
   s390x_vec_fp_binary_op(Iop_Div64Fx2, Iop_DivF64, v1, v2, v3, m4, m5);
   return "vfd";
}

static const HChar *
s390_irgen_VFSQ(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390_insn_assert("vfsq", m3 == 3);
   s390x_vec_fp_unary_op(Iop_Sqrt64Fx2, Iop_SqrtF64, v1, v2, m3, m4);

   return "vfsq";
}

static const HChar *
s390_irgen_VFMA(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   s390_insn_assert("vfma", m6 == 3);
   s390_vector_fp_mulAddOrSub(Iop_MAddF64, v1, v2, v3, v4, m5, m6);
   return "vfma";
}

static const HChar *
s390_irgen_VFMS(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   s390_insn_assert("vfms", m6 == 3);
   s390_vector_fp_mulAddOrSub(Iop_MSubF64, v1, v2, v3, v4, m5, m6);
   return "vfms";
}

static const HChar *
s390_irgen_WFC(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390_insn_assert("wfc", m3 == 3);
   s390_insn_assert("wfc", m4 == 0);

   IRTemp cc_vex = newTemp(Ity_I32);
   assign(cc_vex, binop(Iop_CmpF64,
                        get_vr(v1, Ity_F64, 0), get_vr(v2, Ity_F64, 0)));

   IRTemp cc_s390 = newTemp(Ity_I32);
   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);

   return "wfc";
}

static const HChar *
s390_irgen_WFK(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390_irgen_WFC(v1, v2, m3, m4);

   return "wfk";
}

static const HChar *
s390_irgen_VFCE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6)
{
   s390_insn_assert("vfce", m4 == 3);

   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m5);
   if (!s390_vr_is_cs_set(m6)) {
      if (!isSingleElementOp) {
         put_vr_qw(v1, binop(Iop_CmpEQ64Fx2, get_vr_qw(v2), get_vr_qw(v3)));
      } else {
         IRExpr* comparisonResult = binop(Iop_CmpF64, get_vr(v2, Ity_F64, 0),
                                          get_vr(v3, Ity_F64, 0));
         IRExpr* result = mkite(binop(Iop_CmpEQ32, comparisonResult,
                                      mkU32(Ircr_EQ)),
                                mkU64(0xffffffffffffffffULL),
                                mkU64(0ULL));
         put_vr_qw(v1, binop(Iop_64HLtoV128, result, mkU64(0ULL)));
      }
   } else {
      IRDirty* d;
      IRTemp cc = newTemp(Ity_I64);

      s390x_vec_op_details_t details = { .serialized = 0ULL };
      details.op = S390_VEC_OP_VFCE;
      details.v1 = v1;
      details.v2 = v2;
      details.v3 = v3;
      details.m4 = m4;
      details.m5 = m5;
      details.m6 = m6;

      d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                            &s390x_dirtyhelper_vec_op,
                            mkIRExprVec_2(IRExpr_GSPTR(),
                                          mkU64(details.serialized)));

      const UChar elementSize = isSingleElementOp ? sizeof(ULong) : sizeof(V128);
      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx = Ifx_Read;
      d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
      d->fxState[0].size = elementSize;
      d->fxState[1].fx = Ifx_Read;
      d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
      d->fxState[1].size = elementSize;
      d->fxState[2].fx = Ifx_Write;
      d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
      d->fxState[2].size = sizeof(V128);

      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }

   return "vfce";
}

static const HChar *
s390_irgen_VFCH(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6)
{
   vassert(m4 == 3);

   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m5);
   if (!s390_vr_is_cs_set(m6)) {
      if (!isSingleElementOp) {
         put_vr_qw(v1, binop(Iop_CmpLE64Fx2, get_vr_qw(v3), get_vr_qw(v2)));
      } else {
         IRExpr* comparisonResult = binop(Iop_CmpF64, get_vr(v2, Ity_F64, 0),
                                          get_vr(v3, Ity_F64, 0));
         IRExpr* result = mkite(binop(Iop_CmpEQ32, comparisonResult,
                                      mkU32(Ircr_GT)),
                                mkU64(0xffffffffffffffffULL),
                                mkU64(0ULL));
         put_vr_qw(v1, binop(Iop_64HLtoV128, result, mkU64(0ULL)));
      }
   }
   else {
      IRDirty* d;
      IRTemp cc = newTemp(Ity_I64);

      s390x_vec_op_details_t details = { .serialized = 0ULL };
      details.op = S390_VEC_OP_VFCH;
      details.v1 = v1;
      details.v2 = v2;
      details.v3 = v3;
      details.m4 = m4;
      details.m5 = m5;
      details.m6 = m6;

      d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                            &s390x_dirtyhelper_vec_op,
                            mkIRExprVec_2(IRExpr_GSPTR(),
                                          mkU64(details.serialized)));

      const UChar elementSize = isSingleElementOp ? sizeof(ULong) : sizeof(V128);
      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx = Ifx_Read;
      d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
      d->fxState[0].size = elementSize;
      d->fxState[1].fx = Ifx_Read;
      d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
      d->fxState[1].size = elementSize;
      d->fxState[2].fx = Ifx_Write;
      d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
      d->fxState[2].size = sizeof(V128);

      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }

   return "vfch";
}

static const HChar *
s390_irgen_VFCHE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6)
{
   s390_insn_assert("vfche", m4 == 3);

   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m5);
   if (!s390_vr_is_cs_set(m6)) {
      if (!isSingleElementOp) {
         put_vr_qw(v1, binop(Iop_CmpLT64Fx2, get_vr_qw(v3), get_vr_qw(v2)));
      }
      else {
         IRExpr* comparisonResult = binop(Iop_CmpF64, get_vr(v3, Ity_F64, 0),
                                          get_vr(v2, Ity_F64, 0));
         IRExpr* result = mkite(binop(Iop_CmpEQ32, comparisonResult,
                                      mkU32(Ircr_LT)),
                                mkU64(0xffffffffffffffffULL),
                                mkU64(0ULL));
         put_vr_qw(v1, binop(Iop_64HLtoV128, result, mkU64(0ULL)));
      }
   }
   else {
      IRDirty* d;
      IRTemp cc = newTemp(Ity_I64);

      s390x_vec_op_details_t details = { .serialized = 0ULL };
      details.op = S390_VEC_OP_VFCHE;
      details.v1 = v1;
      details.v2 = v2;
      details.v3 = v3;
      details.m4 = m4;
      details.m5 = m5;
      details.m6 = m6;

      d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                            &s390x_dirtyhelper_vec_op,
                            mkIRExprVec_2(IRExpr_GSPTR(),
                                          mkU64(details.serialized)));

      const UChar elementSize = isSingleElementOp ? sizeof(ULong) : sizeof(V128);
      d->nFxState = 3;
      vex_bzero(&d->fxState, sizeof(d->fxState));
      d->fxState[0].fx = Ifx_Read;
      d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
      d->fxState[0].size = elementSize;
      d->fxState[1].fx = Ifx_Read;
      d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
      d->fxState[1].size = elementSize;
      d->fxState[2].fx = Ifx_Write;
      d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
      d->fxState[2].size = sizeof(V128);

      stmt(IRStmt_Dirty(d));
      s390_cc_set(cc);
   }

   return "vfche";
}

static const HChar *
s390_irgen_VFTCI(UChar v1, UChar v2, UShort i3, UChar m4, UChar m5)
{
   s390_insn_assert("vftci", m4 == 3);

   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m5);

   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VFTCI;
   details.v1 = v1;
   details.v2 = v2;
   details.i3 = i3;
   details.m4 = m4;
   details.m5 = m5;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   const UChar elementSize = isSingleElementOp ? sizeof(ULong) : sizeof(V128);
   d->nFxState = 2;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size = elementSize;
   d->fxState[1].fx = Ifx_Write;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[1].size = sizeof(V128);

   stmt(IRStmt_Dirty(d));
   s390_cc_set(cc);

   return "vftci";
}

/* New insns are added here.
   If an insn is contingent on a facility being installed also
   check whether the list of supported facilities in function
   s390x_dirtyhelper_STFLE needs updating */

/*------------------------------------------------------------*/
/*--- Build IR for special instructions                    ---*/
/*------------------------------------------------------------*/

static void
s390_irgen_client_request(void)
{
   if (0)
      vex_printf("%%R3 = client_request ( %%R2 )\n");

   Addr64 next = guest_IA_curr_instr + S390_SPECIAL_OP_PREAMBLE_SIZE
                                     + S390_SPECIAL_OP_SIZE;

   dis_res->jk_StopHere = Ijk_ClientReq;
   dis_res->whatNext = Dis_StopHere;

   put_IA(mkaddr_expr(next));
}

static void
s390_irgen_guest_NRADDR(void)
{
   if (0)
      vex_printf("%%R3 = guest_NRADDR\n");

   put_gpr_dw0(3, IRExpr_Get(S390X_GUEST_OFFSET(guest_NRADDR), Ity_I64));
}

static void
s390_irgen_call_noredir(void)
{
   Addr64 next = guest_IA_curr_instr + S390_SPECIAL_OP_PREAMBLE_SIZE
                                     + S390_SPECIAL_OP_SIZE;

   /* Continue after special op */
   put_gpr_dw0(14, mkaddr_expr(next));

   /* The address is in REG1, all parameters are in the right (guest) places */
   put_IA(get_gpr_dw0(1));

   dis_res->whatNext = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_NoRedir;
}

static s390_decode_t
s390_decode_2byte_and_irgen(const UChar *bytes)
{
   UShort ovl = ((UShort)bytes[0] << 8) | (UShort)bytes[1];

   switch (ovl & 0xffff) {
   case 0x0101: /* PR */ goto unimplemented;
   case 0x0102: /* UPT */ goto unimplemented;
   case 0x0104: /* PTFF */ goto unimplemented;
   case 0x0107: /* SCKPF */ goto unimplemented;
   case 0x010a: s390_format_E(s390_irgen_PFPO); goto ok;
   case 0x010b: /* TAM */ goto unimplemented;
   case 0x010c: /* SAM24 */ goto unimplemented;
   case 0x010d: /* SAM31 */ goto unimplemented;
   case 0x010e: /* SAM64 */ goto unimplemented;
   case 0x01ff: /* TRAP2 */ goto unimplemented;
   }

   switch ((ovl & 0xff00) >> 8) {
   case 0x04: /* SPM */ goto unimplemented;
   case 0x05: /* BALR */ goto unimplemented;
   case 0x06: s390_format_RR_RR(s390_irgen_BCTR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x07: s390_format_RR(s390_irgen_BCR, RR_r1(ovl), RR_r2(ovl));
                             goto ok;
   case 0x0a: s390_format_I(s390_irgen_SVC, I_i(ovl));  goto ok;
   case 0x0b: /* BSM */ goto unimplemented;
   case 0x0c: /* BASSM */ goto unimplemented;
   case 0x0d: s390_format_RR_RR(s390_irgen_BASR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x0e: s390_format_RR(s390_irgen_MVCL, RR_r1(ovl), RR_r2(ovl));
                             goto ok;
   case 0x0f: s390_format_RR(s390_irgen_CLCL, RR_r1(ovl), RR_r2(ovl));
                             goto ok;
   case 0x10: s390_format_RR_RR(s390_irgen_LPR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x11: s390_format_RR_RR(s390_irgen_LNR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x12: s390_format_RR_RR(s390_irgen_LTR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x13: s390_format_RR_RR(s390_irgen_LCR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x14: s390_format_RR_RR(s390_irgen_NR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x15: s390_format_RR_RR(s390_irgen_CLR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x16: s390_format_RR_RR(s390_irgen_OR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x17: s390_format_RR_RR(s390_irgen_XR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x18: s390_format_RR_RR(s390_irgen_LR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x19: s390_format_RR_RR(s390_irgen_CR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x1a: s390_format_RR_RR(s390_irgen_AR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x1b: s390_format_RR_RR(s390_irgen_SR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x1c: s390_format_RR_RR(s390_irgen_MR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x1d: s390_format_RR_RR(s390_irgen_DR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x1e: s390_format_RR_RR(s390_irgen_ALR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x1f: s390_format_RR_RR(s390_irgen_SLR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x20: /* LPDR */ goto unimplemented;
   case 0x21: /* LNDR */ goto unimplemented;
   case 0x22: /* LTDR */ goto unimplemented;
   case 0x23: /* LCDR */ goto unimplemented;
   case 0x24: /* HDR */ goto unimplemented;
   case 0x25: /* LDXR */ goto unimplemented;
   case 0x26: /* MXR */ goto unimplemented;
   case 0x27: /* MXDR */ goto unimplemented;
   case 0x28: s390_format_RR_FF(s390_irgen_LDR, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x29: /* CDR */ goto unimplemented;
   case 0x2a: /* ADR */ goto unimplemented;
   case 0x2b: /* SDR */ goto unimplemented;
   case 0x2c: /* MDR */ goto unimplemented;
   case 0x2d: /* DDR */ goto unimplemented;
   case 0x2e: /* AWR */ goto unimplemented;
   case 0x2f: /* SWR */ goto unimplemented;
   case 0x30: /* LPER */ goto unimplemented;
   case 0x31: /* LNER */ goto unimplemented;
   case 0x32: /* LTER */ goto unimplemented;
   case 0x33: /* LCER */ goto unimplemented;
   case 0x34: /* HER */ goto unimplemented;
   case 0x35: /* LEDR */ goto unimplemented;
   case 0x36: /* AXR */ goto unimplemented;
   case 0x37: /* SXR */ goto unimplemented;
   case 0x38: s390_format_RR_FF(s390_irgen_LER, RR_r1(ovl), RR_r2(ovl));
                                goto ok;
   case 0x39: /* CER */ goto unimplemented;
   case 0x3a: /* AER */ goto unimplemented;
   case 0x3b: /* SER */ goto unimplemented;
   case 0x3c: /* MDER */ goto unimplemented;
   case 0x3d: /* DER */ goto unimplemented;
   case 0x3e: /* AUR */ goto unimplemented;
   case 0x3f: /* SUR */ goto unimplemented;
   }

   return S390_DECODE_UNKNOWN_INSN;

ok:
   return S390_DECODE_OK;

unimplemented:
   return S390_DECODE_UNIMPLEMENTED_INSN;
}

static s390_decode_t
s390_decode_4byte_and_irgen(const UChar *bytes)
{
   UInt ovl = ((UInt)bytes[0] << 24) | ((UInt)bytes[1] << 16) |
              ((UInt)bytes[2] << 8) | (UInt)bytes[3];

   switch ((ovl & 0xff0f0000) >> 16) {
   case 0xa500: s390_format_RI_RU(s390_irgen_IIHH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa501: s390_format_RI_RU(s390_irgen_IIHL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa502: s390_format_RI_RU(s390_irgen_IILH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa503: s390_format_RI_RU(s390_irgen_IILL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa504: s390_format_RI_RU(s390_irgen_NIHH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa505: s390_format_RI_RU(s390_irgen_NIHL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa506: s390_format_RI_RU(s390_irgen_NILH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa507: s390_format_RI_RU(s390_irgen_NILL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa508: s390_format_RI_RU(s390_irgen_OIHH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa509: s390_format_RI_RU(s390_irgen_OIHL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa50a: s390_format_RI_RU(s390_irgen_OILH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa50b: s390_format_RI_RU(s390_irgen_OILL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa50c: s390_format_RI_RU(s390_irgen_LLIHH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa50d: s390_format_RI_RU(s390_irgen_LLIHL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa50e: s390_format_RI_RU(s390_irgen_LLILH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa50f: s390_format_RI_RU(s390_irgen_LLILL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa700: s390_format_RI_RU(s390_irgen_TMLH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa701: s390_format_RI_RU(s390_irgen_TMLL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa702: s390_format_RI_RU(s390_irgen_TMHH, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa703: s390_format_RI_RU(s390_irgen_TMHL, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa704: s390_format_RI(s390_irgen_BRC, RI_r1(ovl), RI_i2(ovl));
                               goto ok;
   case 0xa705: s390_format_RI_RP(s390_irgen_BRAS, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa706: s390_format_RI_RP(s390_irgen_BRCT, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa707: s390_format_RI_RP(s390_irgen_BRCTG, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa708: s390_format_RI_RI(s390_irgen_LHI, RI_r1(ovl), RI_i2(ovl));
                                  goto ok;
   case 0xa709: s390_format_RI_RI(s390_irgen_LGHI, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa70a: s390_format_RI_RI(s390_irgen_AHI, RI_r1(ovl), RI_i2(ovl));
                                  goto ok;
   case 0xa70b: s390_format_RI_RI(s390_irgen_AGHI, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa70c: s390_format_RI_RI(s390_irgen_MHI, RI_r1(ovl), RI_i2(ovl));
                                  goto ok;
   case 0xa70d: s390_format_RI_RI(s390_irgen_MGHI, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   case 0xa70e: s390_format_RI_RI(s390_irgen_CHI, RI_r1(ovl), RI_i2(ovl));
                                  goto ok;
   case 0xa70f: s390_format_RI_RI(s390_irgen_CGHI, RI_r1(ovl),
                                  RI_i2(ovl));  goto ok;
   }

   switch ((ovl & 0xffff0000) >> 16) {
   case 0x8000: /* SSM */ goto unimplemented;
   case 0x8200: /* LPSW */ goto unimplemented;
   case 0x9300: /* TS */ goto unimplemented;
   case 0xb202: /* STIDP */ goto unimplemented;
   case 0xb204: /* SCK */ goto unimplemented;
   case 0xb205: s390_format_S_RD(s390_irgen_STCK, S_b2(ovl), S_d2(ovl));
                goto ok;
   case 0xb206: /* SCKC */ goto unimplemented;
   case 0xb207: /* STCKC */ goto unimplemented;
   case 0xb208: /* SPT */ goto unimplemented;
   case 0xb209: /* STPT */ goto unimplemented;
   case 0xb20a: /* SPKA */ goto unimplemented;
   case 0xb20b: /* IPK */ goto unimplemented;
   case 0xb20d: /* PTLB */ goto unimplemented;
   case 0xb210: /* SPX */ goto unimplemented;
   case 0xb211: /* STPX */ goto unimplemented;
   case 0xb212: /* STAP */ goto unimplemented;
   case 0xb214: /* SIE */ goto unimplemented;
   case 0xb218: /* PC */ goto unimplemented;
   case 0xb219: /* SAC */ goto unimplemented;
   case 0xb21a: /* CFC */ goto unimplemented;
   case 0xb221: /* IPTE */ goto unimplemented;
   case 0xb222: s390_format_RRE_R0(s390_irgen_IPM, RRE_r1(ovl));  goto ok;
   case 0xb223: /* IVSK */ goto unimplemented;
   case 0xb224: /* IAC */ goto unimplemented;
   case 0xb225: /* SSAR */ goto unimplemented;
   case 0xb226: /* EPAR */ goto unimplemented;
   case 0xb227: /* ESAR */ goto unimplemented;
   case 0xb228: /* PT */ goto unimplemented;
   case 0xb229: /* ISKE */ goto unimplemented;
   case 0xb22a: /* RRBE */ goto unimplemented;
   case 0xb22b: /* SSKE */ goto unimplemented;
   case 0xb22c: /* TB */ goto unimplemented;
   case 0xb22d: /* DXR */ goto unimplemented;
   case 0xb22e: /* PGIN */ goto unimplemented;
   case 0xb22f: /* PGOUT */ goto unimplemented;
   case 0xb230: /* CSCH */ goto unimplemented;
   case 0xb231: /* HSCH */ goto unimplemented;
   case 0xb232: /* MSCH */ goto unimplemented;
   case 0xb233: /* SSCH */ goto unimplemented;
   case 0xb234: /* STSCH */ goto unimplemented;
   case 0xb235: /* TSCH */ goto unimplemented;
   case 0xb236: /* TPI */ goto unimplemented;
   case 0xb237: /* SAL */ goto unimplemented;
   case 0xb238: /* RSCH */ goto unimplemented;
   case 0xb239: /* STCRW */ goto unimplemented;
   case 0xb23a: /* STCPS */ goto unimplemented;
   case 0xb23b: /* RCHP */ goto unimplemented;
   case 0xb23c: /* SCHM */ goto unimplemented;
   case 0xb240: /* BAKR */ goto unimplemented;
   case 0xb241: s390_format_RRE(s390_irgen_CKSM, RRE_r1(ovl),
                                RRE_r2(ovl));  goto ok;
   case 0xb244: /* SQDR */ goto unimplemented;
   case 0xb245: /* SQER */ goto unimplemented;
   case 0xb246: /* STURA */ goto unimplemented;
   case 0xb247: /* MSTA */ goto unimplemented;
   case 0xb248: /* PALB */ goto unimplemented;
   case 0xb249: /* EREG */ goto unimplemented;
   case 0xb24a: /* ESTA */ goto unimplemented;
   case 0xb24b: /* LURA */ goto unimplemented;
   case 0xb24c: /* TAR */ goto unimplemented;
   case 0xb24d: s390_format_RRE(s390_irgen_CPYA, RRE_r1(ovl),
                                RRE_r2(ovl));  goto ok;
   case 0xb24e: s390_format_RRE(s390_irgen_SAR, RRE_r1(ovl), RRE_r2(ovl));
                                goto ok;
   case 0xb24f: s390_format_RRE(s390_irgen_EAR, RRE_r1(ovl), RRE_r2(ovl));
                                goto ok;
   case 0xb250: /* CSP */ goto unimplemented;
   case 0xb252: s390_format_RRE_RR(s390_irgen_MSR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb254: /* MVPG */ goto unimplemented;
   case 0xb255: s390_format_RRE_RR(s390_irgen_MVST, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb257: /* CUSE */ goto unimplemented;
   case 0xb258: /* BSG */ goto unimplemented;
   case 0xb25a: /* BSA */ goto unimplemented;
   case 0xb25d: s390_format_RRE_RR(s390_irgen_CLST, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb25e: s390_format_RRE_RR(s390_irgen_SRST, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb263: /* CMPSC */ goto unimplemented;
   case 0xb274: /* SIGA */ goto unimplemented;
   case 0xb276: /* XSCH */ goto unimplemented;
   case 0xb277: /* RP */ goto unimplemented;
   case 0xb278: s390_format_S_RD(s390_irgen_STCKE, S_b2(ovl), S_d2(ovl));goto ok;
   case 0xb279: /* SACF */ goto unimplemented;
   case 0xb27c: s390_format_S_RD(s390_irgen_STCKF, S_b2(ovl), S_d2(ovl));goto ok;
   case 0xb27d: /* STSI */ goto unimplemented;
   case 0xb280: /* LPP */ goto unimplemented;
   case 0xb284: /* LCCTL */ goto unimplemented;
   case 0xb285: /* LPCTL */ goto unimplemented;
   case 0xb286: /* QSI */ goto unimplemented;
   case 0xb287: /* LSCTL */ goto unimplemented;
   case 0xb28e: /* QCTRI */ goto unimplemented;
   case 0xb299: s390_format_S_RD(s390_irgen_SRNM, S_b2(ovl), S_d2(ovl));
                                 goto ok;
   case 0xb29c: s390_format_S_RD(s390_irgen_STFPC, S_b2(ovl), S_d2(ovl));
                                 goto ok;
   case 0xb29d: s390_format_S_RD(s390_irgen_LFPC, S_b2(ovl), S_d2(ovl));
                                 goto ok;
   case 0xb2a5: s390_format_RRE_FF(s390_irgen_TRE, RRE_r1(ovl), RRE_r2(ovl));  goto ok;
   case 0xb2a6: s390_format_RRF_M0RERE(s390_irgen_CU21, RRF3_r3(ovl),
                                       RRF3_r1(ovl), RRF3_r2(ovl));
      goto ok;
   case 0xb2a7: s390_format_RRF_M0RERE(s390_irgen_CU12, RRF3_r3(ovl),
                                       RRF3_r1(ovl), RRF3_r2(ovl));
      goto ok;
   case 0xb2b0: s390_format_S_RD(s390_irgen_STFLE, S_b2(ovl), S_d2(ovl));
                                 goto ok;
   case 0xb2b1: /* STFL */ goto unimplemented;
   case 0xb2b2: /* LPSWE */ goto unimplemented;
   case 0xb2b8: s390_irgen_srnmb_wrapper(S_b2(ovl), S_d2(ovl));
      goto ok;
   case 0xb2b9: s390_format_S_RD(s390_irgen_SRNMT, S_b2(ovl), S_d2(ovl));
      goto ok;
   case 0xb2bd: /* LFAS */ goto unimplemented;
   case 0xb2e0: /* SCCTR */ goto unimplemented;
   case 0xb2e1: /* SPCTR */ goto unimplemented;
   case 0xb2e4: /* ECCTR */ goto unimplemented;
   case 0xb2e5: /* EPCTR */ goto unimplemented;
   case 0xb2e8: /* PPA */ goto unimplemented;
   case 0xb2ec: /* ETND */ goto unimplemented;
   case 0xb2ed: /* ECPGA */ goto unimplemented;
   case 0xb2f8: /* TEND */ goto unimplemented;
   case 0xb2fa: /* NIAI */ goto unimplemented;
   case 0xb2fc: /* TABORT */ goto unimplemented;
   case 0xb2ff: /* TRAP4 */ goto unimplemented;
   case 0xb300: s390_format_RRE_FF(s390_irgen_LPEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb301: s390_format_RRE_FF(s390_irgen_LNEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb302: s390_format_RRE_FF(s390_irgen_LTEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb303: s390_format_RRE_FF(s390_irgen_LCEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb304: s390_format_RRE_FF(s390_irgen_LDEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb305: s390_format_RRE_FF(s390_irgen_LXDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb306: s390_format_RRE_FF(s390_irgen_LXEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb307: /* MXDBR */ goto unimplemented;
   case 0xb308: /* KEBR */ goto unimplemented;
   case 0xb309: s390_format_RRE_FF(s390_irgen_CEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb30a: s390_format_RRE_FF(s390_irgen_AEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb30b: s390_format_RRE_FF(s390_irgen_SEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb30c: /* MDEBR */ goto unimplemented;
   case 0xb30d: s390_format_RRE_FF(s390_irgen_DEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb30e: s390_format_RRF_F0FF(s390_irgen_MAEBR, RRF_r1(ovl),
                                     RRF_r3(ovl), RRF_r2(ovl));  goto ok;
   case 0xb30f: s390_format_RRF_F0FF(s390_irgen_MSEBR, RRF_r1(ovl),
                                     RRF_r3(ovl), RRF_r2(ovl));  goto ok;
   case 0xb310: s390_format_RRE_FF(s390_irgen_LPDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb311: s390_format_RRE_FF(s390_irgen_LNDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb312: s390_format_RRE_FF(s390_irgen_LTDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb313: s390_format_RRE_FF(s390_irgen_LCDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb314: s390_format_RRE_FF(s390_irgen_SQEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb315: s390_format_RRE_FF(s390_irgen_SQDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb316: s390_format_RRE_FF(s390_irgen_SQXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb317: s390_format_RRE_FF(s390_irgen_MEEBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb318: /* KDBR */ goto unimplemented;
   case 0xb319: s390_format_RRE_FF(s390_irgen_CDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb31a: s390_format_RRE_FF(s390_irgen_ADBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb31b: s390_format_RRE_FF(s390_irgen_SDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb31c: s390_format_RRE_FF(s390_irgen_MDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb31d: s390_format_RRE_FF(s390_irgen_DDBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb31e: s390_format_RRF_F0FF(s390_irgen_MADBR, RRF_r1(ovl),
                                     RRF_r3(ovl), RRF_r2(ovl));  goto ok;
   case 0xb31f: s390_format_RRF_F0FF(s390_irgen_MSDBR, RRF_r1(ovl),
                                     RRF_r3(ovl), RRF_r2(ovl));  goto ok;
   case 0xb324: s390_format_RRE_FF(s390_irgen_LDER, RRE_r1(ovl),
                                   RRE_r2(ovl)); goto ok;
   case 0xb325: /* LXDR */ goto unimplemented;
   case 0xb326: /* LXER */ goto unimplemented;
   case 0xb32e: /* MAER */ goto unimplemented;
   case 0xb32f: /* MSER */ goto unimplemented;
   case 0xb336: /* SQXR */ goto unimplemented;
   case 0xb337: /* MEER */ goto unimplemented;
   case 0xb338: /* MAYLR */ goto unimplemented;
   case 0xb339: /* MYLR */ goto unimplemented;
   case 0xb33a: /* MAYR */ goto unimplemented;
   case 0xb33b: /* MYR */ goto unimplemented;
   case 0xb33c: /* MAYHR */ goto unimplemented;
   case 0xb33d: /* MYHR */ goto unimplemented;
   case 0xb33e: /* MADR */ goto unimplemented;
   case 0xb33f: /* MSDR */ goto unimplemented;
   case 0xb340: s390_format_RRE_FF(s390_irgen_LPXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb341: s390_format_RRE_FF(s390_irgen_LNXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb342: s390_format_RRE_FF(s390_irgen_LTXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb343: s390_format_RRE_FF(s390_irgen_LCXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb344: s390_format_RRF_UUFF(s390_irgen_LEDBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb345: s390_format_RRF_UUFF(s390_irgen_LDXBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb346: s390_format_RRF_UUFF(s390_irgen_LEXBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb347: s390_format_RRF_UUFF(s390_irgen_FIXBRA, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb348: /* KXBR */ goto unimplemented;
   case 0xb349: s390_format_RRE_FF(s390_irgen_CXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb34a: s390_format_RRE_FF(s390_irgen_AXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb34b: s390_format_RRE_FF(s390_irgen_SXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb34c: s390_format_RRE_FF(s390_irgen_MXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb34d: s390_format_RRE_FF(s390_irgen_DXBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb350: /* TBEDR */ goto unimplemented;
   case 0xb351: /* TBDR */ goto unimplemented;
   case 0xb353: /* DIEBR */ goto unimplemented;
   case 0xb357: s390_format_RRF_UUFF(s390_irgen_FIEBRA, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb358: /* THDER */ goto unimplemented;
   case 0xb359: /* THDR */ goto unimplemented;
   case 0xb35b: /* DIDBR */ goto unimplemented;
   case 0xb35f: s390_format_RRF_UUFF(s390_irgen_FIDBRA, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb360: /* LPXR */ goto unimplemented;
   case 0xb361: /* LNXR */ goto unimplemented;
   case 0xb362: /* LTXR */ goto unimplemented;
   case 0xb363: /* LCXR */ goto unimplemented;
   case 0xb365: s390_format_RRE_FF(s390_irgen_LXR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb366: /* LEXR */ goto unimplemented;
   case 0xb367: /* FIXR */ goto unimplemented;
   case 0xb369: /* CXR */ goto unimplemented;
   case 0xb370: s390_format_RRE_FF(s390_irgen_LPDFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb371: s390_format_RRE_FF(s390_irgen_LNDFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb372: s390_format_RRF_F0FF2(s390_irgen_CPSDR, RRF3_r3(ovl),
                                      RRF3_r1(ovl), RRF3_r2(ovl));
                                      goto ok;
   case 0xb373: s390_format_RRE_FF(s390_irgen_LCDFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb374: s390_format_RRE_F0(s390_irgen_LZER, RRE_r1(ovl));  goto ok;
   case 0xb375: s390_format_RRE_F0(s390_irgen_LZDR, RRE_r1(ovl));  goto ok;
   case 0xb376: s390_format_RRE_F0(s390_irgen_LZXR, RRE_r1(ovl));  goto ok;
   case 0xb377: /* FIER */ goto unimplemented;
   case 0xb37f: /* FIDR */ goto unimplemented;
   case 0xb384: s390_format_RRE_R0(s390_irgen_SFPC, RRE_r1(ovl));  goto ok;
   case 0xb385: /* SFASR */ goto unimplemented;
   case 0xb38c: s390_format_RRE_R0(s390_irgen_EFPC, RRE_r1(ovl));  goto ok;
   case 0xb390: s390_format_RRF_UUFR(s390_irgen_CELFBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb391: s390_format_RRF_UUFR(s390_irgen_CDLFBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb392: s390_format_RRF_UUFR(s390_irgen_CXLFBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb394: s390_format_RRF_UUFR(s390_irgen_CEFBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb395: s390_format_RRF_UUFR(s390_irgen_CDFBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb396: s390_format_RRF_UUFR(s390_irgen_CXFBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb398: s390_format_RRF_UURF(s390_irgen_CFEBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb399: s390_format_RRF_UURF(s390_irgen_CFDBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb39a: s390_format_RRF_UURF(s390_irgen_CFXBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb39c: s390_format_RRF_UURF(s390_irgen_CLFEBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb39d: s390_format_RRF_UURF(s390_irgen_CLFDBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb39e: s390_format_RRF_UURF(s390_irgen_CLFXBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3a0: s390_format_RRF_UUFR(s390_irgen_CELGBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3a1: s390_format_RRF_UUFR(s390_irgen_CDLGBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3a2: s390_format_RRF_UUFR(s390_irgen_CXLGBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3a4: s390_format_RRF_UUFR(s390_irgen_CEGBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3a5: s390_format_RRF_UUFR(s390_irgen_CDGBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3a6: s390_format_RRF_UUFR(s390_irgen_CXGBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3a8: s390_format_RRF_UURF(s390_irgen_CGEBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3a9: s390_format_RRF_UURF(s390_irgen_CGDBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3aa: s390_format_RRF_UURF(s390_irgen_CGXBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3ac: s390_format_RRF_UURF(s390_irgen_CLGEBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3ad: s390_format_RRF_UURF(s390_irgen_CLGDBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3ae: s390_format_RRF_UURF(s390_irgen_CLGXBR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3b4: /* CEFR */ goto unimplemented;
   case 0xb3b5: /* CDFR */ goto unimplemented;
   case 0xb3b6: /* CXFR */ goto unimplemented;
   case 0xb3b8: /* CFER */ goto unimplemented;
   case 0xb3b9: /* CFDR */ goto unimplemented;
   case 0xb3ba: /* CFXR */ goto unimplemented;
   case 0xb3c1: s390_format_RRE_FR(s390_irgen_LDGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3c4: /* CEGR */ goto unimplemented;
   case 0xb3c5: /* CDGR */ goto unimplemented;
   case 0xb3c6: /* CXGR */ goto unimplemented;
   case 0xb3c8: /* CGER */ goto unimplemented;
   case 0xb3c9: /* CGDR */ goto unimplemented;
   case 0xb3ca: /* CGXR */ goto unimplemented;
   case 0xb3cd: s390_format_RRE_RF(s390_irgen_LGDR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3d0: s390_format_RRF_FUFF2(s390_irgen_MDTRA, RRF4_r3(ovl),
                                      RRF4_m4(ovl), RRF4_r1(ovl),
                                      RRF4_r2(ovl)); goto ok;
   case 0xb3d1: s390_format_RRF_FUFF2(s390_irgen_DDTRA, RRF4_r3(ovl),
                                      RRF4_m4(ovl), RRF4_r1(ovl),
                                      RRF4_r2(ovl)); goto ok;
   case 0xb3d2: s390_format_RRF_FUFF2(s390_irgen_ADTRA, RRF4_r3(ovl),
                                      RRF4_m4(ovl), RRF4_r1(ovl),
                                      RRF4_r2(ovl)); goto ok;
   case 0xb3d3: s390_format_RRF_FUFF2(s390_irgen_SDTRA, RRF4_r3(ovl),
                                      RRF4_m4(ovl), RRF4_r1(ovl),
                                      RRF4_r2(ovl)); goto ok;
   case 0xb3d4: s390_format_RRF_0UFF(s390_irgen_LDETR, RRF5_m4(ovl),
                                     RRF5_r1(ovl), RRF5_r2(ovl)); goto ok;
   case 0xb3d5: s390_format_RRF_UUFF(s390_irgen_LEDTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3d6: s390_format_RRE_FF(s390_irgen_LTDTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3d7: /* FIDTR */ goto unimplemented;
   case 0xb3d8: s390_format_RRF_FUFF2(s390_irgen_MXTRA, RRF4_r3(ovl),
                                     RRF4_m4(ovl), RRF4_r1(ovl),
                                     RRF4_r2(ovl)); goto ok;
   case 0xb3d9: s390_format_RRF_FUFF2(s390_irgen_DXTRA, RRF4_r3(ovl),
                                     RRF4_m4(ovl), RRF4_r1(ovl),
                                     RRF4_r2(ovl)); goto ok;
   case 0xb3da: s390_format_RRF_FUFF2(s390_irgen_AXTRA, RRF4_r3(ovl),
                                     RRF4_m4(ovl), RRF4_r1(ovl),
                                     RRF4_r2(ovl)); goto ok;
   case 0xb3db: s390_format_RRF_FUFF2(s390_irgen_SXTRA, RRF4_r3(ovl),
                                     RRF4_m4(ovl), RRF4_r1(ovl),
                                     RRF4_r2(ovl)); goto ok;
   case 0xb3dc: s390_format_RRF_0UFF(s390_irgen_LXDTR, RRF5_m4(ovl),
                                     RRF5_r1(ovl), RRF5_r2(ovl)); goto ok;
   case 0xb3dd: s390_format_RRF_UUFF(s390_irgen_LDXTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3de: s390_format_RRE_FF(s390_irgen_LTXTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3df: /* FIXTR */ goto unimplemented;
   case 0xb3e0: /* KDTR */ goto unimplemented;
   case 0xb3e1: s390_format_RRF_UURF(s390_irgen_CGDTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3e2: /* CUDTR */ goto unimplemented;
   case 0xb3e3: /* CSDTR */ goto unimplemented;
   case 0xb3e4: s390_format_RRE_FF(s390_irgen_CDTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3e5: s390_format_RRE_RF(s390_irgen_EEDTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3e7: s390_format_RRE_RF(s390_irgen_ESDTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3e8: /* KXTR */ goto unimplemented;
   case 0xb3e9: s390_format_RRF_UURF(s390_irgen_CGXTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3ea: /* CUXTR */ goto unimplemented;
   case 0xb3eb: /* CSXTR */ goto unimplemented;
   case 0xb3ec: s390_format_RRE_FF(s390_irgen_CXTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3ed: s390_format_RRE_RF(s390_irgen_EEXTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3ef: s390_format_RRE_RF(s390_irgen_ESXTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3f1: s390_format_RRF_UUFR(s390_irgen_CDGTRA, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3f2: /* CDUTR */ goto unimplemented;
   case 0xb3f3: /* CDSTR */ goto unimplemented;
   case 0xb3f4: s390_format_RRE_FF(s390_irgen_CEDTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3f5: s390_format_RRF_FUFF(s390_irgen_QADTR, RRF4_r3(ovl),
                                     RRF4_m4(ovl), RRF4_r1(ovl),
                                     RRF4_r2(ovl)); goto ok;
   case 0xb3f6: s390_format_RRF_F0FR(s390_irgen_IEDTR, RRF3_r3(ovl),
                                     RRF3_r1(ovl), RRF3_r2(ovl)); goto ok;
   case 0xb3f7: s390_format_RRF_FFRU(s390_irgen_RRDTR, RRF4_r3(ovl),
                                     RRF4_m4(ovl), RRF4_r1(ovl),
                                     RRF4_r2(ovl)); goto ok;
   case 0xb3f9: s390_format_RRF_UUFR(s390_irgen_CXGTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb3fa: /* CXUTR */ goto unimplemented;
   case 0xb3fb: /* CXSTR */ goto unimplemented;
   case 0xb3fc: s390_format_RRE_FF(s390_irgen_CEXTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb3fd: s390_format_RRF_FUFF(s390_irgen_QAXTR, RRF4_r3(ovl),
                                     RRF4_m4(ovl), RRF4_r1(ovl),
                                     RRF4_r2(ovl)); goto ok;
   case 0xb3fe: s390_format_RRF_F0FR(s390_irgen_IEXTR, RRF3_r3(ovl),
                                     RRF3_r1(ovl), RRF3_r2(ovl)); goto ok;
   case 0xb3ff: s390_format_RRF_FFRU(s390_irgen_RRXTR, RRF4_r3(ovl),
                                     RRF4_m4(ovl), RRF4_r1(ovl),
                                     RRF4_r2(ovl)); goto ok;
   case 0xb900: s390_format_RRE_RR(s390_irgen_LPGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb901: s390_format_RRE_RR(s390_irgen_LNGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb902: s390_format_RRE_RR(s390_irgen_LTGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb903: s390_format_RRE_RR(s390_irgen_LCGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb904: s390_format_RRE_RR(s390_irgen_LGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb905: /* LURAG */ goto unimplemented;
   case 0xb906: s390_format_RRE_RR(s390_irgen_LGBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb907: s390_format_RRE_RR(s390_irgen_LGHR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb908: s390_format_RRE_RR(s390_irgen_AGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb909: s390_format_RRE_RR(s390_irgen_SGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb90a: s390_format_RRE_RR(s390_irgen_ALGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb90b: s390_format_RRE_RR(s390_irgen_SLGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb90c: s390_format_RRE_RR(s390_irgen_MSGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb90d: s390_format_RRE_RR(s390_irgen_DSGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb90e: /* EREGG */ goto unimplemented;
   case 0xb90f: s390_format_RRE_RR(s390_irgen_LRVGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb910: s390_format_RRE_RR(s390_irgen_LPGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb911: s390_format_RRE_RR(s390_irgen_LNGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb912: s390_format_RRE_RR(s390_irgen_LTGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb913: s390_format_RRE_RR(s390_irgen_LCGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb914: s390_format_RRE_RR(s390_irgen_LGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb916: s390_format_RRE_RR(s390_irgen_LLGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb917: s390_format_RRE_RR(s390_irgen_LLGTR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb918: s390_format_RRE_RR(s390_irgen_AGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb919: s390_format_RRE_RR(s390_irgen_SGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb91a: s390_format_RRE_RR(s390_irgen_ALGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb91b: s390_format_RRE_RR(s390_irgen_SLGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb91c: s390_format_RRE_RR(s390_irgen_MSGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb91d: s390_format_RRE_RR(s390_irgen_DSGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb91e: /* KMAC */ goto unimplemented;
   case 0xb91f: s390_format_RRE_RR(s390_irgen_LRVR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb920: s390_format_RRE_RR(s390_irgen_CGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb921: s390_format_RRE_RR(s390_irgen_CLGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb925: /* STURG */ goto unimplemented;
   case 0xb926: s390_format_RRE_RR(s390_irgen_LBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb927: s390_format_RRE_RR(s390_irgen_LHR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb928: /* PCKMO */ goto unimplemented;
   case 0xb929: /* KMA */ goto unimplemented;
   case 0xb92a: /* KMF */ goto unimplemented;
   case 0xb92b: /* KMO */ goto unimplemented;
   case 0xb92c: /* PCC */ goto unimplemented;
   case 0xb92d: /* KMCTR */ goto unimplemented;
   case 0xb92e: /* KM */ goto unimplemented;
   case 0xb92f: /* KMC */ goto unimplemented;
   case 0xb930: s390_format_RRE_RR(s390_irgen_CGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb931: s390_format_RRE_RR(s390_irgen_CLGFR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb93c: s390_format_RRE_RR(s390_irgen_PPNO, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb93e: /* KIMD */ goto unimplemented;
   case 0xb93f: /* KLMD */ goto unimplemented;
   case 0xb941: s390_format_RRF_UURF(s390_irgen_CFDTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb942: s390_format_RRF_UURF(s390_irgen_CLGDTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb943: s390_format_RRF_UURF(s390_irgen_CLFDTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb946: s390_format_RRE_RR(s390_irgen_BCTGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb949: s390_format_RRF_UURF(s390_irgen_CFXTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb94a: s390_format_RRF_UURF(s390_irgen_CLGXTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb94b: s390_format_RRF_UURF(s390_irgen_CLFXTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb951: s390_format_RRF_UUFR(s390_irgen_CDFTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb952: s390_format_RRF_UUFR(s390_irgen_CDLGTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb953: s390_format_RRF_UUFR(s390_irgen_CDLFTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb959: s390_format_RRF_UUFR(s390_irgen_CXFTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb95a: s390_format_RRF_UUFR(s390_irgen_CXLGTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb95b: s390_format_RRF_UUFR(s390_irgen_CXLFTR, RRF2_m3(ovl),
                                     RRF2_m4(ovl), RRF2_r1(ovl),
                                     RRF2_r2(ovl));  goto ok;
   case 0xb960: s390_format_RRF_U0RR(s390_irgen_CGRT, RRF2_m3(ovl),
                                     RRF2_r1(ovl), RRF2_r2(ovl),
                                     S390_XMNM_CAB); goto ok;
   case 0xb961: s390_format_RRF_U0RR(s390_irgen_CLGRT, RRF2_m3(ovl),
                                     RRF2_r1(ovl), RRF2_r2(ovl),
                                     S390_XMNM_CAB); goto ok;
   case 0xb972: s390_format_RRF_U0RR(s390_irgen_CRT, RRF2_m3(ovl),
                                     RRF2_r1(ovl), RRF2_r2(ovl),
                                     S390_XMNM_CAB); goto ok;
   case 0xb973: s390_format_RRF_U0RR(s390_irgen_CLRT, RRF2_m3(ovl),
                                     RRF2_r1(ovl), RRF2_r2(ovl),
                                     S390_XMNM_CAB); goto ok;
   case 0xb980: s390_format_RRE_RR(s390_irgen_NGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb981: s390_format_RRE_RR(s390_irgen_OGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb982: s390_format_RRE_RR(s390_irgen_XGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb983: s390_format_RRE_RR(s390_irgen_FLOGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb984: s390_format_RRE_RR(s390_irgen_LLGCR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb985: s390_format_RRE_RR(s390_irgen_LLGHR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb986: s390_format_RRE_RR(s390_irgen_MLGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb987: s390_format_RRE_RR(s390_irgen_DLGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb988: s390_format_RRE_RR(s390_irgen_ALCGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb989: s390_format_RRE_RR(s390_irgen_SLBGR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb98a: /* CSPG */ goto unimplemented;
   case 0xb98d: /* EPSW */ goto unimplemented;
   case 0xb98e: /* IDTE */ goto unimplemented;
   case 0xb98f: /* CRDTE */ goto unimplemented;
   case 0xb990: s390_format_RRF_M0RERE(s390_irgen_TRTT, RRF3_r3(ovl),
                                   RRF3_r1(ovl), RRF3_r2(ovl));  goto ok;
   case 0xb991: s390_format_RRF_M0RERE(s390_irgen_TRTO, RRF3_r3(ovl),
                                   RRF3_r1(ovl), RRF3_r2(ovl));  goto ok;
   case 0xb992: s390_format_RRF_M0RERE(s390_irgen_TROT, RRF3_r3(ovl),
                                   RRF3_r1(ovl), RRF3_r2(ovl));  goto ok;
   case 0xb993: s390_format_RRF_M0RERE(s390_irgen_TROO, RRF3_r3(ovl),
                                   RRF3_r1(ovl), RRF3_r2(ovl));  goto ok;
   case 0xb994: s390_format_RRE_RR(s390_irgen_LLCR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb995: s390_format_RRE_RR(s390_irgen_LLHR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb996: s390_format_RRE_RR(s390_irgen_MLR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb997: s390_format_RRE_RR(s390_irgen_DLR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb998: s390_format_RRE_RR(s390_irgen_ALCR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb999: s390_format_RRE_RR(s390_irgen_SLBR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb99a: /* EPAIR */ goto unimplemented;
   case 0xb99b: /* ESAIR */ goto unimplemented;
   case 0xb99d: /* ESEA */ goto unimplemented;
   case 0xb99e: /* PTI */ goto unimplemented;
   case 0xb99f: /* SSAIR */ goto unimplemented;
   case 0xb9a1: /* TPEI */ goto unimplemented;
   case 0xb9a2: /* PTF */ goto unimplemented;
   case 0xb9aa: /* LPTEA */ goto unimplemented;
   case 0xb9ac: /* IRBM */ goto unimplemented;
   case 0xb9ae: /* RRBM */ goto unimplemented;
   case 0xb9af: /* PFMF */ goto unimplemented;
   case 0xb9b0: s390_format_RRF_M0RERE(s390_irgen_CU14, RRF3_r3(ovl),
                                       RRF3_r1(ovl), RRF3_r2(ovl));
      goto ok;
   case 0xb9b1: s390_format_RRF_M0RERE(s390_irgen_CU24, RRF3_r3(ovl),
                                       RRF3_r1(ovl), RRF3_r2(ovl));
      goto ok;
   case 0xb9b2: s390_format_RRE_RR(s390_irgen_CU41, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb9b3: s390_format_RRE_RR(s390_irgen_CU42, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb9bd: /* TRTRE */ goto unimplemented;
   case 0xb9be: /* SRSTU */ goto unimplemented;
   case 0xb9bf: /* TRTE */ goto unimplemented;
   case 0xb9c8: s390_format_RRF_R0RR2(s390_irgen_AHHHR, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9c9: s390_format_RRF_R0RR2(s390_irgen_SHHHR, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9ca: s390_format_RRF_R0RR2(s390_irgen_ALHHHR, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9cb: s390_format_RRF_R0RR2(s390_irgen_SLHHHR, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9cd: s390_format_RRE_RR(s390_irgen_CHHR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb9cf: s390_format_RRE_RR(s390_irgen_CLHHR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb9d8: s390_format_RRF_R0RR2(s390_irgen_AHHLR, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9d9: s390_format_RRF_R0RR2(s390_irgen_SHHLR, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9da: s390_format_RRF_R0RR2(s390_irgen_ALHHLR, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9db: s390_format_RRF_R0RR2(s390_irgen_SLHHLR, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9dd: s390_format_RRE_RR(s390_irgen_CHLR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb9df: s390_format_RRE_RR(s390_irgen_CLHLR, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb9e0: s390_format_RRF_U0RR(s390_irgen_LOCFHR, RRF3_r3(ovl),
                                     RRF3_r1(ovl), RRF3_r2(ovl),
                                     S390_XMNM_LOCFHR);  goto ok;
   case 0xb9e1: s390_format_RRE_RR(s390_irgen_POPCNT, RRE_r1(ovl),
                                   RRE_r2(ovl));  goto ok;
   case 0xb9e2: s390_format_RRF_U0RR(s390_irgen_LOCGR, RRF3_r3(ovl),
                                     RRF3_r1(ovl), RRF3_r2(ovl),
                                     S390_XMNM_LOCGR);  goto ok;
   case 0xb9e4: s390_format_RRF_R0RR2(s390_irgen_NGRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9e6: s390_format_RRF_R0RR2(s390_irgen_OGRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9e7: s390_format_RRF_R0RR2(s390_irgen_XGRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9e8: s390_format_RRF_R0RR2(s390_irgen_AGRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9e9: s390_format_RRF_R0RR2(s390_irgen_SGRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9ea: s390_format_RRF_R0RR2(s390_irgen_ALGRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9eb: s390_format_RRF_R0RR2(s390_irgen_SLGRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9ec: s390_format_RRF_R0RR2(s390_irgen_MGRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9ed: s390_format_RRF_R0RR2(s390_irgen_MSGRKC, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9f2: s390_format_RRF_U0RR(s390_irgen_LOCR, RRF3_r3(ovl),
                                     RRF3_r1(ovl), RRF3_r2(ovl),
                                     S390_XMNM_LOCR);  goto ok;
   case 0xb9f4: s390_format_RRF_R0RR2(s390_irgen_NRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9f6: s390_format_RRF_R0RR2(s390_irgen_ORK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9f7: s390_format_RRF_R0RR2(s390_irgen_XRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9f8: s390_format_RRF_R0RR2(s390_irgen_ARK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9f9: s390_format_RRF_R0RR2(s390_irgen_SRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9fa: s390_format_RRF_R0RR2(s390_irgen_ALRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9fb: s390_format_RRF_R0RR2(s390_irgen_SLRK, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   case 0xb9fd: s390_format_RRF_R0RR2(s390_irgen_MSRKC, RRF4_r3(ovl),
                                      RRF4_r1(ovl), RRF4_r2(ovl));
                                      goto ok;
   }

   switch ((ovl & 0xff000000) >> 24) {
   case 0x40: s390_format_RX_RRRD(s390_irgen_STH, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x41: s390_format_RX_RRRD(s390_irgen_LA, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x42: s390_format_RX_RRRD(s390_irgen_STC, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x43: s390_format_RX_RRRD(s390_irgen_IC, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x44: s390_format_RX_RRRD(s390_irgen_EX, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x45: /* BAL */ goto unimplemented;
   case 0x46: s390_format_RX_RRRD(s390_irgen_BCT, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x47: s390_format_RX(s390_irgen_BC, RX_r1(ovl), RX_x2(ovl),
                             RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x48: s390_format_RX_RRRD(s390_irgen_LH, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x49: s390_format_RX_RRRD(s390_irgen_CH, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x4a: s390_format_RX_RRRD(s390_irgen_AH, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x4b: s390_format_RX_RRRD(s390_irgen_SH, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x4c: s390_format_RX_RRRD(s390_irgen_MH, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x4d: s390_format_RX_RRRD(s390_irgen_BAS, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x4e: s390_format_RX_RRRD(s390_irgen_CVD, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x4f: s390_format_RX_RRRD(s390_irgen_CVB, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x50: s390_format_RX_RRRD(s390_irgen_ST, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x51: s390_format_RX_RRRD(s390_irgen_LAE, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x54: s390_format_RX_RRRD(s390_irgen_N, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x55: s390_format_RX_RRRD(s390_irgen_CL, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x56: s390_format_RX_RRRD(s390_irgen_O, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x57: s390_format_RX_RRRD(s390_irgen_X, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x58: s390_format_RX_RRRD(s390_irgen_L, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x59: s390_format_RX_RRRD(s390_irgen_C, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x5a: s390_format_RX_RRRD(s390_irgen_A, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x5b: s390_format_RX_RRRD(s390_irgen_S, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x5c: s390_format_RX_RRRD(s390_irgen_M, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x5d: s390_format_RX_RRRD(s390_irgen_D, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x5e: s390_format_RX_RRRD(s390_irgen_AL, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x5f: s390_format_RX_RRRD(s390_irgen_SL, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x60: s390_format_RX_FRRD(s390_irgen_STD, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x67: /* MXD */ goto unimplemented;
   case 0x68: s390_format_RX_FRRD(s390_irgen_LD, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x69: /* CD */ goto unimplemented;
   case 0x6a: /* AD */ goto unimplemented;
   case 0x6b: /* SD */ goto unimplemented;
   case 0x6c: /* MD */ goto unimplemented;
   case 0x6d: /* DD */ goto unimplemented;
   case 0x6e: /* AW */ goto unimplemented;
   case 0x6f: /* SW */ goto unimplemented;
   case 0x70: s390_format_RX_FRRD(s390_irgen_STE, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x71: s390_format_RX_RRRD(s390_irgen_MS, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x78: s390_format_RX_FRRD(s390_irgen_LE, RX_r1(ovl), RX_x2(ovl),
                                  RX_b2(ovl), RX_d2(ovl));  goto ok;
   case 0x79: /* CE */ goto unimplemented;
   case 0x7a: /* AE */ goto unimplemented;
   case 0x7b: /* SE */ goto unimplemented;
   case 0x7c: /* MDE */ goto unimplemented;
   case 0x7d: /* DE */ goto unimplemented;
   case 0x7e: /* AU */ goto unimplemented;
   case 0x7f: /* SU */ goto unimplemented;
   case 0x83: /* DIAG */ goto unimplemented;
   case 0x84: s390_format_RSI_RRP(s390_irgen_BRXH, RSI_r1(ovl),
                                  RSI_r3(ovl), RSI_i2(ovl));  goto ok;
   case 0x85: s390_format_RSI_RRP(s390_irgen_BRXLE, RSI_r1(ovl),
                                  RSI_r3(ovl), RSI_i2(ovl));  goto ok;
   case 0x86: s390_format_RS_RRRD(s390_irgen_BXH, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0x87: s390_format_RS_RRRD(s390_irgen_BXLE, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0x88: s390_format_RS_R0RD(s390_irgen_SRL, RS_r1(ovl), RS_b2(ovl),
                                  RS_d2(ovl));  goto ok;
   case 0x89: s390_format_RS_R0RD(s390_irgen_SLL, RS_r1(ovl), RS_b2(ovl),
                                  RS_d2(ovl));  goto ok;
   case 0x8a: s390_format_RS_R0RD(s390_irgen_SRA, RS_r1(ovl), RS_b2(ovl),
                                  RS_d2(ovl));  goto ok;
   case 0x8b: s390_format_RS_R0RD(s390_irgen_SLA, RS_r1(ovl), RS_b2(ovl),
                                  RS_d2(ovl));  goto ok;
   case 0x8c: s390_format_RS_R0RD(s390_irgen_SRDL, RS_r1(ovl), RS_b2(ovl),
                                  RS_d2(ovl));  goto ok;
   case 0x8d: s390_format_RS_R0RD(s390_irgen_SLDL, RS_r1(ovl), RS_b2(ovl),
                                  RS_d2(ovl));  goto ok;
   case 0x8e: s390_format_RS_R0RD(s390_irgen_SRDA, RS_r1(ovl), RS_b2(ovl),
                                  RS_d2(ovl));  goto ok;
   case 0x8f: s390_format_RS_R0RD(s390_irgen_SLDA, RS_r1(ovl), RS_b2(ovl),
                                  RS_d2(ovl));  goto ok;
   case 0x90: s390_format_RS_RRRD(s390_irgen_STM, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0x91: s390_format_SI_URD(s390_irgen_TM, SI_i2(ovl), SI_b1(ovl),
                                 SI_d1(ovl));  goto ok;
   case 0x92: s390_format_SI_URD(s390_irgen_MVI, SI_i2(ovl), SI_b1(ovl),
                                 SI_d1(ovl));  goto ok;
   case 0x94: s390_format_SI_URD(s390_irgen_NI, SI_i2(ovl), SI_b1(ovl),
                                 SI_d1(ovl));  goto ok;
   case 0x95: s390_format_SI_URD(s390_irgen_CLI, SI_i2(ovl), SI_b1(ovl),
                                 SI_d1(ovl));  goto ok;
   case 0x96: s390_format_SI_URD(s390_irgen_OI, SI_i2(ovl), SI_b1(ovl),
                                 SI_d1(ovl));  goto ok;
   case 0x97: s390_format_SI_URD(s390_irgen_XI, SI_i2(ovl), SI_b1(ovl),
                                 SI_d1(ovl));  goto ok;
   case 0x98: s390_format_RS_RRRD(s390_irgen_LM, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0x99: /* TRACE */ goto unimplemented;
   case 0x9a: s390_format_RS_AARD(s390_irgen_LAM, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0x9b: s390_format_RS_AARD(s390_irgen_STAM, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0xa8: s390_format_RS_RRRD(s390_irgen_MVCLE, RS_r1(ovl),
                                  RS_r3(ovl), RS_b2(ovl), RS_d2(ovl));
                                  goto ok;
   case 0xa9: s390_format_RS_RRRD(s390_irgen_CLCLE, RS_r1(ovl),
                                  RS_r3(ovl), RS_b2(ovl), RS_d2(ovl));
                                  goto ok;
   case 0xac: /* STNSM */ goto unimplemented;
   case 0xad: /* STOSM */ goto unimplemented;
   case 0xae: /* SIGP */ goto unimplemented;
   case 0xaf: /* MC */ goto unimplemented;
   case 0xb1: /* LRA */ goto unimplemented;
   case 0xb6: /* STCTL */ goto unimplemented;
   case 0xb7: /* LCTL */ goto unimplemented;
   case 0xba: s390_format_RS_RRRD(s390_irgen_CS, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0xbb: s390_format_RS_RRRD(s390_irgen_CDS, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0xbd: s390_format_RS_RURD(s390_irgen_CLM, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0xbe: s390_format_RS_RURD(s390_irgen_STCM, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   case 0xbf: s390_format_RS_RURD(s390_irgen_ICM, RS_r1(ovl), RS_r3(ovl),
                                  RS_b2(ovl), RS_d2(ovl));  goto ok;
   }

   return S390_DECODE_UNKNOWN_INSN;

ok:
   return S390_DECODE_OK;

unimplemented:
   return S390_DECODE_UNIMPLEMENTED_INSN;
}

static s390_decode_t
s390_decode_6byte_and_irgen(const UChar *bytes)
{
   ULong ovl = ((ULong)bytes[0] << 56) | ((ULong)bytes[1] << 48) |
               ((ULong)bytes[2] << 40) | ((ULong)bytes[3] << 32) |
               ((ULong)bytes[4] << 24) | ((ULong)bytes[5] << 16);

   switch ((ovl >> 16) & 0xff00000000ffULL) {
   case 0xe30000000002ULL: s390_format_RXY_RRRD(s390_irgen_LTG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000003ULL: /* LRAG */ goto unimplemented;
   case 0xe30000000004ULL: s390_format_RXY_RRRD(s390_irgen_LG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000006ULL: s390_format_RXY_RRRD(s390_irgen_CVBY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000008ULL: s390_format_RXY_RRRD(s390_irgen_AG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000009ULL: s390_format_RXY_RRRD(s390_irgen_SG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000000aULL: s390_format_RXY_RRRD(s390_irgen_ALG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000000bULL: s390_format_RXY_RRRD(s390_irgen_SLG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000000cULL: s390_format_RXY_RRRD(s390_irgen_MSG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000000dULL: s390_format_RXY_RRRD(s390_irgen_DSG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000000eULL: /* CVBG */ goto unimplemented;
   case 0xe3000000000fULL: s390_format_RXY_RRRD(s390_irgen_LRVG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000012ULL: s390_format_RXY_RRRD(s390_irgen_LT, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000013ULL: /* LRAY */ goto unimplemented;
   case 0xe30000000014ULL: s390_format_RXY_RRRD(s390_irgen_LGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000015ULL: s390_format_RXY_RRRD(s390_irgen_LGH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000016ULL: s390_format_RXY_RRRD(s390_irgen_LLGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000017ULL: s390_format_RXY_RRRD(s390_irgen_LLGT, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000018ULL: s390_format_RXY_RRRD(s390_irgen_AGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000019ULL: s390_format_RXY_RRRD(s390_irgen_SGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000001aULL: s390_format_RXY_RRRD(s390_irgen_ALGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000001bULL: s390_format_RXY_RRRD(s390_irgen_SLGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000001cULL: s390_format_RXY_RRRD(s390_irgen_MSGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000001dULL: s390_format_RXY_RRRD(s390_irgen_DSGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000001eULL: s390_format_RXY_RRRD(s390_irgen_LRV, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000001fULL: s390_format_RXY_RRRD(s390_irgen_LRVH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000020ULL: s390_format_RXY_RRRD(s390_irgen_CG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000021ULL: s390_format_RXY_RRRD(s390_irgen_CLG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000024ULL: s390_format_RXY_RRRD(s390_irgen_STG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000025ULL: /* NTSTG */ goto unimplemented;
   case 0xe30000000026ULL: s390_format_RXY_RRRD(s390_irgen_CVDY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000002aULL: s390_format_RXY_RRRD(s390_irgen_LZRG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000002eULL: /* CVDG */ goto unimplemented;
   case 0xe3000000002fULL: s390_format_RXY_RRRD(s390_irgen_STRVG,
                                                RXY_r1(ovl), RXY_x2(ovl),
                                                RXY_b2(ovl), RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000030ULL: s390_format_RXY_RRRD(s390_irgen_CGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000031ULL: s390_format_RXY_RRRD(s390_irgen_CLGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000032ULL: s390_format_RXY_RRRD(s390_irgen_LTGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000034ULL: s390_format_RXY_RRRD(s390_irgen_CGH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000036ULL: s390_format_RXY_URRD(s390_irgen_PFD, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000038ULL: s390_format_RXY_RRRD(s390_irgen_AGH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000039ULL: s390_format_RXY_RRRD(s390_irgen_SGH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000003aULL: s390_format_RXY_RRRD(s390_irgen_LLZRGF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000003bULL: s390_format_RXY_RRRD(s390_irgen_LZRF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000003cULL: s390_format_RXY_RRRD(s390_irgen_MGH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000003eULL: s390_format_RXY_RRRD(s390_irgen_STRV, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000003fULL: s390_format_RXY_RRRD(s390_irgen_STRVH,
                                                RXY_r1(ovl), RXY_x2(ovl),
                                                RXY_b2(ovl), RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000046ULL: s390_format_RXY_RRRD(s390_irgen_BCTG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000047ULL: s390_format_RXY_RRRD(s390_irgen_BIC, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000048ULL: /* LLGFSG */ goto unimplemented;
   case 0xe30000000049ULL: /* STGSC */ goto unimplemented;
   case 0xe3000000004cULL: /* LGG */ goto unimplemented;
   case 0xe3000000004dULL: /* LGSC */ goto unimplemented;
   case 0xe30000000050ULL: s390_format_RXY_RRRD(s390_irgen_STY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000051ULL: s390_format_RXY_RRRD(s390_irgen_MSY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000053ULL: s390_format_RXY_RRRD(s390_irgen_MSC, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000054ULL: s390_format_RXY_RRRD(s390_irgen_NY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000055ULL: s390_format_RXY_RRRD(s390_irgen_CLY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000056ULL: s390_format_RXY_RRRD(s390_irgen_OY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000057ULL: s390_format_RXY_RRRD(s390_irgen_XY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000058ULL: s390_format_RXY_RRRD(s390_irgen_LY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000059ULL: s390_format_RXY_RRRD(s390_irgen_CY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000005aULL: s390_format_RXY_RRRD(s390_irgen_AY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000005bULL: s390_format_RXY_RRRD(s390_irgen_SY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000005cULL: s390_format_RXY_RRRD(s390_irgen_MFY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000005eULL: s390_format_RXY_RRRD(s390_irgen_ALY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000005fULL: s390_format_RXY_RRRD(s390_irgen_SLY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000070ULL: s390_format_RXY_RRRD(s390_irgen_STHY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000071ULL: s390_format_RXY_RRRD(s390_irgen_LAY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000072ULL: s390_format_RXY_RRRD(s390_irgen_STCY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000073ULL: s390_format_RXY_RRRD(s390_irgen_ICY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000075ULL: s390_format_RXY_RRRD(s390_irgen_LAEY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000076ULL: s390_format_RXY_RRRD(s390_irgen_LB, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000077ULL: s390_format_RXY_RRRD(s390_irgen_LGB, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000078ULL: s390_format_RXY_RRRD(s390_irgen_LHY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000079ULL: s390_format_RXY_RRRD(s390_irgen_CHY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000007aULL: s390_format_RXY_RRRD(s390_irgen_AHY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000007bULL: s390_format_RXY_RRRD(s390_irgen_SHY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000007cULL: s390_format_RXY_RRRD(s390_irgen_MHY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000080ULL: s390_format_RXY_RRRD(s390_irgen_NG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000081ULL: s390_format_RXY_RRRD(s390_irgen_OG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000082ULL: s390_format_RXY_RRRD(s390_irgen_XG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000083ULL: s390_format_RXY_RRRD(s390_irgen_MSGC, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000084ULL: s390_format_RXY_RRRD(s390_irgen_MG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000085ULL: s390_format_RXY_RRRD(s390_irgen_LGAT, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;

   case 0xe30000000086ULL: s390_format_RXY_RRRD(s390_irgen_MLG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000087ULL: s390_format_RXY_RRRD(s390_irgen_DLG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000088ULL: s390_format_RXY_RRRD(s390_irgen_ALCG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000089ULL: s390_format_RXY_RRRD(s390_irgen_SLBG, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000008eULL: s390_format_RXY_RRRD(s390_irgen_STPQ, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000008fULL: s390_format_RXY_RRRD(s390_irgen_LPQ, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000090ULL: s390_format_RXY_RRRD(s390_irgen_LLGC, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000091ULL: s390_format_RXY_RRRD(s390_irgen_LLGH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000094ULL: s390_format_RXY_RRRD(s390_irgen_LLC, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000095ULL: s390_format_RXY_RRRD(s390_irgen_LLH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000096ULL: s390_format_RXY_RRRD(s390_irgen_ML, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000097ULL: s390_format_RXY_RRRD(s390_irgen_DL, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000098ULL: s390_format_RXY_RRRD(s390_irgen_ALC, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe30000000099ULL: s390_format_RXY_RRRD(s390_irgen_SLB, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000009cULL: s390_format_RXY_RRRD(s390_irgen_LLGTAT, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000009dULL: s390_format_RXY_RRRD(s390_irgen_LLGFAT, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe3000000009fULL: s390_format_RXY_RRRD(s390_irgen_LAT, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000c0ULL: s390_format_RXY_RRRD(s390_irgen_LBH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000c2ULL: s390_format_RXY_RRRD(s390_irgen_LLCH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000c3ULL: s390_format_RXY_RRRD(s390_irgen_STCH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000c4ULL: s390_format_RXY_RRRD(s390_irgen_LHH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000c6ULL: s390_format_RXY_RRRD(s390_irgen_LLHH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000c7ULL: s390_format_RXY_RRRD(s390_irgen_STHH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000c8ULL: s390_format_RXY_RRRD(s390_irgen_LFHAT, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000caULL: s390_format_RXY_RRRD(s390_irgen_LFH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000cbULL: s390_format_RXY_RRRD(s390_irgen_STFH, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000cdULL: s390_format_RXY_RRRD(s390_irgen_CHF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe300000000cfULL: s390_format_RXY_RRRD(s390_irgen_CLHF, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xe60000000034ULL: /* VPKZ */ goto unimplemented;
   case 0xe60000000035ULL: /* VLRL */ goto unimplemented;
   case 0xe60000000037ULL: /* VLRLR */ goto unimplemented;
   case 0xe6000000003cULL: /* VUPKZ */ goto unimplemented;
   case 0xe6000000003dULL: /* VSTRL */ goto unimplemented;
   case 0xe6000000003fULL: /* VSTRLR */ goto unimplemented;
   case 0xe60000000049ULL: /* VLIP */ goto unimplemented;
   case 0xe60000000050ULL: /* VCVB */ goto unimplemented;
   case 0xe60000000052ULL: /* VCVBG */ goto unimplemented;
   case 0xe60000000058ULL: /* VCVD */ goto unimplemented;
   case 0xe60000000059ULL: /* VSRP */ goto unimplemented;
   case 0xe6000000005aULL: /* VCVDG */ goto unimplemented;
   case 0xe6000000005bULL: /* VPSOP */ goto unimplemented;
   case 0xe6000000005fULL: /* VTP */ goto unimplemented;
   case 0xe60000000071ULL: /* VAP */ goto unimplemented;
   case 0xe60000000073ULL: /* VSP */ goto unimplemented;
   case 0xe60000000077ULL: /* VCP */ goto unimplemented;
   case 0xe60000000078ULL: /* VMP */ goto unimplemented;
   case 0xe60000000079ULL: /* VMSP */ goto unimplemented;
   case 0xe6000000007aULL: /* VDP */ goto unimplemented;
   case 0xe6000000007bULL: /* VRP */ goto unimplemented;
   case 0xe6000000007eULL: /* VSDP */ goto unimplemented;
   case 0xe70000000000ULL: s390_format_VRX_VRRDM(s390_irgen_VLEB, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe70000000001ULL: s390_format_VRX_VRRDM(s390_irgen_VLEH, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe70000000002ULL: s390_format_VRX_VRRDM(s390_irgen_VLEG, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe70000000003ULL: s390_format_VRX_VRRDM(s390_irgen_VLEF, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe70000000004ULL: s390_format_VRX_VRRDM(s390_irgen_VLLEZ, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe70000000005ULL: s390_format_VRX_VRRDM(s390_irgen_VLREP, VRX_v1(ovl),
                                                VRX_x2(ovl), VRX_b2(ovl),
                                                VRX_d2(ovl), VRX_m3(ovl),
                                                VRX_rxb(ovl));  goto ok;
   case 0xe70000000006ULL: s390_format_VRX_VRRD(s390_irgen_VL, VRX_v1(ovl),
                                                VRX_x2(ovl), VRX_b2(ovl),
                                                VRX_d2(ovl), VRX_rxb(ovl));  goto ok;
   case 0xe70000000007ULL: s390_format_VRX_VRRDM(s390_irgen_VLBB, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe70000000008ULL: s390_format_VRX_VRRDM(s390_irgen_VSTEB, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe70000000009ULL: s390_format_VRX_VRRDM(s390_irgen_VSTEH, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe7000000000aULL: s390_format_VRX_VRRDM(s390_irgen_VSTEG, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe7000000000bULL: s390_format_VRX_VRRDM(s390_irgen_VSTEF, VRX_v1(ovl),
                                                 VRX_x2(ovl), VRX_b2(ovl),
                                                 VRX_d2(ovl), VRX_m3(ovl),
                                                 VRX_rxb(ovl));  goto ok;
   case 0xe7000000000eULL: s390_format_VRX_VRRD(s390_irgen_VST, VRX_v1(ovl),
                                                VRX_x2(ovl), VRX_b2(ovl),
                                                VRX_d2(ovl), VRX_rxb(ovl));  goto ok;
   case 0xe70000000012ULL: s390_format_VRV_VVRDMT(s390_irgen_VGEG, VRX_v1(ovl),
                                                  VRX_x2(ovl), VRX_b2(ovl),
                                                  VRX_d2(ovl), VRX_m3(ovl),
                                                  VRX_rxb(ovl), Ity_I64);  goto ok;
   case 0xe70000000013ULL: s390_format_VRV_VVRDMT(s390_irgen_VGEF, VRX_v1(ovl),
                                                  VRX_x2(ovl), VRX_b2(ovl),
                                                  VRX_d2(ovl), VRX_m3(ovl),
                                                  VRX_rxb(ovl), Ity_I32);  goto ok;
   case 0xe7000000001aULL: s390_format_VRV_VVRDMT(s390_irgen_VSCEG, VRX_v1(ovl),
                                                  VRX_x2(ovl), VRX_b2(ovl),
                                                  VRX_d2(ovl), VRX_m3(ovl),
                                                  VRX_rxb(ovl), Ity_I64);  goto ok;
   case 0xe7000000001bULL: s390_format_VRV_VVRDMT(s390_irgen_VSCEF, VRX_v1(ovl),
                                                  VRX_x2(ovl), VRX_b2(ovl),
                                                  VRX_d2(ovl), VRX_m3(ovl),
                                                  VRX_rxb(ovl), Ity_I32);  goto ok;
   case 0xe70000000021ULL: s390_format_VRS_RRDVM(s390_irgen_VLGV, VRS_v1(ovl),
                                                VRS_b2(ovl), VRS_d2(ovl), VRS_v3(ovl),
                                                VRS_m4(ovl), VRS_rxb(ovl));  goto ok;
   case 0xe70000000022ULL: s390_format_VRS_VRRDM(s390_irgen_VLVG, VRS_v1(ovl),
                                                VRS_b2(ovl), VRS_d2(ovl), VRS_v3(ovl),
                                                VRS_m4(ovl), VRS_rxb(ovl));  goto ok;
   case 0xe70000000027ULL: s390_format_RXE_RRRDR(s390_irgen_LCBB, RXE_r1(ovl),
                                                 RXE_x2(ovl), RXE_b2(ovl),
                                                 RXE_d2(ovl), RXE_m3(ovl));  goto ok;
   case 0xe70000000030ULL: s390_format_VRS_VRDVM(s390_irgen_VESL, VRS_v1(ovl),
                                                 VRS_b2(ovl), VRS_d2(ovl),
                                                 VRS_v3(ovl), VRS_m4(ovl),
                                                 VRS_rxb(ovl));  goto ok;
   case 0xe70000000033ULL: s390_format_VRS_VRDVM(s390_irgen_VERLL, VRS_v1(ovl),
                                                 VRS_b2(ovl), VRS_d2(ovl),
                                                 VRS_v3(ovl), VRS_m4(ovl),
                                                 VRS_rxb(ovl));  goto ok;
   case 0xe70000000036ULL: s390_format_VRS_VRDV(s390_irgen_VLM, VRS_v1(ovl),
                                                VRS_b2(ovl), VRS_d2(ovl), VRS_v3(ovl),
                                                VRS_rxb(ovl));  goto ok;
   case 0xe70000000037ULL: s390_format_VRS_VRRD(s390_irgen_VLL, VRS_v1(ovl),
                                                VRS_b2(ovl), VRS_d2(ovl), VRS_v3(ovl),
                                                VRS_rxb(ovl));  goto ok;
   case 0xe70000000038ULL: s390_format_VRS_VRDVM(s390_irgen_VESRL, VRS_v1(ovl),
                                                 VRS_b2(ovl), VRS_d2(ovl),
                                                 VRS_v3(ovl), VRS_m4(ovl),
                                                 VRS_rxb(ovl));  goto ok;
   case 0xe7000000003aULL: s390_format_VRS_VRDVM(s390_irgen_VESRA, VRS_v1(ovl),
                                                 VRS_b2(ovl), VRS_d2(ovl),
                                                 VRS_v3(ovl), VRS_m4(ovl),
                                                 VRS_rxb(ovl));  goto ok;
   case 0xe7000000003eULL: s390_format_VRS_VRDV(s390_irgen_VSTM, VRS_v1(ovl),
                                                VRS_b2(ovl), VRS_d2(ovl), VRS_v3(ovl),
                                                VRS_rxb(ovl));  goto ok;
   case 0xe7000000003fULL: s390_format_VRS_VRRD(s390_irgen_VSTL, VRS_v1(ovl),
                                                VRS_b2(ovl), VRS_d2(ovl), VRS_v3(ovl),
                                                VRS_rxb(ovl));  goto ok;
   case 0xe70000000040ULL: s390_format_VRI_VIM(s390_irgen_VLEIB, VRI_v1(ovl),
                                                 VRI_i2(ovl), VRI_m3(ovl),
                                                 VRI_rxb(ovl));  goto ok;
   case 0xe70000000041ULL: s390_format_VRI_VIM(s390_irgen_VLEIH, VRI_v1(ovl),
                                               VRI_i2(ovl), VRI_m3(ovl),
                                               VRI_rxb(ovl));  goto ok;
   case 0xe70000000042ULL: s390_format_VRI_VIM(s390_irgen_VLEIG, VRI_v1(ovl),
                                               VRI_i2(ovl), VRI_m3(ovl),
                                               VRI_rxb(ovl));  goto ok;
   case 0xe70000000043ULL: s390_format_VRI_VIM(s390_irgen_VLEIF, VRI_v1(ovl),
                                               VRI_i2(ovl), VRI_m3(ovl),
                                               VRI_rxb(ovl));  goto ok;break;
   case 0xe70000000044ULL: s390_format_VRI_VIM(s390_irgen_VGBM, VRI_v1(ovl),
                                               VRI_i2(ovl), VRI_m3(ovl),
                                               VRI_rxb(ovl));  goto ok;
   case 0xe70000000045ULL: s390_format_VRI_VIM(s390_irgen_VREPI, VRI_v1(ovl),
                                               VRI_i2(ovl), VRI_m3(ovl),
                                               VRI_rxb(ovl));  goto ok;
   case 0xe70000000046ULL: s390_format_VRI_VIM(s390_irgen_VGM, VRI_v1(ovl),
                                               VRI_i2(ovl), VRI_m3(ovl),
                                               VRI_rxb(ovl));  goto ok;
   case 0xe7000000004aULL: s390_format_VRI_VVIMM(s390_irgen_VFTCI, VRIe_v1(ovl),
                                                 VRIe_v2(ovl), VRIe_i3(ovl),
                                                 VRIe_m4(ovl), VRIe_m5(ovl),
                                                 VRIe_rxb(ovl));  goto ok;
   case 0xe7000000004dULL: s390_format_VRI_VVIM(s390_irgen_VREP, VRI_v1(ovl),
                                               VRI_v3(ovl), VRI_i2(ovl),
                                               VRI_m3(ovl), VRI_rxb(ovl));  goto ok;
   case 0xe70000000050ULL: s390_format_VRR_VVM(s390_irgen_VPOPCT, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000052ULL: s390_format_VRR_VVM(s390_irgen_VCTZ, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000053ULL: s390_format_VRR_VVM(s390_irgen_VCLZ, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000056ULL: s390_format_VRR_VV(s390_irgen_VLR, VRR_v1(ovl),
                                              VRR_v2(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe7000000005cULL: s390_format_VRR_VVMM(s390_irgen_VISTR, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_m4(ovl),
                                                VRR_m5(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe7000000005fULL: s390_format_VRR_VVM(s390_irgen_VSEG, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000060ULL: s390_format_VRR_VVVM(s390_irgen_VMRL, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000061ULL: s390_format_VRR_VVVM(s390_irgen_VMRH, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000062ULL: s390_format_VRR_VRR(s390_irgen_VLVGP, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000064ULL: s390_format_VRR_VVVM(s390_irgen_VSUM, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000065ULL: s390_format_VRR_VVVM(s390_irgen_VSUMG, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000066ULL: s390_format_VRR_VVV(s390_irgen_VCKSM, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000067ULL: s390_format_VRR_VVVM(s390_irgen_VSUMQ, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000068ULL: s390_format_VRR_VVV(s390_irgen_VN, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000069ULL: s390_format_VRR_VVV(s390_irgen_VNC, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe7000000006aULL: s390_format_VRR_VVV(s390_irgen_VO, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe7000000006bULL: s390_format_VRR_VVV(s390_irgen_VNO, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe7000000006cULL: /* VNX */ goto unimplemented;
   case 0xe7000000006dULL: s390_format_VRR_VVV(s390_irgen_VX, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe7000000006eULL: /* VNN */ goto unimplemented;
   case 0xe7000000006fULL: /* VOC */ goto unimplemented;
   case 0xe70000000070ULL: s390_format_VRR_VVVM(s390_irgen_VESLV, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000072ULL: s390_format_VRId_VVVIM(s390_irgen_VERIM, VRId_v1(ovl),
                                                  VRId_v2(ovl), VRId_v3(ovl),
                                                  VRId_i4(ovl), VRId_m5(ovl),
                                                  VRId_rxb(ovl));  goto ok;
   case 0xe70000000073ULL: s390_format_VRR_VVVM(s390_irgen_VERLLV, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000074ULL: s390_format_VRR_VVV(s390_irgen_VSL, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000075ULL: s390_format_VRR_VVV(s390_irgen_VSLB, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000077ULL: s390_format_VRId_VVVI(s390_irgen_VSLDB, VRId_v1(ovl),
                                                 VRId_v2(ovl), VRId_v3(ovl),
                                                 VRId_i4(ovl), VRId_rxb(ovl));  goto ok;
   case 0xe70000000078ULL: s390_format_VRR_VVVM(s390_irgen_VESRLV, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe7000000007aULL: s390_format_VRR_VVVM(s390_irgen_VESRAV, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe7000000007cULL: s390_format_VRR_VVV(s390_irgen_VSRL, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe7000000007dULL: s390_format_VRR_VVV(s390_irgen_VSRLB, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe7000000007eULL: s390_format_VRR_VVV(s390_irgen_VSRA, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe7000000007fULL: s390_format_VRR_VVV(s390_irgen_VSRAB, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe70000000080ULL: s390_format_VRR_VVVMM(s390_irgen_VFEE, VRR_v1(ovl),
                                                 VRR_v2(ovl), VRR_r3(ovl),
                                                 VRR_m4(ovl), VRR_m5(ovl),
                                                 VRR_rxb(ovl));  goto ok;
   case 0xe70000000081ULL: s390_format_VRR_VVVMM(s390_irgen_VFENE, VRR_v1(ovl),
                                                 VRR_v2(ovl), VRR_r3(ovl),
                                                 VRR_m4(ovl), VRR_m5(ovl),
                                                 VRR_rxb(ovl));  goto ok;
   case 0xe70000000082ULL: s390_format_VRR_VVVMM(s390_irgen_VFAE, VRR_v1(ovl),
                                                 VRR_v2(ovl), VRR_r3(ovl),
                                                 VRR_m4(ovl), VRR_m5(ovl),
                                                 VRR_rxb(ovl));  goto ok;
   case 0xe70000000084ULL: s390_format_VRR_VVVM(s390_irgen_VPDI, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000085ULL: /* VBPERM */ goto unimplemented;
   case 0xe7000000008aULL: s390_format_VRR_VVVVMM(s390_irgen_VSTRC, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_m6(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe7000000008cULL: s390_format_VRR_VVVV(s390_irgen_VPERM, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe7000000008dULL: s390_format_VRR_VVVV(s390_irgen_VSEL, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe7000000008eULL: s390_format_VRR_VVVVMM(s390_irgen_VFMS, VRRe_v1(ovl),
                                                  VRRe_v2(ovl), VRRe_v3(ovl),
                                                  VRRe_v4(ovl), VRRe_m5(ovl),
                                                  VRRe_m6(ovl),
                                                  VRRe_rxb(ovl));  goto ok;
   case 0xe7000000008fULL: s390_format_VRR_VVVVMM(s390_irgen_VFMA, VRRe_v1(ovl),
                                                  VRRe_v2(ovl), VRRe_v3(ovl),
                                                  VRRe_v4(ovl), VRRe_m5(ovl),
                                                  VRRe_m6(ovl),
                                                  VRRe_rxb(ovl));  goto ok;
   case 0xe70000000094ULL: s390_format_VRR_VVVM(s390_irgen_VPK, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000095ULL: s390_format_VRR_VVVMM(s390_irgen_VPKLS, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_m5(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe70000000097ULL: s390_format_VRR_VVVMM(s390_irgen_VPKS, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_m5(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe7000000009eULL: /* VFNMS */ goto unimplemented;
   case 0xe7000000009fULL: /* VFNMA */ goto unimplemented;
   case 0xe700000000a1ULL: s390_format_VRR_VVVM(s390_irgen_VMLH, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000a2ULL: s390_format_VRR_VVVM(s390_irgen_VML, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000a3ULL: s390_format_VRR_VVVM(s390_irgen_VMH, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000a4ULL: s390_format_VRR_VVVM(s390_irgen_VMLE, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000a5ULL: s390_format_VRR_VVVM(s390_irgen_VMLO, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000a6ULL: s390_format_VRR_VVVM(s390_irgen_VME, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000a7ULL: s390_format_VRR_VVVM(s390_irgen_VMO, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000a9ULL: s390_format_VRRd_VVVVM(s390_irgen_VMALH, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000aaULL: s390_format_VRRd_VVVVM(s390_irgen_VMAL, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000abULL: s390_format_VRRd_VVVVM(s390_irgen_VMAH, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000acULL: s390_format_VRRd_VVVVM(s390_irgen_VMALE, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000adULL: s390_format_VRRd_VVVVM(s390_irgen_VMALO, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000aeULL: s390_format_VRRd_VVVVM(s390_irgen_VMAE, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000afULL: s390_format_VRRd_VVVVM(s390_irgen_VMAO, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000b4ULL: s390_format_VRR_VVVM(s390_irgen_VGFM, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000b8ULL: /* VMSL */ goto unimplemented;
   case 0xe700000000b9ULL: s390_format_VRRd_VVVVM(s390_irgen_VACCC, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000bbULL: s390_format_VRRd_VVVVM(s390_irgen_VAC, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000bcULL: s390_format_VRRd_VVVVM(s390_irgen_VGFMA, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000bdULL: s390_format_VRRd_VVVVM(s390_irgen_VSBCBI, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000bfULL: s390_format_VRRd_VVVVM(s390_irgen_VSBI, VRRd_v1(ovl),
                                                  VRRd_v2(ovl), VRRd_v3(ovl),
                                                  VRRd_v4(ovl), VRRd_m5(ovl),
                                                  VRRd_rxb(ovl));  goto ok;
   case 0xe700000000c0ULL: s390_format_VRRa_VVMMM(s390_irgen_VCLGD, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_m3(ovl),
                                                  VRRa_m4(ovl), VRRa_m5(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000c1ULL: s390_format_VRRa_VVMMM(s390_irgen_VCDLG, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_m3(ovl),
                                                  VRRa_m4(ovl), VRRa_m5(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000c2ULL: s390_format_VRRa_VVMMM(s390_irgen_VCGD, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_m3(ovl),
                                                  VRRa_m4(ovl), VRRa_m5(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000c3ULL: s390_format_VRRa_VVMMM(s390_irgen_VCDG, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_m3(ovl),
                                                  VRRa_m4(ovl), VRRa_m5(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000c4ULL: s390_format_VRRa_VVMMM(s390_irgen_VLDE, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_m3(ovl),
                                                  VRRa_m4(ovl), VRRa_m5(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000c5ULL: s390_format_VRRa_VVMMM(s390_irgen_VLED, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_m3(ovl),
                                                  VRRa_m4(ovl), VRRa_m5(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000c7ULL: s390_format_VRRa_VVMMM(s390_irgen_VFI, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_m3(ovl),
                                                  VRRa_m4(ovl), VRRa_m5(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000caULL: s390_format_VRRa_VVMM(s390_irgen_WFK, VRRa_v1(ovl),
                                                 VRRa_v2(ovl), VRRa_m3(ovl),
                                                 VRRa_m4(ovl),
                                                 VRRa_rxb(ovl)); goto ok;
   case 0xe700000000cbULL: s390_format_VRRa_VVMM(s390_irgen_WFC, VRRa_v1(ovl),
                                                 VRRa_v2(ovl), VRRa_m3(ovl),
                                                 VRRa_m4(ovl),
                                                 VRRa_rxb(ovl)); goto ok;
   case 0xe700000000ccULL: s390_format_VRRa_VVMMM(s390_irgen_VFPSO, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_m3(ovl),
                                                  VRRa_m4(ovl), VRRa_m5(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000ceULL: s390_format_VRRa_VVMM(s390_irgen_VFSQ, VRRa_v1(ovl),
                                                 VRRa_v2(ovl), VRRa_m3(ovl),
                                                 VRRa_m4(ovl),
                                                 VRRa_rxb(ovl)); goto ok;
   case 0xe700000000d4ULL: s390_format_VRR_VVM(s390_irgen_VUPLL, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe700000000d5ULL: s390_format_VRR_VVM(s390_irgen_VUPLH, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe700000000d6ULL: s390_format_VRR_VVM(s390_irgen_VUPL, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe700000000d7ULL: s390_format_VRR_VVM(s390_irgen_VUPH, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe700000000d8ULL: s390_format_VRR_VV(s390_irgen_VTM, VRR_v1(ovl),
                                              VRR_v2(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000d9ULL: s390_format_VRR_VVM(s390_irgen_VECL, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe700000000dbULL: s390_format_VRR_VVM(s390_irgen_VEC, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe700000000deULL: s390_format_VRR_VVM(s390_irgen_VLC, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe700000000dfULL: s390_format_VRR_VVM(s390_irgen_VLP, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_m4(ovl),
                                               VRR_rxb(ovl));  goto ok;
   case 0xe700000000e2ULL: s390_format_VRRa_VVVMM(s390_irgen_VFS, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_v3(ovl),
                                                  VRRa_m3(ovl), VRRa_m4(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000e3ULL: s390_format_VRRa_VVVMM(s390_irgen_VFA, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_v3(ovl),
                                                  VRRa_m3(ovl), VRRa_m4(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000e5ULL: s390_format_VRRa_VVVMM(s390_irgen_VFD, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_v3(ovl),
                                                  VRRa_m3(ovl), VRRa_m4(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000e7ULL: s390_format_VRRa_VVVMM(s390_irgen_VFM, VRRa_v1(ovl),
                                                  VRRa_v2(ovl), VRRa_v3(ovl),
                                                  VRRa_m3(ovl), VRRa_m4(ovl),
                                                  VRRa_rxb(ovl)); goto ok;
   case 0xe700000000e8ULL: s390_format_VRRa_VVVMMM(s390_irgen_VFCE, VRRa_v1(ovl),
                                                   VRRa_v2(ovl), VRRa_v3(ovl),
                                                   VRRa_m3(ovl), VRRa_m4(ovl),
                                                   VRRa_m5(ovl),
                                                   VRRa_rxb(ovl)); goto ok;
   case 0xe700000000eaULL: s390_format_VRRa_VVVMMM(s390_irgen_VFCHE, VRRa_v1(ovl),
                                                   VRRa_v2(ovl), VRRa_v3(ovl),
                                                   VRRa_m3(ovl), VRRa_m4(ovl),
                                                   VRRa_m5(ovl),
                                                   VRRa_rxb(ovl)); goto ok;
   case 0xe700000000ebULL: s390_format_VRRa_VVVMMM(s390_irgen_VFCH, VRRa_v1(ovl),
                                                   VRRa_v2(ovl), VRRa_v3(ovl),
                                                   VRRa_m3(ovl), VRRa_m4(ovl),
                                                   VRRa_m5(ovl),
                                                   VRRa_rxb(ovl)); goto ok;
   case 0xe700000000eeULL: /* VFMIN */ goto unimplemented;
   case 0xe700000000efULL: /* VFMAX */ goto unimplemented;
   case 0xe700000000f0ULL: s390_format_VRR_VVVM(s390_irgen_VAVGL, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000f1ULL: s390_format_VRR_VVVM(s390_irgen_VACC, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000f2ULL: s390_format_VRR_VVVM(s390_irgen_VAVG, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000f3ULL: s390_format_VRR_VVVM(s390_irgen_VA, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000f5ULL: s390_format_VRR_VVVM(s390_irgen_VSCBI, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000f7ULL: s390_format_VRR_VVVM(s390_irgen_VS, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000f8ULL: s390_format_VRR_VVVMM(s390_irgen_VCEQ, VRR_v1(ovl),
                                                 VRR_v2(ovl), VRR_r3(ovl),
                                                 VRR_m4(ovl), VRR_m5(ovl),
                                                 VRR_rxb(ovl));  goto ok;
   case 0xe700000000f9ULL: s390_format_VRR_VVVMM(s390_irgen_VCHL, VRR_v1(ovl),
                                                 VRR_v2(ovl), VRR_r3(ovl),
                                                 VRR_m4(ovl), VRR_m5(ovl),
                                                 VRR_rxb(ovl));  goto ok;
   case 0xe700000000fbULL: s390_format_VRR_VVVMM(s390_irgen_VCH, VRR_v1(ovl),
                                                 VRR_v2(ovl), VRR_r3(ovl),
                                                 VRR_m4(ovl), VRR_m5(ovl),
                                                 VRR_rxb(ovl));  goto ok;
   case 0xe700000000fcULL: s390_format_VRR_VVVM(s390_irgen_VMNL, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000fdULL: s390_format_VRR_VVVM(s390_irgen_VMXL, VRR_v1(ovl),
                                                VRR_v2(ovl), VRR_r3(ovl),
                                                VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000feULL: s390_format_VRR_VVVM(s390_irgen_VMN, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xe700000000ffULL: s390_format_VRR_VVVM(s390_irgen_VMX, VRR_v1(ovl),
                                               VRR_v2(ovl), VRR_r3(ovl),
                                               VRR_m4(ovl), VRR_rxb(ovl));  goto ok;
   case 0xeb0000000004ULL: s390_format_RSY_RRRD(s390_irgen_LMG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000000aULL: s390_format_RSY_RRRD(s390_irgen_SRAG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000000bULL: s390_format_RSY_RRRD(s390_irgen_SLAG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000000cULL: s390_format_RSY_RRRD(s390_irgen_SRLG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000000dULL: s390_format_RSY_RRRD(s390_irgen_SLLG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000000fULL: /* TRACG */ goto unimplemented;
   case 0xeb0000000014ULL: s390_format_RSY_RRRD(s390_irgen_CSY, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000001cULL: s390_format_RSY_RRRD(s390_irgen_RLLG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000001dULL: s390_format_RSY_RRRD(s390_irgen_RLL, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000020ULL: s390_format_RSY_RURD(s390_irgen_CLMH, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000021ULL: s390_format_RSY_RURD(s390_irgen_CLMY, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000023ULL: s390_format_RSY_RURD(s390_irgen_CLT, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000024ULL: s390_format_RSY_RRRD(s390_irgen_STMG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000025ULL: /* STCTG */ goto unimplemented;
   case 0xeb0000000026ULL: s390_format_RSY_RRRD(s390_irgen_STMH, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000002bULL: s390_format_RSY_RURD(s390_irgen_CLGT, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000002cULL: s390_format_RSY_RURD(s390_irgen_STCMH,
                                                RSY_r1(ovl), RSY_r3(ovl),
                                                RSY_b2(ovl), RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000002dULL: s390_format_RSY_RURD(s390_irgen_STCMY,
                                                RSY_r1(ovl), RSY_r3(ovl),
                                                RSY_b2(ovl), RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000002fULL: /* LCTLG */ goto unimplemented;
   case 0xeb0000000030ULL: s390_format_RSY_RRRD(s390_irgen_CSG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000031ULL: s390_format_RSY_RRRD(s390_irgen_CDSY, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000003eULL: s390_format_RSY_RRRD(s390_irgen_CDSG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000044ULL: s390_format_RSY_RRRD(s390_irgen_BXHG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000045ULL: s390_format_RSY_RRRD(s390_irgen_BXLEG,
                                                RSY_r1(ovl), RSY_r3(ovl),
                                                RSY_b2(ovl), RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000004cULL: s390_format_RSY_RRRD(s390_irgen_ECAG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000051ULL: s390_format_SIY_URD(s390_irgen_TMY, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb0000000052ULL: s390_format_SIY_URD(s390_irgen_MVIY, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb0000000054ULL: s390_format_SIY_URD(s390_irgen_NIY, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb0000000055ULL: s390_format_SIY_URD(s390_irgen_CLIY, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb0000000056ULL: s390_format_SIY_URD(s390_irgen_OIY, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb0000000057ULL: s390_format_SIY_URD(s390_irgen_XIY, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb000000006aULL: s390_format_SIY_IRD(s390_irgen_ASI, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb000000006eULL: s390_format_SIY_IRD(s390_irgen_ALSI, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb000000007aULL: s390_format_SIY_IRD(s390_irgen_AGSI, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb000000007eULL: s390_format_SIY_IRD(s390_irgen_ALGSI, SIY_i2(ovl),
                                               SIY_b1(ovl), SIY_dl1(ovl),
                                               SIY_dh1(ovl));  goto ok;
   case 0xeb0000000080ULL: s390_format_RSY_RURD(s390_irgen_ICMH, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000081ULL: s390_format_RSY_RURD(s390_irgen_ICMY, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000008eULL: /* MVCLU */ goto unimplemented;
   case 0xeb000000008fULL: /* CLCLU */ goto unimplemented;
   case 0xeb0000000090ULL: s390_format_RSY_RRRD(s390_irgen_STMY, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000096ULL: s390_format_RSY_RRRD(s390_irgen_LMH, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb0000000098ULL: s390_format_RSY_RRRD(s390_irgen_LMY, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000009aULL: s390_format_RSY_AARD(s390_irgen_LAMY, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb000000009bULL: s390_format_RSY_AARD(s390_irgen_STAMY,
                                                RSY_r1(ovl), RSY_r3(ovl),
                                                RSY_b2(ovl), RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000c0ULL: /* TP */ goto unimplemented;
   case 0xeb00000000dcULL: s390_format_RSY_RRRD(s390_irgen_SRAK, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000ddULL: s390_format_RSY_RRRD(s390_irgen_SLAK, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000deULL: s390_format_RSY_RRRD(s390_irgen_SRLK, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000dfULL: s390_format_RSY_RRRD(s390_irgen_SLLK, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000e0ULL: s390_format_RSY_RDRM(s390_irgen_LOCFH, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl),
                                                S390_XMNM_LOCFH);  goto ok;
   case 0xeb00000000e1ULL: s390_format_RSY_RDRM(s390_irgen_STOCFH, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl),
                                                S390_XMNM_STOCFH);  goto ok;
   case 0xeb00000000e2ULL: s390_format_RSY_RDRM(s390_irgen_LOCG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl),
                                                S390_XMNM_LOCG);  goto ok;
   case 0xeb00000000e3ULL: s390_format_RSY_RDRM(s390_irgen_STOCG,
                                                RSY_r1(ovl), RSY_r3(ovl),
                                                RSY_b2(ovl), RSY_dl2(ovl),
                                                RSY_dh2(ovl),
                                                S390_XMNM_STOCG);  goto ok;
   case 0xeb00000000e4ULL: s390_format_RSY_RRRD(s390_irgen_LANG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000e6ULL: s390_format_RSY_RRRD(s390_irgen_LAOG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000e7ULL: s390_format_RSY_RRRD(s390_irgen_LAXG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000e8ULL: s390_format_RSY_RRRD(s390_irgen_LAAG, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000eaULL: s390_format_RSY_RRRD(s390_irgen_LAALG,
                                                RSY_r1(ovl), RSY_r3(ovl),
                                                RSY_b2(ovl), RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000f2ULL: s390_format_RSY_RDRM(s390_irgen_LOC, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl), S390_XMNM_LOC);
                                                goto ok;
   case 0xeb00000000f3ULL: s390_format_RSY_RDRM(s390_irgen_STOC, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl),
                                                S390_XMNM_STOC);  goto ok;
   case 0xeb00000000f4ULL: s390_format_RSY_RRRD(s390_irgen_LAN, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000f6ULL: s390_format_RSY_RRRD(s390_irgen_LAO, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000f7ULL: s390_format_RSY_RRRD(s390_irgen_LAX, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000f8ULL: s390_format_RSY_RRRD(s390_irgen_LAA, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xeb00000000faULL: s390_format_RSY_RRRD(s390_irgen_LAAL, RSY_r1(ovl),
                                                RSY_r3(ovl), RSY_b2(ovl),
                                                RSY_dl2(ovl),
                                                RSY_dh2(ovl));  goto ok;
   case 0xec0000000042ULL: s390_format_RIE_RUPIX(s390_irgen_LOCHI,
                                                 RIEv3_r1(ovl),
                                                 RIEv3_m3(ovl),
                                                 RIEv3_i4(ovl),
                                                 RIEv3_i2(ovl),
                                                 S390_XMNM_LOCHI);  goto ok;
   case 0xec0000000044ULL: s390_format_RIE_RRP(s390_irgen_BRXHG, RIE_r1(ovl),
                                               RIE_r3(ovl), RIE_i2(ovl));
                                               goto ok;
   case 0xec0000000045ULL: s390_format_RIE_RRP(s390_irgen_BRXLG, RIE_r1(ovl),
                                               RIE_r3(ovl), RIE_i2(ovl));
                                               goto ok;
   case 0xec0000000046ULL: s390_format_RIE_RUPIX(s390_irgen_LOCGHI,
                                                 RIEv3_r1(ovl),
                                                 RIEv3_m3(ovl),
                                                 RIEv3_i4(ovl),
                                                 RIEv3_i2(ovl),
                                                 S390_XMNM_LOCGHI);  goto ok;
   case 0xec000000004eULL: s390_format_RIE_RUPIX(s390_irgen_LOCHHI,
                                                 RIEv3_r1(ovl),
                                                 RIEv3_m3(ovl),
                                                 RIEv3_i4(ovl),
                                                 RIEv3_i2(ovl),
                                                 S390_XMNM_LOCHHI);  goto ok;
   case 0xec0000000051ULL: s390_format_RIE_RRUUU(s390_irgen_RISBLG,
                                                 RIE_RRUUU_r1(ovl),
                                                 RIE_RRUUU_r2(ovl),
                                                 RIE_RRUUU_i3(ovl),
                                                 RIE_RRUUU_i4(ovl),
                                                 RIE_RRUUU_i5(ovl));
                                                 goto ok;
   case 0xec0000000054ULL: s390_format_RIE_RRUUU(s390_irgen_RNSBG,
                                                 RIE_RRUUU_r1(ovl),
                                                 RIE_RRUUU_r2(ovl),
                                                 RIE_RRUUU_i3(ovl),
                                                 RIE_RRUUU_i4(ovl),
                                                 RIE_RRUUU_i5(ovl));
                                                 goto ok;
   case 0xec0000000055ULL: s390_format_RIE_RRUUU(s390_irgen_RISBG,
                                                 RIE_RRUUU_r1(ovl),
                                                 RIE_RRUUU_r2(ovl),
                                                 RIE_RRUUU_i3(ovl),
                                                 RIE_RRUUU_i4(ovl),
                                                 RIE_RRUUU_i5(ovl));
                                                 goto ok;
   case 0xec0000000056ULL: s390_format_RIE_RRUUU(s390_irgen_ROSBG,
                                                 RIE_RRUUU_r1(ovl),
                                                 RIE_RRUUU_r2(ovl),
                                                 RIE_RRUUU_i3(ovl),
                                                 RIE_RRUUU_i4(ovl),
                                                 RIE_RRUUU_i5(ovl));
                                                 goto ok;
   case 0xec0000000057ULL: s390_format_RIE_RRUUU(s390_irgen_RXSBG,
                                                 RIE_RRUUU_r1(ovl),
                                                 RIE_RRUUU_r2(ovl),
                                                 RIE_RRUUU_i3(ovl),
                                                 RIE_RRUUU_i4(ovl),
                                                 RIE_RRUUU_i5(ovl));
                                                 goto ok;
   case 0xec0000000059ULL: s390_format_RIE_RRUUU(s390_irgen_RISBGN,
                                                 RIE_RRUUU_r1(ovl),
                                                 RIE_RRUUU_r2(ovl),
                                                 RIE_RRUUU_i3(ovl),
                                                 RIE_RRUUU_i4(ovl),
                                                 RIE_RRUUU_i5(ovl));
                                                 goto ok;
   case 0xec000000005dULL: s390_format_RIE_RRUUU(s390_irgen_RISBHG,
                                                 RIE_RRUUU_r1(ovl),
                                                 RIE_RRUUU_r2(ovl),
                                                 RIE_RRUUU_i3(ovl),
                                                 RIE_RRUUU_i4(ovl),
                                                 RIE_RRUUU_i5(ovl));
                                                 goto ok;
   case 0xec0000000064ULL: s390_format_RIE_RRPU(s390_irgen_CGRJ,
                                                RIE_RRPU_r1(ovl),
                                                RIE_RRPU_r2(ovl),
                                                RIE_RRPU_i4(ovl),
                                                RIE_RRPU_m3(ovl));  goto ok;
   case 0xec0000000065ULL: s390_format_RIE_RRPU(s390_irgen_CLGRJ,
                                                RIE_RRPU_r1(ovl),
                                                RIE_RRPU_r2(ovl),
                                                RIE_RRPU_i4(ovl),
                                                RIE_RRPU_m3(ovl));  goto ok;
   case 0xec0000000070ULL: s390_format_RIEv1(s390_irgen_CGIT,
                                             RIEv1_r1(ovl),
                                             RIEv1_i2(ovl),
                                             RIEv1_m3(ovl)); goto ok;
   case 0xec0000000071ULL: s390_format_RIEv1(s390_irgen_CLGIT,
                                             RIEv1_r1(ovl),
                                             RIEv1_i2(ovl),
                                             RIEv1_m3(ovl)); goto ok;
   case 0xec0000000072ULL: s390_format_RIEv1(s390_irgen_CIT,
                                             RIEv1_r1(ovl),
                                             RIEv1_i2(ovl),
                                             RIEv1_m3(ovl)); goto ok;
   case 0xec0000000073ULL: s390_format_RIEv1(s390_irgen_CLFIT,
                                             RIEv1_r1(ovl),
                                             RIEv1_i2(ovl),
                                             RIEv1_m3(ovl)); goto ok;
   case 0xec0000000076ULL: s390_format_RIE_RRPU(s390_irgen_CRJ,
                                                RIE_RRPU_r1(ovl),
                                                RIE_RRPU_r2(ovl),
                                                RIE_RRPU_i4(ovl),
                                                RIE_RRPU_m3(ovl));  goto ok;
   case 0xec0000000077ULL: s390_format_RIE_RRPU(s390_irgen_CLRJ,
                                                RIE_RRPU_r1(ovl),
                                                RIE_RRPU_r2(ovl),
                                                RIE_RRPU_i4(ovl),
                                                RIE_RRPU_m3(ovl));  goto ok;
   case 0xec000000007cULL: s390_format_RIE_RUPI(s390_irgen_CGIJ,
                                                RIEv3_r1(ovl),
                                                RIEv3_m3(ovl),
                                                RIEv3_i4(ovl),
                                                RIEv3_i2(ovl));  goto ok;
   case 0xec000000007dULL: s390_format_RIE_RUPU(s390_irgen_CLGIJ,
                                                RIEv3_r1(ovl),
                                                RIEv3_m3(ovl),
                                                RIEv3_i4(ovl),
                                                RIEv3_i2(ovl));  goto ok;
   case 0xec000000007eULL: s390_format_RIE_RUPI(s390_irgen_CIJ,
                                                RIEv3_r1(ovl),
                                                RIEv3_m3(ovl),
                                                RIEv3_i4(ovl),
                                                RIEv3_i2(ovl));  goto ok;
   case 0xec000000007fULL: s390_format_RIE_RUPU(s390_irgen_CLIJ,
                                                RIEv3_r1(ovl),
                                                RIEv3_m3(ovl),
                                                RIEv3_i4(ovl),
                                                RIEv3_i2(ovl));  goto ok;
   case 0xec00000000d8ULL: s390_format_RIE_RRI0(s390_irgen_AHIK, RIE_r1(ovl),
                                                RIE_r3(ovl), RIE_i2(ovl));
                                                goto ok;
   case 0xec00000000d9ULL: s390_format_RIE_RRI0(s390_irgen_AGHIK,
                                                RIE_r1(ovl), RIE_r3(ovl),
                                                RIE_i2(ovl));  goto ok;
   case 0xec00000000daULL: s390_format_RIE_RRI0(s390_irgen_ALHSIK,
                                                RIE_r1(ovl), RIE_r3(ovl),
                                                RIE_i2(ovl));  goto ok;
   case 0xec00000000dbULL: s390_format_RIE_RRI0(s390_irgen_ALGHSIK,
                                                RIE_r1(ovl), RIE_r3(ovl),
                                                RIE_i2(ovl));  goto ok;
   case 0xec00000000e4ULL: s390_format_RRS(s390_irgen_CGRB, RRS_r1(ovl),
                                           RRS_r2(ovl), RRS_b4(ovl),
                                           RRS_d4(ovl), RRS_m3(ovl));
                                           goto ok;
   case 0xec00000000e5ULL: s390_format_RRS(s390_irgen_CLGRB, RRS_r1(ovl),
                                           RRS_r2(ovl), RRS_b4(ovl),
                                           RRS_d4(ovl), RRS_m3(ovl));
                                           goto ok;
   case 0xec00000000f6ULL: s390_format_RRS(s390_irgen_CRB, RRS_r1(ovl),
                                           RRS_r2(ovl), RRS_b4(ovl),
                                           RRS_d4(ovl), RRS_m3(ovl));
                                           goto ok;
   case 0xec00000000f7ULL: s390_format_RRS(s390_irgen_CLRB, RRS_r1(ovl),
                                           RRS_r2(ovl), RRS_b4(ovl),
                                           RRS_d4(ovl), RRS_m3(ovl));
                                           goto ok;
   case 0xec00000000fcULL: s390_format_RIS_RURDI(s390_irgen_CGIB,
                                                 RIS_r1(ovl), RIS_m3(ovl),
                                                 RIS_b4(ovl), RIS_d4(ovl),
                                                 RIS_i2(ovl));  goto ok;
   case 0xec00000000fdULL: s390_format_RIS_RURDU(s390_irgen_CLGIB,
                                                 RIS_r1(ovl), RIS_m3(ovl),
                                                 RIS_b4(ovl), RIS_d4(ovl),
                                                 RIS_i2(ovl));  goto ok;
   case 0xec00000000feULL: s390_format_RIS_RURDI(s390_irgen_CIB, RIS_r1(ovl),
                                                 RIS_m3(ovl), RIS_b4(ovl),
                                                 RIS_d4(ovl),
                                                 RIS_i2(ovl));  goto ok;
   case 0xec00000000ffULL: s390_format_RIS_RURDU(s390_irgen_CLIB,
                                                 RIS_r1(ovl), RIS_m3(ovl),
                                                 RIS_b4(ovl), RIS_d4(ovl),
                                                 RIS_i2(ovl));  goto ok;
   case 0xed0000000004ULL: s390_format_RXE_FRRD(s390_irgen_LDEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000005ULL: s390_format_RXE_FRRD(s390_irgen_LXDB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000006ULL: s390_format_RXE_FRRD(s390_irgen_LXEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000007ULL: /* MXDB */ goto unimplemented;
   case 0xed0000000008ULL: /* KEB */ goto unimplemented;
   case 0xed0000000009ULL: s390_format_RXE_FRRD(s390_irgen_CEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000000aULL: s390_format_RXE_FRRD(s390_irgen_AEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000000bULL: s390_format_RXE_FRRD(s390_irgen_SEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000000cULL: /* MDEB */ goto unimplemented;
   case 0xed000000000dULL: s390_format_RXE_FRRD(s390_irgen_DEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000000eULL: s390_format_RXF_FRRDF(s390_irgen_MAEB,
                                                 RXF_r3(ovl), RXF_x2(ovl),
                                                 RXF_b2(ovl), RXF_d2(ovl),
                                                 RXF_r1(ovl));  goto ok;
   case 0xed000000000fULL: s390_format_RXF_FRRDF(s390_irgen_MSEB,
                                                 RXF_r3(ovl), RXF_x2(ovl),
                                                 RXF_b2(ovl), RXF_d2(ovl),
                                                 RXF_r1(ovl));  goto ok;
   case 0xed0000000010ULL: s390_format_RXE_FRRD(s390_irgen_TCEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000011ULL: s390_format_RXE_FRRD(s390_irgen_TCDB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000012ULL: s390_format_RXE_FRRD(s390_irgen_TCXB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000014ULL: s390_format_RXE_FRRD(s390_irgen_SQEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000015ULL: s390_format_RXE_FRRD(s390_irgen_SQDB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000017ULL: s390_format_RXE_FRRD(s390_irgen_MEEB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000018ULL: /* KDB */ goto unimplemented;
   case 0xed0000000019ULL: s390_format_RXE_FRRD(s390_irgen_CDB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000001aULL: s390_format_RXE_FRRD(s390_irgen_ADB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000001bULL: s390_format_RXE_FRRD(s390_irgen_SDB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000001cULL: s390_format_RXE_FRRD(s390_irgen_MDB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000001dULL: s390_format_RXE_FRRD(s390_irgen_DDB, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed000000001eULL: s390_format_RXF_FRRDF(s390_irgen_MADB,
                                                 RXF_r3(ovl), RXF_x2(ovl),
                                                 RXF_b2(ovl), RXF_d2(ovl),
                                                 RXF_r1(ovl));  goto ok;
   case 0xed000000001fULL: s390_format_RXF_FRRDF(s390_irgen_MSDB,
                                                 RXF_r3(ovl), RXF_x2(ovl),
                                                 RXF_b2(ovl), RXF_d2(ovl),
                                                 RXF_r1(ovl));  goto ok;
   case 0xed0000000024ULL: s390_format_RXE_FRRD(s390_irgen_LDE,
                                                RXE_r1(ovl), RXE_x2(ovl),
                                                RXE_b2(ovl),
                                                RXE_d2(ovl)); goto ok;
   case 0xed0000000025ULL: /* LXD */ goto unimplemented;
   case 0xed0000000026ULL: /* LXE */ goto unimplemented;
   case 0xed000000002eULL: /* MAE */ goto unimplemented;
   case 0xed000000002fULL: /* MSE */ goto unimplemented;
   case 0xed0000000034ULL: /* SQE */ goto unimplemented;
   case 0xed0000000035ULL: /* SQD */ goto unimplemented;
   case 0xed0000000037ULL: /* MEE */ goto unimplemented;
   case 0xed0000000038ULL: /* MAYL */ goto unimplemented;
   case 0xed0000000039ULL: /* MYL */ goto unimplemented;
   case 0xed000000003aULL: /* MAY */ goto unimplemented;
   case 0xed000000003bULL: /* MY */ goto unimplemented;
   case 0xed000000003cULL: /* MAYH */ goto unimplemented;
   case 0xed000000003dULL: /* MYH */ goto unimplemented;
   case 0xed000000003eULL: /* MAD */ goto unimplemented;
   case 0xed000000003fULL: /* MSD */ goto unimplemented;
   case 0xed0000000040ULL: s390_format_RXF_FRRDF(s390_irgen_SLDT,
                                                 RXF_r3(ovl), RXF_x2(ovl),
                                                 RXF_b2(ovl), RXF_d2(ovl),
                                                 RXF_r1(ovl));  goto ok;
   case 0xed0000000041ULL: s390_format_RXF_FRRDF(s390_irgen_SRDT,
                                                 RXF_r3(ovl), RXF_x2(ovl),
                                                 RXF_b2(ovl), RXF_d2(ovl),
                                                 RXF_r1(ovl));  goto ok;
   case 0xed0000000048ULL: s390_format_RXF_FRRDF(s390_irgen_SLXT,
                                                 RXF_r3(ovl), RXF_x2(ovl),
                                                 RXF_b2(ovl), RXF_d2(ovl),
                                                 RXF_r1(ovl));  goto ok;
   case 0xed0000000049ULL: s390_format_RXF_FRRDF(s390_irgen_SRXT,
                                                 RXF_r3(ovl), RXF_x2(ovl),
                                                 RXF_b2(ovl), RXF_d2(ovl),
                                                 RXF_r1(ovl));  goto ok;
   case 0xed0000000050ULL: s390_format_RXE_FRRD(s390_irgen_TDCET, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000051ULL: s390_format_RXE_FRRD(s390_irgen_TDGET, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000054ULL: s390_format_RXE_FRRD(s390_irgen_TDCDT, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000055ULL: s390_format_RXE_FRRD(s390_irgen_TDGDT, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000058ULL: s390_format_RXE_FRRD(s390_irgen_TDCXT, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000059ULL: s390_format_RXE_FRRD(s390_irgen_TDGXT, RXE_r1(ovl),
                                                RXE_x2(ovl), RXE_b2(ovl),
                                                RXE_d2(ovl));  goto ok;
   case 0xed0000000064ULL: s390_format_RXY_FRRD(s390_irgen_LEY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xed0000000065ULL: s390_format_RXY_FRRD(s390_irgen_LDY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xed0000000066ULL: s390_format_RXY_FRRD(s390_irgen_STEY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xed0000000067ULL: s390_format_RXY_FRRD(s390_irgen_STDY, RXY_r1(ovl),
                                                RXY_x2(ovl), RXY_b2(ovl),
                                                RXY_dl2(ovl),
                                                RXY_dh2(ovl));  goto ok;
   case 0xed00000000a8ULL: /* CZDT */ goto unimplemented;
   case 0xed00000000a9ULL: /* CZXT */ goto unimplemented;
   case 0xed00000000aaULL: /* CDZT */ goto unimplemented;
   case 0xed00000000abULL: /* CXZT */ goto unimplemented;
   case 0xed00000000acULL: /* CPDT */ goto unimplemented;
   case 0xed00000000adULL: /* CPXT */ goto unimplemented;
   case 0xed00000000aeULL: /* CDPT */ goto unimplemented;
   case 0xed00000000afULL: /* CXPT */ goto unimplemented;
   }

   switch (((ovl >> 16) & 0xff0f00000000ULL) >> 32) {
   case 0xc000ULL: s390_format_RIL_RP(s390_irgen_LARL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc001ULL: s390_format_RIL_RI(s390_irgen_LGFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc004ULL: s390_format_RIL(s390_irgen_BRCL, RIL_r1(ovl),
                                   RIL_i2(ovl));  goto ok;
   case 0xc005ULL: s390_format_RIL_RP(s390_irgen_BRASL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc006ULL: s390_format_RIL_RU(s390_irgen_XIHF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc007ULL: s390_format_RIL_RU(s390_irgen_XILF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc008ULL: s390_format_RIL_RU(s390_irgen_IIHF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc009ULL: s390_format_RIL_RU(s390_irgen_IILF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc00aULL: s390_format_RIL_RU(s390_irgen_NIHF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc00bULL: s390_format_RIL_RU(s390_irgen_NILF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc00cULL: s390_format_RIL_RU(s390_irgen_OIHF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc00dULL: s390_format_RIL_RU(s390_irgen_OILF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc00eULL: s390_format_RIL_RU(s390_irgen_LLIHF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc00fULL: s390_format_RIL_RU(s390_irgen_LLILF, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc200ULL: s390_format_RIL_RI(s390_irgen_MSGFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc201ULL: s390_format_RIL_RI(s390_irgen_MSFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc204ULL: s390_format_RIL_RU(s390_irgen_SLGFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc205ULL: s390_format_RIL_RU(s390_irgen_SLFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc208ULL: s390_format_RIL_RI(s390_irgen_AGFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc209ULL: s390_format_RIL_RI(s390_irgen_AFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc20aULL: s390_format_RIL_RU(s390_irgen_ALGFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc20bULL: s390_format_RIL_RU(s390_irgen_ALFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc20cULL: s390_format_RIL_RI(s390_irgen_CGFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc20dULL: s390_format_RIL_RI(s390_irgen_CFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc20eULL: s390_format_RIL_RU(s390_irgen_CLGFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc20fULL: s390_format_RIL_RU(s390_irgen_CLFI, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc402ULL: s390_format_RIL_RP(s390_irgen_LLHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc404ULL: s390_format_RIL_RP(s390_irgen_LGHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc405ULL: s390_format_RIL_RP(s390_irgen_LHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc406ULL: s390_format_RIL_RP(s390_irgen_LLGHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc407ULL: s390_format_RIL_RP(s390_irgen_STHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc408ULL: s390_format_RIL_RP(s390_irgen_LGRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc40bULL: s390_format_RIL_RP(s390_irgen_STGRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc40cULL: s390_format_RIL_RP(s390_irgen_LGFRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc40dULL: s390_format_RIL_RP(s390_irgen_LRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc40eULL: s390_format_RIL_RP(s390_irgen_LLGFRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc40fULL: s390_format_RIL_RP(s390_irgen_STRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc600ULL: s390_format_RIL_RP(s390_irgen_EXRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc602ULL: s390_format_RIL_UP(s390_irgen_PFDRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc604ULL: s390_format_RIL_RP(s390_irgen_CGHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc605ULL: s390_format_RIL_RP(s390_irgen_CHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc606ULL: s390_format_RIL_RP(s390_irgen_CLGHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc607ULL: s390_format_RIL_RP(s390_irgen_CLHRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc608ULL: s390_format_RIL_RP(s390_irgen_CGRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc60aULL: s390_format_RIL_RP(s390_irgen_CLGRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc60cULL: s390_format_RIL_RP(s390_irgen_CGFRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc60dULL: s390_format_RIL_RP(s390_irgen_CRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc60eULL: s390_format_RIL_RP(s390_irgen_CLGFRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc60fULL: s390_format_RIL_RP(s390_irgen_CLRL, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xc800ULL: /* MVCOS */ goto unimplemented;
   case 0xc801ULL: /* ECTG */ goto unimplemented;
   case 0xc802ULL: /* CSST */ goto unimplemented;
   case 0xc804ULL: /* LPD */ goto unimplemented;
   case 0xc805ULL: /* LPDG */ goto unimplemented;
   case 0xcc06ULL:  s390_format_RIL_RP(s390_irgen_BRCTH, RIL_r1(ovl),
                                       RIL_i2(ovl));  goto ok;
   case 0xcc08ULL: s390_format_RIL_RI(s390_irgen_AIH, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xcc0aULL: s390_format_RIL_RI(s390_irgen_ALSIH, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xcc0bULL: s390_format_RIL_RI(s390_irgen_ALSIHN, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xcc0dULL: s390_format_RIL_RI(s390_irgen_CIH, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   case 0xcc0fULL: s390_format_RIL_RU(s390_irgen_CLIH, RIL_r1(ovl),
                                      RIL_i2(ovl));  goto ok;
   }

   switch (((ovl >> 16) & 0xff0000000000ULL) >> 40) {
   case 0xc5ULL: /* BPRP */ goto unimplemented;
   case 0xc7ULL: /* BPP */ goto unimplemented;
   case 0xd0ULL: /* TRTR */ goto unimplemented;
   case 0xd1ULL: /* MVN */ goto unimplemented;
   case 0xd2ULL: s390_format_SS_L0RDRD(s390_irgen_MVC, SS_l(ovl),
                                       SS_b1(ovl), SS_d1(ovl),
                                       SS_b2(ovl), SS_d2(ovl));  goto ok;
   case 0xd3ULL: /* MVZ */ goto unimplemented;
   case 0xd4ULL: s390_format_SS_L0RDRD(s390_irgen_NC, SS_l(ovl),
                                       SS_b1(ovl), SS_d1(ovl),
                                       SS_b2(ovl), SS_d2(ovl));  goto ok;
   case 0xd5ULL: s390_format_SS_L0RDRD(s390_irgen_CLC, SS_l(ovl),
                                       SS_b1(ovl), SS_d1(ovl),
                                       SS_b2(ovl), SS_d2(ovl));  goto ok;
   case 0xd6ULL: s390_format_SS_L0RDRD(s390_irgen_OC, SS_l(ovl),
                                       SS_b1(ovl), SS_d1(ovl),
                                       SS_b2(ovl), SS_d2(ovl));  goto ok;
   case 0xd7ULL:
      if (SS_b1(ovl) == SS_b2(ovl) && SS_d1(ovl) == SS_d2(ovl))
         s390_irgen_XC_sameloc(SS_l(ovl), SS_b1(ovl), SS_d1(ovl));
      else
        s390_format_SS_L0RDRD(s390_irgen_XC, SS_l(ovl),
                              SS_b1(ovl), SS_d1(ovl),
                              SS_b2(ovl), SS_d2(ovl));
      goto ok;
   case 0xd9ULL: /* MVCK */ goto unimplemented;
   case 0xdaULL: /* MVCP */ goto unimplemented;
   case 0xdbULL: /* MVCS */ goto unimplemented;
   case 0xdcULL: s390_format_SS_L0RDRD(s390_irgen_TR, SS_l(ovl),
                                       SS_b1(ovl), SS_d1(ovl),
                                       SS_b2(ovl), SS_d2(ovl));  goto ok;
   case 0xddULL: /* TRT */ goto unimplemented;
   case 0xdeULL: /* ED */ goto unimplemented;
   case 0xdfULL: /* EDMK */ goto unimplemented;
   case 0xe1ULL: /* PKU */ goto unimplemented;
   case 0xe2ULL: /* UNPKU */ goto unimplemented;
   case 0xe8ULL: s390_format_SS_L0RDRD(s390_irgen_MVCIN, SS_l(ovl),
                                       SS_b1(ovl), SS_d1(ovl),
                                       SS_b2(ovl), SS_d2(ovl));  goto ok;
   case 0xe9ULL: /* PKA */ goto unimplemented;
   case 0xeaULL: /* UNPKA */ goto unimplemented;
   case 0xeeULL: /* PLO */ goto unimplemented;
   case 0xefULL: /* LMD */ goto unimplemented;
   case 0xf0ULL: /* SRP */ goto unimplemented;
   case 0xf1ULL: /* MVO */ goto unimplemented;
   case 0xf2ULL: /* PACK */ goto unimplemented;
   case 0xf3ULL: /* UNPK */ goto unimplemented;
   case 0xf8ULL: /* ZAP */ goto unimplemented;
   case 0xf9ULL: /* CP */ goto unimplemented;
   case 0xfaULL: /* AP */ goto unimplemented;
   case 0xfbULL: /* SP */ goto unimplemented;
   case 0xfcULL: /* MP */ goto unimplemented;
   case 0xfdULL: /* DP */ goto unimplemented;
   }

   switch (((ovl >> 16) & 0xffff00000000ULL) >> 32) {
   case 0xe500ULL: /* LASP */ goto unimplemented;
   case 0xe501ULL: /* TPROT */ goto unimplemented;
   case 0xe502ULL: /* STRAG */ goto unimplemented;
   case 0xe50eULL: /* MVCSK */ goto unimplemented;
   case 0xe50fULL: /* MVCDK */ goto unimplemented;
   case 0xe544ULL: s390_format_SIL_RDI(s390_irgen_MVHHI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe548ULL: s390_format_SIL_RDI(s390_irgen_MVGHI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe54cULL: s390_format_SIL_RDI(s390_irgen_MVHI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe554ULL: s390_format_SIL_RDI(s390_irgen_CHHSI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe555ULL: s390_format_SIL_RDU(s390_irgen_CLHHSI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe558ULL: s390_format_SIL_RDI(s390_irgen_CGHSI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe559ULL: s390_format_SIL_RDU(s390_irgen_CLGHSI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe55cULL: s390_format_SIL_RDI(s390_irgen_CHSI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe55dULL: s390_format_SIL_RDU(s390_irgen_CLFHSI, SIL_b1(ovl),
                                       SIL_d1(ovl), SIL_i2(ovl));
                                       goto ok;
   case 0xe560ULL: /* TBEGIN */ goto unimplemented;
   case 0xe561ULL: /* TBEGINC */ goto unimplemented;
   }

   return S390_DECODE_UNKNOWN_INSN;

ok:
   return S390_DECODE_OK;

unimplemented:
   return S390_DECODE_UNIMPLEMENTED_INSN;
}

/* Handle "special" instructions. */
static s390_decode_t
s390_decode_special_and_irgen(const UChar *bytes)
{
   s390_decode_t status = S390_DECODE_OK;

   /* Got a "Special" instruction preamble.  Which one is it? */
   if (bytes[0] == 0x18 && bytes[1] == 0x22 /* lr %r2, %r2 */) {
      s390_irgen_client_request();
   } else if (bytes[0] == 0x18 && bytes[1] == 0x33 /* lr %r3, %r3 */) {
      s390_irgen_guest_NRADDR();
   } else if (bytes[0] == 0x18 && bytes[1] == 0x44 /* lr %r4, %r4 */) {
      s390_irgen_call_noredir();
   } else if (bytes[0] == 0x18 && bytes[1] == 0x55 /* lr %r5, %r5 */) {
      vex_inject_ir(irsb, Iend_BE);

      /* Invalidate the current insn. The reason is that the IRop we're
         injecting here can change. In which case the translation has to
         be redone. For ease of handling, we simply invalidate all the
         time. */
      stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMSTART),
                      mkU64(guest_IA_curr_instr)));
      stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMLEN),
                      mkU64(guest_IA_next_instr - guest_IA_curr_instr)));
      vassert(guest_IA_next_instr - guest_IA_curr_instr ==
              S390_SPECIAL_OP_PREAMBLE_SIZE + S390_SPECIAL_OP_SIZE);

      put_IA(mkaddr_expr(guest_IA_next_instr));
      dis_res->whatNext    = Dis_StopHere;
      dis_res->jk_StopHere = Ijk_InvalICache;
   } else {
      /* We don't know what it is. */
      return S390_DECODE_UNKNOWN_SPECIAL_INSN;
   }

   dis_res->len = S390_SPECIAL_OP_PREAMBLE_SIZE + S390_SPECIAL_OP_SIZE;

   return status;
}


/* Function returns # bytes that were decoded or 0 in case of failure */
static UInt
s390_decode_and_irgen(const UChar *bytes, UInt insn_length, DisResult *dres)
{
   s390_decode_t status;

   dis_res = dres;

   /* Spot the 8-byte preamble:   18ff lr r15,r15
                                  1811 lr r1,r1
                                  1822 lr r2,r2
                                  1833 lr r3,r3 */
   if (bytes[ 0] == 0x18 && bytes[ 1] == 0xff && bytes[ 2] == 0x18 &&
       bytes[ 3] == 0x11 && bytes[ 4] == 0x18 && bytes[ 5] == 0x22 &&
       bytes[ 6] == 0x18 && bytes[ 7] == 0x33) {

      /* Handle special instruction that follows that preamble. */
      if (0) vex_printf("special function handling...\n");

      insn_length = S390_SPECIAL_OP_PREAMBLE_SIZE + S390_SPECIAL_OP_SIZE;
      guest_IA_next_instr = guest_IA_curr_instr + insn_length;

      status =
         s390_decode_special_and_irgen(bytes + S390_SPECIAL_OP_PREAMBLE_SIZE);
   } else {
      /* Handle normal instructions. */
      switch (insn_length) {
      case 2:
         status = s390_decode_2byte_and_irgen(bytes);
         break;

      case 4:
         status = s390_decode_4byte_and_irgen(bytes);
         break;

      case 6:
         status = s390_decode_6byte_and_irgen(bytes);
         break;

      default:
        status = S390_DECODE_ERROR;
        break;
      }
   }
   /* If next instruction is execute, stop here */
   if (dis_res->whatNext == Dis_Continue && bytes[insn_length] == 0x44) {
      put_IA(mkaddr_expr(guest_IA_next_instr));
      dis_res->whatNext = Dis_StopHere;
      dis_res->jk_StopHere = Ijk_Boring;
   }

   if (status == S390_DECODE_OK) {
      /* Adjust status if a specification exception was indicated. */
      if (is_specification_exception())
         status = S390_DECODE_SPECIFICATION_EXCEPTION;
      else
         return insn_length;  /* OK */
   }

   /* Decoding failed somehow */
   if (sigill_diag) {
      vex_printf("vex s390->IR: ");
      switch (status) {
      case S390_DECODE_UNKNOWN_INSN:
         vex_printf("unknown insn: ");
         break;

      case S390_DECODE_UNIMPLEMENTED_INSN:
         vex_printf("unimplemented insn: ");
         break;

      case S390_DECODE_UNKNOWN_SPECIAL_INSN:
         vex_printf("unimplemented special insn: ");
         break;

      case S390_DECODE_SPECIFICATION_EXCEPTION:
         vex_printf("specification exception: ");
         break;

      case S390_DECODE_ERROR:
         vex_printf("decoding error: ");
         break;

      default:
         vpanic("s390_decode_and_irgen");
      }

      vex_printf("%02x%02x", bytes[0], bytes[1]);
      if (insn_length > 2) {
         vex_printf(" %02x%02x", bytes[2], bytes[3]);
      }
      if (insn_length > 4) {
         vex_printf(" %02x%02x", bytes[4], bytes[5]);
      }
      vex_printf("\n");
   }

   return 0;  /* Failed */
}


/* Disassemble a single instruction INSN into IR. */
static DisResult
disInstr_S390_WRK(const UChar *insn)
{
   UChar byte;
   UInt  insn_length;
   DisResult dres;

   /* ---------------------------------------------------- */
   /* --- Compute instruction length                    -- */
   /* ---------------------------------------------------- */

   /* Get the first byte of the insn. */
   byte = insn[0];

   /* The leftmost two bits (0:1) encode the length of the insn in bytes.
      00 -> 2 bytes, 01 -> 4 bytes, 10 -> 4 bytes, 11 -> 6 bytes. */
   insn_length = ((((byte >> 6) + 1) >> 1) + 1) << 1;

   guest_IA_next_instr = guest_IA_curr_instr + insn_length;

   /* ---------------------------------------------------- */
   /* --- Initialise the DisResult data                 -- */
   /* ---------------------------------------------------- */
   dres.whatNext   = Dis_Continue;
   dres.len        = insn_length;
   dres.jk_StopHere = Ijk_INVALID;
   dres.hint        = Dis_HintNone;

   /* fixs390: consider chasing of conditional jumps */

   /* Normal and special instruction handling starts here. */
   if (s390_decode_and_irgen(insn, insn_length, &dres) == 0) {
      /* All decode failures end up here. The decoder has already issued an
         error message.
         Tell the dispatcher that this insn cannot be decoded, and so has
         not been executed, and (is currently) the next to be executed.
         The insn address in the guest state needs to be set to 
         guest_IA_curr_instr, otherwise the complaint will report an
         incorrect address. */
      put_IA(mkaddr_expr(guest_IA_curr_instr));

      dres.len         = 0;
      dres.whatNext    = Dis_StopHere;
      dres.jk_StopHere = Ijk_NoDecode;
   } else {
      /* Decode success */
      switch (dres.whatNext) {
      case Dis_Continue:
         put_IA(mkaddr_expr(guest_IA_next_instr));
         break;
      case Dis_StopHere:
         if (dres.jk_StopHere == Ijk_EmWarn ||
             dres.jk_StopHere == Ijk_EmFail) {
            /* We assume here, that emulation warnings are not given for
               insns that transfer control. There is no good way to
               do that. */
            put_IA(mkaddr_expr(guest_IA_next_instr));
         }
         break;
      default:
         vpanic("disInstr_S390_WRK");
      }
   }

   return dres;
}


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult
disInstr_S390(IRSB        *irsb_IN,
              const UChar *guest_code,
              Long         delta,
              Addr         guest_IP,
              VexArch      guest_arch,
              const VexArchInfo *archinfo,
              const VexAbiInfo  *abiinfo,
              VexEndness   host_endness,
              Bool         sigill_diag_IN)
{
   vassert(guest_arch == VexArchS390X);

   /* Set globals (see top of this file) */
   guest_IA_curr_instr = guest_IP;
   irsb = irsb_IN;
   sigill_diag = sigill_diag_IN;

   return disInstr_S390_WRK(guest_code + delta);
}

/*---------------------------------------------------------------*/
/*--- end                                   guest_s390_toIR.c ---*/
/*---------------------------------------------------------------*/
