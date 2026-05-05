/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                 guest_s390_toIR.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2026

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
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
enum { Invalid_execute_target = 1 };
ULong last_execute_target = Invalid_execute_target;

/* The guest address to be used as the base for relative addresses. */
Addr64 guest_IA_rel_base;

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
#define IE_i1(insn) (((insn) >> 4) & 0xf)
#define IE_i2(insn) ((insn) & 0xf)
#define MII_m1(insn) (((insn) >> 52) & 0xf)
#define MII_i2(insn) (((insn) >> 40) & 0xfff)
#define MII_i3(insn) (((insn) >> 16) & 0xffffff)
#define RR_r1(insn) (((insn) >> 4) & 0xf)
#define RR_r2(insn) ((insn) & 0xf)
#define RI_r1(insn) (((insn) >> 20) & 0xf)
#define RI_i2(insn) ((insn) & 0xffff)
#define RRD_r1(insn) (((insn) >> 12) & 0xf)
#define RRD_r3(insn) (((insn) >> 4) & 0xf)
#define RRD_r2(insn) ((insn) & 0xf)
#define RRE_r1(insn) (((insn) >> 4) & 0xf)
#define RRE_r2(insn) ((insn) & 0xf)
#define RRFa_r3(insn) (((insn) >> 12) & 0xf)
#define RRFa_m4(insn) (((insn) >> 8) & 0xf)
#define RRFa_r1(insn) (((insn) >> 4) & 0xf)
#define RRFa_r2(insn) ((insn) & 0xf)
#define RRFb_r3(insn) (((insn) >> 12) & 0xf)
#define RRFb_m4(insn) (((insn) >> 8) & 0xf)
#define RRFb_r1(insn) (((insn) >> 4) & 0xf)
#define RRFb_r2(insn) ((insn) & 0xf)
#define RRFc_m3(insn) (((insn) >> 12) & 0xf)
#define RRFc_r1(insn) (((insn) >> 4) & 0xf)
#define RRFc_r2(insn) ((insn) & 0xf)
#define RRFd_m4(insn) (((insn) >> 8) & 0xf)
#define RRFd_r1(insn) (((insn) >> 4) & 0xf)
#define RRFd_r2(insn) ((insn) & 0xf)
#define RRFe_m3(insn) (((insn) >> 12) & 0xf)
#define RRFe_m4(insn) (((insn) >> 8) & 0xf)
#define RRFe_r1(insn) (((insn) >> 4) & 0xf)
#define RRFe_r2(insn) ((insn) & 0xf)
#define RS_r1(insn) (((insn) >> 20) & 0xf)
#define RS_r3(insn) (((insn) >> 16) & 0xf)
#define RS_b2(insn) (((insn) >> 12) & 0xf)
#define RS_d2(insn) ((insn) & 0xfff)
#define RSI_r1(insn) (((insn) >> 20) & 0xf)
#define RSI_r3(insn) (((insn) >> 16) & 0xf)
#define RSI_i2(insn) ((insn) & 0xffff)
#define S_b2(insn) (((insn) >> 12) & 0xf)
#define S_d2(insn) ((insn) & 0xfff)
#define RIEd_r1(insn) (((insn) >> 52) & 0xf)
#define RIEd_r3(insn) (((insn) >> 48) & 0xf)
#define RIEd_i2(insn) (((insn) >> 32) & 0xffff)
#define RIEe_r1(insn) (((insn) >> 52) & 0xf)
#define RIEe_r3(insn) (((insn) >> 48) & 0xf)
#define RIEe_i2(insn) (((insn) >> 32) & 0xffff)
#define RIEf_r1(insn) (((insn) >> 52) & 0xf)
#define RIEf_r2(insn) (((insn) >> 48) & 0xf)
#define RIEf_i3(insn) (((insn) >> 40) & 0xff)
#define RIEf_i4(insn) (((insn) >> 32) & 0xff)
#define RIEf_i5(insn) (((insn) >> 24) & 0xff)
#define RIEa_r1(insn) (((insn) >> 52) & 0xf)
#define RIEa_i2(insn) (((insn) >> 32) & 0xffff)
#define RIEa_m3(insn) (((insn) >> 28) & 0xf)
#define RIEb_r1(insn) (((insn) >> 52) & 0xf)
#define RIEb_r2(insn) (((insn) >> 48) & 0xf)
#define RIEb_i4(insn) (((insn) >> 32) & 0xffff)
#define RIEb_m3(insn) (((insn) >> 28) & 0xf)
#define RIEc_r1(insn) (((insn) >> 52) & 0xf)
#define RIEc_m3(insn) (((insn) >> 48) & 0xf)
#define RIEc_i4(insn) (((insn) >> 32) & 0xffff)
#define RIEc_i2(insn) (((insn) >> 24) & 0xff)
#define RIEg_r1(insn) (((insn) >> 52) & 0xf)
#define RIEg_m3(insn) (((insn) >> 48) & 0xf)
#define RIEg_i2(insn) (((insn) >> 32) & 0xffff)
#define RIL_r1(insn) (((insn) >> 52) & 0xf)
#define RIL_i2(insn) (((insn) >> 16) & 0xffffffff)
#define RSY_r1(insn) (((insn) >> 52) & 0xf)
#define RSY_r3(insn) (((insn) >> 48) & 0xf)
#define RSY_m3(insn) (((insn) >> 48) & 0xf)
#define RSY_b2(insn) (((insn) >> 44) & 0xf)
#define RSY_dl2(insn) (((insn) >> 32) & 0xfff)
#define RSY_dh2(insn) (((insn) >> 24) & 0xff)
#define RXE_r1(insn) (((insn) >> 52) & 0xf)
#define RXE_x2(insn) (((insn) >> 48) & 0xf)
#define RXE_b2(insn) (((insn) >> 44) & 0xf)
#define RXE_d2(insn) (((insn) >> 32) & 0xfff)
#define RXE_m3(insn) (((insn) >> 28) & 0xf)
#define RXY_r1(insn) (((insn) >> 52) & 0xf)
#define RXY_x2(insn) (((insn) >> 48) & 0xf)
#define RXY_b2(insn) (((insn) >> 44) & 0xf)
#define RXY_dl2(insn) (((insn) >> 32) & 0xfff)
#define RXY_dh2(insn) (((insn) >> 24) & 0xff)
#define SSa_l(insn) (((insn) >> 48) & 0xff)
#define SSa_b1(insn) (((insn) >> 44) & 0xf)
#define SSa_d1(insn) (((insn) >> 32) & 0xfff)
#define SSa_b2(insn) (((insn) >> 28) & 0xf)
#define SSa_d2(insn) (((insn) >> 16) & 0xfff)
#define V_rxb(insn) (((insn) >> 24) & 0xf)
#define VRR_v1(insn) (((insn) >> 52) & 0xf)
#define VRR_v2(insn) (((insn) >> 48) & 0xf)
#define VRR_r2(insn) (((insn) >> 48) & 0xf)
#define VRR_r3(insn) (((insn) >> 44) & 0xf)
#define VRR_v3(insn) (((insn) >> 44) & 0xf)
#define VRR_v4(insn) (((insn) >> 28) & 0xf)
#define VRRa_m5(insn) (((insn) >> 36) & 0xf)
#define VRRa_m4(insn) (((insn) >> 32) & 0xf)
#define VRRa_m3(insn) (((insn) >> 28) & 0xf)
#define VRRc_m6(insn) (((insn) >> 36) & 0xf)
#define VRRc_m5(insn) (((insn) >> 32) & 0xf)
#define VRRc_m4(insn) (((insn) >> 28) & 0xf)
#define VRRd_m5(insn) (((insn) >> 40) & 0xf)
#define VRRd_m6(insn) (((insn) >> 36) & 0xf)
#define VRIa_v1(insn) (((insn) >> 52) & 0xf)
#define VRIa_v3(insn) (((insn) >> 48) & 0xf)
#define VRIa_i2(insn) (((insn) >> 32) & 0xffff)
#define VRIa_m3(insn) (((insn) >> 28) & 0xf)
#define VRId_v1(insn) (((insn) >> 52) & 0xf)
#define VRId_v2(insn) (((insn) >> 48) & 0xf)
#define VRId_v3(insn) (((insn) >> 44) & 0xf)
#define VRId_i4(insn) (((insn) >> 32) & 0xff)
#define VRId_m5(insn) (((insn) >> 28) & 0xf)
#define VRSb_v1(insn) (((insn) >> 52) & 0xf)
#define VRSb_r3(insn) (((insn) >> 48) & 0xf)
#define VRSb_b2(insn) (((insn) >> 44) & 0xf)
#define VRSb_d2(insn) (((insn) >> 32) & 0xfff)
#define VRSb_m4(insn) (((insn) >> 28) & 0xf)


/*------------------------------------------------------------*/
/*--- Helpers for constructing IR.                         ---*/
/*------------------------------------------------------------*/

/* Whether or not REGNO designates a valid FPR pair. */
#define is_valid_fpr_pair(regno)   (((regno) & 0x2) == 0)

/* Whether or not REGNO designates an even-odd GPR pair. */
#define is_valid_gpr_pair(regno)   (((regno) & 0x1) == 0)

#define is_valid_rounding_mode(rm) ((rm) < 8 && (rm) != 2)


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

/* Create an expression node for a 128-bit vector constant */
static __inline__ IRExpr *
mkV128(UShort value)
{
   return IRExpr_Const(IRConst_V128(value));
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

/* Return the 64-bit address with the given 32-bit "relative long" offset from
   the current guest instruction being translated. */
static __inline__ Addr64
addr_rel_long(UInt offset)
{
   return guest_IA_rel_base + ((Addr64)(Long)(Int)offset << 1);
}

/* Return the 64-bit address with the given 16-bit "relative" offset from the
   current guest instruction being translated. */
static __inline__ Addr64
addr_relative(UShort offset)
{
   return guest_IA_rel_base + ((Addr64)(Long)(Short)offset << 1);
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

/* A system call */
static void
system_call(IRExpr *sysno)
{
   /* Store the system call number in the pseudo register. */
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_SYSNO), sysno));

   put_IA(mkaddr_expr(guest_IA_next_instr));

   /* It's important that all ArchRegs carry their up-to-date value
      at this point.  So we declare an end-of-block here, which
      forces any TempRegs caching ArchRegs to be flushed. */
   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Sys_syscall;
}

/* An extension */
static void
extension(ULong id, ULong variant)
{
   vassert(id < (1 << S390_EXT_ID_NBITS));
   vassert(variant <= ~((ULong) 0) >> S390_EXT_ID_NBITS);

   /* Store the extension ID in the pseudo register. */
   ULong ext_id = id | (variant << S390_EXT_ID_NBITS);
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_SYSNO), mkU64(ext_id)));

   put_IA(mkaddr_expr(guest_IA_next_instr));

   dis_res->whatNext    = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_Extension;
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
#define s390_insn_assert(cond)                  \
   do {                                         \
      if (!(cond)) {                            \
         dis_res->whatNext = Dis_StopHere;      \
         dis_res->jk_StopHere = Ijk_NoDecode;   \
         return;                                \
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
   case Ity_I1:
      expr = unop(sign_extend ? Iop_1Sto64 : Iop_1Uto64, expr);
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

/* Read a float of given type from an fpr. */
static IRExpr *
get_fpr_float(UInt archreg, IRType type)
{
   if (type == Ity_F128)
      return get_fpr_pair(archreg);
   else
      return IRExpr_Get(fpr_offset(archreg), type);
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
   vassert(m <= 4);

   static const IRType results[] = {Ity_I8, Ity_I16, Ity_I32, Ity_I64, Ity_V128};

   return results[m];
}

/* Determine IRType from instruction's floating-point format field */
static IRType
s390_vr_get_ftype(const UChar m)
{
   vassert(m >= 2 && m <= 4);

   static const IRType results[] = {Ity_F32, Ity_F64, Ity_F128};
   return results[m - 2];
}

/* Determine if Condition Code Set (CS) flag is set in m field */
#define s390_vr_is_cs_set(m) (((m) & 0x1) != 0)

/* Determine if Zero Search (ZS) flag is set in m field */
#define s390_vr_is_zs_set(m) (((m) & 0b0010) != 0)

/* Check if the "Single-Element-Control" bit is set.
   Used in vector FP instructions.
 */
#define s390_vr_is_single_element_control_set(m) (((m) & 0x8) != 0)

/* Return a vector that consists of copies of the same value */
static IRExpr*
s390_V128_fill(IRExpr* elem)
{
   IRType o2type = typeOfIRExpr(irsb->tyenv, elem);
   switch (o2type) {
   case Ity_I8:
      return unop(Iop_Dup8x16, elem);
   case Ity_I16:
      return unop(Iop_Dup16x8, elem);
   case Ity_I32:
      return unop(Iop_Dup32x4, elem);
   case Ity_I64: {
      IRTemp val = newTemp(Ity_I64);
      assign(val, elem);
      return binop(Iop_64HLtoV128, mkexpr(val), mkexpr(val));
   }
   default:
      ppIRType(o2type);
      vpanic("s390_V128_fill: invalid IRType");
   }
}

/* Widen a 64-bit vector, sign- or zero-extending its integer lanes */
static IRExpr*
s390_V128_unpack(IRExpr* a, UChar es, Bool is_signed)
{
   vassert(es <= 3);
   if (es <= 2) {
      const IROp sops[] = {Iop_Widen8Sto16x8, Iop_Widen16Sto32x4,
                           Iop_Widen32Sto64x2};
      const IROp uops[] = {Iop_Widen8Uto16x8, Iop_Widen16Uto32x4,
                           Iop_Widen32Uto64x2};
      return unop((is_signed ? sops : uops)[es], a);
   } else {
      IRTemp  lo = newTemp(Ity_I64);
      IRExpr* hi;
      assign(lo, a);
      if (is_signed) {
         hi = binop(Iop_Sar64, mkexpr(lo), mkU8(63));
      } else {
         hi = mkU64(0);
      }
      return binop(Iop_64HLtoV128, hi, mkexpr(lo));
   }
}

/* Return a vector that consists of copies of a 16-bit integer, sign-extended or
   reduced to the lane size as appropriate */
static IRExpr*
s390_V128_fillnum(Short num, UChar es)
{
   switch (es) {
   case 0:
      return unop(Iop_Dup8x16, mkU8((UChar)num));
   case 1:
      return unop(Iop_Dup16x8, mkU16((UShort)num));
   case 2:
      return unop(Iop_Dup32x4, mkU32((UInt)num));
   case 3:
      return s390_V128_fill(mkU64((ULong)num));
   case 4:
      return binop(Iop_64HLtoV128, mkU64((num < 0) ? -1 : 0),
                   mkU64((ULong)num));
   default:
      vpanic("s390_V128_fillnum: unknown element size");
   }
}

/* Replicate highest bit within each lane */
static IRExpr*
s390_V128_high_set(IRExpr* v1, UChar es)
{
   static const IROp ops[] = {Iop_SarN8x16, Iop_SarN16x8, Iop_SarN32x4,
                              Iop_SarN64x2, Iop_SarV128};
   vassert(es < sizeof(ops) / sizeof(ops[0]));
   UChar bitwidth = 8 << es;
   return binop(ops[es], v1, mkU8(bitwidth - 1));
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

/* Return a == b (lane-wise) for the given element size */
static IRExpr*
s390_V128_CmpEQ(IRExpr *a, IRExpr *b, UChar es)
{
   if (es < 4) {
      const IROp op_cmp[] = {Iop_CmpEQ8x16, Iop_CmpEQ16x8, Iop_CmpEQ32x4,
                             Iop_CmpEQ64x2};
      return binop(op_cmp[es], a, b);
   } else {
      IRTemp t = newTemp(Ity_V128);
      assign(t, binop(Iop_CmpEQ64x2, a, b));
      return binop(Iop_AndV128, s390_V128_fill(unop(Iop_V128to64, mkexpr(t))),
                   s390_V128_fill(unop(Iop_V128HIto64, mkexpr(t))));
   }
}

/* Return a > b for a 128-bit integer in a vector */
static IRExpr*
s390_V128_CmpGT128x1(IRExpr* a, IRExpr* b, Bool is_signed, Bool lsb_only)
{
   IRTemp ta = newTemp(Ity_V128);
   IRTemp tb = newTemp(Ity_V128);
   assign(ta, a);
   assign(tb, b);
   IRExpr* result =
      s390_V128_bitwiseITE(binop(Iop_XorV128, mkexpr(ta), mkexpr(tb)),
                           is_signed ? mkexpr(tb) : mkexpr(ta),
                           binop(Iop_Sub128x1, mkexpr(tb), mkexpr(ta)));
   return binop(lsb_only ? Iop_ShrV128 : Iop_SarV128, result, mkU8(127));
}

/* Return a > b (unsigned, lane-wise) for the given element size */
static IRExpr*
s390_V128_CmpGTU(IRExpr* a, IRExpr* b, UChar es)
{
   if (es < 4) {
      const IROp ops[] = {Iop_CmpGT8Ux16, Iop_CmpGT16Ux8, Iop_CmpGT32Ux4,
                          Iop_CmpGT64Ux2};
      return binop(ops[es], a, b);
   } else {
      return s390_V128_CmpGT128x1(a, b, False, False);
   }
}

/* Return a > b (signed, lane-wise) for the given element size */
static IRExpr*
s390_V128_CmpGTS(IRExpr* a, IRExpr* b, UChar es)
{
   if (es < 4) {
      const IROp ops[] = {Iop_CmpGT8Sx16, Iop_CmpGT16Sx8, Iop_CmpGT32Sx4,
                          Iop_CmpGT64Sx2};
      return binop(ops[es], a, b);
   } else {
      return s390_V128_CmpGT128x1(a, b, True, False);
   }
}

/* For a given vector comparison result, set the condition code accordingly */
static void
s390_V128_setcc_for_cmp(IRTemp cmp_res, UChar es)
{
   IRTemp cc = newTemp(Ity_I64);
   IRTemp lo = newTemp(Ity_I64);
   assign(lo, unop(Iop_V128to64, mkexpr(cmp_res)));
   if (es == 4) {
      // Full-sized single lane
      assign(cc, mkite(binop(Iop_CmpEQ64, mkexpr(lo), mkU64(0)), mkU64(3),
                       mkU64(0)));
   } else {
      IRTemp hi = newTemp(Ity_I64);
      assign(hi, unop(Iop_V128HIto64, mkexpr(cmp_res)));
      IRExpr* allZero =
         binop(Iop_CmpEQ64, binop(Iop_Or64, mkexpr(lo), mkexpr(hi)), mkU64(0));
      IRExpr* allOnes = binop(
         Iop_CmpEQ64, binop(Iop_And64, mkexpr(lo), mkexpr(hi)), mkU64(-1ULL));
      assign(cc, mkite(allZero, mkU64(3), mkite(allOnes, mkU64(0), mkU64(1))));
   }
   s390_cc_set(cc);
}

/* Return a + b for the given element size */
static IRExpr*
s390_V128_add(IRExpr* a, IRExpr* b, UChar es)
{
   const IROp add_op[] = {Iop_Add8x16, Iop_Add16x8, Iop_Add32x4, Iop_Add64x2,
                          Iop_Add128x1};
   vassert(es <= 4);
   return binop(add_op[es], a, b);
}

/* Return a - b for the given element size */
static IRExpr*
s390_V128_sub(IRExpr* a, IRExpr* b, UChar es)
{
   const IROp sub_op[] = {Iop_Sub8x16, Iop_Sub16x8, Iop_Sub32x4, Iop_Sub64x2,
                          Iop_Sub128x1};
   vassert(es <= 4);
   return binop(sub_op[es], a, b);
}

/* Return the two's complement of arg for the given element size */
static IRExpr*
s390_V128_get_complement(IRExpr* arg, UChar es)
{
   return s390_V128_sub(mkV128(0), arg, es);
}

/* Return the carry of a + b + (c & 1) with 128-bit integers */
static IRExpr*
s390_V128_calculate_carry_out_with_carry(IRExpr* a, IRExpr* b, IRExpr* c)
{
   IRTemp ta    = newTemp(Ity_V128);
   IRTemp sum   = newTemp(Ity_V128);
   IRTemp carry = newTemp(Ity_V128);
   assign(ta, a);
   assign(sum, binop(Iop_Add128x1, mkexpr(ta), b));
   assign(carry,
          binop(Iop_AndV128, c, binop(Iop_64HLtoV128, mkU64(0), mkU64(1))));

   return binop(Iop_OrV128,
                s390_V128_CmpGT128x1(mkexpr(ta), mkexpr(sum), False, True),
                binop(Iop_AndV128, mkexpr(carry),
                      s390_V128_CmpEQ(mkexpr(sum), mkV128(0xffff), 4)));
}

/* Multiply lane-wise */
static IRExpr*
s390_V128_mul(IRExpr* a, IRExpr* b, UChar es)
{
   vassert(es <= 4);
   if (es <= 2) {
      const IROp ops[] = {Iop_Mul8x16, Iop_Mul16x8, Iop_Mul32x4};
      return binop(ops[es], a, b);
   }

   IRTemp ta  = newTemp(Ity_V128);
   IRTemp tb  = newTemp(Ity_V128);
   IRTemp a_h = newTemp(Ity_V128);
   IRTemp b_h = newTemp(Ity_V128);
   IRTemp a_l = newTemp(Ity_V128);
   IRTemp b_l = newTemp(Ity_V128);
   assign(ta, a);
   assign(tb, b);
   assign(a_l, unop(Iop_V128to64, mkexpr(ta)));
   assign(b_l, unop(Iop_V128to64, mkexpr(tb)));
   assign(a_h, unop(Iop_V128HIto64, mkexpr(ta)));
   assign(b_h, unop(Iop_V128HIto64, mkexpr(tb)));

   if (es == 3) {
      return binop(Iop_64HLtoV128, binop(Iop_Mul64, mkexpr(a_h), mkexpr(b_h)),
                   binop(Iop_Mul64, mkexpr(a_l), mkexpr(b_l)));
   }

   return binop(
      Iop_Add128x1,
      unop(Iop_ReinterpI128asV128,
           binop(Iop_MullU64, mkexpr(a_l), mkexpr(b_l))),
      binop(Iop_64HLtoV128,
            binop(Iop_Add64, binop(Iop_Mul64, mkexpr(a_h), mkexpr(b_l)),
                  binop(Iop_Mul64, mkexpr(b_h), mkexpr(a_l))),
            mkU64(0)));
}

/* Widening multiply from high (even) or low (odd) */
static IRExpr*
s390_V128_mul_widen(IRExpr* a, IRExpr* b, UChar es, Bool is_signed, Bool is_high)
{
   vassert(es <= 3);
   if (es <= 2) {
      const IROp sops[] = {Iop_MullEven8Sx16, Iop_MullEven16Sx8,
                           Iop_MullEven32Sx4};
      const IROp uops[] = {Iop_MullEven8Ux16, Iop_MullEven16Ux8,
                           Iop_MullEven32Ux4};
      if (is_high) {
         a = binop(Iop_ShrV128, a, mkU8(8 << es));
         b = binop(Iop_ShrV128, b, mkU8(8 << es));
      }
      return binop((is_signed ? sops : uops)[es], a, b);
   }
   IROp to64 = is_high ? Iop_V128HIto64 : Iop_V128to64;
   return unop(Iop_ReinterpI128asV128,
               binop(is_signed ? Iop_MullS64 : Iop_MullU64, unop(to64, a),
                     unop(to64, b)));
}

/* Multiply and optional add, high result */
static IRExpr*
s390_V128_mula_high(IRExpr* a, IRExpr* b, IRExpr* c, UChar es, Bool is_signed)
{
   vassert(es <= 4);

   IRTemp op1 = newTemp(Ity_V128);
   IRTemp op2 = newTemp(Ity_V128);
   IRTemp op3 = newTemp(Ity_V128);
   assign(op1, a);
   assign(op2, b);
   assign(op3, c == NULL ? mkV128(0) : c);

   if (es <= 2) {
      const IROp sops[] = {Iop_MulHi8Sx16, Iop_MulHi16Sx8, Iop_MulHi32Sx4};
      const IROp uops[] = {Iop_MulHi8Ux16, Iop_MulHi16Ux8, Iop_MulHi32Ux4};
      IRExpr*    res =
         binop((is_signed ? sops : uops)[es], mkexpr(op1), mkexpr(op2));

      if (c != NULL) {
         IRTemp low = newTemp(Ity_V128);
         assign(low, s390_V128_add(s390_V128_mul(mkexpr(op1), mkexpr(op2), es),
                                   mkexpr(op3), es));
         res = s390_V128_sub(
            res, s390_V128_CmpGTU(mkexpr(op3), mkexpr(low), es), es);
         if (is_signed) {
            res = s390_V128_add(res, s390_V128_high_set(mkexpr(op3), es), es);
         }
      }
      return res;
   }

   if (es == 3) {
      /* Calculate full (128-bit) results, then concatenate high halves */
      IRExpr* res0 =
         s390_V128_mul_widen(mkexpr(op1), mkexpr(op2), es, is_signed, True);
      IRExpr* res1 =
         s390_V128_mul_widen(mkexpr(op1), mkexpr(op2), es, is_signed, False);

      if (c != NULL) {
         /* Add the appropriately-extended 3rd operand */
         res0 = binop(
            Iop_Add128x1, res0,
            s390_V128_unpack(unop(Iop_V128HIto64, mkexpr(op3)), es, is_signed));
         res1 = binop(
            Iop_Add128x1, res1,
            s390_V128_unpack(unop(Iop_V128to64, mkexpr(op3)), es, is_signed));
      }
      return binop(Iop_64HLtoV128, unop(Iop_V128HIto64, res0),
                   unop(Iop_V128HIto64, res1));
   }

   /* 128-bit input operands */
   IRTemp  ah  = newTemp(Ity_I64);
   IRTemp  al  = newTemp(Ity_I64);
   IRTemp  bh  = newTemp(Ity_I64);
   IRTemp  bl  = newTemp(Ity_I64);
   IRTemp  low = newTemp(Ity_V128);
   IRTemp  m0  = newTemp(Ity_V128);
   IRTemp  mid = newTemp(Ity_V128);
   IRExpr* t;

   assign(ah, unop(Iop_V128HIto64, mkexpr(op1)));
   assign(al, unop(Iop_V128to64, mkexpr(op1)));
   assign(bh, unop(Iop_V128HIto64, mkexpr(op2)));
   assign(bl, unop(Iop_V128to64, mkexpr(op2)));
   // The following addition can overflow; remember `low' for carry
   assign(low, binop(Iop_Add128x1, mkexpr(op3),
                     unop(Iop_ReinterpI128asV128,
                          binop(Iop_MullU64, mkexpr(al), mkexpr(bl)))));
   t = binop(Iop_ShrV128, mkexpr(low), mkU8(64));
   // This addition can't overflow, but the next can, so keep `m0'...
   assign(m0, binop(Iop_Add128x1, t,
                    unop(Iop_ReinterpI128asV128,
                         binop(Iop_MullU64, mkexpr(al), mkexpr(bh)))));
   // ... and the sum `mid'
   assign(mid, binop(Iop_Add128x1, mkexpr(m0),
                     unop(Iop_ReinterpI128asV128,
                          binop(Iop_MullU64, mkexpr(ah), mkexpr(bl)))));
   t = binop(Iop_ShrV128, mkexpr(mid), mkU8(64));
   t = binop(
      Iop_Add128x1, t,
      unop(Iop_ReinterpI128asV128, binop(Iop_MullU64, mkexpr(ah), mkexpr(bh))));
   // Add the carries
   t = binop(Iop_Add128x1, t,
             s390_V128_CmpGT128x1(mkexpr(op3), mkexpr(low), False, True));
   t = binop(Iop_Add128x1, t,
             binop(Iop_ShlV128,
                   s390_V128_CmpGT128x1(mkexpr(m0), mkexpr(mid), False, True),
                   mkU8(64)));
   if (is_signed) {
      t = binop(Iop_Sub128x1, t,
                binop(Iop_AndV128, mkexpr(op1),
                      binop(Iop_SarV128, mkexpr(op2), mkU8(127))));
      t = binop(Iop_Sub128x1, t,
                binop(Iop_AndV128, mkexpr(op2),
                      binop(Iop_SarV128, mkexpr(op1), mkU8(127))));
      t = binop(Iop_Add128x1, t, binop(Iop_SarV128, mkexpr(op3), mkU8(127)));
   }
   return t;
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
   case Ity_F128:
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

   if (type == Ity_F128) {
      IRTemp val = newTemp(Ity_F128);
      assign(val, expr);
      stmt(IRStmt_Put(offset, unop(Iop_F128HItoF64, mkexpr(val))));
      stmt(IRStmt_Put(offset + 8, unop(Iop_F128LOtoF64, mkexpr(val))));
   } else {
      stmt(IRStmt_Put(offset, expr));
   }
}

/* Read type sized part specified by index of a vr register. */
static IRExpr *
get_vr(UInt archreg, IRType type, UChar index)
{
   UInt offset = s390_vr_offset_by_index(archreg, type, index);
   if (type == Ity_F128) {
      return binop(Iop_F64HLtoF128,
                   IRExpr_Get(offset, Ity_F64),
                   IRExpr_Get(offset + 8, Ity_F64));
   }
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

/* Starting from addr, load at most maxIndex + 1 bytes into v1.  Fill the
   leftmost or rightmost bytes of v1, depending on whether `rightmost' is set.
   If maxIndex >= 15, load all 16 bytes; otherwise clear the remaining bytes. */
static void
s390_vr_loadWithLength(UChar v1, IRTemp addr, IRExpr *maxIndex, Bool rightmost)
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
      from the unaligned address.  Then shift the loaded data left- or
      right-aligned into the target vector register. */

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

   IRExpr* chunk = load(Ity_V128, binop(Iop_Sub64, mkexpr(addr), mkexpr(back)));

   /* Shift the loaded 16-byte vector to the right, then to the left, or vice
      versa, where each shift amount ranges from 0 to 120. */
   IRExpr* shift1;
   IRExpr* shift2 = unop(Iop_64to8, binop(Iop_Shl64, mkexpr(zeroed), mkU8(3)));

   if (rightmost) {
      shift1 = unop(Iop_64to8, binop(Iop_Shl64, mkexpr(back), mkU8(3)));
      put_vr_qw(v1, binop(Iop_ShrV128,
                          binop(Iop_ShlV128, chunk, shift1),
                          shift2));
   } else {
      shift1 = unop(Iop_64to8,
                    binop(Iop_Shl64,
                          binop(Iop_Sub64, mkexpr(zeroed), mkexpr(back)),
                          mkU8(3)));
      put_vr_qw(v1, binop(Iop_ShlV128,
                          binop(Iop_ShrV128, chunk, shift1),
                          shift2));
   }
}

/* Store at most maxIndex + 1 bytes from v1 to addr.  Store the leftmost or
   rightmost bytes of v1, depending on whether `rightmost' is set.  If maxIndex
   >= 15, store all 16 bytes. */
static void
s390_vr_storeWithLength(UChar v1, IRTemp addr, IRExpr *maxIndex, Bool rightmost)
{
   IRTemp maxIdx = newTemp(Ity_I32);
   IRTemp cappedMax = newTemp(Ity_I64);
   IRTemp counter = newTemp(Ity_I64);
   IRExpr* offset;

   assign(maxIdx, maxIndex);
   assign(cappedMax, mkite(binop(Iop_CmpLT32U, mkexpr(maxIdx), mkU32(15)),
                           unop(Iop_32Uto64, mkexpr(maxIdx)), mkU64(15)));

   assign(counter, get_counter_dw0());

   if (rightmost)
      offset = binop(Iop_Add64,
                     binop(Iop_Sub64, mkU64(15), mkexpr(cappedMax)),
                     mkexpr(counter));
   else
      offset = mkexpr(counter);

   store(binop(Iop_Add64, mkexpr(addr), mkexpr(counter)),
         binop(Iop_GetElem8x16, get_vr_qw(v1), unop(Iop_64to8, offset)));

   /* Check for end of field */
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   iterate_if(binop(Iop_CmpNE64, mkexpr(counter), mkexpr(cappedMax)));
   put_counter_dw0(mkU64(0));
}

/*------------------------------------------------------------*/
/*--- Rounding modes                                       ---*/
/*------------------------------------------------------------*/

/* Extract the bfp rounding mode from the guest FPC reg and encode it as an
   IRRoundingMode:

   rounding mode   | s390 | IR
   ----------------------------
   to nearest      |  000 | 000
   to zero         |  001 | 011
   to +infinity    |  010 | 010
   to -infinity    |  011 | 001
   prepare shorter |  111 | 101

   So:  IR = (4 - s390) & (3 | s390)
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

   IRExpr *rm_s390 = mkexpr(fpc_bits);

   // rm_IR = (4 - rm_s390) & (3 | rm_s390);
   return binop(Iop_And32,
                binop(Iop_Sub32, mkU32(4), rm_s390),
                binop(Iop_Or32,  mkU32(3), rm_s390));
}

/* Encode the s390 rounding mode as it appears in the m3 field of certain
   instructions to VEX's IRRoundingMode. */
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
s390_format_RIS(void (*irgen)(UChar r1, UChar m3, UChar i2, IRTemp op4addr),
                ULong ovl)
{
   UChar  r1      = (ovl >> 52) & 0xf;
   UChar  m3      = (ovl >> 48) & 0xf;
   UChar  b4      = (ovl >> 44) & 0xf;
   UShort d4      = (ovl >> 32) & 0xfff;
   UChar  i2      = (ovl >> 24) & 0xff;
   IRTemp op4addr = newTemp(Ity_I64);

   assign(op4addr,
          binop(Iop_Add64, mkU64(d4), b4 != 0 ? get_gpr_dw0(b4) : mkU64(0)));

   irgen(r1, m3, i2, op4addr);
}

static void
s390_format_RRS(void (*irgen)(UChar r1, UChar r2, UChar m3, IRTemp op4addr),
                ULong ovl)
{
   UChar  r1      = (ovl >> 52) & 0xf;
   UChar  r2      = (ovl >> 48) & 0xf;
   UChar  b4      = (ovl >> 44) & 0xf;
   UShort d4      = (ovl >> 32) & 0xfff;
   UChar  m3      = (ovl >> 28) & 0xf;
   IRTemp op4addr = newTemp(Ity_I64);

   assign(op4addr, binop(Iop_Add64, mkU64(d4), b4 != 0 ? get_gpr_dw0(b4) :
          mkU64(0)));

   irgen(r1, r2, m3, op4addr);
}

static void
s390_format_RS0(void (*irgen)(UChar r1, IRTemp op2addr), ULong ovl)
{
   UChar  r1      = RS_r1(ovl);
   UChar  b2      = RS_b2(ovl);
   UShort d2      = RS_d2(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(r1, op2addr);
}

static void
s390_format_RS(void (*irgen)(UChar r1, UChar r3, IRTemp op2addr), ULong ovl)
{
   UChar  r1      = RS_r1(ovl);
   UChar  r3      = RS_r3(ovl);
   UChar  b2      = RS_b2(ovl);
   UShort d2      = RS_d2(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(r1, r3, op2addr);
}

static void
s390_format_RSY(void (*irgen)(UChar r1, UChar r3, IRTemp op2addr), ULong ovl)
{
   UChar  r1      = RSY_r1(ovl);
   UChar  r3      = RSY_r3(ovl);
   UChar  b2      = RSY_b2(ovl);
   UShort dl2     = RSY_dl2(ovl);
   UChar  dh2     = RSY_dh2(ovl);
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2      = newTemp(Ity_I64);

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, mkexpr(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(r1, r3, op2addr);
}

static void
s390_format_RSYcond(void (*irgen)(UChar r1, IRTemp op2addr), ULong ovl)
{
   UChar  r1      = RSY_r1(ovl);
   UChar  m3      = RSY_m3(ovl);
   UChar  b2      = RSY_b2(ovl);
   UShort dl2     = RSY_dl2(ovl);
   UChar  dh2     = RSY_dh2(ovl);
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2 = newTemp(Ity_I64);

   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, mkexpr(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(r1, op2addr);

   vassert(dis_res->whatNext == Dis_Continue);
}

static void
s390_format_RX(void (*irgen)(UChar r1, IRTemp op2addr), ULong ovl)
{
   UChar  r1      = (ovl >> 20) & 0xf;
   UChar  x2      = (ovl >> 16) & 0xf;
   UChar  b2      = (ovl >> 12) & 0xf;
   UShort d2      = ovl & 0xfff;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   irgen(r1, op2addr);
}

static void
s390_format_RXE0(void (*irgen)(UChar r1, IRTemp op2addr), ULong ovl)
{
   UChar  r1      = RXE_r1(ovl);
   UChar  x2      = RXE_x2(ovl);
   UChar  b2      = RXE_b2(ovl);
   UShort d2      = RXE_d2(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   irgen(r1, op2addr);
}

static void
s390_format_RXE(void (*irgen)(UChar r1, IRTemp op2addr, UChar m3), ULong ovl)
{
   UChar  r1      = RXE_r1(ovl);
   UChar  x2      = RXE_x2(ovl);
   UChar  b2      = RXE_b2(ovl);
   UShort d2      = RXE_d2(ovl);
   UChar  m3      = RXE_m3(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   irgen(r1, op2addr, m3);
}

static void
s390_format_RXF(void (*irgen)(UChar, IRTemp, UChar), ULong ovl)
{
   UChar  r3      = (ovl >> 52) & 0xf;
   UChar  x2      = (ovl >> 48) & 0xf;
   UChar  b2      = (ovl >> 44) & 0xf;
   UShort d2      = (ovl >> 32) & 0xfff;
   UChar  r1      = (ovl >> 28) & 0xf;
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   irgen(r3, op2addr, r1);
}

static void
s390_format_RXY(void (*irgen)(UChar r1, IRTemp op2addr), ULong ovl)
{
   UChar  r1      = (ovl >> 52) & 0xf;
   UChar  x2      = (ovl >> 48) & 0xf;
   UChar  b2      = (ovl >> 44) & 0xf;
   UShort dl2     = (ovl >> 32) & 0xfff;
   UChar  dh2     = (ovl >> 24) & 0xff;
   IRTemp op2addr = newTemp(Ity_I64);
   IRTemp d2      = newTemp(Ity_I64);

   assign(d2, mkU64(((ULong)(Long)(Char)dh2 << 12) | ((ULong)dl2)));
   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkexpr(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   irgen(r1, op2addr);
}

static void
s390_format_RXYc(
   void (*irgen)(UChar r1, UChar x2, UChar b2, UInt dx), ULong ovl)
{
   UChar  r1   = (ovl >> 52) & 0xf;
   UChar  x2   = (ovl >> 48) & 0xf;
   UChar  b2   = (ovl >> 44) & 0xf;
   UShort dxl2 = (ovl >> 32) & 0xfff;
   UChar  dxh2 = (ovl >> 24) & 0xff;
   UInt   dx   = (((UInt)(Int)(Char)dxh2) << 12) | dxl2;

   irgen(r1, x2, b2, dx);
}

static void
s390_format_S(void (*irgen)(IRTemp op2addr), ULong ovl)
{
   UChar  b2      = S_b2(ovl);
   UShort d2      = S_d2(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(op2addr);
}

static void
s390_format_SI(void (*irgen)(UChar i2, IRTemp op1addr), ULong ovl)
{
   UChar  i2      = (ovl >> 16) & 0xff;
   UChar  b1      = (ovl >> 12) & 0xf;
   UShort d1      = ovl & 0xfff;
   IRTemp op1addr = newTemp(Ity_I64);

   assign(op1addr, binop(Iop_Add64, mkU64(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));

   irgen(i2, op1addr);
}

static void
s390_format_SIY(void (*irgen)(UChar i2, IRTemp op1addr), ULong ovl)
{
   UChar  i2      = (ovl >> 48) & 0xff;
   UChar  b1      = (ovl >> 44) & 0xf;
   UShort dl1     = (ovl >> 32) & 0xfff;
   UChar  dh1     = (ovl >> 24) & 0xff;
   IRTemp op1addr = newTemp(Ity_I64);
   IRTemp d1      = newTemp(Ity_I64);

   assign(d1, mkU64(((ULong)(Long)(Char)dh1 << 12) | ((ULong)dl1)));
   assign(op1addr, binop(Iop_Add64, mkexpr(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));

   irgen(i2, op1addr);
}

static void
s390_format_SMI(void (*irgen)(UChar m1, UShort i2, IRTemp op3addr), ULong ovl)
{
   UChar  m1      = (ovl >> 52) & 0xf;
   UChar  b3      = (ovl >> 44) & 0xf;
   UShort d3      = (ovl >> 32) & 0xfff;
   UShort i2      = (ovl >> 16) & 0xffff;
   IRTemp op3addr = newTemp(Ity_I64);

   assign(op3addr,
          binop(Iop_Add64, mkU64(d3), b3 != 0 ? get_gpr_dw0(b3) : mkU64(0)));

   irgen(m1, i2, op3addr);
}

static void
s390_format_SSa(void (*irgen)(UChar, IRTemp, IRTemp), ULong ovl)
{
   UChar  l       = SSa_l(ovl);
   UChar  b1      = SSa_b1(ovl);
   UShort d1      = SSa_d1(ovl);
   UChar  b2      = SSa_b2(ovl);
   UShort d2      = SSa_d2(ovl);
   IRTemp op1addr = newTemp(Ity_I64);
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op1addr, binop(Iop_Add64, mkU64(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));
   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(l, op1addr, op2addr);
}

static void
s390_format_SSE(void (*irgen)(IRTemp, IRTemp), ULong ovl)
{
   UChar  b1      = SSa_b1(ovl);
   UShort d1      = SSa_d1(ovl);
   UChar  b2      = SSa_b2(ovl);
   UShort d2      = SSa_d2(ovl);
   IRTemp op1addr = newTemp(Ity_I64);
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op1addr, binop(Iop_Add64, mkU64(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));
   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(op1addr, op2addr);
}

static void
s390_format_SIL(void (*irgen)(UShort i2, IRTemp op1addr), ULong ovl)
{
   UChar  b1      = (ovl >> 44) & 0xf;
   UShort d1      = (ovl >> 32) & 0xfff;
   UShort i2      = (ovl >> 16) & 0xffff;
   IRTemp op1addr = newTemp(Ity_I64);

   assign(op1addr, binop(Iop_Add64, mkU64(d1), b1 != 0 ? get_gpr_dw0(b1) :
          mkU64(0)));

   irgen(i2, op1addr);
}

static void
s390_format_VRX(void (*irgen)(UChar v1, IRTemp op2addr, UChar m3), ULong ovl)
{
   UChar  v1      = (ovl >> 52) & 0xf;
   UChar  x2      = (ovl >> 48) & 0xf;
   UChar  b2      = (ovl >> 44) & 0xf;
   UShort d2      = (ovl >> 32) & 0xfff;
   UChar  m3      = (ovl >> 28) & 0xf;
   UChar  rxb     = V_rxb(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2 != 0 ? get_gpr_dw0(x2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   irgen(v1, op2addr, m3);
}


static void
s390_format_VRRa0(void (*irgen)(UChar v1, UChar v2), ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   irgen(v1, v2);
}


static void
s390_format_VRRc0(void (*irgen)(UChar v1, UChar v2, UChar v3), ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar v3  = VRR_v3(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   irgen(v1, v2, v3);
}

static void
s390_format_VRRc1(void (*irgen)(UChar v1, UChar v2, UChar v3, UChar m4),
                  ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar v3  = VRR_v3(ovl);
   UChar m4  = VRRc_m4(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   irgen(v1, v2, v3, m4);
}

static void
s390_format_VRRb(
   void (*irgen)(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5), ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar v3  = VRR_v3(ovl);
   UChar m4  = (ovl >> 28) & 0xf;
   UChar m5  = (ovl >> 36) & 0xf;
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   irgen(v1, v2, v3, m4, m5);
}

static void
s390_format_VRRe0(void (*irgen)(UChar v1, UChar v2, UChar v3, UChar v4),
                  ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar v3  = VRR_v3(ovl);
   UChar v4  = VRR_v4(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   v4  = s390_vr_getVRindex(v4, 4, rxb);
   irgen(v1, v2, v3, v4);
}

static void
s390_format_VRRf(void (*irgen)(UChar v1, UChar r2, UChar r3), ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar r2  = VRR_r2(ovl);
   UChar r3  = VRR_r3(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   irgen(v1, r2, r3);
}


static void
s390_format_VRRa1(void (*irgen)(UChar v1, UChar v2, UChar m3), ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar m3  = VRRa_m3(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   irgen(v1, v2, m3);
}


static void
s390_format_VRIa0(void (*irgen)(UChar v1, UShort i2), ULong ovl)
{
   UChar  v1  = VRIa_v1(ovl);
   UShort i2  = VRIa_i2(ovl);
   UChar  rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   irgen(v1, i2);
}


static void
s390_format_VRIb(void (*irgen)(UChar v1, UChar i2, UChar i3, UChar m4),
                 ULong ovl)
{
   UChar  v1  = (ovl >> 52) & 0xf;
   UShort i2  = (ovl >> 40) & 0xff;
   UChar  i3  = (ovl >> 32) & 0xff;
   UChar  m4  = (ovl >> 28) & 0xf;
   UChar  rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   irgen(v1, i2, i3, m4);
}


static void
s390_format_VRIa(void (*irgen)(UChar v1, UShort i2, UChar m3), ULong ovl)
{
   UChar  v1  = VRIa_v1(ovl);
   UShort i2  = VRIa_i2(ovl);
   UChar  m3  = VRIa_m3(ovl);
   UChar  rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   irgen(v1, i2, m3);
}


static void
s390_format_VRIc(void (*irgen)(UChar v1, UChar v3, UShort i2, UChar m4),
                 ULong ovl)
{
   UChar  v1  = (ovl >> 52) & 0xf;
   UChar  v3  = (ovl >> 48) & 0xf;
   UShort i2  = (ovl >> 32) & 0xffff;
   UChar  m4  = (ovl >> 28) & 0xf;
   UChar  rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v3  = s390_vr_getVRindex(v3, 2, rxb);
   irgen(v1, v3, i2, m4);
}

static void
s390_format_VRIe(
   void (*irgen)(UChar v1, UChar v2, UShort i3, UChar m4, UChar m5), ULong ovl)
{
   UChar  v1  = (ovl >> 52) & 0xf;
   UChar  v2  = (ovl >> 48) & 0xf;
   UShort i3  = (ovl >> 36) & 0xfff;
   UChar  m4  = (ovl >> 28) & 0xf;
   UChar  m5  = (ovl >> 32) & 0xf;
   UChar  rxb = V_rxb(ovl);

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   irgen(v1, v2, i3, m4, m5);
}

static void
s390_format_VRSc(void (*irgen)(UChar r1, IRTemp op2addr, UChar v3, UChar m4),
                 ULong ovl)
{
   UChar  r1      = (ovl >> 52) & 0xf;
   UChar  b2      = (ovl >> 44) & 0xf;
   UShort d2      = (ovl >> 32) & 0xfff;
   UChar  v3      = (ovl >> 48) & 0xf;
   UChar  m4      = (ovl >> 28) & 0xf;
   UChar  rxb     = V_rxb(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v3  = s390_vr_getVRindex(v3, 2, rxb);
   irgen(r1, op2addr, v3, m4);
}

static void
s390_format_VRSd(void (*irgen)(UChar v1, UChar r3, IRTemp op2addr), ULong ovl)
{
   UChar  r3      = (ovl >> 48) & 0xf;
   UChar  b2      = (ovl >> 44) & 0xf;
   UShort d2      = (ovl >> 32) & 0xfff;
   UChar  v1      = (ovl >> 28) & 0xf;
   UChar  rxb     = V_rxb(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 4, rxb);
   irgen(v1, r3, op2addr);
}


static void
s390_format_VRSa(void (*irgen)(UChar v1, IRTemp op2addr, UChar v3, UChar m4),
                 ULong ovl)
{
   UChar  v1      = (ovl >> 52) & 0xf;
   UChar  b2      = (ovl >> 44) & 0xf;
   UShort d2      = (ovl >> 32) & 0xfff;
   UChar  v3      = (ovl >> 48) & 0xf;
   UChar  m4      = (ovl >> 28) & 0xf;
   UChar  rxb     = V_rxb(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v3  = s390_vr_getVRindex(v3, 2, rxb);
   irgen(v1, op2addr, v3, m4);
}


static void
s390_format_VRSbm(void (*irgen)(UChar v1, IRTemp op2addr, UChar r3, UChar m4),
                  ULong ovl)
{
   UChar  v1      = VRSb_v1(ovl);
   UChar  b2      = VRSb_b2(ovl);
   UShort d2      = VRSb_d2(ovl);
   UChar  r3      = VRSb_r3(ovl);
   UChar  m4      = VRSb_m4(ovl);
   UChar  rxb     = V_rxb(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   irgen(v1, op2addr, r3, m4);
}


static void
s390_format_VRSb(void (*irgen)(UChar v1, IRTemp op2addr, UChar r3), ULong ovl)
{
   UChar  v1      = VRSb_v1(ovl);
   UChar  b2      = VRSb_b2(ovl);
   UShort d2      = VRSb_d2(ovl);
   UChar  r3      = VRSb_r3(ovl);
   UChar  rxb     = V_rxb(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   irgen(v1, op2addr, r3);
}


static void
s390_format_VRV(void (*irgen)(UChar v1, IRTemp op2addr, UChar m3), ULong ovl,
                IRType type)
{
   UChar  v1      = (ovl >> 52) & 0xf;
   UChar  v2      = (ovl >> 48) & 0xf;
   UChar  b2      = (ovl >> 44) & 0xf;
   UShort d2      = (ovl >> 32) & 0xfff;
   UChar  m3      = (ovl >> 28) & 0xf;
   UChar  rxb     = V_rxb(ovl);
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
      s390_insn_assert(m3 < 4);
      x2 = unop(Iop_32Uto64, get_vr(v2, type, m3));
   } else {
      s390_insn_assert(m3 < 2);
      x2 = get_vr(v2, type, m3);
   }

   assign(op2addr, binop(Iop_Add64, binop(Iop_Add64, mkU64(d2),
          b2 != 0 ? get_gpr_dw0(b2) : mkU64(0)), x2));

   irgen(v1, op2addr, m3);
}

static void
s390_format_VRRd(
   void (*irgen)(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6),
   ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar v3  = VRR_v3(ovl);
   UChar v4  = VRR_v4(ovl);
   UChar m5  = VRRd_m5(ovl);
   UChar m6  = VRRd_m6(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   v4  = s390_vr_getVRindex(v4, 4, rxb);
   irgen(v1, v2, v3, v4, m5, m6);
}

static void
s390_format_VRRe(
   void (*irgen)(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6),
   ULong ovl)
{
   UChar v1  = (ovl >> 52) & 0xf;
   UChar v2  = (ovl >> 48) & 0xf;
   UChar v3  = (ovl >> 44) & 0xf;
   UChar m6  = (ovl >> 40) & 0xf;
   UChar m5  = (ovl >> 32) & 0xf;
   UChar v4  = (ovl >> 28) & 0xf;
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   v4  = s390_vr_getVRindex(v4, 4, rxb);
   irgen(v1, v2, v3, v4, m5, m6);
}

static void
s390_format_VRIdm(
   void (*irgen)(UChar v1, UChar v2, UChar v3, UChar i4, UChar m5), ULong ovl)
{
   UChar v1  = VRId_v1(ovl);
   UChar v2  = VRId_v2(ovl);
   UChar v3  = VRId_v3(ovl);
   UChar i4  = VRId_i4(ovl);
   UChar m5  = VRId_m5(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   irgen(v1, v2, v3, i4, m5);
}

static void
s390_format_VRId(void (*irgen)(UChar v1, UChar v2, UChar v3, UChar i4),
                 ULong ovl)
{
   UChar v1  = VRId_v1(ovl);
   UChar v2  = VRId_v2(ovl);
   UChar v3  = VRId_v3(ovl);
   UChar i4  = VRId_i4(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   irgen(v1, v2, v3, i4);
}

static void
s390_format_VRIk(
   void (*irgen)(UChar v1, UChar v2, UChar v3, UChar v4, UChar i5), ULong ovl)
{
   UChar v1  = (ovl >> 52) & 0xf;
   UChar v2  = (ovl >> 48) & 0xf;
   UChar v3  = (ovl >> 44) & 0xf;
   UChar v4  = (ovl >> 28) & 0xf;
   UChar i5  = (ovl >> 32) & 0xff;
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   v4  = s390_vr_getVRindex(v4, 4, rxb);
   irgen(v1, v2, v3, v4, i5);
}

static void
s390_format_VRRd1(
   void (*irgen)(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5), ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar v3  = VRR_v3(ovl);
   UChar v4  = VRR_v4(ovl);
   UChar m5  = VRRd_m5(ovl);
   UChar rxb = V_rxb(ovl);

   if (! s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1  = s390_vr_getVRindex(v1, 1, rxb);
   v2  = s390_vr_getVRindex(v2, 2, rxb);
   v3  = s390_vr_getVRindex(v3, 3, rxb);
   v4  = s390_vr_getVRindex(v4, 4, rxb);
   irgen(v1, v2, v3, v4, m5);
}

static void
s390_format_VRRa(
   void (*irgen)(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5), ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar m3  = VRRa_m3(ovl);
   UChar m4  = VRRa_m4(ovl);
   UChar m5  = VRRa_m5(ovl);
   UChar rxb = V_rxb(ovl);

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   irgen(v1, v2, m3, m4, m5);
}

static void
s390_format_VRRc2(
   void (*irgen)(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5), ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar v3  = VRR_v3(ovl);
   UChar m4  = VRRc_m4(ovl);
   UChar m5  = VRRc_m5(ovl);
   UChar rxb = V_rxb(ovl);

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   v3 = s390_vr_getVRindex(v3, 3, rxb);
   irgen(v1, v2, v3, m4, m5);
}

static void
s390_format_VRRa2(void (*irgen)(UChar v1, UChar v2, UChar m3, UChar m4),
                      ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar m3  = VRRa_m3(ovl);
   UChar m4  = VRRa_m4(ovl);
   UChar rxb = V_rxb(ovl);

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   irgen(v1, v2, m3, m4);
}

static void
s390_format_VRRc(
   void (*irgen)(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6),
   ULong ovl)
{
   UChar v1  = VRR_v1(ovl);
   UChar v2  = VRR_v2(ovl);
   UChar v3  = VRR_v3(ovl);
   UChar m4  = VRRc_m4(ovl);
   UChar m5  = VRRc_m5(ovl);
   UChar m6  = VRRc_m6(ovl);
   UChar rxb = V_rxb(ovl);

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 1, rxb);
   v2 = s390_vr_getVRindex(v2, 2, rxb);
   v3 = s390_vr_getVRindex(v3, 3, rxb);
   irgen(v1, v2, v3, m4, m5, m6);
}

static void
s390_format_VSI(void (*irgen)(UChar v1, IRTemp op2addr, UChar i3), ULong ovl)
{
   UChar  i3      = (ovl >> 48) & 0xff;
   UChar  b2      = (ovl >> 44) & 0xf;
   UShort d2      = (ovl >> 32) & 0xfff;
   UChar  v1      = (ovl >> 28) & 0xf;
   UChar  rxb     = V_rxb(ovl);
   IRTemp op2addr = newTemp(Ity_I64);

   if (!s390_host_has_vx) {
      emulation_failure(EmFail_S390X_vx);
      return;
   }

   v1 = s390_vr_getVRindex(v1, 4, rxb);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   irgen(v1, op2addr, i3);
}

/*------------------------------------------------------------*/
/*--- Build IR for opcodes                                 ---*/
/*------------------------------------------------------------*/

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_ALSIHN(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp result = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   op2 = i2;
   assign(result, binop(Iop_Add32, mkexpr(op1), mkU32(op2)));
   put_gpr_w0(r1, mkexpr(result));
}

static void
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
}

static void
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
}

/* Helper for bitwise logical instructions with two 32-bit input operands and a
   32-bit output operand.  `inv3' and `inv' indicate whether to invert (build
   bitwise complement of) operand 3 or the result, respectively. */
static void
s390_irgen_logicalK32(UChar r3, UChar r1, UChar r2,
                      IROp op, Bool inv3, Bool inv)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   IRExpr* tmp = binop(op, mkexpr(op2),
                       inv3 ? unop(Iop_Not32, mkexpr(op3)) : mkexpr(op3));
   assign(result, inv ? unop(Iop_Not32, tmp) : tmp);
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_w1(r1, mkexpr(result));
}

/* Same as s390_irgen_logicalK32, but for 64-bit operands. */
static void
s390_irgen_logicalK64(UChar r3, UChar r1, UChar r2,
                      IROp op, Bool inv3, Bool inv)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   IRExpr* tmp = binop(op, mkexpr(op2),
                       inv3 ? unop(Iop_Not64, mkexpr(op3)) : mkexpr(op3));
   assign(result, inv ? unop(Iop_Not64, tmp) : tmp);
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, result);
   put_gpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_NRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK32(r3, r1, r2, Iop_And32, False, False);
}

static void
s390_irgen_NGRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK64(r3, r1, r2, Iop_And64, False, False);
}

static void
s390_irgen_NCRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK32(r3, r1, r2, Iop_And32, True, False);
}

static void
s390_irgen_NCGRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK64(r3, r1, r2, Iop_And64, True, False);
}

static void
s390_irgen_NNRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK32(r3, r1, r2, Iop_And32, False, True);
}

static void
s390_irgen_NNGRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK64(r3, r1, r2, Iop_And64, False, True);
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_BASR(UChar r1, UChar r2)
{
   IRTemp target = newTemp(Ity_I64);

   if (r2 == 0) {
      put_gpr_dw0(r1, mkU64(guest_IA_next_instr));
   } else {
      if (r1 != r2) {
         put_gpr_dw0(r1, mkU64(guest_IA_next_instr));
         call_function(get_gpr_dw0(r2));
      } else {
         assign(target, get_gpr_dw0(r2));
         put_gpr_dw0(r1, mkU64(guest_IA_next_instr));
         call_function(mkexpr(target));
      }
   }
}

static void
s390_irgen_BAS(UChar r1, IRTemp op2addr)
{
   IRTemp target = newTemp(Ity_I64);

   put_gpr_dw0(r1, mkU64(guest_IA_next_instr));
   assign(target, mkexpr(op2addr));
   call_function(mkexpr(target));
}

static void
s390_irgen_BCR(UChar m1, UChar r2)
{
   IRTemp cond = newTemp(Ity_I32);

   if (r2 == 0 && (m1 >= 14)) {    /* serialization */
      stmt(IRStmt_MBE(Imbe_Fence));
   }

   if ((r2 == 0) || (m1 == 0)) {
   } else {
      if (m1 == 15) {
         return_from_function(get_gpr_dw0(r2));
      } else {
         assign(cond, s390_call_calculate_cond(m1));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    get_gpr_dw0(r2));
      }
   }
}

static void
s390_irgen_BC(UChar m1, IRTemp op2addr)
{
   IRTemp cond = newTemp(Ity_I32);

   if (m1 == 0) {
   } else {
      if (m1 == 15) {
         always_goto(mkexpr(op2addr));
      } else {
         assign(cond, s390_call_calculate_cond(m1));
         if_condition_goto_computed(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                                    mkexpr(op2addr));
      }
   }
}

static void
s390_irgen_BCTR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, binop(Iop_Sub32, get_gpr_w1(r1), mkU32(1)));
   if (r2 != 0) {
      if_condition_goto_computed(binop(Iop_CmpNE32, get_gpr_w1(r1), mkU32(0)),
                                 get_gpr_dw0(r2));
   }
}

static void
s390_irgen_BCTGR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, binop(Iop_Sub64, get_gpr_dw0(r1), mkU64(1)));
   if (r2 != 0) {
      if_condition_goto_computed(binop(Iop_CmpNE64, get_gpr_dw0(r1), mkU64(0)),
                                 get_gpr_dw0(r2));
   }
}

static void
s390_irgen_BCT(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, binop(Iop_Sub32, get_gpr_w1(r1), mkU32(1)));
   if_condition_goto_computed(binop(Iop_CmpNE32, get_gpr_w1(r1), mkU32(0)),
                              mkexpr(op2addr));
}

static void
s390_irgen_BCTG(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, binop(Iop_Sub64, get_gpr_dw0(r1), mkU64(1)));
   if_condition_goto_computed(binop(Iop_CmpNE64, get_gpr_dw0(r1), mkU64(0)),
                              mkexpr(op2addr));
}

static void
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
}

static void
s390_irgen_BXH(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_I32);

   assign(value, get_gpr_w1(r3 | 1));
   put_gpr_w1(r1, binop(Iop_Add32, get_gpr_w1(r1), get_gpr_w1(r3)));
   if_condition_goto_computed(binop(Iop_CmpLT32S, mkexpr(value),
                                    get_gpr_w1(r1)), mkexpr(op2addr));
}

static void
s390_irgen_BXHG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_I64);

   assign(value, get_gpr_dw0(r3 | 1));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), get_gpr_dw0(r3)));
   if_condition_goto_computed(binop(Iop_CmpLT64S, mkexpr(value),
                                    get_gpr_dw0(r1)), mkexpr(op2addr));
}

static void
s390_irgen_BXLE(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_I32);

   assign(value, get_gpr_w1(r3 | 1));
   put_gpr_w1(r1, binop(Iop_Add32, get_gpr_w1(r1), get_gpr_w1(r3)));
   if_condition_goto_computed(binop(Iop_CmpLE32S, get_gpr_w1(r1),
                                    mkexpr(value)), mkexpr(op2addr));
}

static void
s390_irgen_BXLEG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_I64);

   assign(value, get_gpr_dw0(r3 | 1));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), get_gpr_dw0(r3)));
   if_condition_goto_computed(binop(Iop_CmpLE64S, get_gpr_dw0(r1),
                                    mkexpr(value)), mkexpr(op2addr));
}

static void
s390_irgen_BRAS(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(guest_IA_next_instr));
   call_function(mkaddr_expr(addr_relative(i2)));
}

static void
s390_irgen_BRASL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64(guest_IA_next_instr));
   call_function(mkaddr_expr(addr_rel_long(i2)));
}

static void
s390_irgen_BRC(UChar m1, UShort i2)
{
   IRTemp cond = newTemp(Ity_I32);

   if (m1 == 0) {
   } else {
      if (m1 == 15) {
         always_goto(mkaddr_expr(addr_relative(i2)));
      } else {
         assign(cond, s390_call_calculate_cond(m1));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i2));

      }
   }
}

static void
s390_irgen_BRCL(UChar m1, UInt i2)
{
   IRTemp cond = newTemp(Ity_I32);

   if (m1 == 0) {
   } else {
      if (m1 == 15) {
         always_goto(mkaddr_expr(addr_rel_long(i2)));
      } else {
         assign(cond, s390_call_calculate_cond(m1));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_rel_long(i2));
      }
   }
}

static void
s390_irgen_BRCT(UChar r1, UShort i2)
{
   put_gpr_w1(r1, binop(Iop_Sub32, get_gpr_w1(r1), mkU32(1)));
   if_condition_goto(binop(Iop_CmpNE32, get_gpr_w1(r1), mkU32(0)),
                     addr_relative(i2));
}

static void
s390_irgen_BRCTH(UChar r1, UInt i2)
{
   put_gpr_w0(r1, binop(Iop_Sub32, get_gpr_w0(r1), mkU32(1)));
   if_condition_goto(binop(Iop_CmpNE32, get_gpr_w0(r1), mkU32(0)),
                     addr_relative(i2));
}

static void
s390_irgen_BRCTG(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, binop(Iop_Sub64, get_gpr_dw0(r1), mkU64(1)));
   if_condition_goto(binop(Iop_CmpNE64, get_gpr_dw0(r1), mkU64(0)),
                     addr_relative(i2));
}

static void
s390_irgen_BRXH(UChar r1, UChar r3, UShort i2)
{
   IRTemp value = newTemp(Ity_I32);

   assign(value, get_gpr_w1(r3 | 1));
   put_gpr_w1(r1, binop(Iop_Add32, get_gpr_w1(r1), get_gpr_w1(r3)));
   if_condition_goto(binop(Iop_CmpLT32S, mkexpr(value), get_gpr_w1(r1)),
                     addr_relative(i2));
}

static void
s390_irgen_BRXHG(UChar r1, UChar r3, UShort i2)
{
   IRTemp value = newTemp(Ity_I64);

   assign(value, get_gpr_dw0(r3 | 1));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), get_gpr_dw0(r3)));
   if_condition_goto(binop(Iop_CmpLT64S, mkexpr(value), get_gpr_dw0(r1)),
                     addr_relative(i2));
}

static void
s390_irgen_BRXLE(UChar r1, UChar r3, UShort i2)
{
   IRTemp value = newTemp(Ity_I32);

   assign(value, get_gpr_w1(r3 | 1));
   put_gpr_w1(r1, binop(Iop_Add32, get_gpr_w1(r1), get_gpr_w1(r3)));
   if_condition_goto(binop(Iop_CmpLE32S, get_gpr_w1(r1), mkexpr(value)),
                     addr_relative(i2));
}

static void
s390_irgen_BRXLG(UChar r1, UChar r3, UShort i2)
{
   IRTemp value = newTemp(Ity_I64);

   assign(value, get_gpr_dw0(r3 | 1));
   put_gpr_dw0(r1, binop(Iop_Add64, get_gpr_dw0(r1), get_gpr_dw0(r3)));
   if_condition_goto(binop(Iop_CmpLE64S, get_gpr_dw0(r1), mkexpr(value)),
                     addr_relative(i2));
}

static void
s390_irgen_CR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_C(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;

   assign(op1, get_gpr_w1(r1));
   op2 = (Int)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));
}

static void
s390_irgen_CGFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;

   assign(op1, get_gpr_dw0(r1));
   op2 = (Long)(Int)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));
}

static void
s390_irgen_CRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkU64(addr_rel_long(i2))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CGRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkU64(addr_rel_long(i2))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CGFRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkU64(addr_rel_long(i2)))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
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
}

static void
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
}

static void
s390_irgen_CRJ(UChar r1, UChar r2, UShort i4, UChar m3)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkaddr_expr(addr_relative(i4)));
      } else {
         assign(op1, get_gpr_w1(r1));
         assign(op2, get_gpr_w1(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i4));
      }
   }
}

static void
s390_irgen_CGRJ(UChar r1, UChar r2, UShort i4, UChar m3)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkaddr_expr(addr_relative(i4)));
      } else {
         assign(op1, get_gpr_dw0(r1));
         assign(op2, get_gpr_dw0(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i4));
      }
   }
}

static void
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
}

static void
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
}

static void
s390_irgen_CIJ(UChar r1, UChar m3, UShort i4, UChar i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkaddr_expr(addr_relative(i4)));
      } else {
         assign(op1, get_gpr_w1(r1));
         op2 = (Int)(Char)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE, op1,
                                              mktemp(Ity_I32, mkU32((UInt)op2))));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i4));
      }
   }
}

static void
s390_irgen_CGIJ(UChar r1, UChar m3, UShort i4, UChar i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkaddr_expr(addr_relative(i4)));
      } else {
         assign(op1, get_gpr_dw0(r1));
         op2 = (Long)(Char)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_SIGNED_COMPARE, op1,
                                              mktemp(Ity_I64, mkU64((ULong)op2))));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i4));
      }
   }
}

static void
s390_irgen_CH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CHY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CGH(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_16Sto64, load(Ity_I16, mkexpr(op2addr))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CHI(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;

   assign(op1, get_gpr_w1(r1));
   op2 = (Int)(Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));
}

static void
s390_irgen_CGHI(UChar r1, UShort i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;

   assign(op1, get_gpr_dw0(r1));
   op2 = (Long)(Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));
}

static void
s390_irgen_CHHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I16);
   Short op2;

   assign(op1, load(Ity_I16, mkexpr(op1addr)));
   op2 = (Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I16,
                       mkU16((UShort)op2)));
}

static void
s390_irgen_CHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;

   assign(op1, load(Ity_I32, mkexpr(op1addr)));
   op2 = (Int)(Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));
}

static void
s390_irgen_CGHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   Long op2;

   assign(op1, load(Ity_I64, mkexpr(op1addr)));
   op2 = (Long)(Short)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64((ULong)op2)));
}

static void
s390_irgen_CHRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Sto32, load(Ity_I16, mkU64(addr_rel_long(i2)))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CGHRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_16Sto64, load(Ity_I16, mkU64(addr_rel_long(i2)))));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CHHR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, get_gpr_w0(r2));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CHLR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, get_gpr_w1(r2));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CHF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CIH(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   Int op2;

   assign(op1, get_gpr_w0(r1));
   op2 = (Int)i2;
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32((UInt)op2)));
}

static void
s390_irgen_CLR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, get_gpr_w1(r2));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLGR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, get_gpr_dw0(r2));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLGFR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, get_gpr_w1(r2)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CL(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLY(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLG(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLGF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;

   assign(op1, get_gpr_w1(r1));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32(op2)));
}

static void
s390_irgen_CLGFI(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;

   assign(op1, get_gpr_dw0(r1));
   op2 = (ULong)i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64(op2)));
}

static void
s390_irgen_CLI(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I8,
                       mkU8(op2)));
}

static void
s390_irgen_CLIY(UChar i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I8);
   UChar op2;

   assign(op1, load(Ity_I8, mkexpr(op1addr)));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I8,
                       mkU8(op2)));
}

static void
s390_irgen_CLFHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;

   assign(op1, load(Ity_I32, mkexpr(op1addr)));
   op2 = (UInt)i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32(op2)));
}

static void
s390_irgen_CLGHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;

   assign(op1, load(Ity_I64, mkexpr(op1addr)));
   op2 = (ULong)i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I64,
                       mkU64(op2)));
}

static void
s390_irgen_CLHHSI(UShort i2, IRTemp op1addr)
{
   IRTemp op1 = newTemp(Ity_I16);
   UShort op2;

   assign(op1, load(Ity_I16, mkexpr(op1addr)));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I16,
                       mkU16(op2)));
}

static void
s390_irgen_CLRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkU64(addr_rel_long(i2))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLGRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkU64(addr_rel_long(i2))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLGFRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_32Uto64, load(Ity_I32, mkU64(addr_rel_long(i2)))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLHRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, unop(Iop_16Uto32, load(Ity_I16, mkU64(addr_rel_long(i2)))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLGHRL(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, unop(Iop_16Uto64, load(Ity_I16, mkU64(addr_rel_long(i2)))));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
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
}

static void
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

static void
s390_irgen_CGRT(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_CxRT(m3, r1, r2, Ity_I64, S390_CC_OP_SIGNED_COMPARE);
}

static void
s390_irgen_CRT(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_CxRT(m3, r1, r2, Ity_I32, S390_CC_OP_SIGNED_COMPARE);
}

static void
s390_irgen_CLGRT(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_CxRT(m3, r1, r2, Ity_I64, S390_CC_OP_UNSIGNED_COMPARE);
}

static void
s390_irgen_CLRT(UChar m3, UChar r1, UChar r2)
{
   s390_irgen_CxRT(m3, r1, r2, Ity_I32, S390_CC_OP_UNSIGNED_COMPARE);
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

static void
s390_irgen_CGIT(UChar r1, UShort i2, UChar m3)
{
   s390_irgen_CxIT(m3, r1, i2, Ity_I64, S390_CC_OP_SIGNED_COMPARE);
}

static void
s390_irgen_CIT(UChar r1, UShort i2, UChar m3)
{
   s390_irgen_CxIT(m3, r1, i2, Ity_I32, S390_CC_OP_SIGNED_COMPARE);
}

static void
s390_irgen_CLGIT(UChar r1, UShort i2, UChar m3)
{
   s390_irgen_CxIT(m3, r1, i2, Ity_I64, S390_CC_OP_UNSIGNED_COMPARE);
}

static void
s390_irgen_CLFIT(UChar r1, UShort i2, UChar m3)
{
   s390_irgen_CxIT(m3, r1, i2, Ity_I32, S390_CC_OP_UNSIGNED_COMPARE);
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

static void
s390_irgen_CLT(UChar r1, UChar m3, IRTemp op2addr)
{
   s390_irgen_CLxT(r1, m3, op2addr, Ity_I32, S390_CC_OP_UNSIGNED_COMPARE);
}

static void
s390_irgen_CLGT(UChar r1, UChar m3, IRTemp op2addr)
{
   s390_irgen_CLxT(r1, m3, op2addr, Ity_I64, S390_CC_OP_UNSIGNED_COMPARE);
}

static void
s390_irgen_LAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I32);
   assign(val, load(Ity_I32, mkexpr(op2addr)));
   put_gpr_w1(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ32, mkexpr(val), mkU32(0)));
}

static void
s390_irgen_LGAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I64);
   assign(val, load(Ity_I64, mkexpr(op2addr)));
   put_gpr_dw0(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ64, mkexpr(val), mkU64(0)));
}

static void
s390_irgen_LFHAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I32);
   assign(val, load(Ity_I32, mkexpr(op2addr)));
   put_gpr_w0(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ32, mkexpr(val), mkU32(0)));
}

static void
s390_irgen_LLGFAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I64);
   assign(val, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));
   put_gpr_dw0(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ64, mkexpr(val), mkU64(0)));
}

static void
s390_irgen_LLGTAT(UChar r1, IRTemp op2addr)
{
   IRTemp val = newTemp(Ity_I64);
   assign(val, binop(Iop_And64, mkU64(0x7fffffff),
                     unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr)))));
   put_gpr_dw0(r1, mkexpr(val));
   s390_trap_on_condition(binop(Iop_CmpEQ64, mkexpr(val), mkU64(0)));
}

static void
s390_irgen_CLRJ(UChar r1, UChar r2, UShort i4, UChar m3)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkaddr_expr(addr_relative(i4)));
      } else {
         assign(op1, get_gpr_w1(r1));
         assign(op2, get_gpr_w1(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i4));
      }
   }
}

static void
s390_irgen_CLGRJ(UChar r1, UChar r2, UShort i4, UChar m3)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkaddr_expr(addr_relative(i4)));
      } else {
         assign(op1, get_gpr_dw0(r1));
         assign(op2, get_gpr_dw0(r2));
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE,
                                              op1, op2));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i4));
      }
   }
}

static void
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
}

static void
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
}

static void
s390_irgen_CLIJ(UChar r1, UChar m3, UShort i4, UChar i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkaddr_expr(addr_relative(i4)));
      } else {
         assign(op1, get_gpr_w1(r1));
         op2 = (UInt)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE, op1,
                                              mktemp(Ity_I32, mkU32(op2))));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i4));
      }
   }
}

static void
s390_irgen_CLGIJ(UChar r1, UChar m3, UShort i4, UChar i2)
{
   IRTemp op1 = newTemp(Ity_I64);
   ULong op2;
   IRTemp cond = newTemp(Ity_I32);

   if (m3 == 0) {
   } else {
      if (m3 == 14) {
         always_goto(mkaddr_expr(addr_relative(i4)));
      } else {
         assign(op1, get_gpr_dw0(r1));
         op2 = (ULong)i2;
         assign(cond, s390_call_calculate_icc(m3, S390_CC_OP_UNSIGNED_COMPARE, op1,
                                              mktemp(Ity_I64, mkU64(op2))));
         if_condition_goto(binop(Iop_CmpNE32, mkexpr(cond), mkU32(0)),
                           addr_relative(i4));
      }
   }
}

static void
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
}

static void
s390_irgen_CLMY(UChar r1, UChar m3, IRTemp op2addr)
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
   if ((m3 & 8) != 0) {
      assign(b0, unop(Iop_8Uto32, get_gpr_b4(r1)));
      assign(c0, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));
      n = n + 1;
   } else {
      assign(b0, mkU32(0));
      assign(c0, mkU32(0));
   }
   if ((m3 & 4) != 0) {
      assign(b1, unop(Iop_8Uto32, get_gpr_b5(r1)));
      assign(c1, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b1, mkU32(0));
      assign(c1, mkU32(0));
   }
   if ((m3 & 2) != 0) {
      assign(b2, unop(Iop_8Uto32, get_gpr_b6(r1)));
      assign(c2, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b2, mkU32(0));
      assign(c2, mkU32(0));
   }
   if ((m3 & 1) != 0) {
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
}

static void
s390_irgen_CLMH(UChar r1, UChar m3, IRTemp op2addr)
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
   if ((m3 & 8) != 0) {
      assign(b0, unop(Iop_8Uto32, get_gpr_b0(r1)));
      assign(c0, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));
      n = n + 1;
   } else {
      assign(b0, mkU32(0));
      assign(c0, mkU32(0));
   }
   if ((m3 & 4) != 0) {
      assign(b1, unop(Iop_8Uto32, get_gpr_b1(r1)));
      assign(c1, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b1, mkU32(0));
      assign(c1, mkU32(0));
   }
   if ((m3 & 2) != 0) {
      assign(b2, unop(Iop_8Uto32, get_gpr_b2(r1)));
      assign(c2, unop(Iop_8Uto32, load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr),
             mkU64(n)))));
      n = n + 1;
   } else {
      assign(b2, mkU32(0));
      assign(c2, mkU32(0));
   }
   if ((m3 & 1) != 0) {
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
}

static void
s390_irgen_CLHHR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, get_gpr_w0(r2));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLHLR(UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, get_gpr_w1(r2));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLHF(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w0(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_CLIH(UChar r1, UInt i2)
{
   IRTemp op1 = newTemp(Ity_I32);
   UInt op2;

   assign(op1, get_gpr_w0(r1));
   op2 = i2;
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, mktemp(Ity_I32,
                       mkU32(op2)));
}

static void
s390_irgen_CPYA(UChar r1, UChar r2)
{
   put_ar_w0(r1, get_ar_w0(r2));
}

static void
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
}

static void
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
}

static void
s390_irgen_XRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK32(r3, r1, r2, Iop_Xor32, False, False);
}

static void
s390_irgen_XGRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK64(r3, r1, r2, Iop_Xor64, False, False);
}

static void
s390_irgen_NXRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK32(r3, r1, r2, Iop_Xor32, False, True);
}

static void
s390_irgen_NXGRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK64(r3, r1, r2, Iop_Xor64, False, True);
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_EAR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, get_ar_w0(r2));
}

static void
s390_irgen_IC(UChar r1, IRTemp op2addr)
{
   put_gpr_b7(r1, load(Ity_I8, mkexpr(op2addr)));
}

static void
s390_irgen_ICY(UChar r1, IRTemp op2addr)
{
   put_gpr_b7(r1, load(Ity_I8, mkexpr(op2addr)));
}

static void
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
}

static void
s390_irgen_ICMY(UChar r1, UChar m3, IRTemp op2addr)
{
   UChar n;
   IRTemp result = newTemp(Ity_I32);
   UInt mask;

   n = 0;
   mask = (UInt)m3;
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
}

static void
s390_irgen_ICMH(UChar r1, UChar m3, IRTemp op2addr)
{
   UChar n;
   IRTemp result = newTemp(Ity_I32);
   UInt mask;

   n = 0;
   mask = (UInt)m3;
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
}

static void
s390_irgen_IIHF(UChar r1, UInt i2)
{
   put_gpr_w0(r1, mkU32(i2));
}

static void
s390_irgen_IIHH(UChar r1, UShort i2)
{
   put_gpr_hw0(r1, mkU16(i2));
}

static void
s390_irgen_IIHL(UChar r1, UShort i2)
{
   put_gpr_hw1(r1, mkU16(i2));
}

static void
s390_irgen_IILF(UChar r1, UInt i2)
{
   put_gpr_w1(r1, mkU32(i2));
}

static void
s390_irgen_IILH(UChar r1, UShort i2)
{
   put_gpr_hw2(r1, mkU16(i2));
}

static void
s390_irgen_IILL(UChar r1, UShort i2)
{
   put_gpr_hw3(r1, mkU16(i2));
}

static void
s390_irgen_LR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, get_gpr_w1(r2));
}

static void
s390_irgen_LGR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, get_gpr_dw0(r2));
}

static void
s390_irgen_LGFR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_32Sto64, get_gpr_w1(r2)));
}

static void
s390_irgen_L(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, load(Ity_I32, mkexpr(op2addr)));
}

static void
s390_irgen_LY(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, load(Ity_I32, mkexpr(op2addr)));
}

static void
s390_irgen_LG(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, load(Ity_I64, mkexpr(op2addr)));
}

static void
s390_irgen_LGF(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));
}

static void
s390_irgen_LGFI(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64((ULong)(Long)(Int)i2));
}

static void
s390_irgen_LRL(UChar r1, UInt i2)
{
   put_gpr_w1(r1, load(Ity_I32, mkU64(addr_rel_long(i2))));
}

static void
s390_irgen_LGRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, load(Ity_I64, mkU64(addr_rel_long(i2))));
}

static void
s390_irgen_LGFRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, unop(Iop_32Sto64, load(Ity_I32, mkU64(addr_rel_long(i2)))));
}

static void
s390_irgen_LA(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, mkexpr(op2addr));
}

static void
s390_irgen_LAY(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, mkexpr(op2addr));
}

static void
s390_irgen_LAE(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, mkexpr(op2addr));
}

static void
s390_irgen_LAEY(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, mkexpr(op2addr));
}

static void
s390_irgen_LARL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64(addr_rel_long(i2)));
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
   yield_if(binop(Iop_CasCmpNE32, mkexpr(old_mem), mkexpr(op2)));
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
   yield_if(binop(Iop_CasCmpNE64, mkexpr(old_mem), mkexpr(op2)));
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
   yield_if(binop(Iop_CasCmpNE32, mkexpr(old_mem), mkexpr(op2)));
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

static void
s390_irgen_LAA(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_add32(r1, r3, op2addr, True /* is_signed */);
}

static void
s390_irgen_LAAG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_add64(r1, r3, op2addr, True /* is_signed */);
}

static void
s390_irgen_LAAL(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_add32(r1, r3, op2addr, False /* is_signed */);
}

static void
s390_irgen_LAALG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_add64(r1, r3, op2addr, False /* is_signed */);
}

static void
s390_irgen_LAN(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise32(r1, r3, op2addr, Iop_And32);
}

static void
s390_irgen_LANG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise64(r1, r3, op2addr, Iop_And64);
}

static void
s390_irgen_LAX(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise32(r1, r3, op2addr, Iop_Xor32);
}

static void
s390_irgen_LAXG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise64(r1, r3, op2addr, Iop_Xor64);
}

static void
s390_irgen_LAO(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise32(r1, r3, op2addr, Iop_Or32);
}

static void
s390_irgen_LAOG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_and_bitwise64(r1, r3, op2addr, Iop_Or64);
}

static void
s390_irgen_LTR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_gpr_w1(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);
}

static void
s390_irgen_LTGR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_gpr_dw0(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);
}

static void
s390_irgen_LTGFR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   put_gpr_dw0(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);
}

static void
s390_irgen_LT(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   put_gpr_w1(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);
}

static void
s390_irgen_LTG(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   put_gpr_dw0(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);
}

static void
s390_irgen_LTGF(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));
   put_gpr_dw0(r1, mkexpr(op2));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, op2);
}

static void
s390_irgen_LBR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, unop(Iop_8Sto32, get_gpr_b7(r2)));
}

static void
s390_irgen_LGBR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_8Sto64, get_gpr_b7(r2)));
}

static void
s390_irgen_LB(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_8Sto32, load(Ity_I8, mkexpr(op2addr))));
}

static void
s390_irgen_LGB(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_8Sto64, load(Ity_I8, mkexpr(op2addr))));
}

static void
s390_irgen_LBH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, unop(Iop_8Sto32, load(Ity_I8, mkexpr(op2addr))));
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_LHR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, unop(Iop_16Sto32, get_gpr_hw3(r2)));
}

static void
s390_irgen_LGHR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_16Sto64, get_gpr_hw3(r2)));
}

static void
s390_irgen_LH(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
}

static void
s390_irgen_LHY(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
}

static void
s390_irgen_LGH(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_16Sto64, load(Ity_I16, mkexpr(op2addr))));
}

static void
s390_irgen_LHI(UChar r1, UShort i2)
{
   put_gpr_w1(r1, mkU32((UInt)(Int)(Short)i2));
}

static void
s390_irgen_LGHI(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64((ULong)(Long)(Short)i2));
}

static void
s390_irgen_LHRL(UChar r1, UInt i2)
{
   put_gpr_w1(r1, unop(Iop_16Sto32, load(Ity_I16, mkU64(addr_rel_long(i2)))));
}

static void
s390_irgen_LGHRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, unop(Iop_16Sto64, load(Ity_I16, mkU64(addr_rel_long(i2)))));
}

static void
s390_irgen_LHH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, unop(Iop_16Sto32, load(Ity_I16, mkexpr(op2addr))));
}

static void
s390_irgen_LFH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, load(Ity_I32, mkexpr(op2addr)));
}

static void
s390_irgen_LLGFR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, get_gpr_w1(r2)));
}

static void
s390_irgen_LLGF(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));
}

static void
s390_irgen_LLGFRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, load(Ity_I32, mkU64(addr_rel_long(i2)))));
}

static void
s390_irgen_LLCR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, unop(Iop_8Uto32, get_gpr_b7(r2)));
}

static void
s390_irgen_LLGCR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_8Uto64, get_gpr_b7(r2)));
}

static void
s390_irgen_LLC(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));
}

static void
s390_irgen_LLGC(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_8Uto64, load(Ity_I8, mkexpr(op2addr))));
}

static void
s390_irgen_LLCH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, unop(Iop_8Uto32, load(Ity_I8, mkexpr(op2addr))));
}

static void
s390_irgen_LLHR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, unop(Iop_16Uto32, get_gpr_hw3(r2)));
}

static void
s390_irgen_LLGHR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_16Uto64, get_gpr_hw3(r2)));
}

static void
s390_irgen_LLH(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, unop(Iop_16Uto32, load(Ity_I16, mkexpr(op2addr))));
}

static void
s390_irgen_LLGH(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_16Uto64, load(Ity_I16, mkexpr(op2addr))));
}

static void
s390_irgen_LLHRL(UChar r1, UInt i2)
{
   put_gpr_w1(r1, unop(Iop_16Uto32, load(Ity_I16, mkU64(addr_rel_long(i2)))));
}

static void
s390_irgen_LLGHRL(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, unop(Iop_16Uto64, load(Ity_I16, mkU64(addr_rel_long(i2)))));
}

static void
s390_irgen_LLHH(UChar r1, IRTemp op2addr)
{
   put_gpr_w0(r1, unop(Iop_16Uto32, load(Ity_I16, mkexpr(op2addr))));
}

static void
s390_irgen_LLIHF(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64(((ULong)i2) << 32));
}

static void
s390_irgen_LLIHH(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(((ULong)i2) << 48));
}

static void
s390_irgen_LLIHL(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(((ULong)i2) << 32));
}

static void
s390_irgen_LLILF(UChar r1, UInt i2)
{
   put_gpr_dw0(r1, mkU64(i2));
}

static void
s390_irgen_LLILH(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(((ULong)i2) << 16));
}

static void
s390_irgen_LLILL(UChar r1, UShort i2)
{
   put_gpr_dw0(r1, mkU64(i2));
}

static void
s390_irgen_LLGTR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, binop(Iop_And32, get_gpr_w1(r2),
               mkU32(2147483647))));
}

static void
s390_irgen_LLGT(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, unop(Iop_32Uto64, binop(Iop_And32, load(Ity_I32,
               mkexpr(op2addr)), mkU32(2147483647))));
}

static void
s390_irgen_LNR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(result, mkite(binop(Iop_CmpLE32S, mkexpr(op2), mkU32(0)), mkexpr(op2),
          binop(Iop_Sub32, mkU32(0), mkexpr(op2))));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_BITWISE, result);
}

static void
s390_irgen_LNGR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(result, mkite(binop(Iop_CmpLE64S, mkexpr(op2), mkU64(0)), mkexpr(op2),
          binop(Iop_Sub64, mkU64(0), mkexpr(op2))));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_BITWISE, result);
}

static void
s390_irgen_LNGFR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   assign(result, mkite(binop(Iop_CmpLE64S, mkexpr(op2), mkU64(0)), mkexpr(op2),
          binop(Iop_Sub64, mkU64(0), mkexpr(op2))));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_BITWISE, result);
}

static void
s390_irgen_LOCR(UChar m3, UChar r1, UChar r2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_w1(r1, get_gpr_w1(r2));
}

static void
s390_irgen_LOCGR(UChar m3, UChar r1, UChar r2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_dw0(r1, get_gpr_dw0(r2));
}

static void
s390_irgen_LOC(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   put_gpr_w1(r1, load(Ity_I32, mkexpr(op2addr)));
}

static void
s390_irgen_LOCG(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   put_gpr_dw0(r1, load(Ity_I64, mkexpr(op2addr)));
}

static void
s390_irgen_LPQ(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   put_gpr_dw0(r1, load(Ity_I64, mkexpr(op2addr)));
   put_gpr_dw0(r1 + 1, load(Ity_I64, binop(Iop_Add64, mkexpr(op2addr), mkU64(8))
               ));
}

static void
s390_irgen_LPR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(result, mkite(binop(Iop_CmpLT32S, mkexpr(op2), mkU32(0)),
          binop(Iop_Sub32, mkU32(0), mkexpr(op2)), mkexpr(op2)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_POSITIVE_32, op2);
}

static void
s390_irgen_LPGR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(result, mkite(binop(Iop_CmpLT64S, mkexpr(op2), mkU64(0)),
          binop(Iop_Sub64, mkU64(0), mkexpr(op2)), mkexpr(op2)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_POSITIVE_64, op2);
}

static void
s390_irgen_LPGFR(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));
   assign(result, mkite(binop(Iop_CmpLT64S, mkexpr(op2), mkU64(0)),
          binop(Iop_Sub64, mkU64(0), mkexpr(op2)), mkexpr(op2)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_POSITIVE_64, op2);
}

static void
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
}

static void
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
}

static void
s390_irgen_LRVH(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I16);

   assign(op2, load(Ity_I16, mkexpr(op2addr)));
   put_gpr_b6(r1, unop(Iop_16to8, mkexpr(op2)));
   put_gpr_b7(r1, unop(Iop_16HIto8, mkexpr(op2)));
}

static void
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
}

static void
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
}

static void
s390_irgen_MVHHI(UShort i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU16(i2));
}

static void
s390_irgen_MVHI(UShort i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU32((UInt)(Int)(Short)i2));
}

static void
s390_irgen_MVGHI(UShort i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU64((ULong)(Long)(Short)i2));
}

static void
s390_irgen_MVI(UChar i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU8(i2));
}

static void
s390_irgen_MVIY(UChar i2, IRTemp op1addr)
{
   store(mkexpr(op1addr), mkU8(i2));
}

static void
s390_irgen_MR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));
}

static void
s390_irgen_M(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));
}

static void
s390_irgen_MFY(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));
}

static void
s390_irgen_MG(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1 + 1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_MullS64, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result)));
}

static void
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
}

static void
s390_irgen_MGRK(UChar r3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   assign(result, binop(Iop_MullS64, mkexpr(op2), mkexpr(op3)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result)));
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_MLR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, get_gpr_w1(r2));
   assign(result, binop(Iop_MullU32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));
}

static void
s390_irgen_MLGR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1 + 1));
   assign(op2, get_gpr_dw0(r2));
   assign(result, binop(Iop_MullU64, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result)));
}

static void
s390_irgen_ML(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I64);

   assign(op1, get_gpr_w1(r1 + 1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   assign(result, binop(Iop_MullU32, mkexpr(op1), mkexpr(op2)));
   put_gpr_w1(r1, unop(Iop_64HIto32, mkexpr(result)));
   put_gpr_w1(r1 + 1, unop(Iop_64to32, mkexpr(result)));
}

static void
s390_irgen_MLG(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp result = newTemp(Ity_I128);

   assign(op1, get_gpr_dw0(r1 + 1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   assign(result, binop(Iop_MullU64, mkexpr(op1), mkexpr(op2)));
   put_gpr_dw0(r1, unop(Iop_128HIto64, mkexpr(result)));
   put_gpr_dw0(r1 + 1, unop(Iop_128to64, mkexpr(result)));
}

static void
s390_irgen_MSR(UChar r1, UChar r2)
{
   put_gpr_w1(r1, binop(Iop_Mul32, get_gpr_w1(r1), get_gpr_w1(r2)));
}

static void
s390_irgen_MSGR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, binop(Iop_Mul64, get_gpr_dw0(r1), get_gpr_dw0(r2)));
}

static void
s390_irgen_MSGFR(UChar r1, UChar r2)
{
   put_gpr_dw0(
      r1, binop(Iop_Mul64, get_gpr_dw0(r1), unop(Iop_32Sto64, get_gpr_w1(r2))));
}

static void
s390_irgen_MS(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1,
              binop(Iop_Mul32, get_gpr_w1(r1), load(Ity_I32, mkexpr(op2addr))));
}

static void
s390_irgen_MSC(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op1, get_gpr_w1(r1));
   assign(op2, load(Ity_I32, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_MUL_32, op1, op2);
   put_gpr_w1(r1, binop(Iop_Mul32, mkexpr(op1), mkexpr(op2)));
}

static void
s390_irgen_MSRKC(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   assign(op3, get_gpr_w1(r3));
   s390_cc_thunk_putSS(S390_CC_OP_MUL_32, op2, op3);
   put_gpr_w1(r1, binop(Iop_Mul32, mkexpr(op2), mkexpr(op3)));
}

static void
s390_irgen_MSY(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1,
              binop(Iop_Mul32, get_gpr_w1(r1), load(Ity_I32, mkexpr(op2addr))));
}

static void
s390_irgen_MSG(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1,
              binop(Iop_Mul64, get_gpr_dw0(r1), load(Ity_I64, mkexpr(op2addr))));
}

static void
s390_irgen_MSGC(UChar r1, IRTemp op2addr)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op1, get_gpr_dw0(r1));
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   s390_cc_thunk_putSS(S390_CC_OP_MUL_64, op1, op2);
   put_gpr_dw0(r1, binop(Iop_Mul64, mkexpr(op1), mkexpr(op2)));
}

static void
s390_irgen_MSGF(UChar r1, IRTemp op2addr)
{
   put_gpr_dw0(r1, binop(Iop_Mul64, get_gpr_dw0(r1),
                         unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr)))));
}

static void
s390_irgen_MSFI(UChar r1, UInt i2)
{
   put_gpr_w1(r1, binop(Iop_Mul32, get_gpr_w1(r1), mkU32(i2)));
}

static void
s390_irgen_MSGFI(UChar r1, UInt i2)
{
   ULong op2 = (ULong)i2 - (((ULong)i2 & (1 << 31)) << 1);
   put_gpr_dw0(r1, binop(Iop_Mul64, get_gpr_dw0(r1), mkU64(op2)));
}

static void
s390_irgen_MSGRKC(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));
   s390_cc_thunk_putSS(S390_CC_OP_MUL_64, op2, op3);
   put_gpr_dw0(r1, binop(Iop_Mul64, mkexpr(op2), mkexpr(op3)));
}

static void
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
}

static void
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
}

static void
s390_irgen_ORK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK32(r3, r1, r2, Iop_Or32, False, False);
}

static void
s390_irgen_OGRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK64(r3, r1, r2, Iop_Or64, False, False);
}

static void
s390_irgen_OCRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK32(r3, r1, r2, Iop_Or32, True, False);
}

static void
s390_irgen_OCGRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK64(r3, r1, r2, Iop_Or64, True, False);
}

static void
s390_irgen_NORK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK32(r3, r1, r2, Iop_Or32, False, True);
}

static void
s390_irgen_NOGRK(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_logicalK64(r3, r1, r2, Iop_Or64, False, True);
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_PFD(void)
{
   /* Treat as a no-op */
}

static void
s390_irgen_PFDRL(void)
{
   /* Treat as a no-op */
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

static void
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
}

static void
s390_irgen_RLL(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp amount = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I32);

   assign(amount, binop(Iop_And64, mkexpr(op2addr), mkU64(31)));
   assign(op, get_gpr_w1(r3));
   put_gpr_w1(r1, binop(Iop_Or32, binop(Iop_Shl32, mkexpr(op), unop(Iop_64to8,
              mkexpr(amount))), binop(Iop_Shr32, mkexpr(op), unop(Iop_64to8,
              binop(Iop_Sub64, mkU64(32), mkexpr(amount))))));
}

static void
s390_irgen_RLLG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp amount = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I64);

   assign(amount, binop(Iop_And64, mkexpr(op2addr), mkU64(63)));
   assign(op, get_gpr_dw0(r3));
   put_gpr_dw0(r1, binop(Iop_Or64, binop(Iop_Shl64, mkexpr(op), unop(Iop_64to8,
               mkexpr(amount))), binop(Iop_Shr64, mkexpr(op), unop(Iop_64to8,
               binop(Iop_Sub64, mkU64(64), mkexpr(amount))))));
}

static void
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
}

static void
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
}

static void
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
}

static void
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
    }
}

static void
s390_irgen_RISBG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   s390_irgen_RISBGx(r1, r2, i3, i4, i5, True);
}

static void
s390_irgen_RISBGN(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   s390_irgen_RISBGx(r1, r2, i3, i4, i5, False);
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

static void
s390_irgen_RISBHG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   put_gpr_w0(r1, s390_irgen_RISBxG(r1, r2, i3, i4, i5, True));
}

static void
s390_irgen_RISBLG(UChar r1, UChar r2, UChar i3, UChar i4, UChar i5)
{
   put_gpr_w1(r1, s390_irgen_RISBxG(r1, r2, i3, i4, i5, False));
}

static void
s390_irgen_SAR(UChar r1, UChar r2)
{
   put_ar_w0(r1, get_gpr_w1(r2));
}

static void
s390_irgen_SLDA(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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
}

static void
s390_irgen_SLDL(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_SLL(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, binop(Iop_Shl32, get_gpr_w1(r1), unop(Iop_64to8,
              binop(Iop_And64, mkexpr(op2addr), mkU64(63)))));
}

static void
s390_irgen_SLLK(UChar r1, UChar r3, IRTemp op2addr)
{
   put_gpr_w1(r1, binop(Iop_Shl32, get_gpr_w1(r3), unop(Iop_64to8,
              binop(Iop_And64, mkexpr(op2addr), mkU64(63)))));
}

static void
s390_irgen_SLLG(UChar r1, UChar r3, IRTemp op2addr)
{
   put_gpr_dw0(r1, binop(Iop_Shl64, get_gpr_dw0(r3), unop(Iop_64to8,
               binop(Iop_And64, mkexpr(op2addr), mkU64(63)))));
}

static void
s390_irgen_SRDA(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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
}

static void
s390_irgen_SRDL(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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
}

static void
s390_irgen_SRA(UChar r1, IRTemp op2addr)
{
   IRTemp result = newTemp(Ity_I32);
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r1));
   assign(result, binop(Iop_Sar32, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, result);
}

static void
s390_irgen_SRAK(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp result = newTemp(Ity_I32);
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r3));
   assign(result, binop(Iop_Sar32, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, result);
}

static void
s390_irgen_SRAG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp result = newTemp(Ity_I64);
   IRTemp op = newTemp(Ity_I64);

   assign(op, get_gpr_dw0(r3));
   assign(result, binop(Iop_Sar64, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
          mkexpr(op2addr), mkU64(63)))));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putS(S390_CC_OP_LOAD_AND_TEST, result);
}

static void
s390_irgen_SRL(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r1));
   put_gpr_w1(r1, binop(Iop_Shr32, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
              mkexpr(op2addr), mkU64(63)))));
}

static void
s390_irgen_SRLK(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_I32);

   assign(op, get_gpr_w1(r3));
   put_gpr_w1(r1, binop(Iop_Shr32, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
              mkexpr(op2addr), mkU64(63)))));
}

static void
s390_irgen_SRLG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_I64);

   assign(op, get_gpr_dw0(r3));
   put_gpr_dw0(r1, binop(Iop_Shr64, mkexpr(op), unop(Iop_64to8, binop(Iop_And64,
               mkexpr(op2addr), mkU64(63)))));
}

static void
s390_irgen_ST(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_w1(r1));
}

static void
s390_irgen_STY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_w1(r1));
}

static void
s390_irgen_STG(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_dw0(r1));
}

static void
s390_irgen_STRL(UChar r1, UInt i2)
{
   store(mkU64(addr_rel_long(i2)), get_gpr_w1(r1));
}

static void
s390_irgen_STGRL(UChar r1, UInt i2)
{
   store(mkU64(addr_rel_long(i2)), get_gpr_dw0(r1));
}

static void
s390_irgen_STC(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));
}

static void
s390_irgen_STCY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));
}

static void
s390_irgen_STCH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b3(r1));
}

static void
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
}

static void
s390_irgen_STCMY(UChar r1, UChar m3, IRTemp op2addr)
{
   UChar mask;
   UChar n;

   mask = (UChar)m3;
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
}

static void
s390_irgen_STCMH(UChar r1, UChar m3, IRTemp op2addr)
{
   UChar mask;
   UChar n;

   mask = (UChar)m3;
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
}

static void
s390_irgen_STH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_hw3(r1));
}

static void
s390_irgen_STHY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_hw3(r1));
}

static void
s390_irgen_STHRL(UChar r1, UInt i2)
{
   store(mkU64(addr_rel_long(i2)), get_gpr_hw3(r1));
}

static void
s390_irgen_STHH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_hw1(r1));
}

static void
s390_irgen_STFH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_w0(r1));
}

static void
s390_irgen_STOC(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   store(mkexpr(op2addr), get_gpr_w1(r1));
}

static void
s390_irgen_STOCG(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   store(mkexpr(op2addr), get_gpr_dw0(r1));
}

static void
s390_irgen_STPQ(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   store(mkexpr(op2addr), get_gpr_dw0(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(8)), get_gpr_dw0(r1 + 1));
}

static void
s390_irgen_STRVH(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(1)), get_gpr_b6(r1));
}

static void
s390_irgen_STRV(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_gpr_b7(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(1)), get_gpr_b6(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(2)), get_gpr_b5(r1));
   store(binop(Iop_Add64, mkexpr(op2addr), mkU64(3)), get_gpr_b4(r1));
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_SHHHR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r2));
   assign(op3, get_gpr_w0(r3));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));
}

static void
s390_irgen_SHHLR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_SUB_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_SLHHHR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r2));
   assign(op3, get_gpr_w0(r3));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));
}

static void
s390_irgen_SLHHLR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
   IRTemp result = newTemp(Ity_I32);

   assign(op2, get_gpr_w0(r2));
   assign(op3, get_gpr_w1(r3));
   assign(result, binop(Iop_Sub32, mkexpr(op2), mkexpr(op3)));
   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_SUB_32, op2, op3);
   put_gpr_w0(r1, mkexpr(result));
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_SVC(UChar i)
{
   IRTemp sysno = newTemp(Ity_I64);

   if (i != 0) {
      assign(sysno, mkU64(i));
   } else {
      assign(sysno, unop(Iop_32Uto64, get_gpr_w1(1)));
   }
   system_call(mkexpr(sysno));
}

static void
s390_irgen_TMx(UChar mask, IRTemp op1addr)
{
   IRTemp masked = newTemp(Ity_I8);

   assign(masked, binop(Iop_And8, load(Ity_I8, mkexpr(op1addr)), mkU8(mask)));
   s390_cc_thunk_putZZ(S390_CC_OP_TEST_UNDER_MASK_8, masked, mktemp(Ity_I8,
                       mkU8(mask)));
}

static void
s390_irgen_TM(UChar i2, IRTemp op1addr)
{
   s390_irgen_TMx(i2, op1addr);
}

static void
s390_irgen_TMY(UChar i2, IRTemp op1addr)
{
   s390_irgen_TMx( i2, op1addr);
}

static void
s390_irgen_TMxx(UChar r1, UShort mask, UChar offs)
{
   if (mask == 0) {
      s390_cc_set_val(0);
      return;
   }

   IRExpr* masked;
   masked = binop(Iop_And64, get_gpr_dw0(r1), mkU64((ULong)mask << offs));

   if ((mask & (mask - 1)) == 0) {
      /* Single-bit mask */
      s390_cc_thunk_put1(S390_CC_OP_BITWISE2, mktemp(Ity_I64, masked), False);
   } else {
      if (offs) {
         masked = binop(Iop_Shr64, masked, mkU8(offs));
      }
      s390_cc_thunk_putZZ(S390_CC_OP_TEST_UNDER_MASK_16,
                          mktemp(Ity_I64, masked),
                          mktemp(Ity_I64, mkU64(mask)));
   }
}

static void
s390_irgen_TMHH(UChar r1, UShort i2)
{
   s390_irgen_TMxx( r1, i2, 48);
}

static void
s390_irgen_TMHL(UChar r1, UShort i2)
{
   s390_irgen_TMxx(r1, i2, 32);
}

static void
s390_irgen_TMLH(UChar r1, UShort i2)
{
   s390_irgen_TMxx(r1, i2, 16);
}

static void
s390_irgen_TMLL(UChar r1, UShort i2)
{
   s390_irgen_TMxx(r1, i2, 0);
}

static void
s390_irgen_EFPC(UChar r1)
{
   put_gpr_w1(r1, get_fpc_w0());
}

static void
s390_irgen_LER(UChar r1, UChar r2)
{
   put_fpr_w0(r1, get_fpr_w0(r2));
}

static void
s390_irgen_LDR(UChar r1, UChar r2)
{
   put_fpr_dw0(r1, get_fpr_dw0(r2));
}

static void
s390_irgen_LDER(UChar r1, UChar r2)
{
   put_fpr_dw0(r1, mkF64i(0x0));
   put_fpr_w0(r1, get_fpr_w0(r2));
}

static void
s390_irgen_LXR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   put_fpr_dw0(r1, get_fpr_dw0(r2));
   put_fpr_dw0(r1 + 2, get_fpr_dw0(r2 + 2));
}

static void
s390_irgen_LE(UChar r1, IRTemp op2addr)
{
   put_fpr_w0(r1, load(Ity_F32, mkexpr(op2addr)));
}

static void
s390_irgen_LD(UChar r1, IRTemp op2addr)
{
   put_fpr_dw0(r1, load(Ity_F64, mkexpr(op2addr)));
}

static void
s390_irgen_LDE(UChar r1, IRTemp op2addr)
{
   put_fpr_dw0(r1, mkF64i(0x0));
   put_fpr_w0(r1, load(Ity_F32, mkexpr(op2addr)));
}

static void
s390_irgen_LEY(UChar r1, IRTemp op2addr)
{
   put_fpr_w0(r1, load(Ity_F32, mkexpr(op2addr)));
}

static void
s390_irgen_LDY(UChar r1, IRTemp op2addr)
{
   put_fpr_dw0(r1, load(Ity_F64, mkexpr(op2addr)));
}

static void
s390_irgen_LFPC(IRTemp op2addr)
{
   put_fpc_w0(load(Ity_I32, mkexpr(op2addr)));
}

static void
s390_irgen_LZER(UChar r1)
{
   put_fpr_w0(r1, mkF32i(0x0));
}

static void
s390_irgen_LZDR(UChar r1)
{
   put_fpr_dw0(r1, mkF64i(0x0));
}

static void
s390_irgen_LZXR(UChar r1)
{
   s390_insn_assert(is_valid_fpr_pair(r1));

   put_fpr_dw0(r1, mkF64i(0x0));
   put_fpr_dw0(r1 + 2, mkF64i(0x0));
}

static void
s390_irgen_SRNM(IRTemp op2addr)
{
   UInt input_mask, fpc_mask;

   input_mask = 3;
   fpc_mask = 7;

   put_fpc_w0(binop(Iop_Or32,
                    binop(Iop_And32, get_fpc_w0(), mkU32(~fpc_mask)),
                    binop(Iop_And32, unop(Iop_64to32, mkexpr(op2addr)),
                          mkU32(input_mask))));
}

static void
s390_irgen_SRNMB(UChar b2, UShort d2)
{
   /* Can only check at IR generation time when b2 == 0 */
   if (b2 == 0) {
      d2 &= 0xff;     // d2[0:55] is ignored
      s390_insn_assert(d2 <= 3 || d2 == 7);  // valid rounding mode
   }
   IRTemp op2addr = newTemp(Ity_I64);

   assign(op2addr, binop(Iop_Add64, mkU64(d2), b2 != 0 ? get_gpr_dw0(b2) :
          mkU64(0)));

   UInt input_mask = 7;
   UInt fpc_mask = 7;

   put_fpc_w0(binop(Iop_Or32,
                    binop(Iop_And32, get_fpc_w0(), mkU32(~fpc_mask)),
                    binop(Iop_And32, unop(Iop_64to32, mkexpr(op2addr)),
                          mkU32(input_mask))));
}


/* All 8 values in op2addr[61:63] correspond to a valid DFP rounding mode */
static void
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
}


static void
s390_irgen_SFPC(UChar r1)
{
   put_fpc_w0(get_gpr_w1(r1));
}

static void
s390_irgen_STE(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpr_w0(r1));
}

static void
s390_irgen_STD(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpr_dw0(r1));
}

static void
s390_irgen_STEY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpr_w0(r1));
}

static void
s390_irgen_STDY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpr_dw0(r1));
}

static void
s390_irgen_STFPC(IRTemp op2addr)
{
   store(mkexpr(op2addr), get_fpc_w0());
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_CEFBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   s390_insn_assert(is_valid_rounding_mode(m3));

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_w0(r1, binop(Iop_I32StoF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        mkexpr(op2)));
}

static void
s390_irgen_CDFBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_dw0(r1, unop(Iop_I32StoF64, mkexpr(op2)));
}

static void
s390_irgen_CEGBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   s390_insn_assert(is_valid_rounding_mode(m3));

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_w0(r1, binop(Iop_I64StoF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        mkexpr(op2)));
}

static void
s390_irgen_CDGBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   s390_insn_assert(is_valid_rounding_mode(m3));

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_dw0(r1, binop(Iop_I64StoF64, mkexpr(encode_bfp_rounding_mode(m3)),
                         mkexpr(op2)));
}

static void
s390_irgen_CELFBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_w0(r1, binop(Iop_I32UtoF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        mkexpr(op2)));
}

static void
s390_irgen_CDLFBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_dw0(r1, unop(Iop_I32UtoF64, mkexpr(op2)));
}

static void
s390_irgen_CELGBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_w0(r1, binop(Iop_I64UtoF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        mkexpr(op2)));
}

static void
s390_irgen_CDLGBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_dw0(r1, binop(Iop_I64UtoF64,
                         mkexpr(encode_bfp_rounding_mode(m3)),
                         mkexpr(op2)));
}

static void
s390_irgen_CLFEBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_w0(r2));
   assign(result, binop(Iop_F32toI32U, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_32_TO_UINT_32, op, rounding_mode);
}

static void
s390_irgen_CLFDBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_dw0(r2));
   assign(result, binop(Iop_F64toI32U, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_64_TO_UINT_32, op, rounding_mode);
}

static void
s390_irgen_CLGEBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_w0(r2));
   assign(result, binop(Iop_F32toI64U, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_32_TO_UINT_64, op, rounding_mode);
}

static void
s390_irgen_CLGDBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_dw0(r2));
   assign(result, binop(Iop_F64toI64U, mkexpr(rounding_mode),
                        mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_64_TO_UINT_64, op, rounding_mode);
}

static void
s390_irgen_CFEBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_w0(r2));
   assign(result, binop(Iop_F32toI32S, mkexpr(rounding_mode),
          mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_32_TO_INT_32, op, rounding_mode);
}

static void
s390_irgen_CFDBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_dw0(r2));
   assign(result, binop(Iop_F64toI32S, mkexpr(rounding_mode),
          mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_64_TO_INT_32, op, rounding_mode);
}

static void
s390_irgen_CGEBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F32);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_w0(r2));
   assign(result, binop(Iop_F32toI64S, mkexpr(rounding_mode),
          mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_32_TO_INT_64, op, rounding_mode);
}

static void
s390_irgen_CGDBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_dw0(r2));
   assign(result, binop(Iop_F64toI64S, mkexpr(rounding_mode),
          mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_BFP_64_TO_INT_64, op, rounding_mode);
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_LTEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, get_fpr_w0(r2));
   put_fpr_w0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_32, result);
}

static void
s390_irgen_LTDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, get_fpr_dw0(r2));
   put_fpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_64, result);
}

static void
s390_irgen_LCEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, unop(Iop_NegF32, get_fpr_w0(r2)));
   put_fpr_w0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_32, result);
}

static void
s390_irgen_LCDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_NegF64, get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_BFP_RESULT_64, result);
}

static void
s390_irgen_LDEBR(UChar r1, UChar r2)
{
   IRTemp op = newTemp(Ity_F32);

   assign(op, get_fpr_w0(r2));
   put_fpr_dw0(r1, unop(Iop_F32toF64, mkexpr(op)));
}

static void
s390_irgen_LDEB(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_F32);

   assign(op, load(Ity_F32, mkexpr(op2addr)));
   put_fpr_dw0(r1, unop(Iop_F32toF64, mkexpr(op)));
}

static void
s390_irgen_LEDBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   s390_insn_assert(is_valid_rounding_mode(m3));

   IRTemp op = newTemp(Ity_F64);

   assign(op, get_fpr_dw0(r2));
   put_fpr_w0(r1, binop(Iop_F64toF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        mkexpr(op)));
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
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
}

static void
s390_irgen_ADTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_D64);
   IRTemp op2 = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_D64);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_dw0(r2));
   assign(op2, get_dpr_dw0(r3));
   assign(result, triop(Iop_AddD64, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_DFP_RESULT_64, result);
   put_dpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_AXTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op1 = newTemp(Ity_D128);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_D128);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_pair(r2));
   assign(op2, get_dpr_pair(r3));
   assign(result, triop(Iop_AddD128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_dpr_pair(r1, mkexpr(result));

   s390_cc_thunk_put1d128(S390_CC_OP_DFP_RESULT_128, result);
}

static void
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
}

static void
s390_irgen_CXTR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp op1 = newTemp(Ity_D128);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_dpr_pair(r1));
   assign(op2, get_dpr_pair(r2));
   assign(cc_vex, binop(Iop_CmpD128, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_dfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);
}

static void
s390_irgen_CDFTR(UChar m3 __attribute__((unused)),
                 UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_dpr_dw0(r1, unop(Iop_I32StoD64, mkexpr(op2)));
}

static void
s390_irgen_CXFTR(UChar m3 __attribute__((unused)),
                 UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_dpr_pair(r1, unop(Iop_I32StoD128, mkexpr(op2)));
}

static void
s390_irgen_CDGTRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   assign(op2, get_gpr_dw0(r2));
   put_dpr_dw0(r1, binop(Iop_I64StoD64, mkexpr(encode_dfp_rounding_mode(m3)),
                         mkexpr(op2)));
}

static void
s390_irgen_CXGTRA(UChar m3 __attribute__((unused)),
                  UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_dpr_pair(r1, unop(Iop_I64StoD128, mkexpr(op2)));
}

static void
s390_irgen_CDLFTR(UChar m3 __attribute__((unused)),
                  UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_dpr_dw0(r1, unop(Iop_I32UtoD64, mkexpr(op2)));
}

static void
s390_irgen_CXLFTR(UChar m3 __attribute__((unused)),
                  UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_dpr_pair(r1, unop(Iop_I32UtoD128, mkexpr(op2)));
}

static void
s390_irgen_CDLGTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_dpr_dw0(r1, binop(Iop_I64UtoD64,
                         mkexpr(encode_dfp_rounding_mode(m3)), mkexpr(op2)));
}

static void
s390_irgen_CXLGTR(UChar m3 __attribute__((unused)),
                  UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_dpr_pair(r1, unop(Iop_I64UtoD128, mkexpr(op2)));
}

static void
s390_irgen_CFDTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp op = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

   assign(op, get_dpr_dw0(r2));
   assign(result, binop(Iop_D64toI32S, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_DFP_64_TO_INT_32, op, rounding_mode);
}

static void
s390_irgen_CFXTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

   assign(op, get_dpr_pair(r2));
   assign(result, binop(Iop_D128toI32S, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_128_TO_INT_32, op, rounding_mode);
}

static void
s390_irgen_CGDTRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp op = newTemp(Ity_D64);
   IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

   assign(op, get_dpr_dw0(r2));
   put_gpr_dw0(r1, binop(Iop_D64toI64S, mkexpr(rounding_mode), mkexpr(op)));
   s390_cc_thunk_putFZ(S390_CC_OP_DFP_64_TO_INT_64, op, rounding_mode);
}

static void
s390_irgen_CGXTRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_D128);
   IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

   assign(op, get_dpr_pair(r2));
   put_gpr_dw0(r1, binop(Iop_D128toI64S, mkexpr(rounding_mode), mkexpr(op)));
   s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_128_TO_INT_64, op, rounding_mode);
}

static void
s390_irgen_CEDTR(UChar r1, UChar r2)
{
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

static void
s390_irgen_CEXTR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

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

static void
s390_irgen_CLFDTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp op = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

   assign(op, get_dpr_dw0(r2));
   assign(result, binop(Iop_D64toI32U, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_DFP_64_TO_UINT_32, op, rounding_mode);
}

static void
s390_irgen_CLFXTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

   assign(op, get_dpr_pair(r2));
   assign(result, binop(Iop_D128toI32U, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_128_TO_UINT_32, op, rounding_mode);
}

static void
s390_irgen_CLGDTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp op = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

   assign(op, get_dpr_dw0(r2));
   assign(result, binop(Iop_D64toI64U, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putFZ(S390_CC_OP_DFP_64_TO_UINT_64, op, rounding_mode);
}

static void
s390_irgen_CLGXTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_dfp_rounding_mode(m3);

   assign(op, get_dpr_pair(r2));
   assign(result, binop(Iop_D128toI64U, mkexpr(rounding_mode), mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_128_TO_UINT_64, op, rounding_mode);
}

static void
s390_irgen_DDTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_D64);
   IRTemp op2 = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_D64);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_dw0(r2));
   assign(op2, get_dpr_dw0(r3));
   assign(result, triop(Iop_DivD64, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_dpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_DXTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op1 = newTemp(Ity_D128);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_D128);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_pair(r2));
   assign(op2, get_dpr_pair(r3));
   assign(result, triop(Iop_DivD128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_dpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_EEDTR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_ExtractExpD64, get_dpr_dw0(r2)));
}

static void
s390_irgen_EEXTR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));

   put_gpr_dw0(r1, unop(Iop_ExtractExpD128, get_dpr_pair(r2)));
}

static void
s390_irgen_ESDTR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_ExtractSigD64, get_dpr_dw0(r2)));
}

static void
s390_irgen_ESXTR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));

   put_gpr_dw0(r1, unop(Iop_ExtractSigD128, get_dpr_pair(r2)));
}

static void
s390_irgen_IEDTR(UChar r3, UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_D64);

   assign(op1, get_gpr_dw0(r2));
   assign(op2, get_dpr_dw0(r3));
   assign(result, binop(Iop_InsertExpD64, mkexpr(op1), mkexpr(op2)));
   put_dpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_IEXTR(UChar r3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op1 = newTemp(Ity_I64);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_D128);

   assign(op1, get_gpr_dw0(r2));
   assign(op2, get_dpr_pair(r3));
   assign(result, binop(Iop_InsertExpD128, mkexpr(op1), mkexpr(op2)));
   put_dpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_LDETR(UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x8) != 0)
      emulation_warning(EmWarn_S390X_XiC_not_zero);
   IRTemp op = newTemp(Ity_D32);

   assign(op, get_dpr_w0(r2));
   put_dpr_dw0(r1, unop(Iop_D32toD64, mkexpr(op)));
}

static void
s390_irgen_LXDTR(UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x8) != 0)
      emulation_warning(EmWarn_S390X_XiC_not_zero);
   s390_insn_assert(is_valid_fpr_pair(r1));

   IRTemp op = newTemp(Ity_D64);

   assign(op, get_dpr_dw0(r2));
   put_dpr_pair(r1, unop(Iop_D64toD128, mkexpr(op)));
}

static void
s390_irgen_LDXTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   if ((m4 & 0x8) != 0)
      emulation_warning(EmWarn_S390X_XiC_not_zero);
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp result = newTemp(Ity_D64);

   assign(result, binop(Iop_D128toD64, mkexpr(encode_dfp_rounding_mode(m3)),
                        get_dpr_pair(r2)));
   put_dpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_LEDTR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x8) != 0)
      emulation_warning(EmWarn_S390X_XiC_not_zero);
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   IRTemp op = newTemp(Ity_D64);

   assign(op, get_dpr_dw0(r2));
   put_dpr_w0(r1, binop(Iop_D64toD32, mkexpr(encode_dfp_rounding_mode(m3)),
                        mkexpr(op)));
}

static void
s390_irgen_LTDTR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_D64);

   assign(result, get_dpr_dw0(r2));
   put_dpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_putF(S390_CC_OP_DFP_RESULT_64, result);
}

static void
s390_irgen_LTXTR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp result = newTemp(Ity_D128);

   assign(result, get_dpr_pair(r2));
   put_dpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1d128(S390_CC_OP_DFP_RESULT_128, result);
}

static void
s390_irgen_MDTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_D64);
   IRTemp op2 = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_D64);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_dw0(r2));
   assign(op2, get_dpr_dw0(r3));
   assign(result, triop(Iop_MulD64, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_dpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_MXTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op1 = newTemp(Ity_D128);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_D128);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_pair(r2));
   assign(op2, get_dpr_pair(r3));
   assign(result, triop(Iop_MulD128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_dpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_QADTR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_D64);
   IRTemp op2 = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_D64);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_dw0(r2));
   assign(op2, get_dpr_dw0(r3));
   assign(result, triop(Iop_QuantizeD64, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_dpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_QAXTR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op1 = newTemp(Ity_D128);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_D128);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_pair(r2));
   assign(op2, get_dpr_pair(r3));
   assign(result, triop(Iop_QuantizeD128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_dpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_RRDTR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_I8);
   IRTemp op2 = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_D64);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_gpr_b7(r2));
   assign(op2, get_dpr_dw0(r3));
   assign(result, triop(Iop_SignificanceRoundD64, mkexpr(rounding_mode),
                        mkexpr(op1), mkexpr(op2)));
   put_dpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_RRXTR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op1 = newTemp(Ity_I8);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_D128);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_gpr_b7(r2));
   assign(op2, get_dpr_pair(r3));
   assign(result, triop(Iop_SignificanceRoundD128, mkexpr(rounding_mode),
                        mkexpr(op1), mkexpr(op2)));
   put_dpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_SDTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRTemp op1 = newTemp(Ity_D64);
   IRTemp op2 = newTemp(Ity_D64);
   IRTemp result = newTemp(Ity_D64);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_dw0(r2));
   assign(op2, get_dpr_dw0(r3));
   assign(result, triop(Iop_SubD64, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   s390_cc_thunk_putF(S390_CC_OP_DFP_RESULT_64, result);
   put_dpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_SXTRA(UChar r3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op1 = newTemp(Ity_D128);
   IRTemp op2 = newTemp(Ity_D128);
   IRTemp result = newTemp(Ity_D128);
   IRTemp rounding_mode;

   rounding_mode = encode_dfp_rounding_mode(m4);
   assign(op1, get_dpr_pair(r2));
   assign(op2, get_dpr_pair(r3));
   assign(result, triop(Iop_SubD128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_dpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1d128(S390_CC_OP_DFP_RESULT_128, result);
}

static void
s390_irgen_SLDT(UChar r3, IRTemp op2addr, UChar r1)
{
   IRTemp op = newTemp(Ity_D64);

   assign(op, get_dpr_dw0(r3));
   put_dpr_dw0(r1, binop(Iop_ShlD64, mkexpr(op),
                         unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr),
                                               mkU64(63)))));
}

static void
s390_irgen_SLXT(UChar r3, IRTemp op2addr, UChar r1)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op = newTemp(Ity_D128);

   assign(op, get_dpr_pair(r3));
   put_dpr_pair(r1, binop(Iop_ShlD128, mkexpr(op),
                          unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr),
                                                mkU64(63)))));
}

static void
s390_irgen_SRDT(UChar r3, IRTemp op2addr, UChar r1)
{
   IRTemp op = newTemp(Ity_D64);

   assign(op, get_dpr_dw0(r3));
   put_dpr_dw0(r1, binop(Iop_ShrD64, mkexpr(op),
                         unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr),
                                               mkU64(63)))));
}

static void
s390_irgen_SRXT(UChar r3, IRTemp op2addr, UChar r1)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r3));

   IRTemp op = newTemp(Ity_D128);

   assign(op, get_dpr_pair(r3));
   put_dpr_pair(r1, binop(Iop_ShrD128, mkexpr(op),
                          unop(Iop_64to8, binop(Iop_And64, mkexpr(op2addr),
                                                mkU64(63)))));
}

static void
s390_irgen_TDCET(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_D32);

   assign(value, get_dpr_w0(r1));

   s390_cc_thunk_putFZ(S390_CC_OP_DFP_TDC_32, value, op2addr);
}

static void
s390_irgen_TDCDT(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_D64);

   assign(value, get_dpr_dw0(r1));

   s390_cc_thunk_putFZ(S390_CC_OP_DFP_TDC_64, value, op2addr);
}

static void
s390_irgen_TDCXT(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_fpr_pair(r1));

   IRTemp value = newTemp(Ity_D128);

   assign(value, get_dpr_pair(r1));

   s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_TDC_128, value, op2addr);
}

static void
s390_irgen_TDGET(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_D32);

   assign(value, get_dpr_w0(r1));

   s390_cc_thunk_putFZ(S390_CC_OP_DFP_TDG_32, value, op2addr);
}

static void
s390_irgen_TDGDT(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_D64);

   assign(value, get_dpr_dw0(r1));

   s390_cc_thunk_putFZ(S390_CC_OP_DFP_TDG_64, value, op2addr);
}

static void
s390_irgen_TDGXT(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_fpr_pair(r1));

   IRTemp value = newTemp(Ity_D128);

   assign(value, get_dpr_pair(r1));

   s390_cc_thunk_put1d128Z(S390_CC_OP_DFP_TDG_128, value, op2addr);
}

static void
s390_irgen_CLC(UChar length, IRTemp start1, IRTemp start2)
{
   IRType ty;

   switch (length) {
   case 0: ty = Ity_I8; break;
   case 1: ty = Ity_I16; break;
   case 3: ty = Ity_I32; break;
   case 7: ty = Ity_I64; break;
   default: ty = Ity_INVALID;
   }
   if (ty != Ity_INVALID) {
      IRTemp a = newTemp(ty);
      IRTemp b = newTemp(ty);

      assign(a, load(ty, mkexpr(start1)));
      assign(b, load(ty, mkexpr(start2)));
      s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, a, b);
   } else {
      IRTemp len = newTemp(Ity_I64);

      assign(len, mkU64(length));
      s390_irgen_CLC_EX(len, start1, start2);
   }
}

static void
s390_irgen_CLCL(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r2));

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
}

static void
s390_irgen_CLCLE(UChar r1, UChar r3, IRTemp pad2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r3));

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

   /* Equal.  Clear CC, to avoid duplicate dependency on the comparison. */
   s390_cc_set_val(0);
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
s390_irgen_EX_SS(UChar r, IRTemp addr2, IRTemp torun,
                 void (*irgen)(IRTemp length, IRTemp start1, IRTemp start2),
                 UInt lensize)
{
   IRTemp cond;
   IRDirty *d;
   ULong ovl;

   IRTemp start1 = newTemp(Ity_I64);
   IRTemp start2 = newTemp(Ity_I64);
   IRTemp len = newTemp(lensize == 64 ? Ity_I64 : Ity_I32);
   cond = newTemp(Ity_I1);

   /* Start with a check that the saved code is still correct */
   assign(cond, binop(Iop_CmpNE64, mkexpr(torun), mkU64(last_execute_target)));
   /* If not, save the new value */
   d = unsafeIRDirty_0_N (0, "s390x_dirtyhelper_EX", &s390x_dirtyhelper_EX,
                          mkIRExprVec_2(mkexpr(torun), mkexpr(addr2)));
   d->guard = mkexpr(cond);
   stmt(IRStmt_Dirty(d));

   /* and restart */
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMSTART),
                   mkU64(guest_IA_curr_instr)));
   stmt(IRStmt_Put(S390X_GUEST_OFFSET(guest_CMLEN), mkU64(4)));
   restart_if(mkexpr(cond));

   ovl = last_execute_target;
   assign(start1, binop(Iop_Add64, mkU64(SSa_d1(ovl)),
          SSa_b1(ovl) != 0 ? get_gpr_dw0(SSa_b1(ovl)) : mkU64(0)));
   assign(start2, binop(Iop_Add64, mkU64(SSa_d2(ovl)),
          SSa_b2(ovl) != 0 ? get_gpr_dw0(SSa_b2(ovl)) : mkU64(0)));
   assign(len, unop(lensize == 64 ? Iop_8Uto64 : Iop_8Uto32, binop(Iop_Or8,
          r != 0 ? get_gpr_b7(r): mkU8(0), mkU8(SSa_l(ovl)))));
   irgen(len, start1, start2);

   last_execute_target = Invalid_execute_target;
}

static void
s390_irgen_EX(UChar r1, IRTemp addr2)
{
   IRTemp  insn0, unmodified_insn;
   IRExpr* incr_addr;
   insn0           = newTemp(Ity_I64);
   unmodified_insn = newTemp(Ity_I64);
   assign(insn0, unop(Iop_16Uto64, load(Ity_I16, mkexpr(addr2))));
   incr_addr = binop(Iop_Add64, mkexpr(addr2), mkU64(2));
   assign(
      unmodified_insn,
      binop(
         Iop_Or64, binop(Iop_Shl64, mkexpr(insn0), mkU8(48)),
         mkite(
            binop(Iop_CmpLT64U, mkexpr(insn0), mkU64(0x4000)), mkU64(0),
            mkite(binop(Iop_CmpLT64U, mkexpr(insn0), mkU64(0xc000)),
                  binop(Iop_Shl64, unop(Iop_16Uto64, load(Ity_I16, incr_addr)),
                        mkU8(32)),
                  binop(Iop_Shl64, unop(Iop_32Uto64, load(Ity_I32, incr_addr)),
                        mkU8(16))))));

   if (last_execute_target == Invalid_execute_target) {
      /* no code information yet */
      IRDirty *d;

      /* so safe the code... */
      d = unsafeIRDirty_0_N (0, "s390x_dirtyhelper_EX", &s390x_dirtyhelper_EX,
                             mkIRExprVec_2(mkexpr(unmodified_insn), mkexpr(addr2)));
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
      return;
   }

   switch (last_execute_target & 0xff00000000000000ULL) {
   case 0xd200000000000000ULL:
      /* special case MVC */
      s390_irgen_EX_SS(r1, addr2, unmodified_insn, s390_irgen_MVC_EX, 64);
      return;

   case 0xd500000000000000ULL:
      /* special case CLC */
      s390_irgen_EX_SS(r1, addr2, unmodified_insn, s390_irgen_CLC_EX, 64);
      return;

   case 0xd700000000000000ULL:
      /* special case XC */
      s390_irgen_EX_SS(r1, addr2, unmodified_insn, s390_irgen_XC_EX, 32);
      return;

   case 0xd600000000000000ULL:
      /* special case OC */
      s390_irgen_EX_SS(r1, addr2, unmodified_insn, s390_irgen_OC_EX, 32);
      return;

   case 0xd400000000000000ULL:
      /* special case NC */
      s390_irgen_EX_SS(r1, addr2, unmodified_insn, s390_irgen_NC_EX, 32);
      return;

   case 0xdc00000000000000ULL:
      /* special case TR */
      s390_irgen_EX_SS(r1, addr2, unmodified_insn, s390_irgen_TR_EX, 64);
      return;

   case 0xe800000000000000ULL:
      /* special case MVCIN */
      s390_irgen_EX_SS(r1, addr2, unmodified_insn, s390_irgen_MVCIN_EX, 64);
      return;

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
      assign(torun, binop(Iop_Or64, mkexpr(unmodified_insn),
                          binop(Iop_Shl64, mkexpr(orperand), mkU8(48))));

      /* Start with a check that saved code is still correct. Compare the target
       * address as well, since it may be relevant to relative addressing. */
      assign(
         cond,
         binop(Iop_Or1,
               binop(Iop_CmpNE64, mkexpr(torun), mkU64(last_execute_target)),
               binop(Iop_CmpNE64, mkexpr(addr2), mkU64(guest_IA_rel_base))));
      /* If not, save the new values */
      d = unsafeIRDirty_0_N (0, "s390x_dirtyhelper_EX", &s390x_dirtyhelper_EX,
                             mkIRExprVec_2(mkexpr(torun), mkexpr(addr2)));
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
      /* dont make useless translations in the next execute */
      last_execute_target = Invalid_execute_target;
   }
   }
}

static void
s390_irgen_EXRL(UChar r1, UInt offset)
{
   IRTemp addr = newTemp(Ity_I64);
   Addr64 bytes_addr;
   UChar *bytes;
   /* we might save one round trip because we know the target */
   if (last_execute_target == Invalid_execute_target) {
      bytes_addr = addr_rel_long(offset);
      bytes = (UChar *)(HWord)bytes_addr;
      last_execute_target = ((ULong)bytes[0] << 56) | ((ULong)bytes[1] << 48);
      if (bytes[0] >= 0x40)
         last_execute_target |= ((ULong)bytes[2] << 40) | ((ULong)bytes[3] << 32);
      if (bytes[0] >= 0xc0)
         last_execute_target |= ((ULong)bytes[4] << 24) | ((ULong)bytes[5] << 16);
      guest_IA_rel_base = bytes_addr;
   } else
      bytes_addr = guest_IA_rel_base;
   assign(addr, mkU64(bytes_addr));
   s390_irgen_EX(r1, addr);
}

static void
s390_irgen_IPM(UChar r1)
{
   // As long as we dont support SPM, lets just assume 0 as program mask
   put_gpr_b4(r1, unop(Iop_32to8, binop(Iop_Or32, mkU32(0 /* program mask */),
                       binop(Iop_Shl32, s390_call_calculate_cc(), mkU8(4)))));
}


static void
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
}

static void
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

static void
s390_irgen_LM(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_multiple_32bit(r1, r3, op2addr);
}

static void
s390_irgen_LMY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_multiple_32bit(r1, r3, op2addr);
}

static void
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
}

static void
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

static void
s390_irgen_STM(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_store_multiple_32bit(r1, r3, op2addr);
}

static void
s390_irgen_STMY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_store_multiple_32bit(r1, r3, op2addr);
}

static void
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
}

static void
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

static void
s390_irgen_XC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I32);

   assign(len, mkU32(length));
   s390_irgen_xonc(Iop_Xor8, len, start1, start2);
}

static void
s390_irgen_XC_sameloc(UChar length, UChar b, UShort d)
{
   IRTemp start = newTemp(Ity_I64);
   assign(start,
          binop(Iop_Add64, mkU64(d), b != 0 ? get_gpr_dw0(b) : mkU64(0)));

   if (length < 7) {
      for (UInt i = 0; i <= length; ++i) {
         store(binop(Iop_Add64, mkexpr(start), mkU64(i)), mkU8(0));
      }
   } else {
      if (length < 32) {
         for (UInt i = 0; i <= length - 7; i += 8) {
            store(binop(Iop_Add64, mkexpr(start), mkU64(i)), mkU64(0));
         }
      } else {
         IRTemp counter = newTemp(Ity_I64);
         assign(counter, get_counter_dw0());
         store(binop(Iop_Add64, mkexpr(start), mkexpr(counter)), mkU64(0));
         put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(8)));
         iterate_if(binop(Iop_CmpLE64U, mkexpr(counter), mkU64(length - 15)));

         /* Reset counter */
         put_counter_dw0(mkU64(0));
      }
      /* Clear the remaining bytes with backward overlap */
      if ((length + 1) % 8 != 0) {
         store(binop(Iop_Add64, mkexpr(start), mkU64(length - 7)), mkU64(0));
      }
   }

   s390_cc_set_val(0);
}

static void
s390_irgen_NC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I32);

   assign(len, mkU32(length));
   s390_irgen_xonc(Iop_And8, len, start1, start2);
}

static void
s390_irgen_OC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I32);

   assign(len, mkU32(length));
   s390_irgen_xonc(Iop_Or8, len, start1, start2);
}


static void
s390_irgen_MVC(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I64);

   assign(len, mkU64(length));
   s390_irgen_MVC_EX(len, start1, start2);
}

static void
s390_irgen_MVCIN(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I64);

   assign(len, mkU64(length));
   s390_irgen_MVCIN_EX(len, start1, start2);
}

static void
s390_irgen_MVCRL(IRTemp op1addr, IRTemp op2addr)
{
   IRTemp counter = newTemp(Ity_I64);
   IRTemp offset = newTemp(Ity_I64);

   assign(counter, get_counter_dw0());
   /* offset = length - 1 - counter, where length-1 is specified in r0 */
   assign(offset,
          binop(Iop_Sub64,
                unop(Iop_16Uto64,
                     binop(Iop_And16, get_gpr_hw3(0), mkU16(0xfff))),
                mkexpr(counter)));

   store(binop(Iop_Add64, mkexpr(op1addr), mkexpr(offset)),
         load(Ity_I8, binop(Iop_Add64, mkexpr(op2addr), mkexpr(offset))));

   /* Check for end of field */
   put_counter_dw0(binop(Iop_Add64, mkexpr(counter), mkU64(1)));
   iterate_if(binop(Iop_CmpNE64, mkexpr(offset), mkU64(0)));
   put_counter_dw0(mkU64(0));
}

static void
s390_irgen_MVCL(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r2));

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
}


static void
s390_irgen_MVCLE(UChar r1, UChar r3, IRTemp pad2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r3));

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
}

static void
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

static void
s390_irgen_DR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));

   s390_irgen_divide_64to32(Iop_DivModS64to32, r1, op2);
}

static void
s390_irgen_D(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));

   s390_irgen_divide_64to32(Iop_DivModS64to32, r1, op2);
}

static void
s390_irgen_DLR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));

   s390_irgen_divide_64to32(Iop_DivModU64to32, r1, op2);
}

static void
s390_irgen_DL(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, load(Ity_I32, mkexpr(op2addr)));

   s390_irgen_divide_64to32(Iop_DivModU64to32, r1, op2);
}

static void
s390_irgen_DLG(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));

   s390_irgen_divide_128to64(Iop_DivModU128to64, r1, op2);
}

static void
s390_irgen_DLGR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));

   s390_irgen_divide_128to64(Iop_DivModU128to64, r1, op2);
}

static void
s390_irgen_DSGR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));

   s390_irgen_divide_64to64(Iop_DivModS64to64, r1, op2);
}

static void
s390_irgen_DSG(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, load(Ity_I64, mkexpr(op2addr)));

   s390_irgen_divide_64to64(Iop_DivModS64to64, r1, op2);
}

static void
s390_irgen_DSGFR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, get_gpr_w1(r2)));

   s390_irgen_divide_64to64(Iop_DivModS64to64, r1, op2);
}

static void
s390_irgen_DSGF(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, unop(Iop_32Sto64, load(Ity_I32, mkexpr(op2addr))));

   s390_irgen_divide_64to64(Iop_DivModS64to64, r1, op2);
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

static void
s390_irgen_LAM(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_ar_multiple(r1, r3, op2addr);
}

static void
s390_irgen_LAMY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_load_ar_multiple(r1, r3, op2addr);
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

static void
s390_irgen_STAM(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_store_ar_multiple(r1, r3, op2addr);
}

static void
s390_irgen_STAMY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_store_ar_multiple(r1, r3, op2addr);
}


/* Implementation for 32-bit compare-and-swap */
static void
s390_irgen_cas_32(UChar r1, UChar r3, IRTemp op2addr)
{
   IRCAS *cas;
   IRTemp op1 = newTemp(Ity_I32);
   IRTemp old_mem = newTemp(Ity_I32);
   IRTemp op3 = newTemp(Ity_I32);
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
   assign(nequal, binop(Iop_CasCmpNE32, mkexpr(op1), mkexpr(old_mem)));
   s390_cc_thunk_put1(S390_CC_OP_BITWISE, nequal, True);

   /* If operands were equal (cc == 0) just store the old value op1 in r1.
      Otherwise, store the old_value from memory in r1 and yield. */
   put_gpr_w1(r1, mkite(mkexpr(nequal), mkexpr(old_mem), mkexpr(op1)));
   yield_if(mkexpr(nequal));
}

static void
s390_irgen_CS(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_cas_32(r1, r3, op2addr);
}

static void
s390_irgen_CSY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_irgen_cas_32(r1, r3, op2addr);
}

static void
s390_irgen_CSG(UChar r1, UChar r3, IRTemp op2addr)
{
   IRCAS *cas;
   IRTemp op1 = newTemp(Ity_I64);
   IRTemp old_mem = newTemp(Ity_I64);
   IRTemp op3 = newTemp(Ity_I64);
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
   assign(nequal, binop(Iop_CasCmpNE64, mkexpr(op1), mkexpr(old_mem)));
   s390_cc_thunk_put1(S390_CC_OP_BITWISE, nequal, True);

   /* If operands were equal (cc == 0) just store the old value op1 in r1.
      Otherwise, store the old_value from memory in r1 and yield. */
   put_gpr_dw0(r1, mkite(mkexpr(nequal), mkexpr(old_mem), mkexpr(op1)));
   yield_if(mkexpr(nequal));
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
   assign(nequal,
          binop(Iop_CasCmpNE32,
                binop(Iop_Or32,
                      binop(Iop_Xor32, mkexpr(op1_high), mkexpr(old_mem_high)),
                      binop(Iop_Xor32, mkexpr(op1_low), mkexpr(old_mem_low))),
                mkU32(0)));

   s390_cc_thunk_put1(S390_CC_OP_BITWISE, nequal, True);

   /* If operands were equal (cc == 0) just store the old value op1 in r1.
      Otherwise, store the old_value from memory in r1 and yield. */
   put_gpr_w1(r1,   mkite(mkexpr(nequal), mkexpr(old_mem_high), mkexpr(op1_high)));
   put_gpr_w1(r1+1, mkite(mkexpr(nequal), mkexpr(old_mem_low),  mkexpr(op1_low)));
   yield_if(mkexpr(nequal));
}

static void
s390_irgen_CDS(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r3));
   s390_irgen_cdas_32(r1, r3, op2addr);
}

static void
s390_irgen_CDSY(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r3));
   s390_irgen_cdas_32(r1, r3, op2addr);
}

static void
s390_irgen_CDSG(UChar r1, UChar r3, IRTemp op2addr)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r3));

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
}


/* Binary floating point */

static void
s390_irgen_AXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

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
}

/* Helper for "compare" insns CEBR, CDBR, CXBR, and their signalling
   counterparts. */
static void
s390_irgen_CxBR(UChar r1, UChar r2, IRType type, IROp cmp_op)
{
   IRTemp op1 = newTemp(type);
   IRTemp op2 = newTemp(type);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_fpr_float(r1, type));
   assign(op2, get_fpr_float(r2, type));
   assign(cc_vex, binop(cmp_op, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);
}

static void
s390_irgen_CEBR(UChar r1, UChar r2)
{
   s390_irgen_CxBR(r1, r2, Ity_F32, Iop_CmpF32);
}

static void
s390_irgen_KEBR(UChar r1, UChar r2)
{
   s390_irgen_CxBR(r1, r2, Ity_F32, Iop_CmpF32);
}

static void
s390_irgen_CDBR(UChar r1, UChar r2)
{
   s390_irgen_CxBR(r1, r2, Ity_F64, Iop_CmpF64);
}

static void
s390_irgen_KDBR(UChar r1, UChar r2)
{
   s390_irgen_CxBR(r1, r2, Ity_F64, Iop_CmpF64);
}

static void
s390_irgen_CXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   s390_irgen_CxBR(r1, r2, Ity_F128, Iop_CmpF128);
}

static void
s390_irgen_KXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   s390_irgen_CxBR(r1, r2, Ity_F128, Iop_CmpF128);
}

/* Helper for "compare" insns CEB, CDB, and their signalling counterparts. */
static void
s390_irgen_CxB(UChar r1, IRTemp op2addr, IRType type,
               IROp cmp_op)
{
   IRTemp op1 = newTemp(type);
   IRTemp op2 = newTemp(type);
   IRTemp cc_vex  = newTemp(Ity_I32);
   IRTemp cc_s390 = newTemp(Ity_I32);

   assign(op1, get_fpr_float(r1, type));
   assign(op2, load(type, mkexpr(op2addr)));
   assign(cc_vex,  binop(cmp_op, mkexpr(op1), mkexpr(op2)));

   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);
}

static void
s390_irgen_CEB(UChar r1, IRTemp op2addr)
{
   s390_irgen_CxB(r1, op2addr, Ity_F32, Iop_CmpF32);
}

static void
s390_irgen_KEB(UChar r1, IRTemp op2addr)
{
   s390_irgen_CxB(r1, op2addr, Ity_F32, Iop_CmpF32);
}

static void
s390_irgen_CDB(UChar r1, IRTemp op2addr)
{
   s390_irgen_CxB(r1, op2addr, Ity_F64, Iop_CmpF64);
}

static void
s390_irgen_KDB(UChar r1, IRTemp op2addr)
{
   s390_irgen_CxB(r1, op2addr, Ity_F64, Iop_CmpF64);
}

static void
s390_irgen_CXFBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_pair(r1, unop(Iop_I32StoF128, mkexpr(op2)));
}

static void
s390_irgen_CXLFBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, get_gpr_w1(r2));
   put_fpr_pair(r1, unop(Iop_I32UtoF128, mkexpr(op2)));
}


static void
s390_irgen_CXGBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_pair(r1, unop(Iop_I64StoF128, mkexpr(op2)));
}

static void
s390_irgen_CXLGBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_fpr_pair(r1, unop(Iop_I64UtoF128, mkexpr(op2)));
}

static void
s390_irgen_CFXBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_pair(r2));
   assign(result, binop(Iop_F128toI32S, mkexpr(rounding_mode),
                        mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_128_TO_INT_32, op, rounding_mode);
}

static void
s390_irgen_CLFXBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_I32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_pair(r2));
   assign(result, binop(Iop_F128toI32U, mkexpr(rounding_mode),
                        mkexpr(op)));
   put_gpr_w1(r1, mkexpr(result));
   s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_128_TO_UINT_32, op, rounding_mode);
}


static void
s390_irgen_CGXBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp op = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_I64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(m3);

   assign(op, get_fpr_pair(r2));
   assign(result, binop(Iop_F128toI64S, mkexpr(rounding_mode),
                        mkexpr(op)));
   put_gpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_128_TO_INT_64, op, rounding_mode);
}

static void
s390_irgen_CLGXBR(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

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

static void
s390_irgen_DXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp op1 = newTemp(Ity_F128);
   IRTemp op2 = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_F128);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_pair(r1));
   assign(op2, get_fpr_pair(r2));
   assign(result, triop(Iop_DivF128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_fpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_LTXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp result = newTemp(Ity_F128);

   assign(result, get_fpr_pair(r2));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);
}

static void
s390_irgen_LCXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp result = newTemp(Ity_F128);

   assign(result, unop(Iop_NegF128, get_fpr_pair(r2)));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);
}

static void
s390_irgen_LXDBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));

   IRTemp op = newTemp(Ity_F64);

   assign(op, get_fpr_dw0(r2));
   put_fpr_pair(r1, unop(Iop_F64toF128, mkexpr(op)));
}

static void
s390_irgen_LXEBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));

   IRTemp op = newTemp(Ity_F32);

   assign(op, get_fpr_w0(r2));
   put_fpr_pair(r1, unop(Iop_F32toF128, mkexpr(op)));
}

static void
s390_irgen_LXDB(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_fpr_pair(r1));

   IRTemp op = newTemp(Ity_F64);

   assign(op, load(Ity_F64, mkexpr(op2addr)));
   put_fpr_pair(r1, unop(Iop_F64toF128, mkexpr(op)));
}

static void
s390_irgen_LXEB(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_fpr_pair(r1));

   IRTemp op = newTemp(Ity_F32);

   assign(op, load(Ity_F32, mkexpr(op2addr)));
   put_fpr_pair(r1, unop(Iop_F32toF128, mkexpr(op)));
}

static void
s390_irgen_FIEBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp result = newTemp(Ity_F32);

   assign(result, binop(Iop_RoundF32toInt, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_w0(r2)));
   put_fpr_w0(r1, mkexpr(result));
}

static void
s390_irgen_FIDBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp result = newTemp(Ity_F64);

   assign(result, binop(Iop_RoundF64toInt, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_FIXBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_rounding_mode(m3));
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);

   IRTemp result = newTemp(Ity_F128);

   assign(result, binop(Iop_RoundF128toInt, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_pair(r2)));
   put_fpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_LNEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, unop(Iop_NegF32, unop(Iop_AbsF32, get_fpr_w0(r2))));
   put_fpr_w0(r1, mkexpr(result));
   s390_cc_thunk_put1f(S390_CC_OP_BFP_RESULT_32, result);
}

static void
s390_irgen_LNDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_NegF64, unop(Iop_AbsF64, get_fpr_dw0(r2))));
   put_fpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_put1f(S390_CC_OP_BFP_RESULT_64, result);
}

static void
s390_irgen_LNXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp result = newTemp(Ity_F128);

   assign(result, unop(Iop_NegF128, unop(Iop_AbsF128, get_fpr_pair(r2))));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);
}

static void
s390_irgen_LPEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);

   assign(result, unop(Iop_AbsF32, get_fpr_w0(r2)));
   put_fpr_w0(r1, mkexpr(result));
   s390_cc_thunk_put1f(S390_CC_OP_BFP_RESULT_32, result);
}

static void
s390_irgen_LPDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_AbsF64, get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));
   s390_cc_thunk_put1f(S390_CC_OP_BFP_RESULT_64, result);
}

static void
s390_irgen_LPXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp result = newTemp(Ity_F128);

   assign(result, unop(Iop_AbsF128, get_fpr_pair(r2)));
   put_fpr_pair(r1, mkexpr(result));
   s390_cc_thunk_put1f128(S390_CC_OP_BFP_RESULT_128, result);
}

static void
s390_irgen_LDXBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_rounding_mode(m3));

   IRTemp result = newTemp(Ity_F64);

   assign(result, binop(Iop_F128toF64, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_pair(r2)));
   put_fpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_LEXBRA(UChar m3, UChar m4, UChar r1, UChar r2)
{
   if ((m4 & 0x4) != 0)
      emulation_warning(EmWarn_S390X_XxC_not_zero);
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));
   s390_insn_assert(is_valid_rounding_mode(m3));

   IRTemp result = newTemp(Ity_F32);

   assign(result, binop(Iop_F128toF32, mkexpr(encode_bfp_rounding_mode(m3)),
                        get_fpr_pair(r2)));
   put_fpr_w0(r1, mkexpr(result));
}

static void
s390_irgen_MXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp op1 = newTemp(Ity_F128);
   IRTemp op2 = newTemp(Ity_F128);
   IRTemp result = newTemp(Ity_F128);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op1, get_fpr_pair(r1));
   assign(op2, get_fpr_pair(r2));
   assign(result, triop(Iop_MulF128, mkexpr(rounding_mode), mkexpr(op1),
                        mkexpr(op2)));
   put_fpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_MAEBR(UChar r1, UChar r3, UChar r2)
{
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_w0(r1, qop(Iop_MAddF32, mkexpr(rounding_mode),
                      get_fpr_w0(r3), get_fpr_w0(r2), get_fpr_w0(r1)));
}

static void
s390_irgen_MADBR(UChar r1, UChar r3, UChar r2)
{
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_dw0(r1, qop(Iop_MAddF64, mkexpr(rounding_mode),
                       get_fpr_dw0(r3), get_fpr_dw0(r2), get_fpr_dw0(r1)));
}

static void
s390_irgen_MAEB(UChar r3, IRTemp op2addr, UChar r1)
{
   IRExpr *op2 = load(Ity_F32, mkexpr(op2addr));
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_w0(r1, qop(Iop_MAddF32, mkexpr(rounding_mode),
                      get_fpr_w0(r3), op2, get_fpr_w0(r1)));
}

static void
s390_irgen_MADB(UChar r3, IRTemp op2addr, UChar r1)
{
   IRExpr *op2 = load(Ity_F64, mkexpr(op2addr));
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_dw0(r1, qop(Iop_MAddF64, mkexpr(rounding_mode),
                       get_fpr_dw0(r3), op2, get_fpr_dw0(r1)));
}

static void
s390_irgen_MSEBR(UChar r1, UChar r3, UChar r2)
{
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_w0(r1, qop(Iop_MSubF32, mkexpr(rounding_mode),
                      get_fpr_w0(r3), get_fpr_w0(r2), get_fpr_w0(r1)));
}

static void
s390_irgen_MSDBR(UChar r1, UChar r3, UChar r2)
{
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_dw0(r1, qop(Iop_MSubF64, mkexpr(rounding_mode),
                       get_fpr_dw0(r3), get_fpr_dw0(r2), get_fpr_dw0(r1)));
}

static void
s390_irgen_MSEB(UChar r3, IRTemp op2addr, UChar r1)
{
   IRExpr *op2 = load(Ity_F32, mkexpr(op2addr));
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_w0(r1, qop(Iop_MSubF32, mkexpr(rounding_mode),
                      get_fpr_w0(r3), op2, get_fpr_w0(r1)));
}

static void
s390_irgen_MSDB(UChar r3, IRTemp op2addr, UChar r1)
{
   IRExpr *op2 = load(Ity_F64, mkexpr(op2addr));
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   put_fpr_dw0(r1, qop(Iop_MSubF64, mkexpr(rounding_mode),
                       get_fpr_dw0(r3), op2, get_fpr_dw0(r1)));
}

static void
s390_irgen_SQEBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(result, binop(Iop_SqrtF32, mkexpr(rounding_mode), get_fpr_w0(r2)));
   put_fpr_w0(r1, mkexpr(result));
}

static void
s390_irgen_SQDBR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(result, binop(Iop_SqrtF64, mkexpr(rounding_mode), get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_SQXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

   IRTemp result = newTemp(Ity_F128);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(result, binop(Iop_SqrtF128, mkexpr(rounding_mode),
                        get_fpr_pair(r2)));
   put_fpr_pair(r1, mkexpr(result));
}

static void
s390_irgen_SQEB(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_F32);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op, load(Ity_F32, mkexpr(op2addr)));
   put_fpr_w0(r1, binop(Iop_SqrtF32, mkexpr(rounding_mode), mkexpr(op)));
}

static void
s390_irgen_SQDB(UChar r1, IRTemp op2addr)
{
   IRTemp op = newTemp(Ity_F64);
   IRTemp rounding_mode = encode_bfp_rounding_mode(S390_BFP_ROUND_PER_FPC);

   assign(op, load(Ity_F64, mkexpr(op2addr)));
   put_fpr_dw0(r1, binop(Iop_SqrtF64, mkexpr(rounding_mode), mkexpr(op)));
}

static void
s390_irgen_SXBR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_fpr_pair(r1));
   s390_insn_assert(is_valid_fpr_pair(r2));

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
}

static void
s390_irgen_TCEB(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_F32);

   assign(value, get_fpr_w0(r1));

   s390_cc_thunk_putFZ(S390_CC_OP_BFP_TDC_32, value, op2addr);
}

static void
s390_irgen_TCDB(UChar r1, IRTemp op2addr)
{
   IRTemp value = newTemp(Ity_F64);

   assign(value, get_fpr_dw0(r1));

   s390_cc_thunk_putFZ(S390_CC_OP_BFP_TDC_64, value, op2addr);
}

static void
s390_irgen_TCXB(UChar r1, IRTemp op2addr)
{
   s390_insn_assert(is_valid_fpr_pair(r1));

   IRTemp value = newTemp(Ity_F128);

   assign(value, get_fpr_pair(r1));

   s390_cc_thunk_put1f128Z(S390_CC_OP_BFP_TDC_128, value, op2addr);
}

static void
s390_irgen_LCDFR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_NegF64, get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_LNDFR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_NegF64, unop(Iop_AbsF64, get_fpr_dw0(r2))));
   put_fpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_LPDFR(UChar r1, UChar r2)
{
   IRTemp result = newTemp(Ity_F64);

   assign(result, unop(Iop_AbsF64, get_fpr_dw0(r2)));
   put_fpr_dw0(r1, mkexpr(result));
}

static void
s390_irgen_LDGR(UChar r1, UChar r2)
{
   put_fpr_dw0(r1, unop(Iop_ReinterpI64asF64, get_gpr_dw0(r2)));
}

static void
s390_irgen_LGDR(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_ReinterpF64asI64, get_fpr_dw0(r2)));
}


static void
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

static void
s390_irgen_CVB(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, s390_call_cvb(load(Ity_I64, mkexpr(op2addr))));
}

static void
s390_irgen_CVBY(UChar r1, IRTemp op2addr)
{
   put_gpr_w1(r1, s390_call_cvb(load(Ity_I64, mkexpr(op2addr))));
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

static void
s390_irgen_CVD(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), s390_call_cvd(unop(Iop_32Uto64, get_gpr_w1(r1))));
}

static void
s390_irgen_CVDY(UChar r1, IRTemp op2addr)
{
   store(mkexpr(op2addr), s390_call_cvd(get_gpr_w1(r1)));
}

static void
s390_irgen_FLOGR(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

   IRTemp input    = newTemp(Ity_I64);
   IRTemp num      = newTemp(Ity_I64);
   IRTemp shift_amount = newTemp(Ity_I8);

   /* Use the "count leading zeroes" operator with "natural" semantics.  The
      results of FLOGR and Iop_ClzNat64 are the same for all inputs, including
      input == 0, in which case both operators yield 64. */

   assign(input, get_gpr_dw0(r2));
   assign(num, unop(Iop_ClzNat64, mkexpr(input)));
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
}

static void
s390_irgen_POPCNT(UChar m3, UChar r1, UChar r2)
{
   if (s390_host_has_mi3 && m3 == 8) {
      IRTemp val = newTemp(Ity_I64);

      assign(val, unop(Iop_PopCount64, get_gpr_dw0(r2)));
      s390_cc_thunk_putZ(S390_CC_OP_BITWISE, val);
      put_gpr_dw0(r1, mkexpr(val));
      return;
   }

   static const ULong masks[] = {
      0x5555555555555555, 0x3333333333333333, 0x0F0F0F0F0F0F0F0F,
      0x00FF00FF00FF00FF, 0x0000FFFF0000FFFF, 0x00000000FFFFFFFF,
   };
   Int i, n;
   IRTemp val = newTemp(Ity_I64);

   assign(val, get_gpr_dw0(r2));
   n = (m3 & 8) ? 6 : 3;
   for (i = 0; i < n; i++) {
      IRTemp mask = newTemp(Ity_I64);
      IRTemp tmp = newTemp(Ity_I64);

      assign (mask, mkU64(masks[i]));
      assign(tmp,
             binop(Iop_Add64,
                   binop(Iop_And64,
                         mkexpr(val),
                         mkexpr(mask)),
                   binop(Iop_And64,
                         binop(Iop_Shr64, mkexpr(val), mkU8(1 << i)),
                         mkexpr(mask))));
      val = tmp;
   }
   s390_cc_thunk_putZ(S390_CC_OP_BITWISE, val);
   put_gpr_dw0(r1, mkexpr(val));
}

static void
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
}

static void
s390_irgen_STCKF(IRTemp op2addr)
{
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

static void
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
}

static void
s390_irgen_STFLE(UChar b2, UShort d2)
{
   extension(S390_EXT_STFLE, b2 | (d2 << 8));
}

static void
s390_irgen_CKSM(UChar r1,UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r2));

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
}

static void
s390_irgen_TROO(UChar m3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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

   if ((m3 & 0x1) == 0) {
      s390_cc_set_val(1);
      next_insn_if(binop(Iop_CmpEQ8, mkexpr(op1), mkexpr(test_byte)));
   }
   store(get_gpr_dw0(r1), mkexpr(op1));

   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(des_addr), mkU64(1)));
   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(src_addr), mkU64(1)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(1)));

   iterate();
}

static void
s390_irgen_TRTO(UChar m3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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

   if ((m3 & 0x1) == 0) {
      s390_cc_set_val(1);
      next_insn_if(binop(Iop_CmpEQ8, mkexpr(op1), mkexpr(test_byte)));
   }
   store(get_gpr_dw0(r1), mkexpr(op1));

   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(src_addr), mkU64(2)));
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(des_addr), mkU64(1)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(2)));

   iterate();
}

static void
s390_irgen_TROT(UChar m3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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

   if ((m3 & 0x1) == 0) {
      s390_cc_set_val(1);
      next_insn_if(binop(Iop_CmpEQ16, mkexpr(op1), mkexpr(test_byte)));
   }
   store(get_gpr_dw0(r1), mkexpr(op1));

   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(src_addr), mkU64(1)));
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(des_addr), mkU64(2)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(1)));

   iterate();
}

static void
s390_irgen_TRTT(UChar m3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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

   if ((m3 & 0x1) == 0) {
      s390_cc_set_val(1);
      next_insn_if(binop(Iop_CmpEQ16, mkexpr(op1), mkexpr(test_byte)));
   }

   store(get_gpr_dw0(r1), mkexpr(op1));

   put_gpr_dw0(r2, binop(Iop_Add64, mkexpr(src_addr), mkU64(2)));
   put_gpr_dw0(r1, binop(Iop_Add64, mkexpr(des_addr), mkU64(2)));
   put_gpr_dw0(r1+1, binop(Iop_Sub64, mkexpr(src_len), mkU64(2)));

   iterate();
}

static void
s390_irgen_TR(UChar length, IRTemp start1, IRTemp start2)
{
   IRTemp len = newTemp(Ity_I64);

   assign(len, mkU64(length));
   s390_irgen_TR_EX(len, start1, start2);
}

static void
s390_irgen_TRE(UChar r1,UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));

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

static void
s390_irgen_CU21(UChar m3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r2));

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
   if ((m3 & 0x1) == 1) {
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

static void
s390_irgen_CU24(UChar m3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r2));

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
   if ((m3 & 0x1) == 1) {
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

static void
s390_irgen_CU42(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r2));

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

static void
s390_irgen_CU41(UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r2));

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

   UInt extended_checking = (m3 & 0x1) == 1;

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

static void
s390_irgen_CU12(UChar m3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r2));

   s390_irgen_cu12_cu14(m3, r1, r2, /* is_cu12 = */ 1);
}

static void
s390_irgen_CU14(UChar m3, UChar r1, UChar r2)
{
   s390_insn_assert(is_valid_gpr_pair(r1));
   s390_insn_assert(is_valid_gpr_pair(r2));

   s390_irgen_cu12_cu14(m3, r1, r2, /* is_cu12 = */ 0);
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

static void
s390_irgen_ECAG(UChar r1, UChar r3 __attribute__((unused)), IRTemp op2addr)
{
   put_gpr_dw0(r1, s390_call_ecag(mkexpr(op2addr)));
}

static void
s390_irgen_VL(UChar v1, IRTemp op2addr,
              UChar m3 __attribute__((unused)))   /* alignment hint */
{
   put_vr_qw(v1, load(Ity_V128, mkexpr(op2addr)));
}

static void
s390_irgen_VLR(UChar v1, UChar v2)
{
   put_vr_qw(v1, get_vr_qw(v2));
}

static void
s390_irgen_VST(UChar v1, IRTemp op2addr,
              UChar m3 __attribute__((unused)))   /* alignment hint */
{
   store(mkexpr(op2addr), get_vr_qw(v1));
}

static void
s390_irgen_VLREP(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 <= 3);

   IRType o2type = s390_vr_get_type(m3);
   IRExpr* o2 = load(o2type, mkexpr(op2addr));
   put_vr_qw(v1, s390_V128_fill(o2));
}

static void
s390_irgen_VLEB(UChar v1, IRTemp op2addr, UChar m3)
{
   /* Specification exception cannot occur. */
   IRExpr* o2 = load(Ity_I8, mkexpr(op2addr));
   put_vr(v1, Ity_I8, m3, o2);
}

static void
s390_irgen_VLEH(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 8);

   IRExpr* o2 = load(Ity_I16, mkexpr(op2addr));
   put_vr(v1, Ity_I16, m3, o2);
}

static void
s390_irgen_VLEF(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 4);

   IRExpr* o2 = load(Ity_I32, mkexpr(op2addr));
   put_vr(v1, Ity_I32, m3, o2);
}

static void
s390_irgen_VLEG(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 2);

   IRExpr* o2 = load(Ity_I64, mkexpr(op2addr));
   put_vr(v1, Ity_I64, m3, o2);
}

static void
s390_irgen_VLEIB(UChar v1, UShort i2, UChar m3)
{
   /* Specification exception cannot occur. */
   IRExpr* o2 = unop(Iop_16to8, mkU16(i2));
   put_vr(v1, Ity_I8, m3, o2);
}

static void
s390_irgen_VLEIH(UChar v1, UShort i2, UChar m3)
{
   s390_insn_assert(m3 < 8);

   IRExpr* o2 = mkU16(i2);
   put_vr(v1, Ity_I16, m3, o2);
}

static void
s390_irgen_VLEIF(UChar v1, UShort i2, UChar m3)
{
   s390_insn_assert(m3 < 4);

   IRExpr* o2 = unop(Iop_16Sto32, mkU16(i2));
   put_vr(v1, Ity_I32, m3, o2);
}

static void
s390_irgen_VLEIG(UChar v1, UShort i2, UChar m3)
{
   s390_insn_assert(m3 < 2);

   IRExpr* o2 = unop(Iop_16Sto64, mkU16(i2));
   put_vr(v1, Ity_I64, m3, o2);
}

static void
s390_irgen_VLGV(UChar r1, IRTemp op2addr, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

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
}

static void
s390_irgen_VGBM(UChar v1, UShort i2)
{
   put_vr_qw(v1, mkV128(i2));
}

static void
s390_irgen_VGM(UChar v1, UChar i2, UChar i3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   UChar  max_idx = (8 << m4) - 1;
   UChar  from    = max_idx & i2;
   UChar  to      = max_idx & i3;
   ULong  all_one = (1ULL << max_idx << 1) - 1;
   ULong  value   = (all_one >> from) ^ (all_one >> to >> 1);

   /* In case of wrap-around we now have a value that needs inverting:
          to         from
          V           V
      00000111111111110000000000000000 */
   if (to < from)
      value ^= all_one;

   IRExpr* fillValue;
   switch (m4) {
   case 0:
      fillValue = mkU8(value);
      break;
   case 1:
      fillValue = mkU16(value);
      break;
   case 2:
      fillValue = mkU32(value);
      break;
   case 3:
      fillValue = mkU64(value);
      break;
   default:
      vpanic("s390_irgen_VGM: unknown element size");
   }

   put_vr_qw(v1, s390_V128_fill(fillValue));
}

static void
s390_irgen_VLLEZ(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 <= 3 || m3 == 6);

   IRType type = s390_vr_get_type(m3 & 3);
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

   if (m3 == 6) {
      /* left-aligned */
      put_vr_dw0(v1, binop(Iop_Shl64, op2as64bit, mkU8(32)));
   } else {
      /* right-aligned */
      put_vr_dw0(v1, op2as64bit);
   }
   put_vr_dw1(v1, mkU64(0));
}

static void
s390_irgen_VGEF(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 4);

   put_vr(v1, Ity_I32, m3, load(Ity_I32, mkexpr(op2addr)));
}

static void
s390_irgen_VGEG(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 2);

   put_vr(v1, Ity_I64, m3, load(Ity_I64, mkexpr(op2addr)));
}

static void
s390_irgen_VLM(UChar v1, IRTemp op2addr, UChar v3,
               UChar m4 __attribute__((unused)))   /* alignment hint */
{
   s390_insn_assert(v3 >= v1);
   s390_insn_assert(v3 - v1 <= 16);

   IRExpr* current = mkexpr(op2addr);

   for(UChar vr = v1; vr <= v3; vr++) {
         IRExpr* next = binop(Iop_Add64, current, mkU64(16));
         put_vr_qw(vr, load(Ity_V128, current));
         current = next;
   }
}

static void
s390_irgen_VLVGP(UChar v1, UChar r2, UChar r3)
{
   put_vr_qw(v1, binop(Iop_64HLtoV128, get_gpr_dw0(r2), get_gpr_dw0(r3)));
}

static void
s390_irgen_VLVG(UChar v1, IRTemp op2addr, UChar r3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

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
}

static void
s390_irgen_VMRH(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   const IROp ops[] = { Iop_InterleaveHI8x16, Iop_InterleaveHI16x8,
                        Iop_InterleaveHI32x4, Iop_InterleaveHI64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VMRL(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   const IROp ops[] = { Iop_InterleaveLO8x16, Iop_InterleaveLO16x8,
                        Iop_InterleaveLO32x4, Iop_InterleaveLO64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VPK(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 >= 1 && m4 <= 3);

   const IROp ops[] = { Iop_NarrowBin16to8x16, Iop_NarrowBin32to16x8,
                        Iop_NarrowBin64to32x4 };
   Char index = m4 - 1;
   put_vr_qw(v1, binop(ops[index], get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VPERM(UChar v1, UChar v2, UChar v3, UChar v4)
{
   put_vr_qw(v1, triop(Iop_Perm8x16x2,
             get_vr_qw(v2), get_vr_qw(v3), get_vr_qw(v4)));
}

static void
s390_irgen_VSCEF(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 4);

   store(mkexpr(op2addr), get_vr(v1, Ity_I32, m3));
}

static void
s390_irgen_VSCEG(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 2);

   store(mkexpr(op2addr), get_vr(v1, Ity_I64, m3));
}

static void
s390_irgen_VPDI(UChar v1, UChar v2, UChar v3, UChar m4)
{
   put_vr_qw(v1, binop(Iop_64HLtoV128, m4 & 4 ? get_vr_dw1(v2) : get_vr_dw0(v2),
                       m4 & 1 ? get_vr_dw1(v3) : get_vr_dw0(v3)));
}

static void
s390_irgen_VSEG(UChar v1, UChar v2, UChar m3)
{
   s390_insn_assert(m3 <= 2);

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
}

static void
s390_irgen_VSTEB(UChar v1, IRTemp op2addr, UChar m3)
{
   /* Specification exception cannot occur. */
   store(mkexpr(op2addr), get_vr(v1, Ity_I8, m3));
}

static void
s390_irgen_VSTEH(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 8);

   store(mkexpr(op2addr), get_vr(v1, Ity_I16, m3));
}

static void
s390_irgen_VSTEF(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 4);

   store(mkexpr(op2addr), get_vr(v1, Ity_I32, m3));
}

static void
s390_irgen_VSTEG(UChar v1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 < 2);

   store(mkexpr(op2addr), get_vr(v1, Ity_I64, m3));
}

static void
s390_irgen_VSTM(UChar v1, IRTemp op2addr, UChar v3,
                UChar m4 __attribute__((unused)))   /* alignment hint */
{
   s390_insn_assert(v3 >= v1);
   s390_insn_assert(v3 - v1 <= 16);

   IRExpr* current = mkexpr(op2addr);

   for(UChar vr = v1; vr <= v3; vr++) {
         IRExpr* next = binop(Iop_Add64, current, mkU64(16));
         store(current, get_vr_qw(vr));
         current = next;
   }
}

static void
s390_irgen_VUPx(UChar v1, UChar m3, Bool is_signed, IRExpr* val)
{
   s390_insn_assert(m3 <= 3);
   put_vr_qw(v1, s390_V128_unpack(val, m3, is_signed));
}

static void
s390_irgen_VUPH(UChar v1, UChar v2, UChar m3)
{
   s390_irgen_VUPx(v1, m3, True, get_vr_dw0(v2));
}

static void
s390_irgen_VUPLH(UChar v1, UChar v2, UChar m3)
{
   s390_irgen_VUPx(v1, m3, False, get_vr_dw0(v2));
}

static void
s390_irgen_VUPL(UChar v1, UChar v2, UChar m3)
{
   s390_irgen_VUPx(v1, m3, True, get_vr_dw1(v2));
}

static void
s390_irgen_VUPLL(UChar v1, UChar v2, UChar m3)
{
   s390_irgen_VUPx(v1, m3, False, get_vr_dw1(v2));
}

static void
s390_irgen_VREP(UChar v1, UChar v3, UShort i2, UChar m4)
{
   s390_insn_assert(m4 <= 3);
   s390_insn_assert((m4 == 0 && i2 < 16) || (m4 == 1 && i2 < 8) ||
                    (m4 == 2 && i2 < 4) || (m4 == 3 && i2 < 2));

   IRType type = s390_vr_get_type(m4);
   IRExpr* arg = get_vr(v3, type, i2);
   put_vr_qw(v1, s390_V128_fill(arg));
}

static void
s390_irgen_VREPI(UChar v1, UShort i2, UChar m3)
{
   s390_insn_assert(m3 <= 3);
   put_vr_qw(v1, s390_V128_fillnum((Short)i2, m3));
}

static void
s390_irgen_VPKS(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert(m4 >= 1 && m4 <= 3);

   if (!s390_vr_is_cs_set(m5)) {
      const IROp ops[] = { Iop_QNarrowBin16Sto8Sx16, Iop_QNarrowBin32Sto16Sx8,
                           Iop_QNarrowBin64Sto32Sx4 };
      Char index = m4 - 1;
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
}

static void
s390_irgen_VPKLS(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert(m4 >= 1 && m4 <= 3);

   if (!s390_vr_is_cs_set(m5)) {
      const IROp ops[] = { Iop_QNarrowBin16Uto8Ux16, Iop_QNarrowBin32Uto16Ux8,
                           Iop_QNarrowBin64Uto32Ux4 };
      Char index = m4 - 1;
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
}

static void
s390_irgen_VSEL(UChar v1, UChar v2, UChar v3, UChar v4)
{
   IRExpr* vIfTrue = get_vr_qw(v2);
   IRExpr* vIfFalse = get_vr_qw(v3);
   IRExpr* vCond = get_vr_qw(v4);

   put_vr_qw(v1, s390_V128_bitwiseITE(vCond, vIfTrue, vIfFalse));
}

static void
s390_irgen_VLBB(UChar v1, IRTemp addr, UChar m3)
{
   s390_insn_assert(m3 <= 6);

   IRExpr* maxIndex = binop(Iop_Sub32,
                            s390_getCountToBlockBoundary(addr, m3),
                            mkU32(1));

   s390_vr_loadWithLength(v1, addr, maxIndex, False);
}

static void
s390_irgen_VLL(UChar v1, IRTemp addr, UChar r3)
{
   s390_vr_loadWithLength(v1, addr, get_gpr_w1(r3), False);
}

static void
s390_irgen_VLRL(UChar v1, IRTemp addr, UChar i3)
{
   if (! s390_host_has_vxd) {
      emulation_failure(EmFail_S390X_vxd);
      return;
   }

   s390_insn_assert((i3 & 0xf0) == 0);

   s390_vr_loadWithLength(v1, addr, mkU32((UInt) i3), True);
}

static void
s390_irgen_VLRLR(UChar v1, UChar r3, IRTemp addr)
{
   if (! s390_host_has_vxd) {
      emulation_failure(EmFail_S390X_vxd);
      return;
   }

   s390_vr_loadWithLength(v1, addr, get_gpr_w1(r3), True);
}

static void
s390_irgen_VSTL(UChar v1, IRTemp addr, UChar r3)
{
   s390_vr_storeWithLength(v1, addr, get_gpr_w1(r3), False);
}

static void
s390_irgen_VSTRL(UChar v1, IRTemp addr, UChar i3)
{
   if (! s390_host_has_vxd) {
      emulation_failure(EmFail_S390X_vxd);
      return;
   }

   s390_insn_assert((i3 & 0xf0) == 0);

   s390_vr_storeWithLength(v1, addr, mkU32((UInt) i3), True);
}

static void
s390_irgen_VSTRLR(UChar v1, UChar r3, IRTemp addr)
{
   if (! s390_host_has_vxd) {
      emulation_failure(EmFail_S390X_vxd);
      return;
   }

   s390_vr_storeWithLength(v1, addr, get_gpr_w1(r3), True);
}

static void
s390_irgen_VX(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, binop(Iop_XorV128, get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VN(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, binop(Iop_AndV128, get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VO(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, binop(Iop_OrV128, get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VOC(UChar v1, UChar v2, UChar v3)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   put_vr_qw(v1, binop(Iop_OrV128, get_vr_qw(v2),
                       unop(Iop_NotV128, get_vr_qw(v3))));
}

static void
s390_irgen_VNN(UChar v1, UChar v2, UChar v3)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   put_vr_qw(v1, unop(Iop_NotV128,
                      binop(Iop_AndV128, get_vr_qw(v2), get_vr_qw(v3))));
}

static void
s390_irgen_VNO(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, unop(Iop_NotV128,
                      binop(Iop_OrV128, get_vr_qw(v2), get_vr_qw(v3))));
}

static void
s390_irgen_VNX(UChar v1, UChar v2, UChar v3)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   put_vr_qw(v1, unop(Iop_NotV128,
                      binop(Iop_XorV128, get_vr_qw(v2), get_vr_qw(v3))));
}

static void
s390_irgen_LZRF(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, binop(Iop_And32, load(Ity_I32, mkexpr(op2addr)), mkU32(0xffffff00)));
   put_gpr_w1(r1, mkexpr(op2));
}

static void
s390_irgen_LZRG(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, binop(Iop_And64, load(Ity_I64, mkexpr(op2addr)), mkU64(0xffffffffffffff00UL)));
   put_gpr_dw0(r1, mkexpr(op2));
}

static void
s390_irgen_LLZRGF(UChar r1, IRTemp op2addr)
{
   IRTemp op2 = newTemp(Ity_I32);

   assign(op2, binop(Iop_And32, load(Ity_I32, mkexpr(op2addr)), mkU32(0xffffff00)));
   put_gpr_w1(r1, mkexpr(op2));
   put_gpr_w0(r1, mkU32(0));
}

static void
s390_irgen_LOCFH(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   put_gpr_w0(r1, load(Ity_I32, mkexpr(op2addr)));
}

static void
s390_irgen_LOCFHR(UChar m3, UChar r1, UChar r2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_w0(r1, get_gpr_w0(r2));
}

static void
s390_irgen_LOCHHI(UChar r1, UChar m3, UShort i2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_w0(r1, mkU32((UInt)(Int)(Short)i2));
}

static void
s390_irgen_LOCHI(UChar r1, UChar m3, UShort i2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_w1(r1, mkU32((UInt)(Int)(Short)i2));
}

static void
s390_irgen_LOCGHI(UChar r1, UChar m3, UShort i2)
{
   next_insn_if(binop(Iop_CmpEQ32, s390_call_calculate_cond(m3), mkU32(0)));
   put_gpr_dw0(r1, mkU64((ULong)(Long)(Short)i2));
}

static void
s390_irgen_STOCFH(UChar r1, IRTemp op2addr)
{
   /* condition is checked in format handler */
   store(mkexpr(op2addr), get_gpr_w1(r1));
}

static void
s390_irgen_LCBB(UChar r1, IRTemp op2addr, UChar m3)
{
   s390_insn_assert(m3 <= 6);

   IRTemp op2 = newTemp(Ity_I32);
   assign(op2, s390_getCountToBlockBoundary(op2addr, m3));
   put_gpr_w1(r1, mkexpr(op2));

   IRExpr* cc = mkite(binop(Iop_CmpEQ32, mkexpr(op2), mkU32(16)), mkU64(0), mkU64(3));
   s390_cc_thunk_fill(mkU64(S390_CC_OP_SET), cc, mkU64(0), mkU64(0));
}

static void
s390_irgen_PRNO(UChar r1, UChar r2)
{
   if (!s390_host_has_msa5) {
      emulation_failure(EmFail_S390X_prno);
      return;
   }

   /* Check for obvious specification exceptions */
   s390_insn_assert(r1 % 2 == 0 && r2 % 2 == 0 && r1 != 0 && r2 != 0);

   extension(S390_EXT_PRNO, r1 | (r2 << 4));
}

static void
s390_irgen_DFLTCC(UChar r3, UChar r1, UChar r2)
{
   if (!s390_host_has_dflt) {
      emulation_failure(EmFail_S390X_dflt);
      return;
   }

   /* Check for obvious specification exceptions */
   s390_insn_assert(r1 % 2 == 0 && r1 != 0 &&
                    r2 % 2 == 0 && r2 != 0 && r3 >= 2);

   extension(S390_EXT_DFLT, r1 | (r2 << 4) | (r3 << 8));
}

enum s390_VStrX {
   s390_VStrX_VSTRC,
   s390_VStrX_VFAE,
   s390_VStrX_VFEE
};

#define S390_VEC_OP3(m, op0, op1, op2)                                  \
   ((m) == 0 ? op0 : (m) == 1 ? op1 : (m) == 2 ? op2 : Iop_INVALID)

/* Helper function for transforming VSTRC, VFAE, or VFEE.  These instructions
   share much of the same logic. */
static void
s390_irgen_VStrX(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5,
                 UChar m6, enum s390_VStrX which_insn)
{
   IRTemp op2 = newTemp(Ity_V128);
   IRTemp op3 = newTemp(Ity_V128);
   IRExpr* tmp;
   IRExpr* match = NULL;
   UChar bitwidth = 8 << m5;
   UChar n_elem = 16 >> m5;
   IROp sub_op = S390_VEC_OP3(m5, Iop_Sub8x16, Iop_Sub16x8, Iop_Sub32x4);
   IROp sar_op = S390_VEC_OP3(m5, Iop_SarN8x16, Iop_SarN16x8, Iop_SarN32x4);
   IROp shl_op = S390_VEC_OP3(m5, Iop_ShlN8x16, Iop_ShlN16x8, Iop_ShlN32x4);
   IROp dup_op = S390_VEC_OP3(m5, Iop_Dup8x16, Iop_Dup16x8, Iop_Dup32x4);
   IROp cmpeq_op = S390_VEC_OP3(m5, Iop_CmpEQ8x16,
                                    Iop_CmpEQ16x8, Iop_CmpEQ32x4);
   IROp cmpgt_op = S390_VEC_OP3(m5, Iop_CmpGT8Ux16,
                                    Iop_CmpGT16Ux8, Iop_CmpGT32Ux4);
   IROp getelem_op = S390_VEC_OP3(m5, Iop_GetElem8x16,
                                      Iop_GetElem16x8, Iop_GetElem32x4);

   assign(op2, get_vr_qw(v2));
   assign(op3, get_vr_qw(v3));

   switch (which_insn) {

   case s390_VStrX_VSTRC: {
      IRTemp op4 = newTemp(Ity_V128);
      assign(op4, get_vr_qw(v4));

      /* Mask off insignificant range boundaries from op3, i.e., all those for
         which the corresponding field in op4 has all or no bits set ("match
         always" / "match never"). */
      IRTemp bounds = newTemp(Ity_V128);
      tmp = unop(Iop_NotV128,
                 binop(cmpeq_op, mkV128(0),
                       binop(sar_op,
                             binop(sub_op,
                                   binop(sar_op, mkexpr(op4),
                                         mkU8(bitwidth - 3)),
                                   mkV128(-1)),
                             mkU8(1))));
      assign(bounds, binop(Iop_AndV128, mkexpr(op3), tmp));

      IRTemp flags_eq = newTemp(Ity_V128);
      IRTemp flags_lt = newTemp(Ity_V128);
      IRTemp flags_gt = newTemp(Ity_V128);
      assign(flags_eq, binop(sar_op, mkexpr(op4), mkU8(bitwidth - 1)));
      assign(flags_lt, binop(sar_op, binop(shl_op, mkexpr(op4), mkU8(1)),
                             mkU8(bitwidth - 1)));
      assign(flags_gt, binop(sar_op, binop(shl_op, mkexpr(op4), mkU8(2)),
                             mkU8(bitwidth - 1)));

      for (UChar idx = 0; idx < n_elem; idx += 2) {
         /* Match according to the even/odd pairs in op3 and op4 at idx */
         IRTemp part[2];

         for (UChar j = 0; j < 2; j++) {
            IRTemp a = newTemp(Ity_V128);
            assign(a, unop(dup_op,
                           binop(getelem_op, mkexpr(bounds), mkU8(idx + j))));

            IRExpr* m[] = {
               binop(cmpeq_op, mkexpr(op2), mkexpr(a)),
               binop(cmpgt_op, mkexpr(a), mkexpr(op2)),
               binop(cmpgt_op, mkexpr(op2), mkexpr(a))
            };
            IRExpr* f[] = {
               unop(dup_op, binop(getelem_op, mkexpr(flags_eq), mkU8(idx + j))),
               unop(dup_op, binop(getelem_op, mkexpr(flags_lt), mkU8(idx + j))),
               unop(dup_op, binop(getelem_op, mkexpr(flags_gt), mkU8(idx + j)))
            };
            part[j] = newTemp(Ity_V128);
            assign(part[j], binop(Iop_OrV128,
                                  binop(Iop_OrV128,
                                        binop(Iop_AndV128, f[0], m[0]),
                                        binop(Iop_AndV128, f[1], m[1])),
                                  binop(Iop_AndV128, f[2], m[2])));
         }
         tmp = binop(Iop_AndV128, mkexpr(part[0]), mkexpr(part[1]));
         match = idx == 0 ? tmp : binop(Iop_OrV128, match, tmp);
      }
      break;
   }

   case s390_VStrX_VFAE:
      for (UChar idx = 0; idx < n_elem; idx++) {
         IRTemp a = newTemp(Ity_V128);
         assign(a, binop(cmpeq_op, mkexpr(op2),
                         unop(dup_op,
                              binop(getelem_op, mkexpr(op3), mkU8(idx)))));
         match = idx == 0 ? mkexpr(a) : binop(Iop_OrV128, match, mkexpr(a));
      }
      break;

   case s390_VStrX_VFEE:
      match = binop(cmpeq_op, mkexpr(op2), mkexpr(op3));
      break;

   default:
      vpanic("s390_irgen_VStrX: unknown insn");
   }

   /* Invert first intermediate result if requested */
   if (m6 & 8)
      match = unop(Iop_NotV128, match);

   IRTemp inter1 = newTemp(Ity_V128);
   IRTemp inter2 = newTemp(Ity_V128);
   IRTemp accu = newTemp(Ity_V128);
   assign(inter1, match);

   /* Determine second intermediate and accumulated result */
   if (s390_vr_is_zs_set(m6)) {
      assign(inter2, binop(cmpeq_op, mkexpr(op2), mkV128(0)));
      assign(accu, binop(Iop_OrV128, mkexpr(inter1), mkexpr(inter2)));
   } else {
      assign(inter2, mkV128(0));
      assign(accu, mkexpr(inter1));
   }

   IRTemp accu0 = newTemp(Ity_I64);
   IRTemp is_match0 = newTemp(Ity_I1);
   IRTemp mismatch_bits = newTemp(Ity_I64);

   assign(accu0, unop(Iop_V128HIto64, mkexpr(accu)));
   assign(is_match0, binop(Iop_ExpCmpNE64, mkexpr(accu0), mkU64(0)));
   assign(mismatch_bits, unop(Iop_ClzNat64,
                              mkite(mkexpr(is_match0), mkexpr(accu0),
                                    unop(Iop_V128to64, mkexpr(accu)))));

   if (m6 & 4) {
      put_vr_qw(v1, mkexpr(inter1));
   } else {
      /* Determine byte position of first match */
      tmp = binop(Iop_Add64,
                  binop(Iop_Shr64, mkexpr(mismatch_bits), mkU8(3)),
                  mkite(mkexpr(is_match0), mkU64(0), mkU64(8)));
      put_vr_qw(v1, binop(Iop_64HLtoV128, tmp, mkU64(0)));
   }

   if (s390_vr_is_cs_set(m6)) {
      /* Set condition code depending on...
                   zero found
                      n  y
                    +------
         match    n | 3  0
          found   y | 1  2   */

      IRTemp cc = newTemp(Ity_I64);

      tmp = binop(Iop_Shr64,
                  mkite(mkexpr(is_match0),
                        unop(Iop_V128HIto64, mkexpr(inter1)),
                        unop(Iop_V128to64, mkexpr(inter1))),
                  unop(Iop_64to8,
                       binop(Iop_Sub64, mkU64(63), mkexpr(mismatch_bits))));
      tmp = binop(Iop_Shl64, tmp, mkU8(1));
      if (s390_vr_is_zs_set(m6)) {
         tmp = binop(Iop_Xor64, tmp,
                     mkite(binop(Iop_ExpCmpNE64, mkU64(0),
                                 binop(Iop_Or64,
                                       unop(Iop_V128HIto64, mkexpr(inter2)),
                                       unop(Iop_V128to64, mkexpr(inter2)))),
                           mkU64(0),
                           mkU64(3)));
      } else {
         tmp = binop(Iop_Xor64, tmp, mkU64(3));
      }
      assign(cc, tmp);
      s390_cc_set(cc);
   }
   dis_res->hint = Dis_HintVerbose;
}

static void
s390_irgen_VFAE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert(m4 <= 2);
   s390_irgen_VStrX(v1, v2, v3, 255, m4, m5, s390_VStrX_VFAE);
}

static void
s390_irgen_VFEE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert(m4 < 3 && m5 == (m5 & 3));
   s390_irgen_VStrX(v1, v2, v3, 255, m4, m5, s390_VStrX_VFEE);
}

static void
s390_irgen_VFENE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert(m4 < 3 && m5 == (m5 & 3));

   static const IROp compare_op[3] = {
      Iop_CmpEQ8x16, Iop_CmpEQ16x8, Iop_CmpEQ32x4
   };
   static const IROp abs_op[3] = {
      Iop_Abs8x16, Iop_Abs16x8, Iop_Abs32x4
   };
   IRTemp op2 = newTemp(Ity_V128);
   IRTemp op3 = newTemp(Ity_V128);
   IRTemp op2zero = newTemp(Ity_V128);
   IRTemp diff = newTemp(Ity_V128);
   IRTemp diff0 = newTemp(Ity_I64);
   IRTemp neq0 = newTemp(Ity_I1);
   IRTemp samebits = newTemp(Ity_I64);
   IRExpr* tmp;

   assign(op2, get_vr_qw(v2));
   assign(op3, get_vr_qw(v3));

   tmp = mkV128(0);
   if (s390_vr_is_zs_set(m5)) {
      tmp = binop(compare_op[m4], mkexpr(op2), tmp);
      if (s390_vr_is_cs_set(m5) && v3 != v2) {
         /* Count leading equal bits in the terminating element too */
         tmp = unop(abs_op[m4], tmp);
      }
      assign(op2zero, tmp);
      tmp = mkexpr(op2zero);
   }
   if (v3 != v2) {
      tmp = binop(Iop_XorV128, mkexpr(op2), mkexpr(op3));
      if (s390_vr_is_zs_set(m5))
         tmp = binop(Iop_OrV128, tmp, mkexpr(op2zero));
   }

   assign(diff, tmp);
   assign(diff0, unop(Iop_V128HIto64, mkexpr(diff)));
   assign(neq0, binop(Iop_ExpCmpNE64, mkexpr(diff0), mkU64(0)));
   assign(samebits, unop(Iop_ClzNat64,
                         mkite(mkexpr(neq0), mkexpr(diff0),
                               unop(Iop_V128to64, mkexpr(diff)))));

   /* Determine the byte size of the initial equal-elements sequence */
   tmp = binop(Iop_Shr64, mkexpr(samebits), mkU8(m4 + 3));
   if (m4 != 0)
      tmp = binop(Iop_Shl64, tmp, mkU8(m4));
   tmp = binop(Iop_Add64, tmp, mkite(mkexpr(neq0), mkU64(0), mkU64(8)));
   put_vr_qw(v1, binop(Iop_64HLtoV128, tmp, mkU64(0)));

   if (s390_vr_is_cs_set(m5)) {
      /* Set condition code like follows --
         0: operands equal up to and including zero element
         1: op2 < op3    2: op2 > op3    3: op2 = op3 */
      IRTemp cc = newTemp(Ity_I64);
      if (v3 == v2) {
         tmp = mkU64(0);
      } else {
         IRTemp shift = newTemp(Ity_I8);
         IRExpr* op2half = mkite(mkexpr(neq0),
                                 unop(Iop_V128HIto64, mkexpr(op2)),
                                 unop(Iop_V128to64, mkexpr(op2)));
         IRExpr* op3half = mkite(mkexpr(neq0),
                                 unop(Iop_V128HIto64, mkexpr(op3)),
                                 unop(Iop_V128to64, mkexpr(op3)));
         assign(shift, unop(Iop_64to8,
                            binop(Iop_Sub64, mkU64(63), mkexpr(samebits))));
         tmp = binop(Iop_Or64,
                     binop(Iop_Shl64,
                           binop(Iop_And64, mkU64(1),
                                 binop(Iop_Shr64, op2half, mkexpr(shift))),
                           mkU8(1)),
                     binop(Iop_And64, mkU64(1),
                           binop(Iop_Shr64, op3half, mkexpr(shift))));
      }
      assign(cc, mkite(binop(Iop_CmpEQ64, mkexpr(samebits), mkU64(64)),
                       mkU64(3), tmp));
      s390_cc_set(cc);
   }
   dis_res->hint = Dis_HintVerbose;
}

static void
s390_irgen_VISTR(
   UChar v1, UChar v2, UChar m3, UChar m4 __attribute__((unused)), UChar m5)
{
   s390_insn_assert(m3 < 3 && m5 == (m5 & 1));

   static const IROp compare_op[3] = {
      Iop_CmpEQ8x16, Iop_CmpEQ16x8, Iop_CmpEQ32x4
   };
   IRExpr* t;
   IRTemp op2 = newTemp(Ity_V128);
   IRTemp op2term = newTemp(Ity_V128);
   IRTemp mask = newTemp(Ity_V128);

   assign(op2, get_vr_qw(v2));
   assign(op2term, binop(compare_op[m3], mkexpr(op2), mkV128(0)));
   t = mkexpr(op2term);

   for (UChar i = m3; i < 4; i++) {
      IRTemp s = newTemp(Ity_V128);
      assign(s, binop(Iop_OrV128, t, binop(Iop_ShrV128, t, mkU8(8 << i))));
      t = mkexpr(s);
   }
   assign(mask, unop(Iop_NotV128, t));
   put_vr_qw(v1, binop(Iop_AndV128, mkexpr(op2), mkexpr(mask)));

   if (s390_vr_is_cs_set(m5)) {
      IRTemp cc = newTemp(Ity_I64);
      assign(cc, binop(Iop_And64, mkU64(3), unop(Iop_V128to64, mkexpr(mask))));
      s390_cc_set(cc);
   }
   dis_res->hint = Dis_HintVerbose;
}

static void
s390_irgen_VSTRC(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   s390_insn_assert(m5 <= 2);
   s390_irgen_VStrX(v1, v2, v3, v4, m5, m6, s390_VStrX_VSTRC);
}

static void
s390_irgen_VSTRS(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m5 <= 2 && m6 == (m6 & 2));

   IRTemp op2 = newTemp(Ity_V128);
   IRTemp op3 = newTemp(Ity_V128);
   IRTemp op4 = newTemp(Ity_I8);
   IRTemp op2clean = newTemp(Ity_V128);
   IRTemp op3mask = newTemp(Ity_V128);
   IRTemp result = newTemp(Ity_V128);
   IRTemp ccnomatch = newTemp(Ity_I64);
   IRExpr* tmp;
   IRExpr* match = NULL;
   UChar elem_bits = 8 << m5;
   IROp cmpeq_op = S390_VEC_OP3(m5, Iop_CmpEQ8x16,
                                Iop_CmpEQ16x8, Iop_CmpEQ32x4);

   assign(op2, get_vr_qw(v2));
   assign(op3, get_vr_qw(v3));
   assign(op4, get_vr_b7(v4));

   tmp = unop(Iop_Dup32x4,
              unop(Iop_1Sto32, binop(Iop_CmpNE8, mkexpr(op4), mkU8(16))));
   tmp = binop(Iop_ShrV128, tmp, binop(Iop_Shl8, mkexpr(op4), mkU8(3)));

   if (s390_vr_is_zs_set(m6)) {
      IRTemp op2eos = newTemp(Ity_V128);
      IRExpr* t;
      t = binop(cmpeq_op, mkexpr(op2), mkV128(0));
      for (UChar i = m5; i < 4; i++) {
         IRTemp s = newTemp(Ity_V128);
         assign(s, t);
         t = binop(Iop_OrV128, mkexpr(s), binop(Iop_ShrV128, mkexpr(s),
                                                mkU8(8 << i)));
      }
      assign(op2eos, t);
      assign(op2clean, binop(Iop_AndV128, mkexpr(op2),
                             unop(Iop_NotV128, mkexpr(op2eos))));
      assign(ccnomatch, binop(Iop_And64, mkU64(1),
                              unop(Iop_V128to64, mkexpr(op2eos))));

      t = binop(cmpeq_op, mkexpr(op3), mkV128(0));
      for (UChar i = m5; i < 4; i++) {
         IRTemp s = newTemp(Ity_V128);
         assign(s, t);
         t = binop(Iop_OrV128, mkexpr(s), binop(Iop_ShrV128, mkexpr(s),
                                                mkU8(8 << i)));
      }
      tmp = binop(Iop_OrV128, tmp, t);
   } else {
      assign(op2clean, mkexpr(op2));
   }
   assign(op3mask, unop(Iop_NotV128, tmp));

   for (UChar shift = 0; shift < 128; shift += elem_bits) {
      IRTemp s = newTemp(Ity_V128);
      tmp = unop(Iop_NotV128,
                 binop(cmpeq_op, mkexpr(op2clean),
                       binop(Iop_ShrV128, mkexpr(op3), mkU8(shift))));
      assign(s, binop(Iop_CmpEQ64x2, mkV128(0),
                      binop(Iop_AndV128, mkexpr(op3mask),
                            binop(Iop_ShlV128, tmp, mkU8(shift)))));
      tmp = mkexpr(s);
      if (shift < 64) {
         tmp = binop(Iop_AndV128, tmp,
                     unop(Iop_Dup16x8, binop(Iop_GetElem16x8, tmp, mkU8(4))));
      }
      tmp = binop(Iop_AndV128, tmp,
                  unop(Iop_Dup16x8, mkU16(1 << (15 - shift / 8))));
      if (shift)
         match = binop(Iop_OrV128, mkexpr(mktemp(Ity_V128, match)), tmp);
      else
         match = tmp;
   }
   assign(result, unop(Iop_ClzNat64,
                       binop(Iop_Or64,
                             unop(Iop_V128HIto64, match),
                             mkU64((1ULL << 48) - 1))));
   put_vr_qw(v1, binop(Iop_64HLtoV128, mkexpr(result), mkU64(0)));

   /* Set condition code.
      0: no match, no string terminator in op2
      1: no match, string terminator found
      2: full match
      3: partial match */
   IRTemp cc = newTemp(Ity_I64);
   tmp = binop(Iop_CmpLE64U,
               binop(Iop_Add64, mkexpr(result), unop(Iop_8Uto64, mkexpr(op4))),
               mkU64(16));
   assign(cc, mkite(binop(Iop_CmpEQ64, mkexpr(result), mkU64(16)),
                    s390_vr_is_zs_set(m6) ? mkexpr(ccnomatch) : mkU64(0),
                    mkite(tmp, mkU64(2), mkU64(3))));
   s390_cc_set(cc);

   dis_res->hint = Dis_HintVerbose;
}

static void
s390_irgen_VNC(UChar v1, UChar v2, UChar v3)
{
   put_vr_qw(v1, binop(Iop_AndV128,
             get_vr_qw(v2), unop(Iop_NotV128, get_vr_qw(v3)))
             );
}

static void
s390_irgen_VA(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);
   put_vr_qw(v1, s390_V128_add(get_vr_qw(v2), get_vr_qw(v3), m4));
}

static void
s390_irgen_VS(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   put_vr_qw(v1, s390_V128_sub(get_vr_qw(v2), get_vr_qw(v3), m4));
}

static void
s390_irgen_VMX(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   const IROp ops[] = {Iop_Max8Sx16, Iop_Max16Sx8, Iop_Max32Sx4, Iop_Max64Sx2};
   IRExpr*    max;

   if (m4 < 4) {
      max = binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3));
   } else {
      IRTemp a = newTemp(Ity_V128);
      IRTemp b = newTemp(Ity_V128);
      assign(a, get_vr_qw(v2));
      assign(b, get_vr_qw(v3));
      max = s390_V128_bitwiseITE(s390_V128_CmpGTS(mkexpr(a), mkexpr(b), m4),
                                 mkexpr(a), mkexpr(b));
   }
   put_vr_qw(v1, max);
}

static void
s390_irgen_VMXL(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   const IROp ops[] = {Iop_Max8Ux16, Iop_Max16Ux8, Iop_Max32Ux4, Iop_Max64Ux2};
   IRExpr*    max;

   if (m4 < 4) {
      max = binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3));
   } else {
      IRTemp a = newTemp(Ity_V128);
      IRTemp b = newTemp(Ity_V128);
      assign(a, get_vr_qw(v2));
      assign(b, get_vr_qw(v3));
      max = s390_V128_bitwiseITE(s390_V128_CmpGTU(mkexpr(a), mkexpr(b), m4),
                                 mkexpr(a), mkexpr(b));
   }
   put_vr_qw(v1, max);
}

static void
s390_irgen_VMN(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   const IROp ops[] = {Iop_Min8Sx16, Iop_Min16Sx8, Iop_Min32Sx4, Iop_Min64Sx2};
   IRExpr*    min;

   if (m4 < 4) {
      min = binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3));
   } else {
      IRTemp a = newTemp(Ity_V128);
      IRTemp b = newTemp(Ity_V128);
      assign(a, get_vr_qw(v2));
      assign(b, get_vr_qw(v3));
      IRTemp diff = newTemp(Ity_V128);
      assign(diff, binop(Iop_Sub128x1, mkexpr(a), mkexpr(b)));
      min = s390_V128_bitwiseITE(s390_V128_CmpGTS(mkexpr(a), mkexpr(b), m4),
                                 mkexpr(b), mkexpr(a));
   }
   put_vr_qw(v1, min);
}

static void
s390_irgen_VMNL(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   const IROp ops[] = {Iop_Min8Ux16, Iop_Min16Ux8, Iop_Min32Ux4, Iop_Min64Ux2};
   IRExpr*    min;

   if (m4 < 4) {
      min = binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3));
   } else {
      IRTemp a = newTemp(Ity_V128);
      IRTemp b = newTemp(Ity_V128);
      assign(a, get_vr_qw(v2));
      assign(b, get_vr_qw(v3));
      IRTemp diff = newTemp(Ity_V128);
      assign(diff, binop(Iop_Sub128x1, mkexpr(a), mkexpr(b)));
      min = s390_V128_bitwiseITE(s390_V128_CmpGTU(mkexpr(a), mkexpr(b), m4),
                                 mkexpr(b), mkexpr(a));
   }
   put_vr_qw(v1, min);
}

static void
s390_irgen_VAVG(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   const IROp ops[] = {Iop_Avg8Sx16, Iop_Avg16Sx8, Iop_Avg32Sx4, Iop_Avg64Sx2};
   IRExpr*    avg;

   if (m4 < 4) {
      avg = binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3));
   } else {
      // a/2 + b/2 + ((a | b) & 1)
      IRTemp a = newTemp(Ity_V128);
      IRTemp b = newTemp(Ity_V128);
      assign(a, get_vr_qw(v2));
      assign(b, get_vr_qw(v3));
      avg = binop(Iop_Add128x1,
                  binop(Iop_Add128x1, binop(Iop_SarV128, mkexpr(a), mkU8(1)),
                        binop(Iop_SarV128, mkexpr(b), mkU8(1))),
                  binop(Iop_AndV128, s390_V128_fillnum(1, m4),
                        binop(Iop_OrV128, mkexpr(a), mkexpr(b))));
   }
   put_vr_qw(v1, avg);
}

static void
s390_irgen_VAVGL(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   const IROp ops[] = {Iop_Avg8Ux16, Iop_Avg16Ux8, Iop_Avg32Ux4, Iop_Avg64Ux2};
   IRExpr*    avg;

   if (m4 < 4) {
      avg = binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3));
   } else {
      // a/2 + b/2 + ((a | b) & 1)
      IRTemp a = newTemp(Ity_V128);
      IRTemp b = newTemp(Ity_V128);
      assign(a, get_vr_qw(v2));
      assign(b, get_vr_qw(v3));
      avg = binop(Iop_Add128x1,
                  binop(Iop_Add128x1, binop(Iop_ShrV128, mkexpr(a), mkU8(1)),
                        binop(Iop_ShrV128, mkexpr(b), mkU8(1))),
                  binop(Iop_AndV128, s390_V128_fillnum(1, m4),
                        binop(Iop_OrV128, mkexpr(a), mkexpr(b))));
   }
   put_vr_qw(v1, avg);
}

static void
s390_irgen_VLC(UChar v1, UChar v2, UChar m3)
{
   s390_insn_assert(m3 <= 4);
   put_vr_qw(v1, s390_V128_get_complement(get_vr_qw(v2), m3));
}

static void
s390_irgen_VLP(UChar v1, UChar v2, UChar m3)
{
   s390_insn_assert(m3 <= 4);

   if (m3 < 4) {
      const IROp ops[] = {Iop_Abs8x16, Iop_Abs16x8, Iop_Abs32x4, Iop_Abs64x2};
      put_vr_qw(v1, unop(ops[m3], get_vr_qw(v2)));
   } else {
      IRTemp op = newTemp(Ity_V128);
      assign(op, get_vr_qw(v2));
      put_vr_qw(v1,
                s390_V128_bitwiseITE(
                   s390_V128_high_set(mkexpr(op), m3),
                   s390_V128_get_complement(mkexpr(op), m3), mkexpr(op)));
   }
}

static void
s390_irgen_VCEQ(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert(m4 <= 4);

   IRTemp res = newTemp(Ity_V128);
   assign(res, s390_V128_CmpEQ(get_vr_qw(v2), get_vr_qw(v3), m4));
   put_vr_qw(v1, mkexpr(res));
   if (s390_vr_is_cs_set(m5))
      s390_V128_setcc_for_cmp(res, m4);
}

static void
s390_irgen_VCH(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert(m4 <= 4);

   IRTemp res = newTemp(Ity_V128);
   assign(res, s390_V128_CmpGTS(get_vr_qw(v2), get_vr_qw(v3), m4));
   put_vr_qw(v1, mkexpr(res));
   if (s390_vr_is_cs_set(m5))
      s390_V128_setcc_for_cmp(res, m4);
}

static void
s390_irgen_VCHL(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_insn_assert(m4 <= 4);

   IRTemp res = newTemp(Ity_V128);
   assign(res, s390_V128_CmpGTU(get_vr_qw(v2), get_vr_qw(v3), m4));
   put_vr_qw(v1, mkexpr(res));
   if (s390_vr_is_cs_set(m5))
      s390_V128_setcc_for_cmp(res, m4);
}

static void
s390_irgen_VCLZ(UChar v1, UChar v2, UChar m3)
{
   s390_insn_assert(m3 <= 4);

   if (m3 < 4) {
      const IROp ops[] = {Iop_Clz8x16, Iop_Clz16x8, Iop_Clz32x4, Iop_Clz64x2};
      put_vr_qw(v1, unop(ops[m3], get_vr_qw(v2)));
   } else {
      IRTemp cnt   = newTemp(Ity_V128);
      IRTemp cnt_h = newTemp(Ity_I64);
      assign(cnt, unop(Iop_Clz64x2, get_vr_qw(v2)));
      assign(cnt_h, unop(Iop_V128HIto64, mkexpr(cnt)));
      put_vr_qw(v1,
                binop(Iop_64HLtoV128, mkU64(0),
                      binop(Iop_Add64, mkexpr(cnt_h),
                            mkite(binop(Iop_CmpEQ64, mkexpr(cnt_h), mkU64(64)),
                                  unop(Iop_V128to64, mkexpr(cnt)), mkU64(0)))));
   }
}

static void
s390_irgen_VCTZ(UChar v1, UChar v2, UChar m3)
{
   s390_insn_assert(m3 <= 4);

   if (m3 < 4) {
      const IROp ops[] = {Iop_Ctz8x16, Iop_Ctz16x8, Iop_Ctz32x4, Iop_Ctz64x2};
      put_vr_qw(v1, unop(ops[m3], get_vr_qw(v2)));
   } else {
      IRTemp cnt   = newTemp(Ity_V128);
      IRTemp cnt_l = newTemp(Ity_I64);
      assign(cnt, unop(Iop_Ctz64x2, get_vr_qw(v2)));
      assign(cnt_l, unop(Iop_V128to64, mkexpr(cnt)));
      put_vr_qw(
         v1, binop(Iop_64HLtoV128, mkU64(0),
                   binop(Iop_Add64, mkexpr(cnt_l),
                         mkite(binop(Iop_CmpEQ64, mkexpr(cnt_l), mkU64(64)),
                               unop(Iop_V128HIto64, mkexpr(cnt)), mkU64(0)))));
   }
}

static void
s390_irgen_VPOPCT(UChar v1, UChar v2, UChar m3)
{
   s390_insn_assert(m3 <= 3);

   IRExpr* cnt = unop(Iop_Cnt8x16, get_vr_qw(v2));

   if (m3 >= 1) {
      cnt = unop(Iop_PwAddL8Ux16, cnt);
      if (m3 >= 2) {
         cnt = unop(Iop_PwAddL16Ux8, cnt);
         if (m3 == 3)
            cnt = unop(Iop_PwAddL32Ux4, cnt);
      }
   }
   put_vr_qw(v1, cnt);
}

static void
s390_irgen_VML(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   put_vr_qw(v1, s390_V128_mul(get_vr_qw(v2), get_vr_qw(v3), m4));
}

static void
s390_irgen_VMLH(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   put_vr_qw(v1, s390_V128_mula_high(get_vr_qw(v2), get_vr_qw(v3),
                                     NULL, m4, False));
}

static void
s390_irgen_VMH(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   put_vr_qw(v1, s390_V128_mula_high(get_vr_qw(v2), get_vr_qw(v3),
                                     NULL, m4, True));
}

static void
s390_irgen_VMO(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   IRExpr* a = get_vr_qw(v2);
   IRExpr* b = get_vr_qw(v3);
   put_vr_qw(v1, s390_V128_mul_widen(a, b, m4, True, False));
}

static void
s390_irgen_VMLO(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   IRExpr* a = get_vr_qw(v2);
   IRExpr* b = get_vr_qw(v3);
   put_vr_qw(v1, s390_V128_mul_widen(a, b, m4, False, False));
}

static void
s390_irgen_VESLV(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   const IROp ops[] = { Iop_Shl8x16, Iop_Shl16x8, Iop_Shl32x4, Iop_Shl64x2};
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VESL(UChar v1, IRTemp op2addr, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   IRExpr* shift_amount = unop(Iop_64to8, mkexpr(op2addr));
   const IROp ops[] = { Iop_ShlN8x16, Iop_ShlN16x8, Iop_ShlN32x4, Iop_ShlN64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v3), shift_amount));
}

static void
s390_irgen_VESRAV(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   const IROp ops[] = { Iop_Sar8x16, Iop_Sar16x8, Iop_Sar32x4, Iop_Sar64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VESRA(UChar v1, IRTemp op2addr, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   IRExpr* shift_amount = unop(Iop_64to8, mkexpr(op2addr));
   const IROp ops[] = { Iop_SarN8x16, Iop_SarN16x8, Iop_SarN32x4, Iop_SarN64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v3), shift_amount));
}

static void
s390_irgen_VESRLV(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   const IROp ops[] = { Iop_Shr8x16, Iop_Shr16x8, Iop_Shr32x4, Iop_Shr64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VESRL(UChar v1, IRTemp op2addr, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   IRExpr* shift_amount = unop(Iop_64to8, mkexpr(op2addr));
   const IROp ops[] = { Iop_ShrN8x16, Iop_ShrN16x8, Iop_ShrN32x4, Iop_ShrN64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v3), shift_amount));
}

static void
s390_irgen_VERLLV(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   const IROp ops[] = { Iop_Rol8x16, Iop_Rol16x8, Iop_Rol32x4, Iop_Rol64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v2), get_vr_qw(v3)));
}

static void
s390_irgen_VERLL(UChar v1, IRTemp op2addr, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);
   /*
      There is no Iop_RolN?x?? operations
      so we have to use VECTOR x VECTOR variant.
    */
   IRExpr* shift_vector = unop(Iop_Dup8x16, unop(Iop_64to8, mkexpr(op2addr)));
   const IROp ops[] = { Iop_Rol8x16, Iop_Rol16x8, Iop_Rol32x4, Iop_Rol64x2 };
   put_vr_qw(v1, binop(ops[m4], get_vr_qw(v3), shift_vector));
}

static void
s390_irgen_VSL(UChar v1, UChar v2, UChar v3)
{
   IRTemp a = newTemp(Ity_V128);
   IRTemp b = newTemp(Ity_V128);

   assign(a, get_vr_qw(v2));
   assign(b, get_vr_qw(v3));

   put_vr_qw(v1,
             binop(Iop_OrV128,
                   binop(Iop_Shl8x16, mkexpr(a), mkexpr(b)),
                   binop(Iop_Shr8x16,
                         binop(Iop_Shr8x16,
                               binop(Iop_ShlV128, mkexpr(a), mkU8(8)),
                               unop(Iop_NotV128, mkexpr(b))),
                         unop(Iop_Dup8x16, mkU8(1)))));
}

static void
s390_irgen_VSRL(UChar v1, UChar v2, UChar v3)
{
   IRTemp a = newTemp(Ity_V128);
   IRTemp b = newTemp(Ity_V128);

   assign(a, get_vr_qw(v2));
   assign(b, get_vr_qw(v3));

   put_vr_qw(v1,
             binop(Iop_OrV128,
                   binop(Iop_Shr8x16, mkexpr(a), mkexpr(b)),
                   binop(Iop_Shl8x16,
                         binop(Iop_Shl8x16,
                               binop(Iop_ShrV128, mkexpr(a), mkU8(8)),
                               unop(Iop_NotV128, mkexpr(b))),
                         unop(Iop_Dup8x16, mkU8(1)))));
}

static void
s390_irgen_VSRA(UChar v1, UChar v2, UChar v3)
{
   IRTemp a = newTemp(Ity_V128);
   IRTemp b = newTemp(Ity_V128);

   assign(a, get_vr_qw(v2));
   assign(b, get_vr_qw(v3));

   /* Shift-right: first byte arithmetically, all others logically */
   IRExpr* elems_shifted =
      binop(Iop_Sar8x16,
            binop(Iop_Shr8x16, mkexpr(a),
                  binop(Iop_AndV128, mkexpr(b), mkV128(0x7fff))),
            binop(Iop_AndV128, mkexpr(b), mkV128(0x8000)));
   /* Then OR the appropriate bits from the byte to the left */
   put_vr_qw(v1,
             binop(Iop_OrV128, elems_shifted,
                   binop(Iop_Shl8x16,
                         binop(Iop_Shl8x16,
                               binop(Iop_ShrV128, mkexpr(a), mkU8(8)),
                               unop(Iop_NotV128, mkexpr(b))),
                         unop(Iop_Dup8x16, mkU8(1)))));
}

static void
s390_irgen_VERIM(UChar v1, UChar v2, UChar v3, UChar i4, UChar m5)
{
   s390_insn_assert(m5 <= 3);
   /*
      There is no Iop_RolN?x?? operations
      so we have to use VECTOR x VECTOR variant.
    */
   const IROp ops[] = { Iop_Rol8x16, Iop_Rol16x8, Iop_Rol32x4, Iop_Rol64x2 };
   IRExpr* shift_vector = unop(Iop_Dup8x16, mkU8(i4));
   IRExpr* rotated_vector = binop(ops[m5], get_vr_qw(v2), shift_vector);

   /* result = (result & ~mask) | (rotated_vector & mask) */
   IRExpr* mask = get_vr_qw(v3);
   IRExpr* result = get_vr_qw(v1);
   put_vr_qw(v1, s390_V128_bitwiseITE(mask, rotated_vector, result));
}

static void
s390_irgen_VEC(UChar v1, UChar v2, UChar m3)
{
   s390_insn_assert(m3 <= 4);

   IRTemp op1, op2;

   if (m3 == 4) {
      IRTemp a = newTemp(Ity_V128);
      IRTemp b = newTemp(Ity_V128);

      op1 = newTemp(Ity_I64);
      op2 = newTemp(Ity_I64);
      assign(a, get_vr_qw(v1));
      assign(b, get_vr_qw(v2));
      assign(op1, unop(Iop_V128to64,
                       s390_V128_CmpGT128x1(mkexpr(a), mkexpr(b), True, True)));
      assign(op2, unop(Iop_V128to64,
                       s390_V128_CmpGT128x1(mkexpr(b), mkexpr(a), True, True)));
   } else {
      IRType type = s390_vr_get_type(m3);

      op1 = newTemp(type);
      op2 = newTemp(type);

      switch (type) {
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
   }

   s390_cc_thunk_putSS(S390_CC_OP_SIGNED_COMPARE, op1, op2);
}

static void
s390_irgen_VECL(UChar v1, UChar v2, UChar m3)
{
   s390_insn_assert(m3 <= 4);

   IRTemp op1, op2;

   if (m3 == 4) {
      IRTemp a      = newTemp(Ity_V128);
      IRTemp b      = newTemp(Ity_V128);
      IRTemp a_hi   = newTemp(Ity_I64);
      IRTemp b_hi   = newTemp(Ity_I64);
      IRTemp use_hi = newTemp(Ity_I1);
      op1           = newTemp(Ity_I64);
      op2           = newTemp(Ity_I64);
      assign(a, get_vr_qw(v1));
      assign(b, get_vr_qw(v2));
      assign(a_hi, unop(Iop_V128HIto64, mkexpr(a)));
      assign(b_hi, unop(Iop_V128HIto64, mkexpr(b)));
      assign(use_hi, binop(Iop_CmpNE64, mkexpr(a_hi), mkexpr(b_hi)));
      assign(op1, mkite(mkexpr(use_hi), mkexpr(a_hi),
                        unop(Iop_V128to64, mkexpr(a))));
      assign(op2, mkite(mkexpr(use_hi), mkexpr(b_hi),
                        unop(Iop_V128to64, mkexpr(b))));
   } else {
      IRType type = s390_vr_get_type(m3);

      op1 = newTemp(type);
      op2 = newTemp(type);

      switch (type) {
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
   }

   s390_cc_thunk_putZZ(S390_CC_OP_UNSIGNED_COMPARE, op1, op2);
}


static void
s390_irgen_VSLB(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b01111000)));

   put_vr_qw(v1, binop(Iop_ShlV128, get_vr_qw(v2), mkexpr(shift_amount)));
}

static void
s390_irgen_VSRLB(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b01111000)));

   put_vr_qw(v1, binop(Iop_ShrV128, get_vr_qw(v2), mkexpr(shift_amount)));
}

static void
s390_irgen_VSRAB(UChar v1, UChar v2, UChar v3)
{
   IRTemp shift_amount = newTemp(Ity_I8);
   assign(shift_amount, binop(Iop_And8, get_vr_b7(v3), mkU8(0b01111000)));

   put_vr_qw(v1, binop(Iop_SarV128, get_vr_qw(v2), mkexpr(shift_amount)));
}

static void
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
}

static void
s390_irgen_VSLD(UChar v1, UChar v2, UChar v3, UChar i4)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(i4 <= 7);

   if (i4 == 0) {
      /* Just copy v2. */
      put_vr_qw(v1, get_vr_qw(v2));
   } else {
      /* Concatenate v2's tail with v3's head. */
      put_vr_qw(v1,
                binop(Iop_OrV128,
                      binop(Iop_ShlV128, get_vr_qw(v2), mkU8(i4)),
                      binop(Iop_ShrV128, get_vr_qw(v3), mkU8(128 - i4))
                     )
               );
   }
}

static void
s390_irgen_VSRD(UChar v1, UChar v2, UChar v3, UChar i4)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(i4 <= 7);

   if (i4 == 0) {
      /* Just copy v3. */
      put_vr_qw(v1, get_vr_qw(v3));
   } else {
      /* Concatenate v2's tail with v3's head. */
      put_vr_qw(v1,
                binop(Iop_OrV128,
                      binop(Iop_ShlV128, get_vr_qw(v2), mkU8(128 - i4)),
                      binop(Iop_ShrV128, get_vr_qw(v3), mkU8(i4))
                     )
               );
   }
}

static void
s390_irgen_VME(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   IRExpr* a = get_vr_qw(v2);
   IRExpr* b = get_vr_qw(v3);
   put_vr_qw(v1, s390_V128_mul_widen(a, b, m4, True, True));
}

static void
s390_irgen_VMLE(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

   IRExpr* a = get_vr_qw(v2);
   IRExpr* b = get_vr_qw(v3);
   put_vr_qw(v1, s390_V128_mul_widen(a, b, m4, False, True));
}

static void
s390_irgen_VMAO(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 3);

   IRExpr* a = get_vr_qw(v2);
   IRExpr* b = get_vr_qw(v3);
   put_vr_qw(v1, s390_V128_add(s390_V128_mul_widen(a, b, m5, True, False),
                               get_vr_qw(v4), m5 + 1));
}

static void
s390_irgen_VMALO(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 3);

   IRExpr* a = get_vr_qw(v2);
   IRExpr* b = get_vr_qw(v3);
   put_vr_qw(v1, s390_V128_add(s390_V128_mul_widen(a, b, m5, False, False),
                               get_vr_qw(v4), m5 + 1));
}

static void
s390_irgen_VMAE(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 3);

   IRExpr* a = get_vr_qw(v2);
   IRExpr* b = get_vr_qw(v3);
   put_vr_qw(v1, s390_V128_add(s390_V128_mul_widen(a, b, m5, True, True),
                               get_vr_qw(v4), m5 + 1));
}

static void
s390_irgen_VMALE(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 3);

   IRExpr* a = get_vr_qw(v2);
   IRExpr* b = get_vr_qw(v3);
   put_vr_qw(v1, s390_V128_add(s390_V128_mul_widen(a, b, m5, False, True),
                               get_vr_qw(v4), m5 + 1));
}

static void
s390_irgen_VMAL(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 4);

   put_vr_qw(v1, s390_V128_add(s390_V128_mul(get_vr_qw(v2), get_vr_qw(v3), m5),
                               get_vr_qw(v4), m5));
}

static void
s390_irgen_VSUM(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 1);

   IRType type = s390_vr_get_type(m4);
   IRExpr* mask;
   IRExpr* sum;
   switch(type) {
   case Ity_I8:
      sum = unop(Iop_PwAddL16Ux8, unop(Iop_PwAddL8Ux16, get_vr_qw(v2)));
      mask = mkV128(0b0001000100010001);
      break;
   case Ity_I16:
      sum = unop(Iop_PwAddL16Ux8, get_vr_qw(v2));
      mask = mkV128(0b0011001100110011);
      break;
   default:
      vpanic("s390_irgen_VSUM: invalid type ");
   }

   IRExpr* addition = binop(Iop_AndV128, get_vr_qw(v3), mask);
   put_vr_qw(v1, binop(Iop_Add32x4, sum, addition));
}

static void
s390_irgen_VSUMG(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 == 1 || m4 == 2);

   IRType type = s390_vr_get_type(m4);
   IRExpr* mask;
   IRExpr* sum;
   switch(type) {
   case Ity_I16:
      sum = unop(Iop_PwAddL32Ux4, unop(Iop_PwAddL16Ux8, get_vr_qw(v2)));
      mask = mkV128(0b0000001100000011);
      break;
   case Ity_I32:
      sum = unop(Iop_PwAddL32Ux4, get_vr_qw(v2));
      mask = mkV128(0b0000111100001111);
      break;
   default:
      vpanic("s390_irgen_VSUMG: invalid type ");
   }

   IRExpr* addition = binop(Iop_AndV128, get_vr_qw(v3), mask);
   put_vr_qw(v1, binop(Iop_Add64x2, sum, addition));
}

static void
s390_irgen_VSUMQ(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 == 2 || m4 == 3);

   IRType type = s390_vr_get_type(m4);
   IRExpr* mask;
   IRExpr* sum;
   switch(type) {
   case Ity_I32:
      sum = unop(Iop_PwAddL64Ux2, unop(Iop_PwAddL32Ux4, get_vr_qw(v2)));
      mask = mkV128(0b0000000000001111);
      break;
   case Ity_I64:
      sum = unop(Iop_PwAddL64Ux2, get_vr_qw(v2));
      mask = mkV128(0b0000000011111111);
      break;
   default:
      vpanic("s390_irgen_VSUMQ: invalid type ");
   }

   IRExpr* addition = binop(Iop_AndV128, get_vr_qw(v3), mask);
   put_vr_qw(v1, binop(Iop_Add128x1, sum, addition));
}

static void
s390_irgen_VTM(UChar v1, UChar v2)
{
   IRTemp  op1    = newTemp(Ity_V128);
   IRTemp  op2    = newTemp(Ity_V128);
   IRTemp  masked = newTemp(Ity_V128);
   IRTemp  diff   = newTemp(Ity_V128);
   IRTemp  cc     = newTemp(Ity_I64);
   IRExpr* masked_is_zero;
   IRExpr* diff_is_zero;

   assign(op1, get_vr_qw(v1));
   assign(op2, get_vr_qw(v2));
   assign(masked, binop(Iop_AndV128, mkexpr(op1), mkexpr(op2)));
   assign(diff, binop(Iop_XorV128, mkexpr(op2), mkexpr(masked)));
   masked_is_zero = binop(Iop_CmpEQ64,
                          binop(Iop_Or64, unop(Iop_V128to64, mkexpr(masked)),
                                unop(Iop_V128HIto64, mkexpr(masked))),
                          mkU64(0));
   diff_is_zero   = binop(Iop_CmpEQ64,
                          binop(Iop_Or64, unop(Iop_V128to64, mkexpr(diff)),
                                unop(Iop_V128HIto64, mkexpr(diff))),
                          mkU64(0));
   assign(cc, mkite(masked_is_zero, mkU64(0),
                    mkite(diff_is_zero, mkU64(3), mkU64(1))));
   s390_cc_set(cc);
}

static void
s390_irgen_VAC(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 == 4);

   IRTemp sum = newTemp(Ity_V128);
   assign(sum, binop(Iop_Add128x1, get_vr_qw(v2), get_vr_qw(v3)));

   IRExpr* mask = binop(Iop_64HLtoV128, mkU64(0), mkU64(1));
   IRExpr* carry_in = binop(Iop_AndV128, get_vr_qw(v4), mask);
   put_vr_qw(v1, binop(Iop_Add128x1, mkexpr(sum), carry_in));
}

static void
s390_irgen_VACC(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   IRTemp arg1 = newTemp(Ity_V128);
   assign(arg1, get_vr_qw(v2));
   IRExpr* sum = s390_V128_add(mkexpr(arg1), get_vr_qw(v3), m4);

   put_vr_qw(v1, binop(Iop_AndV128, s390_V128_CmpGTU(mkexpr(arg1), sum, m4),
                       s390_V128_fillnum(1, m4)));
}

static void
s390_irgen_VACCC(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 == 4);

   IRExpr* result =
         s390_V128_calculate_carry_out_with_carry(get_vr_qw(v2),
                                                  get_vr_qw(v3),
                                                  get_vr_qw(v4)
                                                  );

   put_vr_qw(v1, result);
}

static void
s390_irgen_VCKSM(UChar v1, UChar v2, UChar v3)
{

   IRTemp sum1 = s390_checksum_add(get_vr_w1(v3), get_vr_w0(v2));
   IRTemp sum2 = s390_checksum_add(mkexpr(sum1), get_vr_w1(v2));
   IRTemp sum3 = s390_checksum_add(mkexpr(sum2), get_vr_w2(v2));
   IRTemp result = s390_checksum_add(mkexpr(sum3), get_vr_w3(v2));

   put_vr_qw(v1, binop(Iop_64HLtoV128,
                       unop(Iop_32Uto64, mkexpr(result)), mkU64(0ULL)));
}

static void
s390_irgen_VGFM(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 3);

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
}

static void
s390_irgen_VGFMA(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 3);

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
}

static void
s390_irgen_VSBI(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 == 4);

   IRExpr* mask = binop(Iop_64HLtoV128, mkU64(0ULL), mkU64(1ULL));
   IRExpr* carry_in = binop(Iop_AndV128, get_vr_qw(v4), mask);

   IRTemp sum = newTemp(Ity_V128);
   assign(sum, binop(Iop_Add128x1,
                     get_vr_qw(v2),
                     unop(Iop_NotV128, get_vr_qw(v3))
                     )
         );

   put_vr_qw(v1, binop(Iop_Add128x1, mkexpr(sum), carry_in));
}

static void
s390_irgen_VSCBI(UChar v1, UChar v2, UChar v3, UChar m4)
{
   s390_insn_assert(m4 <= 4);

   IRTemp arg1 = newTemp(Ity_V128);
   assign(arg1, get_vr_qw(v2));
   IRExpr* diff = s390_V128_sub(mkexpr(arg1), get_vr_qw(v3), m4);

   put_vr_qw(v1, binop(Iop_AndV128,
                       unop(Iop_NotV128,
                            s390_V128_CmpGTU(diff, mkexpr(arg1), m4)),
                       s390_V128_fillnum(1, m4)));
}

static void
s390_irgen_VSBCBI(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 == 4);

   IRExpr* result =
      s390_V128_calculate_carry_out_with_carry(get_vr_qw(v2),
                                               unop(Iop_NotV128, get_vr_qw(v3)),
                                               get_vr_qw(v4));

   put_vr_qw(v1, result);
}

static void
s390_irgen_VMAH(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 4);

   put_vr_qw(v1, s390_V128_mula_high(get_vr_qw(v2), get_vr_qw(v3),
                                     get_vr_qw(v4), m5, True));
}

static void
s390_irgen_VMALH(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 4);

   put_vr_qw(v1, s390_V128_mula_high(get_vr_qw(v2), get_vr_qw(v3),
                                     get_vr_qw(v4), m5, False));
}

static void
s390_irgen_VMSL(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   s390_insn_assert(m5 == 3);

   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VMSL;
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
}

static void
s390_vector_fp_convert(IROp op, IRType fromType, IRType toType, Bool rounding,
                       UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m4);

   /* For Iop_F32toF64 we do this:
      f32[0] -> f64[0]
      f32[2] -> f64[1]

      For Iop_F64toF32 we do this:
      f64[0] -> f32[0]
      f64[1] -> f32[2]

      The magic below with scaling factors is used to achieve the logic
      described above.
   */
   Int size_diff = sizeofIRType(toType) - sizeofIRType(fromType);
   const UChar sourceIndexScaleFactor = size_diff > 0 ? 2 : 1;
   const UChar destinationIndexScaleFactor = size_diff < 0 ? 2 : 1;
   UChar n_elem = (isSingleElementOp ? 1 :
                   16 / (size_diff > 0 ?
                         sizeofIRType(toType) : sizeofIRType(fromType)));

   for (UChar i = 0; i < n_elem; i++) {
      IRExpr* argument = get_vr(v2, fromType, i * sourceIndexScaleFactor);
      IRExpr* result;
      if (rounding) {
         result = binop(op,
                        mkexpr(encode_bfp_rounding_mode(m5)),
                        argument);
      } else {
         result = unop(op, argument);
      }
      put_vr(v1, toType, i * destinationIndexScaleFactor, result);
   }
}

static void
s390_irgen_VCDG(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert(m3 == 3 || (m3 == 2 && s390_host_has_vxe2));
   s390_insn_assert((m4 & 0x3) == 0);
   s390_insn_assert(m5 != 2 && m5 <= 7);

   s390_vector_fp_convert(m3 == 2 ? Iop_I32StoF32 : Iop_I64StoF64,
                          m3 == 2 ? Ity_I32       : Ity_I64,
                          m3 == 2 ? Ity_F32       : Ity_F64,
                          True, v1, v2, m3, m4, m5);
}

static void
s390_irgen_VCDLG(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert(m3 == 3 || (m3 == 2 && s390_host_has_vxe2));
   s390_insn_assert((m4 & 0x3) == 0);
   s390_insn_assert(m5 != 2 && m5 <= 7);

   s390_vector_fp_convert(m3 == 2 ? Iop_I32UtoF32 : Iop_I64UtoF64,
                          m3 == 2 ? Ity_I32       : Ity_I64,
                          m3 == 2 ? Ity_F32       : Ity_F64,
                          True, v1, v2, m3, m4, m5);
}

static void
s390_irgen_VCGD(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert(m3 == 3 || (m3 == 2 && s390_host_has_vxe2));
   s390_insn_assert((m4 & 0x3) == 0);
   s390_insn_assert(m5 != 2 && m5 <= 7);

   s390_vector_fp_convert(m3 == 2 ? Iop_F32toI32S : Iop_F64toI64S,
                          m3 == 2 ? Ity_F32       : Ity_F64,
                          m3 == 2 ? Ity_I32       : Ity_I64,
                          True, v1, v2, m3, m4, m5);
}

static void
s390_irgen_VCLGD(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert(m3 == 3 || (m3 == 2 && s390_host_has_vxe2));
   s390_insn_assert((m4 & 0x3) == 0);
   s390_insn_assert(m5 != 2 && m5 <= 7);

   s390_vector_fp_convert(m3 == 2 ? Iop_F32toI32U : Iop_F64toI64U,
                          m3 == 2 ? Ity_F32       : Ity_F64,
                          m3 == 2 ? Ity_I32       : Ity_I64,
                          True, v1, v2, m3, m4, m5);
}

static void
s390_irgen_VFI(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert((m3 == 3 || (s390_host_has_vxe && m3 >= 2 && m3 <= 4)));
   s390_insn_assert((m4 & 0x3) == 0);
   s390_insn_assert(m5 != 2 && m5 <= 7);

   switch (m3) {
   case 2: s390_vector_fp_convert(Iop_RoundF32toInt, Ity_F32, Ity_F32, True,
                                  v1, v2, m3, m4, m5); break;
   case 3: s390_vector_fp_convert(Iop_RoundF64toInt, Ity_F64, Ity_F64, True,
                                  v1, v2, m3, m4, m5); break;
   case 4: s390_vector_fp_convert(Iop_RoundF128toInt, Ity_F128, Ity_F128, True,
                                  v1, v2, m3, m4, m5); break;
   }
}

static void
s390_irgen_VFLL(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390_insn_assert(m3 == 2 || (s390_host_has_vxe && m3 == 3));
   s390_insn_assert((m4 & 0x7) == 0);

   if (m3 == 2)
      s390_vector_fp_convert(Iop_F32toF64, Ity_F32, Ity_F64, False,
                             v1, v2, m3, m4, /* don't care */ 0);
   else
      s390_vector_fp_convert(Iop_F64toF128, Ity_F64, Ity_F128, False,
                             v1, v2, m3, m4, /* don't care */ 0);
}

static void
s390_irgen_VFLR(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert(m3 == 3 || (s390_host_has_vxe && m3 == 4));
   s390_insn_assert((m4 & 0x3) == 0);
   s390_insn_assert(m5 != 2 && m5 <= 7);

   if (m3 == 3)
      s390_vector_fp_convert(Iop_F64toF32, Ity_F64, Ity_F32, True,
                             v1, v2, m3, m4, m5);
   else
      s390_vector_fp_convert(Iop_F128toF64, Ity_F128, Ity_F64, True,
                             v1, v2, m3, m4, m5);
}

static void
s390_irgen_VFPSO(UChar v1, UChar v2, UChar m3, UChar m4, UChar m5)
{
   s390_insn_assert(m3 == 3 || (s390_host_has_vxe && m3 >= 2 && m3 <= 4));
   s390_insn_assert((m4 & 0x7) == 0);
   s390_insn_assert(m5 <= 2);

   Bool single = s390_vr_is_single_element_control_set(m4) || m3 == 4;
   IRType type = single ? s390_vr_get_ftype(m3) : Ity_V128;
   int idx = 2 * (m3 - 2) + (single ? 0 : 1);

   static const IROp negate_ops[] = {
      Iop_NegF32, Iop_Neg32Fx4,
      Iop_NegF64, Iop_Neg64Fx2,
      Iop_NegF128
   };
   static const IROp abs_ops[] = {
      Iop_AbsF32, Iop_Abs32Fx4,
      Iop_AbsF64, Iop_Abs64Fx2,
      Iop_AbsF128
   };

   if (m5 == 1) {
      /* Set sign to negative */
      put_vr(v1, type, 0,
             unop(negate_ops[idx],
                  unop(abs_ops[idx], get_vr(v2, type, 0))));
   } else {
      /* m5 == 0: invert sign; m5 == 2: set sign to positive */
      const IROp *ops = m5 == 2 ? abs_ops : negate_ops;
      put_vr(v1, type, 0, unop(ops[idx], get_vr(v2, type, 0)));
   }
}

static void
s390x_vec_fp_binary_op(const IROp ops[],
                       UChar v1, UChar v2, UChar v3,
                       UChar m4, UChar m5)
{
   s390_insn_assert((m5 & 7) == 0 &&
                    (m4 == 3 || (s390_host_has_vxe && m4 >= 2 && m4 <= 4)));

   int idx = 2 * (m4 - 2);

   if (m4 == 4 || s390_vr_is_single_element_control_set(m5)) {
      IRType type = s390_vr_get_ftype(m4);
      put_vr(v1, type, 0,
             triop(ops[idx], get_bfp_rounding_mode_from_fpc(),
                   get_vr(v2, type, 0), get_vr(v3, type, 0)));
   } else {
      put_vr_qw(v1, triop(ops[idx + 1], get_bfp_rounding_mode_from_fpc(),
                          get_vr_qw(v2), get_vr_qw(v3)));
   }
}

static void
s390x_vec_fp_unary_op(const IROp ops[],
                      UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390_insn_assert((m4 & 7) == 0 &&
                    (m3 == 3 || (s390_host_has_vxe && m3 >= 2 && m3 <= 4)));

   int idx = 2 * (m3 - 2);

   if (m3 == 4 || s390_vr_is_single_element_control_set(m4)) {
      IRType type = s390_vr_get_ftype(m3);
      put_vr(v1, type, 0,
             binop(ops[idx], get_bfp_rounding_mode_from_fpc(),
                   get_vr(v2, type, 0)));
   }
   else {
      put_vr_qw(v1, binop(ops[idx + 1], get_bfp_rounding_mode_from_fpc(),
                          get_vr_qw(v2)));
   }
}


static void
s390_vector_fp_mulAddOrSub(UChar v1, UChar v2, UChar v3, UChar v4,
                           UChar m5, UChar m6,
                           const IROp single_ops[],
                           Bool negate)
{
   s390_insn_assert((m5 & 0x7) == 0);
   s390_insn_assert(m6 == 3 || (s390_host_has_vxe && m6 >= 2 && m6 <= 4));

   static const IROp negate_ops[] = { Iop_NegF32, Iop_NegF64, Iop_NegF128 };
   IRType type = s390_vr_get_ftype(m6);
   Bool single = s390_vr_is_single_element_control_set(m5) || m6 == 4;
   UChar n_elem = single ? 1 : (1 << (4 - m6));
   IRTemp irrm_temp = newTemp(Ity_I32);
   assign(irrm_temp, get_bfp_rounding_mode_from_fpc());
   IRExpr* irrm = mkexpr(irrm_temp);

   for (UChar idx = 0; idx < n_elem; idx++) {
      IRExpr* result = qop(single_ops[m6 - 2],
                           irrm,
                           get_vr(v2, type, idx),
                           get_vr(v3, type, idx),
                           get_vr(v4, type, idx));
      put_vr(v1, type, idx, negate ? unop(negate_ops[m6 - 2], result) : result);
   }
}

static void
s390_irgen_VFA(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   static const IROp vfa_ops[] = {
      Iop_AddF32, Iop_Add32Fx4,
      Iop_AddF64, Iop_Add64Fx2,
      Iop_AddF128,
   };
   s390x_vec_fp_binary_op(vfa_ops, v1, v2, v3, m4, m5);
}

static void
s390_irgen_VFS(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   static const IROp vfs_ops[] = {
      Iop_SubF32, Iop_Sub32Fx4,
      Iop_SubF64, Iop_Sub64Fx2,
      Iop_SubF128,
   };
   s390x_vec_fp_binary_op(vfs_ops, v1, v2, v3, m4, m5);
}

static void
s390_irgen_VFM(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   static const IROp vfm_ops[] = {
      Iop_MulF32, Iop_Mul32Fx4,
      Iop_MulF64, Iop_Mul64Fx2,
      Iop_MulF128,
   };
   s390x_vec_fp_binary_op(vfm_ops, v1, v2, v3, m4, m5);
}

static void
s390_irgen_VFD(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   static const IROp vfd_ops[] = {
      Iop_DivF32, Iop_Div32Fx4,
      Iop_DivF64, Iop_Div64Fx2,
      Iop_DivF128,
   };
   s390x_vec_fp_binary_op(vfd_ops, v1, v2, v3, m4, m5);
}

static void
s390_irgen_VFSQ(UChar v1, UChar v2, UChar m3, UChar m4)
{
   static const IROp vfsq_ops[] = {
      Iop_SqrtF32, Iop_Sqrt32Fx4,
      Iop_SqrtF64, Iop_Sqrt64Fx2,
      Iop_SqrtF128
   };
   s390x_vec_fp_unary_op(vfsq_ops, v1, v2, m3, m4);
}

static const IROp FMA_single_ops[] = {
   Iop_MAddF32, Iop_MAddF64, Iop_MAddF128
};

static void
s390_irgen_VFMA(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   s390_vector_fp_mulAddOrSub(v1, v2, v3, v4, m5, m6,
                              FMA_single_ops, False);
}

static void
s390_irgen_VFNMA(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   s390_vector_fp_mulAddOrSub(v1, v2, v3, v4, m5, m6,
                              FMA_single_ops, True);
}

static const IROp FMS_single_ops[] = {
   Iop_MSubF32, Iop_MSubF64, Iop_MSubF128
};

static void
s390_irgen_VFMS(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   s390_vector_fp_mulAddOrSub(v1, v2, v3, v4, m5, m6,
                              FMS_single_ops, False);
}

static void
s390_irgen_VFNMS(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5, UChar m6)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   s390_vector_fp_mulAddOrSub(v1, v2, v3, v4, m5, m6,
                              FMS_single_ops, True);
}

static void
s390_irgen_WFC(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390_insn_assert(m4 == 0 &&
                    (m3 == 3 || (s390_host_has_vxe && m3 >= 2 && m3 <= 4)));

   static const IROp ops[] = { Iop_CmpF32, Iop_CmpF64, Iop_CmpF128 };
   IRType type = s390_vr_get_ftype(m3);

   IRTemp cc_vex = newTemp(Ity_I32);
   assign(cc_vex, binop(ops[m3 - 2], get_vr(v1, type, 0), get_vr(v2, type, 0)));

   IRTemp cc_s390 = newTemp(Ity_I32);
   assign(cc_s390, convert_vex_bfpcc_to_s390(cc_vex));
   s390_cc_thunk_put1(S390_CC_OP_SET, cc_s390, False);
}

static void
s390_irgen_WFK(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390_insn_assert(m4 == 0 &&
                    (m3 == 3 || (s390_host_has_vxe && m3 >= 2 && m3 <= 4)));

   s390_irgen_WFC(v1, v2, m3, m4);
}

static void
s390_irgen_VFCx(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6,
                IRCmpFResult cmp, Bool equal_ok,
                IROp cmp32, IROp cmp64)
{
   s390_insn_assert((m5 & 3) == 0 && (m6 & 14) == 0 &&
                    (m4 == 3 || (s390_host_has_vxe && m4 >= 2 && m4 <= 4)));

   Bool single = s390_vr_is_single_element_control_set(m5) || m4 == 4;

   if (single) {
      static const IROp ops[] = { Iop_CmpF32, Iop_CmpF64, Iop_CmpF128 };
      IRType type = s390_vr_get_ftype(m4);
      IRTemp result = newTemp(Ity_I32);
      IRTemp cond = newTemp(Ity_I1);

      assign(result, binop(ops[m4 - 2],
                           get_vr(v2, type, 0), get_vr(v3, type, 0)));
      if (equal_ok) {
         assign(cond,
                binop(Iop_Or1,
                      binop(Iop_CmpEQ32, mkexpr(result), mkU32(cmp)),
                      binop(Iop_CmpEQ32, mkexpr(result), mkU32(Ircr_EQ))));
      } else {
         assign(cond, binop(Iop_CmpEQ32, mkexpr(result), mkU32(cmp)));
      }
      put_vr_qw(v1, mkite(mkexpr(cond),
                          mkV128(0xffff),
                          mkV128(0)));
      if (s390_vr_is_cs_set(m6)) {
         IRTemp cc = newTemp(Ity_I64);
         assign(cc, mkite(mkexpr(cond), mkU64(0), mkU64(3)));
         s390_cc_set(cc);
      }
   } else {
      IRTemp result = newTemp(Ity_V128);

      assign(result, binop(m4 == 2 ? cmp32 : cmp64,
                           get_vr_qw(v2), get_vr_qw(v3)));
      put_vr_qw(v1, mkexpr(result));
      if (s390_vr_is_cs_set(m6)) {
         IRTemp cc = newTemp(Ity_I64);
         assign(cc,
                mkite(binop(Iop_CmpEQ64,
                            binop(Iop_And64,
                                  unop(Iop_V128to64, mkexpr(result)),
                                  unop(Iop_V128HIto64, mkexpr(result))),
                            mkU64(-1ULL)),
                      mkU64(0), /* all comparison results are true */
                      mkite(binop(Iop_CmpEQ64,
                                  binop(Iop_Or64,
                                        unop(Iop_V128to64, mkexpr(result)),
                                        unop(Iop_V128HIto64, mkexpr(result))),
                                  mkU64(0)),
                            mkU64(3), /* all false */
                            mkU64(1)))); /* mixed true/false */
         s390_cc_set(cc);
      }
   }
}

static void
s390_irgen_VFCE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6)
{
   s390_irgen_VFCx(v1, v2, v3, m4, m5, m6, Ircr_EQ,
                   False, Iop_CmpEQ32Fx4, Iop_CmpEQ64Fx2);
}

static void
s390_irgen_VFCH(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6)
{
   /* Swap arguments and compare "low" instead. */
   s390_irgen_VFCx(v1, v3, v2, m4, m5, m6, Ircr_LT,
                   False, Iop_CmpLT32Fx4, Iop_CmpLT64Fx2);
}

static void
s390_irgen_VFCHE(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6)
{
   /* Swap arguments and compare "low or equal" instead. */
   s390_irgen_VFCx(v1, v3, v2, m4, m5, m6, Ircr_LT,
                   True, Iop_CmpLE32Fx4, Iop_CmpLE64Fx2);
}

static void
s390_irgen_VFTCI(UChar v1, UChar v2, UShort i3, UChar m4, UChar m5)
{
   s390_insn_assert((m4 == 3 || (s390_host_has_vxe && m4 >= 2 && m4 <= 4)));
   s390_insn_assert((m5 & 0x7) == 0);

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

   const UChar elementSize = isSingleElementOp ?
      sizeofIRType(s390_vr_get_ftype(m4)) : sizeof(V128);
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
}

static void
s390_irgen_VFMIN(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   s390_insn_assert(m4 >= 2 && m4 <= 4);
   s390_insn_assert((m5 & 0x7) == 0);
   s390_insn_assert(m6 <= 4 || (m6 >= 8 && m6 <= 12));

   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m5);
   IRDirty* d;

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VFMIN;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.m4 = m4;
   details.m5 = m5;
   details.m6 = m6;

   d = unsafeIRDirty_0_N(0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   const UChar elementSize = isSingleElementOp ?
      sizeofIRType(s390_vr_get_ftype(m4)) : sizeof(V128);
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
}

static void
s390_irgen_VFMAX(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5, UChar m6)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   s390_insn_assert(m4 >= 2 && m4 <= 4);
   s390_insn_assert((m5 & 0x7) == 0);
   s390_insn_assert(m6 <= 4 || (m6 >= 8 && m6 <= 12));

   Bool isSingleElementOp = s390_vr_is_single_element_control_set(m5);
   IRDirty* d;

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VFMAX;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.m4 = m4;
   details.m5 = m5;
   details.m6 = m6;

   d = unsafeIRDirty_0_N(0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   const UChar elementSize = isSingleElementOp ?
      sizeofIRType(s390_vr_get_ftype(m4)) : sizeof(V128);
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
}

static void
s390_irgen_VBPERM(UChar v1, UChar v2, UChar v3)
{
   if (! s390_host_has_vxe) {
      emulation_failure(EmFail_S390X_vxe);
      return;
   }

   IRDirty* d;
   IRTemp cc = newTemp(Ity_I64);

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VBPERM;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.m4 = 0;
   details.m5 = 0;
   details.m6 = 0;

   d = unsafeIRDirty_1_N(cc, 0, "s390x_dirtyhelper_vec_op",
                         &s390x_dirtyhelper_vec_op,
                         mkIRExprVec_2(IRExpr_GSPTR(),
                                       mkU64(details.serialized)));

   d->nFxState = 3;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size = sizeof(V128);
   d->fxState[1].fx = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size = sizeof(V128);
   d->fxState[2].fx = Ifx_Write;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[2].size = sizeof(V128);

   stmt(IRStmt_Dirty(d));
   s390_cc_set(cc);
}

static void
s390_irgen_SELR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRExpr* cond = binop(Iop_CmpNE32, s390_call_calculate_cond(m4), mkU32(0));
   put_gpr_w1(r1, mkite(cond, get_gpr_w1(r2), get_gpr_w1(r3)));
}

static void
s390_irgen_SELGR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRExpr* cond = binop(Iop_CmpNE32, s390_call_calculate_cond(m4), mkU32(0));
   put_gpr_dw0(r1, mkite(cond, get_gpr_dw0(r2), get_gpr_dw0(r3)));
}

static void
s390_irgen_SELFHR(UChar r3, UChar m4, UChar r1, UChar r2)
{
   IRExpr* cond = binop(Iop_CmpNE32, s390_call_calculate_cond(m4), mkU32(0));
   put_gpr_w0(r1, mkite(cond, get_gpr_w0(r2), get_gpr_w0(r3)));
}

/* Helper function that byte-swaps each element of its V128 input operand */
static IRExpr *
s390_byteswap_elements(IRExpr* v, UChar m)
{
   static const ULong perm[4][2] = {
      { 0x0100030205040706, 0x09080b0a0d0c0f0e }, /* 2-byte elements */
      { 0x0302010007060504, 0x0b0a09080f0e0d0c }, /* 4-byte elements */
      { 0x0706050403020100, 0x0f0e0d0c0b0a0908 }, /* 8-byte elements */
      { 0x0f0e0d0c0b0a0908, 0x0706050403020100 }, /* whole vector */
   };
   return binop(Iop_Perm8x16, v, binop(Iop_64HLtoV128,
                                       mkU64(perm[m - 1][0]),
                                       mkU64(perm[m - 1][1])));
}

/* Helper function that reverses the elements of its V128 input operand */
static IRExpr *
s390_reverse_elements(IRExpr* v, UChar m)
{
   static const ULong perm[3][2] = {
      { 0x0e0f0c0d0a0b0809, 0x0607040502030001 }, /* 2-byte elements */
      { 0x0c0d0e0f08090a0b, 0x0405060700010203 }, /* 4-byte elements */
      { 0x08090a0b0c0d0e0f, 0x0001020304050607 }, /* 8-byte elements */
   };
   return binop(Iop_Perm8x16, v, binop(Iop_64HLtoV128,
                                       mkU64(perm[m - 1][0]),
                                       mkU64(perm[m - 1][1])));
}

static void
s390_irgen_VLBR(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 >= 1 && m3 <= 4);

   put_vr_qw(v1, s390_byteswap_elements(load(Ity_V128, mkexpr(op2addr)), m3));
}

static void
s390_irgen_VSTBR(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 >= 1 && m3 <= 4);

   store(mkexpr(op2addr), s390_byteswap_elements(get_vr_qw(v1), m3));
}

static void
s390_irgen_VLER(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 >= 1 && m3 <= 3);

   put_vr_qw(v1, s390_reverse_elements(load(Ity_V128, mkexpr(op2addr)), m3));
}

static void
s390_irgen_VSTER(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 >= 1 && m3 <= 3);

   store(mkexpr(op2addr), s390_reverse_elements(get_vr_qw(v1), m3));
}

/* Helper function that combines its two V128 operands by replacing element 'to'
   in 'a' by byte-swapped element 'from' in 'b' */
static IRExpr *
s390_insert_byteswapped(IRExpr* a, IRExpr* b, UChar m, UChar to, UChar from)
{
   UInt elem_size = 1U << m;
   UInt start = elem_size * to;
   UInt end = start + elem_size - 1;
   UInt offs = end + elem_size * from + 16;
   UInt i;

   ULong permH = 0;
   for (i = 0; i < 8; i++) {
      permH = (permH << 8) | (i >= start && i <= end ? offs - i : i);
   }
   ULong permL = 0;
   for (i = 8; i < 16; i++) {
      permL = (permL << 8) | (i >= start && i <= end ? offs - i : i);
   }
   return triop(Iop_Perm8x16x2, a, b, binop(Iop_64HLtoV128,
                                            mkU64(permH), mkU64(permL)));
}

static void
s390_irgen_VLEBRH(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 <= 7);

   IRTemp op2 = newTemp(Ity_I16);
   assign(op2, load(Ity_I16, mkexpr(op2addr)));
   put_vr(v1, Ity_I16, m3, binop(Iop_Or16,
                                 binop(Iop_Shl16, mkexpr(op2), mkU8(8)),
                                 binop(Iop_Shr16, mkexpr(op2), mkU8(8))));
}

static void
s390_irgen_VLEBRF(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 <= 3);

   IRTemp op1 = newTemp(Ity_V128);
   assign(op1, get_vr_qw(v1));
   IRTemp op2 = newTemp(Ity_I64);
   assign(op2, unop(Iop_32Uto64, load(Ity_I32, mkexpr(op2addr))));
   IRExpr* b = binop(Iop_64HLtoV128, mkexpr(op2), mkexpr(op2));
   put_vr_qw(v1, s390_insert_byteswapped(mkexpr(op1), b, 2, m3, 3));
}

static void
s390_irgen_VLEBRG(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 <= 1);

   IRTemp op1 = newTemp(Ity_V128);
   assign(op1, get_vr_qw(v1));
   IRTemp op2 = newTemp(Ity_I64);
   assign(op2, load(Ity_I64, mkexpr(op2addr)));
   IRExpr* b = binop(Iop_64HLtoV128, mkexpr(op2), mkexpr(op2));
   put_vr_qw(v1, s390_insert_byteswapped(mkexpr(op1), b, 3, m3, 1));
}

static void
s390_irgen_VLBRREP(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 >= 1 && m3 <= 3);

   static const ULong perm[3] = {
      0x0f0e0f0e0f0e0f0e,       /* 2-byte element */
      0x0f0e0d0c0f0e0d0c,       /* 4-byte element */
      0x0f0e0d0c0b0a0908        /* 8-byte element */
   };
   IRExpr* permHL = mkU64(perm[m3 - 1]);
   IRTemp op2 = newTemp(Ity_I64);
   if (m3 == 3)
      assign(op2, load(Ity_I64, mkexpr(op2addr)));
   else
      assign(op2, unop(m3 == 2 ? Iop_32Uto64 : Iop_16Uto64,
                       load(s390_vr_get_type(m3), mkexpr(op2addr))));
   put_vr_qw(v1, binop(Iop_Perm8x16,
                       binop(Iop_64HLtoV128, mkexpr(op2), mkexpr(op2)),
                       binop(Iop_64HLtoV128, permHL, permHL)));
}

static void
s390_irgen_VLLEBRZ(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert((m3 >= 1 && m3 <= 3) || m3 == 6);

   static const ULong perm[6] = {
      0x0000000000000f0e,       /* 2-byte element */
      0x000000000f0e0d0c,       /* 4-byte element */
      0x0f0e0d0c0b0a0908,       /* 8-byte element */
      0,                        /* invalid (4) */
      0,                        /* invalid (5) */
      0x0f0e0d0c00000000,       /* 4-byte element, left-aligned */
   };
   IRExpr* permH = mkU64(perm[m3 - 1]);
   IRTemp op2 = newTemp(Ity_I64);
   if (m3 == 3)
      assign(op2, load(Ity_I64, mkexpr(op2addr)));
   else
      assign(op2, unop((m3 & 3) == 2 ? Iop_32Uto64 : Iop_16Uto64,
                       load(s390_vr_get_type(m3 & 3), mkexpr(op2addr))));
   put_vr_qw(v1, binop(Iop_Perm8x16,
                       binop(Iop_64HLtoV128, mkU64(0), mkexpr(op2)),
                       binop(Iop_64HLtoV128, permH, mkU64(0))));
}

static void
s390_irgen_VSTEBRH(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 <= 7);

   IRTemp op1 = newTemp(Ity_I16);
   assign(op1, get_vr(v1, Ity_I16, m3));
   store(mkexpr(op2addr), binop(Iop_Or16,
                                binop(Iop_Shl16, mkexpr(op1), mkU8(8)),
                                binop(Iop_Shr16, mkexpr(op1), mkU8(8))));
}

static void
s390_irgen_VSTEBRF(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 <= 3);

   IRTemp op1 = newTemp(Ity_V128);
   assign(op1, get_vr_qw(v1));
   IRExpr* b = s390_insert_byteswapped(mkexpr(op1), mkexpr(op1), 2, 3, m3);
   store(mkexpr(op2addr), unop(Iop_V128to32, b));
}

static void
s390_irgen_VSTEBRG(UChar v1, IRTemp op2addr, UChar m3)
{
   if (! s390_host_has_vxe2) {
      emulation_failure(EmFail_S390X_vxe2);
      return;
   }

   s390_insn_assert(m3 <= 1);

   IRTemp op1 = newTemp(Ity_V128);
   assign(op1, get_vr_qw(v1));
   IRExpr* b = s390_insert_byteswapped(mkexpr(op1), mkexpr(op1), 3, 1, m3);
   store(mkexpr(op2addr), unop(Iop_V128to64, b));
}

static void
s390_irgen_VCxx(s390x_vec_op_details_t details,
                UShort v2_offs, UShort v2_size)
{
   if (! s390_host_has_nnpa) {
      emulation_failure(EmFail_S390X_nnpa);
      return;
   }

   IRDirty* d = unsafeIRDirty_0_N(0, "s390x_dirtyhelper_vec_op",
                                  &s390x_dirtyhelper_vec_op,
                                  mkIRExprVec_2(IRExpr_GSPTR(),
                                                mkU64(details.serialized)));
   d->nFxState = 2;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0)
      + details.v2 * sizeof(V128) + v2_offs;
   d->fxState[0].size = v2_size;
   d->fxState[1].fx = Ifx_Write;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0)
      + details.v1 * sizeof(V128);
   d->fxState[1].size = sizeof(V128);

   stmt(IRStmt_Dirty(d));
}

static void
s390_irgen_VCNF(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VCNF;
   details.v1 = v1;
   details.v2 = v2;
   details.m3 = m3;
   details.m4 = m4;
   s390_irgen_VCxx( details, 0, sizeof(V128));
}

static void
s390_irgen_VCLFNH(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VCLFNH;
   details.v1 = v1;
   details.v2 = v2;
   details.m3 = m3;
   details.m4 = m4;
   s390_irgen_VCxx(details, 0, sizeof(V128) / 2);
}

static void
s390_irgen_VCFN(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VCFN;
   details.v1 = v1;
   details.v2 = v2;
   details.m3 = m3;
   details.m4 = m4;
   s390_irgen_VCxx(details, 0, sizeof(V128));
}

static void
s390_irgen_VCLFNL(UChar v1, UChar v2, UChar m3, UChar m4)
{
   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VCLFNL;
   details.v1 = v1;
   details.v2 = v2;
   details.m3 = m3;
   details.m4 = m4;
   s390_irgen_VCxx(details, sizeof(V128) / 2, sizeof(V128) / 2);
}

static void
s390_irgen_VCRNF(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   if (! s390_host_has_nnpa) {
      emulation_failure(EmFail_S390X_nnpa);
      return;
   }

   s390x_vec_op_details_t details = { .serialized = 0ULL };
   details.op = S390_VEC_OP_VCRNF;
   details.v1 = v1;
   details.v2 = v2;
   details.v3 = v3;
   details.m4 = m4;
   details.m5 = m5;
   details.m6 = 0;
   IRDirty* d = unsafeIRDirty_0_N(0, "s390x_dirtyhelper_vec_op",
                                  &s390x_dirtyhelper_vec_op,
                                  mkIRExprVec_2(IRExpr_GSPTR(),
                                                mkU64(details.serialized)));
   d->nFxState = 3;
   vex_bzero(&d->fxState, sizeof(d->fxState));
   d->fxState[0].fx = Ifx_Read;
   d->fxState[0].offset = S390X_GUEST_OFFSET(guest_v0) + v2 * sizeof(V128);
   d->fxState[0].size = sizeof(V128);
   d->fxState[1].fx = Ifx_Read;
   d->fxState[1].offset = S390X_GUEST_OFFSET(guest_v0) + v3 * sizeof(V128);
   d->fxState[1].size = sizeof(V128);
   d->fxState[2].fx = Ifx_Write;
   d->fxState[2].offset = S390X_GUEST_OFFSET(guest_v0) + v1 * sizeof(V128);
   d->fxState[2].size = sizeof(V128);

   stmt(IRStmt_Dirty(d));
}

static void
s390_irgen_NNPA(void)
{
   if (! s390_host_has_nnpa) {
      emulation_failure(EmFail_S390X_nnpa);
      return;
   }
   extension(S390_EXT_NNPA, 0);
}

static void
s390_irgen_KM(UChar r1, UChar r2)
{
   s390_insn_assert(r1 != 0 && r1 % 2 == 0 && r2 != 0 && r2 % 2 == 0);
   extension(S390_EXT_KM, r1 | (r2 << 4));
}

static void
s390_irgen_KMC(UChar r1, UChar r2)
{
   s390_insn_assert(r1 != 0 && r1 % 2 == 0 && r2 != 0 && r2 % 2 == 0);
   extension(S390_EXT_KMC, r1 | (r2 << 4));
}

static void
s390_irgen_KIMD(UChar r1, UChar r2, UChar m3)
{
   /* r1 is reserved */
   s390_insn_assert(r2 != 0 && r2 % 2 == 0);
   if (!s390_host_has_msa12) {
      m3 = 0;
   }
   extension(S390_EXT_KIMD, r1 | (r2 << 4) | (m3 << 8));
}

static void
s390_irgen_KLMD(UChar r1, UChar r2, UChar m3)
{
   /* r1 is only used by some functions */
   s390_insn_assert(r2 != 0 && r2 % 2 == 0);
   if (!s390_host_has_msa12) {
      m3 = 0;
   }
   extension(S390_EXT_KLMD, r1 | (r2 << 4) | (m3 << 8));
}

static void
s390_irgen_KMAC(UChar r1, UChar r2)
{
   /* r1 is ignored */
   s390_insn_assert(r2 != 0 && r2 % 2 == 0);
   extension(S390_EXT_KMAC, r1 | (r2 << 4));
}

static void
s390_irgen_PCC(void)
{
   extension(S390_EXT_PCC, 0);
}

static void
s390_irgen_KMCTR(UChar r3, UChar r1, UChar r2)
{
   s390_insn_assert(r1 % 2 == 0 && r1 != 0 && r2 % 2 == 0 && r2 != 0 &&
                    r3 % 2 == 0 && r3 != 0);
   extension(S390_EXT_KMCTR, r1 | (r2 << 4) | (r3 << 8));
}

static void
s390_irgen_KMO(UChar r1, UChar r2)
{
   s390_insn_assert(r1 != 0 && r1 % 2 == 0 && r2 != 0 && r2 % 2 == 0);
   extension(S390_EXT_KMO, r1 | (r2 << 4));
}

static void
s390_irgen_KMF(UChar r1, UChar r2)
{
   s390_insn_assert(r1 != 0 && r1 % 2 == 0 && r2 != 0 && r2 % 2 == 0);
   extension(S390_EXT_KMF, r1 | (r2 << 4));
}

static void
s390_irgen_KMA(UChar r3, UChar r1, UChar r2)
{
   if (! s390_host_has_msa8) {
      emulation_failure(EmFail_S390X_msa8);
      return;
   }
   s390_insn_assert(r1 % 2 == 0 && r1 != 0 && r2 % 2 == 0 && r2 != 0 &&
                    r3 % 2 == 0 && r3 != 0 && r3 != r1 && r3 != r2);
   extension(S390_EXT_KMA, r1 | (r2 << 4) | (r3 << 8));
}

static void
s390_irgen_KDSA(UChar r1, UChar r2)
{
   if (! s390_host_has_msa9) {
      emulation_failure(EmFail_S390X_msa9);
      return;
   }
   /* r1 is reserved */
   s390_insn_assert(r2 != 0 && r2 % 2 == 0);
   extension(S390_EXT_KDSA, r1 | (r2 << 4));
}

static void
s390_irgen_BPP(UChar m1, UShort i2, IRTemp op3addr)
{
   /* Treat as a no-op */
}

static void
s390_irgen_BPRP(UChar m1, UShort i2, UInt i3)
{
   /* Treat as a no-op */
}

static void
s390_irgen_NIAI(UChar i1, UChar i2)
{
   /* Treat as a no-op */
}

static void
s390_irgen_PPA(UChar m3, UChar r1, UChar r2)
{
   /* Treat as a no-op.  m3 could indicate one of the following:
       1: transaction-abort assist -- fine, we don't support transactions
      15: in-order-execution assist -- we don't claim support */
}

static void
s390_irgen_CLZG(UChar r1, UChar r2)
{
   put_gpr_dw0(r1, unop(Iop_ClzNat64, get_gpr_dw0(r2)));
}

static void
s390_irgen_CTZG(UChar r1, UChar r2)
{
   IRTemp op2 = newTemp(Ity_I64);

   assign(op2, get_gpr_dw0(r2));
   put_gpr_dw0(
      r1, binop(Iop_Sub64, mkU64(64),
                unop(Iop_ClzNat64,
                     binop(Iop_And64, binop(Iop_Sub64, mkexpr(op2), mkU64(1)),
                           unop(Iop_Not64, mkexpr(op2))))));
}

/* Helper for BEXTG and BDEPG -- they share much of the logic */
static void
s390_irgen_BExtDep(UChar r3, UChar r1, UChar r2, Bool do_ext)
{
   IRTemp  op2 = newTemp(Ity_I64);
   IRTemp  op3 = newTemp(Ity_I64);
   IRTemp  movemask[6];
   IRExpr* mk;
   IRExpr* x;

   assign(op2, get_gpr_dw0(r2));
   assign(op3, get_gpr_dw0(r3));

   x  = mkexpr(op3);
   mk = binop(Iop_Shr64, unop(Iop_Not64, mkexpr(op3)), mkU8(1));
   for (int i = 0; i < 6; i++) {
      IRTemp  mki = newTemp(Ity_I64);
      IRExpr* mpx;
      assign(mki, mk);
      mpx = mkexpr(mki);
      for (int j = 0; j < 6; j++) {
         IRTemp mpj = newTemp(Ity_I64);
         assign(mpj, mpx);
         mpx = binop(Iop_Xor64, mkexpr(mpj),
                     binop(Iop_Shr64, mkexpr(mpj), mkU8(1 << j)));
      }
      IRTemp mp   = newTemp(Ity_I64);
      IRTemp m    = newTemp(Ity_I64);
      movemask[i] = newTemp(Ity_I64);
      assign(mp, mpx);
      assign(m, x);
      assign(movemask[i], binop(Iop_And64, mkexpr(mp), mkexpr(m)));
      if (i != 5) {
         x  = binop(Iop_Or64, binop(Iop_Xor64, mkexpr(m), mkexpr(movemask[i])),
                    binop(Iop_Shl64, mkexpr(movemask[i]), mkU8(1 << i)));
         mk = binop(Iop_And64, mkexpr(mki), unop(Iop_Not64, mkexpr(mp)));
      }
   }

   if (do_ext) {
      /* Extract */
      x = binop(Iop_And64, mkexpr(op2), mkexpr(op3));
      for (int i = 0; i < 6; i++) {
         IRTemp t = newTemp(Ity_I64);
         assign(t, binop(Iop_And64, x, mkexpr(movemask[i])));
         x = binop(Iop_Or64, binop(Iop_Xor64, x, mkexpr(t)),
                   binop(Iop_Shl64, mkexpr(t), mkU8(1 << i)));
      }
   } else {
      /* Deposit */
      x = mkexpr(op2);
      for (int i = 6; i-- > 0;) {
         IRTemp xi = newTemp(Ity_I64);
         assign(xi, x);
         x = binop(
            Iop_Or64,
            binop(Iop_And64, mkexpr(xi), unop(Iop_Not64, mkexpr(movemask[i]))),
            binop(Iop_And64, binop(Iop_Shr64, mkexpr(xi), mkU8(1 << i)),
                  mkexpr(movemask[i])));
      }
      x = binop(Iop_And64, x, mkexpr(op3));
   }
   put_gpr_dw0(r1, x);
}

static void
s390_irgen_BEXTG(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_BExtDep(r3, r1, r2, True);
}

static void
s390_irgen_BDEPG(UChar r3, UChar r1, UChar r2)
{
   s390_irgen_BExtDep(r3, r1, r2, False);
}

/* Implement the various "load (logical) indexed address" variants */
static void
s390_irgen_LXAx(UChar r1, UChar x2, UChar b2, UInt dx, Bool is_signed, UChar es)
{
   IRExpr* res;

   res = mkU32(dx);
   if (x2) {
      res = binop(Iop_Add32, res, get_gpr_w1(x2));
   }
   res = unop(is_signed ? Iop_32Sto64 : Iop_32Uto64, res);
   if (es) {
      res = binop(Iop_Shl64, res, mkU8(es));
   }
   if (b2) {
      res = binop(Iop_Add64, res, get_gpr_dw0(b2));
   }
   put_gpr_dw0(r1, res);
}

static void
s390_irgen_LXAB(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, True, 0);
}

static void
s390_irgen_LXAH(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, True, 1);
}

static void
s390_irgen_LXAF(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, True, 2);
}

static void
s390_irgen_LXAG(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, True, 3);
}

static void
s390_irgen_LXAQ(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, True, 4);
}

static void
s390_irgen_LLXAB(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, False, 0);
}

static void
s390_irgen_LLXAH(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, False, 1);
}

static void
s390_irgen_LLXAF(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, False, 2);
}

static void
s390_irgen_LLXAG(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, False, 3);
}

static void
s390_irgen_LLXAQ(UChar r1, UChar x2, UChar b2, UInt dx)
{
   s390_irgen_LXAx(r1, x2, b2, dx, False, 4);
}

static void
s390_irgen_VBLEND(UChar v1, UChar v2, UChar v3, UChar v4, UChar m5)
{
   s390_insn_assert(m5 <= 4);
   IRExpr* vIfTrue  = get_vr_qw(v2);
   IRExpr* vIfFalse = get_vr_qw(v3);
   IRExpr* vCond    = s390_V128_high_set(get_vr_qw(v4), m5);
   put_vr_qw(v1, s390_V128_bitwiseITE(vCond, vIfTrue, vIfFalse));
}

static void
s390_irgen_VGEM(UChar v1, UChar v2, UChar m3)
{
   static const ULong pos[][2] = {
      {0x0001020304050607, 0x0001020304050607},
      {0x0000000100020003, 0x0004000500060007},
      {0x0000000000000001, 0x0000000200000003},
      {0x0000000000000000, 0x0000000000000001},
   };
   static const IROp shl_op[] = {Iop_Shl8x16, Iop_Shl16x8, Iop_Shl32x4,
                                 Iop_Shl64x2};
   IRTemp            op2      = newTemp(Ity_V128);
   IRExpr*           mask;

   s390_insn_assert(m3 <= 4);
   assign(op2, get_vr_qw(v2));
   if (m3 == 0) {
      /* Separate into high and low half */
      mask =
         binop(Iop_InterleaveHI64x2,
               s390_V128_fill(binop(Iop_GetElem8x16, mkexpr(op2), mkU8(0))),
               s390_V128_fill(binop(Iop_GetElem8x16, mkexpr(op2), mkU8(1))));
   } else if (m3 < 4) {
      IROp getelem = S390_VEC_OP3(m3 - 1, Iop_GetElem8x16, Iop_GetElem16x8,
                                  Iop_GetElem32x4);
      mask         = s390_V128_fill(binop(getelem, mkexpr(op2), mkU8(0)));
   } else {
      mask = mkexpr(op2);
   }
   if (m3 < 4) {
      mask = binop(shl_op[m3], mask,
                   binop(Iop_64HLtoV128, mkU64(pos[m3][0]), mkU64(pos[m3][1])));
   }
   put_vr_qw(v1, s390_V128_high_set(mask, m3));
}

static void
s390_irgen_VEVAL(UChar v1, UChar v2, UChar v3, UChar v4, UChar i5)
{
   IRTemp  a = newTemp(Ity_V128);
   IRTemp  b = newTemp(Ity_V128);
   IRTemp  c = newTemp(Ity_V128);
   IRTemp  not_a = newTemp(Ity_V128);
   IRTemp  not_b = newTemp(Ity_V128);
   IRTemp  not_c = newTemp(Ity_V128);
   IRExpr* t = NULL;

   assign(a, get_vr_qw(v2));
   assign(b, get_vr_qw(v3));
   assign(c, get_vr_qw(v4));
   assign(not_a, unop(Iop_NotV128, mkexpr(a)));
   assign(not_b, unop(Iop_NotV128, mkexpr(b)));
   assign(not_c, unop(Iop_NotV128, mkexpr(c)));

   for (UChar i = 0; i < 8; i++) {
      if ((i5 & (1 << i)) != 0) {
         IRExpr* s = binop(Iop_AndV128, mkexpr(i & 4 ? not_a : a),
                           binop(Iop_AndV128, mkexpr(i & 2 ? not_b : b),
                                 mkexpr(i & 1 ? not_c : c)));
         if (t == NULL) {
            t = s;
         } else {
            t = binop(Iop_OrV128, t, s);
         }
      }
   }
   if (t == NULL)
      t = mkV128(0);
   put_vr_qw(v1, t);
}

/* Return a mask for the lanes where a/b would fail */
static IRExpr*
s390_V128_bad_div_mask(IRTemp a, IRTemp b, UChar m4, Bool is_signed)
{
   IRExpr* res;
   res = s390_V128_CmpEQ(mkexpr(b), mkV128(0), m4);
   if (is_signed) {
      IRExpr* max_neg;
      switch (m4) {
      case 2:
         max_neg = s390_V128_fill(mkU32(0x80000000));
         break;
      case 3:
         max_neg = s390_V128_fill(mkU64(1ULL << 63));
         break;
      case 4:
         max_neg = binop(Iop_64HLtoV128, mkU64(1ULL << 63), mkU64(0));
         break;
      default:
         vpanic("s390_V128_bad_div_mask: bad m4");
      }
      res = binop(Iop_OrV128, res,
                  binop(Iop_AndV128,
                        s390_V128_CmpEQ(mkexpr(b), mkV128(0xffff), m4),
                        s390_V128_CmpEQ(mkexpr(a), max_neg, m4)));
   }
   return res;
}

static void
s390_irgen_VDx(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5,
               Bool is_signed, Bool do_remainder)
{
   s390_insn_assert(m4 >= 2 && m4 <= 4);

   IRTemp  op1 = newTemp(Ity_V128);
   IRTemp  op2 = newTemp(Ity_V128);
   IRTemp  a   = newTemp(Ity_V128);
   IRTemp  b   = newTemp(Ity_V128);
   Bool    idc = (m5 & 8) != 0;
   IRExpr* result;

   assign(op1, get_vr_qw(v2));
   assign(op2, get_vr_qw(v3));
   if (idc) {
      /* Avoid bad divisions; ensure a zero result for affected lanes */
      IRTemp bad = newTemp(Ity_V128);
      assign(bad, s390_V128_bad_div_mask(op1, op2, m4, is_signed));
      assign(a,
             binop(Iop_AndV128, mkexpr(op1), unop(Iop_NotV128, mkexpr(bad))));
      assign(b, binop(Iop_OrV128, mkexpr(op2), mkexpr(bad)));
   } else {
      assign(a, mkexpr(op1));
      assign(b, mkexpr(op2));
   }

   /* There are no Iops for vector divisions, so split up by lane */
   switch (m4) {
   case 2: {
      IRExpr* res[4];
      IROp    divop = is_signed ? Iop_DivModS64to32 : Iop_DivModU64to32;
      IROp    widen = is_signed ? Iop_32Sto64 : Iop_32Uto64;
      IROp    hilo  = do_remainder ? Iop_64HIto32 : Iop_64to32;
      for (UChar i = 0; i < 4; i++) {
         res[i] = unop(
            hilo, binop(divop,
                        unop(widen, binop(Iop_GetElem32x4, mkexpr(a), mkU8(i))),
                        binop(Iop_GetElem32x4, mkexpr(b), mkU8(i))));
      }
      result = binop(Iop_64HLtoV128, binop(Iop_32HLto64, res[0], res[1]),
                     binop(Iop_32HLto64, res[2], res[3]));
      break;
   }
   case 3: {
      IRExpr* res[2];
      IROp    hilo = do_remainder ? Iop_128HIto64 : Iop_128to64;
      if (is_signed) {
         IROp divop = Iop_DivModS64to64;
         for (UChar i = 0; i < 2; i++) {
            res[i] = unop(
               hilo, binop(divop, binop(Iop_GetElem64x2, mkexpr(a), mkU8(i)),
                           binop(Iop_GetElem64x2, mkexpr(b), mkU8(i))));
         }
      } else {
         IROp divop = Iop_DivModU128to64;
         for (UChar i = 0; i < 2; i++) {
            res[i] = unop(
               hilo, binop(divop,
                           binop(Iop_64HLto128, mkU64(0),
                                 binop(Iop_GetElem64x2, mkexpr(a), mkU8(i))),
                           binop(Iop_GetElem64x2, mkexpr(b), mkU8(i))));
         }
      }
      result = binop(Iop_64HLtoV128, res[0], res[1]);
      break;
   }
   case 4: {
      if (!s390_host_has_vxe3) {
         emulation_failure(EmFail_S390X_vxe3);
         return;
      }
      IROp divop = do_remainder ? (is_signed ? Iop_ModS128 : Iop_ModU128)
                                : (is_signed ? Iop_DivS128 : Iop_DivU128);
      result     = binop(divop, mkexpr(a), mkexpr(b));
      break;
   }
   }
   put_vr_qw(v1, result);
}

static void
s390_irgen_VD(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_irgen_VDx(v1, v2, v3, m4, m5, True, False);
}

static void
s390_irgen_VDL(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_irgen_VDx(v1, v2, v3, m4, m5, False, False);
}

static void
s390_irgen_VR(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_irgen_VDx(v1, v2, v3, m4, m5, True, True);
}

static void
s390_irgen_VRL(UChar v1, UChar v2, UChar v3, UChar m4, UChar m5)
{
   s390_irgen_VDx(v1, v2, v3, m4, m5, False, True);
}

/* New insns are added here.
   If an insn is contingent on a facility being installed also
   check whether function do_extension_STFLE needs updating. */

/*------------------------------------------------------------*/
/*--- Build IR for special instructions                    ---*/
/*------------------------------------------------------------*/

static void
s390_irgen_client_request(void)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      vex_printf("special insn: client_request");

   Addr64 next = guest_IA_curr_instr + S390_SPECIAL_OP_PREAMBLE_SIZE
                                     + S390_SPECIAL_OP_SIZE;

   dis_res->jk_StopHere = Ijk_ClientReq;
   dis_res->whatNext = Dis_StopHere;

   put_IA(mkaddr_expr(next));
}

static void
s390_irgen_guest_NRADDR(void)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      vex_printf("special insn: guest_NRADDR");

   put_gpr_dw0(3, IRExpr_Get(S390X_GUEST_OFFSET(guest_NRADDR), Ity_I64));
}

static void
s390_irgen_call_noredir(void)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      vex_printf("special insn: call_noredir");

   Addr64 next = guest_IA_curr_instr + S390_SPECIAL_OP_PREAMBLE_SIZE
                                     + S390_SPECIAL_OP_SIZE;

   /* Continue after special op */
   put_gpr_dw0(14, mkaddr_expr(next));

   /* The address is in REG1, all parameters are in the right (guest) places */
   put_IA(get_gpr_dw0(1));

   dis_res->whatNext = Dis_StopHere;
   dis_res->jk_StopHere = Ijk_NoRedir;
}

static void
s390_irgen_inject_ir(void)
{
   if (UNLIKELY(vex_traceflags & VEX_TRACE_FE))
      vex_printf("special insn: inject_ir");

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
   case 0x010a: s390_irgen_PFPO();
                goto ok;
   case 0x010b: /* TAM */ goto unimplemented;
   case 0x010c: /* SAM24 */ goto unimplemented;
   case 0x010d: /* SAM31 */ goto unimplemented;
   case 0x010e: /* SAM64 */ goto unimplemented;
   case 0x01ff: /* TRAP2 */ goto unimplemented;
   }

   switch ((ovl & 0xff00) >> 8) {
   case 0x04: /* SPM */ goto unimplemented;
   case 0x05: /* BALR */ goto unimplemented;
   case 0x06: s390_irgen_BCTR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x07: s390_irgen_BCR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x0a: s390_irgen_SVC(I_i(ovl));
              goto ok;
   case 0x0b: /* BSM */ goto unimplemented;
   case 0x0c: /* BASSM */ goto unimplemented;
   case 0x0d: s390_irgen_BASR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x0e: s390_irgen_MVCL(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x0f: s390_irgen_CLCL(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x10: s390_irgen_LPR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x11: s390_irgen_LNR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x12: s390_irgen_LTR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x13: s390_irgen_LCR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x14: s390_irgen_NR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x15: s390_irgen_CLR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x16: s390_irgen_OR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x17: s390_irgen_XR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x18: s390_irgen_LR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x19: s390_irgen_CR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x1a: s390_irgen_AR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x1b: s390_irgen_SR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x1c: s390_irgen_MR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x1d: s390_irgen_DR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x1e: s390_irgen_ALR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x1f: s390_irgen_SLR(RR_r1(ovl), RR_r2(ovl));
              goto ok;
   case 0x20: /* LPDR */ goto unimplemented;
   case 0x21: /* LNDR */ goto unimplemented;
   case 0x22: /* LTDR */ goto unimplemented;
   case 0x23: /* LCDR */ goto unimplemented;
   case 0x24: /* HDR */ goto unimplemented;
   case 0x25: /* LDXR */ goto unimplemented;
   case 0x26: /* MXR */ goto unimplemented;
   case 0x27: /* MXDR */ goto unimplemented;
   case 0x28: s390_irgen_LDR(RR_r1(ovl), RR_r2(ovl));
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
   case 0x38: s390_irgen_LER(RR_r1(ovl), RR_r2(ovl));
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
   case 0xa500: s390_irgen_IIHH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa501: s390_irgen_IIHL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa502: s390_irgen_IILH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa503: s390_irgen_IILL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa504: s390_irgen_NIHH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa505: s390_irgen_NIHL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa506: s390_irgen_NILH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa507: s390_irgen_NILL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa508: s390_irgen_OIHH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa509: s390_irgen_OIHL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa50a: s390_irgen_OILH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa50b: s390_irgen_OILL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa50c: s390_irgen_LLIHH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa50d: s390_irgen_LLIHL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa50e: s390_irgen_LLILH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa50f: s390_irgen_LLILL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa700: s390_irgen_TMLH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa701: s390_irgen_TMLL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa702: s390_irgen_TMHH(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa703: s390_irgen_TMHL(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa704: s390_irgen_BRC(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa705: s390_irgen_BRAS(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa706: s390_irgen_BRCT(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa707: s390_irgen_BRCTG(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa708: s390_irgen_LHI(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa709: s390_irgen_LGHI(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa70a: s390_irgen_AHI(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa70b: s390_irgen_AGHI(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa70c: s390_irgen_MHI(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa70d: s390_irgen_MGHI(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa70e: s390_irgen_CHI(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   case 0xa70f: s390_irgen_CGHI(RI_r1(ovl), RI_i2(ovl));
                goto ok;
   }

   switch ((ovl & 0xffff0000) >> 16) {
   case 0x8000: /* SSM */ goto unimplemented;
   case 0x8200: /* LPSW */ goto unimplemented;
   case 0x9300: /* TS */ goto unimplemented;
   case 0xb200: /* LBEAR */ goto unimplemented;
   case 0xb201: /* STBEAR */ goto unimplemented;
   case 0xb202: /* STIDP */ goto unimplemented;
   case 0xb204: /* SCK */ goto unimplemented;
   case 0xb205: s390_format_S(s390_irgen_STCK, ovl);
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
   case 0xb222: s390_irgen_IPM(RRE_r1(ovl));
                goto ok;
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
   case 0xb241: s390_irgen_CKSM(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb244: /* SQDR */ goto unimplemented;
   case 0xb245: /* SQER */ goto unimplemented;
   case 0xb246: /* STURA */ goto unimplemented;
   case 0xb247: /* MSTA */ goto unimplemented;
   case 0xb248: /* PALB */ goto unimplemented;
   case 0xb249: /* EREG */ goto unimplemented;
   case 0xb24a: /* ESTA */ goto unimplemented;
   case 0xb24b: /* LURA */ goto unimplemented;
   case 0xb24c: /* TAR */ goto unimplemented;
   case 0xb24d: s390_irgen_CPYA(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb24e: s390_irgen_SAR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb24f: s390_irgen_EAR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb250: /* CSP */ goto unimplemented;
   case 0xb252: s390_irgen_MSR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb254: /* MVPG */ goto unimplemented;
   case 0xb255: s390_irgen_MVST(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb257: /* CUSE */ goto unimplemented;
   case 0xb258: /* BSG */ goto unimplemented;
   case 0xb25a: /* BSA */ goto unimplemented;
   case 0xb25d: s390_irgen_CLST(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb25e: s390_irgen_SRST(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb263: /* CMPSC */ goto unimplemented;
   case 0xb274: /* SIGA */ goto unimplemented;
   case 0xb276: /* XSCH */ goto unimplemented;
   case 0xb277: /* RP */ goto unimplemented;
   case 0xb278: s390_format_S(s390_irgen_STCKE, ovl);
                goto ok;
   case 0xb279: /* SACF */ goto unimplemented;
   case 0xb27c: s390_format_S(s390_irgen_STCKF, ovl);
                goto ok;
   case 0xb27d: /* STSI */ goto unimplemented;
   case 0xb280: /* LPP */ goto unimplemented;
   case 0xb284: /* LCCTL */ goto unimplemented;
   case 0xb285: /* LPCTL */ goto unimplemented;
   case 0xb286: /* QSI */ goto unimplemented;
   case 0xb287: /* LSCTL */ goto unimplemented;
   case 0xb28e: /* QCTRI */ goto unimplemented;
   case 0xb28f: /* QPACI */ goto unimplemented;
   case 0xb299: s390_format_S(s390_irgen_SRNM, ovl);
                goto ok;
   case 0xb29c: s390_format_S(s390_irgen_STFPC, ovl);
                goto ok;
   case 0xb29d: s390_format_S(s390_irgen_LFPC, ovl);
                goto ok;
   case 0xb2a5: s390_irgen_TRE(RRE_r1(ovl), RRE_r2(ovl));
                                goto ok;
   case 0xb2a6: s390_irgen_CU21(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb2a7: s390_irgen_CU12(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb2b0: s390_irgen_STFLE(S_b2(ovl), S_d2(ovl));
                goto ok;
   case 0xb2b1: /* STFL */ goto unimplemented;
   case 0xb2b2: /* LPSWE */ goto unimplemented;
   case 0xb2b8: s390_irgen_SRNMB(S_b2(ovl), S_d2(ovl));
                goto ok;
   case 0xb2b9: s390_format_S(s390_irgen_SRNMT, ovl);
                goto ok;
   case 0xb2bd: /* LFAS */ goto unimplemented;
   case 0xb2e0: /* SCCTR */ goto unimplemented;
   case 0xb2e1: /* SPCTR */ goto unimplemented;
   case 0xb2e4: /* ECCTR */ goto unimplemented;
   case 0xb2e5: /* EPCTR */ goto unimplemented;
   case 0xb2e8: s390_irgen_PPA(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb2ec: /* ETND */ goto unimplemented;
   case 0xb2ed: /* ECPGA */ goto unimplemented;
   case 0xb2f8: /* TEND */ goto unimplemented;
   case 0xb2fa: s390_irgen_NIAI(IE_i1(ovl), IE_i2(ovl));
                goto ok;
   case 0xb2fc: /* TABORT */ goto unimplemented;
   case 0xb2ff: /* TRAP4 */ goto unimplemented;
   case 0xb300: s390_irgen_LPEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb301: s390_irgen_LNEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb302: s390_irgen_LTEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb303: s390_irgen_LCEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb304: s390_irgen_LDEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb305: s390_irgen_LXDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb306: s390_irgen_LXEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb307: /* MXDBR */ goto unimplemented;
   case 0xb308: s390_irgen_KEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb309: s390_irgen_CEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb30a: s390_irgen_AEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb30b: s390_irgen_SEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb30c: /* MDEBR */ goto unimplemented;
   case 0xb30d: s390_irgen_DEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb30e: s390_irgen_MAEBR(RRD_r1(ovl), RRD_r3(ovl), RRD_r2(ovl));
                goto ok;
   case 0xb30f: s390_irgen_MSEBR(RRD_r1(ovl), RRD_r3(ovl), RRD_r2(ovl));
                goto ok;
   case 0xb310: s390_irgen_LPDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb311: s390_irgen_LNDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb312: s390_irgen_LTDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb313: s390_irgen_LCDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb314: s390_irgen_SQEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb315: s390_irgen_SQDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb316: s390_irgen_SQXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb317: s390_irgen_MEEBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb318: s390_irgen_KDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb319: s390_irgen_CDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb31a: s390_irgen_ADBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb31b: s390_irgen_SDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb31c: s390_irgen_MDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb31d: s390_irgen_DDBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb31e: s390_irgen_MADBR(RRD_r1(ovl), RRD_r3(ovl), RRD_r2(ovl));
                goto ok;
   case 0xb31f: s390_irgen_MSDBR(RRD_r1(ovl), RRD_r3(ovl), RRD_r2(ovl));
                goto ok;
   case 0xb324: s390_irgen_LDER(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
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
   case 0xb340: s390_irgen_LPXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb341: s390_irgen_LNXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb342: s390_irgen_LTXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb343: s390_irgen_LCXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb344: s390_irgen_LEDBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb345: s390_irgen_LDXBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb346: s390_irgen_LEXBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb347: s390_irgen_FIXBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb348: s390_irgen_KXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb349: s390_irgen_CXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb34a: s390_irgen_AXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb34b: s390_irgen_SXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb34c: s390_irgen_MXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb34d: s390_irgen_DXBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb350: /* TBEDR */ goto unimplemented;
   case 0xb351: /* TBDR */ goto unimplemented;
   case 0xb353: /* DIEBR */ goto unimplemented;
   case 0xb357: s390_irgen_FIEBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb358: /* THDER */ goto unimplemented;
   case 0xb359: /* THDR */ goto unimplemented;
   case 0xb35b: /* DIDBR */ goto unimplemented;
   case 0xb35f: s390_irgen_FIDBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb360: /* LPXR */ goto unimplemented;
   case 0xb361: /* LNXR */ goto unimplemented;
   case 0xb362: /* LTXR */ goto unimplemented;
   case 0xb363: /* LCXR */ goto unimplemented;
   case 0xb365: s390_irgen_LXR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb366: /* LEXR */ goto unimplemented;
   case 0xb367: /* FIXR */ goto unimplemented;
   case 0xb369: /* CXR */ goto unimplemented;
   case 0xb370: s390_irgen_LPDFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb371: s390_irgen_LNDFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb372: s390_irgen_CPSDR(RRFb_r3(ovl), RRFb_r1(ovl), RRFb_r2(ovl));
                goto ok;
   case 0xb373: s390_irgen_LCDFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb374: s390_irgen_LZER(RRE_r1(ovl));
                goto ok;
   case 0xb375: s390_irgen_LZDR(RRE_r1(ovl));
                goto ok;
   case 0xb376: s390_irgen_LZXR(RRE_r1(ovl));
                goto ok;
   case 0xb377: /* FIER */ goto unimplemented;
   case 0xb37f: /* FIDR */ goto unimplemented;
   case 0xb384: s390_irgen_SFPC(RRE_r1(ovl));
                goto ok;
   case 0xb385: /* SFASR */ goto unimplemented;
   case 0xb38c: s390_irgen_EFPC(RRE_r1(ovl));
                goto ok;
   case 0xb390: s390_irgen_CELFBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb391: s390_irgen_CDLFBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb392: s390_irgen_CXLFBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb394: s390_irgen_CEFBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb395: s390_irgen_CDFBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb396: s390_irgen_CXFBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb398: s390_irgen_CFEBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb399: s390_irgen_CFDBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb39a: s390_irgen_CFXBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb39c: s390_irgen_CLFEBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb39d: s390_irgen_CLFDBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb39e: s390_irgen_CLFXBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3a0: s390_irgen_CELGBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3a1: s390_irgen_CDLGBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3a2: s390_irgen_CXLGBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3a4: s390_irgen_CEGBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3a5: s390_irgen_CDGBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3a6: s390_irgen_CXGBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3a8: s390_irgen_CGEBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3a9: s390_irgen_CGDBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3aa: s390_irgen_CGXBRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3ac: s390_irgen_CLGEBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3ad: s390_irgen_CLGDBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3ae: s390_irgen_CLGXBR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3b4: /* CEFR */ goto unimplemented;
   case 0xb3b5: /* CDFR */ goto unimplemented;
   case 0xb3b6: /* CXFR */ goto unimplemented;
   case 0xb3b8: /* CFER */ goto unimplemented;
   case 0xb3b9: /* CFDR */ goto unimplemented;
   case 0xb3ba: /* CFXR */ goto unimplemented;
   case 0xb3c1: s390_irgen_LDGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3c4: /* CEGR */ goto unimplemented;
   case 0xb3c5: /* CDGR */ goto unimplemented;
   case 0xb3c6: /* CXGR */ goto unimplemented;
   case 0xb3c8: /* CGER */ goto unimplemented;
   case 0xb3c9: /* CGDR */ goto unimplemented;
   case 0xb3ca: /* CGXR */ goto unimplemented;
   case 0xb3cd: s390_irgen_LGDR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3d0: s390_irgen_MDTRA(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb3d1: s390_irgen_DDTRA(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb3d2: s390_irgen_ADTRA(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb3d3: s390_irgen_SDTRA(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb3d4: s390_irgen_LDETR(RRFd_m4(ovl), RRFd_r1(ovl), RRFd_r2(ovl));
                goto ok;
   case 0xb3d5: s390_irgen_LEDTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                 RRFe_r2(ovl));
                goto ok;
   case 0xb3d6: s390_irgen_LTDTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3d7: /* FIDTR */ goto unimplemented;
   case 0xb3d8: s390_irgen_MXTRA(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb3d9: s390_irgen_DXTRA(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb3da: s390_irgen_AXTRA(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb3db: s390_irgen_SXTRA(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb3dc: s390_irgen_LXDTR(RRFd_m4(ovl), RRFd_r1(ovl), RRFd_r2(ovl));
                goto ok;
   case 0xb3dd: s390_irgen_LDXTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                 RRFe_r2(ovl));
                goto ok;
   case 0xb3de: s390_irgen_LTXTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3df: /* FIXTR */ goto unimplemented;
   case 0xb3e0: /* KDTR */ goto unimplemented;
   case 0xb3e1: s390_irgen_CGDTRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3e2: /* CUDTR */ goto unimplemented;
   case 0xb3e3: /* CSDTR */ goto unimplemented;
   case 0xb3e4: s390_irgen_CDTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3e5: s390_irgen_EEDTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3e7: s390_irgen_ESDTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3e8: /* KXTR */ goto unimplemented;
   case 0xb3e9: s390_irgen_CGXTRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3ea: /* CUXTR */ goto unimplemented;
   case 0xb3eb: /* CSXTR */ goto unimplemented;
   case 0xb3ec: s390_irgen_CXTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3ed: s390_irgen_EEXTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3ef: s390_irgen_ESXTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3f1: s390_irgen_CDGTRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3f2: /* CDUTR */ goto unimplemented;
   case 0xb3f3: /* CDSTR */ goto unimplemented;
   case 0xb3f4: s390_irgen_CEDTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3f5: s390_irgen_QADTR(RRFb_r3(ovl), RRFb_m4(ovl), RRFb_r1(ovl),
                                 RRFb_r2(ovl));
                goto ok;
   case 0xb3f6: s390_irgen_IEDTR(RRFb_r3(ovl), RRFb_r1(ovl), RRFb_r2(ovl));
                goto ok;
   case 0xb3f7: s390_irgen_RRDTR(RRFb_r3(ovl), RRFb_m4(ovl), RRFb_r1(ovl),
                                 RRFb_r2(ovl));
                goto ok;
   case 0xb3f9: s390_irgen_CXGTRA(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb3fa: /* CXUTR */ goto unimplemented;
   case 0xb3fb: /* CXSTR */ goto unimplemented;
   case 0xb3fc: s390_irgen_CEXTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb3fd: s390_irgen_QAXTR(RRFb_r3(ovl), RRFb_m4(ovl), RRFb_r1(ovl),
                                 RRFb_r2(ovl));
                goto ok;
   case 0xb3fe: s390_irgen_IEXTR(RRFb_r3(ovl), RRFb_r1(ovl), RRFb_r2(ovl));
                goto ok;
   case 0xb3ff: s390_irgen_RRXTR(RRFb_r3(ovl), RRFb_m4(ovl), RRFb_r1(ovl),
                                 RRFb_r2(ovl));
                goto ok;
   case 0xb900: s390_irgen_LPGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb901: s390_irgen_LNGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb902: s390_irgen_LTGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb903: s390_irgen_LCGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb904: s390_irgen_LGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb905: /* LURAG */ goto unimplemented;
   case 0xb906: s390_irgen_LGBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb907: s390_irgen_LGHR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb908: s390_irgen_AGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb909: s390_irgen_SGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb90a: s390_irgen_ALGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb90b: s390_irgen_SLGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb90c: s390_irgen_MSGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb90d: s390_irgen_DSGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb90e: /* EREGG */ goto unimplemented;
   case 0xb90f: s390_irgen_LRVGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb910: s390_irgen_LPGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb911: s390_irgen_LNGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb912: s390_irgen_LTGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb913: s390_irgen_LCGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb914: s390_irgen_LGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb916: s390_irgen_LLGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb917: s390_irgen_LLGTR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb918: s390_irgen_AGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb919: s390_irgen_SGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb91a: s390_irgen_ALGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb91b: s390_irgen_SLGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb91c: s390_irgen_MSGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb91d: s390_irgen_DSGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb91e: s390_irgen_KMAC(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb91f: s390_irgen_LRVR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb920: s390_irgen_CGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb921: s390_irgen_CLGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb925: /* STURG */ goto unimplemented;
   case 0xb926: s390_irgen_LBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb927: s390_irgen_LHR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb928: /* PCKMO */ goto unimplemented;
   case 0xb929: s390_irgen_KMA(RRFb_r3(ovl), RRFb_r1(ovl), RRFb_r2(ovl));
                goto ok;
   case 0xb92a: s390_irgen_KMF(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb92b: s390_irgen_KMO(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb92c: s390_irgen_PCC();
                goto ok;
   case 0xb92d: s390_irgen_KMCTR(RRFb_r3(ovl), RRFb_r1(ovl), RRFb_r2(ovl));
                goto ok;
   case 0xb92e: s390_irgen_KM(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb92f: s390_irgen_KMC(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb930: s390_irgen_CGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb931: s390_irgen_CLGFR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb938: /* SORTL */ goto unimplemented;
   case 0xb939: s390_irgen_DFLTCC(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb93a: s390_irgen_KDSA(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb93b: s390_irgen_NNPA();
                goto ok;
   case 0xb93c: s390_irgen_PRNO(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb93e: s390_irgen_KIMD(RRFc_r1(ovl), RRFc_r2(ovl), RRFc_m3(ovl));
                goto ok;
   case 0xb93f: s390_irgen_KLMD(RRFc_r1(ovl), RRFc_r2(ovl), RRFc_m3(ovl));
                goto ok;
   case 0xb941: s390_irgen_CFDTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                 RRFe_r2(ovl));
                goto ok;
   case 0xb942: s390_irgen_CLGDTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb943: s390_irgen_CLFDTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb946: s390_irgen_BCTGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb949: s390_irgen_CFXTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                 RRFe_r2(ovl));
                goto ok;
   case 0xb94a: s390_irgen_CLGXTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb94b: s390_irgen_CLFXTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb951: s390_irgen_CDFTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                 RRFe_r2(ovl));
                goto ok;
   case 0xb952: s390_irgen_CDLGTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb953: s390_irgen_CDLFTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb959: s390_irgen_CXFTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                 RRFe_r2(ovl));
                goto ok;
   case 0xb95a: s390_irgen_CXLGTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb95b: s390_irgen_CXLFTR(RRFe_m3(ovl), RRFe_m4(ovl), RRFe_r1(ovl),
                                  RRFe_r2(ovl));
                goto ok;
   case 0xb960: s390_irgen_CGRT(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb961: s390_irgen_CLGRT(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb964: s390_irgen_NNGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb965: s390_irgen_OCGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb966: s390_irgen_NOGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb967: s390_irgen_NXGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb968: s390_irgen_CLZG(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb969: s390_irgen_CTZG(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb96c: s390_irgen_BEXTG(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb96d: s390_irgen_BDEPG(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb972: s390_irgen_CRT(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb973: s390_irgen_CLRT(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb974: s390_irgen_NNRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb975: s390_irgen_OCRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb976: s390_irgen_NORK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb977: s390_irgen_NXRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb980: s390_irgen_NGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb981: s390_irgen_OGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb982: s390_irgen_XGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb983: s390_irgen_FLOGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb984: s390_irgen_LLGCR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb985: s390_irgen_LLGHR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb986: s390_irgen_MLGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb987: s390_irgen_DLGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb988: s390_irgen_ALCGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb989: s390_irgen_SLBGR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb98a: /* CSPG */ goto unimplemented;
   case 0xb98b: /* RDP */ goto unimplemented;
   case 0xb98d: /* EPSW */ goto unimplemented;
   case 0xb98e: /* IDTE */ goto unimplemented;
   case 0xb98f: /* CRDTE */ goto unimplemented;
   case 0xb990: s390_irgen_TRTT(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb991: s390_irgen_TRTO(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb992: s390_irgen_TROT(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb993: s390_irgen_TROO(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb994: s390_irgen_LLCR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb995: s390_irgen_LLHR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb996: s390_irgen_MLR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb997: s390_irgen_DLR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb998: s390_irgen_ALCR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb999: s390_irgen_SLBR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
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
   case 0xb9b0: s390_irgen_CU14(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb9b1: s390_irgen_CU24(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb9b2: s390_irgen_CU41(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb9b3: s390_irgen_CU42(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb9bd: /* TRTRE */ goto unimplemented;
   case 0xb9be: /* SRSTU */ goto unimplemented;
   case 0xb9bf: /* TRTE */ goto unimplemented;
   case 0xb9c0: s390_irgen_SELFHR(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                  RRFa_r2(ovl));
                goto ok;
   case 0xb9c8: s390_irgen_AHHHR(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9c9: s390_irgen_SHHHR(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9ca: s390_irgen_ALHHHR(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9cb: s390_irgen_SLHHHR(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9cd: s390_irgen_CHHR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb9cf: s390_irgen_CLHHR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb9d8: s390_irgen_AHHLR(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9d9: s390_irgen_SHHLR(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9da: s390_irgen_ALHHLR(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9db: s390_irgen_SLHHLR(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9dd: s390_irgen_CHLR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb9df: s390_irgen_CLHLR(RRE_r1(ovl), RRE_r2(ovl));
                goto ok;
   case 0xb9e0: s390_irgen_LOCFHR(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb9e1: s390_irgen_POPCNT(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb9e2: s390_irgen_LOCGR(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb9e3: s390_irgen_SELGR(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                 RRFa_r2(ovl));
                goto ok;
   case 0xb9e4: s390_irgen_NGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9e5: s390_irgen_NCGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9e6: s390_irgen_OGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9e7: s390_irgen_XGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9e8: s390_irgen_AGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9e9: s390_irgen_SGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9ea: s390_irgen_ALGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9eb: s390_irgen_SLGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9ec: s390_irgen_MGRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9ed: s390_irgen_MSGRKC(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9f0: s390_irgen_SELR(RRFa_r3(ovl), RRFa_m4(ovl), RRFa_r1(ovl),
                                RRFa_r2(ovl));
                goto ok;
   case 0xb9f2: s390_irgen_LOCR(RRFc_m3(ovl), RRFc_r1(ovl), RRFc_r2(ovl));
                goto ok;
   case 0xb9f4: s390_irgen_NRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9f5: s390_irgen_NCRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9f6: s390_irgen_ORK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9f7: s390_irgen_XRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9f8: s390_irgen_ARK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9f9: s390_irgen_SRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9fa: s390_irgen_ALRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9fb: s390_irgen_SLRK(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   case 0xb9fd: s390_irgen_MSRKC(RRFa_r3(ovl), RRFa_r1(ovl), RRFa_r2(ovl));
                goto ok;
   }

   switch ((ovl & 0xff000000) >> 24) {
   case 0x40: s390_format_RX(s390_irgen_STH, ovl);
              goto ok;
   case 0x41: s390_format_RX(s390_irgen_LA, ovl);
              goto ok;
   case 0x42: s390_format_RX(s390_irgen_STC, ovl);
              goto ok;
   case 0x43: s390_format_RX(s390_irgen_IC, ovl);
              goto ok;
   case 0x44: s390_format_RX(s390_irgen_EX, ovl);
              goto ok;
   case 0x45: /* BAL */ goto unimplemented;
   case 0x46: s390_format_RX(s390_irgen_BCT, ovl);
              goto ok;
   case 0x47: s390_format_RX(s390_irgen_BC, ovl);
              goto ok;
   case 0x48: s390_format_RX(s390_irgen_LH, ovl);
              goto ok;
   case 0x49: s390_format_RX(s390_irgen_CH, ovl);
              goto ok;
   case 0x4a: s390_format_RX(s390_irgen_AH, ovl);
              goto ok;
   case 0x4b: s390_format_RX(s390_irgen_SH, ovl);
              goto ok;
   case 0x4c: s390_format_RX(s390_irgen_MH, ovl);
              goto ok;
   case 0x4d: s390_format_RX(s390_irgen_BAS, ovl);
              goto ok;
   case 0x4e: s390_format_RX(s390_irgen_CVD, ovl);
              goto ok;
   case 0x4f: s390_format_RX(s390_irgen_CVB, ovl);
              goto ok;
   case 0x50: s390_format_RX(s390_irgen_ST, ovl);
              goto ok;
   case 0x51: s390_format_RX(s390_irgen_LAE, ovl);
              goto ok;
   case 0x54: s390_format_RX(s390_irgen_N, ovl);
              goto ok;
   case 0x55: s390_format_RX(s390_irgen_CL, ovl);
              goto ok;
   case 0x56: s390_format_RX(s390_irgen_O, ovl);
              goto ok;
   case 0x57: s390_format_RX(s390_irgen_X, ovl);
              goto ok;
   case 0x58: s390_format_RX(s390_irgen_L, ovl);
              goto ok;
   case 0x59: s390_format_RX(s390_irgen_C, ovl);
              goto ok;
   case 0x5a: s390_format_RX(s390_irgen_A, ovl);
              goto ok;
   case 0x5b: s390_format_RX(s390_irgen_S, ovl);
              goto ok;
   case 0x5c: s390_format_RX(s390_irgen_M, ovl);
              goto ok;
   case 0x5d: s390_format_RX(s390_irgen_D, ovl);
              goto ok;
   case 0x5e: s390_format_RX(s390_irgen_AL, ovl);
              goto ok;
   case 0x5f: s390_format_RX(s390_irgen_SL, ovl);
              goto ok;
   case 0x60: s390_format_RX(s390_irgen_STD, ovl);
              goto ok;
   case 0x67: /* MXD */ goto unimplemented;
   case 0x68: s390_format_RX(s390_irgen_LD, ovl);
              goto ok;
   case 0x69: /* CD */ goto unimplemented;
   case 0x6a: /* AD */ goto unimplemented;
   case 0x6b: /* SD */ goto unimplemented;
   case 0x6c: /* MD */ goto unimplemented;
   case 0x6d: /* DD */ goto unimplemented;
   case 0x6e: /* AW */ goto unimplemented;
   case 0x6f: /* SW */ goto unimplemented;
   case 0x70: s390_format_RX(s390_irgen_STE, ovl);
              goto ok;
   case 0x71: s390_format_RX(s390_irgen_MS, ovl);
              goto ok;
   case 0x78: s390_format_RX(s390_irgen_LE, ovl);
              goto ok;
   case 0x79: /* CE */ goto unimplemented;
   case 0x7a: /* AE */ goto unimplemented;
   case 0x7b: /* SE */ goto unimplemented;
   case 0x7c: /* MDE */ goto unimplemented;
   case 0x7d: /* DE */ goto unimplemented;
   case 0x7e: /* AU */ goto unimplemented;
   case 0x7f: /* SU */ goto unimplemented;
   case 0x83: /* DIAG */ goto unimplemented;
   case 0x84: s390_irgen_BRXH(RSI_r1(ovl), RSI_r3(ovl), RSI_i2(ovl));
              goto ok;
   case 0x85: s390_irgen_BRXLE(RSI_r1(ovl), RSI_r3(ovl), RSI_i2(ovl));
              goto ok;
   case 0x86: s390_format_RS(s390_irgen_BXH, ovl);
              goto ok;
   case 0x87: s390_format_RS(s390_irgen_BXLE, ovl);
              goto ok;
   case 0x88: s390_format_RS0(s390_irgen_SRL, ovl);
              goto ok;
   case 0x89: s390_format_RS0(s390_irgen_SLL, ovl);
              goto ok;
   case 0x8a: s390_format_RS0(s390_irgen_SRA, ovl);
              goto ok;
   case 0x8b: s390_format_RS0(s390_irgen_SLA, ovl);
              goto ok;
   case 0x8c: s390_format_RS0(s390_irgen_SRDL, ovl);
              goto ok;
   case 0x8d: s390_format_RS0(s390_irgen_SLDL, ovl);
              goto ok;
   case 0x8e: s390_format_RS0(s390_irgen_SRDA, ovl);
              goto ok;
   case 0x8f: s390_format_RS0(s390_irgen_SLDA, ovl);
              goto ok;
   case 0x90: s390_format_RS(s390_irgen_STM, ovl);
              goto ok;
   case 0x91: s390_format_SI(s390_irgen_TM, ovl);
              goto ok;
   case 0x92: s390_format_SI(s390_irgen_MVI, ovl);
              goto ok;
   case 0x94: s390_format_SI(s390_irgen_NI, ovl);
              goto ok;
   case 0x95: s390_format_SI(s390_irgen_CLI, ovl);
              goto ok;
   case 0x96: s390_format_SI(s390_irgen_OI, ovl);
              goto ok;
   case 0x97: s390_format_SI(s390_irgen_XI, ovl);
              goto ok;
   case 0x98: s390_format_RS(s390_irgen_LM, ovl);
              goto ok;
   case 0x99: /* TRACE */ goto unimplemented;
   case 0x9a: s390_format_RS(s390_irgen_LAM, ovl);
              goto ok;
   case 0x9b: s390_format_RS(s390_irgen_STAM, ovl);
              goto ok;
   case 0xa8: s390_format_RS(s390_irgen_MVCLE, ovl);
              goto ok;
   case 0xa9: s390_format_RS(s390_irgen_CLCLE, ovl);
              goto ok;
   case 0xac: /* STNSM */ goto unimplemented;
   case 0xad: /* STOSM */ goto unimplemented;
   case 0xae: /* SIGP */ goto unimplemented;
   case 0xaf: /* MC */ goto unimplemented;
   case 0xb1: /* LRA */ goto unimplemented;
   case 0xb6: /* STCTL */ goto unimplemented;
   case 0xb7: /* LCTL */ goto unimplemented;
   case 0xba: s390_format_RS(s390_irgen_CS, ovl);
              goto ok;
   case 0xbb: s390_format_RS(s390_irgen_CDS, ovl);
              goto ok;
   case 0xbd: s390_format_RS(s390_irgen_CLM, ovl);
              goto ok;
   case 0xbe: s390_format_RS(s390_irgen_STCM, ovl);
              goto ok;
   case 0xbf: s390_format_RS(s390_irgen_ICM, ovl);
              goto ok;
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
   case 0xe30000000002ULL: s390_format_RXY(s390_irgen_LTG, ovl);
                           goto ok;
   case 0xe30000000003ULL: /* LRAG */ goto unimplemented;
   case 0xe30000000004ULL: s390_format_RXY(s390_irgen_LG, ovl);
                           goto ok;
   case 0xe30000000006ULL: s390_format_RXY(s390_irgen_CVBY, ovl);
                           goto ok;
   case 0xe30000000008ULL: s390_format_RXY(s390_irgen_AG, ovl);
                           goto ok;
   case 0xe30000000009ULL: s390_format_RXY(s390_irgen_SG, ovl);
                           goto ok;
   case 0xe3000000000aULL: s390_format_RXY(s390_irgen_ALG, ovl);
                           goto ok;
   case 0xe3000000000bULL: s390_format_RXY(s390_irgen_SLG, ovl);
                           goto ok;
   case 0xe3000000000cULL: s390_format_RXY(s390_irgen_MSG, ovl);
                           goto ok;
   case 0xe3000000000dULL: s390_format_RXY(s390_irgen_DSG, ovl);
                           goto ok;
   case 0xe3000000000eULL: /* CVBG */ goto unimplemented;
   case 0xe3000000000fULL: s390_format_RXY(s390_irgen_LRVG, ovl);
                           goto ok;
   case 0xe30000000012ULL: s390_format_RXY(s390_irgen_LT, ovl);
                           goto ok;
   case 0xe30000000013ULL: /* LRAY */ goto unimplemented;
   case 0xe30000000014ULL: s390_format_RXY(s390_irgen_LGF, ovl);
                           goto ok;
   case 0xe30000000015ULL: s390_format_RXY(s390_irgen_LGH, ovl);
                           goto ok;
   case 0xe30000000016ULL: s390_format_RXY(s390_irgen_LLGF, ovl);
                           goto ok;
   case 0xe30000000017ULL: s390_format_RXY(s390_irgen_LLGT, ovl);
                           goto ok;
   case 0xe30000000018ULL: s390_format_RXY(s390_irgen_AGF, ovl);
                           goto ok;
   case 0xe30000000019ULL: s390_format_RXY(s390_irgen_SGF, ovl);
                           goto ok;
   case 0xe3000000001aULL: s390_format_RXY(s390_irgen_ALGF, ovl);
                           goto ok;
   case 0xe3000000001bULL: s390_format_RXY(s390_irgen_SLGF, ovl);
                           goto ok;
   case 0xe3000000001cULL: s390_format_RXY(s390_irgen_MSGF, ovl);
                           goto ok;
   case 0xe3000000001dULL: s390_format_RXY(s390_irgen_DSGF, ovl);
                           goto ok;
   case 0xe3000000001eULL: s390_format_RXY(s390_irgen_LRV, ovl);
                           goto ok;
   case 0xe3000000001fULL: s390_format_RXY(s390_irgen_LRVH, ovl);
                           goto ok;
   case 0xe30000000020ULL: s390_format_RXY(s390_irgen_CG, ovl);
                           goto ok;
   case 0xe30000000021ULL: s390_format_RXY(s390_irgen_CLG, ovl);
                           goto ok;
   case 0xe30000000024ULL: s390_format_RXY(s390_irgen_STG, ovl);
                           goto ok;
   case 0xe30000000025ULL: /* NTSTG */ goto unimplemented;
   case 0xe30000000026ULL: s390_format_RXY(s390_irgen_CVDY, ovl);
                           goto ok;
   case 0xe3000000002aULL: s390_format_RXY(s390_irgen_LZRG, ovl);
                           goto ok;
   case 0xe3000000002eULL: /* CVDG */ goto unimplemented;
   case 0xe3000000002fULL: s390_format_RXY(s390_irgen_STRVG, ovl);
                           goto ok;
   case 0xe30000000030ULL: s390_format_RXY(s390_irgen_CGF, ovl);
                           goto ok;
   case 0xe30000000031ULL: s390_format_RXY(s390_irgen_CLGF, ovl);
                           goto ok;
   case 0xe30000000032ULL: s390_format_RXY(s390_irgen_LTGF, ovl);
                           goto ok;
   case 0xe30000000034ULL: s390_format_RXY(s390_irgen_CGH, ovl);
                           goto ok;
   case 0xe30000000036ULL: s390_irgen_PFD();
                           goto ok;
   case 0xe30000000038ULL: s390_format_RXY(s390_irgen_AGH, ovl);
                           goto ok;
   case 0xe30000000039ULL: s390_format_RXY(s390_irgen_SGH, ovl);
                           goto ok;
   case 0xe3000000003aULL: s390_format_RXY(s390_irgen_LLZRGF, ovl);
                           goto ok;
   case 0xe3000000003bULL: s390_format_RXY(s390_irgen_LZRF, ovl);
                           goto ok;
   case 0xe3000000003cULL: s390_format_RXY(s390_irgen_MGH, ovl);
                           goto ok;
   case 0xe3000000003eULL: s390_format_RXY(s390_irgen_STRV, ovl);
                           goto ok;
   case 0xe3000000003fULL: s390_format_RXY(s390_irgen_STRVH, ovl);
                           goto ok;
   case 0xe30000000046ULL: s390_format_RXY(s390_irgen_BCTG, ovl);
                           goto ok;
   case 0xe30000000047ULL: s390_format_RXY(s390_irgen_BIC, ovl);
                           goto ok;
   case 0xe30000000048ULL: /* LLGFSG */ goto unimplemented;
   case 0xe30000000049ULL: /* STGSC */ goto unimplemented;
   case 0xe3000000004cULL: /* LGG */ goto unimplemented;
   case 0xe3000000004dULL: /* LGSC */ goto unimplemented;
   case 0xe30000000050ULL: s390_format_RXY(s390_irgen_STY, ovl);
                           goto ok;
   case 0xe30000000051ULL: s390_format_RXY(s390_irgen_MSY, ovl);
                           goto ok;
   case 0xe30000000053ULL: s390_format_RXY(s390_irgen_MSC, ovl);
                           goto ok;
   case 0xe30000000054ULL: s390_format_RXY(s390_irgen_NY, ovl);
                           goto ok;
   case 0xe30000000055ULL: s390_format_RXY(s390_irgen_CLY, ovl);
                           goto ok;
   case 0xe30000000056ULL: s390_format_RXY(s390_irgen_OY, ovl);
                           goto ok;
   case 0xe30000000057ULL: s390_format_RXY(s390_irgen_XY, ovl);
                           goto ok;
   case 0xe30000000058ULL: s390_format_RXY(s390_irgen_LY, ovl);
                           goto ok;
   case 0xe30000000059ULL: s390_format_RXY(s390_irgen_CY, ovl);
                           goto ok;
   case 0xe3000000005aULL: s390_format_RXY(s390_irgen_AY, ovl);
                           goto ok;
   case 0xe3000000005bULL: s390_format_RXY(s390_irgen_SY, ovl);
                           goto ok;
   case 0xe3000000005cULL: s390_format_RXY(s390_irgen_MFY, ovl);
                           goto ok;
   case 0xe3000000005eULL: s390_format_RXY(s390_irgen_ALY, ovl);
                           goto ok;
   case 0xe3000000005fULL: s390_format_RXY(s390_irgen_SLY, ovl);
                           goto ok;
   case 0xe30000000060ULL: s390_format_RXYc(s390_irgen_LXAB, ovl);
                           goto ok;
   case 0xe30000000061ULL: s390_format_RXYc(s390_irgen_LLXAB, ovl);
                           goto ok;
   case 0xe30000000062ULL: s390_format_RXYc(s390_irgen_LXAH, ovl);
                           goto ok;
   case 0xe30000000063ULL: s390_format_RXYc(s390_irgen_LLXAH, ovl);
                           goto ok;
   case 0xe30000000064ULL: s390_format_RXYc(s390_irgen_LXAF, ovl);
                           goto ok;
   case 0xe30000000065ULL: s390_format_RXYc(s390_irgen_LLXAF, ovl);
                           goto ok;
   case 0xe30000000066ULL: s390_format_RXYc(s390_irgen_LXAG, ovl);
                           goto ok;
   case 0xe30000000067ULL: s390_format_RXYc(s390_irgen_LLXAG, ovl);
                           goto ok;
   case 0xe30000000068ULL: s390_format_RXYc(s390_irgen_LXAQ, ovl);
                           goto ok;
   case 0xe30000000069ULL: s390_format_RXYc(s390_irgen_LLXAQ, ovl);
                           goto ok;
   case 0xe30000000070ULL: s390_format_RXY(s390_irgen_STHY, ovl);
                           goto ok;
   case 0xe30000000071ULL: s390_format_RXY(s390_irgen_LAY, ovl);
                           goto ok;
   case 0xe30000000072ULL: s390_format_RXY(s390_irgen_STCY, ovl);
                           goto ok;
   case 0xe30000000073ULL: s390_format_RXY(s390_irgen_ICY, ovl);
                           goto ok;
   case 0xe30000000075ULL: s390_format_RXY(s390_irgen_LAEY, ovl);
                           goto ok;
   case 0xe30000000076ULL: s390_format_RXY(s390_irgen_LB, ovl);
                           goto ok;
   case 0xe30000000077ULL: s390_format_RXY(s390_irgen_LGB, ovl);
                           goto ok;
   case 0xe30000000078ULL: s390_format_RXY(s390_irgen_LHY, ovl);
                           goto ok;
   case 0xe30000000079ULL: s390_format_RXY(s390_irgen_CHY, ovl);
                           goto ok;
   case 0xe3000000007aULL: s390_format_RXY(s390_irgen_AHY, ovl);
                           goto ok;
   case 0xe3000000007bULL: s390_format_RXY(s390_irgen_SHY, ovl);
                           goto ok;
   case 0xe3000000007cULL: s390_format_RXY(s390_irgen_MHY, ovl);
                           goto ok;
   case 0xe30000000080ULL: s390_format_RXY(s390_irgen_NG, ovl);
                           goto ok;
   case 0xe30000000081ULL: s390_format_RXY(s390_irgen_OG, ovl);
                           goto ok;
   case 0xe30000000082ULL: s390_format_RXY(s390_irgen_XG, ovl);
                           goto ok;
   case 0xe30000000083ULL: s390_format_RXY(s390_irgen_MSGC, ovl);
                           goto ok;
   case 0xe30000000084ULL: s390_format_RXY(s390_irgen_MG, ovl);
                           goto ok;
   case 0xe30000000085ULL: s390_format_RXY(s390_irgen_LGAT, ovl);
                           goto ok;

   case 0xe30000000086ULL: s390_format_RXY(s390_irgen_MLG, ovl);
                           goto ok;
   case 0xe30000000087ULL: s390_format_RXY(s390_irgen_DLG, ovl);
                           goto ok;
   case 0xe30000000088ULL: s390_format_RXY(s390_irgen_ALCG, ovl);
                           goto ok;
   case 0xe30000000089ULL: s390_format_RXY(s390_irgen_SLBG, ovl);
                           goto ok;
   case 0xe3000000008eULL: s390_format_RXY(s390_irgen_STPQ, ovl);
                           goto ok;
   case 0xe3000000008fULL: s390_format_RXY(s390_irgen_LPQ, ovl);
                           goto ok;
   case 0xe30000000090ULL: s390_format_RXY(s390_irgen_LLGC, ovl);
                           goto ok;
   case 0xe30000000091ULL: s390_format_RXY(s390_irgen_LLGH, ovl);
                           goto ok;
   case 0xe30000000094ULL: s390_format_RXY(s390_irgen_LLC, ovl);
                           goto ok;
   case 0xe30000000095ULL: s390_format_RXY(s390_irgen_LLH, ovl);
                           goto ok;
   case 0xe30000000096ULL: s390_format_RXY(s390_irgen_ML, ovl);
                           goto ok;
   case 0xe30000000097ULL: s390_format_RXY(s390_irgen_DL, ovl);
                           goto ok;
   case 0xe30000000098ULL: s390_format_RXY(s390_irgen_ALC, ovl);
                           goto ok;
   case 0xe30000000099ULL: s390_format_RXY(s390_irgen_SLB, ovl);
                           goto ok;
   case 0xe3000000009cULL: s390_format_RXY(s390_irgen_LLGTAT, ovl);
                           goto ok;
   case 0xe3000000009dULL: s390_format_RXY(s390_irgen_LLGFAT, ovl);
                           goto ok;
   case 0xe3000000009fULL: s390_format_RXY(s390_irgen_LAT, ovl);
                           goto ok;
   case 0xe300000000c0ULL: s390_format_RXY(s390_irgen_LBH, ovl);
                           goto ok;
   case 0xe300000000c2ULL: s390_format_RXY(s390_irgen_LLCH, ovl);
                           goto ok;
   case 0xe300000000c3ULL: s390_format_RXY(s390_irgen_STCH, ovl);
                           goto ok;
   case 0xe300000000c4ULL: s390_format_RXY(s390_irgen_LHH, ovl);
                           goto ok;
   case 0xe300000000c6ULL: s390_format_RXY(s390_irgen_LLHH, ovl);
                           goto ok;
   case 0xe300000000c7ULL: s390_format_RXY(s390_irgen_STHH, ovl);
                           goto ok;
   case 0xe300000000c8ULL: s390_format_RXY(s390_irgen_LFHAT, ovl);
                           goto ok;
   case 0xe300000000caULL: s390_format_RXY(s390_irgen_LFH, ovl);
                           goto ok;
   case 0xe300000000cbULL: s390_format_RXY(s390_irgen_STFH, ovl);
                           goto ok;
   case 0xe300000000cdULL: s390_format_RXY(s390_irgen_CHF, ovl);
                           goto ok;
   case 0xe300000000cfULL: s390_format_RXY(s390_irgen_CLHF, ovl);
                           goto ok;
   case 0xe60000000001ULL: s390_format_VRX(s390_irgen_VLEBRH, ovl);
                           goto ok;
   case 0xe60000000002ULL: s390_format_VRX(s390_irgen_VLEBRG, ovl);
                           goto ok;
   case 0xe60000000003ULL: s390_format_VRX(s390_irgen_VLEBRF, ovl);
                           goto ok;
   case 0xe60000000004ULL: s390_format_VRX(s390_irgen_VLLEBRZ, ovl);
                           goto ok;
   case 0xe60000000005ULL: s390_format_VRX(s390_irgen_VLBRREP, ovl);
                           goto ok;
   case 0xe60000000006ULL: s390_format_VRX(s390_irgen_VLBR, ovl);
                           goto ok;
   case 0xe60000000007ULL: s390_format_VRX(s390_irgen_VLER, ovl);
                           goto ok;
   case 0xe60000000009ULL: s390_format_VRX(s390_irgen_VSTEBRH, ovl);
                           goto ok;
   case 0xe6000000000aULL: s390_format_VRX(s390_irgen_VSTEBRG, ovl);
                           goto ok;
   case 0xe6000000000bULL: s390_format_VRX(s390_irgen_VSTEBRF, ovl);
                           goto ok;
   case 0xe6000000000eULL: s390_format_VRX(s390_irgen_VSTBR, ovl);
                           goto ok;
   case 0xe6000000000fULL: s390_format_VRX(s390_irgen_VSTER, ovl);
                           goto ok;
   case 0xe60000000034ULL: /* VPKZ */ goto unimplemented;
   case 0xe60000000035ULL: s390_format_VSI(s390_irgen_VLRL, ovl);
                           goto ok;
   case 0xe60000000037ULL: s390_format_VRSd(s390_irgen_VLRLR, ovl);
                           goto ok;
   case 0xe6000000003cULL: /* VUPKZ */ goto unimplemented;
   case 0xe6000000003dULL: s390_format_VSI(s390_irgen_VSTRL, ovl);
                           goto ok;
   case 0xe6000000003fULL: s390_format_VRSd(s390_irgen_VSTRLR, ovl);
                           goto ok;
   case 0xe60000000049ULL: /* VLIP */ goto unimplemented;
   case 0xe6000000004aULL: /* VCVDQ */ goto unimplemented;
   case 0xe6000000004eULL: /* VCVBQ */ goto unimplemented;
   case 0xe60000000050ULL: /* VCVB */ goto unimplemented;
   case 0xe60000000051ULL: /* VCLZDP */ goto unimplemented;
   case 0xe60000000052ULL: /* VCVBG */ goto unimplemented;
   case 0xe60000000054ULL: /* VUPKZH */ goto unimplemented;
   case 0xe60000000055ULL: s390_format_VRRa2(s390_irgen_VCNF, ovl);
                           goto ok;
   case 0xe60000000056ULL: s390_format_VRRa2(s390_irgen_VCLFNH, ovl);
                           goto ok;
   case 0xe6000000005dULL: s390_format_VRRa2(s390_irgen_VCFN, ovl);
                           goto ok;
   case 0xe6000000005eULL: s390_format_VRRa2(s390_irgen_VCLFNL, ovl);
                           goto ok;
   case 0xe60000000058ULL: /* VCVD */ goto unimplemented;
   case 0xe60000000059ULL: /* VSRP */ goto unimplemented;
   case 0xe6000000005aULL: /* VCVDG */ goto unimplemented;
   case 0xe6000000005bULL: /* VPSOP */ goto unimplemented;
   case 0xe6000000005cULL: /* VUPKZL */ goto unimplemented;
   case 0xe6000000005fULL: /* VTP */ goto unimplemented;
   case 0xe60000000070ULL: /* VPKZR */ goto unimplemented;
   case 0xe60000000071ULL: /* VAP */ goto unimplemented;
   case 0xe60000000072ULL: /* VSRPR */ goto unimplemented;
   case 0xe60000000073ULL: /* VSP */ goto unimplemented;
   case 0xe60000000075ULL: s390_format_VRRc2(s390_irgen_VCRNF, ovl);
                           goto ok;
   case 0xe60000000077ULL: /* VCP */ goto unimplemented;
   case 0xe60000000078ULL: /* VMP */ goto unimplemented;
   case 0xe60000000079ULL: /* VMSP */ goto unimplemented;
   case 0xe60000000074ULL: /* VSCHP */ goto unimplemented;
   case 0xe6000000007aULL: /* VDP */ goto unimplemented;
   case 0xe6000000007bULL: /* VRP */ goto unimplemented;
   case 0xe6000000007cULL: /* VSCSHP */ goto unimplemented;
   case 0xe6000000007dULL: /* VCSPH */ goto unimplemented;
   case 0xe6000000007eULL: /* VSDP */ goto unimplemented;
   case 0xe6000000007fULL: /* VTZ */ goto unimplemented;
   case 0xe70000000000ULL: s390_format_VRX(s390_irgen_VLEB, ovl);
                           goto ok;
   case 0xe70000000001ULL: s390_format_VRX(s390_irgen_VLEH, ovl);
                           goto ok;
   case 0xe70000000002ULL: s390_format_VRX(s390_irgen_VLEG, ovl);
                           goto ok;
   case 0xe70000000003ULL: s390_format_VRX(s390_irgen_VLEF, ovl);
                           goto ok;
   case 0xe70000000004ULL: s390_format_VRX(s390_irgen_VLLEZ, ovl);
                           goto ok;
   case 0xe70000000005ULL: s390_format_VRX(s390_irgen_VLREP, ovl);
                           goto ok;
   case 0xe70000000006ULL: s390_format_VRX(s390_irgen_VL, ovl);
                           goto ok;
   case 0xe70000000007ULL: s390_format_VRX(s390_irgen_VLBB, ovl);
                           goto ok;
   case 0xe70000000008ULL: s390_format_VRX(s390_irgen_VSTEB, ovl);
                           goto ok;
   case 0xe70000000009ULL: s390_format_VRX(s390_irgen_VSTEH, ovl);
                           goto ok;
   case 0xe7000000000aULL: s390_format_VRX(s390_irgen_VSTEG, ovl);
                           goto ok;
   case 0xe7000000000bULL: s390_format_VRX(s390_irgen_VSTEF, ovl);
                           goto ok;
   case 0xe7000000000eULL: s390_format_VRX(s390_irgen_VST, ovl);
                           goto ok;
   case 0xe70000000012ULL: s390_format_VRV(s390_irgen_VGEG, ovl, Ity_I64);
                           goto ok;
   case 0xe70000000013ULL: s390_format_VRV(s390_irgen_VGEF, ovl, Ity_I32);
                           goto ok;
   case 0xe7000000001aULL: s390_format_VRV(s390_irgen_VSCEG, ovl, Ity_I64);
                           goto ok;
   case 0xe7000000001bULL: s390_format_VRV(s390_irgen_VSCEF, ovl, Ity_I32);
                           goto ok;
   case 0xe70000000021ULL: s390_format_VRSc(s390_irgen_VLGV, ovl);
                           goto ok;
   case 0xe70000000022ULL: s390_format_VRSbm(s390_irgen_VLVG, ovl);
                           goto ok;
   case 0xe70000000027ULL: s390_format_RXE(s390_irgen_LCBB, ovl);
                           goto ok;
   case 0xe70000000030ULL: s390_format_VRSa(s390_irgen_VESL, ovl);
                           goto ok;
   case 0xe70000000033ULL: s390_format_VRSa(s390_irgen_VERLL, ovl);
                           goto ok;
   case 0xe70000000036ULL: s390_format_VRSa(s390_irgen_VLM, ovl);
                           goto ok;
   case 0xe70000000037ULL: s390_format_VRSb(s390_irgen_VLL, ovl);
                           goto ok;
   case 0xe70000000038ULL: s390_format_VRSa(s390_irgen_VESRL, ovl);
                           goto ok;
   case 0xe7000000003aULL: s390_format_VRSa(s390_irgen_VESRA, ovl);
                           goto ok;
   case 0xe7000000003eULL: s390_format_VRSa(s390_irgen_VSTM, ovl);
                           goto ok;
   case 0xe7000000003fULL: s390_format_VRSb(s390_irgen_VSTL, ovl);
                           goto ok;
   case 0xe70000000040ULL: s390_format_VRIa(s390_irgen_VLEIB, ovl);
                           goto ok;
   case 0xe70000000041ULL: s390_format_VRIa(s390_irgen_VLEIH, ovl);
                           goto ok;
   case 0xe70000000042ULL: s390_format_VRIa(s390_irgen_VLEIG, ovl);
                           goto ok;
   case 0xe70000000043ULL: s390_format_VRIa(s390_irgen_VLEIF, ovl);
                           goto ok;
   case 0xe70000000044ULL: s390_format_VRIa0(s390_irgen_VGBM, ovl);
                           goto ok;
   case 0xe70000000045ULL: s390_format_VRIa(s390_irgen_VREPI, ovl);
                           goto ok;
   case 0xe70000000046ULL: s390_format_VRIb(s390_irgen_VGM, ovl);
                           goto ok;
   case 0xe7000000004aULL: s390_format_VRIe(s390_irgen_VFTCI, ovl);
                           goto ok;
   case 0xe7000000004dULL: s390_format_VRIc(s390_irgen_VREP, ovl);
                           goto ok;
   case 0xe70000000050ULL: s390_format_VRRa1(s390_irgen_VPOPCT, ovl);
                           goto ok;
   case 0xe70000000052ULL: s390_format_VRRa1(s390_irgen_VCTZ, ovl);
                           goto ok;
   case 0xe70000000053ULL: s390_format_VRRa1(s390_irgen_VCLZ, ovl);
                           goto ok;
   case 0xe70000000054ULL: s390_format_VRRa1(s390_irgen_VGEM, ovl);
                           goto ok;
   case 0xe70000000056ULL: s390_format_VRRa0(s390_irgen_VLR, ovl);
                           goto ok;
   case 0xe7000000005cULL: s390_format_VRRa(s390_irgen_VISTR, ovl);
                           goto ok;
   case 0xe7000000005fULL: s390_format_VRRa1(s390_irgen_VSEG, ovl);
                           goto ok;
   case 0xe70000000060ULL: s390_format_VRRc1(s390_irgen_VMRL, ovl);
                           goto ok;
   case 0xe70000000061ULL: s390_format_VRRc1(s390_irgen_VMRH, ovl);
                           goto ok;
   case 0xe70000000062ULL: s390_format_VRRf(s390_irgen_VLVGP, ovl);
                           goto ok;
   case 0xe70000000064ULL: s390_format_VRRc1(s390_irgen_VSUM, ovl);
                           goto ok;
   case 0xe70000000065ULL: s390_format_VRRc1(s390_irgen_VSUMG, ovl);
                           goto ok;
   case 0xe70000000066ULL: s390_format_VRRc0(s390_irgen_VCKSM, ovl);
                           goto ok;
   case 0xe70000000067ULL: s390_format_VRRc1(s390_irgen_VSUMQ, ovl);
                           goto ok;
   case 0xe70000000068ULL: s390_format_VRRc0(s390_irgen_VN, ovl);
                           goto ok;
   case 0xe70000000069ULL: s390_format_VRRc0(s390_irgen_VNC, ovl);
                           goto ok;
   case 0xe7000000006aULL: s390_format_VRRc0(s390_irgen_VO, ovl);
                           goto ok;
   case 0xe7000000006bULL: s390_format_VRRc0(s390_irgen_VNO, ovl);
                           goto ok;
   case 0xe7000000006cULL: s390_format_VRRc0(s390_irgen_VNX, ovl);
                           goto ok;
   case 0xe7000000006dULL: s390_format_VRRc0(s390_irgen_VX, ovl);
                           goto ok;
   case 0xe7000000006eULL: s390_format_VRRc0(s390_irgen_VNN, ovl);
                           goto ok;
   case 0xe7000000006fULL: s390_format_VRRc0(s390_irgen_VOC, ovl);
                           goto ok;
   case 0xe70000000070ULL: s390_format_VRRc1(s390_irgen_VESLV, ovl);
                           goto ok;
   case 0xe70000000072ULL: s390_format_VRIdm(s390_irgen_VERIM, ovl);
                           goto ok;
   case 0xe70000000073ULL: s390_format_VRRc1(s390_irgen_VERLLV, ovl);
                           goto ok;
   case 0xe70000000074ULL: s390_format_VRRc0(s390_irgen_VSL, ovl);
                           goto ok;
   case 0xe70000000075ULL: s390_format_VRRc0(s390_irgen_VSLB, ovl);
                           goto ok;
   case 0xe70000000077ULL: s390_format_VRId(s390_irgen_VSLDB, ovl);
                           goto ok;
   case 0xe70000000078ULL: s390_format_VRRc1(s390_irgen_VESRLV, ovl);
                           goto ok;
   case 0xe7000000007aULL: s390_format_VRRc1(s390_irgen_VESRAV, ovl);
                           goto ok;
   case 0xe7000000007cULL: s390_format_VRRc0(s390_irgen_VSRL, ovl);
                           goto ok;
   case 0xe7000000007dULL: s390_format_VRRc0(s390_irgen_VSRLB, ovl);
                           goto ok;
   case 0xe7000000007eULL: s390_format_VRRc0(s390_irgen_VSRA, ovl);
                           goto ok;
   case 0xe7000000007fULL: s390_format_VRRc0(s390_irgen_VSRAB, ovl);
                           goto ok;
   case 0xe70000000080ULL: s390_format_VRRb(s390_irgen_VFEE, ovl);
                           goto ok;
   case 0xe70000000081ULL: s390_format_VRRb(s390_irgen_VFENE, ovl);
                           goto ok;
   case 0xe70000000082ULL: s390_format_VRRb(s390_irgen_VFAE, ovl);
                           goto ok;
   case 0xe70000000084ULL: s390_format_VRRc1(s390_irgen_VPDI, ovl);
                           goto ok;
   case 0xe70000000085ULL: s390_format_VRRc0(s390_irgen_VBPERM, ovl);
                           goto ok;
   case 0xe70000000086ULL: s390_format_VRId(s390_irgen_VSLD, ovl);
                           goto ok;
   case 0xe70000000087ULL: s390_format_VRId(s390_irgen_VSRD, ovl);
                           goto ok;
   case 0xe70000000088ULL: s390_format_VRIk(s390_irgen_VEVAL, ovl);
                           goto ok;
   case 0xe70000000089ULL: s390_format_VRRd1(s390_irgen_VBLEND, ovl);
                           goto ok;
   case 0xe7000000008aULL: s390_format_VRRd(s390_irgen_VSTRC, ovl);
                           goto ok;
   case 0xe7000000008bULL: s390_format_VRRd(s390_irgen_VSTRS, ovl);
                           goto ok;
   case 0xe7000000008cULL: s390_format_VRRe0(s390_irgen_VPERM, ovl);
                           goto ok;
   case 0xe7000000008dULL: s390_format_VRRe0(s390_irgen_VSEL, ovl);
                           goto ok;
   case 0xe7000000008eULL: s390_format_VRRe(s390_irgen_VFMS, ovl);
                           goto ok;
   case 0xe7000000008fULL: s390_format_VRRe(s390_irgen_VFMA, ovl);
                           goto ok;
   case 0xe70000000094ULL: s390_format_VRRc1(s390_irgen_VPK, ovl);
                           goto ok;
   case 0xe70000000095ULL: s390_format_VRRb(s390_irgen_VPKLS, ovl);
                           goto ok;
   case 0xe70000000097ULL: s390_format_VRRb(s390_irgen_VPKS, ovl);
                           goto ok;
   case 0xe7000000009eULL: s390_format_VRRe(s390_irgen_VFNMS, ovl);
                           goto ok;
   case 0xe7000000009fULL: s390_format_VRRe(s390_irgen_VFNMA, ovl);
                           goto ok;
   case 0xe700000000a1ULL: s390_format_VRRc1(s390_irgen_VMLH, ovl);
                           goto ok;
   case 0xe700000000a2ULL: s390_format_VRRc1(s390_irgen_VML, ovl);
                           goto ok;
   case 0xe700000000a3ULL: s390_format_VRRc1(s390_irgen_VMH, ovl);
                           goto ok;
   case 0xe700000000a4ULL: s390_format_VRRc1(s390_irgen_VMLE, ovl);
                           goto ok;
   case 0xe700000000a5ULL: s390_format_VRRc1(s390_irgen_VMLO, ovl);
                           goto ok;
   case 0xe700000000a6ULL: s390_format_VRRc1(s390_irgen_VME, ovl);
                           goto ok;
   case 0xe700000000a7ULL: s390_format_VRRc1(s390_irgen_VMO, ovl);
                           goto ok;
   case 0xe700000000a9ULL: s390_format_VRRd1(s390_irgen_VMALH, ovl);
                           goto ok;
   case 0xe700000000aaULL: s390_format_VRRd1(s390_irgen_VMAL, ovl);
                           goto ok;
   case 0xe700000000abULL: s390_format_VRRd1(s390_irgen_VMAH, ovl);
                           goto ok;
   case 0xe700000000acULL: s390_format_VRRd1(s390_irgen_VMALE, ovl);
                           goto ok;
   case 0xe700000000adULL: s390_format_VRRd1(s390_irgen_VMALO, ovl);
                           goto ok;
   case 0xe700000000aeULL: s390_format_VRRd1(s390_irgen_VMAE, ovl);
                           goto ok;
   case 0xe700000000afULL: s390_format_VRRd1(s390_irgen_VMAO, ovl);
                           goto ok;
   case 0xe700000000b0ULL: s390_format_VRRc2(s390_irgen_VDL, ovl);
                           goto ok;
   case 0xe700000000b1ULL: s390_format_VRRc2(s390_irgen_VRL, ovl);
                           goto ok;
   case 0xe700000000b2ULL: s390_format_VRRc2(s390_irgen_VD, ovl);
                           goto ok;
   case 0xe700000000b3ULL: s390_format_VRRc2(s390_irgen_VR, ovl);
                           goto ok;
   case 0xe700000000b4ULL: s390_format_VRRc1(s390_irgen_VGFM, ovl);
                           goto ok;
   case 0xe700000000b8ULL: s390_format_VRRd(s390_irgen_VMSL, ovl);
                           goto ok;
   case 0xe700000000b9ULL: s390_format_VRRd1(s390_irgen_VACCC, ovl);
                           goto ok;
   case 0xe700000000bbULL: s390_format_VRRd1(s390_irgen_VAC, ovl);
                           goto ok;
   case 0xe700000000bcULL: s390_format_VRRd1(s390_irgen_VGFMA, ovl);
                           goto ok;
   case 0xe700000000bdULL: s390_format_VRRd1(s390_irgen_VSBCBI, ovl);
                           goto ok;
   case 0xe700000000bfULL: s390_format_VRRd1(s390_irgen_VSBI, ovl);
                           goto ok;
   case 0xe700000000c0ULL: s390_format_VRRa(s390_irgen_VCLGD, ovl);
                           goto ok;
   case 0xe700000000c1ULL: s390_format_VRRa(s390_irgen_VCDLG, ovl);
                           goto ok;
   case 0xe700000000c2ULL: s390_format_VRRa(s390_irgen_VCGD, ovl);
                           goto ok;
   case 0xe700000000c3ULL: s390_format_VRRa(s390_irgen_VCDG, ovl);
                           goto ok;
   case 0xe700000000c4ULL: s390_format_VRRa2(s390_irgen_VFLL, ovl);
                           goto ok;
   case 0xe700000000c5ULL: s390_format_VRRa(s390_irgen_VFLR, ovl);
                           goto ok;
   case 0xe700000000c7ULL: s390_format_VRRa(s390_irgen_VFI, ovl);
                           goto ok;
   case 0xe700000000caULL: s390_format_VRRa2(s390_irgen_WFK, ovl);
                           goto ok;
   case 0xe700000000cbULL: s390_format_VRRa2(s390_irgen_WFC, ovl);
                           goto ok;
   case 0xe700000000ccULL: s390_format_VRRa(s390_irgen_VFPSO, ovl);
                           goto ok;
   case 0xe700000000ceULL: s390_format_VRRa2(s390_irgen_VFSQ, ovl);
                           goto ok;
   case 0xe700000000d4ULL: s390_format_VRRa1(s390_irgen_VUPLL, ovl);
                           goto ok;
   case 0xe700000000d5ULL: s390_format_VRRa1(s390_irgen_VUPLH, ovl);
                           goto ok;
   case 0xe700000000d6ULL: s390_format_VRRa1(s390_irgen_VUPL, ovl);
                           goto ok;
   case 0xe700000000d7ULL: s390_format_VRRa1(s390_irgen_VUPH, ovl);
                           goto ok;
   case 0xe700000000d8ULL: s390_format_VRRa0(s390_irgen_VTM, ovl);
                           goto ok;
   case 0xe700000000d9ULL: s390_format_VRRa1(s390_irgen_VECL, ovl);
                           goto ok;
   case 0xe700000000dbULL: s390_format_VRRa1(s390_irgen_VEC, ovl);
                           goto ok;
   case 0xe700000000deULL: s390_format_VRRa1(s390_irgen_VLC, ovl);
                           goto ok;
   case 0xe700000000dfULL: s390_format_VRRa1(s390_irgen_VLP, ovl);
                           goto ok;
   case 0xe700000000e2ULL: s390_format_VRRc2(s390_irgen_VFS, ovl);
                           goto ok;
   case 0xe700000000e3ULL: s390_format_VRRc2(s390_irgen_VFA, ovl);
                           goto ok;
   case 0xe700000000e5ULL: s390_format_VRRc2(s390_irgen_VFD, ovl);
                           goto ok;
   case 0xe700000000e7ULL: s390_format_VRRc2(s390_irgen_VFM, ovl);
                           goto ok;
   case 0xe700000000e8ULL: s390_format_VRRc(s390_irgen_VFCE, ovl);
                           goto ok;
   case 0xe700000000eaULL: s390_format_VRRc(s390_irgen_VFCHE, ovl);
                           goto ok;
   case 0xe700000000ebULL: s390_format_VRRc(s390_irgen_VFCH, ovl);
                           goto ok;
   case 0xe700000000eeULL: s390_format_VRRc(s390_irgen_VFMIN, ovl);
                           goto ok;
   case 0xe700000000efULL: s390_format_VRRc(s390_irgen_VFMAX, ovl);
                           goto ok;
   case 0xe700000000f0ULL: s390_format_VRRc1(s390_irgen_VAVGL, ovl);
                           goto ok;
   case 0xe700000000f1ULL: s390_format_VRRc1(s390_irgen_VACC, ovl);
                           goto ok;
   case 0xe700000000f2ULL: s390_format_VRRc1(s390_irgen_VAVG, ovl);
                           goto ok;
   case 0xe700000000f3ULL: s390_format_VRRc1(s390_irgen_VA, ovl);
                           goto ok;
   case 0xe700000000f5ULL: s390_format_VRRc1(s390_irgen_VSCBI, ovl);
                           goto ok;
   case 0xe700000000f7ULL: s390_format_VRRc1(s390_irgen_VS, ovl);
                           goto ok;
   case 0xe700000000f8ULL: s390_format_VRRb(s390_irgen_VCEQ, ovl);
                           goto ok;
   case 0xe700000000f9ULL: s390_format_VRRb(s390_irgen_VCHL, ovl);
                           goto ok;
   case 0xe700000000fbULL: s390_format_VRRb(s390_irgen_VCH, ovl);
                           goto ok;
   case 0xe700000000fcULL: s390_format_VRRc1(s390_irgen_VMNL, ovl);
                           goto ok;
   case 0xe700000000fdULL: s390_format_VRRc1(s390_irgen_VMXL, ovl);
                           goto ok;
   case 0xe700000000feULL: s390_format_VRRc1(s390_irgen_VMN, ovl);
                           goto ok;
   case 0xe700000000ffULL: s390_format_VRRc1(s390_irgen_VMX, ovl);
                           goto ok;
   case 0xeb0000000004ULL: s390_format_RSY(s390_irgen_LMG, ovl);
                           goto ok;
   case 0xeb000000000aULL: s390_format_RSY(s390_irgen_SRAG, ovl);
                           goto ok;
   case 0xeb000000000bULL: s390_format_RSY(s390_irgen_SLAG, ovl);
                           goto ok;
   case 0xeb000000000cULL: s390_format_RSY(s390_irgen_SRLG, ovl);
                           goto ok;
   case 0xeb000000000dULL: s390_format_RSY(s390_irgen_SLLG, ovl);
                           goto ok;
   case 0xeb000000000fULL: /* TRACG */ goto unimplemented;
   case 0xeb0000000014ULL: s390_format_RSY(s390_irgen_CSY, ovl);
                           goto ok;
   case 0xeb0000000016ULL: /* PFCR */ goto unimplemented;
   case 0xeb000000001cULL: s390_format_RSY(s390_irgen_RLLG, ovl);
                           goto ok;
   case 0xeb000000001dULL: s390_format_RSY(s390_irgen_RLL, ovl);
                           goto ok;
   case 0xeb0000000020ULL: s390_format_RSY(s390_irgen_CLMH, ovl);
                           goto ok;
   case 0xeb0000000021ULL: s390_format_RSY(s390_irgen_CLMY, ovl);
                           goto ok;
   case 0xeb0000000023ULL: s390_format_RSY(s390_irgen_CLT, ovl);
                           goto ok;
   case 0xeb0000000024ULL: s390_format_RSY(s390_irgen_STMG, ovl);
                           goto ok;
   case 0xeb0000000025ULL: /* STCTG */ goto unimplemented;
   case 0xeb0000000026ULL: s390_format_RSY(s390_irgen_STMH, ovl);
                           goto ok;
   case 0xeb000000002bULL: s390_format_RSY(s390_irgen_CLGT, ovl);
                           goto ok;
   case 0xeb000000002cULL: s390_format_RSY(s390_irgen_STCMH, ovl);
                           goto ok;
   case 0xeb000000002dULL: s390_format_RSY(s390_irgen_STCMY, ovl);
                           goto ok;
   case 0xeb000000002fULL: /* LCTLG */ goto unimplemented;
   case 0xeb0000000030ULL: s390_format_RSY(s390_irgen_CSG, ovl);
                           goto ok;
   case 0xeb0000000031ULL: s390_format_RSY(s390_irgen_CDSY, ovl);
                           goto ok;
   case 0xeb000000003eULL: s390_format_RSY(s390_irgen_CDSG, ovl);
                           goto ok;
   case 0xeb0000000044ULL: s390_format_RSY(s390_irgen_BXHG, ovl);
                           goto ok;
   case 0xeb0000000045ULL: s390_format_RSY(s390_irgen_BXLEG, ovl);
                           goto ok;
   case 0xeb000000004cULL: s390_format_RSY(s390_irgen_ECAG, ovl);
                           goto ok;
   case 0xeb0000000051ULL: s390_format_SIY(s390_irgen_TMY, ovl);
                           goto ok;
   case 0xeb0000000052ULL: s390_format_SIY(s390_irgen_MVIY, ovl);
                           goto ok;
   case 0xeb0000000054ULL: s390_format_SIY(s390_irgen_NIY, ovl);
                           goto ok;
   case 0xeb0000000055ULL: s390_format_SIY(s390_irgen_CLIY, ovl);
                           goto ok;
   case 0xeb0000000056ULL: s390_format_SIY(s390_irgen_OIY, ovl);
                           goto ok;
   case 0xeb0000000057ULL: s390_format_SIY(s390_irgen_XIY, ovl);
                           goto ok;
   case 0xeb000000006aULL: s390_format_SIY(s390_irgen_ASI, ovl);
                           goto ok;
   case 0xeb000000006eULL: s390_format_SIY(s390_irgen_ALSI, ovl);
                           goto ok;
   case 0xeb0000000071ULL: /* LPSWEY */ goto unimplemented;
   case 0xeb000000007aULL: s390_format_SIY(s390_irgen_AGSI, ovl);
                           goto ok;
   case 0xeb000000007eULL: s390_format_SIY(s390_irgen_ALGSI, ovl);
                           goto ok;
   case 0xeb0000000080ULL: s390_format_RSY(s390_irgen_ICMH, ovl);
                           goto ok;
   case 0xeb0000000081ULL: s390_format_RSY(s390_irgen_ICMY, ovl);
                           goto ok;
   case 0xeb000000008eULL: /* MVCLU */ goto unimplemented;
   case 0xeb000000008fULL: /* CLCLU */ goto unimplemented;
   case 0xeb0000000090ULL: s390_format_RSY(s390_irgen_STMY, ovl);
                           goto ok;
   case 0xeb0000000096ULL: s390_format_RSY(s390_irgen_LMH, ovl);
                           goto ok;
   case 0xeb0000000098ULL: s390_format_RSY(s390_irgen_LMY, ovl);
                           goto ok;
   case 0xeb000000009aULL: s390_format_RSY(s390_irgen_LAMY, ovl);
                           goto ok;
   case 0xeb000000009bULL: s390_format_RSY(s390_irgen_STAMY, ovl);
                           goto ok;
   case 0xeb00000000c0ULL: /* TP */ goto unimplemented;
   case 0xeb00000000dcULL: s390_format_RSY(s390_irgen_SRAK, ovl);
                           goto ok;
   case 0xeb00000000ddULL: s390_format_RSY(s390_irgen_SLAK, ovl);
                           goto ok;
   case 0xeb00000000deULL: s390_format_RSY(s390_irgen_SRLK, ovl);
                           goto ok;
   case 0xeb00000000dfULL: s390_format_RSY(s390_irgen_SLLK, ovl);
                           goto ok;
   case 0xeb00000000e0ULL: s390_format_RSYcond(s390_irgen_LOCFH, ovl);
                           goto ok;
   case 0xeb00000000e1ULL: s390_format_RSYcond(s390_irgen_STOCFH, ovl);
                           goto ok;
   case 0xeb00000000e2ULL: s390_format_RSYcond(s390_irgen_LOCG, ovl);
                           goto ok;
   case 0xeb00000000e3ULL: s390_format_RSYcond(s390_irgen_STOCG, ovl);
                           goto ok;
   case 0xeb00000000e4ULL: s390_format_RSY(s390_irgen_LANG, ovl);
                           goto ok;
   case 0xeb00000000e6ULL: s390_format_RSY(s390_irgen_LAOG, ovl);
                           goto ok;
   case 0xeb00000000e7ULL: s390_format_RSY(s390_irgen_LAXG, ovl);
                           goto ok;
   case 0xeb00000000e8ULL: s390_format_RSY(s390_irgen_LAAG, ovl);
                           goto ok;
   case 0xeb00000000eaULL: s390_format_RSY(s390_irgen_LAALG, ovl);
                           goto ok;
   case 0xeb00000000f2ULL: s390_format_RSYcond(s390_irgen_LOC, ovl);
                           goto ok;
   case 0xeb00000000f3ULL: s390_format_RSYcond(s390_irgen_STOC, ovl);
                           goto ok;
   case 0xeb00000000f4ULL: s390_format_RSY(s390_irgen_LAN, ovl);
                           goto ok;
   case 0xeb00000000f6ULL: s390_format_RSY(s390_irgen_LAO, ovl);
                           goto ok;
   case 0xeb00000000f7ULL: s390_format_RSY(s390_irgen_LAX, ovl);
                           goto ok;
   case 0xeb00000000f8ULL: s390_format_RSY(s390_irgen_LAA, ovl);
                           goto ok;
   case 0xeb00000000faULL: s390_format_RSY(s390_irgen_LAAL, ovl);
                           goto ok;
   case 0xec0000000042ULL: s390_irgen_LOCHI(RIEg_r1(ovl), RIEg_m3(ovl),
                                            RIEg_i2(ovl));
                           goto ok;
   case 0xec0000000044ULL: s390_irgen_BRXHG(RIEe_r1(ovl), RIEe_r3(ovl),
                                            RIEe_i2(ovl));
                           goto ok;
   case 0xec0000000045ULL: s390_irgen_BRXLG(RIEe_r1(ovl), RIEe_r3(ovl),
                                            RIEe_i2(ovl));
                           goto ok;
   case 0xec0000000046ULL: s390_irgen_LOCGHI(RIEg_r1(ovl), RIEg_m3(ovl),
                                             RIEg_i2(ovl));
                           goto ok;
   case 0xec000000004eULL: s390_irgen_LOCHHI(RIEg_r1(ovl), RIEg_m3(ovl),
                                             RIEg_i2(ovl));
                           goto ok;
   case 0xec0000000051ULL: s390_irgen_RISBLG(RIEf_r1(ovl), RIEf_r2(ovl),
                                             RIEf_i3(ovl), RIEf_i4(ovl),
                                             RIEf_i5(ovl));
                           goto ok;
   case 0xec0000000054ULL: s390_irgen_RNSBG(RIEf_r1(ovl), RIEf_r2(ovl),
                                            RIEf_i3(ovl), RIEf_i4(ovl),
                                            RIEf_i5(ovl));
                           goto ok;
   case 0xec0000000055ULL: s390_irgen_RISBG(RIEf_r1(ovl), RIEf_r2(ovl),
                                            RIEf_i3(ovl), RIEf_i4(ovl),
                                            RIEf_i5(ovl));
                           goto ok;
   case 0xec0000000056ULL: s390_irgen_ROSBG(RIEf_r1(ovl), RIEf_r2(ovl),
                                            RIEf_i3(ovl), RIEf_i4(ovl),
                                            RIEf_i5(ovl));
                           goto ok;
   case 0xec0000000057ULL: s390_irgen_RXSBG(RIEf_r1(ovl), RIEf_r2(ovl),
                                            RIEf_i3(ovl), RIEf_i4(ovl),
                                            RIEf_i5(ovl));
                           goto ok;
   case 0xec0000000059ULL: s390_irgen_RISBGN(RIEf_r1(ovl), RIEf_r2(ovl),
                                             RIEf_i3(ovl), RIEf_i4(ovl),
                                             RIEf_i5(ovl));
                           goto ok;
   case 0xec000000005dULL: s390_irgen_RISBHG(RIEf_r1(ovl), RIEf_r2(ovl),
                                             RIEf_i3(ovl), RIEf_i4(ovl),
                                             RIEf_i5(ovl));
                           goto ok;
   case 0xec0000000064ULL: s390_irgen_CGRJ(RIEb_r1(ovl), RIEb_r2(ovl),
                                           RIEb_i4(ovl), RIEb_m3(ovl));
                           goto ok;
   case 0xec0000000065ULL: s390_irgen_CLGRJ(RIEb_r1(ovl), RIEb_r2(ovl),
                                            RIEb_i4(ovl), RIEb_m3(ovl));
                           goto ok;
   case 0xec0000000070ULL: s390_irgen_CGIT(RIEa_r1(ovl), RIEa_i2(ovl),
                                           RIEa_m3(ovl));
                           goto ok;
   case 0xec0000000071ULL: s390_irgen_CLGIT(RIEa_r1(ovl), RIEa_i2(ovl),
                                            RIEa_m3(ovl));
                           goto ok;
   case 0xec0000000072ULL: s390_irgen_CIT(RIEa_r1(ovl), RIEa_i2(ovl),
                                          RIEa_m3(ovl));
                           goto ok;
   case 0xec0000000073ULL: s390_irgen_CLFIT(RIEa_r1(ovl), RIEa_i2(ovl),
                                            RIEa_m3(ovl));
                           goto ok;
   case 0xec0000000076ULL: s390_irgen_CRJ(RIEb_r1(ovl), RIEb_r2(ovl),
                                          RIEb_i4(ovl), RIEb_m3(ovl));
                           goto ok;
   case 0xec0000000077ULL: s390_irgen_CLRJ(RIEb_r1(ovl), RIEb_r2(ovl),
                                           RIEb_i4(ovl), RIEb_m3(ovl));
                           goto ok;
   case 0xec000000007cULL: s390_irgen_CGIJ(RIEc_r1(ovl), RIEc_m3(ovl),
                                           RIEc_i4(ovl), RIEc_i2(ovl));
                           goto ok;
   case 0xec000000007dULL: s390_irgen_CLGIJ(RIEc_r1(ovl), RIEc_m3(ovl),
                                            RIEc_i4(ovl), RIEc_i2(ovl));
                           goto ok;
   case 0xec000000007eULL: s390_irgen_CIJ(RIEc_r1(ovl), RIEc_m3(ovl),
                                          RIEc_i4(ovl), RIEc_i2(ovl));
                           goto ok;
   case 0xec000000007fULL: s390_irgen_CLIJ(RIEc_r1(ovl), RIEc_m3(ovl),
                                           RIEc_i4(ovl), RIEc_i2(ovl));
                           goto ok;
   case 0xec00000000d8ULL: s390_irgen_AHIK(RIEd_r1(ovl), RIEd_r3(ovl),
                                           RIEd_i2(ovl));
                           goto ok;
   case 0xec00000000d9ULL: s390_irgen_AGHIK(RIEd_r1(ovl), RIEd_r3(ovl),
                                            RIEd_i2(ovl));
                           goto ok;
   case 0xec00000000daULL: s390_irgen_ALHSIK(RIEd_r1(ovl), RIEd_r3(ovl),
                                             RIEd_i2(ovl));
                           goto ok;
   case 0xec00000000dbULL: s390_irgen_ALGHSIK(RIEd_r1(ovl), RIEd_r3(ovl),
                                              RIEd_i2(ovl));
                           goto ok;
   case 0xec00000000e4ULL: s390_format_RRS(s390_irgen_CGRB, ovl);
                           goto ok;
   case 0xec00000000e5ULL: s390_format_RRS(s390_irgen_CLGRB, ovl);
                           goto ok;
   case 0xec00000000f6ULL: s390_format_RRS(s390_irgen_CRB, ovl);
                           goto ok;
   case 0xec00000000f7ULL: s390_format_RRS(s390_irgen_CLRB, ovl);
                           goto ok;
   case 0xec00000000fcULL: s390_format_RIS(s390_irgen_CGIB, ovl);
                           goto ok;
   case 0xec00000000fdULL: s390_format_RIS(s390_irgen_CLGIB, ovl);
                           goto ok;
   case 0xec00000000feULL: s390_format_RIS(s390_irgen_CIB, ovl);
                           goto ok;
   case 0xec00000000ffULL: s390_format_RIS(s390_irgen_CLIB, ovl);
                           goto ok;
   case 0xed0000000004ULL: s390_format_RXE0(s390_irgen_LDEB, ovl);
                           goto ok;
   case 0xed0000000005ULL: s390_format_RXE0(s390_irgen_LXDB, ovl);
                           goto ok;
   case 0xed0000000006ULL: s390_format_RXE0(s390_irgen_LXEB, ovl);
                           goto ok;
   case 0xed0000000007ULL: /* MXDB */ goto unimplemented;
   case 0xed0000000008ULL: s390_format_RXE0(s390_irgen_KEB, ovl);
                           goto ok;
   case 0xed0000000009ULL: s390_format_RXE0(s390_irgen_CEB, ovl);
                           goto ok;
   case 0xed000000000aULL: s390_format_RXE0(s390_irgen_AEB, ovl);
                           goto ok;
   case 0xed000000000bULL: s390_format_RXE0(s390_irgen_SEB, ovl);
                           goto ok;
   case 0xed000000000cULL: /* MDEB */ goto unimplemented;
   case 0xed000000000dULL: s390_format_RXE0(s390_irgen_DEB, ovl);
                           goto ok;
   case 0xed000000000eULL: s390_format_RXF(s390_irgen_MAEB, ovl);
                           goto ok;
   case 0xed000000000fULL: s390_format_RXF(s390_irgen_MSEB, ovl);
                           goto ok;
   case 0xed0000000010ULL: s390_format_RXE0(s390_irgen_TCEB, ovl);
                           goto ok;
   case 0xed0000000011ULL: s390_format_RXE0(s390_irgen_TCDB, ovl);
                           goto ok;
   case 0xed0000000012ULL: s390_format_RXE0(s390_irgen_TCXB, ovl);
                           goto ok;
   case 0xed0000000014ULL: s390_format_RXE0(s390_irgen_SQEB, ovl);
                           goto ok;
   case 0xed0000000015ULL: s390_format_RXE0(s390_irgen_SQDB, ovl);
                           goto ok;
   case 0xed0000000017ULL: s390_format_RXE0(s390_irgen_MEEB, ovl);
                           goto ok;
   case 0xed0000000018ULL: s390_format_RXE0(s390_irgen_KDB, ovl);
                           goto ok;
   case 0xed0000000019ULL: s390_format_RXE0(s390_irgen_CDB, ovl);
                           goto ok;
   case 0xed000000001aULL: s390_format_RXE0(s390_irgen_ADB, ovl);
                           goto ok;
   case 0xed000000001bULL: s390_format_RXE0(s390_irgen_SDB, ovl);
                           goto ok;
   case 0xed000000001cULL: s390_format_RXE0(s390_irgen_MDB, ovl);
                           goto ok;
   case 0xed000000001dULL: s390_format_RXE0(s390_irgen_DDB, ovl);
                           goto ok;
   case 0xed000000001eULL: s390_format_RXF(s390_irgen_MADB, ovl);
                           goto ok;
   case 0xed000000001fULL: s390_format_RXF(s390_irgen_MSDB, ovl);
                           goto ok;
   case 0xed0000000024ULL: s390_format_RXE0(s390_irgen_LDE, ovl);
                           goto ok;
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
   case 0xed0000000040ULL: s390_format_RXF(s390_irgen_SLDT, ovl);
                           goto ok;
   case 0xed0000000041ULL: s390_format_RXF(s390_irgen_SRDT, ovl);
                           goto ok;
   case 0xed0000000048ULL: s390_format_RXF(s390_irgen_SLXT, ovl);
                           goto ok;
   case 0xed0000000049ULL: s390_format_RXF(s390_irgen_SRXT, ovl);
                           goto ok;
   case 0xed0000000050ULL: s390_format_RXE0(s390_irgen_TDCET, ovl);
                           goto ok;
   case 0xed0000000051ULL: s390_format_RXE0(s390_irgen_TDGET, ovl);
                           goto ok;
   case 0xed0000000054ULL: s390_format_RXE0(s390_irgen_TDCDT, ovl);
                           goto ok;
   case 0xed0000000055ULL: s390_format_RXE0(s390_irgen_TDGDT, ovl);
                           goto ok;
   case 0xed0000000058ULL: s390_format_RXE0(s390_irgen_TDCXT, ovl);
                           goto ok;
   case 0xed0000000059ULL: s390_format_RXE0(s390_irgen_TDGXT, ovl);
                           goto ok;
   case 0xed0000000064ULL: s390_format_RXY(s390_irgen_LEY, ovl);
                           goto ok;
   case 0xed0000000065ULL: s390_format_RXY(s390_irgen_LDY, ovl);
                           goto ok;
   case 0xed0000000066ULL: s390_format_RXY(s390_irgen_STEY, ovl);
                           goto ok;
   case 0xed0000000067ULL: s390_format_RXY(s390_irgen_STDY, ovl);
                           goto ok;
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
   case 0xc000ULL: s390_irgen_LARL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc001ULL: s390_irgen_LGFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc004ULL: s390_irgen_BRCL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc005ULL: s390_irgen_BRASL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc006ULL: s390_irgen_XIHF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc007ULL: s390_irgen_XILF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc008ULL: s390_irgen_IIHF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc009ULL: s390_irgen_IILF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc00aULL: s390_irgen_NIHF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc00bULL: s390_irgen_NILF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc00cULL: s390_irgen_OIHF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc00dULL: s390_irgen_OILF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc00eULL: s390_irgen_LLIHF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc00fULL: s390_irgen_LLILF(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc200ULL: s390_irgen_MSGFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc201ULL: s390_irgen_MSFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc204ULL: s390_irgen_SLGFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc205ULL: s390_irgen_SLFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc208ULL: s390_irgen_AGFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc209ULL: s390_irgen_AFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc20aULL: s390_irgen_ALGFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc20bULL: s390_irgen_ALFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc20cULL: s390_irgen_CGFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc20dULL: s390_irgen_CFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc20eULL: s390_irgen_CLGFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc20fULL: s390_irgen_CLFI(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc402ULL: s390_irgen_LLHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc404ULL: s390_irgen_LGHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc405ULL: s390_irgen_LHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc406ULL: s390_irgen_LLGHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc407ULL: s390_irgen_STHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc408ULL: s390_irgen_LGRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc40bULL: s390_irgen_STGRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc40cULL: s390_irgen_LGFRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc40dULL: s390_irgen_LRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc40eULL: s390_irgen_LLGFRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc40fULL: s390_irgen_STRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc600ULL: s390_irgen_EXRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc602ULL: s390_irgen_PFDRL();
                   goto ok;
   case 0xc604ULL: s390_irgen_CGHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc605ULL: s390_irgen_CHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc606ULL: s390_irgen_CLGHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc607ULL: s390_irgen_CLHRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc608ULL: s390_irgen_CGRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc60aULL: s390_irgen_CLGRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc60cULL: s390_irgen_CGFRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc60dULL: s390_irgen_CRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc60eULL: s390_irgen_CLGFRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc60fULL: s390_irgen_CLRL(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xc800ULL: /* MVCOS */ goto unimplemented;
   case 0xc801ULL: /* ECTG */ goto unimplemented;
   case 0xc802ULL: /* CSST */ goto unimplemented;
   case 0xc804ULL: /* LPD */ goto unimplemented;
   case 0xc805ULL: /* LPDG */ goto unimplemented;
   case 0xc806ULL: /* CAL */ goto unimplemented;
   case 0xc807ULL: /* CALG */ goto unimplemented;
   case 0xc80fULL: /* CALGF */ goto unimplemented;
   case 0xcc06ULL: s390_irgen_BRCTH(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xcc08ULL: s390_irgen_AIH(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xcc0aULL: s390_irgen_ALSIH(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xcc0bULL: s390_irgen_ALSIHN(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xcc0dULL: s390_irgen_CIH(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   case 0xcc0fULL: s390_irgen_CLIH(RIL_r1(ovl), RIL_i2(ovl));
                   goto ok;
   }

   switch (((ovl >> 16) & 0xff0000000000ULL) >> 40) {
   case 0xc5ULL: s390_irgen_BPRP(MII_m1(ovl), MII_i2(ovl), MII_i3(ovl));
                 goto ok;
   case 0xc7ULL: s390_format_SMI(s390_irgen_BPP, ovl);
                 goto ok;
   case 0xd0ULL: /* TRTR */ goto unimplemented;
   case 0xd1ULL: /* MVN */ goto unimplemented;
   case 0xd2ULL: s390_format_SSa(s390_irgen_MVC, ovl);
                 goto ok;
   case 0xd3ULL: /* MVZ */ goto unimplemented;
   case 0xd4ULL: s390_format_SSa(s390_irgen_NC, ovl);
                 goto ok;
   case 0xd5ULL: s390_format_SSa(s390_irgen_CLC, ovl);
                 goto ok;
   case 0xd6ULL: s390_format_SSa(s390_irgen_OC, ovl);
                 goto ok;
   case 0xd7ULL:
      if (SSa_b1(ovl) == SSa_b2(ovl) && SSa_d1(ovl) == SSa_d2(ovl))
         s390_irgen_XC_sameloc(SSa_l(ovl), SSa_b1(ovl), SSa_d1(ovl));
      else
        s390_format_SSa(s390_irgen_XC, ovl);
      goto ok;
   case 0xd9ULL: /* MVCK */ goto unimplemented;
   case 0xdaULL: /* MVCP */ goto unimplemented;
   case 0xdbULL: /* MVCS */ goto unimplemented;
   case 0xdcULL: s390_format_SSa(s390_irgen_TR, ovl);
                 goto ok;
   case 0xddULL: /* TRT */ goto unimplemented;
   case 0xdeULL: /* ED */ goto unimplemented;
   case 0xdfULL: /* EDMK */ goto unimplemented;
   case 0xe1ULL: /* PKU */ goto unimplemented;
   case 0xe2ULL: /* UNPKU */ goto unimplemented;
   case 0xe8ULL: s390_format_SSa(s390_irgen_MVCIN, ovl);
                 goto ok;
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
   case 0xe50aULL: s390_format_SSE(s390_irgen_MVCRL, ovl);
                   goto ok;
   case 0xe50eULL: /* MVCSK */ goto unimplemented;
   case 0xe50fULL: /* MVCDK */ goto unimplemented;
   case 0xe544ULL: s390_format_SIL(s390_irgen_MVHHI, ovl);
                   goto ok;
   case 0xe548ULL: s390_format_SIL(s390_irgen_MVGHI, ovl);
                   goto ok;
   case 0xe54cULL: s390_format_SIL(s390_irgen_MVHI, ovl);
                   goto ok;
   case 0xe554ULL: s390_format_SIL(s390_irgen_CHHSI, ovl);
                   goto ok;
   case 0xe555ULL: s390_format_SIL(s390_irgen_CLHHSI, ovl);
                   goto ok;
   case 0xe558ULL: s390_format_SIL(s390_irgen_CGHSI, ovl);
                   goto ok;
   case 0xe559ULL: s390_format_SIL(s390_irgen_CLGHSI, ovl);
                   goto ok;
   case 0xe55cULL: s390_format_SIL(s390_irgen_CHSI, ovl);
                   goto ok;
   case 0xe55dULL: s390_format_SIL(s390_irgen_CLFHSI, ovl);
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
      s390_irgen_inject_ir();
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
      if (UNLIKELY(vex_traceflags & VEX_TRACE_FE)) {
         HChar *str = s390_disasm(bytes, /* padmnm */ 1);
         vex_printf("%s\n", str ? str : "disassembly failed");
      }
 
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
   if (dis_res->whatNext == Dis_Continue &&
       (bytes[insn_length] == 0x44 ||
        (bytes[insn_length] == 0xc6 && (bytes[insn_length + 1] & 0xf) == 0))) {
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

      for (unsigned i = 0; i < insn_length; i += 2) {
         if (i != 0)
            vex_printf(" ");
         vex_printf("%02x%02x", bytes[i], bytes[i + 1]);
      }
      if (status == S390_DECODE_UNIMPLEMENTED_INSN ||
          status == S390_DECODE_SPECIFICATION_EXCEPTION) {
         const HChar *str = s390_disasm(bytes, /* padmnm */ 0);
         vex_printf("   %s", str == NULL ? "??????" : str);
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
              const VexAbiInfo*  abiinfo,
                        VexEndness         host_endness,
                        Bool               sigill_diag_IN)
{
   vassert(guest_arch == VexArchS390X);

   /* Set globals (see top of this file) */
   guest_IA_curr_instr = guest_IP;
   if (last_execute_target == Invalid_execute_target)
      guest_IA_rel_base = guest_IA_curr_instr;
   irsb        = irsb_IN;
   sigill_diag = sigill_diag_IN;

   return disInstr_S390_WRK(guest_code + delta);
}

/*---------------------------------------------------------------*/
/*--- end                                   guest_s390_toIR.c ---*/
/*---------------------------------------------------------------*/
