
/*--------------------------------------------------------------------*/
/*--- begin                                       guest_arm_toIR.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
      info@open-works.net

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

/* Limitations, etc

   - pretty dodgy exception semantics for {LD,ST}Mxx, no doubt

   - SWP: the restart jump back is Ijk_Boring; it should be
     Ijk_NoRedir but that's expensive.  See comments on casLE() in
     guest_x86_toIR.c.

*/

/* "Special" instructions.

   This instruction decoder can decode four special instructions
   which mean nothing natively (are no-ops as far as regs/mem are
   concerned) but have meaning for supporting Valgrind.  A special
   instruction is flagged by a 16-byte preamble:

      E1A0C1EC E1A0C6EC E1A0CEEC E1A0C9EC
      (mov r12, r12, ROR #3;   mov r12, r12, ROR #13;
       mov r12, r12, ROR #29;  mov r12, r12, ROR #19)

   Following that, one of the following 3 are allowed
   (standard interpretation in parentheses):

      E18AA00A (orr r10,r10,r10)   R3 = client_request ( R4 )
      E18BB00B (orr r11,r11,r11)   R3 = guest_NRADDR
      E18CC00C (orr r12,r12,r12)   branch-and-link-to-noredir R4

   Any other bytes following the 16-byte preamble are illegal and
   constitute a failure in instruction decoding.  This all assumes
   that the preamble will never occur except in specific code
   fragments designed for Valgrind to catch.
*/

/* Translates ARM(v5) code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_arm.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_arm_defs.h"


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

/* CONST: The guest address for the instruction currently being
   translated. */
static Addr32 guest_R15_curr_instr;

/* MOD: The IRSB* into which we're generating code. */
static IRSB* irsb;

/* These are to do with handling writes to r15.  They are initially
   set at the start of disInstr_ARM_WRK to indicate no update,
   possibly updated during the routine, and examined again at the end.
   If they have been set to indicate a r15 update then a jump is
   generated.  Note, "explicit" jumps (b, bx, etc) are generated
   directly, not using this mechanism -- this is intended to handle
   the implicit-style jumps resulting from (eg) assigning to r15 as
   the result of insns we wouldn't normally consider branchy. */

/* MOD.  Initially False; set to True iff abovementioned handling is
   required. */
static Bool r15written;

/* MOD.  Initially IRTemp_INVALID.  If the r15 branch to be generated
   is conditional, this holds the gating IRTemp :: Ity_I32.  If the
   branch to be generated is unconditional, this remains
   IRTemp_INVALID. */
static IRTemp r15guard; /* :: Ity_I32, 0 or 1 */

/* MOD.  Initially Ijk_Boring.  If an r15 branch is to be generated,
   this holds the jump kind. */
static IRTemp r15kind;


/*------------------------------------------------------------*/
/*--- Debugging output                                     ---*/
/*------------------------------------------------------------*/

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

#define DIS(buf, format, args...)      \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_sprintf(buf, format, ## args)


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- arm insn stream.                                     ---*/
/*------------------------------------------------------------*/

/* Do a little-endian load of a 32-bit word, regardless of the
   endianness of the underlying host. */
static inline UInt getUIntLittleEndianly ( UChar* p )
{
   UInt w = 0;
   w = (w << 8) | p[3];
   w = (w << 8) | p[2];
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
   return w;
}

static UInt ROR32 ( UInt x, UInt sh ) {
   vassert(sh >= 0 && sh < 32);
   if (sh == 0)
      return x;
   else
      return (x << (32-sh)) | (x >> sh);
}

#define BITS2(_b1,_b0) \
   (((_b1) << 1) | (_b0))

#define BITS3(_b2,_b1,_b0)                      \
  (((_b2) << 2) | ((_b1) << 1) | (_b0))

#define BITS4(_b3,_b2,_b1,_b0) \
   (((_b3) << 3) | ((_b2) << 2) | ((_b1) << 1) | (_b0))

#define BITS8(_b7,_b6,_b5,_b4,_b3,_b2,_b1,_b0)  \
   ((BITS4((_b7),(_b6),(_b5),(_b4)) << 4) \
    | BITS4((_b3),(_b2),(_b1),(_b0)))

/* produces _uint[_bMax:_bMin] */
#define SLICE_UInt(_uint,_bMax,_bMin) \
   (( ((UInt)(_uint)) >> (_bMin)) \
      & ((1 << ((_bMax) - (_bMin) + 1)) - 1))


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for creating IR fragments.    ---*/
/*------------------------------------------------------------*/

static IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static IRExpr* mkU8 ( UInt i )
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8( (UChar)i ));
}

static IRExpr* mkexpr ( IRTemp tmp )
{
   return IRExpr_RdTmp(tmp);
}

static IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

static IRExpr* binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   return IRExpr_Binop(op, a1, a2);
}

static IRExpr* triop ( IROp op, IRExpr* a1, IRExpr* a2, IRExpr* a3 )
{
   return IRExpr_Triop(op, a1, a2, a3);
}

static IRExpr* loadLE ( IRType ty, IRExpr* addr )
{
   return IRExpr_Load(Iend_LE, ty, addr);
}

/* Add a statement to the list held by "irbb". */
static void stmt ( IRStmt* st )
{
   addStmtToIRSB( irsb, st );
}

static void assign ( IRTemp dst, IRExpr* e )
{
   stmt( IRStmt_WrTmp(dst, e) );
}

static void storeLE ( IRExpr* addr, IRExpr* data )
{
   stmt( IRStmt_Store(Iend_LE, addr, data) );
}

/* Generate a new temporary of the given type. */
static IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp( irsb->tyenv, ty );
}

/* Produces a value in 0 .. 3, which is encoded as per the type
   IRRoundingMode. */
static IRExpr* /* :: Ity_I32 */ get_FAKE_roundingmode ( void )
{
   return mkU32(Irrm_NEAREST);
}

/* Generate an expression for SRC rotated right by ROT. */
static IRExpr* genROR32( IRTemp src, Int rot )
{
   vassert(rot >= 0 && rot < 32);
   if (rot == 0)
      return mkexpr(src);
   return
      binop(Iop_Or32,
            binop(Iop_Shl32, mkexpr(src), mkU8(32 - rot)),
            binop(Iop_Shr32, mkexpr(src), mkU8(rot)));
}


/*------------------------------------------------------------*/
/*--- Helpers for accessing guest registers.               ---*/
/*------------------------------------------------------------*/

#define OFFB_R0       offsetof(VexGuestARMState,guest_R0)
#define OFFB_R1       offsetof(VexGuestARMState,guest_R1)
#define OFFB_R2       offsetof(VexGuestARMState,guest_R2)
#define OFFB_R3       offsetof(VexGuestARMState,guest_R3)
#define OFFB_R4       offsetof(VexGuestARMState,guest_R4)
#define OFFB_R5       offsetof(VexGuestARMState,guest_R5)
#define OFFB_R6       offsetof(VexGuestARMState,guest_R6)
#define OFFB_R7       offsetof(VexGuestARMState,guest_R7)
#define OFFB_R8       offsetof(VexGuestARMState,guest_R8)
#define OFFB_R9       offsetof(VexGuestARMState,guest_R9)
#define OFFB_R10      offsetof(VexGuestARMState,guest_R10)
#define OFFB_R11      offsetof(VexGuestARMState,guest_R11)
#define OFFB_R12      offsetof(VexGuestARMState,guest_R12)
#define OFFB_R13      offsetof(VexGuestARMState,guest_R13)
#define OFFB_R14      offsetof(VexGuestARMState,guest_R14)
#define OFFB_R15      offsetof(VexGuestARMState,guest_R15)

#define OFFB_CC_OP    offsetof(VexGuestARMState,guest_CC_OP)
#define OFFB_CC_DEP1  offsetof(VexGuestARMState,guest_CC_DEP1)
#define OFFB_CC_DEP2  offsetof(VexGuestARMState,guest_CC_DEP2)
#define OFFB_CC_NDEP  offsetof(VexGuestARMState,guest_CC_NDEP)
#define OFFB_NRADDR   offsetof(VexGuestARMState,guest_NRADDR)

#define OFFB_D0       offsetof(VexGuestARMState,guest_D0)
#define OFFB_D1       offsetof(VexGuestARMState,guest_D1)
#define OFFB_D2       offsetof(VexGuestARMState,guest_D2)
#define OFFB_D3       offsetof(VexGuestARMState,guest_D3)
#define OFFB_D4       offsetof(VexGuestARMState,guest_D4)
#define OFFB_D5       offsetof(VexGuestARMState,guest_D5)
#define OFFB_D6       offsetof(VexGuestARMState,guest_D6)
#define OFFB_D7       offsetof(VexGuestARMState,guest_D7)
#define OFFB_D8       offsetof(VexGuestARMState,guest_D8)
#define OFFB_D9       offsetof(VexGuestARMState,guest_D9)
#define OFFB_D10      offsetof(VexGuestARMState,guest_D10)
#define OFFB_D11      offsetof(VexGuestARMState,guest_D11)
#define OFFB_D12      offsetof(VexGuestARMState,guest_D12)
#define OFFB_D13      offsetof(VexGuestARMState,guest_D13)
#define OFFB_D14      offsetof(VexGuestARMState,guest_D14)
#define OFFB_D15      offsetof(VexGuestARMState,guest_D15)

#define OFFB_FPSCR    offsetof(VexGuestARMState,guest_FPSCR)
#define OFFB_TPIDRURO offsetof(VexGuestARMState,guest_TPIDRURO)


/* ---------------- Integer registers ---------------- */

static Int integerGuestRegOffset ( UInt iregNo )
{
   /* Do we care about endianness here?  We do if sub-parts of integer
      registers are accessed, but I don't think that ever happens on
      ARM. */
   switch (iregNo) {
      case 0:  return OFFB_R0;
      case 1:  return OFFB_R1;
      case 2:  return OFFB_R2;
      case 3:  return OFFB_R3;
      case 4:  return OFFB_R4;
      case 5:  return OFFB_R5;
      case 6:  return OFFB_R6;
      case 7:  return OFFB_R7;
      case 8:  return OFFB_R8;
      case 9:  return OFFB_R9;
      case 10: return OFFB_R10;
      case 11: return OFFB_R11;
      case 12: return OFFB_R12;
      case 13: return OFFB_R13;
      case 14: return OFFB_R14;
      case 15: return OFFB_R15;
      default: vassert(0);
   }
}

/* Plain ("low level") read from a reg; no +8 offset magic for r15. */
static IRExpr* llGetIReg ( UInt iregNo )
{
   vassert(iregNo < 16);
   return IRExpr_Get( integerGuestRegOffset(iregNo), Ity_I32 );
}

/* Architected read from a reg.  This automagically adds 8 to all 
   reads of r15. */
static IRExpr* getIReg ( UInt iregNo )
{
   IRExpr* e;
   vassert(iregNo < 16);
   if (iregNo == 15) {
      /* If asked for r15, don't read the guest state value, as that
         may not be up to date in the case where loop unrolling has
         happened, because the first insn's write to the block is
         omitted; hence in the 2nd and subsequent unrollings we don't
         have a correct value in guest r15.  Instead produce the
         constant that we know would be produced at this point. */
      e = mkU32(guest_R15_curr_instr + 8);
   } else {
      e = IRExpr_Get( integerGuestRegOffset(iregNo), Ity_I32 );
   }
   return e;
}

/* Plain ("low level") write to a reg; no jump or alignment magic for
   r15. */
static void llPutIReg ( UInt iregNo, IRExpr* e )
{
   vassert(iregNo < 16);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I32);
   stmt( IRStmt_Put(integerGuestRegOffset(iregNo), e) );
}

/* Architected write to a reg.  If it is to r15, record info so at the
   end of this insn's translation, a branch to it can be made.  Also
   handles conditional writes to the register:
   if guardT == IRTemp_INVALID then the write is unconditional.
   If writing r15, also 4-align it. */
static void putIReg ( UInt       iregNo,
                      IRExpr*    e,
                      IRTemp     guardT /* :: Ity_I32, 0 or 1 */,
                      IRJumpKind jk /* if a jump is generated */ )
{
   /* if writing r15, force e to be 4-aligned. */
   if (iregNo == 15)
      e = binop(Iop_And32, e, mkU32(~3));
   /* So, generate either an unconditional or a conditional write to
      the reg. */
   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      llPutIReg( iregNo, e );
   } else {
      llPutIReg( iregNo,
                 IRExpr_Mux0X( unop(Iop_32to8, mkexpr(guardT)),
                               llGetIReg(iregNo),
                               e ));
   }
   if (iregNo == 15) {
      // assert against competing r15 updates.  Shouldn't
      // happen; should be ruled out by the instr matching
      // logic.
      vassert(r15written == False);
      vassert(r15guard   == IRTemp_INVALID);
      vassert(r15kind    == Ijk_Boring);
      r15written = True;
      r15guard   = guardT;
      r15kind    = jk;
   }
}


/* ---------------- Double registers ---------------- */

static Int doubleGuestRegOffset ( UInt dregNo )
{
   /* Do we care about endianness here?  Probably do if we ever get
      into the situation of dealing with the single-precision VFP
      registers. */
   switch (dregNo) {
      case 0:  return OFFB_D0;
      case 1:  return OFFB_D1;
      case 2:  return OFFB_D2;
      case 3:  return OFFB_D3;
      case 4:  return OFFB_D4;
      case 5:  return OFFB_D5;
      case 6:  return OFFB_D6;
      case 7:  return OFFB_D7;
      case 8:  return OFFB_D8;
      case 9:  return OFFB_D9;
      case 10: return OFFB_D10;
      case 11: return OFFB_D11;
      case 12: return OFFB_D12;
      case 13: return OFFB_D13;
      case 14: return OFFB_D14;
      case 15: return OFFB_D15;
      default: vassert(0);
   }
}

/* Plain ("low level") read from a VFP Dreg. */
static IRExpr* llGetDReg ( UInt dregNo )
{
   vassert(dregNo < 16);
   return IRExpr_Get( doubleGuestRegOffset(dregNo), Ity_F64 );
}

/* Architected read from a VFP Dreg. */
static IRExpr* getDReg ( UInt dregNo ) {
   return llGetDReg( dregNo );
}

/* Plain ("low level") write to a VFP Dreg. */
static void llPutDReg ( UInt dregNo, IRExpr* e )
{
   vassert(dregNo < 16);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_F64);
   stmt( IRStmt_Put(doubleGuestRegOffset(dregNo), e) );
}

/* Architected write to a VFP Dreg.  Handles conditional writes to the
   register: if guardT == IRTemp_INVALID then the write is
   unconditional. */
static void putDReg ( UInt    dregNo,
                      IRExpr* e,
                      IRTemp  guardT /* :: Ity_I32, 0 or 1 */)
{
   /* So, generate either an unconditional or a conditional write to
      the reg. */
   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      llPutDReg( dregNo, e );
   } else {
      llPutDReg( dregNo,
                 IRExpr_Mux0X( unop(Iop_32to8, mkexpr(guardT)),
                               llGetDReg(dregNo),
                               e ));
   }
}


/* ---------------- Float registers ---------------- */

static Int floatGuestRegOffset ( UInt fregNo )
{
   /* Start with the offset of the containing double, and the correct
      for endianness.  Actually this is completely bogus and needs
      careful thought. */
   Int off;
   vassert(fregNo < 32);
   off = doubleGuestRegOffset(fregNo >> 1);
   if (host_is_bigendian) {
      vassert(0);
   } else {
      if (fregNo & 1)
         off += 4;
   }
   return off;
}

/* Plain ("low level") read from a VFP Freg. */
static IRExpr* llGetFReg ( UInt fregNo )
{
   vassert(fregNo < 32);
   return IRExpr_Get( floatGuestRegOffset(fregNo), Ity_F32 );
}

/* Architected read from a VFP Freg. */
static IRExpr* getFReg ( UInt fregNo ) {
   return llGetFReg( fregNo );
}

/* Plain ("low level") write to a VFP Freg. */
static void llPutFReg ( UInt fregNo, IRExpr* e )
{
   vassert(fregNo < 32);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_F32);
   stmt( IRStmt_Put(floatGuestRegOffset(fregNo), e) );
}

/* Architected write to a VFP Freg.  Handles conditional writes to the
   register: if guardT == IRTemp_INVALID then the write is
   unconditional. */
static void putFReg ( UInt    fregNo,
                      IRExpr* e,
                      IRTemp  guardT /* :: Ity_I32, 0 or 1 */)
{
   /* So, generate either an unconditional or a conditional write to
      the reg. */
   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      llPutFReg( fregNo, e );
   } else {
      llPutFReg( fregNo,
                 IRExpr_Mux0X( unop(Iop_32to8, mkexpr(guardT)),
                               llGetFReg(fregNo),
                               e ));
   }
}


/* ---------------- Misc registers ---------------- */

static void putMiscReg32 ( UInt    gsoffset, 
                           IRExpr* e, /* :: Ity_I32 */
                           IRTemp  guardT /* :: Ity_I32, 0 or 1 */)
{
   switch (gsoffset) {
      case OFFB_FPSCR: break;
      default: vassert(0); /* awaiting more cases */
   }
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I32);

   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      stmt(IRStmt_Put(gsoffset, e));
   } else {
      vassert(0); //ATC
      stmt(IRStmt_Put( gsoffset,
                       IRExpr_Mux0X( unop(Iop_32to8, mkexpr(guardT)),
                                     IRExpr_Get(gsoffset, Ity_I32),
                                     e) ));

   }
}


/* ---------------- FPSCR stuff ---------------- */

/* Generate IR to get hold of the rounding mode bits in FPSCR, and
   convert them to IR format.  Bind the final result to the
   returned temp. */
static IRTemp /* :: Ity_I32 */ mk_get_IR_rounding_mode ( void )
{
   /* The ARMvfp encoding for rounding mode bits is:
         00  to nearest
         01  to +infinity
         10  to -infinity
         11  to zero
      We need to convert that to the IR encoding:
         00  to nearest (the default)
         10  to +infinity
         01  to -infinity
         11  to zero
      Which can be done by swapping bits 0 and 1.
      The rmode bits are at 23:22 in FPSCR.
   */
   IRTemp armEncd = newTemp(Ity_I32);
   IRTemp swapped = newTemp(Ity_I32);
   /* Fish FPSCR[23:22] out, and slide to bottom.  Doesn't matter that
      we don't zero out bits 24 and above, since the assignment to
      'swapped' will mask them out anyway. */
   assign(armEncd,
          binop(Iop_Shr32, IRExpr_Get(OFFB_FPSCR, Ity_I32), mkU8(22)));
   /* Now swap them. */
   assign(swapped,
          binop(Iop_Or32,
                binop(Iop_And32,
                      binop(Iop_Shl32, mkexpr(armEncd), mkU8(1)),
                      mkU32(2)),
                binop(Iop_And32,
                      binop(Iop_Shr32, mkexpr(armEncd), mkU8(1)),
                      mkU32(1))
         ));
   return swapped;
}


/*------------------------------------------------------------*/
/*--- Helpers for flag handling and conditional insns      ---*/
/*------------------------------------------------------------*/

static HChar* name_ARMCondcode ( ARMCondcode cond )
{
   switch (cond) {
      case ARMCondEQ:  return "{eq}";
      case ARMCondNE:  return "{ne}";
      case ARMCondHS:  return "{hs}";  // or 'cs'
      case ARMCondLO:  return "{lo}";  // or 'cc'
      case ARMCondMI:  return "{mi}";
      case ARMCondPL:  return "{pl}";
      case ARMCondVS:  return "{vs}";
      case ARMCondVC:  return "{vc}";
      case ARMCondHI:  return "{hi}";
      case ARMCondLS:  return "{ls}";
      case ARMCondGE:  return "{ge}";
      case ARMCondLT:  return "{lt}";
      case ARMCondGT:  return "{gt}";
      case ARMCondLE:  return "{le}";
      case ARMCondAL:  return ""; // {al}: is the default
      case ARMCondNV:  return "{nv}";
      default: vpanic("name_ARMCondcode");
   }
}
/* and a handy shorthand for it */
static HChar* nCC ( ARMCondcode cond ) {
   return name_ARMCondcode(cond);
}


/* Build IR to calculate some particular condition from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.  Returns an expression of type
   Ity_I32, suitable for narrowing.  Although the return type is
   Ity_I32, the returned value is either 0 or 1.
*/
static IRExpr* mk_armg_calculate_condition ( ARMCondcode cond )
{
  /* First arg is "(cond << 4) | condition".  This requires that the
     ARM_CC_OP_ values all fit in 4 bits.  Hence we are passing a
     (COND, OP) pair in the lowest 8 bits of the first argument. */
   vassert(cond >= 0 && cond <= 15);
   IRExpr** args
      = mkIRExprVec_4(
           binop(Iop_Or32, IRExpr_Get(OFFB_CC_OP, Ity_I32),
                           mkU32(cond << 4)),
           IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
           IRExpr_Get(OFFB_CC_DEP2, Ity_I32),
           IRExpr_Get(OFFB_CC_NDEP, Ity_I32)
        );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "armg_calculate_condition", &armg_calculate_condition,
           args
        );

   /* Exclude the requested condition, OP and NDEP from definedness
      checking.  We're only interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<3);
   return call;
}


/* Build IR to calculate just the carry flag from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.  Returns an expression ::
   Ity_I32. */
static IRExpr* mk_armg_calculate_flag_c ( void )
{
   IRExpr** args
      = mkIRExprVec_4( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32),
                       IRExpr_Get(OFFB_CC_NDEP, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "armg_calculate_flag_c", &armg_calculate_flag_c,
           args
        );
   /* Exclude OP and NDEP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<3);
   return call;
}


/* Build IR to calculate just the overflow flag from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.  Returns an expression ::
   Ity_I32. */
static IRExpr* mk_armg_calculate_flag_v ( void )
{
   IRExpr** args
      = mkIRExprVec_4( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32),
                       IRExpr_Get(OFFB_CC_NDEP, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "armg_calculate_flag_v", &armg_calculate_flag_v,
           args
        );
   /* Exclude OP and NDEP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<3);
   return call;
}


/* Build IR to calculate N Z C V in bits 31:28 of the
   returned word. */
static IRExpr* mk_armg_calculate_flags_nzcv ( void )
{
   IRExpr** args
      = mkIRExprVec_4( IRExpr_Get(OFFB_CC_OP,   Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                       IRExpr_Get(OFFB_CC_DEP2, Ity_I32),
                       IRExpr_Get(OFFB_CC_NDEP, Ity_I32) );
   IRExpr* call
      = mkIRExprCCall(
           Ity_I32,
           0/*regparm*/, 
           "armg_calculate_flags_nzcv", &armg_calculate_flags_nzcv,
           args
        );
   /* Exclude OP and NDEP from definedness checking.  We're only
      interested in DEP1 and DEP2. */
   call->Iex.CCall.cee->mcx_mask = (1<<0) | (1<<3);
   return call;
}


/* Build IR to conditionally set the flags thunk.  As with putIReg, if
   guard is IRTemp_INVALID then it's unconditional, else it holds a
   condition :: Ity_I32. */
static
void setFlags_D1_D2_ND ( UInt cc_op, IRTemp t_dep1,
                         IRTemp t_dep2, IRTemp t_ndep,
                         IRTemp guardT /* :: Ity_I32, 0 or 1 */ )
{
   IRTemp c8;
   vassert(typeOfIRTemp(irsb->tyenv, t_dep1 == Ity_I32));
   vassert(typeOfIRTemp(irsb->tyenv, t_dep2 == Ity_I32));
   vassert(typeOfIRTemp(irsb->tyenv, t_ndep == Ity_I32));
   vassert(cc_op >= ARMG_CC_OP_COPY && cc_op < ARMG_CC_OP_NUMBER);
   if (guardT == IRTemp_INVALID) {
      /* unconditional */
      stmt( IRStmt_Put( OFFB_CC_OP,   mkU32(cc_op) ));
      stmt( IRStmt_Put( OFFB_CC_DEP1, mkexpr(t_dep1) ));
      stmt( IRStmt_Put( OFFB_CC_DEP2, mkexpr(t_dep2) ));
      stmt( IRStmt_Put( OFFB_CC_NDEP, mkexpr(t_ndep) ));
   } else {
      /* conditional */
      c8 = newTemp(Ity_I8);
      assign( c8, unop(Iop_32to8, mkexpr(guardT)) );
      stmt( IRStmt_Put(
               OFFB_CC_OP,
               IRExpr_Mux0X( mkexpr(c8),
                             IRExpr_Get(OFFB_CC_OP, Ity_I32),
                             mkU32(cc_op) )));
      stmt( IRStmt_Put(
               OFFB_CC_DEP1,
               IRExpr_Mux0X( mkexpr(c8),
                             IRExpr_Get(OFFB_CC_DEP1, Ity_I32),
                             mkexpr(t_dep1) )));
      stmt( IRStmt_Put(
               OFFB_CC_DEP2,
               IRExpr_Mux0X( mkexpr(c8),
                             IRExpr_Get(OFFB_CC_DEP2, Ity_I32),
                             mkexpr(t_dep2) )));
      stmt( IRStmt_Put(
               OFFB_CC_NDEP,
               IRExpr_Mux0X( mkexpr(c8),
                             IRExpr_Get(OFFB_CC_NDEP, Ity_I32),
                             mkexpr(t_ndep) )));
   }
}


/* Minor variant of the above that sets NDEP to zero (if it
   sets it at all) */
static void setFlags_D1_D2 ( UInt cc_op, IRTemp t_dep1,
                             IRTemp t_dep2,
                             IRTemp guardT /* :: Ity_I32, 0 or 1 */ )
{
   IRTemp z32 = newTemp(Ity_I32);
   assign( z32, mkU32(0) );
   setFlags_D1_D2_ND( cc_op, t_dep1, t_dep2, z32, guardT );
}


/* Minor variant of the above that sets DEP2 to zero (if it
   sets it at all) */
static void setFlags_D1_ND ( UInt cc_op, IRTemp t_dep1,
                             IRTemp t_ndep,
                             IRTemp guardT /* :: Ity_I32, 0 or 1 */ )
{
   IRTemp z32 = newTemp(Ity_I32);
   assign( z32, mkU32(0) );
   setFlags_D1_D2_ND( cc_op, t_dep1, z32, t_ndep, guardT );
}


/* Minor variant of the above that sets DEP2 and NDEP to zero (if it
   sets them at all) */
static void setFlags_D1 ( UInt cc_op, IRTemp t_dep1,
                          IRTemp guardT /* :: Ity_I32, 0 or 1 */ )
{
   IRTemp z32 = newTemp(Ity_I32);
   assign( z32, mkU32(0) );
   setFlags_D1_D2_ND( cc_op, t_dep1, z32, z32, guardT );
}


/* Generate a side-exit to the next instruction, if the given guard
   expression :: Ity_I32 is 0 (note!  the side exit is taken if the
   condition is false!)  This is used to skip over conditional
   instructions which we can't generate straight-line code for, either
   because they are too complex or (more likely) they potentially
   generate exceptions.
*/
static void mk_skip_to_next_if_cond_is_false ( 
               IRTemp guardT /* :: Ity_I32, 0 or 1 */
            )
{
   vassert(guardT != IRTemp_INVALID);
   stmt( IRStmt_Exit(
            unop(Iop_Not1, unop(Iop_32to1, mkexpr(guardT))),
            Ijk_Boring,
            IRConst_U32(toUInt(guest_R15_curr_instr + 4))
       ));
}


/*------------------------------------------------------------*/
/*--- Larger helpers                                       ---*/
/*------------------------------------------------------------*/

/* Generate an expression corresponding to a shifter_operand, bind it
   to a temporary, and return that via *shop.  If shco is non-NULL,
   also compute a value for the shifter's carry out (in the LSB of a
   word), bind it to a temporary, and return that via *shco.

   If for some reason we can't come up with a shifter operand (missing
   case?  not really a shifter operand?) return False.
*/
static Bool mk_shifter_operand ( UInt insn_25, UInt insn_11_0,
                                 /*OUT*/IRTemp* shop,
                                 /*OUT*/IRTemp* shco,
                                 /*OUT*/HChar* buf )
{
   UInt insn_4 = (insn_11_0 >> 4) & 1;
   UInt insn_7 = (insn_11_0 >> 7) & 1;
   vassert(insn_25 <= 0x1);
   vassert(insn_11_0 <= 0xFFF);

   vassert(shop && *shop == IRTemp_INVALID);
   *shop = newTemp(Ity_I32);

   if (shco) {
      vassert(*shco == IRTemp_INVALID);
      *shco = newTemp(Ity_I32);
   }

   /* 32-bit immediate */

   if (insn_25 == 1) {
      /* immediate: (7:0) rotated right by 2 * (11:8) */
      UInt imm = (insn_11_0 >> 0) & 0xFF;
      UInt rot = 2 * ((insn_11_0 >> 8) & 0xF);
      vassert(rot <= 30);
      imm = ROR32(imm, rot);
      if (shco) {
         if (rot == 0) {
            assign( *shco, mk_armg_calculate_flag_c() );
         } else {
            assign( *shco, mkU32( (imm >> 31) & 1 ) );
         }
      }
      DIS(buf, "#0x%x", imm);
      assign( *shop, mkU32(imm) );
      return True;
   }

   /* Shift/rotate by immediate */

   if (insn_25 == 0 && insn_4 == 0) {
      /* Rm (3:0) shifted (6:5) by immediate (11:7) */
      UInt shift_amt = (insn_11_0 >> 7) & 0x1F;
      UInt rM        = (insn_11_0 >> 0) & 0xF;
      UInt how       = (insn_11_0 >> 5) & 3;
      /* how: 00 = Shl, 01 = Shr, 10 = Sar, 11 = Ror */
      IRTemp rMt = newTemp(Ity_I32);
      assign(rMt, getIReg(rM));

      vassert(shift_amt <= 31);

      switch (how) {

         case 0:
            if (shift_amt == 0) {
               if (shco) {
                  assign( *shco, mk_armg_calculate_flag_c() );
               }
               assign( *shop, mkexpr(rMt) );
               DIS(buf, "r%u", rM);
            } else {
               vassert(shift_amt >= 1 && shift_amt <= 31);
               if (shco) {
                  assign( *shco,
                          binop(Iop_And32,
                                binop(Iop_Shr32, mkexpr(rMt), 
                                                 mkU8(32 - shift_amt)),
                                mkU32(1)));
               }
               assign( *shop,
                       binop(Iop_Shl32, mkexpr(rMt), mkU8(shift_amt)) );
               DIS(buf, "r%u, LSL #%u", rM, shift_amt);
            }
            return True;
            /*NOTREACHED*/

         case 1:
            if (shift_amt == 0) {
               // conceptually a 32-bit shift, however:
               // shop = 0
               // shco = Rm[31]
               if (shco) {
                  assign( *shco,
                          binop(Iop_And32,
                                binop(Iop_Shr32, mkexpr(rMt), mkU8(31)), 
                                mkU32(1)));
               }
               assign( *shop, mkU32(0) );
               DIS(buf, "r%u, LSR #0(a.k.a. 32)", rM);
            } else {
               // shift in range 1..31
               // shop = Rm >>u shift_amt
               // shco = Rm[shift_amt - 1]
               vassert(shift_amt >= 1 && shift_amt <= 31);
               if (shco) {
                  assign( *shco,
                          binop(Iop_And32,
                                binop(Iop_Shr32, mkexpr(rMt), 
                                                 mkU8(shift_amt - 1)),
                                mkU32(1)));
               }
               assign( *shop,
                       binop(Iop_Shr32, mkexpr(rMt), mkU8(shift_amt)) );
               DIS(buf, "r%u, LSR #%u", rM, shift_amt);
            }
            return True;
            /*NOTREACHED*/

         case 2:
            if (shift_amt == 0) {
               // conceptually a 32-bit shift, however:
               // shop = Rm >>s 31
               // shco = Rm[31]
               if (shco) {
                  assign( *shco,
                          binop(Iop_And32,
                                binop(Iop_Shr32, mkexpr(rMt), mkU8(31)), 
                                mkU32(1)));
               }
               assign( *shop, binop(Iop_Sar32, mkexpr(rMt), mkU8(31)) );
               DIS(buf, "r%u, ASR #0(a.k.a. 32)", rM);
            } else {
               // shift in range 1..31
               // shop = Rm >>s shift_amt
               // shco = Rm[shift_amt - 1]
               vassert(shift_amt >= 1 && shift_amt <= 31);
               if (shco) {
                  assign( *shco,
                          binop(Iop_And32,
                                binop(Iop_Shr32, mkexpr(rMt), 
                                                 mkU8(shift_amt - 1)),
                                mkU32(1)));
               }
               assign( *shop,
                       binop(Iop_Sar32, mkexpr(rMt), mkU8(shift_amt)) );
               DIS(buf, "r%u, ASR #%u", rM, shift_amt);
            }
            return True;
            /*NOTREACHED*/

         case 3:
            if (shift_amt == 0) {
               IRTemp oldcT = newTemp(Ity_I32);
               // rotate right 1 bit through carry (?)
               // RRX -- described at ARM ARM A5-17
               // shop = (oldC << 31) | (Rm >>u 1)
               // shco = Rm[0]
               if (shco) {
                  assign( *shco,
                          binop(Iop_And32, mkexpr(rMt), mkU32(1)));
               }
               assign( oldcT, mk_armg_calculate_flag_c() );
               assign( *shop, 
                       binop(Iop_Or32,
                             binop(Iop_Shl32, mkexpr(oldcT), mkU8(31)),
                             binop(Iop_Shr32, mkexpr(rMt), mkU8(1))) );
               DIS(buf, "r%u, RRX", rM);
            } else {
               // rotate right in range 1..31
               // shop = Rm `ror` shift_amt
               // shco = Rm[shift_amt - 1]
               vassert(shift_amt >= 1 && shift_amt <= 31);
               if (shco) {
                  assign( *shco,
                          binop(Iop_And32,
                                binop(Iop_Shr32, mkexpr(rMt), 
                                                 mkU8(shift_amt - 1)),
                                mkU32(1)));
               }
               assign( *shop,
                       binop(Iop_Or32,
                             binop(Iop_Shr32, mkexpr(rMt), mkU8(shift_amt)),
                             binop(Iop_Shl32, mkexpr(rMt),
                                              mkU8(32-shift_amt))));
               DIS(buf, "r%u, ROR #%u", rM, shift_amt);
            }
            return True;
            /*NOTREACHED*/

         default:
            /*NOTREACHED*/
            vassert(0);
      }
   }

   /* Shift/rotate by register */
   if (insn_25 == 0 && insn_4 == 1) {
      /* Rm (3:0) shifted (6:5) by Rs (11:8) */
      UInt rM  = (insn_11_0 >> 0) & 0xF;
      UInt rS  = (insn_11_0 >> 8) & 0xF;
      UInt how = (insn_11_0 >> 5) & 3;
      /* how: 00 = Shl, 01 = Shr, 10 = Sar, 11 = Ror */
      IRTemp rMt = newTemp(Ity_I32);
      IRTemp rSt = newTemp(Ity_I32);

      if (insn_7 == 1)
         return False; /* not really a shifter operand */

      assign(rMt, getIReg(rM));
      assign(rSt, getIReg(rS));

      switch (how) {
         case 0: { /* LSL */
            // shift left in range 0 .. 255
            // amt = rS & 255
            // shop = amt < 32 ?  Rm << amt  : 0
            // shco = amt == 0     ? oldC  :
            //        amt in 1..32 ?  Rm[32-amt]  : 0
            IRTemp amtT = newTemp(Ity_I32);
            assign( amtT, binop(Iop_And32, mkexpr(rSt), mkU32(255)) );
            if (shco) {
               /* mux0X(amt == 0,
                        mux0X(amt < 32, 
                              0,
                              Rm[(32-amt) & 31])
                        oldC)
               */
               /* About the best you can do is pray that iropt is able
                  to nuke most or all of the following junk. */
               IRTemp oldC = newTemp(Ity_I32);
               assign(oldC, mk_armg_calculate_flag_c() );
               assign(
                  *shco,
                  IRExpr_Mux0X(
                     unop(Iop_1Uto8,
                          binop(Iop_CmpEQ32, mkexpr(amtT), mkU32(0))),
                     IRExpr_Mux0X(
                        unop(Iop_1Uto8,
                             binop(Iop_CmpLE32U, mkexpr(amtT), mkU32(32))),
                        mkU32(0),
                        binop(Iop_Shr32,
                              mkexpr(rMt),
                              unop(Iop_32to8,
                                   binop(Iop_And32,
                                         binop(Iop_Sub32,
                                               mkU32(32),
                                               mkexpr(amtT)),
                                         mkU32(31)
                                   )
                              )
                        )
                     ),
                     mkexpr(oldC)
                  )
               );
            }
            // (Rm << (Rs & 31))  &  (((Rs & 255) - 32) >>s 31)
            // Lhs of the & limits the shift to 31 bits, so as to
            // give known IR semantics.  Rhs of the & is all 1s for
            // Rs <= 31 and all 0s for Rs >= 32.
            assign(
               *shop,
               binop(
                  Iop_And32,
                  binop(Iop_Shl32,
                        mkexpr(rMt),
                        unop(Iop_32to8,
                             binop(Iop_And32, mkexpr(rSt), mkU32(31)))),
                  binop(Iop_Sar32,
                        binop(Iop_Sub32,
                              mkexpr(amtT),
                              mkU32(32)),
                        mkU8(31))));
             DIS(buf, "r%u, LSL r%u", rM, rS);
             return True;
         }
         case 1: { /* LSR */
            // shift right in range 0 .. 255
            // amt = rS & 255
            // shop = amt < 32 ?  Rm >>u amt  : 0
            // shco = amt == 0     ? oldC  :
            //        amt in 1..32 ?  Rm[amt-1]  : 0
            IRTemp amtT = newTemp(Ity_I32);
            assign( amtT, binop(Iop_And32, mkexpr(rSt), mkU32(255)) );
            if (shco) {
               /* mux0X(amt == 0,
                        mux0X(amt < 32, 
                              0,
                              Rm[(amt-1) & 31])
                        oldC)
               */
               IRTemp oldC = newTemp(Ity_I32);
               assign(oldC, mk_armg_calculate_flag_c() );
               assign(
                  *shco,
                  IRExpr_Mux0X(
                     unop(Iop_1Uto8,
                          binop(Iop_CmpEQ32, mkexpr(amtT), mkU32(0))),
                     IRExpr_Mux0X(
                        unop(Iop_1Uto8,
                             binop(Iop_CmpLE32U, mkexpr(amtT), mkU32(32))),
                        mkU32(0),
                        binop(Iop_Shr32,
                              mkexpr(rMt),
                              unop(Iop_32to8,
                                   binop(Iop_And32,
                                         binop(Iop_Sub32,
                                               mkexpr(amtT),
                                               mkU32(1)),
                                         mkU32(31)
                                   )
                              )
                        )
                     ),
                     mkexpr(oldC)
                  )
               );
            }
            // (Rm >>u (Rs & 31))  &  (((Rs & 255) - 32) >>s 31)
            // Lhs of the & limits the shift to 31 bits, so as to
            // give known IR semantics.  Rhs of the & is all 1s for
            // Rs <= 31 and all 0s for Rs >= 32.
            assign(
               *shop,
               binop(
                  Iop_And32,
                  binop(Iop_Shr32,
                        mkexpr(rMt),
                        unop(Iop_32to8,
                             binop(Iop_And32, mkexpr(rSt), mkU32(31)))),
                  binop(Iop_Sar32,
                        binop(Iop_Sub32,
                              mkexpr(amtT),
                              mkU32(32)),
                        mkU8(31))));
             DIS(buf, "r%u, LSR r%u", rM, rS);
             return True;
         }
         case 2: { /* ASR */
            // arithmetic shift right in range 0 .. 255
            // amt = rS & 255
            // shop = amt < 32 ?  Rm >>s amt  : Rm >>s 31
            // shco = amt == 0     ? oldC  :
            //        amt in 1..32 ?  Rm[amt-1]  : Rm[31]
            IRTemp amtT = newTemp(Ity_I32);
            assign( amtT, binop(Iop_And32, mkexpr(rSt), mkU32(255)) );
            if (shco) {
               /* mux0X(amt == 0,
                        mux0X(amt < 32, 
                              Rm[31],
                              Rm[(amt-1) & 31])
                        oldC)
               */
               IRTemp oldC = newTemp(Ity_I32);
               assign(oldC, mk_armg_calculate_flag_c() );
               assign(
                  *shco,
                  IRExpr_Mux0X(
                     unop(Iop_1Uto8,
                          binop(Iop_CmpEQ32, mkexpr(amtT), mkU32(0))),
                     IRExpr_Mux0X(
                        unop(Iop_1Uto8,
                             binop(Iop_CmpLE32U, mkexpr(amtT), mkU32(32))),
                        binop(Iop_Shr32,
                              mkexpr(rMt),
                              mkU8(31)
                        ),
                        binop(Iop_Shr32,
                              mkexpr(rMt),
                              unop(Iop_32to8,
                                   binop(Iop_And32,
                                         binop(Iop_Sub32,
                                               mkexpr(amtT),
                                               mkU32(1)),
                                         mkU32(31)
                                   )
                              )
                        )
                     ),
                     mkexpr(oldC)
                  )
               );
            }
            // (Rm >>s (amt <u 32 ? amt : 31))
            assign(
               *shop,
               binop(
                  Iop_Sar32,
                  mkexpr(rMt),
                  unop(
                     Iop_32to8,
                     IRExpr_Mux0X(
                        unop(
                          Iop_1Uto8,
                          binop(Iop_CmpLT32U, mkexpr(amtT), mkU32(32))),
                        mkU32(31),
                        mkexpr(amtT)))));
             DIS(buf, "r%u, ASR r%u", rM, rS);
             return True;
         }
         case 3: { /* ROR */
            // rotate right in range 0 .. 255
            // amt = rS & 255
            // shop =  Rm `ror` (amt & 31)
            // shco =  amt == 0 ? oldC : Rm[(amt-1) & 31]
            IRTemp amtT = newTemp(Ity_I32);
            assign( amtT, binop(Iop_And32, mkexpr(rSt), mkU32(255)) );
            IRTemp amt5T = newTemp(Ity_I32);
            assign( amt5T, binop(Iop_And32, mkexpr(rSt), mkU32(31)) );
            IRTemp oldC = newTemp(Ity_I32);
            assign(oldC, mk_armg_calculate_flag_c() );
            if (shco) {
               assign(
                  *shco,
                  IRExpr_Mux0X(
                     unop(Iop_32to8, mkexpr(amtT)),
                     mkexpr(oldC),
                     binop(Iop_And32,
                           binop(Iop_Shr32,
                                 mkexpr(rMt), 
                                 unop(Iop_32to8,
                                      binop(Iop_And32,
                                            binop(Iop_Sub32,
                                                  mkexpr(amtT), 
                                                  mkU32(1)
                                            ),
                                            mkU32(31)
                                      )
                                 )
                           ),
                           mkU32(1)
                     )
                  )
               );
            }
            assign(
               *shop,
               IRExpr_Mux0X(
                  unop(Iop_32to8, mkexpr(amt5T)), mkexpr(rMt),
                  binop(Iop_Or32,
                        binop(Iop_Shr32,
                              mkexpr(rMt), 
                              unop(Iop_32to8, mkexpr(amt5T))
                        ),
                        binop(Iop_Shl32,
                              mkexpr(rMt),
                              unop(Iop_32to8,
                                   binop(Iop_Sub32, mkU32(32), mkexpr(amt5T))
                              )
                        )
                  )
               )
            );
            DIS(buf, "r%u, ROR r#%u", rM, rS);
            return True;
            /*NOTREACHED*/
         }
         default:
            /*NOTREACHED*/
            vassert(0);
      }
   }

   vex_printf("mk_shifter_operand(0x%x,0x%x)\n", insn_25, insn_11_0 );
   return False;
}


static 
IRExpr* mk_EA_reg_plusminus_imm12 ( UInt rN, UInt bU, UInt imm12,
                                    /*OUT*/HChar* buf )
{
   vassert(rN < 16);
   vassert(bU < 2);
   vassert(imm12 < 0x1000);
   UChar opChar = bU == 1 ? '+' : '-';
   DIS(buf, "[r%u, #%c%u]", rN, opChar, imm12);
   return
      binop( (bU == 1 ? Iop_Add32 : Iop_Sub32),
             getIReg(rN),
             mkU32(imm12) );
}


/* NB: This is "DecodeImmShift" in newer versions of the the ARM ARM.
*/
static
IRExpr* mk_EA_reg_plusminus_shifted_reg ( UInt rN, UInt bU, UInt rM,
                                          UInt sh2, UInt imm5,
                                          /*OUT*/HChar* buf )
{
   vassert(rN < 16);
   vassert(bU < 2);
   vassert(rM < 16);
   vassert(sh2 < 4);
   vassert(imm5 < 32);
   UChar   opChar = bU == 1 ? '+' : '-';
   IRExpr* index  = NULL;
   switch (sh2) {
      case 0: /* LSL */
         /* imm5 can be in the range 0 .. 31 inclusive. */
         index = binop(Iop_Shl32, getIReg(rM), mkU8(imm5));
         DIS(buf, "[r%u, %c r%u LSL #%u]", rN, opChar, rM, imm5); 
         break;
      case 1: /* LSR */
         if (imm5 == 0) {
            index = mkU32(0);
            vassert(0); // ATC
         } else {
            index = binop(Iop_Shr32, getIReg(rM), mkU8(imm5));
         }
         DIS(buf, "[r%u, %cr%u, LSR #%u]",
                  rN, opChar, rM, imm5 == 0 ? 32 : imm5); 
         break;
      case 2: /* ASR */
         /* Doesn't this just mean that the behaviour with imm5 == 0
            is the same as if it had been 31 ? */
         if (imm5 == 0) {
            index = binop(Iop_Sar32, getIReg(rM), mkU8(31));
            vassert(0); // ATC
         } else {
            index = binop(Iop_Sar32, getIReg(rM), mkU8(imm5));
         }
         DIS(buf, "[r%u, %cr%u, ASR #%u]",
                  rN, opChar, rM, imm5 == 0 ? 32 : imm5); 
         break;
      case 3: /* ROR or RRX */
         if (imm5 == 0) {
            IRTemp rmT    = newTemp(Ity_I32);
            IRTemp cflagT = newTemp(Ity_I32);
            assign(rmT, getIReg(rM));
            assign(cflagT, mk_armg_calculate_flag_c());
            index = binop(Iop_Or32, 
                          binop(Iop_Shl32, mkexpr(cflagT), mkU8(31)),
                          binop(Iop_Shr32, mkexpr(rmT), mkU8(1)));
            DIS(buf, "[r%u, %cr%u, RRX]", rN, opChar, rM);
         } else {
            IRTemp rmT = newTemp(Ity_I32);
            assign(rmT, getIReg(rM));
            vassert(imm5 >= 1 && imm5 <= 31);
            index = binop(Iop_Or32, 
                          binop(Iop_Shl32, mkexpr(rmT), mkU8(32-imm5)),
                          binop(Iop_Shr32, mkexpr(rmT), mkU8(imm5)));
            DIS(buf, "[r%u, %cr%u, ROR #%u]", rN, opChar, rM, imm5); 
         }
         break;
      default:
         vassert(0);
   }
   vassert(index);
   return binop(bU == 1 ? Iop_Add32 : Iop_Sub32,
                getIReg(rN), index);
}


static 
IRExpr* mk_EA_reg_plusminus_imm8 ( UInt rN, UInt bU, UInt imm8,
                                   /*OUT*/HChar* buf )
{
   vassert(rN < 16);
   vassert(bU < 2);
   vassert(imm8 < 0x100);
   UChar opChar = bU == 1 ? '+' : '-';
   DIS(buf, "[r%u, #%c%u]", rN, opChar, imm8);
   return
      binop( (bU == 1 ? Iop_Add32 : Iop_Sub32),
             getIReg(rN),
             mkU32(imm8) );
}


static
IRExpr* mk_EA_reg_plusminus_reg ( UInt rN, UInt bU, UInt rM,
                                  /*OUT*/HChar* buf )
{
   vassert(rN < 16);
   vassert(bU < 2);
   vassert(rM < 16);
   UChar   opChar = bU == 1 ? '+' : '-';
   IRExpr* index  = getIReg(rM);
   DIS(buf, "[r%u, %c r%u]", rN, opChar, rM); 
   return binop(bU == 1 ? Iop_Add32 : Iop_Sub32,
                getIReg(rN), index);
}


/* irRes :: Ity_I32 holds a floating point comparison result encoded
   as an IRCmpF64Result.  Generate code to convert it to an
   ARM-encoded (N,Z,C,V) group in the lowest 4 bits of an I32 value.
   Assign a new temp to hold that value, and return the temp. */
static
IRTemp mk_convert_IRCmpF64Result_to_NZCV ( IRTemp irRes )
{
   IRTemp ix       = newTemp(Ity_I32);
   IRTemp termL    = newTemp(Ity_I32);
   IRTemp termR    = newTemp(Ity_I32);
   IRTemp nzcv     = newTemp(Ity_I32);

   /* This is where the fun starts.  We have to convert 'irRes' from
      an IR-convention return result (IRCmpF64Result) to an
      ARM-encoded (N,Z,C,V) group.  The final result is in the bottom
      4 bits of 'nzcv'. */
   /* Map compare result from IR to ARM(nzcv) */
   /*
      FP cmp result | IR   | ARM(nzcv)
      --------------------------------
      UN              0x45   0011
      LT              0x01   1000
      GT              0x00   0010
      EQ              0x40   0110
   */
   /* Now since you're probably wondering WTF ..

      ix fishes the useful bits out of the IR value, bits 6 and 0, and
      places them side by side, giving a number which is 0, 1, 2 or 3.

      termL is a sequence cooked up by GNU superopt.  It converts ix
         into an almost correct value NZCV value (incredibly), except
         for the case of UN, where it produces 0100 instead of the
         required 0011.

      termR is therefore a correction term, also computed from ix.  It
         is 1 in the UN case and 0 for LT, GT and UN.  Hence, to get
         the final correct value, we subtract termR from termL.

      Don't take my word for it.  There's a test program at the bottom
      of this file, to try this out with.
   */
   assign(
      ix,
      binop(Iop_Or32,
            binop(Iop_And32,
                  binop(Iop_Shr32, mkexpr(irRes), mkU8(5)),
                  mkU32(3)),
            binop(Iop_And32, mkexpr(irRes), mkU32(1))));

   assign(
      termL,
      binop(Iop_Add32,
            binop(Iop_Shr32,
                  binop(Iop_Sub32,
                        binop(Iop_Shl32,
                              binop(Iop_Xor32, mkexpr(ix), mkU32(1)),
                              mkU8(30)),
                        mkU32(1)),
                  mkU8(29)),
            mkU32(1)));

   assign(
      termR,
      binop(Iop_And32,
            binop(Iop_And32,
                  mkexpr(ix),
                  binop(Iop_Shr32, mkexpr(ix), mkU8(1))),
            mkU32(1)));

   assign(nzcv, binop(Iop_Sub32, mkexpr(termL), mkexpr(termR)));
   return nzcv;
}


/*------------------------------------------------------------*/
/*--- Instructions in NV (never) space                     ---*/
/*------------------------------------------------------------*/

static Bool decode_NV_instruction ( UInt insn )
{
#  define INSN(_bMax,_bMin)  SLICE_UInt(insn, (_bMax), (_bMin))
#  define INSN_COND          SLICE_UInt(insn, 31, 28)

   HChar dis_buf[128];

   // Should only be called for NV instructions
   vassert(BITS4(1,1,1,1) == INSN_COND);

   /* ------------------------ pld ------------------------ */
   if (BITS8(0,1,0,1, 0, 1,0,1) == (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1))
       && BITS4(1,1,1,1) == INSN(15,12)) {
      UInt rN    = INSN(19,16);
      UInt imm12 = INSN(11,0);
      UInt bU    = INSN(23,23);
      DIP("pld [r%u, #%c%u]\n", rN, bU ? '+' : '-', imm12);
      return True;
   }

   if (BITS8(0,1,1,1, 0, 1,0,1) == (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1))
       && BITS4(1,1,1,1) == INSN(15,12)
       && 0 == INSN(4,4)) {
      UInt rN   = INSN(19,16);
      UInt rM   = INSN(3,0);
      UInt imm5 = INSN(11,7);
      UInt sh2  = INSN(6,5);
      UInt bU   = INSN(23,23);
      if (rM != 15) {
         IRExpr* eaE = mk_EA_reg_plusminus_shifted_reg(rN, bU, rM,
                                                       sh2, imm5, dis_buf);
         IRTemp eaT = newTemp(Ity_I32);
         /* Bind eaE to a temp merely for debugging-vex purposes, so we
            can check it's a plausible decoding.  It will get removed
            by iropt a little later on. */
         vassert(eaE);
         assign(eaT, eaE);
         DIP("pld %s\n", dis_buf);
         return True;
      }
      /* fall through */
   }

   /* ------------------- v7 barrier insns ------------------- */
   switch (insn) {
      case 0xF57FF06F: /* ISB */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("ISB\n");
         return True;
      case 0xF57FF04F: /* DSB */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("DSB\n");
         return True;
      case 0xF57FF05F: /* DMB */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("DMB\n");
         return True;
      default:
         break;
   }

   return False;

#  undef INSN_COND
#  undef INSN
}


/*------------------------------------------------------------*/
/*--- Disassemble a single instruction                     ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction is
   located in host memory at guest_instr, and has guest IP of
   guest_R15_curr_instr, which will have been set before the call
   here. */

static   
DisResult disInstr_ARM_WRK (
             Bool         put_IP,
             Bool         (*resteerOkFn) ( /*opaque*/void*, Addr64 ),
             Bool         resteerCisOk,
             void*        callback_opaque,
             UChar*       guest_instr,
             VexArchInfo* archinfo,
             VexAbiInfo*  abiinfo
          )
{
   // A macro to fish bits out of 'insn'.
#  define INSN(_bMax,_bMin)  SLICE_UInt(insn, (_bMax), (_bMin))
#  define INSN_COND          SLICE_UInt(insn, 31, 28)

   DisResult dres;
   UInt      insn;
   //Bool      allow_VFP = False;
   //UInt      hwcaps = archinfo->hwcaps;
   IRTemp    condT; /* :: Ity_I32 */
   UInt      summary;
   HChar     dis_buf[128];  // big enough to hold LDMIA etc text

   /* What insn variants are we supporting today? */
   //allow_VFP  = (0 != (hwcaps & VEX_HWCAPS_ARM_VFP));
   // etc etc

   /* Set result defaults. */
   dres.whatNext   = Dis_Continue;
   dres.len        = 4;
   dres.continueAt = 0;

   /* Set default actions for post-insn handling of writes to r15, if
      required. */
   r15written = False;
   r15guard   = IRTemp_INVALID; /* unconditional */
   r15kind    = Ijk_Boring;

   /* At least this is simple on ARM: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */
   insn = getUIntLittleEndianly( guest_instr );

   if (0) vex_printf("insn: 0x%x\n", insn);

   DIP("\t0x%x:  ", (UInt)guest_R15_curr_instr);

   /* We may be asked to update the guest R15 before going further. */
   if (put_IP) {
      vassert(0 == (guest_R15_curr_instr & 3));
      llPutIReg( 15, mkU32(guest_R15_curr_instr) );
   }

   /* ----------------------------------------------------------- */

   /* Spot "Special" instructions (see comment at top of file). */
   {
      UChar* code = (UChar*)guest_instr;
      /* Spot the 16-byte preamble: 

         e1a0c1ec  mov r12, r12, ROR #3
         e1a0c6ec  mov r12, r12, ROR #13
         e1a0ceec  mov r12, r12, ROR #29
         e1a0c9ec  mov r12, r12, ROR #19
      */
      UInt word1 = 0xE1A0C1EC;
      UInt word2 = 0xE1A0C6EC;
      UInt word3 = 0xE1A0CEEC;
      UInt word4 = 0xE1A0C9EC;
      if (getUIntLittleEndianly(code+ 0) == word1 &&
          getUIntLittleEndianly(code+ 4) == word2 &&
          getUIntLittleEndianly(code+ 8) == word3 &&
          getUIntLittleEndianly(code+12) == word4) {
         /* Got a "Special" instruction preamble.  Which one is it? */
         if (getUIntLittleEndianly(code+16) == 0xE18AA00A
                                               /* orr r10,r10,r10 */) {
            /* R3 = client_request ( R4 ) */
            DIP("r3 = client_request ( %%r4 )\n");
            irsb->next     = mkU32( guest_R15_curr_instr + 20 );
            irsb->jumpkind = Ijk_ClientReq;
            dres.whatNext  = Dis_StopHere;
            goto decode_success;
         }
         else
         if (getUIntLittleEndianly(code+16) == 0xE18BB00B
                                               /* orr r11,r11,r11 */) {
            /* R3 = guest_NRADDR */
            DIP("r3 = guest_NRADDR\n");
            dres.len = 20;
            llPutIReg(3, IRExpr_Get( OFFB_NRADDR, Ity_I32 ));
            goto decode_success;
         }
         else
         if (getUIntLittleEndianly(code+16) == 0xE18CC00C
                                               /* orr r12,r12,r12 */) {
            /*  branch-and-link-to-noredir R4 */
            DIP("branch-and-link-to-noredir r4\n");
            llPutIReg(14, mkU32( guest_R15_curr_instr + 20) );
            irsb->next     = llGetIReg(4);
            irsb->jumpkind = Ijk_NoRedir;
            dres.whatNext  = Dis_StopHere;
            goto decode_success;
         }
         /* We don't know what it is.  Set opc1/opc2 so decode_failure
            can print the insn following the Special-insn preamble. */
         insn = getUIntLittleEndianly(code+16);
         goto decode_failure;
         /*NOTREACHED*/
      }

   }

   /* ----------------------------------------------------------- */

   /* Main instruction decoder starts here. */

   /* Deal with the condition.  Strategy is to merely generate a
      condition expression at this point (or NULL, meaning
      unconditional).  We leave it to lower-level instruction decoders
      to decide whether they can generate straight-line code, or
      whether they must generate a side exit before the instruction.
      condT :: Ity_I32 and is always either zero or one. */
   condT = IRTemp_INVALID;
   switch ( (ARMCondcode)INSN_COND ) {
      case ARMCondNV: {
         // Illegal instruction prior to v5 (see ARM ARM A3-5), but
         // some cases are acceptable
         Bool ok = decode_NV_instruction(insn);
         if (ok)
            goto decode_success;
         else
            goto decode_failure;
      }
      case ARMCondAL: // Always executed
         break;
      case ARMCondEQ: case ARMCondNE: case ARMCondHS: case ARMCondLO:
      case ARMCondMI: case ARMCondPL: case ARMCondVS: case ARMCondVC:
      case ARMCondHI: case ARMCondLS: case ARMCondGE: case ARMCondLT:
      case ARMCondGT: case ARMCondLE:
         condT = newTemp(Ity_I32);
         assign( condT, mk_armg_calculate_condition( INSN_COND ));
         break;
   }

   /* ----------------------------------------------------------- */
   /* -- ARMv5 integer instructions                            -- */
   /* ----------------------------------------------------------- */

   /* ---------------- Data processing ops ------------------- */

   if (0 == (INSN(27,20) & BITS8(1,1,0,0,0,0,0,0))
       && !(INSN(25,25) == 0 && INSN(7,7) == 1 && INSN(4,4) == 1)) {
      IRTemp  shop = IRTemp_INVALID; /* shifter operand */
      IRTemp  shco = IRTemp_INVALID; /* shifter carry out */
      UInt    rD   = (insn >> 12) & 0xF; /* 15:12 */
      UInt    rN   = (insn >> 16) & 0xF; /* 19:16 */
      UInt    bitS = (insn >> 20) & 1; /* 20:20 */
      IRTemp  rNt  = IRTemp_INVALID;
      IRTemp  res  = IRTemp_INVALID;
      IRTemp  oldV = IRTemp_INVALID;
      IRTemp  oldC = IRTemp_INVALID;
      HChar*  name = NULL;
      IROp    op   = Iop_INVALID;
      Bool    ok;

      switch (INSN(24,21)) {

         /* --------- ADD, SUB, AND, OR --------- */
         case BITS4(0,1,0,0): /* ADD:  Rd = Rn + shifter_operand */
            name = "add"; op = Iop_Add32; goto rd_eq_rn_op_SO;
         case BITS4(0,0,1,0): /* SUB:  Rd = Rn - shifter_operand */
            name = "sub"; op = Iop_Sub32; goto rd_eq_rn_op_SO;
         case BITS4(0,0,1,1): /* RSB:  Rd = shifter_operand - Rn */
            name = "rsb"; op = Iop_Sub32; goto rd_eq_rn_op_SO;
         case BITS4(0,0,0,0): /* AND:  Rd = Rn & shifter_operand */
            name = "and"; op = Iop_And32; goto rd_eq_rn_op_SO;
         case BITS4(1,1,0,0): /* OR:   Rd = Rn | shifter_operand */
            name = "orr"; op = Iop_Or32; goto rd_eq_rn_op_SO;
         case BITS4(0,0,0,1): /* EOR:  Rd = Rn ^ shifter_operand */
            name = "eor"; op = Iop_Xor32; goto rd_eq_rn_op_SO;
         case BITS4(1,1,1,0): /* BIC:  Rd = Rn & ~shifter_operand */
            name = "bic"; op = Iop_And32; goto rd_eq_rn_op_SO;
         rd_eq_rn_op_SO: {
            Bool isRSB = False;
            Bool isBIC = False;
            switch (INSN(24,21)) {
               case BITS4(0,0,1,1):
                  vassert(op == Iop_Sub32); isRSB = True; break;
               case BITS4(1,1,1,0):
                  vassert(op == Iop_And32); isBIC = True; break;
               default:
                  break;
            }
            rNt = newTemp(Ity_I32);
            assign(rNt, getIReg(rN));
            ok = mk_shifter_operand(
                    INSN(25,25), INSN(11,0), 
                    &shop, bitS ? &shco : NULL, dis_buf
                 );
            if (!ok)
               break;
            res = newTemp(Ity_I32);
            // compute the main result
            if (isRSB) {
               // reverse-subtract: shifter_operand - Rn
               vassert(op == Iop_Sub32);
               assign(res, binop(op, mkexpr(shop), mkexpr(rNt)) );
            } else if (isBIC) {
               // andn: shifter_operand & ~Rn
               vassert(op == Iop_And32);
               assign(res, binop(op, mkexpr(rNt),
                                     unop(Iop_Not32, mkexpr(shop))) );
            } else {
               // normal: Rn op shifter_operand
               assign(res, binop(op, mkexpr(rNt), mkexpr(shop)) );
            }
            // but don't commit it until after we've finished
            // all necessary reads from the guest state
            if (bitS
                && (op == Iop_And32 || op == Iop_Or32 || op == Iop_Xor32)) {
               oldV = newTemp(Ity_I32);
               assign( oldV, mk_armg_calculate_flag_v() );
            }
            // now safe to put the main result
            putIReg( rD, mkexpr(res), condT, Ijk_Boring );
            // XXXX!! not safe to read any guest state after
            // this point (I think the code below doesn't do that).
            if (!bitS)
               vassert(shco == IRTemp_INVALID);
            /* Update the flags thunk if necessary */
            if (bitS) {
               vassert(shco != IRTemp_INVALID);
               switch (op) {
                  case Iop_Add32:
                     setFlags_D1_D2( ARMG_CC_OP_ADD, rNt, shop, condT );
                     break;
                  case Iop_Sub32:
                     if (isRSB) {
                        setFlags_D1_D2( ARMG_CC_OP_SUB, shop, rNt, condT );
                     } else {
                        setFlags_D1_D2( ARMG_CC_OP_SUB, rNt, shop, condT );
                     }
                     break;
                  case Iop_And32: /* BIC and AND set the flags the same */
                  case Iop_Or32:
                  case Iop_Xor32:
                     // oldV has been read just above
                     setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC,
                                        res, shco, oldV, condT );
                     break;
                  default:
                     vassert(0);
               }
            }
            DIP("%s%s%s r%u, r%u, %s\n",
                name, nCC(INSN_COND), bitS ? "s" : "", rD, rN, dis_buf );
            goto decode_success;
         }

         /* --------- MOV, MVN --------- */
         case BITS4(1,1,0,1):   /* MOV: Rd = shifter_operand */
         case BITS4(1,1,1,1): { /* MVN: Rd = not(shifter_operand) */
            Bool isMVN = INSN(24,21) == BITS4(1,1,1,1);
            if (rN != 0)
               break; /* rN must be zero */
            ok = mk_shifter_operand(
                    INSN(25,25), INSN(11,0), 
                    &shop, bitS ? &shco : NULL, dis_buf
                 );
            if (!ok)
               break;
            res = newTemp(Ity_I32);
            assign( res, isMVN ? unop(Iop_Not32, mkexpr(shop))
                               : mkexpr(shop) );
            if (bitS) {
               vassert(shco != IRTemp_INVALID);
               oldV = newTemp(Ity_I32);
               assign( oldV, mk_armg_calculate_flag_v() );
            } else {
               vassert(shco == IRTemp_INVALID);
            }
            // can't safely read guest state after here
            putIReg( rD, mkexpr(res), condT, Ijk_Boring );
            /* Update the flags thunk if necessary */
            if (bitS) {
               setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, 
                                  res, shco, oldV, condT );
            }
            DIP("%s%s%s r%u, %s\n",
                isMVN ? "mvn" : "mov",
                nCC(INSN_COND), bitS ? "s" : "", rD, dis_buf );
            goto decode_success;
         }

         /* --------- CMP --------- */
         case BITS4(1,0,1,0):   /* CMP:  (void) Rn - shifter_operand */
         case BITS4(1,0,1,1): { /* CMN:  (void) Rn + shifter_operand */
            Bool isCMN = INSN(24,21) == BITS4(1,0,1,1);
            if (rD != 0)
               break; /* rD must be zero */
            if (bitS == 0)
               break; /* if S (bit 20) is not set, it's not CMP/CMN */
            rNt = newTemp(Ity_I32);
            assign(rNt, getIReg(rN));
            ok = mk_shifter_operand(
                    INSN(25,25), INSN(11,0), 
                    &shop, NULL, dis_buf
                 );
            if (!ok)
               break;
            /* Update the flags thunk. */
            setFlags_D1_D2( isCMN ? ARMG_CC_OP_ADD : ARMG_CC_OP_SUB,
                            rNt, shop, condT );
            DIP("%s%s r%u, %s\n",
                isCMN ? "cmn" : "cmp",
                nCC(INSN_COND), rN, dis_buf );
            goto decode_success;
         }

         /* --------- TST --------- */
         case BITS4(1,0,0,0):   /* TST:  (void) Rn & shifter_operand */
         case BITS4(1,0,0,1): { /* TEQ:  (void) Rn ^ shifter_operand */
            Bool isTEQ = INSN(24,21) == BITS4(1,0,0,1);
            if (rD != 0)
               break; /* rD must be zero */
            if (bitS == 0)
               break; /* if S (bit 20) is not set, it's not TST/TEQ */
            rNt = newTemp(Ity_I32);
            assign(rNt, getIReg(rN));
            ok = mk_shifter_operand(
                    INSN(25,25), INSN(11,0), 
                    &shop, &shco, dis_buf
                 );
            if (!ok)
               break;
            /* Update the flags thunk. */
            res = newTemp(Ity_I32);
            assign( res, binop(isTEQ ? Iop_Xor32 : Iop_And32, 
                               mkexpr(rNt), mkexpr(shop)) );
            oldV = newTemp(Ity_I32);
            assign( oldV, mk_armg_calculate_flag_v() );
            setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC,
                               res, shco, oldV, condT );
            DIP("%s%s r%u, %s\n",
                isTEQ ? "teq" : "tst",
                nCC(INSN_COND), rN, dis_buf );
            goto decode_success;
         }

         /* --------- ADC, SBC, RSC --------- */
         case BITS4(0,1,0,1): /* ADC:  Rd = Rn + shifter_operand + oldC */
            name = "adc"; goto rd_eq_rn_op_SO_op_oldC;
         case BITS4(0,1,1,0): /* SBC:  Rd = Rn - shifter_operand - (oldC ^ 1) */
            name = "sbc"; goto rd_eq_rn_op_SO_op_oldC;
         case BITS4(0,1,1,1): /* RSC:  Rd = shifter_operand - Rn - (oldC ^ 1) */
            name = "rsc"; goto rd_eq_rn_op_SO_op_oldC;
         rd_eq_rn_op_SO_op_oldC: {
            rNt = newTemp(Ity_I32);
            assign(rNt, getIReg(rN));
            ok = mk_shifter_operand(
                    INSN(25,25), INSN(11,0), 
                    &shop, bitS ? &shco : NULL, dis_buf
                 );
            if (!ok)
               break;
            oldC = newTemp(Ity_I32);
            assign( oldC, mk_armg_calculate_flag_c() );
            res = newTemp(Ity_I32);
            // compute the main result
            switch (INSN(24,21)) {
               case BITS4(0,1,0,1): /* ADC */
                  assign(res,
                         binop(Iop_Add32,
                               binop(Iop_Add32, mkexpr(rNt), mkexpr(shop)),
                               mkexpr(oldC) ));
                  break;
               case BITS4(0,1,1,0): /* SBC */
                  assign(res,
                         binop(Iop_Sub32,
                               binop(Iop_Sub32, mkexpr(rNt), mkexpr(shop)),
                               binop(Iop_Xor32, mkexpr(oldC), mkU32(1)) ));
                  break;
               case BITS4(0,1,1,1): /* RSC */
                  assign(res,
                         binop(Iop_Sub32,
                               binop(Iop_Sub32, mkexpr(shop), mkexpr(rNt)),
                               binop(Iop_Xor32, mkexpr(oldC), mkU32(1)) ));
                  break;
               default:
                  vassert(0);
            }
            // but don't commit it until after we've finished
            // all necessary reads from the guest state
            // now safe to put the main result
            putIReg( rD, mkexpr(res), condT, Ijk_Boring );
            // XXXX!! not safe to read any guest state after
            // this point (I think the code below doesn't do that).
            if (!bitS)
               vassert(shco == IRTemp_INVALID);
            /* Update the flags thunk if necessary */
            if (bitS) {
               vassert(shco != IRTemp_INVALID);
               switch (INSN(24,21)) {
                  case BITS4(0,1,0,1): /* ADC */
                     setFlags_D1_D2_ND( ARMG_CC_OP_ADC,
                                        rNt, shop, oldC, condT );
                     break;
                  case BITS4(0,1,1,0): /* SBC */
                     setFlags_D1_D2_ND( ARMG_CC_OP_SBB,
                                        rNt, shop, oldC, condT );
                     break;
                  case BITS4(0,1,1,1): /* RSC */
                     setFlags_D1_D2_ND( ARMG_CC_OP_SBB,
                                        shop, rNt, oldC, condT );
                     break;
                  default:
                     vassert(0);
               }
            }
            DIP("%s%s%s r%u, r%u, %s\n",
                name, nCC(INSN_COND), bitS ? "s" : "", rD, rN, dis_buf );
            goto decode_success;
         }

         /* --------- ??? --------- */
         default:
            break;
      }
   } /* if (0 == (INSN(27,20) & BITS8(1,1,0,0,0,0,0,0)) */

   /* --------------------- Load/store (ubyte & word) -------- */
   // LDR STR LDRB STRB
   /*                 31   27   23   19 15 11    6   4 3  # highest bit
                        28   24   20 16 12
      A5-20   1 | 16  cond 0101 UB0L Rn Rd imm12
      A5-22   1 | 32  cond 0111 UBOL Rn Rd imm5  sh2 0 Rm
      A5-24   2 | 16  cond 0101 UB1L Rn Rd imm12
      A5-26   2 | 32  cond 0111 UB1L Rn Rd imm5  sh2 0 Rm
      A5-28   3 | 16  cond 0100 UB0L Rn Rd imm12
      A5-32   3 | 32  cond 0110 UB0L Rn Rd imm5  sh2 0 Rm
   */
   /* case coding:
             1   at-ea               (access at ea)
             2   at-ea-then-upd      (access at ea, then Rn = ea)
             3   at-Rn-then-upd      (access at Rn, then Rn = ea)
      ea coding
             16  Rn +/- imm12
             32  Rn +/- Rm sh2 imm5
   */
   /* Quickly skip over all of this for hopefully most instructions */
   if ((INSN(27,24) & BITS4(1,1,0,0)) != BITS4(0,1,0,0))
      goto after_load_store_ubyte_or_word;

   summary = 0;
   
   /**/ if (INSN(27,24) == BITS4(0,1,0,1) && INSN(21,21) == 0) {
      summary = 1 | 16;
   }
   else if (INSN(27,24) == BITS4(0,1,1,1) && INSN(21,21) == 0
                                          && INSN(4,4) == 0) {
      summary = 1 | 32;
   }
   else if (INSN(27,24) == BITS4(0,1,0,1) && INSN(21,21) == 1) {
      summary = 2 | 16;
   }
   else if (INSN(27,24) == BITS4(0,1,1,1) && INSN(21,21) == 1
                                          && INSN(4,4) == 0) {
      summary = 2 | 32;
   }
   else if (INSN(27,24) == BITS4(0,1,0,0) && INSN(21,21) == 0) {
      summary = 3 | 16;
   }
   else if (INSN(27,24) == BITS4(0,1,1,0) && INSN(21,21) == 0
                                          && INSN(4,4) == 0) {
      summary = 3 | 32;
   }
   else goto after_load_store_ubyte_or_word;

   { UInt rN = (insn >> 16) & 0xF; /* 19:16 */
     UInt rD = (insn >> 12) & 0xF; /* 15:12 */
     UInt rM = (insn >> 0)  & 0xF; /*  3:0  */
     UInt bU = (insn >> 23) & 1;      /* 23 */
     UInt bB = (insn >> 22) & 1;      /* 22 */
     UInt bL = (insn >> 20) & 1;      /* 20 */
     UInt imm12 = (insn >> 0) & 0xFFF; /* 11:0 */
     UInt imm5  = (insn >> 7) & 0x1F;  /* 11:7 */
     UInt sh2   = (insn >> 5) & 3;     /* 6:5 */

     /* Skip some invalid cases, which would lead to two competing
        updates to the same register, or which are otherwise
        disallowed by the spec. */
     switch (summary) {
        case 1 | 16:
           break;
        case 1 | 32: 
           if (rM == 15) goto after_load_store_ubyte_or_word;
           break;
        case 2 | 16: case 3 | 16:
           if (rN == 15) goto after_load_store_ubyte_or_word;
           if (bL == 1 && rN == rD) goto after_load_store_ubyte_or_word;
           break;
        case 2 | 32: case 3 | 32:
           if (rM == 15) goto after_load_store_ubyte_or_word;
           if (rN == 15) goto after_load_store_ubyte_or_word;
           if (rN == rM) goto after_load_store_ubyte_or_word;
           if (bL == 1 && rN == rD) goto after_load_store_ubyte_or_word;
           break;
        default:
           vassert(0);
     }

     /* Now, we can't do a conditional load or store, since that very
        likely will generate an exception.  So we have to take a side
        exit at this point if the condition is false. */
     if (condT != IRTemp_INVALID) {
        mk_skip_to_next_if_cond_is_false( condT );
        condT = IRTemp_INVALID;
     }
     /* Ok, now we're unconditional.  Do the load or store. */

     /* compute the effective address.  Bind it to a tmp since we
        may need to use it twice. */
     IRExpr* eaE = NULL;
     switch (summary & 0xF0) {
        case 16:
           eaE = mk_EA_reg_plusminus_imm12( rN, bU, imm12, dis_buf );
           break;
        case 32:
           eaE = mk_EA_reg_plusminus_shifted_reg( rN, bU, rM, sh2, imm5,
                                                  dis_buf );
           break;
     }
     vassert(eaE);
     IRTemp eaT = newTemp(Ity_I32);
     assign(eaT, eaE);

     /* get the old Rn value */
     IRTemp rnT = newTemp(Ity_I32);
     assign(rnT, getIReg(rN));

     /* decide on the transfer address */
     IRTemp taT = IRTemp_INVALID;
     switch (summary & 0x0F) {
        case 1: case 2: taT = eaT; break;
        case 3:         taT = rnT; break;
     }
     vassert(taT != IRTemp_INVALID);

     if (bL == 0) {
       /* Store.  If necessary, update the base register before the
          store itself, so that the common idiom of "str rX, [sp,
          #-4]!" (store rX at sp-4, then do new sp = sp-4, a.k.a "push
          rX") doesn't cause Memcheck to complain that the access is
          below the stack pointer.  Also, not updating sp before the
          store confuses Valgrind's dynamic stack-extending logic.  So
          do it before the store.  Hence we need to snarf the store
          data before doing the basereg update. */

        /* get hold of the data to be stored */
        IRTemp rDt = newTemp(Ity_I32);
        assign(rDt, getIReg(rD));

        /* Update Rn if necessary. */
        switch (summary & 0x0F) {
           case 2: case 3:
              putIReg( rN, mkexpr(eaT), IRTemp_INVALID, Ijk_Boring );
              break;
        }

        /* generate the transfer */
        if (bB == 0) { // word store
           storeLE( mkexpr(taT), mkexpr(rDt) );
        } else { // byte store
           vassert(bB == 1);
           storeLE( mkexpr(taT), unop(Iop_32to8, mkexpr(rDt)) );
        }

     } else {
        /* Load */
        vassert(bL == 1);

        /* generate the transfer */
        if (bB == 0) { // word load
           putIReg( rD, loadLE(Ity_I32, mkexpr(taT)),
                    IRTemp_INVALID, Ijk_Boring );
        } else { // byte load
          vassert(bB == 1);
           putIReg( rD, unop(Iop_8Uto32, loadLE(Ity_I8, mkexpr(taT))),
                    IRTemp_INVALID, Ijk_Boring );
        }

        /* Update Rn if necessary. */
        switch (summary & 0x0F) {
           case 2: case 3:
              // should be assured by logic above:
              if (bL == 1)
                 vassert(rD != rN); /* since we just wrote rD */
              putIReg( rN, mkexpr(eaT), IRTemp_INVALID, Ijk_Boring );
              break;
        }
     }
 
     switch (summary & 0x0F) {
        case 1:  DIP("%sr%s%s r%u, %s\n",
                     bL == 0 ? "st" : "ld",
                     bB == 0 ? "" : "b", nCC(INSN_COND), rD, dis_buf);
                 break;
        case 2:  DIP("%sr%s%s r%u, %s! (at-EA-then-Rn=EA)\n",
                     bL == 0 ? "st" : "ld",
                     bB == 0 ? "" : "b", nCC(INSN_COND), rD, dis_buf);
                 break;
        case 3:  DIP("%sr%s%s r%u, %s! (at-Rn-then-Rn=EA)\n",
                     bL == 0 ? "st" : "ld",
                     bB == 0 ? "" : "b", nCC(INSN_COND), rD, dis_buf);
                 break;
        default: vassert(0);
     }

     /* XXX deal with alignment constraints */

     goto decode_success;

     /* Complications:

        For all loads: if the Amode specifies base register
        writeback, and the same register is specified for Rd and Rn,
        the results are UNPREDICTABLE.

        For all loads and stores: if R15 is written, branch to
        that address afterwards.

        STRB: straightforward
        LDRB: loaded data is zero extended
        STR:  lowest 2 bits of address are ignored
        LDR:  if the lowest 2 bits of the address are nonzero
              then the loaded value is rotated right by 8 * the lowest 2 bits
     */
   }

  after_load_store_ubyte_or_word:

   /* --------------------- Load/store (sbyte & hword) -------- */
   // LDRH LDRSH STRH LDRSB
   /*                 31   27   23   19 15 11   7    3     # highest bit
                        28   24   20 16 12    8    4    0
      A5-36   1 | 16  cond 0001 U10L Rn Rd im4h 1SH1 im4l
      A5-38   1 | 32  cond 0001 U00L Rn Rd 0000 1SH1 Rm
      A5-40   2 | 16  cond 0001 U11L Rn Rd im4h 1SH1 im4l
      A5-42   2 | 32  cond 0001 U01L Rn Rd 0000 1SH1 Rm
      A5-44   3 | 16  cond 0000 U10L Rn Rd im4h 1SH1 im4l
      A5-46   3 | 32  cond 0000 U00L Rn Rd 0000 1SH1 Rm
   */
   /* case coding:
             1   at-ea               (access at ea)
             2   at-ea-then-upd      (access at ea, then Rn = ea)
             3   at-Rn-then-upd      (access at Rn, then Rn = ea)
      ea coding
             16  Rn +/- imm8
             32  Rn +/- Rm
   */
   /* Quickly skip over all of this for hopefully most instructions */
   if ((INSN(27,24) & BITS4(1,1,1,0)) != BITS4(0,0,0,0))
      goto after_load_store_sbyte_or_hword;

   /* Check the "1SH1" thing. */
   if ((INSN(7,4) & BITS4(1,0,0,1)) != BITS4(1,0,0,1))
      goto after_load_store_sbyte_or_hword;

   summary = 0;

   /**/ if (INSN(27,24) == BITS4(0,0,0,1) && INSN(22,21) == BITS2(1,0)) {
      summary = 1 | 16;
   }
   else if (INSN(27,24) == BITS4(0,0,0,1) && INSN(22,21) == BITS2(0,0)) {
      summary = 1 | 32;
   }
   else if (INSN(27,24) == BITS4(0,0,0,1) && INSN(22,21) == BITS2(1,1)) {
      summary = 2 | 16;
   }
   else if (INSN(27,24) == BITS4(0,0,0,1) && INSN(22,21) == BITS2(0,1)) {
      summary = 2 | 32;
   }
   else if (INSN(27,24) == BITS4(0,0,0,0) && INSN(22,21) == BITS2(1,0)) {
      summary = 3 | 16;
   }
   else if (INSN(27,24) == BITS4(0,0,0,0) && INSN(22,21) == BITS2(0,0)) {
      summary = 3 | 32;
   }
   else goto after_load_store_sbyte_or_hword;

   { UInt rN   = (insn >> 16) & 0xF; /* 19:16 */
     UInt rD   = (insn >> 12) & 0xF; /* 15:12 */
     UInt rM   = (insn >> 0)  & 0xF; /*  3:0  */
     UInt bU   = (insn >> 23) & 1;   /* 23 U=1 offset+, U=0 offset- */
     UInt bL   = (insn >> 20) & 1;   /* 20 L=1 load, L=0 store */
     UInt bH   = (insn >> 5) & 1;    /* H=1 halfword, H=0 byte */
     UInt bS   = (insn >> 6) & 1;    /* S=1 signed, S=0 unsigned */
     UInt imm8 = ((insn >> 4) & 0xF0) | (insn & 0xF); /* 11:8, 3:0 */

     /* Skip combinations that are either meaningless or already
        handled by main word-or-unsigned-byte load-store
        instructions. */
     if (bS == 0 && bH == 0) /* "unsigned byte" */
        goto after_load_store_sbyte_or_hword;
     if (bS == 1 && bL == 0) /* "signed store" */
        goto after_load_store_sbyte_or_hword;

     /* Require 11:8 == 0 for Rn +/- Rm cases */
     if ((summary & 32) != 0 && (imm8 & 0xF0) != 0)
        goto after_load_store_sbyte_or_hword;

     /* Skip some invalid cases, which would lead to two competing
        updates to the same register, or which are otherwise
        disallowed by the spec. */
     switch (summary) {
        case 1 | 16:
           break;
        case 1 | 32: 
           if (rM == 15) goto after_load_store_sbyte_or_hword;
           break;
        case 2 | 16: case 3 | 16:
           if (rN == 15) goto after_load_store_sbyte_or_hword;
           if (bL == 1 && rN == rD) goto after_load_store_sbyte_or_hword;
           break;
        case 2 | 32: case 3 | 32:
           if (rM == 15) goto after_load_store_sbyte_or_hword;
           if (rN == 15) goto after_load_store_sbyte_or_hword;
           if (rN == rM) goto after_load_store_sbyte_or_hword;
           if (bL == 1 && rN == rD) goto after_load_store_sbyte_or_hword;
           break;
        default:
           vassert(0);
     }

     /* Now, we can't do a conditional load or store, since that very
        likely will generate an exception.  So we have to take a side
        exit at this point if the condition is false. */
     if (condT != IRTemp_INVALID) {
        mk_skip_to_next_if_cond_is_false( condT );
        condT = IRTemp_INVALID;
     }
     /* Ok, now we're unconditional.  Do the load or store. */

     /* compute the effective address.  Bind it to a tmp since we
        may need to use it twice. */
     IRExpr* eaE = NULL;
     switch (summary & 0xF0) {
        case 16:
           eaE = mk_EA_reg_plusminus_imm8( rN, bU, imm8, dis_buf );
           break;
        case 32:
           eaE = mk_EA_reg_plusminus_reg( rN, bU, rM, dis_buf );
           break;
     }
     vassert(eaE);
     IRTemp eaT = newTemp(Ity_I32);
     assign(eaT, eaE);

     /* get the old Rn value */
     IRTemp rnT = newTemp(Ity_I32);
     assign(rnT, getIReg(rN));

     /* decide on the transfer address */
     IRTemp taT = IRTemp_INVALID;
     switch (summary & 0x0F) {
        case 1: case 2: taT = eaT; break;
        case 3:         taT = rnT; break;
     }
     vassert(taT != IRTemp_INVALID);

     /* halfword store  H 1  L 0  S 0
        uhalf load      H 1  L 1  S 0
        shalf load      H 1  L 1  S 1
        sbyte load      H 0  L 1  S 1
     */
     HChar* name = NULL;
     /* generate the transfer */
     /**/ if (bH == 1 && bL == 0 && bS == 0) { // halfword store
        storeLE( mkexpr(taT), unop(Iop_32to16, getIReg(rD)) );
        name = "strh";
     }
     else if (bH == 1 && bL == 1 && bS == 0) { // uhalf load
        putIReg( rD, unop(Iop_16Uto32, loadLE(Ity_I16, mkexpr(taT))),
                 IRTemp_INVALID, Ijk_Boring );
        name = "ldrh";
     }
     else if (bH == 1 && bL == 1 && bS == 1) { // shalf load
        putIReg( rD, unop(Iop_16Sto32, loadLE(Ity_I16, mkexpr(taT))),
                 IRTemp_INVALID, Ijk_Boring );
        name = "ldrsh";
     }
     else if (bH == 0 && bL == 1 && bS == 1) { // sbyte load
        putIReg( rD, unop(Iop_8Sto32, loadLE(Ity_I8, mkexpr(taT))),
                 IRTemp_INVALID, Ijk_Boring );
        name = "ldrsb";
     }
     else
        vassert(0); // should be assured by logic above

     /* Update Rn if necessary. */
     switch (summary & 0x0F) {
        case 2: case 3:
           // should be assured by logic above:
           if (bL == 1)
              vassert(rD != rN); /* since we just wrote rD */
           putIReg( rN, mkexpr(eaT), IRTemp_INVALID, Ijk_Boring );
           break;
     }

     switch (summary & 0x0F) {
        case 1:  DIP("%s%s r%u, %s\n", name, nCC(INSN_COND), rD, dis_buf);
                 break;
        case 2:  DIP("%s%s r%u, %s! (at-EA-then-Rn=EA)\n",
                     name, nCC(INSN_COND), rD, dis_buf);
                 break;
        case 3:  DIP("%s%s r%u, %s! (at-Rn-then-Rn=EA)\n",
                     name, nCC(INSN_COND), rD, dis_buf);
                 break;
        default: vassert(0);
     }

     /* XXX deal with alignment constraints */

     goto decode_success;

     /* Complications:

        For all loads: if the Amode specifies base register
        writeback, and the same register is specified for Rd and Rn,
        the results are UNPREDICTABLE.

        For all loads and stores: if R15 is written, branch to
        that address afterwards.

        Misaligned halfword stores => Unpredictable
        Misaligned halfword loads  => Unpredictable
     */
   }

  after_load_store_sbyte_or_hword:

   /* --------------------- Load/store multiple -------------- */
   // LD/STMIA LD/STMIB LD/STMDA LD/STMDB
   // Remarkably complex and difficult to get right
   // match 27:20 as 100XX0WL
   if (BITS8(1,0,0,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,0,0,1,0,0))) {
      // A5-50 LD/STMIA  cond 1000 10WL Rn RegList
      // A5-51 LD/STMIB  cond 1001 10WL Rn RegList
      // A5-53 LD/STMDA  cond 1000 00WL Rn RegList
      // A5-53 LD/STMDB  cond 1001 00WL Rn RegList
      //                   28   24   20 16       0

      Int  i, r, m, nRegs;

      UInt bINC    = (insn >> 23) & 1;
      UInt bBEFORE = (insn >> 24) & 1;

      UInt bL      = (insn >> 20) & 1;  /* load=1, store=0 */
      UInt bW      = (insn >> 21) & 1;  /* Rn wback=1, no wback=0 */
      UInt rN      = (insn >> 16) & 0xF;
      UInt regList = insn & 0xFFFF;
      /* Skip some invalid cases, which would lead to two competing
         updates to the same register, or which are otherwise
         disallowed by the spec.  Note the test above has required
         that S == 0, since that looks like a kernel-mode only thing.
         Done by forcing the real pattern, viz 100XXSWL to actually be
         100XX0WL. */
      if (rN == 15) goto after_load_store_multiple;
      // reglist can't be empty
      if (regList == 0) goto after_load_store_multiple;
      // if requested to writeback Rn, and this is a load instruction,
      // then Rn can't appear in RegList, since we'd have two competing
      // new values for Rn.  We do however accept this case for store
      // instructions.
      if (bW == 1 && bL == 1 && ((1 << rN) & regList) > 0)
         goto after_load_store_multiple;

      /* Now, we can't do a conditional load or store, since that very
         likely will generate an exception.  So we have to take a side
         exit at this point if the condition is false. */
      if (condT != IRTemp_INVALID) {
         mk_skip_to_next_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      /* Ok, now we're unconditional.  Do the load or store. */

      /* Get hold of the old Rn value.  We might need to write its
         value to memory during a store, and if it's also the
         writeback register then we need to get its value now.  We
         can't treat it exactly like the other registers we're going
         to transfer, because for xxMDA and xxMDB writeback forms, the
         generated IR updates Rn in the guest state before any
         transfers take place.  We have to do this as per comments
         below, in order that if Rn is the stack pointer then it
         always has a value is below or equal to any of the transfer
         addresses.  Ick. */
      IRTemp oldRnT = newTemp(Ity_I32);
      assign(oldRnT, getIReg(rN));

      IRTemp anchorT = newTemp(Ity_I32);
      /* The old (Addison-Wesley) ARM ARM seems to say that
         LDMxx/STMxx ignore the bottom two bits of the address.
         However, Cortex-A8 doesn't seem to care.  Hence: */
      /* No .. don't force alignment .. */
      /* assign(anchorT, binop(Iop_And32, mkexpr(oldRnT), mkU32(~3U))); */
      /* Instead, use the potentially misaligned address directly. */
      assign(anchorT, mkexpr(oldRnT));

      IROp opADDorSUB = bINC ? Iop_Add32 : Iop_Sub32;
      // bINC == 1:  xxMIA, xxMIB
      // bINC == 0:  xxMDA, xxMDB

      // For xxMDA and xxMDB, update Rn first if necessary.  We have
      // to do this first so that, for the common idiom of the transfers
      // faulting because we're pushing stuff onto a stack and the stack
      // is growing down onto allocate-on-fault pages (as Valgrind simulates),
      // we need to have the SP up-to-date "covering" (pointing below) the
      // transfer area.  For the same reason, if we are doing xxMIA or xxMIB,
      // do the transfer first, and then update rN afterwards.
      nRegs = 0;
      for (i = 0; i < 16; i++) {
        if ((regList & (1 << i)) != 0)
            nRegs++;
      }
      if (bW == 1 && !bINC) {
         putIReg( rN, binop(opADDorSUB, mkexpr(oldRnT), mkU32(4*nRegs)),
                  IRTemp_INVALID, Ijk_Boring );
      }

      // Make up a list of the registers to transfer, and their offsets
      // in memory relative to the anchor.  If the base reg (Rn) is part
      // of the transfer, then do it last for a load and first for a store.
      UInt xReg[16], xOff[16];
      Int  nX = 0;
      m = 0;
      for (i = 0; i < 16; i++) {
         r = bINC ? i : (15-i);
         if (0 == (regList & (1<<r)))
            continue;
         if (bBEFORE)
            m++;
         /* paranoia: check we aren't transferring the writeback
            register during a load. Should be assured by decode-point
            check above. */
         if (bW == 1 && bL == 1)
            vassert(r != rN);

         xOff[nX] = 4 * m;
         xReg[nX] = r;
         nX++;

         if (!bBEFORE)
            m++;
      }
      vassert(m == nRegs);
      vassert(nX == nRegs);
      vassert(nX <= 16);

      if (bW == 0 && (regList & (1<<rN)) != 0) {
         /* Non-writeback, and basereg is to be transferred.  Do its
            transfer last for a load and first for a store.  Requires
            reordering xOff/xReg. */
         if (0) {
            vex_printf("\nREG_LIST_PRE: (rN=%d)\n", rN);
            for (i = 0; i < nX; i++)
               vex_printf("reg %d   off %d\n", xReg[i], xOff[i]);
            vex_printf("\n");
         }

         vassert(nX > 0);
         for (i = 0; i < nX; i++) {
            if (xReg[i] == rN)
                break;
         }
         vassert(i < nX); /* else we didn't find it! */
         UInt tReg = xReg[i];
         UInt tOff = xOff[i];
         if (bL == 1) {
            /* load; make this transfer happen last */
            if (i < nX-1) {
               for (m = i+1; m < nX; m++) {
                  xReg[m-1] = xReg[m];
                  xOff[m-1] = xOff[m];
               }
               vassert(m == nX);
               xReg[m-1] = tReg;
               xOff[m-1] = tOff;
            }
         } else {
            /* store; make this transfer happen first */
            if (i > 0) {
               for (m = i-1; m >= 0; m--) {
                  xReg[m+1] = xReg[m];
                  xOff[m+1] = xOff[m];
               }
               vassert(m == -1);
               xReg[0] = tReg;
               xOff[0] = tOff;
            }
         }

         if (0) {
            vex_printf("REG_LIST_POST:\n");
            for (i = 0; i < nX; i++)
               vex_printf("reg %d   off %d\n", xReg[i], xOff[i]);
            vex_printf("\n");
         }
      }

      /* Actually generate the transfers */
      for (i = 0; i < nX; i++) {
         r = xReg[i];
         if (bL == 1) {
            putIReg( r, loadLE(Ity_I32,
                               binop(opADDorSUB, mkexpr(anchorT),
                                                 mkU32(xOff[i]))),
                     IRTemp_INVALID, Ijk_Ret );
         } else {
            /* if we're storing Rn, make sure we use the correct
               value, as per extensive comments above */
            storeLE( binop(opADDorSUB, mkexpr(anchorT), mkU32(xOff[i])),
                     r == rN ? mkexpr(oldRnT) : getIReg(r) );
         }
      }

      // If we are doing xxMIA or xxMIB,
      // do the transfer first, and then update rN afterwards.
      if (bW == 1 && bINC) {
         putIReg( rN, binop(opADDorSUB, mkexpr(oldRnT), mkU32(4*nRegs)),
                  IRTemp_INVALID, Ijk_Boring );
      }

      //if (vex_traceflags & VEX_TRACE_FE) {
      //}
      DIP("%sm%c%c%s r%u%s, {0x%04x}\n",
          bL == 1 ? "ld" : "st", bINC ? 'i' : 'd', bBEFORE ? 'b' : 'a',
          nCC(INSN_COND),
          rN, bW ? "!" : "", regList);

      goto decode_success;
   }

  after_load_store_multiple:

   /* --------------------- Control flow --------------------- */
   // B, BL (Branch, or Branch-and-Link, to immediate offset)
   //
   if (BITS8(1,0,1,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,0,0,0,0,0))) {
      UInt link   = (insn >> 24) & 1;
      UInt uimm24 = insn & ((1<<24)-1);
      Int  simm24 = (Int)uimm24;
      UInt dst    = guest_R15_curr_instr + 8 + (((simm24 << 8) >> 8) << 2);
      IRJumpKind jk = link ? Ijk_Call : Ijk_Boring;
      if (link) {
         putIReg(14, mkU32(guest_R15_curr_instr + 4), condT, Ijk_Boring);
      }
      if (condT == IRTemp_INVALID) {
         /* unconditional transfer to 'dst'.  See if we can simply
            continue tracing at the destination. */
         if (resteerOkFn( callback_opaque, (Addr64)dst )) {
            /* yes */
            dres.whatNext   = Dis_ResteerU;
            dres.continueAt = (Addr64)dst;
         } else {
            /* no; terminate the SB at this point. */
            irsb->next     = mkU32(dst);
            irsb->jumpkind = jk;
            dres.whatNext  = Dis_StopHere;
         }
         DIP("b%s 0x%x\n", link ? "l" : "", dst);
      } else {
         /* conditional transfer to 'dst' */
         HChar* comment = "";

         /* First see if we can do some speculative chasing into one
            arm or the other.  Be conservative and only chase if
            !link, that is, this is a normal conditional branch to a
            known destination. */
         if (!link
             && resteerCisOk
             && vex_control.guest_chase_cond
             && dst < guest_R15_curr_instr
             && resteerOkFn( callback_opaque, (Addr64)(Addr32)dst) ) {
            /* Speculation: assume this backward branch is taken.  So
               we need to emit a side-exit to the insn following this
               one, on the negation of the condition, and continue at
               the branch target address (dst). */
            stmt( IRStmt_Exit( unop(Iop_Not1,
                                    unop(Iop_32to1, mkexpr(condT))),
                               Ijk_Boring,
                               IRConst_U32(guest_R15_curr_instr+4) ));
            dres.whatNext   = Dis_ResteerC;
            dres.continueAt = (Addr64)(Addr32)dst;
            comment = "(assumed taken)";
         }
         else
         if (!link
             && resteerCisOk
             && vex_control.guest_chase_cond
             && dst >= guest_R15_curr_instr
             && resteerOkFn( callback_opaque, 
                             (Addr64)(Addr32)(guest_R15_curr_instr+4)) ) {
            /* Speculation: assume this forward branch is not taken.
               So we need to emit a side-exit to dst (the dest) and
               continue disassembling at the insn immediately
               following this one. */
            stmt( IRStmt_Exit( unop(Iop_32to1, mkexpr(condT)),
                               Ijk_Boring,
                               IRConst_U32(dst) ));
            dres.whatNext   = Dis_ResteerC;
            dres.continueAt = (Addr64)(Addr32)(guest_R15_curr_instr+4);
            comment = "(assumed not taken)";
         }
         else {
            /* Conservative default translation - end the block at
               this point. */
            stmt( IRStmt_Exit( unop(Iop_32to1, mkexpr(condT)),
                               jk, IRConst_U32(dst) ));
            irsb->next     = mkU32(guest_R15_curr_instr + 4);
            irsb->jumpkind = jk;
            dres.whatNext  = Dis_StopHere;
         }
         DIP("b%s%s 0x%x %s\n", link ? "l" : "", nCC(INSN_COND),
             dst, comment);
      }
      goto decode_success;
   }

   // BX, BLX (Branch, or Branch-and-Link, to a register)
   //
   if (INSN(27,20) == BITS8(0,0,0,1,0,0,1,0)
       && INSN(19,12) == BITS8(1,1,1,1,1,1,1,1)
       && (INSN(11,4) == BITS8(1,1,1,1,0,0,1,1)
           || INSN(11,4) == BITS8(1,1,1,1,0,0,0,1))) {
      IRExpr* dst;
      UInt    link = (INSN(11,4) >> 1) & 1;
      UInt    rM   = INSN(3,0);
      // we don't decode the case (link && rM == 15), as that's
      // Unpredictable.
      if (!(link && rM == 15)) {
         if (condT != IRTemp_INVALID) {
            mk_skip_to_next_if_cond_is_false( condT );
         }
         // AL after here
         // pretend no Thumb, hence ~3 instead of ~1
         dst = binop(Iop_And32, getIReg(rM), mkU32(~3));
         if (link) {
            putIReg( 14, mkU32(guest_R15_curr_instr + 4),
                     IRTemp_INVALID/*because AL*/, Ijk_Boring );
         }
         irsb->next     = dst;
         irsb->jumpkind = link ? Ijk_Call
                               : (rM == 14 ? Ijk_Ret : Ijk_Boring);
         dres.whatNext  = Dis_StopHere;
         if (condT == IRTemp_INVALID) {
            DIP("b%sx r%u\n", link ? "l" : "", rM);
         } else {
            DIP("b%sx%s r%u\n", link ? "l" : "", nCC(INSN_COND), rM);
         }
         goto decode_success;
      }
      /* else: (link && rM == 15): just fall through */
   }

   /* --------------------- Clz --------------------- */
   // CLZ
   if (INSN(27,20) == BITS8(0,0,0,1,0,1,1,0)
       && INSN(19,16) == BITS4(1,1,1,1)
       && INSN(11,4) == BITS8(1,1,1,1,0,0,0,1)) {
      UInt rD = INSN(15,12);
      UInt rM = INSN(3,0);
      IRTemp arg = newTemp(Ity_I32);
      IRTemp res = newTemp(Ity_I32);
      assign(arg, getIReg(rM));
      assign(res, IRExpr_Mux0X(
                     unop(Iop_1Uto8,binop(Iop_CmpEQ32, mkexpr(arg),
                                                       mkU32(0))),
                     unop(Iop_Clz32, mkexpr(arg)),
                     mkU32(32)
            ));
      putIReg(rD, mkexpr(res), condT, Ijk_Boring);
      DIP("clz%s r%u, r%u\n", nCC(INSN_COND), rD, rM);
      goto decode_success;
   }

   /* --------------------- Mul etc --------------------- */
   // MUL
   if (BITS8(0,0,0,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,1,1,1,0))
       && INSN(15,12) == BITS4(0,0,0,0)
       && INSN(7,4) == BITS4(1,0,0,1)) {
      UInt bitS = (insn >> 20) & 1; /* 20:20 */
      UInt rD = INSN(19,16);
      UInt rS = INSN(11,8);
      UInt rM = INSN(3,0);
      if (rD == 15 || rM == 15 || rS == 15) {
         /* Unpredictable; don't decode; fall through */
      } else {
         IRTemp argL = newTemp(Ity_I32);
         IRTemp argR = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         IRTemp oldC = IRTemp_INVALID;
         IRTemp oldV = IRTemp_INVALID;
         assign( argL, getIReg(rM));
         assign( argR, getIReg(rS));
         assign( res, binop(Iop_Mul32, mkexpr(argL), mkexpr(argR)) );
         if (bitS) {
            oldC = newTemp(Ity_I32);
            assign(oldC, mk_armg_calculate_flag_c());
            oldV = newTemp(Ity_I32);
            assign(oldV, mk_armg_calculate_flag_v());
         }
         // now update guest state
         putIReg( rD, mkexpr(res), condT, Ijk_Boring );
         if (bitS) {
            IRTemp pair = newTemp(Ity_I32);
            assign( pair, binop(Iop_Or32,
                                binop(Iop_Shl32, mkexpr(oldC), mkU8(1)),
                                mkexpr(oldV)) );
            setFlags_D1_ND( ARMG_CC_OP_MUL, res, pair, condT );
         }
         DIP("mul%c%s r%u, r%u, r%u\n",
             bitS ? 's' : ' ', nCC(INSN_COND), rD, rM, rS);
         goto decode_success;
      }
      /* fall through */
   }

   // MLA, MLS
   if (BITS8(0,0,0,0,0,0,1,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,0))
       && INSN(7,4) == BITS4(1,0,0,1)) {
      UInt bitS  = (insn >> 20) & 1; /* 20:20 */
      UInt isMLS = (insn >> 22) & 1; /* 22:22 */
      UInt rD = INSN(19,16);
      UInt rN = INSN(15,12);
      UInt rS = INSN(11,8);
      UInt rM = INSN(3,0);
      if (bitS == 1 && isMLS == 1) {
         /* This isn't allowed (MLS that sets flags).  don't decode;
            fall through */
      }
      else
      if (rD == 15 || rM == 15 || rS == 15 || rN == 15) {
         /* Unpredictable; don't decode; fall through */
      } else {
         IRTemp argL = newTemp(Ity_I32);
         IRTemp argR = newTemp(Ity_I32);
         IRTemp argP = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         IRTemp oldC = IRTemp_INVALID;
         IRTemp oldV = IRTemp_INVALID;
         assign( argL, getIReg(rM));
         assign( argR, getIReg(rS));
         assign( argP, getIReg(rN));
         assign( res, binop(isMLS ? Iop_Sub32 : Iop_Add32,
                            mkexpr(argP),
                            binop(Iop_Mul32, mkexpr(argL), mkexpr(argR)) ));
         if (bitS) {
            vassert(!isMLS); // guaranteed above
            oldC = newTemp(Ity_I32);
            assign(oldC, mk_armg_calculate_flag_c());
            oldV = newTemp(Ity_I32);
            assign(oldV, mk_armg_calculate_flag_v());
         }
         // now update guest state
         putIReg( rD, mkexpr(res), condT, Ijk_Boring );
         if (bitS) {
            IRTemp pair = newTemp(Ity_I32);
            assign( pair, binop(Iop_Or32,
                                binop(Iop_Shl32, mkexpr(oldC), mkU8(1)),
                                mkexpr(oldV)) );
            setFlags_D1_ND( ARMG_CC_OP_MUL, res, pair, condT );
         }
         DIP("ml%c%c%s r%u, r%u, r%u, r%u\n",
             isMLS ? 's' : 'a', bitS ? 's' : ' ', nCC(INSN_COND), rD, rM, rS, rN);
         goto decode_success;
      }
      /* fall through */
   }

   // SMULL, UMULL
   if (BITS8(0,0,0,0,1,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,0))
       && INSN(7,4) == BITS4(1,0,0,1)) {
      UInt bitS = (insn >> 20) & 1; /* 20:20 */
      UInt rDhi = INSN(19,16);
      UInt rDlo = INSN(15,12);
      UInt rS   = INSN(11,8);
      UInt rM   = INSN(3,0);
      UInt isS  = (INSN(27,20) >> 2) & 1; /* 22:22 */
      if (rDhi == 15 || rDlo == 15 || rM == 15 || rS == 15 || rDhi == rDlo)  {
         /* Unpredictable; don't decode; fall through */
      } else {
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp res   = newTemp(Ity_I64);
         IRTemp resHi = newTemp(Ity_I32);
         IRTemp resLo = newTemp(Ity_I32);
         IRTemp oldC  = IRTemp_INVALID;
         IRTemp oldV  = IRTemp_INVALID;
         IROp   mulOp = isS ? Iop_MullS32 : Iop_MullU32;
         assign( argL, getIReg(rM));
         assign( argR, getIReg(rS));
         assign( res, binop(mulOp, mkexpr(argL), mkexpr(argR)) );
         assign( resHi, unop(Iop_64HIto32, mkexpr(res)) );
         assign( resLo, unop(Iop_64to32, mkexpr(res)) );
         if (bitS) {
            oldC = newTemp(Ity_I32);
            assign(oldC, mk_armg_calculate_flag_c());
            oldV = newTemp(Ity_I32);
            assign(oldV, mk_armg_calculate_flag_v());
         }
         // now update guest state
         putIReg( rDhi, mkexpr(resHi), condT, Ijk_Boring );
         putIReg( rDlo, mkexpr(resLo), condT, Ijk_Boring );
         if (bitS) {
            IRTemp pair = newTemp(Ity_I32);
            assign( pair, binop(Iop_Or32,
                                binop(Iop_Shl32, mkexpr(oldC), mkU8(1)),
                                mkexpr(oldV)) );
            setFlags_D1_D2_ND( ARMG_CC_OP_MULL, resLo, resHi, pair, condT );
         }
         DIP("%cmull%c%s r%u, r%u, r%u, r%u\n",
             isS ? 's' : 'u', bitS ? 's' : ' ',
             nCC(INSN_COND), rDlo, rDhi, rM, rS);
         goto decode_success;
      }
      /* fall through */
   }

   // SMLAL, UMLAL
   if (BITS8(0,0,0,0,1,0,1,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,0))
       && INSN(7,4) == BITS4(1,0,0,1)) {
      UInt bitS = (insn >> 20) & 1; /* 20:20 */
      UInt rDhi = INSN(19,16);
      UInt rDlo = INSN(15,12);
      UInt rS   = INSN(11,8);
      UInt rM   = INSN(3,0);
      UInt isS  = (INSN(27,20) >> 2) & 1; /* 22:22 */
      if (rDhi == 15 || rDlo == 15 || rM == 15 || rS == 15 || rDhi == rDlo)  {
         /* Unpredictable; don't decode; fall through */
      } else {
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp old   = newTemp(Ity_I64);
         IRTemp res   = newTemp(Ity_I64);
         IRTemp resHi = newTemp(Ity_I32);
         IRTemp resLo = newTemp(Ity_I32);
         IRTemp oldC  = IRTemp_INVALID;
         IRTemp oldV  = IRTemp_INVALID;
         IROp   mulOp = isS ? Iop_MullS32 : Iop_MullU32;
         assign( argL, getIReg(rM));
         assign( argR, getIReg(rS));
         assign( old, binop(Iop_32HLto64, getIReg(rDhi), getIReg(rDlo)) );
         assign( res, binop(Iop_Add64,
                            mkexpr(old),
                            binop(mulOp, mkexpr(argL), mkexpr(argR))) );
         assign( resHi, unop(Iop_64HIto32, mkexpr(res)) );
         assign( resLo, unop(Iop_64to32, mkexpr(res)) );
         if (bitS) {
            oldC = newTemp(Ity_I32);
            assign(oldC, mk_armg_calculate_flag_c());
            oldV = newTemp(Ity_I32);
            assign(oldV, mk_armg_calculate_flag_v());
         }
         // now update guest state
         putIReg( rDhi, mkexpr(resHi), condT, Ijk_Boring );
         putIReg( rDlo, mkexpr(resLo), condT, Ijk_Boring );
         if (bitS) {
            IRTemp pair = newTemp(Ity_I32);
            assign( pair, binop(Iop_Or32,
                                binop(Iop_Shl32, mkexpr(oldC), mkU8(1)),
                                mkexpr(oldV)) );
            setFlags_D1_D2_ND( ARMG_CC_OP_MULL, resLo, resHi, pair, condT );
         }
         DIP("%cmlal%c%s r%u, r%u, r%u, r%u\n",
             isS ? 's' : 'u', bitS ? 's' : ' ', nCC(INSN_COND),
             rDlo, rDhi, rM, rS);
         goto decode_success;
      }
      /* fall through */
   }

   /* --------------------- Msr etc --------------------- */

   // MSR (immediate form, flags only)
   if (BITS8(0,0,1,1,0,0,1,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && INSN(15,12) == BITS4(1,1,1,1)) {
      UInt bitR = (insn >> 22) & 1;
      if (bitR == 0 && INSN(19,16) == BITS4(1,0,0,0)) {
         UInt   imm = (INSN(11,0) >> 0) & 0xFF;
         UInt   rot = 2 * ((INSN(11,0) >> 8) & 0xF);
         IRTemp immT = newTemp(Ity_I32);
         vassert(rot <= 30);
         imm = ROR32(imm, rot);
         imm &= 0xFF000000;
         imm &= (ARMG_CC_MASK_N | ARMG_CC_MASK_Z 
                 | ARMG_CC_MASK_V | ARMG_CC_MASK_C);
         assign( immT, mkU32(imm & 0xF0000000) );
         setFlags_D1(ARMG_CC_OP_COPY, immT, condT);
         DIP("msr%s cpsr_f, #0x%08x\n", nCC(INSN_COND), imm);
         goto decode_success;
      }
      /* fall through */
   }

   // MRS
   if (BITS8(0,0,0,1,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && INSN(19,16) == BITS4(1,1,1,1)
       && INSN(11,0) == 0) {
      UInt bitR = (insn >> 22) & 1;
      UInt rD   = INSN(15,12);
      if (bitR == 0 && rD != 15) {
         IRTemp res = newTemp(Ity_I32);
         assign( res, mk_armg_calculate_flags_nzcv() );
         putIReg( rD, mkexpr(res), condT, Ijk_Boring );
         DIP("mrs%s r%u, cpsr\n", nCC(INSN_COND), rD);
         goto decode_success;
      }
      /* fall through */
   }

   /* --------------------- Svc --------------------- */
   if (BITS8(1,1,1,1,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,0,0,0))) {
      UInt imm24 = (insn >> 0) & 0xFFFFFF;
      if (imm24 == 0) {
         /* A syscall.  We can't do this conditionally, hence: */
         if (condT != IRTemp_INVALID) {
            mk_skip_to_next_if_cond_is_false( condT );
         }
         // AL after here
         irsb->next     = mkU32( guest_R15_curr_instr + 4 );
         irsb->jumpkind = Ijk_Sys_syscall;
         dres.whatNext  = Dis_StopHere;
         DIP("svc%s #0x%08x\n", nCC(INSN_COND), imm24);
         goto decode_success;
      }
      /* fall through */
   }

   /* ------------------------ swp ------------------------ */

   // SWP, SWPB
   if (BITS8(0,0,0,1,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,0,0,0) == INSN(11,8)
       && BITS4(1,0,0,1) == INSN(7,4)) {
      UInt   rN   = INSN(19,16);
      UInt   rD   = INSN(15,12);
      UInt   rM   = INSN(3,0);
      IRTemp tRn  = newTemp(Ity_I32);
      IRTemp tNew = newTemp(Ity_I32);
      IRTemp tOld = IRTemp_INVALID;
      IRTemp tSC1 = newTemp(Ity_I1);
      UInt   isB  = (insn >> 22) & 1;

      if (rD == 15 || rN == 15 || rM == 15 || rN == rM || rN == rD) {
         /* undecodable; fall through */
      } else {
         /* make unconditional */
         if (condT != IRTemp_INVALID) {
            mk_skip_to_next_if_cond_is_false( condT );
            condT = IRTemp_INVALID;
         }
         /* Ok, now we're unconditional.  Generate a LL-SC loop. */
         assign(tRn, getIReg(rN));
         assign(tNew, getIReg(rM));
         if (isB) {
            /* swpb */
            tOld = newTemp(Ity_I8);
            stmt( IRStmt_LLSC(Iend_LE, tOld, mkexpr(tRn),
                              NULL/*=>isLL*/) );
            stmt( IRStmt_LLSC(Iend_LE, tSC1, mkexpr(tRn),
                              unop(Iop_32to8, mkexpr(tNew))) );
         } else {
            /* swp */
            tOld = newTemp(Ity_I32);
            stmt( IRStmt_LLSC(Iend_LE, tOld, mkexpr(tRn),
                              NULL/*=>isLL*/) );
            stmt( IRStmt_LLSC(Iend_LE, tSC1, mkexpr(tRn),
                              mkexpr(tNew)) );
         }
         stmt( IRStmt_Exit(unop(Iop_Not1, mkexpr(tSC1)),
                           /*Ijk_NoRedir*/Ijk_Boring,
                           IRConst_U32(guest_R15_curr_instr)) );
         putIReg(rD, isB ? unop(Iop_8Uto32, mkexpr(tOld)) : mkexpr(tOld),
                     IRTemp_INVALID, Ijk_Boring);
         DIP("swp%s%s r%u, r%u, [r%u]\n",
             isB ? "b" : "", nCC(INSN_COND), rD, rM, rN);
         goto decode_success;
      }
      /* fall through */
   }

   /* ----------------------------------------------------------- */
   /* -- VFP instructions -- double precision (mostly)         -- */
   /* ----------------------------------------------------------- */

   /* --------------------- fldmx, fstmx --------------------- */
   /*
                                 31   27   23   19 15 11   7   0
                                         P U WL
      C4-100, C5-26  1  FSTMX    cond 1100 1000 Rn Dd 1011 offset
      C4-100, C5-28  2  FSTMIAX  cond 1100 1010 Rn Dd 1011 offset
      C4-100, C5-30  3  FSTMDBX  cond 1101 0010 Rn Dd 1011 offset

      C4-42, C5-26   1  FLDMX    cond 1100 1001 Rn Dd 1011 offset
      C4-42, C5-28   2  FLDMIAX  cond 1100 1011 Rn Dd 1011 offset
      C4-42, C5-30   3  FLDMDBX  cond 1101 0011 Rn Dd 1011 offset

      Regs transferred: Dd .. D(d + (offset-3)/2)
      offset must be odd, must not imply a reg > 15
      IA/DB: Rn is changed by (4 + 8 x # regs transferred)

      case coding:
         1  at-Rn   (access at Rn)
         2  ia-Rn   (access at Rn, then Rn += 4+8n)
         3  db-Rn   (Rn -= 4+8n,   then access at Rn)
   */
   if (BITS8(1,1,0,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,0,0,1,0,0))
       && INSN(11,8) == BITS4(1,0,1,1)) {
      UInt bP     = (insn >> 24) & 1;
      UInt bU     = (insn >> 23) & 1;
      UInt bW     = (insn >> 21) & 1;
      UInt bL     = (insn >> 20) & 1;
      UInt offset = (insn >> 0) & 0xFF;
      UInt rN     = INSN(19,16);
      UInt dD     = INSN(15,12);
      UInt nRegs  = (offset - 1) / 2;
      Int  i;

      /**/ if (bP == 0 && bU == 1 && bW == 0) {
         vassert(0); //ATC
         summary = 1;
      }
      else if (bP == 0 && bU == 1 && bW == 1) {
         summary = 2;
      }
      else if (bP == 1 && bU == 0 && bW == 1) {
         summary = 3;
      }
      else goto after_vfp_fldmx_fstmx;

      /* no writebacks to r15 allowed */
      if (rN == 15 && (summary == 2 || summary == 3))
         goto after_vfp_fldmx_fstmx;

      /* offset must be odd, and specify at least one register */
      if (0 == (offset & 1) || offset < 3)
         goto after_vfp_fldmx_fstmx;

      /* can't transfer regs after D15 */
      if (dD + nRegs - 1 >= 16)
         goto after_vfp_fldmx_fstmx;

      /* Now, we can't do a conditional load or store, since that very
         likely will generate an exception.  So we have to take a side
         exit at this point if the condition is false. */
      if (condT != IRTemp_INVALID) {
         mk_skip_to_next_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      /* Ok, now we're unconditional.  Do the load or store. */

      /* get the old Rn value */
      IRTemp rnT = newTemp(Ity_I32);
      assign(rnT, getIReg(rN));

      /* make a new value for Rn, post-insn */
      IRTemp rnTnew = IRTemp_INVALID;
      if (summary == 2 || summary == 3) {
         rnTnew = newTemp(Ity_I32);
         assign(rnTnew, binop(summary == 2 ? Iop_Add32 : Iop_Sub32,
                              mkexpr(rnT),
                              mkU32(4 + 8 * nRegs)));
      }

      /* decide on the base transfer address */
      IRTemp taT = newTemp(Ity_I32);
      assign(taT,  summary == 3 ? mkexpr(rnTnew) : mkexpr(rnT));

      /* update Rn if necessary -- in case 3, we're moving it down, so
         update before any memory reference, in order to keep Memcheck
         and V's stack-extending logic (on linux) happy */
      if (summary == 3)
         putIReg(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);

      /* generate the transfers */
      for (i = 0; i < nRegs; i++) {
         IRExpr* addr = binop(Iop_Add32, mkexpr(taT), mkU32(8*i));
         if (bL) {
            putDReg(dD + i, loadLE(Ity_F64, addr), IRTemp_INVALID);
         } else {
            storeLE(addr, getDReg(dD + i));
         }
      }

      /* update Rn if necessary -- in case 2, we're moving it up, so
         update after any memory reference, in order to keep Memcheck
         and V's stack-extending logic (on linux) happy */
      if (summary == 2)
         putIReg(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);

      HChar* nm = bL==1 ? "ld" : "st";
      switch (summary) {
         case 1:  DIP("f%smx%s r%u, {d%u-d%u}\n", 
                      nm, nCC(INSN_COND), rN, dD, dD + nRegs - 1);
                  break;
         case 2:  DIP("f%smiax%s r%u!, {d%u-d%u}\n", 
                      nm, nCC(INSN_COND), rN, dD, dD + nRegs - 1);
                  break;
         case 3:  DIP("f%smdbx%s r%u!, {d%u-d%u}\n", 
                      nm, nCC(INSN_COND), rN, dD, dD + nRegs - 1);
                  break;
         default: vassert(0);
      }


      goto decode_success;
      /* FIXME alignment constraints? */
   }

  after_vfp_fldmx_fstmx:

   /* --------------------- fldmd, fstmd --------------------- */
   /*
                                 31   27   23   19 15 11   7   0
                                         P U WL
      C4-96, C5-26   1  FSTMD    cond 1100 1000 Rn Dd 1011 offset
      C4-96, C5-28   2  FSTMDIA  cond 1100 1010 Rn Dd 1011 offset
      C4-96, C5-30   3  FSTMDDB  cond 1101 0010 Rn Dd 1011 offset

      C4-38, C5-26   1  FLDMD    cond 1100 1001 Rn Dd 1011 offset
      C4-38, C5-28   2  FLDMIAD  cond 1100 1011 Rn Dd 1011 offset
      C4-38, C5-30   3  FLDMDBD  cond 1101 0011 Rn Dd 1011 offset

      Regs transferred: Dd .. D(d + (offset-2)/2)
      offset must be even, must not imply a reg > 15
      IA/DB: Rn is changed by (8 x # regs transferred)

      case coding:
         1  at-Rn   (access at Rn)
         2  ia-Rn   (access at Rn, then Rn += 8n)
         3  db-Rn   (Rn -= 8n,     then access at Rn)
   */
   if (BITS8(1,1,0,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,0,0,1,0,0))
       && INSN(11,8) == BITS4(1,0,1,1)) {
      UInt bP     = (insn >> 24) & 1;
      UInt bU     = (insn >> 23) & 1;
      UInt bW     = (insn >> 21) & 1;
      UInt bL     = (insn >> 20) & 1;
      UInt offset = (insn >> 0) & 0xFF;
      UInt rN     = INSN(19,16);
      UInt dD     = INSN(15,12);
      UInt nRegs  = offset / 2;
      Int  i;

      /**/ if (bP == 0 && bU == 1 && bW == 0) {
         vassert(0); //ATC
         summary = 1;
      }
      else if (bP == 0 && bU == 1 && bW == 1) {
         summary = 2;
      }
      else if (bP == 1 && bU == 0 && bW == 1) {
         summary = 3;
      }
      else goto after_vfp_fldmd_fstmd;

      /* no writebacks to r15 allowed */
      if (rN == 15 && (summary == 2 || summary == 3))
         goto after_vfp_fldmd_fstmd;

      /* offset must be even, and specify at least one register */
      if (1 == (offset & 1) || offset < 2)
         goto after_vfp_fldmd_fstmd;

      /* can't transfer regs after D15 */
      if (dD + nRegs - 1 >= 16)
         goto after_vfp_fldmd_fstmd;

      /* Now, we can't do a conditional load or store, since that very
         likely will generate an exception.  So we have to take a side
         exit at this point if the condition is false. */
      if (condT != IRTemp_INVALID) {
         mk_skip_to_next_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      /* Ok, now we're unconditional.  Do the load or store. */

      /* get the old Rn value */
      IRTemp rnT = newTemp(Ity_I32);
      assign(rnT, getIReg(rN));

      /* make a new value for Rn, post-insn */
      IRTemp rnTnew = IRTemp_INVALID;
      if (summary == 2 || summary == 3) {
         rnTnew = newTemp(Ity_I32);
         assign(rnTnew, binop(summary == 2 ? Iop_Add32 : Iop_Sub32,
                              mkexpr(rnT),
                              mkU32(8 * nRegs)));
      }

      /* decide on the base transfer address */
      IRTemp taT = newTemp(Ity_I32);
      assign(taT, summary == 3 ? mkexpr(rnTnew) : mkexpr(rnT));

      /* update Rn if necessary -- in case 3, we're moving it down, so
         update before any memory reference, in order to keep Memcheck
         and V's stack-extending logic (on linux) happy */
      if (summary == 3)
         putIReg(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);

      /* generate the transfers */
      for (i = 0; i < nRegs; i++) {
         IRExpr* addr = binop(Iop_Add32, mkexpr(taT), mkU32(8*i));
         if (bL) {
            putDReg(dD + i, loadLE(Ity_F64, addr), IRTemp_INVALID);
         } else {
            storeLE(addr, getDReg(dD + i));
         }
      }

      /* update Rn if necessary -- in case 2, we're moving it up, so
         update after any memory reference, in order to keep Memcheck
         and V's stack-extending logic (on linux) happy */
      if (summary == 2)
         putIReg(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);

      HChar* nm = bL==1 ? "ld" : "st";
      switch (summary) {
         case 1:  DIP("f%smd%s r%u, {d%u-d%u}\n", 
                      nm, nCC(INSN_COND), rN, dD, dD + nRegs - 1);
                  break;
         case 2:  DIP("f%smiad%s r%u!, {d%u-d%u}\n", 
                      nm, nCC(INSN_COND), rN, dD, dD + nRegs - 1);
                  break;
         case 3:  DIP("f%smdbd%s r%u!, {d%u-d%u}\n", 
                      nm, nCC(INSN_COND), rN, dD, dD + nRegs - 1);
                  break;
         default: vassert(0);
      }

      goto decode_success;
      /* FIXME alignment constraints? */
   }

  after_vfp_fldmd_fstmd:

   /* ------------------- fmrx, fmxr ------------------- */
   if (BITS8(1,1,1,0,1,1,1,1) == INSN(27,20)
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS8(0,0,0,1,0,0,0,0) == (insn & 0xFF)) {
      UInt rD  = INSN(15,12);
      UInt reg = INSN(19,16);
      if (reg == BITS4(0,0,0,1)) {
         if (rD == 15) {
            IRTemp nzcvT = newTemp(Ity_I32);
            /* When rD is 15, we are copying the top 4 bits of FPSCR
               into CPSR.  That is, set the flags thunk to COPY and
               install FPSCR[31:28] as the value to copy. */
            assign(nzcvT, binop(Iop_And32,
                                IRExpr_Get(OFFB_FPSCR, Ity_I32),
                                mkU32(0xF0000000)));
            setFlags_D1(ARMG_CC_OP_COPY, nzcvT, condT);
            DIP("fmstat%s\n", nCC(INSN_COND));
         } else {
            /* Otherwise, merely transfer FPSCR to r0 .. r14. */
            putIReg(rD, IRExpr_Get(OFFB_FPSCR, Ity_I32), 
                        condT, Ijk_Boring);
            DIP("fmrx%s r%u, fpscr\n", nCC(INSN_COND), rD);
         }
         goto decode_success;
      }
      /* fall through */
   }

   if (BITS8(1,1,1,0,1,1,1,0) == INSN(27,20)
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS8(0,0,0,1,0,0,0,0) == (insn & 0xFF)) {
      UInt rD  = INSN(15,12);
      UInt reg = INSN(19,16);
      if (reg == BITS4(0,0,0,1)) {
         putMiscReg32(OFFB_FPSCR, getIReg(rD), condT);
         DIP("fmxr%s fpscr, r%u\n", nCC(INSN_COND), rD);
         goto decode_success;
      }
      /* fall through */
   }

   /* --------------------- vmov --------------------- */
   // VMOV dM, rD, rN
   if (0x0C400B10 == (insn & 0x0FF00FF0)) {
      UInt dM = INSN(3,0);
      UInt rD = INSN(15,12); /* lo32 */
      UInt rN = INSN(19,16); /* hi32 */
      if (rD == 15 || rN == 15) {
         /* fall through */
      } else {
         putDReg(dM,
                 unop(Iop_ReinterpI64asF64,
                      binop(Iop_32HLto64, getIReg(rN), getIReg(rD))),
                 condT);
         DIP("vmov%s d%u, r%u, r%u\n", nCC(INSN_COND), dM, rD, rN);
         goto decode_success;
      }
      /* fall through */
   }

   // VMOV rD, rN, dM
   if (0x0C500B10 == (insn & 0x0FF00FF0)) {
      UInt dM = INSN(3,0);
      UInt rD = INSN(15,12); /* lo32 */
      UInt rN = INSN(19,16); /* hi32 */
      if (rD == 15 || rN == 15 || rD == rN) {
         /* fall through */
      } else {
         IRTemp i64 = newTemp(Ity_I64);
         assign(i64, unop(Iop_ReinterpF64asI64, getDReg(dM)));
         putIReg(rN, unop(Iop_64HIto32, mkexpr(i64)), condT, Ijk_Boring);
         putIReg(rD, unop(Iop_64to32,   mkexpr(i64)), condT, Ijk_Boring);
         DIP("vmov%s r%u, r%u, d%u\n", nCC(INSN_COND), rD, rN, dM);
         goto decode_success;
      }
      /* fall through */
   }

   /* --------------------- f{ld,st}d --------------------- */
   // FLDD, FSTD
   if (BITS8(1,1,0,1,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,1,1,0))
       && BITS4(1,0,1,1) == INSN(11,8)) {
      UInt dD     = INSN(15,12);
      UInt rN     = INSN(19,16);
      UInt offset = (insn & 0xFF) << 2;
      UInt bU     = (insn >> 23) & 1; /* 1: +offset  0: -offset */
      UInt bL     = (insn >> 20) & 1; /* 1: load  0: store */
      /* make unconditional */
      if (condT != IRTemp_INVALID) {
         mk_skip_to_next_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      IRTemp ea = newTemp(Ity_I32);
      assign(ea, binop(bU ? Iop_Add32 : Iop_Sub32,
                       getIReg(rN), mkU32(offset)));
      if (bL) {
         putDReg(dD, loadLE(Ity_F64,mkexpr(ea)), IRTemp_INVALID);
      } else {
         storeLE(mkexpr(ea), getDReg(dD));
      }
      DIP("f%sd%s d%u, [r%u, %c#%u]\n",
          bL ? "ld" : "st", nCC(INSN_COND), dD, rN,
          bU ? '+' : '-', offset);
      goto decode_success;
   }

   /* --------------------- dp insns (D) --------------------- */
   if (BITS8(1,1,1,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,1,0,0))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,0,0,0) == (INSN(7,4) & BITS4(1,0,1,1))) {
      UInt    dM  = INSN(3,0);   /* argR */
      UInt    dD  = INSN(15,12); /* dst/acc */
      UInt    dN  = INSN(19,16); /* argL */
      UInt    bP  = (insn >> 23) & 1;
      UInt    bQ  = (insn >> 21) & 1;
      UInt    bR  = (insn >> 20) & 1;
      UInt    bS  = (insn >> 6) & 1;
      UInt    opc = (bP << 3) | (bQ << 2) | (bR << 1) | bS;
      IRExpr* rm  = get_FAKE_roundingmode(); /* XXXROUNDINGFIXME */
      switch (opc) {
         case BITS4(0,0,0,0): /* MAC: d + n * m */
            putDReg(dD, triop(Iop_AddF64, rm,
                              getDReg(dD),
                              triop(Iop_MulF64, rm, getDReg(dN),
                                                    getDReg(dM))),
                        condT);
            DIP("fmacd%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         case BITS4(0,0,0,1): /* NMAC: d - n * m */
            putDReg(dD, triop(Iop_SubF64, rm,
                              getDReg(dD),
                              triop(Iop_MulF64, rm, getDReg(dN),
                                                    getDReg(dM))),
                        condT);
            DIP("fnmacd%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         case BITS4(0,0,1,0): /* MSC: - d + n * m */
            putDReg(dD, triop(Iop_AddF64, rm,
                              unop(Iop_NegF64, getDReg(dD)),
                              triop(Iop_MulF64, rm, getDReg(dN),
                                                    getDReg(dM))),
                        condT);
            DIP("fmscd%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         case BITS4(0,0,1,1): /* NMSC: - d - n * m */
            putDReg(dD, triop(Iop_SubF64, rm,
                              unop(Iop_NegF64, getDReg(dD)),
                              triop(Iop_MulF64, rm, getDReg(dN),
                                                    getDReg(dM))),
                        condT);
            DIP("fnmscd%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         case BITS4(0,1,0,0): /* MUL: n * m */
            putDReg(dD, triop(Iop_MulF64, rm, getDReg(dN), getDReg(dM)),
                        condT);
            DIP("fmuld%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         case BITS4(0,1,0,1): /* NMUL: - n * m */
            putDReg(dD, unop(Iop_NegF64,
                             triop(Iop_MulF64, rm, getDReg(dN),
                                                   getDReg(dM))),
                    condT);
            DIP("fnmuld%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         case BITS4(0,1,1,0): /* ADD: n + m */
            putDReg(dD, triop(Iop_AddF64, rm, getDReg(dN), getDReg(dM)),
                        condT);
            DIP("faddd%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         case BITS4(0,1,1,1): /* SUB: n - m */
            putDReg(dD, triop(Iop_SubF64, rm, getDReg(dN), getDReg(dM)),
                        condT);
            DIP("fsubd%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         case BITS4(1,0,0,0): /* DIV: n / m */
            putDReg(dD, triop(Iop_DivF64, rm, getDReg(dN), getDReg(dM)),
                        condT);
            DIP("fdivd%s d%u, d%u, d%u\n", nCC(INSN_COND), dD, dN, dM);
            goto decode_success;
         default:
            break;
      }
   }

   /* --------------------- compares (D) --------------------- */
   /*          31   27   23   19   15 11   7    3
                 28   24   20   16 12    8    4    0 
      FCMPD    cond 1110 1011 0100 Dd 1011 0100 Dm
      FCMPED   cond 1110 1011 0100 Dd 1011 1100 Dm
      FCMPZD   cond 1110 1011 0101 Dd 1011 0100 0000
      FCMPZED  cond 1110 1011 0101 Dd 1011 1100 0000
                                 Z         N

      Z=0 Compare Dd vs Dm     and set FPSCR 31:28 accordingly
      Z=1 Compare Dd vs zero

      N=1 generates Invalid Operation exn if either arg is any kind of NaN
      N=0 generates Invalid Operation exn if either arg is a signalling NaN
      (Not that we pay any attention to N here)
   */
   if (BITS8(1,1,1,0,1,0,1,1) == INSN(27,20)
       && BITS4(0,1,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,1,1))) {
      UInt bZ = (insn >> 16) & 1;
      UInt bN = (insn >> 7) & 1;
      UInt dD = INSN(15,12);
      UInt dM = INSN(3,0);
      if (bZ && INSN(3,0) != 0) {
         /* does not decode; fall through */
      } else {
         IRTemp argL = newTemp(Ity_F64);
         IRTemp argR = newTemp(Ity_F64);
         IRTemp irRes = newTemp(Ity_I32);
         assign(argL, getDReg(dD));
         assign(argR, bZ ? IRExpr_Const(IRConst_F64i(0)) : getDReg(dM));
         assign(irRes, binop(Iop_CmpF64, mkexpr(argL), mkexpr(argR)));

         IRTemp nzcv     = IRTemp_INVALID;
         IRTemp oldFPSCR = newTemp(Ity_I32);
         IRTemp newFPSCR = newTemp(Ity_I32);

         /* This is where the fun starts.  We have to convert 'irRes'
            from an IR-convention return result (IRCmpF64Result) to an
            ARM-encoded (N,Z,C,V) group.  The final result is in the
            bottom 4 bits of 'nzcv'. */
         /* Map compare result from IR to ARM(nzcv) */
         /*
            FP cmp result | IR   | ARM(nzcv)
            --------------------------------
            UN              0x45   0011
            LT              0x01   1000
            GT              0x00   0010
            EQ              0x40   0110
         */
         nzcv = mk_convert_IRCmpF64Result_to_NZCV(irRes);

         /* And update FPSCR accordingly */
         assign(oldFPSCR, IRExpr_Get(OFFB_FPSCR, Ity_I32));
         assign(newFPSCR, 
                binop(Iop_Or32, 
                      binop(Iop_And32, mkexpr(oldFPSCR), mkU32(0x0FFFFFFF)),
                      binop(Iop_Shl32, mkexpr(nzcv), mkU8(28))));

         putMiscReg32(OFFB_FPSCR, mkexpr(newFPSCR), condT);

         if (bZ) {
            DIP("fcmpz%sd%s d%u\n", bN ? "e" : "", nCC(INSN_COND), dD);
         } else {
            DIP("fcmp%sd%s d%u, d%u\n", bN ? "e" : "", nCC(INSN_COND), dD, dM);
         }
         goto decode_success;
      }
      /* fall through */
   }  

   /* --------------------- unary (D) --------------------- */
   if (BITS8(1,1,1,0,1,0,1,1) == INSN(27,20)
       && BITS4(0,0,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,1,1))) {
      UInt dD  = INSN(15,12);
      UInt dM  = INSN(3,0);
      UInt b16 = (insn >> 16) & 1;
      UInt b7  = (insn >> 7) & 1;
      /**/ if (b16 == 0 && b7 == 0) {
         // FCPYD
         putDReg(dD, getDReg(dM), condT);
         DIP("fcpyd%s d%u, d%u\n", nCC(INSN_COND), dD, dM);
         goto decode_success;
      }
      else if (b16 == 0 && b7 == 1) {
         // FABSD
         putDReg(dD, unop(Iop_AbsF64, getDReg(dM)), condT);
         DIP("fabsd%s d%u, d%u\n", nCC(INSN_COND), dD, dM);
         goto decode_success;
      }
      else if (b16 == 1 && b7 == 0) {
         // FNEGD
         putDReg(dD, unop(Iop_NegF64, getDReg(dM)), condT);
         DIP("fnegd%s d%u, d%u\n", nCC(INSN_COND), dD, dM);
         goto decode_success;
      }
      else if (b16 == 1 && b7 == 1) {
         // FSQRTD
         IRExpr* rm = get_FAKE_roundingmode(); /* XXXROUNDINGFIXME */
         putDReg(dD, binop(Iop_SqrtF64, rm, getDReg(dM)), condT);
         DIP("fsqrtd%s d%u, d%u\n", nCC(INSN_COND), dD, dM);
         goto decode_success;
      }
      else
         vassert(0);

      /* fall through */
   }

   /* ----------------- I <-> D conversions ----------------- */

   // F{S,U}ITOD dD, fM
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,1,1,1))
       && BITS4(1,0,0,0) == (INSN(19,16) & BITS4(1,1,1,1))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt bM    = (insn >> 5) & 1;
      UInt fM    = (INSN(3,0) << 1) | bM;
      UInt dD    = INSN(15,12);
      UInt syned = (insn >> 7) & 1;
      if (syned) {
         // FSITOD
         putDReg(dD, unop(Iop_I32StoF64,
                          unop(Iop_ReinterpF32asI32, getFReg(fM))),
                 condT);
         DIP("fsitod%s d%u, s%u\n", nCC(INSN_COND), dD, fM);
      } else {
         // FUITOD
         putDReg(dD, unop(Iop_I32UtoF64,
                          unop(Iop_ReinterpF32asI32, getFReg(fM))),
                 condT);
         DIP("fuitod%s d%u, s%u\n", nCC(INSN_COND), dD, fM);
      }
      goto decode_success;
   }

   // FTO{S,U}ID fD, dM
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(1,1,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,1,1))) {
      UInt   bD    = (insn >> 22) & 1;
      UInt   fD    = (INSN(15,12) << 1) | bD;
      UInt   dM    = INSN(3,0);
      UInt   bZ    = (insn >> 7) & 1;
      UInt   syned = (insn >> 16) & 1;
      IRTemp rmode = newTemp(Ity_I32);
      assign(rmode, bZ ? mkU32(Irrm_ZERO)
                       : mkexpr(mk_get_IR_rounding_mode()));
      if (syned) {
         // FTOSID
         putFReg(fD, unop(Iop_ReinterpI32asF32,
                          binop(Iop_F64toI32S, mkexpr(rmode),
                                getDReg(dM))),
                 condT);
         DIP("ftosi%sd%s s%u, d%u\n", bZ ? "z" : "",
             nCC(INSN_COND), fD, dM);
      } else {
         // FTOUID
         putFReg(fD, unop(Iop_ReinterpI32asF32,
                          binop(Iop_F64toI32U, mkexpr(rmode),
                                getDReg(dM))),
                 condT);
         DIP("ftoui%sd%s s%u, d%u\n", bZ ? "z" : "",
             nCC(INSN_COND), fD, dM);
      }
      goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- VFP instructions -- single precision                  -- */
   /* ----------------------------------------------------------- */

   /* --------------------- fldms, fstms --------------------- */
   /*
                                 31   27   23   19 15 11   7   0
                                         P UDWL
      C4-98, C5-26   1  FSTMD    cond 1100 1x00 Rn Fd 1010 offset
      C4-98, C5-28   2  FSTMDIA  cond 1100 1x10 Rn Fd 1010 offset
      C4-98, C5-30   3  FSTMDDB  cond 1101 0x10 Rn Fd 1010 offset

      C4-40, C5-26   1  FLDMD    cond 1100 1x01 Rn Fd 1010 offset
      C4-40, C5-26   2  FLDMIAD  cond 1100 1x11 Rn Fd 1010 offset
      C4-40, C5-26   3  FLDMDBD  cond 1101 0x11 Rn Fd 1010 offset

      Regs transferred: F(Fd:D) .. F(Fd:d + offset)
      offset must not imply a reg > 15
      IA/DB: Rn is changed by (4 x # regs transferred)

      case coding:
         1  at-Rn   (access at Rn)
         2  ia-Rn   (access at Rn, then Rn += 4n)
         3  db-Rn   (Rn -= 4n,     then access at Rn)
   */
   if (BITS8(1,1,0,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,0,0,0,0,0))
       && INSN(11,8) == BITS4(1,0,1,0)) {
      UInt bP     = (insn >> 24) & 1;
      UInt bU     = (insn >> 23) & 1;
      UInt bW     = (insn >> 21) & 1;
      UInt bL     = (insn >> 20) & 1;
      UInt bD     = (insn >> 22) & 1;
      UInt offset = (insn >> 0) & 0xFF;
      UInt rN     = INSN(19,16);
      UInt fD     = (INSN(15,12) << 1) | bD;
      UInt nRegs  = offset;
      Int  i;

      /**/ if (bP == 0 && bU == 1 && bW == 0) {
         vassert(0); //ATC
         summary = 1;
      }
      else if (bP == 0 && bU == 1 && bW == 1) {
         summary = 2;
      }
      else if (bP == 1 && bU == 0 && bW == 1) {
         summary = 3;
      }
      else goto after_vfp_fldms_fstms;

      /* no writebacks to r15 allowed */
      if (rN == 15 && (summary == 2 || summary == 3))
         goto after_vfp_fldms_fstms;

      /* offset must specify at least one register */
      if (offset < 1)
         goto after_vfp_fldms_fstms;

      /* can't transfer regs after S31 */
      if (fD + nRegs - 1 >= 32)
         goto after_vfp_fldms_fstms;

      /* Now, we can't do a conditional load or store, since that very
         likely will generate an exception.  So we have to take a side
         exit at this point if the condition is false. */
      if (condT != IRTemp_INVALID) {
         mk_skip_to_next_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      /* Ok, now we're unconditional.  Do the load or store. */

      /* get the old Rn value */
      IRTemp rnT = newTemp(Ity_I32);
      assign(rnT, getIReg(rN));

      /* make a new value for Rn, post-insn */
      IRTemp rnTnew = IRTemp_INVALID;
      if (summary == 2 || summary == 3) {
         rnTnew = newTemp(Ity_I32);
         assign(rnTnew, binop(summary == 2 ? Iop_Add32 : Iop_Sub32,
                              mkexpr(rnT),
                              mkU32(4 * nRegs)));
      }

      /* decide on the base transfer address */
      IRTemp taT = newTemp(Ity_I32);
      assign(taT, summary == 3 ? mkexpr(rnTnew) : mkexpr(rnT));

      /* update Rn if necessary -- in case 3, we're moving it down, so
         update before any memory reference, in order to keep Memcheck
         and V's stack-extending logic (on linux) happy */
      if (summary == 3)
         putIReg(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);

      /* generate the transfers */
      for (i = 0; i < nRegs; i++) {
         IRExpr* addr = binop(Iop_Add32, mkexpr(taT), mkU32(4*i));
         if (bL) {
            putFReg(fD + i, loadLE(Ity_F32, addr), IRTemp_INVALID);
         } else {
            storeLE(addr, getFReg(fD + i));
         }
      }

      /* update Rn if necessary -- in case 2, we're moving it up, so
         update after any memory reference, in order to keep Memcheck
         and V's stack-extending logic (on linux) happy */
      if (summary == 2)
         putIReg(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);

      HChar* nm = bL==1 ? "ld" : "st";
      switch (summary) {
         case 1:  DIP("f%sms%s r%u, {s%u-s%u}\n", 
                      nm, nCC(INSN_COND), rN, fD, fD + nRegs - 1);
                  break;
         case 2:  DIP("f%smias%s r%u!, {s%u-s%u}\n", 
                      nm, nCC(INSN_COND), rN, fD, fD + nRegs - 1);
                  break;
         case 3:  DIP("f%smdbs%s r%u!, {s%u-s%u}\n", 
                      nm, nCC(INSN_COND), rN, fD, fD + nRegs - 1);
                  break;
         default: vassert(0);
      }

      goto decode_success;
      /* FIXME alignment constraints? */
   }

  after_vfp_fldms_fstms:

   /* --------------------- fmsr, fmrs --------------------- */
   if (BITS8(1,1,1,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,1,1,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,0,0,0) == INSN(3,0)
       && BITS4(0,0,0,1) == (INSN(7,4) & BITS4(0,1,1,1))) {
      UInt rD  = INSN(15,12);
      UInt b7  = (insn >> 7) & 1;
      UInt fN  = (INSN(19,16) << 1) | b7;
      UInt b20 = (insn >> 20) & 1;
      if (rD == 15) {
         /* fall through */
         /* Let's assume that no sane person would want to do
            floating-point transfers to or from the program counter,
            and simply decline to decode the instruction.  The ARM ARM
            doesn't seem to explicitly disallow this case, though. */
      } else {
         if (b20) {
            putIReg(rD, unop(Iop_ReinterpF32asI32, getFReg(fN)),
                        condT, Ijk_Boring);
            DIP("fmrs%s r%u, s%u\n", nCC(INSN_COND), rD, fN);
         } else {
            putFReg(fN, unop(Iop_ReinterpI32asF32, getIReg(rD)), condT);
            DIP("fmsr%s s%u, r%u\n", nCC(INSN_COND), fN, rD);
         }
         goto decode_success;
      }
      /* fall through */
   }

   /* --------------------- f{ld,st}s --------------------- */
   // FLDS, FSTS
   if (BITS8(1,1,0,1,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,0,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)) {
      UInt bD     = (insn >> 22) & 1;
      UInt fD     = (INSN(15,12) << 1) | bD;
      UInt rN     = INSN(19,16);
      UInt offset = (insn & 0xFF) << 2;
      UInt bU     = (insn >> 23) & 1; /* 1: +offset  0: -offset */
      UInt bL     = (insn >> 20) & 1; /* 1: load  0: store */
      /* make unconditional */
      if (condT != IRTemp_INVALID) {
         mk_skip_to_next_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      IRTemp ea = newTemp(Ity_I32);
      assign(ea, binop(bU ? Iop_Add32 : Iop_Sub32,
                       getIReg(rN), mkU32(offset)));
      if (bL) {
         putFReg(fD, loadLE(Ity_F32,mkexpr(ea)), IRTemp_INVALID);
      } else {
         storeLE(mkexpr(ea), getFReg(fD));
      }
      DIP("f%ss%s s%u, [r%u, %c#%u]\n",
          bL ? "ld" : "st", nCC(INSN_COND), fD, rN,
          bU ? '+' : '-', offset);
      goto decode_success;
   }

   /* --------------------- dp insns (F) --------------------- */
   if (BITS8(1,1,1,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,0,0,0))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,0,0,0) == (INSN(7,4) & BITS4(0,0,0,1))) {
      UInt    bM  = (insn >> 5) & 1;
      UInt    bD  = (insn >> 22) & 1;
      UInt    bN  = (insn >> 7) & 1;
      UInt    fM  = (INSN(3,0) << 1) | bM;   /* argR */
      UInt    fD  = (INSN(15,12) << 1) | bD; /* dst/acc */
      UInt    fN  = (INSN(19,16) << 1) | bN; /* argL */
      UInt    bP  = (insn >> 23) & 1;
      UInt    bQ  = (insn >> 21) & 1;
      UInt    bR  = (insn >> 20) & 1;
      UInt    bS  = (insn >> 6) & 1;
      UInt    opc = (bP << 3) | (bQ << 2) | (bR << 1) | bS;
      IRExpr* rm  = get_FAKE_roundingmode(); /* XXXROUNDINGFIXME */
      switch (opc) {
         case BITS4(0,0,0,0): /* MAC: d + n * m */
            putFReg(fD, triop(Iop_AddF32, rm,
                              getFReg(fD),
                              triop(Iop_MulF32, rm, getFReg(fN), getFReg(fM))),
                        condT);
            DIP("fmacs%s s%u, s%u, s%u\n", nCC(INSN_COND), fD, fN, fM);
            goto decode_success;
         case BITS4(0,0,0,1): /* NMAC: d - n * m */
            putFReg(fD, triop(Iop_SubF32, rm,
                              getFReg(fD),
                              triop(Iop_MulF32, rm, getFReg(fN), getFReg(fM))),
                        condT);
            DIP("fnmacs%s s%u, s%u, s%u\n", nCC(INSN_COND), fD, fN, fM);
            goto decode_success;
         case BITS4(0,0,1,0): /* MSC: - d + n * m */
            putFReg(fD, triop(Iop_AddF32, rm,
                              unop(Iop_NegF32, getFReg(fD)),
                              triop(Iop_MulF32, rm, getFReg(fN), getFReg(fM))),
                        condT);
            DIP("fmscs%s s%u, s%u, s%u\n", nCC(INSN_COND), fD, fN, fM);
            goto decode_success;
         case BITS4(0,0,1,1): /* NMSC: - d - n * m */
            break; //ATC
         case BITS4(0,1,0,0): /* MUL: n * m */
            putFReg(fD, triop(Iop_MulF32, rm, getFReg(fN), getFReg(fM)),
                        condT);
            DIP("fmuls%s s%u, s%u, s%u\n", nCC(INSN_COND), fD, fN, fM);
            goto decode_success;
         case BITS4(0,1,0,1): /* NMUL: - n * m */
            putFReg(fD, unop(Iop_NegF32,
                             triop(Iop_MulF32, rm, getFReg(fN),
                                                   getFReg(fM))),
                    condT);
            DIP("fnmuls%s s%u, s%u, s%u\n", nCC(INSN_COND), fD, fN, fM);
            goto decode_success;
         case BITS4(0,1,1,0): /* ADD: n + m */
            putFReg(fD, triop(Iop_AddF32, rm, getFReg(fN), getFReg(fM)),
                        condT);
            DIP("fadds%s s%u, s%u, s%u\n", nCC(INSN_COND), fD, fN, fM);
            goto decode_success;
         case BITS4(0,1,1,1): /* SUB: n - m */
            putFReg(fD, triop(Iop_SubF32, rm, getFReg(fN), getFReg(fM)),
                        condT);
            DIP("fsubs%s s%u, s%u, s%u\n", nCC(INSN_COND), fD, fN, fM);
            goto decode_success;
         case BITS4(1,0,0,0): /* DIV: n / m */
            putFReg(fD, triop(Iop_DivF32, rm, getFReg(fN), getFReg(fM)),
                        condT);
            DIP("fdivs%s s%u, s%u, s%u\n", nCC(INSN_COND), fD, fN, fM);
            goto decode_success;
         default:
            break;
      }
   }

   /* --------------------- compares (S) --------------------- */
   /*          31   27   23   19   15 11   7    3
                 28   24   20   16 12    8    4    0 
      FCMPS    cond 1110 1D11 0100 Fd 1010 01M0 Fm
      FCMPES   cond 1110 1D11 0100 Fd 1010 11M0 Fm
      FCMPZS   cond 1110 1D11 0101 Fd 1010 0100 0000
      FCMPZED  cond 1110 1D11 0101 Fd 1010 1100 0000
                                 Z         N

      Z=0 Compare Fd:D vs Fm:M     and set FPSCR 31:28 accordingly
      Z=1 Compare Fd:D vs zero

      N=1 generates Invalid Operation exn if either arg is any kind of NaN
      N=0 generates Invalid Operation exn if either arg is a signalling NaN
      (Not that we pay any attention to N here)
   */
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,1,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt bZ = (insn >> 16) & 1;
      UInt bN = (insn >> 7) & 1;
      UInt bD = (insn >> 22) & 1;
      UInt bM = (insn >> 5) & 1;
      UInt fD = (INSN(15,12) << 1) | bD;
      UInt fM = (INSN(3,0) << 1) | bM;
      if (bZ && (INSN(3,0) != 0 || (INSN(7,4) & 3) != 0)) {
         /* does not decode; fall through */
      } else {
         IRTemp argL = newTemp(Ity_F64);
         IRTemp argR = newTemp(Ity_F64);
         IRTemp irRes = newTemp(Ity_I32);

         assign(argL, unop(Iop_F32toF64, getFReg(fD)));
         assign(argR, bZ ? IRExpr_Const(IRConst_F64i(0))
                         : unop(Iop_F32toF64, getFReg(fM)));
         assign(irRes, binop(Iop_CmpF64, mkexpr(argL), mkexpr(argR)));

         IRTemp nzcv     = IRTemp_INVALID;
         IRTemp oldFPSCR = newTemp(Ity_I32);
         IRTemp newFPSCR = newTemp(Ity_I32);

         /* This is where the fun starts.  We have to convert 'irRes'
            from an IR-convention return result (IRCmpF64Result) to an
            ARM-encoded (N,Z,C,V) group.  The final result is in the
            bottom 4 bits of 'nzcv'. */
         /* Map compare result from IR to ARM(nzcv) */
         /*
            FP cmp result | IR   | ARM(nzcv)
            --------------------------------
            UN              0x45   0011
            LT              0x01   1000
            GT              0x00   0010
            EQ              0x40   0110
         */
         nzcv = mk_convert_IRCmpF64Result_to_NZCV(irRes);

         /* And update FPSCR accordingly */
         assign(oldFPSCR, IRExpr_Get(OFFB_FPSCR, Ity_I32));
         assign(newFPSCR, 
                binop(Iop_Or32, 
                      binop(Iop_And32, mkexpr(oldFPSCR), mkU32(0x0FFFFFFF)),
                      binop(Iop_Shl32, mkexpr(nzcv), mkU8(28))));

         putMiscReg32(OFFB_FPSCR, mkexpr(newFPSCR), condT);

         if (bZ) {
            DIP("fcmpz%ss%s s%u\n", bN ? "e" : "", nCC(INSN_COND), fD);
         } else {
            DIP("fcmp%ss%s s%u, s%u\n", bN ? "e" : "",
                nCC(INSN_COND), fD, fM);
         }
         goto decode_success;
      }
      /* fall through */
   }  

   /* --------------------- unary (S) --------------------- */
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,0,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt bD = (insn >> 22) & 1;
      UInt bM = (insn >> 5) & 1;
      UInt fD  = (INSN(15,12) << 1) | bD;
      UInt fM  = (INSN(3,0) << 1) | bM;
      UInt b16 = (insn >> 16) & 1;
      UInt b7  = (insn >> 7) & 1;
      /**/ if (b16 == 0 && b7 == 0) {
         // FCPYS
         putFReg(fD, getFReg(fM), condT);
         DIP("fcpys%s s%u, s%u\n", nCC(INSN_COND), fD, fM);
         goto decode_success;
      }
      else if (b16 == 0 && b7 == 1) {
         // FABSS
         putFReg(fD, unop(Iop_AbsF32, getFReg(fM)), condT);
         DIP("fabss%s s%u, s%u\n", nCC(INSN_COND), fD, fM);
         goto decode_success;
      }
      else if (b16 == 1 && b7 == 0) {
         // FNEGS
         putFReg(fD, unop(Iop_NegF32, getFReg(fM)), condT);
         DIP("fnegs%s s%u, s%u\n", nCC(INSN_COND), fD, fM);
         goto decode_success;
      }
      else if (b16 == 1 && b7 == 1) {
         // FSQRTS
         IRExpr* rm = get_FAKE_roundingmode(); /* XXXROUNDINGFIXME */
         putFReg(fD, binop(Iop_SqrtF32, rm, getFReg(fM)), condT);
         DIP("fsqrts%s s%u, s%u\n", nCC(INSN_COND), fD, fM);
         goto decode_success;
      }
      else
         vassert(0);

      /* fall through */
   }

   /* ----------------- I <-> S conversions ----------------- */

   // F{S,U}ITOS fD, fM
   /* These are more complex than FSITOD/FUITOD.  In the D cases, a 32
      bit int will always fit within the 53 bit mantissa, so there's
      no possibility of a loss of precision, but that's obviously not
      the case here.  Hence this case possibly requires rounding, and
      so it drags in the current rounding mode. */
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(1,0,0,0) == (INSN(19,16) & BITS4(1,1,1,1))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt bM    = (insn >> 5) & 1;
      UInt bD    = (insn >> 22) & 1;
      UInt fM    = (INSN(3,0) << 1) | bM;
      UInt fD    = (INSN(15,12) << 1) | bD;
      UInt syned = (insn >> 7) & 1;
      IRTemp rmode = newTemp(Ity_I32);
      assign(rmode, mkexpr(mk_get_IR_rounding_mode()));
      if (syned) {
         // FSITOS
         putFReg(fD, binop(Iop_F64toF32,
                           mkexpr(rmode),
                           unop(Iop_I32StoF64,
                                unop(Iop_ReinterpF32asI32, getFReg(fM)))),
                 condT);
         DIP("fsitos%s s%u, s%u\n", nCC(INSN_COND), fD, fM);
      } else {
         // FUITOS
         putFReg(fD, binop(Iop_F64toF32,
                           mkexpr(rmode),
                           unop(Iop_I32UtoF64,
                                unop(Iop_ReinterpF32asI32, getFReg(fM)))),
                 condT);
         DIP("fuitos%s s%u, s%u\n", nCC(INSN_COND), fD, fM);
      }
      goto decode_success;
   }

   // FTO{S,U}IS fD, fM
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(1,1,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt   bM    = (insn >> 5) & 1;
      UInt   bD    = (insn >> 22) & 1;
      UInt   fD    = (INSN(15,12) << 1) | bD;
      UInt   fM    = (INSN(3,0) << 1) | bM;
      UInt   bZ    = (insn >> 7) & 1;
      UInt   syned = (insn >> 16) & 1;
      IRTemp rmode = newTemp(Ity_I32);
      assign(rmode, bZ ? mkU32(Irrm_ZERO)
                       : mkexpr(mk_get_IR_rounding_mode()));
      if (syned) {
         // FTOSIS
         putFReg(fD, unop(Iop_ReinterpI32asF32,
                          binop(Iop_F64toI32S, mkexpr(rmode),
                                unop(Iop_F32toF64, getFReg(fM)))),
                 condT);
         DIP("ftosi%ss%s s%u, d%u\n", bZ ? "z" : "",
             nCC(INSN_COND), fD, fM);
         goto decode_success;
      } else {
         // FTOUIS
         putFReg(fD, unop(Iop_ReinterpI32asF32,
                          binop(Iop_F64toI32U, mkexpr(rmode),
                                unop(Iop_F32toF64, getFReg(fM)))),
                 condT);
         DIP("ftoui%ss%s s%u, d%u\n", bZ ? "z" : "",
             nCC(INSN_COND), fD, fM);
         goto decode_success;
      }
   }

   /* ----------------- S <-> D conversions ----------------- */

   // FCVTDS
   if (BITS8(1,1,1,0,1,0,1,1) == INSN(27,20)
       && BITS4(0,1,1,1) == INSN(19,16)
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(1,1,0,0) == (INSN(7,4) & BITS4(1,1,0,1))) {
      UInt dD = INSN(15,12);
      UInt bM = (insn >> 5) & 1;
      UInt fM = (INSN(3,0) << 1) | bM;
      putDReg(dD, unop(Iop_F32toF64, getFReg(fM)), condT);
      DIP("fcvtds%s d%u, s%u\n", nCC(INSN_COND), dD, fM);
      goto decode_success;
   }

   // FCVTSD
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,1,1,1) == INSN(19,16)
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(1,1,0,0) == INSN(7,4)) {
      UInt   bD    = (insn >> 22) & 1;
      UInt   fD    = (INSN(15,12) << 1) | bD;
      UInt   dM    = INSN(3,0);
      IRTemp rmode = newTemp(Ity_I32);
      assign(rmode, mkexpr(mk_get_IR_rounding_mode()));
      putFReg(fD, binop(Iop_F64toF32, mkexpr(rmode), getDReg(dM)),
                  condT);
      DIP("fcvtsd%s s%u, d%u\n", nCC(INSN_COND), fD, dM);
      goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- ARMv6 instructions                                    -- */
   /* ----------------------------------------------------------- */

   /* --------------------- ldrex, strex --------------------- */

   // LDREX
   if (0x01900F9F == (insn & 0x0FF00FFF)) {
      UInt rT = INSN(15,12);
      UInt rN = INSN(19,16);
      if (rT == 15 || rN == 15 || rT == 14 /* || (rT & 1)*/) {
         /* undecodable; fall through */
      } else {
         IRTemp res;
         /* make unconditional */
         if (condT != IRTemp_INVALID) {
            mk_skip_to_next_if_cond_is_false( condT );
            condT = IRTemp_INVALID;
         }
         /* Ok, now we're unconditional.  Do the load. */
         res = newTemp(Ity_I32);
         stmt( IRStmt_LLSC(Iend_LE, res, getIReg(rN), NULL/*this is a load*/) );
         putIReg(rT, mkexpr(res), IRTemp_INVALID, Ijk_Boring);
         DIP("ldrex%s r%u, [r%u]\n", nCC(INSN_COND), rT, rN);
         goto decode_success;
      }
      /* fall through */
   }

   // STREX
   if (0x01800F90 == (insn & 0x0FF00FF0)) {
      UInt rT = INSN(3,0);
      UInt rN = INSN(19,16);
      UInt rD = INSN(15,12);
      if (rT == 15 || rN == 15 || rD == 15
          || rT == 14 /* || (rT & 1)*/
          || rD == rT || rN == rT) {
         /* undecodable; fall through */
      } else {
         IRTemp resSC1, resSC32;

         /* make unconditional */
         if (condT != IRTemp_INVALID) {
            mk_skip_to_next_if_cond_is_false( condT );
            condT = IRTemp_INVALID;
         }

         /* Ok, now we're unconditional.  Do the store. */
         resSC1 = newTemp(Ity_I1);
         stmt( IRStmt_LLSC(Iend_LE, resSC1, getIReg(rN), getIReg(rT)) );

         /* Set rD to 1 on failure, 0 on success.  Currently we have
            resSC1 == 0 on failure, 1 on success. */
         resSC32 = newTemp(Ity_I32);
         assign(resSC32,
                unop(Iop_1Uto32, unop(Iop_Not1, mkexpr(resSC1))));

         putIReg(rD, mkexpr(resSC32),
                     IRTemp_INVALID, Ijk_Boring);
         DIP("strex%s r%u, r%u, [r%u]\n", nCC(INSN_COND), rD, rT, rN);
         goto decode_success;
      }
      /* fall through */
   }

   /* --------------------- movw, movt --------------------- */
   if (0x03000000 == (insn & 0x0FF00000)
       || 0x03400000 == (insn & 0x0FF00000)) /* pray for CSE */ {
      UInt rD    = INSN(15,12);
      UInt imm16 = (insn & 0xFFF) | ((insn >> 4) & 0x0000F000);
      UInt isT   = (insn >> 22) & 1;
      if (rD == 15) {
         /* forget it */
      } else {
         if (isT) {
            putIReg(rD,
                    binop(Iop_Or32,
                          binop(Iop_And32, getIReg(rD), mkU32(0xFFFF)),
                          mkU32(imm16 << 16)),
                    condT, Ijk_Boring);
            DIP("movt%s r%u, #0x%04x\n", nCC(INSN_COND), rD, imm16);
            goto decode_success;
         } else {
            putIReg(rD, mkU32(imm16), condT, Ijk_Boring);
            DIP("movw%s r%u, #0x%04x\n", nCC(INSN_COND), rD, imm16);
            goto decode_success;
         }
      }
      /* fall through */
   }

   /* ------------------- {u,s}xt{b,h}{,16} ------------------- */
   if (BITS8(0,1,1,0,1, 0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,0,0))
       && BITS4(1,1,1,1) == INSN(19,16)
       && BITS4(0,1,1,1) == INSN(7,4)
       && BITS4(0,0, 0,0) == (INSN(11,8) & BITS4(0,0,1,1))) {
      UInt subopc = INSN(27,20) & BITS8(0,0,0,0,0, 1,1,1);
      if (subopc != BITS4(0,0,0,1) && subopc != BITS4(0,1,0,1)) {
         Int    rot  = (INSN(11,8) >> 2) & 3;
         UInt   rM   = INSN(3,0);
         UInt   rD   = INSN(15,12);
         IRTemp srcT = newTemp(Ity_I32);
         IRTemp rotT = newTemp(Ity_I32);
         IRTemp dstT = newTemp(Ity_I32);
         HChar* nm   = "???";
         assign(srcT, getIReg(rM));
         assign(rotT, genROR32(srcT, 8 * rot)); /* 0, 8, 16 or 24 only */
         switch (subopc) {
            case BITS4(0,1,1,0): // UXTB
               assign(dstT, unop(Iop_8Uto32, unop(Iop_32to8, mkexpr(rotT))));
               nm = "uxtb";
               break;
            case BITS4(0,0,1,0): // SXTB
               assign(dstT, unop(Iop_8Sto32, unop(Iop_32to8, mkexpr(rotT))));
               nm = "sxtb";
               break;
            case BITS4(0,1,1,1): // UXTH
               assign(dstT, unop(Iop_16Uto32, unop(Iop_32to16, mkexpr(rotT))));
               nm = "uxth";
               break;
            case BITS4(0,0,1,1): // SXTH
               assign(dstT, unop(Iop_16Sto32, unop(Iop_32to16, mkexpr(rotT))));
               nm = "sxth";
               break;
            case BITS4(0,1,0,0): // UXTB16
               assign(dstT, binop(Iop_And32, mkexpr(rotT), mkU32(0x00FF00FF)));
               nm = "uxtb16";
               break;
            case BITS4(0,0,0,0): { // SXTB16
               IRTemp lo32 = newTemp(Ity_I32);
               IRTemp hi32 = newTemp(Ity_I32);
               assign(lo32, binop(Iop_And32, mkexpr(rotT), mkU32(0xFF)));
               assign(hi32, binop(Iop_Shr32, mkexpr(rotT), mkU8(16)));
               assign(
                  dstT,
                  binop(Iop_Or32,
                        binop(Iop_And32,
                              unop(Iop_8Sto32,
                                   unop(Iop_32to8, mkexpr(lo32))),
                              mkU32(0xFFFF)),
                        binop(Iop_Shl32,
                              unop(Iop_8Sto32,
                                   unop(Iop_32to8, mkexpr(hi32))),
                              mkU8(16))
               ));
               nm = "uxtb16";
               break;
            }
            default:
               vassert(0); // guarded by "if" above
         }
         putIReg(rD, mkexpr(dstT), condT, Ijk_Boring);
         DIP("%s%s r%u, r%u, ROR #%u\n", nm, nCC(INSN_COND), rD, rM, rot);
         goto decode_success;
      }
      /* fall through */
   }

   /* ------------------- bfi, bfc ------------------- */
   if (BITS8(0,1,1,1,1,1,0, 0) == (INSN(27,20) & BITS8(1,1,1,1,1,1,1,0))
       && BITS4(0, 0,0,1) == (INSN(7,4) & BITS4(0,1,1,1))) {
      UInt rD  = INSN(15,12);
      UInt rN  = INSN(3,0);
      UInt msb = (insn >> 16) & 0x1F; /* 20:16 */
      UInt lsb = (insn >> 7) & 0x1F;  /* 11:7 */
      if (rD == 15 || msb < lsb) {
         /* undecodable; fall through */
      } else {
         IRTemp src    = newTemp(Ity_I32);
         IRTemp olddst = newTemp(Ity_I32);
         IRTemp newdst = newTemp(Ity_I32);
         UInt   mask = 1 << (msb - lsb);
         mask = (mask - 1) + mask;
         vassert(mask != 0); // guaranteed by "msb < lsb" check above
         mask <<= lsb;

         assign(src, rN == 15 ? mkU32(0) : getIReg(rN));
         assign(olddst, getIReg(rD));
         assign(newdst,
                binop(Iop_Or32,
                   binop(Iop_And32,
                         binop(Iop_Shl32, mkexpr(src), mkU8(lsb)), 
                         mkU32(mask)),
                   binop(Iop_And32,
                         mkexpr(olddst),
                         mkU32(~mask)))
               );

         putIReg(rD, mkexpr(newdst), condT, Ijk_Boring);

         if (rN == 15) {
            DIP("bfc%s r%u, #%u, #%u\n",
                nCC(INSN_COND), rD, lsb, msb-lsb+1);
         } else {
            DIP("bfi%s r%u, r%u, #%u, #%u\n",
                nCC(INSN_COND), rD, rN, lsb, msb-lsb+1);
         }
         goto decode_success;
      }
      /* fall through */
   }

   /* ------------------- {u,s}bfx ------------------- */
   if (BITS8(0,1,1,1,1,0,1,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,0))
       && BITS4(0,1,0,1) == (INSN(7,4) & BITS4(0,1,1,1))) {
      UInt rD  = INSN(15,12);
      UInt rN  = INSN(3,0);
      UInt wm1 = (insn >> 16) & 0x1F; /* 20:16 */
      UInt lsb = (insn >> 7) & 0x1F;  /* 11:7 */
      UInt msb = lsb + wm1;
      UInt isU = (insn >> 22) & 1;    /* 22:22 */
      if (rD == 15 || rN == 15 || msb >= 32) {
         /* undecodable; fall through */
      } else {
         IRTemp src  = newTemp(Ity_I32);
         IRTemp tmp  = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         UInt   mask = ((1 << wm1) - 1) + (1 << wm1);
         vassert(msb >= 0 && msb <= 31);
         vassert(mask != 0); // guaranteed by msb being in 0 .. 31 inclusive

         assign(src, getIReg(rN));
         assign(tmp, binop(Iop_And32,
                           binop(Iop_Shr32, mkexpr(src), mkU8(lsb)),
                           mkU32(mask)));
         assign(res, binop(isU ? Iop_Shr32 : Iop_Sar32,
                           binop(Iop_Shl32, mkexpr(tmp), mkU8(31-wm1)),
                           mkU8(31-wm1)));

         putIReg(rD, mkexpr(res), condT, Ijk_Boring);

         DIP("%s%s r%u, r%u, #%u, #%u\n",
             isU ? "ubfx" : "sbfx",
             nCC(INSN_COND), rD, rN, lsb, wm1 + 1);
         goto decode_success;
      }
      /* fall through */
   }

   /* ------------------- smul{b,t}{b,t} ------------- */
   if (BITS8(0,0,0,1,0,1,1,0) == INSN(27,20)
       && BITS4(0,0,0,0) == INSN(15,12)
       && BITS4(1,0,0,0) == (INSN(7,4) & BITS4(1,0,0,1))) {
      UInt rD  = INSN(19,16);
      UInt rM  = INSN(11,8);
      UInt rN  = INSN(3,0);
      UInt bM = (insn >> 6) & 1;
      UInt bN = (insn >> 5) & 1;
      if (bN == 0 && bM == 1) goto decode_failure; //ATC
      if (bN == 1 && bM == 0) goto decode_failure; //ATC
      if (bN == 1 && bM == 1) goto decode_failure; //ATC
      if (rD == 15 || rN == 15 || rM == 15) {
         /* undecodable; fall through */
      } else {
         IRTemp srcL = newTemp(Ity_I32);
         IRTemp srcR = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);

         /* Extract and sign extend the two 16-bit operands */
         assign(srcL, binop(Iop_Sar32,
                            binop(Iop_Shl32, getIReg(rN),
                                             mkU8(bN ? 0 : 16)),
                            mkU8(16)));
         assign(srcR, binop(Iop_Sar32,
                            binop(Iop_Shl32, getIReg(rM),
                                             mkU8(bM ? 0 : 16)),
                            mkU8(16)));

         assign(res, binop(Iop_Mul32, mkexpr(srcL), mkexpr(srcR)));
         putIReg(rD, mkexpr(res), condT, Ijk_Boring);

         DIP("smul%c%c%s r%u, r%u, r%u\n",
             bN ? 't' : 'b', bM ? 't' : 'b', nCC(INSN_COND), rD, rN, rM);
         goto decode_success;
      }
      /* fall through */
   }

   /* --------------------- Load/store doubleword ------------- */
   // LDRD STRD
   /*                 31   27   23   19 15 11   7    3     # highest bit
                        28   24   20 16 12    8    4    0
      A5-36   1 | 16  cond 0001 U100 Rn Rd im4h 11S1 im4l
      A5-38   1 | 32  cond 0001 U000 Rn Rd 0000 11S1 Rm
      A5-40   2 | 16  cond 0001 U110 Rn Rd im4h 11S1 im4l
      A5-42   2 | 32  cond 0001 U010 Rn Rd 0000 11S1 Rm
      A5-44   3 | 16  cond 0000 U100 Rn Rd im4h 11S1 im4l
      A5-46   3 | 32  cond 0000 U000 Rn Rd 0000 11S1 Rm
   */
   /* case coding:
             1   at-ea               (access at ea)
             2   at-ea-then-upd      (access at ea, then Rn = ea)
             3   at-Rn-then-upd      (access at Rn, then Rn = ea)
      ea coding
             16  Rn +/- imm8
             32  Rn +/- Rm
   */
   /* Quickly skip over all of this for hopefully most instructions */
   if ((INSN(27,24) & BITS4(1,1,1,0)) != BITS4(0,0,0,0))
      goto after_load_store_doubleword;

   /* Check the "11S1" thing. */
   if ((INSN(7,4) & BITS4(1,1,0,1)) != BITS4(1,1,0,1))
      goto after_load_store_doubleword;

   summary = 0;

   /**/ if (INSN(27,24) == BITS4(0,0,0,1) && INSN(22,20) == BITS3(1,0,0)) {
      summary = 1 | 16;
   }
   else if (INSN(27,24) == BITS4(0,0,0,1) && INSN(22,20) == BITS3(0,0,0)) {
      summary = 1 | 32;
   }
   else if (INSN(27,24) == BITS4(0,0,0,1) && INSN(22,20) == BITS3(1,1,0)) {
      summary = 2 | 16;
   }
   else if (INSN(27,24) == BITS4(0,0,0,1) && INSN(22,20) == BITS3(0,1,0)) {
      summary = 2 | 32;
   }
   else if (INSN(27,24) == BITS4(0,0,0,0) && INSN(22,20) == BITS3(1,0,0)) {
      summary = 3 | 16;
   }
   else if (INSN(27,24) == BITS4(0,0,0,0) && INSN(22,20) == BITS3(0,0,0)) {
      summary = 3 | 32;
      goto decode_failure; //ATC
   }
   else goto after_load_store_doubleword;

   { UInt rN   = (insn >> 16) & 0xF; /* 19:16 */
     UInt rD   = (insn >> 12) & 0xF; /* 15:12 */
     UInt rM   = (insn >> 0)  & 0xF; /*  3:0  */
     UInt bU   = (insn >> 23) & 1;   /* 23 U=1 offset+, U=0 offset- */
     UInt bS   = (insn >> 5) & 1;    /* S=1 store, S=0 load */
     UInt imm8 = ((insn >> 4) & 0xF0) | (insn & 0xF); /* 11:8, 3:0 */

     /* Require rD to be an even numbered register */
     if ((rD & 1) != 0)
        goto after_load_store_doubleword;

     /* Require 11:8 == 0 for Rn +/- Rm cases */
     if ((summary & 32) != 0 && (imm8 & 0xF0) != 0)
        goto after_load_store_doubleword;

     /* Skip some invalid cases, which would lead to two competing
        updates to the same register, or which are otherwise
        disallowed by the spec. */
     switch (summary) {
        case 1 | 16:
           break;
        case 1 | 32: 
           if (rM == 15) goto after_load_store_doubleword;
           break;
        case 2 | 16: case 3 | 16:
           if (rN == 15) goto after_load_store_doubleword;
           if (bS == 0 && (rN == rD || rN == rD+1))
              goto after_load_store_doubleword;
           break;
        case 2 | 32: case 3 | 32:
           if (rM == 15) goto after_load_store_doubleword;
           if (rN == 15) goto after_load_store_doubleword;
           if (rN == rM) goto after_load_store_doubleword;
           if (bS == 0 && (rN == rD || rN == rD+1))
              goto after_load_store_doubleword;
           break;
        default:
           vassert(0);
     }

     /* Now, we can't do a conditional load or store, since that very
        likely will generate an exception.  So we have to take a side
        exit at this point if the condition is false. */
     if (condT != IRTemp_INVALID) {
        mk_skip_to_next_if_cond_is_false( condT );
        condT = IRTemp_INVALID;
     }
     /* Ok, now we're unconditional.  Do the load or store. */

     /* compute the effective address.  Bind it to a tmp since we
        may need to use it twice. */
     IRExpr* eaE = NULL;
     switch (summary & 0xF0) {
        case 16:
           eaE = mk_EA_reg_plusminus_imm8( rN, bU, imm8, dis_buf );
           break;
        case 32:
           eaE = mk_EA_reg_plusminus_reg( rN, bU, rM, dis_buf );
           break;
     }
     vassert(eaE);
     IRTemp eaT = newTemp(Ity_I32);
     assign(eaT, eaE);

     /* get the old Rn value */
     IRTemp rnT = newTemp(Ity_I32);
     assign(rnT, getIReg(rN));

     /* decide on the transfer address */
     IRTemp taT = IRTemp_INVALID;
     switch (summary & 0x0F) {
        case 1: case 2: taT = eaT; break;
        case 3:         taT = rnT; break;
     }
     vassert(taT != IRTemp_INVALID);

     /* XXX deal with alignment constraints */
     /* XXX: but the A8 doesn't seem to trap for misaligned loads, so,
        ignore alignment issues for the time being. */

     /* doubleword store  S 1
        doubleword load   S 0
     */
     HChar* name = NULL;
     /* generate the transfers */
     if (bS == 1) { // doubleword store
        storeLE( binop(Iop_Add32, mkexpr(taT), mkU32(0)), getIReg(rD+0) );
        storeLE( binop(Iop_Add32, mkexpr(taT), mkU32(4)), getIReg(rD+1) );
        name = "strd";
     } else { // doubleword load
        putIReg( rD+0,
                 loadLE(Ity_I32, binop(Iop_Add32, mkexpr(taT), mkU32(0))),
                 IRTemp_INVALID, Ijk_Boring );
        putIReg( rD+1,
                 loadLE(Ity_I32, binop(Iop_Add32, mkexpr(taT), mkU32(4))),
                 IRTemp_INVALID, Ijk_Boring );
        name = "ldrd";
     }

     /* Update Rn if necessary. */
     switch (summary & 0x0F) {
        case 2: case 3:
           // should be assured by logic above:
           if (bS == 0) {
              vassert(rD+0 != rN); /* since we just wrote rD+0 */
              vassert(rD+1 != rN); /* since we just wrote rD+1 */
           }
           putIReg( rN, mkexpr(eaT), IRTemp_INVALID, Ijk_Boring );
           break;
     }

     switch (summary & 0x0F) {
        case 1:  DIP("%s%s r%u, %s\n", name, nCC(INSN_COND), rD, dis_buf);
                 break;
        case 2:  DIP("%s%s r%u, %s! (at-EA-then-Rn=EA)\n",
                     name, nCC(INSN_COND), rD, dis_buf);
                 break;
        case 3:  DIP("%s%s r%u, %s! (at-Rn-then-Rn=EA)\n",
                     name, nCC(INSN_COND), rD, dis_buf);
                 break;
        default: vassert(0);
     }

     goto decode_success;
   }

  after_load_store_doubleword:

   /* ------------------- {s,u}xtab ------------- */
   if (BITS8(0,1,1,0,1,0,1,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,0,0,0) == (INSN(11,8) & BITS4(0,0,1,1))
       && BITS4(0,1,1,1) == INSN(7,4)) {
      UInt rN  = INSN(19,16);
      UInt rD  = INSN(15,12);
      UInt rM  = INSN(3,0);
      UInt rot = (insn >> 10) & 3;
      UInt isU = INSN(22,22);
      if (rN == 15/*it's {S,U}XTB*/ || rD == 15 || rM == 15) {
         /* undecodable; fall through */
      } else {
         IRTemp srcL = newTemp(Ity_I32);
         IRTemp srcR = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         assign(srcR, getIReg(rM));
         assign(srcL, getIReg(rN));
         assign(res,  binop(Iop_Add32,
                            mkexpr(srcL),
                            unop(isU ? Iop_8Uto32 : Iop_8Sto32,
                                 unop(Iop_32to8, 
                                      genROR32(srcR, 8 * rot)))));
         putIReg(rD, mkexpr(res), condT, Ijk_Boring);
         DIP("%cxtab%s r%u, r%u, r%u, ror #%u\n",
             isU ? 'u' : 's', nCC(INSN_COND), rD, rN, rM, rot);
         goto decode_success;
      }
      /* fall through */
   }

   /* ------------------- {s,u}xtah ------------- */
   if (BITS8(0,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,0,0,0) == (INSN(11,8) & BITS4(0,0,1,1))
       && BITS4(0,1,1,1) == INSN(7,4)) {
      UInt rN  = INSN(19,16);
      UInt rD  = INSN(15,12);
      UInt rM  = INSN(3,0);
      UInt rot = (insn >> 10) & 3;
      UInt isU = INSN(22,22);
      if (rN == 15/*it's {S,U}XTH*/ || rD == 15 || rM == 15) {
         /* undecodable; fall through */
      } else {
         IRTemp srcL = newTemp(Ity_I32);
         IRTemp srcR = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         assign(srcR, getIReg(rM));
         assign(srcL, getIReg(rN));
         assign(res,  binop(Iop_Add32,
                            mkexpr(srcL),
                            unop(isU ? Iop_16Uto32 : Iop_16Sto32,
                                 unop(Iop_32to16, 
                                      genROR32(srcR, 8 * rot)))));
         putIReg(rD, mkexpr(res), condT, Ijk_Boring);

         DIP("%cxtah%s r%u, r%u, r%u, ror #%u\n",
             isU ? 'u' : 's', nCC(INSN_COND), rD, rN, rM, rot);
         goto decode_success;
      }
      /* fall through */
   }

   /* ----------------------------------------------------------- */
   /* -- ARMv7 instructions                                    -- */
   /* ----------------------------------------------------------- */

   /* -------------- read CP15 TPIDRURO register ------------- */
   /* mrc     p15, 0, r0, c13, c0, 3  up to
      mrc     p15, 0, r14, c13, c0, 3
   */
   /* I don't know whether this is really v7-only.  But anyway, we
      have to support it since arm-linux uses TPIDRURO as a thread
      state register. */
   if (0x0E1D0F70 == (insn & 0x0FFF0FFF)) {
      UInt rD = INSN(15,12);
      if (rD <= 14) {
         /* skip r15, that's too stupid to handle */
         putIReg(rD, IRExpr_Get(OFFB_TPIDRURO, Ity_I32),
                     condT, Ijk_Boring);
         DIP("mrc%s p15,0, r%u, c13, c0, 3\n", nCC(INSN_COND), rD);
         goto decode_success;
      }
      /* fall through */
   }

   /* Handle various kinds of barriers.  This is rather indiscriminate
      in the sense that they are all turned into an IR Fence, which
      means we don't know which they are, so the back end has to
      re-emit them all when it comes acrosss an IR Fence.
   */
   switch (insn) {
      case 0xEE070F9A: /* v6 */
         /* mcr 15, 0, r0, c7, c10, 4 (v6) equiv to DSB (v7).  Data
            Synch Barrier -- ensures completion of memory accesses. */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("mcr 15, 0, r0, c7, c10, 4 (data synch barrier)\n");
         goto decode_success;
      case 0xEE070FBA: /* v6 */
         /* mcr 15, 0, r0, c7, c10, 5 (v6) equiv to DMB (v7).  Data
            Memory Barrier -- ensures ordering of memory accesses. */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("mcr 15, 0, r0, c7, c10, 5 (data memory barrier)\n");
         goto decode_success;
      case 0xEE070F95: /* v6 */
         /* mcr 15, 0, r0, c7, c5, 4 (v6) equiv to ISB (v7).
            Instruction Synchronisation Barrier (or Flush Prefetch
            Buffer) -- a pipe flush, I think.  I suspect we could
            ignore those, but to be on the safe side emit a fence
            anyway. */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("mcr 15, 0, r0, c7, c5, 4 (insn synch barrier)\n");
         goto decode_success;
      default:
         break;
   }

   /* ----------------------------------------------------------- */
   /* -- Undecodable                                           -- */
   /* ----------------------------------------------------------- */

   goto decode_failure;
   /*NOTREACHED*/

  decode_failure:
   /* All decode failures end up here. */
   vex_printf("disInstr(arm): unhandled instruction: "
              "0x%x\n", insn);
   vex_printf("                 cond=%d(0x%x) 27:20=%u(0x%02x) "
                                "4:4=%d "
                                "3:0=%u(0x%x)\n",
              (Int)INSN_COND, (UInt)INSN_COND,
              (Int)INSN(27,20), (UInt)INSN(27,20),
              (Int)INSN(4,4),
              (Int)INSN(3,0), (UInt)INSN(3,0) );

   /* Tell the dispatcher that this insn cannot be decoded, and so has
      not been executed, and (is currently) the next to be executed.
      R15 should be up-to-date since it made so at the start of each
      insn, but nevertheless be paranoid and update it again right
      now. */
   vassert(0 == (guest_R15_curr_instr & 3));
   llPutIReg( 15, mkU32(guest_R15_curr_instr) );
   irsb->next     = mkU32(guest_R15_curr_instr);
   irsb->jumpkind = Ijk_NoDecode;
   dres.whatNext  = Dis_StopHere;
   dres.len       = 0;
   return dres;

  decode_success:
   /* All decode successes end up here. */
   DIP("\n");

   vassert(dres.len == 4 || dres.len == 20);

   /* Now then.  Do we have an implicit jump to r15 to deal with? */
   if (r15written) {
      /* If we get jump to deal with, we assume that there's been no
         other competing branch stuff previously generated for this
         insn.  That's reasonable, in the sense that the ARM insn set
         appears to declare as "Unpredictable" any instruction which
         generates more than one possible new value for r15.  Hence
         just assert.  The decoders themselves should check against
         all such instructions which are thusly Unpredictable, and
         decline to decode them.  Hence we should never get here if we
         have competing new values for r15, and hence it is safe to
         assert here. */
      vassert(dres.whatNext == Dis_Continue);
      vassert(irsb->next == NULL);
      vassert(irsb->jumpkind = Ijk_Boring);
      /* If r15 is unconditionally written, terminate the block by
         jumping to it.  If it's conditionally written, still
         terminate the block (a shame, but we can't do side exits to
         arbitrary destinations), but first jump to the next
         instruction if the condition doesn't hold. */
      /* We can't use getIReg(15) to get the destination, since that
         will produce r15+8, which isn't what we want.  Must use
         llGetIReg(15) instead. */
      if (r15guard == IRTemp_INVALID) {
         /* unconditional */
      } else {
         /* conditional */
         stmt( IRStmt_Exit( unop(Iop_32to1,
                                 binop(Iop_Xor32,
                                       mkexpr(r15guard), mkU32(1))),
                            r15kind,
                            IRConst_U32(guest_R15_curr_instr + 4)
         ));
      }
      irsb->next     = llGetIReg(15);
      irsb->jumpkind = r15kind;
      dres.whatNext  = Dis_StopHere;
   }

   return dres;

#  undef INSN_COND
#  undef INSN
}

#undef DIP
#undef DIS


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult disInstr_ARM ( IRSB*        irsb_IN,
                         Bool         put_IP,
                         Bool         (*resteerOkFn) ( void*, Addr64 ),
                         Bool         resteerCisOk,
                         void*        callback_opaque,
                         UChar*       guest_code_IN,
                         Long         delta,
                         Addr64       guest_IP,
                         VexArch      guest_arch,
                         VexArchInfo* archinfo,
                         VexAbiInfo*  abiinfo,
                         Bool         host_bigendian_IN )
{
   DisResult dres;

   /* Set globals (see top of this file) */
   vassert(guest_arch == VexArchARM);
   irsb                 = irsb_IN;
   host_is_bigendian    = host_bigendian_IN;
   guest_R15_curr_instr = (Addr32)guest_IP;

   dres = disInstr_ARM_WRK ( put_IP, resteerOkFn,
                             resteerCisOk, callback_opaque,
                             &guest_code_IN[delta],
                             archinfo, abiinfo );

   return dres;
}

/* Test program for the conversion of IRCmpF64Result values to VFP
   nzcv values.  See handling of FCMPD et al above. */
/*
UInt foo ( UInt x )
{
   UInt ix    = ((x >> 5) & 3) | (x & 1);
   UInt termL = (((((ix ^ 1) << 30) - 1) >> 29) + 1);
   UInt termR = (ix & (ix >> 1) & 1);
   return termL  -  termR;
}

void try ( char* s, UInt ir, UInt req )
{
   UInt act = foo(ir);
   printf("%s 0x%02x -> req %d%d%d%d act %d%d%d%d (0x%x)\n",
          s, ir, (req >> 3) & 1, (req >> 2) & 1, 
                 (req >> 1) & 1, (req >> 0) & 1, 
                 (act >> 3) & 1, (act >> 2) & 1, 
                 (act >> 1) & 1, (act >> 0) & 1, act);

}

int main ( void )
{
   printf("\n");
   try("UN", 0x45, 0b0011);
   try("LT", 0x01, 0b1000);
   try("GT", 0x00, 0b0010);
   try("EQ", 0x40, 0b0110);
   printf("\n");
   return 0;
}
*/

/*--------------------------------------------------------------------*/
/*--- end                                         guest_arm_toIR.c ---*/
/*--------------------------------------------------------------------*/
