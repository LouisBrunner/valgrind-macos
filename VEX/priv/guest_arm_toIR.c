
/*--------------------------------------------------------------------*/
/*--- begin                                       guest_arm_toIR.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2015 OpenWorks LLP
      info@open-works.net

   NEON support is
   Copyright (C) 2010-2015 Samsung Electronics
   contributed by Dmitry Zhurikhin <zhur@ispras.ru>
              and Kirill Batuzov <batuzovk@ispras.ru>

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

/* XXXX thumb to check:
   that all cases where putIRegT writes r15, we generate a jump.

   All uses of newTemp assign to an IRTemp and not a UInt

   For all thumb loads and stores, including VFP ones, new-ITSTATE is
   backed out before the memory op, and restored afterwards.  This
   needs to happen even after we go uncond.  (and for sure it doesn't
   happen for VFP loads/stores right now).

   VFP on thumb: check that we exclude all r13/r15 cases that we
   should.

   XXXX thumb to do: improve the ITSTATE-zeroing optimisation by
   taking into account the number of insns guarded by an IT.

   remove the nasty hack, in the spechelper, of looking for Or32(...,
   0xE0) in as the first arg to armg_calculate_condition, and instead
   use Slice44 as specified in comments in the spechelper.

   add specialisations for armg_calculate_flag_c and _v, as they
   are moderately often needed in Thumb code.

   Correctness: ITSTATE handling in Thumb SVCs is wrong.

   Correctness (obscure): in m_transtab, when invalidating code
   address ranges, invalidate up to 18 bytes after the end of the
   range.  This is because the ITSTATE optimisation at the top of
   _THUMB_WRK below analyses up to 18 bytes before the start of any
   given instruction, and so might depend on the invalidated area.
*/

/* Limitations, etc

   - pretty dodgy exception semantics for {LD,ST}Mxx and {LD,ST}RD.
     These instructions are non-restartable in the case where the
     transfer(s) fault.

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
      E1899009 (orr r9,r9,r9)      IR injection

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

/* CONST: what is the host's endianness?  This has to do with float vs
   double register accesses on VFP, but it's complex and not properly
   thought out. */
static VexEndness host_endness;

/* CONST: The guest address for the instruction currently being
   translated.  This is the real, "decoded" address (not subject
   to the CPSR.T kludge). */
static Addr32 guest_R15_curr_instr_notENC;

/* CONST, FOR ASSERTIONS ONLY.  Indicates whether currently processed
   insn is Thumb (True) or ARM (False). */
static Bool __curr_is_Thumb;

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

#define ASSERT_IS_THUMB \
   do { vassert(__curr_is_Thumb); } while (0)

#define ASSERT_IS_ARM \
   do { vassert(! __curr_is_Thumb); } while (0)


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- arm insn stream.                                     ---*/
/*------------------------------------------------------------*/

/* Do a little-endian load of a 32-bit word, regardless of the
   endianness of the underlying host. */
static inline UInt getUIntLittleEndianly ( const UChar* p )
{
   UInt w = 0;
   w = (w << 8) | p[3];
   w = (w << 8) | p[2];
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
   return w;
}

/* Do a little-endian load of a 16-bit word, regardless of the
   endianness of the underlying host. */
static inline UShort getUShortLittleEndianly ( const UChar* p )
{
   UShort w = 0;
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

static Int popcount32 ( UInt x )
{
   Int res = 0, i;
   for (i = 0; i < 32; i++) {
      res += (x & 1);
      x >>= 1;
   }
   return res;
}

static UInt setbit32 ( UInt x, Int ix, UInt b )
{
   UInt mask = 1 << ix;
   x &= ~mask;
   x |= ((b << ix) & mask);
   return x;
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

#define BITS5(_b4,_b3,_b2,_b1,_b0)  \
   (BITS8(0,0,0,(_b4),(_b3),(_b2),(_b1),(_b0)))
#define BITS6(_b5,_b4,_b3,_b2,_b1,_b0)  \
   (BITS8(0,0,(_b5),(_b4),(_b3),(_b2),(_b1),(_b0)))
#define BITS7(_b6,_b5,_b4,_b3,_b2,_b1,_b0)  \
   (BITS8(0,(_b6),(_b5),(_b4),(_b3),(_b2),(_b1),(_b0)))

#define BITS9(_b8,_b7,_b6,_b5,_b4,_b3,_b2,_b1,_b0)      \
   (((_b8) << 8) \
    | BITS8((_b7),(_b6),(_b5),(_b4),(_b3),(_b2),(_b1),(_b0)))

#define BITS10(_b9,_b8,_b7,_b6,_b5,_b4,_b3,_b2,_b1,_b0)  \
   (((_b9) << 9) | ((_b8) << 8)                                \
    | BITS8((_b7),(_b6),(_b5),(_b4),(_b3),(_b2),(_b1),(_b0)))

#define BITS11(_b10,_b9,_b8,_b7,_b6,_b5,_b4,_b3,_b2,_b1,_b0)  \
   ( ((_b10) << 10) | ((_b9) << 9) | ((_b8) << 8)              \
    | BITS8((_b7),(_b6),(_b5),(_b4),(_b3),(_b2),(_b1),(_b0)))

#define BITS12(_b11,_b10,_b9,_b8,_b7,_b6,_b5,_b4,_b3,_b2,_b1,_b0)  \
   ( ((_b11) << 11) | ((_b10) << 10) | ((_b9) << 9) | ((_b8) << 8) \
    | BITS8((_b7),(_b6),(_b5),(_b4),(_b3),(_b2),(_b1),(_b0)))

/* produces _uint[_bMax:_bMin] */
#define SLICE_UInt(_uint,_bMax,_bMin) \
   (( ((UInt)(_uint)) >> (_bMin)) \
    & (UInt)((1ULL << ((_bMax) - (_bMin) + 1)) - 1ULL))


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for creating IR fragments.    ---*/
/*------------------------------------------------------------*/

static IRExpr* mkU64 ( ULong i )
{
   return IRExpr_Const(IRConst_U64(i));
}

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

static void storeGuardedLE ( IRExpr* addr, IRExpr* data, IRTemp guardT )
{
   if (guardT == IRTemp_INVALID) {
      /* unconditional */
      storeLE(addr, data);
   } else {
      stmt( IRStmt_StoreG(Iend_LE, addr, data,
                          binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0))) );
   }
}

static void loadGuardedLE ( IRTemp dst, IRLoadGOp cvt,
                            IRExpr* addr, IRExpr* alt, 
                            IRTemp guardT /* :: Ity_I32, 0 or 1 */ )
{
   if (guardT == IRTemp_INVALID) {
      /* unconditional */
      IRExpr* loaded = NULL;
      switch (cvt) {
         case ILGop_Ident32:
            loaded = loadLE(Ity_I32, addr); break;
         case ILGop_8Uto32:
            loaded = unop(Iop_8Uto32, loadLE(Ity_I8, addr)); break;
         case ILGop_8Sto32:
            loaded = unop(Iop_8Sto32, loadLE(Ity_I8, addr)); break;
         case ILGop_16Uto32:
            loaded = unop(Iop_16Uto32, loadLE(Ity_I16, addr)); break;
         case ILGop_16Sto32:
            loaded = unop(Iop_16Sto32, loadLE(Ity_I16, addr)); break;
         default:
            vassert(0);
      }
      vassert(loaded != NULL);
      assign(dst, loaded);
   } else {
      /* Generate a guarded load into 'dst', but apply 'cvt' to the
         loaded data before putting the data in 'dst'.  If the load
         does not take place, 'alt' is placed directly in 'dst'. */
      stmt( IRStmt_LoadG(Iend_LE, cvt, dst, addr, alt,
                         binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0))) );
   }
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

static IRExpr* mkU128 ( ULong i )
{
   return binop(Iop_64HLtoV128, mkU64(i), mkU64(i));
}

/* Generate a 4-aligned version of the given expression if
   the given condition is true.  Else return it unchanged. */
static IRExpr* align4if ( IRExpr* e, Bool b )
{
   if (b)
      return binop(Iop_And32, e, mkU32(~3));
   else
      return e;
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
#define OFFB_R15T     offsetof(VexGuestARMState,guest_R15T)

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
#define OFFB_D16      offsetof(VexGuestARMState,guest_D16)
#define OFFB_D17      offsetof(VexGuestARMState,guest_D17)
#define OFFB_D18      offsetof(VexGuestARMState,guest_D18)
#define OFFB_D19      offsetof(VexGuestARMState,guest_D19)
#define OFFB_D20      offsetof(VexGuestARMState,guest_D20)
#define OFFB_D21      offsetof(VexGuestARMState,guest_D21)
#define OFFB_D22      offsetof(VexGuestARMState,guest_D22)
#define OFFB_D23      offsetof(VexGuestARMState,guest_D23)
#define OFFB_D24      offsetof(VexGuestARMState,guest_D24)
#define OFFB_D25      offsetof(VexGuestARMState,guest_D25)
#define OFFB_D26      offsetof(VexGuestARMState,guest_D26)
#define OFFB_D27      offsetof(VexGuestARMState,guest_D27)
#define OFFB_D28      offsetof(VexGuestARMState,guest_D28)
#define OFFB_D29      offsetof(VexGuestARMState,guest_D29)
#define OFFB_D30      offsetof(VexGuestARMState,guest_D30)
#define OFFB_D31      offsetof(VexGuestARMState,guest_D31)

#define OFFB_FPSCR    offsetof(VexGuestARMState,guest_FPSCR)
#define OFFB_TPIDRURO offsetof(VexGuestARMState,guest_TPIDRURO)
#define OFFB_ITSTATE  offsetof(VexGuestARMState,guest_ITSTATE)
#define OFFB_QFLAG32  offsetof(VexGuestARMState,guest_QFLAG32)
#define OFFB_GEFLAG0  offsetof(VexGuestARMState,guest_GEFLAG0)
#define OFFB_GEFLAG1  offsetof(VexGuestARMState,guest_GEFLAG1)
#define OFFB_GEFLAG2  offsetof(VexGuestARMState,guest_GEFLAG2)
#define OFFB_GEFLAG3  offsetof(VexGuestARMState,guest_GEFLAG3)

#define OFFB_CMSTART  offsetof(VexGuestARMState,guest_CMSTART)
#define OFFB_CMLEN    offsetof(VexGuestARMState,guest_CMLEN)


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
      case 15: return OFFB_R15T;
      default: vassert(0);
   }
}

/* Plain ("low level") read from a reg; no +8 offset magic for r15. */
static IRExpr* llGetIReg ( UInt iregNo )
{
   vassert(iregNo < 16);
   return IRExpr_Get( integerGuestRegOffset(iregNo), Ity_I32 );
}

/* Architected read from a reg in ARM mode.  This automagically adds 8
   to all reads of r15. */
static IRExpr* getIRegA ( UInt iregNo )
{
   IRExpr* e;
   ASSERT_IS_ARM;
   vassert(iregNo < 16);
   if (iregNo == 15) {
      /* If asked for r15, don't read the guest state value, as that
         may not be up to date in the case where loop unrolling has
         happened, because the first insn's write to the block is
         omitted; hence in the 2nd and subsequent unrollings we don't
         have a correct value in guest r15.  Instead produce the
         constant that we know would be produced at this point. */
      vassert(0 == (guest_R15_curr_instr_notENC & 3));
      e = mkU32(guest_R15_curr_instr_notENC + 8);
   } else {
      e = IRExpr_Get( integerGuestRegOffset(iregNo), Ity_I32 );
   }
   return e;
}

/* Architected read from a reg in Thumb mode.  This automagically adds
   4 to all reads of r15. */
static IRExpr* getIRegT ( UInt iregNo )
{
   IRExpr* e;
   ASSERT_IS_THUMB;
   vassert(iregNo < 16);
   if (iregNo == 15) {
      /* Ditto comment in getIReg. */
      vassert(0 == (guest_R15_curr_instr_notENC & 1));
      e = mkU32(guest_R15_curr_instr_notENC + 4);
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

/* Architected write to an integer register in ARM mode.  If it is to
   r15, record info so at the end of this insn's translation, a branch
   to it can be made.  Also handles conditional writes to the
   register: if guardT == IRTemp_INVALID then the write is
   unconditional.  If writing r15, also 4-align it. */
static void putIRegA ( UInt       iregNo,
                       IRExpr*    e,
                       IRTemp     guardT /* :: Ity_I32, 0 or 1 */,
                       IRJumpKind jk /* if a jump is generated */ )
{
   /* if writing r15, force e to be 4-aligned. */
   // INTERWORKING FIXME.  this needs to be relaxed so that
   // puts caused by LDMxx which load r15 interwork right.
   // but is no aligned too relaxed?
   //if (iregNo == 15)
   //   e = binop(Iop_And32, e, mkU32(~3));
   ASSERT_IS_ARM;
   /* So, generate either an unconditional or a conditional write to
      the reg. */
   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      llPutIReg( iregNo, e );
   } else {
      llPutIReg( iregNo,
                 IRExpr_ITE( binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0)),
                             e, llGetIReg(iregNo) ));
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


/* Architected write to an integer register in Thumb mode.  Writes to
   r15 are not allowed.  Handles conditional writes to the register:
   if guardT == IRTemp_INVALID then the write is unconditional. */
static void putIRegT ( UInt       iregNo,
                       IRExpr*    e,
                       IRTemp     guardT /* :: Ity_I32, 0 or 1 */ )
{
   /* So, generate either an unconditional or a conditional write to
      the reg. */
   ASSERT_IS_THUMB;
   vassert(iregNo >= 0 && iregNo <= 14);
   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      llPutIReg( iregNo, e );
   } else {
      llPutIReg( iregNo,
                 IRExpr_ITE( binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0)),
                             e, llGetIReg(iregNo) ));
   }
}


/* Thumb16 and Thumb32 only.
   Returns true if reg is 13 or 15.  Implements the BadReg
   predicate in the ARM ARM. */
static Bool isBadRegT ( UInt r )
{
   vassert(r <= 15);
   ASSERT_IS_THUMB;
   return r == 13 || r == 15;
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
      case 16: return OFFB_D16;
      case 17: return OFFB_D17;
      case 18: return OFFB_D18;
      case 19: return OFFB_D19;
      case 20: return OFFB_D20;
      case 21: return OFFB_D21;
      case 22: return OFFB_D22;
      case 23: return OFFB_D23;
      case 24: return OFFB_D24;
      case 25: return OFFB_D25;
      case 26: return OFFB_D26;
      case 27: return OFFB_D27;
      case 28: return OFFB_D28;
      case 29: return OFFB_D29;
      case 30: return OFFB_D30;
      case 31: return OFFB_D31;
      default: vassert(0);
   }
}

/* Plain ("low level") read from a VFP Dreg. */
static IRExpr* llGetDReg ( UInt dregNo )
{
   vassert(dregNo < 32);
   return IRExpr_Get( doubleGuestRegOffset(dregNo), Ity_F64 );
}

/* Architected read from a VFP Dreg. */
static IRExpr* getDReg ( UInt dregNo ) {
   return llGetDReg( dregNo );
}

/* Plain ("low level") write to a VFP Dreg. */
static void llPutDReg ( UInt dregNo, IRExpr* e )
{
   vassert(dregNo < 32);
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
                 IRExpr_ITE( binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0)),
                             e, llGetDReg(dregNo) ));
   }
}

/* And now exactly the same stuff all over again, but this time
   taking/returning I64 rather than F64, to support 64-bit Neon
   ops. */

/* Plain ("low level") read from a Neon Integer Dreg. */
static IRExpr* llGetDRegI64 ( UInt dregNo )
{
   vassert(dregNo < 32);
   return IRExpr_Get( doubleGuestRegOffset(dregNo), Ity_I64 );
}

/* Architected read from a Neon Integer Dreg. */
static IRExpr* getDRegI64 ( UInt dregNo ) {
   return llGetDRegI64( dregNo );
}

/* Plain ("low level") write to a Neon Integer Dreg. */
static void llPutDRegI64 ( UInt dregNo, IRExpr* e )
{
   vassert(dregNo < 32);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I64);
   stmt( IRStmt_Put(doubleGuestRegOffset(dregNo), e) );
}

/* Architected write to a Neon Integer Dreg.  Handles conditional
   writes to the register: if guardT == IRTemp_INVALID then the write
   is unconditional. */
static void putDRegI64 ( UInt    dregNo,
                         IRExpr* e,
                         IRTemp  guardT /* :: Ity_I32, 0 or 1 */)
{
   /* So, generate either an unconditional or a conditional write to
      the reg. */
   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      llPutDRegI64( dregNo, e );
   } else {
      llPutDRegI64( dregNo,
                    IRExpr_ITE( binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0)),
                                e, llGetDRegI64(dregNo) ));
   }
}

/* ---------------- Quad registers ---------------- */

static Int quadGuestRegOffset ( UInt qregNo )
{
   /* Do we care about endianness here?  Probably do if we ever get
      into the situation of dealing with the 64 bit Neon registers. */
   switch (qregNo) {
      case 0:  return OFFB_D0;
      case 1:  return OFFB_D2;
      case 2:  return OFFB_D4;
      case 3:  return OFFB_D6;
      case 4:  return OFFB_D8;
      case 5:  return OFFB_D10;
      case 6:  return OFFB_D12;
      case 7:  return OFFB_D14;
      case 8:  return OFFB_D16;
      case 9:  return OFFB_D18;
      case 10: return OFFB_D20;
      case 11: return OFFB_D22;
      case 12: return OFFB_D24;
      case 13: return OFFB_D26;
      case 14: return OFFB_D28;
      case 15: return OFFB_D30;
      default: vassert(0);
   }
}

/* Plain ("low level") read from a Neon Qreg. */
static IRExpr* llGetQReg ( UInt qregNo )
{
   vassert(qregNo < 16);
   return IRExpr_Get( quadGuestRegOffset(qregNo), Ity_V128 );
}

/* Architected read from a Neon Qreg. */
static IRExpr* getQReg ( UInt qregNo ) {
   return llGetQReg( qregNo );
}

/* Plain ("low level") write to a Neon Qreg. */
static void llPutQReg ( UInt qregNo, IRExpr* e )
{
   vassert(qregNo < 16);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_V128);
   stmt( IRStmt_Put(quadGuestRegOffset(qregNo), e) );
}

/* Architected write to a Neon Qreg.  Handles conditional writes to the
   register: if guardT == IRTemp_INVALID then the write is
   unconditional. */
static void putQReg ( UInt    qregNo,
                      IRExpr* e,
                      IRTemp  guardT /* :: Ity_I32, 0 or 1 */)
{
   /* So, generate either an unconditional or a conditional write to
      the reg. */
   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      llPutQReg( qregNo, e );
   } else {
      llPutQReg( qregNo,
                 IRExpr_ITE( binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0)),
                             e, llGetQReg(qregNo) ));
   }
}


/* ---------------- Float registers ---------------- */

static Int floatGuestRegOffset ( UInt fregNo )
{
   /* Start with the offset of the containing double, and then correct
      for endianness.  Actually this is completely bogus and needs
      careful thought. */
   Int off;
   vassert(fregNo < 32);
   off = doubleGuestRegOffset(fregNo >> 1);
   if (host_endness == VexEndnessLE) {
      if (fregNo & 1)
         off += 4;
   } else {
      vassert(0);
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
                 IRExpr_ITE( binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0)),
                             e, llGetFReg(fregNo) ));
   }
}


/* ---------------- Misc registers ---------------- */

static void putMiscReg32 ( UInt    gsoffset, 
                           IRExpr* e, /* :: Ity_I32 */
                           IRTemp  guardT /* :: Ity_I32, 0 or 1 */)
{
   switch (gsoffset) {
      case OFFB_FPSCR:   break;
      case OFFB_QFLAG32: break;
      case OFFB_GEFLAG0: break;
      case OFFB_GEFLAG1: break;
      case OFFB_GEFLAG2: break;
      case OFFB_GEFLAG3: break;
      default: vassert(0); /* awaiting more cases */
   }
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I32);

   if (guardT == IRTemp_INVALID) {
      /* unconditional write */
      stmt(IRStmt_Put(gsoffset, e));
   } else {
      stmt(IRStmt_Put(
         gsoffset,
         IRExpr_ITE( binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0)),
                     e, IRExpr_Get(gsoffset, Ity_I32) )
      ));
   }
}

static IRTemp get_ITSTATE ( void )
{
   ASSERT_IS_THUMB;
   IRTemp t = newTemp(Ity_I32);
   assign(t, IRExpr_Get( OFFB_ITSTATE, Ity_I32));
   return t;
}

static void put_ITSTATE ( IRTemp t )
{
   ASSERT_IS_THUMB;
   stmt( IRStmt_Put( OFFB_ITSTATE, mkexpr(t)) );
}

static IRTemp get_QFLAG32 ( void )
{
   IRTemp t = newTemp(Ity_I32);
   assign(t, IRExpr_Get( OFFB_QFLAG32, Ity_I32));
   return t;
}

static void put_QFLAG32 ( IRTemp t, IRTemp condT )
{
   putMiscReg32( OFFB_QFLAG32, mkexpr(t), condT );
}

/* Stickily set the 'Q' flag (APSR bit 27) of the APSR (Application Program
   Status Register) to indicate that overflow or saturation occurred.
   Nb: t must be zero to denote no saturation, and any nonzero
   value to indicate saturation. */
static void or_into_QFLAG32 ( IRExpr* e, IRTemp condT )
{
   IRTemp old = get_QFLAG32();
   IRTemp nyu = newTemp(Ity_I32);
   assign(nyu, binop(Iop_Or32, mkexpr(old), e) );
   put_QFLAG32(nyu, condT);
}

/* Generate code to set APSR.GE[flagNo]. Each fn call sets 1 bit.
   flagNo: which flag bit to set [3...0]
   lowbits_to_ignore:  0 = look at all 32 bits
                       8 = look at top 24 bits only
                      16 = look at top 16 bits only
                      31 = look at the top bit only
   e: input value to be evaluated.
   The new value is taken from 'e' with the lowest 'lowbits_to_ignore'
   masked out.  If the resulting value is zero then the GE flag is
   set to 0; any other value sets the flag to 1. */
static void put_GEFLAG32 ( Int flagNo,            /* 0, 1, 2 or 3 */
                           Int lowbits_to_ignore, /* 0, 8, 16 or 31   */
                           IRExpr* e,             /* Ity_I32 */
                           IRTemp condT )
{
   vassert( flagNo >= 0 && flagNo <= 3 );
   vassert( lowbits_to_ignore == 0  || 
            lowbits_to_ignore == 8  || 
            lowbits_to_ignore == 16 ||
            lowbits_to_ignore == 31 );
   IRTemp masked = newTemp(Ity_I32);
   assign(masked, binop(Iop_Shr32, e, mkU8(lowbits_to_ignore)));

   switch (flagNo) {
      case 0: putMiscReg32(OFFB_GEFLAG0, mkexpr(masked), condT); break;
      case 1: putMiscReg32(OFFB_GEFLAG1, mkexpr(masked), condT); break;
      case 2: putMiscReg32(OFFB_GEFLAG2, mkexpr(masked), condT); break;
      case 3: putMiscReg32(OFFB_GEFLAG3, mkexpr(masked), condT); break;
      default: vassert(0);
   }
}

/* Return the (32-bit, zero-or-nonzero representation scheme) of
   the specified GE flag. */
static IRExpr* get_GEFLAG32( Int flagNo /* 0, 1, 2, 3 */ )
{
   switch (flagNo) {
      case 0: return IRExpr_Get( OFFB_GEFLAG0, Ity_I32 );
      case 1: return IRExpr_Get( OFFB_GEFLAG1, Ity_I32 );
      case 2: return IRExpr_Get( OFFB_GEFLAG2, Ity_I32 );
      case 3: return IRExpr_Get( OFFB_GEFLAG3, Ity_I32 );
      default: vassert(0);
   }
}

/* Set all 4 GE flags from the given 32-bit value as follows: GE 3 and
   2 are set from bit 31 of the value, and GE 1 and 0 are set from bit
   15 of the value.  All other bits are ignored. */
static void set_GE_32_10_from_bits_31_15 ( IRTemp t32, IRTemp condT )
{
   IRTemp ge10 = newTemp(Ity_I32);
   IRTemp ge32 = newTemp(Ity_I32);
   assign(ge10, binop(Iop_And32, mkexpr(t32), mkU32(0x00008000)));
   assign(ge32, binop(Iop_And32, mkexpr(t32), mkU32(0x80000000)));
   put_GEFLAG32( 0, 0, mkexpr(ge10), condT );
   put_GEFLAG32( 1, 0, mkexpr(ge10), condT );
   put_GEFLAG32( 2, 0, mkexpr(ge32), condT );
   put_GEFLAG32( 3, 0, mkexpr(ge32), condT );
}


/* Set all 4 GE flags from the given 32-bit value as follows: GE 3
   from bit 31, GE 2 from bit 23, GE 1 from bit 15, and GE0 from
   bit 7.  All other bits are ignored. */
static void set_GE_3_2_1_0_from_bits_31_23_15_7 ( IRTemp t32, IRTemp condT )
{
   IRTemp ge0 = newTemp(Ity_I32);
   IRTemp ge1 = newTemp(Ity_I32);
   IRTemp ge2 = newTemp(Ity_I32);
   IRTemp ge3 = newTemp(Ity_I32);
   assign(ge0, binop(Iop_And32, mkexpr(t32), mkU32(0x00000080)));
   assign(ge1, binop(Iop_And32, mkexpr(t32), mkU32(0x00008000)));
   assign(ge2, binop(Iop_And32, mkexpr(t32), mkU32(0x00800000)));
   assign(ge3, binop(Iop_And32, mkexpr(t32), mkU32(0x80000000)));
   put_GEFLAG32( 0, 0, mkexpr(ge0), condT );
   put_GEFLAG32( 1, 0, mkexpr(ge1), condT );
   put_GEFLAG32( 2, 0, mkexpr(ge2), condT );
   put_GEFLAG32( 3, 0, mkexpr(ge3), condT );
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

static const HChar* name_ARMCondcode ( ARMCondcode cond )
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
static const HChar* nCC ( ARMCondcode cond ) {
   return name_ARMCondcode(cond);
}


/* Build IR to calculate some particular condition from stored
   CC_OP/CC_DEP1/CC_DEP2/CC_NDEP.  Returns an expression of type
   Ity_I32, suitable for narrowing.  Although the return type is
   Ity_I32, the returned value is either 0 or 1.  'cond' must be
   :: Ity_I32 and must denote the condition to compute in 
   bits 7:4, and be zero everywhere else.
*/
static IRExpr* mk_armg_calculate_condition_dyn ( IRExpr* cond )
{
   vassert(typeOfIRExpr(irsb->tyenv, cond) == Ity_I32);
   /* And 'cond' had better produce a value in which only bits 7:4 are
      nonzero.  However, obviously we can't assert for that. */

   /* So what we're constructing for the first argument is 
      "(cond << 4) | stored-operation".
      However, as per comments above, 'cond' must be supplied
      pre-shifted to this function.

      This pairing scheme requires that the ARM_CC_OP_ values all fit
      in 4 bits.  Hence we are passing a (COND, OP) pair in the lowest
      8 bits of the first argument. */
   IRExpr** args
      = mkIRExprVec_4(
           binop(Iop_Or32, IRExpr_Get(OFFB_CC_OP, Ity_I32), cond),
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
   return mk_armg_calculate_condition_dyn( mkU32(cond << 4) );
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

static IRExpr* mk_armg_calculate_flag_qc ( IRExpr* resL, IRExpr* resR, Bool Q )
{
   IRExpr** args1;
   IRExpr** args2;
   IRExpr *call1, *call2, *res;

   if (Q) {
      args1 = mkIRExprVec_4 ( binop(Iop_GetElem32x4, resL, mkU8(0)),
                              binop(Iop_GetElem32x4, resL, mkU8(1)),
                              binop(Iop_GetElem32x4, resR, mkU8(0)),
                              binop(Iop_GetElem32x4, resR, mkU8(1)) );
      args2 = mkIRExprVec_4 ( binop(Iop_GetElem32x4, resL, mkU8(2)),
                              binop(Iop_GetElem32x4, resL, mkU8(3)),
                              binop(Iop_GetElem32x4, resR, mkU8(2)),
                              binop(Iop_GetElem32x4, resR, mkU8(3)) );
   } else {
      args1 = mkIRExprVec_4 ( binop(Iop_GetElem32x2, resL, mkU8(0)),
                              binop(Iop_GetElem32x2, resL, mkU8(1)),
                              binop(Iop_GetElem32x2, resR, mkU8(0)),
                              binop(Iop_GetElem32x2, resR, mkU8(1)) );
   }

   call1 = mkIRExprCCall(
             Ity_I32,
             0/*regparm*/, 
             "armg_calculate_flag_qc", &armg_calculate_flag_qc,
             args1
          );
   if (Q) {
      call2 = mkIRExprCCall(
                Ity_I32,
                0/*regparm*/, 
                "armg_calculate_flag_qc", &armg_calculate_flag_qc,
                args2
             );
   }
   if (Q) {
      res = binop(Iop_Or32, call1, call2);
   } else {
      res = call1;
   }
   return res;
}

// FIXME: this is named wrongly .. looks like a sticky set of
// QC, not a write to it.
static void setFlag_QC ( IRExpr* resL, IRExpr* resR, Bool Q,
                         IRTemp condT )
{
   putMiscReg32 (OFFB_FPSCR,
                 binop(Iop_Or32,
                       IRExpr_Get(OFFB_FPSCR, Ity_I32),
                       binop(Iop_Shl32,
                             mk_armg_calculate_flag_qc(resL, resR, Q),
                             mkU8(27))),
                 condT);
}

/* Build IR to conditionally set the flags thunk.  As with putIReg, if
   guard is IRTemp_INVALID then it's unconditional, else it holds a
   condition :: Ity_I32. */
static
void setFlags_D1_D2_ND ( UInt cc_op, IRTemp t_dep1,
                         IRTemp t_dep2, IRTemp t_ndep,
                         IRTemp guardT /* :: Ity_I32, 0 or 1 */ )
{
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
      IRTemp c1 = newTemp(Ity_I1);
      assign( c1, binop(Iop_CmpNE32, mkexpr(guardT), mkU32(0)) );
      stmt( IRStmt_Put(
               OFFB_CC_OP,
               IRExpr_ITE( mkexpr(c1),
                           mkU32(cc_op),
                           IRExpr_Get(OFFB_CC_OP, Ity_I32) ) ));
      stmt( IRStmt_Put(
               OFFB_CC_DEP1,
               IRExpr_ITE( mkexpr(c1),
                           mkexpr(t_dep1),
                           IRExpr_Get(OFFB_CC_DEP1, Ity_I32) ) ));
      stmt( IRStmt_Put(
               OFFB_CC_DEP2,
               IRExpr_ITE( mkexpr(c1),
                           mkexpr(t_dep2),
                           IRExpr_Get(OFFB_CC_DEP2, Ity_I32) ) ));
      stmt( IRStmt_Put(
               OFFB_CC_NDEP,
               IRExpr_ITE( mkexpr(c1),
                           mkexpr(t_ndep),
                           IRExpr_Get(OFFB_CC_NDEP, Ity_I32) ) ));
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


/* ARM only */
/* Generate a side-exit to the next instruction, if the given guard
   expression :: Ity_I32 is 0 (note!  the side exit is taken if the
   condition is false!)  This is used to skip over conditional
   instructions which we can't generate straight-line code for, either
   because they are too complex or (more likely) they potentially
   generate exceptions.
*/
static void mk_skip_over_A32_if_cond_is_false ( 
               IRTemp guardT /* :: Ity_I32, 0 or 1 */
            )
{
   ASSERT_IS_ARM;
   vassert(guardT != IRTemp_INVALID);
   vassert(0 == (guest_R15_curr_instr_notENC & 3));
   stmt( IRStmt_Exit(
            unop(Iop_Not1, unop(Iop_32to1, mkexpr(guardT))),
            Ijk_Boring,
            IRConst_U32(toUInt(guest_R15_curr_instr_notENC + 4)),
            OFFB_R15T
       ));
}

/* Thumb16 only */
/* ditto, but jump over a 16-bit thumb insn */
static void mk_skip_over_T16_if_cond_is_false ( 
               IRTemp guardT /* :: Ity_I32, 0 or 1 */
            )
{
   ASSERT_IS_THUMB;
   vassert(guardT != IRTemp_INVALID);
   vassert(0 == (guest_R15_curr_instr_notENC & 1));
   stmt( IRStmt_Exit(
            unop(Iop_Not1, unop(Iop_32to1, mkexpr(guardT))),
            Ijk_Boring,
            IRConst_U32(toUInt((guest_R15_curr_instr_notENC + 2) | 1)),
            OFFB_R15T
       ));
}


/* Thumb32 only */
/* ditto, but jump over a 32-bit thumb insn */
static void mk_skip_over_T32_if_cond_is_false ( 
               IRTemp guardT /* :: Ity_I32, 0 or 1 */
            )
{
   ASSERT_IS_THUMB;
   vassert(guardT != IRTemp_INVALID);
   vassert(0 == (guest_R15_curr_instr_notENC & 1));
   stmt( IRStmt_Exit(
            unop(Iop_Not1, unop(Iop_32to1, mkexpr(guardT))),
            Ijk_Boring,
            IRConst_U32(toUInt((guest_R15_curr_instr_notENC + 4) | 1)),
            OFFB_R15T
       ));
}


/* Thumb16 and Thumb32 only
   Generate a SIGILL followed by a restart of the current instruction
   if the given temp is nonzero. */
static void gen_SIGILL_T_if_nonzero ( IRTemp t /* :: Ity_I32 */ )
{
   ASSERT_IS_THUMB;
   vassert(t != IRTemp_INVALID);
   vassert(0 == (guest_R15_curr_instr_notENC & 1));
   stmt(
      IRStmt_Exit(
         binop(Iop_CmpNE32, mkexpr(t), mkU32(0)),
         Ijk_NoDecode,
         IRConst_U32(toUInt(guest_R15_curr_instr_notENC | 1)),
         OFFB_R15T
      )
   );
}


/* Inspect the old_itstate, and generate a SIGILL if it indicates that
   we are currently in an IT block and are not the last in the block.
   This also rolls back guest_ITSTATE to its old value before the exit
   and restores it to its new value afterwards.  This is so that if
   the exit is taken, we have an up to date version of ITSTATE
   available.  Without doing that, we have no hope of making precise
   exceptions work. */
static void gen_SIGILL_T_if_in_but_NLI_ITBlock (
               IRTemp old_itstate /* :: Ity_I32 */,
               IRTemp new_itstate /* :: Ity_I32 */
            )
{
   ASSERT_IS_THUMB;
   put_ITSTATE(old_itstate); // backout
   IRTemp guards_for_next3 = newTemp(Ity_I32);
   assign(guards_for_next3,
          binop(Iop_Shr32, mkexpr(old_itstate), mkU8(8)));
   gen_SIGILL_T_if_nonzero(guards_for_next3);
   put_ITSTATE(new_itstate); //restore
}


/* Simpler version of the above, which generates a SIGILL if
   we're anywhere within an IT block. */
static void gen_SIGILL_T_if_in_ITBlock (
               IRTemp old_itstate /* :: Ity_I32 */,
               IRTemp new_itstate /* :: Ity_I32 */
            )
{
   put_ITSTATE(old_itstate); // backout
   gen_SIGILL_T_if_nonzero(old_itstate);
   put_ITSTATE(new_itstate); //restore
}


/* Generate an APSR value, from the NZCV thunk, and
   from QFLAG32 and GEFLAG0 .. GEFLAG3. */
static IRTemp synthesise_APSR ( void )
{
   IRTemp res1 = newTemp(Ity_I32);
   // Get NZCV
   assign( res1, mk_armg_calculate_flags_nzcv() );
   // OR in the Q value
   IRTemp res2 = newTemp(Ity_I32);
   assign(
      res2,
      binop(Iop_Or32,
            mkexpr(res1),
            binop(Iop_Shl32,
                  unop(Iop_1Uto32,
                       binop(Iop_CmpNE32,
                             mkexpr(get_QFLAG32()),
                             mkU32(0))),
                  mkU8(ARMG_CC_SHIFT_Q)))
   );
   // OR in GE0 .. GE3
   IRExpr* ge0
      = unop(Iop_1Uto32, binop(Iop_CmpNE32, get_GEFLAG32(0), mkU32(0)));
   IRExpr* ge1
      = unop(Iop_1Uto32, binop(Iop_CmpNE32, get_GEFLAG32(1), mkU32(0)));
   IRExpr* ge2
      = unop(Iop_1Uto32, binop(Iop_CmpNE32, get_GEFLAG32(2), mkU32(0)));
   IRExpr* ge3
      = unop(Iop_1Uto32, binop(Iop_CmpNE32, get_GEFLAG32(3), mkU32(0)));
   IRTemp res3 = newTemp(Ity_I32);
   assign(res3,
          binop(Iop_Or32,
                mkexpr(res2),
                binop(Iop_Or32,
                      binop(Iop_Or32,
                            binop(Iop_Shl32, ge0, mkU8(16)),
                            binop(Iop_Shl32, ge1, mkU8(17))),
                      binop(Iop_Or32,
                            binop(Iop_Shl32, ge2, mkU8(18)),
                            binop(Iop_Shl32, ge3, mkU8(19))) )));
   return res3;
}


/* and the inverse transformation: given an APSR value,
   set the NZCV thunk, the Q flag, and the GE flags. */
static void desynthesise_APSR ( Bool write_nzcvq, Bool write_ge,
                                IRTemp apsrT, IRTemp condT )
{
   vassert(write_nzcvq || write_ge);
   if (write_nzcvq) {
      // Do NZCV
      IRTemp immT = newTemp(Ity_I32);
      assign(immT, binop(Iop_And32, mkexpr(apsrT), mkU32(0xF0000000)) );
      setFlags_D1(ARMG_CC_OP_COPY, immT, condT);
      // Do Q
      IRTemp qnewT = newTemp(Ity_I32);
      assign(qnewT, binop(Iop_And32, mkexpr(apsrT), mkU32(ARMG_CC_MASK_Q)));
      put_QFLAG32(qnewT, condT);
   }
   if (write_ge) {
      // Do GE3..0
      put_GEFLAG32(0, 0, binop(Iop_And32, mkexpr(apsrT), mkU32(1<<16)),
                   condT);
      put_GEFLAG32(1, 0, binop(Iop_And32, mkexpr(apsrT), mkU32(1<<17)),
                   condT);
      put_GEFLAG32(2, 0, binop(Iop_And32, mkexpr(apsrT), mkU32(1<<18)),
                   condT);
      put_GEFLAG32(3, 0, binop(Iop_And32, mkexpr(apsrT), mkU32(1<<19)),
                   condT);
   }
}


/*------------------------------------------------------------*/
/*--- Helpers for saturation                               ---*/
/*------------------------------------------------------------*/

/* FIXME: absolutely the only diff. between (a) armUnsignedSatQ and
   (b) armSignedSatQ is that in (a) the floor is set to 0, whereas in
   (b) the floor is computed from the value of imm5.  these two fnsn
   should be commoned up. */

/* UnsignedSatQ(): 'clamp' each value so it lies between 0 <= x <= (2^N)-1
   Optionally return flag resQ saying whether saturation occurred.
   See definition in manual, section A2.2.1, page 41
   (bits(N), boolean) UnsignedSatQ( integer i, integer N )
   {
     if ( i > (2^N)-1 ) { result = (2^N)-1; saturated = TRUE; }
     elsif ( i < 0 )    { result = 0; saturated = TRUE; }
     else               { result = i; saturated = FALSE; }
     return ( result<N-1:0>, saturated );
   }
*/
static void armUnsignedSatQ( IRTemp* res,  /* OUT - Ity_I32 */
                             IRTemp* resQ, /* OUT - Ity_I32  */
                             IRTemp regT,  /* value to clamp - Ity_I32 */
                             UInt imm5 )   /* saturation ceiling */
{
   ULong ceil64  = (1ULL << imm5) - 1;    // (2^imm5)-1
   UInt  ceil    = (UInt)ceil64;
   UInt  floor   = 0;

   IRTemp nd0 = newTemp(Ity_I32);
   IRTemp nd1 = newTemp(Ity_I32);
   IRTemp nd2 = newTemp(Ity_I1);
   IRTemp nd3 = newTemp(Ity_I32);
   IRTemp nd4 = newTemp(Ity_I32);
   IRTemp nd5 = newTemp(Ity_I1);
   IRTemp nd6 = newTemp(Ity_I32);

   assign( nd0, mkexpr(regT) );
   assign( nd1, mkU32(ceil) );
   assign( nd2, binop( Iop_CmpLT32S, mkexpr(nd1), mkexpr(nd0) ) );
   assign( nd3, IRExpr_ITE(mkexpr(nd2), mkexpr(nd1), mkexpr(nd0)) );
   assign( nd4, mkU32(floor) );
   assign( nd5, binop( Iop_CmpLT32S, mkexpr(nd3), mkexpr(nd4) ) );
   assign( nd6, IRExpr_ITE(mkexpr(nd5), mkexpr(nd4), mkexpr(nd3)) );
   assign( *res, mkexpr(nd6) );

   /* if saturation occurred, then resQ is set to some nonzero value
      if sat did not occur, resQ is guaranteed to be zero. */
   if (resQ) {
      assign( *resQ, binop(Iop_Xor32, mkexpr(*res), mkexpr(regT)) );
   }
}


/* SignedSatQ(): 'clamp' each value so it lies between  -2^N <= x <= (2^N) - 1
   Optionally return flag resQ saying whether saturation occurred.
   - see definition in manual, section A2.2.1, page 41
   (bits(N), boolean ) SignedSatQ( integer i, integer N ) 
   {
     if ( i > 2^(N-1) - 1 )    { result = 2^(N-1) - 1; saturated = TRUE; }
     elsif ( i < -(2^(N-1)) )  { result = -(2^(N-1));  saturated = FALSE; }
     else                      { result = i;           saturated = FALSE; }
     return ( result[N-1:0], saturated );
   }
*/
static void armSignedSatQ( IRTemp regT,    /* value to clamp - Ity_I32 */
                           UInt imm5,      /* saturation ceiling */
                           IRTemp* res,    /* OUT - Ity_I32 */
                           IRTemp* resQ )  /* OUT - Ity_I32  */
{
   Long ceil64  =  (1LL << (imm5-1)) - 1;  //  (2^(imm5-1))-1
   Long floor64 = -(1LL << (imm5-1));      // -(2^(imm5-1))
   Int  ceil    = (Int)ceil64;
   Int  floor   = (Int)floor64;

   IRTemp nd0 = newTemp(Ity_I32);
   IRTemp nd1 = newTemp(Ity_I32);
   IRTemp nd2 = newTemp(Ity_I1);
   IRTemp nd3 = newTemp(Ity_I32);
   IRTemp nd4 = newTemp(Ity_I32);
   IRTemp nd5 = newTemp(Ity_I1);
   IRTemp nd6 = newTemp(Ity_I32);

   assign( nd0, mkexpr(regT) );
   assign( nd1, mkU32(ceil) );
   assign( nd2, binop( Iop_CmpLT32S, mkexpr(nd1), mkexpr(nd0) ) );
   assign( nd3, IRExpr_ITE( mkexpr(nd2), mkexpr(nd1), mkexpr(nd0) ) );
   assign( nd4, mkU32(floor) );
   assign( nd5, binop( Iop_CmpLT32S, mkexpr(nd3), mkexpr(nd4) ) );
   assign( nd6, IRExpr_ITE( mkexpr(nd5), mkexpr(nd4), mkexpr(nd3) ) );
   assign( *res, mkexpr(nd6) );

   /* if saturation occurred, then resQ is set to some nonzero value
      if sat did not occur, resQ is guaranteed to be zero. */
   if (resQ) {
     assign( *resQ, binop(Iop_Xor32, mkexpr(*res), mkexpr(regT)) );
   }
}


/* Compute a value 0 :: I32 or 1 :: I32, indicating whether signed
   overflow occurred for 32-bit addition.  Needs both args and the
   result.  HD p27. */
static
IRExpr* signed_overflow_after_Add32 ( IRExpr* resE,
                                      IRTemp argL, IRTemp argR )
{
   IRTemp res = newTemp(Ity_I32);
   assign(res, resE);
   return
      binop( Iop_Shr32, 
             binop( Iop_And32,
                    binop( Iop_Xor32, mkexpr(res), mkexpr(argL) ),
                    binop( Iop_Xor32, mkexpr(res), mkexpr(argR) )), 
             mkU8(31) );
}

/* Similarly .. also from HD p27 .. */
static
IRExpr* signed_overflow_after_Sub32 ( IRExpr* resE,
                                      IRTemp argL, IRTemp argR )
{
   IRTemp res = newTemp(Ity_I32);
   assign(res, resE);
   return
      binop( Iop_Shr32, 
             binop( Iop_And32,
                    binop( Iop_Xor32, mkexpr(argL), mkexpr(argR) ),
                    binop( Iop_Xor32, mkexpr(res),  mkexpr(argL) )), 
             mkU8(31) );
}


/*------------------------------------------------------------*/
/*--- Larger helpers                                       ---*/
/*------------------------------------------------------------*/

/* Compute both the result and new C flag value for a LSL by an imm5
   or by a register operand.  May generate reads of the old C value
   (hence only safe to use before any writes to guest state happen).
   Are factored out so can be used by both ARM and Thumb.

   Note that in compute_result_and_C_after_{LSL,LSR,ASR}_by{imm5,reg},
   "res" (the result)  is a.k.a. "shop", shifter operand
   "newC" (the new C)  is a.k.a. "shco", shifter carry out

   The calling convention for res and newC is a bit funny.  They could
   be passed by value, but instead are passed by ref.

   The C (shco) value computed must be zero in bits 31:1, as the IR
   optimisations for flag handling (guest_arm_spechelper) rely on
   that, and the slow-path handlers (armg_calculate_flags_nzcv) assert
   for it.  Same applies to all these functions that compute shco
   after a shift or rotate, not just this one.
*/

static void compute_result_and_C_after_LSL_by_imm5 (
               /*OUT*/HChar* buf,
               IRTemp* res,
               IRTemp* newC,
               IRTemp rMt, UInt shift_amt, /* operands */
               UInt rM      /* only for debug printing */
            )
{
   if (shift_amt == 0) {
      if (newC) {
         assign( *newC, mk_armg_calculate_flag_c() );
      }
      assign( *res, mkexpr(rMt) );
      DIS(buf, "r%u", rM);
   } else {
      vassert(shift_amt >= 1 && shift_amt <= 31);
      if (newC) {
         assign( *newC,
                 binop(Iop_And32,
                       binop(Iop_Shr32, mkexpr(rMt), 
                                        mkU8(32 - shift_amt)),
                       mkU32(1)));
      }
      assign( *res,
              binop(Iop_Shl32, mkexpr(rMt), mkU8(shift_amt)) );
      DIS(buf, "r%u, LSL #%u", rM, shift_amt);
   }
}


static void compute_result_and_C_after_LSL_by_reg (
               /*OUT*/HChar* buf,
               IRTemp* res,
               IRTemp* newC,
               IRTemp rMt, IRTemp rSt,  /* operands */
               UInt rM,    UInt rS      /* only for debug printing */
            )
{
   // shift left in range 0 .. 255
   // amt  = rS & 255
   // res  = amt < 32 ?  Rm << amt  : 0
   // newC = amt == 0     ? oldC  :
   //        amt in 1..32 ?  Rm[32-amt]  : 0
   IRTemp amtT = newTemp(Ity_I32);
   assign( amtT, binop(Iop_And32, mkexpr(rSt), mkU32(255)) );
   if (newC) {
      /* mux0X(amt == 0,
               mux0X(amt < 32, 
                     0,
                     Rm[(32-amt) & 31]),
               oldC)
      */
      /* About the best you can do is pray that iropt is able
         to nuke most or all of the following junk. */
      IRTemp oldC = newTemp(Ity_I32);
      assign(oldC, mk_armg_calculate_flag_c() );
      assign(
         *newC,
         IRExpr_ITE(
            binop(Iop_CmpEQ32, mkexpr(amtT), mkU32(0)),
            mkexpr(oldC),
            IRExpr_ITE(
               binop(Iop_CmpLE32U, mkexpr(amtT), mkU32(32)),
               binop(Iop_And32,
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
                     ),
                     mkU32(1)
                     ),
               mkU32(0)
            )
         )
      );
   }
   // (Rm << (Rs & 31))  &  (((Rs & 255) - 32) >>s 31)
   // Lhs of the & limits the shift to 31 bits, so as to
   // give known IR semantics.  Rhs of the & is all 1s for
   // Rs <= 31 and all 0s for Rs >= 32.
   assign(
      *res,
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
}


static void compute_result_and_C_after_LSR_by_imm5 (
               /*OUT*/HChar* buf,
               IRTemp* res,
               IRTemp* newC,
               IRTemp rMt, UInt shift_amt, /* operands */
               UInt rM      /* only for debug printing */
            )
{
   if (shift_amt == 0) {
      // conceptually a 32-bit shift, however:
      // res  = 0
      // newC = Rm[31]
      if (newC) {
         assign( *newC,
                 binop(Iop_And32,
                       binop(Iop_Shr32, mkexpr(rMt), mkU8(31)), 
                       mkU32(1)));
      }
      assign( *res, mkU32(0) );
      DIS(buf, "r%u, LSR #0(a.k.a. 32)", rM);
   } else {
      // shift in range 1..31
      // res  = Rm >>u shift_amt
      // newC = Rm[shift_amt - 1]
      vassert(shift_amt >= 1 && shift_amt <= 31);
      if (newC) {
         assign( *newC,
                 binop(Iop_And32,
                       binop(Iop_Shr32, mkexpr(rMt), 
                                        mkU8(shift_amt - 1)),
                       mkU32(1)));
      }
      assign( *res,
              binop(Iop_Shr32, mkexpr(rMt), mkU8(shift_amt)) );
      DIS(buf, "r%u, LSR #%u", rM, shift_amt);
   }
}


static void compute_result_and_C_after_LSR_by_reg (
               /*OUT*/HChar* buf,
               IRTemp* res,
               IRTemp* newC,
               IRTemp rMt, IRTemp rSt,  /* operands */
               UInt rM,    UInt rS      /* only for debug printing */
            )
{
   // shift right in range 0 .. 255
   // amt = rS & 255
   // res  = amt < 32 ?  Rm >>u amt  : 0
   // newC = amt == 0     ? oldC  :
   //        amt in 1..32 ?  Rm[amt-1]  : 0
   IRTemp amtT = newTemp(Ity_I32);
   assign( amtT, binop(Iop_And32, mkexpr(rSt), mkU32(255)) );
   if (newC) {
      /* mux0X(amt == 0,
               mux0X(amt < 32, 
                     0,
                     Rm[(amt-1) & 31]),
               oldC)
      */
      IRTemp oldC = newTemp(Ity_I32);
      assign(oldC, mk_armg_calculate_flag_c() );
      assign(
         *newC,
         IRExpr_ITE(
            binop(Iop_CmpEQ32, mkexpr(amtT), mkU32(0)), 
            mkexpr(oldC),
            IRExpr_ITE(
               binop(Iop_CmpLE32U, mkexpr(amtT), mkU32(32)),
               binop(Iop_And32,
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
                     ),
                     mkU32(1)
                     ),
               mkU32(0)
            )
         )
      );
   }
   // (Rm >>u (Rs & 31))  &  (((Rs & 255) - 32) >>s 31)
   // Lhs of the & limits the shift to 31 bits, so as to
   // give known IR semantics.  Rhs of the & is all 1s for
   // Rs <= 31 and all 0s for Rs >= 32.
   assign(
      *res,
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
}


static void compute_result_and_C_after_ASR_by_imm5 (
               /*OUT*/HChar* buf,
               IRTemp* res,
               IRTemp* newC,
               IRTemp rMt, UInt shift_amt, /* operands */
               UInt rM      /* only for debug printing */
            )
{
   if (shift_amt == 0) {
      // conceptually a 32-bit shift, however:
      // res  = Rm >>s 31
      // newC = Rm[31]
      if (newC) {
         assign( *newC,
                 binop(Iop_And32,
                       binop(Iop_Shr32, mkexpr(rMt), mkU8(31)), 
                       mkU32(1)));
      }
      assign( *res, binop(Iop_Sar32, mkexpr(rMt), mkU8(31)) );
      DIS(buf, "r%u, ASR #0(a.k.a. 32)", rM);
   } else {
      // shift in range 1..31
      // res = Rm >>s shift_amt
      // newC = Rm[shift_amt - 1]
      vassert(shift_amt >= 1 && shift_amt <= 31);
      if (newC) {
         assign( *newC,
                 binop(Iop_And32,
                       binop(Iop_Shr32, mkexpr(rMt), 
                                        mkU8(shift_amt - 1)),
                       mkU32(1)));
      }
      assign( *res,
              binop(Iop_Sar32, mkexpr(rMt), mkU8(shift_amt)) );
      DIS(buf, "r%u, ASR #%u", rM, shift_amt);
   }
}


static void compute_result_and_C_after_ASR_by_reg (
               /*OUT*/HChar* buf,
               IRTemp* res,
               IRTemp* newC,
               IRTemp rMt, IRTemp rSt,  /* operands */
               UInt rM,    UInt rS      /* only for debug printing */
            )
{
   // arithmetic shift right in range 0 .. 255
   // amt = rS & 255
   // res  = amt < 32 ?  Rm >>s amt  : Rm >>s 31
   // newC = amt == 0     ? oldC  :
   //        amt in 1..32 ?  Rm[amt-1]  : Rm[31]
   IRTemp amtT = newTemp(Ity_I32);
   assign( amtT, binop(Iop_And32, mkexpr(rSt), mkU32(255)) );
   if (newC) {
      /* mux0X(amt == 0,
               mux0X(amt < 32, 
                     Rm[31],
                     Rm[(amt-1) & 31])
               oldC)
      */
      IRTemp oldC = newTemp(Ity_I32);
      assign(oldC, mk_armg_calculate_flag_c() );
      assign(
         *newC,
         IRExpr_ITE(
            binop(Iop_CmpEQ32, mkexpr(amtT), mkU32(0)),
            mkexpr(oldC),
            IRExpr_ITE(
               binop(Iop_CmpLE32U, mkexpr(amtT), mkU32(32)),
               binop(Iop_And32,
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
                     ),
                     mkU32(1)
                     ),
               binop(Iop_And32,
                     binop(Iop_Shr32,
                           mkexpr(rMt),
                           mkU8(31)
                     ),
                     mkU32(1)
               )
            )
         )
      );
   }
   // (Rm >>s (amt <u 32 ? amt : 31))
   assign(
      *res,
      binop(
         Iop_Sar32,
         mkexpr(rMt),
         unop(
            Iop_32to8,
            IRExpr_ITE(
               binop(Iop_CmpLT32U, mkexpr(amtT), mkU32(32)),
               mkexpr(amtT),
               mkU32(31)))));
    DIS(buf, "r%u, ASR r%u", rM, rS);
}


static void compute_result_and_C_after_ROR_by_reg (
               /*OUT*/HChar* buf,
               IRTemp* res,
               IRTemp* newC,
               IRTemp rMt, IRTemp rSt,  /* operands */
               UInt rM,    UInt rS      /* only for debug printing */
            )
{
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
   if (newC) {
      assign(
         *newC,
         IRExpr_ITE(
            binop(Iop_CmpNE32, mkexpr(amtT), mkU32(0)),
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
            ),
            mkexpr(oldC)
         )
      );
   }
   assign(
      *res,
      IRExpr_ITE(
         binop(Iop_CmpNE32, mkexpr(amt5T), mkU32(0)),
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
               ),
         mkexpr(rMt)
      )
   );
   DIS(buf, "r%u, ROR r#%u", rM, rS);
}


/* Generate an expression corresponding to the immediate-shift case of
   a shifter operand.  This is used both for ARM and Thumb2.

   Bind it to a temporary, and return that via *res.  If newC is
   non-NULL, also compute a value for the shifter's carry out (in the
   LSB of a word), bind it to a temporary, and return that via *shco.

   Generates GETs from the guest state and is therefore not safe to
   use once we start doing PUTs to it, for any given instruction.

   'how' is encoded thusly:
      00b LSL,  01b LSR,  10b ASR,  11b ROR
   Most but not all ARM and Thumb integer insns use this encoding.
   Be careful to ensure the right value is passed here.
*/
static void compute_result_and_C_after_shift_by_imm5 (
               /*OUT*/HChar* buf,
               /*OUT*/IRTemp* res,
               /*OUT*/IRTemp* newC,
               IRTemp  rMt,       /* reg to shift */
               UInt    how,       /* what kind of shift */
               UInt    shift_amt, /* shift amount (0..31) */
               UInt    rM         /* only for debug printing */
            )
{
   vassert(shift_amt < 32);
   vassert(how < 4);

   switch (how) {

      case 0:
         compute_result_and_C_after_LSL_by_imm5(
            buf, res, newC, rMt, shift_amt, rM
         );
         break;

      case 1:
         compute_result_and_C_after_LSR_by_imm5(
            buf, res, newC, rMt, shift_amt, rM
         );
         break;

      case 2:
         compute_result_and_C_after_ASR_by_imm5(
            buf, res, newC, rMt, shift_amt, rM
         );
         break;

      case 3:
         if (shift_amt == 0) {
            IRTemp oldcT = newTemp(Ity_I32);
            // rotate right 1 bit through carry (?)
            // RRX -- described at ARM ARM A5-17
            // res  = (oldC << 31) | (Rm >>u 1)
            // newC = Rm[0]
            if (newC) {
               assign( *newC,
                       binop(Iop_And32, mkexpr(rMt), mkU32(1)));
            }
            assign( oldcT, mk_armg_calculate_flag_c() );
            assign( *res, 
                    binop(Iop_Or32,
                          binop(Iop_Shl32, mkexpr(oldcT), mkU8(31)),
                          binop(Iop_Shr32, mkexpr(rMt), mkU8(1))) );
            DIS(buf, "r%u, RRX", rM);
         } else {
            // rotate right in range 1..31
            // res  = Rm `ror` shift_amt
            // newC = Rm[shift_amt - 1]
            vassert(shift_amt >= 1 && shift_amt <= 31);
            if (newC) {
               assign( *newC,
                       binop(Iop_And32,
                             binop(Iop_Shr32, mkexpr(rMt), 
                                              mkU8(shift_amt - 1)),
                             mkU32(1)));
            }
            assign( *res,
                    binop(Iop_Or32,
                          binop(Iop_Shr32, mkexpr(rMt), mkU8(shift_amt)),
                          binop(Iop_Shl32, mkexpr(rMt),
                                           mkU8(32-shift_amt))));
            DIS(buf, "r%u, ROR #%u", rM, shift_amt);
         }
         break;

      default:
         /*NOTREACHED*/
         vassert(0);
   }
}


/* Generate an expression corresponding to the register-shift case of
   a shifter operand.  This is used both for ARM and Thumb2.

   Bind it to a temporary, and return that via *res.  If newC is
   non-NULL, also compute a value for the shifter's carry out (in the
   LSB of a word), bind it to a temporary, and return that via *shco.

   Generates GETs from the guest state and is therefore not safe to
   use once we start doing PUTs to it, for any given instruction.

   'how' is encoded thusly:
      00b LSL,  01b LSR,  10b ASR,  11b ROR
   Most but not all ARM and Thumb integer insns use this encoding.
   Be careful to ensure the right value is passed here.
*/
static void compute_result_and_C_after_shift_by_reg (
               /*OUT*/HChar*  buf,
               /*OUT*/IRTemp* res,
               /*OUT*/IRTemp* newC,
               IRTemp  rMt,       /* reg to shift */
               UInt    how,       /* what kind of shift */
               IRTemp  rSt,       /* shift amount */
               UInt    rM,        /* only for debug printing */
               UInt    rS         /* only for debug printing */
            )
{
   vassert(how < 4);
   switch (how) {
      case 0: { /* LSL */
         compute_result_and_C_after_LSL_by_reg(
            buf, res, newC, rMt, rSt, rM, rS
         );
         break;
      }
      case 1: { /* LSR */
         compute_result_and_C_after_LSR_by_reg(
            buf, res, newC, rMt, rSt, rM, rS
         );
         break;
      }
      case 2: { /* ASR */
         compute_result_and_C_after_ASR_by_reg(
            buf, res, newC, rMt, rSt, rM, rS
         );
         break;
      }
      case 3: { /* ROR */
         compute_result_and_C_after_ROR_by_reg(
             buf, res, newC, rMt, rSt, rM, rS
         );
         break;
      }
      default:
         /*NOTREACHED*/
         vassert(0);
   }
}


/* Generate an expression corresponding to a shifter_operand, bind it
   to a temporary, and return that via *shop.  If shco is non-NULL,
   also compute a value for the shifter's carry out (in the LSB of a
   word), bind it to a temporary, and return that via *shco.

   If for some reason we can't come up with a shifter operand (missing
   case?  not really a shifter operand?) return False.

   Generates GETs from the guest state and is therefore not safe to
   use once we start doing PUTs to it, for any given instruction.

   For ARM insns only; not for Thumb.
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
      assign(rMt, getIRegA(rM));

      vassert(shift_amt <= 31);

      compute_result_and_C_after_shift_by_imm5(
         buf, shop, shco, rMt, how, shift_amt, rM
      );
      return True;
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

      assign(rMt, getIRegA(rM));
      assign(rSt, getIRegA(rS));

      compute_result_and_C_after_shift_by_reg(
         buf, shop, shco, rMt, how, rSt, rM, rS
      );
      return True;
   }

   vex_printf("mk_shifter_operand(0x%x,0x%x)\n", insn_25, insn_11_0 );
   return False;
}


/* ARM only */
static 
IRExpr* mk_EA_reg_plusminus_imm12 ( UInt rN, UInt bU, UInt imm12,
                                    /*OUT*/HChar* buf )
{
   vassert(rN < 16);
   vassert(bU < 2);
   vassert(imm12 < 0x1000);
   HChar opChar = bU == 1 ? '+' : '-';
   DIS(buf, "[r%u, #%c%u]", rN, opChar, imm12);
   return
      binop( (bU == 1 ? Iop_Add32 : Iop_Sub32),
             getIRegA(rN),
             mkU32(imm12) );
}


/* ARM only.
   NB: This is "DecodeImmShift" in newer versions of the the ARM ARM.
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
   HChar   opChar = bU == 1 ? '+' : '-';
   IRExpr* index  = NULL;
   switch (sh2) {
      case 0: /* LSL */
         /* imm5 can be in the range 0 .. 31 inclusive. */
         index = binop(Iop_Shl32, getIRegA(rM), mkU8(imm5));
         DIS(buf, "[r%u, %c r%u LSL #%u]", rN, opChar, rM, imm5); 
         break;
      case 1: /* LSR */
         if (imm5 == 0) {
            index = mkU32(0);
            vassert(0); // ATC
         } else {
            index = binop(Iop_Shr32, getIRegA(rM), mkU8(imm5));
         }
         DIS(buf, "[r%u, %cr%u, LSR #%u]",
                  rN, opChar, rM, imm5 == 0 ? 32 : imm5); 
         break;
      case 2: /* ASR */
         /* Doesn't this just mean that the behaviour with imm5 == 0
            is the same as if it had been 31 ? */
         if (imm5 == 0) {
            index = binop(Iop_Sar32, getIRegA(rM), mkU8(31));
            vassert(0); // ATC
         } else {
            index = binop(Iop_Sar32, getIRegA(rM), mkU8(imm5));
         }
         DIS(buf, "[r%u, %cr%u, ASR #%u]",
                  rN, opChar, rM, imm5 == 0 ? 32 : imm5); 
         break;
      case 3: /* ROR or RRX */
         if (imm5 == 0) {
            IRTemp rmT    = newTemp(Ity_I32);
            IRTemp cflagT = newTemp(Ity_I32);
            assign(rmT, getIRegA(rM));
            assign(cflagT, mk_armg_calculate_flag_c());
            index = binop(Iop_Or32, 
                          binop(Iop_Shl32, mkexpr(cflagT), mkU8(31)),
                          binop(Iop_Shr32, mkexpr(rmT), mkU8(1)));
            DIS(buf, "[r%u, %cr%u, RRX]", rN, opChar, rM);
         } else {
            IRTemp rmT = newTemp(Ity_I32);
            assign(rmT, getIRegA(rM));
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
                getIRegA(rN), index);
}


/* ARM only */
static 
IRExpr* mk_EA_reg_plusminus_imm8 ( UInt rN, UInt bU, UInt imm8,
                                   /*OUT*/HChar* buf )
{
   vassert(rN < 16);
   vassert(bU < 2);
   vassert(imm8 < 0x100);
   HChar opChar = bU == 1 ? '+' : '-';
   DIS(buf, "[r%u, #%c%u]", rN, opChar, imm8);
   return
      binop( (bU == 1 ? Iop_Add32 : Iop_Sub32),
             getIRegA(rN),
             mkU32(imm8) );
}


/* ARM only */
static
IRExpr* mk_EA_reg_plusminus_reg ( UInt rN, UInt bU, UInt rM,
                                  /*OUT*/HChar* buf )
{
   vassert(rN < 16);
   vassert(bU < 2);
   vassert(rM < 16);
   HChar   opChar = bU == 1 ? '+' : '-';
   IRExpr* index  = getIRegA(rM);
   DIS(buf, "[r%u, %c r%u]", rN, opChar, rM); 
   return binop(bU == 1 ? Iop_Add32 : Iop_Sub32,
                getIRegA(rN), index);
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


/* Thumb32 only.  This is "ThumbExpandImm" in the ARM ARM.  If
   updatesC is non-NULL, a boolean is written to it indicating whether
   or not the C flag is updated, as per ARM ARM "ThumbExpandImm_C".
*/
static UInt thumbExpandImm ( Bool* updatesC,
                             UInt imm1, UInt imm3, UInt imm8 )
{
   vassert(imm1 < (1<<1));
   vassert(imm3 < (1<<3));
   vassert(imm8 < (1<<8));
   UInt i_imm3_a = (imm1 << 4) | (imm3 << 1) | ((imm8 >> 7) & 1);
   UInt abcdefgh = imm8;
   UInt lbcdefgh = imm8 | 0x80;
   if (updatesC) {
      *updatesC = i_imm3_a >= 8;
   }
   switch (i_imm3_a) {
      case 0: case 1:
         return abcdefgh;
      case 2: case 3:
         return (abcdefgh << 16) | abcdefgh;
      case 4: case 5:
         return (abcdefgh << 24) | (abcdefgh << 8);
      case 6: case 7:
         return (abcdefgh << 24) | (abcdefgh << 16)
                | (abcdefgh << 8) | abcdefgh;
      case 8 ... 31:
         return lbcdefgh << (32 - i_imm3_a);
      default:
         break;
   }
   /*NOTREACHED*/vassert(0);
}


/* Version of thumbExpandImm where we simply feed it the
   instruction halfwords (the lowest addressed one is I0). */
static UInt thumbExpandImm_from_I0_I1 ( Bool* updatesC,
                                        UShort i0s, UShort i1s )
{
   UInt i0    = (UInt)i0s;
   UInt i1    = (UInt)i1s;
   UInt imm1  = SLICE_UInt(i0,10,10);
   UInt imm3  = SLICE_UInt(i1,14,12);
   UInt imm8  = SLICE_UInt(i1,7,0);
   return thumbExpandImm(updatesC, imm1, imm3, imm8);
}


/* Thumb16 only.  Given the firstcond and mask fields from an IT
   instruction, compute the 32-bit ITSTATE value implied, as described
   in libvex_guest_arm.h.  This is not the ARM ARM representation.
   Also produce the t/e chars for the 2nd, 3rd, 4th insns, for
   disassembly printing.  Returns False if firstcond or mask
   denote something invalid.

   The number and conditions for the instructions to be
   conditionalised depend on firstcond and mask:

   mask      cond 1    cond 2      cond 3      cond 4

   1000      fc[3:0]
   x100      fc[3:0]   fc[3:1]:x
   xy10      fc[3:0]   fc[3:1]:x   fc[3:1]:y
   xyz1      fc[3:0]   fc[3:1]:x   fc[3:1]:y   fc[3:1]:z

   The condition fields are assembled in *itstate backwards (cond 4 at
   the top, cond 1 at the bottom).  Conditions are << 4'd and then
   ^0xE'd, and those fields that correspond to instructions in the IT
   block are tagged with a 1 bit.
*/
static Bool compute_ITSTATE ( /*OUT*/UInt*  itstate,
                              /*OUT*/HChar* ch1,
                              /*OUT*/HChar* ch2,
                              /*OUT*/HChar* ch3,
                              UInt firstcond, UInt mask )
{
   vassert(firstcond <= 0xF);
   vassert(mask <= 0xF);
   *itstate = 0;
   *ch1 = *ch2 = *ch3 = '.';
   if (mask == 0)
      return False; /* the logic below actually ensures this anyway,
                       but clearer to make it explicit. */
   if (firstcond == 0xF)
      return False; /* NV is not allowed */
   if (firstcond == 0xE && popcount32(mask) != 1) 
      return False; /* if firstcond is AL then all the rest must be too */

   UInt m3 = (mask >> 3) & 1;
   UInt m2 = (mask >> 2) & 1;
   UInt m1 = (mask >> 1) & 1;
   UInt m0 = (mask >> 0) & 1;

   UInt fc = (firstcond << 4) | 1/*in-IT-block*/;
   UInt ni = (0xE/*AL*/ << 4) | 0/*not-in-IT-block*/;

   if (m3 == 1 && (m2|m1|m0) == 0) {
      *itstate = (ni << 24) | (ni << 16) | (ni << 8) | fc;
      *itstate ^= 0xE0E0E0E0;
      return True;
   }

   if (m2 == 1 && (m1|m0) == 0) {
      *itstate = (ni << 24) | (ni << 16) | (setbit32(fc, 4, m3) << 8) | fc;
      *itstate ^= 0xE0E0E0E0;
      *ch1 = m3 == (firstcond & 1) ? 't' : 'e';
      return True;
   }

   if (m1 == 1 && m0 == 0) {
      *itstate = (ni << 24)
                 | (setbit32(fc, 4, m2) << 16)
                 | (setbit32(fc, 4, m3) << 8) | fc;
      *itstate ^= 0xE0E0E0E0;
      *ch1 = m3 == (firstcond & 1) ? 't' : 'e';
      *ch2 = m2 == (firstcond & 1) ? 't' : 'e';
      return True;
   }

   if (m0 == 1) {
      *itstate = (setbit32(fc, 4, m1) << 24)
                 | (setbit32(fc, 4, m2) << 16)
                 | (setbit32(fc, 4, m3) << 8) | fc;
      *itstate ^= 0xE0E0E0E0;
      *ch1 = m3 == (firstcond & 1) ? 't' : 'e';
      *ch2 = m2 == (firstcond & 1) ? 't' : 'e';
      *ch3 = m1 == (firstcond & 1) ? 't' : 'e';
      return True;
   }

   return False;
}


/* Generate IR to do 32-bit bit reversal, a la Hacker's Delight
   Chapter 7 Section 1. */
static IRTemp gen_BITREV ( IRTemp x0 )
{
   IRTemp x1 = newTemp(Ity_I32);
   IRTemp x2 = newTemp(Ity_I32);
   IRTemp x3 = newTemp(Ity_I32);
   IRTemp x4 = newTemp(Ity_I32);
   IRTemp x5 = newTemp(Ity_I32);
   UInt   c1 = 0x55555555;
   UInt   c2 = 0x33333333;
   UInt   c3 = 0x0F0F0F0F;
   UInt   c4 = 0x00FF00FF;
   UInt   c5 = 0x0000FFFF;
   assign(x1,
          binop(Iop_Or32,
                binop(Iop_Shl32,
                      binop(Iop_And32, mkexpr(x0), mkU32(c1)),
                      mkU8(1)),
                binop(Iop_Shr32,
                      binop(Iop_And32, mkexpr(x0), mkU32(~c1)),
                      mkU8(1))
   ));
   assign(x2,
          binop(Iop_Or32,
                binop(Iop_Shl32,
                      binop(Iop_And32, mkexpr(x1), mkU32(c2)),
                      mkU8(2)),
                binop(Iop_Shr32,
                      binop(Iop_And32, mkexpr(x1), mkU32(~c2)),
                      mkU8(2))
   ));
   assign(x3,
          binop(Iop_Or32,
                binop(Iop_Shl32,
                      binop(Iop_And32, mkexpr(x2), mkU32(c3)),
                      mkU8(4)),
                binop(Iop_Shr32,
                      binop(Iop_And32, mkexpr(x2), mkU32(~c3)),
                      mkU8(4))
   ));
   assign(x4,
          binop(Iop_Or32,
                binop(Iop_Shl32,
                      binop(Iop_And32, mkexpr(x3), mkU32(c4)),
                      mkU8(8)),
                binop(Iop_Shr32,
                      binop(Iop_And32, mkexpr(x3), mkU32(~c4)),
                      mkU8(8))
   ));
   assign(x5,
          binop(Iop_Or32,
                binop(Iop_Shl32,
                      binop(Iop_And32, mkexpr(x4), mkU32(c5)),
                      mkU8(16)),
                binop(Iop_Shr32,
                      binop(Iop_And32, mkexpr(x4), mkU32(~c5)),
                      mkU8(16))
   ));
   return x5;
}


/* Generate IR to do rearrange bytes 3:2:1:0 in a word in to the order
   0:1:2:3 (aka byte-swap). */
static IRTemp gen_REV ( IRTemp arg )
{
   IRTemp res = newTemp(Ity_I32);
   assign(res, 
          binop(Iop_Or32,
                binop(Iop_Shl32, mkexpr(arg), mkU8(24)),
          binop(Iop_Or32,
                binop(Iop_And32, binop(Iop_Shl32, mkexpr(arg), mkU8(8)), 
                                 mkU32(0x00FF0000)),
          binop(Iop_Or32,
                binop(Iop_And32, binop(Iop_Shr32, mkexpr(arg), mkU8(8)),
                                       mkU32(0x0000FF00)),
                binop(Iop_And32, binop(Iop_Shr32, mkexpr(arg), mkU8(24)),
                                       mkU32(0x000000FF) )
   ))));
   return res;
}


/* Generate IR to do rearrange bytes 3:2:1:0 in a word in to the order
   2:3:0:1 (swap within lo and hi halves). */
static IRTemp gen_REV16 ( IRTemp arg )
{
   IRTemp res = newTemp(Ity_I32);
   assign(res,
          binop(Iop_Or32,
                binop(Iop_And32,
                      binop(Iop_Shl32, mkexpr(arg), mkU8(8)),
                      mkU32(0xFF00FF00)),
                binop(Iop_And32,
                      binop(Iop_Shr32, mkexpr(arg), mkU8(8)),
                      mkU32(0x00FF00FF))));
   return res;
}


/*------------------------------------------------------------*/
/*--- Advanced SIMD (NEON) instructions                    ---*/
/*------------------------------------------------------------*/

/*------------------------------------------------------------*/
/*--- NEON data processing                                 ---*/
/*------------------------------------------------------------*/

/* For all NEON DP ops, we use the normal scheme to handle conditional
   writes to registers -- pass in condT and hand that on to the
   put*Reg functions.  In ARM mode condT is always IRTemp_INVALID
   since NEON is unconditional for ARM.  In Thumb mode condT is
   derived from the ITSTATE shift register in the normal way. */

static
UInt get_neon_d_regno(UInt theInstr)
{
   UInt x = ((theInstr >> 18) & 0x10) | ((theInstr >> 12) & 0xF);
   if (theInstr & 0x40) {
      if (x & 1) {
         x = x + 0x100;
      } else {
         x = x >> 1;
      }
   }
   return x;
}

static
UInt get_neon_n_regno(UInt theInstr)
{
   UInt x = ((theInstr >> 3) & 0x10) | ((theInstr >> 16) & 0xF);
   if (theInstr & 0x40) {
      if (x & 1) {
         x = x + 0x100;
      } else {
         x = x >> 1;
      }
   }
   return x;
}

static
UInt get_neon_m_regno(UInt theInstr)
{
   UInt x = ((theInstr >> 1) & 0x10) | (theInstr & 0xF);
   if (theInstr & 0x40) {
      if (x & 1) {
         x = x + 0x100;
      } else {
         x = x >> 1;
      }
   }
   return x;
}

static
Bool dis_neon_vext ( UInt theInstr, IRTemp condT )
{
   UInt dreg = get_neon_d_regno(theInstr);
   UInt mreg = get_neon_m_regno(theInstr);
   UInt nreg = get_neon_n_regno(theInstr);
   UInt imm4 = (theInstr >> 8) & 0xf;
   UInt Q = (theInstr >> 6) & 1;
   HChar reg_t = Q ? 'q' : 'd';

   if (Q) {
      putQReg(dreg, triop(Iop_SliceV128, /*hiV128*/getQReg(mreg),
                          /*loV128*/getQReg(nreg), mkU8(imm4)), condT);
   } else {
      putDRegI64(dreg, triop(Iop_Slice64, /*hiI64*/getDRegI64(mreg),
                             /*loI64*/getDRegI64(nreg), mkU8(imm4)), condT);
   }
   DIP("vext.8 %c%u, %c%u, %c%u, #%u\n", reg_t, dreg, reg_t, nreg,
                                         reg_t, mreg, imm4);
   return True;
}

/* Generate specific vector FP binary ops, possibly with a fake
   rounding mode as required by the primop. */
static
IRExpr* binop_w_fake_RM ( IROp op, IRExpr* argL, IRExpr* argR )
{
   switch (op) {
      case Iop_Add32Fx4:
      case Iop_Sub32Fx4:
      case Iop_Mul32Fx4:
         return triop(op, get_FAKE_roundingmode(), argL, argR );
      case Iop_Add32x4: case Iop_Add16x8:
      case Iop_Sub32x4: case Iop_Sub16x8:
      case Iop_Mul32x4: case Iop_Mul16x8:
      case Iop_Mul32x2: case Iop_Mul16x4:
      case Iop_Add32Fx2:
      case Iop_Sub32Fx2:
      case Iop_Mul32Fx2:
      case Iop_PwAdd32Fx2:
         return binop(op, argL, argR);
      default:
        ppIROp(op);
        vassert(0);
   }
}

/* VTBL, VTBX */
static
Bool dis_neon_vtb ( UInt theInstr, IRTemp condT )
{
   UInt op = (theInstr >> 6) & 1;
   UInt dreg = get_neon_d_regno(theInstr & ~(1 << 6));
   UInt nreg = get_neon_n_regno(theInstr & ~(1 << 6));
   UInt mreg = get_neon_m_regno(theInstr & ~(1 << 6));
   UInt len = (theInstr >> 8) & 3;
   Int i;
   IROp cmp;
   ULong imm;
   IRTemp arg_l;
   IRTemp old_mask, new_mask, cur_mask;
   IRTemp old_res, new_res;
   IRTemp old_arg, new_arg;

   if (dreg >= 0x100 || mreg >= 0x100 || nreg >= 0x100)
      return False;
   if (nreg + len > 31)
      return False;

   cmp = Iop_CmpGT8Ux8;

   old_mask = newTemp(Ity_I64);
   old_res = newTemp(Ity_I64);
   old_arg = newTemp(Ity_I64);
   assign(old_mask, mkU64(0));
   assign(old_res, mkU64(0));
   assign(old_arg, getDRegI64(mreg));
   imm = 8;
   imm = (imm <<  8) | imm;
   imm = (imm << 16) | imm;
   imm = (imm << 32) | imm;

   for (i = 0; i <= len; i++) {
      arg_l = newTemp(Ity_I64);
      new_mask = newTemp(Ity_I64);
      cur_mask = newTemp(Ity_I64);
      new_res = newTemp(Ity_I64);
      new_arg = newTemp(Ity_I64);
      assign(arg_l, getDRegI64(nreg+i));
      assign(new_arg, binop(Iop_Sub8x8, mkexpr(old_arg), mkU64(imm)));
      assign(cur_mask, binop(cmp, mkU64(imm), mkexpr(old_arg)));
      assign(new_mask, binop(Iop_Or64, mkexpr(old_mask), mkexpr(cur_mask)));
      assign(new_res, binop(Iop_Or64,
                            mkexpr(old_res),
                            binop(Iop_And64,
                                  binop(Iop_Perm8x8,
                                        mkexpr(arg_l),
                                        binop(Iop_And64,
                                              mkexpr(old_arg),
                                              mkexpr(cur_mask))),
                                  mkexpr(cur_mask))));

      old_arg = new_arg;
      old_mask = new_mask;
      old_res = new_res;
   }
   if (op) {
      new_res = newTemp(Ity_I64);
      assign(new_res, binop(Iop_Or64,
                            binop(Iop_And64,
                                  getDRegI64(dreg),
                                  unop(Iop_Not64, mkexpr(old_mask))),
                            mkexpr(old_res)));
      old_res = new_res;
   }

   putDRegI64(dreg, mkexpr(old_res), condT);
   DIP("vtb%c.8 d%u, {", op ? 'x' : 'l', dreg);
   if (len > 0) {
      DIP("d%u-d%u", nreg, nreg + len);
   } else {
      DIP("d%u", nreg);
   }
   DIP("}, d%u\n", mreg);
   return True;
}

/* VDUP (scalar)  */
static
Bool dis_neon_vdup ( UInt theInstr, IRTemp condT )
{
   UInt Q = (theInstr >> 6) & 1;
   UInt dreg = ((theInstr >> 18) & 0x10) | ((theInstr >> 12) & 0xF);
   UInt mreg = ((theInstr >> 1) & 0x10) | (theInstr & 0xF);
   UInt imm4 = (theInstr >> 16) & 0xF;
   UInt index;
   UInt size;
   IRTemp arg_m;
   IRTemp res;
   IROp op, op2;

   if ((imm4 == 0) || (imm4 == 8))
      return False;
   if ((Q == 1) && ((dreg & 1) == 1))
      return False;
   if (Q)
      dreg >>= 1;
   arg_m = newTemp(Ity_I64);
   assign(arg_m, getDRegI64(mreg));
   if (Q)
      res = newTemp(Ity_V128);
   else
      res = newTemp(Ity_I64);
   if ((imm4 & 1) == 1) {
      op = Q ? Iop_Dup8x16 : Iop_Dup8x8;
      op2 = Iop_GetElem8x8;
      index = imm4 >> 1;
      size = 8;
   } else if ((imm4 & 3) == 2) {
      op = Q ? Iop_Dup16x8 : Iop_Dup16x4;
      op2 = Iop_GetElem16x4;
      index = imm4 >> 2;
      size = 16;
   } else if ((imm4 & 7) == 4) {
      op = Q ? Iop_Dup32x4 : Iop_Dup32x2;
      op2 = Iop_GetElem32x2;
      index = imm4 >> 3;
      size = 32;
   } else {
      return False; // can this ever happen?
   }
   assign(res, unop(op, binop(op2, mkexpr(arg_m), mkU8(index))));
   if (Q) {
      putQReg(dreg, mkexpr(res), condT);
   } else {
      putDRegI64(dreg, mkexpr(res), condT);
   }
   DIP("vdup.%u %c%u, d%u[%u]\n", size, Q ? 'q' : 'd', dreg, mreg, index);
   return True;
}

/* A7.4.1 Three registers of the same length */
static
Bool dis_neon_data_3same ( UInt theInstr, IRTemp condT )
{
   /* In paths where this returns False, indicating a non-decodable
      instruction, there may still be some IR assignments to temporaries
      generated.  This is inconvenient but harmless, and the post-front-end
      IR optimisation pass will just remove them anyway.  So there's no
      effort made here to tidy it up.
   */
   UInt Q = (theInstr >> 6) & 1;
   UInt dreg = get_neon_d_regno(theInstr);
   UInt nreg = get_neon_n_regno(theInstr);
   UInt mreg = get_neon_m_regno(theInstr);
   UInt A = (theInstr >> 8) & 0xF;
   UInt B = (theInstr >> 4) & 1;
   UInt C = (theInstr >> 20) & 0x3;
   UInt U = (theInstr >> 24) & 1;
   UInt size = C;

   IRTemp arg_n;
   IRTemp arg_m;
   IRTemp res;

   if (Q) {
      arg_n = newTemp(Ity_V128);
      arg_m = newTemp(Ity_V128);
      res = newTemp(Ity_V128);
      assign(arg_n, getQReg(nreg));
      assign(arg_m, getQReg(mreg));
   } else {
      arg_n = newTemp(Ity_I64);
      arg_m = newTemp(Ity_I64);
      res = newTemp(Ity_I64);
      assign(arg_n, getDRegI64(nreg));
      assign(arg_m, getDRegI64(mreg));
   }

   switch(A) {
      case 0:
         if (B == 0) {
            /* VHADD */
            ULong imm = 0;
            IRExpr *imm_val;
            IROp addOp;
            IROp andOp;
            IROp shOp;
            HChar regType = Q ? 'q' : 'd';

            if (size == 3)
               return False;
            switch(size) {
               case 0: imm = 0x101010101010101LL; break;
               case 1: imm = 0x1000100010001LL; break;
               case 2: imm = 0x100000001LL; break;
               default: vassert(0);
            }
            if (Q) {
               imm_val = binop(Iop_64HLtoV128, mkU64(imm), mkU64(imm));
               andOp = Iop_AndV128;
            } else {
               imm_val = mkU64(imm);
               andOp = Iop_And64;
            }
            if (U) {
               switch(size) {
                  case 0:
                     addOp = Q ? Iop_Add8x16 : Iop_Add8x8;
                     shOp = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     break;
                  case 1:
                     addOp = Q ? Iop_Add16x8 : Iop_Add16x4;
                     shOp = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     break;
                  case 2:
                     addOp = Q ? Iop_Add32x4 : Iop_Add32x2;
                     shOp = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     break;
                  default:
                     vassert(0);
               }
            } else {
               switch(size) {
                  case 0:
                     addOp = Q ? Iop_Add8x16 : Iop_Add8x8;
                     shOp = Q ? Iop_SarN8x16 : Iop_SarN8x8;
                     break;
                  case 1:
                     addOp = Q ? Iop_Add16x8 : Iop_Add16x4;
                     shOp = Q ? Iop_SarN16x8 : Iop_SarN16x4;
                     break;
                  case 2:
                     addOp = Q ? Iop_Add32x4 : Iop_Add32x2;
                     shOp = Q ? Iop_SarN32x4 : Iop_SarN32x2;
                     break;
                  default:
                     vassert(0);
               }
            }
            assign(res,
                   binop(addOp,
                         binop(addOp,
                               binop(shOp, mkexpr(arg_m), mkU8(1)),
                               binop(shOp, mkexpr(arg_n), mkU8(1))),
                         binop(shOp,
                               binop(addOp,
                                     binop(andOp, mkexpr(arg_m), imm_val),
                                     binop(andOp, mkexpr(arg_n), imm_val)),
                               mkU8(1))));
            DIP("vhadd.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size, regType,
                dreg, regType, nreg, regType, mreg);
         } else {
            /* VQADD */
            IROp op, op2;
            IRTemp tmp;
            HChar reg_t = Q ? 'q' : 'd';
            if (Q) {
               switch (size) {
                  case 0:
                     op = U ? Iop_QAdd8Ux16 : Iop_QAdd8Sx16;
                     op2 = Iop_Add8x16;
                     break;
                  case 1:
                     op = U ? Iop_QAdd16Ux8 : Iop_QAdd16Sx8;
                     op2 = Iop_Add16x8;
                     break;
                  case 2:
                     op = U ? Iop_QAdd32Ux4 : Iop_QAdd32Sx4;
                     op2 = Iop_Add32x4;
                     break;
                  case 3:
                     op = U ? Iop_QAdd64Ux2 : Iop_QAdd64Sx2;
                     op2 = Iop_Add64x2;
                     break;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op = U ? Iop_QAdd8Ux8 : Iop_QAdd8Sx8;
                     op2 = Iop_Add8x8;
                     break;
                  case 1:
                     op = U ? Iop_QAdd16Ux4 : Iop_QAdd16Sx4;
                     op2 = Iop_Add16x4;
                     break;
                  case 2:
                     op = U ? Iop_QAdd32Ux2 : Iop_QAdd32Sx2;
                     op2 = Iop_Add32x2;
                     break;
                  case 3:
                     op = U ? Iop_QAdd64Ux1 : Iop_QAdd64Sx1;
                     op2 = Iop_Add64;
                     break;
                  default:
                     vassert(0);
               }
            }
            if (Q) {
               tmp = newTemp(Ity_V128);
            } else {
               tmp = newTemp(Ity_I64);
            }
            assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
            assign(tmp, binop(op2, mkexpr(arg_n), mkexpr(arg_m)));
            setFlag_QC(mkexpr(res), mkexpr(tmp), Q, condT);
            DIP("vqadd.%c%d %c%u %c%u, %c%u\n",
                U ? 'u' : 's',
                8 << size, reg_t, dreg, reg_t, nreg, reg_t, mreg);
         }
         break;
      case 1:
         if (B == 0) {
            /* VRHADD */
            /* VRHADD C, A, B ::=
                 C = (A >> 1) + (B >> 1) + (((A & 1) + (B & 1) + 1) >> 1) */
            IROp shift_op, add_op;
            IRTemp cc;
            ULong one = 1;
            HChar reg_t = Q ? 'q' : 'd';
            switch (size) {
               case 0: one = (one <<  8) | one; /* fall through */
               case 1: one = (one << 16) | one; /* fall through */
               case 2: one = (one << 32) | one; break;
               case 3: return False;
               default: vassert(0);
            }
            if (Q) {
               switch (size) {
                  case 0:
                     shift_op = U ? Iop_ShrN8x16 : Iop_SarN8x16;
                     add_op = Iop_Add8x16;
                     break;
                  case 1:
                     shift_op = U ? Iop_ShrN16x8 : Iop_SarN16x8;
                     add_op = Iop_Add16x8;
                     break;
                  case 2:
                     shift_op = U ? Iop_ShrN32x4 : Iop_SarN32x4;
                     add_op = Iop_Add32x4;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     shift_op = U ? Iop_ShrN8x8 : Iop_SarN8x8;
                     add_op = Iop_Add8x8;
                     break;
                  case 1:
                     shift_op = U ? Iop_ShrN16x4 : Iop_SarN16x4;
                     add_op = Iop_Add16x4;
                     break;
                  case 2:
                     shift_op = U ? Iop_ShrN32x2 : Iop_SarN32x2;
                     add_op = Iop_Add32x2;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            }
            if (Q) {
               cc = newTemp(Ity_V128);
               assign(cc, binop(shift_op,
                                binop(add_op,
                                      binop(add_op,
                                            binop(Iop_AndV128,
                                                  mkexpr(arg_n),
                                                  binop(Iop_64HLtoV128,
                                                        mkU64(one),
                                                        mkU64(one))),
                                            binop(Iop_AndV128,
                                                  mkexpr(arg_m),
                                                  binop(Iop_64HLtoV128,
                                                        mkU64(one),
                                                        mkU64(one)))),
                                      binop(Iop_64HLtoV128,
                                            mkU64(one),
                                            mkU64(one))),
                                mkU8(1)));
               assign(res, binop(add_op,
                                 binop(add_op,
                                       binop(shift_op,
                                             mkexpr(arg_n),
                                             mkU8(1)),
                                       binop(shift_op,
                                             mkexpr(arg_m),
                                             mkU8(1))),
                                 mkexpr(cc)));
            } else {
               cc = newTemp(Ity_I64);
               assign(cc, binop(shift_op,
                                binop(add_op,
                                      binop(add_op,
                                            binop(Iop_And64,
                                                  mkexpr(arg_n),
                                                  mkU64(one)),
                                            binop(Iop_And64,
                                                  mkexpr(arg_m),
                                                  mkU64(one))),
                                      mkU64(one)),
                                mkU8(1)));
               assign(res, binop(add_op,
                                 binop(add_op,
                                       binop(shift_op,
                                             mkexpr(arg_n),
                                             mkU8(1)),
                                       binop(shift_op,
                                             mkexpr(arg_m),
                                             mkU8(1))),
                                 mkexpr(cc)));
            }
            DIP("vrhadd.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's',
                8 << size, reg_t, dreg, reg_t, nreg, reg_t, mreg);
         } else {
            if (U == 0)  {
               switch(C) {
                  case 0: {
                     /* VAND  */
                     HChar reg_t = Q ? 'q' : 'd';
                     if (Q) {
                        assign(res, binop(Iop_AndV128, mkexpr(arg_n), 
                                                       mkexpr(arg_m)));
                     } else {
                        assign(res, binop(Iop_And64, mkexpr(arg_n),
                                                     mkexpr(arg_m)));
                     }
                     DIP("vand %c%u, %c%u, %c%u\n",
                         reg_t, dreg, reg_t, nreg, reg_t, mreg);
                     break;
                  }
                  case 1: {
                     /* VBIC  */
                     HChar reg_t = Q ? 'q' : 'd';
                     if (Q) {
                        assign(res, binop(Iop_AndV128,mkexpr(arg_n),
                               unop(Iop_NotV128, mkexpr(arg_m))));
                     } else {
                        assign(res, binop(Iop_And64, mkexpr(arg_n),
                               unop(Iop_Not64, mkexpr(arg_m))));
                     }
                     DIP("vbic %c%u, %c%u, %c%u\n",
                         reg_t, dreg, reg_t, nreg, reg_t, mreg);
                     break;
                  }
                  case 2:
                     if ( nreg != mreg) {
                        /* VORR  */
                        HChar reg_t = Q ? 'q' : 'd';
                        if (Q) {
                           assign(res, binop(Iop_OrV128, mkexpr(arg_n),
                                                         mkexpr(arg_m)));
                        } else {
                           assign(res, binop(Iop_Or64, mkexpr(arg_n),
                                                       mkexpr(arg_m)));
                        }
                        DIP("vorr %c%u, %c%u, %c%u\n",
                            reg_t, dreg, reg_t, nreg, reg_t, mreg);
                     } else {
                        /* VMOV  */
                        HChar reg_t = Q ? 'q' : 'd';
                        assign(res, mkexpr(arg_m));
                        DIP("vmov %c%u, %c%u\n", reg_t, dreg, reg_t, mreg);
                     }
                     break;
                  case 3:{
                     /* VORN  */
                     HChar reg_t = Q ? 'q' : 'd';
                     if (Q) {
                        assign(res, binop(Iop_OrV128,mkexpr(arg_n),
                               unop(Iop_NotV128, mkexpr(arg_m))));
                     } else {
                        assign(res, binop(Iop_Or64, mkexpr(arg_n),
                               unop(Iop_Not64, mkexpr(arg_m))));
                     }
                     DIP("vorn %c%u, %c%u, %c%u\n",
                         reg_t, dreg, reg_t, nreg, reg_t, mreg);
                     break;
                  }
               }
            } else {
               switch(C) {
                  case 0:
                     /* VEOR (XOR)  */
                     if (Q) {
                        assign(res, binop(Iop_XorV128, mkexpr(arg_n),
                                                       mkexpr(arg_m)));
                     } else {
                        assign(res, binop(Iop_Xor64, mkexpr(arg_n),
                                                     mkexpr(arg_m)));
                     }
                     DIP("veor %c%u, %c%u, %c%u\n", Q ? 'q' : 'd', dreg,
                           Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
                     break;
                  case 1:
                     /* VBSL  */
                     if (Q) {
                        IRTemp reg_d = newTemp(Ity_V128);
                        assign(reg_d, getQReg(dreg));
                        assign(res,
                               binop(Iop_OrV128,
                                     binop(Iop_AndV128, mkexpr(arg_n),
                                                        mkexpr(reg_d)),
                                     binop(Iop_AndV128,
                                           mkexpr(arg_m),
                                           unop(Iop_NotV128,
                                                 mkexpr(reg_d)) ) ) );
                     } else {
                        IRTemp reg_d = newTemp(Ity_I64);
                        assign(reg_d, getDRegI64(dreg));
                        assign(res,
                               binop(Iop_Or64,
                                     binop(Iop_And64, mkexpr(arg_n),
                                                      mkexpr(reg_d)),
                                     binop(Iop_And64,
                                           mkexpr(arg_m),
                                           unop(Iop_Not64, mkexpr(reg_d)))));
                     }
                     DIP("vbsl %c%u, %c%u, %c%u\n",
                         Q ? 'q' : 'd', dreg,
                         Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
                     break;
                  case 2:
                     /* VBIT  */
                     if (Q) {
                        IRTemp reg_d = newTemp(Ity_V128);
                        assign(reg_d, getQReg(dreg));
                        assign(res,
                               binop(Iop_OrV128,
                                     binop(Iop_AndV128, mkexpr(arg_n), 
                                                        mkexpr(arg_m)),
                                     binop(Iop_AndV128,
                                           mkexpr(reg_d),
                                           unop(Iop_NotV128, mkexpr(arg_m)))));
                     } else {
                        IRTemp reg_d = newTemp(Ity_I64);
                        assign(reg_d, getDRegI64(dreg));
                        assign(res,
                               binop(Iop_Or64,
                                     binop(Iop_And64, mkexpr(arg_n),
                                                      mkexpr(arg_m)),
                                     binop(Iop_And64,
                                           mkexpr(reg_d),
                                           unop(Iop_Not64, mkexpr(arg_m)))));
                     }
                     DIP("vbit %c%u, %c%u, %c%u\n",
                         Q ? 'q' : 'd', dreg,
                         Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
                     break;
                  case 3:
                     /* VBIF  */
                     if (Q) {
                        IRTemp reg_d = newTemp(Ity_V128);
                        assign(reg_d, getQReg(dreg));
                        assign(res,
                               binop(Iop_OrV128,
                                     binop(Iop_AndV128, mkexpr(reg_d),
                                                        mkexpr(arg_m)),
                                     binop(Iop_AndV128,
                                           mkexpr(arg_n),
                                           unop(Iop_NotV128, mkexpr(arg_m)))));
                     } else {
                        IRTemp reg_d = newTemp(Ity_I64);
                        assign(reg_d, getDRegI64(dreg));
                        assign(res,
                               binop(Iop_Or64,
                                     binop(Iop_And64, mkexpr(reg_d),
                                                      mkexpr(arg_m)),
                                     binop(Iop_And64,
                                           mkexpr(arg_n),
                                           unop(Iop_Not64, mkexpr(arg_m)))));
                     }
                     DIP("vbif %c%u, %c%u, %c%u\n",
                         Q ? 'q' : 'd', dreg,
                         Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
                     break;
               }
            }
         }
         break;
      case 2:
         if (B == 0) {
            /* VHSUB */
            /* (A >> 1) - (B >> 1) - (NOT (A) & B & 1)   */
            ULong imm = 0;
            IRExpr *imm_val;
            IROp subOp;
            IROp notOp;
            IROp andOp;
            IROp shOp;
            if (size == 3)
               return False;
            switch(size) {
               case 0: imm = 0x101010101010101LL; break;
               case 1: imm = 0x1000100010001LL; break;
               case 2: imm = 0x100000001LL; break;
               default: vassert(0);
            }
            if (Q) {
               imm_val = binop(Iop_64HLtoV128, mkU64(imm), mkU64(imm));
               andOp = Iop_AndV128;
               notOp = Iop_NotV128;
            } else {
               imm_val = mkU64(imm);
               andOp = Iop_And64;
               notOp = Iop_Not64;
            }
            if (U) {
               switch(size) {
                  case 0:
                     subOp = Q ? Iop_Sub8x16 : Iop_Sub8x8;
                     shOp = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     break;
                  case 1:
                     subOp = Q ? Iop_Sub16x8 : Iop_Sub16x4;
                     shOp = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     break;
                  case 2:
                     subOp = Q ? Iop_Sub32x4 : Iop_Sub32x2;
                     shOp = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     break;
                  default:
                     vassert(0);
               }
            } else {
               switch(size) {
                  case 0:
                     subOp = Q ? Iop_Sub8x16 : Iop_Sub8x8;
                     shOp = Q ? Iop_SarN8x16 : Iop_SarN8x8;
                     break;
                  case 1:
                     subOp = Q ? Iop_Sub16x8 : Iop_Sub16x4;
                     shOp = Q ? Iop_SarN16x8 : Iop_SarN16x4;
                     break;
                  case 2:
                     subOp = Q ? Iop_Sub32x4 : Iop_Sub32x2;
                     shOp = Q ? Iop_SarN32x4 : Iop_SarN32x2;
                     break;
                  default:
                     vassert(0);
               }
            }
            assign(res,
                   binop(subOp,
                         binop(subOp,
                               binop(shOp, mkexpr(arg_n), mkU8(1)),
                               binop(shOp, mkexpr(arg_m), mkU8(1))),
                         binop(andOp,
                               binop(andOp,
                                     unop(notOp, mkexpr(arg_n)),
                                     mkexpr(arg_m)),
                               imm_val)));
            DIP("vhsub.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                mreg);
         } else {
            /* VQSUB */
            IROp op, op2;
            IRTemp tmp;
            if (Q) {
               switch (size) {
                  case 0:
                     op = U ? Iop_QSub8Ux16 : Iop_QSub8Sx16;
                     op2 = Iop_Sub8x16;
                     break;
                  case 1:
                     op = U ? Iop_QSub16Ux8 : Iop_QSub16Sx8;
                     op2 = Iop_Sub16x8;
                     break;
                  case 2:
                     op = U ? Iop_QSub32Ux4 : Iop_QSub32Sx4;
                     op2 = Iop_Sub32x4;
                     break;
                  case 3:
                     op = U ? Iop_QSub64Ux2 : Iop_QSub64Sx2;
                     op2 = Iop_Sub64x2;
                     break;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op = U ? Iop_QSub8Ux8 : Iop_QSub8Sx8;
                     op2 = Iop_Sub8x8;
                     break;
                  case 1:
                     op = U ? Iop_QSub16Ux4 : Iop_QSub16Sx4;
                     op2 = Iop_Sub16x4;
                     break;
                  case 2:
                     op = U ? Iop_QSub32Ux2 : Iop_QSub32Sx2;
                     op2 = Iop_Sub32x2;
                     break;
                  case 3:
                     op = U ? Iop_QSub64Ux1 : Iop_QSub64Sx1;
                     op2 = Iop_Sub64;
                     break;
                  default:
                     vassert(0);
               }
            }
            if (Q)
               tmp = newTemp(Ity_V128);
            else
               tmp = newTemp(Ity_I64);
            assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
            assign(tmp, binop(op2, mkexpr(arg_n), mkexpr(arg_m)));
            setFlag_QC(mkexpr(res), mkexpr(tmp), Q, condT);
            DIP("vqsub.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                mreg);
         }
         break;
      case 3: {
            IROp op;
            if (Q) {
               switch (size) {
                  case 0: op = U ? Iop_CmpGT8Ux16 : Iop_CmpGT8Sx16; break;
                  case 1: op = U ? Iop_CmpGT16Ux8 : Iop_CmpGT16Sx8; break;
                  case 2: op = U ? Iop_CmpGT32Ux4 : Iop_CmpGT32Sx4; break;
                  case 3: return False;
                  default: vassert(0);
               }
            } else {
               switch (size) {
                  case 0: op = U ? Iop_CmpGT8Ux8 : Iop_CmpGT8Sx8; break;
                  case 1: op = U ? Iop_CmpGT16Ux4 : Iop_CmpGT16Sx4; break;
                  case 2: op = U ? Iop_CmpGT32Ux2: Iop_CmpGT32Sx2; break;
                  case 3: return False;
                  default: vassert(0);
               }
            }
            if (B == 0) {
               /* VCGT  */
               assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
               DIP("vcgt.%c%d %c%u, %c%u, %c%u\n",
                   U ? 'u' : 's', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                   mreg);
            } else {
               /* VCGE  */
               /* VCGE res, argn, argm
                    is equal to
                  VCGT tmp, argm, argn
                  VNOT res, tmp */
               assign(res,
                      unop(Q ? Iop_NotV128 : Iop_Not64,
                           binop(op, mkexpr(arg_m), mkexpr(arg_n))));
               DIP("vcge.%c%d %c%u, %c%u, %c%u\n",
                   U ? 'u' : 's', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                   mreg);
            }
         }
         break;
      case 4:
         if (B == 0) {
            /* VSHL */
            IROp op = Iop_INVALID, sub_op = Iop_INVALID;
            IRTemp tmp = IRTemp_INVALID;
            if (U) {
               switch (size) {
                  case 0: op = Q ? Iop_Shl8x16 : Iop_Shl8x8; break;
                  case 1: op = Q ? Iop_Shl16x8 : Iop_Shl16x4; break;
                  case 2: op = Q ? Iop_Shl32x4 : Iop_Shl32x2; break;
                  case 3: op = Q ? Iop_Shl64x2 : Iop_Shl64; break;
                  default: vassert(0);
               }
            } else {
               tmp = newTemp(Q ? Ity_V128 : Ity_I64);
               switch (size) {
                  case 0:
                     op = Q ? Iop_Sar8x16 : Iop_Sar8x8;
                     sub_op = Q ? Iop_Sub8x16 : Iop_Sub8x8;
                     break;
                  case 1:
                     op = Q ? Iop_Sar16x8 : Iop_Sar16x4;
                     sub_op = Q ? Iop_Sub16x8 : Iop_Sub16x4;
                     break;
                  case 2:
                     op = Q ? Iop_Sar32x4 : Iop_Sar32x2;
                     sub_op = Q ? Iop_Sub32x4 : Iop_Sub32x2;
                     break;
                  case 3:
                     op = Q ? Iop_Sar64x2 : Iop_Sar64;
                     sub_op = Q ? Iop_Sub64x2 : Iop_Sub64;
                     break;
                  default:
                     vassert(0);
               }
            }
            if (U) {
               if (!Q && (size == 3))
                  assign(res, binop(op, mkexpr(arg_m),
                                        unop(Iop_64to8, mkexpr(arg_n))));
               else
                  assign(res, binop(op, mkexpr(arg_m), mkexpr(arg_n)));
            } else {
               if (Q)
                  assign(tmp, binop(sub_op,
                                    binop(Iop_64HLtoV128, mkU64(0), mkU64(0)),
                                    mkexpr(arg_n)));
               else
                  assign(tmp, binop(sub_op, mkU64(0), mkexpr(arg_n)));
               if (!Q && (size == 3))
                  assign(res, binop(op, mkexpr(arg_m),
                                        unop(Iop_64to8, mkexpr(tmp))));
               else
                  assign(res, binop(op, mkexpr(arg_m), mkexpr(tmp)));
            }
            DIP("vshl.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, Q ? 'q' : 'd',
                nreg);
         } else {
            /* VQSHL */
            IROp op, op_rev, op_shrn, op_shln, cmp_neq, cmp_gt;
            IRTemp tmp, shval, mask, old_shval;
            UInt i;
            ULong esize;
            cmp_neq = Q ? Iop_CmpNEZ8x16 : Iop_CmpNEZ8x8;
            cmp_gt = Q ? Iop_CmpGT8Sx16 : Iop_CmpGT8Sx8;
            if (U) {
               switch (size) {
                  case 0:
                     op = Q ? Iop_QShl8x16 : Iop_QShl8x8;
                     op_rev = Q ? Iop_Shr8x16 : Iop_Shr8x8;
                     op_shrn = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     op_shln = Q ? Iop_ShlN8x16 : Iop_ShlN8x8;
                     break;
                  case 1:
                     op = Q ? Iop_QShl16x8 : Iop_QShl16x4;
                     op_rev = Q ? Iop_Shr16x8 : Iop_Shr16x4;
                     op_shrn = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     op_shln = Q ? Iop_ShlN16x8 : Iop_ShlN16x4;
                     break;
                  case 2:
                     op = Q ? Iop_QShl32x4 : Iop_QShl32x2;
                     op_rev = Q ? Iop_Shr32x4 : Iop_Shr32x2;
                     op_shrn = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     op_shln = Q ? Iop_ShlN32x4 : Iop_ShlN32x2;
                     break;
                  case 3:
                     op = Q ? Iop_QShl64x2 : Iop_QShl64x1;
                     op_rev = Q ? Iop_Shr64x2 : Iop_Shr64;
                     op_shrn = Q ? Iop_ShrN64x2 : Iop_Shr64;
                     op_shln = Q ? Iop_ShlN64x2 : Iop_Shl64;
                     break;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op = Q ? Iop_QSal8x16 : Iop_QSal8x8;
                     op_rev = Q ? Iop_Sar8x16 : Iop_Sar8x8;
                     op_shrn = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     op_shln = Q ? Iop_ShlN8x16 : Iop_ShlN8x8;
                     break;
                  case 1:
                     op = Q ? Iop_QSal16x8 : Iop_QSal16x4;
                     op_rev = Q ? Iop_Sar16x8 : Iop_Sar16x4;
                     op_shrn = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     op_shln = Q ? Iop_ShlN16x8 : Iop_ShlN16x4;
                     break;
                  case 2:
                     op = Q ? Iop_QSal32x4 : Iop_QSal32x2;
                     op_rev = Q ? Iop_Sar32x4 : Iop_Sar32x2;
                     op_shrn = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     op_shln = Q ? Iop_ShlN32x4 : Iop_ShlN32x2;
                     break;
                  case 3:
                     op = Q ? Iop_QSal64x2 : Iop_QSal64x1;
                     op_rev = Q ? Iop_Sar64x2 : Iop_Sar64;
                     op_shrn = Q ? Iop_ShrN64x2 : Iop_Shr64;
                     op_shln = Q ? Iop_ShlN64x2 : Iop_Shl64;
                     break;
                  default:
                     vassert(0);
               }
            }
            if (Q) {
               tmp = newTemp(Ity_V128);
               shval = newTemp(Ity_V128);
               mask = newTemp(Ity_V128);
            } else {
               tmp = newTemp(Ity_I64);
               shval = newTemp(Ity_I64);
               mask = newTemp(Ity_I64);
            }
            assign(res, binop(op, mkexpr(arg_m), mkexpr(arg_n)));
            /* Only least significant byte from second argument is used.
               Copy this byte to the whole vector element. */
            assign(shval, binop(op_shrn,
                                binop(op_shln,
                                       mkexpr(arg_n),
                                       mkU8((8 << size) - 8)),
                                mkU8((8 << size) - 8)));
            for(i = 0; i < size; i++) {
               old_shval = shval;
               shval = newTemp(Q ? Ity_V128 : Ity_I64);
               assign(shval, binop(Q ? Iop_OrV128 : Iop_Or64,
                                   mkexpr(old_shval),
                                   binop(op_shln,
                                         mkexpr(old_shval),
                                         mkU8(8 << i))));
            }
            /* If shift is greater or equal to the element size and
               element is non-zero, then QC flag should be set. */
            esize = (8 << size) - 1;
            esize = (esize <<  8) | esize;
            esize = (esize << 16) | esize;
            esize = (esize << 32) | esize;
            setFlag_QC(binop(Q ? Iop_AndV128 : Iop_And64,
                             binop(cmp_gt, mkexpr(shval),
                                           Q ? mkU128(esize) : mkU64(esize)),
                             unop(cmp_neq, mkexpr(arg_m))),
                       Q ? mkU128(0) : mkU64(0),
                       Q, condT);
            /* Othervise QC flag should be set if shift value is positive and
               result beign rightshifted the same value is not equal to left
               argument. */
            assign(mask, binop(cmp_gt, mkexpr(shval),
                                       Q ? mkU128(0) : mkU64(0)));
            if (!Q && size == 3)
               assign(tmp, binop(op_rev, mkexpr(res),
                                         unop(Iop_64to8, mkexpr(arg_n))));
            else
               assign(tmp, binop(op_rev, mkexpr(res), mkexpr(arg_n)));
            setFlag_QC(binop(Q ? Iop_AndV128 : Iop_And64,
                             mkexpr(tmp), mkexpr(mask)),
                       binop(Q ? Iop_AndV128 : Iop_And64,
                             mkexpr(arg_m), mkexpr(mask)),
                       Q, condT);
            DIP("vqshl.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, Q ? 'q' : 'd',
                nreg);
         }
         break;
      case 5:
         if (B == 0) {
            /* VRSHL */
            IROp op, op_shrn, op_shln, cmp_gt, op_add;
            IRTemp shval, old_shval, imm_val, round;
            UInt i;
            ULong imm;
            cmp_gt = Q ? Iop_CmpGT8Sx16 : Iop_CmpGT8Sx8;
            imm = 1L;
            switch (size) {
               case 0: imm = (imm <<  8) | imm; /* fall through */
               case 1: imm = (imm << 16) | imm; /* fall through */
               case 2: imm = (imm << 32) | imm; /* fall through */
               case 3: break;
               default: vassert(0);
            }
            imm_val = newTemp(Q ? Ity_V128 : Ity_I64);
            round = newTemp(Q ? Ity_V128 : Ity_I64);
            assign(imm_val, Q ? mkU128(imm) : mkU64(imm));
            if (U) {
               switch (size) {
                  case 0:
                     op = Q ? Iop_Shl8x16 : Iop_Shl8x8;
                     op_add = Q ? Iop_Add8x16 : Iop_Add8x8;
                     op_shrn = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     op_shln = Q ? Iop_ShlN8x16 : Iop_ShlN8x8;
                     break;
                  case 1:
                     op = Q ? Iop_Shl16x8 : Iop_Shl16x4;
                     op_add = Q ? Iop_Add16x8 : Iop_Add16x4;
                     op_shrn = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     op_shln = Q ? Iop_ShlN16x8 : Iop_ShlN16x4;
                     break;
                  case 2:
                     op = Q ? Iop_Shl32x4 : Iop_Shl32x2;
                     op_add = Q ? Iop_Add32x4 : Iop_Add32x2;
                     op_shrn = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     op_shln = Q ? Iop_ShlN32x4 : Iop_ShlN32x2;
                     break;
                  case 3:
                     op = Q ? Iop_Shl64x2 : Iop_Shl64;
                     op_add = Q ? Iop_Add64x2 : Iop_Add64;
                     op_shrn = Q ? Iop_ShrN64x2 : Iop_Shr64;
                     op_shln = Q ? Iop_ShlN64x2 : Iop_Shl64;
                     break;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op = Q ? Iop_Sal8x16 : Iop_Sal8x8;
                     op_add = Q ? Iop_Add8x16 : Iop_Add8x8;
                     op_shrn = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     op_shln = Q ? Iop_ShlN8x16 : Iop_ShlN8x8;
                     break;
                  case 1:
                     op = Q ? Iop_Sal16x8 : Iop_Sal16x4;
                     op_add = Q ? Iop_Add16x8 : Iop_Add16x4;
                     op_shrn = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     op_shln = Q ? Iop_ShlN16x8 : Iop_ShlN16x4;
                     break;
                  case 2:
                     op = Q ? Iop_Sal32x4 : Iop_Sal32x2;
                     op_add = Q ? Iop_Add32x4 : Iop_Add32x2;
                     op_shrn = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     op_shln = Q ? Iop_ShlN32x4 : Iop_ShlN32x2;
                     break;
                  case 3:
                     op = Q ? Iop_Sal64x2 : Iop_Sal64x1;
                     op_add = Q ? Iop_Add64x2 : Iop_Add64;
                     op_shrn = Q ? Iop_ShrN64x2 : Iop_Shr64;
                     op_shln = Q ? Iop_ShlN64x2 : Iop_Shl64;
                     break;
                  default:
                     vassert(0);
               }
            }
            if (Q) {
               shval = newTemp(Ity_V128);
            } else {
               shval = newTemp(Ity_I64);
            }
            /* Only least significant byte from second argument is used.
               Copy this byte to the whole vector element. */
            assign(shval, binop(op_shrn,
                                binop(op_shln,
                                       mkexpr(arg_n),
                                       mkU8((8 << size) - 8)),
                                mkU8((8 << size) - 8)));
            for (i = 0; i < size; i++) {
               old_shval = shval;
               shval = newTemp(Q ? Ity_V128 : Ity_I64);
               assign(shval, binop(Q ? Iop_OrV128 : Iop_Or64,
                                   mkexpr(old_shval),
                                   binop(op_shln,
                                         mkexpr(old_shval),
                                         mkU8(8 << i))));
            }
            /* Compute the result */
            if (!Q && size == 3 && U) {
               assign(round, binop(Q ? Iop_AndV128 : Iop_And64,
                                   binop(op,
                                         mkexpr(arg_m),
                                         unop(Iop_64to8,
                                              binop(op_add,
                                                    mkexpr(arg_n),
                                                    mkexpr(imm_val)))),
                                   binop(Q ? Iop_AndV128 : Iop_And64,
                                         mkexpr(imm_val),
                                         binop(cmp_gt,
                                               Q ? mkU128(0) : mkU64(0),
                                               mkexpr(arg_n)))));
               assign(res, binop(op_add,
                                 binop(op,
                                       mkexpr(arg_m),
                                       unop(Iop_64to8, mkexpr(arg_n))),
                                 mkexpr(round)));
            } else {
               assign(round, binop(Q ? Iop_AndV128 : Iop_And64,
                                   binop(op,
                                         mkexpr(arg_m),
                                         binop(op_add,
                                               mkexpr(arg_n),
                                               mkexpr(imm_val))),
                                   binop(Q ? Iop_AndV128 : Iop_And64,
                                         mkexpr(imm_val),
                                         binop(cmp_gt,
                                               Q ? mkU128(0) : mkU64(0),
                                               mkexpr(arg_n)))));
               assign(res, binop(op_add,
                                 binop(op, mkexpr(arg_m), mkexpr(arg_n)),
                                 mkexpr(round)));
            }
            DIP("vrshl.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, Q ? 'q' : 'd',
                nreg);
         } else {
            /* VQRSHL */
            IROp op, op_rev, op_shrn, op_shln, cmp_neq, cmp_gt, op_add;
            IRTemp tmp, shval, mask, old_shval, imm_val, round;
            UInt i;
            ULong esize, imm;
            cmp_neq = Q ? Iop_CmpNEZ8x16 : Iop_CmpNEZ8x8;
            cmp_gt = Q ? Iop_CmpGT8Sx16 : Iop_CmpGT8Sx8;
            imm = 1L;
            switch (size) {
               case 0: imm = (imm <<  8) | imm; /* fall through */
               case 1: imm = (imm << 16) | imm; /* fall through */
               case 2: imm = (imm << 32) | imm; /* fall through */
               case 3: break;
               default: vassert(0);
            }
            imm_val = newTemp(Q ? Ity_V128 : Ity_I64);
            round = newTemp(Q ? Ity_V128 : Ity_I64);
            assign(imm_val, Q ? mkU128(imm) : mkU64(imm));
            if (U) {
               switch (size) {
                  case 0:
                     op = Q ? Iop_QShl8x16 : Iop_QShl8x8;
                     op_add = Q ? Iop_Add8x16 : Iop_Add8x8;
                     op_rev = Q ? Iop_Shr8x16 : Iop_Shr8x8;
                     op_shrn = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     op_shln = Q ? Iop_ShlN8x16 : Iop_ShlN8x8;
                     break;
                  case 1:
                     op = Q ? Iop_QShl16x8 : Iop_QShl16x4;
                     op_add = Q ? Iop_Add16x8 : Iop_Add16x4;
                     op_rev = Q ? Iop_Shr16x8 : Iop_Shr16x4;
                     op_shrn = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     op_shln = Q ? Iop_ShlN16x8 : Iop_ShlN16x4;
                     break;
                  case 2:
                     op = Q ? Iop_QShl32x4 : Iop_QShl32x2;
                     op_add = Q ? Iop_Add32x4 : Iop_Add32x2;
                     op_rev = Q ? Iop_Shr32x4 : Iop_Shr32x2;
                     op_shrn = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     op_shln = Q ? Iop_ShlN32x4 : Iop_ShlN32x2;
                     break;
                  case 3:
                     op = Q ? Iop_QShl64x2 : Iop_QShl64x1;
                     op_add = Q ? Iop_Add64x2 : Iop_Add64;
                     op_rev = Q ? Iop_Shr64x2 : Iop_Shr64;
                     op_shrn = Q ? Iop_ShrN64x2 : Iop_Shr64;
                     op_shln = Q ? Iop_ShlN64x2 : Iop_Shl64;
                     break;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op = Q ? Iop_QSal8x16 : Iop_QSal8x8;
                     op_add = Q ? Iop_Add8x16 : Iop_Add8x8;
                     op_rev = Q ? Iop_Sar8x16 : Iop_Sar8x8;
                     op_shrn = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     op_shln = Q ? Iop_ShlN8x16 : Iop_ShlN8x8;
                     break;
                  case 1:
                     op = Q ? Iop_QSal16x8 : Iop_QSal16x4;
                     op_add = Q ? Iop_Add16x8 : Iop_Add16x4;
                     op_rev = Q ? Iop_Sar16x8 : Iop_Sar16x4;
                     op_shrn = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     op_shln = Q ? Iop_ShlN16x8 : Iop_ShlN16x4;
                     break;
                  case 2:
                     op = Q ? Iop_QSal32x4 : Iop_QSal32x2;
                     op_add = Q ? Iop_Add32x4 : Iop_Add32x2;
                     op_rev = Q ? Iop_Sar32x4 : Iop_Sar32x2;
                     op_shrn = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     op_shln = Q ? Iop_ShlN32x4 : Iop_ShlN32x2;
                     break;
                  case 3:
                     op = Q ? Iop_QSal64x2 : Iop_QSal64x1;
                     op_add = Q ? Iop_Add64x2 : Iop_Add64;
                     op_rev = Q ? Iop_Sar64x2 : Iop_Sar64;
                     op_shrn = Q ? Iop_ShrN64x2 : Iop_Shr64;
                     op_shln = Q ? Iop_ShlN64x2 : Iop_Shl64;
                     break;
                  default:
                     vassert(0);
               }
            }
            if (Q) {
               tmp = newTemp(Ity_V128);
               shval = newTemp(Ity_V128);
               mask = newTemp(Ity_V128);
            } else {
               tmp = newTemp(Ity_I64);
               shval = newTemp(Ity_I64);
               mask = newTemp(Ity_I64);
            }
            /* Only least significant byte from second argument is used.
               Copy this byte to the whole vector element. */
            assign(shval, binop(op_shrn,
                                binop(op_shln,
                                       mkexpr(arg_n),
                                       mkU8((8 << size) - 8)),
                                mkU8((8 << size) - 8)));
            for (i = 0; i < size; i++) {
               old_shval = shval;
               shval = newTemp(Q ? Ity_V128 : Ity_I64);
               assign(shval, binop(Q ? Iop_OrV128 : Iop_Or64,
                                   mkexpr(old_shval),
                                   binop(op_shln,
                                         mkexpr(old_shval),
                                         mkU8(8 << i))));
            }
            /* Compute the result */
            assign(round, binop(Q ? Iop_AndV128 : Iop_And64,
                                binop(op,
                                      mkexpr(arg_m),
                                      binop(op_add,
                                            mkexpr(arg_n),
                                            mkexpr(imm_val))),
                                binop(Q ? Iop_AndV128 : Iop_And64,
                                      mkexpr(imm_val),
                                      binop(cmp_gt,
                                            Q ? mkU128(0) : mkU64(0),
                                            mkexpr(arg_n)))));
            assign(res, binop(op_add,
                              binop(op, mkexpr(arg_m), mkexpr(arg_n)),
                              mkexpr(round)));
            /* If shift is greater or equal to the element size and element is
               non-zero, then QC flag should be set. */
            esize = (8 << size) - 1;
            esize = (esize <<  8) | esize;
            esize = (esize << 16) | esize;
            esize = (esize << 32) | esize;
            setFlag_QC(binop(Q ? Iop_AndV128 : Iop_And64,
                             binop(cmp_gt, mkexpr(shval),
                                           Q ? mkU128(esize) : mkU64(esize)),
                             unop(cmp_neq, mkexpr(arg_m))),
                       Q ? mkU128(0) : mkU64(0),
                       Q, condT);
            /* Othervise QC flag should be set if shift value is positive and
               result beign rightshifted the same value is not equal to left
               argument. */
            assign(mask, binop(cmp_gt, mkexpr(shval),
                               Q ? mkU128(0) : mkU64(0)));
            if (!Q && size == 3)
               assign(tmp, binop(op_rev, mkexpr(res),
                                         unop(Iop_64to8, mkexpr(arg_n))));
            else
               assign(tmp, binop(op_rev, mkexpr(res), mkexpr(arg_n)));
            setFlag_QC(binop(Q ? Iop_AndV128 : Iop_And64,
                             mkexpr(tmp), mkexpr(mask)),
                       binop(Q ? Iop_AndV128 : Iop_And64,
                             mkexpr(arg_m), mkexpr(mask)),
                       Q, condT);
            DIP("vqrshl.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, Q ? 'q' : 'd',
                nreg);
         }
         break;
      case 6:
         /* VMAX, VMIN  */
         if (B == 0) {
            /* VMAX */
            IROp op;
            if (U == 0) {
               switch (size) {
                  case 0: op = Q ? Iop_Max8Sx16 : Iop_Max8Sx8; break;
                  case 1: op = Q ? Iop_Max16Sx8 : Iop_Max16Sx4; break;
                  case 2: op = Q ? Iop_Max32Sx4 : Iop_Max32Sx2; break;
                  case 3: return False;
                  default: vassert(0);
               }
            } else {
               switch (size) {
                  case 0: op = Q ? Iop_Max8Ux16 : Iop_Max8Ux8; break;
                  case 1: op = Q ? Iop_Max16Ux8 : Iop_Max16Ux4; break;
                  case 2: op = Q ? Iop_Max32Ux4 : Iop_Max32Ux2; break;
                  case 3: return False;
                  default: vassert(0);
               }
            }
            assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
            DIP("vmax.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                mreg);
         } else {
            /* VMIN */
            IROp op;
            if (U == 0) {
               switch (size) {
                  case 0: op = Q ? Iop_Min8Sx16 : Iop_Min8Sx8; break;
                  case 1: op = Q ? Iop_Min16Sx8 : Iop_Min16Sx4; break;
                  case 2: op = Q ? Iop_Min32Sx4 : Iop_Min32Sx2; break;
                  case 3: return False;
                  default: vassert(0);
               }
            } else {
               switch (size) {
                  case 0: op = Q ? Iop_Min8Ux16 : Iop_Min8Ux8; break;
                  case 1: op = Q ? Iop_Min16Ux8 : Iop_Min16Ux4; break;
                  case 2: op = Q ? Iop_Min32Ux4 : Iop_Min32Ux2; break;
                  case 3: return False;
                  default: vassert(0);
               }
            }
            assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
            DIP("vmin.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                mreg);
         }
         break;
      case 7:
         if (B == 0) {
            /* VABD */
            IROp op_cmp, op_sub;
            IRTemp cond;
            if ((theInstr >> 23) & 1) {
               vpanic("VABDL should not be in dis_neon_data_3same\n");
            }
            if (Q) {
               switch (size) {
                  case 0:
                     op_cmp = U ? Iop_CmpGT8Ux16 : Iop_CmpGT8Sx16;
                     op_sub = Iop_Sub8x16;
                     break;
                  case 1:
                     op_cmp = U ? Iop_CmpGT16Ux8 : Iop_CmpGT16Sx8;
                     op_sub = Iop_Sub16x8;
                     break;
                  case 2:
                     op_cmp = U ? Iop_CmpGT32Ux4 : Iop_CmpGT32Sx4;
                     op_sub = Iop_Sub32x4;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op_cmp = U ? Iop_CmpGT8Ux8 : Iop_CmpGT8Sx8;
                     op_sub = Iop_Sub8x8;
                     break;
                  case 1:
                     op_cmp = U ? Iop_CmpGT16Ux4 : Iop_CmpGT16Sx4;
                     op_sub = Iop_Sub16x4;
                     break;
                  case 2:
                     op_cmp = U ? Iop_CmpGT32Ux2 : Iop_CmpGT32Sx2;
                     op_sub = Iop_Sub32x2;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            }
            if (Q) {
               cond = newTemp(Ity_V128);
            } else {
               cond = newTemp(Ity_I64);
            }
            assign(cond, binop(op_cmp, mkexpr(arg_n), mkexpr(arg_m)));
            assign(res, binop(Q ? Iop_OrV128 : Iop_Or64,
                              binop(Q ? Iop_AndV128 : Iop_And64,
                                    binop(op_sub, mkexpr(arg_n),
                                                  mkexpr(arg_m)),
                                    mkexpr(cond)),
                              binop(Q ? Iop_AndV128 : Iop_And64,
                                    binop(op_sub, mkexpr(arg_m),
                                                  mkexpr(arg_n)),
                                    unop(Q ? Iop_NotV128 : Iop_Not64,
                                         mkexpr(cond)))));
            DIP("vabd.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                mreg);
         } else {
            /* VABA */
            IROp op_cmp, op_sub, op_add;
            IRTemp cond, acc, tmp;
            if ((theInstr >> 23) & 1) {
               vpanic("VABAL should not be in dis_neon_data_3same");
            }
            if (Q) {
               switch (size) {
                  case 0:
                     op_cmp = U ? Iop_CmpGT8Ux16 : Iop_CmpGT8Sx16;
                     op_sub = Iop_Sub8x16;
                     op_add = Iop_Add8x16;
                     break;
                  case 1:
                     op_cmp = U ? Iop_CmpGT16Ux8 : Iop_CmpGT16Sx8;
                     op_sub = Iop_Sub16x8;
                     op_add = Iop_Add16x8;
                     break;
                  case 2:
                     op_cmp = U ? Iop_CmpGT32Ux4 : Iop_CmpGT32Sx4;
                     op_sub = Iop_Sub32x4;
                     op_add = Iop_Add32x4;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op_cmp = U ? Iop_CmpGT8Ux8 : Iop_CmpGT8Sx8;
                     op_sub = Iop_Sub8x8;
                     op_add = Iop_Add8x8;
                     break;
                  case 1:
                     op_cmp = U ? Iop_CmpGT16Ux4 : Iop_CmpGT16Sx4;
                     op_sub = Iop_Sub16x4;
                     op_add = Iop_Add16x4;
                     break;
                  case 2:
                     op_cmp = U ? Iop_CmpGT32Ux2 : Iop_CmpGT32Sx2;
                     op_sub = Iop_Sub32x2;
                     op_add = Iop_Add32x2;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            }
            if (Q) {
               cond = newTemp(Ity_V128);
               acc = newTemp(Ity_V128);
               tmp = newTemp(Ity_V128);
               assign(acc, getQReg(dreg));
            } else {
               cond = newTemp(Ity_I64);
               acc = newTemp(Ity_I64);
               tmp = newTemp(Ity_I64);
               assign(acc, getDRegI64(dreg));
            }
            assign(cond, binop(op_cmp, mkexpr(arg_n), mkexpr(arg_m)));
            assign(tmp, binop(Q ? Iop_OrV128 : Iop_Or64,
                              binop(Q ? Iop_AndV128 : Iop_And64,
                                    binop(op_sub, mkexpr(arg_n),
                                                  mkexpr(arg_m)),
                                    mkexpr(cond)),
                              binop(Q ? Iop_AndV128 : Iop_And64,
                                    binop(op_sub, mkexpr(arg_m),
                                                  mkexpr(arg_n)),
                                    unop(Q ? Iop_NotV128 : Iop_Not64,
                                         mkexpr(cond)))));
            assign(res, binop(op_add, mkexpr(acc), mkexpr(tmp)));
            DIP("vaba.%c%d %c%u, %c%u, %c%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                mreg);
         }
         break;
      case 8:
         if (B == 0) {
            IROp op;
            if (U == 0) {
               /* VADD  */
               switch (size) {
                  case 0: op = Q ? Iop_Add8x16 : Iop_Add8x8; break;
                  case 1: op = Q ? Iop_Add16x8 : Iop_Add16x4; break;
                  case 2: op = Q ? Iop_Add32x4 : Iop_Add32x2; break;
                  case 3: op = Q ? Iop_Add64x2 : Iop_Add64; break;
                  default: vassert(0);
               }
               DIP("vadd.i%d %c%u, %c%u, %c%u\n",
                   8 << size, Q ? 'q' : 'd',
                   dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            } else {
               /* VSUB  */
               switch (size) {
                  case 0: op = Q ? Iop_Sub8x16 : Iop_Sub8x8; break;
                  case 1: op = Q ? Iop_Sub16x8 : Iop_Sub16x4; break;
                  case 2: op = Q ? Iop_Sub32x4 : Iop_Sub32x2; break;
                  case 3: op = Q ? Iop_Sub64x2 : Iop_Sub64; break;
                  default: vassert(0);
               }
               DIP("vsub.i%d %c%u, %c%u, %c%u\n",
                   8 << size, Q ? 'q' : 'd',
                   dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            }
            assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
         } else {
            IROp op;
            switch (size) {
               case 0: op = Q ? Iop_CmpNEZ8x16 : Iop_CmpNEZ8x8; break;
               case 1: op = Q ? Iop_CmpNEZ16x8 : Iop_CmpNEZ16x4; break;
               case 2: op = Q ? Iop_CmpNEZ32x4 : Iop_CmpNEZ32x2; break;
               case 3: op = Q ? Iop_CmpNEZ64x2 : Iop_CmpwNEZ64; break;
               default: vassert(0);
            }
            if (U == 0) {
               /* VTST  */
               assign(res, unop(op, binop(Q ? Iop_AndV128 : Iop_And64,
                                          mkexpr(arg_n),
                                          mkexpr(arg_m))));
               DIP("vtst.%d %c%u, %c%u, %c%u\n",
                   8 << size, Q ? 'q' : 'd',
                   dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            } else {
               /* VCEQ  */
               assign(res, unop(Q ? Iop_NotV128 : Iop_Not64,
                                unop(op,
                                     binop(Q ? Iop_XorV128 : Iop_Xor64,
                                           mkexpr(arg_n),
                                           mkexpr(arg_m)))));
               DIP("vceq.i%d %c%u, %c%u, %c%u\n",
                   8 << size, Q ? 'q' : 'd',
                   dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            }
         }
         break;
      case 9:
         if (B == 0) {
            /* VMLA, VMLS (integer) */
            IROp op, op2;
            UInt P = (theInstr >> 24) & 1;
            if (P) {
               switch (size) {
                  case 0:
                     op = Q ? Iop_Mul8x16 : Iop_Mul8x8;
                     op2 = Q ? Iop_Sub8x16 : Iop_Sub8x8;
                     break;
                  case 1:
                     op = Q ? Iop_Mul16x8 : Iop_Mul16x4;
                     op2 = Q ? Iop_Sub16x8 : Iop_Sub16x4;
                     break;
                  case 2:
                     op = Q ? Iop_Mul32x4 : Iop_Mul32x2;
                     op2 = Q ? Iop_Sub32x4 : Iop_Sub32x2;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op = Q ? Iop_Mul8x16 : Iop_Mul8x8;
                     op2 = Q ? Iop_Add8x16 : Iop_Add8x8;
                     break;
                  case 1:
                     op = Q ? Iop_Mul16x8 : Iop_Mul16x4;
                     op2 = Q ? Iop_Add16x8 : Iop_Add16x4;
                     break;
                  case 2:
                     op = Q ? Iop_Mul32x4 : Iop_Mul32x2;
                     op2 = Q ? Iop_Add32x4 : Iop_Add32x2;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            }
            assign(res, binop(op2,
                              Q ? getQReg(dreg) : getDRegI64(dreg),
                              binop(op, mkexpr(arg_n), mkexpr(arg_m))));
            DIP("vml%c.i%d %c%u, %c%u, %c%u\n",
                P ? 's' : 'a', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                mreg);
         } else {
            /* VMUL */
            IROp op;
            UInt P = (theInstr >> 24) & 1;
            if (P) {
               switch (size) {
                  case 0:
                     op = Q ? Iop_PolynomialMul8x16 : Iop_PolynomialMul8x8;
                     break;
                  case 1: case 2: case 3: return False;
                  default: vassert(0);
               }
            } else {
               switch (size) {
                  case 0: op = Q ? Iop_Mul8x16 : Iop_Mul8x8; break;
                  case 1: op = Q ? Iop_Mul16x8 : Iop_Mul16x4; break;
                  case 2: op = Q ? Iop_Mul32x4 : Iop_Mul32x2; break;
                  case 3: return False;
                  default: vassert(0);
               }
            }
            assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
            DIP("vmul.%c%d %c%u, %c%u, %c%u\n",
                P ? 'p' : 'i', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd',
                mreg);
         }
         break;
      case 10: {
         /* VPMAX, VPMIN  */
         UInt P = (theInstr >> 4) & 1;
         IROp op;
         if (Q)
            return False;
         if (P) {
            switch (size) {
               case 0: op = U ? Iop_PwMin8Ux8  : Iop_PwMin8Sx8; break;
               case 1: op = U ? Iop_PwMin16Ux4 : Iop_PwMin16Sx4; break;
               case 2: op = U ? Iop_PwMin32Ux2 : Iop_PwMin32Sx2; break;
               case 3: return False;
               default: vassert(0);
            }
         } else {
            switch (size) {
               case 0: op = U ? Iop_PwMax8Ux8  : Iop_PwMax8Sx8; break;
               case 1: op = U ? Iop_PwMax16Ux4 : Iop_PwMax16Sx4; break;
               case 2: op = U ? Iop_PwMax32Ux2 : Iop_PwMax32Sx2; break;
               case 3: return False;
               default: vassert(0);
            }
         }
         assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
         DIP("vp%s.%c%d %c%u, %c%u, %c%u\n",
             P ? "min" : "max", U ? 'u' : 's',
             8 << size, Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg,
             Q ? 'q' : 'd', mreg);
         break;
      }
      case 11:
         if (B == 0) {
            if (U == 0) {
               /* VQDMULH  */
               IROp op ,op2;
               ULong imm;
               switch (size) {
                  case 0: case 3:
                     return False;
                  case 1:
                     op = Q ? Iop_QDMulHi16Sx8 : Iop_QDMulHi16Sx4;
                     op2 = Q ? Iop_CmpEQ16x8 : Iop_CmpEQ16x4;
                     imm = 1LL << 15;
                     imm = (imm << 16) | imm;
                     imm = (imm << 32) | imm;
                     break;
                  case 2:
                     op = Q ? Iop_QDMulHi32Sx4 : Iop_QDMulHi32Sx2;
                     op2 = Q ? Iop_CmpEQ32x4 : Iop_CmpEQ32x2;
                     imm = 1LL << 31;
                     imm = (imm << 32) | imm;
                     break;
                  default:
                     vassert(0);
               }
               assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
               setFlag_QC(binop(Q ? Iop_AndV128 : Iop_And64,
                                binop(op2, mkexpr(arg_n),
                                           Q ? mkU128(imm) : mkU64(imm)),
                                binop(op2, mkexpr(arg_m),
                                           Q ? mkU128(imm) : mkU64(imm))),
                          Q ? mkU128(0) : mkU64(0),
                          Q, condT);
               DIP("vqdmulh.s%d %c%u, %c%u, %c%u\n",
                   8 << size, Q ? 'q' : 'd',
                   dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            } else {
               /* VQRDMULH */
               IROp op ,op2;
               ULong imm;
               switch(size) {
                  case 0: case 3:
                     return False;
                  case 1:
                     imm = 1LL << 15;
                     imm = (imm << 16) | imm;
                     imm = (imm << 32) | imm;
                     op = Q ? Iop_QRDMulHi16Sx8 : Iop_QRDMulHi16Sx4;
                     op2 = Q ? Iop_CmpEQ16x8 : Iop_CmpEQ16x4;
                     break;
                  case 2:
                     imm = 1LL << 31;
                     imm = (imm << 32) | imm;
                     op = Q ? Iop_QRDMulHi32Sx4 : Iop_QRDMulHi32Sx2;
                     op2 = Q ? Iop_CmpEQ32x4 : Iop_CmpEQ32x2;
                     break;
                  default:
                     vassert(0);
               }
               assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
               setFlag_QC(binop(Q ? Iop_AndV128 : Iop_And64,
                                binop(op2, mkexpr(arg_n),
                                           Q ? mkU128(imm) : mkU64(imm)),
                                binop(op2, mkexpr(arg_m),
                                           Q ? mkU128(imm) : mkU64(imm))),
                          Q ? mkU128(0) : mkU64(0),
                          Q, condT);
               DIP("vqrdmulh.s%d %c%u, %c%u, %c%u\n",
                   8 << size, Q ? 'q' : 'd',
                   dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            }
         } else {
            if (U == 0) {
               /* VPADD */
               IROp op;
               if (Q)
                  return False;
               switch (size) {
                  case 0: op = Q ? Iop_PwAdd8x16 : Iop_PwAdd8x8;  break;
                  case 1: op = Q ? Iop_PwAdd16x8 : Iop_PwAdd16x4; break;
                  case 2: op = Q ? Iop_PwAdd32x4 : Iop_PwAdd32x2; break;
                  case 3: return False;
                  default: vassert(0);
               }
               assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
               DIP("vpadd.i%d %c%u, %c%u, %c%u\n",
                   8 << size, Q ? 'q' : 'd',
                   dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            }
         }
         break;
      case 12: {
         return False;
      }
      /* Starting from here these are FP SIMD cases */
      case 13:
         if (B == 0) {
            IROp op;
            if (U == 0) {
               if ((C >> 1) == 0) {
                  /* VADD  */
                  op = Q ? Iop_Add32Fx4 : Iop_Add32Fx2 ;
                  DIP("vadd.f32 %c%u, %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               } else {
                  /* VSUB  */
                  op = Q ? Iop_Sub32Fx4 : Iop_Sub32Fx2 ;
                  DIP("vsub.f32 %c%u, %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               }
            } else {
               if ((C >> 1) == 0) {
                  /* VPADD */
                  if (Q)
                     return False;
                  op = Iop_PwAdd32Fx2;
                  DIP("vpadd.f32 d%u, d%u, d%u\n", dreg, nreg, mreg);
               } else {
                  /* VABD  */
                  if (Q) {
                     assign(res, unop(Iop_Abs32Fx4,
                                      triop(Iop_Sub32Fx4,
                                            get_FAKE_roundingmode(),
                                            mkexpr(arg_n),
                                            mkexpr(arg_m))));
                  } else {
                     assign(res, unop(Iop_Abs32Fx2,
                                      binop(Iop_Sub32Fx2,
                                            mkexpr(arg_n),
                                            mkexpr(arg_m))));
                  }
                  DIP("vabd.f32 %c%u, %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
                  break;
               }
            }
            assign(res, binop_w_fake_RM(op, mkexpr(arg_n), mkexpr(arg_m)));
         } else {
            if (U == 0) {
               /* VMLA, VMLS  */
               IROp op, op2;
               UInt P = (theInstr >> 21) & 1;
               if (P) {
                  switch (size & 1) {
                     case 0:
                        op = Q ? Iop_Mul32Fx4 : Iop_Mul32Fx2;
                        op2 = Q ? Iop_Sub32Fx4 : Iop_Sub32Fx2;
                        break;
                     case 1: return False;
                     default: vassert(0);
                  }
               } else {
                  switch (size & 1) {
                     case 0:
                        op = Q ? Iop_Mul32Fx4 : Iop_Mul32Fx2;
                        op2 = Q ? Iop_Add32Fx4 : Iop_Add32Fx2;
                        break;
                     case 1: return False;
                     default: vassert(0);
                  }
               }
               assign(res, binop_w_fake_RM(
                              op2,
                              Q ? getQReg(dreg) : getDRegI64(dreg),
                              binop_w_fake_RM(op, mkexpr(arg_n),
                                                  mkexpr(arg_m))));

               DIP("vml%c.f32 %c%u, %c%u, %c%u\n",
                   P ? 's' : 'a', Q ? 'q' : 'd',
                   dreg, Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            } else {
               /* VMUL  */
               IROp op;
               if ((C >> 1) != 0)
                  return False;
               op = Q ? Iop_Mul32Fx4 : Iop_Mul32Fx2 ;
               assign(res, binop_w_fake_RM(op, mkexpr(arg_n), mkexpr(arg_m)));
               DIP("vmul.f32 %c%u, %c%u, %c%u\n",
                   Q ? 'q' : 'd', dreg,
                   Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
            }
         }
         break;
      case 14:
         if (B == 0) {
            if (U == 0) {
               if ((C >> 1) == 0) {
                  /* VCEQ  */
                  IROp op;
                  if ((theInstr >> 20) & 1)
                     return False;
                  op = Q ? Iop_CmpEQ32Fx4 : Iop_CmpEQ32Fx2;
                  assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
                  DIP("vceq.f32 %c%u, %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               } else {
                  return False;
               }
            } else {
               if ((C >> 1) == 0) {
                  /* VCGE  */
                  IROp op;
                  if ((theInstr >> 20) & 1)
                     return False;
                  op = Q ? Iop_CmpGE32Fx4 : Iop_CmpGE32Fx2;
                  assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
                  DIP("vcge.f32 %c%u, %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               } else {
                  /* VCGT  */
                  IROp op;
                  if ((theInstr >> 20) & 1)
                     return False;
                  op = Q ? Iop_CmpGT32Fx4 : Iop_CmpGT32Fx2;
                  assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
                  DIP("vcgt.f32 %c%u, %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               }
            }
         } else {
            if (U == 1) {
               /* VACGE, VACGT */
               UInt op_bit = (theInstr >> 21) & 1;
               IROp op, op2;
               op2 = Q ? Iop_Abs32Fx4 : Iop_Abs32Fx2;
               if (op_bit) {
                  op = Q ? Iop_CmpGT32Fx4 : Iop_CmpGT32Fx2;
                  assign(res, binop(op,
                                    unop(op2, mkexpr(arg_n)),
                                    unop(op2, mkexpr(arg_m))));
               } else {
                  op = Q ? Iop_CmpGE32Fx4 : Iop_CmpGE32Fx2;
                  assign(res, binop(op,
                                    unop(op2, mkexpr(arg_n)),
                                    unop(op2, mkexpr(arg_m))));
               }
               DIP("vacg%c.f32 %c%u, %c%u, %c%u\n", op_bit ? 't' : 'e',
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg,
                   Q ? 'q' : 'd', mreg);
            }
         }
         break;
      case 15:
         if (B == 0) {
            if (U == 0) {
               /* VMAX, VMIN  */
               IROp op;
               if ((theInstr >> 20) & 1)
                  return False;
               if ((theInstr >> 21) & 1) {
                  op = Q ? Iop_Min32Fx4 : Iop_Min32Fx2;
                  DIP("vmin.f32 %c%u, %c%u, %c%u\n", Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               } else {
                  op = Q ? Iop_Max32Fx4 : Iop_Max32Fx2;
                  DIP("vmax.f32 %c%u, %c%u, %c%u\n", Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               }
               assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
            } else {
               /* VPMAX, VPMIN   */
               IROp op;
               if (Q)
                  return False;
               if ((theInstr >> 20) & 1)
                  return False;
               if ((theInstr >> 21) & 1) {
                  op = Iop_PwMin32Fx2;
                  DIP("vpmin.f32 d%u, d%u, d%u\n", dreg, nreg, mreg);
               } else {
                  op = Iop_PwMax32Fx2;
                  DIP("vpmax.f32 d%u, d%u, d%u\n", dreg, nreg, mreg);
               }
               assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
            }
         } else {
            if (U == 0) {
               if ((C >> 1) == 0) {
                  /* VRECPS */
                  if ((theInstr >> 20) & 1)
                     return False;
                  assign(res, binop(Q ? Iop_RecipStep32Fx4
                                      : Iop_RecipStep32Fx2,
                                    mkexpr(arg_n),
                                    mkexpr(arg_m)));
                  DIP("vrecps.f32 %c%u, %c%u, %c%u\n", Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               } else {
                  /* VRSQRTS  */
                  if ((theInstr >> 20) & 1)
                     return False;
                  assign(res, binop(Q ? Iop_RSqrtStep32Fx4
                                      : Iop_RSqrtStep32Fx2,
                                    mkexpr(arg_n),
                                    mkexpr(arg_m)));
                  DIP("vrsqrts.f32 %c%u, %c%u, %c%u\n", Q ? 'q' : 'd', dreg,
                      Q ? 'q' : 'd', nreg, Q ? 'q' : 'd', mreg);
               }
            }
         }
         break;
      default:
         /*NOTREACHED*/
         vassert(0);
   }

   if (Q) {
      putQReg(dreg, mkexpr(res), condT);
   } else {
      putDRegI64(dreg, mkexpr(res), condT);
   }

   return True;
}

/* A7.4.2 Three registers of different length */
static
Bool dis_neon_data_3diff ( UInt theInstr, IRTemp condT )
{
   /* In paths where this returns False, indicating a non-decodable
      instruction, there may still be some IR assignments to temporaries
      generated.  This is inconvenient but harmless, and the post-front-end
      IR optimisation pass will just remove them anyway.  So there's no
      effort made here to tidy it up.
   */
   UInt A = (theInstr >> 8) & 0xf;
   UInt B = (theInstr >> 20) & 3;
   UInt U = (theInstr >> 24) & 1;
   UInt P = (theInstr >> 9) & 1;
   UInt mreg = get_neon_m_regno(theInstr);
   UInt nreg = get_neon_n_regno(theInstr);
   UInt dreg = get_neon_d_regno(theInstr);
   UInt size = B;
   ULong imm;
   IRTemp res, arg_m, arg_n, cond, tmp;
   IROp cvt, cvt2, cmp, op, op2, sh, add;
   switch (A) {
      case 0: case 1: case 2: case 3:
         /* VADDL, VADDW, VSUBL, VSUBW */
         if (dreg & 1)
            return False;
         dreg >>= 1;
         size = B;
         switch (size) {
            case 0:
               cvt = U ? Iop_Widen8Uto16x8 : Iop_Widen8Sto16x8;
               op = (A & 2) ? Iop_Sub16x8 : Iop_Add16x8;
               break;
            case 1:
               cvt = U ? Iop_Widen16Uto32x4 : Iop_Widen16Sto32x4;
               op = (A & 2) ? Iop_Sub32x4 : Iop_Add32x4;
               break;
            case 2:
               cvt = U ? Iop_Widen32Uto64x2 : Iop_Widen32Sto64x2;
               op = (A & 2) ? Iop_Sub64x2 : Iop_Add64x2;
               break;
            case 3:
               return False;
            default:
               vassert(0);
         }
         arg_n = newTemp(Ity_V128);
         arg_m = newTemp(Ity_V128);
         if (A & 1) {
            if (nreg & 1)
               return False;
            nreg >>= 1;
            assign(arg_n, getQReg(nreg));
         } else {
            assign(arg_n, unop(cvt, getDRegI64(nreg)));
         }
         assign(arg_m, unop(cvt, getDRegI64(mreg)));
         putQReg(dreg, binop(op, mkexpr(arg_n), mkexpr(arg_m)),
                       condT);
         DIP("v%s%c.%c%d q%u, %c%u, d%u\n", (A & 2) ? "sub" : "add",
             (A & 1) ? 'w' : 'l', U ? 'u' : 's', 8 << size, dreg,
             (A & 1) ? 'q' : 'd', nreg, mreg);
         return True;
      case 4:
         /* VADDHN, VRADDHN */
         if (mreg & 1)
            return False;
         mreg >>= 1;
         if (nreg & 1)
            return False;
         nreg >>= 1;
         size = B;
         switch (size) {
            case 0:
               op = Iop_Add16x8;
               cvt = Iop_NarrowUn16to8x8;
               sh = Iop_ShrN16x8;
               imm = 1U << 7;
               imm = (imm << 16) | imm;
               imm = (imm << 32) | imm;
               break;
            case 1:
               op = Iop_Add32x4;
               cvt = Iop_NarrowUn32to16x4;
               sh = Iop_ShrN32x4;
               imm = 1U << 15;
               imm = (imm << 32) | imm;
               break;
            case 2:
               op = Iop_Add64x2;
               cvt = Iop_NarrowUn64to32x2;
               sh = Iop_ShrN64x2;
               imm = 1U << 31;
               break;
            case 3:
               return False;
            default:
               vassert(0);
         }
         tmp = newTemp(Ity_V128);
         res = newTemp(Ity_V128);
         assign(tmp, binop(op, getQReg(nreg), getQReg(mreg)));
         if (U) {
            /* VRADDHN */
            assign(res, binop(op, mkexpr(tmp),
                     binop(Iop_64HLtoV128, mkU64(imm), mkU64(imm))));
         } else {
            assign(res, mkexpr(tmp));
         }
         putDRegI64(dreg, unop(cvt, binop(sh, mkexpr(res), mkU8(8 << size))),
                    condT);
         DIP("v%saddhn.i%d d%u, q%u, q%u\n", U ? "r" : "", 16 << size, dreg,
             nreg, mreg);
         return True;
      case 5:
         /* VABAL */
         if (!((theInstr >> 23) & 1)) {
            vpanic("VABA should not be in dis_neon_data_3diff\n");
         }
         if (dreg & 1)
            return False;
         dreg >>= 1;
         switch (size) {
            case 0:
               cmp = U ? Iop_CmpGT8Ux8 : Iop_CmpGT8Sx8;
               cvt = U ? Iop_Widen8Uto16x8 : Iop_Widen8Sto16x8;
               cvt2 = Iop_Widen8Sto16x8;
               op = Iop_Sub16x8;
               op2 = Iop_Add16x8;
               break;
            case 1:
               cmp = U ? Iop_CmpGT16Ux4 : Iop_CmpGT16Sx4;
               cvt = U ? Iop_Widen16Uto32x4 : Iop_Widen16Sto32x4;
               cvt2 = Iop_Widen16Sto32x4;
               op = Iop_Sub32x4;
               op2 = Iop_Add32x4;
               break;
            case 2:
               cmp = U ? Iop_CmpGT32Ux2 : Iop_CmpGT32Sx2;
               cvt = U ? Iop_Widen32Uto64x2 : Iop_Widen32Sto64x2;
               cvt2 = Iop_Widen32Sto64x2;
               op = Iop_Sub64x2;
               op2 = Iop_Add64x2;
               break;
            case 3:
               return False;
            default:
               vassert(0);
         }
         arg_n = newTemp(Ity_V128);
         arg_m = newTemp(Ity_V128);
         cond = newTemp(Ity_V128);
         res = newTemp(Ity_V128);
         assign(arg_n, unop(cvt, getDRegI64(nreg)));
         assign(arg_m, unop(cvt, getDRegI64(mreg)));
         assign(cond, unop(cvt2, binop(cmp, getDRegI64(nreg),
                                            getDRegI64(mreg))));
         assign(res, binop(op2,
                           binop(Iop_OrV128,
                                 binop(Iop_AndV128,
                                       binop(op, mkexpr(arg_n), mkexpr(arg_m)),
                                       mkexpr(cond)),
                                 binop(Iop_AndV128,
                                       binop(op, mkexpr(arg_m), mkexpr(arg_n)),
                                       unop(Iop_NotV128, mkexpr(cond)))),
                           getQReg(dreg)));
         putQReg(dreg, mkexpr(res), condT);
         DIP("vabal.%c%d q%u, d%u, d%u\n", U ? 'u' : 's', 8 << size, dreg,
             nreg, mreg);
         return True;
      case 6:
         /* VSUBHN, VRSUBHN */
         if (mreg & 1)
            return False;
         mreg >>= 1;
         if (nreg & 1)
            return False;
         nreg >>= 1;
         size = B;
         switch (size) {
            case 0:
               op = Iop_Sub16x8;
               op2 = Iop_Add16x8;
               cvt = Iop_NarrowUn16to8x8;
               sh = Iop_ShrN16x8;
               imm = 1U << 7;
               imm = (imm << 16) | imm;
               imm = (imm << 32) | imm;
               break;
            case 1:
               op = Iop_Sub32x4;
               op2 = Iop_Add32x4;
               cvt = Iop_NarrowUn32to16x4;
               sh = Iop_ShrN32x4;
               imm = 1U << 15;
               imm = (imm << 32) | imm;
               break;
            case 2:
               op = Iop_Sub64x2;
               op2 = Iop_Add64x2;
               cvt = Iop_NarrowUn64to32x2;
               sh = Iop_ShrN64x2;
               imm = 1U << 31;
               break;
            case 3:
               return False;
            default:
               vassert(0);
         }
         tmp = newTemp(Ity_V128);
         res = newTemp(Ity_V128);
         assign(tmp, binop(op, getQReg(nreg), getQReg(mreg)));
         if (U) {
            /* VRSUBHN */
            assign(res, binop(op2, mkexpr(tmp),
                     binop(Iop_64HLtoV128, mkU64(imm), mkU64(imm))));
         } else {
            assign(res, mkexpr(tmp));
         }
         putDRegI64(dreg, unop(cvt, binop(sh, mkexpr(res), mkU8(8 << size))),
                    condT);
         DIP("v%ssubhn.i%d d%u, q%u, q%u\n", U ? "r" : "", 16 << size, dreg,
             nreg, mreg);
         return True;
      case 7:
         /* VABDL */
         if (!((theInstr >> 23) & 1)) {
            vpanic("VABL should not be in dis_neon_data_3diff\n");
         }
         if (dreg & 1)
            return False;
         dreg >>= 1;
         switch (size) {
            case 0:
               cmp = U ? Iop_CmpGT8Ux8 : Iop_CmpGT8Sx8;
               cvt = U ? Iop_Widen8Uto16x8 : Iop_Widen8Sto16x8;
               cvt2 = Iop_Widen8Sto16x8;
               op = Iop_Sub16x8;
               break;
            case 1:
               cmp = U ? Iop_CmpGT16Ux4 : Iop_CmpGT16Sx4;
               cvt = U ? Iop_Widen16Uto32x4 : Iop_Widen16Sto32x4;
               cvt2 = Iop_Widen16Sto32x4;
               op = Iop_Sub32x4;
               break;
            case 2:
               cmp = U ? Iop_CmpGT32Ux2 : Iop_CmpGT32Sx2;
               cvt = U ? Iop_Widen32Uto64x2 : Iop_Widen32Sto64x2;
               cvt2 = Iop_Widen32Sto64x2;
               op = Iop_Sub64x2;
               break;
            case 3:
               return False;
            default:
               vassert(0);
         }
         arg_n = newTemp(Ity_V128);
         arg_m = newTemp(Ity_V128);
         cond = newTemp(Ity_V128);
         res = newTemp(Ity_V128);
         assign(arg_n, unop(cvt, getDRegI64(nreg)));
         assign(arg_m, unop(cvt, getDRegI64(mreg)));
         assign(cond, unop(cvt2, binop(cmp, getDRegI64(nreg),
                                            getDRegI64(mreg))));
         assign(res, binop(Iop_OrV128,
                           binop(Iop_AndV128,
                                 binop(op, mkexpr(arg_n), mkexpr(arg_m)),
                                 mkexpr(cond)),
                           binop(Iop_AndV128,
                                 binop(op, mkexpr(arg_m), mkexpr(arg_n)),
                                 unop(Iop_NotV128, mkexpr(cond)))));
         putQReg(dreg, mkexpr(res), condT);
         DIP("vabdl.%c%d q%u, d%u, d%u\n", U ? 'u' : 's', 8 << size, dreg,
             nreg, mreg);
         return True;
      case 8:
      case 10:
         /* VMLAL, VMLSL (integer) */
         if (dreg & 1)
            return False;
         dreg >>= 1;
         size = B;
         switch (size) {
            case 0:
               op = U ? Iop_Mull8Ux8 : Iop_Mull8Sx8;
               op2 = P ? Iop_Sub16x8 : Iop_Add16x8;
               break;
            case 1:
               op = U ? Iop_Mull16Ux4 : Iop_Mull16Sx4;
               op2 = P ? Iop_Sub32x4 : Iop_Add32x4;
               break;
            case 2:
               op = U ? Iop_Mull32Ux2 : Iop_Mull32Sx2;
               op2 = P ? Iop_Sub64x2 : Iop_Add64x2;
               break;
            case 3:
               return False;
            default:
               vassert(0);
         }
         res = newTemp(Ity_V128);
         assign(res, binop(op, getDRegI64(nreg),getDRegI64(mreg)));
         putQReg(dreg, binop(op2, getQReg(dreg), mkexpr(res)), condT);
         DIP("vml%cl.%c%d q%u, d%u, d%u\n", P ? 's' : 'a', U ? 'u' : 's',
             8 << size, dreg, nreg, mreg);
         return True;
      case 9:
      case 11:
         /* VQDMLAL, VQDMLSL */
         if (U)
            return False;
         if (dreg & 1)
            return False;
         dreg >>= 1;
         size = B;
         switch (size) {
            case 0: case 3:
               return False;
            case 1:
               op = Iop_QDMull16Sx4;
               cmp = Iop_CmpEQ16x4;
               add = P ? Iop_QSub32Sx4 : Iop_QAdd32Sx4;
               op2 = P ? Iop_Sub32x4 : Iop_Add32x4;
               imm = 1LL << 15;
               imm = (imm << 16) | imm;
               imm = (imm << 32) | imm;
               break;
            case 2:
               op = Iop_QDMull32Sx2;
               cmp = Iop_CmpEQ32x2;
               add = P ? Iop_QSub64Sx2 : Iop_QAdd64Sx2;
               op2 = P ? Iop_Sub64x2 : Iop_Add64x2;
               imm = 1LL << 31;
               imm = (imm << 32) | imm;
               break;
            default:
               vassert(0);
         }
         res = newTemp(Ity_V128);
         tmp = newTemp(Ity_V128);
         assign(res, binop(op, getDRegI64(nreg), getDRegI64(mreg)));
         assign(tmp, binop(op2, getQReg(dreg), mkexpr(res)));
         setFlag_QC(mkexpr(tmp), binop(add, getQReg(dreg), mkexpr(res)),
                    True, condT);
         setFlag_QC(binop(Iop_And64,
                          binop(cmp, getDRegI64(nreg), mkU64(imm)),
                          binop(cmp, getDRegI64(mreg), mkU64(imm))),
                    mkU64(0),
                    False, condT);
         putQReg(dreg, binop(add, getQReg(dreg), mkexpr(res)), condT);
         DIP("vqdml%cl.s%d q%u, d%u, d%u\n", P ? 's' : 'a', 8 << size, dreg,
             nreg, mreg);
         return True;
      case 12:
      case 14:
         /* VMULL (integer or polynomial) */
         if (dreg & 1)
            return False;
         dreg >>= 1;
         size = B;
         switch (size) {
            case 0:
               op = (U) ? Iop_Mull8Ux8 : Iop_Mull8Sx8;
               if (P)
                  op = Iop_PolynomialMull8x8;
               break;
            case 1:
               if (P) return False;
               op = (U) ? Iop_Mull16Ux4 : Iop_Mull16Sx4;
               break;
            case 2:
               if (P) return False;
               op = (U) ? Iop_Mull32Ux2 : Iop_Mull32Sx2;
               break;
            case 3:
               return False;
            default:
               vassert(0);
         }
         putQReg(dreg, binop(op, getDRegI64(nreg),
                                 getDRegI64(mreg)), condT);
         DIP("vmull.%c%d q%u, d%u, d%u\n", P ? 'p' : (U ? 'u' : 's'),
               8 << size, dreg, nreg, mreg);
         return True;
      case 13:
         /* VQDMULL */
         if (U)
            return False;
         if (dreg & 1)
            return False;
         dreg >>= 1;
         size = B;
         switch (size) {
            case 0:
            case 3:
               return False;
            case 1:
               op = Iop_QDMull16Sx4;
               op2 = Iop_CmpEQ16x4;
               imm = 1LL << 15;
               imm = (imm << 16) | imm;
               imm = (imm << 32) | imm;
               break;
            case 2:
               op = Iop_QDMull32Sx2;
               op2 = Iop_CmpEQ32x2;
               imm = 1LL << 31;
               imm = (imm << 32) | imm;
               break;
            default:
               vassert(0);
         }
         putQReg(dreg, binop(op, getDRegI64(nreg), getDRegI64(mreg)),
               condT);
         setFlag_QC(binop(Iop_And64,
                          binop(op2, getDRegI64(nreg), mkU64(imm)),
                          binop(op2, getDRegI64(mreg), mkU64(imm))),
                    mkU64(0),
                    False, condT);
         DIP("vqdmull.s%d q%u, d%u, d%u\n", 8 << size, dreg, nreg, mreg);
         return True;
      default:
         return False;
   }
   return False;
}

/* A7.4.3 Two registers and a scalar */
static
Bool dis_neon_data_2reg_and_scalar ( UInt theInstr, IRTemp condT )
{
#  define INSN(_bMax,_bMin)  SLICE_UInt(theInstr, (_bMax), (_bMin))
   UInt U = INSN(24,24);
   UInt dreg = get_neon_d_regno(theInstr & ~(1 << 6));
   UInt nreg = get_neon_n_regno(theInstr & ~(1 << 6));
   UInt mreg = get_neon_m_regno(theInstr & ~(1 << 6));
   UInt size = INSN(21,20);
   UInt index;
   UInt Q = INSN(24,24);

   if (INSN(27,25) != 1 || INSN(23,23) != 1
       || INSN(6,6) != 1 || INSN(4,4) != 0)
      return False;

   /* VMLA, VMLS (scalar)  */
   if ((INSN(11,8) & BITS4(1,0,1,0)) == BITS4(0,0,0,0)) {
      IRTemp res, arg_m, arg_n;
      IROp dup, get, op, op2, add, sub;
      if (Q) {
         if ((dreg & 1) || (nreg & 1))
            return False;
         dreg >>= 1;
         nreg >>= 1;
         res = newTemp(Ity_V128);
         arg_m = newTemp(Ity_V128);
         arg_n = newTemp(Ity_V128);
         assign(arg_n, getQReg(nreg));
         switch(size) {
            case 1:
               dup = Iop_Dup16x8;
               get = Iop_GetElem16x4;
               index = mreg >> 3;
               mreg &= 7;
               break;
            case 2:
               dup = Iop_Dup32x4;
               get = Iop_GetElem32x2;
               index = mreg >> 4;
               mreg &= 0xf;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      } else {
         res = newTemp(Ity_I64);
         arg_m = newTemp(Ity_I64);
         arg_n = newTemp(Ity_I64);
         assign(arg_n, getDRegI64(nreg));
         switch(size) {
            case 1:
               dup = Iop_Dup16x4;
               get = Iop_GetElem16x4;
               index = mreg >> 3;
               mreg &= 7;
               break;
            case 2:
               dup = Iop_Dup32x2;
               get = Iop_GetElem32x2;
               index = mreg >> 4;
               mreg &= 0xf;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      }
      if (INSN(8,8)) {
         switch (size) {
            case 2:
               op = Q ? Iop_Mul32Fx4 : Iop_Mul32Fx2;
               add = Q ? Iop_Add32Fx4 : Iop_Add32Fx2;
               sub = Q ? Iop_Sub32Fx4 : Iop_Sub32Fx2;
               break;
            case 0:
            case 1:
            case 3:
               return False;
            default:
               vassert(0);
         }
      } else {
         switch (size) {
            case 1:
               op = Q ? Iop_Mul16x8 : Iop_Mul16x4;
               add = Q ? Iop_Add16x8 : Iop_Add16x4;
               sub = Q ? Iop_Sub16x8 : Iop_Sub16x4;
               break;
            case 2:
               op = Q ? Iop_Mul32x4 : Iop_Mul32x2;
               add = Q ? Iop_Add32x4 : Iop_Add32x2;
               sub = Q ? Iop_Sub32x4 : Iop_Sub32x2;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
      }
      op2 = INSN(10,10) ? sub : add;
      assign(res, binop_w_fake_RM(op, mkexpr(arg_n), mkexpr(arg_m)));
      if (Q)
         putQReg(dreg, binop_w_fake_RM(op2, getQReg(dreg), mkexpr(res)),
                 condT);
      else
         putDRegI64(dreg, binop(op2, getDRegI64(dreg), mkexpr(res)),
                    condT);
      DIP("vml%c.%c%d %c%u, %c%u, d%u[%u]\n", INSN(10,10) ? 's' : 'a',
            INSN(8,8) ? 'f' : 'i', 8 << size,
            Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', nreg, mreg, index);
      return True;
   }

   /* VMLAL, VMLSL (scalar)   */
   if ((INSN(11,8) & BITS4(1,0,1,1)) == BITS4(0,0,1,0)) {
      IRTemp res, arg_m, arg_n;
      IROp dup, get, op, op2, add, sub;
      if (dreg & 1)
         return False;
      dreg >>= 1;
      res = newTemp(Ity_V128);
      arg_m = newTemp(Ity_I64);
      arg_n = newTemp(Ity_I64);
      assign(arg_n, getDRegI64(nreg));
      switch(size) {
         case 1:
            dup = Iop_Dup16x4;
            get = Iop_GetElem16x4;
            index = mreg >> 3;
            mreg &= 7;
            break;
         case 2:
            dup = Iop_Dup32x2;
            get = Iop_GetElem32x2;
            index = mreg >> 4;
            mreg &= 0xf;
            break;
         case 0:
         case 3:
            return False;
         default:
            vassert(0);
      }
      assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      switch (size) {
         case 1:
            op = U ? Iop_Mull16Ux4 : Iop_Mull16Sx4;
            add = Iop_Add32x4;
            sub = Iop_Sub32x4;
            break;
         case 2:
            op = U ? Iop_Mull32Ux2 : Iop_Mull32Sx2;
            add = Iop_Add64x2;
            sub = Iop_Sub64x2;
            break;
         case 0:
         case 3:
            return False;
         default:
            vassert(0);
      }
      op2 = INSN(10,10) ? sub : add;
      assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
      putQReg(dreg, binop(op2, getQReg(dreg), mkexpr(res)), condT);
      DIP("vml%cl.%c%d q%u, d%u, d%u[%u]\n",
          INSN(10,10) ? 's' : 'a', U ? 'u' : 's',
          8 << size, dreg, nreg, mreg, index);
      return True;
   }

   /* VQDMLAL, VQDMLSL (scalar)  */
   if ((INSN(11,8) & BITS4(1,0,1,1)) == BITS4(0,0,1,1) && !U) {
      IRTemp res, arg_m, arg_n, tmp;
      IROp dup, get, op, op2, add, cmp;
      UInt P = INSN(10,10);
      ULong imm;
      if (dreg & 1)
         return False;
      dreg >>= 1;
      res = newTemp(Ity_V128);
      arg_m = newTemp(Ity_I64);
      arg_n = newTemp(Ity_I64);
      assign(arg_n, getDRegI64(nreg));
      switch(size) {
         case 1:
            dup = Iop_Dup16x4;
            get = Iop_GetElem16x4;
            index = mreg >> 3;
            mreg &= 7;
            break;
         case 2:
            dup = Iop_Dup32x2;
            get = Iop_GetElem32x2;
            index = mreg >> 4;
            mreg &= 0xf;
            break;
         case 0:
         case 3:
            return False;
         default:
            vassert(0);
      }
      assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      switch (size) {
         case 0:
         case 3:
            return False;
         case 1:
            op = Iop_QDMull16Sx4;
            cmp = Iop_CmpEQ16x4;
            add = P ? Iop_QSub32Sx4 : Iop_QAdd32Sx4;
            op2 = P ? Iop_Sub32x4 : Iop_Add32x4;
            imm = 1LL << 15;
            imm = (imm << 16) | imm;
            imm = (imm << 32) | imm;
            break;
         case 2:
            op = Iop_QDMull32Sx2;
            cmp = Iop_CmpEQ32x2;
            add = P ? Iop_QSub64Sx2 : Iop_QAdd64Sx2;
            op2 = P ? Iop_Sub64x2 : Iop_Add64x2;
            imm = 1LL << 31;
            imm = (imm << 32) | imm;
            break;
         default:
            vassert(0);
      }
      res = newTemp(Ity_V128);
      tmp = newTemp(Ity_V128);
      assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
      assign(tmp, binop(op2, getQReg(dreg), mkexpr(res)));
      setFlag_QC(binop(Iop_And64,
                       binop(cmp, mkexpr(arg_n), mkU64(imm)),
                       binop(cmp, mkexpr(arg_m), mkU64(imm))),
                 mkU64(0),
                 False, condT);
      setFlag_QC(mkexpr(tmp), binop(add, getQReg(dreg), mkexpr(res)),
                 True, condT);
      putQReg(dreg, binop(add, getQReg(dreg), mkexpr(res)), condT);
      DIP("vqdml%cl.s%d q%u, d%u, d%u[%u]\n", P ? 's' : 'a', 8 << size,
          dreg, nreg, mreg, index);
      return True;
   }

   /* VMUL (by scalar)  */
   if ((INSN(11,8) & BITS4(1,1,1,0)) == BITS4(1,0,0,0)) {
      IRTemp res, arg_m, arg_n;
      IROp dup, get, op;
      if (Q) {
         if ((dreg & 1) || (nreg & 1))
            return False;
         dreg >>= 1;
         nreg >>= 1;
         res = newTemp(Ity_V128);
         arg_m = newTemp(Ity_V128);
         arg_n = newTemp(Ity_V128);
         assign(arg_n, getQReg(nreg));
         switch(size) {
            case 1:
               dup = Iop_Dup16x8;
               get = Iop_GetElem16x4;
               index = mreg >> 3;
               mreg &= 7;
               break;
            case 2:
               dup = Iop_Dup32x4;
               get = Iop_GetElem32x2;
               index = mreg >> 4;
               mreg &= 0xf;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      } else {
         res = newTemp(Ity_I64);
         arg_m = newTemp(Ity_I64);
         arg_n = newTemp(Ity_I64);
         assign(arg_n, getDRegI64(nreg));
         switch(size) {
            case 1:
               dup = Iop_Dup16x4;
               get = Iop_GetElem16x4;
               index = mreg >> 3;
               mreg &= 7;
               break;
            case 2:
               dup = Iop_Dup32x2;
               get = Iop_GetElem32x2;
               index = mreg >> 4;
               mreg &= 0xf;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      }
      if (INSN(8,8)) {
         switch (size) {
            case 2:
               op = Q ? Iop_Mul32Fx4 : Iop_Mul32Fx2;
               break;
            case 0:
            case 1:
            case 3:
               return False;
            default:
               vassert(0);
         }
      } else {
         switch (size) {
            case 1:
               op = Q ? Iop_Mul16x8 : Iop_Mul16x4;
               break;
            case 2:
               op = Q ? Iop_Mul32x4 : Iop_Mul32x2;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
      }
      assign(res, binop_w_fake_RM(op, mkexpr(arg_n), mkexpr(arg_m)));
      if (Q)
         putQReg(dreg, mkexpr(res), condT);
      else
         putDRegI64(dreg, mkexpr(res), condT);
      DIP("vmul.%c%d %c%u, %c%u, d%u[%u]\n", INSN(8,8) ? 'f' : 'i',
          8 << size, Q ? 'q' : 'd', dreg,
          Q ? 'q' : 'd', nreg, mreg, index);
      return True;
   }

   /* VMULL (scalar) */
   if (INSN(11,8) == BITS4(1,0,1,0)) {
      IRTemp res, arg_m, arg_n;
      IROp dup, get, op;
      if (dreg & 1)
         return False;
      dreg >>= 1;
      res = newTemp(Ity_V128);
      arg_m = newTemp(Ity_I64);
      arg_n = newTemp(Ity_I64);
      assign(arg_n, getDRegI64(nreg));
      switch(size) {
         case 1:
            dup = Iop_Dup16x4;
            get = Iop_GetElem16x4;
            index = mreg >> 3;
            mreg &= 7;
            break;
         case 2:
            dup = Iop_Dup32x2;
            get = Iop_GetElem32x2;
            index = mreg >> 4;
            mreg &= 0xf;
            break;
         case 0:
         case 3:
            return False;
         default:
            vassert(0);
      }
      assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      switch (size) {
         case 1: op = U ? Iop_Mull16Ux4 : Iop_Mull16Sx4; break;
         case 2: op = U ? Iop_Mull32Ux2 : Iop_Mull32Sx2; break;
         case 0: case 3: return False;
         default: vassert(0);
      }
      assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
      putQReg(dreg, mkexpr(res), condT);
      DIP("vmull.%c%d q%u, d%u, d%u[%u]\n", U ? 'u' : 's', 8 << size, dreg,
          nreg, mreg, index);
      return True;
   }

   /* VQDMULL */
   if (INSN(11,8) == BITS4(1,0,1,1) && !U) {
      IROp op ,op2, dup, get;
      ULong imm;
      IRTemp arg_m, arg_n;
      if (dreg & 1)
         return False;
      dreg >>= 1;
      arg_m = newTemp(Ity_I64);
      arg_n = newTemp(Ity_I64);
      assign(arg_n, getDRegI64(nreg));
      switch(size) {
         case 1:
            dup = Iop_Dup16x4;
            get = Iop_GetElem16x4;
            index = mreg >> 3;
            mreg &= 7;
            break;
         case 2:
            dup = Iop_Dup32x2;
            get = Iop_GetElem32x2;
            index = mreg >> 4;
            mreg &= 0xf;
            break;
         case 0:
         case 3:
            return False;
         default:
            vassert(0);
      }
      assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      switch (size) {
         case 0:
         case 3:
            return False;
         case 1:
            op = Iop_QDMull16Sx4;
            op2 = Iop_CmpEQ16x4;
            imm = 1LL << 15;
            imm = (imm << 16) | imm;
            imm = (imm << 32) | imm;
            break;
         case 2:
            op = Iop_QDMull32Sx2;
            op2 = Iop_CmpEQ32x2;
            imm = 1LL << 31;
            imm = (imm << 32) | imm;
            break;
         default:
            vassert(0);
      }
      putQReg(dreg, binop(op, mkexpr(arg_n), mkexpr(arg_m)),
            condT);
      setFlag_QC(binop(Iop_And64,
                       binop(op2, mkexpr(arg_n), mkU64(imm)),
                       binop(op2, mkexpr(arg_m), mkU64(imm))),
                 mkU64(0),
                 False, condT);
      DIP("vqdmull.s%d q%u, d%u, d%u[%u]\n", 8 << size, dreg, nreg, mreg,
          index);
      return True;
   }

   /* VQDMULH */
   if (INSN(11,8) == BITS4(1,1,0,0)) {
      IROp op ,op2, dup, get;
      ULong imm;
      IRTemp res, arg_m, arg_n;
      if (Q) {
         if ((dreg & 1) || (nreg & 1))
            return False;
         dreg >>= 1;
         nreg >>= 1;
         res = newTemp(Ity_V128);
         arg_m = newTemp(Ity_V128);
         arg_n = newTemp(Ity_V128);
         assign(arg_n, getQReg(nreg));
         switch(size) {
            case 1:
               dup = Iop_Dup16x8;
               get = Iop_GetElem16x4;
               index = mreg >> 3;
               mreg &= 7;
               break;
            case 2:
               dup = Iop_Dup32x4;
               get = Iop_GetElem32x2;
               index = mreg >> 4;
               mreg &= 0xf;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      } else {
         res = newTemp(Ity_I64);
         arg_m = newTemp(Ity_I64);
         arg_n = newTemp(Ity_I64);
         assign(arg_n, getDRegI64(nreg));
         switch(size) {
            case 1:
               dup = Iop_Dup16x4;
               get = Iop_GetElem16x4;
               index = mreg >> 3;
               mreg &= 7;
               break;
            case 2:
               dup = Iop_Dup32x2;
               get = Iop_GetElem32x2;
               index = mreg >> 4;
               mreg &= 0xf;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      }
      switch (size) {
         case 0:
         case 3:
            return False;
         case 1:
            op = Q ? Iop_QDMulHi16Sx8 : Iop_QDMulHi16Sx4;
            op2 = Q ? Iop_CmpEQ16x8 : Iop_CmpEQ16x4;
            imm = 1LL << 15;
            imm = (imm << 16) | imm;
            imm = (imm << 32) | imm;
            break;
         case 2:
            op = Q ? Iop_QDMulHi32Sx4 : Iop_QDMulHi32Sx2;
            op2 = Q ? Iop_CmpEQ32x4 : Iop_CmpEQ32x2;
            imm = 1LL << 31;
            imm = (imm << 32) | imm;
            break;
         default:
            vassert(0);
      }
      assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
      setFlag_QC(binop(Q ? Iop_AndV128 : Iop_And64,
                       binop(op2, mkexpr(arg_n),
                                  Q ? mkU128(imm) : mkU64(imm)),
                       binop(op2, mkexpr(arg_m),
                             Q ? mkU128(imm) : mkU64(imm))),
                 Q ? mkU128(0) : mkU64(0),
                 Q, condT);
      if (Q)
         putQReg(dreg, mkexpr(res), condT);
      else
         putDRegI64(dreg, mkexpr(res), condT);
      DIP("vqdmulh.s%d %c%u, %c%u, d%u[%u]\n",
          8 << size, Q ? 'q' : 'd', dreg,
          Q ? 'q' : 'd', nreg, mreg, index);
      return True;
   }

   /* VQRDMULH (scalar) */
   if (INSN(11,8) == BITS4(1,1,0,1)) {
      IROp op ,op2, dup, get;
      ULong imm;
      IRTemp res, arg_m, arg_n;
      if (Q) {
         if ((dreg & 1) || (nreg & 1))
            return False;
         dreg >>= 1;
         nreg >>= 1;
         res = newTemp(Ity_V128);
         arg_m = newTemp(Ity_V128);
         arg_n = newTemp(Ity_V128);
         assign(arg_n, getQReg(nreg));
         switch(size) {
            case 1:
               dup = Iop_Dup16x8;
               get = Iop_GetElem16x4;
               index = mreg >> 3;
               mreg &= 7;
               break;
            case 2:
               dup = Iop_Dup32x4;
               get = Iop_GetElem32x2;
               index = mreg >> 4;
               mreg &= 0xf;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      } else {
         res = newTemp(Ity_I64);
         arg_m = newTemp(Ity_I64);
         arg_n = newTemp(Ity_I64);
         assign(arg_n, getDRegI64(nreg));
         switch(size) {
            case 1:
               dup = Iop_Dup16x4;
               get = Iop_GetElem16x4;
               index = mreg >> 3;
               mreg &= 7;
               break;
            case 2:
               dup = Iop_Dup32x2;
               get = Iop_GetElem32x2;
               index = mreg >> 4;
               mreg &= 0xf;
               break;
            case 0:
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(arg_m, unop(dup, binop(get, getDRegI64(mreg), mkU8(index))));
      }
      switch (size) {
         case 0:
         case 3:
            return False;
         case 1:
            op = Q ? Iop_QRDMulHi16Sx8 : Iop_QRDMulHi16Sx4;
            op2 = Q ? Iop_CmpEQ16x8 : Iop_CmpEQ16x4;
            imm = 1LL << 15;
            imm = (imm << 16) | imm;
            imm = (imm << 32) | imm;
            break;
         case 2:
            op = Q ? Iop_QRDMulHi32Sx4 : Iop_QRDMulHi32Sx2;
            op2 = Q ? Iop_CmpEQ32x4 : Iop_CmpEQ32x2;
            imm = 1LL << 31;
            imm = (imm << 32) | imm;
            break;
         default:
            vassert(0);
      }
      assign(res, binop(op, mkexpr(arg_n), mkexpr(arg_m)));
      setFlag_QC(binop(Q ? Iop_AndV128 : Iop_And64,
                       binop(op2, mkexpr(arg_n),
                                  Q ? mkU128(imm) : mkU64(imm)),
                       binop(op2, mkexpr(arg_m),
                                  Q ? mkU128(imm) : mkU64(imm))),
                 Q ? mkU128(0) : mkU64(0),
                 Q, condT);
      if (Q)
         putQReg(dreg, mkexpr(res), condT);
      else
         putDRegI64(dreg, mkexpr(res), condT);
      DIP("vqrdmulh.s%d %c%u, %c%u, d%u[%u]\n",
          8 << size, Q ? 'q' : 'd', dreg,
          Q ? 'q' : 'd', nreg, mreg, index);
      return True;
   }

   return False;
#  undef INSN
}

/* A7.4.4 Two registers and a shift amount */
static
Bool dis_neon_data_2reg_and_shift ( UInt theInstr, IRTemp condT )
{
   UInt A = (theInstr >> 8) & 0xf;
   UInt B = (theInstr >> 6) & 1;
   UInt L = (theInstr >> 7) & 1;
   UInt U = (theInstr >> 24) & 1;
   UInt Q = B;
   UInt imm6 = (theInstr >> 16) & 0x3f;
   UInt shift_imm;
   UInt size = 4;
   UInt tmp;
   UInt mreg = get_neon_m_regno(theInstr);
   UInt dreg = get_neon_d_regno(theInstr);
   ULong imm = 0;
   IROp op, cvt, add = Iop_INVALID, cvt2, op_rev;
   IRTemp reg_m, res, mask;

   if (L == 0 && ((theInstr >> 19) & 7) == 0)
      /* It is one reg and immediate */
      return False;

   tmp = (L << 6) | imm6;
   if (tmp & 0x40) {
      size = 3;
      shift_imm = 64 - imm6;
   } else if (tmp & 0x20) {
      size = 2;
      shift_imm = 64 - imm6;
   } else if (tmp & 0x10) {
      size = 1;
      shift_imm = 32 - imm6;
   } else if (tmp & 0x8) {
      size = 0;
      shift_imm = 16 - imm6;
   } else {
      return False;
   }

   switch (A) {
      case 3:
      case 2:
         /* VRSHR, VRSRA */
         if (shift_imm > 0) {
            IRExpr *imm_val;
            imm = 1L;
            switch (size) {
               case 0:
                  imm = (imm << 8) | imm;
                  /* fall through */
               case 1:
                  imm = (imm << 16) | imm;
                  /* fall through */
               case 2:
                  imm = (imm << 32) | imm;
                  /* fall through */
               case 3:
                  break;
               default:
                  vassert(0);
            }
            if (Q) {
               reg_m = newTemp(Ity_V128);
               res = newTemp(Ity_V128);
               imm_val = binop(Iop_64HLtoV128, mkU64(imm), mkU64(imm));
               assign(reg_m, getQReg(mreg));
               switch (size) {
                  case 0:
                     add = Iop_Add8x16;
                     op = U ? Iop_ShrN8x16 : Iop_SarN8x16;
                     break;
                  case 1:
                     add = Iop_Add16x8;
                     op = U ? Iop_ShrN16x8 : Iop_SarN16x8;
                     break;
                  case 2:
                     add = Iop_Add32x4;
                     op = U ? Iop_ShrN32x4 : Iop_SarN32x4;
                     break;
                  case 3:
                     add = Iop_Add64x2;
                     op = U ? Iop_ShrN64x2 : Iop_SarN64x2;
                     break;
                  default:
                     vassert(0);
               }
            } else {
               reg_m = newTemp(Ity_I64);
               res = newTemp(Ity_I64);
               imm_val = mkU64(imm);
               assign(reg_m, getDRegI64(mreg));
               switch (size) {
                  case 0:
                     add = Iop_Add8x8;
                     op = U ? Iop_ShrN8x8 : Iop_SarN8x8;
                     break;
                  case 1:
                     add = Iop_Add16x4;
                     op = U ? Iop_ShrN16x4 : Iop_SarN16x4;
                     break;
                  case 2:
                     add = Iop_Add32x2;
                     op = U ? Iop_ShrN32x2 : Iop_SarN32x2;
                     break;
                  case 3:
                     add = Iop_Add64;
                     op = U ? Iop_Shr64 : Iop_Sar64;
                     break;
                  default:
                     vassert(0);
               }
            }
            assign(res,
                   binop(add,
                         binop(op,
                               mkexpr(reg_m),
                               mkU8(shift_imm)),
                         binop(Q ? Iop_AndV128 : Iop_And64,
                               binop(op,
                                     mkexpr(reg_m),
                                     mkU8(shift_imm - 1)),
                               imm_val)));
         } else {
            if (Q) {
               res = newTemp(Ity_V128);
               assign(res, getQReg(mreg));
            } else {
               res = newTemp(Ity_I64);
               assign(res, getDRegI64(mreg));
            }
         }
         if (A == 3) {
            if (Q) {
               putQReg(dreg, binop(add, mkexpr(res), getQReg(dreg)),
                             condT);
            } else {
               putDRegI64(dreg, binop(add, mkexpr(res), getDRegI64(dreg)),
                                condT);
            }
            DIP("vrsra.%c%d %c%u, %c%u, #%u\n",
                U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, shift_imm);
         } else {
            if (Q) {
               putQReg(dreg, mkexpr(res), condT);
            } else {
               putDRegI64(dreg, mkexpr(res), condT);
            }
            DIP("vrshr.%c%d %c%u, %c%u, #%u\n", U ? 'u' : 's', 8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, shift_imm);
         }
         return True;
      case 1:
      case 0:
         /* VSHR, VSRA */
         if (Q) {
            reg_m = newTemp(Ity_V128);
            assign(reg_m, getQReg(mreg));
            res = newTemp(Ity_V128);
         } else {
            reg_m = newTemp(Ity_I64);
            assign(reg_m, getDRegI64(mreg));
            res = newTemp(Ity_I64);
         }
         if (Q) {
            switch (size) {
               case 0:
                  op = U ? Iop_ShrN8x16 : Iop_SarN8x16;
                  add = Iop_Add8x16;
                  break;
               case 1:
                  op = U ? Iop_ShrN16x8 : Iop_SarN16x8;
                  add = Iop_Add16x8;
                  break;
               case 2:
                  op = U ? Iop_ShrN32x4 : Iop_SarN32x4;
                  add = Iop_Add32x4;
                  break;
               case 3:
                  op = U ? Iop_ShrN64x2 : Iop_SarN64x2;
                  add = Iop_Add64x2;
                  break;
               default:
                  vassert(0);
            }
         } else {
            switch (size) {
               case 0:
                  op =  U ? Iop_ShrN8x8 : Iop_SarN8x8;
                  add = Iop_Add8x8;
                  break;
               case 1:
                  op = U ? Iop_ShrN16x4 : Iop_SarN16x4;
                  add = Iop_Add16x4;
                  break;
               case 2:
                  op = U ? Iop_ShrN32x2 : Iop_SarN32x2;
                  add = Iop_Add32x2;
                  break;
               case 3:
                  op = U ? Iop_Shr64 : Iop_Sar64;
                  add = Iop_Add64;
                  break;
               default:
                  vassert(0);
            }
         }
         assign(res, binop(op, mkexpr(reg_m), mkU8(shift_imm)));
         if (A == 1) {
            if (Q) {
               putQReg(dreg, binop(add, mkexpr(res), getQReg(dreg)),
                             condT);
            } else {
               putDRegI64(dreg, binop(add, mkexpr(res), getDRegI64(dreg)),
                                condT);
            }
            DIP("vsra.%c%d %c%u, %c%u, #%u\n", U ? 'u' : 's', 8 << size,
                  Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, shift_imm);
         } else {
            if (Q) {
               putQReg(dreg, mkexpr(res), condT);
            } else {
               putDRegI64(dreg, mkexpr(res), condT);
            }
            DIP("vshr.%c%d %c%u, %c%u, #%u\n", U ? 'u' : 's', 8 << size,
                  Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, shift_imm);
         }
         return True;
      case 4:
         /* VSRI */
         if (!U)
            return False;
         if (Q) {
            res = newTemp(Ity_V128);
            mask = newTemp(Ity_V128);
         } else {
            res = newTemp(Ity_I64);
            mask = newTemp(Ity_I64);
         }
         switch (size) {
            case 0: op = Q ? Iop_ShrN8x16 : Iop_ShrN8x8; break;
            case 1: op = Q ? Iop_ShrN16x8 : Iop_ShrN16x4; break;
            case 2: op = Q ? Iop_ShrN32x4 : Iop_ShrN32x2; break;
            case 3: op = Q ? Iop_ShrN64x2 : Iop_Shr64; break;
            default: vassert(0);
         }
         if (Q) {
            assign(mask, binop(op, binop(Iop_64HLtoV128,
                                         mkU64(0xFFFFFFFFFFFFFFFFLL),
                                         mkU64(0xFFFFFFFFFFFFFFFFLL)),
                               mkU8(shift_imm)));
            assign(res, binop(Iop_OrV128,
                              binop(Iop_AndV128,
                                    getQReg(dreg),
                                    unop(Iop_NotV128,
                                         mkexpr(mask))),
                              binop(op,
                                    getQReg(mreg),
                                    mkU8(shift_imm))));
            putQReg(dreg, mkexpr(res), condT);
         } else {
            assign(mask, binop(op, mkU64(0xFFFFFFFFFFFFFFFFLL),
                               mkU8(shift_imm)));
            assign(res, binop(Iop_Or64,
                              binop(Iop_And64,
                                    getDRegI64(dreg),
                                    unop(Iop_Not64,
                                         mkexpr(mask))),
                              binop(op,
                                    getDRegI64(mreg),
                                    mkU8(shift_imm))));
            putDRegI64(dreg, mkexpr(res), condT);
         }
         DIP("vsri.%d %c%u, %c%u, #%u\n",
             8 << size, Q ? 'q' : 'd', dreg,
             Q ? 'q' : 'd', mreg, shift_imm);
         return True;
      case 5:
         if (U) {
            /* VSLI */
            shift_imm = 8 * (1 << size) - shift_imm;
            if (Q) {
               res = newTemp(Ity_V128);
               mask = newTemp(Ity_V128);
            } else {
               res = newTemp(Ity_I64);
               mask = newTemp(Ity_I64);
            }
            switch (size) {
               case 0: op = Q ? Iop_ShlN8x16 : Iop_ShlN8x8; break;
               case 1: op = Q ? Iop_ShlN16x8 : Iop_ShlN16x4; break;
               case 2: op = Q ? Iop_ShlN32x4 : Iop_ShlN32x2; break;
               case 3: op = Q ? Iop_ShlN64x2 : Iop_Shl64; break;
               default: vassert(0);
            }
            if (Q) {
               assign(mask, binop(op, binop(Iop_64HLtoV128,
                                            mkU64(0xFFFFFFFFFFFFFFFFLL),
                                            mkU64(0xFFFFFFFFFFFFFFFFLL)),
                                  mkU8(shift_imm)));
               assign(res, binop(Iop_OrV128,
                                 binop(Iop_AndV128,
                                       getQReg(dreg),
                                       unop(Iop_NotV128,
                                            mkexpr(mask))),
                                 binop(op,
                                       getQReg(mreg),
                                       mkU8(shift_imm))));
               putQReg(dreg, mkexpr(res), condT);
            } else {
               assign(mask, binop(op, mkU64(0xFFFFFFFFFFFFFFFFLL),
                                  mkU8(shift_imm)));
               assign(res, binop(Iop_Or64,
                                 binop(Iop_And64,
                                       getDRegI64(dreg),
                                       unop(Iop_Not64,
                                            mkexpr(mask))),
                                 binop(op,
                                       getDRegI64(mreg),
                                       mkU8(shift_imm))));
               putDRegI64(dreg, mkexpr(res), condT);
            }
            DIP("vsli.%d %c%u, %c%u, #%u\n",
                8 << size, Q ? 'q' : 'd', dreg,
                Q ? 'q' : 'd', mreg, shift_imm);
            return True;
         } else {
            /* VSHL #imm */
            shift_imm = 8 * (1 << size) - shift_imm;
            if (Q) {
               res = newTemp(Ity_V128);
            } else {
               res = newTemp(Ity_I64);
            }
            switch (size) {
               case 0: op = Q ? Iop_ShlN8x16 : Iop_ShlN8x8; break;
               case 1: op = Q ? Iop_ShlN16x8 : Iop_ShlN16x4; break;
               case 2: op = Q ? Iop_ShlN32x4 : Iop_ShlN32x2; break;
               case 3: op = Q ? Iop_ShlN64x2 : Iop_Shl64; break;
               default: vassert(0);
            }
            assign(res, binop(op, Q ? getQReg(mreg) : getDRegI64(mreg),
                     mkU8(shift_imm)));
            if (Q) {
               putQReg(dreg, mkexpr(res), condT);
            } else {
               putDRegI64(dreg, mkexpr(res), condT);
            }
            DIP("vshl.i%d %c%u, %c%u, #%u\n",
                8 << size, Q ? 'q' : 'd', dreg,
                Q ? 'q' : 'd', mreg, shift_imm);
            return True;
         }
         break;
      case 6:
      case 7:
         /* VQSHL, VQSHLU */
         shift_imm = 8 * (1 << size) - shift_imm;
         if (U) {
            if (A & 1) {
               switch (size) {
                  case 0:
                     op = Q ? Iop_QShlNsatUU8x16 : Iop_QShlNsatUU8x8;
                     op_rev = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     break;
                  case 1:
                     op = Q ? Iop_QShlNsatUU16x8 : Iop_QShlNsatUU16x4;
                     op_rev = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     break;
                  case 2:
                     op = Q ? Iop_QShlNsatUU32x4 : Iop_QShlNsatUU32x2;
                     op_rev = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     break;
                  case 3:
                     op = Q ? Iop_QShlNsatUU64x2 : Iop_QShlNsatUU64x1;
                     op_rev = Q ? Iop_ShrN64x2 : Iop_Shr64;
                     break;
                  default:
                     vassert(0);
               }
               DIP("vqshl.u%d %c%u, %c%u, #%u\n",
                   8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, shift_imm);
            } else {
               switch (size) {
                  case 0:
                     op = Q ? Iop_QShlNsatSU8x16 : Iop_QShlNsatSU8x8;
                     op_rev = Q ? Iop_ShrN8x16 : Iop_ShrN8x8;
                     break;
                  case 1:
                     op = Q ? Iop_QShlNsatSU16x8 : Iop_QShlNsatSU16x4;
                     op_rev = Q ? Iop_ShrN16x8 : Iop_ShrN16x4;
                     break;
                  case 2:
                     op = Q ? Iop_QShlNsatSU32x4 : Iop_QShlNsatSU32x2;
                     op_rev = Q ? Iop_ShrN32x4 : Iop_ShrN32x2;
                     break;
                  case 3:
                     op = Q ? Iop_QShlNsatSU64x2 : Iop_QShlNsatSU64x1;
                     op_rev = Q ? Iop_ShrN64x2 : Iop_Shr64;
                     break;
                  default:
                     vassert(0);
               }
               DIP("vqshlu.s%d %c%u, %c%u, #%u\n",
                   8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, shift_imm);
            }
         } else {
            if (!(A & 1))
               return False;
            switch (size) {
               case 0:
                  op = Q ? Iop_QShlNsatSS8x16 : Iop_QShlNsatSS8x8;
                  op_rev = Q ? Iop_SarN8x16 : Iop_SarN8x8;
                  break;
               case 1:
                  op = Q ? Iop_QShlNsatSS16x8 : Iop_QShlNsatSS16x4;
                  op_rev = Q ? Iop_SarN16x8 : Iop_SarN16x4;
                  break;
               case 2:
                  op = Q ? Iop_QShlNsatSS32x4 : Iop_QShlNsatSS32x2;
                  op_rev = Q ? Iop_SarN32x4 : Iop_SarN32x2;
                  break;
               case 3:
                  op = Q ? Iop_QShlNsatSS64x2 : Iop_QShlNsatSS64x1;
                  op_rev = Q ? Iop_SarN64x2 : Iop_Sar64;
                  break;
               default:
                  vassert(0);
            }
            DIP("vqshl.s%d %c%u, %c%u, #%u\n",
                8 << size,
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg, shift_imm);
         }
         if (Q) {
            tmp = newTemp(Ity_V128);
            res = newTemp(Ity_V128);
            reg_m = newTemp(Ity_V128);
            assign(reg_m, getQReg(mreg));
         } else {
            tmp = newTemp(Ity_I64);
            res = newTemp(Ity_I64);
            reg_m = newTemp(Ity_I64);
            assign(reg_m, getDRegI64(mreg));
         }
         assign(res, binop(op, mkexpr(reg_m), mkU8(shift_imm)));
         assign(tmp, binop(op_rev, mkexpr(res), mkU8(shift_imm)));
         setFlag_QC(mkexpr(tmp), mkexpr(reg_m), Q, condT);
         if (Q)
            putQReg(dreg, mkexpr(res), condT);
         else
            putDRegI64(dreg, mkexpr(res), condT);
         return True;
      case 8:
         if (!U) {
            if (L == 1)
               return False;
            size++;
            dreg = ((theInstr >> 18) & 0x10) | ((theInstr >> 12) & 0xF);
            mreg = ((theInstr >> 1) & 0x10) | (theInstr & 0xF);
            if (mreg & 1)
               return False;
            mreg >>= 1;
            if (!B) {
               /* VSHRN*/
               IROp narOp;
               reg_m = newTemp(Ity_V128);
               assign(reg_m, getQReg(mreg));
               res = newTemp(Ity_I64);
               switch (size) {
                  case 1:
                     op = Iop_ShrN16x8;
                     narOp = Iop_NarrowUn16to8x8;
                     break;
                  case 2:
                     op = Iop_ShrN32x4;
                     narOp = Iop_NarrowUn32to16x4;
                     break;
                  case 3:
                     op = Iop_ShrN64x2;
                     narOp = Iop_NarrowUn64to32x2;
                     break;
                  default:
                     vassert(0);
               }
               assign(res, unop(narOp,
                                binop(op,
                                      mkexpr(reg_m),
                                      mkU8(shift_imm))));
               putDRegI64(dreg, mkexpr(res), condT);
               DIP("vshrn.i%d d%u, q%u, #%u\n", 8 << size, dreg, mreg,
                   shift_imm);
               return True;
            } else {
               /* VRSHRN   */
               IROp addOp, shOp, narOp;
               IRExpr *imm_val;
               reg_m = newTemp(Ity_V128);
               assign(reg_m, getQReg(mreg));
               res = newTemp(Ity_I64);
               imm = 1L;
               switch (size) {
                  case 0: imm = (imm <<  8) | imm; /* fall through */
                  case 1: imm = (imm << 16) | imm; /* fall through */
                  case 2: imm = (imm << 32) | imm; /* fall through */
                  case 3: break;
                  default: vassert(0);
               }
               imm_val = binop(Iop_64HLtoV128, mkU64(imm), mkU64(imm));
               switch (size) {
                  case 1:
                     addOp = Iop_Add16x8;
                     shOp = Iop_ShrN16x8;
                     narOp = Iop_NarrowUn16to8x8;
                     break;
                  case 2:
                     addOp = Iop_Add32x4;
                     shOp = Iop_ShrN32x4;
                     narOp = Iop_NarrowUn32to16x4;
                     break;
                  case 3:
                     addOp = Iop_Add64x2;
                     shOp = Iop_ShrN64x2;
                     narOp = Iop_NarrowUn64to32x2;
                     break;
                  default:
                     vassert(0);
               }
               assign(res, unop(narOp,
                                binop(addOp,
                                      binop(shOp,
                                            mkexpr(reg_m),
                                            mkU8(shift_imm)),
                                      binop(Iop_AndV128,
                                            binop(shOp,
                                                  mkexpr(reg_m),
                                                  mkU8(shift_imm - 1)),
                                            imm_val))));
               putDRegI64(dreg, mkexpr(res), condT);
               if (shift_imm == 0) {
                  DIP("vmov%d d%u, q%u, #%u\n", 8 << size, dreg, mreg,
                      shift_imm);
               } else {
                  DIP("vrshrn.i%d d%u, q%u, #%u\n", 8 << size, dreg, mreg,
                      shift_imm);
               }
               return True;
            }
         } else {
            /* fall through */
         }
      case 9:
         dreg = ((theInstr >> 18) & 0x10) | ((theInstr >> 12) & 0xF);
         mreg = ((theInstr >>  1) & 0x10) | (theInstr & 0xF);
         if (mreg & 1)
            return False;
         mreg >>= 1;
         size++;
         if ((theInstr >> 8) & 1) {
            switch (size) {
               case 1:
                  op = U ? Iop_ShrN16x8 : Iop_SarN16x8;
                  cvt = U ? Iop_QNarrowUn16Uto8Ux8 : Iop_QNarrowUn16Sto8Sx8;
                  cvt2 = U ? Iop_Widen8Uto16x8 : Iop_Widen8Sto16x8;
                  break;
               case 2:
                  op = U ? Iop_ShrN32x4 : Iop_SarN32x4;
                  cvt = U ? Iop_QNarrowUn32Uto16Ux4 : Iop_QNarrowUn32Sto16Sx4;
                  cvt2 = U ? Iop_Widen16Uto32x4 : Iop_Widen16Sto32x4;
                  break;
               case 3:
                  op = U ? Iop_ShrN64x2 : Iop_SarN64x2;
                  cvt = U ? Iop_QNarrowUn64Uto32Ux2 : Iop_QNarrowUn64Sto32Sx2;
                  cvt2 = U ? Iop_Widen32Uto64x2 : Iop_Widen32Sto64x2;
                  break;
               default:
                  vassert(0);
            }
            DIP("vq%sshrn.%c%d d%u, q%u, #%u\n", B ? "r" : "",
                U ? 'u' : 's', 8 << size, dreg, mreg, shift_imm);
         } else {
            vassert(U);
            switch (size) {
               case 1:
                  op = Iop_SarN16x8;
                  cvt = Iop_QNarrowUn16Sto8Ux8;
                  cvt2 = Iop_Widen8Uto16x8;
                  break;
               case 2:
                  op = Iop_SarN32x4;
                  cvt = Iop_QNarrowUn32Sto16Ux4;
                  cvt2 = Iop_Widen16Uto32x4;
                  break;
               case 3:
                  op = Iop_SarN64x2;
                  cvt = Iop_QNarrowUn64Sto32Ux2;
                  cvt2 = Iop_Widen32Uto64x2;
                  break;
               default:
                  vassert(0);
            }
            DIP("vq%sshrun.s%d d%u, q%u, #%u\n", B ? "r" : "",
                8 << size, dreg, mreg, shift_imm);
         }
         if (B) {
            if (shift_imm > 0) {
               imm = 1;
               switch (size) {
                  case 1: imm = (imm << 16) | imm; /* fall through */
                  case 2: imm = (imm << 32) | imm; /* fall through */
                  case 3: break;
                  case 0: default: vassert(0);
               }
               switch (size) {
                  case 1: add = Iop_Add16x8; break;
                  case 2: add = Iop_Add32x4; break;
                  case 3: add = Iop_Add64x2; break;
                  case 0: default: vassert(0);
               }
            }
         }
         reg_m = newTemp(Ity_V128);
         res = newTemp(Ity_V128);
         assign(reg_m, getQReg(mreg));
         if (B) {
            /* VQRSHRN, VQRSHRUN */
            assign(res, binop(add,
                              binop(op, mkexpr(reg_m), mkU8(shift_imm)),
                              binop(Iop_AndV128,
                                    binop(op,
                                          mkexpr(reg_m),
                                          mkU8(shift_imm - 1)),
                                    mkU128(imm))));
         } else {
            /* VQSHRN, VQSHRUN */
            assign(res, binop(op, mkexpr(reg_m), mkU8(shift_imm)));
         }
         setFlag_QC(unop(cvt2, unop(cvt, mkexpr(res))), mkexpr(res),
                    True, condT);
         putDRegI64(dreg, unop(cvt, mkexpr(res)), condT);
         return True;
      case 10:
         /* VSHLL
            VMOVL ::= VSHLL #0 */
         if (B)
            return False;
         if (dreg & 1)
            return False;
         dreg >>= 1;
         shift_imm = (8 << size) - shift_imm;
         res = newTemp(Ity_V128);
         switch (size) {
            case 0:
               op = Iop_ShlN16x8;
               cvt = U ? Iop_Widen8Uto16x8 : Iop_Widen8Sto16x8;
               break;
            case 1:
               op = Iop_ShlN32x4;
               cvt = U ? Iop_Widen16Uto32x4 : Iop_Widen16Sto32x4;
               break;
            case 2:
               op = Iop_ShlN64x2;
               cvt = U ? Iop_Widen32Uto64x2 : Iop_Widen32Sto64x2;
               break;
            case 3:
               return False;
            default:
               vassert(0);
         }
         assign(res, binop(op, unop(cvt, getDRegI64(mreg)), mkU8(shift_imm)));
         putQReg(dreg, mkexpr(res), condT);
         if (shift_imm == 0) {
            DIP("vmovl.%c%d q%u, d%u\n", U ? 'u' : 's', 8 << size,
                dreg, mreg);
         } else {
            DIP("vshll.%c%d q%u, d%u, #%u\n", U ? 'u' : 's', 8 << size,
                dreg, mreg, shift_imm);
         }
         return True;
      case 14:
      case 15:
         /* VCVT floating-point <-> fixed-point */
         if ((theInstr >> 8) & 1) {
            if (U) {
               op = Q ? Iop_F32ToFixed32Ux4_RZ : Iop_F32ToFixed32Ux2_RZ;
            } else {
               op = Q ? Iop_F32ToFixed32Sx4_RZ : Iop_F32ToFixed32Sx2_RZ;
            }
            DIP("vcvt.%c32.f32 %c%u, %c%u, #%u\n", U ? 'u' : 's',
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg,
                64 - ((theInstr >> 16) & 0x3f));
         } else {
            if (U) {
               op = Q ? Iop_Fixed32UToF32x4_RN : Iop_Fixed32UToF32x2_RN;
            } else {
               op = Q ? Iop_Fixed32SToF32x4_RN : Iop_Fixed32SToF32x2_RN;
            }
            DIP("vcvt.f32.%c32 %c%u, %c%u, #%u\n", U ? 'u' : 's',
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg,
                64 - ((theInstr >> 16) & 0x3f));
         }
         if (((theInstr >> 21) & 1) == 0)
            return False;
         if (Q) {
            putQReg(dreg, binop(op, getQReg(mreg),
                     mkU8(64 - ((theInstr >> 16) & 0x3f))), condT);
         } else {
            putDRegI64(dreg, binop(op, getDRegI64(mreg),
                       mkU8(64 - ((theInstr >> 16) & 0x3f))), condT);
         }
         return True;
      default:
         return False;

   }
   return False;
}

/* A7.4.5 Two registers, miscellaneous */
static
Bool dis_neon_data_2reg_misc ( UInt theInstr, IRTemp condT )
{
   UInt A = (theInstr >> 16) & 3;
   UInt B = (theInstr >> 6) & 0x1f;
   UInt Q = (theInstr >> 6) & 1;
   UInt U = (theInstr >> 24) & 1;
   UInt size = (theInstr >> 18) & 3;
   UInt dreg = get_neon_d_regno(theInstr);
   UInt mreg = get_neon_m_regno(theInstr);
   UInt F = (theInstr >> 10) & 1;
   IRTemp arg_d = IRTemp_INVALID;
   IRTemp arg_m = IRTemp_INVALID;
   IRTemp res = IRTemp_INVALID;
   switch (A) {
      case 0:
         if (Q) {
            arg_m = newTemp(Ity_V128);
            res = newTemp(Ity_V128);
            assign(arg_m, getQReg(mreg));
         } else {
            arg_m = newTemp(Ity_I64);
            res = newTemp(Ity_I64);
            assign(arg_m, getDRegI64(mreg));
         }
         switch (B >> 1) {
            case 0: {
               /* VREV64 */
               IROp op;
               switch (size) {
                  case 0:
                     op = Q ? Iop_Reverse8sIn64_x2 : Iop_Reverse8sIn64_x1;
                     break;
                  case 1:
                     op = Q ? Iop_Reverse16sIn64_x2 : Iop_Reverse16sIn64_x1;
                     break;
                  case 2:
                     op = Q ? Iop_Reverse32sIn64_x2 : Iop_Reverse32sIn64_x1;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
               assign(res, unop(op, mkexpr(arg_m)));
               DIP("vrev64.%d %c%u, %c%u\n", 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 1: {
               /* VREV32 */
               IROp op;
               switch (size) {
                  case 0:
                     op = Q ? Iop_Reverse8sIn32_x4 : Iop_Reverse8sIn32_x2;
                     break;
                  case 1:
                     op = Q ? Iop_Reverse16sIn32_x4 : Iop_Reverse16sIn32_x2;
                     break;
                  case 2:
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
               assign(res, unop(op, mkexpr(arg_m)));
               DIP("vrev32.%d %c%u, %c%u\n", 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 2: {
               /* VREV16 */
               IROp op;
               switch (size) {
                  case 0:
                     op = Q ? Iop_Reverse8sIn16_x8 : Iop_Reverse8sIn16_x4;
                     break;
                  case 1:
                  case 2:
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
               assign(res, unop(op, mkexpr(arg_m)));
               DIP("vrev16.%d %c%u, %c%u\n", 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 3:
               return False;
            case 4:
            case 5: {
               /* VPADDL */
               IROp op;
               U = (theInstr >> 7) & 1;
               if (Q) {
                  switch (size) {
                     case 0: op = U ? Iop_PwAddL8Ux16 : Iop_PwAddL8Sx16; break;
                     case 1: op = U ? Iop_PwAddL16Ux8 : Iop_PwAddL16Sx8; break;
                     case 2: op = U ? Iop_PwAddL32Ux4 : Iop_PwAddL32Sx4; break;
                     case 3: return False;
                     default: vassert(0);
                  }
               } else {
                  switch (size) {
                     case 0: op = U ? Iop_PwAddL8Ux8  : Iop_PwAddL8Sx8;  break;
                     case 1: op = U ? Iop_PwAddL16Ux4 : Iop_PwAddL16Sx4; break;
                     case 2: op = U ? Iop_PwAddL32Ux2 : Iop_PwAddL32Sx2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
               }
               assign(res, unop(op, mkexpr(arg_m)));
               DIP("vpaddl.%c%d %c%u, %c%u\n", U ? 'u' : 's', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 6:
            case 7:
               return False;
            case 8: {
               /* VCLS */
               IROp op;
               switch (size) {
                  case 0: op = Q ? Iop_Cls8x16 : Iop_Cls8x8; break;
                  case 1: op = Q ? Iop_Cls16x8 : Iop_Cls16x4; break;
                  case 2: op = Q ? Iop_Cls32x4 : Iop_Cls32x2; break;
                  case 3: return False;
                  default: vassert(0);
               }
               assign(res, unop(op, mkexpr(arg_m)));
               DIP("vcls.s%d %c%u, %c%u\n", 8 << size, Q ? 'q' : 'd', dreg,
                   Q ? 'q' : 'd', mreg);
               break;
            }
            case 9: {
               /* VCLZ */
               IROp op;
               switch (size) {
                  case 0: op = Q ? Iop_Clz8x16 : Iop_Clz8x8; break;
                  case 1: op = Q ? Iop_Clz16x8 : Iop_Clz16x4; break;
                  case 2: op = Q ? Iop_Clz32x4 : Iop_Clz32x2; break;
                  case 3: return False;
                  default: vassert(0);
               }
               assign(res, unop(op, mkexpr(arg_m)));
               DIP("vclz.i%d %c%u, %c%u\n", 8 << size, Q ? 'q' : 'd', dreg,
                   Q ? 'q' : 'd', mreg);
               break;
            }
            case 10:
               /* VCNT */
               assign(res, unop(Q ? Iop_Cnt8x16 : Iop_Cnt8x8, mkexpr(arg_m)));
               DIP("vcnt.8 %c%u, %c%u\n", Q ? 'q' : 'd', dreg, Q ? 'q' : 'd',
                   mreg);
               break;
            case 11:
               /* VMVN */
               if (Q)
                  assign(res, unop(Iop_NotV128, mkexpr(arg_m)));
               else
                  assign(res, unop(Iop_Not64, mkexpr(arg_m)));
               DIP("vmvn %c%u, %c%u\n", Q ? 'q' : 'd', dreg, Q ? 'q' : 'd',
                   mreg);
               break;
            case 12:
            case 13: {
               /* VPADAL */
               IROp op, add_op;
               U = (theInstr >> 7) & 1;
               if (Q) {
                  switch (size) {
                     case 0:
                        op = U ? Iop_PwAddL8Ux16 : Iop_PwAddL8Sx16;
                        add_op = Iop_Add16x8;
                        break;
                     case 1:
                        op = U ? Iop_PwAddL16Ux8 : Iop_PwAddL16Sx8;
                        add_op = Iop_Add32x4;
                        break;
                     case 2:
                        op = U ? Iop_PwAddL32Ux4 : Iop_PwAddL32Sx4;
                        add_op = Iop_Add64x2;
                        break;
                     case 3:
                        return False;
                     default:
                        vassert(0);
                  }
               } else {
                  switch (size) {
                     case 0:
                        op = U ? Iop_PwAddL8Ux8 : Iop_PwAddL8Sx8;
                        add_op = Iop_Add16x4;
                        break;
                     case 1:
                        op = U ? Iop_PwAddL16Ux4 : Iop_PwAddL16Sx4;
                        add_op = Iop_Add32x2;
                        break;
                     case 2:
                        op = U ? Iop_PwAddL32Ux2 : Iop_PwAddL32Sx2;
                        add_op = Iop_Add64;
                        break;
                     case 3:
                        return False;
                     default:
                        vassert(0);
                  }
               }
               if (Q) {
                  arg_d = newTemp(Ity_V128);
                  assign(arg_d, getQReg(dreg));
               } else {
                  arg_d = newTemp(Ity_I64);
                  assign(arg_d, getDRegI64(dreg));
               }
               assign(res, binop(add_op, unop(op, mkexpr(arg_m)),
                                         mkexpr(arg_d)));
               DIP("vpadal.%c%d %c%u, %c%u\n", U ? 'u' : 's', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 14: {
               /* VQABS */
               IROp op_sub, op_qsub, op_cmp;
               IRTemp mask, tmp;
               IRExpr *zero1, *zero2;
               IRExpr *neg, *neg2;
               if (Q) {
                  zero1 = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
                  zero2 = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
                  mask = newTemp(Ity_V128);
                  tmp = newTemp(Ity_V128);
               } else {
                  zero1 = mkU64(0);
                  zero2 = mkU64(0);
                  mask = newTemp(Ity_I64);
                  tmp = newTemp(Ity_I64);
               }
               switch (size) {
                  case 0:
                     op_sub = Q ? Iop_Sub8x16 : Iop_Sub8x8;
                     op_qsub = Q ? Iop_QSub8Sx16 : Iop_QSub8Sx8;
                     op_cmp = Q ? Iop_CmpGT8Sx16 : Iop_CmpGT8Sx8;
                     break;
                  case 1:
                     op_sub = Q ? Iop_Sub16x8 : Iop_Sub16x4;
                     op_qsub = Q ? Iop_QSub16Sx8 : Iop_QSub16Sx4;
                     op_cmp = Q ? Iop_CmpGT16Sx8 : Iop_CmpGT16Sx4;
                     break;
                  case 2:
                     op_sub = Q ? Iop_Sub32x4 : Iop_Sub32x2;
                     op_qsub = Q ? Iop_QSub32Sx4 : Iop_QSub32Sx2;
                     op_cmp = Q ? Iop_CmpGT32Sx4 : Iop_CmpGT32Sx2;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
               assign(mask, binop(op_cmp, mkexpr(arg_m), zero1));
               neg = binop(op_qsub, zero2, mkexpr(arg_m));
               neg2 = binop(op_sub, zero2, mkexpr(arg_m));
               assign(res, binop(Q ? Iop_OrV128 : Iop_Or64,
                                 binop(Q ? Iop_AndV128 : Iop_And64,
                                       mkexpr(mask),
                                       mkexpr(arg_m)),
                                 binop(Q ? Iop_AndV128 : Iop_And64,
                                       unop(Q ? Iop_NotV128 : Iop_Not64,
                                            mkexpr(mask)),
                                       neg)));
               assign(tmp, binop(Q ? Iop_OrV128 : Iop_Or64,
                                 binop(Q ? Iop_AndV128 : Iop_And64,
                                       mkexpr(mask),
                                       mkexpr(arg_m)),
                                 binop(Q ? Iop_AndV128 : Iop_And64,
                                       unop(Q ? Iop_NotV128 : Iop_Not64,
                                            mkexpr(mask)),
                                       neg2)));
               setFlag_QC(mkexpr(res), mkexpr(tmp), Q, condT);
               DIP("vqabs.s%d %c%u, %c%u\n", 8 << size, Q ? 'q' : 'd', dreg,
                   Q ? 'q' : 'd', mreg);
               break;
            }
            case 15: {
               /* VQNEG */
               IROp op, op2;
               IRExpr *zero;
               if (Q) {
                  zero = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
               } else {
                  zero = mkU64(0);
               }
               switch (size) {
                  case 0:
                     op = Q ? Iop_QSub8Sx16 : Iop_QSub8Sx8;
                     op2 = Q ? Iop_Sub8x16 : Iop_Sub8x8;
                     break;
                  case 1:
                     op = Q ? Iop_QSub16Sx8 : Iop_QSub16Sx4;
                     op2 = Q ? Iop_Sub16x8 : Iop_Sub16x4;
                     break;
                  case 2:
                     op = Q ? Iop_QSub32Sx4 : Iop_QSub32Sx2;
                     op2 = Q ? Iop_Sub32x4 : Iop_Sub32x2;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
               assign(res, binop(op, zero, mkexpr(arg_m)));
               setFlag_QC(mkexpr(res), binop(op2, zero, mkexpr(arg_m)),
                          Q, condT);
               DIP("vqneg.s%d %c%u, %c%u\n", 8 << size, Q ? 'q' : 'd', dreg,
                   Q ? 'q' : 'd', mreg);
               break;
            }
            default:
               vassert(0);
         }
         if (Q) {
            putQReg(dreg, mkexpr(res), condT);
         } else {
            putDRegI64(dreg, mkexpr(res), condT);
         }
         return True;
      case 1:
         if (Q) {
            arg_m = newTemp(Ity_V128);
            res = newTemp(Ity_V128);
            assign(arg_m, getQReg(mreg));
         } else {
            arg_m = newTemp(Ity_I64);
            res = newTemp(Ity_I64);
            assign(arg_m, getDRegI64(mreg));
         }
         switch ((B >> 1) & 0x7) {
            case 0: {
               /* VCGT #0 */
               IRExpr *zero;
               IROp op;
               if (Q) {
                  zero = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
               } else {
                  zero = mkU64(0);
               }
               if (F) {
                  switch (size) {
                     case 0: case 1: case 3: return False;
                     case 2: op = Q ? Iop_CmpGT32Fx4 : Iop_CmpGT32Fx2; break;
                     default: vassert(0);
                  }
               } else {
                  switch (size) {
                     case 0: op = Q ? Iop_CmpGT8Sx16 : Iop_CmpGT8Sx8; break;
                     case 1: op = Q ? Iop_CmpGT16Sx8 : Iop_CmpGT16Sx4; break;
                     case 2: op = Q ? Iop_CmpGT32Sx4 : Iop_CmpGT32Sx2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
               }
               assign(res, binop(op, mkexpr(arg_m), zero));
               DIP("vcgt.%c%d %c%u, %c%u, #0\n", F ? 'f' : 's', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 1: {
               /* VCGE #0 */
               IROp op;
               IRExpr *zero;
               if (Q) {
                  zero = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
               } else {
                  zero = mkU64(0);
               }
               if (F) {
                  switch (size) {
                     case 0: case 1: case 3: return False;
                     case 2: op = Q ? Iop_CmpGE32Fx4 : Iop_CmpGE32Fx2; break;
                     default: vassert(0);
                  }
                  assign(res, binop(op, mkexpr(arg_m), zero));
               } else {
                  switch (size) {
                     case 0: op = Q ? Iop_CmpGT8Sx16 : Iop_CmpGT8Sx8; break;
                     case 1: op = Q ? Iop_CmpGT16Sx8 : Iop_CmpGT16Sx4; break;
                     case 2: op = Q ? Iop_CmpGT32Sx4 : Iop_CmpGT32Sx2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  assign(res, unop(Q ? Iop_NotV128 : Iop_Not64,
                                   binop(op, zero, mkexpr(arg_m))));
               }
               DIP("vcge.%c%d %c%u, %c%u, #0\n", F ? 'f' : 's', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 2: {
               /* VCEQ #0 */
               IROp op;
               IRExpr *zero;
               if (F) {
                  if (Q) {
                     zero = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
                  } else {
                     zero = mkU64(0);
                  }
                  switch (size) {
                     case 0: case 1: case 3: return False;
                     case 2: op = Q ? Iop_CmpEQ32Fx4 : Iop_CmpEQ32Fx2; break;
                     default: vassert(0);
                  }
                  assign(res, binop(op, zero, mkexpr(arg_m)));
               } else {
                  switch (size) {
                     case 0: op = Q ? Iop_CmpNEZ8x16 : Iop_CmpNEZ8x8; break;
                     case 1: op = Q ? Iop_CmpNEZ16x8 : Iop_CmpNEZ16x4; break;
                     case 2: op = Q ? Iop_CmpNEZ32x4 : Iop_CmpNEZ32x2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  assign(res, unop(Q ? Iop_NotV128 : Iop_Not64,
                                   unop(op, mkexpr(arg_m))));
               }
               DIP("vceq.%c%d %c%u, %c%u, #0\n", F ? 'f' : 'i', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 3: {
               /* VCLE #0 */
               IRExpr *zero;
               IROp op;
               if (Q) {
                  zero = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
               } else {
                  zero = mkU64(0);
               }
               if (F) {
                  switch (size) {
                     case 0: case 1: case 3: return False;
                     case 2: op = Q ? Iop_CmpGE32Fx4 : Iop_CmpGE32Fx2; break;
                     default: vassert(0);
                  }
                  assign(res, binop(op, zero, mkexpr(arg_m)));
               } else {
                  switch (size) {
                     case 0: op = Q ? Iop_CmpGT8Sx16 : Iop_CmpGT8Sx8; break;
                     case 1: op = Q ? Iop_CmpGT16Sx8 : Iop_CmpGT16Sx4; break;
                     case 2: op = Q ? Iop_CmpGT32Sx4 : Iop_CmpGT32Sx2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  assign(res, unop(Q ? Iop_NotV128 : Iop_Not64,
                                   binop(op, mkexpr(arg_m), zero)));
               }
               DIP("vcle.%c%d %c%u, %c%u, #0\n", F ? 'f' : 's', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 4: {
               /* VCLT #0 */
               IROp op;
               IRExpr *zero;
               if (Q) {
                  zero = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
               } else {
                  zero = mkU64(0);
               }
               if (F) {
                  switch (size) {
                     case 0: case 1: case 3: return False;
                     case 2: op = Q ? Iop_CmpGT32Fx4 : Iop_CmpGT32Fx2; break;
                     default: vassert(0);
                  }
                  assign(res, binop(op, zero, mkexpr(arg_m)));
               } else {
                  switch (size) {
                     case 0: op = Q ? Iop_CmpGT8Sx16 : Iop_CmpGT8Sx8; break;
                     case 1: op = Q ? Iop_CmpGT16Sx8 : Iop_CmpGT16Sx4; break;
                     case 2: op = Q ? Iop_CmpGT32Sx4 : Iop_CmpGT32Sx2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  assign(res, binop(op, zero, mkexpr(arg_m)));
               }
               DIP("vclt.%c%d %c%u, %c%u, #0\n", F ? 'f' : 's', 8 << size,
                   Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
               break;
            }
            case 5:
               return False;
            case 6: {
               /* VABS */
               if (!F) {
                  IROp op;
                  switch(size) {
                     case 0: op = Q ? Iop_Abs8x16 : Iop_Abs8x8; break;
                     case 1: op = Q ? Iop_Abs16x8 : Iop_Abs16x4; break;
                     case 2: op = Q ? Iop_Abs32x4 : Iop_Abs32x2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  assign(res, unop(op, mkexpr(arg_m)));
               } else {
                  assign(res, unop(Q ? Iop_Abs32Fx4 : Iop_Abs32Fx2,
                                   mkexpr(arg_m)));
               }
               DIP("vabs.%c%d %c%u, %c%u\n",
                   F ? 'f' : 's', 8 << size, Q ? 'q' : 'd', dreg,
                   Q ? 'q' : 'd', mreg);
               break;
            }
            case 7: {
               /* VNEG */
               IROp op;
               IRExpr *zero;
               if (F) {
                  switch (size) {
                     case 0: case 1: case 3: return False;
                     case 2: op = Q ? Iop_Neg32Fx4 : Iop_Neg32Fx2; break;
                     default: vassert(0);
                  }
                  assign(res, unop(op, mkexpr(arg_m)));
               } else {
                  if (Q) {
                     zero = binop(Iop_64HLtoV128, mkU64(0), mkU64(0));
                  } else {
                     zero = mkU64(0);
                  }
                  switch (size) {
                     case 0: op = Q ? Iop_Sub8x16 : Iop_Sub8x8; break;
                     case 1: op = Q ? Iop_Sub16x8 : Iop_Sub16x4; break;
                     case 2: op = Q ? Iop_Sub32x4 : Iop_Sub32x2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  assign(res, binop(op, zero, mkexpr(arg_m)));
               }
               DIP("vneg.%c%d %c%u, %c%u\n",
                   F ? 'f' : 's', 8 << size, Q ? 'q' : 'd', dreg,
                   Q ? 'q' : 'd', mreg);
               break;
            }
            default:
               vassert(0);
         }
         if (Q) {
            putQReg(dreg, mkexpr(res), condT);
         } else {
            putDRegI64(dreg, mkexpr(res), condT);
         }
         return True;
      case 2:
         if ((B >> 1) == 0) {
            /* VSWP */
            if (Q) {
               arg_m = newTemp(Ity_V128);
               assign(arg_m, getQReg(mreg));
               putQReg(mreg, getQReg(dreg), condT);
               putQReg(dreg, mkexpr(arg_m), condT);
            } else {
               arg_m = newTemp(Ity_I64);
               assign(arg_m, getDRegI64(mreg));
               putDRegI64(mreg, getDRegI64(dreg), condT);
               putDRegI64(dreg, mkexpr(arg_m), condT);
            }
            DIP("vswp %c%u, %c%u\n",
                Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
            return True;
         } else if ((B >> 1) == 1) {
            /* VTRN */
            IROp op_odd = Iop_INVALID, op_even = Iop_INVALID;
            IRTemp old_m, old_d, new_d, new_m;
            if (Q) {
               old_m = newTemp(Ity_V128);
               old_d = newTemp(Ity_V128);
               new_m = newTemp(Ity_V128);
               new_d = newTemp(Ity_V128);
               assign(old_m, getQReg(mreg));
               assign(old_d, getQReg(dreg));
            } else {
               old_m = newTemp(Ity_I64);
               old_d = newTemp(Ity_I64);
               new_m = newTemp(Ity_I64);
               new_d = newTemp(Ity_I64);
               assign(old_m, getDRegI64(mreg));
               assign(old_d, getDRegI64(dreg));
            }
            if (Q) {
               switch (size) {
                  case 0:
                     op_odd  = Iop_InterleaveOddLanes8x16;
                     op_even = Iop_InterleaveEvenLanes8x16;
                     break;
                  case 1:
                     op_odd  = Iop_InterleaveOddLanes16x8;
                     op_even = Iop_InterleaveEvenLanes16x8;
                     break;
                  case 2:
                     op_odd  = Iop_InterleaveOddLanes32x4;
                     op_even = Iop_InterleaveEvenLanes32x4;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            } else {
               switch (size) {
                  case 0:
                     op_odd  = Iop_InterleaveOddLanes8x8;
                     op_even = Iop_InterleaveEvenLanes8x8;
                     break;
                  case 1:
                     op_odd  = Iop_InterleaveOddLanes16x4;
                     op_even = Iop_InterleaveEvenLanes16x4;
                     break;
                  case 2:
                     op_odd  = Iop_InterleaveHI32x2;
                     op_even = Iop_InterleaveLO32x2;
                     break;
                  case 3:
                     return False;
                  default:
                     vassert(0);
               }
            }
            assign(new_d, binop(op_even, mkexpr(old_m), mkexpr(old_d)));
            assign(new_m, binop(op_odd, mkexpr(old_m), mkexpr(old_d)));
            if (Q) {
               putQReg(dreg, mkexpr(new_d), condT);
               putQReg(mreg, mkexpr(new_m), condT);
            } else {
               putDRegI64(dreg, mkexpr(new_d), condT);
               putDRegI64(mreg, mkexpr(new_m), condT);
            }
            DIP("vtrn.%d %c%u, %c%u\n",
                8 << size, Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
            return True;
         } else if ((B >> 1) == 2) {
            /* VUZP */
            IROp op_even, op_odd;
            IRTemp old_m, old_d, new_m, new_d;
            if (!Q && size == 2)
               return False;
            if (Q) {
               old_m = newTemp(Ity_V128);
               old_d = newTemp(Ity_V128);
               new_m = newTemp(Ity_V128);
               new_d = newTemp(Ity_V128);
               assign(old_m, getQReg(mreg));
               assign(old_d, getQReg(dreg));
            } else {
               old_m = newTemp(Ity_I64);
               old_d = newTemp(Ity_I64);
               new_m = newTemp(Ity_I64);
               new_d = newTemp(Ity_I64);
               assign(old_m, getDRegI64(mreg));
               assign(old_d, getDRegI64(dreg));
            }
            switch (size) {
               case 0:
                  op_odd  = Q ? Iop_CatOddLanes8x16 : Iop_CatOddLanes8x8;
                  op_even = Q ? Iop_CatEvenLanes8x16 : Iop_CatEvenLanes8x8;
                  break;
               case 1:
                  op_odd  = Q ? Iop_CatOddLanes16x8 : Iop_CatOddLanes16x4;
                  op_even = Q ? Iop_CatEvenLanes16x8 : Iop_CatEvenLanes16x4;
                  break;
               case 2:
                  op_odd  = Iop_CatOddLanes32x4;
                  op_even = Iop_CatEvenLanes32x4;
                  break;
               case 3:
                  return False;
               default:
                  vassert(0);
            }
            assign(new_d, binop(op_even, mkexpr(old_m), mkexpr(old_d)));
            assign(new_m, binop(op_odd,  mkexpr(old_m), mkexpr(old_d)));
            if (Q) {
               putQReg(dreg, mkexpr(new_d), condT);
               putQReg(mreg, mkexpr(new_m), condT);
            } else {
               putDRegI64(dreg, mkexpr(new_d), condT);
               putDRegI64(mreg, mkexpr(new_m), condT);
            }
            DIP("vuzp.%d %c%u, %c%u\n",
                8 << size, Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
            return True;
         } else if ((B >> 1) == 3) {
            /* VZIP */
            IROp op_lo, op_hi;
            IRTemp old_m, old_d, new_m, new_d;
            if (!Q && size == 2)
               return False;
            if (Q) {
               old_m = newTemp(Ity_V128);
               old_d = newTemp(Ity_V128);
               new_m = newTemp(Ity_V128);
               new_d = newTemp(Ity_V128);
               assign(old_m, getQReg(mreg));
               assign(old_d, getQReg(dreg));
            } else {
               old_m = newTemp(Ity_I64);
               old_d = newTemp(Ity_I64);
               new_m = newTemp(Ity_I64);
               new_d = newTemp(Ity_I64);
               assign(old_m, getDRegI64(mreg));
               assign(old_d, getDRegI64(dreg));
            }
            switch (size) {
               case 0:
                  op_hi = Q ? Iop_InterleaveHI8x16 : Iop_InterleaveHI8x8;
                  op_lo = Q ? Iop_InterleaveLO8x16 : Iop_InterleaveLO8x8;
                  break;
               case 1:
                  op_hi = Q ? Iop_InterleaveHI16x8 : Iop_InterleaveHI16x4;
                  op_lo = Q ? Iop_InterleaveLO16x8 : Iop_InterleaveLO16x4;
                  break;
               case 2:
                  op_hi = Iop_InterleaveHI32x4;
                  op_lo = Iop_InterleaveLO32x4;
                  break;
               case 3:
                  return False;
               default:
                  vassert(0);
            }
            assign(new_d, binop(op_lo, mkexpr(old_m), mkexpr(old_d)));
            assign(new_m, binop(op_hi, mkexpr(old_m), mkexpr(old_d)));
            if (Q) {
               putQReg(dreg, mkexpr(new_d), condT);
               putQReg(mreg, mkexpr(new_m), condT);
            } else {
               putDRegI64(dreg, mkexpr(new_d), condT);
               putDRegI64(mreg, mkexpr(new_m), condT);
            }
            DIP("vzip.%d %c%u, %c%u\n",
                8 << size, Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
            return True;
         } else if (B == 8) {
            /* VMOVN */
            IROp op;
            mreg >>= 1;
            switch (size) {
               case 0: op = Iop_NarrowUn16to8x8;  break;
               case 1: op = Iop_NarrowUn32to16x4; break;
               case 2: op = Iop_NarrowUn64to32x2; break;
               case 3: return False;
               default: vassert(0);
            }
            putDRegI64(dreg, unop(op, getQReg(mreg)), condT);
            DIP("vmovn.i%d d%u, q%u\n", 16 << size, dreg, mreg);
            return True;
         } else if (B == 9 || (B >> 1) == 5) {
            /* VQMOVN, VQMOVUN */
            IROp op, op2;
            IRTemp tmp;
            dreg = ((theInstr >> 18) & 0x10) | ((theInstr >> 12) & 0xF);
            mreg = ((theInstr >> 1) & 0x10) | (theInstr & 0xF);
            if (mreg & 1)
               return False;
            mreg >>= 1;
            switch (size) {
               case 0: op2 = Iop_NarrowUn16to8x8;  break;
               case 1: op2 = Iop_NarrowUn32to16x4; break;
               case 2: op2 = Iop_NarrowUn64to32x2; break;
               case 3: return False;
               default: vassert(0);
            }
            switch (B & 3) {
               case 0:
                  vassert(0);
               case 1:
                  switch (size) {
                     case 0: op = Iop_QNarrowUn16Sto8Ux8;  break;
                     case 1: op = Iop_QNarrowUn32Sto16Ux4; break;
                     case 2: op = Iop_QNarrowUn64Sto32Ux2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  DIP("vqmovun.s%d d%u, q%u\n", 16 << size, dreg, mreg);
                  break;
               case 2:
                  switch (size) {
                     case 0: op = Iop_QNarrowUn16Sto8Sx8;  break;
                     case 1: op = Iop_QNarrowUn32Sto16Sx4; break;
                     case 2: op = Iop_QNarrowUn64Sto32Sx2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  DIP("vqmovn.s%d d%u, q%u\n", 16 << size, dreg, mreg);
                  break;
               case 3:
                  switch (size) {
                     case 0: op = Iop_QNarrowUn16Uto8Ux8;  break;
                     case 1: op = Iop_QNarrowUn32Uto16Ux4; break;
                     case 2: op = Iop_QNarrowUn64Uto32Ux2; break;
                     case 3: return False;
                     default: vassert(0);
                  }
                  DIP("vqmovn.u%d d%u, q%u\n", 16 << size, dreg, mreg);
                  break;
               default:
                  vassert(0);
            }
            res = newTemp(Ity_I64);
            tmp = newTemp(Ity_I64);
            assign(res, unop(op, getQReg(mreg)));
            assign(tmp, unop(op2, getQReg(mreg)));
            setFlag_QC(mkexpr(res), mkexpr(tmp), False, condT);
            putDRegI64(dreg, mkexpr(res), condT);
            return True;
         } else if (B == 12) {
            /* VSHLL (maximum shift) */
            IROp op, cvt;
            UInt shift_imm;
            if (Q)
               return False;
            if (dreg & 1)
               return False;
            dreg >>= 1;
            shift_imm = 8 << size;
            res = newTemp(Ity_V128);
            switch (size) {
               case 0: op = Iop_ShlN16x8; cvt = Iop_Widen8Uto16x8;  break;
               case 1: op = Iop_ShlN32x4; cvt = Iop_Widen16Uto32x4; break;
               case 2: op = Iop_ShlN64x2; cvt = Iop_Widen32Uto64x2; break;
               case 3: return False;
               default: vassert(0);
            }
            assign(res, binop(op, unop(cvt, getDRegI64(mreg)),
                                  mkU8(shift_imm)));
            putQReg(dreg, mkexpr(res), condT);
            DIP("vshll.i%d q%u, d%u, #%d\n", 8 << size, dreg, mreg, 8 << size);
            return True;
         } else if ((B >> 3) == 3 && (B & 3) == 0) {
            /* VCVT (half<->single) */
            /* Half-precision extensions are needed to run this */
            vassert(0); // ATC
            if (((theInstr >> 18) & 3) != 1)
               return False;
            if ((theInstr >> 8) & 1) {
               if (dreg & 1)
                  return False;
               dreg >>= 1;
               putQReg(dreg, unop(Iop_F16toF32x4, getDRegI64(mreg)),
                     condT);
               DIP("vcvt.f32.f16 q%u, d%u\n", dreg, mreg);
            } else {
               if (mreg & 1)
                  return False;
               mreg >>= 1;
               putDRegI64(dreg, unop(Iop_F32toF16x4, getQReg(mreg)),
                                condT);
               DIP("vcvt.f16.f32 d%u, q%u\n", dreg, mreg);
            }
            return True;
         } else {
            return False;
         }
         vassert(0);
         return True;
      case 3:
         if (((B >> 1) & BITS4(1,1,0,1)) == BITS4(1,0,0,0)) {
            /* VRECPE */
            IROp op;
            F = (theInstr >> 8) & 1;
            if (size != 2)
               return False;
            if (Q) {
               op = F ? Iop_RecipEst32Fx4 : Iop_RecipEst32Ux4;
               putQReg(dreg, unop(op, getQReg(mreg)), condT);
               DIP("vrecpe.%c32 q%u, q%u\n", F ? 'f' : 'u', dreg, mreg);
            } else {
               op = F ? Iop_RecipEst32Fx2 : Iop_RecipEst32Ux2;
               putDRegI64(dreg, unop(op, getDRegI64(mreg)), condT);
               DIP("vrecpe.%c32 d%u, d%u\n", F ? 'f' : 'u', dreg, mreg);
            }
            return True;
         } else if (((B >> 1) & BITS4(1,1,0,1)) == BITS4(1,0,0,1)) {
            /* VRSQRTE */
            IROp op;
            F = (B >> 2) & 1;
            if (size != 2)
               return False;
            if (F) {
               /* fp */
               op = Q ? Iop_RSqrtEst32Fx4 : Iop_RSqrtEst32Fx2;
            } else {
               /* unsigned int */
               op = Q ? Iop_RSqrtEst32Ux4 : Iop_RSqrtEst32Ux2;
            }
            if (Q) {
               putQReg(dreg, unop(op, getQReg(mreg)), condT);
               DIP("vrsqrte.%c32 q%u, q%u\n", F ? 'f' : 'u', dreg, mreg);
            } else {
               putDRegI64(dreg, unop(op, getDRegI64(mreg)), condT);
               DIP("vrsqrte.%c32 d%u, d%u\n", F ? 'f' : 'u', dreg, mreg);
            }
            return True;
         } else if ((B >> 3) == 3) {
            /* VCVT (fp<->integer) */
            IROp op;
            if (size != 2)
               return False;
            switch ((B >> 1) & 3) {
               case 0:
                  op = Q ? Iop_I32StoFx4 : Iop_I32StoFx2;
                  DIP("vcvt.f32.s32 %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
                  break;
               case 1:
                  op = Q ? Iop_I32UtoFx4 : Iop_I32UtoFx2;
                  DIP("vcvt.f32.u32 %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
                  break;
               case 2:
                  op = Q ? Iop_FtoI32Sx4_RZ : Iop_FtoI32Sx2_RZ;
                  DIP("vcvt.s32.f32 %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
                  break;
               case 3:
                  op = Q ? Iop_FtoI32Ux4_RZ : Iop_FtoI32Ux2_RZ;
                  DIP("vcvt.u32.f32 %c%u, %c%u\n",
                      Q ? 'q' : 'd', dreg, Q ? 'q' : 'd', mreg);
                  break;
               default:
                  vassert(0);
            }
            if (Q) {
               putQReg(dreg, unop(op, getQReg(mreg)), condT);
            } else {
               putDRegI64(dreg, unop(op, getDRegI64(mreg)), condT);
            }
            return True;
         } else {
            return False;
         }
         vassert(0);
         return True;
      default:
         vassert(0);
   }
   return False;
}

/* A7.4.6 One register and a modified immediate value */
static
void ppNeonImm(UInt imm, UInt cmode, UInt op)
{
   int i;
   switch (cmode) {
      case 0: case 1: case 8: case 9:
         vex_printf("0x%x", imm);
         break;
      case 2: case 3: case 10: case 11:
         vex_printf("0x%x00", imm);
         break;
      case 4: case 5:
         vex_printf("0x%x0000", imm);
         break;
      case 6: case 7:
         vex_printf("0x%x000000", imm);
         break;
      case 12:
         vex_printf("0x%xff", imm);
         break;
      case 13:
         vex_printf("0x%xffff", imm);
         break;
      case 14:
         if (op) {
            vex_printf("0x");
            for (i = 7; i >= 0; i--)
               vex_printf("%s", (imm & (1 << i)) ? "ff" : "00");
         } else {
            vex_printf("0x%x", imm);
         }
         break;
      case 15:
         vex_printf("0x%x", imm);
         break;
   }
}

static
const char *ppNeonImmType(UInt cmode, UInt op)
{
   switch (cmode) {
      case 0 ... 7:
      case 12: case 13:
         return "i32";
      case 8 ... 11:
         return "i16";
      case 14:
         if (op)
            return "i64";
         else
            return "i8";
      case 15:
         if (op)
            vassert(0);
         else
            return "f32";
      default:
         vassert(0);
   }
}

static
void DIPimm(UInt imm, UInt cmode, UInt op,
            const char *instr, UInt Q, UInt dreg)
{
   if (vex_traceflags & VEX_TRACE_FE) {
      vex_printf("%s.%s %c%u, #", instr,
                 ppNeonImmType(cmode, op), Q ? 'q' : 'd', dreg);
      ppNeonImm(imm, cmode, op);
      vex_printf("\n");
   }
}

static
Bool dis_neon_data_1reg_and_imm ( UInt theInstr, IRTemp condT )
{
   UInt dreg = get_neon_d_regno(theInstr);
   ULong imm_raw = ((theInstr >> 17) & 0x80) | ((theInstr >> 12) & 0x70) |
                  (theInstr & 0xf);
   ULong imm_raw_pp = imm_raw;
   UInt cmode = (theInstr >> 8) & 0xf;
   UInt op_bit = (theInstr >> 5) & 1;
   ULong imm = 0;
   UInt Q = (theInstr >> 6) & 1;
   int i, j;
   UInt tmp;
   IRExpr *imm_val;
   IRExpr *expr;
   IRTemp tmp_var;
   switch(cmode) {
      case 7: case 6:
         imm_raw = imm_raw << 8;
         /* fallthrough */
      case 5: case 4:
         imm_raw = imm_raw << 8;
         /* fallthrough */
      case 3: case 2:
         imm_raw = imm_raw << 8;
         /* fallthrough */
      case 0: case 1:
         imm = (imm_raw << 32) | imm_raw;
         break;
      case 11: case 10:
         imm_raw = imm_raw << 8;
         /* fallthrough */
      case 9: case 8:
         imm_raw = (imm_raw << 16) | imm_raw;
         imm = (imm_raw << 32) | imm_raw;
         break;
      case 13:
         imm_raw = (imm_raw << 8) | 0xff;
         /* fallthrough */
      case 12:
         imm_raw = (imm_raw << 8) | 0xff;
         imm = (imm_raw << 32) | imm_raw;
         break;
      case 14:
         if (! op_bit) {
            for(i = 0; i < 8; i++) {
               imm = (imm << 8) | imm_raw;
            }
         } else {
            for(i = 7; i >= 0; i--) {
               tmp = 0;
               for(j = 0; j < 8; j++) {
                  tmp = (tmp << 1) | ((imm_raw >> i) & 1);
               }
               imm = (imm << 8) | tmp;
            }
         }
         break;
      case 15:
         imm = (imm_raw & 0x80) << 5;
         imm |= ((~imm_raw & 0x40) << 5);
         for(i = 1; i <= 4; i++)
            imm |= (imm_raw & 0x40) << i;
         imm |= (imm_raw & 0x7f);
         imm = imm << 19;
         imm = (imm << 32) | imm;
         break;
      default:
         return False;
   }
   if (Q) {
      imm_val = binop(Iop_64HLtoV128, mkU64(imm), mkU64(imm));
   } else {
      imm_val = mkU64(imm);
   }
   if (((op_bit == 0) &&
      (((cmode & 9) == 0) || ((cmode & 13) == 8) || ((cmode & 12) == 12))) ||
      ((op_bit == 1) && (cmode == 14))) {
      /* VMOV (immediate) */
      if (Q) {
         putQReg(dreg, imm_val, condT);
      } else {
         putDRegI64(dreg, imm_val, condT);
      }
      DIPimm(imm_raw_pp, cmode, op_bit, "vmov", Q, dreg);
      return True;
   }
   if ((op_bit == 1) &&
      (((cmode & 9) == 0) || ((cmode & 13) == 8) || ((cmode & 14) == 12))) {
      /* VMVN (immediate) */
      if (Q) {
         putQReg(dreg, unop(Iop_NotV128, imm_val), condT);
      } else {
         putDRegI64(dreg, unop(Iop_Not64, imm_val), condT);
      }
      DIPimm(imm_raw_pp, cmode, op_bit, "vmvn", Q, dreg);
      return True;
   }
   if (Q) {
      tmp_var = newTemp(Ity_V128);
      assign(tmp_var, getQReg(dreg));
   } else {
      tmp_var = newTemp(Ity_I64);
      assign(tmp_var, getDRegI64(dreg));
   }
   if ((op_bit == 0) && (((cmode & 9) == 1) || ((cmode & 13) == 9))) {
      /* VORR (immediate) */
      if (Q)
         expr = binop(Iop_OrV128, mkexpr(tmp_var), imm_val);
      else
         expr = binop(Iop_Or64, mkexpr(tmp_var), imm_val);
      DIPimm(imm_raw_pp, cmode, op_bit, "vorr", Q, dreg);
   } else if ((op_bit == 1) && (((cmode & 9) == 1) || ((cmode & 13) == 9))) {
      /* VBIC (immediate) */
      if (Q)
         expr = binop(Iop_AndV128, mkexpr(tmp_var),
                                   unop(Iop_NotV128, imm_val));
      else
         expr = binop(Iop_And64, mkexpr(tmp_var), unop(Iop_Not64, imm_val));
      DIPimm(imm_raw_pp, cmode, op_bit, "vbic", Q, dreg);
   } else {
      return False;
   }
   if (Q)
      putQReg(dreg, expr, condT);
   else
      putDRegI64(dreg, expr, condT);
   return True;
}

/* A7.4 Advanced SIMD data-processing instructions */
static
Bool dis_neon_data_processing ( UInt theInstr, IRTemp condT )
{
   UInt A = (theInstr >> 19) & 0x1F;
   UInt B = (theInstr >>  8) & 0xF;
   UInt C = (theInstr >>  4) & 0xF;
   UInt U = (theInstr >> 24) & 0x1;

   if (! (A & 0x10)) {
      return dis_neon_data_3same(theInstr, condT);
   }
   if (((A & 0x17) == 0x10) && ((C & 0x9) == 0x1)) {
      return dis_neon_data_1reg_and_imm(theInstr, condT);
   }
   if ((C & 1) == 1) {
      return dis_neon_data_2reg_and_shift(theInstr, condT);
   }
   if (((C & 5) == 0) && (((A & 0x14) == 0x10) || ((A & 0x16) == 0x14))) {
      return dis_neon_data_3diff(theInstr, condT);
   }
   if (((C & 5) == 4) && (((A & 0x14) == 0x10) || ((A & 0x16) == 0x14))) {
      return dis_neon_data_2reg_and_scalar(theInstr, condT);
   }
   if ((A & 0x16) == 0x16) {
      if ((U == 0) && ((C & 1) == 0)) {
         return dis_neon_vext(theInstr, condT);
      }
      if ((U != 1) || ((C & 1) == 1))
         return False;
      if ((B & 8) == 0) {
         return dis_neon_data_2reg_misc(theInstr, condT);
      }
      if ((B & 12) == 8) {
         return dis_neon_vtb(theInstr, condT);
      }
      if ((B == 12) && ((C & 9) == 0)) {
         return dis_neon_vdup(theInstr, condT);
      }
   }
   return False;
}


/*------------------------------------------------------------*/
/*--- NEON loads and stores                                ---*/
/*------------------------------------------------------------*/

/* For NEON memory operations, we use the standard scheme to handle
   conditionalisation: generate a jump around the instruction if the
   condition is false.  That's only necessary in Thumb mode, however,
   since in ARM mode NEON instructions are unconditional. */

/* A helper function for what follows.  It assumes we already went
   uncond as per comments at the top of this section. */
static
void mk_neon_elem_load_to_one_lane( UInt rD, UInt inc, UInt index,
                                    UInt N, UInt size, IRTemp addr )
{
   UInt i;
   switch (size) {
      case 0:
         putDRegI64(rD, triop(Iop_SetElem8x8, getDRegI64(rD), mkU8(index),
                    loadLE(Ity_I8, mkexpr(addr))), IRTemp_INVALID);
         break;
      case 1:
         putDRegI64(rD, triop(Iop_SetElem16x4, getDRegI64(rD), mkU8(index),
                    loadLE(Ity_I16, mkexpr(addr))), IRTemp_INVALID);
         break;
      case 2:
         putDRegI64(rD, triop(Iop_SetElem32x2, getDRegI64(rD), mkU8(index),
                    loadLE(Ity_I32, mkexpr(addr))), IRTemp_INVALID);
         break;
      default:
         vassert(0);
   }
   for (i = 1; i <= N; i++) {
      switch (size) {
         case 0:
            putDRegI64(rD + i * inc,
                       triop(Iop_SetElem8x8,
                             getDRegI64(rD + i * inc),
                             mkU8(index),
                             loadLE(Ity_I8, binop(Iop_Add32,
                                                  mkexpr(addr),
                                                  mkU32(i * 1)))),
                       IRTemp_INVALID);
            break;
         case 1:
            putDRegI64(rD + i * inc,
                       triop(Iop_SetElem16x4,
                             getDRegI64(rD + i * inc),
                             mkU8(index),
                             loadLE(Ity_I16, binop(Iop_Add32,
                                                   mkexpr(addr),
                                                   mkU32(i * 2)))),
                       IRTemp_INVALID);
            break;
         case 2:
            putDRegI64(rD + i * inc,
                       triop(Iop_SetElem32x2,
                             getDRegI64(rD + i * inc),
                             mkU8(index),
                             loadLE(Ity_I32, binop(Iop_Add32,
                                                   mkexpr(addr),
                                                   mkU32(i * 4)))),
                       IRTemp_INVALID);
            break;
         default:
            vassert(0);
      }
   }
}

/* A(nother) helper function for what follows.  It assumes we already
   went uncond as per comments at the top of this section. */
static
void mk_neon_elem_store_from_one_lane( UInt rD, UInt inc, UInt index,
                                       UInt N, UInt size, IRTemp addr )
{
   UInt i;
   switch (size) {
      case 0:
         storeLE(mkexpr(addr),
                 binop(Iop_GetElem8x8, getDRegI64(rD), mkU8(index)));
         break;
      case 1:
         storeLE(mkexpr(addr),
                 binop(Iop_GetElem16x4, getDRegI64(rD), mkU8(index)));
         break;
      case 2:
         storeLE(mkexpr(addr),
                 binop(Iop_GetElem32x2, getDRegI64(rD), mkU8(index)));
         break;
      default:
         vassert(0);
   }
   for (i = 1; i <= N; i++) {
      switch (size) {
         case 0:
            storeLE(binop(Iop_Add32, mkexpr(addr), mkU32(i * 1)),
                    binop(Iop_GetElem8x8, getDRegI64(rD + i * inc),
                                          mkU8(index)));
            break;
         case 1:
            storeLE(binop(Iop_Add32, mkexpr(addr), mkU32(i * 2)),
                    binop(Iop_GetElem16x4, getDRegI64(rD + i * inc),
                                           mkU8(index)));
            break;
         case 2:
            storeLE(binop(Iop_Add32, mkexpr(addr), mkU32(i * 4)),
                    binop(Iop_GetElem32x2, getDRegI64(rD + i * inc),
                                           mkU8(index)));
            break;
         default:
            vassert(0);
      }
   }
}

/* Generate 2x64 -> 2x64 deinterleave code, for VLD2.  Caller must
   make *u0 and *u1 be valid IRTemps before the call. */
static void math_DEINTERLEAVE_2 (/*OUT*/IRTemp* u0, /*OUT*/IRTemp* u1,
                                 IRTemp i0, IRTemp i1, Int laneszB)
{
   /* The following assumes that the guest is little endian, and hence
      that the memory-side (interleaved) data is stored
      little-endianly. */
   vassert(u0 && u1);
   /* This is pretty easy, since we have primitives directly to
      hand. */
   if (laneszB == 4) {
      // memLE(128 bits) == A0 B0 A1 B1
      // i0 == B0 A0, i1 == B1 A1
      // u0 == A1 A0, u1 == B1 B0
      assign(*u0, binop(Iop_InterleaveLO32x2, mkexpr(i1), mkexpr(i0)));
      assign(*u1, binop(Iop_InterleaveHI32x2, mkexpr(i1), mkexpr(i0)));
   } else if (laneszB == 2) {
      // memLE(128 bits) == A0 B0 A1 B1 A2 B2 A3 B3
      // i0 == B1 A1 B0 A0, i1 == B3 A3 B2 A2
      // u0 == A3 A2 A1 A0, u1 == B3 B2 B1 B0
      assign(*u0, binop(Iop_CatEvenLanes16x4, mkexpr(i1), mkexpr(i0)));
      assign(*u1, binop(Iop_CatOddLanes16x4,  mkexpr(i1), mkexpr(i0)));
   } else if (laneszB == 1) {
      // memLE(128 bits) == A0 B0 A1 B1 A2 B2 A3 B3 A4 B4 A5 B5 A6 B6 A7 B7
      // i0 == B3 A3 B2 A2 B1 A1 B0 A0, i1 == B7 A7 B6 A6 B5 A5 B4 A4
      // u0 == A7 A6 A5 A4 A3 A2 A1 A0, u1 == B7 B6 B5 B4 B3 B2 B1 B0
      assign(*u0, binop(Iop_CatEvenLanes8x8, mkexpr(i1), mkexpr(i0)));
      assign(*u1, binop(Iop_CatOddLanes8x8,  mkexpr(i1), mkexpr(i0)));
   } else {
      // Can never happen, since VLD2 only has valid lane widths of 32,
      // 16 or 8 bits.
      vpanic("math_DEINTERLEAVE_2");
   }
}

/* Generate 2x64 -> 2x64 interleave code, for VST2.  Caller must make
   *u0 and *u1 be valid IRTemps before the call. */
static void math_INTERLEAVE_2 (/*OUT*/IRTemp* i0, /*OUT*/IRTemp* i1,
                               IRTemp u0, IRTemp u1, Int laneszB)
{
   /* The following assumes that the guest is little endian, and hence
      that the memory-side (interleaved) data is stored
      little-endianly. */
   vassert(i0 && i1);
   /* This is pretty easy, since we have primitives directly to
      hand. */
   if (laneszB == 4) {
      // memLE(128 bits) == A0 B0 A1 B1
      // i0 == B0 A0, i1 == B1 A1
      // u0 == A1 A0, u1 == B1 B0
      assign(*i0, binop(Iop_InterleaveLO32x2, mkexpr(u1), mkexpr(u0)));
      assign(*i1, binop(Iop_InterleaveHI32x2, mkexpr(u1), mkexpr(u0)));
   } else if (laneszB == 2) {
      // memLE(128 bits) == A0 B0 A1 B1 A2 B2 A3 B3
      // i0 == B1 A1 B0 A0, i1 == B3 A3 B2 A2
      // u0 == A3 A2 A1 A0, u1 == B3 B2 B1 B0
      assign(*i0, binop(Iop_InterleaveLO16x4, mkexpr(u1), mkexpr(u0)));
      assign(*i1, binop(Iop_InterleaveHI16x4, mkexpr(u1), mkexpr(u0)));
   } else if (laneszB == 1) {
      // memLE(128 bits) == A0 B0 A1 B1 A2 B2 A3 B3 A4 B4 A5 B5 A6 B6 A7 B7
      // i0 == B3 A3 B2 A2 B1 A1 B0 A0, i1 == B7 A7 B6 A6 B5 A5 B4 A4
      // u0 == A7 A6 A5 A4 A3 A2 A1 A0, u1 == B7 B6 B5 B4 B3 B2 B1 B0
      assign(*i0, binop(Iop_InterleaveLO8x8, mkexpr(u1), mkexpr(u0)));
      assign(*i1, binop(Iop_InterleaveHI8x8, mkexpr(u1), mkexpr(u0)));
   } else {
      // Can never happen, since VST2 only has valid lane widths of 32,
      // 16 or 8 bits.
      vpanic("math_INTERLEAVE_2");
   }
}

// Helper function for generating arbitrary slicing 'n' dicing of
// 3 8x8 vectors, as needed for VLD3.8 and VST3.8.
static IRExpr* math_PERM_8x8x3(const UChar* desc,
                               IRTemp s0, IRTemp s1, IRTemp s2)
{
   // desc is an array of 8 pairs, encoded as 16 bytes,
   // that describe how to assemble the result lanes, starting with
   // lane 7.  Each pair is: first component (0..2) says which of 
   // s0/s1/s2 to use.  Second component (0..7) is the lane number
   // in the source to use.
   UInt si;
   for (si = 0; si < 7; si++) {
      vassert(desc[2 * si + 0] <= 2);
      vassert(desc[2 * si + 1] <= 7);
   }
   IRTemp h3 = newTemp(Ity_I64);
   IRTemp h2 = newTemp(Ity_I64);
   IRTemp h1 = newTemp(Ity_I64);
   IRTemp h0 = newTemp(Ity_I64);
   IRTemp srcs[3] = {s0, s1, s2};
#  define SRC_VEC(_lane)   mkexpr(srcs[desc[2 * (7-(_lane)) + 0]])
#  define SRC_SHIFT(_lane) mkU8(56-8*(desc[2 * (7-(_lane)) + 1]))
   assign(h3, binop(Iop_InterleaveHI8x8,
                    binop(Iop_Shl64, SRC_VEC(7), SRC_SHIFT(7)),
                    binop(Iop_Shl64, SRC_VEC(6), SRC_SHIFT(6))));
   assign(h2, binop(Iop_InterleaveHI8x8,
                    binop(Iop_Shl64, SRC_VEC(5), SRC_SHIFT(5)),
                    binop(Iop_Shl64, SRC_VEC(4), SRC_SHIFT(4))));
   assign(h1, binop(Iop_InterleaveHI8x8,
                    binop(Iop_Shl64, SRC_VEC(3), SRC_SHIFT(3)),
                    binop(Iop_Shl64, SRC_VEC(2), SRC_SHIFT(2))));
   assign(h0, binop(Iop_InterleaveHI8x8,
                    binop(Iop_Shl64, SRC_VEC(1), SRC_SHIFT(1)),
                    binop(Iop_Shl64, SRC_VEC(0), SRC_SHIFT(0))));
#  undef SRC_VEC
#  undef SRC_SHIFT
   // Now h3..h0 are 64 bit vectors with useful information only
   // in the top 16 bits.  We now concatentate those four 16-bit
   // groups so as to produce the final result.
   IRTemp w1 = newTemp(Ity_I64);
   IRTemp w0 = newTemp(Ity_I64);
   assign(w1, binop(Iop_InterleaveHI16x4, mkexpr(h3), mkexpr(h2)));
   assign(w0, binop(Iop_InterleaveHI16x4, mkexpr(h1), mkexpr(h0)));
   return binop(Iop_InterleaveHI32x2, mkexpr(w1), mkexpr(w0));
}

/* Generate 3x64 -> 3x64 deinterleave code, for VLD3.  Caller must
   make *u0, *u1 and *u2 be valid IRTemps before the call. */
static void math_DEINTERLEAVE_3 (
               /*OUT*/IRTemp* u0, /*OUT*/IRTemp* u1, /*OUT*/IRTemp* u2,
               IRTemp i0, IRTemp i1, IRTemp i2, Int laneszB
            )
{
#  define IHI32x2(_e1, _e2) binop(Iop_InterleaveHI32x2, (_e1), (_e2))
#  define IHI16x4(_e1, _e2) binop(Iop_InterleaveHI16x4, (_e1), (_e2))
#  define SHL64(_tmp, _amt) binop(Iop_Shl64, mkexpr(_tmp), mkU8(_amt))
   /* The following assumes that the guest is little endian, and hence
      that the memory-side (interleaved) data is stored
      little-endianly. */
   vassert(u0 && u1 && u2);
   if (laneszB == 4) {
      // memLE(192 bits) == A0 B0 C0 A1 B1 C1
      // i0 == B0 A0, i1 == A1 C0, i2 == C1 B1
      // u0 == A1 A0, u1 == B1 B0, u2 == C1 C0
      assign(*u0, IHI32x2(SHL64(i1,  0), SHL64(i0, 32)));
      assign(*u1, IHI32x2(SHL64(i2, 32), SHL64(i0,  0)));
      assign(*u2, IHI32x2(SHL64(i2,  0), SHL64(i1, 32)));
   } else if (laneszB == 2) {
      // memLE(192 bits) == A0 B0 C0 A1, B1 C1 A2 B2, C2 A3 B3 C3
      // i0 == A1 C0 B0 A0, i1 == B2 A2 C1 B1, i2 == C3 B3 A3 C2
      // u0 == A3 A2 A1 A0, u1 == B3 B2 B1 B0, u2 == C3 C2 C1 C0
#     define XXX(_tmp3,_la3,_tmp2,_la2,_tmp1,_la1,_tmp0,_la0) \
                IHI32x2(                                      \
                   IHI16x4(SHL64((_tmp3),48-16*(_la3)),       \
                           SHL64((_tmp2),48-16*(_la2))),      \
                   IHI16x4(SHL64((_tmp1),48-16*(_la1)),       \
                           SHL64((_tmp0),48-16*(_la0))))
      assign(*u0, XXX(i2,1, i1,2, i0,3, i0,0));
      assign(*u1, XXX(i2,2, i1,3, i1,0, i0,1));
      assign(*u2, XXX(i2,3, i2,0, i1,1, i0,2));
#     undef XXX
   } else if (laneszB == 1) {
      // These describe how the result vectors [7..0] are
      // assembled from the source vectors.  Each pair is
      // (source vector number, lane number).
      static const UChar de0[16] = {2,5, 2,2, 1,7, 1,4, 1,1, 0,6, 0,3, 0,0};
      static const UChar de1[16] = {2,6, 2,3, 2,0, 1,5, 1,2, 0,7, 0,4, 0,1};
      static const UChar de2[16] = {2,7, 2,4, 2,1, 1,6, 1,3, 1,0, 0,5, 0,2};
      assign(*u0, math_PERM_8x8x3(de0, i0, i1, i2));
      assign(*u1, math_PERM_8x8x3(de1, i0, i1, i2));
      assign(*u2, math_PERM_8x8x3(de2, i0, i1, i2));
   } else {
      // Can never happen, since VLD3 only has valid lane widths of 32,
      // 16 or 8 bits.
      vpanic("math_DEINTERLEAVE_3");
   }
#  undef SHL64
#  undef IHI16x4
#  undef IHI32x2
}

/* Generate 3x64 -> 3x64 interleave code, for VST3.  Caller must
   make *i0, *i1 and *i2 be valid IRTemps before the call. */
static void math_INTERLEAVE_3 (
               /*OUT*/IRTemp* i0, /*OUT*/IRTemp* i1, /*OUT*/IRTemp* i2,
               IRTemp u0, IRTemp u1, IRTemp u2, Int laneszB
            )
{
#  define IHI32x2(_e1, _e2) binop(Iop_InterleaveHI32x2, (_e1), (_e2))
#  define IHI16x4(_e1, _e2) binop(Iop_InterleaveHI16x4, (_e1), (_e2))
#  define SHL64(_tmp, _amt) binop(Iop_Shl64, mkexpr(_tmp), mkU8(_amt))
   /* The following assumes that the guest is little endian, and hence
      that the memory-side (interleaved) data is stored
      little-endianly. */
   vassert(i0 && i1 && i2);
   if (laneszB == 4) {
      // memLE(192 bits) == A0 B0 C0 A1 B1 C1
      // i0 == B0 A0, i1 == A1 C0, i2 == C1 B1
      // u0 == A1 A0, u1 == B1 B0, u2 == C1 C0
      assign(*i0, IHI32x2(SHL64(u1, 32), SHL64(u0, 32)));
      assign(*i1, IHI32x2(SHL64(u0,  0), SHL64(u2, 32)));
      assign(*i2, IHI32x2(SHL64(u2,  0), SHL64(u1,  0)));
   } else if (laneszB == 2) {
      // memLE(192 bits) == A0 B0 C0 A1, B1 C1 A2 B2, C2 A3 B3 C3
      // i0 == A1 C0 B0 A0, i1 == B2 A2 C1 B1, i2 == C3 B3 A3 C2
      // u0 == A3 A2 A1 A0, u1 == B3 B2 B1 B0, u2 == C3 C2 C1 C0
#     define XXX(_tmp3,_la3,_tmp2,_la2,_tmp1,_la1,_tmp0,_la0) \
                IHI32x2(                                      \
                   IHI16x4(SHL64((_tmp3),48-16*(_la3)),       \
                           SHL64((_tmp2),48-16*(_la2))),      \
                   IHI16x4(SHL64((_tmp1),48-16*(_la1)),       \
                           SHL64((_tmp0),48-16*(_la0))))
      assign(*i0, XXX(u0,1, u2,0, u1,0, u0,0));
      assign(*i1, XXX(u1,2, u0,2, u2,1, u1,1));
      assign(*i2, XXX(u2,3, u1,3, u0,3, u2,2));
#     undef XXX
   } else if (laneszB == 1) {
      // These describe how the result vectors [7..0] are
      // assembled from the source vectors.  Each pair is
      // (source vector number, lane number).
      static const UChar in0[16] = {1,2, 0,2, 2,1, 1,1, 0,1, 2,0, 1,0, 0,0};
      static const UChar in1[16] = {0,5, 2,4, 1,4, 0,4, 2,3, 1,3, 0,3, 2,2};
      static const UChar in2[16] = {2,7, 1,7, 0,7, 2,6, 1,6, 0,6, 2,5, 1,5};
      assign(*i0, math_PERM_8x8x3(in0, u0, u1, u2));
      assign(*i1, math_PERM_8x8x3(in1, u0, u1, u2));
      assign(*i2, math_PERM_8x8x3(in2, u0, u1, u2));
   } else {
      // Can never happen, since VST3 only has valid lane widths of 32,
      // 16 or 8 bits.
      vpanic("math_INTERLEAVE_3");
   }
#  undef SHL64
#  undef IHI16x4
#  undef IHI32x2
}

/* Generate 4x64 -> 4x64 deinterleave code, for VLD4.  Caller must
   make *u0, *u1, *u2 and *u3 be valid IRTemps before the call. */
static void math_DEINTERLEAVE_4 (
               /*OUT*/IRTemp* u0, /*OUT*/IRTemp* u1,
               /*OUT*/IRTemp* u2, /*OUT*/IRTemp* u3,
               IRTemp i0, IRTemp i1, IRTemp i2, IRTemp i3, Int laneszB
            )
{
#  define IHI32x2(_t1, _t2) \
             binop(Iop_InterleaveHI32x2, mkexpr(_t1), mkexpr(_t2))
#  define ILO32x2(_t1, _t2) \
             binop(Iop_InterleaveLO32x2, mkexpr(_t1), mkexpr(_t2))
#  define IHI16x4(_t1, _t2) \
             binop(Iop_InterleaveHI16x4, mkexpr(_t1), mkexpr(_t2))
#  define ILO16x4(_t1, _t2) \
             binop(Iop_InterleaveLO16x4, mkexpr(_t1), mkexpr(_t2))
#  define IHI8x8(_t1, _e2) \
             binop(Iop_InterleaveHI8x8, mkexpr(_t1), _e2)
#  define SHL64(_tmp, _amt) \
             binop(Iop_Shl64, mkexpr(_tmp), mkU8(_amt))
   /* The following assumes that the guest is little endian, and hence
      that the memory-side (interleaved) data is stored
      little-endianly. */
   vassert(u0 && u1 && u2 && u3);
   if (laneszB == 4) {
      assign(*u0, ILO32x2(i2, i0));
      assign(*u1, IHI32x2(i2, i0));
      assign(*u2, ILO32x2(i3, i1));
      assign(*u3, IHI32x2(i3, i1));
   } else if (laneszB == 2) {
      IRTemp b1b0a1a0 = newTemp(Ity_I64);
      IRTemp b3b2a3a2 = newTemp(Ity_I64);
      IRTemp d1d0c1c0 = newTemp(Ity_I64);
      IRTemp d3d2c3c2 = newTemp(Ity_I64);
      assign(b1b0a1a0, ILO16x4(i1, i0));
      assign(b3b2a3a2, ILO16x4(i3, i2));
      assign(d1d0c1c0, IHI16x4(i1, i0));
      assign(d3d2c3c2, IHI16x4(i3, i2));
      // And now do what we did for the 32-bit case.
      assign(*u0, ILO32x2(b3b2a3a2, b1b0a1a0));
      assign(*u1, IHI32x2(b3b2a3a2, b1b0a1a0));
      assign(*u2, ILO32x2(d3d2c3c2, d1d0c1c0));
      assign(*u3, IHI32x2(d3d2c3c2, d1d0c1c0));
   } else if (laneszB == 1) {
      // Deinterleave into 16-bit chunks, then do as the 16-bit case.
      IRTemp i0x = newTemp(Ity_I64);
      IRTemp i1x = newTemp(Ity_I64);
      IRTemp i2x = newTemp(Ity_I64);
      IRTemp i3x = newTemp(Ity_I64);
      assign(i0x, IHI8x8(i0, SHL64(i0, 32)));
      assign(i1x, IHI8x8(i1, SHL64(i1, 32)));
      assign(i2x, IHI8x8(i2, SHL64(i2, 32)));
      assign(i3x, IHI8x8(i3, SHL64(i3, 32)));
      // From here on is like the 16 bit case.
      IRTemp b1b0a1a0 = newTemp(Ity_I64);
      IRTemp b3b2a3a2 = newTemp(Ity_I64);
      IRTemp d1d0c1c0 = newTemp(Ity_I64);
      IRTemp d3d2c3c2 = newTemp(Ity_I64);
      assign(b1b0a1a0, ILO16x4(i1x, i0x));
      assign(b3b2a3a2, ILO16x4(i3x, i2x));
      assign(d1d0c1c0, IHI16x4(i1x, i0x));
      assign(d3d2c3c2, IHI16x4(i3x, i2x));
      // And now do what we did for the 32-bit case.
      assign(*u0, ILO32x2(b3b2a3a2, b1b0a1a0));
      assign(*u1, IHI32x2(b3b2a3a2, b1b0a1a0));
      assign(*u2, ILO32x2(d3d2c3c2, d1d0c1c0));
      assign(*u3, IHI32x2(d3d2c3c2, d1d0c1c0));
   } else {
      // Can never happen, since VLD4 only has valid lane widths of 32,
      // 16 or 8 bits.
      vpanic("math_DEINTERLEAVE_4");
   }
#  undef SHL64
#  undef IHI8x8
#  undef ILO16x4
#  undef IHI16x4
#  undef ILO32x2
#  undef IHI32x2
}

/* Generate 4x64 -> 4x64 interleave code, for VST4.  Caller must
   make *i0, *i1, *i2 and *i3 be valid IRTemps before the call. */
static void math_INTERLEAVE_4 (
               /*OUT*/IRTemp* i0, /*OUT*/IRTemp* i1,
               /*OUT*/IRTemp* i2, /*OUT*/IRTemp* i3,
               IRTemp u0, IRTemp u1, IRTemp u2, IRTemp u3, Int laneszB
            )
{
#  define IHI32x2(_t1, _t2) \
             binop(Iop_InterleaveHI32x2, mkexpr(_t1), mkexpr(_t2))
#  define ILO32x2(_t1, _t2) \
             binop(Iop_InterleaveLO32x2, mkexpr(_t1), mkexpr(_t2))
#  define CEV16x4(_t1, _t2) \
             binop(Iop_CatEvenLanes16x4, mkexpr(_t1), mkexpr(_t2))
#  define COD16x4(_t1, _t2) \
             binop(Iop_CatOddLanes16x4, mkexpr(_t1), mkexpr(_t2))
#  define COD8x8(_t1, _e2) \
             binop(Iop_CatOddLanes8x8, mkexpr(_t1), _e2)
#  define SHL64(_tmp, _amt) \
             binop(Iop_Shl64, mkexpr(_tmp), mkU8(_amt))
   /* The following assumes that the guest is little endian, and hence
      that the memory-side (interleaved) data is stored
      little-endianly. */
   vassert(u0 && u1 && u2 && u3);
   if (laneszB == 4) {
      assign(*i0, ILO32x2(u1, u0));
      assign(*i1, ILO32x2(u3, u2));
      assign(*i2, IHI32x2(u1, u0));
      assign(*i3, IHI32x2(u3, u2));
   } else if (laneszB == 2) {
      // First, interleave at the 32-bit lane size.
      IRTemp b1b0a1a0 = newTemp(Ity_I64);
      IRTemp b3b2a3a2 = newTemp(Ity_I64);
      IRTemp d1d0c1c0 = newTemp(Ity_I64);
      IRTemp d3d2c3c2 = newTemp(Ity_I64);
      assign(b1b0a1a0, ILO32x2(u1, u0));
      assign(b3b2a3a2, IHI32x2(u1, u0));
      assign(d1d0c1c0, ILO32x2(u3, u2));
      assign(d3d2c3c2, IHI32x2(u3, u2));
      // And interleave (cat) at the 16 bit size.
      assign(*i0, CEV16x4(d1d0c1c0, b1b0a1a0));
      assign(*i1, COD16x4(d1d0c1c0, b1b0a1a0));
      assign(*i2, CEV16x4(d3d2c3c2, b3b2a3a2));
      assign(*i3, COD16x4(d3d2c3c2, b3b2a3a2));
   } else if (laneszB == 1) {
      // First, interleave at the 32-bit lane size.
      IRTemp b1b0a1a0 = newTemp(Ity_I64);
      IRTemp b3b2a3a2 = newTemp(Ity_I64);
      IRTemp d1d0c1c0 = newTemp(Ity_I64);
      IRTemp d3d2c3c2 = newTemp(Ity_I64);
      assign(b1b0a1a0, ILO32x2(u1, u0));
      assign(b3b2a3a2, IHI32x2(u1, u0));
      assign(d1d0c1c0, ILO32x2(u3, u2));
      assign(d3d2c3c2, IHI32x2(u3, u2));
      // And interleave (cat) at the 16 bit size.
      IRTemp i0x = newTemp(Ity_I64);
      IRTemp i1x = newTemp(Ity_I64);
      IRTemp i2x = newTemp(Ity_I64);
      IRTemp i3x = newTemp(Ity_I64);
      assign(i0x, CEV16x4(d1d0c1c0, b1b0a1a0));
      assign(i1x, COD16x4(d1d0c1c0, b1b0a1a0));
      assign(i2x, CEV16x4(d3d2c3c2, b3b2a3a2));
      assign(i3x, COD16x4(d3d2c3c2, b3b2a3a2));
      // And rearrange within each word, to get the right 8 bit lanes.
      assign(*i0, COD8x8(i0x, SHL64(i0x, 8)));
      assign(*i1, COD8x8(i1x, SHL64(i1x, 8)));
      assign(*i2, COD8x8(i2x, SHL64(i2x, 8)));
      assign(*i3, COD8x8(i3x, SHL64(i3x, 8)));
   } else {
      // Can never happen, since VLD4 only has valid lane widths of 32,
      // 16 or 8 bits.
      vpanic("math_DEINTERLEAVE_4");
   }
#  undef SHL64
#  undef COD8x8
#  undef COD16x4
#  undef CEV16x4
#  undef ILO32x2
#  undef IHI32x2
}

/* A7.7 Advanced SIMD element or structure load/store instructions */
static
Bool dis_neon_load_or_store ( UInt theInstr,
                              Bool isT, IRTemp condT )
{
#  define INSN(_bMax,_bMin)  SLICE_UInt(theInstr, (_bMax), (_bMin))
   UInt bA = INSN(23,23);
   UInt fB = INSN(11,8);
   UInt bL = INSN(21,21);
   UInt rD = (INSN(22,22) << 4) | INSN(15,12);
   UInt rN = INSN(19,16);
   UInt rM = INSN(3,0);
   UInt N, size, i, j;
   UInt inc;
   UInt regs = 1;

   if (isT) {
      vassert(condT != IRTemp_INVALID);
   } else {
      vassert(condT == IRTemp_INVALID);
   }
   /* So now, if condT is not IRTemp_INVALID, we know we're
      dealing with Thumb code. */

   if (INSN(20,20) != 0)
      return False;

   IRTemp initialRn = newTemp(Ity_I32);
   assign(initialRn, isT ? getIRegT(rN) : getIRegA(rN));

   IRTemp initialRm = newTemp(Ity_I32);
   assign(initialRm, isT ? getIRegT(rM) : getIRegA(rM));

   /* There are 3 cases:
      (1) VSTn / VLDn (n-element structure from/to one lane)
      (2) VLDn (single element to all lanes)
      (3) VSTn / VLDn (multiple n-element structures)
   */
   if (bA) {
      N = fB & 3;
      if ((fB >> 2) < 3) {
         /* ------------ Case (1) ------------
            VSTn / VLDn (n-element structure from/to one lane) */

         size = fB >> 2;

         switch (size) {
            case 0: i = INSN(7,5); inc = 1; break;
            case 1: i = INSN(7,6); inc = INSN(5,5) ? 2 : 1; break;
            case 2: i = INSN(7,7); inc = INSN(6,6) ? 2 : 1; break;
            case 3: return False;
            default: vassert(0);
         }

         IRTemp addr = newTemp(Ity_I32);
         assign(addr, mkexpr(initialRn));

         // go uncond
         if (condT != IRTemp_INVALID)
            mk_skip_over_T32_if_cond_is_false(condT);
         // now uncond

         if (bL)
            mk_neon_elem_load_to_one_lane(rD, inc, i, N, size, addr);
         else
            mk_neon_elem_store_from_one_lane(rD, inc, i, N, size, addr);
         DIP("v%s%u.%d {", bL ? "ld" : "st", N + 1, 8 << size);
         for (j = 0; j <= N; j++) {
            if (j)
               DIP(", ");
            DIP("d%u[%u]", rD + j * inc, i);
         }
         DIP("}, [r%u]", rN);
         if (rM != 13 && rM != 15) {
            DIP(", r%u\n", rM);
         } else {
            DIP("%s\n", (rM != 15) ? "!" : "");
         }
      } else {
         /* ------------ Case (2) ------------ 
            VLDn (single element to all lanes) */
         UInt r;
         if (bL == 0)
            return False;

         inc = INSN(5,5) + 1;
         size = INSN(7,6);

         /* size == 3 and size == 2 cases differ in alignment constraints */
         if (size == 3 && N == 3 && INSN(4,4) == 1)
            size = 2;

         if (size == 0 && N == 0 && INSN(4,4) == 1)
            return False;
         if (N == 2 && INSN(4,4) == 1)
            return False;
         if (size == 3)
            return False;

         // go uncond
         if (condT != IRTemp_INVALID)
            mk_skip_over_T32_if_cond_is_false(condT);
         // now uncond

         IRTemp addr = newTemp(Ity_I32);
         assign(addr, mkexpr(initialRn));

         if (N == 0 && INSN(5,5))
            regs = 2;

         for (r = 0; r < regs; r++) {
            switch (size) {
               case 0:
                  putDRegI64(rD + r, unop(Iop_Dup8x8,
                                          loadLE(Ity_I8, mkexpr(addr))),
                             IRTemp_INVALID);
                  break;
               case 1:
                  putDRegI64(rD + r, unop(Iop_Dup16x4,
                                          loadLE(Ity_I16, mkexpr(addr))),
                             IRTemp_INVALID);
                  break;
               case 2:
                  putDRegI64(rD + r, unop(Iop_Dup32x2,
                                          loadLE(Ity_I32, mkexpr(addr))),
                             IRTemp_INVALID);
                  break;
               default:
                  vassert(0);
            }
            for (i = 1; i <= N; i++) {
               switch (size) {
                  case 0:
                     putDRegI64(rD + r + i * inc,
                                unop(Iop_Dup8x8,
                                     loadLE(Ity_I8, binop(Iop_Add32,
                                                          mkexpr(addr),
                                                          mkU32(i * 1)))),
                                IRTemp_INVALID);
                     break;
                  case 1:
                     putDRegI64(rD + r + i * inc,
                                unop(Iop_Dup16x4,
                                     loadLE(Ity_I16, binop(Iop_Add32,
                                                           mkexpr(addr),
                                                           mkU32(i * 2)))),
                                IRTemp_INVALID);
                     break;
                  case 2:
                     putDRegI64(rD + r + i * inc,
                                unop(Iop_Dup32x2,
                                     loadLE(Ity_I32, binop(Iop_Add32,
                                                           mkexpr(addr),
                                                           mkU32(i * 4)))),
                                IRTemp_INVALID);
                     break;
                  default:
                     vassert(0);
               }
            }
         }
         DIP("vld%u.%d {", N + 1, 8 << size);
         for (r = 0; r < regs; r++) {
            for (i = 0; i <= N; i++) {
               if (i || r)
                  DIP(", ");
               DIP("d%u[]", rD + r + i * inc);
            }
         }
         DIP("}, [r%u]", rN);
         if (rM != 13 && rM != 15) {
            DIP(", r%u\n", rM);
         } else {
            DIP("%s\n", (rM != 15) ? "!" : "");
         }
      }
      /* Writeback.  We're uncond here, so no condT-ing. */
      if (rM != 15) {
         if (rM == 13) {
            IRExpr* e = binop(Iop_Add32,
                              mkexpr(initialRn),
                              mkU32((1 << size) * (N + 1)));
            if (isT)
               putIRegT(rN, e, IRTemp_INVALID);
            else
               putIRegA(rN, e, IRTemp_INVALID, Ijk_Boring);
         } else {
            IRExpr* e = binop(Iop_Add32,
                              mkexpr(initialRn),
                              mkexpr(initialRm));
            if (isT)
               putIRegT(rN, e, IRTemp_INVALID);
            else
               putIRegA(rN, e, IRTemp_INVALID, Ijk_Boring);
         }
      }
      return True;
   } else {
      /* ------------ Case (3) ------------
         VSTn / VLDn (multiple n-element structures) */
      inc = (fB & 1) + 1;

      if (fB == BITS4(0,0,1,0)       // Dd, Dd+1, Dd+2, Dd+3  inc = 1  regs = 4
          || fB == BITS4(0,1,1,0)    // Dd, Dd+1, Dd+2        inc = 1  regs = 3
          || fB == BITS4(0,1,1,1)    // Dd                    inc = 2  regs = 1
          || fB == BITS4(1,0,1,0)) { // Dd, Dd+1              inc = 1  regs = 2
         N = 0; // VLD1/VST1.  'inc' does not appear to have any
                // meaning for the VLD1/VST1 cases.  'regs' is the number of
                // registers involved.
         if (rD + regs > 32) return False;
      } 
      else 
      if (fB == BITS4(0,0,1,1)       // Dd, Dd+1, Dd+2, Dd+3  inc=2  regs = 2
          || fB == BITS4(1,0,0,0)    // Dd, Dd+1              inc=1  regs = 1
          || fB == BITS4(1,0,0,1)) { // Dd, Dd+2              inc=2  regs = 1
         N = 1; // VLD2/VST2.  'regs' is the number of register-pairs involved
         if (regs == 1 && inc == 1 && rD + 1 >= 32) return False;
         if (regs == 1 && inc == 2 && rD + 2 >= 32) return False;
         if (regs == 2 && inc == 2 && rD + 3 >= 32) return False;
      } else if (fB == BITS4(0,1,0,0) || fB == BITS4(0,1,0,1)) {
         N = 2; // VLD3/VST3
         if (inc == 1 && rD + 2 >= 32) return False;
         if (inc == 2 && rD + 4 >= 32) return False;
      } else if (fB == BITS4(0,0,0,0) || fB == BITS4(0,0,0,1)) {
         N = 3; // VLD4/VST4
         if (inc == 1 && rD + 3 >= 32) return False;
         if (inc == 2 && rD + 6 >= 32) return False;
      } else {
         return False;
      }

      if (N == 1 && fB == BITS4(0,0,1,1)) {
         regs = 2;
      } else if (N == 0) {
         if (fB == BITS4(1,0,1,0)) {
            regs = 2;
         } else if (fB == BITS4(0,1,1,0)) {
            regs = 3;
         } else if (fB == BITS4(0,0,1,0)) {
            regs = 4;
         }
      }

      size = INSN(7,6);
      if (N == 0 && size == 3)
         size = 2;
      if (size == 3)
         return False;

      // go uncond
      if (condT != IRTemp_INVALID)
         mk_skip_over_T32_if_cond_is_false(condT);
      // now uncond

      IRTemp addr = newTemp(Ity_I32);
      assign(addr, mkexpr(initialRn));

      if (N == 0 /* No interleaving -- VLD1/VST1 */) {
         UInt r;
         vassert(regs == 1 || regs == 2 || regs == 3 || regs == 4);
         /* inc has no relevance here */
         for (r = 0; r < regs; r++) {
            if (bL)
               putDRegI64(rD+r, loadLE(Ity_I64, mkexpr(addr)), IRTemp_INVALID);
            else
               storeLE(mkexpr(addr), getDRegI64(rD+r));
            IRTemp tmp = newTemp(Ity_I32);
            assign(tmp, binop(Iop_Add32, mkexpr(addr), mkU32(8)));
            addr = tmp;
         }
      }
      else
      if (N == 1 /* 2-interleaving -- VLD2/VST2 */) {
         vassert( (regs == 1 && (inc == 1 || inc == 2))
                   || (regs == 2 && inc == 2) );
         // Make 'nregs' be the number of registers and 'regstep'
         // equal the actual register-step.  The ARM encoding, using 'regs'
         // and 'inc', is bizarre.  After this, we have:
         // Dd, Dd+1              regs = 1, inc = 1,   nregs = 2, regstep = 1
         // Dd, Dd+2              regs = 1, inc = 2,   nregs = 2, regstep = 2
         // Dd, Dd+1, Dd+2, Dd+3  regs = 2, inc = 2,   nregs = 4, regstep = 1
         UInt nregs   = 2;
         UInt regstep = 1;
         if (regs == 1 && inc == 1) {
            /* nothing */
         } else if (regs == 1 && inc == 2) {
            regstep = 2;
         } else if (regs == 2 && inc == 2) {
            nregs = 4;
         } else {
            vassert(0);
         }
         // 'a' is address,
         // 'di' is interleaved data, 'du' is uninterleaved data
         if (nregs == 2) {
            IRExpr* a0  = binop(Iop_Add32, mkexpr(addr), mkU32(0));
            IRExpr* a1  = binop(Iop_Add32, mkexpr(addr), mkU32(8));
            IRTemp  di0 = newTemp(Ity_I64);
            IRTemp  di1 = newTemp(Ity_I64);
            IRTemp  du0 = newTemp(Ity_I64); 
            IRTemp  du1 = newTemp(Ity_I64);
            if (bL) {
               assign(di0, loadLE(Ity_I64, a0));
               assign(di1, loadLE(Ity_I64, a1));
               math_DEINTERLEAVE_2(&du0, &du1, di0, di1, 1 << size);
               putDRegI64(rD + 0 * regstep, mkexpr(du0), IRTemp_INVALID);
               putDRegI64(rD + 1 * regstep, mkexpr(du1), IRTemp_INVALID);
            } else {
               assign(du0, getDRegI64(rD + 0 * regstep));
               assign(du1, getDRegI64(rD + 1 * regstep));
               math_INTERLEAVE_2(&di0, &di1, du0, du1, 1 << size);
               storeLE(a0, mkexpr(di0));
               storeLE(a1, mkexpr(di1));
            }
            IRTemp tmp = newTemp(Ity_I32);
            assign(tmp, binop(Iop_Add32, mkexpr(addr), mkU32(16)));
            addr = tmp;
         } else {
            vassert(nregs == 4);
            vassert(regstep == 1);
            IRExpr* a0  = binop(Iop_Add32, mkexpr(addr), mkU32(0));
            IRExpr* a1  = binop(Iop_Add32, mkexpr(addr), mkU32(8));
            IRExpr* a2  = binop(Iop_Add32, mkexpr(addr), mkU32(16));
            IRExpr* a3  = binop(Iop_Add32, mkexpr(addr), mkU32(24));
            IRTemp  di0 = newTemp(Ity_I64);
            IRTemp  di1 = newTemp(Ity_I64);
            IRTemp  di2 = newTemp(Ity_I64);
            IRTemp  di3 = newTemp(Ity_I64);
            IRTemp  du0 = newTemp(Ity_I64); 
            IRTemp  du1 = newTemp(Ity_I64);
            IRTemp  du2 = newTemp(Ity_I64); 
            IRTemp  du3 = newTemp(Ity_I64);
            if (bL) {
               assign(di0, loadLE(Ity_I64, a0));
               assign(di1, loadLE(Ity_I64, a1));
               assign(di2, loadLE(Ity_I64, a2));
               assign(di3, loadLE(Ity_I64, a3));
               // Note spooky interleaving: du0, du2, di0, di1 etc
               math_DEINTERLEAVE_2(&du0, &du2, di0, di1, 1 << size);
               math_DEINTERLEAVE_2(&du1, &du3, di2, di3, 1 << size);
               putDRegI64(rD + 0 * regstep, mkexpr(du0), IRTemp_INVALID);
               putDRegI64(rD + 1 * regstep, mkexpr(du1), IRTemp_INVALID);
               putDRegI64(rD + 2 * regstep, mkexpr(du2), IRTemp_INVALID);
               putDRegI64(rD + 3 * regstep, mkexpr(du3), IRTemp_INVALID);
            } else {
               assign(du0, getDRegI64(rD + 0 * regstep));
               assign(du1, getDRegI64(rD + 1 * regstep));
               assign(du2, getDRegI64(rD + 2 * regstep));
               assign(du3, getDRegI64(rD + 3 * regstep));
               // Note spooky interleaving: du0, du2, di0, di1 etc
               math_INTERLEAVE_2(&di0, &di1, du0, du2, 1 << size);
               math_INTERLEAVE_2(&di2, &di3, du1, du3, 1 << size);
               storeLE(a0, mkexpr(di0));
               storeLE(a1, mkexpr(di1));
               storeLE(a2, mkexpr(di2));
               storeLE(a3, mkexpr(di3));
            }

            IRTemp tmp = newTemp(Ity_I32);
            assign(tmp, binop(Iop_Add32, mkexpr(addr), mkU32(32)));
            addr = tmp;
         }
      }
      else
      if (N == 2 /* 3-interleaving -- VLD3/VST3 */) {
         // Dd, Dd+1, Dd+2   regs = 1, inc = 1
         // Dd, Dd+2, Dd+4   regs = 1, inc = 2
         vassert(regs == 1 && (inc == 1 || inc == 2));
         IRExpr* a0  = binop(Iop_Add32, mkexpr(addr), mkU32(0));
         IRExpr* a1  = binop(Iop_Add32, mkexpr(addr), mkU32(8));
         IRExpr* a2  = binop(Iop_Add32, mkexpr(addr), mkU32(16));
         IRTemp  di0 = newTemp(Ity_I64);
         IRTemp  di1 = newTemp(Ity_I64);
         IRTemp  di2 = newTemp(Ity_I64);
         IRTemp  du0 = newTemp(Ity_I64); 
         IRTemp  du1 = newTemp(Ity_I64);
         IRTemp  du2 = newTemp(Ity_I64);
         if (bL) {
            assign(di0, loadLE(Ity_I64, a0));
            assign(di1, loadLE(Ity_I64, a1));
            assign(di2, loadLE(Ity_I64, a2));
            math_DEINTERLEAVE_3(&du0, &du1, &du2, di0, di1, di2, 1 << size);
            putDRegI64(rD + 0 * inc, mkexpr(du0), IRTemp_INVALID);
            putDRegI64(rD + 1 * inc, mkexpr(du1), IRTemp_INVALID);
            putDRegI64(rD + 2 * inc, mkexpr(du2), IRTemp_INVALID);
         } else {
            assign(du0, getDRegI64(rD + 0 * inc));
            assign(du1, getDRegI64(rD + 1 * inc));
            assign(du2, getDRegI64(rD + 2 * inc));
            math_INTERLEAVE_3(&di0, &di1, &di2, du0, du1, du2, 1 << size);
            storeLE(a0, mkexpr(di0));
            storeLE(a1, mkexpr(di1));
            storeLE(a2, mkexpr(di2));
         }
         IRTemp tmp = newTemp(Ity_I32);
         assign(tmp, binop(Iop_Add32, mkexpr(addr), mkU32(24)));
         addr = tmp;
      }
      else 
      if (N == 3 /* 4-interleaving -- VLD4/VST4 */) {
         // Dd, Dd+1, Dd+2, Dd+3   regs = 1, inc = 1
         // Dd, Dd+2, Dd+4, Dd+6   regs = 1, inc = 2
         vassert(regs == 1 && (inc == 1 || inc == 2));
         IRExpr* a0  = binop(Iop_Add32, mkexpr(addr), mkU32(0));
         IRExpr* a1  = binop(Iop_Add32, mkexpr(addr), mkU32(8));
         IRExpr* a2  = binop(Iop_Add32, mkexpr(addr), mkU32(16));
         IRExpr* a3  = binop(Iop_Add32, mkexpr(addr), mkU32(24));
         IRTemp  di0 = newTemp(Ity_I64);
         IRTemp  di1 = newTemp(Ity_I64);
         IRTemp  di2 = newTemp(Ity_I64);
         IRTemp  di3 = newTemp(Ity_I64);
         IRTemp  du0 = newTemp(Ity_I64); 
         IRTemp  du1 = newTemp(Ity_I64);
         IRTemp  du2 = newTemp(Ity_I64);
         IRTemp  du3 = newTemp(Ity_I64);
         if (bL) {
            assign(di0, loadLE(Ity_I64, a0));
            assign(di1, loadLE(Ity_I64, a1));
            assign(di2, loadLE(Ity_I64, a2));
            assign(di3, loadLE(Ity_I64, a3));
            math_DEINTERLEAVE_4(&du0, &du1, &du2, &du3,
                                di0, di1, di2, di3, 1 << size);
            putDRegI64(rD + 0 * inc, mkexpr(du0), IRTemp_INVALID);
            putDRegI64(rD + 1 * inc, mkexpr(du1), IRTemp_INVALID);
            putDRegI64(rD + 2 * inc, mkexpr(du2), IRTemp_INVALID);
            putDRegI64(rD + 3 * inc, mkexpr(du3), IRTemp_INVALID);
         } else {
            assign(du0, getDRegI64(rD + 0 * inc));
            assign(du1, getDRegI64(rD + 1 * inc));
            assign(du2, getDRegI64(rD + 2 * inc));
            assign(du3, getDRegI64(rD + 3 * inc));
            math_INTERLEAVE_4(&di0, &di1, &di2, &di3,
                              du0, du1, du2, du3, 1 << size);
            storeLE(a0, mkexpr(di0));
            storeLE(a1, mkexpr(di1));
            storeLE(a2, mkexpr(di2));
            storeLE(a3, mkexpr(di3));
         }
         IRTemp tmp = newTemp(Ity_I32);
         assign(tmp, binop(Iop_Add32, mkexpr(addr), mkU32(32)));
         addr = tmp;
      }
      else {
         vassert(0);
      }

      /* Writeback */
      if (rM != 15) {
         IRExpr* e;
         if (rM == 13) {
            e = binop(Iop_Add32, mkexpr(initialRn),
                                 mkU32(8 * (N + 1) * regs));
         } else {
            e = binop(Iop_Add32, mkexpr(initialRn),
                                 mkexpr(initialRm));
         }
         if (isT)
            putIRegT(rN, e, IRTemp_INVALID);
         else
            putIRegA(rN, e, IRTemp_INVALID, Ijk_Boring);
      }

      DIP("v%s%u.%d {", bL ? "ld" : "st", N + 1, 8 << INSN(7,6));
      if ((inc == 1 && regs * (N + 1) > 1)
          || (inc == 2 && regs > 1 && N > 0)) {
         DIP("d%u-d%u", rD, rD + regs * (N + 1) - 1);
      } else {
         UInt r;
         for (r = 0; r < regs; r++) {
            for (i = 0; i <= N; i++) {
               if (i || r)
                  DIP(", ");
               DIP("d%u", rD + r + i * inc);
            }
         }
      }
      DIP("}, [r%u]", rN);
      if (rM != 13 && rM != 15) {
         DIP(", r%u\n", rM);
      } else {
         DIP("%s\n", (rM != 15) ? "!" : "");
      }
      return True;
   }
#  undef INSN
}


/*------------------------------------------------------------*/
/*--- NEON, top level control                              ---*/
/*------------------------------------------------------------*/

/* Both ARM and Thumb */

/* Translate a NEON instruction.    If successful, returns
   True and *dres may or may not be updated.  If failure, returns
   False and doesn't change *dres nor create any IR.

   The Thumb and ARM encodings are similar for the 24 bottom bits, but
   the top 8 bits are slightly different.  In both cases, the caller
   must pass the entire 32 bits.  Callers may pass any instruction;
   this ignores non-NEON ones.

   Caller must supply an IRTemp 'condT' holding the gating condition,
   or IRTemp_INVALID indicating the insn is always executed.  In ARM
   code, this must always be IRTemp_INVALID because NEON insns are
   unconditional for ARM.

   Finally, the caller must indicate whether this occurs in ARM or in
   Thumb code.

   This only handles NEON for ARMv7 and below.  The NEON extensions
   for v8 are handled by decode_V8_instruction.
*/
static Bool decode_NEON_instruction_ARMv7_and_below (
               /*MOD*/DisResult* dres,
               UInt              insn32,
               IRTemp            condT,
               Bool              isT
            )
{
#  define INSN(_bMax,_bMin)  SLICE_UInt(insn32, (_bMax), (_bMin))

   /* There are two kinds of instruction to deal with: load/store and
      data processing.  In each case, in ARM mode we merely identify
      the kind, and pass it on to the relevant sub-handler.  In Thumb
      mode we identify the kind, swizzle the bits around to make it
      have the same encoding as in ARM, and hand it on to the
      sub-handler.
   */

   /* In ARM mode, NEON instructions can't be conditional. */
   if (!isT)
      vassert(condT == IRTemp_INVALID);

   /* Data processing:
      Thumb: 111U 1111 AAAA Axxx xxxx BBBB CCCC xxxx
      ARM:   1111 001U AAAA Axxx xxxx BBBB CCCC xxxx
   */
   if (!isT && INSN(31,25) == BITS7(1,1,1,1,0,0,1)) {
      // ARM, DP
      return dis_neon_data_processing(INSN(31,0), condT);
   }
   if (isT && INSN(31,29) == BITS3(1,1,1)
       && INSN(27,24) == BITS4(1,1,1,1)) {
      // Thumb, DP
      UInt reformatted = INSN(23,0);
      reformatted |= (((UInt)INSN(28,28)) << 24); // U bit
      reformatted |= (((UInt)BITS7(1,1,1,1,0,0,1)) << 25);
      return dis_neon_data_processing(reformatted, condT);
   }

   /* Load/store:
      Thumb: 1111 1001 AxL0 xxxx xxxx BBBB xxxx xxxx
      ARM:   1111 0100 AxL0 xxxx xxxx BBBB xxxx xxxx
   */
   if (!isT && INSN(31,24) == BITS8(1,1,1,1,0,1,0,0)) {
      // ARM, memory
      return dis_neon_load_or_store(INSN(31,0), isT, condT);
   }
   if (isT && INSN(31,24) == BITS8(1,1,1,1,1,0,0,1)) {
      UInt reformatted = INSN(23,0);
      reformatted |= (((UInt)BITS8(1,1,1,1,0,1,0,0)) << 24);
      return dis_neon_load_or_store(reformatted, isT, condT);
   }

   /* Doesn't match. */
   return False;

#  undef INSN
}


/*------------------------------------------------------------*/
/*--- V6 MEDIA instructions                                ---*/
/*------------------------------------------------------------*/

/* Both ARM and Thumb */

/* Translate a V6 media instruction.    If successful, returns
   True and *dres may or may not be updated.  If failure, returns
   False and doesn't change *dres nor create any IR.

   The Thumb and ARM encodings are completely different.  In Thumb
   mode, the caller must pass the entire 32 bits.  In ARM mode it must
   pass the lower 28 bits.  Apart from that, callers may pass any
   instruction; this function ignores anything it doesn't recognise.

   Caller must supply an IRTemp 'condT' holding the gating condition,
   or IRTemp_INVALID indicating the insn is always executed.

   Caller must also supply an ARMCondcode 'conq'.  This is only used
   for debug printing, no other purpose.  For ARM, this is simply the
   top 4 bits of the original instruction.  For Thumb, the condition
   is not (really) known until run time, and so ARMCondAL should be
   passed, only so that printing of these instructions does not show
   any condition.

   Finally, the caller must indicate whether this occurs in ARM or in
   Thumb code.
*/
static Bool decode_V6MEDIA_instruction (
               /*MOD*/DisResult* dres,
               UInt              insnv6m,
               IRTemp            condT,
               ARMCondcode       conq,
               Bool              isT
            )
{
#  define INSNA(_bMax,_bMin)   SLICE_UInt(insnv6m, (_bMax), (_bMin))
#  define INSNT0(_bMax,_bMin)  SLICE_UInt( ((insnv6m >> 16) & 0xFFFF), \
                                           (_bMax), (_bMin) )
#  define INSNT1(_bMax,_bMin)  SLICE_UInt( ((insnv6m >> 0)  & 0xFFFF), \
                                           (_bMax), (_bMin) )
   HChar dis_buf[128];
   dis_buf[0] = 0;

   if (isT) {
      vassert(conq == ARMCondAL);
   } else {
      vassert(INSNA(31,28) == BITS4(0,0,0,0)); // caller's obligation
      vassert(conq >= ARMCondEQ && conq <= ARMCondAL);
   }

   /* ----------- smulbb, smulbt, smultb, smultt ----------- */
   {
     UInt regD = 99, regM = 99, regN = 99, bitM = 0, bitN = 0;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFB1 && INSNT1(15,12) == BITS4(1,1,1,1)
            && INSNT1(7,6) == BITS2(0,0)) {
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           regN = INSNT0(3,0);
           bitM = INSNT1(4,4);
           bitN = INSNT1(5,5);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (BITS8(0,0,0,1,0,1,1,0) == INSNA(27,20) &&
            BITS4(0,0,0,0)         == INSNA(15,12) &&
            BITS4(1,0,0,0)         == (INSNA(7,4) & BITS4(1,0,0,1)) ) {
           regD = INSNA(19,16);
           regM = INSNA(11,8);
           regN = INSNA(3,0);
           bitM = INSNA(6,6);
           bitN = INSNA(5,5);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp srcN = newTemp(Ity_I32);
        IRTemp srcM = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);

        assign( srcN, binop(Iop_Sar32,
                            binop(Iop_Shl32,
                                  isT ? getIRegT(regN) : getIRegA(regN),
                                  mkU8(bitN ? 0 : 16)), mkU8(16)) );
        assign( srcM, binop(Iop_Sar32,
                            binop(Iop_Shl32,
                                  isT ? getIRegT(regM) : getIRegA(regM),
                                  mkU8(bitM ? 0 : 16)), mkU8(16)) );
        assign( res, binop(Iop_Mul32, mkexpr(srcN), mkexpr(srcM)) );

        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        DIP( "smul%c%c%s r%u, r%u, r%u\n", bitN ? 't' : 'b', bitM ? 't' : 'b',
             nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ------------ smulwb<y><c> <Rd>,<Rn>,<Rm> ------------- */
   /* ------------ smulwt<y><c> <Rd>,<Rn>,<Rm> ------------- */
   {
     UInt regD = 99, regN = 99, regM = 99, bitM = 0;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFB3 && INSNT1(15,12) == BITS4(1,1,1,1)
            && INSNT1(7,5) == BITS3(0,0,0)) {
          regN = INSNT0(3,0);
          regD = INSNT1(11,8);
          regM = INSNT1(3,0);
          bitM = INSNT1(4,4);
          if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
             gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,0,0,1,0,0,1,0) && 
            INSNA(15,12) == BITS4(0,0,0,0)         &&
            (INSNA(7,4) & BITS4(1,0,1,1)) == BITS4(1,0,1,0)) {
           regD = INSNA(19,16);
           regN = INSNA(3,0);
           regM = INSNA(11,8);
           bitM = INSNA(6,6);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_prod = newTemp(Ity_I64);

        assign( irt_prod, 
                binop(Iop_MullS32,
                      isT ? getIRegT(regN) : getIRegA(regN),
                      binop(Iop_Sar32, 
                            binop(Iop_Shl32,
                                  isT ? getIRegT(regM) : getIRegA(regM),
                                  mkU8(bitM ? 0 : 16)), 
                            mkU8(16))) );

        IRExpr* ire_result = binop(Iop_Or32, 
                                   binop( Iop_Shl32, 
                                          unop(Iop_64HIto32, mkexpr(irt_prod)), 
                                          mkU8(16) ), 
                                   binop( Iop_Shr32, 
                                          unop(Iop_64to32, mkexpr(irt_prod)), 
                                          mkU8(16) ) );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP("smulw%c%s r%u, r%u, r%u\n",
            bitM ? 't' : 'b', nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------ pkhbt<c> Rd, Rn, Rm {,LSL #imm} ------------- */
   /* ------------ pkhtb<c> Rd, Rn, Rm {,ASR #imm} ------------- */
   {
     UInt regD = 99, regN = 99, regM = 99, imm5 = 99, shift_type = 99;
     Bool tbform = False;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xEAC 
            && INSNT1(15,15) == 0 && INSNT1(4,4) == 0) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           imm5 = (INSNT1(14,12) << 2) | INSNT1(7,6);
           shift_type = (INSNT1(5,5) << 1) | 0;
           tbform = (INSNT1(5,5) == 0) ? False : True;
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,1,0,0,0) &&
            INSNA(5,4)   == BITS2(0,1)             &&
            (INSNA(6,6)  == 0 || INSNA(6,6) == 1) ) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           imm5 = INSNA(11,7);
           shift_type = (INSNA(6,6) << 1) | 0;
           tbform = (INSNA(6,6) == 0) ? False : True;
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regM       = newTemp(Ity_I32);
        IRTemp irt_regM_shift = newTemp(Ity_I32);
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );
        compute_result_and_C_after_shift_by_imm5(
           dis_buf, &irt_regM_shift, NULL, irt_regM, shift_type, imm5, regM );

        UInt mask = (tbform == True) ? 0x0000FFFF : 0xFFFF0000;
        IRExpr* ire_result 
          = binop( Iop_Or32, 
                   binop(Iop_And32, mkexpr(irt_regM_shift), mkU32(mask)), 
                   binop(Iop_And32, isT ? getIRegT(regN) : getIRegA(regN),
                                    unop(Iop_Not32, mkU32(mask))) );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "pkh%s%s r%u, r%u, r%u %s\n", tbform ? "tb" : "bt", 
             nCC(conq), regD, regN, regM, dis_buf );

        return True;
     }
     /* fall through */
   }

   /* ---------- usat<c> <Rd>,#<imm5>,<Rn>{,<shift>} ----------- */
   {
     UInt regD = 99, regN = 99, shift_type = 99, imm5 = 99, sat_imm = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,6) == BITS10(1,1,1,1,0,0,1,1,1,0)
            && INSNT0(4,4) == 0
            && INSNT1(15,15) == 0 && INSNT1(5,5) == 0) {
           regD       = INSNT1(11,8);
           regN       = INSNT0(3,0);
           shift_type = (INSNT0(5,5) << 1) | 0;
           imm5       = (INSNT1(14,12) << 2) | INSNT1(7,6);
           sat_imm    = INSNT1(4,0);
           if (!isBadRegT(regD) && !isBadRegT(regN))
              gate = True;
           if (shift_type == BITS2(1,0) && imm5 == 0)
              gate = False;
        }
     } else {
        if (INSNA(27,21) == BITS7(0,1,1,0,1,1,1) &&
            INSNA(5,4)   == BITS2(0,1)) {
           regD       = INSNA(15,12);
           regN       = INSNA(3,0);
           shift_type = (INSNA(6,6) << 1) | 0;
           imm5       = INSNA(11,7);
           sat_imm    = INSNA(20,16);
           if (regD != 15 && regN != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN       = newTemp(Ity_I32);
        IRTemp irt_regN_shift = newTemp(Ity_I32);
        IRTemp irt_sat_Q      = newTemp(Ity_I32);
        IRTemp irt_result     = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        compute_result_and_C_after_shift_by_imm5(
                dis_buf, &irt_regN_shift, NULL,
                irt_regN, shift_type, imm5, regN );

        armUnsignedSatQ( &irt_result, &irt_sat_Q, irt_regN_shift, sat_imm );
        or_into_QFLAG32( mkexpr(irt_sat_Q), condT );

        if (isT)
           putIRegT( regD, mkexpr(irt_result), condT );
        else
           putIRegA( regD, mkexpr(irt_result), condT, Ijk_Boring );

        DIP("usat%s r%u, #0x%04x, %s\n",
            nCC(conq), regD, imm5, dis_buf);
        return True;
     }
     /* fall through */
   }

  /* ----------- ssat<c> <Rd>,#<imm5>,<Rn>{,<shift>} ----------- */
   {
     UInt regD = 99, regN = 99, shift_type = 99, imm5 = 99, sat_imm = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,6) == BITS10(1,1,1,1,0,0,1,1,0,0)
            && INSNT0(4,4) == 0
            && INSNT1(15,15) == 0 && INSNT1(5,5) == 0) {
           regD       = INSNT1(11,8);
           regN       = INSNT0(3,0);
           shift_type = (INSNT0(5,5) << 1) | 0;
           imm5       = (INSNT1(14,12) << 2) | INSNT1(7,6);
           sat_imm    = INSNT1(4,0) + 1;
           if (!isBadRegT(regD) && !isBadRegT(regN))
              gate = True;
           if (shift_type == BITS2(1,0) && imm5 == 0)
              gate = False;
        }
     } else {
        if (INSNA(27,21) == BITS7(0,1,1,0,1,0,1) &&
            INSNA(5,4)   == BITS2(0,1)) {
           regD       = INSNA(15,12);
           regN       = INSNA(3,0);
           shift_type = (INSNA(6,6) << 1) | 0;
           imm5       = INSNA(11,7);
           sat_imm    = INSNA(20,16) + 1;
           if (regD != 15 && regN != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN       = newTemp(Ity_I32);
        IRTemp irt_regN_shift = newTemp(Ity_I32);
        IRTemp irt_sat_Q      = newTemp(Ity_I32);
        IRTemp irt_result     = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        compute_result_and_C_after_shift_by_imm5(
                dis_buf, &irt_regN_shift, NULL,
                irt_regN, shift_type, imm5, regN );

        armSignedSatQ( irt_regN_shift, sat_imm, &irt_result, &irt_sat_Q );
        or_into_QFLAG32( mkexpr(irt_sat_Q), condT );

        if (isT)
           putIRegT( regD, mkexpr(irt_result), condT );
        else
           putIRegA( regD, mkexpr(irt_result), condT, Ijk_Boring );

        DIP( "ssat%s r%u, #0x%04x, %s\n",
             nCC(conq), regD, imm5, dis_buf);
        return True;
    }
    /* fall through */
  }

   /* ----------- ssat16<c> <Rd>,#<imm>,<Rn> ----------- */
   {
     UInt regD = 99, regN = 99, sat_imm = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,6) == BITS10(1,1,1,1,0,0,1,1,0,0)
            && INSNT0(5,4) == BITS2(1,0)
            && INSNT1(15,12) == BITS4(0,0,0,0)
            && INSNT1(7,4) == BITS4(0,0,0,0)) {
           regD       = INSNT1(11,8);
           regN       = INSNT0(3,0);
           sat_imm    = INSNT1(3,0) + 1;
           if (!isBadRegT(regD) && !isBadRegT(regN))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,1,0,1,0) &&
            INSNA(11,4)   == BITS8(1,1,1,1,0,0,1,1)) {
           regD       = INSNA(15,12);
           regN       = INSNA(3,0);
           sat_imm    = INSNA(19,16) + 1;
           if (regD != 15 && regN != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN    = newTemp(Ity_I32);
        IRTemp irt_regN_lo = newTemp(Ity_I32);
        IRTemp irt_regN_hi = newTemp(Ity_I32);
        IRTemp irt_Q_lo    = newTemp(Ity_I32);
        IRTemp irt_Q_hi    = newTemp(Ity_I32);
        IRTemp irt_res_lo  = newTemp(Ity_I32);
        IRTemp irt_res_hi  = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regN_lo,
                binop( Iop_Sar32,
                       binop(Iop_Shl32, mkexpr(irt_regN), mkU8(16)),
                       mkU8(16)) );
        assign( irt_regN_hi, binop(Iop_Sar32, mkexpr(irt_regN), mkU8(16)) );

        armSignedSatQ( irt_regN_lo, sat_imm, &irt_res_lo, &irt_Q_lo );
        or_into_QFLAG32( mkexpr(irt_Q_lo), condT );

        armSignedSatQ( irt_regN_hi, sat_imm, &irt_res_hi, &irt_Q_hi );
        or_into_QFLAG32( mkexpr(irt_Q_hi), condT );

        IRExpr* ire_result 
           = binop(Iop_Or32, 
                   binop(Iop_And32, mkexpr(irt_res_lo), mkU32(0xFFFF)),
                   binop(Iop_Shl32, mkexpr(irt_res_hi), mkU8(16)));
        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "ssat16%s r%u, #0x%04x, r%u\n", nCC(conq), regD, sat_imm, regN );
        return True;
     }
     /* fall through */
   }

   /* -------------- usat16<c> <Rd>,#<imm4>,<Rn> --------------- */
   {
     UInt regD = 99, regN = 99, sat_imm = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xF3A && (INSNT1(15,0) & 0xF0F0) == 0x0000) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           sat_imm = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN))
              gate = True;
       }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,1,1,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,1,1)) {
           regD    = INSNA(15,12);
           regN    = INSNA(3,0);
           sat_imm = INSNA(19,16);
           if (regD != 15 && regN != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN    = newTemp(Ity_I32);
        IRTemp irt_regN_lo = newTemp(Ity_I32);
        IRTemp irt_regN_hi = newTemp(Ity_I32);
        IRTemp irt_Q_lo    = newTemp(Ity_I32);
        IRTemp irt_Q_hi    = newTemp(Ity_I32);
        IRTemp irt_res_lo  = newTemp(Ity_I32);
        IRTemp irt_res_hi  = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regN_lo, binop( Iop_Sar32, 
                                    binop(Iop_Shl32, mkexpr(irt_regN), mkU8(16)), 
                                    mkU8(16)) );
        assign( irt_regN_hi, binop(Iop_Sar32, mkexpr(irt_regN), mkU8(16)) );

        armUnsignedSatQ( &irt_res_lo, &irt_Q_lo, irt_regN_lo, sat_imm );
        or_into_QFLAG32( mkexpr(irt_Q_lo), condT );

        armUnsignedSatQ( &irt_res_hi, &irt_Q_hi, irt_regN_hi, sat_imm );
        or_into_QFLAG32( mkexpr(irt_Q_hi), condT );

        IRExpr* ire_result = binop( Iop_Or32, 
                                    binop(Iop_Shl32, mkexpr(irt_res_hi), mkU8(16)),
                                    mkexpr(irt_res_lo) );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "usat16%s r%u, #0x%04x, r%u\n", nCC(conq), regD, sat_imm, regN );
        return True;
     }
     /* fall through */
   }

   /* -------------- uadd16<c> <Rd>,<Rn>,<Rm> -------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA9 && (INSNT1(15,0) & 0xF0F0) == 0xF040) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,0,1) && 
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt  = newTemp(Ity_I32);
        IRTemp rMt  = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);
        IRTemp reso = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res, binop(Iop_Add16x2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        assign(reso, binop(Iop_HAdd16Ux2, mkexpr(rNt), mkexpr(rMt)));
        set_GE_32_10_from_bits_31_15(reso, condT);

        DIP("uadd16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* -------------- sadd16<c> <Rd>,<Rn>,<Rm> -------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA9 && (INSNT1(15,0) & 0xF0F0) == 0xF000) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,0,1) && 
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt  = newTemp(Ity_I32);
        IRTemp rMt  = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);
        IRTemp reso = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res, binop(Iop_Add16x2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        assign(reso, unop(Iop_Not32,
                          binop(Iop_HAdd16Sx2, mkexpr(rNt), mkexpr(rMt))));
        set_GE_32_10_from_bits_31_15(reso, condT);

        DIP("sadd16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ---------------- usub16<c> <Rd>,<Rn>,<Rm> ---------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAD && (INSNT1(15,0) & 0xF0F0) == 0xF040) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,0,1) && 
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
             gate = True;
        }
     }

     if (gate) {
        IRTemp rNt  = newTemp(Ity_I32);
        IRTemp rMt  = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);
        IRTemp reso = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res, binop(Iop_Sub16x2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        assign(reso, unop(Iop_Not32,
                          binop(Iop_HSub16Ux2, mkexpr(rNt), mkexpr(rMt))));
        set_GE_32_10_from_bits_31_15(reso, condT);

        DIP("usub16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* -------------- ssub16<c> <Rd>,<Rn>,<Rm> -------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAD && (INSNT1(15,0) & 0xF0F0) == 0xF000) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,0,1) && 
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt  = newTemp(Ity_I32);
        IRTemp rMt  = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);
        IRTemp reso = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res, binop(Iop_Sub16x2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        assign(reso, unop(Iop_Not32,
                          binop(Iop_HSub16Sx2, mkexpr(rNt), mkexpr(rMt))));
        set_GE_32_10_from_bits_31_15(reso, condT);

        DIP("ssub16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- uadd8<c> <Rd>,<Rn>,<Rm> ---------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF040) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,0,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            (INSNA(7,4)  == BITS4(1,0,0,1))) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt  = newTemp(Ity_I32);
        IRTemp rMt  = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);
        IRTemp reso = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res, binop(Iop_Add8x4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        assign(reso, binop(Iop_HAdd8Ux4, mkexpr(rNt), mkexpr(rMt)));
        set_GE_3_2_1_0_from_bits_31_23_15_7(reso, condT);

        DIP("uadd8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------- sadd8<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF000) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,0,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            (INSNA(7,4)  == BITS4(1,0,0,1))) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt  = newTemp(Ity_I32);
        IRTemp rMt  = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);
        IRTemp reso = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res, binop(Iop_Add8x4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        assign(reso, unop(Iop_Not32,
                          binop(Iop_HAdd8Sx4, mkexpr(rNt), mkexpr(rMt))));
        set_GE_3_2_1_0_from_bits_31_23_15_7(reso, condT);

        DIP("sadd8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------- usub8<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAC && (INSNT1(15,0) & 0xF0F0) == 0xF040) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,0,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            (INSNA(7,4)  == BITS4(1,1,1,1))) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
             gate = True;
        }
     }

     if (gate) {
        IRTemp rNt  = newTemp(Ity_I32);
        IRTemp rMt  = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);
        IRTemp reso = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res, binop(Iop_Sub8x4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        assign(reso, unop(Iop_Not32,
                          binop(Iop_HSub8Ux4, mkexpr(rNt), mkexpr(rMt))));
        set_GE_3_2_1_0_from_bits_31_23_15_7(reso, condT);

        DIP("usub8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------- ssub8<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAC && (INSNT1(15,0) & 0xF0F0) == 0xF000) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,0,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(1,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt  = newTemp(Ity_I32);
        IRTemp rMt  = newTemp(Ity_I32);
        IRTemp res  = newTemp(Ity_I32);
        IRTemp reso = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res, binop(Iop_Sub8x4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res), condT );
        else
           putIRegA( regD, mkexpr(res), condT, Ijk_Boring );

        assign(reso, unop(Iop_Not32,
                          binop(Iop_HSub8Sx4, mkexpr(rNt), mkexpr(rMt))));
        set_GE_3_2_1_0_from_bits_31_23_15_7(reso, condT);

        DIP("ssub8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------ qadd8<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF010) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(1,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QAdd8Sx4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("qadd8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------ qsub8<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAC && (INSNT1(15,0) & 0xF0F0) == 0xF010) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(1,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QSub8Sx4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("qsub8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------ uqadd8<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF050) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            (INSNA(7,4)  == BITS4(1,0,0,1))) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QAdd8Ux4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uqadd8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------ uqsub8<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAC && (INSNT1(15,0) & 0xF0F0) == 0xF050) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            (INSNA(7,4)  == BITS4(1,1,1,1))) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
             gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QSub8Ux4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uqsub8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- uhadd8<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF060) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(1,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_HAdd8Ux4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uhadd8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- uhadd16<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA9 && (INSNT1(15,0) & 0xF0F0) == 0xF060) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_HAdd16Ux2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uhadd16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- shadd8<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF020) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(1,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_HAdd8Sx4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("shadd8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------ qadd16<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA9 && (INSNT1(15,0) & 0xF0F0) == 0xF010) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QAdd16Sx2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("qadd16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------ qsub16<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

      if (isT) {
        if (INSNT0(15,4) == 0xFAD && (INSNT1(15,0) & 0xF0F0) == 0xF010) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
             gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QSub16Sx2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("qsub16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------- qsax<c> <Rd>,<Rn>,<Rm> ------------------- */
   /* note: the hardware seems to construct the result differently
      from wot the manual says. */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAE && (INSNT1(15,0) & 0xF0F0) == 0xF010) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN     = newTemp(Ity_I32);
        IRTemp irt_regM     = newTemp(Ity_I32);
        IRTemp irt_sum      = newTemp(Ity_I32);
        IRTemp irt_diff     = newTemp(Ity_I32);
        IRTemp irt_sum_res  = newTemp(Ity_I32);
        IRTemp irt_diff_res = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_diff, 
                binop( Iop_Sub32, 
                       binop( Iop_Sar32, mkexpr(irt_regN), mkU8(16) ),
                       binop( Iop_Sar32, 
                              binop(Iop_Shl32, mkexpr(irt_regM), mkU8(16)), 
                              mkU8(16) ) ) );
        armSignedSatQ( irt_diff, 0x10, &irt_diff_res, NULL);

        assign( irt_sum, 
                binop( Iop_Add32, 
                       binop( Iop_Sar32, 
                              binop( Iop_Shl32, mkexpr(irt_regN), mkU8(16) ), 
                              mkU8(16) ), 
                       binop( Iop_Sar32, mkexpr(irt_regM), mkU8(16) )) );
        armSignedSatQ( irt_sum, 0x10, &irt_sum_res, NULL );

        IRExpr* ire_result = binop( Iop_Or32, 
                                    binop( Iop_Shl32, mkexpr(irt_diff_res), 
                                           mkU8(16) ), 
                                    binop( Iop_And32, mkexpr(irt_sum_res), 
                                           mkU32(0xFFFF)) );

        if (isT) 
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "qsax%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ------------------- qasx<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAA && (INSNT1(15,0) & 0xF0F0) == 0xF010) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN     = newTemp(Ity_I32);
        IRTemp irt_regM     = newTemp(Ity_I32);
        IRTemp irt_sum      = newTemp(Ity_I32);
        IRTemp irt_diff     = newTemp(Ity_I32);
        IRTemp irt_res_sum  = newTemp(Ity_I32);
        IRTemp irt_res_diff = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_diff,  
                binop( Iop_Sub32, 
                       binop( Iop_Sar32, 
                              binop( Iop_Shl32, mkexpr(irt_regN), mkU8(16) ), 
                              mkU8(16) ), 
                       binop( Iop_Sar32, mkexpr(irt_regM), mkU8(16) ) ) );
        armSignedSatQ( irt_diff, 0x10, &irt_res_diff, NULL );

        assign( irt_sum, 
                binop( Iop_Add32, 
                       binop( Iop_Sar32, mkexpr(irt_regN), mkU8(16) ), 
                       binop( Iop_Sar32, 
                              binop( Iop_Shl32, mkexpr(irt_regM), mkU8(16) ), 
                              mkU8(16) ) ) );
        armSignedSatQ( irt_sum, 0x10, &irt_res_sum, NULL );
       
        IRExpr* ire_result 
          = binop( Iop_Or32, 
                   binop( Iop_Shl32, mkexpr(irt_res_sum), mkU8(16) ), 
                   binop( Iop_And32, mkexpr(irt_res_diff), mkU32(0xFFFF) ) );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "qasx%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ------------------- sasx<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAA && (INSNT1(15,0) & 0xF0F0) == 0xF000) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,0,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN = newTemp(Ity_I32);
        IRTemp irt_regM = newTemp(Ity_I32);
        IRTemp irt_sum  = newTemp(Ity_I32);
        IRTemp irt_diff = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_diff,  
                binop( Iop_Sub32, 
                       binop( Iop_Sar32, 
                              binop( Iop_Shl32, mkexpr(irt_regN), mkU8(16) ), 
                              mkU8(16) ), 
                       binop( Iop_Sar32, mkexpr(irt_regM), mkU8(16) ) ) );

        assign( irt_sum, 
                binop( Iop_Add32, 
                       binop( Iop_Sar32, mkexpr(irt_regN), mkU8(16) ), 
                       binop( Iop_Sar32, 
                              binop( Iop_Shl32, mkexpr(irt_regM), mkU8(16) ), 
                              mkU8(16) ) ) );
       
        IRExpr* ire_result 
          = binop( Iop_Or32, 
                   binop( Iop_Shl32, mkexpr(irt_sum), mkU8(16) ), 
                   binop( Iop_And32, mkexpr(irt_diff), mkU32(0xFFFF) ) );

        IRTemp ge10 = newTemp(Ity_I32);
        assign(ge10, unop(Iop_Not32, mkexpr(irt_diff)));
        put_GEFLAG32( 0, 31, mkexpr(ge10), condT );
        put_GEFLAG32( 1, 31, mkexpr(ge10), condT );

        IRTemp ge32 = newTemp(Ity_I32);
        assign(ge32, unop(Iop_Not32, mkexpr(irt_sum)));
        put_GEFLAG32( 2, 31, mkexpr(ge32), condT );
        put_GEFLAG32( 3, 31, mkexpr(ge32), condT );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "sasx%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* --------------- smuad, smuadx<c><Rd>,<Rn>,<Rm> --------------- */
   /* --------------- smsad, smsadx<c><Rd>,<Rn>,<Rm> --------------- */
   {
     UInt regD = 99, regN = 99, regM = 99, bitM = 99;
     Bool gate = False, isAD = False;

     if (isT) {
        if ((INSNT0(15,4) == 0xFB2 || INSNT0(15,4) == 0xFB4)
            && (INSNT1(15,0) & 0xF0E0) == 0xF000) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           bitM = INSNT1(4,4);
           isAD = INSNT0(15,4) == 0xFB2;
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,1,0,0,0,0) &&
            INSNA(15,12) == BITS4(1,1,1,1)         &&
            (INSNA(7,4) & BITS4(1,0,0,1)) == BITS4(0,0,0,1) ) {
           regD = INSNA(19,16);
           regN = INSNA(3,0);
           regM = INSNA(11,8);
           bitM = INSNA(5,5);
           isAD = INSNA(6,6) == 0;
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN    = newTemp(Ity_I32);
        IRTemp irt_regM    = newTemp(Ity_I32);
        IRTemp irt_prod_lo = newTemp(Ity_I32);
        IRTemp irt_prod_hi = newTemp(Ity_I32);
        IRTemp tmpM        = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );

        assign( tmpM, isT ? getIRegT(regM) : getIRegA(regM) );
        assign( irt_regM, genROR32(tmpM, (bitM & 1) ? 16 : 0) );

        assign( irt_prod_lo, 
                binop( Iop_Mul32, 
                       binop( Iop_Sar32, 
                              binop(Iop_Shl32, mkexpr(irt_regN), mkU8(16)), 
                              mkU8(16) ), 
                       binop( Iop_Sar32, 
                              binop(Iop_Shl32, mkexpr(irt_regM), mkU8(16)), 
                              mkU8(16) ) ) );
        assign( irt_prod_hi, binop(Iop_Mul32, 
                                   binop(Iop_Sar32, mkexpr(irt_regN), mkU8(16)), 
                                   binop(Iop_Sar32, mkexpr(irt_regM), mkU8(16))) );
        IRExpr* ire_result 
           = binop( isAD ? Iop_Add32 : Iop_Sub32,
                    mkexpr(irt_prod_lo), mkexpr(irt_prod_hi) );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        if (isAD) {
           or_into_QFLAG32(
              signed_overflow_after_Add32( ire_result,
                                           irt_prod_lo, irt_prod_hi ),
              condT
           );
        }

        DIP("smu%cd%s%s r%u, r%u, r%u\n",
            isAD ? 'a' : 's',
            bitM ? "x" : "", nCC(conq), regD, regN, regM);
        return True;
     }
     /* fall through */
   }

   /* --------------- smlad{X}<c> <Rd>,<Rn>,<Rm>,<Ra> -------------- */
   /* --------------- smlsd{X}<c> <Rd>,<Rn>,<Rm>,<Ra> -------------- */
   {
     UInt regD = 99, regN = 99, regM = 99, regA = 99, bitM = 99;
     Bool gate = False, isAD = False;

     if (isT) {
       if ((INSNT0(15,4) == 0xFB2 || INSNT0(15,4) == 0xFB4)
           && INSNT1(7,5) == BITS3(0,0,0)) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           regA = INSNT1(15,12);
           bitM = INSNT1(4,4);
           isAD = INSNT0(15,4) == 0xFB2;
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM)
               && !isBadRegT(regA))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,1,0,0,0,0) &&
            (INSNA(7,4) & BITS4(1,0,0,1)) == BITS4(0,0,0,1)) {
           regD = INSNA(19,16);
           regA = INSNA(15,12);
           regN = INSNA(3,0);
           regM = INSNA(11,8);
           bitM = INSNA(5,5);
           isAD = INSNA(6,6) == 0;
           if (regD != 15 && regN != 15 && regM != 15 && regA != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN    = newTemp(Ity_I32);
        IRTemp irt_regM    = newTemp(Ity_I32);
        IRTemp irt_regA    = newTemp(Ity_I32);
        IRTemp irt_prod_lo = newTemp(Ity_I32);
        IRTemp irt_prod_hi = newTemp(Ity_I32);
        IRTemp irt_sum     = newTemp(Ity_I32);
        IRTemp tmpM        = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regA, isT ? getIRegT(regA) : getIRegA(regA) );

        assign( tmpM, isT ? getIRegT(regM) : getIRegA(regM) );
        assign( irt_regM, genROR32(tmpM, (bitM & 1) ? 16 : 0) );

        assign( irt_prod_lo, 
                binop(Iop_Mul32, 
                      binop(Iop_Sar32, 
                            binop( Iop_Shl32, mkexpr(irt_regN), mkU8(16) ), 
                            mkU8(16)), 
                      binop(Iop_Sar32, 
                            binop( Iop_Shl32, mkexpr(irt_regM), mkU8(16) ), 
                            mkU8(16))) );
        assign( irt_prod_hi, 
                binop( Iop_Mul32, 
                       binop( Iop_Sar32, mkexpr(irt_regN), mkU8(16) ), 
                       binop( Iop_Sar32, mkexpr(irt_regM), mkU8(16) ) ) );
        assign( irt_sum, binop( isAD ? Iop_Add32 : Iop_Sub32, 
                                mkexpr(irt_prod_lo), mkexpr(irt_prod_hi) ) );

        IRExpr* ire_result = binop(Iop_Add32, mkexpr(irt_sum), mkexpr(irt_regA));

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        if (isAD) {
           or_into_QFLAG32(
              signed_overflow_after_Add32( mkexpr(irt_sum),
                                           irt_prod_lo, irt_prod_hi ),
              condT
           );
        }

        or_into_QFLAG32(
           signed_overflow_after_Add32( ire_result, irt_sum, irt_regA ),
           condT
        );

        DIP("sml%cd%s%s r%u, r%u, r%u, r%u\n",
            isAD ? 'a' : 's',
            bitM ? "x" : "", nCC(conq), regD, regN, regM, regA);
        return True;
     }
     /* fall through */
   }

   /* ----- smlabb, smlabt, smlatb, smlatt <Rd>,<Rn>,<Rm>,<Ra> ----- */
   {
     UInt regD = 99, regN = 99, regM = 99, regA = 99, bitM = 99, bitN = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFB1 && INSNT1(7,6) == BITS2(0,0)) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           regA = INSNT1(15,12);
           bitM = INSNT1(4,4);
           bitN = INSNT1(5,5);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM)
               && !isBadRegT(regA))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,0,0,1,0,0,0,0) &&
            (INSNA(7,4) & BITS4(1,0,0,1)) == BITS4(1,0,0,0)) {
           regD = INSNA(19,16);
           regN = INSNA(3,0);
           regM = INSNA(11,8);
           regA = INSNA(15,12);
           bitM = INSNA(6,6);
           bitN = INSNA(5,5);
           if (regD != 15 && regN != 15 && regM != 15 && regA != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regA = newTemp(Ity_I32);
        IRTemp irt_prod = newTemp(Ity_I32);

        assign( irt_prod, 
                binop(Iop_Mul32, 
                      binop(Iop_Sar32, 
                            binop(Iop_Shl32,
                                  isT ? getIRegT(regN) : getIRegA(regN),
                                  mkU8(bitN ? 0 : 16)),
                            mkU8(16)), 
                      binop(Iop_Sar32, 
                            binop(Iop_Shl32,
                                  isT ? getIRegT(regM) : getIRegA(regM),
                                  mkU8(bitM ? 0 : 16)), 
                            mkU8(16))) );

        assign( irt_regA, isT ? getIRegT(regA) : getIRegA(regA) );

        IRExpr* ire_result = binop(Iop_Add32, mkexpr(irt_prod), mkexpr(irt_regA));

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        or_into_QFLAG32(
           signed_overflow_after_Add32( ire_result, irt_prod, irt_regA ),
           condT
        );

        DIP( "smla%c%c%s r%u, r%u, r%u, r%u\n", 
             bitN ? 't' : 'b', bitM ? 't' : 'b', 
             nCC(conq), regD, regN, regM, regA );
        return True;
     }
     /* fall through */
   }

   /* ----- smlalbb, smlalbt, smlaltb, smlaltt <Rd>,<Rn>,<Rm>,<Ra> ----- */
   {
     UInt regDHi = 99, regN = 99, regM = 99, regDLo = 99, bitM = 99, bitN = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFBC && INSNT1(7,6) == BITS2(1,0)) {
           regN   = INSNT0(3,0);
           regDHi = INSNT1(11,8);
           regM   = INSNT1(3,0);
           regDLo = INSNT1(15,12);
           bitM   = INSNT1(4,4);
           bitN   = INSNT1(5,5);
           if (!isBadRegT(regDHi) && !isBadRegT(regN) && !isBadRegT(regM)
               && !isBadRegT(regDLo) && regDHi != regDLo)
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,0,0,1,0,1,0,0) &&
            (INSNA(7,4) & BITS4(1,0,0,1)) == BITS4(1,0,0,0)) {
           regDHi = INSNA(19,16);
           regN   = INSNA(3,0);
           regM   = INSNA(11,8);
           regDLo = INSNA(15,12);
           bitM   = INSNA(6,6);
           bitN   = INSNA(5,5);
           if (regDHi != 15 && regN != 15 && regM != 15 && regDLo != 15 &&
               regDHi != regDLo)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regD  = newTemp(Ity_I64);
        IRTemp irt_prod  = newTemp(Ity_I64);
        IRTemp irt_res   = newTemp(Ity_I64);
        IRTemp irt_resHi = newTemp(Ity_I32);
        IRTemp irt_resLo = newTemp(Ity_I32);

        assign( irt_prod,
                binop(Iop_MullS32,
                      binop(Iop_Sar32,
                            binop(Iop_Shl32,
                                  isT ? getIRegT(regN) : getIRegA(regN),
                                  mkU8(bitN ? 0 : 16)),
                            mkU8(16)),
                      binop(Iop_Sar32,
                            binop(Iop_Shl32,
                                  isT ? getIRegT(regM) : getIRegA(regM),
                                  mkU8(bitM ? 0 : 16)),
                            mkU8(16))) );

        assign( irt_regD, binop(Iop_32HLto64,
                                isT ? getIRegT(regDHi) : getIRegA(regDHi),
                                isT ? getIRegT(regDLo) : getIRegA(regDLo)) );
        assign( irt_res, binop(Iop_Add64, mkexpr(irt_regD), mkexpr(irt_prod)) );
        assign( irt_resHi, unop(Iop_64HIto32, mkexpr(irt_res)) );
        assign( irt_resLo, unop(Iop_64to32, mkexpr(irt_res)) );

        if (isT) {
           putIRegT( regDHi, mkexpr(irt_resHi), condT );
           putIRegT( regDLo, mkexpr(irt_resLo), condT );
        } else {
           putIRegA( regDHi, mkexpr(irt_resHi), condT, Ijk_Boring );
           putIRegA( regDLo, mkexpr(irt_resLo), condT, Ijk_Boring );
        }

        DIP( "smlal%c%c%s r%u, r%u, r%u, r%u\n",
             bitN ? 't' : 'b', bitM ? 't' : 'b',
             nCC(conq), regDHi, regN, regM, regDLo );
        return True;
     }
     /* fall through */
   }

   /* ----- smlawb, smlawt <Rd>,<Rn>,<Rm>,<Ra> ----- */
   {
     UInt regD = 99, regN = 99, regM = 99, regA = 99, bitM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFB3 && INSNT1(7,5) == BITS3(0,0,0)) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           regA = INSNT1(15,12);
           bitM = INSNT1(4,4);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM)
               && !isBadRegT(regA))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,0,0,1,0,0,1,0) &&
            (INSNA(7,4) & BITS4(1,0,1,1)) == BITS4(1,0,0,0)) {
           regD = INSNA(19,16);
           regN = INSNA(3,0);
           regM = INSNA(11,8);
           regA = INSNA(15,12);
           bitM = INSNA(6,6);
           if (regD != 15 && regN != 15 && regM != 15 && regA != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regA = newTemp(Ity_I32);
        IRTemp irt_prod = newTemp(Ity_I64);

        assign( irt_prod, 
                binop(Iop_MullS32, 
                      isT ? getIRegT(regN) : getIRegA(regN),
                      binop(Iop_Sar32, 
                            binop(Iop_Shl32,
                                  isT ? getIRegT(regM) : getIRegA(regM),
                                  mkU8(bitM ? 0 : 16)), 
                            mkU8(16))) );

        assign( irt_regA, isT ? getIRegT(regA) : getIRegA(regA) );

        IRTemp prod32 = newTemp(Ity_I32);
        assign(prod32,
               binop(Iop_Or32,
                     binop(Iop_Shl32, unop(Iop_64HIto32, mkexpr(irt_prod)), mkU8(16)),
                     binop(Iop_Shr32, unop(Iop_64to32, mkexpr(irt_prod)), mkU8(16))
        ));

        IRExpr* ire_result = binop(Iop_Add32, mkexpr(prod32), mkexpr(irt_regA));

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        or_into_QFLAG32(
           signed_overflow_after_Add32( ire_result, prod32, irt_regA ),
           condT
        );

        DIP( "smlaw%c%s r%u, r%u, r%u, r%u\n", 
             bitM ? 't' : 'b', 
             nCC(conq), regD, regN, regM, regA );
        return True;
     }
     /* fall through */
   }

   /* ------------------- sel<c> <Rd>,<Rn>,<Rm> -------------------- */
   /* fixme: fix up the test in v6media.c so that we can pass the ge
      flags as part of the test. */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAA && (INSNT1(15,0) & 0xF0F0) == 0xF080) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,1,0,0,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(1,0,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_ge_flag0 = newTemp(Ity_I32);
        IRTemp irt_ge_flag1 = newTemp(Ity_I32);
        IRTemp irt_ge_flag2 = newTemp(Ity_I32);
        IRTemp irt_ge_flag3 = newTemp(Ity_I32);

        assign( irt_ge_flag0, get_GEFLAG32(0) );
        assign( irt_ge_flag1, get_GEFLAG32(1) );
        assign( irt_ge_flag2, get_GEFLAG32(2) );
        assign( irt_ge_flag3, get_GEFLAG32(3) );

        IRExpr* ire_ge_flag0_or 
          = binop(Iop_Or32, mkexpr(irt_ge_flag0), 
                  binop(Iop_Sub32, mkU32(0), mkexpr(irt_ge_flag0)));
        IRExpr* ire_ge_flag1_or 
          = binop(Iop_Or32, mkexpr(irt_ge_flag1), 
                  binop(Iop_Sub32, mkU32(0), mkexpr(irt_ge_flag1)));
        IRExpr* ire_ge_flag2_or 
          = binop(Iop_Or32, mkexpr(irt_ge_flag2), 
                  binop(Iop_Sub32, mkU32(0), mkexpr(irt_ge_flag2)));
        IRExpr* ire_ge_flag3_or 
          = binop(Iop_Or32, mkexpr(irt_ge_flag3), 
                  binop(Iop_Sub32, mkU32(0), mkexpr(irt_ge_flag3)));

        IRExpr* ire_ge_flags 
          = binop( Iop_Or32, 
                   binop(Iop_Or32, 
                         binop(Iop_And32, 
                               binop(Iop_Sar32, ire_ge_flag0_or, mkU8(31)), 
                               mkU32(0x000000ff)), 
                         binop(Iop_And32, 
                               binop(Iop_Sar32, ire_ge_flag1_or, mkU8(31)), 
                               mkU32(0x0000ff00))), 
                   binop(Iop_Or32, 
                         binop(Iop_And32, 
                               binop(Iop_Sar32, ire_ge_flag2_or, mkU8(31)), 
                               mkU32(0x00ff0000)), 
                         binop(Iop_And32, 
                               binop(Iop_Sar32, ire_ge_flag3_or, mkU8(31)), 
                               mkU32(0xff000000))) );

        IRExpr* ire_result 
          = binop(Iop_Or32, 
                  binop(Iop_And32,
                        isT ? getIRegT(regN) : getIRegA(regN),
                        ire_ge_flags ), 
                  binop(Iop_And32,
                        isT ? getIRegT(regM) : getIRegA(regM),
                        unop(Iop_Not32, ire_ge_flags)));

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP("sel%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ----------------- uxtab16<c> Rd,Rn,Rm{,rot} ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99, rotate = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA3 && (INSNT1(15,0) & 0xF0C0) == 0xF080) {
           regN   = INSNT0(3,0);
           regD   = INSNT1(11,8);
           regM   = INSNT1(3,0);
           rotate = INSNT1(5,4);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,1,1,0,0) &&
            INSNA(9,4)   == BITS6(0,0,0,1,1,1) ) {
           regD   = INSNA(15,12);
           regN   = INSNA(19,16);
           regM   = INSNA(3,0);
           rotate = INSNA(11,10);
           if (regD != 15 && regN != 15 && regM != 15)
             gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN = newTemp(Ity_I32);
        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );

        IRTemp irt_regM = newTemp(Ity_I32);
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        IRTemp irt_rot = newTemp(Ity_I32);
        assign( irt_rot, binop(Iop_And32,
                               genROR32(irt_regM, 8 * rotate),
                               mkU32(0x00FF00FF)) );

        IRExpr* resLo
           = binop(Iop_And32,
                   binop(Iop_Add32, mkexpr(irt_regN), mkexpr(irt_rot)),
                   mkU32(0x0000FFFF));

        IRExpr* resHi
           = binop(Iop_Add32, 
                   binop(Iop_And32, mkexpr(irt_regN), mkU32(0xFFFF0000)),
                   binop(Iop_And32, mkexpr(irt_rot),  mkU32(0xFFFF0000)));

        IRExpr* ire_result 
           = binop( Iop_Or32, resHi, resLo );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "uxtab16%s r%u, r%u, r%u, ROR #%u\n", 
             nCC(conq), regD, regN, regM, 8 * rotate );
        return True;
     }
     /* fall through */
   }

   /* --------------- usad8  Rd,Rn,Rm    ---------------- */
   /* --------------- usada8 Rd,Rn,Rm,Ra ---------------- */
   {
     UInt rD = 99, rN = 99, rM = 99, rA = 99;
     Bool gate = False;

     if (isT) {
       if (INSNT0(15,4) == 0xFB7 && INSNT1(7,4) == BITS4(0,0,0,0)) {
           rN = INSNT0(3,0);
           rA = INSNT1(15,12);
           rD = INSNT1(11,8);
           rM = INSNT1(3,0);
           if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM) && rA != 13)
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,1,1,0,0,0) &&
            INSNA(7,4)   == BITS4(0,0,0,1) ) {
           rD = INSNA(19,16);
           rA = INSNA(15,12);
           rM = INSNA(11,8);
           rN = INSNA(3,0);
           if (rD != 15 && rN != 15 && rM != 15 /* but rA can be 15 */)
              gate = True;
        }
     }
     /* We allow rA == 15, to denote the usad8 (no accumulator) case. */

     if (gate) {
        IRExpr* rNe = isT ? getIRegT(rN) : getIRegA(rN);
        IRExpr* rMe = isT ? getIRegT(rM) : getIRegA(rM);
        IRExpr* rAe = rA == 15 ? mkU32(0)
                               : (isT ? getIRegT(rA) : getIRegA(rA)); 
        IRExpr* res = binop(Iop_Add32,
                            binop(Iop_Sad8Ux4, rNe, rMe),
                            rAe);
        if (isT)
           putIRegT( rD, res, condT );
        else
           putIRegA( rD, res, condT, Ijk_Boring );

        if (rA == 15) {
           DIP( "usad8%s r%u, r%u, r%u\n", 
                nCC(conq), rD, rN, rM );
        } else {
           DIP( "usada8%s r%u, r%u, r%u, r%u\n", 
                nCC(conq), rD, rN, rM, rA );
        }
        return True;
     }
     /* fall through */
   }

   /* ------------------ qadd<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF080) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,0,0,1,0,0,0,0) &&
            INSNA(11,8)  == BITS4(0,0,0,0)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QAdd32S, mkexpr(rMt), mkexpr(rNt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        or_into_QFLAG32(
           signed_overflow_after_Add32(
              binop(Iop_Add32, mkexpr(rMt), mkexpr(rNt)), rMt, rNt),
           condT
        );

        DIP("qadd%s r%u, r%u, r%u\n", nCC(conq),regD,regM,regN);
        return True;
     }
     /* fall through */
   }

   /* ------------------ qdadd<c> <Rd>,<Rm>,<Rn> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF090) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,0,0,1,0,1,0,0) &&
            INSNA(11,8)  == BITS4(0,0,0,0)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp rN_d  = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        or_into_QFLAG32(
           signed_overflow_after_Add32(
              binop(Iop_Add32, mkexpr(rNt), mkexpr(rNt)), rNt, rNt),
           condT
        );

        assign(rN_d,  binop(Iop_QAdd32S, mkexpr(rNt), mkexpr(rNt)));
        assign(res_q, binop(Iop_QAdd32S, mkexpr(rMt), mkexpr(rN_d)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        or_into_QFLAG32(
           signed_overflow_after_Add32(
              binop(Iop_Add32, mkexpr(rMt), mkexpr(rN_d)), rMt, rN_d),
           condT
        );

        DIP("qdadd%s r%u, r%u, r%u\n", nCC(conq),regD,regM,regN);
        return True;
     }
     /* fall through */
   }

   /* ------------------ qsub<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF0A0) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,0,0,1,0,0,1,0) &&
            INSNA(11,8)  == BITS4(0,0,0,0)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QSub32S, mkexpr(rMt), mkexpr(rNt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        or_into_QFLAG32(
           signed_overflow_after_Sub32(
              binop(Iop_Sub32, mkexpr(rMt), mkexpr(rNt)), rMt, rNt),
           condT
        );

        DIP("qsub%s r%u, r%u, r%u\n", nCC(conq),regD,regM,regN);
        return True;
     }
     /* fall through */
   }

   /* ------------------ qdsub<c> <Rd>,<Rm>,<Rn> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA8 && (INSNT1(15,0) & 0xF0F0) == 0xF0B0) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,0,0,1,0,1,1,0) &&
            INSNA(11,8)  == BITS4(0,0,0,0)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp rN_d  = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        or_into_QFLAG32(
           signed_overflow_after_Add32(
              binop(Iop_Add32, mkexpr(rNt), mkexpr(rNt)), rNt, rNt),
           condT
        );

        assign(rN_d,  binop(Iop_QAdd32S, mkexpr(rNt), mkexpr(rNt)));
        assign(res_q, binop(Iop_QSub32S, mkexpr(rMt), mkexpr(rN_d)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        or_into_QFLAG32(
           signed_overflow_after_Sub32(
              binop(Iop_Sub32, mkexpr(rMt), mkexpr(rN_d)), rMt, rN_d),
           condT
        );

        DIP("qdsub%s r%u, r%u, r%u\n", nCC(conq),regD,regM,regN);
        return True;
     }
     /* fall through */
   }

   /* ------------------ uqsub16<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAD && (INSNT1(15,0) & 0xF0F0) == 0xF050) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
             gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QSub16Ux2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uqsub16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- shadd16<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA9 && (INSNT1(15,0) & 0xF0F0) == 0xF020) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_HAdd16Sx2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("shadd16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- uhsub8<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAC && (INSNT1(15,0) & 0xF0F0) == 0xF060) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(1,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_HSub8Ux4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uhsub8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- uhsub16<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAD && (INSNT1(15,0) & 0xF0F0) == 0xF060) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_HSub16Ux2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uhsub16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------ uqadd16<c> <Rd>,<Rn>,<Rm> ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA9 && (INSNT1(15,0) & 0xF0F0) == 0xF050) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_QAdd16Ux2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uqadd16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ------------------- uqsax<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAE && (INSNT1(15,0) & 0xF0F0) == 0xF050) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN     = newTemp(Ity_I32);
        IRTemp irt_regM     = newTemp(Ity_I32);
        IRTemp irt_sum      = newTemp(Ity_I32);
        IRTemp irt_diff     = newTemp(Ity_I32);
        IRTemp irt_sum_res  = newTemp(Ity_I32);
        IRTemp irt_diff_res = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_diff, 
                binop( Iop_Sub32, 
                       binop( Iop_Shr32, mkexpr(irt_regN), mkU8(16) ),
                       binop( Iop_Shr32, 
                              binop(Iop_Shl32, mkexpr(irt_regM), mkU8(16)), 
                              mkU8(16) ) ) );
        armUnsignedSatQ( &irt_diff_res, NULL, irt_diff, 0x10);

        assign( irt_sum, 
                binop( Iop_Add32, 
                       binop( Iop_Shr32, 
                              binop( Iop_Shl32, mkexpr(irt_regN), mkU8(16) ), 
                              mkU8(16) ), 
                       binop( Iop_Shr32, mkexpr(irt_regM), mkU8(16) )) );
        armUnsignedSatQ( &irt_sum_res, NULL, irt_sum, 0x10 );

        IRExpr* ire_result = binop( Iop_Or32, 
                                    binop( Iop_Shl32, mkexpr(irt_diff_res), 
                                           mkU8(16) ), 
                                    binop( Iop_And32, mkexpr(irt_sum_res), 
                                           mkU32(0xFFFF)) );

        if (isT) 
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "uqsax%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ------------------- uqasx<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAA && (INSNT1(15,0) & 0xF0F0) == 0xF050) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,0) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN     = newTemp(Ity_I32);
        IRTemp irt_regM     = newTemp(Ity_I32);
        IRTemp irt_sum      = newTemp(Ity_I32);
        IRTemp irt_diff     = newTemp(Ity_I32);
        IRTemp irt_res_sum  = newTemp(Ity_I32);
        IRTemp irt_res_diff = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_diff,  
                binop( Iop_Sub32,
                       binop( Iop_Shr32, 
                              binop( Iop_Shl32, mkexpr(irt_regN), mkU8(16) ), 
                              mkU8(16) ), 
                       binop( Iop_Shr32, mkexpr(irt_regM), mkU8(16) ) ) );
        armUnsignedSatQ( &irt_res_diff, NULL, irt_diff, 0x10 );

        assign( irt_sum, 
                binop( Iop_Add32, 
                       binop( Iop_Shr32, mkexpr(irt_regN), mkU8(16) ), 
                       binop( Iop_Shr32, 
                              binop( Iop_Shl32, mkexpr(irt_regM), mkU8(16) ), 
                              mkU8(16) ) ) );
        armUnsignedSatQ( &irt_res_sum, NULL, irt_sum, 0x10 );
       
        IRExpr* ire_result 
          = binop( Iop_Or32, 
                   binop( Iop_Shl32, mkexpr(irt_res_sum), mkU8(16) ), 
                   binop( Iop_And32, mkexpr(irt_res_diff), mkU32(0xFFFF) ) );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "uqasx%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ------------------- usax<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAE && (INSNT1(15,0) & 0xF0F0) == 0xF040) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,0,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN = newTemp(Ity_I32);
        IRTemp irt_regM = newTemp(Ity_I32);
        IRTemp irt_sum  = newTemp(Ity_I32);
        IRTemp irt_diff = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_sum,  
                binop( Iop_Add32, 
                       unop( Iop_16Uto32,
                             unop( Iop_32to16, mkexpr(irt_regN) )
                       ),  
                       binop( Iop_Shr32, mkexpr(irt_regM), mkU8(16) ) ) );

        assign( irt_diff, 
                binop( Iop_Sub32, 
                       binop( Iop_Shr32, mkexpr(irt_regN), mkU8(16) ), 
                       unop( Iop_16Uto32, 
                             unop( Iop_32to16, mkexpr(irt_regM) )
                       )
                )
        );
       
        IRExpr* ire_result 
          = binop( Iop_Or32, 
                   binop( Iop_Shl32, mkexpr(irt_diff), mkU8(16) ), 
                   binop( Iop_And32, mkexpr(irt_sum), mkU32(0xFFFF) ) );

        IRTemp ge10 = newTemp(Ity_I32);
        assign( ge10, IRExpr_ITE( binop( Iop_CmpLE32U, 
                                         mkU32(0x10000), mkexpr(irt_sum) ),
                                  mkU32(1), mkU32(0) ) );
        put_GEFLAG32( 0, 0, mkexpr(ge10), condT );
        put_GEFLAG32( 1, 0, mkexpr(ge10), condT );

        IRTemp ge32 = newTemp(Ity_I32);
        assign(ge32, unop(Iop_Not32, mkexpr(irt_diff)));
        put_GEFLAG32( 2, 31, mkexpr(ge32), condT );
        put_GEFLAG32( 3, 31, mkexpr(ge32), condT );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "usax%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ------------------- uasx<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAA && (INSNT1(15,0) & 0xF0F0) == 0xF040) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,0,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN = newTemp(Ity_I32);
        IRTemp irt_regM = newTemp(Ity_I32);
        IRTemp irt_sum  = newTemp(Ity_I32);
        IRTemp irt_diff = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_diff,  
                binop( Iop_Sub32, 
                       unop( Iop_16Uto32, 
                             unop( Iop_32to16, mkexpr(irt_regN) )
                       ), 
                       binop( Iop_Shr32, mkexpr(irt_regM), mkU8(16) ) ) );

        assign( irt_sum, 
                binop( Iop_Add32, 
                       binop( Iop_Shr32, mkexpr(irt_regN), mkU8(16) ), 
                       unop( Iop_16Uto32, 
                             unop( Iop_32to16, mkexpr(irt_regM) )
                       ) ) );
       
        IRExpr* ire_result 
          = binop( Iop_Or32, 
                   binop( Iop_Shl32, mkexpr(irt_sum), mkU8(16) ), 
                   binop( Iop_And32, mkexpr(irt_diff), mkU32(0xFFFF) ) );

        IRTemp ge10 = newTemp(Ity_I32);
        assign(ge10, unop(Iop_Not32, mkexpr(irt_diff)));
        put_GEFLAG32( 0, 31, mkexpr(ge10), condT );
        put_GEFLAG32( 1, 31, mkexpr(ge10), condT );

        IRTemp ge32 = newTemp(Ity_I32);
        assign( ge32, IRExpr_ITE( binop( Iop_CmpLE32U,
                                         mkU32(0x10000), mkexpr(irt_sum) ),
                                  mkU32(1), mkU32(0) ) );
        put_GEFLAG32( 2, 0, mkexpr(ge32), condT );
        put_GEFLAG32( 3, 0, mkexpr(ge32), condT );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "uasx%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ------------------- ssax<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAE && (INSNT1(15,0) & 0xF0F0) == 0xF000) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,0,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN = newTemp(Ity_I32);
        IRTemp irt_regM = newTemp(Ity_I32);
        IRTemp irt_sum  = newTemp(Ity_I32);
        IRTemp irt_diff = newTemp(Ity_I32);

        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_sum,  
                binop( Iop_Add32, 
                       binop( Iop_Sar32, 
                              binop( Iop_Shl32, mkexpr(irt_regN), mkU8(16) ), 
                              mkU8(16) ), 
                       binop( Iop_Sar32, mkexpr(irt_regM), mkU8(16) ) ) );

        assign( irt_diff, 
                binop( Iop_Sub32, 
                       binop( Iop_Sar32, mkexpr(irt_regN), mkU8(16) ), 
                       binop( Iop_Sar32, 
                              binop( Iop_Shl32, mkexpr(irt_regM), mkU8(16) ), 
                              mkU8(16) ) ) );
       
        IRExpr* ire_result 
          = binop( Iop_Or32, 
                   binop( Iop_Shl32, mkexpr(irt_diff), mkU8(16) ), 
                   binop( Iop_And32, mkexpr(irt_sum), mkU32(0xFFFF) ) );

        IRTemp ge10 = newTemp(Ity_I32);
        assign(ge10, unop(Iop_Not32, mkexpr(irt_sum)));
        put_GEFLAG32( 0, 31, mkexpr(ge10), condT );
        put_GEFLAG32( 1, 31, mkexpr(ge10), condT );

        IRTemp ge32 = newTemp(Ity_I32);
        assign(ge32, unop(Iop_Not32, mkexpr(irt_diff)));
        put_GEFLAG32( 2, 31, mkexpr(ge32), condT );
        put_GEFLAG32( 3, 31, mkexpr(ge32), condT );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "ssax%s r%u, r%u, r%u\n", nCC(conq), regD, regN, regM );
        return True;
     }
     /* fall through */
   }

   /* ----------------- shsub8<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAC && (INSNT1(15,0) & 0xF0F0) == 0xF020) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(1,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_HSub8Sx4, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("shsub8%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- sxtab16<c> Rd,Rn,Rm{,rot} ------------------ */
   {
     UInt regD = 99, regN = 99, regM = 99, rotate = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFA2 && (INSNT1(15,0) & 0xF0C0) == 0xF080) {
           regN   = INSNT0(3,0);
           regD   = INSNT1(11,8);
           regM   = INSNT1(3,0);
           rotate = INSNT1(5,4);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,1,0,0,0) &&
            INSNA(9,4)   == BITS6(0,0,0,1,1,1) ) {
           regD   = INSNA(15,12);
           regN   = INSNA(19,16);
           regM   = INSNA(3,0);
           rotate = INSNA(11,10);
           if (regD != 15 && regN != 15 && regM != 15)
             gate = True;
        }
     }

     if (gate) {
        IRTemp irt_regN = newTemp(Ity_I32);
        assign( irt_regN, isT ? getIRegT(regN) : getIRegA(regN) );

        IRTemp irt_regM = newTemp(Ity_I32);
        assign( irt_regM, isT ? getIRegT(regM) : getIRegA(regM) );

        IRTemp irt_rot = newTemp(Ity_I32);
        assign( irt_rot, genROR32(irt_regM, 8 * rotate) );

        /* FIXME Maybe we can write this arithmetic in shorter form. */
        IRExpr* resLo
           = binop(Iop_And32,
                   binop(Iop_Add32,
                         mkexpr(irt_regN),
                         unop(Iop_16Uto32,
                              unop(Iop_8Sto16,
                                   unop(Iop_32to8, mkexpr(irt_rot))))),
                   mkU32(0x0000FFFF));

        IRExpr* resHi
           = binop(Iop_And32,
                   binop(Iop_Add32,
                         mkexpr(irt_regN),
                         binop(Iop_Shl32,
                               unop(Iop_16Uto32,
                                    unop(Iop_8Sto16,
                                         unop(Iop_32to8,
                                              binop(Iop_Shr32,
                                                    mkexpr(irt_rot),
                                                    mkU8(16))))),
                               mkU8(16))),
                   mkU32(0xFFFF0000));

        IRExpr* ire_result 
           = binop( Iop_Or32, resHi, resLo );

        if (isT)
           putIRegT( regD, ire_result, condT );
        else
           putIRegA( regD, ire_result, condT, Ijk_Boring );

        DIP( "sxtab16%s r%u, r%u, r%u, ROR #%u\n", 
             nCC(conq), regD, regN, regM, 8 * rotate );
        return True;
     }
     /* fall through */
   }

   /* ----------------- shasx<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAA && (INSNT1(15,0) & 0xF0F0) == 0xF020) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp irt_diff  = newTemp(Ity_I32);
        IRTemp irt_sum   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_diff,
                binop(Iop_Sub32,
                      unop(Iop_16Sto32,
                           unop(Iop_32to16,
                                mkexpr(rNt)
                           )
                      ),
                      unop(Iop_16Sto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(rMt), mkU8(16)
                                )
                           )
                      )
                )
        );

        assign( irt_sum,
                binop(Iop_Add32,
                      unop(Iop_16Sto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(rNt), mkU8(16)
                                )
                           )
                      ),
                      unop(Iop_16Sto32,
                           unop(Iop_32to16, mkexpr(rMt)
                           )
                      )
                )
        );

        assign( res_q,
                binop(Iop_Or32, 
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(irt_diff), mkU8(1)
                                )
                           )
                      ),
                      binop(Iop_Shl32,
                            binop(Iop_Shr32,
                                  mkexpr(irt_sum), mkU8(1)
                            ),
                            mkU8(16)
                     )
                )
        );

        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("shasx%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- uhasx<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAA && (INSNT1(15,0) & 0xF0F0) == 0xF060) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,0,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp irt_diff  = newTemp(Ity_I32);
        IRTemp irt_sum   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_diff,
                binop(Iop_Sub32,
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                mkexpr(rNt)
                           )
                      ),
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(rMt), mkU8(16)
                                )
                           )
                      )
                )
        );

        assign( irt_sum,
                binop(Iop_Add32,
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(rNt), mkU8(16)
                                )
                           )
                      ),
                      unop(Iop_16Uto32,
                           unop(Iop_32to16, mkexpr(rMt)
                           )
                      )
                )
        );

        assign( res_q,
                binop(Iop_Or32, 
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(irt_diff), mkU8(1)
                                )
                           )
                      ),
                      binop(Iop_Shl32,
                            binop(Iop_Shr32,
                                  mkexpr(irt_sum), mkU8(1)
                            ),
                            mkU8(16)
                     )
                )
        );

        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uhasx%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- shsax<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAE && (INSNT1(15,0) & 0xF0F0) == 0xF020) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp irt_diff  = newTemp(Ity_I32);
        IRTemp irt_sum   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_sum,
                binop(Iop_Add32,
                      unop(Iop_16Sto32,
                           unop(Iop_32to16,
                                mkexpr(rNt)
                           )
                      ),
                      unop(Iop_16Sto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(rMt), mkU8(16)
                                )
                           )
                      )
                )
        );

        assign( irt_diff,
                binop(Iop_Sub32,
                      unop(Iop_16Sto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(rNt), mkU8(16)
                                )
                           )
                      ),
                      unop(Iop_16Sto32,
                           unop(Iop_32to16, mkexpr(rMt)
                           )
                      )
                )
        );

        assign( res_q,
                binop(Iop_Or32, 
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(irt_sum), mkU8(1)
                                )
                           )
                      ),
                      binop(Iop_Shl32,
                            binop(Iop_Shr32,
                                  mkexpr(irt_diff), mkU8(1)
                            ),
                            mkU8(16)
                     )
                )
        );

        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("shsax%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- uhsax<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAE && (INSNT1(15,0) & 0xF0F0) == 0xF060) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,1,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,0,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp irt_diff  = newTemp(Ity_I32);
        IRTemp irt_sum   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign( irt_sum,
                binop(Iop_Add32,
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                mkexpr(rNt)
                           )
                      ),
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(rMt), mkU8(16)
                                )
                           )
                      )
                )
        );

        assign( irt_diff,
                binop(Iop_Sub32,
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(rNt), mkU8(16)
                                )
                           )
                      ),
                      unop(Iop_16Uto32,
                           unop(Iop_32to16, mkexpr(rMt)
                           )
                      )
                )
        );

        assign( res_q,
                binop(Iop_Or32, 
                      unop(Iop_16Uto32,
                           unop(Iop_32to16,
                                binop(Iop_Shr32,
                                      mkexpr(irt_sum), mkU8(1)
                                )
                           )
                      ),
                      binop(Iop_Shl32,
                            binop(Iop_Shr32,
                                  mkexpr(irt_diff), mkU8(1)
                            ),
                            mkU8(16)
                     )
                )
        );

        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("uhsax%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- shsub16<c> <Rd>,<Rn>,<Rm> ------------------- */
   {
     UInt regD = 99, regN = 99, regM = 99;
     Bool gate = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFAD && (INSNT1(15,0) & 0xF0F0) == 0xF020) {
           regN = INSNT0(3,0);
           regD = INSNT1(11,8);
           regM = INSNT1(3,0);
           if (!isBadRegT(regD) && !isBadRegT(regN) && !isBadRegT(regM))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,0,0,0,1,1) &&
            INSNA(11,8)  == BITS4(1,1,1,1)         &&
            INSNA(7,4)   == BITS4(0,1,1,1)) {
           regD = INSNA(15,12);
           regN = INSNA(19,16);
           regM = INSNA(3,0);
           if (regD != 15 && regN != 15 && regM != 15)
              gate = True;
        }
     }

     if (gate) {
        IRTemp rNt   = newTemp(Ity_I32);
        IRTemp rMt   = newTemp(Ity_I32);
        IRTemp res_q = newTemp(Ity_I32);

        assign( rNt, isT ? getIRegT(regN) : getIRegA(regN) );
        assign( rMt, isT ? getIRegT(regM) : getIRegA(regM) );

        assign(res_q, binop(Iop_HSub16Sx2, mkexpr(rNt), mkexpr(rMt)));
        if (isT)
           putIRegT( regD, mkexpr(res_q), condT );
        else
           putIRegA( regD, mkexpr(res_q), condT, Ijk_Boring );

        DIP("shsub16%s r%u, r%u, r%u\n", nCC(conq),regD,regN,regM);
        return True;
     }
     /* fall through */
   }

   /* ----------------- smmls{r}<c> <Rd>,<Rn>,<Rm>,<Ra> ------------------- */
   {
     UInt rD = 99, rN = 99, rM = 99, rA = 99;
     Bool round  = False;
     Bool gate   = False;

     if (isT) {
        if (INSNT0(15,7) == BITS9(1,1,1,1,1,0,1,1,0)
            && INSNT0(6,4) == BITS3(1,1,0)
            && INSNT1(7,5) == BITS3(0,0,0)) {
           round = INSNT1(4,4);
           rA    = INSNT1(15,12);
           rD    = INSNT1(11,8);
           rM    = INSNT1(3,0);
           rN    = INSNT0(3,0);
           if (!isBadRegT(rD)
               && !isBadRegT(rN) && !isBadRegT(rM) && !isBadRegT(rA))
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,1,0,1,0,1)
            && INSNA(15,12) != BITS4(1,1,1,1)
            && (INSNA(7,4) & BITS4(1,1,0,1)) == BITS4(1,1,0,1)) {
           round = INSNA(5,5);
           rD    = INSNA(19,16);
           rA    = INSNA(15,12);
           rM    = INSNA(11,8);
           rN    = INSNA(3,0);
           if (rD != 15 && rM != 15 && rN != 15)
              gate = True;
        }
     }
     if (gate) {
        IRTemp irt_rA   = newTemp(Ity_I32);
        IRTemp irt_rN   = newTemp(Ity_I32);
        IRTemp irt_rM   = newTemp(Ity_I32);
        assign( irt_rA, isT ? getIRegT(rA) : getIRegA(rA) );
        assign( irt_rN, isT ? getIRegT(rN) : getIRegA(rN) );
        assign( irt_rM, isT ? getIRegT(rM) : getIRegA(rM) );
        IRExpr* res
        = unop(Iop_64HIto32,
               binop(Iop_Add64,
                     binop(Iop_Sub64,
                           binop(Iop_32HLto64, mkexpr(irt_rA), mkU32(0)),
                           binop(Iop_MullS32, mkexpr(irt_rN), mkexpr(irt_rM))),
                     mkU64(round ? 0x80000000ULL : 0ULL)));
        if (isT)
           putIRegT( rD, res, condT );
        else
           putIRegA(rD, res, condT, Ijk_Boring);
        DIP("smmls%s%s r%u, r%u, r%u, r%u\n",
            round ? "r" : "", nCC(conq), rD, rN, rM, rA);
        return True;
     }
     /* fall through */
   }

   /* -------------- smlald{x}<c> <RdLo>,<RdHi>,<Rn>,<Rm> ---------------- */
   {
     UInt rN = 99, rDlo = 99, rDhi = 99, rM = 99;
     Bool m_swap = False;
     Bool gate   = False;

     if (isT) {
        if (INSNT0(15,4) == 0xFBC &&
            (INSNT1(7,4) & BITS4(1,1,1,0)) == BITS4(1,1,0,0)) {
           rN     = INSNT0(3,0);
           rDlo   = INSNT1(15,12);
           rDhi   = INSNT1(11,8);
           rM     = INSNT1(3,0);
           m_swap = (INSNT1(4,4) & 1) == 1;
           if (!isBadRegT(rDlo) && !isBadRegT(rDhi) && !isBadRegT(rN)
               && !isBadRegT(rM) && rDhi != rDlo)
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,1,0,1,0,0)
            && (INSNA(7,4) & BITS4(1,1,0,1)) == BITS4(0,0,0,1)) {
           rN     = INSNA(3,0);
           rDlo   = INSNA(15,12);
           rDhi   = INSNA(19,16);
           rM     = INSNA(11,8);
           m_swap = ( INSNA(5,5) & 1 ) == 1;
           if (rDlo != 15 && rDhi != 15
               && rN != 15 && rM != 15 && rDlo != rDhi)
              gate = True;
        }
     }

     if (gate) {
        IRTemp irt_rM   = newTemp(Ity_I32);
        IRTemp irt_rN   = newTemp(Ity_I32);
        IRTemp irt_rDhi = newTemp(Ity_I32);
        IRTemp irt_rDlo = newTemp(Ity_I32);
        IRTemp op_2     = newTemp(Ity_I32);
        IRTemp pr_1     = newTemp(Ity_I64);
        IRTemp pr_2     = newTemp(Ity_I64);
        IRTemp result   = newTemp(Ity_I64);
        IRTemp resHi    = newTemp(Ity_I32);
        IRTemp resLo    = newTemp(Ity_I32);
        assign( irt_rM, isT ? getIRegT(rM) : getIRegA(rM));
        assign( irt_rN, isT ? getIRegT(rN) : getIRegA(rN));
        assign( irt_rDhi, isT ? getIRegT(rDhi) : getIRegA(rDhi));
        assign( irt_rDlo, isT ? getIRegT(rDlo) : getIRegA(rDlo));
        assign( op_2, genROR32(irt_rM, m_swap ? 16 : 0) );
        assign( pr_1, binop(Iop_MullS32,
                            unop(Iop_16Sto32,
                                 unop(Iop_32to16, mkexpr(irt_rN))
                            ),
                            unop(Iop_16Sto32,
                                 unop(Iop_32to16, mkexpr(op_2))
                            )
                      )
        );
        assign( pr_2, binop(Iop_MullS32,
                            binop(Iop_Sar32, mkexpr(irt_rN), mkU8(16)),
                            binop(Iop_Sar32, mkexpr(op_2), mkU8(16))
                      )
        );
        assign( result, binop(Iop_Add64,
                              binop(Iop_Add64,
                                    mkexpr(pr_1),
                                    mkexpr(pr_2)
                              ),
                              binop(Iop_32HLto64,
                                    mkexpr(irt_rDhi),
                                    mkexpr(irt_rDlo)
                              )
                        )
        );
        assign( resHi, unop(Iop_64HIto32, mkexpr(result)) );
        assign( resLo, unop(Iop_64to32, mkexpr(result)) );
        if (isT) {
           putIRegT( rDhi, mkexpr(resHi), condT );
           putIRegT( rDlo, mkexpr(resLo), condT );
        } else {
           putIRegA( rDhi, mkexpr(resHi), condT, Ijk_Boring );
           putIRegA( rDlo, mkexpr(resLo), condT, Ijk_Boring );
        }
        DIP("smlald%c%s r%u, r%u, r%u, r%u\n",
            m_swap ? 'x' : ' ', nCC(conq), rDlo, rDhi, rN, rM);
        return True;
     }
     /* fall through */
   }

   /* -------------- smlsld{x}<c> <RdLo>,<RdHi>,<Rn>,<Rm> ---------------- */
   {
     UInt rN = 99, rDlo = 99, rDhi = 99, rM = 99;
     Bool m_swap = False;
     Bool gate   = False;

     if (isT) {
        if ((INSNT0(15,4) == 0xFBD &&
            (INSNT1(7,4) & BITS4(1,1,1,0)) == BITS4(1,1,0,0))) {
           rN     = INSNT0(3,0);
           rDlo   = INSNT1(15,12);
           rDhi   = INSNT1(11,8);
           rM     = INSNT1(3,0);
           m_swap = (INSNT1(4,4) & 1) == 1;
           if (!isBadRegT(rDlo) && !isBadRegT(rDhi) && !isBadRegT(rN) &&
               !isBadRegT(rM) && rDhi != rDlo)
              gate = True;
        }
     } else {
        if (INSNA(27,20) == BITS8(0,1,1,1,0,1,0,0) &&
            (INSNA(7,4) & BITS4(1,1,0,1)) == BITS4(0,1,0,1)) {
           rN     = INSNA(3,0);
           rDlo   = INSNA(15,12);
           rDhi   = INSNA(19,16);
           rM     = INSNA(11,8);
           m_swap = (INSNA(5,5) & 1) == 1;
           if (rDlo != 15 && rDhi != 15 &&
               rN != 15 && rM != 15 && rDlo != rDhi)
              gate = True;
        }
     }
     if (gate) {
        IRTemp irt_rM   = newTemp(Ity_I32);
        IRTemp irt_rN   = newTemp(Ity_I32);
        IRTemp irt_rDhi = newTemp(Ity_I32);
        IRTemp irt_rDlo = newTemp(Ity_I32);
        IRTemp op_2     = newTemp(Ity_I32);
        IRTemp pr_1     = newTemp(Ity_I64);
        IRTemp pr_2     = newTemp(Ity_I64);
        IRTemp result   = newTemp(Ity_I64);
        IRTemp resHi    = newTemp(Ity_I32);
        IRTemp resLo    = newTemp(Ity_I32);
        assign( irt_rM, isT ? getIRegT(rM) : getIRegA(rM) );
        assign( irt_rN, isT ? getIRegT(rN) : getIRegA(rN) );
        assign( irt_rDhi, isT ? getIRegT(rDhi) : getIRegA(rDhi) );
        assign( irt_rDlo, isT ? getIRegT(rDlo) : getIRegA(rDlo) );
        assign( op_2, genROR32(irt_rM, m_swap ? 16 : 0) );
        assign( pr_1, binop(Iop_MullS32,
                            unop(Iop_16Sto32,
                                 unop(Iop_32to16, mkexpr(irt_rN))
                            ),
                            unop(Iop_16Sto32,
                                 unop(Iop_32to16, mkexpr(op_2))
                            )
                      )
        );
        assign( pr_2, binop(Iop_MullS32,
                            binop(Iop_Sar32, mkexpr(irt_rN), mkU8(16)),
                            binop(Iop_Sar32, mkexpr(op_2), mkU8(16))
                      )
        );
        assign( result, binop(Iop_Add64,
                              binop(Iop_Sub64,
                                    mkexpr(pr_1),
                                    mkexpr(pr_2)
                              ),
                              binop(Iop_32HLto64,
                                    mkexpr(irt_rDhi),
                                    mkexpr(irt_rDlo)
                              )
                        )
        );
        assign( resHi, unop(Iop_64HIto32, mkexpr(result)) );
        assign( resLo, unop(Iop_64to32, mkexpr(result)) );
        if (isT) {
           putIRegT( rDhi, mkexpr(resHi), condT );
           putIRegT( rDlo, mkexpr(resLo), condT );
        } else {
           putIRegA( rDhi, mkexpr(resHi), condT, Ijk_Boring );
           putIRegA( rDlo, mkexpr(resLo), condT, Ijk_Boring );
        }
        DIP("smlsld%c%s r%u, r%u, r%u, r%u\n",
            m_swap ? 'x' : ' ', nCC(conq), rDlo, rDhi, rN, rM);
        return True;
     }
     /* fall through */
   }

   /* ---------- Doesn't match anything. ---------- */
   return False;

#  undef INSNA
#  undef INSNT0
#  undef INSNT1
}


/*------------------------------------------------------------*/
/*--- V8 instructions                                      ---*/
/*------------------------------------------------------------*/

/* Break a V128-bit value up into four 32-bit ints. */

static void breakupV128to32s ( IRTemp t128,
                               /*OUTs*/
                               IRTemp* t3, IRTemp* t2,
                               IRTemp* t1, IRTemp* t0 )
{
   IRTemp hi64 = newTemp(Ity_I64);
   IRTemp lo64 = newTemp(Ity_I64);
   assign( hi64, unop(Iop_V128HIto64, mkexpr(t128)) );
   assign( lo64, unop(Iop_V128to64,   mkexpr(t128)) );

   vassert(t0 && *t0 == IRTemp_INVALID);
   vassert(t1 && *t1 == IRTemp_INVALID);
   vassert(t2 && *t2 == IRTemp_INVALID);
   vassert(t3 && *t3 == IRTemp_INVALID);

   *t0 = newTemp(Ity_I32);
   *t1 = newTemp(Ity_I32);
   *t2 = newTemp(Ity_I32);
   *t3 = newTemp(Ity_I32);
   assign( *t0, unop(Iop_64to32,   mkexpr(lo64)) );
   assign( *t1, unop(Iop_64HIto32, mkexpr(lo64)) );
   assign( *t2, unop(Iop_64to32,   mkexpr(hi64)) );
   assign( *t3, unop(Iop_64HIto32, mkexpr(hi64)) );
}


/* Both ARM and Thumb */

/* Translate a V8 instruction.  If successful, returns True and *dres
   may or may not be updated.  If unsuccessful, returns False and
   doesn't change *dres nor create any IR.

   The Thumb and ARM encodings are potentially different.  In both
   ARM and Thumb mode, the caller must pass the entire 32 bits of
   the instruction.  Callers may pass any instruction; this function
   ignores anything it doesn't recognise.

   Caller must supply an IRTemp 'condT' holding the gating condition,
   or IRTemp_INVALID indicating the insn is always executed.

   If we are decoding an ARM instruction which is in the NV space
   then it is expected that condT will be IRTemp_INVALID, and that is
   asserted for.  That condition is ensured by the logic near the top
   of disInstr_ARM_WRK, that sets up condT.

   When decoding for Thumb, the caller must pass the ITState pre/post
   this instruction, so that we can generate a SIGILL in the cases where
   the instruction may not be in an IT block.  When decoding for ARM,
   both of these must be IRTemp_INVALID.

   Finally, the caller must indicate whether this occurs in ARM or in
   Thumb code.
*/
static Bool decode_V8_instruction (
               /*MOD*/DisResult* dres,
               UInt              insnv8,
               IRTemp            condT,
               Bool              isT,
               IRTemp            old_itstate,
               IRTemp            new_itstate
            )
{
#  define INSN(_bMax,_bMin)   SLICE_UInt(insnv8, (_bMax), (_bMin))

   if (isT) {
      vassert(old_itstate != IRTemp_INVALID);
      vassert(new_itstate != IRTemp_INVALID);
   } else {
      vassert(old_itstate == IRTemp_INVALID);
      vassert(new_itstate == IRTemp_INVALID);
   }

   /* ARMCondcode 'conq' is only used for debug printing and for no other
      purpose.  For ARM, this is simply the top 4 bits of the instruction.
      For Thumb, the condition is not (really) known until run time, and so
      we set it to ARMCondAL in order that printing of these instructions
      does not show any condition. */
   ARMCondcode conq;
   if (isT) {
      conq = ARMCondAL;
   } else {
      conq = (ARMCondcode)INSN(31,28);
      if (conq == ARMCondNV || conq == ARMCondAL) {
         vassert(condT == IRTemp_INVALID);
      } else {
         vassert(condT != IRTemp_INVALID);
      }
      vassert(conq >= ARMCondEQ && conq <= ARMCondNV);
   }

   /* ----------- {AESD, AESE, AESMC, AESIMC}.8 q_q ----------- */
   /*     31   27   23  21 19 17 15 11   7      3
      T1: 1111 1111 1 D 11 sz 00 d  0011 00 M 0 m  AESE Qd, Qm
      A1: 1111 0011 1 D 11 sz 00 d  0011 00 M 0 m  AESE Qd, Qm

      T1: 1111 1111 1 D 11 sz 00 d  0011 01 M 0 m  AESD Qd, Qm
      A1: 1111 0011 1 D 11 sz 00 d  0011 01 M 0 m  AESD Qd, Qm

      T1: 1111 1111 1 D 11 sz 00 d  0011 10 M 0 m  AESMC Qd, Qm
      A1: 1111 0011 1 D 11 sz 00 d  0011 10 M 0 m  AESMC Qd, Qm

      T1: 1111 1111 1 D 11 sz 00 d  0011 11 M 0 m  AESIMC Qd, Qm
      A1: 1111 0011 1 D 11 sz 00 d  0011 11 M 0 m  AESIMC Qd, Qm

      sz must be 00
      ARM encoding is in NV space.
      In Thumb mode, we must not be in an IT block.
   */
   {
     UInt regD = 99, regM = 99, opc = 4/*invalid*/;
     Bool gate = True;

     UInt high9 = isT ? BITS9(1,1,1,1,1,1,1,1,1) : BITS9(1,1,1,1,0,0,1,1,1);
     if (INSN(31,23) == high9 && INSN(21,16) == BITS6(1,1,0,0,0,0)
         && INSN(11,8) == BITS4(0,0,1,1) && INSN(4,4) == 0) {
        UInt bitD = INSN(22,22);
        UInt fldD = INSN(15,12);
        UInt bitM = INSN(5,5);
        UInt fldM = INSN(3,0);
        opc  = INSN(7,6);
        regD = (bitD << 4) | fldD;
        regM = (bitM << 4) | fldM;
     }
     if ((regD & 1) == 1 || (regM & 1) == 1)
        gate = False;

     if (gate) {
        if (isT) {
           gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);
        }
        /* In ARM mode, this is statically unconditional.  In Thumb mode,
           this must be dynamically unconditional, and we've SIGILLd if not.
           In either case we can create unconditional IR. */
        IRTemp op1 = newTemp(Ity_V128);
        IRTemp op2 = newTemp(Ity_V128);
        IRTemp src = newTemp(Ity_V128);
        IRTemp res = newTemp(Ity_V128);
        assign(op1,  getQReg(regD >> 1));
        assign(op2,  getQReg(regM >> 1));
        assign(src,  opc == BITS2(0,0) || opc == BITS2(0,1)
                        ? binop(Iop_XorV128, mkexpr(op1), mkexpr(op2))
                        : mkexpr(op2));

        void* helpers[4]
           = { &armg_dirtyhelper_AESE,  &armg_dirtyhelper_AESD,
               &armg_dirtyhelper_AESMC, &armg_dirtyhelper_AESIMC };
        const HChar* hNames[4]
           = { "armg_dirtyhelper_AESE",  "armg_dirtyhelper_AESD",
               "armg_dirtyhelper_AESMC", "armg_dirtyhelper_AESIMC" };
        const HChar* iNames[4]
           = { "aese", "aesd", "aesmc", "aesimc" };

        vassert(opc >= 0 && opc <= 3);
        void*        helper = helpers[opc];
        const HChar* hname  = hNames[opc];

        IRTemp w32_3, w32_2, w32_1, w32_0;
        w32_3 = w32_2 = w32_1 = w32_0 = IRTemp_INVALID;
        breakupV128to32s( src, &w32_3, &w32_2, &w32_1, &w32_0 );

        IRDirty* di
          = unsafeIRDirty_1_N( res, 0/*regparms*/, hname, helper,
                               mkIRExprVec_5(
                                  IRExpr_VECRET(),
                                  mkexpr(w32_3), mkexpr(w32_2),
                                  mkexpr(w32_1), mkexpr(w32_0)) );
        stmt(IRStmt_Dirty(di));

        putQReg(regD >> 1, mkexpr(res), IRTemp_INVALID);
        DIP("%s.8 q%d, q%d\n", iNames[opc], regD >> 1, regM >> 1);
        return True;
     }
     /* fall through */
   }

   /* ----------- SHA 3-reg insns q_q_q ----------- */
   /*
          31   27   23      19 15 11   7       3
      T1: 1110 1111 0  D 00 n  d  1100 N Q M 0 m  SHA1C Qd, Qn, Qm  ix=0
      A1: 1111 0010 ----------------------------

      T1: 1110 1111 0  D 01 n  d  1100 N Q M 0 m  SHA1P Qd, Qn, Qm  ix=1
      A1: 1111 0010 ----------------------------

      T1: 1110 1111 0  D 10 n  d  1100 N Q M 0 m  SHA1M Qd, Qn, Qm  ix=2
      A1: 1111 0010 ----------------------------

      T1: 1110 1111 0  D 11 n  d  1100 N Q M 0 m  SHA1SU0 Qd, Qn, Qm  ix=3
      A1: 1111 0010 ----------------------------
      (that's a complete set of 4, based on insn[21,20])

      T1: 1111 1111 0  D 00 n  d  1100 N Q M 0 m  SHA256H Qd, Qn, Qm  ix=4
      A1: 1111 0011 ----------------------------

      T1: 1111 1111 0  D 01 n  d  1100 N Q M 0 m  SHA256H2 Qd, Qn, Qm  ix=5
      A1: 1111 0011 ----------------------------

      T1: 1111 1111 0  D 10 n  d  1100 N Q M 0 m  SHA256SU1 Qd, Qn, Qm  ix=6
      A1: 1111 0011 ----------------------------
      (3/4 of a complete set of 4, based on insn[21,20])

      Q must be 1.  Same comments about conditionalisation as for the AES
      group above apply.
   */
   {
     UInt ix = 8; /* invalid */
     Bool gate = False;

     UInt hi9_sha1   = isT ? BITS9(1,1,1,0,1,1,1,1,0)
                           : BITS9(1,1,1,1,0,0,1,0,0);
     UInt hi9_sha256 = isT ? BITS9(1,1,1,1,1,1,1,1,0)
                           : BITS9(1,1,1,1,0,0,1,1,0);
     if ((INSN(31,23) == hi9_sha1 || INSN(31,23) == hi9_sha256)
         && INSN(11,8) == BITS4(1,1,0,0)
         && INSN(6,6) == 1 && INSN(4,4) == 0) {
        ix = INSN(21,20);
        if (INSN(31,23) == hi9_sha256)
           ix |= 4;
        if (ix < 7)
           gate = True;
     }

     UInt regN = (INSN(7,7)   << 4)  | INSN(19,16);
     UInt regD = (INSN(22,22) << 4)  | INSN(15,12);
     UInt regM = (INSN(5,5)   << 4)  | INSN(3,0);
     if ((regD & 1) == 1 || (regM & 1) == 1 || (regN & 1) == 1)
        gate = False;

     if (gate) {
        vassert(ix >= 0 && ix < 7);
        const HChar* inames[7]
           = { "sha1c", "sha1p", "sha1m", "sha1su0",
               "sha256h", "sha256h2", "sha256su1" };
        void(*helpers[7])(V128*,UInt,UInt,UInt,UInt,UInt,UInt,
                                UInt,UInt,UInt,UInt,UInt,UInt)
           = { &armg_dirtyhelper_SHA1C,    &armg_dirtyhelper_SHA1P,
               &armg_dirtyhelper_SHA1M,    &armg_dirtyhelper_SHA1SU0,
               &armg_dirtyhelper_SHA256H,  &armg_dirtyhelper_SHA256H2,
               &armg_dirtyhelper_SHA256SU1 };
        const HChar* hnames[7]
           = { "armg_dirtyhelper_SHA1C",    "armg_dirtyhelper_SHA1P",
               "armg_dirtyhelper_SHA1M",    "armg_dirtyhelper_SHA1SU0",
               "armg_dirtyhelper_SHA256H",  "armg_dirtyhelper_SHA256H2",
               "armg_dirtyhelper_SHA256SU1" };

        /* This is a really lame way to implement this, even worse than
           the arm64 version.  But at least it works. */

        if (isT) {
           gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);
        }

        IRTemp vD = newTemp(Ity_V128);
        IRTemp vN = newTemp(Ity_V128);
        IRTemp vM = newTemp(Ity_V128);
        assign(vD,  getQReg(regD >> 1));
        assign(vN,  getQReg(regN >> 1));
        assign(vM,  getQReg(regM >> 1));

        IRTemp d32_3, d32_2, d32_1, d32_0;
        d32_3 = d32_2 = d32_1 = d32_0 = IRTemp_INVALID;
        breakupV128to32s( vD, &d32_3, &d32_2, &d32_1, &d32_0 );

        IRTemp n32_3_pre, n32_2_pre, n32_1_pre, n32_0_pre;
        n32_3_pre = n32_2_pre = n32_1_pre = n32_0_pre = IRTemp_INVALID;
        breakupV128to32s( vN, &n32_3_pre, &n32_2_pre, &n32_1_pre, &n32_0_pre );

        IRTemp m32_3, m32_2, m32_1, m32_0;
        m32_3 = m32_2 = m32_1 = m32_0 = IRTemp_INVALID;
        breakupV128to32s( vM, &m32_3, &m32_2, &m32_1, &m32_0 );

        IRTemp n32_3 = newTemp(Ity_I32);
        IRTemp n32_2 = newTemp(Ity_I32);
        IRTemp n32_1 = newTemp(Ity_I32);
        IRTemp n32_0 = newTemp(Ity_I32);

        /* Mask off any bits of the N register operand that aren't actually
           needed, so that Memcheck doesn't complain unnecessarily. */
        switch (ix) {
           case 0: case 1: case 2:
              assign(n32_3, mkU32(0));
              assign(n32_2, mkU32(0));
              assign(n32_1, mkU32(0));
              assign(n32_0, mkexpr(n32_0_pre));
              break;
           case 3: case 4: case 5: case 6:
              assign(n32_3, mkexpr(n32_3_pre));
              assign(n32_2, mkexpr(n32_2_pre));
              assign(n32_1, mkexpr(n32_1_pre));
              assign(n32_0, mkexpr(n32_0_pre));
              break;
           default:
              vassert(0);
        }

        IRExpr** argvec
           = mkIRExprVec_13(
                IRExpr_VECRET(),
                mkexpr(d32_3), mkexpr(d32_2), mkexpr(d32_1), mkexpr(d32_0), 
                mkexpr(n32_3), mkexpr(n32_2), mkexpr(n32_1), mkexpr(n32_0), 
                mkexpr(m32_3), mkexpr(m32_2), mkexpr(m32_1), mkexpr(m32_0)
             );

        IRTemp res = newTemp(Ity_V128);
        IRDirty* di = unsafeIRDirty_1_N( res, 0/*regparms*/,
                                         hnames[ix], helpers[ix], argvec );
        stmt(IRStmt_Dirty(di));
        putQReg(regD >> 1, mkexpr(res), IRTemp_INVALID);

        DIP("%s.8 q%u, q%u, q%u\n",
            inames[ix], regD >> 1, regN >> 1, regM >> 1);
        return True;
     }
     /* fall through */
   }

   /* ----------- SHA1SU1, SHA256SU0 ----------- */
   /*
          31   27   23  21 19   15 11   7      3
      T1: 1111 1111 1 D 11 1010 d  0011 10 M 0 m  SHA1SU1 Qd, Qm
      A1: 1111 0011 ----------------------------

      T1: 1111 1111 1 D 11 1010 d  0011 11 M 0 m  SHA256SU0 Qd, Qm
      A1: 1111 0011 ----------------------------

      Same comments about conditionalisation as for the AES group above apply.
   */
   {
     Bool gate = False;

     UInt hi9 = isT ? BITS9(1,1,1,1,1,1,1,1,1) : BITS9(1,1,1,1,0,0,1,1,1);
     if (INSN(31,23) == hi9 && INSN(21,16) == BITS6(1,1,1,0,1,0)
         && INSN(11,7) == BITS5(0,0,1,1,1) && INSN(4,4) == 0) {
        gate = True;
     }

     UInt regD = (INSN(22,22) << 4) | INSN(15,12);
     UInt regM = (INSN(5,5)   << 4) | INSN(3,0);
     if ((regD & 1) == 1 || (regM & 1) == 1)
        gate = False;

     Bool is_1SU1 = INSN(6,6) == 0;

     if (gate) {
        const HChar* iname
           = is_1SU1 ? "sha1su1" : "sha256su0";
        void (*helper)(V128*,UInt,UInt,UInt,UInt,UInt,UInt,UInt,UInt)
           = is_1SU1 ? &armg_dirtyhelper_SHA1SU1
                     : *armg_dirtyhelper_SHA256SU0;
        const HChar* hname
           = is_1SU1 ? "armg_dirtyhelper_SHA1SU1"
                     : "armg_dirtyhelper_SHA256SU0";

        if (isT) {
           gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);
        }

        IRTemp vD = newTemp(Ity_V128);
        IRTemp vM = newTemp(Ity_V128);
        assign(vD,  getQReg(regD >> 1));
        assign(vM,  getQReg(regM >> 1));

        IRTemp d32_3, d32_2, d32_1, d32_0;
        d32_3 = d32_2 = d32_1 = d32_0 = IRTemp_INVALID;
        breakupV128to32s( vD, &d32_3, &d32_2, &d32_1, &d32_0 );

        IRTemp m32_3, m32_2, m32_1, m32_0;
        m32_3 = m32_2 = m32_1 = m32_0 = IRTemp_INVALID;
        breakupV128to32s( vM, &m32_3, &m32_2, &m32_1, &m32_0 );

        IRExpr** argvec
           = mkIRExprVec_9(
                IRExpr_VECRET(),
                mkexpr(d32_3), mkexpr(d32_2), mkexpr(d32_1), mkexpr(d32_0), 
                mkexpr(m32_3), mkexpr(m32_2), mkexpr(m32_1), mkexpr(m32_0)
             );

        IRTemp res = newTemp(Ity_V128);
        IRDirty* di = unsafeIRDirty_1_N( res, 0/*regparms*/,
                                         hname, helper, argvec );
        stmt(IRStmt_Dirty(di));
        putQReg(regD >> 1, mkexpr(res), IRTemp_INVALID);

        DIP("%s.8 q%u, q%u\n", iname, regD >> 1, regM >> 1);
        return True;
     }
     /* fall through */
   }

   /* ----------- SHA1H ----------- */
   /*
          31   27   23  21 19   15 11   7      3
      T1: 1111 1111 1 D 11 1001 d  0010 11 M 0 m  SHA1H Qd, Qm
      A1: 1111 0011 ----------------------------

      Same comments about conditionalisation as for the AES group above apply.
   */
   {
     Bool gate = False;

     UInt hi9 = isT ? BITS9(1,1,1,1,1,1,1,1,1) : BITS9(1,1,1,1,0,0,1,1,1);
     if (INSN(31,23) == hi9 && INSN(21,16) == BITS6(1,1,1,0,0,1)
         && INSN(11,6) == BITS6(0,0,1,0,1,1) && INSN(4,4) == 0) {
        gate = True;
     }

     UInt regD = (INSN(22,22) << 4) | INSN(15,12);
     UInt regM = (INSN(5,5)   << 4) | INSN(3,0);
     if ((regD & 1) == 1 || (regM & 1) == 1)
        gate = False;

     if (gate) {
        const HChar* iname = "sha1h";
        void (*helper)(V128*,UInt,UInt,UInt,UInt) = &armg_dirtyhelper_SHA1H;
        const HChar* hname                        = "armg_dirtyhelper_SHA1H";

        if (isT) {
           gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);
        }

        IRTemp vM = newTemp(Ity_V128);
        assign(vM,  getQReg(regM >> 1));

        IRTemp m32_3, m32_2, m32_1, m32_0;
        m32_3 = m32_2 = m32_1 = m32_0 = IRTemp_INVALID;
        breakupV128to32s( vM, &m32_3, &m32_2, &m32_1, &m32_0 );
        /* m32_3, m32_2, m32_1 are just abandoned.  No harm; iropt will
           remove them. */

        IRExpr*  zero   = mkU32(0);
        IRExpr** argvec = mkIRExprVec_5(IRExpr_VECRET(),
                                        zero, zero, zero, mkexpr(m32_0));

        IRTemp res = newTemp(Ity_V128);
        IRDirty* di = unsafeIRDirty_1_N( res, 0/*regparms*/,
                                         hname, helper, argvec );
        stmt(IRStmt_Dirty(di));
        putQReg(regD >> 1, mkexpr(res), IRTemp_INVALID);

        DIP("%s.8 q%u, q%u\n", iname, regD >> 1, regM >> 1);
        return True;
     }
     /* fall through */
   }

   /* ----------- VMULL.P64 ----------- */
   /*
          31   27   23  21 19 15 11   7       3
      T2: 1110 1111 1 D 10 n  d  1110 N 0 M 0 m
      A2: 1111 0010 -------------------------

      The ARM documentation is pretty difficult to follow here.
      Same comments about conditionalisation as for the AES group above apply.
   */
   {
     Bool gate = False;

     UInt hi9 = isT ? BITS9(1,1,1,0,1,1,1,1,1) : BITS9(1,1,1,1,0,0,1,0,1);
     if (INSN(31,23) == hi9 && INSN(21,20) == BITS2(1,0)
         && INSN(11,8) == BITS4(1,1,1,0)
         && INSN(6,6) == 0 && INSN(4,4) == 0) {
        gate = True;
     }

     UInt regN = (INSN(7,7)   << 4)  | INSN(19,16);
     UInt regD = (INSN(22,22) << 4)  | INSN(15,12);
     UInt regM = (INSN(5,5)   << 4)  | INSN(3,0);

     if ((regD & 1) == 1)
        gate = False;

     if (gate) {
        const HChar* iname = "vmull";
        void (*helper)(V128*,UInt,UInt,UInt,UInt) = &armg_dirtyhelper_VMULLP64;
        const HChar* hname                        = "armg_dirtyhelper_VMULLP64";

        if (isT) {
           gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);
        }

        IRTemp srcN = newTemp(Ity_I64);
        IRTemp srcM = newTemp(Ity_I64);
        assign(srcN, getDRegI64(regN));
        assign(srcM, getDRegI64(regM));
        
        IRExpr** argvec = mkIRExprVec_5(IRExpr_VECRET(),
                                        unop(Iop_64HIto32, mkexpr(srcN)),
                                        unop(Iop_64to32,   mkexpr(srcN)),
                                        unop(Iop_64HIto32, mkexpr(srcM)),
                                        unop(Iop_64to32, mkexpr(srcM)));

        IRTemp res = newTemp(Ity_V128);
        IRDirty* di = unsafeIRDirty_1_N( res, 0/*regparms*/,
                                         hname, helper, argvec );
        stmt(IRStmt_Dirty(di));
        putQReg(regD >> 1, mkexpr(res), IRTemp_INVALID);

        DIP("%s.p64 q%u, q%u, w%u\n", iname, regD >> 1, regN, regM);
        return True;
     }
     /* fall through */
   }

   /* ----------- LDA{,B,H}, STL{,B,H} ----------- */
   /*     31   27   23   19   15 11   7    3
      A1: cond 0001 1001  n    t 1100 1001 1111  LDA  Rt, [Rn]
      A1: cond 0001 1111  n    t 1100 1001 1111  LDAH Rt, [Rn]
      A1: cond 0001 1101  n    t 1100 1001 1111  LDAB Rt, [Rn]

      A1: cond 0001 1000  n 1111 1100 1001    t  STL  Rt, [Rn]
      A1: cond 0001 1110  n 1111 1100 1001    t  STLH Rt, [Rn]
      A1: cond 0001 1100  n 1111 1100 1001    t  STLB Rt, [Rn]

      T1: 1110 1000 1101  n    t 1111 1010 1111  LDA  Rt, [Rn]
      T1: 1110 1000 1101  n    t 1111 1001 1111  LDAH Rt, [Rn]
      T1: 1110 1000 1101  n    t 1111 1000 1111  LDAB Rt, [Rn]

      T1: 1110 1000 1100  n    t 1111 1010 1111  STL  Rt, [Rn]
      T1: 1110 1000 1100  n    t 1111 1001 1111  STLH Rt, [Rn]
      T1: 1110 1000 1100  n    t 1111 1000 1111  STLB Rt, [Rn]
   */
   {
     UInt nn     = 16; // invalid
     UInt tt     = 16; // invalid
     UInt sz     = 4;  // invalid
     Bool isLoad = False;
     Bool gate   = False;
     if (isT) {
        if (INSN(31,21) == BITS11(1,1,1,0,1,0,0,0,1,1,0)
            && INSN(11,6) == BITS6(1,1,1,1,1,0)
            && INSN(3,0) == BITS4(1,1,1,1)) {
           nn     = INSN(19,16);
           tt     = INSN(15,12);
           isLoad = INSN(20,20) == 1;
           sz     = INSN(5,4); // 00:B 01:H 10:W 11:invalid
           gate   = sz != BITS2(1,1) && tt != 15 && nn != 15;
        }
     } else {
        if (INSN(27,23) == BITS5(0,0,0,1,1) && INSN(20,20) == 1
            && INSN(11,0) == BITS12(1,1,0,0,1,0,0,1,1,1,1,1)) {
           nn     = INSN(19,16);
           tt     = INSN(15,12);
           isLoad = True;
           sz     = INSN(22,21); // 10:B 11:H 00:W 01:invalid
           gate   = sz != BITS2(0,1) && tt != 15 && nn != 15;
        }
        else
        if (INSN(27,23) == BITS5(0,0,0,1,1) && INSN(20,20) == 0
            && INSN(15,4) == BITS12(1,1,1,1,1,1,0,0,1,0,0,1)) {
           nn     = INSN(19,16);
           tt     = INSN(3,0);
           isLoad = False;
           sz     = INSN(22,21);  // 10:B 11:H 00:W 01:invalid
           gate   = sz != BITS2(0,1) && tt != 15 && nn != 15;
        }
        if (gate) {
           // Rearrange sz bits to be the same as the Thumb case
           switch (sz) {
              case 2: sz = 0; break;
              case 3: sz = 1; break;
              case 0: sz = 2; break;
              default: /*NOTREACHED*/vassert(0);
           }
        }
     }
     // For both encodings, the instruction is guarded by condT, which
     // is passed in by the caller.  Note that the the loads and stores
     // are conditional, so we don't have to truncate the IRSB at this
     // point, but the fence is unconditional.  There's no way to
     // represent a conditional fence (without a side exit), but it
     // doesn't matter from a correctness standpoint that it is
     // unconditional -- it just loses a bit of performance in the
     // case where the condition doesn't hold.
     if (gate) {
        vassert(sz <= 2 && nn <= 14 && tt <= 14);
        IRExpr* ea = llGetIReg(nn);
        if (isLoad) {
           static IRLoadGOp cvt[3]
              = { ILGop_8Uto32, ILGop_16Uto32, ILGop_Ident32 };
           IRTemp data = newTemp(Ity_I32);
           loadGuardedLE(data, cvt[sz], ea, mkU32(0)/*alt*/, condT);
           if (isT) {
              putIRegT(tt, mkexpr(data), condT);
           } else {
              putIRegA(tt, mkexpr(data), condT, Ijk_INVALID);
           }
           stmt(IRStmt_MBE(Imbe_Fence));
        } else {
           stmt(IRStmt_MBE(Imbe_Fence));
           IRExpr* data = llGetIReg(tt);
           switch (sz) {
              case 0: data = unop(Iop_32to8,  data); break;
              case 1: data = unop(Iop_32to16, data); break;
              case 2: break;
              default: vassert(0);
           }
           storeGuardedLE(ea, data, condT);
        }
        const HChar* ldNames[3] = { "ldab", "ldah", "lda" };
        const HChar* stNames[3] = { "stlb", "stlh", "stl" };
        DIP("%s r%u, [r%u]", (isLoad ? ldNames : stNames)[sz], tt, nn);
        return True;
     }
     /* else fall through */
   }

   /* ---------- Doesn't match anything. ---------- */
   return False;

#  undef INSN
}


/*------------------------------------------------------------*/
/*--- LDMxx/STMxx helper (both ARM and Thumb32)            ---*/
/*------------------------------------------------------------*/

/* Generate IR for LDMxx and STMxx.  This is complex.  Assumes it's
   unconditional, so the caller must produce a jump-around before
   calling this, if the insn is to be conditional.  Caller is
   responsible for all validation of parameters.  For LDMxx, if PC is
   amongst the values loaded, caller is also responsible for
   generating the jump. */
static void mk_ldm_stm ( Bool arm,     /* True: ARM, False: Thumb */
                         UInt rN,      /* base reg */
                         UInt bINC,    /* 1: inc,  0: dec */
                         UInt bBEFORE, /* 1: inc/dec before, 0: after */
                         UInt bW,      /* 1: writeback to Rn */
                         UInt bL,      /* 1: load, 0: store */
                         UInt regList )
{
   Int i, r, m, nRegs;
   IRTemp jk = Ijk_Boring;

   /* Get hold of the old Rn value.  We might need to write its value
      to memory during a store, and if it's also the writeback
      register then we need to get its value now.  We can't treat it
      exactly like the other registers we're going to transfer,
      because for xxMDA and xxMDB writeback forms, the generated IR
      updates Rn in the guest state before any transfers take place.
      We have to do this as per comments below, in order that if Rn is
      the stack pointer then it always has a value is below or equal
      to any of the transfer addresses.  Ick. */
   IRTemp oldRnT = newTemp(Ity_I32);
   assign(oldRnT, arm ? getIRegA(rN) : getIRegT(rN));

   IRTemp anchorT = newTemp(Ity_I32);
   /* The old (Addison-Wesley) ARM ARM seems to say that LDMxx/STMxx
      ignore the bottom two bits of the address.  However, Cortex-A8
      doesn't seem to care.  Hence: */
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
      IRExpr* e = binop(opADDorSUB, mkexpr(oldRnT), mkU32(4*nRegs));
      if (arm)
         putIRegA( rN, e, IRTemp_INVALID, Ijk_Boring );
      else
         putIRegT( rN, e, IRTemp_INVALID );
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
         vex_printf("\nREG_LIST_PRE: (rN=%u)\n", rN);
         for (i = 0; i < nX; i++)
            vex_printf("reg %u   off %u\n", xReg[i], xOff[i]);
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
            vex_printf("reg %u   off %u\n", xReg[i], xOff[i]);
         vex_printf("\n");
      }
   }

   /* According to the Cortex A8 TRM Sec. 5.2.1, LDM(1) with r13 as the base
       register and PC in the register list is a return for purposes of branch
       prediction.
      The ARM ARM Sec. C9.10.1 further specifies that writeback must be enabled
       to be counted in event 0x0E (Procedure return).*/
   if (rN == 13 && bL == 1 && bINC && !bBEFORE && bW == 1) {
      jk = Ijk_Ret;
   }

   /* Actually generate the transfers */
   for (i = 0; i < nX; i++) {
      r = xReg[i];
      if (bL == 1) {
         IRExpr* e = loadLE(Ity_I32,
                            binop(opADDorSUB, mkexpr(anchorT),
                                  mkU32(xOff[i])));
         if (arm) {
            putIRegA( r, e, IRTemp_INVALID, jk );
         } else {
            // no: putIRegT( r, e, IRTemp_INVALID );
            // putIRegT refuses to write to R15.  But that might happen.
            // Since this is uncond, and we need to be able to
            // write the PC, just use the low level put:
            llPutIReg( r, e );
         }
      } else {
         /* if we're storing Rn, make sure we use the correct
            value, as per extensive comments above */
         storeLE( binop(opADDorSUB, mkexpr(anchorT), mkU32(xOff[i])),
                  r == rN ? mkexpr(oldRnT) 
                          : (arm ? getIRegA(r) : getIRegT(r) ) );
      }
   }

   // If we are doing xxMIA or xxMIB,
   // do the transfer first, and then update rN afterwards.
   if (bW == 1 && bINC) {
      IRExpr* e = binop(opADDorSUB, mkexpr(oldRnT), mkU32(4*nRegs));
      if (arm)
         putIRegA( rN, e, IRTemp_INVALID, Ijk_Boring );
      else
         putIRegT( rN, e, IRTemp_INVALID );
   }
}


/*------------------------------------------------------------*/
/*--- VFP (CP 10 and 11) instructions                      ---*/
/*------------------------------------------------------------*/

/* Both ARM and Thumb */

/* Translate a CP10 or CP11 instruction.  If successful, returns
   True and *dres may or may not be updated.  If failure, returns
   False and doesn't change *dres nor create any IR.

   The ARM and Thumb encodings are identical for the low 28 bits of
   the insn (yay!) and that's what the caller must supply, iow, imm28
   has the top 4 bits masked out.  Caller is responsible for
   determining whether the masked-out bits are valid for a CP10/11
   insn.  The rules for the top 4 bits are:

     ARM: 0000 to 1110 allowed, and this is the gating condition.
     1111 (NV) is not allowed.

     Thumb: must be 1110.  The gating condition is taken from
     ITSTATE in the normal way.

   Conditionalisation:

   Caller must supply an IRTemp 'condT' holding the gating condition,
   or IRTemp_INVALID indicating the insn is always executed.

   Caller must also supply an ARMCondcode 'cond'.  This is only used
   for debug printing, no other purpose.  For ARM, this is simply the
   top 4 bits of the original instruction.  For Thumb, the condition
   is not (really) known until run time, and so ARMCondAL should be
   passed, only so that printing of these instructions does not show
   any condition.

   Finally, the caller must indicate whether this occurs in ARM or
   Thumb code.
*/
static Bool decode_CP10_CP11_instruction (
               /*MOD*/DisResult* dres,
               UInt              insn28,
               IRTemp            condT,
               ARMCondcode       conq,
               Bool              isT
            )
{
#  define INSN(_bMax,_bMin)  SLICE_UInt(insn28, (_bMax), (_bMin))

   vassert(INSN(31,28) == BITS4(0,0,0,0)); // caller's obligation

   if (isT) {
      vassert(conq == ARMCondAL);
   } else {
      vassert(conq >= ARMCondEQ && conq <= ARMCondAL);
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
   if (BITS8(1,1,0,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,0,0,0,0,0))
       && INSN(11,8) == BITS4(1,0,1,1)) {
      UInt bP      = (insn28 >> 24) & 1;
      UInt bU      = (insn28 >> 23) & 1;
      UInt bW      = (insn28 >> 21) & 1;
      UInt bL      = (insn28 >> 20) & 1;
      UInt offset  = (insn28 >> 0) & 0xFF;
      UInt rN      = INSN(19,16);
      UInt dD      = (INSN(22,22) << 4) | INSN(15,12);
      UInt nRegs   = (offset - 1) / 2;
      UInt summary = 0;
      Int  i;

      /**/ if (bP == 0 && bU == 1 && bW == 0) {
         summary = 1;
      }
      else if (bP == 0 && bU == 1 && bW == 1) {
         summary = 2;
      }
      else if (bP == 1 && bU == 0 && bW == 1) {
         summary = 3;
      }
      else goto after_vfp_fldmx_fstmx;

      /* no writebacks to r15 allowed.  No use of r15 in thumb mode. */
      if (rN == 15 && (summary == 2 || summary == 3 || isT))
         goto after_vfp_fldmx_fstmx;

      /* offset must be odd, and specify at least one register */
      if (0 == (offset & 1) || offset < 3)
         goto after_vfp_fldmx_fstmx;

      /* can't transfer regs after D15 */
      if (dD + nRegs - 1 >= 32)
         goto after_vfp_fldmx_fstmx;

      /* Now, we can't do a conditional load or store, since that very
         likely will generate an exception.  So we have to take a side
         exit at this point if the condition is false. */
      if (condT != IRTemp_INVALID) {
         if (isT)
            mk_skip_over_T32_if_cond_is_false( condT );
         else
            mk_skip_over_A32_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      /* Ok, now we're unconditional.  Do the load or store. */

      /* get the old Rn value */
      IRTemp rnT = newTemp(Ity_I32);
      assign(rnT, align4if(isT ? getIRegT(rN) : getIRegA(rN),
                           rN == 15));

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
      if (summary == 3) {
         if (isT)
            putIRegT(rN, mkexpr(rnTnew), IRTemp_INVALID);
         else
            putIRegA(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);
      }

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
      if (summary == 2) {
         if (isT)
            putIRegT(rN, mkexpr(rnTnew), IRTemp_INVALID);
         else
            putIRegA(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);
      }

      const HChar* nm = bL==1 ? "ld" : "st";
      switch (summary) {
         case 1:  DIP("f%smx%s r%u, {d%u-d%u}\n", 
                      nm, nCC(conq), rN, dD, dD + nRegs - 1);
                  break;
         case 2:  DIP("f%smiax%s r%u!, {d%u-d%u}\n", 
                      nm, nCC(conq), rN, dD, dD + nRegs - 1);
                  break;
         case 3:  DIP("f%smdbx%s r%u!, {d%u-d%u}\n", 
                      nm, nCC(conq), rN, dD, dD + nRegs - 1);
                  break;
         default: vassert(0);
      }

      goto decode_success_vfp;
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
   if (BITS8(1,1,0,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,0,0,0,0,0))
       && INSN(11,8) == BITS4(1,0,1,1)) {
      UInt bP      = (insn28 >> 24) & 1;
      UInt bU      = (insn28 >> 23) & 1;
      UInt bW      = (insn28 >> 21) & 1;
      UInt bL      = (insn28 >> 20) & 1;
      UInt offset  = (insn28 >> 0) & 0xFF;
      UInt rN      = INSN(19,16);
      UInt dD      = (INSN(22,22) << 4) | INSN(15,12);
      UInt nRegs   = offset / 2;
      UInt summary = 0;
      Int  i;

      /**/ if (bP == 0 && bU == 1 && bW == 0) {
         summary = 1;
      }
      else if (bP == 0 && bU == 1 && bW == 1) {
         summary = 2;
      }
      else if (bP == 1 && bU == 0 && bW == 1) {
         summary = 3;
      }
      else goto after_vfp_fldmd_fstmd;

      /* no writebacks to r15 allowed.  No use of r15 in thumb mode. */
      if (rN == 15 && (summary == 2 || summary == 3 || isT))
         goto after_vfp_fldmd_fstmd;

      /* offset must be even, and specify at least one register */
      if (1 == (offset & 1) || offset < 2)
         goto after_vfp_fldmd_fstmd;

      /* can't transfer regs after D15 */
      if (dD + nRegs - 1 >= 32)
         goto after_vfp_fldmd_fstmd;

      /* Now, we can't do a conditional load or store, since that very
         likely will generate an exception.  So we have to take a side
         exit at this point if the condition is false. */
      if (condT != IRTemp_INVALID) {
         if (isT)
            mk_skip_over_T32_if_cond_is_false( condT );
         else
            mk_skip_over_A32_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      /* Ok, now we're unconditional.  Do the load or store. */

      /* get the old Rn value */
      IRTemp rnT = newTemp(Ity_I32);
      assign(rnT, align4if(isT ? getIRegT(rN) : getIRegA(rN),
                           rN == 15));

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
      if (summary == 3) {
         if (isT)
            putIRegT(rN, mkexpr(rnTnew), IRTemp_INVALID);
         else
            putIRegA(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);
      }

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
      if (summary == 2) {
         if (isT)
            putIRegT(rN, mkexpr(rnTnew), IRTemp_INVALID);
         else
            putIRegA(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);
      }

      const HChar* nm = bL==1 ? "ld" : "st";
      switch (summary) {
         case 1:  DIP("f%smd%s r%u, {d%u-d%u}\n", 
                      nm, nCC(conq), rN, dD, dD + nRegs - 1);
                  break;
         case 2:  DIP("f%smiad%s r%u!, {d%u-d%u}\n", 
                      nm, nCC(conq), rN, dD, dD + nRegs - 1);
                  break;
         case 3:  DIP("f%smdbd%s r%u!, {d%u-d%u}\n", 
                      nm, nCC(conq), rN, dD, dD + nRegs - 1);
                  break;
         default: vassert(0);
      }

      goto decode_success_vfp;
      /* FIXME alignment constraints? */
   }

  after_vfp_fldmd_fstmd:

   /* ------------------- fmrx, fmxr ------------------- */
   if (BITS8(1,1,1,0,1,1,1,1) == INSN(27,20)
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS8(0,0,0,1,0,0,0,0) == (insn28 & 0xFF)) {
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
            DIP("fmstat%s\n", nCC(conq));
         } else {
            /* Otherwise, merely transfer FPSCR to r0 .. r14. */
            IRExpr* e = IRExpr_Get(OFFB_FPSCR, Ity_I32);
            if (isT)
               putIRegT(rD, e, condT);
            else
               putIRegA(rD, e, condT, Ijk_Boring);
            DIP("fmrx%s r%u, fpscr\n", nCC(conq), rD);
         }
         goto decode_success_vfp;
      }
      /* fall through */
   }

   if (BITS8(1,1,1,0,1,1,1,0) == INSN(27,20)
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS8(0,0,0,1,0,0,0,0) == (insn28 & 0xFF)) {
      UInt rD  = INSN(15,12);
      UInt reg = INSN(19,16);
      if (reg == BITS4(0,0,0,1)) {
         putMiscReg32(OFFB_FPSCR,
                      isT ? getIRegT(rD) : getIRegA(rD), condT);
         DIP("fmxr%s fpscr, r%u\n", nCC(conq), rD);
         goto decode_success_vfp;
      }
      /* fall through */
   }

   /* --------------------- vmov --------------------- */
   // VMOV dM, rD, rN
   if (0x0C400B10 == (insn28 & 0x0FF00FD0)) {
      UInt dM = INSN(3,0) | (INSN(5,5) << 4);
      UInt rD = INSN(15,12); /* lo32 */
      UInt rN = INSN(19,16); /* hi32 */
      if (rD == 15 || rN == 15 || (isT && (rD == 13 || rN == 13))) {
         /* fall through */
      } else {
         putDReg(dM,
                 unop(Iop_ReinterpI64asF64,
                      binop(Iop_32HLto64,
                            isT ? getIRegT(rN) : getIRegA(rN),
                            isT ? getIRegT(rD) : getIRegA(rD))),
                 condT);
         DIP("vmov%s d%u, r%u, r%u\n", nCC(conq), dM, rD, rN);
         goto decode_success_vfp;
      }
      /* fall through */
   }

   // VMOV rD, rN, dM
   if (0x0C500B10 == (insn28 & 0x0FF00FD0)) {
      UInt dM = INSN(3,0) | (INSN(5,5) << 4);
      UInt rD = INSN(15,12); /* lo32 */
      UInt rN = INSN(19,16); /* hi32 */
      if (rD == 15 || rN == 15 || (isT && (rD == 13 || rN == 13))
          || rD == rN) {
         /* fall through */
      } else {
         IRTemp i64 = newTemp(Ity_I64);
         assign(i64, unop(Iop_ReinterpF64asI64, getDReg(dM)));
         IRExpr* hi32 = unop(Iop_64HIto32, mkexpr(i64));
         IRExpr* lo32 = unop(Iop_64to32,   mkexpr(i64));
         if (isT) {
            putIRegT(rN, hi32, condT);
            putIRegT(rD, lo32, condT);
         } else {
            putIRegA(rN, hi32, condT, Ijk_Boring);
            putIRegA(rD, lo32, condT, Ijk_Boring);
         }
         DIP("vmov%s r%u, r%u, d%u\n", nCC(conq), rD, rN, dM);
         goto decode_success_vfp;
      }
      /* fall through */
   }

   // VMOV sD, sD+1, rN, rM
   if (0x0C400A10 == (insn28 & 0x0FF00FD0)) {
      UInt sD = (INSN(3,0) << 1) | INSN(5,5);
      UInt rN = INSN(15,12);
      UInt rM = INSN(19,16);
      if (rM == 15 || rN == 15 || (isT && (rM == 13 || rN == 13))
          || sD == 31) {
         /* fall through */
      } else {
         putFReg(sD,
                 unop(Iop_ReinterpI32asF32, isT ? getIRegT(rN) : getIRegA(rN)),
                 condT);
         putFReg(sD+1,
                 unop(Iop_ReinterpI32asF32, isT ? getIRegT(rM) : getIRegA(rM)),
                 condT);
         DIP("vmov%s, s%u, s%u, r%u, r%u\n",
              nCC(conq), sD, sD + 1, rN, rM);
         goto decode_success_vfp;
      }
   }

   // VMOV rN, rM, sD, sD+1
   if (0x0C500A10 == (insn28 & 0x0FF00FD0)) {
      UInt sD = (INSN(3,0) << 1) | INSN(5,5);
      UInt rN = INSN(15,12);
      UInt rM = INSN(19,16);
      if (rM == 15 || rN == 15 || (isT && (rM == 13 || rN == 13))
          || sD == 31 || rN == rM) {
         /* fall through */
      } else {
         IRExpr* res0 = unop(Iop_ReinterpF32asI32, getFReg(sD));
         IRExpr* res1 = unop(Iop_ReinterpF32asI32, getFReg(sD+1));
         if (isT) {
            putIRegT(rN, res0, condT);
            putIRegT(rM, res1, condT);
         } else {
            putIRegA(rN, res0, condT, Ijk_Boring);
            putIRegA(rM, res1, condT, Ijk_Boring);
         }
         DIP("vmov%s, r%u, r%u, s%u, s%u\n",
             nCC(conq), rN, rM, sD, sD + 1);
         goto decode_success_vfp;
      }
   }

   // VMOV rD[x], rT  (ARM core register to scalar)
   if (0x0E000B10 == (insn28 & 0x0F900F1F)) {
      UInt rD  = (INSN(7,7) << 4) | INSN(19,16);
      UInt rT  = INSN(15,12);
      UInt opc = (INSN(22,21) << 2) | INSN(6,5);
      UInt index;
      if (rT == 15 || (isT && rT == 13)) {
         /* fall through */
      } else {
         if ((opc & BITS4(1,0,0,0)) == BITS4(1,0,0,0)) {
            index = opc & 7;
            putDRegI64(rD, triop(Iop_SetElem8x8,
                                 getDRegI64(rD),
                                 mkU8(index),
                                 unop(Iop_32to8,
                                      isT ? getIRegT(rT) : getIRegA(rT))),
                           condT);
            DIP("vmov%s.8 d%u[%u], r%u\n", nCC(conq), rD, index, rT);
            goto decode_success_vfp;
         }
         else if ((opc & BITS4(1,0,0,1)) == BITS4(0,0,0,1)) {
            index = (opc >> 1) & 3;
            putDRegI64(rD, triop(Iop_SetElem16x4,
                                 getDRegI64(rD),
                                 mkU8(index),
                                 unop(Iop_32to16,
                                      isT ? getIRegT(rT) : getIRegA(rT))),
                           condT);
            DIP("vmov%s.16 d%u[%u], r%u\n", nCC(conq), rD, index, rT);
            goto decode_success_vfp;
         }
         else if ((opc & BITS4(1,0,1,1)) == BITS4(0,0,0,0)) {
            index = (opc >> 2) & 1;
            putDRegI64(rD, triop(Iop_SetElem32x2,
                                 getDRegI64(rD),
                                 mkU8(index),
                                 isT ? getIRegT(rT) : getIRegA(rT)),
                           condT);
            DIP("vmov%s.32 d%u[%u], r%u\n", nCC(conq), rD, index, rT);
            goto decode_success_vfp;
         } else {
            /* fall through */
         }
      }
   }

   // VMOV (scalar to ARM core register)
   // VMOV rT, rD[x]
   if (0x0E100B10 == (insn28 & 0x0F100F1F)) {
      UInt rN  = (INSN(7,7) << 4) | INSN(19,16);
      UInt rT  = INSN(15,12);
      UInt U   = INSN(23,23);
      UInt opc = (INSN(22,21) << 2) | INSN(6,5);
      UInt index;
      if (rT == 15 || (isT && rT == 13)) {
         /* fall through */
      } else {
         if ((opc & BITS4(1,0,0,0)) == BITS4(1,0,0,0)) {
            index = opc & 7;
            IRExpr* e = unop(U ? Iop_8Uto32 : Iop_8Sto32,
                             binop(Iop_GetElem8x8,
                                   getDRegI64(rN),
                                   mkU8(index)));
            if (isT)
               putIRegT(rT, e, condT);
            else
               putIRegA(rT, e, condT, Ijk_Boring);
            DIP("vmov%s.%c8 r%u, d%u[%u]\n", nCC(conq), U ? 'u' : 's',
                  rT, rN, index);
            goto decode_success_vfp;
         }
         else if ((opc & BITS4(1,0,0,1)) == BITS4(0,0,0,1)) {
            index = (opc >> 1) & 3;
            IRExpr* e = unop(U ? Iop_16Uto32 : Iop_16Sto32,
                             binop(Iop_GetElem16x4,
                                   getDRegI64(rN),
                                   mkU8(index)));
            if (isT)
               putIRegT(rT, e, condT);
            else
               putIRegA(rT, e, condT, Ijk_Boring);
            DIP("vmov%s.%c16 r%u, d%u[%u]\n", nCC(conq), U ? 'u' : 's',
                  rT, rN, index);
            goto decode_success_vfp;
         }
         else if ((opc & BITS4(1,0,1,1)) == BITS4(0,0,0,0) && U == 0) {
            index = (opc >> 2) & 1;
            IRExpr* e = binop(Iop_GetElem32x2, getDRegI64(rN), mkU8(index));
            if (isT)
               putIRegT(rT, e, condT);
            else
               putIRegA(rT, e, condT, Ijk_Boring);
            DIP("vmov%s.32 r%u, d%u[%u]\n", nCC(conq), rT, rN, index);
            goto decode_success_vfp;
         } else {
            /* fall through */
         }
      }
   }

   // VMOV.F32 sD, #imm
   // FCONSTS sD, #imm
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,0,0,0) == INSN(7,4) && INSN(11,8) == BITS4(1,0,1,0)) {
      UInt rD   = (INSN(15,12) << 1) | INSN(22,22);
      UInt imm8 = (INSN(19,16) << 4) | INSN(3,0);
      UInt b    = (imm8 >> 6) & 1;
      UInt imm;
      imm = (BITS8((imm8 >> 7) & 1,(~b) & 1,b,b,b,b,b,(imm8 >> 5) & 1) << 8)
             | ((imm8 & 0x1f) << 3);
      imm <<= 16;
      putFReg(rD, unop(Iop_ReinterpI32asF32, mkU32(imm)), condT);
      DIP("fconsts%s s%u #%u", nCC(conq), rD, imm8);
      goto decode_success_vfp;
   }

   // VMOV.F64 dD, #imm
   // FCONSTD dD, #imm
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,0,0,0) == INSN(7,4) && INSN(11,8) == BITS4(1,0,1,1)) {
      UInt rD   = INSN(15,12) | (INSN(22,22) << 4);
      UInt imm8 = (INSN(19,16) << 4) | INSN(3,0);
      UInt b    = (imm8 >> 6) & 1;
      ULong imm;
      imm = (BITS8((imm8 >> 7) & 1,(~b) & 1,b,b,b,b,b,b) << 8)
             | BITS8(b,b,0,0,0,0,0,0) | (imm8 & 0x3f);
      imm <<= 48;
      putDReg(rD, unop(Iop_ReinterpI64asF64, mkU64(imm)), condT);
      DIP("fconstd%s d%u #%u", nCC(conq), rD, imm8);
      goto decode_success_vfp;
   }

   /* ---------------------- vdup ------------------------- */
   // VDUP dD, rT
   // VDUP qD, rT
   if (BITS8(1,1,1,0,1,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,1,0,0,1))
       && BITS4(1,0,1,1) == INSN(11,8) && INSN(6,6) == 0 && INSN(4,4) == 1) {
      UInt rD   = (INSN(7,7) << 4) | INSN(19,16);
      UInt rT   = INSN(15,12);
      UInt Q    = INSN(21,21);
      UInt size = (INSN(22,22) << 1) | INSN(5,5);
      if (rT == 15 || (isT && rT == 13) || size == 3 || (Q && (rD & 1))) {
         /* fall through */
      } else {
         IRExpr* e = isT ? getIRegT(rT) : getIRegA(rT);
         if (Q) {
            rD >>= 1;
            switch (size) {
               case 0:
                  putQReg(rD, unop(Iop_Dup32x4, e), condT);
                  break;
               case 1:
                  putQReg(rD, unop(Iop_Dup16x8, unop(Iop_32to16, e)),
                              condT);
                  break;
               case 2:
                  putQReg(rD, unop(Iop_Dup8x16, unop(Iop_32to8, e)),
                              condT);
                  break;
               default:
                  vassert(0);
            }
            DIP("vdup.%d q%u, r%u\n", 32 / (1<<size), rD, rT);
         } else {
            switch (size) {
               case 0:
                  putDRegI64(rD, unop(Iop_Dup32x2, e), condT);
                  break;
               case 1:
                  putDRegI64(rD, unop(Iop_Dup16x4, unop(Iop_32to16, e)),
                               condT);
                  break;
               case 2:
                  putDRegI64(rD, unop(Iop_Dup8x8, unop(Iop_32to8, e)),
                               condT);
                  break;
               default:
                  vassert(0);
            }
            DIP("vdup.%d d%u, r%u\n", 32 / (1<<size), rD, rT);
         }
         goto decode_success_vfp;
      }
   }

   /* --------------------- f{ld,st}d --------------------- */
   // FLDD, FSTD
   if (BITS8(1,1,0,1,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,0,1,0))
       && BITS4(1,0,1,1) == INSN(11,8)) {
      UInt dD     = INSN(15,12) | (INSN(22,22) << 4);
      UInt rN     = INSN(19,16);
      UInt offset = (insn28 & 0xFF) << 2;
      UInt bU     = (insn28 >> 23) & 1; /* 1: +offset  0: -offset */
      UInt bL     = (insn28 >> 20) & 1; /* 1: load  0: store */
      /* make unconditional */
      if (condT != IRTemp_INVALID) {
         if (isT)
            mk_skip_over_T32_if_cond_is_false( condT );
         else
            mk_skip_over_A32_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      IRTemp ea = newTemp(Ity_I32);
      assign(ea, binop(bU ? Iop_Add32 : Iop_Sub32,
                       align4if(isT ? getIRegT(rN) : getIRegA(rN),
                                rN == 15),
                       mkU32(offset)));
      if (bL) {
         putDReg(dD, loadLE(Ity_F64,mkexpr(ea)), IRTemp_INVALID);
      } else {
         storeLE(mkexpr(ea), getDReg(dD));
      }
      DIP("f%sd%s d%u, [r%u, %c#%u]\n",
          bL ? "ld" : "st", nCC(conq), dD, rN,
          bU ? '+' : '-', offset);
      goto decode_success_vfp;
   }

   /* --------------------- dp insns (D) --------------------- */
   if (BITS8(1,1,1,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,0,0,0))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,0,0,0) == (INSN(7,4) & BITS4(0,0,0,1))) {
      UInt    dM  = INSN(3,0)   | (INSN(5,5) << 4);       /* argR */
      UInt    dD  = INSN(15,12) | (INSN(22,22) << 4);   /* dst/acc */
      UInt    dN  = INSN(19,16) | (INSN(7,7) << 4);     /* argL */
      UInt    bP  = (insn28 >> 23) & 1;
      UInt    bQ  = (insn28 >> 21) & 1;
      UInt    bR  = (insn28 >> 20) & 1;
      UInt    bS  = (insn28 >> 6) & 1;
      UInt    opc = (bP << 3) | (bQ << 2) | (bR << 1) | bS;
      IRExpr* rm  = get_FAKE_roundingmode(); /* XXXROUNDINGFIXME */
      switch (opc) {
         case BITS4(0,0,0,0): /* MAC: d + n * m */
            putDReg(dD, triop(Iop_AddF64, rm,
                              getDReg(dD),
                              triop(Iop_MulF64, rm, getDReg(dN),
                                                    getDReg(dM))),
                        condT);
            DIP("fmacd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(0,0,0,1): /* NMAC: d + -(n * m) */
            putDReg(dD, triop(Iop_AddF64, rm,
                              getDReg(dD),
                              unop(Iop_NegF64,
                                   triop(Iop_MulF64, rm, getDReg(dN),
                                                         getDReg(dM)))),
                        condT);
            DIP("fnmacd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(0,0,1,0): /* MSC: - d + n * m */
            putDReg(dD, triop(Iop_AddF64, rm,
                              unop(Iop_NegF64, getDReg(dD)),
                              triop(Iop_MulF64, rm, getDReg(dN),
                                                    getDReg(dM))),
                        condT);
            DIP("fmscd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(0,0,1,1): /* NMSC: - d + -(n * m) */
            putDReg(dD, triop(Iop_AddF64, rm,
                              unop(Iop_NegF64, getDReg(dD)),
                              unop(Iop_NegF64,
                                   triop(Iop_MulF64, rm, getDReg(dN),
                                                         getDReg(dM)))),
                        condT);
            DIP("fnmscd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(0,1,0,0): /* MUL: n * m */
            putDReg(dD, triop(Iop_MulF64, rm, getDReg(dN), getDReg(dM)),
                        condT);
            DIP("fmuld%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(0,1,0,1): /* NMUL: - n * m */
            putDReg(dD, unop(Iop_NegF64,
                             triop(Iop_MulF64, rm, getDReg(dN),
                                                   getDReg(dM))),
                    condT);
            DIP("fnmuld%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(0,1,1,0): /* ADD: n + m */
            putDReg(dD, triop(Iop_AddF64, rm, getDReg(dN), getDReg(dM)),
                        condT);
            DIP("faddd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(0,1,1,1): /* SUB: n - m */
            putDReg(dD, triop(Iop_SubF64, rm, getDReg(dN), getDReg(dM)),
                        condT);
            DIP("fsubd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(1,0,0,0): /* DIV: n / m */
            putDReg(dD, triop(Iop_DivF64, rm, getDReg(dN), getDReg(dM)),
                        condT);
            DIP("fdivd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(1,0,1,0): /* VNFMS: -(d - n * m) (fused) */
            /* XXXROUNDINGFIXME look up ARM reference for fused
               multiply-add rounding */
            putDReg(dD, triop(Iop_AddF64, rm,
                              unop(Iop_NegF64, getDReg(dD)),
                              triop(Iop_MulF64, rm,
                                                getDReg(dN),
                                                getDReg(dM))),
                        condT);
            DIP("vfnmsd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(1,0,1,1): /* VNFMA: -(d + n * m) (fused) */
            /* XXXROUNDINGFIXME look up ARM reference for fused
               multiply-add rounding */
            putDReg(dD, triop(Iop_AddF64, rm,
                              unop(Iop_NegF64, getDReg(dD)),
                              triop(Iop_MulF64, rm,
                                                unop(Iop_NegF64, getDReg(dN)),
                                                getDReg(dM))),
                        condT);
            DIP("vfnmad%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(1,1,0,0): /* VFMA: d + n * m (fused) */
            /* XXXROUNDINGFIXME look up ARM reference for fused
               multiply-add rounding */
            putDReg(dD, triop(Iop_AddF64, rm,
                              getDReg(dD),
                              triop(Iop_MulF64, rm, getDReg(dN),
                                                    getDReg(dM))),
                        condT);
            DIP("vfmad%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         case BITS4(1,1,0,1): /* VFMS: d + (-n * m) (fused) */
            /* XXXROUNDINGFIXME look up ARM reference for fused
               multiply-add rounding */
            putDReg(dD, triop(Iop_AddF64, rm,
                              getDReg(dD),
                              triop(Iop_MulF64, rm,
                                    unop(Iop_NegF64, getDReg(dN)),
                                    getDReg(dM))),
                        condT);
            DIP("vfmsd%s d%u, d%u, d%u\n", nCC(conq), dD, dN, dM);
            goto decode_success_vfp;
         default:
            break;
      }
   }

   /* --------------------- compares (D) --------------------- */
   /*          31   27   23   19   15 11   7    3
                 28   24   20   16 12    8    4    0 
      FCMPD    cond 1110 1D11 0100 Dd 1011 0100 Dm
      FCMPED   cond 1110 1D11 0100 Dd 1011 1100 Dm
      FCMPZD   cond 1110 1D11 0101 Dd 1011 0100 0000
      FCMPZED  cond 1110 1D11 0101 Dd 1011 1100 0000
                                 Z         N

      Z=0 Compare Dd vs Dm     and set FPSCR 31:28 accordingly
      Z=1 Compare Dd vs zero

      N=1 generates Invalid Operation exn if either arg is any kind of NaN
      N=0 generates Invalid Operation exn if either arg is a signalling NaN
      (Not that we pay any attention to N here)
   */
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,1,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt bZ = (insn28 >> 16) & 1;
      UInt bN = (insn28 >> 7) & 1;
      UInt dD = INSN(15,12) | (INSN(22,22) << 4);
      UInt dM = INSN(3,0) | (INSN(5,5) << 4);
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
            DIP("fcmpz%sd%s d%u\n", bN ? "e" : "", nCC(conq), dD);
         } else {
            DIP("fcmp%sd%s d%u, d%u\n", bN ? "e" : "", nCC(conq), dD, dM);
         }
         goto decode_success_vfp;
      }
      /* fall through */
   }  

   /* --------------------- unary (D) --------------------- */
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,0,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt dD  = INSN(15,12) | (INSN(22,22) << 4);
      UInt dM  = INSN(3,0) | (INSN(5,5) << 4);
      UInt b16 = (insn28 >> 16) & 1;
      UInt b7  = (insn28 >> 7) & 1;
      /**/ if (b16 == 0 && b7 == 0) {
         // FCPYD
         putDReg(dD, getDReg(dM), condT);
         DIP("fcpyd%s d%u, d%u\n", nCC(conq), dD, dM);
         goto decode_success_vfp;
      }
      else if (b16 == 0 && b7 == 1) {
         // FABSD
         putDReg(dD, unop(Iop_AbsF64, getDReg(dM)), condT);
         DIP("fabsd%s d%u, d%u\n", nCC(conq), dD, dM);
         goto decode_success_vfp;
      }
      else if (b16 == 1 && b7 == 0) {
         // FNEGD
         putDReg(dD, unop(Iop_NegF64, getDReg(dM)), condT);
         DIP("fnegd%s d%u, d%u\n", nCC(conq), dD, dM);
         goto decode_success_vfp;
      }
      else if (b16 == 1 && b7 == 1) {
         // FSQRTD
         IRExpr* rm = get_FAKE_roundingmode(); /* XXXROUNDINGFIXME */
         putDReg(dD, binop(Iop_SqrtF64, rm, getDReg(dM)), condT);
         DIP("fsqrtd%s d%u, d%u\n", nCC(conq), dD, dM);
         goto decode_success_vfp;
      }
      else
         vassert(0);

      /* fall through */
   }

   /* ----------------- I <-> D conversions ----------------- */

   // F{S,U}ITOD dD, fM
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(1,0,0,0) == (INSN(19,16) & BITS4(1,1,1,1))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt bM    = (insn28 >> 5) & 1;
      UInt fM    = (INSN(3,0) << 1) | bM;
      UInt dD    = INSN(15,12) | (INSN(22,22) << 4);
      UInt syned = (insn28 >> 7) & 1;
      if (syned) {
         // FSITOD
         putDReg(dD, unop(Iop_I32StoF64,
                          unop(Iop_ReinterpF32asI32, getFReg(fM))),
                 condT);
         DIP("fsitod%s d%u, s%u\n", nCC(conq), dD, fM);
      } else {
         // FUITOD
         putDReg(dD, unop(Iop_I32UtoF64,
                          unop(Iop_ReinterpF32asI32, getFReg(fM))),
                 condT);
         DIP("fuitod%s d%u, s%u\n", nCC(conq), dD, fM);
      }
      goto decode_success_vfp;
   }

   // FTO{S,U}ID fD, dM
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(1,1,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt   bD    = (insn28 >> 22) & 1;
      UInt   fD    = (INSN(15,12) << 1) | bD;
      UInt   dM    = INSN(3,0) | (INSN(5,5) << 4);
      UInt   bZ    = (insn28 >> 7) & 1;
      UInt   syned = (insn28 >> 16) & 1;
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
             nCC(conq), fD, dM);
      } else {
         // FTOUID
         putFReg(fD, unop(Iop_ReinterpI32asF32,
                          binop(Iop_F64toI32U, mkexpr(rmode),
                                getDReg(dM))),
                 condT);
         DIP("ftoui%sd%s s%u, d%u\n", bZ ? "z" : "",
             nCC(conq), fD, dM);
      }
      goto decode_success_vfp;
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
      UInt bP      = (insn28 >> 24) & 1;
      UInt bU      = (insn28 >> 23) & 1;
      UInt bW      = (insn28 >> 21) & 1;
      UInt bL      = (insn28 >> 20) & 1;
      UInt bD      = (insn28 >> 22) & 1;
      UInt offset  = (insn28 >> 0) & 0xFF;
      UInt rN      = INSN(19,16);
      UInt fD      = (INSN(15,12) << 1) | bD;
      UInt nRegs   = offset;
      UInt summary = 0;
      Int  i;

      /**/ if (bP == 0 && bU == 1 && bW == 0) {
         summary = 1;
      }
      else if (bP == 0 && bU == 1 && bW == 1) {
         summary = 2;
      }
      else if (bP == 1 && bU == 0 && bW == 1) {
         summary = 3;
      }
      else goto after_vfp_fldms_fstms;

      /* no writebacks to r15 allowed.  No use of r15 in thumb mode. */
      if (rN == 15 && (summary == 2 || summary == 3 || isT))
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
         if (isT)
            mk_skip_over_T32_if_cond_is_false( condT );
         else
            mk_skip_over_A32_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      /* Ok, now we're unconditional.  Do the load or store. */

      /* get the old Rn value */
      IRTemp rnT = newTemp(Ity_I32);
      assign(rnT, align4if(isT ? getIRegT(rN) : getIRegA(rN),
                           rN == 15));

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
      if (summary == 3) {
         if (isT)
            putIRegT(rN, mkexpr(rnTnew), IRTemp_INVALID);
         else
            putIRegA(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);
      }

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
      if (summary == 2) {
         if (isT)
            putIRegT(rN, mkexpr(rnTnew), IRTemp_INVALID);
         else
            putIRegA(rN, mkexpr(rnTnew), IRTemp_INVALID, Ijk_Boring);
      }

      const HChar* nm = bL==1 ? "ld" : "st";
      switch (summary) {
         case 1:  DIP("f%sms%s r%u, {s%u-s%u}\n", 
                      nm, nCC(conq), rN, fD, fD + nRegs - 1);
                  break;
         case 2:  DIP("f%smias%s r%u!, {s%u-s%u}\n", 
                      nm, nCC(conq), rN, fD, fD + nRegs - 1);
                  break;
         case 3:  DIP("f%smdbs%s r%u!, {s%u-s%u}\n", 
                      nm, nCC(conq), rN, fD, fD + nRegs - 1);
                  break;
         default: vassert(0);
      }

      goto decode_success_vfp;
      /* FIXME alignment constraints? */
   }

  after_vfp_fldms_fstms:

   /* --------------------- fmsr, fmrs --------------------- */
   if (BITS8(1,1,1,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,1,1,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,0,0,0) == INSN(3,0)
       && BITS4(0,0,0,1) == (INSN(7,4) & BITS4(0,1,1,1))) {
      UInt rD  = INSN(15,12);
      UInt b7  = (insn28 >> 7) & 1;
      UInt fN  = (INSN(19,16) << 1) | b7;
      UInt b20 = (insn28 >> 20) & 1;
      if (rD == 15) {
         /* fall through */
         /* Let's assume that no sane person would want to do
            floating-point transfers to or from the program counter,
            and simply decline to decode the instruction.  The ARM ARM
            doesn't seem to explicitly disallow this case, though. */
      } else {
         if (b20) {
            IRExpr* res = unop(Iop_ReinterpF32asI32, getFReg(fN));
            if (isT)
               putIRegT(rD, res, condT);
            else
               putIRegA(rD, res, condT, Ijk_Boring);
            DIP("fmrs%s r%u, s%u\n", nCC(conq), rD, fN);
         } else {
            putFReg(fN, unop(Iop_ReinterpI32asF32,
                             isT ? getIRegT(rD) : getIRegA(rD)),
                        condT);
            DIP("fmsr%s s%u, r%u\n", nCC(conq), fN, rD);
         }
         goto decode_success_vfp;
      }
      /* fall through */
   }

   /* --------------------- f{ld,st}s --------------------- */
   // FLDS, FSTS
   if (BITS8(1,1,0,1,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,0,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)) {
      UInt bD     = (insn28 >> 22) & 1;
      UInt fD     = (INSN(15,12) << 1) | bD;
      UInt rN     = INSN(19,16);
      UInt offset = (insn28 & 0xFF) << 2;
      UInt bU     = (insn28 >> 23) & 1; /* 1: +offset  0: -offset */
      UInt bL     = (insn28 >> 20) & 1; /* 1: load  0: store */
      /* make unconditional */
      if (condT != IRTemp_INVALID) {
         if (isT)
            mk_skip_over_T32_if_cond_is_false( condT );
         else
            mk_skip_over_A32_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }
      IRTemp ea = newTemp(Ity_I32);
      assign(ea, binop(bU ? Iop_Add32 : Iop_Sub32,
                       align4if(isT ? getIRegT(rN) : getIRegA(rN),
                                rN == 15),
                       mkU32(offset)));
      if (bL) {
         putFReg(fD, loadLE(Ity_F32,mkexpr(ea)), IRTemp_INVALID);
      } else {
         storeLE(mkexpr(ea), getFReg(fD));
      }
      DIP("f%ss%s s%u, [r%u, %c#%u]\n",
          bL ? "ld" : "st", nCC(conq), fD, rN,
          bU ? '+' : '-', offset);
      goto decode_success_vfp;
   }

   /* --------------------- dp insns (F) --------------------- */
   if (BITS8(1,1,1,0,0,0,0,0) == (INSN(27,20) & BITS8(1,1,1,1,0,0,0,0))
       && BITS4(1,0,1,0) == (INSN(11,8) & BITS4(1,1,1,0))
       && BITS4(0,0,0,0) == (INSN(7,4) & BITS4(0,0,0,1))) {
      UInt    bM  = (insn28 >> 5) & 1;
      UInt    bD  = (insn28 >> 22) & 1;
      UInt    bN  = (insn28 >> 7) & 1;
      UInt    fM  = (INSN(3,0) << 1) | bM;   /* argR */
      UInt    fD  = (INSN(15,12) << 1) | bD; /* dst/acc */
      UInt    fN  = (INSN(19,16) << 1) | bN; /* argL */
      UInt    bP  = (insn28 >> 23) & 1;
      UInt    bQ  = (insn28 >> 21) & 1;
      UInt    bR  = (insn28 >> 20) & 1;
      UInt    bS  = (insn28 >> 6) & 1;
      UInt    opc = (bP << 3) | (bQ << 2) | (bR << 1) | bS;
      IRExpr* rm  = get_FAKE_roundingmode(); /* XXXROUNDINGFIXME */
      switch (opc) {
         case BITS4(0,0,0,0): /* MAC: d + n * m */
            putFReg(fD, triop(Iop_AddF32, rm,
                              getFReg(fD),
                              triop(Iop_MulF32, rm, getFReg(fN), getFReg(fM))),
                        condT);
            DIP("fmacs%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(0,0,0,1): /* NMAC: d + -(n * m) */
            putFReg(fD, triop(Iop_AddF32, rm,
                              getFReg(fD),
                              unop(Iop_NegF32,
                                   triop(Iop_MulF32, rm, getFReg(fN),
                                                         getFReg(fM)))),
                        condT);
            DIP("fnmacs%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(0,0,1,0): /* MSC: - d + n * m */
            putFReg(fD, triop(Iop_AddF32, rm,
                              unop(Iop_NegF32, getFReg(fD)),
                              triop(Iop_MulF32, rm, getFReg(fN), getFReg(fM))),
                        condT);
            DIP("fmscs%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(0,0,1,1): /* NMSC: - d + -(n * m) */
            putFReg(fD, triop(Iop_AddF32, rm,
                              unop(Iop_NegF32, getFReg(fD)),
                              unop(Iop_NegF32,
                                   triop(Iop_MulF32, rm,
                                                     getFReg(fN),
                                                    getFReg(fM)))),
                        condT);
            DIP("fnmscs%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(0,1,0,0): /* MUL: n * m */
            putFReg(fD, triop(Iop_MulF32, rm, getFReg(fN), getFReg(fM)),
                        condT);
            DIP("fmuls%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(0,1,0,1): /* NMUL: - n * m */
            putFReg(fD, unop(Iop_NegF32,
                             triop(Iop_MulF32, rm, getFReg(fN),
                                                   getFReg(fM))),
                    condT);
            DIP("fnmuls%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(0,1,1,0): /* ADD: n + m */
            putFReg(fD, triop(Iop_AddF32, rm, getFReg(fN), getFReg(fM)),
                        condT);
            DIP("fadds%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(0,1,1,1): /* SUB: n - m */
            putFReg(fD, triop(Iop_SubF32, rm, getFReg(fN), getFReg(fM)),
                        condT);
            DIP("fsubs%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(1,0,0,0): /* DIV: n / m */
            putFReg(fD, triop(Iop_DivF32, rm, getFReg(fN), getFReg(fM)),
                        condT);
            DIP("fdivs%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(1,0,1,0): /* VNFMS: -(d - n * m) (fused) */
            /* XXXROUNDINGFIXME look up ARM reference for fused
               multiply-add rounding */
            putFReg(fD, triop(Iop_AddF32, rm,
                              unop(Iop_NegF32, getFReg(fD)),
                              triop(Iop_MulF32, rm,
                                                getFReg(fN),
                                                getFReg(fM))),
                        condT);
            DIP("vfnmss%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(1,0,1,1): /* VNFMA: -(d + n * m) (fused) */
            /* XXXROUNDINGFIXME look up ARM reference for fused
               multiply-add rounding */
            putFReg(fD, triop(Iop_AddF32, rm,
                              unop(Iop_NegF32, getFReg(fD)),
                              triop(Iop_MulF32, rm,
                                                unop(Iop_NegF32, getFReg(fN)),
                                                getFReg(fM))),
                        condT);
            DIP("vfnmas%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(1,1,0,0): /* VFMA: d + n * m (fused) */
            /* XXXROUNDINGFIXME look up ARM reference for fused
               multiply-add rounding */
            putFReg(fD, triop(Iop_AddF32, rm,
                              getFReg(fD),
                              triop(Iop_MulF32, rm, getFReg(fN),
                                                    getFReg(fM))),
                        condT);
            DIP("vfmas%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
         case BITS4(1,1,0,1): /* VFMS: d + (-n * m) (fused) */
            /* XXXROUNDINGFIXME look up ARM reference for fused
               multiply-add rounding */
            putFReg(fD, triop(Iop_AddF32, rm,
                              getFReg(fD),
                              triop(Iop_MulF32, rm,
                                    unop(Iop_NegF32, getFReg(fN)),
                                    getFReg(fM))),
                        condT);
            DIP("vfmss%s s%u, s%u, s%u\n", nCC(conq), fD, fN, fM);
            goto decode_success_vfp;
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
      UInt bZ = (insn28 >> 16) & 1;
      UInt bN = (insn28 >> 7) & 1;
      UInt bD = (insn28 >> 22) & 1;
      UInt bM = (insn28 >> 5) & 1;
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
            DIP("fcmpz%ss%s s%u\n", bN ? "e" : "", nCC(conq), fD);
         } else {
            DIP("fcmp%ss%s s%u, s%u\n", bN ? "e" : "",
                nCC(conq), fD, fM);
         }
         goto decode_success_vfp;
      }
      /* fall through */
   }  

   /* --------------------- unary (S) --------------------- */
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,0,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt bD = (insn28 >> 22) & 1;
      UInt bM = (insn28 >> 5) & 1;
      UInt fD  = (INSN(15,12) << 1) | bD;
      UInt fM  = (INSN(3,0) << 1) | bM;
      UInt b16 = (insn28 >> 16) & 1;
      UInt b7  = (insn28 >> 7) & 1;
      /**/ if (b16 == 0 && b7 == 0) {
         // FCPYS
         putFReg(fD, getFReg(fM), condT);
         DIP("fcpys%s s%u, s%u\n", nCC(conq), fD, fM);
         goto decode_success_vfp;
      }
      else if (b16 == 0 && b7 == 1) {
         // FABSS
         putFReg(fD, unop(Iop_AbsF32, getFReg(fM)), condT);
         DIP("fabss%s s%u, s%u\n", nCC(conq), fD, fM);
         goto decode_success_vfp;
      }
      else if (b16 == 1 && b7 == 0) {
         // FNEGS
         putFReg(fD, unop(Iop_NegF32, getFReg(fM)), condT);
         DIP("fnegs%s s%u, s%u\n", nCC(conq), fD, fM);
         goto decode_success_vfp;
      }
      else if (b16 == 1 && b7 == 1) {
         // FSQRTS
         IRExpr* rm = get_FAKE_roundingmode(); /* XXXROUNDINGFIXME */
         putFReg(fD, binop(Iop_SqrtF32, rm, getFReg(fM)), condT);
         DIP("fsqrts%s s%u, s%u\n", nCC(conq), fD, fM);
         goto decode_success_vfp;
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
       && BITS4(1,0,0,0) == INSN(19,16)
       && BITS4(1,0,1,0) == (INSN(11,8) & BITS4(1,1,1,0))
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt bM    = (insn28 >> 5) & 1;
      UInt bD    = (insn28 >> 22) & 1;
      UInt fM    = (INSN(3,0) << 1) | bM;
      UInt fD    = (INSN(15,12) << 1) | bD;
      UInt syned = (insn28 >> 7) & 1;
      IRTemp rmode = newTemp(Ity_I32);
      assign(rmode, mkexpr(mk_get_IR_rounding_mode()));
      if (syned) {
         // FSITOS
         putFReg(fD, binop(Iop_F64toF32,
                           mkexpr(rmode),
                           unop(Iop_I32StoF64,
                                unop(Iop_ReinterpF32asI32, getFReg(fM)))),
                 condT);
         DIP("fsitos%s s%u, s%u\n", nCC(conq), fD, fM);
      } else {
         // FUITOS
         putFReg(fD, binop(Iop_F64toF32,
                           mkexpr(rmode),
                           unop(Iop_I32UtoF64,
                                unop(Iop_ReinterpF32asI32, getFReg(fM)))),
                 condT);
         DIP("fuitos%s s%u, s%u\n", nCC(conq), fD, fM);
      }
      goto decode_success_vfp;
   }

   // FTO{S,U}IS fD, fM
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(1,1,0,0) == (INSN(19,16) & BITS4(1,1,1,0))
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(0,1,0,0) == (INSN(7,4) & BITS4(0,1,0,1))) {
      UInt   bM    = (insn28 >> 5) & 1;
      UInt   bD    = (insn28 >> 22) & 1;
      UInt   fD    = (INSN(15,12) << 1) | bD;
      UInt   fM    = (INSN(3,0) << 1) | bM;
      UInt   bZ    = (insn28 >> 7) & 1;
      UInt   syned = (insn28 >> 16) & 1;
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
             nCC(conq), fD, fM);
         goto decode_success_vfp;
      } else {
         // FTOUIS
         putFReg(fD, unop(Iop_ReinterpI32asF32,
                          binop(Iop_F64toI32U, mkexpr(rmode),
                                unop(Iop_F32toF64, getFReg(fM)))),
                 condT);
         DIP("ftoui%ss%s s%u, d%u\n", bZ ? "z" : "",
             nCC(conq), fD, fM);
         goto decode_success_vfp;
      }
   }

   /* ----------------- S <-> D conversions ----------------- */

   // FCVTDS
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,1,1,1) == INSN(19,16)
       && BITS4(1,0,1,0) == INSN(11,8)
       && BITS4(1,1,0,0) == (INSN(7,4) & BITS4(1,1,0,1))) {
      UInt dD = INSN(15,12) | (INSN(22,22) << 4);
      UInt bM = (insn28 >> 5) & 1;
      UInt fM = (INSN(3,0) << 1) | bM;
      putDReg(dD, unop(Iop_F32toF64, getFReg(fM)), condT);
      DIP("fcvtds%s d%u, s%u\n", nCC(conq), dD, fM);
      goto decode_success_vfp;
   }

   // FCVTSD
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(0,1,1,1) == INSN(19,16)
       && BITS4(1,0,1,1) == INSN(11,8)
       && BITS4(1,1,0,0) == (INSN(7,4) & BITS4(1,1,0,1))) {
      UInt   bD    = (insn28 >> 22) & 1;
      UInt   fD    = (INSN(15,12) << 1) | bD;
      UInt   dM    = INSN(3,0) | (INSN(5,5) << 4);
      IRTemp rmode = newTemp(Ity_I32);
      assign(rmode, mkexpr(mk_get_IR_rounding_mode()));
      putFReg(fD, binop(Iop_F64toF32, mkexpr(rmode), getDReg(dM)),
                  condT);
      DIP("fcvtsd%s s%u, d%u\n", nCC(conq), fD, dM);
      goto decode_success_vfp;
   }

   /* --------------- VCVT fixed<->floating, VFP --------------- */
   /*          31   27   23   19   15 11   7    3
                 28   24   20   16 12    8    4    0 

               cond 1110 1D11 1p1U Vd 101f x1i0 imm4

      VCVT<c>.<Td>.F64 <Dd>, <Dd>, #fbits
      VCVT<c>.<Td>.F32 <Dd>, <Dd>, #fbits
      VCVT<c>.F64.<Td> <Dd>, <Dd>, #fbits
      VCVT<c>.F32.<Td> <Dd>, <Dd>, #fbits
      are of this form.  We only handle a subset of the cases though.
   */
   if (BITS8(1,1,1,0,1,0,1,1) == (INSN(27,20) & BITS8(1,1,1,1,1,0,1,1))
       && BITS4(1,0,1,0) == (INSN(19,16) & BITS4(1,0,1,0))
       && BITS3(1,0,1) == INSN(11,9)
       && BITS3(1,0,0) == (INSN(6,4) & BITS3(1,0,1))) {
      UInt bD        = INSN(22,22);
      UInt bOP       = INSN(18,18);
      UInt bU        = INSN(16,16);
      UInt Vd        = INSN(15,12);
      UInt bSF       = INSN(8,8);
      UInt bSX       = INSN(7,7);
      UInt bI        = INSN(5,5);
      UInt imm4      = INSN(3,0);
      Bool to_fixed  = bOP == 1;
      Bool dp_op     = bSF == 1;
      Bool unsyned   = bU == 1;
      UInt size      = bSX == 0 ? 16 : 32;
      Int  frac_bits = size - ((imm4 << 1) | bI);
      UInt d         = dp_op  ? ((bD << 4) | Vd)  : ((Vd << 1) | bD);

      IRExpr* rm     = mkU32(Irrm_NEAREST);
      IRTemp  scale  = newTemp(Ity_F64);
      assign(scale, unop(Iop_I32UtoF64, mkU32( ((UInt)1) << (frac_bits-1) )));

      if (frac_bits >= 1 && frac_bits <= 32 && !to_fixed && !dp_op
                                            && size == 32) {
         /* VCVT.F32.{S,U}32 S[d], S[d], #frac_bits */
         /* This generates really horrible code.  We could potentially
            do much better. */
         IRTemp rmode = newTemp(Ity_I32);
         assign(rmode, mkU32(Irrm_NEAREST)); // per the spec
         IRTemp src32 = newTemp(Ity_I32);
         assign(src32,  unop(Iop_ReinterpF32asI32, getFReg(d)));
         IRExpr* as_F64 = unop( unsyned ? Iop_I32UtoF64 : Iop_I32StoF64,
                                mkexpr(src32 ) );
         IRExpr* resF64 = triop(Iop_DivF64,
                                rm, as_F64, 
                                triop(Iop_AddF64, rm, mkexpr(scale),
                                                      mkexpr(scale)));
         IRExpr* resF32 = binop(Iop_F64toF32, mkexpr(rmode), resF64);
         putFReg(d, resF32, condT);
         DIP("vcvt.f32.%c32, s%u, s%u, #%d\n",
             unsyned ? 'u' : 's', d, d, frac_bits);
         goto decode_success_vfp;
      }
      if (frac_bits >= 1 && frac_bits <= 32 && !to_fixed && dp_op
                                            && size == 32) {
         /* VCVT.F64.{S,U}32 D[d], D[d], #frac_bits */
         /* This generates really horrible code.  We could potentially
            do much better. */
         IRTemp src32 = newTemp(Ity_I32);
         assign(src32, unop(Iop_64to32, getDRegI64(d)));
         IRExpr* as_F64 = unop( unsyned ? Iop_I32UtoF64 : Iop_I32StoF64,
                                mkexpr(src32 ) );
         IRExpr* resF64 = triop(Iop_DivF64,
                                rm, as_F64, 
                                triop(Iop_AddF64, rm, mkexpr(scale),
                                                      mkexpr(scale)));
         putDReg(d, resF64, condT);
         DIP("vcvt.f64.%c32, d%u, d%u, #%d\n",
             unsyned ? 'u' : 's', d, d, frac_bits);
         goto decode_success_vfp;
      }
      if (frac_bits >= 1 && frac_bits <= 32 && to_fixed && dp_op
                                            && size == 32) {
         /* VCVT.{S,U}32.F64 D[d], D[d], #frac_bits */
         IRTemp srcF64 = newTemp(Ity_F64);
         assign(srcF64, getDReg(d));
         IRTemp scaledF64 = newTemp(Ity_F64);
         assign(scaledF64, triop(Iop_MulF64,
                                 rm, mkexpr(srcF64),
                                 triop(Iop_AddF64, rm, mkexpr(scale),
                                                       mkexpr(scale))));
         IRTemp rmode = newTemp(Ity_I32);
         assign(rmode, mkU32(Irrm_ZERO)); // as per the spec
         IRTemp asI32 = newTemp(Ity_I32);
         assign(asI32, binop(unsyned ? Iop_F64toI32U : Iop_F64toI32S,
                             mkexpr(rmode), mkexpr(scaledF64)));
         putDRegI64(d, unop(unsyned ? Iop_32Uto64 : Iop_32Sto64,
                            mkexpr(asI32)), condT);

         DIP("vcvt.%c32.f64, d%u, d%u, #%d\n",
             unsyned ? 'u' : 's', d, d, frac_bits);
         goto decode_success_vfp;
      }
      if (frac_bits >= 1 && frac_bits <= 32 && to_fixed && !dp_op
                                            && size == 32) {
         /* VCVT.{S,U}32.F32 S[d], S[d], #frac_bits */
         IRTemp srcF32 = newTemp(Ity_F32);
         assign(srcF32, getFReg(d));
         IRTemp scaledF64 = newTemp(Ity_F64);
         assign(scaledF64, triop(Iop_MulF64,
                                 rm, unop(Iop_F32toF64, mkexpr(srcF32)),
                                 triop(Iop_AddF64, rm, mkexpr(scale),
                                                       mkexpr(scale))));
         IRTemp rmode = newTemp(Ity_I32);
         assign(rmode, mkU32(Irrm_ZERO)); // as per the spec
         IRTemp asI32 = newTemp(Ity_I32);
         assign(asI32, binop(unsyned ? Iop_F64toI32U : Iop_F64toI32S,
                             mkexpr(rmode), mkexpr(scaledF64)));
         putFReg(d, unop(Iop_ReinterpI32asF32, mkexpr(asI32)), condT);
         DIP("vcvt.%c32.f32, d%u, d%u, #%d\n",
             unsyned ? 'u' : 's', d, d, frac_bits);
         goto decode_success_vfp;
      }
      /* fall through */
   }

   /* FAILURE */
   return False;

  decode_success_vfp:
   /* Check that any accepted insn really is a CP10 or CP11 insn, iow,
      assert that we aren't accepting, in this fn, insns that actually
      should be handled somewhere else. */
   vassert(INSN(11,9) == BITS3(1,0,1)); // 11:8 = 1010 or 1011
   return True;  

#  undef INSN
}


/*------------------------------------------------------------*/
/*--- Instructions in NV (never) space                     ---*/
/*------------------------------------------------------------*/

/* ARM only */
/* Translate a NV space instruction.  If successful, returns True and
   *dres may or may not be updated.  If failure, returns False and
   doesn't change *dres nor create any IR.

   Note that all NEON instructions (in ARM mode) up to and including
   ARMv7, but not later, are handled through here, since they are all
   in NV space.
*/
static Bool decode_NV_instruction_ARMv7_and_below
                                 ( /*MOD*/DisResult* dres,
                                    const VexArchInfo* archinfo,
                                    UInt insn )
{
#  define INSN(_bMax,_bMin)  SLICE_UInt(insn, (_bMax), (_bMin))
#  define INSN_COND          SLICE_UInt(insn, 31, 28)

   HChar dis_buf[128];

   // Should only be called for NV instructions
   vassert(BITS4(1,1,1,1) == INSN_COND);

   /* ------------------------ pld{w} ------------------------ */
   if (BITS8(0,1,0,1, 0,0, 0,1) == (INSN(27,20) & BITS8(1,1,1,1, 0,0, 1,1))
       && BITS4(1,1,1,1) == INSN(15,12)) {
      UInt rN    = INSN(19,16);
      UInt imm12 = INSN(11,0);
      UInt bU    = INSN(23,23);
      UInt bR    = INSN(22,22);
      DIP("pld%c [r%u, #%c%u]\n", bR ? ' ' : 'w', rN, bU ? '+' : '-', imm12);
      return True;
   }

   if (BITS8(0,1,1,1, 0,0, 0,1) == (INSN(27,20) & BITS8(1,1,1,1, 0,0, 1,1))
       && BITS4(1,1,1,1) == INSN(15,12)
       && 0 == INSN(4,4)) {
      UInt rN   = INSN(19,16);
      UInt rM   = INSN(3,0);
      UInt imm5 = INSN(11,7);
      UInt sh2  = INSN(6,5);
      UInt bU   = INSN(23,23);
      UInt bR   = INSN(22,22);
      if (rM != 15 && (rN != 15 || bR)) {
         IRExpr* eaE = mk_EA_reg_plusminus_shifted_reg(rN, bU, rM,
                                                       sh2, imm5, dis_buf);
         IRTemp eaT = newTemp(Ity_I32);
         /* Bind eaE to a temp merely for debugging-vex purposes, so we
            can check it's a plausible decoding.  It will get removed
            by iropt a little later on. */
         vassert(eaE);
         assign(eaT, eaE);
         DIP("pld%c %s\n", bR ? ' ' : 'w', dis_buf);
         return True;
      }
      /* fall through */
   }

   /* ------------------------ pli ------------------------ */
   if (BITS8(0,1,0,0, 0, 1,0,1) == (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1))
       && BITS4(1,1,1,1) == INSN(15,12)) {
      UInt rN    = INSN(19,16);
      UInt imm12 = INSN(11,0);
      UInt bU    = INSN(23,23);
      DIP("pli [r%u, #%c%u]\n", rN, bU ? '+' : '-', imm12);
      return True;
   }

   /* --------------------- Interworking branches --------------------- */

   // BLX (1), viz, unconditional branch and link to R15+simm24
   // and set CPSR.T = 1, that is, switch to Thumb mode
   if (INSN(31,25) == BITS7(1,1,1,1,1,0,1)) {
      UInt bitH   = INSN(24,24);
      UInt uimm24 = INSN(23,0);   uimm24 <<= 8;
      Int  simm24 = (Int)uimm24;  simm24 >>= 8;
      simm24 = (((UInt)simm24) << 2) + (bitH << 1);
      /* Now this is a bit tricky.  Since we're decoding an ARM insn,
         it is implies that CPSR.T == 0.  Hence the current insn's
         address is guaranteed to be of the form X--(30)--X00.  So, no
         need to mask any bits off it.  But need to set the lowest bit
         to 1 to denote we're in Thumb mode after this, since
         guest_R15T has CPSR.T as the lowest bit.  And we can't chase
         into the call, so end the block at this point. */
      UInt dst = guest_R15_curr_instr_notENC + 8 + (simm24 | 1);
      putIRegA( 14, mkU32(guest_R15_curr_instr_notENC + 4),
                    IRTemp_INVALID/*because AL*/, Ijk_Boring );
      llPutIReg(15, mkU32(dst));
      dres->jk_StopHere = Ijk_Call;
      dres->whatNext    = Dis_StopHere;
      DIP("blx 0x%x (and switch to Thumb mode)\n", dst - 1);
      return True;
   }

   /* ------------------- v7 barrier insns ------------------- */
   switch (insn) {
      case 0xF57FF06F: /* ISB */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("ISB\n");
         return True;
      case 0xF57FF04F: /* DSB sy */
      case 0xF57FF04E: /* DSB st */
      case 0xF57FF04B: /* DSB ish */
      case 0xF57FF04A: /* DSB ishst */
      case 0xF57FF047: /* DSB nsh */
      case 0xF57FF046: /* DSB nshst */
      case 0xF57FF043: /* DSB osh */
      case 0xF57FF042: /* DSB oshst */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("DSB\n");
         return True;
      case 0xF57FF05F: /* DMB sy */
      case 0xF57FF05E: /* DMB st */
      case 0xF57FF05B: /* DMB ish */
      case 0xF57FF05A: /* DMB ishst */
      case 0xF57FF057: /* DMB nsh */
      case 0xF57FF056: /* DMB nshst */
      case 0xF57FF053: /* DMB osh */
      case 0xF57FF052: /* DMB oshst */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("DMB\n");
         return True;
      default:
         break;
   }

   /* ------------------- CLREX ------------------ */
   if (insn == 0xF57FF01F) {
      /* AFAICS, this simply cancels a (all?) reservations made by a
         (any?) preceding LDREX(es).  Arrange to hand it through to
         the back end. */
      stmt( IRStmt_MBE(Imbe_CancelReservation) );
      DIP("clrex\n");
      return True;
   }

   /* ------------------- NEON ------------------- */
   if (archinfo->hwcaps & VEX_HWCAPS_ARM_NEON) {
      Bool ok_neon = decode_NEON_instruction_ARMv7_and_below(
                        dres, insn, IRTemp_INVALID/*unconditional*/, 
                        False/*!isT*/
                     );
      if (ok_neon)
         return True;
   }

   // unrecognised
   return False;

#  undef INSN_COND
#  undef INSN
}


/*------------------------------------------------------------*/
/*--- Disassemble a single ARM instruction                 ---*/
/*------------------------------------------------------------*/

/* Disassemble a single ARM instruction into IR.  The instruction is
   located in host memory at guest_instr, and has (decoded) guest IP
   of guest_R15_curr_instr_notENC, which will have been set before the
   call here. */

static
DisResult disInstr_ARM_WRK (
             Bool         (*resteerOkFn) ( /*opaque*/void*, Addr ),
             Bool         resteerCisOk,
             void*        callback_opaque,
             const UChar* guest_instr,
             const VexArchInfo* archinfo,
             const VexAbiInfo*  abiinfo,
             Bool         sigill_diag
          )
{
   // A macro to fish bits out of 'insn'.
#  define INSN(_bMax,_bMin)  SLICE_UInt(insn, (_bMax), (_bMin))
#  define INSN_COND          SLICE_UInt(insn, 31, 28)

   DisResult dres;
   UInt      insn;
   IRTemp    condT; /* :: Ity_I32 */
   UInt      summary;
   HChar     dis_buf[128];  // big enough to hold LDMIA etc text

   /* Set result defaults. */
   dres.whatNext    = Dis_Continue;
   dres.len         = 4;
   dres.continueAt  = 0;
   dres.jk_StopHere = Ijk_INVALID;

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

   DIP("\t(arm) 0x%x:  ", (UInt)guest_R15_curr_instr_notENC);

   vassert(0 == (guest_R15_curr_instr_notENC & 3));

   /* ----------------------------------------------------------- */

   /* Spot "Special" instructions (see comment at top of file). */
   {
      const UChar* code = guest_instr;
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
            llPutIReg(15, mkU32( guest_R15_curr_instr_notENC + 20 ));
            dres.jk_StopHere = Ijk_ClientReq;
            dres.whatNext    = Dis_StopHere;
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
            llPutIReg(14, mkU32( guest_R15_curr_instr_notENC + 20) );
            llPutIReg(15, llGetIReg(4));
            dres.jk_StopHere = Ijk_NoRedir;
            dres.whatNext    = Dis_StopHere;
            goto decode_success;
         }
         else
         if (getUIntLittleEndianly(code+16) == 0xE1899009
                                               /* orr r9,r9,r9 */) {
            /* IR injection */
            DIP("IR injection\n");
            vex_inject_ir(irsb, Iend_LE);
            // Invalidate the current insn. The reason is that the IRop we're
            // injecting here can change. In which case the translation has to
            // be redone. For ease of handling, we simply invalidate all the
            // time.
            stmt(IRStmt_Put(OFFB_CMSTART, mkU32(guest_R15_curr_instr_notENC)));
            stmt(IRStmt_Put(OFFB_CMLEN,   mkU32(20)));
            llPutIReg(15, mkU32( guest_R15_curr_instr_notENC + 20 ));
            dres.whatNext    = Dis_StopHere;
            dres.jk_StopHere = Ijk_InvalICache;
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

   /* Main ARM instruction decoder starts here. */

   /* Deal with the condition.  Strategy is to merely generate a
      condition temporary at this point (or IRTemp_INVALID, meaning
      unconditional).  We leave it to lower-level instruction decoders
      to decide whether they can generate straight-line code, or
      whether they must generate a side exit before the instruction.
      condT :: Ity_I32 and is always either zero or one. */
   condT = IRTemp_INVALID;
   switch ( (ARMCondcode)INSN_COND ) {
      case ARMCondNV: {
         // Illegal instruction prior to v5 (see ARM ARM A3-5), but
         // some cases are acceptable
         Bool ok
            = decode_NV_instruction_ARMv7_and_below(&dres, archinfo, insn);
         if (ok)
            goto decode_success;
         else
            goto after_v7_decoder;
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
      const HChar*  name = NULL;
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
            assign(rNt, getIRegA(rN));
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
            // can't safely read guest state after here
            // now safe to put the main result
            putIRegA( rD, mkexpr(res), condT, Ijk_Boring );
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
            IRTemp jk = Ijk_Boring;
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
            /* According to the Cortex A8 TRM Sec. 5.2.1, MOV PC, r14 is a
                return for purposes of branch prediction. */
            if (!isMVN && INSN(11,0) == 14) {
              jk = Ijk_Ret;
            }
            // can't safely read guest state after here
            putIRegA( rD, mkexpr(res), condT, jk );
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
            assign(rNt, getIRegA(rN));
            ok = mk_shifter_operand(
                    INSN(25,25), INSN(11,0), 
                    &shop, NULL, dis_buf
                 );
            if (!ok)
               break;
            // can't safely read guest state after here
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
            assign(rNt, getIRegA(rN));
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
            // can't safely read guest state after here
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
            // FIXME: shco isn't used for anything.  Get rid of it.
            rNt = newTemp(Ity_I32);
            assign(rNt, getIRegA(rN));
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
            putIRegA( rD, mkexpr(res), condT, Ijk_Boring );
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

         default:
            vassert(0);
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
     assign(rnT, getIRegA(rN));

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
        assign(rDt, getIRegA(rD));

        /* Update Rn if necessary. */
        switch (summary & 0x0F) {
           case 2: case 3:
              putIRegA( rN, mkexpr(eaT), condT, Ijk_Boring );
              break;
        }

        /* generate the transfer */
        if (bB == 0) { // word store
           storeGuardedLE( mkexpr(taT), mkexpr(rDt), condT );
        } else { // byte store
           vassert(bB == 1);
           storeGuardedLE( mkexpr(taT), unop(Iop_32to8, mkexpr(rDt)), condT );
        }

     } else {
        /* Load */
        vassert(bL == 1);

        /* generate the transfer */
        if (bB == 0) { // word load
           IRTemp jk = Ijk_Boring;
           /* According to the Cortex A8 TRM Sec. 5.2.1, LDR(1) with r13 as the
               base register and PC as the destination register is a return for
               purposes of branch prediction.
              The ARM ARM Sec. C9.10.1 further specifies that it must use a
               post-increment by immediate addressing mode to be counted in
               event 0x0E (Procedure return).*/
           if (rN == 13 && summary == (3 | 16) && bB == 0) {
              jk = Ijk_Ret;
           }
           IRTemp tD = newTemp(Ity_I32);
           loadGuardedLE( tD, ILGop_Ident32,
                          mkexpr(taT), llGetIReg(rD), condT );
           /* "rD == 15 ? condT : IRTemp_INVALID": simply
              IRTemp_INVALID would be correct in all cases here, and
              for the non-r15 case it generates better code, by
              avoiding two tests of the cond (since it is already
              tested by loadGuardedLE).  However, the logic at the end
              of this function, that deals with writes to r15, has an
              optimisation which depends on seeing whether or not the
              write is conditional.  Hence in this particular case we
              let it "see" the guard condition. */
           putIRegA( rD, mkexpr(tD),
                     rD == 15 ? condT : IRTemp_INVALID, jk );
        } else { // byte load
           vassert(bB == 1);
           IRTemp tD = newTemp(Ity_I32);
           loadGuardedLE( tD, ILGop_8Uto32, mkexpr(taT), llGetIReg(rD), condT );
           /* No point in similar 3rd arg complexity here, since we
              can't sanely write anything to r15 like this. */
           putIRegA( rD, mkexpr(tD), IRTemp_INVALID, Ijk_Boring );
        }

        /* Update Rn if necessary. */
        switch (summary & 0x0F) {
           case 2: case 3:
              // should be assured by logic above:
              if (bL == 1)
                 vassert(rD != rN); /* since we just wrote rD */
              putIRegA( rN, mkexpr(eaT), condT, Ijk_Boring );
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

     /* If this is a branch, make it unconditional at this point.
        Doing conditional branches in-line is too complex (for now).
        Note that you'd have to be insane to use any of these loads to
        do a branch, since they only load 16 bits at most, but we
        handle it just in case. */
     if (bL == 1 && rD == 15 && condT != IRTemp_INVALID) {
        // go uncond
        mk_skip_over_A32_if_cond_is_false( condT );
        condT = IRTemp_INVALID;
        // now uncond
     }

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
     assign(rnT, getIRegA(rN));

     /* decide on the transfer address */
     IRTemp taT = IRTemp_INVALID;
     switch (summary & 0x0F) {
        case 1: case 2: taT = eaT; break;
        case 3:         taT = rnT; break;
     }
     vassert(taT != IRTemp_INVALID);

     /* ll previous value of rD, for dealing with conditional loads */
     IRTemp llOldRd = newTemp(Ity_I32);
     assign(llOldRd, llGetIReg(rD));

     /* halfword store  H 1  L 0  S 0
        uhalf load      H 1  L 1  S 0
        shalf load      H 1  L 1  S 1
        sbyte load      H 0  L 1  S 1
     */
     const HChar* name = NULL;
     /* generate the transfer */
     /**/ if (bH == 1 && bL == 0 && bS == 0) { // halfword store
        storeGuardedLE( mkexpr(taT),
                        unop(Iop_32to16, getIRegA(rD)), condT );
        name = "strh";
     }
     else if (bH == 1 && bL == 1 && bS == 0) { // uhalf load
        IRTemp newRd = newTemp(Ity_I32);
        loadGuardedLE( newRd, ILGop_16Uto32, 
                       mkexpr(taT), mkexpr(llOldRd), condT );
        putIRegA( rD, mkexpr(newRd), IRTemp_INVALID, Ijk_Boring );
        name = "ldrh";
     }
     else if (bH == 1 && bL == 1 && bS == 1) { // shalf load
        IRTemp newRd = newTemp(Ity_I32);
        loadGuardedLE( newRd, ILGop_16Sto32, 
                       mkexpr(taT), mkexpr(llOldRd), condT );
        putIRegA( rD, mkexpr(newRd), IRTemp_INVALID, Ijk_Boring );
        name = "ldrsh";
     }
     else if (bH == 0 && bL == 1 && bS == 1) { // sbyte load
        IRTemp newRd = newTemp(Ity_I32);
        loadGuardedLE( newRd, ILGop_8Sto32, 
                       mkexpr(taT), mkexpr(llOldRd), condT );
        putIRegA( rD, mkexpr(newRd), IRTemp_INVALID, Ijk_Boring );
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
           putIRegA( rN, mkexpr(eaT), condT, Ijk_Boring );
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
         mk_skip_over_A32_if_cond_is_false( condT );
         condT = IRTemp_INVALID;
      }

      /* Ok, now we're unconditional.  Generate the IR. */
      mk_ldm_stm( True/*arm*/, rN, bINC, bBEFORE, bW, bL, regList );

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
      UInt uimm24 = insn & ((1<<24)-1);  uimm24 <<= 8;
      Int  simm24 = (Int)uimm24;         simm24 >>= 8;
      UInt dst    = guest_R15_curr_instr_notENC + 8 + (((UInt)simm24) << 2);
      IRJumpKind jk = link ? Ijk_Call : Ijk_Boring;
      if (link) {
         putIRegA(14, mkU32(guest_R15_curr_instr_notENC + 4),
                      condT, Ijk_Boring);
      }
      if (condT == IRTemp_INVALID) {
         /* unconditional transfer to 'dst'.  See if we can simply
            continue tracing at the destination. */
         if (resteerOkFn( callback_opaque, dst )) {
            /* yes */
            dres.whatNext   = Dis_ResteerU;
            dres.continueAt = dst;
         } else {
            /* no; terminate the SB at this point. */
            llPutIReg(15, mkU32(dst));
            dres.jk_StopHere = jk;
            dres.whatNext    = Dis_StopHere;
         }
         DIP("b%s 0x%x\n", link ? "l" : "", dst);
      } else {
         /* conditional transfer to 'dst' */
         const HChar* comment = "";

         /* First see if we can do some speculative chasing into one
            arm or the other.  Be conservative and only chase if
            !link, that is, this is a normal conditional branch to a
            known destination. */
         if (!link
             && resteerCisOk
             && vex_control.guest_chase_cond
             && dst < guest_R15_curr_instr_notENC
             && resteerOkFn( callback_opaque, dst) ) {
            /* Speculation: assume this backward branch is taken.  So
               we need to emit a side-exit to the insn following this
               one, on the negation of the condition, and continue at
               the branch target address (dst). */
            stmt( IRStmt_Exit( unop(Iop_Not1,
                                    unop(Iop_32to1, mkexpr(condT))),
                               Ijk_Boring,
                               IRConst_U32(guest_R15_curr_instr_notENC+4),
                               OFFB_R15T ));
            dres.whatNext   = Dis_ResteerC;
            dres.continueAt = (Addr32)dst;
            comment = "(assumed taken)";
         }
         else
         if (!link
             && resteerCisOk
             && vex_control.guest_chase_cond
             && dst >= guest_R15_curr_instr_notENC
             && resteerOkFn( callback_opaque, 
                             guest_R15_curr_instr_notENC+4) ) {
            /* Speculation: assume this forward branch is not taken.
               So we need to emit a side-exit to dst (the dest) and
               continue disassembling at the insn immediately
               following this one. */
            stmt( IRStmt_Exit( unop(Iop_32to1, mkexpr(condT)),
                               Ijk_Boring,
                               IRConst_U32(dst),
                               OFFB_R15T ));
            dres.whatNext   = Dis_ResteerC;
            dres.continueAt = guest_R15_curr_instr_notENC+4;
            comment = "(assumed not taken)";
         }
         else {
            /* Conservative default translation - end the block at
               this point. */
            stmt( IRStmt_Exit( unop(Iop_32to1, mkexpr(condT)),
                               jk, IRConst_U32(dst), OFFB_R15T ));
            llPutIReg(15, mkU32(guest_R15_curr_instr_notENC + 4));
            dres.jk_StopHere = Ijk_Boring;
            dres.whatNext    = Dis_StopHere;
         }
         DIP("b%s%s 0x%x %s\n", link ? "l" : "", nCC(INSN_COND),
             dst, comment);
      }
      goto decode_success;
   }

   // B, BL (Branch, or Branch-and-Link, to a register)
   // NB: interworking branch
   if (INSN(27,20) == BITS8(0,0,0,1,0,0,1,0)
       && INSN(19,12) == BITS8(1,1,1,1,1,1,1,1)
       && (INSN(11,4) == BITS8(1,1,1,1,0,0,1,1)
           || INSN(11,4) == BITS8(1,1,1,1,0,0,0,1))) {
      IRTemp  dst = newTemp(Ity_I32);
      UInt    link = (INSN(11,4) >> 1) & 1;
      UInt    rM   = INSN(3,0);
      // we don't decode the case (link && rM == 15), as that's
      // Unpredictable.
      if (!(link && rM == 15)) {
         if (condT != IRTemp_INVALID) {
            mk_skip_over_A32_if_cond_is_false( condT );
         }
         // rM contains an interworking address exactly as we require
         // (with continuation CPSR.T in bit 0), so we can use it
         // as-is, with no masking.
         assign( dst, getIRegA(rM) );
         if (link) {
            putIRegA( 14, mkU32(guest_R15_curr_instr_notENC + 4),
                      IRTemp_INVALID/*because AL*/, Ijk_Boring );
         }
         llPutIReg(15, mkexpr(dst));
         dres.jk_StopHere = link ? Ijk_Call
                                 : (rM == 14 ? Ijk_Ret : Ijk_Boring);
         dres.whatNext    = Dis_StopHere;
         if (condT == IRTemp_INVALID) {
            DIP("b%sx r%u\n", link ? "l" : "", rM);
         } else {
            DIP("b%sx%s r%u\n", link ? "l" : "", nCC(INSN_COND), rM);
         }
         goto decode_success;
      }
      /* else: (link && rM == 15): just fall through */
   }

   /* --- NB: ARM interworking branches are in NV space, hence
      are handled elsewhere by decode_NV_instruction_ARMv7_and_below.
      ---
   */

   /* --------------------- Clz --------------------- */
   // CLZ
   if (INSN(27,20) == BITS8(0,0,0,1,0,1,1,0)
       && INSN(19,16) == BITS4(1,1,1,1)
       && INSN(11,4) == BITS8(1,1,1,1,0,0,0,1)) {
      UInt rD = INSN(15,12);
      UInt rM = INSN(3,0);
      IRTemp arg = newTemp(Ity_I32);
      IRTemp res = newTemp(Ity_I32);
      assign(arg, getIRegA(rM));
      assign(res, IRExpr_ITE(
                     binop(Iop_CmpEQ32, mkexpr(arg), mkU32(0)),
                     mkU32(32),
                     unop(Iop_Clz32, mkexpr(arg))
            ));
      putIRegA(rD, mkexpr(res), condT, Ijk_Boring);
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
         assign( argL, getIRegA(rM));
         assign( argR, getIRegA(rS));
         assign( res, binop(Iop_Mul32, mkexpr(argL), mkexpr(argR)) );
         if (bitS) {
            oldC = newTemp(Ity_I32);
            assign(oldC, mk_armg_calculate_flag_c());
            oldV = newTemp(Ity_I32);
            assign(oldV, mk_armg_calculate_flag_v());
         }
         // now update guest state
         putIRegA( rD, mkexpr(res), condT, Ijk_Boring );
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

   /* --------------------- Integer Divides --------------------- */
   // SDIV
   if (BITS8(0,1,1,1,0,0,0,1) == INSN(27,20)
       && INSN(15,12) == BITS4(1,1,1,1)
       && INSN(7,4) == BITS4(0,0,0,1)) {
      UInt rD = INSN(19,16);
      UInt rM = INSN(11,8);
      UInt rN = INSN(3,0);
      if (rD == 15 || rM == 15 || rN == 15) {
         /* Unpredictable; don't decode; fall through */
      } else {
         IRTemp res  = newTemp(Ity_I32);
         IRTemp argL = newTemp(Ity_I32);
         IRTemp argR = newTemp(Ity_I32);
         assign(argL, getIRegA(rN));
         assign(argR, getIRegA(rM));
         assign(res, binop(Iop_DivS32, mkexpr(argL), mkexpr(argR)));
         putIRegA(rD, mkexpr(res), condT, Ijk_Boring);
         DIP("sdiv r%u, r%u, r%u\n", rD, rN, rM);
         goto decode_success;
      }
    }

   // UDIV
   if (BITS8(0,1,1,1,0,0,1,1) == INSN(27,20)
       && INSN(15,12) == BITS4(1,1,1,1)
       && INSN(7,4) == BITS4(0,0,0,1)) {
      UInt rD = INSN(19,16);
      UInt rM = INSN(11,8);
      UInt rN = INSN(3,0);
      if (rD == 15 || rM == 15 || rN == 15) {
         /* Unpredictable; don't decode; fall through */
      } else {
         IRTemp res  = newTemp(Ity_I32);
         IRTemp argL = newTemp(Ity_I32);
         IRTemp argR = newTemp(Ity_I32);
         assign(argL, getIRegA(rN));
         assign(argR, getIRegA(rM));
         assign(res, binop(Iop_DivU32, mkexpr(argL), mkexpr(argR)));
         putIRegA(rD, mkexpr(res), condT, Ijk_Boring);
         DIP("udiv r%u, r%u, r%u\n", rD, rN, rM);
         goto decode_success;
      }
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
         assign( argL, getIRegA(rM));
         assign( argR, getIRegA(rS));
         assign( argP, getIRegA(rN));
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
         putIRegA( rD, mkexpr(res), condT, Ijk_Boring );
         if (bitS) {
            IRTemp pair = newTemp(Ity_I32);
            assign( pair, binop(Iop_Or32,
                                binop(Iop_Shl32, mkexpr(oldC), mkU8(1)),
                                mkexpr(oldV)) );
            setFlags_D1_ND( ARMG_CC_OP_MUL, res, pair, condT );
         }
         DIP("ml%c%c%s r%u, r%u, r%u, r%u\n",
             isMLS ? 's' : 'a', bitS ? 's' : ' ',
             nCC(INSN_COND), rD, rM, rS, rN);
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
         assign( argL, getIRegA(rM));
         assign( argR, getIRegA(rS));
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
         putIRegA( rDhi, mkexpr(resHi), condT, Ijk_Boring );
         putIRegA( rDlo, mkexpr(resLo), condT, Ijk_Boring );
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
         assign( argL, getIRegA(rM));
         assign( argR, getIRegA(rS));
         assign( old, binop(Iop_32HLto64, getIRegA(rDhi), getIRegA(rDlo)) );
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
         putIRegA( rDhi, mkexpr(resHi), condT, Ijk_Boring );
         putIRegA( rDlo, mkexpr(resLo), condT, Ijk_Boring );
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

   // UMAAL
   if (BITS8(0,0,0,0,0,1,0,0) == INSN(27,20) && INSN(7,4) == BITS4(1,0,0,1)) {
      UInt rDhi = INSN(19,16);
      UInt rDlo = INSN(15,12);
      UInt rM   = INSN(11,8);
      UInt rN   = INSN(3,0);
      if (rDlo == 15 || rDhi == 15 || rN == 15 || rM == 15 || rDhi == rDlo)  {
         /* Unpredictable; don't decode; fall through */
      } else {
         IRTemp argN   = newTemp(Ity_I32);
         IRTemp argM   = newTemp(Ity_I32);
         IRTemp argDhi = newTemp(Ity_I32);
         IRTemp argDlo = newTemp(Ity_I32);
         IRTemp res    = newTemp(Ity_I64);
         IRTemp resHi  = newTemp(Ity_I32);
         IRTemp resLo  = newTemp(Ity_I32);
         assign( argN,   getIRegA(rN) );
         assign( argM,   getIRegA(rM) );
         assign( argDhi, getIRegA(rDhi) );
         assign( argDlo, getIRegA(rDlo) );
         assign( res, 
                 binop(Iop_Add64,
                       binop(Iop_Add64,
                             binop(Iop_MullU32, mkexpr(argN), mkexpr(argM)),
                             unop(Iop_32Uto64, mkexpr(argDhi))),
                       unop(Iop_32Uto64, mkexpr(argDlo))) );
         assign( resHi, unop(Iop_64HIto32, mkexpr(res)) );
         assign( resLo, unop(Iop_64to32, mkexpr(res)) );
         // now update guest state
         putIRegA( rDhi, mkexpr(resHi), condT, Ijk_Boring );
         putIRegA( rDlo, mkexpr(resLo), condT, Ijk_Boring );
         DIP("umaal %s r%u, r%u, r%u, r%u\n",
             nCC(INSN_COND), rDlo, rDhi, rN, rM);
         goto decode_success;
      }
      /* fall through */
   }

   /* --------------------- Msr etc --------------------- */

   // MSR apsr, #imm
   if (INSN(27,20) == BITS8(0,0,1,1,0,0,1,0)
       && INSN(17,12) == BITS6(0,0,1,1,1,1)) {
      UInt write_ge    = INSN(18,18);
      UInt write_nzcvq = INSN(19,19);
      if (write_nzcvq || write_ge) {
         UInt   imm = (INSN(11,0) >> 0) & 0xFF;
         UInt   rot = 2 * ((INSN(11,0) >> 8) & 0xF);
         IRTemp immT = newTemp(Ity_I32);
         vassert(rot <= 30);
         imm = ROR32(imm, rot);
         assign(immT, mkU32(imm));
         desynthesise_APSR( write_nzcvq, write_ge, immT, condT );
         DIP("msr%s cpsr%s%sf, #0x%08x\n", nCC(INSN_COND),
             write_nzcvq ? "f" : "", write_ge ? "g" : "", imm);
         goto decode_success;
      }
      /* fall through */
   }

   // MSR apsr, reg
   if (INSN(27,20) == BITS8(0,0,0,1,0,0,1,0) 
       && INSN(17,12) == BITS6(0,0,1,1,1,1)
       && INSN(11,4) == BITS8(0,0,0,0,0,0,0,0)) {
      UInt rN          = INSN(3,0);
      UInt write_ge    = INSN(18,18);
      UInt write_nzcvq = INSN(19,19);
      if (rN != 15 && (write_nzcvq || write_ge)) {
         IRTemp rNt = newTemp(Ity_I32);
         assign(rNt, getIRegA(rN));
         desynthesise_APSR( write_nzcvq, write_ge, rNt, condT );
         DIP("msr%s cpsr_%s%s, r%u\n", nCC(INSN_COND),
             write_nzcvq ? "f" : "", write_ge ? "g" : "", rN);
         goto decode_success;
      }
      /* fall through */
   }

   // MRS rD, cpsr
   if ((insn & 0x0FFF0FFF) == 0x010F0000) {
      UInt rD   = INSN(15,12);
      if (rD != 15) {
         IRTemp apsr = synthesise_APSR();
         putIRegA( rD, mkexpr(apsr), condT, Ijk_Boring );
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
            mk_skip_over_A32_if_cond_is_false( condT );
         }
         // AL after here
         llPutIReg(15, mkU32( guest_R15_curr_instr_notENC + 4 ));
         dres.jk_StopHere = Ijk_Sys_syscall;
         dres.whatNext    = Dis_StopHere;
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
            mk_skip_over_A32_if_cond_is_false( condT );
            condT = IRTemp_INVALID;
         }
         /* Ok, now we're unconditional.  Generate a LL-SC loop. */
         assign(tRn, getIRegA(rN));
         assign(tNew, getIRegA(rM));
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
                           IRConst_U32(guest_R15_curr_instr_notENC),
                           OFFB_R15T ));
         putIRegA(rD, isB ? unop(Iop_8Uto32, mkexpr(tOld)) : mkexpr(tOld),
                      IRTemp_INVALID, Ijk_Boring);
         DIP("swp%s%s r%u, r%u, [r%u]\n",
             isB ? "b" : "", nCC(INSN_COND), rD, rM, rN);
         goto decode_success;
      }
      /* fall through */
   }

   /* ----------------------------------------------------------- */
   /* -- ARMv6 instructions                                    -- */
   /* ----------------------------------------------------------- */

   /* ------------------- {ldr,str}ex{,b,h,d} ------------------- */

   // LDREXD, LDREX, LDREXH, LDREXB
   if (0x01900F9F == (insn & 0x0F900FFF)) {
      UInt   rT    = INSN(15,12);
      UInt   rN    = INSN(19,16);
      IRType ty    = Ity_INVALID;
      IROp   widen = Iop_INVALID;
      const HChar* nm = NULL;
      Bool   valid = True;
      switch (INSN(22,21)) {
         case 0: nm = "";  ty = Ity_I32; break;
         case 1: nm = "d"; ty = Ity_I64; break;
         case 2: nm = "b"; ty = Ity_I8;  widen = Iop_8Uto32; break;
         case 3: nm = "h"; ty = Ity_I16; widen = Iop_16Uto32; break;
         default: vassert(0);
      }
      if (ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
         if (rT == 15 || rN == 15)
            valid = False;
      } else {
         vassert(ty == Ity_I64);
         if ((rT & 1) == 1 || rT == 14 || rN == 15)
            valid = False;
      }
      if (valid) {
         IRTemp res;
         /* make unconditional */
         if (condT != IRTemp_INVALID) {
           mk_skip_over_A32_if_cond_is_false( condT );
           condT = IRTemp_INVALID;
         }
         /* Ok, now we're unconditional.  Do the load. */
         res = newTemp(ty);
         // FIXME: assumes little-endian guest
         stmt( IRStmt_LLSC(Iend_LE, res, getIRegA(rN),
                           NULL/*this is a load*/) );
         if (ty == Ity_I64) {
            // FIXME: assumes little-endian guest
            putIRegA(rT+0, unop(Iop_64to32, mkexpr(res)),
                           IRTemp_INVALID, Ijk_Boring);
            putIRegA(rT+1, unop(Iop_64HIto32, mkexpr(res)),
                           IRTemp_INVALID, Ijk_Boring);
            DIP("ldrex%s%s r%u, r%u, [r%u]\n",
                nm, nCC(INSN_COND), rT+0, rT+1, rN);
         } else {
            putIRegA(rT, widen == Iop_INVALID
                            ? mkexpr(res) : unop(widen, mkexpr(res)),
                     IRTemp_INVALID, Ijk_Boring);
            DIP("ldrex%s%s r%u, [r%u]\n", nm, nCC(INSN_COND), rT, rN);
         }
         goto decode_success;
      }
      /* undecodable; fall through */
   }

   // STREXD, STREX, STREXH, STREXB
   if (0x01800F90 == (insn & 0x0F900FF0)) {
      UInt   rT     = INSN(3,0);
      UInt   rN     = INSN(19,16);
      UInt   rD     = INSN(15,12);
      IRType ty     = Ity_INVALID;
      IROp   narrow = Iop_INVALID;
      const HChar* nm = NULL;
      Bool   valid  = True;
      switch (INSN(22,21)) {
         case 0: nm = "";  ty = Ity_I32; break;
         case 1: nm = "d"; ty = Ity_I64; break;
         case 2: nm = "b"; ty = Ity_I8;  narrow = Iop_32to8; break;
         case 3: nm = "h"; ty = Ity_I16; narrow = Iop_32to16; break;
         default: vassert(0);
      }
      if (ty == Ity_I32 || ty == Ity_I16 || ty == Ity_I8) {
         if (rD == 15 || rN == 15 || rT == 15
             || rD == rN || rD == rT)
            valid = False;
      } else {
         vassert(ty == Ity_I64);
         if (rD == 15 || (rT & 1) == 1 || rT == 14 || rN == 15
             || rD == rN || rD == rT || rD == rT+1)
            valid = False;
      }
      if (valid) {
         IRTemp resSC1, resSC32, data;
         /* make unconditional */
         if (condT != IRTemp_INVALID) {
            mk_skip_over_A32_if_cond_is_false( condT );
            condT = IRTemp_INVALID;
         }
         /* Ok, now we're unconditional.  Do the store. */
         data = newTemp(ty);
         assign(data,
                ty == Ity_I64
                   // FIXME: assumes little-endian guest
                   ? binop(Iop_32HLto64, getIRegA(rT+1), getIRegA(rT+0))
                   : narrow == Iop_INVALID
                      ? getIRegA(rT)
                      : unop(narrow, getIRegA(rT)));
         resSC1 = newTemp(Ity_I1);
         // FIXME: assumes little-endian guest
         stmt( IRStmt_LLSC(Iend_LE, resSC1, getIRegA(rN), mkexpr(data)) );

         /* Set rD to 1 on failure, 0 on success.  Currently we have
            resSC1 == 0 on failure, 1 on success. */
         resSC32 = newTemp(Ity_I32);
         assign(resSC32,
                unop(Iop_1Uto32, unop(Iop_Not1, mkexpr(resSC1))));

         putIRegA(rD, mkexpr(resSC32),
                      IRTemp_INVALID, Ijk_Boring);
         if (ty == Ity_I64) {
            DIP("strex%s%s r%u, r%u, r%u, [r%u]\n",
                nm, nCC(INSN_COND), rD, rT, rT+1, rN);
         } else {
            DIP("strex%s%s r%u, r%u, [r%u]\n",
                nm, nCC(INSN_COND), rD, rT, rN);
         }
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
            putIRegA(rD,
                     binop(Iop_Or32,
                           binop(Iop_And32, getIRegA(rD), mkU32(0xFFFF)),
                           mkU32(imm16 << 16)),
                     condT, Ijk_Boring);
            DIP("movt%s r%u, #0x%04x\n", nCC(INSN_COND), rD, imm16);
            goto decode_success;
         } else {
            putIRegA(rD, mkU32(imm16), condT, Ijk_Boring);
            DIP("movw%s r%u, #0x%04x\n", nCC(INSN_COND), rD, imm16);
            goto decode_success;
         }
      }
      /* fall through */
   }

   /* ----------- uxtb, sxtb, uxth, sxth, uxtb16, sxtb16 ----------- */
   /* FIXME: this is an exact duplicate of the Thumb version.  They
      should be commoned up. */
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
         const HChar* nm = "???";
         assign(srcT, getIRegA(rM));
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
               nm = "sxtb16";
               break;
            }
            default:
               vassert(0); // guarded by "if" above
         }
         putIRegA(rD, mkexpr(dstT), condT, Ijk_Boring);
         DIP("%s%s r%u, r%u, ROR #%d\n", nm, nCC(INSN_COND), rD, rM, rot);
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
         UInt   mask   = ((UInt)1) << (msb - lsb);
         mask = (mask - 1) + mask;
         vassert(mask != 0); // guaranteed by "msb < lsb" check above
         mask <<= lsb;

         assign(src, rN == 15 ? mkU32(0) : getIRegA(rN));
         assign(olddst, getIRegA(rD));
         assign(newdst,
                binop(Iop_Or32,
                   binop(Iop_And32,
                         binop(Iop_Shl32, mkexpr(src), mkU8(lsb)), 
                         mkU32(mask)),
                   binop(Iop_And32,
                         mkexpr(olddst),
                         mkU32(~mask)))
               );

         putIRegA(rD, mkexpr(newdst), condT, Ijk_Boring);

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

         assign(src, getIRegA(rN));
         assign(tmp, binop(Iop_And32,
                           binop(Iop_Shr32, mkexpr(src), mkU8(lsb)),
                           mkU32(mask)));
         assign(res, binop(isU ? Iop_Shr32 : Iop_Sar32,
                           binop(Iop_Shl32, mkexpr(tmp), mkU8(31-wm1)),
                           mkU8(31-wm1)));

         putIRegA(rD, mkexpr(res), condT, Ijk_Boring);

         DIP("%s%s r%u, r%u, #%u, #%u\n",
             isU ? "ubfx" : "sbfx",
             nCC(INSN_COND), rD, rN, lsb, wm1 + 1);
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

     /* If this is a branch, make it unconditional at this point.
        Doing conditional branches in-line is too complex (for
        now). */
     vassert((rD & 1) == 0); /* from tests above */
     if (bS == 0 && rD+1 == 15 && condT != IRTemp_INVALID) {
        // go uncond
        mk_skip_over_A32_if_cond_is_false( condT );
        condT = IRTemp_INVALID;
        // now uncond
     }

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
     assign(rnT, getIRegA(rN));

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

     /* For almost all cases, we do the writeback after the transfers.
        However, that leaves the stack "uncovered" in this case:
           strd    rD, [sp, #-8]
        In which case, do the writeback to SP now, instead of later.
        This is bad in that it makes the insn non-restartable if the
        accesses fault, but at least keeps Memcheck happy. */
     Bool writeback_already_done = False;
     if (bS == 1 /*store*/ && summary == (2 | 16)
         && rN == 13 && rN != rD && rN != rD+1
         && bU == 0/*minus*/ && imm8 == 8) {
        putIRegA( rN, mkexpr(eaT), condT, Ijk_Boring );
        writeback_already_done = True;
     }

     /* doubleword store  S 1
        doubleword load   S 0
     */
     const HChar* name = NULL;
     /* generate the transfers */
     if (bS == 1) { // doubleword store
        storeGuardedLE( binop(Iop_Add32, mkexpr(taT), mkU32(0)),
                        getIRegA(rD+0), condT );
        storeGuardedLE( binop(Iop_Add32, mkexpr(taT), mkU32(4)),
                        getIRegA(rD+1), condT );
        name = "strd";
     } else { // doubleword load
        IRTemp oldRd0 = newTemp(Ity_I32);
        IRTemp oldRd1 = newTemp(Ity_I32);
        assign(oldRd0, llGetIReg(rD+0));
        assign(oldRd1, llGetIReg(rD+1));
        IRTemp newRd0 = newTemp(Ity_I32);
        IRTemp newRd1 = newTemp(Ity_I32);
        loadGuardedLE( newRd0, ILGop_Ident32,
                       binop(Iop_Add32, mkexpr(taT), mkU32(0)),
                       mkexpr(oldRd0), condT );
        putIRegA( rD+0, mkexpr(newRd0), IRTemp_INVALID, Ijk_Boring );
        loadGuardedLE( newRd1, ILGop_Ident32,
                       binop(Iop_Add32, mkexpr(taT), mkU32(4)),
                       mkexpr(oldRd1), condT );
        putIRegA( rD+1, mkexpr(newRd1), IRTemp_INVALID, Ijk_Boring );
        name = "ldrd";
     }

     /* Update Rn if necessary. */
     switch (summary & 0x0F) {
        case 2: case 3:
           // should be assured by logic above:
           vassert(rN != 15); /* from checks above */
           if (bS == 0) {
              vassert(rD+0 != rN); /* since we just wrote rD+0 */
              vassert(rD+1 != rN); /* since we just wrote rD+1 */
           }
           if (!writeback_already_done)
              putIRegA( rN, mkexpr(eaT), condT, Ijk_Boring );
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
         assign(srcR, getIRegA(rM));
         assign(srcL, getIRegA(rN));
         assign(res,  binop(Iop_Add32,
                            mkexpr(srcL),
                            unop(isU ? Iop_8Uto32 : Iop_8Sto32,
                                 unop(Iop_32to8, 
                                      genROR32(srcR, 8 * rot)))));
         putIRegA(rD, mkexpr(res), condT, Ijk_Boring);
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
         assign(srcR, getIRegA(rM));
         assign(srcL, getIRegA(rN));
         assign(res,  binop(Iop_Add32,
                            mkexpr(srcL),
                            unop(isU ? Iop_16Uto32 : Iop_16Sto32,
                                 unop(Iop_32to16, 
                                      genROR32(srcR, 8 * rot)))));
         putIRegA(rD, mkexpr(res), condT, Ijk_Boring);

         DIP("%cxtah%s r%u, r%u, r%u, ror #%u\n",
             isU ? 'u' : 's', nCC(INSN_COND), rD, rN, rM, rot);
         goto decode_success;
      }
      /* fall through */
   }

   /* ------------------- rev16, rev ------------------ */
   if (INSN(27,16) == 0x6BF
       && (INSN(11,4) == 0xFB/*rev16*/ || INSN(11,4) == 0xF3/*rev*/)) {
      Bool isREV = INSN(11,4) == 0xF3;
      UInt rM    = INSN(3,0);
      UInt rD    = INSN(15,12);
      if (rM != 15 && rD != 15) {
         IRTemp rMt = newTemp(Ity_I32);
         assign(rMt, getIRegA(rM));
         IRTemp res = isREV ? gen_REV(rMt) : gen_REV16(rMt);
         putIRegA(rD, mkexpr(res), condT, Ijk_Boring);
         DIP("rev%s%s r%u, r%u\n", isREV ? "" : "16",
             nCC(INSN_COND), rD, rM);
         goto decode_success;
      }
   }

   /* ------------------- revsh ----------------------- */
   if (INSN(27,16) == 0x6FF && INSN(11,4) == 0xFB) {
      UInt rM = INSN(3,0);
      UInt rD = INSN(15,12);
      if (rM != 15 && rD != 15) {
         IRTemp irt_rM  = newTemp(Ity_I32);
         IRTemp irt_hi  = newTemp(Ity_I32);
         IRTemp irt_low = newTemp(Ity_I32);
         IRTemp irt_res = newTemp(Ity_I32);
         assign(irt_rM, getIRegA(rM));
         assign(irt_hi,
                binop(Iop_Sar32,
                      binop(Iop_Shl32, mkexpr(irt_rM), mkU8(24)),
                      mkU8(16)
                )
         );
         assign(irt_low,
                binop(Iop_And32,
                      binop(Iop_Shr32, mkexpr(irt_rM), mkU8(8)),
                      mkU32(0xFF)
                )
         );
         assign(irt_res,
                binop(Iop_Or32, mkexpr(irt_hi), mkexpr(irt_low))
         );
         putIRegA(rD, mkexpr(irt_res), condT, Ijk_Boring);
         DIP("revsh%s r%u, r%u\n", nCC(INSN_COND), rD, rM);
         goto decode_success;
      }
   }

   /* ------------------- rbit ------------------ */
   if (INSN(27,16) == 0x6FF && INSN(11,4) == 0xF3) {
      UInt rD = INSN(15,12);
      UInt rM = INSN(3,0);
      if (rD != 15 && rM != 15) {
         IRTemp arg = newTemp(Ity_I32);
         assign(arg, getIRegA(rM));
         IRTemp res = gen_BITREV(arg);
         putIRegA(rD, mkexpr(res), condT, Ijk_Boring);
         DIP("rbit r%u, r%u\n", rD, rM);
         goto decode_success;
      }
   }

   /* ------------------- smmul ------------------ */
   if (INSN(27,20) == BITS8(0,1,1,1,0,1,0,1)
       && INSN(15,12) == BITS4(1,1,1,1)
       && (INSN(7,4) & BITS4(1,1,0,1)) == BITS4(0,0,0,1)) {
      UInt bitR = INSN(5,5);
      UInt rD = INSN(19,16);
      UInt rM = INSN(11,8);
      UInt rN = INSN(3,0);
      if (rD != 15 && rM != 15 && rN != 15) {
         IRExpr* res
         = unop(Iop_64HIto32,
                binop(Iop_Add64,
                      binop(Iop_MullS32, getIRegA(rN), getIRegA(rM)),
                      mkU64(bitR ? 0x80000000ULL : 0ULL)));
         putIRegA(rD, res, condT, Ijk_Boring);
         DIP("smmul%s%s r%u, r%u, r%u\n",
             nCC(INSN_COND), bitR ? "r" : "", rD, rN, rM);
         goto decode_success;
      }
   }

   /* ------------------- smmla ------------------ */
   if (INSN(27,20) == BITS8(0,1,1,1,0,1,0,1)
       && INSN(15,12) != BITS4(1,1,1,1)
       && (INSN(7,4) & BITS4(1,1,0,1)) == BITS4(0,0,0,1)) {
      UInt bitR = INSN(5,5);
      UInt rD = INSN(19,16);
      UInt rA = INSN(15,12);
      UInt rM = INSN(11,8);
      UInt rN = INSN(3,0);
      if (rD != 15 && rM != 15 && rN != 15) {
         IRExpr* res
         = unop(Iop_64HIto32,
                binop(Iop_Add64,
                      binop(Iop_Add64,
                            binop(Iop_32HLto64, getIRegA(rA), mkU32(0)),
                            binop(Iop_MullS32, getIRegA(rN), getIRegA(rM))),
                      mkU64(bitR ? 0x80000000ULL : 0ULL)));
         putIRegA(rD, res, condT, Ijk_Boring);
         DIP("smmla%s%s r%u, r%u, r%u, r%u\n",
             nCC(INSN_COND), bitR ? "r" : "", rD, rN, rM, rA);
         goto decode_success;
      }
   }

   /* -------------- (A1) LDRT reg+/-#imm12 -------------- */
   /* Load Register Unprivileged:
      ldrt<c> Rt, [Rn] {, #+/-imm12}
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,1,0,0,0,0,1,1) ) {
      UInt rT     = INSN(15,12);
      UInt rN     = INSN(19,16);
      UInt imm12  = INSN(11,0);
      UInt bU     = INSN(23,23);
      Bool valid  = True;
      if (rT == 15 || rN == 15 || rN == rT) valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_Ident32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         IRExpr* erN = binop(bU ? Iop_Add32 : Iop_Sub32,
                             getIRegA(rN), mkU32(imm12));
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrt%s r%u, [r%u], #%c%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', imm12);
         goto decode_success;
      }
   }

   /* -------------- (A2) LDRT reg+/-reg with shift -------------- */
   /* Load Register Unprivileged:
      ldrt<c> Rt, [Rn], +/-Rm{, shift}
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,1,1,0,0,0,1,1)
        && INSN(4,4) == 0 ) {
      UInt rT     = INSN(15,12);
      UInt rN     = INSN(19,16);
      UInt rM     = INSN(3,0);
      UInt imm5   = INSN(11,7);
      UInt bU     = INSN(23,23);
      UInt type   = INSN(6,5);
      Bool valid  = True;
      if (rT == 15 || rN == 15 || rN == rT || rM == 15
          /* || (ArchVersion() < 6 && rM == rN) */)
         valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_Ident32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         // dis_buf generated is slightly bogus, in fact.
         IRExpr* erN = mk_EA_reg_plusminus_shifted_reg(rN, bU, rM,
                                                       type, imm5, dis_buf);
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrt%s r%u, %s\n", nCC(INSN_COND), rT, dis_buf);
         goto decode_success;
      }
   }

   /* -------------- (A1) LDRBT reg+/-#imm12 -------------- */
   /* Load Register Byte Unprivileged:
      ldrbt<c> Rt, [Rn], #+/-imm12
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,1,0,0,0,1,1,1) ) {
      UInt rT     = INSN(15,12);
      UInt rN     = INSN(19,16);
      UInt imm12  = INSN(11,0);
      UInt bU     = INSN(23,23);
      Bool valid  = True;
      if (rT == 15 || rN == 15 || rN == rT) valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_8Uto32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         IRExpr* erN = binop(bU ? Iop_Add32 : Iop_Sub32,
                             getIRegA(rN), mkU32(imm12));
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrbt%s r%u, [r%u], #%c%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', imm12);
         goto decode_success;
      }
   }

   /* -------------- (A2) LDRBT reg+/-reg with shift -------------- */
   /* Load Register Byte Unprivileged:
      ldrbt<c> Rt, [Rn], +/-Rm{, shift}
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,1,1,0,0,1,1,1)
        && INSN(4,4) == 0 ) {
      UInt rT     = INSN(15,12);
      UInt rN     = INSN(19,16);
      UInt rM     = INSN(3,0);
      UInt imm5   = INSN(11,7);
      UInt bU     = INSN(23,23);
      UInt type   = INSN(6,5);
      Bool valid  = True;
      if (rT == 15 || rN == 15 || rN == rT || rM == 15
          /* || (ArchVersion() < 6 && rM == rN) */)
         valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_8Uto32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         // dis_buf generated is slightly bogus, in fact.
         IRExpr* erN = mk_EA_reg_plusminus_shifted_reg(rN, bU, rM,
                                                       type, imm5, dis_buf);
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrbt%s r%u, %s\n", nCC(INSN_COND), rT, dis_buf);
         goto decode_success;
      }
   }

   /* -------------- (A1) LDRHT reg+#imm8 -------------- */
   /* Load Register Halfword Unprivileged:
      ldrht<c> Rt, [Rn] {, #+/-imm8}
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,0,0,0,0,1,1,1)
       && INSN(7,4) == BITS4(1,0,1,1) ) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt bU    = INSN(23,23);
      UInt imm4H = INSN(11,8);
      UInt imm4L = INSN(3,0);
      UInt imm8  = (imm4H << 4) | imm4L;
      Bool valid = True;
      if (rT == 15 || rN == 15 || rN == rT)
         valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_16Uto32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         IRExpr* erN = binop(bU ? Iop_Add32 : Iop_Sub32,
                             getIRegA(rN), mkU32(imm8));
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrht%s r%u, [r%u], #%c%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', imm8);
         goto decode_success;
      }
   }

   /* -------------- (A2) LDRHT reg+/-reg -------------- */
   /* Load Register Halfword Unprivileged:
      ldrht<c> Rt, [Rn], +/-Rm
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,0,0,0,0,0,1,1)
       && INSN(11,4) == BITS8(0,0,0,0,1,0,1,1) ) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt rM    = INSN(3,0);
      UInt bU    = INSN(23,23);
      Bool valid = True;
      if (rT == 15 || rN == 15 || rN == rT || rM == 15)
         valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_16Uto32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         IRExpr* erN = binop(bU ? Iop_Add32 : Iop_Sub32,
                             getIRegA(rN), getIRegA(rM));
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrht%s r%u, [r%u], %cr%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', rM);
         goto decode_success;
      }
   }

   /* -------------- (A1) LDRSHT reg+#imm8 -------------- */
   /* Load Register Signed Halfword Unprivileged:
      ldrsht<c> Rt, [Rn] {, #+/-imm8}
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,0,0,0,0,1,1,1)
       && INSN(7,4) == BITS4(1,1,1,1)) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt bU    = INSN(23,23);
      UInt imm4H = INSN(11,8);
      UInt imm4L = INSN(3,0);
      UInt imm8  = (imm4H << 4) | imm4L;
      Bool valid = True;
      if (rN == 15 || rT == 15 || rN == rT)
         valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_16Sto32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         IRExpr* erN = binop(bU ? Iop_Add32 : Iop_Sub32,
                             getIRegA(rN), mkU32(imm8));
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrsht%s r%u, [r%u], #%c%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', imm8);
         goto decode_success;
      }
   }

   /* -------------- (A2) LDRSHT reg+/-reg -------------- */
   /* Load Register Signed Halfword Unprivileged:
      ldrsht<c> Rt, [Rn], +/-Rm
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,0,0,0,0,0,1,1)
       && INSN(11,4) == BITS8(0,0,0,0,1,1,1,1)) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt rM    = INSN(3,0);
      UInt bU    = INSN(23,23);
      Bool valid = True;
      if (rN == 15 || rT == 15 || rN == rT || rM == 15)
         valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_16Sto32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         IRExpr* erN = binop(bU ? Iop_Add32 : Iop_Sub32,
                             getIRegA(rN), getIRegA(rM));
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrsht%s r%u, [r%u], %cr%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', rM);
         goto decode_success;
      }
   }

   /* -------------- (A1) LDRSBT reg+#imm8 -------------- */
   /* Load Register Signed Byte Unprivileged:
      ldrsbt<c> Rt, [Rn] {, #+/-imm8}
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,0,0,0,0,1,1,1)
       && INSN(7,4) == BITS4(1,1,0,1)) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt bU    = INSN(23,23);
      UInt imm4H = INSN(11,8);
      UInt imm4L = INSN(3,0);
      UInt imm8  = (imm4H << 4) | imm4L;
      Bool valid = True;
      if (rT == 15 || rN == 15 || rN == rT)
         valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_8Sto32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         IRExpr* erN = binop(bU ? Iop_Add32 : Iop_Sub32,
                             getIRegA(rN), mkU32(imm8));
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrsbt%s r%u, [r%u], #%c%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', imm8);
         goto decode_success;
      }
   }

   /* -------------- (A2) LDRSBT reg+/-reg -------------- */
   /* Load Register Signed Byte Unprivileged:
      ldrsbt<c> Rt, [Rn], +/-Rm
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,0,0,0,0,0,1,1)
       && INSN(11,4) == BITS8(0,0,0,0,1,1,0,1)) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt bU    = INSN(23,23);
      UInt rM    = INSN(3,0);
      Bool valid = True;
      if (rT == 15 || rN == 15 || rN == rT || rM == 15)
         valid = False;
      if (valid) {
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt,
                        ILGop_8Sto32, getIRegA(rN), getIRegA(rT), condT );
         putIRegA(rT, mkexpr(newRt), IRTemp_INVALID, Ijk_Boring);
         IRExpr* erN = binop(bU ? Iop_Add32 : Iop_Sub32,
                             getIRegA(rN), getIRegA(rM));
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("ldrsbt%s r%u, [r%u], %cr%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', rM);
         goto decode_success;
      }
   }

   /* -------------- (A1) STRBT reg+#imm12 -------------- */
   /* Store Register Byte Unprivileged:
      strbt<c> Rt, [Rn], #+/-imm12
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,1,0,0,0,1,1,0) ) {
      UInt rT     = INSN(15,12);
      UInt rN     = INSN(19,16);
      UInt imm12  = INSN(11,0);
      UInt bU     = INSN(23,23);
      Bool valid = True;
      if (rT == 15 || rN == 15 || rN == rT) valid = False;
      if (valid) {
         IRExpr* address = getIRegA(rN);
         IRExpr* data = unop(Iop_32to8, getIRegA(rT));
         storeGuardedLE( address, data, condT);
         IRExpr* newRn = binop(bU ? Iop_Add32 : Iop_Sub32,
                               getIRegA(rN), mkU32(imm12));
         putIRegA(rN, newRn, condT, Ijk_Boring);
         DIP("strbt%s r%u, [r%u], #%c%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', imm12);
         goto decode_success;
      }
   }

   /* -------------- (A2) STRBT reg+/-reg -------------- */
   /* Store Register Byte Unprivileged:
      strbt<c> Rt, [Rn], +/-Rm{, shift}
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,1,1,0,0,1,1,0)
       && INSN(4,4) == 0) {
      UInt rT     = INSN(15,12);
      UInt rN     = INSN(19,16);
      UInt imm5   = INSN(11,7);
      UInt type   = INSN(6,5);
      UInt rM     = INSN(3,0);
      UInt bU     = INSN(23,23);
      Bool valid  = True;
      if (rT == 15 || rN == 15 || rN == rT || rM == 15) valid = False;
      if (valid) {
         IRExpr* address = getIRegA(rN);
         IRExpr* data = unop(Iop_32to8, getIRegA(rT));
         storeGuardedLE( address, data, condT);
         // dis_buf generated is slightly bogus, in fact.
         IRExpr* erN = mk_EA_reg_plusminus_shifted_reg(rN, bU, rM,
                                                       type, imm5, dis_buf);
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("strbt%s r%u, %s\n", nCC(INSN_COND), rT, dis_buf);
         goto decode_success;
      }
   }

   /* -------------- (A1) STRHT reg+#imm8 -------------- */
   /* Store Register Halfword Unprivileged:
      strht<c> Rt, [Rn], #+/-imm8
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,0,0,0,0,1,1,0)
       && INSN(7,4) == BITS4(1,0,1,1) ) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt imm4H = INSN(11,8);
      UInt imm4L = INSN(3,0);
      UInt imm8  = (imm4H << 4) | imm4L;
      UInt bU    = INSN(23,23);
      Bool valid = True;
      if (rT == 15 || rN == 15 || rN == rT) valid = False;
      if (valid) {
         IRExpr* address = getIRegA(rN);
         IRExpr* data = unop(Iop_32to16, getIRegA(rT));
         storeGuardedLE( address, data, condT);
         IRExpr* newRn = binop(bU ? Iop_Add32 : Iop_Sub32,
                               getIRegA(rN), mkU32(imm8));
         putIRegA(rN, newRn, condT, Ijk_Boring);
         DIP("strht%s r%u, [r%u], #%c%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', imm8);
         goto decode_success;
      }
   }

   /* -------------- (A2) STRHT reg+reg -------------- */
   /* Store Register Halfword Unprivileged:
      strht<c> Rt, [Rn], +/-Rm
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,0,0,0,0,0,1,0)
       && INSN(11,4) == BITS8(0,0,0,0,1,0,1,1) ) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt rM    = INSN(3,0);
      UInt bU    = INSN(23,23);
      Bool valid = True;
      if (rT == 15 || rN == 15 || rN == rT || rM == 15) valid = False;
      if (valid) {
         IRExpr* address = getIRegA(rN);
         IRExpr* data = unop(Iop_32to16, getIRegA(rT));
         storeGuardedLE( address, data, condT);
         IRExpr* newRn = binop(bU ? Iop_Add32 : Iop_Sub32,
                               getIRegA(rN), getIRegA(rM));
         putIRegA(rN, newRn, condT, Ijk_Boring);
         DIP("strht%s r%u, [r%u], %cr%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', rM);
         goto decode_success;
      }
   }

   /* -------------- (A1) STRT reg+imm12 -------------- */
   /* Store Register Unprivileged:
      strt<c> Rt, [Rn], #+/-imm12
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,1,0,0,0,0,1,0) ) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt imm12 = INSN(11,0);
      UInt bU    = INSN(23,23);
      Bool valid = True;
      if (rN == 15 || rN == rT) valid = False;
      if (valid) {
         IRExpr* address = getIRegA(rN);
         storeGuardedLE( address, getIRegA(rT), condT);
         IRExpr* newRn = binop(bU ? Iop_Add32 : Iop_Sub32,
                               getIRegA(rN), mkU32(imm12));
         putIRegA(rN, newRn, condT, Ijk_Boring);
         DIP("strt%s r%u, [r%u], %c%u\n",
             nCC(INSN_COND), rT, rN, bU ? '+' : '-', imm12);
         goto decode_success;
      }
   }

   /* -------------- (A2) STRT reg+reg -------------- */
   /* Store Register Unprivileged:
      strt<c> Rt, [Rn], +/-Rm{, shift}
   */
   if ( (INSN(27,20) & BITS8(1,1,1,1,0,1,1,1)) == BITS8(0,1,1,0,0,0,1,0)
       && INSN(4,4) == 0 ) {
      UInt rT    = INSN(15,12);
      UInt rN    = INSN(19,16);
      UInt rM    = INSN(3,0);
      UInt type  = INSN(6,5);
      UInt imm5  = INSN(11,7);
      UInt bU    = INSN(23,23);
      Bool valid = True;
      if (rN == 15 || rN == rT || rM == 15) valid = False;
      /* FIXME We didn't do:
         if ArchVersion() < 6 && rM == rN then UNPREDICTABLE */
      if (valid) {
         storeGuardedLE( getIRegA(rN), getIRegA(rT), condT);
         // dis_buf generated is slightly bogus, in fact.
         IRExpr* erN = mk_EA_reg_plusminus_shifted_reg(rN, bU, rM,
                                                       type, imm5, dis_buf);
         putIRegA(rN, erN, condT, Ijk_Boring);
         DIP("strt%s r%u, %s\n", nCC(INSN_COND), rT, dis_buf);
         goto decode_success;
      }
   }

   /* ----------------------------------------------------------- */
   /* -- ARMv7 instructions                                    -- */
   /* ----------------------------------------------------------- */

   /* -------------- read CP15 TPIDRURO register ------------- */
   /* mrc     p15, 0, r0,  c13, c0, 3  up to
      mrc     p15, 0, r14, c13, c0, 3
   */
   /* I don't know whether this is really v7-only.  But anyway, we
      have to support it since arm-linux uses TPIDRURO as a thread
      state register. */
   if (0x0E1D0F70 == (insn & 0x0FFF0FFF)) {
      UInt rD = INSN(15,12);
      if (rD <= 14) {
         /* skip r15, that's too stupid to handle */
         putIRegA(rD, IRExpr_Get(OFFB_TPIDRURO, Ity_I32),
                      condT, Ijk_Boring);
         DIP("mrc%s p15,0, r%u, c13, c0, 3\n", nCC(INSN_COND), rD);
         goto decode_success;
      }
      /* fall through */
   }

   /* -------------- read CP15 PMUSRENR register ------------- */
   /* mrc     p15, 0, r0,  c9, c14, 0  up to
      mrc     p15, 0, r14, c9, c14, 0
   */
   /* A program reading this register is really asking "which
      performance monitoring registes are available in user space?
      The simple answer here is to return zero, meaning "none".  See
      #345984. */
   if (0x0E190F1E == (insn & 0x0FFF0FFF)) {
      UInt rD = INSN(15,12);
      if (rD <= 14) {
         /* skip r15, that's too stupid to handle */
         putIRegA(rD, mkU32(0), condT, Ijk_Boring);
         DIP("mrc%s p15,0, r%u, c9, c14, 0\n", nCC(INSN_COND), rD);
         goto decode_success;
      }
      /* fall through */
   }

   /* Handle various kinds of barriers.  This is rather indiscriminate
      in the sense that they are all turned into an IR Fence, which
      means we don't know which they are, so the back end has to
      re-emit them all when it comes acrosss an IR Fence.
   */
   /* v6 */ /* mcr 15, 0, rT, c7, c10, 5 */
   if (0xEE070FBA == (insn & 0xFFFF0FFF)) {
      UInt rT = INSN(15,12);
      if (rT <= 14) {
         /* mcr 15, 0, rT, c7, c10, 5 (v6) equiv to DMB (v7).  Data
            Memory Barrier -- ensures ordering of memory accesses. */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("mcr 15, 0, r%u, c7, c10, 5 (data memory barrier)\n", rT);
         goto decode_success;
      }
      /* fall through */
   }
   /* other flavours of barrier */
   switch (insn) {
      case 0xEE070F9A: /* v6 */
         /* mcr 15, 0, r0, c7, c10, 4 (v6) equiv to DSB (v7).  Data
            Synch Barrier -- ensures completion of memory accesses. */
         stmt( IRStmt_MBE(Imbe_Fence) );
         DIP("mcr 15, 0, r0, c7, c10, 4 (data synch barrier)\n");
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
   /* -- Hints                                                 -- */
   /* ----------------------------------------------------------- */

   switch (insn & 0x0FFFFFFF) {
      /* ------------------- NOP ------------------ */
      case 0x0320F000:
         DIP("nop%s\n", nCC(INSN_COND));
         goto decode_success;
      /* ------------------- YIELD ------------------ */
      case 0x0320F001:
         /* Continue after conditionally yielding. */
         DIP("yield%s\n", nCC(INSN_COND));
         stmt( IRStmt_Exit( unop(Iop_32to1, 
                                 condT == IRTemp_INVALID 
                                    ? mkU32(1) : mkexpr(condT)),
                            Ijk_Yield,
                            IRConst_U32(guest_R15_curr_instr_notENC + 4),
                            OFFB_R15T ));
         goto decode_success;
      default:
         break;
   }

   /* ----------------------------------------------------------- */
   /* -- VFP (CP 10, CP 11) instructions (in ARM mode)         -- */
   /* ----------------------------------------------------------- */

   if (INSN_COND != ARMCondNV) {
      Bool ok_vfp = decode_CP10_CP11_instruction (
                       &dres, INSN(27,0), condT, INSN_COND,
                       False/*!isT*/
                    );
      if (ok_vfp)
         goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- NEON instructions (in ARM mode)                       -- */
   /* ----------------------------------------------------------- */

   /* These are all in NV space, and so are taken care of (far) above,
      by a call from this function to
      decode_NV_instruction_ARMv7_and_below(). */

   /* ----------------------------------------------------------- */
   /* -- v6 media instructions (in ARM mode)                   -- */
   /* ----------------------------------------------------------- */

   { Bool ok_v6m = decode_V6MEDIA_instruction(
                       &dres, INSN(27,0), condT, INSN_COND,
                       False/*!isT*/
                   );
     if (ok_v6m)
        goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- v8 instructions (in ARM mode)                         -- */
   /* ----------------------------------------------------------- */

  after_v7_decoder:

   /* If we get here, it means that all attempts to decode the
      instruction as ARMv7 or earlier have failed.  So, if we're doing
      ARMv8 or later, here is the point to try for it. */

   if (VEX_ARM_ARCHLEVEL(archinfo->hwcaps) >= 8) {
      Bool ok_v8
         = decode_V8_instruction( &dres, insn, condT, False/*!isT*/,
                                  IRTemp_INVALID, IRTemp_INVALID );
      if (ok_v8)
         goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- Undecodable                                           -- */
   /* ----------------------------------------------------------- */

   goto decode_failure;
   /*NOTREACHED*/

  decode_failure:
   /* All decode failures end up here. */
   if (sigill_diag) {
      vex_printf("disInstr(arm): unhandled instruction: "
                 "0x%x\n", insn);
      vex_printf("                 cond=%d(0x%x) 27:20=%d(0x%02x) "
                                   "4:4=%d "
                                   "3:0=%d(0x%x)\n",
                 (Int)INSN_COND, (UInt)INSN_COND,
                 (Int)INSN(27,20), (UInt)INSN(27,20),
                 (Int)INSN(4,4),
                 (Int)INSN(3,0), (UInt)INSN(3,0) );
   }

   /* Tell the dispatcher that this insn cannot be decoded, and so has
      not been executed, and (is currently) the next to be executed.
      R15 should be up-to-date since it made so at the start of each
      insn, but nevertheless be paranoid and update it again right
      now. */
   vassert(0 == (guest_R15_curr_instr_notENC & 3));
   llPutIReg( 15, mkU32(guest_R15_curr_instr_notENC) );
   dres.len         = 0;
   dres.whatNext    = Dis_StopHere;
   dres.jk_StopHere = Ijk_NoDecode;
   dres.continueAt  = 0;
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
      vassert(irsb->jumpkind == Ijk_Boring);
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
         stmt( IRStmt_Exit(
                  unop(Iop_32to1,
                       binop(Iop_Xor32,
                             mkexpr(r15guard), mkU32(1))),
                  r15kind,
                  IRConst_U32(guest_R15_curr_instr_notENC + 4),
                  OFFB_R15T
         ));
      }
      /* This seems crazy, but we're required to finish the insn with
         a write to the guest PC.  As usual we rely on ir_opt to tidy
         up later. */
      llPutIReg(15, llGetIReg(15));
      dres.whatNext    = Dis_StopHere;
      dres.jk_StopHere = r15kind;
   } else {
      /* Set up the end-state in the normal way. */
      switch (dres.whatNext) {
         case Dis_Continue:
            llPutIReg(15, mkU32(dres.len + guest_R15_curr_instr_notENC));
            break;
         case Dis_ResteerU:
         case Dis_ResteerC:
            llPutIReg(15, mkU32(dres.continueAt));
            break;
         case Dis_StopHere:
            break;
         default:
            vassert(0);
      }
   }

   return dres;

#  undef INSN_COND
#  undef INSN
}


/*------------------------------------------------------------*/
/*--- Disassemble a single Thumb2 instruction              ---*/
/*------------------------------------------------------------*/

static const UChar it_length_table[256]; /* fwds */

/* NB: in Thumb mode we do fetches of regs with getIRegT, which
   automagically adds 4 to fetches of r15.  However, writes to regs
   are done with putIRegT, which disallows writes to r15.  Hence any
   r15 writes and associated jumps have to be done "by hand". */

/* Disassemble a single Thumb instruction into IR.  The instruction is
   located in host memory at guest_instr, and has (decoded) guest IP
   of guest_R15_curr_instr_notENC, which will have been set before the
   call here. */

static   
DisResult disInstr_THUMB_WRK (
             Bool         (*resteerOkFn) ( /*opaque*/void*, Addr ),
             Bool         resteerCisOk,
             void*        callback_opaque,
             const UChar* guest_instr,
             const VexArchInfo* archinfo,
             const VexAbiInfo*  abiinfo,
             Bool         sigill_diag
          )
{
   /* A macro to fish bits out of insn0.  There's also INSN1, to fish
      bits out of insn1, but that's defined only after the end of the
      16-bit insn decoder, so as to stop it mistakenly being used
      therein. */
#  define INSN0(_bMax,_bMin)  SLICE_UInt(((UInt)insn0), (_bMax), (_bMin))

   DisResult dres;
   UShort    insn0; /*  first 16 bits of the insn */
   UShort    insn1; /* second 16 bits of the insn */
   HChar     dis_buf[128];  // big enough to hold LDMIA etc text

   /* Summary result of the ITxxx backwards analysis: False == safe
      but suboptimal. */
   Bool guaranteedUnconditional = False;

   /* Set result defaults. */
   dres.whatNext    = Dis_Continue;
   dres.len         = 2;
   dres.continueAt  = 0;
   dres.jk_StopHere = Ijk_INVALID;

   /* Set default actions for post-insn handling of writes to r15, if
      required. */
   r15written = False;
   r15guard   = IRTemp_INVALID; /* unconditional */
   r15kind    = Ijk_Boring;

   /* Insns could be 2 or 4 bytes long.  Just get the first 16 bits at
      this point.  If we need the second 16, get them later.  We can't
      get them both out immediately because it risks a fault (very
      unlikely, but ..) if the second 16 bits aren't actually
      necessary. */
   insn0 = getUShortLittleEndianly( guest_instr );
   insn1 = 0; /* We'll get it later, once we know we need it. */

   /* Similarly, will set this later. */
   IRTemp old_itstate = IRTemp_INVALID;

   if (0) vex_printf("insn: 0x%x\n", insn0);

   DIP("\t(thumb) 0x%x:  ", (UInt)guest_R15_curr_instr_notENC);

   vassert(0 == (guest_R15_curr_instr_notENC & 1));

   /* ----------------------------------------------------------- */
   /* Spot "Special" instructions (see comment at top of file). */
   {
      const UChar* code = guest_instr;
      /* Spot the 16-byte preamble: 

         ea4f 0cfc  mov.w   ip, ip, ror #3
         ea4f 3c7c  mov.w   ip, ip, ror #13
         ea4f 7c7c  mov.w   ip, ip, ror #29
         ea4f 4cfc  mov.w   ip, ip, ror #19
      */
      UInt word1 = 0x0CFCEA4F;
      UInt word2 = 0x3C7CEA4F;
      UInt word3 = 0x7C7CEA4F;
      UInt word4 = 0x4CFCEA4F;
      if (getUIntLittleEndianly(code+ 0) == word1 &&
          getUIntLittleEndianly(code+ 4) == word2 &&
          getUIntLittleEndianly(code+ 8) == word3 &&
          getUIntLittleEndianly(code+12) == word4) {
         /* Got a "Special" instruction preamble.  Which one is it? */
         // 0x 0A 0A EA 4A
         if (getUIntLittleEndianly(code+16) == 0x0A0AEA4A
                                               /* orr.w r10,r10,r10 */) {
            /* R3 = client_request ( R4 ) */
            DIP("r3 = client_request ( %%r4 )\n");
            llPutIReg(15, mkU32( (guest_R15_curr_instr_notENC + 20) | 1 ));
            dres.jk_StopHere = Ijk_ClientReq;
            dres.whatNext    = Dis_StopHere;
            goto decode_success;
         }
         else
         // 0x 0B 0B EA 4B
         if (getUIntLittleEndianly(code+16) == 0x0B0BEA4B
                                               /* orr r11,r11,r11 */) {
            /* R3 = guest_NRADDR */
            DIP("r3 = guest_NRADDR\n");
            dres.len = 20;
            llPutIReg(3, IRExpr_Get( OFFB_NRADDR, Ity_I32 ));
            goto decode_success;
         }
         else
         // 0x 0C 0C EA 4C
         if (getUIntLittleEndianly(code+16) == 0x0C0CEA4C
                                               /* orr r12,r12,r12 */) {
            /*  branch-and-link-to-noredir R4 */
            DIP("branch-and-link-to-noredir r4\n");
            llPutIReg(14, mkU32( (guest_R15_curr_instr_notENC + 20) | 1 ));
            llPutIReg(15, getIRegT(4));
            dres.jk_StopHere = Ijk_NoRedir;
            dres.whatNext    = Dis_StopHere;
            goto decode_success;
         }
         else
         // 0x 09 09 EA 49
         if (getUIntLittleEndianly(code+16) == 0x0909EA49
                                               /* orr r9,r9,r9 */) {
            /* IR injection */
            DIP("IR injection\n");
            vex_inject_ir(irsb, Iend_LE);
            // Invalidate the current insn. The reason is that the IRop we're
            // injecting here can change. In which case the translation has to
            // be redone. For ease of handling, we simply invalidate all the
            // time.
            stmt(IRStmt_Put(OFFB_CMSTART, mkU32(guest_R15_curr_instr_notENC)));
            stmt(IRStmt_Put(OFFB_CMLEN,   mkU32(20)));
            llPutIReg(15, mkU32( (guest_R15_curr_instr_notENC + 20) | 1 ));
            dres.whatNext    = Dis_StopHere;
            dres.jk_StopHere = Ijk_InvalICache;
            goto decode_success;
         }
         /* We don't know what it is.  Set insn0 so decode_failure
            can print the insn following the Special-insn preamble. */
         insn0 = getUShortLittleEndianly(code+16);
         goto decode_failure;
         /*NOTREACHED*/
      }

   }

   /* ----------------------------------------------------------- */

   /* Main Thumb instruction decoder starts here.  It's a series of
      switches which examine ever longer bit sequences at the MSB of
      the instruction word, first for 16-bit insns, then for 32-bit
      insns. */

   /* --- BEGIN ITxxx optimisation analysis --- */
   /* This is a crucial optimisation for the ITState boilerplate that
      follows.  Examine the 9 halfwords preceding this instruction,
      and if we are absolutely sure that none of them constitute an
      'it' instruction, then we can be sure that this instruction is
      not under the control of any 'it' instruction, and so
      guest_ITSTATE must be zero.  So write zero into ITSTATE right
      now, so that iropt can fold out almost all of the resulting
      junk.

      If we aren't sure, we can always safely skip this step.  So be a
      bit conservative about it: only poke around in the same page as
      this instruction, lest we get a fault from the previous page
      that would not otherwise have happened.  The saving grace is
      that such skipping is pretty rare -- it only happens,
      statistically, 18/4096ths of the time, so is judged unlikely to
      be a performance problems.

      FIXME: do better.  Take into account the number of insns covered
      by any IT insns we find, to rule out cases where an IT clearly
      cannot cover this instruction.  This would improve behaviour for
      branch targets immediately following an IT-guarded group that is
      not of full length.  Eg, (and completely ignoring issues of 16-
      vs 32-bit insn length):

             ite cond
             insn1
             insn2
      label: insn3
             insn4

      The 'it' only conditionalises insn1 and insn2.  However, the
      current analysis is conservative and considers insn3 and insn4
      also possibly guarded.  Hence if 'label:' is the start of a hot
      loop we will get a big performance hit.
   */
   {
      /* Summary result of this analysis: False == safe but
         suboptimal. */
      vassert(guaranteedUnconditional == False);

      UInt pc = guest_R15_curr_instr_notENC;
      vassert(0 == (pc & 1));

      UInt pageoff = pc & 0xFFF;
      if (pageoff >= 18) {
         /* It's safe to poke about in the 9 halfwords preceding this
            insn.  So, have a look at them. */
         guaranteedUnconditional = True; /* assume no 'it' insn found,
                                            till we do */
         UShort* hwp = (UShort*)(HWord)pc;
         Int i;
         for (i = -1; i >= -9; i--) {
            /* We're in the same page.  (True, but commented out due
               to expense.) */
            /*
            vassert( ( ((UInt)(&hwp[i])) & 0xFFFFF000 )
                      == ( pc & 0xFFFFF000 ) );
            */
            /* All valid IT instructions must have the form 0xBFxy,
               where x can be anything, but y must be nonzero.  Find
               the number of insns covered by it (1 .. 4) and check to
               see if it can possibly reach up to the instruction in
               question.  Some (x,y) combinations mean UNPREDICTABLE,
               and the table is constructed to be conservative by
               returning 4 for those cases, so the analysis is safe
               even if the code uses unpredictable IT instructions (in
               which case its authors are nuts, but hey.)  */
            UShort hwp_i = hwp[i];
            if (UNLIKELY((hwp_i & 0xFF00) == 0xBF00 && (hwp_i & 0xF) != 0)) {
               /* might be an 'it' insn. */
               /* # guarded insns */
               Int n_guarded = (Int)it_length_table[hwp_i & 0xFF];
               vassert(n_guarded >= 1 && n_guarded <= 4);
               if (n_guarded * 2 /* # guarded HWs, worst case */
                   > (-(i+1)))   /* -(i+1): # remaining HWs after the IT */
                   /* -(i+0) also seems to work, even though I think
                      it's wrong.  I don't understand that. */
                  guaranteedUnconditional = False;
               break;
            }
         }
      }
   }
   /* --- END ITxxx optimisation analysis --- */

   /* Generate the guarding condition for this insn, by examining
      ITSTATE.  Assign it to condT.  Also, generate new
      values for ITSTATE ready for stuffing back into the
      guest state, but don't actually do the Put yet, since it will
      need to stuffed back in only after the instruction gets to a
      point where it is sure to complete.  Mostly we let the code at
      decode_success handle this, but in cases where the insn contains
      a side exit, we have to update them before the exit. */

   /* If the ITxxx optimisation analysis above could not prove that
      this instruction is guaranteed unconditional, we insert a
      lengthy IR preamble to compute the guarding condition at
      runtime.  If it can prove it (which obviously we hope is the
      normal case) then we insert a minimal preamble, which is
      equivalent to setting guest_ITSTATE to zero and then folding
      that through the full preamble (which completely disappears). */

   IRTemp condT              = IRTemp_INVALID;
   IRTemp cond_AND_notInIT_T = IRTemp_INVALID;

   IRTemp new_itstate        = IRTemp_INVALID;
   vassert(old_itstate == IRTemp_INVALID);

   if (guaranteedUnconditional) {
      /* BEGIN "partial eval { ITSTATE = 0; STANDARD_PREAMBLE; }" */

      // ITSTATE = 0 :: I32
      IRTemp z32 = newTemp(Ity_I32);
      assign(z32, mkU32(0));
      put_ITSTATE(z32);

      // old_itstate = 0 :: I32
      //
      // old_itstate = get_ITSTATE();
      old_itstate = z32; /* 0 :: I32 */

      // new_itstate = old_itstate >> 8
      //             = 0 >> 8
      //             = 0 :: I32
      //
      // new_itstate = newTemp(Ity_I32);
      // assign(new_itstate,
      //        binop(Iop_Shr32, mkexpr(old_itstate), mkU8(8)));
      new_itstate = z32;

      // ITSTATE = 0 :: I32(again)
      //
      // put_ITSTATE(new_itstate);

      // condT1 = calc_cond_dyn( xor(and(old_istate,0xF0), 0xE0) )
      //        = calc_cond_dyn( xor(0,0xE0) )
      //        = calc_cond_dyn ( 0xE0 )
      //        = 1 :: I32
      // Not that this matters, since the computed value is not used:
      // see condT folding below
      //
      // IRTemp condT1 = newTemp(Ity_I32);
      // assign(condT1,
      //        mk_armg_calculate_condition_dyn(
      //           binop(Iop_Xor32,
      //                 binop(Iop_And32, mkexpr(old_itstate), mkU32(0xF0)),
      //                 mkU32(0xE0))
      //       )
      // );

      // condT = 32to8(and32(old_itstate,0xF0)) == 0  ? 1  : condT1
      //       = 32to8(and32(0,0xF0)) == 0  ? 1  : condT1
      //       = 32to8(0) == 0  ? 1  : condT1
      //       = 0 == 0  ? 1  : condT1
      //       = 1
      //
      // condT = newTemp(Ity_I32);
      // assign(condT, IRExpr_ITE(
      //                  unop(Iop_32to8, binop(Iop_And32,
      //                                        mkexpr(old_itstate),
      //                                        mkU32(0xF0))),
      //                  mkexpr(condT1),
      //                  mkU32(1))
      //       ));
      condT = newTemp(Ity_I32);
      assign(condT, mkU32(1));

      // notInITt = xor32(and32(old_itstate, 1), 1)
      //          = xor32(and32(0, 1), 1)
      //          = xor32(0, 1)
      //          = 1 :: I32
      //
      // IRTemp notInITt = newTemp(Ity_I32);
      // assign(notInITt,
      //        binop(Iop_Xor32,
      //              binop(Iop_And32, mkexpr(old_itstate), mkU32(1)),
      //              mkU32(1)));

      // cond_AND_notInIT_T = and32(notInITt, condT)
      //                    = and32(1, 1)
      //                    = 1
      //
      // cond_AND_notInIT_T = newTemp(Ity_I32);
      // assign(cond_AND_notInIT_T,
      //        binop(Iop_And32, mkexpr(notInITt), mkexpr(condT)));
      cond_AND_notInIT_T = condT; /* 1 :: I32 */

      /* END "partial eval { ITSTATE = 0; STANDARD_PREAMBLE; }" */
   } else {
      /* BEGIN { STANDARD PREAMBLE; } */

      old_itstate = get_ITSTATE();

      new_itstate = newTemp(Ity_I32);
      assign(new_itstate,
             binop(Iop_Shr32, mkexpr(old_itstate), mkU8(8)));

      put_ITSTATE(new_itstate);

      /* Same strategy as for ARM insns: generate a condition
         temporary at this point (or IRTemp_INVALID, meaning
         unconditional).  We leave it to lower-level instruction
         decoders to decide whether they can generate straight-line
         code, or whether they must generate a side exit before the
         instruction.  condT :: Ity_I32 and is always either zero or
         one. */
      IRTemp condT1 = newTemp(Ity_I32);
      assign(condT1,
             mk_armg_calculate_condition_dyn(
                binop(Iop_Xor32,
                      binop(Iop_And32, mkexpr(old_itstate), mkU32(0xF0)),
                      mkU32(0xE0))
            )
      );

      /* This is a bit complex, but needed to make Memcheck understand
         that, if the condition in old_itstate[7:4] denotes AL (that
         is, if this instruction is to be executed unconditionally),
         then condT does not depend on the results of calling the
         helper.

         We test explicitly for old_itstate[7:4] == AL ^ 0xE, and in
         that case set condT directly to 1.  Else we use the results
         of the helper.  Since old_itstate is always defined and
         because Memcheck does lazy V-bit propagation through ITE,
         this will cause condT to always be a defined 1 if the
         condition is 'AL'.  From an execution semantics point of view
         this is irrelevant since we're merely duplicating part of the
         behaviour of the helper.  But it makes it clear to Memcheck,
         in this case, that condT does not in fact depend on the
         contents of the condition code thunk.  Without it, we get
         quite a lot of false errors.

         So, just to clarify: from a straight semantics point of view,
         we can simply do "assign(condT, mkexpr(condT1))", and the
         simulator still runs fine.  It's just that we get loads of
         false errors from Memcheck. */
      condT = newTemp(Ity_I32);
      assign(condT, IRExpr_ITE(
                       binop(Iop_CmpNE32, binop(Iop_And32,
                                                mkexpr(old_itstate),
                                                mkU32(0xF0)),
                                          mkU32(0)),
                       mkexpr(condT1),
                       mkU32(1)
            ));

      /* Something we don't have in ARM: generate a 0 or 1 value
         indicating whether or not we are in an IT block (NB: 0 = in
         IT block, 1 = not in IT block).  This is used to gate
         condition code updates in 16-bit Thumb instructions. */
      IRTemp notInITt = newTemp(Ity_I32);
      assign(notInITt,
             binop(Iop_Xor32,
                   binop(Iop_And32, mkexpr(old_itstate), mkU32(1)),
                   mkU32(1)));

      /* Compute 'condT && notInITt' -- that is, the instruction is
         going to execute, and we're not in an IT block.  This is the
         gating condition for updating condition codes in 16-bit Thumb
         instructions, except for CMP, CMN and TST. */
      cond_AND_notInIT_T = newTemp(Ity_I32);
      assign(cond_AND_notInIT_T,
             binop(Iop_And32, mkexpr(notInITt), mkexpr(condT)));
      /* END { STANDARD PREAMBLE; } */
   }


   /* At this point:
      * ITSTATE has been updated
      * condT holds the guarding condition for this instruction (0 or 1),
      * notInITt is 1 if we're in "normal" code, 0 if in an IT block
      * cond_AND_notInIT_T is the AND of the above two.

      If the instruction proper can't trap, then there's nothing else
      to do w.r.t. ITSTATE -- just go and and generate IR for the
      insn, taking into account the guarding condition.

      If, however, the instruction might trap, then we must back up
      ITSTATE to the old value, and re-update it after the potentially
      trapping IR section.  A trap can happen either via a memory
      reference or because we need to throw SIGILL.

      If an instruction has a side exit, we need to be sure that any
      ITSTATE backup is re-updated before the side exit.
   */

   /* ----------------------------------------------------------- */
   /* --                                                       -- */
   /* -- Thumb 16-bit integer instructions                     -- */
   /* --                                                       -- */
   /* -- IMPORTANT: references to insn1 or INSN1 are           -- */
   /* --            not allowed in this section                -- */
   /* --                                                       -- */
   /* ----------------------------------------------------------- */

   /* 16-bit instructions inside an IT block, apart from CMP, CMN and
      TST, do not set the condition codes.  Hence we must dynamically
      test for this case for every condition code update. */

   IROp   anOp   = Iop_INVALID;
   const HChar* anOpNm = NULL;

   /* ================ 16-bit 15:6 cases ================ */

   switch (INSN0(15,6)) {

   case 0x10a:   // CMP
   case 0x10b: { // CMN
      /* ---------------- CMP Rn, Rm ---------------- */
      Bool   isCMN = INSN0(15,6) == 0x10b;
      UInt   rN    = INSN0(2,0);
      UInt   rM    = INSN0(5,3);
      IRTemp argL  = newTemp(Ity_I32);
      IRTemp argR  = newTemp(Ity_I32);
      assign( argL, getIRegT(rN) );
      assign( argR, getIRegT(rM) );
      /* Update flags regardless of whether in an IT block or not. */
      setFlags_D1_D2( isCMN ? ARMG_CC_OP_ADD : ARMG_CC_OP_SUB,
                      argL, argR, condT );
      DIP("%s r%u, r%u\n", isCMN ? "cmn" : "cmp", rN, rM);
      goto decode_success;
   }

   case 0x108: {
      /* ---------------- TST Rn, Rm ---------------- */
      UInt   rN   = INSN0(2,0);
      UInt   rM   = INSN0(5,3);
      IRTemp oldC = newTemp(Ity_I32);
      IRTemp oldV = newTemp(Ity_I32);
      IRTemp res  = newTemp(Ity_I32);
      assign( oldC, mk_armg_calculate_flag_c() );
      assign( oldV, mk_armg_calculate_flag_v() );
      assign( res,  binop(Iop_And32, getIRegT(rN), getIRegT(rM)) );
      /* Update flags regardless of whether in an IT block or not. */
      setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV, condT );
      DIP("tst r%u, r%u\n", rN, rM);
      goto decode_success;
   }

   case 0x109: {
      /* ---------------- NEGS Rd, Rm ---------------- */
      /* Rd = -Rm */
      UInt   rM   = INSN0(5,3);
      UInt   rD   = INSN0(2,0);
      IRTemp arg  = newTemp(Ity_I32);
      IRTemp zero = newTemp(Ity_I32);
      assign(arg, getIRegT(rM));
      assign(zero, mkU32(0));
      // rD can never be r15
      putIRegT(rD, binop(Iop_Sub32, mkexpr(zero), mkexpr(arg)), condT);
      setFlags_D1_D2( ARMG_CC_OP_SUB, zero, arg, cond_AND_notInIT_T);
      DIP("negs r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x10F: {
      /* ---------------- MVNS Rd, Rm ---------------- */
      /* Rd = ~Rm */
      UInt   rM   = INSN0(5,3);
      UInt   rD   = INSN0(2,0);
      IRTemp oldV = newTemp(Ity_I32);
      IRTemp oldC = newTemp(Ity_I32);
      IRTemp res  = newTemp(Ity_I32);
      assign( oldV, mk_armg_calculate_flag_v() );
      assign( oldC, mk_armg_calculate_flag_c() );
      assign(res, unop(Iop_Not32, getIRegT(rM)));
      // rD can never be r15
      putIRegT(rD, mkexpr(res), condT);
      setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                         cond_AND_notInIT_T );
      DIP("mvns r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x10C:
      /* ---------------- ORRS Rd, Rm ---------------- */
      anOp = Iop_Or32; anOpNm = "orr"; goto and_orr_eor_mul;
   case 0x100:
      /* ---------------- ANDS Rd, Rm ---------------- */
      anOp = Iop_And32; anOpNm = "and"; goto and_orr_eor_mul;
   case 0x101:
      /* ---------------- EORS Rd, Rm ---------------- */
      anOp = Iop_Xor32; anOpNm = "eor"; goto and_orr_eor_mul;
   case 0x10d:
      /* ---------------- MULS Rd, Rm ---------------- */
      anOp = Iop_Mul32; anOpNm = "mul"; goto and_orr_eor_mul;
   and_orr_eor_mul: {
      /* Rd = Rd `op` Rm */
      UInt   rM   = INSN0(5,3);
      UInt   rD   = INSN0(2,0);
      IRTemp res  = newTemp(Ity_I32);
      IRTemp oldV = newTemp(Ity_I32);
      IRTemp oldC = newTemp(Ity_I32);
      assign( oldV, mk_armg_calculate_flag_v() );
      assign( oldC, mk_armg_calculate_flag_c() );
      assign( res, binop(anOp, getIRegT(rD), getIRegT(rM) ));
      // not safe to read guest state after here
      // rD can never be r15
      putIRegT(rD, mkexpr(res), condT);
      setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                         cond_AND_notInIT_T );
      DIP("%s r%u, r%u\n", anOpNm, rD, rM);
      goto decode_success;
   }

   case 0x10E: {
      /* ---------------- BICS Rd, Rm ---------------- */
      /* Rd = Rd & ~Rm */
      UInt   rM   = INSN0(5,3);
      UInt   rD   = INSN0(2,0);
      IRTemp res  = newTemp(Ity_I32);
      IRTemp oldV = newTemp(Ity_I32);
      IRTemp oldC = newTemp(Ity_I32);
      assign( oldV, mk_armg_calculate_flag_v() );
      assign( oldC, mk_armg_calculate_flag_c() );
      assign( res, binop(Iop_And32, getIRegT(rD),
                                    unop(Iop_Not32, getIRegT(rM) )));
      // not safe to read guest state after here
      // rD can never be r15
      putIRegT(rD, mkexpr(res), condT);
      setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                         cond_AND_notInIT_T );
      DIP("bics r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x105: {
      /* ---------------- ADCS Rd, Rm ---------------- */
      /* Rd = Rd + Rm + oldC */
      UInt   rM   = INSN0(5,3);
      UInt   rD   = INSN0(2,0);
      IRTemp argL = newTemp(Ity_I32);
      IRTemp argR = newTemp(Ity_I32);
      IRTemp oldC = newTemp(Ity_I32);
      IRTemp res  = newTemp(Ity_I32);
      assign(argL, getIRegT(rD));
      assign(argR, getIRegT(rM));
      assign(oldC, mk_armg_calculate_flag_c());
      assign(res, binop(Iop_Add32,
                        binop(Iop_Add32, mkexpr(argL), mkexpr(argR)),
                        mkexpr(oldC)));
      // rD can never be r15
      putIRegT(rD, mkexpr(res), condT);
      setFlags_D1_D2_ND( ARMG_CC_OP_ADC, argL, argR, oldC,
                         cond_AND_notInIT_T );
      DIP("adcs r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x106: {
      /* ---------------- SBCS Rd, Rm ---------------- */
      /* Rd = Rd - Rm - (oldC ^ 1) */
      UInt   rM   = INSN0(5,3);
      UInt   rD   = INSN0(2,0);
      IRTemp argL = newTemp(Ity_I32);
      IRTemp argR = newTemp(Ity_I32);
      IRTemp oldC = newTemp(Ity_I32);
      IRTemp res  = newTemp(Ity_I32);
      assign(argL, getIRegT(rD));
      assign(argR, getIRegT(rM));
      assign(oldC, mk_armg_calculate_flag_c());
      assign(res, binop(Iop_Sub32,
                        binop(Iop_Sub32, mkexpr(argL), mkexpr(argR)),
                        binop(Iop_Xor32, mkexpr(oldC), mkU32(1))));
      // rD can never be r15
      putIRegT(rD, mkexpr(res), condT);
      setFlags_D1_D2_ND( ARMG_CC_OP_SBB, argL, argR, oldC,
                         cond_AND_notInIT_T );
      DIP("sbcs r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x2CB: {
      /* ---------------- UXTB Rd, Rm ---------------- */
      /* Rd = 8Uto32(Rm) */
      UInt rM = INSN0(5,3);
      UInt rD = INSN0(2,0);
      putIRegT(rD, binop(Iop_And32, getIRegT(rM), mkU32(0xFF)),
                   condT);
      DIP("uxtb r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x2C9: {
      /* ---------------- SXTB Rd, Rm ---------------- */
      /* Rd = 8Sto32(Rm) */
      UInt rM = INSN0(5,3);
      UInt rD = INSN0(2,0);
      putIRegT(rD, binop(Iop_Sar32,
                         binop(Iop_Shl32, getIRegT(rM), mkU8(24)),
                         mkU8(24)),
                   condT);
      DIP("sxtb r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x2CA: {
      /* ---------------- UXTH Rd, Rm ---------------- */
      /* Rd = 16Uto32(Rm) */
      UInt rM = INSN0(5,3);
      UInt rD = INSN0(2,0);
      putIRegT(rD, binop(Iop_And32, getIRegT(rM), mkU32(0xFFFF)),
                   condT);
      DIP("uxth r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x2C8: {
      /* ---------------- SXTH Rd, Rm ---------------- */
      /* Rd = 16Sto32(Rm) */
      UInt rM = INSN0(5,3);
      UInt rD = INSN0(2,0);
      putIRegT(rD, binop(Iop_Sar32,
                         binop(Iop_Shl32, getIRegT(rM), mkU8(16)),
                         mkU8(16)),
                   condT);
      DIP("sxth r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   case 0x102:   // LSLS
   case 0x103:   // LSRS
   case 0x104:   // ASRS
   case 0x107: { // RORS
      /* ---------------- LSLS Rs, Rd ---------------- */
      /* ---------------- LSRS Rs, Rd ---------------- */
      /* ---------------- ASRS Rs, Rd ---------------- */
      /* ---------------- RORS Rs, Rd ---------------- */
      /* Rd = Rd `op` Rs, and set flags */
      UInt   rS   = INSN0(5,3);
      UInt   rD   = INSN0(2,0);
      IRTemp oldV = newTemp(Ity_I32);
      IRTemp rDt  = newTemp(Ity_I32);
      IRTemp rSt  = newTemp(Ity_I32);
      IRTemp res  = newTemp(Ity_I32);
      IRTemp resC = newTemp(Ity_I32);
      const HChar* wot  = "???";
      assign(rSt, getIRegT(rS));
      assign(rDt, getIRegT(rD));
      assign(oldV, mk_armg_calculate_flag_v());
      /* Does not appear to be the standard 'how' encoding. */
      switch (INSN0(15,6)) {
         case 0x102:
            compute_result_and_C_after_LSL_by_reg(
               dis_buf, &res, &resC, rDt, rSt, rD, rS
            );
            wot = "lsl";
            break;
         case 0x103:
            compute_result_and_C_after_LSR_by_reg(
               dis_buf, &res, &resC, rDt, rSt, rD, rS
            );
            wot = "lsr";
            break;
         case 0x104:
            compute_result_and_C_after_ASR_by_reg(
               dis_buf, &res, &resC, rDt, rSt, rD, rS
            );
            wot = "asr";
            break;
         case 0x107:
            compute_result_and_C_after_ROR_by_reg(
               dis_buf, &res, &resC, rDt, rSt, rD, rS
            );
            wot = "ror";
            break;
         default:
            /*NOTREACHED*/vassert(0);
      }
      // not safe to read guest state after this point
      putIRegT(rD, mkexpr(res), condT);
      setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, resC, oldV,
                         cond_AND_notInIT_T );
      DIP("%ss r%u, r%u\n", wot, rS, rD);
      goto decode_success;
   }

   case 0x2E8:   // REV
   case 0x2E9: { // REV16
      /* ---------------- REV   Rd, Rm ---------------- */
      /* ---------------- REV16 Rd, Rm ---------------- */
      UInt rM = INSN0(5,3);
      UInt rD = INSN0(2,0);
      Bool isREV = INSN0(15,6) == 0x2E8;
      IRTemp arg = newTemp(Ity_I32);
      assign(arg, getIRegT(rM));
      IRTemp res = isREV ? gen_REV(arg) : gen_REV16(arg);
      putIRegT(rD, mkexpr(res), condT);
      DIP("rev%s r%u, r%u\n", isREV ? "" : "16", rD, rM);
      goto decode_success;
   }

   case 0x2EB: { // REVSH
      /* ---------------- REVSH Rd, Rn ---------------- */
      UInt rM = INSN0(5,3);
      UInt rD = INSN0(2,0);
      IRTemp irt_rM  = newTemp(Ity_I32);
      IRTemp irt_hi  = newTemp(Ity_I32);
      IRTemp irt_low = newTemp(Ity_I32);
      IRTemp irt_res = newTemp(Ity_I32);
      assign(irt_rM, getIRegT(rM));
      assign(irt_hi,
             binop(Iop_Sar32,
                   binop(Iop_Shl32, mkexpr(irt_rM), mkU8(24)),
                   mkU8(16)
             )
      );
      assign(irt_low,
             binop(Iop_And32,
                   binop(Iop_Shr32, mkexpr(irt_rM), mkU8(8)),
                   mkU32(0xFF)
             )
      );
      assign(irt_res,
             binop(Iop_Or32, mkexpr(irt_hi), mkexpr(irt_low))
      );
      putIRegT(rD, mkexpr(irt_res), condT);
      DIP("revsh r%u, r%u\n", rD, rM);
      goto decode_success;
   }

   default:
      break; /* examine the next shortest prefix */

   }


   /* ================ 16-bit 15:7 cases ================ */

   switch (INSN0(15,7)) {

   case BITS9(1,0,1,1,0,0,0,0,0): {
      /* ------------ ADD SP, #imm7 * 4 ------------ */
      UInt uimm7 = INSN0(6,0);
      putIRegT(13, binop(Iop_Add32, getIRegT(13), mkU32(uimm7 * 4)),
                   condT);
      DIP("add sp, #%u\n", uimm7 * 4);
      goto decode_success;
   }

   case BITS9(1,0,1,1,0,0,0,0,1): {
      /* ------------ SUB SP, #imm7 * 4 ------------ */
      UInt uimm7 = INSN0(6,0);
      putIRegT(13, binop(Iop_Sub32, getIRegT(13), mkU32(uimm7 * 4)),
                   condT);
      DIP("sub sp, #%u\n", uimm7 * 4);
      goto decode_success;
   }

   case BITS9(0,1,0,0,0,1,1,1,0): {
      /* ---------------- BX rM ---------------- */
      /* Branch to reg, and optionally switch modes.  Reg contains a
         suitably encoded address therefore (w CPSR.T at the bottom).
         Have to special-case r15, as usual. */
      UInt rM = (INSN0(6,6) << 3) | INSN0(5,3);
      if (BITS3(0,0,0) == INSN0(2,0)) {
         IRTemp dst = newTemp(Ity_I32);
         gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
         mk_skip_over_T16_if_cond_is_false(condT);
         condT = IRTemp_INVALID;
         // now uncond
         if (rM <= 14) {
            assign( dst, getIRegT(rM) );
         } else {
            vassert(rM == 15);
            assign( dst, mkU32(guest_R15_curr_instr_notENC + 4) );
         }
         llPutIReg(15, mkexpr(dst));
         dres.jk_StopHere = rM == 14 ? Ijk_Ret : Ijk_Boring;
         dres.whatNext    = Dis_StopHere;
         DIP("bx r%u (possibly switch to ARM mode)\n", rM);
         goto decode_success;
      }
      break;
   }

   /* ---------------- BLX rM ---------------- */
   /* Branch and link to interworking address in rM. */
   case BITS9(0,1,0,0,0,1,1,1,1): {
      if (BITS3(0,0,0) == INSN0(2,0)) {
         UInt rM = (INSN0(6,6) << 3) | INSN0(5,3);
         IRTemp dst = newTemp(Ity_I32);
         if (rM <= 14) {
            gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
            mk_skip_over_T16_if_cond_is_false(condT);
            condT = IRTemp_INVALID;
            // now uncond
            /* We're returning to Thumb code, hence "| 1" */
            assign( dst, getIRegT(rM) );
            putIRegT( 14, mkU32( (guest_R15_curr_instr_notENC + 2) | 1 ),
                          IRTemp_INVALID );
            llPutIReg(15, mkexpr(dst));
            dres.jk_StopHere = Ijk_Call;
            dres.whatNext    = Dis_StopHere;
            DIP("blx r%u (possibly switch to ARM mode)\n", rM);
            goto decode_success;
         }
         /* else unpredictable, fall through */
      }
      break;
   }

   default:
      break; /* examine the next shortest prefix */

   }


   /* ================ 16-bit 15:8 cases ================ */

   switch (INSN0(15,8)) {

   case BITS8(1,1,0,1,1,1,1,1): {
      /* ---------------- SVC ---------------- */
      UInt imm8 = INSN0(7,0);
      if (imm8 == 0) {
         /* A syscall.  We can't do this conditionally, hence: */
         mk_skip_over_T16_if_cond_is_false( condT );
         // FIXME: what if we have to back up and restart this insn?
         // then ITSTATE will be wrong (we'll have it as "used")
         // when it isn't.  Correct is to save ITSTATE in a 
         // stash pseudo-reg, and back up from that if we have to
         // restart.
         // uncond after here
         llPutIReg(15, mkU32( (guest_R15_curr_instr_notENC + 2) | 1 ));
         dres.jk_StopHere = Ijk_Sys_syscall;
         dres.whatNext    = Dis_StopHere;
         DIP("svc #0x%08x\n", imm8);
         goto decode_success;
      }
      /* else fall through */
      break;
   }

   case BITS8(0,1,0,0,0,1,0,0): {
      /* ---------------- ADD(HI) Rd, Rm ---------------- */
      UInt h1 = INSN0(7,7);
      UInt h2 = INSN0(6,6);
      UInt rM = (h2 << 3) | INSN0(5,3);
      UInt rD = (h1 << 3) | INSN0(2,0);
      //if (h1 == 0 && h2 == 0) { // Original T1 was more restrictive
      if (rD == 15 && rM == 15) {
         // then it's invalid
      } else {
         IRTemp res = newTemp(Ity_I32);
         assign( res, binop(Iop_Add32, getIRegT(rD), getIRegT(rM) ));
         if (rD != 15) {
            putIRegT( rD, mkexpr(res), condT );
         } else {
            /* Only allowed outside or last-in IT block; SIGILL if not so. */
            gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
            /* jump over insn if not selected */
            mk_skip_over_T16_if_cond_is_false(condT);
            condT = IRTemp_INVALID;
            // now uncond
            /* non-interworking branch */
            llPutIReg(15, binop(Iop_Or32, mkexpr(res), mkU32(1)));
            dres.jk_StopHere = Ijk_Boring;
            dres.whatNext    = Dis_StopHere;
         }
         DIP("add(hi) r%u, r%u\n", rD, rM);
         goto decode_success;
      }
      break;
   }

   case BITS8(0,1,0,0,0,1,0,1): {
      /* ---------------- CMP(HI) Rd, Rm ---------------- */
      UInt h1 = INSN0(7,7);
      UInt h2 = INSN0(6,6);
      UInt rM = (h2 << 3) | INSN0(5,3);
      UInt rN = (h1 << 3) | INSN0(2,0);
      if (h1 != 0 || h2 != 0) {
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         assign( argL, getIRegT(rN) );
         assign( argR, getIRegT(rM) );
         /* Update flags regardless of whether in an IT block or not. */
         setFlags_D1_D2( ARMG_CC_OP_SUB, argL, argR, condT );
         DIP("cmphi r%u, r%u\n", rN, rM);
         goto decode_success;
      }
      break;
   }

   case BITS8(0,1,0,0,0,1,1,0): {
      /* ---------------- MOV(HI) Rd, Rm ---------------- */
      UInt h1 = INSN0(7,7);
      UInt h2 = INSN0(6,6);
      UInt rM = (h2 << 3) | INSN0(5,3);
      UInt rD = (h1 << 3) | INSN0(2,0);
      /* The old ARM ARM seems to disallow the case where both Rd and
         Rm are "low" registers, but newer versions allow it. */
      if (1 /*h1 != 0 || h2 != 0*/) {
         IRTemp val = newTemp(Ity_I32);
         assign( val, getIRegT(rM) );
         if (rD != 15) {
            putIRegT( rD, mkexpr(val), condT );
         } else {
            /* Only allowed outside or last-in IT block; SIGILL if not so. */
            gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
            /* jump over insn if not selected */
            mk_skip_over_T16_if_cond_is_false(condT);
            condT = IRTemp_INVALID;
            // now uncond
            /* non-interworking branch */
            llPutIReg(15, binop(Iop_Or32, mkexpr(val), mkU32(1)));
            dres.jk_StopHere = rM == 14 ? Ijk_Ret : Ijk_Boring;
            dres.whatNext    = Dis_StopHere;
         }
         DIP("mov r%u, r%u\n", rD, rM);
         goto decode_success;
      }
      break;
   }

   case BITS8(1,0,1,1,1,1,1,1): {
      /* ---------------- IT (if-then) ---------------- */
      UInt firstcond = INSN0(7,4);
      UInt mask = INSN0(3,0);
      UInt newITSTATE = 0;
      /* This is the ITSTATE represented as described in
         libvex_guest_arm.h.  It is not the ARM ARM representation. */
      HChar c1 = '.';
      HChar c2 = '.';
      HChar c3 = '.';
      Bool valid = compute_ITSTATE( &newITSTATE, &c1, &c2, &c3,
                                    firstcond, mask );
      if (valid && firstcond != 0xF/*NV*/) {
         /* Not allowed in an IT block; SIGILL if so. */
         gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);

         IRTemp t = newTemp(Ity_I32);
         assign(t, mkU32(newITSTATE));
         put_ITSTATE(t);

         DIP("it%c%c%c %s\n", c1, c2, c3, nCC(firstcond));
         goto decode_success;
      }
      break;
   }

   case BITS8(1,0,1,1,0,0,0,1):
   case BITS8(1,0,1,1,0,0,1,1):
   case BITS8(1,0,1,1,1,0,0,1):
   case BITS8(1,0,1,1,1,0,1,1): {
      /* ---------------- CB{N}Z ---------------- */
      UInt rN    = INSN0(2,0);
      UInt bOP   = INSN0(11,11);
      UInt imm32 = (INSN0(9,9) << 6) | (INSN0(7,3) << 1);
      gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);
      /* It's a conditional branch forward. */
      IRTemp kond = newTemp(Ity_I1);
      assign( kond, binop(bOP ? Iop_CmpNE32 : Iop_CmpEQ32,
                          getIRegT(rN), mkU32(0)) );

      vassert(0 == (guest_R15_curr_instr_notENC & 1));
      /* Looks like the nearest insn we can branch to is the one after
         next.  That makes sense, as there's no point in being able to
         encode a conditional branch to the next instruction. */
      UInt dst = (guest_R15_curr_instr_notENC + 4 + imm32) | 1;
      stmt(IRStmt_Exit( mkexpr(kond),
                        Ijk_Boring,
                        IRConst_U32(toUInt(dst)),
                        OFFB_R15T ));
      DIP("cb%s r%u, 0x%x\n", bOP ? "nz" : "z", rN, dst - 1);
      goto decode_success;
   }

   default:
      break; /* examine the next shortest prefix */

   }


   /* ================ 16-bit 15:9 cases ================ */

   switch (INSN0(15,9)) {

   case BITS7(1,0,1,1,0,1,0): {
      /* ---------------- PUSH ---------------- */
      /* This is a bit like STMxx, but way simpler. Complications we
         don't have to deal with:
         * SP being one of the transferred registers
         * direction (increment vs decrement)
         * before-vs-after-ness
      */
      Int  i, nRegs;
      UInt bitR    = INSN0(8,8);
      UInt regList = INSN0(7,0);
      if (bitR) regList |= (1 << 14);
   
      /* At least one register must be transferred, else result is
         UNPREDICTABLE. */
      if (regList != 0) {
         /* Since we can't generate a guaranteed non-trapping IR
            sequence, (1) jump over the insn if it is gated false, and
            (2) back out the ITSTATE update. */
         mk_skip_over_T16_if_cond_is_false(condT);
         condT = IRTemp_INVALID;
         put_ITSTATE(old_itstate);
         // now uncond

         nRegs = 0;
         for (i = 0; i < 16; i++) {
            if ((regList & (1 << i)) != 0)
               nRegs++;
         }
         vassert(nRegs >= 1 && nRegs <= 9);

         /* Move SP down first of all, so we're "covered".  And don't
            mess with its alignment. */
         IRTemp newSP = newTemp(Ity_I32);
         assign(newSP, binop(Iop_Sub32, getIRegT(13), mkU32(4 * nRegs)));
         putIRegT(13, mkexpr(newSP), IRTemp_INVALID);

         /* Generate a transfer base address as a forced-aligned
            version of the final SP value. */
         IRTemp base = newTemp(Ity_I32);
         assign(base, binop(Iop_And32, mkexpr(newSP), mkU32(~3)));

         /* Now the transfers */
         nRegs = 0;
         for (i = 0; i < 16; i++) {
            if ((regList & (1 << i)) != 0) {
               storeLE( binop(Iop_Add32, mkexpr(base), mkU32(4 * nRegs)),
                        getIRegT(i) );
               nRegs++;
            }
         }

         /* Reinstate the ITSTATE update. */
         put_ITSTATE(new_itstate);

         DIP("push {%s0x%04x}\n", bitR ? "lr," : "", regList & 0xFF);
         goto decode_success;
      }
      break;
   }

   case BITS7(1,0,1,1,1,1,0): {
      /* ---------------- POP ---------------- */
      Int  i, nRegs;
      UInt bitR    = INSN0(8,8);
      UInt regList = INSN0(7,0);
   
      /* At least one register must be transferred, else result is
         UNPREDICTABLE. */
      if (regList != 0 || bitR) {
         /* Since we can't generate a guaranteed non-trapping IR
            sequence, (1) jump over the insn if it is gated false, and
            (2) back out the ITSTATE update. */
         mk_skip_over_T16_if_cond_is_false(condT);
         condT = IRTemp_INVALID;
         put_ITSTATE(old_itstate);
         // now uncond

         nRegs = 0;
         for (i = 0; i < 8; i++) {
            if ((regList & (1 << i)) != 0)
               nRegs++;
         }
         vassert(nRegs >= 0 && nRegs <= 8);
         vassert(bitR == 0 || bitR == 1);

         IRTemp oldSP = newTemp(Ity_I32);
         assign(oldSP, getIRegT(13));

         /* Generate a transfer base address as a forced-aligned
            version of the original SP value. */
         IRTemp base = newTemp(Ity_I32);
         assign(base, binop(Iop_And32, mkexpr(oldSP), mkU32(~3)));

         /* Compute a new value for SP, but don't install it yet, so
            that we're "covered" until all the transfers are done.
            And don't mess with its alignment. */
         IRTemp newSP = newTemp(Ity_I32);
         assign(newSP, binop(Iop_Add32, mkexpr(oldSP),
                                        mkU32(4 * (nRegs + bitR))));

         /* Now the transfers, not including PC */
         nRegs = 0;
         for (i = 0; i < 8; i++) {
            if ((regList & (1 << i)) != 0) {
               putIRegT(i, loadLE( Ity_I32,
                                   binop(Iop_Add32, mkexpr(base),
                                                    mkU32(4 * nRegs))),
                           IRTemp_INVALID );
               nRegs++;
            }
         }

         IRTemp newPC = IRTemp_INVALID;
         if (bitR) {
            newPC = newTemp(Ity_I32);
            assign( newPC, loadLE( Ity_I32,
                                   binop(Iop_Add32, mkexpr(base),
                                                    mkU32(4 * nRegs))));
         }

         /* Now we can safely install the new SP value */
         putIRegT(13, mkexpr(newSP), IRTemp_INVALID);

         /* Reinstate the ITSTATE update. */
         put_ITSTATE(new_itstate);

         /* now, do we also have to do a branch?  If so, it turns out
            that the new PC value is encoded exactly as we need it to
            be -- with CPSR.T in the bottom bit.  So we can simply use
            it as is, no need to mess with it.  Note, therefore, this
            is an interworking return. */
         if (bitR) {
            llPutIReg(15, mkexpr(newPC));
            dres.jk_StopHere = Ijk_Ret;
            dres.whatNext    = Dis_StopHere;
         }

         DIP("pop {%s0x%04x}\n", bitR ? "pc," : "", regList & 0xFF);
         goto decode_success;
      }
      break;
   }

   case BITS7(0,0,0,1,1,1,0):   /* ADDS */
   case BITS7(0,0,0,1,1,1,1): { /* SUBS */
      /* ---------------- ADDS Rd, Rn, #uimm3 ---------------- */
      /* ---------------- SUBS Rd, Rn, #uimm3 ---------------- */
      UInt   uimm3 = INSN0(8,6);
      UInt   rN    = INSN0(5,3);
      UInt   rD    = INSN0(2,0);
      UInt   isSub = INSN0(9,9);
      IRTemp argL  = newTemp(Ity_I32);
      IRTemp argR  = newTemp(Ity_I32);
      assign( argL, getIRegT(rN) );
      assign( argR, mkU32(uimm3) );
      putIRegT(rD, binop(isSub ? Iop_Sub32 : Iop_Add32,
                         mkexpr(argL), mkexpr(argR)),
                   condT);
      setFlags_D1_D2( isSub ? ARMG_CC_OP_SUB : ARMG_CC_OP_ADD,
                      argL, argR, cond_AND_notInIT_T );
      DIP("%s r%u, r%u, #%u\n", isSub ? "subs" : "adds", rD, rN, uimm3);
      goto decode_success;
   }

   case BITS7(0,0,0,1,1,0,0):   /* ADDS */
   case BITS7(0,0,0,1,1,0,1): { /* SUBS */
      /* ---------------- ADDS Rd, Rn, Rm ---------------- */
      /* ---------------- SUBS Rd, Rn, Rm ---------------- */
      UInt   rM    = INSN0(8,6);
      UInt   rN    = INSN0(5,3);
      UInt   rD    = INSN0(2,0);
      UInt   isSub = INSN0(9,9);
      IRTemp argL  = newTemp(Ity_I32);
      IRTemp argR  = newTemp(Ity_I32);
      assign( argL, getIRegT(rN) );
      assign( argR, getIRegT(rM) );
      putIRegT( rD, binop(isSub ? Iop_Sub32 : Iop_Add32,
                          mkexpr(argL), mkexpr(argR)),
                    condT );
      setFlags_D1_D2( isSub ? ARMG_CC_OP_SUB : ARMG_CC_OP_ADD,
                      argL, argR, cond_AND_notInIT_T );
      DIP("%s r%u, r%u, r%u\n", isSub ? "subs" : "adds", rD, rN, rM);
      goto decode_success;
   }

   case BITS7(0,1,0,1,0,0,0):   /* STR */
   case BITS7(0,1,0,1,1,0,0): { /* LDR */
      /* ------------- LDR Rd, [Rn, Rm] ------------- */
      /* ------------- STR Rd, [Rn, Rm] ------------- */
      /* LDR/STR Rd, [Rn + Rm] */
      UInt    rD   = INSN0(2,0);
      UInt    rN   = INSN0(5,3);
      UInt    rM   = INSN0(8,6);
      UInt    isLD = INSN0(11,11);

      IRExpr* ea = binop(Iop_Add32, getIRegT(rN), getIRegT(rM));
      put_ITSTATE(old_itstate); // backout
      if (isLD) {
         IRTemp tD = newTemp(Ity_I32);
         loadGuardedLE( tD, ILGop_Ident32, ea, llGetIReg(rD), condT );
         putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      } else {
         storeGuardedLE(ea, getIRegT(rD), condT);
      }
      put_ITSTATE(new_itstate); // restore

      DIP("%s r%u, [r%u, r%u]\n", isLD ? "ldr" : "str", rD, rN, rM);
      goto decode_success;
   }

   case BITS7(0,1,0,1,0,0,1):
   case BITS7(0,1,0,1,1,0,1): {
      /* ------------- LDRH Rd, [Rn, Rm] ------------- */
      /* ------------- STRH Rd, [Rn, Rm] ------------- */
      /* LDRH/STRH Rd, [Rn + Rm] */
      UInt    rD   = INSN0(2,0);
      UInt    rN   = INSN0(5,3);
      UInt    rM   = INSN0(8,6);
      UInt    isLD = INSN0(11,11);

      IRExpr* ea = binop(Iop_Add32, getIRegT(rN), getIRegT(rM));
      put_ITSTATE(old_itstate); // backout
      if (isLD) {
         IRTemp tD = newTemp(Ity_I32);
         loadGuardedLE(tD, ILGop_16Uto32, ea, llGetIReg(rD), condT);
         putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      } else {
         storeGuardedLE( ea, unop(Iop_32to16, getIRegT(rD)), condT );
      }
      put_ITSTATE(new_itstate); // restore

      DIP("%sh r%u, [r%u, r%u]\n", isLD ? "ldr" : "str", rD, rN, rM);
      goto decode_success;
   }

   case BITS7(0,1,0,1,1,1,1): {
      /* ------------- LDRSH Rd, [Rn, Rm] ------------- */
      /* LDRSH Rd, [Rn + Rm] */
      UInt    rD = INSN0(2,0);
      UInt    rN = INSN0(5,3);
      UInt    rM = INSN0(8,6);

      IRExpr* ea = binop(Iop_Add32, getIRegT(rN), getIRegT(rM));
      put_ITSTATE(old_itstate); // backout
      IRTemp tD = newTemp(Ity_I32);
      loadGuardedLE(tD, ILGop_16Sto32, ea, llGetIReg(rD), condT);
      putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      put_ITSTATE(new_itstate); // restore

      DIP("ldrsh r%u, [r%u, r%u]\n", rD, rN, rM);
      goto decode_success;
   }

   case BITS7(0,1,0,1,0,1,1): {
      /* ------------- LDRSB Rd, [Rn, Rm] ------------- */
      /* LDRSB Rd, [Rn + Rm] */
      UInt    rD = INSN0(2,0);
      UInt    rN = INSN0(5,3);
      UInt    rM = INSN0(8,6);

      IRExpr* ea = binop(Iop_Add32, getIRegT(rN), getIRegT(rM));
      put_ITSTATE(old_itstate); // backout
      IRTemp tD = newTemp(Ity_I32);
      loadGuardedLE(tD, ILGop_8Sto32, ea, llGetIReg(rD), condT);
      putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      put_ITSTATE(new_itstate); // restore

      DIP("ldrsb r%u, [r%u, r%u]\n", rD, rN, rM);
      goto decode_success;
   }

   case BITS7(0,1,0,1,0,1,0):
   case BITS7(0,1,0,1,1,1,0): {
      /* ------------- LDRB Rd, [Rn, Rm] ------------- */
      /* ------------- STRB Rd, [Rn, Rm] ------------- */
      /* LDRB/STRB Rd, [Rn + Rm] */
      UInt    rD   = INSN0(2,0);
      UInt    rN   = INSN0(5,3);
      UInt    rM   = INSN0(8,6);
      UInt    isLD = INSN0(11,11);

      IRExpr* ea = binop(Iop_Add32, getIRegT(rN), getIRegT(rM));
      put_ITSTATE(old_itstate); // backout
      if (isLD) {
         IRTemp tD = newTemp(Ity_I32);
         loadGuardedLE(tD, ILGop_8Uto32, ea, llGetIReg(rD), condT);
         putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      } else {
         storeGuardedLE( ea, unop(Iop_32to8, getIRegT(rD)), condT );
      }
      put_ITSTATE(new_itstate); // restore

      DIP("%sb r%u, [r%u, r%u]\n", isLD ? "ldr" : "str", rD, rN, rM);
      goto decode_success;
   }

   default:
      break; /* examine the next shortest prefix */

   }


   /* ================ 16-bit 15:11 cases ================ */

   switch (INSN0(15,11)) {

   case BITS5(0,0,1,1,0):
   case BITS5(0,0,1,1,1): {
      /* ---------------- ADDS Rn, #uimm8 ---------------- */
      /* ---------------- SUBS Rn, #uimm8 ---------------- */
      UInt   isSub = INSN0(11,11);
      UInt   rN    = INSN0(10,8);
      UInt   uimm8 = INSN0(7,0);
      IRTemp argL  = newTemp(Ity_I32);
      IRTemp argR  = newTemp(Ity_I32);
      assign( argL, getIRegT(rN) );
      assign( argR, mkU32(uimm8) );
      putIRegT( rN, binop(isSub ? Iop_Sub32 : Iop_Add32,
                          mkexpr(argL), mkexpr(argR)), condT );
      setFlags_D1_D2( isSub ? ARMG_CC_OP_SUB : ARMG_CC_OP_ADD,
                      argL, argR, cond_AND_notInIT_T );
      DIP("%s r%u, #%u\n", isSub ? "subs" : "adds", rN, uimm8);
      goto decode_success;
   }

   case BITS5(1,0,1,0,0): {
      /* ---------------- ADD rD, PC, #imm8 * 4 ---------------- */
      /* a.k.a. ADR */
      /* rD = align4(PC) + imm8 * 4 */
      UInt rD   = INSN0(10,8);
      UInt imm8 = INSN0(7,0);
      putIRegT(rD, binop(Iop_Add32, 
                         binop(Iop_And32, getIRegT(15), mkU32(~3U)),
                         mkU32(imm8 * 4)),
                   condT);
      DIP("add r%u, pc, #%u\n", rD, imm8 * 4);
      goto decode_success;
   }

   case BITS5(1,0,1,0,1): {
      /* ---------------- ADD rD, SP, #imm8 * 4 ---------------- */
      UInt rD   = INSN0(10,8);
      UInt imm8 = INSN0(7,0);
      putIRegT(rD, binop(Iop_Add32, getIRegT(13), mkU32(imm8 * 4)),
                   condT);
      DIP("add r%u, r13, #%u\n", rD, imm8 * 4);
      goto decode_success;
   }

   case BITS5(0,0,1,0,1): {
      /* ---------------- CMP Rn, #uimm8 ---------------- */
      UInt   rN    = INSN0(10,8);
      UInt   uimm8 = INSN0(7,0);
      IRTemp argL  = newTemp(Ity_I32);
      IRTemp argR  = newTemp(Ity_I32);
      assign( argL, getIRegT(rN) );
      assign( argR, mkU32(uimm8) );
      /* Update flags regardless of whether in an IT block or not. */
      setFlags_D1_D2( ARMG_CC_OP_SUB, argL, argR, condT );
      DIP("cmp r%u, #%u\n", rN, uimm8);
      goto decode_success;
   }

   case BITS5(0,0,1,0,0): {
      /* -------------- (T1) MOVS Rn, #uimm8 -------------- */
      UInt   rD    = INSN0(10,8);
      UInt   uimm8 = INSN0(7,0);
      IRTemp oldV  = newTemp(Ity_I32);
      IRTemp oldC  = newTemp(Ity_I32);
      IRTemp res   = newTemp(Ity_I32);
      assign( oldV, mk_armg_calculate_flag_v() );
      assign( oldC, mk_armg_calculate_flag_c() );
      assign( res, mkU32(uimm8) );
      putIRegT(rD, mkexpr(res), condT);
      setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                         cond_AND_notInIT_T );
      DIP("movs r%u, #%u\n", rD, uimm8);
      goto decode_success;
   }

   case BITS5(0,1,0,0,1): {
      /* ------------- LDR Rd, [PC, #imm8 * 4] ------------- */
      /* LDR Rd, [align4(PC) + imm8 * 4] */
      UInt   rD   = INSN0(10,8);
      UInt   imm8 = INSN0(7,0);
      IRTemp ea   = newTemp(Ity_I32);

      assign(ea, binop(Iop_Add32, 
                       binop(Iop_And32, getIRegT(15), mkU32(~3U)),
                       mkU32(imm8 * 4)));
      put_ITSTATE(old_itstate); // backout
      IRTemp tD = newTemp(Ity_I32);
      loadGuardedLE( tD, ILGop_Ident32, mkexpr(ea), llGetIReg(rD), condT );
      putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      put_ITSTATE(new_itstate); // restore

      DIP("ldr r%u, [pc, #%u]\n", rD, imm8 * 4);
      goto decode_success;
   }

   case BITS5(0,1,1,0,0):   /* STR */
   case BITS5(0,1,1,0,1): { /* LDR */
      /* ------------- LDR Rd, [Rn, #imm5 * 4] ------------- */
      /* ------------- STR Rd, [Rn, #imm5 * 4] ------------- */
      /* LDR/STR Rd, [Rn + imm5 * 4] */
      UInt    rD   = INSN0(2,0);
      UInt    rN   = INSN0(5,3);
      UInt    imm5 = INSN0(10,6);
      UInt    isLD = INSN0(11,11);

      IRExpr* ea = binop(Iop_Add32, getIRegT(rN), mkU32(imm5 * 4));
      put_ITSTATE(old_itstate); // backout
      if (isLD) {
         IRTemp tD = newTemp(Ity_I32);
         loadGuardedLE( tD, ILGop_Ident32, ea, llGetIReg(rD), condT );
         putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      } else {
         storeGuardedLE( ea, getIRegT(rD), condT );
      }
      put_ITSTATE(new_itstate); // restore

      DIP("%s r%u, [r%u, #%u]\n", isLD ? "ldr" : "str", rD, rN, imm5 * 4);
      goto decode_success;
   }

   case BITS5(1,0,0,0,0):   /* STRH */
   case BITS5(1,0,0,0,1): { /* LDRH */
      /* ------------- LDRH Rd, [Rn, #imm5 * 2] ------------- */
      /* ------------- STRH Rd, [Rn, #imm5 * 2] ------------- */
      /* LDRH/STRH Rd, [Rn + imm5 * 2] */
      UInt    rD   = INSN0(2,0);
      UInt    rN   = INSN0(5,3);
      UInt    imm5 = INSN0(10,6);
      UInt    isLD = INSN0(11,11);

      IRExpr* ea = binop(Iop_Add32, getIRegT(rN), mkU32(imm5 * 2));
      put_ITSTATE(old_itstate); // backout
      if (isLD) {
         IRTemp tD = newTemp(Ity_I32);
         loadGuardedLE( tD, ILGop_16Uto32, ea, llGetIReg(rD), condT );
         putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      } else {
         storeGuardedLE( ea, unop(Iop_32to16, getIRegT(rD)), condT );
      }
      put_ITSTATE(new_itstate); // restore

      DIP("%sh r%u, [r%u, #%u]\n", isLD ? "ldr" : "str", rD, rN, imm5 * 2);
      goto decode_success;
   }

   case BITS5(0,1,1,1,0):   /* STRB */
   case BITS5(0,1,1,1,1): { /* LDRB */
      /* ------------- LDRB Rd, [Rn, #imm5] ------------- */
      /* ------------- STRB Rd, [Rn, #imm5] ------------- */
      /* LDRB/STRB Rd, [Rn + imm5] */
      UInt    rD   = INSN0(2,0);
      UInt    rN   = INSN0(5,3);
      UInt    imm5 = INSN0(10,6);
      UInt    isLD = INSN0(11,11);

      IRExpr* ea = binop(Iop_Add32, getIRegT(rN), mkU32(imm5));
      put_ITSTATE(old_itstate); // backout
      if (isLD) {
         IRTemp tD = newTemp(Ity_I32);
         loadGuardedLE( tD, ILGop_8Uto32, ea, llGetIReg(rD), condT );
         putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      } else {
         storeGuardedLE( ea, unop(Iop_32to8, getIRegT(rD)), condT );
      }
      put_ITSTATE(new_itstate); // restore

      DIP("%sb r%u, [r%u, #%u]\n", isLD ? "ldr" : "str", rD, rN, imm5);
      goto decode_success;
   }

   case BITS5(1,0,0,1,0):   /* STR */
   case BITS5(1,0,0,1,1): { /* LDR */
      /* ------------- LDR Rd, [SP, #imm8 * 4] ------------- */
      /* ------------- STR Rd, [SP, #imm8 * 4] ------------- */
      /* LDR/STR Rd, [SP + imm8 * 4] */
      UInt rD    = INSN0(10,8);
      UInt imm8  = INSN0(7,0);
      UInt isLD  = INSN0(11,11);

      IRExpr* ea = binop(Iop_Add32, getIRegT(13), mkU32(imm8 * 4));
      put_ITSTATE(old_itstate); // backout
      if (isLD) {
         IRTemp tD = newTemp(Ity_I32);
         loadGuardedLE( tD, ILGop_Ident32, ea, llGetIReg(rD), condT );
         putIRegT(rD, mkexpr(tD), IRTemp_INVALID);
      } else {
         storeGuardedLE(ea, getIRegT(rD), condT);
      }
      put_ITSTATE(new_itstate); // restore

      DIP("%s r%u, [sp, #%u]\n", isLD ? "ldr" : "str", rD, imm8 * 4);
      goto decode_success;
   }

   case BITS5(1,1,0,0,1): {
      /* ------------- LDMIA Rn!, {reglist} ------------- */
      Int i, nRegs = 0;
      UInt rN   = INSN0(10,8);
      UInt list = INSN0(7,0);
      /* Empty lists aren't allowed. */
      if (list != 0) {
         mk_skip_over_T16_if_cond_is_false(condT);
         condT = IRTemp_INVALID;
         put_ITSTATE(old_itstate);
         // now uncond

         IRTemp oldRn = newTemp(Ity_I32);
         IRTemp base  = newTemp(Ity_I32);
         assign(oldRn, getIRegT(rN));
         assign(base, binop(Iop_And32, mkexpr(oldRn), mkU32(~3U)));
         for (i = 0; i < 8; i++) {
            if (0 == (list & (1 << i)))
               continue;
            nRegs++;
            putIRegT(
               i, loadLE(Ity_I32,
                         binop(Iop_Add32, mkexpr(base),
                                          mkU32(nRegs * 4 - 4))),
               IRTemp_INVALID
            );
         }
         /* Only do the writeback for rN if it isn't in the list of
            registers to be transferred. */
         if (0 == (list & (1 << rN))) {
            putIRegT(rN,
                     binop(Iop_Add32, mkexpr(oldRn),
                                      mkU32(nRegs * 4)),
                     IRTemp_INVALID
            );
         }

         /* Reinstate the ITSTATE update. */
         put_ITSTATE(new_itstate);

         DIP("ldmia r%u!, {0x%04x}\n", rN, list);
         goto decode_success;
      }
      break;
   }

   case BITS5(1,1,0,0,0): {
      /* ------------- STMIA Rn!, {reglist} ------------- */
      Int i, nRegs = 0;
      UInt rN   = INSN0(10,8);
      UInt list = INSN0(7,0);
      /* Empty lists aren't allowed.  Also, if rN is in the list then
         it must be the lowest numbered register in the list. */
      Bool valid = list != 0;
      if (valid && 0 != (list & (1 << rN))) {
         for (i = 0; i < rN; i++) {
            if (0 != (list & (1 << i)))
               valid = False;
         }
      }
      if (valid) {
         mk_skip_over_T16_if_cond_is_false(condT);
         condT = IRTemp_INVALID;
         put_ITSTATE(old_itstate);
         // now uncond

         IRTemp oldRn = newTemp(Ity_I32);
         IRTemp base = newTemp(Ity_I32);
         assign(oldRn, getIRegT(rN));
         assign(base, binop(Iop_And32, mkexpr(oldRn), mkU32(~3U)));
         for (i = 0; i < 8; i++) {
            if (0 == (list & (1 << i)))
               continue;
            nRegs++;
            storeLE( binop(Iop_Add32, mkexpr(base), mkU32(nRegs * 4 - 4)),
                     getIRegT(i) );
         }
         /* Always do the writeback. */
         putIRegT(rN,
                  binop(Iop_Add32, mkexpr(oldRn),
                                   mkU32(nRegs * 4)),
                  IRTemp_INVALID);

         /* Reinstate the ITSTATE update. */
         put_ITSTATE(new_itstate);

         DIP("stmia r%u!, {0x%04x}\n", rN, list);
         goto decode_success;
      }
      break;
   }

   case BITS5(0,0,0,0,0):   /* LSLS */
   case BITS5(0,0,0,0,1):   /* LSRS */
   case BITS5(0,0,0,1,0): { /* ASRS */
      /* ---------------- LSLS Rd, Rm, #imm5 ---------------- */
      /* ---------------- LSRS Rd, Rm, #imm5 ---------------- */
      /* ---------------- ASRS Rd, Rm, #imm5 ---------------- */
      UInt   rD   = INSN0(2,0);
      UInt   rM   = INSN0(5,3);
      UInt   imm5 = INSN0(10,6);
      IRTemp res  = newTemp(Ity_I32);
      IRTemp resC = newTemp(Ity_I32);
      IRTemp rMt  = newTemp(Ity_I32);
      IRTemp oldV = newTemp(Ity_I32);
      const HChar* wot  = "???";
      assign(rMt, getIRegT(rM));
      assign(oldV, mk_armg_calculate_flag_v());
      /* Looks like INSN0(12,11) are the standard 'how' encoding.
         Could compactify if the ROR case later appears. */
      switch (INSN0(15,11)) {
         case BITS5(0,0,0,0,0):
            compute_result_and_C_after_LSL_by_imm5(
               dis_buf, &res, &resC, rMt, imm5, rM
            );
            wot = "lsl";
            break;
         case BITS5(0,0,0,0,1):
            compute_result_and_C_after_LSR_by_imm5(
               dis_buf, &res, &resC, rMt, imm5, rM
            );
            wot = "lsr";
            break;
         case BITS5(0,0,0,1,0):
            compute_result_and_C_after_ASR_by_imm5(
               dis_buf, &res, &resC, rMt, imm5, rM
            );
            wot = "asr";
            break;
         default:
            /*NOTREACHED*/vassert(0);
      }
      // not safe to read guest state after this point
      putIRegT(rD, mkexpr(res), condT);
      setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, resC, oldV,
                         cond_AND_notInIT_T );
      /* ignore buf and roll our own output */
      DIP("%ss r%u, r%u, #%u\n", wot, rD, rM, imm5);
      goto decode_success;
   }

   case BITS5(1,1,1,0,0): {
      /* ---------------- B #simm11 ---------------- */
      UInt uimm11 = INSN0(10,0);  uimm11 <<= 21;
      Int  simm11 = (Int)uimm11;  simm11 >>= 20;
      UInt dst    = simm11 + guest_R15_curr_instr_notENC + 4;
      /* Only allowed outside or last-in IT block; SIGILL if not so. */
      gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
      // and skip this insn if not selected; being cleverer is too
      // difficult
      mk_skip_over_T16_if_cond_is_false(condT);
      condT = IRTemp_INVALID;
      // now uncond
      llPutIReg(15, mkU32( dst | 1 /*CPSR.T*/ ));
      dres.jk_StopHere = Ijk_Boring;
      dres.whatNext    = Dis_StopHere;
      DIP("b 0x%x\n", dst);
      goto decode_success;
   }

   default:
      break; /* examine the next shortest prefix */

   }


   /* ================ 16-bit 15:12 cases ================ */

   switch (INSN0(15,12)) {

   case BITS4(1,1,0,1): {
      /* ---------------- Bcond #simm8 ---------------- */
      UInt cond  = INSN0(11,8);
      UInt uimm8 = INSN0(7,0);  uimm8 <<= 24;
      Int  simm8 = (Int)uimm8;  simm8 >>= 23;
      UInt dst   = simm8 + guest_R15_curr_instr_notENC + 4;
      if (cond != ARMCondAL && cond != ARMCondNV) {
         /* Not allowed in an IT block; SIGILL if so. */
         gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);

         IRTemp kondT = newTemp(Ity_I32);
         assign( kondT, mk_armg_calculate_condition(cond) );
         stmt( IRStmt_Exit( unop(Iop_32to1, mkexpr(kondT)),
                            Ijk_Boring,
                            IRConst_U32(dst | 1/*CPSR.T*/),
                            OFFB_R15T ));
         llPutIReg(15, mkU32( (guest_R15_curr_instr_notENC + 2) 
                              | 1 /*CPSR.T*/ ));
         dres.jk_StopHere = Ijk_Boring;
         dres.whatNext    = Dis_StopHere;
         DIP("b%s 0x%x\n", nCC(cond), dst);
         goto decode_success;
      }
      break;
   }

   default:
      break; /* hmm, nothing matched */

   }

   /* ================ 16-bit misc cases ================ */

   switch (INSN0(15,0)) {
      case 0xBF00:
         /* ------ NOP ------ */
         DIP("nop\n");
         goto decode_success;
      case 0xBF10: // YIELD
      case 0xBF20: // WFE
         /* ------ WFE, YIELD ------ */
         /* Both appear to get used as a spin-loop hints.  Do the usual thing,
            which is to continue after yielding. */
         stmt( IRStmt_Exit( unop(Iop_32to1, mkexpr(condT)),
                            Ijk_Yield,
                            IRConst_U32((guest_R15_curr_instr_notENC + 2) 
                                        | 1 /*CPSR.T*/),
                            OFFB_R15T ));
         Bool isWFE = INSN0(15,0) == 0xBF20;
         DIP(isWFE ? "wfe\n" : "yield\n");
         goto decode_success;
      case 0xBF40:
         /* ------ SEV ------ */
         /* Treat this as a no-op.  Any matching WFEs won't really
            cause the host CPU to snooze; they just cause V to try to
            run some other thread for a while.  So there's no point in
            really doing anything for SEV. */
         DIP("sev\n");
         goto decode_success;
      default:
         break; /* fall through */
   }

   /* ----------------------------------------------------------- */
   /* --                                                       -- */
   /* -- Thumb 32-bit integer instructions                     -- */
   /* --                                                       -- */
   /* ----------------------------------------------------------- */

#  define INSN1(_bMax,_bMin)  SLICE_UInt(((UInt)insn1), (_bMax), (_bMin))

   /* second 16 bits of the instruction, if any */
   vassert(insn1 == 0);
   insn1 = getUShortLittleEndianly( guest_instr+2 );

   anOp   = Iop_INVALID; /* paranoia */
   anOpNm = NULL;        /* paranoia */

   /* Change result defaults to suit 32-bit insns. */
   vassert(dres.whatNext   == Dis_Continue);
   vassert(dres.len        == 2);
   vassert(dres.continueAt == 0);
   dres.len = 4;

   /* ---------------- BL/BLX simm26 ---------------- */
   if (BITS5(1,1,1,1,0) == INSN0(15,11) && BITS2(1,1) == INSN1(15,14)) {
      UInt isBL = INSN1(12,12);
      UInt bS   = INSN0(10,10);
      UInt bJ1  = INSN1(13,13);
      UInt bJ2  = INSN1(11,11);
      UInt bI1  = 1 ^ (bJ1 ^ bS);
      UInt bI2  = 1 ^ (bJ2 ^ bS);
      UInt uimm25
         =   (bS          << (1 + 1 + 10 + 11 + 1))
           | (bI1         << (1 + 10 + 11 + 1))
           | (bI2         << (10 + 11 + 1))
           | (INSN0(9,0)  << (11 + 1))
           | (INSN1(10,0) << 1);
      uimm25 <<= 7;
      Int simm25 = (Int)uimm25;
      simm25 >>= 7;

      vassert(0 == (guest_R15_curr_instr_notENC & 1));
      UInt dst = simm25 + guest_R15_curr_instr_notENC + 4;

      /* One further validity case to check: in the case of BLX
         (not-BL), that insn1[0] must be zero. */
      Bool valid = True;
      if (isBL == 0 && INSN1(0,0) == 1) valid = False;
      if (valid) {
         /* Only allowed outside or last-in IT block; SIGILL if not so. */
         gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
         // and skip this insn if not selected; being cleverer is too
         // difficult
         mk_skip_over_T32_if_cond_is_false(condT);
         condT = IRTemp_INVALID;
         // now uncond

         /* We're returning to Thumb code, hence "| 1" */
         putIRegT( 14, mkU32( (guest_R15_curr_instr_notENC + 4) | 1 ),
                   IRTemp_INVALID);
         if (isBL) {
            /* BL: unconditional T -> T call */
            /* we're calling Thumb code, hence "| 1" */
            llPutIReg(15, mkU32( dst | 1 ));
            DIP("bl 0x%x (stay in Thumb mode)\n", dst);
         } else {
            /* BLX: unconditional T -> A call */
            /* we're calling ARM code, hence "& 3" to align to a
               valid ARM insn address */
            llPutIReg(15, mkU32( dst & ~3 ));
            DIP("blx 0x%x (switch to ARM mode)\n", dst & ~3);
         }
         dres.whatNext    = Dis_StopHere;
         dres.jk_StopHere = Ijk_Call;
         goto decode_success;
      }
   }

   /* ---------------- {LD,ST}M{IA,DB} ---------------- */
   if (0x3a2 == INSN0(15,6) // {LD,ST}MIA
       || 0x3a4 == INSN0(15,6)) { // {LD,ST}MDB
      UInt bW      = INSN0(5,5); /* writeback Rn ? */
      UInt bL      = INSN0(4,4);
      UInt rN      = INSN0(3,0);
      UInt bP      = INSN1(15,15); /* reglist entry for r15 */
      UInt bM      = INSN1(14,14); /* reglist entry for r14 */
      UInt rLmost  = INSN1(12,0);  /* reglist entry for r0 .. 12 */
      UInt rL13    = INSN1(13,13); /* must be zero */
      UInt regList = 0;
      Bool valid   = True;

      UInt bINC    = 1;
      UInt bBEFORE = 0;
      if (INSN0(15,6) == 0x3a4) {
         bINC    = 0;
         bBEFORE = 1;
      }

      /* detect statically invalid cases, and construct the final
         reglist */
      if (rL13 == 1)
         valid = False;

      if (bL == 1) {
         regList = (bP << 15) | (bM << 14) | rLmost;
         if (rN == 15)                       valid = False;
         if (popcount32(regList) < 2)        valid = False;
         if (bP == 1 && bM == 1)             valid = False;
         if (bW == 1 && (regList & (1<<rN))) valid = False;
      } else {
         regList = (bM << 14) | rLmost;
         if (bP == 1)                        valid = False;
         if (rN == 15)                       valid = False;
         if (popcount32(regList) < 2)        valid = False;
         if (bW == 1 && (regList & (1<<rN))) valid = False;
      }

      if (valid) {
         if (bL == 1 && bP == 1) {
            // We'll be writing the PC.  Hence:
            /* Only allowed outside or last-in IT block; SIGILL if not so. */
            gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
         }

         /* Go uncond: */
         mk_skip_over_T32_if_cond_is_false(condT);
         condT = IRTemp_INVALID;
         // now uncond

         /* Generate the IR.  This might generate a write to R15. */
         mk_ldm_stm(False/*!arm*/, rN, bINC, bBEFORE, bW, bL, regList);

         if (bL == 1 && (regList & (1<<15))) {
            // If we wrote to R15, we have an interworking return to
            // deal with.
            llPutIReg(15, llGetIReg(15));
            dres.jk_StopHere = Ijk_Ret;
            dres.whatNext    = Dis_StopHere;
         }

         DIP("%sm%c%c r%u%s, {0x%04x}\n",
              bL == 1 ? "ld" : "st", bINC ? 'i' : 'd', bBEFORE ? 'b' : 'a',
              rN, bW ? "!" : "", regList);

         goto decode_success;
      }
   }

   /* -------------- (T3) ADD{S}.W Rd, Rn, #constT -------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && INSN0(9,5) == BITS5(0,1,0,0,0)
       && INSN1(15,15) == 0) {
      UInt bS = INSN0(4,4);
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      Bool valid = !isBadRegT(rN) && !isBadRegT(rD);
      /* but allow "add.w reg, sp, #constT" for reg != PC */ 
      if (!valid && rD <= 14 && rN == 13)
         valid = True;
      if (valid) {
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp res   = newTemp(Ity_I32);
         UInt   imm32 = thumbExpandImm_from_I0_I1(NULL, insn0, insn1);
         assign(argL, getIRegT(rN));
         assign(argR, mkU32(imm32));
         assign(res,  binop(Iop_Add32, mkexpr(argL), mkexpr(argR)));
         putIRegT(rD, mkexpr(res), condT);
         if (bS == 1)
            setFlags_D1_D2( ARMG_CC_OP_ADD, argL, argR, condT );
         DIP("add%s.w r%u, r%u, #%u\n",
             bS == 1 ? "s" : "", rD, rN, imm32);
         goto decode_success;
      }
   }

   /* ---------------- (T4) ADDW Rd, Rn, #uimm12 -------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && INSN0(9,4) == BITS6(1,0,0,0,0,0)
       && INSN1(15,15) == 0) {
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      Bool valid = !isBadRegT(rN) && !isBadRegT(rD);
      /* but allow "addw reg, sp, #uimm12" for reg != PC */
      if (!valid && rD <= 14 && rN == 13)
         valid = True;
      if (valid) {
         IRTemp argL = newTemp(Ity_I32);
         IRTemp argR = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         UInt imm12  = (INSN0(10,10) << 11) | (INSN1(14,12) << 8) | INSN1(7,0);
         assign(argL, getIRegT(rN));
         assign(argR, mkU32(imm12));
         assign(res,  binop(Iop_Add32, mkexpr(argL), mkexpr(argR)));
         putIRegT(rD, mkexpr(res), condT);
         DIP("addw r%u, r%u, #%u\n", rD, rN, imm12);
         goto decode_success;
      }
   }

   /* ---------------- (T2) CMP.W Rn, #constT ---------------- */
   /* ---------------- (T2) CMN.W Rn, #constT ---------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && (   INSN0(9,4) == BITS6(0,1,1,0,1,1)  // CMP
           || INSN0(9,4) == BITS6(0,1,0,0,0,1)) // CMN
       && INSN1(15,15) == 0
       && INSN1(11,8) == BITS4(1,1,1,1)) {
      UInt rN = INSN0(3,0);
      if (rN != 15) {
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         Bool   isCMN = INSN0(9,4) == BITS6(0,1,0,0,0,1);
         UInt   imm32 = thumbExpandImm_from_I0_I1(NULL, insn0, insn1);
         assign(argL, getIRegT(rN));
         assign(argR, mkU32(imm32));
         setFlags_D1_D2( isCMN ? ARMG_CC_OP_ADD : ARMG_CC_OP_SUB,
                         argL, argR, condT );
         DIP("%s.w r%u, #%u\n", isCMN ? "cmn" : "cmp", rN, imm32);
         goto decode_success;
      }
   }

   /* -------------- (T1) TST.W Rn, #constT -------------- */
   /* -------------- (T1) TEQ.W Rn, #constT -------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && (   INSN0(9,4) == BITS6(0,0,0,0,0,1)  // TST
           || INSN0(9,4) == BITS6(0,0,1,0,0,1)) // TEQ
       && INSN1(15,15) == 0
       && INSN1(11,8) == BITS4(1,1,1,1)) {
      UInt rN = INSN0(3,0);
      if (!isBadRegT(rN)) { // yes, really, it's inconsistent with CMP.W
         Bool  isTST  = INSN0(9,4) == BITS6(0,0,0,0,0,1);
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp res   = newTemp(Ity_I32);
         IRTemp oldV  = newTemp(Ity_I32);
         IRTemp oldC  = newTemp(Ity_I32);
         Bool   updC  = False;
         UInt   imm32 = thumbExpandImm_from_I0_I1(&updC, insn0, insn1);
         assign(argL, getIRegT(rN));
         assign(argR, mkU32(imm32));
         assign(res,  binop(isTST ? Iop_And32 : Iop_Xor32,
                            mkexpr(argL), mkexpr(argR)));
         assign( oldV, mk_armg_calculate_flag_v() );
         assign( oldC, updC 
                       ? mkU32((imm32 >> 31) & 1)
                       : mk_armg_calculate_flag_c() );
         setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV, condT );
         DIP("%s.w r%u, #%u\n", isTST ? "tst" : "teq", rN, imm32);
         goto decode_success;
      }
   }

   /* -------------- (T3) SUB{S}.W Rd, Rn, #constT -------------- */
   /* -------------- (T3) RSB{S}.W Rd, Rn, #constT -------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && (INSN0(9,5) == BITS5(0,1,1,0,1) // SUB
           || INSN0(9,5) == BITS5(0,1,1,1,0)) // RSB
       && INSN1(15,15) == 0) {
      Bool isRSB = INSN0(9,5) == BITS5(0,1,1,1,0);
      UInt bS    = INSN0(4,4);
      UInt rN    = INSN0(3,0);
      UInt rD    = INSN1(11,8);
      Bool valid = !isBadRegT(rN) && !isBadRegT(rD);
      /* but allow "sub{s}.w reg, sp, #constT 
         this is (T2) of "SUB (SP minus immediate)" */
      if (!valid && !isRSB && rN == 13 && rD != 15)
         valid = True;
      if (valid) {
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp res   = newTemp(Ity_I32);
         UInt   imm32 = thumbExpandImm_from_I0_I1(NULL, insn0, insn1);
         assign(argL, getIRegT(rN));
         assign(argR, mkU32(imm32));
         assign(res,  isRSB
                      ? binop(Iop_Sub32, mkexpr(argR), mkexpr(argL))
                      : binop(Iop_Sub32, mkexpr(argL), mkexpr(argR)));
         putIRegT(rD, mkexpr(res), condT);
         if (bS == 1) {
            if (isRSB)
               setFlags_D1_D2( ARMG_CC_OP_SUB, argR, argL, condT );
            else
               setFlags_D1_D2( ARMG_CC_OP_SUB, argL, argR, condT );
         }
         DIP("%s%s.w r%u, r%u, #%u\n",
             isRSB ? "rsb" : "sub", bS == 1 ? "s" : "", rD, rN, imm32);
         goto decode_success;
      }
   }

   /* -------------- (T4) SUBW Rd, Rn, #uimm12 ------------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && INSN0(9,4) == BITS6(1,0,1,0,1,0)
       && INSN1(15,15) == 0) {
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      Bool valid = !isBadRegT(rN) && !isBadRegT(rD);
      /* but allow "subw sp, sp, #uimm12" */
      if (!valid && rD == 13 && rN == 13)
         valid = True;
      if (valid) {
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp res   = newTemp(Ity_I32);
         UInt imm12   = (INSN0(10,10) << 11) | (INSN1(14,12) << 8) | INSN1(7,0);
         assign(argL, getIRegT(rN));
         assign(argR, mkU32(imm12));
         assign(res,  binop(Iop_Sub32, mkexpr(argL), mkexpr(argR)));
         putIRegT(rD, mkexpr(res), condT);
         DIP("subw r%u, r%u, #%u\n", rD, rN, imm12);
         goto decode_success;
      }
   }

   /* -------------- (T1) ADC{S}.W Rd, Rn, #constT -------------- */
   /* -------------- (T1) SBC{S}.W Rd, Rn, #constT -------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && (   INSN0(9,5) == BITS5(0,1,0,1,0)  // ADC
           || INSN0(9,5) == BITS5(0,1,0,1,1)) // SBC
       && INSN1(15,15) == 0) {
      /* ADC:  Rd = Rn + constT + oldC */
      /* SBC:  Rd = Rn - constT - (oldC ^ 1) */
      UInt bS    = INSN0(4,4);
      UInt rN    = INSN0(3,0);
      UInt rD    = INSN1(11,8);
      if (!isBadRegT(rN) && !isBadRegT(rD)) {
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp res   = newTemp(Ity_I32);
         IRTemp oldC  = newTemp(Ity_I32);
         UInt   imm32 = thumbExpandImm_from_I0_I1(NULL, insn0, insn1);
         assign(argL, getIRegT(rN));
         assign(argR, mkU32(imm32));
         assign(oldC, mk_armg_calculate_flag_c() );
         const HChar* nm  = "???";
         switch (INSN0(9,5)) {
            case BITS5(0,1,0,1,0): // ADC
               nm = "adc";
               assign(res,
                      binop(Iop_Add32,
                            binop(Iop_Add32, mkexpr(argL), mkexpr(argR)),
                            mkexpr(oldC) ));
               putIRegT(rD, mkexpr(res), condT);
               if (bS)
                  setFlags_D1_D2_ND( ARMG_CC_OP_ADC,
                                     argL, argR, oldC, condT );
               break;
            case BITS5(0,1,0,1,1): // SBC
               nm = "sbc";
               assign(res,
                      binop(Iop_Sub32,
                            binop(Iop_Sub32, mkexpr(argL), mkexpr(argR)),
                            binop(Iop_Xor32, mkexpr(oldC), mkU32(1)) ));
               putIRegT(rD, mkexpr(res), condT);
               if (bS)
                  setFlags_D1_D2_ND( ARMG_CC_OP_SBB,
                                     argL, argR, oldC, condT );
               break;
            default:
              vassert(0);
         }
         DIP("%s%s.w r%u, r%u, #%u\n",
             nm, bS == 1 ? "s" : "", rD, rN, imm32);
         goto decode_success;
      }
   }

   /* -------------- (T1) ORR{S}.W Rd, Rn, #constT -------------- */
   /* -------------- (T1) AND{S}.W Rd, Rn, #constT -------------- */
   /* -------------- (T1) BIC{S}.W Rd, Rn, #constT -------------- */
   /* -------------- (T1) EOR{S}.W Rd, Rn, #constT -------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && (   INSN0(9,5) == BITS5(0,0,0,1,0)  // ORR
           || INSN0(9,5) == BITS5(0,0,0,0,0)  // AND
           || INSN0(9,5) == BITS5(0,0,0,0,1)  // BIC
           || INSN0(9,5) == BITS5(0,0,1,0,0)  // EOR
           || INSN0(9,5) == BITS5(0,0,0,1,1)) // ORN
       && INSN1(15,15) == 0) {
      UInt bS = INSN0(4,4);
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      if (!isBadRegT(rN) && !isBadRegT(rD)) {
         Bool   notArgR = False;
         IROp   op      = Iop_INVALID;
         const HChar* nm = "???";
         switch (INSN0(9,5)) {
            case BITS5(0,0,0,1,0): op = Iop_Or32;  nm = "orr"; break;
            case BITS5(0,0,0,0,0): op = Iop_And32; nm = "and"; break;
            case BITS5(0,0,0,0,1): op = Iop_And32; nm = "bic";
                                   notArgR = True; break;
            case BITS5(0,0,1,0,0): op = Iop_Xor32; nm = "eor"; break;
            case BITS5(0,0,0,1,1): op = Iop_Or32;  nm = "orn";
                                   notArgR = True; break;
            default: vassert(0);
         }
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp res   = newTemp(Ity_I32);
         Bool   updC  = False;
         UInt   imm32 = thumbExpandImm_from_I0_I1(&updC, insn0, insn1);
         assign(argL, getIRegT(rN));
         assign(argR, mkU32(notArgR ? ~imm32 : imm32));
         assign(res,  binop(op, mkexpr(argL), mkexpr(argR)));
         putIRegT(rD, mkexpr(res), condT);
         if (bS) {
            IRTemp oldV = newTemp(Ity_I32);
            IRTemp oldC = newTemp(Ity_I32);
            assign( oldV, mk_armg_calculate_flag_v() );
            assign( oldC, updC 
                          ? mkU32((imm32 >> 31) & 1)
                          : mk_armg_calculate_flag_c() );
            setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                               condT );
         }
         DIP("%s%s.w r%u, r%u, #%u\n",
             nm, bS == 1 ? "s" : "", rD, rN, imm32);
         goto decode_success;
      }
   }

   /* ---------- (T3) ADD{S}.W Rd, Rn, Rm, {shift} ---------- */
   /* ---------- (T3) SUB{S}.W Rd, Rn, Rm, {shift} ---------- */
   /* ---------- (T3) RSB{S}.W Rd, Rn, Rm, {shift} ---------- */
   if (INSN0(15,9) == BITS7(1,1,1,0,1,0,1)
       && (   INSN0(8,5) == BITS4(1,0,0,0)  // add subopc
           || INSN0(8,5) == BITS4(1,1,0,1)  // sub subopc
           || INSN0(8,5) == BITS4(1,1,1,0)) // rsb subopc
       && INSN1(15,15) == 0) {
      UInt rN   = INSN0(3,0);
      UInt rD   = INSN1(11,8);
      UInt rM   = INSN1(3,0);
      UInt bS   = INSN0(4,4);
      UInt imm5 = (INSN1(14,12) << 2) | INSN1(7,6);
      UInt how  = INSN1(5,4);

      Bool valid = !isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM);
      /* but allow "add.w reg, sp, reg, lsl #N for N=0..31
         (T3) "ADD (SP plus register) */
      if (!valid && INSN0(8,5) == BITS4(1,0,0,0) // add
          && rD != 15 && rN == 13 && imm5 <= 31 && how == 0) {
         valid = True;
      }
      /* also allow "sub.w reg, sp, reg   w/ no shift
         (T1) "SUB (SP minus register) */
      if (!valid && INSN0(8,5) == BITS4(1,1,0,1) // sub
          && rD != 15 && rN == 13 && imm5 == 0 && how == 0) {
         valid = True;
      }
      if (valid) {
         Bool   swap = False;
         IROp   op   = Iop_INVALID;
         const HChar* nm = "???";
         switch (INSN0(8,5)) {
            case BITS4(1,0,0,0): op = Iop_Add32; nm = "add"; break;
            case BITS4(1,1,0,1): op = Iop_Sub32; nm = "sub"; break;
            case BITS4(1,1,1,0): op = Iop_Sub32; nm = "rsb"; 
                                 swap = True; break;
            default: vassert(0);
         }

         IRTemp argL = newTemp(Ity_I32);
         assign(argL, getIRegT(rN));

         IRTemp rMt = newTemp(Ity_I32);
         assign(rMt, getIRegT(rM));

         IRTemp argR = newTemp(Ity_I32);
         compute_result_and_C_after_shift_by_imm5(
            dis_buf, &argR, NULL, rMt, how, imm5, rM
         );

         IRTemp res = newTemp(Ity_I32);
         assign(res, swap 
                     ? binop(op, mkexpr(argR), mkexpr(argL))
                     : binop(op, mkexpr(argL), mkexpr(argR)));

         putIRegT(rD, mkexpr(res), condT);
         if (bS) {
            switch (op) {
               case Iop_Add32:
                  setFlags_D1_D2( ARMG_CC_OP_ADD, argL, argR, condT );
                  break;
               case Iop_Sub32:
                  if (swap)
                     setFlags_D1_D2( ARMG_CC_OP_SUB, argR, argL, condT );
                  else
                     setFlags_D1_D2( ARMG_CC_OP_SUB, argL, argR, condT );
                  break;
               default:
                  vassert(0);
            }
         }

         DIP("%s%s.w r%u, r%u, %s\n",
             nm, bS ? "s" : "", rD, rN, dis_buf);
         goto decode_success;
      }
   }

   /* ---------- (T3) ADC{S}.W Rd, Rn, Rm, {shift} ---------- */
   /* ---------- (T2) SBC{S}.W Rd, Rn, Rm, {shift} ---------- */
   if (INSN0(15,9) == BITS7(1,1,1,0,1,0,1)
       && (   INSN0(8,5) == BITS4(1,0,1,0)   // adc subopc
           || INSN0(8,5) == BITS4(1,0,1,1))  // sbc subopc
       && INSN1(15,15) == 0) {
      /* ADC:  Rd = Rn + shifter_operand + oldC */
      /* SBC:  Rd = Rn - shifter_operand - (oldC ^ 1) */
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM)) {
         UInt bS   = INSN0(4,4);
         UInt imm5 = (INSN1(14,12) << 2) | INSN1(7,6);
         UInt how  = INSN1(5,4);

         IRTemp argL = newTemp(Ity_I32);
         assign(argL, getIRegT(rN));

         IRTemp rMt = newTemp(Ity_I32);
         assign(rMt, getIRegT(rM));

         IRTemp oldC = newTemp(Ity_I32);
         assign(oldC, mk_armg_calculate_flag_c());

         IRTemp argR = newTemp(Ity_I32);
         compute_result_and_C_after_shift_by_imm5(
            dis_buf, &argR, NULL, rMt, how, imm5, rM
         );

         const HChar* nm  = "???";
         IRTemp res = newTemp(Ity_I32);
         switch (INSN0(8,5)) {
            case BITS4(1,0,1,0): // ADC
               nm = "adc";
               assign(res,
                      binop(Iop_Add32,
                            binop(Iop_Add32, mkexpr(argL), mkexpr(argR)),
                            mkexpr(oldC) ));
               putIRegT(rD, mkexpr(res), condT);
               if (bS)
                  setFlags_D1_D2_ND( ARMG_CC_OP_ADC,
                                     argL, argR, oldC, condT );
               break;
            case BITS4(1,0,1,1): // SBC
               nm = "sbc";
               assign(res,
                      binop(Iop_Sub32,
                            binop(Iop_Sub32, mkexpr(argL), mkexpr(argR)),
                            binop(Iop_Xor32, mkexpr(oldC), mkU32(1)) ));
               putIRegT(rD, mkexpr(res), condT);
               if (bS)
                  setFlags_D1_D2_ND( ARMG_CC_OP_SBB,
                                     argL, argR, oldC, condT );
               break;
            default:
               vassert(0);
         }

         DIP("%s%s.w r%u, r%u, %s\n",
             nm, bS ? "s" : "", rD, rN, dis_buf);
         goto decode_success;
      }
   }

   /* ---------- (T3) AND{S}.W Rd, Rn, Rm, {shift} ---------- */
   /* ---------- (T3) ORR{S}.W Rd, Rn, Rm, {shift} ---------- */
   /* ---------- (T3) EOR{S}.W Rd, Rn, Rm, {shift} ---------- */
   /* ---------- (T3) BIC{S}.W Rd, Rn, Rm, {shift} ---------- */
   /* ---------- (T1) ORN{S}.W Rd, Rn, Rm, {shift} ---------- */
   if (INSN0(15,9) == BITS7(1,1,1,0,1,0,1)
       && (   INSN0(8,5) == BITS4(0,0,0,0)  // and subopc
           || INSN0(8,5) == BITS4(0,0,1,0)  // orr subopc
           || INSN0(8,5) == BITS4(0,1,0,0)  // eor subopc
           || INSN0(8,5) == BITS4(0,0,0,1)  // bic subopc
           || INSN0(8,5) == BITS4(0,0,1,1)) // orn subopc
       && INSN1(15,15) == 0) {
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM)) {
         Bool notArgR = False;
         IROp op      = Iop_INVALID;
         const HChar* nm  = "???";
         switch (INSN0(8,5)) {
            case BITS4(0,0,0,0): op = Iop_And32; nm = "and"; break;
            case BITS4(0,0,1,0): op = Iop_Or32;  nm = "orr"; break;
            case BITS4(0,1,0,0): op = Iop_Xor32; nm = "eor"; break;
            case BITS4(0,0,0,1): op = Iop_And32; nm = "bic";
                                 notArgR = True; break;
            case BITS4(0,0,1,1): op = Iop_Or32; nm = "orn";
                                 notArgR = True; break;
            default: vassert(0);
         }
         UInt bS   = INSN0(4,4);
         UInt imm5 = (INSN1(14,12) << 2) | INSN1(7,6);
         UInt how  = INSN1(5,4);

         IRTemp rNt = newTemp(Ity_I32);
         assign(rNt, getIRegT(rN));

         IRTemp rMt = newTemp(Ity_I32);
         assign(rMt, getIRegT(rM));

         IRTemp argR = newTemp(Ity_I32);
         IRTemp oldC = bS ? newTemp(Ity_I32) : IRTemp_INVALID;

         compute_result_and_C_after_shift_by_imm5(
            dis_buf, &argR, bS ? &oldC : NULL, rMt, how, imm5, rM
         );

         IRTemp res = newTemp(Ity_I32);
         if (notArgR) {
            vassert(op == Iop_And32 || op == Iop_Or32);
            assign(res, binop(op, mkexpr(rNt),
                                  unop(Iop_Not32, mkexpr(argR))));
         } else {
            assign(res, binop(op, mkexpr(rNt), mkexpr(argR)));
         }

         putIRegT(rD, mkexpr(res), condT);
         if (bS) {
            IRTemp oldV = newTemp(Ity_I32);
            assign( oldV, mk_armg_calculate_flag_v() );
            setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                               condT );
         }

         DIP("%s%s.w r%u, r%u, %s\n",
             nm, bS ? "s" : "", rD, rN, dis_buf);
         goto decode_success;
      }
   }

   /* -------------- (T?) LSL{S}.W Rd, Rn, Rm -------------- */
   /* -------------- (T?) LSR{S}.W Rd, Rn, Rm -------------- */
   /* -------------- (T?) ASR{S}.W Rd, Rn, Rm -------------- */
   /* -------------- (T?) ROR{S}.W Rd, Rn, Rm -------------- */
   if (INSN0(15,7) == BITS9(1,1,1,1,1,0,1,0,0)
       && INSN1(15,12) == BITS4(1,1,1,1)
       && INSN1(7,4) == BITS4(0,0,0,0)) {
      UInt how = INSN0(6,5); // standard encoding
      UInt rN  = INSN0(3,0);
      UInt rD  = INSN1(11,8);
      UInt rM  = INSN1(3,0);
      UInt bS  = INSN0(4,4);
      Bool valid = !isBadRegT(rN) && !isBadRegT(rM) && !isBadRegT(rD);
      if (valid) {
         IRTemp rNt    = newTemp(Ity_I32);
         IRTemp rMt    = newTemp(Ity_I32);
         IRTemp res    = newTemp(Ity_I32);
         IRTemp oldC   = bS ? newTemp(Ity_I32) : IRTemp_INVALID;
         IRTemp oldV   = bS ? newTemp(Ity_I32) : IRTemp_INVALID;
         const HChar* nms[4] = { "lsl", "lsr", "asr", "ror" };
         const HChar* nm     = nms[how];
         assign(rNt, getIRegT(rN));
         assign(rMt, getIRegT(rM));
         compute_result_and_C_after_shift_by_reg(
            dis_buf, &res, bS ? &oldC : NULL,
            rNt, how, rMt, rN, rM
         );
         if (bS)
            assign(oldV, mk_armg_calculate_flag_v());
         putIRegT(rD, mkexpr(res), condT);
         if (bS) {
            setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                               condT );
         }
         DIP("%s%s.w r%u, r%u, r%u\n",
             nm, bS ? "s" : "", rD, rN, rM);
         goto decode_success;
      }
   }

   /* ------------ (T?) MOV{S}.W Rd, Rn, {shift} ------------ */
   /* ------------ (T?) MVN{S}.W Rd, Rn, {shift} ------------ */
   if ((INSN0(15,0) & 0xFFCF) == 0xEA4F
       && INSN1(15,15) == 0) {
      UInt rD      = INSN1(11,8);
      UInt rN      = INSN1(3,0);
      UInt bS      = INSN0(4,4);
      UInt isMVN   = INSN0(5,5);
      Bool regsOK  = (bS || isMVN) 
                        ? (!isBadRegT(rD) && !isBadRegT(rN))
                        : (rD != 15 && rN != 15 && (rD != 13 || rN != 13));
      if (regsOK) {
         UInt imm5  = (INSN1(14,12) << 2) | INSN1(7,6);
         UInt how   = INSN1(5,4);

         IRTemp rNt = newTemp(Ity_I32);
         assign(rNt, getIRegT(rN));

         IRTemp oldRn = newTemp(Ity_I32);
         IRTemp oldC  = bS ? newTemp(Ity_I32) : IRTemp_INVALID;
         compute_result_and_C_after_shift_by_imm5(
            dis_buf, &oldRn, bS ? &oldC : NULL, rNt, how, imm5, rN
         );

         IRTemp res = newTemp(Ity_I32);
         assign(res, isMVN ? unop(Iop_Not32, mkexpr(oldRn))
                           : mkexpr(oldRn));

         putIRegT(rD, mkexpr(res), condT);
         if (bS) {
            IRTemp oldV = newTemp(Ity_I32);
            assign( oldV, mk_armg_calculate_flag_v() );
            setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV, condT);
         }
         DIP("%s%s.w r%u, %s\n",
             isMVN ? "mvn" : "mov", bS ? "s" : "", rD, dis_buf);
         goto decode_success;
      }
   }

   /* -------------- (T?) TST.W Rn, Rm, {shift} -------------- */
   /* -------------- (T?) TEQ.W Rn, Rm, {shift} -------------- */
   if (INSN0(15,9) == BITS7(1,1,1,0,1,0,1)
       && (   INSN0(8,4) == BITS5(0,0,0,0,1)  // TST
           || INSN0(8,4) == BITS5(0,1,0,0,1)) // TEQ
       && INSN1(15,15) == 0
       && INSN1(11,8) == BITS4(1,1,1,1)) {
      UInt rN = INSN0(3,0);
      UInt rM = INSN1(3,0);
      if (!isBadRegT(rN) && !isBadRegT(rM)) {
         Bool isTST = INSN0(8,4) == BITS5(0,0,0,0,1);

         UInt how  = INSN1(5,4);
         UInt imm5 = (INSN1(14,12) << 2) | INSN1(7,6);

         IRTemp argL = newTemp(Ity_I32);
         assign(argL, getIRegT(rN));

         IRTemp rMt = newTemp(Ity_I32);
         assign(rMt, getIRegT(rM));

         IRTemp argR = newTemp(Ity_I32);
         IRTemp oldC = newTemp(Ity_I32);
         compute_result_and_C_after_shift_by_imm5(
            dis_buf, &argR, &oldC, rMt, how, imm5, rM
         );

         IRTemp oldV = newTemp(Ity_I32);
         assign( oldV, mk_armg_calculate_flag_v() );

         IRTemp res = newTemp(Ity_I32);
         assign(res, binop(isTST ? Iop_And32 : Iop_Xor32,
                           mkexpr(argL), mkexpr(argR)));

         setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                            condT );
         DIP("%s.w r%u, %s\n", isTST ? "tst" : "teq", rN, dis_buf);
         goto decode_success;
      }
   }

   /* -------------- (T3) CMP.W Rn, Rm, {shift} -------------- */
   /* -------------- (T2) CMN.W Rn, Rm, {shift} -------------- */
   if (INSN0(15,9) == BITS7(1,1,1,0,1,0,1)
       && (   INSN0(8,4) == BITS5(1,1,0,1,1)  // CMP
           || INSN0(8,4) == BITS5(1,0,0,0,1)) // CMN
       && INSN1(15,15) == 0
       && INSN1(11,8) == BITS4(1,1,1,1)) {
      UInt rN = INSN0(3,0);
      UInt rM = INSN1(3,0);
      if (!isBadRegT(rN) && !isBadRegT(rM)) {
         Bool isCMN = INSN0(8,4) == BITS5(1,0,0,0,1);
         UInt how   = INSN1(5,4);
         UInt imm5  = (INSN1(14,12) << 2) | INSN1(7,6);

         IRTemp argL = newTemp(Ity_I32);
         assign(argL, getIRegT(rN));

         IRTemp rMt = newTemp(Ity_I32);
         assign(rMt, getIRegT(rM));

         IRTemp argR = newTemp(Ity_I32);
         compute_result_and_C_after_shift_by_imm5(
            dis_buf, &argR, NULL, rMt, how, imm5, rM
         );

         setFlags_D1_D2( isCMN ? ARMG_CC_OP_ADD : ARMG_CC_OP_SUB,
                         argL, argR, condT );

         DIP("%s.w r%u, %s\n", isCMN ? "cmn" : "cmp", rN, dis_buf);
         goto decode_success;
      }
   }

   /* -------------- (T2) MOV{S}.W Rd, #constT -------------- */
   /* -------------- (T2) MVN{S}.W Rd, #constT -------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && (   INSN0(9,5) == BITS5(0,0,0,1,0)  // MOV
           || INSN0(9,5) == BITS5(0,0,0,1,1)) // MVN
       && INSN0(3,0) == BITS4(1,1,1,1)
       && INSN1(15,15) == 0) {
      UInt rD = INSN1(11,8);
      if (!isBadRegT(rD)) {
         Bool   updC  = False;
         UInt   bS    = INSN0(4,4);
         Bool   isMVN = INSN0(5,5) == 1;
         UInt   imm32 = thumbExpandImm_from_I0_I1(&updC, insn0, insn1);
         IRTemp res   = newTemp(Ity_I32);
         assign(res, mkU32(isMVN ? ~imm32 : imm32));
         putIRegT(rD, mkexpr(res), condT);
         if (bS) {
            IRTemp oldV = newTemp(Ity_I32);
            IRTemp oldC = newTemp(Ity_I32);
            assign( oldV, mk_armg_calculate_flag_v() );
            assign( oldC, updC 
                          ? mkU32((imm32 >> 31) & 1)
                          : mk_armg_calculate_flag_c() );
            setFlags_D1_D2_ND( ARMG_CC_OP_LOGIC, res, oldC, oldV,
                               condT );
         }
         DIP("%s%s.w r%u, #%u\n",
             isMVN ? "mvn" : "mov", bS ? "s" : "", rD, imm32);
         goto decode_success;
      }
   }

   /* -------------- (T3) MOVW Rd, #imm16 -------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && INSN0(9,4) == BITS6(1,0,0,1,0,0)
       && INSN1(15,15) == 0) {
      UInt rD = INSN1(11,8);
      if (!isBadRegT(rD)) {
         UInt imm16 = (INSN0(3,0) << 12) | (INSN0(10,10) << 11)
                      | (INSN1(14,12) << 8) | INSN1(7,0);
         putIRegT(rD, mkU32(imm16), condT);
         DIP("movw r%u, #%u\n", rD, imm16);
         goto decode_success;
      }
   }

   /* ---------------- MOVT Rd, #imm16 ---------------- */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && INSN0(9,4) == BITS6(1,0,1,1,0,0)
       && INSN1(15,15) == 0) {
      UInt rD = INSN1(11,8);
      if (!isBadRegT(rD)) {
         UInt imm16 = (INSN0(3,0) << 12) | (INSN0(10,10) << 11)
                      | (INSN1(14,12) << 8) | INSN1(7,0);
         IRTemp res = newTemp(Ity_I32);
         assign(res,
                binop(Iop_Or32,
                      binop(Iop_And32, getIRegT(rD), mkU32(0xFFFF)),
                      mkU32(imm16 << 16)));
         putIRegT(rD, mkexpr(res), condT);
         DIP("movt r%u, #%u\n", rD, imm16);
         goto decode_success;
      }
   }

   /* ---------------- LD/ST reg+/-#imm8 ---------------- */
   /* Loads and stores of the form:
         op  Rt, [Rn, #-imm8]      or
         op  Rt, [Rn], #+/-imm8    or
         op  Rt, [Rn, #+/-imm8]!  
      where op is one of
         ldrb ldrh ldr  ldrsb ldrsh
         strb strh str
   */
   if (INSN0(15,9) == BITS7(1,1,1,1,1,0,0) && INSN1(11,11) == 1) {
      Bool   valid  = True;
      Bool   syned  = False;
      Bool   isST   = False;
      IRType ty     = Ity_I8;
      const HChar* nm = "???";

      switch (INSN0(8,4)) {
         case BITS5(0,0,0,0,0):   // strb
            nm = "strb"; isST = True; break;
         case BITS5(0,0,0,0,1):   // ldrb
            nm = "ldrb"; break;
         case BITS5(1,0,0,0,1):   // ldrsb
            nm = "ldrsb"; syned = True; break;
         case BITS5(0,0,0,1,0):   // strh
            nm = "strh"; ty = Ity_I16; isST = True; break;
         case BITS5(0,0,0,1,1):   // ldrh
            nm = "ldrh"; ty = Ity_I16; break;
         case BITS5(1,0,0,1,1):   // ldrsh
            nm = "ldrsh"; ty = Ity_I16; syned = True; break;
         case BITS5(0,0,1,0,0):   // str
            nm = "str"; ty = Ity_I32; isST = True; break;
         case BITS5(0,0,1,0,1):
            nm = "ldr"; ty = Ity_I32; break;  // ldr
         default:
            valid = False; break;
      }

      UInt rN      = INSN0(3,0);
      UInt rT      = INSN1(15,12);
      UInt bP      = INSN1(10,10);
      UInt bU      = INSN1(9,9);
      UInt bW      = INSN1(8,8);
      UInt imm8    = INSN1(7,0);
      Bool loadsPC = False;

      if (valid) {
         if (bP == 1 && bU == 1 && bW == 0)
            valid = False;
         if (bP == 0 && bW == 0)
            valid = False;
         if (rN == 15)
            valid = False;
         if (bW == 1 && rN == rT)
            valid = False;
         if (ty == Ity_I8 || ty == Ity_I16) {
            if (isBadRegT(rT))
               valid = False;
         } else {
            /* ty == Ity_I32 */
            if (isST && rT == 15)
               valid = False;
            if (!isST && rT == 15)
               loadsPC = True;
         }
      }

      if (valid) {
         // if it's a branch, it can't happen in the middle of an IT block
         // Also, if it is a branch, make it unconditional at this point.
         // Doing conditional branches in-line is too complex (for now)
         if (loadsPC) {
            gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
            // go uncond
            mk_skip_over_T32_if_cond_is_false(condT);
            condT = IRTemp_INVALID;
            // now uncond
         }

         IRTemp preAddr = newTemp(Ity_I32);
         assign(preAddr, getIRegT(rN));

         IRTemp postAddr = newTemp(Ity_I32);
         assign(postAddr, binop(bU == 1 ? Iop_Add32 : Iop_Sub32,
                                mkexpr(preAddr), mkU32(imm8)));

         IRTemp transAddr = bP == 1 ? postAddr : preAddr;

         if (isST) {

            /* Store.  If necessary, update the base register before
               the store itself, so that the common idiom of "str rX,
               [sp, #-4]!" (store rX at sp-4, then do new sp = sp-4,
               a.k.a "push rX") doesn't cause Memcheck to complain
               that the access is below the stack pointer.  Also, not
               updating sp before the store confuses Valgrind's
               dynamic stack-extending logic.  So do it before the
               store.  Hence we need to snarf the store data before
               doing the basereg update. */

            /* get hold of the data to be stored */
            IRTemp oldRt = newTemp(Ity_I32);
            assign(oldRt, getIRegT(rT));

            /* Update Rn if necessary. */
            if (bW == 1) {
               vassert(rN != rT); // assured by validity check above
               putIRegT(rN, mkexpr(postAddr), condT);
            }

            /* generate the transfer */
            IRExpr* data = NULL;
            switch (ty) {
               case Ity_I8:
                  data = unop(Iop_32to8, mkexpr(oldRt));
                  break;
               case Ity_I16:
                  data = unop(Iop_32to16, mkexpr(oldRt));
                  break;
               case Ity_I32:
                  data = mkexpr(oldRt);
                  break;
               default:
                  vassert(0);
            }
            storeGuardedLE(mkexpr(transAddr), data, condT);

         } else {

            /* Load. */
            IRTemp llOldRt = newTemp(Ity_I32);
            assign(llOldRt, llGetIReg(rT));

            /* generate the transfer */
            IRTemp    newRt = newTemp(Ity_I32);
            IRLoadGOp widen = ILGop_INVALID;
            switch (ty) {
               case Ity_I8:
                  widen = syned ? ILGop_8Sto32 : ILGop_8Uto32; break;
               case Ity_I16:
                  widen = syned ? ILGop_16Sto32 : ILGop_16Uto32; break;
               case Ity_I32:
                  widen = ILGop_Ident32; break;
               default:
                  vassert(0);
            }
            loadGuardedLE(newRt, widen,
                          mkexpr(transAddr), mkexpr(llOldRt), condT);
            if (rT == 15) {
               vassert(loadsPC);
               /* We'll do the write to the PC just below */
            } else {
               vassert(!loadsPC);
               /* IRTemp_INVALID is OK here because in the case where
                  condT is false at run time, we're just putting the
                  old rT value back. */
               putIRegT(rT, mkexpr(newRt), IRTemp_INVALID);
            }

            /* Update Rn if necessary. */
            if (bW == 1) {
               vassert(rN != rT); // assured by validity check above
               putIRegT(rN, mkexpr(postAddr), condT);
            }

            if (loadsPC) {
               /* Presumably this is an interworking branch. */
               vassert(rN != 15); // assured by validity check above
               vassert(rT == 15);
               vassert(condT == IRTemp_INVALID); /* due to check above */
               llPutIReg(15, mkexpr(newRt));
               dres.jk_StopHere = Ijk_Boring;  /* or _Ret ? */
               dres.whatNext    = Dis_StopHere;
            }
         }

         if (bP == 1 && bW == 0) {
            DIP("%s.w r%u, [r%u, #%c%u]\n",
                nm, rT, rN, bU ? '+' : '-', imm8);
         }
         else if (bP == 1 && bW == 1) {
            DIP("%s.w r%u, [r%u, #%c%u]!\n",
                nm, rT, rN, bU ? '+' : '-', imm8);
         }
         else {
            vassert(bP == 0 && bW == 1);
            DIP("%s.w r%u, [r%u], #%c%u\n",
                nm, rT, rN, bU ? '+' : '-', imm8);
         }

         goto decode_success;
      }
   }

   /* ------------- LD/ST reg+(reg<<imm2) ------------- */
   /* Loads and stores of the form:
         op  Rt, [Rn, Rm, LSL #imm8]
      where op is one of
         ldrb ldrh ldr  ldrsb ldrsh
         strb strh str
   */
   if (INSN0(15,9) == BITS7(1,1,1,1,1,0,0)
       && INSN1(11,6) == BITS6(0,0,0,0,0,0)) {
      Bool   valid  = True;
      Bool   syned  = False;
      Bool   isST   = False;
      IRType ty     = Ity_I8;
      const HChar* nm = "???";

      switch (INSN0(8,4)) {
         case BITS5(0,0,0,0,0):   // strb
            nm = "strb"; isST = True; break;
         case BITS5(0,0,0,0,1):   // ldrb
            nm = "ldrb"; break;
         case BITS5(1,0,0,0,1):   // ldrsb
            nm = "ldrsb"; syned = True; break;
         case BITS5(0,0,0,1,0):   // strh
            nm = "strh"; ty = Ity_I16; isST = True; break;
         case BITS5(0,0,0,1,1):   // ldrh
            nm = "ldrh"; ty = Ity_I16; break;
         case BITS5(1,0,0,1,1):   // ldrsh
            nm = "ldrsh"; ty = Ity_I16; syned = True; break;
         case BITS5(0,0,1,0,0):   // str
            nm = "str"; ty = Ity_I32; isST = True; break;
         case BITS5(0,0,1,0,1):
            nm = "ldr"; ty = Ity_I32; break;  // ldr
         default:
            valid = False; break;
      }

      UInt rN      = INSN0(3,0);
      UInt rM      = INSN1(3,0);
      UInt rT      = INSN1(15,12);
      UInt imm2    = INSN1(5,4);
      Bool loadsPC = False;

      if (ty == Ity_I8 || ty == Ity_I16) {
         /* all 8- and 16-bit load and store cases have the
            same exclusion set. */
         if (rN == 15 || isBadRegT(rT) || isBadRegT(rM))
            valid = False;
      } else {
         vassert(ty == Ity_I32);
         if (rN == 15 || isBadRegT(rM))
            valid = False;
         if (isST && rT == 15)
            valid = False;
         /* If it is a load and rT is 15, that's only allowable if we
            not in an IT block, or are the last in it.  Need to insert
            a dynamic check for that. */
         if (!isST && rT == 15)
            loadsPC = True;
      }

      if (valid) {
         // if it's a branch, it can't happen in the middle of an IT block
         // Also, if it is a branch, make it unconditional at this point.
         // Doing conditional branches in-line is too complex (for now)
         if (loadsPC) {
            gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
            // go uncond
            mk_skip_over_T32_if_cond_is_false(condT);
            condT = IRTemp_INVALID;
            // now uncond
         }

         IRTemp transAddr = newTemp(Ity_I32);
         assign(transAddr,
                binop( Iop_Add32,
                       getIRegT(rN),
                       binop(Iop_Shl32, getIRegT(rM), mkU8(imm2)) ));

         if (isST) {

            /* get hold of the data to be stored */
            IRTemp oldRt = newTemp(Ity_I32);
            assign(oldRt, getIRegT(rT));

            /* generate the transfer */
            IRExpr* data = NULL;
            switch (ty) {
               case Ity_I8:
                  data = unop(Iop_32to8, mkexpr(oldRt));
                  break;
               case Ity_I16:
                  data = unop(Iop_32to16, mkexpr(oldRt));
                  break;
              case Ity_I32:
                  data = mkexpr(oldRt);
                  break;
              default:
                 vassert(0);
            }
            storeGuardedLE(mkexpr(transAddr), data, condT);

         } else {

            /* Load. */
            IRTemp llOldRt = newTemp(Ity_I32);
            assign(llOldRt, llGetIReg(rT));

            /* generate the transfer */
            IRTemp    newRt = newTemp(Ity_I32);
            IRLoadGOp widen = ILGop_INVALID;
            switch (ty) {
               case Ity_I8:
                  widen = syned ? ILGop_8Sto32 : ILGop_8Uto32; break;
               case Ity_I16:
                  widen = syned ? ILGop_16Sto32 : ILGop_16Uto32; break;
               case Ity_I32:
                  widen = ILGop_Ident32; break;
               default:
                  vassert(0);
            }
            loadGuardedLE(newRt, widen,
                          mkexpr(transAddr), mkexpr(llOldRt), condT);

            if (rT == 15) {
               vassert(loadsPC);
               /* We'll do the write to the PC just below */
            } else {
               vassert(!loadsPC);
               /* IRTemp_INVALID is OK here because in the case where
                  condT is false at run time, we're just putting the
                  old rT value back. */
               putIRegT(rT, mkexpr(newRt), IRTemp_INVALID);
            }

            if (loadsPC) {
               /* Presumably this is an interworking branch. */
               vassert(rN != 15); // assured by validity check above
               vassert(rT == 15);
               vassert(condT == IRTemp_INVALID); /* due to check above */
               llPutIReg(15, mkexpr(newRt));
               dres.jk_StopHere = Ijk_Boring;  /* or _Ret ? */
               dres.whatNext    = Dis_StopHere;
            }
         }

         DIP("%s.w r%u, [r%u, r%u, LSL #%u]\n",
             nm, rT, rN, rM, imm2);

         goto decode_success;
      }
   }

   /* --------------- LD/ST reg+imm12 --------------- */
   /* Loads and stores of the form:
         op  Rt, [Rn, #+-imm12]
      where op is one of
         ldrb ldrh ldr  ldrsb ldrsh
         strb strh str
   */
   if (INSN0(15,9) == BITS7(1,1,1,1,1,0,0)) {
      Bool   valid  = True;
      Bool   syned  = INSN0(8,8) == 1;
      Bool   isST   = False;
      IRType ty     = Ity_I8;
      UInt   bU     = INSN0(7,7); // 1: +imm   0: -imm
                                  // -imm is only supported by literal versions
      const HChar* nm = "???";

      switch (INSN0(6,4)) {
         case BITS3(0,0,0):   // strb
            nm = "strb"; isST = True; break;
         case BITS3(0,0,1):   // ldrb
            nm = syned ? "ldrsb" : "ldrb"; break;
         case BITS3(0,1,0):   // strh
            nm = "strh"; ty = Ity_I16; isST = True; break;
         case BITS3(0,1,1):   // ldrh
            nm = syned ? "ldrsh" : "ldrh"; ty = Ity_I16; break;
         case BITS3(1,0,0):   // str
            nm = "str"; ty = Ity_I32; isST = True; break;
         case BITS3(1,0,1):
            nm = "ldr"; ty = Ity_I32; break;  // ldr
         default:
            valid = False; break;
      }

      UInt rN      = INSN0(3,0);
      UInt rT      = INSN1(15,12);
      UInt imm12   = INSN1(11,0);
      Bool loadsPC = False;

      if (rN != 15 && bU == 0) {
         // only pc supports #-imm12
         valid = False;
      }

      if (isST) {
         if (syned) valid = False;
         if (rN == 15 || rT == 15)
            valid = False;
      } else {
         /* For a 32-bit load, rT == 15 is only allowable if we are not
            in an IT block, or are the last in it.  Need to insert
            a dynamic check for that.  Also, in this particular
            case, rN == 15 is allowable.  In this case however, the
            value obtained for rN is (apparently)
            "word-align(address of current insn + 4)". */
         if (rT == 15) {
            if (ty == Ity_I32)
               loadsPC = True;
            else // Can't do it for B/H loads
               valid = False;
         }
      }

      if (valid) {
         // if it's a branch, it can't happen in the middle of an IT block
         // Also, if it is a branch, make it unconditional at this point.
         // Doing conditional branches in-line is too complex (for now)
         if (loadsPC) {
            gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
            // go uncond
            mk_skip_over_T32_if_cond_is_false(condT);
            condT = IRTemp_INVALID;
            // now uncond
         }

         IRTemp rNt = newTemp(Ity_I32);
         if (rN == 15) {
            vassert(!isST);
            assign(rNt, binop(Iop_And32, getIRegT(15), mkU32(~3)));
         } else {
            assign(rNt, getIRegT(rN));
         }

         IRTemp transAddr = newTemp(Ity_I32);
         assign(transAddr,
                binop(bU == 1 ? Iop_Add32 : Iop_Sub32,
                      mkexpr(rNt), mkU32(imm12)));

         IRTemp oldRt = newTemp(Ity_I32);
         assign(oldRt, getIRegT(rT));

         IRTemp llOldRt = newTemp(Ity_I32);
         assign(llOldRt, llGetIReg(rT));

         if (isST) {
            IRExpr* data = NULL;
            switch (ty) {
               case Ity_I8:
                  data = unop(Iop_32to8, mkexpr(oldRt));
                  break;
               case Ity_I16:
                  data = unop(Iop_32to16, mkexpr(oldRt));
                  break;
              case Ity_I32:
                  data = mkexpr(oldRt);
                  break;
              default:
                 vassert(0);
            }
            storeGuardedLE(mkexpr(transAddr), data, condT);
         } else {
            IRTemp    newRt = newTemp(Ity_I32);
            IRLoadGOp widen = ILGop_INVALID;
            switch (ty) {
               case Ity_I8:
                  widen = syned ? ILGop_8Sto32 : ILGop_8Uto32; break;
               case Ity_I16:
                  widen = syned ? ILGop_16Sto32 : ILGop_16Uto32; break;
               case Ity_I32:
                  widen = ILGop_Ident32; break;
               default:
                  vassert(0);
            }
            loadGuardedLE(newRt, widen,
                          mkexpr(transAddr), mkexpr(llOldRt), condT);
            if (rT == 15) {
               vassert(loadsPC);
               /* We'll do the write to the PC just below */
            } else {
               vassert(!loadsPC);
               /* IRTemp_INVALID is OK here because in the case where
                  condT is false at run time, we're just putting the
                  old rT value back. */
               putIRegT(rT, mkexpr(newRt), IRTemp_INVALID);
            }

            if (loadsPC) {
               /* Presumably this is an interworking branch. */
               vassert(rT == 15);
               vassert(condT == IRTemp_INVALID); /* due to check above */
               llPutIReg(15, mkexpr(newRt));
               dres.jk_StopHere = Ijk_Boring;
               dres.whatNext    = Dis_StopHere;
            }
         }

         DIP("%s.w r%u, [r%u, +#%u]\n", nm, rT, rN, imm12);

         goto decode_success;
      }
   }

   /* -------------- LDRD/STRD reg+/-#imm8 -------------- */
   /* Doubleword loads and stores of the form:
         ldrd/strd  Rt, Rt2, [Rn, #+/-imm8]    or
         ldrd/strd  Rt, Rt2, [Rn], #+/-imm8    or
         ldrd/strd  Rt, Rt2, [Rn, #+/-imm8]!  
   */
   if (INSN0(15,9) == BITS7(1,1,1,0,1,0,0) && INSN0(6,6) == 1) {
      UInt bP   = INSN0(8,8);
      UInt bU   = INSN0(7,7);
      UInt bW   = INSN0(5,5);
      UInt bL   = INSN0(4,4);  // 1: load  0: store
      UInt rN   = INSN0(3,0);
      UInt rT   = INSN1(15,12);
      UInt rT2  = INSN1(11,8);
      UInt imm8 = INSN1(7,0);

      Bool valid = True;
      if (bP == 0 && bW == 0)                 valid = False;
      if (bW == 1 && (rN == rT || rN == rT2)) valid = False;
      if (isBadRegT(rT) || isBadRegT(rT2))    valid = False;
      if (bL == 1 && rT == rT2)               valid = False;
      /* It's OK to use PC as the base register only in the
         following case: ldrd Rt, Rt2, [PC, #+/-imm8] */
      if (rN == 15 && (bL == 0/*store*/
                       || bW == 1/*wb*/))     valid = False;

      if (valid) {
         IRTemp preAddr = newTemp(Ity_I32);
         assign(preAddr, 15 == rN
                           ? binop(Iop_And32, getIRegT(15), mkU32(~3U))
                           : getIRegT(rN));

         IRTemp postAddr = newTemp(Ity_I32);
         assign(postAddr, binop(bU == 1 ? Iop_Add32 : Iop_Sub32,
                                mkexpr(preAddr), mkU32(imm8 << 2)));

         IRTemp transAddr = bP == 1 ? postAddr : preAddr;

         /* For almost all cases, we do the writeback after the transfers.
            However, that leaves the stack "uncovered" in this case:
               strd    rD, [sp, #-8]
            In which case, do the writeback to SP now, instead of later.
            This is bad in that it makes the insn non-restartable if the
            accesses fault, but at least keeps Memcheck happy. */
         Bool writeback_already_done = False;
         if (bL == 0/*store*/ && bW == 1/*wb*/
             && rN == 13 && rN != rT && rN != rT2
             && bU == 0/*minus*/ && (imm8 << 2) == 8) {
            putIRegT(rN, mkexpr(postAddr), condT);
            writeback_already_done = True;
         }

         if (bL == 0) {
            IRTemp oldRt  = newTemp(Ity_I32);
            IRTemp oldRt2 = newTemp(Ity_I32);
            assign(oldRt,  getIRegT(rT));
            assign(oldRt2, getIRegT(rT2));
            storeGuardedLE( mkexpr(transAddr),
                            mkexpr(oldRt), condT );
            storeGuardedLE( binop(Iop_Add32, mkexpr(transAddr), mkU32(4)),
                            mkexpr(oldRt2), condT );
         } else {
            IRTemp oldRt  = newTemp(Ity_I32);
            IRTemp oldRt2 = newTemp(Ity_I32);
            IRTemp newRt  = newTemp(Ity_I32);
            IRTemp newRt2 = newTemp(Ity_I32);
            assign(oldRt,  llGetIReg(rT));
            assign(oldRt2, llGetIReg(rT2));
            loadGuardedLE( newRt, ILGop_Ident32,
                           mkexpr(transAddr),
                           mkexpr(oldRt), condT );
            loadGuardedLE( newRt2, ILGop_Ident32,
                           binop(Iop_Add32, mkexpr(transAddr), mkU32(4)),
                           mkexpr(oldRt2), condT );
            /* Put unconditionally, since we already switched on the condT
               in the guarded loads. */ 
            putIRegT(rT,  mkexpr(newRt),  IRTemp_INVALID);
            putIRegT(rT2, mkexpr(newRt2), IRTemp_INVALID);
         }

         if (bW == 1 && !writeback_already_done) {
            putIRegT(rN, mkexpr(postAddr), condT);
         }

         const HChar* nm = bL ? "ldrd" : "strd";

         if (bP == 1 && bW == 0) {
            DIP("%s.w r%u, r%u, [r%u, #%c%u]\n",
                nm, rT, rT2, rN, bU ? '+' : '-', imm8 << 2);
         }
         else if (bP == 1 && bW == 1) {
            DIP("%s.w r%u, r%u, [r%u, #%c%u]!\n",
                nm, rT, rT2, rN, bU ? '+' : '-', imm8 << 2);
         }
         else {
            vassert(bP == 0 && bW == 1);
            DIP("%s.w r%u, r%u, [r%u], #%c%u\n",
                nm, rT, rT2, rN, bU ? '+' : '-', imm8 << 2);
         }

         goto decode_success;
      }
   }

   /* -------------- (T3) Bcond.W label -------------- */
   /* This variant carries its own condition, so can't be part of an
      IT block ... */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && INSN1(15,14) == BITS2(1,0)
       && INSN1(12,12) == 0) {
      UInt cond = INSN0(9,6);
      if (cond != ARMCondAL && cond != ARMCondNV) {
         UInt uimm21
            =   (INSN0(10,10) << (1 + 1 + 6 + 11 + 1))
              | (INSN1(11,11) << (1 + 6 + 11 + 1))
              | (INSN1(13,13) << (6 + 11 + 1))
              | (INSN0(5,0)   << (11 + 1))
              | (INSN1(10,0)  << 1);
         uimm21 <<= 11;
         Int simm21 = (Int)uimm21;
         simm21 >>= 11;

         vassert(0 == (guest_R15_curr_instr_notENC & 1));
         UInt dst = simm21 + guest_R15_curr_instr_notENC + 4;

         /* Not allowed in an IT block; SIGILL if so. */
         gen_SIGILL_T_if_in_ITBlock(old_itstate, new_itstate);

         IRTemp kondT = newTemp(Ity_I32);
         assign( kondT, mk_armg_calculate_condition(cond) );
         stmt( IRStmt_Exit( unop(Iop_32to1, mkexpr(kondT)),
                            Ijk_Boring,
                            IRConst_U32(dst | 1/*CPSR.T*/),
                            OFFB_R15T ));
         llPutIReg(15, mkU32( (guest_R15_curr_instr_notENC + 4) 
                              | 1 /*CPSR.T*/ ));
         dres.jk_StopHere = Ijk_Boring;
         dres.whatNext    = Dis_StopHere;
         DIP("b%s.w 0x%x\n", nCC(cond), dst);
         goto decode_success;
      }
   }

   /* ---------------- (T4) B.W label ---------------- */
   /* ... whereas this variant doesn't carry its own condition, so it
      has to be either unconditional or the conditional by virtue of
      being the last in an IT block.  The upside is that there's 4
      more bits available for the jump offset, so it has a 16-times
      greater branch range than the T3 variant. */
   if (INSN0(15,11) == BITS5(1,1,1,1,0)
       && INSN1(15,14) == BITS2(1,0)
       && INSN1(12,12) == 1) {
      if (1) {
         UInt bS  = INSN0(10,10);
         UInt bJ1 = INSN1(13,13);
         UInt bJ2 = INSN1(11,11);
         UInt bI1 = 1 ^ (bJ1 ^ bS);
         UInt bI2 = 1 ^ (bJ2 ^ bS);
         UInt uimm25
            =   (bS          << (1 + 1 + 10 + 11 + 1))
              | (bI1         << (1 + 10 + 11 + 1))
              | (bI2         << (10 + 11 + 1))
              | (INSN0(9,0)  << (11 + 1))
              | (INSN1(10,0) << 1);
         uimm25 <<= 7;
         Int simm25 = (Int)uimm25;
         simm25 >>= 7;

         vassert(0 == (guest_R15_curr_instr_notENC & 1));
         UInt dst = simm25 + guest_R15_curr_instr_notENC + 4;

         /* If in an IT block, must be the last insn. */
         gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);

         // go uncond
         mk_skip_over_T32_if_cond_is_false(condT);
         condT = IRTemp_INVALID;
         // now uncond

         // branch to dst
         llPutIReg(15, mkU32( dst | 1 /*CPSR.T*/ ));
         dres.jk_StopHere = Ijk_Boring;
         dres.whatNext    = Dis_StopHere;
         DIP("b.w 0x%x\n", dst);
         goto decode_success;
      }
   }

   /* ------------------ TBB, TBH ------------------ */
   if (INSN0(15,4) == 0xE8D && INSN1(15,5) == 0x780) {
      UInt rN = INSN0(3,0);
      UInt rM = INSN1(3,0);
      UInt bH = INSN1(4,4);
      if (bH/*ATC*/ || (rN != 13 && !isBadRegT(rM))) {
         /* Must be last or not-in IT block */
         gen_SIGILL_T_if_in_but_NLI_ITBlock(old_itstate, new_itstate);
         /* Go uncond */
         mk_skip_over_T32_if_cond_is_false(condT);
         condT = IRTemp_INVALID;

         IRExpr* ea
             = binop(Iop_Add32,
                     getIRegT(rN),
                     bH ? binop(Iop_Shl32, getIRegT(rM), mkU8(1))
                        : getIRegT(rM));

         IRTemp delta = newTemp(Ity_I32);
         if (bH) {
            assign(delta, unop(Iop_16Uto32, loadLE(Ity_I16, ea)));
         } else {
            assign(delta, unop(Iop_8Uto32, loadLE(Ity_I8, ea)));
         }

         llPutIReg(
            15,
            binop(Iop_Or32,
                  binop(Iop_Add32,
                        getIRegT(15),
                        binop(Iop_Shl32, mkexpr(delta), mkU8(1))
                  ),
                  mkU32(1)
         ));
         dres.jk_StopHere = Ijk_Boring;
         dres.whatNext    = Dis_StopHere;
         DIP("tb%c [r%u, r%u%s]\n",
             bH ? 'h' : 'b', rN, rM, bH ? ", LSL #1" : "");
         goto decode_success;
      }
   }

   /* ------------------ UBFX ------------------ */
   /* ------------------ SBFX ------------------ */
   /* There's also ARM versions of same, but it doesn't seem worth the
      hassle to common up the handling (it's only a couple of C
      statements). */
   if ((INSN0(15,4) == 0xF3C // UBFX
        || INSN0(15,4) == 0xF34) // SBFX
       && INSN1(15,15) == 0 && INSN1(5,5) == 0) {
      UInt rN  = INSN0(3,0);
      UInt rD  = INSN1(11,8);
      UInt lsb = (INSN1(14,12) << 2) | INSN1(7,6);
      UInt wm1 = INSN1(4,0);
      UInt msb =  lsb + wm1;
      if (!isBadRegT(rD) && !isBadRegT(rN) && msb <= 31) {
         Bool   isU  = INSN0(15,4) == 0xF3C;
         IRTemp src  = newTemp(Ity_I32);
         IRTemp tmp  = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         UInt   mask = ((1 << wm1) - 1) + (1 << wm1);
         vassert(msb >= 0 && msb <= 31);
         vassert(mask != 0); // guaranteed by msb being in 0 .. 31 inclusive

         assign(src, getIRegT(rN));
         assign(tmp, binop(Iop_And32,
                           binop(Iop_Shr32, mkexpr(src), mkU8(lsb)),
                           mkU32(mask)));
         assign(res, binop(isU ? Iop_Shr32 : Iop_Sar32,
                           binop(Iop_Shl32, mkexpr(tmp), mkU8(31-wm1)),
                           mkU8(31-wm1)));

         putIRegT(rD, mkexpr(res), condT);

         DIP("%s r%u, r%u, #%u, #%u\n",
             isU ? "ubfx" : "sbfx", rD, rN, lsb, wm1 + 1);
         goto decode_success;
      }
   }

   /* ------------------ UXTB ------------------ */
   /* ------------------ UXTH ------------------ */
   /* ------------------ SXTB ------------------ */
   /* ------------------ SXTH ------------------ */
   /* ----------------- UXTB16 ----------------- */
   /* ----------------- SXTB16 ----------------- */
   /* FIXME: this is an exact duplicate of the ARM version.  They
      should be commoned up. */
   if ((INSN0(15,0) == 0xFA5F     // UXTB
        || INSN0(15,0) == 0xFA1F  // UXTH
        || INSN0(15,0) == 0xFA4F  // SXTB
        || INSN0(15,0) == 0xFA0F  // SXTH
        || INSN0(15,0) == 0xFA3F  // UXTB16
        || INSN0(15,0) == 0xFA2F) // SXTB16
       && INSN1(15,12) == BITS4(1,1,1,1)
       && INSN1(7,6) == BITS2(1,0)) {
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      UInt rot = INSN1(5,4);
      if (!isBadRegT(rD) && !isBadRegT(rM)) {
         const HChar* nm = "???";
         IRTemp srcT = newTemp(Ity_I32);
         IRTemp rotT = newTemp(Ity_I32);
         IRTemp dstT = newTemp(Ity_I32);
         assign(srcT, getIRegT(rM));
         assign(rotT, genROR32(srcT, 8 * rot));
         switch (INSN0(15,0)) {
            case 0xFA5F: // UXTB
               nm = "uxtb";
               assign(dstT, unop(Iop_8Uto32,
                                 unop(Iop_32to8, mkexpr(rotT))));
               break;
            case 0xFA1F: // UXTH
               nm = "uxth";
               assign(dstT, unop(Iop_16Uto32,
                                 unop(Iop_32to16, mkexpr(rotT))));
               break;
            case 0xFA4F: // SXTB
               nm = "sxtb";
               assign(dstT, unop(Iop_8Sto32,
                                 unop(Iop_32to8, mkexpr(rotT))));
               break;
            case 0xFA0F: // SXTH
               nm = "sxth";
               assign(dstT, unop(Iop_16Sto32,
                                 unop(Iop_32to16, mkexpr(rotT))));
               break;
            case 0xFA3F: // UXTB16
               nm = "uxtb16";
               assign(dstT, binop(Iop_And32, mkexpr(rotT),
                                             mkU32(0x00FF00FF)));
               break;
            case 0xFA2F: { // SXTB16
               nm = "sxtb16";
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
               break;
            }
            default:
               vassert(0);
         }
         putIRegT(rD, mkexpr(dstT), condT);
         DIP("%s r%u, r%u, ror #%u\n", nm, rD, rM, 8 * rot);
         goto decode_success;
      }
   }

   /* -------------- MUL.W Rd, Rn, Rm -------------- */
   if (INSN0(15,4) == 0xFB0
       && (INSN1(15,0) & 0xF0F0) == 0xF000) {
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM)) {
         IRTemp res = newTemp(Ity_I32);
         assign(res, binop(Iop_Mul32, getIRegT(rN), getIRegT(rM)));
         putIRegT(rD, mkexpr(res), condT);
         DIP("mul.w r%u, r%u, r%u\n", rD, rN, rM);
         goto decode_success;
      }
   }

   /* -------------- SDIV.W Rd, Rn, Rm -------------- */
   if (INSN0(15,4) == 0xFB9
       && (INSN1(15,0) & 0xF0F0) == 0xF0F0) {
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM)) {
         IRTemp res  = newTemp(Ity_I32);
         IRTemp argL = newTemp(Ity_I32);
         IRTemp argR = newTemp(Ity_I32);
         assign(argL, getIRegT(rN));
         assign(argR, getIRegT(rM));
         assign(res, binop(Iop_DivS32, mkexpr(argL), mkexpr(argR)));
         putIRegT(rD, mkexpr(res), condT);
         DIP("sdiv.w r%u, r%u, r%u\n", rD, rN, rM);
         goto decode_success;
      }
   }

   /* -------------- UDIV.W Rd, Rn, Rm -------------- */
   if (INSN0(15,4) == 0xFBB
       && (INSN1(15,0) & 0xF0F0) == 0xF0F0) {
      UInt rN = INSN0(3,0);
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM)) {
         IRTemp res  = newTemp(Ity_I32);
         IRTemp argL = newTemp(Ity_I32);
         IRTemp argR = newTemp(Ity_I32);
         assign(argL, getIRegT(rN));
         assign(argR, getIRegT(rM));
         assign(res, binop(Iop_DivU32, mkexpr(argL), mkexpr(argR)));
         putIRegT(rD, mkexpr(res), condT);
         DIP("udiv.w r%u, r%u, r%u\n", rD, rN, rM);
         goto decode_success;
      }
   }

   /* ------------------ {U,S}MULL ------------------ */
   if ((INSN0(15,4) == 0xFB8 || INSN0(15,4) == 0xFBA)
       && INSN1(7,4) == BITS4(0,0,0,0)) {
      UInt isU  = INSN0(5,5);
      UInt rN   = INSN0(3,0);
      UInt rDlo = INSN1(15,12);
      UInt rDhi = INSN1(11,8);
      UInt rM   = INSN1(3,0);
      if (!isBadRegT(rDhi) && !isBadRegT(rDlo)
          && !isBadRegT(rN) && !isBadRegT(rM) && rDlo != rDhi) {
         IRTemp res   = newTemp(Ity_I64);
         assign(res, binop(isU ? Iop_MullU32 : Iop_MullS32,
                           getIRegT(rN), getIRegT(rM)));
         putIRegT( rDhi, unop(Iop_64HIto32, mkexpr(res)), condT );
         putIRegT( rDlo, unop(Iop_64to32, mkexpr(res)), condT );
         DIP("%cmull r%u, r%u, r%u, r%u\n",
             isU ? 'u' : 's', rDlo, rDhi, rN, rM);
         goto decode_success;
      }
   }

   /* ------------------ ML{A,S} ------------------ */
   if (INSN0(15,4) == 0xFB0
       && (   INSN1(7,4) == BITS4(0,0,0,0)    // MLA
           || INSN1(7,4) == BITS4(0,0,0,1))) { // MLS
      UInt rN = INSN0(3,0);
      UInt rA = INSN1(15,12);
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rN)
          && !isBadRegT(rM) && !isBadRegT(rA)) {
         Bool   isMLA = INSN1(7,4) == BITS4(0,0,0,0);
         IRTemp res   = newTemp(Ity_I32);
         assign(res,
                binop(isMLA ? Iop_Add32 : Iop_Sub32,
                      getIRegT(rA),
                      binop(Iop_Mul32, getIRegT(rN), getIRegT(rM))));
         putIRegT(rD, mkexpr(res), condT);
         DIP("%s r%u, r%u, r%u, r%u\n",
             isMLA ? "mla" : "mls", rD, rN, rM, rA);
         goto decode_success;
      }
   }

   /* ------------------ (T3) ADR ------------------ */
   if ((INSN0(15,0) == 0xF20F || INSN0(15,0) == 0xF60F)
       && INSN1(15,15) == 0) {
      /* rD = align4(PC) + imm32 */
      UInt rD = INSN1(11,8);
      if (!isBadRegT(rD)) {
         UInt imm32 = (INSN0(10,10) << 11)
                      | (INSN1(14,12) << 8) | INSN1(7,0);
         putIRegT(rD, binop(Iop_Add32, 
                            binop(Iop_And32, getIRegT(15), mkU32(~3U)),
                            mkU32(imm32)),
                      condT);
         DIP("add r%u, pc, #%u\n", rD, imm32);
         goto decode_success;
      }
   }

   /* ----------------- (T1) UMLAL ----------------- */
   /* ----------------- (T1) SMLAL ----------------- */
   if ((INSN0(15,4) == 0xFBE // UMLAL
        || INSN0(15,4) == 0xFBC) // SMLAL
       && INSN1(7,4) == BITS4(0,0,0,0)) {
      UInt rN   = INSN0(3,0);
      UInt rDlo = INSN1(15,12);
      UInt rDhi = INSN1(11,8);
      UInt rM   = INSN1(3,0);
      if (!isBadRegT(rDlo) && !isBadRegT(rDhi) && !isBadRegT(rN)
          && !isBadRegT(rM) && rDhi != rDlo) {
         Bool   isS   = INSN0(15,4) == 0xFBC;
         IRTemp argL  = newTemp(Ity_I32);
         IRTemp argR  = newTemp(Ity_I32);
         IRTemp old   = newTemp(Ity_I64);
         IRTemp res   = newTemp(Ity_I64);
         IRTemp resHi = newTemp(Ity_I32);
         IRTemp resLo = newTemp(Ity_I32);
         IROp   mulOp = isS ? Iop_MullS32 : Iop_MullU32;
         assign( argL, getIRegT(rM));
         assign( argR, getIRegT(rN));
         assign( old, binop(Iop_32HLto64, getIRegT(rDhi), getIRegT(rDlo)) );
         assign( res, binop(Iop_Add64,
                            mkexpr(old),
                            binop(mulOp, mkexpr(argL), mkexpr(argR))) );
         assign( resHi, unop(Iop_64HIto32, mkexpr(res)) );
         assign( resLo, unop(Iop_64to32, mkexpr(res)) );
         putIRegT( rDhi, mkexpr(resHi), condT );
         putIRegT( rDlo, mkexpr(resLo), condT );
         DIP("%cmlal r%u, r%u, r%u, r%u\n",
             isS ? 's' : 'u', rDlo, rDhi, rN, rM);
         goto decode_success;
      }
   }

   /* ------------------ (T1) UMAAL ------------------ */
   if (INSN0(15,4) == 0xFBE && INSN1(7,4) == BITS4(0,1,1,0)) {
      UInt rN   = INSN0(3,0);
      UInt rDlo = INSN1(15,12);
      UInt rDhi = INSN1(11,8);
      UInt rM   = INSN1(3,0);
      if (!isBadRegT(rDlo) && !isBadRegT(rDhi) && !isBadRegT(rN)
          && !isBadRegT(rM) && rDhi != rDlo) {
         IRTemp argN   = newTemp(Ity_I32);
         IRTemp argM   = newTemp(Ity_I32);
         IRTemp argDhi = newTemp(Ity_I32);
         IRTemp argDlo = newTemp(Ity_I32);
         IRTemp res    = newTemp(Ity_I64);
         IRTemp resHi  = newTemp(Ity_I32);
         IRTemp resLo  = newTemp(Ity_I32);
         assign( argN,   getIRegT(rN) );
         assign( argM,   getIRegT(rM) );
         assign( argDhi, getIRegT(rDhi) );
         assign( argDlo, getIRegT(rDlo) );
         assign( res, 
                 binop(Iop_Add64,
                       binop(Iop_Add64,
                             binop(Iop_MullU32, mkexpr(argN), mkexpr(argM)),
                             unop(Iop_32Uto64, mkexpr(argDhi))),
                       unop(Iop_32Uto64, mkexpr(argDlo))) );
         assign( resHi, unop(Iop_64HIto32, mkexpr(res)) );
         assign( resLo, unop(Iop_64to32, mkexpr(res)) );
         putIRegT( rDhi, mkexpr(resHi), condT );
         putIRegT( rDlo, mkexpr(resLo), condT );
         DIP("umaal r%u, r%u, r%u, r%u\n", rDlo, rDhi, rN, rM);
         goto decode_success;
      }
   }

   /* ------------------- (T1) SMMUL{R} ------------------ */
   if (INSN0(15,7) == BITS9(1,1,1,1,1,0,1,1,0)
       && INSN0(6,4) == BITS3(1,0,1)
       && INSN1(15,12) == BITS4(1,1,1,1)
       && INSN1(7,5) == BITS3(0,0,0)) {
      UInt bitR = INSN1(4,4);
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      UInt rN = INSN0(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM)) {
         IRExpr* res
         = unop(Iop_64HIto32,
                binop(Iop_Add64,
                      binop(Iop_MullS32, getIRegT(rN), getIRegT(rM)),
                      mkU64(bitR ? 0x80000000ULL : 0ULL)));
         putIRegT(rD, res, condT);
         DIP("smmul%s r%u, r%u, r%u\n",
             bitR ? "r" : "", rD, rN, rM);
         goto decode_success;
      }
   }

   /* ------------------- (T1) SMMLA{R} ------------------ */
   if (INSN0(15,7) == BITS9(1,1,1,1,1,0,1,1,0)
       && INSN0(6,4) == BITS3(1,0,1)
       && INSN1(7,5) == BITS3(0,0,0)) {
      UInt bitR = INSN1(4,4);
      UInt rA = INSN1(15,12);
      UInt rD = INSN1(11,8);
      UInt rM = INSN1(3,0);
      UInt rN = INSN0(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM) && (rA != 13)) {
         IRExpr* res
         = unop(Iop_64HIto32,
                binop(Iop_Add64,
                      binop(Iop_Add64,
                            binop(Iop_32HLto64, getIRegT(rA), mkU32(0)),
                            binop(Iop_MullS32, getIRegT(rN), getIRegT(rM))),
                      mkU64(bitR ? 0x80000000ULL : 0ULL)));
         putIRegT(rD, res, condT);
         DIP("smmla%s r%u, r%u, r%u, r%u\n",
             bitR ? "r" : "", rD, rN, rM, rA);
         goto decode_success;
      }
   }

   /* ------------------ (T2) ADR ------------------ */
   if ((INSN0(15,0) == 0xF2AF || INSN0(15,0) == 0xF6AF)
       && INSN1(15,15) == 0) {
      /* rD = align4(PC) - imm32 */
      UInt rD = INSN1(11,8);
      if (!isBadRegT(rD)) {
         UInt imm32 = (INSN0(10,10) << 11)
                      | (INSN1(14,12) << 8) | INSN1(7,0);
         putIRegT(rD, binop(Iop_Sub32, 
                            binop(Iop_And32, getIRegT(15), mkU32(~3U)),
                            mkU32(imm32)),
                      condT);
         DIP("sub r%u, pc, #%u\n", rD, imm32);
         goto decode_success;
      }
   }

   /* ------------------- (T1) BFI ------------------- */
   /* ------------------- (T1) BFC ------------------- */
   if (INSN0(15,4) == 0xF36 && INSN1(15,15) == 0 && INSN1(5,5) == 0) {
      UInt rD  = INSN1(11,8);
      UInt rN  = INSN0(3,0);
      UInt msb = INSN1(4,0);
      UInt lsb = (INSN1(14,12) << 2) | INSN1(7,6);
      if (isBadRegT(rD) || rN == 13 || msb < lsb) {
         /* undecodable; fall through */
      } else {
         IRTemp src    = newTemp(Ity_I32);
         IRTemp olddst = newTemp(Ity_I32);
         IRTemp newdst = newTemp(Ity_I32);
         UInt   mask   = ((UInt)1) << (msb - lsb);
         mask = (mask - 1) + mask;
         vassert(mask != 0); // guaranteed by "msb < lsb" check above
         mask <<= lsb;

         assign(src, rN == 15 ? mkU32(0) : getIRegT(rN));
         assign(olddst, getIRegT(rD));
         assign(newdst,
                binop(Iop_Or32,
                   binop(Iop_And32,
                         binop(Iop_Shl32, mkexpr(src), mkU8(lsb)), 
                         mkU32(mask)),
                   binop(Iop_And32,
                         mkexpr(olddst),
                         mkU32(~mask)))
               );

         putIRegT(rD, mkexpr(newdst), condT);

         if (rN == 15) {
            DIP("bfc r%u, #%u, #%u\n",
                rD, lsb, msb-lsb+1);
         } else {
            DIP("bfi r%u, r%u, #%u, #%u\n",
                rD, rN, lsb, msb-lsb+1);
         }
         goto decode_success;
      }
   }

   /* ------------------- (T1) SXTAH ------------------- */
   /* ------------------- (T1) UXTAH ------------------- */
   if ((INSN0(15,4) == 0xFA1      // UXTAH
        || INSN0(15,4) == 0xFA0)  // SXTAH
       && INSN1(15,12) == BITS4(1,1,1,1)
       && INSN1(7,6) == BITS2(1,0)) {
      Bool isU = INSN0(15,4) == 0xFA1;
      UInt rN  = INSN0(3,0);
      UInt rD  = INSN1(11,8);
      UInt rM  = INSN1(3,0);
      UInt rot = INSN1(5,4);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM)) {
         IRTemp srcL = newTemp(Ity_I32);
         IRTemp srcR = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         assign(srcR, getIRegT(rM));
         assign(srcL, getIRegT(rN));
         assign(res,  binop(Iop_Add32,
                            mkexpr(srcL),
                            unop(isU ? Iop_16Uto32 : Iop_16Sto32,
                                 unop(Iop_32to16, 
                                      genROR32(srcR, 8 * rot)))));
         putIRegT(rD, mkexpr(res), condT);
         DIP("%cxtah r%u, r%u, r%u, ror #%u\n",
             isU ? 'u' : 's', rD, rN, rM, rot);
         goto decode_success;
      }
   }

   /* ------------------- (T1) SXTAB ------------------- */
   /* ------------------- (T1) UXTAB ------------------- */
   if ((INSN0(15,4) == 0xFA5      // UXTAB
        || INSN0(15,4) == 0xFA4)  // SXTAB
       && INSN1(15,12) == BITS4(1,1,1,1)
       && INSN1(7,6) == BITS2(1,0)) {
      Bool isU = INSN0(15,4) == 0xFA5;
      UInt rN  = INSN0(3,0);
      UInt rD  = INSN1(11,8);
      UInt rM  = INSN1(3,0);
      UInt rot = INSN1(5,4);
      if (!isBadRegT(rD) && !isBadRegT(rN) && !isBadRegT(rM)) {
         IRTemp srcL = newTemp(Ity_I32);
         IRTemp srcR = newTemp(Ity_I32);
         IRTemp res  = newTemp(Ity_I32);
         assign(srcR, getIRegT(rM));
         assign(srcL, getIRegT(rN));
         assign(res,  binop(Iop_Add32,
                            mkexpr(srcL),
                            unop(isU ? Iop_8Uto32 : Iop_8Sto32,
                                 unop(Iop_32to8, 
                                      genROR32(srcR, 8 * rot)))));
         putIRegT(rD, mkexpr(res), condT);
         DIP("%cxtab r%u, r%u, r%u, ror #%u\n",
             isU ? 'u' : 's', rD, rN, rM, rot);
         goto decode_success;
      }
   }

   /* ------------------- (T1) CLZ ------------------- */
   if (INSN0(15,4) == 0xFAB
       && INSN1(15,12) == BITS4(1,1,1,1)
       && INSN1(7,4) == BITS4(1,0,0,0)) {
      UInt rM1 = INSN0(3,0);
      UInt rD  = INSN1(11,8);
      UInt rM2 = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rM1) && rM1 == rM2) {
         IRTemp arg = newTemp(Ity_I32);
         IRTemp res = newTemp(Ity_I32);
         assign(arg, getIRegT(rM1));
         assign(res, IRExpr_ITE(
                        binop(Iop_CmpEQ32, mkexpr(arg), mkU32(0)),
                        mkU32(32),
                        unop(Iop_Clz32, mkexpr(arg))
         ));
         putIRegT(rD, mkexpr(res), condT);
         DIP("clz r%u, r%u\n", rD, rM1);
         goto decode_success;
      }
   }

   /* ------------------- (T1) RBIT ------------------- */
   if (INSN0(15,4) == 0xFA9
       && INSN1(15,12) == BITS4(1,1,1,1)
       && INSN1(7,4) == BITS4(1,0,1,0)) {
      UInt rM1 = INSN0(3,0);
      UInt rD  = INSN1(11,8);
      UInt rM2 = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rM1) && rM1 == rM2) {
         IRTemp arg = newTemp(Ity_I32);
         assign(arg, getIRegT(rM1));
         IRTemp res = gen_BITREV(arg);
         putIRegT(rD, mkexpr(res), condT);
         DIP("rbit r%u, r%u\n", rD, rM1);
         goto decode_success;
      }
   }

   /* ------------------- (T2) REV   ------------------- */
   /* ------------------- (T2) REV16 ------------------- */
   if (INSN0(15,4) == 0xFA9
       && INSN1(15,12) == BITS4(1,1,1,1)
       && (   INSN1(7,4) == BITS4(1,0,0,0)     // REV
           || INSN1(7,4) == BITS4(1,0,0,1))) { // REV16
      UInt rM1   = INSN0(3,0);
      UInt rD    = INSN1(11,8);
      UInt rM2   = INSN1(3,0);
      Bool isREV = INSN1(7,4) == BITS4(1,0,0,0);
      if (!isBadRegT(rD) && !isBadRegT(rM1) && rM1 == rM2) {
         IRTemp arg = newTemp(Ity_I32);
         assign(arg, getIRegT(rM1));
         IRTemp res = isREV ? gen_REV(arg) : gen_REV16(arg);
         putIRegT(rD, mkexpr(res), condT);
         DIP("rev%s r%u, r%u\n", isREV ? "" : "16", rD, rM1);
         goto decode_success;
      }
   }

   /* ------------------- (T2) REVSH ------------------ */
   if (INSN0(15,4) == 0xFA9
       && INSN1(15,12) == BITS4(1,1,1,1)
       && INSN1(7,4) == BITS4(1,0,1,1)) {
      UInt rM1 = INSN0(3,0);
      UInt rM2 = INSN1(3,0);
      UInt rD  = INSN1(11,8);
      if (!isBadRegT(rD) && !isBadRegT(rM1) && rM1 == rM2) {
         IRTemp irt_rM  = newTemp(Ity_I32);
         IRTemp irt_hi  = newTemp(Ity_I32);
         IRTemp irt_low = newTemp(Ity_I32);
         IRTemp irt_res = newTemp(Ity_I32);
         assign(irt_rM, getIRegT(rM1));
         assign(irt_hi,
                binop(Iop_Sar32,
                      binop(Iop_Shl32, mkexpr(irt_rM), mkU8(24)),
                      mkU8(16)
                )
         );
         assign(irt_low,
                binop(Iop_And32,
                      binop(Iop_Shr32, mkexpr(irt_rM), mkU8(8)),
                      mkU32(0xFF)
                )
         );
         assign(irt_res,
                binop(Iop_Or32, mkexpr(irt_hi), mkexpr(irt_low))
         );
         putIRegT(rD, mkexpr(irt_res), condT);
         DIP("revsh r%u, r%u\n", rD, rM1);
         goto decode_success;
      }
   }

   /* -------------- (T1) MSR apsr, reg -------------- */
   if (INSN0(15,4) == 0xF38 
       && INSN1(15,12) == BITS4(1,0,0,0) && INSN1(9,0) == 0x000) {
      UInt rN          = INSN0(3,0);
      UInt write_ge    = INSN1(10,10);
      UInt write_nzcvq = INSN1(11,11);
      if (!isBadRegT(rN) && (write_nzcvq || write_ge)) {
         IRTemp rNt = newTemp(Ity_I32);
         assign(rNt, getIRegT(rN));
         desynthesise_APSR( write_nzcvq, write_ge, rNt, condT );
         DIP("msr cpsr_%s%s, r%u\n",
             write_nzcvq ? "f" : "", write_ge ? "g" : "", rN);
         goto decode_success;
      }
   }

   /* -------------- (T1) MRS reg, apsr -------------- */
   if (INSN0(15,0) == 0xF3EF
       && INSN1(15,12) == BITS4(1,0,0,0) && INSN1(7,0) == 0x00) {
      UInt rD = INSN1(11,8);
      if (!isBadRegT(rD)) {
         IRTemp apsr = synthesise_APSR();
         putIRegT( rD, mkexpr(apsr), condT );
         DIP("mrs r%u, cpsr\n", rD);
         goto decode_success;
      }
   }

   /* ----------------- (T1) LDREX ----------------- */
   if (INSN0(15,4) == 0xE85 && INSN1(11,8) == BITS4(1,1,1,1)) {
      UInt rN   = INSN0(3,0);
      UInt rT   = INSN1(15,12);
      UInt imm8 = INSN1(7,0);
      if (!isBadRegT(rT) && rN != 15) {
         IRTemp res;
         // go uncond
         mk_skip_over_T32_if_cond_is_false( condT );
         // now uncond
         res = newTemp(Ity_I32);
         stmt( IRStmt_LLSC(Iend_LE,
                           res,
                           binop(Iop_Add32, getIRegT(rN), mkU32(imm8 * 4)),
                           NULL/*this is a load*/ ));
         putIRegT(rT, mkexpr(res), IRTemp_INVALID);
         DIP("ldrex r%u, [r%u, #+%u]\n", rT, rN, imm8 * 4);
         goto decode_success;
      }
   }

   /* --------------- (T1) LDREX{B,H} --------------- */
   if (INSN0(15,4) == 0xE8D
       && (INSN1(11,0) == 0xF4F || INSN1(11,0) == 0xF5F)) {
      UInt rN  = INSN0(3,0);
      UInt rT  = INSN1(15,12);
      Bool isH = INSN1(11,0) == 0xF5F;
      if (!isBadRegT(rT) && rN != 15) {
         IRTemp res;
         // go uncond
         mk_skip_over_T32_if_cond_is_false( condT );
         // now uncond
         res = newTemp(isH ? Ity_I16 : Ity_I8);
         stmt( IRStmt_LLSC(Iend_LE, res, getIRegT(rN),
                           NULL/*this is a load*/ ));
         putIRegT(rT, unop(isH ? Iop_16Uto32 : Iop_8Uto32, mkexpr(res)),
                      IRTemp_INVALID);
         DIP("ldrex%c r%u, [r%u]\n", isH ? 'h' : 'b', rT, rN);
         goto decode_success;
      }
   }

   /* --------------- (T1) LDREXD --------------- */
   if (INSN0(15,4) == 0xE8D && INSN1(7,0) == 0x7F) {
      UInt rN  = INSN0(3,0);
      UInt rT  = INSN1(15,12);
      UInt rT2 = INSN1(11,8);
      if (!isBadRegT(rT) && !isBadRegT(rT2) && rT != rT2 && rN != 15) {
         IRTemp res;
         // go uncond
         mk_skip_over_T32_if_cond_is_false( condT );
         // now uncond
         res = newTemp(Ity_I64);
         // FIXME: assumes little-endian guest
         stmt( IRStmt_LLSC(Iend_LE, res, getIRegT(rN),
                           NULL/*this is a load*/ ));
         // FIXME: assumes little-endian guest
         putIRegT(rT,  unop(Iop_64to32,   mkexpr(res)), IRTemp_INVALID);
         putIRegT(rT2, unop(Iop_64HIto32, mkexpr(res)), IRTemp_INVALID);
         DIP("ldrexd r%u, r%u, [r%u]\n", rT, rT2, rN);
         goto decode_success;
      }
   }

   /* ----------------- (T1) STREX ----------------- */
   if (INSN0(15,4) == 0xE84) {
      UInt rN   = INSN0(3,0);
      UInt rT   = INSN1(15,12);
      UInt rD   = INSN1(11,8);
      UInt imm8 = INSN1(7,0);
      if (!isBadRegT(rD) && !isBadRegT(rT) && rN != 15 
          && rD != rN && rD != rT) {
         IRTemp resSC1, resSC32;
         // go uncond
         mk_skip_over_T32_if_cond_is_false( condT );
         // now uncond
         /* Ok, now we're unconditional.  Do the store. */
         resSC1 = newTemp(Ity_I1);
         stmt( IRStmt_LLSC(Iend_LE,
                           resSC1,
                           binop(Iop_Add32, getIRegT(rN), mkU32(imm8 * 4)),
                           getIRegT(rT)) );
         /* Set rD to 1 on failure, 0 on success.  Currently we have
            resSC1 == 0 on failure, 1 on success. */
         resSC32 = newTemp(Ity_I32);
         assign(resSC32,
                unop(Iop_1Uto32, unop(Iop_Not1, mkexpr(resSC1))));
         putIRegT(rD, mkexpr(resSC32), IRTemp_INVALID);
         DIP("strex r%u, r%u, [r%u, #+%u]\n", rD, rT, rN, imm8 * 4);
         goto decode_success;
      }
   }

   /* --------------- (T1) STREX{B,H} --------------- */
   if (INSN0(15,4) == 0xE8C
       && (INSN1(11,4) == 0xF4 || INSN1(11,4) == 0xF5)) {
      UInt rN  = INSN0(3,0);
      UInt rT  = INSN1(15,12);
      UInt rD  = INSN1(3,0);
      Bool isH = INSN1(11,4) == 0xF5;
      if (!isBadRegT(rD) && !isBadRegT(rT) && rN != 15 
          && rD != rN && rD != rT) {
         IRTemp resSC1, resSC32;
         // go uncond
         mk_skip_over_T32_if_cond_is_false( condT );
         // now uncond
         /* Ok, now we're unconditional.  Do the store. */
         resSC1 = newTemp(Ity_I1);
         stmt( IRStmt_LLSC(Iend_LE, resSC1, getIRegT(rN),
                           unop(isH ? Iop_32to16 : Iop_32to8,
                                getIRegT(rT))) );
         /* Set rD to 1 on failure, 0 on success.  Currently we have
            resSC1 == 0 on failure, 1 on success. */
         resSC32 = newTemp(Ity_I32);
         assign(resSC32,
                unop(Iop_1Uto32, unop(Iop_Not1, mkexpr(resSC1))));
         putIRegT(rD, mkexpr(resSC32), IRTemp_INVALID);
         DIP("strex%c r%u, r%u, [r%u]\n", isH ? 'h' : 'b', rD, rT, rN);
         goto decode_success;
      }
   }

   /* ---------------- (T1) STREXD ---------------- */
   if (INSN0(15,4) == 0xE8C && INSN1(7,4) == BITS4(0,1,1,1)) {
      UInt rN  = INSN0(3,0);
      UInt rT  = INSN1(15,12);
      UInt rT2 = INSN1(11,8);
      UInt rD  = INSN1(3,0);
      if (!isBadRegT(rD) && !isBadRegT(rT) && !isBadRegT(rT2)
          && rN != 15 && rD != rN && rD != rT && rD != rT2) {
         IRTemp resSC1, resSC32, data;
         // go uncond
         mk_skip_over_T32_if_cond_is_false( condT );
         // now uncond
         /* Ok, now we're unconditional.  Do the store. */
         resSC1 = newTemp(Ity_I1);
         data = newTemp(Ity_I64);
         // FIXME: assumes little-endian guest
         assign(data, binop(Iop_32HLto64, getIRegT(rT2), getIRegT(rT)));
         // FIXME: assumes little-endian guest
         stmt( IRStmt_LLSC(Iend_LE, resSC1, getIRegT(rN), mkexpr(data)));
         /* Set rD to 1 on failure, 0 on success.  Currently we have
            resSC1 == 0 on failure, 1 on success. */
         resSC32 = newTemp(Ity_I32);
         assign(resSC32,
                unop(Iop_1Uto32, unop(Iop_Not1, mkexpr(resSC1))));
         putIRegT(rD, mkexpr(resSC32), IRTemp_INVALID);
         DIP("strexd r%u, r%u, r%u, [r%u]\n", rD, rT, rT2, rN);
         goto decode_success;
      }
   }

   /* -------------- v7 barrier insns -------------- */
   if (INSN0(15,0) == 0xF3BF && (INSN1(15,0) & 0xFF00) == 0x8F00) {
      /* FIXME: should this be unconditional? */
      /* XXX this isn't really right, is it?  The generated IR does
         them unconditionally.  I guess it doesn't matter since it
         doesn't do any harm to do them even when the guarding
         condition is false -- it's just a performance loss. */
      switch (INSN1(7,0)) {
         case 0x4F: /* DSB sy */
         case 0x4E: /* DSB st */
         case 0x4B: /* DSB ish */
         case 0x4A: /* DSB ishst */
         case 0x47: /* DSB nsh */
         case 0x46: /* DSB nshst */
         case 0x43: /* DSB osh */
         case 0x42: /* DSB oshst */
            stmt( IRStmt_MBE(Imbe_Fence) );
            DIP("DSB\n");
            goto decode_success;
         case 0x5F: /* DMB sy */
         case 0x5E: /* DMB st */
         case 0x5B: /* DMB ish */
         case 0x5A: /* DMB ishst */
         case 0x57: /* DMB nsh */
         case 0x56: /* DMB nshst */
         case 0x53: /* DMB osh */
         case 0x52: /* DMB oshst */
            stmt( IRStmt_MBE(Imbe_Fence) );
            DIP("DMB\n");
            goto decode_success;
         case 0x6F: /* ISB */
            stmt( IRStmt_MBE(Imbe_Fence) );
            DIP("ISB\n");
            goto decode_success;
         default:
            break;
      }
   }

   /* ---------------------- PLD{,W} ---------------------- */
   if ((INSN0(15,4) & 0xFFD) == 0xF89 && INSN1(15,12) == 0xF) {
      /* FIXME: should this be unconditional? */
      /* PLD/PLDW immediate, encoding T1 */
      UInt rN    = INSN0(3,0);
      UInt bW    = INSN0(5,5);
      UInt imm12 = INSN1(11,0);
      DIP("pld%s [r%u, #%u]\n", bW ? "w" : "",  rN, imm12);
      goto decode_success;
   }

   if ((INSN0(15,4) & 0xFFD) == 0xF81 && INSN1(15,8) == 0xFC) {
      /* FIXME: should this be unconditional? */
      /* PLD/PLDW immediate, encoding T2 */
      UInt rN    = INSN0(3,0);
      UInt bW    = INSN0(5,5);
      UInt imm8  = INSN1(7,0);
      DIP("pld%s [r%u, #-%u]\n", bW ? "w" : "",  rN, imm8);
      goto decode_success;
   }

   if ((INSN0(15,4) & 0xFFD) == 0xF81 && INSN1(15,6) == 0x3C0) {
      /* FIXME: should this be unconditional? */
      /* PLD/PLDW register, encoding T1 */
      UInt rN   = INSN0(3,0);
      UInt rM   = INSN1(3,0);
      UInt bW   = INSN0(5,5);
      UInt imm2 = INSN1(5,4);
      if (!isBadRegT(rM)) {
         DIP("pld%s [r%u, r%u, lsl %u]\n", bW ? "w" : "", rN, rM, imm2);
         goto decode_success;
      }
      /* fall through */
   }

   /* -------------- read CP15 TPIDRURO register ------------- */
   /* mrc     p15, 0,  r0, c13, c0, 3  up to
      mrc     p15, 0, r14, c13, c0, 3
   */
   /* I don't know whether this is really v7-only.  But anyway, we
      have to support it since arm-linux uses TPIDRURO as a thread
      state register. */
   if ((INSN0(15,0) == 0xEE1D) && (INSN1(11,0) == 0x0F70)) {
      UInt rD = INSN1(15,12);
      if (!isBadRegT(rD)) {
         putIRegT(rD, IRExpr_Get(OFFB_TPIDRURO, Ity_I32), condT);
         DIP("mrc p15,0, r%u, c13, c0, 3\n", rD);
         goto decode_success;
      }
      /* fall through */
   }

   /* -------------- read CP15 PMUSRENR register ------------- */
   /* mrc     p15, 0, r0,  c9, c14, 0  up to
      mrc     p15, 0, r14, c9, c14, 0
      See comment on the ARM equivalent of this (above) for details.
   */
   if ((INSN0(15,0) == 0xEE19) && (INSN1(11,0) == 0x0F1E)) {
      UInt rD = INSN1(15,12);
      if (!isBadRegT(rD)) {
         putIRegT(rD, mkU32(0), condT);
         DIP("mrc p15,0, r%u, c9, c14, 0\n", rD);
         goto decode_success;
      }
      /* fall through */
   }

   /* ------------------- CLREX ------------------ */
   if (INSN0(15,0) == 0xF3BF && INSN1(15,0) == 0x8F2F) {
      /* AFAICS, this simply cancels a (all?) reservations made by a
         (any?) preceding LDREX(es).  Arrange to hand it through to
         the back end. */
      mk_skip_over_T32_if_cond_is_false( condT );
      stmt( IRStmt_MBE(Imbe_CancelReservation) );
      DIP("clrex\n");
      goto decode_success;
   }

   /* ------------------- NOP ------------------ */
   if (INSN0(15,0) == 0xF3AF && INSN1(15,0) == 0x8000) {
      DIP("nop\n");
      goto decode_success;
   }

   /* -------------- (T1) LDRT reg+#imm8 -------------- */
   /* Load Register Unprivileged:
      ldrt Rt, [Rn, #imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,0,0,1) && INSN0(5,4) == BITS2(0,1)
       && INSN1(11,8) == BITS4(1,1,1,0)) {
      UInt rT    = INSN1(15,12);
      UInt rN    = INSN0(3,0);
      UInt imm8  = INSN1(7,0);
      Bool valid = True;
      if (rN == 15 || isBadRegT(rT)) valid = False;
      if (valid) {
         put_ITSTATE(old_itstate);
         IRExpr* ea = binop(Iop_Add32, getIRegT(rN), mkU32(imm8));
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt, ILGop_Ident32, ea, llGetIReg(rT), condT );
         putIRegT(rT, mkexpr(newRt), IRTemp_INVALID);
         put_ITSTATE(new_itstate);
         DIP("ldrt r%u, [r%u, #%u]\n", rT, rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T1) STRT reg+#imm8 -------------- */
   /* Store Register Unprivileged:
      strt Rt, [Rn, #imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,0,0,1) && INSN0(5,4) == BITS2(0,0)
       && INSN1(11,8) == BITS4(1,1,1,0)) {
      UInt rT    = INSN1(15,12);
      UInt rN    = INSN0(3,0);
      UInt imm8  = INSN1(7,0);
      Bool valid = True;
      if (rN == 15 || isBadRegT(rT)) valid = False;
      if (valid) {
         put_ITSTATE(old_itstate);
         IRExpr* address = binop(Iop_Add32, getIRegT(rN), mkU32(imm8));
         storeGuardedLE( address, llGetIReg(rT), condT );
         put_ITSTATE(new_itstate);
         DIP("strt r%u, [r%u, #%u]\n", rT, rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T1) STRBT reg+#imm8 -------------- */
   /* Store Register Byte Unprivileged:
      strbt Rt, [Rn, #imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,0,0,0) && INSN0(5,4) == BITS2(0,0)
       && INSN1(11,8) == BITS4(1,1,1,0)) {
      UInt rT    = INSN1(15,12);
      UInt rN    = INSN0(3,0);
      UInt imm8  = INSN1(7,0);
      Bool valid = True;
      if (rN == 15 || isBadRegT(rT)) valid = False;
      if (valid) {
         put_ITSTATE(old_itstate);
         IRExpr* address = binop(Iop_Add32, getIRegT(rN), mkU32(imm8));
         IRExpr* data = unop(Iop_32to8, llGetIReg(rT));
         storeGuardedLE( address, data, condT );
         put_ITSTATE(new_itstate);
         DIP("strbt r%u, [r%u, #%u]\n", rT, rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T1) LDRHT reg+#imm8 -------------- */
   /* Load Register Halfword Unprivileged:
      ldrht Rt, [Rn, #imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,0,0,0) && INSN0(5,4) == BITS2(1,1)
       && INSN1(11,8) == BITS4(1,1,1,0)) {
      UInt rN    = INSN0(3,0);
      Bool valid = True;
      if (rN == 15) {
         /* In this case our instruction is LDRH (literal), in fact:
            LDRH (literal) was realized earlier, so we don't want to
            make it twice. */
         valid = False;
      }
      UInt rT    = INSN1(15,12);
      UInt imm8  = INSN1(7,0);
      if (isBadRegT(rT)) valid = False;
      if (valid) {
         put_ITSTATE(old_itstate);
         IRExpr* ea = binop(Iop_Add32, getIRegT(rN), mkU32(imm8));
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt, ILGop_16Uto32, ea, llGetIReg(rT), condT );
         putIRegT(rT, mkexpr(newRt), IRTemp_INVALID);
         put_ITSTATE(new_itstate);
         DIP("ldrht r%u, [r%u, #%u]\n", rT, rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T1) LDRSHT reg+#imm8 -------------- */
   /* Load Register Signed Halfword Unprivileged:
      ldrsht Rt, [Rn, #imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,1,0,0) && INSN0(5,4) == BITS2(1,1)
       && INSN1(11,8) == BITS4(1,1,1,0)) {
      UInt rN    = INSN0(3,0);
      Bool valid = True;
      if (rN == 15) {
         /* In this case our instruction is LDRSH (literal), in fact:
            LDRSH (literal) was realized earlier, so we don't want to
            make it twice. */
         valid = False;
      }
      UInt rT    = INSN1(15,12);
      UInt imm8  = INSN1(7,0);
      if (isBadRegT(rT)) valid = False;
      if (valid) {
         put_ITSTATE(old_itstate);
         IRExpr* ea = binop(Iop_Add32, getIRegT(rN), mkU32(imm8));
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt, ILGop_16Sto32, ea, llGetIReg(rT), condT );
         putIRegT(rT, mkexpr(newRt), IRTemp_INVALID);
         put_ITSTATE(new_itstate);
         DIP("ldrsht r%u, [r%u, #%u]\n", rT, rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T1) STRHT reg+#imm8 -------------- */
   /* Store Register Halfword Unprivileged:
      strht Rt, [Rn, #imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,0,0,0) && INSN0(5,4) == BITS2(1,0)
       && INSN1(11,8) == BITS4(1,1,1,0)) {
      UInt rT    = INSN1(15,12);
      UInt rN    = INSN0(3,0);
      UInt imm8  = INSN1(7,0);
      Bool valid = True;
      if (rN == 15 || isBadRegT(rT)) valid = False;
      if (valid) {
         put_ITSTATE(old_itstate);
         IRExpr* address = binop(Iop_Add32, getIRegT(rN), mkU32(imm8));
         IRExpr* data = unop(Iop_32to16, llGetIReg(rT));
         storeGuardedLE( address, data, condT );
         put_ITSTATE(new_itstate);
         DIP("strht r%u, [r%u, #%u]\n", rT, rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T1) LDRBT reg+#imm8 -------------- */
   /* Load Register Byte Unprivileged:
      ldrbt Rt, [Rn, #imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,0,0,0) && INSN0(5,4) == BITS2(0,1)
       && INSN1(11,8) == BITS4(1,1,1,0)) {
      UInt rN    = INSN0(3,0);
      UInt rT    = INSN1(15,12);
      UInt imm8  = INSN1(7,0);
      Bool valid = True;
      if (rN == 15 /* insn is LDRB (literal) */) valid = False;
      if (isBadRegT(rT)) valid = False;
      if (valid) {
         put_ITSTATE(old_itstate);
         IRExpr* ea = binop(Iop_Add32, getIRegT(rN), mkU32(imm8));
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt, ILGop_8Uto32, ea, llGetIReg(rT), condT );
         putIRegT(rT, mkexpr(newRt), IRTemp_INVALID);
         put_ITSTATE(new_itstate);
         DIP("ldrbt r%u, [r%u, #%u]\n", rT, rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T1) LDRSBT reg+#imm8 -------------- */
   /* Load Register Signed Byte Unprivileged:
      ldrsbt Rt, [Rn, #imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,1,0,0) && INSN0(5,4) == BITS2(0,1)
       && INSN1(11,8) == BITS4(1,1,1,0)) {
      UInt rN    = INSN0(3,0);
      Bool valid = True;
      UInt rT    = INSN1(15,12);
      UInt imm8  = INSN1(7,0);
      if (rN == 15 /* insn is LDRSB (literal) */) valid = False;
      if (isBadRegT(rT)) valid = False;
      if (valid) {
         put_ITSTATE(old_itstate);
         IRExpr* ea = binop(Iop_Add32, getIRegT(rN), mkU32(imm8));
         IRTemp newRt = newTemp(Ity_I32);
         loadGuardedLE( newRt, ILGop_8Sto32, ea, llGetIReg(rT), condT );
         putIRegT(rT, mkexpr(newRt), IRTemp_INVALID);
         put_ITSTATE(new_itstate);
         DIP("ldrsbt r%u, [r%u, #%u]\n", rT, rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T1) PLI reg+#imm12 -------------- */
   /* Preload Instruction:
      pli [Rn, #imm12]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,1,1,0) && INSN0(5,4) == BITS2(0,1)
       && INSN1(15,12) == BITS4(1,1,1,1)) {
      UInt rN    = INSN0(3,0);
      UInt imm12 = INSN1(11,0);
      if (rN != 15) {
         DIP("pli [r%u, #%u]\n", rN, imm12);
         goto decode_success;
      }
   }

   /* -------------- (T2) PLI reg-#imm8 -------------- */
   /* Preload Instruction:
      pli [Rn, #-imm8]
   */
   if (INSN0(15,6) == BITS10(1,1,1,1,1,0,0,1,0,0) && INSN0(5,4) == BITS2(0,1)
       && INSN1(15,8) == BITS8(1,1,1,1,1,1,0,0)) {
      UInt rN   = INSN0(3,0);
      UInt imm8 = INSN1(7,0);
      if (rN != 15) {
         DIP("pli [r%u, #-%u]\n", rN, imm8);
         goto decode_success;
      }
   }

   /* -------------- (T3) PLI PC+/-#imm12 -------------- */
   /* Preload Instruction:
      pli [PC, #+/-imm12]
   */
   if (INSN0(15,8) == BITS8(1,1,1,1,1,0,0,1)
       && INSN0(6,0) == BITS7(0,0,1,1,1,1,1)
       && INSN1(15,12) == BITS4(1,1,1,1)) {
      UInt imm12 = INSN1(11,0);
      UInt bU    = INSN0(7,7);
      DIP("pli [pc, #%c%u]\n", bU == 1 ? '+' : '-', imm12);
      goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- VFP (CP 10, CP 11) instructions (in Thumb mode)       -- */
   /* ----------------------------------------------------------- */

   if (INSN0(15,12) == BITS4(1,1,1,0)) {
      UInt insn28 = (INSN0(11,0) << 16) | INSN1(15,0);
      Bool ok_vfp = decode_CP10_CP11_instruction (
                       &dres, insn28, condT, ARMCondAL/*bogus*/,
                       True/*isT*/
                    );
      if (ok_vfp)
         goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- NEON instructions (only v7 and below, in Thumb mode)  -- */
   /* ----------------------------------------------------------- */

   if (archinfo->hwcaps & VEX_HWCAPS_ARM_NEON) {
      UInt insn32 = (INSN0(15,0) << 16) | INSN1(15,0);
      Bool ok_neon = decode_NEON_instruction_ARMv7_and_below(
                        &dres, insn32, condT, True/*isT*/
                     );
      if (ok_neon)
         goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- v6 media instructions (in Thumb mode)                 -- */
   /* ----------------------------------------------------------- */

   { UInt insn32 = (INSN0(15,0) << 16) | INSN1(15,0);
     Bool ok_v6m = decode_V6MEDIA_instruction(
                      &dres, insn32, condT, ARMCondAL/*bogus*/,
                      True/*isT*/
                   );
     if (ok_v6m)
        goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- v8 instructions (in Thumb mode)                       -- */
   /* ----------------------------------------------------------- */

   /* If we get here, it means that all attempts to decode the
      instruction as ARMv7 or earlier have failed.  So, if we're doing
      ARMv8 or later, here is the point to try for it. */

   if (VEX_ARM_ARCHLEVEL(archinfo->hwcaps) >= 8) {
      UInt insn32 = (INSN0(15,0) << 16) | INSN1(15,0);
      Bool ok_v8
         = decode_V8_instruction( &dres, insn32, condT, True/*isT*/,
                                  old_itstate, new_itstate );
      if (ok_v8)
         goto decode_success;
   }

   /* ----------------------------------------------------------- */
   /* -- Undecodable                                           -- */
   /* ----------------------------------------------------------- */

   goto decode_failure;
   /*NOTREACHED*/

  decode_failure:
   /* All decode failures end up here. */
   if (sigill_diag)
      vex_printf("disInstr(thumb): unhandled instruction: "
                 "0x%04x 0x%04x\n", (UInt)insn0, (UInt)insn1);

   /* Back up ITSTATE to the initial value for this instruction.
      If we don't do that, any subsequent restart of the instruction
      will restart with the wrong value. */
   if (old_itstate != IRTemp_INVALID)
      put_ITSTATE(old_itstate);

   /* Tell the dispatcher that this insn cannot be decoded, and so has
      not been executed, and (is currently) the next to be executed.
      R15 should be up-to-date since it made so at the start of each
      insn, but nevertheless be paranoid and update it again right
      now. */
   vassert(0 == (guest_R15_curr_instr_notENC & 1));
   llPutIReg( 15, mkU32(guest_R15_curr_instr_notENC | 1) );
   dres.len         = 0;
   dres.whatNext    = Dis_StopHere;
   dres.jk_StopHere = Ijk_NoDecode;
   dres.continueAt  = 0;
   return dres;

  decode_success:
   /* All decode successes end up here. */
   vassert(dres.len == 4 || dres.len == 2 || dres.len == 20);
   switch (dres.whatNext) {
      case Dis_Continue:
         llPutIReg(15, mkU32(dres.len + (guest_R15_curr_instr_notENC | 1)));
         break;
      case Dis_ResteerU:
      case Dis_ResteerC:
         llPutIReg(15, mkU32(dres.continueAt));
         break;
      case Dis_StopHere:
         break;
      default:
         vassert(0);
   }

   DIP("\n");

   return dres;

#  undef INSN0
#  undef INSN1
}

#undef DIP
#undef DIS


/* Helper table for figuring out how many insns an IT insn
   conditionalises.

   An ITxyz instruction of the format "1011 1111 firstcond mask"
   conditionalises some number of instructions, as indicated by the
   following table.  A value of zero indicates the instruction is
   invalid in some way.

   mask = 0 means this isn't an IT instruction
   fc = 15 (NV) means unpredictable

   The line fc = 14 (AL) is different from the others; there are
   additional constraints in this case.

          mask(0 ..                   15)
        +--------------------------------
   fc(0 | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
   ..   | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 0 2 0 0 0 1 0 0 0 0 0 0 0 
   15)  | 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 

   To be conservative with the analysis, let's rule out the mask = 0
   case, since that isn't an IT insn at all.  But for all the other
   cases where the table contains zero, that means unpredictable, so
   let's say 4 to be conservative.  Hence we have a safe value for any
   IT (mask,fc) pair that the CPU would actually identify as an IT
   instruction.  The final table is

          mask(0 ..                   15)
        +--------------------------------
   fc(0 | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
   ..   | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 3 4 1 4 3 4 2 4 3 4 
        | 0 4 3 4 2 4 4 4 1 4 4 4 4 4 4 4 
   15)  | 0 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
*/
static const UChar it_length_table[256]
   = { 0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4, 
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 3, 4, 1, 4, 3, 4, 2, 4, 3, 4,
       0, 4, 3, 4, 2, 4, 4, 4, 1, 4, 4, 4, 4, 4, 4, 4,
       0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
     };


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult disInstr_ARM ( IRSB*        irsb_IN,
                         Bool         (*resteerOkFn) ( void*, Addr ),
                         Bool         resteerCisOk,
                         void*        callback_opaque,
                         const UChar* guest_code_IN,
                         Long         delta_ENCODED,
                         Addr         guest_IP_ENCODED,
                         VexArch      guest_arch,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo*  abiinfo,
                         VexEndness   host_endness_IN,
                         Bool         sigill_diag_IN )
{
   DisResult dres;
   Bool isThumb = (Bool)(guest_IP_ENCODED & 1);

   /* Set globals (see top of this file) */
   vassert(guest_arch == VexArchARM);

   irsb            = irsb_IN;
   host_endness    = host_endness_IN;
   __curr_is_Thumb = isThumb;

   if (isThumb) {
      guest_R15_curr_instr_notENC = (Addr32)guest_IP_ENCODED - 1;
   } else {
      guest_R15_curr_instr_notENC = (Addr32)guest_IP_ENCODED;
   }

   if (isThumb) {
      dres = disInstr_THUMB_WRK ( resteerOkFn,
                                  resteerCisOk, callback_opaque,
                                  &guest_code_IN[delta_ENCODED - 1],
                                  archinfo, abiinfo, sigill_diag_IN );
   } else {
      dres = disInstr_ARM_WRK ( resteerOkFn,
                                resteerCisOk, callback_opaque,
                                &guest_code_IN[delta_ENCODED],
                                archinfo, abiinfo, sigill_diag_IN );
   }

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

/* Spare code for doing reference implementations of various 64-bit
   SIMD interleaves/deinterleaves/concatenation ops. */
/*
// Split a 64 bit value into 4 16 bit ones, in 32-bit IRTemps with
// the top halves guaranteed to be zero.
static void break64to16s ( IRTemp* out3, IRTemp* out2, IRTemp* out1,
                           IRTemp* out0, IRTemp v64 )
{
  if (out3) *out3 = newTemp(Ity_I32);
  if (out2) *out2 = newTemp(Ity_I32);
  if (out1) *out1 = newTemp(Ity_I32);
  if (out0) *out0 = newTemp(Ity_I32);
  IRTemp hi32 = newTemp(Ity_I32);
  IRTemp lo32 = newTemp(Ity_I32);
  assign(hi32, unop(Iop_64HIto32, mkexpr(v64)) );
  assign(lo32, unop(Iop_64to32, mkexpr(v64)) );
  if (out3) assign(*out3, binop(Iop_Shr32, mkexpr(hi32), mkU8(16)));
  if (out2) assign(*out2, binop(Iop_And32, mkexpr(hi32), mkU32(0xFFFF)));
  if (out1) assign(*out1, binop(Iop_Shr32, mkexpr(lo32), mkU8(16)));
  if (out0) assign(*out0, binop(Iop_And32, mkexpr(lo32), mkU32(0xFFFF)));
}

// Make a 64 bit value from 4 16 bit ones, each of which is in a 32 bit
// IRTemp.
static IRTemp mk64from16s ( IRTemp in3, IRTemp in2, IRTemp in1, IRTemp in0 )
{
  IRTemp hi32 = newTemp(Ity_I32);
  IRTemp lo32 = newTemp(Ity_I32);
  assign(hi32,
         binop(Iop_Or32,
               binop(Iop_Shl32, mkexpr(in3), mkU8(16)),
               binop(Iop_And32, mkexpr(in2), mkU32(0xFFFF))));
  assign(lo32,
         binop(Iop_Or32,
               binop(Iop_Shl32, mkexpr(in1), mkU8(16)),
               binop(Iop_And32, mkexpr(in0), mkU32(0xFFFF))));
  IRTemp res = newTemp(Ity_I64);
  assign(res, binop(Iop_32HLto64, mkexpr(hi32), mkexpr(lo32)));
  return res;
}

static IRExpr* mk_InterleaveLO16x4 ( IRTemp a3210, IRTemp b3210 )
{
  // returns a1 b1 a0 b0
  IRTemp a1, a0, b1, b0;
  break64to16s(NULL, NULL, &a1, &a0, a3210);
  break64to16s(NULL, NULL, &b1, &b0, b3210);
  return mkexpr(mk64from16s(a1, b1, a0, b0));
}

static IRExpr* mk_InterleaveHI16x4 ( IRTemp a3210, IRTemp b3210 )
{
  // returns a3 b3 a2 b2
  IRTemp a3, a2, b3, b2;
  break64to16s(&a3, &a2, NULL, NULL, a3210);
  break64to16s(&b3, &b2, NULL, NULL, b3210);
  return mkexpr(mk64from16s(a3, b3, a2, b2));
}

static IRExpr* mk_CatEvenLanes16x4 ( IRTemp a3210, IRTemp b3210 )
{
  // returns a2 a0 b2 b0
  IRTemp a2, a0, b2, b0;
  break64to16s(NULL, &a2, NULL, &a0, a3210);
  break64to16s(NULL, &b2, NULL, &b0, b3210);
  return mkexpr(mk64from16s(a2, a0, b2, b0));
}

static IRExpr* mk_CatOddLanes16x4 ( IRTemp a3210, IRTemp b3210 )
{
  // returns a3 a1 b3 b1
  IRTemp a3, a1, b3, b1;
  break64to16s(&a3, NULL, &a1, NULL, a3210);
  break64to16s(&b3, NULL, &b1, NULL, b3210);
  return mkexpr(mk64from16s(a3, a1, b3, b1));
}

static IRExpr* mk_InterleaveOddLanes16x4 ( IRTemp a3210, IRTemp b3210 )
{
  // returns a3 b3 a1 b1
  IRTemp a3, b3, a1, b1;
  break64to16s(&a3, NULL, &a1, NULL, a3210);
  break64to16s(&b3, NULL, &b1, NULL, b3210);
  return mkexpr(mk64from16s(a3, b3, a1, b1));
}

static IRExpr* mk_InterleaveEvenLanes16x4 ( IRTemp a3210, IRTemp b3210 )
{
  // returns a2 b2 a0 b0
  IRTemp a2, b2, a0, b0;
  break64to16s(NULL, &a2, NULL, &a0, a3210);
  break64to16s(NULL, &b2, NULL, &b0, b3210);
  return mkexpr(mk64from16s(a2, b2, a0, b0));
}

static void break64to8s ( IRTemp* out7, IRTemp* out6, IRTemp* out5,
                          IRTemp* out4, IRTemp* out3, IRTemp* out2,
                          IRTemp* out1,IRTemp* out0, IRTemp v64 )
{
  if (out7) *out7 = newTemp(Ity_I32);
  if (out6) *out6 = newTemp(Ity_I32);
  if (out5) *out5 = newTemp(Ity_I32);
  if (out4) *out4 = newTemp(Ity_I32);
  if (out3) *out3 = newTemp(Ity_I32);
  if (out2) *out2 = newTemp(Ity_I32);
  if (out1) *out1 = newTemp(Ity_I32);
  if (out0) *out0 = newTemp(Ity_I32);
  IRTemp hi32 = newTemp(Ity_I32);
  IRTemp lo32 = newTemp(Ity_I32);
  assign(hi32, unop(Iop_64HIto32, mkexpr(v64)) );
  assign(lo32, unop(Iop_64to32, mkexpr(v64)) );
  if (out7)
    assign(*out7, binop(Iop_And32,
                        binop(Iop_Shr32, mkexpr(hi32), mkU8(24)),
                        mkU32(0xFF)));
  if (out6)
    assign(*out6, binop(Iop_And32,
                        binop(Iop_Shr32, mkexpr(hi32), mkU8(16)),
                        mkU32(0xFF)));
  if (out5)
    assign(*out5, binop(Iop_And32,
                        binop(Iop_Shr32, mkexpr(hi32), mkU8(8)),
                        mkU32(0xFF)));
  if (out4)
    assign(*out4, binop(Iop_And32, mkexpr(hi32), mkU32(0xFF)));
  if (out3)
    assign(*out3, binop(Iop_And32,
                        binop(Iop_Shr32, mkexpr(lo32), mkU8(24)),
                        mkU32(0xFF)));
  if (out2)
    assign(*out2, binop(Iop_And32,
                        binop(Iop_Shr32, mkexpr(lo32), mkU8(16)),
                        mkU32(0xFF)));
  if (out1)
    assign(*out1, binop(Iop_And32,
                        binop(Iop_Shr32, mkexpr(lo32), mkU8(8)),
                        mkU32(0xFF)));
  if (out0)
    assign(*out0, binop(Iop_And32, mkexpr(lo32), mkU32(0xFF)));
}

static IRTemp mk64from8s ( IRTemp in7, IRTemp in6, IRTemp in5, IRTemp in4,
                           IRTemp in3, IRTemp in2, IRTemp in1, IRTemp in0 )
{
  IRTemp hi32 = newTemp(Ity_I32);
  IRTemp lo32 = newTemp(Ity_I32);
  assign(hi32,
         binop(Iop_Or32,
               binop(Iop_Or32,
                     binop(Iop_Shl32,
                           binop(Iop_And32, mkexpr(in7), mkU32(0xFF)),
                           mkU8(24)),
                     binop(Iop_Shl32,
                           binop(Iop_And32, mkexpr(in6), mkU32(0xFF)),
                           mkU8(16))),
               binop(Iop_Or32,
                     binop(Iop_Shl32,
                           binop(Iop_And32, mkexpr(in5), mkU32(0xFF)), mkU8(8)),
                     binop(Iop_And32,
                           mkexpr(in4), mkU32(0xFF)))));
  assign(lo32,
         binop(Iop_Or32,
               binop(Iop_Or32,
                     binop(Iop_Shl32,
                           binop(Iop_And32, mkexpr(in3), mkU32(0xFF)),
                           mkU8(24)),
                     binop(Iop_Shl32,
                           binop(Iop_And32, mkexpr(in2), mkU32(0xFF)),
                           mkU8(16))),
               binop(Iop_Or32,
                     binop(Iop_Shl32,
                           binop(Iop_And32, mkexpr(in1), mkU32(0xFF)), mkU8(8)),
                     binop(Iop_And32,
                           mkexpr(in0), mkU32(0xFF)))));
  IRTemp res = newTemp(Ity_I64);
  assign(res, binop(Iop_32HLto64, mkexpr(hi32), mkexpr(lo32)));
  return res;
}

static IRExpr* mk_InterleaveLO8x8 ( IRTemp a76543210, IRTemp b76543210 )
{
  // returns a3 b3 a2 b2 a1 b1 a0 b0
  IRTemp a3, b3, a2, b2, a1, a0, b1, b0;
  break64to8s(NULL, NULL, NULL, NULL, &a3, &a2, &a1, &a0, a76543210);
  break64to8s(NULL, NULL, NULL, NULL, &b3, &b2, &b1, &b0, b76543210);
  return mkexpr(mk64from8s(a3, b3, a2, b2, a1, b1, a0, b0));
}

static IRExpr* mk_InterleaveHI8x8 ( IRTemp a76543210, IRTemp b76543210 )
{
  // returns a7 b7 a6 b6 a5 b5 a4 b4
  IRTemp a7, b7, a6, b6, a5, b5, a4, b4;
  break64to8s(&a7, &a6, &a5, &a4, NULL, NULL, NULL, NULL, a76543210);
  break64to8s(&b7, &b6, &b5, &b4, NULL, NULL, NULL, NULL, b76543210);
  return mkexpr(mk64from8s(a7, b7, a6, b6, a5, b5, a4, b4));
}

static IRExpr* mk_CatEvenLanes8x8 ( IRTemp a76543210, IRTemp b76543210 )
{
  // returns a6 a4 a2 a0 b6 b4 b2 b0
  IRTemp a6, a4, a2, a0, b6, b4, b2, b0;
  break64to8s(NULL, &a6, NULL, &a4, NULL, &a2, NULL, &a0, a76543210);
  break64to8s(NULL, &b6, NULL, &b4, NULL, &b2, NULL, &b0, b76543210);
  return mkexpr(mk64from8s(a6, a4, a2, a0, b6, b4, b2, b0));
}

static IRExpr* mk_CatOddLanes8x8 ( IRTemp a76543210, IRTemp b76543210 )
{
  // returns a7 a5 a3 a1 b7 b5 b3 b1
  IRTemp a7, a5, a3, a1, b7, b5, b3, b1;
  break64to8s(&a7, NULL, &a5, NULL, &a3, NULL, &a1, NULL, a76543210);
  break64to8s(&b7, NULL, &b5, NULL, &b3, NULL, &b1, NULL, b76543210);
  return mkexpr(mk64from8s(a7, a5, a3, a1, b7, b5, b3, b1));
}

static IRExpr* mk_InterleaveEvenLanes8x8 ( IRTemp a76543210, IRTemp b76543210 )
{
  // returns a6 b6 a4 b4 a2 b2 a0 b0
  IRTemp a6, b6, a4, b4, a2, b2, a0, b0;
  break64to8s(NULL, &a6, NULL, &a4, NULL, &a2, NULL, &a0, a76543210);
  break64to8s(NULL, &b6, NULL, &b4, NULL, &b2, NULL, &b0, b76543210);
  return mkexpr(mk64from8s(a6, b6, a4, b4, a2, b2, a0, b0));
}

static IRExpr* mk_InterleaveOddLanes8x8 ( IRTemp a76543210, IRTemp b76543210 )
{
  // returns a7 b7 a5 b5 a3 b3 a1 b1
  IRTemp a7, b7, a5, b5, a3, b3, a1, b1;
  break64to8s(&a7, NULL, &a5, NULL, &a3, NULL, &a1, NULL, a76543210);
  break64to8s(&b7, NULL, &b5, NULL, &b3, NULL, &b1, NULL, b76543210);
  return mkexpr(mk64from8s(a7, b7, a5, b5, a3, b3, a1, b1));
}

static IRExpr* mk_InterleaveLO32x2 ( IRTemp a10, IRTemp b10 )
{
  // returns a0 b0
  return binop(Iop_32HLto64, unop(Iop_64to32, mkexpr(a10)),
                             unop(Iop_64to32, mkexpr(b10)));
}

static IRExpr* mk_InterleaveHI32x2 ( IRTemp a10, IRTemp b10 )
{
  // returns a1 b1
  return binop(Iop_32HLto64, unop(Iop_64HIto32, mkexpr(a10)),
                             unop(Iop_64HIto32, mkexpr(b10)));
}
*/

/*--------------------------------------------------------------------*/
/*--- end                                         guest_arm_toIR.c ---*/
/*--------------------------------------------------------------------*/
