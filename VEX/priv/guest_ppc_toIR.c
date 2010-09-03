
/*--------------------------------------------------------------------*/
/*--- begin                                       guest_ppc_toIR.c ---*/
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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* TODO 18/Nov/05:

   Spot rld... cases which are simply left/right shifts and emit
   Shl64/Shr64 accordingly.

   Altivec
   - datastream insns
   - lvxl,stvxl: load/store with 'least recently used' hint
   - vexptefp, vlogefp

   LIMITATIONS:

   Various, including:

   - Some invalid forms of lswi and lswx are accepted when they should
     not be.

   - Floating Point:
     - All exceptions disabled in FPSCR
     - condition codes not set in FPSCR

   - Altivec floating point:
     - vmaddfp, vnmsubfp
       Because we're using Java/IEEE mode (FPSCR[NJ]), rather than the
       system default of Non-Java mode, we get some small errors
       (lowest bit only).
       This is because Non-Java mode brutally hacks denormalised results
       to zero, whereas we keep maximum accuracy.  However, using
       Non-Java mode would give us more inaccuracy, as our intermediate
       results would then be zeroed, too.

   - AbiHints for the stack red zone are only emitted for
       unconditional calls and returns (bl, blr).  They should also be
       emitted for conditional calls and returns, but we don't have a 
       way to express that right now.  Ah well.
*/

/* "Special" instructions.

   This instruction decoder can decode four special instructions
   which mean nothing natively (are no-ops as far as regs/mem are
   concerned) but have meaning for supporting Valgrind.  A special
   instruction is flagged by a 16-byte preamble:

      32-bit mode: 54001800 54006800 5400E800 54009800
                   (rlwinm 0,0,3,0,0; rlwinm 0,0,13,0,0; 
                    rlwinm 0,0,29,0,0; rlwinm 0,0,19,0,0)

      64-bit mode: 78001800 78006800 7800E802 78009802
                   (rotldi 0,0,3; rotldi 0,0,13;
                    rotldi 0,0,61; rotldi 0,0,51)

   Following that, one of the following 3 are allowed
   (standard interpretation in parentheses):

      7C210B78 (or 1,1,1)   %R3 = client_request ( %R4 )
      7C421378 (or 2,2,2)   %R3 = guest_NRADDR
      7C631B78 (or 3,3,3)   branch-and-link-to-noredir %R11
      7C842378 (or 4,4,4)   %R3 = guest_NRADDR_GPR2

   Any other bytes following the 16-byte preamble are illegal and
   constitute a failure in instruction decoding.  This all assumes
   that the preamble will never occur except in specific code
   fragments designed for Valgrind to catch.
*/


/* Translates PPC32/64 code to IR. */

/* References

#define PPC32
   "PowerPC Microprocessor Family:
    The Programming Environments Manual for 32-Bit Microprocessors"
    02/21/2000
    http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/852569B20050FF778525699600719DF2

#define PPC64
   "PowerPC Microprocessor Family:
    Programming Environments Manual for 64-Bit Microprocessors"
    06/10/2003
   http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/F7E732FF811F783187256FDD004D3797

#define AV
   "PowerPC Microprocessor Family:
    AltiVec(TM) Technology Programming Environments Manual"
    07/10/2003
   http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/FBFA164F824370F987256D6A006F424D
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_ppc32.h"
#include "libvex_guest_ppc64.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_ppc_defs.h"


/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of an insn, right
   down in disInstr_PPC, so that we don't have to pass them around
   endlessly.  They are all constant during the translation of any
   given insn. */

/* We need to know this to do sub-register accesses correctly. */
static Bool host_is_bigendian;

/* Pointer to the guest code area. */
static UChar* guest_code;

/* The guest address corresponding to guest_code[0]. */
static Addr64 guest_CIA_bbstart;

/* The guest address for the instruction currently being
   translated. */
static Addr64 guest_CIA_curr_instr;

/* The IRSB* into which we're generating code. */
static IRSB* irsb;

/* Is our guest binary 32 or 64bit?  Set at each call to
   disInstr_PPC below. */
static Bool mode64 = False;

// Given a pointer to a function as obtained by "& functionname" in C,
// produce a pointer to the actual entry point for the function.  For
// most platforms it's the identity function.  Unfortunately, on
// ppc64-linux it isn't (sigh) and ditto for ppc32-aix5 and
// ppc64-aix5.
static void* fnptr_to_fnentry( VexAbiInfo* vbi, void* f )
{
   if (vbi->host_ppc_calls_use_fndescrs) {
      /* f is a pointer to a 3-word function descriptor, of which the
         first word is the entry address. */
      /* note, this is correct even with cross-jitting, since this is
         purely a host issue, not a guest one. */
      HWord* fdescr = (HWord*)f;
      return (void*)(fdescr[0]);
   } else {
      /* Simple; "& f" points directly at the code for f. */
      return f;
   }
}


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
/*--- Offsets of various parts of the ppc32/64 guest state ---*/
/*------------------------------------------------------------*/

#define offsetofPPCGuestState(_x) \
   (mode64 ? offsetof(VexGuestPPC64State, _x) : \
             offsetof(VexGuestPPC32State, _x))

#define OFFB_CIA         offsetofPPCGuestState(guest_CIA)
#define OFFB_IP_AT_SYSCALL offsetofPPCGuestState(guest_IP_AT_SYSCALL)
#define OFFB_SPRG3_RO    offsetofPPCGuestState(guest_SPRG3_RO)
#define OFFB_LR          offsetofPPCGuestState(guest_LR)
#define OFFB_CTR         offsetofPPCGuestState(guest_CTR)
#define OFFB_XER_SO      offsetofPPCGuestState(guest_XER_SO)
#define OFFB_XER_OV      offsetofPPCGuestState(guest_XER_OV)
#define OFFB_XER_CA      offsetofPPCGuestState(guest_XER_CA)
#define OFFB_XER_BC      offsetofPPCGuestState(guest_XER_BC)
#define OFFB_FPROUND     offsetofPPCGuestState(guest_FPROUND)
#define OFFB_VRSAVE      offsetofPPCGuestState(guest_VRSAVE)
#define OFFB_VSCR        offsetofPPCGuestState(guest_VSCR)
#define OFFB_EMWARN      offsetofPPCGuestState(guest_EMWARN)
#define OFFB_TISTART     offsetofPPCGuestState(guest_TISTART)
#define OFFB_TILEN       offsetofPPCGuestState(guest_TILEN)
#define OFFB_NRADDR      offsetofPPCGuestState(guest_NRADDR)
#define OFFB_NRADDR_GPR2 offsetofPPCGuestState(guest_NRADDR_GPR2)


/*------------------------------------------------------------*/
/*--- Extract instruction fields                          --- */
/*------------------------------------------------------------*/

/* Extract field from insn, given idx (zero = lsb) and field length */
#define IFIELD( insn, idx, len ) ((insn >> idx) & ((1<<len)-1))

/* Extract primary opcode, instr[31:26] */
static UChar ifieldOPC( UInt instr ) {
   return toUChar( IFIELD( instr, 26, 6 ) );
}

/* Extract 10-bit secondary opcode, instr[10:1] */
static UInt ifieldOPClo10 ( UInt instr) {
   return IFIELD( instr, 1, 10 );
}

/* Extract 9-bit secondary opcode, instr[9:1] */
static UInt ifieldOPClo9 ( UInt instr) {
   return IFIELD( instr, 1, 9 );
}

/* Extract 5-bit secondary opcode, instr[5:1] */
static UInt ifieldOPClo5 ( UInt instr) {
   return IFIELD( instr, 1, 5 );
}

/* Extract RD (destination register) field, instr[25:21] */
static UChar ifieldRegDS( UInt instr ) {
   return toUChar( IFIELD( instr, 21, 5 ) );
}

/* Extract RA (1st source register) field, instr[20:16] */
static UChar ifieldRegA ( UInt instr ) {
   return toUChar( IFIELD( instr, 16, 5 ) );
}

/* Extract RB (2nd source register) field, instr[15:11] */
static UChar ifieldRegB ( UInt instr ) {
   return toUChar( IFIELD( instr, 11, 5 ) );
}

/* Extract RC (3rd source register) field, instr[10:6] */
static UChar ifieldRegC ( UInt instr ) {
   return toUChar( IFIELD( instr, 6, 5 ) );
}

/* Extract 2nd lowest bit, instr[1] */
static UChar ifieldBIT10 ( UInt instr ) {
   return toUChar( IFIELD( instr, 10, 1 ) );
}

/* Extract 2nd lowest bit, instr[1] */
static UChar ifieldBIT1 ( UInt instr ) {
   return toUChar( IFIELD( instr, 1, 1 ) );
}

/* Extract lowest bit, instr[0] */
static UChar ifieldBIT0 ( UInt instr ) {
   return toUChar( instr & 0x1 );
}

/* Extract unsigned bottom half, instr[15:0] */
static UInt ifieldUIMM16 ( UInt instr ) {
   return instr & 0xFFFF;
}

/* Extract unsigned bottom 26 bits, instr[25:0] */
static UInt ifieldUIMM26 ( UInt instr ) {
   return instr & 0x3FFFFFF;
}


/*------------------------------------------------------------*/
/*--- Guest-state identifiers                              ---*/
/*------------------------------------------------------------*/

typedef enum {
    PPC_GST_CIA,    // Current Instruction Address
    PPC_GST_LR,     // Link Register
    PPC_GST_CTR,    // Count Register
    PPC_GST_XER,    // Overflow, carry flags, byte count
    PPC_GST_CR,     // Condition Register
    PPC_GST_FPSCR,  // Floating Point Status/Control Register
    PPC_GST_VRSAVE, // Vector Save/Restore Register
    PPC_GST_VSCR,   // Vector Status and Control Register
    PPC_GST_EMWARN, // Emulation warnings
    PPC_GST_TISTART,// For icbi: start of area to invalidate
    PPC_GST_TILEN,  // For icbi: length of area to invalidate
    PPC_GST_IP_AT_SYSCALL, // the CIA of the most recently executed SC insn
    PPC_GST_SPRG3_RO, // SPRG3
    PPC_GST_MAX
} PPC_GST;

#define MASK_FPSCR_RN   0x3
#define MASK_FPSCR_FPRF 0x1F000
#define MASK_VSCR_VALID 0x00010001


/*------------------------------------------------------------*/
/*---  FP Helpers                                          ---*/
/*------------------------------------------------------------*/

/* Produce the 32-bit pattern corresponding to the supplied
   float. */
static UInt float_to_bits ( Float f )
{
   union { UInt i; Float f; } u;
   vassert(4 == sizeof(UInt));
   vassert(4 == sizeof(Float));
   vassert(4 == sizeof(u));
   u.f = f;
   return u.i;
}


/*------------------------------------------------------------*/
/*--- Misc Helpers                                         ---*/
/*------------------------------------------------------------*/

/* Generate mask with 1's from 'begin' through 'end',
   wrapping if begin > end.
   begin->end works from right to left, 0=lsb
*/
static UInt MASK32( UInt begin, UInt end )
{
   UInt m1, m2, mask;
   vassert(begin < 32);
   vassert(end < 32);
   m1   = ((UInt)(-1)) << begin;
   m2   = ((UInt)(-1)) << end << 1;
   mask = m1 ^ m2;
   if (begin > end) mask = ~mask;  // wrap mask
   return mask;
}

/* ditto for 64bit mask */
static ULong MASK64( UInt begin, UInt end )
{
   ULong m1, m2, mask;
   vassert(begin < 64);
   vassert(end < 64);
   m1   = ((ULong)(-1)) << begin;
   m2   = ((ULong)(-1)) << end << 1;
   mask = m1 ^ m2;
   if (begin > end) mask = ~mask;  // wrap mask
   return mask;
}

static Addr64 nextInsnAddr( void )
{
   return guest_CIA_curr_instr + 4;
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- ppc32/64 insn stream.                                ---*/
/*------------------------------------------------------------*/

/* Add a statement to the list held by "irsb". */
static void stmt ( IRStmt* st )
{
   addStmtToIRSB( irsb, st );
}

/* Generate a new temporary of the given type. */
static IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp( irsb->tyenv, ty );
}

/* Various simple conversions */

static UChar extend_s_5to8 ( UChar x )
{
   return toUChar((((Int)x) << 27) >> 27);
}

static UInt extend_s_8to32( UChar x )
{
   return (UInt)((((Int)x) << 24) >> 24);
}

static UInt extend_s_16to32 ( UInt x )
{
   return (UInt)((((Int)x) << 16) >> 16);
}

static ULong extend_s_16to64 ( UInt x )
{
   return (ULong)((((Long)x) << 48) >> 48);
}

static ULong extend_s_26to64 ( UInt x )
{
   return (ULong)((((Long)x) << 38) >> 38);
}

static ULong extend_s_32to64 ( UInt x )
{
   return (ULong)((((Long)x) << 32) >> 32);
}

/* Do a big-endian load of a 32-bit word, regardless of the endianness
   of the underlying host. */
static UInt getUIntBigendianly ( UChar* p )
{
   UInt w = 0;
   w = (w << 8) | p[0];
   w = (w << 8) | p[1];
   w = (w << 8) | p[2];
   w = (w << 8) | p[3];
   return w;
}


/*------------------------------------------------------------*/
/*--- Helpers for constructing IR.                         ---*/
/*------------------------------------------------------------*/

static void assign ( IRTemp dst, IRExpr* e )
{
   stmt( IRStmt_WrTmp(dst, e) );
}

/* This generates a normal (non store-conditional) store. */
static void storeBE ( IRExpr* addr, IRExpr* data )
{
   IRType tyA = typeOfIRExpr(irsb->tyenv, addr);
   vassert(tyA == Ity_I32 || tyA == Ity_I64);
   stmt( IRStmt_Store(Iend_BE, addr, data) );
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

static IRExpr* qop ( IROp op, IRExpr* a1, IRExpr* a2, 
                              IRExpr* a3, IRExpr* a4 )
{
   return IRExpr_Qop(op, a1, a2, a3, a4);
}

static IRExpr* mkexpr ( IRTemp tmp )
{
   return IRExpr_RdTmp(tmp);
}

static IRExpr* mkU8 ( UChar i )
{
   return IRExpr_Const(IRConst_U8(i));
}

static IRExpr* mkU16 ( UInt i )
{
   return IRExpr_Const(IRConst_U16(i));
}

static IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static IRExpr* mkU64 ( ULong i )
{
   return IRExpr_Const(IRConst_U64(i));
}

/* This generates a normal (non load-linked) load. */
static IRExpr* loadBE ( IRType ty, IRExpr* addr )
{
   return IRExpr_Load(Iend_BE, ty, addr);
}

static IRExpr* mkOR1 ( IRExpr* arg1, IRExpr* arg2 )
{
   vassert(typeOfIRExpr(irsb->tyenv, arg1) == Ity_I1);
   vassert(typeOfIRExpr(irsb->tyenv, arg2) == Ity_I1);
   return unop(Iop_32to1, binop(Iop_Or32, unop(Iop_1Uto32, arg1), 
                                          unop(Iop_1Uto32, arg2)));
}

static IRExpr* mkAND1 ( IRExpr* arg1, IRExpr* arg2 )
{
   vassert(typeOfIRExpr(irsb->tyenv, arg1) == Ity_I1);
   vassert(typeOfIRExpr(irsb->tyenv, arg2) == Ity_I1);
   return unop(Iop_32to1, binop(Iop_And32, unop(Iop_1Uto32, arg1), 
                                           unop(Iop_1Uto32, arg2)));
}

/* expand V128_8Ux16 to 2x V128_16Ux8's */
static void expand8Ux16( IRExpr* vIn,
                         /*OUTs*/ IRTemp* vEvn, IRTemp* vOdd )
{
   IRTemp ones8x16 = newTemp(Ity_V128);

   vassert(typeOfIRExpr(irsb->tyenv, vIn) == Ity_V128);
   vassert(vEvn && *vEvn == IRTemp_INVALID);
   vassert(vOdd && *vOdd == IRTemp_INVALID);
   *vEvn = newTemp(Ity_V128);
   *vOdd = newTemp(Ity_V128);

   assign( ones8x16, unop(Iop_Dup8x16, mkU8(0x1)) );
   assign( *vOdd, binop(Iop_MullEven8Ux16, mkexpr(ones8x16), vIn) );
   assign( *vEvn, binop(Iop_MullEven8Ux16, mkexpr(ones8x16), 
                        binop(Iop_ShrV128, vIn, mkU8(8))) );
}

/* expand V128_8Sx16 to 2x V128_16Sx8's */
static void expand8Sx16( IRExpr* vIn,
                         /*OUTs*/ IRTemp* vEvn, IRTemp* vOdd )
{
   IRTemp ones8x16 = newTemp(Ity_V128);

   vassert(typeOfIRExpr(irsb->tyenv, vIn) == Ity_V128);
   vassert(vEvn && *vEvn == IRTemp_INVALID);
   vassert(vOdd && *vOdd == IRTemp_INVALID);
   *vEvn = newTemp(Ity_V128);
   *vOdd = newTemp(Ity_V128);

   assign( ones8x16, unop(Iop_Dup8x16, mkU8(0x1)) );
   assign( *vOdd, binop(Iop_MullEven8Sx16, mkexpr(ones8x16), vIn) );
   assign( *vEvn, binop(Iop_MullEven8Sx16, mkexpr(ones8x16), 
                        binop(Iop_ShrV128, vIn, mkU8(8))) );
}

/* expand V128_16Uto8 to 2x V128_32Ux4's */
static void expand16Ux8( IRExpr* vIn,
                         /*OUTs*/ IRTemp* vEvn, IRTemp* vOdd )
{
   IRTemp ones16x8 = newTemp(Ity_V128);

   vassert(typeOfIRExpr(irsb->tyenv, vIn) == Ity_V128);
   vassert(vEvn && *vEvn == IRTemp_INVALID);
   vassert(vOdd && *vOdd == IRTemp_INVALID);
   *vEvn = newTemp(Ity_V128);
   *vOdd = newTemp(Ity_V128);

   assign( ones16x8, unop(Iop_Dup16x8, mkU16(0x1)) );
   assign( *vOdd, binop(Iop_MullEven16Ux8, mkexpr(ones16x8), vIn) );
   assign( *vEvn, binop(Iop_MullEven16Ux8, mkexpr(ones16x8), 
                        binop(Iop_ShrV128, vIn, mkU8(16))) );
}

/* expand V128_16Sto8 to 2x V128_32Sx4's */
static void expand16Sx8( IRExpr* vIn,
                         /*OUTs*/ IRTemp* vEvn, IRTemp* vOdd )
{
   IRTemp ones16x8 = newTemp(Ity_V128);

   vassert(typeOfIRExpr(irsb->tyenv, vIn) == Ity_V128);
   vassert(vEvn && *vEvn == IRTemp_INVALID);
   vassert(vOdd && *vOdd == IRTemp_INVALID);
   *vEvn = newTemp(Ity_V128);
   *vOdd = newTemp(Ity_V128);

   assign( ones16x8, unop(Iop_Dup16x8, mkU16(0x1)) );
   assign( *vOdd, binop(Iop_MullEven16Sx8, mkexpr(ones16x8), vIn) );
   assign( *vEvn, binop(Iop_MullEven16Sx8, mkexpr(ones16x8), 
                       binop(Iop_ShrV128, vIn, mkU8(16))) );
}

/* break V128 to 4xI32's, then sign-extend to I64's */
static void breakV128to4x64S( IRExpr* t128,
                              /*OUTs*/
                              IRTemp* t3, IRTemp* t2,
                              IRTemp* t1, IRTemp* t0 )
{
   IRTemp hi64 = newTemp(Ity_I64);
   IRTemp lo64 = newTemp(Ity_I64);

   vassert(typeOfIRExpr(irsb->tyenv, t128) == Ity_V128);
   vassert(t0 && *t0 == IRTemp_INVALID);
   vassert(t1 && *t1 == IRTemp_INVALID);
   vassert(t2 && *t2 == IRTemp_INVALID);
   vassert(t3 && *t3 == IRTemp_INVALID);
   *t0 = newTemp(Ity_I64);
   *t1 = newTemp(Ity_I64);
   *t2 = newTemp(Ity_I64);
   *t3 = newTemp(Ity_I64);

   assign( hi64, unop(Iop_V128HIto64, t128) );
   assign( lo64, unop(Iop_V128to64,   t128) );
   assign( *t3, unop(Iop_32Sto64, unop(Iop_64HIto32, mkexpr(hi64))) );
   assign( *t2, unop(Iop_32Sto64, unop(Iop_64to32,   mkexpr(hi64))) );
   assign( *t1, unop(Iop_32Sto64, unop(Iop_64HIto32, mkexpr(lo64))) );
   assign( *t0, unop(Iop_32Sto64, unop(Iop_64to32,   mkexpr(lo64))) );
}

/* break V128 to 4xI32's, then zero-extend to I64's */
static void breakV128to4x64U ( IRExpr* t128,
                               /*OUTs*/
                               IRTemp* t3, IRTemp* t2,
                               IRTemp* t1, IRTemp* t0 )
{
   IRTemp hi64 = newTemp(Ity_I64);
   IRTemp lo64 = newTemp(Ity_I64);

   vassert(typeOfIRExpr(irsb->tyenv, t128) == Ity_V128);
   vassert(t0 && *t0 == IRTemp_INVALID);
   vassert(t1 && *t1 == IRTemp_INVALID);
   vassert(t2 && *t2 == IRTemp_INVALID);
   vassert(t3 && *t3 == IRTemp_INVALID);
   *t0 = newTemp(Ity_I64);
   *t1 = newTemp(Ity_I64);
   *t2 = newTemp(Ity_I64);
   *t3 = newTemp(Ity_I64);

   assign( hi64, unop(Iop_V128HIto64, t128) );
   assign( lo64, unop(Iop_V128to64,   t128) );
   assign( *t3, unop(Iop_32Uto64, unop(Iop_64HIto32, mkexpr(hi64))) );
   assign( *t2, unop(Iop_32Uto64, unop(Iop_64to32,   mkexpr(hi64))) );
   assign( *t1, unop(Iop_32Uto64, unop(Iop_64HIto32, mkexpr(lo64))) );
   assign( *t0, unop(Iop_32Uto64, unop(Iop_64to32,   mkexpr(lo64))) );
}

/* Signed saturating narrow 64S to 32 */
static IRExpr* mkQNarrow64Sto32 ( IRExpr* t64 )
{
   IRTemp hi32 = newTemp(Ity_I32);
   IRTemp lo32 = newTemp(Ity_I32);

   vassert(typeOfIRExpr(irsb->tyenv, t64) == Ity_I64);

   assign( hi32, unop(Iop_64HIto32, t64));
   assign( lo32, unop(Iop_64to32,   t64));

   return IRExpr_Mux0X(
             /* if (hi32 == (lo32 >>s 31)) */
             unop(Iop_1Uto8,
                  binop(Iop_CmpEQ32, mkexpr(hi32),
                        binop( Iop_Sar32, mkexpr(lo32), mkU8(31)))),
             /* else: sign dep saturate: 1->0x80000000, 0->0x7FFFFFFF */
             binop(Iop_Add32, mkU32(0x7FFFFFFF),
                   binop(Iop_Shr32, mkexpr(hi32), mkU8(31))),
             /* then: within signed-32 range: lo half good enough */
             mkexpr(lo32) );
}

/* Unsigned saturating narrow 64S to 32 */
static IRExpr* mkQNarrow64Uto32 ( IRExpr* t64 )
{
   IRTemp hi32 = newTemp(Ity_I32);
   IRTemp lo32 = newTemp(Ity_I32);

   vassert(typeOfIRExpr(irsb->tyenv, t64) == Ity_I64);

   assign( hi32, unop(Iop_64HIto32, t64));
   assign( lo32, unop(Iop_64to32,   t64));

   return IRExpr_Mux0X(
            /* if (top 32 bits of t64 are 0) */
            unop(Iop_1Uto8, binop(Iop_CmpEQ32, mkexpr(hi32), mkU32(0))),
            /* else: positive saturate -> 0xFFFFFFFF */
            mkU32(0xFFFFFFFF),
            /* then: within unsigned-32 range: lo half good enough */
            mkexpr(lo32) );
}

/* Signed saturate narrow 64->32, combining to V128 */
static IRExpr* mkV128from4x64S ( IRExpr* t3, IRExpr* t2,
                                 IRExpr* t1, IRExpr* t0 )
{
   vassert(typeOfIRExpr(irsb->tyenv, t3) == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv, t2) == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv, t1) == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv, t0) == Ity_I64);
   return binop(Iop_64HLtoV128,
                binop(Iop_32HLto64,
                      mkQNarrow64Sto32( t3 ),
                      mkQNarrow64Sto32( t2 )),
                binop(Iop_32HLto64,
                      mkQNarrow64Sto32( t1 ),
                      mkQNarrow64Sto32( t0 )));
}

/* Unsigned saturate narrow 64->32, combining to V128 */
static IRExpr* mkV128from4x64U ( IRExpr* t3, IRExpr* t2,
                                 IRExpr* t1, IRExpr* t0 )
{
   vassert(typeOfIRExpr(irsb->tyenv, t3) == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv, t2) == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv, t1) == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv, t0) == Ity_I64);
   return binop(Iop_64HLtoV128,
                binop(Iop_32HLto64,
                      mkQNarrow64Uto32( t3 ),
                      mkQNarrow64Uto32( t2 )),
                binop(Iop_32HLto64,
                      mkQNarrow64Uto32( t1 ),
                      mkQNarrow64Uto32( t0 )));
}

/* Simulate irops Iop_MullOdd*, since we don't have them  */
#define MK_Iop_MullOdd8Ux16( expr_vA, expr_vB ) \
      binop(Iop_MullEven8Ux16, \
            binop(Iop_ShrV128, expr_vA, mkU8(8)), \
            binop(Iop_ShrV128, expr_vB, mkU8(8)))

#define MK_Iop_MullOdd8Sx16( expr_vA, expr_vB ) \
      binop(Iop_MullEven8Sx16, \
            binop(Iop_ShrV128, expr_vA, mkU8(8)), \
            binop(Iop_ShrV128, expr_vB, mkU8(8)))

#define MK_Iop_MullOdd16Ux8( expr_vA, expr_vB ) \
      binop(Iop_MullEven16Ux8, \
            binop(Iop_ShrV128, expr_vA, mkU8(16)), \
            binop(Iop_ShrV128, expr_vB, mkU8(16)))

#define MK_Iop_MullOdd16Sx8( expr_vA, expr_vB ) \
      binop(Iop_MullEven16Sx8, \
            binop(Iop_ShrV128, expr_vA, mkU8(16)), \
            binop(Iop_ShrV128, expr_vB, mkU8(16)))

static IRExpr* /* :: Ity_I64 */ mk64lo32Sto64 ( IRExpr* src )
{
   vassert(typeOfIRExpr(irsb->tyenv, src) == Ity_I64);
   return unop(Iop_32Sto64, unop(Iop_64to32, src));
}

static IRExpr* /* :: Ity_I64 */ mk64lo32Uto64 ( IRExpr* src )
{
   vassert(typeOfIRExpr(irsb->tyenv, src) == Ity_I64);
   return unop(Iop_32Uto64, unop(Iop_64to32, src));
}

static IROp mkSzOp ( IRType ty, IROp op8 )
{
   Int adj;
   vassert(ty == Ity_I8  || ty == Ity_I16 ||
           ty == Ity_I32 || ty == Ity_I64);
   vassert(op8 == Iop_Add8   || op8 == Iop_Sub8   || op8 == Iop_Mul8 ||
           op8 == Iop_Or8    || op8 == Iop_And8   || op8 == Iop_Xor8 ||
           op8 == Iop_Shl8   || op8 == Iop_Shr8   || op8 == Iop_Sar8 ||
           op8 == Iop_CmpEQ8 || op8 == Iop_CmpNE8 ||
           op8 == Iop_Not8 );
   adj = ty==Ity_I8 ? 0 : (ty==Ity_I16 ? 1 : (ty==Ity_I32 ? 2 : 3));
   return adj + op8;
}

/* Make sure we get valid 32 and 64bit addresses */
static Addr64 mkSzAddr ( IRType ty, Addr64 addr )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ( ty == Ity_I64 ?
            (Addr64)addr :
            (Addr64)extend_s_32to64( toUInt(addr) ) );
}

/* sz, ULong -> IRExpr */
static IRExpr* mkSzImm ( IRType ty, ULong imm64 )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? mkU64(imm64) : mkU32((UInt)imm64);
}

/* sz, ULong -> IRConst */
static IRConst* mkSzConst ( IRType ty, ULong imm64 )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ( ty == Ity_I64 ?
            IRConst_U64(imm64) :
            IRConst_U32((UInt)imm64) );
}

/* Sign extend imm16 -> IRExpr* */
static IRExpr* mkSzExtendS16 ( IRType ty, UInt imm16 )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ( ty == Ity_I64 ?
            mkU64(extend_s_16to64(imm16)) :
            mkU32(extend_s_16to32(imm16)) );
}

/* Sign extend imm32 -> IRExpr* */
static IRExpr* mkSzExtendS32 ( IRType ty, UInt imm32 )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ( ty == Ity_I64 ?
            mkU64(extend_s_32to64(imm32)) :
            mkU32(imm32) );
}

/* IR narrows I32/I64 -> I8/I16/I32 */
static IRExpr* mkNarrowTo8 ( IRType ty, IRExpr* src )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to8, src) : unop(Iop_32to8, src);
}

static IRExpr* mkNarrowTo16 ( IRType ty, IRExpr* src )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to16, src) : unop(Iop_32to16, src);
}

static IRExpr* mkNarrowTo32 ( IRType ty, IRExpr* src )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   return ty == Ity_I64 ? unop(Iop_64to32, src) : src;
}

/* Signed/Unsigned IR widens I8/I16/I32 -> I32/I64 */
static IRExpr* mkWidenFrom8 ( IRType ty, IRExpr* src, Bool sined )
{
   IROp op;
   vassert(ty == Ity_I32 || ty == Ity_I64);
   if (sined) op = (ty==Ity_I32) ? Iop_8Sto32 : Iop_8Sto64;
   else       op = (ty==Ity_I32) ? Iop_8Uto32 : Iop_8Uto64;
   return unop(op, src);
}

static IRExpr* mkWidenFrom16 ( IRType ty, IRExpr* src, Bool sined )
{
   IROp op;
   vassert(ty == Ity_I32 || ty == Ity_I64);
   if (sined) op = (ty==Ity_I32) ? Iop_16Sto32 : Iop_16Sto64;
   else       op = (ty==Ity_I32) ? Iop_16Uto32 : Iop_16Uto64;
   return unop(op, src);
}

static IRExpr* mkWidenFrom32 ( IRType ty, IRExpr* src, Bool sined )
{
   vassert(ty == Ity_I32 || ty == Ity_I64);
   if (ty == Ity_I32)
      return src;
   return (sined) ? unop(Iop_32Sto64, src) : unop(Iop_32Uto64, src);
}


static Int integerGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   // jrs: probably not necessary; only matters if we reference sub-parts
   // of the ppc registers, but that isn't the case
   // later: this might affect Altivec though?
   vassert(host_is_bigendian);

   switch (archreg) {
   case  0: return offsetofPPCGuestState(guest_GPR0);
   case  1: return offsetofPPCGuestState(guest_GPR1);
   case  2: return offsetofPPCGuestState(guest_GPR2);
   case  3: return offsetofPPCGuestState(guest_GPR3);
   case  4: return offsetofPPCGuestState(guest_GPR4);
   case  5: return offsetofPPCGuestState(guest_GPR5);
   case  6: return offsetofPPCGuestState(guest_GPR6);
   case  7: return offsetofPPCGuestState(guest_GPR7);
   case  8: return offsetofPPCGuestState(guest_GPR8);
   case  9: return offsetofPPCGuestState(guest_GPR9);
   case 10: return offsetofPPCGuestState(guest_GPR10);
   case 11: return offsetofPPCGuestState(guest_GPR11);
   case 12: return offsetofPPCGuestState(guest_GPR12);
   case 13: return offsetofPPCGuestState(guest_GPR13);
   case 14: return offsetofPPCGuestState(guest_GPR14);
   case 15: return offsetofPPCGuestState(guest_GPR15);
   case 16: return offsetofPPCGuestState(guest_GPR16);
   case 17: return offsetofPPCGuestState(guest_GPR17);
   case 18: return offsetofPPCGuestState(guest_GPR18);
   case 19: return offsetofPPCGuestState(guest_GPR19);
   case 20: return offsetofPPCGuestState(guest_GPR20);
   case 21: return offsetofPPCGuestState(guest_GPR21);
   case 22: return offsetofPPCGuestState(guest_GPR22);
   case 23: return offsetofPPCGuestState(guest_GPR23);
   case 24: return offsetofPPCGuestState(guest_GPR24);
   case 25: return offsetofPPCGuestState(guest_GPR25);
   case 26: return offsetofPPCGuestState(guest_GPR26);
   case 27: return offsetofPPCGuestState(guest_GPR27);
   case 28: return offsetofPPCGuestState(guest_GPR28);
   case 29: return offsetofPPCGuestState(guest_GPR29);
   case 30: return offsetofPPCGuestState(guest_GPR30);
   case 31: return offsetofPPCGuestState(guest_GPR31);
   default: break;
   }
   vpanic("integerGuestRegOffset(ppc,be)"); /*notreached*/
}

static IRExpr* getIReg ( UInt archreg )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert(archreg < 32);
   return IRExpr_Get( integerGuestRegOffset(archreg), ty );
}

/* Ditto, but write to a reg instead. */
static void putIReg ( UInt archreg, IRExpr* e )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irsb->tyenv, e) == ty );
   stmt( IRStmt_Put(integerGuestRegOffset(archreg), e) );
}


static Int floatGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   switch (archreg) {
   case  0: return offsetofPPCGuestState(guest_FPR0);
   case  1: return offsetofPPCGuestState(guest_FPR1);
   case  2: return offsetofPPCGuestState(guest_FPR2);
   case  3: return offsetofPPCGuestState(guest_FPR3);
   case  4: return offsetofPPCGuestState(guest_FPR4);
   case  5: return offsetofPPCGuestState(guest_FPR5);
   case  6: return offsetofPPCGuestState(guest_FPR6);
   case  7: return offsetofPPCGuestState(guest_FPR7);
   case  8: return offsetofPPCGuestState(guest_FPR8);
   case  9: return offsetofPPCGuestState(guest_FPR9);
   case 10: return offsetofPPCGuestState(guest_FPR10);
   case 11: return offsetofPPCGuestState(guest_FPR11);
   case 12: return offsetofPPCGuestState(guest_FPR12);
   case 13: return offsetofPPCGuestState(guest_FPR13);
   case 14: return offsetofPPCGuestState(guest_FPR14);
   case 15: return offsetofPPCGuestState(guest_FPR15);
   case 16: return offsetofPPCGuestState(guest_FPR16);
   case 17: return offsetofPPCGuestState(guest_FPR17);
   case 18: return offsetofPPCGuestState(guest_FPR18);
   case 19: return offsetofPPCGuestState(guest_FPR19);
   case 20: return offsetofPPCGuestState(guest_FPR20);
   case 21: return offsetofPPCGuestState(guest_FPR21);
   case 22: return offsetofPPCGuestState(guest_FPR22);
   case 23: return offsetofPPCGuestState(guest_FPR23);
   case 24: return offsetofPPCGuestState(guest_FPR24);
   case 25: return offsetofPPCGuestState(guest_FPR25);
   case 26: return offsetofPPCGuestState(guest_FPR26);
   case 27: return offsetofPPCGuestState(guest_FPR27);
   case 28: return offsetofPPCGuestState(guest_FPR28);
   case 29: return offsetofPPCGuestState(guest_FPR29);
   case 30: return offsetofPPCGuestState(guest_FPR30);
   case 31: return offsetofPPCGuestState(guest_FPR31);
   default: break;
   }
   vpanic("floatGuestRegOffset(ppc)"); /*notreached*/
}

static IRExpr* getFReg ( UInt archreg )
{
   vassert(archreg < 32);
   return IRExpr_Get( floatGuestRegOffset(archreg), Ity_F64 );
}

/* Ditto, but write to a reg instead. */
static void putFReg ( UInt archreg, IRExpr* e )
{
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_F64);
   stmt( IRStmt_Put(floatGuestRegOffset(archreg), e) );
}


static Int vectorGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   switch (archreg) {
   case  0: return offsetofPPCGuestState(guest_VR0);
   case  1: return offsetofPPCGuestState(guest_VR1);
   case  2: return offsetofPPCGuestState(guest_VR2);
   case  3: return offsetofPPCGuestState(guest_VR3);
   case  4: return offsetofPPCGuestState(guest_VR4);
   case  5: return offsetofPPCGuestState(guest_VR5);
   case  6: return offsetofPPCGuestState(guest_VR6);
   case  7: return offsetofPPCGuestState(guest_VR7);
   case  8: return offsetofPPCGuestState(guest_VR8);
   case  9: return offsetofPPCGuestState(guest_VR9);
   case 10: return offsetofPPCGuestState(guest_VR10);
   case 11: return offsetofPPCGuestState(guest_VR11);
   case 12: return offsetofPPCGuestState(guest_VR12);
   case 13: return offsetofPPCGuestState(guest_VR13);
   case 14: return offsetofPPCGuestState(guest_VR14);
   case 15: return offsetofPPCGuestState(guest_VR15);
   case 16: return offsetofPPCGuestState(guest_VR16);
   case 17: return offsetofPPCGuestState(guest_VR17);
   case 18: return offsetofPPCGuestState(guest_VR18);
   case 19: return offsetofPPCGuestState(guest_VR19);
   case 20: return offsetofPPCGuestState(guest_VR20);
   case 21: return offsetofPPCGuestState(guest_VR21);
   case 22: return offsetofPPCGuestState(guest_VR22);
   case 23: return offsetofPPCGuestState(guest_VR23);
   case 24: return offsetofPPCGuestState(guest_VR24);
   case 25: return offsetofPPCGuestState(guest_VR25);
   case 26: return offsetofPPCGuestState(guest_VR26);
   case 27: return offsetofPPCGuestState(guest_VR27);
   case 28: return offsetofPPCGuestState(guest_VR28);
   case 29: return offsetofPPCGuestState(guest_VR29);
   case 30: return offsetofPPCGuestState(guest_VR30);
   case 31: return offsetofPPCGuestState(guest_VR31);
   default: break;
   }
   vpanic("vextorGuestRegOffset(ppc)"); /*notreached*/
}

static IRExpr* getVReg ( UInt archreg )
{
   vassert(archreg < 32);
   return IRExpr_Get( vectorGuestRegOffset(archreg), Ity_V128 );
}

/* Ditto, but write to a reg instead. */
static void putVReg ( UInt archreg, IRExpr* e )
{
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_V128);
   stmt( IRStmt_Put(vectorGuestRegOffset(archreg), e) );
}

static Int guestCR321offset ( UInt cr )
{
   switch (cr) {
   case 0: return offsetofPPCGuestState(guest_CR0_321 );
   case 1: return offsetofPPCGuestState(guest_CR1_321 );
   case 2: return offsetofPPCGuestState(guest_CR2_321 );
   case 3: return offsetofPPCGuestState(guest_CR3_321 );
   case 4: return offsetofPPCGuestState(guest_CR4_321 );
   case 5: return offsetofPPCGuestState(guest_CR5_321 );
   case 6: return offsetofPPCGuestState(guest_CR6_321 );
   case 7: return offsetofPPCGuestState(guest_CR7_321 );
   default: vpanic("guestCR321offset(ppc)");
   }
} 

static Int guestCR0offset ( UInt cr )
{
   switch (cr) {
   case 0: return offsetofPPCGuestState(guest_CR0_0 );
   case 1: return offsetofPPCGuestState(guest_CR1_0 );
   case 2: return offsetofPPCGuestState(guest_CR2_0 );
   case 3: return offsetofPPCGuestState(guest_CR3_0 );
   case 4: return offsetofPPCGuestState(guest_CR4_0 );
   case 5: return offsetofPPCGuestState(guest_CR5_0 );
   case 6: return offsetofPPCGuestState(guest_CR6_0 );
   case 7: return offsetofPPCGuestState(guest_CR7_0 );
   default: vpanic("guestCR3offset(ppc)");
   }
}

// ROTL(src32/64, rot_amt5/6)
static IRExpr* /* :: Ity_I32/64 */ ROTL ( IRExpr* src,
                                          IRExpr* rot_amt )
{
   IRExpr *mask, *rot;
   vassert(typeOfIRExpr(irsb->tyenv,rot_amt) == Ity_I8);

   if (typeOfIRExpr(irsb->tyenv,src) == Ity_I64) {
      // rot = (src << rot_amt) | (src >> (64-rot_amt))
      mask = binop(Iop_And8, rot_amt, mkU8(63));
      rot  = binop(Iop_Or64,
                binop(Iop_Shl64, src, mask),
                binop(Iop_Shr64, src, binop(Iop_Sub8, mkU8(64), mask)));
   } else {
      // rot = (src << rot_amt) | (src >> (32-rot_amt))
      mask = binop(Iop_And8, rot_amt, mkU8(31));
      rot  = binop(Iop_Or32,
                binop(Iop_Shl32, src, mask),
                binop(Iop_Shr32, src, binop(Iop_Sub8, mkU8(32), mask)));
   }
   /* Note: the MuxOX is not merely an optimisation; it's needed
      because otherwise the Shr is a shift by the word size when
      mask denotes zero.  For rotates by immediates, a lot of
      this junk gets folded out. */
   return IRExpr_Mux0X( mask, /*     zero rotate */ src,
                              /* non-zero rotate */ rot );
}

/* Standard effective address calc: (rA + rB) */
static IRExpr* ea_rA_idxd ( UInt rA, UInt rB )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert(rA < 32);
   vassert(rB < 32);
   return binop(mkSzOp(ty, Iop_Add8), getIReg(rA), getIReg(rB));
}

/* Standard effective address calc: (rA + simm) */
static IRExpr* ea_rA_simm ( UInt rA, UInt simm16 )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert(rA < 32);
   return binop(mkSzOp(ty, Iop_Add8), getIReg(rA),
                mkSzExtendS16(ty, simm16));
}

/* Standard effective address calc: (rA|0) */
static IRExpr* ea_rAor0 ( UInt rA )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert(rA < 32);
   if (rA == 0) {
      return mkSzImm(ty, 0);
   } else {
      return getIReg(rA);
   }
}

/* Standard effective address calc: (rA|0) + rB */
static IRExpr* ea_rAor0_idxd ( UInt rA, UInt rB )
{
   vassert(rA < 32);
   vassert(rB < 32);
   return (rA == 0) ? getIReg(rB) : ea_rA_idxd( rA, rB );
}

/* Standard effective address calc: (rA|0) + simm16 */
static IRExpr* ea_rAor0_simm ( UInt rA, UInt simm16 )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert(rA < 32);
   if (rA == 0) {
      return mkSzExtendS16(ty, simm16);
   } else {
      return ea_rA_simm( rA, simm16 );
   }
}


/* Align effective address */
static IRExpr* addr_align( IRExpr* addr, UChar align )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   Long mask;
   switch (align) {
   case 1:  return addr;                    // byte aligned
   case 2:  mask = ((Long)-1) << 1; break;  // half-word aligned
   case 4:  mask = ((Long)-1) << 2; break;  // word aligned
   case 16: mask = ((Long)-1) << 4; break;  // quad-word aligned
   default:
      vex_printf("addr_align: align = %u\n", align);
      vpanic("addr_align(ppc)");
   }

   vassert(typeOfIRExpr(irsb->tyenv,addr) == ty);
   return binop( mkSzOp(ty, Iop_And8), addr, mkSzImm(ty, mask) );
}


/* Exit the trace if ADDR (intended to be a guest memory address) is
   not ALIGN-aligned, generating a request for a SIGBUS followed by a
   restart of the current insn. */
static void gen_SIGBUS_if_misaligned ( IRTemp addr, UChar align )
{
   vassert(align == 4 || align == 8);
   if (mode64) {
      vassert(typeOfIRTemp(irsb->tyenv, addr) == Ity_I64);
      stmt(
         IRStmt_Exit(
            binop(Iop_CmpNE64,
                  binop(Iop_And64, mkexpr(addr), mkU64(align-1)),
                  mkU64(0)),
            Ijk_SigBUS,
            IRConst_U64( guest_CIA_curr_instr )
         )
      );
   } else {
      vassert(typeOfIRTemp(irsb->tyenv, addr) == Ity_I32);
      stmt(
         IRStmt_Exit(
            binop(Iop_CmpNE32,
                  binop(Iop_And32, mkexpr(addr), mkU32(align-1)),
                  mkU32(0)),
            Ijk_SigBUS,
            IRConst_U32( guest_CIA_curr_instr )
         )
      );
   }
}


/* Generate AbiHints which mark points at which the ELF or PowerOpen
   ABIs say that the stack red zone (viz, -N(r1) .. -1(r1), for some
   N) becomes undefined.  That is at function calls and returns.  ELF
   ppc32 doesn't have this "feature" (how fortunate for it).  nia is
   the address of the next instruction to be executed.
*/
static void make_redzone_AbiHint ( VexAbiInfo* vbi, 
                                   IRTemp nia, HChar* who )
{
   Int szB = vbi->guest_stack_redzone_size;
   if (0) vex_printf("AbiHint: %s\n", who);
   vassert(szB >= 0);
   if (szB > 0) {
      if (mode64) {
         vassert(typeOfIRTemp(irsb->tyenv, nia) == Ity_I64);
         stmt( IRStmt_AbiHint( 
                  binop(Iop_Sub64, getIReg(1), mkU64(szB)), 
                  szB,
                  mkexpr(nia)
         ));
      } else {
         vassert(typeOfIRTemp(irsb->tyenv, nia) == Ity_I32);
         stmt( IRStmt_AbiHint( 
                  binop(Iop_Sub32, getIReg(1), mkU32(szB)), 
                  szB,
                  mkexpr(nia)
         ));
      }
   }
}


/*------------------------------------------------------------*/
/*--- Helpers for condition codes.                         ---*/
/*------------------------------------------------------------*/

/* Condition register layout. 

   In the hardware, CR is laid out like this.  The leftmost end is the
   most significant bit in the register; however the IBM documentation
   numbers the bits backwards for some reason.

   CR0      CR1    ..........   CR6       CR7
   0 .. 3   .......................  28 .. 31    (IBM bit numbering)
   31  28                             3    0     (normal bit numbering)

   Each CR field is 4 bits:  [<,>,==,SO]

   Hence in IBM's notation, BI=0 is CR7[SO], BI=1 is CR7[==], etc.

   Indexing from BI to guest state:

     let    n = BI / 4
          off = BI % 4
     this references CR n:

        off==0   ->  guest_CRn_321 >> 3
        off==1   ->  guest_CRn_321 >> 2
        off==2   ->  guest_CRn_321 >> 1
        off==3   ->  guest_CRn_SO

   Bear in mind the only significant bit in guest_CRn_SO is bit 0
   (normal notation) and in guest_CRn_321 the significant bits are
   3, 2 and 1 (normal notation).
*/

static void putCR321 ( UInt cr, IRExpr* e )
{
   vassert(cr < 8);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(guestCR321offset(cr), e) );
}

static void putCR0 ( UInt cr, IRExpr* e )
{
   vassert(cr < 8);
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(guestCR0offset(cr), e) );
}

static IRExpr* /* :: Ity_I8 */ getCR0 ( UInt cr )
{
   vassert(cr < 8);
   return IRExpr_Get(guestCR0offset(cr), Ity_I8);
}

static IRExpr* /* :: Ity_I8 */ getCR321 ( UInt cr )
{
   vassert(cr < 8);
   return IRExpr_Get(guestCR321offset(cr), Ity_I8);
}

/* Fetch the specified CR bit (as per IBM/hardware notation) and
   return it at the bottom of an I32; the top 31 bits are guaranteed
   to be zero. */
static IRExpr* /* :: Ity_I32 */ getCRbit ( UInt bi )
{
   UInt n   = bi / 4;
   UInt off = bi % 4;
   vassert(bi < 32);
   if (off == 3) {
      /* Fetch the SO bit for this CR field */
      /* Note: And32 is redundant paranoia iff guest state only has 0
         or 1 in that slot. */
      return binop(Iop_And32, unop(Iop_8Uto32, getCR0(n)), mkU32(1));
   } else {
      /* Fetch the <, > or == bit for this CR field */
      return binop( Iop_And32, 
                    binop( Iop_Shr32, 
                           unop(Iop_8Uto32, getCR321(n)),
                           mkU8(toUChar(3-off)) ),
                    mkU32(1) );
   }
}

/* Dually, write the least significant bit of BIT to the specified CR
   bit.  Indexing as per getCRbit. */
static void putCRbit ( UInt bi, IRExpr* bit )
{
   UInt    n, off;
   IRExpr* safe;
   vassert(typeOfIRExpr(irsb->tyenv,bit) == Ity_I32);
   safe = binop(Iop_And32, bit, mkU32(1));
   n   = bi / 4;
   off = bi % 4;
   vassert(bi < 32);
   if (off == 3) {
      /* This is the SO bit for this CR field */
      putCR0(n, unop(Iop_32to8, safe));
   } else {
      off = 3 - off;
      vassert(off == 1 || off == 2 || off == 3);
      putCR321(
         n,
         unop( Iop_32to8,
               binop( Iop_Or32,
                      /* old value with field masked out */
                      binop(Iop_And32, unop(Iop_8Uto32, getCR321(n)),
                                       mkU32(~(1 << off))),
                      /* new value in the right place */
                      binop(Iop_Shl32, safe, mkU8(toUChar(off)))
               )
         )
      );
   }
}

/* Fetch the specified CR bit (as per IBM/hardware notation) and
   return it somewhere in an I32; it does not matter where, but
   whichever bit it is, all other bits are guaranteed to be zero.  In
   other words, the I32-typed expression will be zero if the bit is
   zero and nonzero if the bit is 1.  Write into *where the index
   of where the bit will be. */

static
IRExpr* /* :: Ity_I32 */ getCRbit_anywhere ( UInt bi, Int* where )
{
   UInt n   = bi / 4;
   UInt off = bi % 4;
   vassert(bi < 32);
   if (off == 3) {
      /* Fetch the SO bit for this CR field */
      /* Note: And32 is redundant paranoia iff guest state only has 0
         or 1 in that slot. */
      *where = 0;
      return binop(Iop_And32, unop(Iop_8Uto32, getCR0(n)), mkU32(1));
   } else {
      /* Fetch the <, > or == bit for this CR field */
      *where = 3-off;
      return binop( Iop_And32, 
                    unop(Iop_8Uto32, getCR321(n)),
                    mkU32(1 << (3-off)) );
   }
}

/* Set the CR0 flags following an arithmetic operation.
   (Condition Register CR0 Field Definition, PPC32 p60)
*/
static IRExpr* getXER_SO ( void );
static void set_CR0 ( IRExpr* result )
{
   vassert(typeOfIRExpr(irsb->tyenv,result) == Ity_I32 ||
           typeOfIRExpr(irsb->tyenv,result) == Ity_I64);
   if (mode64) {
      putCR321( 0, unop(Iop_64to8,
                        binop(Iop_CmpORD64S, result, mkU64(0))) );
   } else {
      putCR321( 0, unop(Iop_32to8,
                        binop(Iop_CmpORD32S, result, mkU32(0))) );
   }
   putCR0( 0, getXER_SO() );
}


/* Set the CR6 flags following an AltiVec compare operation. */
static void set_AV_CR6 ( IRExpr* result, Bool test_all_ones )
{
   /* CR6[0:3] = {all_ones, 0, all_zeros, 0}
      all_ones  = (v[0] && v[1] && v[2] && v[3])
      all_zeros = ~(v[0] || v[1] || v[2] || v[3])
   */
   IRTemp v0 = newTemp(Ity_V128);
   IRTemp v1 = newTemp(Ity_V128);
   IRTemp v2 = newTemp(Ity_V128);
   IRTemp v3 = newTemp(Ity_V128);
   IRTemp rOnes  = newTemp(Ity_I8);
   IRTemp rZeros = newTemp(Ity_I8);

   vassert(typeOfIRExpr(irsb->tyenv,result) == Ity_V128);

   assign( v0, result );
   assign( v1, binop(Iop_ShrV128, result, mkU8(32)) );
   assign( v2, binop(Iop_ShrV128, result, mkU8(64)) );
   assign( v3, binop(Iop_ShrV128, result, mkU8(96)) );

   assign( rZeros, unop(Iop_1Uto8,
       binop(Iop_CmpEQ32, mkU32(0xFFFFFFFF),
             unop(Iop_Not32,
                  unop(Iop_V128to32,
                       binop(Iop_OrV128,
                             binop(Iop_OrV128, mkexpr(v0), mkexpr(v1)),
                             binop(Iop_OrV128, mkexpr(v2), mkexpr(v3))))
                  ))) );

   if (test_all_ones) {
      assign( rOnes, unop(Iop_1Uto8,
         binop(Iop_CmpEQ32, mkU32(0xFFFFFFFF),
               unop(Iop_V128to32,
                    binop(Iop_AndV128,
                          binop(Iop_AndV128, mkexpr(v0), mkexpr(v1)),
                          binop(Iop_AndV128, mkexpr(v2), mkexpr(v3)))
                    ))) );
      putCR321( 6, binop(Iop_Or8,
                         binop(Iop_Shl8, mkexpr(rOnes),  mkU8(3)),
                         binop(Iop_Shl8, mkexpr(rZeros), mkU8(1))) );
   } else {
      putCR321( 6, binop(Iop_Shl8, mkexpr(rZeros), mkU8(1)) );
   }
   putCR0( 6, mkU8(0) );
} 



/*------------------------------------------------------------*/
/*--- Helpers for XER flags.                               ---*/
/*------------------------------------------------------------*/

static void putXER_SO ( IRExpr* e )
{
   IRExpr* so;
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I8);
   so = binop(Iop_And8, e, mkU8(1));
   stmt( IRStmt_Put( OFFB_XER_SO, so ) );
}

static void putXER_OV ( IRExpr* e )
{
   IRExpr* ov;
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I8);
   ov = binop(Iop_And8, e, mkU8(1));
   stmt( IRStmt_Put( OFFB_XER_OV, ov ) );
}

static void putXER_CA ( IRExpr* e )
{
   IRExpr* ca;
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I8);
   ca = binop(Iop_And8, e, mkU8(1));
   stmt( IRStmt_Put( OFFB_XER_CA, ca ) );
}

static void putXER_BC ( IRExpr* e )
{
   IRExpr* bc;
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I8);
   bc = binop(Iop_And8, e, mkU8(0x7F));
   stmt( IRStmt_Put( OFFB_XER_BC, bc ) );
}

static IRExpr* /* :: Ity_I8 */ getXER_SO ( void )
{
   return IRExpr_Get( OFFB_XER_SO, Ity_I8 );
}

static IRExpr* /* :: Ity_I32 */ getXER_SO32 ( void )
{
   return binop( Iop_And32, unop(Iop_8Uto32, getXER_SO()), mkU32(1) );
}

static IRExpr* /* :: Ity_I8 */ getXER_OV ( void )
{
   return IRExpr_Get( OFFB_XER_OV, Ity_I8 );
}

static IRExpr* /* :: Ity_I32 */ getXER_OV32 ( void )
{
   return binop( Iop_And32, unop(Iop_8Uto32, getXER_OV()), mkU32(1) );
}

static IRExpr* /* :: Ity_I32 */ getXER_CA32 ( void )
{
   IRExpr* ca = IRExpr_Get( OFFB_XER_CA, Ity_I8 );
   return binop( Iop_And32, unop(Iop_8Uto32, ca ), mkU32(1) );
}

static IRExpr* /* :: Ity_I8 */ getXER_BC ( void )
{
   return IRExpr_Get( OFFB_XER_BC, Ity_I8 );
}

static IRExpr* /* :: Ity_I32 */ getXER_BC32 ( void )
{
   IRExpr* bc = IRExpr_Get( OFFB_XER_BC, Ity_I8 );
   return binop( Iop_And32, unop(Iop_8Uto32, bc), mkU32(0x7F) );
}


/* RES is the result of doing OP on ARGL and ARGR.  Set %XER.OV and
   %XER.SO accordingly. */

static void set_XER_OV_32( UInt op, IRExpr* res,
                           IRExpr* argL, IRExpr* argR )
{
   IRTemp  t64;
   IRExpr* xer_ov;
   vassert(op < PPCG_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irsb->tyenv,res)  == Ity_I32);
   vassert(typeOfIRExpr(irsb->tyenv,argL) == Ity_I32);
   vassert(typeOfIRExpr(irsb->tyenv,argR) == Ity_I32);

#  define INT32_MIN 0x80000000

#  define XOR2(_aa,_bb) \
      binop(Iop_Xor32,(_aa),(_bb))

#  define XOR3(_cc,_dd,_ee) \
      binop(Iop_Xor32,binop(Iop_Xor32,(_cc),(_dd)),(_ee))

#  define AND3(_ff,_gg,_hh) \
      binop(Iop_And32,binop(Iop_And32,(_ff),(_gg)),(_hh))

#define NOT(_jj) \
      unop(Iop_Not32, (_jj))

   switch (op) {
   case /* 0  */ PPCG_FLAG_OP_ADD:
   case /* 1  */ PPCG_FLAG_OP_ADDE:
      /* (argL^argR^-1) & (argL^res) & (1<<31)  ?1:0 */
      // i.e. ((both_same_sign) & (sign_changed) & (sign_mask))
      xer_ov 
         = AND3( XOR3(argL,argR,mkU32(-1)),
                 XOR2(argL,res),
                 mkU32(INT32_MIN) );
      /* xer_ov can only be 0 or 1<<31 */
      xer_ov 
         = binop(Iop_Shr32, xer_ov, mkU8(31) );
      break;
      
   case /* 2  */ PPCG_FLAG_OP_DIVW:
      /* (argL == INT32_MIN && argR == -1) || argR == 0 */
      xer_ov
         = mkOR1(
              mkAND1( 
                 binop(Iop_CmpEQ32, argL, mkU32(INT32_MIN)),
                 binop(Iop_CmpEQ32, argR, mkU32(-1)) 
              ),
              binop(Iop_CmpEQ32, argR, mkU32(0) ) 
           );
      xer_ov 
         = unop(Iop_1Uto32, xer_ov);
      break;
      
   case /* 3  */ PPCG_FLAG_OP_DIVWU:
      /* argR == 0 */
      xer_ov 
         = unop(Iop_1Uto32, binop(Iop_CmpEQ32, argR, mkU32(0)));
      break;
      
   case /* 4  */ PPCG_FLAG_OP_MULLW:
      /* OV true if result can't be represented in 32 bits
         i.e sHi != sign extension of sLo */
      t64 = newTemp(Ity_I64);
      assign( t64, binop(Iop_MullS32, argL, argR) );
      xer_ov 
         = binop( Iop_CmpNE32,
                  unop(Iop_64HIto32, mkexpr(t64)),
                  binop( Iop_Sar32, 
                         unop(Iop_64to32, mkexpr(t64)), 
                         mkU8(31))
                  );
      xer_ov
         = unop(Iop_1Uto32, xer_ov);
      break;
      
   case /* 5  */ PPCG_FLAG_OP_NEG:
      /* argL == INT32_MIN */
      xer_ov
         = unop( Iop_1Uto32, 
                 binop(Iop_CmpEQ32, argL, mkU32(INT32_MIN)) );
      break;
      
   case /* 6  */ PPCG_FLAG_OP_SUBF:
   case /* 7  */ PPCG_FLAG_OP_SUBFC:
   case /* 8  */ PPCG_FLAG_OP_SUBFE:
      /* ((~argL)^argR^-1) & ((~argL)^res) & (1<<31) ?1:0; */
      xer_ov 
         = AND3( XOR3(NOT(argL),argR,mkU32(-1)),
                 XOR2(NOT(argL),res),
                 mkU32(INT32_MIN) );
      /* xer_ov can only be 0 or 1<<31 */
      xer_ov 
         = binop(Iop_Shr32, xer_ov, mkU8(31) );
      break;
      
   default: 
      vex_printf("set_XER_OV: op = %u\n", op);
      vpanic("set_XER_OV(ppc)");
   }
   
   /* xer_ov MUST denote either 0 or 1, no other value allowed */
   putXER_OV( unop(Iop_32to8, xer_ov) );

   /* Update the summary overflow */
   putXER_SO( binop(Iop_Or8, getXER_SO(), getXER_OV()) );

#  undef INT32_MIN
#  undef AND3
#  undef XOR3
#  undef XOR2
#  undef NOT
}

static void set_XER_OV_64( UInt op, IRExpr* res,
                           IRExpr* argL, IRExpr* argR )
{
   IRExpr* xer_ov;
   vassert(op < PPCG_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irsb->tyenv,res)  == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv,argL) == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv,argR) == Ity_I64);

#  define INT64_MIN 0x8000000000000000ULL

#  define XOR2(_aa,_bb) \
      binop(Iop_Xor64,(_aa),(_bb))

#  define XOR3(_cc,_dd,_ee) \
      binop(Iop_Xor64,binop(Iop_Xor64,(_cc),(_dd)),(_ee))

#  define AND3(_ff,_gg,_hh) \
      binop(Iop_And64,binop(Iop_And64,(_ff),(_gg)),(_hh))

#define NOT(_jj) \
      unop(Iop_Not64, (_jj))

   switch (op) {
   case /* 0  */ PPCG_FLAG_OP_ADD:
   case /* 1  */ PPCG_FLAG_OP_ADDE:
      /* (argL^argR^-1) & (argL^res) & (1<<63)  ? 1:0 */
      // i.e. ((both_same_sign) & (sign_changed) & (sign_mask))
      xer_ov 
         = AND3( XOR3(argL,argR,mkU64(-1)),
                 XOR2(argL,res),
                 mkU64(INT64_MIN) );
      /* xer_ov can only be 0 or 1<<63 */
      xer_ov 
         = unop(Iop_64to1, binop(Iop_Shr64, xer_ov, mkU8(63)));
      break;
      
   case /* 2  */ PPCG_FLAG_OP_DIVW:
      /* (argL == INT64_MIN && argR == -1) || argR == 0 */
      xer_ov
         = mkOR1(
              mkAND1( 
                 binop(Iop_CmpEQ64, argL, mkU64(INT64_MIN)),
                 binop(Iop_CmpEQ64, argR, mkU64(-1)) 
              ),
              binop(Iop_CmpEQ64, argR, mkU64(0) ) 
           );
      break;

   case /* 3  */ PPCG_FLAG_OP_DIVWU:
      /* argR == 0 */
      xer_ov 
         = binop(Iop_CmpEQ64, argR, mkU64(0));
      break;
      
   case /* 4  */ PPCG_FLAG_OP_MULLW: {
      /* OV true if result can't be represented in 64 bits
         i.e sHi != sign extension of sLo */
      xer_ov 
         = binop( Iop_CmpNE32,
                  unop(Iop_64HIto32, res),
                  binop( Iop_Sar32, 
                         unop(Iop_64to32, res), 
                         mkU8(31))
                  );
      break;
   }
      
   case /* 5  */ PPCG_FLAG_OP_NEG:
      /* argL == INT64_MIN */
      xer_ov
         = binop(Iop_CmpEQ64, argL, mkU64(INT64_MIN));
      break;
      
   case /* 6  */ PPCG_FLAG_OP_SUBF:
   case /* 7  */ PPCG_FLAG_OP_SUBFC:
   case /* 8  */ PPCG_FLAG_OP_SUBFE:
      /* ((~argL)^argR^-1) & ((~argL)^res) & (1<<63) ?1:0; */
      xer_ov 
         = AND3( XOR3(NOT(argL),argR,mkU64(-1)),
                 XOR2(NOT(argL),res),
                 mkU64(INT64_MIN) );
      /* xer_ov can only be 0 or 1<<63 */
      xer_ov 
         = unop(Iop_64to1, binop(Iop_Shr64, xer_ov, mkU8(63)));
      break;
      
   default: 
      vex_printf("set_XER_OV: op = %u\n", op);
      vpanic("set_XER_OV(ppc64)");
   }
   
   /* xer_ov MUST denote either 0 or 1, no other value allowed */
   putXER_OV( unop(Iop_1Uto8, xer_ov) );

   /* Update the summary overflow */
   putXER_SO( binop(Iop_Or8, getXER_SO(), getXER_OV()) );

#  undef INT64_MIN
#  undef AND3
#  undef XOR3
#  undef XOR2
#  undef NOT
}

static void set_XER_OV ( IRType ty, UInt op, IRExpr* res,
                         IRExpr* argL, IRExpr* argR )
{
   if (ty == Ity_I32)
      set_XER_OV_32( op, res, argL, argR );
   else
      set_XER_OV_64( op, res, argL, argR );
}



/* RES is the result of doing OP on ARGL and ARGR with the old %XER.CA
   value being OLDCA.  Set %XER.CA accordingly. */

static void set_XER_CA_32 ( UInt op, IRExpr* res,
                            IRExpr* argL, IRExpr* argR, IRExpr* oldca )
{
   IRExpr* xer_ca;
   vassert(op < PPCG_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irsb->tyenv,res)   == Ity_I32);
   vassert(typeOfIRExpr(irsb->tyenv,argL)  == Ity_I32);
   vassert(typeOfIRExpr(irsb->tyenv,argR)  == Ity_I32);
   vassert(typeOfIRExpr(irsb->tyenv,oldca) == Ity_I32);

   /* Incoming oldca is assumed to hold the values 0 or 1 only.  This
      seems reasonable given that it's always generated by
      getXER_CA32(), which masks it accordingly.  In any case it being
      0 or 1 is an invariant of the ppc guest state representation;
      if it has any other value, that invariant has been violated. */

   switch (op) {
   case /* 0 */ PPCG_FLAG_OP_ADD:
      /* res <u argL */
      xer_ca
         = unop(Iop_1Uto32, binop(Iop_CmpLT32U, res, argL));
      break;
      
   case /* 1 */ PPCG_FLAG_OP_ADDE:
      /* res <u argL || (old_ca==1 && res==argL) */
      xer_ca 
         = mkOR1( 
              binop(Iop_CmpLT32U, res, argL),
              mkAND1( 
                 binop(Iop_CmpEQ32, oldca, mkU32(1)),
                 binop(Iop_CmpEQ32, res, argL) 
              ) 
           );
      xer_ca 
         = unop(Iop_1Uto32, xer_ca);
      break;
      
   case /* 8 */ PPCG_FLAG_OP_SUBFE:
      /* res <u argR || (old_ca==1 && res==argR) */
      xer_ca 
         = mkOR1( 
              binop(Iop_CmpLT32U, res, argR),
              mkAND1( 
                 binop(Iop_CmpEQ32, oldca, mkU32(1)),
                 binop(Iop_CmpEQ32, res, argR) 
              ) 
           );
      xer_ca 
         = unop(Iop_1Uto32, xer_ca);
      break;
      
   case /* 7 */ PPCG_FLAG_OP_SUBFC:
   case /* 9 */ PPCG_FLAG_OP_SUBFI:
      /* res <=u argR */
      xer_ca
         = unop(Iop_1Uto32, binop(Iop_CmpLE32U, res, argR));
      break;
      
   case /* 10 */ PPCG_FLAG_OP_SRAW:
      /* The shift amount is guaranteed to be in 0 .. 63 inclusive.
         If it is <= 31, behave like SRAWI; else XER.CA is the sign
         bit of argL. */
      /* This term valid for shift amount < 32 only */
      xer_ca
         = binop(
              Iop_And32,
              binop(Iop_Sar32, argL, mkU8(31)),
              binop( Iop_And32,
                     argL,
                     binop( Iop_Sub32,
                            binop(Iop_Shl32, mkU32(1),
                                             unop(Iop_32to8,argR)),
                            mkU32(1) )
                     )
              );
      xer_ca 
         = IRExpr_Mux0X(
              /* shift amt > 31 ? */
              unop(Iop_1Uto8, binop(Iop_CmpLT32U, mkU32(31), argR)),
              /* no -- be like srawi */
              unop(Iop_1Uto32, binop(Iop_CmpNE32, xer_ca, mkU32(0))),
              /* yes -- get sign bit of argL */
              binop(Iop_Shr32, argL, mkU8(31))
           );
      break;

   case /* 11 */ PPCG_FLAG_OP_SRAWI:
      /* xer_ca is 1 iff src was negative and bits_shifted_out != 
         0.  Since the shift amount is known to be in the range
         0 .. 31 inclusive the following seems viable:
         xer.ca == 1 iff the following is nonzero:
         (argL >>s 31)           -- either all 0s or all 1s
         & (argL & (1<<argR)-1)  -- the stuff shifted out */
      xer_ca
         = binop(
              Iop_And32,
              binop(Iop_Sar32, argL, mkU8(31)),
              binop( Iop_And32,
                     argL,
                     binop( Iop_Sub32,
                            binop(Iop_Shl32, mkU32(1),
                                             unop(Iop_32to8,argR)),
                            mkU32(1) )
                     )
              );
      xer_ca 
         = unop(Iop_1Uto32, binop(Iop_CmpNE32, xer_ca, mkU32(0)));
      break;
      
   default: 
      vex_printf("set_XER_CA: op = %u\n", op);
      vpanic("set_XER_CA(ppc)");
   }

   /* xer_ca MUST denote either 0 or 1, no other value allowed */
   putXER_CA( unop(Iop_32to8, xer_ca) );
}

static void set_XER_CA_64 ( UInt op, IRExpr* res,
                            IRExpr* argL, IRExpr* argR, IRExpr* oldca )
{
   IRExpr* xer_ca;
   vassert(op < PPCG_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irsb->tyenv,res)   == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv,argL)  == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv,argR)  == Ity_I64);
   vassert(typeOfIRExpr(irsb->tyenv,oldca) == Ity_I64);

   /* Incoming oldca is assumed to hold the values 0 or 1 only.  This
      seems reasonable given that it's always generated by
      getXER_CA32(), which masks it accordingly.  In any case it being
      0 or 1 is an invariant of the ppc guest state representation;
      if it has any other value, that invariant has been violated. */

   switch (op) {
   case /* 0 */ PPCG_FLAG_OP_ADD:
      /* res <u argL */
      xer_ca
         = unop(Iop_1Uto32, binop(Iop_CmpLT64U, res, argL));
      break;
      
   case /* 1 */ PPCG_FLAG_OP_ADDE:
      /* res <u argL || (old_ca==1 && res==argL) */
      xer_ca 
         = mkOR1( 
              binop(Iop_CmpLT64U, res, argL),
              mkAND1( 
                 binop(Iop_CmpEQ64, oldca, mkU64(1)),
                 binop(Iop_CmpEQ64, res, argL) 
                 ) 
              );
      xer_ca 
         = unop(Iop_1Uto32, xer_ca);
      break;
      
   case /* 8 */ PPCG_FLAG_OP_SUBFE:
      /* res <u argR || (old_ca==1 && res==argR) */
      xer_ca 
         = mkOR1( 
              binop(Iop_CmpLT64U, res, argR),
              mkAND1( 
                 binop(Iop_CmpEQ64, oldca, mkU64(1)),
                 binop(Iop_CmpEQ64, res, argR) 
              ) 
           );
      xer_ca 
         = unop(Iop_1Uto32, xer_ca);
      break;
      
   case /* 7 */ PPCG_FLAG_OP_SUBFC:
   case /* 9 */ PPCG_FLAG_OP_SUBFI:
      /* res <=u argR */
      xer_ca
         = unop(Iop_1Uto32, binop(Iop_CmpLE64U, res, argR));
      break;
      
      
   case /* 10 */ PPCG_FLAG_OP_SRAW:
      /* The shift amount is guaranteed to be in 0 .. 31 inclusive.
         If it is <= 31, behave like SRAWI; else XER.CA is the sign
         bit of argL. */
         /* This term valid for shift amount < 31 only */

      xer_ca
         = binop(
              Iop_And64,
              binop(Iop_Sar64, argL, mkU8(31)),
              binop( Iop_And64,
                     argL,
                     binop( Iop_Sub64,
                            binop(Iop_Shl64, mkU64(1),
                                             unop(Iop_64to8,argR)),
                            mkU64(1) )
              )
           );
      xer_ca 
         = IRExpr_Mux0X(
              /* shift amt > 31 ? */
              unop(Iop_1Uto8, binop(Iop_CmpLT64U, mkU64(31), argR)),
              /* no -- be like srawi */
              unop(Iop_1Uto32, binop(Iop_CmpNE64, xer_ca, mkU64(0))),
              /* yes -- get sign bit of argL */
              unop(Iop_64to32, binop(Iop_Shr64, argL, mkU8(63)))
           );
      break;
      
   case /* 11 */ PPCG_FLAG_OP_SRAWI:
      /* xer_ca is 1 iff src was negative and bits_shifted_out != 0.
         Since the shift amount is known to be in the range 0 .. 31
         inclusive the following seems viable:
         xer.ca == 1 iff the following is nonzero:
         (argL >>s 31)           -- either all 0s or all 1s
         & (argL & (1<<argR)-1)  -- the stuff shifted out */

      xer_ca
         = binop(
              Iop_And64,
              binop(Iop_Sar64, argL, mkU8(31)),
              binop( Iop_And64,
                     argL,
                     binop( Iop_Sub64,
                            binop(Iop_Shl64, mkU64(1),
                                             unop(Iop_64to8,argR)),
                            mkU64(1) )
              )
           );
      xer_ca 
         = unop(Iop_1Uto32, binop(Iop_CmpNE64, xer_ca, mkU64(0)));
      break;
      

   case /* 12 */ PPCG_FLAG_OP_SRAD:
      /* The shift amount is guaranteed to be in 0 .. 63 inclusive.
         If it is <= 63, behave like SRADI; else XER.CA is the sign
         bit of argL. */
         /* This term valid for shift amount < 63 only */

      xer_ca
         = binop(
              Iop_And64,
              binop(Iop_Sar64, argL, mkU8(63)),
              binop( Iop_And64,
                     argL,
                     binop( Iop_Sub64,
                            binop(Iop_Shl64, mkU64(1),
                                             unop(Iop_64to8,argR)),
                            mkU64(1) )
              )
           );
      xer_ca 
         = IRExpr_Mux0X(
              /* shift amt > 63 ? */
              unop(Iop_1Uto8, binop(Iop_CmpLT64U, mkU64(63), argR)),
              /* no -- be like sradi */
              unop(Iop_1Uto32, binop(Iop_CmpNE64, xer_ca, mkU64(0))),
              /* yes -- get sign bit of argL */
              unop(Iop_64to32, binop(Iop_Shr64, argL, mkU8(63)))
           );
      break;


   case /* 13 */ PPCG_FLAG_OP_SRADI:
      /* xer_ca is 1 iff src was negative and bits_shifted_out != 0.
         Since the shift amount is known to be in the range 0 .. 63
         inclusive, the following seems viable:
         xer.ca == 1 iff the following is nonzero:
         (argL >>s 63)           -- either all 0s or all 1s
         & (argL & (1<<argR)-1)  -- the stuff shifted out */

      xer_ca
         = binop(
              Iop_And64,
              binop(Iop_Sar64, argL, mkU8(63)),
              binop( Iop_And64,
                     argL,
                     binop( Iop_Sub64,
                            binop(Iop_Shl64, mkU64(1),
                                             unop(Iop_64to8,argR)),
                            mkU64(1) )
              )
           );
      xer_ca 
         = unop(Iop_1Uto32, binop(Iop_CmpNE64, xer_ca, mkU64(0)));
      break;

   default: 
      vex_printf("set_XER_CA: op = %u\n", op);
      vpanic("set_XER_CA(ppc64)");
   }

   /* xer_ca MUST denote either 0 or 1, no other value allowed */
   putXER_CA( unop(Iop_32to8, xer_ca) );
}

static void set_XER_CA ( IRType ty, UInt op, IRExpr* res,
                         IRExpr* argL, IRExpr* argR, IRExpr* oldca )
{
   if (ty == Ity_I32)
      set_XER_CA_32( op, res, argL, argR, oldca );
   else
      set_XER_CA_64( op, res, argL, argR, oldca );
}



/*------------------------------------------------------------*/
/*--- Read/write to guest-state                           --- */
/*------------------------------------------------------------*/

static IRExpr* /* :: Ity_I32/64 */ getGST ( PPC_GST reg )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   switch (reg) {
   case PPC_GST_SPRG3_RO:
      return IRExpr_Get( OFFB_SPRG3_RO, ty );

   case PPC_GST_CIA: 
      return IRExpr_Get( OFFB_CIA, ty );

   case PPC_GST_LR: 
      return IRExpr_Get( OFFB_LR, ty );

   case PPC_GST_CTR: 
      return IRExpr_Get( OFFB_CTR, ty );

   case PPC_GST_VRSAVE: 
      return IRExpr_Get( OFFB_VRSAVE, Ity_I32 );

   case PPC_GST_VSCR:
      return binop(Iop_And32, IRExpr_Get( OFFB_VSCR,Ity_I32 ),
                              mkU32(MASK_VSCR_VALID));

   case PPC_GST_CR: {
      /* Synthesise the entire CR into a single word.  Expensive. */
#     define FIELD(_n)                                               \
         binop(Iop_Shl32,                                            \
               unop(Iop_8Uto32,                                      \
                    binop(Iop_Or8,                                   \
                          binop(Iop_And8, getCR321(_n), mkU8(7<<1)), \
                          binop(Iop_And8, getCR0(_n), mkU8(1))       \
                    )                                                \
               ),                                                    \
               mkU8(4 * (7-(_n)))                                    \
         )
      return binop(Iop_Or32,
                   binop(Iop_Or32,
                         binop(Iop_Or32, FIELD(0), FIELD(1)),
                         binop(Iop_Or32, FIELD(2), FIELD(3))
                         ),
                   binop(Iop_Or32,
                         binop(Iop_Or32, FIELD(4), FIELD(5)),
                         binop(Iop_Or32, FIELD(6), FIELD(7))
                         )
                   );
#     undef FIELD
   }

   case PPC_GST_XER:
      return binop(Iop_Or32,
                   binop(Iop_Or32,
                         binop( Iop_Shl32, getXER_SO32(), mkU8(31)),
                         binop( Iop_Shl32, getXER_OV32(), mkU8(30))),
                   binop(Iop_Or32,
                         binop( Iop_Shl32, getXER_CA32(), mkU8(29)),
                         getXER_BC32()));

   default:
      vex_printf("getGST(ppc): reg = %u", reg);
      vpanic("getGST(ppc)");
   }
}

/* Get a masked word from the given reg */
static IRExpr* /* ::Ity_I32 */ getGST_masked ( PPC_GST reg, UInt mask )
{
   IRTemp val = newTemp(Ity_I32);
   vassert( reg < PPC_GST_MAX );
    
   switch (reg) {

   case PPC_GST_FPSCR: {
      /* Vex-generated code expects the FPSCR to be set as follows:
         all exceptions masked, round-to-nearest.
         This corresponds to a FPSCR value of 0x0. */

      /* We're only keeping track of the rounding mode,
         so if the mask isn't asking for this, just return 0x0 */
      if (mask & (MASK_FPSCR_RN|MASK_FPSCR_FPRF)) {
         assign( val, IRExpr_Get( OFFB_FPROUND, Ity_I32 ) );
      } else {
         assign( val, mkU32(0x0) );
      }
      break;
   }

   default:
      vex_printf("getGST_masked(ppc): reg = %u", reg);
      vpanic("getGST_masked(ppc)");
   }

   if (mask != 0xFFFFFFFF) {
      return binop(Iop_And32, mkexpr(val), mkU32(mask));
   } else {
      return mkexpr(val);
   }
}

/* Fetch the specified REG[FLD] nibble (as per IBM/hardware notation)
   and return it at the bottom of an I32; the top 27 bits are
   guaranteed to be zero. */
static IRExpr* /* ::Ity_I32 */ getGST_field ( PPC_GST reg, UInt fld )
{
   UInt shft, mask;

   vassert( fld < 8 );
   vassert( reg < PPC_GST_MAX );
   
   shft = 4*(7-fld);
   mask = 0xF<<shft;

   switch (reg) {
   case PPC_GST_XER:
      vassert(fld ==7);
      return binop(Iop_Or32,
                   binop(Iop_Or32,
                         binop(Iop_Shl32, getXER_SO32(), mkU8(3)),
                         binop(Iop_Shl32, getXER_OV32(), mkU8(2))),
                   binop(      Iop_Shl32, getXER_CA32(), mkU8(1)));
      break;

   default:
      if (shft == 0)
         return getGST_masked( reg, mask );
      else
         return binop(Iop_Shr32,
                      getGST_masked( reg, mask ),
                      mkU8(toUChar( shft )));
   }
}

static void putGST ( PPC_GST reg, IRExpr* src )
{
   IRType ty     = mode64 ? Ity_I64 : Ity_I32;
   IRType ty_src = typeOfIRExpr(irsb->tyenv,src );
   vassert( reg < PPC_GST_MAX );
   switch (reg) {
   case PPC_GST_IP_AT_SYSCALL: 
      vassert( ty_src == ty );
      stmt( IRStmt_Put( OFFB_IP_AT_SYSCALL, src ) );
      break;
   case PPC_GST_CIA: 
      vassert( ty_src == ty );
      stmt( IRStmt_Put( OFFB_CIA, src ) );
      break;
   case PPC_GST_LR: 
      vassert( ty_src == ty );
      stmt( IRStmt_Put( OFFB_LR, src ) );
      break;
   case PPC_GST_CTR: 
      vassert( ty_src == ty );
      stmt( IRStmt_Put( OFFB_CTR, src ) );
      break;
   case PPC_GST_VRSAVE: 
      vassert( ty_src == Ity_I32 );
      stmt( IRStmt_Put( OFFB_VRSAVE,src));
      break;
   case PPC_GST_VSCR:
      vassert( ty_src == Ity_I32 );
      stmt( IRStmt_Put( OFFB_VSCR,
                        binop(Iop_And32, src,
                              mkU32(MASK_VSCR_VALID)) ) );
      break;
   case PPC_GST_XER:
      vassert( ty_src == Ity_I32 );
      putXER_SO( unop(Iop_32to8, binop(Iop_Shr32, src, mkU8(31))) );
      putXER_OV( unop(Iop_32to8, binop(Iop_Shr32, src, mkU8(30))) );
      putXER_CA( unop(Iop_32to8, binop(Iop_Shr32, src, mkU8(29))) );
      putXER_BC( unop(Iop_32to8, src) );
      break;
      
   case PPC_GST_EMWARN:
      vassert( ty_src == Ity_I32 );
      stmt( IRStmt_Put( OFFB_EMWARN,src) );
      break;
      
   case PPC_GST_TISTART: 
      vassert( ty_src == ty );
      stmt( IRStmt_Put( OFFB_TISTART, src) );
      break;
      
   case PPC_GST_TILEN: 
      vassert( ty_src == ty );
      stmt( IRStmt_Put( OFFB_TILEN, src) );
      break;
      
   default:
      vex_printf("putGST(ppc): reg = %u", reg);
      vpanic("putGST(ppc)");
   }
}

/* Write masked src to the given reg */
static void putGST_masked ( PPC_GST reg, IRExpr* src, UInt mask )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   vassert( reg < PPC_GST_MAX );
   vassert( typeOfIRExpr(irsb->tyenv,src ) == Ity_I32 );
   
   switch (reg) {
   case PPC_GST_FPSCR: {
      /* Allow writes to Rounding Mode */
      if (mask & (MASK_FPSCR_RN|MASK_FPSCR_FPRF)) {
         /* construct new fpround from new and old values as per mask:
            new fpround = (src & (3 & mask)) | (fpround & (3 & ~mask)) */
         stmt( 
            IRStmt_Put( 
               OFFB_FPROUND,
               binop(
                  Iop_Or32, 
                  binop(Iop_And32, src, mkU32((MASK_FPSCR_RN|MASK_FPSCR_FPRF) & mask)),
                  binop(
                     Iop_And32, 
                     IRExpr_Get(OFFB_FPROUND,Ity_I32),
                     mkU32((MASK_FPSCR_RN|MASK_FPSCR_FPRF) & ~mask)
                  )
               )
            )
         );
      }

      /* Give EmWarn for attempted writes to:
         - Exception Controls
         - Non-IEEE Mode
      */
      if (mask & 0xFC) {  // Exception Control, Non-IEE mode
         VexEmWarn ew = EmWarn_PPCexns;

         /* If any of the src::exception_control bits are actually set,
            side-exit to the next insn, reporting the warning,
            so that Valgrind's dispatcher sees the warning. */
         putGST( PPC_GST_EMWARN, mkU32(ew) );
         stmt( 
            IRStmt_Exit(
               binop(Iop_CmpNE32, mkU32(ew), mkU32(EmWarn_NONE)),
               Ijk_EmWarn,
               mkSzConst( ty, nextInsnAddr()) ));
      }

      /* Ignore all other writes */
      break;
   }

   default:
      vex_printf("putGST_masked(ppc): reg = %u", reg);
      vpanic("putGST_masked(ppc)");
   }
}

/* Write the least significant nibble of src to the specified
   REG[FLD] (as per IBM/hardware notation). */
static void putGST_field ( PPC_GST reg, IRExpr* src, UInt fld )
{
   UInt shft, mask;

   vassert( typeOfIRExpr(irsb->tyenv,src ) == Ity_I32 );
   vassert( fld < 8 );
   vassert( reg < PPC_GST_MAX );
   
   shft = 4*(7-fld);
   mask = 0xF<<shft;

   switch (reg) {
   case PPC_GST_CR:
      putCR0  (fld, binop(Iop_And8, mkU8(1   ), unop(Iop_32to8, src)));
      putCR321(fld, binop(Iop_And8, mkU8(7<<1), unop(Iop_32to8, src)));
      break;

   default:
      if (shft == 0) {
         putGST_masked( reg, src, mask );
      } else {
         putGST_masked( reg,
                        binop(Iop_Shl32, src, mkU8(toUChar(shft))),
                        mask );
      }
   }
}



/*------------------------------------------------------------*/
/*--- Integer Instruction Translation                     --- */
/*------------------------------------------------------------*/

/*
  Integer Arithmetic Instructions
*/
static Bool dis_int_arith ( UInt theInstr )
{
   /* D-Form, XO-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar rD_addr = ifieldRegDS(theInstr);
   UChar rA_addr = ifieldRegA(theInstr);
   UInt  uimm16  = ifieldUIMM16(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UChar flag_OE = ifieldBIT10(theInstr);
   UInt  opc2    = ifieldOPClo9(theInstr);
   UChar flag_rC = ifieldBIT0(theInstr);

   Long   simm16 = extend_s_16to64(uimm16);
   IRType ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp rA     = newTemp(ty);
   IRTemp rB     = newTemp(ty);
   IRTemp rD     = newTemp(ty);

   Bool do_rc = False;

   assign( rA, getIReg(rA_addr) );
   assign( rB, getIReg(rB_addr) );         // XO-Form: rD, rA, rB

   switch (opc1) {
   /* D-Form */
   case 0x0C: // addic  (Add Immediate Carrying, PPC32 p351
      DIP("addic r%u,r%u,%d\n", rD_addr, rA_addr, (Int)simm16);
      assign( rD, binop( mkSzOp(ty, Iop_Add8), mkexpr(rA),
                         mkSzExtendS16(ty, uimm16) ) );
      set_XER_CA( ty, PPCG_FLAG_OP_ADD, 
                  mkexpr(rD), mkexpr(rA), mkSzExtendS16(ty, uimm16),
                  mkSzImm(ty, 0)/*old xer.ca, which is ignored*/ );
      break;
    
   case 0x0D: // addic. (Add Immediate Carrying and Record, PPC32 p352)
      DIP("addic. r%u,r%u,%d\n", rD_addr, rA_addr, (Int)simm16);
      assign( rD, binop( mkSzOp(ty, Iop_Add8), mkexpr(rA),
                         mkSzExtendS16(ty, uimm16) ) );
      set_XER_CA( ty, PPCG_FLAG_OP_ADD, 
                  mkexpr(rD), mkexpr(rA), mkSzExtendS16(ty, uimm16),
                  mkSzImm(ty, 0)/*old xer.ca, which is ignored*/ );
      do_rc = True;  // Always record to CR
      flag_rC = 1;
      break;

   case 0x0E: // addi   (Add Immediate, PPC32 p350)
      // li rD,val   == addi rD,0,val
      // la disp(rA) == addi rD,rA,disp
      if ( rA_addr == 0 ) {
         DIP("li r%u,%d\n", rD_addr, (Int)simm16);
         assign( rD, mkSzExtendS16(ty, uimm16) );
      } else {
         DIP("addi r%u,r%u,%d\n", rD_addr, rA_addr, (Int)simm16);
         assign( rD, binop( mkSzOp(ty, Iop_Add8), mkexpr(rA),
                            mkSzExtendS16(ty, uimm16) ) );
      }
      break;

   case 0x0F: // addis  (Add Immediate Shifted, PPC32 p353)
      // lis rD,val == addis rD,0,val
      if ( rA_addr == 0 ) {
         DIP("lis r%u,%d\n", rD_addr, (Int)simm16);
         assign( rD, mkSzExtendS32(ty, uimm16 << 16) );
      } else {
         DIP("addis r%u,r%u,0x%x\n", rD_addr, rA_addr, (Int)simm16);
         assign( rD, binop( mkSzOp(ty, Iop_Add8), mkexpr(rA),
                            mkSzExtendS32(ty, uimm16 << 16) ) );
      }
      break;

   case 0x07: // mulli    (Multiply Low Immediate, PPC32 p490)
      DIP("mulli r%u,r%u,%d\n", rD_addr, rA_addr, (Int)simm16);
      if (mode64)
         assign( rD, unop(Iop_128to64,
                          binop(Iop_MullS64, mkexpr(rA),
                                mkSzExtendS16(ty, uimm16))) );
      else
         assign( rD, unop(Iop_64to32,
                          binop(Iop_MullS32, mkexpr(rA),
                                mkSzExtendS16(ty, uimm16))) );
      break;

   case 0x08: // subfic   (Subtract from Immediate Carrying, PPC32 p540)
      DIP("subfic r%u,r%u,%d\n", rD_addr, rA_addr, (Int)simm16);
      // rD = simm16 - rA
      assign( rD, binop( mkSzOp(ty, Iop_Sub8),
                         mkSzExtendS16(ty, uimm16),
                         mkexpr(rA)) );
      set_XER_CA( ty, PPCG_FLAG_OP_SUBFI, 
                  mkexpr(rD), mkexpr(rA), mkSzExtendS16(ty, uimm16),
                  mkSzImm(ty, 0)/*old xer.ca, which is ignored*/ );
      break;

   /* XO-Form */
   case 0x1F:
      do_rc = True;    // All below record to CR
      
      switch (opc2) {
      case 0x10A: // add  (Add, PPC32 p347)
         DIP("add%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop( mkSzOp(ty, Iop_Add8),
                            mkexpr(rA), mkexpr(rB) ) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_ADD,
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;

      case 0x00A: // addc      (Add Carrying, PPC32 p348)
         DIP("addc%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop( mkSzOp(ty, Iop_Add8),
                            mkexpr(rA), mkexpr(rB)) );
         set_XER_CA( ty, PPCG_FLAG_OP_ADD, 
                     mkexpr(rD), mkexpr(rA), mkexpr(rB),
                     mkSzImm(ty, 0)/*old xer.ca, which is ignored*/ );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_ADD, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
         
      case 0x08A: { // adde      (Add Extended, PPC32 p349)
         IRTemp old_xer_ca = newTemp(ty);
         DIP("adde%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         // rD = rA + rB + XER[CA]
         assign( old_xer_ca, mkWidenFrom32(ty, getXER_CA32(), False) );
         assign( rD, binop( mkSzOp(ty, Iop_Add8), mkexpr(rA),
                            binop( mkSzOp(ty, Iop_Add8),
                                   mkexpr(rB), mkexpr(old_xer_ca))) );
         set_XER_CA( ty, PPCG_FLAG_OP_ADDE, 
                     mkexpr(rD), mkexpr(rA), mkexpr(rB),
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_ADDE, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
      }

      case 0x0EA: { // addme     (Add to Minus One Extended, PPC32 p354)
         IRTemp old_xer_ca = newTemp(ty);
         IRExpr *min_one;
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(ppc)(addme,rB_addr)\n");
            return False;
         }
         DIP("addme%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         // rD = rA + (-1) + XER[CA]
         // => Just another form of adde
         assign( old_xer_ca, mkWidenFrom32(ty, getXER_CA32(), False) );
         min_one = mkSzImm(ty, (Long)-1);
         assign( rD, binop( mkSzOp(ty, Iop_Add8), mkexpr(rA),
                            binop( mkSzOp(ty, Iop_Add8),
                                   min_one, mkexpr(old_xer_ca)) ));
         set_XER_CA( ty, PPCG_FLAG_OP_ADDE,
                     mkexpr(rD), mkexpr(rA), min_one,
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_ADDE, 
                        mkexpr(rD), mkexpr(rA), min_one );
         }
         break;
      }

      case 0x0CA: { // addze      (Add to Zero Extended, PPC32 p355)
         IRTemp old_xer_ca = newTemp(ty);
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(ppc)(addze,rB_addr)\n");
            return False;
         }
         DIP("addze%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         // rD = rA + (0) + XER[CA]
         // => Just another form of adde
         assign( old_xer_ca, mkWidenFrom32(ty, getXER_CA32(), False) );
         assign( rD, binop( mkSzOp(ty, Iop_Add8),
                            mkexpr(rA), mkexpr(old_xer_ca)) );
         set_XER_CA( ty, PPCG_FLAG_OP_ADDE, 
                     mkexpr(rD), mkexpr(rA), mkSzImm(ty, 0), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_ADDE, 
                        mkexpr(rD), mkexpr(rA), mkSzImm(ty, 0) );
         }
         break;
      }

      case 0x1EB: // divw       (Divide Word, PPC32 p388)
         DIP("divw%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         if (mode64) {
            /* Note:
               XER settings are mode independent, and reflect the 
               overflow of the low-order 32bit result
               CR0[LT|GT|EQ] are undefined if flag_rC && mode64
            */
            /* rD[hi32] are undefined: setting them to sign of lo32
                - makes set_CR0 happy */
            IRExpr* dividend = mk64lo32Sto64( mkexpr(rA) );
            IRExpr* divisor  = mk64lo32Sto64( mkexpr(rB) );
            assign( rD, mk64lo32Uto64( binop(Iop_DivS64, dividend,
                                                         divisor) ) );
            if (flag_OE) {
               set_XER_OV( ty, PPCG_FLAG_OP_DIVW, 
                           mkexpr(rD), dividend, divisor );
            }
         } else {
            assign( rD, binop(Iop_DivS32, mkexpr(rA), mkexpr(rB)) );
            if (flag_OE) {
               set_XER_OV( ty, PPCG_FLAG_OP_DIVW, 
                           mkexpr(rD), mkexpr(rA), mkexpr(rB) );
            }
         }
         /* Note:
            if (0x8000_0000 / -1) or (x / 0)
            => rD=undef, if(flag_rC) CR7=undef, if(flag_OE) XER_OV=1
            => But _no_ exception raised. */
         break;

      case 0x1CB: // divwu      (Divide Word Unsigned, PPC32 p389)
         DIP("divwu%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         if (mode64) {
            /* Note:
               XER settings are mode independent, and reflect the 
               overflow of the low-order 32bit result
               CR0[LT|GT|EQ] are undefined if flag_rC && mode64
            */
            IRExpr* dividend = mk64lo32Uto64( mkexpr(rA) );
            IRExpr* divisor  = mk64lo32Uto64( mkexpr(rB) );
            assign( rD, mk64lo32Uto64( binop(Iop_DivU64, dividend,
                                                         divisor) ) );
            if (flag_OE) {
               set_XER_OV( ty, PPCG_FLAG_OP_DIVWU, 
                           mkexpr(rD), dividend, divisor );
            }
         } else {
            assign( rD, binop(Iop_DivU32, mkexpr(rA), mkexpr(rB)) );
            if (flag_OE) {
               set_XER_OV( ty, PPCG_FLAG_OP_DIVWU, 
                           mkexpr(rD), mkexpr(rA), mkexpr(rB) );
            }
         }
         /* Note: ditto comment divw, for (x / 0) */
         break;

      case 0x04B: // mulhw      (Multiply High Word, PPC32 p488)
         if (flag_OE != 0) {
            vex_printf("dis_int_arith(ppc)(mulhw,flag_OE)\n");
            return False;
         }
         DIP("mulhw%s r%u,r%u,r%u\n", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         if (mode64) {
            /* rD[hi32] are undefined: setting them to sign of lo32
                - makes set_CR0 happy */
            assign( rD, binop(Iop_Sar64,
                           binop(Iop_Mul64,
                                 mk64lo32Sto64( mkexpr(rA) ),
                                 mk64lo32Sto64( mkexpr(rB) )),
                              mkU8(32)) );
         } else {
            assign( rD, unop(Iop_64HIto32,
                             binop(Iop_MullS32,
                                   mkexpr(rA), mkexpr(rB))) );
         }
         break;

      case 0x00B: // mulhwu    (Multiply High Word Unsigned, PPC32 p489)
         if (flag_OE != 0) {
            vex_printf("dis_int_arith(ppc)(mulhwu,flag_OE)\n");
            return False;
         }
         DIP("mulhwu%s r%u,r%u,r%u\n", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         if (mode64) {
            /* rD[hi32] are undefined: setting them to sign of lo32
                - makes set_CR0 happy */
            assign( rD, binop(Iop_Sar64,
                           binop(Iop_Mul64,
                                 mk64lo32Uto64( mkexpr(rA) ),
                                 mk64lo32Uto64( mkexpr(rB) ) ),
                              mkU8(32)) );
         } else {
            assign( rD, unop(Iop_64HIto32, 
                             binop(Iop_MullU32,
                                   mkexpr(rA), mkexpr(rB))) );
         }
         break;
         
      case 0x0EB: // mullw      (Multiply Low Word, PPC32 p491)
         DIP("mullw%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         if (mode64) {
            /* rD[hi32] are undefined: setting them to sign of lo32
                - set_XER_OV() and set_CR0() depend on this */
            IRExpr *a = unop(Iop_64to32, mkexpr(rA) );
            IRExpr *b = unop(Iop_64to32, mkexpr(rB) );
            assign( rD, binop(Iop_MullS32, a, b) );
            if (flag_OE) {
               set_XER_OV( ty, PPCG_FLAG_OP_MULLW, 
                           mkexpr(rD),
                           unop(Iop_32Uto64, a), unop(Iop_32Uto64, b) );
            }
         } else {
            assign( rD, unop(Iop_64to32,
                             binop(Iop_MullU32,
                                   mkexpr(rA), mkexpr(rB))) );
            if (flag_OE) {
               set_XER_OV( ty, PPCG_FLAG_OP_MULLW, 
                           mkexpr(rD), mkexpr(rA), mkexpr(rB) );
            }
         }
         break;

      case 0x068: // neg        (Negate, PPC32 p493)
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(ppc)(neg,rB_addr)\n");
            return False;
         }
         DIP("neg%s%s r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr);
         // rD = (~rA) + 1
         assign( rD, binop( mkSzOp(ty, Iop_Add8),
                            unop( mkSzOp(ty, Iop_Not8), mkexpr(rA) ),
                            mkSzImm(ty, 1)) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_NEG, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;

      case 0x028: // subf       (Subtract From, PPC32 p537)
         DIP("subf%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         // rD = rB - rA
         assign( rD, binop( mkSzOp(ty, Iop_Sub8),
                            mkexpr(rB), mkexpr(rA)) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_SUBF, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;

      case 0x008: // subfc      (Subtract from Carrying, PPC32 p538)
         DIP("subfc%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         // rD = rB - rA
         assign( rD, binop( mkSzOp(ty, Iop_Sub8),
                            mkexpr(rB), mkexpr(rA)) );
         set_XER_CA( ty, PPCG_FLAG_OP_SUBFC, 
                     mkexpr(rD), mkexpr(rA), mkexpr(rB),
                     mkSzImm(ty, 0)/*old xer.ca, which is ignored*/ );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_SUBFC, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
         
      case 0x088: {// subfe      (Subtract from Extended, PPC32 p539)
         IRTemp old_xer_ca = newTemp(ty);
         DIP("subfe%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         // rD = (log not)rA + rB + XER[CA]
         assign( old_xer_ca, mkWidenFrom32(ty, getXER_CA32(), False) );
         assign( rD, binop( mkSzOp(ty, Iop_Add8),
                            unop( mkSzOp(ty, Iop_Not8), mkexpr(rA)),
                            binop( mkSzOp(ty, Iop_Add8),
                                   mkexpr(rB), mkexpr(old_xer_ca))) );
         set_XER_CA( ty, PPCG_FLAG_OP_SUBFE, 
                     mkexpr(rD), mkexpr(rA), mkexpr(rB), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_SUBFE, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
      }

      case 0x0E8: { // subfme    (Subtract from -1 Extended, PPC32 p541)
         IRTemp old_xer_ca = newTemp(ty);
         IRExpr *min_one;
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(ppc)(subfme,rB_addr)\n");
            return False;
         }
         DIP("subfme%s%s r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr);
         // rD = (log not)rA + (-1) + XER[CA]
         // => Just another form of subfe
         assign( old_xer_ca, mkWidenFrom32(ty, getXER_CA32(), False) );
         min_one = mkSzImm(ty, (Long)-1);
         assign( rD, binop( mkSzOp(ty, Iop_Add8),
                            unop( mkSzOp(ty, Iop_Not8), mkexpr(rA)),
                            binop( mkSzOp(ty, Iop_Add8),
                                   min_one, mkexpr(old_xer_ca))) );
         set_XER_CA( ty, PPCG_FLAG_OP_SUBFE,
                     mkexpr(rD), mkexpr(rA), min_one,
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_SUBFE, 
                        mkexpr(rD), mkexpr(rA), min_one );
         }
         break;
      }

      case 0x0C8: { // subfze  (Subtract from Zero Extended, PPC32 p542)
         IRTemp old_xer_ca = newTemp(ty);
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(ppc)(subfze,rB_addr)\n");
            return False;
         }
         DIP("subfze%s%s r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr);
         // rD = (log not)rA + (0) + XER[CA]
         // => Just another form of subfe
         assign( old_xer_ca, mkWidenFrom32(ty, getXER_CA32(), False) );
         assign( rD, binop( mkSzOp(ty, Iop_Add8),
                           unop( mkSzOp(ty, Iop_Not8),
                                 mkexpr(rA)), mkexpr(old_xer_ca)) );
         set_XER_CA( ty, PPCG_FLAG_OP_SUBFE,
                     mkexpr(rD), mkexpr(rA), mkSzImm(ty, 0), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_SUBFE,
                        mkexpr(rD), mkexpr(rA), mkSzImm(ty, 0) );
         }
         break;
      }


      /* 64bit Arithmetic */
      case 0x49:  // mulhd (Multiply High DWord, PPC64 p539)
         if (flag_OE != 0) {
            vex_printf("dis_int_arith(ppc)(mulhd,flagOE)\n");
            return False;
         }
         DIP("mulhd%s r%u,r%u,r%u\n", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         assign( rD, unop(Iop_128HIto64, 
                          binop(Iop_MullS64,
                                mkexpr(rA), mkexpr(rB))) );

         break;

      case 0x9:   // mulhdu  (Multiply High DWord Unsigned, PPC64 p540)
         if (flag_OE != 0) {
            vex_printf("dis_int_arith(ppc)(mulhdu,flagOE)\n");
            return False;
         }
         DIP("mulhdu%s r%u,r%u,r%u\n", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         assign( rD, unop(Iop_128HIto64, 
                          binop(Iop_MullU64,
                                mkexpr(rA), mkexpr(rB))) );
         break;

      case 0xE9:  // mulld (Multiply Low DWord, PPC64 p543)
         DIP("mulld%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop(Iop_Mul64, mkexpr(rA), mkexpr(rB)) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_MULLW, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;

      case 0x1E9: // divd (Divide DWord, PPC64 p419)
         DIP("divd%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop(Iop_DivS64, mkexpr(rA), mkexpr(rB)) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_DIVW, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
         /* Note:
            if (0x8000_0000_0000_0000 / -1) or (x / 0)
            => rD=undef, if(flag_rC) CR7=undef, if(flag_OE) XER_OV=1
            => But _no_ exception raised. */

      case 0x1C9: // divdu (Divide DWord Unsigned, PPC64 p420)
         DIP("divdu%s%s r%u,r%u,r%u\n",
             flag_OE ? "o" : "", flag_rC ? ".":"",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop(Iop_DivU64, mkexpr(rA), mkexpr(rB)) );
         if (flag_OE) {
            set_XER_OV( ty, PPCG_FLAG_OP_DIVWU, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
         /* Note: ditto comment divd, for (x / 0) */

      default:
         vex_printf("dis_int_arith(ppc)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_int_arith(ppc)(opc1)\n");
      return False;
   }

   putIReg( rD_addr, mkexpr(rD) );

   if (do_rc && flag_rC) {
      set_CR0( mkexpr(rD) );
   }
   return True;
}



/*
  Integer Compare Instructions
*/
static Bool dis_int_cmp ( UInt theInstr )
{
   /* D-Form, X-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar crfD    = toUChar( IFIELD( theInstr, 23, 3 ) );
   UChar b22     = toUChar( IFIELD( theInstr, 22, 1 ) );
   UChar flag_L  = toUChar( IFIELD( theInstr, 21, 1 ) );
   UChar rA_addr = ifieldRegA(theInstr);
   UInt  uimm16  = ifieldUIMM16(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b0      = ifieldBIT0(theInstr);

   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IRExpr *a = getIReg(rA_addr);
   IRExpr *b;

   if (!mode64 && flag_L==1) {  // L==1 invalid for 32 bit.
      vex_printf("dis_int_cmp(ppc)(flag_L)\n");
      return False;
   }
   
   if (b22 != 0) {
      vex_printf("dis_int_cmp(ppc)(b22)\n");
      return False;
   }
   
   switch (opc1) {
   case 0x0B: // cmpi (Compare Immediate, PPC32 p368)
      DIP("cmpi cr%u,%u,r%u,%d\n", crfD, flag_L, rA_addr,
          (Int)extend_s_16to32(uimm16));
      b = mkSzExtendS16( ty, uimm16 );
      if (flag_L == 1) {
         putCR321(crfD, unop(Iop_64to8, binop(Iop_CmpORD64S, a, b)));
      } else {
         a = mkNarrowTo32( ty, a );
         b = mkNarrowTo32( ty, b );
         putCR321(crfD, unop(Iop_32to8, binop(Iop_CmpORD32S, a, b)));
      }
      putCR0( crfD, getXER_SO() );
      break;
      
   case 0x0A: // cmpli (Compare Logical Immediate, PPC32 p370)
      DIP("cmpli cr%u,%u,r%u,0x%x\n", crfD, flag_L, rA_addr, uimm16);
      b = mkSzImm( ty, uimm16 );
      if (flag_L == 1) {
         putCR321(crfD, unop(Iop_64to8, binop(Iop_CmpORD64U, a, b)));
      } else {
         a = mkNarrowTo32( ty, a );
         b = mkNarrowTo32( ty, b );
         putCR321(crfD, unop(Iop_32to8, binop(Iop_CmpORD32U, a, b)));
      }
      putCR0( crfD, getXER_SO() );
      break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_cmp(ppc)(0x1F,b0)\n");
         return False;
      }
      b = getIReg(rB_addr);

      switch (opc2) {
      case 0x000: // cmp (Compare, PPC32 p367)
         DIP("cmp cr%u,%u,r%u,r%u\n", crfD, flag_L, rA_addr, rB_addr);
         /* Comparing a reg with itself produces a result which
            doesn't depend on the contents of the reg.  Therefore
            remove the false dependency, which has been known to cause
            memcheck to produce false errors. */
         if (rA_addr == rB_addr)
            a = b = typeOfIRExpr(irsb->tyenv,a) == Ity_I64
                    ? mkU64(0)  : mkU32(0);
         if (flag_L == 1) {
            putCR321(crfD, unop(Iop_64to8, binop(Iop_CmpORD64S, a, b)));
         } else {
            a = mkNarrowTo32( ty, a );
            b = mkNarrowTo32( ty, b );
            putCR321(crfD, unop(Iop_32to8,binop(Iop_CmpORD32S, a, b)));
         }
         putCR0( crfD, getXER_SO() );
         break;
         
      case 0x020: // cmpl (Compare Logical, PPC32 p369)
         DIP("cmpl cr%u,%u,r%u,r%u\n", crfD, flag_L, rA_addr, rB_addr);
         /* Comparing a reg with itself produces a result which
            doesn't depend on the contents of the reg.  Therefore
            remove the false dependency, which has been known to cause
            memcheck to produce false errors. */
         if (rA_addr == rB_addr)
            a = b = typeOfIRExpr(irsb->tyenv,a) == Ity_I64
                    ? mkU64(0)  : mkU32(0);
         if (flag_L == 1) {
            putCR321(crfD, unop(Iop_64to8, binop(Iop_CmpORD64U, a, b)));
         } else {
            a = mkNarrowTo32( ty, a );
            b = mkNarrowTo32( ty, b );
            putCR321(crfD, unop(Iop_32to8, binop(Iop_CmpORD32U, a, b)));
         }
         putCR0( crfD, getXER_SO() );
         break;

      default:
         vex_printf("dis_int_cmp(ppc)(opc2)\n");
         return False;
      }
      break;
      
   default:
      vex_printf("dis_int_cmp(ppc)(opc1)\n");
      return False;
   }
   
   return True;
}


/*
  Integer Logical Instructions
*/
static Bool dis_int_logic ( UInt theInstr )
{
   /* D-Form, X-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar rS_addr = ifieldRegDS(theInstr);
   UChar rA_addr = ifieldRegA(theInstr);
   UInt  uimm16  = ifieldUIMM16(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar flag_rC = ifieldBIT0(theInstr);
   
   IRType ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp rS     = newTemp(ty);
   IRTemp rA     = newTemp(ty);
   IRTemp rB     = newTemp(ty);
   IRExpr* irx;
   Bool do_rc    = False;

   assign( rS, getIReg(rS_addr) );
   assign( rB, getIReg(rB_addr) );
   
   switch (opc1) {
   case 0x1C: // andi. (AND Immediate, PPC32 p358)
      DIP("andi. r%u,r%u,0x%x\n", rA_addr, rS_addr, uimm16);
      assign( rA, binop( mkSzOp(ty, Iop_And8), mkexpr(rS),
                         mkSzImm(ty, uimm16)) );
      do_rc = True;  // Always record to CR
      flag_rC = 1;
      break;
      
   case 0x1D: // andis. (AND Immediate Shifted, PPC32 p359)
      DIP("andis r%u,r%u,0x%x\n", rA_addr, rS_addr, uimm16);
      assign( rA, binop( mkSzOp(ty, Iop_And8), mkexpr(rS),
                         mkSzImm(ty, uimm16 << 16)) );
      do_rc = True;  // Always record to CR
      flag_rC = 1;
      break;

   case 0x18: // ori (OR Immediate, PPC32 p497)
      DIP("ori r%u,r%u,0x%x\n", rA_addr, rS_addr, uimm16);
      assign( rA, binop( mkSzOp(ty, Iop_Or8), mkexpr(rS),
                         mkSzImm(ty, uimm16)) );
      break;

   case 0x19: // oris (OR Immediate Shifted, PPC32 p498)
      DIP("oris r%u,r%u,0x%x\n", rA_addr, rS_addr, uimm16);
      assign( rA, binop( mkSzOp(ty, Iop_Or8), mkexpr(rS),
                         mkSzImm(ty, uimm16 << 16)) );
      break;

   case 0x1A: // xori (XOR Immediate, PPC32 p550)
      DIP("xori r%u,r%u,0x%x\n", rA_addr, rS_addr, uimm16);
      assign( rA, binop( mkSzOp(ty, Iop_Xor8), mkexpr(rS),
                         mkSzImm(ty, uimm16)) );
      break;

   case 0x1B: // xoris (XOR Immediate Shifted, PPC32 p551)
      DIP("xoris r%u,r%u,0x%x\n", rA_addr, rS_addr, uimm16);
      assign( rA, binop( mkSzOp(ty, Iop_Xor8), mkexpr(rS),
                         mkSzImm(ty, uimm16 << 16)) );
      break;

   /* X Form */
   case 0x1F:
      do_rc = True;    // All below record to CR

      switch (opc2) {
      case 0x01C: // and (AND, PPC32 p356)
         DIP("and%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         assign(rA, binop( mkSzOp(ty, Iop_And8),
                           mkexpr(rS), mkexpr(rB)));
         break;
         
      case 0x03C: // andc (AND with Complement, PPC32 p357)
         DIP("andc%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         assign(rA, binop( mkSzOp(ty, Iop_And8), mkexpr(rS),
                           unop( mkSzOp(ty, Iop_Not8),
                                 mkexpr(rB))));
         break;
         
      case 0x01A: { // cntlzw (Count Leading Zeros Word, PPC32 p371)
         IRExpr* lo32;
         if (rB_addr!=0) {
            vex_printf("dis_int_logic(ppc)(cntlzw,rB_addr)\n");
            return False;
         }
         DIP("cntlzw%s r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr);
         
         // mode64: count in low word only
         lo32 = mode64 ? unop(Iop_64to32, mkexpr(rS)) : mkexpr(rS);
         
         // Iop_Clz32 undefined for arg==0, so deal with that case:
         irx =  binop(Iop_CmpNE32, lo32, mkU32(0));
         assign(rA, mkWidenFrom32(ty,
                         IRExpr_Mux0X( unop(Iop_1Uto8, irx),
                                       mkU32(32),
                                       unop(Iop_Clz32, lo32)),
                         False));

         // TODO: alternatively: assign(rA, verbose_Clz32(rS));
         break;
      }
         
      case 0x11C: // eqv (Equivalent, PPC32 p396)
         DIP("eqv%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         assign( rA, unop( mkSzOp(ty, Iop_Not8),
                           binop( mkSzOp(ty, Iop_Xor8),
                                  mkexpr(rS), mkexpr(rB))) );
         break;

      case 0x3BA: // extsb (Extend Sign Byte, PPC32 p397
         if (rB_addr!=0) {
            vex_printf("dis_int_logic(ppc)(extsb,rB_addr)\n");
            return False;
         }
         DIP("extsb%s r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr);
         if (mode64)
            assign( rA, unop(Iop_8Sto64, unop(Iop_64to8, mkexpr(rS))) );
         else
            assign( rA, unop(Iop_8Sto32, unop(Iop_32to8, mkexpr(rS))) );
         break;

      case 0x39A: // extsh (Extend Sign Half Word, PPC32 p398)
         if (rB_addr!=0) {
            vex_printf("dis_int_logic(ppc)(extsh,rB_addr)\n");
            return False;
         }
         DIP("extsh%s r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr);
         if (mode64)
            assign( rA, unop(Iop_16Sto64,
                             unop(Iop_64to16, mkexpr(rS))) );
         else
            assign( rA, unop(Iop_16Sto32,
                             unop(Iop_32to16, mkexpr(rS))) );
         break;

      case 0x1DC: // nand (NAND, PPC32 p492)
         DIP("nand%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         assign( rA, unop( mkSzOp(ty, Iop_Not8),
                           binop( mkSzOp(ty, Iop_And8),
                                  mkexpr(rS), mkexpr(rB))) );
         break;
         
      case 0x07C: // nor (NOR, PPC32 p494)
         DIP("nor%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         assign( rA, unop( mkSzOp(ty, Iop_Not8),
                           binop( mkSzOp(ty, Iop_Or8),
                                  mkexpr(rS), mkexpr(rB))) );
         break;

      case 0x1BC: // or (OR, PPC32 p495)
         if ((!flag_rC) && rS_addr == rB_addr) {
            DIP("mr r%u,r%u\n", rA_addr, rS_addr);
            assign( rA, mkexpr(rS) );
         } else {
            DIP("or%s r%u,r%u,r%u\n",
                flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
            assign( rA, binop( mkSzOp(ty, Iop_Or8),
                               mkexpr(rS), mkexpr(rB)) );
         }
         break;

      case 0x19C: // orc  (OR with Complement, PPC32 p496)
         DIP("orc%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         assign( rA, binop( mkSzOp(ty, Iop_Or8), mkexpr(rS),
                            unop(mkSzOp(ty, Iop_Not8), mkexpr(rB))));
         break;
         
      case 0x13C: // xor (XOR, PPC32 p549)
         DIP("xor%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         assign( rA, binop( mkSzOp(ty, Iop_Xor8),
                            mkexpr(rS), mkexpr(rB)) );
         break;


      /* 64bit Integer Logical Instructions */
      case 0x3DA: // extsw (Extend Sign Word, PPC64 p430)
         if (rB_addr!=0) {
            vex_printf("dis_int_logic(ppc)(extsw,rB_addr)\n");
            return False;
         }
         DIP("extsw%s r%u,r%u\n", flag_rC ? ".":"", rA_addr, rS_addr);
         assign(rA, unop(Iop_32Sto64, unop(Iop_64to32, mkexpr(rS))));
         break;

      case 0x03A: // cntlzd (Count Leading Zeros DWord, PPC64 p401)
         if (rB_addr!=0) {
            vex_printf("dis_int_logic(ppc)(cntlzd,rB_addr)\n");
            return False;
         }
         DIP("cntlzd%s r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr);
         // Iop_Clz64 undefined for arg==0, so deal with that case:
         irx =  binop(Iop_CmpNE64, mkexpr(rS), mkU64(0));
         assign(rA, IRExpr_Mux0X( unop(Iop_1Uto8, irx),
                                  mkU64(64),
                                  unop(Iop_Clz64, mkexpr(rS)) ));
         // TODO: alternatively: assign(rA, verbose_Clz64(rS));
         break;

      case 0x1FC: // cmpb (Power6: compare bytes)
         DIP("cmpb r%u,r%u,r%u\n", rA_addr, rS_addr, rB_addr);

         if (mode64)
            assign( rA, unop( Iop_V128to64,
                              binop( Iop_CmpEQ8x16,
                                     binop( Iop_64HLtoV128, mkU64(0), mkexpr(rS) ),
                                     binop( Iop_64HLtoV128, mkU64(0), mkexpr(rB) )
                                     )) );
         else
            assign( rA, unop( Iop_V128to32,
                              binop( Iop_CmpEQ8x16,
                                     unop( Iop_32UtoV128, mkexpr(rS) ),
                                     unop( Iop_32UtoV128, mkexpr(rB) )
                                     )) );
         break;

      case 0x2DF: { // mftgpr (move floating-point to general purpose register)
         IRTemp frB = newTemp(Ity_F64);
         DIP("mftgpr r%u,fr%u\n", rS_addr, rB_addr);

         assign( frB, getFReg(rB_addr));  // always F64
         if (mode64)
            assign( rA, unop( Iop_ReinterpF64asI64, mkexpr(frB)) );
         else
            assign( rA, unop( Iop_64to32, unop( Iop_ReinterpF64asI64, mkexpr(frB))) );

         putIReg( rS_addr, mkexpr(rA));
         return True;
      }

      case 0x25F: { // mffgpr (move floating-point from general purpose register)
         IRTemp frA = newTemp(Ity_F64);
         DIP("mffgpr fr%u,r%u\n", rS_addr, rB_addr);

         if (mode64)
            assign( frA, unop( Iop_ReinterpI64asF64, mkexpr(rB)) );
         else
            assign( frA, unop( Iop_ReinterpI64asF64, unop( Iop_32Uto64, mkexpr(rB))) );

         putFReg( rS_addr, mkexpr(frA));
         return True;
      }

      default:
         vex_printf("dis_int_logic(ppc)(opc2)\n");
         return False;
      }
      break;
      
   default:
      vex_printf("dis_int_logic(ppc)(opc1)\n");
      return False;
   }

   putIReg( rA_addr, mkexpr(rA) );

   if (do_rc && flag_rC) {
      set_CR0( mkexpr(rA) );
   }
   return True;
}

/*
  Integer Parity Instructions
*/
static Bool dis_int_parity ( UInt theInstr )
{
   /* X-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar rS_addr = ifieldRegDS(theInstr);
   UChar rA_addr = ifieldRegA(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b0      = ifieldBIT0(theInstr);
   IRType ty     = mode64 ? Ity_I64 : Ity_I32;

   IRTemp rS     = newTemp(ty);
   IRTemp rA     = newTemp(ty);
   IRTemp iTot1  = newTemp(Ity_I32);
   IRTemp iTot2  = newTemp(Ity_I32);
   IRTemp iTot3  = newTemp(Ity_I32);
   IRTemp iTot4  = newTemp(Ity_I32);
   IRTemp iTot5  = newTemp(Ity_I32);
   IRTemp iTot6  = newTemp(Ity_I32);
   IRTemp iTot7  = newTemp(Ity_I32);
   IRTemp iTot8  = newTemp(Ity_I32);
   IRTemp rS1    = newTemp(ty);
   IRTemp rS2    = newTemp(ty);
   IRTemp rS3    = newTemp(ty);
   IRTemp rS4    = newTemp(ty);
   IRTemp rS5    = newTemp(ty);
   IRTemp rS6    = newTemp(ty);
   IRTemp rS7    = newTemp(ty);
   IRTemp iHi    = newTemp(Ity_I32);
   IRTemp iLo    = newTemp(Ity_I32);
   IROp to_bit   = (mode64 ? Iop_64to1 : Iop_32to1);
   IROp shr_op   = (mode64 ? Iop_Shr64 : Iop_Shr32);

   if (opc1 != 0x1f || rB_addr || b0) {
      vex_printf("dis_int_parity(ppc)(0x1F,opc1:rB|b0)\n");
      return False;
   }

   assign( rS, getIReg(rS_addr) );

   switch (opc2) {
   case 0xba:  // prtyd (Parity Doubleword, ISA 2.05 p320)
      DIP("prtyd r%u,r%u\n", rA_addr, rS_addr);
      assign( iTot1, unop(Iop_1Uto32, unop(to_bit, mkexpr(rS))) );
      assign( rS1, binop(shr_op, mkexpr(rS), mkU8(8)) );
      assign( iTot2, binop(Iop_Add32,
                           unop(Iop_1Uto32, unop(to_bit, mkexpr(rS1))),
                           mkexpr(iTot1)) );
      assign( rS2, binop(shr_op, mkexpr(rS1), mkU8(8)) );
      assign( iTot3, binop(Iop_Add32,
                           unop(Iop_1Uto32, unop(to_bit, mkexpr(rS2))),
                           mkexpr(iTot2)) );
      assign( rS3, binop(shr_op, mkexpr(rS2), mkU8(8)) );
      assign( iTot4, binop(Iop_Add32,
                           unop(Iop_1Uto32, unop(to_bit, mkexpr(rS3))),
                           mkexpr(iTot3)) );
      if (mode64) {
         assign( rS4, binop(shr_op, mkexpr(rS3), mkU8(8)) );
         assign( iTot5, binop(Iop_Add32,
                              unop(Iop_1Uto32, unop(to_bit, mkexpr(rS4))),
                              mkexpr(iTot4)) );
         assign( rS5, binop(shr_op, mkexpr(rS4), mkU8(8)) );
         assign( iTot6, binop(Iop_Add32,
                              unop(Iop_1Uto32, unop(to_bit, mkexpr(rS5))),
                              mkexpr(iTot5)) );
         assign( rS6, binop(shr_op, mkexpr(rS5), mkU8(8)) );
         assign( iTot7, binop(Iop_Add32,
                              unop(Iop_1Uto32, unop(to_bit, mkexpr(rS6))),
                              mkexpr(iTot6)) );
         assign( rS7, binop(shr_op, mkexpr(rS6), mkU8(8)) );
         assign( iTot8, binop(Iop_Add32,
                              unop(Iop_1Uto32, unop(to_bit, mkexpr(rS7))),
                              mkexpr(iTot7)) );
         assign( rA, unop(Iop_32Uto64,
                          binop(Iop_And32, mkexpr(iTot8), mkU32(1))) );
      } else
         assign( rA, mkexpr(iTot4) );

      break;
   case 0x9a:  // prtyw (Parity Word, ISA 2.05 p320)
      assign( iTot1, unop(Iop_1Uto32, unop(to_bit, mkexpr(rS))) );
      assign( rS1, binop(shr_op, mkexpr(rS), mkU8(8)) );
      assign( iTot2, binop(Iop_Add32,
                           unop(Iop_1Uto32, unop(to_bit, mkexpr(rS1))),
                           mkexpr(iTot1)) );
      assign( rS2, binop(shr_op, mkexpr(rS1), mkU8(8)) );
      assign( iTot3, binop(Iop_Add32,
                           unop(Iop_1Uto32, unop(to_bit, mkexpr(rS2))),
                           mkexpr(iTot2)) );
      assign( rS3, binop(shr_op, mkexpr(rS2), mkU8(8)) );
      assign( iTot4, binop(Iop_Add32,
                           unop(Iop_1Uto32, unop(to_bit, mkexpr(rS3))),
                           mkexpr(iTot3)) );
      assign( iLo, unop(Iop_1Uto32, unop(Iop_32to1, mkexpr(iTot4) )) );

      if (mode64) {
         assign( rS4, binop(shr_op, mkexpr(rS3), mkU8(8)) );
         assign( iTot5, unop(Iop_1Uto32, unop(to_bit, mkexpr(rS4))) );
         assign( rS5, binop(shr_op, mkexpr(rS4), mkU8(8)) );
         assign( iTot6, binop(Iop_Add32,
                              unop(Iop_1Uto32, unop(to_bit, mkexpr(rS5))),
                              mkexpr(iTot5)) );
         assign( rS6, binop(shr_op, mkexpr(rS5), mkU8(8)) );
         assign( iTot7, binop(Iop_Add32,
                              unop(Iop_1Uto32, unop(to_bit, mkexpr(rS6))),
                              mkexpr(iTot6)) );
         assign( rS7, binop(shr_op, mkexpr(rS6), mkU8(8)));
         assign( iTot8, binop(Iop_Add32,
                              unop(Iop_1Uto32, unop(to_bit, mkexpr(rS7))),
                              mkexpr(iTot7)) );
         assign( iHi, binop(Iop_And32, mkU32(1), mkexpr(iTot8)) ),
            assign( rA, binop(Iop_32HLto64, mkexpr(iHi), mkexpr(iLo)) );
      } else
         assign( rA, binop(Iop_Or32, mkU32(0), mkexpr(iLo)) );
      break;
   default:
      vex_printf("dis_int_parity(ppc)(opc2)\n");
      return False;
   }

   putIReg( rA_addr, mkexpr(rA) );

   return True;
}


/*
  Integer Rotate Instructions
*/
static Bool dis_int_rot ( UInt theInstr )
{
   /* M-Form, MDS-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar rS_addr = ifieldRegDS(theInstr);
   UChar rA_addr = ifieldRegA(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UChar sh_imm  = rB_addr;
   UChar MaskBeg = toUChar( IFIELD( theInstr, 6, 5 ) );
   UChar MaskEnd = toUChar( IFIELD( theInstr, 1, 5 ) );
   UChar msk_imm = toUChar( IFIELD( theInstr, 5, 6 ) );
   UChar opc2    = toUChar( IFIELD( theInstr, 2, 3 ) );
   UChar b1      = ifieldBIT1(theInstr);
   UChar flag_rC = ifieldBIT0(theInstr);

   IRType ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp rS     = newTemp(ty);
   IRTemp rA     = newTemp(ty);
   IRTemp rB     = newTemp(ty);
   IRTemp rot    = newTemp(ty);
   IRExpr *r;
   UInt   mask32;
   ULong  mask64;

   assign( rS, getIReg(rS_addr) );
   assign( rB, getIReg(rB_addr) );

   switch (opc1) {
   case 0x14: {
      // rlwimi (Rotate Left Word Imm then Mask Insert, PPC32 p500)
      DIP("rlwimi%s r%u,r%u,%d,%d,%d\n", flag_rC ? ".":"",
          rA_addr, rS_addr, sh_imm, MaskBeg, MaskEnd);
      if (mode64) {
         // tmp32 = (ROTL(rS_Lo32, Imm)
         // rA = ((tmp32 || tmp32) & mask64) | (rA & ~mask64)
         mask64 = MASK64(31-MaskEnd, 31-MaskBeg);
         r = ROTL( unop(Iop_64to32, mkexpr(rS) ), mkU8(sh_imm) );
         r = unop(Iop_32Uto64, r);
         assign( rot, binop(Iop_Or64, r,
                            binop(Iop_Shl64, r, mkU8(32))) );
         assign( rA,
            binop(Iop_Or64,
                  binop(Iop_And64, mkexpr(rot), mkU64(mask64)),
                  binop(Iop_And64, getIReg(rA_addr), mkU64(~mask64))) );
      }
      else {
         // rA = (ROTL(rS, Imm) & mask) | (rA & ~mask);
         mask32 = MASK32(31-MaskEnd, 31-MaskBeg);
         r = ROTL(mkexpr(rS), mkU8(sh_imm));
         assign( rA,
            binop(Iop_Or32,
                  binop(Iop_And32, mkU32(mask32), r),
                  binop(Iop_And32, getIReg(rA_addr), mkU32(~mask32))) );
      }
      break;
   }

   case 0x15: {
      // rlwinm (Rotate Left Word Imm then AND with Mask, PPC32 p501)
      vassert(MaskBeg < 32);
      vassert(MaskEnd < 32);
      vassert(sh_imm  < 32);

      if (mode64) {
         IRTemp rTmp = newTemp(Ity_I64);
         mask64 = MASK64(31-MaskEnd, 31-MaskBeg);
         DIP("rlwinm%s r%u,r%u,%d,%d,%d\n", flag_rC ? ".":"",
             rA_addr, rS_addr, sh_imm, MaskBeg, MaskEnd);
         // tmp32 = (ROTL(rS_Lo32, Imm)
         // rA = ((tmp32 || tmp32) & mask64)
         r = ROTL( unop(Iop_64to32, mkexpr(rS) ), mkU8(sh_imm) );
         r = unop(Iop_32Uto64, r);
         assign( rTmp, r );
         r = NULL;
         assign( rot, binop(Iop_Or64, mkexpr(rTmp),
                            binop(Iop_Shl64, mkexpr(rTmp), mkU8(32))) );
         assign( rA, binop(Iop_And64, mkexpr(rot), mkU64(mask64)) );
      }
      else {
         if (MaskBeg == 0 && sh_imm+MaskEnd == 31) {
            /* Special-case the ,n,0,31-n form as that is just n-bit
               shift left, PPC32 p501 */
            DIP("slwi%s r%u,r%u,%d\n", flag_rC ? ".":"",
                rA_addr, rS_addr, sh_imm);
            assign( rA, binop(Iop_Shl32, mkexpr(rS), mkU8(sh_imm)) );
         }
         else if (MaskEnd == 31 && sh_imm+MaskBeg == 32) {
            /* Special-case the ,32-n,n,31 form as that is just n-bit
               unsigned shift right, PPC32 p501 */
            DIP("srwi%s r%u,r%u,%d\n", flag_rC ? ".":"",
                rA_addr, rS_addr, MaskBeg);
            assign( rA, binop(Iop_Shr32, mkexpr(rS), mkU8(MaskBeg)) );
         }
         else {
            /* General case. */
            mask32 = MASK32(31-MaskEnd, 31-MaskBeg);
            DIP("rlwinm%s r%u,r%u,%d,%d,%d\n", flag_rC ? ".":"",
                rA_addr, rS_addr, sh_imm, MaskBeg, MaskEnd);
            // rA = ROTL(rS, Imm) & mask
            assign( rA, binop(Iop_And32,
                              ROTL(mkexpr(rS), mkU8(sh_imm)), 
                              mkU32(mask32)) );
         }
      }
      break;
   }

   case 0x17: {
      // rlwnm (Rotate Left Word then AND with Mask, PPC32 p503
      DIP("rlwnm%s r%u,r%u,r%u,%d,%d\n", flag_rC ? ".":"",
          rA_addr, rS_addr, rB_addr, MaskBeg, MaskEnd);
      if (mode64) {
         mask64 = MASK64(31-MaskEnd, 31-MaskBeg);
         /* weird insn alert!
            tmp32 = (ROTL(rS_Lo32, rB[0-4])
            rA = ((tmp32 || tmp32) & mask64)
         */
         // note, ROTL does the masking, so we don't do it here
         r = ROTL( unop(Iop_64to32, mkexpr(rS)),
                   unop(Iop_64to8, mkexpr(rB)) );
         r = unop(Iop_32Uto64, r);
         assign(rot, binop(Iop_Or64, r, binop(Iop_Shl64, r, mkU8(32))));
         assign( rA, binop(Iop_And64, mkexpr(rot), mkU64(mask64)) );
      } else {
         mask32 = MASK32(31-MaskEnd, 31-MaskBeg);
         // rA = ROTL(rS, rB[0-4]) & mask
         // note, ROTL does the masking, so we don't do it here
         assign( rA, binop(Iop_And32,
                           ROTL(mkexpr(rS),
                                unop(Iop_32to8, mkexpr(rB))),
                           mkU32(mask32)) );
      }
      break;
   }

   /* 64bit Integer Rotates */
   case 0x1E: {
      msk_imm = ((msk_imm & 1) << 5) | (msk_imm >> 1);
      sh_imm |= b1 << 5;

      vassert( msk_imm < 64 );
      vassert( sh_imm < 64 );

      switch (opc2) {
      case 0x4: {
         /* r = ROTL64( rS, rB_lo6) */
         r = ROTL( mkexpr(rS), unop(Iop_64to8, mkexpr(rB)) );

         if (b1 == 0) { // rldcl (Rotl DWord, Clear Left, PPC64 p555)
            DIP("rldcl%s r%u,r%u,r%u,%u\n", flag_rC ? ".":"",
                rA_addr, rS_addr, rB_addr, msk_imm);
            // note, ROTL does the masking, so we don't do it here
            mask64 = MASK64(0, 63-msk_imm);
            assign( rA, binop(Iop_And64, r, mkU64(mask64)) );
            break;
         } else {       // rldcr (Rotl DWord, Clear Right, PPC64 p556)
            DIP("rldcr%s r%u,r%u,r%u,%u\n", flag_rC ? ".":"",
                rA_addr, rS_addr, rB_addr, msk_imm);
            mask64 = MASK64(63-msk_imm, 63);
            assign( rA, binop(Iop_And64, r, mkU64(mask64)) );
            break;
         }
         break;
      }
      case 0x2: // rldic (Rotl DWord Imm, Clear, PPC64 p557)
         DIP("rldic%s r%u,r%u,%u,%u\n", flag_rC ? ".":"",
             rA_addr, rS_addr, sh_imm, msk_imm);
         r = ROTL(mkexpr(rS), mkU8(sh_imm));
         mask64 = MASK64(sh_imm, 63-msk_imm);
         assign( rA, binop(Iop_And64, r, mkU64(mask64)) );
         break;
         // later: deal with special case: (msk_imm==0) => SHL(sh_imm)
         /*
           Hmm... looks like this'll do the job more simply:
           r = SHL(rS, sh_imm)
           m = ~(1 << (63-msk_imm))
           assign(rA, r & m);
         */
         
      case 0x0: // rldicl (Rotl DWord Imm, Clear Left, PPC64 p558)
         if (mode64
             && sh_imm + msk_imm == 64 && msk_imm >= 1 && msk_imm <= 63) {
            /* special-case the ,64-n,n form as that is just
               unsigned shift-right by n */
            DIP("srdi%s r%u,r%u,%u\n",
                flag_rC ? ".":"", rA_addr, rS_addr, msk_imm);
            assign( rA, binop(Iop_Shr64, mkexpr(rS), mkU8(msk_imm)) );
         } else {
            DIP("rldicl%s r%u,r%u,%u,%u\n", flag_rC ? ".":"",
                rA_addr, rS_addr, sh_imm, msk_imm);
            r = ROTL(mkexpr(rS), mkU8(sh_imm));
            mask64 = MASK64(0, 63-msk_imm);
            assign( rA, binop(Iop_And64, r, mkU64(mask64)) );
         }
         break;
         
      case 0x1: // rldicr (Rotl DWord Imm, Clear Right, PPC64 p559)
         if (mode64 
             && sh_imm + msk_imm == 63 && sh_imm >= 1 && sh_imm <= 63) {
            /* special-case the ,n,63-n form as that is just
               shift-left by n */
            DIP("sldi%s r%u,r%u,%u\n",
                flag_rC ? ".":"", rA_addr, rS_addr, sh_imm);
            assign( rA, binop(Iop_Shl64, mkexpr(rS), mkU8(sh_imm)) );
         } else {
            DIP("rldicr%s r%u,r%u,%u,%u\n", flag_rC ? ".":"",
                rA_addr, rS_addr, sh_imm, msk_imm);
            r = ROTL(mkexpr(rS), mkU8(sh_imm));
            mask64 = MASK64(63-msk_imm, 63);
            assign( rA, binop(Iop_And64, r, mkU64(mask64)) );
         }
         break;
         
      case 0x3: { // rldimi (Rotl DWord Imm, Mask Insert, PPC64 p560)
         IRTemp rA_orig = newTemp(ty);
         DIP("rldimi%s r%u,r%u,%u,%u\n", flag_rC ? ".":"",
             rA_addr, rS_addr, sh_imm, msk_imm);
         r = ROTL(mkexpr(rS), mkU8(sh_imm));
         mask64 = MASK64(sh_imm, 63-msk_imm);
         assign( rA_orig, getIReg(rA_addr) );
         assign( rA, binop(Iop_Or64,
                           binop(Iop_And64, mkU64(mask64),  r),
                           binop(Iop_And64, mkU64(~mask64),
                                            mkexpr(rA_orig))) );
         break;
      }
      default:
         vex_printf("dis_int_rot(ppc)(opc2)\n");
         return False;
      }
      break;         
   }

   default:
      vex_printf("dis_int_rot(ppc)(opc1)\n");
      return False;
   }

   putIReg( rA_addr, mkexpr(rA) );

   if (flag_rC) {
      set_CR0( mkexpr(rA) );
   }
   return True;
}


/*
  Integer Load Instructions
*/
static Bool dis_int_load ( UInt theInstr )
{
   /* D-Form, X-Form, DS-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar rD_addr  = ifieldRegDS(theInstr);
   UChar rA_addr  = ifieldRegA(theInstr);
   UInt  uimm16   = ifieldUIMM16(theInstr);
   UChar rB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b1       = ifieldBIT1(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   Int     simm16 = extend_s_16to32(uimm16);
   IRType  ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp  EA     = newTemp(ty);
   IRExpr* val;

   switch (opc1) {
   case 0x1F: // register offset
      assign( EA, ea_rAor0_idxd( rA_addr, rB_addr ) );
      break;
   case 0x3A: // immediate offset: 64bit: ld/ldu/lwa: mask off
              // lowest 2 bits of immediate before forming EA
      simm16 = simm16 & 0xFFFFFFFC;
   default:   // immediate offset
      assign( EA, ea_rAor0_simm( rA_addr, simm16  ) );
      break;
   }

   switch (opc1) {
   case 0x22: // lbz (Load B & Zero, PPC32 p433)
      DIP("lbz r%u,%d(r%u)\n", rD_addr, (Int)simm16, rA_addr);
      val = loadBE(Ity_I8, mkexpr(EA));
      putIReg( rD_addr, mkWidenFrom8(ty, val, False) );
      break;
      
   case 0x23: // lbzu (Load B & Zero, Update, PPC32 p434)
      if (rA_addr == 0 || rA_addr == rD_addr) {
         vex_printf("dis_int_load(ppc)(lbzu,rA_addr|rD_addr)\n");
         return False;
      }
      DIP("lbzu r%u,%d(r%u)\n", rD_addr, (Int)simm16, rA_addr);
      val = loadBE(Ity_I8, mkexpr(EA));
      putIReg( rD_addr, mkWidenFrom8(ty, val, False) );
      putIReg( rA_addr, mkexpr(EA) );
      break;
      
   case 0x2A: // lha (Load HW Alg, PPC32 p445)
      DIP("lha r%u,%d(r%u)\n", rD_addr, (Int)simm16, rA_addr);
      val = loadBE(Ity_I16, mkexpr(EA));
      putIReg( rD_addr, mkWidenFrom16(ty, val, True) );
      break;

   case 0x2B: // lhau (Load HW Alg, Update, PPC32 p446)
      if (rA_addr == 0 || rA_addr == rD_addr) {
         vex_printf("dis_int_load(ppc)(lhau,rA_addr|rD_addr)\n");
         return False;
      }
      DIP("lhau r%u,%d(r%u)\n", rD_addr, (Int)simm16, rA_addr);
      val = loadBE(Ity_I16, mkexpr(EA));
      putIReg( rD_addr, mkWidenFrom16(ty, val, True) );
      putIReg( rA_addr, mkexpr(EA) );
      break;
      
   case 0x28: // lhz (Load HW & Zero, PPC32 p450)
      DIP("lhz r%u,%d(r%u)\n", rD_addr, (Int)simm16, rA_addr);
      val = loadBE(Ity_I16, mkexpr(EA));
      putIReg( rD_addr, mkWidenFrom16(ty, val, False) );
      break;
      
   case 0x29: // lhzu (Load HW & and Zero, Update, PPC32 p451)
      if (rA_addr == 0 || rA_addr == rD_addr) {
         vex_printf("dis_int_load(ppc)(lhzu,rA_addr|rD_addr)\n");
         return False;
      }
      DIP("lhzu r%u,%d(r%u)\n", rD_addr, (Int)simm16, rA_addr);
      val = loadBE(Ity_I16, mkexpr(EA));
      putIReg( rD_addr, mkWidenFrom16(ty, val, False) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x20: // lwz (Load W & Zero, PPC32 p460)
      DIP("lwz r%u,%d(r%u)\n", rD_addr, (Int)simm16, rA_addr);
      val = loadBE(Ity_I32, mkexpr(EA));
      putIReg( rD_addr, mkWidenFrom32(ty, val, False) );
      break;
      
   case 0x21: // lwzu (Load W & Zero, Update, PPC32 p461))
      if (rA_addr == 0 || rA_addr == rD_addr) {
         vex_printf("dis_int_load(ppc)(lwzu,rA_addr|rD_addr)\n");
         return False;
      }
      DIP("lwzu r%u,%d(r%u)\n", rD_addr, (Int)simm16, rA_addr);
      val = loadBE(Ity_I32, mkexpr(EA));
      putIReg( rD_addr, mkWidenFrom32(ty, val, False) );
      putIReg( rA_addr, mkexpr(EA) );
      break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_load(ppc)(Ox1F,b0)\n");
         return False;
      }

      switch (opc2) {
      case 0x077: // lbzux (Load B & Zero, Update Indexed, PPC32 p435)
         DIP("lbzux r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(ppc)(lwzux,rA_addr|rD_addr)\n");
            return False;
         }
         val = loadBE(Ity_I8, mkexpr(EA));
         putIReg( rD_addr, mkWidenFrom8(ty, val, False) );
         putIReg( rA_addr, mkexpr(EA) );
         break;
         
      case 0x057: // lbzx (Load B & Zero, Indexed, PPC32 p436)
         DIP("lbzx r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         val = loadBE(Ity_I8, mkexpr(EA));
         putIReg( rD_addr, mkWidenFrom8(ty, val, False) );
         break;
         
      case 0x177: // lhaux (Load HW Alg, Update Indexed, PPC32 p447)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(ppc)(lhaux,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("lhaux r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         val = loadBE(Ity_I16, mkexpr(EA));
         putIReg( rD_addr, mkWidenFrom16(ty, val, True) );
         putIReg( rA_addr, mkexpr(EA) );
         break;
         
      case 0x157: // lhax (Load HW Alg, Indexed, PPC32 p448)
         DIP("lhax r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         val = loadBE(Ity_I16, mkexpr(EA));
         putIReg( rD_addr, mkWidenFrom16(ty, val, True) );
         break;
         
      case 0x137: // lhzux (Load HW & Zero, Update Indexed, PPC32 p452)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(ppc)(lhzux,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("lhzux r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         val = loadBE(Ity_I16, mkexpr(EA));
         putIReg( rD_addr, mkWidenFrom16(ty, val, False) );
         putIReg( rA_addr, mkexpr(EA) );
         break;
         
      case 0x117: // lhzx (Load HW & Zero, Indexed, PPC32 p453)
         DIP("lhzx r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         val = loadBE(Ity_I16, mkexpr(EA));
         putIReg( rD_addr, mkWidenFrom16(ty, val, False) );
         break;

      case 0x037: // lwzux (Load W & Zero, Update Indexed, PPC32 p462)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(ppc)(lwzux,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("lwzux r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         val = loadBE(Ity_I32, mkexpr(EA));
         putIReg( rD_addr, mkWidenFrom32(ty, val, False) );
         putIReg( rA_addr, mkexpr(EA) );
         break;
         
      case 0x017: // lwzx (Load W & Zero, Indexed, PPC32 p463)
         DIP("lwzx r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         val = loadBE(Ity_I32, mkexpr(EA));
         putIReg( rD_addr, mkWidenFrom32(ty, val, False) );
         break;


      /* 64bit Loads */
      case 0x035: // ldux (Load DWord, Update Indexed, PPC64 p475)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(ppc)(ldux,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("ldux r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, loadBE(Ity_I64, mkexpr(EA)) );
         putIReg( rA_addr, mkexpr(EA) );
         break;

      case 0x015: // ldx (Load DWord, Indexed, PPC64 p476)
         DIP("ldx r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, loadBE(Ity_I64, mkexpr(EA)) );
         break;

      case 0x175: // lwaux (Load W Alg, Update Indexed, PPC64 p501)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(ppc)(lwaux,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("lwaux r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr,
                  unop(Iop_32Sto64, loadBE(Ity_I32, mkexpr(EA))) );
         putIReg( rA_addr, mkexpr(EA) );
         break;

      case 0x155: // lwax (Load W Alg, Indexed, PPC64 p502)
         DIP("lwax r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr,
                  unop(Iop_32Sto64, loadBE(Ity_I32, mkexpr(EA))) );
         break;

      default:
         vex_printf("dis_int_load(ppc)(opc2)\n");
         return False;
      }
      break;

   /* DS Form - 64bit Loads.  In each case EA will have been formed
      with the lowest 2 bits masked off the immediate offset. */
   case 0x3A:
      switch ((b1<<1) | b0) {
      case 0x0: // ld (Load DWord, PPC64 p472)
         DIP("ld r%u,%d(r%u)\n", rD_addr, simm16, rA_addr);
         putIReg( rD_addr, loadBE(Ity_I64, mkexpr(EA)) );
         break;

      case 0x1: // ldu (Load DWord, Update, PPC64 p474)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(ppc)(ldu,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("ldu r%u,%d(r%u)\n", rD_addr, simm16, rA_addr);
         putIReg( rD_addr, loadBE(Ity_I64, mkexpr(EA)) );
         putIReg( rA_addr, mkexpr(EA) );
         break;

      case 0x2: // lwa (Load Word Alg, PPC64 p499)
         DIP("lwa r%u,%d(r%u)\n", rD_addr, simm16, rA_addr);
         putIReg( rD_addr,
                  unop(Iop_32Sto64, loadBE(Ity_I32, mkexpr(EA))) );
         break;

      default:
         vex_printf("dis_int_load(ppc)(0x3A, opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_int_load(ppc)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Store Instructions
*/
static Bool dis_int_store ( UInt theInstr, VexAbiInfo* vbi )
{
   /* D-Form, X-Form, DS-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UInt  rS_addr = ifieldRegDS(theInstr);
   UInt  rA_addr = ifieldRegA(theInstr);
   UInt  uimm16  = ifieldUIMM16(theInstr);
   UInt  rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b1      = ifieldBIT1(theInstr);
   UChar b0      = ifieldBIT0(theInstr);

   Int    simm16 = extend_s_16to32(uimm16);
   IRType ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp rS     = newTemp(ty);
   IRTemp rB     = newTemp(ty);
   IRTemp EA     = newTemp(ty);
   
   assign( rB, getIReg(rB_addr) );
   assign( rS, getIReg(rS_addr) );
   
   switch (opc1) {
   case 0x1F: // register offset
      assign( EA, ea_rAor0_idxd( rA_addr, rB_addr ) );
      break;
   case 0x3E: // immediate offset: 64bit: std/stdu: mask off
              // lowest 2 bits of immediate before forming EA
      simm16 = simm16 & 0xFFFFFFFC;
   default:   // immediate offset
      assign( EA, ea_rAor0_simm( rA_addr, simm16  ) );
      break;
   }

   switch (opc1) {
   case 0x26: // stb (Store B, PPC32 p509)
      DIP("stb r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      storeBE( mkexpr(EA), mkNarrowTo8(ty, mkexpr(rS)) );
      break;
       
   case 0x27: // stbu (Store B, Update, PPC32 p510)
      if (rA_addr == 0 ) {
         vex_printf("dis_int_store(ppc)(stbu,rA_addr)\n");
         return False;
      }
      DIP("stbu r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      putIReg( rA_addr, mkexpr(EA) );
      storeBE( mkexpr(EA), mkNarrowTo8(ty, mkexpr(rS)) );
      break;

   case 0x2C: // sth (Store HW, PPC32 p522)
      DIP("sth r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      storeBE( mkexpr(EA), mkNarrowTo16(ty, mkexpr(rS)) );
      break;
      
   case 0x2D: // sthu (Store HW, Update, PPC32 p524)
      if (rA_addr == 0) {
         vex_printf("dis_int_store(ppc)(sthu,rA_addr)\n");
         return False;
      }
      DIP("sthu r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      putIReg( rA_addr, mkexpr(EA) );
      storeBE( mkexpr(EA), mkNarrowTo16(ty, mkexpr(rS)) );
      break;

   case 0x24: // stw (Store W, PPC32 p530)
      DIP("stw r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      storeBE( mkexpr(EA), mkNarrowTo32(ty, mkexpr(rS)) );
      break;

   case 0x25: // stwu (Store W, Update, PPC32 p534)
      if (rA_addr == 0) {
         vex_printf("dis_int_store(ppc)(stwu,rA_addr)\n");
         return False;
      }
      DIP("stwu r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      putIReg( rA_addr, mkexpr(EA) );
      storeBE( mkexpr(EA), mkNarrowTo32(ty, mkexpr(rS)) );
      break;
      
   /* X Form : all these use EA_indexed */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_store(ppc)(0x1F,b0)\n");
         return False;
      }

      switch (opc2) {
      case 0x0F7: // stbux (Store B, Update Indexed, PPC32 p511)
         if (rA_addr == 0) {
            vex_printf("dis_int_store(ppc)(stbux,rA_addr)\n");
            return False;
         }
         DIP("stbux r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         putIReg( rA_addr, mkexpr(EA) );
         storeBE( mkexpr(EA), mkNarrowTo8(ty, mkexpr(rS)) );
         break;
         
      case 0x0D7: // stbx (Store B Indexed, PPC32 p512)
         DIP("stbx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         storeBE( mkexpr(EA), mkNarrowTo8(ty, mkexpr(rS)) );
         break;
         
      case 0x1B7: // sthux (Store HW, Update Indexed, PPC32 p525)
         if (rA_addr == 0) {
            vex_printf("dis_int_store(ppc)(sthux,rA_addr)\n");
            return False;
         }
         DIP("sthux r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         putIReg( rA_addr, mkexpr(EA) );
         storeBE( mkexpr(EA), mkNarrowTo16(ty, mkexpr(rS)) );
         break;
         
      case 0x197: // sthx (Store HW Indexed, PPC32 p526)
         DIP("sthx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         storeBE( mkexpr(EA), mkNarrowTo16(ty, mkexpr(rS)) );
         break;
         
      case 0x0B7: // stwux (Store W, Update Indexed, PPC32 p535)
         if (rA_addr == 0) {
            vex_printf("dis_int_store(ppc)(stwux,rA_addr)\n");
            return False;
         }
         DIP("stwux r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         putIReg( rA_addr, mkexpr(EA) );
         storeBE( mkexpr(EA), mkNarrowTo32(ty, mkexpr(rS)) );
         break;

      case 0x097: // stwx (Store W Indexed, PPC32 p536)
         DIP("stwx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         storeBE( mkexpr(EA), mkNarrowTo32(ty, mkexpr(rS)) );
         break;
         

      /* 64bit Stores */
      case 0x0B5: // stdux (Store DWord, Update Indexed, PPC64 p584)
         if (rA_addr == 0) {
            vex_printf("dis_int_store(ppc)(stdux,rA_addr)\n");
            return False;
         }
         DIP("stdux r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         putIReg( rA_addr, mkexpr(EA) );
         storeBE( mkexpr(EA), mkexpr(rS) );
         break;

      case 0x095: // stdx (Store DWord Indexed, PPC64 p585)
         DIP("stdx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         storeBE( mkexpr(EA), mkexpr(rS) );
         break;

      default:
         vex_printf("dis_int_store(ppc)(opc2)\n");
         return False;
      }
      break;

   /* DS Form - 64bit Stores.  In each case EA will have been formed
      with the lowest 2 bits masked off the immediate offset. */
   case 0x3E:
      switch ((b1<<1) | b0) {
      case 0x0: // std (Store DWord, PPC64 p580)
         DIP("std r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
         storeBE( mkexpr(EA), mkexpr(rS) );
         break;

      case 0x1: // stdu (Store DWord, Update, PPC64 p583)
         DIP("stdu r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
         putIReg( rA_addr, mkexpr(EA) );
         storeBE( mkexpr(EA), mkexpr(rS) );
         break;

      default:
         vex_printf("dis_int_load(ppc)(0x3A, opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_int_store(ppc)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Load/Store Multiple Instructions
*/
static Bool dis_int_ldst_mult ( UInt theInstr )
{
   /* D-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar rD_addr  = ifieldRegDS(theInstr);
   UChar rS_addr  = rD_addr;
   UChar rA_addr  = ifieldRegA(theInstr);
   UInt  uimm16   = ifieldUIMM16(theInstr);

   Int     simm16 = extend_s_16to32(uimm16);
   IRType  ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp  EA     = newTemp(ty);
   UInt    r      = 0;
   UInt    ea_off = 0;
   IRExpr* irx_addr;

   assign( EA, ea_rAor0_simm( rA_addr, simm16 ) );

   switch (opc1) {
   case 0x2E: // lmw (Load Multiple Word, PPC32 p454)
      if (rA_addr >= rD_addr) {
         vex_printf("dis_int_ldst_mult(ppc)(lmw,rA_addr)\n");
         return False;
      }
      DIP("lmw r%u,%d(r%u)\n", rD_addr, simm16, rA_addr);
      for (r = rD_addr; r <= 31; r++) {
         irx_addr = binop(Iop_Add32, mkexpr(EA), mkU32(ea_off));
         putIReg( r, mkWidenFrom32(ty, loadBE(Ity_I32, irx_addr ),
                                       False) );
         ea_off += 4;
      }
      break;
      
   case 0x2F: // stmw (Store Multiple Word, PPC32 p527)
      DIP("stmw r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      for (r = rS_addr; r <= 31; r++) {
         irx_addr = binop(Iop_Add32, mkexpr(EA), mkU32(ea_off));
         storeBE( irx_addr, mkNarrowTo32(ty, getIReg(r)) );
         ea_off += 4;
      }
      break;
      
   default:
      vex_printf("dis_int_ldst_mult(ppc)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Load/Store String Instructions
*/
static 
void generate_lsw_sequence ( IRTemp tNBytes,   // # bytes, :: Ity_I32
                             IRTemp EA,        // EA
                             Int    rD,        // first dst register
                             Int    maxBytes ) // 32 or 128
{
   Int     i, shift = 24;
   IRExpr* e_nbytes = mkexpr(tNBytes);
   IRExpr* e_EA     = mkexpr(EA);
   IRType  ty       = mode64 ? Ity_I64 : Ity_I32;

   vassert(rD >= 0 && rD < 32);
   rD--; if (rD < 0) rD = 31;

   for (i = 0; i < maxBytes; i++) {
      /* if (nBytes < (i+1)) goto NIA; */
      stmt( IRStmt_Exit( binop(Iop_CmpLT32U, e_nbytes, mkU32(i+1)),
                         Ijk_Boring, 
                         mkSzConst( ty, nextInsnAddr()) ));
      /* when crossing into a new dest register, set it to zero. */
      if ((i % 4) == 0) {
         rD++; if (rD == 32) rD = 0;
         putIReg(rD, mkSzImm(ty, 0));
         shift = 24;
      }
      /* rD |=  (8Uto32(*(EA+i))) << shift */
      vassert(shift == 0 || shift == 8 || shift == 16 || shift == 24);
      putIReg( 
         rD, 
         mkWidenFrom32(
            ty, 
            binop(
               Iop_Or32, 
               mkNarrowTo32(ty, getIReg(rD)),
               binop(
                  Iop_Shl32, 
                  unop(
                     Iop_8Uto32, 
                     loadBE(Ity_I8, 
                            binop(mkSzOp(ty,Iop_Add8), e_EA, mkSzImm(ty,i)))
                  ), 
                  mkU8(toUChar(shift))
               )
            ),
            /*Signed*/False
	 ) 
      ); 
      shift -= 8;
   }
}

static 
void generate_stsw_sequence ( IRTemp tNBytes,   // # bytes, :: Ity_I32
                              IRTemp EA,        // EA
                              Int    rS,        // first src register
                              Int    maxBytes ) // 32 or 128
{
   Int     i, shift = 24;
   IRExpr* e_nbytes = mkexpr(tNBytes);
   IRExpr* e_EA     = mkexpr(EA);
   IRType  ty       = mode64 ? Ity_I64 : Ity_I32;

   vassert(rS >= 0 && rS < 32);
   rS--; if (rS < 0) rS = 31;

   for (i = 0; i < maxBytes; i++) {
      /* if (nBytes < (i+1)) goto NIA; */
      stmt( IRStmt_Exit( binop(Iop_CmpLT32U, e_nbytes, mkU32(i+1)),
                         Ijk_Boring, 
                         mkSzConst( ty, nextInsnAddr() ) ));
      /* check for crossing into a new src register. */
      if ((i % 4) == 0) {
         rS++; if (rS == 32) rS = 0;
         shift = 24;
      }
      /* *(EA+i) = 32to8(rS >> shift) */
      vassert(shift == 0 || shift == 8 || shift == 16 || shift == 24);
      storeBE(
         binop(mkSzOp(ty,Iop_Add8), e_EA, mkSzImm(ty,i)),
         unop(Iop_32to8,
              binop(Iop_Shr32,
                    mkNarrowTo32(ty, getIReg(rS)),
                    mkU8(toUChar(shift))))
      );
      shift -= 8;
   }
}

static Bool dis_int_ldst_str ( UInt theInstr, /*OUT*/Bool* stopHere )
{
   /* X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar rD_addr  = ifieldRegDS(theInstr);
   UChar rS_addr  = rD_addr;
   UChar rA_addr  = ifieldRegA(theInstr);
   UChar rB_addr  = ifieldRegB(theInstr);
   UChar NumBytes = rB_addr;
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   IRType ty      = mode64 ? Ity_I64 : Ity_I32;
   IRTemp t_EA    = newTemp(ty);
   IRTemp t_nbytes = IRTemp_INVALID;

   *stopHere = False;

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_int_ldst_str(ppc)(opc1)\n");
      return False;
   }

   switch (opc2) {
   case 0x255: // lswi (Load String Word Immediate, PPC32 p455)
      /* NB: does not reject the case where RA is in the range of
         registers to be loaded.  It should. */
      DIP("lswi r%u,r%u,%d\n", rD_addr, rA_addr, NumBytes);
      assign( t_EA, ea_rAor0(rA_addr) );
      if (NumBytes == 8 && !mode64) {
         /* Special case hack */
         /* rD = Mem[EA]; (rD+1)%32 = Mem[EA+4] */
         putIReg( rD_addr,          
                  loadBE(Ity_I32, mkexpr(t_EA)) );
         putIReg( (rD_addr+1) % 32, 
                  loadBE(Ity_I32,
                         binop(Iop_Add32, mkexpr(t_EA), mkU32(4))) );
      } else {
         t_nbytes = newTemp(Ity_I32);
         assign( t_nbytes, mkU32(NumBytes==0 ? 32 : NumBytes) );
         generate_lsw_sequence( t_nbytes, t_EA, rD_addr, 32 );
         *stopHere = True;
      }
      return True;

   case 0x215: // lswx (Load String Word Indexed, PPC32 p456)
      /* NB: does not reject the case where RA is in the range of
         registers to be loaded.  It should.  Although considering
         that that can only be detected at run time, it's not easy to
         do so. */
      if (rD_addr == rA_addr || rD_addr == rB_addr)
         return False;
      if (rD_addr == 0 && rA_addr == 0)
         return False;
      DIP("lswx r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
      t_nbytes = newTemp(Ity_I32);
      assign( t_EA, ea_rAor0_idxd(rA_addr,rB_addr) );
      assign( t_nbytes, unop( Iop_8Uto32, getXER_BC() ) );
      generate_lsw_sequence( t_nbytes, t_EA, rD_addr, 128 );
      *stopHere = True;
      return True;

   case 0x2D5: // stswi (Store String Word Immediate, PPC32 p528)
      DIP("stswi r%u,r%u,%d\n", rS_addr, rA_addr, NumBytes);
      assign( t_EA, ea_rAor0(rA_addr) );
      if (NumBytes == 8 && !mode64) {
         /* Special case hack */
         /* Mem[EA] = rD; Mem[EA+4] = (rD+1)%32 */
         storeBE( mkexpr(t_EA), 
                  getIReg(rD_addr) );
         storeBE( binop(Iop_Add32, mkexpr(t_EA), mkU32(4)), 
                  getIReg((rD_addr+1) % 32) );
      } else {
         t_nbytes = newTemp(Ity_I32);
         assign( t_nbytes, mkU32(NumBytes==0 ? 32 : NumBytes) );
         generate_stsw_sequence( t_nbytes, t_EA, rD_addr, 32 );
         *stopHere = True;
      }
      return True;

   case 0x295: // stswx (Store String Word Indexed, PPC32 p529)
      DIP("stswx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
      t_nbytes = newTemp(Ity_I32);
      assign( t_EA, ea_rAor0_idxd(rA_addr,rB_addr) );
      assign( t_nbytes, unop( Iop_8Uto32, getXER_BC() ) );
      generate_stsw_sequence( t_nbytes, t_EA, rS_addr, 128 );
      *stopHere = True;
      return True;

   default:
      vex_printf("dis_int_ldst_str(ppc)(opc2)\n");
      return False;
   }
   return True;
}


/* ------------------------------------------------------------------
   Integer Branch Instructions
   ------------------------------------------------------------------ */

/*
  Branch helper function
  ok = BO[2] | ((CTR[0] != 0) ^ BO[1])
  Returns an I32 which is 0x00000000 if the ctr condition failed
  and 0xFFFFFFFF otherwise.
*/
static IRExpr* /* :: Ity_I32 */ branch_ctr_ok( UInt BO )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IRTemp ok = newTemp(Ity_I32);

   if ((BO >> 2) & 1) {     // independent of ctr
      assign( ok, mkU32(0xFFFFFFFF) );
   } else {
      if ((BO >> 1) & 1) {  // ctr == 0 ?
         assign( ok, unop( Iop_1Sto32,
                           binop( mkSzOp(ty, Iop_CmpEQ8),
                                  getGST( PPC_GST_CTR ),
                                  mkSzImm(ty,0))) );
      } else {              // ctr != 0 ?
         assign( ok, unop( Iop_1Sto32,
                           binop( mkSzOp(ty, Iop_CmpNE8),
                                  getGST( PPC_GST_CTR ),
                                  mkSzImm(ty,0))) );
      }
   }
   return mkexpr(ok);
}


/*
  Branch helper function cond_ok = BO[4] | (CR[BI] == BO[3])
  Returns an I32 which is either 0 if the condition failed or 
  some arbitrary nonzero value otherwise. */

static IRExpr* /* :: Ity_I32 */ branch_cond_ok( UInt BO, UInt BI )
{
   Int where;
   IRTemp res   = newTemp(Ity_I32);
   IRTemp cr_bi = newTemp(Ity_I32);
   
   if ((BO >> 4) & 1) {
      assign( res, mkU32(1) );
   } else {
      // ok = (CR[BI] == BO[3]) Note, the following relies on
      // getCRbit_anywhere returning a value which
      // is either zero or has exactly 1 bit set.  
      assign( cr_bi, getCRbit_anywhere( BI, &where ) );

      if ((BO >> 3) & 1) {
         /* We can use cr_bi as-is. */
         assign( res, mkexpr(cr_bi) );
      } else {
         /* We have to invert the sense of the information held in
            cr_bi.  For that we need to know which bit
            getCRbit_anywhere regards as significant. */
         assign( res, binop(Iop_Xor32, mkexpr(cr_bi),
                                       mkU32(1<<where)) );
      }
   }
   return mkexpr(res);
}


/*
  Integer Branch Instructions
*/
static Bool dis_branch ( UInt theInstr, 
                         VexAbiInfo* vbi,
                         /*OUT*/DisResult* dres,
                         Bool (*resteerOkFn)(void*,Addr64),
                         void* callback_opaque )
{
   UChar opc1    = ifieldOPC(theInstr);
   UChar BO      = ifieldRegDS(theInstr);
   UChar BI      = ifieldRegA(theInstr);
   UInt  BD_u16  = ifieldUIMM16(theInstr) & 0xFFFFFFFC; /* mask off */
   UChar b11to15 = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UInt  LI_u26  = ifieldUIMM26(theInstr) & 0xFFFFFFFC; /* mask off */
   UChar flag_AA = ifieldBIT1(theInstr);
   UChar flag_LK = ifieldBIT0(theInstr);

   IRType   ty        = mode64 ? Ity_I64 : Ity_I32;
   Addr64   tgt       = 0;
   Int      BD        = extend_s_16to32(BD_u16);
   IRTemp   do_branch = newTemp(Ity_I32);
   IRTemp   ctr_ok    = newTemp(Ity_I32);
   IRTemp   cond_ok   = newTemp(Ity_I32);
   IRExpr*  e_nia     = mkSzImm(ty, nextInsnAddr());
   IRConst* c_nia     = mkSzConst(ty, nextInsnAddr());
   IRTemp   lr_old    = newTemp(ty);

   /* Hack to pass through code that just wants to read the PC */
   if (theInstr == 0x429F0005) {
      DIP("bcl 0x%x, 0x%x (a.k.a mr lr,cia+4)\n", BO, BI);
      putGST( PPC_GST_LR, e_nia );
      return True;
   }

   /* The default what-next.  Individual cases can override it. */    
   dres->whatNext = Dis_StopHere;

   switch (opc1) {
   case 0x12: // b     (Branch, PPC32 p360)
      if (flag_AA) {
         tgt = mkSzAddr( ty, extend_s_26to64(LI_u26) );
      } else {
         tgt = mkSzAddr( ty, guest_CIA_curr_instr +
                             (Long)extend_s_26to64(LI_u26) );
      }
      if (mode64) {
         DIP("b%s%s 0x%llx\n",
             flag_LK ? "l" : "", flag_AA ? "a" : "", tgt);
      } else {
         DIP("b%s%s 0x%x\n",
             flag_LK ? "l" : "", flag_AA ? "a" : "", (Addr32)tgt);
      }

      if (flag_LK) {
         putGST( PPC_GST_LR, e_nia );
         if (vbi->guest_ppc_zap_RZ_at_bl
             && vbi->guest_ppc_zap_RZ_at_bl( (ULong)tgt) ) {
            IRTemp t_tgt = newTemp(ty);
            assign(t_tgt, mode64 ? mkU64(tgt) : mkU32(tgt) );
            make_redzone_AbiHint( vbi, t_tgt,
                                  "branch-and-link (unconditional call)" );
         }
      }

      if (resteerOkFn( callback_opaque, tgt )) {
         dres->whatNext   = Dis_ResteerU;
         dres->continueAt = tgt;
      } else {
         irsb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
         irsb->next     = mkSzImm(ty, tgt);
      }
      break;
      
   case 0x10: // bc    (Branch Conditional, PPC32 p361)
      DIP("bc%s%s 0x%x, 0x%x, 0x%x\n",
          flag_LK ? "l" : "", flag_AA ? "a" : "", BO, BI, BD);
      
      if (!(BO & 0x4)) {
         putGST( PPC_GST_CTR,
                 binop(mkSzOp(ty, Iop_Sub8),
                       getGST( PPC_GST_CTR ), mkSzImm(ty, 1)) );
      }

      /* This is a bit subtle.  ctr_ok is either all 0s or all 1s.
         cond_ok is either zero or nonzero, since that's the cheapest
         way to compute it.  Anding them together gives a value which
         is either zero or non zero and so that's what we must test
         for in the IRStmt_Exit. */
      assign( ctr_ok,  branch_ctr_ok( BO ) );
      assign( cond_ok, branch_cond_ok( BO, BI ) );
      assign( do_branch,
              binop(Iop_And32, mkexpr(cond_ok), mkexpr(ctr_ok)) );

      if (flag_AA) {
         tgt = mkSzAddr(ty, extend_s_16to64(BD_u16));
      } else {
         tgt = mkSzAddr(ty, guest_CIA_curr_instr +
                            (Long)extend_s_16to64(BD_u16));
      }
      if (flag_LK)
         putGST( PPC_GST_LR, e_nia );
      
      stmt( IRStmt_Exit(
               binop(Iop_CmpNE32, mkexpr(do_branch), mkU32(0)),
               flag_LK ? Ijk_Call : Ijk_Boring,
               mkSzConst(ty, tgt) ) );
      
      irsb->jumpkind = Ijk_Boring;
      irsb->next     = e_nia;
      break;
      
   case 0x13:
      /* For bclr and bcctr, it appears that the lowest two bits of
         b11to15 are a branch hint, and so we only need to ensure it's
         of the form 000XX. */
      if ((b11to15 & ~3) != 0) {
         vex_printf("dis_int_branch(ppc)(0x13,b11to15)(%d)\n", (Int)b11to15);
         return False;
      }

      switch (opc2) {
      case 0x210: // bcctr (Branch Cond. to Count Register, PPC32 p363) 
         if ((BO & 0x4) == 0) { // "decr and test CTR" option invalid
            vex_printf("dis_int_branch(ppc)(bcctr,BO)\n");
            return False;
         }
         DIP("bcctr%s 0x%x, 0x%x\n", flag_LK ? "l" : "", BO, BI);
         
         assign( cond_ok, branch_cond_ok( BO, BI ) );

         /* FIXME: this is confusing.  lr_old holds the old value
            of ctr, not lr :-) */
         assign( lr_old, addr_align( getGST( PPC_GST_CTR ), 4 ));

         if (flag_LK)
            putGST( PPC_GST_LR, e_nia );
         
         stmt( IRStmt_Exit(
                  binop(Iop_CmpEQ32, mkexpr(cond_ok), mkU32(0)),
                  Ijk_Boring,
                  c_nia ));

         if (flag_LK && vbi->guest_ppc_zap_RZ_at_bl) {
            make_redzone_AbiHint( vbi, lr_old,
                                  "b-ctr-l (indirect call)" );
	 }

         irsb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
         irsb->next     = mkexpr(lr_old);
         break;
         
      case 0x010: { // bclr (Branch Cond. to Link Register, PPC32 p365) 
         Bool vanilla_return = False;
         if ((BO & 0x14 /* 1z1zz */) == 0x14 && flag_LK == 0) {
            DIP("blr\n");
            vanilla_return = True;
         } else {
            DIP("bclr%s 0x%x, 0x%x\n", flag_LK ? "l" : "", BO, BI);
         }

         if (!(BO & 0x4)) {
            putGST( PPC_GST_CTR,
                    binop(mkSzOp(ty, Iop_Sub8),
                          getGST( PPC_GST_CTR ), mkSzImm(ty, 1)) );
         }
         
         /* See comments above for 'bc' about this */
         assign( ctr_ok,  branch_ctr_ok( BO ) );
         assign( cond_ok, branch_cond_ok( BO, BI ) );
         assign( do_branch,
                 binop(Iop_And32, mkexpr(cond_ok), mkexpr(ctr_ok)) );
         
         assign( lr_old, addr_align( getGST( PPC_GST_LR ), 4 ));

         if (flag_LK)
            putGST( PPC_GST_LR,  e_nia );

         stmt( IRStmt_Exit(
                  binop(Iop_CmpEQ32, mkexpr(do_branch), mkU32(0)),
                  Ijk_Boring,
                  c_nia ));

         if (vanilla_return && vbi->guest_ppc_zap_RZ_at_blr) {
            make_redzone_AbiHint( vbi, lr_old,
                                  "branch-to-lr (unconditional return)" );
         }

         /* blrl is pretty strange; it's like a return that sets the
            return address of its caller to the insn following this
            one.  Mark it as a return. */
         irsb->jumpkind = Ijk_Ret;  /* was flag_LK ? Ijk_Call : Ijk_Ret; */
         irsb->next     = mkexpr(lr_old);
         break;
      }
      default:
         vex_printf("dis_int_branch(ppc)(opc2)\n");
         return False;
      }
      break;
      
   default:
      vex_printf("dis_int_branch(ppc)(opc1)\n");
      return False;
   }
   
   return True;
}



/*
  Condition Register Logical Instructions
*/
static Bool dis_cond_logic ( UInt theInstr )
{
   /* XL-Form */
   UChar opc1      = ifieldOPC(theInstr);
   UChar crbD_addr = ifieldRegDS(theInstr);
   UChar crfD_addr = toUChar( IFIELD(theInstr, 23, 3) );
   UChar crbA_addr = ifieldRegA(theInstr);
   UChar crfS_addr = toUChar( IFIELD(theInstr, 18, 3) );
   UChar crbB_addr = ifieldRegB(theInstr);
   UInt  opc2      = ifieldOPClo10(theInstr);
   UChar b0        = ifieldBIT0(theInstr);

   IRTemp crbD     = newTemp(Ity_I32);
   IRTemp crbA     = newTemp(Ity_I32);
   IRTemp crbB     = newTemp(Ity_I32);

   if (opc1 != 19 || b0 != 0) {
      vex_printf("dis_cond_logic(ppc)(opc1)\n");
      return False;
   }

   if (opc2 == 0) {  // mcrf    (Move Cond Reg Field, PPC32 p464)
      if (((crbD_addr & 0x3) != 0) ||
          ((crbA_addr & 0x3) != 0) || (crbB_addr != 0)) {
         vex_printf("dis_cond_logic(ppc)(crbD|crbA|crbB != 0)\n");
         return False;
      }
      DIP("mcrf cr%u,cr%u\n", crfD_addr, crfS_addr);
      putCR0(   crfD_addr, getCR0(  crfS_addr) );
      putCR321( crfD_addr, getCR321(crfS_addr) );
   } else {
      assign( crbA, getCRbit(crbA_addr) );
      if (crbA_addr == crbB_addr)
         crbB = crbA;
      else
         assign( crbB, getCRbit(crbB_addr) );

      switch (opc2) {
      case 0x101: // crand   (Cond Reg AND, PPC32 p372)
         DIP("crand crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_And32, mkexpr(crbA), mkexpr(crbB)) );
         break;
      case 0x081: // crandc  (Cond Reg AND w. Complement, PPC32 p373)
         DIP("crandc crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_And32, 
                             mkexpr(crbA),
                             unop(Iop_Not32, mkexpr(crbB))) );
         break;
      case 0x121: // creqv   (Cond Reg Equivalent, PPC32 p374)
         DIP("creqv crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, unop(Iop_Not32,
                            binop(Iop_Xor32, mkexpr(crbA), mkexpr(crbB))) );
         break;
      case 0x0E1: // crnand  (Cond Reg NAND, PPC32 p375)
         DIP("crnand crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, unop(Iop_Not32,
                            binop(Iop_And32, mkexpr(crbA), mkexpr(crbB))) );
         break;
      case 0x021: // crnor   (Cond Reg NOR, PPC32 p376)
         DIP("crnor crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, unop(Iop_Not32,
                            binop(Iop_Or32, mkexpr(crbA), mkexpr(crbB))) );
         break;
      case 0x1C1: // cror    (Cond Reg OR, PPC32 p377)
         DIP("cror crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_Or32, mkexpr(crbA), mkexpr(crbB)) );
         break;
      case 0x1A1: // crorc   (Cond Reg OR w. Complement, PPC32 p378)
         DIP("crorc crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_Or32, 
                             mkexpr(crbA),
                             unop(Iop_Not32, mkexpr(crbB))) );
         break;
      case 0x0C1: // crxor   (Cond Reg XOR, PPC32 p379)
         DIP("crxor crb%d,crb%d,crb%d\n", crbD_addr, crbA_addr, crbB_addr);
         assign( crbD, binop(Iop_Xor32, mkexpr(crbA), mkexpr(crbB)) );
         break;
      default:
         vex_printf("dis_cond_logic(ppc)(opc2)\n");
         return False;
      }

      putCRbit( crbD_addr, mkexpr(crbD) );
   }
   return True;
}


/* 
  Trap instructions
*/

/* Do the code generation for a trap.  Returned Bool is true iff
   this is an unconditional trap.  If the two arg IRExpr*s are 
   Ity_I32s then the comparison is 32-bit.  If they are Ity_I64s
   then they are 64-bit, and we must be disassembling 64-bit
   instructions. */
static Bool do_trap ( UChar TO, 
                      IRExpr* argL0, IRExpr* argR0, Addr64 cia )
{
   IRTemp argL, argR;
   IRExpr *argLe, *argRe, *cond, *tmp;

   Bool    is32bit = typeOfIRExpr(irsb->tyenv, argL0 ) == Ity_I32;

   IROp    opAND     = is32bit ? Iop_And32     : Iop_And64;
   IROp    opOR      = is32bit ? Iop_Or32      : Iop_Or64;
   IROp    opCMPORDS = is32bit ? Iop_CmpORD32S : Iop_CmpORD64S;
   IROp    opCMPORDU = is32bit ? Iop_CmpORD32U : Iop_CmpORD64U;
   IROp    opCMPNE   = is32bit ? Iop_CmpNE32   : Iop_CmpNE64;
   IROp    opCMPEQ   = is32bit ? Iop_CmpEQ32   : Iop_CmpEQ64;
   IRExpr* const0    = is32bit ? mkU32(0)      : mkU64(0);
   IRExpr* const2    = is32bit ? mkU32(2)      : mkU64(2);
   IRExpr* const4    = is32bit ? mkU32(4)      : mkU64(4);
   IRExpr* const8    = is32bit ? mkU32(8)      : mkU64(8);

   const UChar b11100 = 0x1C;
   const UChar b00111 = 0x07;

   if (is32bit) {
      vassert( typeOfIRExpr(irsb->tyenv, argL0) == Ity_I32 );
      vassert( typeOfIRExpr(irsb->tyenv, argR0) == Ity_I32 );
   } else {
      vassert( typeOfIRExpr(irsb->tyenv, argL0) == Ity_I64 );
      vassert( typeOfIRExpr(irsb->tyenv, argR0) == Ity_I64 );
      vassert( mode64 );
   }

   if ((TO & b11100) == b11100 || (TO & b00111) == b00111) {
      /* Unconditional trap.  Just do the exit without 
         testing the arguments. */
      stmt( IRStmt_Exit( 
               binop(opCMPEQ, const0, const0), 
               Ijk_SigTRAP,
               mode64 ? IRConst_U64(cia) : IRConst_U32((UInt)cia) 
      ));
      return True; /* unconditional trap */
   }

   if (is32bit) {
      argL = newTemp(Ity_I32);
      argR = newTemp(Ity_I32);
   } else {
      argL = newTemp(Ity_I64);
      argR = newTemp(Ity_I64);
   }

   assign( argL, argL0 );
   assign( argR, argR0 );

   argLe = mkexpr(argL);
   argRe = mkexpr(argR);

   cond = const0;
   if (TO & 16) { // L <s R
      tmp = binop(opAND, binop(opCMPORDS, argLe, argRe), const8);
      cond = binop(opOR, tmp, cond);
   }
   if (TO & 8) { // L >s R
      tmp = binop(opAND, binop(opCMPORDS, argLe, argRe), const4);
      cond = binop(opOR, tmp, cond);
   }
   if (TO & 4) { // L == R
      tmp = binop(opAND, binop(opCMPORDS, argLe, argRe), const2);
      cond = binop(opOR, tmp, cond);
   }
   if (TO & 2) { // L <u R
      tmp = binop(opAND, binop(opCMPORDU, argLe, argRe), const8);
      cond = binop(opOR, tmp, cond);
   }
   if (TO & 1) { // L >u R
      tmp = binop(opAND, binop(opCMPORDU, argLe, argRe), const4);
      cond = binop(opOR, tmp, cond);
   }
   stmt( IRStmt_Exit( 
            binop(opCMPNE, cond, const0), 
            Ijk_SigTRAP,
            mode64 ? IRConst_U64(cia) : IRConst_U32((UInt)cia) 
   ));
   return False; /* not an unconditional trap */
}

static Bool dis_trapi ( UInt theInstr,
                        /*OUT*/DisResult* dres )
{
   /* D-Form */
   UChar  opc1    = ifieldOPC(theInstr);
   UChar  TO      = ifieldRegDS(theInstr);
   UChar  rA_addr = ifieldRegA(theInstr);
   UInt   uimm16  = ifieldUIMM16(theInstr);
   ULong  simm16  = extend_s_16to64(uimm16);
   Addr64 cia     = guest_CIA_curr_instr;
   IRType ty      = mode64 ? Ity_I64 : Ity_I32;
   Bool   uncond  = False;

   switch (opc1) {
   case 0x03: // twi  (Trap Word Immediate, PPC32 p548)
      uncond = do_trap( TO, 
                        mode64 ? unop(Iop_64to32, getIReg(rA_addr)) 
                               : getIReg(rA_addr),
                        mkU32( (UInt)simm16 ),
                        cia );
      if (TO == 4) {
         DIP("tweqi r%u,%d\n", (UInt)rA_addr, (Int)simm16);
      } else {
         DIP("tw%di r%u,%d\n", (Int)TO, (UInt)rA_addr, (Int)simm16);
      }
      break;
   case 0x02: // tdi
      if (!mode64)
         return False;
      uncond = do_trap( TO, getIReg(rA_addr), mkU64( (ULong)simm16 ), cia );
      if (TO == 4) {
         DIP("tdeqi r%u,%d\n", (UInt)rA_addr, (Int)simm16);
      } else {
         DIP("td%di r%u,%d\n", (Int)TO, (UInt)rA_addr, (Int)simm16);
      }
      break;
   default:
      return False;
   }

   if (uncond) {
      /* If the trap shows signs of being unconditional, don't
         continue decoding past it. */
      irsb->next     = mkSzImm( ty, nextInsnAddr() );
      irsb->jumpkind = Ijk_Boring;
      dres->whatNext = Dis_StopHere;
   }

   return True;
}

static Bool dis_trap ( UInt theInstr,
                        /*OUT*/DisResult* dres )
{
   /* X-Form */
   UInt   opc2    = ifieldOPClo10(theInstr);
   UChar  TO      = ifieldRegDS(theInstr);
   UChar  rA_addr = ifieldRegA(theInstr);
   UChar  rB_addr = ifieldRegB(theInstr);
   Addr64 cia     = guest_CIA_curr_instr;
   IRType ty      = mode64 ? Ity_I64 : Ity_I32;
   Bool   uncond  = False;

   if (ifieldBIT0(theInstr) != 0)
      return False;

   switch (opc2) {
   case 0x004: // tw  (Trap Word, PPC64 p540)
      uncond = do_trap( TO, 
                        mode64 ? unop(Iop_64to32, getIReg(rA_addr)) 
                               : getIReg(rA_addr),
                        mode64 ? unop(Iop_64to32, getIReg(rB_addr)) 
                               : getIReg(rB_addr),
                        cia );
      if (TO == 4) {
         DIP("tweq r%u,r%u\n", (UInt)rA_addr, (UInt)rB_addr);
      } else {
         DIP("tw%d r%u,r%u\n", (Int)TO, (UInt)rA_addr, (UInt)rB_addr);
      }
      break;
   case 0x044: // td (Trap Doubleword, PPC64 p534)
      if (!mode64)
         return False;
      uncond = do_trap( TO, getIReg(rA_addr), getIReg(rB_addr), cia );
      if (TO == 4) {
         DIP("tdeq r%u,r%u\n", (UInt)rA_addr, (UInt)rB_addr);
      } else {
         DIP("td%d r%u,r%u\n", (Int)TO, (UInt)rA_addr, (UInt)rB_addr);
      }
      break;
   default:
      return False;
   }

   if (uncond) {
      /* If the trap shows signs of being unconditional, don't
         continue decoding past it. */
      irsb->next     = mkSzImm( ty, nextInsnAddr() );
      irsb->jumpkind = Ijk_Boring;
      dres->whatNext = Dis_StopHere;
   }

   return True;
}


/*
  System Linkage Instructions
*/
static Bool dis_syslink ( UInt theInstr, 
                          VexAbiInfo* abiinfo, DisResult* dres )
{
   IRType ty = mode64 ? Ity_I64 : Ity_I32;

   if (theInstr != 0x44000002) {
      vex_printf("dis_syslink(ppc)(theInstr)\n");
      return False;
   }

   // sc  (System Call, PPC32 p504)
   DIP("sc\n");

   /* Copy CIA into the IP_AT_SYSCALL pseudo-register, so that on AIX
      Valgrind can back the guest up to this instruction if it needs
      to restart the syscall. */
   putGST( PPC_GST_IP_AT_SYSCALL, getGST( PPC_GST_CIA ) );

   /* It's important that all ArchRegs carry their up-to-date value
      at this point.  So we declare an end-of-block here, which
      forces any TempRegs caching ArchRegs to be flushed. */
   irsb->next     = abiinfo->guest_ppc_sc_continues_at_LR
                       ? getGST( PPC_GST_LR )
                       : mkSzImm( ty, nextInsnAddr() );
   irsb->jumpkind = Ijk_Sys_syscall;

   dres->whatNext = Dis_StopHere;
   return True;
}


/*
  Memory Synchronization Instructions

  Note on Reservations:
  We rely on the assumption that V will in fact only allow one thread at
  once to run.  In effect, a thread can make a reservation, but we don't
  check any stores it does.  Instead, the reservation is cancelled when
  the scheduler switches to another thread (run_thread_for_a_while()).
*/
static Bool dis_memsync ( UInt theInstr )
{
   /* X-Form, XL-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UInt  b11to25 = IFIELD(theInstr, 11, 15);
   UChar flag_L  = ifieldRegDS(theInstr);
   UInt  b11to20 = IFIELD(theInstr, 11, 10);
   UChar rD_addr = ifieldRegDS(theInstr);
   UChar rS_addr = rD_addr;
   UChar rA_addr = ifieldRegA(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b0      = ifieldBIT0(theInstr);

   IRType ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp EA     = newTemp(ty);

   assign( EA, ea_rAor0_idxd( rA_addr, rB_addr ) );

   switch (opc1) {
   /* XL-Form */
   case 0x13:   // isync (Instruction Synchronize, PPC32 p432)
      if (opc2 != 0x096) {
         vex_printf("dis_memsync(ppc)(0x13,opc2)\n");
         return False;
      }
      if (b11to25 != 0 || b0 != 0) {
         vex_printf("dis_memsync(ppc)(0x13,b11to25|b0)\n");
         return False;
      }
      DIP("isync\n");
      stmt( IRStmt_MBE(Imbe_Fence) );
      break;

   /* X-Form */
   case 0x1F:
      switch (opc2) {
      case 0x356: // eieio (Enforce In-Order Exec of I/O, PPC32 p394)
         if (b11to25 != 0 || b0 != 0) {
            vex_printf("dis_memsync(ppc)(eiei0,b11to25|b0)\n");
            return False;
         }
         DIP("eieio\n");
         /* Insert a memory fence, just to be on the safe side. */
         stmt( IRStmt_MBE(Imbe_Fence) );
         break;

      case 0x014: { // lwarx (Load Word and Reserve Indexed, PPC32 p458)
         IRTemp res;
         /* According to the PowerPC ISA version 2.05, b0 (called EH
            in the documentation) is merely a hint bit to the
            hardware, I think as to whether or not contention is
            likely.  So we can just ignore it. */
         DIP("lwarx r%u,r%u,r%u,EH=%u\n", rD_addr, rA_addr, rB_addr, (UInt)b0);

         // trap if misaligned
         gen_SIGBUS_if_misaligned( EA, 4 );

         // and actually do the load
         res = newTemp(Ity_I32);
         stmt( IRStmt_LLSC(Iend_BE, res, mkexpr(EA), NULL/*this is a load*/) );

         putIReg( rD_addr, mkWidenFrom32(ty, mkexpr(res), False) );
         break;
      }

      case 0x096: { 
         // stwcx. (Store Word Conditional Indexed, PPC32 p532)
         // Note this has to handle stwcx. in both 32- and 64-bit modes,
         // so isn't quite as straightforward as it might otherwise be.
         IRTemp rS = newTemp(Ity_I32);
         IRTemp resSC;
         if (b0 != 1) {
            vex_printf("dis_memsync(ppc)(stwcx.,b0)\n");
            return False;
         }
         DIP("stwcx. r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);

         // trap if misaligned
         gen_SIGBUS_if_misaligned( EA, 4 );

         // Get the data to be stored, and narrow to 32 bits if necessary
         assign( rS, mkNarrowTo32(ty, getIReg(rS_addr)) );

         // Do the store, and get success/failure bit into resSC
         resSC = newTemp(Ity_I1);
         stmt( IRStmt_LLSC(Iend_BE, resSC, mkexpr(EA), mkexpr(rS)) );

         // Set CR0[LT GT EQ S0] = 0b000 || XER[SO]  on failure
         // Set CR0[LT GT EQ S0] = 0b001 || XER[SO]  on success
         putCR321(0, binop(Iop_Shl8, unop(Iop_1Uto8, mkexpr(resSC)), mkU8(1)));
         putCR0(0, getXER_SO());

         /* Note:
            If resaddr != lwarx_resaddr, CR0[EQ] is undefined, and
            whether rS is stored is dependent on that value. */
         /* So I guess we can just ignore this case? */
         break;
      }

      case 0x256: // sync (Synchronize, PPC32 p543), 
                  // also lwsync (L==1), ptesync (L==2)
         /* http://sources.redhat.com/ml/binutils/2000-12/msg00311.html

            The PowerPC architecture used in IBM chips has expanded
            the sync instruction into two variants: lightweight sync
            and heavyweight sync.  The original sync instruction is
            the new heavyweight sync and lightweight sync is a strict
            subset of the heavyweight sync functionality. This allows
            the programmer to specify a less expensive operation on
            high-end systems when the full sync functionality is not
            necessary.

            The basic "sync" mnemonic now utilizes an operand. "sync"
            without an operand now becomes a extended mnemonic for
            heavyweight sync.  Processors without the lwsync
            instruction will not decode the L field and will perform a
            heavyweight sync.  Everything is backward compatible.

            sync    =       sync 0
            lwsync  =       sync 1
            ptesync =       sync 2    *** TODO - not implemented ***
         */
         if (b11to20 != 0 || b0 != 0) {
            vex_printf("dis_memsync(ppc)(sync/lwsync,b11to20|b0)\n");
            return False;
         }
         if (flag_L != 0/*sync*/ && flag_L != 1/*lwsync*/) {
            vex_printf("dis_memsync(ppc)(sync/lwsync,flag_L)\n");
            return False;
         }
         DIP("%ssync\n", flag_L == 1 ? "lw" : "");
         /* Insert a memory fence.  It's sometimes important that these
            are carried through to the generated code. */
         stmt( IRStmt_MBE(Imbe_Fence) );
         break;

      /* 64bit Memsync */
      case 0x054: { // ldarx (Load DWord and Reserve Indexed, PPC64 p473)
         IRTemp res;
         /* According to the PowerPC ISA version 2.05, b0 (called EH
            in the documentation) is merely a hint bit to the
            hardware, I think as to whether or not contention is
            likely.  So we can just ignore it. */
         if (!mode64)
            return False;
         DIP("ldarx r%u,r%u,r%u,EH=%u\n", rD_addr, rA_addr, rB_addr, (UInt)b0);

         // trap if misaligned
         gen_SIGBUS_if_misaligned( EA, 8 );

         // and actually do the load
         res = newTemp(Ity_I64);
         stmt( IRStmt_LLSC(Iend_BE, res, mkexpr(EA), NULL/*this is a load*/) );

         putIReg( rD_addr, mkexpr(res) );
         break;
      }
      
      case 0x0D6: { // stdcx. (Store DWord Condition Indexd, PPC64 p581)
         // A marginally simplified version of the stwcx. case
         IRTemp rS = newTemp(Ity_I64);
         IRTemp resSC;
         if (b0 != 1) {
            vex_printf("dis_memsync(ppc)(stdcx.,b0)\n");
            return False;
         }
         if (!mode64)
            return False;
         DIP("stdcx. r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);

         // trap if misaligned
         gen_SIGBUS_if_misaligned( EA, 8 );

         // Get the data to be stored
         assign( rS, getIReg(rS_addr) );

         // Do the store, and get success/failure bit into resSC
         resSC = newTemp(Ity_I1);
         stmt( IRStmt_LLSC(Iend_BE, resSC, mkexpr(EA), mkexpr(rS)) );

         // Set CR0[LT GT EQ S0] = 0b000 || XER[SO]  on failure
         // Set CR0[LT GT EQ S0] = 0b001 || XER[SO]  on success
         putCR321(0, binop(Iop_Shl8, unop(Iop_1Uto8, mkexpr(resSC)), mkU8(1)));
         putCR0(0, getXER_SO());

         /* Note:
            If resaddr != lwarx_resaddr, CR0[EQ] is undefined, and
            whether rS is stored is dependent on that value. */
         /* So I guess we can just ignore this case? */
         break;
      }

      default:
         vex_printf("dis_memsync(ppc)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_memsync(ppc)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Shift Instructions
*/
static Bool dis_int_shift ( UInt theInstr )
{
   /* X-Form, XS-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar rS_addr = ifieldRegDS(theInstr);
   UChar rA_addr = ifieldRegA(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UChar sh_imm  = rB_addr;
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b1      = ifieldBIT1(theInstr);
   UChar flag_rC = ifieldBIT0(theInstr);

   IRType  ty         = mode64 ? Ity_I64 : Ity_I32;
   IRTemp  rA         = newTemp(ty);
   IRTemp  rS         = newTemp(ty);
   IRTemp  rB         = newTemp(ty);
   IRTemp  outofrange = newTemp(Ity_I8);
   IRTemp  rS_lo32    = newTemp(Ity_I32);
   IRTemp  rB_lo32    = newTemp(Ity_I32);
   IRExpr* e_tmp;

   assign( rS, getIReg(rS_addr) );
   assign( rB, getIReg(rB_addr) );
   assign( rS_lo32, mkNarrowTo32(ty, mkexpr(rS)) );
   assign( rB_lo32, mkNarrowTo32(ty, mkexpr(rB)) );
   
   if (opc1 == 0x1F) {
      switch (opc2) {
      case 0x018: { // slw (Shift Left Word, PPC32 p505)
         DIP("slw%s r%u,r%u,r%u\n", flag_rC ? ".":"",
             rA_addr, rS_addr, rB_addr);
         /* rA = rS << rB */
         /* ppc32 semantics are: 
            slw(x,y) = (x << (y & 31))         -- primary result
                       & ~((y << 26) >>s 31)   -- make result 0 
                                                  for y in 32 .. 63
         */
         e_tmp =
            binop( Iop_And32,
               binop( Iop_Shl32,
                      mkexpr(rS_lo32), 
                      unop( Iop_32to8,
                            binop(Iop_And32,
                                  mkexpr(rB_lo32), mkU32(31)))),
               unop( Iop_Not32,
                     binop( Iop_Sar32,
                            binop(Iop_Shl32, mkexpr(rB_lo32), mkU8(26)),
                            mkU8(31))) );
         assign( rA, mkWidenFrom32(ty, e_tmp, /* Signed */False) );
         break;
      }

      case 0x318: { // sraw (Shift Right Alg Word, PPC32 p506)
         IRTemp sh_amt = newTemp(Ity_I32);
         DIP("sraw%s r%u,r%u,r%u\n", flag_rC ? ".":"",
             rA_addr, rS_addr, rB_addr);
         /* JRS: my reading of the (poorly worded) PPC32 doc p506 is:
            amt = rB & 63
            rA = Sar32( rS, amt > 31 ? 31 : amt )
            XER.CA = amt > 31 ? sign-of-rS : (computation as per srawi)
         */
         assign( sh_amt, binop(Iop_And32, mkU32(0x3F),
                                          mkexpr(rB_lo32)) );
         assign( outofrange,
                 unop( Iop_1Uto8, 
                       binop(Iop_CmpLT32U, mkU32(31),
                                           mkexpr(sh_amt)) ));
         e_tmp = binop( Iop_Sar32, 
                        mkexpr(rS_lo32), 
                        unop( Iop_32to8, 
                              IRExpr_Mux0X( mkexpr(outofrange), 
                                            mkexpr(sh_amt), 
                                            mkU32(31)) ) );
         assign( rA, mkWidenFrom32(ty, e_tmp, /* Signed */True) );

         set_XER_CA( ty, PPCG_FLAG_OP_SRAW,
                     mkexpr(rA),
                     mkWidenFrom32(ty, mkexpr(rS_lo32), True),
                     mkWidenFrom32(ty, mkexpr(sh_amt), True ),
                     mkWidenFrom32(ty, getXER_CA32(), True) );
         break;
      }
         
      case 0x338: // srawi (Shift Right Alg Word Immediate, PPC32 p507)
         DIP("srawi%s r%u,r%u,%d\n", flag_rC ? ".":"",
             rA_addr, rS_addr, sh_imm);
         vassert(sh_imm < 32);
         if (mode64) {
            assign( rA, binop(Iop_Sar64,
                              binop(Iop_Shl64, getIReg(rS_addr),
                                               mkU8(32)),
                              mkU8(32 + sh_imm)) );
         } else {
            assign( rA, binop(Iop_Sar32, mkexpr(rS_lo32),
                                         mkU8(sh_imm)) );
         }

         set_XER_CA( ty, PPCG_FLAG_OP_SRAWI, 
                     mkexpr(rA),
                     mkWidenFrom32(ty, mkexpr(rS_lo32), /* Syned */True),
                     mkSzImm(ty, sh_imm),
                     mkWidenFrom32(ty, getXER_CA32(), /* Syned */False) );
         break;
      
      case 0x218: // srw (Shift Right Word, PPC32 p508)
         DIP("srw%s r%u,r%u,r%u\n", flag_rC ? ".":"",
             rA_addr, rS_addr, rB_addr);
         /* rA = rS >>u rB */
         /* ppc32 semantics are: 
            srw(x,y) = (x >>u (y & 31))        -- primary result
                       & ~((y << 26) >>s 31)   -- make result 0 
                                                  for y in 32 .. 63
         */
         e_tmp = 
            binop(
               Iop_And32,
               binop( Iop_Shr32, 
                      mkexpr(rS_lo32), 
                      unop( Iop_32to8, 
                            binop(Iop_And32, mkexpr(rB_lo32),
                                             mkU32(31)))),
               unop( Iop_Not32, 
                     binop( Iop_Sar32, 
                            binop(Iop_Shl32, mkexpr(rB_lo32),
                                             mkU8(26)), 
                            mkU8(31))));
         assign( rA, mkWidenFrom32(ty, e_tmp, /* Signed */False) );
         break;


      /* 64bit Shifts */
      case 0x01B: // sld (Shift Left DWord, PPC64 p568)
         DIP("sld%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         /* rA = rS << rB */
         /* ppc64 semantics are: 
            slw(x,y) = (x << (y & 63))         -- primary result
                       & ~((y << 57) >>s 63)   -- make result 0 
                                                  for y in 64 .. 
         */
         assign( rA,
            binop(
               Iop_And64,
               binop( Iop_Shl64,
                      mkexpr(rS), 
                      unop( Iop_64to8, 
                            binop(Iop_And64, mkexpr(rB), mkU64(63)))),
               unop( Iop_Not64,
                     binop( Iop_Sar64,
                            binop(Iop_Shl64, mkexpr(rB), mkU8(57)), 
                            mkU8(63)))) );
         break;
      
      case 0x31A: { // srad (Shift Right Alg DWord, PPC64 p570)
         IRTemp sh_amt = newTemp(Ity_I64);
         DIP("srad%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         /* amt = rB & 127
            rA = Sar64( rS, amt > 63 ? 63 : amt )
            XER.CA = amt > 63 ? sign-of-rS : (computation as per srawi)
         */
         assign( sh_amt, binop(Iop_And64, mkU64(0x7F), mkexpr(rB)) );
         assign( outofrange,
                 unop( Iop_1Uto8, 
                       binop(Iop_CmpLT64U, mkU64(63),
                                           mkexpr(sh_amt)) ));
         assign( rA,
                 binop( Iop_Sar64, 
                        mkexpr(rS), 
                        unop( Iop_64to8, 
                              IRExpr_Mux0X( mkexpr(outofrange), 
                                            mkexpr(sh_amt), 
                                            mkU64(63)) ))
               );
         set_XER_CA( ty, PPCG_FLAG_OP_SRAD,
                     mkexpr(rA), mkexpr(rS), mkexpr(sh_amt),
                     mkWidenFrom32(ty, getXER_CA32(), /* Syned */False) );
         break;
      }

      case 0x33A: case 0x33B: // sradi (Shr Alg DWord Imm, PPC64 p571)
         sh_imm |= b1<<5;
         vassert(sh_imm < 64);
         DIP("sradi%s r%u,r%u,%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, sh_imm);
         assign( rA, binop(Iop_Sar64, getIReg(rS_addr), mkU8(sh_imm)) );

         set_XER_CA( ty, PPCG_FLAG_OP_SRADI, 
                     mkexpr(rA),
                     getIReg(rS_addr),
                     mkU64(sh_imm), 
                     mkWidenFrom32(ty, getXER_CA32(), /* Syned */False) );
         break;

      case 0x21B: // srd (Shift Right DWord, PPC64 p574)
         DIP("srd%s r%u,r%u,r%u\n",
             flag_rC ? ".":"", rA_addr, rS_addr, rB_addr);
         /* rA = rS >>u rB */
         /* ppc semantics are: 
            srw(x,y) = (x >>u (y & 63))        -- primary result
                       & ~((y << 57) >>s 63)   -- make result 0 
                                                  for y in 64 .. 127
         */
         assign( rA,
            binop(
               Iop_And64,
               binop( Iop_Shr64, 
                      mkexpr(rS), 
                      unop( Iop_64to8, 
                            binop(Iop_And64, mkexpr(rB), mkU64(63)))),
               unop( Iop_Not64, 
                     binop( Iop_Sar64, 
                            binop(Iop_Shl64, mkexpr(rB), mkU8(57)), 
                            mkU8(63)))) );
         break;
     
      default:
         vex_printf("dis_int_shift(ppc)(opc2)\n");
         return False;
      }
   } else {
      vex_printf("dis_int_shift(ppc)(opc1)\n");
      return False;
   }

   putIReg( rA_addr, mkexpr(rA) );
   
   if (flag_rC) {
      set_CR0( mkexpr(rA) );
   }
   return True;
}



/*
  Integer Load/Store Reverse Instructions
*/
/* Generates code to swap the byte order in an Ity_I32. */
static IRExpr* /* :: Ity_I32 */ gen_byterev32 ( IRTemp t )
{
   vassert(typeOfIRTemp(irsb->tyenv, t) == Ity_I32);
   return
      binop(Iop_Or32,
         binop(Iop_Shl32, mkexpr(t), mkU8(24)),
      binop(Iop_Or32,
         binop(Iop_And32, binop(Iop_Shl32, mkexpr(t), mkU8(8)), 
                          mkU32(0x00FF0000)),
      binop(Iop_Or32,
         binop(Iop_And32, binop(Iop_Shr32, mkexpr(t), mkU8(8)),
                          mkU32(0x0000FF00)),
         binop(Iop_And32, binop(Iop_Shr32, mkexpr(t), mkU8(24)),
                          mkU32(0x000000FF) )
      )));
}

/* Generates code to swap the byte order in the lower half of an Ity_I32,
   and zeroes the upper half. */
static IRExpr* /* :: Ity_I32 */ gen_byterev16 ( IRTemp t )
{
   vassert(typeOfIRTemp(irsb->tyenv, t) == Ity_I32);
   return
      binop(Iop_Or32,
         binop(Iop_And32, binop(Iop_Shl32, mkexpr(t), mkU8(8)),
                          mkU32(0x0000FF00)),
         binop(Iop_And32, binop(Iop_Shr32, mkexpr(t), mkU8(8)),
                          mkU32(0x000000FF))
      );
}

static Bool dis_int_ldst_rev ( UInt theInstr )
{
   /* X-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar rD_addr = ifieldRegDS(theInstr);
   UChar rS_addr = rD_addr;
   UChar rA_addr = ifieldRegA(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b0      = ifieldBIT0(theInstr);

   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IRTemp EA = newTemp(ty);
   IRTemp w1 = newTemp(Ity_I32);
   IRTemp w2 = newTemp(Ity_I32);

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_int_ldst_rev(ppc)(opc1|b0)\n");
      return False;
   }

   assign( EA, ea_rAor0_idxd( rA_addr, rB_addr ) );
   
   switch (opc2) {

      case 0x316: // lhbrx (Load Halfword Byte-Reverse Indexed, PPC32 p449)
         DIP("lhbrx r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         assign( w1, unop(Iop_16Uto32, loadBE(Ity_I16, mkexpr(EA))) );
         assign( w2, gen_byterev16(w1) );
         putIReg( rD_addr, mkWidenFrom32(ty, mkexpr(w2),
                                         /* Signed */False) );
         break;

      case 0x216: // lwbrx (Load Word Byte-Reverse Indexed, PPC32 p459)
         DIP("lwbrx r%u,r%u,r%u\n", rD_addr, rA_addr, rB_addr);
         assign( w1, loadBE(Ity_I32, mkexpr(EA)) );
         assign( w2, gen_byterev32(w1) );
         putIReg( rD_addr, mkWidenFrom32(ty, mkexpr(w2),
                                         /* Signed */False) );
         break;
      
      case 0x396: // sthbrx (Store Half Word Byte-Reverse Indexed, PPC32 p523)
         DIP("sthbrx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         assign( w1, mkNarrowTo32(ty, getIReg(rS_addr)) );
         storeBE( mkexpr(EA), unop(Iop_32to16, gen_byterev16(w1)) );
         break;
      
      case 0x296: // stwbrx (Store Word Byte-Reverse Indxd, PPC32 p531)
         DIP("stwbrx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         assign( w1, mkNarrowTo32(ty, getIReg(rS_addr)) );
         storeBE( mkexpr(EA), gen_byterev32(w1) );
         break;
      
      default:
         vex_printf("dis_int_ldst_rev(ppc)(opc2)\n");
         return False;
   }
   return True;
}



/*
  Processor Control Instructions
*/
static Bool dis_proc_ctl ( VexAbiInfo* vbi, UInt theInstr )
{
   UChar opc1     = ifieldOPC(theInstr);
   
   /* X-Form */
   UChar crfD     = toUChar( IFIELD( theInstr, 23, 3 ) );
   UChar b21to22  = toUChar( IFIELD( theInstr, 21, 2 ) );
   UChar rD_addr  = ifieldRegDS(theInstr);
   UInt  b11to20  = IFIELD( theInstr, 11, 10 );

   /* XFX-Form */
   UChar rS_addr  = rD_addr;
   UInt  SPR      = b11to20;
   UInt  TBR      = b11to20;
   UChar b20      = toUChar( IFIELD( theInstr, 20, 1 ) );
   UInt  CRM      = IFIELD( theInstr, 12, 8 );
   UChar b11      = toUChar( IFIELD( theInstr, 11, 1 ) );

   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   IRType ty = mode64 ? Ity_I64 : Ity_I32;
   IRTemp rS = newTemp(ty);
   assign( rS, getIReg(rS_addr) );

   /* Reorder SPR field as per PPC32 p470 */
   SPR = ((SPR & 0x1F) << 5) | ((SPR >> 5) & 0x1F);
   /* Reorder TBR field as per PPC32 p475 */
   TBR = ((TBR & 31) << 5) | ((TBR >> 5) & 31);
   
   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_proc_ctl(ppc)(opc1|b0)\n");
      return False;
   }
   
   switch (opc2) {
   /* X-Form */
   case 0x200: { // mcrxr (Move to Cond Register from XER, PPC32 p466)
      if (b21to22 != 0 || b11to20 != 0) {
         vex_printf("dis_proc_ctl(ppc)(mcrxr,b21to22|b11to20)\n");
         return False;
      }
      DIP("mcrxr crf%d\n", crfD);
      /* Move XER[0-3] (the top 4 bits of XER) to CR[crfD] */
      putGST_field( PPC_GST_CR,
                    getGST_field( PPC_GST_XER, 7 ),
                    crfD );

      // Clear XER[0-3]
      putXER_SO( mkU8(0) );
      putXER_OV( mkU8(0) );
      putXER_CA( mkU8(0) );
      break;
   }
      
   case 0x013: 
      // b11to20==0:      mfcr (Move from Cond Register, PPC32 p467)
      // b20==1 & b11==0: mfocrf (Move from One CR Field)
      // However it seems that the 'mfcr' behaviour is an acceptable
      // implementation of mfocr (from the 2.02 arch spec)
      if (b11to20 == 0) {
         DIP("mfcr r%u\n", rD_addr);
         putIReg( rD_addr, mkWidenFrom32(ty, getGST( PPC_GST_CR ),
                                         /* Signed */False) );
         break;
      }
      if (b20 == 1 && b11 == 0) {
         DIP("mfocrf r%u,%u\n", rD_addr, CRM);
         putIReg( rD_addr, mkWidenFrom32(ty, getGST( PPC_GST_CR ),
                                         /* Signed */False) );
         break;
      }
      /* not decodable */
      return False;
    
   /* XFX-Form */
   case 0x153: // mfspr (Move from Special-Purpose Register, PPC32 p470)
      
      switch (SPR) {  // Choose a register...
      case 0x1:
         DIP("mfxer r%u\n", rD_addr);
         putIReg( rD_addr, mkWidenFrom32(ty, getGST( PPC_GST_XER ),
                                         /* Signed */False) );
         break;
      case 0x8:
         DIP("mflr r%u\n", rD_addr);
         putIReg( rD_addr, getGST( PPC_GST_LR ) ); 
         break;
      case 0x9:
         DIP("mfctr r%u\n", rD_addr);
         putIReg( rD_addr, getGST( PPC_GST_CTR ) ); 
         break;
      case 0x100: 
         DIP("mfvrsave r%u\n", rD_addr);
         putIReg( rD_addr, mkWidenFrom32(ty, getGST( PPC_GST_VRSAVE ),
                                         /* Signed */False) );
         break;

      case 0x103:
         DIP("mfspr r%u, SPRG3(readonly)\n", rD_addr);
         putIReg( rD_addr, getGST( PPC_GST_SPRG3_RO ) );
         break;

      /* Even a lowly PPC7400 can run the associated helper, so no
         obvious need for feature testing at this point. */
      case 268 /* 0x10C */:
      case 269 /* 0x10D */: {
         UInt     arg  = SPR==268 ? 0 : 1;
         IRTemp   val  = newTemp(Ity_I32);
         IRExpr** args = mkIRExprVec_1( mkU32(arg) );
         IRDirty* d    = unsafeIRDirty_1_N(
                            val,
                            0/*regparms*/,
                            "ppc32g_dirtyhelper_MFSPR_268_269",
                            fnptr_to_fnentry
                               (vbi, &ppc32g_dirtyhelper_MFSPR_268_269),
                            args
                         );
         /* execute the dirty call, dumping the result in val. */
         stmt( IRStmt_Dirty(d) );
         putIReg( rD_addr,
                  mkWidenFrom32(ty, mkexpr(val), False/*unsigned*/) );
         DIP("mfspr r%u,%u", rD_addr, (UInt)SPR);
         break;
      }

      /* Again, runs natively on PPC7400 (7447, really).  Not
         bothering with a feature test. */
      case 287: /* 0x11F */ {
         IRTemp   val  = newTemp(Ity_I32);
         IRExpr** args = mkIRExprVec_0();
         IRDirty* d    = unsafeIRDirty_1_N(
                            val,
                            0/*regparms*/,
                            "ppc32g_dirtyhelper_MFSPR_287",
                            fnptr_to_fnentry
                               (vbi, &ppc32g_dirtyhelper_MFSPR_287),
                            args
                         );
         /* execute the dirty call, dumping the result in val. */
         stmt( IRStmt_Dirty(d) );
         putIReg( rD_addr,
                  mkWidenFrom32(ty, mkexpr(val), False/*unsigned*/) );
         DIP("mfspr r%u,%u", rD_addr, (UInt)SPR);
         break;
      }

      default:
         vex_printf("dis_proc_ctl(ppc)(mfspr,SPR)(0x%x)\n", SPR);
         return False;
      }
      break;
      
   case 0x173: { // mftb (Move from Time Base, PPC32 p475)
      IRTemp   val  = newTemp(Ity_I64);
      IRExpr** args = mkIRExprVec_0();
      IRDirty* d    = unsafeIRDirty_1_N(
                              val, 
                              0/*regparms*/, 
                              "ppcg_dirtyhelper_MFTB", 
                              fnptr_to_fnentry(vbi, &ppcg_dirtyhelper_MFTB), 
                              args );
      /* execute the dirty call, dumping the result in val. */
      stmt( IRStmt_Dirty(d) );

      switch (TBR) {
      case 269: 
         DIP("mftbu r%u", rD_addr);
         putIReg( rD_addr,
                  mkWidenFrom32(ty, unop(Iop_64HIto32, mkexpr(val)),
                                /* Signed */False) );
         break;
      case 268: 
         DIP("mftb r%u", rD_addr);
         putIReg( rD_addr, (mode64) ? mkexpr(val) :
                                      unop(Iop_64to32, mkexpr(val)) );
         break;
      default:
         return False; /* illegal instruction */
      }
      break;
   }

   case 0x090: { 
      // b20==0: mtcrf (Move to Cond Register Fields, PPC32 p477)
      // b20==1: mtocrf (Move to One Cond Reg Field)
      Int   cr;
      UChar shft;
      if (b11 != 0)
         return False;
      if (b20 == 1) {
         /* ppc64 v2.02 spec says mtocrf gives undefined outcome if >
            1 field is written.  It seems more robust to decline to
            decode the insn if so. */
         switch (CRM) {
            case 0x01: case 0x02: case 0x04: case 0x08:
            case 0x10: case 0x20: case 0x40: case 0x80:
               break;
            default: 
               return False; 
         }
      }
      DIP("%s 0x%x,r%u\n", b20==1 ? "mtocrf" : "mtcrf", 
                           CRM, rS_addr);
      /* Write to each field specified by CRM */
      for (cr = 0; cr < 8; cr++) {
         if ((CRM & (1 << (7-cr))) == 0)
            continue;
         shft = 4*(7-cr);
         putGST_field( PPC_GST_CR,
                       binop(Iop_Shr32,
                             mkNarrowTo32(ty, mkexpr(rS)),
                             mkU8(shft)), cr );
      }
      break;
   }

   case 0x1D3: // mtspr (Move to Special-Purpose Register, PPC32 p483)
      
      switch (SPR) {  // Choose a register...
      case 0x1:
         DIP("mtxer r%u\n", rS_addr);
         putGST( PPC_GST_XER, mkNarrowTo32(ty, mkexpr(rS)) );
         break;
      case 0x8:
         DIP("mtlr r%u\n", rS_addr);
         putGST( PPC_GST_LR, mkexpr(rS) ); 
         break;
      case 0x9:
         DIP("mtctr r%u\n", rS_addr);
         putGST( PPC_GST_CTR, mkexpr(rS) ); 
         break;
      case 0x100:
         DIP("mtvrsave r%u\n", rS_addr);
         putGST( PPC_GST_VRSAVE, mkNarrowTo32(ty, mkexpr(rS)) );
         break;
         
      default:
         vex_printf("dis_proc_ctl(ppc)(mtspr,SPR)(%u)\n", SPR);
         return False;
      }
      break;
      
   default:
      vex_printf("dis_proc_ctl(ppc)(opc2)\n");
      return False;
   }
   return True;
}


/*
  Cache Management Instructions
*/
static Bool dis_cache_manage ( UInt         theInstr, 
                               DisResult*   dres,
                               VexArchInfo* guest_archinfo )
{
   /* X-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar b21to25 = ifieldRegDS(theInstr);
   UChar rA_addr = ifieldRegA(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b0      = ifieldBIT0(theInstr);
   UInt  lineszB = guest_archinfo->ppc_cache_line_szB;
   Bool  is_dcbzl = False;

   IRType ty     = mode64 ? Ity_I64 : Ity_I32;

   /* For dcbt, the lowest two bits of b21to25 encode an
      access-direction hint (TH field) which we ignore.  Well, that's
      what the PowerPC documentation says.  In fact xlc -O4 on POWER5
      seems to generate values of 8 and 10 for b21to25. */
   if (opc1 == 0x1F && opc2 == 0x116) {
     /* b21to25 &= ~3; */ /* if the docs were true */
     b21to25 = 0; /* blunt instrument */
   }
   if (opc1 == 0x1F && opc2 == 0x3F6) { // dcbz
      if (b21to25 == 1) {
         is_dcbzl = True;
         b21to25 = 0;
         if (!(guest_archinfo->ppc_dcbzl_szB)) {
            vex_printf("dis_cache_manage(ppc)(dcbzl not supported by host)\n");
            return False;
         }
      }
   }

   if (opc1 != 0x1F || b21to25 != 0 || b0 != 0) {
      if (0) vex_printf("dis_cache_manage %d %d %d\n", 
                        (Int)opc1, (Int)b21to25, (Int)b0);
      vex_printf("dis_cache_manage(ppc)(opc1|b21to25|b0)\n");
      return False;
   }

   /* stay sane .. */
   vassert(lineszB == 32 || lineszB == 64 || lineszB == 128);
   
   switch (opc2) {
//zz    case 0x2F6: // dcba (Data Cache Block Allocate, PPC32 p380)
//zz       vassert(0); /* AWAITING TEST CASE */
//zz       DIP("dcba r%u,r%u\n", rA_addr, rB_addr);
//zz       if (0) vex_printf("vex ppc->IR: kludged dcba\n");
//zz       break;
      
   case 0x056: // dcbf (Data Cache Block Flush, PPC32 p382)
      DIP("dcbf r%u,r%u\n", rA_addr, rB_addr);
      /* nop as far as vex is concerned */
      break;
      
   case 0x036: // dcbst (Data Cache Block Store, PPC32 p384)
      DIP("dcbst r%u,r%u\n", rA_addr, rB_addr);
      /* nop as far as vex is concerned */
      break;

   case 0x116: // dcbt (Data Cache Block Touch, PPC32 p385)
      DIP("dcbt r%u,r%u\n", rA_addr, rB_addr);
      /* nop as far as vex is concerned */
      break;
      
   case 0x0F6: // dcbtst (Data Cache Block Touch for Store, PPC32 p386)
      DIP("dcbtst r%u,r%u\n", rA_addr, rB_addr);
      /* nop as far as vex is concerned */
      break;
      
   case 0x3F6: { // dcbz (Data Cache Block Clear to Zero, PPC32 p387)
                 // dcbzl (Data Cache Block Clear to Zero Long, bug#135264)
      /* Clear all bytes in cache block at (rA|0) + rB. */
      IRTemp  EA   = newTemp(ty);
      IRTemp  addr = newTemp(ty);
      IRExpr* irx_addr;
      UInt    i;
      UInt clearszB;
      if (is_dcbzl) {
          clearszB = guest_archinfo->ppc_dcbzl_szB;
          DIP("dcbzl r%u,r%u\n", rA_addr, rB_addr);
      }
      else {
          clearszB = guest_archinfo->ppc_dcbz_szB;
          DIP("dcbz r%u,r%u\n", rA_addr, rB_addr);
      }

      assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );

      if (mode64) {
         /* Round EA down to the start of the containing block. */
         assign( addr, binop( Iop_And64,
                              mkexpr(EA),
                              mkU64( ~((ULong)clearszB-1) )) );
         
         for (i = 0; i < clearszB / 8; i++) {
            irx_addr = binop( Iop_Add64, mkexpr(addr), mkU64(i*8) );
            storeBE( irx_addr, mkU64(0) );
         }
      } else {
         /* Round EA down to the start of the containing block. */
         assign( addr, binop( Iop_And32,
                              mkexpr(EA),
                              mkU32( ~(clearszB-1) )) );
         
         for (i = 0; i < clearszB / 4; i++) {
            irx_addr = binop( Iop_Add32, mkexpr(addr), mkU32(i*4) );
            storeBE( irx_addr, mkU32(0) );
         }
      }
      break;
   }

   case 0x3D6: { 
      // icbi (Instruction Cache Block Invalidate, PPC32 p431)
      /* Invalidate all translations containing code from the cache
         block at (rA|0) + rB. */
      IRTemp EA   = newTemp(ty);
      IRTemp addr = newTemp(ty);
      DIP("icbi r%u,r%u\n", rA_addr, rB_addr);
      assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );

      /* Round EA down to the start of the containing block. */
      assign( addr, binop( mkSzOp(ty, Iop_And8),
                           mkexpr(EA),
                           mkSzImm(ty, ~(((ULong)lineszB)-1) )) );
      putGST( PPC_GST_TISTART, mkexpr(addr) );
      putGST( PPC_GST_TILEN, mkSzImm(ty, lineszB) );

      /* be paranoid ... */
      stmt( IRStmt_MBE(Imbe_Fence) );

      irsb->jumpkind = Ijk_TInval;
      irsb->next     = mkSzImm(ty, nextInsnAddr());
      dres->whatNext = Dis_StopHere;
      break;
   }

   default:
      vex_printf("dis_cache_manage(ppc)(opc2)\n");
      return False;
   }
   return True;
}


/*------------------------------------------------------------*/
/*--- Floating Point Helpers                               ---*/
/*------------------------------------------------------------*/

/* --------- Synthesise a 2-bit FPU rounding mode. --------- */
/* Produces a value in 0 .. 3, which is encoded as per the type
   IRRoundingMode.  PPCRoundingMode encoding is different to
   IRRoundingMode, so need to map it.
*/
static IRExpr* /* :: Ity_I32 */ get_IR_roundingmode ( void )
{
/* 
   rounding mode | PPC | IR
   ------------------------
   to nearest    | 00  | 00
   to zero       | 01  | 11
   to +infinity  | 10  | 10
   to -infinity  | 11  | 01
*/
   IRTemp rm_PPC32 = newTemp(Ity_I32);
   assign( rm_PPC32, getGST_masked( PPC_GST_FPSCR, MASK_FPSCR_RN ) );

   // rm_IR = XOR( rm_PPC32, (rm_PPC32 << 1) & 2)
   return binop( Iop_Xor32, 
                 mkexpr(rm_PPC32),
                 binop( Iop_And32, 
                        binop(Iop_Shl32, mkexpr(rm_PPC32), mkU8(1)),
                        mkU32(2) ));
}


/*------------------------------------------------------------*/
/*--- Floating Point Instruction Translation               ---*/
/*------------------------------------------------------------*/

/*
  Floating Point Load Instructions
*/
static Bool dis_fp_load ( UInt theInstr )
{
   /* X-Form, D-Form */
   UChar opc1      = ifieldOPC(theInstr);
   UChar frD_addr  = ifieldRegDS(theInstr);
   UChar rA_addr   = ifieldRegA(theInstr);
   UChar rB_addr   = ifieldRegB(theInstr);
   UInt  opc2      = ifieldOPClo10(theInstr);
   UChar b0        = ifieldBIT0(theInstr);
   UInt  uimm16    = ifieldUIMM16(theInstr);

   Int    simm16 = extend_s_16to32(uimm16);
   IRType ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp EA     = newTemp(ty);
   IRTemp rA     = newTemp(ty);
   IRTemp rB     = newTemp(ty);
   IRTemp iHi    = newTemp(Ity_I32);
   IRTemp iLo    = newTemp(Ity_I32);

   assign( rA, getIReg(rA_addr) );
   assign( rB, getIReg(rB_addr) );

   /* These are completely straightforward from a rounding and status
      bits perspective: no rounding involved and no funny status or CR
      bits affected. */

   switch (opc1) {
   case 0x30: // lfs (Load Float Single, PPC32 p441)
      DIP("lfs fr%u,%d(r%u)\n", frD_addr, simm16, rA_addr);
      assign( EA, ea_rAor0_simm(rA_addr, simm16) );
      putFReg( frD_addr,
               unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
      break;

   case 0x31: // lfsu (Load Float Single, Update, PPC32 p442)
      if (rA_addr == 0)
         return False;
      DIP("lfsu fr%u,%d(r%u)\n", frD_addr, simm16, rA_addr);
      assign( EA, ea_rA_simm(rA_addr, simm16) );
      putFReg( frD_addr,
               unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
      putIReg( rA_addr, mkexpr(EA) );
      break;
      
   case 0x32: // lfd (Load Float Double, PPC32 p437)
      DIP("lfd fr%u,%d(r%u)\n", frD_addr, simm16, rA_addr);
      assign( EA, ea_rAor0_simm(rA_addr, simm16) );
      putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
      break;

   case 0x33: // lfdu (Load Float Double, Update, PPC32 p438)
      if (rA_addr == 0)
         return False;
      DIP("lfdu fr%u,%d(r%u)\n", frD_addr, simm16, rA_addr);
      assign( EA, ea_rA_simm(rA_addr, simm16) );
      putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_fp_load(ppc)(instr,b0)\n");
         return False;
      }

      switch(opc2) {
      case 0x217: // lfsx (Load Float Single Indexed, PPC32 p444)
         DIP("lfsx fr%u,r%u,r%u\n", frD_addr, rA_addr, rB_addr);
         assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );
         putFReg( frD_addr, unop( Iop_F32toF64, 
                                  loadBE(Ity_F32, mkexpr(EA))) );
         break;
         
      case 0x237: // lfsux (Load Float Single, Update Indxd, PPC32 p443)
         if (rA_addr == 0)
            return False;
         DIP("lfsux fr%u,r%u,r%u\n", frD_addr, rA_addr, rB_addr);
         assign( EA, ea_rA_idxd(rA_addr, rB_addr) );
         putFReg( frD_addr,
                  unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
         putIReg( rA_addr, mkexpr(EA) );
         break;
         
      case 0x257: // lfdx (Load Float Double Indexed, PPC32 p440)
         DIP("lfdx fr%u,r%u,r%u\n", frD_addr, rA_addr, rB_addr);
         assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );
         putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
         break;
         
      case 0x277: // lfdux (Load Float Double, Update Indxd, PPC32 p439)
         if (rA_addr == 0)
            return False;
         DIP("lfdux fr%u,r%u,r%u\n", frD_addr, rA_addr, rB_addr);
         assign( EA, ea_rA_idxd(rA_addr, rB_addr) );
         putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
         putIReg( rA_addr, mkexpr(EA) );
         break;
         
      case 0x357: // lfiwax (Load Float As Integer, Indxd, ISA 2.05 p120)
         DIP("lfiwax fr%u,r%u,r%u\n", frD_addr, rA_addr, rB_addr);
         assign( EA, ea_rAor0_idxd( rA_addr, rB_addr ) );
         assign( iLo, loadBE(Ity_I32, mkexpr(EA)) );
         assign( iHi, binop(Iop_Sub32,
                            mkU32(0),
                            binop(Iop_Shr32, mkexpr(iLo), mkU8(31)))  );
         putFReg( frD_addr, unop(Iop_ReinterpI64asF64,
                                 binop(Iop_32HLto64, mkexpr(iHi), mkexpr(iLo))) );
         break;

      default:
         vex_printf("dis_fp_load(ppc)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_load(ppc)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Store Instructions
*/
static Bool dis_fp_store ( UInt theInstr )
{
   /* X-Form, D-Form */
   UChar opc1      = ifieldOPC(theInstr);
   UChar frS_addr  = ifieldRegDS(theInstr);
   UChar rA_addr   = ifieldRegA(theInstr);
   UChar rB_addr   = ifieldRegB(theInstr);
   UInt  opc2      = ifieldOPClo10(theInstr);
   UChar b0        = ifieldBIT0(theInstr);
   Int   uimm16    = ifieldUIMM16(theInstr);

   Int    simm16 = extend_s_16to32(uimm16);
   IRTemp frS    = newTemp(Ity_F64);
   IRType ty     = mode64 ? Ity_I64 : Ity_I32;
   IRTemp EA     = newTemp(ty);
   IRTemp rA     = newTemp(ty);
   IRTemp rB     = newTemp(ty);

   assign( frS, getFReg(frS_addr) );
   assign( rA,  getIReg(rA_addr) );
   assign( rB,  getIReg(rB_addr) );

   /* These are straightforward from a status bits perspective: no
      funny status or CR bits affected.  For single precision stores,
      the values are truncated and denormalised (not rounded) to turn
      them into single precision values. */

   switch (opc1) {

   case 0x34: // stfs (Store Float Single, PPC32 p518)
      DIP("stfs fr%u,%d(r%u)\n", frS_addr, simm16, rA_addr);
      assign( EA, ea_rAor0_simm(rA_addr, simm16) );
      /* Use Iop_TruncF64asF32 to truncate and possible denormalise
         the value to be stored in the correct way, without any
         rounding. */
      storeBE( mkexpr(EA),
               unop(Iop_TruncF64asF32, mkexpr(frS)) );
      break;

   case 0x35: // stfsu (Store Float Single, Update, PPC32 p519)
      if (rA_addr == 0)
         return False;
      DIP("stfsu fr%u,%d(r%u)\n", frS_addr, simm16, rA_addr);
      assign( EA, ea_rA_simm(rA_addr, simm16) );
      /* See comment for stfs */
      storeBE( mkexpr(EA),
               unop(Iop_TruncF64asF32, mkexpr(frS)) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x36: // stfd (Store Float Double, PPC32 p513)
      DIP("stfd fr%u,%d(r%u)\n", frS_addr, simm16, rA_addr);
      assign( EA, ea_rAor0_simm(rA_addr, simm16) );
      storeBE( mkexpr(EA), mkexpr(frS) );
      break;

   case 0x37: // stfdu (Store Float Double, Update, PPC32 p514)
      if (rA_addr == 0)
         return False;
      DIP("stfdu fr%u,%d(r%u)\n", frS_addr, simm16, rA_addr);
      assign( EA, ea_rA_simm(rA_addr, simm16) );
      storeBE( mkexpr(EA), mkexpr(frS) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_fp_store(ppc)(instr,b0)\n");
         return False;
      }
      switch(opc2) {
      case 0x297: // stfsx (Store Float Single Indexed, PPC32 p521)
         DIP("stfsx fr%u,r%u,r%u\n", frS_addr, rA_addr, rB_addr);
         assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );
         /* See note for stfs */
         storeBE( mkexpr(EA), 
                  unop(Iop_TruncF64asF32, mkexpr(frS)) );
         break;
         
      case 0x2B7: // stfsux (Store Float Sgl, Update Indxd, PPC32 p520)
         if (rA_addr == 0)
            return False;
         DIP("stfsux fr%u,r%u,r%u\n", frS_addr, rA_addr, rB_addr);
         assign( EA, ea_rA_idxd(rA_addr, rB_addr) );
         /* See note for stfs */
         storeBE( mkexpr(EA), 
                  unop(Iop_TruncF64asF32, mkexpr(frS)) );
         putIReg( rA_addr, mkexpr(EA) );
         break;

      case 0x2D7: // stfdx (Store Float Double Indexed, PPC32 p516)
         DIP("stfdx fr%u,r%u,r%u\n", frS_addr, rA_addr, rB_addr);
         assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );
         storeBE( mkexpr(EA), mkexpr(frS) );
         break;
         
      case 0x2F7: // stfdux (Store Float Dbl, Update Indxd, PPC32 p515)
         if (rA_addr == 0)
            return False;
         DIP("stfdux fr%u,r%u,r%u\n", frS_addr, rA_addr, rB_addr);
         assign( EA, ea_rA_idxd(rA_addr, rB_addr) );
         storeBE( mkexpr(EA), mkexpr(frS) );
         putIReg( rA_addr, mkexpr(EA) );
         break;

      case 0x3D7: // stfiwx (Store Float as Int, Indexed, PPC32 p517)
         // NOTE: POWERPC OPTIONAL, "Graphics Group" (PPC32_GX)
         DIP("stfiwx fr%u,r%u,r%u\n", frS_addr, rA_addr, rB_addr);
         assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );
         storeBE( mkexpr(EA),
                  unop(Iop_64to32, unop(Iop_ReinterpF64asI64, mkexpr(frS))) );
         break;

      default:
         vex_printf("dis_fp_store(ppc)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_store(ppc)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Arith Instructions
*/
static Bool dis_fp_arith ( UInt theInstr )
{
   /* A-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar frD_addr = ifieldRegDS(theInstr);
   UChar frA_addr = ifieldRegA(theInstr);
   UChar frB_addr = ifieldRegB(theInstr);
   UChar frC_addr = ifieldRegC(theInstr);
   UChar opc2     = ifieldOPClo5(theInstr);
   UChar flag_rC  = ifieldBIT0(theInstr);

   IRTemp  frD = newTemp(Ity_F64);
   IRTemp  frA = newTemp(Ity_F64);
   IRTemp  frB = newTemp(Ity_F64);
   IRTemp  frC = newTemp(Ity_F64);
   IRExpr* rm  = get_IR_roundingmode();

   /* By default, we will examine the results of the operation and set
      fpscr[FPRF] accordingly. */
   Bool set_FPRF = True;

   /* By default, if flag_RC is set, we will clear cr1 after the
      operation.  In reality we should set cr1 to indicate the
      exception status of the operation, but since we're not
      simulating exceptions, the exception status will appear to be
      zero.  Hence cr1 should be cleared if this is a . form insn. */
   Bool clear_CR1 = True;

   assign( frA, getFReg(frA_addr));
   assign( frB, getFReg(frB_addr));
   assign( frC, getFReg(frC_addr));

   switch (opc1) {
   case 0x3B:
      switch (opc2) {
      case 0x12: // fdivs (Floating Divide Single, PPC32 p407)
         if (frC_addr != 0)
            return False;
         DIP("fdivs%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frB_addr);
         assign( frD, triop( Iop_DivF64r32, 
                             rm, mkexpr(frA), mkexpr(frB) ));
         break;

      case 0x14: // fsubs (Floating Subtract Single, PPC32 p430)
         if (frC_addr != 0)
            return False;
         DIP("fsubs%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frB_addr);
         assign( frD, triop( Iop_SubF64r32, 
                             rm, mkexpr(frA), mkexpr(frB) ));
         break;

      case 0x15: // fadds (Floating Add Single, PPC32 p401)
         if (frC_addr != 0)
            return False;
         DIP("fadds%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frB_addr);
         assign( frD, triop( Iop_AddF64r32, 
                             rm, mkexpr(frA), mkexpr(frB) ));
         break;

      case 0x16: // fsqrts (Floating SqRt (Single-Precision), PPC32 p428)
         // NOTE: POWERPC OPTIONAL, "General-Purpose Group" (PPC32_FX)
         if (frA_addr != 0 || frC_addr != 0)
            return False;
         DIP("fsqrts%s fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frB_addr);
         // however illogically, on ppc970 this insn behaves identically
         // to fsqrt (double-precision).  So use SqrtF64, not SqrtF64r32.
         assign( frD, binop( Iop_SqrtF64, rm, mkexpr(frB) ));
         break;

      case 0x18: // fres (Floating Reciprocal Estimate Single, PPC32 p421)
         // NOTE: POWERPC OPTIONAL, "Graphics Group" (PPC32_GX)
         if (frA_addr != 0 || frC_addr != 0)
            return False;
         DIP("fres%s fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frB_addr);
         { IRExpr* ieee_one
              = IRExpr_Const(IRConst_F64i(0x3ff0000000000000ULL));
           assign( frD, triop( Iop_DivF64r32, 
                               rm,
                               ieee_one, mkexpr(frB) ));
         }
         break;

      case 0x19: // fmuls (Floating Multiply Single, PPC32 p414)
         if (frB_addr != 0)
            return False;
         DIP("fmuls%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr);
         assign( frD, triop( Iop_MulF64r32,
                             rm, mkexpr(frA), mkexpr(frC) ));
         break;

      case 0x1A: // frsqrtes (Floating Recip SqRt Est Single)
         // NOTE: POWERPC OPTIONAL, "Graphics Group" (PPC32_GX)
         // Undocumented instruction?
         if (frA_addr != 0 || frC_addr != 0)
            return False;
         DIP("frsqrtes%s fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frB_addr);
         assign( frD, unop(Iop_Est5FRSqrt, mkexpr(frB)) );
         break;

      default:
         vex_printf("dis_fp_arith(ppc)(3B: opc2)\n");
         return False;
      }
      break;

   case 0x3F:
      switch (opc2) {           
      case 0x12: // fdiv (Floating Div (Double-Precision), PPC32 p406)
         if (frC_addr != 0)
            return False;
         DIP("fdiv%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frB_addr);
         assign( frD, triop(Iop_DivF64, rm, mkexpr(frA), mkexpr(frB)) );
         break;

      case 0x14: // fsub (Floating Sub (Double-Precision), PPC32 p429)
         if (frC_addr != 0)
            return False;
         DIP("fsub%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frB_addr);
         assign( frD, triop(Iop_SubF64, rm, mkexpr(frA), mkexpr(frB)) );
         break;

      case 0x15: // fadd (Floating Add (Double-Precision), PPC32 p400)
         if (frC_addr != 0)
            return False;
         DIP("fadd%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frB_addr);
         assign( frD, triop(Iop_AddF64, rm, mkexpr(frA), mkexpr(frB)) );
         break;

      case 0x16: // fsqrt (Floating SqRt (Double-Precision), PPC32 p427)
         // NOTE: POWERPC OPTIONAL, "General-Purpose Group" (PPC32_FX)
         if (frA_addr != 0 || frC_addr != 0)
            return False;
         DIP("fsqrt%s fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frB_addr);
         assign( frD, binop(Iop_SqrtF64, rm, mkexpr(frB)) );
         break;

      case 0x17: { // fsel (Floating Select, PPC32 p426)
         // NOTE: POWERPC OPTIONAL, "Graphics Group" (PPC32_GX)
         IRTemp cc    = newTemp(Ity_I32);
         IRTemp cc_b0 = newTemp(Ity_I32);

         DIP("fsel%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);

         // cc: UN == 0x41, LT == 0x01, GT == 0x00, EQ == 0x40
         // => GT|EQ == (cc & 0x1 == 0)
         assign( cc, binop(Iop_CmpF64, mkexpr(frA),
                                       IRExpr_Const(IRConst_F64(0))) );
         assign( cc_b0, binop(Iop_And32, mkexpr(cc), mkU32(1)) );

         // frD = (frA >= 0.0) ? frC : frB
         //     = (cc_b0 == 0) ? frC : frB
         assign( frD,
                 IRExpr_Mux0X(
                    unop(Iop_1Uto8,
                         binop(Iop_CmpEQ32, mkexpr(cc_b0), mkU32(0))),
                    mkexpr(frB),
                    mkexpr(frC) ));

         /* One of the rare ones which don't mess with FPRF */
         set_FPRF = False;
         break;
      }

      case 0x18: // fre (Floating Reciprocal Estimate)
         // NOTE: POWERPC OPTIONAL, "Graphics Group" (PPC32_GX)
         // Note: unclear whether this insn really exists or not
         // ppc970 doesn't have it, but POWER5 does
         if (frA_addr != 0 || frC_addr != 0)
            return False;
         DIP("fre%s fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frB_addr);
         { IRExpr* ieee_one
              = IRExpr_Const(IRConst_F64i(0x3ff0000000000000ULL));
           assign( frD, triop( Iop_DivF64, 
                               rm,
                               ieee_one, mkexpr(frB) ));
         }
         break;

      case 0x19: // fmul (Floating Mult (Double Precision), PPC32 p413)
         if (frB_addr != 0)
            vex_printf("dis_fp_arith(ppc)(instr,fmul)\n");
         DIP("fmul%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr);
         assign( frD, triop(Iop_MulF64, rm, mkexpr(frA), mkexpr(frC)) );
         break;

      case 0x1A: // frsqrte (Floating Recip SqRt Est., PPC32 p424)
         // NOTE: POWERPC OPTIONAL, "Graphics Group" (PPC32_GX)
         if (frA_addr != 0 || frC_addr != 0)
            return False;
         DIP("frsqrte%s fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frB_addr);
         assign( frD, unop(Iop_Est5FRSqrt, mkexpr(frB)) );
         break;

      default:
         vex_printf("dis_fp_arith(ppc)(3F: opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_arith(ppc)(opc1)\n");
      return False;
   }

   putFReg( frD_addr, mkexpr(frD) );

   if (set_FPRF) {
      // XXX XXX XXX FIXME
      // set FPRF from frD
   }

   if (flag_rC && clear_CR1) {
      putCR321( 1, mkU8(0) );
      putCR0( 1, mkU8(0) );
   }

   return True;
}



/*
  Floating Point Mult-Add Instructions
*/
static Bool dis_fp_multadd ( UInt theInstr )
{
   /* A-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar frD_addr = ifieldRegDS(theInstr);
   UChar frA_addr = ifieldRegA(theInstr);
   UChar frB_addr = ifieldRegB(theInstr);
   UChar frC_addr = ifieldRegC(theInstr);
   UChar opc2     = ifieldOPClo5(theInstr);
   UChar flag_rC  = ifieldBIT0(theInstr);

   IRTemp  frD = newTemp(Ity_F64);
   IRTemp  frA = newTemp(Ity_F64);
   IRTemp  frB = newTemp(Ity_F64);
   IRTemp  frC = newTemp(Ity_F64);
   IRTemp  rmt = newTemp(Ity_I32);
   IRExpr* rm;

   /* By default, we will examine the results of the operation and set
      fpscr[FPRF] accordingly. */
   Bool set_FPRF = True;

   /* By default, if flag_RC is set, we will clear cr1 after the
      operation.  In reality we should set cr1 to indicate the
      exception status of the operation, but since we're not
      simulating exceptions, the exception status will appear to be
      zero.  Hence cr1 should be cleared if this is a . form insn. */
   Bool clear_CR1 = True;

   /* Bind the rounding mode expression to a temp; there's no
      point in creating gratuitous CSEs, as we know we'll need 
      to use it twice. */
   assign( rmt, get_IR_roundingmode() );
   rm = mkexpr(rmt);

   assign( frA, getFReg(frA_addr));
   assign( frB, getFReg(frB_addr));
   assign( frC, getFReg(frC_addr));

   /* The rounding in this is all a bit dodgy.  The idea is to only do
      one rounding.  That clearly isn't achieveable without dedicated
      four-input IR primops, although in the single precision case we
      can sort-of simulate it by doing the inner multiply in double
      precision. 

      In the negated cases, the negation happens after rounding. */

   switch (opc1) {
   case 0x3B:
      switch (opc2) {
      case 0x1C: // fmsubs (Floating Mult-Subtr Single, PPC32 p412)
         DIP("fmsubs%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, qop( Iop_MSubF64r32, rm,
                           mkexpr(frA), mkexpr(frC), mkexpr(frB) ));
         break;

      case 0x1D: // fmadds (Floating Mult-Add Single, PPC32 p409)
         DIP("fmadds%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, qop( Iop_MAddF64r32, rm,
                           mkexpr(frA), mkexpr(frC), mkexpr(frB) ));
         break;

      case 0x1E: // fnmsubs (Float Neg Mult-Subtr Single, PPC32 p420)
         DIP("fnmsubs%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, unop( Iop_NegF64,
                      qop( Iop_MSubF64r32, rm,
                           mkexpr(frA), mkexpr(frC), mkexpr(frB) )));
         break;

      case 0x1F: // fnmadds (Floating Negative Multiply-Add Single, PPC32 p418)
         DIP("fnmadds%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, unop( Iop_NegF64,
                      qop( Iop_MAddF64r32, rm,
                           mkexpr(frA), mkexpr(frC), mkexpr(frB) )));
         break;

      default:
         vex_printf("dis_fp_multadd(ppc)(3B: opc2)\n");
         return False;
      }
      break;

   case 0x3F:
      switch (opc2) {           
      case 0x1C: // fmsub (Float Mult-Sub (Dbl Precision), PPC32 p411)
         DIP("fmsub%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, qop( Iop_MSubF64, rm,
                           mkexpr(frA), mkexpr(frC), mkexpr(frB) ));
         break;

      case 0x1D: // fmadd (Float Mult-Add (Dbl Precision), PPC32 p408)
         DIP("fmadd%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, qop( Iop_MAddF64, rm,
                           mkexpr(frA), mkexpr(frC), mkexpr(frB) ));
         break;

      case 0x1E: // fnmsub (Float Neg Mult-Subtr (Dbl Precision), PPC32 p419)
         DIP("fnmsub%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, unop( Iop_NegF64,
                      qop( Iop_MSubF64, rm,
                           mkexpr(frA), mkexpr(frC), mkexpr(frB) )));
         break;

      case 0x1F: // fnmadd (Float Neg Mult-Add (Dbl Precision), PPC32 p417)
         DIP("fnmadd%s fr%u,fr%u,fr%u,fr%u\n", flag_rC ? ".":"",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, unop( Iop_NegF64,
                      qop( Iop_MAddF64, rm,
                           mkexpr(frA), mkexpr(frC), mkexpr(frB) )));
         break;

      default:
         vex_printf("dis_fp_multadd(ppc)(3F: opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_multadd(ppc)(opc1)\n");
      return False;
   }

   putFReg( frD_addr, mkexpr(frD) );

   if (set_FPRF) {
      // XXX XXX XXX FIXME
      // set FPRF from frD
   }

   if (flag_rC && clear_CR1) {
      putCR321( 1, mkU8(0) );
      putCR0( 1, mkU8(0) );
   }

   return True;
}



/*
  Floating Point Compare Instructions
*/
static Bool dis_fp_cmp ( UInt theInstr )
{   
   /* X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar crfD     = toUChar( IFIELD( theInstr, 23, 3 ) );
   UChar b21to22  = toUChar( IFIELD( theInstr, 21, 2 ) );
   UChar frA_addr = ifieldRegA(theInstr);
   UChar frB_addr = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   IRTemp ccIR    = newTemp(Ity_I32);
   IRTemp ccPPC32 = newTemp(Ity_I32);

   IRTemp frA     = newTemp(Ity_F64);
   IRTemp frB     = newTemp(Ity_F64);

   if (opc1 != 0x3F || b21to22 != 0 || b0 != 0) {
      vex_printf("dis_fp_cmp(ppc)(instr)\n");
      return False;
   }

   assign( frA, getFReg(frA_addr));
   assign( frB, getFReg(frB_addr));

   assign( ccIR, binop(Iop_CmpF64, mkexpr(frA), mkexpr(frB)) );
   
   /* Map compare result from IR to PPC32 */
   /*
     FP cmp result | PPC | IR
     --------------------------
     UN            | 0x1 | 0x45
     EQ            | 0x2 | 0x40
     GT            | 0x4 | 0x00
     LT            | 0x8 | 0x01
   */

   // ccPPC32 = Shl(1, (~(ccIR>>5) & 2) 
   //                    | ((ccIR ^ (ccIR>>6)) & 1)
   assign(
      ccPPC32,
      binop(
         Iop_Shl32, 
         mkU32(1),
         unop(
            Iop_32to8, 
            binop(
               Iop_Or32,
               binop(
                  Iop_And32, 
                  unop(
                     Iop_Not32,
                     binop(Iop_Shr32, mkexpr(ccIR), mkU8(5))
                  ),
                  mkU32(2)
               ),
               binop(
                  Iop_And32, 
                  binop(
                     Iop_Xor32, 
                     mkexpr(ccIR),
                     binop(Iop_Shr32, mkexpr(ccIR), mkU8(6))
                  ),
                  mkU32(1)
               )
            )
         )
      )
   );

   putGST_field( PPC_GST_CR, mkexpr(ccPPC32), crfD );

   /* CAB: TODO?: Support writing cc to FPSCR->FPCC ?
      putGST_field( PPC_GST_FPSCR, mkexpr(ccPPC32), 4 );
   */
   // XXX XXX XXX FIXME
   // Also write the result into FPRF (it's not entirely clear how)

   /* Note: Differences between fcmpu and fcmpo are only in exception
      flag settings, which aren't supported anyway. */
   switch (opc2) {
   case 0x000: // fcmpu (Floating Compare Unordered, PPC32 p403)
      DIP("fcmpu crf%d,fr%u,fr%u\n", crfD, frA_addr, frB_addr);
      break;
   case 0x020: // fcmpo (Floating Compare Ordered, PPC32 p402)
      DIP("fcmpo crf%d,fr%u,fr%u\n", crfD, frA_addr, frB_addr);
      break;
   default:
      vex_printf("dis_fp_cmp(ppc)(opc2)\n");
      return False;
   }
   return True;
}



/*
  Floating Point Rounding/Conversion Instructions
*/
static Bool dis_fp_round ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar b16to20  = ifieldRegA(theInstr);
   UChar frD_addr = ifieldRegDS(theInstr);
   UChar frB_addr = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar flag_rC  = ifieldBIT0(theInstr);

   IRTemp  frD     = newTemp(Ity_F64);
   IRTemp  frB     = newTemp(Ity_F64);
   IRTemp  r_tmp32 = newTemp(Ity_I32);
   IRTemp  r_tmp64 = newTemp(Ity_I64);
   IRExpr* rm      = get_IR_roundingmode();

   /* By default, we will examine the results of the operation and set
      fpscr[FPRF] accordingly. */
   Bool set_FPRF = True;

   /* By default, if flag_RC is set, we will clear cr1 after the
      operation.  In reality we should set cr1 to indicate the
      exception status of the operation, but since we're not
      simulating exceptions, the exception status will appear to be
      zero.  Hence cr1 should be cleared if this is a . form insn. */
   Bool clear_CR1 = True;
   
   if (opc1 != 0x3F || b16to20 != 0) {
      vex_printf("dis_fp_round(ppc)(instr)\n");
      return False;
   }

   assign( frB, getFReg(frB_addr));

   switch (opc2) {
   case 0x00C: // frsp (Float Round to Single, PPC32 p423)
      DIP("frsp%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( frD, binop( Iop_RoundF64toF32, rm, mkexpr(frB) ));
      break;
      
   case 0x00E: // fctiw (Float Conv to Int, PPC32 p404)
      DIP("fctiw%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( r_tmp32,
              binop(Iop_F64toI32S, rm, mkexpr(frB)) );
      assign( frD, unop( Iop_ReinterpI64asF64,
                         unop( Iop_32Uto64, mkexpr(r_tmp32))));
      /* FPRF is undefined after fctiw.  Leave unchanged. */
      set_FPRF = False;
      break;
      
   case 0x00F: // fctiwz (Float Conv to Int, Round to Zero, PPC32 p405)
      DIP("fctiwz%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( r_tmp32, 
              binop(Iop_F64toI32S, mkU32(Irrm_ZERO), mkexpr(frB) ));
      assign( frD, unop( Iop_ReinterpI64asF64,
                         unop( Iop_32Uto64, mkexpr(r_tmp32))));
      /* FPRF is undefined after fctiwz.  Leave unchanged. */
      set_FPRF = False;
      break;

   case 0x32E: // fctid (Float Conv to Int DWord, PPC64 p437)
      DIP("fctid%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( r_tmp64,
              binop(Iop_F64toI64S, rm, mkexpr(frB)) );
      assign( frD, unop( Iop_ReinterpI64asF64, mkexpr(r_tmp64)) );
      /* FPRF is undefined after fctid.  Leave unchanged. */
      set_FPRF = False;
      break;

   case 0x32F: // fctidz (Float Conv to Int DWord, Round to Zero, PPC64 p437)
      DIP("fctidz%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( r_tmp64, 
              binop(Iop_F64toI64S, mkU32(Irrm_ZERO), mkexpr(frB)) );
      assign( frD, unop( Iop_ReinterpI64asF64, mkexpr(r_tmp64)) );
      /* FPRF is undefined after fctidz.  Leave unchanged. */
      set_FPRF = False;
      break;

   case 0x34E: // fcfid (Float Conv from Int DWord, PPC64 p434)
      DIP("fcfid%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( r_tmp64, unop( Iop_ReinterpF64asI64, mkexpr(frB)) );
      assign( frD, 
              binop(Iop_I64StoF64, rm, mkexpr(r_tmp64)) );
      break;

   case 0x188: case 0x1A8: case 0x1C8: case 0x1E8: // frin, friz, frip, frim
      switch(opc2) {
      case 0x188: // frin (Floating Round to Integer Nearest)
         DIP("frin%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
         assign( r_tmp64,
                 binop(Iop_F64toI64S, mkU32(Irrm_NEAREST), mkexpr(frB)) );
         break;
      case 0x1A8: // friz (Floating Round to Integer Toward Zero)
         DIP("friz%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
         assign( r_tmp64,
                 binop(Iop_F64toI64S, mkU32(Irrm_ZERO), mkexpr(frB)) );
         break;
      case 0x1C8: // frip (Floating Round to Integer Plus)
         DIP("frip%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
         assign( r_tmp64,
                 binop(Iop_F64toI64S, mkU32(Irrm_PosINF), mkexpr(frB)) );
         break;
      case 0x1E8: // frim (Floating Round to Integer Minus)
         DIP("frim%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
         assign( r_tmp64,
                 binop(Iop_F64toI64S, mkU32(Irrm_NegINF), mkexpr(frB)) );
         break;
      }

      /* don't use the rounded integer if frB is outside -9e18..9e18 */
      /* F64 has only log10(2**52) significant digits anyway */
      /* need to preserve sign of zero */
      /*   frD = (fabs(frB) > 9e18) ? frB :
               (sign(frB)) ? -fabs((double)r_tmp64) : (double)r_tmp64  */
      assign(frD, IRExpr_Mux0X( unop(Iop_32to8,
                                     binop(Iop_CmpF64,
                                           IRExpr_Const(IRConst_F64(9e18)),
                                           unop(Iop_AbsF64, mkexpr(frB)))),
                                IRExpr_Mux0X(unop(Iop_32to8,
                                                  binop(Iop_Shr32,
                                                        unop(Iop_64HIto32,
                                                             unop(Iop_ReinterpF64asI64,
                                                                  mkexpr(frB))), mkU8(31))),
                                             binop(Iop_I64StoF64, mkU32(0), mkexpr(r_tmp64) ),
                                             unop(Iop_NegF64,
                                                  unop( Iop_AbsF64,
                                                        binop(Iop_I64StoF64, mkU32(0),
                                                              mkexpr(r_tmp64)) )) ),
                                mkexpr(frB)));
      break;

   default:
      vex_printf("dis_fp_round(ppc)(opc2)\n");
      return False;
   }

   putFReg( frD_addr, mkexpr(frD) );

   if (set_FPRF) {
      // XXX XXX XXX FIXME
      // set FPRF from frD
   }

   if (flag_rC && clear_CR1) {
      putCR321( 1, mkU8(0) );
      putCR0( 1, mkU8(0) );
   }

   return True;
}

/*
  Floating Point Pair Instructions
*/
static Bool dis_fp_pair ( UInt theInstr )
{
   /* X-Form/DS-Form */
   UChar  opc1         = ifieldOPC(theInstr);
   UChar  frT_hi_addr  = ifieldRegDS(theInstr);
   UChar  frT_lo_addr  = frT_hi_addr + 1;
   UChar  rA_addr      = ifieldRegA(theInstr);
   UChar  rB_addr      = ifieldRegB(theInstr);
   UInt  uimm16        = ifieldUIMM16(theInstr);
   Int    simm16       = extend_s_16to32(uimm16);
   UInt   opc2         = ifieldOPClo10(theInstr);
   IRType ty           = mode64 ? Ity_I64 : Ity_I32;
   IRTemp EA_hi        = newTemp(ty);
   IRTemp EA_lo        = newTemp(ty);
   IRTemp frT_hi       = newTemp(Ity_F64);
   IRTemp frT_lo       = newTemp(Ity_F64);
   UChar b0            = ifieldBIT0(theInstr);
   Bool is_load        = 0;

   if ((frT_hi_addr %2) != 0) {
      vex_printf("dis_fp_pair(ppc) : odd frT register\n");
      return False;
   }

   switch (opc1) {
   case 0x1F: // register offset
      switch(opc2) {
      case 0x317:     // lfdpx (FP Load Double Pair X-form, ISA 2.05  p125)
         DIP("ldpx fr%u,r%u,r%u\n", frT_hi_addr, rA_addr, rB_addr);
         is_load = 1;
         break;
      case 0x397:     // stfdpx (FP STORE Double Pair X-form, ISA 2.05  p125)
         DIP("stdpx fr%u,r%u,r%u\n", frT_hi_addr, rA_addr, rB_addr);
         break;
      default:
         vex_printf("dis_fp_pair(ppc) : X-form wrong opc2\n");
         return False;
      }

      if (b0 != 0) {
         vex_printf("dis_fp_pair(ppc)(0x1F,b0)\n");
         return False;
      }
      assign( EA_hi, ea_rAor0_idxd( rA_addr, rB_addr ) );
      break;
   case 0x39: // lfdp (FP Load Double Pair DS-form, ISA 2.05  p125)
      DIP("lfdp fr%u,%d(r%u)\n", frT_hi_addr, simm16, rA_addr);
      assign( EA_hi, ea_rAor0_simm( rA_addr, simm16  ) );
      is_load = 1;
      break;
   case 0x3d: // stfdp (FP Store Double Pair DS-form, ISA 2.05  p125)
      DIP("stfdp fr%u,%d(r%u)\n", frT_hi_addr, simm16, rA_addr);
      assign( EA_hi, ea_rAor0_simm( rA_addr, simm16  ) );
      break;
   default:   // immediate offset
      vex_printf("dis_fp_pair(ppc)(instr)\n");
      return False;
   }

   if (mode64)
      assign( EA_lo, binop(Iop_Add64, mkexpr(EA_hi), mkU64(8)) );
   else
      assign( EA_lo, binop(Iop_Add32, mkexpr(EA_hi), mkU32(8)) );

   assign( frT_hi, getFReg(frT_hi_addr) );
   assign( frT_lo, getFReg(frT_lo_addr) );

   if (is_load) {
      putFReg( frT_hi_addr, loadBE(Ity_F64, mkexpr(EA_hi)) );
      putFReg( frT_lo_addr, loadBE(Ity_F64, mkexpr(EA_lo)) );
   } else {
      storeBE( mkexpr(EA_hi), mkexpr(frT_hi) );
      storeBE( mkexpr(EA_lo), mkexpr(frT_lo) );
   }

   return True;
}


/*
  Floating Point Move Instructions
*/
static Bool dis_fp_move ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar frD_addr = ifieldRegDS(theInstr);
   UChar frA_addr = ifieldRegA(theInstr);
   UChar frB_addr = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar flag_rC  = ifieldBIT0(theInstr);

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);
   IRTemp itmpB = newTemp(Ity_F64);
   IRTemp frA;
   IRTemp signA;
   IRTemp hiD;

   if (opc1 != 0x3F || (frA_addr != 0 && opc2 != 0x008)) {
      vex_printf("dis_fp_move(ppc)(instr)\n");
      return False;
   }

   assign( frB, getFReg(frB_addr));

   switch (opc2) {
   case 0x008: // fcpsgn (Floating Copy Sign, ISA_V2.05 p126)
      DIP("fcpsgn%s fr%u,fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frA_addr,
          frB_addr);
      signA = newTemp(Ity_I32);
      hiD = newTemp(Ity_I32);
      itmpB = newTemp(Ity_I64);
      frA = newTemp(Ity_F64);
      assign( frA, getFReg(frA_addr) );

      /* get A's sign bit */
      assign(signA, binop(Iop_And32,
                          unop(Iop_64HIto32, unop(Iop_ReinterpF64asI64,
                                                  mkexpr(frA))),
                          mkU32(0x80000000)) );

      assign( itmpB, unop(Iop_ReinterpF64asI64, mkexpr(frB)) );

      /* mask off B's sign bit and or in A's sign bit */
      assign(hiD, binop(Iop_Or32,
                        binop(Iop_And32,
                              unop(Iop_64HIto32,
                                   mkexpr(itmpB)),  /* frB's high 32 bits */
                              mkU32(0x7fffffff)),
                        mkexpr(signA)) );

      /* combine hiD/loB into frD */
      assign( frD, unop(Iop_ReinterpI64asF64,
                        binop(Iop_32HLto64,
                              mkexpr(hiD),
                              unop(Iop_64to32,
                                   mkexpr(itmpB)))) );   /* frB's low 32 bits */
      break;

   case 0x028: // fneg (Floating Negate, PPC32 p416)
      DIP("fneg%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( frD, unop( Iop_NegF64, mkexpr(frB) ));
      break;
      
   case 0x048: // fmr (Floating Move Register, PPC32 p410)
      DIP("fmr%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( frD, mkexpr(frB) );
      break;
      
   case 0x088: // fnabs (Floating Negative Absolute Value, PPC32 p415)
      DIP("fnabs%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( frD, unop( Iop_NegF64, unop( Iop_AbsF64, mkexpr(frB) )));
      break;
      
   case 0x108: // fabs (Floating Absolute Value, PPC32 p399)
      DIP("fabs%s fr%u,fr%u\n", flag_rC ? ".":"", frD_addr, frB_addr);
      assign( frD, unop( Iop_AbsF64, mkexpr(frB) ));
      break;
      
   default:
      vex_printf("dis_fp_move(ppc)(opc2)\n");
      return False;
   }

   putFReg( frD_addr, mkexpr(frD) );

   /* None of these change FPRF.  cr1 is set in the usual way though,
      if flag_rC is set. */

   if (flag_rC) {
      putCR321( 1, mkU8(0) );
      putCR0( 1, mkU8(0) );
   }

   return True;
}



/*
  Floating Point Status/Control Register Instructions
*/
static Bool dis_fp_scr ( UInt theInstr )
{
   /* Many forms - see each switch case */
   UChar opc1    = ifieldOPC(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar flag_rC = ifieldBIT0(theInstr);

   if (opc1 != 0x3F) {
      vex_printf("dis_fp_scr(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x026: { // mtfsb1 (Move to FPSCR Bit 1, PPC32 p479)
      // Bit crbD of the FPSCR is set.
      UChar crbD    = ifieldRegDS(theInstr);
      UInt  b11to20 = IFIELD(theInstr, 11, 10);

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(ppc)(instr,mtfsb1)\n");
         return False;
      }
      DIP("mtfsb1%s crb%d \n", flag_rC ? ".":"", crbD);
      putGST_masked( PPC_GST_FPSCR, mkU32(1<<(31-crbD)), 1<<(31-crbD) );
      break;
   }

   case 0x040: { // mcrfs (Move to Condition Register from FPSCR, PPC32 p465)
      UChar   crfD    = toUChar( IFIELD( theInstr, 23, 3 ) );
      UChar   b21to22 = toUChar( IFIELD( theInstr, 21, 2 ) );
      UChar   crfS    = toUChar( IFIELD( theInstr, 18, 3 ) );
      UChar   b11to17 = toUChar( IFIELD( theInstr, 11, 7 ) );
      IRTemp  tmp     = newTemp(Ity_I32);
      IRExpr* fpscr_all;
      if (b21to22 != 0 || b11to17 != 0 || flag_rC != 0) {
         vex_printf("dis_fp_scr(ppc)(instr,mcrfs)\n");
         return False;
      }
      DIP("mcrfs crf%d,crf%d\n", crfD, crfS);
      vassert(crfD < 8);
      vassert(crfS < 8);
      fpscr_all = getGST_masked( PPC_GST_FPSCR, MASK_FPSCR_RN );
      assign( tmp, binop(Iop_And32,
                         binop(Iop_Shr32,fpscr_all,mkU8(4 * (7-crfS))),
                        mkU32(0xF)) );
      putGST_field( PPC_GST_CR, mkexpr(tmp), crfD );
      break;
   }

   case 0x046: { // mtfsb0 (Move to FPSCR Bit 0, PPC32 p478)
      // Bit crbD of the FPSCR is cleared.
      UChar crbD    = ifieldRegDS(theInstr);
      UInt  b11to20 = IFIELD(theInstr, 11, 10);

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(ppc)(instr,mtfsb0)\n");
         return False;
      }      
      DIP("mtfsb0%s crb%d\n", flag_rC ? ".":"", crbD);
      putGST_masked( PPC_GST_FPSCR, mkU32(0), 1<<(31-crbD) );
      break;
   }

   case 0x086: { // mtfsfi (Move to FPSCR Field Immediate, PPC32 p481)
      UChar crfD    = toUChar( IFIELD( theInstr, 23, 3 ) );
      UChar b16to22 = toUChar( IFIELD( theInstr, 16, 7 ) );
      UChar IMM     = toUChar( IFIELD( theInstr, 12, 4 ) );
      UChar b11     = toUChar( IFIELD( theInstr, 11, 1 ) );

      if (b16to22 != 0 || b11 != 0) {
         vex_printf("dis_fp_scr(ppc)(instr,mtfsfi)\n");
         return False;
      }      
      DIP("mtfsfi%s crf%d,%d\n", flag_rC ? ".":"", crfD, IMM);
      putGST_field( PPC_GST_FPSCR, mkU32(IMM), crfD );
      break;
   }

   case 0x247: { // mffs (Move from FPSCR, PPC32 p468)
      UChar   frD_addr  = ifieldRegDS(theInstr);
      UInt    b11to20   = IFIELD(theInstr, 11, 10);
      IRExpr* fpscr_all = getGST_masked( PPC_GST_FPSCR, MASK_FPSCR_RN );

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(ppc)(instr,mffs)\n");
         return False;
      }
      DIP("mffs%s fr%u\n", flag_rC ? ".":"", frD_addr);
      putFReg( frD_addr,
          unop( Iop_ReinterpI64asF64,
                unop( Iop_32Uto64, fpscr_all )));
      break;
   }

   case 0x2C7: { // mtfsf (Move to FPSCR Fields, PPC32 p480)
      UChar b25      = toUChar( IFIELD(theInstr, 25, 1) );
      UChar FM       = toUChar( IFIELD(theInstr, 17, 8) );
      UChar frB_addr = ifieldRegB(theInstr);
      IRTemp frB   = newTemp(Ity_F64);
      IRTemp rB_32 = newTemp(Ity_I32);
      Int i, mask;

      if (b25 == 1) {
         /* new 64 bit move variant for power 6.  If L field (bit 25) is
          * a one do a full 64 bit move.  Note, the FPSCR is not really
          * properly modeled.  This instruciton only changes the value of
          * the rounding mode.  The HW exception bits do not get set in
          * the simulator.  1/12/09
          */
         DIP("mtfsf%s %d,fr%u (L=1)\n", flag_rC ? ".":"", FM, frB_addr);
         mask = 0xFF;

      } else {
         DIP("mtfsf%s %d,fr%u\n", flag_rC ? ".":"", FM, frB_addr);
         // Build 32bit mask from FM:
         mask = 0;
         for (i=0; i<8; i++) {
            if ((FM & (1<<(7-i))) == 1) {
               mask |= 0xF << (7-i);
            }
         }
      }
      assign( frB, getFReg(frB_addr));
      assign( rB_32, unop( Iop_64to32,
                           unop( Iop_ReinterpF64asI64, mkexpr(frB) )));
      putGST_masked( PPC_GST_FPSCR, mkexpr(rB_32), mask );
      break;
   }

   default:
      vex_printf("dis_fp_scr(ppc)(opc2)\n");
      return False;
   }
   return True;
}



/*------------------------------------------------------------*/
/*--- AltiVec Instruction Translation                      ---*/
/*------------------------------------------------------------*/

/*
  Altivec Cache Control Instructions (Data Streams)
*/
static Bool dis_av_datastream ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar flag_T   = toUChar( IFIELD( theInstr, 25, 1 ) );
   UChar flag_A   = flag_T;
   UChar b23to24  = toUChar( IFIELD( theInstr, 23, 2 ) );
   UChar STRM     = toUChar( IFIELD( theInstr, 21, 2 ) );
   UChar rA_addr  = ifieldRegA(theInstr);
   UChar rB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   if (opc1 != 0x1F || b23to24 != 0 || b0 != 0) {
      vex_printf("dis_av_datastream(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x156: // dst (Data Stream Touch, AV p115)
      DIP("dst%s r%u,r%u,%d\n", flag_T ? "t" : "",
                                rA_addr, rB_addr, STRM);
      break;

   case 0x176: // dstst (Data Stream Touch for Store, AV p117)
      DIP("dstst%s r%u,r%u,%d\n", flag_T ? "t" : "",
                                  rA_addr, rB_addr, STRM);
      break;

   case 0x336: // dss (Data Stream Stop, AV p114)
      if (rA_addr != 0 || rB_addr != 0) {
         vex_printf("dis_av_datastream(ppc)(opc2,dst)\n");
         return False;
      }
      if (flag_A == 0) {
         DIP("dss %d\n", STRM);
      } else {
         DIP("dssall\n");
      }
      break;

   default:
      vex_printf("dis_av_datastream(ppc)(opc2)\n");
      return False;
   }
   return True;
}

/*
  AltiVec Processor Control Instructions
*/
static Bool dis_av_procctl ( UInt theInstr )
{
   /* VX-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar vD_addr = ifieldRegDS(theInstr);
   UChar vA_addr = ifieldRegA(theInstr);
   UChar vB_addr = ifieldRegB(theInstr);
   UInt  opc2    = IFIELD( theInstr, 0, 11 );

   if (opc1 != 0x4) {
      vex_printf("dis_av_procctl(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x604: // mfvscr (Move from VSCR, AV p129)
      if (vA_addr != 0 || vB_addr != 0) {
         vex_printf("dis_av_procctl(ppc)(opc2,dst)\n");
         return False;
      }
      DIP("mfvscr v%d\n", vD_addr);
      putVReg( vD_addr, unop(Iop_32UtoV128, getGST( PPC_GST_VSCR )) ); 
      break;

   case 0x644: { // mtvscr (Move to VSCR, AV p130)
      IRTemp vB = newTemp(Ity_V128);
      if (vD_addr != 0 || vA_addr != 0) {
         vex_printf("dis_av_procctl(ppc)(opc2,dst)\n");
         return False;
      }
      DIP("mtvscr v%d\n", vB_addr);
      assign( vB, getVReg(vB_addr));
      putGST( PPC_GST_VSCR, unop(Iop_V128to32, mkexpr(vB)) ); 
      break;
   }
   default:
      vex_printf("dis_av_procctl(ppc)(opc2)\n");
      return False;
   }
   return True;
}

/*
  AltiVec Load Instructions
*/
static Bool dis_av_load ( VexAbiInfo* vbi, UInt theInstr )
{
   /* X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar rA_addr  = ifieldRegA(theInstr);
   UChar rB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   IRType ty         = mode64 ? Ity_I64 : Ity_I32;
   IRTemp EA         = newTemp(ty);
   IRTemp EA_align16 = newTemp(ty);

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_av_load(ppc)(instr)\n");
      return False;
   }

   assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );
   assign( EA_align16, addr_align( mkexpr(EA), 16 ) );

   switch (opc2) {

   case 0x006: { // lvsl (Load Vector for Shift Left, AV p123)
      IRDirty* d;
      UInt vD_off = vectorGuestRegOffset(vD_addr);
      IRExpr** args = mkIRExprVec_3(
                         mkU32(vD_off), 
                         binop(Iop_And32, mkNarrowTo32(ty, mkexpr(EA)),
                                          mkU32(0xF)),
                         mkU32(0)/*left*/ );
      if (!mode64) {
         d = unsafeIRDirty_0_N (
                        0/*regparms*/, 
                        "ppc32g_dirtyhelper_LVS",
                        fnptr_to_fnentry(vbi, &ppc32g_dirtyhelper_LVS),
                        args );
      } else {
         d = unsafeIRDirty_0_N (
                        0/*regparms*/, 
                        "ppc64g_dirtyhelper_LVS",
                        fnptr_to_fnentry(vbi, &ppc64g_dirtyhelper_LVS),
                        args );
      }
      DIP("lvsl v%d,r%u,r%u\n", vD_addr, rA_addr, rB_addr);
      /* declare guest state effects */
      d->needsBBP = True;
      d->nFxState = 1;
      d->fxState[0].fx     = Ifx_Write;
      d->fxState[0].offset = vD_off;
      d->fxState[0].size   = sizeof(U128);

      /* execute the dirty call, side-effecting guest state */
      stmt( IRStmt_Dirty(d) );
      break;
   }
   case 0x026: { // lvsr (Load Vector for Shift Right, AV p125)
      IRDirty* d;
      UInt vD_off = vectorGuestRegOffset(vD_addr);
      IRExpr** args = mkIRExprVec_3(
                         mkU32(vD_off), 
                         binop(Iop_And32, mkNarrowTo32(ty, mkexpr(EA)),
                                          mkU32(0xF)),
                         mkU32(1)/*right*/ );
      if (!mode64) {
         d = unsafeIRDirty_0_N (
                        0/*regparms*/, 
                        "ppc32g_dirtyhelper_LVS",
                        fnptr_to_fnentry(vbi, &ppc32g_dirtyhelper_LVS),
                        args );
      } else {
         d = unsafeIRDirty_0_N (
                        0/*regparms*/, 
                        "ppc64g_dirtyhelper_LVS",
                        fnptr_to_fnentry(vbi, &ppc64g_dirtyhelper_LVS),
                        args );
      }
      DIP("lvsr v%d,r%u,r%u\n", vD_addr, rA_addr, rB_addr);
      /* declare guest state effects */
      d->needsBBP = True;
      d->nFxState = 1;
      d->fxState[0].fx     = Ifx_Write;
      d->fxState[0].offset = vD_off;
      d->fxState[0].size   = sizeof(U128);

      /* execute the dirty call, side-effecting guest state */
      stmt( IRStmt_Dirty(d) );
      break;
   }
   case 0x007: // lvebx (Load Vector Element Byte Indexed, AV p119)
      DIP("lvebx v%d,r%u,r%u\n", vD_addr, rA_addr, rB_addr);
      /* loads addressed byte into vector[EA[0:3]
         since all other destination bytes are undefined,
         can simply load entire vector from 16-aligned EA */
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_align16)) );
      break;

   case 0x027: // lvehx (Load Vector Element Half Word Indexed, AV p121)
      DIP("lvehx v%d,r%u,r%u\n", vD_addr, rA_addr, rB_addr);
      /* see note for lvebx */
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_align16)) );
      break;

   case 0x047: // lvewx (Load Vector Element Word Indexed, AV p122)
      DIP("lvewx v%d,r%u,r%u\n", vD_addr, rA_addr, rB_addr);
      /* see note for lvebx */
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_align16)) );
      break;

   case 0x067: // lvx (Load Vector Indexed, AV p127)
      DIP("lvx v%d,r%u,r%u\n", vD_addr, rA_addr, rB_addr);
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_align16)) );
      break;

   case 0x167: // lvxl (Load Vector Indexed LRU, AV p128)
      DIP("lvxl v%d,r%u,r%u\n", vD_addr, rA_addr, rB_addr);
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_align16)) );
      break;

   default:
      vex_printf("dis_av_load(ppc)(opc2)\n");
      return False;
   }
   return True;
}


/*
  AltiVec Store Instructions
*/
static Bool dis_av_store ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vS_addr  = ifieldRegDS(theInstr);
   UChar rA_addr  = ifieldRegA(theInstr);
   UChar rB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   IRType ty           = mode64 ? Ity_I64 : Ity_I32;
   IRTemp EA           = newTemp(ty);
   IRTemp addr_aligned = newTemp(ty);
   IRTemp vS           = newTemp(Ity_V128);
   IRTemp eb           = newTemp(Ity_I8);
   IRTemp idx          = newTemp(Ity_I8);

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_av_store(ppc)(instr)\n");
      return False;
   }

   assign( vS, getVReg(vS_addr));
   assign( EA, ea_rAor0_idxd(rA_addr, rB_addr) );

   switch (opc2) {
   case 0x087: { // stvebx (Store Vector Byte Indexed, AV p131)
      DIP("stvebx v%d,r%u,r%u\n", vS_addr, rA_addr, rB_addr);
      assign( eb, binop(Iop_And8, mkU8(0xF),
                        unop(Iop_32to8,
                             mkNarrowTo32(ty, mkexpr(EA)) )) );
      assign( idx, binop(Iop_Shl8,
                         binop(Iop_Sub8, mkU8(15), mkexpr(eb)),
                         mkU8(3)) );
      storeBE( mkexpr(EA),
               unop(Iop_32to8, unop(Iop_V128to32,
                    binop(Iop_ShrV128, mkexpr(vS), mkexpr(idx)))) );
      break;
   }
   case 0x0A7: { // stvehx (Store Vector Half Word Indexed, AV p132)
      DIP("stvehx v%d,r%u,r%u\n", vS_addr, rA_addr, rB_addr);
      assign( addr_aligned, addr_align(mkexpr(EA), 2) );
      assign( eb, binop(Iop_And8, mkU8(0xF),
                        mkNarrowTo8(ty, mkexpr(addr_aligned) )) );
      assign( idx, binop(Iop_Shl8,
                         binop(Iop_Sub8, mkU8(14), mkexpr(eb)),
                         mkU8(3)) );
      storeBE( mkexpr(addr_aligned),
               unop(Iop_32to16, unop(Iop_V128to32,
                    binop(Iop_ShrV128, mkexpr(vS), mkexpr(idx)))) );
      break;
   }
   case 0x0C7: { // stvewx (Store Vector Word Indexed, AV p133)
      DIP("stvewx v%d,r%u,r%u\n", vS_addr, rA_addr, rB_addr);
      assign( addr_aligned, addr_align(mkexpr(EA), 4) );
      assign( eb, binop(Iop_And8, mkU8(0xF),
                        mkNarrowTo8(ty, mkexpr(addr_aligned) )) );
      assign( idx, binop(Iop_Shl8,
                         binop(Iop_Sub8, mkU8(12), mkexpr(eb)),
                         mkU8(3)) );
      storeBE( mkexpr(addr_aligned),
               unop(Iop_V128to32,
                    binop(Iop_ShrV128, mkexpr(vS), mkexpr(idx))) );
      break;
   }

   case 0x0E7: // stvx (Store Vector Indexed, AV p134)
      DIP("stvx v%d,r%u,r%u\n", vS_addr, rA_addr, rB_addr);
      storeBE( addr_align( mkexpr(EA), 16 ), mkexpr(vS) );
      break;

   case 0x1E7: // stvxl (Store Vector Indexed LRU, AV p135)
      DIP("stvxl v%d,r%u,r%u\n", vS_addr, rA_addr, rB_addr);
      storeBE( addr_align( mkexpr(EA), 16 ), mkexpr(vS) );
      break;

   default:
      vex_printf("dis_av_store(ppc)(opc2)\n");
      return False;
   }
   return True;
}

/*
  AltiVec Arithmetic Instructions
*/
static Bool dis_av_arith ( UInt theInstr )
{
   /* VX-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar vA_addr  = ifieldRegA(theInstr);
   UChar vB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = IFIELD( theInstr, 0, 11 );

   IRTemp vA = newTemp(Ity_V128);
   IRTemp vB = newTemp(Ity_V128);
   IRTemp z3 = newTemp(Ity_I64);
   IRTemp z2 = newTemp(Ity_I64);
   IRTemp z1 = newTemp(Ity_I64);
   IRTemp z0 = newTemp(Ity_I64);
   IRTemp aEvn, aOdd;
   IRTemp a15, a14, a13, a12, a11, a10, a9, a8;
   IRTemp a7, a6, a5, a4, a3, a2, a1, a0;
   IRTemp b3, b2, b1, b0;

   aEvn = aOdd = IRTemp_INVALID;
   a15 = a14 = a13 = a12 = a11 = a10 = a9 = a8 = IRTemp_INVALID;
   a7 = a6 = a5 = a4 = a3 = a2 = a1 = a0 = IRTemp_INVALID;
   b3 = b2 = b1 = b0 = IRTemp_INVALID;

   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));

   if (opc1 != 0x4) {
      vex_printf("dis_av_arith(ppc)(opc1 != 0x4)\n");
      return False;
   }

   switch (opc2) {
   /* Add */
   case 0x180: { // vaddcuw (Add Carryout Unsigned Word, AV p136)
      DIP("vaddcuw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      /* unsigned_ov(x+y) = (y >u not(x)) */
      putVReg( vD_addr, binop(Iop_ShrN32x4,
                              binop(Iop_CmpGT32Ux4, mkexpr(vB),
                                    unop(Iop_NotV128, mkexpr(vA))),
                              mkU8(31)) );
      break;
   }
   case 0x000: // vaddubm (Add Unsigned Byte Modulo, AV p141)
      DIP("vaddubm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Add8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x040: // vadduhm (Add Unsigned Half Word Modulo, AV p143)
      DIP("vadduhm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Add16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x080: // vadduwm (Add Unsigned Word Modulo, AV p145)
      DIP("vadduwm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Add32x4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x200: // vaddubs (Add Unsigned Byte Saturate, AV p142)
      DIP("vaddubs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QAdd8Ux16, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT], perhaps via new primop: Iop_SatOfQAdd8Ux16
      break;

   case 0x240: // vadduhs (Add Unsigned Half Word Saturate, AV p144)
      DIP("vadduhs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QAdd16Ux8, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x280: // vadduws (Add Unsigned Word Saturate, AV p146)
      DIP("vadduws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QAdd32Ux4, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x300: // vaddsbs (Add Signed Byte Saturate, AV p138)
      DIP("vaddsbs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QAdd8Sx16, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x340: // vaddshs (Add Signed Half Word Saturate, AV p139)
      DIP("vaddshs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QAdd16Sx8, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x380: // vaddsws (Add Signed Word Saturate, AV p140)
      DIP("vaddsws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QAdd32Sx4, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;


   /* Subtract */
   case 0x580: { // vsubcuw (Subtract Carryout Unsigned Word, AV p260)
      DIP("vsubcuw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      /* unsigned_ov(x-y) = (y >u x) */
      putVReg( vD_addr, binop(Iop_ShrN32x4,
                              unop(Iop_NotV128,
                                   binop(Iop_CmpGT32Ux4, mkexpr(vB),
                                         mkexpr(vA))),
                              mkU8(31)) );
      break;
   }     
   case 0x400: // vsububm (Subtract Unsigned Byte Modulo, AV p265)
      DIP("vsububm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sub8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x440: // vsubuhm (Subtract Unsigned Half Word Modulo, AV p267)
      DIP("vsubuhm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sub16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x480: // vsubuwm (Subtract Unsigned Word Modulo, AV p269)
      DIP("vsubuwm v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sub32x4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x600: // vsububs (Subtract Unsigned Byte Saturate, AV p266)
      DIP("vsububs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QSub8Ux16, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x640: // vsubuhs (Subtract Unsigned HWord Saturate, AV p268)
      DIP("vsubuhs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QSub16Ux8, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x680: // vsubuws (Subtract Unsigned Word Saturate, AV p270)
      DIP("vsubuws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QSub32Ux4, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x700: // vsubsbs (Subtract Signed Byte Saturate, AV p262)
      DIP("vsubsbs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QSub8Sx16, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x740: // vsubshs (Subtract Signed Half Word Saturate, AV p263)
      DIP("vsubshs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QSub16Sx8, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;

   case 0x780: // vsubsws (Subtract Signed Word Saturate, AV p264)
      DIP("vsubsws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QSub32Sx4, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      break;


   /* Maximum */
   case 0x002: // vmaxub (Maximum Unsigned Byte, AV p182)
      DIP("vmaxub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Max8Ux16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x042: // vmaxuh (Maximum Unsigned Half Word, AV p183)
      DIP("vmaxuh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Max16Ux8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x082: // vmaxuw (Maximum Unsigned Word, AV p184)
      DIP("vmaxuw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Max32Ux4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x102: // vmaxsb (Maximum Signed Byte, AV p179)
      DIP("vmaxsb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Max8Sx16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x142: // vmaxsh (Maximum Signed Half Word, AV p180)
      DIP("vmaxsh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Max16Sx8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x182: // vmaxsw (Maximum Signed Word, AV p181)
      DIP("vmaxsw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Max32Sx4, mkexpr(vA), mkexpr(vB)) );
      break;


   /* Minimum */
   case 0x202: // vminub (Minimum Unsigned Byte, AV p191)
      DIP("vminub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Min8Ux16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x242: // vminuh (Minimum Unsigned Half Word, AV p192)
      DIP("vminuh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Min16Ux8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x282: // vminuw (Minimum Unsigned Word, AV p193)
      DIP("vminuw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Min32Ux4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x302: // vminsb (Minimum Signed Byte, AV p188)
      DIP("vminsb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Min8Sx16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x342: // vminsh (Minimum Signed Half Word, AV p189)
      DIP("vminsh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Min16Sx8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x382: // vminsw (Minimum Signed Word, AV p190)
      DIP("vminsw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Min32Sx4, mkexpr(vA), mkexpr(vB)) );
      break;


   /* Average */
   case 0x402: // vavgub (Average Unsigned Byte, AV p152)
      DIP("vavgub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Avg8Ux16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x442: // vavguh (Average Unsigned Half Word, AV p153)
      DIP("vavguh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Avg16Ux8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x482: // vavguw (Average Unsigned Word, AV p154)
      DIP("vavguw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Avg32Ux4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x502: // vavgsb (Average Signed Byte, AV p149)
      DIP("vavgsb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Avg8Sx16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x542: // vavgsh (Average Signed Half Word, AV p150)
      DIP("vavgsh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Avg16Sx8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x582: // vavgsw (Average Signed Word, AV p151)
      DIP("vavgsw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Avg32Sx4, mkexpr(vA), mkexpr(vB)) );
      break;


   /* Multiply */
   case 0x008: // vmuloub (Multiply Odd Unsigned Byte, AV p213)
      DIP("vmuloub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_MullEven8Ux16, mkexpr(vA), mkexpr(vB)));
      break;

   case 0x048: // vmulouh (Multiply Odd Unsigned Half Word, AV p214)
      DIP("vmulouh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_MullEven16Ux8, mkexpr(vA), mkexpr(vB)));
      break;

   case 0x108: // vmulosb (Multiply Odd Signed Byte, AV p211)
      DIP("vmulosb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_MullEven8Sx16, mkexpr(vA), mkexpr(vB)));
      break;

   case 0x148: // vmulosh (Multiply Odd Signed Half Word, AV p212)
      DIP("vmulosh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_MullEven16Sx8, mkexpr(vA), mkexpr(vB)));
      break;

   case 0x208: // vmuleub (Multiply Even Unsigned Byte, AV p209)
      DIP("vmuleub v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, MK_Iop_MullOdd8Ux16( mkexpr(vA), mkexpr(vB) ));
      break;

   case 0x248: // vmuleuh (Multiply Even Unsigned Half Word, AV p210)
      DIP("vmuleuh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, MK_Iop_MullOdd16Ux8( mkexpr(vA), mkexpr(vB) ));
      break;

   case 0x308: // vmulesb (Multiply Even Signed Byte, AV p207)
      DIP("vmulesb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, MK_Iop_MullOdd8Sx16( mkexpr(vA), mkexpr(vB) ));
      break;

   case 0x348: // vmulesh (Multiply Even Signed Half Word, AV p208)
      DIP("vmulesh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, MK_Iop_MullOdd16Sx8( mkexpr(vA), mkexpr(vB) ));
      break;


   /* Sum Across Partial */
   case 0x608: { // vsum4ubs (Sum Partial (1/4) UB Saturate, AV p275)
      IRTemp aEE, aEO, aOE, aOO;
      aEE = aEO = aOE = aOO = IRTemp_INVALID;
      DIP("vsum4ubs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);

      /* vA: V128_8Ux16 -> 4 x V128_32Ux4, sign-extended */
      expand8Ux16( mkexpr(vA), &aEvn, &aOdd ); // (15,13...),(14,12...)
      expand16Ux8( mkexpr(aEvn), &aEE, &aEO ); // (15,11...),(13, 9...)
      expand16Ux8( mkexpr(aOdd), &aOE, &aOO ); // (14,10...),(12, 8...)

      /* break V128 to 4xI32's, zero-extending to I64's */
      breakV128to4x64U( mkexpr(aEE), &a15, &a11, &a7, &a3 );
      breakV128to4x64U( mkexpr(aOE), &a14, &a10, &a6, &a2 );
      breakV128to4x64U( mkexpr(aEO), &a13, &a9,  &a5, &a1 );
      breakV128to4x64U( mkexpr(aOO), &a12, &a8,  &a4, &a0 );
      breakV128to4x64U( mkexpr(vB),  &b3,  &b2,  &b1, &b0 );

      /* add lanes */
      assign( z3, binop(Iop_Add64, mkexpr(b3),
                     binop(Iop_Add64,
                        binop(Iop_Add64, mkexpr(a15), mkexpr(a14)),
                        binop(Iop_Add64, mkexpr(a13), mkexpr(a12)))) );
      assign( z2, binop(Iop_Add64, mkexpr(b2),
                     binop(Iop_Add64,
                         binop(Iop_Add64, mkexpr(a11), mkexpr(a10)),
                         binop(Iop_Add64, mkexpr(a9), mkexpr(a8)))) );
      assign( z1, binop(Iop_Add64, mkexpr(b1),
                     binop(Iop_Add64,
                         binop(Iop_Add64, mkexpr(a7), mkexpr(a6)),
                         binop(Iop_Add64, mkexpr(a5), mkexpr(a4)))) );
      assign( z0, binop(Iop_Add64, mkexpr(b0),
                     binop(Iop_Add64,
                         binop(Iop_Add64, mkexpr(a3), mkexpr(a2)),
                         binop(Iop_Add64, mkexpr(a1), mkexpr(a0)))) );
      
      /* saturate-narrow to 32bit, and combine to V128 */
      putVReg( vD_addr, mkV128from4x64U( mkexpr(z3), mkexpr(z2),
                                         mkexpr(z1), mkexpr(z0)) );
      break;
   }
   case 0x708: { // vsum4sbs (Sum Partial (1/4) SB Saturate, AV p273)
      IRTemp aEE, aEO, aOE, aOO;
      aEE = aEO = aOE = aOO = IRTemp_INVALID;
      DIP("vsum4sbs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);

      /* vA: V128_8Sx16 -> 4 x V128_32Sx4, sign-extended */
      expand8Sx16( mkexpr(vA), &aEvn, &aOdd ); // (15,13...),(14,12...)
      expand16Sx8( mkexpr(aEvn), &aEE, &aEO ); // (15,11...),(13, 9...)
      expand16Sx8( mkexpr(aOdd), &aOE, &aOO ); // (14,10...),(12, 8...)

      /* break V128 to 4xI32's, sign-extending to I64's */
      breakV128to4x64S( mkexpr(aEE), &a15, &a11, &a7, &a3 );
      breakV128to4x64S( mkexpr(aOE), &a14, &a10, &a6, &a2 );
      breakV128to4x64S( mkexpr(aEO), &a13, &a9,  &a5, &a1 );
      breakV128to4x64S( mkexpr(aOO), &a12, &a8,  &a4, &a0 );
      breakV128to4x64S( mkexpr(vB),  &b3,  &b2,  &b1, &b0 );

      /* add lanes */
      assign( z3, binop(Iop_Add64, mkexpr(b3),
                     binop(Iop_Add64,
                        binop(Iop_Add64, mkexpr(a15), mkexpr(a14)),
                        binop(Iop_Add64, mkexpr(a13), mkexpr(a12)))) );
      assign( z2, binop(Iop_Add64, mkexpr(b2),
                     binop(Iop_Add64,
                        binop(Iop_Add64, mkexpr(a11), mkexpr(a10)),
                        binop(Iop_Add64, mkexpr(a9), mkexpr(a8)))) );
      assign( z1, binop(Iop_Add64, mkexpr(b1),
                     binop(Iop_Add64,
                        binop(Iop_Add64, mkexpr(a7), mkexpr(a6)),
                        binop(Iop_Add64, mkexpr(a5), mkexpr(a4)))) );
      assign( z0, binop(Iop_Add64, mkexpr(b0),
                     binop(Iop_Add64,
                        binop(Iop_Add64, mkexpr(a3), mkexpr(a2)),
                        binop(Iop_Add64, mkexpr(a1), mkexpr(a0)))) );
      
      /* saturate-narrow to 32bit, and combine to V128 */
      putVReg( vD_addr, mkV128from4x64S( mkexpr(z3), mkexpr(z2),
                                         mkexpr(z1), mkexpr(z0)) );
      break;
   }
   case 0x648: { // vsum4shs (Sum Partial (1/4) SHW Saturate, AV p274)
      DIP("vsum4shs v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);

      /* vA: V128_16Sx8 -> 2 x V128_32Sx4, sign-extended */
      expand16Sx8( mkexpr(vA), &aEvn, &aOdd ); // (7,5...),(6,4...)

      /* break V128 to 4xI32's, sign-extending to I64's */
      breakV128to4x64S( mkexpr(aEvn), &a7, &a5, &a3, &a1 );
      breakV128to4x64S( mkexpr(aOdd), &a6, &a4, &a2, &a0 );
      breakV128to4x64S( mkexpr(vB),   &b3, &b2, &b1, &b0 );

      /* add lanes */
      assign( z3, binop(Iop_Add64, mkexpr(b3),
                        binop(Iop_Add64, mkexpr(a7), mkexpr(a6))));
      assign( z2, binop(Iop_Add64, mkexpr(b2),
                        binop(Iop_Add64, mkexpr(a5), mkexpr(a4))));
      assign( z1, binop(Iop_Add64, mkexpr(b1),
                        binop(Iop_Add64, mkexpr(a3), mkexpr(a2))));
      assign( z0, binop(Iop_Add64, mkexpr(b0),
                        binop(Iop_Add64, mkexpr(a1), mkexpr(a0))));

      /* saturate-narrow to 32bit, and combine to V128 */
      putVReg( vD_addr, mkV128from4x64S( mkexpr(z3), mkexpr(z2),
                                         mkexpr(z1), mkexpr(z0)) );
      break;
   }
   case 0x688: { // vsum2sws (Sum Partial (1/2) SW Saturate, AV p272)
      DIP("vsum2sws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);

      /* break V128 to 4xI32's, sign-extending to I64's */
      breakV128to4x64S( mkexpr(vA), &a3, &a2, &a1, &a0 );
      breakV128to4x64S( mkexpr(vB), &b3, &b2, &b1, &b0 );

      /* add lanes */
      assign( z2, binop(Iop_Add64, mkexpr(b2),
                        binop(Iop_Add64, mkexpr(a3), mkexpr(a2))) );
      assign( z0, binop(Iop_Add64, mkexpr(b0),
                        binop(Iop_Add64, mkexpr(a1), mkexpr(a0))) );

      /* saturate-narrow to 32bit, and combine to V128 */
      putVReg( vD_addr, mkV128from4x64S( mkU64(0), mkexpr(z2),
                                         mkU64(0), mkexpr(z0)) );
      break;
   }
   case 0x788: { // vsumsws  (Sum SW Saturate, AV p271)
      DIP("vsumsws v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);

      /* break V128 to 4xI32's, sign-extending to I64's */
      breakV128to4x64S( mkexpr(vA), &a3, &a2, &a1, &a0 );
      breakV128to4x64S( mkexpr(vB), &b3, &b2, &b1, &b0 );

      /* add lanes */
      assign( z0, binop(Iop_Add64, mkexpr(b0),
                     binop(Iop_Add64,
                        binop(Iop_Add64, mkexpr(a3), mkexpr(a2)),
                        binop(Iop_Add64, mkexpr(a1), mkexpr(a0)))) );

      /* saturate-narrow to 32bit, and combine to V128 */
      putVReg( vD_addr, mkV128from4x64S( mkU64(0), mkU64(0),
                                         mkU64(0), mkexpr(z0)) );
      break;
   }
   default:
      vex_printf("dis_av_arith(ppc)(opc2=0x%x)\n", opc2);
      return False;
   }
   return True;
}

/*
  AltiVec Logic Instructions
*/
static Bool dis_av_logic ( UInt theInstr )
{
   /* VX-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar vD_addr = ifieldRegDS(theInstr);
   UChar vA_addr = ifieldRegA(theInstr);
   UChar vB_addr = ifieldRegB(theInstr);
   UInt  opc2    = IFIELD( theInstr, 0, 11 );

   IRTemp vA = newTemp(Ity_V128);
   IRTemp vB = newTemp(Ity_V128);
   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));

   if (opc1 != 0x4) {
      vex_printf("dis_av_logic(ppc)(opc1 != 0x4)\n");
      return False;
   }

   switch (opc2) {
   case 0x404: // vand (And, AV p147)
      DIP("vand v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_AndV128, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x444: // vandc (And, AV p148)
      DIP("vandc v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_AndV128, mkexpr(vA),
                              unop(Iop_NotV128, mkexpr(vB))) );
      break;

   case 0x484: // vor (Or, AV p217)
      DIP("vor v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_OrV128, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x4C4: // vxor (Xor, AV p282)
      DIP("vxor v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_XorV128, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x504: // vnor (Nor, AV p216)
      DIP("vnor v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
         unop(Iop_NotV128, binop(Iop_OrV128, mkexpr(vA), mkexpr(vB))) );
      break;

   default:
      vex_printf("dis_av_logic(ppc)(opc2=0x%x)\n", opc2);
      return False;
   }
   return True;
}

/*
  AltiVec Compare Instructions
*/
static Bool dis_av_cmp ( UInt theInstr )
{
   /* VXR-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar vA_addr  = ifieldRegA(theInstr);
   UChar vB_addr  = ifieldRegB(theInstr);
   UChar flag_rC  = ifieldBIT10(theInstr);
   UInt  opc2     = IFIELD( theInstr, 0, 10 );

   IRTemp vA = newTemp(Ity_V128);
   IRTemp vB = newTemp(Ity_V128);
   IRTemp vD = newTemp(Ity_V128);
   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));

   if (opc1 != 0x4) {
      vex_printf("dis_av_cmp(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x006: // vcmpequb (Compare Equal-to Unsigned B, AV p160)
      DIP("vcmpequb%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpEQ8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x046: // vcmpequh (Compare Equal-to Unsigned HW, AV p161)
      DIP("vcmpequh%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpEQ16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x086: // vcmpequw (Compare Equal-to Unsigned W, AV p162)
      DIP("vcmpequw%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpEQ32x4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x206: // vcmpgtub (Compare Greater-than Unsigned B, AV p168)
      DIP("vcmpgtub%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT8Ux16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x246: // vcmpgtuh (Compare Greater-than Unsigned HW, AV p169)
      DIP("vcmpgtuh%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT16Ux8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x286: // vcmpgtuw (Compare Greater-than Unsigned W, AV p170)
      DIP("vcmpgtuw%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                       vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT32Ux4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x306: // vcmpgtsb (Compare Greater-than Signed B, AV p165)
      DIP("vcmpgtsb%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                       vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT8Sx16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x346: // vcmpgtsh (Compare Greater-than Signed HW, AV p166)
      DIP("vcmpgtsh%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT16Sx8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x386: // vcmpgtsw (Compare Greater-than Signed W, AV p167)
      DIP("vcmpgtsw%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT32Sx4, mkexpr(vA), mkexpr(vB)) );
      break;

   default:
      vex_printf("dis_av_cmp(ppc)(opc2)\n");
      return False;
   }

   putVReg( vD_addr, mkexpr(vD) );

   if (flag_rC) {
      set_AV_CR6( mkexpr(vD), True );
   }
   return True;
}

/*
  AltiVec Multiply-Sum Instructions
*/
static Bool dis_av_multarith ( UInt theInstr )
{
   /* VA-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar vA_addr  = ifieldRegA(theInstr);
   UChar vB_addr  = ifieldRegB(theInstr);
   UChar vC_addr  = ifieldRegC(theInstr);
   UChar opc2     = toUChar( IFIELD( theInstr, 0, 6 ) );

   IRTemp vA    = newTemp(Ity_V128);
   IRTemp vB    = newTemp(Ity_V128);
   IRTemp vC    = newTemp(Ity_V128);
   IRTemp zeros = newTemp(Ity_V128);
   IRTemp aLo   = newTemp(Ity_V128);
   IRTemp bLo   = newTemp(Ity_V128);
   IRTemp cLo   = newTemp(Ity_V128);
   IRTemp zLo   = newTemp(Ity_V128);
   IRTemp aHi   = newTemp(Ity_V128);
   IRTemp bHi   = newTemp(Ity_V128);
   IRTemp cHi   = newTemp(Ity_V128);
   IRTemp zHi   = newTemp(Ity_V128);
   IRTemp abEvn = newTemp(Ity_V128);
   IRTemp abOdd = newTemp(Ity_V128);
   IRTemp z3    = newTemp(Ity_I64);
   IRTemp z2    = newTemp(Ity_I64);
   IRTemp z1    = newTemp(Ity_I64);
   IRTemp z0    = newTemp(Ity_I64);
   IRTemp ab7, ab6, ab5, ab4, ab3, ab2, ab1, ab0;
   IRTemp c3, c2, c1, c0;

   ab7 = ab6 = ab5 = ab4 = ab3 = ab2 = ab1 = ab0 = IRTemp_INVALID;
   c3 = c2 = c1 = c0 = IRTemp_INVALID;

   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));
   assign( vC, getVReg(vC_addr));
   assign( zeros, unop(Iop_Dup32x4, mkU32(0)) );

   if (opc1 != 0x4) {
      vex_printf("dis_av_multarith(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   /* Multiply-Add */
   case 0x20: { // vmhaddshs (Mult Hi, Add Signed HW Saturate, AV p185)
      IRTemp cSigns = newTemp(Ity_V128);
      DIP("vmhaddshs v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);
      assign(cSigns, binop(Iop_CmpGT16Sx8, mkexpr(zeros), mkexpr(vC)));
      assign(aLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vA)));
      assign(bLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vB)));
      assign(cLo, binop(Iop_InterleaveLO16x8, mkexpr(cSigns),mkexpr(vC)));
      assign(aHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vA)));
      assign(bHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vB)));
      assign(cHi, binop(Iop_InterleaveHI16x8, mkexpr(cSigns),mkexpr(vC)));

      assign( zLo, binop(Iop_Add32x4, mkexpr(cLo),
                         binop(Iop_SarN32x4,
                               binop(Iop_MullEven16Sx8,
                                     mkexpr(aLo), mkexpr(bLo)),
                               mkU8(15))) );

      assign( zHi, binop(Iop_Add32x4, mkexpr(cHi),
                         binop(Iop_SarN32x4,
                               binop(Iop_MullEven16Sx8,
                                     mkexpr(aHi), mkexpr(bHi)),
                               mkU8(15))) );

      putVReg( vD_addr,
               binop(Iop_QNarrow32Sx4, mkexpr(zHi), mkexpr(zLo)) );
      break;
   }
   case 0x21: { // vmhraddshs (Mult High Round, Add Signed HW Saturate, AV p186)
      IRTemp zKonst = newTemp(Ity_V128);
      IRTemp cSigns = newTemp(Ity_V128);
      DIP("vmhraddshs v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);
      assign(cSigns, binop(Iop_CmpGT16Sx8, mkexpr(zeros), mkexpr(vC)) );
      assign(aLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vA)));
      assign(bLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vB)));
      assign(cLo, binop(Iop_InterleaveLO16x8, mkexpr(cSigns),mkexpr(vC)));
      assign(aHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vA)));
      assign(bHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vB)));
      assign(cHi, binop(Iop_InterleaveHI16x8, mkexpr(cSigns),mkexpr(vC)));

      /* shifting our const avoids store/load version of Dup */
      assign( zKonst, binop(Iop_ShlN32x4, unop(Iop_Dup32x4, mkU32(0x1)),
                            mkU8(14)) );

      assign( zLo, binop(Iop_Add32x4, mkexpr(cLo),
                         binop(Iop_SarN32x4,
                               binop(Iop_Add32x4, mkexpr(zKonst),
                                     binop(Iop_MullEven16Sx8,
                                           mkexpr(aLo), mkexpr(bLo))),
                               mkU8(15))) );

      assign( zHi, binop(Iop_Add32x4, mkexpr(cHi),
                         binop(Iop_SarN32x4,
                               binop(Iop_Add32x4, mkexpr(zKonst),
                                     binop(Iop_MullEven16Sx8,
                                           mkexpr(aHi), mkexpr(bHi))),
                               mkU8(15))) );

      putVReg( vD_addr, binop(Iop_QNarrow32Sx4, mkexpr(zHi), mkexpr(zLo)) );
      break;
   }
   case 0x22: { // vmladduhm (Mult Low, Add Unsigned HW Modulo, AV p194)
      DIP("vmladduhm v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);
      assign(aLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vA)));
      assign(bLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vB)));
      assign(cLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vC)));
      assign(aHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vA)));
      assign(bHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vB)));
      assign(cHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vC)));
      assign(zLo, binop(Iop_Add32x4,
                     binop(Iop_MullEven16Ux8, mkexpr(aLo), mkexpr(bLo)),
                     mkexpr(cLo)) );
      assign(zHi, binop(Iop_Add32x4,
                     binop(Iop_MullEven16Ux8, mkexpr(aHi), mkexpr(bHi)),
                     mkexpr(cHi)));
      putVReg(vD_addr, binop(Iop_Narrow32x4, mkexpr(zHi), mkexpr(zLo)));
      break;
   }


   /* Multiply-Sum */
   case 0x24: { // vmsumubm (Multiply Sum Unsigned B Modulo, AV p204)
      IRTemp abEE, abEO, abOE, abOO;
      abEE = abEO = abOE = abOO = IRTemp_INVALID;
      DIP("vmsumubm v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);

      /* multiply vA,vB (unsigned, widening) */
      assign( abEvn, MK_Iop_MullOdd8Ux16( mkexpr(vA), mkexpr(vB) ));
      assign( abOdd, binop(Iop_MullEven8Ux16, mkexpr(vA), mkexpr(vB)) );
      
      /* evn,odd: V128_16Ux8 -> 2 x V128_32Ux4, zero-extended */
      expand16Ux8( mkexpr(abEvn), &abEE, &abEO );
      expand16Ux8( mkexpr(abOdd), &abOE, &abOO );
      
      putVReg( vD_addr,
         binop(Iop_Add32x4, mkexpr(vC),
               binop(Iop_Add32x4,
                     binop(Iop_Add32x4, mkexpr(abEE), mkexpr(abEO)),
                     binop(Iop_Add32x4, mkexpr(abOE), mkexpr(abOO)))) );
      break;
   }
   case 0x25: { // vmsummbm (Multiply Sum Mixed-Sign B Modulo, AV p201)
      IRTemp aEvn, aOdd, bEvn, bOdd;
      IRTemp abEE = newTemp(Ity_V128);
      IRTemp abEO = newTemp(Ity_V128);
      IRTemp abOE = newTemp(Ity_V128);
      IRTemp abOO = newTemp(Ity_V128);
      aEvn = aOdd = bEvn = bOdd = IRTemp_INVALID;
      DIP("vmsummbm v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);

      /* sign-extend vA, zero-extend vB, for mixed-sign multiply
         (separating out adjacent lanes to different vectors) */
      expand8Sx16( mkexpr(vA), &aEvn, &aOdd );
      expand8Ux16( mkexpr(vB), &bEvn, &bOdd );

      /* multiply vA, vB, again separating adjacent lanes */
      assign( abEE, MK_Iop_MullOdd16Sx8( mkexpr(aEvn), mkexpr(bEvn) ));
      assign( abEO, binop(Iop_MullEven16Sx8, mkexpr(aEvn), mkexpr(bEvn)) );
      assign( abOE, MK_Iop_MullOdd16Sx8( mkexpr(aOdd), mkexpr(bOdd) ));
      assign( abOO, binop(Iop_MullEven16Sx8, mkexpr(aOdd), mkexpr(bOdd)) );

      /* add results together, + vC */
      putVReg( vD_addr,
         binop(Iop_QAdd32Sx4, mkexpr(vC),
               binop(Iop_QAdd32Sx4,
                     binop(Iop_QAdd32Sx4, mkexpr(abEE), mkexpr(abEO)),
                     binop(Iop_QAdd32Sx4, mkexpr(abOE), mkexpr(abOO)))) );
      break;
   }
   case 0x26: { // vmsumuhm (Multiply Sum Unsigned HW Modulo, AV p205)
      DIP("vmsumuhm v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);
      assign( abEvn, MK_Iop_MullOdd16Ux8( mkexpr(vA), mkexpr(vB) ));
      assign( abOdd, binop(Iop_MullEven16Ux8, mkexpr(vA), mkexpr(vB)) );
      putVReg( vD_addr,
         binop(Iop_Add32x4, mkexpr(vC),
               binop(Iop_Add32x4, mkexpr(abEvn), mkexpr(abOdd))) );
      break;
   }
   case 0x27: { // vmsumuhs (Multiply Sum Unsigned HW Saturate, AV p206)
      DIP("vmsumuhs v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);
      /* widening multiply, separating lanes */
      assign( abEvn, MK_Iop_MullOdd16Ux8(mkexpr(vA), mkexpr(vB) ));
      assign( abOdd, binop(Iop_MullEven16Ux8, mkexpr(vA), mkexpr(vB)) );

      /* break V128 to 4xI32's, zero-extending to I64's */
      breakV128to4x64U( mkexpr(abEvn), &ab7, &ab5, &ab3, &ab1 );
      breakV128to4x64U( mkexpr(abOdd), &ab6, &ab4, &ab2, &ab0 );
      breakV128to4x64U( mkexpr(vC),    &c3,  &c2,  &c1,  &c0  );

      /* add lanes */
      assign( z3, binop(Iop_Add64, mkexpr(c3),
                        binop(Iop_Add64, mkexpr(ab7), mkexpr(ab6))));
      assign( z2, binop(Iop_Add64, mkexpr(c2),
                        binop(Iop_Add64, mkexpr(ab5), mkexpr(ab4))));
      assign( z1, binop(Iop_Add64, mkexpr(c1),
                        binop(Iop_Add64, mkexpr(ab3), mkexpr(ab2))));
      assign( z0, binop(Iop_Add64, mkexpr(c0),
                        binop(Iop_Add64, mkexpr(ab1), mkexpr(ab0))));

      /* saturate-narrow to 32bit, and combine to V128 */
      putVReg( vD_addr, mkV128from4x64U( mkexpr(z3), mkexpr(z2),
                                         mkexpr(z1), mkexpr(z0)) );

      break;
   }
   case 0x28: { // vmsumshm (Multiply Sum Signed HW Modulo, AV p202)
      DIP("vmsumshm v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);
      assign( abEvn, MK_Iop_MullOdd16Sx8( mkexpr(vA), mkexpr(vB) ));
      assign( abOdd, binop(Iop_MullEven16Sx8, mkexpr(vA), mkexpr(vB)) );
      putVReg( vD_addr,
         binop(Iop_Add32x4, mkexpr(vC),
               binop(Iop_Add32x4, mkexpr(abOdd), mkexpr(abEvn))) );
      break;
   }
   case 0x29: { // vmsumshs (Multiply Sum Signed HW Saturate, AV p203)
      DIP("vmsumshs v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);
      /* widening multiply, separating lanes */
      assign( abEvn, MK_Iop_MullOdd16Sx8( mkexpr(vA), mkexpr(vB) ));
      assign( abOdd, binop(Iop_MullEven16Sx8, mkexpr(vA), mkexpr(vB)) );

      /* break V128 to 4xI32's, sign-extending to I64's */
      breakV128to4x64S( mkexpr(abEvn), &ab7, &ab5, &ab3, &ab1 );
      breakV128to4x64S( mkexpr(abOdd), &ab6, &ab4, &ab2, &ab0 );
      breakV128to4x64S( mkexpr(vC),    &c3,  &c2,  &c1,  &c0  );

      /* add lanes */
      assign( z3, binop(Iop_Add64, mkexpr(c3),
                        binop(Iop_Add64, mkexpr(ab7), mkexpr(ab6))));
      assign( z2, binop(Iop_Add64, mkexpr(c2),
                        binop(Iop_Add64, mkexpr(ab5), mkexpr(ab4))));
      assign( z1, binop(Iop_Add64, mkexpr(c1),
                        binop(Iop_Add64, mkexpr(ab3), mkexpr(ab2))));
      assign( z0, binop(Iop_Add64, mkexpr(c0),
                        binop(Iop_Add64, mkexpr(ab1), mkexpr(ab0))));

      /* saturate-narrow to 32bit, and combine to V128 */
      putVReg( vD_addr, mkV128from4x64S( mkexpr(z3), mkexpr(z2),
                                         mkexpr(z1), mkexpr(z0)) );
      break;
   }
   default:
      vex_printf("dis_av_multarith(ppc)(opc2)\n");
      return False;
   }
   return True;
}

/*
  AltiVec Shift/Rotate Instructions
*/
static Bool dis_av_shift ( UInt theInstr )
{
   /* VX-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar vD_addr = ifieldRegDS(theInstr);
   UChar vA_addr = ifieldRegA(theInstr);
   UChar vB_addr = ifieldRegB(theInstr);
   UInt  opc2    = IFIELD( theInstr, 0, 11 );

   IRTemp vA = newTemp(Ity_V128);
   IRTemp vB = newTemp(Ity_V128);
   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));

   if (opc1 != 0x4){
      vex_printf("dis_av_shift(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   /* Rotate */
   case 0x004: // vrlb (Rotate Left Integer B, AV p234)
      DIP("vrlb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Rol8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x044: // vrlh (Rotate Left Integer HW, AV p235)
      DIP("vrlh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Rol16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x084: // vrlw (Rotate Left Integer W, AV p236)
      DIP("vrlw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Rol32x4, mkexpr(vA), mkexpr(vB)) );
      break;


   /* Shift Left */
   case 0x104: // vslb (Shift Left Integer B, AV p240)
      DIP("vslb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Shl8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x144: // vslh (Shift Left Integer HW, AV p242)
      DIP("vslh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Shl16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x184: // vslw (Shift Left Integer W, AV p244)
      DIP("vslw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Shl32x4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x1C4: { // vsl (Shift Left, AV p239)
      IRTemp sh = newTemp(Ity_I8);
      DIP("vsl v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      assign( sh, binop(Iop_And8, mkU8(0x7),
                        unop(Iop_32to8,
                             unop(Iop_V128to32, mkexpr(vB)))) );
      putVReg( vD_addr,
               binop(Iop_ShlV128, mkexpr(vA), mkexpr(sh)) );
      break;
   }
   case 0x40C: { // vslo (Shift Left by Octet, AV p243)
      IRTemp sh = newTemp(Ity_I8);
      DIP("vslo v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      assign( sh, binop(Iop_And8, mkU8(0x78),
                        unop(Iop_32to8,
                             unop(Iop_V128to32, mkexpr(vB)))) );
      putVReg( vD_addr,
               binop(Iop_ShlV128, mkexpr(vA), mkexpr(sh)) );
      break;
   }


   /* Shift Right */
   case 0x204: // vsrb (Shift Right B, AV p256)
      DIP("vsrb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Shr8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x244: // vsrh (Shift Right HW, AV p257)
      DIP("vsrh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Shr16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x284: // vsrw (Shift Right W, AV p259)
      DIP("vsrw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Shr32x4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x2C4: { // vsr (Shift Right, AV p251)
      IRTemp sh = newTemp(Ity_I8);
      DIP("vsr v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      assign( sh, binop(Iop_And8, mkU8(0x7),
                        unop(Iop_32to8,
                             unop(Iop_V128to32, mkexpr(vB)))) );
      putVReg( vD_addr,
               binop(Iop_ShrV128, mkexpr(vA), mkexpr(sh)) );
      break;
   }
   case 0x304: // vsrab (Shift Right Alg B, AV p253)
      DIP("vsrab v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sar8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x344: // vsrah (Shift Right Alg HW, AV p254)
      DIP("vsrah v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sar16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x384: // vsraw (Shift Right Alg W, AV p255)
      DIP("vsraw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sar32x4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x44C: { // vsro (Shift Right by Octet, AV p258)
      IRTemp sh = newTemp(Ity_I8);
      DIP("vsro v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      assign( sh, binop(Iop_And8, mkU8(0x78),
                        unop(Iop_32to8,
                             unop(Iop_V128to32, mkexpr(vB)))) );
      putVReg( vD_addr,
               binop(Iop_ShrV128, mkexpr(vA), mkexpr(sh)) );
      break;
   }

   default:
      vex_printf("dis_av_shift(ppc)(opc2)\n");
      return False;
   }
   return True;
}

/*
  AltiVec Permute Instructions
*/
static Bool dis_av_permute ( UInt theInstr )
{
   /* VA-Form, VX-Form */
   UChar opc1      = ifieldOPC(theInstr);
   UChar vD_addr   = ifieldRegDS(theInstr);
   UChar vA_addr   = ifieldRegA(theInstr);
   UChar UIMM_5    = vA_addr;
   UChar vB_addr   = ifieldRegB(theInstr);
   UChar vC_addr   = ifieldRegC(theInstr);
   UChar b10       = ifieldBIT10(theInstr);
   UChar SHB_uimm4 = toUChar( IFIELD( theInstr, 6, 4 ) );
   UInt  opc2      = toUChar( IFIELD( theInstr, 0, 6 ) );

   UChar SIMM_8 = extend_s_5to8(UIMM_5);

   IRTemp vA = newTemp(Ity_V128);
   IRTemp vB = newTemp(Ity_V128);
   IRTemp vC = newTemp(Ity_V128);
   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));
   assign( vC, getVReg(vC_addr));

   if (opc1 != 0x4) {
      vex_printf("dis_av_permute(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x2A: // vsel (Conditional Select, AV p238)
      DIP("vsel v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
      /* vD = (vA & ~vC) | (vB & vC) */
      putVReg( vD_addr, binop(Iop_OrV128,
         binop(Iop_AndV128, mkexpr(vA), unop(Iop_NotV128, mkexpr(vC))),
         binop(Iop_AndV128, mkexpr(vB), mkexpr(vC))) );
      return True;
     
   case 0x2B: { // vperm (Permute, AV p218)
      /* limited to two args for IR, so have to play games... */
      IRTemp a_perm  = newTemp(Ity_V128);
      IRTemp b_perm  = newTemp(Ity_V128);
      IRTemp mask    = newTemp(Ity_V128);
      IRTemp vC_andF = newTemp(Ity_V128);
      DIP("vperm v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vB_addr, vC_addr);
      /* Limit the Perm8x16 steering values to 0 .. 15 as that is what
         IR specifies, and also to hide irrelevant bits from
         memcheck */
      assign( vC_andF,
              binop(Iop_AndV128, mkexpr(vC),
                                 unop(Iop_Dup8x16, mkU8(0xF))) );
      assign( a_perm,
              binop(Iop_Perm8x16, mkexpr(vA), mkexpr(vC_andF)) );
      assign( b_perm,
              binop(Iop_Perm8x16, mkexpr(vB), mkexpr(vC_andF)) );
      // mask[i8] = (vC[i8]_4 == 1) ? 0xFF : 0x0
      assign( mask, binop(Iop_SarN8x16,
                          binop(Iop_ShlN8x16, mkexpr(vC), mkU8(3)),
                          mkU8(7)) );
      // dst = (a & ~mask) | (b & mask)
      putVReg( vD_addr, binop(Iop_OrV128,
                              binop(Iop_AndV128, mkexpr(a_perm),
                                    unop(Iop_NotV128, mkexpr(mask))),
                              binop(Iop_AndV128, mkexpr(b_perm),
                                    mkexpr(mask))) );
      return True;
   }
   case 0x2C: // vsldoi (Shift Left Double by Octet Imm, AV p241)
      if (b10 != 0) {
         vex_printf("dis_av_permute(ppc)(vsldoi)\n");
         return False;
      }
      DIP("vsldoi v%d,v%d,v%d,%d\n",
          vD_addr, vA_addr, vB_addr, SHB_uimm4);
      if (SHB_uimm4 == 0)
         putVReg( vD_addr, mkexpr(vA) );
      else
         putVReg( vD_addr,
            binop(Iop_OrV128,
                  binop(Iop_ShlV128, mkexpr(vA), mkU8(SHB_uimm4*8)),
                  binop(Iop_ShrV128, mkexpr(vB), mkU8((16-SHB_uimm4)*8))) );
      return True;

   default:
     break; // Fall through...
   }

   opc2 = IFIELD( theInstr, 0, 11 );
   switch (opc2) {

   /* Merge */
   case 0x00C: // vmrghb (Merge High B, AV p195)
      DIP("vmrghb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_InterleaveHI8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x04C: // vmrghh (Merge High HW, AV p196)
      DIP("vmrghh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_InterleaveHI16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x08C: // vmrghw (Merge High W, AV p197)
      DIP("vmrghw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_InterleaveHI32x4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x10C: // vmrglb (Merge Low B, AV p198)
      DIP("vmrglb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_InterleaveLO8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x14C: // vmrglh (Merge Low HW, AV p199)
      DIP("vmrglh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_InterleaveLO16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x18C: // vmrglw (Merge Low W, AV p200)
      DIP("vmrglw v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_InterleaveLO32x4, mkexpr(vA), mkexpr(vB)) );
      break;


   /* Splat */
   case 0x20C: { // vspltb (Splat Byte, AV p245)
      /* vD = Dup8x16( vB[UIMM_5] ) */
      UChar sh_uimm = (15 - (UIMM_5 & 15)) * 8;
      DIP("vspltb v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
      putVReg( vD_addr, unop(Iop_Dup8x16,
           unop(Iop_32to8, unop(Iop_V128to32, 
                binop(Iop_ShrV128, mkexpr(vB), mkU8(sh_uimm))))) );
      break;
   }
   case 0x24C: { // vsplth (Splat Half Word, AV p246)
      UChar sh_uimm = (7 - (UIMM_5 & 7)) * 16;
      DIP("vsplth v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
      putVReg( vD_addr, unop(Iop_Dup16x8,
           unop(Iop_32to16, unop(Iop_V128to32, 
                binop(Iop_ShrV128, mkexpr(vB), mkU8(sh_uimm))))) );
      break;
   }
   case 0x28C: { // vspltw (Splat Word, AV p250)
      /* vD = Dup32x4( vB[UIMM_5] ) */
      UChar sh_uimm = (3 - (UIMM_5 & 3)) * 32;
      DIP("vspltw v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
      putVReg( vD_addr, unop(Iop_Dup32x4,
         unop(Iop_V128to32,
              binop(Iop_ShrV128, mkexpr(vB), mkU8(sh_uimm)))) );
      break;
   }
   case 0x30C: // vspltisb (Splat Immediate Signed B, AV p247)
      DIP("vspltisb v%d,%d\n", vD_addr, (Char)SIMM_8);
      putVReg( vD_addr, unop(Iop_Dup8x16, mkU8(SIMM_8)) );
      break;

   case 0x34C: // vspltish (Splat Immediate Signed HW, AV p248)
      DIP("vspltish v%d,%d\n", vD_addr, (Char)SIMM_8);
      putVReg( vD_addr,
               unop(Iop_Dup16x8, mkU16(extend_s_8to32(SIMM_8))) );
      break;

   case 0x38C: // vspltisw (Splat Immediate Signed W, AV p249)
      DIP("vspltisw v%d,%d\n", vD_addr, (Char)SIMM_8);
      putVReg( vD_addr,
               unop(Iop_Dup32x4, mkU32(extend_s_8to32(SIMM_8))) );
      break;

   default:
      vex_printf("dis_av_permute(ppc)(opc2)\n");
      return False;
   }
   return True;
}

/*
  AltiVec Pack/Unpack Instructions
*/
static Bool dis_av_pack ( UInt theInstr )
{
   /* VX-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar vA_addr  = ifieldRegA(theInstr);
   UChar vB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = IFIELD( theInstr, 0, 11 );

   IRTemp signs = IRTemp_INVALID;
   IRTemp zeros = IRTemp_INVALID;
   IRTemp vA    = newTemp(Ity_V128);
   IRTemp vB    = newTemp(Ity_V128);
   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));

   if (opc1 != 0x4) {
      vex_printf("dis_av_pack(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   /* Packing */
   case 0x00E: // vpkuhum (Pack Unsigned HW Unsigned Modulo, AV p224)
      DIP("vpkuhum v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Narrow16x8, mkexpr(vA), mkexpr(vB)) );
      return True;

   case 0x04E: // vpkuwum (Pack Unsigned W Unsigned Modulo, AV p226)
      DIP("vpkuwum v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Narrow32x4, mkexpr(vA), mkexpr(vB)) );
      return True;

   case 0x08E: // vpkuhus (Pack Unsigned HW Unsigned Saturate, AV p225)
      DIP("vpkuhus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_QNarrow16Ux8, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      return True;

   case 0x0CE: // vpkuwus (Pack Unsigned W Unsigned Saturate, AV p227)
      DIP("vpkuwus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_QNarrow32Ux4, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      return True;

   case 0x10E: { // vpkshus (Pack Signed HW Unsigned Saturate, AV p221)
      // This insn does a signed->unsigned saturating conversion.
      // Conversion done here, then uses unsigned->unsigned vpk insn:
      //  => UnsignedSaturatingNarrow( x & ~ (x >>s 15) )
      IRTemp vA_tmp = newTemp(Ity_V128);
      IRTemp vB_tmp = newTemp(Ity_V128);
      DIP("vpkshus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      assign( vA_tmp, binop(Iop_AndV128, mkexpr(vA),
                            unop(Iop_NotV128,
                                 binop(Iop_SarN16x8,
                                       mkexpr(vA), mkU8(15)))) );
      assign( vB_tmp, binop(Iop_AndV128, mkexpr(vB),
                            unop(Iop_NotV128,
                                 binop(Iop_SarN16x8,
                                       mkexpr(vB), mkU8(15)))) );
      putVReg( vD_addr, binop(Iop_QNarrow16Ux8,
                              mkexpr(vA_tmp), mkexpr(vB_tmp)) );
      // TODO: set VSCR[SAT]
      return True;
   }
   case 0x14E: { // vpkswus (Pack Signed W Unsigned Saturate, AV p223)
      // This insn does a signed->unsigned saturating conversion.
      // Conversion done here, then uses unsigned->unsigned vpk insn:
      //  => UnsignedSaturatingNarrow( x & ~ (x >>s 31) )
      IRTemp vA_tmp = newTemp(Ity_V128);
      IRTemp vB_tmp = newTemp(Ity_V128);
      DIP("vpkswus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      assign( vA_tmp, binop(Iop_AndV128, mkexpr(vA),
                            unop(Iop_NotV128,
                                 binop(Iop_SarN32x4,
                                       mkexpr(vA), mkU8(31)))) );
      assign( vB_tmp, binop(Iop_AndV128, mkexpr(vB),
                            unop(Iop_NotV128,
                                 binop(Iop_SarN32x4,
                                       mkexpr(vB), mkU8(31)))) );
      putVReg( vD_addr, binop(Iop_QNarrow32Ux4,
                              mkexpr(vA_tmp), mkexpr(vB_tmp)) );
      // TODO: set VSCR[SAT]
      return True;
   }
   case 0x18E: // vpkshss (Pack Signed HW Signed Saturate, AV p220)
      DIP("vpkshss v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_QNarrow16Sx8, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      return True;

   case 0x1CE: // vpkswss (Pack Signed W Signed Saturate, AV p222)
      DIP("vpkswss v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_QNarrow32Sx4, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      return True;

   case 0x30E: { // vpkpx (Pack Pixel, AV p219)
      /* CAB: Worth a new primop? */
      /* Using shifts to compact pixel elements, then packing them */
      IRTemp a1 = newTemp(Ity_V128);
      IRTemp a2 = newTemp(Ity_V128);
      IRTemp a3 = newTemp(Ity_V128);
      IRTemp a_tmp = newTemp(Ity_V128);
      IRTemp b1 = newTemp(Ity_V128);
      IRTemp b2 = newTemp(Ity_V128);
      IRTemp b3 = newTemp(Ity_V128);
      IRTemp b_tmp = newTemp(Ity_V128);
      DIP("vpkpx v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      assign( a1, binop(Iop_ShlN16x8,
                        binop(Iop_ShrN32x4, mkexpr(vA), mkU8(19)),
                        mkU8(10)) );
      assign( a2, binop(Iop_ShlN16x8, 
                        binop(Iop_ShrN16x8, mkexpr(vA), mkU8(11)),
                        mkU8(5)) );
      assign( a3,  binop(Iop_ShrN16x8, 
                         binop(Iop_ShlN16x8, mkexpr(vA), mkU8(8)),
                         mkU8(11)) );
      assign( a_tmp, binop(Iop_OrV128, mkexpr(a1),
                           binop(Iop_OrV128, mkexpr(a2), mkexpr(a3))) );

      assign( b1, binop(Iop_ShlN16x8,
                        binop(Iop_ShrN32x4, mkexpr(vB), mkU8(19)),
                        mkU8(10)) );
      assign( b2, binop(Iop_ShlN16x8, 
                        binop(Iop_ShrN16x8, mkexpr(vB), mkU8(11)),
                        mkU8(5)) );
      assign( b3,  binop(Iop_ShrN16x8, 
                         binop(Iop_ShlN16x8, mkexpr(vB), mkU8(8)),
                         mkU8(11)) );
      assign( b_tmp, binop(Iop_OrV128, mkexpr(b1),
                           binop(Iop_OrV128, mkexpr(b2), mkexpr(b3))) );

      putVReg( vD_addr, binop(Iop_Narrow32x4,
                              mkexpr(a_tmp), mkexpr(b_tmp)) );
      return True;
   }

   default:
      break; // Fall through...
   }


   if (vA_addr != 0) {
      vex_printf("dis_av_pack(ppc)(vA_addr)\n");
      return False;
   }

   signs = newTemp(Ity_V128);
   zeros = newTemp(Ity_V128);
   assign( zeros, unop(Iop_Dup32x4, mkU32(0)) );

   switch (opc2) {
   /* Unpacking */
   case 0x20E: { // vupkhsb (Unpack High Signed B, AV p277)
      DIP("vupkhsb v%d,v%d\n", vD_addr, vB_addr);
      assign( signs, binop(Iop_CmpGT8Sx16, mkexpr(zeros), mkexpr(vB)) );
      putVReg( vD_addr,
               binop(Iop_InterleaveHI8x16, mkexpr(signs), mkexpr(vB)) );
      break;
   }
   case 0x24E: { // vupkhsh (Unpack High Signed HW, AV p278)
      DIP("vupkhsh v%d,v%d\n", vD_addr, vB_addr);
      assign( signs, binop(Iop_CmpGT16Sx8, mkexpr(zeros), mkexpr(vB)) );
      putVReg( vD_addr,
               binop(Iop_InterleaveHI16x8, mkexpr(signs), mkexpr(vB)) );
      break;
   }
   case 0x28E: { // vupklsb (Unpack Low Signed B, AV p280)
      DIP("vupklsb v%d,v%d\n", vD_addr, vB_addr);
      assign( signs, binop(Iop_CmpGT8Sx16, mkexpr(zeros), mkexpr(vB)) );
      putVReg( vD_addr,
               binop(Iop_InterleaveLO8x16, mkexpr(signs), mkexpr(vB)) );
      break;
   }
   case 0x2CE: { // vupklsh (Unpack Low Signed HW, AV p281)
      DIP("vupklsh v%d,v%d\n", vD_addr, vB_addr);
      assign( signs, binop(Iop_CmpGT16Sx8, mkexpr(zeros), mkexpr(vB)) );
      putVReg( vD_addr,
               binop(Iop_InterleaveLO16x8, mkexpr(signs), mkexpr(vB)) );
      break;
   }
   case 0x34E: { // vupkhpx (Unpack High Pixel16, AV p276)
      /* CAB: Worth a new primop? */
      /* Using shifts to isolate pixel elements, then expanding them */
      IRTemp z0  = newTemp(Ity_V128);
      IRTemp z1  = newTemp(Ity_V128);
      IRTemp z01 = newTemp(Ity_V128);
      IRTemp z2  = newTemp(Ity_V128);
      IRTemp z3  = newTemp(Ity_V128);
      IRTemp z23 = newTemp(Ity_V128);
      DIP("vupkhpx v%d,v%d\n", vD_addr, vB_addr);
      assign( z0,  binop(Iop_ShlN16x8,
                         binop(Iop_SarN16x8, mkexpr(vB), mkU8(15)),
                         mkU8(8)) );
      assign( z1,  binop(Iop_ShrN16x8, 
                         binop(Iop_ShlN16x8, mkexpr(vB), mkU8(1)),
                         mkU8(11)) );
      assign( z01, binop(Iop_InterleaveHI16x8, mkexpr(zeros),
                         binop(Iop_OrV128, mkexpr(z0), mkexpr(z1))) );
      assign( z2,  binop(Iop_ShrN16x8,
                         binop(Iop_ShlN16x8, 
                               binop(Iop_ShrN16x8, mkexpr(vB), mkU8(5)),
                               mkU8(11)),
                         mkU8(3)) );
      assign( z3,  binop(Iop_ShrN16x8, 
                         binop(Iop_ShlN16x8, mkexpr(vB), mkU8(11)),
                         mkU8(11)) );
      assign( z23, binop(Iop_InterleaveHI16x8, mkexpr(zeros),
                         binop(Iop_OrV128, mkexpr(z2), mkexpr(z3))) );
      putVReg( vD_addr,
               binop(Iop_OrV128,
                     binop(Iop_ShlN32x4, mkexpr(z01), mkU8(16)),
                     mkexpr(z23)) );
      break;
   }
   case 0x3CE: { // vupklpx (Unpack Low Pixel16, AV p279)
      /* identical to vupkhpx, except interleaving LO */
      IRTemp z0  = newTemp(Ity_V128);
      IRTemp z1  = newTemp(Ity_V128);
      IRTemp z01 = newTemp(Ity_V128);
      IRTemp z2  = newTemp(Ity_V128);
      IRTemp z3  = newTemp(Ity_V128);
      IRTemp z23 = newTemp(Ity_V128);
      DIP("vupklpx v%d,v%d\n", vD_addr, vB_addr);
      assign( z0,  binop(Iop_ShlN16x8,
                         binop(Iop_SarN16x8, mkexpr(vB), mkU8(15)),
                         mkU8(8)) );
      assign( z1,  binop(Iop_ShrN16x8, 
                         binop(Iop_ShlN16x8, mkexpr(vB), mkU8(1)),
                         mkU8(11)) );
      assign( z01, binop(Iop_InterleaveLO16x8, mkexpr(zeros),
                         binop(Iop_OrV128, mkexpr(z0), mkexpr(z1))) );
      assign( z2,  binop(Iop_ShrN16x8,
                         binop(Iop_ShlN16x8, 
                               binop(Iop_ShrN16x8, mkexpr(vB), mkU8(5)),
                               mkU8(11)),
                         mkU8(3)) );
      assign( z3,  binop(Iop_ShrN16x8, 
                         binop(Iop_ShlN16x8, mkexpr(vB), mkU8(11)),
                         mkU8(11)) );
      assign( z23, binop(Iop_InterleaveLO16x8, mkexpr(zeros),
                         binop(Iop_OrV128, mkexpr(z2), mkexpr(z3))) );
      putVReg( vD_addr,
               binop(Iop_OrV128,
                     binop(Iop_ShlN32x4, mkexpr(z01), mkU8(16)),
                     mkexpr(z23)) );
      break;
   }
   default:
      vex_printf("dis_av_pack(ppc)(opc2)\n");
      return False;
   }
   return True;
}


/*
  AltiVec Floating Point Arithmetic Instructions
*/
static Bool dis_av_fp_arith ( UInt theInstr )
{
   /* VA-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar vA_addr  = ifieldRegA(theInstr);
   UChar vB_addr  = ifieldRegB(theInstr);
   UChar vC_addr  = ifieldRegC(theInstr);
   UInt  opc2=0;

   IRTemp vA = newTemp(Ity_V128);
   IRTemp vB = newTemp(Ity_V128);
   IRTemp vC = newTemp(Ity_V128);
   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));
   assign( vC, getVReg(vC_addr));

   if (opc1 != 0x4) {
      vex_printf("dis_av_fp_arith(ppc)(instr)\n");
      return False;
   }

   opc2 = IFIELD( theInstr, 0, 6 );
   switch (opc2) {
   case 0x2E: // vmaddfp (Multiply Add FP, AV p177)
      DIP("vmaddfp v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vC_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_Add32Fx4, mkexpr(vB),
                     binop(Iop_Mul32Fx4, mkexpr(vA), mkexpr(vC))) );
      return True;

   case 0x2F: { // vnmsubfp (Negative Multiply-Subtract FP, AV p215)
      DIP("vnmsubfp v%d,v%d,v%d,v%d\n",
          vD_addr, vA_addr, vC_addr, vB_addr);
      putVReg( vD_addr,
               binop(Iop_Sub32Fx4,
                     mkexpr(vB),
                     binop(Iop_Mul32Fx4, mkexpr(vA), mkexpr(vC))) );
      return True;
   }

   default:
     break; // Fall through...
   }

   opc2 = IFIELD( theInstr, 0, 11 );
   switch (opc2) {
   case 0x00A: // vaddfp (Add FP, AV p137)
      DIP("vaddfp v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Add32Fx4, mkexpr(vA), mkexpr(vB)) );
      return True;

  case 0x04A: // vsubfp (Subtract FP, AV p261)
      DIP("vsubfp v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sub32Fx4, mkexpr(vA), mkexpr(vB)) );
      return True;

   case 0x40A: // vmaxfp (Maximum FP, AV p178)
      DIP("vmaxfp v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Max32Fx4, mkexpr(vA), mkexpr(vB)) );
      return True;

   case 0x44A: // vminfp (Minimum FP, AV p187)
      DIP("vminfp v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Min32Fx4, mkexpr(vA), mkexpr(vB)) );
      return True;

   default:
      break; // Fall through...
   }


   if (vA_addr != 0) {
      vex_printf("dis_av_fp_arith(ppc)(vA_addr)\n");
      return False;
   }

   switch (opc2) {
   case 0x10A: // vrefp (Reciprocal Esimate FP, AV p228)
      DIP("vrefp v%d,v%d\n", vD_addr, vB_addr);
      putVReg( vD_addr, unop(Iop_Recip32Fx4, mkexpr(vB)) );
      return True;

   case 0x14A: // vrsqrtefp (Reciprocal Sqrt Estimate FP, AV p237)
      DIP("vrsqrtefp v%d,v%d\n", vD_addr, vB_addr);
      putVReg( vD_addr, unop(Iop_RSqrt32Fx4, mkexpr(vB)) );
      return True;

   case 0x18A: // vexptefp (2 Raised to the Exp Est FP, AV p173)
      DIP("vexptefp v%d,v%d\n", vD_addr, vB_addr);
      DIP(" => not implemented\n");
      return False;

   case 0x1CA: // vlogefp (Log2 Estimate FP, AV p175)
      DIP("vlogefp v%d,v%d\n", vD_addr, vB_addr);
      DIP(" => not implemented\n");
      return False;

   default:
      vex_printf("dis_av_fp_arith(ppc)(opc2=0x%x)\n",opc2);
      return False;
   }
   return True;
}

/*
  AltiVec Floating Point Compare Instructions
*/
static Bool dis_av_fp_cmp ( UInt theInstr )
{
   /* VXR-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar vA_addr  = ifieldRegA(theInstr);
   UChar vB_addr  = ifieldRegB(theInstr);
   UChar flag_rC  = ifieldBIT10(theInstr);
   UInt  opc2     = IFIELD( theInstr, 0, 10 );

   Bool cmp_bounds = False;

   IRTemp vA = newTemp(Ity_V128);
   IRTemp vB = newTemp(Ity_V128);
   IRTemp vD = newTemp(Ity_V128);
   assign( vA, getVReg(vA_addr));
   assign( vB, getVReg(vB_addr));

   if (opc1 != 0x4) {
      vex_printf("dis_av_fp_cmp(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x0C6: // vcmpeqfp (Compare Equal-to FP, AV p159)
      DIP("vcmpeqfp%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpEQ32Fx4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x1C6: // vcmpgefp (Compare Greater-than-or-Equal-to, AV p163)
      DIP("vcmpgefp%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGE32Fx4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x2C6: // vcmpgtfp (Compare Greater-than FP, AV p164)
      DIP("vcmpgtfp%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                      vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT32Fx4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x3C6: { // vcmpbfp (Compare Bounds FP, AV p157)
      IRTemp gt      = newTemp(Ity_V128);
      IRTemp lt      = newTemp(Ity_V128);
      IRTemp zeros   = newTemp(Ity_V128);
      DIP("vcmpbfp%s v%d,v%d,v%d\n", (flag_rC ? ".":""),
                                     vD_addr, vA_addr, vB_addr);
      cmp_bounds = True;
      assign( zeros,   unop(Iop_Dup32x4, mkU32(0)) );

      /* Note: making use of fact that the ppc backend for compare insns
         return zero'd lanes if either of the corresponding arg lanes is
         a nan.

         Perhaps better to have an irop Iop_isNan32Fx4, but then we'd
         need this for the other compares too (vcmpeqfp etc)...
         Better still, tighten down the spec for compare irops.
       */
      assign( gt, unop(Iop_NotV128,
                       binop(Iop_CmpLE32Fx4, mkexpr(vA), mkexpr(vB))) );
      assign( lt, unop(Iop_NotV128,
                       binop(Iop_CmpGE32Fx4, mkexpr(vA),
                             binop(Iop_Sub32Fx4, mkexpr(zeros),
                                                 mkexpr(vB)))) );

      // finally, just shift gt,lt to correct position
      assign( vD, binop(Iop_ShlN32x4,
                        binop(Iop_OrV128,
                              binop(Iop_AndV128, mkexpr(gt),
                                    unop(Iop_Dup32x4, mkU32(0x2))),
                              binop(Iop_AndV128, mkexpr(lt),
                                    unop(Iop_Dup32x4, mkU32(0x1)))),
                        mkU8(30)) );
      break;
   }

   default:
      vex_printf("dis_av_fp_cmp(ppc)(opc2)\n");
      return False;
   }

   putVReg( vD_addr, mkexpr(vD) );

   if (flag_rC) {
      set_AV_CR6( mkexpr(vD), !cmp_bounds );
   }
   return True;
}

/*
  AltiVec Floating Point Convert/Round Instructions
*/
static Bool dis_av_fp_convert ( UInt theInstr )
{
   /* VX-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar UIMM_5   = ifieldRegA(theInstr);
   UChar vB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = IFIELD( theInstr, 0, 11 );

   IRTemp vB        = newTemp(Ity_V128);
   IRTemp vScale    = newTemp(Ity_V128);
   IRTemp vInvScale = newTemp(Ity_V128);

   float scale, inv_scale;

   assign( vB, getVReg(vB_addr));

   /* scale = 2^UIMM, cast to float, reinterpreted as uint */
   scale = (float)( (unsigned int) 1<<UIMM_5 );
   assign( vScale, unop(Iop_Dup32x4, mkU32( float_to_bits(scale) )) );
   inv_scale = 1/scale;
   assign( vInvScale,
           unop(Iop_Dup32x4, mkU32( float_to_bits(inv_scale) )) );

   if (opc1 != 0x4) {
      vex_printf("dis_av_fp_convert(ppc)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x30A: // vcfux (Convert from Unsigned Fixed-Point W, AV p156)
      DIP("vcfux v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
      putVReg( vD_addr, binop(Iop_Mul32Fx4,
                              unop(Iop_I32UtoFx4, mkexpr(vB)),
                              mkexpr(vInvScale)) );
      return True;

   case 0x34A: // vcfsx (Convert from Signed Fixed-Point W, AV p155)
      DIP("vcfsx v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);

      putVReg( vD_addr, binop(Iop_Mul32Fx4,
                              unop(Iop_I32StoFx4, mkexpr(vB)),
                              mkexpr(vInvScale)) );
      return True;

   case 0x38A: // vctuxs (Convert to Unsigned Fixed-Point W Saturate, AV p172)
      DIP("vctuxs v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
      putVReg( vD_addr,
               unop(Iop_QFtoI32Ux4_RZ, 
                    binop(Iop_Mul32Fx4, mkexpr(vB), mkexpr(vScale))) );
      return True;

   case 0x3CA: // vctsxs (Convert to Signed Fixed-Point W Saturate, AV p171)
      DIP("vctsxs v%d,v%d,%d\n", vD_addr, vB_addr, UIMM_5);
      putVReg( vD_addr, 
               unop(Iop_QFtoI32Sx4_RZ, 
                     binop(Iop_Mul32Fx4, mkexpr(vB), mkexpr(vScale))) );
      return True;

   default:
     break;    // Fall through...
   }

   if (UIMM_5 != 0) {
      vex_printf("dis_av_fp_convert(ppc)(UIMM_5)\n");
      return False;
   }

   switch (opc2) {
   case 0x20A: // vrfin (Round to FP Integer Nearest, AV p231)
      DIP("vrfin v%d,v%d\n", vD_addr, vB_addr);
      putVReg( vD_addr, unop(Iop_RoundF32x4_RN, mkexpr(vB)) );
      break;

   case 0x24A: // vrfiz (Round to FP Integer toward zero, AV p233)
      DIP("vrfiz v%d,v%d\n", vD_addr, vB_addr);
      putVReg( vD_addr, unop(Iop_RoundF32x4_RZ, mkexpr(vB)) );
      break;

   case 0x28A: // vrfip (Round to FP Integer toward +inf, AV p232)
      DIP("vrfip v%d,v%d\n", vD_addr, vB_addr);
      putVReg( vD_addr, unop(Iop_RoundF32x4_RP, mkexpr(vB)) );
      break;

   case 0x2CA: // vrfim (Round to FP Integer toward -inf, AV p230)
      DIP("vrfim v%d,v%d\n", vD_addr, vB_addr);
      putVReg( vD_addr, unop(Iop_RoundF32x4_RM, mkexpr(vB)) );
      break;

   default:
      vex_printf("dis_av_fp_convert(ppc)(opc2)\n");
      return False;
   }
   return True;
}





/*------------------------------------------------------------*/
/*--- Disassemble a single instruction                     ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

static   
DisResult disInstr_PPC_WRK ( 
             Bool         put_IP,
             Bool         (*resteerOkFn) ( /*opaque*/void*, Addr64 ),
             Bool         resteerCisOk,
             void*        callback_opaque,
             Long         delta64,
             VexArchInfo* archinfo,
             VexAbiInfo*  abiinfo
          )
{
   UChar     opc1;
   UInt      opc2;
   DisResult dres;
   UInt      theInstr;
   IRType    ty = mode64 ? Ity_I64 : Ity_I32;
   Bool      allow_F  = False;
   Bool      allow_V  = False;
   Bool      allow_FX = False;
   Bool      allow_GX = False;
   UInt      hwcaps = archinfo->hwcaps;
   Long      delta;

   /* What insn variants are we supporting today? */
   if (mode64) {
      allow_F  = True;
      allow_V  = (0 != (hwcaps & VEX_HWCAPS_PPC64_V));
      allow_FX = (0 != (hwcaps & VEX_HWCAPS_PPC64_FX));
      allow_GX = (0 != (hwcaps & VEX_HWCAPS_PPC64_GX));
   } else {
      allow_F  = (0 != (hwcaps & VEX_HWCAPS_PPC32_F));
      allow_V  = (0 != (hwcaps & VEX_HWCAPS_PPC32_V));
      allow_FX = (0 != (hwcaps & VEX_HWCAPS_PPC32_FX));
      allow_GX = (0 != (hwcaps & VEX_HWCAPS_PPC32_GX));
   }

   /* The running delta */
   delta = (Long)mkSzAddr(ty, (ULong)delta64);

   /* Set result defaults. */
   dres.whatNext   = Dis_Continue;
   dres.len        = 0;
   dres.continueAt = 0;

   /* At least this is simple on PPC32: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */
   theInstr = getUIntBigendianly( (UChar*)(&guest_code[delta]) );

   if (0) vex_printf("insn: 0x%x\n", theInstr);

   DIP("\t0x%llx:  ", (ULong)guest_CIA_curr_instr);

   /* We may be asked to update the guest CIA before going further. */
   if (put_IP)
      putGST( PPC_GST_CIA, mkSzImm(ty, guest_CIA_curr_instr) );

   /* Spot "Special" instructions (see comment at top of file). */
   {
      UChar* code = (UChar*)(guest_code + delta);
      /* Spot the 16-byte preamble: 
         32-bit mode:
            54001800  rlwinm 0,0,3,0,0
            54006800  rlwinm 0,0,13,0,0
            5400E800  rlwinm 0,0,29,0,0
            54009800  rlwinm 0,0,19,0,0
         64-bit mode:
            78001800  rotldi 0,0,3
            78006800  rotldi 0,0,13
            7800E802  rotldi 0,0,61
            78009802  rotldi 0,0,51
      */
      UInt word1 = mode64 ? 0x78001800 : 0x54001800;
      UInt word2 = mode64 ? 0x78006800 : 0x54006800;
      UInt word3 = mode64 ? 0x7800E802 : 0x5400E800;
      UInt word4 = mode64 ? 0x78009802 : 0x54009800;
      if (getUIntBigendianly(code+ 0) == word1 &&
          getUIntBigendianly(code+ 4) == word2 &&
          getUIntBigendianly(code+ 8) == word3 &&
          getUIntBigendianly(code+12) == word4) {
         /* Got a "Special" instruction preamble.  Which one is it? */
         if (getUIntBigendianly(code+16) == 0x7C210B78 /* or 1,1,1 */) {
            /* %R3 = client_request ( %R4 ) */
            DIP("r3 = client_request ( %%r4 )\n");
            delta += 20;
            irsb->next     = mkSzImm( ty, guest_CIA_bbstart + delta );
            irsb->jumpkind = Ijk_ClientReq;
            dres.whatNext  = Dis_StopHere;
            goto decode_success;
         }
         else
         if (getUIntBigendianly(code+16) == 0x7C421378 /* or 2,2,2 */) {
            /* %R3 = guest_NRADDR */
            DIP("r3 = guest_NRADDR\n");
            delta += 20;
            dres.len = 20;
            putIReg(3, IRExpr_Get( OFFB_NRADDR, ty ));
            goto decode_success;
         }
         else
         if (getUIntBigendianly(code+16) == 0x7C631B78 /* or 3,3,3 */) {
            /*  branch-and-link-to-noredir %R11 */
            DIP("branch-and-link-to-noredir r11\n");
            delta += 20;
            putGST( PPC_GST_LR, mkSzImm(ty, guest_CIA_bbstart + (Long)delta) );
            irsb->next     = getIReg(11);
            irsb->jumpkind = Ijk_NoRedir;
            dres.whatNext  = Dis_StopHere;
            goto decode_success;
         }
         else
         if (getUIntBigendianly(code+16) == 0x7C842378 /* or 4,4,4 */) {
            /* %R3 = guest_NRADDR_GPR2 */
            DIP("r3 = guest_NRADDR_GPR2\n");
            delta += 20;
            dres.len = 20;
            putIReg(3, IRExpr_Get( OFFB_NRADDR_GPR2, ty ));
            goto decode_success;
         }
         /* We don't know what it is.  Set opc1/opc2 so decode_failure
            can print the insn following the Special-insn preamble. */
         theInstr = getUIntBigendianly(code+16);
         opc1     = ifieldOPC(theInstr);
         opc2     = ifieldOPClo10(theInstr);
         goto decode_failure;
         /*NOTREACHED*/
      }
   }

   opc1 = ifieldOPC(theInstr);
   opc2 = ifieldOPClo10(theInstr);

   // Note: all 'reserved' bits must be cleared, else invalid
   switch (opc1) {

   /* Integer Arithmetic Instructions */
   case 0x0C: case 0x0D: case 0x0E:  // addic, addic., addi
   case 0x0F: case 0x07: case 0x08:  // addis, mulli,  subfic
      if (dis_int_arith( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Compare Instructions */
   case 0x0B: case 0x0A: // cmpi, cmpli
      if (dis_int_cmp( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Logical Instructions */
   case 0x1C: case 0x1D: case 0x18: // andi., andis., ori
   case 0x19: case 0x1A: case 0x1B: // oris,  xori,   xoris
      if (dis_int_logic( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Rotate Instructions */
   case 0x14: case 0x15:  case 0x17: // rlwimi, rlwinm, rlwnm
      if (dis_int_rot( theInstr )) goto decode_success;
      goto decode_failure;

   /* 64bit Integer Rotate Instructions */
   case 0x1E: // rldcl, rldcr, rldic, rldicl, rldicr, rldimi
      if (dis_int_rot( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Load Instructions */
   case 0x22: case 0x23: case 0x2A: // lbz,  lbzu, lha
   case 0x2B: case 0x28: case 0x29: // lhau, lhz,  lhzu
   case 0x20: case 0x21:            // lwz,  lwzu
      if (dis_int_load( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Store Instructions */
   case 0x26: case 0x27: case 0x2C: // stb,  stbu, sth
   case 0x2D: case 0x24: case 0x25: // sthu, stw,  stwu
      if (dis_int_store( theInstr, abiinfo )) goto decode_success;
      goto decode_failure;

   /* Integer Load and Store Multiple Instructions */
   case 0x2E: case 0x2F: // lmw, stmw
      if (dis_int_ldst_mult( theInstr )) goto decode_success;
      goto decode_failure;

   /* Branch Instructions */
   case 0x12: case 0x10: // b, bc
      if (dis_branch(theInstr, abiinfo, &dres, 
                               resteerOkFn, callback_opaque)) 
         goto decode_success;
      goto decode_failure;

   /* System Linkage Instructions */
   case 0x11: // sc
      if (dis_syslink(theInstr, abiinfo, &dres)) goto decode_success;
      goto decode_failure;

   /* Trap Instructions */
   case 0x02: case 0x03: // tdi, twi
      if (dis_trapi(theInstr, &dres)) goto decode_success;
      goto decode_failure;

   /* Floating Point Load Instructions */
   case 0x30: case 0x31: case 0x32: // lfs, lfsu, lfd
   case 0x33:                       // lfdu
      if (!allow_F) goto decode_noF;
      if (dis_fp_load( theInstr )) goto decode_success;
      goto decode_failure;

   /* Floating Point Store Instructions */
   case 0x34: case 0x35: case 0x36: // stfsx, stfsux, stfdx
   case 0x37:                       // stfdux
      if (!allow_F) goto decode_noF;
      if (dis_fp_store( theInstr )) goto decode_success;
      goto decode_failure;

      /* Floating Point Load Double Pair Instructions */
   case 0x39: case 0x3D:
      if (!allow_F) goto decode_noF;
      if (dis_fp_pair( theInstr )) goto decode_success;
      goto decode_failure;

   /* 64bit Integer Loads */
   case 0x3A:  // ld, ldu, lwa
      if (!mode64) goto decode_failure;
      if (dis_int_load( theInstr )) goto decode_success;
      goto decode_failure;

   case 0x3B:
      if (!allow_F) goto decode_noF;
      opc2 = IFIELD(theInstr, 1, 5);
      switch (opc2) {
      /* Floating Point Arith Instructions */
      case 0x12: case 0x14: case 0x15: // fdivs,  fsubs, fadds
      case 0x19:                       // fmuls
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;
      case 0x16:                       // fsqrts
         if (!allow_FX) goto decode_noFX;
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;
      case 0x18:                       // fres
         if (!allow_GX) goto decode_noGX;
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;

      /* Floating Point Mult-Add Instructions */
      case 0x1C: case 0x1D: case 0x1E: // fmsubs, fmadds, fnmsubs
      case 0x1F:                       // fnmadds
         if (dis_fp_multadd(theInstr)) goto decode_success;
         goto decode_failure;

      case 0x1A:                       // frsqrtes
         if (!allow_GX) goto decode_noGX;
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;
         
      default:
         goto decode_failure;
      }
      break;

   /* 64bit Integer Stores */
   case 0x3E:  // std, stdu
      if (!mode64) goto decode_failure;
      if (dis_int_store( theInstr, abiinfo )) goto decode_success;
      goto decode_failure;

   case 0x3F:
      if (!allow_F) goto decode_noF;
      /* Instrs using opc[1:5] never overlap instrs using opc[1:10],
         so we can simply fall through the first switch statement */

      opc2 = IFIELD(theInstr, 1, 5);
      switch (opc2) {
      /* Floating Point Arith Instructions */
      case 0x12: case 0x14: case 0x15: // fdiv, fsub, fadd
      case 0x19:                       // fmul
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;
      case 0x16:                       // fsqrt
         if (!allow_FX) goto decode_noFX;
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;
      case 0x17: case 0x1A:            // fsel, frsqrte
         if (!allow_GX) goto decode_noGX;
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;
         
      /* Floating Point Mult-Add Instructions */         
      case 0x1C: case 0x1D: case 0x1E: // fmsub, fmadd, fnmsub
      case 0x1F:                       // fnmadd
         if (dis_fp_multadd(theInstr)) goto decode_success;
         goto decode_failure;

      case 0x18:                       // fre
         if (!allow_GX) goto decode_noGX;
         if (dis_fp_arith(theInstr)) goto decode_success;
         goto decode_failure;
         
      default:
         break; // Fall through
      }

      opc2 = IFIELD(theInstr, 1, 10);
      switch (opc2) {
      /* Floating Point Compare Instructions */         
      case 0x000: // fcmpu
      case 0x020: // fcmpo
         if (dis_fp_cmp(theInstr)) goto decode_success;
         goto decode_failure;
         
      /* Floating Point Rounding/Conversion Instructions */         
      case 0x00C: // frsp
      case 0x00E: // fctiw
      case 0x00F: // fctiwz
      case 0x32E: // fctid
      case 0x32F: // fctidz
      case 0x34E: // fcfid
         if (dis_fp_round(theInstr)) goto decode_success;
         goto decode_failure;

      /* Power6 rounding stuff */
      case 0x1E8: // frim
      case 0x1C8: // frip
      case 0x188: // frin
      case 0x1A8: // friz
         /* A hack to check for P6 capability . . . */
         if ((allow_F && allow_V && allow_FX && allow_GX) &&
             (dis_fp_round(theInstr)))
            goto decode_success;
         goto decode_failure;
         
      /* Floating Point Move Instructions */         
      case 0x008: // fcpsgn
      case 0x028: // fneg
      case 0x048: // fmr
      case 0x088: // fnabs
      case 0x108: // fabs
         if (dis_fp_move( theInstr )) goto decode_success;
         goto decode_failure;

      /* Floating Point Status/Control Register Instructions */         
      case 0x026: // mtfsb1
      case 0x040: // mcrfs
      case 0x046: // mtfsb0
      case 0x086: // mtfsfi
      case 0x247: // mffs
      case 0x2C7: // mtfsf
         if (dis_fp_scr( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         goto decode_failure;
      }
      break;
      
   case 0x13:
      switch (opc2) {

      /* Condition Register Logical Instructions */
      case 0x101: case 0x081: case 0x121: // crand,  crandc, creqv
      case 0x0E1: case 0x021: case 0x1C1: // crnand, crnor,  cror
      case 0x1A1: case 0x0C1: case 0x000: // crorc,  crxor,  mcrf
         if (dis_cond_logic( theInstr )) goto decode_success;
         goto decode_failure;
         
      /* Branch Instructions */
      case 0x210: case 0x010: // bcctr, bclr
         if (dis_branch(theInstr, abiinfo, &dres, 
                                  resteerOkFn, callback_opaque)) 
            goto decode_success;
         goto decode_failure;
         
      /* Memory Synchronization Instructions */
      case 0x096: // isync
         if (dis_memsync( theInstr )) goto decode_success;
         goto decode_failure;
         
      default:
         goto decode_failure;
      }
      break;


   case 0x1F:

      /* For arith instns, bit10 is the OE flag (overflow enable) */

      opc2 = IFIELD(theInstr, 1, 9);
      switch (opc2) {
      /* Integer Arithmetic Instructions */
      case 0x10A: case 0x00A: case 0x08A: // add,   addc,  adde
      case 0x0EA: case 0x0CA: case 0x1EB: // addme, addze, divw
      case 0x1CB: case 0x04B: case 0x00B: // divwu, mulhw, mulhwu
      case 0x0EB: case 0x068: case 0x028: // mullw, neg,   subf
      case 0x008: case 0x088: case 0x0E8: // subfc, subfe, subfme
      case 0x0C8:                         // subfze
         if (dis_int_arith( theInstr )) goto decode_success;
         goto decode_failure;

      /* 64bit Integer Arithmetic */
      case 0x009: case 0x049: case 0x0E9: // mulhdu, mulhd, mulld
      case 0x1C9: case 0x1E9:             // divdu, divd
         if (!mode64) goto decode_failure;
         if (dis_int_arith( theInstr )) goto decode_success;
         goto decode_failure;

      case 0x1FC:                         // cmpb
         if (dis_int_logic( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         break;  // Fall through...
      }

      /* All remaining opcodes use full 10 bits. */

      opc2 = IFIELD(theInstr, 1, 10);
      switch (opc2) {
      /* Integer Compare Instructions  */
      case 0x000: case 0x020: // cmp, cmpl
         if (dis_int_cmp( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Logical Instructions */
      case 0x01C: case 0x03C: case 0x01A: // and,  andc,  cntlzw
      case 0x11C: case 0x3BA: case 0x39A: // eqv,  extsb, extsh
      case 0x1DC: case 0x07C: case 0x1BC: // nand, nor,   or
      case 0x19C: case 0x13C:             // orc,  xor
      case 0x2DF: case 0x25F:            // mftgpr, mffgpr
         if (dis_int_logic( theInstr )) goto decode_success;
         goto decode_failure;

      /* 64bit Integer Logical Instructions */
      case 0x3DA: case 0x03A: // extsw, cntlzd
         if (!mode64) goto decode_failure;
         if (dis_int_logic( theInstr )) goto decode_success;
         goto decode_failure;

         /* 64bit Integer Parity Instructions */
      case 0xba: case 0x9a: // prtyd, prtyw
         if (dis_int_parity( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Shift Instructions */
      case 0x018: case 0x318: case 0x338: // slw, sraw, srawi
      case 0x218:                         // srw
         if (dis_int_shift( theInstr )) goto decode_success;
         goto decode_failure;

      /* 64bit Integer Shift Instructions */
      case 0x01B: case 0x31A: // sld, srad
      case 0x33A: case 0x33B: // sradi
      case 0x21B:             // srd
         if (!mode64) goto decode_failure;
         if (dis_int_shift( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Load Instructions */
      case 0x057: case 0x077: case 0x157: // lbzx,  lbzux, lhax
      case 0x177: case 0x117: case 0x137: // lhaux, lhzx,  lhzux
      case 0x017: case 0x037:             // lwzx,  lwzux
         if (dis_int_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* 64bit Integer Load Instructions */
      case 0x035: case 0x015:             // ldux,  ldx
      case 0x175: case 0x155:             // lwaux, lwax
         if (!mode64) goto decode_failure;
         if (dis_int_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Store Instructions */
      case 0x0F7: case 0x0D7: case 0x1B7: // stbux, stbx,  sthux
      case 0x197: case 0x0B7: case 0x097: // sthx,  stwux, stwx
         if (dis_int_store( theInstr, abiinfo )) goto decode_success;
         goto decode_failure;

      /* 64bit Integer Store Instructions */
      case 0x0B5: case 0x095: // stdux, stdx
         if (!mode64) goto decode_failure;
         if (dis_int_store( theInstr, abiinfo )) goto decode_success;
         goto decode_failure;

      /* Integer Load and Store with Byte Reverse Instructions */
      case 0x316: case 0x216: case 0x396: // lhbrx, lwbrx, sthbrx
      case 0x296:                         // stwbrx
         if (dis_int_ldst_rev( theInstr )) goto decode_success;
         goto decode_failure;
         
      /* Integer Load and Store String Instructions */
      case 0x255: case 0x215: case 0x2D5: // lswi, lswx, stswi
      case 0x295: {                       // stswx
         Bool stopHere = False;
         Bool ok = dis_int_ldst_str( theInstr, &stopHere );
         if (!ok) goto decode_failure;
         if (stopHere) {
            irsb->next     = mkSzImm(ty, nextInsnAddr());
            irsb->jumpkind = Ijk_Boring;
            dres.whatNext  = Dis_StopHere;
         }
         goto decode_success;
      }

      /* Memory Synchronization Instructions */
      case 0x356: case 0x014: case 0x096: // eieio, lwarx, stwcx.
      case 0x256:                         // sync
         if (dis_memsync( theInstr )) goto decode_success;
         goto decode_failure;
         
      /* 64bit Memory Synchronization Instructions */
      case 0x054: case 0x0D6: // ldarx, stdcx.
         if (!mode64) goto decode_failure;
         if (dis_memsync( theInstr )) goto decode_success;
         goto decode_failure;

      /* Processor Control Instructions */
      case 0x200: case 0x013: case 0x153: // mcrxr, mfcr,  mfspr
      case 0x173: case 0x090: case 0x1D3: // mftb,  mtcrf, mtspr
         if (dis_proc_ctl( abiinfo, theInstr )) goto decode_success;
         goto decode_failure;

      /* Cache Management Instructions */
      case 0x2F6: case 0x056: case 0x036: // dcba, dcbf,   dcbst
      case 0x116: case 0x0F6: case 0x3F6: // dcbt, dcbtst, dcbz
      case 0x3D6:                         // icbi
         if (dis_cache_manage( theInstr, &dres, archinfo )) 
            goto decode_success;
         goto decode_failure;

//zz       /* External Control Instructions */
//zz       case 0x136: case 0x1B6: // eciwx, ecowx
//zz          DIP("external control op => not implemented\n");
//zz          goto decode_failure;

      /* Trap Instructions */
      case 0x004: case 0x044:             // tw,   td
         if (dis_trap(theInstr, &dres)) goto decode_success;
         goto decode_failure;

      /* Floating Point Load Instructions */
      case 0x217: case 0x237: case 0x257: // lfsx, lfsux, lfdx
      case 0x277:                         // lfdux
         if (!allow_F) goto decode_noF;
         if (dis_fp_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* Floating Point Store Instructions */
      case 0x297: case 0x2B7: case 0x2D7: // stfs,  stfsu, stfd
      case 0x2F7:                         // stfdu, stfiwx
         if (!allow_F) goto decode_noF;
         if (dis_fp_store( theInstr )) goto decode_success;
         goto decode_failure;
      case 0x3D7:                         // stfiwx
         if (!allow_F) goto decode_noF;
         if (!allow_GX) goto decode_noGX;
         if (dis_fp_store( theInstr )) goto decode_success;
         goto decode_failure;

         /* Floating Point Double Pair Indexed Instructions */
      case 0x317: // lfdpx (Power6)
      case 0x397: // stfdpx (Power6)
         if (!allow_F) goto decode_noF;
         if (dis_fp_pair(theInstr)) goto decode_success;
         goto decode_failure;

      case 0x357:                         // lfiwax
         if (!allow_F) goto decode_noF;
         if (dis_fp_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* AltiVec instructions */

      /* AV Cache Control - Data streams */
      case 0x156: case 0x176: case 0x336: // dst, dstst, dss
         if (!allow_V) goto decode_noV;
         if (dis_av_datastream( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Load */
      case 0x006: case 0x026:             // lvsl, lvsr
      case 0x007: case 0x027: case 0x047: // lvebx, lvehx, lvewx
      case 0x067: case 0x167:             // lvx, lvxl
         if (!allow_V) goto decode_noV;
         if (dis_av_load( abiinfo, theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Store */
      case 0x087: case 0x0A7: case 0x0C7: // stvebx, stvehx, stvewx
      case 0x0E7: case 0x1E7:             // stvx, stvxl
         if (!allow_V) goto decode_noV;
         if (dis_av_store( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         /* Deal with some other cases that we would otherwise have
            punted on. */
         /* --- ISEL (PowerISA_V2.05.pdf, p74) --- */
         /* only decode this insn when reserved bit 0 (31 in IBM's
            notation) is zero */
         if (IFIELD(theInstr, 0, 6) == (15<<1)) {
            UInt rT = ifieldRegDS( theInstr );
            UInt rA = ifieldRegA( theInstr );
            UInt rB = ifieldRegB( theInstr );
            UInt bi = ifieldRegC( theInstr );
            putIReg(
               rT,
               IRExpr_Mux0X( unop(Iop_32to8,getCRbit( bi )),
                             getIReg(rB),
                             rA == 0 ? (mode64 ? mkU64(0) : mkU32(0))
                                     : getIReg(rA) )
            );
            DIP("isel r%u,r%u,r%u,crb%u\n", rT,rA,rB,bi);
            goto decode_success;
         }
         goto decode_failure;
      }
      break;


   case 0x04:
      /* AltiVec instructions */

      opc2 = IFIELD(theInstr, 0, 6);
      switch (opc2) {
      /* AV Mult-Add, Mult-Sum */
      case 0x20: case 0x21: case 0x22: // vmhaddshs, vmhraddshs, vmladduhm
      case 0x24: case 0x25: case 0x26: // vmsumubm, vmsummbm, vmsumuhm
      case 0x27: case 0x28: case 0x29: // vmsumuhs, vmsumshm, vmsumshs
         if (!allow_V) goto decode_noV;
         if (dis_av_multarith( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Permutations */
      case 0x2A:                       // vsel
      case 0x2B:                       // vperm
      case 0x2C:                       // vsldoi
         if (!allow_V) goto decode_noV;
         if (dis_av_permute( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Floating Point Mult-Add/Sub */
      case 0x2E: case 0x2F:            // vmaddfp, vnmsubfp
         if (!allow_V) goto decode_noV;
         if (dis_av_fp_arith( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         break;  // Fall through...
      }

      opc2 = IFIELD(theInstr, 0, 11);
      switch (opc2) {
      /* AV Arithmetic */
      case 0x180:                         // vaddcuw
      case 0x000: case 0x040: case 0x080: // vaddubm, vadduhm, vadduwm
      case 0x200: case 0x240: case 0x280: // vaddubs, vadduhs, vadduws
      case 0x300: case 0x340: case 0x380: // vaddsbs, vaddshs, vaddsws
      case 0x580:                         // vsubcuw
      case 0x400: case 0x440: case 0x480: // vsububm, vsubuhm, vsubuwm
      case 0x600: case 0x640: case 0x680: // vsububs, vsubuhs, vsubuws
      case 0x700: case 0x740: case 0x780: // vsubsbs, vsubshs, vsubsws
      case 0x402: case 0x442: case 0x482: // vavgub, vavguh, vavguw
      case 0x502: case 0x542: case 0x582: // vavgsb, vavgsh, vavgsw
      case 0x002: case 0x042: case 0x082: // vmaxub, vmaxuh, vmaxuw
      case 0x102: case 0x142: case 0x182: // vmaxsb, vmaxsh, vmaxsw
      case 0x202: case 0x242: case 0x282: // vminub, vminuh, vminuw
      case 0x302: case 0x342: case 0x382: // vminsb, vminsh, vminsw
      case 0x008: case 0x048:             // vmuloub, vmulouh
      case 0x108: case 0x148:             // vmulosb, vmulosh
      case 0x208: case 0x248:             // vmuleub, vmuleuh
      case 0x308: case 0x348:             // vmulesb, vmulesh
      case 0x608: case 0x708: case 0x648: // vsum4ubs, vsum4sbs, vsum4shs
      case 0x688: case 0x788:             // vsum2sws, vsumsws
         if (!allow_V) goto decode_noV;
         if (dis_av_arith( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Rotate, Shift */
      case 0x004: case 0x044: case 0x084: // vrlb, vrlh, vrlw
      case 0x104: case 0x144: case 0x184: // vslb, vslh, vslw
      case 0x204: case 0x244: case 0x284: // vsrb, vsrh, vsrw
      case 0x304: case 0x344: case 0x384: // vsrab, vsrah, vsraw
      case 0x1C4: case 0x2C4:             // vsl, vsr
      case 0x40C: case 0x44C:             // vslo, vsro
         if (!allow_V) goto decode_noV;
         if (dis_av_shift( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Logic */
      case 0x404: case 0x444: case 0x484: // vand, vandc, vor
      case 0x4C4: case 0x504:             // vxor, vnor
         if (!allow_V) goto decode_noV;
         if (dis_av_logic( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Processor Control */
      case 0x604: case 0x644:             // mfvscr, mtvscr
         if (!allow_V) goto decode_noV;
         if (dis_av_procctl( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Floating Point Arithmetic */
      case 0x00A: case 0x04A:             // vaddfp, vsubfp
      case 0x10A: case 0x14A: case 0x18A: // vrefp, vrsqrtefp, vexptefp
      case 0x1CA:                         // vlogefp
      case 0x40A: case 0x44A:             // vmaxfp, vminfp
         if (!allow_V) goto decode_noV;
         if (dis_av_fp_arith( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Floating Point Round/Convert */
      case 0x20A: case 0x24A: case 0x28A: // vrfin, vrfiz, vrfip
      case 0x2CA:                         // vrfim
      case 0x30A: case 0x34A: case 0x38A: // vcfux, vcfsx, vctuxs
      case 0x3CA:                         // vctsxs
         if (!allow_V) goto decode_noV;
         if (dis_av_fp_convert( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Merge, Splat */
      case 0x00C: case 0x04C: case 0x08C: // vmrghb, vmrghh, vmrghw
      case 0x10C: case 0x14C: case 0x18C: // vmrglb, vmrglh, vmrglw
      case 0x20C: case 0x24C: case 0x28C: // vspltb, vsplth, vspltw
      case 0x30C: case 0x34C: case 0x38C: // vspltisb, vspltish, vspltisw
         if (!allow_V) goto decode_noV;
         if (dis_av_permute( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Pack, Unpack */
      case 0x00E: case 0x04E: case 0x08E: // vpkuhum, vpkuwum, vpkuhus
      case 0x0CE:                         // vpkuwus
      case 0x10E: case 0x14E: case 0x18E: // vpkshus, vpkswus, vpkshss
      case 0x1CE:                         // vpkswss
      case 0x20E: case 0x24E: case 0x28E: // vupkhsb, vupkhsh, vupklsb
      case 0x2CE:                         // vupklsh
      case 0x30E: case 0x34E: case 0x3CE: // vpkpx, vupkhpx, vupklpx
         if (!allow_V) goto decode_noV;
         if (dis_av_pack( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         break;  // Fall through...
      }

      opc2 = IFIELD(theInstr, 0, 10);
      switch (opc2) {

      /* AV Compare */
      case 0x006: case 0x046: case 0x086: // vcmpequb, vcmpequh, vcmpequw
      case 0x206: case 0x246: case 0x286: // vcmpgtub, vcmpgtuh, vcmpgtuw
      case 0x306: case 0x346: case 0x386: // vcmpgtsb, vcmpgtsh, vcmpgtsw
         if (!allow_V) goto decode_noV;
         if (dis_av_cmp( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Floating Point Compare */
      case 0x0C6: case 0x1C6: case 0x2C6: // vcmpeqfp, vcmpgefp, vcmpgtfp
      case 0x3C6:                         // vcmpbfp
         if (!allow_V) goto decode_noV;
         if (dis_av_fp_cmp( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         goto decode_failure;
      }
      break;

   default:
      goto decode_failure;

   decode_noF:
      vassert(!allow_F);
      vex_printf("disInstr(ppc): declined to decode an FP insn.\n");
      goto decode_failure;
   decode_noV:
      vassert(!allow_V);
      vex_printf("disInstr(ppc): declined to decode an AltiVec insn.\n");
      goto decode_failure;
   decode_noFX:
      vassert(!allow_FX);
      vex_printf("disInstr(ppc): "
                 "declined to decode a GeneralPurpose-Optional insn.\n");
      goto decode_failure;
   decode_noGX:
      vassert(!allow_GX);
      vex_printf("disInstr(ppc): "
                 "declined to decode a Graphics-Optional insn.\n");
      goto decode_failure;

   decode_failure:
   /* All decode failures end up here. */
   opc2 = (theInstr) & 0x7FF;
   vex_printf("disInstr(ppc): unhandled instruction: "
              "0x%x\n", theInstr);
   vex_printf("                 primary %d(0x%x), secondary %u(0x%x)\n", 
              opc1, opc1, opc2, opc2);

   /* Tell the dispatcher that this insn cannot be decoded, and so has
      not been executed, and (is currently) the next to be executed.
      CIA should be up-to-date since it made so at the start of each
      insn, but nevertheless be paranoid and update it again right
      now. */
   putGST( PPC_GST_CIA, mkSzImm(ty, guest_CIA_curr_instr) );
   irsb->next     = mkSzImm(ty, guest_CIA_curr_instr);
   irsb->jumpkind = Ijk_NoDecode;
   dres.whatNext  = Dis_StopHere;
   dres.len       = 0;
   return dres;

   } /* switch (opc) for the main (primary) opcode switch. */

  decode_success:
   /* All decode successes end up here. */
   DIP("\n");

   if (dres.len == 0) {
      dres.len = 4;
   } else {
      vassert(dres.len == 20);
   }
   return dres;
}

#undef DIP
#undef DIS


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult disInstr_PPC ( IRSB*        irsb_IN,
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
   IRType     ty;
   DisResult  dres;
   UInt       mask32, mask64;
   UInt hwcaps_guest = archinfo->hwcaps;

   vassert(guest_arch == VexArchPPC32 || guest_arch == VexArchPPC64);

   /* global -- ick */
   mode64 = guest_arch == VexArchPPC64;
   ty = mode64 ? Ity_I64 : Ity_I32;

   /* do some sanity checks */
   mask32 = VEX_HWCAPS_PPC32_F | VEX_HWCAPS_PPC32_V
            | VEX_HWCAPS_PPC32_FX | VEX_HWCAPS_PPC32_GX;

   mask64 = VEX_HWCAPS_PPC64_V
            | VEX_HWCAPS_PPC64_FX | VEX_HWCAPS_PPC64_GX;

   if (mode64) {
      vassert((hwcaps_guest & mask32) == 0);
   } else {
      vassert((hwcaps_guest & mask64) == 0);
   }

   /* Set globals (see top of this file) */
   guest_code           = guest_code_IN;
   irsb                 = irsb_IN;
   host_is_bigendian    = host_bigendian_IN;

   guest_CIA_curr_instr = mkSzAddr(ty, guest_IP);
   guest_CIA_bbstart    = mkSzAddr(ty, guest_IP - delta);

   dres = disInstr_PPC_WRK ( put_IP, 
                             resteerOkFn, resteerCisOk, callback_opaque,
                             delta, archinfo, abiinfo );

   return dres;
}


/*------------------------------------------------------------*/
/*--- Unused stuff                                         ---*/
/*------------------------------------------------------------*/

///* A potentially more memcheck-friendly implementation of Clz32, with
//   the boundary case Clz32(0) = 32, which is what ppc requires. */
//
//static IRExpr* /* :: Ity_I32 */ verbose_Clz32 ( IRTemp arg )
//{
//   /* Welcome ... to SSA R Us. */
//   IRTemp n1  = newTemp(Ity_I32);
//   IRTemp n2  = newTemp(Ity_I32);
//   IRTemp n3  = newTemp(Ity_I32);
//   IRTemp n4  = newTemp(Ity_I32);
//   IRTemp n5  = newTemp(Ity_I32);
//   IRTemp n6  = newTemp(Ity_I32);
//   IRTemp n7  = newTemp(Ity_I32);
//   IRTemp n8  = newTemp(Ity_I32);
//   IRTemp n9  = newTemp(Ity_I32);
//   IRTemp n10 = newTemp(Ity_I32);
//   IRTemp n11 = newTemp(Ity_I32);
//   IRTemp n12 = newTemp(Ity_I32);
//
//   /* First, propagate the most significant 1-bit into all lower
//      positions in the word. */
//   /* unsigned int clz ( unsigned int n )
//      {
//         n |= (n >> 1);
//         n |= (n >> 2);
//         n |= (n >> 4);
//         n |= (n >> 8);
//         n |= (n >> 16);
//         return bitcount(~n);
//      }
//   */
//   assign(n1, mkexpr(arg));
//   assign(n2, binop(Iop_Or32, mkexpr(n1), binop(Iop_Shr32, mkexpr(n1), mkU8(1))));
//   assign(n3, binop(Iop_Or32, mkexpr(n2), binop(Iop_Shr32, mkexpr(n2), mkU8(2))));
//   assign(n4, binop(Iop_Or32, mkexpr(n3), binop(Iop_Shr32, mkexpr(n3), mkU8(4))));
//   assign(n5, binop(Iop_Or32, mkexpr(n4), binop(Iop_Shr32, mkexpr(n4), mkU8(8))));
//   assign(n6, binop(Iop_Or32, mkexpr(n5), binop(Iop_Shr32, mkexpr(n5), mkU8(16))));
//   /* This gives a word of the form 0---01---1.  Now invert it, giving
//      a word of the form 1---10---0, then do a population-count idiom
//      (to count the 1s, which is the number of leading zeroes, or 32
//      if the original word was 0. */
//   assign(n7, unop(Iop_Not32, mkexpr(n6)));
//
//   /* unsigned int bitcount ( unsigned int n )
//      {
//         n = n - ((n >> 1) & 0x55555555);
//         n = (n & 0x33333333) + ((n >> 2) & 0x33333333);
//         n = (n + (n >> 4)) & 0x0F0F0F0F;
//         n = n + (n >> 8);
//         n = (n + (n >> 16)) & 0x3F;
//         return n;
//      }
//   */
//   assign(n8, 
//          binop(Iop_Sub32, 
//                mkexpr(n7),  
//                binop(Iop_And32, 
//                      binop(Iop_Shr32, mkexpr(n7), mkU8(1)),
//                      mkU32(0x55555555))));
//   assign(n9,
//          binop(Iop_Add32,
//                binop(Iop_And32, mkexpr(n8), mkU32(0x33333333)),
//                binop(Iop_And32,
//                      binop(Iop_Shr32, mkexpr(n8), mkU8(2)),
//                      mkU32(0x33333333))));
//   assign(n10,
//          binop(Iop_And32,
//                binop(Iop_Add32, 
//                      mkexpr(n9), 
//                      binop(Iop_Shr32, mkexpr(n9), mkU8(4))),
//                mkU32(0x0F0F0F0F)));
//   assign(n11,
//          binop(Iop_Add32,
//                mkexpr(n10),
//                binop(Iop_Shr32, mkexpr(n10), mkU8(8))));
//   assign(n12,
//          binop(Iop_Add32,
//                mkexpr(n11),
//                binop(Iop_Shr32, mkexpr(n11), mkU8(16))));
//   return
//      binop(Iop_And32, mkexpr(n12), mkU32(0x3F));
//}

/*--------------------------------------------------------------------*/
/*--- end                                         guest_ppc_toIR.c ---*/
/*--------------------------------------------------------------------*/
