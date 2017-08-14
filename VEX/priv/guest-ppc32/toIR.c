
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-ppc32/toIR.c) is                            ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* TODO 18/Nov/05:

   Spot rlwimi cases which are simply left/right shifts and
   emit Shl32/Shr32 accordingly.

   Altivec
   - datastream insns
   - lvxl,stvxl: load/store with 'least recently used' hint
   - vexptefp, vlogefp

   Floating Point
   - Single precision stores are rounded twice - once by F64toF32,
     and then again by the backend for storeBE( F32 ), giving a loss
     of precision.


   LIMITATIONS:

   Various, including:

   - Some invalid forms of lswi and lswx are accepted when they should
     not be.

   - Floating Point:
     - All exceptions disabled in FPSCR
     - condition codes not set in FPSCR
     - some error in accuracy

   - Altivec floating point:
     - vmaddfp, vnmsubfp
       Because we're using Java/IEEE mode (FPSCR[NJ]), rather than the
       system default of Non-Java mode, we get some small errors
       (lowest bit only).
       This is because Non-Java mode brutally hacks denormalised results
       to zero, whereas we keep maximum accuracy.  However, using
       Non-Java mode would give us more inaccuracy, as our intermediate
       results would then be zeroed, too.
*/


/* Translates PPC32 code to IR. */

/* References

#define PPC32
   "PowerPC Microprocessor Family:
    The Programming Environments for 32-Bit Microprocessors"
    02/21/2000
    http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/852569B20050FF778525699600719DF2

#define AV
   "PowerPC Microprocessor Family:
    AltiVec(TM) Technology Programming Environments Manual"
    07/10/2003
   http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/FBFA164F824370F987256D6A006F424D

   Other refs:
   "PowerPC Microprocessor Family:
    Programming Environments Manual for 64 and 32-Bit Microprocessors
    Version 2.0"
    06/10/2003
   http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/F6153E213FDD912E87256D49006C6541
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_ppc32.h"

#include "main/vex_util.h"
#include "main/vex_globals.h"
#include "guest-generic/bb_to_IR.h"
#include "guest-ppc32/gdefs.h"


/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of an insn, right
   down in disInstr_PPC32, so that we don't have to pass them around
   endlessly.  They are all constant during the translation of any
   given insn. */

/* We need to know this to do sub-register accesses correctly. */
static Bool host_is_bigendian;

/* Pointer to the guest code area. */
static UChar* guest_code;

/* The guest address corresponding to guest_code[0]. */
static Addr32 guest_CIA_bbstart;

/* The guest address for the instruction currently being
   translated. */
static Addr32 guest_CIA_curr_instr;

/* The IRBB* into which we're generating code. */
static IRBB* irbb;


/*------------------------------------------------------------*/
/*--- Debugging output                                     ---*/
/*------------------------------------------------------------*/

#define PPC32_TOIR_DEBUG 0

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

#define DIS(buf, format, args...)      \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_sprintf(buf, format, ## args)

#if PPC32_TOIR_DEBUG
static void vex_printf_binary( UInt x, UInt len, Bool spaces )
{
   UInt i;
   vassert(len > 0 && len <= 32);
   
   for (i=len; i>0; i--) {
      vex_printf("%d", ((x & (1<<(len-1))) != 0) );
      x = x << 1;
      if (((i-1)%4)==0 && (i > 1) && spaces) {
         vex_printf(" ");
      }
   }
}
#endif


/*------------------------------------------------------------*/
/*--- Offsets of various parts of the ppc32 guest state.   ---*/
/*------------------------------------------------------------*/

#define OFFB_CIA        offsetof(VexGuestPPC32State,guest_CIA)
#define OFFB_LR         offsetof(VexGuestPPC32State,guest_LR)
#define OFFB_CTR        offsetof(VexGuestPPC32State,guest_CTR)

#define OFFB_XER_SO     offsetof(VexGuestPPC32State,guest_XER_SO)
#define OFFB_XER_OV     offsetof(VexGuestPPC32State,guest_XER_OV)
#define OFFB_XER_CA     offsetof(VexGuestPPC32State,guest_XER_CA)
#define OFFB_XER_BC     offsetof(VexGuestPPC32State,guest_XER_BC)

#define OFFB_FPROUND    offsetof(VexGuestPPC32State,guest_FPROUND)

#define OFFB_VRSAVE     offsetof(VexGuestPPC32State,guest_VRSAVE)
#define OFFB_VSCR       offsetof(VexGuestPPC32State,guest_VSCR)

#define OFFB_EMWARN     offsetof(VexGuestPPC32State,guest_EMWARN)

#define OFFB_TISTART    offsetof(VexGuestPPC32State,guest_TISTART)
#define OFFB_TILEN      offsetof(VexGuestPPC32State,guest_TILEN)

#define OFFB_RESVN      offsetof(VexGuestPPC32State,guest_RESVN)


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

/* Extract bottom half, instr[15:0], and sign extent to 32 bits */
static Int ifieldSIMM16 ( UInt instr ) {
   Int i = instr & 0xFFFF;
   return (i << 16) >> 16;
}

/* Extract bottom 26 bits, instr[25:0], and sign extent to 32 bits */
static Int ifieldSIMM26 ( UInt instr ) {
   Int i = instr & 0x3FFFFFF;
   return (i << 6) >> 6;
}


/*------------------------------------------------------------*/
/*--- Special purpose registers (SPRs)                     ---*/
/*--- All non-GPR/FPRs, not only strict SPR's {XER,LR,CTR} ---*/
/*------------------------------------------------------------*/

typedef enum {
    PPC32_SPR_CIA,    // Current Instruction Address
    PPC32_SPR_LR,     // Link Register
    PPC32_SPR_CTR,    // Count Register
    PPC32_SPR_XER,    // Overflow, carry flags, byte count
    PPC32_SPR_CR,     // Condition Register
    PPC32_SPR_FPSCR,  // Floating Point Status/Control Register
    PPC32_SPR_VRSAVE, // Vector Save/Restore Register
    PPC32_SPR_VSCR,   // Vector Status and Control Register
    PPC32_SPR_MAX
} PPC32SPR;

#define MASK_FPSCR_RN   0x3
#define MASK_VSCR_VALID 0x00010001


/*------------------------------------------------------------*/
/*---  FP Helpers                                          ---*/
/*------------------------------------------------------------*/

static void put_emwarn ( IRExpr* e /* :: Ity_I32 */ );

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

static UInt MASK( UInt begin, UInt end )
{
   UInt m1 = ((UInt)(-1)) << begin;
   UInt m2 = ((UInt)(-1)) << (end + 1);
   UInt mask = m1 ^ m2;
   if (begin > end) mask = ~mask;  // wrap mask
   return mask;
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- ppc32 insn stream.                                   ---*/
/*------------------------------------------------------------*/

/* Add a statement to the list held by "irbb". */
static void stmt ( IRStmt* st )
{
   addStmtToIRBB( irbb, st );
}

/* Generate a new temporary of the given type. */
static IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp( irbb->tyenv, ty );
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
   stmt( IRStmt_Tmp(dst, e) );
}

static void storeBE ( IRExpr* addr, IRExpr* data )
{
   vassert(typeOfIRExpr(irbb->tyenv, addr) == Ity_I32);
   stmt( IRStmt_Store(Iend_BE,addr,data) );
}

static IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

static IRExpr* binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   return IRExpr_Binop(op, a1, a2);
}

static IRExpr* mkexpr ( IRTemp tmp )
{
   return IRExpr_Tmp(tmp);
}

//uu static IRExpr* mkU1 ( UInt i )
//uu {
//uu    vassert(i < 2);
//uu    return IRExpr_Const(IRConst_U1( toBool(i) ));
//uu }

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

static IRExpr* loadBE ( IRType ty, IRExpr* data )
{
   return IRExpr_Load(Iend_BE,ty,data);
}

static IRExpr* mkOR1 ( IRExpr* arg1, IRExpr* arg2 )
{
   vassert(typeOfIRExpr(irbb->tyenv, arg1) == Ity_I1);
   vassert(typeOfIRExpr(irbb->tyenv, arg2) == Ity_I1);
   return
      unop(Iop_32to1, binop(Iop_Or32, unop(Iop_1Uto32, arg1), 
                                      unop(Iop_1Uto32, arg2)));
}

static IRExpr* mkAND1 ( IRExpr* arg1, IRExpr* arg2 )
{
   vassert(typeOfIRExpr(irbb->tyenv, arg1) == Ity_I1);
   vassert(typeOfIRExpr(irbb->tyenv, arg2) == Ity_I1);
   return
      unop(Iop_32to1, binop(Iop_And32, unop(Iop_1Uto32, arg1), 
                                       unop(Iop_1Uto32, arg2)));
}

/* expand V128_8Ux16 to 2x V128_16Ux8's */
static void expand8Ux16( IRExpr* vIn, /*OUTs*/ IRTemp* vEvn, IRTemp* vOdd )
{
   IRTemp ones8x16 = newTemp(Ity_V128);

   vassert(typeOfIRExpr(irbb->tyenv, vIn) == Ity_V128);
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
static void expand8Sx16( IRExpr* vIn, /*OUTs*/ IRTemp* vEvn, IRTemp* vOdd )
{
   IRTemp ones8x16 = newTemp(Ity_V128);

   vassert(typeOfIRExpr(irbb->tyenv, vIn) == Ity_V128);
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
static void expand16Ux8( IRExpr* vIn, /*OUTs*/ IRTemp* vEvn, IRTemp* vOdd )
{
   IRTemp ones16x8 = newTemp(Ity_V128);

   vassert(typeOfIRExpr(irbb->tyenv, vIn) == Ity_V128);
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
static void expand16Sx8( IRExpr* vIn, /*OUTs*/ IRTemp* vEvn, IRTemp* vOdd )
{
   IRTemp ones16x8 = newTemp(Ity_V128);

   vassert(typeOfIRExpr(irbb->tyenv, vIn) == Ity_V128);
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

   vassert(typeOfIRExpr(irbb->tyenv, t128) == Ity_V128);
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

   vassert(typeOfIRExpr(irbb->tyenv, t128) == Ity_V128);
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

   vassert(typeOfIRExpr(irbb->tyenv, t64) == Ity_I64);

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

   vassert(typeOfIRExpr(irbb->tyenv, t64) == Ity_I64);

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
   vassert(typeOfIRExpr(irbb->tyenv, t3) == Ity_I64);
   vassert(typeOfIRExpr(irbb->tyenv, t2) == Ity_I64);
   vassert(typeOfIRExpr(irbb->tyenv, t1) == Ity_I64);
   vassert(typeOfIRExpr(irbb->tyenv, t0) == Ity_I64);
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
   vassert(typeOfIRExpr(irbb->tyenv, t3) == Ity_I64);
   vassert(typeOfIRExpr(irbb->tyenv, t2) == Ity_I64);
   vassert(typeOfIRExpr(irbb->tyenv, t1) == Ity_I64);
   vassert(typeOfIRExpr(irbb->tyenv, t0) == Ity_I64);
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



static Int integerGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   // jrs: probably not necessary; only matters if we reference sub-parts
   // of the ppc32 registers, but that isn't the case
   // later: this might affect Altivec though?
   vassert(host_is_bigendian);

   switch (archreg) {
      case  0: return offsetof(VexGuestPPC32State, guest_GPR0);
      case  1: return offsetof(VexGuestPPC32State, guest_GPR1);
      case  2: return offsetof(VexGuestPPC32State, guest_GPR2);
      case  3: return offsetof(VexGuestPPC32State, guest_GPR3);
      case  4: return offsetof(VexGuestPPC32State, guest_GPR4);
      case  5: return offsetof(VexGuestPPC32State, guest_GPR5);
      case  6: return offsetof(VexGuestPPC32State, guest_GPR6);
      case  7: return offsetof(VexGuestPPC32State, guest_GPR7);
      case  8: return offsetof(VexGuestPPC32State, guest_GPR8);
      case  9: return offsetof(VexGuestPPC32State, guest_GPR9);
      case 10: return offsetof(VexGuestPPC32State, guest_GPR10);
      case 11: return offsetof(VexGuestPPC32State, guest_GPR11);
      case 12: return offsetof(VexGuestPPC32State, guest_GPR12);
      case 13: return offsetof(VexGuestPPC32State, guest_GPR13);
      case 14: return offsetof(VexGuestPPC32State, guest_GPR14);
      case 15: return offsetof(VexGuestPPC32State, guest_GPR15);
      case 16: return offsetof(VexGuestPPC32State, guest_GPR16);
      case 17: return offsetof(VexGuestPPC32State, guest_GPR17);
      case 18: return offsetof(VexGuestPPC32State, guest_GPR18);
      case 19: return offsetof(VexGuestPPC32State, guest_GPR19);
      case 20: return offsetof(VexGuestPPC32State, guest_GPR20);
      case 21: return offsetof(VexGuestPPC32State, guest_GPR21);
      case 22: return offsetof(VexGuestPPC32State, guest_GPR22);
      case 23: return offsetof(VexGuestPPC32State, guest_GPR23);
      case 24: return offsetof(VexGuestPPC32State, guest_GPR24);
      case 25: return offsetof(VexGuestPPC32State, guest_GPR25);
      case 26: return offsetof(VexGuestPPC32State, guest_GPR26);
      case 27: return offsetof(VexGuestPPC32State, guest_GPR27);
      case 28: return offsetof(VexGuestPPC32State, guest_GPR28);
      case 29: return offsetof(VexGuestPPC32State, guest_GPR29);
      case 30: return offsetof(VexGuestPPC32State, guest_GPR30);
      case 31: return offsetof(VexGuestPPC32State, guest_GPR31);
      default: break;
   }
   vpanic("integerGuestRegOffset(ppc32,be)"); /*notreached*/
}

static IRExpr* getIReg ( UInt archreg )
{
   vassert(archreg < 32);
   return IRExpr_Get( integerGuestRegOffset(archreg), Ity_I32 );
}

/* Ditto, but write to a reg instead. */
static void putIReg ( UInt archreg, IRExpr* e )
{
   vassert(archreg < 32);
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I32);
   stmt( IRStmt_Put(integerGuestRegOffset(archreg), e) );
}


static Int floatGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   switch (archreg) {
      case  0: return offsetof(VexGuestPPC32State, guest_FPR0);
      case  1: return offsetof(VexGuestPPC32State, guest_FPR1);
      case  2: return offsetof(VexGuestPPC32State, guest_FPR2);
      case  3: return offsetof(VexGuestPPC32State, guest_FPR3);
      case  4: return offsetof(VexGuestPPC32State, guest_FPR4);
      case  5: return offsetof(VexGuestPPC32State, guest_FPR5);
      case  6: return offsetof(VexGuestPPC32State, guest_FPR6);
      case  7: return offsetof(VexGuestPPC32State, guest_FPR7);
      case  8: return offsetof(VexGuestPPC32State, guest_FPR8);
      case  9: return offsetof(VexGuestPPC32State, guest_FPR9);
      case 10: return offsetof(VexGuestPPC32State, guest_FPR10);
      case 11: return offsetof(VexGuestPPC32State, guest_FPR11);
      case 12: return offsetof(VexGuestPPC32State, guest_FPR12);
      case 13: return offsetof(VexGuestPPC32State, guest_FPR13);
      case 14: return offsetof(VexGuestPPC32State, guest_FPR14);
      case 15: return offsetof(VexGuestPPC32State, guest_FPR15);
      case 16: return offsetof(VexGuestPPC32State, guest_FPR16);
      case 17: return offsetof(VexGuestPPC32State, guest_FPR17);
      case 18: return offsetof(VexGuestPPC32State, guest_FPR18);
      case 19: return offsetof(VexGuestPPC32State, guest_FPR19);
      case 20: return offsetof(VexGuestPPC32State, guest_FPR20);
      case 21: return offsetof(VexGuestPPC32State, guest_FPR21);
      case 22: return offsetof(VexGuestPPC32State, guest_FPR22);
      case 23: return offsetof(VexGuestPPC32State, guest_FPR23);
      case 24: return offsetof(VexGuestPPC32State, guest_FPR24);
      case 25: return offsetof(VexGuestPPC32State, guest_FPR25);
      case 26: return offsetof(VexGuestPPC32State, guest_FPR26);
      case 27: return offsetof(VexGuestPPC32State, guest_FPR27);
      case 28: return offsetof(VexGuestPPC32State, guest_FPR28);
      case 29: return offsetof(VexGuestPPC32State, guest_FPR29);
      case 30: return offsetof(VexGuestPPC32State, guest_FPR30);
      case 31: return offsetof(VexGuestPPC32State, guest_FPR31);
      default: break;
   }
   vpanic("floatGuestRegOffset(ppc32)"); /*notreached*/
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
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_F64);
   stmt( IRStmt_Put(floatGuestRegOffset(archreg), e) );
}


static Int vectorGuestRegOffset ( UInt archreg )
{
   vassert(archreg < 32);
   
   switch (archreg) {
      case  0: return offsetof(VexGuestPPC32State, guest_VR0);
      case  1: return offsetof(VexGuestPPC32State, guest_VR1);
      case  2: return offsetof(VexGuestPPC32State, guest_VR2);
      case  3: return offsetof(VexGuestPPC32State, guest_VR3);
      case  4: return offsetof(VexGuestPPC32State, guest_VR4);
      case  5: return offsetof(VexGuestPPC32State, guest_VR5);
      case  6: return offsetof(VexGuestPPC32State, guest_VR6);
      case  7: return offsetof(VexGuestPPC32State, guest_VR7);
      case  8: return offsetof(VexGuestPPC32State, guest_VR8);
      case  9: return offsetof(VexGuestPPC32State, guest_VR9);
      case 10: return offsetof(VexGuestPPC32State, guest_VR10);
      case 11: return offsetof(VexGuestPPC32State, guest_VR11);
      case 12: return offsetof(VexGuestPPC32State, guest_VR12);
      case 13: return offsetof(VexGuestPPC32State, guest_VR13);
      case 14: return offsetof(VexGuestPPC32State, guest_VR14);
      case 15: return offsetof(VexGuestPPC32State, guest_VR15);
      case 16: return offsetof(VexGuestPPC32State, guest_VR16);
      case 17: return offsetof(VexGuestPPC32State, guest_VR17);
      case 18: return offsetof(VexGuestPPC32State, guest_VR18);
      case 19: return offsetof(VexGuestPPC32State, guest_VR19);
      case 20: return offsetof(VexGuestPPC32State, guest_VR20);
      case 21: return offsetof(VexGuestPPC32State, guest_VR21);
      case 22: return offsetof(VexGuestPPC32State, guest_VR22);
      case 23: return offsetof(VexGuestPPC32State, guest_VR23);
      case 24: return offsetof(VexGuestPPC32State, guest_VR24);
      case 25: return offsetof(VexGuestPPC32State, guest_VR25);
      case 26: return offsetof(VexGuestPPC32State, guest_VR26);
      case 27: return offsetof(VexGuestPPC32State, guest_VR27);
      case 28: return offsetof(VexGuestPPC32State, guest_VR28);
      case 29: return offsetof(VexGuestPPC32State, guest_VR29);
      case 30: return offsetof(VexGuestPPC32State, guest_VR30);
      case 31: return offsetof(VexGuestPPC32State, guest_VR31);
      default: break;
   }
   vpanic("vextorGuestRegOffset(ppc32)"); /*notreached*/
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
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_V128);
   stmt( IRStmt_Put(vectorGuestRegOffset(archreg), e) );
}

static Int guestCR321offset ( UInt cr )
{
   switch (cr) {
      case 0: return offsetof(VexGuestPPC32State, guest_CR0_321 );
      case 1: return offsetof(VexGuestPPC32State, guest_CR1_321 );
      case 2: return offsetof(VexGuestPPC32State, guest_CR2_321 );
      case 3: return offsetof(VexGuestPPC32State, guest_CR3_321 );
      case 4: return offsetof(VexGuestPPC32State, guest_CR4_321 );
      case 5: return offsetof(VexGuestPPC32State, guest_CR5_321 );
      case 6: return offsetof(VexGuestPPC32State, guest_CR6_321 );
      case 7: return offsetof(VexGuestPPC32State, guest_CR7_321 );
      default: vpanic("guestCR321offset(ppc32)");
   }
} 

static Int guestCR0offset ( UInt cr )
{
   switch (cr) {
      case 0: return offsetof(VexGuestPPC32State, guest_CR0_0 );
      case 1: return offsetof(VexGuestPPC32State, guest_CR1_0 );
      case 2: return offsetof(VexGuestPPC32State, guest_CR2_0 );
      case 3: return offsetof(VexGuestPPC32State, guest_CR3_0 );
      case 4: return offsetof(VexGuestPPC32State, guest_CR4_0 );
      case 5: return offsetof(VexGuestPPC32State, guest_CR5_0 );
      case 6: return offsetof(VexGuestPPC32State, guest_CR6_0 );
      case 7: return offsetof(VexGuestPPC32State, guest_CR7_0 );
      default: vpanic("guestCR3offset(ppc32)");
   }
}

// ROTL(src32, rot_amt5)
static IRExpr* ROTL32 ( IRExpr* src, IRExpr* rot_amt )
{
   IRExpr* masked;
   vassert(typeOfIRExpr(irbb->tyenv,src) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,rot_amt) == Ity_I32);

   masked 
      = unop(Iop_32to8, binop(Iop_And32, rot_amt, mkU32(31)));

   // (src << rot_amt) | (src >> (32-rot_amt))
   /* Note: the MuxOX is not merely an optimisation; it's needed
      because otherwise the Shr32 is a shift by the word size when
      masked denotes zero.  For rotates by immediates, a lot of
      this junk gets folded out. */
   return 
      IRExpr_Mux0X( 
         masked,
         /* zero rotate. */
         src,
         /* non-zero rotate */
         binop( Iop_Or32,
                binop(Iop_Shl32, src, masked),
                binop(Iop_Shr32, src, binop(Iop_Sub8, mkU8(32), masked))
         )
      );
}

/* Do the standard effective address calculation: (rA|0) + rB. */
static IRExpr* /* :: Ity_I32 */ ea_standard ( Int rA, Int rB )
{
   vassert(rA >= 0 && rA < 32);
   vassert(rB >= 0 && rB < 32);
   if (rA == 0) {
      return getIReg(rB);
   } else {
      return binop(Iop_Add32, getIReg(rA), getIReg(rB));
   }
}

/* Do the effective address calculation: (rA|0). */
static IRExpr* /* :: Ity_I32 */ ea_rA_or_zero ( Int rA )
{
   vassert(rA >= 0 && rA < 32);
   if (rA == 0) {
      return mkU32(0);
   } else {
      return getIReg(rA);
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
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(guestCR321offset(cr), e) );
}

static void putCR0 ( UInt cr, IRExpr* e )
{
   vassert(cr < 8);
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I8);
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
   vassert(typeOfIRExpr(irbb->tyenv,bit) == Ity_I32);
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

static IRExpr* /* :: Ity_I32 */ getCRbit_anywhere ( UInt bi, Int* where )
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
   vassert(typeOfIRExpr(irbb->tyenv,result) == Ity_I32);
   putCR321( 0, unop(Iop_32to8,
                     binop(Iop_CmpORD32S, result, mkU32(0))) );
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

   vassert(typeOfIRExpr(irbb->tyenv,result) == Ity_V128);

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
                          binop(Iop_AndV128, mkexpr(v2), mkexpr(v3)))))) );
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
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(OFFB_XER_SO, binop(Iop_And8, mkU8(1), e)) );
}

static void putXER_OV ( IRExpr* e )
{
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(OFFB_XER_OV, binop(Iop_And8, mkU8(1), e)) );
}

static void putXER_CA ( IRExpr* e )
{
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(OFFB_XER_CA, binop(Iop_And8, mkU8(1), e)) );
}

static void putXER_BC ( IRExpr* e )
{
   vassert(typeOfIRExpr(irbb->tyenv, e) == Ity_I8);
   stmt( IRStmt_Put(OFFB_XER_BC,  binop(Iop_And8, mkU8(0x7F), e)) );
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
   return binop( Iop_And32,
                 unop(Iop_8Uto32, IRExpr_Get( OFFB_XER_CA, Ity_I8 )),
                 mkU32(1) );
}

static IRExpr* /* :: Ity_I8 */ getXER_BC ( void )
{
   return IRExpr_Get( OFFB_XER_BC, Ity_I8 );
}

static IRExpr* /* :: Ity_I32 */ getXER_BC32 ( void )
{
   return binop( Iop_And32,
                 unop(Iop_8Uto32, IRExpr_Get( OFFB_XER_BC, Ity_I8 )),
                 mkU32(0x7F) );
}


/* RES is the result of doing OP on ARGL and ARGR.  Set %XER.OV and
   %XER.SO accordingly. */

static void set_XER_OV( UInt op, IRExpr* res,
                                 IRExpr* argL, IRExpr* argR )
{
   IRTemp  t64;
   IRExpr* xer_ov;
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argL) == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argR) == Ity_I32);

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

      case /* 0  */ PPC32G_FLAG_OP_ADD:
      case /* 1  */ PPC32G_FLAG_OP_ADDE:
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

      case /* 2  */ PPC32G_FLAG_OP_DIVW:
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

      case /* 3  */ PPC32G_FLAG_OP_DIVWU:
         /* argR == 0 */
         xer_ov 
            = unop(Iop_1Uto32, binop(Iop_CmpEQ32, argR, mkU32(0)));
         break;

      case /* 4  */ PPC32G_FLAG_OP_MULLW:
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

      case /* 5  */ PPC32G_FLAG_OP_NEG:
         /* argL == INT32_MIN */
         xer_ov
            = unop( Iop_1Uto32, 
                    binop(Iop_CmpEQ32, argL, mkU32(INT32_MIN)) );
         break;

      case /* 6  */ PPC32G_FLAG_OP_SUBF:
      case /* 7  */ PPC32G_FLAG_OP_SUBFC:
      case /* 8  */ PPC32G_FLAG_OP_SUBFE:
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
         vpanic("set_XER_OV(ppc32)");
   }
 
   /* xer_ov MUST denote either 0 or 1, no other value allowed */
   stmt( IRStmt_Put( OFFB_XER_OV, unop(Iop_32to8, xer_ov) ) );

   /* Update the summary overflow */
   putXER_SO( binop(Iop_Or8, getXER_SO(), getXER_OV()) );

#  undef INT32_MIN
#  undef AND3
#  undef XOR3
#  undef XOR2
#  undef NOT
}


/* RES is the result of doing OP on ARGL and ARGR with the old %XER.CA
   value being OLDCA.  Set %XER.CA accordingly. */

static void set_XER_CA( UInt op, 
                        IRExpr* res,
                        IRExpr* argL, 
                        IRExpr* argR,
                        IRExpr* oldca )
{
   IRExpr* xer_ca;
   vassert(op < PPC32G_FLAG_OP_NUMBER);
   vassert(typeOfIRExpr(irbb->tyenv,res)   == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argL)  == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,argR)  == Ity_I32);
   vassert(typeOfIRExpr(irbb->tyenv,oldca) == Ity_I32);

   /* Incoming oldca is assumed to hold the values 0 or 1 only.  This
      seems reasonable given that it's always generated by
      getXER_CA32(), which masks it accordingly.  In any case it being
      0 or 1 is an invariant of the ppc32 guest state representation;
      if it has any other value, that invariant has been violated. */

   switch (op) {

      case /* 0 */ PPC32G_FLAG_OP_ADD:
         /* res <u argL */
         xer_ca
            = unop(Iop_1Uto32, binop(Iop_CmpLT32U, res, argL));
         break;

      case /* 1 */ PPC32G_FLAG_OP_ADDE:
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

      case /* 8 */ PPC32G_FLAG_OP_SUBFE:
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

      case /* 7 */ PPC32G_FLAG_OP_SUBFC:
      case /* 9 */ PPC32G_FLAG_OP_SUBFI:
         /* res <=u argR */
         xer_ca
            = unop(Iop_1Uto32, binop(Iop_CmpLE32U, res, argR));
         break;

      case /* 10 */ PPC32G_FLAG_OP_SRAW:
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
                               binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,argR)),
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

      case /* 11 */ PPC32G_FLAG_OP_SRAWI:
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
                               binop(Iop_Shl32, mkU32(1), unop(Iop_32to8,argR)),
                               mkU32(1) )
                 )
              );
         xer_ca 
            = unop(Iop_1Uto32, binop(Iop_CmpNE32, xer_ca, mkU32(0)));
         break;

      default: 
         vex_printf("set_XER_CA: op = %u\n", op);
         vpanic("set_XER_CA(ppc32)");
   }

   /* xer_ca MUST denote either 0 or 1, no other value allowed */
   putXER_CA( unop(Iop_32to8, xer_ca) );
}



/*------------------------------------------------------------*/
/*--- SPR register interface                              --- */
/*------------------------------------------------------------*/

/* Get a masked word from the given reg */
static IRExpr* /* ::Ity_I32 */ getSPR_masked ( PPC32SPR reg, UInt mask )
{
   IRTemp val = newTemp(Ity_I32);
   vassert( reg < PPC32_SPR_MAX );
    
   switch (reg) {

   case PPC32_SPR_FPSCR: {
      vassert((mask & 0x3)    == 0x3    || (mask & 0x3)    == 0x0);
      vassert((mask & 0xF000) == 0xF000 || (mask & 0xF000) == 0x0);
      /* all masks now refer to valid fields */
      
      /* Vex-generated code expects to run with the FPSCR set as follows:
         all exceptions masked, round-to-nearest.
         This corresponds to a FPSCR value of 0x0. */

      /* We're only keeping track of the rounding mode,
         so if the mask isn't asking for this, just return 0x0 */
      if (mask & 0x3) {
         assign( val, IRExpr_Get(OFFB_FPROUND, Ity_I32) );
      } else {
         assign( val, mkU32(0x0) );
      }
      break;
   }

   default:
      vpanic("getSPR_masked(ppc32)");
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
static IRExpr* /* ::Ity_I32 */ getSPR_field ( PPC32SPR reg, UInt fld )
{
   UInt shft, mask;

   vassert( fld < 8 );
   vassert( reg < PPC32_SPR_MAX );
   
   shft = 4*(7-fld);
   mask = 0xF<<shft;

   switch (reg) {
   case PPC32_SPR_XER:
      vassert(fld ==7);
      return binop(Iop_Or32,
                   binop(Iop_Or32,
                         binop(Iop_Shl32, getXER_SO32(), mkU8(3)),
                         binop(Iop_Shl32, getXER_OV32(), mkU8(2))),
                   binop(      Iop_Shl32, getXER_CA32(), mkU8(1)));
      break;

   default:
      if (shft == 0)
         return getSPR_masked( reg, mask );
      else
         return binop(Iop_Shr32,
                      getSPR_masked( reg, mask ),
                      mkU8(toUChar( shft )));
   }
}

static IRExpr* /* :: Ity_I32 */ getSPR ( PPC32SPR reg )
{
   switch (reg) {
   case PPC32_SPR_LR: 
      return IRExpr_Get( OFFB_LR, Ity_I32 );
   case PPC32_SPR_CTR: 
      return IRExpr_Get( OFFB_CTR, Ity_I32 );
   case PPC32_SPR_VRSAVE: 
      return IRExpr_Get( OFFB_VRSAVE, Ity_I32 );
   case PPC32_SPR_VSCR:
      return binop(Iop_And32,
                   IRExpr_Get( OFFB_VSCR, Ity_I32 ),
                   mkU32(MASK_VSCR_VALID));
   case PPC32_SPR_CR: {
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
   case PPC32_SPR_XER: {
      return binop(Iop_Or32,
                   binop(Iop_Or32,
                         binop( Iop_Shl32, getXER_SO32(), mkU8(31)),
                         binop( Iop_Shl32, getXER_OV32(), mkU8(30))),
                   binop(Iop_Or32,
                         binop( Iop_Shl32, getXER_CA32(), mkU8(29)),
                         getXER_BC32()));
   }
   default:
      vpanic("getSPR(ppc32)");
   }
}

/* Write masked src to the given reg */
static void putSPR_masked ( PPC32SPR reg, IRExpr* src, UInt mask )
{
   vassert( reg < PPC32_SPR_MAX );
   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   
   switch (reg) {
   case PPC32_SPR_FPSCR: {
      vassert((mask & 0x3)    == 0x3    || (mask & 0x3)    == 0x0);
      vassert((mask & 0xF000) == 0xF000 || (mask & 0xF000) == 0x0);
      /* all masks now refer to valid fields */

      /* Allow writes to Rounding Mode */
      if (mask & 0x3) {
         /* construct new fpround from new and old values as per mask:
            new fpround = (src & (3 & mask)) | (fpround & (3 & ~mask)) */
         stmt(
            IRStmt_Put(
               OFFB_FPROUND,
               binop(
                  Iop_Or32,
                  binop(Iop_And32, src, mkU32(3 & mask)),
                  binop(
                     Iop_And32,
                     IRExpr_Get(OFFB_FPROUND,Ity_I32),
                     mkU32(3 & ~mask)
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
         VexEmWarn ew = EmWarn_PPC32exns;

         /* If any of the src::exception_control bits are actually set,
            side-exit to the next insn, reporting the warning,
            so that Valgrind's dispatcher sees the warning. */
         put_emwarn( mkU32(ew) );
         stmt( 
            IRStmt_Exit(
               binop(Iop_CmpNE32, mkU32(ew), mkU32(EmWarn_NONE)),
               Ijk_EmWarn,
               IRConst_U32(guest_CIA_curr_instr + 4)
               )
            );
      }

      /* Ignore all other writes */
      break;
   }

   default:
      vex_printf("putSPR_masked(ppc32): %u", reg);
      vpanic("putSPR_masked(ppc32)");
   }
}

/* Write the least significant nibble of src to the specified
   REG[FLD] (as per IBM/hardware notation). */
static void putSPR_field ( PPC32SPR reg, IRExpr* src, UInt fld )
{
   UInt shft, mask;

   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   vassert( fld < 8 );
   vassert( reg < PPC32_SPR_MAX );
   
   shft = 4*(7-fld);
   mask = 0xF<<shft;

   switch (reg) {
   case PPC32_SPR_CR:
      putCR0  (fld, binop(Iop_And8, mkU8(1   ), unop(Iop_32to8, src)));
      putCR321(fld, binop(Iop_And8, mkU8(7<<1), unop(Iop_32to8, src)));
      break;

   default:
      if (shft == 0) {
         putSPR_masked( reg, src, mask );
      } else {
         putSPR_masked( reg,
                        binop(Iop_Shl32, src, mkU8(toUChar(shft))),
                        mask );
      }
   }
}

static void putSPR ( PPC32SPR reg, IRExpr* src )
{
   vassert( reg < PPC32_SPR_MAX );
   vassert( typeOfIRExpr(irbb->tyenv,src ) == Ity_I32 );
   switch (reg) {
      case PPC32_SPR_CIA: 
         stmt( IRStmt_Put( OFFB_CIA, src ) );
         break;
      case PPC32_SPR_LR: 
         stmt( IRStmt_Put( OFFB_LR, src ) );
         break;
      case PPC32_SPR_CTR: 
         stmt( IRStmt_Put( OFFB_CTR, src ) );
         break;
      case PPC32_SPR_VRSAVE: 
         stmt( IRStmt_Put( OFFB_VRSAVE, src ) );
         break;
      case PPC32_SPR_VSCR:
         stmt( IRStmt_Put( OFFB_VSCR,
                           binop(Iop_And32, src,
                                 mkU32(MASK_VSCR_VALID)) ) );
         break;
      case PPC32_SPR_XER:
         putXER_SO( unop(Iop_32to8, binop(Iop_Shr32, src, mkU8(31))) );
         putXER_OV( unop(Iop_32to8, binop(Iop_Shr32, src, mkU8(30))) );
         putXER_CA( unop(Iop_32to8, binop(Iop_Shr32, src, mkU8(29))) );
         putXER_BC( unop(Iop_32to8, src) );
         break;

      default:
         vpanic("putSPR(ppc32)");
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
   Int  simm16   = ifieldSIMM16(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UChar flag_OE = ifieldBIT10(theInstr);
   UInt  opc2    = ifieldOPClo9(theInstr);
   UChar flag_rC = ifieldBIT0(theInstr);

   IRTemp rA     = newTemp(Ity_I32);
   IRTemp rB     = newTemp(Ity_I32);
   IRTemp rD     = newTemp(Ity_I32);
   IRTemp res64  = newTemp(Ity_I64);  // multiplies need this.

   Bool do_rc = False;

   assign( rA, getIReg(rA_addr) );
   assign( rB, getIReg(rB_addr) );         // XO-Form: rD, rA, rB

   switch (opc1) {
   /* D-Form */
   case 0x0C: // addic  (Add Immediate Carrying, PPC32 p351
      DIP("addic r%d,r%d,%d\n", rD_addr, rA_addr, simm16);
      assign( rD, binop( Iop_Add32, mkexpr(rA), mkU32(simm16) ) );
      set_XER_CA( PPC32G_FLAG_OP_ADD, 
                  mkexpr(rD), mkexpr(rA), mkU32(simm16),
                  mkU32(0)/*old xer.ca, which is ignored*/ );
      break;
    
   case 0x0D: // addic. (Add Immediate Carrying and Record, PPC32 p352)
      DIP("addic. r%d,r%d,%d\n", rD_addr, rA_addr, simm16);
      assign( rD, binop( Iop_Add32, mkexpr(rA), mkU32(simm16) ) );
      set_XER_CA( PPC32G_FLAG_OP_ADD, 
                  mkexpr(rD), mkexpr(rA), mkU32(simm16), 
                  mkU32(0)/*old xer.ca, which is ignored*/ );
      do_rc = True;  // Always record to CR
      flag_rC = 1;
      break;

   case 0x0E: // addi   (Add Immediate, PPC32 p350)
      // li rD,val   == addi rD,0,val
      // la disp(rA) == addi rD,rA,disp
      if ( rA_addr == 0 ) {
         DIP("li r%d,%d\n", rD_addr, simm16);
         assign( rD, mkU32(simm16) );
      } else {
         DIP("addi r%d,r%d,%d\n", rD_addr, rA_addr, simm16);
         assign( rD, binop( Iop_Add32, mkexpr(rA), mkU32(simm16) ) );
      }
      break;

   case 0x0F: // addis  (Add Immediate Shifted, PPC32 p353)
      // lis rD,val == addis rD,0,val
      if ( rA_addr == 0 ) {
         DIP("lis r%d,%d\n", rD_addr, simm16);
         assign( rD, mkU32(simm16 << 16) );
      } else {
         DIP("addis r%d,r%d,0x%x\n", rD_addr, rA_addr, simm16);
         assign( rD, binop(Iop_Add32, mkexpr(rA), mkU32(simm16 << 16)) );
      }
      break;

   case 0x07: // mulli    (Multiply Low Immediate, PPC32 p490)
      DIP("mulli r%d,r%d,%d\n", rD_addr, rA_addr, simm16);
      assign( res64, binop(Iop_MullS32, mkexpr(rA), mkU32(simm16)) );
      assign( rD, unop(Iop_64to32, mkexpr(res64)) );
      break;

   case 0x08: // subfic   (Subtract from Immediate Carrying, PPC32 p540)
      DIP("subfic r%d,r%d,%d\n", rD_addr, rA_addr, simm16);
      // rD = simm16 - rA
      assign( rD, binop(Iop_Sub32, mkU32(simm16), mkexpr(rA)) );
      set_XER_CA( PPC32G_FLAG_OP_SUBFI, 
                  mkexpr(rD), mkexpr(rA), mkU32(simm16),
                  mkU32(0)/*old xer.ca, which is ignored*/ );
      break;

   /* XO-Form */
   case 0x1F:
      do_rc = True;    // All below record to CR
      
      switch (opc2) {
      case 0x10A: // add  (Add, PPC32 p347)
         DIP("add%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop(Iop_Add32, mkexpr(rA), mkexpr(rB)) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADD,
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;

      case 0x00A: // addc      (Add Carrying, PPC32 p348)
         DIP("addc%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop(Iop_Add32, mkexpr(rA), mkexpr(rB)) );
         set_XER_CA( PPC32G_FLAG_OP_ADD, 
                     mkexpr(rD), mkexpr(rA), mkexpr(rB),
                     mkU32(0)/*old xer.ca, which is ignored*/ );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADD, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
         
      case 0x08A: { // adde      (Add Extended, PPC32 p349)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         DIP("adde%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         // rD = rA + rB + XER[CA]
         assign( old_xer_ca, getXER_CA32() );
         assign( rD, binop(Iop_Add32, mkexpr(rA),
                           binop(Iop_Add32, mkexpr(rB), mkexpr(old_xer_ca))) );
         set_XER_CA( PPC32G_FLAG_OP_ADDE, 
                     mkexpr(rD), mkexpr(rA), mkexpr(rB),
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADDE, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
      }

      case 0x0EA: { // addme      (Add to Minus One Extended, PPC32 p354)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(addme,rB_addr)\n");
            return False;
         }
         DIP("addme%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         // rD = rA + (-1) + XER[CA]
         // => Just another form of adde
         assign( old_xer_ca, getXER_CA32() );
         assign( rD, binop(Iop_Add32, mkexpr(rA),
                           binop(Iop_Add32, mkU32(-1), mkexpr(old_xer_ca)) ));
         set_XER_CA( PPC32G_FLAG_OP_ADDE,
                     mkexpr(rD), mkexpr(rA), mkU32(-1),
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADDE, 
                        mkexpr(rD), mkexpr(rA), mkU32(-1) );
         }
         break;
      }

      case 0x0CA: { // addze      (Add to Zero Extended, PPC32 p355)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(addze,rB_addr)\n");
            return False;
         }
         DIP("addze%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         // rD = rA + (0) + XER[CA]
         // => Just another form of adde
         assign( old_xer_ca, getXER_CA32() );
         assign( rD, binop(Iop_Add32, mkexpr(rA), mkexpr(old_xer_ca)) );
         set_XER_CA( PPC32G_FLAG_OP_ADDE, 
                     mkexpr(rD), mkexpr(rA), mkU32(0), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_ADDE, 
                        mkexpr(rD), mkexpr(rA), mkU32(0) );
         }
         break;
      }

      case 0x1EB: // divw       (Divide Word, PPC32 p388)
         DIP("divw%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop(Iop_DivS32, mkexpr(rA), mkexpr(rB)) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_DIVW, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         /* Note:
            if (0x8000_0000 / -1) or (x / 0)
            => rD=undef, if(flag_rC) CR7=undef, if(flag_OE) XER_OV=1
            => But _no_ exception raised. */
         break;

      case 0x1CB: // divwu      (Divide Word Unsigned, PPC32 p389)
         DIP("divwu%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         assign( rD, binop(Iop_DivU32, mkexpr(rA), mkexpr(rB)) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_DIVWU, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         /* Note: ditto comment divw, for (x / 0) */
         break;

      case 0x04B: // mulhw      (Multiply High Word, PPC32 p488)
         if (flag_OE != 0) {
            vex_printf("dis_int_arith(PPC32)(mulhw,flag_OE)\n");
            return False;
         }
         DIP("mulhw%s r%d,r%d,r%d\n", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         assign( res64, binop(Iop_MullS32, mkexpr(rA), mkexpr(rB)) );
         assign( rD, unop(Iop_64HIto32, mkexpr(res64)) );
         break;

      case 0x00B: // mulhwu     (Multiply High Word Unsigned, PPC32 p489)
         if (flag_OE != 0) {
            vex_printf("dis_int_arith(PPC32)(mulhwu,flag_OE)\n");
            return False;
         }
         DIP("mulhwu%s r%d,r%d,r%d\n", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         assign( res64, binop(Iop_MullU32, mkexpr(rA), mkexpr(rB)) );
         assign( rD, unop(Iop_64HIto32, mkexpr(res64)) );
         break;
         
      case 0x0EB: // mullw      (Multiply Low Word, PPC32 p491)
         DIP("mullw%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         assign( res64, binop(Iop_MullU32, mkexpr(rA), mkexpr(rB)) );
         assign( rD, unop(Iop_64to32, mkexpr(res64)) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_MULLW, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;

      case 0x068: // neg        (Negate, PPC32 p493)
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(neg,rB_addr)\n");
            return False;
         }
         DIP("neg%s%s r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr);
         // rD = (log not)rA + 1
         assign( rD, binop(Iop_Add32,
                           unop(Iop_Not32, mkexpr(rA)), mkU32(1)) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_NEG, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;

      case 0x028: // subf       (Subtract From, PPC32 p537)
         DIP("subf%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         // rD = rB - rA
         assign( rD, binop(Iop_Sub32, mkexpr(rB), mkexpr(rA)) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBF, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;

      case 0x008: // subfc      (Subtract from Carrying, PPC32 p538)
         DIP("subfc%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         // rD = rB - rA
         assign( rD, binop(Iop_Sub32, mkexpr(rB), mkexpr(rA)) );
         set_XER_CA( PPC32G_FLAG_OP_SUBFC, 
                     mkexpr(rD), mkexpr(rA), mkexpr(rB),
                     mkU32(0)/*old xer.ca, which is ignored*/ );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBFC, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
         
      case 0x088: {// subfe      (Subtract from Extended, PPC32 p539)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         DIP("subfe%s%s r%d,r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr, rB_addr);
         // rD = (log not)rA + rB + XER[CA]
         assign( old_xer_ca, getXER_CA32() );
         assign( rD, binop(Iop_Add32, unop(Iop_Not32, mkexpr(rA)),
                           binop(Iop_Add32, mkexpr(rB), mkexpr(old_xer_ca))) );
         set_XER_CA( PPC32G_FLAG_OP_SUBFE, 
                     mkexpr(rD), mkexpr(rA), mkexpr(rB), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBFE, 
                        mkexpr(rD), mkexpr(rA), mkexpr(rB) );
         }
         break;
      }

      case 0x0E8: { // subfme     (Subtract from Minus One Extended, PPC32 p541)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(subfme,rB_addr)\n");
            return False;
         }
         DIP("subfme%s%s r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr);
         // rD = (log not)rA + (-1) + XER[CA]
         // => Just another form of subfe
         assign( old_xer_ca, getXER_CA32() );
         assign( rD, binop(Iop_Add32, unop(Iop_Not32, mkexpr(rA)),
                           binop(Iop_Add32, mkU32(-1), mkexpr(old_xer_ca))) );
         set_XER_CA( PPC32G_FLAG_OP_SUBFE,
                     mkexpr(rD), mkexpr(rA), mkU32(-1),
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBFE, 
                        mkexpr(rD), mkexpr(rA), mkU32(-1) );
         }
         break;
      }

      case 0x0C8: { // subfze     (Subtract from Zero Extended, PPC32 p542)
         IRTemp old_xer_ca = newTemp(Ity_I32);
         if (rB_addr != 0) {
            vex_printf("dis_int_arith(PPC32)(subfze,rB_addr)\n");
            return False;
         }
         DIP("subfze%s%s r%d,r%d\n",
             flag_OE ? "o" : "", flag_rC ? "." : "",
             rD_addr, rA_addr);
         // rD = (log not)rA + (0) + XER[CA]
         // => Just another form of subfe
         assign( old_xer_ca, getXER_CA32() );
         assign( rD, binop(Iop_Add32,
                           unop(Iop_Not32, mkexpr(rA)), mkexpr(old_xer_ca)) );
         set_XER_CA( PPC32G_FLAG_OP_SUBFE,
                     mkexpr(rD), mkexpr(rA), mkU32(0), 
                     mkexpr(old_xer_ca) );
         if (flag_OE) {
            set_XER_OV( PPC32G_FLAG_OP_SUBFE,
                        mkexpr(rD), mkexpr(rA), mkU32(0) );
         }
         break;
      }

      default:
         vex_printf("dis_int_arith(PPC32)(opc2)\n");
         return False;
      }
      break;
   default:
      vex_printf("dis_int_arith(PPC32)(opc1)\n");
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
   UInt  UIMM_16 = ifieldUIMM16(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b0      = ifieldBIT0(theInstr);

   Int EXTS_SIMM = extend_s_16to32(UIMM_16);
   IRTemp rA     = newTemp(Ity_I32);
   IRTemp rB     = newTemp(Ity_I32);

   assign( rA, getIReg(rA_addr) );
   
   if (flag_L==1) {  // L==1 invalid for 32 bit.
      vex_printf("dis_int_cmp(PPC32)(flag_L)\n");
      return False;
   }
   
   if (b22 != 0) {
      vex_printf("dis_int_cmp(PPC32)(b22)\n");
      return False;
   }
   
   switch (opc1) {
      case 0x0B: // cmpi (Compare Immediate, PPC32 p368)
         DIP("cmp cr%d,r%d,%d\n", crfD, rA_addr, EXTS_SIMM);
         putCR321( crfD, unop(Iop_32to8,
                              binop(Iop_CmpORD32S, mkexpr(rA), 
                                                   mkU32(EXTS_SIMM))) );
         putCR0( crfD, getXER_SO() );
         break;

      case 0x0A: // cmpli (Compare Logical Immediate, PPC32 p370)
         DIP("cmpli cr%d,r%d,0x%x\n", crfD, rA_addr, UIMM_16);
         putCR321( crfD, unop(Iop_32to8,
                              binop(Iop_CmpORD32U, mkexpr(rA), 
                                                   mkU32(UIMM_16))) );
         putCR0( crfD, getXER_SO() );
         break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_cmp(PPC32)(0x1F,b0)\n");
         return False;
      }
      assign( rB, getIReg(rB_addr) );

      switch (opc2) {
         case 0x000: // cmp (Compare, PPC32 p367)
            DIP("cmp cr%d,r%d,r%d\n", crfD, rA_addr, rB_addr);
            putCR321( crfD, unop(Iop_32to8,
                      binop(Iop_CmpORD32S, mkexpr(rA), mkexpr(rB))) );
            putCR0( crfD, getXER_SO() );
            break;
         
         case 0x020: // cmpl (Compare Logical, PPC32 p369)
            DIP("cmpl cr%d,r%d,r%d\n", crfD, rA_addr, rB_addr);
            putCR321( crfD, unop(Iop_32to8,
                      binop(Iop_CmpORD32U, mkexpr(rA), mkexpr(rB))) );
            putCR0( crfD, getXER_SO() );
            break;

          default:
             vex_printf("dis_int_cmp(PPC32)(opc2)\n");
             return False;
      }
      break;

   default:
      vex_printf("dis_int_cmp(PPC32)(opc1)\n");
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
   UInt  UIMM_16 = ifieldUIMM16(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar flag_rC = ifieldBIT0(theInstr);
   
   Bool do_rc = False;
   
   IRTemp rS = newTemp(Ity_I32);
   IRTemp rA = newTemp(Ity_I32);
   IRTemp rB = newTemp(Ity_I32);
   IRExpr* irx;
   
   assign( rS, getIReg(rS_addr) );
   assign( rB, getIReg(rB_addr) );
   
   switch (opc1) {
   case 0x1C: // andi. (AND Immediate, PPC32 p358)
      DIP("andi. r%d,r%d,0x%x\n", rA_addr, rS_addr, UIMM_16);
      assign( rA, binop(Iop_And32, mkexpr(rS), mkU32(UIMM_16)) );
      do_rc = True;  // Always record to CR
      flag_rC = 1;
      break;
      
   case 0x1D: // andis. (AND Immediate Shifted, PPC32 p359)
      DIP("andis r%d,r%d,0x%x\n", rA_addr, rS_addr, UIMM_16);
      assign( rA, binop(Iop_And32, mkexpr(rS), mkU32(UIMM_16 << 16)) );
      do_rc = True;  // Always record to CR
      flag_rC = 1;
      break;

   case 0x18: // ori (OR Immediate, PPC32 p497)
      DIP("ori r%d,r%d,0x%x\n", rA_addr, rS_addr, UIMM_16);
      assign( rA, binop(Iop_Or32, mkexpr(rS), mkU32(UIMM_16)) );
      break;

   case 0x19: // oris (OR Immediate Shifted, PPC32 p498)
      DIP("oris r%d,r%d,0x%x\n", rA_addr, rS_addr, UIMM_16);
      assign( rA, binop(Iop_Or32, mkexpr(rS), mkU32(UIMM_16 << 16)) );
      break;

   case 0x1A: // xori (XOR Immediate, PPC32 p550)
      DIP("xori r%d,r%d,0x%x\n", rA_addr, rS_addr, UIMM_16);
      assign( rA, binop(Iop_Xor32, mkexpr(rS), mkU32(UIMM_16)) );
      break;

   case 0x1B: // xoris (XOR Immediate Shifted, PPC32 p551)
      DIP("xoris r%d,r%d,0x%x\n", rA_addr, rS_addr, UIMM_16);
      assign( rA, binop(Iop_Xor32, mkexpr(rS), mkU32(UIMM_16 << 16)) );
      break;

   /* X Form */
   case 0x1F:
      do_rc = True;    // All below record to CR

      switch (opc2) {
         case 0x01C: // and (AND, PPC32 p356)
            DIP("and%s r%d,r%d,r%d\n",
                flag_rC ? "." : "", rA_addr, rS_addr, rB_addr);
            assign(rA, binop(Iop_And32, mkexpr(rS), mkexpr(rB)));
            break;
         
         case 0x03C: // andc (AND with Complement, PPC32 p357)
            DIP("andc%s r%d,r%d,r%d\n",
                flag_rC ? "." : "", rA_addr, rS_addr, rB_addr);
            assign(rA, binop(Iop_And32, mkexpr(rS),
                             unop(Iop_Not32, mkexpr(rB))));
            break;
  
         case 0x01A: // cntlzw (Count Leading Zeros Word, PPC32 p371)
            if (rB_addr!=0) {
               vex_printf("dis_int_logic(PPC32)(cntlzw,rB_addr)\n");
               return False;
            }
            DIP("cntlzw%s r%d,r%d\n",
                flag_rC ? "." : "", rA_addr, rS_addr);
         
            // Iop_Clz32 undefined for arg==0, so deal with that case:
            irx =  binop(Iop_CmpNE32, mkexpr(rS), mkU32(0));
            assign(rA, IRExpr_Mux0X( unop(Iop_1Uto8, irx),
                                     mkU32(32),
                                     unop(Iop_Clz32, mkexpr(rS)) ));
            // alternatively: assign(rA, verbose_Clz32(rS));
            break;

      case 0x11C: // eqv (Equivalent, PPC32 p396)
         DIP("eqv%s r%d,r%d,r%d\n",
             flag_rC ? "." : "", rA_addr, rS_addr, rB_addr);
         assign( rA, unop(Iop_Not32, binop(Iop_Xor32,
                                           mkexpr(rS), mkexpr(rB))) );
         break;

      case 0x3BA: // extsb (Extend Sign Byte, PPC32 p397
         if (rB_addr!=0) {
            vex_printf("dis_int_logic(PPC32)(extsb,rB_addr)\n");
            return False;
         }
         DIP("extsb%s r%d,r%d\n",
             flag_rC ? "." : "", rA_addr, rS_addr);
         assign( rA, unop(Iop_8Sto32, unop(Iop_32to8, mkexpr(rS))) );
         break;

      case 0x39A: // extsh (Extend Sign Half Word, PPC32 p398)
         if (rB_addr!=0) {
            vex_printf("dis_int_logic(PPC32)(extsh,rB_addr)\n");
            return False;
         }
         DIP("extsh%s r%d,r%d\n",
             flag_rC ? "." : "", rA_addr, rS_addr);
         assign( rA, unop(Iop_16Sto32, unop(Iop_32to16, mkexpr(rS))) );
         break;

      case 0x1DC: // nand (NAND, PPC32 p492)
         DIP("nand%s r%d,r%d,r%d\n",
             flag_rC ? "." : "", rA_addr, rS_addr, rB_addr);
         assign( rA, unop(Iop_Not32,
                          binop(Iop_And32, mkexpr(rS), mkexpr(rB))) );
         break;
         
      case 0x07C: // nor (NOR, PPC32 p494)
         DIP("nor%s r%d,r%d,r%d\n",
             flag_rC ? "." : "", rA_addr, rS_addr, rB_addr);
         assign( rA, unop(Iop_Not32,
                          binop(Iop_Or32, mkexpr(rS), mkexpr(rB))) );
         break;

      case 0x1BC: // or (OR, PPC32 p495)
         if ((!flag_rC) && rS_addr == rB_addr) {
            DIP("mr r%d,r%d\n", rA_addr, rS_addr);
            assign( rA, mkexpr(rS) );
         } else {
            DIP("or%s r%d,r%d,r%d\n",
                flag_rC ? "." : "", rA_addr, rS_addr, rB_addr);
            assign( rA, binop(Iop_Or32, mkexpr(rS), mkexpr(rB)) );
         }
         break;

      case 0x19C: // orc  (OR with Complement, PPC32 p496)
         DIP("orc%s r%d,r%d,r%d\n",
             flag_rC ? "." : "", rA_addr, rS_addr, rB_addr);
         assign( rA, binop(Iop_Or32, mkexpr(rS),
                           unop(Iop_Not32, mkexpr(rB))) );
         break;
         
      case 0x13C: // xor (XOR, PPC32 p549)
         DIP("xor%s r%d,r%d,r%d\n",
             flag_rC ? "." : "", rA_addr, rS_addr, rB_addr);
         assign( rA, binop(Iop_Xor32, mkexpr(rS), mkexpr(rB)) );
         break;

      default:
         vex_printf("dis_int_logic(PPC32)(opc2)\n");
         return False;
      }
      break;
      
   default:
      vex_printf("dis_int_logic(PPC32)(opc1)\n");
      return False;
   }

   putIReg( rA_addr, mkexpr(rA) );

   if (do_rc && flag_rC) {
      set_CR0( mkexpr(rA) );
   }
   return True;
}



/*
  Integer Rotate Instructions
*/
static Bool dis_int_rot ( UInt theInstr )
{
   /* M-Form */
   UChar opc1      = ifieldOPC(theInstr);
   UChar rS_addr   = ifieldRegDS(theInstr);
   UChar rA_addr   = ifieldRegA(theInstr);
   UChar rB_addr   = ifieldRegB(theInstr);
   UChar sh_imm    = rB_addr;
   UChar MaskBegin = toUChar( IFIELD( theInstr, 6, 5 ) );
   UChar MaskEnd   = toUChar( IFIELD( theInstr, 1, 5 ) );
   UChar flag_rC   = ifieldBIT0(theInstr);

   UInt mask = MASK(31-MaskEnd, 31-MaskBegin);
   IRTemp rS = newTemp(Ity_I32);
   IRTemp rA = newTemp(Ity_I32);
   IRTemp rB = newTemp(Ity_I32);
   
   assign( rS, getIReg(rS_addr) );
   assign( rB, getIReg(rB_addr) );
      
   switch (opc1) {
   case 0x14: 
      // rlwimi (Rotate Left Word Immediate then Mask Insert, PPC32 p500)
      DIP("rlwimi%s r%d,r%d,%d,%d,%d\n", flag_rC ? "." : "",
          rA_addr, rS_addr, sh_imm, MaskBegin, MaskEnd);
      // rA = (ROTL(rS, Imm) & mask) | (rA & ~mask);
      assign( rA, binop(Iop_Or32,
                        binop(Iop_And32, mkU32(mask),
                              ROTL32(mkexpr(rS), mkU32(sh_imm))),
                        binop(Iop_And32, getIReg(rA_addr), mkU32(~mask))) );
      break;

   case 0x15: 
      // rlwinm (Rotate Left Word Immediate then AND with Mask, PPC32 p501)
      vassert(MaskBegin < 32);
      vassert(MaskEnd   < 32);
      vassert(sh_imm    < 32);

      if (MaskBegin == 0 && sh_imm+MaskEnd == 31) {
         /* Special-case the ,n,0,31-n form as that is just n-bit
            shift left (PPC32 p501) */
         DIP("slwi%s r%d,r%d,%d\n", flag_rC ? "." : "",
             rA_addr, rS_addr, sh_imm);
         assign( rA, binop(Iop_Shl32, mkexpr(rS), mkU8(sh_imm)) );
      }
      else
      if (MaskEnd == 31 && sh_imm+MaskBegin == 32) {
         /* Special-case the ,32-n,n,31 form as that is just n-bit
            unsigned shift right (PPC32 p501) */
         DIP("srwi%s r%d,r%d,%d\n", flag_rC ? "." : "",
             rA_addr, rS_addr, sh_imm);
         assign( rA, binop(Iop_Shr32, mkexpr(rS), mkU8(MaskBegin)) );
      }
      else {
         /* General case. */
         DIP("rlwinm%s r%d,r%d,%d,%d,%d\n", flag_rC ? "." : "",
             rA_addr, rS_addr, sh_imm, MaskBegin, MaskEnd);
         // rA = ROTL(rS, Imm) & mask
         assign( rA, binop(Iop_And32, ROTL32(mkexpr(rS), mkU32(sh_imm)), 
                                      mkU32(mask)) );
      }
      break;

   case 0x17: 
      // rlwnm (Rotate Left Word then AND with Mask, PPC32 p503
      DIP("rlwnm%s r%d,r%d,r%d,%d,%d\n", flag_rC ? "." : "",
          rA_addr, rS_addr, rB_addr, MaskBegin, MaskEnd);
      // rA = ROTL(rS, rB[0-4]) & mask
      // note, ROTL32 does the masking, so we don't do it here
      assign( rA, binop(Iop_And32, ROTL32(mkexpr(rS), mkexpr(rB)), 
                                   mkU32(mask)) );
      break;

   default:
      vex_printf("dis_int_rot(PPC32)(opc1)\n");
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
   /* D-Form, X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar rD_addr  = ifieldRegDS(theInstr);
   UChar rA_addr  = ifieldRegA(theInstr);
   Int   d_simm16 = ifieldSIMM16(theInstr);
   UChar rB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   IRTemp rA_or_0 = newTemp(Ity_I32);
   IRTemp EA_imm  = newTemp(Ity_I32);
   IRTemp EA_reg  = newTemp(Ity_I32);
   IRTemp rA      = newTemp(Ity_I32);
   IRTemp rB      = newTemp(Ity_I32);

   assign( rA, getIReg(rA_addr) );
   assign( rB, getIReg(rB_addr) );

   assign( rA_or_0, ea_rA_or_zero(rA_addr));

   assign( EA_imm, binop(Iop_Add32, mkexpr(rA_or_0), mkU32(d_simm16)) );
   
   switch (opc1) {
   case 0x22: // lbz (Load B & Zero, PPC32 p433)
      DIP("lbz r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
      putIReg( rD_addr, unop(Iop_8Uto32,
                             loadBE(Ity_I8, mkexpr(EA_imm))) );
      break;
      
   case 0x23: // lbzu (Load B & Zero with Update, PPC32 p434)
      if (rA_addr == 0 || rA_addr == rD_addr) {
         vex_printf("dis_int_load(PPC32)(lbzu,rA_addr|rD_addr)\n");
         return False;
      }
      DIP("lbzu r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
      putIReg( rD_addr, unop(Iop_8Uto32,
                             loadBE(Ity_I8, mkexpr(EA_imm))) );
      putIReg( rA_addr, mkexpr(EA_imm) );
      break;
      
   case 0x2A: // lha (Load HW Algebraic, PPC32 p445)
      DIP("lha r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
      putIReg( rD_addr, unop(Iop_16Sto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      break;

   case 0x2B: // lhau (Load HW Algebraic with Update, PPC32 p446)
      if (rA_addr == 0 || rA_addr == rD_addr) {
         vex_printf("dis_int_load(PPC32)(lhau,rA_addr|rD_addr)\n");
         return False;
      }
      DIP("lhau r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
      putIReg( rD_addr, unop(Iop_16Sto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      putIReg( rA_addr, mkexpr(EA_imm) );
      break;
      
   case 0x28: // lhz (Load HW & Zero, PPC32 p450)
      DIP("lhz r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
      putIReg( rD_addr, unop(Iop_16Uto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      break;
      
   case 0x29: // lhzu (Load HW & and Zero with Update, PPC32 p451)
      if (rA_addr == 0 || rA_addr == rD_addr) {
         vex_printf("dis_int_load(PPC32)(lhzu,rA_addr|rD_addr)\n");
         return False;
      }
      DIP("lhzu r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
      putIReg( rD_addr, unop(Iop_16Uto32,
                             loadBE(Ity_I16, mkexpr(EA_imm))) );
      putIReg( rA_addr, mkexpr(EA_imm) );
      break;

   case 0x20: // lwz (Load W & Zero, PPC32 p460)
      DIP("lwz r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
      putIReg( rD_addr, loadBE(Ity_I32, mkexpr(EA_imm)) );
      break;
      
   case 0x21: // lwzu (Load W & Zero with Update, PPC32 p461))
      if (rA_addr == 0 || rA_addr == rD_addr) {
         vex_printf("dis_int_load(PPC32)(lwzu,rA_addr|rD_addr)\n");
         return False;
      }
      DIP("lwzu r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
      putIReg( rD_addr, loadBE(Ity_I32, mkexpr(EA_imm)) );
      putIReg( rA_addr, mkexpr(EA_imm) );
      break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_load(PPC32)(Ox1F,b0)\n");
         return False;
      }
      assign( EA_reg, binop(Iop_Add32, mkexpr(rA_or_0), mkexpr(rB)) );

      switch (opc2) {
      case 0x077: // lbzux (Load B & Zero with Update Indexed, PPC32 p435)
         DIP("lbzux r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(PPC32)(lwzux,rA_addr|rD_addr)\n");
            return False;
         }
         putIReg( rD_addr, unop(Iop_8Uto32,
                                loadBE(Ity_I8, mkexpr(EA_reg))) );
         putIReg( rA_addr, mkexpr(EA_reg) );
         break;
         
      case 0x057: // lbzx (Load B & Zero Indexed, PPC32 p436)
         DIP("lbzx r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, unop(Iop_8Uto32,
                                loadBE(Ity_I8, mkexpr(EA_reg))) );
         break;
         
      case 0x177: // lhaux (Load HW Algebraic with Update Indexed, PPC32 p447)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(PPC32)(lhaux,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("lhaux r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, unop(Iop_16Sto32,
                                loadBE(Ity_I16, mkexpr(EA_reg))) );
         putIReg( rA_addr, mkexpr(EA_reg) );
         break;
         
      case 0x157: // lhax (Load HW Algebraic Indexed, PPC32 p448)
         DIP("lhax r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, unop(Iop_16Sto32,
                                loadBE(Ity_I16, mkexpr(EA_reg))) );
         break;
         
      case 0x137: // lhzux (Load HW & Zero with Update Indexed, PPC32 p452)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(PPC32)(lhzux,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("lhzux r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, unop(Iop_16Uto32,
                                loadBE(Ity_I16, mkexpr(EA_reg))) );
         putIReg( rA_addr, mkexpr(EA_reg) );
         break;
         
      case 0x117: // lhzx (Load HW & Zero Indexed, PPC32 p453)
         DIP("lhzx r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, unop(Iop_16Uto32,
                                loadBE(Ity_I16, mkexpr(EA_reg))) );
         break;

      case 0x037: // lwzux (Load W & Zero with Update Indexed, PPC32 p462)
         if (rA_addr == 0 || rA_addr == rD_addr) {
            vex_printf("dis_int_load(PPC32)(lwzux,rA_addr|rD_addr)\n");
            return False;
         }
         DIP("lwzux r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, loadBE(Ity_I32, mkexpr(EA_reg)) );
         putIReg( rA_addr, mkexpr(EA_reg) );
         break;
         
      case 0x017: // lwzx (Load W & Zero Indexed, PPC32 p463)
         DIP("lwzx r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         putIReg( rD_addr, loadBE(Ity_I32, mkexpr(EA_reg)) );
         break;

      default:
         vex_printf("dis_int_load(PPC32)(opc2)\n");
         return False;
      }
      break;
   default:
      vex_printf("dis_int_load(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Store Instructions
*/
static Bool dis_int_store ( UInt theInstr )
{
   /* D-Form, X-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UInt  rS_addr = ifieldRegDS(theInstr);
   UInt  rA_addr = ifieldRegA(theInstr);
   Int   simm16  = ifieldSIMM16(theInstr);
   UInt  rB_addr = ifieldRegB(theInstr);
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar b0      = ifieldBIT0(theInstr);

   IRTemp rA_or_0 = newTemp(Ity_I32);
   IRTemp rB      = newTemp(Ity_I32);
   IRTemp rS      = newTemp(Ity_I32);
   IRTemp EA_imm  = newTemp(Ity_I32);
   IRTemp EA_reg  = newTemp(Ity_I32);
   
   assign( rB, getIReg(rB_addr) );
   assign( rS, getIReg(rS_addr) );
   
   assign( rA_or_0, ea_rA_or_zero(rA_addr) );
   assign( EA_imm, binop(Iop_Add32, mkexpr(rA_or_0), mkU32(simm16)) );
   
   switch (opc1) {
   case 0x26: // stb (Store B, PPC32 p509)
      DIP("stb r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      storeBE( mkexpr(EA_imm), unop(Iop_32to8, mkexpr(rS)) );
      break;
       
   case 0x27: // stbu (Store B with Update, PPC32 p510)
      if (rA_addr == 0 ) {
         vex_printf("dis_int_store(PPC32)(stbu,rA_addr)\n");
         return False;
      }
      DIP("stbu r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      putIReg( rA_addr, mkexpr(EA_imm) );
      storeBE( mkexpr(EA_imm), unop(Iop_32to8, mkexpr(rS)) );
      break;

   case 0x2C: // sth (Store HW, PPC32 p522)
      DIP("sth r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      storeBE( mkexpr(EA_imm), unop(Iop_32to16, mkexpr(rS)) );
      break;
      
   case 0x2D: // sthu (Store HW with Update, PPC32 p524)
      if (rA_addr == 0) {
         vex_printf("dis_int_store(PPC32)(sthu,rA_addr)\n");
         return False;
      }
      DIP("sthu r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      putIReg( rA_addr, mkexpr(EA_imm) );
      storeBE( mkexpr(EA_imm), unop(Iop_32to16, mkexpr(rS)) );
      break;

   case 0x24: // stw (Store W, PPC32 p530)
      DIP("stw r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      storeBE( mkexpr(EA_imm), mkexpr(rS) );
      break;

   case 0x25: // stwu (Store W with Update, PPC32 p534)
      if (rA_addr == 0) {
         vex_printf("dis_int_store(PPC32)(stwu,rA_addr)\n");
         return False;
      }
      DIP("stwu r%u,%d(r%u)\n", rS_addr, simm16, rA_addr);
      putIReg( rA_addr, mkexpr(EA_imm) );
      storeBE( mkexpr(EA_imm), mkexpr(rS) );
      break;
      
   /* X Form */
   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_int_store(PPC32)(0x1F,b0)\n");
         return False;
      }
      assign( EA_reg, binop(Iop_Add32, mkexpr(rA_or_0), mkexpr(rB)) );

      switch (opc2) {
      case 0x0F7: // stbux (Store B with Update Indexed, PPC32 p511)
         if (rA_addr == 0) {
            vex_printf("dis_int_store(PPC32)(stbux,rA_addr)\n");
            return False;
         }
         DIP("stbux r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         putIReg( rA_addr, mkexpr(EA_reg) );
         storeBE( mkexpr(EA_reg), unop(Iop_32to8, mkexpr(rS)) );
         break;
         
      case 0x0D7: // stbx (Store B Indexed, PPC32 p512)
         DIP("stbx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         storeBE( mkexpr(EA_reg), unop(Iop_32to8, mkexpr(rS)) );
         break;
         
      case 0x1B7: // sthux (Store HW with Update Indexed, PPC32 p525)
         if (rA_addr == 0) {
            vex_printf("dis_int_store(PPC32)(sthux,rA_addr)\n");
            return False;
         }
         DIP("sthux r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         putIReg( rA_addr, mkexpr(EA_reg) );
         storeBE( mkexpr(EA_reg), unop(Iop_32to16, mkexpr(rS)) );
         break;
         
      case 0x197: // sthx (Store HW Indexed, PPC32 p526)
         DIP("sthx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         storeBE( mkexpr(EA_reg), unop(Iop_32to16, mkexpr(rS)) );
         break;
         
      case 0x0B7: // stwux (Store W with Update Indexed, PPC32 p535)
         if (rA_addr == 0) {
            vex_printf("dis_int_store(PPC32)(stwux,rA_addr)\n");
            return False;
         }
         DIP("stwux r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         putIReg( rA_addr, mkexpr(EA_reg) );
         storeBE( mkexpr(EA_reg), mkexpr(rS) );
         break;

      case 0x097: // stwx (Store W Indexed, PPC32 p536)
         DIP("stwx r%u,r%u,r%u\n", rS_addr, rA_addr, rB_addr);
         storeBE( mkexpr(EA_reg), mkexpr(rS) );
         break;
         
      default:
         vex_printf("dis_int_store(PPC32)(opc2)\n");
         return False;
      }
      break;
   default:
      vex_printf("dis_int_store(PPC32)(opc1)\n");
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
   Int   d_simm16 = ifieldSIMM16(theInstr);

   UInt reg_idx = 0;
   UInt offset  = 0;
   IRTemp rA    = newTemp(Ity_I32);
   IRTemp EA    = newTemp(Ity_I32);
   IRExpr* irx_addr;

   if (rA_addr == 0) {
      assign( EA, binop(Iop_Add32, mkU32(0), mkU32(d_simm16)) );
   } else {
      assign( rA, getIReg(rA_addr) );
      assign( EA, binop(Iop_Add32, mkexpr(rA), mkU32(d_simm16)) );
   }
   
   switch (opc1) {
      case 0x2E: // lmw (Load Multiple Word, PPC32 p454)
         if (rA_addr >= rD_addr) {
            vex_printf("dis_int_ldst_mult(PPC32)(lmw,rA_addr)\n");
            return False;
         }
         DIP("lmw r%d,%d(r%d)\n", rD_addr, d_simm16, rA_addr);
         for (reg_idx = rD_addr; reg_idx <= 31; reg_idx++) {
            irx_addr = binop(Iop_Add32, mkexpr(EA), mkU32(offset));
            putIReg( reg_idx, loadBE(Ity_I32, irx_addr ) );
            offset += 4;
         }
         break;
      
      case 0x2F: // stmw (Store Multiple Word, PPC32 p527)
         DIP("stmw r%d,%d(r%d)\n", rS_addr, d_simm16, rA_addr);
         for (reg_idx = rS_addr; reg_idx <= 31; reg_idx++) {
            irx_addr = binop(Iop_Add32, mkexpr(EA), mkU32(offset));
            storeBE( irx_addr, getIReg(reg_idx) );
            offset += 4;
         }
         break;
      
      default:
         vex_printf("dis_int_ldst_mult(PPC32)(opc1)\n");
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
                             Int    maxBytes,  // 32 or 128
                             Addr32 NIA )      // where next?
{
   Int     i, shift = 24;
   IRExpr* e_nbytes = mkexpr(tNBytes);
   IRExpr* e_EA = mkexpr(EA);

   vassert(rD >= 0 && rD < 32);
   rD--; if (rD < 0) rD = 31;

   for (i = 0; i < maxBytes; i++) {

      /* if (nBytes < (i+1)) goto NIA; */
      stmt( IRStmt_Exit( binop(Iop_CmpLT32U, e_nbytes, mkU32(i+1)),
                         Ijk_Boring, 
                         IRConst_U32(NIA)) );
      /* when crossing into a new dest register, set it to zero. */
      if ((i % 4) == 0) {
         rD++; if (rD == 32) rD = 0;
         putIReg(rD, mkU32(0));
         shift = 24;
      }
      /* rD |=  (8Uto32(*(EA+i))) << shift */
      vassert(shift == 0 || shift == 8 || shift == 16 || shift == 24);
      putIReg(
         rD, 
         binop(Iop_Or32, 
               getIReg(rD),
               binop(Iop_Shl32, 
                     unop(Iop_8Uto32, 
                          loadBE(Ity_I8, 
                                 binop(Iop_Add32, e_EA, mkU32(i)))), 
                     mkU8(toUChar(shift))) 
      ));
      shift -= 8;
   }
}

static 
void generate_stsw_sequence ( IRTemp tNBytes,   // # bytes, :: Ity_I32
                              IRTemp EA,        // EA
                              Int    rS,        // first src register
                              Int    maxBytes,  // 32 or 128
                              Addr32 NIA )      // where next?
{
   Int     i, shift = 24;
   IRExpr* e_nbytes = mkexpr(tNBytes);
   IRExpr* e_EA = mkexpr(EA);

   vassert(rS >= 0 && rS < 32);
   rS--; if (rS < 0) rS = 31;

   for (i = 0; i < maxBytes; i++) {
      /* if (nBytes < (i+1)) goto NIA; */
      stmt( IRStmt_Exit( binop(Iop_CmpLT32U, e_nbytes, mkU32(i+1)),
                         Ijk_Boring, 
                         IRConst_U32(NIA)) );
      /* check for crossing into a new src register. */
      if ((i % 4) == 0) {
         rS++; if (rS == 32) rS = 0;
         shift = 24;
      }
      /* *(EA+i) = 32to8(rS >> shift) */
      vassert(shift == 0 || shift == 8 || shift == 16 || shift == 24);
      storeBE(
         binop(Iop_Add32, e_EA, mkU32(i)),
         unop(Iop_32to8,
              binop(Iop_Shr32, getIReg(rS), mkU8(toUChar(shift))))
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

   IRTemp t_EA     = newTemp(Ity_I32);
   IRTemp t_nbytes = IRTemp_INVALID;

   *stopHere = False;

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_int_ldst_str(PPC32)(opc1)\n");
      return False;
   }

   switch (opc2) {
   case 0x255: // lswi (Load String Word Immediate, PPC32 p455)
      /* NB: does not reject the case where RA is in the range of
         registers to be loaded.  It should. */
      DIP("lswi r%d,r%d,%d\n", rD_addr, rA_addr, NumBytes);
      assign( t_EA, ea_rA_or_zero(rA_addr) );
      if (NumBytes == 8) {
         /* Special case hack */
         /* rD = Mem[EA]; (rD+1)%32 = Mem[EA+4] */
         putIReg( rD_addr,          
                  loadBE(Ity_I32, mkexpr(t_EA)) );
         putIReg( (rD_addr+1) % 32, 
                  loadBE(Ity_I32, binop(Iop_Add32, mkexpr(t_EA), mkU32(4))) );
      } else {
         t_nbytes = newTemp(Ity_I32);
         assign( t_nbytes, mkU32(NumBytes==0 ? 32 : NumBytes) );
         generate_lsw_sequence( t_nbytes, t_EA, rD_addr, 
                                32, guest_CIA_curr_instr+4 );
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
      DIP("lswx r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
      t_nbytes = newTemp(Ity_I32);
      assign( t_EA, ea_standard(rA_addr,rB_addr) );
      assign( t_nbytes, unop( Iop_8Uto32, getXER_BC() ) );
      generate_lsw_sequence( t_nbytes, t_EA, rD_addr, 
                             128, guest_CIA_curr_instr+4 );
      *stopHere = True;
      return True;

   case 0x2D5: // stswi (Store String Word Immediate, PPC32 p528)
      DIP("stswi r%d,r%d,%d\n", rS_addr, rA_addr, NumBytes);
      assign( t_EA, ea_rA_or_zero(rA_addr) );
      if (NumBytes == 8) {
         /* Special case hack */
         /* Mem[EA] = rD; Mem[EA+4] = (rD+1)%32 */
         storeBE( mkexpr(t_EA), 
                  getIReg(rD_addr) );
         storeBE( binop(Iop_Add32, mkexpr(t_EA), mkU32(4)), 
                  getIReg((rD_addr+1) % 32) );
      } else {
         t_nbytes = newTemp(Ity_I32);
         assign( t_nbytes, mkU32(NumBytes==0 ? 32 : NumBytes) );
         generate_stsw_sequence( t_nbytes, t_EA, rD_addr, 
                                 32, guest_CIA_curr_instr+4 );
         *stopHere = True;
      }
      return True;

   case 0x295: // stswx (Store String Word Indexed, PPC32 p529)
      DIP("stswx r%d,r%d,r%d\n", rS_addr, rA_addr, rB_addr);
      t_nbytes = newTemp(Ity_I32);
      assign( t_EA, ea_standard(rA_addr,rB_addr) );
      assign( t_nbytes, unop( Iop_8Uto32, getXER_BC() ) );
      generate_stsw_sequence( t_nbytes, t_EA, rS_addr, 
                              128, guest_CIA_curr_instr+4 );
      *stopHere = True;
      return True;

   default:
      vex_printf("dis_int_ldst_str(PPC32)(opc2)\n");
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
   IRTemp ok = newTemp(Ity_I32);

   if ((BO >> 2) & 1) {
      assign( ok, mkU32(0xFFFFFFFF) );
   } else {
      if ((BO >> 1) & 1) {
         assign( ok, unop( Iop_1Sto32, 
                           binop( Iop_CmpEQ32, 
                                  getSPR( PPC32_SPR_CTR ), mkU32(0))) );
      } else {
         assign( ok, unop( Iop_1Sto32,
                           binop( Iop_CmpNE32, 
                                  getSPR( PPC32_SPR_CTR ), mkU32(0))) );
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
         assign( res, binop(Iop_Xor32, mkexpr(cr_bi), mkU32(1<<where)) );
      }
   }
   return mkexpr(res);
}


/*
  Integer Branch Instructions
*/
static Bool dis_branch ( UInt theInstr, 
                         /*OUT*/DisResult* dres,
                         Bool (*resteerOkFn)(Addr64) )
{
   UChar opc1     = ifieldOPC(theInstr);
   UChar BO       = ifieldRegDS(theInstr);
   UChar BI       = ifieldRegA(theInstr);
   Int   BD_s16   = ifieldSIMM16(theInstr) & 0xFFFFFFFC; /* mask off */
   UChar b11to15  = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   Int   LI_s26   = ifieldSIMM26(theInstr) & 0xFFFFFFFC; /* mask off */
   UChar flag_AA  = ifieldBIT1(theInstr);
   UChar flag_LK  = ifieldBIT0(theInstr);
   
   Addr32 nia = 0;
   
   IRTemp ir_nia    = newTemp(Ity_I32);
   IRTemp do_branch = newTemp(Ity_I32);
   IRTemp ctr_ok    = newTemp(Ity_I32);
   IRTemp cond_ok   = newTemp(Ity_I32);
   
   /* Hack to pass through code that just wants to read the PC */
   if (theInstr == 0x429F0005) {
      DIP("bcl 0x%x, 0x%x (a.k.a mr lr,cia+4)\n", BO, BI);
      putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
      return True;
   }

   /* The default what-next.  Individual cases can override it. */    
   dres->whatNext = Dis_StopHere;

   switch (opc1) {
   case 0x12: // b     (Branch, PPC32 p360)
      if (flag_AA) {
         nia = (UInt)LI_s26;
      } else {
         nia = (UInt)((Int)guest_CIA_curr_instr + LI_s26);
      }
      DIP("b%s%s 0x%x\n", flag_LK ? "l" : "", flag_AA ? "a" : "", nia);

      if (flag_LK) {
         putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
      }

      if (resteerOkFn((Addr64)nia)) {
         dres->whatNext = Dis_Resteer;
         dres->continueAt = (Addr64)nia;
      } else {
         irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
         irbb->next     = mkU32(nia);
      }
      break;
      
   case 0x10: // bc    (Branch Conditional, PPC32 p361)
      DIP("bc%s%s 0x%x, 0x%x, 0x%x\n",
          flag_LK ? "l" : "", flag_AA ? "a" : "", BO, BI, BD_s16);
      
      if (!(BO & 0x4)) {
         putSPR( PPC32_SPR_CTR,
                 binop(Iop_Sub32, getSPR( PPC32_SPR_CTR ), mkU32(1)) );
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
         nia = (UInt)BD_s16;
      } else {
         nia = (UInt)((Int)guest_CIA_curr_instr + BD_s16);
      }
      if (flag_LK) {
         putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
      }
      
      stmt( IRStmt_Exit( binop(Iop_CmpNE32, mkexpr(do_branch), mkU32(0)),
                         flag_LK ? Ijk_Call : Ijk_Boring,
                         IRConst_U32(nia) ));
      
      irbb->jumpkind = Ijk_Boring;
      irbb->next     = mkU32(guest_CIA_curr_instr + 4);
      break;
      
   case 0x13:
      if (b11to15!=0) {
         vex_printf("dis_int_branch(PPC32)(0x13,b11to15)\n");
         return False;
      }

      switch (opc2) {
      case 0x210: // bcctr (Branch Cond. to Count Register, PPC32 p363) 
         if ((BO & 0x4) == 0) { // "decrement and test CTR" option invalid
            vex_printf("dis_int_branch(PPC32)(bcctr,BO)\n");
            return False;
         }
         DIP("bcctr%s 0x%x, 0x%x\n", flag_LK ? "l" : "", BO, BI);
         
         assign( cond_ok, branch_cond_ok( BO, BI ) );
         
         assign( ir_nia,
                 binop(Iop_And32, mkU32(0xFFFFFFFC), 
                                  getSPR( PPC32_SPR_CTR ) ));
         
         if (flag_LK) {
            putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
         }
         
         stmt( IRStmt_Exit(
                  binop(Iop_CmpEQ32, mkexpr(cond_ok), mkU32(0)),
                  Ijk_Boring,
                  IRConst_U32(guest_CIA_curr_instr + 4) 
             ));
         
         irbb->jumpkind = flag_LK ? Ijk_Call : Ijk_Boring;
         irbb->next     = mkexpr(ir_nia);
         break;
         
      case 0x010: // bclr (Branch Cond. to Link Register, PPC32 p365) 

         if ((BO & 0x14 /* 1z1zz */) == 0x14 && flag_LK == 0) {
            DIP("blr\n");
         } else {
            DIP("bclr%s 0x%x, 0x%x\n", flag_LK ? "l" : "", BO, BI);
         }

         if (!(BO & 0x4)) {
            putSPR( PPC32_SPR_CTR, 
                    binop(Iop_Sub32, getSPR( PPC32_SPR_CTR ), mkU32(1)) );
         }
         
         /* See comments above for 'bc' about this */
         assign( ctr_ok,  branch_ctr_ok( BO ) );
         assign( cond_ok, branch_cond_ok( BO, BI ) );
         assign( do_branch,
                 binop(Iop_And32, mkexpr(cond_ok), mkexpr(ctr_ok)) );
         
         assign( ir_nia, binop(Iop_And32,
                               getSPR( PPC32_SPR_LR ),
                               mkU32(0xFFFFFFFC)) );
         if (flag_LK) {
            putSPR( PPC32_SPR_LR, mkU32(guest_CIA_curr_instr + 4) );
         }

         stmt( IRStmt_Exit(
                  binop(Iop_CmpEQ32, mkexpr(do_branch), mkU32(0)),
                  Ijk_Boring,
                  IRConst_U32(guest_CIA_curr_instr + 4)
             ));

         /* blrl is pretty strange; it's like a return that sets the
            return address of its caller to the insn following this
            one.  Mark it as a return. */
         irbb->jumpkind = Ijk_Ret;  /* was flag_LK ? Ijk_Call : Ijk_Ret; */
         irbb->next     = mkexpr(ir_nia);
         break;
         
      default:
         vex_printf("dis_int_branch(PPC32)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_int_branch(PPC32)(opc1)\n");
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

   IRTemp crbD = newTemp(Ity_I32);
   IRTemp crbA = newTemp(Ity_I32);
   IRTemp crbB = newTemp(Ity_I32);

   if (opc1 != 19 || b0 != 0) {
      vex_printf("dis_cond_logic(PPC32)(opc1)\n");
      return False;
   }

   if (opc2 == 0) {  // mcrf    (Move Cond Reg Field, PPC32 p464)
      if (((crbD_addr & 0x3) != 0) ||
          ((crbA_addr & 0x3) != 0) || (crbB_addr != 0)) {
         vex_printf("dis_cond_logic(PPC32)(crbD|crbA|crbB != 0)\n");
         return False;
      }
      DIP("mcrf cr%d,cr%d\n", crfD_addr, crfS_addr);
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
         vex_printf("dis_cond_logic(PPC32)(opc2)\n");
         return False;
      }

      putCRbit( crbD_addr, mkexpr(crbD) );
   }
   return True;
}


/*
  System Linkage Instructions
*/
static Bool dis_syslink ( UInt theInstr, DisResult* dres )
{
   if (theInstr != 0x44000002) {
      vex_printf("dis_int_syslink(PPC32)(theInstr)\n");
      return False;
   }

   // sc  (System Call, PPC32 p504)
   DIP("sc\n");
   
   /* It's important that all ArchRegs carry their up-to-date value
      at this point.  So we declare an end-of-block here, which
      forces any TempRegs caching ArchRegs to be flushed. */
   irbb->next     = mkU32( guest_CIA_curr_instr + 4 );
   irbb->jumpkind = Ijk_Sys_syscall;
   
   dres->whatNext = Dis_StopHere;
   return True;
}


/*
  Memory Synchronization Instructions
*/
static Bool dis_memsync ( UInt theInstr )
{
   /* X-Form, XL-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UInt  b11to25  = IFIELD(theInstr, 11, 15);
   UChar rD_addr  = ifieldRegDS(theInstr);
   UChar rS_addr  = rD_addr;
   UChar rA_addr  = ifieldRegA(theInstr);
   UChar rB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   IRTemp EA = newTemp(Ity_I32);
   IRTemp rS = newTemp(Ity_I32);
   
   switch (opc1) {
   /* XL-Form */
   case 0x13:   // isync (Instruction Synchronize, PPC32 p432)
      if (opc2 != 0x096) {
         vex_printf("dis_int_memsync(PPC32)(0x13,opc2)\n");
         return False;
      }
      if (b11to25 != 0 || b0 != 0) {
         vex_printf("dis_int_memsync(PPC32)(0x13,b11to25|b0)\n");
         return False;
      }
      DIP("isync\n");
      stmt( IRStmt_MFence() );
      break;

   /* X-Form */
   case 0x1F:
      switch (opc2) {
      case 0x356: // eieio (Enforce In-Order Execution of I/O, PPC32 p394)
         if (b11to25 != 0 || b0 != 0) {
            vex_printf("dis_int_memsync(PPC32)(eiei0,b11to25|b0)\n");
            return False;
         }
         DIP("eieio\n");
         /* Insert a memory fence, just to be on the safe side. */
         stmt( IRStmt_MFence() );
         break;

      case 0x014: // lwarx (Load Word and Reserve Indexed, PPC32 p458)
         if (b0 != 0) {
            vex_printf("dis_int_memsync(PPC32)(lwarx,b0)\n");
            return False;
         }
         DIP("lwarx r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         assign( EA, ea_standard(rA_addr, rB_addr) );
         putIReg( rD_addr, loadBE(Ity_I32, mkexpr(EA)) );
         /* Take a reservation */
         stmt( IRStmt_Put( OFFB_RESVN, mkexpr(EA) ));
         break;
         
      case 0x096: { 
         // stwcx. (Store Word Conditional Indexed, PPC32 p532)
         IRTemp resaddr = newTemp(Ity_I32);
         if (b0 != 1) {
            vex_printf("dis_int_memsync(PPC32)(stwcx.,b0)\n");
            return False;
         }
         DIP("stwcx. r%d,r%d,r%d\n", rS_addr, rA_addr, rB_addr);
         assign( rS, getIReg(rS_addr) );
         assign( EA, ea_standard(rA_addr, rB_addr) );

         /* First set up as if the reservation failed */
         // Set CR0[LT GT EQ S0] = 0b000 || XER[SO]
         putCR321(0, mkU8(0<<1));
         putCR0(0, getXER_SO());

         /* Get the reservation address into a temporary, then
            clear it. */
         assign( resaddr, IRExpr_Get(OFFB_RESVN, Ity_I32) );
         stmt( IRStmt_Put( OFFB_RESVN, mkU32(0) ));

         /* Skip the rest if the reservation really did fail. */
         stmt( IRStmt_Exit(
                  binop(Iop_CmpNE32, mkexpr(resaddr),
                                     mkexpr(EA)),
                  Ijk_Boring,
                  IRConst_U32(guest_CIA_curr_instr + 4)) );

         /* Success?  Do the store */
         storeBE( mkexpr(EA), mkexpr(rS) );
         
         // Set CR0[LT GT EQ S0] = 0b001 || XER[SO]
         putCR321(0, mkU8(1<<1));
         break;
      }

      case 0x256: // sync (Synchronize, PPC32 p543)
         if (b11to25 != 0 || b0 != 0) {
            vex_printf("dis_int_memsync(PPC32)(sync,b11to25|b0)\n");
            return False;
         }
         DIP("sync\n");
         /* Insert a memory fence.  It's sometimes important that these
            are carried through to the generated code. */
         stmt( IRStmt_MFence() );
         break;
         
      default:
         vex_printf("dis_int_memsync(PPC32)(opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_int_memsync(PPC32)(opc1)\n");
      return False;
   }
   return True;
}



/*
  Integer Shift Instructions
*/
static Bool dis_int_shift ( UInt theInstr )
{
   /* X-Form */
   UChar opc1    = ifieldOPC(theInstr);
   UChar rS_addr = ifieldRegDS(theInstr);
   UChar rA_addr = ifieldRegA(theInstr);
   UChar rB_addr = ifieldRegB(theInstr);
   UChar sh_imm  = rB_addr;
   UInt  opc2    = ifieldOPClo10(theInstr);
   UChar flag_rC = ifieldBIT0(theInstr);

   IRTemp sh_amt = newTemp(Ity_I8);
   IRTemp rS     = newTemp(Ity_I32);
   IRTemp rA     = newTemp(Ity_I32);
   IRTemp rB     = newTemp(Ity_I32);
   IRTemp sh_amt32   = newTemp(Ity_I32);
   IRTemp outofrange = newTemp(Ity_I8);
   
   assign( rS, getIReg(rS_addr) );
   assign( rB, getIReg(rB_addr) );
   
   if (opc1 == 0x1F) {
      switch (opc2) {
      case 0x018: // slw (Shift Left Word, PPC32 p505)
         DIP("slw%s r%d,r%d,r%d\n", flag_rC ? "." : "",
             rA_addr, rS_addr, rB_addr);
         /* rA = rS << rB */
         /* ppc32 semantics are: 
            slw(x,y) = (x << (y & 31))         -- primary result
                       & ~((y << 26) >>s 31)   -- make result 0 
                                                  for y in 32 .. 63
         */
         assign( rA,
            binop(
               Iop_And32,
               binop( Iop_Shl32, 
                      mkexpr(rS), 
                      unop( Iop_32to8, 
                            binop(Iop_And32, mkexpr(rB), mkU32(31)))),
               unop( Iop_Not32, 
                     binop( Iop_Sar32, 
                            binop(Iop_Shl32, mkexpr(rB), mkU8(26)), 
                            mkU8(31)))) );
         break;
         
      case 0x318: // sraw (Shift Right Algebraic Word, PPC32 p506)
         DIP("sraw%s r%d,r%d,r%d\n", flag_rC ? "." : "",
             rA_addr, rS_addr, rB_addr);
         
         /* JRS: my reading of the (poorly worded) PPC32 doc p506 is:
            amt = rB & 63
            rA = Sar32( rS, amt > 31 ? 31 : amt )
            XER.CA = amt > 31 ? sign-of-rS : (computation as per srawi)
         */
         assign( sh_amt32, binop(Iop_And32, mkU32(0x3F), mkexpr(rB)) );
         assign( outofrange,
                 unop( Iop_1Uto8, 
                       binop(Iop_CmpLT32U, mkU32(31), mkexpr(sh_amt32)) ));
         assign( rA,
                 binop( Iop_Sar32, 
                        mkexpr(rS), 
                        unop( Iop_32to8, 
                              IRExpr_Mux0X( mkexpr(outofrange), 
                                            mkexpr(sh_amt32), 
                                            mkU32(31)) ))
               );
         set_XER_CA( PPC32G_FLAG_OP_SRAW,
                     mkexpr(rA), mkexpr(rS), mkexpr(sh_amt32),
                     getXER_CA32() );
         break;
         
      case 0x338: // srawi (Shift Right Algebraic Word Immediate, PPC32 p507)
         DIP("srawi%s r%d,r%d,%d\n", flag_rC ? "." : "",
             rA_addr, rS_addr, sh_imm);
         vassert(sh_imm < 32);
         assign( sh_amt, mkU8(sh_imm) );
         assign( rA, binop(Iop_Sar32, mkexpr(rS), mkexpr(sh_amt)) );
         set_XER_CA( PPC32G_FLAG_OP_SRAWI, 
                     mkexpr(rA), mkexpr(rS), mkU32(sh_imm), 
                     getXER_CA32() );
         break;
      
      case 0x218: // srw (Shift Right Word, PPC32 p508)
         DIP("srw%s r%d,r%d,r%d\n", flag_rC ? "." : "",
             rA_addr, rS_addr, rB_addr);
         /* rA = rS >>u rB */
         /* ppc32 semantics are: 
            slw(x,y) = (x >>u (y & 31))        -- primary result
                       & ~((y << 26) >>s 31)   -- make result 0 
                                                  for y in 32 .. 63
         */
         assign( rA,
            binop(
               Iop_And32,
               binop( Iop_Shr32, 
                      mkexpr(rS), 
                      unop( Iop_32to8, 
                            binop(Iop_And32, mkexpr(rB), mkU32(31)))),
               unop( Iop_Not32, 
                     binop( Iop_Sar32, 
                            binop(Iop_Shl32, mkexpr(rB), mkU8(26)), 
                            mkU8(31)))) );
         break;
         
      default:
         vex_printf("dis_int_shift(PPC32)(opc2)\n");
         return False;
      }
   } else {
      vex_printf("dis_int_shift(PPC32)(opc1)\n");
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
static IRExpr* /* :: Ity_I32 */ gen_byterev32 ( IRTemp t )
{
   vassert(typeOfIRTemp(irbb->tyenv, t) == Ity_I32);
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

   IRTemp EA = newTemp(Ity_I32);
   IRTemp w1 = newTemp(Ity_I32);
   IRTemp w2 = newTemp(Ity_I32);

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_int_ldst_rev(PPC32)(opc1|b0)\n");
      return False;
   }

   assign( EA, ea_standard(rA_addr, rB_addr) );
   
   switch (opc2) {
//zz    case 0x316: // lhbrx (Load Half Word Byte-Reverse Indexed, PPC32 p449)
//zz vassert(0);
//zz 
//zz       DIP("lhbrx r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
//zz       assign( byte0, loadBE(Ity_I8, mkexpr(EA)) );
//zz       assign( byte1, loadBE(Ity_I8, binop(Iop_Add32, mkexpr(EA),mkU32(1))) );
//zz       assign( rD, binop(Iop_Or32,
//zz                         binop(Iop_Shl32, mkexpr(byte1), mkU8(8)),
//zz                         mkexpr(byte0)) );
//zz       putIReg( rD_addr, mkexpr(rD));
//zz       break;
       
      case 0x216: // lwbrx (Load Word Byte-Reverse Indexed, PPC32 p459)
         DIP("lwbrx r%d,r%d,r%d\n", rD_addr, rA_addr, rB_addr);
         assign( w1, loadBE(Ity_I32, mkexpr(EA)) );
         assign( w2, gen_byterev32(w1) );
         putIReg( rD_addr, mkexpr(w2));
         break;
      
//zz    case 0x396: // sthbrx (Store Half Word Byte-Reverse Indexed, PPC32 p523)
//zz vassert(0);
//zz 
//zz       DIP("sthbrx r%d,r%d,r%d\n", rS_addr, rA_addr, rB_addr);
//zz       assign( rS, getIReg(rS_addr) );
//zz       assign( byte0, binop(Iop_And32, mkexpr(rS), mkU32(0x00FF)) );
//zz       assign( byte1, binop(Iop_And32, mkexpr(rS), mkU32(0xFF00)) );
//zz       
//zz       assign( tmp16,
//zz               unop(Iop_32to16,
//zz                    binop(Iop_Or32,
//zz                          binop(Iop_Shl32, mkexpr(byte0), mkU8(8)),
//zz                          binop(Iop_Shr32, mkexpr(byte1), mkU8(8)))) );
//zz       storeBE( mkexpr(EA), getIReg(tmp16) );
//zz       break;
      
      case 0x296: // stwbrx (Store Word Byte-Reverse Indexed, PPC32 p531)
         DIP("stwbrx r%d,r%d,r%d\n", rS_addr, rA_addr, rB_addr);
         assign( w1, getIReg(rS_addr) );
         storeBE( mkexpr(EA), gen_byterev32(w1) );
         break;
      
      default:
         vex_printf("dis_int_ldst_rev(PPC32)(opc2)\n");
         return False;
   }
   return True;
}



/*
  Processor Control Instructions
*/
static Bool dis_proc_ctl ( UInt theInstr )
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

   IRTemp rS;

   /* Reorder SPR field as per PPC32 p470 */
   SPR = ((SPR & 0x1F) << 5) | ((SPR >> 5) & 0x1F);

   /* Reorder TBR field as per PPC32 p475 */
   TBR = ((TBR & 31) << 5) | ((TBR >> 5) & 31);

   rS = newTemp(Ity_I32);
   assign( rS, getIReg(rS_addr) );
   
   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_proc_ctl(PPC32)(opc1|b0)\n");
      return False;
   }
   
   switch (opc2) {
   /* X-Form */
   case 0x200: { // mcrxr (Move to Condition Register from XER, PPC32 p466)
      if (b21to22 != 0 || b11to20 != 0) {
         vex_printf("dis_proc_ctl(PPC32)(mcrxr,b21to22|b11to20)\n");
         return False;
      }
      DIP("mcrxr crf%d\n", crfD);
      /* Move XER[0-3] (the top 4 bits of XER) to CR[crfD] */
      putSPR_field( PPC32_SPR_CR,
                    getSPR_field( PPC32_SPR_XER, 7 ),
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
         putIReg( rD_addr, getSPR( PPC32_SPR_CR ) );
         break;
      }
      if (b20 == 1 && b11 == 0) {
         DIP("mfocrf r%u,%u\n", rD_addr, CRM);
         putIReg( rD_addr, getSPR( PPC32_SPR_CR ) );
         break;
      }
      /* not decodable */
      return False;
      
   /* XFX-Form */
   case 0x153: // mfspr (Move from Special-Purpose Register, PPC32 p470)
      
      switch (SPR) {  // Choose a register...

         case 0x1:
            DIP("mfxer r%d\n", rD_addr);
            putIReg( rD_addr, getSPR( PPC32_SPR_XER ) ); 
            break;
         case 0x8:
            DIP("mflr r%d\n", rD_addr);
            putIReg( rD_addr, getSPR( PPC32_SPR_LR ) ); 
            break;
         case 0x9:
            DIP("mfctr r%d\n", rD_addr);
            putIReg( rD_addr, getSPR( PPC32_SPR_CTR ) ); 
            break;
         case 0x100: 
            DIP("mfvrsave r%d\n", rD_addr);
            putIReg( rD_addr, getSPR( PPC32_SPR_VRSAVE ) ); 
            break;

         case 0x012: case 0x013: case 0x016:
         case 0x019: case 0x01A: case 0x01B:
         case 0x110: case 0x111: case 0x112: case 0x113:
         // case 0x118: // 64bit only
         case 0x11A: case 0x11F:
         case 0x210: case 0x211: case 0x212: case 0x213:
         case 0x214: case 0x215: case 0x216: case 0x217:
         case 0x218: case 0x219: case 0x21A: case 0x21B:
         case 0x21C: case 0x21D: case 0x21E: case 0x21F:
         case 0x3F5:
            vex_printf("dis_proc_ctl(PPC32)(mfspr) - supervisor level op\n");
            return False;

         default:
            vex_printf("dis_proc_ctl(PPC32)(mfspr,SPR)(0x%x)\n", SPR);
            return False;
      }
      break;
      
   case 0x173: { // mftb (Move from Time Base, PPC32 p475)
      IRTemp   val  = newTemp(Ity_I64);
      IRExpr** args = mkIRExprVec_0();
      IRDirty* d    = unsafeIRDirty_1_N ( 
                         val, 
                         0/*regparms*/, 
                         "ppc32g_dirtyhelper_MFTB", 
                         &ppc32g_dirtyhelper_MFTB, 
                         args 
                      );
      /* execute the dirty call, dumping the result in val. */
      stmt( IRStmt_Dirty(d) );

      switch (TBR) {
         case 269: 
            putIReg( rD_addr, unop(Iop_64HIto32, mkexpr(val)) );
            DIP("mftbu r%d", rD_addr);
            break;
         case 268: 
            putIReg( rD_addr, unop(Iop_64to32, mkexpr(val)) );
            DIP("mftb r%d", rD_addr);
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
         putSPR_field( PPC32_SPR_CR,
                       binop(Iop_Shr32, mkexpr(rS), mkU8(shft)), cr );
      }
      break;
   }




   case 0x1D3: // mtspr (Move to Special-Purpose Register, PPC32 p483)
      
      switch (SPR) {  // Choose a register...
         case 0x1:
            DIP("mtxer r%d\n", rS_addr);
            putSPR( PPC32_SPR_XER, mkexpr(rS) ); 
            break;
         case 0x8:
            DIP("mtlr r%d\n", rS_addr);
            putSPR( PPC32_SPR_LR, mkexpr(rS) ); 
            break;
         case 0x9:
            DIP("mtctr r%d\n", rS_addr);
            putSPR( PPC32_SPR_CTR, mkexpr(rS) ); 
            break;
         case 0x100:
            DIP("mtvrsave r%d\n", rS_addr);
            putSPR( PPC32_SPR_VRSAVE, mkexpr(rS) ); 
            break;

         default:
            vex_printf("dis_proc_ctl(PPC32)(mtspr,SPR)(%u)\n", SPR);
            return False;
      }
      break;
      
   default:
      vex_printf("dis_proc_ctl(PPC32)(opc2)\n");
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
   Int   lineszB = guest_archinfo->ppc32_cache_line_szB;

   if (opc1 != 0x1F || b21to25 != 0 || b0 != 0) {
      vex_printf("dis_cache_manage(PPC32)(opc1|b21to25|b0)\n");
      return False;
   }

   /* stay sane .. */
   vassert(lineszB == 32 || lineszB == 128);
   
   switch (opc2) {
//zz    case 0x2F6: // dcba (Data Cache Block Allocate, PPC32 p380)
//zz       vassert(0); /* AWAITING TEST CASE */
//zz       DIP("dcba r%d,r%d\n", rA_addr, rB_addr);
//zz       if (0) vex_printf("vex ppc32->IR: kludged dcba\n");
//zz       break;
      
   case 0x056: // dcbf (Data Cache Block Flush, PPC32 p382)
      DIP("dcbf r%d,r%d\n", rA_addr, rB_addr);
      /* nop as far as vex is concerned */
      if (0) vex_printf("vex ppc32->IR: kludged dcbf\n");
      break;
      
   case 0x036: // dcbst (Data Cache Block Store, PPC32 p384)
      DIP("dcbst r%d,r%d\n", rA_addr, rB_addr);
      /* nop as far as vex is concerned */
      break;

   case 0x116: // dcbt (Data Cache Block Touch, PPC32 p385)
      DIP("dcbt r%d,r%d\n", rA_addr, rB_addr);
      /* nop as far as vex is concerned */
      break;
      
   case 0x0F6: // dcbtst (Data Cache Block Touch for Store, PPC32 p386)
      DIP("dcbtst r%d,r%d\n", rA_addr, rB_addr);
      /* nop as far as vex is concerned */
      break;
      
   case 0x3F6: { // dcbz (Data Cache Block Clear to Zero, PPC32 p387)
      /* Clear all bytes in cache block at (rA|0) + rB. */
      IRTemp  EA   = newTemp(Ity_I32);
      IRTemp  addr = newTemp(Ity_I32);
      IRExpr* irx_addr;
      UInt    i;
      DIP("dcbz r%d,r%d\n", rA_addr, rB_addr);
      assign( EA, binop( Iop_Add32,
                         getIReg(rB_addr), 
                         rA_addr==0 ? mkU32(0) : getIReg(rA_addr)) );

      /* Round EA down to the start of the containing block. */
      assign( addr, binop( Iop_And32,
                           mkexpr(EA),
                           mkU32( ~(lineszB-1) )) );

      for (i = 0; i < lineszB / 4; i++) {
         irx_addr = binop( Iop_Add32, mkexpr(addr), mkU32(i*4) );
         storeBE( irx_addr, mkU32(0) );
      }
      break;
   }

   case 0x3D6: { 
      // icbi (Instruction Cache Block Invalidate, PPC32 p431)
      /* Invalidate all translations containing code from the cache
         block at (rA|0) + rB. */
      IRTemp addr = newTemp(Ity_I32);
      DIP("icbi r%d,r%d\n", rA_addr, rB_addr);

      assign( addr,
              binop( Iop_Add32, 
                     getIReg(rB_addr), 
                     rA_addr==0 ? mkU32(0) : getIReg(rA_addr)) );

      /* Round addr down to the start of the containing block. */
      stmt( IRStmt_Put(
               OFFB_TISTART,
               binop( Iop_And32, 
                      mkexpr(addr), 
                      mkU32( ~(lineszB-1) ))) );

      stmt( IRStmt_Put(OFFB_TILEN, mkU32(lineszB) ) );

      /* be paranoid ... */
      stmt( IRStmt_MFence() );

      irbb->jumpkind = Ijk_TInval;
      irbb->next     = mkU32(guest_CIA_curr_instr + 4);
      dres->whatNext = Dis_StopHere;
      break;
   }

   default:
      vex_printf("dis_cache_manage(PPC32)(opc2)\n");
      return False;
   }
   return True;
}


/*------------------------------------------------------------*/
/*--- Floating Point Helpers                               ---*/
/*------------------------------------------------------------*/

/* --- Set the emulation-warning pseudo-register. --- */

static void put_emwarn ( IRExpr* e /* :: Ity_I32 */ )
{
   vassert(typeOfIRExpr(irbb->tyenv,e) == Ity_I32);
   stmt( IRStmt_Put( OFFB_EMWARN, e ) );
}

/* --------- Synthesise a 2-bit FPU rounding mode. --------- */
/* Produces a value in 0 .. 3, which is encoded as per the type
   IRRoundingMode.  PPC32RoundingMode encoding is different to
   IRRoundingMode, so need to map it.
*/
static IRExpr* /* :: Ity_I32 */ get_roundingmode ( void )
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
   assign( rm_PPC32, getSPR_masked( PPC32_SPR_FPSCR, MASK_FPSCR_RN ) );

   // rm_IR = XOR( rm_PPC32, (rm_PPC32 << 1) & 2)
   return binop(Iop_Xor32, mkexpr(rm_PPC32),
                binop(Iop_And32, mkU32(2),
                      binop(Iop_Shl32, mkexpr(rm_PPC32), mkU8(1))));
}

/* Round float to single precision
 - returns type Ity_F64 */
static IRExpr* roundToSgl ( IRExpr* src )
{
   return unop(Iop_F32toF64, binop(Iop_F64toF32, get_roundingmode(), src));
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
   Int   d_simm16  = ifieldSIMM16(theInstr);

   IRTemp EA       = newTemp(Ity_I32);
   IRTemp rA       = newTemp(Ity_I32);
   IRTemp rB       = newTemp(Ity_I32);
   IRTemp rA_or_0  = newTemp(Ity_I32);

   assign( rA, getIReg(rA_addr) );
   assign( rB, getIReg(rB_addr) );
   assign( rA_or_0, ea_rA_or_zero(rA_addr) );

   switch(opc1) {
   case 0x30: // lfs (Load Float Single, PPC32 p441)
      DIP("lfs fr%d,%d(r%d)\n", frD_addr, d_simm16, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(d_simm16), mkexpr(rA_or_0)) );
      putFReg( frD_addr, unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
      break;

   case 0x31: // lfsu (Load Float Single with Update, PPC32 p442)
      if (rA_addr == 0) {
         vex_printf("dis_fp_load(PPC32)(instr,lfsu)\n");
         return False;
      }
      DIP("lfsu fr%u,%d(r%u)\n", frD_addr, d_simm16, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(d_simm16), mkexpr(rA_or_0)) );
      putFReg( frD_addr, unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
      putIReg( rA_addr, mkexpr(EA) );
      break;
      
   case 0x32: // lfd (Load Float Double, PPC32 p437)
      DIP("lfd fr%d,%d(r%d)\n", frD_addr, d_simm16, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(d_simm16), mkexpr(rA_or_0)) );
      putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
      break;

   case 0x33: // lfdu (Load Float Double with Update, PPC32 p438)
      if (rA_addr == 0) {
         vex_printf("dis_fp_load(PPC32)(instr,lfdu)\n");
         return False;
      }
      DIP("lfdu fr%d,%d(r%d)\n", frD_addr, d_simm16, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(d_simm16), mkexpr(rA)) );
      putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_fp_load(PPC32)(instr,b0)\n");
         return False;
      }

      switch(opc2) {
         case 0x217: // lfsx (Load Float Single Indexed, PPC32 p444)
            DIP("lfsx fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
            putFReg( frD_addr, unop( Iop_F32toF64, 
                                     loadBE(Ity_F32, mkexpr(EA))) );
            break;

         case 0x237: // lfsux (Load Float Single with Update Indexed, PPC32 p443)
            if (rA_addr == 0) {
               vex_printf("dis_fp_load(PPC32)(instr,lfsux)\n");
               return False;
            }
            DIP("lfsux fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA)) );
            putFReg( frD_addr, unop(Iop_F32toF64, loadBE(Ity_F32, mkexpr(EA))) );
            putIReg( rA_addr, mkexpr(EA) );
            break;

         case 0x257: // lfdx (Load Float Double Indexed, PPC32 p440)
            DIP("lfdx fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
            putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
            break;

         case 0x277: // lfdux (Load Float Double with Update Indexed, PPC32 p439)
            if (rA_addr == 0) {
               vex_printf("dis_fp_load(PPC32)(instr,lfdux)\n");
               return False;
            }
            DIP("lfdux fr%d,r%d,r%d\n", frD_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA)) );
            putFReg( frD_addr, loadBE(Ity_F64, mkexpr(EA)) );
            putIReg( rA_addr, mkexpr(EA) );
            break;

        default:
            vex_printf("dis_fp_load(PPC32)(opc2)\n");
            return False;
      }
      break;

   default:
      vex_printf("dis_fp_load(PPC32)(opc1)\n");
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
   Int   d_simm16  = ifieldSIMM16(theInstr);

   IRTemp EA       = newTemp(Ity_I32);
   IRTemp frS      = newTemp(Ity_F64);
   IRTemp rA       = newTemp(Ity_I32);
   IRTemp rB       = newTemp(Ity_I32);
   IRTemp rA_or_0  = newTemp(Ity_I32);

   assign( frS, getFReg(frS_addr) );
   assign( rA,  getIReg(rA_addr) );
   assign( rB,  getIReg(rB_addr) );
   assign( rA_or_0, ea_rA_or_zero(rA_addr) );

   switch(opc1) {

   case 0x34: // stfs (Store Float Single, PPC32 p518)
      DIP("stfs fr%d,%d(r%d)\n", frS_addr, d_simm16, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(d_simm16), mkexpr(rA_or_0)) );
      storeBE( mkexpr(EA),
               binop(Iop_F64toF32, get_roundingmode(), mkexpr(frS)) );
      break;

   case 0x35: // stfsu (Store Float Single with Update, PPC32 p519)
      if (rA_addr == 0) {
         vex_printf("dis_fp_store(PPC32)(instr,stfsu)\n");
         return False;
      }
      DIP("stfsu fr%u,%d(r%u)\n", frS_addr, d_simm16, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(d_simm16), mkexpr(rA_or_0)) );
      storeBE( mkexpr(EA),
               binop(Iop_F64toF32, get_roundingmode(), mkexpr(frS)) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x36: // stfd (Store Float Double, PPC32 p513)
      DIP("stfd fr%d,%d(r%d)\n", frS_addr, d_simm16, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(d_simm16), mkexpr(rA_or_0)) );
      storeBE( mkexpr(EA), mkexpr(frS) );
      break;

   case 0x37: // stfdu (Store Float Double with Update, PPC32 p514)
      if (rA_addr == 0) {
         vex_printf("dis_fp_store(PPC32)(instr,stfdu)\n");
         return False;
      }
      DIP("stfdu fr%d,%d(r%d)\n", frS_addr, d_simm16, rA_addr);
      assign( EA, binop(Iop_Add32, mkU32(d_simm16), mkexpr(rA)) );
      storeBE( mkexpr(EA), mkexpr(frS) );
      putIReg( rA_addr, mkexpr(EA) );
      break;

   case 0x1F:
      if (b0 != 0) {
         vex_printf("dis_fp_store(PPC32)(instr,b0)\n");
         return False;
      }

      switch(opc2) {
      case 0x297: // stfsx (Store Float Single Indexed, PPC32 p521)
         DIP("stfsx fr%u,r%u,r%u\n", frS_addr, rA_addr, rB_addr);
         assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
         storeBE( mkexpr(EA),
                  binop(Iop_F64toF32, get_roundingmode(), mkexpr(frS)) );
         break;
         
      case 0x2B7: // stfsux (Store Float Single with Update Indexed, PPC32 p520)
         if (rA_addr == 0) {
            vex_printf("dis_fp_store(PPC32)(instr,stfsux)\n");
            return False;
         }
         DIP("stfsux fr%u,r%u,r%u\n", frS_addr, rA_addr, rB_addr);
         assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA)) );
         storeBE( mkexpr(EA),
                  binop(Iop_F64toF32, get_roundingmode(), mkexpr(frS)) );
         putIReg( rA_addr, mkexpr(EA) );
         break;

         case 0x2D7: // stfdx (Store Float Double Indexed, PPC32 p516)
            DIP("stfdx fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
            storeBE( mkexpr(EA), mkexpr(frS) );
            break;
         
         case 0x2F7: // stfdux (Store Float Double with Update Indexed, PPC32 p515)
            if (rA_addr == 0) {
               vex_printf("dis_fp_store(PPC32)(instr,stfdux)\n");
               return False;
            }
            DIP("stfdux fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
            assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA)) );
            storeBE( mkexpr(EA), mkexpr(frS) );
            putIReg( rA_addr, mkexpr(EA) );
            break;

//zz       case 0x3D7: // stfiwx (Store Float as Int, Indexed, PPC32 p517)
//zz          DIP("stfiwx fr%d,r%d,r%d\n", frS_addr, rA_addr, rB_addr);
//zz          assign( EA, binop(Iop_Add32, mkexpr(rB), mkexpr(rA_or_0)) );
//zz          storeBE( mkexpr(EA),
//zz                   unop(Iop_64to32, unop(Iop_ReinterpF64asI64, mkexpr(frS))) );
//zz          break;

         default:
            vex_printf("dis_fp_store(PPC32)(opc2)\n");
            return False;
      }
      break;

   default:
      vex_printf("dis_fp_store(PPC32)(opc1)\n");
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
   // Note: flag_rC ignored as fp exceptions not supported.

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frA = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);
   IRTemp frC = newTemp(Ity_F64);

   assign( frA, getFReg(frA_addr));
   assign( frB, getFReg(frB_addr));
   assign( frC, getFReg(frC_addr));

   switch (opc1) {
   case 0x3B:
      switch (opc2) {
      case 0x12: // fdivs (Floating Divide Single, PPC32 p407)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fdivs)\n");
            return False;
         }
         DIP("fdivs%s fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, roundToSgl( binop(Iop_DivF64, mkexpr(frA), mkexpr(frB)) ));
         break;

      case 0x14: // fsubs (Floating Subtract Single, PPC32 p430)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fsubs)\n");
            return False;
         }
         DIP("fsubs%s fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, roundToSgl( 
                         binop(Iop_SubF64, mkexpr(frA), mkexpr(frB)) ));
         break;

      case 0x15: // fadds (Floating Add Single, PPC32 p401)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fadds)\n");
            return False;
         }
         DIP("fadds%s fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, roundToSgl( 
                         binop(Iop_AddF64, mkexpr(frA), mkexpr(frB)) ));
         break;

      case 0x16: // fsqrt (Floating SqRt (Double-Precision), PPC32 p427)
         if (frA_addr != 0 || frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fsqrt)\n");
            return False;
         }
         DIP("fsqrt%s fr%u,fr%u\n", flag_rC ? "." : "",
             frD_addr, frB_addr);
         assign( frD, unop( Iop_SqrtF64, mkexpr(frB) ) );
         break;

//zz       case 0x16: // fsqrts (Floating SqRt (Single-Precision), PPC32 p428)
//zz          if (frA_addr != 0 || frC_addr != 0) {
//zz             vex_printf("dis_fp_arith(PPC32)(instr,fsqrts)\n");
//zz             return False;
//zz          }
//zz          DIP("fsqrts%s fr%d,fr%d\n", flag_rC ? "." : "",
//zz              frD_addr, frB_addr);
//zz          assign( frD, roundToSgl( unop(Iop_SqrtF64, mkexpr(frB)) ));
//zz          break;

//zz       case 0x18: // fres (Floating Reciprocal Estimate Single, PPC32 p421)
//zz          if (frA_addr != 0 || frC_addr != 0) {
//zz             vex_printf("dis_fp_arith(PPC32)(instr,fres)\n");
//zz             return False;
//zz          }
//zz          DIP("fres%s fr%d,fr%d\n", flag_rC ? "." : "",
//zz              frD_addr, frB_addr);
//zz          DIP(" => not implemented\n");        
//zz          // CAB: Can we use one of the 128 bit SIMD Iop_Recip32F ops?
//zz          return False;

      case 0x19: // fmuls (Floating Multiply Single, PPC32 p414)
         if (frB_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fmuls)\n");
            return False;
         }
         DIP("fmuls%s fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr);
         assign( frD, roundToSgl( binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)) ));
         break;

      default:
         vex_printf("dis_fp_arith(PPC32)(3B: opc2)\n");
         return False;
      }
      break;

   case 0x3F:
      switch (opc2) {           
      case 0x12: // fdiv (Floating Divide (Double-Precision), PPC32 p406)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fdiv)\n");
            return False;
         }
         DIP("fdiv%s fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, binop( Iop_DivF64, mkexpr(frA), mkexpr(frB) ) );
         break;

      case 0x14: // fsub (Floating Subtract (Double-Precision), PPC32 p429)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fsub)\n");
            return False;
         }
         DIP("fsub%s fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, binop( Iop_SubF64, mkexpr(frA), mkexpr(frB) ) );
         break;

      case 0x15: // fadd (Floating Add (Double-Precision), PPC32 p400)
         if (frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fadd)\n");
            return False;
         }
         DIP("fadd%s fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frB_addr);
         assign( frD, binop( Iop_AddF64, mkexpr(frA), mkexpr(frB) ) );
         break;

      case 0x16: // fsqrt (Floating SqRt (Double-Precision), PPC32 p427)
         if (frA_addr != 0 || frC_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fsqrt)\n");
            return False;
         }
         DIP("fsqrt%s fr%d,fr%d\n", flag_rC ? "." : "",
              frD_addr, frB_addr);
         assign( frD, unop( Iop_SqrtF64, mkexpr(frB) ) );
         break;

      case 0x17: { // fsel (Floating Select, PPC32 p426)
         IRTemp cc    = newTemp(Ity_I32);
         IRTemp cc_b0 = newTemp(Ity_I32);

         DIP("fsel%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);

         // cc: UN == 0x41, LT == 0x01, GT == 0x00, EQ == 0x40
         // => GT|EQ == (cc & 0x1 == 0)
         assign( cc, binop(Iop_CmpF64, mkexpr(frA), IRExpr_Const(IRConst_F64(0))) );
         assign( cc_b0, binop(Iop_And32, mkexpr(cc), mkU32(1)) );

         // frD = (frA >= 0.0) ? frC : frB
         //     = (cc_b0 == 0) ? frC : frB
         assign( frD,
                 IRExpr_Mux0X(
                    unop(Iop_1Uto8,
                         binop(Iop_CmpEQ32, mkexpr(cc_b0), mkU32(0))),
                    mkexpr(frB),
                    mkexpr(frC) ));
         break;
      }

      case 0x19: // fmul (Floating Multiply (Double Precision), PPC32 p413)
         if (frB_addr != 0) {
            vex_printf("dis_fp_arith(PPC32)(instr,fmul)\n");
            return False;
         }
         DIP("fmul%s fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr);
         assign( frD, binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ) );
         break;

//zz       case 0x1A: // frsqrte (Floating Reciprocal SqRt Estimate, PPC32 p424)
//zz          if (frA_addr != 0 || frC_addr != 0) {
//zz             vex_printf("dis_fp_arith(PPC32)(instr,frsqrte)\n");
//zz             return False;
//zz          }
//zz          DIP("frsqrte%s fr%d,fr%d\n", flag_rC ? "." : "",
//zz              frD_addr, frB_addr);
//zz          DIP(" => not implemented\n");
//zz          // CAB: Iop_SqrtF64, then one of the 128 bit SIMD Iop_Recip32F ops?
//zz          return False;

      default:
         vex_printf("dis_fp_arith(PPC32)(3F: opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_arith(PPC32)(opc1)\n");
      return False;
   }

   putFReg( frD_addr, mkexpr(frD) );
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

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frA = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);
   IRTemp frC = newTemp(Ity_F64);

   assign( frA, getFReg(frA_addr));
   assign( frB, getFReg(frB_addr));
   assign( frC, getFReg(frC_addr));

   switch (opc1) {
   case 0x3B:
      switch (opc2) {
      case 0x1C: // fmsubs (Floating Mult-Subtr Single, PPC32 p412)
         DIP("fmsubs%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, roundToSgl( 
                         binop( Iop_SubF64,
                                binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)),
                                mkexpr(frB)) ));
          break;

      case 0x1D: // fmadds (Floating Mult-Add Single, PPC32 p409)
         DIP("fmadds%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, roundToSgl( 
                         binop( Iop_AddF64,
                                binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)),
                                mkexpr(frB)) ));
         break;

      case 0x1E: // fnmsubs (Float Neg Mult-Subtr Single, PPC32 p420)
         DIP("fnmsubs%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, roundToSgl(
                    unop(Iop_NegF64,
                         binop(Iop_SubF64,
                               binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)),
                               mkexpr(frB))) ));
         break;

      case 0x1F: // fnmadds (Floating Negative Multiply-Add Single, PPC32 p418)
         DIP("fnmadds%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, roundToSgl(
                    unop(Iop_NegF64,
                         binop(Iop_AddF64,
                               binop(Iop_MulF64, mkexpr(frA), mkexpr(frC)),
                               mkexpr(frB))) ));
         break;

      default:
         vex_printf("dis_fp_multadd(PPC32)(3B: opc2)\n");
         return False;
      }
      break;

   case 0x3F:
      switch (opc2) {           
      case 0x1C: // fmsub (Float Mult-Subtr (Double Precision), PPC32 p411)
         DIP("fmsub%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, binop( Iop_SubF64,
                             binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ),
                             mkexpr(frB) ));
         break;

      case 0x1D: // fmadd (Float Mult-Add (Double Precision), PPC32 p408)
         DIP("fmadd%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, binop( Iop_AddF64,
                             binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ),
                             mkexpr(frB) ));
         break;

      case 0x1E: // fnmsub (Float Neg Mult-Subtr (Double Precision), PPC32 p419)
         DIP("fnmsub%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, unop( Iop_NegF64,
                            binop( Iop_SubF64,
                                   binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ),
                                   mkexpr(frB) )));
         break;

      case 0x1F: // fnmadd (Float Neg Mult-Add (Double Precision), PPC32 p417)
         DIP("fnmadd%s fr%d,fr%d,fr%d,fr%d\n", flag_rC ? "." : "",
             frD_addr, frA_addr, frC_addr, frB_addr);
         assign( frD, unop( Iop_NegF64,
                            binop( Iop_AddF64,
                                   binop( Iop_MulF64, mkexpr(frA), mkexpr(frC) ),
                                   mkexpr(frB) )));
         break;

      default:
         vex_printf("dis_fp_multadd(PPC32)(3F: opc2)\n");
         return False;
      }
      break;

   default:
      vex_printf("dis_fp_multadd(PPC32)(opc1)\n");
      return False;
   }

   putFReg( frD_addr, mkexpr(frD) );
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
      vex_printf("dis_fp_cmp(PPC32)(instr)\n");
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

   // ccPPC32 = Shl(1, (0x2 & ~(ccIR>>5)) || (0x1 & (XOR(ccIR, ccIR>>6))))
   assign(
      ccPPC32,
      binop(Iop_Shl32, mkU32(1),
            unop(Iop_32to8, 
                 binop(Iop_Or32,
                       binop(Iop_And32, mkU32(2),
                             unop(Iop_Not32,
                                  binop(Iop_Shr32, mkexpr(ccIR), mkU8(5)))),
                       binop(Iop_And32, mkU32(1),
                             binop(Iop_Xor32, mkexpr(ccIR),
                                   binop(Iop_Shr32, mkexpr(ccIR), mkU8(6)))))))
   );

   putSPR_field( PPC32_SPR_CR, mkexpr(ccPPC32), crfD );

   /* CAB: TODO?: Support writing cc to FPSCR->FPCC ?
      putSPR_field( PPC32_SPR_FPSCR, mkexpr(ccPPC32), 4 );
   */

   /* Note: Differences between fcmpu and fcmpo are only in exception
      flag settings, which aren't supported anyway. */
   switch (opc2) {
      case 0x000: // fcmpu (Floating Compare Unordered, PPC32 p403)
         DIP("fcmpu crf%d,fr%d,fr%d\n", crfD, frA_addr, frB_addr);
         break;
      case 0x020: // fcmpo (Floating Compare Ordered, PPC32 p402)
         DIP("fcmpo crf%d,fr%d,fr%d\n", crfD, frA_addr, frB_addr);
         break;
      default:
         vex_printf("dis_fp_cmp(PPC32)(opc2)\n");
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
   UChar frD_addr = ifieldRegDS(theInstr);
   UChar b16to20  = ifieldRegA(theInstr);
   UChar frB_addr = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar flag_rC  = ifieldBIT0(theInstr);

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);
   IRTemp r_tmp = newTemp(Ity_I32);

   if (opc1 != 0x3F || b16to20 != 0) {
      vex_printf("dis_fp_round(PPC32)(instr)\n");
      return False;
   }

   assign( frB, getFReg(frB_addr));

   switch (opc2) {
      case 0x00C: // frsp (Floating Round to Single, PPC32 p423)
         DIP("frsp%s fr%d,fr%d\n", flag_rC ? "." : "", frD_addr, frB_addr);
         assign( frD, roundToSgl( mkexpr(frB) ));
         break;

      case 0x00E: // fctiw (Floating Conv to Int, PPC32 p404)
         DIP("fctiw%s fr%d,fr%d\n", flag_rC ? "." : "", frD_addr, frB_addr);
         assign( r_tmp, binop(Iop_F64toI32, get_roundingmode(), mkexpr(frB)) );
         assign( frD, unop( Iop_ReinterpI64asF64,
                            unop( Iop_32Uto64, mkexpr(r_tmp))));
         break;

      case 0x00F: // fctiwz (Floating Conv to Int, Round to Zero, PPC32 p405)
         DIP("fctiwz%s fr%d,fr%d\n", flag_rC ? "." : "", frD_addr, frB_addr);
         assign( r_tmp, binop(Iop_F64toI32, mkU32(0x3), mkexpr(frB)) );
         assign( frD, unop( Iop_ReinterpI64asF64,
                            unop( Iop_32Uto64, mkexpr(r_tmp))));
         break;

      default:
         vex_printf("dis_fp_round(PPC32)(opc2)\n");
         return False;
   }

   putFReg( frD_addr, mkexpr(frD) );
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
   UChar b16to20  = ifieldRegA(theInstr);
   UChar frB_addr = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar flag_rC  = ifieldBIT0(theInstr);

   IRTemp frD = newTemp(Ity_F64);
   IRTemp frB = newTemp(Ity_F64);

   if (opc1 != 0x3F || b16to20 != 0) {
      vex_printf("dis_fp_move(PPC32)(instr)\n");
      return False;
   }

   assign( frB, getFReg(frB_addr));

   switch (opc2) {
      case 0x028: // fneg (Floating Negate, PPC32 p416)
         DIP("fneg%s fr%d,fr%d\n", flag_rC ? "." : "", frD_addr, frB_addr);
         assign( frD, unop( Iop_NegF64, mkexpr(frB) ));
         break;

      case 0x048: // fmr (Floating Move Register, PPC32 p410)
         DIP("fmr%s fr%d,fr%d\n", flag_rC ? "." : "", frD_addr, frB_addr);
         assign( frD, mkexpr(frB) );
         break;

      case 0x088: // fnabs (Floating Negative Absolute Value, PPC32 p415)
         DIP("fnabs%s fr%d,fr%d\n", flag_rC ? "." : "", frD_addr, frB_addr);
         assign( frD, unop( Iop_NegF64, unop( Iop_AbsF64, mkexpr(frB) )));
         break;

      case 0x108: // fabs (Floating Absolute Value, PPC32 p399)
         DIP("fabs%s fr%d,fr%d\n", flag_rC ? "." : "", frD_addr, frB_addr);
         assign( frD, unop( Iop_AbsF64, mkexpr(frB) ));
         break;

      default:
         vex_printf("dis_fp_move(PPC32)(opc2)\n");
         return False;
   }

   putFReg( frD_addr, mkexpr(frD) );
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
      vex_printf("dis_fp_scr(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
//zz    case 0x026: { // mtfsb1 (Move to FPSCR Bit 1, PPC32 p479)
//zz       // Bit crbD of the FPSCR is set.
//zz       UChar crbD    = ifieldRegDS(theInstr);
//zz       UInt  b11to20 = IFIELD(theInstr, 11, 10);
//zz 
//zz       if (b11to20 != 0) {
//zz          vex_printf("dis_fp_scr(PPC32)(instr,mtfsb1)\n");
//zz          return False;
//zz       }
//zz       DIP("mtfsb1%s crb%d \n", flag_rC ? "." : "", crbD);
//zz       putSPR_masked( PPC32_SPR_FPSCR, mkU32(1<<(31-crbD)), 1<<(31-crbD) );
//zz       break;
//zz    }
//zz 
//zz    case 0x040: { // mcrfs (Move to Condition Register from FPSCR, PPC32 p465)
//zz       UChar crfD    = toUChar( IFIELD( theInstr, 23, 3 ) );
//zz       UChar b21to22 = toUChar( IFIELD( theInstr, 21, 2 ) );
//zz       UChar crfS    = toUChar( IFIELD( theInstr, 18, 3 ) );
//zz       UChar b11to17 = toUChar( IFIELD( theInstr, 11, 7 ) );
//zz 
//zz       IRTemp tmp = newTemp(Ity_I32);
//zz 
//zz       if (b21to22 != 0 || b11to17 != 0 || flag_rC != 0) {
//zz          vex_printf("dis_fp_scr(PPC32)(instr,mcrfs)\n");
//zz          return False;
//zz       }
//zz       DIP("mcrfs crf%d,crf%d\n", crfD, crfS);
//zz       assign( tmp, getSPR_field( PPC32_SPR_FPSCR, crfS ) );
//zz       putSPR_field( PPC32_SPR_CR, mkexpr(tmp), crfD );
//zz       break;
//zz    }

   case 0x046: { // mtfsb0 (Move to FPSCR Bit 0, PPC32 p478)
      // Bit crbD of the FPSCR is cleared.
      UChar crbD    = ifieldRegDS(theInstr);
      UInt  b11to20 = IFIELD(theInstr, 11, 10);

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsb0)\n");
         return False;
      }      
      DIP("mtfsb0%s crb%d\n", flag_rC ? "." : "", crbD);
      putSPR_masked( PPC32_SPR_FPSCR, mkU32(0), 1<<(31-crbD) );
      break;
   }

   case 0x086: { // mtfsfi (Move to FPSCR Field Immediate, PPC32 p481)
      UChar crfD    = toUChar( IFIELD( theInstr, 23, 3 ) );
      UChar b16to22 = toUChar( IFIELD( theInstr, 16, 7 ) );
      UChar IMM     = toUChar( IFIELD( theInstr, 12, 4 ) );
      UChar b11     = toUChar( IFIELD( theInstr, 11, 1 ) );

      if (b16to22 != 0 || b11 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsfi)\n");
         return False;
      }      
      DIP("mtfsfi%s crf%d,%d\n", flag_rC ? "." : "", crfD, IMM);
      putSPR_field( PPC32_SPR_FPSCR, mkU32(IMM), crfD );
      break;
   }

   case 0x247: { // mffs (Move from FPSCR, PPC32 p468)
      UChar frD_addr = ifieldRegDS(theInstr);
      UInt  b11to20  = IFIELD(theInstr, 11, 10);

      if (b11to20 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mffs)\n");
         return False;
      }
      DIP("mffs%s fr%d\n", flag_rC ? "." : "", frD_addr);
      putFReg( frD_addr, unop( Iop_ReinterpI64asF64,
                               unop( Iop_32Uto64, 
                                     getSPR_masked( PPC32_SPR_FPSCR, 0x3 ) )));
      break;
   }

   case 0x2C7: { // mtfsf (Move to FPSCR Fields, PPC32 p480)
      UChar b25      = toUChar( IFIELD(theInstr, 25, 1) );
      UChar FM       = toUChar( IFIELD(theInstr, 17, 8) );
      UChar b16      = toUChar( IFIELD(theInstr, 16, 1) );
      UChar frB_addr = ifieldRegB(theInstr);
      IRTemp frB   = newTemp(Ity_F64);
      IRTemp rB_32 = newTemp(Ity_I32);
      Int i, mask;

      if (b25 != 0 || b16 != 0) {
         vex_printf("dis_fp_scr(PPC32)(instr,mtfsf)\n");
         return False;
      }      
      DIP("mtfsf%s %d,fr%d\n", flag_rC ? "." : "", FM, frB_addr);
      assign( frB, getFReg(frB_addr));
      assign( rB_32, unop( Iop_64to32,
                           unop( Iop_ReinterpF64asI64, mkexpr(frB) )));
      // Build 32bit mask from FM:
      mask = 0;
      for (i=0; i<8; i++) {
         if ((FM & (1<<(7-i))) == 1) {
            mask |= 0xF << (7-i);
         }
      }
      putSPR_masked( PPC32_SPR_FPSCR, mkexpr(rB_32), mask );
      break;
   }

   default:
      vex_printf("dis_fp_scr(PPC32)(opc2)\n");
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
      vex_printf("dis_av_datastream(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x156: // dst (Data Stream Touch, AV p115)
      DIP("dst%s r%d,r%d,%d\n", flag_T ? "t" : "", rA_addr, rB_addr, STRM);
      DIP(" => not implemented\n");
      return False;

   case 0x176: // dstst (Data Stream Touch for Store, AV p117)
      DIP("dstst%s r%d,r%d,%d\n", flag_T ? "t" : "", rA_addr, rB_addr, STRM);
      DIP(" => not implemented\n");
      return False;

   case 0x336: // dss (Data Stream Stop, AV p114)
      if (rA_addr != 0 || rB_addr != 0) {
         vex_printf("dis_av_datastream(PPC32)(opc2,dst)\n");
         return False;
      }
      if (flag_A == 0) {
         DIP("dss %d\n", STRM);
         DIP(" => not implemented\n");
      } else {
         DIP("dssall\n");
         DIP(" => not implemented\n");
      }
      return False;

   default:
      vex_printf("dis_av_datastream(PPC32)(opc2)\n");
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
      vex_printf("dis_av_procctl(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x604: // mfvscr (Move from VSCR, AV p129)
      if (vA_addr != 0 || vB_addr != 0) {
         vex_printf("dis_av_procctl(PPC32)(opc2,dst)\n");
         return False;
      }
      DIP("mfvscr v%d\n", vD_addr);
      putVReg( vD_addr, unop(Iop_32UtoV128, getSPR( PPC32_SPR_VSCR )) ); 
      break;

   case 0x644: { // mtvscr (Move to VSCR, AV p130)
      IRTemp vB = newTemp(Ity_V128);
      if (vD_addr != 0 || vA_addr != 0) {
         vex_printf("dis_av_procctl(PPC32)(opc2,dst)\n");
         return False;
      }
      DIP("mtvscr v%d\n", vB_addr);
      assign( vB, getVReg(vB_addr));
      putSPR( PPC32_SPR_VSCR, unop(Iop_V128to32, mkexpr(vB)) ); 
      break;
   }
   default:
      vex_printf("dis_av_procctl(PPC32)(opc2)\n");
      return False;
   }
   return True;
}

/*
  AltiVec Load Instructions
*/
static Bool dis_av_load ( UInt theInstr )
{
   /* X-Form */
   UChar opc1     = ifieldOPC(theInstr);
   UChar vD_addr  = ifieldRegDS(theInstr);
   UChar rA_addr  = ifieldRegA(theInstr);
   UChar rB_addr  = ifieldRegB(theInstr);
   UInt  opc2     = ifieldOPClo10(theInstr);
   UChar b0       = ifieldBIT0(theInstr);

   IRTemp EA          = newTemp(Ity_I32);
   IRTemp EA_aligned  = newTemp(Ity_I32);

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_av_load(PPC32)(instr)\n");
      return False;
   }

   assign( EA, ea_standard(rA_addr, rB_addr) );

   switch (opc2) {

   case 0x006: { // lvsl (Load Vector for Shift Left, AV p123)
      UInt vD_off = vectorGuestRegOffset(vD_addr);
      IRExpr** args = mkIRExprVec_3(
                         mkU32(vD_off), 
                         binop(Iop_And32, mkexpr(EA), mkU32(0xF)),
                         mkU32(0)/*left*/ );
      IRDirty* d = unsafeIRDirty_0_N (
                      0/*regparms*/, 
                      "ppc32g_dirtyhelper_LVS",
                      &ppc32g_dirtyhelper_LVS,
                      args );
      DIP("lvsl v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
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
      UInt vD_off = vectorGuestRegOffset(vD_addr);
      IRExpr** args = mkIRExprVec_3(
                         mkU32(vD_off), 
                         binop(Iop_And32, mkexpr(EA), mkU32(0xF)),
                         mkU32(1)/*right*/ );
      IRDirty*    d = unsafeIRDirty_0_N (
                         0/*regparms*/, 
                         "ppc32g_dirtyhelper_LVS",
                         &ppc32g_dirtyhelper_LVS,
                         args );
      DIP("lvsr v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
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
      DIP("lvebx v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
      /* loads addressed byte into vector[EA[0:3]
         since all other destination bytes are undefined,
         can simply load entire vector from 16-aligned EA */
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFF0) ));
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_aligned)) );
      break;

   case 0x027: // lvehx (Load Vector Element Half Word Indexed, AV p121)
      DIP("lvehx v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
      /* see note for lvebx */
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFF0) ));
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_aligned)) );
      break;

   case 0x047: // lvewx (Load Vector Element Word Indexed, AV p122)
      DIP("lvewx v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
      /* see note for lvebx */
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFF0) ));
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_aligned)) );
      break;

   case 0x067: // lvx (Load Vector Indexed, AV p127)
      DIP("lvx v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFF0) ));
      putVReg( vD_addr, loadBE(Ity_V128, mkexpr(EA_aligned)) );
      break;

   case 0x167: // lvxl (Load Vector Indexed LRU, AV p128)
     // XXX: lvxl gives explicit control over cache block replacement
      DIP("lvxl v%d,r%d,r%d\n", vD_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;

   default:
      vex_printf("dis_av_load(PPC32)(opc2)\n");
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

   IRTemp vS         = newTemp(Ity_V128);
   IRTemp EA         = newTemp(Ity_I32);
   IRTemp EA_aligned = newTemp(Ity_I32);

   assign( vS, getVReg(vS_addr));
   assign( EA, ea_standard(rA_addr, rB_addr) );

   if (opc1 != 0x1F || b0 != 0) {
      vex_printf("dis_av_store(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x087: { // stvebx (Store Vector Byte Indexed, AV p131)
      IRTemp eb  = newTemp(Ity_I8);
      IRTemp idx = newTemp(Ity_I8);
      DIP("stvebx v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
      assign( eb, binop(Iop_And8, mkU8(0xF),
                        unop(Iop_32to8, mkexpr(EA) )) );
      assign( idx, binop(Iop_Shl8, binop(Iop_Sub8, mkU8(15), mkexpr(eb)),
                         mkU8(3)) );
      storeBE( mkexpr(EA),
               unop(Iop_32to8, unop(Iop_V128to32,
                    binop(Iop_ShrV128, mkexpr(vS), mkexpr(idx)))) );
      break;
   }
   case 0x0A7: { // stvehx (Store Vector Half Word Indexed, AV p132)
      IRTemp eb  = newTemp(Ity_I8);
      IRTemp idx = newTemp(Ity_I8);
      DIP("stvehx v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFFE) ));
      assign( eb, binop(Iop_And8, mkU8(0xF),
                        unop(Iop_32to8, mkexpr(EA_aligned) )) );
      assign( idx, binop(Iop_Shl8, binop(Iop_Sub8, mkU8(14), mkexpr(eb)),
                         mkU8(3)) );
      storeBE( mkexpr(EA_aligned),
               unop(Iop_32to16, unop(Iop_V128to32,
                    binop(Iop_ShrV128, mkexpr(vS), mkexpr(idx)))) );
      break;
   }
   case 0x0C7: { // stvewx (Store Vector Word Indexed, AV p133)
      IRTemp eb  = newTemp(Ity_I8);
      IRTemp idx = newTemp(Ity_I8);
      DIP("stvewx v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFFC) ));
      assign( eb, binop(Iop_And8, mkU8(0xF),
                        unop(Iop_32to8, mkexpr(EA_aligned) )) );
      assign( idx, binop(Iop_Shl8, binop(Iop_Sub8, mkU8(12), mkexpr(eb)),
                         mkU8(3)) );
      storeBE( mkexpr(EA_aligned),
               unop(Iop_V128to32,
                    binop(Iop_ShrV128, mkexpr(vS), mkexpr(idx))) );
      break;
   }

   case 0x0E7: // stvx (Store Vector Indexed, AV p134)
      DIP("stvx v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
      assign( EA_aligned, binop( Iop_And32, mkexpr(EA), mkU32(0xFFFFFFF0) ));
      storeBE( mkexpr(EA_aligned), mkexpr(vS) );
      break;

   case 0x1E7: // stvxl (Store Vector Indexed LRU, AV p135)
     // XXX: stvxl can give explicit control over cache block replacement
      DIP("stvxl v%d,r%d,r%d\n", vS_addr, rA_addr, rB_addr);
      DIP(" => not implemented\n");
      return False;
   
//      EA_aligned = EA & 0xFFFF_FFF0;
//      STORE(vS, 16, EA);

   default:
      vex_printf("dis_av_store(PPC32)(opc2)\n");
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
      vex_printf("dis_av_arith(PPC32)(opc1 != 0x4)\n");
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

   case 0x640: // vsubuhs (Subtract Unsigned Half Word Saturate, AV p268)
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
      putVReg( vD_addr, binop(Iop_MullEven8Ux16, mkexpr(vA), mkexpr(vB)));
      break;

   case 0x048: // vmulouh (Multiply Odd Unsigned Half Word, AV p214)
      DIP("vmulouh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_MullEven16Ux8, mkexpr(vA), mkexpr(vB)));
      break;

   case 0x108: // vmulosb (Multiply Odd Signed Byte, AV p211)
      DIP("vmulosb v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_MullEven8Sx16, mkexpr(vA), mkexpr(vB)));
      break;

   case 0x148: // vmulosh (Multiply Odd Signed Half Word, AV p212)
      DIP("vmulosh v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_MullEven16Sx8, mkexpr(vA), mkexpr(vB)));
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
      vex_printf("dis_av_arith(PPC32)(opc2=0x%x)\n", opc2);
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
      vex_printf("dis_av_logic(PPC32)(opc1 != 0x4)\n");
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
      vex_printf("dis_av_logic(PPC32)(opc2=0x%x)\n", opc2);
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
      vex_printf("dis_av_cmp(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x006: // vcmpequb (Compare Equal-to Unsigned B, AV p160)
      DIP("vcmpequb%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpEQ8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x046: // vcmpequh (Compare Equal-to Unsigned HW, AV p161)
      DIP("vcmpequh%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpEQ16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x086: // vcmpequw (Compare Equal-to Unsigned W, AV p162)
      DIP("vcmpequw%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpEQ32x4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x206: // vcmpgtub (Compare Greater-than Unsigned B, AV p168)
      DIP("vcmpgtub%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT8Ux16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x246: // vcmpgtuh (Compare Greater-than Unsigned HW, AV p169)
      DIP("vcmpgtuh%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT16Ux8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x286: // vcmpgtuw (Compare Greater-than Unsigned W, AV p170)
      DIP("vcmpgtuw%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT32Ux4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x306: // vcmpgtsb (Compare Greater-than Signed B, AV p165)
      DIP("vcmpgtsb%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT8Sx16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x346: // vcmpgtsh (Compare Greater-than Signed HW, AV p166)
      DIP("vcmpgtsh%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT16Sx8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x386: // vcmpgtsw (Compare Greater-than Signed W, AV p167)
      DIP("vcmpgtsw%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT32Sx4, mkexpr(vA), mkexpr(vB)) );
      break;

   default:
      vex_printf("dis_av_cmp(PPC32)(opc2)\n");
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
      vex_printf("dis_av_multarith(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
   /* Multiply-Add */
   case 0x20: { // vmhaddshs (Multiply High, Add Signed HW Saturate, AV p185)
      IRTemp cSigns = newTemp(Ity_V128);
      DIP("vmhaddshs v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
      assign( cSigns, binop(Iop_CmpGT16Sx8, mkexpr(zeros), mkexpr(vC)) );
      assign( aLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros),  mkexpr(vA)) );
      assign( bLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros),  mkexpr(vB)) );
      assign( cLo, binop(Iop_InterleaveLO16x8, mkexpr(cSigns), mkexpr(vC)) );
      assign( aHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros),  mkexpr(vA)) );
      assign( bHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros),  mkexpr(vB)) );
      assign( cHi, binop(Iop_InterleaveHI16x8, mkexpr(cSigns), mkexpr(vC)) );

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

      putVReg( vD_addr, binop(Iop_QNarrow32Sx4, mkexpr(zHi), mkexpr(zLo)) );
      break;
   }
   case 0x21: { // vmhraddshs (Multiply High Round, Add Signed HW Saturate, AV p186)
      IRTemp zKonst = newTemp(Ity_V128);
      IRTemp cSigns = newTemp(Ity_V128);
      DIP("vmhraddshs v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
      assign( cSigns, binop(Iop_CmpGT16Sx8, mkexpr(zeros), mkexpr(vC)) );
      assign( aLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros),  mkexpr(vA)) );
      assign( bLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros),  mkexpr(vB)) );
      assign( cLo, binop(Iop_InterleaveLO16x8, mkexpr(cSigns), mkexpr(vC)) );
      assign( aHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros),  mkexpr(vA)) );
      assign( bHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros),  mkexpr(vB)) );
      assign( cHi, binop(Iop_InterleaveHI16x8, mkexpr(cSigns), mkexpr(vC)) );

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
   case 0x22: { // vmladduhm (Multiply Low, Add Unsigned HW Modulo, AV p194)
      DIP("vmladduhm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
      assign( aLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vA)) );
      assign( bLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vB)) );
      assign( cLo, binop(Iop_InterleaveLO16x8, mkexpr(zeros), mkexpr(vC)) );
      assign( aHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vA)) );
      assign( bHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vB)) );
      assign( cHi, binop(Iop_InterleaveHI16x8, mkexpr(zeros), mkexpr(vC)) );
      assign( zLo, binop(Iop_Add32x4,
                         binop(Iop_MullEven16Ux8, mkexpr(aLo), mkexpr(bLo) ),
                         mkexpr(cLo)) );
      assign( zHi, binop(Iop_Add32x4,
                         binop(Iop_MullEven16Ux8, mkexpr(aHi), mkexpr(bHi) ),
                         mkexpr(cHi)) );
      putVReg( vD_addr, binop(Iop_Narrow32x4, mkexpr(zHi), mkexpr(zLo)) );
      break;
   }


   /* Multiply-Sum */
   case 0x24: { // vmsumubm (Multiply Sum Unsigned B Modulo, AV p204)
      IRTemp abEE, abEO, abOE, abOO;
      abEE = abEO = abOE = abOO = IRTemp_INVALID;
      DIP("vmsumubm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);

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
      DIP("vmsummbm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);

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
      DIP("vmsumuhm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
      assign( abEvn, MK_Iop_MullOdd16Ux8( mkexpr(vA), mkexpr(vB) ));
      assign( abOdd, binop(Iop_MullEven16Ux8, mkexpr(vA), mkexpr(vB)) );
      putVReg( vD_addr,
               binop(Iop_Add32x4, mkexpr(vC),
                     binop(Iop_Add32x4, mkexpr(abEvn), mkexpr(abOdd))) );
      break;
   }
   case 0x27: { // vmsumuhs (Multiply Sum Unsigned HW Saturate, AV p206)
      DIP("vmsumuhs v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
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
      DIP("vmsumshm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
      assign( abEvn, MK_Iop_MullOdd16Sx8( mkexpr(vA), mkexpr(vB) ));
      assign( abOdd, binop(Iop_MullEven16Sx8, mkexpr(vA), mkexpr(vB)) );
      putVReg( vD_addr,
               binop(Iop_Add32x4, mkexpr(vC),
                     binop(Iop_Add32x4, mkexpr(abOdd), mkexpr(abEvn))) );
      break;
   }
   case 0x29: { // vmsumshs (Multiply Sum Signed HW Saturate, AV p203)
      DIP("vmsumshs v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
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
      vex_printf("dis_av_multarith(PPC32)(opc2)\n");
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
      vex_printf("dis_av_shift(PPC32)(instr)\n");
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
   case 0x304: // vsrab (Shift Right Algebraic B, AV p253)
      DIP("vsrab v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sar8x16, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x344: // vsrah (Shift Right Algebraic HW, AV p254)
      DIP("vsrah v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sar16x8, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x384: // vsraw (Shift Right Algebraic W, AV p255)
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
      vex_printf("dis_av_shift(PPC32)(opc2)\n");
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
      vex_printf("dis_av_permute(PPC32)(instr)\n");
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
      DIP("vperm v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr, vC_addr);
      /* Limit the Perm8x16 steering values to 0 .. 15 as that is what
         IR specifies, and also to hide irrelevant bits from
         memcheck */
      assign( vC_andF, binop(Iop_AndV128, mkexpr(vC), 
                                          unop(Iop_Dup8x16, mkU8(0xF))) );
      assign( a_perm, binop(Iop_Perm8x16, mkexpr(vA), mkexpr(vC_andF)) );
      assign( b_perm, binop(Iop_Perm8x16, mkexpr(vB), mkexpr(vC_andF)) );
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
         vex_printf("dis_av_permute(PPC32)(vsldoi)\n");
         return False;
      }
      DIP("vsldoi v%d,v%d,v%d,%d\n", vD_addr, vA_addr, vB_addr, SHB_uimm4);
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
      putVReg( vD_addr, unop(Iop_Dup16x8, mkU16(extend_s_8to32(SIMM_8))) );
      break;

   case 0x38C: // vspltisw (Splat Immediate Signed W, AV p249)
      DIP("vspltisw v%d,%d\n", vD_addr, (Char)SIMM_8);
      putVReg( vD_addr, unop(Iop_Dup32x4, mkU32(extend_s_8to32(SIMM_8))) );
      break;

   default:
      vex_printf("dis_av_permute(PPC32)(opc2)\n");
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
      vex_printf("dis_av_pack(PPC32)(instr)\n");
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
      putVReg( vD_addr, binop(Iop_QNarrow16Ux8, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      return True;

   case 0x0CE: // vpkuwus (Pack Unsigned W Unsigned Saturate, AV p227)
      DIP("vpkuwus v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QNarrow32Ux4, mkexpr(vA), mkexpr(vB)) );
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
      putVReg( vD_addr, binop(Iop_QNarrow16Sx8, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      return True;

   case 0x1CE: // vpkswss (Pack Signed W Signed Saturate, AV p222)
      DIP("vpkswss v%d,v%d,v%d\n", vD_addr, vA_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_QNarrow32Sx4, mkexpr(vA), mkexpr(vB)) );
      // TODO: set VSCR[SAT]
      return True;

   case 0x30E: { // vpkpx (Pack Pixel, AV p219)
      /* CAB: Worth a new primop? */
      /* Using shifts to compact pixel elements, then packing them them */
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
      vex_printf("dis_av_pack(PPC32)(vA_addr)\n");
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
      putVReg( vD_addr, binop(Iop_InterleaveHI8x16, mkexpr(signs), mkexpr(vB)) );
      break;
   }
   case 0x24E: { // vupkhsh (Unpack High Signed HW, AV p278)
      DIP("vupkhsh v%d,v%d\n", vD_addr, vB_addr);
      assign( signs, binop(Iop_CmpGT16Sx8, mkexpr(zeros), mkexpr(vB)) );
      putVReg( vD_addr, binop(Iop_InterleaveHI16x8, mkexpr(signs), mkexpr(vB)) );
      break;
   }
   case 0x28E: { // vupklsb (Unpack Low Signed B, AV p280)
      DIP("vupklsb v%d,v%d\n", vD_addr, vB_addr);
      assign( signs, binop(Iop_CmpGT8Sx16, mkexpr(zeros), mkexpr(vB)) );
      putVReg( vD_addr, binop(Iop_InterleaveLO8x16, mkexpr(signs), mkexpr(vB)) );
      break;
   }
   case 0x2CE: { // vupklsh (Unpack Low Signed HW, AV p281)
      DIP("vupklsh v%d,v%d\n", vD_addr, vB_addr);
      assign( signs, binop(Iop_CmpGT16Sx8, mkexpr(zeros), mkexpr(vB)) );
      putVReg( vD_addr, binop(Iop_InterleaveLO16x8, mkexpr(signs), mkexpr(vB)) );
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
      putVReg( vD_addr, binop(Iop_OrV128,
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
      putVReg( vD_addr, binop(Iop_OrV128,
                              binop(Iop_ShlN32x4, mkexpr(z01), mkU8(16)),
                              mkexpr(z23)) );
      break;
   }
   default:
      vex_printf("dis_av_pack(PPC32)(opc2)\n");
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
      vex_printf("dis_av_fp_arith(PPC32)(instr)\n");
      return False;
   }

   opc2 = IFIELD( theInstr, 0, 6 );
   switch (opc2) {
   case 0x2E: // vmaddfp (Multiply Add FP, AV p177)
      DIP("vmaddfp v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vC_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Add32Fx4, mkexpr(vB),
                              binop(Iop_Mul32Fx4, mkexpr(vA), mkexpr(vC))) );
      return True;

   case 0x2F: { // vnmsubfp (Negative Multiply-Subtract FP, AV p215)
      DIP("vnmsubfp v%d,v%d,v%d,v%d\n", vD_addr, vA_addr, vC_addr, vB_addr);
      putVReg( vD_addr, binop(Iop_Sub32Fx4,
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
      vex_printf("dis_av_fp_arith(PPC32)(vA_addr)\n");
      return False;
   }

   switch (opc2) {
   case 0x10A: // vrefp (Reciprocal Esimate FP, AV p228)
      DIP("vrefp v%d,v%d\n", vD_addr, vB_addr);
      putVReg( vD_addr, unop(Iop_Recip32Fx4, mkexpr(vB)) );
      return True;

   case 0x14A: // vrsqrtefp (Reciprocal Square Root Estimate FP, AV p237)
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
      vex_printf("dis_av_fp_arith(PPC32)(opc2=0x%x)\n",opc2);
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
      vex_printf("dis_av_fp_cmp(PPC32)(instr)\n");
      return False;
   }

   switch (opc2) {
   case 0x0C6: // vcmpeqfp (Compare Equal-to FP, AV p159)
      DIP("vcmpeqfp%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpEQ32Fx4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x1C6: // vcmpgefp (Compare Greater-than-or-Equal-to FP, AV p163)
      DIP("vcmpgefp%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGE32Fx4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x2C6: // vcmpgtfp (Compare Greater-than FP, AV p164)
      DIP("vcmpgtfp%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      assign( vD, binop(Iop_CmpGT32Fx4, mkexpr(vA), mkexpr(vB)) );
      break;

   case 0x3C6: { // vcmpbfp (Compare Bounds FP, AV p157)
      IRTemp gt      = newTemp(Ity_V128);
      IRTemp lt      = newTemp(Ity_V128);
      IRTemp zeros   = newTemp(Ity_V128);
      DIP("vcmpbfp%s v%d,v%d,v%d\n", (flag_rC ? ".":""), vD_addr, vA_addr, vB_addr);
      cmp_bounds = True;
      assign( zeros,   unop(Iop_Dup32x4, mkU32(0)) );

      /* Note: making use of fact that the ppc backend for compare insns
         return zero'd lanes if either of the corresponding arg lanes is a nan.

         Perhaps better to have an irop Iop_isNan32Fx4, but then we'd
         need this for the other compares too (vcmpeqfp etc)...
         Better still, tighten down the spec for compare irops.
       */
      assign( gt, unop(Iop_NotV128,
                       binop(Iop_CmpLE32Fx4, mkexpr(vA), mkexpr(vB))) );
      assign( lt, unop(Iop_NotV128,
                       binop(Iop_CmpGE32Fx4, mkexpr(vA),
                             binop(Iop_Sub32Fx4, mkexpr(zeros), mkexpr(vB)))) );

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
      vex_printf("dis_av_fp_cmp(PPC32)(opc2)\n");
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
   assign( vInvScale, unop(Iop_Dup32x4, mkU32( float_to_bits(inv_scale) )) );

   if (opc1 != 0x4) {
      vex_printf("dis_av_fp_convert(PPC32)(instr)\n");
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
      vex_printf("dis_av_fp_convert(PPC32)(UIMM_5)\n");
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
      vex_printf("dis_av_fp_convert(PPC32)(opc2)\n");
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
DisResult disInstr_PPC32_WRK ( 
             Bool         put_IP,
             Bool         (*resteerOkFn) ( Addr64 ),
             Long         delta64,
             VexArchInfo* archinfo 
          )
{
   UChar     opc1;
   UInt      opc2;
   DisResult dres;
   UInt      theInstr;

   /* What insn variants are we supporting today? */
   Bool allow_FP  = archinfo->subarch == VexSubArchPPC32_FI
                    || archinfo->subarch == VexSubArchPPC32_VFI;

   Bool allow_VMX = archinfo->subarch == VexSubArchPPC32_VFI;

   /* The running delta */
   Int delta = (Int)delta64;

   /* Set result defaults. */
   dres.whatNext   = Dis_Continue;
   dres.len        = 0;
   dres.continueAt = 0;

   /* At least this is simple on PPC32: insns are all 4 bytes long, and
      4-aligned.  So just fish the whole thing out of memory right now
      and have done. */
   theInstr = getUIntBigendianly( (UChar*)(&guest_code[delta]) );

   DIP("\t0x%x:  ", guest_CIA_curr_instr);

   /* We may be asked to update the guest CIA before going further. */
   if (put_IP)
      putSPR( PPC32_SPR_CIA, mkU32(guest_CIA_curr_instr) );

   /* Spot the client-request magic sequence. */
   // Essentially a v. unlikely sequence of noops that we can catch
   {
      UChar* code = (UChar*)(&guest_code[delta]);

      /* Spot this:                                       
         0x7C03D808   tw 0,3,27            => trap word if (0) => nop
         0x5400E800   rlwinm 0,0,29,0,0    => r0 = rotl(r0,29)
         0x54001800   rlwinm 0,0,3,0,0     => r0 = rotl(r0,3)
         0x54006800   rlwinm 0,0,13,0,0    => r0 = rotl(r0,13)
         0x54009800   rlwinm 0,0,19,0,0    => r0 = rotl(r0,19)
         0x60000000   nop
      */
      if (getUIntBigendianly(code+ 0) == 0x7C03D808 &&
          getUIntBigendianly(code+ 4) == 0x5400E800 &&
          getUIntBigendianly(code+ 8) == 0x54001800 &&
          getUIntBigendianly(code+12) == 0x54006800 &&
          getUIntBigendianly(code+16) == 0x54009800 &&
          getUIntBigendianly(code+20) == 0x60000000) {
         DIP("%%r3 = client_request ( %%r31 )\n");
         dres.len = 24;
         delta += 24;

         irbb->next     = mkU32(guest_CIA_bbstart+delta);
         irbb->jumpkind = Ijk_ClientReq;
         dres.whatNext  = Dis_StopHere;
         goto decode_success;
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

   /* Integer Load Instructions */
   case 0x22: case 0x23: case 0x2A: // lbz,  lbzu, lha
   case 0x2B: case 0x28: case 0x29: // lhau, lhz,  lhzu
   case 0x20: case 0x21:            // lwz,  lwzu
      if (dis_int_load( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Store Instructions */
   case 0x26: case 0x27: case 0x2C: // stb,  stbu, sth
   case 0x2D: case 0x24: case 0x25: // sthu, stw,  stwu
      if (dis_int_store( theInstr )) goto decode_success;
      goto decode_failure;

   /* Integer Load and Store Multiple Instructions */
   case 0x2E: case 0x2F: // lmw, stmw
      if (dis_int_ldst_mult( theInstr )) goto decode_success;
      goto decode_failure;

   /* Branch Instructions */
   case 0x12: case 0x10: // b, bc
      if (dis_branch(theInstr, &dres, resteerOkFn)) goto decode_success;
      goto decode_failure;

   /* System Linkage Instructions */
   case 0x11: // sc
      if (dis_syslink(theInstr, &dres)) goto decode_success;
      goto decode_failure;

//zz    /* Trap Instructions */
//zz    case 0x03: // twi
//zz       DIP("trap op (twi) => not implemented\n");
//zz       goto decode_failure;

   /* Floating Point Load Instructions */
   case 0x30: case 0x31: case 0x32: // lfs, lfsu, lfd
   case 0x33:                       // lfdu
      if (!allow_FP) goto decode_failure;
      if (dis_fp_load( theInstr )) goto decode_success;
      goto decode_failure;

   /* Floating Point Store Instructions */
   case 0x34: case 0x35: case 0x36: // stfsx, stfsux, stfdx
   case 0x37:                       // stfdux
      if (!allow_FP) goto decode_failure;
      if (dis_fp_store( theInstr )) goto decode_success;
      goto decode_failure;

   case 0x3B:
      if (!allow_FP) goto decode_failure;

      opc2 = IFIELD(theInstr, 1, 5);
      switch (opc2) {
         /* Floating Point Arith Instructions */
         case 0x12: case 0x14: case 0x15: // fdivs,  fsubs, fadds
         case 0x16: case 0x18: case 0x19: // fsqrts, fres,  fmuls
            if (dis_fp_arith(theInstr)) goto decode_success;
            goto decode_failure;

         /* Floating Point Mult-Add Instructions */
         case 0x1C: case 0x1D: case 0x1E: // fmsubs, fmadds, fnmsubs
         case 0x1F:                       // fnmadds
            if (dis_fp_multadd(theInstr)) goto decode_success;
            goto decode_failure;

         default:
            goto decode_failure;
      }
      break;

   case 0x3F:
      if (!allow_FP) goto decode_failure;
      /* Instrs using opc[1:5] never overlap with instrs using opc[1:10],
         so we can simply fall through the first switch statement */

      opc2 = IFIELD(theInstr, 1, 5);
      switch (opc2) {
         /* Floating Point Arith Instructions */
         case 0x12: case 0x14: case 0x15: // fdiv,  fsub, fadd
         case 0x16: case 0x17: case 0x19: // fsqrt, fsel, fmul
         case 0x1A:                       // frsqrte
            if (dis_fp_arith(theInstr)) goto decode_success;
            goto decode_failure;

         /* Floating Point Mult-Add Instructions */         
         case 0x1C: case 0x1D: case 0x1E: // fmsub, fmadd, fnmsub
         case 0x1F:                       // fnmadd
            if (dis_fp_multadd(theInstr)) goto decode_success;
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
            if (dis_fp_round(theInstr)) goto decode_success;
            goto decode_failure;

         /* Floating Point Move Instructions */         
         case 0x028: // fneg
         case 0x048: // fmr
         case 0x088: // fnabs
         case 0x108: // fabs
            if (dis_fp_move( theInstr )) goto decode_success;
            goto decode_failure;

//zz       /* Floating Point Status/Control Register Instructions */         
//zz       case 0x026: // mtfsb1
//zz       case 0x040: // mcrfs
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
            if (dis_branch(theInstr, &dres, resteerOkFn)) goto decode_success;
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
         if (dis_int_logic( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Shift Instructions */
      case 0x018: case 0x318: case 0x338: // slw, sraw, srawi
      case 0x218:                         // srw
         if (dis_int_shift( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Load Instructions */
      case 0x057: case 0x077: case 0x157: // lbzx,  lbzux, lhax
      case 0x177: case 0x117: case 0x137: // lhaux, lhzx,  lhzux
      case 0x017: case 0x037:             // lwzx,  lwzux
         if (dis_int_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* Integer Store Instructions */
      case 0x0F7: case 0x0D7: case 0x1B7: // stbux, stbx,  sthux
      case 0x197: case 0x0B7: case 0x097: // sthx,  stwux, stwx
         if (dis_int_store( theInstr )) goto decode_success;
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
            irbb->next = mkU32(guest_CIA_curr_instr+4);
            irbb->jumpkind = Ijk_Boring;
            dres.whatNext = Dis_StopHere;
         }
         goto decode_success;
      }

      /* Memory Synchronization Instructions */
      case 0x356: case 0x014: case 0x096: // eieio, lwarx, stwcx.
      case 0x256:                         // sync
         if (dis_memsync( theInstr )) goto decode_success;
         goto decode_failure;
         
      /* Processor Control Instructions */
      case 0x200: case 0x013: case 0x153: // mcrxr, mfcr,  mfspr
      case 0x173: case 0x090: case 0x1D3: // mftb,  mtcrf, mtspr
         if (dis_proc_ctl( theInstr )) goto decode_success;
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
//zz 
//zz       /* Trap Instructions */
//zz       case 0x004:                         // tw
//zz          DIP("trap op (tw) => not implemented\n");
//zz          goto decode_failure;

      /* Floating Point Load Instructions */
      case 0x217: case 0x237: case 0x257: // lfsx, lfsux, lfdx
      case 0x277:                         // lfdux
         if (!allow_FP) goto decode_failure;
         if (dis_fp_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* Floating Point Store Instructions */
      case 0x297: case 0x2B7: case 0x2D7: // stfs,  stfsu, stfd
      case 0x2F7: case 0x3D7:             // stfdu, stfiwx
         if (!allow_FP) goto decode_failure;
         if (dis_fp_store( theInstr )) goto decode_success;
         goto decode_failure;


      /* AltiVec instructions */

      /* AV Cache Control - Data streams */
      case 0x156: case 0x176: case 0x336: // dst, dstst, dss
         if (!allow_VMX) goto decode_failure;
         if (dis_av_datastream( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Load */
      case 0x006: case 0x026:             // lvsl, lvsr
      case 0x007: case 0x027: case 0x047: // lvebx, lvehx, lvewx
      case 0x067: case 0x167:             // lvx, lvxl
         if (!allow_VMX) goto decode_failure;
         if (dis_av_load( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Store */
      case 0x087: case 0x0A7: case 0x0C7: // stvebx, stvehx, stvewx
      case 0x0E7: case 0x1E7:             // stvx, stvxl
         if (!allow_VMX) goto decode_failure;
         if (dis_av_store( theInstr )) goto decode_success;
         goto decode_failure;

      default:
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
         if (!allow_VMX) goto decode_failure;
         if (dis_av_multarith( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Permutations */
      case 0x2A:                       // vsel
      case 0x2B:                       // vperm
      case 0x2C:                       // vsldoi
         if (!allow_VMX) goto decode_failure;
         if (dis_av_permute( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Floating Point Mult-Add/Sub */
      case 0x2E: case 0x2F:            // vmaddfp, vnmsubfp
         if (!allow_VMX) goto decode_failure;
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
         if (!allow_VMX) goto decode_failure;
         if (dis_av_arith( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Rotate, Shift */
      case 0x004: case 0x044: case 0x084: // vrlb, vrlh, vrlw
      case 0x104: case 0x144: case 0x184: // vslb, vslh, vslw
      case 0x204: case 0x244: case 0x284: // vsrb, vsrh, vsrw
      case 0x304: case 0x344: case 0x384: // vsrab, vsrah, vsraw
      case 0x1C4: case 0x2C4:             // vsl, vsr
      case 0x40C: case 0x44C:             // vslo, vsro
         if (!allow_VMX) goto decode_failure;
         if (dis_av_shift( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Logic */
      case 0x404: case 0x444: case 0x484: // vand, vandc, vor
      case 0x4C4: case 0x504:             // vxor, vnor
         if (!allow_VMX) goto decode_failure;
         if (dis_av_logic( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Processor Control */
      case 0x604: case 0x644:             // mfvscr, mtvscr
         if (!allow_VMX) goto decode_failure;
         if (dis_av_procctl( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Floating Point Arithmetic */
      case 0x00A: case 0x04A:             // vaddfp, vsubfp
      case 0x10A: case 0x14A: case 0x18A: // vrefp, vrsqrtefp, vexptefp
      case 0x1CA:                         // vlogefp
      case 0x40A: case 0x44A:             // vmaxfp, vminfp
         if (!allow_VMX) goto decode_failure;
         if (dis_av_fp_arith( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Floating Point Round/Convert */
      case 0x20A: case 0x24A: case 0x28A: // vrfin, vrfiz, vrfip
      case 0x2CA:                         // vrfim
      case 0x30A: case 0x34A: case 0x38A: // vcfux, vcfsx, vctuxs
      case 0x3CA:                         // vctsxs
         if (!allow_VMX) goto decode_failure;
         if (dis_av_fp_convert( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Merge, Splat */
      case 0x00C: case 0x04C: case 0x08C: // vmrghb, vmrghh, vmrghw
      case 0x10C: case 0x14C: case 0x18C: // vmrglb, vmrglh, vmrglw
      case 0x20C: case 0x24C: case 0x28C: // vspltb, vsplth, vspltw
      case 0x30C: case 0x34C: case 0x38C: // vspltisb, vspltish, vspltisw
         if (!allow_VMX) goto decode_failure;
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
         if (!allow_VMX) goto decode_failure;
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
         if (!allow_VMX) goto decode_failure;
         if (dis_av_cmp( theInstr )) goto decode_success;
         goto decode_failure;

      /* AV Floating Point Compare */
      case 0x0C6: case 0x1C6: case 0x2C6: // vcmpeqfp, vcmpgefp, vcmpgtfp
      case 0x3C6:                         // vcmpbfp
         if (!allow_VMX) goto decode_failure;
         if (dis_av_fp_cmp( theInstr )) goto decode_success;
         goto decode_failure;

      default:
         goto decode_failure;
      }
      break;

   default:
   decode_failure:
   /* All decode failures end up here. */
   opc2 = (theInstr) & 0x7FF;
   vex_printf("disInstr(ppc32): unhandled instruction: "
              "0x%x\n", theInstr);
   vex_printf("                 primary %d(0x%x), secondary %u(0x%x)\n", 
              opc1, opc1, opc2, opc2);
   
#if PPC32_TOIR_DEBUG
   vex_printf("disInstr(ppc32): instr:   ");
   vex_printf_binary( theInstr, 32, True );
   vex_printf("\n");

   vex_printf("disInstr(ppc32): opcode1: ");
   vex_printf_binary( opc1, 6, False );
   vex_printf("\n");

   vex_printf("disInstr(ppc32): opcode2: ");
   vex_printf_binary( opc2, 10, False );
   vex_printf("\n\n");
#endif


   /* Tell the dispatcher that this insn cannot be decoded, and so has
      not been executed, and (is currently) the next to be executed.
      CIA should be up-to-date since it made so at the start of each
      insn, but nevertheless be paranoid and update it again right
      now. */
   putSPR( PPC32_SPR_CIA, mkU32(guest_CIA_curr_instr) );
   irbb->next = mkU32(guest_CIA_curr_instr);
   irbb->jumpkind = Ijk_NoDecode;
   dres.whatNext = Dis_StopHere;
   dres.len = 0;
   return dres;

   } /* switch (opc) for the main (primary) opcode switch. */

  decode_success:
   /* All decode successes end up here. */
   DIP("\n");

   dres.len = 4;
   return dres;
}

#undef DIP
#undef DIS


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult disInstr_PPC32 ( IRBB*        irbb_IN,
                           Bool         put_IP,
                           Bool         (*resteerOkFn) ( Addr64 ),
                           UChar*       guest_code_IN,
                           Long         delta,
                           Addr64       guest_IP,
                           VexArchInfo* archinfo,
                           Bool         host_bigendian_IN )
{
   DisResult dres;

   /* Set globals (see top of this file) */
   guest_code           = guest_code_IN;
   irbb                 = irbb_IN;
   host_is_bigendian    = host_bigendian_IN;
   guest_CIA_curr_instr = (Addr32)guest_IP;
   guest_CIA_bbstart    = (Addr32)toUInt(guest_IP - delta);

   dres = disInstr_PPC32_WRK ( put_IP, resteerOkFn,
                               delta, archinfo );

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
/*--- end                                       guest-ppc32/toIR.c ---*/
/*--------------------------------------------------------------------*/
