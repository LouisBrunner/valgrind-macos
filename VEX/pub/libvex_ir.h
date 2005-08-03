
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_ir.h) is                              ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

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

#ifndef __LIBVEX_IR_H
#define __LIBVEX_IR_H

#include "libvex_basictypes.h"



/*---------------------------------------------------------------*/
/*--- Type definitions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* General comments about naming schemes:

   All publically visible functions contain the name of the primary
   type on which they operate (IRFoo, IRBar, etc).  Hence you should
   be able to identify these functions by grepping for "IR[A-Z]".

   For some type 'IRFoo':

   - ppIRFoo is the printing method for IRFoo, printing it to the
     output channel specified in the LibVEX_Initialise call.

   - eqIRFoo is a structural equality predicate for IRFoos.

   - dopyIRFoo is a deep copy constructor for IRFoos. 
     It recursively traverses the entire argument tree and
     produces a complete new tree.

   - sopyIRFoo is the shallow copy constructor for IRFoos.
     It creates a new top-level copy of the supplied object,
     but does not copy any sub-objects.
*/

/* ------------------ Types ------------------ */

typedef 
   enum { 
      Ity_INVALID=0x10FFF,
      Ity_I1=0x11000, 
      Ity_I8, 
      Ity_I16, 
      Ity_I32, 
      Ity_I64,
      Ity_I128,  /* 128-bit scalar */
      Ity_F32,   /* IEEE 754 float */
      Ity_F64,   /* IEEE 754 double */
      Ity_V128   /* 128-bit SIMD */
   }
   IRType;

extern void ppIRType ( IRType );
extern Int  sizeofIRType ( IRType );


/* ------------------ Endianness ------------------ */

typedef
   enum { 
      Iend_LE=22, /* little endian */
      Iend_BE=33 /* big endian */
   }
   IREndness;


/* ------------------ Constants ------------------ */

typedef
   enum { 
      Ico_U1=0x12000,
      Ico_U8, 
      Ico_U16, 
      Ico_U32, 
      Ico_U64,
      Ico_F64,   /* 64-bit IEEE754 floating */
      Ico_F64i,  /* 64-bit unsigned int to be interpreted literally
                    as a IEEE754 double value. */
      Ico_V128   /* 128-bit restricted vector constant, with 1 bit for
                    each of 16 byte lanes */
   }
   IRConstTag;

typedef
   struct _IRConst {
      IRConstTag tag;
      union {
         Bool   U1;
         UChar  U8;
         UShort U16;
         UInt   U32;
         ULong  U64;
         Double F64;
         ULong  F64i;
         UShort V128;
      } Ico;
   }
   IRConst;

extern IRConst* IRConst_U1   ( Bool );
extern IRConst* IRConst_U8   ( UChar );
extern IRConst* IRConst_U16  ( UShort );
extern IRConst* IRConst_U32  ( UInt );
extern IRConst* IRConst_U64  ( ULong );
extern IRConst* IRConst_F64  ( Double );
extern IRConst* IRConst_F64i ( ULong );
extern IRConst* IRConst_V128 ( UShort );

extern IRConst* dopyIRConst ( IRConst* );

extern void ppIRConst ( IRConst* );
extern Bool eqIRConst ( IRConst*, IRConst* );


/* ------------------ Call targets ------------------ */

/* Describes a helper function to call.  The name part is purely for
   pretty printing and not actually used.  regparms=n tells the back
   end that the callee has been declared
   "__attribute__((regparm(n)))".  On some targets (x86) the back end
   will need to construct a non-standard sequence to call a function
   declared like this. 

   mcx_mask is a sop to Memcheck.  It indicates which args should be
   considered 'always defined' when lazily computing definedness of
   the result.  Bit 0 of mcx_mask corresponds to args[0], bit 1 to
   args[1], etc.  If a bit is set, the corresponding arg is excluded
   (hence "x" in "mcx") from definedness checking.  
*/

typedef
   struct {
      Int    regparms;
      HChar* name;
      void*  addr;
      UInt   mcx_mask;
   }
   IRCallee;

extern IRCallee* mkIRCallee ( Int regparms, HChar* name, void* addr );

extern IRCallee* dopyIRCallee ( IRCallee* );

extern void ppIRCallee ( IRCallee* );


/* ------------------ Guest state arrays ------------------ */

typedef
   struct {
      Int    base;
      IRType elemTy;
      Int    nElems;
   }
   IRArray;

extern IRArray* mkIRArray ( Int, IRType, Int );

extern IRArray* dopyIRArray ( IRArray* );

extern void ppIRArray ( IRArray* );
extern Bool eqIRArray ( IRArray*, IRArray* );


/* ------------------ Temporaries ------------------ */

/* The IR optimiser relies on the fact that IRTemps are 32-bit
   ints.  Do not change them to be ints of any other size. */
typedef UInt IRTemp;

extern void ppIRTemp ( IRTemp );

#define IRTemp_INVALID ((IRTemp)0xFFFFFFFF)


/* ------------------ Binary and unary ops ------------------ */

typedef
   enum { 
      /* -- Do not change this ordering.  The IR generators rely on
            (eg) Iop_Add64 == IopAdd8 + 3. -- */

      Iop_INVALID=0x13000,
      Iop_Add8,  Iop_Add16,  Iop_Add32,  Iop_Add64,
      Iop_Sub8,  Iop_Sub16,  Iop_Sub32,  Iop_Sub64,
      /* Signless mul.  MullS/MullU is elsewhere. */
      Iop_Mul8,  Iop_Mul16,  Iop_Mul32,  Iop_Mul64,
      Iop_Or8,   Iop_Or16,   Iop_Or32,   Iop_Or64,
      Iop_And8,  Iop_And16,  Iop_And32,  Iop_And64,
      Iop_Xor8,  Iop_Xor16,  Iop_Xor32,  Iop_Xor64,
      Iop_Shl8,  Iop_Shl16,  Iop_Shl32,  Iop_Shl64,
      Iop_Shr8,  Iop_Shr16,  Iop_Shr32,  Iop_Shr64,
      Iop_Sar8,  Iop_Sar16,  Iop_Sar32,  Iop_Sar64,
      /* Integer comparisons. */
      Iop_CmpEQ8,  Iop_CmpEQ16,  Iop_CmpEQ32,  Iop_CmpEQ64,
      Iop_CmpNE8,  Iop_CmpNE16,  Iop_CmpNE32,  Iop_CmpNE64,
      /* Tags for unary ops */
      Iop_Not8,  Iop_Not16,  Iop_Not32,  Iop_Not64,
      Iop_Neg8,  Iop_Neg16,  Iop_Neg32,  Iop_Neg64,

      /* -- Ordering not important after here. -- */

      /* Widening multiplies */
      Iop_MullS8, Iop_MullS16, Iop_MullS32, Iop_MullS64,
      Iop_MullU8, Iop_MullU16, Iop_MullU32, Iop_MullU64,

      /* Wierdo integer stuff */
      Iop_Clz64, Iop_Clz32,   /* count leading zeroes */
      Iop_Ctz64, Iop_Ctz32,   /* count trailing zeros */
      /* Ctz64/Ctz32/Clz64/Clz32 are UNDEFINED when given arguments of
         zero.  You must ensure they are never given a zero argument.
      */

      /* Standard integer comparisons */
      Iop_CmpLT32S, Iop_CmpLT64S,
      Iop_CmpLE32S, Iop_CmpLE64S,
      Iop_CmpLT32U, Iop_CmpLT64U,
      Iop_CmpLE32U, Iop_CmpLE64U,

      /* As a sop to Valgrind-Memcheck, the following are useful. */
      Iop_CmpNEZ8, Iop_CmpNEZ16,  Iop_CmpNEZ32,  Iop_CmpNEZ64,

      /* PowerPC-style 3-way integer comparisons.  Without them it is difficult
         to simulate PPC efficiently.
         op(x,y) | x < y  = 0x8 else 
                 | x > y  = 0x4 else
                 | x == y = 0x2
      */
      Iop_CmpORD32U, Iop_CmpORD32S,

      /* Division */
      /* TODO: clarify semantics wrt rounding, negative values, whatever */
      Iop_DivU32,   // :: I32,I32 -> I32 (simple div, no mod)
      Iop_DivS32,   // ditto, signed

      Iop_DivModU64to32, // :: I64,I32 -> I64
                         // of which lo half is div and hi half is mod
      Iop_DivModS64to32, // ditto, signed

      Iop_DivModU128to64, // :: V128,I64 -> V128
                          // of which lo half is div and hi half is mod
      Iop_DivModS128to64, // ditto, signed

      /* Integer conversions.  Some of these are redundant (eg
         Iop_64to8 is the same as Iop_64to32 and then Iop_32to8), but
         having a complete set reduces the typical dynamic size of IR
         and makes the instruction selectors easier to write. */

      /* Widening conversions */
      Iop_8Uto16, Iop_8Uto32,  Iop_8Uto64,
                  Iop_16Uto32, Iop_16Uto64,
                               Iop_32Uto64,
      Iop_8Sto16, Iop_8Sto32,  Iop_8Sto64,
                  Iop_16Sto32, Iop_16Sto64,
                               Iop_32Sto64,

      /* Narrowing conversions */
      Iop_64to8, Iop_32to8, Iop_64to16,
      /* 8 <-> 16 bit conversions */
      Iop_16to8,      // :: I16 -> I8, low half
      Iop_16HIto8,    // :: I16 -> I8, high half
      Iop_8HLto16,    // :: (I8,I8) -> I16
      /* 16 <-> 32 bit conversions */
      Iop_32to16,     // :: I32 -> I16, low half
      Iop_32HIto16,   // :: I32 -> I16, high half
      Iop_16HLto32,   // :: (I16,I16) -> I32
      /* 32 <-> 64 bit conversions */
      Iop_64to32,     // :: I64 -> I32, low half
      Iop_64HIto32,   // :: I64 -> I32, high half
      Iop_32HLto64,   // :: (I32,I32) -> I64
      /* 64 <-> 128 bit conversions */
      Iop_128to64,    // :: I128 -> I64, low half
      Iop_128HIto64,  // :: I128 -> I64, high half
      Iop_64HLto128,  // :: (I64,I64) -> I128
      /* 1-bit stuff */
      Iop_Not1,   /* :: Ity_Bit -> Ity_Bit */
      Iop_32to1,  /* :: Ity_I32 -> Ity_Bit, just select bit[0] */
      Iop_64to1,  /* :: Ity_I64 -> Ity_Bit, just select bit[0] */
      Iop_1Uto8,  /* :: Ity_Bit -> Ity_I8,  unsigned widen */
      Iop_1Uto32, /* :: Ity_Bit -> Ity_I32, unsigned widen */
      Iop_1Uto64, /* :: Ity_Bit -> Ity_I64, unsigned widen */
      Iop_1Sto8,  /* :: Ity_Bit -> Ity_I8,  signed widen */
      Iop_1Sto16, /* :: Ity_Bit -> Ity_I16, signed widen */
      Iop_1Sto32, /* :: Ity_Bit -> Ity_I32, signed widen */
      Iop_1Sto64, /* :: Ity_Bit -> Ity_I64, signed widen */

      /* ------ Floating point.  We try and be IEEE754 compliant. ------ */

      /* Binary operations mandated by IEEE754. */
      Iop_AddF64, Iop_SubF64, Iop_MulF64, Iop_DivF64, /* Iop_RemF64, */

      /* Binary ops supported by IA32 but not mandated by 754. */
      Iop_AtanF64,       /* FPATAN,  arctan(arg1/arg2)       */
      Iop_Yl2xF64,       /* FYL2X,   arg1 * log2(arg2)       */
      Iop_Yl2xp1F64,     /* FYL2XP1, arg1 * log2(arg2+1.0)   */
      Iop_PRemF64,       /* FPREM,   non-IEEE remainder(arg1/arg2)    */
      Iop_PRemC3210F64,  /* C3210 flags resulting from FPREM, :: I32 */
      Iop_PRem1F64,      /* FPREM1,  IEEE remainder(arg1/arg2)    */
      Iop_PRem1C3210F64, /* C3210 flags resulting from FPREM1, :: I32 */
      Iop_ScaleF64,      /* FSCALE,  arg1 * (2^RoundTowardsZero(arg2)) */
      /* Note that on x86 guest, PRem1{C3210} has the same behaviour
         as the IEEE mandated RemF64, except it is limited in the
         range of its operand.  Hence the partialness. */

      /* Unary operations mandated by IEEE754. */
      Iop_NegF64, Iop_SqrtF64, 

      /* Unary ops supported by IA32 but not mandated by 754. */
      Iop_AbsF64,    /* FABS */
      Iop_SinF64,    /* FSIN */
      Iop_CosF64,    /* FCOS */
      Iop_TanF64,    /* FTAN */
      Iop_2xm1F64,   /* (2^arg - 1.0) */

      /* Comparison, yielding GT/LT/EQ/UN(ordered), as per the following:
            0x45 Unordered
            0x01 LT
            0x00 GT
            0x40 EQ
         This just happens to be the Intel encoding.  The values
         are recorded in the type IRCmpF64Result.
      */
      Iop_CmpF64,

      /* --- Int to/from FP conversions. --- */
      /* For the most part, these take a first argument :: Ity_I32
         (as IRRoundingMode) which is an indication of the rounding
         mode to use, as per the following encoding:
            00b  to nearest (the default)
            01b  to -infinity
            10b  to +infinity
            11b  to zero
         This just happens to be the Intel encoding.  For reference only,
         the PPC encoding is:
            00b  to nearest (the default)
            01b  to zero
            10b  to +infinity
            11b  to -infinity
         Any PPC -> IR front end will have to translate these PPC
         encodings to the standard encodings.

         If one of these conversions gets an out-of-range condition,
         or a NaN, as an argument, the result is host-defined.  On x86
         the "integer indefinite" value 0x80..00 is produced.
         On PPC it is either 0x80..00 or 0x7F..FF depending on the sign
         of the argument.

         Rounding is required whenever the destination type cannot
         represent exactly all values of the source type.
      */
      Iop_F64toI16,  /* IRRoundingMode(I32) x F64 -> I16 */
      Iop_F64toI32,  /* IRRoundingMode(I32) x F64 -> I32 */
      Iop_F64toI64,  /* IRRoundingMode(I32) x F64 -> I64 */

      Iop_I16toF64,  /*                       I16 -> F64 */
      Iop_I32toF64,  /*                       I32 -> F64 */
      Iop_I64toF64,  /* IRRoundingMode(I32) x I64 -> F64 */

      Iop_F32toF64,  /*                       F32 -> F64 */
      Iop_F64toF32,  /* IRRoundingMode(I32) x F64 -> F32 */

      /* F64 -> F64, also takes an I32 first argument encoding the
         rounding mode. */
      Iop_RoundF64,

      /* Reinterpretation.  Take an F64 and produce an I64 with 
         the same bit pattern, or vice versa. */
      Iop_ReinterpF64asI64, Iop_ReinterpI64asF64,
      Iop_ReinterpF32asI32, Iop_ReinterpI32asF32,

      /* ------------------ 64-bit SIMD Integer. ------------------ */

      /* MISC (vector integer cmp != 0) */
      Iop_CmpNEZ8x8, Iop_CmpNEZ16x4, Iop_CmpNEZ32x2,

      /* ADDITION (normal / unsigned sat / signed sat) */
      Iop_Add8x8,   Iop_Add16x4,   Iop_Add32x2,
      Iop_QAdd8Ux8, Iop_QAdd16Ux4,
      Iop_QAdd8Sx8, Iop_QAdd16Sx4,

      /* SUBTRACTION (normal / unsigned sat / signed sat) */
      Iop_Sub8x8,   Iop_Sub16x4,   Iop_Sub32x2,
      Iop_QSub8Ux8, Iop_QSub16Ux4,
      Iop_QSub8Sx8, Iop_QSub16Sx4,

      /* MULTIPLICATION (normal / high half of signed/unsigned) */
      Iop_Mul16x4,
      Iop_MulHi16Ux4,
      Iop_MulHi16Sx4,

      /* AVERAGING: note: (arg1 + arg2 + 1) >>u 1 */
      Iop_Avg8Ux8,
      Iop_Avg16Ux4,

      /* MIN/MAX */
      Iop_Max16Sx4,
      Iop_Max8Ux8,
      Iop_Min16Sx4,
      Iop_Min8Ux8,

      /* COMPARISON */
      Iop_CmpEQ8x8,  Iop_CmpEQ16x4,  Iop_CmpEQ32x2,
      Iop_CmpGT8Sx8, Iop_CmpGT16Sx4, Iop_CmpGT32Sx2,

      /* VECTOR x SCALAR SHIFT (shift amt :: Ity_I8) */
      Iop_ShlN16x4, Iop_ShlN32x2,
      Iop_ShrN16x4, Iop_ShrN32x2,
      Iop_SarN16x4, Iop_SarN32x2,

      /* NARROWING -- narrow 2xI64 into 1xI64, hi half from left arg */
      Iop_QNarrow16Ux4,
      Iop_QNarrow16Sx4,
      Iop_QNarrow32Sx2,

      /* INTERLEAVING -- interleave lanes from low or high halves of
         operands.  Most-significant result lane is from the left
         arg. */
      Iop_InterleaveHI8x8, Iop_InterleaveHI16x4, Iop_InterleaveHI32x2,
      Iop_InterleaveLO8x8, Iop_InterleaveLO16x4, Iop_InterleaveLO32x2,

      /* ------------------ 128-bit SIMD FP. ------------------ */

      /* --- 32x4 vector FP --- */

      /* binary */
      Iop_Add32Fx4, Iop_Sub32Fx4, Iop_Mul32Fx4, Iop_Div32Fx4, 
      Iop_Max32Fx4, Iop_Min32Fx4,
      Iop_CmpEQ32Fx4, Iop_CmpLT32Fx4, Iop_CmpLE32Fx4, Iop_CmpUN32Fx4, 

      /* unary */
      Iop_Recip32Fx4, Iop_Sqrt32Fx4, Iop_RSqrt32Fx4,

      /* --- 32x4 lowest-lane-only scalar FP --- */

      /* In binary cases, upper 3/4 is copied from first operand.  In
         unary cases, upper 3/4 is copied from the operand. */

      /* binary */
      Iop_Add32F0x4, Iop_Sub32F0x4, Iop_Mul32F0x4, Iop_Div32F0x4, 
      Iop_Max32F0x4, Iop_Min32F0x4,
      Iop_CmpEQ32F0x4, Iop_CmpLT32F0x4, Iop_CmpLE32F0x4, Iop_CmpUN32F0x4, 

      /* unary */
      Iop_Recip32F0x4, Iop_Sqrt32F0x4, Iop_RSqrt32F0x4,

      /* --- 64x2 vector FP --- */

      /* binary */
      Iop_Add64Fx2, Iop_Sub64Fx2, Iop_Mul64Fx2, Iop_Div64Fx2, 
      Iop_Max64Fx2, Iop_Min64Fx2,
      Iop_CmpEQ64Fx2, Iop_CmpLT64Fx2, Iop_CmpLE64Fx2, Iop_CmpUN64Fx2, 

      /* unary */
      Iop_Recip64Fx2, Iop_Sqrt64Fx2, Iop_RSqrt64Fx2,

      /* --- 64x2 lowest-lane-only scalar FP --- */

      /* In binary cases, upper half is copied from first operand.  In
         unary cases, upper half is copied from the operand. */

      /* binary */
      Iop_Add64F0x2, Iop_Sub64F0x2, Iop_Mul64F0x2, Iop_Div64F0x2, 
      Iop_Max64F0x2, Iop_Min64F0x2,
      Iop_CmpEQ64F0x2, Iop_CmpLT64F0x2, Iop_CmpLE64F0x2, Iop_CmpUN64F0x2, 

      /* unary */
      Iop_Recip64F0x2, Iop_Sqrt64F0x2, Iop_RSqrt64F0x2,

      /* --- pack / unpack --- */

      /* 64 <-> 128 bit vector */
      Iop_V128to64,     // :: V128 -> I64, low half
      Iop_V128HIto64,   // :: V128 -> I64, high half
      Iop_64HLtoV128,   // :: (I64,I64) -> V128

      Iop_64UtoV128,
      Iop_SetV128lo64,

      /* 32 <-> 128 bit vector */
      Iop_32UtoV128,
      Iop_V128to32,     // :: V128 -> I32, lowest lane
      Iop_SetV128lo32,  // :: (V128,I32) -> V128

      /* ------------------ 128-bit SIMD Integer. ------------------ */

      /* BITWISE OPS */
      Iop_NotV128,
      Iop_AndV128, Iop_OrV128, Iop_XorV128, 

      /* MISC (vector integer cmp != 0) */
      Iop_CmpNEZ8x16, Iop_CmpNEZ16x8, Iop_CmpNEZ32x4, Iop_CmpNEZ64x2,

      /* ADDITION (normal / unsigned sat / signed sat) */
      Iop_Add8x16,   Iop_Add16x8,   Iop_Add32x4,  Iop_Add64x2,
      Iop_QAdd8Ux16, Iop_QAdd16Ux8,
      Iop_QAdd8Sx16, Iop_QAdd16Sx8,

      /* SUBTRACTION (normal / unsigned sat / signed sat) */
      Iop_Sub8x16,   Iop_Sub16x8,   Iop_Sub32x4,  Iop_Sub64x2,
      Iop_QSub8Ux16, Iop_QSub16Ux8,
      Iop_QSub8Sx16, Iop_QSub16Sx8,

      /* MULTIPLICATION (normal / high half of signed/unsigned) */
      Iop_Mul16x8,
      Iop_MulHi16Ux8,
      Iop_MulHi16Sx8,

      /* AVERAGING: note: (arg1 + arg2 + 1) >>u 1 */
      Iop_Avg8Ux16,
      Iop_Avg16Ux8,

      /* MIN/MAX */
      Iop_Max16Sx8,
      Iop_Max8Ux16,
      Iop_Min16Sx8,
      Iop_Min8Ux16,

      /* COMPARISON */
      Iop_CmpEQ8x16,  Iop_CmpEQ16x8,  Iop_CmpEQ32x4,
      Iop_CmpGT8Sx16, Iop_CmpGT16Sx8, Iop_CmpGT32Sx4,

      /* VECTOR x SCALAR SHIFT (shift amt :: Ity_I8) */
      Iop_ShlN16x8, Iop_ShlN32x4, Iop_ShlN64x2,
      Iop_ShrN16x8, Iop_ShrN32x4, Iop_ShrN64x2,
      Iop_SarN16x8, Iop_SarN32x4,

      /* NARROWING -- narrow 2xV128 into 1xV128, hi half from left arg */
      Iop_QNarrow16Ux8,
      Iop_QNarrow16Sx8,
      Iop_QNarrow32Sx4,

      /* INTERLEAVING -- interleave lanes from low or high halves of
         operands.  Most-significant result lane is from the left
         arg. */
      Iop_InterleaveHI8x16, Iop_InterleaveHI16x8,
      Iop_InterleaveHI32x4, Iop_InterleaveHI64x2,
      Iop_InterleaveLO8x16, Iop_InterleaveLO16x8, 
      Iop_InterleaveLO32x4, Iop_InterleaveLO64x2
   }
   IROp;

extern void ppIROp ( IROp );


/* Encoding of IEEE754-specified rounding modes in Float -> Int
   conversions.  This is the same as the encoding used by Intel IA32
   to indicate x87 rounding mode. */
typedef
   enum { Irrm_NEAREST=0, Irrm_NegINF=1, Irrm_PosINF=2, Irrm_ZERO=3 }
   IRRoundingMode;

/* Floating point comparison result values, as created by Iop_CmpF64.
   This is also derived from what IA32 does. */
typedef
   enum {
      Ircr_UN = 0x45,
      Ircr_LT = 0x01,
      Ircr_GT = 0x00,
      Ircr_EQ = 0x40
   }
   IRCmpF64Result;


/* ------------------ Expressions ------------------ */
/* 
   Some details of expression semantics:

   IRExpr_GetI (also IRStmt_PutI)
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   These allow circular indexing into parts of the guest state, which
   is essential for modelling situations where the identity of guest
   registers is not known until run time.  One example is the x87 FP
   register stack.

   The part of the guest state to be treated as a circular array is
   described in an IRArray structure attached to the GetI/PutI.
   IRArray holds the offset of the first element in the array, the
   type of each element, and the number of elements.

   The array index is indicated rather indirectly, in a way which
   makes optimisation easy: as the sum of variable part (the 'ix'
   field) and a constant offset (the 'bias' field).

   Since the indexing is circular, the actual array index to use
   is computed as (ix + bias) % number-of-elements-in-the-array.

   Here's an example.  The description

      (96:8xF64)[t39,-7]

   describes an array of 8 F64-typed values, the guest-state-offset
   of the first being 96.  This array is being indexed at
   (t39 - 7) % 8.

   It is important to get the array size/type exactly correct since IR
   optimisation looks closely at such info in order to establish
   aliasing/non-aliasing between seperate GetI and PutI events, which
   is used to establish when they can be reordered, etc.  Putting
   incorrect info in will lead to obscure IR optimisation bugs.

   IRExpr_CCall
   ~~~~~~~~~~~~
   The name is the C helper function; the backends will call back to
   the front ends to get the address of a host-code helper function to
   be called.

   The args are a NULL-terminated array of arguments.  The stated
   return IRType, and the implied argument types, must match that of
   the function being called well enough so that the back end can
   actually generate correct code for the call.

   The called function **must** satisfy the following:

   * no side effects -- must be a pure function, the result of which
     depends only on the passed parameters.

   * it may not look at, nor modify, any of the guest state since that
     would hide guest state transitions from instrumenters

   * it may not access guest memory, since that would hide guest
     memory transactions from the instrumenters

   This is restrictive, but makes the semantics clean, and does
   not interfere with IR optimisation.

   If you want to call a helper which can mess with guest state and/or
   memory, instead use IRStmt_Dirty.  This is a lot more flexible, but
   you pay for that flexibility in that you have to give a bunch of
   details about what the helper does (and you better be telling the
   truth, otherwise any derived instrumentation will be wrong).  Also
   IRStmt_Dirty inhibits various IR optimisations and so can cause
   quite poor code to be generated.  Try to avoid it.  */

/* The possible kinds of expressions are as follows: */
typedef
   enum { 
      Iex_Binder,  /* Used only in pattern matching.  
                      Not an expression. */
      Iex_Get,     /* read guest state, fixed offset */
      Iex_GetI,    /* read guest state, run-time offset */
      Iex_Tmp,     /* value of temporary */
      Iex_Binop,   /* binary operation */
      Iex_Unop,    /* unary operation */
      Iex_Load,    /* read from memory */ 
      Iex_Const,   /* constant-valued expression */
      Iex_Mux0X,   /* ternary if-then-else operator (STRICT) */
      Iex_CCall    /* call to pure (side-effect-free) helper fn */
   }
   IRExprTag;

typedef 
   struct _IRExpr {
      IRExprTag tag;
      union {
         struct {
            Int binder;
         } Binder;
         struct {
            Int    offset;
            IRType ty;
         } Get;
         struct {
            IRArray* descr;
            struct _IRExpr* ix;
            Int bias;
         } GetI;
         struct {
            IRTemp tmp;
         } Tmp;
         struct {
            IROp op;
            struct _IRExpr* arg1;
            struct _IRExpr* arg2;
         } Binop;
         struct {
            IROp op;
            struct _IRExpr* arg;
         } Unop;
         struct {
            IREndness end;
            IRType ty;
            struct _IRExpr* addr;
         } Load;
         struct {
            IRConst* con;
         } Const;
         struct {
            IRCallee* cee;
            IRType    retty;
            struct _IRExpr** args;
         }  CCall;
         struct {
            struct _IRExpr* cond;
            struct _IRExpr* expr0;
            struct _IRExpr* exprX;
         } Mux0X;
      } Iex;
   }
   IRExpr;

extern IRExpr* IRExpr_Binder ( Int binder );
extern IRExpr* IRExpr_Get    ( Int off, IRType ty );
extern IRExpr* IRExpr_GetI   ( IRArray* descr, IRExpr* ix, Int bias );
extern IRExpr* IRExpr_Tmp    ( IRTemp tmp );
extern IRExpr* IRExpr_Binop  ( IROp op, IRExpr* arg1, IRExpr* arg2 );
extern IRExpr* IRExpr_Unop   ( IROp op, IRExpr* arg );
extern IRExpr* IRExpr_Load   ( IREndness end, IRType ty, IRExpr* addr );
extern IRExpr* IRExpr_Const  ( IRConst* con );
extern IRExpr* IRExpr_CCall  ( IRCallee* cee, IRType retty, IRExpr** args );
extern IRExpr* IRExpr_Mux0X  ( IRExpr* cond, IRExpr* expr0, IRExpr* exprX );

extern IRExpr* dopyIRExpr ( IRExpr* );

extern void ppIRExpr ( IRExpr* );

/* NULL-terminated IRExpr expression vectors, suitable for use as arg
   lists in clean/dirty helper calls. */

extern IRExpr** mkIRExprVec_0 ( void );
extern IRExpr** mkIRExprVec_1 ( IRExpr* );
extern IRExpr** mkIRExprVec_2 ( IRExpr*, IRExpr* );
extern IRExpr** mkIRExprVec_3 ( IRExpr*, IRExpr*, IRExpr* );
extern IRExpr** mkIRExprVec_4 ( IRExpr*, IRExpr*, IRExpr*, IRExpr* );
extern IRExpr** mkIRExprVec_5 ( IRExpr*, IRExpr*, 
                                IRExpr*, IRExpr*, IRExpr* );

extern IRExpr** sopyIRExprVec ( IRExpr** );
extern IRExpr** dopyIRExprVec ( IRExpr** );

/* Make a constant expression from the given host word taking into
   account of course the host word size. */
extern IRExpr* mkIRExpr_HWord ( HWord );

/* Convenience function for constructing clean helper calls. */
extern 
IRExpr* mkIRExprCCall ( IRType retty,
                        Int regparms, HChar* name, void* addr, 
                        IRExpr** args );


/* Convenience functions for atoms, that is, IRExprs which
   are either Iex_Tmp or Iex_Const. */
static inline Bool isIRAtom ( IRExpr* e ) {
   return toBool(e->tag == Iex_Tmp || e->tag == Iex_Const);
}

/* Are these two IR atoms identical?  Causes an assertion
   failure if they are passed non-atoms. */
extern Bool eqIRAtom ( IRExpr*, IRExpr* );


/* ------------------ Jump kinds ------------------ */

/* This describes hints which can be passed to the dispatcher at guest
   control-flow transfer points.

   Re Ijk_Invalidate: the guest state _must_ have two
   pseudo-registers, guest_TISTART and guest_TILEN, which specify the
   start and length of the region to be invalidated.  These are both
   the size of a guest word. It is the responsibility of the relevant
   toIR.c to ensure that these are filled in with suitable values
   before issuing a jump of kind Ijk_TInval.  
*/
typedef
   enum { 
      Ijk_Boring=0x14000, /* not interesting; just goto next */
      Ijk_Call,           /* guest is doing a call */
      Ijk_Ret,            /* guest is doing a return */
      Ijk_ClientReq,      /* do guest client req before continuing */
      Ijk_Syscall,        /* do guest syscall before continuing */
      Ijk_Yield,          /* client is yielding to thread scheduler */
      Ijk_EmWarn,         /* report emulation warning before continuing */
      Ijk_NoDecode,       /* next instruction cannot be decoded */
      Ijk_MapFail,        /* Vex-provided address translation failed */
      Ijk_TInval          /* Invalidate translations before continuing. */
   }
   IRJumpKind;

extern void ppIRJumpKind ( IRJumpKind );


/* ------------------ Dirty helper calls ------------------ */

/* A dirty call is a flexible mechanism for calling a helper function
   or procedure.  The helper function may read, write or modify client
   memory, and may read, write or modify client state.  It can take
   arguments and optionally return a value.  It may return different
   results and/or do different things when called repeated with the
   same arguments, by means of storing private state.

   If a value is returned, it is assigned to the nominated return
   temporary.

   Dirty calls are statements rather than expressions for obvious
   reasons.  If a dirty call is stated as writing guest state, any
   values derived from the written parts of the guest state are
   invalid.  Similarly, if the dirty call is stated as writing
   memory, any loaded values are invalidated by it.

   In order that instrumentation is possible, the call must state, and
   state correctly

   * whether it reads, writes or modifies memory, and if so where
     (only one chunk can be stated)

   * whether it reads, writes or modifies guest state, and if so which
     pieces (several pieces may be stated, and currently their extents
     must be known at translation-time).

   Normally, code is generated to pass just the args to the helper.
   However, if .needsBBP is set, then an extra first argument is
   passed, which is the baseblock pointer, so that the callee can
   access the guest state.  It is invalid for .nFxState to be zero
   but .needsBBP to be True, since .nFxState==0 is a claim that the
   call does not access guest state.

   IMPORTANT NOTE re GUARDS: Dirty calls are strict, very strict.  The
   arguments are evaluated REGARDLESS of the guard value.  It is
   unspecified the relative order of arg evaluation and guard
   evaluation.
*/

#define VEX_N_FXSTATE  7   /* enough for FXSAVE/FXRSTOR on x86 */

typedef
   enum {
      Ifx_None = 0x15000,   /* no effect */
      Ifx_Read,             /* reads the resource */
      Ifx_Write,            /* writes the resource */
      Ifx_Modify,           /* modifies the resource */
   }
   IREffect;

extern void ppIREffect ( IREffect );


typedef
   struct {
      /* What to call, and details of args/results */
      IRCallee* cee;    /* where to call */
      IRExpr*   guard;  /* :: Ity_Bit.  Controls whether call happens */
      IRExpr**  args;   /* arg list, ends in NULL */
      IRTemp    tmp;    /* to assign result to, or IRTemp_INVALID if none */

      /* Mem effects; we allow only one R/W/M region to be stated */
      IREffect  mFx;    /* indicates memory effects, if any */
      IRExpr*   mAddr;  /* of access, or NULL if mFx==Ifx_None */
      Int       mSize;  /* of access, or zero if mFx==Ifx_None */

      /* Guest state effects; up to N allowed */
      Bool needsBBP; /* True => also pass guest state ptr to callee */
      Int  nFxState; /* must be 0 .. VEX_N_FXSTATE */
      struct {
         IREffect fx;   /* read, write or modify?  Ifx_None is invalid. */
         Int      offset;
         Int      size;
      } fxState[VEX_N_FXSTATE];
   }
   IRDirty;

extern void     ppIRDirty ( IRDirty* );
extern IRDirty* emptyIRDirty ( void );

extern IRDirty* dopyIRDirty ( IRDirty* );

/* A handy function which takes some of the tedium out of constructing
   dirty helper calls.  The called function impliedly does not return
   any value and has a constant-True guard.  The call is marked as
   accessing neither guest state nor memory (hence the "unsafe"
   designation) -- you can mess with this later if need be.  A
   suitable IRCallee is constructed from the supplied bits. */
extern 
IRDirty* unsafeIRDirty_0_N ( Int regparms, HChar* name, void* addr, 
                             IRExpr** args );

/* Similarly, make a zero-annotation dirty call which returns a value,
   and assign that to the given temp. */
extern 
IRDirty* unsafeIRDirty_1_N ( IRTemp dst, 
                             Int regparms, HChar* name, void* addr, 
                             IRExpr** args );


/* ------------------ Statements ------------------ */

/* The possible kinds of statements are as follows.  Those marked
   OPTIONAL are hints of one kind or another, and as such do not
   denote any change in the guest state or of IR temporaries.  They
   can therefore be omitted without changing the meaning denoted by
   the IR. 

   At the moment, the only AbiHint is one which indicates that a given
   chunk of address space has become undefined.  This is used on
   amd64-linux to pass stack-redzoning hints to whoever wants to see
   them.
*/
typedef 
   enum {
      Ist_NoOp,    /* OPTIONAL: no-op (usually resulting from IR
                      optimisation) */
      Ist_IMark,   /* OPTIONAL: instruction mark: describe addr/len of
                      guest insn whose IR follows.  */
      Ist_AbiHint, /* OPTIONAL: tell me something about this
                      platform's ABI */
      Ist_Put,     /* write guest state, fixed offset */
      Ist_PutI,    /* write guest state, run-time offset */
      Ist_Tmp,     /* assign value to temporary */
      Ist_Store,   /* write to memory */
      Ist_Dirty,   /* call complex ("dirty") helper function */
      Ist_MFence,  /* memory fence */
      Ist_Exit     /* conditional exit from BB */
   } 
   IRStmtTag;

typedef
   struct _IRStmt {
      IRStmtTag tag;
      union {
         struct {
	 } NoOp;
         struct {
            Addr64 addr;
            Int    len;
         } IMark;
         struct {
            /* [base .. base+len-1] has become uninitialised */
            IRExpr* base;
            Int     len;
         } AbiHint;
         struct {
            Int     offset;
            IRExpr* data;
         } Put;
         struct {
            IRArray* descr;
            IRExpr*  ix;
            Int      bias;
            IRExpr*  data;
         } PutI;
         struct {
            IRTemp  tmp;
            IRExpr* data;
         } Tmp;
         struct {
            IREndness end;
            IRExpr*   addr;
            IRExpr*   data;
         } Store;
         struct {
            IRDirty* details;
         } Dirty;
         struct {
         } MFence;
         struct {
            IRExpr*    guard;
            IRJumpKind jk;
            IRConst*   dst;
         } Exit;
      } Ist;
   }
   IRStmt;

extern IRStmt* IRStmt_NoOp    ( void );
extern IRStmt* IRStmt_IMark   ( Addr64 addr, Int len );
extern IRStmt* IRStmt_AbiHint ( IRExpr* base, Int len );
extern IRStmt* IRStmt_Put     ( Int off, IRExpr* data );
extern IRStmt* IRStmt_PutI    ( IRArray* descr, IRExpr* ix, Int bias, 
                                IRExpr* data );
extern IRStmt* IRStmt_Tmp     ( IRTemp tmp, IRExpr* data );
extern IRStmt* IRStmt_Store   ( IREndness end, IRExpr* addr, IRExpr* data );
extern IRStmt* IRStmt_Dirty   ( IRDirty* details );
extern IRStmt* IRStmt_MFence  ( void );
extern IRStmt* IRStmt_Exit    ( IRExpr* guard, IRJumpKind jk, IRConst* dst );

extern IRStmt* dopyIRStmt ( IRStmt* );

extern void ppIRStmt ( IRStmt* );


/* ------------------ Basic Blocks ------------------ */

/* A bunch of statements, expressions, etc, are incomplete without an
   environment indicating the type of each IRTemp.  So this provides
   one.  IR temporaries are really just unsigned ints and so this
   provides an array, 0 .. n_types_used-1 of them.
*/
typedef
   struct {
      IRType* types;
      Int     types_size;
      Int     types_used;
   }
   IRTypeEnv;

extern IRTemp     newIRTemp     ( IRTypeEnv*, IRType );
extern IRTypeEnv* dopyIRTypeEnv ( IRTypeEnv* );

extern void ppIRTypeEnv ( IRTypeEnv* );


/* Basic blocks contain:
   - A table giving a type for each temp
   - An expandable array of statements
   - An expression of type 32 or 64 bits, depending on the
     guest's word size, indicating the next destination.
   - An indication of any special actions (JumpKind) needed
     for this final jump.
*/
typedef
   struct _IRBB {
      IRTypeEnv* tyenv;
      IRStmt**   stmts;
      Int        stmts_size;
      Int        stmts_used;
      IRExpr*    next;
      IRJumpKind jumpkind;
   }
   IRBB;

extern IRBB* emptyIRBB ( void );

extern IRBB* dopyIRBB ( IRBB* );

extern void ppIRBB ( IRBB* );

extern void addStmtToIRBB ( IRBB*, IRStmt* );


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* For messing with IR type environments */
extern IRTypeEnv* emptyIRTypeEnv  ( void );

/* What is the type of this expression? */
extern IRType typeOfIRConst ( IRConst* );
extern IRType typeOfIRTemp  ( IRTypeEnv*, IRTemp );
extern IRType typeOfIRExpr  ( IRTypeEnv*, IRExpr* );

/* Sanity check a BB of IR */
extern void sanityCheckIRBB ( IRBB*  bb, 
                              HChar* caller,
                              Bool   require_flatness, 
                              IRType guest_word_size );
extern Bool isFlatIRStmt ( IRStmt* );

/* Is this any value actually in the enumeration 'IRType' ? */
extern Bool isPlausibleIRType ( IRType ty );

#endif /* ndef __LIBVEX_IR_H */


/*---------------------------------------------------------------*/
/*---                                             libvex_ir.h ---*/
/*---------------------------------------------------------------*/
