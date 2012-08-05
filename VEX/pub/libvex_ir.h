
/*---------------------------------------------------------------*/
/*--- begin                                       libvex_ir.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2012 OpenWorks LLP
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

#ifndef __LIBVEX_IR_H
#define __LIBVEX_IR_H

#include "libvex_basictypes.h"

   
/*---------------------------------------------------------------*/
/*--- High-level IR description                               ---*/
/*---------------------------------------------------------------*/

/* Vex IR is an architecture-neutral intermediate representation.
   Unlike some IRs in systems similar to Vex, it is not like assembly
   language (ie. a list of instructions).  Rather, it is more like the
   IR that might be used in a compiler.

   Code blocks
   ~~~~~~~~~~~
   The code is broken into small code blocks ("superblocks", type:
   'IRSB').  Each code block typically represents from 1 to perhaps 50
   instructions.  IRSBs are single-entry, multiple-exit code blocks.
   Each IRSB contains three things:
   - a type environment, which indicates the type of each temporary
     value present in the IRSB
   - a list of statements, which represent code
   - a jump that exits from the end the IRSB
   Because the blocks are multiple-exit, there can be additional
   conditional exit statements that cause control to leave the IRSB
   before the final exit.  Also because of this, IRSBs can cover
   multiple non-consecutive sequences of code (up to 3).  These are
   recorded in the type VexGuestExtents (see libvex.h).

   Statements and expressions
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
   Statements (type 'IRStmt') represent operations with side-effects,
   eg.  guest register writes, stores, and assignments to temporaries.
   Expressions (type 'IRExpr') represent operations without
   side-effects, eg. arithmetic operations, loads, constants.
   Expressions can contain sub-expressions, forming expression trees,
   eg. (3 + (4 * load(addr1)).

   Storage of guest state
   ~~~~~~~~~~~~~~~~~~~~~~
   The "guest state" contains the guest registers of the guest machine
   (ie.  the machine that we are simulating).  It is stored by default
   in a block of memory supplied by the user of the VEX library,
   generally referred to as the guest state (area).  To operate on
   these registers, one must first read ("Get") them from the guest
   state into a temporary value.  Afterwards, one can write ("Put")
   them back into the guest state.

   Get and Put are characterised by a byte offset into the guest
   state, a small integer which effectively gives the identity of the
   referenced guest register, and a type, which indicates the size of
   the value to be transferred.

   The basic "Get" and "Put" operations are sufficient to model normal
   fixed registers on the guest.  Selected areas of the guest state
   can be treated as a circular array of registers (type:
   'IRRegArray'), which can be indexed at run-time.  This is done with
   the "GetI" and "PutI" primitives.  This is necessary to describe
   rotating register files, for example the x87 FPU stack, SPARC
   register windows, and the Itanium register files.

   Examples, and flattened vs. unflattened code
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   For example, consider this x86 instruction:
     
     addl %eax, %ebx

   One Vex IR translation for this code would be this:

     ------ IMark(0x24F275, 7, 0) ------
     t3 = GET:I32(0)             # get %eax, a 32-bit integer
     t2 = GET:I32(12)            # get %ebx, a 32-bit integer
     t1 = Add32(t3,t2)           # addl
     PUT(0) = t1                 # put %eax

   (For simplicity, this ignores the effects on the condition codes, and
   the update of the instruction pointer.)

   The "IMark" is an IR statement that doesn't represent actual code.
   Instead it indicates the address and length of the original
   instruction.  The numbers 0 and 12 are offsets into the guest state
   for %eax and %ebx.  The full list of offsets for an architecture
   <ARCH> can be found in the type VexGuest<ARCH>State in the file
   VEX/pub/libvex_guest_<ARCH>.h.

   The five statements in this example are:
   - the IMark
   - three assignments to temporaries
   - one register write (put)

   The six expressions in this example are:
   - two register reads (gets)
   - one arithmetic (add) operation
   - three temporaries (two nested within the Add32, one in the PUT)

   The above IR is "flattened", ie. all sub-expressions are "atoms",
   either constants or temporaries.  An equivalent, unflattened version
   would be:
   
     PUT(0) = Add32(GET:I32(0), GET:I32(12))

   IR is guaranteed to be flattened at instrumentation-time.  This makes
   instrumentation easier.  Equivalent flattened and unflattened IR
   typically results in the same generated code.

   Another example, this one showing loads and stores:

     addl %edx,4(%eax)

   This becomes (again ignoring condition code and instruction pointer
   updates):

     ------ IMark(0x4000ABA, 3, 0) ------
     t3 = Add32(GET:I32(0),0x4:I32)
     t2 = LDle:I32(t3)
     t1 = GET:I32(8)
     t0 = Add32(t2,t1)
     STle(t3) = t0

   The "le" in "LDle" and "STle" is short for "little-endian".

   No need for deallocations
   ~~~~~~~~~~~~~~~~~~~~~~~~~
   Although there are allocation functions for various data structures
   in this file, there are no deallocation functions.  This is because
   Vex uses a memory allocation scheme that automatically reclaims the
   memory used by allocated structures once translation is completed.
   This makes things easier for tools that instruments/transforms code
   blocks.

   SSAness and typing
   ~~~~~~~~~~~~~~~~~~
   The IR is fully typed.  For every IRSB (IR block) it is possible to
   say unambiguously whether or not it is correctly typed.
   Incorrectly typed IR has no meaning and the VEX will refuse to
   process it.  At various points during processing VEX typechecks the
   IR and aborts if any violations are found.  This seems overkill but
   makes it a great deal easier to build a reliable JIT.

   IR also has the SSA property.  SSA stands for Static Single
   Assignment, and what it means is that each IR temporary may be
   assigned to only once.  This idea became widely used in compiler
   construction in the mid to late 90s.  It makes many IR-level
   transformations/code improvements easier, simpler and faster.
   Whenever it typechecks an IR block, VEX also checks the SSA
   property holds, and will abort if not so.  So SSAness is
   mechanically and rigidly enforced.
*/

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

   - deepCopyIRFoo is a deep copy constructor for IRFoos. 
     It recursively traverses the entire argument tree and
     produces a complete new tree.  All types have a deep copy
     constructor.

   - shallowCopyIRFoo is the shallow copy constructor for IRFoos.
     It creates a new top-level copy of the supplied object,
     but does not copy any sub-objects.  Only some types have a
     shallow copy constructor.
*/

/* ------------------ Types ------------------ */

/* A type indicates the size of a value, and whether it's an integer, a
   float, or a vector (SIMD) value. */
typedef 
   enum { 
      Ity_INVALID=0x11000,
      Ity_I1, 
      Ity_I8, 
      Ity_I16, 
      Ity_I32, 
      Ity_I64,
      Ity_I128,  /* 128-bit scalar */
      Ity_F32,   /* IEEE 754 float */
      Ity_F64,   /* IEEE 754 double */
      Ity_D32,   /* 32-bit Decimal floating point */
      Ity_D64,   /* 64-bit Decimal floating point */
      Ity_D128,  /* 128-bit Decimal floating point */
      Ity_F128,  /* 128-bit floating point; implementation defined */
      Ity_V128,  /* 128-bit SIMD */
      Ity_V256   /* 256-bit SIMD */
   }
   IRType;

/* Pretty-print an IRType */
extern void ppIRType ( IRType );

/* Get the size (in bytes) of an IRType */ 
extern Int sizeofIRType ( IRType );


/* ------------------ Endianness ------------------ */

/* IREndness is used in load IRExprs and store IRStmts. */
typedef
   enum { 
      Iend_LE=0x12000, /* little endian */
      Iend_BE          /* big endian */
   }
   IREndness;


/* ------------------ Constants ------------------ */

/* IRConsts are used within 'Const' and 'Exit' IRExprs. */

/* The various kinds of constant. */
typedef
   enum { 
      Ico_U1=0x13000,
      Ico_U8, 
      Ico_U16, 
      Ico_U32, 
      Ico_U64,
      Ico_F32,   /* 32-bit IEEE754 floating */
      Ico_F32i,  /* 32-bit unsigned int to be interpreted literally
                    as a IEEE754 single value. */
      Ico_F64,   /* 64-bit IEEE754 floating */
      Ico_F64i,  /* 64-bit unsigned int to be interpreted literally
                    as a IEEE754 double value. */
      Ico_V128,  /* 128-bit restricted vector constant, with 1 bit
                    (repeated 8 times) for each of the 16 x 1-byte lanes */
      Ico_V256   /* 256-bit restricted vector constant, with 1 bit
                    (repeated 8 times) for each of the 32 x 1-byte lanes */
   }
   IRConstTag;

/* A constant.  Stored as a tagged union.  'tag' indicates what kind of
   constant this is.  'Ico' is the union that holds the fields.  If an
   IRConst 'c' has c.tag equal to Ico_U32, then it's a 32-bit constant,
   and its value can be accessed with 'c.Ico.U32'. */
typedef
   struct _IRConst {
      IRConstTag tag;
      union {
         Bool   U1;
         UChar  U8;
         UShort U16;
         UInt   U32;
         ULong  U64;
         Float  F32;
         UInt   F32i;
         Double F64;
         ULong  F64i;
         UShort V128;   /* 16-bit value; see Ico_V128 comment above */
         UInt   V256;   /* 32-bit value; see Ico_V256 comment above */
      } Ico;
   }
   IRConst;

/* IRConst constructors */
extern IRConst* IRConst_U1   ( Bool );
extern IRConst* IRConst_U8   ( UChar );
extern IRConst* IRConst_U16  ( UShort );
extern IRConst* IRConst_U32  ( UInt );
extern IRConst* IRConst_U64  ( ULong );
extern IRConst* IRConst_F32  ( Float );
extern IRConst* IRConst_F32i ( UInt );
extern IRConst* IRConst_F64  ( Double );
extern IRConst* IRConst_F64i ( ULong );
extern IRConst* IRConst_V128 ( UShort );
extern IRConst* IRConst_V256 ( UInt );

/* Deep-copy an IRConst */
extern IRConst* deepCopyIRConst ( IRConst* );

/* Pretty-print an IRConst */
extern void ppIRConst ( IRConst* );

/* Compare two IRConsts for equality */
extern Bool eqIRConst ( IRConst*, IRConst* );


/* ------------------ Call targets ------------------ */

/* Describes a helper function to call.  The name part is purely for
   pretty printing and not actually used.  regparms=n tells the back
   end that the callee has been declared
   "__attribute__((regparm(n)))", although indirectly using the
   VEX_REGPARM(n) macro.  On some targets (x86) the back end will need
   to construct a non-standard sequence to call a function declared
   like this.

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

/* Create an IRCallee. */
extern IRCallee* mkIRCallee ( Int regparms, HChar* name, void* addr );

/* Deep-copy an IRCallee. */
extern IRCallee* deepCopyIRCallee ( IRCallee* );

/* Pretty-print an IRCallee. */
extern void ppIRCallee ( IRCallee* );


/* ------------------ Guest state arrays ------------------ */

/* This describes a section of the guest state that we want to
   be able to index at run time, so as to be able to describe 
   indexed or rotating register files on the guest. */
typedef
   struct {
      Int    base;   /* guest state offset of start of indexed area */
      IRType elemTy; /* type of each element in the indexed area */
      Int    nElems; /* number of elements in the indexed area */
   }
   IRRegArray;

extern IRRegArray* mkIRRegArray ( Int, IRType, Int );

extern IRRegArray* deepCopyIRRegArray ( IRRegArray* );

extern void ppIRRegArray ( IRRegArray* );
extern Bool eqIRRegArray ( IRRegArray*, IRRegArray* );


/* ------------------ Temporaries ------------------ */

/* This represents a temporary, eg. t1.  The IR optimiser relies on the
   fact that IRTemps are 32-bit ints.  Do not change them to be ints of
   any other size. */
typedef UInt IRTemp;

/* Pretty-print an IRTemp. */
extern void ppIRTemp ( IRTemp );

#define IRTemp_INVALID ((IRTemp)0xFFFFFFFF)


/* --------------- Primops (arity 1,2,3 and 4) --------------- */

/* Primitive operations that are used in Unop, Binop, Triop and Qop
   IRExprs.  Once we take into account integer, floating point and SIMD
   operations of all the different sizes, there are quite a lot of them.
   Most instructions supported by the architectures that Vex supports
   (x86, PPC, etc) are represented.  Some more obscure ones (eg. cpuid)
   are not;  they are instead handled with dirty helpers that emulate
   their functionality.  Such obscure ones are thus not directly visible
   in the IR, but their effects on guest state (memory and registers) 
   are made visible via the annotations in IRDirty structures.
*/
typedef
   enum { 
      /* -- Do not change this ordering.  The IR generators rely on
            (eg) Iop_Add64 == IopAdd8 + 3. -- */

      Iop_INVALID=0x14000,
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

      /* Exactly like CmpEQ8/16/32/64, but carrying the additional
         hint that these compute the success/failure of a CAS
         operation, and hence are almost certainly applied to two
         copies of the same value, which in turn has implications for
         Memcheck's instrumentation. */
      Iop_CasCmpEQ8, Iop_CasCmpEQ16, Iop_CasCmpEQ32, Iop_CasCmpEQ64,
      Iop_CasCmpNE8, Iop_CasCmpNE16, Iop_CasCmpNE32, Iop_CasCmpNE64,

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
      Iop_CmpwNEZ32, Iop_CmpwNEZ64, /* all-0s -> all-Os; other -> all-1s */
      Iop_Left8, Iop_Left16, Iop_Left32, Iop_Left64, /*  \x -> x | -x */
      Iop_Max32U, /* unsigned max */

      /* PowerPC-style 3-way integer comparisons.  Without them it is
         difficult to simulate PPC efficiently.
         op(x,y) | x < y  = 0x8 else 
                 | x > y  = 0x4 else
                 | x == y = 0x2
      */
      Iop_CmpORD32U, Iop_CmpORD64U,
      Iop_CmpORD32S, Iop_CmpORD64S,

      /* Division */
      /* TODO: clarify semantics wrt rounding, negative values, whatever */
      Iop_DivU32,   // :: I32,I32 -> I32 (simple div, no mod)
      Iop_DivS32,   // ditto, signed
      Iop_DivU64,   // :: I64,I64 -> I64 (simple div, no mod)
      Iop_DivS64,   // ditto, signed
      Iop_DivU64E,  // :: I64,I64 -> I64 (dividend is 64-bit arg (hi) concat with 64 0's (low))
      Iop_DivS64E,  // ditto, signed
      Iop_DivU32E,  // :: I32,I32 -> I32 (dividend is 32-bit arg (hi) concat with 32 0's (low))
      Iop_DivS32E,  // ditto, signed

      Iop_DivModU64to32, // :: I64,I32 -> I64
                         // of which lo half is div and hi half is mod
      Iop_DivModS64to32, // ditto, signed

      Iop_DivModU128to64, // :: V128,I64 -> V128
                          // of which lo half is div and hi half is mod
      Iop_DivModS128to64, // ditto, signed

      Iop_DivModS64to64, // :: I64,I64 -> I128
                         // of which lo half is div and hi half is mod

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

      /* ------ Floating point.  We try to be IEEE754 compliant. ------ */

      /* --- Simple stuff as mandated by 754. --- */

      /* Binary operations, with rounding. */
      /* :: IRRoundingMode(I32) x F64 x F64 -> F64 */ 
      Iop_AddF64, Iop_SubF64, Iop_MulF64, Iop_DivF64,

      /* :: IRRoundingMode(I32) x F32 x F32 -> F32 */ 
      Iop_AddF32, Iop_SubF32, Iop_MulF32, Iop_DivF32,

      /* Variants of the above which produce a 64-bit result but which
         round their result to a IEEE float range first. */
      /* :: IRRoundingMode(I32) x F64 x F64 -> F64 */ 
      Iop_AddF64r32, Iop_SubF64r32, Iop_MulF64r32, Iop_DivF64r32, 

      /* Unary operations, without rounding. */
      /* :: F64 -> F64 */
      Iop_NegF64, Iop_AbsF64,

      /* :: F32 -> F32 */
      Iop_NegF32, Iop_AbsF32,

      /* Unary operations, with rounding. */
      /* :: IRRoundingMode(I32) x F64 -> F64 */
      Iop_SqrtF64, Iop_SqrtF64r32,

      /* :: IRRoundingMode(I32) x F32 -> F32 */
      Iop_SqrtF32,

      /* Comparison, yielding GT/LT/EQ/UN(ordered), as per the following:
            0x45 Unordered
            0x01 LT
            0x00 GT
            0x40 EQ
         This just happens to be the Intel encoding.  The values
         are recorded in the type IRCmpF64Result.
      */
      /* :: F64 x F64 -> IRCmpF64Result(I32) */
      Iop_CmpF64,
      Iop_CmpF32,
      Iop_CmpF128,

      /* --- Int to/from FP conversions. --- */

      /* For the most part, these take a first argument :: Ity_I32 (as
         IRRoundingMode) which is an indication of the rounding mode
         to use, as per the following encoding ("the standard
         encoding"):
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
         encodings, as encoded in the guest state, to the standard
         encodings, to pass to the primops.
         For reference only, the ARM VFP encoding is:
            00b  to nearest
            01b  to +infinity
            10b  to -infinity
            11b  to zero
         Again, this will have to be converted to the standard encoding
         to pass to primops.

         If one of these conversions gets an out-of-range condition,
         or a NaN, as an argument, the result is host-defined.  On x86
         the "integer indefinite" value 0x80..00 is produced.  On PPC
         it is either 0x80..00 or 0x7F..FF depending on the sign of
         the argument.

         On ARMvfp, when converting to a signed integer result, the
         overflow result is 0x80..00 for negative args and 0x7F..FF
         for positive args.  For unsigned integer results it is
         0x00..00 and 0xFF..FF respectively.

         Rounding is required whenever the destination type cannot
         represent exactly all values of the source type.
      */
      Iop_F64toI16S, /* IRRoundingMode(I32) x F64 -> signed I16 */
      Iop_F64toI32S, /* IRRoundingMode(I32) x F64 -> signed I32 */
      Iop_F64toI64S, /* IRRoundingMode(I32) x F64 -> signed I64 */
      Iop_F64toI64U, /* IRRoundingMode(I32) x F64 -> unsigned I64 */

      Iop_F64toI32U, /* IRRoundingMode(I32) x F64 -> unsigned I32 */

      Iop_I16StoF64, /*                       signed I16 -> F64 */
      Iop_I32StoF64, /*                       signed I32 -> F64 */
      Iop_I64StoF64, /* IRRoundingMode(I32) x signed I64 -> F64 */
      Iop_I64UtoF64, /* IRRoundingMode(I32) x unsigned I64 -> F64 */
      Iop_I64UtoF32, /* IRRoundingMode(I32) x unsigned I64 -> F32 */

      Iop_I32UtoF64, /*                       unsigned I32 -> F64 */

      Iop_F32toI16S, /* IRRoundingMode(I32) x F32 -> signed I16 */
      Iop_F32toI32S, /* IRRoundingMode(I32) x F32 -> signed I32 */
      Iop_F32toI64S, /* IRRoundingMode(I32) x F32 -> signed I64 */

      Iop_I16StoF32, /*                       signed I16 -> F32 */
      Iop_I32StoF32, /* IRRoundingMode(I32) x signed I32 -> F32 */
      Iop_I64StoF32, /* IRRoundingMode(I32) x signed I64 -> F32 */

      /* Conversion between floating point formats */
      Iop_F32toF64,  /*                       F32 -> F64 */
      Iop_F64toF32,  /* IRRoundingMode(I32) x F64 -> F32 */

      /* Reinterpretation.  Take an F64 and produce an I64 with 
         the same bit pattern, or vice versa. */
      Iop_ReinterpF64asI64, Iop_ReinterpI64asF64,
      Iop_ReinterpF32asI32, Iop_ReinterpI32asF32,

      /* Support for 128-bit floating point */
      Iop_F64HLtoF128,/* (high half of F128,low half of F128) -> F128 */
      Iop_F128HItoF64,/* F128 -> high half of F128 into a F64 register */
      Iop_F128LOtoF64,/* F128 -> low  half of F128 into a F64 register */

      /* :: IRRoundingMode(I32) x F128 x F128 -> F128 */
      Iop_AddF128, Iop_SubF128, Iop_MulF128, Iop_DivF128,

      /* :: F128 -> F128 */
      Iop_NegF128, Iop_AbsF128,

      /* :: IRRoundingMode(I32) x F128 -> F128 */
      Iop_SqrtF128,

      Iop_I32StoF128, /*                signed I32  -> F128 */
      Iop_I64StoF128, /*                signed I64  -> F128 */
      Iop_F32toF128,  /*                       F32  -> F128 */
      Iop_F64toF128,  /*                       F64  -> F128 */

      Iop_F128toI32S, /* IRRoundingMode(I32) x F128 -> signed I32  */
      Iop_F128toI64S, /* IRRoundingMode(I32) x F128 -> signed I64  */
      Iop_F128toF64,  /* IRRoundingMode(I32) x F128 -> F64         */
      Iop_F128toF32,  /* IRRoundingMode(I32) x F128 -> F32         */

      /* --- guest x86/amd64 specifics, not mandated by 754. --- */

      /* Binary ops, with rounding. */
      /* :: IRRoundingMode(I32) x F64 x F64 -> F64 */ 
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

      /* Unary ops, with rounding. */
      /* :: IRRoundingMode(I32) x F64 -> F64 */
      Iop_SinF64,    /* FSIN */
      Iop_CosF64,    /* FCOS */
      Iop_TanF64,    /* FTAN */
      Iop_2xm1F64,   /* (2^arg - 1.0) */
      Iop_RoundF64toInt, /* F64 value to nearest integral value (still
                            as F64) */
      Iop_RoundF32toInt, /* F32 value to nearest integral value (still
                            as F32) */

      /* --- guest s390 specifics, not mandated by 754. --- */

      /* Fused multiply-add/sub */
      /* :: IRRoundingMode(I32) x F32 x F32 x F32 -> F32
            (computes op3 * op2 +/- op1 */
      Iop_MAddF32, Iop_MSubF32,

      /* --- guest ppc32/64 specifics, not mandated by 754. --- */

      /* Ternary operations, with rounding. */
      /* Fused multiply-add/sub, with 112-bit intermediate
         precision for ppc.
         Also used to implement fused multiply-add/sub for s390. */
      /* :: IRRoundingMode(I32) x F64 x F64 x F64 -> F64 
            (computes arg2 * arg3 +/- arg4) */ 
      Iop_MAddF64, Iop_MSubF64,

      /* Variants of the above which produce a 64-bit result but which
         round their result to a IEEE float range first. */
      /* :: IRRoundingMode(I32) x F64 x F64 x F64 -> F64 */ 
      Iop_MAddF64r32, Iop_MSubF64r32,

      /* :: F64 -> F64 */
      Iop_Est5FRSqrt,    /* reciprocal square root estimate, 5 good bits */
      Iop_RoundF64toF64_NEAREST, /* frin */
      Iop_RoundF64toF64_NegINF,  /* frim */ 
      Iop_RoundF64toF64_PosINF,  /* frip */
      Iop_RoundF64toF64_ZERO,    /* friz */

      /* :: F64 -> F32 */
      Iop_TruncF64asF32, /* do F64->F32 truncation as per 'fsts' */

      /* :: IRRoundingMode(I32) x F64 -> F64 */
      Iop_RoundF64toF32, /* round F64 to nearest F32 value (still as F64) */
      /* NB: pretty much the same as Iop_F64toF32, except no change 
         of type. */

      /* :: F64 -> I32 */
      Iop_CalcFPRF, /* Calc 5 fpscr[FPRF] bits (Class, <, =, >, Unord)
                       from FP result */

      /* ------------------ 32-bit SIMD Integer ------------------ */

      /* 32x1 saturating add/sub (ok, well, not really SIMD :) */
      Iop_QAdd32S,
      Iop_QSub32S,

      /* 16x2 add/sub, also signed/unsigned saturating variants */
      Iop_Add16x2, Iop_Sub16x2,
      Iop_QAdd16Sx2, Iop_QAdd16Ux2,
      Iop_QSub16Sx2, Iop_QSub16Ux2,

      /* 16x2 signed/unsigned halving add/sub.  For each lane, these
         compute bits 16:1 of (eg) sx(argL) + sx(argR),
         or zx(argL) - zx(argR) etc. */
      Iop_HAdd16Ux2, Iop_HAdd16Sx2,
      Iop_HSub16Ux2, Iop_HSub16Sx2,

      /* 8x4 add/sub, also signed/unsigned saturating variants */
      Iop_Add8x4, Iop_Sub8x4,
      Iop_QAdd8Sx4, Iop_QAdd8Ux4,
      Iop_QSub8Sx4, Iop_QSub8Ux4,

      /* 8x4 signed/unsigned halving add/sub.  For each lane, these
         compute bits 8:1 of (eg) sx(argL) + sx(argR),
         or zx(argL) - zx(argR) etc. */
      Iop_HAdd8Ux4, Iop_HAdd8Sx4,
      Iop_HSub8Ux4, Iop_HSub8Sx4,

      /* 8x4 sum of absolute unsigned differences. */
      Iop_Sad8Ux4,

      /* MISC (vector integer cmp != 0) */
      Iop_CmpNEZ16x2, Iop_CmpNEZ8x4,

      /* ------------------ 64-bit SIMD FP ------------------------ */

      /* Convertion to/from int */
      Iop_I32UtoFx2,  Iop_I32StoFx2,    /* I32x4 -> F32x4 */
      Iop_FtoI32Ux2_RZ,  Iop_FtoI32Sx2_RZ,    /* F32x4 -> I32x4 */
      /* Fixed32 format is floating-point number with fixed number of fraction
         bits. The number of fraction bits is passed as a second argument of
         type I8. */
      Iop_F32ToFixed32Ux2_RZ, Iop_F32ToFixed32Sx2_RZ, /* fp -> fixed-point */
      Iop_Fixed32UToF32x2_RN, Iop_Fixed32SToF32x2_RN, /* fixed-point -> fp */

      /* Binary operations */
      Iop_Max32Fx2,      Iop_Min32Fx2,
      /* Pairwise Min and Max. See integer pairwise operations for more
         details. */
      Iop_PwMax32Fx2,    Iop_PwMin32Fx2,
      /* Note: For the following compares, the arm front-end assumes a
         nan in a lane of either argument returns zero for that lane. */
      Iop_CmpEQ32Fx2, Iop_CmpGT32Fx2, Iop_CmpGE32Fx2,

      /* Vector Reciprocal Estimate finds an approximate reciprocal of each
      element in the operand vector, and places the results in the destination
      vector.  */
      Iop_Recip32Fx2,

      /* Vector Reciprocal Step computes (2.0 - arg1 * arg2).
         Note, that if one of the arguments is zero and another one is infinity
         of arbitrary sign the result of the operation is 2.0. */
      Iop_Recps32Fx2,

      /* Vector Reciprocal Square Root Estimate finds an approximate reciprocal
         square root of each element in the operand vector. */
      Iop_Rsqrte32Fx2,

      /* Vector Reciprocal Square Root Step computes (3.0 - arg1 * arg2) / 2.0.
         Note, that of one of the arguments is zero and another one is infiinty
         of arbitrary sign the result of the operation is 1.5. */
      Iop_Rsqrts32Fx2,

      /* Unary */
      Iop_Neg32Fx2, Iop_Abs32Fx2,

      /* ------------------ 64-bit SIMD Integer. ------------------ */

      /* MISC (vector integer cmp != 0) */
      Iop_CmpNEZ8x8, Iop_CmpNEZ16x4, Iop_CmpNEZ32x2,

      /* ADDITION (normal / unsigned sat / signed sat) */
      Iop_Add8x8,   Iop_Add16x4,   Iop_Add32x2,
      Iop_QAdd8Ux8, Iop_QAdd16Ux4, Iop_QAdd32Ux2, Iop_QAdd64Ux1,
      Iop_QAdd8Sx8, Iop_QAdd16Sx4, Iop_QAdd32Sx2, Iop_QAdd64Sx1,

      /* PAIRWISE operations */
      /* Iop_PwFoo16x4( [a,b,c,d], [e,f,g,h] ) =
            [Foo16(a,b), Foo16(c,d), Foo16(e,f), Foo16(g,h)] */
      Iop_PwAdd8x8,  Iop_PwAdd16x4,  Iop_PwAdd32x2,
      Iop_PwMax8Sx8, Iop_PwMax16Sx4, Iop_PwMax32Sx2,
      Iop_PwMax8Ux8, Iop_PwMax16Ux4, Iop_PwMax32Ux2,
      Iop_PwMin8Sx8, Iop_PwMin16Sx4, Iop_PwMin32Sx2,
      Iop_PwMin8Ux8, Iop_PwMin16Ux4, Iop_PwMin32Ux2,
      /* Longening variant is unary. The resulting vector contains two times
         less elements than operand, but they are two times wider.
         Example:
            Iop_PAddL16Ux4( [a,b,c,d] ) = [a+b,c+d]
               where a+b and c+d are unsigned 32-bit values. */
      Iop_PwAddL8Ux8, Iop_PwAddL16Ux4, Iop_PwAddL32Ux2,
      Iop_PwAddL8Sx8, Iop_PwAddL16Sx4, Iop_PwAddL32Sx2,

      /* SUBTRACTION (normal / unsigned sat / signed sat) */
      Iop_Sub8x8,   Iop_Sub16x4,   Iop_Sub32x2,
      Iop_QSub8Ux8, Iop_QSub16Ux4, Iop_QSub32Ux2, Iop_QSub64Ux1,
      Iop_QSub8Sx8, Iop_QSub16Sx4, Iop_QSub32Sx2, Iop_QSub64Sx1,

      /* ABSOLUTE VALUE */
      Iop_Abs8x8, Iop_Abs16x4, Iop_Abs32x2,

      /* MULTIPLICATION (normal / high half of signed/unsigned / plynomial ) */
      Iop_Mul8x8, Iop_Mul16x4, Iop_Mul32x2,
      Iop_Mul32Fx2,
      Iop_MulHi16Ux4,
      Iop_MulHi16Sx4,
      /* Plynomial multiplication treats it's arguments as coefficients of
         polynoms over {0, 1}. */
      Iop_PolynomialMul8x8,

      /* Vector Saturating Doubling Multiply Returning High Half and
         Vector Saturating Rounding Doubling Multiply Returning High Half */
      /* These IROp's multiply corresponding elements in two vectors, double
         the results, and place the most significant half of the final results
         in the destination vector. The results are truncated or rounded. If
         any of the results overflow, they are saturated. */
      Iop_QDMulHi16Sx4, Iop_QDMulHi32Sx2,
      Iop_QRDMulHi16Sx4, Iop_QRDMulHi32Sx2,

      /* AVERAGING: note: (arg1 + arg2 + 1) >>u 1 */
      Iop_Avg8Ux8,
      Iop_Avg16Ux4,

      /* MIN/MAX */
      Iop_Max8Sx8, Iop_Max16Sx4, Iop_Max32Sx2,
      Iop_Max8Ux8, Iop_Max16Ux4, Iop_Max32Ux2,
      Iop_Min8Sx8, Iop_Min16Sx4, Iop_Min32Sx2,
      Iop_Min8Ux8, Iop_Min16Ux4, Iop_Min32Ux2,

      /* COMPARISON */
      Iop_CmpEQ8x8,  Iop_CmpEQ16x4,  Iop_CmpEQ32x2,
      Iop_CmpGT8Ux8, Iop_CmpGT16Ux4, Iop_CmpGT32Ux2,
      Iop_CmpGT8Sx8, Iop_CmpGT16Sx4, Iop_CmpGT32Sx2,

      /* COUNT ones / leading zeroes / leading sign bits (not including topmost
         bit) */
      Iop_Cnt8x8,
      Iop_Clz8Sx8, Iop_Clz16Sx4, Iop_Clz32Sx2,
      Iop_Cls8Sx8, Iop_Cls16Sx4, Iop_Cls32Sx2,

      /* VECTOR x VECTOR SHIFT / ROTATE */
      Iop_Shl8x8, Iop_Shl16x4, Iop_Shl32x2,
      Iop_Shr8x8, Iop_Shr16x4, Iop_Shr32x2,
      Iop_Sar8x8, Iop_Sar16x4, Iop_Sar32x2,
      Iop_Sal8x8, Iop_Sal16x4, Iop_Sal32x2, Iop_Sal64x1,

      /* VECTOR x SCALAR SHIFT (shift amt :: Ity_I8) */
      Iop_ShlN8x8, Iop_ShlN16x4, Iop_ShlN32x2,
      Iop_ShrN8x8, Iop_ShrN16x4, Iop_ShrN32x2,
      Iop_SarN8x8, Iop_SarN16x4, Iop_SarN32x2,

      /* VECTOR x VECTOR SATURATING SHIFT */
      Iop_QShl8x8, Iop_QShl16x4, Iop_QShl32x2, Iop_QShl64x1,
      Iop_QSal8x8, Iop_QSal16x4, Iop_QSal32x2, Iop_QSal64x1,
      /* VECTOR x INTEGER SATURATING SHIFT */
      Iop_QShlN8Sx8, Iop_QShlN16Sx4, Iop_QShlN32Sx2, Iop_QShlN64Sx1,
      Iop_QShlN8x8, Iop_QShlN16x4, Iop_QShlN32x2, Iop_QShlN64x1,
      Iop_QSalN8x8, Iop_QSalN16x4, Iop_QSalN32x2, Iop_QSalN64x1,

      /* NARROWING (binary) 
         -- narrow 2xI64 into 1xI64, hi half from left arg */
      /* For saturated narrowing, I believe there are 4 variants of
         the basic arithmetic operation, depending on the signedness
         of argument and result.  Here are examples that exemplify
         what I mean:

         QNarrow16Uto8U ( UShort x )  if (x >u 255) x = 255;
                                      return x[7:0];

         QNarrow16Sto8S ( Short x )   if (x <s -128) x = -128;
                                      if (x >s  127) x = 127;
                                      return x[7:0];

         QNarrow16Uto8S ( UShort x )  if (x >u 127) x = 127;
                                      return x[7:0];

         QNarrow16Sto8U ( Short x )   if (x <s 0)   x = 0;
                                      if (x >s 255) x = 255;
                                      return x[7:0];
      */
      Iop_QNarrowBin16Sto8Ux8,
      Iop_QNarrowBin16Sto8Sx8, Iop_QNarrowBin32Sto16Sx4,
      Iop_NarrowBin16to8x8,    Iop_NarrowBin32to16x4,

      /* INTERLEAVING */
      /* Interleave lanes from low or high halves of
         operands.  Most-significant result lane is from the left
         arg. */
      Iop_InterleaveHI8x8, Iop_InterleaveHI16x4, Iop_InterleaveHI32x2,
      Iop_InterleaveLO8x8, Iop_InterleaveLO16x4, Iop_InterleaveLO32x2,
      /* Interleave odd/even lanes of operands.  Most-significant result lane
         is from the left arg.  Note that Interleave{Odd,Even}Lanes32x2 are
         identical to Interleave{HI,LO}32x2 and so are omitted.*/
      Iop_InterleaveOddLanes8x8, Iop_InterleaveEvenLanes8x8,
      Iop_InterleaveOddLanes16x4, Iop_InterleaveEvenLanes16x4,


      /* CONCATENATION -- build a new value by concatenating either
         the even or odd lanes of both operands.  Note that
         Cat{Odd,Even}Lanes32x2 are identical to Interleave{HI,LO}32x2
         and so are omitted. */
      Iop_CatOddLanes8x8, Iop_CatOddLanes16x4,
      Iop_CatEvenLanes8x8, Iop_CatEvenLanes16x4,

      /* GET / SET elements of VECTOR
         GET is binop (I64, I8) -> I<elem_size>
         SET is triop (I64, I8, I<elem_size>) -> I64 */
      /* Note: the arm back-end handles only constant second argument */
      Iop_GetElem8x8, Iop_GetElem16x4, Iop_GetElem32x2,
      Iop_SetElem8x8, Iop_SetElem16x4, Iop_SetElem32x2,

      /* DUPLICATING -- copy value to all lanes */
      Iop_Dup8x8,   Iop_Dup16x4,   Iop_Dup32x2,

      /* EXTRACT -- copy 8-arg3 highest bytes from arg1 to 8-arg3 lowest bytes
         of result and arg3 lowest bytes of arg2 to arg3 highest bytes of
         result.
         It is a triop: (I64, I64, I8) -> I64 */
      /* Note: the arm back-end handles only constant third argumnet. */
      Iop_Extract64,

      /* REVERSE the order of elements in each Half-words, Words,
         Double-words */
      /* Examples:
            Reverse16_8x8([a,b,c,d,e,f,g,h]) = [b,a,d,c,f,e,h,g]
            Reverse32_8x8([a,b,c,d,e,f,g,h]) = [d,c,b,a,h,g,f,e]
            Reverse64_8x8([a,b,c,d,e,f,g,h]) = [h,g,f,e,d,c,b,a] */
      Iop_Reverse16_8x8,
      Iop_Reverse32_8x8, Iop_Reverse32_16x4,
      Iop_Reverse64_8x8, Iop_Reverse64_16x4, Iop_Reverse64_32x2,

      /* PERMUTING -- copy src bytes to dst,
         as indexed by control vector bytes:
            for i in 0 .. 7 . result[i] = argL[ argR[i] ] 
         argR[i] values may only be in the range 0 .. 7, else behaviour
         is undefined. */
      Iop_Perm8x8,

      /* Vector Reciprocal Estimate and Vector Reciprocal Square Root Estimate
         See floating-point equiwalents for details. */
      Iop_Recip32x2, Iop_Rsqrte32x2,

      /* ------------------ Decimal Floating Point ------------------ */

      /* ARITHMETIC INSTRUCTIONS   64-bit
	 ----------------------------------
	 IRRoundingModeDFP(I32) X D64 X D64 -> D64
      */
      Iop_AddD64, Iop_SubD64, Iop_MulD64, Iop_DivD64,

      /* ARITHMETIC INSTRUCTIONS  128-bit
	 ----------------------------------
	 IRRoundingModeDFP(I32) X D128 X D128 -> D128
      */
      Iop_AddD128, Iop_SubD128, Iop_MulD128, Iop_DivD128,

      /* SHIFT SIGNIFICAND INSTRUCTIONS
       *    The DFP significand is shifted by the number of digits specified
       *    by the U8 operand.  Digits shifted out of the leftmost digit are
       *    lost. Zeros are supplied to the vacated positions on the right.
       *    The sign of the result is the same as the sign of the original
       *    operand.
       *
       * D64 x U8  -> D64    left shift and right shift respectively */
      Iop_ShlD64, Iop_ShrD64,

      /* D128 x U8  -> D128  left shift and right shift respectively */
      Iop_ShlD128, Iop_ShrD128,


      /* FORMAT CONVERSION INSTRUCTIONS
       *   D32 -> D64
       */
      Iop_D32toD64,

      /*   D64 -> D128 */
      Iop_D64toD128, 

      /*   I64S -> D128 */
      Iop_I64StoD128, 

      /*   IRRoundingModeDFP(I32) x D64 -> D32 */
      Iop_D64toD32,

      /*   IRRoundingModeDFP(I32) x D128 -> D64 */
      Iop_D128toD64,

      /*   IRRoundingModeDFP(I32) x I64 -> D64 */
      Iop_I64StoD64,

      /*   IRRoundingModeDFP(I32) x D64 -> I64 */
      Iop_D64toI64S,

      /*   IRRoundingModeDFP(I32) x D128 -> I64 */
      Iop_D128toI64S,

      /* ROUNDING INSTRUCTIONS
       * IRRoundingMode(I32) x D64 -> D64
       * The D64 operand, if a finite number, is rounded to an integer value.
       */
      Iop_RoundD64toInt,

      /* IRRoundingMode(I32) x D128 -> D128 */
      Iop_RoundD128toInt,

      /* COMPARE INSTRUCTIONS
       * D64 x D64 -> IRCmpD64Result(I32) */
      Iop_CmpD64,

      /* D128 x D128 -> IRCmpD64Result(I32) */
      Iop_CmpD128,

      /* QUANTIZE AND ROUND INSTRUCTIONS
       * The source operand is converted and rounded to the form with the 
       * immediate exponent specified by the rounding and exponent parameter.
       *
       * The second operand is converted and rounded to the form
       * of the first operand's exponent and the rounded based on the specified
       * rounding mode parameter.
       *
       * IRRoundingModeDFP(I32) x D64 x D64-> D64 */
      Iop_QuantizeD64,

      /* IRRoundingModeDFP(I32) x D128 x D128 -> D128 */
      Iop_QuantizeD128,

      /* IRRoundingModeDFP(I32) x I8 x D64 -> D64
       *    The Decimal Floating point operand is rounded to the requested 
       *    significance given by the I8 operand as specified by the rounding 
       *    mode.
       */
      Iop_SignificanceRoundD64,

      /* IRRoundingModeDFP(I32) x I8 x D128 -> D128 */
      Iop_SignificanceRoundD128,

      /* EXTRACT AND INSERT INSTRUCTIONS
       * D64 -> I64
       *    The exponent of the D32 or D64 operand is extracted.  The 
       *    extracted exponent is converted to a 64-bit signed binary integer.
       */
      Iop_ExtractExpD64,

      /* D128 -> I64 */
      Iop_ExtractExpD128,

      /* I64 x I64  -> D64 
       *    The exponent is specified by the first I64 operand the signed
       *    significand is given by the second I64 value.  The result is a D64
       *    value consisting of the specified significand and exponent whose 
       *    sign is that of the specified significand.
       */
      Iop_InsertExpD64,

      /* I64 x I128 -> D128 */
      Iop_InsertExpD128,

      /* Support for 128-bit DFP type */
      Iop_D64HLtoD128, Iop_D128HItoD64, Iop_D128LOtoD64,

      /*  I64 -> I64  
       *     Convert 50-bit densely packed BCD string to 60 bit BCD string
       */
      Iop_DPBtoBCD,

      /* I64 -> I64
       *     Convert 60 bit BCD string to 50-bit densely packed BCD string
       */
      Iop_BCDtoDPB,

      /* Conversion I64 -> D64 */
      Iop_ReinterpI64asD64,

      /* Conversion D64 -> I64 */
      Iop_ReinterpD64asI64,

      /* ------------------ 128-bit SIMD FP. ------------------ */

      /* --- 32x4 vector FP --- */

      /* binary */
      Iop_Add32Fx4, Iop_Sub32Fx4, Iop_Mul32Fx4, Iop_Div32Fx4, 
      Iop_Max32Fx4, Iop_Min32Fx4,
      Iop_Add32Fx2, Iop_Sub32Fx2,
      /* Note: For the following compares, the ppc and arm front-ends assume a
         nan in a lane of either argument returns zero for that lane. */
      Iop_CmpEQ32Fx4, Iop_CmpLT32Fx4, Iop_CmpLE32Fx4, Iop_CmpUN32Fx4,
      Iop_CmpGT32Fx4, Iop_CmpGE32Fx4,

      /* Vector Absolute */
      Iop_Abs32Fx4,

      /* Pairwise Max and Min. See integer pairwise operations for details. */
      Iop_PwMax32Fx4, Iop_PwMin32Fx4,

      /* unary */
      Iop_Sqrt32Fx4, Iop_RSqrt32Fx4,
      Iop_Neg32Fx4,

      /* Vector Reciprocal Estimate finds an approximate reciprocal of each
      element in the operand vector, and places the results in the destination
      vector.  */
      Iop_Recip32Fx4,

      /* Vector Reciprocal Step computes (2.0 - arg1 * arg2).
         Note, that if one of the arguments is zero and another one is infinity
         of arbitrary sign the result of the operation is 2.0. */
      Iop_Recps32Fx4,

      /* Vector Reciprocal Square Root Estimate finds an approximate reciprocal
         square root of each element in the operand vector. */
      Iop_Rsqrte32Fx4,

      /* Vector Reciprocal Square Root Step computes (3.0 - arg1 * arg2) / 2.0.
         Note, that of one of the arguments is zero and another one is infiinty
         of arbitrary sign the result of the operation is 1.5. */
      Iop_Rsqrts32Fx4,


      /* --- Int to/from FP conversion --- */
      /* Unlike the standard fp conversions, these irops take no
         rounding mode argument. Instead the irop trailers _R{M,P,N,Z}
         indicate the mode: {-inf, +inf, nearest, zero} respectively. */
      Iop_I32UtoFx4,  Iop_I32StoFx4,       /* I32x4 -> F32x4       */
      Iop_FtoI32Ux4_RZ,  Iop_FtoI32Sx4_RZ,    /* F32x4 -> I32x4       */
      Iop_QFtoI32Ux4_RZ, Iop_QFtoI32Sx4_RZ,   /* F32x4 -> I32x4 (with saturation) */
      Iop_RoundF32x4_RM, Iop_RoundF32x4_RP,   /* round to fp integer  */
      Iop_RoundF32x4_RN, Iop_RoundF32x4_RZ,   /* round to fp integer  */
      /* Fixed32 format is floating-point number with fixed number of fraction
         bits. The number of fraction bits is passed as a second argument of
         type I8. */
      Iop_F32ToFixed32Ux4_RZ, Iop_F32ToFixed32Sx4_RZ, /* fp -> fixed-point */
      Iop_Fixed32UToF32x4_RN, Iop_Fixed32SToF32x4_RN, /* fixed-point -> fp */

      /* --- Single to/from half conversion --- */
      /* FIXME: what kind of rounding in F32x4 -> F16x4 case? */
      Iop_F32toF16x4, Iop_F16toF32x4,         /* F32x4 <-> F16x4      */

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

      /* VECTOR SHIFT (shift amt :: Ity_I8) */
      Iop_ShlV128, Iop_ShrV128,

      /* MISC (vector integer cmp != 0) */
      Iop_CmpNEZ8x16, Iop_CmpNEZ16x8, Iop_CmpNEZ32x4, Iop_CmpNEZ64x2,

      /* ADDITION (normal / unsigned sat / signed sat) */
      Iop_Add8x16,   Iop_Add16x8,   Iop_Add32x4,   Iop_Add64x2,
      Iop_QAdd8Ux16, Iop_QAdd16Ux8, Iop_QAdd32Ux4, Iop_QAdd64Ux2,
      Iop_QAdd8Sx16, Iop_QAdd16Sx8, Iop_QAdd32Sx4, Iop_QAdd64Sx2,

      /* SUBTRACTION (normal / unsigned sat / signed sat) */
      Iop_Sub8x16,   Iop_Sub16x8,   Iop_Sub32x4,   Iop_Sub64x2,
      Iop_QSub8Ux16, Iop_QSub16Ux8, Iop_QSub32Ux4, Iop_QSub64Ux2,
      Iop_QSub8Sx16, Iop_QSub16Sx8, Iop_QSub32Sx4, Iop_QSub64Sx2,

      /* MULTIPLICATION (normal / high half of signed/unsigned) */
      Iop_Mul8x16,  Iop_Mul16x8,    Iop_Mul32x4,
                    Iop_MulHi16Ux8, Iop_MulHi32Ux4,
                    Iop_MulHi16Sx8, Iop_MulHi32Sx4,
      /* (widening signed/unsigned of even lanes, with lowest lane=zero) */
      Iop_MullEven8Ux16, Iop_MullEven16Ux8,
      Iop_MullEven8Sx16, Iop_MullEven16Sx8,
      /* FIXME: document these */
      Iop_Mull8Ux8, Iop_Mull8Sx8,
      Iop_Mull16Ux4, Iop_Mull16Sx4,
      Iop_Mull32Ux2, Iop_Mull32Sx2,
      /* Vector Saturating Doubling Multiply Returning High Half and
         Vector Saturating Rounding Doubling Multiply Returning High Half */
      /* These IROp's multiply corresponding elements in two vectors, double
         the results, and place the most significant half of the final results
         in the destination vector. The results are truncated or rounded. If
         any of the results overflow, they are saturated. */
      Iop_QDMulHi16Sx8, Iop_QDMulHi32Sx4,
      Iop_QRDMulHi16Sx8, Iop_QRDMulHi32Sx4,
      /* Doubling saturating multiplication (long) (I64, I64) -> V128 */
      Iop_QDMulLong16Sx4, Iop_QDMulLong32Sx2,
      /* Plynomial multiplication treats it's arguments as coefficients of
         polynoms over {0, 1}. */
      Iop_PolynomialMul8x16, /* (V128, V128) -> V128 */
      Iop_PolynomialMull8x8, /*   (I64, I64) -> V128 */

      /* PAIRWISE operations */
      /* Iop_PwFoo16x4( [a,b,c,d], [e,f,g,h] ) =
            [Foo16(a,b), Foo16(c,d), Foo16(e,f), Foo16(g,h)] */
      Iop_PwAdd8x16, Iop_PwAdd16x8, Iop_PwAdd32x4,
      Iop_PwAdd32Fx2,
      /* Longening variant is unary. The resulting vector contains two times
         less elements than operand, but they are two times wider.
         Example:
            Iop_PwAddL16Ux4( [a,b,c,d] ) = [a+b,c+d]
               where a+b and c+d are unsigned 32-bit values. */
      Iop_PwAddL8Ux16, Iop_PwAddL16Ux8, Iop_PwAddL32Ux4,
      Iop_PwAddL8Sx16, Iop_PwAddL16Sx8, Iop_PwAddL32Sx4,

      /* ABSOLUTE VALUE */
      Iop_Abs8x16, Iop_Abs16x8, Iop_Abs32x4,

      /* AVERAGING: note: (arg1 + arg2 + 1) >>u 1 */
      Iop_Avg8Ux16, Iop_Avg16Ux8, Iop_Avg32Ux4,
      Iop_Avg8Sx16, Iop_Avg16Sx8, Iop_Avg32Sx4,

      /* MIN/MAX */
      Iop_Max8Sx16, Iop_Max16Sx8, Iop_Max32Sx4,
      Iop_Max8Ux16, Iop_Max16Ux8, Iop_Max32Ux4,
      Iop_Min8Sx16, Iop_Min16Sx8, Iop_Min32Sx4,
      Iop_Min8Ux16, Iop_Min16Ux8, Iop_Min32Ux4,

      /* COMPARISON */
      Iop_CmpEQ8x16,  Iop_CmpEQ16x8,  Iop_CmpEQ32x4,  Iop_CmpEQ64x2,
      Iop_CmpGT8Sx16, Iop_CmpGT16Sx8, Iop_CmpGT32Sx4, Iop_CmpGT64Sx2,
      Iop_CmpGT8Ux16, Iop_CmpGT16Ux8, Iop_CmpGT32Ux4,

      /* COUNT ones / leading zeroes / leading sign bits (not including topmost
         bit) */
      Iop_Cnt8x16,
      Iop_Clz8Sx16, Iop_Clz16Sx8, Iop_Clz32Sx4,
      Iop_Cls8Sx16, Iop_Cls16Sx8, Iop_Cls32Sx4,

      /* VECTOR x SCALAR SHIFT (shift amt :: Ity_I8) */
      Iop_ShlN8x16, Iop_ShlN16x8, Iop_ShlN32x4, Iop_ShlN64x2,
      Iop_ShrN8x16, Iop_ShrN16x8, Iop_ShrN32x4, Iop_ShrN64x2,
      Iop_SarN8x16, Iop_SarN16x8, Iop_SarN32x4, Iop_SarN64x2,

      /* VECTOR x VECTOR SHIFT / ROTATE */
      Iop_Shl8x16, Iop_Shl16x8, Iop_Shl32x4, Iop_Shl64x2,
      Iop_Shr8x16, Iop_Shr16x8, Iop_Shr32x4, Iop_Shr64x2,
      Iop_Sar8x16, Iop_Sar16x8, Iop_Sar32x4, Iop_Sar64x2,
      Iop_Sal8x16, Iop_Sal16x8, Iop_Sal32x4, Iop_Sal64x2,
      Iop_Rol8x16, Iop_Rol16x8, Iop_Rol32x4,

      /* VECTOR x VECTOR SATURATING SHIFT */
      Iop_QShl8x16, Iop_QShl16x8, Iop_QShl32x4, Iop_QShl64x2,
      Iop_QSal8x16, Iop_QSal16x8, Iop_QSal32x4, Iop_QSal64x2,
      /* VECTOR x INTEGER SATURATING SHIFT */
      Iop_QShlN8Sx16, Iop_QShlN16Sx8, Iop_QShlN32Sx4, Iop_QShlN64Sx2,
      Iop_QShlN8x16, Iop_QShlN16x8, Iop_QShlN32x4, Iop_QShlN64x2,
      Iop_QSalN8x16, Iop_QSalN16x8, Iop_QSalN32x4, Iop_QSalN64x2,

      /* NARROWING (binary) 
         -- narrow 2xV128 into 1xV128, hi half from left arg */
      /* See comments above w.r.t. U vs S issues in saturated narrowing. */
      Iop_QNarrowBin16Sto8Ux16, Iop_QNarrowBin32Sto16Ux8,
      Iop_QNarrowBin16Sto8Sx16, Iop_QNarrowBin32Sto16Sx8,
      Iop_QNarrowBin16Uto8Ux16, Iop_QNarrowBin32Uto16Ux8,
      Iop_NarrowBin16to8x16, Iop_NarrowBin32to16x8,

      /* NARROWING (unary) -- narrow V128 into I64 */
      Iop_NarrowUn16to8x8, Iop_NarrowUn32to16x4, Iop_NarrowUn64to32x2,
      /* Saturating narrowing from signed source to signed/unsigned destination */
      Iop_QNarrowUn16Sto8Sx8, Iop_QNarrowUn32Sto16Sx4, Iop_QNarrowUn64Sto32Sx2,
      Iop_QNarrowUn16Sto8Ux8, Iop_QNarrowUn32Sto16Ux4, Iop_QNarrowUn64Sto32Ux2,
      /* Saturating narrowing from unsigned source to unsigned destination */
      Iop_QNarrowUn16Uto8Ux8, Iop_QNarrowUn32Uto16Ux4, Iop_QNarrowUn64Uto32Ux2,

      /* WIDENING -- sign or zero extend each element of the argument
         vector to the twice original size.  The resulting vector consists of
         the same number of elements but each element and the vector itself
         are twice as wide.
         All operations are I64->V128.
         Example
            Iop_Widen32Sto64x2( [a, b] ) = [c, d]
               where c = Iop_32Sto64(a) and d = Iop_32Sto64(b) */
      Iop_Widen8Uto16x8, Iop_Widen16Uto32x4, Iop_Widen32Uto64x2,
      Iop_Widen8Sto16x8, Iop_Widen16Sto32x4, Iop_Widen32Sto64x2,

      /* INTERLEAVING */
      /* Interleave lanes from low or high halves of
         operands.  Most-significant result lane is from the left
         arg. */
      Iop_InterleaveHI8x16, Iop_InterleaveHI16x8,
      Iop_InterleaveHI32x4, Iop_InterleaveHI64x2,
      Iop_InterleaveLO8x16, Iop_InterleaveLO16x8,
      Iop_InterleaveLO32x4, Iop_InterleaveLO64x2,
      /* Interleave odd/even lanes of operands.  Most-significant result lane
         is from the left arg. */
      Iop_InterleaveOddLanes8x16, Iop_InterleaveEvenLanes8x16,
      Iop_InterleaveOddLanes16x8, Iop_InterleaveEvenLanes16x8,
      Iop_InterleaveOddLanes32x4, Iop_InterleaveEvenLanes32x4,

      /* CONCATENATION -- build a new value by concatenating either
         the even or odd lanes of both operands. */
      Iop_CatOddLanes8x16, Iop_CatOddLanes16x8, Iop_CatOddLanes32x4,
      Iop_CatEvenLanes8x16, Iop_CatEvenLanes16x8, Iop_CatEvenLanes32x4,

      /* GET elements of VECTOR
         GET is binop (V128, I8) -> I<elem_size> */
      /* Note: the arm back-end handles only constant second argument. */
      Iop_GetElem8x16, Iop_GetElem16x8, Iop_GetElem32x4, Iop_GetElem64x2,

      /* DUPLICATING -- copy value to all lanes */
      Iop_Dup8x16,   Iop_Dup16x8,   Iop_Dup32x4,

      /* EXTRACT -- copy 16-arg3 highest bytes from arg1 to 16-arg3 lowest bytes
         of result and arg3 lowest bytes of arg2 to arg3 highest bytes of
         result.
         It is a triop: (V128, V128, I8) -> V128 */
      /* Note: the ARM back end handles only constant arg3 in this operation. */
      Iop_ExtractV128,

      /* REVERSE the order of elements in each Half-words, Words,
         Double-words */
      /* Examples:
            Reverse32_16x8([a,b,c,d,e,f,g,h]) = [b,a,d,c,f,e,h,g]
            Reverse64_16x8([a,b,c,d,e,f,g,h]) = [d,c,b,a,h,g,f,e] */
      Iop_Reverse16_8x16,
      Iop_Reverse32_8x16, Iop_Reverse32_16x8,
      Iop_Reverse64_8x16, Iop_Reverse64_16x8, Iop_Reverse64_32x4,

      /* PERMUTING -- copy src bytes to dst,
         as indexed by control vector bytes:
            for i in 0 .. 15 . result[i] = argL[ argR[i] ] 
         argR[i] values may only be in the range 0 .. 15, else behaviour
         is undefined. */
      Iop_Perm8x16,
      Iop_Perm32x4, /* ditto, except argR values are restricted to 0 .. 3 */

      /* Vector Reciprocal Estimate and Vector Reciprocal Square Root Estimate
         See floating-point equiwalents for details. */
      Iop_Recip32x4, Iop_Rsqrte32x4,

      /* ------------------ 256-bit SIMD Integer. ------------------ */

      /* Pack/unpack */
      Iop_V256to64_0,  // V256 -> I64, extract least significant lane
      Iop_V256to64_1,
      Iop_V256to64_2,
      Iop_V256to64_3,  // V256 -> I64, extract most significant lane

      Iop_64x4toV256,  // (I64,I64,I64,I64)->V256
                       // first arg is most significant lane

      Iop_V256toV128_0, // V256 -> V128, less significant lane
      Iop_V256toV128_1, // V256 -> V128, more significant lane
      Iop_V128HLtoV256, // (V128,V128)->V256, first arg is most signif

      Iop_AndV256,
      Iop_OrV256,
      Iop_XorV256,
      Iop_NotV256,

      /* MISC (vector integer cmp != 0) */
      Iop_CmpNEZ32x8, Iop_CmpNEZ64x4,

      /* ------------------ 256-bit SIMD FP. ------------------ */
      Iop_Add64Fx4,
      Iop_Sub64Fx4,
      Iop_Mul64Fx4,
      Iop_Div64Fx4,
      Iop_Add32Fx8,
      Iop_Sub32Fx8,
      Iop_Mul32Fx8,
      Iop_Div32Fx8,

      Iop_Sqrt32Fx8,
      Iop_Sqrt64Fx4,
      Iop_RSqrt32Fx8,
      Iop_Recip32Fx8,

      Iop_Max32Fx8, Iop_Min32Fx8,
      Iop_Max64Fx4, Iop_Min64Fx4
   }
   IROp;

/* Pretty-print an op. */
extern void ppIROp ( IROp );


/* Encoding of IEEE754-specified rounding modes.  This is the same as
   the encoding used by Intel IA32 to indicate x87 rounding mode.
   Note, various front and back ends rely on the actual numerical
   values of these, so do not change them. */
typedef
   enum { 
      Irrm_NEAREST = 0, 
      Irrm_NegINF  = 1, 
      Irrm_PosINF  = 2, 
      Irrm_ZERO    = 3 
   }
   IRRoundingMode;

/* DFP encoding of IEEE754 2008 specified rounding modes extends the two bit
 * binary floating point rounding mode (IRRoundingMode) to three bits.  The 
 * DFP rounding modes are a super set of the binary rounding modes.  The 
 * encoding was chosen such that the mapping of the least significant two bits
 * of the IR to POWER encodings is same.  The upper IR encoding bit is just
 * a logical OR of the upper rounding mode bit from the POWER encoding.
 */
typedef
   enum { 
      Irrm_DFP_NEAREST              = 0,  // Round to nearest, ties to even
      Irrm_DFP_NegINF               = 1,  // Round to negative infinity
      Irrm_DFP_PosINF               = 2,  // Round to posative infinity
      Irrm_DFP_ZERO                 = 3,  // Round toward zero
      Irrm_DFP_NEAREST_TIE_AWAY_0   = 4,  // Round to nearest, ties away from 0
      Irrm_DFP_PREPARE_SHORTER      = 5,  // Round to prepare for storter 
                                          // precision
      Irrm_DFP_AWAY_FROM_ZERO       = 6,  // Round to away from 0
      Irrm_DFP_NEAREST_TIE_TOWARD_0 = 7   // Round to nearest, ties towards 0
   }
   IRRoundingModeDFP;

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

typedef IRCmpF64Result IRCmpF32Result;
typedef IRCmpF64Result IRCmpF128Result;

/* ------------------ Expressions ------------------ */

typedef struct _IRQop   IRQop;   /* forward declaration */
typedef struct _IRTriop IRTriop; /* forward declaration */


/* The different kinds of expressions.  Their meaning is explained below
   in the comments for IRExpr. */
typedef
   enum { 
      Iex_Binder=0x15000,
      Iex_Get,
      Iex_GetI,
      Iex_RdTmp,
      Iex_Qop,
      Iex_Triop,
      Iex_Binop,
      Iex_Unop,
      Iex_Load,
      Iex_Const,
      Iex_Mux0X,
      Iex_CCall
   }
   IRExprTag;

/* An expression.  Stored as a tagged union.  'tag' indicates what kind
   of expression this is.  'Iex' is the union that holds the fields.  If
   an IRExpr 'e' has e.tag equal to Iex_Load, then it's a load
   expression, and the fields can be accessed with
   'e.Iex.Load.<fieldname>'.

   For each kind of expression, we show what it looks like when
   pretty-printed with ppIRExpr().
*/
typedef
   struct _IRExpr
   IRExpr;

struct _IRExpr {
   IRExprTag tag;
   union {
      /* Used only in pattern matching within Vex.  Should not be seen
         outside of Vex. */
      struct {
         Int binder;
      } Binder;

      /* Read a guest register, at a fixed offset in the guest state.
         ppIRExpr output: GET:<ty>(<offset>), eg. GET:I32(0)
      */
      struct {
         Int    offset;    /* Offset into the guest state */
         IRType ty;        /* Type of the value being read */
      } Get;

      /* Read a guest register at a non-fixed offset in the guest
         state.  This allows circular indexing into parts of the guest
         state, which is essential for modelling situations where the
         identity of guest registers is not known until run time.  One
         example is the x87 FP register stack.

         The part of the guest state to be treated as a circular array
         is described in the IRRegArray 'descr' field.  It holds the
         offset of the first element in the array, the type of each
         element, and the number of elements.

         The array index is indicated rather indirectly, in a way
         which makes optimisation easy: as the sum of variable part
         (the 'ix' field) and a constant offset (the 'bias' field).

         Since the indexing is circular, the actual array index to use
         is computed as (ix + bias) % num-of-elems-in-the-array.

         Here's an example.  The description

            (96:8xF64)[t39,-7]

         describes an array of 8 F64-typed values, the
         guest-state-offset of the first being 96.  This array is
         being indexed at (t39 - 7) % 8.

         It is important to get the array size/type exactly correct
         since IR optimisation looks closely at such info in order to
         establish aliasing/non-aliasing between seperate GetI and
         PutI events, which is used to establish when they can be
         reordered, etc.  Putting incorrect info in will lead to
         obscure IR optimisation bugs.

            ppIRExpr output: GETI<descr>[<ix>,<bias]
                         eg. GETI(128:8xI8)[t1,0]
      */
      struct {
         IRRegArray* descr; /* Part of guest state treated as circular */
         IRExpr*     ix;    /* Variable part of index into array */
         Int         bias;  /* Constant offset part of index into array */
      } GetI;

      /* The value held by a temporary.
         ppIRExpr output: t<tmp>, eg. t1
      */
      struct {
         IRTemp tmp;       /* The temporary number */
      } RdTmp;

      /* A quaternary operation.
         ppIRExpr output: <op>(<arg1>, <arg2>, <arg3>, <arg4>),
                      eg. MAddF64r32(t1, t2, t3, t4)
      */
      struct {
        IRQop* details;
      } Qop;

      /* A ternary operation.
         ppIRExpr output: <op>(<arg1>, <arg2>, <arg3>),
                      eg. MulF64(1, 2.0, 3.0)
      */
      struct {
        IRTriop* details;
      } Triop;

      /* A binary operation.
         ppIRExpr output: <op>(<arg1>, <arg2>), eg. Add32(t1,t2)
      */
      struct {
         IROp op;          /* op-code   */
         IRExpr* arg1;     /* operand 1 */
         IRExpr* arg2;     /* operand 2 */
      } Binop;

      /* A unary operation.
         ppIRExpr output: <op>(<arg>), eg. Neg8(t1)
      */
      struct {
         IROp    op;       /* op-code */
         IRExpr* arg;      /* operand */
      } Unop;

      /* A load from memory -- a normal load, not a load-linked.
         Load-Linkeds (and Store-Conditionals) are instead represented
         by IRStmt.LLSC since Load-Linkeds have side effects and so
         are not semantically valid IRExpr's.
         ppIRExpr output: LD<end>:<ty>(<addr>), eg. LDle:I32(t1)
      */
      struct {
         IREndness end;    /* Endian-ness of the load */
         IRType    ty;     /* Type of the loaded value */
         IRExpr*   addr;   /* Address being loaded from */
      } Load;

      /* A constant-valued expression.
         ppIRExpr output: <con>, eg. 0x4:I32
      */
      struct {
         IRConst* con;     /* The constant itself */
      } Const;

      /* A call to a pure (no side-effects) helper C function.

         With the 'cee' field, 'name' is the function's name.  It is
         only used for pretty-printing purposes.  The address to call
         (host address, of course) is stored in the 'addr' field
         inside 'cee'.

         The 'args' field is a NULL-terminated array of arguments.
         The stated return IRType, and the implied argument types,
         must match that of the function being called well enough so
         that the back end can actually generate correct code for the
         call.

         The called function **must** satisfy the following:

         * no side effects -- must be a pure function, the result of
           which depends only on the passed parameters.

         * it may not look at, nor modify, any of the guest state
           since that would hide guest state transitions from
           instrumenters

         * it may not access guest memory, since that would hide
           guest memory transactions from the instrumenters

         * it must not assume that arguments are being evaluated in a
           particular order. The oder of evaluation is unspecified.

         This is restrictive, but makes the semantics clean, and does
         not interfere with IR optimisation.

         If you want to call a helper which can mess with guest state
         and/or memory, instead use Ist_Dirty.  This is a lot more
         flexible, but you have to give a bunch of details about what
         the helper does (and you better be telling the truth,
         otherwise any derived instrumentation will be wrong).  Also
         Ist_Dirty inhibits various IR optimisations and so can cause
         quite poor code to be generated.  Try to avoid it.

         ppIRExpr output: <cee>(<args>):<retty>
                      eg. foo{0x80489304}(t1, t2):I32
      */
      struct {
         IRCallee* cee;    /* Function to call. */
         IRType    retty;  /* Type of return value. */
         IRExpr**  args;   /* Vector of argument expressions. */
      }  CCall;

      /* A ternary if-then-else operator.  It returns expr0 if cond is
         zero, exprX otherwise.  Note that it is STRICT, ie. both
         expr0 and exprX are evaluated in all cases.

         ppIRExpr output: Mux0X(<cond>,<expr0>,<exprX>),
                         eg. Mux0X(t6,t7,t8)
      */
      struct {
         IRExpr* cond;     /* Condition */
         IRExpr* expr0;    /* True expression */
         IRExpr* exprX;    /* False expression */
      } Mux0X;
   } Iex;
};

/* ------------------ A ternary expression ---------------------- */
struct _IRTriop {
   IROp op;          /* op-code   */
   IRExpr* arg1;     /* operand 1 */
   IRExpr* arg2;     /* operand 2 */
   IRExpr* arg3;     /* operand 3 */
};

/* ------------------ A quarternary expression ------------------ */
struct _IRQop {
   IROp op;          /* op-code   */
   IRExpr* arg1;     /* operand 1 */
   IRExpr* arg2;     /* operand 2 */
   IRExpr* arg3;     /* operand 3 */
   IRExpr* arg4;     /* operand 4 */
};

/* Expression constructors. */
extern IRExpr* IRExpr_Binder ( Int binder );
extern IRExpr* IRExpr_Get    ( Int off, IRType ty );
extern IRExpr* IRExpr_GetI   ( IRRegArray* descr, IRExpr* ix, Int bias );
extern IRExpr* IRExpr_RdTmp  ( IRTemp tmp );
extern IRExpr* IRExpr_Qop    ( IROp op, IRExpr* arg1, IRExpr* arg2, 
                                        IRExpr* arg3, IRExpr* arg4 );
extern IRExpr* IRExpr_Triop  ( IROp op, IRExpr* arg1, 
                                        IRExpr* arg2, IRExpr* arg3 );
extern IRExpr* IRExpr_Binop  ( IROp op, IRExpr* arg1, IRExpr* arg2 );
extern IRExpr* IRExpr_Unop   ( IROp op, IRExpr* arg );
extern IRExpr* IRExpr_Load   ( IREndness end, IRType ty, IRExpr* addr );
extern IRExpr* IRExpr_Const  ( IRConst* con );
extern IRExpr* IRExpr_CCall  ( IRCallee* cee, IRType retty, IRExpr** args );
extern IRExpr* IRExpr_Mux0X  ( IRExpr* cond, IRExpr* expr0, IRExpr* exprX );

/* Deep-copy an IRExpr. */
extern IRExpr* deepCopyIRExpr ( IRExpr* );

/* Pretty-print an IRExpr. */
extern void ppIRExpr ( IRExpr* );

/* NULL-terminated IRExpr vector constructors, suitable for
   use as arg lists in clean/dirty helper calls. */
extern IRExpr** mkIRExprVec_0 ( void );
extern IRExpr** mkIRExprVec_1 ( IRExpr* );
extern IRExpr** mkIRExprVec_2 ( IRExpr*, IRExpr* );
extern IRExpr** mkIRExprVec_3 ( IRExpr*, IRExpr*, IRExpr* );
extern IRExpr** mkIRExprVec_4 ( IRExpr*, IRExpr*, IRExpr*, IRExpr* );
extern IRExpr** mkIRExprVec_5 ( IRExpr*, IRExpr*, IRExpr*, IRExpr*,
                                IRExpr* );
extern IRExpr** mkIRExprVec_6 ( IRExpr*, IRExpr*, IRExpr*, IRExpr*,
                                IRExpr*, IRExpr* );
extern IRExpr** mkIRExprVec_7 ( IRExpr*, IRExpr*, IRExpr*, IRExpr*,
                                IRExpr*, IRExpr*, IRExpr* );
extern IRExpr** mkIRExprVec_8 ( IRExpr*, IRExpr*, IRExpr*, IRExpr*,
                                IRExpr*, IRExpr*, IRExpr*, IRExpr*);

/* IRExpr copiers:
   - shallowCopy: shallow-copy (ie. create a new vector that shares the
     elements with the original).
   - deepCopy: deep-copy (ie. create a completely new vector). */
extern IRExpr** shallowCopyIRExprVec ( IRExpr** );
extern IRExpr** deepCopyIRExprVec ( IRExpr** );

/* Make a constant expression from the given host word taking into
   account (of course) the host word size. */
extern IRExpr* mkIRExpr_HWord ( HWord );

/* Convenience function for constructing clean helper calls. */
extern 
IRExpr* mkIRExprCCall ( IRType retty,
                        Int regparms, HChar* name, void* addr, 
                        IRExpr** args );


/* Convenience functions for atoms (IRExprs which are either Iex_Tmp or
 * Iex_Const). */
static inline Bool isIRAtom ( IRExpr* e ) {
   return toBool(e->tag == Iex_RdTmp || e->tag == Iex_Const);
}

/* Are these two IR atoms identical?  Causes an assertion
   failure if they are passed non-atoms. */
extern Bool eqIRAtom ( IRExpr*, IRExpr* );


/* ------------------ Jump kinds ------------------ */

/* This describes hints which can be passed to the dispatcher at guest
   control-flow transfer points.

   Re Ijk_TInval: the guest state _must_ have two pseudo-registers,
   guest_TISTART and guest_TILEN, which specify the start and length
   of the region to be invalidated.  These are both the size of a
   guest word.  It is the responsibility of the relevant toIR.c to
   ensure that these are filled in with suitable values before issuing
   a jump of kind Ijk_TInval.

   Re Ijk_EmWarn and Ijk_EmFail: the guest state must have a
   pseudo-register guest_EMWARN, which is 32-bits regardless of the
   host or guest word size.  That register should be made to hold an
   EmWarn_* value to indicate the reason for the exit.

   In the case of Ijk_EmFail, the exit is fatal (Vex-generated code
   cannot continue) and so the jump destination can be anything.

   Re Ijk_Sys_ (syscall jumps): the guest state must have a
   pseudo-register guest_IP_AT_SYSCALL, which is the size of a guest
   word.  Front ends should set this to be the IP at the most recently
   executed kernel-entering (system call) instruction.  This makes it
   very much easier (viz, actually possible at all) to back up the
   guest to restart a syscall that has been interrupted by a signal.
*/
typedef
   enum {
      Ijk_INVALID=0x16000, 
      Ijk_Boring,         /* not interesting; just goto next */
      Ijk_Call,           /* guest is doing a call */
      Ijk_Ret,            /* guest is doing a return */
      Ijk_ClientReq,      /* do guest client req before continuing */
      Ijk_Yield,          /* client is yielding to thread scheduler */
      Ijk_EmWarn,         /* report emulation warning before continuing */
      Ijk_EmFail,         /* emulation critical (FATAL) error; give up */
      Ijk_NoDecode,       /* next instruction cannot be decoded */
      Ijk_MapFail,        /* Vex-provided address translation failed */
      Ijk_TInval,         /* Invalidate translations before continuing. */
      Ijk_NoRedir,        /* Jump to un-redirected guest addr */
      Ijk_SigTRAP,        /* current instruction synths SIGTRAP */
      Ijk_SigSEGV,        /* current instruction synths SIGSEGV */
      Ijk_SigBUS,         /* current instruction synths SIGBUS */
      /* Unfortunately, various guest-dependent syscall kinds.  They
	 all mean: do a syscall before continuing. */
      Ijk_Sys_syscall,    /* amd64 'syscall', ppc 'sc', arm 'svc #0' */
      Ijk_Sys_int32,      /* amd64/x86 'int $0x20' */
      Ijk_Sys_int128,     /* amd64/x86 'int $0x80' */
      Ijk_Sys_int129,     /* amd64/x86 'int $0x81' */
      Ijk_Sys_int130,     /* amd64/x86 'int $0x82' */
      Ijk_Sys_sysenter    /* x86 'sysenter'.  guest_EIP becomes 
                             invalid at the point this happens. */
   }
   IRJumpKind;

extern void ppIRJumpKind ( IRJumpKind );


/* ------------------ Dirty helper calls ------------------ */

/* A dirty call is a flexible mechanism for calling (possibly
   conditionally) a helper function or procedure.  The helper function
   may read, write or modify client memory, and may read, write or
   modify client state.  It can take arguments and optionally return a
   value.  It may return different results and/or do different things
   when called repeatedly with the same arguments, by means of storing
   private state.

   If a value is returned, it is assigned to the nominated return
   temporary.

   Dirty calls are statements rather than expressions for obvious
   reasons.  If a dirty call is marked as writing guest state, any
   values derived from the written parts of the guest state are
   invalid.  Similarly, if the dirty call is stated as writing
   memory, any loaded values are invalidated by it.

   In order that instrumentation is possible, the call must state, and
   state correctly:

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
   arguments are evaluated REGARDLESS of the guard value.  The order of
   argument evaluation is unspecified. The guard expression is evaluated
   AFTER the arguments have been evaluated.
*/

#define VEX_N_FXSTATE  7   /* enough for FXSAVE/FXRSTOR on x86 */

/* Effects on resources (eg. registers, memory locations) */
typedef
   enum {
      Ifx_None = 0x1700,    /* no effect */
      Ifx_Read,             /* reads the resource */
      Ifx_Write,            /* writes the resource */
      Ifx_Modify,           /* modifies the resource */
   }
   IREffect;

/* Pretty-print an IREffect */
extern void ppIREffect ( IREffect );


typedef
   struct _IRDirty {
      /* What to call, and details of args/results.  .guard must be
         non-NULL.  If .tmp is not IRTemp_INVALID (that is, the call
         returns a result) then .guard must be demonstrably (at
         JIT-time) always true, that is, the call must be
         unconditional.  Conditional calls that assign .tmp are not
         allowed. */
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
         IREffect fx:16;   /* read, write or modify?  Ifx_None is invalid. */
         UShort   offset;
         UShort   size;
         UChar    nRepeats;
         UChar    repeatLen;
      } fxState[VEX_N_FXSTATE];
      /* The access can be repeated, as specified by nRepeats and
         repeatLen.  To describe only a single access, nRepeats and
         repeatLen should be zero.  Otherwise, repeatLen must be a
         multiple of size and greater than size. */
      /* Overall, the parts of the guest state denoted by (offset,
         size, nRepeats, repeatLen) is
               [offset, +size)
            and, if nRepeats > 0,
               for (i = 1; i <= nRepeats; i++)
                  [offset + i * repeatLen, +size)
         A convenient way to enumerate all segments is therefore
            for (i = 0; i < 1 + nRepeats; i++)
               [offset + i * repeatLen, +size)
      */
   }
   IRDirty;

/* Pretty-print a dirty call */
extern void     ppIRDirty ( IRDirty* );

/* Allocate an uninitialised dirty call */
extern IRDirty* emptyIRDirty ( void );

/* Deep-copy a dirty call */
extern IRDirty* deepCopyIRDirty ( IRDirty* );

/* A handy function which takes some of the tedium out of constructing
   dirty helper calls.  The called function impliedly does not return
   any value and has a constant-True guard.  The call is marked as
   accessing neither guest state nor memory (hence the "unsafe"
   designation) -- you can change this marking later if need be.  A
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


/* --------------- Memory Bus Events --------------- */

typedef
   enum { 
      Imbe_Fence=0x18000, 
      /* Needed only on ARM.  It cancels a reservation made by a
         preceding Linked-Load, and needs to be handed through to the
         back end, just as LL and SC themselves are. */
      Imbe_CancelReservation
   }
   IRMBusEvent;

extern void ppIRMBusEvent ( IRMBusEvent );


/* --------------- Compare and Swap --------------- */

/* This denotes an atomic compare and swap operation, either
   a single-element one or a double-element one.

   In the single-element case:

     .addr is the memory address.
     .end  is the endianness with which memory is accessed

     If .addr contains the same value as .expdLo, then .dataLo is
     written there, else there is no write.  In both cases, the
     original value at .addr is copied into .oldLo.

     Types: .expdLo, .dataLo and .oldLo must all have the same type.
     It may be any integral type, viz: I8, I16, I32 or, for 64-bit
     guests, I64.

     .oldHi must be IRTemp_INVALID, and .expdHi and .dataHi must
     be NULL.

   In the double-element case:

     .addr is the memory address.
     .end  is the endianness with which memory is accessed

     The operation is the same:

     If .addr contains the same value as .expdHi:.expdLo, then
     .dataHi:.dataLo is written there, else there is no write.  In
     both cases the original value at .addr is copied into
     .oldHi:.oldLo.

     Types: .expdHi, .expdLo, .dataHi, .dataLo, .oldHi, .oldLo must
     all have the same type, which may be any integral type, viz: I8,
     I16, I32 or, for 64-bit guests, I64.

     The double-element case is complicated by the issue of
     endianness.  In all cases, the two elements are understood to be
     located adjacently in memory, starting at the address .addr.

       If .end is Iend_LE, then the .xxxLo component is at the lower
       address and the .xxxHi component is at the higher address, and
       each component is itself stored little-endianly.

       If .end is Iend_BE, then the .xxxHi component is at the lower
       address and the .xxxLo component is at the higher address, and
       each component is itself stored big-endianly.

   This allows representing more cases than most architectures can
   handle.  For example, x86 cannot do DCAS on 8- or 16-bit elements.

   How to know if the CAS succeeded?

   * if .oldLo == .expdLo (resp. .oldHi:.oldLo == .expdHi:.expdLo),
     then the CAS succeeded, .dataLo (resp. .dataHi:.dataLo) is now
     stored at .addr, and the original value there was .oldLo (resp
     .oldHi:.oldLo).

   * if .oldLo != .expdLo (resp. .oldHi:.oldLo != .expdHi:.expdLo),
     then the CAS failed, and the original value at .addr was .oldLo
     (resp. .oldHi:.oldLo).

   Hence it is easy to know whether or not the CAS succeeded.
*/
typedef
   struct {
      IRTemp    oldHi;  /* old value of *addr is written here */
      IRTemp    oldLo;
      IREndness end;    /* endianness of the data in memory */
      IRExpr*   addr;   /* store address */
      IRExpr*   expdHi; /* expected old value at *addr */
      IRExpr*   expdLo;
      IRExpr*   dataHi; /* new value for *addr */
      IRExpr*   dataLo;
   }
   IRCAS;

extern void ppIRCAS ( IRCAS* cas );

extern IRCAS* mkIRCAS ( IRTemp oldHi, IRTemp oldLo,
                        IREndness end, IRExpr* addr, 
                        IRExpr* expdHi, IRExpr* expdLo,
                        IRExpr* dataHi, IRExpr* dataLo );

extern IRCAS* deepCopyIRCAS ( IRCAS* );


/* ------------------ Circular Array Put ------------------ */
typedef
   struct {
      IRRegArray* descr; /* Part of guest state treated as circular */
      IRExpr*     ix;    /* Variable part of index into array */
      Int         bias;  /* Constant offset part of index into array */
      IRExpr*     data;  /* The value to write */
   } IRPutI;

extern void ppIRPutI ( IRPutI* puti );

extern IRPutI* mkIRPutI ( IRRegArray* descr, IRExpr* ix,
                          Int bias, IRExpr* data );

extern IRPutI* deepCopyIRPutI ( IRPutI* );


/* ------------------ Statements ------------------ */

/* The different kinds of statements.  Their meaning is explained
   below in the comments for IRStmt.

   Those marked META do not represent code, but rather extra
   information about the code.  These statements can be removed
   without affecting the functional behaviour of the code, however
   they are required by some IR consumers such as tools that
   instrument the code.
*/

typedef 
   enum {
      Ist_NoOp=0x19000,
      Ist_IMark,     /* META */
      Ist_AbiHint,   /* META */
      Ist_Put,
      Ist_PutI,
      Ist_WrTmp,
      Ist_Store,
      Ist_CAS,
      Ist_LLSC,
      Ist_Dirty,
      Ist_MBE,       /* META (maybe) */
      Ist_Exit
   } 
   IRStmtTag;

/* A statement.  Stored as a tagged union.  'tag' indicates what kind
   of expression this is.  'Ist' is the union that holds the fields.
   If an IRStmt 'st' has st.tag equal to Iex_Store, then it's a store
   statement, and the fields can be accessed with
   'st.Ist.Store.<fieldname>'.

   For each kind of statement, we show what it looks like when
   pretty-printed with ppIRStmt().
*/
typedef
   struct _IRStmt {
      IRStmtTag tag;
      union {
         /* A no-op (usually resulting from IR optimisation).  Can be
            omitted without any effect.

            ppIRStmt output: IR-NoOp
         */
         struct {
	 } NoOp;

         /* META: instruction mark.  Marks the start of the statements
            that represent a single machine instruction (the end of
            those statements is marked by the next IMark or the end of
            the IRSB).  Contains the address and length of the
            instruction.

            It also contains a delta value.  The delta must be
            subtracted from a guest program counter value before
            attempting to establish, by comparison with the address
            and length values, whether or not that program counter
            value refers to this instruction.  For x86, amd64, ppc32,
            ppc64 and arm, the delta value is zero.  For Thumb
            instructions, the delta value is one.  This is because, on
            Thumb, guest PC values (guest_R15T) are encoded using the
            top 31 bits of the instruction address and a 1 in the lsb;
            hence they appear to be (numerically) 1 past the start of
            the instruction they refer to.  IOW, guest_R15T on ARM
            holds a standard ARM interworking address.

            ppIRStmt output: ------ IMark(<addr>, <len>, <delta>) ------,
                         eg. ------ IMark(0x4000792, 5, 0) ------,
         */
         struct {
            Addr64 addr;   /* instruction address */
            Int    len;    /* instruction length */
            UChar  delta;  /* addr = program counter as encoded in guest state
                                     - delta */
         } IMark;

         /* META: An ABI hint, which says something about this
            platform's ABI.

            At the moment, the only AbiHint is one which indicates
            that a given chunk of address space, [base .. base+len-1],
            has become undefined.  This is used on amd64-linux and
            some ppc variants to pass stack-redzoning hints to whoever
            wants to see them.  It also indicates the address of the
            next (dynamic) instruction that will be executed.  This is
            to help Memcheck to origin tracking.

            ppIRStmt output: ====== AbiHint(<base>, <len>, <nia>) ======
                         eg. ====== AbiHint(t1, 16, t2) ======
         */
         struct {
            IRExpr* base;     /* Start  of undefined chunk */
            Int     len;      /* Length of undefined chunk */
            IRExpr* nia;      /* Address of next (guest) insn */
         } AbiHint;

         /* Write a guest register, at a fixed offset in the guest state.
            ppIRStmt output: PUT(<offset>) = <data>, eg. PUT(60) = t1
         */
         struct {
            Int     offset;   /* Offset into the guest state */
            IRExpr* data;     /* The value to write */
         } Put;

         /* Write a guest register, at a non-fixed offset in the guest
            state.  See the comment for GetI expressions for more
            information.

            ppIRStmt output: PUTI<descr>[<ix>,<bias>] = <data>,
                         eg. PUTI(64:8xF64)[t5,0] = t1
         */
         struct {
            IRPutI* details;
         } PutI;

         /* Assign a value to a temporary.  Note that SSA rules require
            each tmp is only assigned to once.  IR sanity checking will
            reject any block containing a temporary which is not assigned
            to exactly once.

            ppIRStmt output: t<tmp> = <data>, eg. t1 = 3
         */
         struct {
            IRTemp  tmp;   /* Temporary  (LHS of assignment) */
            IRExpr* data;  /* Expression (RHS of assignment) */
         } WrTmp;

         /* Write a value to memory.  This is a normal store, not a
            Store-Conditional.  To represent a Store-Conditional,
            instead use IRStmt.LLSC.
            ppIRStmt output: ST<end>(<addr>) = <data>, eg. STle(t1) = t2
         */
         struct {
            IREndness end;    /* Endianness of the store */
            IRExpr*   addr;   /* store address */
            IRExpr*   data;   /* value to write */
         } Store;

         /* Do an atomic compare-and-swap operation.  Semantics are
            described above on a comment at the definition of IRCAS.

            ppIRStmt output:
               t<tmp> = CAS<end>(<addr> :: <expected> -> <new>)
            eg
               t1 = CASle(t2 :: t3->Add32(t3,1))
               which denotes a 32-bit atomic increment 
               of a value at address t2

            A double-element CAS may also be denoted, in which case <tmp>,
            <expected> and <new> are all pairs of items, separated by
            commas.
         */
         struct {
            IRCAS* details;
         } CAS;

         /* Either Load-Linked or Store-Conditional, depending on
            STOREDATA.

            If STOREDATA is NULL then this is a Load-Linked, meaning
            that data is loaded from memory as normal, but a
            'reservation' for the address is also lodged in the
            hardware.

               result = Load-Linked(addr, end)

            The data transfer type is the type of RESULT (I32, I64,
            etc).  ppIRStmt output:

               result = LD<end>-Linked(<addr>), eg. LDbe-Linked(t1)

            If STOREDATA is not NULL then this is a Store-Conditional,
            hence:

               result = Store-Conditional(addr, storedata, end)

            The data transfer type is the type of STOREDATA and RESULT
            has type Ity_I1. The store may fail or succeed depending
            on the state of a previously lodged reservation on this
            address.  RESULT is written 1 if the store succeeds and 0
            if it fails.  eg ppIRStmt output:

               result = ( ST<end>-Cond(<addr>) = <storedata> )
               eg t3 = ( STbe-Cond(t1, t2) )

            In all cases, the address must be naturally aligned for
            the transfer type -- any misaligned addresses should be
            caught by a dominating IR check and side exit.  This
            alignment restriction exists because on at least some
            LL/SC platforms (ppc), stwcx. etc will trap w/ SIGBUS on
            misaligned addresses, and we have to actually generate
            stwcx. on the host, and we don't want it trapping on the
            host.

            Summary of rules for transfer type:
              STOREDATA == NULL (LL):
                transfer type = type of RESULT
              STOREDATA != NULL (SC):
                transfer type = type of STOREDATA, and RESULT :: Ity_I1
         */
         struct {
            IREndness end;
            IRTemp    result;
            IRExpr*   addr;
            IRExpr*   storedata; /* NULL => LL, non-NULL => SC */
         } LLSC;

         /* Call (possibly conditionally) a C function that has side
            effects (ie. is "dirty").  See the comments above the
            IRDirty type declaration for more information.

            ppIRStmt output:
               t<tmp> = DIRTY <guard> <effects> 
                  ::: <callee>(<args>)
            eg.
               t1 = DIRTY t27 RdFX-gst(16,4) RdFX-gst(60,4)
                     ::: foo{0x380035f4}(t2)
         */       
         struct {
            IRDirty* details;
         } Dirty;

         /* A memory bus event - a fence, or acquisition/release of the
            hardware bus lock.  IR optimisation treats all these as fences
            across which no memory references may be moved.
            ppIRStmt output: MBusEvent-Fence,
                             MBusEvent-BusLock, MBusEvent-BusUnlock.
         */
         struct {
            IRMBusEvent event;
         } MBE;

         /* Conditional exit from the middle of an IRSB.
            ppIRStmt output: if (<guard>) goto {<jk>} <dst>
                         eg. if (t69) goto {Boring} 0x4000AAA:I32
            If <guard> is true, the guest state is also updated by
            PUT-ing <dst> at <offsIP>.  This is done because a
            taken exit must update the guest program counter.
         */
         struct {
            IRExpr*    guard;    /* Conditional expression */
            IRConst*   dst;      /* Jump target (constant only) */
            IRJumpKind jk;       /* Jump kind */
            Int        offsIP;   /* Guest state offset for IP */
         } Exit;
      } Ist;
   }
   IRStmt;

/* Statement constructors. */
extern IRStmt* IRStmt_NoOp    ( void );
extern IRStmt* IRStmt_IMark   ( Addr64 addr, Int len, UChar delta );
extern IRStmt* IRStmt_AbiHint ( IRExpr* base, Int len, IRExpr* nia );
extern IRStmt* IRStmt_Put     ( Int off, IRExpr* data );
extern IRStmt* IRStmt_PutI    ( IRPutI* details );
extern IRStmt* IRStmt_WrTmp   ( IRTemp tmp, IRExpr* data );
extern IRStmt* IRStmt_Store   ( IREndness end, IRExpr* addr, IRExpr* data );
extern IRStmt* IRStmt_CAS     ( IRCAS* details );
extern IRStmt* IRStmt_LLSC    ( IREndness end, IRTemp result,
                                IRExpr* addr, IRExpr* storedata );
extern IRStmt* IRStmt_Dirty   ( IRDirty* details );
extern IRStmt* IRStmt_MBE     ( IRMBusEvent event );
extern IRStmt* IRStmt_Exit    ( IRExpr* guard, IRJumpKind jk, IRConst* dst,
                                Int offsIP );

/* Deep-copy an IRStmt. */
extern IRStmt* deepCopyIRStmt ( IRStmt* );

/* Pretty-print an IRStmt. */
extern void ppIRStmt ( IRStmt* );


/* ------------------ Basic Blocks ------------------ */

/* Type environments: a bunch of statements, expressions, etc, are
   incomplete without an environment indicating the type of each
   IRTemp.  So this provides one.  IR temporaries are really just
   unsigned ints and so this provides an array, 0 .. n_types_used-1 of
   them.
*/
typedef
   struct {
      IRType* types;
      Int     types_size;
      Int     types_used;
   }
   IRTypeEnv;

/* Obtain a new IRTemp */
extern IRTemp newIRTemp ( IRTypeEnv*, IRType );

/* Deep-copy a type environment */
extern IRTypeEnv* deepCopyIRTypeEnv ( IRTypeEnv* );

/* Pretty-print a type environment */
extern void ppIRTypeEnv ( IRTypeEnv* );


/* Code blocks, which in proper compiler terminology are superblocks
   (single entry, multiple exit code sequences) contain:

   - A table giving a type for each temp (the "type environment")
   - An expandable array of statements
   - An expression of type 32 or 64 bits, depending on the
     guest's word size, indicating the next destination if the block 
     executes all the way to the end, without a side exit
   - An indication of any special actions (JumpKind) needed
     for this final jump.
   - Offset of the IP field in the guest state.  This will be
     updated before the final jump is done.
   
   "IRSB" stands for "IR Super Block".
*/
typedef
   struct {
      IRTypeEnv* tyenv;
      IRStmt**   stmts;
      Int        stmts_size;
      Int        stmts_used;
      IRExpr*    next;
      IRJumpKind jumpkind;
      Int        offsIP;
   }
   IRSB;

/* Allocate a new, uninitialised IRSB */
extern IRSB* emptyIRSB ( void );

/* Deep-copy an IRSB */
extern IRSB* deepCopyIRSB ( IRSB* );

/* Deep-copy an IRSB, except for the statements list, which set to be
   a new, empty, list of statements. */
extern IRSB* deepCopyIRSBExceptStmts ( IRSB* );

/* Pretty-print an IRSB */
extern void ppIRSB ( IRSB* );

/* Append an IRStmt to an IRSB */
extern void addStmtToIRSB ( IRSB*, IRStmt* );


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
extern void sanityCheckIRSB ( IRSB*  bb, 
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
