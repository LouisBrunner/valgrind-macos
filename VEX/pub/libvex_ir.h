
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_ir.h) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBVEX_IR_H
#define __LIBVEX_IR_H

#include "libvex_basictypes.h"


/*---------------------------------------------------------------*/
/*--- Type definitions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* ------------------ Types ------------------ */

typedef 
   enum { Ity_INVALID=0x10FFF,
          Ity_Bit=0x11000, 
          Ity_I8, Ity_I16, Ity_I32, Ity_I64,
          Ity_F32, Ity_F64
   }
   IRType;

extern void ppIRType ( IRType );


/* ------------------ Constants ------------------ */

typedef
   enum { Ico_Bit=0x12000,
          Ico_U8, Ico_U16, Ico_U32, Ico_U64, Ico_F64,
          Ico_NaN64 /* 64-bit IEEE NaN. */
   }
   IRConstTag;

typedef
   struct _IRConst {
      IRConstTag tag;
      union {
         Bool   Bit;
         UChar  U8;
         UShort U16;
         UInt   U32;
         ULong  U64;
         Double F64;
      } Ico;
   }
   IRConst;

extern IRConst* IRConst_Bit   ( Bool );
extern IRConst* IRConst_U8    ( UChar );
extern IRConst* IRConst_U16   ( UShort );
extern IRConst* IRConst_U32   ( UInt );
extern IRConst* IRConst_U64   ( ULong );
extern IRConst* IRConst_F64   ( Double );
extern IRConst* IRConst_NaN64 ( void );

extern void ppIRConst ( IRConst* );


/* ------------------ Temporaries ------------------ */

typedef UInt IRTemp;

extern void ppIRTemp ( IRTemp );

#define INVALID_IRTEMP ((IRTemp)0xFFFFFFFF)


/* ------------------ Binary and unary ops ------------------ */

/* Encoding of rounding modes in Float -> Int conversions.  This is
   the same as the encoding used by Intel IA32 to indicate x87
   rounding mode. */
typedef
   enum { Irrm_NEAREST=0, Irrm_NegINF=1, Irrm_PosINF=2, Irrm_ZERO=3 }
   IRRoundingMode;

/* Floating point comparison result values, as created by Iop_CmpF64.
   This is also derived from what IA32 does. */
typedef
   enum {
      Ircr_UN = 0x85,
      Ircr_LT = 0x01,
      Ircr_GT = 0x00,
      Ircr_EQ = 0x80
   }
   IRCmpF64Result;


typedef
   enum { 
      /* Do not change this ordering.  The IR generators
         rely on (eg) Iop_Add64 == IopAdd8 + 3. */
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
      /* Widening multiplies */
      Iop_MullS8, Iop_MullS16, Iop_MullS32,
      Iop_MullU8, Iop_MullU16, Iop_MullU32,

      /* Wierdo integer stuff */
      Iop_Clz32,   /* count leading zeroes */
      Iop_Ctz32,   /* count trailing zeros */
      /* Ctz32/Clz32 are UNDEFINED when given arguments of zero.
         You must ensure they are never given a zero argument. 
      */

      /* Ordering not important after here. */
      Iop_CmpLT32S,
      Iop_CmpLE32S,
      Iop_CmpLT32U,
      Iop_CmpLE32U,
      /* Division */
      /* TODO: clarify semantics wrt rounding, negative values, whatever */
      Iop_DivModU64to32, // :: I64,I32 -> I64
                         // of which lo half is div and hi half is mod
      Iop_DivModS64to32, // ditto, signed
      /* Widening conversions */
      Iop_8Uto16, Iop_8Uto32, Iop_16Uto32,
      Iop_8Sto16, Iop_8Sto32, Iop_16Sto32, Iop_32Sto64,
      /* Narrowing conversions */
      Iop_32to8,
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
      /* 1-bit stuff */
      Iop_32to1,  /* :: Ity_I32 -> Ity_Bit, just select bit[0] */
      Iop_1Uto8,  /* :: Ity_Bit -> Ity_I8, unsigned widen */
      Iop_1Uto32, /* :: Ity_Bit -> Ity_I32, unsigned widen */

      /* ------ Floating point.  We try and be IEEE754 compliant. ------ */

      /* Binary operations */
      Iop_AddF64, Iop_SubF64, Iop_MulF64, Iop_DivF64, Iop_RemF64,
      Iop_AtanYXF64,

      /* Unary operations */
      Iop_NegF64, Iop_SqrtF64, Iop_SinF64, Iop_CosF64,

      /* Comparison, yielding GT/LT/EQ/UN(ordered), as per the following:
            0x85 Unordered
            0x01 LT
            0x00 GT
            0x80 EQ
         This just happens to be the Intel encoding.  The values
         are recorded in the type IRCmpF64Result.
      */
      Iop_CmpF64,

      /* int -> double */
      Iop_I32toF64, Iop_I64toF64,

      /* double -> int.  These take a first argument :: Ity_I32 
         (an IRRoundingMode) which is an indication of the rounding mode,
         as per the following encoding:
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
      */
      Iop_F64toI64, Iop_F64toI32, Iop_F64toI16,

      /* double <-> float.  What does this mean -- does it round? */
      Iop_F32toF64, Iop_F64toF32
   }
   IROp;


extern void ppIROp ( IROp );


/* ------------------ Expressions ------------------ */
/*
data Expr
   = GET   Int Type        -- offset, size
   | GETI  Expr Type Int Int -- offset, size, minoff, maxoff
   | TMP   Temp            -- value of temporary
   | BINOP Op Expr Expr    -- binary op
   | UNOP  Op Expr         -- unary op
   | LDle  Type Expr       -- load of the given type, Expr:: 32 or 64
   | CONST Const           -- 8/16/32/64-bit int constant

Re GETI.  It carries two ints, which give the lowest and highest
possible byte offsets that the GetI can possibly reference.
For example, if the type is Ity_I32, and the Expr may have
a value of M, M+4 or M+8, where M is a translation-time known
constant, then the low and high limits are M and M+11 respectively.

PUTI carries similar limit values.

These can be used by IR optimisers to establish aliasing/non-aliasing
between seperate GETI and PUTI terms, which could be used to do
reordering of them, or suchlike things.  Clearly it's critical to give
the correct limit values -- this is something that can't be
automatically checked (in general), and so the front-end writers must
be very careful to tell the truth, since not doing so could lead to
obscure IR optimisation bugs.
*/

typedef
     enum { Iex_Binder, /* Used only in pattern matching.  
                           Not an expression. */
          Iex_Get, Iex_GetI, Iex_Tmp, Iex_Binop, Iex_Unop, Iex_LDle, 
          Iex_Const, Iex_CCall, Iex_Mux0X }
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
            struct _IRExpr* offset;
            IRType  ty;
            UShort  minoff;
            UShort  maxoff;
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
            IRType ty;
            struct _IRExpr* addr;
         } LDle;
         struct {
            IRConst* con;
         } Const;
         struct {
            Char*  name;
            IRType retty;
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
extern IRExpr* IRExpr_GetI   ( IRExpr* off, IRType ty,  
                               UShort minoff, UShort maxoff );
extern IRExpr* IRExpr_Tmp    ( IRTemp tmp );
extern IRExpr* IRExpr_Binop  ( IROp op, IRExpr* arg1, IRExpr* arg2 );
extern IRExpr* IRExpr_Unop   ( IROp op, IRExpr* arg );
extern IRExpr* IRExpr_LDle   ( IRType ty, IRExpr* addr );
extern IRExpr* IRExpr_Const  ( IRConst* con );
extern IRExpr* IRExpr_CCall  ( Char* name, IRType retty, IRExpr** args );
extern IRExpr* IRExpr_Mux0X  ( IRExpr* cond, IRExpr* expr0, IRExpr* exprX );

extern void ppIRExpr ( IRExpr* );

/* CCall info.  The name is the C helper function; the backends
   will hand the name to the front ends to get the address of a 
   host-code helper function to be called.

   The args are a NULL-terminated array of arguments.  The stated
   return IRType, and the implied argument types, must match that
   of the function being called well enough so that the back end
   can actually generate correct code for the call.  (too vague)

   The called function must satisfy the following:

   * no side effects -- must be a pure function
   * it may not look at any of the guest state -- must depend
     purely on passed parameters
   * it may not access guest memory -- since that would
     hide guest memory transactions from the instrumenters
*/

/* ------------------ Statements ------------------ */
/*
data Stmt
   = PUT    Int Int Expr      -- offset, size, value
   | TMP    Temp Expr         -- store value in Temp
   | STle   Expr Expr         -- address (32 or 64 bit), value
   | Exit   Expr Const        -- conditional exit from middle of BB
                              -- Const is destination guest addr
*/
typedef 
   enum { Ist_Put, Ist_PutI, Ist_Tmp, Ist_STle, Ist_Exit } 
   IRStmtTag;

typedef
   struct _IRStmt {
      IRStmtTag tag;
      union {
         struct {
            Int     offset;
            IRExpr* expr;
         } Put;
         struct {
            IRExpr* offset;
            IRExpr* expr;
            UShort  minoff;
            UShort  maxoff;
         } PutI;
         struct {
            IRTemp  tmp;
            IRExpr* expr;
         } Tmp;
         struct {
            IRExpr* addr;
            IRExpr* data;
         } STle;
         struct {
            IRExpr*  cond;
            IRConst* dst;
         } Exit;
      } Ist;
   }
   IRStmt;

extern IRStmt* IRStmt_Put  ( Int off, IRExpr* value );
extern IRStmt* IRStmt_PutI ( IRExpr* off, IRExpr* value, 
                             UShort minoff, UShort maxoff );
extern IRStmt* IRStmt_Tmp  ( IRTemp tmp, IRExpr* expr );
extern IRStmt* IRStmt_STle ( IRExpr* addr, IRExpr* value );
extern IRStmt* IRStmt_Exit ( IRExpr* cond, IRConst* dst );

extern void ppIRStmt ( IRStmt* );

/* Guards in Put: if NULL, the Put is always done.
   If non-NULL, the expr must denote a value of Ity_Bit, and
   the Put is only done if this evaluates to 1.  The expression
   to be stored (expr) will be evaluated regardless of what
   the guard is.
*/

/* ------------------ Basic Blocks ------------------ */

/* This describes the unconditional jumps which implicitly happen at
   the end of each basic block.  Conditional jumps -- which can only
   be done with the IRStmt_Exit statement -- are implicitly of the
   Ijk_Boring kind. */

typedef
   enum { 
      Ijk_Boring=0x14000, /* not interesting; just goto next */
      Ijk_Call,           /* guest is doing a call */
      Ijk_Ret,            /* guest is doing a return */
      Ijk_ClientReq,      /* do guest client req before continuing */
      Ijk_Syscall,        /* do guest syscall before continuing */
      Ijk_Yield           /* client is yielding to thread scheduler */
   }
   IRJumpKind;

extern void ppIRJumpKind ( IRJumpKind );


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
extern void  addStmtToIRBB ( IRBB*, IRStmt* );

extern void ppIRBB ( IRBB* );


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* For messing with IR type environments */
extern IRTypeEnv* emptyIRTypeEnv  ( void );
extern IRTemp     newIRTemp       ( IRTypeEnv*, IRType );
extern IRType     lookupIRTypeEnv ( IRTypeEnv*, IRTemp );
extern IRTypeEnv* copyIRTypeEnv   ( IRTypeEnv* );

/* What is the type of this expression? */
extern IRType typeOfIRConst ( IRConst* );
extern IRType typeOfIRExpr  ( IRTypeEnv*, IRExpr* );

/* Sanity check a BB of IR */
extern void sanityCheckIRBB ( IRBB* bb, IRType guest_word_size );

/* Is this any value actually in the enumeration 'IRType' ? */
extern Bool isPlausibleType ( IRType ty );

#endif /* ndef __LIBVEX_IR_H */


/*---------------------------------------------------------------*/
/*---                                             libvex_ir.h ---*/
/*---------------------------------------------------------------*/
