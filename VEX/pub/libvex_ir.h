
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
          Ity_I8, Ity_I16, Ity_I32, Ity_I64 }
   IRType;

extern void ppIRType ( IRType );


/* ------------------ Constants ------------------ */

typedef
   enum { Ico_U8=0x12000, 
          Ico_U16, Ico_U32, Ico_U64 }
   IRConstTag;

typedef
   struct _IRConst {
      IRConstTag tag;
      union {
         UChar  U8;
         UShort U16;
         UInt   U32;
         ULong  U64;
      } Ico;
   }
   IRConst;

extern IRConst* IRConst_U8  ( UChar );
extern IRConst* IRConst_U16 ( UShort );
extern IRConst* IRConst_U32 ( UInt );
extern IRConst* IRConst_U64 ( ULong );

extern void ppIRConst ( IRConst* );


/* ------------------ Temporaries ------------------ */

typedef UInt IRTemp;

extern void ppIRTemp ( IRTemp );

#define INVALID_IRTEMP ((IRTemp)0xFFFFFFFF)


/* ------------------ Binary and unary ops ------------------ */

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
      /* Ordering not important after here. */
      /* Division */
      Iop_DivModU64to32, // :: I64,I32 -> I64
                         // of which lo half is div and hi half is mod
      Iop_DivModS64to32, // ditto, signed
      /* Widening conversions */
      Iop_8Uto16, Iop_8Uto32, Iop_16Uto32,
      Iop_8Sto16, Iop_8Sto32, Iop_16Sto32,
      /* Narrowing conversions */
      Iop_32to8,
      /* 16 <-> 32 bit conversions */
      Iop_32to16,     // :: I32 -> I16, low half
      Iop_32HIto16,   // :: I32 -> I16, high half
      Iop_16HLto32,   // :: (I16,I16) -> I32
      /* 32 <-> 64 bit conversions */
      Iop_64to32,     // :: I64 -> I32, low half
      Iop_64HIto32,   // :: I64 -> I32, high half
      Iop_32HLto64,   // :: (I32,I32) -> I64
      /* 1-bit stuff */
      Iop_32to1, /* :: Ity_I32 -> Ity_Bit, just select bit[0] */
      Iop_1Uto8  /* :: Ity_Bit -> Ity_I8, unsigned widen */
   }
   IROp;

extern void ppIROp ( IROp );


/* ------------------ Expressions ------------------ */
/*
data Expr
   = GET   Int Int         -- offset, size
   | TMP   Temp            -- value of temporary
   | BINOP Op Expr Expr    -- binary op
   | UNOP  Op Expr         -- unary op
   | LDle  Type Expr       -- load of the given type, Expr:: 32 or 64
   | CONST Const           -- 8/16/32/64-bit int constant
*/
typedef
     enum { Iex_Binder, /* Used only in pattern matching.  
                           Not an expression. */
          Iex_Get, Iex_Tmp, Iex_Binop, Iex_Unop, Iex_LDle, 
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
extern IRExpr* IRExpr_Tmp    ( IRTemp tmp );
extern IRExpr* IRExpr_Binop  ( IROp op, IRExpr* arg1, IRExpr* arg2 );
extern IRExpr* IRExpr_Unop   ( IROp op, IRExpr* arg );
extern IRExpr* IRExpr_LDle   ( IRType ty, IRExpr* addr );
extern IRExpr* IRExpr_Const  ( IRConst* con );
extern IRExpr* IRExpr_CCall  ( Char* name, IRType retty, IRExpr** args );
extern IRExpr* IRExpr_Mux0X  ( IRExpr* cond, IRExpr* expr0, IRExpr* exprX );

extern void ppIRExpr ( IRExpr* );

/* CCall info.  The name is the C helper function; the backends
   will look it up in a table of known helpers, to get the address.

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
   enum { Ist_Put, Ist_Tmp, Ist_STle, Ist_Exit } 
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
      struct _IRStmt* link;
   }
   IRStmt;

extern IRStmt* IRStmt_Put  ( Int off, IRExpr* value );
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


/* Basic blocks contain 4 fields:
   - A table giving a type for each temp
   - A list of statements
   - An expression of type 32 or 64 bits, depending on the
     guest's word size, indicating the next destination.
*/
typedef
   struct _IRBB {
      IRTypeEnv* tyenv;
      IRStmt*    stmts;
      IRExpr*    next;
      IRJumpKind jumpkind;
   }
   IRBB;

extern IRBB* mkIRBB ( IRTypeEnv*, IRStmt*, IRExpr*, IRJumpKind );

extern void ppIRBB ( IRBB* );


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* For messing with IR type environments */
extern IRTypeEnv* newIRTypeEnv    ( void );
extern IRTemp     newIRTemp       ( IRTypeEnv*, IRType );
extern IRType     lookupIRTypeEnv ( IRTypeEnv*, IRTemp );

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
