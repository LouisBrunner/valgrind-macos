
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir_defs.h) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __IR_DEFS_H
#define __IR_DEFS_H


/*---------------------------------------------------------------*/
/*--- Type definitions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* ------------------ Types ------------------ */

typedef 
   enum { Ity_Bit, Ity_I8, Ity_I16, Ity_I32, Ity_I64 }
   IRType;

extern void ppIRType ( FILE* f, IRType );


/* ------------------ Constants ------------------ */

typedef
   enum { Ico_U8, Ico_U16, Ico_U32, Ico_U64 }
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

extern void ppIRConst ( FILE* f, IRConst* );


/* ------------------ Temporaries ------------------ */

typedef int IRTemp;

extern void ppIRTemp ( FILE* f, IRTemp );


/* ------------------ Binary and unary ops ------------------ */

typedef
   enum { Iop_Add32, 
          Iop_Sub32, 
          Iop_Mul32, 
          Iop_Or32, 
          Iop_And32,
          Iop_Xor32,
          Iop_Shl32,
          Iop_Shr32,
          Iop_Sar32,
          /* Tags for unary ops */
          Iop_Not32,
          Iop_Neg32
   }
   IROp;

extern void ppIROp ( FILE* f, IROp );


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
   enum { Iex_Get, Iex_Tmp, Iex_Binop, Iex_Unop, Iex_LDle, Iex_Const }
   IRExprTag;

typedef 
   struct _IRExpr {
      IRExprTag tag;
      union {
         struct {
            Int offset;
            Int size;
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
      } Iex;
   }
   IRExpr;

extern IRExpr* IRExpr_Get   ( Int off, Int sz );
extern IRExpr* IRExpr_Tmp   ( IRTemp tmp );
extern IRExpr* IRExpr_Binop ( IROp op, IRExpr* arg1, IRExpr* arg2 );
extern IRExpr* IRExpr_Unop  ( IROp op, IRExpr* arg );
extern IRExpr* IRExpr_LDle  ( IRType ty, IRExpr* addr );
extern IRExpr* IRExpr_Const ( IRConst* con );

extern void ppIRExpr ( FILE* f, IRExpr* );


/* ------------------ Statements ------------------ */
/*
data Stmt
   = PUT    Int Int Expr      -- offset, size, value
   | TMP    Temp Expr         -- store value in Temp
   | STle   Expr Expr         -- address (32 or 64 bit), value
*/
typedef 
   enum { Ist_Put, Ist_Tmp, Ist_STle } 
   IRStmtTag;

typedef
   struct _IRStmt {
      IRStmtTag tag;
      union {
         struct {
            Int     offset;
            Int     size;
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
      } Ist;
      struct _IRStmt* link;
   }
   IRStmt;

extern IRStmt* IRStmt_Put  ( Int off, Int sz, IRExpr* value );
extern IRStmt* IRStmt_Tmp  ( IRTemp tmp, IRExpr* expr );
extern IRStmt* IRStmt_STle ( IRExpr* addr, IRExpr* value );

extern void ppIRStmt ( FILE* f, IRStmt* );


/* ------------------ Basic block enders. ------------------ */
/*
   IRConst represents a guest address, which is either a
   32 or 64 bit integer, depending on the architecture we're simulating.

data Next
   = UJump    Const              -- unconditional jump
   | CJump01  Expr  Const Const  -- conditional jump, Expr::TY_Bit
   | IJump    Expr               -- jump to unknown address
*/
typedef
   enum { Inx_UJump, Inx_CJump01, Inx_IJump } 
   IRNextTag;

typedef
   struct {
      IRNextTag tag;
      union {
         struct {
            IRConst* dst;
         } UJump;
         struct {
            IRExpr*  cond;
            IRConst* dst0;
            IRConst* dst1;
         } CJump01;
         struct {
            IRExpr* dst;
         } IJump;
      } Inx;
   }
   IRNext;

extern IRNext* IRNext_UJump ( IRConst* dst );

extern void ppIRNext ( FILE* f, IRNext* );


/* ------------------ Basic Blocks ------------------ */

/* A bunch of statements, expressions, etc, are incomplete without an
   environment indicating the type of each IRTemp.  So this provides
   one. 
*/
typedef
   struct { 
      IRTemp name; 
      IRType type; 
   }
   IRTypeEnvMaplet;

typedef
   struct {
      IRTypeEnvMaplet* map;
      Int map_size;
      Int map_used;
   }
   IRTypeEnv;

extern void ppIRTypeEnv ( FILE* f, IRTypeEnv* );


/* Basic blocks contain 3 fields:
   - A table giving a type for each temp
   - A list of statements
   - A Next
*/
typedef
   struct _IRBB {
      IRTypeEnv* tyenv;
      IRStmt*    stmts;
      IRNext*    next;
   }
   IRBB;

extern IRBB* mk_IRBB ( IRTypeEnv*, IRStmt*, IRNext* );

extern void ppIRBB ( FILE* f, IRBB* );


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* For messing with IR type environments */
extern IRTypeEnv* newIRTypeEnv    ( void );
extern void       addToIRTypeEnv  ( IRTypeEnv*, IRTemp, IRType );
extern IRType     lookupIRTypeEnv ( IRTypeEnv*, IRTemp );

/* What is the type of this expression? */
extern IRType typeOfIRExpr ( IRTypeEnv*, IRExpr* );


#endif /* ndef __IR_DEFS_H */
