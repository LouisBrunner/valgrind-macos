
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir_defs.h) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __IR_DEFS_H
#define __IR_DEFS_H

//#include "storage.h"
#include <stdio.h>


/*---------------------------------------------------------------*/
/*--- Type definitions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* Constants */

typedef
   enum { Ico_u8, Ico_u16, Ico_u32, Ico_u64 }
   IRConstTag;

typedef
   struct {
      IRConstTag tag;
      union {
         UChar  u8;
         UShort u16;
         UInt   u32;
         ULong  u64;
      } Ico;
   }
   IRConst;


/* Temporaries */

typedef int IRTemp;


/* Types */

typedef 
   enum { Ity_Bit, Ity_I8, Ity_I16, Ity_I32, Ity_I64 }
   IRType;


/* Binary and unary ops */

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
          // Tags for unary ops
          Iop_Not32,
          Iop_Neg32
   }
   IROp;


/* Statements:

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
            struct _IRExpr* expr;
         } Put;
         struct {
            IRTemp  tmp;
            struct _IRExpr* expr;
         } Tmp;
         struct {
            struct _IRExpr* addr;
            struct _IRExpr* data;
         } STle;
      } Ist;
      struct _IRStmt* link;
   }
   IRStmt;


/* Basic block enders.
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
   struct _IRNext {
      IRNextTag tag;
      union {
         struct {
            IRConst dst;
         } UJump;
         struct {
            struct _IRExpr* cond;
            IRConst dst0;
            IRConst dst1;
         } CJump01;
         struct {
            struct _IRExpr* dst;
         } IJump;
      } Inx;
   }
   IRNext;


/* Expressions

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
            struct _IrExpr* arg;
         } Unop;
         struct {
            IRType ty;
            struct _IRExpr* addr;
         } LDle;
         struct {
            IRConst con;
         } Const;
      } Iex;
   }
   IRExpr;


/* Basic blocks contain 3 fields:
   - An association list, giving a type for each temp
   - A list of statements
   - A Next
*/
#define IR_BB      190    /* arity = 3 */

typedef
   struct _IRBB {
      struct _IRStmt* stmts;
      struct _IRNext* next;
   }
   IRBB;


/*---------------------------------------------------------------*/
/*--- Basic functions for the IR                              ---*/
/*---------------------------------------------------------------*/

/* Printers ... */
extern void ppIRBB      ( FILE* f, IRBB*      );
extern void ppIRNext    ( FILE* f, IRNext*    );
extern void ppIRStmt    ( FILE* f, IRStmt*    );
extern void ppIRExpr    ( FILE* f, IRExpr*    );
extern void ppIRTemp    ( FILE* f, IRTemp*    );
extern void ppIRType    ( FILE* f, IRType*    );
extern void ppIRConst   ( FILE* f, IRConst*   );
extern void ppIROp      ( FILE* f, IROp       );

/* Constructors -- IRBB */
extern IRBB* mkBB ( IRStmt* stmts, IRNext* next );

/* Constructors -- IRNext */
extern IRNext* mkUJump ( IRConst* dst );

/* Constructors -- IRStmt */
extern IRStmt* mkStPut  ( Int off, Int sz, IRExpr* value );
extern IRStmt* mkStTmp  ( IRTemp tmp, IRExpr* expr );
extern IRStmt* mkStSTle ( IRExpr* addr, IRExpr* value );

/* Constructors -- IRExpr */
extern IRExpr mkExGet   ( Int off, Int sz );
extern IRExpr mkExTmp   ( IRTemp tmp );
extern IRExpr mkExBinop ( IROp op, IRExpr* arg1, IRExpr* arg2 );
extern IRExpr mkExUnop  ( IROp op, IRExpr* arg );
extern IRExpr mkExLDle  ( IRType ty, IRExpr* addr );
extern IRExpr mkExConst ( IRConst* con );

/* Constructors -- IRTemp */
extern IRTemp mkIRTemp  ( Int n );

/* Constructors -- IRType */
/* Constructors -- IRAtomI */
/* Constructors -- IROp */


#endif /* ndef __IR_DEFS_H */
