
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
   enum { Ity_Bit=0x11000, 
          Ity_I8, Ity_I16, Ity_I32, Ity_I64 }
   IRType;

extern void ppIRType ( IRType );


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
      Iop_Add8=0x12000,  
                 Iop_Add16,  Iop_Add32,  Iop_Add64,
      Iop_Sub8,  Iop_Sub16,  Iop_Sub32,  Iop_Sub64,
      Iop_Adc8,  Iop_Adc16,  Iop_Adc32,  Iop_Adc64,
      Iop_Sbb8,  Iop_Sbb16,  Iop_Sbb32,  Iop_Sbb64,
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
      /* Specials */
      Iop_32to1 /* :: Ity_I32 -> Ity_Bit, just select bit[0] */
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
   enum { Iex_Get, Iex_Tmp, Iex_Binop, Iex_Unop, Iex_LDle, 
          Iex_Const, Iex_CCall }
   IRExprTag;

typedef 
   struct _IRExpr {
      IRExprTag tag;
      union {
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
      } Iex;
   }
   IRExpr;

extern IRExpr* IRExpr_Get   ( Int off, IRType ty );
extern IRExpr* IRExpr_Tmp   ( IRTemp tmp );
extern IRExpr* IRExpr_Binop ( IROp op, IRExpr* arg1, IRExpr* arg2 );
extern IRExpr* IRExpr_Unop  ( IROp op, IRExpr* arg );
extern IRExpr* IRExpr_LDle  ( IRType ty, IRExpr* addr );
extern IRExpr* IRExpr_Const ( IRConst* con );
extern IRExpr* IRExpr_CCall ( Char* name, IRType retty, IRExpr** args );

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
*/
typedef 
   enum { Ist_Put, Ist_Tmp, Ist_STle } 
   IRStmtTag;

typedef
   struct _IRStmt {
      IRStmtTag tag;
      union {
         struct {
            IRExpr* guard;
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
      } Ist;
      struct _IRStmt* link;
   }
   IRStmt;

extern IRStmt* IRStmt_Put  ( IRExpr* guard, Int off, IRExpr* value );
extern IRStmt* IRStmt_Tmp  ( IRTemp tmp, IRExpr* expr );
extern IRStmt* IRStmt_STle ( IRExpr* addr, IRExpr* value );

extern void ppIRStmt ( IRStmt* );

/* Guards in Put: if NULL, the Put is always done.
   If non-NULL, the expr must denote a value of Ity_Bit, and
   the Put is only done if this evaluates to 1.  The expression
   to be stored (expr) will be evaluated regardless of what
   the guard is.
*/

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

extern IRNext* IRNext_UJump   ( IRConst* dst );
extern IRNext* IRNext_CJump01 ( IRExpr* cond, IRConst* dst0, IRConst* dst1 );
extern IRNext* IRNext_IJump   ( IRExpr* dst );

extern void ppIRNext ( IRNext* );


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

extern void ppIRTypeEnv ( IRTypeEnv* );


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

extern IRBB* mkIRBB ( IRTypeEnv*, IRStmt*, IRNext* );

extern void ppIRBB ( IRBB* );


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR                             ---*/
/*---------------------------------------------------------------*/

/* For messing with IR type environments */
extern IRTypeEnv* newIRTypeEnv    ( void );
extern void       addToIRTypeEnv  ( IRTypeEnv*, IRTemp, IRType );
extern IRType     lookupIRTypeEnv ( IRTypeEnv*, IRTemp );

/* What is the type of this expression? */
extern IRType typeOfIRExpr ( IRTypeEnv*, IRExpr* );


#endif /* ndef __LIBVEX_IR_H */


/*---------------------------------------------------------------*/
/*---                                             libvex_ir.h ---*/
/*---------------------------------------------------------------*/
