
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir_defs.h) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __IR_DEFS_H
#define __IR_DEFS_H

#include "storage.h"
#include <stdio.h>


/*---------------------------------------------------------------*/
/*--- Storage tags for the IR                                 ---*/
/*---------------------------------------------------------------*/

// Temporaries

#define IR_TMP    80    /* :: Int32# */


// Tags for types

#define TY_Bit   100
#define TY_I8    101
#define TY_I16   102
#define TY_I32   103
#define TY_I64   104


// Tags for binary ops

#define OP_Add32  120
#define OP_Sub32  121
#define OP_Mul32  122
#define OP_Or32   123
#define OP_And32  124
#define OP_Xor32  125
#define OP_Shl32  126
#define OP_Shr32  127
#define OP_Sar32  128


// Tags for unary ops

#define OP_Not32  150
#define OP_Neg32  151


/* Statements:

data Stmt
   = PUT    Int Int Expr      -- offset, size, value
   | TMP    Temp Expr         -- store value in Temp
   | STle   Expr Expr         -- address (32 or 64 bit), value
*/
#define ST_Put     160
#define ST_Tmp     161
#define ST_STle    162


/* Basic block enders.
   HAddr is intended to represent a guest address, which is either a
   32 or 64 bit integer, depending on the architecture we're simulating.

data Next
   = UJump    HAddr              -- unconditional jump
   | CJump01  Expr HAddr HAddr   -- conditional jump, Expr::TY_Bit
   | IJump    Expr               -- jump to unknown address
*/
#define NX_UJump   170
#define NX_CJump01 171
#define NX_IJump   172


/* Expressions

data Expr
   = GET   Int Int         -- offset, size
   | TMP   Temp            -- value of temporary
   | BINOP Op Expr Expr    -- binary op
   | UNOP  Op Expr         -- unary op
   | LDle  Type Expr       -- load of the given type, Expr:: 32 or 64
   | ATOMI Atom            -- 8/16/32/64-bit int constant
*/
#define EX_Get     180
#define EX_Tmp     181
#define EX_Binop   182
#define EX_Unop    183
#define EX_LDle    184
#define EX_AtomI   185


/* Basic blocks contain 3 fields:
   - An association list, giving a type for each temp
   - A list of statements
   - A Next
*/
#define IR_BB      190    /* arity = 3 */


/*---------------------------------------------------------------*/
/*--- Types for the IR                                        ---*/
/*---------------------------------------------------------------*/

typedef  Cell  IRBB;
typedef  Cell  IRStmt;
typedef  Cell  IRExpr;
typedef  Cell  IRNext;
typedef  Cell  IRTemp;
typedef  Cell  IRType;
typedef  Cell  IROp;
typedef  Cell  IRAtomI;
typedef  Cell  GuestAddr;  /* Word32 or Word64 */


/*---------------------------------------------------------------*/
/*--- Basic functions for the IR                              ---*/
/*---------------------------------------------------------------*/

/* Call this before using any of the IR tags or constructors. */
extern void register_IR_tags ( void );

/* Printers ... */
extern void ppIRBB      ( FILE* f, IRBB      );
extern void ppIRNext    ( FILE* f, IRNext    );
extern void ppIRStmt    ( FILE* f, IRStmt    );
extern void ppIRExpr    ( FILE* f, IRExpr    );
extern void ppIRTemp    ( FILE* f, IRTemp    );
extern void ppIRType    ( FILE* f, IRType    );
extern void ppIRAtomI   ( FILE* f, IRAtomI   );
extern void ppIROp      ( FILE* f, IROp      );
extern void ppGuestAddr ( FILE* f, GuestAddr );

/* Constructors -- IRBB */
extern IRBB mkBB ( AList tys, List stmts, IRNext next );

/* Constructors -- IRNext */
extern IRNext mkUJump  ( GuestAddr dst );

/* Constructors -- IRStmt */
extern IRStmt mkSTPut  ( Int off, Int sz, IRExpr value );
extern IRStmt mkSTTmp  ( IRTemp tmp, IRExpr expr );
extern IRStmt mkSTSTle ( IRExpr addr, IRExpr value );

/* Constructors -- IRExpr */
extern IRExpr mkEXGet   ( Int off, Int sz );
extern IRExpr mkEXTmp   ( IRTemp tmp );
extern IRExpr mkEXBinop ( IROp op, IRExpr arg1, IRExpr arg2 );
extern IRExpr mkEXUnop  ( IROp op, IRExpr arg );
extern IRExpr mkEXLDle  ( IRType ty, IRExpr addr );
extern IRExpr mkEXAtomI ( IRAtomI atom );

/* Constructors -- IRTemp */
extern IRTemp mkIRTemp  ( Int n );

/* Constructors -- IRType */
/* Constructors -- IRAtomI */
/* Constructors -- IROp */


#endif /* ndef __IR_DEFS_H */
