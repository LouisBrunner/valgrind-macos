
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (test_main.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>

#include "basictypes.h"
#include "ir_defs.h"





/*---------------------------------------------------------------*/
/*--- Test                                                    ---*/
/*---------------------------------------------------------------*/

/* HACK */
extern
void /* not really, but for the time being ... */
     iselBB ( IRBB* bb );

int main ( void )
{
   IRBB*      bb;
   IRTypeEnv* env = newIRTypeEnv();

   IRTemp t1 = 1;
   IRTemp t2 = 2;

   addToIRTypeEnv ( env, t1, Ity_I32 );
   addToIRTypeEnv ( env, t2, Ity_I32 );

   IRStmt* s1 = IRStmt_Put(8,4, IRExpr_Const(IRConst_U32(99)) );
   IRStmt* s2 = IRStmt_Put(7,4, IRExpr_Binop(Iop_Add32,
                                             IRExpr_Tmp(t1),
					     IRExpr_Const(IRConst_U32(55))));
   s1->link = s2;

   bb = mk_IRBB(env, s1, IRNext_UJump(IRConst_U32(-65565)));

   printf("bb is ...\n");
   ppIRBB(stdout, bb);
   printf("\n");

   iselBB(bb);

   return 0;
}
