
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (main.c) is                                   ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>

#include <stdlib.h>
#include <assert.h>

#include "storage.h"
#include "ir_defs.h"





/*---------------------------------------------------------------*/
/*--- Test                                                    ---*/
/*---------------------------------------------------------------*/

int main ( void )
{
   Cell bb;
   Cell stmts;

   storage_init();
   register_IR_tags();

   stmts = cons( mkSTPut(8,4, mkEXAtomI(mkWord32(99))),
 	   cons( mkSTPut(7,3, mkEXTmp(mkIRTemp(55))),
                 NIL ));

   bb = mkBB(NIL, stmts, mkUJump(mkWord32(65565)));
   ppIRBB(stdout, bb);

   storage_done();

   return 0;
}



