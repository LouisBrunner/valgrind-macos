
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
#include "host_regs.h"
#include "x86h_defs.h"


/*---------------------------------------------------------------*/
/*--- Test                                                    ---*/
/*---------------------------------------------------------------*/

/* HACK */
extern
HInstrArray* /* not really, but for the time being ... */
             iselBB ( IRBB* bb );


int main ( void )
{
   HInstrArray* vcode;
   IRBB*        bb;
   IRTypeEnv*   env = newIRTypeEnv();

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

   vcode = iselBB(bb);
   {
     HInstrArray* rcode;
     HReg rregs_to_use[4];
     rregs_to_use[0] = hregX86_EAX();
     rregs_to_use[1] = hregX86_EBX();
     rregs_to_use[2] = hregX86_ECX();
     rregs_to_use[3] = hregX86_EDX();

     rcode =
     doRegisterAllocation(vcode, 3, /* vregs */
                          rregs_to_use, 4, /* rregs */
			  NULL, /* ismove */
			  getRegUsage_X86Instr,
			  mapRegs_X86Instr,
			  NULL, /* genspill */
			  NULL /* genreload */
			  );


   }
   return 0;
}
