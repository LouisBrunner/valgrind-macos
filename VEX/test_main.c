
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (test_main.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>



/*---------------------------------------------------------------*/
/*--- Test                                                    ---*/
/*---------------------------------------------------------------*/

#include "libjit_basictypes.h"
#include "libjit.h"

void failure_exit ( void )
{
   fprintf(stdout, "VEX did failure_exit.  Bye.\n");
   exit(1);
}

void log_bytes ( Char* bytes, Int nbytes )
{
   fwrite ( bytes, 1, nbytes, stdout );
}

int main ( void )
{
   Int* p;
   Int i, j, n = 0;
   LibJIT_Init ( &failure_exit, &log_bytes, 
                 1, 1, False, 10 );
   for (j = 0; j < 5000; j++) {
      LibJIT_Clear(False);
      for (i = 0; i < 2000; i++) {
         n++;
         p = LibVEX_Alloc(16);
         p[0] = p[1] = p[2] = p[3] = 44;
      }
   }
   LibJIT_Clear(True);
   printf("Did %d allocs\n", n);
   return 0;
}

/*---------------------------------------------------------------*/
/*--- Test (old)                                              ---*/
/*---------------------------------------------------------------*/

#if 0

#include "libjit_basictypes.h"
#include "ir_defs.h"
#include "host_regs.h"
#include "x86h_defs.h"


/* HACK */
extern
HInstrArray* /* not really, but for the time being ... */
             iselBB_X86Instr ( IRBB* bb );


int main ( void )
{
   HInstrArray* vcode;
   IRBB*        bb;
   IRTypeEnv*   env = newIRTypeEnv();

   IRTemp t0 = 0;
   IRTemp t1 = 1;
   IRTemp t2 = 2;

   addToIRTypeEnv ( env, t0, Ity_I32 );
   addToIRTypeEnv ( env, t1, Ity_I32 );
   addToIRTypeEnv ( env, t2, Ity_I32 );

   IRStmt* s10 = IRStmt_Tmp(t0, IRExpr_Const(IRConst_U32(0x2000)));
   IRStmt* s11 = IRStmt_Tmp(t1, IRExpr_Const(IRConst_U32(0x2001)));
   IRStmt* s12 = IRStmt_Tmp(t2, IRExpr_Const(IRConst_U32(0x2002)));

   IRStmt* s1 = IRStmt_Put(8,4, IRExpr_Const(IRConst_U32(99)) );
#if 0
   IRStmt* s2 = IRStmt_Put(7,4, IRExpr_Binop(Iop_Add32,
                                             IRExpr_Tmp(t1),
					     IRExpr_Const(IRConst_U32(55))));
#endif

   IRStmt* s2 = IRStmt_Put(9,4,
			   IRExpr_Binop(Iop_Shl32,
					IRExpr_Tmp(t0),
					IRExpr_Binop(Iop_Add32,
						     IRExpr_Tmp(t1), 
						     IRExpr_Tmp(t2))));


   s10->link = s11;
   s11->link = s12;
   s12->link = s1;
   s1->link = s2;

   bb = mk_IRBB(env, s10, IRNext_UJump(IRConst_U32(-65565)));

   printf("bb is ...\n");
   ppIRBB(stdout, bb);
   printf("\n");

   if (0)
   vcode = iselBB_X86Instr(bb);
   else
   {
     Int i;
     HReg vr0 = mkHReg(0, HRcInt, True);
     HReg vr1 = mkHReg(1, HRcInt, True);
     HReg vr2 = mkHReg(2, HRcInt, True);
     HReg vr3 = mkHReg(3, HRcInt, True);
     HReg eax = hregX86_EAX();
     HReg ebx = hregX86_EBX();
     HReg ecx = hregX86_ECX();
     HReg edx = hregX86_EDX();
     HReg ebp = hregX86_EBP();
     vcode = newHInstrArray();
     vcode->n_vregs = 4;

     addHInstr(vcode, X86Instr_Alu32R(Xalu_MOV, 
				      X86RMI_Imm(0x10001), vr0));
     addHInstr(vcode, X86Instr_Alu32R(Xalu_MOV, 
				      X86RMI_Imm(0x10101), vr1));
     addHInstr(vcode, X86Instr_Alu32R(Xalu_MOV, 
				      X86RMI_Imm(0x10201), vr2));
     addHInstr(vcode, X86Instr_Alu32R(Xalu_MOV, 
				      X86RMI_Imm(0x10301), vr3));

     addHInstr(vcode, X86Instr_Alu32R(Xalu_MOV,
				      X86RMI_Imm(0x99999), eax));
     addHInstr(vcode, X86Instr_Alu32R(Xalu_MOV,
				      X86RMI_Imm(0x99999), edx));

     addHInstr(vcode, X86Instr_Alu32M(Xalu_MOV,
				      X86RI_Reg(vr0),
				      X86AMode_IR(0x100, ebp)));
     addHInstr(vcode, X86Instr_Alu32M(Xalu_MOV,
				      X86RI_Reg(vr1),
				      X86AMode_IR(0x101, ebp)));
     addHInstr(vcode, X86Instr_Alu32M(Xalu_MOV,
				      X86RI_Reg(vr2),
				      X86AMode_IR(0x101, ebp)));
     addHInstr(vcode, X86Instr_Alu32M(Xalu_MOV,
				      X86RI_Reg(vr3),
				      X86AMode_IR(0x101, ebp)));
   printf("\nBefore\n");
   for (i = 0; i < vcode->arr_used; i++) {
     ppX86Instr(stdout, vcode->arr[i]);
     printf("\n");
   }
   printf("\n");
   }

   {
     Int i;
     HInstrArray* rcode;
     HReg rregs_to_use[4];
     rregs_to_use[0] = hregX86_EAX();
     rregs_to_use[1] = hregX86_EBX();
     rregs_to_use[2] = hregX86_ECX();
     rregs_to_use[3] = hregX86_EDX();

     rcode =
     doRegisterAllocation(vcode,
                          rregs_to_use, 3, /* rregs */
			  isMove_X86Instr,
			  getRegUsage_X86Instr,
			  mapRegs_X86Instr,
			  genSpill_X86,
			  genReload_X86
			  );

   printf("\nAfter\n");
   for (i = 0; i < rcode->arr_used; i++) {
     ppX86Instr(stdout, rcode->arr[i]);
     printf("\n");
   }
   printf("\n");
   }

   return 0;
}
#endif
