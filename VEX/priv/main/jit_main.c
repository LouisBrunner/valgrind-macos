
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (jit_main.c) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libjit.h"

#include "jit_globals.h"
#include "vex_util.h"
#include "host_regs.h"
#include "x86h_defs.h"


/* This file contains the top level interface to the library. */

/* --------- Initialise the library. --------- */

/* Exported to library client. */

void LibJIT_Init (
   /* failure exit function */
   void (*failure_exit) ( void ),
   /* logging output function */
   void (*log_bytes) ( Char*, Int nbytes ),
   /* debug paranoia level */
   Int debuglevel,
   /* verbosity level */
   Int verbosity,
   /* Are we supporting valgrind checking? */
   Bool valgrind_support,
   /* Max # guest insns per bb */
   Int guest_insns_per_bb
)
{
   vassert(!vex_initdone);
   vassert(failure_exit);
   vex_failure_exit = failure_exit;
   vassert(log_bytes);
   vex_log_bytes = log_bytes;
   vassert(debuglevel >= 0);
   vex_debuglevel = debuglevel;
   vassert(verbosity >= 0);
   vex_verbosity = verbosity;
   vex_valgrind_support = valgrind_support;
   vassert(guest_insns_per_bb >= 1 && guest_insns_per_bb <= 100);
   vex_guest_insns_per_bb = guest_insns_per_bb;
   vex_initdone = True;
}


/* --------- Make a translation. --------- */

/* Exported to library client. */

TranslateResult LibJIT_Translate (
   /* The instruction sets we are translating from and to. */
   InsnSet iset_guest,
   InsnSet iset_host,
   /* IN: the block to translate, and its guest address. */
   Char*  guest_bytes,
   Addr64 guest_bytes_addr,
   /* OUT: the number of bytes actually read */
   Int* guest_bytes_read,
   /* IN: a place to put the resulting code, and its size */
   Char* host_bytes,
   Int   host_bytes_size,
   /* OUT: how much of the output area is used. */
   Int* host_bytes_used,
   /* IN: optionally, an instrumentation function. */
   IRBB* (*instrument) ( IRBB* ),
   /* IN: optionally, an access check function for guest code. */
   Bool (*byte_accessible) ( Addr64 )
)
{
   /* Stuff we need to know for reg-alloc. */
   HReg* available_real_regs;
   Int   n_available_real_regs;
   Bool (*isMove) (HInstr*, HReg*, HReg*);
   void (*getRegUsage) (HRegUsage*, HInstr*);
   void (*mapRegs) (HRegRemap*, HInstr*);
   HInstr* (*genSpill) ( HReg, Int );
   HInstr* (*genReload) ( HReg, Int );
   HInstrArray* (*iselBB) ( IRBB* );
   IRBB* (*bbToIR) ( Char*, Addr64, Int*, Bool(*)(Addr64) );

   IRBB*        irbb;
   HInstrArray* vcode;
   HInstrArray* rcode;

   vassert(vex_initdone);
   LibJIT_Clear(False);

   /* First off, check that the guest and host insn sets
      are supported. */
   switch (iset_host) {
      case InsnSetX86:
         getAllocableRegs_X86 ( &n_available_real_regs,
                                &available_real_regs );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_X86Instr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*)) getRegUsage_X86Instr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*)) mapRegs_X86Instr;
         genSpill    = (HInstr*(*)(HReg,Int)) genSpill_X86;
         genReload   = (HInstr*(*)(HReg,Int)) genReload_X86;
         iselBB      = iselBB_X86;
         break;
      default:
         vpanic("LibJIT_Translate: unsupported target insn set");
   }

   switch (iset_guest) {
      case InsnSetX86:
         bbToIR = NULL; //bbToIR_X86Instr;
         break;
      default:
         vpanic("LibJIT_Translate: unsupported guest insn set");
   }

   irbb = bbToIR ( guest_bytes, 
		   guest_bytes_addr,
		   guest_bytes_read,
		   byte_accessible );

   if (irbb == NULL) {
      /* Access failure. */
      LibJIT_Clear(False);
      return TransAccessFail;
   }

   /* Get the thing instrumented. */
   if (instrument)
      irbb = (*instrument)(irbb);

   /* Turn it into virtual-registerised code. */
   vcode = iselBB ( irbb );

   /* Register allocate. */
   rcode = doRegisterAllocation ( vcode, available_real_regs,
                  	       	  n_available_real_regs,
			          isMove, getRegUsage, mapRegs, 
			          genSpill, genReload );

   /* Assemble, etc. */
   LibJIT_Clear(True);

   return TransOK;
}



/*---------------------------------------------------------------*/
/*--- end                                          jit_main.c ---*/
/*---------------------------------------------------------------*/
