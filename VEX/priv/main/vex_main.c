
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (main/vex_main.c) is                          ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004 OpenWorks, LLP.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; Version 2 dated June 1991 of the
   license.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, or liability
   for damages.  See the GNU General Public License for more details.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.
*/

#include "libvex.h"
#include "libvex_emwarn.h"
#include "libvex_guest_x86.h"
#include "libvex_guest_arm.h"

#include "main/vex_globals.h"
#include "main/vex_util.h"
#include "host-generic/h_generic_regs.h"
#include "ir/iropt.h"

#include "host-x86/hdefs.h"

#include "guest-x86/gdefs.h"
#include "guest-arm/gdefs.h"


/* This file contains the top level interface to the library. */

/* --------- Initialise the library. --------- */

/* Exported to library client. */

void LibVEX_default_VexControl ( /*OUT*/ VexControl* vcon )
{
   vcon->iropt_verbosity            = 0;
   vcon->iropt_level                = 2;
   vcon->iropt_precise_memory_exns  = False;
   vcon->iropt_unroll_thresh        = 120;
   vcon->guest_max_insns            = 50;
   vcon->guest_chase_thresh         = 10;
}


/* Exported to library client. */

void LibVEX_Init (
   /* failure exit function */
   __attribute__ ((noreturn))
   void (*failure_exit) ( void ),
   /* logging output function */
   void (*log_bytes) ( Char*, Int nbytes ),
   /* debug paranoia level */
   Int debuglevel,
   /* Are we supporting valgrind checking? */
   Bool valgrind_support,
   /* Control ... */
   /*READONLY*/VexControl* vcon
)
{
   /* First off, do enough minimal setup so that the following
      assertions can fail in a sane fashion, if need be. */
   vex_failure_exit = failure_exit;
   vex_log_bytes    = log_bytes;

   /* Now it's safe to check parameters for sanity. */
   vassert(!vex_initdone);
   vassert(failure_exit);
   vassert(log_bytes);
   vassert(debuglevel >= 0);

   vassert(vcon->iropt_verbosity >= 0);
   vassert(vcon->iropt_level >= 0);
   vassert(vcon->iropt_level <= 2);
   vassert(vcon->iropt_unroll_thresh >= 0);
   vassert(vcon->iropt_unroll_thresh <= 400);
   vassert(vcon->guest_max_insns >= 1);
   vassert(vcon->guest_max_insns <= 100);
   vassert(vcon->guest_chase_thresh >= 0);
   vassert(vcon->guest_chase_thresh < vcon->guest_max_insns);

   /* All the guest state structs must have an 8-aligned size. */
   vassert(0 == sizeof(VexGuestX86State) % 8);

   /* Check that Vex has been built with sizes of basic types as
      stated in priv/libvex_basictypes.h.  Failure of any of these is
      a serious configuration error and should be corrected
      immediately.  If any of these assertions fail you can fully
      expect Vex not to work properly, if at all. */

   vassert(1 == sizeof(UChar));
   vassert(1 == sizeof(Char));
   vassert(2 == sizeof(UShort));
   vassert(2 == sizeof(Short));
   vassert(4 == sizeof(UInt));
   vassert(4 == sizeof(Int));
   vassert(8 == sizeof(ULong));
   vassert(8 == sizeof(Long));
   vassert(4 == sizeof(Float));
   vassert(8 == sizeof(Double));
   vassert(1 == sizeof(Bool));
   vassert(4 == sizeof(Addr32));
   vassert(8 == sizeof(Addr64));

   vassert(sizeof(void*) == 4 || sizeof(void*) == 8);
   vassert(sizeof(void*) == sizeof(int*));
   vassert(sizeof(void*) == sizeof(HWord));

   /* Really start up .. */
   vex_debuglevel         = debuglevel;
   vex_valgrind_support   = valgrind_support;
   vex_control            = *vcon;
   vex_initdone           = True;
   LibVEX_SetAllocMode ( AllocModeTEMPORARY );
}


/* --------- Make a translation. --------- */

/* Exported to library client. */

TranslateResult LibVEX_Translate (
   /* The instruction sets we are translating from and to. */
   InsnSet iset_guest,
   InsnSet iset_host,
   /* IN: the block to translate, and its guest address. */
   UChar* guest_bytes,
   Addr64 guest_bytes_addr,
   Bool   (*chase_into_ok) ( Addr64 ),
   /* OUT: the number of bytes actually read */
   Int* guest_bytes_read,
   /* IN: a place to put the resulting code, and its size */
   UChar* host_bytes,
   Int    host_bytes_size,
   /* OUT: how much of the output area is used. */
   Int* host_bytes_used,
   /* IN: optionally, two instrumentation functions. */
   IRBB* (*instrument1) ( IRBB*, VexGuestLayout*, IRType hWordTy ),
   IRBB* (*instrument2) ( IRBB*, VexGuestLayout*, IRType hWordTy ),
   Bool  cleanup_after_instrumentation,
   /* IN: optionally, an access check function for guest code. */
   Bool (*byte_accessible) ( Addr64 ),
   /* IN: debug: trace vex activity at various points */
   Int  traceflags
)
{
   /* This the bundle of functions we need to do the back-end stuff
      (insn selection, reg-alloc, assembly) whilst being insulated
      from the target instruction set. */
   HReg* available_real_regs;
   Int   n_available_real_regs;
   Bool         (*isMove)      (HInstr*, HReg*, HReg*);
   void         (*getRegUsage) (HRegUsage*, HInstr*);
   void         (*mapRegs)     (HRegRemap*, HInstr*);
   HInstr*      (*genSpill)    ( HReg, Int );
   HInstr*      (*genReload)   ( HReg, Int );
   void         (*ppInstr)     ( HInstr* );
   void         (*ppReg)       ( HReg );
   HInstrArray* (*iselBB)      ( IRBB* );
   IRBB*        (*bbToIR)      ( UChar*, Addr64, Int*, 
                                 Bool(*)(Addr64), 
                                 Bool(*)(Addr64), Bool );
   Int          (*emit)        ( UChar*, Int, HInstr* );
   IRExpr*      (*specHelper)  ( Char*, IRExpr** );
   Bool         (*preciseMemExnsFn) ( Int, Int );

   VexGuestLayout* guest_layout;
   Bool            host_is_bigendian = False;
   IRBB*           irbb;
   HInstrArray*    vcode;
   HInstrArray*    rcode;
   Int             i, j, k, out_used, guest_sizeB;
   UChar           insn_bytes[32];
   IRType          guest_word_type;
   IRType          host_word_type;

   guest_layout           = NULL;
   available_real_regs    = NULL;
   n_available_real_regs  = 0;
   isMove                 = NULL;
   getRegUsage            = NULL;
   mapRegs                = NULL;
   genSpill               = NULL;
   genReload              = NULL;
   ppInstr                = NULL;
   ppReg                  = NULL;
   iselBB                 = NULL;
   bbToIR                 = NULL;
   emit                   = NULL;
   specHelper             = NULL;
   preciseMemExnsFn       = NULL;
   guest_word_type        = Ity_INVALID;
   host_word_type         = Ity_INVALID;

   vex_traceflags = traceflags;

   vassert(vex_initdone);
   LibVEX_ClearTemporary(False);


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
         ppInstr     = (void(*)(HInstr*)) ppX86Instr;
         ppReg       = (void(*)(HReg)) ppHRegX86;
         iselBB      = iselBB_X86;
         emit        = (Int(*)(UChar*,Int,HInstr*)) emit_X86Instr;
	 host_is_bigendian = False;
         host_word_type    = Ity_I32;
         break;

      default:
         vpanic("LibVEX_Translate: unsupported target insn set");
   }


   switch (iset_guest) {

      case InsnSetX86:
         preciseMemExnsFn = guest_x86_state_requires_precise_mem_exns;
         bbToIR           = bbToIR_X86;
         specHelper       = guest_x86_spechelper;
         guest_sizeB      = sizeof(VexGuestX86State);
         guest_word_type  = Ity_I32;
         guest_layout     = &x86guest_layout;
         break;

      case InsnSetARM:
         preciseMemExnsFn = guest_arm_state_requires_precise_mem_exns;
         bbToIR           = bbToIR_ARM;
         specHelper       = guest_arm_spechelper;
         guest_sizeB      = sizeof(VexGuestARMState);
         guest_word_type  = Ity_I32;
         guest_layout     = &armGuest_layout;
         break;

      default:
         vpanic("LibVEX_Translate: unsupported guest insn set");
   }


   if (vex_traceflags & VEX_TRACE_FE)
      vex_printf("\n------------------------" 
                   " Front end "
                   "------------------------\n\n");

   irbb = bbToIR ( guest_bytes, 
		   guest_bytes_addr,
		   guest_bytes_read,
		   byte_accessible,
                   chase_into_ok,
		   host_is_bigendian );

   if (irbb == NULL) {
      /* Access failure. */
      LibVEX_ClearTemporary(False);
      vex_traceflags = 0;
      return TransAccessFail;
   }

   /* If debugging, show the raw guest bytes for this bb. */
   if (vex_traceflags & VEX_TRACE_FE) {
      UChar* p = guest_bytes;
      vex_printf(". 0 %llx %d\n.", guest_bytes_addr, *guest_bytes_read );
      for (i = 0; i < *guest_bytes_read; i++)
         vex_printf(" %02x", (Int)p[i] );
      vex_printf("\n\n");
   }

   /* Sanity check the initial IR. */
   sanityCheckIRBB(irbb, guest_word_type);

   /* Clean it up, hopefully a lot. */
   irbb = do_iropt_BB ( irbb, specHelper, preciseMemExnsFn, 
                              guest_bytes_addr );
   sanityCheckIRBB(irbb, guest_word_type);

   if (vex_traceflags & VEX_TRACE_OPT1) {
      vex_printf("\n------------------------" 
                   " After pre-instr IR optimisation "
                   "------------------------\n\n");
      ppIRBB ( irbb );
      vex_printf("\n");
   }

   /* Get the thing instrumented. */
   if (instrument1)
      irbb = (*instrument1)(irbb, guest_layout, host_word_type);
   if (instrument2)
      irbb = (*instrument2)(irbb, guest_layout, host_word_type);
      
   if (vex_traceflags & VEX_TRACE_INST) {
      vex_printf("\n------------------------" 
                   " After instrumentation "
                   "------------------------\n\n");
      ppIRBB ( irbb );
      vex_printf("\n");
   }

   if (instrument1 || instrument2)
      sanityCheckIRBB(irbb, guest_word_type);

   /* Do a post-instrumentation cleanup pass. */
   if (cleanup_after_instrumentation) {
      do_deadcode_BB( irbb );
      irbb = cprop_BB( irbb );
      do_deadcode_BB( irbb );
      sanityCheckIRBB(irbb, guest_word_type);
   }

   if (vex_traceflags & VEX_TRACE_OPT2) {
      vex_printf("\n------------------------" 
                   " After post-instr IR optimisation "
                   "------------------------\n\n");
      ppIRBB ( irbb );
      vex_printf("\n");
   }

   /* Turn it into virtual-registerised code. */
   do_deadcode_BB( irbb );
   do_treebuild_BB( irbb );

   if (vex_traceflags & VEX_TRACE_TREES) {
      vex_printf("\n------------------------" 
                   "  After tree-building "
                   "------------------------\n\n");
      ppIRBB ( irbb );
      vex_printf("\n");
   }

   if (vex_traceflags & VEX_TRACE_VCODE)
      vex_printf("\n------------------------" 
                   " Instruction selection "
                   "------------------------\n");

   vcode = iselBB ( irbb );

   if (vex_traceflags & VEX_TRACE_VCODE)
      vex_printf("\n");

   if (vex_traceflags & VEX_TRACE_VCODE) {
      for (i = 0; i < vcode->arr_used; i++) {
         vex_printf("%3d   ", i);
         ppInstr(vcode->arr[i]);
         vex_printf("\n");
      }
      vex_printf("\n");
   }

   /* Register allocate. */
   rcode = doRegisterAllocation ( vcode, available_real_regs,
                  	       	  n_available_real_regs,
			          isMove, getRegUsage, mapRegs, 
			          genSpill, genReload, guest_sizeB,
				  ppInstr, ppReg );

   if (vex_traceflags & VEX_TRACE_RCODE) {
      vex_printf("\n------------------------" 
                   " Register-allocated code "
                   "------------------------\n\n");
      for (i = 0; i < rcode->arr_used; i++) {
         vex_printf("%3d   ", i);
         ppInstr(rcode->arr[i]);
         vex_printf("\n");
      }
      vex_printf("\n");
   }

   /* Assemble */
   if (vex_traceflags & VEX_TRACE_ASM) {
      vex_printf("\n------------------------" 
                   " Assembly "
                   "------------------------\n\n");
   }

   out_used = 0; /* tracks along the host_bytes array */
   for (i = 0; i < rcode->arr_used; i++) {
      if (vex_traceflags & VEX_TRACE_ASM) {
         ppInstr(rcode->arr[i]);
         vex_printf("\n");
      }
      j = (*emit)( insn_bytes, 32, rcode->arr[i] );
      if (vex_traceflags & VEX_TRACE_ASM) {
         for (k = 0; k < j; k++)
	    if (insn_bytes[k] < 16)
               vex_printf("0%x ",  (UInt)insn_bytes[k]);
            else
               vex_printf("%x ", (UInt)insn_bytes[k]);
         vex_printf("\n\n");
      }
      if (out_used + j > host_bytes_size) {
         LibVEX_ClearTemporary(False);
         vex_traceflags = 0;
         return TransOutputFull;
      }
      for (k = 0; k < j; k++) {
         host_bytes[out_used] = insn_bytes[k];
         out_used++;
      }
      vassert(out_used <= host_bytes_size);
   }
   *host_bytes_used = out_used;

   LibVEX_ClearTemporary(False);

   vex_traceflags = 0;
   return TransOK;
}


/* --------- Emulation warnings. --------- */

HChar* LibVEX_EmWarn_string ( VexEmWarn ew )
{
   switch (ew) {
     case EmWarn_NONE: 
        return "none";
     case EmWarn_X86_x87exns:
        return "Unmasking x87 FP exceptions";
     case EmWarn_X86_x87rounding:
        return "Selection of unsupported x87 FP rounding mode (+inf/-inf)";
     case EmWarn_X86_x87precision:
        return "Selection of non-80-bit x87 FP precision";
     case EmWarn_X86_sseExns:
        return "Unmasking SSE FP exceptionss";
     case EmWarn_X86_sseRounding:
        return "Selection of unsupported SSE FP rounding mode";
     default: 
        vpanic("LibVEX_EmWarn_string: unknown warning");
   }
}

/*---------------------------------------------------------------*/
/*--- end                                     main/vex_main.c ---*/
/*---------------------------------------------------------------*/
