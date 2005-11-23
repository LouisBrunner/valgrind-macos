
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (main/vex_main.c) is                          ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "libvex.h"
#include "libvex_emwarn.h"
#include "libvex_guest_x86.h"
#include "libvex_guest_amd64.h"
#include "libvex_guest_arm.h"
#include "libvex_guest_ppc32.h"

#include "main/vex_globals.h"
#include "main/vex_util.h"
#include "host-generic/h_generic_regs.h"
#include "ir/iropt.h"

#include "host-x86/hdefs.h"
#include "host-amd64/hdefs.h"
#include "host-ppc32/hdefs.h"

#include "guest-generic/bb_to_IR.h"
#include "guest-x86/gdefs.h"
#include "guest-amd64/gdefs.h"
#include "guest-arm/gdefs.h"
#include "guest-ppc32/gdefs.h"


/* This file contains the top level interface to the library. */

/* --------- Initialise the library. --------- */

/* Exported to library client. */

const HChar* LibVEX_Version ( void )
{
return
#include "main/vex_svnversion.h"
    ;
}


/* Exported to library client. */

void LibVEX_default_VexControl ( /*OUT*/ VexControl* vcon )
{
   vcon->iropt_verbosity            = 0;
   vcon->iropt_level                = 2;
   vcon->iropt_precise_memory_exns  = False;
   vcon->iropt_unroll_thresh        = 120;
   vcon->guest_max_insns            = 60;
   vcon->guest_chase_thresh         = 10;
}


/* Exported to library client. */

void LibVEX_Init (
   /* failure exit function */
   __attribute__ ((noreturn))
   void (*failure_exit) ( void ),
   /* logging output function */
   void (*log_bytes) ( HChar*, Int nbytes ),
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
   vassert(16 == sizeof(U128));

   vassert(sizeof(void*) == 4 || sizeof(void*) == 8);
   vassert(sizeof(void*) == sizeof(int*));
   vassert(sizeof(void*) == sizeof(HWord));

   vassert(VEX_HOST_WORDSIZE == sizeof(void*));
   vassert(VEX_HOST_WORDSIZE == sizeof(HWord));

   /* Really start up .. */
   vex_debuglevel         = debuglevel;
   vex_valgrind_support   = valgrind_support;
   vex_control            = *vcon;
   vex_initdone           = True;
   vexSetAllocMode ( VexAllocModeTEMP );
}


/* --------- Make a translation. --------- */

/* Exported to library client. */

VexTranslateResult LibVEX_Translate (
   /* The instruction sets we are translating from and to. */
   VexArch      arch_guest,
   VexArchInfo* archinfo_guest,
   VexArch      arch_host,
   VexArchInfo* archinfo_host,
   /* IN: the block to translate, and its guest address. */
   /* where are the actual bytes in the host's address space? */
   UChar*  guest_bytes,
   /* where do the bytes came from in the guest's aspace? */
   Addr64  guest_bytes_addr,
   /* what guest entry point address do they correspond to? */
   Addr64  guest_bytes_addr_noredir,
   /* Is it OK to chase into this guest address? */
   Bool    (*chase_into_ok) ( Addr64 ),
   /* OUT: which bits of guest code actually got translated */
   VexGuestExtents* guest_extents,
   /* IN: a place to put the resulting code, and its size */
   UChar*  host_bytes,
   Int     host_bytes_size,
   /* OUT: how much of the output area is used. */
   Int*    host_bytes_used,
   /* IN: optionally, two instrumentation functions. */
   IRBB*   (*instrument1) ( IRBB*, VexGuestLayout*, 
                            Addr64, VexGuestExtents*, 
                            IRType gWordTy, IRType hWordTy ),
   IRBB*   (*instrument2) ( IRBB*, VexGuestLayout*, 
                            Addr64, VexGuestExtents*,
                            IRType gWordTy, IRType hWordTy ),
   Bool    cleanup_after_instrumentation,
   /* IN: should this translation be self-checking? */
   Bool    do_self_check,
   /* IN: optionally, an access check function for guest code. */
   Bool    (*byte_accessible) ( Addr64 ),
   /* IN: debug: trace vex activity at various points */
   Int     traceflags
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
   HInstrArray* (*iselBB)      ( IRBB*, VexArchInfo* );
   Int          (*emit)        ( UChar*, Int, HInstr* );
   IRExpr*      (*specHelper)  ( HChar*, IRExpr** );
   Bool         (*preciseMemExnsFn) ( Int, Int );

   DisOneInstrFn disInstrFn;

   VexGuestLayout* guest_layout;
   Bool            host_is_bigendian = False;
   IRBB*           irbb;
   HInstrArray*    vcode;
   HInstrArray*    rcode;
   Int             i, j, k, out_used, guest_sizeB;
   Int             offB_TISTART, offB_TILEN;
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
   emit                   = NULL;
   specHelper             = NULL;
   preciseMemExnsFn       = NULL;
   disInstrFn             = NULL;
   guest_word_type        = Ity_INVALID;
   host_word_type         = Ity_INVALID;
   offB_TISTART           = 0;
   offB_TILEN             = 0;

   vex_traceflags = traceflags;

   vassert(vex_initdone);
   vexSetAllocModeTEMP_and_clear();
   vexAllocSanityCheck();

   /* First off, check that the guest and host insn sets
      are supported. */

   switch (arch_host) {

      case VexArchX86:
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
         vassert(archinfo_host->subarch == VexSubArchX86_sse0
                 || archinfo_host->subarch == VexSubArchX86_sse1
                 || archinfo_host->subarch == VexSubArchX86_sse2);
         break;

      case VexArchAMD64:
         getAllocableRegs_AMD64 ( &n_available_real_regs,
                                  &available_real_regs );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_AMD64Instr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*)) getRegUsage_AMD64Instr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*)) mapRegs_AMD64Instr;
         genSpill    = (HInstr*(*)(HReg,Int)) genSpill_AMD64;
         genReload   = (HInstr*(*)(HReg,Int)) genReload_AMD64;
         ppInstr     = (void(*)(HInstr*)) ppAMD64Instr;
         ppReg       = (void(*)(HReg)) ppHRegAMD64;
         iselBB      = iselBB_AMD64;
         emit        = (Int(*)(UChar*,Int,HInstr*)) emit_AMD64Instr;
         host_is_bigendian = False;
         host_word_type    = Ity_I64;
         vassert(archinfo_host->subarch == VexSubArch_NONE);
         break;

      case VexArchPPC32:
         getAllocableRegs_PPC32 ( &n_available_real_regs,
                                  &available_real_regs );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_PPC32Instr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*)) getRegUsage_PPC32Instr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*)) mapRegs_PPC32Instr;
         genSpill    = (HInstr*(*)(HReg,Int)) genSpill_PPC32;
         genReload   = (HInstr*(*)(HReg,Int)) genReload_PPC32;
         ppInstr     = (void(*)(HInstr*)) ppPPC32Instr;
         ppReg       = (void(*)(HReg)) ppHRegPPC32;
         iselBB      = iselBB_PPC32;
         emit        = (Int(*)(UChar*,Int,HInstr*)) emit_PPC32Instr;
         host_is_bigendian = True;
         host_word_type    = Ity_I32;
         vassert(archinfo_guest->subarch == VexSubArchPPC32_I
                 || archinfo_guest->subarch == VexSubArchPPC32_FI
                 || archinfo_guest->subarch == VexSubArchPPC32_VFI);
         break;

      default:
         vpanic("LibVEX_Translate: unsupported target insn set");
   }


   switch (arch_guest) {

      case VexArchX86:
         preciseMemExnsFn = guest_x86_state_requires_precise_mem_exns;
         disInstrFn       = disInstr_X86;
         specHelper       = guest_x86_spechelper;
         guest_sizeB      = sizeof(VexGuestX86State);
         guest_word_type  = Ity_I32;
         guest_layout     = &x86guest_layout;
         offB_TISTART     = offsetof(VexGuestX86State,guest_TISTART);
         offB_TILEN       = offsetof(VexGuestX86State,guest_TILEN);
         vassert(archinfo_guest->subarch == VexSubArchX86_sse0
                 || archinfo_guest->subarch == VexSubArchX86_sse1
                 || archinfo_guest->subarch == VexSubArchX86_sse2);
         vassert(0 == sizeof(VexGuestX86State) % 8);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_TISTART ) == 4);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_TILEN ) == 4);
         break;

      case VexArchAMD64:
         preciseMemExnsFn = guest_amd64_state_requires_precise_mem_exns;
         disInstrFn       = disInstr_AMD64;
         specHelper       = guest_amd64_spechelper;
         guest_sizeB      = sizeof(VexGuestAMD64State);
         guest_word_type  = Ity_I64;
         guest_layout     = &amd64guest_layout;
         offB_TISTART     = offsetof(VexGuestAMD64State,guest_TISTART);
         offB_TILEN       = offsetof(VexGuestAMD64State,guest_TILEN);
         vassert(archinfo_guest->subarch == VexSubArch_NONE);
         vassert(0 == sizeof(VexGuestAMD64State) % 8);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_TISTART ) == 8);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_TILEN ) == 8);
         break;

      case VexArchARM:
         preciseMemExnsFn = guest_arm_state_requires_precise_mem_exns;
         disInstrFn       = NULL; /* HACK */
         specHelper       = guest_arm_spechelper;
         guest_sizeB      = sizeof(VexGuestARMState);
         guest_word_type  = Ity_I32;
         guest_layout     = &armGuest_layout;
         offB_TISTART     = 0; /* hack ... arm has bitrot */
         offB_TILEN       = 0; /* hack ... arm has bitrot */
         vassert(archinfo_guest->subarch == VexSubArchARM_v4);
         break;

      case VexArchPPC32:
         preciseMemExnsFn = guest_ppc32_state_requires_precise_mem_exns;
         disInstrFn       = disInstr_PPC32;
         specHelper       = guest_ppc32_spechelper;
         guest_sizeB      = sizeof(VexGuestPPC32State);
         guest_word_type  = Ity_I32;
         guest_layout     = &ppc32Guest_layout;
         offB_TISTART     = offsetof(VexGuestPPC32State,guest_TISTART);
         offB_TILEN       = offsetof(VexGuestPPC32State,guest_TILEN);
         vassert(archinfo_guest->subarch == VexSubArchPPC32_I
                 || archinfo_guest->subarch == VexSubArchPPC32_FI
                 || archinfo_guest->subarch == VexSubArchPPC32_VFI);
         vassert(0 == sizeof(VexGuestPPC32State) % 8);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_TISTART ) == 4);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_TILEN ) == 4);
         break;

      default:
         vpanic("LibVEX_Translate: unsupported guest insn set");
   }

   /* yet more sanity checks ... */
   if (arch_guest == arch_host) {
      /* doesn't necessarily have to be true, but if it isn't it means
         we are simulating one flavour of an architecture a different
         flavour of the same architecture, which is pretty strange. */
      vassert(archinfo_guest->subarch == archinfo_host->subarch);
   }

   vexAllocSanityCheck();

   if (vex_traceflags & VEX_TRACE_FE)
      vex_printf("\n------------------------" 
                   " Front end "
                   "------------------------\n\n");

   irbb = bb_to_IR ( guest_extents,
                     disInstrFn,
                     guest_bytes, 
                     guest_bytes_addr,
                     chase_into_ok,
                     host_is_bigendian,
                     archinfo_guest,
                     guest_word_type,
                     do_self_check,
                     offB_TISTART,
                     offB_TILEN );

   vexAllocSanityCheck();

   if (irbb == NULL) {
      /* Access failure. */
      vexSetAllocModeTEMP_and_clear();
      vex_traceflags = 0;
      return VexTransAccessFail;
   }

   vassert(guest_extents->n_used >= 1 && guest_extents->n_used <= 3);
   vassert(guest_extents->base[0] == guest_bytes_addr);
   for (i = 0; i < guest_extents->n_used; i++) {
      vassert(guest_extents->len[i] < 10000); /* sanity */
   }

   /* If debugging, show the raw guest bytes for this bb. */
   if (0 || (vex_traceflags & VEX_TRACE_FE)) {
      if (guest_extents->n_used > 1) {
         vex_printf("can't show code due to extents > 1\n");
      } else {
         /* HACK */
         UChar* p = (UChar*)guest_bytes;
         UInt   guest_bytes_read = (UInt)guest_extents->len[0];
         vex_printf(". 0 %llx %u\n.", guest_bytes_addr, guest_bytes_read );
         for (i = 0; i < guest_bytes_read; i++)
         vex_printf(" %02x", (Int)p[i] );
         vex_printf("\n\n");
      }
   }

   /* Sanity check the initial IR. */
   sanityCheckIRBB( irbb, "initial IR", 
                    False/*can be non-flat*/, guest_word_type );

   vexAllocSanityCheck();

   /* Clean it up, hopefully a lot. */
   irbb = do_iropt_BB ( irbb, specHelper, preciseMemExnsFn, 
                              guest_bytes_addr );
   sanityCheckIRBB( irbb, "after initial iropt", 
                    True/*must be flat*/, guest_word_type );

   if (vex_traceflags & VEX_TRACE_OPT1) {
      vex_printf("\n------------------------" 
                   " After pre-instr IR optimisation "
                   "------------------------\n\n");
      ppIRBB ( irbb );
      vex_printf("\n");
   }

   vexAllocSanityCheck();

   /* Get the thing instrumented. */
   if (instrument1)
      irbb = (*instrument1)(irbb, guest_layout, 
                            guest_bytes_addr_noredir, guest_extents,
                            guest_word_type, host_word_type);
   vexAllocSanityCheck();

   if (instrument2)
      irbb = (*instrument2)(irbb, guest_layout,
                            guest_bytes_addr_noredir, guest_extents,
                            guest_word_type, host_word_type);
      
   if (vex_traceflags & VEX_TRACE_INST) {
      vex_printf("\n------------------------" 
                   " After instrumentation "
                   "------------------------\n\n");
      ppIRBB ( irbb );
      vex_printf("\n");
   }

   if (instrument1 || instrument2)
      sanityCheckIRBB( irbb, "after instrumentation",
                       True/*must be flat*/, guest_word_type );

   /* Do a post-instrumentation cleanup pass. */
   if (cleanup_after_instrumentation) {
      do_deadcode_BB( irbb );
      irbb = cprop_BB( irbb );
      do_deadcode_BB( irbb );
      sanityCheckIRBB( irbb, "after post-instrumentation cleanup",
                       True/*must be flat*/, guest_word_type );
   }

   vexAllocSanityCheck();

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

   vexAllocSanityCheck();

   if (vex_traceflags & VEX_TRACE_TREES) {
      vex_printf("\n------------------------" 
                   "  After tree-building "
                   "------------------------\n\n");
      ppIRBB ( irbb );
      vex_printf("\n");
   }

   /* HACK */
   if (0) { *host_bytes_used = 0; return VexTransOK; }
   /* end HACK */

   if (vex_traceflags & VEX_TRACE_VCODE)
      vex_printf("\n------------------------" 
                   " Instruction selection "
                   "------------------------\n");

   vcode = iselBB ( irbb, archinfo_host );

   vexAllocSanityCheck();

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

   vexAllocSanityCheck();

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

   /* HACK */
   if (0) { *host_bytes_used = 0; return VexTransOK; }
   /* end HACK */

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
         vexSetAllocModeTEMP_and_clear();
         vex_traceflags = 0;
         return VexTransOutputFull;
      }
      for (k = 0; k < j; k++) {
         host_bytes[out_used] = insn_bytes[k];
         out_used++;
      }
      vassert(out_used <= host_bytes_size);
   }
   *host_bytes_used = out_used;

   vexAllocSanityCheck();

   vexSetAllocModeTEMP_and_clear();

   vex_traceflags = 0;
   return VexTransOK;
}


/* --------- Emulation warnings. --------- */

HChar* LibVEX_EmWarn_string ( VexEmWarn ew )
{
   switch (ew) {
     case EmWarn_NONE: 
        return "none";
     case EmWarn_X86_x87exns:
        return "Unmasking x87 FP exceptions";
     case EmWarn_X86_x87precision:
        return "Selection of non-80-bit x87 FP precision";
     case EmWarn_X86_sseExns:
        return "Unmasking SSE FP exceptions";
     case EmWarn_X86_fz:
        return "Setting %mxcsr.fz (SSE flush-underflows-to-zero mode)";
     case EmWarn_X86_daz:
        return "Setting %mxcsr.daz (SSE treat-denormals-as-zero mode)";
     case EmWarn_X86_acFlag:
        return "Setting %eflags.ac (setting noted but ignored)";
     case EmWarn_PPC32exns:
        return "Unmasking PPC32 FP exceptions";
     default: 
        vpanic("LibVEX_EmWarn_string: unknown warning");
   }
}

/* --------- Arch/Subarch stuff. --------- */

const HChar* LibVEX_ppVexArch ( VexArch arch )
{
   switch (arch) {
      case VexArch_INVALID: return "INVALID";
      case VexArchX86:      return "X86";
      case VexArchAMD64:    return "AMD64";
      case VexArchARM:      return "ARM";
      case VexArchPPC32:    return "PPC32";
      default:              return "VexArch???";
   }
}

const HChar* LibVEX_ppVexSubArch ( VexSubArch subarch )
{
   switch (subarch) {
      case VexSubArch_INVALID:   return "INVALID";
      case VexSubArch_NONE:      return "NONE";
      case VexSubArchX86_sse0:   return "x86-sse0";
      case VexSubArchX86_sse1:   return "x86-sse1";
      case VexSubArchX86_sse2:   return "x86-sse2";
      case VexSubArchARM_v4:     return "arm-v4";
      case VexSubArchPPC32_I:    return "ppc32-int-only";
      case VexSubArchPPC32_FI:   return "ppc32-int-and-fp";
      case VexSubArchPPC32_VFI:  return "ppc32-int-fp-and-AV";
      default:                   return "VexSubArch???";
   }
}

/* Write default settings info *vai. */
void LibVEX_default_VexArchInfo ( /*OUT*/VexArchInfo* vai )
{
   vai->subarch              = VexSubArch_INVALID;
   vai->ppc32_cache_line_szB = 0;
}


/*---------------------------------------------------------------*/
/*--- end                                     main/vex_main.c ---*/
/*---------------------------------------------------------------*/
