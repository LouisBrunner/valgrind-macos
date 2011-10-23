
/*---------------------------------------------------------------*/
/*--- begin                                       main_main.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2011 OpenWorks LLP
      info@open-works.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

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
#include "libvex_guest_ppc64.h"
#include "libvex_guest_s390x.h"

#include "main_globals.h"
#include "main_util.h"
#include "host_generic_regs.h"
#include "ir_opt.h"

#include "host_x86_defs.h"
#include "host_amd64_defs.h"
#include "host_ppc_defs.h"
#include "host_arm_defs.h"
#include "host_s390_defs.h"

#include "guest_generic_bb_to_IR.h"
#include "guest_x86_defs.h"
#include "guest_amd64_defs.h"
#include "guest_arm_defs.h"
#include "guest_ppc_defs.h"
#include "guest_s390_defs.h"

#include "host_generic_simd128.h"


/* This file contains the top level interface to the library. */

/* --------- fwds ... --------- */

static Bool   are_valid_hwcaps ( VexArch arch, UInt hwcaps );
static HChar* show_hwcaps ( VexArch arch, UInt hwcaps );


/* --------- Initialise the library. --------- */

/* Exported to library client. */

void LibVEX_default_VexControl ( /*OUT*/ VexControl* vcon )
{
   vcon->iropt_verbosity            = 0;
   vcon->iropt_level                = 2;
   vcon->iropt_precise_memory_exns  = False;
   vcon->iropt_unroll_thresh        = 120;
   vcon->guest_max_insns            = 60;
   vcon->guest_chase_thresh         = 10;
   vcon->guest_chase_cond           = False;
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
   vassert(vcon->guest_chase_cond == True 
           || vcon->guest_chase_cond == False);

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
   vassert(16 == sizeof(V128));

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

VexTranslateResult LibVEX_Translate ( VexTranslateArgs* vta )
{
   /* This the bundle of functions we need to do the back-end stuff
      (insn selection, reg-alloc, assembly) whilst being insulated
      from the target instruction set. */
   HReg* available_real_regs;
   Int   n_available_real_regs;
   Bool         (*isMove)       ( HInstr*, HReg*, HReg* );
   void         (*getRegUsage)  ( HRegUsage*, HInstr*, Bool );
   void         (*mapRegs)      ( HRegRemap*, HInstr*, Bool );
   void         (*genSpill)     ( HInstr**, HInstr**, HReg, Int, Bool );
   void         (*genReload)    ( HInstr**, HInstr**, HReg, Int, Bool );
   HInstr*      (*directReload) ( HInstr*, HReg, Short );
   void         (*ppInstr)      ( HInstr*, Bool );
   void         (*ppReg)        ( HReg );
   HInstrArray* (*iselSB)       ( IRSB*, VexArch, VexArchInfo*, 
                                                  VexAbiInfo* );
   Int          (*emit)         ( UChar*, Int, HInstr*, Bool, void*, void* );
   IRExpr*      (*specHelper)   ( HChar*, IRExpr**, IRStmt**, Int );
   Bool         (*preciseMemExnsFn) ( Int, Int );

   DisOneInstrFn disInstrFn;

   VexGuestLayout* guest_layout;
   Bool            host_is_bigendian = False;
   IRSB*           irsb;
   HInstrArray*    vcode;
   HInstrArray*    rcode;
   Int             i, j, k, out_used, guest_sizeB;
   Int             offB_TISTART, offB_TILEN;
   UChar           insn_bytes[48];
   IRType          guest_word_type;
   IRType          host_word_type;
   Bool            mode64;

   guest_layout           = NULL;
   available_real_regs    = NULL;
   n_available_real_regs  = 0;
   isMove                 = NULL;
   getRegUsage            = NULL;
   mapRegs                = NULL;
   genSpill               = NULL;
   genReload              = NULL;
   directReload           = NULL;
   ppInstr                = NULL;
   ppReg                  = NULL;
   iselSB                 = NULL;
   emit                   = NULL;
   specHelper             = NULL;
   preciseMemExnsFn       = NULL;
   disInstrFn             = NULL;
   guest_word_type        = Ity_INVALID;
   host_word_type         = Ity_INVALID;
   offB_TISTART           = 0;
   offB_TILEN             = 0;
   mode64                 = False;

   vex_traceflags = vta->traceflags;

   vassert(vex_initdone);
   vassert(vta->needs_self_check != NULL);

   vexSetAllocModeTEMP_and_clear();
   vexAllocSanityCheck();

   /* First off, check that the guest and host insn sets
      are supported. */

   switch (vta->arch_host) {

      case VexArchX86:
         mode64       = False;
         getAllocableRegs_X86 ( &n_available_real_regs,
                                &available_real_regs );
         isMove       = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_X86Instr;
         getRegUsage  = (void(*)(HRegUsage*,HInstr*, Bool))
                        getRegUsage_X86Instr;
         mapRegs      = (void(*)(HRegRemap*,HInstr*, Bool)) mapRegs_X86Instr;
         genSpill     = (void(*)(HInstr**,HInstr**,HReg,Int,Bool))
                        genSpill_X86;
         genReload    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool))
                        genReload_X86;
         directReload = (HInstr*(*)(HInstr*,HReg,Short)) directReload_X86;
         ppInstr      = (void(*)(HInstr*, Bool)) ppX86Instr;
         ppReg        = (void(*)(HReg)) ppHRegX86;
         iselSB       = iselSB_X86;
         emit         = (Int(*)(UChar*,Int,HInstr*,Bool,void*,void*))
                        emit_X86Instr;
         host_is_bigendian = False;
         host_word_type    = Ity_I32;
         vassert(are_valid_hwcaps(VexArchX86, vta->archinfo_host.hwcaps));
         /* jump-to-dispatcher scheme */
         vassert(vta->dispatch_unassisted != NULL);
         vassert(vta->dispatch_assisted != NULL);
         break;

      case VexArchAMD64:
         mode64      = True;
         getAllocableRegs_AMD64 ( &n_available_real_regs,
                                  &available_real_regs );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_AMD64Instr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*, Bool))
                       getRegUsage_AMD64Instr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*, Bool)) mapRegs_AMD64Instr;
         genSpill    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool))
                       genSpill_AMD64;
         genReload   = (void(*)(HInstr**,HInstr**,HReg,Int,Bool))
                       genReload_AMD64;
         ppInstr     = (void(*)(HInstr*, Bool)) ppAMD64Instr;
         ppReg       = (void(*)(HReg)) ppHRegAMD64;
         iselSB      = iselSB_AMD64;
         emit        = (Int(*)(UChar*,Int,HInstr*,Bool,void*,void*))
                       emit_AMD64Instr;
         host_is_bigendian = False;
         host_word_type    = Ity_I64;
         vassert(are_valid_hwcaps(VexArchAMD64, vta->archinfo_host.hwcaps));
         /* jump-to-dispatcher scheme */
         vassert(vta->dispatch_unassisted != NULL);
         vassert(vta->dispatch_assisted != NULL);
         break;

      case VexArchPPC32:
         mode64      = False;
         getAllocableRegs_PPC ( &n_available_real_regs,
                                &available_real_regs, mode64 );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_PPCInstr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*,Bool)) getRegUsage_PPCInstr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*,Bool)) mapRegs_PPCInstr;
         genSpill    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genSpill_PPC;
         genReload   = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genReload_PPC;
         ppInstr     = (void(*)(HInstr*,Bool)) ppPPCInstr;
         ppReg       = (void(*)(HReg)) ppHRegPPC;
         iselSB      = iselSB_PPC;
         emit        = (Int(*)(UChar*,Int,HInstr*,Bool,void*,void*))
                       emit_PPCInstr;
         host_is_bigendian = True;
         host_word_type    = Ity_I32;
         vassert(are_valid_hwcaps(VexArchPPC32, vta->archinfo_host.hwcaps));
         /* return-to-dispatcher scheme */
         vassert(vta->dispatch_unassisted == NULL);
         vassert(vta->dispatch_assisted == NULL);
         break;

      case VexArchPPC64:
         mode64      = True;
         getAllocableRegs_PPC ( &n_available_real_regs,
                                &available_real_regs, mode64 );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_PPCInstr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*, Bool)) getRegUsage_PPCInstr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*, Bool)) mapRegs_PPCInstr;
         genSpill    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genSpill_PPC;
         genReload   = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genReload_PPC;
         ppInstr     = (void(*)(HInstr*, Bool)) ppPPCInstr;
         ppReg       = (void(*)(HReg)) ppHRegPPC;
         iselSB      = iselSB_PPC;
         emit        = (Int(*)(UChar*,Int,HInstr*,Bool,void*,void*))
                       emit_PPCInstr;
         host_is_bigendian = True;
         host_word_type    = Ity_I64;
         vassert(are_valid_hwcaps(VexArchPPC64, vta->archinfo_host.hwcaps));
         /* return-to-dispatcher scheme */
         vassert(vta->dispatch_unassisted == NULL);
         vassert(vta->dispatch_assisted == NULL);
         break;

      case VexArchS390X:
         mode64      = True;
         getAllocableRegs_S390 ( &n_available_real_regs,
                                 &available_real_regs, mode64 );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_S390Instr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*, Bool)) getRegUsage_S390Instr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*, Bool)) mapRegs_S390Instr;
         genSpill    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genSpill_S390;
         genReload   = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genReload_S390;
         ppInstr     = (void(*)(HInstr*, Bool)) ppS390Instr;
         ppReg       = (void(*)(HReg)) ppHRegS390;
         iselSB      = iselSB_S390;
         emit        = (Int(*)(UChar*,Int,HInstr*,Bool,void*,void*))
                       emit_S390Instr;
         host_is_bigendian = True;
         host_word_type    = Ity_I64;
         vassert(are_valid_hwcaps(VexArchS390X, vta->archinfo_host.hwcaps));
         /* return-to-dispatcher scheme */
         vassert(vta->dispatch_unassisted == NULL);
         vassert(vta->dispatch_assisted == NULL);
         break;

      case VexArchARM:
         mode64      = False;
         getAllocableRegs_ARM ( &n_available_real_regs,
                                &available_real_regs );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_ARMInstr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*, Bool)) getRegUsage_ARMInstr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*, Bool)) mapRegs_ARMInstr;
         genSpill    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genSpill_ARM;
         genReload   = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genReload_ARM;
         ppInstr     = (void(*)(HInstr*, Bool)) ppARMInstr;
         ppReg       = (void(*)(HReg)) ppHRegARM;
         iselSB      = iselSB_ARM;
         emit        = (Int(*)(UChar*,Int,HInstr*,Bool,void*,void*))
                       emit_ARMInstr;
         host_is_bigendian = False;
         host_word_type    = Ity_I32;
         vassert(are_valid_hwcaps(VexArchARM, vta->archinfo_host.hwcaps));
         vassert(vta->dispatch_unassisted == NULL);
         vassert(vta->dispatch_assisted == NULL);
         /* return-to-dispatcher scheme */
         break;

      default:
         vpanic("LibVEX_Translate: unsupported host insn set");
   }


   switch (vta->arch_guest) {

      case VexArchX86:
         preciseMemExnsFn = guest_x86_state_requires_precise_mem_exns;
         disInstrFn       = disInstr_X86;
         specHelper       = guest_x86_spechelper;
         guest_sizeB      = sizeof(VexGuestX86State);
         guest_word_type  = Ity_I32;
         guest_layout     = &x86guest_layout;
         offB_TISTART     = offsetof(VexGuestX86State,guest_TISTART);
         offB_TILEN       = offsetof(VexGuestX86State,guest_TILEN);
         vassert(are_valid_hwcaps(VexArchX86, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestX86State) % 16);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_TISTART) == 4);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_TILEN  ) == 4);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_NRADDR ) == 4);
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
         vassert(are_valid_hwcaps(VexArchAMD64, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestAMD64State) % 16);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_TISTART ) == 8);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_TILEN   ) == 8);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_NRADDR  ) == 8);
         break;

      case VexArchPPC32:
         preciseMemExnsFn = guest_ppc32_state_requires_precise_mem_exns;
         disInstrFn       = disInstr_PPC;
         specHelper       = guest_ppc32_spechelper;
         guest_sizeB      = sizeof(VexGuestPPC32State);
         guest_word_type  = Ity_I32;
         guest_layout     = &ppc32Guest_layout;
         offB_TISTART     = offsetof(VexGuestPPC32State,guest_TISTART);
         offB_TILEN       = offsetof(VexGuestPPC32State,guest_TILEN);
         vassert(are_valid_hwcaps(VexArchPPC32, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestPPC32State) % 16);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_TISTART ) == 4);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_TILEN   ) == 4);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_NRADDR  ) == 4);
         break;

      case VexArchPPC64:
         preciseMemExnsFn = guest_ppc64_state_requires_precise_mem_exns;
         disInstrFn       = disInstr_PPC;
         specHelper       = guest_ppc64_spechelper;
         guest_sizeB      = sizeof(VexGuestPPC64State);
         guest_word_type  = Ity_I64;
         guest_layout     = &ppc64Guest_layout;
         offB_TISTART     = offsetof(VexGuestPPC64State,guest_TISTART);
         offB_TILEN       = offsetof(VexGuestPPC64State,guest_TILEN);
         vassert(are_valid_hwcaps(VexArchPPC64, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestPPC64State) % 16);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_TISTART    ) == 8);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_TILEN      ) == 8);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_NRADDR     ) == 8);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_NRADDR_GPR2) == 8);
         break;

      case VexArchS390X:
         preciseMemExnsFn = guest_s390x_state_requires_precise_mem_exns;
         disInstrFn       = disInstr_S390;
         specHelper       = guest_s390x_spechelper;
         guest_sizeB      = sizeof(VexGuestS390XState);
         guest_word_type  = Ity_I64;
         guest_layout     = &s390xGuest_layout;
         offB_TISTART     = offsetof(VexGuestS390XState,guest_TISTART);
         offB_TILEN       = offsetof(VexGuestS390XState,guest_TILEN);
         vassert(are_valid_hwcaps(VexArchS390X, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestS390XState) % 16);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_TISTART    ) == 8);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_TILEN      ) == 8);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_NRADDR     ) == 8);
         break;

      case VexArchARM:
         preciseMemExnsFn = guest_arm_state_requires_precise_mem_exns;
         disInstrFn       = disInstr_ARM;
         specHelper       = guest_arm_spechelper;
         guest_sizeB      = sizeof(VexGuestARMState);
         guest_word_type  = Ity_I32;
         guest_layout     = &armGuest_layout;
         offB_TISTART     = offsetof(VexGuestARMState,guest_TISTART);
         offB_TILEN       = offsetof(VexGuestARMState,guest_TILEN);
         vassert(are_valid_hwcaps(VexArchARM, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestARMState) % 16);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_TISTART) == 4);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_TILEN  ) == 4);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_NRADDR ) == 4);
         break;

      default:
         vpanic("LibVEX_Translate: unsupported guest insn set");
   }

   /* Set up result struct. */
   VexTranslateResult res;
   res.status       = VexTransOK;
   res.n_sc_extents = 0;

   /* yet more sanity checks ... */
   if (vta->arch_guest == vta->arch_host) {
      /* doesn't necessarily have to be true, but if it isn't it means
         we are simulating one flavour of an architecture a different
         flavour of the same architecture, which is pretty strange. */
      vassert(vta->archinfo_guest.hwcaps == vta->archinfo_host.hwcaps);
   }

   vexAllocSanityCheck();

   if (vex_traceflags & VEX_TRACE_FE)
      vex_printf("\n------------------------" 
                   " Front end "
                   "------------------------\n\n");

   irsb = bb_to_IR ( vta->guest_extents,
                     &res.n_sc_extents,
                     vta->callback_opaque,
                     disInstrFn,
                     vta->guest_bytes, 
                     vta->guest_bytes_addr,
                     vta->chase_into_ok,
                     host_is_bigendian,
                     vta->arch_guest,
                     &vta->archinfo_guest,
                     &vta->abiinfo_both,
                     guest_word_type,
                     vta->needs_self_check,
                     vta->preamble_function,
                     offB_TISTART,
                     offB_TILEN );

   vexAllocSanityCheck();

   if (irsb == NULL) {
      /* Access failure. */
      vexSetAllocModeTEMP_and_clear();
      vex_traceflags = 0;
      res.status = VexTransAccessFail; return res;
   }

   vassert(vta->guest_extents->n_used >= 1 && vta->guest_extents->n_used <= 3);
   vassert(vta->guest_extents->base[0] == vta->guest_bytes_addr);
   for (i = 0; i < vta->guest_extents->n_used; i++) {
      vassert(vta->guest_extents->len[i] < 10000); /* sanity */
   }

   /* If debugging, show the raw guest bytes for this bb. */
   if (0 || (vex_traceflags & VEX_TRACE_FE)) {
      if (vta->guest_extents->n_used > 1) {
         vex_printf("can't show code due to extents > 1\n");
      } else {
         /* HACK */
         UChar* p = (UChar*)vta->guest_bytes;
         UInt   sum = 0;
         UInt   guest_bytes_read = (UInt)vta->guest_extents->len[0];
         vex_printf("GuestBytes %llx %u ", vta->guest_bytes_addr, 
                                           guest_bytes_read );
         for (i = 0; i < guest_bytes_read; i++) {
            UInt b = (UInt)p[i];
            vex_printf(" %02x", b );
            sum = (sum << 1) ^ b;
         }
         vex_printf("  %08x\n\n", sum);
      }
   }

   /* Sanity check the initial IR. */
   sanityCheckIRSB( irsb, "initial IR", 
                    False/*can be non-flat*/, guest_word_type );

   vexAllocSanityCheck();

   /* Clean it up, hopefully a lot. */
   irsb = do_iropt_BB ( irsb, specHelper, preciseMemExnsFn, 
                              vta->guest_bytes_addr,
                              vta->arch_guest );
   sanityCheckIRSB( irsb, "after initial iropt", 
                    True/*must be flat*/, guest_word_type );

   if (vex_traceflags & VEX_TRACE_OPT1) {
      vex_printf("\n------------------------" 
                   " After pre-instr IR optimisation "
                   "------------------------\n\n");
      ppIRSB ( irsb );
      vex_printf("\n");
   }

   vexAllocSanityCheck();

   /* Get the thing instrumented. */
   if (vta->instrument1)
      irsb = vta->instrument1(vta->callback_opaque,
                              irsb, guest_layout, 
                              vta->guest_extents,
                              guest_word_type, host_word_type);
   vexAllocSanityCheck();

   if (vta->instrument2)
      irsb = vta->instrument2(vta->callback_opaque,
                              irsb, guest_layout,
                              vta->guest_extents,
                              guest_word_type, host_word_type);
      
   if (vex_traceflags & VEX_TRACE_INST) {
      vex_printf("\n------------------------" 
                   " After instrumentation "
                   "------------------------\n\n");
      ppIRSB ( irsb );
      vex_printf("\n");
   }

   if (vta->instrument1 || vta->instrument2)
      sanityCheckIRSB( irsb, "after instrumentation",
                       True/*must be flat*/, guest_word_type );

   /* Do a post-instrumentation cleanup pass. */
   if (vta->instrument1 || vta->instrument2) {
      do_deadcode_BB( irsb );
      irsb = cprop_BB( irsb );
      do_deadcode_BB( irsb );
      sanityCheckIRSB( irsb, "after post-instrumentation cleanup",
                       True/*must be flat*/, guest_word_type );
   }

   vexAllocSanityCheck();

   if (vex_traceflags & VEX_TRACE_OPT2) {
      vex_printf("\n------------------------" 
                   " After post-instr IR optimisation "
                   "------------------------\n\n");
      ppIRSB ( irsb );
      vex_printf("\n");
   }

   /* Turn it into virtual-registerised code.  Build trees -- this
      also throws away any dead bindings. */
   ado_treebuild_BB( irsb );

   if (vta->finaltidy) {
      irsb = vta->finaltidy(irsb);
   }

   vexAllocSanityCheck();

   if (vex_traceflags & VEX_TRACE_TREES) {
      vex_printf("\n------------------------" 
                   "  After tree-building "
                   "------------------------\n\n");
      ppIRSB ( irsb );
      vex_printf("\n");
   }

   /* HACK */
   if (0) {
      *(vta->host_bytes_used) = 0;
      res.status = VexTransOK; return res;
   }
   /* end HACK */

   if (vex_traceflags & VEX_TRACE_VCODE)
      vex_printf("\n------------------------" 
                   " Instruction selection "
                   "------------------------\n");

   vcode = iselSB ( irsb, vta->arch_host, &vta->archinfo_host, 
                                          &vta->abiinfo_both );

   vexAllocSanityCheck();

   if (vex_traceflags & VEX_TRACE_VCODE)
      vex_printf("\n");

   if (vex_traceflags & VEX_TRACE_VCODE) {
      for (i = 0; i < vcode->arr_used; i++) {
         vex_printf("%3d   ", i);
         ppInstr(vcode->arr[i], mode64);
         vex_printf("\n");
      }
      vex_printf("\n");
   }

   /* Register allocate. */
   rcode = doRegisterAllocation ( vcode, available_real_regs,
                                  n_available_real_regs,
                                  isMove, getRegUsage, mapRegs, 
                                  genSpill, genReload, directReload, 
                                  guest_sizeB,
                                  ppInstr, ppReg, mode64 );

   vexAllocSanityCheck();

   if (vex_traceflags & VEX_TRACE_RCODE) {
      vex_printf("\n------------------------" 
                   " Register-allocated code "
                   "------------------------\n\n");
      for (i = 0; i < rcode->arr_used; i++) {
         vex_printf("%3d   ", i);
         ppInstr(rcode->arr[i], mode64);
         vex_printf("\n");
      }
      vex_printf("\n");
   }

   /* HACK */
   if (0) { 
      *(vta->host_bytes_used) = 0;
      res.status = VexTransOK; return res;
   }
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
         ppInstr(rcode->arr[i], mode64);
         vex_printf("\n");
      }
      j = (*emit)( insn_bytes, sizeof insn_bytes, rcode->arr[i], mode64,
                   vta->dispatch_unassisted, vta->dispatch_assisted );
      if (vex_traceflags & VEX_TRACE_ASM) {
         for (k = 0; k < j; k++)
            if (insn_bytes[k] < 16)
               vex_printf("0%x ",  (UInt)insn_bytes[k]);
            else
               vex_printf("%x ", (UInt)insn_bytes[k]);
         vex_printf("\n\n");
      }
      if (out_used + j > vta->host_bytes_size) {
         vexSetAllocModeTEMP_and_clear();
         vex_traceflags = 0;
         res.status = VexTransOutputFull;
         return res;
      }
      for (k = 0; k < j; k++) {
         vta->host_bytes[out_used] = insn_bytes[k];
         out_used++;
      }
      vassert(out_used <= vta->host_bytes_size);
   }
   *(vta->host_bytes_used) = out_used;

   vexAllocSanityCheck();

   vexSetAllocModeTEMP_and_clear();

   vex_traceflags = 0;
   res.status = VexTransOK;
   return res;
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
     case EmWarn_PPCexns:
        return "Unmasking PPC32/64 FP exceptions";
     case EmWarn_PPC64_redir_overflow:
        return "PPC64 function redirection stack overflow";
     case EmWarn_PPC64_redir_underflow:
        return "PPC64 function redirection stack underflow";
     default: 
        vpanic("LibVEX_EmWarn_string: unknown warning");
   }
}

/* ------------------ Arch/HwCaps stuff. ------------------ */

const HChar* LibVEX_ppVexArch ( VexArch arch )
{
   switch (arch) {
      case VexArch_INVALID: return "INVALID";
      case VexArchX86:      return "X86";
      case VexArchAMD64:    return "AMD64";
      case VexArchARM:      return "ARM";
      case VexArchPPC32:    return "PPC32";
      case VexArchPPC64:    return "PPC64";
      case VexArchS390X:    return "S390X";
      default:              return "VexArch???";
   }
}

const HChar* LibVEX_ppVexHwCaps ( VexArch arch, UInt hwcaps )
{
   HChar* str = show_hwcaps(arch,hwcaps);
   return str ? str : "INVALID";
}


/* Write default settings info *vai. */
void LibVEX_default_VexArchInfo ( /*OUT*/VexArchInfo* vai )
{
   vai->hwcaps             = 0;
   vai->ppc_cache_line_szB = 0;
   vai->ppc_dcbz_szB       = 0;
   vai->ppc_dcbzl_szB      = 0;

}

/* Write default settings info *vbi. */
void LibVEX_default_VexAbiInfo ( /*OUT*/VexAbiInfo* vbi )
{
   vbi->guest_stack_redzone_size       = 0;
   vbi->guest_amd64_assume_fs_is_zero  = False;
   vbi->guest_amd64_assume_gs_is_0x60  = False;
   vbi->guest_ppc_zap_RZ_at_blr        = False;
   vbi->guest_ppc_zap_RZ_at_bl         = NULL;
   vbi->guest_ppc_sc_continues_at_LR   = False;
   vbi->host_ppc_calls_use_fndescrs    = False;
   vbi->host_ppc32_regalign_int64_args = False;
}


/* Return a string showing the hwcaps in a nice way.  The string will
   be NULL for invalid combinations of flags, so these functions also
   serve as a way to validate hwcaps values. */

static HChar* show_hwcaps_x86 ( UInt hwcaps ) 
{
   /* Monotonic, SSE3 > SSE2 > SSE1 > baseline. */
   switch (hwcaps) {
      case 0:
         return "x86-sse0";
      case VEX_HWCAPS_X86_SSE1:
         return "x86-sse1";
      case VEX_HWCAPS_X86_SSE1 | VEX_HWCAPS_X86_SSE2:
         return "x86-sse1-sse2";
      case VEX_HWCAPS_X86_SSE1 | VEX_HWCAPS_X86_SSE2
           | VEX_HWCAPS_X86_LZCNT:
         return "x86-sse1-sse2-lzcnt";
      case VEX_HWCAPS_X86_SSE1 | VEX_HWCAPS_X86_SSE2
           | VEX_HWCAPS_X86_SSE3:
         return "x86-sse1-sse2-sse3";
      case VEX_HWCAPS_X86_SSE1 | VEX_HWCAPS_X86_SSE2
           | VEX_HWCAPS_X86_SSE3 | VEX_HWCAPS_X86_LZCNT:
         return "x86-sse1-sse2-sse3-lzcnt";
      default:
         return NULL;
   }
}

static HChar* show_hwcaps_amd64 ( UInt hwcaps )
{
   /* SSE3 and CX16 are orthogonal and > baseline, although we really
      don't expect to come across anything which can do SSE3 but can't
      do CX16.  Still, we can handle that case.  LZCNT is similarly
      orthogonal. */
   switch (hwcaps) {
      case 0:
         return "amd64-sse2";
      case VEX_HWCAPS_AMD64_SSE3:
         return "amd64-sse3";
      case VEX_HWCAPS_AMD64_CX16:
         return "amd64-sse2-cx16";
      case VEX_HWCAPS_AMD64_SSE3 | VEX_HWCAPS_AMD64_CX16:
         return "amd64-sse3-cx16";
      case VEX_HWCAPS_AMD64_SSE3 | VEX_HWCAPS_AMD64_LZCNT:
         return "amd64-sse3-lzcnt";
      case VEX_HWCAPS_AMD64_CX16 | VEX_HWCAPS_AMD64_LZCNT:
         return "amd64-sse2-cx16-lzcnt";
      case VEX_HWCAPS_AMD64_SSE3 | VEX_HWCAPS_AMD64_CX16
           | VEX_HWCAPS_AMD64_LZCNT:
         return "amd64-sse3-cx16-lzcnt";

      default:
         return NULL;
   }
}

static HChar* show_hwcaps_ppc32 ( UInt hwcaps )
{
   /* Monotonic with complications.  Basically V > F > baseline,
      but once you have F then you can have FX or GX too. */
   const UInt F  = VEX_HWCAPS_PPC32_F;
   const UInt V  = VEX_HWCAPS_PPC32_V;
   const UInt FX = VEX_HWCAPS_PPC32_FX;
   const UInt GX = VEX_HWCAPS_PPC32_GX;
   const UInt VX = VEX_HWCAPS_PPC32_VX;
         UInt c  = hwcaps;
   if (c == 0)           return "ppc32-int";
   if (c == F)           return "ppc32-int-flt";
   if (c == (F|FX))      return "ppc32-int-flt-FX";
   if (c == (F|GX))      return "ppc32-int-flt-GX";
   if (c == (F|FX|GX))   return "ppc32-int-flt-FX-GX";
   if (c == (F|V))       return "ppc32-int-flt-vmx";
   if (c == (F|V|FX))    return "ppc32-int-flt-vmx-FX";
   if (c == (F|V|GX))    return "ppc32-int-flt-vmx-GX";
   if (c == (F|V|FX|GX)) return "ppc32-int-flt-vmx-FX-GX";
   if (c == (F|V|FX|GX|VX)) return "ppc32-int-flt-vmx-FX-GX-VX";
   return NULL;
}

static HChar* show_hwcaps_ppc64 ( UInt hwcaps )
{
   /* Monotonic with complications.  Basically V > baseline(==F),
      but once you have F then you can have FX or GX too. */
   const UInt V  = VEX_HWCAPS_PPC64_V;
   const UInt FX = VEX_HWCAPS_PPC64_FX;
   const UInt GX = VEX_HWCAPS_PPC64_GX;
   const UInt VX = VEX_HWCAPS_PPC64_VX;
         UInt c  = hwcaps;
   if (c == 0)         return "ppc64-int-flt";
   if (c == FX)        return "ppc64-int-flt-FX";
   if (c == GX)        return "ppc64-int-flt-GX";
   if (c == (FX|GX))   return "ppc64-int-flt-FX-GX";
   if (c == V)         return "ppc64-int-flt-vmx";
   if (c == (V|FX))    return "ppc64-int-flt-vmx-FX";
   if (c == (V|GX))    return "ppc64-int-flt-vmx-GX";
   if (c == (V|FX|GX)) return "ppc64-int-flt-vmx-FX-GX";
   if (c == (V|FX|GX|VX)) return "ppc64-int-flt-vmx-FX-GX-VX";
   return NULL;
}

static HChar* show_hwcaps_arm ( UInt hwcaps )
{
   Bool N = ((hwcaps & VEX_HWCAPS_ARM_NEON) != 0);
   Bool vfp = ((hwcaps & (VEX_HWCAPS_ARM_VFP |
               VEX_HWCAPS_ARM_VFP2 | VEX_HWCAPS_ARM_VFP3)) != 0);
   switch (VEX_ARM_ARCHLEVEL(hwcaps)) {
      case 5:
         if (N)
            return NULL;
         if (vfp)
            return "ARMv5-vfp";
         else
            return "ARMv5";
         return NULL;
      case 6:
         if (N)
            return NULL;
         if (vfp)
            return "ARMv6-vfp";
         else
            return "ARMv6";
         return NULL;
      case 7:
         if (vfp) {
            if (N)
               return "ARMv7-vfp-neon";
            else
               return "ARMv7-vfp";
         } else {
            if (N)
               return "ARMv7-neon";
            else
               return "ARMv7";
         }
      default:
         return NULL;
   }
   return NULL;
}

static HChar* show_hwcaps_s390x ( UInt hwcaps )
{
   static const HChar prefix[] = "s390x";
   static const HChar facilities[][6] = {
     { "ldisp" },
     { "eimm" },
     { "gie" },
     { "dfp" },
     { "fgx" },
   };
   static HChar buf[sizeof facilities + sizeof prefix + 1];
   static HChar *p;

   if (buf[0] != '\0') return buf;  /* already constructed */

   hwcaps = VEX_HWCAPS_S390X(hwcaps);

   p = buf + vex_sprintf(buf, "%s", prefix);
   if (hwcaps & VEX_HWCAPS_S390X_LDISP)
     p = p + vex_sprintf(p, "-%s", facilities[0]);
   if (hwcaps & VEX_HWCAPS_S390X_EIMM)
     p = p + vex_sprintf(p, "-%s", facilities[1]);
   if (hwcaps & VEX_HWCAPS_S390X_GIE)
     p = p + vex_sprintf(p, "-%s", facilities[2]);
   if (hwcaps & VEX_HWCAPS_S390X_DFP)
     p = p + vex_sprintf(p, "-%s", facilities[3]);
   if (hwcaps & VEX_HWCAPS_S390X_FGX)
     p = p + vex_sprintf(p, "-%s", facilities[4]);

   /* If there are no facilities, add "zarch" */
   if (hwcaps == 0)
     vex_sprintf(p, "-%s", "zarch");

   return buf;
}

/* ---- */
static HChar* show_hwcaps ( VexArch arch, UInt hwcaps )
{
   switch (arch) {
      case VexArchX86:   return show_hwcaps_x86(hwcaps);
      case VexArchAMD64: return show_hwcaps_amd64(hwcaps);
      case VexArchPPC32: return show_hwcaps_ppc32(hwcaps);
      case VexArchPPC64: return show_hwcaps_ppc64(hwcaps);
      case VexArchARM:   return show_hwcaps_arm(hwcaps);
      case VexArchS390X: return show_hwcaps_s390x(hwcaps);
      default: return NULL;
   }
}

static Bool are_valid_hwcaps ( VexArch arch, UInt hwcaps )
{
   return show_hwcaps(arch,hwcaps) != NULL;
}


/*---------------------------------------------------------------*/
/*--- end                                         main_main.c ---*/
/*---------------------------------------------------------------*/
