
/*---------------------------------------------------------------*/
/*--- begin                                       main_main.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2013 OpenWorks LLP
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
#include "libvex_emnote.h"
#include "libvex_guest_x86.h"
#include "libvex_guest_amd64.h"
#include "libvex_guest_arm.h"
#include "libvex_guest_arm64.h"
#include "libvex_guest_ppc32.h"
#include "libvex_guest_ppc64.h"
#include "libvex_guest_s390x.h"
#include "libvex_guest_mips32.h"
#include "libvex_guest_mips64.h"

#include "main_globals.h"
#include "main_util.h"
#include "host_generic_regs.h"
#include "ir_opt.h"

#include "host_x86_defs.h"
#include "host_amd64_defs.h"
#include "host_ppc_defs.h"
#include "host_arm_defs.h"
#include "host_arm64_defs.h"
#include "host_s390_defs.h"
#include "host_mips_defs.h"

#include "guest_generic_bb_to_IR.h"
#include "guest_x86_defs.h"
#include "guest_amd64_defs.h"
#include "guest_arm_defs.h"
#include "guest_arm64_defs.h"
#include "guest_ppc_defs.h"
#include "guest_s390_defs.h"
#include "guest_mips_defs.h"

#include "host_generic_simd128.h"


/* This file contains the top level interface to the library. */

/* --------- fwds ... --------- */

static Bool   are_valid_hwcaps ( VexArch arch, UInt hwcaps );
static const HChar* show_hwcaps ( VexArch arch, UInt hwcaps );


/* --------- helpers --------- */

__attribute__((noinline))
static UInt udiv32 ( UInt x, UInt y ) { return x/y; }
__attribute__((noinline))
static  Int sdiv32 (  Int x,  Int y ) { return x/y; }


/* --------- Initialise the library. --------- */

/* Exported to library client. */

void LibVEX_default_VexControl ( /*OUT*/ VexControl* vcon )
{
   vex_bzero(vcon, sizeof(*vcon));
   vcon->iropt_verbosity            = 0;
   vcon->iropt_level                = 2;
   vcon->iropt_register_updates     = VexRegUpdUnwindregsAtMemAccess;
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
   vassert(32 == sizeof(U256));

   vassert(sizeof(void*) == 4 || sizeof(void*) == 8);
   vassert(sizeof(void*) == sizeof(int*));
   vassert(sizeof(void*) == sizeof(HWord));

   vassert(VEX_HOST_WORDSIZE == sizeof(void*));
   vassert(VEX_HOST_WORDSIZE == sizeof(HWord));

   /* These take a lot of space, so make sure we don't have
      any unnoticed size regressions. */
   if (VEX_HOST_WORDSIZE == 4) {
      vassert(sizeof(IRExpr) == 16);
      vassert(sizeof(IRStmt) == 20 /* x86 */
              || sizeof(IRStmt) == 24 /* arm */);
   } else {
      vassert(sizeof(IRExpr) == 32);
      vassert(sizeof(IRStmt) == 32);
   }

   /* Check that signed integer division on the host rounds towards
      zero.  If not, h_calc_sdiv32_w_arm_semantics() won't work
      correctly. */
   /* 100.0 / 7.0 == 14.2857 */
   vassert(udiv32(100, 7) == 14);
   vassert(sdiv32(100, 7) == 14);
   vassert(sdiv32(-100, 7) == -14); /* and not -15 */
   vassert(sdiv32(100, -7) == -14); /* ditto */
   vassert(sdiv32(-100, -7) == 14); /* not sure what this proves */

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
   HInstrArray* (*iselSB)       ( IRSB*, VexArch, VexArchInfo*, VexAbiInfo*,
                                  Int, Int, Bool, Bool, Addr64 );
   Int          (*emit)         ( /*MB_MOD*/Bool*,
                                  UChar*, Int, HInstr*, Bool,
                                  void*, void*, void*, void* );
   IRExpr*      (*specHelper)   ( const HChar*, IRExpr**, IRStmt**, Int );
   Bool         (*preciseMemExnsFn) ( Int, Int );

   DisOneInstrFn disInstrFn;

   VexGuestLayout* guest_layout;
   Bool            host_is_bigendian = False;
   IRSB*           irsb;
   HInstrArray*    vcode;
   HInstrArray*    rcode;
   Int             i, j, k, out_used, guest_sizeB;
   Int             offB_CMSTART, offB_CMLEN, offB_GUEST_IP, szB_GUEST_IP;
   Int             offB_HOST_EvC_COUNTER, offB_HOST_EvC_FAILADDR;
   UChar           insn_bytes[128];
   IRType          guest_word_type;
   IRType          host_word_type;
   Bool            mode64, chainingAllowed;
   Addr64          max_ga;

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
   offB_CMSTART           = 0;
   offB_CMLEN             = 0;
   offB_GUEST_IP          = 0;
   szB_GUEST_IP           = 0;
   offB_HOST_EvC_COUNTER  = 0;
   offB_HOST_EvC_FAILADDR = 0;
   mode64                 = False;
   chainingAllowed        = False;

   vex_traceflags = vta->traceflags;

   vassert(vex_initdone);
   vassert(vta->needs_self_check  != NULL);
   vassert(vta->disp_cp_xassisted != NULL);
   /* Both the chainers and the indir are either NULL or non-NULL. */
   if (vta->disp_cp_chain_me_to_slowEP        != NULL) {
      vassert(vta->disp_cp_chain_me_to_fastEP != NULL);
      vassert(vta->disp_cp_xindir             != NULL);
      chainingAllowed = True;
   } else {
      vassert(vta->disp_cp_chain_me_to_fastEP == NULL);
      vassert(vta->disp_cp_xindir             == NULL);
   }

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
         emit         = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*))
                        emit_X86Instr;
         host_is_bigendian = False;
         host_word_type    = Ity_I32;
         vassert(are_valid_hwcaps(VexArchX86, vta->archinfo_host.hwcaps));
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
         emit        = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*))
                       emit_AMD64Instr;
         host_is_bigendian = False;
         host_word_type    = Ity_I64;
         vassert(are_valid_hwcaps(VexArchAMD64, vta->archinfo_host.hwcaps));
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
         emit        = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*))
                       emit_PPCInstr;
         host_is_bigendian = True;
         host_word_type    = Ity_I32;
         vassert(are_valid_hwcaps(VexArchPPC32, vta->archinfo_host.hwcaps));
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
         emit        = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*))
                       emit_PPCInstr;
         host_is_bigendian = True;
         host_word_type    = Ity_I64;
         vassert(are_valid_hwcaps(VexArchPPC64, vta->archinfo_host.hwcaps));
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
         emit        = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*)) emit_S390Instr;
         host_is_bigendian = True;
         host_word_type    = Ity_I64;
         vassert(are_valid_hwcaps(VexArchS390X, vta->archinfo_host.hwcaps));
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
         emit        = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*))
                       emit_ARMInstr;
         host_is_bigendian = False;
         host_word_type    = Ity_I32;
         vassert(are_valid_hwcaps(VexArchARM, vta->archinfo_host.hwcaps));
         break;

      case VexArchARM64:
         mode64      = True;
         getAllocableRegs_ARM64 ( &n_available_real_regs,
                                  &available_real_regs );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_ARM64Instr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*, Bool))
                       getRegUsage_ARM64Instr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*, Bool))
                       mapRegs_ARM64Instr;
         genSpill    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool))
                       genSpill_ARM64;
         genReload   = (void(*)(HInstr**,HInstr**,HReg,Int,Bool))
                       genReload_ARM64;
         ppInstr     = (void(*)(HInstr*, Bool)) ppARM64Instr;
         ppReg       = (void(*)(HReg)) ppHRegARM64;
         iselSB      = iselSB_ARM64;
         emit        = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*))
                       emit_ARM64Instr;
         host_is_bigendian = False;
         host_word_type    = Ity_I64;
         vassert(are_valid_hwcaps(VexArchARM64, vta->archinfo_host.hwcaps));
         break;

      case VexArchMIPS32:
         mode64      = False;
         getAllocableRegs_MIPS ( &n_available_real_regs,
                                &available_real_regs, mode64 );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_MIPSInstr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*, Bool)) getRegUsage_MIPSInstr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*, Bool)) mapRegs_MIPSInstr;
         genSpill    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genSpill_MIPS;
         genReload   = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genReload_MIPS;
         ppInstr     = (void(*)(HInstr*, Bool)) ppMIPSInstr;
         ppReg       = (void(*)(HReg)) ppHRegMIPS;
         iselSB      = iselSB_MIPS;
         emit        = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*))
                       emit_MIPSInstr;
#        if defined(VKI_LITTLE_ENDIAN)
         host_is_bigendian = False;
#        elif defined(VKI_BIG_ENDIAN)
         host_is_bigendian = True;
#        endif
         host_word_type    = Ity_I32;
         vassert(are_valid_hwcaps(VexArchMIPS32, vta->archinfo_host.hwcaps));
         break;

      case VexArchMIPS64:
         mode64      = True;
         getAllocableRegs_MIPS ( &n_available_real_regs,
                                 &available_real_regs, mode64 );
         isMove      = (Bool(*)(HInstr*,HReg*,HReg*)) isMove_MIPSInstr;
         getRegUsage = (void(*)(HRegUsage*,HInstr*, Bool)) getRegUsage_MIPSInstr;
         mapRegs     = (void(*)(HRegRemap*,HInstr*, Bool)) mapRegs_MIPSInstr;
         genSpill    = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genSpill_MIPS;
         genReload   = (void(*)(HInstr**,HInstr**,HReg,Int,Bool)) genReload_MIPS;
         ppInstr     = (void(*)(HInstr*, Bool)) ppMIPSInstr;
         ppReg       = (void(*)(HReg)) ppHRegMIPS;
         iselSB      = iselSB_MIPS;
         emit        = (Int(*)(Bool*,UChar*,Int,HInstr*,Bool,
                               void*,void*,void*,void*))
                       emit_MIPSInstr;
#        if defined(VKI_LITTLE_ENDIAN)
         host_is_bigendian = False;
#        elif defined(VKI_BIG_ENDIAN)
         host_is_bigendian = True;
#        endif
         host_word_type    = Ity_I64;
         vassert(are_valid_hwcaps(VexArchMIPS64, vta->archinfo_host.hwcaps));
         break;

      default:
         vpanic("LibVEX_Translate: unsupported host insn set");
   }


   switch (vta->arch_guest) {

      case VexArchX86:
         preciseMemExnsFn       = guest_x86_state_requires_precise_mem_exns;
         disInstrFn             = disInstr_X86;
         specHelper             = guest_x86_spechelper;
         guest_sizeB            = sizeof(VexGuestX86State);
         guest_word_type        = Ity_I32;
         guest_layout           = &x86guest_layout;
         offB_CMSTART           = offsetof(VexGuestX86State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestX86State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestX86State,guest_EIP);
         szB_GUEST_IP           = sizeof( ((VexGuestX86State*)0)->guest_EIP );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestX86State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestX86State,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchX86, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestX86State) % 16);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_CMSTART) == 4);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_CMLEN  ) == 4);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_NRADDR ) == 4);
         break;

      case VexArchAMD64:
         preciseMemExnsFn       = guest_amd64_state_requires_precise_mem_exns;
         disInstrFn             = disInstr_AMD64;
         specHelper             = guest_amd64_spechelper;
         guest_sizeB            = sizeof(VexGuestAMD64State);
         guest_word_type        = Ity_I64;
         guest_layout           = &amd64guest_layout;
         offB_CMSTART           = offsetof(VexGuestAMD64State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestAMD64State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestAMD64State,guest_RIP);
         szB_GUEST_IP           = sizeof( ((VexGuestAMD64State*)0)->guest_RIP );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestAMD64State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestAMD64State,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchAMD64, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestAMD64State) % 16);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_CMSTART ) == 8);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_CMLEN   ) == 8);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_NRADDR  ) == 8);
         break;

      case VexArchPPC32:
         preciseMemExnsFn       = guest_ppc32_state_requires_precise_mem_exns;
         disInstrFn             = disInstr_PPC;
         specHelper             = guest_ppc32_spechelper;
         guest_sizeB            = sizeof(VexGuestPPC32State);
         guest_word_type        = Ity_I32;
         guest_layout           = &ppc32Guest_layout;
         offB_CMSTART           = offsetof(VexGuestPPC32State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestPPC32State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestPPC32State,guest_CIA);
         szB_GUEST_IP           = sizeof( ((VexGuestPPC32State*)0)->guest_CIA );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestPPC32State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestPPC32State,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchPPC32, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestPPC32State) % 16);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_CMSTART ) == 4);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_CMLEN   ) == 4);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_NRADDR  ) == 4);
         break;

      case VexArchPPC64:
         preciseMemExnsFn       = guest_ppc64_state_requires_precise_mem_exns;
         disInstrFn             = disInstr_PPC;
         specHelper             = guest_ppc64_spechelper;
         guest_sizeB            = sizeof(VexGuestPPC64State);
         guest_word_type        = Ity_I64;
         guest_layout           = &ppc64Guest_layout;
         offB_CMSTART           = offsetof(VexGuestPPC64State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestPPC64State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestPPC64State,guest_CIA);
         szB_GUEST_IP           = sizeof( ((VexGuestPPC64State*)0)->guest_CIA );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestPPC64State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestPPC64State,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchPPC64, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestPPC64State) % 16);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_CMSTART    ) == 8);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_CMLEN      ) == 8);
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
         offB_CMSTART     = offsetof(VexGuestS390XState,guest_CMSTART);
         offB_CMLEN       = offsetof(VexGuestS390XState,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestS390XState,guest_IA);
         szB_GUEST_IP           = sizeof( ((VexGuestS390XState*)0)->guest_IA);
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestS390XState,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestS390XState,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchS390X, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestS390XState) % 16);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_CMSTART    ) == 8);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_CMLEN      ) == 8);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_NRADDR     ) == 8);
         break;

      case VexArchARM:
         preciseMemExnsFn       = guest_arm_state_requires_precise_mem_exns;
         disInstrFn             = disInstr_ARM;
         specHelper             = guest_arm_spechelper;
         guest_sizeB            = sizeof(VexGuestARMState);
         guest_word_type        = Ity_I32;
         guest_layout           = &armGuest_layout;
         offB_CMSTART           = offsetof(VexGuestARMState,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestARMState,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestARMState,guest_R15T);
         szB_GUEST_IP           = sizeof( ((VexGuestARMState*)0)->guest_R15T );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestARMState,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestARMState,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchARM, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestARMState) % 16);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_CMSTART) == 4);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_CMLEN  ) == 4);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_NRADDR ) == 4);
         break;

      case VexArchARM64:
         preciseMemExnsFn     = guest_arm64_state_requires_precise_mem_exns;
         disInstrFn           = disInstr_ARM64;
         specHelper           = guest_arm64_spechelper;
         guest_sizeB          = sizeof(VexGuestARM64State);
         guest_word_type      = Ity_I64;
         guest_layout         = &arm64Guest_layout;
         offB_CMSTART         = offsetof(VexGuestARM64State,guest_CMSTART);
         offB_CMLEN           = offsetof(VexGuestARM64State,guest_CMLEN);
         offB_GUEST_IP        = offsetof(VexGuestARM64State,guest_PC);
         szB_GUEST_IP         = sizeof( ((VexGuestARM64State*)0)->guest_PC );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestARM64State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestARM64State,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchARM64, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestARM64State) % 16);
         vassert(sizeof( ((VexGuestARM64State*)0)->guest_CMSTART) == 8);
         vassert(sizeof( ((VexGuestARM64State*)0)->guest_CMLEN  ) == 8);
         vassert(sizeof( ((VexGuestARM64State*)0)->guest_NRADDR ) == 8);
         break;

      case VexArchMIPS32:
         preciseMemExnsFn       = guest_mips32_state_requires_precise_mem_exns;
         disInstrFn             = disInstr_MIPS;
         specHelper             = guest_mips32_spechelper;
         guest_sizeB            = sizeof(VexGuestMIPS32State);
         guest_word_type        = Ity_I32;
         guest_layout           = &mips32Guest_layout;
         offB_CMSTART           = offsetof(VexGuestMIPS32State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestMIPS32State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestMIPS32State,guest_PC);
         szB_GUEST_IP           = sizeof( ((VexGuestMIPS32State*)0)->guest_PC );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestMIPS32State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestMIPS32State,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchMIPS32, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestMIPS32State) % 16);
         vassert(sizeof( ((VexGuestMIPS32State*)0)->guest_CMSTART) == 4);
         vassert(sizeof( ((VexGuestMIPS32State*)0)->guest_CMLEN  ) == 4);
         vassert(sizeof( ((VexGuestMIPS32State*)0)->guest_NRADDR ) == 4);
         break;

      case VexArchMIPS64:
         preciseMemExnsFn       = guest_mips64_state_requires_precise_mem_exns;
         disInstrFn             = disInstr_MIPS;
         specHelper             = guest_mips64_spechelper;
         guest_sizeB            = sizeof(VexGuestMIPS64State);
         guest_word_type        = Ity_I64;
         guest_layout           = &mips64Guest_layout;
         offB_CMSTART           = offsetof(VexGuestMIPS64State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestMIPS64State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestMIPS64State,guest_PC);
         szB_GUEST_IP           = sizeof( ((VexGuestMIPS64State*)0)->guest_PC );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestMIPS64State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestMIPS64State,host_EvC_FAILADDR);
         vassert(are_valid_hwcaps(VexArchMIPS64, vta->archinfo_guest.hwcaps));
         vassert(0 == sizeof(VexGuestMIPS64State) % 16);
         vassert(sizeof( ((VexGuestMIPS64State*)0)->guest_CMSTART) == 8);
         vassert(sizeof( ((VexGuestMIPS64State*)0)->guest_CMLEN  ) == 8);
         vassert(sizeof( ((VexGuestMIPS64State*)0)->guest_NRADDR ) == 8);
         break;

      default:
         vpanic("LibVEX_Translate: unsupported guest insn set");
   }

   /* Set up result struct. */
   VexTranslateResult res;
   res.status         = VexTransOK;
   res.n_sc_extents   = 0;
   res.offs_profInc   = -1;
   res.n_guest_instrs = 0;

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
                     &res.n_guest_instrs,
                     vta->callback_opaque,
                     disInstrFn,
                     vta->guest_bytes, 
                     vta->guest_bytes_addr,
                     vta->chase_into_ok,
                     host_is_bigendian,
                     vta->sigill_diag,
                     vta->arch_guest,
                     &vta->archinfo_guest,
                     &vta->abiinfo_both,
                     guest_word_type,
                     vta->needs_self_check,
                     vta->preamble_function,
                     offB_CMSTART,
                     offB_CMLEN,
                     offB_GUEST_IP,
                     szB_GUEST_IP );

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
                              &vta->archinfo_host,
                              guest_word_type, host_word_type);
   vexAllocSanityCheck();

   if (vta->instrument2)
      irsb = vta->instrument2(vta->callback_opaque,
                              irsb, guest_layout,
                              vta->guest_extents,
                              &vta->archinfo_host,
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
   max_ga = ado_treebuild_BB( irsb, preciseMemExnsFn );

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

   /* No guest has its IP field at offset zero.  If this fails it
      means some transformation pass somewhere failed to update/copy
      irsb->offsIP properly. */
   vassert(irsb->offsIP >= 16);

   vcode = iselSB ( irsb, vta->arch_host,
                    &vta->archinfo_host, 
                    &vta->abiinfo_both,
                    offB_HOST_EvC_COUNTER,
                    offB_HOST_EvC_FAILADDR,
                    chainingAllowed,
                    vta->addProfInc,
                    max_ga );

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
      HInstr* hi           = rcode->arr[i];
      Bool    hi_isProfInc = False;
      if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
         ppInstr(hi, mode64);
         vex_printf("\n");
      }
      j = emit( &hi_isProfInc,
                insn_bytes, sizeof insn_bytes, hi, mode64,
                vta->disp_cp_chain_me_to_slowEP,
                vta->disp_cp_chain_me_to_fastEP,
                vta->disp_cp_xindir,
                vta->disp_cp_xassisted );
      if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
         for (k = 0; k < j; k++)
            if (insn_bytes[k] < 16)
               vex_printf("0%x ",  (UInt)insn_bytes[k]);
            else
               vex_printf("%x ", (UInt)insn_bytes[k]);
         vex_printf("\n\n");
      }
      if (UNLIKELY(out_used + j > vta->host_bytes_size)) {
         vexSetAllocModeTEMP_and_clear();
         vex_traceflags = 0;
         res.status = VexTransOutputFull;
         return res;
      }
      if (UNLIKELY(hi_isProfInc)) {
         vassert(vta->addProfInc); /* else where did it come from? */
         vassert(res.offs_profInc == -1); /* there can be only one (tm) */
         vassert(out_used >= 0);
         res.offs_profInc = out_used;
      }
      { UChar* dst = &vta->host_bytes[out_used];
        for (k = 0; k < j; k++) {
           dst[k] = insn_bytes[k];
        }
        out_used += j;
      }
      vassert(out_used <= vta->host_bytes_size);
   }
   *(vta->host_bytes_used) = out_used;

   vexAllocSanityCheck();

   vexSetAllocModeTEMP_and_clear();

   if (vex_traceflags) {
      /* Print the expansion ratio for this SB. */
      j = 0; /* total guest bytes */
      for (i = 0; i < vta->guest_extents->n_used; i++) {
         j += vta->guest_extents->len[i];
      }
      if (1) vex_printf("VexExpansionRatio %d %d   %d :10\n\n",
                        j, out_used, (10 * out_used) / (j == 0 ? 1 : j));
   }

   vex_traceflags = 0;
   res.status = VexTransOK;
   return res;
}


/* --------- Chain/Unchain XDirects. --------- */

VexInvalRange LibVEX_Chain ( VexArch arch_host,
                             void*   place_to_chain,
                             void*   disp_cp_chain_me_EXPECTED,
                             void*   place_to_jump_to )
{
   VexInvalRange (*chainXDirect)(void*, void*, void*) = NULL;
   switch (arch_host) {
      case VexArchX86:
         chainXDirect = chainXDirect_X86; break;
      case VexArchAMD64:
         chainXDirect = chainXDirect_AMD64; break;
      case VexArchARM:
         chainXDirect = chainXDirect_ARM; break;
      case VexArchARM64:
         chainXDirect = chainXDirect_ARM64; break;
      case VexArchS390X:
         chainXDirect = chainXDirect_S390; break;
      case VexArchPPC32:
         return chainXDirect_PPC(place_to_chain,
                                 disp_cp_chain_me_EXPECTED,
                                 place_to_jump_to, False/*!mode64*/);
      case VexArchPPC64:
         return chainXDirect_PPC(place_to_chain,
                                 disp_cp_chain_me_EXPECTED,
                                 place_to_jump_to, True/*mode64*/);
      case VexArchMIPS32:
         return chainXDirect_MIPS(place_to_chain,
                                  disp_cp_chain_me_EXPECTED,
                                  place_to_jump_to, False/*!mode64*/);
      case VexArchMIPS64:
         return chainXDirect_MIPS(place_to_chain,
                                  disp_cp_chain_me_EXPECTED,
                                  place_to_jump_to, True/*!mode64*/);
      default:
         vassert(0);
   }
   vassert(chainXDirect);
   VexInvalRange vir
      = chainXDirect(place_to_chain, disp_cp_chain_me_EXPECTED,
                     place_to_jump_to);
   return vir;
}

VexInvalRange LibVEX_UnChain ( VexArch arch_host,
                               void*   place_to_unchain,
                               void*   place_to_jump_to_EXPECTED,
                               void*   disp_cp_chain_me )
{
   VexInvalRange (*unchainXDirect)(void*, void*, void*) = NULL;
   switch (arch_host) {
      case VexArchX86:
         unchainXDirect = unchainXDirect_X86; break;
      case VexArchAMD64:
         unchainXDirect = unchainXDirect_AMD64; break;
      case VexArchARM:
         unchainXDirect = unchainXDirect_ARM; break;
      case VexArchARM64:
         unchainXDirect = unchainXDirect_ARM64; break;
      case VexArchS390X:
         unchainXDirect = unchainXDirect_S390; break;
      case VexArchPPC32:
         return unchainXDirect_PPC(place_to_unchain,
                                   place_to_jump_to_EXPECTED,
                                   disp_cp_chain_me, False/*!mode64*/);
      case VexArchPPC64:
         return unchainXDirect_PPC(place_to_unchain,
                                   place_to_jump_to_EXPECTED,
                                   disp_cp_chain_me, True/*mode64*/);
      case VexArchMIPS32:
         return unchainXDirect_MIPS(place_to_unchain,
                                    place_to_jump_to_EXPECTED,
                                    disp_cp_chain_me, False/*!mode64*/);
      case VexArchMIPS64:
         return unchainXDirect_MIPS(place_to_unchain,
                                    place_to_jump_to_EXPECTED,
                                    disp_cp_chain_me, True/*!mode64*/);
      default:
         vassert(0);
   }
   vassert(unchainXDirect);
   VexInvalRange vir
      = unchainXDirect(place_to_unchain, place_to_jump_to_EXPECTED,
                       disp_cp_chain_me);
   return vir;
}

Int LibVEX_evCheckSzB ( VexArch arch_host )
{
   static Int cached = 0; /* DO NOT MAKE NON-STATIC */
   if (UNLIKELY(cached == 0)) {
      switch (arch_host) {
         case VexArchX86:
            cached = evCheckSzB_X86(); break;
         case VexArchAMD64:
            cached = evCheckSzB_AMD64(); break;
         case VexArchARM:
            cached = evCheckSzB_ARM(); break;
         case VexArchARM64:
            cached = evCheckSzB_ARM64(); break;
         case VexArchS390X:
            cached = evCheckSzB_S390(); break;
         case VexArchPPC32:
         case VexArchPPC64:
            cached = evCheckSzB_PPC(); break;
         case VexArchMIPS32:
         case VexArchMIPS64:
            cached = evCheckSzB_MIPS(); break;
         default:
            vassert(0);
      }
   }
   return cached;
}

VexInvalRange LibVEX_PatchProfInc ( VexArch arch_host,
                                    void*   place_to_patch,
                                    ULong*  location_of_counter )
{
   VexInvalRange (*patchProfInc)(void*,ULong*) = NULL;
   switch (arch_host) {
      case VexArchX86:
         patchProfInc = patchProfInc_X86; break;
      case VexArchAMD64:
         patchProfInc = patchProfInc_AMD64; break;
      case VexArchARM:
         patchProfInc = patchProfInc_ARM; break;
      case VexArchS390X:
         patchProfInc = patchProfInc_S390; break;
      case VexArchPPC32:
         return patchProfInc_PPC(place_to_patch,
                                 location_of_counter, False/*!mode64*/);
      case VexArchPPC64:
         return patchProfInc_PPC(place_to_patch,
                                 location_of_counter, True/*mode64*/);
      case VexArchMIPS32:
         return patchProfInc_MIPS(place_to_patch,
                                  location_of_counter, False/*!mode64*/);
      case VexArchMIPS64:
         return patchProfInc_MIPS(place_to_patch,
                                  location_of_counter, True/*!mode64*/);
      default:
         vassert(0);
   }
   vassert(patchProfInc);
   VexInvalRange vir
      = patchProfInc(place_to_patch, location_of_counter);
   return vir;
}


/* --------- Emulation warnings. --------- */

const HChar* LibVEX_EmNote_string ( VexEmNote ew )
{
   switch (ew) {
     case EmNote_NONE: 
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
     case EmWarn_S390X_fpext_rounding:
        return "The specified rounding mode cannot be supported. That\n"
               "  feature requires the floating point extension facility.\n"
               "  which is not available on this host. Continuing using\n"
               "  the rounding mode from FPC. Results may differ!";
     case EmWarn_S390X_invalid_rounding:
        return "The specified rounding mode is invalid.\n"
               "  Continuing using 'round to nearest'. Results may differ!";
     case EmFail_S390X_stfle:
        return "Instruction stfle is not supported on this host";
     case EmFail_S390X_stckf:
        return "Instruction stckf is not supported on this host";
     case EmFail_S390X_ecag:
        return "Instruction ecag is not supported on this host";
     case EmFail_S390X_fpext:
        return "Encountered an instruction that requires the floating "
               "point extension facility.\n"
               "  That facility is not available on this host";
     case EmFail_S390X_invalid_PFPO_rounding_mode:
        return "The rounding mode specified in GPR 0 for PFPO instruction"
               " is invalid";
     case EmFail_S390X_invalid_PFPO_function:
        return "The function code specified in GPR 0 for PFPO instruction"
               " is invalid";
     default: 
        vpanic("LibVEX_EmNote_string: unknown warning");
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
      case VexArchARM64:    return "ARM64";
      case VexArchPPC32:    return "PPC32";
      case VexArchPPC64:    return "PPC64";
      case VexArchS390X:    return "S390X";
      case VexArchMIPS32:   return "MIPS32";
      case VexArchMIPS64:   return "MIPS64";
      default:              return "VexArch???";
   }
}

const HChar* LibVEX_ppVexHwCaps ( VexArch arch, UInt hwcaps )
{
   const HChar* str = show_hwcaps(arch,hwcaps);
   return str ? str : "INVALID";
}


/* Write default settings info *vai. */
void LibVEX_default_VexArchInfo ( /*OUT*/VexArchInfo* vai )
{
   vex_bzero(vai, sizeof(*vai));
   vai->hwcaps              = 0;
   vai->ppc_icache_line_szB = 0;
   vai->ppc_dcbz_szB        = 0;
   vai->ppc_dcbzl_szB       = 0;
   vai->arm64_dMinLine_lg2_szB  = 0;
   vai->arm64_iMinLine_lg2_szB  = 0;
   vai->hwcache_info.num_levels = 0;
   vai->hwcache_info.num_caches = 0;
   vai->hwcache_info.caches     = NULL;
   vai->hwcache_info.icaches_maintain_coherence = True;  // whatever
}

/* Write default settings info *vbi. */
void LibVEX_default_VexAbiInfo ( /*OUT*/VexAbiInfo* vbi )
{
   vex_bzero(vbi, sizeof(*vbi));
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

static const HChar* show_hwcaps_x86 ( UInt hwcaps ) 
{
   /* Monotonic, LZCNT > SSE3 > SSE2 > SSE1 > MMXEXT > baseline. */
   switch (hwcaps) {
      case 0:
         return "x86-sse0";
      case VEX_HWCAPS_X86_MMXEXT:
         return "x86-mmxext";
      case VEX_HWCAPS_X86_MMXEXT | VEX_HWCAPS_X86_SSE1:
         return "x86-mmxext-sse1";
      case VEX_HWCAPS_X86_MMXEXT | VEX_HWCAPS_X86_SSE1 | VEX_HWCAPS_X86_SSE2:
         return "x86-mmxext-sse1-sse2";
      case VEX_HWCAPS_X86_MMXEXT | VEX_HWCAPS_X86_SSE1 | VEX_HWCAPS_X86_SSE2
           | VEX_HWCAPS_X86_LZCNT:
         return "x86-mmxext-sse1-sse2-lzcnt";
      case VEX_HWCAPS_X86_MMXEXT | VEX_HWCAPS_X86_SSE1 | VEX_HWCAPS_X86_SSE2
           | VEX_HWCAPS_X86_SSE3:
         return "x86-mmxext-sse1-sse2-sse3";
      case VEX_HWCAPS_X86_MMXEXT | VEX_HWCAPS_X86_SSE1 | VEX_HWCAPS_X86_SSE2
           | VEX_HWCAPS_X86_SSE3 | VEX_HWCAPS_X86_LZCNT:
         return "x86-mmxext-sse1-sse2-sse3-lzcnt";
      default:
         return NULL;
   }
}

static const HChar* show_hwcaps_amd64 ( UInt hwcaps )
{
   /* SSE3 and CX16 are orthogonal and > baseline, although we really
      don't expect to come across anything which can do SSE3 but can't
      do CX16.  Still, we can handle that case.  LZCNT is similarly
      orthogonal. */

   /* Throw out obviously stupid cases: */
   Bool have_sse3 = (hwcaps & VEX_HWCAPS_AMD64_SSE3) != 0;
   Bool have_avx  = (hwcaps & VEX_HWCAPS_AMD64_AVX)  != 0;
   Bool have_bmi  = (hwcaps & VEX_HWCAPS_AMD64_BMI)  != 0;
   Bool have_avx2 = (hwcaps & VEX_HWCAPS_AMD64_AVX2) != 0;
   /* AVX without SSE3 */
   if (have_avx && !have_sse3)
      return NULL;
   /* AVX2 or BMI without AVX */
   if ((have_avx2 || have_bmi) && !have_avx)
      return NULL;

   /* This isn't threadsafe.  We might need to fix it at some point. */
   static HChar buf[100] = { 0 };
   if (buf[0] != 0) return buf; /* already constructed */

   vex_bzero(buf, sizeof(buf));

   HChar* p = &buf[0];

   p = p + vex_sprintf(p, "%s", "amd64");
   if (hwcaps == 0) {
      /* special-case the baseline case */
      p = p + vex_sprintf(p, "%s", "-sse2");
      goto out;
   }
   if (hwcaps & VEX_HWCAPS_AMD64_CX16) {
      p = p + vex_sprintf(p, "%s", "-cx16");
   }
   if (hwcaps & VEX_HWCAPS_AMD64_LZCNT) {
      p = p + vex_sprintf(p, "%s", "-lzcnt");
   }
   if (hwcaps & VEX_HWCAPS_AMD64_RDTSCP) {
      p = p + vex_sprintf(p, "%s", "-rdtscp");
   }
   if (hwcaps & VEX_HWCAPS_AMD64_SSE3) {
      p = p + vex_sprintf(p, "%s", "-sse3");
   }
   if (hwcaps & VEX_HWCAPS_AMD64_AVX) {
      p = p + vex_sprintf(p, "%s", "-avx");
   }
   if (hwcaps & VEX_HWCAPS_AMD64_AVX2) {
      p = p + vex_sprintf(p, "%s", "-avx2");
   }
   if (hwcaps & VEX_HWCAPS_AMD64_BMI) {
      p = p + vex_sprintf(p, "%s", "-bmi");
   }

  out:
   vassert(buf[sizeof(buf)-1] == 0);
   return buf;
}

static const HChar* show_hwcaps_ppc32 ( UInt hwcaps )
{
   /* Monotonic with complications.  Basically V > F > baseline,
      but once you have F then you can have FX or GX too. */
   const UInt F  = VEX_HWCAPS_PPC32_F;
   const UInt V  = VEX_HWCAPS_PPC32_V;
   const UInt FX = VEX_HWCAPS_PPC32_FX;
   const UInt GX = VEX_HWCAPS_PPC32_GX;
   const UInt VX = VEX_HWCAPS_PPC32_VX;
   const UInt DFP = VEX_HWCAPS_PPC32_DFP;
   const UInt ISA2_07 = VEX_HWCAPS_PPC32_ISA2_07;
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
   if (c == (F|V|FX|GX|DFP))    return "ppc32-int-flt-vmx-FX-GX-DFP";
   if (c == (F|V|FX|GX|VX|DFP)) return "ppc32-int-flt-vmx-FX-GX-VX-DFP";
   if (c == (F|V|FX|GX|VX|DFP|ISA2_07))
      return "ppc32-int-flt-vmx-FX-GX-VX-DFP-ISA2_07";

   return NULL;
}

static const HChar* show_hwcaps_ppc64 ( UInt hwcaps )
{
   /* Monotonic with complications.  Basically V > baseline(==F),
      but once you have F then you can have FX or GX too. */
   const UInt V  = VEX_HWCAPS_PPC64_V;
   const UInt FX = VEX_HWCAPS_PPC64_FX;
   const UInt GX = VEX_HWCAPS_PPC64_GX;
   const UInt VX = VEX_HWCAPS_PPC64_VX;
   const UInt DFP = VEX_HWCAPS_PPC64_DFP;
   const UInt ISA2_07 = VEX_HWCAPS_PPC64_ISA2_07;
         UInt c  = hwcaps;
   if (c == 0)         return "ppc64-int-flt";
   if (c == FX)        return "ppc64-int-flt-FX";
   if (c == GX)        return "ppc64-int-flt-GX";
   if (c == (FX|GX))   return "ppc64-int-flt-FX-GX";
   if (c == V)         return "ppc64-int-flt-vmx";
   if (c == (V|FX))    return "ppc64-int-flt-vmx-FX";
   if (c == (V|GX))    return "ppc64-int-flt-vmx-GX";
   if (c == (V|FX|GX)) return "ppc64-int-flt-vmx-FX-GX";
   if (c == (V|FX|GX|DFP))    return "ppc64-int-flt-vmx-FX-GX-DFP";
   if (c == (V|FX|GX|VX|DFP)) return "ppc64-int-flt-vmx-FX-GX-VX-DFP";
   if (c == (V|FX|GX|VX|DFP|ISA2_07))
      return "ppc64-int-flt-vmx-FX-GX-VX-DFP-ISA2_07";
   return NULL;
}

static const HChar* show_hwcaps_arm ( UInt hwcaps )
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

static const HChar* show_hwcaps_arm64 ( UInt hwcaps )
{
   /* Since there are no variants, just insist that hwcaps is zero,
      and declare it invalid otherwise. */
  if (hwcaps == 0)
     return "baseline";
  return NULL;
}

static const HChar* show_hwcaps_s390x ( UInt hwcaps )
{
   static const HChar prefix[] = "s390x";
   static const struct {
      UInt  hwcaps_bit;
      HChar name[6];
   } hwcaps_list[] = {
      { VEX_HWCAPS_S390X_LDISP, "ldisp" },
      { VEX_HWCAPS_S390X_EIMM,  "eimm" },
      { VEX_HWCAPS_S390X_GIE,   "gie" },
      { VEX_HWCAPS_S390X_DFP,   "dfp" },
      { VEX_HWCAPS_S390X_FGX,   "fgx" },
      { VEX_HWCAPS_S390X_STFLE, "stfle" },
      { VEX_HWCAPS_S390X_ETF2,  "etf2" },
      { VEX_HWCAPS_S390X_ETF3,  "etf3" },
      { VEX_HWCAPS_S390X_STCKF, "stckf" },
      { VEX_HWCAPS_S390X_FPEXT, "fpext" },
      { VEX_HWCAPS_S390X_LSC,   "lsc" },
      { VEX_HWCAPS_S390X_PFPO,  "pfpo" },
   };
#define NUM_HWCAPS (sizeof hwcaps_list / sizeof hwcaps_list[0])
   static HChar buf[sizeof prefix + 
                    NUM_HWCAPS * (sizeof hwcaps_list[0].name + 1) +
                    1];  // '\0'
   HChar *p;
   UInt i;

   if (buf[0] != '\0') return buf;  /* already constructed */

   hwcaps = VEX_HWCAPS_S390X(hwcaps);

   p = buf + vex_sprintf(buf, "%s", prefix);
   for (i = 0 ; i < NUM_HWCAPS; ++i) {
      if (hwcaps & hwcaps_list[i].hwcaps_bit)
         p = p + vex_sprintf(p, "-%s", hwcaps_list[i].name);
   }

   /* If there are no facilities, add "zarch" */
   if (hwcaps == 0)
     vex_sprintf(p, "-%s", "zarch");

   return buf;
}

static const HChar* show_hwcaps_mips32 ( UInt hwcaps )
{
   /* MIPS baseline. */
   if (VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_MIPS) {
      /* MIPS baseline with dspr2. */
      if (VEX_MIPS_PROC_DSP2(hwcaps)) {
         return "MIPS-baseline-dspr2";
      }
      /* MIPS baseline with dsp. */
      if (VEX_MIPS_PROC_DSP(hwcaps)) {
         return "MIPS-baseline-dsp";
      }
      return "MIPS-baseline";
   }

   /* Broadcom baseline. */
   if (VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_BROADCOM) {
      return "Broadcom-baseline";
   }

   /* Netlogic baseline. */
   if (VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_NETLOGIC) {
      return "Netlogic-baseline";
   }

   /* Cavium baseline. */
   if (VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_CAVIUM) {
      return "Cavium-baseline";
   }

   return NULL;
}

static const HChar* show_hwcaps_mips64 ( UInt hwcaps )
{
   return "mips64-baseline";
}

/* ---- */
static const HChar* show_hwcaps ( VexArch arch, UInt hwcaps )
{
   switch (arch) {
      case VexArchX86:    return show_hwcaps_x86(hwcaps);
      case VexArchAMD64:  return show_hwcaps_amd64(hwcaps);
      case VexArchPPC32:  return show_hwcaps_ppc32(hwcaps);
      case VexArchPPC64:  return show_hwcaps_ppc64(hwcaps);
      case VexArchARM:    return show_hwcaps_arm(hwcaps);
      case VexArchARM64:  return show_hwcaps_arm64(hwcaps);
      case VexArchS390X:  return show_hwcaps_s390x(hwcaps);
      case VexArchMIPS32: return show_hwcaps_mips32(hwcaps);
      case VexArchMIPS64: return show_hwcaps_mips64(hwcaps);
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
