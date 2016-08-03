/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- Begin                                       main_main.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2015 OpenWorks LLP
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
#include "libvex_guest_tilegx.h"

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
#include "host_tilegx_defs.h"

#include "guest_generic_bb_to_IR.h"
#include "guest_x86_defs.h"
#include "guest_amd64_defs.h"
#include "guest_arm_defs.h"
#include "guest_arm64_defs.h"
#include "guest_ppc_defs.h"
#include "guest_s390_defs.h"
#include "guest_mips_defs.h"
#include "guest_tilegx_defs.h"

#include "host_generic_simd128.h"

/* For each architecture <arch>, we define 2 macros:
   <arch>FN that has as argument a pointer (typically to a function
            or the return value of a function).
   <arch>ST that has as argument a statement.
   If main_main.c is compiled for <arch>, then these macros just expand
   their arg.
   Otherwise, the macros expand to respectively NULL and vassert(0).
   These macros are used to avoid introducing dependencies to object
   files not needed for the (only) architecture we are compiling for. 

   To still compile the below for all supported architectures, define
   VEXMULTIARCH. This is used by the file multiarch_main_main.c */

#if defined(VGA_x86) || defined(VEXMULTIARCH)
#define X86FN(f) f
#define X86ST(f) f
#else
#define X86FN(f) NULL
#define X86ST(f) vassert(0)
#endif

#if defined(VGA_amd64) || defined(VEXMULTIARCH)
#define AMD64FN(f) f
#define AMD64ST(f) f
#else
#define AMD64FN(f) NULL
#define AMD64ST(f) vassert(0)
#endif

#if defined(VGA_ppc32) || defined(VEXMULTIARCH)
#define PPC32FN(f) f
#define PPC32ST(f) f
#else
#define PPC32FN(f) NULL
#define PPC32ST(f) vassert(0)
#endif

#if defined(VGA_ppc64be) || defined(VGA_ppc64le) || defined(VEXMULTIARCH)
#define PPC64FN(f) f
#define PPC64ST(f) f
#else
#define PPC64FN(f) NULL
#define PPC64ST(f) vassert(0)
#endif

#if defined(VGA_s390x) || defined(VEXMULTIARCH)
#define S390FN(f) f
#define S390ST(f) f
#else
#define S390FN(f) NULL
#define S390ST(f) vassert(0)
#endif

#if defined(VGA_arm) || defined(VEXMULTIARCH)
#define ARMFN(f) f
#define ARMST(f) f
#else
#define ARMFN(f) NULL
#define ARMST(f) vassert(0)
#endif

#if defined(VGA_arm64) || defined(VEXMULTIARCH)
#define ARM64FN(f) f
#define ARM64ST(f) f
#else
#define ARM64FN(f) NULL
#define ARM64ST(f) vassert(0)
#endif

#if defined(VGA_mips32) || defined(VEXMULTIARCH)
#define MIPS32FN(f) f
#define MIPS32ST(f) f
#else
#define MIPS32FN(f) NULL
#define MIPS32ST(f) vassert(0)
#endif

#if defined(VGA_mips64) || defined(VEXMULTIARCH)
#define MIPS64FN(f) f
#define MIPS64ST(f) f
#else
#define MIPS64FN(f) NULL
#define MIPS64ST(f) vassert(0)
#endif

#if defined(VGA_tilegx) || defined(VEXMULTIARCH)
#define TILEGXFN(f) f
#define TILEGXST(f) f
#else
#define TILEGXFN(f) NULL
#define TILEGXST(f) vassert(0)
#endif


/* This file contains the top level interface to the library. */

/* --------- fwds ... --------- */

static void  check_hwcaps ( VexArch arch, UInt hwcaps );
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
   vcon->iropt_verbosity                = 0;
   vcon->iropt_level                    = 2;
   vcon->iropt_register_updates_default = VexRegUpdUnwindregsAtMemAccess;
   vcon->iropt_unroll_thresh            = 120;
   vcon->guest_max_insns                = 60;
   vcon->guest_chase_thresh             = 10;
   vcon->guest_chase_cond               = False;
}


/* Exported to library client. */

void LibVEX_Init (
   /* failure exit function */
   __attribute__ ((noreturn))
   void (*failure_exit) ( void ),
   /* logging output function */
   void (*log_bytes) ( const HChar*, SizeT nbytes ),
   /* debug paranoia level */
   Int debuglevel,
   /* Control ... */
   const VexControl* vcon
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
   vassert(sizeof(void*) == sizeof(Addr));
   vassert(sizeof(unsigned long) == sizeof(SizeT));

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

   /* Ditto */
   vassert(sizeof(HReg) == 4);
   /* If N_RREGUNIVERSE_REGS ever exceeds 64, the bitset fields in
      RRegSet and HRegUsage will need to be changed to something
      better than ULong. */
   vassert(N_RREGUNIVERSE_REGS == 64);

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
   vex_control            = *vcon;
   vex_initdone           = True;
   vexSetAllocMode ( VexAllocModeTEMP );
}


/* --------- Make a translation. --------- */
/* KLUDGE: S390 need to know the hwcaps of the host when generating
   code. But that info is not passed to emit_S390Instr. Only mode64 is
   being passed. So, ideally, we want this passed as an argument, too.
   Until then, we use a global variable. This variable is set as a side
   effect of LibVEX_Translate. The variable is defined here rather than
   in host_s390_defs.c to avoid having main_main.c dragging S390
   object files in non VEXMULTIARCH. */
UInt s390_host_hwcaps;


/* Exported to library client. */

VexTranslateResult LibVEX_Translate ( VexTranslateArgs* vta )
{
   /* This the bundle of functions we need to do the back-end stuff
      (insn selection, reg-alloc, assembly) whilst being insulated
      from the target instruction set. */
   Bool         (*isMove)       ( const HInstr*, HReg*, HReg* );
   void         (*getRegUsage)  ( HRegUsage*, const HInstr*, Bool );
   void         (*mapRegs)      ( HRegRemap*, HInstr*, Bool );
   void         (*genSpill)     ( HInstr**, HInstr**, HReg, Int, Bool );
   void         (*genReload)    ( HInstr**, HInstr**, HReg, Int, Bool );
   HInstr*      (*directReload) ( HInstr*, HReg, Short );
   void         (*ppInstr)      ( const HInstr*, Bool );
   void         (*ppReg)        ( HReg );
   HInstrArray* (*iselSB)       ( const IRSB*, VexArch, const VexArchInfo*,
                                  const VexAbiInfo*, Int, Int, Bool, Bool,
                                  Addr );
   Int          (*emit)         ( /*MB_MOD*/Bool*,
                                  UChar*, Int, const HInstr*, Bool, VexEndness,
                                  const void*, const void*, const void*,
                                  const void* );
   IRExpr*      (*specHelper)   ( const HChar*, IRExpr**, IRStmt**, Int );
   Bool         (*preciseMemExnsFn) ( Int, Int, VexRegisterUpdates );

   const RRegUniverse* rRegUniv = NULL;

   DisOneInstrFn disInstrFn;

   VexGuestLayout* guest_layout;
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
   Addr            max_ga;

   guest_layout           = NULL;
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
         rRegUniv     = X86FN(getRRegUniverse_X86());
         isMove       = (__typeof__(isMove)) X86FN(isMove_X86Instr);
         getRegUsage  
            = (__typeof__(getRegUsage)) X86FN(getRegUsage_X86Instr);
         mapRegs      = (__typeof__(mapRegs)) X86FN(mapRegs_X86Instr);
         genSpill     = (__typeof__(genSpill)) X86FN(genSpill_X86);
         genReload    = (__typeof__(genReload)) X86FN(genReload_X86);
         directReload = (__typeof__(directReload)) X86FN(directReload_X86);
         ppInstr      = (__typeof__(ppInstr)) X86FN(ppX86Instr);
         ppReg        = (__typeof__(ppReg)) X86FN(ppHRegX86);
         iselSB       = X86FN(iselSB_X86);
         emit         = (__typeof__(emit)) X86FN(emit_X86Instr);
         host_word_type = Ity_I32;
         vassert(vta->archinfo_host.endness == VexEndnessLE);
         break;

      case VexArchAMD64:
         mode64       = True;
         rRegUniv     = AMD64FN(getRRegUniverse_AMD64());
         isMove       = (__typeof__(isMove)) AMD64FN(isMove_AMD64Instr);
         getRegUsage  
            = (__typeof__(getRegUsage)) AMD64FN(getRegUsage_AMD64Instr);
         mapRegs      = (__typeof__(mapRegs)) AMD64FN(mapRegs_AMD64Instr);
         genSpill     = (__typeof__(genSpill)) AMD64FN(genSpill_AMD64);
         genReload    = (__typeof__(genReload)) AMD64FN(genReload_AMD64);
         ppInstr      = (__typeof__(ppInstr)) AMD64FN(ppAMD64Instr);
         ppReg        = (__typeof__(ppReg)) AMD64FN(ppHRegAMD64);
         iselSB       = AMD64FN(iselSB_AMD64);
         emit         = (__typeof__(emit)) AMD64FN(emit_AMD64Instr);
         host_word_type = Ity_I64;
         vassert(vta->archinfo_host.endness == VexEndnessLE);
         break;

      case VexArchPPC32:
         mode64       = False;
         rRegUniv     = PPC32FN(getRRegUniverse_PPC(mode64));
         isMove       = (__typeof__(isMove)) PPC32FN(isMove_PPCInstr);
         getRegUsage  
            = (__typeof__(getRegUsage)) PPC32FN(getRegUsage_PPCInstr);
         mapRegs      = (__typeof__(mapRegs)) PPC32FN(mapRegs_PPCInstr);
         genSpill     = (__typeof__(genSpill)) PPC32FN(genSpill_PPC);
         genReload    = (__typeof__(genReload)) PPC32FN(genReload_PPC);
         ppInstr      = (__typeof__(ppInstr)) PPC32FN(ppPPCInstr);
         ppReg        = (__typeof__(ppReg)) PPC32FN(ppHRegPPC);
         iselSB       = PPC32FN(iselSB_PPC);
         emit         = (__typeof__(emit)) PPC32FN(emit_PPCInstr);
         host_word_type = Ity_I32;
         vassert(vta->archinfo_host.endness == VexEndnessBE);
         break;

      case VexArchPPC64:
         mode64       = True;
         rRegUniv     = PPC64FN(getRRegUniverse_PPC(mode64));
         isMove       = (__typeof__(isMove)) PPC64FN(isMove_PPCInstr);
         getRegUsage  
            = (__typeof__(getRegUsage)) PPC64FN(getRegUsage_PPCInstr);
         mapRegs      = (__typeof__(mapRegs)) PPC64FN(mapRegs_PPCInstr);
         genSpill     = (__typeof__(genSpill)) PPC64FN(genSpill_PPC);
         genReload    = (__typeof__(genReload)) PPC64FN(genReload_PPC);
         ppInstr      = (__typeof__(ppInstr)) PPC64FN(ppPPCInstr);
         ppReg        = (__typeof__(ppReg)) PPC64FN(ppHRegPPC);
         iselSB       = PPC64FN(iselSB_PPC);
         emit         = (__typeof__(emit)) PPC64FN(emit_PPCInstr);
         host_word_type = Ity_I64;
         vassert(vta->archinfo_host.endness == VexEndnessBE ||
                 vta->archinfo_host.endness == VexEndnessLE );
         break;

      case VexArchS390X:
         mode64       = True;
         /* KLUDGE: export hwcaps. */
         s390_host_hwcaps = vta->archinfo_host.hwcaps;
         rRegUniv     = S390FN(getRRegUniverse_S390());
         isMove       = (__typeof__(isMove)) S390FN(isMove_S390Instr);
         getRegUsage  
            = (__typeof__(getRegUsage)) S390FN(getRegUsage_S390Instr);
         mapRegs      = (__typeof__(mapRegs)) S390FN(mapRegs_S390Instr);
         genSpill     = (__typeof__(genSpill)) S390FN(genSpill_S390);
         genReload    = (__typeof__(genReload)) S390FN(genReload_S390);
         // fixs390: consider implementing directReload_S390
         ppInstr      = (__typeof__(ppInstr)) S390FN(ppS390Instr);
         ppReg        = (__typeof__(ppReg)) S390FN(ppHRegS390);
         iselSB       = S390FN(iselSB_S390);
         emit         = (__typeof__(emit)) S390FN(emit_S390Instr);
         host_word_type = Ity_I64;
         vassert(vta->archinfo_host.endness == VexEndnessBE);
         break;

      case VexArchARM:
         mode64       = False;
         rRegUniv     = ARMFN(getRRegUniverse_ARM());
         isMove       = (__typeof__(isMove)) ARMFN(isMove_ARMInstr);
         getRegUsage  
            = (__typeof__(getRegUsage)) ARMFN(getRegUsage_ARMInstr);
         mapRegs      = (__typeof__(mapRegs)) ARMFN(mapRegs_ARMInstr);
         genSpill     = (__typeof__(genSpill)) ARMFN(genSpill_ARM);
         genReload    = (__typeof__(genReload)) ARMFN(genReload_ARM);
         ppInstr      = (__typeof__(ppInstr)) ARMFN(ppARMInstr);
         ppReg        = (__typeof__(ppReg)) ARMFN(ppHRegARM);
         iselSB       = ARMFN(iselSB_ARM);
         emit         = (__typeof__(emit)) ARMFN(emit_ARMInstr);
         host_word_type = Ity_I32;
         vassert(vta->archinfo_host.endness == VexEndnessLE);
         break;

      case VexArchARM64:
         mode64       = True;
         rRegUniv     = ARM64FN(getRRegUniverse_ARM64());
         isMove       = (__typeof__(isMove)) ARM64FN(isMove_ARM64Instr);
         getRegUsage  
            = (__typeof__(getRegUsage)) ARM64FN(getRegUsage_ARM64Instr);
         mapRegs      = (__typeof__(mapRegs)) ARM64FN(mapRegs_ARM64Instr);
         genSpill     = (__typeof__(genSpill)) ARM64FN(genSpill_ARM64);
         genReload    = (__typeof__(genReload)) ARM64FN(genReload_ARM64);
         ppInstr      = (__typeof__(ppInstr)) ARM64FN(ppARM64Instr);
         ppReg        = (__typeof__(ppReg)) ARM64FN(ppHRegARM64);
         iselSB       = ARM64FN(iselSB_ARM64);
         emit         = (__typeof__(emit)) ARM64FN(emit_ARM64Instr);
         host_word_type = Ity_I64;
         vassert(vta->archinfo_host.endness == VexEndnessLE);
         break;

      case VexArchMIPS32:
         mode64       = False;
         rRegUniv     = MIPS32FN(getRRegUniverse_MIPS(mode64));
         isMove       = (__typeof__(isMove)) MIPS32FN(isMove_MIPSInstr);
         getRegUsage  
            = (__typeof__(getRegUsage)) MIPS32FN(getRegUsage_MIPSInstr);
         mapRegs      = (__typeof__(mapRegs)) MIPS32FN(mapRegs_MIPSInstr);
         genSpill     = (__typeof__(genSpill)) MIPS32FN(genSpill_MIPS);
         genReload    = (__typeof__(genReload)) MIPS32FN(genReload_MIPS);
         ppInstr      = (__typeof__(ppInstr)) MIPS32FN(ppMIPSInstr);
         ppReg        = (__typeof__(ppReg)) MIPS32FN(ppHRegMIPS);
         iselSB       = MIPS32FN(iselSB_MIPS);
         emit         = (__typeof__(emit)) MIPS32FN(emit_MIPSInstr);
         host_word_type = Ity_I32;
         vassert(vta->archinfo_host.endness == VexEndnessLE
                 || vta->archinfo_host.endness == VexEndnessBE);
         break;

      case VexArchMIPS64:
         mode64       = True;
         rRegUniv     = MIPS64FN(getRRegUniverse_MIPS(mode64));
         isMove       = (__typeof__(isMove)) MIPS64FN(isMove_MIPSInstr);
         getRegUsage  
            = (__typeof__(getRegUsage)) MIPS64FN(getRegUsage_MIPSInstr);
         mapRegs      = (__typeof__(mapRegs)) MIPS64FN(mapRegs_MIPSInstr);
         genSpill     = (__typeof__(genSpill)) MIPS64FN(genSpill_MIPS);
         genReload    = (__typeof__(genReload)) MIPS64FN(genReload_MIPS);
         ppInstr      = (__typeof__(ppInstr)) MIPS64FN(ppMIPSInstr);
         ppReg        = (__typeof__(ppReg)) MIPS64FN(ppHRegMIPS);
         iselSB       = MIPS64FN(iselSB_MIPS);
         emit         = (__typeof__(emit)) MIPS64FN(emit_MIPSInstr);
         host_word_type = Ity_I64;
         vassert(vta->archinfo_host.endness == VexEndnessLE
                 || vta->archinfo_host.endness == VexEndnessBE);
         break;

      case VexArchTILEGX:
         mode64      = True;
         rRegUniv    = TILEGXFN(getRRegUniverse_TILEGX());
         isMove      = (__typeof__(isMove)) TILEGXFN(isMove_TILEGXInstr);
         getRegUsage =
            (__typeof__(getRegUsage)) TILEGXFN(getRegUsage_TILEGXInstr);
         mapRegs     = (__typeof__(mapRegs)) TILEGXFN(mapRegs_TILEGXInstr);
         genSpill    = (__typeof__(genSpill)) TILEGXFN(genSpill_TILEGX);
         genReload   = (__typeof__(genReload)) TILEGXFN(genReload_TILEGX);
         ppInstr     = (__typeof__(ppInstr)) TILEGXFN(ppTILEGXInstr);
         ppReg       = (__typeof__(ppReg)) TILEGXFN(ppHRegTILEGX);
         iselSB      = TILEGXFN(iselSB_TILEGX);
         emit        = (__typeof__(emit)) TILEGXFN(emit_TILEGXInstr);
         host_word_type    = Ity_I64;
         vassert(vta->archinfo_host.endness == VexEndnessLE);
         break;

      default:
         vpanic("LibVEX_Translate: unsupported host insn set");
   }

   // Are the host's hardware capabilities feasible. The function will
   // not return if hwcaps are infeasible in some sense.
   check_hwcaps(vta->arch_host, vta->archinfo_host.hwcaps);

   switch (vta->arch_guest) {

      case VexArchX86:
         preciseMemExnsFn       
            = X86FN(guest_x86_state_requires_precise_mem_exns);
         disInstrFn             = X86FN(disInstr_X86);
         specHelper             = X86FN(guest_x86_spechelper);
         guest_sizeB            = sizeof(VexGuestX86State);
         guest_word_type        = Ity_I32;
         guest_layout           = X86FN(&x86guest_layout);
         offB_CMSTART           = offsetof(VexGuestX86State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestX86State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestX86State,guest_EIP);
         szB_GUEST_IP           = sizeof( ((VexGuestX86State*)0)->guest_EIP );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestX86State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestX86State,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessLE);
         vassert(0 == sizeof(VexGuestX86State) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_CMSTART) == 4);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_CMLEN  ) == 4);
         vassert(sizeof( ((VexGuestX86State*)0)->guest_NRADDR ) == 4);
         break;

      case VexArchAMD64:
         preciseMemExnsFn       
            = AMD64FN(guest_amd64_state_requires_precise_mem_exns);
         disInstrFn             = AMD64FN(disInstr_AMD64);
         specHelper             = AMD64FN(guest_amd64_spechelper);
         guest_sizeB            = sizeof(VexGuestAMD64State);
         guest_word_type        = Ity_I64;
         guest_layout           = AMD64FN(&amd64guest_layout);
         offB_CMSTART           = offsetof(VexGuestAMD64State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestAMD64State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestAMD64State,guest_RIP);
         szB_GUEST_IP           = sizeof( ((VexGuestAMD64State*)0)->guest_RIP );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestAMD64State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestAMD64State,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessLE);
         vassert(0 == sizeof(VexGuestAMD64State) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_CMSTART ) == 8);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_CMLEN   ) == 8);
         vassert(sizeof( ((VexGuestAMD64State*)0)->guest_NRADDR  ) == 8);
         break;

      case VexArchPPC32:
         preciseMemExnsFn       
            = PPC32FN(guest_ppc32_state_requires_precise_mem_exns);
         disInstrFn             = PPC32FN(disInstr_PPC);
         specHelper             = PPC32FN(guest_ppc32_spechelper);
         guest_sizeB            = sizeof(VexGuestPPC32State);
         guest_word_type        = Ity_I32;
         guest_layout           = PPC32FN(&ppc32Guest_layout);
         offB_CMSTART           = offsetof(VexGuestPPC32State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestPPC32State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestPPC32State,guest_CIA);
         szB_GUEST_IP           = sizeof( ((VexGuestPPC32State*)0)->guest_CIA );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestPPC32State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestPPC32State,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessBE);
         vassert(0 == sizeof(VexGuestPPC32State) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_CMSTART ) == 4);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_CMLEN   ) == 4);
         vassert(sizeof( ((VexGuestPPC32State*)0)->guest_NRADDR  ) == 4);
         break;

      case VexArchPPC64:
         preciseMemExnsFn       
            = PPC64FN(guest_ppc64_state_requires_precise_mem_exns);
         disInstrFn             = PPC64FN(disInstr_PPC);
         specHelper             = PPC64FN(guest_ppc64_spechelper);
         guest_sizeB            = sizeof(VexGuestPPC64State);
         guest_word_type        = Ity_I64;
         guest_layout           = PPC64FN(&ppc64Guest_layout);
         offB_CMSTART           = offsetof(VexGuestPPC64State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestPPC64State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestPPC64State,guest_CIA);
         szB_GUEST_IP           = sizeof( ((VexGuestPPC64State*)0)->guest_CIA );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestPPC64State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestPPC64State,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessBE ||
                 vta->archinfo_guest.endness == VexEndnessLE );
         vassert(0 == sizeof(VexGuestPPC64State) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_CMSTART    ) == 8);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_CMLEN      ) == 8);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_NRADDR     ) == 8);
         vassert(sizeof( ((VexGuestPPC64State*)0)->guest_NRADDR_GPR2) == 8);
         break;

      case VexArchS390X:
         preciseMemExnsFn 
            = S390FN(guest_s390x_state_requires_precise_mem_exns);
         disInstrFn       = S390FN(disInstr_S390);
         specHelper       = S390FN(guest_s390x_spechelper);
         guest_sizeB      = sizeof(VexGuestS390XState);
         guest_word_type  = Ity_I64;
         guest_layout     = S390FN(&s390xGuest_layout);
         offB_CMSTART     = offsetof(VexGuestS390XState,guest_CMSTART);
         offB_CMLEN       = offsetof(VexGuestS390XState,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestS390XState,guest_IA);
         szB_GUEST_IP           = sizeof( ((VexGuestS390XState*)0)->guest_IA);
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestS390XState,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestS390XState,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessBE);
         vassert(0 == sizeof(VexGuestS390XState) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_CMSTART    ) == 8);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_CMLEN      ) == 8);
         vassert(sizeof( ((VexGuestS390XState*)0)->guest_NRADDR     ) == 8);
         break;

      case VexArchARM:
         preciseMemExnsFn       
            = ARMFN(guest_arm_state_requires_precise_mem_exns);
         disInstrFn             = ARMFN(disInstr_ARM);
         specHelper             = ARMFN(guest_arm_spechelper);
         guest_sizeB            = sizeof(VexGuestARMState);
         guest_word_type        = Ity_I32;
         guest_layout           = ARMFN(&armGuest_layout);
         offB_CMSTART           = offsetof(VexGuestARMState,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestARMState,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestARMState,guest_R15T);
         szB_GUEST_IP           = sizeof( ((VexGuestARMState*)0)->guest_R15T );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestARMState,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestARMState,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessLE);
         vassert(0 == sizeof(VexGuestARMState) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_CMSTART) == 4);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_CMLEN  ) == 4);
         vassert(sizeof( ((VexGuestARMState*)0)->guest_NRADDR ) == 4);
         break;

      case VexArchARM64:
         preciseMemExnsFn     
            = ARM64FN(guest_arm64_state_requires_precise_mem_exns);
         disInstrFn           = ARM64FN(disInstr_ARM64);
         specHelper           = ARM64FN(guest_arm64_spechelper);
         guest_sizeB          = sizeof(VexGuestARM64State);
         guest_word_type      = Ity_I64;
         guest_layout         = ARM64FN(&arm64Guest_layout);
         offB_CMSTART         = offsetof(VexGuestARM64State,guest_CMSTART);
         offB_CMLEN           = offsetof(VexGuestARM64State,guest_CMLEN);
         offB_GUEST_IP        = offsetof(VexGuestARM64State,guest_PC);
         szB_GUEST_IP         = sizeof( ((VexGuestARM64State*)0)->guest_PC );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestARM64State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestARM64State,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessLE);
         vassert(0 == sizeof(VexGuestARM64State) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestARM64State*)0)->guest_CMSTART) == 8);
         vassert(sizeof( ((VexGuestARM64State*)0)->guest_CMLEN  ) == 8);
         vassert(sizeof( ((VexGuestARM64State*)0)->guest_NRADDR ) == 8);
         break;

      case VexArchMIPS32:
         preciseMemExnsFn       
            = MIPS32FN(guest_mips32_state_requires_precise_mem_exns);
         disInstrFn             = MIPS32FN(disInstr_MIPS);
         specHelper             = MIPS32FN(guest_mips32_spechelper);
         guest_sizeB            = sizeof(VexGuestMIPS32State);
         guest_word_type        = Ity_I32;
         guest_layout           = MIPS32FN(&mips32Guest_layout);
         offB_CMSTART           = offsetof(VexGuestMIPS32State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestMIPS32State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestMIPS32State,guest_PC);
         szB_GUEST_IP           = sizeof( ((VexGuestMIPS32State*)0)->guest_PC );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestMIPS32State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestMIPS32State,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessLE
                 || vta->archinfo_guest.endness == VexEndnessBE);
         vassert(0 == sizeof(VexGuestMIPS32State) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestMIPS32State*)0)->guest_CMSTART) == 4);
         vassert(sizeof( ((VexGuestMIPS32State*)0)->guest_CMLEN  ) == 4);
         vassert(sizeof( ((VexGuestMIPS32State*)0)->guest_NRADDR ) == 4);
         break;

      case VexArchMIPS64:
         preciseMemExnsFn       
            = MIPS64FN(guest_mips64_state_requires_precise_mem_exns);
         disInstrFn             = MIPS64FN(disInstr_MIPS);
         specHelper             = MIPS64FN(guest_mips64_spechelper);
         guest_sizeB            = sizeof(VexGuestMIPS64State);
         guest_word_type        = Ity_I64;
         guest_layout           = MIPS64FN(&mips64Guest_layout);
         offB_CMSTART           = offsetof(VexGuestMIPS64State,guest_CMSTART);
         offB_CMLEN             = offsetof(VexGuestMIPS64State,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestMIPS64State,guest_PC);
         szB_GUEST_IP           = sizeof( ((VexGuestMIPS64State*)0)->guest_PC );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestMIPS64State,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestMIPS64State,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessLE
                 || vta->archinfo_guest.endness == VexEndnessBE);
         vassert(0 == sizeof(VexGuestMIPS64State) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestMIPS64State*)0)->guest_CMSTART) == 8);
         vassert(sizeof( ((VexGuestMIPS64State*)0)->guest_CMLEN  ) == 8);
         vassert(sizeof( ((VexGuestMIPS64State*)0)->guest_NRADDR ) == 8);
         break;

      case VexArchTILEGX:
         preciseMemExnsFn =
            TILEGXFN(guest_tilegx_state_requires_precise_mem_exns);
         disInstrFn       = TILEGXFN(disInstr_TILEGX);
         specHelper       = TILEGXFN(guest_tilegx_spechelper);
         guest_sizeB      = sizeof(VexGuestTILEGXState);
         guest_word_type  = Ity_I64;
         guest_layout     = TILEGXFN(&tilegxGuest_layout);
         offB_CMSTART     = offsetof(VexGuestTILEGXState,guest_CMSTART);
         offB_CMLEN       = offsetof(VexGuestTILEGXState,guest_CMLEN);
         offB_GUEST_IP          = offsetof(VexGuestTILEGXState,guest_pc);
         szB_GUEST_IP           = sizeof( ((VexGuestTILEGXState*)0)->guest_pc );
         offB_HOST_EvC_COUNTER  = offsetof(VexGuestTILEGXState,host_EvC_COUNTER);
         offB_HOST_EvC_FAILADDR = offsetof(VexGuestTILEGXState,host_EvC_FAILADDR);
         vassert(vta->archinfo_guest.endness == VexEndnessLE);
         vassert(0 ==
                 sizeof(VexGuestTILEGXState) % LibVEX_GUEST_STATE_ALIGN);
         vassert(sizeof( ((VexGuestTILEGXState*)0)->guest_CMSTART    ) == 8);
         vassert(sizeof( ((VexGuestTILEGXState*)0)->guest_CMLEN      ) == 8);
         vassert(sizeof( ((VexGuestTILEGXState*)0)->guest_NRADDR     ) == 8);
         break;

      default:
         vpanic("LibVEX_Translate: unsupported guest insn set");
   }

   // Are the guest's hardware capabilities feasible. The function will
   // not return if hwcaps are infeasible in some sense.
   // FIXME: how can we know the guest's hardware capabilities?
   check_hwcaps(vta->arch_guest, vta->archinfo_guest.hwcaps);

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
      /* ditto */
      vassert(vta->archinfo_guest.endness == vta->archinfo_host.endness);
   }

   vexAllocSanityCheck();

   if (vex_traceflags & VEX_TRACE_FE)
      vex_printf("\n------------------------" 
                   " Front end "
                   "------------------------\n\n");

   VexRegisterUpdates pxControl = vex_control.iropt_register_updates_default;
   vassert(pxControl >= VexRegUpdSpAtMemAccess
           && pxControl <= VexRegUpdAllregsAtEachInsn);

   irsb = bb_to_IR ( vta->guest_extents,
                     &res.n_sc_extents,
                     &res.n_guest_instrs,
                     &pxControl,
                     vta->callback_opaque,
                     disInstrFn,
                     vta->guest_bytes, 
                     vta->guest_bytes_addr,
                     vta->chase_into_ok,
                     vta->archinfo_host.endness,
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

   /* bb_to_IR() could have caused pxControl to change. */
   vassert(pxControl >= VexRegUpdSpAtMemAccess
           && pxControl <= VexRegUpdAllregsAtEachInsn);

   /* If debugging, show the raw guest bytes for this bb. */
   if (0 || (vex_traceflags & VEX_TRACE_FE)) {
      if (vta->guest_extents->n_used > 1) {
         vex_printf("can't show code due to extents > 1\n");
      } else {
         /* HACK */
         const UChar* p = vta->guest_bytes;
         UInt   sum = 0;
         UInt   guest_bytes_read = (UInt)vta->guest_extents->len[0];
         vex_printf("GuestBytes %lx %u ", vta->guest_bytes_addr, 
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
   irsb = do_iropt_BB ( irsb, specHelper, preciseMemExnsFn, pxControl,
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
   max_ga = ado_treebuild_BB( irsb, preciseMemExnsFn, pxControl );

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
   rcode = doRegisterAllocation ( vcode, rRegUniv,
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
                insn_bytes, sizeof insn_bytes, hi,
                mode64, vta->archinfo_host.endness,
                vta->disp_cp_chain_me_to_slowEP,
                vta->disp_cp_chain_me_to_fastEP,
                vta->disp_cp_xindir,
                vta->disp_cp_xassisted );
      if (UNLIKELY(vex_traceflags & VEX_TRACE_ASM)) {
         for (k = 0; k < j; k++)
            vex_printf("%02x ", (UInt)insn_bytes[k]);
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

VexInvalRange LibVEX_Chain ( VexArch     arch_host,
                             VexEndness  endness_host,
                             void*       place_to_chain,
                             const void* disp_cp_chain_me_EXPECTED,
                             const void* place_to_jump_to )
{
   switch (arch_host) {
      case VexArchX86:
         X86ST(return chainXDirect_X86(endness_host,
                                       place_to_chain,
                                       disp_cp_chain_me_EXPECTED,
                                       place_to_jump_to));
      case VexArchAMD64:
         AMD64ST(return chainXDirect_AMD64(endness_host,
                                           place_to_chain,
                                           disp_cp_chain_me_EXPECTED,
                                           place_to_jump_to));
      case VexArchARM:
         ARMST(return chainXDirect_ARM(endness_host,
                                       place_to_chain,
                                       disp_cp_chain_me_EXPECTED,
                                       place_to_jump_to));
      case VexArchARM64:
         ARM64ST(return chainXDirect_ARM64(endness_host,
                                           place_to_chain,
                                           disp_cp_chain_me_EXPECTED,
                                           place_to_jump_to));
      case VexArchS390X:
         S390ST(return chainXDirect_S390(endness_host,
                                         place_to_chain,
                                         disp_cp_chain_me_EXPECTED,
                                         place_to_jump_to));
      case VexArchPPC32:
         PPC32ST(return chainXDirect_PPC(endness_host,
                                         place_to_chain,
                                         disp_cp_chain_me_EXPECTED,
                                         place_to_jump_to, False/*!mode64*/));
      case VexArchPPC64:
         PPC64ST(return chainXDirect_PPC(endness_host,
                                         place_to_chain,
                                         disp_cp_chain_me_EXPECTED,
                                         place_to_jump_to, True/*mode64*/));
      case VexArchMIPS32:
         MIPS32ST(return chainXDirect_MIPS(endness_host,
                                           place_to_chain,
                                           disp_cp_chain_me_EXPECTED,
                                           place_to_jump_to, False/*!mode64*/));
      case VexArchMIPS64:
         MIPS64ST(return chainXDirect_MIPS(endness_host,
                                           place_to_chain,
                                           disp_cp_chain_me_EXPECTED,
                                           place_to_jump_to, True/*!mode64*/));

      case VexArchTILEGX:
         TILEGXST(return chainXDirect_TILEGX(endness_host,
                                             place_to_chain,
                                             disp_cp_chain_me_EXPECTED,
                                             place_to_jump_to, True/*!mode64*/));
      default:
         vassert(0);
   }
}

VexInvalRange LibVEX_UnChain ( VexArch     arch_host,
                               VexEndness  endness_host,
                               void*       place_to_unchain,
                               const void* place_to_jump_to_EXPECTED,
                               const void* disp_cp_chain_me )
{
   switch (arch_host) {
      case VexArchX86:
         X86ST(return unchainXDirect_X86(endness_host,
                                         place_to_unchain,
                                         place_to_jump_to_EXPECTED,
                                         disp_cp_chain_me));
      case VexArchAMD64:
         AMD64ST(return unchainXDirect_AMD64(endness_host,
                                             place_to_unchain,
                                             place_to_jump_to_EXPECTED,
                                             disp_cp_chain_me));
      case VexArchARM:
         ARMST(return unchainXDirect_ARM(endness_host,
                                         place_to_unchain,
                                         place_to_jump_to_EXPECTED,
                                         disp_cp_chain_me));
      case VexArchARM64:
         ARM64ST(return unchainXDirect_ARM64(endness_host,
                                             place_to_unchain,
                                             place_to_jump_to_EXPECTED,
                                             disp_cp_chain_me));
      case VexArchS390X:
         S390ST(return unchainXDirect_S390(endness_host,
                                           place_to_unchain,
                                           place_to_jump_to_EXPECTED,
                                           disp_cp_chain_me));
      case VexArchPPC32:
         PPC32ST(return unchainXDirect_PPC(endness_host,
                                           place_to_unchain,
                                           place_to_jump_to_EXPECTED,
                                           disp_cp_chain_me, False/*!mode64*/));
      case VexArchPPC64:
         PPC64ST(return unchainXDirect_PPC(endness_host,
                                           place_to_unchain,
                                           place_to_jump_to_EXPECTED,
                                           disp_cp_chain_me, True/*mode64*/));
      case VexArchMIPS32:
         MIPS32ST(return unchainXDirect_MIPS(endness_host,
                                             place_to_unchain,
                                             place_to_jump_to_EXPECTED,
                                             disp_cp_chain_me, False/*!mode64*/));
      case VexArchMIPS64:
         MIPS64ST(return unchainXDirect_MIPS(endness_host,
                                             place_to_unchain,
                                             place_to_jump_to_EXPECTED,
                                             disp_cp_chain_me, True/*!mode64*/));

      case VexArchTILEGX:
         TILEGXST(return unchainXDirect_TILEGX(endness_host,
                                      place_to_unchain,
                                      place_to_jump_to_EXPECTED,
                                               disp_cp_chain_me, True/*!mode64*/));

      default:
         vassert(0);
   }
}

Int LibVEX_evCheckSzB ( VexArch    arch_host )
{
   static Int cached = 0; /* DO NOT MAKE NON-STATIC */
   if (UNLIKELY(cached == 0)) {
      switch (arch_host) {
         case VexArchX86:
            X86ST(cached = evCheckSzB_X86()); break;
         case VexArchAMD64:
            AMD64ST(cached = evCheckSzB_AMD64()); break;
         case VexArchARM:
            ARMST(cached = evCheckSzB_ARM()); break;
         case VexArchARM64:
            ARM64ST(cached = evCheckSzB_ARM64()); break;
         case VexArchS390X:
            S390ST(cached = evCheckSzB_S390()); break;
         case VexArchPPC32:
            PPC32ST(cached = evCheckSzB_PPC()); break;
         case VexArchPPC64:
            PPC64ST(cached = evCheckSzB_PPC()); break;
         case VexArchMIPS32:
            MIPS32ST(cached = evCheckSzB_MIPS()); break;
         case VexArchMIPS64:
            MIPS64ST(cached = evCheckSzB_MIPS()); break;
         case VexArchTILEGX:
            TILEGXST(cached = evCheckSzB_TILEGX()); break;
         default:
            vassert(0);
      }
   }
   return cached;
}

VexInvalRange LibVEX_PatchProfInc ( VexArch    arch_host,
                                    VexEndness endness_host,
                                    void*      place_to_patch,
                                    const ULong* location_of_counter )
{
   switch (arch_host) {
      case VexArchX86:
         X86ST(return patchProfInc_X86(endness_host, place_to_patch,
                                       location_of_counter));
      case VexArchAMD64:
         AMD64ST(return patchProfInc_AMD64(endness_host, place_to_patch,
                                           location_of_counter));
      case VexArchARM:
         ARMST(return patchProfInc_ARM(endness_host, place_to_patch,
                                       location_of_counter));
      case VexArchARM64:
         ARM64ST(return patchProfInc_ARM64(endness_host, place_to_patch,
                                           location_of_counter));
      case VexArchS390X:
         S390ST(return patchProfInc_S390(endness_host, place_to_patch,
                                         location_of_counter));
      case VexArchPPC32:
         PPC32ST(return patchProfInc_PPC(endness_host, place_to_patch,
                                         location_of_counter, False/*!mode64*/));
      case VexArchPPC64:
         PPC64ST(return patchProfInc_PPC(endness_host, place_to_patch,
                                         location_of_counter, True/*mode64*/));
      case VexArchMIPS32:
         MIPS32ST(return patchProfInc_MIPS(endness_host, place_to_patch,
                                           location_of_counter, False/*!mode64*/));
      case VexArchMIPS64:
         MIPS64ST(return patchProfInc_MIPS(endness_host, place_to_patch,
                                           location_of_counter, True/*!mode64*/));
      case VexArchTILEGX:
         TILEGXST(return patchProfInc_TILEGX(endness_host, place_to_patch,
                                             location_of_counter,
                                             True/*!mode64*/));
      default:
         vassert(0);
   }
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
               "  feature requires the floating point extension facility\n"
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
     case EmFail_S390X_pfpo:
        return "Instruction pfpo is not supported on this host";
     case EmFail_S390X_DFP_insn:
        return "DFP instructions are not supported on this host";
     case EmFail_S390X_fpext:
        return "Encountered an instruction that requires the floating "
               "point extension facility.\n"
               "  That facility is not available on this host";
     case EmFail_S390X_invalid_PFPO_rounding_mode:
        return "The rounding mode in GPR 0 for the PFPO instruction"
               " is invalid";
     case EmFail_S390X_invalid_PFPO_function:
        return "The function code in GPR 0 for the PFPO instruction"
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
      case VexArchTILEGX:   return "TILEGX";
      default:              return "VexArch???";
   }
}

const HChar* LibVEX_ppVexEndness ( VexEndness endness )
{
   switch (endness) {
      case VexEndness_INVALID: return "INVALID";
      case VexEndnessLE:       return "LittleEndian";
      case VexEndnessBE:       return "BigEndian";
      default:                 return "VexEndness???";
   }
}

/* Return a string with the hardware capabilities to the extent as
   they pertain to the translation process. No attempt is made, to
   detect *all* capabilities an architecture may have. */
const HChar* LibVEX_ppVexHwCaps ( VexArch arch, UInt hwcaps )
{
   return show_hwcaps(arch, hwcaps);
}


/* Write default settings info *vai. */
void LibVEX_default_VexArchInfo ( /*OUT*/VexArchInfo* vai )
{
   vex_bzero(vai, sizeof(*vai));
   vai->hwcaps                  = 0;
   vai->endness                 = VexEndness_INVALID;
   vai->ppc_icache_line_szB     = 0;
   vai->ppc_dcbz_szB            = 0;
   vai->ppc_dcbzl_szB           = 0;
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
   vbi->guest_amd64_assume_fs_is_const = False;
   vbi->guest_amd64_assume_gs_is_const = False;
   vbi->guest_ppc_zap_RZ_at_blr        = False;
   vbi->guest_ppc_zap_RZ_at_bl         = NULL;
   vbi->host_ppc_calls_use_fndescrs    = False;
}


/* Convenience macro to be used in show_hwcaps_ARCH functions */
#define NUM_HWCAPS (sizeof hwcaps_list / sizeof hwcaps_list[0])

/* Return a string showing the hwcaps in a nice way.  The string will
   be NULL for unrecognised hardware capabilities. */

static const HChar* show_hwcaps_x86 ( UInt hwcaps ) 
{
   static const HChar prefix[] = "x86";
   static const struct {
      UInt  hwcaps_bit;
      HChar name[7];
   } hwcaps_list[] = {
      { VEX_HWCAPS_X86_MMXEXT, "mmxext" },
      { VEX_HWCAPS_X86_SSE1,   "sse1"   },
      { VEX_HWCAPS_X86_SSE2,   "sse2"   },
      { VEX_HWCAPS_X86_SSE3,   "sse3"   },
      { VEX_HWCAPS_X86_LZCNT,  "lzcnt"  },
   };
   /* Allocate a large enough buffer */
   static HChar buf[sizeof prefix + 
                    NUM_HWCAPS * (sizeof hwcaps_list[0].name + 1) + 1]; // '\0'
   if (buf[0] != '\0') return buf;  /* already constructed */

   HChar *p = buf + vex_sprintf(buf, "%s", prefix);

   if (hwcaps == 0) {
      vex_sprintf(p, "-%s", "sse0");
   } else {      
      UInt i;
      for (i = 0 ; i < NUM_HWCAPS; ++i) {
         if (hwcaps & hwcaps_list[i].hwcaps_bit)
            p = p + vex_sprintf(p, "-%s", hwcaps_list[i].name);
      }
   }
   return buf;
}

static const HChar* show_hwcaps_amd64 ( UInt hwcaps )
{
   static const HChar prefix[] = "amd64";
   static const struct {
      UInt  hwcaps_bit;
      HChar name[7];
   } hwcaps_list[] = {
      { VEX_HWCAPS_AMD64_CX16,   "cx16"   },
      { VEX_HWCAPS_AMD64_LZCNT,  "lzcnt"  },
      { VEX_HWCAPS_AMD64_RDTSCP, "rdtscp" },
      { VEX_HWCAPS_AMD64_SSE3,   "sse3"   },
      { VEX_HWCAPS_AMD64_AVX,    "avx"    },
      { VEX_HWCAPS_AMD64_AVX2,   "avx2"   },
      { VEX_HWCAPS_AMD64_BMI,    "bmi"    },
   };
   /* Allocate a large enough buffer */
   static HChar buf[sizeof prefix + 
                    NUM_HWCAPS * (sizeof hwcaps_list[0].name + 1) + 1]; // '\0'
   if (buf[0] != '\0') return buf;  /* already constructed */

   HChar *p = buf + vex_sprintf(buf, "%s", prefix);

   if (hwcaps == 0) {
      vex_sprintf(p, "-%s", "sse2");
   } else {      
      UInt i;
      for (i = 0 ; i < NUM_HWCAPS; ++i) {
         if (hwcaps & hwcaps_list[i].hwcaps_bit)
            p = p + vex_sprintf(p, "-%s", hwcaps_list[i].name);
      }
   }
   return buf;
}

static const HChar* show_hwcaps_ppc32 ( UInt hwcaps )
{
   static const HChar prefix[] = "ppc32-int";
   static const struct {
      UInt  hwcaps_bit;
      HChar name[8];
   } hwcaps_list[] = {
      { VEX_HWCAPS_PPC32_F,       "flt"     },
      { VEX_HWCAPS_PPC32_V,       "vmx"     },
      { VEX_HWCAPS_PPC32_FX,      "FX"      },
      { VEX_HWCAPS_PPC32_GX,      "GX"      },
      { VEX_HWCAPS_PPC32_VX,      "VX"      },
      { VEX_HWCAPS_PPC32_DFP,     "DFP"     },
      { VEX_HWCAPS_PPC32_ISA2_07, "ISA2_07" },
      { VEX_HWCAPS_PPC32_ISA3_0,  "ISA3_0"  },
   };
   /* Allocate a large enough buffer */
   static HChar buf[sizeof prefix + 
                    NUM_HWCAPS * (sizeof hwcaps_list[0].name + 1) + 1]; // '\0'
   if (buf[0] != '\0') return buf;  /* already constructed */

   HChar *p = buf + vex_sprintf(buf, "%s", prefix);

   if (hwcaps == 0) return buf;

   UInt i;
   for (i = 0 ; i < NUM_HWCAPS; ++i) {
      if (hwcaps & hwcaps_list[i].hwcaps_bit)
         p = p + vex_sprintf(p, "-%s", hwcaps_list[i].name);
   }
   return buf;
}

static const HChar* show_hwcaps_ppc64 ( UInt hwcaps )
{
   static const HChar prefix[] = "ppc64-int-flt";
   static const struct {
      UInt  hwcaps_bit;
      HChar name[8];
   } hwcaps_list[] = {
      { VEX_HWCAPS_PPC64_FX,      "FX"      },
      { VEX_HWCAPS_PPC64_GX,      "GX"      },
      { VEX_HWCAPS_PPC64_V,       "vmx"     },
      { VEX_HWCAPS_PPC64_DFP,     "DFP"     },
      { VEX_HWCAPS_PPC64_ISA2_07, "ISA2_07" },
      { VEX_HWCAPS_PPC64_ISA3_0,  "ISA3_0"  },
   };
   /* Allocate a large enough buffer */
   static HChar buf[sizeof prefix + 
                    NUM_HWCAPS * (sizeof hwcaps_list[0].name + 1) + 1]; // '\0'
   if (buf[0] != '\0') return buf;  /* already constructed */

   HChar *p = buf + vex_sprintf(buf, "%s", prefix);

   if (hwcaps == 0) return buf;

   UInt i;
   for (i = 0 ; i < NUM_HWCAPS; ++i) {
      if (hwcaps & hwcaps_list[i].hwcaps_bit)
         p = p + vex_sprintf(p, "-%s", hwcaps_list[i].name);
   }
   return buf;
}

static const HChar* show_hwcaps_arm ( UInt hwcaps )
{
   static const HChar prefix[] = "ARM";
   static const struct {
      UInt  hwcaps_bit;
      HChar name[6];
   } hwcaps_list[] = {
      { VEX_HWCAPS_ARM_NEON, "neon" },
      { VEX_HWCAPS_ARM_VFP | VEX_HWCAPS_ARM_VFP2 | VEX_HWCAPS_ARM_VFP3, "vfp" },
   };
   /* Allocate a large enough buffer */
   static HChar buf[sizeof prefix + 12 +    // level
                    NUM_HWCAPS * (sizeof hwcaps_list[0].name + 1) + 1]; // '\0'
   if (buf[0] != '\0') return buf;  /* already constructed */

   HChar *p;
   UInt i, level;

   level = VEX_ARM_ARCHLEVEL(hwcaps);

   p = buf + vex_sprintf(buf, "%sv%u", prefix, level);
   for (i = 0 ; i < NUM_HWCAPS; ++i) {
      if (hwcaps & hwcaps_list[i].hwcaps_bit)
         p = p + vex_sprintf(p, "-%s", hwcaps_list[i].name);
   }
   return buf;
}

static const HChar* show_hwcaps_arm64 ( UInt hwcaps )
{
   /* Since there are no variants, just insist that hwcaps is zero,
      and declare it invalid otherwise. */
  if (hwcaps == 0)
     return "baseline";
  return "Unsupported";
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
   /* Allocate a large enough buffer */
   static HChar buf[sizeof prefix + 
                    NUM_HWCAPS * (sizeof hwcaps_list[0].name + 1) + 1]; // '\0'

   if (buf[0] != '\0') return buf;  /* already constructed */

   HChar *p;
   UInt i;

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

   /* Ingenic baseline. */
   if (VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_INGENIC_E1) {
      return "Ingenic-baseline";
   }

   /* Loongson baseline. */
   if ((VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_LEGACY) &&
       (VEX_MIPS_PROC_ID(hwcaps) == VEX_PRID_IMP_LOONGSON_64)) {
      return "Loongson-baseline";
   }

   return "Unsupported baseline";
}

static const HChar* show_hwcaps_mips64 ( UInt hwcaps )
{
   /* Netlogic baseline. */
   if (VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_NETLOGIC) {
      return "Netlogic-baseline";
   }

   /* Cavium baseline. */
   if (VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_CAVIUM) {
      return "Cavium-baseline";
   }

   /* Loongson baseline. */
   if ((VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_LEGACY) &&
       (VEX_MIPS_PROC_ID(hwcaps) == VEX_PRID_IMP_LOONGSON_64)) {
      return "Loongson-baseline";
   }

   /* MIPS64 baseline. */
   if (VEX_MIPS_COMP_ID(hwcaps) == VEX_PRID_COMP_MIPS) {
      return "mips64-baseline";
   }

   return "Unsupported baseline";
}

static const HChar* show_hwcaps_tilegx ( UInt hwcaps )
{
   return "tilegx-baseline";
}

#undef NUM_HWCAPS

/* Thie function must not return NULL. */

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
      case VexArchTILEGX: return show_hwcaps_tilegx(hwcaps);
      default: return NULL;
   }
}

/* To be used to complain about hwcaps we cannot handle */
__attribute__((noreturn))
static void invalid_hwcaps ( VexArch arch, UInt hwcaps, const HChar *message )
{
   vfatal("\nVEX: %s"
          "     Found: %s\n", message, show_hwcaps(arch, hwcaps));
}

/* This function will not return iff the hwcaps don't pass the test. */
static void check_hwcaps ( VexArch arch, UInt hwcaps )
{
   switch (arch) {
      case VexArchX86: {
         if (hwcaps == 0) return;    // baseline

         /* Monotonic: SSE3 > SSE2 > SSE1 > MMXEXT > baseline. */
         static const UInt extras[] = {
            VEX_HWCAPS_X86_MMXEXT, VEX_HWCAPS_X86_SSE1, VEX_HWCAPS_X86_SSE2,
            VEX_HWCAPS_X86_SSE3
         };

         UInt i, caps = 0;
         for (i = 0; i < sizeof extras / sizeof extras[0]; ++i) {
            caps |= extras[i];
            if (caps == hwcaps) return;
            /* For SSE2 or later LZCNT is optional */
            if ((caps & VEX_HWCAPS_X86_SSE2) != 0) {
               if ((caps | VEX_HWCAPS_X86_LZCNT) == hwcaps) return;
            }
         }
         invalid_hwcaps(arch, hwcaps, "Cannot handle capabilities\n");
      }

      case VexArchAMD64: {
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
            invalid_hwcaps(arch, hwcaps,
                           "Support for AVX requires SSE3 capabilities\n");
         /* AVX2 or BMI without AVX */
         if (have_avx2 && !have_avx)
            invalid_hwcaps(arch, hwcaps,
                           "Support for AVX2 requires AVX capabilities\n");
         if (have_bmi && !have_avx)
            invalid_hwcaps(arch, hwcaps,
                           "Support for BMI requires AVX capabilities\n");
         return;
      }

      case VexArchPPC32: {
         /* Monotonic with complications.  Basically V > F > baseline,
            but once you have F then you can have FX or GX too. */
         if (hwcaps == 0) return;   // baseline

         if ((hwcaps & VEX_HWCAPS_PPC32_F) == 0)
            invalid_hwcaps(arch, hwcaps,
                           "Missing floating point capability\n");
         /* V, FX, and GX can appear in any combination */

         /* DFP requires V and FX and GX */
         UInt v_fx_gx = VEX_HWCAPS_PPC32_V | VEX_HWCAPS_PPC32_FX |
                        VEX_HWCAPS_PPC32_GX;
         Bool has_v_fx_gx = (hwcaps & v_fx_gx) == v_fx_gx;

         if ((hwcaps & VEX_HWCAPS_PPC32_DFP) && ! has_v_fx_gx)
            invalid_hwcaps(arch, hwcaps,
                           "DFP requires VMX and FX and GX capabilities\n");

         /* VX requires V and FX and GX */
         if ((hwcaps & VEX_HWCAPS_PPC32_VX) && ! has_v_fx_gx)
            invalid_hwcaps(arch, hwcaps,
                           "VX requires VMX and FX and GX capabilities\n");

         /* ISA2_07 requires everything else */
         if ((hwcaps & VEX_HWCAPS_PPC32_ISA2_07) != 0) {
            if (! has_v_fx_gx)
               invalid_hwcaps(arch, hwcaps,
                          "ISA2_07 requires VMX and FX and GX capabilities\n");
            if (! (hwcaps & VEX_HWCAPS_PPC32_VX))
               invalid_hwcaps(arch, hwcaps,
                              "ISA2_07 requires VX capabilities\n");
            if (! (hwcaps & VEX_HWCAPS_PPC32_DFP))
               invalid_hwcaps(arch, hwcaps,
                              "ISA2_07 requires DFP capabilities\n");
         }

         /* ISA 3.0 not supported on 32-bit machines */
         if ((hwcaps & VEX_HWCAPS_PPC32_ISA3_0) != 0) {
            invalid_hwcaps(arch, hwcaps,
                           "ISA 3.0 not supported in 32-bit mode \n");
         }
         return;
      }

      case VexArchPPC64: {
         /* Monotonic with complications.  Basically V > baseline(==F),
            but once you have F then you can have FX or GX too. */
         if (hwcaps == 0) return;   // baseline

         /* V, FX, and GX can appear in any combination */

         /* DFP requires V and FX and GX */
         UInt v_fx_gx = VEX_HWCAPS_PPC64_V | VEX_HWCAPS_PPC64_FX |
                        VEX_HWCAPS_PPC64_GX;
         Bool has_v_fx_gx = (hwcaps & v_fx_gx) == v_fx_gx;

         if ((hwcaps & VEX_HWCAPS_PPC64_DFP) && ! has_v_fx_gx)
            invalid_hwcaps(arch, hwcaps,
                           "DFP requires VMX and FX and GX capabilities\n");

         /* VX requires V and FX and GX */
         if ((hwcaps & VEX_HWCAPS_PPC32_VX) && ! has_v_fx_gx)
            invalid_hwcaps(arch, hwcaps,
                           "VX requires VMX and FX and GX capabilities\n");

         /* ISA2_07 requires everything else */
         if ((hwcaps & VEX_HWCAPS_PPC64_ISA2_07) != 0) {
            if (! has_v_fx_gx)
               invalid_hwcaps(arch, hwcaps,
                        "ISA2_07 requires VMX and FX and GX capabilities\n");
            if (! (hwcaps & VEX_HWCAPS_PPC64_VX))
               invalid_hwcaps(arch, hwcaps,
                              "ISA2_07 requires VX capabilities\n");
            if (! (hwcaps & VEX_HWCAPS_PPC64_DFP))
               invalid_hwcaps(arch, hwcaps,
                              "ISA2_07 requires DFP capabilities\n");
         }

         /* ISA3_0 requires everything else */
         if ((hwcaps & VEX_HWCAPS_PPC64_ISA3_0) != 0) {
            if ( !((hwcaps
                    & VEX_HWCAPS_PPC64_ISA2_07) == VEX_HWCAPS_PPC64_ISA2_07))
               invalid_hwcaps(arch, hwcaps,
                          "ISA3_0 requires ISA2_07 capabilities\n");
            if ( !has_v_fx_gx)
               invalid_hwcaps(arch, hwcaps,
                        "ISA3_0 requires VMX and FX and GX capabilities\n");
            if ( !(hwcaps & VEX_HWCAPS_PPC64_VX))
               invalid_hwcaps(arch, hwcaps,
                              "ISA3_0 requires VX capabilities\n");
            if ( !(hwcaps & VEX_HWCAPS_PPC64_DFP))
               invalid_hwcaps(arch, hwcaps,
                              "ISA3_0 requires DFP capabilities\n");
         }
         return;
      }

      case VexArchARM: {
         Bool NEON  = ((hwcaps & VEX_HWCAPS_ARM_NEON) != 0);
         Bool VFP3  = ((hwcaps & VEX_HWCAPS_ARM_VFP3) != 0);
         UInt level = VEX_ARM_ARCHLEVEL(hwcaps);
         switch (level) {
            case 5:
               if (NEON)
                  invalid_hwcaps(arch, hwcaps,
                          "NEON instructions are not supported for ARMv5.\n");
               return;
            case 6:
               if (NEON)
                  invalid_hwcaps(arch, hwcaps,
                          "NEON instructions are not supported for ARMv6.\n");
               return;
            case 7:
               return;
            case 8:
               if (!NEON || !VFP3)
                  invalid_hwcaps(arch, hwcaps,
                          "NEON and VFP3 are required for ARMv8.\n");
               return;
            default:
               invalid_hwcaps(arch, hwcaps,
                              "ARM architecture level is not supported.\n");
         }
      }

      case VexArchARM64:
         if (hwcaps != 0)
            invalid_hwcaps(arch, hwcaps,
                           "Unsupported hardware capabilities.\n");
         return;

      case VexArchS390X:
         if (! s390_host_has_ldisp)
            invalid_hwcaps(arch, hwcaps,
                           "Host does not have long displacement facility.\n");
         return;

      case VexArchMIPS32:
         switch (VEX_MIPS_COMP_ID(hwcaps)) {
            case VEX_PRID_COMP_MIPS:
            case VEX_PRID_COMP_CAVIUM:
            case VEX_PRID_COMP_INGENIC_E1:
            case VEX_PRID_COMP_BROADCOM:
            case VEX_PRID_COMP_NETLOGIC:
               return;
            default:
               invalid_hwcaps(arch, hwcaps, "Unsupported baseline\n");
         }

      case VexArchMIPS64:
         switch (VEX_MIPS_COMP_ID(hwcaps)) {
            case VEX_PRID_COMP_MIPS:
            case VEX_PRID_COMP_CAVIUM:
            case VEX_PRID_COMP_NETLOGIC:
               return;
            default:
               invalid_hwcaps(arch, hwcaps, "Unsupported baseline\n");
         }

      case VexArchTILEGX:
         return;

      default:
         vpanic("unknown architecture");
   }
}


/*---------------------------------------------------------------*/
/*--- end                                         main_main.c ---*/
/*---------------------------------------------------------------*/
