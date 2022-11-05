
/*--------------------------------------------------------------------*/
/*--- Machine-related things.                   pub_core_machine.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_MACHINE_H
#define __PUB_CORE_MACHINE_H

//--------------------------------------------------------------------
// PURPOSE: This module contains code related to the particular
// architecture, things like accessing guest state, endianness, word size,
// etc.
//--------------------------------------------------------------------

#include "pub_tool_machine.h"
#include "pub_core_basics.h"      // UnwindStartRegs

// XXX: this is *really* the wrong spot for these things
#if defined(VGP_x86_linux) || defined(VGP_x86_solaris) || defined(VGP_x86_freebsd)
#  define VG_ELF_DATA2XXX     ELFDATA2LSB
#  define VG_ELF_MACHINE      EM_386
#  define VG_ELF_CLASS        ELFCLASS32
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGP_amd64_linux) || defined(VGP_amd64_solaris) || defined(VGP_amd64_freebsd)
#  define VG_ELF_DATA2XXX     ELFDATA2LSB
#  define VG_ELF_MACHINE      EM_X86_64
#  define VG_ELF_CLASS        ELFCLASS64
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGP_ppc32_linux)
#  define VG_ELF_DATA2XXX     ELFDATA2MSB
#  define VG_ELF_MACHINE      EM_PPC
#  define VG_ELF_CLASS        ELFCLASS32
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGP_ppc64be_linux)
#  define VG_ELF_DATA2XXX     ELFDATA2MSB
#  define VG_ELF_MACHINE      EM_PPC64
#  define VG_ELF_CLASS        ELFCLASS64
#  define VG_PLAT_USES_PPCTOC 1
#elif defined(VGP_ppc64le_linux)
#  define VG_ELF_DATA2XXX     ELFDATA2LSB
#  define VG_ELF_MACHINE      EM_PPC64
#  define VG_ELF_CLASS        ELFCLASS64
#  undef VG_PLAT_USES_PPCTOC
#elif defined(VGP_arm_linux)
#  define VG_ELF_DATA2XXX     ELFDATA2LSB
#  define VG_ELF_MACHINE      EM_ARM
#  define VG_ELF_CLASS        ELFCLASS32
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGP_arm64_linux)
#  define VG_ELF_DATA2XXX     ELFDATA2LSB
#  define VG_ELF_MACHINE      EM_AARCH64
#  define VG_ELF_CLASS        ELFCLASS64
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGO_darwin)
#  undef  VG_ELF_DATA2XXX
#  undef  VG_ELF_MACHINE
#  undef  VG_ELF_CLASS
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGP_s390x_linux)
#  define VG_ELF_DATA2XXX     ELFDATA2MSB
#  define VG_ELF_MACHINE      EM_S390
#  define VG_ELF_CLASS        ELFCLASS64
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGP_mips32_linux)
#  if defined (VG_LITTLEENDIAN)
#    define VG_ELF_DATA2XXX   ELFDATA2LSB
#  elif defined (VG_BIGENDIAN)
#    define VG_ELF_DATA2XXX   ELFDATA2MSB
#  else
#    error "Unknown endianness"
#  endif
#  define VG_ELF_MACHINE      EM_MIPS
#  define VG_ELF_CLASS        ELFCLASS32
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGP_mips64_linux)
#  if defined (VG_LITTLEENDIAN)
#    define VG_ELF_DATA2XXX     ELFDATA2LSB
#  elif defined (VG_BIGENDIAN)
#    define VG_ELF_DATA2XXX     ELFDATA2MSB
#  else
#    error "Unknown endianness"
#  endif
#  define VG_ELF_MACHINE      EM_MIPS
#  if defined(VGABI_N32)
#    define VG_ELF_CLASS        ELFCLASS32
#  elif defined(VGABI_64)
#    define VG_ELF_CLASS        ELFCLASS64
#  else
#    error Unknown mips64 abi
#  endif
#  undef  VG_PLAT_USES_PPCTOC
#elif defined(VGP_nanomips_linux)
#  if defined (VG_LITTLEENDIAN)
#    define VG_ELF_DATA2XXX     ELFDATA2LSB
#  elif defined (VG_BIGENDIAN)
#    define VG_ELF_DATA2XXX     ELFDATA2MSB
#  else
#    error "Unknown endianness"
#  endif
#  if !defined(EM_NANOMIPS)
#    define EM_NANOMIPS 249   /* MIPS Tech nanoMIPS */
#  endif
#  define VG_ELF_MACHINE      EM_NANOMIPS
#  define VG_ELF_CLASS        ELFCLASS32
#  undef  VG_PLAT_USES_PPCTOC
#else
#  error Unknown platform
#endif

#if defined(VGA_x86)
#  define VG_INSTR_PTR        guest_EIP
#  define VG_STACK_PTR        guest_ESP
#  define VG_FRAME_PTR        guest_EBP
#elif defined(VGA_amd64)
#  define VG_INSTR_PTR        guest_RIP
#  define VG_STACK_PTR        guest_RSP
#  define VG_FRAME_PTR        guest_RBP
#elif defined(VGA_ppc32)
#  define VG_INSTR_PTR        guest_CIA
#  define VG_STACK_PTR        guest_GPR1
#  define VG_FRAME_PTR        guest_GPR1   // No frame ptr for PPC
#elif defined(VGA_ppc64be) || defined(VGA_ppc64le)
#  define VG_INSTR_PTR        guest_CIA
#  define VG_STACK_PTR        guest_GPR1
#  define VG_FRAME_PTR        guest_GPR1   // No frame ptr for PPC
#elif defined(VGA_arm)
#  define VG_INSTR_PTR        guest_R15T
#  define VG_STACK_PTR        guest_R13
#  define VG_FRAME_PTR        guest_R11
#elif defined(VGA_arm64)
#  define VG_INSTR_PTR        guest_PC
#  define VG_STACK_PTR        guest_XSP
#  define VG_FRAME_PTR        guest_X29   // FIXME: is this right?
#elif defined(VGA_s390x)
#  define VG_INSTR_PTR        guest_IA
#  define VG_STACK_PTR        guest_SP
#  define VG_FRAME_PTR        guest_FP
#  define VG_FPC_REG          guest_fpc
#elif defined(VGA_mips32) || defined(VGA_mips64) || defined(VGA_nanomips)
#  define VG_INSTR_PTR        guest_PC
#  define VG_STACK_PTR        guest_r29
#  define VG_FRAME_PTR        guest_r30
#else
#  error Unknown arch
#endif


// Offsets for the Vex state
#define VG_O_STACK_PTR        (offsetof(VexGuestArchState, VG_STACK_PTR))
#define VG_O_INSTR_PTR        (offsetof(VexGuestArchState, VG_INSTR_PTR))
#define VG_O_FRAME_PTR        (offsetof(VexGuestArchState, VG_FRAME_PTR))
#define VG_O_FPC_REG          (offsetof(VexGuestArchState, VG_FPC_REG))


//-------------------------------------------------------------
// Guest state accessors that are not visible to tools.  The only
// ones that are visible are get_IP and get_SP.

//Addr VG_(get_IP) ( ThreadId tid );  // in pub_tool_machine.h
//Addr VG_(get_SP) ( ThreadId tid );  // in pub_tool_machine.h
Addr VG_(get_FP) ( ThreadId tid );

void VG_(set_IP) ( ThreadId tid, Addr encip );
void VG_(set_SP) ( ThreadId tid, Addr sp );


//-------------------------------------------------------------
// Get hold of the values needed for a stack unwind, for the specified
// (client) thread.
void VG_(get_UnwindStartRegs) ( /*OUT*/UnwindStartRegs* regs,
                                ThreadId tid );


//-------------------------------------------------------------
/* Details about the capabilities of the underlying (host) CPU.  These
   details are acquired by (1) enquiring with the CPU at startup, or
   (2) from the AT_SYSINFO entries the kernel gave us (ppc cache
   line size).  It's a bit nasty in the sense that there's no obvious
   way to stop uses of some of this info before it's ready to go.

   Current dependencies are:

   x86:   initially:  call VG_(machine_get_hwcaps)

          then safe to use VG_(machine_get_VexArchInfo) 
                       and VG_(machine_x86_have_mxcsr)
   -------------
   amd64: initially:  call VG_(machine_get_hwcaps)

          then safe to use VG_(machine_get_VexArchInfo) 
   -------------
   ppc32: initially:  call VG_(machine_get_hwcaps)
                      call VG_(machine_ppc32_set_clszB)

          then safe to use VG_(machine_get_VexArchInfo) 
                       and VG_(machine_ppc32_has_FP)
                       and VG_(machine_ppc32_has_VMX)
   -------------
   ppc64: initially:  call VG_(machine_get_hwcaps)
                      call VG_(machine_ppc64_set_clszB)
                      call VG_(machine_ppc64_set_scv_support)

          then safe to use VG_(machine_get_VexArchInfo) 
                       and VG_(machine_ppc64_has_VMX)
   -------------
   arm:   initially:  call VG_(machine_get_hwcaps)
                      call VG_(machine_arm_set_has_NEON)

          then safe to use VG_(machine_get_VexArchInfo) 
   -------------
   s390x: initially:  call VG_(machine_get_hwcaps)

          then safe to use VG_(machine_get_VexArchInfo)

   VG_(machine_get_hwcaps) may use signals (although it attempts to
   leave signal state unchanged) and therefore should only be
   called before m_main sets up the client's signal state.
*/

/* Determine what insn set and insn set variant the host has, and
   record it.  To be called once at system startup.  Returns False if
   this a CPU incapable of running Valgrind. */
extern Bool VG_(machine_get_hwcaps)( void );

/* Determine information about the cache system this host has and
   record it. Returns False, if cache information cannot be auto-detected. */
extern Bool VG_(machine_get_cache_info)( VexArchInfo * );

/* Notify host cpu cache line size, as per above comment. */
#if defined(VGA_ppc32)
extern void VG_(machine_ppc32_set_clszB)( Int );
#endif

#if defined(VGA_ppc64be) || defined(VGA_ppc64le)
extern void VG_(machine_ppc64_set_clszB)( Int );
extern void VG_(machine_ppc64_set_scv_support)( Int );
#endif

#if defined(VGA_arm)
extern void VG_(machine_arm_set_has_NEON)( Bool );
#endif

/* X86: set to 1 if the host is able to do {ld,st}mxcsr (load/store
   the SSE control/status register), else zero.  Is referenced from
   assembly code, so do not change from a 32-bit int. */
#if defined(VGA_x86)
extern UInt VG_(machine_x86_have_mxcsr);
#endif

/* PPC32: set to 1 if FP instructions are supported in user-space,
   else 0.  Is referenced from assembly code, so do not change from a
   32-bit int. */
#if defined(VGA_ppc32)
extern UInt VG_(machine_ppc32_has_FP);
#endif

/* PPC32: set to 1 if Altivec instructions are supported in
   user-space, else 0.  Is referenced from assembly code, so do not
   change from a 32-bit int. */
#if defined(VGA_ppc32)
extern UInt VG_(machine_ppc32_has_VMX);
#endif

/* PPC64: set to 1 if Altivec instructions are supported in
   user-space, else 0.  Is referenced from assembly code, so do not
   change from a 64-bit int. */
#if defined(VGA_ppc64be) || defined(VGA_ppc64le)
extern ULong VG_(machine_ppc64_has_VMX);
#endif

#if defined(VGA_arm)
extern Int VG_(machine_arm_archlevel);
#endif

#endif   // __PUB_CORE_MACHINE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
