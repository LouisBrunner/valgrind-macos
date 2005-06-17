
/*--------------------------------------------------------------------*/
/*--- Arch-specific registers, etc.                    x86/state.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn@valgrind.org

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "core.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_tooliface.h"
#include <sys/ptrace.h>

#include "libvex_guest_x86.h"


/*------------------------------------------------------------*/
/*--- Determining arch/subarch.                            ---*/
/*------------------------------------------------------------*/

// Returns the architecture and subarchitecture, or indicates
// that this subarchitecture is unable to run Valgrind
// Returns False to indicate we cannot proceed further.
Bool VGA_(getArchAndSubArch)( /*OUT*/VexArch*    vex_arch, 
                              /*OUT*/VexSubArch* vex_subarch )
{
   Bool have_sse0, have_sse1, have_sse2;
   UInt eax, ebx, ecx, edx;

   if (!VG_(has_cpuid)())
      /* we can't do cpuid at all.  Give up. */
      return False;

   VG_(cpuid)(0, &eax, &ebx, &ecx, &edx);
   if (eax < 1)
     /* we can't ask for cpuid(x) for x > 0.  Give up. */
     return False;

   /* get capabilities bits into edx */
   VG_(cpuid)(1, &eax, &ebx, &ecx, &edx);

   have_sse0 = (edx & (1<<24)) != 0; /* True => have fxsave/fxrstor */
   have_sse1 = (edx & (1<<25)) != 0; /* True => have sse insns */
   have_sse2 = (edx & (1<<26)) != 0; /* True => have sse2 insns */

   if (have_sse2 && have_sse1 && have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse2;
      return True;
   }

   if (have_sse1 && have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse1;
      return True;
   }

   if (have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse0;
      return True;
   }

   /* we need at least SSE state to operate. */
   return False;
}


/*------------------------------------------------------------*/
/*--- Initialising the first thread                        ---*/
/*------------------------------------------------------------*/

/* Given a pointer to the ThreadArchState for thread 1 (the root
   thread), initialise the VEX guest state, and copy in essential
   starting values.
*/
void VGA_(init_thread1state) ( Addr client_eip, 
                               Addr esp_at_startup,
			       /*MOD*/ ThreadArchState* arch )
{
   vg_assert(0 == sizeof(VexGuestX86State) % 8);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestX86_initialise(&arch->vex);

   /* Zero out the shadow area. */
   VG_(memset)(&arch->vex_shadow, 0, sizeof(VexGuestX86State));

   /* Put essential stuff into the new state. */

   arch->vex.guest_ESP = esp_at_startup;
   arch->vex.guest_EIP = client_eip;

   /* initialise %cs, %ds and %ss to point at the operating systems
      default code, data and stack segments */
   asm volatile("movw %%cs, %0"
                :
                : "m" (arch->vex.guest_CS));
   asm volatile("movw %%ds, %0"
                :
                : "m" (arch->vex.guest_DS));
   asm volatile("movw %%ss, %0"
                :
                : "m" (arch->vex.guest_SS));

   VG_TRACK( post_reg_write, Vg_CoreStartup, /*tid*/1, /*offset*/0,
             sizeof(VexGuestArchState));
}

/*------------------------------------------------------------*/
/*--- Debugger-related operations                          ---*/
/*------------------------------------------------------------*/

Int VGA_(ptrace_setregs_from_tst)(Int pid, ThreadArchState* arch)
{
   struct vki_user_regs_struct regs;

   regs.cs     = arch->vex.guest_CS;
   regs.ss     = arch->vex.guest_SS;
   regs.ds     = arch->vex.guest_DS;
   regs.es     = arch->vex.guest_ES;
   regs.fs     = arch->vex.guest_FS;
   regs.gs     = arch->vex.guest_GS;
   regs.eax    = arch->vex.guest_EAX;
   regs.ebx    = arch->vex.guest_EBX;
   regs.ecx    = arch->vex.guest_ECX;
   regs.edx    = arch->vex.guest_EDX;
   regs.esi    = arch->vex.guest_ESI;
   regs.edi    = arch->vex.guest_EDI;
   regs.ebp    = arch->vex.guest_EBP;
   regs.esp    = arch->vex.guest_ESP;
   regs.eflags = LibVEX_GuestX86_get_eflags(&arch->vex);
   regs.eip    = arch->vex.guest_EIP;

   return ptrace(PTRACE_SETREGS, pid, NULL, &regs);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
