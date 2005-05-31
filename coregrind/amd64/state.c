
/*--------------------------------------------------------------------*/
/*--- Arch-specific registers, etc.                  amd64/state.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "core.h"
#include "pub_core_tooliface.h"
#include <sys/ptrace.h>

#include "libvex_guest_amd64.h"


/*------------------------------------------------------------*/
/*--- Determining arch/subarch.                            ---*/
/*------------------------------------------------------------*/

// Returns the architecture and subarchitecture, or indicates
// that this subarchitecture is unable to run Valgrind
// Returns False to indicate we cannot proceed further.
Bool VGA_(getArchAndSubArch)( /*OUT*/VexArch*    vex_arch, 
                              /*OUT*/VexSubArch* vex_subarch )
{
   vg_assert(VG_(has_cpuid)());
   *vex_arch = VexArchAMD64;
   *vex_subarch = VexSubArch_NONE;
   return True;
}


/*------------------------------------------------------------*/
/*--- Initialising the first thread                        ---*/
/*------------------------------------------------------------*/

/* Given a pointer to the ThreadArchState for thread 1 (the root
   thread), initialise the VEX guest state, and copy in essential
   starting values.
*/
void VGA_(init_thread1state) ( Addr client_rip, 
                               Addr rsp_at_startup,
			       /*MOD*/ ThreadArchState* arch )
{
   vg_assert(0 == sizeof(VexGuestAMD64State) % 8);

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestAMD64_initialise(&arch->vex);

   /* Zero out the shadow area. */
   VG_(memset)(&arch->vex_shadow, 0, sizeof(VexGuestAMD64State));

   /* Put essential stuff into the new state. */
   if (0) 
      VG_(printf)("startup rsp 0x%llx  rip 0x%llx\n", 
                  rsp_at_startup, client_rip);
   arch->vex.guest_RSP = rsp_at_startup;
   arch->vex.guest_RIP = client_rip;
}


/*------------------------------------------------------------*/
/*--- Thread stuff                                         ---*/
/*------------------------------------------------------------*/

void VGA_(mark_from_registers)(ThreadId tid, void (*marker)(Addr))
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   ThreadArchState *arch = &tst->arch;

   /* XXX ask tool about validity? */
   (*marker)(arch->vex.guest_RAX);
   (*marker)(arch->vex.guest_RCX);
   (*marker)(arch->vex.guest_RDX);
   (*marker)(arch->vex.guest_RBX);
   (*marker)(arch->vex.guest_RSI);
   (*marker)(arch->vex.guest_RDI);
   (*marker)(arch->vex.guest_RSP);
   (*marker)(arch->vex.guest_RBP);
   (*marker)(arch->vex.guest_R8);
   (*marker)(arch->vex.guest_R9);
   (*marker)(arch->vex.guest_R10);
   (*marker)(arch->vex.guest_R11);
   (*marker)(arch->vex.guest_R12);
   (*marker)(arch->vex.guest_R13);
   (*marker)(arch->vex.guest_R14);
   (*marker)(arch->vex.guest_R15);
}


/*------------------------------------------------------------*/
/*--- pointercheck                                         ---*/
/*------------------------------------------------------------*/

Bool VGA_(setup_pointercheck)(Addr client_base, Addr client_end)
{
   vg_assert(0 != client_end);
   if (0) 
      VG_(message)(Vg_DebugMsg, "ignoring --pointercheck (unimplemented)");
   return True;
}

/*------------------------------------------------------------*/
/*--- Debugger-related operations                          ---*/
/*------------------------------------------------------------*/

Int VGA_(ptrace_setregs_from_tst)(Int pid, ThreadArchState* arch)
{
   I_die_here;
#if 0
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
#endif
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
