
/*--------------------------------------------------------------------*/
/*--- Arch-specific registers, etc.                  amd64/state.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn25@cam.ac.uk

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
#include "amd64_private.h"
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

void VGA_(cleanup_thread) ( ThreadArchState *arch )
{  
   /* TODO: deallocate the thread's LDT / GDT ? */
}  


void VGA_(setup_child) ( /*OUT*/ ThreadArchState *child, 
                         /*IN*/  ThreadArchState *parent )
{  
   /* We inherit our parent's guest state. */
   child->vex = parent->vex;
   child->vex_shadow = parent->vex_shadow;
#if 0
   /* TODO: inherit the thread's LDT / GDT ? */
   /* We inherit our parent's LDT. */
   if (parent->vex.guest_LDT == (HWord)NULL) {
      /* We hope this is the common case. */
      child->vex.guest_LDT = (HWord)NULL;
   } else {
      /* No luck .. we have to take a copy of the parent's. */
      child->vex.guest_LDT = (HWord)VG_(alloc_zeroed_x86_LDT)();
      copy_LDT_from_to( (VexGuestX86SegDescr*)parent->vex.guest_LDT, 
                        (VexGuestX86SegDescr*)child->vex.guest_LDT );
   }

   /* We need an empty GDT. */
   child->vex.guest_GDT = (HWord)NULL;
#endif
}  

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
/*--- Symtab stuff                                         ---*/
/*------------------------------------------------------------*/

#if 0
/* This is the Intel register encoding -- integer regs. */
#define R_EAX 0
#define R_ECX 1
#define R_EDX 2
#define R_EBX 3
#define R_ESP 4
#define R_EBP 5
#define R_ESI 6
#define R_EDI 7
#define R_E8  8
#define R_E9  9
#define R_E10 10
#define R_E11 11
#define R_E12 12
#define R_E13 13
#define R_E14 14
#define R_E15 15
#endif

UInt *VGA_(reg_addr_from_tst)(Int regno, ThreadArchState *arch)
{
   I_die_here;
#if 0
   switch (regno) {
   case R_RAX: return &arch->vex.guest_RAX;
   case R_RCX: return &arch->vex.guest_RCX;
   case R_RDX: return &arch->vex.guest_RDX;
   case R_RBX: return &arch->vex.guest_RBX;
   case R_RSP: return &arch->vex.guest_RSP;
   case R_RBP: return &arch->vex.guest_RBP;
   case R_RSI: return &arch->vex.guest_RSI;
   case R_RDI: return &arch->vex.guest_RDI;
   case R_R8 : return &arch->vex.guest_R8 ;
   case R_R9 : return &arch->vex.guest_R9 ;
   case R_R10: return &arch->vex.guest_R10;
   case R_R11: return &arch->vex.guest_R11;
   case R_R12: return &arch->vex.guest_R12;
   case R_R13: return &arch->vex.guest_R13;
   case R_R14: return &arch->vex.guest_R14;
   case R_R15: return &arch->vex.guest_R15;
   default:    return NULL;
   }
#endif
}

/*------------------------------------------------------------*/
/*--- pointercheck                                         ---*/
/*------------------------------------------------------------*/

Bool VGA_(setup_pointercheck)(void)
{
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

