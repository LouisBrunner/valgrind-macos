
/*--------------------------------------------------------------------*/
/*--- Arch-specific registers, etc.                  amd64/state.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2004 Nicholas Nethercote
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

   arch->vex.guest_RSP = rsp_at_startup;
   arch->vex.guest_RIP = client_rip;

   // XXX: something will probably have to be done with the segment
   // registers, once they're added to Vex-AMD64.
#if 0
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
#endif
}

/*------------------------------------------------------------*/
/*--- Thread stuff                                         ---*/
/*------------------------------------------------------------*/

void VGA_(cleanup_thread) ( ThreadArchState *arch )
{  
   /* TODO: deallocate the thread's LDT / GDT ? */
}  


void VGA_(setup_child) ( ThreadArchState *arch, ThreadArchState *parent_arch )
{  
   I_die_here;
#if 0
   /* We inherit our parent's LDT. */
   if (parent_arch->ldt == NULL) {
      /* We hope this is the common case. */
      arch->ldt = NULL;
   } else {
      /* No luck .. we have to take a copy of the parent's. */
      arch->ldt = VG_(allocate_LDT_for_thread)( parent_arch->ldt );
   }

   /* Initialise the thread's TLS array */
   VG_(clear_TLS_for_thread)( arch->tls );
#endif
}  

void VGA_(set_arg_and_bogus_ret)( ThreadId tid, UWord arg, Addr ret )
{
   I_die_here;
#if 0
   /* Push the arg, and mark it as readable. */
   SET_PTHREQ_ESP(tid, VG_(threads)[tid].arch.vex.guest_ESP - sizeof(UWord));
   * (UInt*)(VG_(threads)[tid].arch.vex.guest_ESP) = arg;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid, 
             VG_(threads)[tid].arch.vex.guest_ESP, sizeof(void*) );

   /* Don't mark the pushed return address as readable; any attempt to read
      this is an internal valgrind bug since thread_exit_wrapper() should not
      return. */
   SET_PTHREQ_ESP(tid, VG_(threads)[tid].arch.vex.guest_ESP - sizeof(UWord));
   * (UInt*)(VG_(threads)[tid].arch.vex.guest_ESP) = ret;
#endif
}

void VGA_(thread_initial_stack)(ThreadId tid, UWord arg, Addr ret)
{
   I_die_here;
#if 0
   Addr esp = (Addr)STACK_PTR(VG_(threads)[tid].arch);

   /* push two args */
   esp -= 2 * sizeof(UWord);
   SET_PTHREQ_ESP(tid, esp);
   
   VG_TRACK ( new_mem_stack, esp, 2 * sizeof(UWord) );
   VG_TRACK ( pre_mem_write, Vg_CorePThread, tid, "new thread: stack",
                             esp, 2 * sizeof(UWord) );

   /* push arg and (bogus) return address */
   *(UWord*)(esp+sizeof(UWord)) = arg;
   *(UWord*)(esp)               = ret;

   VG_TRACK ( post_mem_write, Vg_CoreSignal, tid, esp, 2 * sizeof(UWord) );
#endif
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

