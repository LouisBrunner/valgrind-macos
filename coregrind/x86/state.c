
/*--------------------------------------------------------------------*/
/*--- Arch-specific registers, etc.                    x86/state.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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
#include "x86_private.h"
#include <sys/ptrace.h>

#include "libvex_guest_x86.h"

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
   /* initialise %cs, %ds and %ss to point at the operating systems
      default code, data and stack segments */
   arch->vex.guest_ESP = esp_at_startup;
   arch->vex.guest_EIP = client_eip;

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

   /* I assume that if we have SSE2 we also have SSE */
   VG_(have_ssestate) = False;
   //      VG_(cpu_has_feature)(VG_X86_FEAT_FXSR) &&
   //   VG_(cpu_has_feature)(VG_X86_FEAT_SSE);

   if (0) {
      if (VG_(have_ssestate))
         VG_(printf)("Looks like a SSE-capable CPU\n");
      else
         VG_(printf)("Looks like a MMX-only CPU\n");
   }
}


/*------------------------------------------------------------*/
/*--- Thread stuff                                         ---*/
/*------------------------------------------------------------*/

void VGA_(clear_thread)( ThreadArchState *arch )
{
   arch->ldt = NULL;
   VG_(clear_TLS_for_thread)(arch->tls);
}  

void VGA_(cleanup_thread) ( ThreadArchState *arch )
{  
   /* Deallocate its LDT, if it ever had one. */
   VG_(deallocate_LDT_for_thread)( arch->ldt ); 
   arch->ldt = NULL;
   
   /* Clear its TLS array. */
   VG_(clear_TLS_for_thread)( arch->tls );
}  

void VGA_(setup_child) ( ThreadArchState *arch, ThreadArchState *parent_arch )
{  
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
}  

void VGA_(set_arg_and_bogus_ret)( ThreadId tid, UWord arg, Addr ret )
{
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
}

void VGA_(thread_initial_stack)(ThreadId tid, UWord arg, Addr ret)
{
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
}


/*------------------------------------------------------------*/
/*--- Symtab stuff                                         ---*/
/*------------------------------------------------------------*/

/* This is the Intel register encoding -- integer regs. */
#define R_EAX 0
#define R_ECX 1
#define R_EDX 2
#define R_EBX 3
#define R_ESP 4
#define R_EBP 5
#define R_ESI 6
#define R_EDI 7

UInt *VGA_(reg_addr_from_tst)(Int regno, ThreadArchState *arch)
{
   switch (regno) {
   case R_EAX: return &arch->vex.guest_EAX;
   case R_ECX: return &arch->vex.guest_ECX;
   case R_EDX: return &arch->vex.guest_EDX;
   case R_EBX: return &arch->vex.guest_EBX;
   case R_ESP: return &arch->vex.guest_ESP;
   case R_EBP: return &arch->vex.guest_EBP;
   case R_ESI: return &arch->vex.guest_ESI;
   case R_EDI: return &arch->vex.guest_EDI;
   default:    return NULL;
   }
}

/*------------------------------------------------------------*/
/*--- pointercheck                                         ---*/
/*------------------------------------------------------------*/

Bool VGA_(setup_pointercheck)(void)
{
   vki_modify_ldt_t ldt = { 
      VG_POINTERCHECK_SEGIDX,    // entry_number
      VG_(client_base),          // base_addr
      (VG_(client_end)-VG_(client_base)) / VKI_PAGE_SIZE, // limit
      1,                         // seg_32bit
      0,                         // contents: data, RW, non-expanding
      0,                         // ! read_exec_only
      1,                         // limit_in_pages
      0,                         // ! seg not present
      1,                         // useable
   };
   int ret = VG_(do_syscall)(__NR_modify_ldt, 1, &ldt, sizeof(ldt));
   if (ret < 0) {
      VG_(message)(Vg_UserMsg,
                   "Warning: ignoring --pointercheck=yes, "
                   "because modify_ldt failed (errno=%d)", -ret);
      return False;
   } else {
      return True;
   }
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

