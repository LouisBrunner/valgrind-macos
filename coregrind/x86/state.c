
/*--------------------------------------------------------------------*/
/*--- x86 registers, etc.                              x86/state.c ---*/
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
/*--- baseBlock setup and operations                       ---*/
/*------------------------------------------------------------*/

Int VGOFF_(m_eip) = INVALID_OFFSET;


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

   /* The dispatch loop needs to be able to find %EIP given a pointer
      to the start of the .vex field. */
   VGOFF_(m_eip) = offsetof(VexGuestX86State,guest_EIP)/4;

   if (VG_(needs).shadow_regs) {
      VG_TRACK( post_regs_write_init );
   }

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
/*--- Register access stuff                                ---*/
/*------------------------------------------------------------*/

void VGA_(set_thread_shadow_archreg) ( ThreadId tid, UInt archreg, UInt val )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];
   if (0)
   VG_(printf)("set_thread_shadow_archreg(%d, %d, 0x%x)\n",
               tid, archreg, val);
   switch (archreg) {
      case R_EAX: tst->arch.vex_shadow.guest_EAX = val; break;
      case R_ECX: tst->arch.vex_shadow.guest_ECX = val; break;
      case R_EDX: tst->arch.vex_shadow.guest_EDX = val; break;
      case R_EBX: tst->arch.vex_shadow.guest_EBX = val; break;
      case R_ESP: tst->arch.vex_shadow.guest_ESP = val; break;
      case R_EBP: tst->arch.vex_shadow.guest_EBP = val; break;
      case R_ESI: tst->arch.vex_shadow.guest_ESI = val; break;
      case R_EDI: tst->arch.vex_shadow.guest_EDI = val; break;
      default:    VG_(core_panic)( "set_thread_shadow_archreg");
   }
}

UInt VGA_(get_thread_shadow_archreg) ( ThreadId tid, UInt archreg )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   if (0)
   VG_(printf)("get_thread_shadow_archreg(%d, %d)\n",
               tid, archreg);

   switch (archreg) {
      case R_EAX: return tst->arch.vex_shadow.guest_EAX;
      case R_ECX: return tst->arch.vex_shadow.guest_ECX;
      case R_EDX: return tst->arch.vex_shadow.guest_EDX;
      case R_EBX: return tst->arch.vex_shadow.guest_EBX;
      case R_ESP: return tst->arch.vex_shadow.guest_ESP;
      case R_EBP: return tst->arch.vex_shadow.guest_EBP;
      case R_ESI: return tst->arch.vex_shadow.guest_ESI;
      case R_EDI: return tst->arch.vex_shadow.guest_EDI;
      default:    VG_(core_panic)( "get_thread_shadow_archreg");
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
   VG_TRACK( post_mem_write, VG_(threads)[tid].arch.vex.guest_ESP, 
                             sizeof(void*) );

   /* Don't mark the pushed return address as readable; any attempt to read
      this is an internal valgrind bug since thread_exit_wrapper() should not
      return. */
   SET_PTHREQ_ESP(tid, VG_(threads)[tid].arch.vex.guest_ESP - sizeof(UWord));
   * (UInt*)(VG_(threads)[tid].arch.vex.guest_ESP) = ret;
}

void VGA_(thread_initial_stack)(ThreadId tid, UWord arg, Addr ret)
{
   Addr esp = (Addr)ARCH_STACK_PTR(VG_(threads)[tid].arch);

   /* push two args */
   esp -= 2 * sizeof(UWord);
   SET_PTHREQ_ESP(tid, esp);
   
   VG_TRACK ( new_mem_stack, esp, 2 * sizeof(UWord) );
   VG_TRACK ( pre_mem_write, Vg_CorePThread, tid, "new thread: stack",
                             esp, 2 * sizeof(UWord) );

   /* push arg and (bogus) return address */
   *(UWord*)(esp+sizeof(UWord)) = arg;
   *(UWord*)(esp)               = ret;

   VG_TRACK ( post_mem_write, esp, 2 * sizeof(UWord) );
}


/*------------------------------------------------------------*/
/*--- Symtab stuff                                         ---*/
/*------------------------------------------------------------*/

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
