
/*--------------------------------------------------------------------*/
/*--- Arch-specific registers, etc.                    x86/state.c ---*/
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
#include <sys/ptrace.h>

#include "libvex_guest_arm.h"


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
   I_die_here;
#if 0
   // When implementing this, look at x86/state.c
#endif
}


/*------------------------------------------------------------*/
/*--- Thread stuff                                         ---*/
/*------------------------------------------------------------*/

void VGA_(clear_thread)( ThreadArchState *arch )
{
   I_die_here;
#if 0
   arch->ldt = NULL;
   VG_(clear_TLS_for_thread)(arch->tls);
#endif
}  

void VGA_(cleanup_thread) ( ThreadArchState *arch )
{  
   I_die_here;
#if 0
   /* Deallocate its LDT, if it ever had one. */
   VG_(deallocate_LDT_for_thread)( arch->ldt ); 
   arch->ldt = NULL;
   
   /* Clear its TLS array. */
   VG_(clear_TLS_for_thread)( arch->tls );
#endif
}  

void VGA_(setup_child) ( ThreadArchState *regs, ThreadArchState *parent_regs )
{  
   I_die_here;
#if 0
   // XXX: look at x86/state.c
#endif
}  


/*------------------------------------------------------------*/
/*--- Symtab stuff                                         ---*/
/*------------------------------------------------------------*/

UInt *VGA_(reg_addr_from_tst)(Int regno, ThreadArchState *arch)
{
   I_die_here;
#if 0
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
#endif
}

/*------------------------------------------------------------*/
/*--- pointercheck                                         ---*/
/*------------------------------------------------------------*/

Bool VGA_(setup_pointercheck)(void)
{
   I_die_here;
#if 0
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
#endif
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
