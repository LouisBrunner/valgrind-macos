
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                               x86/dispatch.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, an extensible x86 protected-mode
  emulator for monitoring program execution on x86-Unixes.

  Copyright (C) 2000-2004 Julian Seward 
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

#include "core_asm.h"
#include "x86_private_asm.h"


/*------------------------------------------------------------*/
/*--- The normal-case dispatch machinery.                  ---*/
/*------------------------------------------------------------*/
	
/* To transfer to an (original) code address, load it into %eax and
   jump to vg_dispatch.  This fragment of code tries to find the
   address of the corresponding translation by searching the translation
   table.   If it fails, a new translation is made, added to the
   translation table, and then jumped to.  Almost all the hard
   work is done by C routines; this code simply handles the
   common case fast -- when the translation address is found in
   the translation cache.

   At entry, %eax is the only live (real-machine) register; the
   entire simulated state is tidily saved in vg_m_state.  
*/

	
#define TT_LOOKUP(reg, fail)				\
	movl %eax, reg;					\
	andl $VG_TT_FAST_MASK, reg;			\
	movl VG_(tt_fast)(,reg,4), reg;			\
	cmpl %eax, (reg);				\
	jnz  fail
	
/* The C world needs a way to get started simulating.  So we provide
   a function void vg_run_innerloop ( void ), which starts running
   from vg_m_eip, and exits when the counter reaches zero.  This loop
   can also exit if vg_oursignalhandler() catches a non-resumable
   signal, for example SIGSEGV.  It then longjmp()s back past here.
*/

/* signature: UInt VG_(run_innerloop) ( void* guest_state ) */

.globl VG_(run_innerloop)
VG_(run_innerloop):
	/* 4(%esp) holds guest_state */
	
	/* ----- entry point to VG_(run_innerloop) ----- */
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	
	/* 28(%esp) holds guest_state */

	/* Set up the guest state pointer */
	movl	28(%esp), %ebp
	
	/* fetch m_eip into %eax */
	movl	VG_(instr_ptr_offset), %esi
	movl	(%ebp, %esi, 1), %eax
	
	/* fall into main loop */

dispatch_boring:
	/* save the jump address in the guest state */
	movl	VG_(instr_ptr_offset), %esi
	movl	%eax, (%ebp, %esi, 1)

	/* Are we out of timeslice?  If yes, defer to scheduler. */
	subl $1, VG_(dispatch_ctr)
	
	jz	counter_is_zero
	/* try a fast lookup in the translation cache */
	TT_LOOKUP(%ebx, fast_lookup_failed)

	/* Found a match.  Call the tce.payload field.  The magic 8
	   value is offsetof(TCEntry,payload) on a 32-bit platform. */
	
	addl $8, %ebx
	call *%ebx
	
	/* 
	   %eax holds destination (original) address.
	   %ebp indicates further details of the control transfer
	   requested to the address in %eax.
	
	   If ebp is unchanged (== * 28(%esp)), just jump next to %eax.
	 
	   If ebp == VG_EBP_JMP_SYSCALL, do a system call before 
	   continuing at eax.
	
	   If ebp == VG_EBP_JMP_CLIENTREQ, do a client request before 
	   continuing at eax.
	
	   If %ebp has any other value, we panic.
	*/

	cmpl	28(%esp), %ebp
	jz	dispatch_boring

	jmp	dispatch_exceptional

	
fast_lookup_failed:
	/* %EIP is up to date here since dispatch_boring dominates */
	addl	$1, VG_(dispatch_ctr)
	movl	$VG_TRC_INNER_FASTMISS, %eax
	jmp	run_innerloop_exit

counter_is_zero:
	/* %EIP is up to date here since dispatch_boring dominates */
	addl	$1, VG_(dispatch_ctr)
	movl	$VG_TRC_INNER_COUNTERZERO, %eax
	jmp	run_innerloop_exit
	
run_innerloop_exit:
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	ret	



/* Other ways of getting out of the inner loop.  Placed out-of-line to
   make it look cleaner. 
*/
dispatch_exceptional:
	/* this is jumped to only, not fallen-through from above */
	cmpl	$VG_TRC_INNER_COUNTERZERO, %ebp
	jz	counter_is_zero

	/* save %eax in %EIP and defer to sched */
	movl	VG_(instr_ptr_offset), %esi
	movl	28(%esp), %edi
	movl	%eax, (%edi, %esi, 1)
	movl	%ebp, %eax
	jmp	run_innerloop_exit

	
.data
panic_msg_ebp:
.ascii	"vg_dispatch: %ebp has invalid value!"
.byte	0
.text	
        
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
