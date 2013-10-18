
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.          syscall-x86-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2013 Julian Seward 
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

#if defined(VGP_x86_linux)

#include "pub_core_basics_asm.h"
#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"
		
	
/*----------------------------------------------------------------*/
/*
	Perform a syscall for the client.  This will run a syscall
	with the client's specific per-thread signal mask.
	
	The structure of this function is such that, if the syscall is
	interrupted by a signal, we can determine exactly what
	execution state we were in with respect to the execution of
	the syscall by examining the value of %eip in the signal
	handler.  This means that we can always do the appropriate
	thing to precisely emulate the kernel's signal/syscall
	interactions.

	The syscall number is taken from the argument, even though it
	should also be in regs->m_eax.  The syscall result is written
	back to regs->m_eax on completion.
	
	Returns 0 if the syscall was successfully called (even if the
	syscall itself failed), or a nonzero error code in the lowest
        8 bits if one of the sigprocmasks failed (there's no way to
        determine which one failed).  And there's no obvious way to
        recover from that either, but nevertheless we want to know.

	VG_(fixup_guest_state_after_syscall_interrupted) does the
	thread state fixup in the case where we were interrupted by a
	signal.
	
	Prototype:

	UWord ML_(do_syscall_for_client_WRK)(
	                          Int syscallno,		// 0
				  void* guest_state,		// 4
				  const vki_sigset_t *sysmask,	// 8
				  const vki_sigset_t *postmask,	// 12
				  Int sigsetSzB)		// 16
				   
*/

/* from vki_arch.h */	
#define VKI_SIG_SETMASK	2
	
.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):
	/* save callee-saved regs */
	push	%esi
	push	%edi
	push	%ebx
	push	%ebp
#define FSZ	((4+1)*4)	/* 4 args + ret addr */

1:	/* Even though we can't take a signal until the sigprocmask completes,
	   start the range early.
	   If eip is in the range [1,2), the syscall hasn't been started yet */

	/* Set the signal mask which should be current during the syscall. */
	movl	$ __NR_rt_sigprocmask, %eax
	movl	$ VKI_SIG_SETMASK, %ebx
	movl	8+FSZ(%esp), %ecx
	movl	12+FSZ(%esp), %edx
	movl	16+FSZ(%esp), %esi
	int	$0x80
	testl	%eax, %eax
	js	7f	/* sigprocmask failed */
	
	movl	 4+FSZ(%esp), %eax	/* eax == ThreadState * */

	movl	 OFFSET_x86_EBX(%eax), %ebx
	movl	 OFFSET_x86_ECX(%eax), %ecx
	movl	 OFFSET_x86_EDX(%eax), %edx
	movl	 OFFSET_x86_ESI(%eax), %esi
	movl	 OFFSET_x86_EDI(%eax), %edi
	movl	 OFFSET_x86_EBP(%eax), %ebp
	movl	 0+FSZ(%esp), %eax	/* use syscallno argument rather than thread EAX */
	
	/* If eip==2, then the syscall was either just about to start, 
	   or was interrupted and the kernel was restarting it. */
2:	int	$0x80
3:	/* In the range [3, 4), the syscall result is in %eax, but hasn't been
	   committed to EAX. */
	movl	4+FSZ(%esp), %ebx
	movl	%eax, OFFSET_x86_EAX(%ebx)	/* save back to EAX */

4:	/* Re-block signals.  If eip is in [4,5), then the syscall is complete and 
	   we needn't worry about it. */
	movl	$ __NR_rt_sigprocmask, %eax
	movl	$ VKI_SIG_SETMASK, %ebx
	movl	12+FSZ(%esp), %ecx
	xorl	%edx, %edx
	movl	16+FSZ(%esp), %esi
	int	$0x80
	testl	%eax, %eax
	js	7f	/* sigprocmask failed */

5:	/* now safe from signals */
	movl	$0, %eax	/* SUCCESS */
	popl	%ebp
	popl	%ebx
	popl	%edi
	popl	%esi
	ret

7:	/* failure: return 0x8000 | error code */
	negl	%eax
	andl	$0x7FFF, %eax
	orl	$0x8000, %eax
	popl	%ebp
	popl	%ebx
	popl	%edi
	popl	%esi
	ret
#undef FSZ

	
.section .rodata
/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */
	
.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):	.long 1b
ML_(blksys_restart):	.long 2b
ML_(blksys_complete):	.long 3b
ML_(blksys_committed):	.long 4b
ML_(blksys_finished):	.long 5b
.previous
	
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif // defined(VGP_x86_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
