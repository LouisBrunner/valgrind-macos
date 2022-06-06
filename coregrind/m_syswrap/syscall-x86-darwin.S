
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.         syscall-x86-darwin.S ---*/
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

#include "pub_core_basics_asm.h"

#if defined(VGP_x86_darwin)

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
	syscall itself failed), or a -ve error code if one of the
	sigprocmasks failed (there's no way to determine which one
	failed).

	VG_(fixup_guest_state_after_syscall_interrupted) does the
	thread state fixup in the case where we were interrupted by a
	signal.
	
	Prototype:

	Int ML_(do_syscall_for_client_WRK)(
	                          Int syscallno,		// ebp+8
				  void* guest_state,		// ebp+12
				  const vki_sigset_t *sysmask,	// ebp+16
				  const vki_sigset_t *postmask,	// ebp+20
				  Int sigsetSzB)		// ebp+24

        Note that sigsetSzB is totally ignored (and irrelevant).
*/

/* from vki-darwin.h, checked at startup by m_vki.c */	
#define VKI_SIG_SETMASK	3

/* DO_SYSCALL MACH|MDEP|UNIX */
#define MACH 1
#define MDEP 2
#define UNIX 3
	
.macro DO_SYSCALL
	/* establish stack frame */
	push	%ebp
	mov	%esp, %ebp
	subl	$$8, %esp	/* 16-byte align stack */
	
L_$0_1:	/* Even though we can't take a signal until the
           __pthread_sigmask completes, start the range early.
           If eip is in the range [1,2), the syscall hasn't been started yet */

        /* Set the signal mask which should be current during the syscall. */
        /* Set up for __pthread_sigmask(SIG_SETMASK, sysmask, postmask) */
        pushl   20(%ebp)
        pushl   16(%ebp)
        pushl   $$VKI_SIG_SETMASK
        pushl   $$0xcafebabe    /* totally fake return address */
        movl    $$__NR___pthread_sigmask, %eax
        int     $$0x80  /* should be sysenter? */
        jc      L_$0_7  /* __pthread_sigmask failed */
        addl    $$16,%esp

	/* Copy syscall parameters to the stack - assume no more than 8 
	 * plus the return address */
	/* do_syscall8 */
	/* stack is currently aligned assuming 8 parameters */
	movl	12(%ebp), %edx			
	movl	OFFSET_x86_ESP(%edx), %edx	/* edx = simulated ESP */
	movl	28+4(%edx), %eax
	pushl	%eax
	movl	24+4(%edx), %eax
	pushl	%eax
	movl	20+4(%edx), %eax
	pushl	%eax
	movl	16+4(%edx), %eax
	pushl	%eax
	movl	12+4(%edx), %eax
	pushl	%eax
	movl	8+4(%edx), %eax
	pushl	%eax
	movl	4+4(%edx), %eax
	pushl	%eax
	movl	0+4(%edx), %eax
	pushl	%eax
	/* return address */
	movl	0(%edx), %eax
	pushl	%eax

	/* Put syscall number in eax */
	movl	8(%ebp), %eax

	/* If eip==2, then the syscall was either just about to start, 
	   or was interrupted and the kernel was restarting it. */
L_$0_2:
.if $0 == UNIX
	int	$$0x80		/* UNIX (GrP fixme should be sysenter?) */
.elseif $0 == MACH
	int	$$0x81
.elseif $0 == MDEP
	int	$$0x82
.else
	error$0 x
.endif

L_$0_3:	/* In the range [3, 4), the syscall result is in %eax and %edx and C,
           but hasn't been committed to the thread state. */
	setc	0(%esp)				/* stash returned carry flag */
	movl	12(%ebp), %ecx
	movl	%eax, OFFSET_x86_EAX(%ecx)	/* save EAX to vex */
	movl	%edx, OFFSET_x86_EDX(%ecx)	/* save EDX to vex */
.if $0 == UNIX
	/* UNIX: save carry flag to vex */
	subl	$$12, %esp
	movl	%ecx, 4(%esp)
	movl	$$0, 0(%esp)
	movb	12(%esp), %al
	movb	%al, 0(%esp)
	call	_LibVEX_GuestX86_put_eflag_c
	addl	$$12, %esp
.endif

L_$0_4:	/* Re-block signals.  If eip is in [4,5), then the syscall is
           complete and we needn't worry about it. */
        /* Set up for __pthread_sigmask(SIG_SETMASK, postmask, NULL) */
        pushl   $$0
        pushl   20(%ebp)
        pushl   $$VKI_SIG_SETMASK
        pushl   $$0xcafef00d    /* totally fake return address */
        movl    $$__NR___pthread_sigmask, %eax
        int     $$0x80  /* should be sysenter? */
        jc      L_$0_7  /* __pthread_sigmask failed */
        addl    $$16,%esp

L_$0_5:	/* now safe from signals */
	movl	$$0, %eax       /* SUCCESS */
	movl	%ebp, %esp
	popl	%ebp
	ret

L_$0_7: /* failure: return 0x8000 | error code */
        /* Note that we enter here with %esp being 16 too low
           (4 extra words on the stack).  But because we're nuking
           the stack frame now, that doesn't matter. */
        andl    $$0x7FFF, %eax
        orl     $$0x8000, %eax
	movl	%ebp, %esp
	popl	%ebp
	ret

.endmacro
	

.globl ML_(do_syscall_for_client_unix_WRK)
ML_(do_syscall_for_client_unix_WRK):
	DO_SYSCALL UNIX

.globl ML_(do_syscall_for_client_mach_WRK)
ML_(do_syscall_for_client_mach_WRK):
	DO_SYSCALL MACH
	
.globl ML_(do_syscall_for_client_mdep_WRK)
ML_(do_syscall_for_client_mdep_WRK):
	DO_SYSCALL MDEP

.data
/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */

/* eg MK_L_SCLASS_N(UNIX,99) produces L_3_99
   since UNIX is #defined to 3 at the top of this file */
#define FOO(scclass,labelno) L_##scclass##_##labelno
#define MK_L_SCCLASS_N(scclass,labelno) FOO(scclass,labelno)

.globl ML_(blksys_setup_MACH)
.globl ML_(blksys_restart_MACH)
.globl ML_(blksys_complete_MACH)
.globl ML_(blksys_committed_MACH)
.globl ML_(blksys_finished_MACH)
ML_(blksys_setup_MACH):	.long MK_L_SCCLASS_N(MACH,1)
ML_(blksys_restart_MACH):	.long MK_L_SCCLASS_N(MACH,2)
ML_(blksys_complete_MACH):	.long MK_L_SCCLASS_N(MACH,3)
ML_(blksys_committed_MACH):	.long MK_L_SCCLASS_N(MACH,4)
ML_(blksys_finished_MACH):	.long MK_L_SCCLASS_N(MACH,5)

.globl ML_(blksys_setup_MDEP)
.globl ML_(blksys_restart_MDEP)
.globl ML_(blksys_complete_MDEP)
.globl ML_(blksys_committed_MDEP)
.globl ML_(blksys_finished_MDEP)
ML_(blksys_setup_MDEP):	.long MK_L_SCCLASS_N(MDEP,1)
ML_(blksys_restart_MDEP):	.long MK_L_SCCLASS_N(MDEP,2)
ML_(blksys_complete_MDEP):	.long MK_L_SCCLASS_N(MDEP,3)
ML_(blksys_committed_MDEP):	.long MK_L_SCCLASS_N(MDEP,4)
ML_(blksys_finished_MDEP):	.long MK_L_SCCLASS_N(MDEP,5)

.globl ML_(blksys_setup_UNIX)
.globl ML_(blksys_restart_UNIX)
.globl ML_(blksys_complete_UNIX)
.globl ML_(blksys_committed_UNIX)
.globl ML_(blksys_finished_UNIX)
ML_(blksys_setup_UNIX):	.long MK_L_SCCLASS_N(UNIX,1)
ML_(blksys_restart_UNIX):	.long MK_L_SCCLASS_N(UNIX,2)
ML_(blksys_complete_UNIX):	.long MK_L_SCCLASS_N(UNIX,3)
ML_(blksys_committed_UNIX):	.long MK_L_SCCLASS_N(UNIX,4)
ML_(blksys_finished_UNIX):	.long MK_L_SCCLASS_N(UNIX,5)

#endif // defined(VGP_x86_darwin)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
