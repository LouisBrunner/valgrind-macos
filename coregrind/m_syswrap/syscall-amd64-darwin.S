
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.       syscall-amd64-darwin.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2012 Julian Seward 
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

#if defined(VGP_amd64_darwin)

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
	should also be in guest_state->guest_RAX.  The syscall result
	is written back to guest_state->guest_RAX on completion.
	
	Returns 0 if the syscall was successfully called (even if the
	syscall itself failed), or a -ve error code if one of the
	sigprocmasks failed (there's no way to determine which one
	failed).

	VG_(fixup_guest_state_after_syscall_interrupted) does the
	thread state fixup in the case where we were interrupted by a
	signal.
	
	Prototype:

	Int ML_(do_syscall_for_client_WRK(
	                          Int syscallno,		// rdi
				  void* guest_state,		// rsi
				  const vki_sigset_t *sysmask,	// rdx
				  const vki_sigset_t *postmask,	// rcx
				  Int sigsetSzB)		// r8

        Note that sigsetSzB is totally ignored (and irrelevant).
*/

/* from vki_arch.h */	
#define VKI_SIG_SETMASK	3

/* DO_SYSCALL MACH|MDEP|UNIX */
#define MACH 1
#define MDEP 2
#define UNIX 3

.macro DO_SYSCALL
	/* save callee-saved regs */
	pushq	%rbp
	movq	%rsp, %rbp
	// stack is now aligned
	pushq	%rdi  // -8(%rbp)   syscallno
	pushq	%rsi  // -16(%rbp)  guest_state
	pushq	%rdx  // -24(%rbp)  sysmask
	pushq	%rcx  // -32(%rbp)  postmask
	pushq	%r8   // -40(%rbp)  sigsetSzB
	// stack is now aligned
	
L_$0_1:	/* Even though we can't take a signal until the sigprocmask completes,
	   start the range early.
	   If rip is in the range [1,2), the syscall hasn't been started yet */

	/* Set the signal mask which should be current during the syscall. */
	/* GrP fixme signals
           DDD: JRS fixme: use __NR___pthread_sigmask, not __NR_rt_sigprocmask
	movq	$__NR_rt_sigprocmask, %rax	// syscall #
	movq	$VKI_SIG_SETMASK, %rdi		// how
	movq	-24(%rbp), %rsi			// sysmask
	movq	-32(%rbp), %rdx			// postmask
	movq	-40(%rbp), %r10			// sigsetSzB in r10 not rcx
	DDD: fixme return address
	syscall

	jnc	7f	// sigprocmask failed
	*/
	
	/* OK, that worked.  Now do the syscall proper. */

	/* 6 register parameters */
	movq	-16(%rbp), %r11	/* r11 = VexGuestAMD64State * */
	movq	OFFSET_amd64_RDI(%r11), %rdi
	movq	OFFSET_amd64_RSI(%r11), %rsi
	movq	OFFSET_amd64_RDX(%r11), %rdx
	movq	OFFSET_amd64_RCX(%r11), %r10 /* rcx is passed in r10 instead */
	movq	OFFSET_amd64_R8(%r11), %r8
	movq	OFFSET_amd64_R9(%r11), %r9
	/* 2 stack parameters plus return address (ignored by syscall) */
	movq	OFFSET_amd64_RSP(%r11), %r11 /* r11 = simulated RSP */
	movq	16(%r11), %rax
	pushq	%rax
	movq	8(%r11), %rax
	pushq	%rax
	/* stack is currently aligned - return address misaligns */
	movq	0(%r11), %rax
	pushq	%rax
	/* syscallno */
	movq	-8(%rbp), %rax
	
	/* If rip==2, then the syscall was either just about
	   to start, or was interrupted and the kernel was 
	   restarting it. */
L_$0_2:	syscall
L_$0_3:	/* In the range [3, 4), the syscall result is in %rax, 
	   but hasn't been committed to RAX. */

	/* stack contents: 3 words for syscall above, plus our prologue */
	setc	0(%rsp) 	/* stash returned carry flag */

	movq	-16(%rbp), %r11	/* r11 = VexGuestAMD64State * */
	movq	%rax, OFFSET_amd64_RAX(%r11)	/* save back to RAX */
	movq	%rdx, OFFSET_amd64_RDX(%r11)	/* save back to RDX */

.if $0 == UNIX
	/* save carry flag to VEX */
	xor	%rax, %rax
	movb	0(%rsp), %al
	movq	%rax, %rdi	/* arg1 = new flag */
	movq	%r11, %rsi	/* arg2 = vex state */
	addq	$$24, %rsp	/* remove syscall parameters */
	call	_LibVEX_GuestAMD64_put_rflag_c
.else
	addq	$$24, %rsp	/* remove syscall parameters*/
.endif

L_$0_4:	/* Re-block signals.  If eip is in [4,5), then the syscall 
	   is complete and we needn't worry about it. */
	/* GrP fixme signals
           DDD: JRS fixme: use __NR___pthread_sigmask, not __NR_rt_sigprocmask
	PUSH_di_si_dx_cx_8

	movq	$__NR_rt_sigprocmask, %rax	// syscall #
	movq	$VKI_SIG_SETMASK, %rdi		// how
	movq	%rcx, %rsi			// postmask
	xorq	%rdx, %rdx			// NULL
	movq	%r8, %r10			// sigsetSzB
	DDD: fixme return address
	syscall

	POP_di_si_dx_cx_8

	jnc	7f	// sigprocmask failed
	*/
L_$0_5:	/* now safe from signals */
	movq	$$0, %rax	/* SUCCESS */
	movq	%rbp, %rsp
	popq	%rbp
	ret

/* GrP fixme signals
L_$0_7:	// failure:	 return 0x8000 | error code
	DDD: fixme return value
	movq	%rbp, %rsp
	popq	%rbp
	ret
*/

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
ML_(blksys_setup_MACH):	.quad MK_L_SCCLASS_N(MACH,1)
ML_(blksys_restart_MACH):	.quad MK_L_SCCLASS_N(MACH,2)
ML_(blksys_complete_MACH):	.quad MK_L_SCCLASS_N(MACH,3)
ML_(blksys_committed_MACH):	.quad MK_L_SCCLASS_N(MACH,4)
ML_(blksys_finished_MACH):	.quad MK_L_SCCLASS_N(MACH,5)

.globl ML_(blksys_setup_MDEP)
.globl ML_(blksys_restart_MDEP)
.globl ML_(blksys_complete_MDEP)
.globl ML_(blksys_committed_MDEP)
.globl ML_(blksys_finished_MDEP)
ML_(blksys_setup_MDEP):	.quad MK_L_SCCLASS_N(MDEP,1)
ML_(blksys_restart_MDEP):	.quad MK_L_SCCLASS_N(MDEP,2)
ML_(blksys_complete_MDEP):	.quad MK_L_SCCLASS_N(MDEP,3)
ML_(blksys_committed_MDEP):	.quad MK_L_SCCLASS_N(MDEP,4)
ML_(blksys_finished_MDEP):	.quad MK_L_SCCLASS_N(MDEP,5)

.globl ML_(blksys_setup_UNIX)
.globl ML_(blksys_restart_UNIX)
.globl ML_(blksys_complete_UNIX)
.globl ML_(blksys_committed_UNIX)
.globl ML_(blksys_finished_UNIX)
ML_(blksys_setup_UNIX):	.quad MK_L_SCCLASS_N(UNIX,1)
ML_(blksys_restart_UNIX):	.quad MK_L_SCCLASS_N(UNIX,2)
ML_(blksys_complete_UNIX):	.quad MK_L_SCCLASS_N(UNIX,3)
ML_(blksys_committed_UNIX):	.quad MK_L_SCCLASS_N(UNIX,4)
ML_(blksys_finished_UNIX):	.quad MK_L_SCCLASS_N(UNIX,5)

#endif // defined(VGP_amd64_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
