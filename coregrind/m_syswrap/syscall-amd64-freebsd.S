
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.      syscall-amd64-freebsd.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2008 Julian Seward
     jseward@acm.org
   Copyright (C) 2018-2021 Paul Floyd
      pjfloyd@wanadoo.fr

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

#if defined(VGP_amd64_freebsd)

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
*/

/* from vki_arch.h */
#define VKI_SIG_SETMASK	3

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):
   /* save callee-saved regs */
   pushq %rbp
   movq  %rsp, %rbp
   pushq %rdi  // -8(%rbp)   syscallno
   pushq %rsi  // -16(%rbp)  guest_state
   pushq %rdx  // -24(%rbp)  sysmask
   pushq %rcx  // -32(%rbp)  postmask
   pushq %r8   // -40(%rbp)  sigsetSzB

1: /* Even though we can't take a signal until the sigprocmask completes,
      start the range early.
      If eip is in the range [1,2), the syscall hasn't been started yet */

   /* Set the signal mask which should be current during the syscall. */
   /* Save and restore all 5 arg regs round the call.  This is easier
      than figuring out the minimal set to save/restore. */

   movq  $__NR_sigprocmask, %rax		// syscall #
   movq  $VKI_SIG_SETMASK, %rdi		// how
   movq  %rdx, %rsi			// sysmask
   movq  %rcx, %rdx			// postmask
   syscall

   jb	7f	/* sigprocmask failed */

   /* OK, that worked.  Now do the syscall proper. */

   /* 6 register parameters */
   movq  -16(%rbp), %r11 /* r11 = VexGuestAMD64State * */
   movq  OFFSET_amd64_RDI(%r11), %rdi
   movq  OFFSET_amd64_RSI(%r11), %rsi
   movq  OFFSET_amd64_RDX(%r11), %rdx
   movq  OFFSET_amd64_R10(%r11), %r10
   movq  OFFSET_amd64_R8(%r11), %r8
   movq  OFFSET_amd64_R9(%r11), %r9
   /* 2 stack parameters plus return address (ignored by syscall) */
   /* @todo PJF there is a potential bug here
    * syscall can take up to 8 arguments
    * but when syscall0 or syscall198 is being used
    * one argument is used for the syscall0/198 id
    * and one for the actual id and in this case
    * there could be 3 stack parameters.
    * However, only mmap takes 8 arguments
    * and only on x86. It would be an unlikely combination,
    * but this might break one day. */
   movq  OFFSET_amd64_RSP(%r11), %r11 /* r11 = simulated RSP */
   movq  16(%r11), %rax
   pushq %rax
   movq  8(%r11), %rax
   pushq %rax
   /* (fake) return address. */
   movq  0(%r11), %rax
   pushq %rax
   /* syscallno */
   movq  -8(%rbp), %rax

   /* If rip==2, then the syscall was either just about
      to start, or was interrupted and the kernel was
      restarting it. */
2:   syscall
3:   /* In the range [3, 4), the syscall result is in %rax,
      but hasn't been committed to RAX. */

   /* stack contents: 3 words for syscall above, plus our prologue */
   setc  0(%rsp)         /* stash returned carry flag */

   movq  -16(%rbp), %r11 /* r11 = VexGuestAMD64State * */
   movq  %rax, OFFSET_amd64_RAX(%r11)    /* save back to RAX */
   movq  %rdx, OFFSET_amd64_RDX(%r11)    /* save back to RDX */

   /* save carry flag to VEX */
   xorq  %rax, %rax
   movb  0(%rsp), %al
   movq  %rax, %rdi      /* arg1 = new flag */
   movq  %r11, %rsi      /* arg2 = vex state */
   addq  $24, %rsp       /* remove syscall parameters */
   call  LibVEX_GuestAMD64_put_rflag_c

4: /* Re-block signals.  If eip is in [4,5), then the syscall
      is complete and we needn't worry about it. */

   movq  $__NR_sigprocmask, %rax	// syscall #
   movq  $VKI_SIG_SETMASK, %rdi		// how
   movq  -32(%rbp), %rsi			// postmask
   xorq  %rdx, %rdx			// NULL
   syscall

   jb    7f /* sigprocmask failed */

5:	/* now safe from signals */

   xorq  %rax,%rax
   movq  -8(%rbp), %rdi
   movq  -16(%rbp), %rsi
   movq  -24(%rbp), %rdx
   movq  -32(%rbp), %rcx
   movq  -40(%rbp), %r8
   movq  %rbp, %rsp
   popq  %rbp
   ret

7:	/* failure:	 return 0x8000 | error code */
   orq   $0x8000, %rax
   movq  -8(%rbp), %rdi
   movq  -16(%rbp), %rsi
   movq  -24(%rbp), %rdx
   movq  -32(%rbp), %rcx
   movq  -40(%rbp), %r8
   movq  %rbp, %rsp
   popq  %rbp
   ret

.section .rodata
   /* export the ranges so that
      VG_(fixup_guest_state_after_syscall_interrupted) can do the
      right thing */

.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):	.quad 1b
ML_(blksys_restart):	.quad 2b
ML_(blksys_complete):	.quad 3b
ML_(blksys_committed):	.quad 4b
ML_(blksys_finished):	.quad 5b
.previous

#endif /* defined(VGP_amd64_freebsd) */

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
