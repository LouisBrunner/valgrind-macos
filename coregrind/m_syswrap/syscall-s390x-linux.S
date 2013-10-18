
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.        syscall-s390x-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2013

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

/* Contributed by Christian Borntraeger */

#include "pub_core_basics_asm.h"
#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"

#if defined(VGA_s390x)

/*----------------------------------------------------------------*/
/*
        Perform a syscall for the client.  This will run a syscall
        with the client's specific per-thread signal mask.

        The structure of this function is such that, if the syscall is
        interrupted by a signal, we can determine exactly what
        execution state we were in with respect to the execution of
        the syscall by examining the value of NIP in the signal
        handler.  This means that we can always do the appropriate
        thing to precisely emulate the kernel's signal/syscall
        interactions.

        The syscall number is taken from the argument, since the syscall
        number can be encoded in the svc instruction itself.
        The syscall result is written back to guest register r2.

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
				  Int syscallno,		// r2
				  void* guest_state,		// r3
				  const vki_sigset_t *sysmask,	// r4
				  const vki_sigset_t *postmask,	// r5
				  Int nsigwords)		// r6
*/
/* from vki_arch.h */
#define VKI_SIG_SETMASK 2

#define SP_SAVE 16
#define SP_R2	SP_SAVE + 0*8
#define SP_R3	SP_SAVE + 1*8
#define SP_R4	SP_SAVE + 2*8
#define SP_R5	SP_SAVE + 3*8
#define SP_R6	SP_SAVE + 4*8
#define SP_R7	SP_SAVE + 5*8
#define SP_R8	SP_SAVE + 6*8
#define SP_R9	SP_SAVE + 7*8

.align 4
.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):
1:	/* Even though we can't take a signal until the sigprocmask completes,
	start the range early.
	If IA is in the range [1,2), the syscall hasn't been started yet */

	/* Set the signal mask which should be current during the syscall. */
	/* Save and restore all the parameters and all the registers that
	   we clobber (r6-r9) */
	stmg	%r2,%r9, SP_R2(%r15)

	lghi	%r2, VKI_SIG_SETMASK		/* how */
	lgr	%r3, %r4			/* sysmask */
	lgr	%r4, %r5			/* postmask */
	lgr	%r5, %r6			/* nsigwords */
	svc	__NR_rt_sigprocmask
	cghi	%r2, 0x0
	jne	7f				/* sigprocmask failed */

	/* OK, that worked.  Now do the syscall proper. */
	lg	%r9, SP_R3(%r15)		/* guest state --> r9 */
	lg	%r2, OFFSET_s390x_r2(%r9)	/* guest r2 --> real r2 */
	lg	%r3, OFFSET_s390x_r3(%r9)	/* guest r3 --> real r3 */
	lg	%r4, OFFSET_s390x_r4(%r9)	/* guest r4 --> real r4 */
	lg	%r5, OFFSET_s390x_r5(%r9)	/* guest r5 --> real r5 */
	lg	%r6, OFFSET_s390x_r6(%r9)	/* guest r6 --> real r6 */
	lg	%r7, OFFSET_s390x_r7(%r9)	/* guest r7 --> real r7 */
	lg	%r1, SP_R2(%r15)		/* syscallno -> r1 */

2:	svc	0

3:
	stg	%r2, OFFSET_s390x_r2(%r9)

4:	/* Re-block signals.  If IA is in [4,5), then the syscall
	   is complete and we needn't worry about it. */
	lghi	%r2, VKI_SIG_SETMASK		/* how */
	lg	%r3, SP_R5(%r15)		/* postmask */
	lghi	%r4, 0x0			/* NULL */
	lg	%r5, SP_R6(%r15)		/* nsigwords */
	svc	__NR_rt_sigprocmask
	cghi	%r2, 0x0
	jne	7f				/* sigprocmask failed */

5:	/* Everyting ok. Return 0 and restore the call-saved
	   registers, that we have clobbered */
	lghi	%r2, 0x0
	lmg	%r6,%r9, SP_R6(%r15)
	br	%r14

7:	/* Some problem. Return 0x8000 | error and restore the call-saved
	   registers we have clobbered. */
	nill	%r2, 0x7fff
	oill	%r2, 0x8000
	lmg	%r6,%r9, SP_R6(%r15)
	br	%r14

.section .rodata
/* Export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */

.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)

/* The compiler can assume that 8 byte data elements are aligned on 8 byte */
.align 8
ML_(blksys_setup):     .quad 1b
ML_(blksys_restart):   .quad 2b
ML_(blksys_complete):  .quad 3b
ML_(blksys_committed): .quad 4b
ML_(blksys_finished):  .quad 5b
.previous

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif /* VGA_s390x */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
