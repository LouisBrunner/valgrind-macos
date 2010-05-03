
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.         syscall-ppc32-aix5.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2006-2010 OpenWorks LLP
     info@open-works.co.uk

  Derived from Paul Mackerras' implementation of same for ppc32-linux
  in syscall-ppc32-linux.S.

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

#if defined(VGP_ppc32_aix5)

#include "pub_core_basics_asm.h"
#include "libvex_guest_offsets.h"

/* kludge: from include/vki/vki-ppc32-aix5.h */
#define VKI_SIG_SETMASK    2


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

        The syscall number is taken from the argument, even though it
        should also be in R2 in guest_state.  The syscall result is written
        back to R3 and R4 in the guest state on completion.

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
				  Int syscallno,		// r3
				  void* guest_state,		// r4
				  const vki_sigset_t *sysmask,	// r5
				  const vki_sigset_t *postmask,	// r6
				  Int sigsetSzB,		// r7
				  Int __nr_sigprocmask)		// r8
*/
	.file	"syscall-ppc32-aix6.S"

	.toc
	.csect .text[PR]
	.align 2
	.globl ML_(do_syscall_for_client_WRK)
	.globl .ML_(do_syscall_for_client_WRK)
	.csect ML_(do_syscall_for_client_WRK)[DS]
ML_(do_syscall_for_client_WRK):
	.long .ML_(do_syscall_for_client_WRK), TOC[tc0], 0
	.csect .text[PR]
.ML_(do_syscall_for_client_WRK):
        /* make a stack frame */
        stwu    1,-512(1)
        stw     31,256(1)
        stw     30,260(1)
        stw     29,264(1)
        stw     28,268(1)
	stw	27,272(1)
	stw	26,276(1)
	mflr	26
	stw	26,280(1)
	stw	2,284(1)
        mr      31,3            /* syscall number */
        mr      30,4            /* guest_state */
        mr      29,6            /* postmask */
        mr      28,7            /* sigsetSzB */
	mr	27,8		/* __nr_sigprocmask */

Lvg1:   /* Even though we can't take a signal until the sigprocmask 
	completes, start the range early.  If PC is in the range [1,2),
	the syscall hasn't been started yet */

        /* set the signal mask for doing the system call */
        /* set up for sigprocmask(SIG_SETMASK, sysmask, postmask) */
	mr      2,8
        li      3,VKI_SIG_SETMASK
        mr      4,5
        mr      5,6
	mr	6,7	/* sigsetSzB -- needed on AIX ? */

	/* actually do the sigprocmask */
	crorc	6,6,6
	.long 0x48000005 /* bl here+4 */
	mflr	26
	addi	26,26,16
	mtlr	26
	sc

	/* did it fail? (assuming r3 == 0 for success) */
	cmpwi	0,3,0
	bne	0,Lvg7

        /* load up syscall args from the threadstate */
        lwz     3,OFFSET_ppc32_GPR3(30)
        lwz     4,OFFSET_ppc32_GPR4(30)
        lwz     5,OFFSET_ppc32_GPR5(30)
        lwz     6,OFFSET_ppc32_GPR6(30)
        lwz     7,OFFSET_ppc32_GPR7(30)
        lwz     8,OFFSET_ppc32_GPR8(30)
        lwz     9,OFFSET_ppc32_GPR9(30)
        lwz     10,OFFSET_ppc32_GPR10(30)
        mr      2,31            /* syscall number */

	crorc	6,6,6
	.long 0x48000005 /* bl here+4 */
	mflr	26
	addi	26,26,16
	mtlr	26

        /* If PC is in the range [2,2], then the syscall was either
	just about to start, or was interrupted and the kernel was 
	restarting it. */
Lvg2:   sc                      /* do the syscall */

	/* In the range [3, 4), the syscall result is in r3/r4, but 
	hasn't been committed to R3/R4. */
        /* put the result back in the threadstate  */

Lvg3:	stw     3,OFFSET_ppc32_GPR3(30)     /* gst->GPR3 = res */
	stw     4,OFFSET_ppc32_GPR4(30)     /* gst->GPR4 = err */

        /* Block signals again.  If PC is in [4,5), then the syscall 
	is complete and we needn't worry about it. */
	/* set up for sigprocmask(SIG_SETMASK, postmask, NULL) */
Lvg4:   mr	2,27
        li      3,VKI_SIG_SETMASK
        mr      4,29
        li      5,0
        mr      6,28	/* sigsetSzB -- needed on AIX ? */

	/* actually do the sigprocmask */
	crorc	6,6,6
	.long 0x48000005 /* bl here+4 */
	mflr	26
	addi	26,26,16
	mtlr	26
	sc

	/* did it fail? (assuming r3 == 0 for success) */
	cmpwi	0,3,0
	bne	0,Lvg7

        /* now safe from signals */
	li	3,0		/* SUCCESS */

        /* pop off stack frame */
Lvg5:   lwz	2,284(1)
	lwz	26,280(1)
	mtlr	26
	lwz     26,276(1)
	lwz	27,272(1)
	lwz	28,268(1)
        lwz     29,264(1)
        lwz     30,260(1)
        lwz     31,256(1)
        addi    1,1,512
        blr

	/* failure: return 0x8000 | error code */
Lvg7:   mr	3,4
	ori	3,3,0x8000	/* FAILURE -- ensure return value is nonzero */
        b       Lvg5

/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */

	.csect .data[RW],3
        .align 2
.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):     .long Lvg1
ML_(blksys_restart):   .long Lvg2
ML_(blksys_complete):  .long Lvg3
ML_(blksys_committed): .long Lvg4
ML_(blksys_finished):  .long Lvg5

#endif // defined(VGP_ppc32_aix5)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
