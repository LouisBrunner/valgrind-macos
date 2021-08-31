
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.        syscall-ppc64-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2005-2017 Paul Mackerras <paulus@samba.org>

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

#if defined(VGP_ppc64le_linux)

#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"
		

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
        should also be in regs->m_gpr[0].  The syscall result is written
        back to regs->m_gpr[3]/m_xer/m_result on completion.

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
				  Int sigsetSzB)		// r7
*/
/* from vki_arch.h */

/* SCV flag defines.  Must be consistently defined here and in
   coregrind/pub_core_syscall.h,in the do_syscall_WRK() assembly code in
   coregrind/m_syscall.c and coregrind/m_syswrap/syscall-ppcle-linux.S code.
*/
#define SC_FLAG  1
#define SCV_FLAG 2

#define VKI_SIG_SETMASK 2

.align 2
.globl ML_(do_syscall_for_client_WRK)
#if _CALL_ELF == 2
.type .ML_(do_syscall_for_client_WRK),@function
ML_(do_syscall_for_client_WRK):
0:      addis         2,12,.TOC.-0b@ha
        addi          2,2,.TOC.-0b@l
        .localentry   ML_(do_syscall_for_client_WRK), .-ML_(do_syscall_for_client_WRK)
#else
.section ".opd","aw"
.align 3
ML_(do_syscall_for_client_WRK):
.quad .ML_(do_syscall_for_client_WRK),.TOC.@tocbase,0
.previous
#endif
#if _CALL_ELF == 2
0:      addis        2,12,.TOC.-0b@ha
        addi         2,2,.TOC.-0b@l
        .localentry  ML_(do_syscall_for_client_WRK), .-ML_(do_syscall_for_client_WRK)
#endif
.type  .ML_(do_syscall_for_client_WRK),@function
.globl .ML_(do_syscall_for_client_WRK)
.ML_(do_syscall_for_client_WRK):
        /* make a stack frame */
        stdu    1,-80(1)
        std     31,72(1)
        std     30,64(1)
        std     29,56(1)
        std     28,48(1)
        std     27,40(1)        /* save r27 to stack frame */
        mflr    27              /* Get link register */
        std     27,16(1)        /* Save link register */
        std     9,32(1)         /* save gpr 9 */
        mr      31,3            /* syscall number */
        mr      30,4            /* guest_state */
        mr      29,6            /* postmask */
        mr      28,7            /* sigsetSzB */

        /* set the signal mask for doing the system call */
        /* set up for sigprocmask(SIG_SETMASK, sysmask, postmask) */
1:      li      0,__NR_rt_sigprocmask
        li      3,VKI_SIG_SETMASK
        mr      4,5
        mr      5,6
	mr	6,7
        sc                      /* set the mask */
        bso     7f              /* if the sigprocmask fails */

        /* load up syscall args from the threadstate */
        ld      3,OFFSET_ppc64_GPR3(30)
        ld      4,OFFSET_ppc64_GPR4(30)
        ld      5,OFFSET_ppc64_GPR5(30)
        ld      6,OFFSET_ppc64_GPR6(30)
        ld      7,OFFSET_ppc64_GPR7(30)
        ld      8,OFFSET_ppc64_GPR8(30)
        ld      9,OFFSET_ppc64_GPR9(30)   /* get flag for sc or scv inst */
        mr      0,31            /* syscall number */
        cmpdi   9,SC_FLAG
        bne     8f              /* jump to scv call if gpr9 != 0 */
        /* If you change the code between labels 2 and 3, you need to update
           the corresponding ppc64le calculations for blksys_restart and
           blksys_complete in function getSyscallArgsFromGuestState in file
           syswrap-main.c.  */
2:      sc                      /* do the syscall */
        b       3f              /* jump over scv call */
8:      scv     0

        /* put the result back in the threadstate  */
3:	std     3,OFFSET_ppc64_GPR3(30)     /* gst->GPR3 = sc result */

	/* copy cr0.so back to simulated state */
	mfcr    5                           /* r5 = CR               */
	rlwinm	5,5,4,31,31                 /* r5 = (CR >> 28) & 1   */
	ld      9,OFFSET_ppc64_GPR9(30)
	cmpdi   9,SC_FLAG
	bne     4f                          /* scv does not change CR */
        stb     5,OFFSET_ppc64_CR0_0(30)    /* gst->CR0.SO = cr0.so  */

        /* block signals again */
	/* set up for sigprocmask(SIG_SETMASK, postmask, NULL) */
4:      li      0,__NR_rt_sigprocmask
        li      3,VKI_SIG_SETMASK
        mr      4,29
        li      5,0
        mr      6,28
        sc                      /* set the mask */
        bso     7f              /* if the sigprocmask fails */
        /* now safe from signals */
	li	3,0		/* SUCCESS */

        /* pop off stack frame */
5:      ld      28,48(1)
        ld      29,56(1)
        ld      30,64(1)
        ld      31,72(1)
        ld      9,32(1)
        ld      27,16(1)        /* Fetch LR from frame */
        mtlr    27              /* restore LR */
        ld      27,40(1)        /* restore r27 from stack frame */
        addi    1,1,80
        blr

	/* failure: return 0x8000 | error code */
7:	ori	3,3,0x8000	/* FAILURE -- ensure return value is nonzero */
        b       5b
#if _CALL_ELF == 2
        .size .ML_(do_syscall_for_client_WRK),.-.ML_(do_syscall_for_client_WRK)
#else
        .size .ML_(do_syscall_for_client_WRK),.-.ML_(do_syscall_for_client_WRK)
#endif
.section .rodata
/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */

.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):     .quad 1b
ML_(blksys_restart):   .quad 2b
ML_(blksys_complete):  .quad 3b
ML_(blksys_committed): .quad 4b
ML_(blksys_finished):  .quad 5b

#endif // defined(VGP_ppc64le_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
