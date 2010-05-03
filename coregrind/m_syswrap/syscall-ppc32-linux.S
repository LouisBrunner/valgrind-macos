
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.        syscall-ppc32-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2005-2010 Paul Mackerras (paulus@samba.org)

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

#if defined(VGP_ppc32_linux)

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
#define VKI_SIG_SETMASK 2

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):
        /* make a stack frame */
        stwu    1,-32(1)
        stw     31,28(1)
        stw     30,24(1)
        stw     29,20(1)
        stw     28,16(1)
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
        lwz     3,OFFSET_ppc32_GPR3(30)
        lwz     4,OFFSET_ppc32_GPR4(30)
        lwz     5,OFFSET_ppc32_GPR5(30)
        lwz     6,OFFSET_ppc32_GPR6(30)
        lwz     7,OFFSET_ppc32_GPR7(30)
        lwz     8,OFFSET_ppc32_GPR8(30)
        mr      0,31            /* syscall number */
2:      sc                      /* do the syscall */

        /* put the result back in the threadstate  */
3:	stw     3,OFFSET_ppc32_GPR3(30)     /* gst->GPR3 = sc result */
	/* copy cr0.so back to simulated state */
	mfcr    5                           /* r5 = CR               */
	rlwinm	5,5,4,31,31                 /* r5 = (CR >> 28) & 1   */
        stb     5,OFFSET_ppc32_CR0_0(30)    /* gst->CR0.SO = cr0.so  */

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
5:      lwz     28,16(1)
        lwz     29,20(1)
        lwz     30,24(1)
        lwz     31,28(1)
        addi    1,1,32
        blr

	/* failure: return 0x8000 | error code */
7:      ori	3,3,0x8000	/* FAILURE -- ensure return value is nonzero */
        b       5b

.section .rodata
/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */

.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):     .long 1b
ML_(blksys_restart):   .long 2b
ML_(blksys_complete):  .long 3b
ML_(blksys_committed): .long 4b
ML_(blksys_finished):  .long 5b

.previous
		
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif // defined(VGP_ppc32_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
