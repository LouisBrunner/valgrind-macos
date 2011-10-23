
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.        syscall-arm-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2008-2011 Evan Geller (gaze@bea.ms)

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

#if defined(VGP_arm_linux)

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
        the syscall by examining the value of IP in the signal
        handler.  This means that we can always do the appropriate
        thing to precisely emulate the kernel's signal/syscall
        interactions.

        The syscall number is taken from the argument, even though it
        should also be in regs->m_R7.  The syscall result is written
        back to regs->m_R0 on completion.

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
              Int syscallno,                 // r0
              void* guest_state,             // r1
              const vki_sigset_t *sysmask,   // r2
              const vki_sigset_t *postmask,  // r3
              Int nsigwords)                 // [sp, #0]
*/
/* from vki_arch.h */
#define VKI_SIG_SETMASK 2

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):

   /* Stash callee-saves and our args on the stack */
   push {r0, r1, r3, r4, r5, r7, fp, lr}

1:

   mov r7, #__NR_rt_sigprocmask
   mov r0, #VKI_SIG_SETMASK 
   mov r1, r2 /* sysmask */
   mov r2, r3 /* postmask */
   ldr r3, [sp, #32] /* nsigwords */
   svc 0x00000000


   ldr r5, [sp, #4] /* guest_state */

   ldr r7, [sp, #0] /* syscall# */
   ldr r0, [r5, #OFFSET_arm_R0]
   ldr r1, [r5, #OFFSET_arm_R1]
   ldr r2, [r5, #OFFSET_arm_R2]
   ldr r3, [r5, #OFFSET_arm_R3]
   ldr r4, [r5, #OFFSET_arm_R4]
   ldr r5, [r5, #OFFSET_arm_R5]

2: svc 0x00000000
3:
   ldr r5, [sp, #4] /* guest_state */
   str r0, [r5, #OFFSET_arm_R0]

4:
   mov r7, #__NR_rt_sigprocmask
   mov r0, #VKI_SIG_SETMASK 
   ldr r1, [sp, #8] /* postmask */
   mov r2, #0
   ldr r3, [sp, #32] /* nsigwords */
   svc 0x00000000

  cmp r0, #0
   blt 7f
   add sp, sp, #4 /* r0 contains return value */

5: /* Success */
   mov r0, #0
   pop {r1, r3, r4, r5, r7, fp, pc}

7: /* Failure: return 0x8000 | error code */
   orr r0, r0, #0x8000
   pop {r1, r3, r4, r5, r7, fp, pc}


.section .rodata
/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */

.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):      .long 1b
ML_(blksys_restart):    .long 2b
ML_(blksys_complete):   .long 3b
ML_(blksys_committed):  .long 4b
ML_(blksys_finished):   .long 5b

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",%progbits

.previous

#endif // defined(VGP_arm_linux)
   
/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
