
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.        syscall-arm64-darwin.S ---*/
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

#if defined(VGP_arm64_darwin)

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
        should also be in guest_state->guest_X8.  The syscall result
	is written back to guest_state->guest_X0 on completion.

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
              Int syscallno,                 // x0
              void* guest_state,             // x1
              const vki_sigset_t *sysmask,   // x2
              const vki_sigset_t *postmask,  // x3
              Int nsigwords)                 // x4
*/

/* from vki_arch.h */
#define VKI_SIG_SETMASK	3

/* DO_SYSCALL MACH|MDEP|UNIX */
#define MACH 1
#define MDEP 2
#define UNIX 3

.macro DO_SYSCALL
   /* Stash callee-saves and our args on the stack */
   stp  x29, x30, [sp, #-16]!
   stp  x27, x28, [sp, #-16]!
   stp  x25, x26, [sp, #-16]!
   stp  x23, x24, [sp, #-16]!
   stp  x21, x22, [sp, #-16]!
   stp  x19, x20, [sp, #-16]!
   stp  x4,  x5,  [sp, #-16]!
   stp  x2,  x3,  [sp, #-16]!
   stp  x0,  x1,  [sp, #-16]!

L_$0_1:	/* Even though we can't take a signal until the sigprocmask completes,
	   start the range early.
	   If rip is in the range [1,2), the syscall hasn't been started yet */

	/* Set the signal mask which should be current during the syscall. */
	/* GrP fixme signals
           DDD: JRS fixme: use __NR___pthread_sigmask, not __NR_rt_sigprocmask

  ldr x16, =__NR_rt_sigprocmask
  ldr x0, =VKI_SIG_SETMASK
  mov x1, x2 // sysmask
  mov x2, x3 // postmask
  mov x3, x4 // nsigwords
  svc 0x80

  cmp x0, #0
  blt 7f
	*/

	/* OK, that worked.  Now do the syscall proper. */

  ldr x8, [sp, #8] /* saved x1 == guest_state */

  ldr x16, [sp, #0] /* saved x0 == syscall# */
  ldr x0, [x8, #OFFSET_arm64_X0]
  ldr x1, [x8, #OFFSET_arm64_X1]
  ldr x2, [x8, #OFFSET_arm64_X2]
  ldr x3, [x8, #OFFSET_arm64_X3]
  ldr x4, [x8, #OFFSET_arm64_X4]
  ldr x5, [x8, #OFFSET_arm64_X5]
  ldr x6, [x8, #OFFSET_arm64_X6]
  ldr x7, [x8, #OFFSET_arm64_X7]
  ldr x8, [x8, #OFFSET_arm64_X8]

	/* If rip==2, then the syscall was either just about
	   to start, or was interrupted and the kernel was
	   restarting it. */
L_$0_2:	svc 0x80
L_$0_3:	/* In the range [3, 4), the syscall result is in %rax,
	   but hasn't been committed to RAX. */

  /* stash returned carry flag */
  mov x4, 1
  csel x4, x4, xzr, cs

  ldr x5, [sp, #8] /* saved x1 == guest_state */
  str x0, [x5, #OFFSET_arm64_X0]
  str x1, [x5, #OFFSET_arm64_X1]

.if $0 == UNIX
	/* save carry flag to VEX */
  ldr x0, [sp, #8] /* arg1 = vex state */
  mov x1, x4       /* arg2 = new flag */
	bl	_LibVEX_GuestARM64_put_nzcv_c
.endif

L_$0_4:	/* Re-block signals.  If eip is in [4,5), then the syscall
	   is complete and we needn't worry about it. */
	/* GrP fixme signals
     DDD: JRS fixme: use __NR___pthread_sigmask, not __NR_rt_sigprocmask

    ldr x16, =__NR_rt_sigprocmask
    ldr x0, =VKI_SIG_SETMASK
    ldr x1, [sp, #24] // saved x3 == postmask
    mov x2, #0
    ldr x3, [sp, #32] // saved x4 == nsigwords
    svc 0x80

    cmp x0, #0
    blt 7f
	*/

L_$0_5:	/* now safe from signals */
  mov  x0, #0
  ldp  xzr, x1,  [sp], #16
  ldp  x2,  x3,  [sp], #16
  ldp  x4,  x5,  [sp], #16
  ldp  x19, x20, [sp], #16
  ldp  x21, x22, [sp], #16
  ldp  x23, x24, [sp], #16
  ldp  x25, x26, [sp], #16
  ldp  x27, x28, [sp], #16
  ldp  x29, x30, [sp], #16
  ret

7: /* Failure: return 0x8000 | error code */
  orr  x0, x0, #0x8000
  ldp  xzr, x1,  [sp], #16
  ldp  x2,  x3,  [sp], #16
  ldp  x4,  x5,  [sp], #16
  ldp  x19, x20, [sp], #16
  ldp  x21, x22, [sp], #16
  ldp  x23, x24, [sp], #16
  ldp  x25, x26, [sp], #16
  ldp  x27, x28, [sp], #16
  ldp  x29, x30, [sp], #16
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


#endif // defined(VGP_arm64_darwin)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
