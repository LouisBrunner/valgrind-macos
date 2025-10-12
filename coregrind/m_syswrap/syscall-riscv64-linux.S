
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.      syscall-riscv64-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2020-2023 Petr Pavlu
      petr.pavlu@dagobah.cz

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 3 of the
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

#if defined(VGP_riscv64_linux)

#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"


/*----------------------------------------------------------------*/
/*
        Perform a syscall for the client.  This will run a syscall
        with the client's specific per-thread signal mask.

        The structure of this function is such that, if the syscall is
        interrupted by a signal, we can determine exactly what
        execution state we were in with respect to the execution of
        the syscall by examining the value of pc in the signal
        handler.  This means that we can always do the appropriate
        thing to precisely emulate the kernel's signal/syscall
        interactions.

        The syscall number is taken from the argument, even though it
        should also be in guest_state->guest_x17.  The syscall result
        is written back to guest_state->guest_x10 on completion.

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
              Int syscallno,                 // a0
              void* guest_state,             // a1
              const vki_sigset_t *sysmask,   // a2
              const vki_sigset_t *postmask,  // a3
              Int nsigwords)                 // a4
*/
/* from vki-riscv64-linux.h */
#define VKI_SIG_SETMASK 2

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):

   /* Stash callee-saves and our args on the stack */
   addi sp, sp, -144
   sd ra, 136(sp)
   sd s0, 128(sp)
   sd s1, 120(sp)
   sd s2, 112(sp)
   sd s3, 104(sp)
   sd s4, 96(sp)
   sd s5, 88(sp)
   sd s6, 80(sp)
   sd s7, 72(sp)
   sd s8, 64(sp)
   sd s9, 56(sp)
   sd s10, 48(sp)
   sd s11, 40(sp)
   sd a0, 32(sp)
   sd a1, 24(sp)
   sd a2, 16(sp)
   sd a3, 8(sp)
   sd a4, 0(sp)

1:

   li a7, __NR_rt_sigprocmask
   li a0, VKI_SIG_SETMASK
   mv a1, a2 /* sysmask */
   mv a2, a3 /* postmask */
   mv a3, a4 /* nsigwords */
   ecall


   ld a5, 24(sp) /* saved a1 == guest_state */

   ld a7, 32(sp) /* saved a0 == syscall# */
   ld a0, OFFSET_riscv64_x10(a5)
   ld a1, OFFSET_riscv64_x11(a5)
   ld a2, OFFSET_riscv64_x12(a5)
   ld a3, OFFSET_riscv64_x13(a5)
   ld a4, OFFSET_riscv64_x14(a5)
   ld a5, OFFSET_riscv64_x15(a5)

2: ecall
3:
   ld a5, 24(sp) /* saved a1 == guest_state */
   sd a0, OFFSET_riscv64_x10(a5)

4:
   li a7, __NR_rt_sigprocmask
   li a0, VKI_SIG_SETMASK
   ld a1, 8(sp) /* saved a3 == postmask */
   li a2, 0
   ld a3, 0(sp) /* saved a4 == nsigwords */
   ecall

   bltz x0, 7f

5: /* Success: return zero */
   li a0, 0
   ld ra, 136(sp)
   ld s0, 128(sp)
   ld s1, 120(sp)
   ld s2, 112(sp)
   ld s3, 104(sp)
   ld s4, 96(sp)
   ld s5, 88(sp)
   ld s6, 80(sp)
   ld s7, 72(sp)
   ld s8, 64(sp)
   ld s9, 56(sp)
   ld s10, 48(sp)
   ld s11, 40(sp)
   addi sp, sp, 144
   ret

7: /* Failure: return 0x8000 | error code */
   li a1, 0x8000
   or a0, a0, a1
   ld ra, 136(sp)
   ld s0, 128(sp)
   ld s1, 120(sp)
   ld s2, 112(sp)
   ld s3, 104(sp)
   ld s4, 96(sp)
   ld s5, 88(sp)
   ld s6, 80(sp)
   ld s7, 72(sp)
   ld s8, 64(sp)
   ld s9, 56(sp)
   ld s10, 48(sp)
   ld s11, 40(sp)
   addi sp, sp, 144
   ret



.section .rodata
/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */

.align 3
.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):      .quad 1b
ML_(blksys_restart):    .quad 2b
ML_(blksys_complete):   .quad 3b
ML_(blksys_committed):  .quad 4b
ML_(blksys_finished):   .quad 5b

#endif // defined(VGP_riscv64_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
