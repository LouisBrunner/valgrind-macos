
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.    syscall-nanomips-linux.S  ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2018 RT-RK

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

#if defined(VGP_nanomips_linux)

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
        should also be in regs->v0.  The syscall result is written
        back to regs->v0 on completion.

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
/* from vki_arch.h */
#define VKI_SIG_SETMASK       2
.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):

    save 32, $a0-$a4

1:  li $t4, __NR_rt_sigprocmask
    li $a0, VKI_SIG_SETMASK
    move $a1, $a2 # sysmask
    move $a2, $a3 # postmask
    move $a3, $a4 # nsigwords
    syscall[32]

    bnezc $a0, 5f # error

    lw $t4, 28($sp)   # t4 = syscallno
    lw $t0, 24($sp)   # t0 = &guest_state

    lw $a0, OFFSET_mips32_r4($t0)
    lw $a1, OFFSET_mips32_r5($t0)
    lw $a2, OFFSET_mips32_r6($t0)
    lw $a3, OFFSET_mips32_r7($t0)
    lw $a4, OFFSET_mips32_r8($t0)
    lw $a5, OFFSET_mips32_r9($t0)
    lw $a6, OFFSET_mips32_r10($t0)
    lw $a7, OFFSET_mips32_r11($t0)
2:  syscall[32]

3:  lw $t0, 24($sp)   # t0 = &guest_state

    sw $a0, OFFSET_mips32_r4($t0)

4:  li $t4, __NR_rt_sigprocmask
    li $a0, VKI_SIG_SETMASK
    lw $a1, 16($sp)
    li $a2, 0
    lw $a3, 12($sp)
    syscall[32]

    bnezc $a0, 5f # error

    li $a0, 0
    bc 6f

5:  #error, return 0x8000
    lui $a0, 0x00008

6:  addu $sp, $sp, 32
    jrc $ra

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
ML_(blksys_finished):   .long 6b

#endif

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                 syscall-nanomips-linux.S ---*/
/*--------------------------------------------------------------------*/
