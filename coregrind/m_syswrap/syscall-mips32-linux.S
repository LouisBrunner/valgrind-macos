
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.      syscall-mips32-linux.S  ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2012 RT-RK
      mips-valgrind@rt-rk.com

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

#if defined(VGP_mips32_linux)

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
              Int syscallno,                 // $4 - a0
              void* guest_state,             // $5 - a1
              const vki_sigset_t *sysmask,   // $6 - a2
              const vki_sigset_t *postmask,  // $7 - a3
              Int nsigwords)                 // stack
*/
/* from vki_arch.h */
#define VKI_SIG_SETMASK 3

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):

    subu $29, $29, 56  #set up the steck frame, 
    sw $4, 52($29)
    sw $5, 48($29)
    sw $6, 44($29)
    sw $7, 40($29)
    sw $31, 36($29)
    sw $30, 32($29)
    sw $16, 28($29)
    sw $17, 24($29)
    sw $18, 20($29)
    sw $19, 16($29)
    sw $20, 12($29)
    sw $21, 8($29)
    sw $22, 4($29)
    sw $23, 0($29)
    addu $30, $29, 56


    /* Set the signal mask which should be current during the syscall. */

1:  li $2, __NR_rt_sigprocmask  
    li $4, VKI_SIG_SETMASK
    move $5, $6 /* sysmask */
    move $6, $7 /* postmask */
    lw $7, 72($29) /* nsigwords */
    syscall
    nop

# TODO: this should remain, keep it just for now
    bnez $7, 7f
    nop

    lw $8, 48($29)    /* t0 == ThreadState */

    lw $2, 52($29) /* v0 == syscallno */
    lw $4, OFFSET_mips32_r4($8)
    lw $5, OFFSET_mips32_r5($8)
    lw $6, OFFSET_mips32_r6($8)
    lw $7, OFFSET_mips32_r7($8)
    subu $29, $29, 24  #set up the steck frame, 
    lw $9, OFFSET_mips32_r29($8)
    lw $10, 16($9)
    sw $10, 16($29)
    lw $10, 20($9)
    sw $10, 20($29)

2:  syscall

3:  addu $29, $29, 24  #set up the steck frame,

    lw $8, 48($29) /* t0 == ThreadState */
    sw $2, OFFSET_mips32_r2($8)
    sw $3, OFFSET_mips32_r3($8)
    sw $7, OFFSET_mips32_r7($8)

4:  li $2, __NR_rt_sigprocmask
    li $4, VKI_SIG_SETMASK
    lw $5, 40($29)
    li $6, 0
    lw $7, 72($29) /* nsigwords */
    syscall
    nop

    bnez $7, 7f
    nop

    /* Success */
    li $2, 0

5:  lw $4, 52($29)
    lw $5, 48($29)
    lw $6, 44($29)
    lw $7, 40($29)
    lw $31, 36($29)
    lw $30, 32($29)
    lw $16, 28($29)
    lw $17, 24($29)
    lw $18, 20($29)
    lw $19, 16($29)
    lw $20, 12($29)
    lw $21, 8($29)
    lw $22, 4($29)
    lw $23, 0($29)
    addu $29, $29, 56 #release the stack frame.

    jr $31
    nop

7:  /* Failure: return 0x8000 | error code */
    li $2, 0x8000

    lw $4, 52($29)
    lw $5, 48($29)
    lw $6, 44($29)
    lw $7, 40($29)
    lw $31, 36($29)
    lw $30, 32($29)
    lw $16, 28($29)
    lw $17, 24($29)
    lw $18, 20($29)
    lw $19, 16($29)
    lw $20, 12($29)
    lw $21, 8($29)
    lw $22, 4($29)
    lw $23, 0($29)
    addu $29, $29, 56 #release the stack frame.

    jr $31
    nop

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
.previous
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",%progbits



#endif // defined(VGP_mips32_linux)
   
/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
