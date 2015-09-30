/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.      syscall-tilegx-linux.S  ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2012 Tilera Corp.

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

/* Contributed by Zhi-Gang Liu <zliu at tilera dot com> */

#include "pub_core_basics_asm.h"

#if defined(VGP_tilegx_linux)

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
        Int syscallno,                 // r0
        void* guest_state,             // r1
        const vki_sigset_t *sysmask,   // r2
        const vki_sigset_t *postmask,  // r3
        Int nsigwords)                 // r4
*/
/* from vki_arch.h */
#define VKI_SIG_SETMASK 2

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):

    addli  sp,  sp, -64   // alloc 64B new stack space
    addli  r29, sp, 56    // r29 points to offset 56 above sp
    st_add r29, r0, -8    // save r0
                          // offset 48
    st_add r29, r1, -8    // save r1
                          // offset 40
    st_add r29, r2, -8    // save r2
                          // offset 32
    st_add r29, r3, -8    // save r3
                          // offset 24
    st_add r29, r4, -8    // save r4
                          // offset 16
    st     r29, lr        // save lr
1:
    {
     moveli r10, __NR_rt_sigprocmask
     moveli r0, VKI_SIG_SETMASK
    }
    {
     move   r1, r2
     move   r2, r3
    }
    move   r3, r4
    swint1

    // error, go 7f
    bnez  r1, 7f

    /* Get registers from guest_state. */
    addli  r29, sp, 56    // get syscallno
    ld     r10, r29
    addli  r29, sp, 48
    ld     r29, r29       // r29 points to guest_state
    ld_add r0, r29, 8     // read r0
    ld_add r1, r29, 8     // read r1
    ld_add r2, r29, 8     // read r2
    ld_add r3, r29, 8     // read r3
    ld_add r4, r29, 8     // read r4
    ld_add r5, r29, 8     // read r5

2:  swint1                // syscall
3:
    // Write register into guest_state
    addli  r29, sp, 48
    ld     r29, r29
    st_add r29, r0, 8
    st_add r29, r1, 8
    st_add r29, r2, 8
    st_add r29, r3, 8
    st_add r29, r4, 8
    st_add r29, r5, 8
    nop
4:
    {
     moveli r10, __NR_rt_sigprocmask
     moveli r0, VKI_SIG_SETMASK
    }
    addli  r29, sp, 32
    {
     ld     r1, r29
     movei  r2, 0
    }
    addli  r29, sp, 24
    ld     r3, r29

    swint1
    // error, go 7f
    bnez  r1, 7f
    nop
5:  addli  r29, sp, 16
    {
     ld     lr, r29       // restore lr
     addli  sp, sp,  64
    }
    jr lr

7:  addi   r29, sp, 16
    {
     ld     lr, r29       // restore lr
     addi   sp, sp, 64
    }
    {
     // r0 = 0x8000
     shl16insli r0, zero, -0x8000
     jr lr
    }

    .section .rodata
    /* export the ranges so that
       VG_(fixup_guest_state_after_syscall_interrupted) can do the
       right thing */

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
    .previous

#endif /* defined(VGP_tilegx_linux) */

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

