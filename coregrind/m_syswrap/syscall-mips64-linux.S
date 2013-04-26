
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.      syscall-mips64-linux.S  ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

   Copyright (C) 2010-2013 RT-RK
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

#if defined(VGP_mips64_linux)

#include "pub_core_basics_asm.h"
#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"

/*----------------------------------------------------------------*/
/*
   Perform a syscall for the client. This will run a syscall
   with the client's specific per-thread signal mask.

   The structure of this function is such that, if the syscall is
   interrupted by a signal, we can determine exactly what
   execution state we were in with respect to the execution of
   the syscall by examining the value of PC in the signal
   handler. This means that we can always do the appropriate
   thing to precisely emulate the kernel's signal/syscall
   interactions.

   The syscall number is taken from the argument, even though it
   should also be in regs->v0. The syscall result is written
   back to regs->v0 on completion.

   VG_(fixup_guest_state_after_syscall_interrupted) does the
   thread state fixup in the case where we were interrupted by a
   signal.

   Prototype:

   UWord ML_(do_syscall_for_client_WRK)(
             Int syscallno,                 // $4 - a0
             void* guest_state,             // $5 - a1
             const vki_sigset_t *sysmask,   // $6 - a2
             const vki_sigset_t *postmask,  // $7 - a3
             Int nsigwords)                 // $8 - a4
*/

/* from vki_arch.h */
#define VKI_SIG_SETMASK 3

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):

   /* save regs $a0 - $a4 on stack */
   daddiu  $29, $29, -48
   sd      $4, 0($29)                /* syscallno */
   sd      $5, 8($29)                /* guest_state */
   sd      $6, 16($29)               /* sysmask */
   sd      $7, 24($29)               /* postmask */
   sd      $8, 32($29)               /* nsigwords */

1: li      $2, __NR_rt_sigprocmask
   li      $4, VKI_SIG_SETMASK
   move    $5, $6                    /* sysmask */
   move    $6, $7                    /* postmask */
   move    $7, $8                    /* nsigwords */
   syscall

   bnez    $7, 7f
   nop

   /* Actually do the client syscall */ 
   ld      $5, 8($29)                /* guest_state */
   ld      $4, OFFSET_mips64_r4($5)  /* a0 */

   ld      $6, OFFSET_mips64_r6($5)  /* a2 */
   ld      $7, OFFSET_mips64_r7($5)  /* a3 */
   ld      $8, OFFSET_mips64_r8($5)  /* a4 */
   ld      $9, OFFSET_mips64_r9($5)  /* a5 */

   ld      $5, OFFSET_mips64_r5($5)  /* a1 */
   ld      $2, 0($29)                /* syscallno */

2: syscall

   /* Saving return values into Guest state  */
3: ld      $5, 8($29)                /* guest_state */
   sd      $2, OFFSET_mips64_r2($5)  /* v0 */
   sd      $7, OFFSET_mips64_r7($5)  /* a3 */

4: li      $2, __NR_rt_sigprocmask
   li      $4, VKI_SIG_SETMASK
   ld      $5, 24($29)               /* postmask */
   move    $6, $0                    /* 0 (zero) */
   ld      $7, 32($29)               /* nsigwords */
   syscall

   bnez    $7, 7f
   nop

5: /* restore 29 and return */
   daddiu  $29, $29, 48
   jr      $31
   nop

7: /* error */
   daddiu  $29, $29, 48
   li      $2, 0x8000
   jr      $31
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
ML_(blksys_setup):      .quad 1b
ML_(blksys_restart):    .quad 2b
ML_(blksys_complete):   .quad 3b
ML_(blksys_committed):  .quad 4b
ML_(blksys_finished):   .quad 5b
.previous
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",%progbits

#endif // defined(VGP_mips64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
