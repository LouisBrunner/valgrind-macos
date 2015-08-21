
/*--------------------------------------------------------------------*/
/*--- begin                                dispatch-tilegx-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2010-2015  Tilera Corp.

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

#if defined(VGP_tilegx_linux)
#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"       /* for OFFSET_tilegx_PC */

        /*------------------------------------------------------------*/
        /*---                                                      ---*/
        /*--- The dispatch loop.  VG_(run_innerloop) is used to    ---*/
        /*--- run all translations except no-redir ones.           ---*/
        /*---                                                      ---*/
        /*------------------------------------------------------------*/

        /*----------------------------------------------------*/
        /*--- Preamble (set everything up)                 ---*/
        /*----------------------------------------------------*/

        /* signature:
        void VG_(disp_run_translations)(UWord* two_words,
        void*  guest_state,
        Addr   host_addr );
        UWord VG_(run_innerloop) ( void* guest_state, UWord do_profiling );
        */

        .text
        .globl  VG_(disp_run_translations)
        VG_(disp_run_translations):

        /* r0 holds two_words
           r1 holds guest_state
           r2 holds host_addr */

        /* New stack frame */
        addli sp, sp, -256
        addi  r29, sp, 8
        /*
        high memory of stack
        216  lr
        208  r53
        200  r52
        192  r51
        ...
        48   r33
        40   r32
        32   r31
        24   r30
        16   r1 <---
        8    r0
        0       <-sp
        */
        st_add r29, r0, 8
        st_add r29, r1, 8

        /* ... and r30 - r53 */
        st_add  r29, r30, 8
        st_add  r29, r31, 8
        st_add  r29, r32, 8
        st_add  r29, r33, 8
        st_add  r29, r34, 8
        st_add  r29, r35, 8
        st_add  r29, r36, 8
        st_add  r29, r37, 8
        st_add  r29, r38, 8
        st_add  r29, r39, 8
        st_add  r29, r40, 8
        st_add  r29, r41, 8
        st_add  r29, r42, 8
        st_add  r29, r43, 8
        st_add  r29, r44, 8
        st_add  r29, r45, 8
        st_add  r29, r46, 8
        st_add  r29, r47, 8
        st_add  r29, r48, 8
        st_add  r29, r49, 8
        st_add  r29, r50, 8
        st_add  r29, r51, 8
        st_add  r29, r52, 8
        st_add  r29, r53, 8
        st      r29, lr

        /* Load the address of guest state into guest state register r50. */
        move r50, r1

        //j postamble

        /* jump to the code cache. */
        jr  r2
        /*NOTREACHED*/


       /*----------------------------------------------------*/
       /*--- Postamble and exit.                          ---*/
       /*----------------------------------------------------*/

postamble:
        /* At this point, r12 and r13 contain two
        words to be returned to the caller.  r12
        holds a TRC value, and r13 optionally may
        hold another word (for CHAIN_ME exits, the
        address of the place to patch.) */

        /* run_innerloop_exit_REALLY:
        r50 holds VG_TRC_* value to return
        Return to parent stack
        addli  sp, sp, 256 */

        addi r29, sp, 8

        /* Restore r0 from stack; holding address of twp words */
        ld_add  r0, r29, 16
        /* store r12 in two_words[0] */
        st_add  r0, r12, 8
        /* store r13 in two_words[1] */
        st  r0, r13

        /* Restore callee-saved registers... */
        ld_add  r30, r29, 8
        ld_add  r31, r29, 8
        ld_add  r32, r29, 8
        ld_add  r33, r29, 8
        ld_add  r34, r29, 8
        ld_add  r35, r29, 8
        ld_add  r36, r29, 8
        ld_add  r37, r29, 8
        ld_add  r38, r29, 8
        ld_add  r39, r29, 8
        ld_add  r40, r29, 8
        ld_add  r41, r29, 8
        ld_add  r42, r29, 8
        ld_add  r43, r29, 8
        ld_add  r44, r29, 8
        ld_add  r45, r29, 8
        ld_add  r46, r29, 8
        ld_add  r47, r29, 8
        ld_add  r48, r29, 8
        ld_add  r49, r29, 8
        ld_add  r50, r29, 8
        ld_add  r51, r29, 8
        ld_add  r52, r29, 8
        ld_add  r53, r29, 8
        ld      lr, r29
        addli   sp, sp, 256   /* stack_size */
        jr      lr
        nop


       /*----------------------------------------------------*/
       /*---           Continuation points                ---*/
       /*----------------------------------------------------*/

       /* ------ Chain me to slow entry point ------ */
       .global VG_(disp_cp_chain_me_to_slowEP)
       VG_(disp_cp_chain_me_to_slowEP):
        /* We got called.  The return address indicates
        where the patching needs to happen.  Collect
        the return address and, exit back to C land,
        handing the caller the pair (Chain_me_S, RA) */
        # if (VG_TRC_CHAIN_ME_TO_SLOW_EP > 128)
        # error ("VG_TRC_CHAIN_ME_TO_SLOW_EP is > 128");
        # endif
        moveli r12, VG_TRC_CHAIN_ME_TO_SLOW_EP
        move   r13, lr
        /* 32 = mkLoadImm_EXACTLY4
        8 = jalr r9
        8 = nop */
        addi   r13, r13, -40
        j      postamble

        /* ------ Chain me to slow entry point ------ */
        .global VG_(disp_cp_chain_me_to_fastEP)
        VG_(disp_cp_chain_me_to_fastEP):
        /* We got called.  The return address indicates
        where the patching needs to happen.  Collect
        the return address and, exit back to C land,
        handing the caller the pair (Chain_me_S, RA) */
        # if (VG_TRC_CHAIN_ME_TO_FAST_EP > 128)
        # error ("VG_TRC_CHAIN_ME_TO_FAST_EP is > 128");
        # endif
        moveli r12, VG_TRC_CHAIN_ME_TO_FAST_EP
        move   r13, lr
        /* 32 = mkLoadImm_EXACTLY4
        8 = jalr r9
        8 = nop */
        addi   r13, r13, -40
        j      postamble

        /* ------ Indirect but boring jump ------ */
        .global VG_(disp_cp_xindir)
        VG_(disp_cp_xindir):
        /* Where are we going? */
        addli    r11, r50, OFFSET_tilegx_pc
        ld       r11, r11

        moveli      r7, hw2_last(VG_(stats__n_xindirs_32))
        shl16insli  r7, r7, hw1(VG_(stats__n_xindirs_32))
        shl16insli  r7, r7, hw0(VG_(stats__n_xindirs_32))
        ld4u   r6, r7
        addi   r6, r6, 1
        st4    r7, r6

        /* try a fast lookup in the translation cache */
        /* r14 = VG_TT_FAST_HASH(addr) * sizeof(ULong*)
        = (t8 >> 3 & VG_TT_FAST_MASK)  << 3 */

        move    r14, r11
        /* Assume VG_TT_FAST_MASK < 4G */
        moveli  r12, hw1(VG_TT_FAST_MASK)
        shl16insli r12, r12, hw0(VG_TT_FAST_MASK)
        shrui   r14, r14, 3
        and     r14, r14, r12
        shli    r14, r14, 4
        /* Note, each tt_fast hash entry has two pointers i.e. 16 Bytes. */

        /* r13 = (addr of VG_(tt_fast)) + r14 */
        moveli  r13, hw2_last(VG_(tt_fast))
        shl16insli   r13, r13, hw1(VG_(tt_fast))
        shl16insli   r13, r13, hw0(VG_(tt_fast))

        add     r13, r13, r14

        /* r12 = VG_(tt_fast)[hash] :: ULong* */
        ld_add  r12, r13, 8

        {
        ld      r25, r13
        sub     r7, r12, r11
        }

        bnez     r7, fast_lookup_failed

        /* Run the translation */
        jr      r25

        .quad   0x0

fast_lookup_failed:
        /* %PC is up to date */
        /* back out decrement of the dispatch counter */
        /* hold dispatch_ctr in t0 (r8) */

        moveli      r7, hw2_last(VG_(stats__n_xindir_misses_32))
        shl16insli  r7, r7, hw1(VG_(stats__n_xindir_misses_32))
        shl16insli  r7, r7, hw0(VG_(stats__n_xindir_misses_32))
        ld4u  r6, r7
        addi  r6, r6, 1
        st4   r7, r6
        moveli  r12, VG_TRC_INNER_FASTMISS
        movei   r13, 0
        j       postamble

        /* ------ Assisted jump ------ */
        .global VG_(disp_cp_xassisted)
        VG_(disp_cp_xassisted):
        /* guest-state-pointer contains the TRC. Put the value into the
        return register */
        move    r12, r50
        movei   r13, 0
        j       postamble

        /* ------ Event check failed ------ */
        .global VG_(disp_cp_evcheck_fail)
        VG_(disp_cp_evcheck_fail):
        moveli  r12, VG_TRC_INNER_COUNTERZERO
        movei   r13, 0
        j       postamble

        .size VG_(disp_run_translations), .-VG_(disp_run_translations)


        /* Let the linker know we do not need an executable stack */
        .section .note.GNU-stack,"",@progbits

#endif /* defined(VGP_tilegx_linux) */
/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

