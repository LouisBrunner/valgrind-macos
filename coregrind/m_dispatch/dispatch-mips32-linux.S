
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                        dispatch-mips-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2012 RT-RK
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
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_mips_PC */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(disp_run_translations) is    ---*/
/*--- used to run all translations,                        ---*/
/*--- including no-redir ones.                             ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*----------------------------------------------------*/
/*--- Entry and preamble (set everything up)       ---*/
/*----------------------------------------------------*/

/* signature:
void VG_(disp_run_translations)( UWord* two_words,
                                 void*  guest_state, 
                                 Addr   host_addr );
*/

.text
.globl VG_(disp_run_translations)
VG_(disp_run_translations):
    /* a0 ($4) holds two_words   */
    /* a1 ($5) holds guest_state */
    /* a2 ($6) holds host_addr   */

    /* New stack frame.  Stack must remain 8 aligned (at least) */
    addiu $29, -56

    /* Save ra */
    sw  $31, 0($29)

    /* ... and orig guest state*/
    sw $5, 4($29)

    /* ... and s0 - s7 */
    sw $16, 8($29)
    sw $17, 12($29)
    sw $18, 16($29)
    sw $19, 20($29)
    sw $20, 24($29)
    sw $21, 28($29)
    sw $22, 32($29)
    sw $23, 36($29)

    /* ... and gp, fp/s8 */
    sw $28, 40($29)
    sw $30, 44($29)

    /* Save a0 ($4) on stack. In postamble it will be restored such that the
       return values can be written */
    sw $4, 48($29)

    /* Load address of guest state into guest state register (r10) */
    move $10, $5

    /* and jump into the code cache.  Chained translations in
           the code cache run, until for whatever reason, they can't
           continue.  When that happens, the translation in question
           will jump (or call) to one of the continuation points
           VG_(cp_...) below. */
    jr $6
    /*NOTREACHED*/

/*----------------------------------------------------*/
/*--- Postamble and exit.                          ---*/
/*----------------------------------------------------*/

postamble:
        /* At this point, r2 and r3 contain two
           words to be returned to the caller.  r2
           holds a TRC value, and r3 optionally may
           hold another word (for CHAIN_ME exits, the
           address of the place to patch.) */

    /* Restore $4 from stack; holds address of two_words */
    lw $4, 48($29)
    sw  $2, 0($4)         /* Store $2 to two_words[0] */
    sw  $3, 4($4)         /* Store $3 to two_words[1] */

    /* Restore callee-saved registers... */

    /* Restore ra */
    lw $31, 0($29)

    /* ... and s0 - s7 */
    lw $16, 8($29)
    lw $17, 12($29)
    lw $18, 16($29)
    lw $19, 20($29)
    lw $20, 24($29)
    lw $21, 28($29)
    lw $22, 32($29)
    lw $23, 36($29)

    /* ... and gp, fp/s8 */
    lw $28, 40($29)
    lw $30, 44($29)


    addiu $29, 56   /* stack_size */
    jr $31
    nop

/*----------------------------------------------------*/
/*--- Continuation points                          ---*/
/*----------------------------------------------------*/

/* ------ Chain me to slow entry point ------ */
.global VG_(disp_cp_chain_me_to_slowEP)
VG_(disp_cp_chain_me_to_slowEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_S, RA) */
        li $2, VG_TRC_CHAIN_ME_TO_SLOW_EP
        move $3, $31
        /* 8 = mkLoadImm_EXACTLY2or5
           4 = jalr $9
           4 = nop */
        addiu  $3, $3, -16
        b    postamble

/* ------ Chain me to slow entry point ------ */
.global VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_S, RA) */
        li $2, VG_TRC_CHAIN_ME_TO_FAST_EP
        move $3, $31
        /* 8 = mkLoadImm_EXACTLY2or5
           4 = jalr $9
           4 = nop */
        addiu  $3, $3, -16
        b    postamble

/* ------ Indirect but boring jump ------ */
.global VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
        /* Where are we going? */
        lw  $11, OFFSET_mips32_PC($10)

        lw $13, vgPlain_stats__n_xindirs_32
        addiu $13, $13, 0x1
        sw $13, vgPlain_stats__n_xindirs_32

        /* try a fast lookup in the translation cache */
        /* t1 = VG_TT_FAST_HASH(addr) * sizeof(ULong*)
                = (t8 >> 2 & VG_TT_FAST_MASK)  << 3 */

        move $14, $11
        li $12, VG_TT_FAST_MASK
        srl $14, $14, 2
        and $14, $14, $12
        sll $14, $14, 3

        /* t2 = (addr of VG_(tt_fast)) + t1 */
        la $13, VG_(tt_fast)
        addu $13, $13, $14

        lw $12, 0($13) /* t3 = VG_(tt_fast)[hash] :: ULong* */
        addi $13, $13, 4
        lw $25, 0($13) /* little-endian, so comparing 1st 32bit word */
        nop

check:
        bne $12, $11, fast_lookup_failed
        /* run the translation */
        jr $25
        .long   0x0   /* persuade insn decoders not to speculate past here */

fast_lookup_failed:
        /* %PC is up to date */
        /* back out decrement of the dispatch counter */
        /* hold dispatch_ctr in t0 (r8) */
        lw $13, vgPlain_stats__n_xindirs_32
        addiu $13, $13, 0x1
        sw $13, vgPlain_stats__n_xindirs_32
        li $2, VG_TRC_INNER_FASTMISS
        li $3, 0
        b       postamble

/* ------ Assisted jump ------ */
        .global VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
        /* guest-state-pointer contains the TRC. Put the value into the
           return register */
        move    $2, $10
        move    $3, $0
        b       postamble

/* ------ Event check failed ------ */
        .global VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
        li      $2, VG_TRC_INNER_COUNTERZERO
        move    $3, $0
        b       postamble

.size VG_(disp_run_translations), .-VG_(disp_run_translations)


/* Let the linker know we do not need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif // defined(VGP_mips32_linux)
/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
