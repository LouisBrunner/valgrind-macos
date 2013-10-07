
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                      dispatch-mips64-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2013 RT-RK
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

    /* New stack frame.  Stack must remain 16 aligned (at least) */
    daddiu $29, -176

    /* Save ra */
    sd  $31, 72($29)

    /* ... and s0 - s7 */
    sd $16, 80($29)
    sd $17, 88($29)
    sd $18, 96($29)
    sd $19, 104($29)
    sd $20, 112($29)
    sd $21, 120($29)
    sd $22, 128($29)
    sd $23, 136($29)

    /* ... and gp, fp/s8 */
    sd $28, 144($29)
    sd $30, 152($29)

    /* Save a0 ($4) on stack. In postamble it will be restored such that the
       return values can be written */
    sd $4, 160($29)

    /* Load address of guest state into guest state register ($23) */
    move $23, $5

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
        /* At this point, $2 and $3 contain two
           words to be returned to the caller.  $2
           holds a TRC value, and $3 optionally may
           hold another word (for CHAIN_ME exits, the
           address of the place to patch.) */

    /* Restore $4 from stack; holds address of two_words */
    ld $4, 160($29)
    sd  $2, 0($4)         /* Store $2 to two_words[0] */
    sd  $3, 8($4)         /* Store $3 to two_words[1] */

    /* Restore callee-saved registers... */

    /* Restore ra */
    ld $31, 72($29)

    /* ... and s0 - s7 */
    ld $16, 80($29)
    ld $17, 88($29)
    ld $18, 96($29)
    ld $19, 104($29)
    ld $20, 112($29)
    ld $21, 120($29)
    ld $22, 128($29)
    ld $23, 136($29)

    /* ... and gp, fp/s8 */
    ld $28, 144($29)
    ld $30, 152($29)

    daddiu $29, 176   /* stack_size */
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
        /* 8 = mkLoadImm_EXACTLY2or6
           4 = jalr $9
           4 = nop */
        daddiu  $3, $3, -32
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
        /* 8 = mkLoadImm_EXACTLY2or6
           4 = jalr $9
           4 = nop */
        daddiu  $3, $3, -32
        b    postamble

/* ------ Indirect but boring jump ------ */
.global VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
        /* Where are we going? */
        ld  $11, OFFSET_mips64_PC($23)

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
        dla $13, VG_(tt_fast)
        daddu $13, $13, $14

        ld $12, 0($13) /* t3 = VG_(tt_fast)[hash] :: ULong* */
        daddi $13, $13, 8
        ld $25, 0($13) /* little-endian, so comparing 1st 32bit word */
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
        move    $2, $23
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

#endif // defined(VGP_mips64_linux)
/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
