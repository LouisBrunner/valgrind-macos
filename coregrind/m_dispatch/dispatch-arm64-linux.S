
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                       dispatch-arm64-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2013-2013 OpenWorks
      info@open-works.net

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

#if defined(VGP_arm64_linux)

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_arm_R* */


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
.global VG_(disp_run_translations)
VG_(disp_run_translations):
        /* x0  holds two_words
           x1  holds guest_state
           x2  holds host_addr
        */
        /* Push the callee-saved registers.  Unclear if x19/x20 are
           callee-saved, but be on the safe side.  Note this sequence
           maintains 16-alignment of sp.  Also save x0 since it will
           be needed in the postamble. */
        stp  x29, x30, [sp, #-16]!
        stp  x27, x28, [sp, #-16]!
        stp  x25, x26, [sp, #-16]!
        stp  x23, x24, [sp, #-16]!
        stp  x21, x22, [sp, #-16]!
        stp  x19, x20, [sp, #-16]!
        stp  x0,  xzr, [sp, #-16]!

        /* set FPSCR to vex-required default value */
        // FIXME
        // mov  r4, #0
        // fmxr fpscr, r4

       	/* Set up the guest state pointer */
        mov x21, x1

        /* and jump into the code cache.  Chained translations in
           the code cache run, until for whatever reason, they can't
           continue.  When that happens, the translation in question
           will jump (or call) to one of the continuation points
           VG_(cp_...) below. */
        br x2
        /* NOTREACHED */
        
/*----------------------------------------------------*/
/*--- Postamble and exit.                          ---*/
/*----------------------------------------------------*/

postamble:
        /* At this point, r1 and r2 contain two
           words to be returned to the caller.  r1
           holds a TRC value, and r2 optionally may
           hold another word (for CHAIN_ME exits, the
           address of the place to patch.) */

        /* We're leaving.  Check that nobody messed with
           FPSCR in ways we don't expect. */
        // FIXME
        // fmrx r4, fpscr
        // bic  r4, #0xF8000000 /* mask out NZCV and QC */
        // bic  r4, #0x0000009F /* mask out IDC,IXC,UFC,OFC,DZC,IOC */
        // cmp  r4, #0
        // beq  remove_frame /* we're OK */
        /* otherwise we have an invariant violation */
        // movw r1, #VG_TRC_INVARIANT_FAILED
        // movw r2, #0
        /* fall through */

remove_frame:
        /* Restore int regs, including importantly x0 (two_words),
           but not x1 */
        ldp  x0,  xzr, [sp], #16
        ldp  x19, x20, [sp], #16
        ldp  x21, x22, [sp], #16
        ldp  x23, x24, [sp], #16
        ldp  x25, x26, [sp], #16
        ldp  x27, x28, [sp], #16
        ldp  x29, x30, [sp], #16

        /* Stash return values */
        str  x1, [x0, #0]
        str  x2, [x0, #8]
        ret

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
        mov  x1, #VG_TRC_CHAIN_ME_TO_SLOW_EP
        mov  x2, x30 // 30 == LR
        /* 4 = movw x9, disp_cp_chain_me_to_slowEP[15:0]
           4 = movk x9, disp_cp_chain_me_to_slowEP[31:16], lsl 16
           4 = movk x9, disp_cp_chain_me_to_slowEP[47:32], lsl 32
           4 = movk x9, disp_cp_chain_me_to_slowEP[63:48], lsl 48
           4 = blr  x9
        */
        sub  x2, x2, #4+4+4+4+4
        b    postamble

/* ------ Chain me to fast entry point ------ */
.global VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_F, RA) */
        mov  x1, #VG_TRC_CHAIN_ME_TO_FAST_EP
        mov  x2, x30 // 30 == LR
        /* 4 = movw x9, disp_cp_chain_me_to_fastEP[15:0]
           4 = movk x9, disp_cp_chain_me_to_fastEP[31:16], lsl 16
           4 = movk x9, disp_cp_chain_me_to_fastEP[47:32], lsl 32
           4 = movk x9, disp_cp_chain_me_to_fastEP[63:48], lsl 48
           4 = blr  x9
        */
        sub  x2, x2, #4+4+4+4+4
        b    postamble

/* ------ Indirect but boring jump ------ */
.global VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
	/* Where are we going? */
        ldr  x0, [x21, #OFFSET_arm64_PC]

        /* stats only */
        adrp x1,           VG_(stats__n_xindirs_32)
        add  x1, x1, :lo12:VG_(stats__n_xindirs_32)
        ldr  w2, [x1, #0]
        add  w2, w2, #1
        str  w2, [x1, #0]
        
        /* try a fast lookup in the translation cache */
        // x0 = next guest, x1,x2,x3,x4 scratch
        mov  x1, #VG_TT_FAST_MASK       // x1 = VG_TT_FAST_MASK
	and  x2, x1, x0, LSR #2         // x2 = entry # = (x1 & (x0 >> 2))

        adrp x4,           VG_(tt_fast)
        add  x4, x4, :lo12:VG_(tt_fast) // x4 = &VG_(tt_fast)

	add  x1, x4, x2, LSL #4         // r1 = &tt_fast[entry#]

        ldp  x4, x5, [x1, #0]           // x4 = .guest, x5 = .host

	cmp  x4, x0

        // jump to host if lookup succeeded
        bne  fast_lookup_failed
	br   x5
        /*NOTREACHED*/

fast_lookup_failed:
        /* RM ME -- stats only */
        adrp x1,           VG_(stats__n_xindir_misses_32)
        add  x1, x1, :lo12:VG_(stats__n_xindir_misses_32)
        ldr  w2, [x1, #0]
        add  w2, w2, #1
        str  w2, [x1, #0]

	mov  x1, #VG_TRC_INNER_FASTMISS
        mov  x2, #0
	b    postamble

/* ------ Assisted jump ------ */
.global VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
        /* x21 contains the TRC */
        mov  x1, x21
        mov  x2, #0
        b    postamble

/* ------ Event check failed ------ */
.global VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
       	mov  x1, #VG_TRC_INNER_COUNTERZERO
        mov  x2, #0
	b    postamble


.size VG_(disp_run_translations), .-VG_(disp_run_translations)

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",%progbits

#endif // defined(VGP_arm64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                   dispatch-arm64-linux.S ---*/
/*--------------------------------------------------------------------*/
