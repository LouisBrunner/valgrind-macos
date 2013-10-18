
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                         dispatch-arm-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2008-2013 Evan Geller
     gaze@bea.ms

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

#if defined(VGP_arm_linux)
	.fpu vfp

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
        /* r0  holds two_words
           r1  holds guest_state
           r2  holds host_addr
        */
        /* The number of regs in this list needs to be even, in
           order to keep the stack 8-aligned. */
	push {r0, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, lr}

        /* set FPSCR to vex-required default value */
        mov  r4, #0
        fmxr fpscr, r4

       	/* Set up the guest state pointer */
        mov r8, r1

        /* and jump into the code cache.  Chained translations in
           the code cache run, until for whatever reason, they can't
           continue.  When that happens, the translation in question
           will jump (or call) to one of the continuation points
           VG_(cp_...) below. */
        bx r2
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
        fmrx r4, fpscr
        bic  r4, #0xF8000000 /* mask out NZCV and QC */
        bic  r4, #0x0000009F /* mask out IDC,IXC,UFC,OFC,DZC,IOC */
        cmp  r4, #0
        beq  remove_frame /* we're OK */
        /* otherwise we have an invariant violation */
        movw r1, #VG_TRC_INVARIANT_FAILED
        movw r2, #0
        /* fall through */

remove_frame:
        /* Restore int regs, including importantly r0 (two_words) */
	pop {r0, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, lr}     
        /* Stash return values */
        str  r1, [r0, #0]
        str  r2, [r0, #4]
        bx   lr

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
        mov  r1, #VG_TRC_CHAIN_ME_TO_SLOW_EP
        mov  r2, lr
        /* 4 = movw r12, lo16(disp_cp_chain_me_to_slowEP)
           4 = movt r12, hi16(disp_cp_chain_me_to_slowEP)
           4 = blx  r12 */
        sub  r2, r2, #4+4+4
        b    postamble

/* ------ Chain me to fast entry point ------ */
.global VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_F, RA) */
        mov  r1, #VG_TRC_CHAIN_ME_TO_FAST_EP
        mov  r2, lr
        /* 4 = movw r12, lo16(disp_cp_chain_me_to_fastEP)
           4 = movt r12, hi16(disp_cp_chain_me_to_fastEP)
           4 = blx  r12 */
        sub  r2, r2, #4+4+4
        b    postamble

/* ------ Indirect but boring jump ------ */
.global VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
	/* Where are we going? */
        ldr  r0, [r8, #OFFSET_arm_R15T]

        /* stats only */
        movw r1, #:lower16:vgPlain_stats__n_xindirs_32
        movt r1, #:upper16:vgPlain_stats__n_xindirs_32
        ldr  r2, [r1, #0]
        add  r2, r2, #1
        str  r2, [r1, #0]
        
        /* try a fast lookup in the translation cache */
        // r0 = next guest, r1,r2,r3,r4 scratch
        movw r1, #VG_TT_FAST_MASK       // r1 = VG_TT_FAST_MASK
        movw r4, #:lower16:VG_(tt_fast)

	and  r2, r1, r0, LSR #1         // r2 = entry #
        movt r4, #:upper16:VG_(tt_fast) // r4 = &VG_(tt_fast)

	add  r1, r4, r2, LSL #3         // r1 = &tt_fast[entry#]

        ldrd r4, r5, [r1, #0]           // r4 = .guest, r5 = .host

	cmp  r4, r0

        // jump to host if lookup succeeded
	bxeq r5

        /* otherwise the fast lookup failed */
        /* RM ME -- stats only */
        movw r1, #:lower16:vgPlain_stats__n_xindir_misses_32
        movt r1, #:upper16:vgPlain_stats__n_xindir_misses_32
        ldr  r2, [r1, #0]
        add  r2, r2, #1
        str  r2, [r1, #0]

	mov  r1, #VG_TRC_INNER_FASTMISS
        mov  r2, #0
	b    postamble

/* ------ Assisted jump ------ */
.global VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
        /* r8 contains the TRC */
        mov  r1, r8
        mov  r2, #0
        b    postamble

/* ------ Event check failed ------ */
.global VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
       	mov  r1, #VG_TRC_INNER_COUNTERZERO
        mov  r2, #0
	b    postamble


.size VG_(disp_run_translations), .-VG_(disp_run_translations)

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",%progbits

#endif // defined(VGP_arm_linux)

/*--------------------------------------------------------------------*/
/*--- end                                     dispatch-arm-linux.S ---*/
/*--------------------------------------------------------------------*/
