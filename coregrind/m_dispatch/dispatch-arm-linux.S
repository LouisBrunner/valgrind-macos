
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                         dispatch-arm-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2008-2017 Evan Geller
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
  along with this program; if not, see <http://www.gnu.org/licenses/>.

  The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics_asm.h"

#if defined(VGP_arm_linux)
	.fpu vfp

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
        movw r4, #:lower16:VG_(stats__n_xIndirs_32)
        movt r4, #:upper16:VG_(stats__n_xIndirs_32)
        ldr  r5, [r4, #0]
        add  r5, r5, #1
        str  r5, [r4, #0]

        // LIVE: r8 (guest state ptr), r0 (guest address to go to).
        // We use 6 temporaries:
        //   r6 (to point at the relevant FastCacheSet),
        //   r1, r2, r3 (scratch, for swapping entries within a set)
        //   r4, r5 (other scratch)

        /* Try a fast lookup in the translation cache.  This is pretty much
           a handcoded version of VG_(lookupInFastCache). */

        // Compute r6 = VG_TT_FAST_HASH(guest)
        lsr  r6, r0, #1                       // g1 = guest >> 1
        eor  r6, r6, r6, LSR #VG_TT_FAST_BITS // (g1 >> VG_TT_FAST_BITS) ^ g1
        ubfx r6, r6, #0, #VG_TT_FAST_BITS     // setNo
        
        // Compute r6 = &VG_(tt_fast)[r6]
        movw r4, #:lower16:VG_(tt_fast)
        movt r4, #:upper16:VG_(tt_fast)
        add  r6, r4, r6, LSL #VG_FAST_CACHE_SET_BITS // &VG_(tt_fast)[setNo]

        // LIVE: r8 (guest state ptr), r0 (guest addr), r6 (cache set)
        // try way 0
        ldr  r4, [r6, #FCS_g0]   // .guest0
        ldr  r5, [r6, #FCS_h0]   // .host0
        cmp  r4, r0   // cmp against .guest0
        bne  1f
        // hit at way 0
        // goto .host0
        bx   r5
        /*NOTREACHED*/

1:      // try way 1
        ldr  r4, [r6, #FCS_g1]
        cmp  r4, r0   // cmp against .guest1
        bne  2f
        // hit at way 1; swap upwards
        ldr  r1, [r6, #FCS_g0]   // r1 = old .guest0
        ldr  r2, [r6, #FCS_h0]   // r2 = old .host0
        ldr  r3, [r6, #FCS_h1]   // r3 = old .host1
        str  r0, [r6, #FCS_g0]   // new .guest0 = guest
        str  r3, [r6, #FCS_h0]   // new .host0 = old .host1
        str  r1, [r6, #FCS_g1]   // new .guest1 = old .guest0
        str  r2, [r6, #FCS_h1]   // new .host1 = old .host0
        // stats only
        movw r4, #:lower16:VG_(stats__n_xIndir_hits1_32)
        movt r4, #:upper16:VG_(stats__n_xIndir_hits1_32)
        ldr  r5, [r4, #0]
        add  r5, r5, #1
        str  r5, [r4, #0]
        // goto old .host1 a.k.a. new .host0
        bx   r3
        /*NOTREACHED*/

2:      // try way 2
        ldr  r4, [r6, #FCS_g2]
        cmp  r4, r0   // cmp against .guest2
        bne  3f
        // hit at way 2; swap upwards
        ldr  r1, [r6, #FCS_g1]
        ldr  r2, [r6, #FCS_h1]
        ldr  r3, [r6, #FCS_h2]
        str  r0, [r6, #FCS_g1]
        str  r3, [r6, #FCS_h1]
        str  r1, [r6, #FCS_g2]
        str  r2, [r6, #FCS_h2]
        // stats only
        movw r4, #:lower16:VG_(stats__n_xIndir_hits2_32)
        movt r4, #:upper16:VG_(stats__n_xIndir_hits2_32)
        ldr  r5, [r4, #0]
        add  r5, r5, #1
        str  r5, [r4, #0]
        // goto old .host2 a.k.a. new .host1
        bx   r3
        /*NOTREACHED*/

3:      // try way 3
        ldr  r4, [r6, #FCS_g3]
        cmp  r4, r0   // cmp against .guest3
        bne  4f
        // hit at way 3; swap upwards
        ldr  r1, [r6, #FCS_g2]
        ldr  r2, [r6, #FCS_h2]
        ldr  r3, [r6, #FCS_h3]
        str  r0, [r6, #FCS_g2]
        str  r3, [r6, #FCS_h2]
        str  r1, [r6, #FCS_g3]
        str  r2, [r6, #FCS_h3]
        // stats only
        movw r4, #:lower16:VG_(stats__n_xIndir_hits3_32)
        movt r4, #:upper16:VG_(stats__n_xIndir_hits3_32)
        ldr  r5, [r4, #0]
        add  r5, r5, #1
        str  r5, [r4, #0]
        // goto old .host3 a.k.a. new .host2
        bx   r3
        /*NOTREACHED*/

4:      // fast lookup failed
        movw r4, #:lower16:VG_(stats__n_xIndir_misses_32)
        movt r4, #:upper16:VG_(stats__n_xIndir_misses_32)
        ldr  r5, [r4, #0]
        add  r5, r5, #1
        str  r5, [r4, #0]

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

#endif // defined(VGP_arm_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                     dispatch-arm-linux.S ---*/
/*--------------------------------------------------------------------*/
