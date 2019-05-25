
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                       dispatch-arm64-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2013-2017 OpenWorks
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
  along with this program; if not, see <http://www.gnu.org/licenses/>.

  The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics_asm.h"

#if defined(VGP_arm64_linux)

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
	// Where are we going?
        ldr  x0, [x21, #OFFSET_arm64_PC]

        // stats only
        adrp x4,           VG_(stats__n_xIndirs_32)
        add  x4, x4, :lo12:VG_(stats__n_xIndirs_32)
        ldr  w5, [x4, #0]
        add  w5, w5, #1
        str  w5, [x4, #0]

        // LIVE: x21 (guest state ptr), x0 (guest address to go to).
        // We use 6 temporaries:
        //   x6 (to point at the relevant FastCacheSet),
        //   x1, x2, x3 (scratch, for swapping entries within a set)
        //   x4, x5 (other scratch)

        /* Try a fast lookup in the translation cache.  This is pretty much
           a handcoded version of VG_(lookupInFastCache). */

        // Compute x6 = VG_TT_FAST_HASH(guest)
        lsr  x6, x0, #2                       // g2 = guest >> 2
        eor  x6, x6, x6, LSR #VG_TT_FAST_BITS // (g2 >> VG_TT_FAST_BITS) ^ g2
        mov  x4, #VG_TT_FAST_MASK             // VG_TT_FAST_MASK
        and  x6, x6, x4                       // setNo

        // Compute x6 = &VG_(tt_fast)[x6]
        adrp x4,           VG_(tt_fast)
        add  x4, x4, :lo12:VG_(tt_fast)              // &VG_(tt_fast)[0]
        add  x6, x4, x6, LSL #VG_FAST_CACHE_SET_BITS // &VG_(tt_fast)[setNo]

        // LIVE: x21 (guest state ptr), x0 (guest addr), x6 (cache set)
        // try way 0
        ldp  x4, x5, [x6, #FCS_g0]  // x4 = .guest0, x5 = .host0
        cmp  x4, x0   // cmp against .guest0
        bne  1f
        // hit at way 0
        // goto .host0
        br   x5
        /*NOTREACHED*/

1:      // try way 1
        ldr  x4, [x6, #FCS_g1]
        cmp  x4, x0   // cmp against .guest1
        bne  2f
        // hit at way 1; swap upwards
        ldr  x1, [x6, #FCS_g0]   // x1 = old .guest0
        ldr  x2, [x6, #FCS_h0]   // x2 = old .host0
        ldr  x3, [x6, #FCS_h1]   // x3 = old .host1
        str  x0, [x6, #FCS_g0]   // new .guest0 = guest
        str  x3, [x6, #FCS_h0]   // new .host0 = old .host1
        str  x1, [x6, #FCS_g1]   // new .guest1 = old .guest0
        str  x2, [x6, #FCS_h1]   // new .host1 = old .host0
        // stats only
        adrp x4,           VG_(stats__n_xIndir_hits1_32)
        add  x4, x4, :lo12:VG_(stats__n_xIndir_hits1_32)
        ldr  w5, [x4, #0]
        add  w5, w5, #1
        str  w5, [x4, #0]
        // goto old .host1 a.k.a. new .host0
        br   x3
        /*NOTREACHED*/

2:      // try way 2
        ldr  x4, [x6, #FCS_g2]
        cmp  x4, x0   // cmp against .guest2
        bne  3f
        // hit at way 2; swap upwards
        ldr  x1, [x6, #FCS_g1]
        ldr  x2, [x6, #FCS_h1]
        ldr  x3, [x6, #FCS_h2]
        str  x0, [x6, #FCS_g1]
        str  x3, [x6, #FCS_h1]
        str  x1, [x6, #FCS_g2]
        str  x2, [x6, #FCS_h2]
        // stats only
        adrp x4,           VG_(stats__n_xIndir_hits2_32)
        add  x4, x4, :lo12:VG_(stats__n_xIndir_hits2_32)
        ldr  w5, [x4, #0]
        add  w5, w5, #1
        str  w5, [x4, #0]
        // goto old .host2 a.k.a. new .host1
        br   x3
        /*NOTREACHED*/

3:      // try way 3
        ldr  x4, [x6, #FCS_g3]
        cmp  x4, x0   // cmp against .guest3
        bne  4f
        // hit at way 3; swap upwards
        ldr  x1, [x6, #FCS_g2]
        ldr  x2, [x6, #FCS_h2]
        ldr  x3, [x6, #FCS_h3]
        str  x0, [x6, #FCS_g2]
        str  x3, [x6, #FCS_h2]
        str  x1, [x6, #FCS_g3]
        str  x2, [x6, #FCS_h3]
        // stats only
        adrp x4,           VG_(stats__n_xIndir_hits3_32)
        add  x4, x4, :lo12:VG_(stats__n_xIndir_hits3_32)
        ldr  w5, [x4, #0]
        add  w5, w5, #1
        str  w5, [x4, #0]
        // goto old .host3 a.k.a. new .host2
        br   x3
        /*NOTREACHED*/

4:      // fast lookup failed
        adrp x4,           VG_(stats__n_xIndir_misses_32)
        add  x4, x4, :lo12:VG_(stats__n_xIndir_misses_32)
        ldr  w5, [x4, #0]
        add  w5, w5, #1
        str  w5, [x4, #0]

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

#endif // defined(VGP_arm64_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                   dispatch-arm64-linux.S ---*/
/*--------------------------------------------------------------------*/
