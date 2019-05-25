
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                       dispatch-s390x-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright IBM Corp. 2010-2017
  Copyright (C) 2011-2017, Florian Krohm (britzel@acm.org)
        
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

/* Contributed by Florian Krohm and Christian Borntraeger */

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"
#include "libvex_s390x_common.h"

#if defined(VGA_s390x)

/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(disp_run_translations) is    ---*/
/*--- used to run all translations,                        ---*/
/*--- including no-redir ones.                             ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Convenience definitions for readability */
#undef  SP
#define SP S390_REGNO_STACK_POINTER

#undef  LR
#define LR S390_REGNO_LINK_REGISTER

/* Location of valgrind's saved FPC register */
#define S390_LOC_SAVED_FPC_V S390_OFFSET_SAVED_FPC_V(SP)

/* Location of saved R2 register */
#define S390_LOC_SAVED_R2 S390_OFFSET_SAVED_R2(SP)


/*----------------------------------------------------*/
/*--- Entry and preamble (set everything up)       ---*/
/*----------------------------------------------------*/

/* signature:
void VG_(disp_run_translations)( UWord* two_words,
                                 void*  guest_state, 
                                 Addr   host_addr );

        Return results are placed in two_words:
        
        two_words[0] is set to the TRC
        two_words[1] is set to the address to patch (in case two_words[0] is
                     VG_TRC_CHAIN_ME_TO_{SLOW,FAST}_EP). Otherwise, it is 0.
*/
        .text
        .align   4
        .globl   VG_(disp_run_translations)
        .type    VG_(disp_run_translations), @function
VG_(disp_run_translations):

        /* r2 holds two_words */
        /* r3 holds pointer to guest_state */
        /* r4 holds host_addr, i.e. the address of the translation to run */

        /* Save gprs   ABI: r6...r13 and r15 */
        stmg  %r6,%r15,48(SP)

        /* New stack frame */
        aghi  SP,-S390_INNERLOOP_FRAME_SIZE

        /* Save fprs:   ABI: f8...f15 */
        std   %f8,160+0(SP)
        std   %f9,160+8(SP)
        std   %f10,160+16(SP)
        std   %f11,160+24(SP)
        std   %f12,160+32(SP)
        std   %f13,160+40(SP)
        std   %f14,160+48(SP)
        std   %f15,160+56(SP)

        /* Load address of guest state into guest state register (r13) */
        lgr   %r13,%r3

        /* Save R2 on stack. In postamble it will be restored such that the
           return values can be written */
        stg   %r2,S390_LOC_SAVED_R2
        
        /* Save valgrind's FPC on stack so postamble can restore
           it later . */
        stfpc S390_LOC_SAVED_FPC_V

        /* Load the FPC the way the client code wants it. I.e. pull the
           value from the guest state. */
        lfpc  OFFSET_s390x_fpc(%r13)

        /* Jump into the code cache.  Chained translations in
           the code cache run, until for whatever reason, they can't
           continue.  When that happens, the translation in question
           will jump (or call) to one of the continuation points
           VG_(cp_...) below. */
        br    %r4


/*----------------------------------------------------*/
/*--- Postamble and return to C code.              ---*/
/*----------------------------------------------------*/

postamble:
        /* At this point, %r0 and %r1 contain two
           words to be returned to the caller.  %r0
           holds a TRC value, and %r1 optionally may
           hold another word (for CHAIN_ME exits, the
           address of the place to patch.) */

        /* We're leaving. AMD has some code here to check invariants.
           We don't have (need) that, as we save and restore the FPC register
           whenever we switch between valgrind proper to client code. */

	/* Restore valgrind's FPC, as client code may have changed it. */
        lfpc S390_LOC_SAVED_FPC_V

        /* Restore %r2 from stack; holds address of two_words */
        lg   %r2,S390_LOC_SAVED_R2

        stg  %r0,0(%r2)         /* Store %r0 to two_words[0] */
        stg  %r1,8(%r2)         /* Store %r1 to two_words[1] */
        
        /* Restore callee-saved registers... */

        /* Floating-point regs */
        ld   %f8,160+0(SP)
        ld   %f9,160+8(SP)
        ld   %f10,160+16(SP)
        ld   %f11,160+24(SP)
        ld   %f12,160+32(SP)
        ld   %f13,160+40(SP)
        ld   %f14,160+48(SP)
        ld   %f15,160+56(SP)

        /* Remove stack frame */
        aghi SP,S390_INNERLOOP_FRAME_SIZE

        /* General-purpose regs. This also restores the original link
           register (r14) and stack pointer (r15). */
        lmg  %r6,%r15,48(SP)

        /* Return */
        br   LR
        
        
/*----------------------------------------------------*/
/*--- Continuation points                          ---*/
/*----------------------------------------------------*/

/* ------ Chain me to slow entry point ------ */
        .global VG_(disp_cp_chain_me_to_slowEP)
VG_(disp_cp_chain_me_to_slowEP):
        /* When we come here %r1 contains the address of the place to patch.
           The return values (TRC, address-to-patch) are stored here in
           %r0 and %r1, respectively */
        lghi    %r0,VG_TRC_CHAIN_ME_TO_SLOW_EP
        j       postamble


/* ------ Chain me to fast entry point ------ */
        .global VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
        /* Identical to VG_(disp_cp_chain_me_to_slowEP), except value of %r0. */
        lghi    %r0,VG_TRC_CHAIN_ME_TO_FAST_EP
        j       postamble


/* ------ Indirect but boring jump ------ */
        .global VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
        /* Where are we going? */
        lg      %r6, OFFSET_s390x_IA(%r13)   // "guest"

        /* stats only */
        larl    %r11, VG_(stats__n_xIndirs_32)
        l       %r12, 0(%r11)
        ahi     %r12, 1
        st      %r12, 0(%r11)

        // LIVE: r13 (guest state ptr), r6 (guest address to go to).
        // We use 6 temporaries:
        //   r7 (to point at the relevant FastCacheSet),
        //   r8, r9, r10 (scratch, for swapping entries within a set)
        //   r11, r12 (other scratch)

        /* Try a fast lookup in the translation cache.  This is pretty much
           a handcoded version of VG_(lookupInFastCache). */

        // Compute %r7 = VG_TT_FAST_HASH(guest)
        srlg    %r7, %r6, 1                     // g1 = guest >> 1
        srlg    %r8, %r6, (VG_TT_FAST_BITS + 1) // (g1 >> VG_TT_FAST_BITS)
        xgr     %r7, %r8                        // (g1 >> VG_TT_FAST_BITS) ^ g1
        llill   %r8, VG_TT_FAST_MASK & 0xffff
#       if ((VG_TT_FAST_MASK & 0xffff0000) >> 16 != 0)
        iilh    %r8, (VG_TT_FAST_MASK & 0xffff0000) >> 16
#       endif
        ngr     %r7, %r8                        // setNo

        // Compute %r7 = &VG_(tt_fast)[%r7]
        sllg    %r7,%r7, VG_FAST_CACHE_SET_BITS // setNo * sizeof(FastCacheSet)
        larl    %r8, VG_(tt_fast)             // &VG_(tt_fast)[0]
        agr     %r7, %r8                      // &VG_(tt_fast)[setNo]

        // LIVE: %r13 (guest state ptr), %r6 (guest addr), %r7 (cache set)
        // try way 0
        cg      %r6, FCS_g0(%r7)   // cmp against .guest0
        lg      %r8, FCS_h0(%r7)
        jne     1f
        // hit at way 0
        // goto .host0
        br      %r8
        /*NOTREACHED*/
        .long   0

1:      // try way 1
        cg      %r6, FCS_g1(%r7)   // cmp against .guest1
        jne     2f
        // hit at way 1; swap upwards
        lg      %r8,  FCS_g0(%r7)  // r8  = old .guest0
        lg      %r9,  FCS_h0(%r7)  // r9  = old .host0
        lg      %r10, FCS_h1(%r7)  // r10 = old .host1
        stg     %r6,  FCS_g0(%r7)  // new .guest0 = guest
        stg     %r10, FCS_h0(%r7)  // new .host0 = old .host1
        stg     %r8,  FCS_g1(%r7)  // new .guest1 = old .guest0
        stg     %r9,  FCS_h1(%r7)  // new .host1 = old .host0
        // stats only
        larl    %r11, VG_(stats__n_xIndir_hits1_32)
        l       %r12, 0(%r11)
        ahi     %r12, 1
        st      %r12, 0(%r11)
        // goto old .host1 a.k.a. new .host0
        br      %r10
        /*NOTREACHED*/
        .long 0

2:      // try way 2
        cg      %r6, FCS_g2(%r7)   // cmp against .guest2
        jne     3f
        lg      %r8,  FCS_g1(%r7)
        lg      %r9,  FCS_h1(%r7)
        lg      %r10, FCS_h2(%r7)
        stg     %r6,  FCS_g1(%r7)
        stg     %r10, FCS_h1(%r7)
        stg     %r8,  FCS_g2(%r7)
        stg     %r9,  FCS_h2(%r7)
        // stats only
        larl    %r11, VG_(stats__n_xIndir_hits2_32)
        l       %r12, 0(%r11)
        ahi     %r12, 1
        st      %r12, 0(%r11)
        // goto old .host2 a.k.a. new .host1
        br      %r10
        /*NOTREACHED*/
        .long 0

3:      // try way 3
        cg      %r6, FCS_g3(%r7)   // cmp against .guest3
        jne     4f
        // hit at way 3; swap upwards
        lg      %r8,  FCS_g2(%r7)
        lg      %r9,  FCS_h2(%r7)
        lg      %r10, FCS_h3(%r7)
        stg     %r6,  FCS_g2(%r7)
        stg     %r10, FCS_h2(%r7)
        stg     %r8,  FCS_g3(%r7)
        stg     %r9,  FCS_h3(%r7)
        // stats only
        larl    %r11, VG_(stats__n_xIndir_hits3_32)
        l       %r12, 0(%r11)
        ahi     %r12, 1
        st      %r12, 0(%r11)
        // goto old .host3 a.k.a. new .host2
        br      %r10
        .long 0

4:      // fast lookup failed
        larl    %r11, VG_(stats__n_xIndir_misses_32)
        l       %r12, 0(%r11)
        ahi     %r12, 1
        st      %r12, 0(%r11)

        lghi    %r0, VG_TRC_INNER_FASTMISS
        lghi    %r1, 0
        j       postamble
        /*NOTREACHED*/

        
/* ------ Assisted jump ------ */
        .global VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
        /* guest-state-pointer contains the TRC. Put the value into the
           return register */
        lgr     %r0,%r13
        lghi    %r1,0
        j       postamble


/* ------ Event check failed ------ */
        .global VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
        lghi    %r0,VG_TRC_INNER_COUNTERZERO
        lghi    %r1,0
        j       postamble


        .size VG_(disp_run_translations), .-VG_(disp_run_translations)

#endif /* VGA_s390x */

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                   dispatch-s390x-linux.S ---*/
/*--------------------------------------------------------------------*/
