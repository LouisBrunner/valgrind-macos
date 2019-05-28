
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                       dispatch-ppc32-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2005-2017 Cerion Armour-Brown <cerion@open-works.co.uk>

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

#if defined(VGP_ppc32_linux)

#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_ppc32_CIA */


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
.globl  VG_(disp_run_translations)
.type  VG_(disp_run_translations), @function
VG_(disp_run_translations):
	/* r3 holds two_words */
	/* r4 holds guest_state */
        /* r5 holds host_addr */

        /* ----- entry point to VG_(disp_run_translations) ----- */
        /* For Linux/ppc32 we need the SysV ABI, which uses
           LR->4(parent_sp), CR->anywhere.
           (The AIX ABI, used on Darwin,
           uses LR->8(prt_sp), CR->4(prt_sp))
        */

        /* Save lr */
        mflr    6
        stw     6,4(1)

        /* New stack frame */
        stwu    1,-496(1)  /* sp should maintain 16-byte alignment */

        /* Save callee-saved registers... */
	/* r3, r4, r5 are live here, so use r6 */
        lis     6,VG_(machine_ppc32_has_FP)@ha
        lwz     6,VG_(machine_ppc32_has_FP)@l(6)
        cmplwi  6,0
        beq     LafterFP1

        /* Floating-point reg save area : 144 bytes */
        stfd    31,488(1)
        stfd    30,480(1)
        stfd    29,472(1)
        stfd    28,464(1)
        stfd    27,456(1)
        stfd    26,448(1)
        stfd    25,440(1)
        stfd    24,432(1)
        stfd    23,424(1)
        stfd    22,416(1)
        stfd    21,408(1)
        stfd    20,400(1)
        stfd    19,392(1)
        stfd    18,384(1)
        stfd    17,376(1)
        stfd    16,368(1)
        stfd    15,360(1)
        stfd    14,352(1)
LafterFP1:

        /* General reg save area : 76 bytes */
        stw     31,348(1)
        stw     30,344(1)
        stw     29,340(1)
        stw     28,336(1)
        stw     27,332(1)
        stw     26,328(1)
        stw     25,324(1)
        stw     24,320(1)
        stw     23,316(1)
        stw     22,312(1)
        stw     21,308(1)
        stw     20,304(1)
        stw     19,300(1)
        stw     18,296(1)
        stw     17,292(1)
        stw     16,288(1)
        stw     15,284(1)
        stw     14,280(1)
        stw     13,276(1)
        stw     3,272(1)  /* save two_words for later */

        /* It's necessary to save/restore VRSAVE in the AIX / Darwin ABI.
           The Linux kernel might not actually use VRSAVE for its intended
           purpose, but it should be harmless to preserve anyway. */
	/* r3, r4, r5 are live here, so use r6 */
        lis     6,VG_(machine_ppc32_has_VMX)@ha
        lwz     6,VG_(machine_ppc32_has_VMX)@l(6)
        cmplwi  6,0
        beq     LafterVMX1

#ifdef HAS_ALTIVEC
        /* VRSAVE save word : 32 bytes */
        mfspr   6,256         /* vrsave reg is spr number 256 */
        stw     6,244(1)

        /* Alignment padding : 4 bytes */

        /* Vector reg save area (quadword aligned) : 192 bytes */
        li      6,224
        stvx    31,6,1
        li      6,208
        stvx    30,6,1
        li      6,192
        stvx    29,6,1
        li      6,176
        stvx    28,6,1
        li      6,160
        stvx    27,6,1
        li      6,144
        stvx    26,6,1
        li      6,128
        stvx    25,6,1
        li      6,112
        stvx    24,6,1
        li      6,96
        stvx    23,6,1
        li      6,80
        stvx    22,6,1
        li      6,64
        stvx    21,6,1
        li      6,48
        stvx    20,6,1
#endif
        
LafterVMX1:

        /* Save cr */
        mfcr    6
        stw     6,44(1)

        /* Local variable space... */

        /* 32(sp) used later to check FPSCR[RM] */

	/* r3 holds two_words */
	/* r4 holds guest_state */
        /* r5 holds host_addr */

        /* 24(sp) used later to stop ctr reg being clobbered */
        /* 20(sp) used later to load fpscr with zero */
        /* 8:16(sp) free */
	
        /* Linkage Area (reserved)
           4(sp)  : LR
           0(sp)  : back-chain
        */

        /* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        lis     6,VG_(machine_ppc32_has_FP)@ha
        lwz     6,VG_(machine_ppc32_has_FP)@l(6)
        cmplwi  6,0
        beq     LafterFP2

        /* get zero into f3 (tedious) */
        /* note: fsub 3,3,3 is not a reliable way to do this, 
           since if f3 holds a NaN or similar then we don't necessarily
           wind up with zero. */
        li      6,0
        stw     6,20(1)
        lfs     3,20(1)
        mtfsf   0xFF,3   /* fpscr = f3 */
LafterFP2:

        /* set host AltiVec control word to the default mode expected 
           by VEX-generated code. */
        lis     6,VG_(machine_ppc32_has_VMX)@ha
        lwz     6,VG_(machine_ppc32_has_VMX)@l(6)
        cmplwi  6,0
        beq     LafterVMX2

#ifdef HAS_ALTIVEC
        vspltisw 3,0x0  /* generate zero */
        mtvscr  3       /* sets VSCR[NJ]=0 */
#endif
        
LafterVMX2:

        /* make a stack frame for the code we are calling */
        stwu    1,-16(1)

        /* Set up the guest state ptr */
        mr      31,4      /* r31 (generated code gsp) = r4 */

        /* and jump into the code cache.  Chained translations in
           the code cache run, until for whatever reason, they can't
           continue.  When that happens, the translation in question
           will jump (or call) to one of the continuation points
           VG_(cp_...) below. */
        mtctr   5
        bctr
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- Postamble and exit.                          ---*/
/*----------------------------------------------------*/

postamble:
        /* At this point, r6 and r7 contain two
           words to be returned to the caller.  r6
           holds a TRC value, and r7 optionally may
           hold another word (for CHAIN_ME exits, the
           address of the place to patch.) */

        /* We're leaving.  Check that nobody messed with
           VSCR or FPSCR in ways we don't expect. */
        /* Using r10 - value used again further on, so don't trash! */
        lis     10,VG_(machine_ppc32_has_FP)@ha
        lwz     10,VG_(machine_ppc32_has_FP)@l(10)

	/* Using r11 - value used again further on, so don't trash! */
        lis     11,VG_(machine_ppc32_has_VMX)@ha
        lwz     11,VG_(machine_ppc32_has_VMX)@l(11)

        cmplwi  10,0    /* Do we have FP ? */
        beq     LafterFP8

	/* Set fpscr back to a known state, since vex-generated code
	   may have messed with fpscr[rm]. */
        li      5,0
        addi    1,1,-16
        stw     5,0(1)
        lfs     3,0(1)
        addi    1,1,16
        mtfsf   0xFF,3   /* fpscr = f3 */
LafterFP8:

        cmplwi  11,0    /* Do we have altivec? */
        beq     LafterVMX8

#ifdef HAS_ALTIVEC
        /* Expect VSCR[NJ] to be 0, call invariant_violation if
           VSCR[NJ] == 1 . */
        /* first generate 4x 0x00010000 */
        vspltisw  4,0x1                   /* 4x 0x00000001 */
        vspltisw  5,0x0                   /* zero */
        vsldoi    6,4,5,0x2               /* <<2*8 => 4x 0x00010000 */
        /* retrieve VSCR and mask wanted bits */
        mfvscr    7
        vand      7,7,6                   /* gives NJ flag */
        vspltw    7,7,0x3                 /* flags-word to all lanes */
        vcmpequw. 8,6,7                   /* CR[24] = 1 if v6 == v7 */
        bt        24,invariant_violation  /* branch if all_equal */
#endif

LafterVMX8:
	/* otherwise we're OK */
        b       remove_frame

invariant_violation:
        li      6,VG_TRC_INVARIANT_FAILED
        li      7,0
        /* fall through */

remove_frame:
        /* Restore FP regs */
        /* r10 already holds VG_(machine_ppc32_has_FP) value */
        cmplwi  10,0
        beq     LafterFP9

        /* Floating-point regs */
        lfd     31,488(1)
        lfd     30,480(1)
        lfd     29,472(1)
        lfd     28,464(1)
        lfd     27,456(1)
        lfd     26,448(1)
        lfd     25,440(1)
        lfd     24,432(1)
        lfd     23,424(1)
        lfd     22,416(1)
        lfd     21,408(1)
        lfd     20,400(1)
        lfd     19,392(1)
        lfd     18,384(1)
        lfd     17,376(1)
        lfd     16,368(1)
        lfd     15,360(1)
        lfd     14,352(1)
LafterFP9:

        /* r11 already holds VG_(machine_ppc32_has_VMX) value */
        cmplwi  11,0
        beq     LafterVMX9

        /* Restore Altivec regs */
#ifdef HAS_ALTIVEC
        /* VRSAVE */
        lwz     4,244(1)
        mfspr   4,256         /* VRSAVE reg is spr number 256 */

        /* Vector regs */
        li      4,224
        lvx     31,4,1
        li      4,208
        lvx     30,4,1
        li      4,192
        lvx     29,4,1
        li      4,176
        lvx     28,4,1
        li      4,160
        lvx     27,4,1
        li      4,144
        lvx     26,4,1
        li      4,128
        lvx     25,4,1
        li      4,112
        lvx     24,4,1
        li      4,96
        lvx     23,4,1
        li      4,80
        lvx     22,4,1
        li      4,64
        lvx     21,4,1
        li      4,48
        lvx     20,4,1
#endif
LafterVMX9:

        /* restore int regs, including importantly r3 (two_words) */
        addi    1,1,16
        lwz     31,348(1)
        lwz     30,344(1)
        lwz     29,340(1)
        lwz     28,336(1)
        lwz     27,332(1)
        lwz     26,328(1)
        lwz     25,324(1)
        lwz     24,320(1)
        lwz     23,316(1)
        lwz     22,312(1)
        lwz     21,308(1)
        lwz     20,304(1)
        lwz     19,300(1)
        lwz     18,296(1)
        lwz     17,292(1)
        lwz     16,288(1)
        lwz     15,284(1)
        lwz     14,280(1)
        lwz     13,276(1)
        lwz     3,272(1)
        /* Stash return values */
        stw     6,0(3)
        stw     7,4(3)

        /* restore lr & sp, and leave */
        lwz     0,500(1)  /* stack_size + 4 */
        mtlr    0
        addi    1,1,496   /* stack_size */
        blr


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
        li   6, VG_TRC_CHAIN_ME_TO_SLOW_EP
        mflr 7
        /* 8 = imm32-fixed2 r30, disp_cp_chain_me_to_slowEP
           4 = mtctr r30
           4 = btctr
        */
        subi 7,7,8+4+4
        b    postamble

/* ------ Chain me to fast entry point ------ */
.global VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_S, RA) */
        li   6, VG_TRC_CHAIN_ME_TO_FAST_EP
        mflr 7
        /* 8 = imm32-fixed2 r30, disp_cp_chain_me_to_fastEP
           4 = mtctr r30
           4 = btctr
        */
        subi 7,7,8+4+4
        b    postamble

/* ------ Indirect but boring jump ------ */
.global VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
        /* Where are we going? */
        lwz   20, OFFSET_ppc32_CIA(31)

        /* stats only */
        lis   24,     VG_(stats__n_xIndirs_32)@ha
        addi  24, 24, VG_(stats__n_xIndirs_32)@l
        lwz   25, 0(24)
        addi  25, 25, 1
        stw   25, 0(24)

        // LIVE: r31 (guest state ptr), r20 (guest address to go to).
        // We use 6 temporaries:
        //   r26 (to point at the relevant FastCacheSet),
        //   r21, r22, r23 (scratch, for swapping entries within a set)
        //   r24, r25 (other scratch)

        /* Try a fast lookup in the translation cache.  This is pretty much
           a handcoded version of VG_(lookupInFastCache). */

        // Compute r26 = VG_TT_FAST_HASH(guest)
        srwi  26, 20, 2                       // g2 = guest >> 2
        srwi  25, 20, (VG_TT_FAST_BITS + 2)   // (g2 >> VG_TT_FAST_BITS)
        xor   26, 26, 25                      // (g2 >> VG_TT_FAST_BITS) ^ g2
        andi. 26, 26, VG_TT_FAST_MASK         // setNo
        
        // Compute r6 = &VG_(tt_fast)[r6]
        lis   25, VG_(tt_fast)@ha
        addi  25, 25, VG_(tt_fast)@l
        slwi  26, 26, VG_FAST_CACHE_SET_BITS
        add   26, 26, 25

        // LIVE: r31 (guest state ptr), r20 (guest addr), r26 (cache set)
        // try way 0
        lwz   24, FCS_g0(26)   // .guest0
        lwz   25, FCS_h0(26)   // .host0
        cmpw  24, 20  // cmp against .guest0
        bne   1f
        // hit at way 0
        // goto .host0
        mtctr 25
        bctr
        /*NOTREACHED*/

1:      // try way 1
        lwz   24, FCS_g1(26)
        cmpw  24, 20  // cmp against .guest1
        bne   2f
        // hit at way 1; swap upwards
        lwz   21, FCS_g0(26)   // 21 = old .guest0
        lwz   22, FCS_h0(26)   // 22 = old .host0
        lwz   23, FCS_h1(26)   // 23 = old .host1
        stw   20, FCS_g0(26)   // new .guest0 = guest
        stw   23, FCS_h0(26)   // new .host0 = old .host1
        stw   21, FCS_g1(26)   // new .guest1 = old .guest0
        stw   22, FCS_h1(26)   // new .host1 = old .host0
        // stats only
        lis   24,     VG_(stats__n_xIndir_hits1_32)@ha
        addi  24, 24, VG_(stats__n_xIndir_hits1_32)@l
        lwz   25, 0(24)
        addi  25, 25, 1
        stw   25, 0(24)
        // goto old .host1 a.k.a. new .host0
        mtctr 23
        bctr
        /*NOTREACHED*/

2:      // try way 2
        lwz   24, FCS_g2(26)
        cmpw  24, 20   // cmp against .guest2
        bne   3f
        // hit at way 2; swap upwards
        lwz   21, FCS_g1(26)
        lwz   22, FCS_h1(26)
        lwz   23, FCS_h2(26)
        stw   20, FCS_g1(26)
        stw   23, FCS_h1(26)
        stw   21, FCS_g2(26)
        stw   22, FCS_h2(26)
        // stats only
        lis   24,     VG_(stats__n_xIndir_hits2_32)@ha
        addi  24, 24, VG_(stats__n_xIndir_hits2_32)@l
        lwz   25, 0(24)
        addi  25, 25, 1
        stw   25, 0(24)
        // goto old .host2 a.k.a. new .host1
        mtctr 23
        bctr
        /*NOTREACHED*/

3:      // try way 3
        lwz   24, FCS_g3(26)
        cmpw  24, 20   // cmp against .guest3
        bne   4f
        // hit at way 3; swap upwards
        lwz   21, FCS_g2(26)
        lwz   22, FCS_h2(26)
        lwz   23, FCS_h3(26)
        stw   20, FCS_g2(26)
        stw   23, FCS_h2(26)
        stw   21, FCS_g3(26)
        stw   22, FCS_h3(26)
        // stats only
        lis   24,     VG_(stats__n_xIndir_hits3_32)@ha
        addi  24, 24, VG_(stats__n_xIndir_hits3_32)@l
        lwz   25, 0(24)
        addi  25, 25, 1
        stw   25, 0(24)
        // goto old .host3 a.k.a. new .host2
        mtctr 23
        bctr
        /*NOTREACHED*/

4:      // fast lookup failed:
        /* stats only */
        lis   24,     VG_(stats__n_xIndir_misses_32)@ha
        addi  24, 24, VG_(stats__n_xIndir_misses_32)@l
        lwz   25, 0(24)
        addi  25, 25, 1
        stw   25, 0(24)

        li    6, VG_TRC_INNER_FASTMISS
        li    7, 0
        b     postamble
	/*NOTREACHED*/

/* ------ Assisted jump ------ */
.global VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
        /* r31 contains the TRC */
        mr      6,31
        li      7,0
        b       postamble

/* ------ Event check failed ------ */
.global VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
        li      6,VG_TRC_INNER_COUNTERZERO
        li      7,0
        b       postamble

        
.size VG_(disp_run_translations), .-VG_(disp_run_translations)

#endif // defined(VGP_ppc32_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
