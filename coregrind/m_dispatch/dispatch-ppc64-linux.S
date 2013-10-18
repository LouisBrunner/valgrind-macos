
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                       dispatch-ppc64-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2005-2013 Cerion Armour-Brown <cerion@open-works.co.uk>

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

#if defined(VGP_ppc64_linux)

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_ppc64_CIA */


/* References to globals via the TOC */

/*
        .globl  vgPlain_tt_fast
        .lcomm  vgPlain_tt_fast,4,4
        .type   vgPlain_tt_fast, @object
*/
.section ".toc","aw"
.tocent__vgPlain_tt_fast:
        .tc vgPlain_tt_fast[TC],vgPlain_tt_fast
.tocent__vgPlain_stats__n_xindirs_32:
        .tc vgPlain_stats__n_xindirs_32[TC],vgPlain_stats__n_xindirs_32
.tocent__vgPlain_stats__n_xindir_misses_32:
        .tc vgPlain_stats__n_xindir_misses_32[TC],vgPlain_stats__n_xindir_misses_32
.tocent__vgPlain_machine_ppc64_has_VMX:
        .tc vgPlain_machine_ppc64_has_VMX[TC],vgPlain_machine_ppc64_has_VMX

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

.section ".text"
.align   2
.globl   VG_(disp_run_translations)
.section ".opd","aw"
.align   3
VG_(disp_run_translations):
.quad    .VG_(disp_run_translations),.TOC.@tocbase,0
.previous
.type    .VG_(disp_run_translations),@function
.globl   .VG_(disp_run_translations)
.VG_(disp_run_translations):
	/* r3 holds two_words */
	/* r4 holds guest_state */
        /* r5 holds host_addr */

        /* ----- entry point to VG_(disp_run_translations) ----- */
        /* PPC64 ABI saves LR->16(prt_sp), CR->8(prt_sp)) */

        /* Save lr, cr */
        mflr    6
        std     6,16(1)
        mfcr    6
        std     6,8(1)

        /* New stack frame */
        stdu    1,-624(1)  /* sp should maintain 16-byte alignment */

        /* General reg save area : 152 bytes */
        std     31,472(1)
        std     30,464(1)
        std     29,456(1)
        std     28,448(1)
        std     27,440(1)
        std     26,432(1)
        std     25,424(1)
        std     24,416(1)
        std     23,408(1)
        std     22,400(1)
        std     21,392(1)
        std     20,384(1)
        std     19,376(1)
        std     18,368(1)
        std     17,360(1)
        std     16,352(1)
        std     15,344(1)
        std     14,336(1)
        std     13,328(1)
        std     3,104(1)  /* save two_words for later */

        /* Save callee-saved registers... */
        /* Floating-point reg save area : 144 bytes */
        stfd    31,616(1)
        stfd    30,608(1)
        stfd    29,600(1)
        stfd    28,592(1)
        stfd    27,584(1)
        stfd    26,576(1)
        stfd    25,568(1)
        stfd    24,560(1)
        stfd    23,552(1)
        stfd    22,544(1)
        stfd    21,536(1)
        stfd    20,528(1)
        stfd    19,520(1)
        stfd    18,512(1)
        stfd    17,504(1)
        stfd    16,496(1)
        stfd    15,488(1)
        stfd    14,480(1)

        /* It's necessary to save/restore VRSAVE in the AIX / Darwin ABI.
           The Linux kernel might not actually use VRSAVE for its intended
           purpose, but it should be harmless to preserve anyway. */
	/* r3, r4, r5 are live here, so use r6 */
	ld	6,.tocent__vgPlain_machine_ppc64_has_VMX@toc(2)
	ld	6,0(6)
        cmpldi  6,0
        beq     .LafterVMX1

        /* VRSAVE save word : 32 bytes */
        mfspr   6,256         /* vrsave reg is spr number 256 */
        stw     6,324(1)

        /* Alignment padding : 4 bytes */

        /* Vector reg save area (quadword aligned) : 192 bytes */
        li      6,304
        stvx    31,6,1
        li      6,288
        stvx    30,6,1
        li      6,272
        stvx    29,6,1
        li      6,256
        stvx    28,6,1
        li      6,240
        stvx    27,6,1
        li      6,224
        stvx    26,6,1
        li      6,208
        stvx    25,6,1
        li      6,192
        stvx    24,6,1
        li      6,176
        stvx    23,6,1
        li      6,160
        stvx    22,6,1
        li      6,144
        stvx    21,6,1
        li      6,128
        stvx    20,6,1
.LafterVMX1:

        /* Local variable space... */

	/* r3 holds two_words */
	/* r4 holds guest_state */
        /* r5 holds host_addr */

        /* 96(sp) used later to check FPSCR[RM] */
        /* 88(sp) used later to load fpscr with zero */
	/* 48:87(sp) free */
	
        /* Linkage Area (reserved)
           40(sp) : TOC
           32(sp) : link editor doubleword
           24(sp) : compiler doubleword
           16(sp) : LR
           8(sp)  : CR
           0(sp)  : back-chain
        */

        /* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        /* => get zero into f3 (tedious)
           fsub 3,3,3 is not a reliable way to do this, since if
           f3 holds a NaN or similar then we don't necessarily
           wind up with zero. */
        li      6,0
        stw     6,88(1)
        lfs     3,88(1)
        mtfsf   0xFF,3   /* fpscr = lo32 of f3 */

        /* set host AltiVec control word to the default mode expected 
           by VEX-generated code. */
	ld	6,.tocent__vgPlain_machine_ppc64_has_VMX@toc(2)
	ld	6,0(6)
        cmpldi  6,0
        beq     .LafterVMX2

        vspltisw 3,0x0  /* generate zero */
        mtvscr  3
.LafterVMX2:

        /* make a stack frame for the code we are calling */
        stdu    1,-48(1)

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

.postamble:
        /* At this point, r6 and r7 contain two
           words to be returned to the caller.  r6
           holds a TRC value, and r7 optionally may
           hold another word (for CHAIN_ME exits, the
           address of the place to patch.) */

        /* undo the "make a stack frame for the code we are calling" */
        addi    1,1,48

        /* We're leaving.  Check that nobody messed with
           VSCR or FPSCR in ways we don't expect. */
	/* Using r11 - value used again further on, so don't trash! */
	ld	11,.tocent__vgPlain_machine_ppc64_has_VMX@toc(2)
	ld	11,0(11)

	/* Set fpscr back to a known state, since vex-generated code
	   may have messed with fpscr[rm]. */
        li      5,0
        addi    1,1,-16
        stw     5,0(1)
        lfs     3,0(1)
        addi    1,1,16
        mtfsf   0xFF,3   /* fpscr = f3 */
	
        cmpldi  11,0    /* Do we have altivec? */
        beq     .LafterVMX8

        /* Check VSCR[NJ] == 1 */
        /* first generate 4x 0x00010000 */
        vspltisw  4,0x1                   /* 4x 0x00000001 */
        vspltisw  5,0x0                   /* zero */
        vsldoi    6,4,5,0x2               /* <<2*8 => 4x 0x00010000 */
        /* retrieve VSCR and mask wanted bits */
        mfvscr    7
        vand      7,7,6                   /* gives NJ flag */
        vspltw    7,7,0x3                 /* flags-word to all lanes */
        vcmpequw. 8,6,7                   /* CR[24] = 1 if v6 == v7 */
        bt        24,.invariant_violation /* branch if all_equal */

.LafterVMX8:
	/* otherwise we're OK */
        b       .remove_frame

.invariant_violation:
        li      6,VG_TRC_INVARIANT_FAILED
        li      7,0
        /* fall through */

.remove_frame:
        /* r11 already holds VG_(machine_ppc32_has_VMX) value */
        cmplwi  11,0
        beq     .LafterVMX9

        /* Restore Altivec regs.
           Use r5 as scratch since r6/r7 are live. */
        /* VRSAVE */
        lwz     5,324(1)
        mfspr   5,256         /* VRSAVE reg is spr number 256 */

        /* Vector regs */
        li      5,304
        lvx     31,5,1
        li      5,288
        lvx     30,5,1
        li      5,272
        lvx     29,5,1
        li      5,256
        lvx     28,5,1
        li      5,240
        lvx     27,5,1
        li      5,224
        lvx     26,5,1
        li      5,208
        lvx     25,5,1
        li      5,192
        lvx     24,5,1
        li      5,176
        lvx     23,5,1
        li      5,160
        lvx     22,5,1
        li      5,144
        lvx     21,5,1
        li      5,128
        lvx     20,5,1
.LafterVMX9:

        /* Restore FP regs */
        /* Floating-point regs */
        lfd     31,616(1)
        lfd     30,608(1)
        lfd     29,600(1)
        lfd     28,592(1)
        lfd     27,584(1)
        lfd     26,576(1)
        lfd     25,568(1)
        lfd     24,560(1)
        lfd     23,552(1)
        lfd     22,544(1)
        lfd     21,536(1)
        lfd     20,528(1)
        lfd     19,520(1)
        lfd     18,512(1)
        lfd     17,504(1)
        lfd     16,496(1)
        lfd     15,488(1)
        lfd     14,480(1)

        /* restore int regs, including importantly r3 (two_words) */
        ld      31,472(1)
        ld      30,464(1)
        ld      29,456(1)
        ld      28,448(1)
        ld      27,440(1)
        ld      26,432(1)
        ld      25,424(1)
        ld      24,416(1)
        ld      23,408(1)
        ld      22,400(1)
        ld      21,392(1)
        ld      20,384(1)
        ld      19,376(1)
        ld      18,368(1)
        ld      17,360(1)
        ld      16,352(1)
        ld      15,344(1)
        ld      14,336(1)
        ld      13,328(1)
        ld      3,104(1)
        /* Stash return values */
        std     6,0(3)
        std     7,8(3)

        /* restore lr & sp, and leave */
        ld      0,632(1)  /* stack_size + 8 */
        mtcr    0
        ld      0,640(1)  /* stack_size + 16 */
        mtlr    0
        addi    1,1,624   /* stack_size */
        blr


/*----------------------------------------------------*/
/*--- Continuation points                          ---*/
/*----------------------------------------------------*/

/* ------ Chain me to slow entry point ------ */
        .section ".text"
        .align   2
        .globl   VG_(disp_cp_chain_me_to_slowEP)
        .section ".opd","aw"
        .align   3
VG_(disp_cp_chain_me_to_slowEP):
        .quad    .VG_(disp_cp_chain_me_to_slowEP),.TOC.@tocbase,0
        .previous
        .type    .VG_(disp_cp_chain_me_to_slowEP),@function
        .globl   .VG_(disp_cp_chain_me_to_slowEP)
.VG_(disp_cp_chain_me_to_slowEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_S, RA) */
        li   6, VG_TRC_CHAIN_ME_TO_SLOW_EP
        mflr 7
        /* 20 = imm64-fixed5 r30, disp_cp_chain_me_to_slowEP
           4  = mtctr r30
           4  = btctr
        */
        subi 7,7,20+4+4
        b    .postamble

/* ------ Chain me to fast entry point ------ */
        .section ".text"
        .align   2
        .globl   VG_(disp_cp_chain_me_to_fastEP)
        .section ".opd","aw"
        .align   3
VG_(disp_cp_chain_me_to_fastEP):
        .quad    .VG_(disp_cp_chain_me_to_fastEP),.TOC.@tocbase,0
        .previous
        .type    .VG_(disp_cp_chain_me_to_fastEP),@function
        .globl   .VG_(disp_cp_chain_me_to_fastEP)
.VG_(disp_cp_chain_me_to_fastEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_S, RA) */
        li   6, VG_TRC_CHAIN_ME_TO_FAST_EP
        mflr 7
        /* 20 = imm64-fixed5 r30, disp_cp_chain_me_to_fastEP
           4  = mtctr r30
           4  = btctr
        */
        subi 7,7,20+4+4
        b    .postamble

/* ------ Indirect but boring jump ------ */
        .section ".text"
        .align   2
        .globl   VG_(disp_cp_xindir)
        .section ".opd","aw"
        .align   3
VG_(disp_cp_xindir):
        .quad    .VG_(disp_cp_xindir),.TOC.@tocbase,0
        .previous
        .type    .VG_(disp_cp_xindir),@function
        .globl   .VG_(disp_cp_xindir)
.VG_(disp_cp_xindir):
        /* Where are we going? */
        ld      3,OFFSET_ppc64_CIA(31)

        /* stats only */
	ld	5, .tocent__vgPlain_stats__n_xindirs_32@toc(2)
        lwz     6,0(5)
        addi    6,6,1
        stw     6,0(5)

	/* r5 = &VG_(tt_fast) */
	ld	5, .tocent__vgPlain_tt_fast@toc(2) /* &VG_(tt_fast) */

        /* try a fast lookup in the translation cache */
        /* r4 = VG_TT_FAST_HASH(addr)           * sizeof(FastCacheEntry)
              = ((r3 >>u 2) & VG_TT_FAST_MASK)  << 4 */
	rldicl	4,3, 62, 64-VG_TT_FAST_BITS   /* entry# */
	sldi	4,4,4      /* entry# * sizeof(FastCacheEntry) */
	add	5,5,4      /* & VG_(tt_fast)[entry#] */
	ld	6,0(5)     /* .guest */
	ld	7,8(5)     /* .host */
        cmpd    3,6
        bne     .fast_lookup_failed

        /* Found a match.  Jump to .host. */
        mtctr   7
        bctr

.fast_lookup_failed:
        /* stats only */
	ld	5, .tocent__vgPlain_stats__n_xindir_misses_32@toc(2)
        lwz     6,0(5)
        addi    6,6,1
        stw     6,0(5)

        li      6,VG_TRC_INNER_FASTMISS
        li      7,0
        b       .postamble
	/*NOTREACHED*/

/* ------ Assisted jump ------ */
.section ".text"
        .align   2
        .globl   VG_(disp_cp_xassisted)
        .section ".opd","aw"
        .align   3
VG_(disp_cp_xassisted):
        .quad    .VG_(disp_cp_xassisted),.TOC.@tocbase,0
        .previous
        .type    .VG_(disp_cp_xassisted),@function
        .globl   .VG_(disp_cp_xassisted)
.VG_(disp_cp_xassisted):
        /* r31 contains the TRC */
        mr      6,31
        li      7,0
        b       .postamble

/* ------ Event check failed ------ */
        .section ".text"
        .align   2
        .globl   VG_(disp_cp_evcheck_fail)
        .section ".opd","aw"
        .align   3
VG_(disp_cp_evcheck_fail):
        .quad    .VG_(disp_cp_evcheck_fail),.TOC.@tocbase,0
        .previous
        .type    .VG_(disp_cp_evcheck_fail),@function
        .globl   .VG_(disp_cp_evcheck_fail)
.VG_(disp_cp_evcheck_fail):
        li      6,VG_TRC_INNER_COUNTERZERO
        li      7,0
        b       .postamble

        
.size .VG_(disp_run_translations), .-.VG_(disp_run_translations)

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif // defined(VGP_ppc64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
