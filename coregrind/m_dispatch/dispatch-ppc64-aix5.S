
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                        dispatch-ppc64-aix5.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2006-2010 OpenWorks LLP
     info@open-works.co.uk

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

  Neither the names of the U.S. Department of Energy nor the
  University of California nor the names of its contributors may be
  used to endorse or promote products derived from this software
  without prior written permission.
*/

#if defined(VGP_ppc64_aix5)

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_ppc64_CIA */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(run_innerloop) is used to    ---*/
/*--- run all translations except no-redir ones.           ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*----------------------------------------------------*/
/*--- Incomprehensible TOC mumbo-jumbo nonsense.   ---*/
/*----------------------------------------------------*/

/* No, I don't have a clue either.  I just compiled a bit of
   C with gcc and copied the assembly code it produced. */

/* Basically "ld rd, tocent__foo(2)" gets &foo into rd. */

    .file       "dispatch-ppc64-aix5.S"
    .machine	"ppc64"
    .toc
    .csect .text[PR]
    .toc
tocent__vgPlain_dispatch_ctr:
    .tc vgPlain_dispatch_ctr[TC],vgPlain_dispatch_ctr[RW]
tocent__vgPlain_machine_ppc64_has_VMX:
    .tc vgPlain_machine_ppc64_has_VMX[TC],vgPlain_machine_ppc64_has_VMX[RW]
tocent__vgPlain_tt_fast:
    .tc vgPlain_tt_fast[TC],vgPlain_tt_fast[RW]
tocent__vgPlain_tt_fastN:
    .tc vgPlain_tt_fast[TC],vgPlain_tt_fastN[RW]
    .csect .text[PR]
    .align 2
    .globl vgPlain_run_innerloop
    .globl .vgPlain_run_innerloop
    .csect vgPlain_run_innerloop[DS]
vgPlain_run_innerloop:
    .llong .vgPlain_run_innerloop, TOC[tc0], 0
    .csect .text[PR]

/*----------------------------------------------------*/
/*--- Preamble (set everything up)                 ---*/
/*----------------------------------------------------*/

/* signature:
UWord VG_(run_innerloop) ( void* guest_state, UWord do_profiling );
*/
.vgPlain_run_innerloop:

	/* r3 holds guest_state */
	/* r4 holds do_profiling */
	/* Rather than attempt to make sense of the AIX ABI, just
           drop r1 by 512 (to get away from the caller's frame), then
	   1024 (to give ourselves a 1024-byte save area), and then
	   another 512 (to clear our save area).  In all, drop r1 by 2048
	   and dump stuff on the stack at 512(1)..1536(1).  */

        /* ----- entry point to VG_(run_innerloop) ----- */
        /* For AIX/ppc64 we do:	 LR-> +16(parent_sp), CR-> +8(parent_sp) */

        /* Save lr and cr*/
        mflr    0
        std     0,16(1)
	mfcr	0
	std	0,8(1)

        /* New stack frame */
        stdu    1,-2048(1)  /* sp should maintain 16-byte alignment */

        /* Save callee-saved registers... */
	/* r3, r4 are live here, so use r5 */

        /* Floating-point reg save area : 144 bytes at r1[256+256..256+399] */
        stfd    31,256+392(1)
        stfd    30,256+384(1)
        stfd    29,256+376(1)
        stfd    28,256+368(1)
        stfd    27,256+360(1)
        stfd    26,256+352(1)
        stfd    25,256+344(1)
        stfd    24,256+336(1)
        stfd    23,256+328(1)
        stfd    22,256+320(1)
        stfd    21,256+312(1)
        stfd    20,256+304(1)
        stfd    19,256+296(1)
        stfd    18,256+288(1)
        stfd    17,256+280(1)
        stfd    16,256+272(1)
        stfd    15,256+264(1)
        stfd    14,256+256(1)

        /* General reg save area : 76 bytes at r1[256+400 .. 256+543] */
        std     31,256+544(1)
        std     30,256+536(1)
        std     29,256+528(1)
        std     28,256+520(1)
        std     27,256+512(1)
        std     26,256+504(1)
        std     25,256+496(1)
        std     24,256+488(1)
        std     23,256+480(1)
        std     22,256+472(1)
        std     21,256+464(1)
        std     20,256+456(1)
        std     19,256+448(1)
        std     18,256+440(1)
        std     17,256+432(1)
        std     16,256+424(1)
        std     15,256+416(1)
        std     14,256+408(1)
        /* Probably not necessary to save r13 (thread-specific ptr),
           as VEX stays clear of it... but what the hell. */
        std     13,256+400(1)

        /* It's necessary to save/restore VRSAVE in the AIX / Darwin ABI.
           The Linux kernel might not actually use VRSAVE for its intended
           purpose, but it should be harmless to preserve anyway. */
	/* r3, r4 are live here, so use r5 */
        ld      5,tocent__vgPlain_machine_ppc64_has_VMX(2)
        ld      5,0(5)
        cmpldi  5,0
        beq     LafterVMX1

//	Sigh.  AIX 5.2 has no idea that Altivec exists.
//        /* VRSAVE save word : 4 bytes at r1[476 .. 479] */
//        mfspr   5,256        /* vrsave reg is spr number 256 */
//        stw     5,476(1)
//
//        /* Vector reg save area (quadword aligned): 
//	   192 bytes at r1[480 .. 671] */
//        li      5,656
//        stvx    31,5,1
//        li      5,640
//        stvx    30,5,1
//        li      5,624
//        stvx    29,5,1
//        li      5,608
//        stvx    28,5,1
//        li      5,592
//        stvx    27,5,1
//        li      5,576
//        stvx    26,5,1
//        li      5,560
//        stvx    25,5,1
//        li      5,544
//        stvx    25,5,1
//        li      5,528
//        stvx    23,5,1
//        li      5,512
//        stvx    22,5,1
//        li      5,496
//        stvx    21,5,1
//        li      5,480
//        stvx    20,5,1
LafterVMX1:

        /* Local variable space... */
	/* Put the original guest state pointer at r1[256].  We
           will need to refer to it each time round the dispatch loop.
	   Apart from that, we can use r1[0 .. 255] and r1[264 .. 511]
	   as scratch space. */

        /* r3 holds guest_state */
        /* r4 holds do_profiling */
        mr      31,3      /* r31 (generated code gsp) = r3 */
        std     3,256(1)  /* stash orig guest_state ptr */

        /* hold dispatch_ctr (NOTE: 32-bit value) in r29 */
        ld      5,tocent__vgPlain_dispatch_ctr(2)
        lwz     29,0(5)	/* 32-bit zero-extending load */

        /* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        /* get zero into f3 (tedious) */
        /* note: fsub 3,3,3 is not a reliable way to do this, 
           since if f3 holds a NaN or similar then we don't necessarily
           wind up with zero. */
        li      5,0
        std     5,128(1) /* r1[128] is scratch */
        lfd     3,128(1)
        mtfsf   0xFF,3   /* fpscr = f3 */

        /* set host AltiVec control word to the default mode expected 
           by VEX-generated code. */
        ld      5,tocent__vgPlain_machine_ppc64_has_VMX(2)
        ld      5,0(5)
        cmpldi  5,0
        beq     LafterVMX2

//	Sigh.  AIX 5.2 has no idea that Altivec exists.
//        vspltisw 3,0x0  /* generate zero */
//        mtvscr  3
LafterVMX2:

        /* fetch %CIA into r3 */
        ld     3,OFFSET_ppc64_CIA(31)

        /* fall into main loop (the right one) */
	/* r4 = do_profiling.  It's probably trashed after here,
           but that's OK: we don't need it after here. */
	cmpldi	4,0
	beq	VG_(run_innerloop__dispatch_unprofiled)
	b	VG_(run_innerloop__dispatch_profiled)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- NO-PROFILING (standard) dispatcher           ---*/
/*----------------------------------------------------*/

.globl VG_(run_innerloop__dispatch_unprofiled)
VG_(run_innerloop__dispatch_unprofiled):
	/* At entry: Live regs:
		r1  (=sp)
		r3  (=CIA = next guest address)
		r29 (=dispatch_ctr)
		r31 (=guest_state)
	   Stack state:
		256(r1) (=orig guest_state)
	*/

	/* Has the guest state pointer been messed with?  If yes, exit. */
        ld      5,256(1)         /* original guest_state ptr */
        cmpd    5,31
	ld	5,tocent__vgPlain_tt_fast(2)	/* &VG_(tt_fast) */
        bne	gsp_changed

        /* save the jump address in the guest state */
        std     3,OFFSET_ppc64_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
	addi	29,29,-1
	cmplwi	29,0	/* yes, lwi - is 32-bit */
        beq	counter_is_zero

        /* try a fast lookup in the translation cache */
        /* r4 = VG_TT_FAST_HASH(addr)           * sizeof(FastCacheEntry)
              = ((r3 >>u 2) & VG_TT_FAST_MASK)  << 4 */
        rldicl  4,3, 62, 64-VG_TT_FAST_BITS   /* entry# */
        sldi    4,4,4    /* entry# * sizeof(FastCacheEntry) */
	add     5,5,4	/* &VG_(tt_fast)[entry#] */
	ld      6,0(5)  /* .guest */
	ld      7,8(5)  /* .host */
        cmpd    3,6
        bne     fast_lookup_failed

        /* Found a match.  Call .host. */
        mtctr   7
        bctrl

        /* On return from guest code:
	   r3  holds destination (original) address.
           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.
        */
	/* start over */
	b	VG_(run_innerloop__dispatch_unprofiled)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- PROFILING dispatcher (can be much slower)    ---*/
/*----------------------------------------------------*/

.globl VG_(run_innerloop__dispatch_profiled)
VG_(run_innerloop__dispatch_profiled):
	/* At entry: Live regs:
		r1  (=sp)
		r3  (=CIA = next guest address)
		r29 (=dispatch_ctr)
		r31 (=guest_state)
	   Stack state:
		256(r1) (=orig guest_state)
	*/

	/* Has the guest state pointer been messed with?  If yes, exit. */
        ld      5,256(1)         /* original guest_state ptr */
        cmpd    5,31
	ld	5,tocent__vgPlain_tt_fast(2)	/* &VG_(tt_fast) */
        bne	gsp_changed

        /* save the jump address in the guest state */
        std     3,OFFSET_ppc64_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
	addi	29,29,-1
	cmplwi	29,0	/* yes, lwi - is 32-bit */
        beq	counter_is_zero

        /* try a fast lookup in the translation cache */
        /* r4 = VG_TT_FAST_HASH(addr)           * sizeof(FastCacheEntry)
              = ((r3 >>u 2) & VG_TT_FAST_MASK)  << 4 */
        rldicl  4,3, 62, 64-VG_TT_FAST_BITS   /* entry# */
        sldi    4,4,4    /* entry# * sizeof(FastCacheEntry) */
	add     5,5,4	/* &VG_(tt_fast)[entry#] */
	ld      6,0(5)  /* .guest */
	ld      7,8(5)  /* .host */
        cmpd    3,6
        bne     fast_lookup_failed

        /* increment bb profile counter */
	ld      9,tocent__vgPlain_tt_fastN(2)   /* r9 = &tt_fastN */
	srdi	4,4,1   /* entry# * sizeof(UInt*) */	
        ldx     8,9,4   /* r7 = tt_fastN[r4] */
        lwz     10,0(8)
        addi    10,10,1
        stw     10,0(8)
	
        /* Found a match.  Call .host. */
        mtctr   7
        bctrl

        /* On return from guest code:
	   r3  holds destination (original) address.
           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.
        */
	/* start over */
	b	VG_(run_innerloop__dispatch_profiled)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- exit points                                  ---*/
/*----------------------------------------------------*/

gsp_changed:
	/* Someone messed with the gsp (in r31).  Have to
           defer to scheduler to resolve this.  dispatch ctr
	   is not yet decremented, so no need to increment. */
	/* %CIA is NOT up to date here.  First, need to write
	   %r3 back to %CIA, but without trashing %r31 since
	   that holds the value we want to return to the scheduler.
	   Hence use %r5 transiently for the guest state pointer. */
        ld      5,256(1)        /* original guest_state ptr */
        std     3,OFFSET_ppc64_CIA(5)
	mr	3,31		/* r3 = new gsp value */
	b	run_innerloop_exit
	/*NOTREACHED*/

counter_is_zero:
	/* %CIA is up to date */
	/* back out decrement of the dispatch counter */
        addi    29,29,1
        li      3,VG_TRC_INNER_COUNTERZERO
        b       run_innerloop_exit

fast_lookup_failed:
	/* %CIA is up to date */
	/* back out decrement of the dispatch counter */
        addi    29,29,1
        li      3,VG_TRC_INNER_FASTMISS
	b       run_innerloop_exit



/* All exits from the dispatcher go through here.
   r3 holds the return value. 
*/
run_innerloop_exit: 
        /* We're leaving.  Check that nobody messed with
           VSCR or FPSCR. */

	/* Set fpscr back to a known state, since vex-generated code
	   may have messed with fpscr[rm]. */
        li      5,0
        std     5,128(1) /* r1[128] is scratch */
        lfd     3,128(1)
        mtfsf   0xFF,3   /* fpscr = f3 */

	/* Using r11 - value used again further on, so don't trash! */
        ld      11,tocent__vgPlain_machine_ppc64_has_VMX(2)
        ld      11,0(11)
        cmpldi  11,0
        beq     LafterVMX8

//	Sigh.  AIX 5.2 has no idea that Altivec exists.
//        /* Check VSCR[NJ] == 1 */
//        /* first generate 4x 0x00010000 */
//        vspltisw  4,0x1                   /* 4x 0x00000001 */
//        vspltisw  5,0x0                   /* zero */
//        vsldoi    6,4,5,0x2               /* <<2*8 => 4x 0x00010000 */
//        /* retrieve VSCR and mask wanted bits */
//        mfvscr    7
//        vand      7,7,6                   /* gives NJ flag */
//        vspltw    7,7,0x3                 /* flags-word to all lanes */
//        vcmpequw. 8,6,7                   /* CR[24] = 1 if v6 == v7 */
//        bt        24,invariant_violation  /* branch if all_equal */
LafterVMX8:

	/* otherwise we're OK */
        b       run_innerloop_exit_REALLY


invariant_violation:
        li      3,VG_TRC_INVARIANT_FAILED
        b       run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
        /* r3 holds VG_TRC_* value to return */

        /* Write ctr to VG(dispatch_ctr) */
        ld      5,tocent__vgPlain_dispatch_ctr(2)
        stw     29,0(5)	 /* yes, really stw */

        /* Restore callee-saved registers... */

        /* Floating-point regs */
        lfd     31,256+392(1)
        lfd     30,256+384(1)
        lfd     29,256+376(1)
        lfd     28,256+368(1)
        lfd     27,256+360(1)
        lfd     26,256+352(1)
        lfd     25,256+344(1)
        lfd     24,256+336(1)
        lfd     23,256+328(1)
        lfd     22,256+320(1)
        lfd     21,256+312(1)
        lfd     20,256+304(1)
        lfd     19,256+296(1)
        lfd     18,256+288(1)
        lfd     17,256+280(1)
        lfd     16,256+272(1)
        lfd     15,256+264(1)
        lfd     14,256+256(1)

        /* General regs */
        ld      31,256+544(1)
        ld      30,256+536(1)
        ld      29,256+528(1)
        ld      28,256+520(1)
        ld      27,256+512(1)
        ld      26,256+504(1)
        ld      25,256+496(1)
        ld      24,256+488(1)
        ld      23,256+480(1)
        ld      22,256+472(1)
        ld      21,256+464(1)
        ld      20,256+456(1)
        ld      19,256+448(1)
        ld      18,256+440(1)
        ld      17,256+432(1)
        ld      16,256+424(1)
        ld      15,256+416(1)
        ld      14,256+408(1)
        ld      13,256+400(1)

        /* r11 already holds VG_(machine_ppc64_has_VMX) value */
        cmpldi  11,0
        beq     LafterVMX9

//       Sigh.  AIX 5.2 has no idea that Altivec exists.
//        /* VRSAVE */
//        lwz     4,476(1)
//        mtspr   4,256         /* VRSAVE reg is spr number 256 */
//
//        /* Vector regs */
//        li      4,656
//        lvx     31,4,1
//        li      4,640
//        lvx     30,4,1
//        li      4,624
//        lvx     29,4,1
//        li      4,608
//        lvx     28,4,1
//        li      4,592
//        lvx     27,4,1
//        li      4,576
//        lvx     26,4,1
//        li      4,560
//        lvx     25,4,1
//        li      4,544
//        lvx     24,4,1
//        li      4,528
//        lvx     23,4,1
//        li      4,512
//        lvx     22,4,1
//        li      4,496
//        lvx     21,4,1
//        li      4,480
//        lvx     20,4,1
LafterVMX9:

	/* r3 is live here; don't trash it */
        /* restore lr,cr,sp */
	addi	4,1,2048 /* r4 = old SP */
	ld	0,16(4)
	mtlr	0
	ld	0,8(4)
	mtcr	0
	mr	1,4
	blr

LT..vgPlain_run_innerloop:
    .long 0
    .byte 0,0,32,64,0,0,1,0
    .long 0
    .long LT..vgPlain_run_innerloop-.vgPlain_run_innerloop
    .short 3
    .byte "vgPlain_run_innerloop"
    .align 2
_section_.text:
    .csect .data[RW],3
    .llong _section_.text

/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- A special dispatcher, for running no-redir           ---*/
/*--- translations.  Just runs the given translation once. ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/
	
/* signature:
void VG_(run_a_noredir_translation) ( UWord* argblock );
*/

/* Run a no-redir translation.  argblock points to 4 UWords, 2 to carry args
   and 2 to carry results:
      0: input:  ptr to translation
      1: input:  ptr to guest state
      2: output: next guest PC
      3: output: guest state pointer afterwards (== thread return code)
*/
.csect .text[PR]
.align 2
.globl  .VG_(run_a_noredir_translation)
.VG_(run_a_noredir_translation):
	/* Rather than attempt to make sense of the AIX ABI, just
           drop r1 by 512 (to get away from the caller's frame), then
	   1024 (to give ourselves a 1024-byte save area), and then
	   another 1024 (to clear our save area).  In all, drop r1 by 2048
	   and dump stuff on the stack at 512(1)..1536(1).  */
	/* At entry, r3 points to argblock */

        /* ----- entry point to VG_(run_innerloop) ----- */
        /* For AIX/ppc64 we do:	 LR-> +16(parent_sp), CR-> +8(parent_sp) */

        /* Save lr and cr*/
        mflr    0
        std     0,16(1)
	mfcr	0
	std	0,8(1)

        /* New stack frame */
        stdu    1,-2048(1)  /* sp should maintain 16-byte alignment */

        /* General reg save area : 160 bytes at r1[512 .. 671] */
        std     31,664(1)
        std     30,656(1)
        std     29,648(1)
        std     28,640(1)
        std     27,632(1)
        std     26,624(1)
        std     25,616(1)
        std     24,608(1)
        std     23,600(1)
        std     22,592(1)
        std     21,584(1)
        std     20,576(1)
        std     19,568(1)
        std     18,560(1)
        std     17,552(1)
        std     16,544(1)
        std     15,536(1)
        std     14,528(1)
        std     13,520(1)
	std	 3,512(1)	/* will need it later */
	
        ld      31,8(3)		/* rd argblock[1] */
        ld      30,0(3)		/* rd argblock[0] */
        mtlr    30		/* run translation */
        blrl

        ld      4,512(1)	/* &argblock */
        std     3, 16(4)	/* wr argblock[2] */
        std     31,24(4)	/* wr argblock[3] */
			
        /* General regs */
        ld      31,664(1)
        ld      30,656(1)
        ld      29,648(1)
        ld      28,640(1)
        ld      27,632(1)
        ld      26,624(1)
        ld      25,616(1)
        ld      24,608(1)
        ld      23,600(1)
        ld      22,592(1)
        ld      21,584(1)
        ld      20,576(1)
        ld      19,568(1)
        ld      18,560(1)
        ld      17,552(1)
        ld      16,544(1)
        ld      15,536(1)
        ld      14,528(1)
        ld      13,520(1)

        /* restore lr,cr,sp */
	addi	4,1,2048  /* r4 = old SP */
	ld	0,16(4)
	mtlr	0
	ld	0,8(4)
	mtcr	0
	mr	1,4
	blr

#endif // defined(VGP_ppc64_aix5)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
