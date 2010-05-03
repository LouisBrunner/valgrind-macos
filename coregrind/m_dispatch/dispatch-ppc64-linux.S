
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                       dispatch-ppc64-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2005-2010 Cerion Armour-Brown <cerion@open-works.co.uk>

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
        .globl vgPlain_tt_fast
        .lcomm  vgPlain_tt_fast,4,4
        .type   vgPlain_tt_fast, @object
*/
        .section        ".toc","aw"
.tocent__vgPlain_tt_fast:
        .tc vgPlain_tt_fast[TC],vgPlain_tt_fast
.tocent__vgPlain_tt_fastN:
        .tc vgPlain_tt_fastN[TC],vgPlain_tt_fastN
.tocent__vgPlain_dispatch_ctr:
        .tc vgPlain_dispatch_ctr[TC],vgPlain_dispatch_ctr
.tocent__vgPlain_machine_ppc64_has_VMX:
        .tc vgPlain_machine_ppc64_has_VMX[TC],vgPlain_machine_ppc64_has_VMX

/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(run_innerloop) is used to    ---*/
/*--- run all translations except no-redir ones.           ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*----------------------------------------------------*/
/*--- Preamble (set everything up)                 ---*/
/*----------------------------------------------------*/

/* signature:
UWord VG_(run_innerloop) ( void* guest_state, UWord do_profiling );
*/

.section ".text"
.align   2
.globl VG_(run_innerloop)
.section ".opd","aw"
.align   3
VG_(run_innerloop):
.quad    .VG_(run_innerloop),.TOC.@tocbase,0
.previous
.type    .VG_(run_innerloop),@function
.globl   .VG_(run_innerloop)
.VG_(run_innerloop):
	/* r3 holds guest_state */
	/* r4 holds do_profiling */

        /* ----- entry point to VG_(run_innerloop) ----- */
        /* PPC64 ABI saves LR->16(prt_sp), CR->8(prt_sp)) */

        /* Save lr, cr */
        mflr    0
        std     0,16(1)
        mfcr    0
        std     0,8(1)

        /* New stack frame */
        stdu    1,-624(1)  /* sp should maintain 16-byte alignment */

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

        /* General reg save area : 144 bytes */
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
        /* Probably not necessary to save r13 (thread-specific ptr),
           as VEX stays clear of it... but what the hey. */
        std     13,328(1)

        /* It's necessary to save/restore VRSAVE in the AIX / Darwin ABI.
           The Linux kernel might not actually use VRSAVE for its intended
           purpose, but it should be harmless to preserve anyway. */
	/* r3, r4 are live here, so use r5 */
	ld	5,.tocent__vgPlain_machine_ppc64_has_VMX@toc(2)
	ld	5,0(5)
        cmpldi  5,0
        beq     .LafterVMX1

        /* VRSAVE save word : 32 bytes */
        mfspr   5,256         /* vrsave reg is spr number 256 */
        stw     5,324(1)

        /* Alignment padding : 4 bytes */

        /* Vector reg save area (quadword aligned) : 192 bytes */
        li      5,304
        stvx    31,5,1
        li      5,288
        stvx    30,5,1
        li      5,272
        stvx    29,5,1
        li      5,256
        stvx    28,5,1
        li      5,240
        stvx    27,5,1
        li      5,224
        stvx    26,5,1
        li      5,208
        stvx    25,5,1
        li      5,192
        stvx    24,5,1
        li      5,176
        stvx    23,5,1
        li      5,160
        stvx    22,5,1
        li      5,144
        stvx    21,5,1
        li      5,128
        stvx    20,5,1
.LafterVMX1:

        /* Local variable space... */

        /* r3 holds guest_state */
        /* r4 holds do_profiling */
        mr      31,3
        std     3,104(1)       /* spill orig guest_state ptr */

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

// CAB TODO: Use a caller-saved reg for orig guest_state ptr
// - rem to set non-allocateable in isel.c

        /* hold dispatch_ctr (=32bit value) in r29 */
	ld	29,.tocent__vgPlain_dispatch_ctr@toc(2)
	lwz	29,0(29)  /* 32-bit zero-extending load */

        /* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        /* => get zero into f3 (tedious)
           fsub 3,3,3 is not a reliable way to do this, since if
           f3 holds a NaN or similar then we don't necessarily
           wind up with zero. */
        li      5,0
        stw     5,88(1)
        lfs     3,88(1)
        mtfsf   0xFF,3   /* fpscr = lo32 of f3 */

        /* set host AltiVec control word to the default mode expected 
           by VEX-generated code. */
	ld	5,.tocent__vgPlain_machine_ppc64_has_VMX@toc(2)
	ld	5,0(5)
        cmpldi  5,0
        beq     .LafterVMX2

        vspltisw 3,0x0  /* generate zero */
        mtvscr  3
.LafterVMX2:

        /* make a stack frame for the code we are calling */
        stdu    1,-48(1)

        /* fetch %CIA into r3 */
        ld      3,OFFSET_ppc64_CIA(31)

        /* fall into main loop (the right one) */
	/* r4 = do_profiling.  It's probably trashed after here,
           but that's OK: we don't need it after here. */
	cmplwi	4,0
	beq	.VG_(run_innerloop__dispatch_unprofiled)
	b	.VG_(run_innerloop__dispatch_profiled)
	/*NOTREACHED*/


/*----------------------------------------------------*/
/*--- NO-PROFILING (standard) dispatcher           ---*/
/*----------------------------------------------------*/

        .section        ".text"
        .align 2
        .globl VG_(run_innerloop__dispatch_unprofiled)
        .section        ".opd","aw"
        .align 3
VG_(run_innerloop__dispatch_unprofiled):
        .quad   .VG_(run_innerloop__dispatch_unprofiled),.TOC.@tocbase,0
        .previous
        .type   .VG_(run_innerloop__dispatch_unprofiled),@function
        .globl  .VG_(run_innerloop__dispatch_unprofiled)
.VG_(run_innerloop__dispatch_unprofiled):
	/* At entry: Live regs:
		r1  (=sp)
		r2  (toc pointer)
		r3  (=CIA = next guest address)
		r29 (=dispatch_ctr)
		r31 (=guest_state)
	   Stack state:
		152(r1) (=orig guest_state)
		144(r1) (=var space for FPSCR[RM])
	*/
	/* Has the guest state pointer been messed with?  If yes, exit.
           Also set up & VG_(tt_fast) early in an attempt at better
           scheduling. */
        ld      9,152(1)        /* original guest_state ptr */
	ld	5, .tocent__vgPlain_tt_fast@toc(2) /* &VG_(tt_fast) */
        cmpd    9,31
        bne	.gsp_changed

        /* save the jump address in the guest state */
        std     3,OFFSET_ppc64_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
	subi	29,29,1
	cmpldi	29,0
        beq	.counter_is_zero

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

        /* Found a match.  Call .host. */
        mtctr   7
        bctrl

        /* On return from guest code:
	   r3  holds destination (original) address.
           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.
        */
	/* start over */
	b	.VG_(run_innerloop__dispatch_unprofiled)
	/*NOTREACHED*/
        .size VG_(run_innerloop), .-VG_(run_innerloop)


/*----------------------------------------------------*/
/*--- PROFILING dispatcher (can be much slower)    ---*/
/*----------------------------------------------------*/

        .section        ".text"
        .align 2
        .globl VG_(run_innerloop__dispatch_profiled)
        .section        ".opd","aw"
        .align 3
VG_(run_innerloop__dispatch_profiled):
        .quad   .VG_(run_innerloop__dispatch_profiled),.TOC.@tocbase,0
        .previous
        .type   .VG_(run_innerloop__dispatch_profiled),@function
        .globl  .VG_(run_innerloop__dispatch_profiled)
.VG_(run_innerloop__dispatch_profiled):
	/* At entry: Live regs:
		r1  (=sp)
		r2  (toc pointer)
		r3  (=CIA = next guest address)
		r29 (=dispatch_ctr)
		r31 (=guest_state)
	   Stack state:
		152(r1) (=orig guest_state)
		144(r1) (=var space for FPSCR[RM])
	*/
	/* Has the guest state pointer been messed with?  If yes, exit.
           Also set up & VG_(tt_fast) early in an attempt at better
           scheduling. */
        ld      9,152(1)        /* original guest_state ptr */
	ld	5, .tocent__vgPlain_tt_fast@toc(2) /* &VG_(tt_fast) */
        cmpd    9,31
        bne	.gsp_changed

        /* save the jump address in the guest state */
        std     3,OFFSET_ppc64_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
	subi	29,29,1
	cmpldi	29,0
        beq	.counter_is_zero

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

        /* increment bb profile counter VG_(tt_fastN)[x] (=32bit val) */
	ld	9, .tocent__vgPlain_tt_fastN@toc(2)
	srdi	4, 4,1     /* entry# * sizeof(UInt*) */
	ldx	9, 9,4     /* r7 = VG_(tt_fastN)[VG_TT_HASH(addr)] */
	lwz	6, 0(9)    /* *(UInt*)r7 ++ */
	addi	6, 6,1
	stw	6, 0(9)

        /* Found a match.  Call .host. */
        mtctr   7
        bctrl

        /* On return from guest code:
	   r3  holds destination (original) address.
           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.
        */
	/* start over */
	b	.VG_(run_innerloop__dispatch_profiled)
	/*NOTREACHED*/
        .size VG_(run_a_noredir_translation), .-VG_(run_a_noredir_translation)


/*----------------------------------------------------*/
/*--- exit points                                  ---*/
/*----------------------------------------------------*/

.gsp_changed:
	/* Someone messed with the gsp (in r31).  Have to
           defer to scheduler to resolve this.  dispatch ctr
	   is not yet decremented, so no need to increment. */
	/* %CIA is NOT up to date here.  First, need to write
	   %r3 back to %CIA, but without trashing %r31 since
	   that holds the value we want to return to the scheduler.
	   Hence use %r5 transiently for the guest state pointer. */
        ld      5,152(1)         /* original guest_state ptr */
        std     3,OFFSET_ppc64_CIA(5)
	mr	3,31		/* r3 = new gsp value */
	b	.run_innerloop_exit
	/*NOTREACHED*/

.counter_is_zero:
	/* %CIA is up to date */
	/* back out decrement of the dispatch counter */
        addi    29,29,1
        li      3,VG_TRC_INNER_COUNTERZERO
        b       .run_innerloop_exit

.fast_lookup_failed:
	/* %CIA is up to date */
	/* back out decrement of the dispatch counter */
        addi    29,29,1
        li      3,VG_TRC_INNER_FASTMISS
	b       .run_innerloop_exit



/* All exits from the dispatcher go through here.
   r3 holds the return value. 
*/
.run_innerloop_exit: 
        /* We're leaving.  Check that nobody messed with
           VSCR or FPSCR. */

	/* Set fpscr back to a known state, since vex-generated code
	   may have messed with fpscr[rm]. */
        li      5,0
        addi    1,1,-16
        stw     5,0(1)
        lfs     3,0(1)
        addi    1,1,16
        mtfsf   0xFF,3   /* fpscr = f3 */
	
	/* Using r11 - value used again further on, so don't trash! */
	ld	11,.tocent__vgPlain_machine_ppc64_has_VMX@toc(2)
        ld      11,0(11)
        cmpldi  11,0
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
        b       .run_innerloop_exit_REALLY


.invariant_violation:
        li      3,VG_TRC_INVARIANT_FAILED
        b       .run_innerloop_exit_REALLY

.run_innerloop_exit_REALLY:
        /* r3 holds VG_TRC_* value to return */

        /* Return to parent stack */
        addi    1,1,48

        /* Write ctr to VG_(dispatch_ctr) (=32bit value) */
	ld	5,.tocent__vgPlain_dispatch_ctr@toc(2)
        stw     29,0(5)

        /* Restore cr */
        lwz     0,44(1)
        mtcr    0

        /* Restore callee-saved registers... */

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

        /* General regs */
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

        /* r11 already holds VG_(machine_ppc64_has_VMX) value */
        cmpldi  11,0
        beq     .LafterVMX9

        /* VRSAVE */
        lwz     4,324(1)
        mfspr   4,256         /* VRSAVE reg is spr number 256 */

        /* Vector regs */
        li      4,304
        lvx     31,4,1
        li      4,288
        lvx     30,4,1
        li      4,272
        lvx     29,4,1
        li      4,256
        lvx     28,4,1
        li      4,240
        lvx     27,4,1
        li      4,224
        lvx     26,4,1
        li      4,208
        lvx     25,4,1
        li      4,192
        lvx     24,4,1
        li      4,176
        lvx     23,4,1
        li      4,160
        lvx     22,4,1
        li      4,144
        lvx     21,4,1
        li      4,128
        lvx     20,4,1
.LafterVMX9:

        /* reset cr, lr, sp */
        ld      0,632(1)  /* stack_size + 8 */
        mtcr    0
        ld      0,640(1)  /* stack_size + 16 */
        mtlr    0
        addi    1,1,624   /* stack_size */
        blr


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
.section ".text"
.align   2
.globl VG_(run_a_noredir_translation)
.section ".opd","aw"
.align   3
VG_(run_a_noredir_translation):
.quad    .VG_(run_a_noredir_translation),.TOC.@tocbase,0
.previous
.type    .VG_(run_a_noredir_translation),@function
.globl   .VG_(run_a_noredir_translation)
.VG_(run_a_noredir_translation):
	/* save callee-save int regs, & lr */
	stdu 1,-512(1)
	std  14,256(1)
	std  15,264(1)
	std  16,272(1)
	std  17,280(1)
	std  18,288(1)
	std  19,296(1)
	std  20,304(1)
	std  21,312(1)
	std  22,320(1)
	std  23,328(1)
	std  24,336(1)
	std  25,344(1)
	std  26,352(1)
	std  27,360(1)
	std  28,368(1)
	std  29,376(1)
	std  30,384(1)
	std  31,392(1)
	mflr 31
	std  31,400(1)
	std   2,408(1)  /* also preserve R2, just in case .. */

	std  3,416(1)
	ld   31,8(3)
	ld   30,0(3)
	mtlr 30
	blrl

	ld   4,416(1)
	std  3, 16(4)
	std  31,24(4)

	ld   14,256(1)
	ld   15,264(1)
	ld   16,272(1)
	ld   17,280(1)
	ld   18,288(1)
	ld   19,296(1)
	ld   20,304(1)
	ld   21,312(1)
	ld   22,320(1)
	ld   23,328(1)
	ld   24,336(1)
	ld   25,344(1)
	ld   26,352(1)
	ld   27,360(1)
	ld   28,368(1)
	ld   29,376(1)
	ld   30,384(1)
	ld   31,400(1)
	mtlr 31
	ld   31,392(1)
	ld    2,408(1)  /* also preserve R2, just in case .. */

	addi 1,1,512
	blr


/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif // defined(VGP_ppc64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
