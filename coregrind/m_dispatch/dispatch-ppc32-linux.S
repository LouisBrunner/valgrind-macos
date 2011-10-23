
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                       dispatch-ppc32-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2005-2011 Cerion Armour-Brown <cerion@open-works.co.uk>

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

#if defined(VGP_ppc32_linux)

#include "config.h"
#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_ppc32_CIA */


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
.text
.globl  VG_(run_innerloop)
.type  VG_(run_innerloop), @function
VG_(run_innerloop):
	/* r3 holds guest_state */
	/* r4 holds do_profiling */

        /* ----- entry point to VG_(run_innerloop) ----- */
        /* For Linux/ppc32 we need the SysV ABI, which uses
           LR->4(parent_sp), CR->anywhere.
           (The AIX ABI, used on Darwin,
           uses LR->8(prt_sp), CR->4(prt_sp))
        */

        /* Save lr */
        mflr    0
        stw     0,4(1)

        /* New stack frame */
        stwu    1,-496(1)  /* sp should maintain 16-byte alignment */

        /* Save callee-saved registers... */
	/* r3, r4 are live here, so use r5 */
        lis     5,VG_(machine_ppc32_has_FP)@ha
        lwz     5,VG_(machine_ppc32_has_FP)@l(5)
        cmplwi  5,0
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

        /* General reg save area : 72 bytes */
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
        /* Probably not necessary to save r13 (thread-specific ptr),
           as VEX stays clear of it... but what the hey. */
        stw     13,276(1)

        /* It's necessary to save/restore VRSAVE in the AIX / Darwin ABI.
           The Linux kernel might not actually use VRSAVE for its intended
           purpose, but it should be harmless to preserve anyway. */
	/* r3, r4 are live here, so use r5 */
        lis     5,VG_(machine_ppc32_has_VMX)@ha
        lwz     5,VG_(machine_ppc32_has_VMX)@l(5)
        cmplwi  5,0
        beq     LafterVMX1

#ifdef HAS_ALTIVEC
        /* VRSAVE save word : 32 bytes */
        mfspr   5,256         /* vrsave reg is spr number 256 */
        stw     5,244(1)

        /* Alignment padding : 4 bytes */

        /* Vector reg save area (quadword aligned) : 192 bytes */
        li      5,224
        stvx    31,5,1
        li      5,208
        stvx    30,5,1
        li      5,192
        stvx    29,5,1
        li      5,176
        stvx    28,5,1
        li      5,160
        stvx    27,5,1
        li      5,144
        stvx    26,5,1
        li      5,128
        stvx    25,5,1
        li      5,112
        stvx    25,5,1
        li      5,96
        stvx    23,5,1
        li      5,80
        stvx    22,5,1
        li      5,64
        stvx    21,5,1
        li      5,48
        stvx    20,5,1
#endif
        
LafterVMX1:

        /* Save cr */
        mfcr    0
        stw     0,44(1)

        /* Local variable space... */

        /* 32(sp) used later to check FPSCR[RM] */

        /* r3 holds guest_state */
        /* r4 holds do_profiling */
        mr      31,3      /* r31 (generated code gsp) = r3 */
        stw     3,28(1)   /* spill orig guest_state ptr */

        /* 24(sp) used later to stop ctr reg being clobbered */
        /* 20(sp) used later to load fpscr with zero */
        /* 8:16(sp) free */
	
        /* Linkage Area (reserved)
           4(sp)  : LR
           0(sp)  : back-chain
        */

        /* CAB TODO: Use a caller-saved reg for orig guest_state ptr
           - rem to set non-allocateable in isel.c */

        /* hold dispatch_ctr in r29 */
        lis     5,VG_(dispatch_ctr)@ha
        lwz     29,VG_(dispatch_ctr)@l(5)

        /* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        lis     5,VG_(machine_ppc32_has_FP)@ha
        lwz     5,VG_(machine_ppc32_has_FP)@l(5)
        cmplwi  5,0
        beq     LafterFP2

        /* get zero into f3 (tedious) */
        /* note: fsub 3,3,3 is not a reliable way to do this, 
           since if f3 holds a NaN or similar then we don't necessarily
           wind up with zero. */
        li      5,0
        stw     5,20(1)
        lfs     3,20(1)
        mtfsf   0xFF,3   /* fpscr = f3 */
LafterFP2:

        /* set host AltiVec control word to the default mode expected 
           by VEX-generated code. */
        lis     5,VG_(machine_ppc32_has_VMX)@ha
        lwz     5,VG_(machine_ppc32_has_VMX)@l(5)
        cmplwi  5,0
        beq     LafterVMX2

#ifdef HAS_ALTIVEC
        vspltisw 3,0x0  /* generate zero */
        mtvscr  3
#endif
        
LafterVMX2:

        /* make a stack frame for the code we are calling */
        stwu    1,-16(1)

        /* fetch %CIA into r3 */
        lwz     3,OFFSET_ppc32_CIA(31)

        /* fall into main loop (the right one) */
	/* r4 = do_profiling.  It's probably trashed after here,
           but that's OK: we don't need it after here. */
	cmplwi	4,0
	beq	VG_(run_innerloop__dispatch_unprofiled)
	b	VG_(run_innerloop__dispatch_profiled)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- NO-PROFILING (standard) dispatcher           ---*/
/*----------------------------------------------------*/

.global	VG_(run_innerloop__dispatch_unprofiled)
VG_(run_innerloop__dispatch_unprofiled):
	/* At entry: Live regs:
		r1  (=sp)
		r3  (=CIA = next guest address)
		r29 (=dispatch_ctr)
		r31 (=guest_state)
	*/
	/* Has the guest state pointer been messed with?  If yes, exit.
           Also set up & VG_(tt_fast) early in an attempt at better
           scheduling. */
        lis	5,VG_(tt_fast)@ha
        addi    5,5,VG_(tt_fast)@l   /* & VG_(tt_fast) */
        andi.   0,31,1
        bne	gsp_changed

        /* save the jump address in the guest state */
        stw     3,OFFSET_ppc32_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
	subi	29,29,1
	cmplwi	29,0
        beq	counter_is_zero

        /* try a fast lookup in the translation cache */
        /* r4 = VG_TT_FAST_HASH(addr)           * sizeof(FastCacheEntry)
              = ((r3 >>u 2) & VG_TT_FAST_MASK)  << 3 */
	rlwinm	4,3,1, 29-VG_TT_FAST_BITS, 28	/* entry# * 8 */
	add	5,5,4	/* & VG_(tt_fast)[entry#] */
	lwz	6,0(5)   /* .guest */
	lwz	7,4(5)   /* .host */
        cmpw    3,6
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

.global	VG_(run_innerloop__dispatch_profiled)
VG_(run_innerloop__dispatch_profiled):
	/* At entry: Live regs:
		r1 (=sp)
		r3  (=CIA = next guest address)
		r29 (=dispatch_ctr)
		r31 (=guest_state)
	*/
	/* Has the guest state pointer been messed with?  If yes, exit.
           Also set up & VG_(tt_fast) early in an attempt at better
           scheduling. */
        lis	5,VG_(tt_fast)@ha
        addi    5,5,VG_(tt_fast)@l   /* & VG_(tt_fast) */
        andi.   0,31,1
        bne	gsp_changed

        /* save the jump address in the guest state */
        stw     3,OFFSET_ppc32_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
	subi	29,29,1
	cmplwi	29,0
        beq	counter_is_zero

        /* try a fast lookup in the translation cache */
        /* r4 = VG_TT_FAST_HASH(addr)           * sizeof(FastCacheEntry)
              = ((r3 >>u 2) & VG_TT_FAST_MASK)  << 3 */
	rlwinm	4,3,1, 29-VG_TT_FAST_BITS, 28	/* entry# * 8 */
	add	5,5,4	/* & VG_(tt_fast)[entry#] */
	lwz	6,0(5)   /* .guest */
	lwz	7,4(5)   /* .host */
        cmpw    3,6
        bne     fast_lookup_failed

        /* increment bb profile counter */
	srwi	4,4,1	/* entry# * sizeof(UInt*) */
        addis   6,4,VG_(tt_fastN)@ha
        lwz     9,VG_(tt_fastN)@l(6)
        lwz     8,0(9)
        addi    8,8,1
        stw     8,0(9)

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
        lwz     5,44(1)         /* original guest_state ptr */
        stw     3,OFFSET_ppc32_CIA(5)
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

        /* Using r10 - value used again further on, so don't trash! */
        lis     10,VG_(machine_ppc32_has_FP)@ha
        lwz     10,VG_(machine_ppc32_has_FP)@l(10)
        cmplwi  10,0
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

	/* Using r11 - value used again further on, so don't trash! */
        lis     11,VG_(machine_ppc32_has_VMX)@ha
        lwz     11,VG_(machine_ppc32_has_VMX)@l(11)
        cmplwi  11,0
        beq     LafterVMX8

#ifdef HAS_ALTIVEC
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
        bt        24,invariant_violation  /* branch if all_equal */
#endif
LafterVMX8:

	/* otherwise we're OK */
        b       run_innerloop_exit_REALLY


invariant_violation:
        li      3,VG_TRC_INVARIANT_FAILED
        b       run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
        /* r3 holds VG_TRC_* value to return */

        /* Return to parent stack */
        addi    1,1,16

        /* Write ctr to VG(dispatch_ctr) */
        lis     5,VG_(dispatch_ctr)@ha
        stw     29,VG_(dispatch_ctr)@l(5)

        /* Restore cr */
        lwz     0,44(1)
        mtcr    0

        /* Restore callee-saved registers... */

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

        /* General regs */
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

        /* r11 already holds VG_(machine_ppc32_has_VMX) value */
        cmplwi  11,0
        beq     LafterVMX9

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

        /* reset lr & sp */
        lwz     0,500(1)  /* stack_size + 4 */
        mtlr    0
        addi    1,1,496   /* stack_size */
        blr
.size VG_(run_innerloop), .-VG_(run_innerloop)


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
.global VG_(run_a_noredir_translation)
.type VG_(run_a_noredir_translation), @function
VG_(run_a_noredir_translation):
	/* save callee-save int regs, & lr */
	stwu 1,-256(1)
	stw  14,128(1)
	stw  15,132(1)
	stw  16,136(1)
	stw  17,140(1)
	stw  18,144(1)
	stw  19,148(1)
	stw  20,152(1)
	stw  21,156(1)
	stw  22,160(1)
	stw  23,164(1)
	stw  24,168(1)
	stw  25,172(1)
	stw  26,176(1)
	stw  27,180(1)
	stw  28,184(1)
	stw  29,188(1)
	stw  30,192(1)
	stw  31,196(1)
	mflr 31
	stw  31,200(1)

	stw  3,204(1)
	lwz  31,4(3)
	lwz  30,0(3)
	mtlr 30
	blrl

	lwz  4,204(1)
	stw  3,  8(4)
	stw  31,12(4)

	lwz  14,128(1)
	lwz  15,132(1)
	lwz  16,136(1)
	lwz  17,140(1)
	lwz  18,144(1)
	lwz  19,148(1)
	lwz  20,152(1)
	lwz  21,156(1)
	lwz  22,160(1)
	lwz  23,164(1)
	lwz  24,168(1)
	lwz  25,172(1)
	lwz  26,176(1)
	lwz  27,180(1)
	lwz  28,184(1)
	lwz  29,188(1)
	lwz  30,192(1)
	lwz  31,200(1)
	mtlr 31
	lwz  31,196(1)
	addi 1,1,256
	blr
.size VG_(run_a_noredir_translation), .-VG_(run_a_noredir_translation)


/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif // defined(VGP_ppc32_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
