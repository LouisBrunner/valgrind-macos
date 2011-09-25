
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                       dispatch-s390x-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright IBM Corp. 2010-2011

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

/* Contributed by Florian Krohm and Christian Borntraeger */

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"
#include "libvex_s390x_common.h"

#if defined(VGA_s390x)

/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(run_innerloop) is used to    ---*/
/*--- run all translations except no-redir ones.           ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Convenience definitions for readability */
#undef  SP
#define SP S390_REGNO_STACK_POINTER

#undef  LR
#define LR S390_REGNO_LINK_REGISTER

/* Location of valgrind's saved FPC register */
#define S390_LOC_SAVED_FPC_V S390_OFFSET_SAVED_FPC_V(SP)

/* Location of saved guest state pointer */
#define S390_LOC_SAVED_GSP S390_OFFSET_SAVED_GSP(SP)

/* Location of saved R2 register */
#define S390_LOC_SAVED_R2 S390_OFFSET_SAVED_R2(SP)

/*----------------------------------------------------*/
/*--- Preamble (set everything up)                 ---*/
/*----------------------------------------------------*/

/* signature:
UWord VG_(run_innerloop) ( void* guest_state, UWord do_profiling );
*/

.text
.align   4
.globl VG_(run_innerloop)
VG_(run_innerloop):
        /* r2 holds address of guest_state */
        /* r3 holds do_profiling (a flag) */

        /* Save gprs   ABI: r6...r13 and r15 */
        stmg %r6,%r15,48(SP)

        /* New stack frame */
        aghi SP,-S390_INNERLOOP_FRAME_SIZE

        /* Save fprs:   ABI: f8...f15 */
        std  %f8,160+0(SP)
        std  %f9,160+8(SP)
        std  %f10,160+16(SP)
        std  %f11,160+24(SP)
        std  %f12,160+32(SP)
        std  %f13,160+40(SP)
        std  %f14,160+48(SP)
        std  %f15,160+56(SP)

        /* Load address of guest state into guest state register (r13) */
        lgr  %r13,%r2

        /* Store address of guest state pointer on stack.
           It will be needed later because upon return from a VEX translation
           r13 may contain a special value. So the old value will be used to
           determine whether r13 contains a special value. */
        stg  %r13,S390_LOC_SAVED_GSP

        /* Save valgrind's FPC on stack so run_innerloop_exit can restore
           it later . */
        stfpc S390_LOC_SAVED_FPC_V

        /* Load the FPC the way the client code wants it. I.e. pull the
           value from the guest state.
        lfpc OFFSET_s390x_fpc(%r13)

        /* Get the IA from the guest state */
        lg   %r2,OFFSET_s390x_IA(%r13)

        /* Get VG_(dispatch_ctr) -- a 32-bit value -- and store it in a reg */
        larl %r6,VG_(dispatch_ctr)
        l    S390_REGNO_DISPATCH_CTR,0(%r6)

        /* Fall into main loop (the right one) */

        /* r3 = 1 --> do_profiling. We may trash r3 later on. That's OK,
           because it's a volatile register (does not need to be preserved). */
        ltgr %r3,%r3
        je   run_innerloop__dispatch_unprofiled
        j    run_innerloop__dispatch_profiled

/*----------------------------------------------------*/
/*--- NO-PROFILING (standard) dispatcher           ---*/
/*----------------------------------------------------*/

run_innerloop__dispatch_unprofiled:
        /* Load the link register with the address the jitted code will
           return to when it's done executing. The link register is loaded
           exactly once per loop. This is safe, because the jitted code 
           cannot possibly modify the LR. How else would it be able to return
           to the location in the LR otherwise? */
        basr LR,0

        /* Loop begins here */
        
        /* This is the story:

           r2  = IA = next guest address
           r12 = VG_(dispatch_ctr)
           r13 = guest state pointer or (upon return from guest code) some
                 special value
           r15 = stack pointer (as usual)
        */
innermost_loop: 
        /* Has the guest state pointer been messed with? If yes, exit.
           The mess is recognised by r13 containing an odd value. */
        tmll %r13,1
        jne  gsp_changed

        /* Save the jump address in the guest state */
        stg  %r2,OFFSET_s390x_IA(%r13)


	/* Try a fast lookup in the translation cache:
           Compute offset (not index) into VT_(tt_fast):

           offset = VG_TT_FAST_HASH(addr) * sizeof(FastCacheEntry)

           with VG_TT_FAST_HASH(addr) == (addr >> 1) & VG_TT_FAST_MASK
           and  sizeof(FastCacheEntry) == 16

           offset = ((addr >> 1) & VG_TT_FAST_MASK) << 4
           which is
           offset = ((addr & (VG_TT_FAST_MASK << 1) ) << 3
        */
        larl  %r8, VG_(tt_fast)
        llill %r5,( VG_TT_FAST_MASK << 1) & 0xffff
#if ((( VG_TT_FAST_MASK << 1) & 0xffff0000) >> 16 != 0)
        iilh %r5,(( VG_TT_FAST_MASK << 1) & 0xffff0000) >> 16
#endif
        ngr  %r5,%r2
        sllg %r7,%r5,3

	/* Are we out of timeslice?  If yes, defer to scheduler. */
        ahi  S390_REGNO_DISPATCH_CTR,-1
        jz   counter_is_zero

        lg   %r11, 8(%r8,%r7)      /* .host */
        cg   %r2,  0(%r8,%r7)      /* next guest address == .guest ? */
        jne  fast_lookup_failed

        /* Found a match.  Call .host.
           r11 is an address. There we will find the instrumented client code.
           That code may modify the guest state register r13. The client code
           will return to the beginning of this loop start by issuing br LR.
           We can simply branch to the host code */
        br %r11


/*----------------------------------------------------*/
/*--- PROFILING dispatcher (can be much slower)    ---*/
/*----------------------------------------------------*/

run_innerloop__dispatch_profiled:
        stg  %r2,S390_LOC_SAVED_R2

        /* Load the link register with the address the jitted code will
           return to when it's done executing. */
        bras LR,innermost_loop

        /* Jitted code returns here. Update profile counter for previous IA */

        llill %r5,( VG_TT_FAST_MASK << 1) & 0xffff
#if ((( VG_TT_FAST_MASK << 1) & 0xffff0000) >> 16 != 0)
        iilh %r5,(( VG_TT_FAST_MASK << 1) & 0xffff0000) >> 16
#endif
        ng   %r5,S390_LOC_SAVED_R2
        sllg %r7,%r5,2

        /* Increment bb profile counter */
        larl %r8, VG_(tt_fastN)
        lg   %r9,0(%r8,%r7)
        l    %r10,0(%r9)
        ahi  %r10,1
        st   %r10,0(%r9)

        j    run_innerloop__dispatch_profiled

/*----------------------------------------------------*/
/*--- exit points                                  ---*/
/*----------------------------------------------------*/

gsp_changed:
	/* Someone messed with the gsp (in r13).  Have to
           defer to scheduler to resolve this.  The register
           holding VG_(dispatch_ctr) is not yet decremented,
           so no need to increment. */

        /* Update the IA in the guest state */
        lg  %r6,S390_LOC_SAVED_GSP       /* r6 = original guest state pointer */
        stg %r2,OFFSET_s390x_IA(%r6)

        /* Return the special guest state pointer value */
        lgr %r2, %r13
	j   run_innerloop_exit


counter_is_zero:
	/* IA is up to date */

	/* Back out decrement of the dispatch counter */
        ahi S390_REGNO_DISPATCH_CTR,1

        /* Set return value for the scheduler */
        lghi %r2,VG_TRC_INNER_COUNTERZERO
        j    run_innerloop_exit


fast_lookup_failed:
	/* IA is up to date */

	/* Back out decrement of the dispatch counter */
        ahi S390_REGNO_DISPATCH_CTR,1

        /* Set return value for the scheduler */
        lghi %r2,VG_TRC_INNER_FASTMISS
        j    run_innerloop_exit


        /* All exits from the dispatcher go through here.
           When we come here r2 holds the return value. */
run_innerloop_exit:

	/* Restore valgrind's FPC, as client code may have changed it. */
        lfpc S390_LOC_SAVED_FPC_V

        /* Write ctr to VG_(dispatch_ctr) (=32bit value) */
        larl %r6,VG_(dispatch_ctr)
        st   S390_REGNO_DISPATCH_CTR,0(%r6)

        /* Restore callee-saved registers... */

        /* Floating-point regs */
        ld  %f8,160+0(SP)
        ld  %f9,160+8(SP)
        ld  %f10,160+16(SP)
        ld  %f11,160+24(SP)
        ld  %f12,160+32(SP)
        ld  %f13,160+40(SP)
        ld  %f14,160+48(SP)
        ld  %f15,160+56(SP)

        /* Remove atack frame */
        aghi SP,S390_INNERLOOP_FRAME_SIZE

        /* General-purpose regs. This also restores the original link
           register (r14) and stack pointer (r15). */
        lmg %r6,%r15,48(SP)

        /* Return */
        br  LR

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
.text
.align   4
.globl VG_(run_a_noredir_translation)
VG_(run_a_noredir_translation):
        stmg %r6,%r15,48(SP)
        aghi SP,-S390_INNERLOOP_FRAME_SIZE
        std  %f8,160+0(SP)
        std  %f9,160+8(SP)
        std  %f10,160+16(SP)
        std  %f11,160+24(SP)
        std  %f12,160+32(SP)
        std  %f13,160+40(SP)
        std  %f14,160+48(SP)
        std  %f15,160+56(SP)

        /* Load address of guest state into guest state register (r13) */
        lg   %r13,8(%r2)

        /* Get the IA */
        lg   %r11,0(%r2)

        /* save r2 (argblock) as it is clobbered */
	stg  %r2,160+64(SP)

        /* the call itself */
        basr LR,%r11

        /* restore argblock */
	lg   %r1,160+64(SP)
	/* save the next guest PC */
	stg  %r2,16(%r1)

	/* save the guest state */
	stg  %r13,24(%r1)

        /* Restore Floating-point regs */
        ld  %f8,160+0(SP)
        ld  %f9,160+8(SP)
        ld  %f10,160+16(SP)
        ld  %f11,160+24(SP)
        ld  %f12,160+32(SP)
        ld  %f13,160+40(SP)
        ld  %f14,160+48(SP)
        ld  %f15,160+56(SP)

        aghi SP,S390_INNERLOOP_FRAME_SIZE

        lmg %r6,%r15,48(SP)
	br  %r14


/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif /* VGA_s390x */

/*--------------------------------------------------------------------*/
/*--- end                                   dispatch-s390x-linux.S ---*/
/*--------------------------------------------------------------------*/
