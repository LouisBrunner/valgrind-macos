
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                        sigframe-ppc64-aix5.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_machine.h"
#include "pub_core_options.h"
#include "pub_core_signals.h"
#include "pub_core_tooliface.h"
#include "pub_core_trampoline.h"
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "pub_core_sigframe.h"      /* self */


/* This module creates and removes signal frames for signal deliveries
   on ppc64-aix5.  Kludgey; the machine state ought to be saved in a
   ucontext and retrieved from it later, so the handler can modify it
   and return.  However .. for now .. just stick the vex guest state
   in the frame and snarf it again later.

   Also, don't bother with creating siginfo and ucontext in the
   handler, although do point them somewhere non-faulting.
*/
struct hacky_sigframe {
   UChar              lower_guardzone[1024];  // put nothing here
   VexGuestPPC64State gst;
   VexGuestPPC64State gshadow1;
   VexGuestPPC64State gshadow2;
   UInt               magicPI;
   UInt               sigNo_private;
   UInt               tramp[2];
   UChar              upper_guardzone[1024]; // put nothing here
};


/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful.
*/
static Bool extend ( ThreadState *tst, Addr addr, SizeT size )
{
   ThreadId tid = tst->tid;
   /* For tracking memory events, indicate the entire frame has been
      allocated.  Except, don't mess with the area which
      overlaps the previous frame's redzone. */
   /* XXX is the following call really right?  compared with the
      amd64-linux version, this doesn't appear to handle the redzone
      in the same way. */
   VG_TRACK( new_mem_stack_signal,
             addr, size - VG_STACK_REDZONE_SZB, tid );
   return True;
}

#define SET_SIGNAL_LR(zztst, zzval)                          \
   do { tst->arch.vex.guest_LR = (zzval);                    \
      VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,     \
                offsetof(VexGuestPPC64State,guest_LR),       \
                sizeof(UWord) );                             \
   } while (0)

#define SET_SIGNAL_GPR(zztst, zzn, zzval)                    \
   do { tst->arch.vex.guest_GPR##zzn = (zzval);              \
      VG_TRACK( post_reg_write, Vg_CoreSignal, tst->tid,     \
                offsetof(VexGuestPPC64State,guest_GPR##zzn), \
                sizeof(UWord) );                             \
   } while (0)


/* Create a signal frame for thread 'tid'. */
void VG_(sigframe_create) ( ThreadId tid,
                            Addr sp_top_of_frame,
                            const vki_siginfo_t *siginfo,
                            const struct vki_ucontext *siguc,
                            void *handler,
                            UInt flags,
                            const vki_sigset_t *mask,
                            void *restorer )
{
   ThreadState* tst;
   Addr sp;
   struct hacky_sigframe* frame;
   Int sigNo = siginfo->si_signo;
   Int __NR_FAKE_SIGRETURN = __NR_AIX5_FAKE_SIGRETURN;

   vg_assert(VG_IS_16_ALIGNED(sizeof(struct hacky_sigframe)));

   sp_top_of_frame &= ~0xf;
   sp = sp_top_of_frame - sizeof(struct hacky_sigframe);

   tst = VG_(get_ThreadState)(tid);
   if (!extend(tst, sp, sp_top_of_frame - sp))
     return;

   vg_assert(VG_IS_16_ALIGNED(sp));

   frame = (struct hacky_sigframe *) sp;

   /* clear it (very conservatively) */
   VG_(memset)(&frame->lower_guardzone, 0, 1024);
   VG_(memset)(&frame->gst,      0, sizeof(VexGuestPPC64State));
   VG_(memset)(&frame->gshadow1, 0, sizeof(VexGuestPPC64State));
   VG_(memset)(&frame->gshadow2, 0, sizeof(VexGuestPPC64State));

   /* save stuff in frame */
   frame->gst           = tst->arch.vex;
   frame->gshadow1      = tst->arch.vex_shadow1;
   frame->gshadow2      = tst->arch.vex_shadow2;
   frame->sigNo_private = sigNo;
   frame->magicPI       = 0x31415927;

   /* Set up stack frame pointer */
   sp += 512;
   vg_assert(sp == (Addr)&frame->lower_guardzone[512]);
   VG_TRACK( pre_mem_write, Vg_CoreSignal, tid, "signal handler frame",
             sp, sizeof(UWord) );
   *(Addr*)sp = tst->arch.vex.guest_GPR1;
   VG_TRACK( post_mem_write, Vg_CoreSignal, tid,
             sp, sizeof(UWord) );

   /* Set regs for the handler */
   SET_SIGNAL_GPR(tid, 1, sp);
   SET_SIGNAL_GPR(tid, 2, ((UWord*)handler)[1]);
   SET_SIGNAL_GPR(tid, 3, sigNo);
   SET_SIGNAL_GPR(tid, 4, 0); /* XXX: the siginfo* */
   SET_SIGNAL_GPR(tid, 5, 0); /* XXX: the ucontext* */
   tst->arch.vex.guest_CIA = ((UWord*)handler)[0];

   /* set up return trampoline */
   vg_assert(__NR_FAKE_SIGRETURN >= 10000);
   vg_assert(__NR_FAKE_SIGRETURN <= 32767);
   frame->tramp[0] = 0x38400000U 
                     + __NR_FAKE_SIGRETURN; /* li 2,__NR_FAKE_SIGRETURN */
   frame->tramp[1] = 0x44000002U;           /* sc */

   /* invalidate any translation of this area */
   VG_(discard_translations)( (Addr64)(Addr)&frame->tramp[0], 
                              sizeof(frame->tramp), "sigframe tramp" );   
   /* set the signal handler to return to the trampoline */
   SET_SIGNAL_LR(tst, (Addr) &frame->tramp[0]);

   VG_TRACK(post_mem_write, Vg_CoreSignal, tst->tid,
            (Addr)&frame->tramp, sizeof(frame->tramp));

   if (0) {
      VG_(printf)("pushed signal frame for sig %d; R1 now = %#lx, "
                  "next %%CIA = %#llx, status=%d\n", 
                  sigNo,
	          sp, tst->arch.vex.guest_CIA, tst->status);
      VG_(printf)("trampoline is at %p\n",  &frame->tramp[0]);
   }
}


/* Remove a signal frame from thread 'tid's stack, and restore the CPU
   state from it.  Note, isRT is irrelevant here. */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   ThreadState *tst;
   Addr sp, sp_max;
   const UWord one_meg = 1048576;
   UWord scannable_bytes;

   Int sigNo, i;
   struct hacky_sigframe* frame;
 
   vg_assert(VG_(is_valid_tid)(tid));
   tst = VG_(get_ThreadState)(tid);

   /* Check that the stack frame looks valid */
   sp = tst->arch.vex.guest_GPR1;
   vg_assert(VG_IS_16_ALIGNED(sp));

   /* If the frame is being cleared by some mechanism other than our
      fake sigreturn, sp may not be as it was after the frame was
      constructed.  If so, scan back up the stack looking for the most
      recently pushed frame and assume that's the right one to use.
      Urk. */
   sp_max = tst->client_stack_highest_word;
   scannable_bytes = 0;
   if (sp_max > sp)
      scannable_bytes = sp_max - sp;
   if (scannable_bytes > one_meg)
      scannable_bytes = one_meg;
   if (scannable_bytes < (sizeof(struct hacky_sigframe)-512)) 
      scannable_bytes = 0;
   else
      scannable_bytes -= (sizeof(struct hacky_sigframe)-512);

   vg_assert(scannable_bytes <= one_meg);

   frame = (struct hacky_sigframe*)(sp - 512);
   if (frame->magicPI != 0x31415927) {
      if (!VG_(clo_xml))
         VG_(message)(Vg_DebugMsg, 
            "WARNING: dubious signal return: searching %ld bytes for frame\n", 
            scannable_bytes);
      for (i = 0; i < scannable_bytes/4; i++) {
         if (frame->magicPI == 0x31415927)
            break;
         frame = (struct hacky_sigframe*)(((Addr)frame)+4);
      }
   }

   /* If we haven't found the frame by now, we're hosed. */
   vg_assert(frame->magicPI == 0x31415927);

   /* restore the entire guest state, and shadow, from the
      frame.  Note, as per comments above, this is a kludge - should
      restore it from saved ucontext.  Oh well. */
   tst->arch.vex = frame->gst;
   tst->arch.vex_shadow1 = frame->gshadow1;
   tst->arch.vex_shadow2 = frame->gshadow2;
   sigNo = frame->sigNo_private;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "vg_pop_signal_frame (thread %d): valid magic; CIA=%#llx\n",
                   tid, tst->arch.vex.guest_CIA);

   VG_TRACK( die_mem_stack_signal, 
             (Addr)frame, 
             sizeof(struct hacky_sigframe) - VG_STACK_REDZONE_SZB );

   /* tell the tools */
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif // defined(VGP_ppc64_aix5)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
