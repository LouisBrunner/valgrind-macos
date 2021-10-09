/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Signal frames handling: stuff common to most/all platforms   ---*/
/*---                                                              ---*/
/*---                                            sigframe-common.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2006-2017 OpenWorks Ltd
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_aspacemgr.h"      // VG_(am_find_nsegment)
#include "pub_core_signals.h"        // VG_(extend_stack)
#include "pub_core_libcprint.h"      // VG_(umsg)
#include "pub_core_tooliface.h"      // VG_TRACK
#include "pub_core_machine.h"        // VG_STACK_REDZONE_SZB
#include "priv_sigframe.h"           // self


/* For tracking memory events, indicate the entire frame has been
   allocated.  Except, don't mess with the area which
   overlaps the previous frame's redzone. */
static void track_frame_memory ( Addr addr, SizeT size, ThreadId tid )
{
   VG_TRACK( new_mem_stack_signal, addr - VG_STACK_REDZONE_SZB, size, tid );
}

#if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)

/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful. */
Bool ML_(sf_maybe_extend_stack) ( const ThreadState *tst, Addr addr,
                                  SizeT size, UInt flags )
{
   ThreadId        tid = tst->tid;
   const NSegment *stackseg = NULL;

   if (flags & VKI_SA_ONSTACK) {
      /* If the sigframe is allocated on an alternate stack, then we cannot
         extend that stack. Nothing to do here. */
      stackseg = VG_(am_find_nsegment)(addr);
   } else if (VG_(am_addr_is_in_extensible_client_stack)(addr)) {
      if (VG_(extend_stack)(tid, addr)) {
         stackseg = VG_(am_find_nsegment)(addr);
         if (0 && stackseg)
            VG_(printf)("frame=%#lx seg=%#lx-%#lx\n",
                        addr, stackseg->start, stackseg->end);
      }
   } else if ((stackseg = VG_(am_find_nsegment)(addr)) &&
              VG_(am_is_valid_for_client)(addr, 1,
                                          VKI_PROT_READ | VKI_PROT_WRITE)) {
      /* We come here for explicitly defined pthread-stacks which can be
         located in any client segment. */
   } else {
      /* Something unexpected */
      stackseg = NULL;
   }

   if (stackseg == NULL || !stackseg->hasR || !stackseg->hasW) {
      VG_(umsg)("Can't extend stack to %#lx during signal delivery for "
                "thread %u:\n", addr, tid);
      if (stackseg == NULL)
         VG_(umsg)("  no stack segment\n");
      else
         VG_(umsg)("  too small or bad protection modes\n");

      /* set SIGSEGV to default handler */
      VG_(set_default_handler)(VKI_SIGSEGV);
      VG_(synth_fault_mapping)(tid, addr);

      /* The whole process should be about to die, since the default
	 action of SIGSEGV to kill the whole process. */
      return False;
   }

   /* Tell the tool about the new memory */
   track_frame_memory(addr, size, tid);

   return True;
}

#elif defined(VGO_darwin)

/* Extend the stack segment downwards if needed so as to ensure the
   new signal frames are mapped to something.  Return a Bool
   indicating whether or not the operation was successful. */
Bool ML_(sf_maybe_extend_stack) ( const ThreadState *tst, Addr addr,
                                  SizeT size, UInt flags )
{
   /* Tell the tool about the new memory */
   track_frame_memory(addr, size, tst->tid);
   return True;
}

#else
#error unknown OS
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
