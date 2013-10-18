
/*--------------------------------------------------------------------*/
/*--- System call wrappers, etc.                pub_core_syswrap.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward
      jseward@acm.org

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

#ifndef __PUB_CORE_SYSWRAP_H
#define __PUB_CORE_SYSWRAP_H

#include "pub_core_basics.h"        // VG_ macro
#include "pub_core_threadstate.h"   // ThreadArchState

//--------------------------------------------------------------------
// PURPOSE: This module contains all the syscall junk:  mostly PRE/POST
// wrappers, but also the main syscall jacketing code.
//--------------------------------------------------------------------

// Allocates a stack for the first thread, then runs it,
// as if the thread had been set up by clone()
extern void VG_(main_thread_wrapper_NORETURN)(ThreadId tid);

extern void VG_(client_syscall) ( ThreadId tid, UInt trc );

extern void VG_(post_syscall)   ( ThreadId tid );

/* Clear this module's private state for thread 'tid' */
extern void VG_(clear_syscallInfo) ( Int tid );

// Fix up a thread's state when syscall is interrupted by a signal.
extern void VG_(fixup_guest_state_after_syscall_interrupted)(
               ThreadId tid,
               Addr     ip, 
               SysRes   sysret,
               Bool     restart
            );

// Wait until all other threads are dead
extern void VG_(reap_threads)(ThreadId self);

// Release resources held by this thread
extern void VG_(cleanup_thread) ( ThreadArchState* );

/* fd leakage calls. */
extern void VG_(init_preopened_fds) ( void );
extern void VG_(show_open_fds) ( const HChar* when );

// When the final thread is done, where shall I call to shutdown the
// system cleanly?  Is set once at startup (in m_main) and never
// changes after that.  Is basically a pointer to the exit
// continuation.  This is all just a nasty hack to avoid calling
// directly from m_syswrap to m_main at exit, since that would cause
// m_main to become part of a module cycle, which is silly.
extern void (* VG_(address_of_m_main_shutdown_actions_NORETURN) )
            (ThreadId,VgSchedReturnCode);

#endif   // __PUB_CORE_SYSWRAP_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

