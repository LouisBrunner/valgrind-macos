
/*--------------------------------------------------------------------*/
/*--- System call wrappers, etc.               pub_core_syscalls.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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

#ifndef __PUB_CORE_SYSCALLS_H
#define __PUB_CORE_SYSCALLS_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the syscall junk:  mostly wrappers,
// but also the code that executes them, and related stuff.
//--------------------------------------------------------------------

// Return how many bytes of a thread's Valgrind stack are unused
extern SSizeT VGA_(stack_unused)(ThreadId tid);

// Allocates a stack for the first thread, then runs it,
// as if the thread had been set up by clone()
extern void VGP_(main_thread_wrapper_NORETURN)(ThreadId tid);

extern HChar* VG_(resolve_filename_nodup)(Int fd);
extern HChar* VG_(resolve_filename)(Int fd);

extern void VG_(client_syscall) ( ThreadId tid );

extern void VG_(post_syscall)   ( ThreadId tid );

/* Clear this module's private state for thread 'tid' */
extern void VG_(clear_syscallInfo) ( Int tid );

// Fix up a thread's state when syscall is interrupted by a signal.
extern void VG_(fixup_guest_state_after_syscall_interrupted)(
               ThreadId tid,
               Addr     ip, 
               UWord    sysnum,
               SysRes   sysret,
               Bool     restart
            );

// Wait until all other threads are dead
extern void VG_(reap_threads)(ThreadId self);

// Release resources held by this thread
extern void VGP_(cleanup_thread) ( ThreadArchState* );

/* fd leakage calls. */
extern void VG_(init_preopened_fds) ( void );
extern void VG_(show_open_fds) ( void );

#endif   // __PUB_CORE_SYSCALLS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

