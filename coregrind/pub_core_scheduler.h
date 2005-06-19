
/*--------------------------------------------------------------------*/
/*--- The scheduler.                          pub_core_scheduler.h ---*/
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

#ifndef __PUB_CORE_SCHEDULER_H
#define __PUB_CORE_SCHEDULER_H

//--------------------------------------------------------------------
// PURPOSE: This module is the scheduler, which is the main loop
// controlling the running of all the program's threads.
// It's at the centre of everything.
//--------------------------------------------------------------------

/* Allocate a new ThreadState */
extern ThreadId VG_(alloc_ThreadState)(void);

/* A thread exits.  tid must currently be running. */
extern void VG_(exit_thread)(ThreadId tid);

/* Kill a thread.  This interrupts whatever a thread is doing, and
   makes it exit ASAP.  This does not set the exitreason or
   exitcode. */
extern void VG_(kill_thread)(ThreadId tid);

/* Nuke all threads except tid. */
extern void VG_(nuke_all_threads_except) ( ThreadId me,
                                           VgSchedReturnCode reason );

/* Make a thread the running thread.  The thread must previously been
   sleeping, and not holding the CPU semaphore. This will set the
   thread state to VgTs_Runnable, and the thread will attempt to take
   the CPU semaphore.  By the time it returns, tid will be the running
   thread. */
extern void VG_(set_running) ( ThreadId tid );

/* Set a thread into a sleeping state.  Before the call, the thread
   must be runnable, and holding the CPU semaphore.  When this call
   returns, the thread will be set to the specified sleeping state,
   and will not be holding the CPU semaphore.  Note that another
   thread could be running by the time this call returns, so the
   caller must be careful not to touch any shared state.  It is also
   the caller's responsibility to actually block until the thread is
   ready to run again. */
extern void VG_(set_sleeping) ( ThreadId tid, ThreadStatus state );

/* Yield the CPU for a while */
extern void VG_(vg_yield)(void);

// The scheduler.
extern VgSchedReturnCode VG_(scheduler) ( ThreadId tid );

extern void VG_(scheduler_init) ( void );

/* Stats ... */
extern void VG_(print_scheduler_stats) ( void );

// Longjmp back to the scheduler and thus enter the sighandler immediately.
extern void VG_(resume_scheduler) ( ThreadId tid );

/* If true, a fault is Valgrind-internal (ie, a bug) */
extern Bool VG_(my_fault);

/* Sanity checks which may be done at any time.  The scheduler decides when. */
extern void VG_(sanity_check_general) ( Bool force_expensive );

#endif   // __PUB_CORE_SCHEDULER_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
