
/*--------------------------------------------------------------------*/
/*--- Private exports of syswrap-main.c.       priv_syswrap-main.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Nicholas Nethercote
      njn@valgrind.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PRIV_SYSWRAP_MAIN_H
#define __PRIV_SYSWRAP_MAIN_H

#include "pub_core_basics.h"        // ThreadID
#include "pub_core_threadstate.h"   // ThreadArchState

/* Back up a thread so as to restart a system call. */
extern
void ML_(fixup_guest_state_to_restart_syscall) ( ThreadArchState* arch );

extern
void VG_(sanitize_client_sigmask)(vki_sigset_t *mask);

#if defined(VGO_darwin)
/* Longjmp to scheduler after client calls workq_ops(WQOPS_THREAD_RETURN)*/
extern
void ML_(wqthread_continue_NORETURN)(ThreadId tid);
#endif

#endif   // __PRIV_SYSWRAP_MAIN_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

