
/*--------------------------------------------------------------------*/
/*--- Private exports of syswrap-main.c.       priv_syswrap-main.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Nicholas Nethercote
      njn@valgrind.org

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

#ifndef __PRIV_SYSWRAP_MAIN_H
#define __PRIV_SYSWRAP_MAIN_H

/* Back up a thread so as to restart a system call. */
extern
void ML_(fixup_guest_state_to_restart_syscall) ( ThreadArchState* arch );

#if defined(VGO_darwin)
/* Longjmp to scheduler after client calls workq_ops(WQOPS_THREAD_RETURN)*/
extern
void ML_(wqthread_continue_NORETURN)(ThreadId tid);
#endif

#endif   // __PRIV_SYSWRAP_MAIN_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

