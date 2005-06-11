
/*--------------------------------------------------------------------*/
/*--- Thread modelling.                     pub_core_threadmodel.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005 Jeremy Fitzhardinge
      jeremy@goop.org

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

#ifndef __PUB_CORE_THREADMODEL_H
#define __PUB_CORE_THREADMODEL_H

//--------------------------------------------------------------------
// PURPOSE: This module does thread modelling stuff...
//--------------------------------------------------------------------

//extern void VG_(tm_thread_create)  (ThreadId creator, ThreadId tid, Bool detached);
//extern void VG_(tm_thread_exit)    (ThreadId tid);
//extern Bool VG_(tm_thread_exists)  (ThreadId tid);
//extern void VG_(tm_thread_detach)  (ThreadId tid);
//extern void VG_(tm_thread_join)    (ThreadId joiner, ThreadId joinee);
//extern void VG_(tm_thread_switchto)(ThreadId tid);
//
//extern void VG_(tm_mutex_init)   (ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_destroy)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_trylock)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_giveup) (ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_acquire)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_tryunlock)(ThreadId tid, Addr mutexp);
//extern void VG_(tm_mutex_unlock) (ThreadId tid, Addr mutexp);
//extern Bool VG_(tm_mutex_exists) (Addr mutexp);
//
//extern UInt VG_(tm_error_update_extra) (Error *err);
//extern Bool VG_(tm_error_equal) (VgRes res, Error *e1, Error *e2);
//extern void VG_(tm_error_print) (Error *err);
//
//extern void VG_(tm_init) ();
//
//extern void VG_(tm_cond_init)    (ThreadId tid, Addr condp);
//extern void VG_(tm_cond_destroy) (ThreadId tid, Addr condp);
//extern void VG_(tm_cond_wait)    (ThreadId tid, Addr condp, Addr mutexp);
//extern void VG_(tm_cond_wakeup)  (ThreadId tid, Addr condp, Addr mutexp);
//extern void VG_(tm_cond_signal)  (ThreadId tid, Addr condp);

#endif   // __PUB_CORE_THREADMODEL_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
