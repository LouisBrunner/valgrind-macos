
/*--------------------------------------------------------------------*/
/*--- Private scheduler lock header.        priv_sched-lock-impl.h ---*/
/*---                                                              ---*/
/*--- Scheduler lock implementation details.                       ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Bart Van Assche <bvanassche@acm.org>.

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

#ifndef __PRIV_SCHED_LOCK_IMPL_H
#define __PRIV_SCHED_LOCK_IMPL_H

#include "pub_core_basics.h"    // HChar

struct sched_lock_ops {
   const HChar *(*get_sched_lock_name)(void);
   struct sched_lock *(*create_sched_lock)(void);
   void (*destroy_sched_lock)(struct sched_lock *p);
   int (*get_sched_lock_owner)(struct sched_lock *p);
   void (*acquire_sched_lock)(struct sched_lock *p);
   void (*release_sched_lock)(struct sched_lock *p);
};

extern const struct sched_lock_ops ML_(generic_sched_lock_ops);
extern const struct sched_lock_ops ML_(linux_ticket_lock_ops);

#endif   // __PRIV_SCHED_LOCK_IMPL_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
