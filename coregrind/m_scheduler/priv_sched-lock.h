
/*--------------------------------------------------------------------*/
/*--- Private scheduler lock header.             priv_sched-lock.h ---*/
/*---                                                              ---*/
/*--- Scheduler lock API.                                          ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Bart Van Assche <bvanassche@acm.org>.

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

#ifndef __PRIV_SCHED_LOCK_H
#define __PRIV_SCHED_LOCK_H

#include "pub_core_basics.h"   // Bool

struct sched_lock;

enum SchedLockType { sched_lock_generic, sched_lock_ticket };

Bool ML_(set_sched_lock_impl)(const enum SchedLockType t);
const HChar *ML_(get_sched_lock_name)(void);
struct sched_lock *ML_(create_sched_lock)(void);
void ML_(destroy_sched_lock)(struct sched_lock *p);
int ML_(get_sched_lock_owner)(struct sched_lock *p);
void ML_(acquire_sched_lock)(struct sched_lock *p);
void ML_(release_sched_lock)(struct sched_lock *p);

#endif   // __PRIV_SCHED_LOCK_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
