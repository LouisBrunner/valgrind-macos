/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2020 Bart Van Assche <bvanassche@acm.org>.

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


#ifndef __DRD_MUTEX_H
#define __DRD_MUTEX_H


#include "drd_clientreq.h"        // MutexT
#include "drd_thread.h"           // DrdThreadId
#include "pub_tool_basics.h"      // Addr


struct mutex_info;


void DRD_(mutex_set_trace)(const Bool trace_mutex);
void DRD_(mutex_set_lock_threshold)(const UInt lock_threshold_ms);
struct mutex_info* DRD_(mutex_init)(const Addr mutex, const MutexT mutex_type);
void DRD_(mutex_ignore_ordering)(const Addr mutex);
void DRD_(mutex_post_destroy)(const Addr mutex);
void DRD_(not_a_mutex)(const Addr mutex);
struct mutex_info* DRD_(mutex_get)(const Addr mutex);
void DRD_(mutex_pre_lock)(const Addr mutex, const MutexT mutex_type,
                          const Bool trylock);
void DRD_(mutex_post_lock)(const Addr mutex, const Bool took_lock,
                           const Bool post_cond_wait);
void DRD_(mutex_unlock)(const Addr mutex, const MutexT mutex_type);
void DRD_(spinlock_init_or_unlock)(const Addr spinlock);
const HChar* DRD_(mutex_get_typename)(struct mutex_info* const p);
const HChar* DRD_(mutex_type_name)(const MutexT mt);
Bool DRD_(mutex_is_locked_by)(const Addr mutex, const DrdThreadId tid);
int DRD_(mutex_get_recursion_count)(const Addr mutex);
ULong DRD_(get_mutex_lock_count)(void);
ULong DRD_(get_mutex_segment_creation_count)(void);


#endif /* __DRD_MUTEX_H */
