/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2008 Bart Van Assche
  bart.vanassche@gmail.com

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


#ifndef __DRD_MUTEX_H
#define __DRD_MUTEX_H


#include "drd_clientreq.h"        // MutexT
#include "drd_thread.h"           // DrdThreadId
#include "drd_vc.h"
#include "pub_tool_basics.h"      // Addr


struct mutex_info;


void mutex_set_trace(const Bool trace_mutex);
void mutex_set_lock_threshold(const UInt lock_threshold_ms);
struct mutex_info* mutex_init(const Addr mutex,
                              const MutexT mutex_type);
void mutex_post_destroy(const Addr mutex);
void not_a_mutex(const Addr mutex);
struct mutex_info* mutex_get(const Addr mutex);
void mutex_pre_lock(const Addr mutex, const MutexT mutex_type,
                    const Bool trylock);
void mutex_post_lock(const Addr mutex, const Bool took_lock,
                     const Bool post_cond_wait);
void mutex_unlock(const Addr mutex, const MutexT mutex_type);
const char* mutex_get_typename(struct mutex_info* const p);
const char* mutex_type_name(const MutexT mt);
Bool mutex_is_locked_by(const Addr mutex, const DrdThreadId tid);
int mutex_get_recursion_count(const Addr mutex);
void mutex_thread_delete(const DrdThreadId tid);
ULong get_mutex_lock_count(void);
ULong get_mutex_segment_creation_count(void);


#endif /* __DRD_MUTEX_H */
