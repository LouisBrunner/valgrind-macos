/* -*- mode: C; c-basic-offset: 3; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2010 Bart Van Assche <bvanassche@acm.org>.

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


// Reader-writer lock state information.


#ifndef __DRD_RWLOCK_H
#define __DRD_RWLOCK_H


#include "drd_clientobj.h"        // struct rwlock_info
#include "drd_thread.h"           // DrdThreadId
#include "pub_tool_basics.h"      // Addr


struct rwlock_info;


void DRD_(rwlock_set_trace)(const Bool trace_rwlock);
void DRD_(rwlock_set_exclusive_threshold)(const UInt exclusive_threshold_ms);
void DRD_(rwlock_set_shared_threshold)(const UInt shared_threshold_ms);
struct rwlock_info* DRD_(rwlock_pre_init)(const Addr rwlock,
                                          const RwLockT rwlock_type);
void DRD_(rwlock_post_destroy)(const Addr rwlock, const RwLockT rwlock_type);
void DRD_(rwlock_pre_rdlock)(const Addr rwlock, const RwLockT rwlock_type);
void DRD_(rwlock_post_rdlock)(const Addr rwlock, const RwLockT rwlock_type,
                              const Bool took_lock);
void DRD_(rwlock_pre_wrlock)(const Addr rwlock, const RwLockT rwlock_type);
void DRD_(rwlock_post_wrlock)(const Addr rwlock, const RwLockT rwlock_type,
                              const Bool took_lock);
void DRD_(rwlock_pre_unlock)(const Addr rwlock, const RwLockT rwlock_type);
ULong DRD_(get_rwlock_segment_creation_count)(void);


#endif /* __DRD_RWLOCK_H */
