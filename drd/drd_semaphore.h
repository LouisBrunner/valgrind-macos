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


// Semaphore state information: owner thread and recursion count.


#ifndef __DRD_SEMAPHORE_H
#define __DRD_SEMAPHORE_H


#include "drd_thread.h"           // DrdThreadId
#include "pub_tool_basics.h"      // Addr


struct semaphore_info;


void DRD_(semaphore_set_trace)(const Bool trace_semaphore);
struct semaphore_info* DRD_(semaphore_init)(const Addr semaphore,
                                            const Word pshared,
                                            const UInt value);
void DRD_(semaphore_destroy)(const Addr semaphore);
struct semaphore_info* DRD_(semaphore_open)(const Addr semaphore,
                                            const Char* name, const Word oflag,
                                            const Word mode, const UInt value);
void DRD_(semaphore_close)(const Addr semaphore);
void DRD_(semaphore_pre_wait)(const Addr semaphore);
void DRD_(semaphore_post_wait)(const DrdThreadId tid, const Addr semaphore,
                               const Bool waited);
void DRD_(semaphore_pre_post)(const DrdThreadId tid, const Addr semaphore);
void DRD_(semaphore_post_post)(const DrdThreadId tid, const Addr semaphore,
                               const Bool waited);
ULong DRD_(get_semaphore_segment_creation_count)(void);


#endif /* __DRD_SEMAPHORE_H */
