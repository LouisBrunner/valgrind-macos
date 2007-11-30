/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2007 Bart Van Assche
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


// Condition variable state information: mutex specified in pthread_cond_wait()
// call.


#ifndef __COND_H
#define __COND_H


#include "pub_tool_basics.h"      // Addr, SizeT
#include "drd_vc.h"
#include "drd_thread.h"           // DrdThreadId


struct cond_info
{
  Addr  cond;  // Pointer to client condition variable.
  SizeT size;  // sizeof(pthread_cond_t)
  int   waiter_count;
  Addr  mutex; // Client mutex specified in pthread_cond_wait() call, and null
              // if no client threads are currently waiting on this cond.var.
};

void cond_set_trace(const Bool trace_cond);
void cond_init(const Addr cond, const SizeT size);
void cond_destroy(struct cond_info* const p);
struct cond_info* cond_get(Addr const mutex);
int cond_pre_wait(const Addr cond, const SizeT cond_size, const Addr mutex);
int cond_post_wait(const Addr cond);
void cond_pre_signal(Addr const cond);
void cond_pre_broadcast(Addr const cond);
void cond_stop_using_mem(const Addr a1, const Addr a2);


#endif /* __COND_H */
