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


// Barrier state information.


#ifndef __DRD_BARRIER_H
#define __DRD_BARRIER_H


#include "drd_thread.h"           // DrdThreadId
#include "drd_vc.h"
#include "pub_tool_basics.h"      // Addr, SizeT


struct barrier_info;


void barrier_set_trace(const Bool trace_barrier);
struct barrier_info* barrier_init(const Addr barrier, const SizeT size,
                                  const Word count);
void barrier_destroy(const Addr barrier);
void barrier_pre_wait(const DrdThreadId tid, const Addr barrier);
void barrier_post_wait(const DrdThreadId tid, const Addr barrier,
                       const Bool waited);
void barrier_thread_delete(const DrdThreadId threadid);
void barrier_stop_using_mem(const Addr a1, const Addr a2);


#endif /* __DRD_BARRIER_H */
