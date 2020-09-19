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


/* Barrier state information. */


#ifndef __DRD_BARRIER_H
#define __DRD_BARRIER_H


#include "drd_basics.h"       // DrdThreadId
#include "drd_clientreq.h"    // BarrierT
#include "pub_tool_basics.h"  // Addr


struct barrier_info;


void DRD_(barrier_set_trace)(const Bool trace_barrier);
void DRD_(barrier_init)(const Addr barrier,
                        const BarrierT barrier_type, const Word count,
                        const Bool reinitialization);
void DRD_(barrier_destroy)(const Addr barrier, const BarrierT barrier_type);
void DRD_(barrier_pre_wait)(const DrdThreadId tid, const Addr barrier,
                            const BarrierT barrier_type);
void DRD_(barrier_post_wait)(const DrdThreadId tid, const Addr barrier,
                             const BarrierT barrier_type, const Bool waited,
                             const Bool serializing);
void DRD_(barrier_stop_using_mem)(const Addr a1, const Addr a2);
ULong DRD_(get_barrier_segment_creation_count)(void);


#endif /* __DRD_BARRIER_H */
