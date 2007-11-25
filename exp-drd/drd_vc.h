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


#ifndef __DRD_VC_H
#define __DRD_VC_H


// DRD vector clock implementation:
// - One counter per thread.
// - A vector clock is implemented as multiple pairs of (thread id, counter).
// - Pairs are stored in an array sorted by thread id.
// Semantics:
// - Each time a thread performs an action that implies an ordering between
//   intra-thread events, the counter of that thread is incremented.
// - Vector clocks are compared by comparing all counters of all threads.
// - When a thread synchronization action is performed that guarantees that
//   new actions of the current thread are executed after the actions of the
//   other thread, the vector clock of the synchronization object and the 
//   current thread are combined (by taking the component-wise maximum).
// - A vector clock is incremented during actions such as
//   pthread_create(), pthread_mutex_unlock(), sem_post(). (Actions where
//   an inter-thread ordering "arrow" starts).


#include "pub_tool_basics.h"    // Addr, SizeT


typedef struct
{
  ThreadId threadid;
  UInt count;
} VCElem;

typedef struct
{
  unsigned       capacity;
  unsigned       size;
  VCElem* vc;
} VectorClock;


void vc_init(VectorClock* const vc,
             const VCElem* const vcelem,
             const unsigned size);
void vc_cleanup(VectorClock* const vc);
void vc_copy(VectorClock* const new,
             const VectorClock* const rhs);
void vc_increment(VectorClock* const vc, ThreadId const threadid);
Bool vc_lte(const VectorClock* const vc1,
            const VectorClock* const vc2);
Bool vc_ordered(const VectorClock* const vc1,
                const VectorClock* const vc2);
void vc_min(VectorClock* const result,
            const VectorClock* const rhs);
void vc_combine(VectorClock* const result,
                const VectorClock* const rhs);
void vc_print(const VectorClock* const vc);
void vc_snprint(Char* const str, Int const size,
                const VectorClock* const vc);
void vc_check(const VectorClock* const vc);
void vc_test(void);


#endif /* __DRD_VC_H */
