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


#ifndef __SEGMENT_H
#define __SEGMENT_H


/*
 * Segments and segment lists. A segment represents information about
 * a contiguous group of statements of a specific thread. There is a vector
 * clock associated with each segment.
 */


#include "drd_vc.h"
#include "pub_drd_bitmap.h"
#include "pub_tool_execontext.h" // ExeContext
#include "pub_tool_stacktrace.h" // StackTrace


typedef struct segment
{
   struct segment*    g_next;
   struct segment*    g_prev;
   /** Pointers to next and previous segments executed by the same thread. */
   struct segment*    thr_next;
   struct segment*    thr_prev;
   DrdThreadId        tid;
   /** Reference count: number of pointers that point to this segment. */
   int                refcnt;
   /** Stack trace of the first instruction of the segment. */
   ExeContext*        stacktrace;
   /** Vector clock associated with the segment. */
   VectorClock        vc;
   /**
    * Bitmap representing the memory accesses by the instructions associated
    * with the segment.
    */
   struct bitmap      bm;
} Segment;

extern Segment* DRD_(g_sg_list);

Segment* DRD_(sg_new)(const DrdThreadId creator, const DrdThreadId created);
static int DRD_(sg_get_refcnt)(const Segment* const sg);
Segment* DRD_(sg_get)(Segment* const sg);
void DRD_(sg_put)(Segment* const sg);
static struct bitmap* DRD_(sg_bm)(Segment* const sg);
void DRD_(sg_merge)(Segment* const sg1, Segment* const sg2);
void DRD_(sg_print)(Segment* const sg);
Bool DRD_(sg_get_trace)(void);
void DRD_(sg_set_trace)(const Bool trace_segment);
ULong DRD_(sg_get_segments_created_count)(void);
ULong DRD_(sg_get_segments_alive_count)(void);
ULong DRD_(sg_get_max_segments_alive_count)(void);
ULong DRD_(sg_get_segment_merge_count)(void);


/** Query the reference count of the specified segment. */
static __inline__ int DRD_(sg_get_refcnt)(const Segment* const sg)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(sg);
#endif

   return sg->refcnt;
}

/** Return the pointer to the bitmap of the segment. */
static __inline__ struct bitmap* DRD_(sg_bm)(Segment* const sg)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   tl_assert(sg);
#endif

   return &sg->bm;
}



#endif // __SEGMENT_H
