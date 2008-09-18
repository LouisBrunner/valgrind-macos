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


#include "drd_error.h"
#include "drd_segment.h"
#include "drd_thread.h"
#include "pub_tool_basics.h"      // Addr, SizeT
#include "pub_tool_errormgr.h"    // VG_(unique_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strlen)()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"     // VG_(get_SP)()
#include "pub_tool_mallocfree.h"  // VG_(malloc)(), VG_(free)()
#include "pub_tool_threadstate.h" // VG_INVALID_THREADID


// Local variables.

static ULong s_created_segments_count;
static ULong s_alive_segments_count;
static ULong s_max_alive_segments_count;
static Bool drd_trace_segment = False;


// Function definitions.

/** Initialize the memory pointed at by sg.
 *  @note The creator and created thread ID's may be equal.
 */
static
void sg_init(Segment* const sg,
             DrdThreadId const creator,
             DrdThreadId const created)
{
  Segment* creator_sg;
  ThreadId vg_created = DrdThreadIdToVgThreadId(created);

  tl_assert(sg);
  tl_assert(creator == DRD_INVALID_THREADID || IsValidDrdThreadId(creator));

  creator_sg = (creator != DRD_INVALID_THREADID
                ? thread_get_segment(creator) : 0);

  sg->next = 0;
  sg->prev = 0;
  sg->refcnt = 1;

  if (vg_created != VG_INVALID_THREADID && VG_(get_SP)(vg_created) != 0)
    sg->stacktrace = VG_(record_ExeContext)(vg_created, 0);
  else
    sg->stacktrace = 0;

  if (creator_sg)
    vc_copy(&sg->vc, &creator_sg->vc);
  else
    vc_init(&sg->vc, 0, 0);
  vc_increment(&sg->vc, created);
  sg->bm = bm_new();

  if (drd_trace_segment)
  {
    char msg[256];
    VG_(snprintf)(msg, sizeof(msg),
                  "New segment for thread %d/%d with vc ",
                  created != VG_INVALID_THREADID
                  ? DrdThreadIdToVgThreadId(created)
                  : DRD_INVALID_THREADID,
                  created);
    vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
               &sg->vc);
    VG_(message)(Vg_UserMsg, "%s", msg);
  }
}

/** Deallocate the memory that was allocated by sg_init(). */
static
void sg_cleanup(Segment* const sg)
{
  tl_assert(sg);
  tl_assert(sg->refcnt == 0);

  vc_cleanup(&sg->vc);
  bm_delete(sg->bm);
  sg->bm = 0;
}

/** Allocate and initialize a new segment. */
Segment* sg_new(ThreadId const creator, ThreadId const created)
{
  Segment* sg;

  s_created_segments_count++;
  s_alive_segments_count++;
  if (s_max_alive_segments_count < s_alive_segments_count)
    s_max_alive_segments_count = s_alive_segments_count;

  sg = VG_(malloc)("drd.segment.sn.1", sizeof(*sg));
  tl_assert(sg);
  sg_init(sg, creator, created);
  return sg;
}

static
void sg_delete(Segment* const sg)
{
#if 1
  if (sg_get_trace())
  {
    char msg[256];
    VG_(snprintf)(msg, sizeof(msg),
                  "Discarding the segment with vector clock ");
    vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
               &sg->vc);
    VG_(message)(Vg_UserMsg, "%s", msg);
  }
#endif

  s_alive_segments_count--;

  tl_assert(sg);
  sg_cleanup(sg);
  VG_(free)(sg);
}

/** Query the reference count of the specified segment. */
int sg_get_refcnt(const Segment* const sg)
{
  tl_assert(sg);

  return sg->refcnt;
}

/** Increment the reference count of the specified segment. */
Segment* sg_get(Segment* const sg)
{
  tl_assert(sg);

  sg->refcnt++;
  return sg;
}

/** Decrement the reference count of the specified segment and deallocate the
 *  segment if the reference count became zero.
 */
void sg_put(Segment* const sg)
{
  if (sg == 0)
    return;

  if (drd_trace_segment)
  {
    char msg[256];
    VG_(snprintf)(msg, sizeof(msg),
                  "Decrementing segment reference count %d -> %d with vc ",
                  sg->refcnt, sg->refcnt - 1);
    vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
               &sg->vc);
    VG_(message)(Vg_UserMsg, "%s", msg);
  }

  tl_assert(sg->refcnt >= 1);

  if (--sg->refcnt == 0)
  {
    sg_delete(sg);
  }
}

/** Merge sg1 and sg2 into sg1. */
void sg_merge(const Segment* const sg1, Segment* const sg2)
{
  tl_assert(sg1);
  tl_assert(sg1->refcnt == 1);
  tl_assert(sg2);
  tl_assert(sg2->refcnt == 1);

  if (drd_trace_segment)
  {
      char msg[256];

      VG_(snprintf)(msg, sizeof(msg), "Merging segments with vector clocks ");
      vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                 &sg1->vc);
      VG_(snprintf)(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                    " and ");
      vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                 &sg2->vc);
      VG_(message)(Vg_UserMsg, "%s", msg);
  }

  // Keep sg1->stacktrace.
  // Keep sg1->vc.
  // Merge sg2->bm into sg1->bm.
  bm_merge2(sg1->bm, sg2->bm);
}

void sg_print(const Segment* const sg)
{
  tl_assert(sg);
  VG_(printf)("vc: ");
  vc_print(&sg->vc);
  VG_(printf)("\n");
  bm_print(sg->bm);
}

Bool sg_get_trace(void)
{
  return drd_trace_segment;
}

void sg_set_trace(Bool const trace_segment)
{
  tl_assert(trace_segment == False || trace_segment == True);
  drd_trace_segment = trace_segment;
}

ULong sg_get_created_segments_count(void)
{
  return s_created_segments_count;
}

ULong sg_get_alive_segments_count(void)
{
  return s_alive_segments_count;
}

ULong sg_get_max_alive_segments_count(void)
{
  return s_max_alive_segments_count;
}
