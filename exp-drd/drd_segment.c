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


#include "drd_error.h"
#include "drd_segment.h"
#include "drd_thread.h"
#include "pub_tool_basics.h"      // Addr, SizeT
#include "pub_tool_errormgr.h"    // VG_(unique_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strlen)()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_mallocfree.h"  // VG_(malloc)(), VG_(free)()
#include "pub_tool_threadstate.h" // VG_INVALID_THREADID


// Local variables.

static ULong s_segments_created_count;
static ULong s_segments_alive_count;
static ULong s_max_segments_alive_count;
static Bool drd_trace_segment = False;


// Function definitions.

/**
 * Note: creator and created may be equal.
 */
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

  if (vg_created != VG_INVALID_THREADID)
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
                  "New segment for thread %d with vc ",
                  creator);
    vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
               &sg->vc);
    VG_(message)(Vg_DebugMsg, "%s", msg);
  }
}

void sg_cleanup(Segment* const sg)
{
  tl_assert(sg);
  vc_cleanup(&sg->vc);
  bm_delete(sg->bm);
  sg->bm = 0;
}

Segment* sg_new(ThreadId const creator, ThreadId const created)
{
  Segment* sg;

  s_segments_created_count++;
  s_segments_alive_count++;
  if (s_max_segments_alive_count < s_segments_alive_count)
    s_max_segments_alive_count = s_segments_alive_count;

  sg = VG_(malloc)(sizeof(*sg));
  tl_assert(sg);
  sg_init(sg, creator, created);
  return sg;
}

void sg_delete(Segment* const sg)
{
  s_segments_alive_count--;

  tl_assert(sg);
  sg_cleanup(sg);
  VG_(free)(sg);
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

ULong sg_get_segments_created_count(void)
{
  return s_segments_created_count;
}

ULong sg_get_max_segments_alive_count(void)
{
  return s_max_segments_alive_count;
}
