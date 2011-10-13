/* -*- mode: C; c-basic-offset: 3; indent-tabs-mode: nil; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2011 Bart Van Assche <bvanassche@acm.org>.

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


#include "drd_clientobj.h"
#include "drd_hb.h"
#include "drd_error.h"
#include "pub_tool_errormgr.h"    /* VG_(maybe_record_error)() */
#include "pub_tool_libcassert.h"  /* tl_assert()               */
#include "pub_tool_libcprint.h"   /* VG_(printf)()             */
#include "pub_tool_machine.h"     /* VG_(get_IP)()             */
#include "pub_tool_mallocfree.h"  /* VG_(malloc)(), VG_(free)()*/
#include "pub_tool_threadstate.h" /* VG_(get_running_tid)()    */


/* Type definitions. */

/** Per-thread hb information. */
struct hb_thread_info
{
   UWord       tid; // A DrdThreadId declared as UWord because
                    // this member variable is the key of an OSet.
   Segment*    sg;  // Segment created before most recent
                    // ANNOTATE_HAPPENS_BEFORE().
};


/* Local functions. */

static void DRD_(hb_cleanup)(struct hb_info* p);


/* Local variables. */

static Bool DRD_(s_trace_hb);


/* Function definitions. */

void DRD_(hb_set_trace)(const Bool trace_hb)
{
   DRD_(s_trace_hb) = trace_hb;
}

/**
 * Initialize the structure *p with the specified thread ID.
 */
static
void DRD_(hb_thread_initialize)(struct hb_thread_info* const p,
                                  const DrdThreadId tid)
{
   p->tid  = tid;
   p->sg   = 0;
}

/**
 * Deallocate the memory that is owned by members of struct hb_thread_info.
 */
static void DRD_(hb_thread_destroy)(struct hb_thread_info* const p)
{
   tl_assert(p);
   DRD_(sg_put)(p->sg);
}

static
void DRD_(hb_initialize)(struct hb_info* const p, const Addr hb)
{
   tl_assert(hb != 0);
   tl_assert(p->a1   == hb);
   tl_assert(p->type == ClientHbvar);

   p->cleanup       = (void(*)(DrdClientobj*))(DRD_(hb_cleanup));
   p->delete_thread = 0;
   p->oset          = VG_(OSetGen_Create)(0, 0, VG_(malloc), "drd.hb",
                                          VG_(free));
}

/**
 * Free the memory that was allocated by hb_initialize(). Called by
 * DRD_(clientobj_remove)().
 */
static void DRD_(hb_cleanup)(struct hb_info* p)
{
   struct hb_thread_info* r;

   tl_assert(p);
   VG_(OSetGen_ResetIter)(p->oset);
   for ( ; (r = VG_(OSetGen_Next)(p->oset)) != 0; )
      DRD_(hb_thread_destroy)(r);
   VG_(OSetGen_Destroy)(p->oset);
}

/**
 * Report that the synchronization object at address 'addr' is of the
 * wrong type.
 */
static void wrong_type(const Addr addr)
{
   GenericErrInfo gei = {
      .tid  = DRD_(thread_get_running_tid)(),
      .addr = addr,
   };
   VG_(maybe_record_error)(VG_(get_running_tid)(),
                           GenericErr,
                           VG_(get_IP)(VG_(get_running_tid)()),
                           "wrong type of synchronization object",
                           &gei);
}

struct hb_info* DRD_(hb_get_or_allocate)(const Addr hb)
{
   struct hb_info *p;

   tl_assert(offsetof(DrdClientobj, hb) == 0);
   p = &(DRD_(clientobj_get)(hb, ClientHbvar)->hb);
   if (p)
      return p;

   if (DRD_(clientobj_present)(hb, hb + 1))
   {
      wrong_type(hb);
      return 0;
   }

   p = &(DRD_(clientobj_add)(hb, ClientHbvar)->hb);
   DRD_(hb_initialize)(p, hb);
   return p;
}

struct hb_info* DRD_(hb_get)(const Addr hb)
{
   tl_assert(offsetof(DrdClientobj, hb) == 0);
   return &(DRD_(clientobj_get)(hb, ClientHbvar)->hb);
}

/** Called because of a happens-before annotation. */
void DRD_(hb_happens_before)(const DrdThreadId tid, Addr const hb)
{
   const ThreadId vg_tid = VG_(get_running_tid)();
   const DrdThreadId drd_tid = DRD_(VgThreadIdToDrdThreadId)(vg_tid);
   const UWord word_tid = tid;
   struct hb_info* p;
   struct hb_thread_info* q;

   p = DRD_(hb_get_or_allocate)(hb);
   if (DRD_(s_trace_hb))
      DRD_(trace_msg)("[%d] happens_before 0x%lx",
                      DRD_(thread_get_running_tid)(), hb);

   if (!p)
      return;

   /* Allocate the per-thread data structure if necessary. */
   q = VG_(OSetGen_Lookup)(p->oset, &word_tid);
   if (!q)
   {
      q = VG_(OSetGen_AllocNode)(p->oset, sizeof(*q));
      DRD_(hb_thread_initialize)(q, tid);
      VG_(OSetGen_Insert)(p->oset, q);
      tl_assert(VG_(OSetGen_Lookup)(p->oset, &word_tid) == q);
   }

   /*
    * Store a pointer to the latest segment of the current thread in the
    * per-thread data structure.
    */
   DRD_(thread_get_latest_segment)(&q->sg, tid);
   DRD_(thread_new_segment)(drd_tid);
}

/** Called because of a happens-after annotation. */
void DRD_(hb_happens_after)(const DrdThreadId tid, const Addr hb)
{
   struct hb_info* p;
   struct hb_thread_info* q;
   VectorClock old_vc;

   p = DRD_(hb_get_or_allocate)(hb);

   if (DRD_(s_trace_hb))
      DRD_(trace_msg)("[%d] happens_after  0x%lx",
                      DRD_(thread_get_running_tid)(), hb);

   if (!p)
      return;

   DRD_(thread_new_segment)(tid);

   /*
    * Combine all vector clocks that were stored because of happens-before
    * annotations with the vector clock of the current thread.
    */
   DRD_(vc_copy)(&old_vc, &DRD_(g_threadinfo)[tid].last->vc);
   VG_(OSetGen_ResetIter)(p->oset);
   for ( ; (q = VG_(OSetGen_Next)(p->oset)) != 0; )
   {
      if (q->tid != tid)
      {
         tl_assert(q->sg);
         DRD_(vc_combine)(&DRD_(g_threadinfo)[tid].last->vc, &q->sg->vc);
      }
   }
   DRD_(thread_update_conflict_set)(tid, &old_vc);
   DRD_(vc_cleanup)(&old_vc);
}

/** Called because of a happens-done annotation. */
void DRD_(hb_happens_done)(const DrdThreadId tid, const Addr hb)
{
   struct hb_info* p;

   if (DRD_(s_trace_hb))
      DRD_(trace_msg)("[%d] happens_done  0x%lx",
                      DRD_(thread_get_running_tid)(), hb);

   p = DRD_(hb_get)(hb);
   if (!p)
   {
      GenericErrInfo gei = {
	 .tid = DRD_(thread_get_running_tid)(),
	 .addr = hb,
      };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              GenericErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "missing happens-before annotation",
                              &gei);
      return;
   }

   DRD_(clientobj_remove)(p->a1, ClientHbvar);
}
