/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2013 Bart Van Assche <bvanassche@acm.org>.

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
#include "drd_cond.h"
#include "drd_error.h"
#include "drd_mutex.h"
#include "pub_tool_errormgr.h"    /* VG_(maybe_record_error)() */
#include "pub_tool_libcassert.h"  /* tl_assert()               */
#include "pub_tool_libcbase.h"    /* VG_(memcmp)()             */
#include "pub_tool_libcprint.h"   /* VG_(printf)()             */
#include "pub_tool_machine.h"     /* VG_(get_IP)()             */
#include "pub_tool_threadstate.h" /* VG_(get_running_tid)()    */


/* Local functions. */

static void DRD_(cond_cleanup)(struct cond_info* p);


/* Local variables. */

static Bool DRD_(s_report_signal_unlocked) = True;
static Bool DRD_(s_trace_cond);


/* Function definitions. */

void DRD_(cond_set_report_signal_unlocked)(const Bool r)
{
   DRD_(s_report_signal_unlocked) = r;
}

void DRD_(cond_set_trace)(const Bool trace_cond)
{
   DRD_(s_trace_cond) = trace_cond;
}

static
void DRD_(cond_initialize)(struct cond_info* const p, const Addr cond)
{
   tl_assert(cond != 0);
   tl_assert(p->a1   == cond);
   tl_assert(p->type == ClientCondvar);

   p->cleanup       = (void(*)(DrdClientobj*))(DRD_(cond_cleanup));
   p->delete_thread = 0;
   p->waiter_count  = 0;
   p->mutex         = 0;
}

/**
 * Free the memory that was allocated by cond_initialize(). Called by
 * DRD_(clientobj_remove)().
 */
static void DRD_(cond_cleanup)(struct cond_info* p)
{
   tl_assert(p);
   if (p->mutex)
   {
      struct mutex_info* q;
      q = &(DRD_(clientobj_get)(p->mutex, ClientMutex)->mutex);
      {
         CondDestrErrInfo cde = {
	    DRD_(thread_get_running_tid)(),
	    p->a1,
	    q ? q->a1 : 0,
	    q ? q->owner : DRD_INVALID_THREADID
	 };
         VG_(maybe_record_error)(VG_(get_running_tid)(),
                                 CondDestrErr,
                                 VG_(get_IP)(VG_(get_running_tid)()),
                                 "Destroying condition variable that is being"
                                 " waited upon",
                                 &cde);
      }
   }
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

static struct cond_info* cond_get_or_allocate(const Addr cond)
{
   struct cond_info *p;

   tl_assert(offsetof(DrdClientobj, cond) == 0);
   p = &(DRD_(clientobj_get)(cond, ClientCondvar)->cond);
   if (p)
      return p;

   if (DRD_(clientobj_present)(cond, cond + 1))
   {
      wrong_type(cond);
      return 0;
   }

   p = &(DRD_(clientobj_add)(cond, ClientCondvar)->cond);
   DRD_(cond_initialize)(p, cond);
   return p;
}

struct cond_info* DRD_(cond_get)(const Addr cond)
{
   tl_assert(offsetof(DrdClientobj, cond) == 0);
   return &(DRD_(clientobj_get)(cond, ClientCondvar)->cond);
}

/** Called before pthread_cond_init(). */
void DRD_(cond_pre_init)(const Addr cond)
{
   struct cond_info* p;

   if (DRD_(s_trace_cond))
      DRD_(trace_msg)("[%d] cond_init       cond 0x%lx",
                      DRD_(thread_get_running_tid)(), cond);

   p = DRD_(cond_get)(cond);

   if (p) {
      CondErrInfo cei = { .tid = DRD_(thread_get_running_tid)(), .cond = cond };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "initialized twice",
                              &cei);
   }

   cond_get_or_allocate(cond);
}

/** Called after pthread_cond_destroy(). */
void DRD_(cond_post_destroy)(const Addr cond, const Bool destroy_succeeded)
{
   struct cond_info* p;

   if (DRD_(s_trace_cond))
      DRD_(trace_msg)("[%d] cond_destroy    cond 0x%lx",
                      DRD_(thread_get_running_tid)(), cond);

   p = DRD_(cond_get)(cond);
   if (p == 0)
   {
      CondErrInfo cei = { .tid = DRD_(thread_get_running_tid)(), .cond = cond };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "not a condition variable",
                              &cei);
      return;
   }

   if (p->waiter_count != 0)
   {
      CondErrInfo cei = { .tid = DRD_(thread_get_running_tid)(), .cond = cond };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "destruction of condition variable being waited"
                              " upon",
                              &cei);
   }

   if (destroy_succeeded)
      DRD_(clientobj_remove)(p->a1, ClientCondvar);
}

/**
 * Called before pthread_cond_wait(). Note: before this function is called,
 * mutex_unlock() has already been called from drd_clientreq.c.
 */
void DRD_(cond_pre_wait)(const Addr cond, const Addr mutex)
{
   struct cond_info* p;
   struct mutex_info* q;

   if (DRD_(s_trace_cond))
      DRD_(trace_msg)("[%d] cond_pre_wait   cond 0x%lx",
                      DRD_(thread_get_running_tid)(), cond);

   p = cond_get_or_allocate(cond);
   if (!p)
   {
      CondErrInfo cei = { .tid = DRD_(thread_get_running_tid)(), .cond = cond };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "not a condition variable",
                              &cei);
      return;
   }

   if (p->waiter_count == 0)
   {
      p->mutex = mutex;
   }
   else if (p->mutex != mutex)
   {
      CondWaitErrInfo cwei
         = { .tid = DRD_(thread_get_running_tid)(),
             .cond = cond, .mutex1 = p->mutex, .mutex2 = mutex };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondWaitErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Inconsistent association of condition variable"
                              " and mutex",
                              &cwei);
   }
   tl_assert(p->mutex);
   q = DRD_(mutex_get)(p->mutex);
   if (q
       && q->owner == DRD_(thread_get_running_tid)() && q->recursion_count > 0)
   {
      const ThreadId vg_tid = VG_(get_running_tid)();
      MutexErrInfo MEI = { DRD_(thread_get_running_tid)(),
                           q->a1, q->recursion_count, q->owner };
      VG_(maybe_record_error)(vg_tid,
                              MutexErr,
                              VG_(get_IP)(vg_tid),
                              "Mutex locked recursively",
                              &MEI);
   }
   else if (q == 0)
   {
      DRD_(not_a_mutex)(p->mutex);
   }

   ++p->waiter_count;
}

/**
 * Called after pthread_cond_wait().
 */
void DRD_(cond_post_wait)(const Addr cond)
{
   struct cond_info* p;

   if (DRD_(s_trace_cond))
      DRD_(trace_msg)("[%d] cond_post_wait  cond 0x%lx",
                      DRD_(thread_get_running_tid)(), cond);

   p = DRD_(cond_get)(cond);
   if (!p)
   {
      CondDestrErrInfo cde = {
         DRD_(thread_get_running_tid)(), cond, 0, DRD_INVALID_THREADID
      };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondDestrErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "condition variable has been destroyed while"
                              " being waited upon",
                              &cde);
      return;
   }

   if (p->waiter_count > 0)
   {
      --p->waiter_count;
      if (p->waiter_count == 0)
      {
	 p->mutex = 0;
      }
   }
}

static void cond_signal(const DrdThreadId tid, struct cond_info* const cond_p)
{
   const ThreadId vg_tid = VG_(get_running_tid)();
   const DrdThreadId drd_tid = DRD_(VgThreadIdToDrdThreadId)(vg_tid);

   tl_assert(cond_p);

   if (cond_p->waiter_count > 0)
   {
      if (DRD_(s_report_signal_unlocked)
	  && ! DRD_(mutex_is_locked_by)(cond_p->mutex, drd_tid))
      {
	 /*
	  * A signal is sent while the associated mutex has not been locked.
	  * This can indicate but is not necessarily a race condition.
	  */
	 CondRaceErrInfo cei = { .tid = DRD_(thread_get_running_tid)(),
				 .cond  = cond_p->a1,
				 .mutex = cond_p->mutex,
	 };
	 VG_(maybe_record_error)(vg_tid,
				 CondRaceErr,
				 VG_(get_IP)(vg_tid),
				 "CondErr",
				 &cei);
      }
   }
   else
   {
      /*
       * No other thread is waiting for the signal, hence the signal will
       * be lost. This is normal in a POSIX threads application.
       */
   }
}

static void not_initialized(Addr const cond)
{
   CondErrInfo cei = { .tid = DRD_(thread_get_running_tid)(), .cond = cond };
   VG_(maybe_record_error)(VG_(get_running_tid)(),
                           CondErr,
                           VG_(get_IP)(VG_(get_running_tid)()),
                           "condition variable has not been initialized",
                           &cei);
}

/** Called before pthread_cond_signal(). */
void DRD_(cond_pre_signal)(Addr const cond)
{
   struct cond_info* p;

   p = DRD_(cond_get)(cond);
   if (DRD_(s_trace_cond))
      DRD_(trace_msg)("[%d] cond_signal     cond 0x%lx",
                      DRD_(thread_get_running_tid)(), cond);

   tl_assert(DRD_(pthread_cond_initializer));
   if (!p && VG_(memcmp)((void*)cond, (void*)DRD_(pthread_cond_initializer),
                         DRD_(pthread_cond_initializer_size)) != 0)
   {
      not_initialized(cond);
      return;
   }

   if (!p)
      p = cond_get_or_allocate(cond);

   cond_signal(DRD_(thread_get_running_tid)(), p);
}

/** Called before pthread_cond_broadcast(). */
void DRD_(cond_pre_broadcast)(Addr const cond)
{
   struct cond_info* p;

   if (DRD_(s_trace_cond))
      DRD_(trace_msg)("[%d] cond_broadcast  cond 0x%lx",
                      DRD_(thread_get_running_tid)(), cond);

   p = DRD_(cond_get)(cond);
   tl_assert(DRD_(pthread_cond_initializer));
   if (!p && VG_(memcmp)((void*)cond, (void*)DRD_(pthread_cond_initializer),
                         DRD_(pthread_cond_initializer_size)) != 0)
   {
      not_initialized(cond);
      return;
   }

   if (!p)
      p = cond_get_or_allocate(cond);

   cond_signal(DRD_(thread_get_running_tid)(), p);
}
