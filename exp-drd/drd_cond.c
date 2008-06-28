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


#include "drd_clientobj.h"
#include "drd_cond.h"
#include "drd_error.h"
#include "drd_mutex.h"
#include "drd_suppression.h"
#include "pub_tool_errormgr.h"    /* VG_(maybe_record_error)() */
#include "pub_tool_libcassert.h"  /* tl_assert()               */
#include "pub_tool_libcprint.h"   /* VG_(printf)()             */
#include "pub_tool_machine.h"     /* VG_(get_IP)()             */
#include "pub_tool_options.h"     /* VG_(clo_backtrace_size)   */
#include "pub_tool_threadstate.h" /* VG_(get_running_tid)()    */


/* Local functions. */

static void cond_cleanup(struct cond_info* p);


/* Global variables. */

Bool s_drd_report_signal_unlocked = True;


/* Local variables. */

static Bool s_trace_cond;


/* Function definitions. */

void cond_set_trace(const Bool trace_cond)
{
  s_trace_cond = trace_cond;
}

static
void cond_initialize(struct cond_info* const p, const Addr cond)
{
  tl_assert(cond != 0);
  tl_assert(p->a1         == cond);
  tl_assert(p->type       == ClientCondvar);

  p->cleanup      = (void(*)(DrdClientobj*))cond_cleanup;
  p->waiter_count = 0;
  p->mutex        = 0;
}

/** Free the memory that was allocated by cond_initialize(). Called by
 *  clientobj_remove().
 */
static void cond_cleanup(struct cond_info* p)
{
  tl_assert(p);
  if (p->mutex)
  {
    struct mutex_info* q;
    q = &clientobj_get(p->mutex, ClientMutex)->mutex;
    tl_assert(q);
    {
      CondDestrErrInfo cde = { p->a1, q->a1, q->owner };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondDestrErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Destroying condition variable that is being"
                              " waited upon",
                              &cde);
    }
  }
}

static struct cond_info* cond_get_or_allocate(const Addr cond)
{
  struct cond_info *p;

  tl_assert(offsetof(DrdClientobj, cond) == 0);
  p = &clientobj_get(cond, ClientCondvar)->cond;
  if (p == 0)
  {
    p = &clientobj_add(cond, ClientCondvar)->cond;
    cond_initialize(p, cond);
  }
  return p;
}

static struct cond_info* cond_get(const Addr cond)
{
  tl_assert(offsetof(DrdClientobj, cond) == 0);
  return &clientobj_get(cond, ClientCondvar)->cond;
}

/** Called before pthread_cond_init(). */
void cond_pre_init(const Addr cond)
{
  struct cond_info* p;

  if (s_trace_cond)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] cond_init       cond 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 cond);
  }

  p = cond_get(cond);

  if (p)
  {
    CondErrInfo cei = { .cond = cond };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            CondErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "initialized twice",
                            &cei);
  }

  p = cond_get_or_allocate(cond);
}

/** Called after pthread_cond_destroy(). */
void cond_post_destroy(const Addr cond)
{
  struct cond_info* p;

  if (s_trace_cond)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] cond_destroy    cond 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 cond);
  }

  p = cond_get(cond);
  if (p == 0)
  {
    CondErrInfo cei = { .cond = cond };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            CondErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "not a condition variable",
                            &cei);
    return;
  }

  if (p->waiter_count != 0)
  {
    CondErrInfo cei = { .cond = cond };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            CondErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "destruction of condition variable being waited"
                            " upon",
                            &cei);
  }

  clientobj_remove(p->a1, ClientCondvar);
}

/** Called before pthread_cond_wait(). Note: before this function is called,
 *  mutex_unlock() has already been called from drd_clientreq.c.
 */
int cond_pre_wait(const Addr cond, const Addr mutex)
{
  struct cond_info* p;
  struct mutex_info* q;

  if (s_trace_cond)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] cond_pre_wait   cond 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 cond);
  }

  p = cond_get_or_allocate(cond);
  tl_assert(p);

  if (p->waiter_count == 0)
  {
    p->mutex = mutex;
  }
  else if (p->mutex != mutex)
  {
    CondWaitErrInfo cwei
      = { .cond = cond, .mutex1 = p->mutex, .mutex2 = mutex };
    VG_(maybe_record_error)(VG_(get_running_tid)(),
                            CondWaitErr,
                            VG_(get_IP)(VG_(get_running_tid)()),
                            "Inconsistent association of condition variable"
                            " and mutex",
                            &cwei);
  }
  tl_assert(p->mutex);
  q = mutex_get(p->mutex);
  if (q && q->owner == thread_get_running_tid() && q->recursion_count > 0)
  {
    const ThreadId vg_tid = VG_(get_running_tid)();
    MutexErrInfo MEI = { q->a1, q->recursion_count, q->owner };
    VG_(maybe_record_error)(vg_tid,
                            MutexErr,
                            VG_(get_IP)(vg_tid),
                            "Mutex locked recursively",
                            &MEI);
  }
  else if (q == 0)
  {
    not_a_mutex(p->mutex);
  }

  return ++p->waiter_count;
}

/** Called after pthread_cond_wait(). */
int cond_post_wait(const Addr cond)
{
  struct cond_info* p;

  if (s_trace_cond)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] cond_post_wait  cond 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 cond);
  }

  p = cond_get(cond);
  if (p)
  {
    if (p->waiter_count > 0)
    {
      --p->waiter_count;
      if (p->waiter_count == 0)
      {
        p->mutex = 0;
      }
    }
    return p->waiter_count;
  }
  return 0;
}

static void cond_signal(Addr const cond)
{
  const ThreadId vg_tid = VG_(get_running_tid)();
  const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(vg_tid);
  struct cond_info* const cond_p = cond_get(cond);

  if (cond_p && cond_p->waiter_count > 0)
  {
    if (s_drd_report_signal_unlocked
        && ! mutex_is_locked_by(cond_p->mutex, drd_tid))
    {
      /* A signal is sent while the associated mutex has not been locked. */
      /* This can indicate but is not necessarily a race condition.       */
      CondRaceErrInfo cei;
      cei.cond  = cond;
      cei.mutex = cond_p->mutex;
      VG_(maybe_record_error)(vg_tid,
                              CondRaceErr,
                              VG_(get_IP)(vg_tid),
                              "CondErr",
                              &cei);
    }
  }
  else
  {
    /* No other thread is waiting for the signal, hence the signal will be */
    /* lost. This is normal in a POSIX threads application.                */
  }
}

/** Called before pthread_cond_signal(). */
void cond_pre_signal(Addr const cond)
{
  if (s_trace_cond)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] cond_signal     cond 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 cond);
  }

  cond_signal(cond);
}

/** Called before pthread_cond_broadcast(). */
void cond_pre_broadcast(Addr const cond)
{
  if (s_trace_cond)
  {
    VG_(message)(Vg_UserMsg,
                 "[%d/%d] cond_broadcast  cond 0x%lx",
                 VG_(get_running_tid)(),
                 thread_get_running_tid(),
                 cond);
  }

  cond_signal(cond);
}

/** Called after pthread_cond_destroy(). */
void cond_thread_delete(const DrdThreadId tid)
{ }
