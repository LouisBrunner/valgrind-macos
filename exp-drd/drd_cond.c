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


#include "drd_cond.h"
#include "drd_error.h"
#include "drd_mutex.h"
#include "drd_suppression.h"
#include "pub_tool_errormgr.h"    // VG_(maybe_record_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()
#include "pub_core_options.h"     // VG_(clo_backtrace_size)


static struct cond_info s_cond[256];
static Bool s_trace_cond;


void cond_set_trace(const Bool trace_cond)
{
  s_trace_cond = trace_cond;
}

static
void cond_initialize(struct cond_info* const p, const Addr cond,
                     const SizeT size)
{
  tl_assert(cond != 0);

  p->cond         = cond;
  p->size         = size;
  p->waiter_count = 0;
  p->mutex        = 0;
}

static struct cond_info*
cond_get_or_allocate(const Addr cond, const SizeT size)
{
  int i;
  for (i = 0; i < sizeof(s_cond)/sizeof(s_cond[0]); i++)
  {
    if (s_cond[i].cond == cond)
    {
      tl_assert(s_cond[i].size == size);
      return &s_cond[i];
    }
  }
  for (i = 0; i < sizeof(s_cond)/sizeof(s_cond[0]); i++)
  {
    if (s_cond[i].cond == 0)
    {
      cond_initialize(&s_cond[i], cond, size);
      /* TO DO: replace the constant below by a symbolic constant referring */
      /* to sizeof(pthread_cond_t).                                        */
      drd_start_suppression(cond, cond + size, "cond");
      return &s_cond[i];
    }
  }
  tl_assert(0);
  return 0;
}

void cond_init(const Addr cond, const SizeT size)
{
  if (s_trace_cond)
  {
    VG_(message)(Vg_UserMsg, "Initializing condition variable 0x%lx", cond);
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(),
                               VG_(clo_backtrace_size));
  }
  tl_assert(cond_get(cond) == 0);
  tl_assert(size > 0);
  cond_get_or_allocate(cond, size);
}

void cond_destroy(struct cond_info* const p)
{
  if (s_trace_cond)
  {
    VG_(message)(Vg_UserMsg, "Destroying condition variable 0x%lx", p->cond);
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(),
                               VG_(clo_backtrace_size));
  }

  // TO DO: print a proper error message if waiter_count != 0.
  tl_assert(p->waiter_count == 0);

  drd_finish_suppression(p->cond, p->cond + p->size);

  p->cond         = 0;
  p->waiter_count = 0;
  p->mutex        = 0;
}

struct cond_info* cond_get(const Addr cond)
{
  int i;
  for (i = 0; i < sizeof(s_cond)/sizeof(s_cond[0]); i++)
    if (s_cond[i].cond == cond)
      return &s_cond[i];
  return 0;
}

int cond_pre_wait(const Addr cond, const SizeT cond_size, const Addr mutex)
{
  struct cond_info* p;

  p = cond_get_or_allocate(cond, cond_size);
  if (p->waiter_count == 0)
  {
    p->mutex = mutex;
  }
  else
  {
    // TO DO: print a proper error message if two different threads call
    // pthread_cond_*wait() on the same condition variable but with a different
    // mutex argument.
    tl_assert(p->mutex == mutex);
  }
  return ++p->waiter_count;
}

int cond_post_wait(const Addr cond)
{
  struct cond_info* p;

  p = cond_get(cond);
  tl_assert(p);
  tl_assert(p->waiter_count > 0);
  tl_assert(p->mutex);
  if (--p->waiter_count == 0)
  {
    p->mutex = 0;
  }
  return p->waiter_count;
}

void cond_pre_signal(Addr const cond)
{
  const ThreadId vg_tid = VG_(get_running_tid)();
  const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(vg_tid);
  struct cond_info* const cond_p = cond_get(cond);
#if 0
  VG_(message)(Vg_DebugMsg, "cond_pre_signal cond %d, w.c. %d, mutex %d",
               cond,
               cond_p ? cond_p->waiter_count : 0,
               cond_p ? cond_p->mutex : 0);
#endif
  if (cond_p && cond_p->waiter_count > 0)
  {
    if (! mutex_is_locked_by(cond_p->mutex, drd_tid))
    {
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

void cond_pre_broadcast(Addr const cond)
{
  cond_pre_signal(cond);
}

void cond_stop_using_mem(const Addr a1, const Addr a2)
{
  unsigned i;
  for (i = 0; i < sizeof(s_cond)/sizeof(s_cond[0]); i++)
  {
    if (a1 <= s_cond[i].cond && s_cond[i].cond < a2)
    {
      tl_assert(s_cond[i].cond + s_cond[i].size <= a2);
      cond_destroy(&s_cond[i]);
    }
  }
}
