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
#include "drd_semaphore.h"
#include "drd_suppression.h"
#include "priv_drd_clientreq.h"
#include "pub_tool_errormgr.h"    // VG_(maybe_record_error)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"     // VG_(get_IP)()
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()


// Type definitions.

struct semaphore_info
{
  Addr        semaphore;         // Pointer to client semaphore.
  SizeT       size;              // Size in bytes of client-side object.
  UWord       value;             // Semaphore value.
  DrdThreadId last_sem_post_tid; // Thread ID associated with last sem_post().
  VectorClock vc;                // Vector clock of last sem_post() call.
};


// Local variables.

static Bool s_trace_semaphore;
struct semaphore_info s_semaphore[256];


// Function definitions.

void semaphore_set_trace(const Bool trace_semaphore)
{
  s_trace_semaphore = trace_semaphore;
}

static
void semaphore_initialize(struct semaphore_info* const p,
                          const Addr semaphore,
                          const SizeT size,
                          const UWord value)
{
  tl_assert(semaphore != 0);
  tl_assert(size > 0);

  p->semaphore = semaphore;
  p->size      = size;
  p->value     = value;
  p->last_sem_post_tid = DRD_INVALID_THREADID;
  vc_init(&p->vc, 0, 0);
}

static
struct semaphore_info*
semaphore_get_or_allocate(const Addr semaphore, const SizeT size)
{
  int i;

  for (i = 0; i < sizeof(s_semaphore)/sizeof(s_semaphore[0]); i++)
  {
    if (s_semaphore[i].semaphore == semaphore)
    {
      tl_assert(s_semaphore[i].size == size);
      return &s_semaphore[i];
    }
  }
  for (i = 0; i < sizeof(s_semaphore)/sizeof(s_semaphore[0]); i++)
  {
    if (s_semaphore[i].semaphore == 0)
    {
      semaphore_initialize(&s_semaphore[i], semaphore, size, 0);
      drd_start_suppression(semaphore, semaphore + size, "semaphore");
      return &s_semaphore[i];
    }
  }
  tl_assert(0);
  return 0;
}

struct semaphore_info* semaphore_init(const Addr semaphore, const SizeT size,
                                      const Word pshared, const UWord value)
{
  struct semaphore_info* p;

  tl_assert(semaphore_get(semaphore) == 0);
  p = semaphore_get_or_allocate(semaphore, size);
  p->value = value;
  return p;
}

void semaphore_destroy(struct semaphore_info* const p)
{
  drd_finish_suppression(p->semaphore, p->semaphore + p->size);

  vc_cleanup(&p->vc);
  p->semaphore = 0;
}

struct semaphore_info* semaphore_get(const Addr semaphore)
{
  int i;
  for (i = 0; i < sizeof(s_semaphore)/sizeof(s_semaphore[0]); i++)
    if (s_semaphore[i].semaphore == semaphore)
      return &s_semaphore[i];
  return 0;
}

/** Called after sem_wait() finished successfully. */
void semaphore_post_wait(const DrdThreadId tid, const Addr semaphore,
                         const SizeT size)
{
  struct semaphore_info* p;

  p = semaphore_get_or_allocate(semaphore, size);
  tl_assert(p->value >= 0);
  p->value--;
  tl_assert(p->value >= 0);
  if (p->last_sem_post_tid != tid)
    thread_combine_vc2(tid, &p->vc);
  thread_new_segment(tid);
}

/** Called before sem_post(). */
void semaphore_pre_post(const DrdThreadId tid, const Addr semaphore,
                        const SizeT size)
{
  struct semaphore_info* p;

  p = semaphore_get_or_allocate(semaphore, size);
  p->value++;
  if (p->value == 1)
  {
    p->last_sem_post_tid = tid;
  }
}

/** Called after sem_post() finished successfully. */
void semaphore_post_post(const DrdThreadId tid, const Addr semaphore,
                         const SizeT size)
{
  struct semaphore_info* p;

  p = semaphore_get_or_allocate(semaphore, size);
  thread_new_segment(tid);
  vc_copy(&p->vc, thread_get_vc(tid));
}

void semaphore_thread_delete(const DrdThreadId threadid)
{ }

void semaphore_stop_using_mem(const Addr a1, const Addr a2)
{
  unsigned i;
  for (i = 0; i < sizeof(s_semaphore)/sizeof(s_semaphore[0]); i++)
  {
    if (a1 <= s_semaphore[i].semaphore && s_semaphore[i].semaphore < a2)
    {
      tl_assert(s_semaphore[i].semaphore + s_semaphore[i].size <= a2);
      semaphore_destroy(&s_semaphore[i]);
    }
  }
}
