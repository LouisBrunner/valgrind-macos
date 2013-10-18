
/*--------------------------------------------------------------------*/
/*--- Scheduler lock support functions                sched-lock.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2013 Bart Van Assche <bvanassche@acm.org>.

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

#include "config.h"
#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_mallocfree.h"
#include "priv_sema.h"
#include "priv_sched-lock.h"
#include "priv_sched-lock-impl.h"

static struct sched_lock_ops const *sched_lock_ops =
   &ML_(generic_sched_lock_ops);

static struct sched_lock_ops const *const sched_lock_impl[] = {
   [sched_lock_generic] = &ML_(generic_sched_lock_ops),
#ifdef ENABLE_LINUX_TICKET_LOCK
   [sched_lock_ticket]  = &ML_(linux_ticket_lock_ops),
#endif
};

/**
 * Define which scheduler lock implementation to use.
 *
 * @param[in] t Scheduler lock type.
 *
 * @return True if and only if this function succeeded.
 *
 * @note Must be called before any other sched_lock*() function is invoked.
 */
Bool ML_(set_sched_lock_impl)(const enum SchedLockType t)
{
   struct sched_lock_ops const *p = NULL;

   if ((unsigned)t < sizeof(sched_lock_impl)/sizeof(sched_lock_impl[0]))
      p = sched_lock_impl[t];
   if (p)
      sched_lock_ops = p;
   return !!p;
}

const HChar *ML_(get_sched_lock_name)(void)
{
   return (sched_lock_ops->get_sched_lock_name)();
}

struct sched_lock *ML_(create_sched_lock)(void)
{
   return (sched_lock_ops->create_sched_lock)();
}

void ML_(destroy_sched_lock)(struct sched_lock *p)
{
   return (sched_lock_ops->destroy_sched_lock)(p);
}

int ML_(get_sched_lock_owner)(struct sched_lock *p)
{
   return (sched_lock_ops->get_sched_lock_owner)(p);
}

void ML_(acquire_sched_lock)(struct sched_lock *p)
{
   return (sched_lock_ops->acquire_sched_lock)(p);
}

void ML_(release_sched_lock)(struct sched_lock *p)
{
   return (sched_lock_ops->release_sched_lock)(p);
}
