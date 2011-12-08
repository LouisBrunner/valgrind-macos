
/*--------------------------------------------------------------------*/
/*--- Generic scheduler lock implementation   sched-lock-generic.c ---*/
/*---                                                              ---*/
/*--- This implementation does not guarantee fair scheduling on    ---*/
/*--- multicore systems but is sufficient to make the Valgrind     ---*/
/*--- scheduler work reasonably.                                   ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011 Bart Van Assche <bvanassche@acm.org>.

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

#include "pub_core_basics.h"
#include "pub_tool_mallocfree.h"
#include "priv_sema.h"
#include "priv_sched-lock.h"
#include "priv_sched-lock-impl.h"

struct sched_lock {
   vg_sema_t sema;
};

static const Char *get_sched_lock_name(void)
{
   return "generic";
}

static struct sched_lock *create_sched_lock(void)
{
   struct sched_lock *p;

   p = VG_(malloc)("sched_lock", sizeof(*p));
   if (p)
      ML_(sema_init)(&p->sema);
   return p;
}

static void destroy_sched_lock(struct sched_lock *p)
{
   ML_(sema_deinit)(&p->sema);
   VG_(free)(p);
}

static int get_sched_lock_owner(struct sched_lock *p)
{
   return p->sema.owner_lwpid;
}

static void acquire_sched_lock(struct sched_lock *p)
{
   ML_(sema_down)(&p->sema, False);
}

static void release_sched_lock(struct sched_lock *p)
{
   ML_(sema_up)(&p->sema, False);
}

const struct sched_lock_ops ML_(generic_sched_lock_ops) = {
   .get_sched_lock_name  = get_sched_lock_name,
   .create_sched_lock    = create_sched_lock,
   .destroy_sched_lock   = destroy_sched_lock,
   .get_sched_lock_owner = get_sched_lock_owner,
   .acquire_sched_lock   = acquire_sched_lock,
   .release_sched_lock   = release_sched_lock,
};
