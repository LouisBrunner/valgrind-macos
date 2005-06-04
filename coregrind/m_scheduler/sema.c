
/*--------------------------------------------------------------------*/
/*--- Semaphore stuff.                                      sema.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
      jseward@acm.org

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

#include "core.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "priv_sema.h"

/* 
   Slower (than the removed futex-based sema scheme) but more portable
   pipe-based token passing scheme.
 */

void VG_(sema_init)(vg_sema_t *sema)
{
   VG_(pipe)(sema->pipe);
   sema->pipe[0] = VG_(safe_fd)(sema->pipe[0]);
   sema->pipe[1] = VG_(safe_fd)(sema->pipe[1]);

   sema->owner_thread = -1;

   /* create initial token */
   VG_(write)(sema->pipe[1], "T", 1);
}

void VG_(sema_deinit)(vg_sema_t *sema)
{
   VG_(close)(sema->pipe[0]);
   VG_(close)(sema->pipe[1]);
   sema->pipe[0] = sema->pipe[1] = -1;
}

/* get a token */
void VG_(sema_down)(vg_sema_t *sema)
{
   Char buf[2] = { 'x' };
   Int ret;
   Int lwpid = VG_(gettid)();

   vg_assert(sema->owner_thread != lwpid); /* can't have it already */

  again:
   ret = VG_(read)(sema->pipe[0], buf, 2);

   if (ret == -VKI_EINTR)
      goto again;

   vg_assert(ret == 1);		/* should get exactly 1 token */
   vg_assert(buf[0] == 'T');

   sema->owner_thread = lwpid;
}

/* put token back */
void VG_(sema_up)(vg_sema_t *sema)
{
   Int ret;

   vg_assert(sema->owner_thread == VG_(gettid)()); /* must have it */

   sema->owner_thread = 0;

   ret = VG_(write)(sema->pipe[1], "T", 1);

   vg_assert(ret == 1);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/


