
/*--------------------------------------------------------------------*/
/*--- Private scheduler header.                        priv_sema.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward
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

#ifndef __PRIV_SEMA_H
#define __PRIV_SEMA_H

#include "pub_core_basics.h"   // Bool

/* Not really a semaphore, but use a pipe for a token-passing scheme */
typedef struct {
   Int  pipe[2];
   Int  owner_lwpid;  /* who currently has it */
   Bool held_as_LL;   /* if held, True == held by a _LL call */
} vg_sema_t;

// Nb: this may be OS-specific, but let's not factor it out until we
// implement an OS port for which this isn't ok.
void ML_(sema_init)   ( vg_sema_t *sema );
void ML_(sema_deinit) ( vg_sema_t *sema );
void ML_(sema_down)   ( vg_sema_t *sema, Bool as_LL );
void ML_(sema_up)     ( vg_sema_t *sema, Bool as_LL );

#endif   // __PRIV_SEMA_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

