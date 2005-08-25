
/*--------------------------------------------------------------------*/
/*--- Client-space code for the core.               vg_preloaded.c ---*/
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


/* ---------------------------------------------------------------------
   ALL THE CODE IN THIS FILE RUNS ON THE SIMULATED CPU. 

   These functions are not called directly - they're the targets of code
   redirection or load notifications (see pub_core_redir.h for info).
   They're named weirdly so that the intercept code can find them when the
   shared object is initially loaded.

   Note that this filename has the "vg_" prefix because it can appear
   in stack traces, and the "vg_" makes it a little clearer that it
   originates from Valgrind.
   ------------------------------------------------------------------ */

#include "valgrind.h"
#include "coregrind.h"

#include "pub_core_basics.h"
#include "pub_core_debuginfo.h"  // Needed for pub_core_redir.h
#include "pub_core_redir.h"      // For VG_NOTIFY_ON_LOAD

/* ---------------------------------------------------------------------
   Hook for running __libc_freeres once the program exits.
   ------------------------------------------------------------------ */

void VG_NOTIFY_ON_LOAD(freeres)( void );
void VG_NOTIFY_ON_LOAD(freeres)( void )
{
   int res;
#ifndef __UCLIBC__
   extern void __libc_freeres(void);
   __libc_freeres();
#endif
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__LIBC_FREERES_DONE, 0, 0, 0, 0);
   /*NOTREACHED*/
   *(int *)0 = 'x';
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

