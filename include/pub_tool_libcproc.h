
/*--------------------------------------------------------------------*/
/*--- Process-related libc stuff               pub_tool_libcproc.h ---*/
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

#ifndef __PUB_TOOL_LIBCPROC_H
#define __PUB_TOOL_LIBCPROC_H

/* ---------------------------------------------------------------------
   Command-line and environment stuff
   ------------------------------------------------------------------ */

/* Client args and environment (which can be inspected with VG_(getenv)(). */
extern Int    VG_(client_argc);
extern Char** VG_(client_argv);
extern Char** VG_(client_envp);

/* Looks up VG_(client_envp) */
extern Char* VG_(getenv) ( Char* name );

/* ---------------------------------------------------------------------
   Important syscalls
   ------------------------------------------------------------------ */

extern Int VG_(waitpid)( Int pid, Int *status, Int options );
extern Int VG_(system) ( Char* cmd );

/* ---------------------------------------------------------------------
   Resource limits
   ------------------------------------------------------------------ */

extern Int VG_(getrlimit) ( Int resource, struct vki_rlimit *rlim );
extern Int VG_(setrlimit) ( Int resource, const struct vki_rlimit *rlim );

/* ---------------------------------------------------------------------
   pids, etc
   ------------------------------------------------------------------ */

extern Int VG_(gettid)	 ( void );
extern Int VG_(getpid)  ( void );
extern Int VG_(getppid) ( void );
extern Int VG_(getpgrp) ( void );
extern Int VG_(setpgid) ( Int pid, Int pgrp );

/* ---------------------------------------------------------------------
   Timing
   ------------------------------------------------------------------ */

extern UInt VG_(read_millisecond_timer) ( void );

#endif   // __PUB_TOOL_LIBCPROC_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
