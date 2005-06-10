
/*--------------------------------------------------------------------*/
/*--- Signal-related libc stuff.             pub_tool_libcsignal.h ---*/
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

#ifndef __PUB_TOOL_LIBCBSIGNAL_H
#define __PUB_TOOL_LIBCBSIGNAL_H

/* Note that these use the vki_ (kernel) structure
   definitions, which are different in places from those that glibc
   defines.  Since we're operating right at the kernel interface, glibc's view
   of the world is entirely irrelevant. */

/* --- Signal set ops --- */
extern Int  VG_(sigfillset)  ( vki_sigset_t* set );
extern Int  VG_(sigemptyset) ( vki_sigset_t* set );

extern Bool VG_(isfullsigset)  ( const vki_sigset_t* set );
extern Bool VG_(isemptysigset) ( const vki_sigset_t* set );
extern Bool VG_(iseqsigset)    ( const vki_sigset_t* set1,
                                 const vki_sigset_t* set2 );

extern Int  VG_(sigaddset)   ( vki_sigset_t* set, Int signum );
extern Int  VG_(sigdelset)   ( vki_sigset_t* set, Int signum );
extern Int  VG_(sigismember) ( const vki_sigset_t* set, Int signum );

extern void VG_(sigaddset_from_set) ( vki_sigset_t* dst, vki_sigset_t* src );
extern void VG_(sigdelset_from_set) ( vki_sigset_t* dst, vki_sigset_t* src );

/* --- Mess with the kernel's sig state --- */
extern Int VG_(sigprocmask) ( Int how, const vki_sigset_t* set,
                              vki_sigset_t* oldset );
extern Int VG_(sigaction)   ( Int signum,
                              const struct vki_sigaction* act,
                              struct vki_sigaction* oldact );

extern Int VG_(sigtimedwait)( const vki_sigset_t *, vki_siginfo_t *, 
			      const struct vki_timespec * );

extern Int VG_(signal)      ( Int signum, void (*sighandler)(Int) );
extern Int VG_(sigaltstack) ( const vki_stack_t* ss, vki_stack_t* oss );

extern Int VG_(kill)        ( Int pid, Int signo );
extern Int VG_(tkill)       ( ThreadId tid, Int signo );
extern Int VG_(sigpending)  ( vki_sigset_t* set );

#endif   // __PUB_TOOL_LIBCBSIGNAL_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
