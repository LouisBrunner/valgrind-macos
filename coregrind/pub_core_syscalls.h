
/*--------------------------------------------------------------------*/
/*--- System call wrappers, etc.               pub_core_syscalls.h ---*/
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

#ifndef __PUB_CORE_SYSCALLS_H
#define __PUB_CORE_SYSCALLS_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the syscall junk:  mostly wrappers,
// but also the code that executes them, and related stuff.
//--------------------------------------------------------------------

extern HChar* VG_(resolve_filename_nodup)(Int fd);
extern HChar* VG_(resolve_filename)(Int fd);

extern void VG_(client_syscall) ( ThreadId tid );

extern void VG_(post_syscall)   ( ThreadId tid );

// Fix up the thread's state because a syscall may have been
// interrupted with a signal.  Returns True if the syscall completed
// (either interrupted or finished normally), or False if it was
// restarted (or the signal didn't actually interrupt a syscall).
extern void VGP_(interrupted_syscall)(ThreadId tid,
                                      struct vki_ucontext *uc,
                                      Bool restart);

// Release resources held by this thread
extern void VGP_(cleanup_thread) ( ThreadArchState* );

extern Bool VG_(is_kerror) ( Word res );

/* Internal atfork handlers */
typedef void (*vg_atfork_t)(ThreadId);
extern void VG_(atfork)(vg_atfork_t pre, vg_atfork_t parent, vg_atfork_t child);

/* fd leakage calls. */
extern void VG_(init_preopened_fds) ( void );
extern void VG_(show_open_fds) ( void );

#endif   // __PUB_CORE_SYSCALLS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

