
/*--------------------------------------------------------------------*/
/*--- Misc client state info                pub_core_clientstate.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_CLIENTSTATE_H
#define __PUB_CORE_CLIENTSTATE_H

//--------------------------------------------------------------------
// PURPOSE: This module holds various bits of client state which don't
// live comfortably anywhere else.  Note that the ThreadStates for the
// client don't live here; they instead live in m_threadstate.h.  Most
// of these values are set once at startup and not changed later.
//--------------------------------------------------------------------

#include "pub_tool_clientstate.h"

// Address space globals

// client stack range
extern Addr  VG_(clstk_start_base); // *Initial* lowest byte address
extern Addr  VG_(clstk_end);        // Highest byte address
extern UWord VG_(clstk_id);      // client stack id
extern SizeT VG_(clstk_max_size); // max size of the main threads's client stack

/* Linux and Solaris only: where is the client auxv? */
/* This is setup as part of setup_client_stack in initimg-linux.c
   or initimg-solaris.c, respectively. */
extern UWord* VG_(client_auxv);

extern Addr  VG_(brk_base);	 // start of brk
extern Addr  VG_(brk_limit);	 // current brk

/* A fd which refers to the client executable. */
extern Int VG_(cl_exec_fd);

/* A fd which refers to the fake /proc/<pid>/cmdline in /tmp.  The
   idea is: make up the /proc/<pid>/cmdline file the client would
   expect to see if it was running natively.  Copy into a file in
   /tmp.  When the client then does an open of /proc/<pid>/cmdline or
   /proc/self/cmdline, instead give it a file handle to the file in
   /tmp.  The problem of deleting said file when Valgrind exits is
   neatly sidestepped by unlinking it as soon as it has been created,
   but holding on to the file handle.  That causes the kernel to keep
   the file contents alive exactly until the process exits. */
extern Int VG_(cl_cmdline_fd);

/* Same as above, but for /proc/<pid>/auxv. */
extern Int VG_(cl_auxv_fd);

#if defined(VGO_solaris)
/* Same as above, but for /proc/<pid>/psinfo. */
extern Int VG_(cl_psinfo_fd);
#endif /* VGO_solaris */

// Client's original rlimit data and rlimit stack
extern struct vki_rlimit VG_(client_rlimit_data);
extern struct vki_rlimit VG_(client_rlimit_stack);

// Name of the launcher, as extracted from VALGRIND_LAUNCHER at
// startup.
extern HChar* VG_(name_of_launcher);

/* Application-visible file descriptor limits */
extern Int VG_(fd_soft_limit);
extern Int VG_(fd_hard_limit);

/* Useful addresses extracted from the client. */
/* Where is the freeres_wrapper routine we made? */
extern Addr VG_(client_freeres_wrapper);

/* x86-linux only: where is ld.so's _dl_sysinfo_int80 function?
   Finding it isn't essential, but knowing where it is does sometimes
   help produce better back traces.  See big comment in
   VG_(get_StackTrace) in m_stacktrace.c for further info. */
extern Addr VG_(client__dl_sysinfo_int80);

/* Obtains the initial client stack pointer from the finalised image info. */
extern Addr VG_(get_initial_client_SP)(void);

/* glibc nptl pthread systems only, when no-nptl-pthread-stackcache
   was given in --sim-hints.
   Used for a (kludgy) way to disable the cache of stacks as implemented in
   nptl glibc. 
   Based on internal knowledge of the pthread glibc nptl/allocatestack.c code:
   a huge value in stack_cache_actsize (bigger than the constant
   stack_cache_maxsize) makes glibc believes the cache is full
   and so stacks are always released when a pthread terminates.
   Several ugliness in this kludge:
    * hardcodes private glibc var name "stack_cache_maxsize"
    * based on knowledge of the code of the functions
      queue_stack and __free_stacks
    * static symbol for "stack_cache_maxsize" must be in
      the debug info.
   It would be much cleaner to have a documented and supported
   way to disable the pthread stack cache. */
extern SizeT* VG_(client__stack_cache_actsize__addr);
typedef const HChar* (*client__gnu_get_libc_version_type)(void);
extern client__gnu_get_libc_version_type VG_(client__gnu_get_libc_version_addr);

#if defined(VGO_solaris)
/* Address of variable vg_vfork_fildes in vgpreload_core.so.0
   (vg_preloaded.c). */
extern Int* VG_(vfork_fildes_addr);
#endif

#if defined(VGO_freebsd)
extern Bool VG_(have_slash_proc);
#endif

#endif   // __PUB_CORE_CLIENTSTATE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
