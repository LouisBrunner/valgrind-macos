
/*--------------------------------------------------------------------*/
/*--- A home for miscellaneous bits of information which pertain   ---*/
/*--- to the client's state.                                       ---*/
/*---                                              m_clientstate.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_threadstate.h"
#include "pub_core_vki.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"

/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Basic globals about the address space.                    ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Client address space, lowest to highest (see top of ume.c) */
// TODO: get rid of as many of these as possible.

/* ***Initial*** lowest address of the stack segment of the main thread.
   The main stack will grow if needed but VG_(clstk_start_base) will
   not be changed according to the growth. */
Addr  VG_(clstk_start_base)  = 0;
/* Initial highest address of the stack segment of the main thread. */
Addr  VG_(clstk_end)   = 0;
UWord VG_(clstk_id)    = NULL_STK_ID;
/* Maximum size of the main thread's client stack. */
SizeT VG_(clstk_max_size) = 0;

/* Solaris and Linux only, specifies where the client auxv is.

   This is set up as part of setup_client_stack() in
   initimg-{linux,solaris}.c. */
UWord* VG_(client_auxv) = NULL;

Addr  VG_(brk_base)    = 0;       /* start of brk */
Addr  VG_(brk_limit)   = 0;       /* current brk */

/* A fd which refers to the client executable. */
Int VG_(cl_exec_fd) = -1;

/* A fd which refers to the fake /proc/<pid>/cmdline in /tmp. */
Int VG_(cl_cmdline_fd) = -1;

/* A fd which refers to the fake /proc/<pid>/auxv in /tmp. */
Int VG_(cl_auxv_fd) = -1;

#if defined(VGO_solaris)
/* A fd which refers to the fake /proc/<pid>/psinfo in /tmp. */
Int VG_(cl_psinfo_fd) = -1;
#endif /* VGO_solaris */

// Command line pieces, after they have been extracted from argv in
// m_main.main().  The payload vectors are allocated in VG_AR_CORE
// (the default arena).  They are never freed.

/* Args for the client. */
XArray* /* of HChar* */ VG_(args_for_client) = NULL;

/* Args for V (augments, then those from the launcher). */
XArray* /* of HChar* */ VG_(args_for_valgrind) = NULL;

/* How many of the above not to pass on at execve time? */
Int VG_(args_for_valgrind_noexecpass) = 0;

/* The name of the client executable, as specified on the command
   line. */
const HChar* VG_(args_the_exename) = NULL;

/* The real name of the executable, with resolved
 * relative paths and symlinks */
const HChar* VG_(resolved_exename) = NULL;

// Client's original rlimit data and rlimit stack
struct vki_rlimit VG_(client_rlimit_data);
struct vki_rlimit VG_(client_rlimit_stack);

// Name of the launcher, as extracted from VALGRIND_LAUNCHER at
// startup.
HChar* VG_(name_of_launcher) = NULL;

/* Application-visible file descriptor limits */
Int VG_(fd_soft_limit) = -1;
Int VG_(fd_hard_limit) = -1;

/* Useful addresses extracted from the client. */
/* Where is the freeres_wrapper routine we made? */
Addr VG_(client_freeres_wrapper) = 0;

/* x86-linux only: where is glibc's _dl_sysinfo_int80 function?
   Finding it isn't essential, but knowing where it is does sometimes
   help produce better back traces.  See big comment in
   VG_(get_StackTrace) in m_stacktrace.c for further info. */
Addr VG_(client__dl_sysinfo_int80) = 0;

/* Address of the (internal) glibc nptl pthread stack cache size,
   declared as:
      static size_t stack_cache_actsize;
   in nptl/allocatestack.c */
SizeT* VG_(client__stack_cache_actsize__addr) = 0;

client__gnu_get_libc_version_type VG_(client__gnu_get_libc_version_addr) = 0;

#if defined(VGO_solaris)
/* Address of variable vg_vfork_fildes in vgpreload_core.so.0
   (vg_preloaded.c). */
Int* VG_(vfork_fildes_addr) = 0;
#endif

#if defined(VGO_freebsd)
Bool VG_(have_slash_proc) = False;
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
