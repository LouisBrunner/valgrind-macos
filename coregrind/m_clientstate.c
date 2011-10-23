
/*--------------------------------------------------------------------*/
/*--- A home for miscellaneous bits of information which pertain   ---*/
/*--- to the client's state.                                       ---*/
/*---                                              m_clientstate.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2011 Julian Seward 
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

#include "pub_core_basics.h"
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

Addr  VG_(client_base) = 0;       /* client address space limits */
Addr  VG_(client_end)  = 0;

Addr  VG_(clstk_base)  = 0;
Addr  VG_(clstk_end)   = 0;
UWord VG_(clstk_id)    = 0;

/* linux only: where is the client auxv ? */
/* This is set up as part of setup_client_stack in initimg-linux.c. */
UWord* VG_(client_auxv) = NULL;

Addr  VG_(brk_base)    = 0;       /* start of brk */
Addr  VG_(brk_limit)   = 0;       /* current brk */

/* A fd which refers to the client executable. */
Int VG_(cl_exec_fd) = -1;

/* A fd which refers to the fake /proc/<pid>/cmdline in /tmp. */
Int VG_(cl_cmdline_fd) = -1;

// Command line pieces, after they have been extracted from argv in
// m_main.main().  The payload vectors are allocated in VG_AR_TOOL
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

// Client's original rlimit data and rlimit stack
struct vki_rlimit VG_(client_rlimit_data);
struct vki_rlimit VG_(client_rlimit_stack);

// Name of the launcher, as extracted from VALGRIND_LAUNCHER at
// startup.
HChar* VG_(name_of_launcher) = NULL;

/* Application-visible file descriptor limits */
Int VG_(fd_soft_limit) = -1;
Int VG_(fd_hard_limit) = -1;

/* Useful addresses extracted from the client */
/* Where is the __libc_freeres_wrapper routine we made? */
Addr VG_(client___libc_freeres_wrapper) = 0;

/* x86-linux only: where is glibc's _dl_sysinfo_int80 function?
   Finding it isn't essential, but knowing where it is does sometimes
   help produce better back traces.  See big comment in
   VG_(get_StackTrace) in m_stacktrace.c for further info. */
Addr VG_(client__dl_sysinfo_int80) = 0;


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
