
/*--------------------------------------------------------------------*/
/*--- Command line options.                     pub_core_options.h ---*/
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

#ifndef __PUB_CORE_OPTIONS_H
#define __PUB_CORE_OPTIONS_H

//--------------------------------------------------------------------
// PURPOSE: This module holds the variables for all command line options,
// plus some functions and macros for manipulating them.  Almost every
// other module imports this one, if only for VG_(clo_verbosity).
//--------------------------------------------------------------------

#include "pub_tool_options.h"

/* The max number of suppression files. */
#define VG_CLO_MAX_SFILES 10

/* Should we stop collecting errors if too many appear?  default: YES */
extern Bool  VG_(clo_error_limit);
/* Enquire about whether to attach to a debugger at errors?   default: NO */
extern Bool  VG_(clo_db_attach);
/* The debugger command?  default: whatever gdb ./configure found */
extern Char* VG_(clo_db_command);
/* Generating a suppression for each error?   default: 0 (NO)
   Other values: 1 (yes, but ask user), 2 (yes, don't ask user) */
extern Int  VG_(clo_gen_suppressions);
/* Sanity-check level: 0 = none, 1 (default), > 1 = expensive. */
extern Int   VG_(clo_sanity_level);
/* Automatically attempt to demangle C++ names?  default: YES */
extern Bool  VG_(clo_demangle);
/* Simulate child processes? default: NO */
extern Bool  VG_(clo_trace_children);

/* Where logging output is to be sent to.

   With --log-fd (and by default), clo_log_fd holds the file id, and is
   taken from the command line.  (fd 2, stderr, is the default.)
   clo_log_name is irrelevant.

   With --log-file/--log-file-exactly, clo_log_name holds the log-file
   name, and is taken from the command line.  clo_log_fd is then
   made to hold the relevant file id, by opening clo_log_name
   (concatenated with the process ID) for writing.

   With --log-file, there is an additional twist: if
   clo_log_file_qualifier is non-NULL, the contents of the environment
   variable specified by clo_log_file_qualifier is incorporated into
   the logfile name.  This is useful in that it allows the logfile
   name to incorporate environmental information.

   With --log-socket, clo_log_name holds the hostname:portnumber pair,
   and is taken from the command line.  clo_log_fd is then made to hold
   the relevant file handle, by opening a connection to that
   hostname:portnumber pair. 

   Global default is to set log_to == VgLogTo_Fd and log_fd == 2
   (stderr). */
extern Int   VG_(clo_log_fd);
extern Char* VG_(clo_log_name);
extern Char* VG_(clo_log_file_qualifier);

/* Add timestamps to log messages?  default: NO */
extern Bool  VG_(clo_time_stamp);

/* The file descriptor to read for input.  default: 0 == stdin */
extern Int   VG_(clo_input_fd);
/* The number of suppression files specified. */
extern Int   VG_(clo_n_suppressions);
/* The names of the suppression files. */
extern Char* VG_(clo_suppressions)[VG_CLO_MAX_SFILES];

/* DEBUG: print generated code?  default: 00000000 ( == NO ) */
extern Bool  VG_(clo_trace_flags);
/* DEBUG: do bb profiling?  default: 00000000 ( == NO ) */
extern Bool  VG_(clo_profile_flags);
/* DEBUG: if tracing codegen, be quiet until after this bb ( 0 ) */
extern Int   VG_(clo_trace_notbelow);
/* DEBUG: print system calls?  default: NO */
extern Bool  VG_(clo_trace_syscalls);
/* DEBUG: print signal details?  default: NO */
extern Bool  VG_(clo_trace_signals);
/* DEBUG: print symtab details?  default: NO */
extern Bool  VG_(clo_trace_symtab);
/* DEBUG: print call-frame-info details?  default: NO */
extern Bool  VG_(clo_trace_cfi);
/* DEBUG: print redirection details?  default: NO */
extern Bool  VG_(clo_trace_redir);
/* DEBUG: print thread scheduling events?  default: NO */
extern Bool  VG_(clo_trace_sched);
/* DEBUG: print pthreads calls?  default: NO */
extern Bool  VG_(clo_trace_pthreads);
/* Display gory details for the k'th most popular error.  default:
   Infinity. */
extern Int   VG_(clo_dump_error);
/* Number of parents of a backtrace.  Default: 8.  */
extern Int   VG_(clo_backtrace_size);
/* Engage miscellaneous weird hacks needed for some progs. */
extern Char* VG_(clo_weird_hacks);

/* Track open file descriptors? */
extern Bool  VG_(clo_track_fds);

/* Should we run __libc_freeres at exit?  Sometimes causes crashes.
   Default: YES.  Note this is subservient to VG_(needs).libc_freeres;
   if the latter says False, then the setting of VG_(clo_weird_hacks)
   is ignored.  Ie if a tool says no, I don't want this to run, that
   cannot be overridden from the command line. */
extern Bool  VG_(clo_run_libc_freeres);
/* Continue stack traces below main()?  Default: NO */
extern Bool VG_(clo_show_below_main);
/* Test each client pointer dereference to check it's within the
   client address space bounds */
extern Bool VG_(clo_pointercheck);
/* Model the pthread library */
extern Bool VG_(clo_model_pthreads);

/* Should we show VEX emulation warnings?  Default: NO */
extern Bool VG_(clo_show_emwarns);

/* How much does the stack pointer have to change before tools
   consider a stack switch to have happened?  Default: 2000000 bytes */
extern Int VG_(clo_max_stackframe);

/* Delay startup to allow GDB to be attached?  Default: NO */
extern Bool VG_(clo_wait_for_gdb);

/* To what extent should self-checking translations be made?  These
   are needed to deal with self-modifying code on uncooperative
   platforms. */
typedef 
   enum { 
      Vg_SmcNone,  // never generate self-checking translations
      Vg_SmcStack, // generate s-c-t's for code found in stacks
                   // (this is the default)
      Vg_SmcAll    // make all translations self-checking.
   } 
   VgSmc;

/* Describe extent to which self-modifying-code should be
   auto-detected. */
extern VgSmc VG_(clo_smc_check);

/* String containing comma-separated names of minor kernel variants,
   so they can be properly handled by m_syswrap. */
extern HChar* VG_(clo_kernel_variant);

#endif   // __PUB_CORE_OPTIONS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
