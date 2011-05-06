
/*--------------------------------------------------------------------*/
/*--- Command line options.                     pub_core_options.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward
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
#define VG_CLO_MAX_SFILES 100

/* The max number of --require-text-symbol= specification strings. */
#define VG_CLO_MAX_REQ_TSYMS 100

/* The max number of --fullpath-after= parameters. */
#define VG_CLO_MAX_FULLPATH_AFTER 100

/* Should we stop collecting errors if too many appear?  default: YES */
extern Bool  VG_(clo_error_limit);
/* Alternative exit code to hand to parent if errors were found.
   default: 0 (no, return the application's exit code in the normal
   way. */
extern Int   VG_(clo_error_exitcode);

typedef 
   enum { 
      Vg_VgdbNo,   // Do not activate gdbserver.
      Vg_VgdbYes,  // Activate gdbserver (default).
      Vg_VgdbFull, // ACtivate gdbserver in full mode, allowing
                   // a precise handling of watchpoints and single stepping
                   // at any moment.
   } 
   VgVgdb;
/* if != Vg_VgdbNo, allows valgrind to serve vgdb/gdb. */
extern VgVgdb VG_(clo_vgdb);
/* if > 0, checks every VG_(clo_vgdb_poll) BBS if vgdb wants to be served. */
extern Int VG_(clo_vgdb_poll);
/* prefix for the named pipes (FIFOs) used by vgdb/gdb to communicate with valgrind */
extern Char* VG_(clo_vgdb_prefix);
/* if True, gdbserver in valgrind will expose a target description containing
   shadow registers */
extern Bool  VG_(clo_vgdb_shadow_registers);
#define VG_CLO_VGDB_PREFIX_DEFAULT "/tmp/vgdb-pipe"

/* Enquire about whether to attach to a debugger at errors?   default: NO */
extern Bool  VG_(clo_db_attach);
/* The debugger command?  default: whatever gdb ./configure found */
extern Char* VG_(clo_db_command);
/* Generating a suppression for each error?   default: 0 (NO)
   Other values: 1 (yes, but ask user), 2 (yes, don't ask user) */
extern Int   VG_(clo_gen_suppressions);
/* Sanity-check level: 0 = none, 1 (default), > 1 = expensive. */
extern Int   VG_(clo_sanity_level);
/* Automatically attempt to demangle C++ names?  default: YES */
extern Bool  VG_(clo_demangle);
/* Simulate child processes? default: NO */
extern Bool  VG_(clo_trace_children);
/* String containing comma-separated patterns for executable names
   that should not be traced into even when --trace-children=yes */
extern HChar* VG_(clo_trace_children_skip);
/* The same as VG_(clo_trace_children), except that these patterns are
   tested against the arguments for child processes, rather than the
   executable name. */
extern HChar* VG_(clo_trace_children_skip_by_arg);
/* After a fork, the child's output can become confusingly
   intermingled with the parent's output.  This is especially
   problematic when VG_(clo_xml) is True.  Setting
   VG_(clo_child_silent_after_fork) causes children to fall silent
   after fork() calls.  Although note they become un-silent again
   after the subsequent exec(). */
extern Bool  VG_(clo_child_silent_after_fork);

/* If the user specified --log-file=STR and/or --xml-file=STR, these
   hold STR after expansion of the %p and %q templates. */
extern Char* VG_(clo_log_fname_expanded);
extern Char* VG_(clo_xml_fname_expanded);

/* Add timestamps to log messages?  default: NO */
extern Bool  VG_(clo_time_stamp);

/* The file descriptor to read for input.  default: 0 == stdin */
extern Int   VG_(clo_input_fd);

/* The number of suppression files specified. */
extern Int   VG_(clo_n_suppressions);
/* The names of the suppression files. */
extern Char* VG_(clo_suppressions)[VG_CLO_MAX_SFILES];

/* An array of strings harvested from --fullpath-after= flags. */
extern Int   VG_(clo_n_fullpath_after);
extern Char* VG_(clo_fullpath_after)[VG_CLO_MAX_FULLPATH_AFTER];

/* DEBUG: print generated code?  default: 00000000 ( == NO ) */
extern UChar VG_(clo_trace_flags);
/* DEBUG: do bb profiling?  default: 00000000 ( == NO ) */
extern UChar VG_(clo_profile_flags);
/* DEBUG: if tracing codegen, be quiet until after this bb ( 0 ) */
extern Int   VG_(clo_trace_notbelow);
/* DEBUG: print system calls?  default: NO */
extern Bool  VG_(clo_trace_syscalls);
/* DEBUG: print signal details?  default: NO */
extern Bool  VG_(clo_trace_signals);
/* DEBUG: print symtab details?  default: NO */
extern Bool  VG_(clo_trace_symtab);
/* DEBUG: restrict symtab etc details to object name pattern.  Default: "*" */
extern HChar* VG_(clo_trace_symtab_patt);
/* DEBUG: print call-frame-info details?  default: NO */
extern Bool  VG_(clo_trace_cfi);
/* DEBUG:  mimic /usr/bin/readelf --syms?  default: NO */
extern Bool  VG_(clo_debug_dump_syms);
/* DEBUG: mimic /usr/bin/readelf --debug-dump=line?  default: NO */
extern Bool  VG_(clo_debug_dump_line);
/* DEBUG: mimic  /usr/bin/readelf --debug-dump=frames?  default: NO */
extern Bool  VG_(clo_debug_dump_frames);
/* DEBUG: print redirection details?  default: NO */
extern Bool  VG_(clo_trace_redir);
/* DEBUG: print thread scheduling events?  default: NO */
extern Bool  VG_(clo_trace_sched);
/* DEBUG: do heap profiling?  default: NO */
extern Bool  VG_(clo_profile_heap);
/* DEBUG: display gory details for the k'th most popular error.
   default: Infinity. */
extern Int   VG_(clo_dump_error);
/* Engage miscellaneous weird hacks needed for some progs. */
extern Char* VG_(clo_sim_hints);
/* Show symbols in the form 'name+offset' ?  Default: NO */
extern Bool VG_(clo_sym_offsets);
/* Read DWARF3 variable info even if tool doesn't ask for it? */
extern Bool VG_(clo_read_var_info);
/* Which prefix to strip from full source file paths, if any. */
extern Char* VG_(clo_prefix_to_strip);

/* An array of strings harvested from --require-text-symbol= 
   flags.

   Each string specifies a pair: a soname pattern and a text symbol
   name pattern, separated by a colon.  The patterns can be written
   using the normal "?" and "*" wildcards.  For example:
   ":*libc.so*:foo?bar".

   These flags take effect when reading debuginfo from objects.  If an
   object is loaded and the object's soname matches the soname
   component of one of the specified pairs, then Valgrind will examine
   all the text symbol names in the object.  If none of them match the
   symbol name component of that same specification, then the run is
   aborted, with an error message.

   The purpose of this is to support reliable usage of marked-up
   libraries.  For example, suppose we have a version of GCC's
   libgomp.so which has been marked up with annotations to support
   Helgrind.  It is only too easy and confusing to load the 'wrong'
   libgomp.so into the application.  So the idea is: add a text symbol
   in the marked-up library (eg), "annotated_for_helgrind_3_6", and
   then give the flag

     --require-text-symbol=:*libgomp*so*:annotated_for_helgrind_3_6

   so that when libgomp.so is loaded, we scan the symbol table, and if
   the symbol isn't present the run is aborted, rather than continuing
   silently with the un-marked-up library.  Note that you should put
   the entire flag in quotes to stop shells messing up the * and ?
   wildcards. */
extern Int    VG_(clo_n_req_tsyms);
extern HChar* VG_(clo_req_tsyms)[VG_CLO_MAX_REQ_TSYMS];

/* Track open file descriptors? */
extern Bool  VG_(clo_track_fds);

/* Should we run __libc_freeres at exit?  Sometimes causes crashes.
   Default: YES.  Note this is subservient to VG_(needs).libc_freeres;
   if the latter says False, then the setting of VG_(clo_run_libc_freeres)
   is ignored.  Ie if a tool says no, I don't want this to run, that
   cannot be overridden from the command line. */
extern Bool  VG_(clo_run_libc_freeres);

/* Should we show VEX emulation warnings?  Default: NO */
extern Bool VG_(clo_show_emwarns);

/* How much does the stack pointer have to change before tools
   consider a stack switch to have happened?  Default: 2000000 bytes
   NB: must be host-word-sized to be correct (hence Word). */
extern Word VG_(clo_max_stackframe);
/* How large should Valgrind allow the primary thread's guest stack to
   be? */
extern Word VG_(clo_main_stacksize);

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

/* Darwin-specific: automatically run /usr/bin/dsymutil to update
   .dSYM directories as necessary? */
extern Bool VG_(clo_dsymutil);

/* Should we trace into this child executable (across execve etc) ?
   This involves considering --trace-children=,
   --trace-children-skip=, --trace-children-skip-by-arg=, and the name
   of the executable.  'child_argv' must not include the name of the
   executable itself; iow child_argv[0] must be the first arg, if any,
   for the child. */
extern Bool VG_(should_we_trace_this_child) ( HChar* child_exe_name,
                                              HChar** child_argv );

#endif   // __PUB_CORE_OPTIONS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
