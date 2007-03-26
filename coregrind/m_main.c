
/*--------------------------------------------------------------------*/
/*--- Startup: the real stuff                             m_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Julian Seward 
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
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_commandline.h"
#include "pub_core_debuglog.h"
#include "pub_core_errormgr.h"
#include "pub_core_execontext.h"
#include "pub_core_initimg.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_syscall.h"       // VG_(strerror)
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_debuginfo.h"
#include "pub_core_redir.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_stacks.h"        // For VG_(register_stack)
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_translate.h"     // For VG_(translate)
#include "pub_core_trampoline.h"
#include "pub_core_transtab.h"

/* Stuff for reading AIX5 /proc/<pid>/sysent files */
#if defined(VGO_aix5)
   /* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#  include <sys/procfs.h>  /* prsysent_t */
   /* --- !!! --- EXTERNAL HEADERS end --- !!! --- */
#  define VG_AIX5_SYSENT_SIZE 100000
   static UChar aix5_sysent_buf[VG_AIX5_SYSENT_SIZE];
#endif


/*====================================================================*/
/*=== Counters, for profiling purposes only                        ===*/
/*====================================================================*/

static void print_all_stats ( void )
{
   VG_(print_translation_stats)();
   VG_(print_tt_tc_stats)();
   VG_(print_scheduler_stats)();
   VG_(print_ExeContext_stats)();
   VG_(print_errormgr_stats)();

   // Memory stats
   if (VG_(clo_verbosity) > 2) {
      VG_(message)(Vg_DebugMsg, "");
      VG_(message)(Vg_DebugMsg, 
         "------ Valgrind's internal memory use stats follow ------" );
      VG_(sanity_check_malloc_all)();
      VG_(message)(Vg_DebugMsg, "------" );
      VG_(print_all_arena_stats)();
      VG_(message)(Vg_DebugMsg, "");
   }
}


/*====================================================================*/
/*=== Command-line: variables, processing, etc                     ===*/
/*====================================================================*/

// See pub_{core,tool}_options.h for explanations of all these.

static void usage_NORETURN ( Bool debug_help )
{
   Char* usage1 = 
"usage: valgrind [options] prog-and-args\n"
"\n"
"  common user options for all Valgrind tools, with defaults in [ ]:\n"
"    --tool=<name>             use the Valgrind tool named <name> [memcheck]\n"
"    -h --help                 show this message\n"
"    --help-debug              show this message, plus debugging options\n"
"    --version                 show version\n"
"    -q --quiet                run silently; only print error msgs\n"
"    -v --verbose              be more verbose, incl counts of errors\n"
"    --trace-children=no|yes   Valgrind-ise child processes? [no]\n"
"    --track-fds=no|yes        track open file descriptors? [no]\n"
"    --time-stamp=no|yes       add timestamps to log messages? [no]\n"
"    --log-fd=<number>         log messages to file descriptor [2=stderr]\n"
"    --log-file=<file>         log messages to <file>.<pid>\n"
"    --log-file-exactly=<file> log messages to <file>\n"
"    --log-file-qualifier=<VAR> incorporate $VAR in logfile name [none]\n"
"    --log-socket=ipaddr:port  log messages to socket ipaddr:port\n"
"\n"
"  uncommon user options for all Valgrind tools:\n"
"    --run-libc-freeres=no|yes free up glibc memory at exit? [yes]\n"
"    --sim-hints=hint1,hint2,...  known hints:\n"
"                                 lax-ioctls, enable-outer [none]\n"
"    --show-emwarns=no|yes     show warnings about emulation limits? [no]\n"
"    --smc-check=none|stack|all  checks for self-modifying code: none,\n"
"                              only for code found in stacks, or all [stack]\n"
"    --kernel-variant=variant1,variant2,...  known variants: bproc [none]\n"
"                              handle non-standard kernel variants\n"
"\n"
"  user options for Valgrind tools that report errors:\n"
"    --xml=yes                 all output is in XML (some tools only)\n"
"    --xml-user-comment=STR    copy STR verbatim to XML output\n"
"    --demangle=no|yes         automatically demangle C++ names? [yes]\n"
"    --num-callers=<number>    show <number> callers in stack traces [12]\n"
"    --error-limit=no|yes      stop showing new errors if too many? [yes]\n"
"    --error-exitcode=<number> exit code to return if errors found [0=disable]\n"
"    --show-below-main=no|yes  continue stack traces below main() [no]\n"
"    --suppressions=<filename> suppress errors described in <filename>\n"
"    --gen-suppressions=no|yes|all    print suppressions for errors? [no]\n"
"    --db-attach=no|yes        start debugger when errors detected? [no]\n"
"    --db-command=<command>    command to start debugger [gdb -nw %%f %%p]\n"
"    --input-fd=<number>       file descriptor for input [0=stdin]\n"
"    --max-stackframe=<number> assume stack switch for SP changes larger\n"
"                              than <number> bytes [2000000]\n"
"\n";

   Char* usage2 = 
"\n"
"  debugging options for all Valgrind tools:\n"
"    --sanity-level=<number>   level of sanity checking to do [1]\n"
"    --trace-flags=<XXXXXXXX>   show generated code? (X = 0|1) [00000000]\n"
"    --profile-flags=<XXXXXXXX> ditto, but for profiling (X = 0|1) [00000000]\n"
"    --trace-notbelow=<number> only show BBs above <number> [999999999]\n"
"    --trace-syscalls=no|yes   show all system calls? [no]\n"
"    --trace-signals=no|yes    show signal handling details? [no]\n"
"    --trace-symtab=no|yes     show symbol table details? [no]\n"
"    --trace-symtab-patt=<patt> limit debuginfo tracing to obj name <patt>\n"
"    --trace-cfi=no|yes        show call-frame-info details? [no]\n"
"    --debug-dump=syms         mimic /usr/bin/readelf --syms\n"
"    --debug-dump=line         mimic /usr/bin/readelf --debug-dump=line\n"
"    --debug-dump=frames       mimic /usr/bin/readelf --debug-dump=frames\n"
"    --trace-redir=no|yes      show redirection details? [no]\n"
"    --trace-sched=no|yes      show thread scheduler details? [no]\n"
"    --wait-for-gdb=yes|no     pause on startup to wait for gdb attach\n"
"    --sym-offsets=yes|no      show syms in form 'name+offset' ? [no]\n"
#if 0
"    --model-pthreads=yes|no   model the pthreads library [no]\n"
#endif
"    --command-line-only=no|yes  only use command line options [no]\n"
"\n"
"    --vex-iropt-verbosity             0 .. 9 [0]\n"
"    --vex-iropt-level                 0 .. 2 [2]\n"
"    --vex-iropt-precise-memory-exns   [no]\n"
"    --vex-iropt-unroll-thresh         0 .. 400 [120]\n"
"    --vex-guest-max-insns             1 .. 100 [50]\n"
"    --vex-guest-chase-thresh          0 .. 99  [10]\n"
"\n"
"    --trace-flags and --profile-flags values (omit the middle space):\n"
"       1000 0000   show conversion into IR\n"
"       0100 0000   show after initial opt\n"
"       0010 0000   show after instrumentation\n"
"       0001 0000   show after second opt\n"
"       0000 1000   show after tree building\n"
"       0000 0100   show selecting insns\n"
"       0000 0010   show after reg-alloc\n"
"       0000 0001   show final assembly\n"
"      (Nb: you need --trace-notbelow with --trace-flags for full details)\n"
"\n"
"  debugging options for Valgrind tools that report errors\n"
"    --dump-error=<number>     show translation for basic block associated\n"
"                              with <number>'th error context [0=show none]\n"
"\n";

   Char* usage3 =
"\n"
"  Extra options read from ~/.valgrindrc, $VALGRIND_OPTS, ./.valgrindrc\n"
"\n"
"  Valgrind is Copyright (C) 2000-2007 Julian Seward et al.\n"
"  and licensed under the GNU General Public License, version 2.\n"
"  Bug reports, feedback, admiration, abuse, etc, to: %s.\n"
"\n"
"  Tools are copyright and licensed by their authors.  See each\n"
"  tool's start-up message for more information.\n"
"\n";

   // Ensure the message goes to stdout
   VG_(clo_log_fd) = 1;
   vg_assert( !VG_(logging_to_socket) );

   VG_(printf)(usage1);
   if (VG_(details).name) {
      VG_(printf)("  user options for %s:\n", VG_(details).name);
      if (VG_(needs).command_line_options)
	 VG_TDICT_CALL(tool_print_usage);
      else
	 VG_(printf)("    (none)\n");
   }
   if (debug_help) {
      VG_(printf)(usage2);

      if (VG_(details).name) {
         VG_(printf)("  debugging options for %s:\n", VG_(details).name);
      
         if (VG_(needs).command_line_options)
            VG_TDICT_CALL(tool_print_debug_usage);
         else
            VG_(printf)("    (none)\n");
      }
   }
   VG_(printf)(usage3, VG_BUGS_TO);
   VG_(exit)(0);
}


/* Peer at previously set up VG_(args_for_valgrind) and extract any
   request for help and also the tool name. */

static void get_helprequest_and_toolname ( Int* need_help, HChar** tool )
{
   UInt   i;
   HChar* str;

   vg_assert( VG_(args_for_valgrind) );

   /* parse the options we have (only the options we care about now) */
   for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {

      str = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      vg_assert(str);

      if (VG_STREQ(str, "--version")) {
         // Ensure the version string goes to stdout
         VG_(clo_log_fd) = 1;
         VG_(printf)("valgrind-" VERSION "\n");
         VG_(exit)(0);

      } else if (VG_CLO_STREQ(str, "--help") ||
                 VG_CLO_STREQ(str, "-h")) {
         *need_help = 1;

      } else if (VG_CLO_STREQ(str, "--help-debug")) {
         *need_help = 2;

      // The tool has already been determined, but we need to know the name
      // here.
      } else if (VG_CLO_STREQN(7, str, "--tool=")) {
         *tool = &str[7];
      }
   }
}

static Bool process_cmd_line_options( UInt* client_auxv, const char* toolname )
{
   // VG_(clo_log_fd) is used by all the messaging.  It starts as 2 (stderr)
   // and we cannot change it until we know what we are changing it to is
   // ok.  So we have tmp_log_fd to hold the tmp fd prior to that point.
   SysRes sres;
   Int    i, tmp_log_fd;
   Int    toolname_len = VG_(strlen)(toolname);
   enum {
      VgLogTo_Fd,
      VgLogTo_File,
      VgLogTo_FileExactly,
      VgLogTo_Socket
   } log_to = VgLogTo_Fd;   // Where is logging output to be sent?

   /* log to stderr by default, but usage message goes to stdout */
   tmp_log_fd = 2; 

   /* Check for sane path in ./configure --prefix=... */
   if (VG_LIBDIR[0] != '/') 
      VG_(err_config_error)("Please use absolute paths in "
                            "./configure --prefix=... or --libdir=...");

   vg_assert( VG_(args_for_valgrind) );

   for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {

      HChar* arg   = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      HChar* colon = arg;

      // Look for a colon in the option name.
      while (*colon && *colon != ':' && *colon != '=')
         colon++;

      // Does it have the form "--toolname:foo"?  We have to do it at the start
      // in case someone has combined a prefix with a core-specific option,
      // eg.  "--memcheck:verbose".
      if (*colon == ':') {
         if (VG_CLO_STREQN(2,            arg,                "--") && 
             VG_CLO_STREQN(toolname_len, arg+2,              toolname) &&
             VG_CLO_STREQN(1,            arg+2+toolname_len, ":"))
         {
            // Prefix matches, convert "--toolname:foo" to "--foo".
            // Two things to note:
            // - We cannot modify the option in-place.  If we did, and then
            //   a child was spawned with --trace-children=yes, the
            //   now-non-prefixed option would be passed and could screw up
            //   the child.
            // - We create copies, and never free them.  Why?  Non-prefixed
            //   options hang around forever, so tools need not make copies
            //   of strings within them.  We need to have the same behaviour
            //   for prefixed options.  The pointer to the copy will be lost
            //   once we leave this function (although a tool may keep a
            //   pointer into it), but the space wasted is insignificant.
            //   (In bug #142197, the copies were being freed, which caused
            //   problems for tools that reasonably assumed that arguments
            //   wouldn't disappear on them.)
            if (0)
               VG_(printf)("tool-specific arg: %s\n", arg);
            arg = VG_(strdup)(arg + toolname_len + 1);
            arg[0] = '-';
            arg[1] = '-';

         } else {
            // prefix doesn't match, skip to next arg
            continue;
         }
      }
      
      /* Ignore these options - they've already been handled */
      if      (VG_CLO_STREQN( 7, arg, "--tool="))              { }
      else if (VG_CLO_STREQN(20, arg, "--command-line-only=")) { }
      else if (VG_CLO_STREQ(arg, "--"))                        { }
      else if (VG_CLO_STREQ(arg, "-d"))                        { }

      else if (VG_CLO_STREQ(arg, "-v") ||
               VG_CLO_STREQ(arg, "--verbose"))
         VG_(clo_verbosity)++;

      else if (VG_CLO_STREQ(arg, "-q") ||
               VG_CLO_STREQ(arg, "--quiet"))
         VG_(clo_verbosity)--;

      else VG_BOOL_CLO(arg, "--xml",              VG_(clo_xml))
      else VG_BOOL_CLO(arg, "--db-attach",        VG_(clo_db_attach))
      else VG_BOOL_CLO(arg, "--demangle",         VG_(clo_demangle))
      else VG_BOOL_CLO(arg, "--error-limit",      VG_(clo_error_limit))
      else VG_NUM_CLO (arg, "--error-exitcode",   VG_(clo_error_exitcode))
      else VG_BOOL_CLO(arg, "--show-emwarns",     VG_(clo_show_emwarns))
      else VG_NUM_CLO (arg, "--max-stackframe",   VG_(clo_max_stackframe))
      else VG_BOOL_CLO(arg, "--run-libc-freeres", VG_(clo_run_libc_freeres))
      else VG_BOOL_CLO(arg, "--show-below-main",  VG_(clo_show_below_main))
      else VG_BOOL_CLO(arg, "--time-stamp",       VG_(clo_time_stamp))
      else VG_BOOL_CLO(arg, "--track-fds",        VG_(clo_track_fds))
      else VG_BOOL_CLO(arg, "--trace-children",   VG_(clo_trace_children))
      else VG_BOOL_CLO(arg, "--trace-sched",      VG_(clo_trace_sched))
      else VG_BOOL_CLO(arg, "--trace-signals",    VG_(clo_trace_signals))
      else VG_BOOL_CLO(arg, "--trace-symtab",     VG_(clo_trace_symtab))
      else VG_STR_CLO (arg, "--trace-symtab-patt", VG_(clo_trace_symtab_patt))
      else VG_BOOL_CLO(arg, "--trace-cfi",        VG_(clo_trace_cfi))
      else VG_XACT_CLO(arg, "--debug-dump=syms",  VG_(clo_debug_dump_syms))
      else VG_XACT_CLO(arg, "--debug-dump=line",  VG_(clo_debug_dump_line))
      else VG_XACT_CLO(arg, "--debug-dump=frames", VG_(clo_debug_dump_frames))
      else VG_BOOL_CLO(arg, "--trace-redir",      VG_(clo_trace_redir))

      else VG_BOOL_CLO(arg, "--trace-syscalls",   VG_(clo_trace_syscalls))
      else VG_BOOL_CLO(arg, "--trace-pthreads",   VG_(clo_trace_pthreads))
      else VG_BOOL_CLO(arg, "--wait-for-gdb",     VG_(clo_wait_for_gdb))
      else VG_STR_CLO (arg, "--db-command",       VG_(clo_db_command))
      else VG_STR_CLO (arg, "--sim-hints",        VG_(clo_sim_hints))
      else VG_BOOL_CLO(arg, "--sym-offsets",      VG_(clo_sym_offsets))

      else VG_NUM_CLO (arg, "--dump-error",       VG_(clo_dump_error))
      else VG_NUM_CLO (arg, "--input-fd",         VG_(clo_input_fd))
      else VG_NUM_CLO (arg, "--sanity-level",     VG_(clo_sanity_level))
      else VG_BNUM_CLO(arg, "--num-callers",      VG_(clo_backtrace_size), 1,
                                                  VG_DEEPEST_BACKTRACE)

      else if (VG_CLO_STREQ(arg, "--smc-check=none"))
         VG_(clo_smc_check) = Vg_SmcNone;
      else if (VG_CLO_STREQ(arg, "--smc-check=stack"))
         VG_(clo_smc_check) = Vg_SmcStack;
      else if (VG_CLO_STREQ(arg, "--smc-check=all"))
         VG_(clo_smc_check) = Vg_SmcAll;

      else VG_STR_CLO (arg, "--kernel-variant",   VG_(clo_kernel_variant))

      else VG_BNUM_CLO(arg, "--vex-iropt-verbosity",
                       VG_(clo_vex_control).iropt_verbosity, 0, 10)
      else VG_BNUM_CLO(arg, "--vex-iropt-level",
                       VG_(clo_vex_control).iropt_level, 0, 2)
      else VG_BOOL_CLO(arg, "--vex-iropt-precise-memory-exns",
                       VG_(clo_vex_control).iropt_precise_memory_exns)
      else VG_BNUM_CLO(arg, "--vex-iropt-unroll-thresh",
                       VG_(clo_vex_control).iropt_unroll_thresh, 0, 400)
      else VG_BNUM_CLO(arg, "--vex-guest-max-insns",
                       VG_(clo_vex_control).guest_max_insns, 1, 100)
      else VG_BNUM_CLO(arg, "--vex-guest-chase-thresh",
                       VG_(clo_vex_control).guest_chase_thresh, 0, 99)

      else if (VG_CLO_STREQN(9,  arg, "--log-fd=")) {
         log_to            = VgLogTo_Fd;
         VG_(clo_log_name) = NULL;
         tmp_log_fd        = (Int)VG_(atoll)(&arg[9]);
      }

      else if (VG_CLO_STREQN(11, arg, "--log-file=")) {
         log_to            = VgLogTo_File;
         VG_(clo_log_name) = &arg[11];
      }

      else if (VG_CLO_STREQN(21, arg, "--log-file-qualifier=")) {
         VG_(clo_log_file_qualifier) = &arg[21];
      }

      else if (VG_CLO_STREQN(19, arg, "--log-file-exactly=")) {
         log_to            = VgLogTo_FileExactly;
         VG_(clo_log_name) = &arg[19];
      }

      else if (VG_CLO_STREQN(13, arg, "--log-socket=")) {
         log_to            = VgLogTo_Socket;
         VG_(clo_log_name) = &arg[13];
      }

      else if (VG_CLO_STREQN(19, arg, "--xml-user-comment=")) {
         VG_(clo_xml_user_comment) = &arg[19];
      }

      else if (VG_CLO_STREQN(15, arg, "--suppressions=")) {
         if (VG_(clo_n_suppressions) >= VG_CLO_MAX_SFILES) {
            VG_(message)(Vg_UserMsg, "Too many suppression files specified.");
            VG_(message)(Vg_UserMsg, 
                         "Increase VG_CLO_MAX_SFILES and recompile.");
            VG_(err_bad_option)(arg);
         }
         VG_(clo_suppressions)[VG_(clo_n_suppressions)] = &arg[15];
         VG_(clo_n_suppressions)++;
      }

      /* "stuvwxyz" --> stuvwxyz (binary) */
      else if (VG_CLO_STREQN(14, arg, "--trace-flags=")) {
         Int j;
         char* opt = & arg[14];
   
         if (8 != VG_(strlen)(opt)) {
            VG_(message)(Vg_UserMsg, 
                         "--trace-flags argument must have 8 digits");
            VG_(err_bad_option)(arg);
         }
         for (j = 0; j < 8; j++) {
            if      ('0' == opt[j]) { /* do nothing */ }
            else if ('1' == opt[j]) VG_(clo_trace_flags) |= (1 << (7-j));
            else {
               VG_(message)(Vg_UserMsg, "--trace-flags argument can only "
                                        "contain 0s and 1s");
               VG_(err_bad_option)(arg);
            }
         }
      }

      /* "stuvwxyz" --> stuvwxyz (binary) */
      else if (VG_CLO_STREQN(16, arg, "--profile-flags=")) {
         Int j;
         char* opt = & arg[16];
   
         if (8 != VG_(strlen)(opt)) {
            VG_(message)(Vg_UserMsg, 
                         "--profile-flags argument must have 8 digits");
            VG_(err_bad_option)(arg);
         }
         for (j = 0; j < 8; j++) {
            if      ('0' == opt[j]) { /* do nothing */ }
            else if ('1' == opt[j]) VG_(clo_profile_flags) |= (1 << (7-j));
            else {
               VG_(message)(Vg_UserMsg, "--profile-flags argument can only "
                                        "contain 0s and 1s");
               VG_(err_bad_option)(arg);
            }
         }
      }

      else VG_NUM_CLO (arg, "--trace-notbelow",   VG_(clo_trace_notbelow))

      else if (VG_CLO_STREQ(arg, "--gen-suppressions=no"))
         VG_(clo_gen_suppressions) = 0;
      else if (VG_CLO_STREQ(arg, "--gen-suppressions=yes"))
         VG_(clo_gen_suppressions) = 1;
      else if (VG_CLO_STREQ(arg, "--gen-suppressions=all"))
         VG_(clo_gen_suppressions) = 2;

      else if ( ! VG_(needs).command_line_options
             || ! VG_TDICT_CALL(tool_process_cmd_line_option, arg) ) {
         VG_(err_bad_option)(arg);
      }
   }

   /* Make VEX control parameters sane */

   if (VG_(clo_vex_control).guest_chase_thresh
       >= VG_(clo_vex_control).guest_max_insns)
      VG_(clo_vex_control).guest_chase_thresh
         = VG_(clo_vex_control).guest_max_insns - 1;

   if (VG_(clo_vex_control).guest_chase_thresh < 0)
      VG_(clo_vex_control).guest_chase_thresh = 0;

   /* Check various option values */

   if (VG_(clo_verbosity) < 0)
      VG_(clo_verbosity) = 0;

   if (VG_(clo_db_attach) && VG_(clo_trace_children)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, 
         "--db-attach=yes conflicts with --trace-children=yes");
      VG_(message)(Vg_UserMsg, 
         "Please choose one or the other, but not both.");
      VG_(err_bad_option)("--db-attach=yes and --trace-children=yes");
   }

   if (VG_(clo_gen_suppressions) > 0 && 
       !VG_(needs).core_errors && !VG_(needs).tool_errors) {
      VG_(message)(Vg_UserMsg, 
                   "Can't use --gen-suppressions= with this tool,");
      VG_(message)(Vg_UserMsg, 
                   "as it doesn't generate errors.");
      VG_(err_bad_option)("--gen-suppressions=");
   }

   /* If we've been asked to emit XML, mash around various other
      options so as to constrain the output somewhat, and to remove
      any need for user input during the run. */
   if (VG_(clo_xml)) {
      /* Disable suppression generation (requires user input) */
      VG_(clo_gen_suppressions) = 0;
      /* Disable attaching to GDB (requires user input) */
      VG_(clo_db_attach) = False;
      /* Set a known verbosity level */
      VG_(clo_verbosity) = 1;
      /* Disable error limits (this might be a bad idea!) */
      VG_(clo_error_limit) = False;
      /* Disable emulation warnings */
      VG_(clo_show_emwarns) = False;
      /* Disable waiting for GDB to debug Valgrind */
      VG_(clo_wait_for_gdb) = False;
      /* No file-descriptor leak checking yet */
      VG_(clo_track_fds) = False;
      /* Disable timestamped output */
      VG_(clo_time_stamp) = False;
      /* Also, we want to set options for the leak checker, but that
         will have to be done in Memcheck's flag-handling code, not
         here. */
   }

   /* All non-logging-related options have been checked.  If the logging
      option specified is ok, we can switch to it, as we know we won't
      have to generate any other command-line-related error messages.
      (So far we should be still attached to stderr, so we can show on
      the terminal any problems to do with processing command line
      opts.)
   
      So set up logging now.  After this is done, VG_(clo_log_fd)
      should be connected to whatever sink has been selected, and we
      indiscriminately chuck stuff into it without worrying what the
      nature of it is.  Oh the wonder of Unix streams. */

   vg_assert(VG_(clo_log_fd) == 2 /* stderr */);
   vg_assert(VG_(logging_to_socket) == False);

   switch (log_to) {

      case VgLogTo_Fd: 
         vg_assert(VG_(clo_log_name) == NULL);
         break;

      case VgLogTo_File: {
         HChar  logfilename[1000];
	 Int    seq  = 0;
	 Int    pid  = VG_(getpid)();
         HChar* qual = NULL;

         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */

	 if (VG_(clo_log_file_qualifier)) {
            qual = VG_(getenv)(VG_(clo_log_file_qualifier));
	 }

	 for (;;) {
            HChar pidtxt[20], seqtxt[20];

            VG_(sprintf)(pidtxt, "%d", pid);

            if (seq == 0)
               seqtxt[0] = 0;
            else
               VG_(sprintf)(seqtxt, ".%d", seq);

	    seq++;

            /* Result:
                  if (qual)      base_name ++ "." ++ qual ++ seqtxt
                  if (not qual)  base_name ++ "." ++ pid  ++ seqtxt
            */
            VG_(sprintf)( logfilename, 
                          "%s.%s%s",
                          VG_(clo_log_name), 
                          qual ? qual : pidtxt,
                          seqtxt );

            // EXCL: it will fail with EEXIST if the file already exists.
            sres = VG_(open)(logfilename, 
                             VKI_O_CREAT|VKI_O_WRONLY|VKI_O_EXCL|VKI_O_TRUNC, 
                             VKI_S_IRUSR|VKI_S_IWUSR);
	    if (!sres.isError) {
               tmp_log_fd = sres.res;
	       break; /* for (;;) */
	    } else {
               // If the file already existed, we try the next name.  If it
               // was some other file error, we give up.
	       if (sres.err != VKI_EEXIST) {
		  VG_(message)(Vg_UserMsg, 
			       "Can't create log file '%s' (%s); giving up!", 
			       logfilename, VG_(strerror)(sres.err));
		  VG_(err_bad_option)(
		     "--log-file=<file> (didn't work out for some reason.)");
                  /*NOTREACHED*/
	       }
	    }
	 }
         break; /* switch (VG_(clo_log_to)) */
      }

      case VgLogTo_FileExactly: {
         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */

         sres = VG_(open)(VG_(clo_log_name),
                          VKI_O_CREAT|VKI_O_WRONLY|VKI_O_TRUNC, 
                          VKI_S_IRUSR|VKI_S_IWUSR);
         if (!sres.isError) {
            tmp_log_fd = sres.res;
         } else {
            VG_(message)(Vg_UserMsg, 
                         "Can't create/open log file '%s'; giving up!", 
                         VG_(clo_log_name));
            VG_(err_bad_option)(
               "--log-file-exactly=<file> (didn't work out for some reason.)");
            /*NOTREACHED*/
	 }
         break; /* switch (VG_(clo_log_to)) */
      }

      case VgLogTo_Socket: {
         vg_assert(VG_(clo_log_name) != NULL);
         vg_assert(VG_(strlen)(VG_(clo_log_name)) <= 900); /* paranoia */
         tmp_log_fd = VG_(connect_via_socket)( VG_(clo_log_name) );
         if (tmp_log_fd == -1) {
            VG_(message)(Vg_UserMsg, 
               "Invalid --log-socket=ipaddr or --log-socket=ipaddr:port spec"); 
            VG_(message)(Vg_UserMsg, 
               "of '%s'; giving up!", VG_(clo_log_name) );
            VG_(err_bad_option)(
               "--log-socket=");
            /*NOTREACHED*/
	 }
         if (tmp_log_fd == -2) {
            VG_(message)(Vg_UserMsg, 
               "valgrind: failed to connect to logging server '%s'.",
               VG_(clo_log_name) ); 
            VG_(message)(Vg_UserMsg, 
                "Log messages will sent to stderr instead." );
            VG_(message)(Vg_UserMsg, 
                "" );
            /* We don't change anything here. */
            vg_assert(VG_(clo_log_fd) == 2);
            tmp_log_fd = 2;
	 } else {
            vg_assert(tmp_log_fd > 0);
            VG_(logging_to_socket) = True;
         }
         break;
      }
   }


   /* Check that the requested tool actually supports XML output. */
   if (VG_(clo_xml) && !VG_(needs).xml_output) {
      VG_(clo_xml) = False;
      VG_(message)(Vg_UserMsg, 
         "%s does not support XML output.", VG_(details).name); 
      VG_(err_bad_option)("--xml=yes");
      /*NOTREACHED*/
   }

   if (tmp_log_fd >= 0) {
      // Move log_fd into the safe range, so it doesn't conflict with any app fds.
      tmp_log_fd = VG_(fcntl)(tmp_log_fd, VKI_F_DUPFD, VG_(fd_hard_limit));
      if (tmp_log_fd < 0) {
         VG_(message)(Vg_UserMsg, "valgrind: failed to move logfile fd into safe range, using stderr");
         VG_(clo_log_fd) = 2;   // stderr
      } else {
         VG_(clo_log_fd) = tmp_log_fd;
         VG_(fcntl)(VG_(clo_log_fd), VKI_F_SETFD, VKI_FD_CLOEXEC);
      }
   } else {
      // If they said --log-fd=-1, don't print anything.  Plausible for use in
      // regression testing suites that use client requests to count errors.
      VG_(clo_log_fd) = tmp_log_fd;
   }

   if (VG_(clo_n_suppressions) < VG_CLO_MAX_SFILES-1 &&
       (VG_(needs).core_errors || VG_(needs).tool_errors)) {
      /* If we haven't reached the max number of suppressions, load
         the default one. */
      static const Char default_supp[] = "default.supp";
      Int len = VG_(strlen)(VG_(libdir)) + 1 + sizeof(default_supp);
      Char *buf = VG_(arena_malloc)(VG_AR_CORE, len);
      VG_(sprintf)(buf, "%s/%s", VG_(libdir), default_supp);
      VG_(clo_suppressions)[VG_(clo_n_suppressions)] = buf;
      VG_(clo_n_suppressions)++;
   }

   return (log_to == VgLogTo_Fd);
}


/*====================================================================*/
/*=== Printing the preamble                                        ===*/
/*====================================================================*/

/* Ok, the logging sink is running now.  Print a suitable preamble.
   If logging to file or a socket, write details of parent PID and
   command line args, to help people trying to interpret the
   results of a run which encompasses multiple processes. */
static void print_preamble(Bool logging_to_fd, const char* toolname)
{
   HChar* xpre  = VG_(clo_xml) ? "  <line>" : "";
   HChar* xpost = VG_(clo_xml) ? "</line>" : "";
   Int    i;

   vg_assert( VG_(args_for_client) );
   vg_assert( VG_(args_for_valgrind) );

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "<?xml version=\"1.0\"?>");
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<valgrindoutput>");
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<protocolversion>2</protocolversion>");
      VG_(message)(Vg_UserMsg, "");
   }

   if (VG_(clo_verbosity > 0)) {

      if (VG_(clo_xml))
         VG_(message)(Vg_UserMsg, "<preamble>");

      /* Tool details */
      VG_(message)(Vg_UserMsg, "%s%s%s%s, %s.%s",
                   xpre,
                   VG_(details).name, 
                   NULL == VG_(details).version ? "" : "-",
                   NULL == VG_(details).version 
                      ? (Char*)"" : VG_(details).version,
                   VG_(details).description,
                   xpost);
      VG_(message)(Vg_UserMsg, "%s%s%s", 
                               xpre, VG_(details).copyright_author, xpost);

      /* Core details */
      VG_(message)(Vg_UserMsg,
         "%sUsing LibVEX rev %s, a library for dynamic binary translation.%s",
         xpre, LibVEX_Version(), xpost );
      VG_(message)(Vg_UserMsg, 
         "%sCopyright (C) 2004-2007, and GNU GPL'd, by OpenWorks LLP.%s",
         xpre, xpost );
      VG_(message)(Vg_UserMsg,
         "%sUsing valgrind-%s, a dynamic binary instrumentation framework.%s",
         xpre, VERSION, xpost);
      VG_(message)(Vg_UserMsg, 
         "%sCopyright (C) 2000-2007, and GNU GPL'd, by Julian Seward et al.%s",
         xpre, xpost );

      if (VG_(clo_verbosity) == 1 && !VG_(clo_xml))
         VG_(message)(Vg_UserMsg, "For more details, rerun with: -v");

      if (VG_(clo_xml))
         VG_(message)(Vg_UserMsg, "</preamble>");
   }

   if (!VG_(clo_xml) && VG_(clo_verbosity) > 0 && !logging_to_fd) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, 
         "My PID = %d, parent PID = %d.  Prog and args are:",
         VG_(getpid)(), VG_(getppid)() );
      if (VG_(args_the_exename))
         VG_(message)(Vg_UserMsg, "   %s", VG_(args_the_exename));
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) 
	VG_(message)(Vg_UserMsg, 
                     "   %s", 
                     * (HChar**) VG_(indexXA)( VG_(args_for_client), i ));
      if (VG_(clo_log_file_qualifier)) {
         HChar* val = VG_(getenv)(VG_(clo_log_file_qualifier));
         VG_(message)(Vg_UserMsg, "");
         VG_(message)(Vg_UserMsg, "Log file qualifier: var %s, value %s.",
                                  VG_(clo_log_file_qualifier),
                                  val ? val : "");
      }
   }
   else
   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<pid>%d</pid>", VG_(getpid)());
      VG_(message)(Vg_UserMsg, "<ppid>%d</ppid>", VG_(getppid)());
      VG_(message)(Vg_UserMsg, "<tool>%t</tool>", toolname);
      if (VG_(clo_log_file_qualifier)) {
         HChar* val = VG_(getenv)(VG_(clo_log_file_qualifier));
         VG_(message)(Vg_UserMsg, "<logfilequalifier> <var>%t</var> "
                                  "<value>%t</value> </logfilequalifier>",
                                  VG_(clo_log_file_qualifier),
                                  val ? val : "");
      }
      if (VG_(clo_xml_user_comment)) {
         /* Note: the user comment itself is XML and is therefore to
            be passed through verbatim (%s) rather than escaped
            (%t). */
         VG_(message)(Vg_UserMsg, "<usercomment>%s</usercomment>",
                                  VG_(clo_xml_user_comment));
      }
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "<args>");

      VG_(message)(Vg_UserMsg, "  <vargv>");
      if (VG_(name_of_launcher))
         VG_(message)(Vg_UserMsg, "    <exe>%t</exe>", 
                                  VG_(name_of_launcher));
      else
         VG_(message)(Vg_UserMsg, "    <exe>%t</exe>",
                                  "(launcher name unknown)");
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         VG_(message)(Vg_UserMsg, 
                      "    <arg>%t</arg>", 
                      * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i ));
      }
      VG_(message)(Vg_UserMsg, "  </vargv>");

      VG_(message)(Vg_UserMsg, "  <argv>");
      if (VG_(args_the_exename))
         VG_(message)(Vg_UserMsg, "    <exe>%t</exe>", 
                                  VG_(args_the_exename));
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
         VG_(message)(Vg_UserMsg,
                      "    <arg>%t</arg>", 
                      * (HChar**) VG_(indexXA)( VG_(args_for_client), i ));
      }
      VG_(message)(Vg_UserMsg, "  </argv>");

      VG_(message)(Vg_UserMsg, "</args>");
   }

   // Empty line after the preamble
   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "");

   if (VG_(clo_verbosity) > 1) {
      SysRes fd;
      VexArch vex_arch;
      VexArchInfo vex_archinfo;
      if (!logging_to_fd)
         VG_(message)(Vg_DebugMsg, "");
      VG_(message)(Vg_DebugMsg, "Command line");
      if (VG_(args_the_exename))
         VG_(message)(Vg_DebugMsg, "   %s", VG_(args_the_exename));
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++)
         VG_(message)(Vg_DebugMsg, 
                     "   %s", 
                     * (HChar**) VG_(indexXA)( VG_(args_for_client), i ));

      VG_(message)(Vg_DebugMsg, "Startup, with flags:");
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         VG_(message)(Vg_DebugMsg, 
                     "   %s", 
                     * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i ));
      }

      VG_(message)(Vg_DebugMsg, "Contents of /proc/version:");
      fd = VG_(open) ( "/proc/version", VKI_O_RDONLY, 0 );
      if (fd.isError) {
         VG_(message)(Vg_DebugMsg, "  can't open /proc/version");
      } else {
#        define BUF_LEN    256
         Char version_buf[BUF_LEN];
         Int n = VG_(read) ( fd.res, version_buf, BUF_LEN );
         vg_assert(n <= BUF_LEN);
         if (n > 0) {
            version_buf[n-1] = '\0';
            VG_(message)(Vg_DebugMsg, "  %s", version_buf);
         } else {
            VG_(message)(Vg_DebugMsg, "  (empty?)");
         }
         VG_(close)(fd.res);
#        undef BUF_LEN
      }

      VG_(machine_get_VexArchInfo)( &vex_arch, &vex_archinfo );
      VG_(message)(
         Vg_DebugMsg, 
         "Arch and hwcaps: %s, %s",
         LibVEX_ppVexArch   ( vex_arch ),
         LibVEX_ppVexHwCaps ( vex_arch, vex_archinfo.hwcaps )
      );
      VG_(message)(
         Vg_DebugMsg, 
         "Page sizes: currently %d, max supported %d", 
         (Int)VKI_PAGE_SIZE, (Int)VKI_MAX_PAGE_SIZE
      );
      VG_(message)(Vg_DebugMsg, "Valgrind library directory: %s", VG_(libdir));
   }
}


/*====================================================================*/
/*=== File descriptor setup                                        ===*/
/*====================================================================*/

/* Number of file descriptors that Valgrind tries to reserve for
   it's own use - just a small constant. */
#define N_RESERVED_FDS (10)

static void setup_file_descriptors(void)
{
   struct vki_rlimit rl;
   Bool show = False;

   /* Get the current file descriptor limits. */
   if (VG_(getrlimit)(VKI_RLIMIT_NOFILE, &rl) < 0) {
      rl.rlim_cur = 1024;
      rl.rlim_max = 1024;
   }

   if (show)
      VG_(printf)("fd limits: host, before: cur %u max %u\n", 
                  rl.rlim_cur, rl.rlim_max);

#  if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   /* I don't know why this kludge is needed; however if rl.rlim_cur
      is RLIM_INFINITY, then VG_(safe_fd)'s attempts using VG_(fcntl)
      to lift V's file descriptors above the threshold RLIM_INFINITY -
      N_RESERVED_FDS fail.  So just use a relatively conservative
      value in this case. */
   if (rl.rlim_cur > 1024)
      rl.rlim_cur = 1024;
#  endif

   /* Work out where to move the soft limit to. */
   if (rl.rlim_cur + N_RESERVED_FDS <= rl.rlim_max) {
      rl.rlim_cur = rl.rlim_cur + N_RESERVED_FDS;
   } else {
      rl.rlim_cur = rl.rlim_max;
   }

   /* Reserve some file descriptors for our use. */
   VG_(fd_soft_limit) = rl.rlim_cur - N_RESERVED_FDS;
   VG_(fd_hard_limit) = rl.rlim_cur - N_RESERVED_FDS;

   /* Update the soft limit. */
   VG_(setrlimit)(VKI_RLIMIT_NOFILE, &rl);

   if (show) {
      VG_(printf)("fd limits: host,  after: cur %u max %u\n",
                  rl.rlim_cur, rl.rlim_max);
      VG_(printf)("fd limits: guest       : cur %u max %u\n",
                  VG_(fd_soft_limit), VG_(fd_hard_limit));
   }

   if (VG_(cl_exec_fd) != -1)
      VG_(cl_exec_fd) = VG_(safe_fd)( VG_(cl_exec_fd) );
}


/*====================================================================*/
/*=== BB profiling                                                 ===*/
/*====================================================================*/

static 
void show_BB_profile ( BBProfEntry tops[], UInt n_tops, ULong score_total )
{
   ULong score_cumul,   score_here;
   Char  buf_cumul[10], buf_here[10];
   Char  name[64];
   Int   r;

   VG_(printf)("\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("--- BEGIN BB Profile (summary of scores)                ---\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("\n");

   VG_(printf)("Total score = %lld\n\n", score_total);

   score_cumul = 0;
   for (r = 0; r < n_tops; r++) {
      if (tops[r].addr == 0)
         continue;
      name[0] = 0;
      VG_(get_fnname_w_offset)(tops[r].addr, name, 64);
      name[63] = 0;
      score_here = tops[r].score;
      score_cumul += score_here;
      VG_(percentify)(score_cumul, score_total, 2, 6, buf_cumul);
      VG_(percentify)(score_here,  score_total, 2, 6, buf_here);
      VG_(printf)("%3d: (%9lld %s)   %9lld %s      0x%llx %s\n",
                  r,
                  score_cumul, buf_cumul,
                  score_here,  buf_here, tops[r].addr, name );
   }

   VG_(printf)("\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("--- BB Profile (BB details)                             ---\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("\n");

   score_cumul = 0;
   for (r = 0; r < n_tops; r++) {
      if (tops[r].addr == 0)
         continue;
      name[0] = 0;
      VG_(get_fnname_w_offset)(tops[r].addr, name, 64);
      name[63] = 0;
      score_here = tops[r].score;
      score_cumul += score_here;
      VG_(percentify)(score_cumul, score_total, 2, 6, buf_cumul);
      VG_(percentify)(score_here,  score_total, 2, 6, buf_here);
      VG_(printf)("\n");
      VG_(printf)("=-=-=-=-=-=-=-=-=-=-=-=-=-= begin BB rank %d "
                  "=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n", r);
      VG_(printf)("%3d: (%9lld %s)   %9lld %s      0x%llx %s\n",
                  r,
                  score_cumul, buf_cumul,
                  score_here,  buf_here, tops[r].addr, name );
      VG_(printf)("\n");
      VG_(translate)(0, tops[r].addr, True, VG_(clo_profile_flags), 0, True);
      VG_(printf)("=-=-=-=-=-=-=-=-=-=-=-=-=-=  end BB rank %d  "
                  "=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n", r);
   }

   VG_(printf)("\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("--- END BB Profile                                      ---\n");
   VG_(printf)("-----------------------------------------------------------\n");
   VG_(printf)("\n");
}


/*====================================================================*/
/*=== main()                                                       ===*/
/*====================================================================*/

/* When main() is entered, we should be on the following stack, not
   the one the kernel gave us.  We will run on this stack until
   simulation of the root thread is started, at which point a transfer
   is made to a dynamically allocated stack.  This is for the sake of
   uniform overflow detection for all Valgrind threads.  This is
   marked global even though it isn't, because assembly code below
   needs to reference the name. */

/*static*/ VgStack VG_(interim_stack);

/* These are the structures used to hold info for creating the initial
   client image.

   'iicii' mostly holds important register state present at system
   startup (_start_valgrind).  valgrind_main() then fills in the rest
   of it and passes it to VG_(ii_create_image)().  That produces
   'iifii', which is later handed to VG_(ii_finalise_image). */

/* In all OS-instantiations, the_iicii has a field .sp_at_startup.
   This should get some address inside the stack on which we gained
   control (eg, it could be the SP at startup).  It doesn't matter
   exactly where in the stack it is.  This value is passed to the
   address space manager at startup.  On Linux, aspacem then uses it
   to identify the initial stack segment and hence the upper end of
   the usable address space. */

static IICreateImageInfo   the_iicii;
static IIFinaliseImageInfo the_iifii;


/* --- Forwards decls to do with shutdown --- */

static void final_tidyup(ThreadId tid); 

/* Do everything which needs doing when the last thread exits */
static 
void shutdown_actions_NORETURN( ThreadId tid, 
                                VgSchedReturnCode tids_schedretcode );

/* --- end of Forwards decls to do with shutdown --- */


/* TODO: GIVE THIS A PROPER HOME
   TODO: MERGE THIS WITH DUPLICATE IN mc_leakcheck.c and coredump-elf.c.
   Extract from aspacem a vector of the current segment start
   addresses.  The vector is dynamically allocated and should be freed
   by the caller when done.  REQUIRES m_mallocfree to be running.
   Writes the number of addresses required into *n_acquired. */

static Addr* get_seg_starts ( /*OUT*/Int* n_acquired )
{
   Addr* starts;
   Int   n_starts, r = 0;

   n_starts = 1;
   while (True) {
      starts = VG_(malloc)( n_starts * sizeof(Addr) );
      if (starts == NULL)
         break;
      r = VG_(am_get_segment_starts)( starts, n_starts );
      if (r >= 0)
         break;
      VG_(free)(starts);
      n_starts *= 2;
   }

   if (starts == NULL) {
     *n_acquired = 0;
     return NULL;
   }

   *n_acquired = r;
   return starts;
}


/* By the time we get to valgrind_main, the_iicii should already have
   been filled in with any important details as required by whatever
   OS we have been built for.
*/
static
Int valgrind_main ( Int argc, HChar **argv, HChar **envp )
{
   HChar*  toolname           = "memcheck";    // default to Memcheck
   Int     need_help          = 0; // 0 = no, 1 = --help, 2 = --help-debug
   UInt*   client_auxv        = NULL;
   Int     loglevel, i;
   Bool    logging_to_fd;
   struct vki_rlimit zero = { 0, 0 };

   //============================================================
   //
   // Nb: startup is complex.  Prerequisites are shown at every step.
   // *** Be very careful when messing with the order ***
   //
   // The first order of business is to get debug logging, the address
   // space manager and the dynamic memory manager up and running.
   // Once that's done, we can relax a bit.
   //
   //============================================================
   
   /* This is needed to make VG_(getenv) usable early. */
   VG_(client_envp) = (Char**)envp;

   //--------------------------------------------------------------
   // Start up the logging mechanism
   //   p: none
   //--------------------------------------------------------------
   /* Start the debugging-log system ASAP.  First find out how many 
      "-d"s were specified.  This is a pre-scan of the command line. */
   loglevel = 0;
   for (i = 1; i < argc; i++) {
      if (argv[i][0] != '-')
         break;
      if (VG_STREQ(argv[i], "--")) 
         break;
      if (VG_STREQ(argv[i], "-d")) 
         loglevel++;
   }

   /* ... and start the debug logger.  Now we can safely emit logging
      messages all through startup. */
   VG_(debugLog_startup)(loglevel, "Stage 2 (main)");
   VG_(debugLog)(1, "main", "Welcome to Valgrind version " 
                            VERSION " debug logging\n");

   //--------------------------------------------------------------
   // AIX5 only: register the system call numbers
   //   p: logging
   //   p: that the initial few syscall numbers stated in the
   //      bootblock have been installed (else we can't 
   //      open/read/close).
   //--------------------------------------------------------------
#  if defined(VGO_aix5)
   VG_(debugLog)(1, "main", "aix5: registering syscalls ..\n");
   { UChar  sysent_name[50];
     SysRes fd;
     Bool   ok;
     Int    n_unregd, sysent_used = 0;
     prsysent_t* sysent_hdr;

     VG_(sprintf)(sysent_name, "/proc/%d/sysent", VG_(getpid)());
     fd = VG_(open)(sysent_name, VKI_O_RDONLY, 0);
     if (fd.isError)
        VG_(err_config_error)("aix5: can't open /proc/<pid>/sysent");

     sysent_used = VG_(read)(fd.res, aix5_sysent_buf, VG_AIX5_SYSENT_SIZE);
     if (sysent_used < 0)
        VG_(err_config_error)("aix5: error reading /proc/<pid>/sysent");
     if (sysent_used >= VG_AIX5_SYSENT_SIZE)
        VG_(err_config_error)("aix5: VG_AIX5_SYSENT_SIZE is too low; "
                              "increase and recompile");
     VG_(close)(fd.res);

     vg_assert(sysent_used > 0 && sysent_used < VG_AIX5_SYSENT_SIZE);

     sysent_hdr = (prsysent_t*)&aix5_sysent_buf[0];

     n_unregd = 0;
     for (i = 0; i < sysent_hdr->pr_nsyscalls; i++) {
        UChar* name = &aix5_sysent_buf[ sysent_hdr
                                        ->pr_syscall[i].pr_nameoff ];
        UInt   nmbr = sysent_hdr->pr_syscall[i].pr_number;
        VG_(debugLog)(3, "main", "aix5: bind syscall %d to \"%s\"\n", 
                                 nmbr, name);
        ok = VG_(aix5_register_syscall)(nmbr, name);
        if (!ok)
           n_unregd++;
	if (!ok)
           VG_(debugLog)(3, "main", 
                            "aix5: bind FAILED: %d to \"%s\"\n", 
                            nmbr, name);
     }
     VG_(debugLog)(1, "main", "aix5: .. %d syscalls known, %d unknown\n",
                      sysent_hdr->pr_nsyscalls - n_unregd, n_unregd );
     VG_(debugLog)(1, "main", "aix5: __NR_AIX5_FAKE_SIGRETURN = %d\n",
                      __NR_AIX5_FAKE_SIGRETURN );
   }
#  endif

   //--------------------------------------------------------------
   // Ensure we're on a plausible stack.
   //   p: logging
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Checking current stack is plausible\n");
   { HChar* limLo  = (HChar*)(&VG_(interim_stack).bytes[0]);
     HChar* limHi  = limLo + sizeof(VG_(interim_stack));
     HChar* aLocal = (HChar*)&zero; /* any auto local will do */
     if (aLocal < limLo || aLocal >= limHi) {
        /* something's wrong.  Stop. */
        VG_(debugLog)(0, "main", "Root stack %p to %p, a local %p\n",
                          limLo, limHi, aLocal );
        VG_(debugLog)(0, "main", "Valgrind: FATAL: "
                                 "Initial stack switched failed.\n");
        VG_(debugLog)(0, "main", "   Cannot continue.  Sorry.\n");
        VG_(exit)(1);
     }
   }

   //--------------------------------------------------------------
   // Ensure we have a plausible pointer to the stack on which
   // we gained control (not the current stack!)
   //   p: logging
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Checking initial stack was noted\n");
   if (the_iicii.sp_at_startup == 0) {
      VG_(debugLog)(0, "main", "Valgrind: FATAL: "
                               "Initial stack was not noted.\n");
      VG_(debugLog)(0, "main", "   Cannot continue.  Sorry.\n");
      VG_(exit)(1);
   }

   //--------------------------------------------------------------
   // Start up the address space manager, and determine the
   // approximate location of the client's stack
   //   p: logging, plausible-stack
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Starting the address space manager\n");
   vg_assert(VKI_PAGE_SIZE     == 4096 || VKI_PAGE_SIZE     == 65536);
   vg_assert(VKI_MAX_PAGE_SIZE == 4096 || VKI_MAX_PAGE_SIZE == 65536);
   vg_assert(VKI_PAGE_SIZE <= VKI_MAX_PAGE_SIZE);
   vg_assert(VKI_PAGE_SIZE     == (1 << VKI_PAGE_SHIFT));
   vg_assert(VKI_MAX_PAGE_SIZE == (1 << VKI_MAX_PAGE_SHIFT));
   the_iicii.clstack_top = VG_(am_startup)( the_iicii.sp_at_startup );
   VG_(debugLog)(1, "main", "Address space manager is running\n");

   //--------------------------------------------------------------
   // Start up the dynamic memory manager
   //   p: address space management
   //   In fact m_mallocfree is self-initialising, so there's no
   //   initialisation call to do.  Instead, try a simple malloc/
   //   free pair right now to check that nothing is broken.
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Starting the dynamic memory manager\n");
   { void* p = VG_(malloc)( 12345 );
     if (p) VG_(free)( p );
   }
   VG_(debugLog)(1, "main", "Dynamic memory manager is running\n");

   //============================================================
   //
   // Dynamic memory management is now available.
   //
   //============================================================

   //--------------------------------------------------------------
   // Look for alternative libdir                                  
   { HChar *cp = VG_(getenv)(VALGRIND_LIB);
     if (cp != NULL)
        VG_(libdir) = cp;
   }

   //--------------------------------------------------------------
   // Extract the launcher name from the environment.
   VG_(debugLog)(1, "main", "Getting stage1's name\n");
   VG_(name_of_launcher) = VG_(getenv)(VALGRIND_LAUNCHER);
   if (VG_(name_of_launcher) == NULL) {
      VG_(printf)("valgrind: You cannot run '%s' directly.\n", argv[0]);
      VG_(printf)("valgrind: You should use $prefix/bin/valgrind.\n");
      VG_(exit)(1);
   }

   //--------------------------------------------------------------
   // Get the current process datasize rlimit, and set it to zero.
   // This prevents any internal uses of brk() from having any effect.
   // We remember the old value so we can restore it on exec, so that
   // child processes will have a reasonable brk value.
   VG_(getrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));
   zero.rlim_max = VG_(client_rlimit_data).rlim_max;
   VG_(setrlimit)(VKI_RLIMIT_DATA, &zero);

   // Get the current process stack rlimit.
   VG_(getrlimit)(VKI_RLIMIT_STACK, &VG_(client_rlimit_stack));

   //--------------------------------------------------------------
   // Figure out what sort of CPU we're on, and whether it is 
   // able to run V.
   VG_(debugLog)(1, "main", "Get hardware capabilities ...\n");
   { VexArch     vex_arch;
     VexArchInfo vex_archinfo;
     Bool ok = VG_(machine_get_hwcaps)();
     if (!ok) {
        VG_(printf)("\n");
        VG_(printf)("valgrind: fatal error: unsupported CPU.\n");
        VG_(printf)("   Supported CPUs are:\n");
        VG_(printf)("   * x86 (practically any; Pentium-I or above), "
                    "AMD Athlon or above)\n");
        VG_(printf)("   * AMD Athlon64/Opteron\n");
        VG_(printf)("   * PowerPC (most; ppc405 and above)\n");
        VG_(printf)("\n");
        VG_(exit)(1);
     }
     VG_(machine_get_VexArchInfo)( &vex_arch, &vex_archinfo );
     VG_(debugLog)(
        1, "main", "... arch = %s, hwcaps = %s\n",
           LibVEX_ppVexArch   ( vex_arch ),
           LibVEX_ppVexHwCaps ( vex_arch, vex_archinfo.hwcaps ) 
     );
   }

   //============================================================
   // Command line argument handling order:
   // * If --help/--help-debug are present, show usage message 
   //   (including the tool-specific usage)
   // * (If no --tool option given, default to Memcheck)
   // * Then, if client is missing, abort with error msg
   // * Then, if any cmdline args are bad, abort with error msg
   //============================================================

   //--------------------------------------------------------------
   // Split up argv into: C args, V args, V extra args, and exename.
   //   p: dynamic memory allocation
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Split up command line\n");
   VG_(split_up_argv)( argc, argv );
   vg_assert( VG_(args_for_valgrind) );
   vg_assert( VG_(args_for_client) );
   if (0) {
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++)
         VG_(printf)(
            "varg %s\n", 
            * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i )
         );
      VG_(printf)(" exe %s\n", VG_(args_the_exename));
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++)
         VG_(printf)(
            "carg %s\n", 
            * (HChar**) VG_(indexXA)( VG_(args_for_client), i )
         );
   }

#  if defined(VGO_aix5)
   /* Tolerate ptraced-based launchers.  They can't run 'no program'
      if the user types "valgrind --help", so they run a do-nothing
      program $prefix/bin/no_op_client_for_valgrind, and we catch that
      here and turn it the exe name back into NULL.  Then --help,
      --version etc work as they should. */
   if (VG_(args_the_exename) 
       && VG_(strstr)( VG_(args_the_exename), "/no_op_client_for_valgrind" )) {
      VG_(args_the_exename) = NULL;
   }
#  endif

   //--------------------------------------------------------------
   // Extract tool name and whether help has been requested.
   // Note we can't print the help message yet, even if requested,
   // because the tool has not been initialised.
   //   p: split_up_argv [for VG_(args_for_valgrind)]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Preprocess command line opts\n");
   get_helprequest_and_toolname(&need_help, &toolname);

   // Set default vex control params
   LibVEX_default_VexControl(& VG_(clo_vex_control));

   //--------------------------------------------------------------
   // Load client executable, finding in $PATH if necessary
   //   p: get_helprequest_and_toolname()  [for 'exec', 'need_help']
   //   p: layout_remaining_space          [so there's space]
   //
   // Set up client's environment
   //   p: set-libdir                   [for VG_(libdir)]
   //   p: get_helprequest_and_toolname [for toolname]
   //
   // Setup client stack, eip, and VG_(client_arg[cv])
   //   p: load_client()     [for 'info']
   //   p: fix_environment() [for 'env']
   //
   // Setup client data (brk) segment.  Initially a 1-page segment
   // which abuts a shrinkable reservation. 
   //     p: load_client()     [for 'info' and hence VG_(brk_base)]
   //
   // p: _start_in_C (for zeroing out the_iicii and putting some
   //    initial values into it)
   //--------------------------------------------------------------
   if (!need_help) {
      VG_(debugLog)(1, "main", "Create initial image\n");

#     if defined(VGO_linux)
      the_iicii.argv              = argv;
      the_iicii.envp              = envp;
      the_iicii.toolname          = toolname;
#     elif defined(VGO_aix5)
      /* the_iicii.intregs37      already set up */
      /* the_iicii.bootblock      already set up */
      /* the_iicii.adler32_exp    already set up */
      /* the_iicii.sp_at_startup  is irrelevant */
      /* the_iicii.clstack_top    is irrelevant */
      the_iicii.toolname          = toolname;
#     else
#       error "Uknown platform"
#     endif

      the_iifii = VG_(ii_create_image)( the_iicii );

#     if defined(VGO_aix5)
      /* Tell aspacem where the initial client stack is, so that it
         can later produce a faked-up NSegment in response to
         VG_(am_find_nsegment) for that address range, if asked. */
      VG_(am_aix5_set_initial_client_sp)( the_iifii.initial_client_SP );
      /* Now have a look at said fake segment, so we can find out
         the size of it. */
      { SizeT sz;
        NSegment const* seg 
           = VG_(am_find_nsegment)( the_iifii.initial_client_SP );
        vg_assert(seg);
        sz = seg->end - seg->start + 1;
        vg_assert(sz >= 0 && sz <= 64*1024*1024); /* stay sane */
        the_iifii.clstack_max_size = sz;
      }
#     endif
   }

   //==============================================================
   //
   // Finished loading/setting up the client address space.
   //
   //==============================================================

   //--------------------------------------------------------------
   // setup file descriptors
   //   p: n/a
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Setup file descriptors\n");
   setup_file_descriptors();

   //--------------------------------------------------------------
   // create the fake /proc/<pid>/cmdline file and then unlink it,
   // but hold onto the fd, so we can hand it out to the client
   // when it tries to open /proc/<pid>/cmdline for itself.
   //   p: setup file descriptors
   //--------------------------------------------------------------
   if (!need_help) {
      HChar  buf[50], buf2[50+64];
      HChar  nul[1];
      Int    fd, r;
      HChar* exename;

      VG_(debugLog)(1, "main", "Create fake /proc/<pid>/cmdline\n");

      VG_(sprintf)(buf, "proc_%d_cmdline", VG_(getpid)());
      fd = VG_(mkstemp)( buf, buf2 );
      if (fd == -1)
         VG_(err_config_error)("Can't create client cmdline file in /tmp.");

      nul[0] = 0;
      exename = VG_(args_the_exename) ? VG_(args_the_exename)
                                      : "unknown_exename";
      VG_(write)(fd, VG_(args_the_exename), 
                     VG_(strlen)( VG_(args_the_exename) ));
      VG_(write)(fd, nul, 1);

      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
         HChar* arg = * (HChar**) VG_(indexXA)( VG_(args_for_client), i );
         VG_(write)(fd, arg, VG_(strlen)( arg ));
         VG_(write)(fd, nul, 1);
      }

      /* Don't bother to seek the file back to the start; instead do
	 it every time a copy of it is given out (by PRE(sys_open)). 
	 That is probably more robust across fork() etc. */

      /* Now delete it, but hang on to the fd. */
      r = VG_(unlink)( buf2 );
      if (r)
         VG_(err_config_error)("Can't delete client cmdline file in /tmp.");

      VG_(cl_cmdline_fd) = fd;
   }

   //--------------------------------------------------------------
   // Init tool part 1: pre_clo_init
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //--------------------------------------------------------------
   {
      Char* s;
      Bool  ok;
      VG_(debugLog)(1, "main", "Initialise the tool part 1 (pre_clo_init)\n");
      (VG_(tool_info).tl_pre_clo_init)();
      ok = VG_(sanity_check_needs)( &s );
      if (!ok) {
         VG_(tool_panic)(s);
      }
   }

   //--------------------------------------------------------------
   // If --tool and --help/--help-debug was given, now give the core+tool
   // help message
   //   p: get_helprequest_and_toolname() [for 'need_help']
   //   p: tl_pre_clo_init                [for 'VG_(tdict).usage']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Print help and quit, if requested\n");
   if (need_help) {
      usage_NORETURN(/*--help-debug?*/2 == need_help);
   }

   //--------------------------------------------------------------
   // Process command line options to Valgrind + tool
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Process Valgrind's command line options, "
                            "setup logging\n");
   logging_to_fd = process_cmd_line_options(client_auxv, toolname);

   //--------------------------------------------------------------
   // Zeroise the millisecond counter by doing a first read of it.
   //   p: none
   //--------------------------------------------------------------
   (void) VG_(read_millisecond_timer)();

   //--------------------------------------------------------------
   // Print the preamble
   //   p: tl_pre_clo_init            [for 'VG_(details).name' and friends]
   //   p: process_cmd_line_options() [for VG_(clo_verbosity), VG_(clo_xml),
   //                                      VG_(clo_log_file_qualifier),
   //                                      logging_to_fd]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Print the preamble...\n");
   print_preamble(logging_to_fd, toolname);
   VG_(debugLog)(1, "main", "...finished the preamble\n");

   //--------------------------------------------------------------
   // Init tool part 2: post_clo_init
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //   p: print_preamble()          [so any warnings printed in post_clo_init
   //                                 are shown after the preamble]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise the tool part 2 (post_clo_init)\n");
   VG_TDICT_CALL(tool_post_clo_init);

   //--------------------------------------------------------------
   // Initialise translation table and translation cache
   //   p: aspacem         [??]
   //   p: tl_pre_clo_init [for 'VG_(details).avg_translation_sizeB']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise TT/TC\n");
   VG_(init_tt_tc)();

   //--------------------------------------------------------------
   // Initialise the redirect table.
   //   p: init_tt_tc [so it can call VG_(search_transtab) safely]
   //   p: aspacem [so can change ownership of sysinfo pages]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise redirects\n");
   VG_(redir_initialise)();

   //--------------------------------------------------------------
   // Allow GDB attach
   //   p: process_cmd_line_options()  [for VG_(clo_wait_for_gdb)]
   //--------------------------------------------------------------
   /* Hook to delay things long enough so we can get the pid and
      attach GDB in another shell. */
   if (VG_(clo_wait_for_gdb)) {
      Long iters;
      volatile Long q;
      VG_(debugLog)(1, "main", "Wait for GDB\n");
      VG_(printf)("pid=%d, entering delay loop\n", VG_(getpid)());

#     if defined(VGP_x86_linux)
      iters = 5;
#     elif defined(VGP_amd64_linux) || defined(VGP_ppc64_linux)
      iters = 10;
#     elif defined(VGP_ppc32_linux)
      iters = 5;
#     elif defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
      iters = 4;
#     else
#       error "Unknown plat"
#     endif

      iters *= 1000*1000*1000;
      for (q = 0; q < iters; q++) 
         ;
   }

   //--------------------------------------------------------------
   // Search for file descriptors that are inherited from our parent
   //   p: process_cmd_line_options  [for VG_(clo_track_fds)]
   //--------------------------------------------------------------
   if (VG_(clo_track_fds)) {
      VG_(debugLog)(1, "main", "Init preopened fds\n");
      VG_(init_preopened_fds)();
   }

   //--------------------------------------------------------------
   // Load debug info for the existing segments.
   //   p: setup_code_redirect_table [so that redirs can be recorded]
   //   p: mallocfree
   //   p: probably: setup fds and process CLOs, so that logging works
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Load initial debug info\n");
#  if defined(VGO_linux)
   { Addr* seg_starts;
     Int   n_seg_starts;

     seg_starts = get_seg_starts( &n_seg_starts );
     vg_assert(seg_starts && n_seg_starts >= 0);

     /* show them all to the debug info reader.  allow_SkFileV has to
        be True here so that we read info from the valgrind executable
        itself. */
     for (i = 0; i < n_seg_starts; i++)
        VG_(di_notify_mmap)( seg_starts[i], True/*allow_SkFileV*/ );

     VG_(free)( seg_starts );
   }
#  elif defined(VGO_aix5)
   { AixCodeSegChange* changes;
     Int changes_size, changes_used;

     /* Find out how many AixCodeSegChange records we will need,
	and acquire them. */
     changes_size = VG_(am_aix5_reread_procmap_howmany_directives)(); 
     changes = VG_(malloc)(changes_size * sizeof(AixCodeSegChange));
     vg_assert(changes);

     /* Now re-read /proc/<pid>/map and acquire a change set */
     VG_(am_aix5_reread_procmap)( changes, &changes_used );
     vg_assert(changes_used >= 0 && changes_used <= changes_size);

     /* And notify m_debuginfo of the changes. */
     for (i = 0; i < changes_used; i++)
        VG_(di_aix5_notify_segchange)(
           changes[i].code_start,
           changes[i].code_len,
           changes[i].data_start,
           changes[i].data_len,
           changes[i].file_name,
           changes[i].mem_name,
           changes[i].is_mainexe,
           changes[i].acquire
        );

     VG_(free)(changes);
   }
#  else
#    error Unknown OS
#  endif

   //--------------------------------------------------------------
   // Tell aspacem of ownership change of the asm helpers, so that
   // m_translate allows them to be translated.  However, only do this
   // after the initial debug info read, since making a hole in the
   // address range for the stage2 binary confuses the debug info reader.
   //   p: aspacem
   //--------------------------------------------------------------
   { Bool change_ownership_v_c_OK;
     Addr co_start   = VG_PGROUNDDN( (Addr)&VG_(trampoline_stuff_start) );
     Addr co_endPlus = VG_PGROUNDUP( (Addr)&VG_(trampoline_stuff_end) );
     VG_(debugLog)(1,"redir",
                     "transfer ownership V -> C of 0x%llx .. 0x%llx\n",
                     (ULong)co_start, (ULong)co_endPlus-1 );

     change_ownership_v_c_OK 
        = VG_(am_change_ownership_v_to_c)( co_start, co_endPlus - co_start );
     vg_assert(change_ownership_v_c_OK);
   }

   //--------------------------------------------------------------
   // Tell the tool about the initial client memory permissions
   //   p: aspacem
   //   p: mallocfree
   //   p: setup_client_stack
   //   p: setup_client_dataseg
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Tell tool about initial permissions\n");
   { Addr*     seg_starts;
     Int       n_seg_starts;

     seg_starts = get_seg_starts( &n_seg_starts );
     vg_assert(seg_starts && n_seg_starts >= 0);

     /* show interesting ones to the tool */
     for (i = 0; i < n_seg_starts; i++) {
        NSegment const* seg 
           = VG_(am_find_nsegment)( seg_starts[i] );
        vg_assert(seg);
        if (seg->kind == SkFileC || seg->kind == SkAnonC) {
           VG_(debugLog)(2, "main", 
                            "tell tool about %010lx-%010lx %c%c%c\n",
                             seg->start, seg->end,
                             seg->hasR ? 'r' : '-',
                             seg->hasW ? 'w' : '-',
                             seg->hasX ? 'x' : '-' );
           VG_TRACK( new_mem_startup, seg->start, seg->end+1-seg->start, 
                                      seg->hasR, seg->hasW, seg->hasX );
        }
     }

     VG_(free)( seg_starts );

     /* Also do the initial stack permissions. */
     { NSegment const* seg 
          = VG_(am_find_nsegment)( the_iifii.initial_client_SP );
       vg_assert(seg);
       vg_assert(seg->kind == SkAnonC);
       vg_assert(the_iifii.initial_client_SP >= seg->start);
       vg_assert(the_iifii.initial_client_SP <= seg->end);
#      if defined(VGO_aix5)
       VG_(clstk_base) = seg->start;
       VG_(clstk_end) = seg->end;
#      endif

       /* Stuff below the initial SP is unaddressable.  Take into
	  account any ABI-mandated space below the stack pointer that
	  is required (VG_STACK_REDZONE_SZB).  setup_client_stack()
	  will have allocated an extra page if a red zone is required,
	  to be on the safe side. */
       vg_assert(the_iifii.initial_client_SP - VG_STACK_REDZONE_SZB 
                 >= seg->start);
       VG_TRACK( die_mem_stack, 
                 seg->start, 
                 the_iifii.initial_client_SP - VG_STACK_REDZONE_SZB 
                                             - seg->start );
       VG_(debugLog)(2, "main", "mark stack inaccessible %010lx-%010lx\n",
                        seg->start, 
                        the_iifii.initial_client_SP-1 - VG_STACK_REDZONE_SZB);
     }

     /* Also the assembly helpers. */
     VG_TRACK( new_mem_startup,
               (Addr)&VG_(trampoline_stuff_start),
               (Addr)&VG_(trampoline_stuff_end) 
                  - (Addr)&VG_(trampoline_stuff_start),
               False, /* readable? */
               False, /* writable? */
               True   /* executable? */ );
   }

   //--------------------------------------------------------------
   // Initialise the scheduler
   //   p: setup_file_descriptors() [else VG_(safe_fd)() breaks]
   //   p: setup_client_stack
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise scheduler\n");
   { NSegment const* seg 
        = VG_(am_find_nsegment)( the_iifii.initial_client_SP );
     vg_assert(seg);
     vg_assert(seg->kind == SkAnonC);
     vg_assert(the_iifii.initial_client_SP >= seg->start);
     vg_assert(the_iifii.initial_client_SP <= seg->end);
     VG_(scheduler_init)( seg->end, the_iifii.clstack_max_size );
   }

   //--------------------------------------------------------------
   // Set up state for the root thread
   //   p: ?
   //      setup_scheduler()      [for sched-specific thread 1 stuff]
   //      VG_(ii_create_image)   [for 'the_iicii' initial info]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Finalise initial image\n");
   VG_(ii_finalise_image)( the_iifii );

   //--------------------------------------------------------------
   // Initialise the signal handling subsystem
   //   p: n/a
   //--------------------------------------------------------------
   // Nb: temporarily parks the saved blocking-mask in saved_sigmask.
   VG_(debugLog)(1, "main", "Initialise signal management\n");
   VG_(sigstartup_actions)();

   //--------------------------------------------------------------
   // Read suppression file
   //   p: process_cmd_line_options()  [for VG_(clo_suppressions)]
   //--------------------------------------------------------------
   if (VG_(needs).core_errors || VG_(needs).tool_errors) {
      VG_(debugLog)(1, "main", "Load suppressions\n");
      VG_(load_suppressions)();
   }

   //--------------------------------------------------------------
   // register client stack
   //--------------------------------------------------------------
   VG_(clstk_id) = VG_(register_stack)(VG_(clstk_base), VG_(clstk_end));

   //--------------------------------------------------------------
   // Show the address space state so far
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "\n");
   VG_(debugLog)(1, "main", "\n");
   VG_(am_show_nsegments)(1,"Memory layout at client startup");
   VG_(debugLog)(1, "main", "\n");
   VG_(debugLog)(1, "main", "\n");

   //--------------------------------------------------------------
   // Run!
   //--------------------------------------------------------------
   if (VG_(clo_xml)) {
      HChar buf[50];
      VG_(elapsed_wallclock_time)(buf);
      VG_(message)(Vg_UserMsg, "<status>\n"
                               "  <state>RUNNING</state>\n"
                               "  <time>%t</time>\n"
                               "</status>", 
                               buf);
      VG_(message)(Vg_UserMsg, "");
   }

   VG_(debugLog)(1, "main", "Running thread 1\n");

   /* As a result of the following call, the last thread standing
      eventually winds up running shutdown_actions_NORETURN
      just below.  Unfortunately, simply exporting said function
      causes m_main to be part of a module cycle, which is pretty
      nonsensical.  So instead of doing that, the address of said
      function is stored in a global variable 'owned' by m_syswrap,
      and it uses that function pointer to get back here when it needs
      to. */

   /* Set continuation address. */
   VG_(address_of_m_main_shutdown_actions_NORETURN)
      = & shutdown_actions_NORETURN;

   /* Run the first thread, eventually ending up at the continuation
      address. */
   VG_(main_thread_wrapper_NORETURN)(1);

   /*NOTREACHED*/
   vg_assert(0);
}

/* Do everything which needs doing when the last thread exits or when
   a thread exits requesting a complete process exit (exit on AIX).

   We enter here holding The Lock.  For the case VgSrc_ExitProcess we
   must never release it, because to do so would allow other threads
   to continue after the system is ostensibly shut down.  So we must
   go to our grave, so to speak, holding the lock. 

   In fact, there is never any point in releasing the lock at this
   point - we have it, we're shutting down the entire system, and
   for the case VgSrc_ExitProcess doing so positively causes trouble.
   So don't. 

   The final_tidyup call makes a bit of a nonsense of the ExitProcess
   case, since it will run the libc_freeres function, thus allowing
   other lurking threads to run again.  Hmm. */

static 
void shutdown_actions_NORETURN( ThreadId tid, 
                                VgSchedReturnCode tids_schedretcode )
{
   VG_(debugLog)(1, "main", "entering VG_(shutdown_actions_NORETURN)\n");
   VG_(am_show_nsegments)(1,"Memory layout at client shutdown");

   vg_assert(VG_(is_running_thread)(tid));

   vg_assert(tids_schedretcode == VgSrc_ExitThread
	     || tids_schedretcode == VgSrc_ExitProcess
             || tids_schedretcode == VgSrc_FatalSig );

   if (tids_schedretcode == VgSrc_ExitThread) {

      // We are the last surviving thread.  Right?
      vg_assert( VG_(count_living_threads)() == 1 );

      // Wait for all other threads to exit.
      // jrs: Huh?  but they surely are already gone
      VG_(reap_threads)(tid);

      // Clean the client up before the final report
      // this causes the libc_freeres function to run
      final_tidyup(tid);

      /* be paranoid */
      vg_assert(VG_(is_running_thread)(tid));
      vg_assert(VG_(count_living_threads)() == 1);

   } else {

      // We may not be the last surviving thread.  However, we
      // want to shut down the entire process.  We hold the lock
      // and we need to keep hold of it all the way out, in order
      // that none of the other threads ever run again.
      vg_assert( VG_(count_living_threads)() >= 1 );

      // Clean the client up before the final report
      // this causes the libc_freeres function to run
      // perhaps this is unsafe, as per comment above
      final_tidyup(tid);

      /* be paranoid */
      vg_assert(VG_(is_running_thread)(tid));
      vg_assert(VG_(count_living_threads)() >= 1);
   }

   VG_(threads)[tid].status = VgTs_Empty;
   //--------------------------------------------------------------
   // Finalisation: cleanup, messages, etc.  Order no so important, only
   // affects what order the messages come.
   //--------------------------------------------------------------
   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "");

   if (VG_(clo_xml)) {
      HChar buf[50];
      if (VG_(needs).core_errors || VG_(needs).tool_errors) {
         VG_(show_error_counts_as_XML)();
         VG_(message)(Vg_UserMsg, "");
      }
      VG_(elapsed_wallclock_time)(buf);
      VG_(message)(Vg_UserMsg, "<status>\n"
                               "  <state>FINISHED</state>\n"
                               "  <time>%t</time>\n"
                               "</status>", 
                               buf);
      VG_(message)(Vg_UserMsg, "");
   }

   /* Print out file descriptor summary and stats. */
   if (VG_(clo_track_fds))
      VG_(show_open_fds)();

   if (VG_(needs).core_errors || VG_(needs).tool_errors)
      VG_(show_all_errors)();

   VG_TDICT_CALL(tool_fini, 0/*exitcode*/);

   if (VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "</valgrindoutput>");
      VG_(message)(Vg_UserMsg, "");
   }

   VG_(sanity_check_general)( True /*include expensive checks*/ );

   if (VG_(clo_verbosity) > 1)
      print_all_stats();

   if (VG_(clo_profile_flags) > 0) {
      #define N_MAX 200
      BBProfEntry tops[N_MAX];
      ULong score_total = VG_(get_BB_profile) (tops, N_MAX);
      show_BB_profile(tops, N_MAX, score_total);
   }

   /* Print Vex storage stats */
   if (0)
       LibVEX_ShowAllocStats();

   /* Ok, finally exit in the os-specific way, according to the scheduler's
      return code.  In short, if the (last) thread exited by calling
      sys_exit, do likewise; if the (last) thread stopped due to a fatal
      signal, terminate the entire system with that same fatal signal. */
   VG_(debugLog)(1, "core_os", 
                    "VG_(terminate_NORETURN)(tid=%lld)\n", (ULong)tid);

   switch (tids_schedretcode) {
   case VgSrc_ExitThread:  /* the normal way out (Linux) */
   case VgSrc_ExitProcess: /* the normal way out (AIX) */
      /* Change the application return code to user's return code,
         if an error was found */
      if (VG_(clo_error_exitcode) > 0 
          && VG_(get_n_errs_found)() > 0) {
         VG_(exit)( VG_(clo_error_exitcode) );
      } else {
         /* otherwise, return the client's exit code, in the normal
            way. */
         VG_(exit)( VG_(threads)[tid].os_state.exitcode );
      }
      /* NOT ALIVE HERE! */
      VG_(core_panic)("entered the afterlife in main() -- ExitT/P");
      break; /* what the hell :) */

   case VgSrc_FatalSig:
      /* We were killed by a fatal signal, so replicate the effect */
      vg_assert(VG_(threads)[tid].os_state.fatalsig != 0);
      VG_(kill_self)(VG_(threads)[tid].os_state.fatalsig);
      VG_(core_panic)("main(): signal was supposed to be fatal");
      break;

   default:
      VG_(core_panic)("main(): unexpected scheduler return code");
   }
}

/* -------------------- */

/* Final clean-up before terminating the process.  
   Clean up the client by calling __libc_freeres() (if requested) 
   This is Linux-specific?
*/
static void final_tidyup(ThreadId tid)
{
#  if defined(VGP_ppc64_linux)
   Addr r2;
#  endif
   Addr __libc_freeres_wrapper = VG_(client___libc_freeres_wrapper);

   vg_assert(VG_(is_running_thread)(tid));
   
   if ( !VG_(needs).libc_freeres ||
        !VG_(clo_run_libc_freeres) ||
        0 == __libc_freeres_wrapper )
      return;			/* can't/won't do it */
#  if defined(VGO_aix5)
   return; /* inapplicable on non-Linux platforms */
#  endif

#  if defined(VGP_ppc64_linux)
   r2 = VG_(get_tocptr)( __libc_freeres_wrapper );
   if (r2 == 0) {
      VG_(message)(Vg_UserMsg, 
                   "Caught __NR_exit, but can't run __libc_freeres()");
      VG_(message)(Vg_UserMsg, 
                   "   since cannot establish TOC pointer for it.");
      return;
   }
#  endif

   if (VG_(clo_verbosity) > 2  ||
       VG_(clo_trace_syscalls) ||
       VG_(clo_trace_sched))
      VG_(message)(Vg_DebugMsg, 
		   "Caught __NR_exit; running __libc_freeres()");
      
   /* set thread context to point to libc_freeres_wrapper */
   /* ppc64-linux note: __libc_freeres_wrapper gives us the real
      function entry point, not a fn descriptor, so can use it
      directly.  However, we need to set R2 (the toc pointer)
      appropriately. */
   VG_(set_IP)(tid, __libc_freeres_wrapper);
#  if defined(VGP_ppc64_linux)
   VG_(threads)[tid].arch.vex.guest_GPR2 = r2;
#  endif

   /* Block all blockable signals by copying the real block state into
      the thread's block state*/
   VG_(sigprocmask)(VKI_SIG_BLOCK, NULL, &VG_(threads)[tid].sig_mask);
   VG_(threads)[tid].tmp_sig_mask = VG_(threads)[tid].sig_mask;

   /* and restore handlers to default */
   VG_(set_default_handler)(VKI_SIGSEGV);
   VG_(set_default_handler)(VKI_SIGBUS);
   VG_(set_default_handler)(VKI_SIGILL);
   VG_(set_default_handler)(VKI_SIGFPE);

   // We were exiting, so assert that...
   vg_assert(VG_(is_exiting)(tid));
   // ...but now we're not again
   VG_(threads)[tid].exitreason = VgSrc_None;

   // run until client thread exits - ideally with LIBC_FREERES_DONE,
   // but exit/exitgroup/signal will do
   VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));
}


/*====================================================================*/
/*=== Getting to main() alive: LINUX (for AIX5 see below)          ===*/
/*====================================================================*/

#if defined(VGO_linux)

/* If linking of the final executables is done with glibc present,
   then Valgrind starts at main() above as usual, and all of the
   following code is irrelevant.

   However, this is not the intended mode of use.  The plan is to
   avoid linking against glibc, by giving gcc the flags 
   -nodefaultlibs -lgcc -nostartfiles at startup.

   From this derive two requirements:

   1. gcc may emit calls to memcpy and memset to deal with structure
      assignments etc.  Since we have chosen to ignore all the
      "normal" supporting libraries, we have to provide our own
      implementations of them.  No problem.

   2. We have to provide a symbol "_start", to which the kernel
      hands control at startup.  Hence the code below.
*/

/* ---------------- Requirement 1 ---------------- */

void* memcpy(void *dest, const void *src, SizeT n);
void* memcpy(void *dest, const void *src, SizeT n) {
   return VG_(memcpy)(dest,src,n);
}
void* memset(void *s, int c, SizeT n);
void* memset(void *s, int c, SizeT n) {
  return VG_(memset)(s,c,n);
}

/* ---------------- Requirement 2 ---------------- */

/* Glibc's sysdeps/i386/elf/start.S has the following gem of a
   comment, which explains how the stack looks right at process start
   (when _start is jumped to).  Hence _start passes %esp to
   _start_in_C_linux, which extracts argc/argv/envp and starts up
   correctly. */

/* This is the canonical entry point, usually the first thing in the text
   segment.  The SVR4/i386 ABI (pages 3-31, 3-32) says that when the entry
   point runs, most registers' values are unspecified, except for:

   %edx         Contains a function pointer to be registered with `atexit'.
                This is how the dynamic linker arranges to have DT_FINI
                functions called for shared libraries that have been loaded
                before this code runs.

   %esp         The stack contains the arguments and environment:
                0(%esp)                 argc
                4(%esp)                 argv[0]
                ...
                (4*argc)(%esp)          NULL
                (4*(argc+1))(%esp)      envp[0]
                ...
                                        NULL
*/

/* The kernel hands control to _start, which extracts the initial
   stack pointer and calls onwards to _start_in_C_linux.  This also switches
   the new stack.  */
#if defined(VGP_x86_linux)
asm("\n"
    ".text\n"
    "\t.globl _start\n"
    "\t.type _start,@function\n"
    "_start:\n"
    /* set up the new stack in %eax */
    "\tmovl  $vgPlain_interim_stack, %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_STACK_ACTIVE_SZB)", %eax\n"
    "\tsubl  $16, %eax\n"
    "\tandl  $~15, %eax\n"
    /* install it, and collect the original one */
    "\txchgl %eax, %esp\n"
    /* call _start_in_C_linux, passing it the startup %esp */
    "\tpushl %eax\n"
    "\tcall  _start_in_C_linux\n"
    "\thlt\n"
    ".previous\n"
);
#elif defined(VGP_amd64_linux)
asm("\n"
    ".text\n"
    "\t.globl _start\n"
    "\t.type _start,@function\n"
    "_start:\n"
    /* set up the new stack in %rdi */
    "\tmovq  $vgPlain_interim_stack, %rdi\n"
    "\taddq  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %rdi\n"
    "\taddq  $"VG_STRINGIFY(VG_STACK_ACTIVE_SZB)", %rdi\n"
    "\tandq  $~15, %rdi\n"
    /* install it, and collect the original one */
    "\txchgq %rdi, %rsp\n"
    /* call _start_in_C_linux, passing it the startup %rsp */
    "\tcall  _start_in_C_linux\n"
    "\thlt\n"
    ".previous\n"
);
#elif defined(VGP_ppc32_linux)
asm("\n"
    ".text\n"
    "\t.globl _start\n"
    "\t.type _start,@function\n"
    "_start:\n"
    /* set up the new stack in r16 */
    "\tlis 16,vgPlain_interim_stack@ha\n"
    "\tla  16,vgPlain_interim_stack@l(16)\n"
    "\tlis    17,("VG_STRINGIFY(VG_STACK_GUARD_SZB)" >> 16)\n"
    "\tori 17,17,("VG_STRINGIFY(VG_STACK_GUARD_SZB)" & 0xFFFF)\n"
    "\tlis    18,("VG_STRINGIFY(VG_STACK_ACTIVE_SZB)" >> 16)\n"
    "\tori 18,18,("VG_STRINGIFY(VG_STACK_ACTIVE_SZB)" & 0xFFFF)\n"
    "\tadd 16,17,16\n"
    "\tadd 16,18,16\n"
    "\trlwinm 16,16,0,0,27\n"
    /* now r16 = &vgPlain_interim_stack + VG_STACK_GUARD_SZB +
       VG_STACK_ACTIVE_SZB rounded down to the nearest 16-byte
       boundary.  And r1 is the original SP.  Set the SP to r16 and
       call _start_in_C_linux, passing it the initial SP. */
    "\tmr 3,1\n"
    "\tmr 1,16\n"
    "\tbl _start_in_C_linux\n"
    "\ttrap\n"
    ".previous\n"
);
#elif defined(VGP_ppc64_linux)
asm("\n"
    /* PPC64 ELF ABI says '_start' points to a function descriptor.
       So we must have one, and that is what goes into the .opd section. */
    "\t.align 2\n"
    "\t.global _start\n"
    "\t.section \".opd\",\"aw\"\n"
    "\t.align 3\n"
    "_start:\n"
    "\t.quad ._start,.TOC.@tocbase,0\n"
    "\t.previous\n"
    "\t.type ._start,@function\n"
    "\t.global  ._start\n"
    "._start:\n"
    /* set up the new stack in r16 */
    "\tlis  16,   vgPlain_interim_stack@highest\n"
    "\tori  16,16,vgPlain_interim_stack@higher\n"
    "\tsldi 16,16,32\n"
    "\toris 16,16,vgPlain_interim_stack@h\n"
    "\tori  16,16,vgPlain_interim_stack@l\n"
    "\txor  17,17,17\n"
    "\tlis    17,("VG_STRINGIFY(VG_STACK_GUARD_SZB)" >> 16)\n"
    "\tori 17,17,("VG_STRINGIFY(VG_STACK_GUARD_SZB)" & 0xFFFF)\n"
    "\txor 18,18,18\n"
    "\tlis    18,("VG_STRINGIFY(VG_STACK_ACTIVE_SZB)" >> 16)\n"
    "\tori 18,18,("VG_STRINGIFY(VG_STACK_ACTIVE_SZB)" & 0xFFFF)\n"
    "\tadd 16,17,16\n"
    "\tadd 16,18,16\n"
    "\trldicr 16,16,0,59\n"
    /* now r16 = &vgPlain_interim_stack + VG_STACK_GUARD_SZB +
       VG_STACK_ACTIVE_SZB rounded down to the nearest 16-byte
       boundary.  And r1 is the original SP.  Set the SP to r16 and
       call _start_in_C_linux, passing it the initial SP. */
    "\tmr 3,1\n"
    "\tmr 1,16\n"
    "\tbl ._start_in_C_linux\n"
    "\tnop\n"
    "\ttrap\n"
);
#else
#error "_start: needs implementation on this platform"
#endif

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#define _GNU_SOURCE
#define _FILE_OFFSET_BITS 64
/* This is in order to get AT_NULL and AT_PAGESIZE. */
#include <elf.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

/* Avoid compiler warnings: this fn _is_ used, but labelling it
   'static' causes gcc to complain it isn't. */
void _start_in_C_linux ( UWord* pArgc );
void _start_in_C_linux ( UWord* pArgc )
{
   Int     r;
   Word    argc = pArgc[0];
   HChar** argv = (HChar**)&pArgc[1];
   HChar** envp = (HChar**)&pArgc[1+argc+1];

   VG_(memset)( &the_iicii, 0, sizeof(the_iicii) );
   VG_(memset)( &the_iifii, 0, sizeof(the_iifii) );

   the_iicii.sp_at_startup = (Addr)pArgc;

#  if defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux)
   {
      /* ppc/ppc64 can be configured with different page sizes.
         Determine this early.  This is an ugly hack and really should
         be moved into valgrind_main. */
      UWord *sp = &pArgc[1+argc+1];
      while (*sp++ != 0)
         ;
      for (; *sp != AT_NULL && *sp != AT_PAGESZ; sp += 2);
      if (*sp == AT_PAGESZ) {
         VKI_PAGE_SIZE = sp[1];
         for (VKI_PAGE_SHIFT = 12;
              VKI_PAGE_SHIFT <= VKI_MAX_PAGE_SHIFT; VKI_PAGE_SHIFT++)
            if (VKI_PAGE_SIZE == (1UL << VKI_PAGE_SHIFT))
         break;
      }
   }
#  endif

   r = valgrind_main( (Int)argc, argv, envp );
   /* NOTREACHED */
   VG_(exit)(r);
}

#endif /* defined(VGO_linux) */


/*====================================================================*/
/*=== Getting to main() alive: AIX5                                ===*/
/*====================================================================*/

#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)

/* This is somewhat simpler than the Linux case.  _start_valgrind
   receives control from the magic piece of code created in this
   process' address space by the launcher, via use of ptrace().  At
   the point of entry:

   - the initial client process image is in memory and ready to roll,
     except that we've partially trashed its integer register state
     in order to get this far.   So ..

   - intregs37 holds the client's initial integer register state, so
     we can restore it before starting the client on the VCPU.

   - we're on the client's stack.  This is not good; therefore the
     first order of business is to switch to our temporary stack.  

   - the client's initial argc/v/envp is in r3/r4/r5 (32 bit mode) or
     r14/r15/r16 (64 bit mode).  They are pulled out of the stashed
     integer register state and passed to our main().

   The launcher will have played some games with argv.  If the launcher
   ($prefix/bin/valgrind) was started like this

      valgrind [args-for-V] app [args-for-app]

   then the launcher will have started the client as

      app [args-for-V] app [args-for-app]

   m_initimg will have to mess with the client's initial r4/r5
   (32-bit) or r15/r16 (64-bit) so that it believes it was execd as
   "app [args-for-app]".  Well, that's no big deal.
*/

#include "launcher-aix5-bootblock.h"

void _start_in_C_aix5 ( AIX5Bootblock* bootblock );
void _start_in_C_aix5 ( AIX5Bootblock* bootblock )
{
   Int     r;
   ULong* intregs37;
   UWord   argc, argv, envp;
   __NR_getpid = bootblock->__NR_getpid;
   __NR_write  = bootblock->__NR_write;
   __NR_exit   = bootblock->__NR_exit;
   __NR_open   = bootblock->__NR_open;
   __NR_read   = bootblock->__NR_read;
   __NR_close  = bootblock->__NR_close;

   VG_(memset)( &the_iicii, 0, sizeof(the_iicii) );
   VG_(memset)( &the_iifii, 0, sizeof(the_iifii) );

   intregs37 = &bootblock->iregs_pc_cr_lr_ctr_xer[0];
   the_iicii.intregs37   = intregs37;
   the_iicii.bootblock   = (void*)bootblock;
   the_iicii.adler32_exp = bootblock->adler32;

   /* Not important on AIX. */
   the_iicii.sp_at_startup = (Addr)0x31415927ULL;

#  if defined(VGP_ppc32_aix5)
   argc = (UWord)intregs37[3];  /* client's r3 == argc */
   argv = (UWord)intregs37[4];
   envp = (UWord)intregs37[5];
#  else /* defined(VGP_ppc64_aix5) */
   argc = (UWord)intregs37[14];  /* client's r14 == argc */
   argv = (UWord)intregs37[15];
   envp = (UWord)intregs37[16];
#  endif

   r = valgrind_main( (Int)argc, (HChar**)argv, (HChar**)envp );

   /* NOTREACHED */
   VG_(exit)(r);
}

/* THE ENTRY POINT */
void _start_valgrind ( AIX5Bootblock* bootblock );
void _start_valgrind ( AIX5Bootblock* bootblock )
{
   /* Switch immediately to our temporary stack, and continue.  This
      is pretty dodgy in that it assumes that gcc does not place on
      the stack, anything needed to form the _start_in_C_aix5 call,
      since it will be on the old stack. */
   register UWord new_r1;
   new_r1  = (UWord)&VG_(interim_stack);
   new_r1 += VG_STACK_GUARD_SZB;  /* step over lower guard page */
   new_r1 += VG_STACK_ACTIVE_SZB; /* step to top of active area */
   new_r1 -= 512; /* paranoia */
   __asm__ __volatile__("mr 1,%0" :/*wr*/ 
                                  :/*rd*/ "b"(new_r1) 
                                  :/*trash*/"r1","memory");
   _start_in_C_aix5(bootblock);
   /*NOTREACHED*/
   VG_(exit)(0);
}

#endif /* defined(VGP_ppc{32,64}_aix5) */


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
