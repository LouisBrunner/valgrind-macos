
/*--------------------------------------------------------------------*/
/*--- Startup: the real stuff                             m_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward 
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
#include "pub_core_libcsetjmp.h"    // to keep _threadstate.h happy
#include "pub_core_threadstate.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_aspacehl.h"
#include "pub_core_commandline.h"
#include "pub_core_debuglog.h"
#include "pub_core_errormgr.h"
#include "pub_core_execontext.h"
#include "pub_core_gdbserver.h"
#include "pub_core_initimg.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_syscall.h"       // VG_(strerror)
#include "pub_core_mach.h"
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_debuginfo.h"
#include "pub_core_redir.h"
#include "pub_core_scheduler.h"
#include "pub_core_seqmatch.h"      // For VG_(string_match)
#include "pub_core_signals.h"
#include "pub_core_stacks.h"        // For VG_(register_stack)
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_translate.h"     // For VG_(translate)
#include "pub_core_trampoline.h"
#include "pub_core_transtab.h"
#include "pub_tool_inner.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
#include "valgrind.h"
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
      VG_(message)(Vg_DebugMsg, "\n");
      VG_(message)(Vg_DebugMsg, 
         "------ Valgrind's internal memory use stats follow ------\n" );
      VG_(sanity_check_malloc_all)();
      VG_(message)(Vg_DebugMsg, "------\n" );
      VG_(print_all_arena_stats)();
      VG_(message)(Vg_DebugMsg, "\n");
   }
}


/*====================================================================*/
/*=== Command-line: variables, processing, etc                     ===*/
/*====================================================================*/

// See pub_{core,tool}_options.h for explanations of all these.

static void usage_NORETURN ( Bool debug_help )
{
   /* 'usage1' contains a %s 
      - for the name of the GDB executable
      - for the name of vgdb's path prefix
      which must be supplied when they are VG_(printf)'d. */
   Char* usage1 = 
"usage: valgrind [options] prog-and-args\n"
"\n"
"  tool-selection option, with default in [ ]:\n"
"    --tool=<name>             use the Valgrind tool named <name> [memcheck]\n"
"\n"
"  basic user options for all Valgrind tools, with defaults in [ ]:\n"
"    -h --help                 show this message\n"
"    --help-debug              show this message, plus debugging options\n"
"    --version                 show version\n"
"    -q --quiet                run silently; only print error msgs\n"
"    -v --verbose              be more verbose -- show misc extra info\n"
"    --trace-children=no|yes   Valgrind-ise child processes (follow execve)? [no]\n"
"    --trace-children-skip=patt1,patt2,...    specifies a list of executables\n"
"                              that --trace-children=yes should not trace into\n"
"    --trace-children-skip-by-arg=patt1,patt2,...   same as --trace-children-skip=\n"
"                              but check the argv[] entries for children, rather\n"
"                              than the exe name, to make a follow/no-follow decision\n"
"    --child-silent-after-fork=no|yes omit child output between fork & exec? [no]\n"
"    --vgdb=no|yes|full        activate gdbserver? [yes]\n"
"                              full is slower but provides precise watchpoint/step\n"
"    --vgdb-error=<number>     invoke gdbserver after <number> errors [%d]\n"
"                              to get started quickly, use --vgdb-error=0\n"
"                              and follow the on-screen directions\n"
"    --track-fds=no|yes        track open file descriptors? [no]\n"
"    --time-stamp=no|yes       add timestamps to log messages? [no]\n"
"    --log-fd=<number>         log messages to file descriptor [2=stderr]\n"
"    --log-file=<file>         log messages to <file>\n"
"    --log-socket=ipaddr:port  log messages to socket ipaddr:port\n"
"\n"
"  user options for Valgrind tools that report errors:\n"
"    --xml=yes                 emit error output in XML (some tools only)\n"
"    --xml-fd=<number>         XML output to file descriptor\n"
"    --xml-file=<file>         XML output to <file>\n"
"    --xml-socket=ipaddr:port  XML output to socket ipaddr:port\n"
"    --xml-user-comment=STR    copy STR verbatim into XML output\n"
"    --demangle=no|yes         automatically demangle C++ names? [yes]\n"
"    --num-callers=<number>    show <number> callers in stack traces [12]\n"
"    --error-limit=no|yes      stop showing new errors if too many? [yes]\n"
"    --error-exitcode=<number> exit code to return if errors found [0=disable]\n"
"    --show-below-main=no|yes  continue stack traces below main() [no]\n"
"    --suppressions=<filename> suppress errors described in <filename>\n"
"    --gen-suppressions=no|yes|all    print suppressions for errors? [no]\n"
"    --db-attach=no|yes        start debugger when errors detected? [no]\n"
"    --db-command=<command>    command to start debugger [%s -nw %%f %%p]\n"
"    --input-fd=<number>       file descriptor for input [0=stdin]\n"
"    --dsymutil=no|yes         run dsymutil on Mac OS X when helpful? [no]\n"
"    --max-stackframe=<number> assume stack switch for SP changes larger\n"
"                              than <number> bytes [2000000]\n"
"    --main-stacksize=<number> set size of main thread's stack (in bytes)\n"
"                              [use current 'ulimit' value]\n"
"\n"
"  user options for Valgrind tools that replace malloc:\n"
"    --alignment=<number>      set minimum alignment of heap allocations [%s]\n"
"    --redzone-size=<number>   set minimum size of redzones added before/after\n"
"                              heap blocks (in bytes). [%s]\n"
"\n"
"  uncommon user options for all Valgrind tools:\n"
"    --fullpath-after=         (with nothing after the '=')\n"
"                              show full source paths in call stacks\n"
"    --fullpath-after=string   like --fullpath-after=, but only show the\n"
"                              part of the path after 'string'.  Allows removal\n"
"                              of path prefixes.  Use this flag multiple times\n"
"                              to specify a set of prefixes to remove.\n"
"    --smc-check=none|stack|all|all-non-file [stack]\n"
"                              checks for self-modifying code: none, only for\n"
"                              code found in stacks, for all code, or for all\n"
"                              code except that from file-backed mappings\n"
"    --read-var-info=yes|no    read debug info on stack and global variables\n"
"                              and use it to print better error messages in\n"
"                              tools that make use of it (Memcheck, Helgrind,\n"
"                              DRD) [no]\n"
"    --vgdb-poll=<number>      gdbserver poll max every <number> basic blocks [%d] \n"
"    --vgdb-shadow-registers=no|yes   let gdb see the shadow registers [no]\n"
"    --vgdb-prefix=<prefix>    prefix for vgdb FIFOs [%s]\n"
"    --run-libc-freeres=no|yes free up glibc memory at exit on Linux? [yes]\n"
"    --sim-hints=hint1,hint2,...  known hints:\n"
"                                 lax-ioctls, enable-outer, fuse-compatible [none]\n"
"    --fair-sched=no|yes|try   schedule threads fairly on multicore systems [no]\n"
"    --kernel-variant=variant1,variant2,...  known variants: bproc [none]\n"
"                              handle non-standard kernel variants\n"
"    --show-emwarns=no|yes     show warnings about emulation limits? [no]\n"
"    --require-text-symbol=:sonamepattern:symbolpattern    abort run if the\n"
"                              stated shared object doesn't have the stated\n"
"                              text symbol.  Patterns can contain ? and *.\n"
"    --soname-synonyms=syn1=pattern1,syn2=pattern2,... synonym soname\n"
"              specify patterns for function wrapping or replacement.\n"
"              To use a non-libc malloc library that is\n"
"                  in the main exe:  --soname-synonyms=somalloc=NONE\n"
"                  in libxyzzy.so:   --soname-synonyms=somalloc=libxyzzy.so\n"
"\n";

   Char* usage2 = 
"\n"
"  debugging options for all Valgrind tools:\n"
"    -d                        show verbose debugging output\n"
"    --stats=no|yes            show tool and core statistics [no]\n"
"    --sanity-level=<number>   level of sanity checking to do [1]\n"
"    --trace-flags=<XXXXXXXX>   show generated code? (X = 0|1) [00000000]\n"
"    --profile-flags=<XXXXXXXX> ditto, but for profiling (X = 0|1) [00000000]\n"
"    --trace-notbelow=<number> only show BBs above <number> [999999999]\n"
"    --trace-notabove=<number> only show BBs below <number> [0]\n"
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
"    --profile-heap=no|yes     profile Valgrind's own space use\n"
"    --core-redzone=<number>   set minimum size of redzones added before/after\n"
"                              heap blocks allocated for Valgrind internal use (in bytes) [4]\n"
"    --wait-for-gdb=yes|no     pause on startup to wait for gdb attach\n"
"    --sym-offsets=yes|no      show syms in form 'name+offset' ? [no]\n"
"    --command-line-only=no|yes  only use command line options [no]\n"
"\n"
"  Vex options for all Valgrind tools:\n"
"    --vex-iropt-verbosity=<0..9>           [0]\n"
"    --vex-iropt-level=<0..2>               [2]\n"
"    --vex-iropt-register-updates=unwindregs-at-mem-access\n"
"                                |allregs-at-mem-access\n"
"                                |allregs-at-each-insn  [unwindregs-at-mem-access]\n"
"    --vex-iropt-unroll-thresh=<0..400>     [120]\n"
"    --vex-guest-max-insns=<1..100>         [50]\n"
"    --vex-guest-chase-thresh=<0..99>       [10]\n"
"    --vex-guest-chase-cond=no|yes          [no]\n"
"    --trace-flags and --profile-flags values (omit the middle space):\n"
"       1000 0000   show conversion into IR\n"
"       0100 0000   show after initial opt\n"
"       0010 0000   show after instrumentation\n"
"       0001 0000   show after second opt\n"
"       0000 1000   show after tree building\n"
"       0000 0100   show selecting insns\n"
"       0000 0010   show after reg-alloc\n"
"       0000 0001   show final assembly\n"
"      (Nb: you need --trace-notbelow and/or --trace-notabove with --trace-flags for full details)\n"
"\n"
"  debugging options for Valgrind tools that report errors\n"
"    --dump-error=<number>     show translation for basic block associated\n"
"                              with <number>'th error context [0=show none]\n"
"\n"
"  debugging options for Valgrind tools that replace malloc:\n"
"    --trace-malloc=no|yes     show client malloc details? [no]\n"
"\n";

   Char* usage3 =
"\n"
"  Extra options read from ~/.valgrindrc, $VALGRIND_OPTS, ./.valgrindrc\n"
"\n"
"  %s is %s\n"
"  Valgrind is Copyright (C) 2000-2012, and GNU GPL'd, by Julian Seward et al.\n"
"  LibVEX is Copyright (C) 2004-2012, and GNU GPL'd, by OpenWorks LLP et al.\n"
"\n"
"  Bug reports, feedback, admiration, abuse, etc, to: %s.\n"
"\n";

   Char* gdb_path = GDB_PATH;
   Char default_alignment[30];
   Char default_redzone_size[30];

   // Ensure the message goes to stdout
   VG_(log_output_sink).fd = 1;
   VG_(log_output_sink).is_socket = False;

   if (VG_(needs).malloc_replacement) {
      VG_(sprintf)(default_alignment,    "%d",  VG_MIN_MALLOC_SZB);
      VG_(sprintf)(default_redzone_size, "%lu", VG_(tdict).tool_client_redzone_szB);
   } else {
      VG_(strcpy)(default_alignment,    "not used by this tool");
      VG_(strcpy)(default_redzone_size, "not used by this tool");
   }
   /* 'usage1' a type as described after each arg. */
   VG_(printf)(usage1, 
               VG_(clo_vgdb_error)        /* int */,
               gdb_path                   /* char* */,
               default_alignment          /* char* */,
               default_redzone_size       /* char* */,
               VG_(clo_vgdb_poll)         /* int */,
               VG_(vgdb_prefix_default)() /* char* */
               ); 
   if (VG_(details).name) {
      VG_(printf)("  user options for %s:\n", VG_(details).name);
      if (VG_(needs).command_line_options)
	 VG_TDICT_CALL(tool_print_usage);
      else
	 VG_(printf)("    (none)\n");
   }
   if (debug_help) {
      VG_(printf)("%s", usage2);

      if (VG_(details).name) {
         VG_(printf)("  debugging options for %s:\n", VG_(details).name);
      
         if (VG_(needs).command_line_options)
            VG_TDICT_CALL(tool_print_debug_usage);
         else
            VG_(printf)("    (none)\n");
      }
   }
   VG_(printf)(usage3, VG_(details).name, VG_(details).copyright_author,
               VG_BUGS_TO);
   VG_(exit)(0);
}


/* Peer at previously set up VG_(args_for_valgrind) and do some
   minimal command line processing that must happen early on:

   - show the version string, if requested (-v)
   - extract any request for help (--help, -h, --help-debug)
   - get the toolname (--tool=)
   - set VG_(clo_max_stackframe) (--max-stackframe=)
   - set VG_(clo_main_stacksize) (--main-stacksize=)
   - set VG_(clo_sim_hints) (--sim-hints=)

   That's all it does.  The main command line processing is done below
   by main_process_cmd_line_options.  Note that
   main_process_cmd_line_options has to handle but ignore the ones we
   have handled here.
*/
static void early_process_cmd_line_options ( /*OUT*/Int* need_help,
                                             /*OUT*/HChar** tool )
{
   UInt   i;
   HChar* str;

   vg_assert( VG_(args_for_valgrind) );

   /* parse the options we have (only the options we care about now) */
   for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {

      str = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      vg_assert(str);

      // Nb: the version string goes to stdout.
      if VG_XACT_CLO(str, "--version", VG_(log_output_sink).fd, 1) {
         VG_(log_output_sink).is_socket = False;
         VG_(printf)("valgrind-" VERSION "\n");
         VG_(exit)(0);
      }
      else if VG_XACT_CLO(str, "--help", *need_help, *need_help+1) {}
      else if VG_XACT_CLO(str, "-h",     *need_help, *need_help+1) {}

      else if VG_XACT_CLO(str, "--help-debug", *need_help, *need_help+2) {}

      // The tool has already been determined, but we need to know the name
      // here.
      else if VG_STR_CLO(str, "--tool", *tool) {} 

      // Set up VG_(clo_max_stackframe) and VG_(clo_main_stacksize).
      // These are needed by VG_(ii_create_image), which happens
      // before main_process_cmd_line_options().
      else if VG_INT_CLO(str, "--max-stackframe", VG_(clo_max_stackframe)) {}
      else if VG_INT_CLO(str, "--main-stacksize", VG_(clo_main_stacksize)) {}

      // Set up VG_(clo_sim_hints). This is needed a.o. for an inner
      // running in an outer, to have "no-inner-prefix" enabled
      // as early as possible.
      else if VG_STR_CLO (str, "--sim-hints",     VG_(clo_sim_hints)) {}
   }
}

/* The main processing for command line options.  See comments above
   on early_process_cmd_line_options.

   Comments on how the logging options are handled:

   User can specify:
      --log-fd=      for a fd to write to (default setting, fd = 2)
      --log-file=    for a file name to write to
      --log-socket=  for a socket to write to

   As a result of examining these and doing relevant socket/file
   opening, a final fd is established.  This is stored in
   VG_(log_output_sink) in m_libcprint.  Also, if --log-file=STR was
   specified, then STR, after expansion of %p and %q templates within
   it, is stored in VG_(clo_log_fname_expanded), in m_options, just in
   case anybody wants to know what it is.

   When printing, VG_(log_output_sink) is consulted to find the
   fd to send output to.

   Exactly analogous actions are undertaken for the XML output
   channel, with the one difference that the default fd is -1, meaning
   the channel is disabled by default.
*/
static
void main_process_cmd_line_options ( /*OUT*/Bool* logging_to_fd,
                                     /*OUT*/Char** xml_fname_unexpanded,
                                     const HChar* toolname )
{
   // VG_(clo_log_fd) is used by all the messaging.  It starts as 2 (stderr)
   // and we cannot change it until we know what we are changing it to is
   // ok.  So we have tmp_log_fd to hold the tmp fd prior to that point.
   SysRes sres;
   Int    i, tmp_log_fd, tmp_xml_fd;
   Int    toolname_len = VG_(strlen)(toolname);
   Char*  tmp_str;         // Used in a couple of places.
   enum {
      VgLogTo_Fd,
      VgLogTo_File,
      VgLogTo_Socket
   } log_to = VgLogTo_Fd,   // Where is logging output to be sent?
     xml_to = VgLogTo_Fd;   // Where is XML output to be sent?

   /* Temporarily holds the string STR specified with
      --{log,xml}-{name,socket}=STR.  'fs' stands for
      file-or-socket. */
   Char* log_fsname_unexpanded = NULL;
   Char* xml_fsname_unexpanded = NULL;

   /* Log to stderr by default, but usage message goes to stdout.  XML
      output is initially disabled. */
   tmp_log_fd = 2; 
   tmp_xml_fd = -1;
 
   /* Check for sane path in ./configure --prefix=... */
   if (VG_LIBDIR[0] != '/') 
      VG_(err_config_error)("Please use absolute paths in "
                            "./configure --prefix=... or --libdir=...\n");

   vg_assert( VG_(args_for_valgrind) );

   /* BEGIN command-line processing loop */

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
         if (VG_STREQN(2,            arg,                "--") && 
             VG_STREQN(toolname_len, arg+2,              toolname) &&
             VG_STREQN(1,            arg+2+toolname_len, ":"))
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
            arg = VG_(strdup)("main.mpclo.1", arg + toolname_len + 1);
            arg[0] = '-';
            arg[1] = '-';

         } else {
            // prefix doesn't match, skip to next arg
            continue;
         }
      }
      
      /* Ignore these options - they've already been handled */
      if      VG_STREQN( 7, arg, "--tool=")              {}
      else if VG_STREQN(20, arg, "--command-line-only=") {}
      else if VG_STREQ(     arg, "--")                   {}
      else if VG_STREQ(     arg, "-d")                   {}
      else if VG_STREQN(16, arg, "--max-stackframe")     {}
      else if VG_STREQN(16, arg, "--main-stacksize")     {}
      else if VG_STREQN(11, arg,  "--sim-hints")         {}
      else if VG_STREQN(14, arg, "--profile-heap")       {}
      else if VG_STREQN(14, arg, "--core-redzone-size")  {}
      else if VG_STREQN(14, arg, "--redzone-size")       {}

      /* Obsolete options. Report an error and exit */
      else if VG_STREQN(34, arg, "--vex-iropt-precise-memory-exns=no") {
         VG_(fmsg_bad_option)
            (arg,
             "--vex-iropt-precise-memory-exns is obsolete\n"
             "Use --vex-iropt-register-updates=unwindregs-at-mem-access instead\n");
      }
      else if VG_STREQN(35, arg, "--vex-iropt-precise-memory-exns=yes") {
         VG_(fmsg_bad_option)
            (arg,
             "--vex-iropt-precise-memory-exns is obsolete\n"
             "Use --vex-iropt-register-updates=allregs-at-mem-access instead\n"
             " (or --vex-iropt-register-updates=allregs-at-each-insn)\n");
      }

      // These options are new.
      else if (VG_STREQ(arg, "-v") ||
               VG_STREQ(arg, "--verbose"))
         VG_(clo_verbosity)++;

      else if (VG_STREQ(arg, "-q") ||
               VG_STREQ(arg, "--quiet"))
         VG_(clo_verbosity)--;

      else if VG_BOOL_CLO(arg, "--stats",          VG_(clo_stats)) {}
      else if VG_BOOL_CLO(arg, "--xml",            VG_(clo_xml))
         VG_(debugLog_setXml)(VG_(clo_xml));

      else if VG_XACT_CLO(arg, "--vgdb=no",        VG_(clo_vgdb), Vg_VgdbNo) {}
      else if VG_XACT_CLO(arg, "--vgdb=yes",       VG_(clo_vgdb), Vg_VgdbYes) {}
      else if VG_XACT_CLO(arg, "--vgdb=full",      VG_(clo_vgdb), Vg_VgdbFull) {
         /* automatically updates register values at each insn
            with --vgdb=full */
         VG_(clo_vex_control).iropt_register_updates 
            = VexRegUpdAllregsAtEachInsn;
      }
      else if VG_INT_CLO (arg, "--vgdb-poll",      VG_(clo_vgdb_poll)) {}
      else if VG_INT_CLO (arg, "--vgdb-error",     VG_(clo_vgdb_error)) {}
      else if VG_STR_CLO (arg, "--vgdb-prefix",    VG_(clo_vgdb_prefix)) {}
      else if VG_BOOL_CLO(arg, "--vgdb-shadow-registers",
                            VG_(clo_vgdb_shadow_registers)) {}
      else if VG_BOOL_CLO(arg, "--db-attach",      VG_(clo_db_attach)) {}
      else if VG_BOOL_CLO(arg, "--demangle",       VG_(clo_demangle)) {}
      else if VG_STR_CLO (arg, "--soname-synonyms",VG_(clo_soname_synonyms)) {}
      else if VG_BOOL_CLO(arg, "--error-limit",    VG_(clo_error_limit)) {}
      else if VG_INT_CLO (arg, "--error-exitcode", VG_(clo_error_exitcode)) {}
      else if VG_BOOL_CLO(arg, "--show-emwarns",   VG_(clo_show_emwarns)) {}

      else if VG_BOOL_CLO(arg, "--run-libc-freeres", VG_(clo_run_libc_freeres)) {}
      else if VG_BOOL_CLO(arg, "--show-below-main",  VG_(clo_show_below_main)) {}
      else if VG_BOOL_CLO(arg, "--time-stamp",       VG_(clo_time_stamp)) {}
      else if VG_BOOL_CLO(arg, "--track-fds",        VG_(clo_track_fds)) {}
      else if VG_BOOL_CLO(arg, "--trace-children",   VG_(clo_trace_children)) {}
      else if VG_BOOL_CLO(arg, "--child-silent-after-fork",
                            VG_(clo_child_silent_after_fork)) {}
      else if VG_STR_CLO(arg, "--fair-sched",        tmp_str) {
         if (VG_(strcmp)(tmp_str, "yes") == 0)
            VG_(clo_fair_sched) = enable_fair_sched;
         else if (VG_(strcmp)(tmp_str, "try") == 0)
            VG_(clo_fair_sched) = try_fair_sched;
         else if (VG_(strcmp)(tmp_str, "no") == 0)
            VG_(clo_fair_sched) = disable_fair_sched;
         else
            VG_(fmsg_bad_option)(arg, "");

      }
      else if VG_BOOL_CLO(arg, "--trace-sched",      VG_(clo_trace_sched)) {}
      else if VG_BOOL_CLO(arg, "--trace-signals",    VG_(clo_trace_signals)) {}
      else if VG_BOOL_CLO(arg, "--trace-symtab",     VG_(clo_trace_symtab)) {}
      else if VG_STR_CLO (arg, "--trace-symtab-patt", VG_(clo_trace_symtab_patt)) {}
      else if VG_BOOL_CLO(arg, "--trace-cfi",        VG_(clo_trace_cfi)) {}
      else if VG_XACT_CLO(arg, "--debug-dump=syms",  VG_(clo_debug_dump_syms),
                                                     True) {}
      else if VG_XACT_CLO(arg, "--debug-dump=line",  VG_(clo_debug_dump_line),
                                                     True) {}
      else if VG_XACT_CLO(arg, "--debug-dump=frames",
                               VG_(clo_debug_dump_frames), True) {}
      else if VG_BOOL_CLO(arg, "--trace-redir",      VG_(clo_trace_redir)) {}

      else if VG_BOOL_CLO(arg, "--trace-syscalls",   VG_(clo_trace_syscalls)) {}
      else if VG_BOOL_CLO(arg, "--wait-for-gdb",     VG_(clo_wait_for_gdb)) {}
      else if VG_STR_CLO (arg, "--db-command",       VG_(clo_db_command)) {}
      else if VG_BOOL_CLO(arg, "--sym-offsets",      VG_(clo_sym_offsets)) {}
      else if VG_BOOL_CLO(arg, "--read-var-info",    VG_(clo_read_var_info)) {}

      else if VG_INT_CLO (arg, "--dump-error",       VG_(clo_dump_error))   {}
      else if VG_INT_CLO (arg, "--input-fd",         VG_(clo_input_fd))     {}
      else if VG_INT_CLO (arg, "--sanity-level",     VG_(clo_sanity_level)) {}
      else if VG_BINT_CLO(arg, "--num-callers",      VG_(clo_backtrace_size), 1,
                                                     VG_DEEPEST_BACKTRACE) {}

      else if VG_XACT_CLO(arg, "--smc-check=none",  VG_(clo_smc_check),
                                                    Vg_SmcNone);
      else if VG_XACT_CLO(arg, "--smc-check=stack", VG_(clo_smc_check),
                                                    Vg_SmcStack);
      else if VG_XACT_CLO(arg, "--smc-check=all",   VG_(clo_smc_check),
                                                    Vg_SmcAll);
      else if VG_XACT_CLO(arg, "--smc-check=all-non-file",
                                                    VG_(clo_smc_check),
                                                    Vg_SmcAllNonFile);

      else if VG_STR_CLO (arg, "--kernel-variant",  VG_(clo_kernel_variant)) {}

      else if VG_BOOL_CLO(arg, "--dsymutil",        VG_(clo_dsymutil)) {}

      else if VG_STR_CLO (arg, "--trace-children-skip",
                               VG_(clo_trace_children_skip)) {}
      else if VG_STR_CLO (arg, "--trace-children-skip-by-arg",
                               VG_(clo_trace_children_skip_by_arg)) {}

      else if VG_BINT_CLO(arg, "--vex-iropt-verbosity",
                       VG_(clo_vex_control).iropt_verbosity, 0, 10) {}
      else if VG_BINT_CLO(arg, "--vex-iropt-level",
                       VG_(clo_vex_control).iropt_level, 0, 2) {}
      else if VG_XACT_CLO(arg, 
                       "--vex-iropt-register-updates=unwindregs-at-mem-access",
                       VG_(clo_vex_control).iropt_register_updates,
                       VexRegUpdUnwindregsAtMemAccess);
      else if VG_XACT_CLO(arg, 
                       "--vex-iropt-register-updates=allregs-at-mem-access",
                       VG_(clo_vex_control).iropt_register_updates,
                       VexRegUpdAllregsAtMemAccess);
      else if VG_XACT_CLO(arg, 
                       "--vex-iropt-register-updates=allregs-at-each-insn",
                       VG_(clo_vex_control).iropt_register_updates,
                       VexRegUpdAllregsAtEachInsn);
      else if VG_BINT_CLO(arg, "--vex-iropt-unroll-thresh",
                       VG_(clo_vex_control).iropt_unroll_thresh, 0, 400) {}
      else if VG_BINT_CLO(arg, "--vex-guest-max-insns",
                       VG_(clo_vex_control).guest_max_insns, 1, 100) {}
      else if VG_BINT_CLO(arg, "--vex-guest-chase-thresh",
                       VG_(clo_vex_control).guest_chase_thresh, 0, 99) {}
      else if VG_BOOL_CLO(arg, "--vex-guest-chase-cond",
                       VG_(clo_vex_control).guest_chase_cond) {}

      else if VG_INT_CLO(arg, "--log-fd", tmp_log_fd) {
         log_to = VgLogTo_Fd;
         log_fsname_unexpanded = NULL;
      }
      else if VG_INT_CLO(arg, "--xml-fd", tmp_xml_fd) {
         xml_to = VgLogTo_Fd;
         xml_fsname_unexpanded = NULL;
      }

      else if VG_STR_CLO(arg, "--log-file", log_fsname_unexpanded) {
         log_to = VgLogTo_File;
      }
      else if VG_STR_CLO(arg, "--xml-file", xml_fsname_unexpanded) {
         xml_to = VgLogTo_File;
      }
 
      else if VG_STR_CLO(arg, "--log-socket", log_fsname_unexpanded) {
         log_to = VgLogTo_Socket;
      }
      else if VG_STR_CLO(arg, "--xml-socket", xml_fsname_unexpanded) {
         xml_to = VgLogTo_Socket;
      }

      else if VG_STR_CLO(arg, "--xml-user-comment",
                              VG_(clo_xml_user_comment)) {}

      else if VG_STR_CLO(arg, "--suppressions", tmp_str) {
         if (VG_(clo_n_suppressions) >= VG_CLO_MAX_SFILES) {
            VG_(fmsg_bad_option)(arg,
               "Too many suppression files specified.\n"
               "Increase VG_CLO_MAX_SFILES and recompile.\n");
         }
         VG_(clo_suppressions)[VG_(clo_n_suppressions)] = tmp_str;
         VG_(clo_n_suppressions)++;
      }

      else if VG_STR_CLO (arg, "--fullpath-after", tmp_str) {
         if (VG_(clo_n_fullpath_after) >= VG_CLO_MAX_FULLPATH_AFTER) {
            VG_(fmsg_bad_option)(arg,
               "Too many --fullpath-after= specifications.\n"
               "Increase VG_CLO_MAX_FULLPATH_AFTER and recompile.\n");
         }
         VG_(clo_fullpath_after)[VG_(clo_n_fullpath_after)] = tmp_str;
         VG_(clo_n_fullpath_after)++;
      }

      else if VG_STR_CLO(arg, "--require-text-symbol", tmp_str) {
         if (VG_(clo_n_req_tsyms) >= VG_CLO_MAX_REQ_TSYMS) {
            VG_(fmsg_bad_option)(arg,
               "Too many --require-text-symbol= specifications.\n"
               "Increase VG_CLO_MAX_REQ_TSYMS and recompile.\n");
         }
         /* String needs to be of the form C?*C?*, where C is any
            character, but is the same both times.  Having it in this
            form facilitates finding the boundary between the sopatt
            and the fnpatt just by looking for the second occurrence
            of C, without hardwiring any assumption about what C
            is. */
         Char patt[7];
         Bool ok = True;
         ok = tmp_str && VG_(strlen)(tmp_str) > 0;
         if (ok) {
           patt[0] = patt[3] = tmp_str[0];
           patt[1] = patt[4] = '?';
           patt[2] = patt[5] = '*';
           patt[6] = 0;
           ok = VG_(string_match)(patt, tmp_str);
         }
         if (!ok) {
            VG_(fmsg_bad_option)(arg,
               "Invalid --require-text-symbol= specification.\n");
         }
         VG_(clo_req_tsyms)[VG_(clo_n_req_tsyms)] = tmp_str;
         VG_(clo_n_req_tsyms)++;
      }

      /* "stuvwxyz" --> stuvwxyz (binary) */
      else if VG_STR_CLO(arg, "--trace-flags", tmp_str) {
         Int j;
   
         if (8 != VG_(strlen)(tmp_str)) {
            VG_(fmsg_bad_option)(arg,
               "--trace-flags argument must have 8 digits\n");
         }
         for (j = 0; j < 8; j++) {
            if      ('0' == tmp_str[j]) { /* do nothing */ }
            else if ('1' == tmp_str[j]) VG_(clo_trace_flags) |= (1 << (7-j));
            else {
               VG_(fmsg_bad_option)(arg,
                  "--trace-flags argument can only contain 0s and 1s\n");
            }
         }
      }

      /* "stuvwxyz" --> stuvwxyz (binary) */
      else if VG_STR_CLO(arg, "--profile-flags", tmp_str) {
         Int j;
   
         if (8 != VG_(strlen)(tmp_str)) {
            VG_(fmsg_bad_option)(arg, 
               "--profile-flags argument must have 8 digits\n");
         }
         for (j = 0; j < 8; j++) {
            if      ('0' == tmp_str[j]) { /* do nothing */ }
            else if ('1' == tmp_str[j]) VG_(clo_profile_flags) |= (1 << (7-j));
            else {
               VG_(fmsg_bad_option)(arg,
                  "--profile-flags argument can only contain 0s and 1s\n");
            }
         }
      }

      else if VG_INT_CLO (arg, "--trace-notbelow", VG_(clo_trace_notbelow)) {}

      else if VG_INT_CLO (arg, "--trace-notabove", VG_(clo_trace_notabove)) {}

      else if VG_XACT_CLO(arg, "--gen-suppressions=no",
                               VG_(clo_gen_suppressions), 0) {}
      else if VG_XACT_CLO(arg, "--gen-suppressions=yes",
                               VG_(clo_gen_suppressions), 1) {}
      else if VG_XACT_CLO(arg, "--gen-suppressions=all",
                               VG_(clo_gen_suppressions), 2) {}

      else if ( ! VG_(needs).command_line_options
             || ! VG_TDICT_CALL(tool_process_cmd_line_option, arg) ) {
         VG_(fmsg_bad_option)(arg, "");
      }
   }

   /* END command-line processing loop */

   /* Determine the path prefix for vgdb */
   if (VG_(clo_vgdb_prefix) == NULL)
     VG_(clo_vgdb_prefix) = VG_(vgdb_prefix_default)();

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

   if (VG_(clo_trace_notbelow) == -1) {
     if (VG_(clo_trace_notabove) == -1) {
       /* [] */
       VG_(clo_trace_notbelow) = 2147483647;
       VG_(clo_trace_notabove) = 0;
     } else {
       /* [0 .. notabove] */
       VG_(clo_trace_notbelow) = 0;
     }
   } else {
     if (VG_(clo_trace_notabove) == -1) {
       /* [notbelow .. ]  */
       VG_(clo_trace_notabove) = 2147483647;
     } else {
       /* [notbelow .. notabove]  */
     }
   }

   VG_(dyn_vgdb_error) = VG_(clo_vgdb_error);

   if (VG_(clo_gen_suppressions) > 0 && 
       !VG_(needs).core_errors && !VG_(needs).tool_errors) {
      VG_(fmsg_bad_option)("--gen-suppressions=yes",
         "Can't use --gen-suppressions= with %s\n"
         "because it doesn't generate errors.\n", VG_(details).name);
   }

   /* If XML output is requested, check that the tool actually
      supports it. */
   if (VG_(clo_xml) && !VG_(needs).xml_output) {
      VG_(clo_xml) = False;
      VG_(fmsg_bad_option)("--xml=yes",
         "%s does not support XML output.\n", VG_(details).name); 
      /*NOTREACHED*/
   }

   vg_assert( VG_(clo_gen_suppressions) >= 0 );
   vg_assert( VG_(clo_gen_suppressions) <= 2 );

   /* If we've been asked to emit XML, mash around various other
      options so as to constrain the output somewhat, and to remove
      any need for user input during the run. 
   */
   if (VG_(clo_xml)) {

      /* We can't allow --gen-suppressions=yes, since that requires us
         to print the error and then ask the user if she wants a
         suppression for it, but in XML mode we won't print it until
         we know whether we also need to print a suppression.  Hence a
         circular dependency.  So disallow this.
         (--gen-suppressions=all is still OK since we don't need any
         user interaction in this case.) */
      if (VG_(clo_gen_suppressions) == 1) {
         VG_(fmsg_bad_option)(
            "--xml=yes together with --gen-suppressions=yes",
            "When --xml=yes is specified, --gen-suppressions=no\n"
            "or --gen-suppressions=all is allowed, but not "
            "--gen-suppressions=yes.\n");
      }

      /* We can't allow DB attaching (or we maybe could, but results
         could be chaotic ..) since it requires user input.  Hence
         disallow. */
      if (VG_(clo_db_attach)) {
         VG_(fmsg_bad_option)(
            "--xml=yes together with --db-attach=yes",
            "--db-attach=yes is not allowed with --xml=yes\n"
            "because it would require user input.\n");
      }

      /* Disallow dump_error in XML mode; sounds like a recipe for
         chaos.  No big deal; dump_error is a flag for debugging V
         itself. */
      if (VG_(clo_dump_error) > 0) {
         VG_(fmsg_bad_option)("--xml=yes together with --dump-error", "");
      }

      /* Disable error limits (this might be a bad idea!) */
      VG_(clo_error_limit) = False;
      /* Disable emulation warnings */

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
   
      So set up logging now.  After this is done, VG_(log_output_sink)
      and (if relevant) VG_(xml_output_sink) should be connected to
      whatever sink has been selected, and we indiscriminately chuck
      stuff into it without worrying what the nature of it is.  Oh the
      wonder of Unix streams. */

   vg_assert(VG_(log_output_sink).fd == 2 /* stderr */);
   vg_assert(VG_(log_output_sink).is_socket == False);
   vg_assert(VG_(clo_log_fname_expanded) == NULL);

   vg_assert(VG_(xml_output_sink).fd == -1 /* disabled */);
   vg_assert(VG_(xml_output_sink).is_socket == False);
   vg_assert(VG_(clo_xml_fname_expanded) == NULL);

   /* --- set up the normal text output channel --- */

   switch (log_to) {

      case VgLogTo_Fd: 
         vg_assert(log_fsname_unexpanded == NULL);
         break;

      case VgLogTo_File: {
         Char* logfilename;

         vg_assert(log_fsname_unexpanded != NULL);
         vg_assert(VG_(strlen)(log_fsname_unexpanded) <= 900); /* paranoia */

         // Nb: we overwrite an existing file of this name without asking
         // any questions.
         logfilename = VG_(expand_file_name)("--log-file",
                                             log_fsname_unexpanded);
         sres = VG_(open)(logfilename, 
                          VKI_O_CREAT|VKI_O_WRONLY|VKI_O_TRUNC, 
                          VKI_S_IRUSR|VKI_S_IWUSR);
         if (!sr_isError(sres)) {
            tmp_log_fd = sr_Res(sres);
            VG_(clo_log_fname_expanded) = logfilename;
         } else {
            VG_(fmsg)("can't create log file '%s': %s\n", 
                      logfilename, VG_(strerror)(sr_Err(sres)));
            VG_(exit)(1);
            /*NOTREACHED*/
         }
         break;
      }

      case VgLogTo_Socket: {
         vg_assert(log_fsname_unexpanded != NULL);
         vg_assert(VG_(strlen)(log_fsname_unexpanded) <= 900); /* paranoia */
         tmp_log_fd = VG_(connect_via_socket)( log_fsname_unexpanded );
         if (tmp_log_fd == -1) {
            VG_(fmsg)("Invalid --log-socket spec of '%s'\n",
                      log_fsname_unexpanded);
            VG_(exit)(1);
            /*NOTREACHED*/
	 }
         if (tmp_log_fd == -2) {
            VG_(umsg)("failed to connect to logging server '%s'.\n"
                      "Log messages will sent to stderr instead.\n",
                      log_fsname_unexpanded ); 

            /* We don't change anything here. */
            vg_assert(VG_(log_output_sink).fd == 2);
            tmp_log_fd = 2;
	 } else {
            vg_assert(tmp_log_fd > 0);
            VG_(log_output_sink).is_socket = True;
         }
         break;
      }
   }

   /* --- set up the XML output channel --- */

   switch (xml_to) {

      case VgLogTo_Fd: 
         vg_assert(xml_fsname_unexpanded == NULL);
         break;

      case VgLogTo_File: {
         Char* xmlfilename;

         vg_assert(xml_fsname_unexpanded != NULL);
         vg_assert(VG_(strlen)(xml_fsname_unexpanded) <= 900); /* paranoia */

         // Nb: we overwrite an existing file of this name without asking
         // any questions.
         xmlfilename = VG_(expand_file_name)("--xml-file",
                                             xml_fsname_unexpanded);
         sres = VG_(open)(xmlfilename, 
                          VKI_O_CREAT|VKI_O_WRONLY|VKI_O_TRUNC, 
                          VKI_S_IRUSR|VKI_S_IWUSR);
         if (!sr_isError(sres)) {
            tmp_xml_fd = sr_Res(sres);
            VG_(clo_xml_fname_expanded) = xmlfilename;
            /* strdup here is probably paranoid overkill, but ... */
            *xml_fname_unexpanded = VG_(strdup)( "main.mpclo.2",
                                                 xml_fsname_unexpanded );
         } else {
            VG_(fmsg)("can't create XML file '%s': %s\n", 
                      xmlfilename, VG_(strerror)(sr_Err(sres)));
            VG_(exit)(1);
            /*NOTREACHED*/
         }
         break;
      }

      case VgLogTo_Socket: {
         vg_assert(xml_fsname_unexpanded != NULL);
         vg_assert(VG_(strlen)(xml_fsname_unexpanded) <= 900); /* paranoia */
         tmp_xml_fd = VG_(connect_via_socket)( xml_fsname_unexpanded );
         if (tmp_xml_fd == -1) {
            VG_(fmsg)("Invalid --xml-socket spec of '%s'\n",
                      xml_fsname_unexpanded );
            VG_(exit)(1);
            /*NOTREACHED*/
	 }
         if (tmp_xml_fd == -2) {
            VG_(umsg)("failed to connect to XML logging server '%s'.\n"
                      "XML output will sent to stderr instead.\n",
                      xml_fsname_unexpanded); 
            /* We don't change anything here. */
            vg_assert(VG_(xml_output_sink).fd == 2);
            tmp_xml_fd = 2;
	 } else {
            vg_assert(tmp_xml_fd > 0);
            VG_(xml_output_sink).is_socket = True;
         }
         break;
      }
   }

   /* If we've got this far, and XML mode was requested, but no XML
      output channel appears to have been specified, just stop.  We
      could continue, and XML output will simply vanish into nowhere,
      but that is likely to confuse the hell out of users, which is
      distinctly Ungood. */
   if (VG_(clo_xml) && tmp_xml_fd == -1) {
      VG_(fmsg_bad_option)(
          "--xml=yes, but no XML destination specified",
          "--xml=yes has been specified, but there is no XML output\n"
          "destination.  You must specify an XML output destination\n"
          "using --xml-fd, --xml-file or --xml-socket.\n"
      );
   }

   // Finalise the output fds: the log fd ..

   if (tmp_log_fd >= 0) {
      // Move log_fd into the safe range, so it doesn't conflict with
      // any app fds.
      tmp_log_fd = VG_(fcntl)(tmp_log_fd, VKI_F_DUPFD, VG_(fd_hard_limit));
      if (tmp_log_fd < 0) {
         VG_(message)(Vg_UserMsg, "valgrind: failed to move logfile fd "
                                  "into safe range, using stderr\n");
         VG_(log_output_sink).fd = 2;   // stderr
         VG_(log_output_sink).is_socket = False;
      } else {
         VG_(log_output_sink).fd = tmp_log_fd;
         VG_(fcntl)(VG_(log_output_sink).fd, VKI_F_SETFD, VKI_FD_CLOEXEC);
      }
   } else {
      // If they said --log-fd=-1, don't print anything.  Plausible for use in
      // regression testing suites that use client requests to count errors.
      VG_(log_output_sink).fd = -1;
      VG_(log_output_sink).is_socket = False;
   }

   // Finalise the output fds: and the XML fd ..

   if (tmp_xml_fd >= 0) {
      // Move xml_fd into the safe range, so it doesn't conflict with
      // any app fds.
      tmp_xml_fd = VG_(fcntl)(tmp_xml_fd, VKI_F_DUPFD, VG_(fd_hard_limit));
      if (tmp_xml_fd < 0) {
         VG_(message)(Vg_UserMsg, "valgrind: failed to move XML file fd "
                                  "into safe range, using stderr\n");
         VG_(xml_output_sink).fd = 2;   // stderr
         VG_(xml_output_sink).is_socket = False;
      } else {
         VG_(xml_output_sink).fd = tmp_xml_fd;
         VG_(fcntl)(VG_(xml_output_sink).fd, VKI_F_SETFD, VKI_FD_CLOEXEC);
      }
   } else {
      // If they said --xml-fd=-1, don't print anything.  Plausible for use in
      // regression testing suites that use client requests to count errors.
      VG_(xml_output_sink).fd = -1;
      VG_(xml_output_sink).is_socket = False;
   }

   // Suppressions related stuff

   if (VG_(clo_n_suppressions) < VG_CLO_MAX_SFILES-1 &&
       (VG_(needs).core_errors || VG_(needs).tool_errors)) {
      /* If we haven't reached the max number of suppressions, load
         the default one. */
      static const Char default_supp[] = "default.supp";
      Int len = VG_(strlen)(VG_(libdir)) + 1 + sizeof(default_supp);
      Char *buf = VG_(arena_malloc)(VG_AR_CORE, "main.mpclo.3", len);
      VG_(sprintf)(buf, "%s/%s", VG_(libdir), default_supp);
      VG_(clo_suppressions)[VG_(clo_n_suppressions)] = buf;
      VG_(clo_n_suppressions)++;
   }

   *logging_to_fd = log_to == VgLogTo_Fd || log_to == VgLogTo_Socket;
}

// Write the name and value of log file qualifiers to the xml file.
static void print_file_vars(Char* format)
{
   Int i = 0;
   
   while (format[i]) {
      if (format[i] == '%') {
         // We saw a '%'.  What's next...
         i++;
	 if ('q' == format[i]) {
            i++;
            if ('{' == format[i]) {
	       // Get the env var name, print its contents.
	       Char* qualname;
               Char* qual;
               i++;
               qualname = &format[i];
               while (True) {
		  if ('}' == format[i]) {
                     // Temporarily replace the '}' with NUL to extract var
                     // name.
		     format[i] = 0;
                     qual = VG_(getenv)(qualname);
		     break;
                  }
                  i++;
               }

               VG_(printf_xml)(
                  "<logfilequalifier> <var>%pS</var> "
                  "<value>%pS</value> </logfilequalifier>\n",
                  qualname,qual
               );
	       format[i] = '}';
	       i++;
	    }
         }
      } else {
	 i++;
      }
   }
}


/*====================================================================*/
/*=== Printing the preamble                                        ===*/
/*====================================================================*/

// Print the argument, escaping any chars that require it.
static void umsg_arg(const Char* arg)
{
   SizeT len = VG_(strlen)(arg);
   Char* special = " \\<>";
   Int i;
   for (i = 0; i < len; i++) {
      if (VG_(strchr)(special, arg[i])) {
         VG_(umsg)("\\");   // escape with a backslash if necessary
      }
      VG_(umsg)("%c", arg[i]);
   }
}

// Send output to the XML-stream and escape any XML meta-characters.
static void xml_arg(const Char* arg)
{
   VG_(printf_xml)("%pS", arg);
}

/* Ok, the logging sink is running now.  Print a suitable preamble.
   If logging to file or a socket, write details of parent PID and
   command line args, to help people trying to interpret the
   results of a run which encompasses multiple processes. */
static void print_preamble ( Bool logging_to_fd, 
                             Char* xml_fname_unexpanded,
                             const HChar* toolname )
{
   Int    i;
   HChar* xpre  = VG_(clo_xml) ? "  <line>" : "";
   HChar* xpost = VG_(clo_xml) ? "</line>" : "";
   UInt (*umsg_or_xml)( const HChar*, ... )
      = VG_(clo_xml) ? VG_(printf_xml) : VG_(umsg);

   void (*umsg_or_xml_arg)( const Char* )
      = VG_(clo_xml) ? xml_arg : umsg_arg;

   vg_assert( VG_(args_for_client) );
   vg_assert( VG_(args_for_valgrind) );
   vg_assert( toolname );

   if (VG_(clo_xml)) {
      VG_(printf_xml)("<?xml version=\"1.0\"?>\n");
      VG_(printf_xml)("\n");
      VG_(printf_xml)("<valgrindoutput>\n");
      VG_(printf_xml)("\n");
      VG_(printf_xml)("<protocolversion>4</protocolversion>\n");
      VG_(printf_xml)("<protocoltool>%s</protocoltool>\n", toolname);
      VG_(printf_xml)("\n");
   }

   if (VG_(clo_xml) || VG_(clo_verbosity > 0)) {

      if (VG_(clo_xml))
         VG_(printf_xml)("<preamble>\n");

      /* Tool details */
      umsg_or_xml( VG_(clo_xml) ? "%s%pS%pS%pS, %pS%s\n" : "%s%s%s%s, %s%s\n",
                   xpre,
                   VG_(details).name, 
                   NULL == VG_(details).version ? "" : "-",
                   NULL == VG_(details).version 
                      ? (Char*)"" : VG_(details).version,
                   VG_(details).description,
                   xpost );

      if (VG_(strlen)(toolname) >= 4 && VG_STREQN(4, toolname, "exp-")) {
         umsg_or_xml(
            "%sNOTE: This is an Experimental-Class Valgrind Tool%s\n",
            xpre, xpost
         );
      }

      umsg_or_xml( VG_(clo_xml) ? "%s%pS%s\n" : "%s%s%s\n",
                   xpre, VG_(details).copyright_author, xpost );

      /* Core details */
      umsg_or_xml(
         "%sUsing Valgrind-%s and LibVEX; rerun with -h for copyright info%s\n",
         xpre, VERSION, xpost
      );

      // Print the command line.  At one point we wrapped at 80 chars and
      // printed a '\' as a line joiner, but that makes it hard to cut and
      // paste the command line (because of the "==pid==" prefixes), so we now
      // favour utility and simplicity over aesthetics.
      umsg_or_xml("%sCommand: ", xpre);
      if (VG_(args_the_exename))
         umsg_or_xml_arg(VG_(args_the_exename));
          
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
         HChar* s = *(HChar**)VG_(indexXA)( VG_(args_for_client), i );
         umsg_or_xml(" ");
         umsg_or_xml_arg(s);
      }
      umsg_or_xml("%s\n", xpost);

      if (VG_(clo_xml))
         VG_(printf_xml)("</preamble>\n");
   }

   // Print the parent PID, and other stuff, if necessary.
   if (!VG_(clo_xml) && VG_(clo_verbosity) > 0 && !logging_to_fd) {
      VG_(umsg)("Parent PID: %d\n", VG_(getppid)());
   }
   else
   if (VG_(clo_xml)) {
      VG_(printf_xml)("\n");
      VG_(printf_xml)("<pid>%d</pid>\n", VG_(getpid)());
      VG_(printf_xml)("<ppid>%d</ppid>\n", VG_(getppid)());
      VG_(printf_xml)("<tool>%pS</tool>\n", toolname);
      if (xml_fname_unexpanded)
         print_file_vars(xml_fname_unexpanded);
      if (VG_(clo_xml_user_comment)) {
         /* Note: the user comment itself is XML and is therefore to
            be passed through verbatim (%s) rather than escaped
            (%pS). */
         VG_(printf_xml)("<usercomment>%s</usercomment>\n",
                         VG_(clo_xml_user_comment));
      }
      VG_(printf_xml)("\n");
      VG_(printf_xml)("<args>\n");

      VG_(printf_xml)("  <vargv>\n");
      if (VG_(name_of_launcher))
         VG_(printf_xml)("    <exe>%pS</exe>\n",
                                VG_(name_of_launcher));
      else
         VG_(printf_xml)("    <exe>%pS</exe>\n",
                                "(launcher name unknown)");
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         VG_(printf_xml)(
            "    <arg>%pS</arg>\n",
            * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i )
         );
      }
      VG_(printf_xml)("  </vargv>\n");

      VG_(printf_xml)("  <argv>\n");
      if (VG_(args_the_exename))
         VG_(printf_xml)("    <exe>%pS</exe>\n",
                                VG_(args_the_exename));
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
         VG_(printf_xml)(
            "    <arg>%pS</arg>\n",
            * (HChar**) VG_(indexXA)( VG_(args_for_client), i )
         );
      }
      VG_(printf_xml)("  </argv>\n");

      VG_(printf_xml)("</args>\n");
   }

   // Last thing in the preamble is a blank line.
   if (VG_(clo_xml))
      VG_(printf_xml)("\n");
   else if (VG_(clo_verbosity) > 0)
      VG_(umsg)("\n");

#  if defined(VGO_darwin) && DARWIN_VERS == DARWIN_10_8
   /* Uh, this doesn't play nice with XML output. */
   umsg_or_xml( "WARNING: Support on MacOS 10.8 is experimental and mostly broken.\n");
   umsg_or_xml( "WARNING: Expect incorrect results, assertions and crashes.\n");
   umsg_or_xml( "WARNING: In particular, Memcheck on 32-bit programs will fail to\n");
   umsg_or_xml( "WARNING: detect any errors associated with heap-allocated data.\n");
   umsg_or_xml( "\n" );
#  endif

   if (VG_(clo_verbosity) > 1) {
      SysRes fd;
      VexArch vex_arch;
      VexArchInfo vex_archinfo;
      if (!logging_to_fd)
         VG_(message)(Vg_DebugMsg, "\n");
      VG_(message)(Vg_DebugMsg, "Valgrind options:\n");
      for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         VG_(message)(Vg_DebugMsg, 
                     "   %s\n", 
                     * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i ));
      }

      VG_(message)(Vg_DebugMsg, "Contents of /proc/version:\n");
      fd = VG_(open) ( "/proc/version", VKI_O_RDONLY, 0 );
      if (sr_isError(fd)) {
         VG_(message)(Vg_DebugMsg, "  can't open /proc/version\n");
      } else {
#        define BUF_LEN    256
         Char version_buf[BUF_LEN];
         Int n = VG_(read) ( sr_Res(fd), version_buf, BUF_LEN );
         vg_assert(n <= BUF_LEN);
         if (n > 0) {
            version_buf[n-1] = '\0';
            VG_(message)(Vg_DebugMsg, "  %s\n", version_buf);
         } else {
            VG_(message)(Vg_DebugMsg, "  (empty?)\n");
         }
         VG_(close)(sr_Res(fd));
#        undef BUF_LEN
      }

      VG_(machine_get_VexArchInfo)( &vex_arch, &vex_archinfo );
      VG_(message)(
         Vg_DebugMsg, 
         "Arch and hwcaps: %s, %s\n",
         LibVEX_ppVexArch   ( vex_arch ),
         LibVEX_ppVexHwCaps ( vex_arch, vex_archinfo.hwcaps )
      );
      VG_(message)(
         Vg_DebugMsg, 
         "Page sizes: currently %d, max supported %d\n", 
         (Int)VKI_PAGE_SIZE, (Int)VKI_MAX_PAGE_SIZE
      );
      VG_(message)(Vg_DebugMsg,
                   "Valgrind library directory: %s\n", VG_(libdir));
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

#  if defined(VGO_darwin)
   /* Darwin lies. It reports file max as RLIM_INFINITY but
      silently disallows anything bigger than 10240. */
   if (rl.rlim_cur >= 10240  &&  rl.rlim_max == 0x7fffffffffffffffULL) {
      rl.rlim_max = 10240;
   }
#  endif

   if (show)
      VG_(printf)("fd limits: host, before: cur %lu max %lu\n", 
                  (UWord)rl.rlim_cur, (UWord)rl.rlim_max);

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
      VG_(printf)("fd limits: host,  after: cur %lu max %lu\n",
                  (UWord)rl.rlim_cur, (UWord)rl.rlim_max);
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
      VG_(discard_translations)(tops[r].addr, 1, "bb profile");
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


/* A simple pair structure, used for conveying debuginfo handles to
   calls to VG_TRACK(new_mem_startup, ...). */
typedef  struct { Addr a; ULong ull; }  Addr_n_ULong;


/* --- Forwards decls to do with shutdown --- */

static void final_tidyup(ThreadId tid); 

/* Do everything which needs doing when the last thread exits */
static 
void shutdown_actions_NORETURN( ThreadId tid, 
                                VgSchedReturnCode tids_schedretcode );

/* --- end of Forwards decls to do with shutdown --- */


/* By the time we get to valgrind_main, the_iicii should already have
   been filled in with any important details as required by whatever
   OS we have been built for.
*/
static
Int valgrind_main ( Int argc, HChar **argv, HChar **envp )
{
   HChar*  toolname           = "memcheck";    // default to Memcheck
   Int     need_help          = 0; // 0 = no, 1 = --help, 2 = --help-debug
   ThreadId tid_main          = VG_INVALID_THREADID;
   Bool    logging_to_fd      = False;
   Char* xml_fname_unexpanded = NULL;
   Int     loglevel, i;
   struct vki_rlimit zero = { 0, 0 };
   XArray* addr2dihandle = NULL;

   // For an inner Valgrind, register the interim stack asap.
   // This is needed to allow the outer valgrind to do stacktraces during init.
   // Note that this stack is not unregistered when the main thread
   // is switching to the (real) stack. Unregistering this would imply
   // to save the stack id in a global variable, and have a "if"
   // in run_a_thread_NORETURN to do the unregistration only for the
   // main thread. This unregistration is not worth this complexity.
   INNER_REQUEST
      ((void) VALGRIND_STACK_REGISTER
       (&VG_(interim_stack).bytes[0],
        &VG_(interim_stack).bytes[0] + sizeof(VG_(interim_stack))));


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
   // Start up Mach kernel interface, if any
   //   p: none
   //--------------------------------------------------------------
#  if defined(VGO_darwin)
   VG_(mach_init)();
#  endif

   //--------------------------------------------------------------
   // Start up the logging mechanism
   //   p: none
   //--------------------------------------------------------------
   /* Start the debugging-log system ASAP.  First find out how many 
      "-d"s were specified.  This is a pre-scan of the command line.  Also
      get --profile-heap=yes, --core-redzone-size, --redzone-size which are
      needed by the time we start up dynamic memory management.  */
   loglevel = 0;
   for (i = 1; i < argc; i++) {
      if (argv[i][0] != '-') break;
      if VG_STREQ(argv[i], "--") break;
      if VG_STREQ(argv[i], "-d") loglevel++;
      if VG_BOOL_CLO(argv[i], "--profile-heap", VG_(clo_profile_heap)) {}
      if VG_BINT_CLO(argv[i], "--core-redzone-size", VG_(clo_core_redzone_size),
                     0, MAX_CLO_REDZONE_SZB) {}
      if VG_BINT_CLO(argv[i], "--redzone-size", VG_(clo_redzone_size),
                     0, MAX_CLO_REDZONE_SZB) {}
   }

   /* ... and start the debug logger.  Now we can safely emit logging
      messages all through startup. */
   VG_(debugLog_startup)(loglevel, "Stage 2 (main)");
   VG_(debugLog)(1, "main", "Welcome to Valgrind version " 
                            VERSION " debug logging\n");

   //--------------------------------------------------------------
   // Ensure we're on a plausible stack.
   //   p: logging
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Checking current stack is plausible\n");
   { HChar* limLo  = (HChar*)(&VG_(interim_stack).bytes[0]);
     HChar* limHi  = limLo + sizeof(VG_(interim_stack));
     HChar* volatile 
            aLocal = (HChar*)&limLo; /* any auto local will do */
     /* Re "volatile": Apple clang version 4.0
        (tags/Apple/clang-421.0.57) (based on LLVM 3.1svn)" appeared
        to miscompile the following check, causing run to abort at
        this point (in 64-bit mode) even though aLocal is within limLo
        .. limHi.  But in fact clang is within its rights to do
        strange things here.  "The reason is that the comparisons
        aLocal < limLo and aLocal >= limHi cause undefined behaviour
        (according to c99 6.5.8) because they compare pointers that do
        not point into the same aggregate."  Adding "volatile" appears
        to fix it because "The compiler would have to prove that there
        is undefined behavior in order to exploit it.  But as a
        volatile variable can change its value in ways invisible to
        the compiler, the compiler must make the conservative
        assumption that it points into the same aggregate as the other
        pointer its compared against.  I.e. the behaviour is possibly
        defined." (Analysis by Florian Krohm). */
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
   vg_assert(VKI_PAGE_SIZE     == 4096 || VKI_PAGE_SIZE     == 65536
             || VKI_PAGE_SIZE     == 16384);
   vg_assert(VKI_MAX_PAGE_SIZE == 4096 || VKI_MAX_PAGE_SIZE == 65536
             || VKI_MAX_PAGE_SIZE == 16384);
   vg_assert(VKI_PAGE_SIZE <= VKI_MAX_PAGE_SIZE);
   vg_assert(VKI_PAGE_SIZE     == (1 << VKI_PAGE_SHIFT));
   vg_assert(VKI_MAX_PAGE_SIZE == (1 << VKI_MAX_PAGE_SHIFT));
   the_iicii.clstack_top = VG_(am_startup)( the_iicii.sp_at_startup );
   VG_(debugLog)(1, "main", "Address space manager is running\n");

   //--------------------------------------------------------------
   // Start up the dynamic memory manager
   //   p: address space management
   //   p: getting --profile-heap,--core-redzone-size,--redzone-size
   //   In fact m_mallocfree is self-initialising, so there's no
   //   initialisation call to do.  Instead, try a simple malloc/
   //   free pair right now to check that nothing is broken.
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Starting the dynamic memory manager\n");
   { void* p = VG_(malloc)( "main.vm.1", 12345 );
     if (p) VG_(free)( p );
   }
   VG_(debugLog)(1, "main", "Dynamic memory manager is running\n");

   //============================================================
   //
   // Dynamic memory management is now available.
   //
   //============================================================

   //--------------------------------------------------------------
   // Initialise m_debuginfo
   //  p: dynamic memory allocation
   VG_(debugLog)(1, "main", "Initialise m_debuginfo\n");
   VG_(di_initialise)();

   //--------------------------------------------------------------
   // Look for alternative libdir                                  
   { HChar *cp = VG_(getenv)(VALGRIND_LIB);
     if (cp != NULL)
        VG_(libdir) = cp;
     VG_(debugLog)(1, "main", "VG_(libdir) = %s\n", VG_(libdir));
   }

   //--------------------------------------------------------------
   // Extract the launcher name from the environment.
   VG_(debugLog)(1, "main", "Getting launcher's name ...\n");
   VG_(name_of_launcher) = VG_(getenv)(VALGRIND_LAUNCHER);
   if (VG_(name_of_launcher) == NULL) {
      VG_(printf)("valgrind: You cannot run '%s' directly.\n", argv[0]);
      VG_(printf)("valgrind: You should use $prefix/bin/valgrind.\n");
      VG_(exit)(1);
   }
   VG_(debugLog)(1, "main", "... %s\n", VG_(name_of_launcher));

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
        VG_(printf)("   * System z (64bit only - s390x; z900 and above)\n");
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

   //--------------------------------------------------------------
   // Record the working directory at startup
   //   p: none
   VG_(debugLog)(1, "main", "Getting the working directory at startup\n");
   { Bool ok = VG_(record_startup_wd)();
     if (!ok) 
        VG_(err_config_error)( "Can't establish current working "
                               "directory at startup\n");
   }
   { Char buf[VKI_PATH_MAX+1];
     Bool ok = VG_(get_startup_wd)( buf, sizeof(buf) );
     vg_assert(ok);
     buf[VKI_PATH_MAX] = 0;
     VG_(debugLog)(1, "main", "... %s\n", buf );
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

   //--------------------------------------------------------------
   // Extract tool name and whether help has been requested.
   // Note we can't print the help message yet, even if requested,
   // because the tool has not been initialised.
   //   p: split_up_argv [for VG_(args_for_valgrind)]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main",
                    "(early_) Process Valgrind's command line options\n");
   early_process_cmd_line_options(&need_help, &toolname);

   // Set default vex control params
   LibVEX_default_VexControl(& VG_(clo_vex_control));

   //--------------------------------------------------------------
   // Load client executable, finding in $PATH if necessary
   //   p: early_process_cmd_line_options()  [for 'exec', 'need_help',
   //                                         clo_max_stackframe,
   //                                         clo_main_stacksize]
   //   p: layout_remaining_space            [so there's space]
   //
   // Set up client's environment
   //   p: set-libdir                     [for VG_(libdir)]
   //   p: early_process_cmd_line_options [for toolname]
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

#     if defined(VGO_linux) || defined(VGO_darwin)
      the_iicii.argv              = argv;
      the_iicii.envp              = envp;
      the_iicii.toolname          = toolname;
#     else
#       error "Unknown platform"
#     endif

      /* NOTE: this call reads VG_(clo_main_stacksize). */
      the_iifii = VG_(ii_create_image)( the_iicii );
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
#if !defined(VGO_linux)
   // client shouldn't be using /proc!
   VG_(cl_cmdline_fd) = -1;
#else
   if (!need_help) {
      HChar  buf[50], buf2[50+64];
      HChar  nul[1];
      Int    fd, r;
      const HChar* exename;

      VG_(debugLog)(1, "main", "Create fake /proc/<pid>/cmdline\n");

      VG_(sprintf)(buf, "proc_%d_cmdline", VG_(getpid)());
      fd = VG_(mkstemp)( buf, buf2 );
      if (fd == -1)
         VG_(err_config_error)("Can't create client cmdline file in %s\n", buf2);

      nul[0] = 0;
      exename = VG_(args_the_exename) ? VG_(args_the_exename)
                                      : "unknown_exename";
      VG_(write)(fd, exename, VG_(strlen)( exename ));
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
         VG_(err_config_error)("Can't delete client cmdline file in %s\n", buf2);

      VG_(cl_cmdline_fd) = fd;
   }
#endif

   //--------------------------------------------------------------
   // Init tool part 1: pre_clo_init
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise the tool part 1 (pre_clo_init)\n");
   VG_(tl_pre_clo_init)();

   //--------------------------------------------------------------
   // If --tool and --help/--help-debug was given, now give the core+tool
   // help message
   //   p: early_process_cmd_line_options() [for 'need_help']
   //   p: tl_pre_clo_init                  [for 'VG_(tdict).usage']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Print help and quit, if requested\n");
   if (need_help) {
      usage_NORETURN(/*--help-debug?*/need_help >= 2);
   }

   //--------------------------------------------------------------
   // Process command line options to Valgrind + tool
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main",
                    "(main_) Process Valgrind's command line options, "
                    "setup logging\n");
   main_process_cmd_line_options ( &logging_to_fd, &xml_fname_unexpanded,
                                   toolname );

   //--------------------------------------------------------------
   // Zeroise the millisecond counter by doing a first read of it.
   //   p: none
   //--------------------------------------------------------------
   (void) VG_(read_millisecond_timer)();

   //--------------------------------------------------------------
   // Print the preamble
   //   p: tl_pre_clo_init            [for 'VG_(details).name' and friends]
   //   p: main_process_cmd_line_options()
   //         [for VG_(clo_verbosity), VG_(clo_xml),
   //          logging_to_fd, xml_fname_unexpanded]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Print the preamble...\n");
   print_preamble(logging_to_fd, xml_fname_unexpanded, toolname);
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
   {
      /* The tool's "needs" will by now be finalised, since it has no
         further opportunity to specify them.  So now sanity check
         them. */
      Char* s;
      Bool  ok;
      ok = VG_(sanity_check_needs)( &s );
      if (!ok) {
         VG_(tool_panic)(s);
      }
   }

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
   //   p: main_process_cmd_line_options()  [for VG_(clo_wait_for_gdb)]
   //--------------------------------------------------------------
   /* Hook to delay things long enough so we can get the pid and
      attach GDB in another shell. */
   if (VG_(clo_wait_for_gdb)) {
      ULong iters, q;
      VG_(debugLog)(1, "main", "Wait for GDB\n");
      VG_(printf)("pid=%d, entering delay loop\n", VG_(getpid)());

#     if defined(VGP_x86_linux)
      iters = 10;
#     elif defined(VGP_amd64_linux) || defined(VGP_ppc64_linux)
      iters = 10;
#     elif defined(VGP_ppc32_linux)
      iters = 5;
#     elif defined(VGP_arm_linux)
      iters = 5;
#     elif defined(VGP_s390x_linux)
      iters = 10;
#     elif defined(VGP_mips32_linux)
      iters = 10;
#     elif defined(VGO_darwin)
      iters = 3;
#     else
#       error "Unknown plat"
#     endif

      iters *= 1000ULL * 1000 * 1000;
      for (q = 0; q < iters; q++) 
         __asm__ __volatile__("" ::: "memory","cc");
   }

   //--------------------------------------------------------------
   // Search for file descriptors that are inherited from our parent
   //   p: main_process_cmd_line_options  [for VG_(clo_track_fds)]
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
   //   p: initialise m_debuginfo
   //
   // While doing this, make a note of the debuginfo-handles that
   // come back from VG_(di_notify_mmap).
   // Later, in "Tell the tool about the initial client memory permissions"
   // (just below) we can then hand these handles off to the tool in
   // calls to VG_TRACK(new_mem_startup, ...).  This gives the tool the
   // opportunity to make further queries to m_debuginfo before the
   // client is started, if it wants.  We put this information into an
   // XArray, each handle along with the associated segment start address,
   // and search the XArray for the handles later, when calling
   // VG_TRACK(new_mem_startup, ...).
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Load initial debug info\n");

   tl_assert(!addr2dihandle);
   addr2dihandle = VG_(newXA)( VG_(malloc), "main.vm.2",
                               VG_(free), sizeof(Addr_n_ULong) );
   tl_assert(addr2dihandle);

#  if defined(VGO_linux)
   { Addr* seg_starts;
     Int   n_seg_starts;
     Addr_n_ULong anu;

     seg_starts = VG_(get_segment_starts)( &n_seg_starts );
     vg_assert(seg_starts && n_seg_starts >= 0);

     /* show them all to the debug info reader.  allow_SkFileV has to
        be True here so that we read info from the valgrind executable
        itself. */
     for (i = 0; i < n_seg_starts; i++) {
        anu.ull = VG_(di_notify_mmap)( seg_starts[i], True/*allow_SkFileV*/,
                                       -1/*Don't use_fd*/);
        /* anu.ull holds the debuginfo handle returned by di_notify_mmap,
           if any. */
        if (anu.ull > 0) {
           anu.a = seg_starts[i];
           VG_(addToXA)( addr2dihandle, &anu );
        }
     }

     VG_(free)( seg_starts );
   }
#  elif defined(VGO_darwin)
   { Addr* seg_starts;
     Int   n_seg_starts;
     seg_starts = VG_(get_segment_starts)( &n_seg_starts );
     vg_assert(seg_starts && n_seg_starts >= 0);

     /* show them all to the debug info reader.  
        Don't read from V segments (unlike Linux) */
     // GrP fixme really?
     for (i = 0; i < n_seg_starts; i++) {
        VG_(di_notify_mmap)( seg_starts[i], False/*don't allow_SkFileV*/,
                             -1/*don't use_fd*/);
     }

     VG_(free)( seg_starts );
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

   if (VG_(clo_xml)) {
      HChar buf[50];
      VG_(elapsed_wallclock_time)(buf);
      VG_(printf_xml)( "<status>\n"
                       "  <state>RUNNING</state>\n"
                       "  <time>%pS</time>\n"
                       "</status>\n",
                       buf );
      VG_(printf_xml)( "\n" );
   }

   VG_(init_Threads)();

   //--------------------------------------------------------------
   // Initialise the scheduler (phase 1) [generates tid_main]
   //   p: none, afaics
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise scheduler (phase 1)\n");
   tid_main = VG_(scheduler_init_phase1)();
   vg_assert(tid_main >= 0 && tid_main < VG_N_THREADS
             && tid_main != VG_INVALID_THREADID);
   /* Tell the tool about tid_main */
   VG_TRACK( pre_thread_ll_create, VG_INVALID_THREADID, tid_main );
   
   //--------------------------------------------------------------
   // Tell the tool about the initial client memory permissions
   //   p: aspacem
   //   p: mallocfree
   //   p: setup_client_stack
   //   p: setup_client_dataseg
   //
   // For each segment we tell the client about, look up in 
   // addr2dihandle as created above, to see if there's a debuginfo
   // handle associated with the segment, that we can hand along
   // to the tool, to be helpful.
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Tell tool about initial permissions\n");
   { Addr*     seg_starts;
     Int       n_seg_starts;

     tl_assert(addr2dihandle);

     /* Mark the main thread as running while we tell the tool about
        the client memory so that the tool can associate that memory
        with the main thread. */
     tl_assert(VG_(running_tid) == VG_INVALID_THREADID);
     VG_(running_tid) = tid_main;

     seg_starts = VG_(get_segment_starts)( &n_seg_starts );
     vg_assert(seg_starts && n_seg_starts >= 0);

     /* show interesting ones to the tool */
     for (i = 0; i < n_seg_starts; i++) {
        Word j, n;
        NSegment const* seg 
           = VG_(am_find_nsegment)( seg_starts[i] );
        vg_assert(seg);
        if (seg->kind == SkFileC || seg->kind == SkAnonC) {
          /* This next assertion is tricky.  If it is placed
             immediately before this 'if', it very occasionally fails.
             Why?  Because previous iterations of the loop may have
             caused tools (via the new_mem_startup calls) to do
             dynamic memory allocation, and that may affect the mapped
             segments; in particular it may cause segment merging to
             happen.  Hence we cannot assume that seg_starts[i], which
             reflects the state of the world before we started this
             loop, is the same as seg->start, as the latter reflects
             the state of the world (viz, mappings) at this particular
             iteration of the loop.

             Why does moving it inside the 'if' make it safe?  Because
             any dynamic memory allocation done by the tools will
             affect only the state of Valgrind-owned segments, not of
             Client-owned segments.  And the 'if' guards against that
             -- we only get in here for Client-owned segments.

             In other words: the loop may change the state of
             Valgrind-owned segments as it proceeds.  But it should
             not cause the Client-owned segments to change. */
           vg_assert(seg->start == seg_starts[i]);
           VG_(debugLog)(2, "main", 
                            "tell tool about %010lx-%010lx %c%c%c\n",
                             seg->start, seg->end,
                             seg->hasR ? 'r' : '-',
                             seg->hasW ? 'w' : '-',
                             seg->hasX ? 'x' : '-' );
           /* search addr2dihandle to see if we have an entry
              matching seg->start. */
           n = VG_(sizeXA)( addr2dihandle );
           for (j = 0; j < n; j++) {
              Addr_n_ULong* anl = VG_(indexXA)( addr2dihandle, j );
              if (anl->a == seg->start) {
                  tl_assert(anl->ull > 0); /* check it's a valid handle */
                  break;
              }
           }
           vg_assert(j >= 0 && j <= n);
           VG_TRACK( new_mem_startup, seg->start, seg->end+1-seg->start, 
                     seg->hasR, seg->hasW, seg->hasX,
                     /* and the retrieved debuginfo handle, if any */
                     j < n
                     ? ((Addr_n_ULong*)VG_(indexXA)( addr2dihandle, j ))->ull
                        : 0 );
        }
     }

     VG_(free)( seg_starts );
     VG_(deleteXA)( addr2dihandle );

     /* Also do the initial stack permissions. */
     {
       SSizeT inaccessible_len;
       NSegment const* seg 
          = VG_(am_find_nsegment)( the_iifii.initial_client_SP );
       vg_assert(seg);
       vg_assert(seg->kind == SkAnonC);
       vg_assert(the_iifii.initial_client_SP >= seg->start);
       vg_assert(the_iifii.initial_client_SP <= seg->end);

       /* Stuff below the initial SP is unaddressable.  Take into
	  account any ABI-mandated space below the stack pointer that
	  is required (VG_STACK_REDZONE_SZB).  setup_client_stack()
	  will have allocated an extra page if a red zone is required,
	  to be on the safe side. */
       inaccessible_len = the_iifii.initial_client_SP - VG_STACK_REDZONE_SZB 
                          - seg->start;
       vg_assert(inaccessible_len >= 0);
       if (inaccessible_len > 0)
          VG_TRACK( die_mem_stack, 
                    seg->start, 
                    inaccessible_len );
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
               True   /* executable? */,
               0 /* di_handle: no associated debug info */ );

     /* Clear the running thread indicator */
     VG_(running_tid) = VG_INVALID_THREADID;
     tl_assert(VG_(running_tid) == VG_INVALID_THREADID);
   }

   //--------------------------------------------------------------
   // Initialise the scheduler (phase 2)
   //   p: Initialise the scheduler (phase 1) [for tid_main]
   //   p: setup_file_descriptors() [else VG_(safe_fd)() breaks]
   //   p: setup_client_stack
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise scheduler (phase 2)\n");
   { NSegment const* seg 
        = VG_(am_find_nsegment)( the_iifii.initial_client_SP );
     vg_assert(seg);
     vg_assert(seg->kind == SkAnonC);
     vg_assert(the_iifii.initial_client_SP >= seg->start);
     vg_assert(the_iifii.initial_client_SP <= seg->end);
     VG_(scheduler_init_phase2)( tid_main, 
                                 seg->end, the_iifii.clstack_max_size );
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
   /* Check that the kernel-interface signal definitions look sane */
   VG_(vki_do_initial_consistency_checks)();
   /* .. and go on to use them. */
   VG_(sigstartup_actions)();

   //--------------------------------------------------------------
   // Read suppression file
   //   p: main_process_cmd_line_options()  [for VG_(clo_suppressions)]
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
   a thread exits requesting a complete process exit.

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
   // Finalisation: cleanup, messages, etc.  Order not so important, only
   // affects what order the messages come.
   //--------------------------------------------------------------
   // First thing in the post-amble is a blank line.
   if (VG_(clo_xml))
      VG_(printf_xml)("\n");
   else if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "\n");

   if (VG_(clo_xml)) {
      HChar buf[50];
      VG_(elapsed_wallclock_time)(buf);
      VG_(printf_xml)( "<status>\n"
                              "  <state>FINISHED</state>\n"
                              "  <time>%pS</time>\n"
                              "</status>\n"
                              "\n",
                              buf);
   }

   /* Print out file descriptor summary and stats. */
   if (VG_(clo_track_fds))
      VG_(show_open_fds)();

   /* Call the tool's finalisation function.  This makes Memcheck's
      leak checker run, and possibly chuck a bunch of leak errors into
      the error management machinery. */
   VG_TDICT_CALL(tool_fini, 0/*exitcode*/);

   /* Show the error counts. */
   if (VG_(clo_xml)
       && (VG_(needs).core_errors || VG_(needs).tool_errors)) {
      VG_(show_error_counts_as_XML)();
   }

   /* In XML mode, this merely prints the used suppressions. */
   if (VG_(needs).core_errors || VG_(needs).tool_errors)
      VG_(show_all_errors)(VG_(clo_verbosity), VG_(clo_xml));

   if (VG_(clo_xml)) {
      VG_(printf_xml)("\n");
      VG_(printf_xml)("</valgrindoutput>\n");
      VG_(printf_xml)("\n");
   }

   VG_(sanity_check_general)( True /*include expensive checks*/ );

   if (VG_(clo_stats))
      print_all_stats();

   /* Show a profile of the heap(s) at shutdown.  Optionally, first
      throw away all the debug info, as that makes it easy to spot
      leaks in the debuginfo reader. */
   if (VG_(clo_profile_heap)) {
      if (0) VG_(di_discard_ALL_debuginfo)();
      VG_(print_arena_cc_analysis)();
   }

   if (VG_(clo_profile_flags) > 0) {
      #define N_MAX 200
      BBProfEntry tops[N_MAX];
      ULong score_total = VG_(get_BB_profile) (tops, N_MAX);
      show_BB_profile(tops, N_MAX, score_total);
   }

   /* Print Vex storage stats */
   if (0)
       LibVEX_ShowAllocStats();

   /* Flush any output cached by previous calls to VG_(message). */
   VG_(message_flush)();

   /* terminate gdbserver if ever it was started. We terminate it here so that it get
      the output above if output was redirected to gdb */
   VG_(gdbserver) (0);

   /* Ok, finally exit in the os-specific way, according to the scheduler's
      return code.  In short, if the (last) thread exited by calling
      sys_exit, do likewise; if the (last) thread stopped due to a fatal
      signal, terminate the entire system with that same fatal signal. */
   VG_(debugLog)(1, "core_os", 
                    "VG_(terminate_NORETURN)(tid=%lld)\n", (ULong)tid);

   switch (tids_schedretcode) {
   case VgSrc_ExitThread:  /* the normal way out (Linux) */
   case VgSrc_ExitProcess: /* the normal way out (AIX) -- still needed? */
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
      /* we shouldn't be alive at this point.  But VG_(kill_self)
         sometimes fails with EPERM on Darwin, for unclear reasons. */
#     if defined(VGO_darwin)
      VG_(debugLog)(0, "main", "VG_(kill_self) failed.  Exiting normally.\n");
      VG_(exit)(0); /* bogus, but we really need to exit now */
      /* fall through .. */
#     endif
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
   GrP fixme glibc-specific, anyway
*/
static void final_tidyup(ThreadId tid)
{
#if !defined(VGO_darwin)
#  if defined(VGP_ppc64_linux)
   Addr r2;
#  endif
   Addr __libc_freeres_wrapper = VG_(client___libc_freeres_wrapper);

   vg_assert(VG_(is_running_thread)(tid));
   
   if ( !VG_(needs).libc_freeres ||
        !VG_(clo_run_libc_freeres) ||
        0 == __libc_freeres_wrapper )
      return;			/* can't/won't do it */

#  if defined(VGP_ppc64_linux)
   r2 = VG_(get_tocptr)( __libc_freeres_wrapper );
   if (r2 == 0) {
      VG_(message)(Vg_UserMsg, 
                   "Caught __NR_exit, but can't run __libc_freeres()\n");
      VG_(message)(Vg_UserMsg, 
                   "   since cannot establish TOC pointer for it.\n");
      return;
   }
#  endif

   if (VG_(clo_verbosity) > 2  ||
       VG_(clo_trace_syscalls) ||
       VG_(clo_trace_sched))
      VG_(message)(Vg_DebugMsg, 
		   "Caught __NR_exit; running __libc_freeres()\n");
      
   /* set thread context to point to libc_freeres_wrapper */
   /* ppc64-linux note: __libc_freeres_wrapper gives us the real
      function entry point, not a fn descriptor, so can use it
      directly.  However, we need to set R2 (the toc pointer)
      appropriately. */
   VG_(set_IP)(tid, __libc_freeres_wrapper);
#  if defined(VGP_ppc64_linux)
   VG_(threads)[tid].arch.vex.guest_GPR2 = r2;
#  endif
   /* mips-linux note: we need to set t9 */
#  if defined(VGP_mips32_linux)
   VG_(threads)[tid].arch.vex.guest_r25 = __libc_freeres_wrapper;
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
#endif
}


/*====================================================================*/
/*=== Getting to main() alive: LINUX                               ===*/
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

/* BVA: abort() for those platforms that need it (PPC and ARM). */
void abort(void);
void abort(void){
   VG_(printf)("Something called raise().\n");
   vg_assert(0);
}

/* EAZG: ARM's EABI will call floating point exception handlers in
   libgcc which boil down to an abort or raise, that's usually defined
   in libc. Instead, define them here. */
#if defined(VGP_arm_linux)
void raise(void);
void raise(void){
   VG_(printf)("Something called raise().\n");
   vg_assert(0);
}

void __aeabi_unwind_cpp_pr0(void);
void __aeabi_unwind_cpp_pr0(void){
   VG_(printf)("Something called __aeabi_unwind_cpp_pr0()\n");
   vg_assert(0);
}

void __aeabi_unwind_cpp_pr1(void);
void __aeabi_unwind_cpp_pr1(void){
   VG_(printf)("Something called __aeabi_unwind_cpp_pr1()\n");
   vg_assert(0);
}
#endif

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
    "\tlis  14,   _start_in_C_linux@highest\n"
    "\tori  14,14,_start_in_C_linux@higher\n"
    "\tsldi 14,14,32\n"
    "\toris 14,14,_start_in_C_linux@h\n"
    "\tori  14,14,_start_in_C_linux@l\n"
    "\tld 14,0(14)\n"
    "\tmtctr 14\n"
    "\tbctrl\n"
    "\tnop\n"
    "\ttrap\n"
);
#elif defined(VGP_s390x_linux)
/*
    This is the canonical entry point, usually the first thing in the text
    segment. Most registers' values are unspecified, except for:

    %r14         Contains a function pointer to be registered with `atexit'.
                 This is how the dynamic linker arranges to have DT_FINI
                 functions called for shared libraries that have been loaded
                 before this code runs.

    %r15         The stack contains the arguments and environment:
                 0(%r15)              argc
                 8(%r15)              argv[0]
                 ...
                 (8*argc)(%r15)       NULL
                 (8*(argc+1))(%r15)   envp[0]
                 ...
                                      NULL
*/
asm("\n\t"
    ".text\n\t"
    ".globl _start\n\t"
    ".type  _start,@function\n\t"
    "_start:\n\t"
    /* set up the new stack in %r1 */
    "larl   %r1,  vgPlain_interim_stack\n\t"
    "larl   %r5,  1f\n\t"
    "ag     %r1,  0(%r5)\n\t"
    "ag     %r1,  2f-1f(%r5)\n\t"
    "nill   %r1,  0xFFF0\n\t"
    /* install it, and collect the original one */
    "lgr    %r2,  %r15\n\t"
    "lgr    %r15, %r1\n\t"
    /* call _start_in_C_linux, passing it the startup %r15 */
    "brasl  %r14, _start_in_C_linux\n\t"
    /* trigger execution of an invalid opcode -> halt machine */
    "j      .+2\n\t"
    "1:   .quad "VG_STRINGIFY(VG_STACK_GUARD_SZB)"\n\t"
    "2:   .quad "VG_STRINGIFY(VG_STACK_ACTIVE_SZB)"\n\t"
    ".previous\n"
);
#elif defined(VGP_arm_linux)
asm("\n"
    "\t.text\n"
    "\t.align 4\n"
    "\t.type _start,#function\n"
    "\t.global _start\n"
    "_start:\n"
    "\tldr  r0, [pc, #36]\n"
    "\tldr  r1, [pc, #36]\n"
    "\tadd  r0, r1, r0\n"
    "\tldr  r1, [pc, #32]\n"
    "\tadd  r0, r1, r0\n"
    "\tmvn  r1, #15\n"
    "\tand  r0, r0, r1\n"
    "\tmov  r1, sp\n"
    "\tmov  sp, r0\n"
    "\tmov  r0, r1\n"
    "\tb _start_in_C_linux\n"
    "\t.word vgPlain_interim_stack\n"
    "\t.word "VG_STRINGIFY(VG_STACK_GUARD_SZB)"\n"
    "\t.word "VG_STRINGIFY(VG_STACK_ACTIVE_SZB)"\n"
);
#elif defined(VGP_mips32_linux)
asm("\n"
    "\t.type _gp_disp,@object\n"
    ".text\n"
    "\t.globl __start\n"
    "\t.type __start,@function\n"
    "__start:\n"

    "\tbal 1f\n"
    "\tnop\n"
    
    "1:\n"    

    "\tlui      $28, %hi(_gp_disp)\n"
    "\taddiu    $28, $28, %lo(_gp_disp)\n"
    "\taddu     $28, $28, $31\n"
    /* t1/$9 <- Addr(interim_stack) */
    "\tlui      $9, %hi(vgPlain_interim_stack)\n"
    /* t1/$9 <- Addr(interim_stack) */
    "\taddiu    $9, %lo(vgPlain_interim_stack)\n"


    "\tli    $10, "VG_STRINGIFY(VG_STACK_GUARD_SZB)"\n"
    "\tli    $11, "VG_STRINGIFY(VG_STACK_ACTIVE_SZB)"\n"
    
    "\taddu     $9, $9, $10\n"
    "\taddu     $9, $9, $11\n"
    "\tli       $12, 0xFFFFFFF0\n"
    "\tand      $9, $9, $12\n"
    /* now t1/$9 = &vgPlain_interim_stack + VG_STACK_GUARD_SZB +
       VG_STACK_ACTIVE_SZB rounded down to the nearest 16-byte
       boundary.  And $29 is the original SP.  Set the SP to t1 and
       call _start_in_C, passing it the initial SP. */
       
    "\tmove    $4, $29\n"     // a0 <- $sp (_start_in_C first arg)
    "\tmove    $29, $9\n"     // $sp <- t1 (new sp)
    
    "\tlui     $25, %hi(_start_in_C_linux)\n"
    "\taddiu   $25, %lo(_start_in_C_linux)\n"
    
    "\tbal  _start_in_C_linux\n"
    "\tbreak  0x7\n"
    ".previous\n"
);
#else
#  error "Unknown linux platform"
#endif

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#define _GNU_SOURCE
#define _FILE_OFFSET_BITS 64
/* This is in order to get AT_NULL and AT_PAGESIZE. */
#include <elf.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

/* Avoid compiler warnings: this fn _is_ used, but labelling it
   'static' causes gcc to complain it isn't.
   attribute 'used' also ensures the code is not eliminated at link
   time */
__attribute__ ((used))
void _start_in_C_linux ( UWord* pArgc );
__attribute__ ((used))
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


/*====================================================================*/
/*=== Getting to main() alive: darwin                              ===*/
/*====================================================================*/

#elif defined(VGO_darwin)

/*
   Memory layout established by kernel:

   0(%esp)   argc
   4(%esp)   argv[0]
             ...
             argv[argc-1]
             NULL
             envp[0]
             ...
             envp[n]
             NULL
             executable name (presumably, a pointer to it)
             NULL

   Ditto in the 64-bit case, except all offsets from SP are obviously
   twice as large.
*/

/* The kernel hands control to _start, which extracts the initial
   stack pointer and calls onwards to _start_in_C_darwin.  This also
   switches to the new stack.  */
#if defined(VGP_x86_darwin)
asm("\n"
    ".text\n"
    ".align 2,0x90\n"
    "\t.globl __start\n"
    "__start:\n"
    /* set up the new stack in %eax */
    "\tmovl  $_vgPlain_interim_stack, %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_STACK_ACTIVE_SZB)", %eax\n"
    "\tsubl  $16, %eax\n"
    "\tandl  $~15, %eax\n"
    /* install it, and collect the original one */
    "\txchgl %eax, %esp\n"
    "\tsubl  $12, %esp\n"  // keep stack 16 aligned; see #295428
    /* call _start_in_C_darwin, passing it the startup %esp */
    "\tpushl %eax\n"
    "\tcall  __start_in_C_darwin\n"
    "\tint $3\n"
    "\tint $3\n"
);
#elif defined(VGP_amd64_darwin)
asm("\n"
    ".text\n"
    "\t.globl __start\n"
    ".align 3,0x90\n"
    "__start:\n"
    /* set up the new stack in %rdi */
    "\tmovabsq $_vgPlain_interim_stack, %rdi\n"
    "\taddq    $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %rdi\n"
    "\taddq    $"VG_STRINGIFY(VG_STACK_ACTIVE_SZB)", %rdi\n"
    "\tandq    $~15, %rdi\n"
    /* install it, and collect the original one */
    "\txchgq %rdi, %rsp\n"
    /* call _start_in_C_darwin, passing it the startup %rsp */
    "\tcall  __start_in_C_darwin\n"
    "\tint $3\n"
    "\tint $3\n"
);
#endif

void* __memcpy_chk(void *dest, const void *src, SizeT n, SizeT n2);
void* __memcpy_chk(void *dest, const void *src, SizeT n, SizeT n2) {
    // skip check
   return VG_(memcpy)(dest,src,n);
}
void* __memset_chk(void *s, int c, SizeT n, SizeT n2);
void* __memset_chk(void *s, int c, SizeT n, SizeT n2) {
    // skip check
  return VG_(memset)(s,c,n);
}
void bzero(void *s, SizeT n);
void bzero(void *s, SizeT n) {
    VG_(memset)(s,0,n);
}

void* memcpy(void *dest, const void *src, SizeT n);
void* memcpy(void *dest, const void *src, SizeT n) {
   return VG_(memcpy)(dest,src,n);
}
void* memset(void *s, int c, SizeT n);
void* memset(void *s, int c, SizeT n) {
  return VG_(memset)(s,c,n);
}

/* Avoid compiler warnings: this fn _is_ used, but labelling it
   'static' causes gcc to complain it isn't. */
void _start_in_C_darwin ( UWord* pArgc );
void _start_in_C_darwin ( UWord* pArgc )
{
   Int     r;
   Int     argc = *(Int *)pArgc;  // not pArgc[0] on LP64
   HChar** argv = (HChar**)&pArgc[1];
   HChar** envp = (HChar**)&pArgc[1+argc+1];

   VG_(memset)( &the_iicii, 0, sizeof(the_iicii) );
   VG_(memset)( &the_iifii, 0, sizeof(the_iifii) );

   the_iicii.sp_at_startup = (Addr)pArgc;

   r = valgrind_main( (Int)argc, argv, envp );
   /* NOTREACHED */
   VG_(exit)(r);
}


#else

#  error "Unknown OS"
#endif


/*====================================================================*/
/*=== {u,}{div,mod}di3 replacements                                ===*/
/*====================================================================*/

/* For static linking on x86-darwin, we need to supply our own 64-bit
   integer division code, else the link dies thusly:

   ld_classic: Undefined symbols:
     ___udivdi3
     ___umoddi3
*/
#if defined(VGP_x86_darwin)

/* Routines for doing signed/unsigned 64 x 64 ==> 64 div and mod
   (udivdi3, umoddi3, divdi3, moddi3) using only 32 x 32 ==> 32
   division.  Cobbled together from

   http://www.hackersdelight.org/HDcode/divlu.c
   http://www.hackersdelight.org/HDcode/divls.c
   http://www.hackersdelight.org/HDcode/newCode/divDouble.c

   The code from those three files is covered by the following license,
   as it appears at:

   http://www.hackersdelight.org/permissions.htm

      You are free to use, copy, and distribute any of the code on
      this web site, whether modified by you or not. You need not give
      attribution. This includes the algorithms (some of which appear
      in Hacker's Delight), the Hacker's Assistant, and any code
      submitted by readers. Submitters implicitly agree to this.
*/

/* Long division, unsigned (64/32 ==> 32).
   This procedure performs unsigned "long division" i.e., division of a
64-bit unsigned dividend by a 32-bit unsigned divisor, producing a
32-bit quotient.  In the overflow cases (divide by 0, or quotient
exceeds 32 bits), it returns a remainder of 0xFFFFFFFF (an impossible
value).
   The dividend is u1 and u0, with u1 being the most significant word.
The divisor is parameter v. The value returned is the quotient.
   Max line length is 57, to fit in hacker.book. */

static Int nlz32(UInt x) 
{
   Int n;
   if (x == 0) return(32);
   n = 0;
   if (x <= 0x0000FFFF) {n = n +16; x = x <<16;}
   if (x <= 0x00FFFFFF) {n = n + 8; x = x << 8;}
   if (x <= 0x0FFFFFFF) {n = n + 4; x = x << 4;}
   if (x <= 0x3FFFFFFF) {n = n + 2; x = x << 2;}
   if (x <= 0x7FFFFFFF) {n = n + 1;}
   return n;
}

/* 64 x 32 ==> 32 unsigned division, using only 32 x 32 ==> 32
   division as a primitive. */
static UInt divlu2(UInt u1, UInt u0, UInt v, UInt *r)
{
   const UInt b = 65536;     // Number base (16 bits).
   UInt un1, un0,            // Norm. dividend LSD's.
        vn1, vn0,            // Norm. divisor digits.
        q1, q0,              // Quotient digits.
        un32, un21, un10,    // Dividend digit pairs.
        rhat;                // A remainder.
   Int s;                    // Shift amount for norm.

   if (u1 >= v) {            // If overflow, set rem.
      if (r != NULL)         // to an impossible value,
         *r = 0xFFFFFFFF;    // and return the largest
      return 0xFFFFFFFF;}    // possible quotient.

   s = nlz32(v);             // 0 <= s <= 31.
   v = v << s;               // Normalize divisor.
   vn1 = v >> 16;            // Break divisor up into
   vn0 = v & 0xFFFF;         // two 16-bit digits.

   un32 = (u1 << s) | ((u0 >> (32 - s)) & (-s >> 31));
   un10 = u0 << s;           // Shift dividend left.

   un1 = un10 >> 16;         // Break right half of
   un0 = un10 & 0xFFFF;      // dividend into two digits.

   q1 = un32/vn1;            // Compute the first
   rhat = un32 - q1*vn1;     // quotient digit, q1.
 again1:
   if (q1 >= b || q1*vn0 > b*rhat + un1) {
     q1 = q1 - 1;
     rhat = rhat + vn1;
     if (rhat < b) goto again1;}

   un21 = un32*b + un1 - q1*v;  // Multiply and subtract.

   q0 = un21/vn1;            // Compute the second
   rhat = un21 - q0*vn1;     // quotient digit, q0.
 again2:
   if (q0 >= b || q0*vn0 > b*rhat + un0) {
     q0 = q0 - 1;
     rhat = rhat + vn1;
     if (rhat < b) goto again2;}

   if (r != NULL)            // If remainder is wanted,
      *r = (un21*b + un0 - q0*v) >> s;     // return it.
   return q1*b + q0;
}


/* 64 x 32 ==> 32 signed division, using only 32 x 32 ==> 32 division
   as a primitive. */
static Int divls(Int u1, UInt u0, Int v, Int *r)
{
   Int q, uneg, vneg, diff, borrow;

   uneg = u1 >> 31;          // -1 if u < 0.
   if (uneg) {               // Compute the absolute
      u0 = -u0;              // value of the dividend u.
      borrow = (u0 != 0);
      u1 = -u1 - borrow;}

   vneg = v >> 31;           // -1 if v < 0.
   v = (v ^ vneg) - vneg;    // Absolute value of v.

   if ((UInt)u1 >= (UInt)v) goto overflow;

   q = divlu2(u1, u0, v, (UInt *)r);

   diff = uneg ^ vneg;       // Negate q if signs of
   q = (q ^ diff) - diff;    // u and v differed.
   if (uneg && r != NULL)
      *r = -*r;

   if ((diff ^ q) < 0 && q != 0) {  // If overflow,
 overflow:                    // set remainder
      if (r != NULL)         // to an impossible value,
         *r = 0x80000000;    // and return the largest
      q = 0x80000000;}       // possible neg. quotient.
   return q;
}



/* This file contains a program for doing 64/64 ==> 64 division, on a
machine that does not have that instruction but that does have
instructions for "long division" (64/32 ==> 32). Code for unsigned
division is given first, followed by a simple program for doing the
signed version by using the unsigned version.
   These programs are useful in implementing "long long" (64-bit)
arithmetic on a machine that has the long division instruction. It will
work on 64- and 32-bit machines, provided the compiler implements long
long's (64-bit integers). It is desirable that the machine have the
Count Leading Zeros instruction.
   In the GNU world, these programs are known as __divdi3 and __udivdi3,
and similar names are used here.
   This material is not in HD, but may be in a future edition.
Max line length is 57, to fit in hacker.book. */


static Int nlz64(ULong x) 
{
   Int n;
   if (x == 0) return(64);
   n = 0;
   if (x <= 0x00000000FFFFFFFFULL) {n = n + 32; x = x << 32;}
   if (x <= 0x0000FFFFFFFFFFFFULL) {n = n + 16; x = x << 16;}
   if (x <= 0x00FFFFFFFFFFFFFFULL) {n = n +  8; x = x <<  8;}
   if (x <= 0x0FFFFFFFFFFFFFFFULL) {n = n +  4; x = x <<  4;}
   if (x <= 0x3FFFFFFFFFFFFFFFULL) {n = n +  2; x = x <<  2;}
   if (x <= 0x7FFFFFFFFFFFFFFFULL) {n = n +  1;}
   return n;
}

// ---------------------------- udivdi3 --------------------------------

   /* The variables u0, u1, etc. take on only 32-bit values, but they
   are declared long long to avoid some compiler warning messages and to
   avoid some unnecessary EXTRs that the compiler would put in, to
   convert long longs to ints.

   First the procedure takes care of the case in which the divisor is a
   32-bit quantity. There are two subcases: (1) If the left half of the
   dividend is less than the divisor, one execution of DIVU is all that
   is required (overflow is not possible). (2) Otherwise it does two
   divisions, using the grade school method, with variables used as
   suggested below.

       q1 q0
    ________
   v)  u1 u0
     q1*v
     ____
        k u0   */

/* These macros must be used with arguments of the appropriate type
(unsigned long long for DIVU and long long for DIVS. They are
simulations of the presumed machines ops. I.e., they look at only the
low-order 32 bits of the divisor, they return garbage if the division
overflows, and they return garbage in the high-order half of the
quotient doubleword.
   In practice, these would be replaced with uses of the machine's DIVU
and DIVS instructions (e.g., by using the GNU "asm" facility). */

static UInt DIVU ( ULong u, UInt v )
{
  UInt uHi = (UInt)(u >> 32);
  UInt uLo = (UInt)u;
  return divlu2(uHi, uLo, v, NULL);
}

static Int DIVS ( Long u, Int v )
{
  Int  uHi = (Int)(u >> 32);
  UInt uLo = (UInt)u;
  return divls(uHi, uLo, v, NULL);
}

/* 64 x 64 ==> 64 unsigned division, using only 32 x 32 ==> 32
   division as a primitive. */
static ULong udivdi3(ULong u, ULong v)
{
   ULong u0, u1, v1, q0, q1, k, n;

   if (v >> 32 == 0) {          // If v < 2**32:
      if (u >> 32 < v)          // If u/v cannot overflow,
         return DIVU(u, v)      // just do one division.
            & 0xFFFFFFFF;
      else {                    // If u/v would overflow:
         u1 = u >> 32;          // Break u up into two
         u0 = u & 0xFFFFFFFF;   // halves.
         q1 = DIVU(u1, v)       // First quotient digit.
            & 0xFFFFFFFF;
         k = u1 - q1*v;         // First remainder, < v.
         q0 = DIVU((k << 32) + u0, v) // 2nd quot. digit.
            & 0xFFFFFFFF;
         return (q1 << 32) + q0;
      }
   }
                                // Here v >= 2**32.
   n = nlz64(v);                // 0 <= n <= 31.
   v1 = (v << n) >> 32;         // Normalize the divisor
                                // so its MSB is 1.
   u1 = u >> 1;                 // To ensure no overflow.
   q1 = DIVU(u1, v1)            // Get quotient from
       & 0xFFFFFFFF;            // divide unsigned insn.
   q0 = (q1 << n) >> 31;        // Undo normalization and
                                // division of u by 2.
   if (q0 != 0)                 // Make q0 correct or
      q0 = q0 - 1;              // too small by 1.
   if ((u - q0*v) >= v)
      q0 = q0 + 1;              // Now q0 is correct.
   return q0;
}


// ----------------------------- divdi3 --------------------------------

/* This routine presumes that smallish cases (those which can be done in
one execution of DIVS) are common. If this is not the case, the test for
this case should be deleted.
   Note that the test for when DIVS can be used is not entirely
accurate. For example, DIVS is not used if v = 0xFFFFFFFF8000000,
whereas if could be (if u is sufficiently small in magnitude). */

// ------------------------------ cut ----------------------------------

static ULong my_llabs ( Long x )
{
   ULong t = x >> 63;
   return (x ^ t) - t;
}

/* 64 x 64 ==> 64 signed division, using only 32 x 32 ==> 32 division
   as a primitive. */
static Long divdi3(Long u, Long v)
{
   ULong au, av;
   Long q, t;
   au = my_llabs(u);
   av = my_llabs(v);
   if (av >> 31 == 0) {         // If |v| < 2**31 and
   // if (v << 32 >> 32 == v) { // If v is in range and
      if (au < av << 31) {      // |u|/|v| cannot
         q = DIVS(u, v);        // overflow, use DIVS.
         return (q << 32) >> 32;
      }
   }
   q = udivdi3(au,av);          // Invoke udivdi3.
   t = (u ^ v) >> 63;           // If u, v have different
   return (q ^ t) - t;          // signs, negate q.
}

// ---------------------------- end cut --------------------------------

ULong __udivdi3 (ULong u, ULong v);
ULong __udivdi3 (ULong u, ULong v)
{
  return udivdi3(u,v);
}

Long __divdi3 (Long u, Long v);
Long __divdi3 (Long u, Long v)
{
  return divdi3(u,v);
}

ULong __umoddi3 (ULong u, ULong v);
ULong __umoddi3 (ULong u, ULong v)
{
  ULong q = __udivdi3(u, v);
  ULong r = u - q * v;
  return r;
}

Long __moddi3 (Long u, Long v);
Long __moddi3 (Long u, Long v)
{
  Long q = __divdi3(u, v);
  Long r = u - q * v;
  return r;
}

/* ------------------------------------------------
   ld_classic: Undefined symbols:
      ___fixunsdfdi
   ------------------------------------------------
*/

/* ===-- fixunsdfdi.c - Implement __fixunsdfdi -----------------------------===
 *
 *                     The LLVM Compiler Infrastructure
 *
 * This file is dual licensed under the MIT and the University of Illinois Open
 * Source Licenses. See LICENSE.TXT for details.
 *
 * ===----------------------------------------------------------------------===
 *
 * This file implements __fixunsdfdi for the compiler_rt library.
 *
 * ===----------------------------------------------------------------------===
 */

/* As per http://www.gnu.org/licenses/license-list.html#GPLCompatibleLicenses,

   the "NCSA/University of Illinois Open Source License" is compatible
   with the GPL (both version 2 and 3).  What is claimed to be
   compatible is this

   http://www.opensource.org/licenses/UoI-NCSA.php

   and the LLVM documentation at

   http://www.llvm.org/docs/DeveloperPolicy.html#license

   says all the code in LLVM is available under the University of
   Illinois/NCSA Open Source License, at this URL

   http://www.opensource.org/licenses/UoI-NCSA.php

   viz, the same one that the FSF pages claim is compatible.  So I
   think it's OK to include it.
*/

/* Returns: convert a to a unsigned long long, rounding toward zero.
 *          Negative values all become zero.
 */

/* Assumption: double is a IEEE 64 bit floating point type 
 *             du_int is a 64 bit integral type
 *             value in double is representable in du_int or is negative 
 *                 (no range checking performed)
 */

/* seee eeee eeee mmmm mmmm mmmm mmmm mmmm | mmmm mmmm mmmm mmmm mmmm mmmm mmmm mmmm */

typedef unsigned long long du_int;
typedef unsigned su_int;

typedef union
{
    du_int all;
    struct
    {
#if VG_LITTLEENDIAN
        su_int low;
        su_int high;
#else
        su_int high;
        su_int low;
#endif /* VG_LITTLEENDIAN */
    }s;
} udwords;

typedef union
{
    udwords u;
    double  f;
} double_bits;

du_int __fixunsdfdi(double a);

du_int
__fixunsdfdi(double a)
{
    double_bits fb;
    fb.f = a;
    int e = ((fb.u.s.high & 0x7FF00000) >> 20) - 1023;
    if (e < 0 || (fb.u.s.high & 0x80000000))
        return 0;
    udwords r;
    r.s.high = (fb.u.s.high & 0x000FFFFF) | 0x00100000;
    r.s.low = fb.u.s.low;
    if (e > 52)
        r.all <<= (e - 52);
    else
        r.all >>= (52 - e);
    return r.all;
}


#endif


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
