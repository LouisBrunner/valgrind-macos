
/*--------------------------------------------------------------------*/
/*--- Startup: the real stuff                             m_main.c ---*/
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

#include "vgversion.h"
#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_threadstate.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_aspacehl.h"
#include "pub_core_clreq.h"
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
#include "pub_core_sbprofile.h"
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
#include "pub_core_inner.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
#include "pub_core_clreq.h"
#endif


/*====================================================================*/
/*=== Command-line: variables, processing, etc                     ===*/
/*====================================================================*/

// See pub_{core,tool}_options.h for explanations of all these.

// need_help: 0 = no, 1 = --help-dyn-options, 2 = --help 3 = --help-debug
static void usage_NORETURN ( int need_help )
{
   /* 'usage1' contains a %s
      - for the name of the GDB executable
      - for the name of vgdb's path prefix
      which must be supplied when they are VG_(printf)'d. */
   const HChar usage1[] =
"usage: valgrind [options] prog-and-args\n"
"\n"
"  tool-selection option, with default in [ ]:\n"
"    --tool=<name>             use the Valgrind tool named <name> [memcheck]\n"
"                              available tools are:\n"
"                              memcheck cachegrind callgrind helgrind drd\n"
"                              massif dhat lackey none exp-bbv\n"
"\n"
"  basic user options for all Valgrind tools, with defaults in [ ]:\n"
"    -h --help                 show this message\n"
"    --help-debug              show this message, plus debugging options\n"
"    --help-dyn-options        show the dynamically changeable options\n"
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
"    --vgdb-stop-at=event1,event2,... invoke gdbserver for given events [none]\n"
"         where event is one of:\n"
"           startup exit abexit valgrindabexit all none\n"
"    --track-fds=no|yes|all    track open file descriptors? [no]\n"
"                              all includes reporting inherited file descriptors\n"
"    --modify-fds=no|high      modify newly open file descriptors? [no]\n"
"    --time-stamp=no|yes       add timestamps to log messages? [no]\n"
"    --log-fd=<number>         log messages to file descriptor [2=stderr]\n"
"    --log-file=<file>         log messages to <file>\n"
"    --log-socket=ipaddr:port  log messages to socket ipaddr:port\n"
#if defined(VGO_linux)
"    --enable-debuginfod=no|yes query debuginfod servers for missing\n"
"                              debuginfo [yes]\n"
#endif
"\n"
"  user options for Valgrind tools that report errors:\n"
"    --xml=yes                 emit error output in XML (some tools only)\n"
"    --xml-fd=<number>         XML output to file descriptor\n"
"    --xml-file=<file>         XML output to <file>\n"
"    --xml-socket=ipaddr:port  XML output to socket ipaddr:port\n"
"    --xml-user-comment=STR    copy STR verbatim into XML output\n"
"    --demangle=no|yes         automatically demangle decorated names? [yes]\n"
"                              supported languages: C++, D, Rust, Java, Ada\n"
"    --num-callers=<number>    show <number> callers in stack traces [12]\n"
"    --error-limit=no|yes      stop showing new errors if too many? [yes]\n"
"    --exit-on-first-error=no|yes exit code on the first error found? [no]\n"
"    --error-exitcode=<number> exit code to return if errors found [0=disable]\n"
"    --error-markers=<begin>,<end> add lines with begin/end markers before/after\n"
"                              each error output in plain text mode [none]\n"
"    --show-error-list=no|yes|all  show detected errors list and\n"
"                              suppression counts at exit [no].\n"
"                              all means to also print suppressed errors.\n"
"    -s                        same as --show-error-list=yes\n"
"    --keep-debuginfo=no|yes   Keep symbols etc for unloaded code [no]\n"
"                              This allows saved stack traces (e.g. memory leaks)\n"
"                              to include file/line info for code that has been\n"
"                              dlclose'd (or similar)\n"
"    --show-below-main=no|yes  continue stack traces below main() [no]\n"
"    --default-suppressions=yes|no\n"
"                              load default suppressions [yes]\n"
"    --suppressions=<filename> suppress errors described in <filename>\n"
"    --gen-suppressions=no|yes|all    print suppressions for errors? [no]\n"
"    --input-fd=<number>       file descriptor for input [0=stdin]\n"
"    --dsymutil=no|yes         run dsymutil on Mac OS X when helpful? [yes]\n"
"    --max-stackframe=<number> assume stack switch for SP changes larger\n"
"                              than <number> bytes [2000000]\n"
"    --main-stacksize=<number> set size of main thread's stack (in bytes)\n"
"                              [min(max(current 'ulimit' value,1MB),16MB)]\n"
"\n"
"  user options for Valgrind tools that replace malloc:\n"
"    --alignment=<number>      set minimum alignment of heap allocations [%s]\n"
"    --redzone-size=<number>   set minimum size of redzones added before/after\n"
"                              heap blocks (in bytes). [%s]\n"
"    --xtree-memory=none|allocs|full   profile heap memory in an xtree [none]\n"
"                              and produces a report at the end of the execution\n"
"                              none: no profiling, allocs: current allocated\n"
"                              size/blocks, full: profile current and cumulative\n"
"                              allocated size/blocks and freed size/blocks.\n"
"    --xtree-memory-file=<file>   xtree memory report file [xtmemory.kcg.%%p]\n"
"    --realloc-zero-bytes-frees=yes|no [yes on Linux glibc, no otherwise]\n"
"                              should calls to realloc with a size of 0\n"
"                              free memory and return NULL or\n"
"                              allocate/resize and return non-NULL\n"
"\n"
"  uncommon user options for all Valgrind tools:\n"
"    --fullpath-after=         (with nothing after the '=')\n"
"                              show full source paths in call stacks\n"
"    --fullpath-after=string   like --fullpath-after=, but only show the\n"
"                              part of the path after 'string'.  Allows removal\n"
"                              of path prefixes.  Use this flag multiple times\n"
"                              to specify a set of prefixes to remove.\n"
"    --extra-debuginfo-path=path    absolute path to search for additional\n"
"                              debug symbols, in addition to existing default\n"
"                              well known search paths.\n"
"    --debuginfo-server=ipaddr:port    also query this server\n"
"                              (valgrind-di-server) for debug symbols\n"
"    --allow-mismatched-debuginfo=no|yes  [no]\n"
"                              for the above two flags only, accept debuginfo\n"
"                              objects that don't \"match\" the main object\n"
"    --smc-check=none|stack|all|all-non-file [all-non-file]\n"
"                              checks for self-modifying code: none, only for\n"
"                              code found in stacks, for all code, or for all\n"
"                              code except that from file-backed mappings\n"
"    --read-inline-info=yes|no read debug info about inlined function calls\n"
"                              and use it to do better stack traces.\n"
"                              [yes] on Linux/Android/Solaris for the tools\n"
"                              Memcheck/Massif/Helgrind/DRD only.\n"
"                              [no] for all other tools and platforms.\n"
"    --read-var-info=yes|no    read debug info on stack and global variables\n"
"                              and use it to print better error messages in\n"
"                              tools that make use of it (Memcheck, Helgrind,\n"
"                              DRD) [no]\n"
"    --vgdb-poll=<number>      gdbserver poll max every <number> basic blocks [%d] \n"
"    --vgdb-shadow-registers=no|yes   let gdb see the shadow registers [no]\n"
"    --vgdb-prefix=<prefix>    prefix for vgdb FIFOs [%s]\n"
"    --run-libc-freeres=no|yes free up glibc memory at exit on Linux? [yes]\n"
"    --run-cxx-freeres=no|yes  free up libstdc++ memory at exit on Linux\n"
"                              and Solaris? [yes]\n"
"    --sim-hints=hint1,hint2,...  activate unusual sim behaviours [none] \n"
"         where hint is one of:\n"
"           lax-ioctls lax-doors fuse-compatible enable-outer\n"
"           no-inner-prefix no-nptl-pthread-stackcache fallback-llsc none\n"
"    --scheduling-quantum=<number>  thread-scheduling timeslice in number of\n"
"           basic blocks [100000]\n"
"    --fair-sched=no|yes|try   schedule threads fairly on multicore systems [no]\n"
"    --kernel-variant=variant1,variant2,...\n"
"         handle non-standard kernel variants [none]\n"
"         where variant is one of:\n"
"           bproc android-no-hw-tls\n"
"           android-gpu-sgx5xx android-gpu-adreno3xx none\n"
"    --merge-recursive-frames=<number>  merge frames between identical\n"
"           program counters in max <number> frames) [0]\n"
"    --num-transtab-sectors=<number> size of translated code cache [%d]\n"
"           more sectors may increase performance, but use more memory.\n"
"    --avg-transtab-entry-size=<number> avg size in bytes of a translated\n"
"           basic block [0, meaning use tool provided default]\n"
"    --aspace-minaddr=0xPP     avoid mapping memory below 0xPP [guessed]\n"
"    --valgrind-stacksize=<number> size of valgrind (host) thread's stack\n"
"                               (in bytes) ["
                                VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)
                                                "]\n"
"    --show-emwarns=no|yes     show warnings about emulation limits? [no]\n"
"    --require-text-symbol=:sonamepattern:symbolpattern    abort run if the\n"
"                              stated shared object doesn't have the stated\n"
"                              text symbol.  Patterns can contain ? and *.\n"
"    --soname-synonyms=syn1=pattern1,syn2=pattern2,... synonym soname\n"
"              specify patterns for function wrapping or replacement.\n"
"              To use a non-libc malloc library that is\n"
"                  in the main exe:  --soname-synonyms=somalloc=NONE\n"
"                  in libxyzzy.so:   --soname-synonyms=somalloc=libxyzzy.so\n"
"    --sigill-diagnostics=yes|no  warn about illegal instructions? [yes]\n"
"    --unw-stack-scan-thresh=<number>   Enable stack-scan unwind if fewer\n"
"                  than <number> good frames found  [0, meaning \"disabled\"]\n"
"                  NOTE: stack scanning is only available on arm-linux.\n"
"    --unw-stack-scan-frames=<number>   Max number of frames that can be\n"
"                  recovered by stack scanning [5]\n"
"    --resync-filter=no|yes|verbose [yes on MacOS, no on other OSes]\n"
"              attempt to avoid expensive address-space-resync operations\n"
"    --max-threads=<number>    maximum number of threads that valgrind can\n"
"                              handle [%d]\n"
"\n";

   const HChar usage2[] =
"\n"
"  debugging options for all Valgrind tools:\n"
"    -d                        show verbose debugging output\n"
"    --stats=no|yes            show tool and core statistics [no]\n"
"    --sanity-level=<number>   level of sanity checking to do [1]\n"
"                              1 - does occasional stack checking\n"
"                              2 - more stack checks and malloc checks\n"
"                              3 - as 2 and mmap checks\n"
"                              4 - as 3 and translation sector checks\n"
"    --trace-flags=<XXXXXXXX>   show generated code? (X = 0|1) [00000000]\n"
"    --profile-flags=<XXXXXXXX> ditto, but for profiling (X = 0|1) [00000000]\n"
"    --profile-interval=<number> show profile every <number> event checks\n"
"                                [0, meaning only at the end of the run]\n"
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
"    --core-redzone-size=<number>  set minimum size of redzones added before/after\n"
"                              heap blocks allocated for Valgrind internal use (in bytes) [4]\n"
"    --wait-for-gdb=yes|no     pause on startup to wait for gdb attach\n"
"    --sym-offsets=yes|no      show syms in form 'name+offset'? [no]\n"
"    --progress-interval=<number>  report progress every <number>\n"
"                                  CPU seconds [0, meaning disabled]\n"
"    --command-line-only=no|yes  only use command line options [no]\n\n"
"  Vex options for all Valgrind tools:\n"
"    --vex-iropt-verbosity=<0..9>           [0]\n"
"    --vex-iropt-level=<0..2>               [2]\n"
"    --vex-iropt-unroll-thresh=<0..400>     [120]\n"
"    --vex-guest-max-insns=<1..100>         [50]\n"
"    --vex-guest-chase=no|yes               [yes]\n"
"    Precise exception control.  Possible values for 'mode' are as follows\n"
"      and specify the minimum set of registers guaranteed to be correct\n"
"      immediately prior to memory access instructions:\n"
"         sp-at-mem-access          stack pointer only\n"
"         unwindregs-at-mem-access  registers needed for stack unwinding\n"
"         allregs-at-mem-access     all registers\n"
"         allregs-at-each-insn      all registers are always correct\n"
"      Default value for all 3 following flags is [unwindregs-at-mem-access].\n"
"      --vex-iropt-register-updates=mode   setting to use by default\n"
"      --px-default=mode      synonym for --vex-iropt-register-updates\n"
"      --px-file-backed=mode  optional setting for file-backed (non-JIT) code\n"
"    Tracing and profile control:\n"
"      --trace-flags and --profile-flags values (omit the middle space):\n"
"         1000 0000   show conversion into IR\n"
"         0100 0000   show after initial opt\n"
"         0010 0000   show after instrumentation\n"
"         0001 0000   show after second opt\n"
"         0000 1000   show after tree building\n"
"         0000 0100   show selecting insns\n"
"         0000 0010   show after reg-alloc\n"
"         0000 0001   show final assembly\n"
"         0000 0000   show summary profile only\n"
"        (Nb: you need --trace-notbelow and/or --trace-notabove\n"
"             with --trace-flags for full details)\n"
"    --vex-regalloc-version=2|3             [3]\n"
"\n"
"  debugging options for Valgrind tools that report errors\n"
"    --dump-error=<number>     show translation for basic block associated\n"
"                              with <number>'th error context [0=show none]\n"
"\n"
"  debugging options for Valgrind tools that replace malloc:\n"
"    --trace-malloc=no|yes     show client malloc details? [no]\n"
"    --xtree-compress-strings=no|yes   compress strings in xtree callgrind format [yes]\n"
"\n";

   const HChar usage3[] =
"\n"
"  Extra options read from ~/.valgrindrc, $VALGRIND_OPTS, ./.valgrindrc\n"
"\n"
"  %s is %s\n"
"  Valgrind is Copyright (C) 2000-2024, and GNU GPL'd, by Julian Seward et al.\n"
"  LibVEX is Copyright (C) 2004-2024, and GNU GPL'd, by OpenWorks LLP et al.\n"
"\n"
"  Bug reports, feedback, admiration, abuse, etc, to: %s.\n"
"\n";

   const HChar dyn_usage[] =
"Some command line settings are \"dynamic\", meaning they can be changed\n"
"while Valgrind is running, like this:\n"
"    From the shell, using vgdb. Example:\n"
"      $ vgdb \"v.clo --trace-children=yes --child-silent-after-fork=no\"\n"
"    From a gdb attached to the valgrind gdbserver. Example:\n"
"      (gdb) monitor v.clo --trace-children=yes --child-silent-after-fork=no\"\n"
"    From your program, using a client request. Example:\n"
"      #include <valgrind/valgrind.h>\n"
"      VALGRIND_CLO_CHANGE(\"--trace-children=yes\");\n"
"      VALGRIND_CLO_CHANGE(\"--child-silent-after-fork=no\");\n\n";


   HChar default_alignment[30];      // large enough
   HChar default_redzone_size[30];   // large enough

   // Ensure the message goes to stdout
   VG_(log_output_sink).fd = 1;
   VG_(log_output_sink).type = VgLogTo_Fd;

   if (VG_(needs).malloc_replacement) {
      VG_(sprintf)(default_alignment,    "%d",  VG_MIN_MALLOC_SZB);
      VG_(sprintf)(default_redzone_size, "%lu", VG_(tdict).tool_client_redzone_szB);
   } else {
      VG_(strcpy)(default_alignment,    "not used by this tool");
      VG_(strcpy)(default_redzone_size, "not used by this tool");
   }
   if (need_help > 1)
      /* 'usage1' a type as described after each arg. */
      VG_(printf)(usage1,
                  VG_(clo_vgdb_error)        /* int */,
                  default_alignment          /* char* */,
                  default_redzone_size       /* char* */,
                  VG_(clo_vgdb_poll)         /* int */,
                  VG_(vgdb_prefix_default)() /* char* */,
                  N_SECTORS_DEFAULT          /* int */,
                  MAX_THREADS_DEFAULT        /* int */
               );
   if (need_help > 1 && VG_(details).name) {
      VG_(printf)("  user options for %s:\n", VG_(details).name);
      if (VG_(needs).command_line_options)
	 VG_TDICT_CALL(tool_print_usage);
      else
	 VG_(printf)("    (none)\n");
   }
   if (need_help == 1) {
      VG_(printf)(dyn_usage);
      VG_(list_dynamic_options) ();
      VG_(printf)("valgrind: Use --help for more information.\n");
   }

   if (need_help > 2) {
      VG_(printf)("%s", usage2);

      if (VG_(details).name) {
         VG_(printf)("  debugging options for %s:\n", VG_(details).name);

         if (VG_(needs).command_line_options)
            VG_TDICT_CALL(tool_print_debug_usage);
         else
            VG_(printf)("    (none)\n");
      }
   }
   if (need_help > 1)
      VG_(printf)(usage3, VG_(details).name, VG_(details).copyright_author,
                  VG_BUGS_TO);
   VG_(exit)(0);
}


struct process_option_state {
   /* Whether the user has asked for --version/--help.  */
   Int need_version;
   Int need_help;

   /* Whether the user has explicitly provided --sigill-diagnostics
      or --show-error-list.
      If not explicitly given depends on general verbosity setting. */
   Bool sigill_diag_set;
   Bool show_error_list_set;

   /* Log to stderr by default, but usage message goes to stdout.  XML
      output is initially disabled. */
   VgLogTo log_to;  // Where is logging output to be sent?
   VgLogTo xml_to;  // Where is XML output to be sent?
   Int tmp_log_fd;
   Int tmp_xml_fd;
};

static void process_option (Clo_Mode mode,
                            HChar *arg, struct process_option_state *pos)
{
   const HChar* tmp_str;         // Used in a couple of places.
   Int   toolname_len = VG_(strlen)(VG_(clo_toolname));
   HChar* colon = arg;
   UInt   ix    = 0;

   /* Constants for parsing PX control flags. */
   const HChar* pxStrings[5]
      = { "sp-at-mem-access",      "unwindregs-at-mem-access",
          "allregs-at-mem-access", "allregs-at-each-insn", NULL };
   const VexRegisterUpdates pxVals[5]
      = { VexRegUpdSpAtMemAccess,      VexRegUpdUnwindregsAtMemAccess,
          VexRegUpdAllregsAtMemAccess, VexRegUpdAllregsAtEachInsn, 0/*inval*/ };

   VG_(set_Clo_Mode) (mode);

   // Look for a colon in the option name.
   while (*colon && *colon != ':' && *colon != '=')
      colon++;

   // Does it have the form "--toolname:foo"?  We have to do it at the start
   // in case someone has combined a prefix with a core-specific option,
   // eg.  "--memcheck:verbose".
   if (*colon == ':') {
      if (VG_STREQN(2,            arg,                "--") &&
          VG_STREQN(toolname_len, arg+2,              VG_(clo_toolname)) &&
          VG_STREQN(1,            arg+2+toolname_len, ":")) {
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
         // prefix doesn't match, declare it as recognised and skip this arg
         VG_(set_Clo_Recognised) ();
         return;
      }
   }

   if VG_XACT_CLOM(cloE, arg, "--version", pos->need_version, 1) {}
   else if (VG_STREQ_CLOM(cloED, arg, "-v") ||
            VG_STREQ_CLOM(cloED, arg, "--verbose"))
      VG_(clo_verbosity)++;
   else if (VG_STREQ_CLOM(cloED, arg, "-q") ||
            VG_STREQ_CLOM(cloED, arg, "--quiet"))
      VG_(clo_verbosity)--;
   else if VG_XACT_CLOM(cloE, arg, "--help-dyn-options", pos->need_help, 1) {}
   else if VG_XACT_CLOM(cloE, arg, "-h", pos->need_help, 2) {}
   else if VG_XACT_CLOM(cloE, arg, "--help", pos->need_help, 2) {}
   else if VG_XACT_CLOM(cloE, arg, "--help-debug", pos->need_help, 3) {}

   // The tool has already been determined, but we need to know the name
   // here.
   else if VG_STR_CLOM(cloE, arg, "--tool", VG_(clo_toolname)) {}

   // Set up VG_(clo_max_stackframe) and VG_(clo_main_stacksize).
   // These are needed by VG_(ii_create_image), which happens
   // before main_process_cmd_line_options().
   else if VG_INT_CLOM(cloE, arg, "--max-stackframe", VG_(clo_max_stackframe)) {}
   else if VG_INT_CLOM(cloE, arg, "--main-stacksize", VG_(clo_main_stacksize)) {}

   // Set up VG_(clo_max_threads); needed for VG_(tl_pre_clo_init)
   else if VG_INT_CLOM(cloE, arg, "--max-threads", VG_(clo_max_threads)) {}

   // Set up VG_(clo_sim_hints). This is needed a.o. for an inner
   // running in an outer, to have "no-inner-prefix" enabled
   // as early as possible.
   else if VG_USETX_CLOM (cloE, arg, "--sim-hints",
                          "lax-ioctls,lax-doors,fuse-compatible,"
                          "enable-outer,no-inner-prefix,"
                          "no-nptl-pthread-stackcache,fallback-llsc",
                          VG_(clo_sim_hints)) {}

   else if VG_STREQN_CLOM(0, 20, arg, "--command-line-only=") {} // m_commandline.c
   else if VG_STREQ(arg, "--")                   {}
   else if VG_STREQ_CLOM(cloD, arg, "-d") // pre-early + Dynamic
      VG_(debugLog_startup) (VG_(debugLog_getLevel)() + 1,
                             "dynamic option change");
   else if VG_STREQN_CLOM(0, 15, arg, "--profile-heap=")      {} // pre-early
   else if VG_STREQN_CLOM(0, 20, arg, "--core-redzone-size=") {} // pre-early
   else if VG_STREQN_CLOM(0, 15, arg, "--redzone-size=")      {} // pre-early
   else if VG_STREQN_CLOM(0, 17, arg, "--aspace-minaddr=")    {} // pre-early

   else if VG_BINT_CLOM(cloE, arg, "--valgrind-stacksize",
                        VG_(clo_valgrind_stacksize),
                        2*VKI_PAGE_SIZE, 10*VG_DEFAULT_STACK_ACTIVE_SZB)
      VG_(clo_valgrind_stacksize) = VG_PGROUNDUP(VG_(clo_valgrind_stacksize));

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

   /* These options are new, not yet handled by
      early_process_cmd_line_options. */
   else if VG_BOOL_CLO(arg, "--sigill-diagnostics", VG_(clo_sigill_diag))
      pos->sigill_diag_set = True;

   else if VG_BOOL_CLOM(cloPD, arg, "--stats",          VG_(clo_stats)) {}
   else if VG_BOOL_CLO(arg, "--xml",            VG_(clo_xml))
      VG_(debugLog_setXml)(VG_(clo_xml));

   else if VG_XACT_CLOM(cloPD, arg, "--vgdb=no",   VG_(clo_vgdb), Vg_VgdbNo) {}
   else if VG_XACT_CLOM(cloPD, arg, "--vgdb=yes",  VG_(clo_vgdb), Vg_VgdbYes) {}
   else if VG_XACT_CLOM(cloPD, arg, "--vgdb=full", VG_(clo_vgdb), Vg_VgdbFull) {
      /* automatically updates register values at each insn
         with --vgdb=full */
      VG_(clo_vex_control).iropt_register_updates_default
         = VG_(clo_px_file_backed)
         = VexRegUpdAllregsAtEachInsn;
   }
   else if VG_INT_CLOM (cloPD, arg, "--vgdb-poll",      VG_(clo_vgdb_poll)) {}
   else if VG_INT_CLOM (cloPD, arg, "--vgdb-error", VG_(clo_vgdb_error)) {}
   /* --launched-with-multi is an internal option used by vgdb to suppress
      some output that valgrind normally shows when using --vgdb-error.  */
   else if VG_BOOL_CLO (arg, "--launched-with-multi",
                        VG_(clo_launched_with_multi)) {}
   else if VG_USET_CLOM (cloPD, arg, "--vgdb-stop-at",
                         "startup,exit,abexit,valgrindabexit",
                         VG_(clo_vgdb_stop_at)) {}
   else if VG_STR_CLO (arg, "--vgdb-prefix",    VG_(clo_vgdb_prefix)) {
      VG_(arg_vgdb_prefix) = arg;
   }
   else if VG_BOOL_CLO(arg, "--vgdb-shadow-registers",
                       VG_(clo_vgdb_shadow_registers)) {}
   else if VG_BOOL_CLO(arg, "--demangle",       VG_(clo_demangle)) {}
   else if VG_STR_CLO (arg, "--soname-synonyms",VG_(clo_soname_synonyms)) {}
   else if VG_BOOL_CLO(arg, "--error-limit",    VG_(clo_error_limit)) {}
   else if VG_BOOL_CLO(arg, "--exit-on-first-error", VG_(clo_exit_on_first_error)) {}
   else if VG_INT_CLO (arg, "--error-exitcode", VG_(clo_error_exitcode)) {}
   else if VG_STR_CLOM (cloPD, arg, "--error-markers",  tmp_str) {
      Int m;
      const HChar *startpos = tmp_str;
      const HChar *nextpos;
      for (m = 0;
           VG_(Clo_Mode)() != cloE
              && m < sizeof(VG_(clo_error_markers))
              /sizeof(VG_(clo_error_markers)[0]);
           m++) {
         /* Release previous value if clo given multiple times. */
         VG_(free)(VG_(clo_error_markers)[m]);
         VG_(clo_error_markers)[m] = NULL;

         nextpos = VG_(strchr)(startpos, ',');
         if (!nextpos)
            nextpos = startpos + VG_(strlen)(startpos);
         if (startpos != nextpos) {
            VG_(clo_error_markers)[m]
               = VG_(malloc)("main.mpclo.2", nextpos - startpos + 1);
            VG_(memcpy)(VG_(clo_error_markers)[m], startpos,
                        nextpos - startpos);
            VG_(clo_error_markers)[m][nextpos - startpos] = '\0';
         }
         startpos = *nextpos ? nextpos + 1 : nextpos;
      }
   }
   else if VG_STR_CLOM(cloPD, arg, "--show-error-list", tmp_str) {
      if (VG_(strcmp)(tmp_str, "yes") == 0)
         VG_(clo_show_error_list) = 1;
      else if (VG_(strcmp)(tmp_str, "all") == 0)
         VG_(clo_show_error_list) = 2;
      else if (VG_(strcmp)(tmp_str, "no") == 0)
         VG_(clo_show_error_list) = 0;
      else
         VG_(fmsg_bad_option)(arg,
            "Bad argument, should be 'yes', 'all' or 'no'\n");
      pos->show_error_list_set = True; }
   else if (VG_STREQ_CLOM(cloPD, arg, "-s")) {
      VG_(clo_show_error_list) = 1;
      pos->show_error_list_set = True;
   }
   else if VG_BOOL_CLO(arg, "--show-emwarns",   VG_(clo_show_emwarns)) {}

   else if VG_BOOL_CLO(arg, "--run-libc-freeres", VG_(clo_run_libc_freeres)) {}
   else if VG_BOOL_CLO(arg, "--run-cxx-freeres",  VG_(clo_run_cxx_freeres)) {}
   else if VG_BOOL_CLOM(cloPD, arg, "--show-below-main",  VG_(clo_show_below_main)) {}
   else if VG_BOOL_CLO(arg, "--keep-debuginfo",   VG_(clo_keep_debuginfo)) {}
#if defined(VGO_linux)
   else if VG_BOOL_CLO(arg, "--enable-debuginfod", VG_(clo_enable_debuginfod)) {}
#endif
   else if VG_BOOL_CLOM(cloPD, arg, "--time-stamp",       VG_(clo_time_stamp)) {}
   else if VG_STR_CLO(arg, "--track-fds",         tmp_str) {
      if (VG_(strcmp)(tmp_str, "yes") == 0)
         VG_(clo_track_fds) = 1;
      else if (VG_(strcmp)(tmp_str, "all") == 0)
         VG_(clo_track_fds) = 2;
      else if (VG_(strcmp)(tmp_str, "no") == 0)
         VG_(clo_track_fds) = 0;
      else
         VG_(fmsg_bad_option)(arg,
            "Bad argument, should be 'yes', 'all' or 'no'\n");
   }
   else if VG_STR_CLO(arg, "--modify-fds",         tmp_str) {
      if (VG_(strcmp)(tmp_str, "high") == 0)
         VG_(clo_modify_fds) = 1;
      else if (VG_(strcmp)(tmp_str, "no") == 0)
         VG_(clo_modify_fds) = 0;
      else
         VG_(fmsg_bad_option)(arg,
            "Bad argument, should be 'high' or 'no'\n");
   }
   else if VG_BOOL_CLOM(cloPD, arg, "--trace-children",   VG_(clo_trace_children)) {}
   else if VG_BOOL_CLOM(cloPD, arg, "--child-silent-after-fork",
                        VG_(clo_child_silent_after_fork)) {}
   else if VG_INT_CLOM(cloPD, arg, "--scheduling-quantum",
                       VG_(clo_scheduling_quantum)) {}
   else if VG_STR_CLO(arg, "--fair-sched",        tmp_str) {
      if (VG_(Clo_Mode)() != cloP)
         ;
      else if (VG_(strcmp)(tmp_str, "yes") == 0)
         VG_(clo_fair_sched) = enable_fair_sched;
      else if (VG_(strcmp)(tmp_str, "try") == 0)
         VG_(clo_fair_sched) = try_fair_sched;
      else if (VG_(strcmp)(tmp_str, "no") == 0)
         VG_(clo_fair_sched) = disable_fair_sched;
      else
         VG_(fmsg_bad_option)(arg,
            "Bad argument, should be 'yes', 'try' or 'no'\n");
   }
   else if VG_BOOL_CLOM(cloPD, arg, "--trace-sched",      VG_(clo_trace_sched)) {}
   else if VG_BOOL_CLOM(cloPD, arg, "--trace-signals",    VG_(clo_trace_signals)) {}
   else if VG_BOOL_CLOM(cloPD, arg, "--trace-symtab",     VG_(clo_trace_symtab)) {}
   else if VG_STR_CLO (arg, "--trace-symtab-patt", VG_(clo_trace_symtab_patt)) {}
   else if VG_BOOL_CLOM(cloPD, arg, "--trace-cfi",        VG_(clo_trace_cfi)) {}
   else if VG_XACT_CLOM(cloPD, arg, "--debug-dump=syms",  VG_(clo_debug_dump_syms),
                        True) {}
   else if VG_XACT_CLOM(cloPD, arg, "--debug-dump=line",  VG_(clo_debug_dump_line),
                        True) {}
   else if VG_XACT_CLOM(cloPD, arg, "--debug-dump=frames",
                        VG_(clo_debug_dump_frames), True) {}
   else if VG_BOOL_CLOM(cloPD, arg, "--trace-redir",      VG_(clo_trace_redir)) {}

   else if VG_BOOL_CLOM(cloPD, arg, "--trace-syscalls",   VG_(clo_trace_syscalls)) {}
   else if VG_BOOL_CLOM(cloE, arg, "--wait-for-gdb",     VG_(clo_wait_for_gdb)) { 
      //--------------------------------------------------------------
      // Allow GDB attach
      //   p: logging
      //--------------------------------------------------------------
      /* Hook to delay things long enough so we can get the pid and
         attach GDB in another shell. */
      if (VG_(clo_wait_for_gdb)) {
         const int ms = 8000; // milliseconds
         VG_(debugLog)(1, "main", "Wait for GDB during %d ms\n", ms);
         VG_(printf)("pid=%d, entering delay %d ms loop\n", VG_(getpid)(), ms);
         VG_(poll)(NULL, 0, ms);
      }
   }

   else if VG_BOOL_CLOM(cloPD, arg, "--sym-offsets",      VG_(clo_sym_offsets)) {}
   else if VG_BUINT_CLOM(cloPD, arg, "--progress-interval",
                        VG_(clo_progress_interval), 3600) {}
   else if VG_BOOL_CLO(arg, "--read-inline-info", VG_(clo_read_inline_info)) {}
   else if VG_BOOL_CLO(arg, "--read-var-info",    VG_(clo_read_var_info)) {}

   else if VG_INT_CLO (arg, "--dump-error",       VG_(clo_dump_error))   {}
   else if VG_INT_CLO (arg, "--input-fd",         VG_(clo_input_fd))     {}
   else if VG_INT_CLO (arg, "--sanity-level",     VG_(clo_sanity_level)) {}
   else if VG_BINT_CLO(arg, "--num-callers",      VG_(clo_backtrace_size), 1,
                       VG_DEEPEST_BACKTRACE) {}
   else if VG_BINT_CLO(arg, "--num-transtab-sectors",
                       VG_(clo_num_transtab_sectors),
                       MIN_N_SECTORS, MAX_N_SECTORS) {}
   else if VG_BINT_CLO(arg, "--avg-transtab-entry-size",
                       VG_(clo_avg_transtab_entry_size),
                       50, 5000) {}
   else if VG_BINT_CLOM(cloPD, arg, "--merge-recursive-frames",
                        VG_(clo_merge_recursive_frames), 0,
                        VG_DEEPEST_BACKTRACE) {}

   else if VG_XACT_CLO(arg, "--smc-check=none",
                       VG_(clo_smc_check), Vg_SmcNone) {}
   else if VG_XACT_CLO(arg, "--smc-check=stack",
                       VG_(clo_smc_check), Vg_SmcStack) {}
   else if VG_XACT_CLO(arg, "--smc-check=all",
                       VG_(clo_smc_check), Vg_SmcAll) {}
   else if VG_XACT_CLO(arg, "--smc-check=all-non-file",
                       VG_(clo_smc_check), Vg_SmcAllNonFile) {}

   else if VG_USETX_CLO (arg, "--kernel-variant",
                         "bproc,"
                         "android-no-hw-tls,"
                         "android-gpu-sgx5xx,"
                         "android-gpu-adreno3xx",
                         VG_(clo_kernel_variant)) {}

   else if VG_BOOL_CLO(arg, "--dsymutil",        VG_(clo_dsymutil)) {}

   else if VG_STR_CLO (arg, "--trace-children-skip",
                       VG_(clo_trace_children_skip)) {}
   else if VG_STR_CLO (arg, "--trace-children-skip-by-arg",
                       VG_(clo_trace_children_skip_by_arg)) {}

   else if VG_BINT_CLOM(cloPD, arg, "--vex-iropt-verbosity",
                        VG_(clo_vex_control).iropt_verbosity, 0, 10) {}
   else if VG_BINT_CLO(arg, "--vex-iropt-level",
                       VG_(clo_vex_control).iropt_level, 0, 2) {}
   else if VG_BINT_CLO(arg, "--vex-regalloc-version",
                       VG_(clo_vex_control).regalloc_version, 2, 3) {}

   else if (VG_STRINDEX_CLO(arg, "--vex-iropt-register-updates",
                           pxStrings, ix)
            || VG_STRINDEX_CLO(arg, "--px-default", pxStrings, ix))
      // NB: --px-default is an alias for the hard-to-remember
      // --vex-iropt-register-updates, hence the same logic.
      {
         vg_assert(ix < 4);
         vg_assert(pxVals[ix] >= VexRegUpdSpAtMemAccess);
         vg_assert(pxVals[ix] <= VexRegUpdAllregsAtEachInsn);
         VG_(clo_vex_control).iropt_register_updates_default = pxVals[ix];
      }

   else if VG_STRINDEX_CLO(arg, "--px-file-backed", pxStrings, ix) {
         // Whereas --px-file-backed isn't
         // the same flag as --vex-iropt-register-updates.
         vg_assert(ix < 4);
         vg_assert(pxVals[ix] >= VexRegUpdSpAtMemAccess);
         vg_assert(pxVals[ix] <= VexRegUpdAllregsAtEachInsn);
         VG_(clo_px_file_backed) = pxVals[ix];
      }

   else if VG_BINT_CLO(arg, "--vex-iropt-unroll-thresh",
                       VG_(clo_vex_control).iropt_unroll_thresh, 0, 400) {}
   else if VG_BINT_CLO(arg, "--vex-guest-max-insns",
                       VG_(clo_vex_control).guest_max_insns, 1, 100) {}
   else if VG_BOOL_CLO(arg, "--vex-guest-chase",
                       VG_(clo_vex_control).guest_chase) {}

   else if VG_INT_CLO(arg, "--log-fd", pos->tmp_log_fd) {
      pos->log_to = VgLogTo_Fd;
      VG_(clo_log_fname_unexpanded) = NULL;
   }
   else if VG_INT_CLO(arg, "--xml-fd", pos->tmp_xml_fd) {
      pos->xml_to = VgLogTo_Fd;
      VG_(clo_xml_fname_unexpanded) = NULL;
   }

   else if VG_STR_CLO(arg, "--log-file", VG_(clo_log_fname_unexpanded)) {
      pos->log_to = VgLogTo_File;
   }
   else if VG_STR_CLO(arg, "--xml-file", VG_(clo_xml_fname_unexpanded)) {
      pos->xml_to = VgLogTo_File;
   }

   else if VG_STR_CLO(arg, "--log-socket", VG_(clo_log_fname_unexpanded)) {
      pos->log_to = VgLogTo_Socket;
   }
   else if VG_STR_CLO(arg, "--xml-socket", VG_(clo_xml_fname_unexpanded)) {
      pos->xml_to = VgLogTo_Socket;
   }

   else if VG_STR_CLO(arg, "--debuginfo-server",
                      VG_(clo_debuginfo_server)) {}

   else if VG_BOOL_CLO(arg, "--allow-mismatched-debuginfo",
                       VG_(clo_allow_mismatched_debuginfo)) {}

   else if VG_STR_CLO(arg, "--xml-user-comment",
                      VG_(clo_xml_user_comment)) {}

   else if VG_BOOL_CLO(arg, "--default-suppressions",
                       VG_(clo_default_supp)) {}

   else if VG_STR_CLOM(cloPD, arg, "--suppressions", tmp_str) {
      VG_(add_suppression_file)(tmp_str);
   }

   else if VG_STR_CLO (arg, "--fullpath-after", tmp_str) {
      VG_(addToXA)(VG_(clo_fullpath_after), &tmp_str);
   }

   else if VG_STR_CLO (arg, "--extra-debuginfo-path",
                       VG_(clo_extra_debuginfo_path)) {}

   else if VG_STR_CLO(arg, "--require-text-symbol", tmp_str) {
      /* String needs to be of the form C?*C?*, where C is any
         character, but is the same both times.  Having it in this
         form facilitates finding the boundary between the sopatt
         and the fnpatt just by looking for the second occurrence
         of C, without hardwiring any assumption about what C
         is. */
      HChar patt[7];
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
      VG_(addToXA)(VG_(clo_req_tsyms), &tmp_str);
   }

   /* "stuvwxyz" --> stuvwxyz (binary) */
   else if VG_STR_CLOM(cloPD, arg, "--trace-flags", tmp_str) {
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

   else if VG_INT_CLOM (cloPD, arg, "--trace-notbelow", VG_(clo_trace_notbelow)) {}

   else if VG_INT_CLOM (cloPD, arg, "--trace-notabove", VG_(clo_trace_notabove)) {}

   /* "stuvwxyz" --> stuvwxyz (binary) */
   else if VG_STR_CLOM(cloPD, arg, "--profile-flags", tmp_str) {
      Int j;
      if (8 != VG_(strlen)(tmp_str)) {
         VG_(fmsg_bad_option)(arg,
             "--profile-flags argument must have 8 digits\n");
      }
      for (j = 0; j < 8; j++) {
         if      ('0' == tmp_str[j]) { /* do nothing */ }
         else if ('1' == tmp_str[j]) VG_(clo_profyle_flags) |= (1 << (7-j));
         else {
            VG_(fmsg_bad_option)(arg,
               "--profile-flags argument can only contain 0s and 1s\n");
         }
      }
      VG_(clo_profyle_sbs) = True;
   }

   else if VG_INT_CLO (arg, "--profile-interval",
                       VG_(clo_profyle_interval)) {}

   else if VG_XACT_CLOM(cloPD, arg, "--gen-suppressions=no",
                       VG_(clo_gen_suppressions), 0) {}
   else if VG_XACT_CLOM(cloPD, arg, "--gen-suppressions=yes",
                       VG_(clo_gen_suppressions), 1) {}
   else if VG_XACT_CLOM(cloPD, arg, "--gen-suppressions=all",
                       VG_(clo_gen_suppressions), 2) {}

   else if VG_BINT_CLO(arg, "--unw-stack-scan-thresh",
                       VG_(clo_unw_stack_scan_thresh), 0, 100) {}
   else if VG_BINT_CLO(arg, "--unw-stack-scan-frames",
                       VG_(clo_unw_stack_scan_frames), 0, 32) {}

   else if VG_XACT_CLO(arg, "--resync-filter=no",
                       VG_(clo_resync_filter), 0) {}
   else if VG_XACT_CLO(arg, "--resync-filter=yes",
                       VG_(clo_resync_filter), 1) {}
   else if VG_XACT_CLO(arg, "--resync-filter=verbose",
                       VG_(clo_resync_filter), 2) {}

   else if ( VG_(Clo_Mode)() != cloE // tool does not have Early options
             && !VG_(Clo_Recognised) ()
             && (! VG_(needs).command_line_options
                 || ! VG_TDICT_CALL(tool_process_cmd_line_option, arg) )) {
      if (VG_(Clo_Mode)() == cloH)
         ;
      else if (VG_(Clo_Mode)() == cloP && !VG_(Clo_Recognised) ())
         VG_(fmsg_unknown_option)(arg);
      else if (VG_(Clo_Mode)() == cloD && !VG_(Clo_Recognised) ())
         VG_(umsg)("Ignoring dynamic change to unrecognised option %s\n", arg);
   }
}

void VG_(process_dynamic_option) (Clo_Mode mode, HChar *value)
{
   struct process_option_state dummy;
   process_option (mode, value, &dummy);
   // No need to handle a process_option_state once valgrind has started.
}

/* Peer at previously set up VG_(args_for_valgrind) and do some
   minimal command line processing that must happen early on:

   - show the version string, if requested (-v)
   - extract any request for help (-h --help, --help-dyn-options, --help-debug)
   - set VG_(toolname) (--tool=)
   - set VG_(clo_max_stackframe) (--max-stackframe=)
   - set VG_(clo_main_stacksize) (--main-stacksize=)
   - set VG_(clo_sim_hints) (--sim-hints=)
   - set VG_(clo_max_threads) (--max-threads)

   That's all it does.  The main command line processing is done below
   by main_process_cmd_line_options.  Note that
   main_process_cmd_line_options has to handle but ignore the ones we
   have handled here.
*/
static void early_process_cmd_line_options ( /*OUT*/Int* need_help )
{
   UInt   i;
   HChar* str;
   struct process_option_state pos
      = {0, 0, False, False, VgLogTo_Fd, VgLogTo_Fd, 2, -1};

   vg_assert( VG_(args_for_valgrind) );

   /* parse the options we have (only the options we care about now) */
   for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {

      str = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      vg_assert(str);
      process_option (cloE, str, &pos);
   }

   if (pos.need_version) {
      // Nb: the version string goes to stdout.
      VG_(log_output_sink).fd = 1;
      VG_(log_output_sink).type = VgLogTo_Fd;
      if (VG_(clo_verbosity) <= 1)
         VG_(printf)("valgrind-" VERSION "\n");
      else
         VG_(printf)("valgrind-" VERSION "-" VGGIT "\n");
      VG_(exit)(0);
   }

   *need_help = pos.need_help;

   /* For convenience */
   VG_N_THREADS = VG_(clo_max_threads);

#  if defined(VGO_solaris) || defined(VGO_darwin)
   /* Sim hint no-nptl-pthread-stackcache should be ignored. */
   VG_(clo_sim_hints) &= ~SimHint2S(SimHint_no_nptl_pthread_stackcache);
#  endif
}

/* The main processing for command line options.  See comments above
   on early_process_cmd_line_options. */
static
void main_process_cmd_line_options( void )
{
   Int i;
   struct process_option_state pos
      = {0, 0, False, False, VgLogTo_Fd, VgLogTo_Fd, 2, -1};

   /* Check for sane path in ./configure --prefix=... */
   if (VG_LIBDIR[0] != '/')
      VG_(err_config_error)("Please use absolute paths in "
                            "./configure --prefix=... or --libdir=...\n");

   vg_assert( VG_(args_for_valgrind) );

   VG_(clo_suppressions) = VG_(newXA)(VG_(malloc), "main.mpclo.4",
                                      VG_(free), sizeof(HChar *));
   VG_(clo_fullpath_after) = VG_(newXA)(VG_(malloc), "main.mpclo.5",
                                        VG_(free), sizeof(HChar *));
   VG_(clo_req_tsyms) = VG_(newXA)(VG_(malloc), "main.mpclo.6",
                                   VG_(free), sizeof(HChar *));

   /* BEGIN command-line processing loop */

   for (i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
      HChar* arg   = * (HChar**) VG_(indexXA)( VG_(args_for_valgrind), i );
      process_option (cloP, arg, &pos);
   }

   /* END command-line processing loop.  From now on, only dynamically
      changeable options will have an effect. */
   VG_(set_Clo_Mode)(cloD);

   /* Notify about deprecated features here. */

   /* Determine the path prefix for vgdb */
   if (VG_(clo_vgdb_prefix) == NULL)
     VG_(clo_vgdb_prefix) = VG_(vgdb_prefix_default)();

   /* Check various option values */

   if (VG_(clo_verbosity) < 0)
      VG_(clo_verbosity) = 0;

   if (!pos.sigill_diag_set)
      VG_(clo_sigill_diag) = (VG_(clo_verbosity) > 0);

   if (!pos.show_error_list_set) {
      if (VG_(clo_xml))
         VG_(clo_show_error_list) = VG_(clo_verbosity) >= 1;
      else
         VG_(clo_show_error_list) = VG_(clo_verbosity) >= 2;
   }

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

   if (VG_(clo_gen_suppressions) > 0 &&
       !VG_(needs).core_errors && !VG_(needs).tool_errors) {
      VG_(fmsg_bad_option)("--gen-suppressions=yes",
         "Can't use --gen-suppressions= with %s\n"
         "because it doesn't generate errors.\n", VG_(details).name);
   }
   if ((VG_(clo_exit_on_first_error)) &&
       (VG_(clo_error_exitcode)==0)) {
      VG_(fmsg_bad_option)("--exit-on-first-error=yes",
         "You must define a non nul exit error code, with --error-exitcode=...\n");
   }

#  if !defined(VGO_darwin)
   if (VG_(clo_resync_filter) != 0) {
      VG_(fmsg_bad_option)("--resync-filter=yes or =verbose",
                           "--resync-filter= is only available on MacOS X.\n");
      /*NOTREACHED*/
   }
#  endif

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

      /* Disallow dump_error in XML mode; sounds like a recipe for
         chaos.  No big deal; dump_error is a flag for debugging V
         itself. */
      if (VG_(clo_dump_error) > 0) {
         VG_(fmsg_bad_option)("--xml=yes",
            "Cannot be used together with --dump-error");
      }

      /* Disable error limits (this might be a bad idea!) */
      VG_(clo_error_limit) = False;
      /* Disable emulation warnings */

      /* Also, we want to set options for the leak checker, but that
         will have to be done in Memcheck's flag-handling code, not
         here. */
   }

#if defined(VGO_freebsd)
   if (VG_(clo_sanity_level) >= 3) {
      VG_(debugLog)(0, "main", "Warning: due to transparent memory mappings with MAP_STACK\n");
      VG_(debugLog)(0, "main", "--sanity-level=3 and above may give spurious errors.\n");
   }
#endif

   /* All non-logging-related options have been checked.  If the logging
      option specified is ok, we can switch to it, as we know we won't
      have to generate any other command-line-related error messages.
      (So far we should be still attached to stderr, so we can show on
      the terminal any problems to do with processing command line
      opts.) */
   VG_(init_log_xml_sinks)(pos.log_to, pos.xml_to,
                           pos.tmp_log_fd, pos.tmp_xml_fd);

   /* Register child at-fork handler which will take care of handling
      --child-silent-after-fork clo and also reopening output sinks for forked
      children, if requested via --log|xml-file= options. */
   VG_(atfork)(NULL, NULL, VG_(logging_atfork_child));

   // Suppressions related stuff

   if (VG_(clo_default_supp) &&
       (VG_(needs).core_errors || VG_(needs).tool_errors)) {
      /* If loading default is enabled, add it to the supp list. */
      static const HChar default_supp[] = "default.supp";
      Int len = VG_(strlen)(VG_(libdir)) + 1 + sizeof(default_supp);
      HChar *buf = VG_(malloc)("main.mpclo.3", len);
      VG_(sprintf)(buf, "%s/%s", VG_(libdir), default_supp);
      VG_(add_suppression_file)(buf);
      VG_(free)(buf);
   }
}


/*====================================================================*/
/*=== File descriptor setup                                        ===*/
/*====================================================================*/

/* Number of file descriptors that Valgrind tries to reserve for
   its own use - just a small constant. */
#define N_RESERVED_FDS (12)

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
      VG_(printf)("fd limits: host, before: cur %llu max %llu\n",
                  (ULong)rl.rlim_cur, (ULong)rl.rlim_max);

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
      VG_(printf)("fd limits: guest       : cur %d max %d\n",
                  VG_(fd_soft_limit), VG_(fd_hard_limit));
   }

   if (VG_(cl_exec_fd) != -1)
      VG_(cl_exec_fd) = VG_(safe_fd)( VG_(cl_exec_fd) );
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

/*static*/ struct {
   HChar bytes [VG_STACK_GUARD_SZB + VG_DEFAULT_STACK_ACTIVE_SZB + VG_STACK_GUARD_SZB];
} VG_(interim_stack);

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
   Int     need_help;
   ThreadId tid_main          = VG_INVALID_THREADID;
   Int     loglevel, i;
   XArray* addr2dihandle = NULL;

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
   VG_(client_envp) = (HChar**)envp;

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
      get --profile-heap=yes, --core-redzone-size, --redzone-size
      --aspace-minaddr which are needed by the time we start up dynamic
      memory management.  */
   loglevel = 0;
   for (i = 1; i < argc; i++) {
      const HChar* tmp_str;
      if (argv[i][0] != '-') break;
      if VG_STREQ(argv[i], "--") break;
      if VG_STREQ(argv[i], "-d") loglevel++;
      if VG_BOOL_CLOM(cloE, argv[i], "--profile-heap", VG_(clo_profile_heap)) {}
      if VG_BINT_CLOM(cloE, argv[i], "--core-redzone-size", VG_(clo_core_redzone_size),
                     0, MAX_CLO_REDZONE_SZB) {}
      if VG_BINT_CLOM(cloE, argv[i], "--redzone-size", VG_(clo_redzone_size),
                     0, MAX_CLO_REDZONE_SZB) {}
      if VG_STR_CLOM(cloE, argv[i], "--aspace-minaddr", tmp_str) {
         Bool ok = VG_(parse_Addr) (&tmp_str, &VG_(clo_aspacem_minAddr));
         if (!ok)
            VG_(fmsg_bad_option)(argv[i], "Invalid address\n");
         const HChar *errmsg;
         if (!VG_(am_is_valid_for_aspacem_minAddr)(VG_(clo_aspacem_minAddr),
                                                   &errmsg))
            VG_(fmsg_bad_option)(argv[i], "%s\n", errmsg);
      }
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

#if defined(VGO_freebsd)
   Int val;
   SizeT len = sizeof(val);
   //--------------------------------------------------------------
   // FreeBSD check security.bsd.unprivileged_proc_debug sysctl
   // This needs to be done before aspacemgr starts, otherwise that
   // will fail with mysterious error codes
   //--------------------------------------------------------------
   Int error = VG_(sysctlbyname)("security.bsd.unprivileged_proc_debug", &val, &len, 0, 0);
   if (error != -1 && val != 1) {
       VG_(debugLog)(0, "main", "Valgrind: FATAL:\n");
       VG_(debugLog)(0, "main", "security.bsd.unprivileged_proc_debug sysctl is 0.\n");
       VG_(debugLog)(0, "main", "   Set this sysctl with\n");
       VG_(debugLog)(0, "main", "   'sysctl security.bsd.unprivileged_proc_debug=1'.\n");
       VG_(debugLog)(0, "main", "   Cannot continue.\n");

       VG_(exit)(1);
   }
#endif


   //--------------------------------------------------------------
   // Start up the address space manager, and determine the
   // approximate location of the client's stack
   //   p: logging, plausible-stack
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Starting the address space manager\n");
   vg_assert(VKI_PAGE_SIZE    == 4096  || VKI_PAGE_SIZE == 8192
             || VKI_PAGE_SIZE == 16384 || VKI_PAGE_SIZE == 32768
             || VKI_PAGE_SIZE == 65536);
   vg_assert(VKI_MAX_PAGE_SIZE    == 4096  || VKI_MAX_PAGE_SIZE == 8192
             || VKI_MAX_PAGE_SIZE == 16384 || VKI_MAX_PAGE_SIZE == 32768
             || VKI_MAX_PAGE_SIZE == 65536);
   vg_assert(VKI_PAGE_SIZE <= VKI_MAX_PAGE_SIZE);
   vg_assert(VKI_PAGE_SIZE     == (1 << VKI_PAGE_SHIFT));
   vg_assert(VKI_MAX_PAGE_SIZE == (1 << VKI_MAX_PAGE_SHIFT));
   the_iicii.clstack_end = VG_(am_startup)( the_iicii.sp_at_startup );
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
     VG_(free)( p );
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
   // We used to set the process datasize rlimit to zero to prevent
   // any internal use of brk() from having any effect. But later
   // linux kernels redefine RLIMIT_DATA as the size of any data
   // areas, including some dynamic mmap memory allocations.
   // See bug #357833 for the commit that went into linux 4.5
   // changing the definition of RLIMIT_DATA. So don't mess with
   // RLIMIT_DATA here now anymore. Just remember it for use in
   // the syscall wrappers.
   VG_(getrlimit)(VKI_RLIMIT_DATA, &VG_(client_rlimit_data));

   // Get the current process stack rlimit.
   VG_(getrlimit)(VKI_RLIMIT_STACK, &VG_(client_rlimit_stack));

   //--------------------------------------------------------------
   // Figure out what sort of CPU we're on, and whether it is
   // able to run V.
   /* The vex_archinfo structure is passed down later to the client
    * to verify the HW info settings are consistent.
    */
   VexArchInfo vex_archinfo;
   VG_(debugLog)(1, "main", "Get hardware capabilities ...\n");
   { VexArch     vex_arch;
     Bool ok = VG_(machine_get_hwcaps)();
     if (!ok) {
        VG_(printf)("\n");
        VG_(printf)("valgrind: fatal error: unsupported CPU.\n");
        VG_(printf)("   Supported CPUs are:\n");
        VG_(printf)("   * x86 (practically any; Pentium-I or above), "
                    "AMD Athlon or above)\n");
        VG_(printf)("   * AMD Athlon64/Opteron\n");
        VG_(printf)("   * ARM (armv7)\n");
        VG_(printf)("   * MIPS (mips32 and above; mips64 and above)\n");
        VG_(printf)("   * PowerPC (most; ppc405 and above)\n");
        VG_(printf)("   * System z (64bit only - s390x; z990 and above)\n");
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
   VG_(record_startup_wd)();
   const HChar *wd = VG_(get_startup_wd)();
   VG_(debugLog)(1, "main", "... %s\n", wd != NULL ? wd : "<NO CWD>" );

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
   early_process_cmd_line_options(&need_help);

   // BEGIN HACK
   // When changing the logic for the VG_(clo_read_inline_info) default,
   // the manual and --help output have to be changed accordingly.
   vg_assert(VG_(clo_toolname) != NULL);
   vg_assert(VG_(clo_read_inline_info) == False);
#  if !defined(VGO_darwin)
   if (0 == VG_(strcmp)(VG_(clo_toolname), "memcheck")
       || 0 == VG_(strcmp)(VG_(clo_toolname), "helgrind")
       || 0 == VG_(strcmp)(VG_(clo_toolname), "drd")
       || 0 == VG_(strcmp)(VG_(clo_toolname), "massif")
       || 0 == VG_(strcmp)(VG_(clo_toolname), "dhat")) {
      /* Change the default setting.  Later on (just below)
         main_process_cmd_line_options should pick up any
         user-supplied setting for it and will override the default
         set here. */
      VG_(clo_read_inline_info) = True;
   }
#  endif
   // END HACK

   // Set default vex control params.
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
   //   p: early_process_cmd_line_options [for VG_(clo_toolname)]
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

#     if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)
      the_iicii.argv              = argv;
      the_iicii.envp              = envp;
      the_iicii.toolname          = VG_(clo_toolname);
#     else
#       error "Unknown platform"
#     endif

      /* NOTE: this call reads VG_(clo_main_stacksize). */
      the_iifii = VG_(ii_create_image)( the_iicii, &vex_archinfo );
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
   // create fake /proc/<pid>/cmdline and /proc/<pid>/auxv files
   // and then unlink them, but hold onto the fds, so we can handr
   // them out to the client when it tries to open
   // /proc/<pid>/cmdline or /proc/<pid>/auxv for itself.
   //   p: setup file descriptors
   //   p: ii_create_image for VG_(client_auxv) setup.
   //--------------------------------------------------------------
   VG_(cl_cmdline_fd) = -1;
   VG_(cl_auxv_fd) = -1;
#if defined(VGO_solaris)
   VG_(cl_psinfo_fd) = -1;
#endif

#if defined(VGO_linux) || defined(VGO_solaris)
   if (!need_help) {
      HChar  buf[50];   // large enough
      HChar  buf2[VG_(mkstemp_fullname_bufsz)(sizeof buf - 1)];
      Int    fd, r;

#if defined(VGO_linux) || defined(SOLARIS_PROC_CMDLINE)
      /* Fake /proc/<pid>/cmdline only on Linux and Solaris if supported. */
      HChar  nul[1];
      const HChar* exename;

      VG_(debugLog)(1, "main", "Create fake /proc/<pid>/cmdline\n");

      VG_(sprintf)(buf, "proc_%d_cmdline", VG_(getpid)());
      fd = VG_(mkstemp)( buf, buf2 );
      if (fd == -1)
         VG_(err_config_error)("Can't create client cmdline file in %s\n", buf2);

      nul[0] = 0;
      exename = VG_(args_the_exename);
      VG_(write)(fd, exename, VG_(strlen)( exename ));
      VG_(write)(fd, nul, 1);

      for (i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
         HChar* arg = * (HChar**) VG_(indexXA)( VG_(args_for_client), i );
         VG_(write)(fd, arg, VG_(strlen)( arg ));
         VG_(write)(fd, nul, 1);
      }

      /* Don't bother to seek the file back to the start; instead do
	 it every time a copy of it is given out (by PRE(sys_open) or
	 PRE(sys_openat)). That is probably more robust across fork() etc. */

      /* Now delete it, but hang on to the fd. */
      r = VG_(unlink)( buf2 );
      if (r)
         VG_(err_config_error)("Can't delete client cmdline file in %s\n", buf2);

      VG_(cl_cmdline_fd) = fd;
#endif // defined(VGO_linux) || defined(SOLARIS_PROC_CMDLINE)

      /* Fake /proc/<pid>/auxv on both Linux and Solaris. */
      VG_(debugLog)(1, "main", "Create fake /proc/<pid>/auxv\n");

      VG_(sprintf)(buf, "proc_%d_auxv", VG_(getpid)());
      fd = VG_(mkstemp)( buf, buf2 );
      if (fd == -1)
         VG_(err_config_error)("Can't create client auxv file in %s\n", buf2);

      UWord *client_auxv = VG_(client_auxv);
      unsigned int client_auxv_len = 0;
      while (*client_auxv != 0) {
         client_auxv++;
         client_auxv++;
         client_auxv_len += 2 * sizeof(UWord);
      }
      client_auxv_len += 2 * sizeof(UWord);

      VG_(write)(fd, VG_(client_auxv), client_auxv_len);

      /* Don't bother to seek the file back to the start; instead do
	 it every time a copy of it is given out (by PRE(sys_open)).
	 That is probably more robust across fork() etc. */

      /* Now delete it, but hang on to the fd. */
      r = VG_(unlink)( buf2 );
      if (r)
         VG_(err_config_error)("Can't delete client auxv file in %s\n", buf2);

      VG_(cl_auxv_fd) = fd;

#if defined(VGO_solaris)
      /* Fake /proc/<pid>/psinfo on Solaris.
       * Contents will be fetched and partially faked later on the fly. */
      VG_(debugLog)(1, "main", "Create fake /proc/<pid>/psinfo\n");

      VG_(sprintf)(buf, "proc_%d_psinfo", VG_(getpid)());
      fd = VG_(mkstemp)( buf, buf2 );
      if (fd == -1)
         VG_(err_config_error)("Can't create client psinfo file in %s\n", buf2);

      /* Now delete it, but hang on to the fd. */
      r = VG_(unlink)( buf2 );
      if (r)
         VG_(err_config_error)("Can't delete client psinfo file in %s\n", buf2);

      VG_(cl_psinfo_fd) = fd;
#endif /* VGO_solaris */
   }
#endif

#if defined(VGO_freebsd)
   /* On FreeBSD /proc is optional
    * Most functionality is accessed through sysctl instead */
   if (!need_help) {
      struct vg_stat statbuf;
      SysRes statres = VG_(stat)("/proc", &statbuf);
      if (!sr_isError(statres) || VKI_S_ISLNK(statbuf.mode)) {
         VG_(have_slash_proc) = True;
      }
      // each directory contains the following that might get read
      // file - a symlink to the exe
      // cmdline - null separate command line
      // etype - the executable type e.g., FreeBSD ELF64 (same for guest and host)
      // map - a memory map, tricky to synthesize
      // rlimit - list of process limits
      // status - process, pid, ppid pts cty uid gid and some other stuff
   }
#endif

   //--------------------------------------------------------------
   // Init tool part 1: pre_clo_init
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Initialise the tool part 1 (pre_clo_init)\n");
   VG_(tl_pre_clo_init)();
   // Activate var info readers, if the tool asked for it:
   if (VG_(needs).var_info)
      VG_(clo_read_var_info) = True;

   //--------------------------------------------------------------
   // If --tool and --help/--help-debug was given, now give the core+tool
   // help message
   //   p: early_process_cmd_line_options() [for 'need_help']
   //   p: tl_pre_clo_init                  [for 'VG_(tdict).usage']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Print help and quit, if requested\n");
   if (need_help) {
      usage_NORETURN(need_help);
   }

   //--------------------------------------------------------------
   // Process command line options to Valgrind + tool
   //   p: setup_client_stack()      [for 'VG_(client_arg[cv]']
   //   p: setup_file_descriptors()  [for 'VG_(fd_xxx_limit)']
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main",
                    "(main_) Process Valgrind's command line options, "
                    "setup logging\n");
   main_process_cmd_line_options();

   //--------------------------------------------------------------
   // Zeroise the millisecond counter by doing a first read of it.
   //   p: none
   //--------------------------------------------------------------
   (void) VG_(read_millisecond_timer)();

   //--------------------------------------------------------------
   // Print the preamble
   //   p: tl_pre_clo_init            [for 'VG_(details).name' and friends]
   //   p: main_process_cmd_line_options()
   //         [for VG_(clo_verbosity), VG_(clo_xml)]
   //--------------------------------------------------------------
   VG_(debugLog)(1, "main", "Print the preamble...\n");
   VG_(print_preamble)(VG_(log_output_sink).type != VgLogTo_File);
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
         and finish initialising the needs. */
      const HChar* s;
      Bool  ok;
      ok = VG_(finish_needs_init)( &s );
      if (!ok) {
         VG_(core_panic)(s);
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
   // Search for file descriptors that are inherited from our parent
   //   p: main_process_cmd_line_options  [for VG_(clo_track_fds)]
   //--------------------------------------------------------------
   if (VG_(clo_track_fds)) {
      VG_(debugLog)(1, "main", "Init preopened fds\n");
      VG_(init_preopened_fds)();
   }

#if defined(VGO_solaris)
   VG_(syswrap_init)();
#endif

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

   vg_assert(!addr2dihandle);
   addr2dihandle = VG_(newXA)( VG_(malloc), "main.vm.2",
                               VG_(free), sizeof(Addr_n_ULong) );

#  if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
   { Addr* seg_starts;
     Int   n_seg_starts;
     Addr_n_ULong anu;

     seg_starts = VG_(get_segment_starts)( SkFileC | SkFileV, &n_seg_starts );
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
     seg_starts = VG_(get_segment_starts)( SkFileC, &n_seg_starts );
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
      HChar buf[50];    // large enough
      VG_(elapsed_wallclock_time)(buf, sizeof buf);
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
   vg_assert(tid_main < VG_N_THREADS
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

     vg_assert(addr2dihandle);

     /* Mark the main thread as running while we tell the tool about
        the client memory so that the tool can associate that memory
        with the main thread. */
     vg_assert(VG_(running_tid) == VG_INVALID_THREADID);
     VG_(running_tid) = tid_main;

     seg_starts = VG_(get_segment_starts)( SkFileC | SkAnonC | SkShmC,
                                           &n_seg_starts );
     vg_assert(seg_starts && n_seg_starts >= 0);

     /* Show client segments to the tool */
     for (i = 0; i < n_seg_starts; i++) {
        Word j, n;
        NSegment const* seg
           = VG_(am_find_nsegment)( seg_starts[i] );
        vg_assert(seg);
        vg_assert(seg->kind == SkFileC || seg->kind == SkAnonC ||
                  seg->kind == SkShmC);
        vg_assert(seg->start == seg_starts[i]);
        {
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
                  vg_assert(anl->ull > 0); /* check it's a valid handle */
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

     /* Darwin only: tell the tools where the client's kernel commpage
        is.  It would be better to do this by telling aspacemgr about
        it -- see the now disused record_system_memory() in
        initimg-darwin.c -- but that causes the sync checker to fail,
        since the mapping doesn't appear in the kernel-supplied
        process map.  So do it here instead. */
#    if defined(VGP_amd64_darwin)
     VG_TRACK( new_mem_startup,
               0x7fffffe00000, 0x7ffffffff000-0x7fffffe00000,
               True, False, True, /* r-x */
               0 /* di_handle: no associated debug info */ );
#    elif defined(VGP_x86_darwin)
     VG_TRACK( new_mem_startup,
               0xfffec000, 0xfffff000-0xfffec000,
               True, False, True, /* r-x */
               0 /* di_handle: no associated debug info */ );
#    endif

     /* Clear the running thread indicator */
     VG_(running_tid) = VG_INVALID_THREADID;
     vg_assert(VG_(running_tid) == VG_INVALID_THREADID);
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
   { /* Mark the main thread as running while we tell the tool about
        the client memory which could be tracked during initial image
        finalisation. So the tool can associate that memory with the
        main thread. */
     vg_assert(VG_(running_tid) == VG_INVALID_THREADID);
     VG_(running_tid) = tid_main;

     VG_(ii_finalise_image)( the_iifii );

     /* Clear the running thread indicator */
     VG_(running_tid) = VG_INVALID_THREADID;
     vg_assert(VG_(running_tid) == VG_INVALID_THREADID);
   }

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
   VG_(clstk_id) = VG_(register_stack)(VG_(clstk_start_base), VG_(clstk_end));

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

/* Return the exit code to use when tid exits, depending on the tid os_state
   exit code and the clo options controlling valgrind exit code. */
static
Int tid_exit_code (ThreadId tid)
{
   if (VG_(clo_error_exitcode) > 0 && VG_(get_n_errs_found)() > 0)
      /* Change the application return code to user's return code,
         if an error was found */
      return VG_(clo_error_exitcode);
   else
      /* otherwise, return the client's exit code, in the normal
         way. */
      return VG_(threads)[tid].os_state.exitcode;
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
   case, since it will run __gnu_cxx::__freeres and libc_freeres functions,
   thus allowing other lurking threads to run again.  Hmm. */

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

   /* Try to do final tidyup on "normal" exit, not on FatalSig.  */
   if (tids_schedretcode == VgSrc_ExitThread) {

      // We are the last surviving thread.  Right?
      vg_assert( VG_(count_living_threads)() == 1 );

      // Wait for all other threads to exit.
      // jrs: Huh?  but they surely are already gone
      VG_(reap_threads)(tid);

      // Clean the client up before the final report.
      // This causes __gnu_cxx::__freeres and libc_freeres functions to run.
      final_tidyup(tid);

      /* be paranoid */
      vg_assert(VG_(is_running_thread)(tid));
      vg_assert(VG_(count_living_threads)() == 1);

   } else if (tids_schedretcode == VgSrc_ExitProcess) {

      // We may not be the last surviving thread.  However, we
      // want to shut down the entire process.  We hold the lock
      // and we need to keep hold of it all the way out, in order
      // that none of the other threads ever run again.
      vg_assert( VG_(count_living_threads)() >= 1 );

      // Clean the client up before the final report.
      // This causes __gnu_cxx::__freeres and libc_freeres functions to run.
      // Perhaps this is unsafe, as per comment above.
      final_tidyup(tid);

      /* be paranoid */
      vg_assert(VG_(is_running_thread)(tid));
      vg_assert(VG_(count_living_threads)() >= 1);
   }

   /* Final call to gdbserver, if requested. */
   if (VG_(gdbserver_stop_at) (VgdbStopAt_Abexit)
              && tid_exit_code (tid) != 0) {
      if (!(VG_(clo_launched_with_multi)))
         VG_(umsg)("(action at abexit, exit code %d) vgdb me ... \n",
                   tid_exit_code (tid));
      VG_(gdbserver) (tid);
   } else if (VG_(gdbserver_stop_at) (VgdbStopAt_Exit)) {
      if (!(VG_(clo_launched_with_multi)))
          VG_(umsg)("(action at exit, exit code %d) vgdb me ... \n",
                    tid_exit_code (tid));
      VG_(gdbserver) (tid);
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
      HChar buf[50];    // large enough
      VG_(elapsed_wallclock_time)(buf, sizeof buf);
      VG_(printf_xml)( "<status>\n"
                              "  <state>FINISHED</state>\n"
                              "  <time>%pS</time>\n"
                              "</status>\n"
                              "\n",
                              buf);
   }

   /* Print out file descriptor summary and stats. */
   if (VG_(clo_track_fds))
      VG_(show_open_fds)("at exit");

   /* Call the tool's finalisation function.  This makes Memcheck's
      leak checker run, and possibly chuck a bunch of leak errors into
      the error management machinery. */
   VG_TDICT_CALL(tool_fini, 0/*exitcode*/);

   if ((VG_(needs).core_errors && VG_(found_or_suppressed_errs)())
       || VG_(needs).tool_errors) {
      if (VG_(clo_verbosity) == 1
          && !VG_(clo_xml)
          && !VG_(clo_show_error_list))
         VG_(message)(Vg_UserMsg,
                      "For lists of detected and suppressed errors,"
                      " rerun with: -s\n");

      /* Show the error counts. */
      if (VG_(clo_xml)) {
         VG_(show_error_counts_as_XML)();
      }

      /* In XML mode, this merely prints the used suppressions. */
      VG_(show_all_errors)(VG_(clo_verbosity), VG_(clo_xml), VG_(clo_show_error_list));
   }

   if (VG_(clo_xml)) {
      VG_(printf_xml)("\n");
      VG_(printf_xml)("</valgrindoutput>\n");
      VG_(printf_xml)("\n");
   }

   VG_(sanity_check_general)( True /*include expensive checks*/ );

   if (VG_(clo_stats))
      VG_(print_all_stats)(VG_(clo_verbosity) >= 1, /* Memory stats */
                           False /* tool prints stats in the tool fini */);

   /* Show a profile of the heap(s) at shutdown.  Optionally, first
      throw away all the debug info, as that makes it easy to spot
      leaks in the debuginfo reader. */
   if (VG_(clo_profile_heap)) {
      if (0) VG_(di_discard_ALL_debuginfo)();
      VG_(print_arena_cc_analysis)();
   }

   /* If profiling has been requested, but with zero interval, it
      means "profile at the end of the run only".  In which case we
      need to dump the profile now. */
   if (VG_(clo_profyle_sbs) && VG_(clo_profyle_interval) == 0) {
      VG_(get_and_show_SB_profile)(0/*denoting end-of-run*/);
   }

   /* Print Vex storage stats */
   if (0)
       LibVEX_ShowAllocStats();

   /* Flush any output cached by previous calls to VG_(message). */
   VG_(message_flush)();

   /* Terminate gdbserver if ever it was started. We terminate it here
      so that it get the output above if output was redirected to
      gdb */
   VG_(gdbserver_exit) (tid, tids_schedretcode);

   /* Ok, finally exit in the os-specific way, according to the scheduler's
      return code.  In short, if the (last) thread exited by calling
      sys_exit, do likewise; if the (last) thread stopped due to a fatal
      signal, terminate the entire system with that same fatal signal. */
   VG_(debugLog)(1, "core_os",
                 "VG_(terminate_NORETURN)(tid=%u) schedretcode %s"
                 " os_state.exit_code %ld fatalsig %d\n",
                 tid, VG_(name_of_VgSchedReturnCode)(tids_schedretcode),
                 VG_(threads)[tid].os_state.exitcode,
                 VG_(threads)[tid].os_state.fatalsig);

   switch (tids_schedretcode) {
   case VgSrc_ExitThread:  /* the normal way out (Linux, Solaris) */
   case VgSrc_ExitProcess: /* the normal way out (Darwin) */
      VG_(client_exit)(tid_exit_code (tid));
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
   Clean up the client by calling __gnu_cxx::__freeres() (if requested)
   and __libc_freeres() (if requested).
*/
static void final_tidyup(ThreadId tid)
{
#if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
   Addr freeres_wrapper = VG_(client_freeres_wrapper);

   vg_assert(VG_(is_running_thread)(tid));

   if (freeres_wrapper == 0) {
      return; /* can't do it */
   }

   Vg_FreeresToRun to_run = 0;
   if (VG_(needs).cxx_freeres && VG_(clo_run_cxx_freeres)) {
      to_run |= VG_RUN__GNU_CXX__FREERES;
   }

   if (VG_(needs).libc_freeres && VG_(clo_run_libc_freeres)) {
      to_run |= VG_RUN__LIBC_FREERES;
   }

   if (to_run == 0) {
      return; /* won't do it */
   }

#  if defined(VGP_ppc64be_linux)
   Addr r2 = VG_(get_tocptr)(VG_(current_DiEpoch)(),
                             freeres_wrapper);
   if (r2 == 0) {
      VG_(message)(Vg_UserMsg,
                   "Caught __NR_exit, but can't run __gnu_cxx::__freeres()\n");
      VG_(message)(Vg_UserMsg,
                   "   or __libc_freeres() since cannot establish TOC pointer "
                   "for it.\n");
      return;
   }
#  endif

   if (VG_(clo_verbosity) > 2  ||
       VG_(clo_trace_syscalls) ||
       VG_(clo_trace_sched)) {

      vg_assert(to_run > 0);
      vg_assert(to_run <= (VG_RUN__GNU_CXX__FREERES | VG_RUN__LIBC_FREERES));

      const HChar *msgs[] = {"__gnu_cxx::__freeres()", "__libc_freeres()",
                             "__gnu_cxx::__freeres and __libc_freeres()"};
      VG_(message)(Vg_DebugMsg,
                   "Caught __NR_exit; running %s wrapper\n", msgs[to_run - 1]);
   }

   /* Set thread context to point to freeres_wrapper.
      ppc64be-linux note: freeres_wrapper gives us the real
      function entry point, not a fn descriptor, so can use it
      directly.  However, we need to set R2 (the toc pointer)
      appropriately. */
   VG_(set_IP)(tid, freeres_wrapper);

#  if defined(VGP_ppc64be_linux)
   VG_(threads)[tid].arch.vex.guest_GPR2 = r2;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestPPC64State, guest_GPR2),
            sizeof(VG_(threads)[tid].arch.vex.guest_GPR2));
#  elif  defined(VGP_ppc64le_linux)
   /* setting GPR2 but not really needed, GPR12 is needed */
   VG_(threads)[tid].arch.vex.guest_GPR2  = freeres_wrapper;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestPPC64State, guest_GPR2),
            sizeof(VG_(threads)[tid].arch.vex.guest_GPR2));
   VG_(threads)[tid].arch.vex.guest_GPR12 = freeres_wrapper;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestPPC64State, guest_GPR12),
            sizeof(VG_(threads)[tid].arch.vex.guest_GPR12));
#  endif
   /* mips-linux note: we need to set t9 */
#  if defined(VGP_mips32_linux) || defined(VGP_nanomips_linux)
   VG_(threads)[tid].arch.vex.guest_r25 = freeres_wrapper;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestMIPS32State, guest_r25),
            sizeof(VG_(threads)[tid].arch.vex.guest_r25));
#  elif defined(VGP_mips64_linux)
   VG_(threads)[tid].arch.vex.guest_r25 = freeres_wrapper;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestMIPS64State, guest_r25),
            sizeof(VG_(threads)[tid].arch.vex.guest_r25));
#  endif

   /* Pass a parameter to freeres_wrapper(). */
#  if defined(VGA_x86)
   Addr sp = VG_(threads)[tid].arch.vex.guest_ESP;
   *((UWord *) sp) = to_run;
   VG_TRACK(post_mem_write, Vg_CoreClientReq, tid, sp, sizeof(UWord));
   sp = sp - sizeof(UWord);
   VG_(threads)[tid].arch.vex.guest_ESP = sp;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestX86State, guest_ESP),
            sizeof(VG_(threads)[tid].arch.vex.guest_ESP));
#  elif defined(VGA_amd64)
   VG_(threads)[tid].arch.vex.guest_RDI = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestAMD64State, guest_RDI),
            sizeof(VG_(threads)[tid].arch.vex.guest_RDI));
#   elif defined(VGA_arm)
   VG_(threads)[tid].arch.vex.guest_R0 = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestARMState, guest_R0),
            sizeof(VG_(threads)[tid].arch.vex.guest_R0));
#  elif defined(VGA_arm64)
   VG_(threads)[tid].arch.vex.guest_X0 = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestARM64State, guest_X0),
            sizeof(VG_(threads)[tid].arch.vex.guest_X0));
#  elif defined(VGA_mips32) || defined(VGA_nanomips)
   VG_(threads)[tid].arch.vex.guest_r4 = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestMIPS32State, guest_r4),
            sizeof(VG_(threads)[tid].arch.vex.guest_r4));
#  elif defined(VGA_mips64)
   VG_(threads)[tid].arch.vex.guest_r4 = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestMIPS64State, guest_r4),
            sizeof(VG_(threads)[tid].arch.vex.guest_r4));
#  elif defined(VGA_ppc32)
   VG_(threads)[tid].arch.vex.guest_GPR3 = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestPPC32State, guest_GPR3),
            sizeof(VG_(threads)[tid].arch.vex.guest_GPR3));
#  elif defined(VGA_ppc64be) || defined(VGA_ppc64le)
   VG_(threads)[tid].arch.vex.guest_GPR3 = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestPPC64State, guest_GPR3),
            sizeof(VG_(threads)[tid].arch.vex.guest_GPR3));
#  elif defined(VGA_riscv64)
   VG_(threads)[tid].arch.vex.guest_x10 = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestRISCV64State, guest_x10),
            sizeof(VG_(threads)[tid].arch.vex.guest_x10));
#  elif defined(VGA_s390x)
   VG_(threads)[tid].arch.vex.guest_r2 = to_run;
   VG_TRACK(post_reg_write, Vg_CoreClientReq, tid,
            offsetof(VexGuestS390XState, guest_r2),
            sizeof(VG_(threads)[tid].arch.vex.guest_r2));
#else
   I_die_here : architecture missing in m_main.c
#endif

   /* Block all blockable signals by copying the real block state into
      the thread's block state */
   VG_(sigprocmask)(VKI_SIG_BLOCK, NULL, &VG_(threads)[tid].sig_mask);
   VG_(threads)[tid].tmp_sig_mask = VG_(threads)[tid].sig_mask;

   /* and restore handlers to default. */
   VG_(set_default_handler)(VKI_SIGSEGV);
   VG_(set_default_handler)(VKI_SIGBUS);
   VG_(set_default_handler)(VKI_SIGILL);
   VG_(set_default_handler)(VKI_SIGFPE);
   VG_(set_default_handler)(VKI_SIGSYS);

   // We were exiting, so assert that...
   vg_assert(VG_(is_exiting)(tid));
   // ...but now we're not again.
   VG_(threads)[tid].exitreason = VgSrc_None;

   // Run until client thread exits - ideally with FREERES_DONE,
   // but exit/exitgroup/signal will do.
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

   1. gcc may emit calls to memcpy, memmove and memset to deal with
      structure assignments etc.  Since we have chosen to ignore all the
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
void* memmove(void *dest, const void *src, SizeT n);
void* memmove(void *dest, const void *src, SizeT n) {
   return VG_(memmove)(dest,src,n);
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

#endif /* defined(VGP_arm_linux) */

/* Some Android helpers.  See bug 368529. */
#if defined(__clang__) \
    && (defined(VGPV_arm_linux_android) \
        || defined(VGPV_x86_linux_android) \
        || defined(VGPV_mips32_linux_android) \
        || defined(VGPV_arm64_linux_android))

/* Replace __aeabi_memcpy* functions with vgPlain_memcpy. */
void *__aeabi_memcpy(void *dest, const void *src, SizeT n);
void *__aeabi_memcpy(void *dest, const void *src, SizeT n)
{
    return VG_(memcpy)(dest, src, n);
}

void *__aeabi_memcpy4(void *dest, const void *src, SizeT n);
void *__aeabi_memcpy4(void *dest, const void *src, SizeT n)
{
    return VG_(memcpy)(dest, src, n);
}

void *__aeabi_memcpy8(void *dest, const void *src, SizeT n);
void *__aeabi_memcpy8(void *dest, const void *src, SizeT n)
{
    return VG_(memcpy)(dest, src, n);
}

/* Replace __aeabi_memclr* functions with vgPlain_memset. */
void *__aeabi_memclr(void *dest, SizeT n);
void *__aeabi_memclr(void *dest, SizeT n)
{
    return VG_(memset)(dest, 0, n);
}

void *__aeabi_memclr4(void *dest, SizeT n);
void *__aeabi_memclr4(void *dest, SizeT n)
{
    return VG_(memset)(dest, 0, n);
}

void *__aeabi_memclr8(void *dest, SizeT n);
void *__aeabi_memclr8(void *dest, SizeT n)
{
    return VG_(memset)(dest, 0, n);
}
#endif /* clang and android, basically */

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
    "\taddl  $"VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)", %eax\n"
    /* allocate at least 16 bytes on the new stack, and aligned */
    "\tsubl  $16, %eax\n"
    "\tandl  $~15, %eax\n"
    /* install it, and collect the original one */
    "\txchgl %eax, %esp\n"
    /* call _start_in_C_linux, passing it the startup %esp */
    "\tmovl  %eax, (%esp)\n"
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
    "\taddq  $"VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)", %rdi\n"
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
    "\tlis    18,("VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)" >> 16)\n"
    "\tori 18,18,("VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)" & 0xFFFF)\n"
    "\tadd 16,17,16\n"
    "\tadd 16,18,16\n"
    "\trlwinm 16,16,0,0,27\n"
    /* now r16 = &vgPlain_interim_stack + VG_STACK_GUARD_SZB +
       VG_DEFAULT_STACK_ACTIVE_SZB rounded down to the nearest 16-byte
       boundary.  And r1 is the original SP.  Set the SP to r16 and
       call _start_in_C_linux, passing it the initial SP. */
    "\tmr 3,1\n"
    "\tmr 1,16\n"
    "\tbl _start_in_C_linux\n"
    "\ttrap\n"
    ".previous\n"
);
#elif defined(VGP_ppc64be_linux)
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
    "\tlis    18,("VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)" >> 16)\n"
    "\tori 18,18,("VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)" & 0xFFFF)\n"
    "\tadd 16,17,16\n"
    "\tadd 16,18,16\n"
    "\trldicr 16,16,0,59\n"
    /* now r16 = &vgPlain_interim_stack + VG_STACK_GUARD_SZB +
       VG_DEFAULT_STACK_ACTIVE_SZB rounded down to the nearest 16-byte
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
#elif defined(VGP_ppc64le_linux)
/* Little Endian uses ELF version 2 but in the future may also
 * support other ELF versions.
 */
asm("\n"
    "\t.align 2\n"
    "\t.global _start\n"
    "\t.type _start,@function\n"
    "_start:\n"
    "#if _CALL_ELF == 2    \n"
    "0:  addis        2,12,.TOC.-0b@ha\n"
    "    addi         2,2,.TOC.-0b@l\n"
    "    .localentry  _start, .-_start\n"
    "#endif \n"
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
    "\tlis    18,("VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)" >> 16)\n"
    "\tori 18,18,("VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)" & 0xFFFF)\n"
    "\tadd 16,17,16\n"
    "\tadd 16,18,16\n"
    "\trldicr 16,16,0,59\n"
    /* now r16 = &vgPlain_interim_stack + VG_STACK_GUARD_SZB +
       VG_DEFAULT_STACK_ACTIVE_SZB rounded down to the nearest 16-byte
       boundary.  And r1 is the original SP.  Set the SP to r16 and
       call _start_in_C_linux, passing it the initial SP. */
    "\tmr 3,1\n"
    "\tmr 1,16\n"
    "\tlis  14,   _start_in_C_linux@highest\n"
    "\tori  14,14,_start_in_C_linux@higher\n"
    "\tsldi 14,14,32\n"
    "\toris 14,14,_start_in_C_linux@h\n"
    "\tori  14,14,_start_in_C_linux@l\n"
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
    "2:   .quad "VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)"\n\t"
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
    "\t.word "VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)"\n"
);
#elif defined(VGP_arm64_linux)
asm("\n"
    "\t.text\n"
    "\t.align 2\n"
    "\t.type _start,#function\n"
    "\t.global _start\n"
    "_start:\n"
    "\tadrp x0, vgPlain_interim_stack\n"
    "\tadd  x0, x0, :lo12:vgPlain_interim_stack\n"
    // The next 2 assume that VG_STACK_GUARD_SZB fits in 32 bits
    "\tmov  x1, (("VG_STRINGIFY(VG_STACK_GUARD_SZB)") >> 0) & 0xFFFF\n"
    "\tmovk x1, (("VG_STRINGIFY(VG_STACK_GUARD_SZB)") >> 16) & 0xFFFF,"
                " lsl 16\n"
    "\tadd  x0, x0, x1\n"
    // The next 2 assume that VG_DEFAULT_STACK_ACTIVE_SZB fits in 32 bits
    "\tmov  x1, (("VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)") >> 0) & 0xFFFF\n"
    "\tmovk x1, (("VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)") >> 16) & 0xFFFF,"
                " lsl 16\n"
    "\tadd  x0, x0, x1\n"
    "\tand  x0, x0, -16\n"
    "\tmov  x1, sp\n"
    "\tmov  sp, x0\n"
    "\tmov  x0, x1\n"
    "\tb _start_in_C_linux\n"
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
    "\tli    $11, "VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)"\n"

    "\taddu     $9, $9, $10\n"
    "\taddu     $9, $9, $11\n"
    "\tli       $12, 0xFFFFFFF0\n"
    "\tand      $9, $9, $12\n"
    /* now t1/$9 = &vgPlain_interim_stack + VG_STACK_GUARD_SZB +
       VG_DEFAULT_STACK_ACTIVE_SZB rounded down to the nearest 16-byte
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
#elif defined(VGP_mips64_linux)
asm(
".text\n"
".globl __start\n"
".type __start,@function\n"
"__start:\n"
    "\t.set noreorder\n"
    "\t.cpload $25\n"
    "\t.set reorder\n"
    "\t.cprestore 16\n"
    "\tlui    $9, %hi(vgPlain_interim_stack)\n"
    /* t1/$9 <- Addr(interim_stack) */
    "\tdaddiu $9, %lo(vgPlain_interim_stack)\n"

    "\tli     $10, "VG_STRINGIFY(VG_STACK_GUARD_SZB)"\n"
    "\tli     $11, "VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)"\n"

    "\tdaddu  $9, $9, $10\n"
    "\tdaddu  $9, $9, $11\n"
    "\tli     $12, 0xFFFFFF00\n"
    "\tand    $9, $9, $12\n"
    /* now t1/$9 = &vgPlain_interim_stack + VG_STACK_GUARD_SZB +
       VG_DEFAULT_STACK_ACTIVE_SZB rounded down to the nearest 16-byte
       boundary.  And $29 is the original SP.  Set the SP to t1 and
       call _start_in_C, passing it the initial SP. */

    "\tmove   $4, $29\n"     // a0 <- $sp (_start_in_C first arg)
    "\tmove   $29, $9\n"     // $sp <- t1 (new sp)

    "\tlui    $9, %highest(_start_in_C_linux)\n"
    "\tori    $9, %higher(_start_in_C_linux)\n"
    "\tdsll32 $9, $9, 0x0\n"
    "\tlui    $10, %hi(_start_in_C_linux)\n"
    "\tdaddiu $10, %lo(_start_in_C_linux)\n"
    "\tdaddu  $25, $9, $10\n"
    "\tjalr   $25\n"
    "\tnop\n"
".previous\n"
);
#elif defined(VGP_nanomips_linux)
   asm(
".text                                                  \n\t"
".globl __start                                         \n\t"
".type __start,@function                                \n\t"
"__start:                                               \n\t"
    ".set push                                          \n\t"
    ".set noreorder                                     \n\t"
    "li $t1, vgPlain_interim_stack                      \n\t"
    "li $t0, "VG_STRINGIFY(VG_STACK_GUARD_SZB)"         \n\t"
    "addu $t1, $t1, $t0                                 \n\t"
    "li $t0, "VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)"\n\t"
    "addu $t1, $t1, $t0                                 \n\t"
    "li $t0, 0xFFFFFF00                                 \n\t"
    "and $t1, $t1, $t0                                  \n\t"
    "move $a0, $sp                                      \n\t"
    "move $sp, $t1                                      \n\t"
    "li $t0, _start_in_C_linux                          \n\t"
    "jrc $t0                                            \n\t"
    "break                                              \n\t"
    ".set pop                                           \n\t"
".previous                                              \n\t"
);
#elif defined(VGP_riscv64_linux)
asm("\n"
    "\t.text\n"
    "\t.type _start,@function\n"
    "\t.global _start\n"
    "_start:\n"
    /* establish the global pointer in gp */
    ".option push\n"
    ".option norelax\n"
    "\tla gp, __global_pointer$\n"
    ".option pop\n"
    /* set up the new stack in t0 */
    "\tla t0, vgPlain_interim_stack\n"
    "\tli t1, "VG_STRINGIFY(VG_STACK_GUARD_SZB)"\n"
    "\tadd t0, t0, t1\n"
    "\tli t1, "VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)"\n"
    "\tadd t0, t0, t1\n"
    "\tli t1, 0xFFFFFF00\n"
    "\tand t0, t0, t1\n"
    /* install it, and collect the original one */
    "\tmv a0, sp\n"
    "\tmv sp, t0\n"
    /* call _start_in_C_linux, passing it the startup sp */
    "\tj _start_in_C_linux\n"
    "\tunimp\n"
    ".previous\n"
);
#else
#  error "Unknown platform"
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

   VG_(memset)( &the_iicii, 0, sizeof(the_iicii) );
   VG_(memset)( &the_iifii, 0, sizeof(the_iifii) );

   the_iicii.sp_at_startup = (Addr)pArgc;

#  if defined(VGP_ppc32_linux) || defined(VGP_ppc64be_linux) \
      || defined(VGP_ppc64le_linux) || defined(VGP_arm64_linux) \
      || defined(VGP_mips32_linux)  || defined(VGP_mips64_linux) \
      || defined(VGP_nanomips_linux)
   {
      /* ppc32/ppc64, arm64, mips32/64 can be configured with different
         page sizes. Determine this early. This is an ugly hack and really
         should be moved into valgrind_main. */
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
    "\taddl  $"VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)", %eax\n"
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
    "\taddq    $"VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)", %rdi\n"
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
void __bzero(void* s, UWord n);
void __bzero(void* s, UWord n) {
    (void)VG_(memset)(s,0,n);
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

   // See _start_in_C_linux
   INNER_REQUEST
      ((void) VALGRIND_STACK_REGISTER
       (&VG_(interim_stack).bytes[0],
        &VG_(interim_stack).bytes[0] + sizeof(VG_(interim_stack))));

   VG_(memset)( &the_iicii, 0, sizeof(the_iicii) );
   VG_(memset)( &the_iifii, 0, sizeof(the_iifii) );

   the_iicii.sp_at_startup = (Addr)pArgc;

   r = valgrind_main( (Int)argc, argv, envp );
   /* NOTREACHED */
   VG_(exit)(r);
}

/*====================================================================*/
/*=== Getting to main() alive: Solaris                             ===*/
/*====================================================================*/
#elif defined(VGO_solaris)
#if defined(VGP_x86_solaris)
/* The kernel hands control to _start, which extracts the initial stack
   pointer and calls onwards to _start_in_C_solaris.  This also switches to
   the new stack. */
asm("\n"
    "\t.text\n"
    "\t.globl _start\n"
    "\t.type _start, @function\n"
    "_start:\n"
    /* Set up the new stack in %eax. */
    "\tmovl  $vgPlain_interim_stack, %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)", %eax\n"
    "\tandl  $~15, %eax\n"
    /* Install it, and collect the original one. */
    "\txchgl %eax, %esp\n"
    "\tsubl  $12, %esp\n"  /* Keep stack 16-byte aligned. */
    /* Call _start_in_C_solaris, passing it the startup %esp. */
    "\tpushl %eax\n"
    "\tcall  _start_in_C_solaris\n"
    /* NOTREACHED */
    "\thlt\n"
    "\t.previous\n"
);
#elif defined(VGP_amd64_solaris)
asm("\n"
    ".text\n"
    "\t.globl _start\n"
    "\t.type _start, @function\n"
    "_start:\n"
    /* Set up the new stack in %rdi. */
    "\tmovq  $vgPlain_interim_stack, %rdi\n"
    "\taddq  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %rdi\n"
    "\taddq  $"VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)", %rdi\n"
    "\tandq  $~15, %rdi\n"
    /* Install it, and collect the original one. */
    "\txchgq %rdi, %rsp\n"
    /* Call _start_in_C_solaris, passing it the startup %rsp. */
    "\tcall  _start_in_C_solaris\n"
    /* NOTREACHED */
    "\thlt\n"
    ".previous\n"
);
#else
#  error "Unknown Solaris platform"
#endif

void *memcpy(void *dest, const void *src, size_t n);
void *memcpy(void *dest, const void *src, size_t n) {
   return VG_(memcpy)(dest, src, n);
}

__attribute__ ((used))
void _start_in_C_solaris ( UWord* pArgc );
__attribute__ ((used))
void _start_in_C_solaris ( UWord* pArgc )
{
   Int     r;
   Word    argc = pArgc[0];
   HChar** argv = (HChar**)&pArgc[1];
   HChar** envp = (HChar**)&pArgc[1 + argc + 1];

   VG_(memset)( &the_iicii, 0, sizeof(the_iicii) );
   VG_(memset)( &the_iifii, 0, sizeof(the_iifii) );

   the_iicii.sp_at_startup = (Addr)pArgc;

   r = valgrind_main((Int)argc, argv, envp);
   /* NOTREACHED */
   VG_(exit)(r);
}

/*====================================================================*/
/*=== Getting to main() alive: FreeBSD                             ===*/
/*====================================================================*/
#elif defined(VGO_freebsd)

/*
 * Could probably extract __FreeBSD_version at configure time
 */
/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <sys/param.h>       /* __FreeBSD_version */
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

/*
 * We need to add two elf notes in order for image activator to parse
 * additional binary properites.
 * First note declares the ABI, second is the feature note.
 * This is primarly used to turn off W^X policy for all valgrind tools,
 * as they don't work with it enabled.
 */

/* Based on FreeBSD sources: lib/csu/common/crtbrand.S */
asm("\n"
    ".section .note.tag,\"aG\",%note,.freebsd.noteG,comdat\n"
    ".p2align        2\n"
    ".4byte          2f-1f\n"
    ".4byte          4f-3f\n"
    ".4byte          "VG_STRINGIFY(VKI_NT_FREEBSD_ABI_TAG)"\n"
"1:  .asciz          \"FreeBSD\"\n"
"2:  .p2align        2\n"
"3:  .4byte          "VG_STRINGIFY(__FreeBSD_version)"\n"
"4:  .previous\n"
);

/* Based on FreeBSD sources: lib/csu/common/feature_note.S */
asm("\n"
    ".section .note.tag,\"a\",%note\n"
    ".p2align        2\n"
    ".4byte          2f-1f\n"
    ".4byte          4f-3f\n"
    ".4byte          "VG_STRINGIFY(VKI_NT_FREEBSD_FEATURE_CTL)"\n"
"1:  .asciz          \"FreeBSD\"\n"
"2:  .p2align        2\n"
"3:  .4byte          "VG_STRINGIFY(VKI_NT_FREEBSD_FCTL_WXNEEDED)"\n"
"4:  .previous\n"
);

#if defined(VGP_x86_freebsd)
asm("\n"
    ".text\n"
    "\t.globl _start\n"
    "\t.type _start,@function\n"
    "_start:\n"
    /* set up the new stack in %eax */
    "\tmovl  $vgPlain_interim_stack, %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %eax\n"
    "\taddl  $"VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)", %eax\n"
    /* allocate at least 16 bytes on the new stack, and aligned */
    "\tsubl  $16, %eax\n"
    "\tandl  $~15, %eax\n"
    /* install it, and collect the original one */
    "\txchgl %eax, %esp\n"
    "\tsubl  $12, %esp\n"  /* Keep stack 16-byte aligned. */
    /* call _start_in_C_freebsd, passing it the startup %esp */
    "\tpushl %eax\n"
    "\tcall  _start_in_C_freebsd\n"
    "\thlt\n"
    ".previous\n"
);
#elif defined(VGP_amd64_freebsd)

// @todo PJF I don't really understand why this is done this way
// other amd64 platforms just put the new stack address in rdi
// then do an exchange so that the stack pointer points to the
// new stack and rdi (which is the 1st argument in the amd64 sysv abi)
// contains the old stack

// instead for amd64 the same thing is done for rsi, the second
// function argument and rdi is unchanged
//
// In gdb I see the initial rdp is 8+rsp
// e.g.
// rdi            0x7fffffffe3b0
// rsp            0x7fffffffe3a8
//
// Maybe on FreeBSD the pointer to argc is 16byte aligned and can be 8 bytes above the
// start of the stack?
//
// Some answers to this mystery here
// https://forums.freebsd.org/threads/stack-alignment-argc-location-in-assembled-binaries.89302/#post-613119
// and here
// https://github.com/freebsd/freebsd-src/blob/releng/5.1/sys/amd64/amd64/machdep.c#LL487C1-L488C42

asm("\n"
    ".text\n"
    "\t.globl _start\n"
    "\t.type _start,@function\n"
    "_start:\n"
    /* set up the new stack in %rsi */
    "\tmovq  $vgPlain_interim_stack, %rsi\n"
    "\taddq  $"VG_STRINGIFY(VG_STACK_GUARD_SZB)", %rsi\n"
    "\taddq  $"VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)", %rsi\n"
    "\tandq  $~15, %rsi\n"
    /* install it, and collect the original one */
    "\txchgq %rsi, %rsp\n"
    /* call _start_in_C_freebsd, passing it the startup %rsp */
    "\tcall  _start_in_C_freebsd\n"
    "\thlt\n"
    ".previous\n"
);

#elif defined(VGP_arm64_freebsd)


// on entry
// x0 contains a pointer to argc
// sp contains a pointer either to the same address
//    or 8 below it depending on whether the stack pointer
//    was 16byte aligned
//
// before calling we want
// x0 to contain a pointer to argc - just leave it alone
// x1 to contain a pointer to the original stack in case we need it like amd64
// sp to contain a pointer to the end of VG_(interim_stack)
asm("\n"
    ".text\n"
    "\t.align 2\n"
    "\t.type _start,#function\n"
    "\t.global _start\n"
    "_start:\n"
    "\tadrp x2, vgPlain_interim_stack\n"
    "\tadd  x2, x2, :lo12:vgPlain_interim_stack\n"
    "\tldr  x3, ="VG_STRINGIFY(VG_STACK_GUARD_SZB)"\n"
    "\tadd  x2, x2, x3\n"
    "\tldr  x3, ="VG_STRINGIFY(VG_DEFAULT_STACK_ACTIVE_SZB)"\n"
    "\tadd  x2, x2, x3\n"
    "\tand  x2, x2, -16\n"
    "\tmov  x1, sp\n"
    "\tmov  sp, x2\n"
    "\tb _start_in_C_freebsd\n"
);
#endif

void *memcpy(void *dest, const void *src, size_t n);
void *memcpy(void *dest, const void *src, size_t n) {
   return VG_(memcpy)(dest, src, n);
}
void* memmove(void *dest, const void *src, SizeT n);
void* memmove(void *dest, const void *src, SizeT n) {
   return VG_(memmove)(dest,src,n);
}
void* memset(void *s, int c, SizeT n);
void* memset(void *s, int c, SizeT n) {
  return VG_(memset)(s,c,n);
}

__attribute__ ((used))
void _start_in_C_freebsd ( UWord* pArgc, UWord *initial_sp );
__attribute__ ((used))
void _start_in_C_freebsd ( UWord* pArgc, UWord *initial_sp )
{
   Int     r;
   Word    argc = pArgc[0];
   HChar** argv = (HChar**)&pArgc[1];
   HChar** envp = (HChar**)&pArgc[1+argc+1];

   INNER_REQUEST
      ((void) VALGRIND_STACK_REGISTER
       (&VG_(interim_stack).bytes[0],
        &VG_(interim_stack).bytes[0] + sizeof(VG_(interim_stack))));

   VG_(memset)( &the_iicii, 0, sizeof(the_iicii) );
   VG_(memset)( &the_iifii, 0, sizeof(the_iifii) );

#if defined(VGP_amd64_freebsd) || defined(VGP_arm64_freebsd)
   the_iicii.sp_at_startup = (Addr)initial_sp;
#else
   the_iicii.sp_at_startup = (Addr)pArgc;
#endif

   r = valgrind_main( (Int)argc, argv, envp );
   /* NOTREACHED */
   VG_(exit)(r);
}

#else
#  error "Unknown OS"
#endif

SizeT VG_(get_client_stack_max_size)(void)
{
   return the_iifii.clstack_max_size;
}

Addr VG_(get_initial_client_SP)( void )
{
   return the_iifii.initial_client_SP;
}

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


/*====================================================================*/
/*=== Dummy _voucher_mach_msg_set for OSX 10.10                    ===*/
/*====================================================================*/

#if defined(VGO_darwin) && DARWIN_VERS >= DARWIN_10_10

/* Builds on MacOSX 10.10+ seem to need this for some reason. */
/* extern boolean_t voucher_mach_msg_set(mach_msg_header_t *msg)
                    __attribute__((weak_import));
   I haven't a clue what the return value means, so just return 0.
   Looks like none of the generated uses in the tree look at the
   return value anyway.
*/
UWord voucher_mach_msg_set ( UWord arg1 );
UWord voucher_mach_msg_set ( UWord arg1 )
{
   return 0;
}

#endif

#if defined(VGO_freebsd)
Word VG_(get_usrstack)(void)
{
   return VG_PGROUNDDN(the_iicii.clstack_end) + VKI_PAGE_SIZE;
}
#endif



/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
