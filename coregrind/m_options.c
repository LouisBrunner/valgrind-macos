
/*--------------------------------------------------------------------*/
/*--- Command line options.                            m_options.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Nicholas Nethercote
      njn@valgrind.org

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
#include "pub_core_vki.h"
#include "pub_core_options.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_mallocfree.h"
#include "pub_core_seqmatch.h"     // VG_(string_match)
#include "pub_core_aspacemgr.h"

// See pub_{core,tool}_options.h for explanations of all these.

static Clo_Mode clo_mode = cloE;
static Bool clo_recognised = False;

void VG_(set_Clo_Mode) (Clo_Mode mode)
{
   clo_mode = mode;
   clo_recognised = False;
}
Clo_Mode VG_(Clo_Mode) (void)
{
   return clo_mode;
}

void VG_(set_Clo_Recognised) (void)
{
   clo_recognised = True;
}

Bool VG_(Clo_Recognised) (void)
{
   return clo_recognised;
}

Bool VG_(check_clom) (Clo_Mode modes, const HChar* arg, const HChar* option,
                      Bool recognised)
{
   Bool res = recognised && (modes & VG_(Clo_Mode)());
   Bool dynamic = cloD == VG_(Clo_Mode)();

   if (recognised) {
      VG_(set_Clo_Recognised) ();
      if (dynamic && !res)
         VG_(umsg)("Cannot change %s option dynamically\n", option);
      else if (dynamic && VG_(clo_verbosity) >= 1)
         VG_(umsg)("Handling new value %s for option %s\n", arg, option);
   }
   if (cloH == VG_(Clo_Mode)() && (cloD & modes))
      VG_(list_clo)(option);

   return res;
}

/* Define, and set defaults. */

const HChar *VG_(clo_toolname) = "memcheck";    // default to Memcheck
VexControl VG_(clo_vex_control);
VexRegisterUpdates VG_(clo_px_file_backed) = VexRegUpd_INVALID;

Bool   VG_(clo_error_limit)    = True;
Int    VG_(clo_error_exitcode) = 0;
HChar *VG_(clo_error_markers)[2] = {NULL, NULL};
Bool   VG_(clo_exit_on_first_error) = False;

Bool   VG_(clo_show_error_list) = False;

#if defined(VGPV_arm_linux_android) \
    || defined(VGPV_x86_linux_android) \
    || defined(VGPV_mips32_linux_android) \
    || defined(VGPV_arm64_linux_android) \
    || defined(VGP_nanomips_linux)
VgVgdb VG_(clo_vgdb)           = Vg_VgdbNo; // currently disabled on Android
#else
VgVgdb VG_(clo_vgdb)           = Vg_VgdbYes;
#endif
Int    VG_(clo_vgdb_poll)      = 5000; 
Int    VG_(clo_vgdb_error)     = 999999999;
Bool   VG_(clo_launched_with_multi)  = False;
UInt   VG_(clo_vgdb_stop_at)   = 0;
const HChar *VG_(clo_vgdb_prefix)    = NULL;
const HChar *VG_(arg_vgdb_prefix)    = NULL;
Bool   VG_(clo_vgdb_shadow_registers) = False;

Int    VG_(clo_gen_suppressions) = 0;
Int    VG_(clo_sanity_level)   = 1;
Int    VG_(clo_verbosity)      = 1;
Bool   VG_(clo_stats)          = False;
Bool   VG_(clo_xml)            = False;
const HChar* VG_(clo_xml_user_comment) = NULL;
Bool   VG_(clo_demangle)       = True;
const HChar* VG_(clo_soname_synonyms)    = NULL;
Bool   VG_(clo_trace_children) = False;
const HChar* VG_(clo_trace_children_skip) = NULL;
const HChar* VG_(clo_trace_children_skip_by_arg) = NULL;
Bool   VG_(clo_child_silent_after_fork) = False;
const HChar *VG_(clo_log_fname_unexpanded) = NULL;
const HChar *VG_(clo_xml_fname_unexpanded) = NULL;
Bool   VG_(clo_time_stamp)     = False;
Int    VG_(clo_input_fd)       = 0; /* stdin */
Bool   VG_(clo_default_supp)   = True;
XArray *VG_(clo_suppressions);   // array of strings
XArray *VG_(clo_fullpath_after); // array of strings
const HChar* VG_(clo_extra_debuginfo_path) = NULL;
const HChar* VG_(clo_debuginfo_server) = NULL;
Bool   VG_(clo_allow_mismatched_debuginfo) = False;
UChar  VG_(clo_trace_flags)    = 0; // 00000000b
Bool   VG_(clo_profyle_sbs)    = False;
UChar  VG_(clo_profyle_flags)  = 0; // 00000000b
ULong  VG_(clo_profyle_interval) = 0;
Int    VG_(clo_trace_notbelow) = -1;  // unspecified
Int    VG_(clo_trace_notabove) = -1;  // unspecified
Bool   VG_(clo_trace_syscalls) = False;
Bool   VG_(clo_trace_signals)  = False;
Bool   VG_(clo_trace_symtab)   = False;
const HChar* VG_(clo_trace_symtab_patt) = "*";
Bool   VG_(clo_trace_cfi)      = False;
Bool   VG_(clo_debug_dump_syms) = False;
Bool   VG_(clo_debug_dump_line) = False;
Bool   VG_(clo_debug_dump_frames) = False;
Bool   VG_(clo_trace_redir)    = False;
enum FairSchedType
       VG_(clo_fair_sched)     = disable_fair_sched;
/* VG_(clo_scheduling_quantum) defines the thread-scheduling timeslice,
   in terms of the number of basic blocks we attempt to run each thread for.
   Smaller values give finer interleaving but much increased scheduling
   overheads. */
Word   VG_(clo_scheduling_quantum) = 100000;
Bool   VG_(clo_trace_sched)    = False;
Bool   VG_(clo_profile_heap)   = False;
UInt   VG_(clo_progress_interval) = 0; /* in seconds, 1 .. 3600,
                                          or 0 == disabled */
Int    VG_(clo_core_redzone_size) = CORE_REDZONE_DEFAULT_SZB;
// A value != -1 overrides the tool-specific value
// VG_(needs_malloc_replacement).tool_client_redzone_szB
Int    VG_(clo_redzone_size)   = -1;
VgXTMemory VG_(clo_xtree_memory) =  Vg_XTMemory_None;
const HChar* VG_(clo_xtree_memory_file) = "xtmemory.kcg.%p";
Bool VG_(clo_xtree_compress_strings) = True;

#if defined(VGO_linux)
Bool VG_(clo_enable_debuginfod) = True;
#endif
Int    VG_(clo_dump_error)     = 0;
Int    VG_(clo_backtrace_size) = 12;
Int    VG_(clo_merge_recursive_frames) = 0; // default value: no merge
UInt   VG_(clo_sim_hints)      = 0;
Bool   VG_(clo_sym_offsets)    = False;
Bool   VG_(clo_read_inline_info) = False; // Or should be put it to True by default ???
Bool   VG_(clo_read_var_info)  = False;
XArray *VG_(clo_req_tsyms);  // array of strings
Bool   VG_(clo_run_libc_freeres) = True;
Bool   VG_(clo_run_cxx_freeres) = True;
UInt   VG_(clo_track_fds)      = 0;
Bool   VG_(clo_show_below_main)= False;
Bool   VG_(clo_keep_debuginfo) = False;
Bool   VG_(clo_show_emwarns)   = False;
Word   VG_(clo_max_stackframe) = 2000000;
UInt   VG_(clo_max_threads)    = MAX_THREADS_DEFAULT;
Word   VG_(clo_main_stacksize) = 0; /* use client's rlimit.stack */
Word   VG_(clo_valgrind_stacksize) = VG_DEFAULT_STACK_ACTIVE_SZB;
Bool   VG_(clo_wait_for_gdb)   = False;
UInt   VG_(clo_kernel_variant) = 0;
Bool   VG_(clo_dsymutil)       = True;
Bool   VG_(clo_sigill_diag)    = True;
UInt   VG_(clo_unw_stack_scan_thresh) = 0; /* disabled by default */
UInt   VG_(clo_unw_stack_scan_frames) = 5;

// Set clo_smc_check so that it provides transparent self modifying
// code support for "correct" programs at the smallest achievable
// expense for this arch.
#if defined(VGA_x86) || defined(VGA_amd64) || defined(VGA_s390x)
VgSmc VG_(clo_smc_check) = Vg_SmcAllNonFile;
#elif defined(VGA_ppc32) || defined(VGA_ppc64be) || defined(VGA_ppc64le) \
      || defined(VGA_arm) || defined(VGA_arm64) \
      || defined(VGA_mips32) || defined(VGA_mips64) || defined(VGA_nanomips)
VgSmc VG_(clo_smc_check) = Vg_SmcStack;
#else
#  error "Unknown arch"
#endif

#if defined(VGO_darwin)
UInt VG_(clo_resync_filter) = 1; /* enabled, but quiet */
#else
UInt VG_(clo_resync_filter) = 0; /* disabled */
#endif


/*====================================================================*/
/*=== File expansion                                               ===*/
/*====================================================================*/

// Copies the string, prepending it with the startup working directory, and
// expanding %p and %q entries.  Returns a new, malloc'd string.
HChar* VG_(expand_file_name)(const HChar* option_name, const HChar* format)
{
   const HChar *base_dir;
   Int len, i = 0, j = 0;
   HChar* out;
   const HChar *message = NULL;

   base_dir = VG_(get_startup_wd)();

   if (VG_STREQ(format, "")) {
      // Empty name, bad.
      message = "No filename given\n";
      goto bad;
   }
   
   // If 'format' starts with a '~', abort -- the user probably expected the
   // shell to expand but it didn't (see bug 195268 for details).  This means
   // that we don't allow a legitimate filename beginning with '~' but that
   // seems very unlikely.
   if (format[0] == '~') {
      message = 
         "Filename begins with '~'\n"
         "You probably expected the shell to expand the '~', but it\n"
         "didn't.  The rules for '~'-expansion vary from shell to shell.\n"
         "You might have more luck using $HOME instead.\n";
      goto bad;
   }

   len = VG_(strlen)(format) + 1;
   out = VG_(malloc)( "options.efn.1", len );

#define ENSURE_THIS_MUCH_SPACE(x) \
   if (j + x >= len) { \
      len += (10 + x); \
      out = VG_(realloc)("options.efn.2(multiple)", out, len); \
   }

   while (format[i]) {
      if (format[i] != '%') {
         ENSURE_THIS_MUCH_SPACE(1);
         out[j++] = format[i++];
         
      } else {
         // We saw a '%'.  What's next...
         i++;
         if      ('%' == format[i]) {
            // Replace '%%' with '%'.
            ENSURE_THIS_MUCH_SPACE(1);
            out[j++] = format[i++];
         }
         else if ('p' == format[i]) {
            // Print the PID.  Assume that it's not longer than 10 chars --
            // reasonable since 'pid' is an Int (ie. 32 bits).
            Int pid = VG_(getpid)();
            ENSURE_THIS_MUCH_SPACE(10);
            j += VG_(sprintf)(&out[j], "%d", pid);
            i++;
         } 
         else if ('n' == format[i]) {
            // Print a seq nr.
            static Int last_pid;
            static Int seq_nr;
            Int pid = VG_(getpid)();
            if (last_pid != pid)
               seq_nr = 0;
            last_pid = pid;
            seq_nr++;
            ENSURE_THIS_MUCH_SPACE(10);
            j += VG_(sprintf)(&out[j], "%d", seq_nr);
            i++;
         } 
         else if ('q' == format[i]) {
            i++;
            if ('{' == format[i]) {
               // Get the env var name, print its contents.
               HChar *qual;
               Int begin_qualname = ++i;
               while (True) {
                  if (0 == format[i]) {
                     message = "Missing '}' in %q specifier\n";
                     goto bad;
                  } else if ('}' == format[i]) {
                     Int qualname_len = i - begin_qualname;
                     HChar qualname[qualname_len + 1];
                     VG_(strncpy)(qualname, format + begin_qualname,
                                  qualname_len);
                     qualname[qualname_len] = '\0';
                     qual = VG_(getenv)(qualname);
                     if (NULL == qual) {
                        // This memory will leak, But we don't care because
                        // VG_(fmsg_bad_option) will terminate the process.
                        HChar *str = VG_(malloc)("options.efn.3",
                                                 100 + qualname_len);
                        VG_(sprintf)(str,
                                     "Environment variable '%s' is not set\n",
                                     qualname);
                        message = str;
                        goto bad;
                     }
                     i++;
                     break;
                  }
                  i++;
               }
               ENSURE_THIS_MUCH_SPACE(VG_(strlen)(qual));
               j += VG_(sprintf)(&out[j], "%s", qual);
            } else {
               message = "Expected '{' after '%q'\n";
               goto bad;
            }
         } 
         else {
            // Something else, abort.
            message = "Expected 'p' or 'q' or '%' after '%'\n";
            goto bad;
         }
      }
   }
   ENSURE_THIS_MUCH_SPACE(1);
   out[j++] = 0;

   // If 'out' is not an absolute path name, prefix it with the startup dir.
   if (out[0] != '/') {
      if (base_dir == NULL) {
         message = "Current working dir doesn't exist, use absolute path\n";
         goto bad;
      }
      len = VG_(strlen)(base_dir) + 1 + VG_(strlen)(out) + 1;

      HChar *absout = VG_(malloc)("options.efn.4", len);
      VG_(strcpy)(absout, base_dir);
      VG_(strcat)(absout, "/");
      VG_(strcat)(absout, out);
      VG_(free)(out);
      out = absout;
   }

   return out;

  bad: {
   vg_assert(message != NULL);
   // 2:  1 for the '=', 1 for the NUL.
   HChar opt[VG_(strlen)(option_name) + VG_(strlen)(format) + 2];
   VG_(sprintf)(opt, "%s=%s", option_name, format);
   VG_(fmsg_bad_option)(opt, "%s", message);
   VG_(exit)(1); // Cannot continue
   /*NOTREACHED*/
  }
}

static int col = 0;
void VG_(list_clo)(const HChar *qq_option)
{
   int len = VG_(strlen)(qq_option);
   if (col + len + 1 > 80) {
      VG_(printf)("\n");
      col = 0;
   }

   if (col == 0) {
      VG_(printf)("    ");
      col += 4;
   } else {
      VG_(printf)(" ");
      col += 1;
   }
   VG_(printf)("%s", qq_option);
   col += len;
}
void VG_(list_dynamic_options) (void)
{
   HChar dummy[40];

   VG_(sprintf)(dummy, "%s", "<dummy option to trigger help>");
   VG_(printf)("  dynamically changeable options:\n");
   VG_(process_dynamic_option) (cloH, dummy);
   if (col > 0) {
      VG_(printf)("\n");
      col = 0;
   }
}

/*====================================================================*/
/*=== --trace-children= support                                    ===*/
/*====================================================================*/

static HChar const* consume_commas ( HChar const* c ) {
   while (*c && *c == ',') {
      ++c;
   }
   return c;
}

static HChar const* consume_field ( HChar const* c ) {
   while (*c && *c != ',') {
      ++c;
   }
   return c;
}

/* Should we trace into this child executable (across execve, spawn etc) ?
   This involves considering --trace-children=,
   --trace-children-skip=, --trace-children-skip-by-arg=, and the name
   of the executable.  'child_argv' must not include the name of the
   executable itself; iow child_argv[0] must be the first arg, if any,
   for the child. */
Bool VG_(should_we_trace_this_child) ( const HChar* child_exe_name,
                                       const HChar** child_argv )
{
   // child_exe_name is pulled out of the guest's space.  We
   // should be at least marginally cautious with it, lest it
   // explode or burst into flames unexpectedly.
   if (child_exe_name == NULL || VG_(strlen)(child_exe_name) == 0)
      return VG_(clo_trace_children);  // we know narfink

   // If --trace-children=no, the answer is simply NO.
   if (! VG_(clo_trace_children))
      return False;

   // Otherwise, look for other reasons to say NO.  First,
   // see if the exe name matches any of the patterns specified
   // by --trace-children-skip=.
   if (VG_(clo_trace_children_skip)) {
      HChar const* last = VG_(clo_trace_children_skip);
      HChar const* name = child_exe_name;
      while (*last) {
         Bool   matches;
         HChar* patt;
         HChar const* first = consume_commas(last);
         last = consume_field(first);
         if (first == last)
            break;
         vg_assert(last > first);
         /* copy the candidate string into a temporary malloc'd block
            so we can use VG_(string_match) on it. */
         patt = VG_(calloc)("m_options.swttc.1", last - first + 1, 1);
         VG_(memcpy)(patt, first, last - first);
         vg_assert(patt[last-first] == 0);
         matches = VG_(string_match)(patt, name);
         VG_(free)(patt);
         if (matches)
            return False;
      }
   }

   // Check if any of the args match any of the patterns specified
   // by --trace-children-skip-by-arg=. 
   if (VG_(clo_trace_children_skip_by_arg) && child_argv != NULL) {
      HChar const* last = VG_(clo_trace_children_skip_by_arg);
      while (*last) {
         Int    i;
         Bool   matches;
         HChar* patt;
         HChar const* first = consume_commas(last);
         last = consume_field(first);
         if (first == last)
            break;
         vg_assert(last > first);
         /* copy the candidate string into a temporary malloc'd block
            so we can use VG_(string_match) on it. */
         patt = VG_(calloc)("m_options.swttc.1", last - first + 1, 1);
         VG_(memcpy)(patt, first, last - first);
         vg_assert(patt[last-first] == 0);
         for (i = 0; child_argv[i]; i++) {
            matches = VG_(string_match)(patt, child_argv[i]);
            if (matches) {
               VG_(free)(patt);
               return False;
            }
         }
         VG_(free)(patt);
      }
   }

   // --trace-children=yes, and this particular executable isn't
   // excluded
   return True;
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
