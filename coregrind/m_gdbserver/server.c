/* Main code for remote server for GDB.
   Copyright (C) 1989, 1993, 1994, 1995, 1997, 1998, 1999, 2000, 2002, 2003,
   2004, 2005, 2006, 2011
   Free Software Foundation, Inc.

   This file is part of GDB.
   It has been modified to integrate it in valgrind

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#include "server.h"
#include "regdef.h"
#include "pub_core_options.h"
#include "pub_core_translate.h"
#include "pub_core_mallocfree.h"
#include "pub_core_initimg.h"
#include "pub_core_execontext.h"
#include "pub_core_syswrap.h"      // VG_(show_open_fds)
#include "pub_core_scheduler.h"
#include "pub_core_transtab.h"
#include "pub_core_debuginfo.h"
#include "pub_core_addrinfo.h"
#include "pub_core_aspacemgr.h"

unsigned long cont_thread;
unsigned long general_thread;
unsigned long step_thread;
unsigned long thread_from_wait;
unsigned long old_thread_from_wait;

int pass_signals[TARGET_SIGNAL_LAST]; /* indexed by gdb signal nr */

/* for a gdbserver integrated in valgrind, resuming the process consists
   in returning the control to valgrind.
   The guess process resumes its execution.
   Then at the next error or break or ..., valgrind calls gdbserver again.
   A resume reply packet must then be built to inform GDB that the
   resume request is finished.
   resume_reply_packet_needed records the fact that the next call to gdbserver
   must send a resume packet to gdb. */
static Bool resume_reply_packet_needed = False;

VG_MINIMAL_JMP_BUF(toplevel);

/* Decode a qXfer read request.  Return 0 if everything looks OK,
   or -1 otherwise.  */

static
int decode_xfer_read (char *buf, const char **annex, CORE_ADDR *ofs, unsigned int *len)
{
   /* Extract and NUL-terminate the annex.  */
   *annex = buf;
   while (*buf && *buf != ':')
      buf++;
   if (*buf == '\0')
      return -1;
   *buf++ = 0;

   /* After the read/write marker and annex, qXfer looks like a
      traditional 'm' packet.  */
   decode_m_packet (buf, ofs, len);

   return 0;
}

/* Write the response to a successful qXfer read.  Returns the
   length of the (binary) data stored in BUF, corresponding
   to as much of DATA/LEN as we could fit.  IS_MORE controls
   the first character of the response.  */
static
int write_qxfer_response (char *buf, unsigned char *data, int len, int is_more)
{
   int out_len;

   if (is_more)
      buf[0] = 'm';
   else
      buf[0] = 'l';

   return remote_escape_output (data, len, (unsigned char *) buf + 1, &out_len,
                                PBUFSIZ - POVERHSIZ - 1) + 1;
}

static Bool initial_valgrind_sink_saved = False;
/* True <=> valgrind log sink saved in initial_valgrind_sink */
static OutputSink initial_valgrind_sink;

static Bool command_output_to_log = False;
/* True <=> command output goes to log instead of gdb */

void reset_valgrind_sink(const char *info)
{
   if (VG_(log_output_sink).fd != initial_valgrind_sink.fd
       && initial_valgrind_sink_saved) {
      VG_(log_output_sink).fd = initial_valgrind_sink.fd;
      VG_(umsg) ("Reset valgrind output to log (%s)\n",
                 (info == NULL ? "" : info));
   }
}

void print_to_initial_valgrind_sink (const char *msg)
{
   vg_assert (initial_valgrind_sink_saved);
   VG_(write) (initial_valgrind_sink.fd, msg, strlen(msg));
}


static
void kill_request (const char *msg)
{
   VG_(umsg) ("%s", msg);
   VG_(exit) (0);
}

// s is a NULL terminated string made of O or more words (separated by spaces).
// Returns a pointer to the Nth word in s.
// If Nth word does not exist, return a pointer to the last (0) byte of s.
static
const char *wordn (const char *s, int n)
{
   int word_seen = 0;
   Bool searching_word = True;

   while (*s) {
      if (*s == ' ')
         searching_word = True;
      else {
         if (searching_word) {
            searching_word = False;
            word_seen++;
            if (word_seen == n)
               return s;
         }
      }
      s++;
   }
   return s;
}

void VG_(print_all_stats) (Bool memory_stats, Bool tool_stats)
{
   if (memory_stats) {
      VG_(message)(Vg_DebugMsg, "\n");
      VG_(message)(Vg_DebugMsg,
         "------ Valgrind's internal memory use stats follow ------\n" );
      VG_(sanity_check_malloc_all)();
       VG_(message)
          (Vg_DebugMsg,
           "------ %'13llu bytes have already been mmap-ed ANONYMOUS.\n",
           VG_(am_get_anonsize_total)());
      VG_(print_all_arena_stats)();
      if (VG_(clo_profile_heap))
         VG_(print_arena_cc_analysis) ();
      VG_(message)(Vg_DebugMsg, "\n");
   }

   VG_(print_translation_stats)();
   VG_(print_tt_tc_stats)();
   VG_(print_scheduler_stats)();
   VG_(print_ExeContext_stats)( False /* with_stacktraces */ );
   VG_(print_errormgr_stats)();
   if (tool_stats && VG_(needs).print_stats) {
      VG_TDICT_CALL(tool_print_stats);
   }
}

/* handle_gdb_valgrind_command handles the provided mon string command.
   If command is recognised, return 1 else return 0.
   Note that in case of ambiguous command, 1 is returned.

   *sink_wanted_at_return is modified if one of the commands
   'v.set *_output' is handled.
*/
static
int handle_gdb_valgrind_command (char *mon, OutputSink *sink_wanted_at_return)
{
   UWord ret = 0;
   char s[strlen(mon)+1]; /* copy for strtok_r */
   char *wcmd;
   HChar *ssaveptr;
   const char *endptr;
   int   kwdid;
   int int_value;

   const DiEpoch cur_ep = VG_(current_DiEpoch)();

   vg_assert (initial_valgrind_sink_saved);

   strcpy (s, mon);
   wcmd = strtok_r (s, " ", &ssaveptr);
   /* NB: if possible, avoid introducing a new command below which
      starts with the same 3 first letters as an already existing
      command. This ensures a shorter abbreviation for the user. */
   switch (VG_(keyword_id) ("help v.set v.info v.wait v.kill v.translate"
                            " v.do v.clo",
                            wcmd, kwd_report_duplicated_matches)) {
   case -2:
      ret = 1;
      break;
   case -1:
      break;
   case  0: /* help */
      ret = 1;
      wcmd = strtok_r (NULL, " ", &ssaveptr);
      if (wcmd == NULL) {
         int_value = 0;
      } else {
         switch (VG_(keyword_id) ("debug", wcmd, kwd_report_all)) {
         case -2: int_value = 0; break;
         case -1: int_value = 0; break;
         case  0: int_value = 1; break;
         default: vg_assert (0);
         }
      }

      VG_(gdb_printf) (
"general valgrind monitor commands:\n"
"  help [debug]            : monitor command help. With debug: + debugging commands\n"
"  v.wait [<ms>]           : sleep <ms> (default 0) then continue\n"
"  v.info all_errors       : show all errors found so far\n"
"  v.info last_error       : show last error found\n"
"  v.info location <addr>  : show information about location <addr>\n"
"  v.info n_errs_found [msg] : show the nr of errors found so far and the given msg\n"
"  v.info open_fds         : show open file descriptors (only if --track-fds=[yes|all])\n"
"  v.kill                  : kill the Valgrind process\n"
"  v.clo <clo_option>...   : changes one or more dynamic command line options\n"
"     with no clo_option, show the dynamically changeable options.\n"
"  v.set gdb_output        : set valgrind output to gdb\n"
"  v.set log_output        : set valgrind output to log\n"
"  v.set mixed_output      : set valgrind output to log, interactive output to gdb\n"
"  v.set merge-recursive-frames <num> : merge recursive calls in max <num> frames\n"
"  v.set vgdb-error <errornr> : debug me at error >= <errornr> \n");
      if (int_value) { VG_(gdb_printf) (
"debugging valgrind internals monitor commands:\n"
"  v.do   expensive_sanity_check_general : do an expensive sanity check now\n"
"  v.info gdbserver_status : show gdbserver status\n"
"  v.info memory [aspacemgr] : show valgrind heap memory stats\n"
"     (with aspacemgr arg, also shows valgrind segments on log output)\n"
"  v.info exectxt          : show stacktraces and stats of all execontexts\n"
"  v.info scheduler        : show valgrind thread state and stacktrace\n"
"  v.info stats            : show various valgrind and tool stats\n"
"  v.info unwind <addr> [<len>] : show unwind debug info for <addr> .. <addr+len>\n"
"  v.set debuglog <level>  : set valgrind debug log level to <level>\n"
"  v.set hostvisibility [yes*|no] : (en/dis)ables access by gdb/gdbserver to\n"
"    Valgrind internal host status/memory\n"
"  v.translate <addr> [<traceflags>]  : debug translation of <addr> with <traceflags>\n"
"    (default traceflags 0b00100000 : show after instrumentation)\n"
"   An additional flag  0b100000000 allows one to show gdbserver instrumentation\n");
      }
      break;
   case  1: /* v.set */
      ret = 1;
      wcmd = strtok_r (NULL, " ", &ssaveptr);
      switch (kwdid = VG_(keyword_id)
              ("vgdb-error debuglog merge-recursive-frames"
               " gdb_output log_output mixed_output hostvisibility",
               wcmd, kwd_report_all)) {
      case -2:
      case -1:
         break;
      case 0: /* vgdb-error */
      case 1: /* debuglog */
      case 2: /* merge-recursive-frames */
         wcmd = strtok_r (NULL, " ", &ssaveptr);
         if (wcmd == NULL) {
            int_value = 0;
            endptr = "empty"; /* to report an error below */
         } else {
            HChar *the_end;
            int_value = strtol (wcmd, &the_end, 10);
            endptr = the_end;
         }
         if (*endptr != '\0') {
            VG_(gdb_printf) ("missing or malformed integer value\n");
         } else if (kwdid == 0) {
            VG_(printf) ("vgdb-error value changed from %d to %d\n",
                             VG_(clo_vgdb_error), int_value);
            VG_(clo_vgdb_error) = int_value;
         } else if (kwdid == 1) {
            VG_(printf) ("debuglog value changed from %d to %d\n",
                             VG_(debugLog_getLevel)(), int_value);
            VG_(debugLog_startup) (int_value, "gdbsrv");
         } else if (kwdid == 2) {
            VG_(printf)
               ("merge-recursive-frames value changed from %d to %d\n",
                VG_(clo_merge_recursive_frames), int_value);
            VG_(clo_merge_recursive_frames) = int_value;
         } else {
            vg_assert (0);
         }
         break;
      case 3: /* gdb_output */
         (*sink_wanted_at_return).fd = -2;
         command_output_to_log = False;
         VG_(gdb_printf) ("valgrind output will go to gdb\n");
         break;
      case 4: /* log_output */
         (*sink_wanted_at_return).fd = initial_valgrind_sink.fd;
         command_output_to_log = True;
         VG_(gdb_printf) ("valgrind output will go to log\n");
         break;
      case 5: /* mixed output */
         (*sink_wanted_at_return).fd = initial_valgrind_sink.fd;
         command_output_to_log = False;
         VG_(gdb_printf)
            ("valgrind output will go to log, "
             "interactive output will go to gdb\n");
         break;
      case 6: /* hostvisibility */
         wcmd = strtok_r (NULL, " ", &ssaveptr);
         if (wcmd != NULL) {
            switch (VG_(keyword_id) ("yes no", wcmd, kwd_report_all)) {
            case -2:
            case -1: break;
            case  0:
               hostvisibility = True;
               break;
            case 1:
               hostvisibility = False;
               break;
            default: vg_assert (0);
            }
         } else {
            hostvisibility = True;
         }
         if (hostvisibility) {
            const DebugInfo *tooldi
               = VG_(find_DebugInfo) (cur_ep, (Addr)handle_gdb_valgrind_command);
            /* Normally, we should always find the tooldi. In case we
               do not, suggest a 'likely somewhat working' address: */
            const Addr tool_text_start
               = tooldi ?
               VG_(DebugInfo_get_text_avma) (tooldi) : 0x58000000;
            const NSegment *toolseg
               = tooldi ?
                 VG_(am_find_nsegment) (VG_(DebugInfo_get_text_avma) (tooldi))
                 : NULL;
            VG_(gdb_printf)
               ("Enabled access to Valgrind memory/status by GDB\n"
                "If not yet done, tell GDB which valgrind file(s) to use, "
                "typically:\n"
                "add-symbol-file %s %p\n",
                toolseg ? VG_(am_get_filename)(toolseg)
                : "<toolfile> <address> e.g.",
                (void*)tool_text_start);
         } else
            VG_(gdb_printf)
               ("Disabled access to Valgrind memory/status by GDB\n");
         break;
      default:
         vg_assert (0);
      }
      break;
   case  2: /* v.info */ {
      ret = 1;
      wcmd = strtok_r (NULL, " ", &ssaveptr);
      switch (kwdid = VG_(keyword_id)
              ("all_errors n_errs_found last_error gdbserver_status memory"
               " scheduler stats open_fds exectxt location unwind",
               wcmd, kwd_report_all)) {
      case -2:
      case -1:
         break;
      case 0: // all_errors
         // A verbosity of minimum 2 is needed to show the errors.
         VG_(show_all_errors)(/* verbosity */ 2, /* xml */ False);
         break;
      case  1: // n_errs_found
         VG_(printf) ("n_errs_found %u n_errs_shown %u (vgdb-error %d) %s\n",
                      VG_(get_n_errs_found) (),
                      VG_(get_n_errs_shown) (),
                      VG_(clo_vgdb_error),
                      wordn (mon, 3));
         break;
      case 2: // last_error
         VG_(show_last_error)();
         break;
      case  3: // gdbserver_status
         VG_(gdbserver_status_output)();
         break;
      case  4: /* memory */
         VG_(printf) ("%'13llu bytes have already been mmap-ed ANONYMOUS.\n",
                      VG_(am_get_anonsize_total)());
         VG_(print_all_arena_stats) ();
         if (VG_(clo_profile_heap))
            VG_(print_arena_cc_analysis) ();
         wcmd = strtok_r (NULL, " ", &ssaveptr);
         if (wcmd != NULL) {
            switch (VG_(keyword_id) ("aspacemgr", wcmd, kwd_report_all)) {
            case -2:
            case -1: break;
            case  0:
               VG_(am_show_nsegments) (0, "gdbserver v.info memory aspacemgr");
               break;
            default: vg_assert (0);
            }
         }

         ret = 1;
         break;
      case  5: /* scheduler */
         VG_(show_sched_status) (True,  // host_stacktrace
                                 True,  // stack_usage
                                 True); // exited_threads
         ret = 1;
         break;
      case  6: /* stats */
         VG_(print_all_stats)(False, /* Memory stats */
                              True   /* Tool stats */);
         ret = 1;
         break;
      case  7: /* open_fds */
         if (VG_(clo_track_fds))
            VG_(show_open_fds) ("");
         else
            VG_(gdb_printf)
               ("Valgrind must be started with --track-fds=[yes|all]"
                " to show open fds\n");
         ret = 1;
         break;
      case  8: /* exectxt */
         VG_(print_ExeContext_stats) (True /* with_stacktraces */);
         ret = 1;
         break;
      case  9: { /* location */
         /* Note: we prefer 'v.info location' and not 'v.info address' as
            v.info address is inconsistent with the GDB (native)
            command 'info address' which gives the address for a symbol.
            GDB equivalent command of 'v.info location' is 'info symbol'. */
         Addr address;
         SizeT dummy_sz = 0x1234;
         if (VG_(strtok_get_address_and_size) (&address,
                                               &dummy_sz, &ssaveptr)) {
            // If tool provides location information, use that.
            if (VG_(needs).info_location) {
               VG_TDICT_CALL(tool_info_location, cur_ep, address);
            }
            // If tool does not provide location info, use the common one.
            // Also use the common to compare with tool when debug log is set.
            if (!VG_(needs).info_location || VG_(debugLog_getLevel)() > 0 ) {
               AddrInfo ai;
               ai.tag = Addr_Undescribed;
               VG_(describe_addr) (cur_ep, address, &ai);
               VG_(pp_addrinfo) (address, &ai);
               VG_(clear_addrinfo) (&ai);
            }
         }
         ret = 1;
         break;
      }
      case 10: { /* unwind */
         Addr address;
         SizeT sz = 1;
         if (VG_(strtok_get_address_and_size) (&address,
                                               &sz, &ssaveptr)) {
            VG_(ppUnwindInfo) (address, address + sz - 1);
         }
         ret = 1;
         break;
      }

      default:
         vg_assert(0);
      }
      break;
   }
   case  3: /* v.wait */
      wcmd = strtok_r (NULL, " ", &ssaveptr);
      if (wcmd != NULL) {
         int_value = strtol (wcmd, NULL, 10);
         VG_(printf) ("gdbserver: continuing in %d ms ...\n", int_value);
         VG_(poll)(NULL, 0, int_value);
      }
      VG_(printf) ("gdbserver: continuing after wait ...\n");
      ret = 1;
      break;
   case  4: /* v.kill */
      kill_request ("monitor command request to kill this process\n");
      break;
   case  5: { /* v.translate */
      Addr address;
      SizeT verbosity = 0x20;

      ret = 1;

      if (VG_(strtok_get_address_and_size) (&address, &verbosity, &ssaveptr)) {
         /* we need to force the output to log for the translation trace,
            as low level VEX tracing cannot be redirected to gdb. */
         int saved_command_output_to_log = command_output_to_log;
         int saved_fd = VG_(log_output_sink).fd;
         Bool single_stepping_on_entry = valgrind_single_stepping();
         int vex_verbosity = verbosity & 0xff;
         VG_(log_output_sink).fd = initial_valgrind_sink.fd;
         if ((verbosity & 0x100) && !single_stepping_on_entry) {
            valgrind_set_single_stepping(True);
            // to force gdbserver instrumentation.
         }
#        if defined(VGA_arm)
         // on arm, we need to (potentially) convert this address
         // to the thumb form.
         address = thumb_pc (address);
#        endif

         VG_(translate) ( 0 /* dummy ThreadId; irrelevant due to debugging*/,
                          address,
                          /*debugging*/True,
                          (Int) vex_verbosity,
                          /*bbs_done*/0,
                          /*allow redir?*/True);
         if ((verbosity & 0x100) && !single_stepping_on_entry) {
            valgrind_set_single_stepping(False);
            // reset single stepping.
         }
         command_output_to_log = saved_command_output_to_log;
         VG_(log_output_sink).fd = saved_fd;
      }
      break;
   }

   case  6: /* v.do */
      ret = 1;
      wcmd = strtok_r (NULL, " ", &ssaveptr);
      switch (VG_(keyword_id) ("expensive_sanity_check_general",
                               wcmd, kwd_report_all)) {
         case -2:
         case -1: break;
         case  0: { /* expensive_sanity_check_general */
            // Temporarily bump up sanity level to check e.g. the malloc arenas.
            const Int save_clo_sanity_level = VG_(clo_sanity_level);
            if (VG_(clo_sanity_level) < 4) VG_(clo_sanity_level) = 4;
            VG_(sanity_check_general) (/* force_expensive */ True);
            VG_(clo_sanity_level) = save_clo_sanity_level;
            break;
         }
         default: vg_assert (0);
      }
      break;

   case  7: /* v.clo */
      ret = 0;
      for (wcmd = VG_(strtok_r)(NULL, " ", &ssaveptr);
           wcmd != NULL;
           wcmd = VG_(strtok_r)(NULL, " ", &ssaveptr)) {
         ret++;
         VG_(process_dynamic_option) (cloD, wcmd);
      }
      if (ret == 0)
         VG_(list_dynamic_options) ();
      ret = 1;
      break;

   default:
      vg_assert (0);
   }
   return ret;
}

/* handle_gdb_monitor_command handles the provided mon string command,
   which can be either a "standard" valgrind monitor command
   or a tool specific monitor command.
   If command recognised, return 1 else return 0.
   Note that in case of ambiguous command, 1 is returned.
*/
static
int handle_gdb_monitor_command (char *mon)
{
   UWord ret = 0;
   UWord tool_ret = 0;
   // initially, we assume that when returning, the desired sink is the
   // one we have when entering. It can however be changed by the standard
   // valgrind command handling.
   OutputSink sink_wanted_at_return = VG_(log_output_sink);
   // When using gdbserver, we temporarily disable xml output.
   Bool save_clo_xml = VG_(clo_xml);
   VG_(clo_xml) = False;

   if (!initial_valgrind_sink_saved) {
      /* first time we enter here, we save the valgrind default log sink */
      initial_valgrind_sink = sink_wanted_at_return;
      initial_valgrind_sink_saved = True;
   }

   if (!command_output_to_log)
      VG_(log_output_sink).fd = -2; /* redirect to monitor_output */

   ret = handle_gdb_valgrind_command (mon, &sink_wanted_at_return);

   /* Even if command was recognised by valgrind core, we call the
      tool command handler : this is needed to handle help command
      and/or to let the tool do some additional processing of a
      valgrind standard command. Note however that if valgrind
      recognised the command, we will always return success. */
   if (VG_(needs).client_requests) {
      /* If the tool reports an error when handling a monitor command,
         we need to avoid calling gdbserver during this command
         handling. So, we temporarily set VG_(clo_vgdb_error) to
         a huge value to ensure m_errormgr.c does not call gdbserver. */
      Int save_clo_vgdb_error = VG_(clo_vgdb_error);
      UWord arg[2];
      VG_(clo_vgdb_error) = 999999999;
      arg[0] = (UWord) VG_USERREQ__GDB_MONITOR_COMMAND;
      arg[1] = (UWord) mon;
      VG_TDICT_CALL(tool_handle_client_request, VG_(running_tid), arg,
                    &tool_ret);
      VG_(clo_vgdb_error) = save_clo_vgdb_error;
   }

   VG_(message_flush) ();

   /* restore or set the desired output */
   VG_(log_output_sink).fd = sink_wanted_at_return.fd;
   VG_(clo_xml) = save_clo_xml;

   if (ret | tool_ret)
      return 1;
   else
      return 0;
}


/* Handle all of the extended 'Q' packets.  */
static
void handle_set (char *arg_own_buf, int *new_packet_len_p)
{
   if (strcmp ("QStartNoAckMode", arg_own_buf) == 0) {
      noack_mode = True;
      write_ok (arg_own_buf);
      return;
   }

   if (strcmp ("QCatchSyscalls:0", arg_own_buf) == 0) {
      dlog (3, "catch syscall all off\n");
      catching_syscalls = False;
      write_ok (arg_own_buf);
      return;
   }

   const char *q1 = "QCatchSyscalls:1";
   if (strncmp (q1, arg_own_buf, strlen(q1)) == 0) {
      Int i;
      const char *p;

      if (syscalls_to_catch != NULL) {
         free (syscalls_to_catch);
         syscalls_to_catch = NULL;
      }
      syscalls_to_catch_size = 0;
      p = arg_own_buf + strlen(q1);
      while (*p) {
         if (*p++ == ';')
	    syscalls_to_catch_size++;
      }
      if (syscalls_to_catch_size > 0) {
         CORE_ADDR sysno;
         char *from, *to;

         syscalls_to_catch = malloc (syscalls_to_catch_size * sizeof (int));

         from = strchr (arg_own_buf, ';') + 1;
         for (i = 0; i < syscalls_to_catch_size; i++) {
            to = strchr (from, ';');
            if (to == NULL)
               to = arg_own_buf + strlen (arg_own_buf);
            decode_address (&sysno, from, to - from);
            syscalls_to_catch[i] = (Int)sysno;
            dlog(4, "catch syscall sysno %d\n", (int)sysno);
            from = to;
            if (*from == ';') from++;
         }
      } else
         dlog (4, "catch syscall all sysno\n");
      catching_syscalls = True;
      write_ok (arg_own_buf);
      return;
   }

   if (strncmp ("QPassSignals:", arg_own_buf, 13) == 0) {
      int i;
      char *from, *to;
      char *end = arg_own_buf + strlen(arg_own_buf);
      CORE_ADDR sig;
      for (i = 0; i < TARGET_SIGNAL_LAST; i++)
         pass_signals[i] = 0;

      from = arg_own_buf + 13;
      while (from < end) {
         to = strchr(from, ';');
         if (to == NULL) to = end;
         decode_address (&sig, from, to - from);
         pass_signals[(int)sig] = 1;
         dlog(3, "pass_signal gdb_nr %d %s\n",
              (int)sig, target_signal_to_name(sig));
         from = to;
         if (*from == ';') from++;
      }
      write_ok (arg_own_buf);
      return;
   }
   /* Otherwise we didn't know what packet it was.  Say we didn't
      understand it.  */
   arg_own_buf[0] = 0;
}

Bool VG_(client_monitor_command) (HChar *cmd)
{
   const Bool connected = remote_connected();
   const int saved_command_output_to_log = command_output_to_log;
   Bool handled;

   if (!connected)
      command_output_to_log = True;
   handled = handle_gdb_monitor_command (cmd);
   if (!connected) {
      // reset the log output unless cmd changed it.
      if (command_output_to_log)
         command_output_to_log = saved_command_output_to_log;
   }
   if (handled)
      return False; // recognised
   else
      return True; // not recognised
}

/* Handle all of the extended 'q' packets.  */
static
void handle_query (char *arg_own_buf, int *new_packet_len_p)
{
   static struct inferior_list_entry *thread_ptr;

   /* thread local storage query */
   if (strncmp ("qGetTLSAddr:", arg_own_buf, 12) == 0) {
      char *from, *to;
      char *end = arg_own_buf + strlen(arg_own_buf);
      unsigned long gdb_id;
      CORE_ADDR lm;
      CORE_ADDR offset;
      struct thread_info *ti;

      from = arg_own_buf + 12;
      to = strchr(from, ',');
      *to = 0;
      gdb_id = strtoul (from, NULL, 16);
      from = to + 1;
      to = strchr(from, ',');
      decode_address (&offset, from, to - from);
      from = to + 1;
      to = end;
      decode_address (&lm, from, to - from);
      dlog(2, "qGetTLSAddr thread %lu offset %p lm %p\n",
           gdb_id, (void*)offset, (void*)lm);

      ti = gdb_id_to_thread (gdb_id);
      if (ti != NULL) {
         ThreadState *tst;
         Addr tls_addr;

         tst = (ThreadState *) inferior_target_data (ti);
         if (valgrind_get_tls_addr(tst, offset, lm, &tls_addr)) {
            VG_(sprintf) (arg_own_buf, "%lx", tls_addr);
            return;
         }
         // else we will report we do not support qGetTLSAddr
      } else {
         write_enn (arg_own_buf);
         return;
      }
   }

   /* qRcmd, monitor command handling.  */
   if (strncmp ("qRcmd,", arg_own_buf, 6) == 0) {
      char *p = arg_own_buf + 6;
      int cmdlen = strlen(p)/2;
      char cmd[cmdlen+1];

      if (unhexify (cmd, p, cmdlen) != cmdlen) {
         write_enn (arg_own_buf);
         return;
      }
      cmd[cmdlen] = '\0';

      if (handle_gdb_monitor_command (cmd)) {
         write_ok (arg_own_buf);
         return;
      } else {
         /* cmd not recognised */
         VG_(gdb_printf)
            ("command '%s' not recognised\n"
             "In gdb,     try 'monitor help'\n"
             "In a shell, try 'vgdb help'\n",
             cmd);
         write_ok (arg_own_buf);
         return;
      }
   }

   /* provide some valgrind specific info in return to qThreadExtraInfo. */
   if (strncmp ("qThreadExtraInfo,", arg_own_buf, 17) == 0) {
      unsigned long gdb_id;
      struct thread_info *ti;
      ThreadState *tst;

      gdb_id = strtoul (&arg_own_buf[17], NULL, 16);
      ti = gdb_id_to_thread (gdb_id);
      if (ti != NULL) {
         tst = (ThreadState *) inferior_target_data (ti);
         /* Additional info is the tid, the thread status and the thread's
            name, if any. */
         SizeT len = strlen(VG_(name_of_ThreadStatus)(tst->status)) + 20;
         if (tst->thread_name) len += strlen(tst->thread_name);
         /* As the string will be hexified and copied into own_buf we need
            to limit the length to avoid buffer overflow. */
         if (len * 2 > (PBUFSIZ + POVERHSIZ))
            len = (PBUFSIZ + POVERHSIZ) / 2;
         char status[len];
         if (tst->thread_name) {
            VG_(snprintf) (status, sizeof(status), "tid %u %s %s",
                           tst->tid,
                           VG_(name_of_ThreadStatus)(tst->status),
                           tst->thread_name);
         } else {
            VG_(snprintf) (status, sizeof(status), "tid %u %s",
                           tst->tid,
                           VG_(name_of_ThreadStatus)(tst->status));
         }
         hexify (arg_own_buf, status, strlen(status));
         return;
      } else {
         write_enn (arg_own_buf);
         return;
      }
   }

   /* Without argument, traditional remote protocol.  */
   if (strcmp ("qAttached", arg_own_buf) == 0) {
      /* tell gdb to always detach, never kill the process */
      arg_own_buf[0] = '1';
      arg_own_buf[1] = 0;
      return;
   }

   /* With argument, extended-remote protocol.  */
   if (strncmp ("qAttached:", arg_own_buf, strlen ("qAttached:")) == 0) {
      /* We just created this process */
      arg_own_buf[0] = '0';
      arg_own_buf[1] = 0;
      return;
   }

   if (strcmp ("qSymbol::", arg_own_buf) == 0) {
      /* We have no symbol to read. */
      write_ok (arg_own_buf);
      return;
   }

   if (strcmp ("qC", arg_own_buf) == 0) {
      VG_(sprintf) (arg_own_buf, "QC%x",
                    thread_to_gdb_id (current_inferior));
      return;
   }

   if (strcmp ("qfThreadInfo", arg_own_buf) == 0) {
      thread_ptr = all_threads.head;
      VG_(sprintf) (arg_own_buf, "m%x",
                    thread_to_gdb_id ((struct thread_info *)thread_ptr));
      thread_ptr = thread_ptr->next;
      return;
   }

   if (strcmp ("qsThreadInfo", arg_own_buf) == 0) {
      if (thread_ptr != NULL) {
         VG_(sprintf) (arg_own_buf, "m%x",
                       thread_to_gdb_id ((struct thread_info *)thread_ptr));
         thread_ptr = thread_ptr->next;
         return;
      } else {
         VG_(sprintf) (arg_own_buf, "l");
         return;
      }
   }

   if (valgrind_target_xml(VG_(clo_vgdb_shadow_registers)) != NULL
        && strncmp ("qXfer:features:read:", arg_own_buf, 20) == 0) {
      CORE_ADDR ofs;
      unsigned int len, doc_len;
      const char *annex = NULL;
      // First, the annex is extracted from the packet received.
      // Then, it is replaced by the corresponding file name.
      int fd;

      /* Grab the annex, offset, and length.  */
      if (decode_xfer_read (arg_own_buf + 20, &annex, &ofs, &len) < 0) {
         strcpy (arg_own_buf, "E00");
         return;
      }

      if (strcmp (annex, "target.xml") == 0) {
         annex = valgrind_target_xml(VG_(clo_vgdb_shadow_registers));
         if (annex != NULL && VG_(clo_vgdb_shadow_registers)) {
            /* Ensure the shadow registers are initialized. */
            initialize_shadow_low(True);
         }
         if (annex == NULL) {
            strcpy (arg_own_buf, "E00");
            return;
         }
      }

      {
         char doc[VG_(strlen)(VG_(libdir)) + 1 + VG_(strlen)(annex) + 1];
         struct vg_stat stat_doc;
         char toread[len];
         int len_read;

         VG_(sprintf)(doc, "%s/%s", VG_(libdir), annex);
         fd = VG_(fd_open) (doc, VKI_O_RDONLY, 0);
         if (fd == -1) {
            strcpy (arg_own_buf, "E00");
            return;
         }
         if (VG_(fstat) (fd, &stat_doc) != 0) {
            VG_(close) (fd);
            strcpy (arg_own_buf, "E00");
            return;
         }
         doc_len = stat_doc.size;

         if (len > PBUFSIZ - POVERHSIZ)
            len = PBUFSIZ - POVERHSIZ;

         if (ofs > doc_len) {
            write_enn (arg_own_buf);
            VG_(close) (fd);
            return;
         }
         VG_(lseek) (fd, ofs, VKI_SEEK_SET);
         len_read = VG_(read) (fd, toread, len);
         *new_packet_len_p = write_qxfer_response (arg_own_buf,
                                                   (unsigned char *)toread,
                                                   len_read,
                                                   ofs + len_read < doc_len);
         VG_(close) (fd);
         return;
      }
   }

   if (strncmp ("qXfer:auxv:read:", arg_own_buf, 16) == 0) {
      unsigned char *data;
      int n;
      CORE_ADDR ofs;
      unsigned int len;
      const char *annex;

      /* Reject any annex; grab the offset and length.  */
      if (decode_xfer_read (arg_own_buf + 16, &annex, &ofs, &len) < 0
          || annex[0] != '\0') {
         strcpy (arg_own_buf, "E00");
         return;
      }

      if (len > PBUFSIZ - POVERHSIZ)
         len = PBUFSIZ - POVERHSIZ;
      data = malloc (len);

      {
         UWord *client_auxv = VG_(client_auxv);
         unsigned int client_auxv_len = 0;
         while (*client_auxv != 0) {
            dlog(4, "auxv %llu %llx\n",
                 (ULong)*client_auxv,
                 (ULong)*(client_auxv+1));
            client_auxv++;
            client_auxv++;
            client_auxv_len += 2 * sizeof(UWord);
         }
         client_auxv_len += 2 * sizeof(UWord);
         dlog(4, "auxv len %u\n", client_auxv_len);

         if (ofs >= client_auxv_len)
            n = -1;
         else {
            n = client_auxv_len - ofs;
            VG_(memcpy) (data, (unsigned char *) VG_(client_auxv), n);
         }
      }

      if (n < 0)
         write_enn (arg_own_buf);
      else if (n > len)
         *new_packet_len_p = write_qxfer_response (arg_own_buf, data, len, 1);
      else
         *new_packet_len_p = write_qxfer_response (arg_own_buf, data, n, 0);

      free (data);

      return;
   }

   if (strncmp ("qXfer:exec-file:read:", arg_own_buf, 21) == 0) {
      unsigned char *data;
      int n;
      CORE_ADDR ofs;
      unsigned int len;
      const char *annex;
      unsigned long pid;
      const HChar *name;

      /* grab the annex, offset and length.  */
      if (decode_xfer_read (arg_own_buf + 21, &annex, &ofs, &len) < 0) {
         strcpy (arg_own_buf, "E00");
         return;
      }

      /* Reject any annex with invalid/unexpected pid */
      if (strlen(annex) > 0)
         pid = strtoul (annex, NULL, 16);
      else
         pid = 0;
      if ((int)pid != VG_(getpid)() && pid != 0) {
         VG_(sprintf) (arg_own_buf,
                       "E.Valgrind gdbserver pid is %d."
                       " Cannot give info for pid %d",
                       VG_(getpid)(), (int) pid);
         return;
      }

      if (len > PBUFSIZ - 2)
         len = PBUFSIZ - 2;
      data = malloc (len);

      if (!VG_(resolve_filename)(VG_(cl_exec_fd), &name)) {
         VG_(sprintf) (arg_own_buf,
                       "E.Valgrind gdbserver could not"
                       " resolve pid %d exec filename.",
                       VG_(getpid)());
         return;
      }

      if (ofs >= strlen(name))
         n = -1;
      else {
         n = strlen(name) - ofs;
         VG_(memcpy) (data, name, n);
      }

      if (n < 0)
         write_enn (arg_own_buf);
      else if (n > len)
         *new_packet_len_p = write_qxfer_response (arg_own_buf, data, len, 1);
      else
         *new_packet_len_p = write_qxfer_response (arg_own_buf, data, n, 0);

      free (data);

      return;
   }

   if (strncmp ("qXfer:siginfo:read:", arg_own_buf, 19) == 0) {
      vki_siginfo_t info;
      int n;
      CORE_ADDR ofs;
      unsigned int len;
      const char *annex;

      /* Reject any annex; grab the offset and length.  */
      if (decode_xfer_read (arg_own_buf + 19, &annex, &ofs, &len) < 0
          || annex[0] != '\0') {
         strcpy (arg_own_buf, "E00");
         return;
      }

      if (len > PBUFSIZ - POVERHSIZ)
         len = PBUFSIZ - POVERHSIZ;

      gdbserver_pending_signal_to_report(&info);

      if (ofs >= sizeof(info))
         n = -1;
      else
         n = sizeof(info) - ofs;

      if (n < 0)
         write_enn (arg_own_buf);
      else if (n > len)
         *new_packet_len_p = write_qxfer_response (arg_own_buf,
                                                   (unsigned char *)&info,
                                                   len, 1);
      else
         *new_packet_len_p = write_qxfer_response (arg_own_buf,
                                                   (unsigned char *)&info,
                                                   n, 0);

      return;
   }

   /* Protocol features query.  Keep this in sync with coregind/vgdb.c.  */
   if (strncmp ("qSupported", arg_own_buf, 10) == 0
       && (arg_own_buf[10] == ':' || arg_own_buf[10] == '\0')) {
      VG_(sprintf) (arg_own_buf, "PacketSize=%x", (UInt)PBUFSIZ - 1);
      /* Note: max packet size including frame and checksum, but without
         trailing null byte, which is not sent/received. */

      strcat (arg_own_buf, ";QStartNoAckMode+");
      strcat (arg_own_buf, ";QPassSignals+");
      strcat (arg_own_buf, ";QCatchSyscalls+");
      if (VG_(client_auxv))
         strcat (arg_own_buf, ";qXfer:auxv:read+");

      if (valgrind_target_xml(VG_(clo_vgdb_shadow_registers)) != NULL) {
         strcat (arg_own_buf, ";qXfer:features:read+");
         /* if a new gdb connects to us, we have to reset the register
            set to the normal register sets to allow this new gdb to
            decide to use or not the shadow registers.

            Note that the reset is only done for gdb that are sending
            qSupported packets. If a user first connected with a recent
            gdb using shadow registers and then with a very old gdb
            that does not use qSupported packet, then the old gdb will
            not properly connect. */
         initialize_shadow_low(False);
      }
      strcat (arg_own_buf, ";qXfer:exec-file:read+");
      strcat (arg_own_buf, ";qXfer:siginfo:read+");
      return;
   }

   /* Otherwise we didn't know what packet it was.  Say we didn't
      understand it.  */
   arg_own_buf[0] = 0;
}

/* Handle all of the extended 'v' packets.  */
static
void handle_v_requests (char *arg_own_buf, char *status, int *zignal)
{
   /* vcont packet code from gdb 6.6 removed */

   /* Otherwise we didn't know what packet it was.  Say we didn't
      understand it.  */
   arg_own_buf[0] = 0;
   return;
}

static
void myresume (int step, int sig)
{
   struct thread_resume resume_info[2];
   int n = 0;

   if (step || sig) {
      resume_info[0].step = step;
      resume_info[0].sig = sig;
      n++;
   }
   resume_info[n].step = 0;
   resume_info[n].sig = 0;

   resume_reply_packet_needed = True;
   valgrind_resume (resume_info);
}

/* server_main global variables */
static char *own_buf;
static unsigned char *mem_buf;

void gdbserver_init (void)
{
   dlog(1, "gdbserver_init gdbserver embedded in valgrind: %s\n", version);
   noack_mode = False;
   valgrind_initialize_target ();
   // After a fork, gdbserver_init can be called again.
   // We do not have to re-malloc the buffers in such a case.
   if (own_buf == NULL)
      own_buf = malloc (PBUFSIZ+POVERHSIZ);
   if (mem_buf == NULL)
      mem_buf = malloc (PBUFSIZ+POVERHSIZ);
   // Note: normally, we should only malloc PBUFSIZ. However,
   // GDB has a bug, and in some cases, sends e.g. 'm' packets
   // asking for slightly more than the PacketSize given at
   // connection initialisation. So, we bypass the GDB bug
   // by allocating slightly more.
}

void gdbserver_terminate (void)
{
   /* last call to gdbserver is cleanup call */
   if (VG_MINIMAL_SETJMP(toplevel)) {
      dlog(0, "error caused VG_MINIMAL_LONGJMP to gdbserver_terminate\n");
      return;
   }
   remote_close();
}

void server_main (void)
{
   static char status;
   static int zignal;

   char ch;
   int i = 0;
   unsigned int len;
   CORE_ADDR mem_addr;

   zignal = valgrind_wait (&status);
   if (VG_MINIMAL_SETJMP(toplevel)) {
      dlog(0, "error caused VG_MINIMAL_LONGJMP to server_main\n");
   }
   while (1) {
      unsigned char sig;
      int packet_len;
      int new_packet_len = -1;

      if (resume_reply_packet_needed) {
         /* Send the resume reply to reply to last GDB resume
            request. */
         resume_reply_packet_needed = False;
         prepare_resume_reply (own_buf, status, zignal);
         putpkt (own_buf);
      }

      /* If our status is terminal (exit or fatal signal) get out
         as quickly as we can. We won't be able to handle any request
         anymore.  */
      if (status == 'W' || status == 'X') {
         return;
      }

      packet_len = getpkt (own_buf);
      if (packet_len <= 0)
         break;

      i = 0;
      ch = own_buf[i++];
      switch (ch) {
      case 'Q':
         handle_set (own_buf, &new_packet_len);
         break;
      case 'q':
         handle_query (own_buf, &new_packet_len);
         break;
      case 'd':
         /* set/unset debugging is done through valgrind debug level. */
         own_buf[0] = '\0';
         break;
      case 'D':
         reset_valgrind_sink("gdb detaching from process");

         /* When detaching or kill the process, gdb expects to get
            an packet OK back.  Any other output will make gdb
            believes detach did not work. */
         write_ok (own_buf);
         putpkt (own_buf);
         remote_finish (reset_after_error);
         remote_open (VG_(clo_vgdb_prefix));
         myresume (0, 0);
         resume_reply_packet_needed = False;
         return;
      case '!':
         /* We can not use the extended protocol with valgrind,
            because we can not restart the running
            program.  So return unrecognized.  */
         own_buf[0] = '\0';
         break;
      case '?':
         prepare_resume_reply (own_buf, status, zignal);
         break;
      case 'H':
         if (own_buf[1] == 'c' || own_buf[1] == 'g' || own_buf[1] == 's') {
            unsigned long gdb_id, thread_id;

            gdb_id = strtoul (&own_buf[2], NULL, 16);
            thread_id = gdb_id_to_thread_id (gdb_id);
            if (thread_id == 0) {
               write_enn (own_buf);
               break;
            }

            if (own_buf[1] == 'g') {
               general_thread = thread_id;
               set_desired_inferior (1);
            } else if (own_buf[1] == 'c') {
               cont_thread = thread_id;
            } else if (own_buf[1] == 's') {
               step_thread = thread_id;
            }

            write_ok (own_buf);
         } else {
            /* Silently ignore it so that gdb can extend the protocol
               without compatibility headaches.  */
            own_buf[0] = '\0';
         }
         break;
      case 'g':
         set_desired_inferior (1);
         registers_to_string (own_buf);
         break;
      case 'G':
         set_desired_inferior (1);
         registers_from_string (&own_buf[1]);
         write_ok (own_buf);
         break;
      case 'P': {
         int regno;
         char *regbytes;
         ThreadState *tst;
         regno = strtol(&own_buf[1], NULL, 16);
         regbytes = strchr(&own_buf[0], '=') + 1;
         set_desired_inferior (1);
         tst = (ThreadState *) inferior_target_data (current_inferior);
         /* Only accept changing registers in "runnable state3.
            In fact, it would be ok to change most of the registers
            except a few "sensitive" registers such as the PC, SP, BP.
            We assume we do not need to very specific here, and that we
            can just refuse all of these. */
         if (tst->status == VgTs_Runnable || tst->status == VgTs_Yielding) {
            supply_register_from_string (regno, regbytes);
            write_ok (own_buf);
         } else {
            /* at least from gdb 6.6 onwards, an E. error
               reply is shown to the user. So, we do an error
               msg which both is accepted by gdb as an error msg
               and is readable by the user. */
            VG_(sprintf)
               (own_buf,
"E.\n"
"ERROR changing register %s regno %d\n"
"gdb commands changing registers (pc, sp, ...) (e.g. 'jump',\n"
"set pc, calling from gdb a function in the debugged process, ...)\n"
"can only be accepted if the thread is VgTs_Runnable or VgTs_Yielding state\n"
"Thread status is %s\n",
                find_register_by_number (regno)->name, regno,
                VG_(name_of_ThreadStatus)(tst->status));
            if (VG_(clo_verbosity) > 1)
               VG_(umsg) ("%s\n", own_buf);
         }
         break;
      }
      case 'm':
         decode_m_packet (&own_buf[1], &mem_addr, &len);
         if (valgrind_read_memory (mem_addr, mem_buf, len) == 0)
            convert_int_to_ascii (mem_buf, own_buf, len);
         else
            write_enn (own_buf);
         break;
      case 'M':
         decode_M_packet (&own_buf[1], &mem_addr, &len, mem_buf);
         if (valgrind_write_memory (mem_addr, mem_buf, len) == 0)
            write_ok (own_buf);
         else
            write_enn (own_buf);
         break;
      case 'X':
         if (decode_X_packet (&own_buf[1], packet_len - 1,
                              &mem_addr, &len, mem_buf) < 0
             || valgrind_write_memory (mem_addr, mem_buf, len) != 0)
            write_enn (own_buf);
         else
            write_ok (own_buf);
         break;
      case 'C':
         convert_ascii_to_int (own_buf + 1, &sig, 1);
         if (target_signal_to_host_p (sig))
            zignal = target_signal_to_host (sig);
         else
            zignal = 0;
         set_desired_inferior (0);
         myresume (0, zignal);
         return; // return control to valgrind
      case 'S':
         convert_ascii_to_int (own_buf + 1, &sig, 1);
         if (target_signal_to_host_p (sig))
            zignal = target_signal_to_host (sig);
         else
            zignal = 0;
         set_desired_inferior (0);
         myresume (1, zignal);
         return; // return control to valgrind
      case 'c':
         set_desired_inferior (0);
         myresume (0, 0);
         return; // return control to valgrind
      case 's':
         set_desired_inferior (0);
         myresume (1, 0);
         return; // return control to valgrind
      case 'Z': {
         char *lenptr;
         char *dataptr;
         CORE_ADDR addr = strtoul (&own_buf[3], &lenptr, 16);
         int zlen = strtol (lenptr + 1, &dataptr, 16);
         char type = own_buf[1];

         if (type < '0' || type > '4') {
            /* Watchpoint command type unrecognized. */
            own_buf[0] = '\0';
         } else {
            int res;

            res = valgrind_insert_watchpoint (type, addr, zlen);
            if (res == 0)
               write_ok (own_buf);
            else if (res == 1)
               /* Unsupported.  */
               own_buf[0] = '\0';
            else
               write_enn (own_buf);
         }
         break;
      }
      case 'z': {
         char *lenptr;
         char *dataptr;
         CORE_ADDR addr = strtoul (&own_buf[3], &lenptr, 16);
         int zlen = strtol (lenptr + 1, &dataptr, 16);
         char type = own_buf[1];

         if (type < '0' || type > '4') {
            /* Watchpoint command type unrecognized. */
            own_buf[0] = '\0';
         } else {
            int res;

            res = valgrind_remove_watchpoint (type, addr, zlen);
            if (res == 0)
               write_ok (own_buf);
            else if (res == 1)
               /* Unsupported.  */
               own_buf[0] = '\0';
            else
               write_enn (own_buf);
         }
         break;
      }
      case 'k':
         kill_request("Gdb request to kill this process\n");
         break;
      case 'T': {
         unsigned long gdb_id, thread_id;

         gdb_id = strtoul (&own_buf[1], NULL, 16);
         thread_id = gdb_id_to_thread_id (gdb_id);
         if (thread_id == 0) {
            write_enn (own_buf);
            break;
         }

         if (valgrind_thread_alive (thread_id))
            write_ok (own_buf);
         else
            write_enn (own_buf);
         break;
      }
      case 'R':
         /* Restarting the inferior is only supported in the
            extended protocol.
            => It is a request we don't understand.  Respond with an
            empty packet so that gdb knows that we don't support this
            request.  */
         own_buf[0] = '\0';
         break;
      case 'v':
         /* Extended (long) request.  */
         handle_v_requests (own_buf, &status, &zignal);
         break;
      default:
         /* It is a request we don't understand.  Respond with an
            empty packet so that gdb knows that we don't support this
            request.  */
         own_buf[0] = '\0';
         break;
      }

      if (new_packet_len != -1)
         putpkt_binary (own_buf, new_packet_len);
      else
         putpkt (own_buf);

      if (status == 'W')
         VG_(umsg) ("\nChild exited with status %d\n", zignal);
      if (status == 'X')
         VG_(umsg) ("\nChild terminated with signal = 0x%x (%s)\n",
                    (UInt)target_signal_to_host (zignal),
                    target_signal_to_name (zignal));
      if (status == 'W' || status == 'X') {
         VG_(umsg) ("Process exiting\n");
         VG_(exit) (0);
      }
   }

   /* We come here when getpkt fails => close the connection,
      and re-open. Then return control to valgrind.
      We return the control to valgrind as we assume that
      the connection was closed due to vgdb having finished
      to execute a command. */
   if (VG_(clo_verbosity) > 1)
      VG_(umsg) ("Remote side has terminated connection.  "
                 "GDBserver will reopen the connection.\n");
   remote_finish (reset_after_error);
   remote_open (VG_(clo_vgdb_prefix));
   myresume (0, 0);
   resume_reply_packet_needed = False;
   return;
}
