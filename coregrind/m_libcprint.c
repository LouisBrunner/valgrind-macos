/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Libc printing.                                 m_libcprint.c ---*/
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
#include "pub_core_vkiscnums.h"
#include "pub_core_debuglog.h"
#include "pub_core_gdbserver.h"  // VG_(gdb_printf)
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"   // VG_(write)(), VG_(write_socket)()
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"   // VG_(getpid)(), VG_(read_millisecond_timer()
#include "pub_core_mallocfree.h" // VG_(malloc)
#include "pub_core_machine.h"    // VG_(machine_get_VexArchInfo)
#include "pub_core_options.h"
#include "pub_core_clreq.h"      // For RUNNING_ON_VALGRIND
#include "pub_core_clientstate.h"
#include "pub_core_syscall.h"    // VG_(strerror)
#include "pub_core_tooliface.h"  // VG_(details)


/*====================================================================*/
/*=== Printing the preamble                                        ===*/
/*====================================================================*/

// Returns a strdup'd copy of |str| in which characters which are not in the
// obviously-harmless-ASCII range are replaced with '_'.  Not doing this has
// been observed to cause xfce4-terminal to assert.  Caller takes ownership
// of the returned string.
static HChar* sanitise_arg (const HChar* arg)
{
   HChar* clone = VG_(strdup)("m_libcprint.sanitise_arg", arg);
   for (HChar* p = clone; *p; p++) {
      UInt c = * ((UChar*)p);
      if (c < 32 || c > 127) c = '_';
      *p = (HChar)c;
   }
   return clone;
}

// Print the argument, escaping any chars that require it.
static void umsg_arg(const HChar *unsanitised_arg)
{
   HChar* arg = sanitise_arg(unsanitised_arg);
   SizeT len = VG_(strlen)(arg);
   const HChar *special = " \\<>";
   for (UInt i = 0; i < len; i++) {
      if (VG_(strchr)(special, arg[i])) {
         VG_(umsg)("\\");   // escape with a backslash if necessary
      }
      VG_(umsg)("%c", arg[i]);
   }
   VG_(free)(arg);
}

// Send output to the XML-stream and escape any XML meta-characters.
static void xml_arg(const HChar *unsanitised_arg)
{
   HChar* arg = sanitise_arg(unsanitised_arg);
   VG_(printf_xml)("%pS", arg);
   VG_(free)(arg);
}

// Write the name and value of log file qualifiers to the xml file.
// We can safely assume here that the format string is well-formed.
// It has been checked earlier in VG_(expand_file_name) when processing
// command line options.
static void print_file_vars(const HChar *format)
{
   UInt i = 0;
   
   while (format[i]) {
      if (format[i] == '%') {
         // We saw a '%'.  What's next...
         i++;
         if ('q' == format[i]) {
            i++;
            if ('{' == format[i]) {
               // Get the env var name, print its contents.
               UInt begin_qualname = ++i;
               while (True) {
                  if ('}' == format[i]) {
                     UInt qualname_len = i - begin_qualname;
                     HChar qualname[qualname_len + 1];
                     VG_(strncpy)(qualname, format + begin_qualname,
                                  qualname_len);
                     qualname[qualname_len] = '\0';
                     HChar *qual = VG_(getenv)(qualname);
                     i++;
                     VG_(printf_xml)("<logfilequalifier> <var>%pS</var> "
                                     "<value>%pS</value> </logfilequalifier>\n",
                                     qualname, qual);
                     break;
                  }
                  i++;
               }
            }
         }
      } else {
         i++;
      }
   }
}

/* Ok, the logging sink is running now.  Print a suitable preamble.
   If logging to file or a socket, write details of parent PID and
   command line args, to help people trying to interpret the
   results of a run which encompasses multiple processes. */
void VG_(print_preamble)(Bool logging_to_fd)
{
   const HChar *xpre  = VG_(clo_xml) ? "  <line>" : "";
   const HChar *xpost = VG_(clo_xml) ? "</line>" : "";
   UInt (*umsg_or_xml)( const HChar *, ... )
      = VG_(clo_xml) ? VG_(printf_xml) : VG_(umsg);
   void (*umsg_or_xml_arg)( const HChar *) = VG_(clo_xml) ? xml_arg : umsg_arg;

   vg_assert( VG_(args_for_client) );
   vg_assert( VG_(args_for_valgrind) );
   vg_assert( VG_(clo_toolname) );

   if (VG_(clo_xml)) {
      VG_(printf_xml)("<?xml version=\"1.0\"?>\n");
      VG_(printf_xml)("\n");
      VG_(printf_xml)("<valgrindoutput>\n");
      VG_(printf_xml)("\n");
      /* track-fds introduced some new elements.  */
      if (VG_(clo_track_fds))
         VG_(printf_xml)("<protocolversion>5</protocolversion>\n");
      else
         VG_(printf_xml)("<protocolversion>4</protocolversion>\n");
      VG_(printf_xml)("<protocoltool>%s</protocoltool>\n", VG_(clo_toolname));
      VG_(printf_xml)("\n");
   }

   if (VG_(clo_xml) || VG_(clo_verbosity) > 0) {

      if (VG_(clo_xml))
         VG_(printf_xml)("<preamble>\n");

      /* Tool details */
      umsg_or_xml(VG_(clo_xml) ? "%s%pS%pS%pS, %pS%s\n" : "%s%s%s%s, %s%s\n",
                  xpre,
                  VG_(details).name, 
                  NULL == VG_(details).version ? "" : "-",
                  NULL == VG_(details).version ? "" : VG_(details).version,
                  VG_(details).description,
                  xpost);

      if (VG_(strlen)(VG_(clo_toolname)) >= 4 &&
          VG_STREQN(4, VG_(clo_toolname), "exp-")) {
         umsg_or_xml("%sNOTE: This is an Experimental-Class Valgrind Tool%s\n",
                     xpre, xpost);
      }

      umsg_or_xml(VG_(clo_xml) ? "%s%pS%s\n" : "%s%s%s\n",
                  xpre, VG_(details).copyright_author, xpost);

      /* Core details */
      umsg_or_xml(
         "%sUsing Valgrind-%s and LibVEX; rerun with -h for copyright info%s\n",
         xpre, VG_(clo_verbosity) <= 1 ? VERSION : VERSION "-" VGGIT, xpost);

      // Print the command line.  At one point we wrapped at 80 chars and
      // printed a '\' as a line joiner, but that makes it hard to cut and
      // paste the command line (because of the "==pid==" prefixes), so we now
      // favour utility and simplicity over aesthetics.
      umsg_or_xml("%sCommand: ", xpre);
      umsg_or_xml_arg(VG_(args_the_exename));
          
      for (UInt i = 0; i < VG_(sizeXA)( VG_(args_for_client)); i++) {
         HChar *s = *(HChar **)VG_(indexXA)( VG_(args_for_client), i);
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
   } else if (VG_(clo_xml)) {
      VG_(printf_xml)("\n");
      VG_(printf_xml)("<pid>%d</pid>\n", VG_(getpid)());
      VG_(printf_xml)("<ppid>%d</ppid>\n", VG_(getppid)());
      VG_(printf_xml)("<tool>%pS</tool>\n", VG_(clo_toolname));
      if (VG_(clo_xml_fname_unexpanded) != NULL)
         print_file_vars(VG_(clo_xml_fname_unexpanded));
      if (VG_(clo_xml_user_comment)) {
         /* Note: the user comment itself is XML and is therefore to
            be passed through verbatim (%s) rather than escaped (%pS). */
         VG_(printf_xml)("<usercomment>%s</usercomment>\n",
                         VG_(clo_xml_user_comment));
      }
      VG_(printf_xml)("\n");
      VG_(printf_xml)("<args>\n");

      VG_(printf_xml)("  <vargv>\n");
      if (VG_(name_of_launcher))
         VG_(printf_xml)("    <exe>%pS</exe>\n", VG_(name_of_launcher));
      else
         VG_(printf_xml)("    <exe>%pS</exe>\n", "(launcher name unknown)");
      for (UInt i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         VG_(printf_xml)(
            "    <arg>%pS</arg>\n",
            *(HChar **) VG_(indexXA)( VG_(args_for_valgrind), i));
      }
      VG_(printf_xml)("  </vargv>\n");

      VG_(printf_xml)("  <argv>\n");
      VG_(printf_xml)("    <exe>%pS</exe>\n", VG_(args_the_exename));
      for (UInt i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
         VG_(printf_xml)(
            "    <arg>%pS</arg>\n",
            *(HChar **) VG_(indexXA)( VG_(args_for_client), i));
      }
      VG_(printf_xml)("  </argv>\n");

      VG_(printf_xml)("</args>\n");
   }

   // Last thing in the preamble is a blank line.
   if (VG_(clo_xml))
      VG_(printf_xml)("\n");
   else if (VG_(clo_verbosity) > 0)
      VG_(umsg)("\n");

   if (VG_(clo_verbosity) > 1) {
# if defined(VGO_linux)
      SysRes fd;
# endif
      VexArch vex_arch;
      VexArchInfo vex_archinfo;
      if (!logging_to_fd)
         VG_(message)(Vg_DebugMsg, "\n");
      VG_(message)(Vg_DebugMsg, "Valgrind options:\n");
      for (UInt i = 0; i < VG_(sizeXA)( VG_(args_for_valgrind) ); i++) {
         VG_(message)(Vg_DebugMsg, 
                     "   %s\n", 
                     *(HChar **) VG_(indexXA)( VG_(args_for_valgrind), i));
      }

# if defined(VGO_linux)
      VG_(message)(Vg_DebugMsg, "Contents of /proc/version:\n");
      fd = VG_(open)("/proc/version", VKI_O_RDONLY, 0);
      if (sr_isError(fd)) {
         VG_(message)(Vg_DebugMsg, "  can't open /proc/version\n");
      } else {
         const SizeT bufsiz = 255;
         HChar version_buf[bufsiz+1];
         VG_(message)(Vg_DebugMsg, "  ");
         Int n, fdno = sr_Res(fd);
         do {
            n = VG_(read)(fdno, version_buf, bufsiz);
            if (n < 0) {
               VG_(message)(Vg_DebugMsg, "  error reading /proc/version\n");
               break;
            }
            version_buf[n] = '\0';
            VG_(message)(Vg_DebugMsg, "%s", version_buf);
         } while (n == bufsiz);
         VG_(message)(Vg_DebugMsg, "\n");
         VG_(close)(fdno);
      }
# elif defined(VGO_darwin)
      VG_(message)(Vg_DebugMsg, "Output from sysctl({CTL_KERN,KERN_VERSION}):\n");
      /* Note: preferable to use sysctlbyname("kern.version", kernelVersion, &len, NULL, 0)
         however that syscall is OS X 10.10+ only. */
      Int mib[] = {CTL_KERN, KERN_VERSION};
      SizeT len;
      VG_(sysctl)(mib, sizeof(mib)/sizeof(Int), NULL, &len, NULL, 0);
      HChar *kernelVersion = VG_(malloc)("main.pp.1", len);
      VG_(sysctl)(mib, sizeof(mib)/sizeof(Int), kernelVersion, &len, NULL, 0);
      VG_(message)(Vg_DebugMsg, "  %s\n", kernelVersion);
      VG_(free)( kernelVersion );
# elif defined(VGO_solaris)
      /* There is no /proc/version file on Solaris so we try to get some
         system information using the uname(2) syscall. */
      struct vki_utsname uts;
      VG_(message)(Vg_DebugMsg, "System information:\n");
      SysRes res = VG_(do_syscall1)(__NR_uname, (UWord)&uts);
      if (sr_isError(res))
         VG_(message)(Vg_DebugMsg, "  uname() failed\n");
      else
         VG_(message)(Vg_DebugMsg, "  %s %s %s %s\n",
                      uts.sysname, uts.release, uts.version, uts.machine);
# endif

      VG_(machine_get_VexArchInfo)(&vex_arch, &vex_archinfo);
      VG_(message)(
         Vg_DebugMsg, 
         "Arch and hwcaps: %s, %s, %s\n",
         LibVEX_ppVexArch    ( vex_arch ),
         LibVEX_ppVexEndness ( vex_archinfo.endness ),
         LibVEX_ppVexHwCaps  ( vex_arch, vex_archinfo.hwcaps )
      );
      VG_(message)(Vg_DebugMsg, 
                  "Page sizes: currently %u, max supported %u\n", 
                  (UInt) VKI_PAGE_SIZE, (UInt) VKI_MAX_PAGE_SIZE);
      VG_(message)(Vg_DebugMsg,
                   "Valgrind library directory: %s\n", VG_(libdir));
   }
}

/* ---------------------------------------------------------------------
   Writing to file or a socket
   ------------------------------------------------------------------ */

/* The destination sinks for normal and XML output.  These have their
   initial values here; they are set to final values by
   m_main.main_process_cmd_line_options().  See comment at the top of
   that function for the associated logic. 
   After startup, the gdbserver monitor command might temporarily
   set the fd of log_output_sink to -2 to indicate that output is
   to be given to gdb rather than output to the startup fd */
OutputSink VG_(log_output_sink) = {  2, VgLogTo_Fd, NULL }; /* 2 = stderr */
OutputSink VG_(xml_output_sink) = { -1, VgLogTo_Fd, NULL }; /* disabled */

static void revert_sink_to_stderr ( OutputSink *sink )
{
   sink->fd = 2; /* stderr */
   sink->type = VgLogTo_Fd;
   VG_(free)(sink->fsname_expanded);
   sink->fsname_expanded = NULL;
}

static Int prepare_sink_fd(const HChar *clo_fname_unexpanded, OutputSink *sink,
                           Bool is_xml)
{
   vg_assert(clo_fname_unexpanded != NULL);
   vg_assert(VG_(strlen)(clo_fname_unexpanded) <= 900); /* paranoia */

   // Nb: we overwrite an existing file of this name without asking
   // any questions.
   HChar *logfilename = VG_(expand_file_name)(
                                         (is_xml) ? "--xml-file" : "--log-file",
                                         clo_fname_unexpanded);
   SysRes sres = VG_(open)(logfilename, 
                           VKI_O_CREAT|VKI_O_WRONLY|VKI_O_TRUNC, 
                           VKI_S_IRUSR|VKI_S_IWUSR|VKI_S_IRGRP|VKI_S_IROTH);
   if (!sr_isError(sres)) {
      Int fd = sr_Res(sres);
      sink->fsname_expanded = logfilename;
      sink->type = VgLogTo_File;
      return fd;
   } else {
      VG_(fmsg)("Cannot create %s file '%s': %s\n", 
                (is_xml) ? "XML" : "log", logfilename,
                VG_(strerror)(sr_Err(sres)));
      VG_(exit)(1);
      /*NOTREACHED*/
   }
}

static Int prepare_sink_socket(const HChar *clo_fname_unexpanded,
                               OutputSink *sink, Bool is_xml)
{
   vg_assert(clo_fname_unexpanded != NULL);
   vg_assert(VG_(strlen)(clo_fname_unexpanded) <= 900); /* paranoia */

   Int fd = VG_(connect_via_socket)(clo_fname_unexpanded);
   if (fd == -1) {
      VG_(fmsg)("Invalid %s spec of '%s'\n",
                (is_xml) ? "--xml-socket" : "--log-socket",
                clo_fname_unexpanded);
      VG_(exit)(1);
      /*NOTREACHED*/
   }
   if (fd == -2) {
      VG_(umsg)("Failed to connect to %slogging server '%s'.\n"
                "%s will be sent to stderr instead.\n",
                (is_xml) ? "XML " : "",
                clo_fname_unexpanded,
                (is_xml) ? "XML output" : "Logging messages");
      /* We don't change anything here. */
      vg_assert(sink->fd == 2);
      vg_assert(sink->type == VgLogTo_Fd);
      return 2;
   } else {
      vg_assert(fd > 0);
      sink->type = VgLogTo_Socket;
      return fd;
   }
}

static void finalize_sink_fd(OutputSink *sink, Int new_fd, Bool is_xml)
{
   // Move new_fd into the safe range, so it doesn't conflict with any app fds.
   Int safe_fd = VG_(fcntl)(new_fd, VKI_F_DUPFD, VG_(fd_hard_limit));
   if (safe_fd < 0) {
      VG_(message)(Vg_UserMsg, "Valgrind: failed to move %s file descriptor "
                               "into safe range, using stderr\n",
                               (is_xml) ? "XML" : "log");
      revert_sink_to_stderr(sink);
   } else {
      VG_(fcntl)(safe_fd, VKI_F_SETFD, VKI_FD_CLOEXEC);
      sink->fd = safe_fd;
      /* If we created the new_fd (VgLogTo_File or VgLogTo_Socket), then we
         don't need the original file descriptor open anymore. We only need
         to keep it open if it was an existing fd given by the user (or
         stderr).  */
      if (sink->type != VgLogTo_Fd)
         VG_(close)(new_fd);
   }
}

/* Re-opens an output file sink when exanded file name differs from what we
   have now. Returns 'True' if the sink was reopened  */
static Bool reopen_sink_if_needed(const HChar *clo_fname_unexpanded,
                                  OutputSink *sink, Bool is_xml)
{
   if (sink->type == VgLogTo_File) {
      /* Try to expand --log|xml-file again and see if it differs from what
         we have now. */
      HChar *logfilename = VG_(expand_file_name)(
                                         (is_xml) ? "--xml-file" : "--log-file",
                                         clo_fname_unexpanded);
      if (VG_(strcmp)(logfilename, sink->fsname_expanded) != 0) {
         Int fd = prepare_sink_fd(clo_fname_unexpanded, sink, is_xml);
         finalize_sink_fd(sink, fd, is_xml);
         return True;
      }
      VG_(free)(logfilename);
   }

   return False;
}

void VG_(logging_atfork_child)(ThreadId tid)
{
   /* If --child-silent-after-fork=yes was specified, set the output file
      descriptors to 'impossible' values. This is noticed by
      send_bytes_to_logging_sink(), which duly stops writing any further
      output. */
   if (VG_(clo_child_silent_after_fork)) {
      if (VG_(log_output_sink).type != VgLogTo_Socket) {
         VG_(log_output_sink).fd = -1;
         VG_(log_output_sink).type = VgLogTo_Fd;
      }
      if (VG_(xml_output_sink).type != VgLogTo_Socket) {
         VG_(xml_output_sink).fd = -1;
         VG_(xml_output_sink).type = VgLogTo_Fd;
      }
   } else {
      if (reopen_sink_if_needed(VG_(clo_log_fname_unexpanded),
                                &VG_(log_output_sink), False) ||
          reopen_sink_if_needed(VG_(clo_xml_fname_unexpanded),
                                &VG_(xml_output_sink), True)) {
         VG_(print_preamble)(VG_(log_output_sink).type != VgLogTo_File);
      }
   }
}

/* Initializes normal log and xml sinks (of type fd, file, or socket).
   Any problem encountered is considered a hard error and causes V. to exit.

   Comments on how the logging options are handled:

   User can specify:
      --log-fd=      for a fd to write to (default setting, fd = 2)
      --log-file=    for a file name to write to
      --log-socket=  for a socket to write to

   As a result of examining these and doing relevant socket/file
   opening, a final fd is established.  This is stored in
   VG_(log_output_sink) in m_libcprint.  Also, if --log-file=STR was
   specified, then it is stored in VG_(clo_log_fname_unexpanded), in m_options.
   And then STR, after expansion of %p and %q templates within
   it, is stored in VG_(log_output_sink), just in case anybody wants to know
   what it is.

   When printing, VG_(log_output_sink) is consulted to find the
   fd to send output to.

   Exactly analogous actions are undertaken for the XML output
   channel, with the one difference that the default fd is -1, meaning
   the channel is disabled by default. */
void VG_(init_log_xml_sinks)(VgLogTo log_to, VgLogTo xml_to,
                             Int /*initial*/log_fd, Int /*initial*/xml_fd)
{
   // VG_(clo_log_fd) is used by all the messaging.  It starts as 2 (stderr)
   // and we cannot change it until we know what we are changing it to is ok.

   /* Start setting up logging now. After this is done, VG_(log_output_sink)
      and (if relevant) VG_(xml_output_sink) should be connected to whatever
      sink has been selected, and we indiscriminately chuck stuff into it
      without worrying what the nature of it is.
      Oh the wonder of Unix streams. */

   vg_assert(VG_(log_output_sink).fd == 2 /* stderr */);
   vg_assert(VG_(log_output_sink).type == VgLogTo_Fd);
   vg_assert(VG_(log_output_sink).fsname_expanded == NULL);

   vg_assert(VG_(xml_output_sink).fd == -1 /* disabled */);
   vg_assert(VG_(xml_output_sink).type == VgLogTo_Fd);
   vg_assert(VG_(xml_output_sink).fsname_expanded == NULL);

   /* --- set up the normal text output channel --- */
   switch (log_to) {
      case VgLogTo_Fd: 
         vg_assert(VG_(clo_log_fname_unexpanded) == NULL);
         break;

      case VgLogTo_File:
         log_fd = prepare_sink_fd(VG_(clo_log_fname_unexpanded),
                                  &VG_(log_output_sink), False);
         break;

      case VgLogTo_Socket:
         log_fd = prepare_sink_socket(VG_(clo_log_fname_unexpanded),
                                      &VG_(log_output_sink), False);
         break;
   }

   /* --- set up the XML output channel --- */
   switch (xml_to) {
      case VgLogTo_Fd: 
         vg_assert(VG_(clo_xml_fname_unexpanded) == NULL);
         break;

      case VgLogTo_File:
         xml_fd = prepare_sink_fd(VG_(clo_xml_fname_unexpanded),
                                  &VG_(xml_output_sink), True);
         break;

      case VgLogTo_Socket:
         xml_fd = prepare_sink_socket(VG_(clo_xml_fname_unexpanded),
                                      &VG_(xml_output_sink), True);
         break;
   }

   /* If we've got this far, and XML mode was requested, but no XML
      output channel appears to have been specified, just stop.  We
      could continue, and XML output will simply vanish into nowhere,
      but that is likely to confuse the hell out of users, which is
      distinctly Ungood. */
   if (VG_(clo_xml) && xml_fd == -1) {
      VG_(fmsg_bad_option)(
          "--xml=yes, but no XML destination specified",
          "--xml=yes has been specified, but there is no XML output\n"
          "destination.  You must specify an XML output destination\n"
          "using --xml-fd, --xml-file or --xml-socket.\n"
      );
   }

   // Finalise the output fds: the log fd ..
   if (log_fd >= 0) {
      finalize_sink_fd(&VG_(log_output_sink), log_fd, False);
   } else {
      // If they said --log-fd=-1, don't print anything.  Plausible for use in
      // regression testing suites that use client requests to count errors.
      VG_(log_output_sink).fd = -1;
      VG_(log_output_sink).type = VgLogTo_Fd;
   }

   // Finalise the output fds: and the XML fd ..
   if (xml_fd >= 0) {
      finalize_sink_fd(&VG_(xml_output_sink), xml_fd, True);
   } else {
      // If they said --xml-fd=-1, don't print anything.  Plausible for use in
      // regression testing suites that use client requests to count errors.
      VG_(xml_output_sink).fd = -1;
      VG_(xml_output_sink).type = VgLogTo_Fd;
   }
}

/* Do the low-level send of a message to the logging sink. */
static
void send_bytes_to_logging_sink ( OutputSink* sink, const HChar* msg, Int nbytes )
{
   if (sink->type == VgLogTo_Socket) {
      Int rc = VG_(write_socket)( sink->fd, msg, nbytes );
      if (rc == -1) {
         // For example, the listener process died.  Switch back to stderr.
         revert_sink_to_stderr(sink);
         VG_(write)( sink->fd, msg, nbytes );
      }
   } else {
      /* sink->fd could have been set to -1 in the various
         sys-wrappers for sys_fork, if --child-silent-after-fork=yes
         is in effect.  That is a signal that we should not produce
         any more output. */
      if (sink->fd >= 0)
         VG_(write)( sink->fd, msg, nbytes );
      else if (sink->fd == -2 && nbytes > 0)
         /* send to gdb the provided data, which must be
            a null terminated string with len >= 1 */
         VG_(gdb_printf)("%s", msg);
   }
}


/* ---------------------------------------------------------------------
   printf() and friends
   ------------------------------------------------------------------ */

/* --------- printf --------- */

typedef 
   struct {
      HChar       buf[512];
      Int         buf_used;
      OutputSink* sink;
   } 
   printf_buf_t;

// Adds a single char to the buffer.  When the buffer gets sufficiently
// full, we write its contents to the logging sink.
static void add_to__printf_buf ( HChar c, void *p )
{
   printf_buf_t *b = (printf_buf_t *)p;
   
   if (b->buf_used > sizeof(b->buf) - 2 ) {
      send_bytes_to_logging_sink( b->sink, b->buf, b->buf_used );
      b->buf_used = 0;
   }
   b->buf[b->buf_used++] = c;
   b->buf[b->buf_used]   = 0;
   vg_assert(b->buf_used < sizeof(b->buf));
}

static UInt vprintf_to_buf ( printf_buf_t* b,
                             const HChar *format, va_list vargs )
{
   UInt ret = 0;
   if (b->sink->fd >= 0 || b->sink->fd == -2) {
      ret = VG_(debugLog_vprintf) 
               ( add_to__printf_buf, b, format, vargs );
   }
   return ret;
}

static UInt vprintf_WRK ( OutputSink* sink,
                          const HChar *format, va_list vargs )
{
   printf_buf_t myprintf_buf
      = { "", 0, sink };
   UInt ret
      = vprintf_to_buf(&myprintf_buf, format, vargs);
   // Write out any chars left in the buffer.
   if (myprintf_buf.buf_used > 0) {
      send_bytes_to_logging_sink( myprintf_buf.sink,
                                  myprintf_buf.buf,
                                  myprintf_buf.buf_used );
   }
   return ret;
}

UInt VG_(vprintf) ( const HChar *format, va_list vargs )
{
   return vprintf_WRK( &VG_(log_output_sink), format, vargs );
}

UInt VG_(printf) ( const HChar *format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs, format);
   ret = VG_(vprintf)(format, vargs);
   va_end(vargs);
   return ret;
}

UInt VG_(vprintf_xml) ( const HChar *format, va_list vargs )
{
   return vprintf_WRK( &VG_(xml_output_sink), format, vargs );
}

UInt VG_(printf_xml) ( const HChar *format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs, format);
   ret = VG_(vprintf_xml)(format, vargs);
   va_end(vargs);
   return ret;
}

static UInt emit_WRK ( const HChar* format, va_list vargs )
{
   if (VG_(clo_xml)) {
      return VG_(vprintf_xml)(format, vargs);
   } else if (VG_(log_output_sink).fd == -2) {
      return VG_(vprintf) (format, vargs);
   } else {
      return VG_(vmessage)(Vg_UserMsg, format, vargs);
   }
}
UInt VG_(emit) ( const HChar* format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs, format);
   ret = emit_WRK(format, vargs);
   va_end(vargs);
   return ret;
}

/* --------- sprintf --------- */

/* If we had an explicit buf structure here, it would contain only one
   field, indicating where the next char is to go.  So use p directly
   for that, rather than having it be a pointer to a structure. */

static void add_to__sprintf_buf ( HChar c, void *p )
{
   HChar** b = p;
   *(*b)++ = c;
}

UInt VG_(vsprintf) ( HChar* buf, const HChar *format, va_list vargs )
{
   Int ret;
   HChar* sprintf_ptr = buf;

   ret = VG_(debugLog_vprintf) 
            ( add_to__sprintf_buf, &sprintf_ptr, format, vargs );
   add_to__sprintf_buf('\0', &sprintf_ptr);

   vg_assert(VG_(strlen)(buf) == ret);

   return ret;
}

UInt VG_(sprintf) ( HChar* buf, const HChar *format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs,format);
   ret = VG_(vsprintf)(buf, format, vargs);
   va_end(vargs);
   return ret;
}


/* --------- snprintf --------- */

/* The return value of VG_(snprintf) and VG_(vsnprintf) differs from
   what is defined in C99. Let S be the size of the buffer as given in
   the 2nd argument.
   Return value R:
     R < S:  The output string was successfully written to the buffer.
             It is null-terminated and R == strlen( output string )
     R == S: The supplied buffer was too small to hold the output string.
             The first S-1 characters of the output string were written
             to the buffer followed by the terminating null character.
*/

typedef 
   struct {
      HChar* buf;
      Int    buf_size;
      Int    buf_used;
   } 
   snprintf_buf_t;

static void add_to__snprintf_buf ( HChar c, void* p )
{
   snprintf_buf_t* b = p;
   if (b->buf_size > 0 && b->buf_used < b->buf_size) {
      b->buf[b->buf_used++] = c;
      if (b->buf_used < b->buf_size)
         b->buf[b->buf_used] = 0;
      else
         b->buf[b->buf_size-1] = 0; /* pre: b->buf_size > 0 */
   } 
}

UInt VG_(vsnprintf) ( HChar* buf, Int size, const HChar *format, va_list vargs )
{
   snprintf_buf_t b;
   b.buf      = buf;
   b.buf_size = size < 0 ? 0 : size;
   b.buf_used = 0;
   if (b.buf_size > 0)
      b.buf[0] = 0; // ensure to null terminate buf if empty format
   (void) VG_(debugLog_vprintf) 
             ( add_to__snprintf_buf, &b, format, vargs );

   return b.buf_used;
}

UInt VG_(snprintf) ( HChar* buf, Int size, const HChar *format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs,format);
   ret = VG_(vsnprintf)(buf, size, format, vargs);
   va_end(vargs);
   return ret;
}


/* --------- vcbprintf --------- */

void VG_(vcbprintf)( void(*char_sink)(HChar, void* opaque),
                     void* opaque,
                     const HChar* format, va_list vargs )
{
   (void) VG_(debugLog_vprintf)
             ( char_sink, opaque, format, vargs );
}


/* --------- fprintf ---------- */

/* This is like [v]fprintf, except it writes to a file handle using
   VG_(write). */

#define VGFILE_BUFSIZE  8192

struct _VgFile {
   HChar buf[VGFILE_BUFSIZE];
   UInt  num_chars;   // number of characters in buf
   Int   fd;          // file descriptor to write to
};


static void add_to__vgfile ( HChar c, void *p )
{
   VgFile *fp = p;

   fp->buf[fp->num_chars++] = c;

   if (fp->num_chars == VGFILE_BUFSIZE) {
      VG_(write)(fp->fd, fp->buf, fp->num_chars);
      fp->num_chars = 0;
   }
}

VgFile *VG_(fopen)(const HChar *name, Int flags, Int mode)
{
   SysRes res = VG_(open)(name, flags, mode);

   if (sr_isError(res))
      return NULL;

   VgFile *fp = VG_(malloc)("fopen", sizeof(VgFile));

   fp->fd = sr_Res(res);
   fp->num_chars = 0;

   return fp;
}


UInt VG_(vfprintf) ( VgFile *fp, const HChar *format, va_list vargs )
{
   return VG_(debugLog_vprintf)(add_to__vgfile, fp, format, vargs);
}

UInt VG_(fprintf) ( VgFile *fp, const HChar *format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs,format);
   ret = VG_(vfprintf)(fp, format, vargs);
   va_end(vargs);
   return ret;
}

void VG_(fclose)( VgFile *fp )
{
   // Flush the buffer.
   if (fp->num_chars)
      VG_(write)(fp->fd, fp->buf, fp->num_chars);

   VG_(close)(fp->fd);
   VG_(free)(fp);
}


/* ---------------------------------------------------------------------
   elapsed_wallclock_time()
   ------------------------------------------------------------------ */

/* Get the elapsed wallclock time since startup into buf, which must
   16 chars long.  This is unchecked.  It also relies on the
   millisecond timer having been set to zero by an initial read in
   m_main during startup. */

void VG_(elapsed_wallclock_time) ( /*OUT*/HChar* buf, SizeT bufsize )
{
   UInt t, ms, s, mins, hours, days;

   vg_assert(bufsize > 20);

   t  = VG_(read_millisecond_timer)(); /* milliseconds */

   ms = t % 1000;
   t /= 1000; /* now in seconds */

   s = t % 60;
   t /= 60; /* now in minutes */

   mins = t % 60;
   t /= 60; /* now in hours */

   hours = t % 24;
   t /= 24; /* now in days */

   days = t;

   VG_(sprintf)(buf, "%02u:%02u:%02u:%02u.%03u ", days, hours, mins, s, ms);
}


/* ---------------------------------------------------------------------
   message()
   ------------------------------------------------------------------ */

/* A buffer for accumulating VG_(message) style output.  This is
   pretty much the same as VG_(printf)'s scheme, with two differences:

   * The message buffer persists between calls, so that multiple
     calls to VG_(message) can build up output.

   * Whenever the first character on a line is emitted, the
     ==PID== style preamble is stuffed in before it.
*/
typedef 
   struct {
      HChar buf[512+128];
      Int   buf_used;
      Bool  atLeft; /* notionally, is the next char position at the
                       leftmost column? */
      /* Current message kind - changes from call to call */
      VgMsgKind kind;
      /* destination */
      OutputSink* sink;
   } 
   vmessage_buf_t;

static vmessage_buf_t vmessage_buf
   = { "", 0, True, Vg_UserMsg, &VG_(log_output_sink) };


// Adds a single char to the buffer.  We aim to have at least 128
// bytes free in the buffer, so that it's always possible to emit
// the preamble into the buffer if c happens to be the character
// following a \n.  When the buffer gets too full, we write its
// contents to the logging sink.
static void add_to__vmessage_buf ( HChar c, void *p )
{
   HChar tmp[64];
   vmessage_buf_t* b = (vmessage_buf_t*)p;

   vg_assert(b->buf_used >= 0 && b->buf_used < sizeof(b->buf)-128);

   if (UNLIKELY(b->atLeft)) {
      // insert preamble
      HChar ch;
      Int   i, depth;

      // Print one '>' in front of the messages for each level of
      // self-hosting being performed.
      // Do not print such '>' if sim hint "no-inner-prefix" given
      // (useful to run regression tests in an outer/inner setup
      // and avoid the diff failing due to these unexpected '>').
      depth = RUNNING_ON_VALGRIND;
      if (depth > 0 
          && !SimHintiS(SimHint_no_inner_prefix, VG_(clo_sim_hints))) {
         if (depth > 10)
            depth = 10; // ?!?!
         for (i = 0; i < depth; i++) {
            b->buf[b->buf_used++] = '>';
         }
      }

      if (Vg_FailMsg == b->kind) {
         // "valgrind: " prefix.
         b->buf[b->buf_used++] = 'v';
         b->buf[b->buf_used++] = 'a';
         b->buf[b->buf_used++] = 'l';
         b->buf[b->buf_used++] = 'g';
         b->buf[b->buf_used++] = 'r';
         b->buf[b->buf_used++] = 'i';
         b->buf[b->buf_used++] = 'n';
         b->buf[b->buf_used++] = 'd';
         b->buf[b->buf_used++] = ':';
         b->buf[b->buf_used++] = ' ';
      } else {
         switch (b->kind) {
            case Vg_UserMsg:       ch = '='; break;
            case Vg_DebugMsg:      ch = '-'; break;
            case Vg_ClientMsg:     ch = '*'; break;
            default:               ch = '?'; break;
         }

         b->buf[b->buf_used++] = ch;
         b->buf[b->buf_used++] = ch;

         if (VG_(clo_time_stamp)) {
            VG_(elapsed_wallclock_time)(tmp, sizeof tmp);
            for (i = 0; tmp[i]; i++)
               b->buf[b->buf_used++] = tmp[i];
         }

         VG_(sprintf)(tmp, "%d", VG_(getpid)());
         tmp[sizeof(tmp)-1] = 0;
         for (i = 0; tmp[i]; i++)
            b->buf[b->buf_used++] = tmp[i];

         b->buf[b->buf_used++] = ch;
         b->buf[b->buf_used++] = ch;
         b->buf[b->buf_used++] = ' ';
      }

      /* We can't possibly have stuffed 96 chars in merely as a result
         of making the preamble (can we?) */
      vg_assert(b->buf_used < sizeof(b->buf)-32);
   }

   b->buf[b->buf_used++] = c;
   b->buf[b->buf_used]   = 0;
   
   if (b->buf_used >= sizeof(b->buf) - 128) {
      send_bytes_to_logging_sink( b->sink, b->buf, b->buf_used );
      b->buf_used = 0;
   }

   b->atLeft = c == '\n';
}


UInt VG_(vmessage) ( VgMsgKind kind, const HChar* format, va_list vargs )
{
   UInt ret;

   /* Note (carefully) that the buf persists from call to call, unlike
      with the other printf variants in earlier parts of this file. */
   vmessage_buf_t* b = &vmessage_buf; /* shorthand for convenience */

   /* We have to set this each call, so that the correct flavour
      of preamble is emitted at each \n. */
   b->kind = kind;

   ret = VG_(debugLog_vprintf) ( add_to__vmessage_buf,
                                 b, format, vargs );

   /* If the message finished exactly with a \n, then flush it at this
      point.  If not, assume more bits of the same line will turn up
      in later messages, so don't bother to flush it right now. */

   if (b->atLeft && b->buf_used > 0) {
      send_bytes_to_logging_sink( b->sink, b->buf, b->buf_used );
      b->buf_used = 0;
   }

   return ret;
}

/* Send a simple single-part message. */
UInt VG_(message) ( VgMsgKind kind, const HChar* format, ... )
{
   UInt count;
   va_list vargs;
   va_start(vargs,format);
   count = VG_(vmessage) ( kind, format, vargs );
   va_end(vargs);
   return count;
}

static void revert_to_stderr ( void )
{
   revert_sink_to_stderr(&VG_(log_output_sink));
}

/* VG_(message) variants with hardwired first argument. */

UInt VG_(fmsg) ( const HChar* format, ... )
{
   UInt count;
   va_list vargs;
   va_start(vargs,format);
   count = VG_(vmessage) ( Vg_FailMsg, format, vargs );
   va_end(vargs);
   return count;
}

void VG_(fmsg_bad_option) ( const HChar* opt, const HChar* format, ... )
{
   va_list vargs;
   va_start(vargs,format);
   Bool fatal = VG_(Clo_Mode)() & cloEP;
   VgMsgKind mkind = fatal ? Vg_FailMsg : Vg_UserMsg;

   if (fatal)
      revert_to_stderr();
   VG_(message) (mkind, "Bad option: %s\n", opt);
   VG_(vmessage)(mkind, format, vargs );
   VG_(message) (mkind, "Use --help for more information or consult the user manual.\n");
   va_end(vargs);
   if (fatal)
      VG_(exit)(1);
}

void VG_(fmsg_unknown_option) ( const HChar* opt)
{
   Bool fatal = VG_(Clo_Mode)() & cloEP;
   VgMsgKind mkind = fatal ? Vg_FailMsg : Vg_UserMsg;

   if (fatal)
      revert_to_stderr();

   VG_(message) (mkind, "Unknown option: %s\n", opt);
   VG_(message) (mkind, "Use --help for more information or consult the user manual.\n");
   if (fatal)
      VG_(exit)(1);
}

UInt VG_(umsg) ( const HChar* format, ... )
{
   UInt count;
   va_list vargs;
   va_start(vargs,format);
   count = VG_(vmessage) ( Vg_UserMsg, format, vargs );
   va_end(vargs);
   return count;
}

UInt VG_(dmsg) ( const HChar* format, ... )
{
   UInt count;
   va_list vargs;
   va_start(vargs,format);
   count = VG_(vmessage) ( Vg_DebugMsg, format, vargs );
   va_end(vargs);
   return count;
}

/* Flush any output that has accumulated in vmessage_buf as a 
   result of previous calls to VG_(message) et al. */
void VG_(message_flush) ( void )
{
   vmessage_buf_t* b = &vmessage_buf;
   send_bytes_to_logging_sink( b->sink, b->buf, b->buf_used );
   b->buf_used = 0;
}

__attribute__((noreturn))
void VG_(err_missing_prog) ( void  )
{
   revert_to_stderr();
   VG_(fmsg)("no program specified\n");
   VG_(fmsg)("Use --help for more information.\n");
   VG_(exit)(1);
}

__attribute__((noreturn))
void VG_(err_config_error) ( const HChar* format, ... )
{
   va_list vargs;
   va_start(vargs,format);
   revert_to_stderr();
   VG_(message) (Vg_FailMsg, "Startup or configuration error:\n   ");
   VG_(vmessage)(Vg_FailMsg, format, vargs );
   VG_(message) (Vg_FailMsg, "Unable to start up properly.  Giving up.\n");
   va_end(vargs);
   VG_(exit)(1);
}

/* ---------------------------------------------------------------------
   VG_(sr_as_string)()
   ------------------------------------------------------------------ */

#if defined(VGO_linux) || defined(VGO_freebsd)
// FIXME: Does this function need to be adjusted for MIPS's _valEx ?
const HChar *VG_(sr_as_string) ( SysRes sr )
{
   static HChar buf[7+1+2+16+1+1];   // large enough

   if (sr_isError(sr))
      VG_(sprintf)(buf, "Failure(0x%" FMT_REGWORD "x)", (RegWord)sr_Err(sr));
   else
      VG_(sprintf)(buf, "Success(0x%" FMT_REGWORD "x)", (RegWord)sr_Res(sr));
   return buf;
}

#elif defined(VGO_darwin) || defined(VGO_solaris)

const HChar *VG_(sr_as_string) ( SysRes sr )
{
   static HChar buf[7+1+2+16+1+2+16+1+1];   // large enough

   if (sr_isError(sr))
      VG_(sprintf)(buf, "Failure(0x%lx)", sr_Err(sr));
   else
      VG_(sprintf)(buf, "Success(0x%lx:0x%lx)", sr_ResHI(sr), sr_Res(sr));
   return buf;
}

#else

#error unknown OS

#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
