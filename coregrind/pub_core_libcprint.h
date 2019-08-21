
/*--------------------------------------------------------------------*/
/*--- Printing libc stuff.                    pub_core_libcprint.h ---*/
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

#ifndef __PUB_CORE_LIBCPRINT_H
#define __PUB_CORE_LIBCPRINT_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the libc code that is related to
// higher-level (ie. higher than DebugLog) printing, eg. VG_(printf)().
//--------------------------------------------------------------------

#include "pub_tool_libcprint.h"

typedef
   enum {
      VgLogTo_Fd,
      VgLogTo_File,
      VgLogTo_Socket
   }
   VgLogTo;

/* An output file descriptor wrapped up with its type and expanded name. */
typedef
   struct {
      Int fd;
      VgLogTo type;
      HChar *fsname_expanded; // 'fs' stands for file or socket
   }
   OutputSink;
 
/* And the destinations for normal and XML output. */
extern OutputSink VG_(log_output_sink);
extern OutputSink VG_(xml_output_sink);

/* Initializes normal log and xml sinks (of type fd, file, or socket).
   Any problem encountered is considered a hard error and causes V. to exit. */
extern void VG_(init_log_xml_sinks)(VgLogTo log_to, VgLogTo xml_to,
                                  Int /*initial*/log_fd, Int /*initial*/xml_fd);

extern void VG_(print_preamble)(Bool logging_to_fd);

extern void VG_(logging_atfork_child)(ThreadId tid);

/* Get the elapsed wallclock time since startup into buf which has size
   bufsize. The function will assert if bufsize is not large enough.
   Upon return, buf will contain the zero-terminated wallclock time as
   a string. The function also relies on the
   millisecond timer having been set to zero by an initial read in
   m_main during startup. */
void VG_(elapsed_wallclock_time) ( /*OUT*/HChar* buf, SizeT bufsize );

/* Call this if the executable is missing.  This function prints an
   error message, then shuts down the entire system. */
__attribute__((noreturn))
extern void VG_(err_missing_prog) ( void );

/* Similarly - complain and stop if there is some kind of config
   error. */
__attribute__((noreturn))
extern void VG_(err_config_error) ( const HChar* format, ... );

/* Called by main_process_cmd_line_options to indicate an unrecognised
   command line option.
   It shutdowns the system if Clo_Mode is cloE or cloP.*/
extern void VG_(fmsg_unknown_option) ( const HChar *opt );

#endif   // __PUB_CORE_LIBCPRINT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
