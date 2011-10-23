
/*--------------------------------------------------------------------*/
/*--- Printing libc stuff.                    pub_core_libcprint.h ---*/
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

#ifndef __PUB_CORE_LIBCPRINT_H
#define __PUB_CORE_LIBCPRINT_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the libc code that is related to
// higher-level (ie. higher than DebugLog) printing, eg. VG_(printf)().
//--------------------------------------------------------------------

#include "pub_tool_libcprint.h"

/* An output file descriptor wrapped up with a Bool indicating whether
   or not the fd is a socket. */
typedef
   struct { Int fd; Bool is_socket; }
   OutputSink;
 
/* And the destinations for normal and XML output. */
extern OutputSink VG_(log_output_sink);
extern OutputSink VG_(xml_output_sink);

/* Get the elapsed wallclock time since startup into buf, which must
   16 chars long.  This is unchecked.  It also relies on the
   millisecond timer having been set to zero by an initial read in
   m_main during startup. */
void VG_(elapsed_wallclock_time) ( /*OUT*/HChar* buf );

/* Call this if the executable is missing.  This function prints an
   error message, then shuts down the entire system. */
__attribute__((noreturn))
extern void VG_(err_missing_prog) ( void );

/* Similarly - complain and stop if there is some kind of config
   error. */
__attribute__((noreturn))
extern void VG_(err_config_error) ( Char* format, ... );

#endif   // __PUB_CORE_LIBCPRINT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
