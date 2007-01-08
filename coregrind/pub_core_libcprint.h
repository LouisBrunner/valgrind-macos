
/*--------------------------------------------------------------------*/
/*--- Printing libc stuff.                    pub_core_libcprint.h ---*/
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

#ifndef __PUB_CORE_LIBCPRINT_H
#define __PUB_CORE_LIBCPRINT_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the libc code that is related to
// higher-level (ie. higher than DebugLog) printing, eg. VG_(printf)().
//--------------------------------------------------------------------

#include "pub_tool_libcprint.h"

/* Tell the logging mechanism whether we are logging to a file
   descriptor or a socket descriptor. */
extern Bool VG_(logging_to_socket);

/* Get the elapsed wallclock time since startup into buf, which must
   16 chars long.  This is unchecked.  It also relies on the
   millisecond timer having been set to zero by an initial read in
   m_main during startup. */
void VG_(elapsed_wallclock_time) ( /*OUT*/HChar* buf );

#endif   // __PUB_CORE_LIBCPRINT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
