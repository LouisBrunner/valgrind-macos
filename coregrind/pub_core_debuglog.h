
/*--------------------------------------------------------------------*/
/*--- Debug (not-for-user) logging.            pub_core_debuglog.h ---*/
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

#ifndef __PUB_CORE_DEBUGLOG_H
#define __PUB_CORE_DEBUGLOG_H

//--------------------------------------------------------------------
// PURPOSE: This module provides a low-level debug logging facility
// that works through all the twists and turns of program startup.  Is
// is completely independent of everything, including all memory
// facilities, and emits the debug log on file descriptor 2 (stderr).
// This module is the first to be initialised at system startup.
//
// Because VG_(debugLog) does printf-style formatting, and because
// this module depends on NO OTHERS, this module contains Valgrind's
// vfprintf implementation too.
//--------------------------------------------------------------------

/* Gaaah!  We don't want glibc dependencies, but there is no easy,
   portable way to avoid using stdarg.h. */
#include <stdarg.h>

#include "pub_tool_basics.h"  /* For definition of VG_ macro */

/* There are no tool-visible exports from m_debuglog, hence no header
   file for it. */
/* #include "pub_tool_debuglog.h" */


/* Module startup. */
extern 
void VG_(debugLog_startup) ( Int level, HChar* who );


/* Whether %ps should escape XML metacharacters. */
extern void VG_(debugLog_setXml)(Bool xml);


/* Get the logging threshold level, as set by the most recent call to
   VG_(debugLog_startup), or zero if there have been no such calls so
   far. */
extern
Int VG_(debugLog_getLevel) ( void );


/* Send debugging output.  Nothing happens unless 'level' 
   does not exceed the logging threshold level. */
extern
void VG_(debugLog) ( Int level, const HChar* modulename,
                                const HChar* format, ... )
     __attribute__((format(__printf__, 3, 4)));


/* A simple vprintf().  For each emitted byte, (*send_fn) is called with
   that byte, and 'send_arg2' as its second param. */
extern
UInt VG_(debugLog_vprintf) ( 
        void (*send_fn)(HChar,void*),/* byte sink */
        void* send_arg2,             /* 2nd arg for byte sink */
        const HChar *format, 
        va_list vargs
     );


#endif   // __PUB_CORE_DEBUGLOG_H

/*--------------------------------------------------------------------*/
/*--- end                                      pub_core_debuglog.h ---*/
/*--------------------------------------------------------------------*/
