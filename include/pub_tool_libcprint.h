
/*--------------------------------------------------------------------*/
/*--- Printing libc stuff.                    pub_tool_libcprint.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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

#ifndef __PUB_TOOL_LIBCPRINT_H
#define __PUB_TOOL_LIBCPRINT_H

/* ---------------------------------------------------------------------
   Basic printing
   ------------------------------------------------------------------ */

/* Note that they all output to the file descriptor given by the
 * --log-fd/--log-file/--log-socket argument, which defaults to 2 (stderr).
 * Hence no need for VG_(fprintf)().
 */
extern UInt VG_(printf)  ( const HChar *format, ... );
extern UInt VG_(vprintf) ( const HChar *format, va_list vargs );
/* too noisy ...  __attribute__ ((format (printf, 1, 2))) ; */
extern UInt VG_(sprintf) ( Char* buf, const HChar* format, ... );
extern UInt VG_(vsprintf)( Char* buf, const HChar* format, va_list vargs );

/* ---------------------------------------------------------------------
   Messages for the user
   ------------------------------------------------------------------ */

/* Print a message prefixed by "??<pid>?? "; '?' depends on the VgMsgKind.
   Should be used for all user output. */

typedef
   enum { Vg_UserMsg,         /* '?' == '=' */
          Vg_DebugMsg,        /* '?' == '-' */
          Vg_DebugExtraMsg,   /* '?' == '+' */
          Vg_ClientMsg        /* '?' == '*' */
   }
   VgMsgKind;

/* Send a single-part message.  Appends a newline. */
extern UInt VG_(message)    ( VgMsgKind kind, const HChar* format, ... );
extern UInt VG_(vmessage)   ( VgMsgKind kind, const HChar* format, va_list vargs );

#endif   // __PUB_TOOL_LIBCPRINT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
