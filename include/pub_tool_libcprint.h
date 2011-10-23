
/*--------------------------------------------------------------------*/
/*--- Printing libc stuff.                    pub_tool_libcprint.h ---*/
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

#ifndef __PUB_TOOL_LIBCPRINT_H
#define __PUB_TOOL_LIBCPRINT_H

/* ---------------------------------------------------------------------
   Formatting functions
   ------------------------------------------------------------------ */

extern UInt VG_(sprintf)  ( Char* buf, const HChar* format, ... )
                          PRINTF_CHECK(2, 3);

extern UInt VG_(vsprintf) ( Char* buf, const HChar* format, va_list vargs )
                          PRINTF_CHECK(2, 0);

extern UInt VG_(snprintf) ( Char* buf, Int size, 
                                       const HChar *format, ... )
                          PRINTF_CHECK(3, 4);

extern UInt VG_(vsnprintf)( Char* buf, Int size, 
                                       const HChar *format, va_list vargs )
                          PRINTF_CHECK(3, 0);

// Percentify n/m with d decimal places.  Includes the '%' symbol at the end.
// Right justifies in 'buf'.
extern void VG_(percentify)(ULong n, ULong m, UInt d, Int n_buf, char buf[]);


/* ---------------------------------------------------------------------
   Output-printing functions
   ------------------------------------------------------------------ */

// Note that almost all output goes to the file descriptor given by the
// --log-fd/--log-file/--log-socket argument, which defaults to 2 (stderr).
// (Except that some text always goes to stdout/stderr at startup, and
// debugging messages always go to stderr.)  Hence no need for
// VG_(fprintf)().

/* No, really.  I _am_ that strange. */
#define OINK(nnn) VG_(message)(Vg_DebugMsg, "OINK %d\n",nnn)

/* Print a message with a prefix that depends on the VgMsgKind.
   Should be used for all user output. */

typedef
   enum {                 // Prefix
      Vg_FailMsg,         // "valgrind:"
      Vg_UserMsg,         // "==pid=="
      Vg_DebugMsg,        // "--pid--"
      Vg_ClientMsg        // "**pid**"
   }
   VgMsgKind;

// These print output that isn't prefixed with anything, and should be
// used in very few cases, such as printing usage messages.
extern UInt VG_(printf)   ( const HChar *format, ... )
                          PRINTF_CHECK(1, 2);
extern UInt VG_(vprintf)  ( const HChar *format, va_list vargs )
                          PRINTF_CHECK(1, 0);

extern UInt VG_(printf_xml)  ( const HChar *format, ... )
                             PRINTF_CHECK(1, 2);

extern UInt VG_(vprintf_xml) ( const HChar *format, va_list vargs )
                             PRINTF_CHECK(1, 0);

/* Yet another, totally general, version of vprintf, which hands all
   output bytes to CHAR_SINK, passing it OPAQUE as the second arg. */
extern void VG_(vcbprintf)( void(*char_sink)(HChar, void* opaque),
                            void* opaque,
                            const HChar* format, va_list vargs );

extern UInt VG_(message)( VgMsgKind kind, const HChar* format, ... )
   PRINTF_CHECK(2, 3);

extern UInt VG_(vmessage)( VgMsgKind kind, const HChar* format, va_list vargs )
   PRINTF_CHECK(2, 0);

// Short-cuts for VG_(message)().

// This is used for messages printed due to start-up failures that occur
// before the preamble is printed, eg. due a bad executable.
extern UInt VG_(fmsg)( const HChar* format, ... ) PRINTF_CHECK(1, 2);

// This is used if an option was bad for some reason.  Note: don't use it just
// because an option was unrecognised -- return 'False' from
// VG_(tdict).tool_process_cmd_line_option) to indicate that -- use it if eg.
// an option was given an inappropriate argument.  This function prints an
// error message, then shuts down the entire system.
__attribute__((noreturn))
extern void VG_(fmsg_bad_option) ( HChar* opt, const HChar* format, ... )
   PRINTF_CHECK(2, 3);

// This is used for messages that are interesting to the user:  info about
// their program (eg. preamble, tool error messages, postamble) or stuff they
// requested.
extern UInt VG_(umsg)( const HChar* format, ... ) PRINTF_CHECK(1, 2);

// This is used for debugging messages that are only of use to developers.
extern UInt VG_(dmsg)( const HChar* format, ... ) PRINTF_CHECK(1, 2);

/* Flush any output cached by previous calls to VG_(message) et al. */
extern void VG_(message_flush) ( void );

#endif   // __PUB_TOOL_LIBCPRINT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
