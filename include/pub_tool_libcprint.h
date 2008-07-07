
/*--------------------------------------------------------------------*/
/*--- Printing libc stuff.                    pub_tool_libcprint.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2008 Julian Seward
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


/* Enable compile-time format string checking by gcc.
   This feature is supported since at least gcc version 2.95.
   For more information about the format attribute, see also
   http://gcc.gnu.org/onlinedocs/gcc-4.3.0/gcc/Function-Attributes.html.
 */

#if defined(__GNUC__)
#define PRINTF_CHECK(x, y) __attribute__((format(__printf__, x, y)))
#else
#define PRINTF_CHECK(x, y)
#endif


/* ---------------------------------------------------------------------
   Basic printing
   ------------------------------------------------------------------ */

/* Note that they all output to the file descriptor given by the
 * --log-fd/--log-file/--log-socket argument, which defaults to 2 (stderr).
 * Hence no need for VG_(fprintf)().
 */
extern UInt VG_(printf)   ( const HChar *format, ... ) PRINTF_CHECK(1, 2);
extern UInt VG_(vprintf)  ( const HChar *format, va_list vargs ) PRINTF_CHECK(1, 0);
extern UInt VG_(sprintf)  ( Char* buf, const HChar* format, ... ) PRINTF_CHECK(2, 3);
extern UInt VG_(vsprintf) ( Char* buf, const HChar* format, va_list vargs ) PRINTF_CHECK(2, 0);
extern UInt VG_(snprintf) ( Char* buf, Int size, 
                                       const HChar *format, ... ) PRINTF_CHECK(3, 4);
extern UInt VG_(vsnprintf)( Char* buf, Int size, 
                                       const HChar *format, va_list vargs ) PRINTF_CHECK(3, 0);

// Percentify n/m with d decimal places.  Includes the '%' symbol at the end.
// Right justifies in 'buf'.
extern void VG_(percentify)(ULong n, ULong m, UInt d, Int n_buf, char buf[]);

/* ---------------------------------------------------------------------
   Messages for the user
   ------------------------------------------------------------------ */

/* No, really.  I _am_ that strange. */
#define OINK(nnn) VG_(message)(Vg_DebugMsg, "OINK %d",nnn)

/* Print a message prefixed by "??<pid>?? "; '?' depends on the VgMsgKind.
   Should be used for all user output. */

typedef
   enum { Vg_UserMsg,         /* '?' == '=' */
          Vg_DebugMsg,        /* '?' == '-' */
          Vg_DebugExtraMsg,   /* '?' == '+' */
          Vg_ClientMsg        /* '?' == '*' */
   }
   VgMsgKind;

/* Send a single-part message.  Appends a newline. The format
   specification may contain any ISO C format specifier or %t.
   No attempt is made to let the compiler verify consistency of the
   format string and the argument list. */
extern UInt VG_(message_no_f_c)( VgMsgKind kind, const HChar* format, ... );
/* Send a single-part message.  Appends a newline. The format
   specification may contain any ISO C format specifier. The gcc compiler
   will verify consistency of the format string and the argument list. */
extern UInt VG_(message)( VgMsgKind kind, const HChar* format, ... )
  PRINTF_CHECK(2, 3);

extern UInt VG_(vmessage)( VgMsgKind kind, const HChar* format, va_list vargs )
  PRINTF_CHECK(2, 0);



#endif   // __PUB_TOOL_LIBCPRINT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
