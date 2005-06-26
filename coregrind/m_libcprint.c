
/*--------------------------------------------------------------------*/
/*--- Libc printing.                                 m_libcprint.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"   // For VG_(write)(), VG_(write_socket)()
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"   // For VG_(getpid)()
#include "pub_core_options.h"
#include "valgrind.h"            // For RUNNING_ON_VALGRIND

#include <time.h>
#include <sys/time.h>

/* ---------------------------------------------------------------------
   Writing to file or a socket
   ------------------------------------------------------------------ */

/* Tell the logging mechanism whether we are logging to a file
   descriptor or a socket descriptor. */
Bool VG_(logging_to_socket) = False;

/* Do the low-level send of a message to the logging sink. */
static void send_bytes_to_logging_sink ( Char* msg, Int nbytes )
{
   if (!VG_(logging_to_socket)) {
      VG_(write)( VG_(clo_log_fd), msg, nbytes );
   } else {
      Int rc = VG_(write_socket)( VG_(clo_log_fd), msg, nbytes );
      if (rc == -1) {
         // For example, the listener process died.  Switch back to stderr.
         VG_(logging_to_socket) = False;
         VG_(clo_log_fd) = 2;
         VG_(write)( VG_(clo_log_fd), msg, nbytes );
      }
   }
}

/* ---------------------------------------------------------------------
   printf() and friends
   ------------------------------------------------------------------ */

typedef struct {
   char buf[100];
   int n;
} printf_buf;

// Adds a single char to the buffer.  When the buffer gets sufficiently
// full, we write its contents to the logging sink.
static void add_to_myprintf_buf ( HChar c, void *p )
{
   printf_buf *myprintf_buf = (printf_buf *)p;
   
   if (myprintf_buf->n >= 100-10 /*paranoia*/ ) {
      send_bytes_to_logging_sink( myprintf_buf->buf, myprintf_buf->n );
      myprintf_buf->n = 0;
   }
   myprintf_buf->buf[myprintf_buf->n++] = c;
   myprintf_buf->buf[myprintf_buf->n]   = 0;
}

UInt VG_(vprintf) ( const HChar *format, va_list vargs )
{
   UInt ret = 0;
   printf_buf myprintf_buf = {"",0};

   if (VG_(clo_log_fd) >= 0) {
      ret = VG_(debugLog_vprintf) 
               ( add_to_myprintf_buf, &myprintf_buf, format, vargs );

      // Write out any chars left in the buffer.
      if (myprintf_buf.n > 0) {
         send_bytes_to_logging_sink( myprintf_buf.buf, myprintf_buf.n );
      }
   }
   return ret;
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

/* A general replacement for sprintf(). */
static void add_to_vg_sprintf_buf ( HChar c, void *p )
{
   char **vg_sprintf_ptr = p;
   *(*vg_sprintf_ptr)++ = c;
}

UInt VG_(vsprintf) ( Char* buf, const HChar *format, va_list vargs )
{
   Int ret;
   Char *vg_sprintf_ptr = buf;

   ret = VG_(debugLog_vprintf) 
            ( add_to_vg_sprintf_buf, &vg_sprintf_ptr, format, vargs );
   add_to_vg_sprintf_buf('\0', &vg_sprintf_ptr);

   vg_assert(VG_(strlen)(buf) == ret);

   return ret;
}

UInt VG_(sprintf) ( Char* buf, const HChar *format, ... )
{
   UInt ret;
   va_list vargs;

   va_start(vargs,format);
   ret = VG_(vsprintf)(buf, format, vargs);
   va_end(vargs);

   return ret;
}

/* ---------------------------------------------------------------------
   percentify()
   ------------------------------------------------------------------ */

// Percentify n/m with p decimal places.  Includes the '%' symbol at the end.
void VG_(percentify)(UInt n, UInt m, UInt d, Int n_buf, char buf[]) 
{
   Int i, len, space;

   ULong p1 = (100*n) / m;
    
   if (d == 0) {
      VG_(sprintf)(buf, "%lld%%", p1);
   } else {
      ULong p2;
      UInt  ex;
      Char fmt[32];
      switch (d) {
      case 1: ex = 10;    break;
      case 2: ex = 100;   break;
      case 3: ex = 1000;  break;
      default: VG_(tool_panic)("Currently can only handle 3 decimal places");
      }
      p2 = ((100*n*ex) / m) % ex;
      // Have to generate the format string in order to be flexible about
      // the width of the post-decimal-point part.
      VG_(sprintf)(fmt, "%%lld.%%0%dlld%%%%", d);
      // fmt is now "%lld.%0<d>lld%%" where <d> is 1,2,3...
      VG_(sprintf)(buf, fmt, p1, p2);
   }

   len = VG_(strlen)(buf);
   space = n_buf - len;
   if (space < 0) space = 0;     /* Allow for v. small field_width */
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  buf[i + space] = buf[i];
   for (i = 0; i < space; i++)  buf[i] = ' ';
}

/* ---------------------------------------------------------------------
   message()
   ------------------------------------------------------------------ */

UInt VG_(vmessage) ( VgMsgKind kind, const HChar* format, va_list vargs )
{
   UInt  count = 0;
   Char  c;
   const Char* pfx_s;
   static const Char pfx[] = ">>>>>>>>>>>>>>>>";

   switch (kind) {
      case Vg_UserMsg:       c = '='; break;
      case Vg_DebugMsg:      c = '-'; break;
      case Vg_DebugExtraMsg: c = '+'; break;
      case Vg_ClientMsg:     c = '*'; break;
      default:               c = '?'; break;
   }

   // The pfx trick prints one or more '>' characters in front of the
   // messages when running Valgrind under Valgrind, one per level of
   // self-hosting.
   pfx_s = &pfx[sizeof(pfx)-1-RUNNING_ON_VALGRIND],

   // Print the message
   count = 0;

   if (!VG_(clo_xml))
      count += VG_(printf) ("%s%c%c", pfx_s, c,c);

   if (VG_(clo_time_stamp)) {
      struct timeval tv;
      struct tm tm;
     
      if ( gettimeofday( &tv, NULL ) == 0 &&
           localtime_r( &tv.tv_sec, &tm ) == &tm )
      {
         count +=
            VG_(printf)( "%04d-%02d-%02d %02d:%02d:%02d.%03d ",
                         tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
                         tm.tm_hour, tm.tm_min, tm.tm_sec, tv.tv_usec / 1000 );
      }
   }

   if (!VG_(clo_xml))
      count += VG_(printf) ("%d%c%c ", VG_(getpid)(), c,c);

   count += VG_(vprintf)(format, vargs);
   count += VG_(printf) ("\n");
   return count;
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

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

