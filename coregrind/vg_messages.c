
/*--------------------------------------------------------------------*/
/*--- For sending error/informative messages.                      ---*/
/*---                                                vg_messages.c ---*/
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


#include "core.h"

#include <time.h>
#include <sys/time.h>

/* Size of a buffer used for creating messages. */
#define M_MSGBUF 10000

static char mbuf[M_MSGBUF];
static int n_mbuf;

static void add_to_buf ( Char c, void *p )
{
  if (n_mbuf >= (M_MSGBUF-1)) return;
  mbuf[n_mbuf++] = c;
  mbuf[n_mbuf]   = 0;
}

static void add_timestamp ( Char *buf )
{
   struct timeval tv;
   struct tm tm;
  
   if ( gettimeofday( &tv, NULL ) == 0 &&
        localtime_r( &tv.tv_sec, &tm ) == &tm ) {
      VG_(sprintf)( buf, "%04d-%02d-%02d %02d:%02d:%02d.%03d ",
                    tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
                    tm.tm_hour, tm.tm_min, tm.tm_sec, tv.tv_usec / 1000 );
   }
   else {
      VG_(strcpy)( buf, "" );
   }
   
   return;
}

static int add_to_msg ( const Char *format, ... )
{
   int count;
   va_list vargs;
   va_start(vargs,format);
   count = VG_(vprintf) ( add_to_buf, format, vargs, 0 );
   va_end(vargs);
   return count;
}

static int start_msg ( VgMsgKind kind )
{
   Char ts[32];
   Char c;
   static const Char pfx[] = ">>>>>>>>>>>>>>>>";
   n_mbuf = 0;
   mbuf[n_mbuf] = 0;

   if (VG_(clo_time_stamp))
     add_timestamp(ts);
   else
     VG_(strcpy)(ts, "");
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
   return add_to_msg( "%s%c%c%s%d%c%c ", 
                      &pfx[sizeof(pfx)-1-RUNNING_ON_VALGRIND],
                      c,c, ts, VG_(getpid)(), c,c );
}

static 
int end_msg ( void )
{
   int count = 0;
   if (VG_(clo_log_fd) >= 0) {
      add_to_buf('\n',0);
      VG_(send_bytes_to_logging_sink) ( mbuf, VG_(strlen)(mbuf) );
      count = 1;
   }
   return count;
}

int VG_(vmessage) ( VgMsgKind kind, const Char* format, va_list vargs )
{
   int count;
   count = start_msg ( kind );
   count += VG_(vprintf) ( add_to_buf, format, vargs, 0 );
   count += end_msg();
   return count;
}

/* Send a simple single-part message. */
int VG_(message) ( VgMsgKind kind, const Char* format, ... )
{
   int count;
   va_list vargs;
   va_start(vargs,format);
   count = VG_(vmessage) ( kind, format, vargs );
   va_end(vargs);
   return count;
}

/* Do the low-level send of a message to the logging sink. */
void VG_(send_bytes_to_logging_sink) ( Char* msg, Int nbytes )
{
   Int rc;
   if (VG_(logging_to_filedes)) {
      VG_(write)( VG_(clo_log_fd), msg, nbytes );
   } else {
      rc = VG_(write_socket)( VG_(clo_log_fd), msg, nbytes );
      if (rc == -1) {
         /* for example, the listener process died.  Switch back to
            stderr. */
         VG_(logging_to_filedes) = True;
         VG_(clo_log_to) = VgLogTo_Fd;
         VG_(clo_log_fd) = 2;
         VG_(write)( VG_(clo_log_fd), msg, nbytes );
      }
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
