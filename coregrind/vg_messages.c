
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

UInt VG_(vmessage) ( VgMsgKind kind, const Char* format, va_list vargs )
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

   count += VG_(printf) ("%d%c%c ", VG_(getpid)(), c,c);
   count += VG_(vprintf)(format, vargs);
   count += VG_(printf) ("\n");
   return count;
}

/* Send a simple single-part message. */
UInt VG_(message) ( VgMsgKind kind, const Char* format, ... )
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
