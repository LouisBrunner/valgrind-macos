
/*--------------------------------------------------------------------*/
/*--- For sending error/informative messages.                      ---*/
/*---                                                 vg_message.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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


#include "vg_include.h"


static char vg_mbuf[M_VG_MSGBUF];
static int vg_n_mbuf;

static void add_to_buf ( Char c )
{
  if (vg_n_mbuf >= (M_VG_MSGBUF-1)) return;
  vg_mbuf[vg_n_mbuf++] = c;
  vg_mbuf[vg_n_mbuf]   = 0;
}


/* Publically visible from here onwards. */

int
VG_(add_to_msg) ( Char *format, ... )
{
   int count;
   va_list vargs;
   va_start(vargs,format);
   count = VG_(vprintf) ( add_to_buf, format, vargs );
   va_end(vargs);
   return count;
}

int VG_(vmessage) ( VgMsgKind kind, Char* format, va_list vargs )
{
   int count;
   count = VG_(start_msg) ( kind );
   count += VG_(vprintf) ( add_to_buf, format, vargs );
   count += VG_(end_msg)();
   return count;
}

/* Send a simple single-part message. */
int VG_(message) ( VgMsgKind kind, Char* format, ... )
{
   int count;
   va_list vargs;
   va_start(vargs,format);
   count = VG_(vmessage) ( kind, format, vargs );
   va_end(vargs);
   return count;
}

int VG_(start_msg) ( VgMsgKind kind )
{
   Char c;
   vg_n_mbuf = 0;
   vg_mbuf[vg_n_mbuf] = 0;
   switch (kind) {
      case Vg_UserMsg:       c = '='; break;
      case Vg_DebugMsg:      c = '-'; break;
      case Vg_DebugExtraMsg: c = '+'; break;
      case Vg_ClientMsg:     c = '*'; break;
      default:               c = '?'; break;
   }
   return VG_(add_to_msg)( "%c%c%d%c%c ", 
                           c,c, VG_(getpid)(), c,c );
}


int VG_(end_msg) ( void )
{
   int count = 0;
   if (VG_(clo_logfile_fd) >= 0) {
      add_to_buf('\n');
      VG_(send_bytes_to_logging_sink) ( 
         vg_mbuf, VG_(strlen)(vg_mbuf) );
      count = 1;
   }
   return count;
}


/* Do the low-level send of a message to the logging sink. */
void VG_(send_bytes_to_logging_sink) ( Char* msg, Int nbytes )
{
   Int rc;
   if (VG_(logging_to_filedes)) {
      VG_(write)( VG_(clo_logfile_fd), msg, nbytes );
   } else {
      rc = VG_(write_socket)( VG_(clo_logfile_fd), msg, nbytes );
      if (rc == -1) {
         /* for example, the listener process died.  Switch back to
            stderr. */
         VG_(logging_to_filedes) = True;
         VG_(clo_log_to) = VgLogTo_Fd;
         VG_(clo_logfile_fd) = 2;
         VG_(write)( VG_(clo_logfile_fd), msg, nbytes );
      }
   }
}


void VG_(startup_logging) ( void )
{
}

void VG_(shutdown_logging) ( void )
{
}

/*--------------------------------------------------------------------*/
/*--- end                                             vg_message.c ---*/
/*--------------------------------------------------------------------*/
