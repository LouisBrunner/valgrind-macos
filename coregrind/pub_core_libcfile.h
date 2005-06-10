
/*--------------------------------------------------------------------*/
/*--- File/socket-related libc stuff.          pub_core_libcfile.h ---*/
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

#ifndef __PUB_CORE_LIBCFILE_H
#define __PUB_CORE_LIBCFILE_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the libc code that relates to
// files and sockets:  opening, reading, writing, etc.
//--------------------------------------------------------------------

#include "pub_tool_libcfile.h"

/* Application-visible file descriptor limits */
extern Int VG_(fd_soft_limit);
extern Int VG_(fd_hard_limit);

/* Move an fd into the Valgrind-safe range */
extern Int VG_(safe_fd)(Int oldfd);

extern Int VG_(write_socket)( Int sd, void *msg, Int count );
extern Int VG_(connect_via_socket)( UChar* str );
extern Int VG_(getsockname) ( Int sd, struct vki_sockaddr *name, Int *namelen );
extern Int VG_(getpeername) ( Int sd, struct vki_sockaddr *name, Int *namelen );
extern Int VG_(getsockopt)  ( Int sd, Int level, Int optname, void *optval,
                              Int *optlen );

#endif   // __PUB_CORE_LIBCFILE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
