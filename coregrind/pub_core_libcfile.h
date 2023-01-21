
/*--------------------------------------------------------------------*/
/*--- File/socket-related libc stuff.          pub_core_libcfile.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_LIBCFILE_H
#define __PUB_CORE_LIBCFILE_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the libc code that relates to
// files and sockets:  opening, reading, writing, etc.
// To use, you must first include: pub_core_vki.h
//--------------------------------------------------------------------

#include "pub_tool_libcfile.h"

/* Move an fd into the Valgrind-safe range */
extern Int VG_(safe_fd) ( Int oldfd );
extern Int VG_(fcntl)   ( Int fd, Int cmd, Addr arg );

/* Convert an fd into a filename */
extern Bool VG_(resolve_filename) ( Int fd, const HChar** buf );

#if defined(VGO_freebsd)
/* get the flags used to obtain an fd */
extern Bool VG_(resolve_filemode) ( Int fd, Int * result );
#endif

/* Return the size of a file, or -1 in case of error */
extern Long VG_(fsize) ( Int fd );

/* Lookup an extended attribute for a file */
extern SysRes VG_(getxattr) ( const HChar* file_name, const HChar* attr_name,
                              Addr attr_value, SizeT attr_value_len );

/* Is the file a directory? */
extern Bool VG_(is_dir) ( const HChar* f );

/* Default destination port to be used in logging over a network, if
   none specified. */
#define VG_CLO_DEFAULT_LOGPORT 1500

extern Int VG_(connect_via_socket)( const HChar* str );

extern UInt   VG_(htonl) ( UInt x );
extern UInt   VG_(ntohl) ( UInt x );
extern UShort VG_(htons) ( UShort x );
extern UShort VG_(ntohs) ( UShort x );

extern Int VG_(socket) ( Int domain, Int type, Int protocol );

extern Int VG_(write_socket)( Int sd, const void *msg, Int count );
extern Int VG_(getsockname) ( Int sd, struct vki_sockaddr *name, Int *namelen );
extern Int VG_(getpeername) ( Int sd, struct vki_sockaddr *name, Int *namelen );
extern Int VG_(getsockopt)  ( Int sd, Int level, Int optname, 
                              void *optval, Int *optlen );
extern Int VG_(setsockopt)  ( Int sd, Int level, Int optname,
                              void *optval, Int optlen );

extern Int VG_(access) ( const HChar* path, Bool irusr, Bool iwusr,
                                            Bool ixusr );

/* Is the file executable?  Returns: 0 = success, non-0 is failure */
extern Int VG_(check_executable)(/*OUT*/Bool* is_setuid,
                                 const HChar* f, Bool allow_setuid);

/* DDD: Note this moves (or at least, is believed to move) the file
   pointer on Linux but doesn't on Darwin.  This inconsistency should
   be fixed.  (In other words, why isn't the Linux version implemented
   in terms of pread()?) */
extern SysRes VG_(pread) ( Int fd, void* buf, Int count, OffT offset );

/* Size of fullname buffer needed for a call to VG_(mkstemp) with
   part_of_name having the given part_of_name_len. */
extern SizeT VG_(mkstemp_fullname_bufsz) ( SizeT part_of_name_len );

/* Create and open (-rw------) a tmp file name incorporating said arg.
   Returns -1 on failure, else the fd of the file.  The file name is
   written to the memory pointed to be fullname. The number of bytes written
   is equal to VG_(mkstemp_fullname_bufsz)(VG_(strlen)(part_of_name)). */
extern Int VG_(mkstemp) ( const HChar* part_of_name, /*OUT*/HChar* fullname );

/* Record the process' working directory at startup.  Is intended to
   be called exactly once, at startup, before the working directory
   changes.  The saved value can later be acquired by calling
   VG_(get_startup_wd) (in pub_tool_libcfile.h).  Note that might
   return if the working directory couldn't be found.  */
extern void VG_(record_startup_wd) ( void );

#if defined(VGO_freebsd)
extern Bool VG_(realpath)(const HChar *path, HChar *resolved);
#endif

#endif   // __PUB_CORE_LIBCFILE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
