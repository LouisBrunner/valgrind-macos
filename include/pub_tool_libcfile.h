
/*--------------------------------------------------------------------*/
/*--- File/socket-related libc stuff.          pub_tool_libcfile.h ---*/
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

#ifndef __PUB_TOOL_LIBCFILE_H
#define __PUB_TOOL_LIBCFILE_H

/* ---------------------------------------------------------------------
   File-related functions.
   ------------------------------------------------------------------ */

extern Int  VG_(open)   ( const Char* pathname, Int flags, Int mode );
extern void VG_(close)  ( Int fd );
extern Int  VG_(read)   ( Int fd, void* buf, Int count);
extern Int  VG_(write)  ( Int fd, const void* buf, Int count);
extern Int  VG_(pipe)   ( Int fd[2] );
extern OffT VG_(lseek)  ( Int fd, OffT offset, Int whence);

extern Int  VG_(stat)   ( Char* file_name, struct vki_stat* buf );
extern Int  VG_(fstat)  ( Int   fd,        struct vki_stat* buf );
extern Int  VG_(dup2)   ( Int oldfd, Int newfd );
extern Int  VG_(rename) ( Char* old_name, Char* new_name );
extern Int  VG_(unlink) ( Char* file_name );

extern Char* VG_(getcwd) ( Char* buf, SizeT size );

/* Easier to use than VG_(getcwd)() -- does the buffer fiddling itself.
   String put into 'cwd' is VG_(malloc)'d, and should be VG_(free)'d.
   Returns False if it fails.  Will fail if the pathname is > 65535 bytes. */
extern Bool VG_(getcwd_alloc) ( Char** cwd );

extern Int  VG_(readlink)( Char* path, Char* buf, UInt bufsize );
extern Int  VG_(getdents)( UInt fd, struct vki_dirent *dirp, UInt count );

#endif   // __PUB_TOOL_LIBCFILE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
