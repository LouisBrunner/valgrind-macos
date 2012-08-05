
/*--------------------------------------------------------------------*/
/*--- File/socket-related libc stuff.          pub_tool_libcfile.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward
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

/* To use this file you must first include pub_tool_vki.h. */

/* Note that VG_(stat) and VG_(fstat) write to a 'struct vg_stat*' and
   not a 'struct vki_stat*' or a 'struct vki_stat64*'.  'struct
   vg_stat*' is not the same as either of the vki_ versions.  No
   specific vki_stat{,64} kernel structure will work and is
   consistently available on different architectures on Linux, so we
   have to use this 'struct vg_stat' impedance-matching type
   instead.

   Also note that the fieldnames aren't prefixed with "st_".  This is because
   st_atime et al are macros in sys/stat.h on Darwin, and using those names
   screws things up.
*/
struct vg_stat {
   ULong   dev;
   ULong   ino;
   ULong   nlink;
   UInt    mode;
   UInt    uid;
   UInt    gid;
   ULong   rdev;
   Long    size;
   ULong   blksize;
   ULong   blocks;
   ULong   atime;
   ULong   atime_nsec;
   ULong   mtime;
   ULong   mtime_nsec;
   ULong   ctime;
   ULong   ctime_nsec;
};

extern SysRes VG_(mknod)  ( const Char* pathname, Int mode, UWord dev );
extern SysRes VG_(open)   ( const Char* pathname, Int flags, Int mode );
/* fd_open words like the open(2) system call: 
   returns fd if success, -1 otherwise */
extern Int VG_(fd_open)  (const Char* pathname, Int flags, Int mode);
extern void   VG_(close)  ( Int fd );
extern Int    VG_(read)   ( Int fd, void* buf, Int count);
extern Int    VG_(write)  ( Int fd, const void* buf, Int count);
extern Int    VG_(pipe)   ( Int fd[2] );
extern Off64T VG_(lseek)  ( Int fd, Off64T offset, Int whence );

extern SysRes VG_(stat)   ( const Char* file_name, struct vg_stat* buf );
extern Int    VG_(fstat)  ( Int   fd,        struct vg_stat* buf );
extern SysRes VG_(dup)    ( Int oldfd );
extern SysRes VG_(dup2)   ( Int oldfd, Int newfd );
extern Int    VG_(rename) ( const Char* old_name, const Char* new_name );
extern Int    VG_(unlink) ( const Char* file_name );

extern Int    VG_(poll) (struct vki_pollfd *fds, Int nfds, Int timeout);

extern Int    VG_(readlink)( const Char* path, Char* buf, UInt bufsize );
extern Int    VG_(getdents)( Int fd, struct vki_dirent *dirp, UInt count );

extern Char*  VG_(basename)( const Char* path );
extern Char*  VG_(dirname) ( const Char* path );

/* Return the name of a directory for temporary files. */
extern const HChar* VG_(tmpdir)(void);

/* Copy the working directory at startup into buf[0 .. size-1], or return
   False if buf is too small. */
extern Bool VG_(get_startup_wd) ( Char* buf, SizeT size );

#endif   // __PUB_TOOL_LIBCFILE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
