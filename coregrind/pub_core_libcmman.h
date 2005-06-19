
/*--------------------------------------------------------------------*/
/*--- Memory management libc stuff.            pub_core_libcmman.h ---*/
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

#ifndef __PUB_CORE_LIBCMMAN_H
#define __PUB_CORE_LIBCMMAN_H

//--------------------------------------------------------------------
// PURPOSE: This module contains libc code related to low-level
// memory management, ie. mmap and friends.
//--------------------------------------------------------------------

#include "pub_tool_libcmman.h"

extern void* VG_(mmap)   ( void* start, SizeT length, UInt prot, UInt flags,
                           UInt sf_flags, UInt fd, OffT offset );
extern Int VG_(munmap)   ( void* start, SizeT length );
extern Int VG_(mprotect) ( void *start, SizeT length, UInt prot );

extern SysRes VG_(mmap_native)     ( void* start, SizeT length, UInt prot,
                                     UInt flags, UInt fd, OffT offset );
extern SysRes VG_(munmap_native)   ( void* start, SizeT length );
extern SysRes VG_(mprotect_native) ( void *start, SizeT length, UInt prot );

extern Addr VG_(get_memory_from_mmap_for_client)
               (Addr base, SizeT len, UInt prot, UInt flags);

#endif   // __PUB_CORE_LIBCMMAN_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
