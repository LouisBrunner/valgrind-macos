
/*--------------------------------------------------------------------*/
/*--- Memory management libc stuff.                   m_libcmman.c ---*/
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
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcmman.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_syscall.h"
#include "vki_unistd.h"

SysRes VG_(mmap_native)(void *start, SizeT length, UInt prot, UInt flags,
                        UInt fd, OffT offset)
{
   SysRes res;
#  if defined(VGP_x86_linux)
   { 
      UWord args[6];
      args[0] = (UWord)start;
      args[1] = length;
      args[2] = prot;
      args[3] = flags;
      args[4] = fd;
      args[5] = offset;
      res = VG_(do_syscall1)(__NR_mmap, (UWord)args );
   }
#  elif defined(VGP_amd64_linux)
   res = VG_(do_syscall6)(__NR_mmap, (UWord)start, length, 
                         prot, flags, fd, offset);
#  else
#    error Unknown platform
#  endif
   return res;
}

/* Returns -1 on failure. */
void* VG_(mmap)( void* start, SizeT length,
                 UInt prot, UInt flags, UInt sf_flags, UInt fd, OffT offset)
{
   SysRes res;

   if (!(flags & VKI_MAP_FIXED)) {
      start = (void *)VG_(find_map_space)((Addr)start, length, !!(flags & VKI_MAP_CLIENT));

      flags |= VKI_MAP_FIXED;
   }
   if (start == 0)
      return (void *)-1;

   res = VG_(mmap_native)(start, length, prot, 
                          flags & ~(VKI_MAP_NOSYMS | VKI_MAP_CLIENT),
                          fd, offset);

   // Check it ended up in the right place.
   if (!res.isError) {
      if (flags & VKI_MAP_CLIENT) {
         vg_assert(VG_(client_base) <= res.val 
                   && res.val+length <= VG_(client_end));
      } else {
         vg_assert(VG_(valgrind_base) <= res.val 
                   && res.val+length-1 <= VG_(valgrind_last));
      }

      sf_flags |= SF_MMAP;
      if (  flags & VKI_MAP_FIXED)      sf_flags |= SF_FIXED;
      if (  flags & VKI_MAP_SHARED)     sf_flags |= SF_SHARED;
      if (!(flags & VKI_MAP_ANONYMOUS)) sf_flags |= SF_FILE;
      if (!(flags & VKI_MAP_CLIENT))    sf_flags |= SF_VALGRIND;
      if (  flags & VKI_MAP_NOSYMS)     sf_flags |= SF_NOSYMS;

      VG_(map_fd_segment)(res.val, length, prot, sf_flags, fd, offset, NULL);
   }

   return res.isError ? (void*)-1 : (void*)res.val;
}

static SysRes munmap_native(void *start, SizeT length)
{
   return VG_(do_syscall2)(__NR_munmap, (UWord)start, length );
}

/* Returns -1 on failure. */
Int VG_(munmap)( void* start, SizeT length )
{
   SysRes res = munmap_native(start, length);
   if (!res.isError) {
      VG_(unmap_range)((Addr)start, length);
      return 0;
   } else {
      return -1;
   }
}

SysRes VG_(mprotect_native)( void *start, SizeT length, UInt prot )
{
   return VG_(do_syscall3)(__NR_mprotect, (UWord)start, length, prot );
}

Int VG_(mprotect)( void *start, SizeT length, UInt prot )
{
   SysRes res = VG_(mprotect_native)(start, length, prot);
   if (!res.isError) {
      VG_(mprotect_range)((Addr)start, length, prot);
      return 0;
   } else {
      return -1;
   }
}

void* VG_(get_memory_from_mmap) ( SizeT nBytes, Char* who )
{
   static SizeT tot_alloc = 0;
   void* p;
   p = VG_(mmap)(0, nBytes,
                 VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC,
                 VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 0, -1, 0);

   if (p != ((void*)(-1))) {
      vg_assert((void*)VG_(valgrind_base) <= p && p <= (void*)VG_(valgrind_last));
      tot_alloc += nBytes;
      if (0)
         VG_(printf)(
            "get_memory_from_mmap: %llu tot, %llu req = %p .. %p, caller %s\n",
            (ULong)tot_alloc, (ULong)nBytes, p, ((char*)p) + nBytes - 1, who );
      return p;
   }

   VG_(printf)("\n");
   VG_(printf)("VG_(get_memory_from_mmap): %s's request for %llu bytes failed.\n",
               who, (ULong)nBytes);
   VG_(printf)("VG_(get_memory_from_mmap): %llu bytes already allocated.\n", 
               (ULong)tot_alloc);
   VG_(printf)("\n");
   VG_(printf)("Sorry.  You could try using a tool that uses less memory;\n");
   VG_(printf)("eg. addrcheck instead of memcheck.\n");
   VG_(printf)("\n");
   VG_(exit)(1);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

