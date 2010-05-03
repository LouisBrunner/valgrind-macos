
/*--------------------------------------------------------------------*/
/*--- Module-local header file for m_aspacemgr.                    ---*/
/*---                                             priv_aspacemgr.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2010 OpenWorks LLP
      info@open-works.co.uk

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

#ifndef __PRIV_ASPACEMGR_H
#define __PRIV_ASPACEMGR_H

/* One of the important design goals of the address space manager is
   to minimise dependence on other modules.  Hence the following
   minimal set of imports. */

#include "pub_core_basics.h"     // types
#include "pub_core_vkiscnums.h"  // system call numbers
#include "pub_core_vki.h"        // VKI_PAGE_SIZE, VKI_MREMAP_MAYMOVE,
                                 // VKI_MREMAP_FIXED, vki_stat64

#include "pub_core_debuglog.h"   // VG_(debugLog)

#include "pub_core_libcbase.h"   // VG_(strlen), VG_(strcmp), VG_(strncpy)
                                 // VG_IS_PAGE_ALIGNED
                                 // VG_PGROUNDDN, VG_PGROUNDUP

#include "pub_core_syscall.h"    // VG_(do_syscallN)
                                 // VG_(mk_SysRes_Error)
                                 // VG_(mk_SysRes_Success)

#include "pub_core_options.h"    // VG_(clo_sanity_level)

#include "pub_core_aspacemgr.h"  // self


/* --------------- Implemented in aspacemgr-common.c ---------------*/

/* Simple assert-like, file I/O and syscall facilities, which avoid
   dependence on m_libcassert, and hence on the entire module graph.
   This is important since most of the system itself depends on
   aspacem, so we have to do this to avoid a circular dependency. */

__attribute__ ((noreturn))
extern void   ML_(am_exit) ( Int status );
extern void   ML_(am_barf) ( HChar* what );
extern void   ML_(am_barf_toolow) ( HChar* what );

__attribute__ ((noreturn))
extern void   ML_(am_assert_fail) ( const HChar* expr,
                                    const Char* file,
                                    Int line, 
                                    const Char* fn );

#define aspacem_assert(expr)                              \
  ((void) ((expr) ? 0 :                                   \
           (ML_(am_assert_fail)(#expr,                    \
                                __FILE__, __LINE__,       \
                                __PRETTY_FUNCTION__))))

/* Dude, what's my process ID ? */
extern Int    ML_(am_getpid)( void );

/* A simple, self-contained sprintf implementation. */
extern UInt   ML_(am_sprintf) ( HChar* buf, const HChar *format, ... );

/* mmap et al wrappers */
/* wrapper for munmap */
extern SysRes ML_(am_do_munmap_NO_NOTIFY)(Addr start, SizeT length);

/* wrapper for the ghastly 'mremap' syscall */
extern SysRes ML_(am_do_extend_mapping_NO_NOTIFY)( 
                 Addr  old_addr, 
                 SizeT old_len,
                 SizeT new_len 
              );
/* ditto */
extern SysRes ML_(am_do_relocate_nooverlap_mapping_NO_NOTIFY)( 
                 Addr old_addr, Addr old_len, 
                 Addr new_addr, Addr new_len 
              );

/* There is also VG_(do_mmap_NO_NOTIFY), but that's not declared
   here (obviously). */

extern SysRes ML_(am_open)  ( const Char* pathname, Int flags, Int mode );
extern void   ML_(am_close) ( Int fd );
extern Int    ML_(am_read)  ( Int fd, void* buf, Int count);
extern Int    ML_(am_readlink) ( HChar* path, HChar* buf, UInt bufsiz );
extern Int    ML_(am_fcntl) ( Int fd, Int cmd, Addr arg );

/* Get the dev, inode and mode info for a file descriptor, if
   possible.  Returns True on success. */
extern
Bool ML_(am_get_fd_d_i_m)( Int fd, 
                           /*OUT*/ULong* dev, 
                           /*OUT*/ULong* ino, /*OUT*/UInt* mode );

extern
Bool ML_(am_resolve_filename) ( Int fd, /*OUT*/HChar* buf, Int nbuf );

/* ------ Implemented seperately in aspacemgr-{linux,aix5}.c ------ */

/* Do a sanity check (/proc/self/maps sync check) */
extern void ML_(am_do_sanity_check)( void );


#endif   // __PRIV_ASPACEMGR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
