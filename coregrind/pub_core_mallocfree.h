
/*--------------------------------------------------------------------*/
/*--- High-level memory management.          pub_core_mallocfree.h ---*/
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

#ifndef __PUB_CORE_MALLOCFREE_H
#define __PUB_CORE_MALLOCFREE_H

#include "pub_tool_mallocfree.h"

//--------------------------------------------------------------------
// PURPOSE: high-level memory allocation (malloc/free), for the core and
// tools.
//--------------------------------------------------------------------

/* Allocation arenas.  

      CORE      for the core's and tools' general use.
      DINFO     for debug info (symbols, line #s, CFI, etc) storage.
      CLIENT    for the client's mallocs/frees, if the tool replaces glibc's
                    malloc() et al -- redzone size is chosen by the tool.
      DEMANGLE  for the C++ demangler.
      TTAUX     for storing TT/TC auxiliary structures (address range
                equivalence classes).

   When adding a new arena, remember also to add it to ensure_mm_init(). 
*/
typedef Int ArenaId;

#define VG_N_ARENAS        5

#define VG_AR_CORE         0
#define VG_AR_DINFO        1
#define VG_AR_CLIENT       2
#define VG_AR_DEMANGLE     3
#define VG_AR_TTAUX        4

// This is both the minimum payload size of a malloc'd block, and its
// minimum alignment.  Must be a power of 2 greater than 4, and should be
// greater than 8.
#if   defined(VGP_x86_linux)    || \
      defined(VGP_arm_linux)    || \
      defined(VGP_mips32_linux) || \
      (defined(VGP_mips64_linux) && defined(VGABI_N32)) || \
      defined(VGP_nanomips_linux) || \
      defined(VGP_x86_solaris)
#  define VG_MIN_MALLOC_SZB        8
// Nb: We always use 16 bytes for Darwin, even on 32-bits, so it can be used
// for any AltiVec- or SSE-related type.  This matches the Darwin libc.
// Also, use 16 bytes for any PPC variant, since 16 is required to make
// Altiveccery work right.
#elif defined(VGP_amd64_linux)    || \
      defined(VGP_ppc32_linux)    || \
      defined(VGP_ppc64be_linux)  || \
      defined(VGP_ppc64le_linux)  || \
      defined(VGP_s390x_linux)    || \
      (defined(VGP_mips64_linux) && !defined(VGABI_N32)) || \
      defined(VGP_x86_freebsd)    || \
      defined(VGP_amd64_freebsd)  || \
      defined(VGP_x86_darwin)     || \
      defined(VGP_amd64_darwin)   || \
      defined(VGP_arm64_linux)    || \
      defined(VGP_amd64_solaris)
#  define VG_MIN_MALLOC_SZB       16
#else
#  error Unknown platform
#endif

/* This struct definition MUST match the system one. */
/* SVID2/XPG mallinfo structure */
struct vg_mallinfo {
   int arena;    /* total space allocated from system */
   int ordblks;  /* number of non-inuse chunks */
   int smblks;   /* unused -- always zero */
   int hblks;    /* number of mmapped regions */
   int hblkhd;   /* total space in mmapped regions */
   int usmblks;  /* unused -- always zero */
   int fsmblks;  /* unused -- always zero */
   int uordblks; /* total allocated space */
   int fordblks; /* total non-inuse space */
   int keepcost; /* top-most, releasable (via malloc_trim) space */
};

extern void* VG_(arena_malloc)  ( ArenaId arena, const HChar* cc, SizeT nbytes );
extern void  VG_(arena_free)    ( ArenaId arena, void* ptr );
extern void* VG_(arena_calloc)  ( ArenaId arena, const HChar* cc,
                                  SizeT nmemb, SizeT bytes_per_memb );
extern void* VG_(arena_realloc) ( ArenaId arena, const HChar* cc,
                                  void* ptr, SizeT size );
extern void* VG_(arena_memalign)( ArenaId aid, const HChar* cc,
                                  SizeT req_alignB, SizeT req_pszB );
extern HChar* VG_(arena_strdup)  ( ArenaId aid, const HChar* cc, 
                                   const HChar* s);

/* Specialised version of realloc, that shrinks the size of the block ptr from
   its current size to req_pszB.
   req_pszB must be <= to the current size of ptr (otherwise it will assert).
   Compared to VG_(arena_realloc):
     * VG_(arena_realloc_shrink) cannot increase the size of ptr.
     * If large enough, the unused memory is made usable for other allocation.
     * ptr is shrunk in place, so as to avoid temporary allocation and memcpy. */
extern void VG_(arena_realloc_shrink) ( ArenaId aid,
                                        void* ptr, SizeT req_pszB);

extern SizeT VG_(arena_malloc_usable_size) ( ArenaId aid, void* payload );

extern SizeT VG_(arena_redzone_size) ( ArenaId aid );

extern void  VG_(mallinfo) ( ThreadId tid, struct vg_mallinfo* mi );

// VG_(arena_perm_malloc) is for permanent allocation of small blocks.
// See VG_(perm_malloc) in pub_tool_mallocfree.h for more details.
// Do not call any VG_(arena_*) functions with these permanent blocks.
extern void* VG_(arena_perm_malloc) ( ArenaId aid, SizeT nbytes, Int align );

extern void  VG_(sanity_check_malloc_all) ( void );

extern void  VG_(print_all_arena_stats) ( void );

extern void  VG_(print_arena_cc_analysis) ( void );

typedef 
   struct _AddrArenaInfo
   AddrArenaInfo;

struct _AddrArenaInfo {
   ArenaId aid;
   const HChar* name; // arena name, !NULL if Addr a points in an arena.
   SizeT       block_szB;
   PtrdiffT    rwoffset;
   Bool        free;  // True if this is in the arena free zone.
};
/* If Addr a points in one of the allocation arenas, describes Addr a in *aai
   otherwise sets *aai to 0/NULL/...
   Note that no information is produced for addresses allocated with
   VG_(arena_perm_malloc). */
extern void VG_(describe_arena_addr) ( Addr a, /*OUT*/AddrArenaInfo* aai );

#endif   // __PUB_CORE_MALLOCFREE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
