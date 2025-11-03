
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
   published by the Free Software Foundation; either version 3 of the
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
#if   defined(VGP_arm_linux)    || \
      defined(VGP_mips32_linux) || \
      (defined(VGP_mips64_linux) && defined(VGABI_N32)) || \
      defined(VGP_nanomips_linux) || \
      defined(VGP_x86_solaris)
#  define VG_MIN_MALLOC_SZB        8
// Nb: We always use 16 bytes for Darwin, even on 32-bits, so it can be used
// for any AltiVec- or SSE-related type.  This matches the Darwin libc.
// Also, use 16 bytes for any PPC variant, since 16 is required to make
// Altiveccery work right.
#elif defined(VGP_x86_linux)    || \
      defined(VGP_amd64_linux)    || \
      defined(VGP_ppc32_linux)    || \
      defined(VGP_ppc64be_linux)  || \
      defined(VGP_ppc64le_linux)  || \
      defined(VGP_s390x_linux)    || \
      (defined(VGP_mips64_linux) && !defined(VGABI_N32)) || \
      defined(VGP_x86_freebsd)    || \
      defined(VGP_amd64_freebsd)  || \
      defined(VGP_arm64_freebsd)  || \
      defined(VGP_x86_darwin)     || \
      defined(VGP_amd64_darwin)   || \
      defined(VGP_arm64_darwin)   || \
      defined(VGP_arm64_linux)    || \
      defined(VGP_riscv64_linux)  || \
      defined(VGP_amd64_solaris)
#  define VG_MIN_MALLOC_SZB       16
#else
#  error Unknown platform
#endif

#if defined(VGO_linux)
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

/* This struct definition MUST match the system one. */
/* SVID2/XPG mallinfo structure */
struct vg_mallinfo2 {
   SizeT arena;    /* total space allocated from system */
   SizeT ordblks;  /* number of non-inuse chunks */
   SizeT smblks;   /* unused -- always zero */
   SizeT hblks;    /* number of mmapped regions */
   SizeT hblkhd;   /* total space in mmapped regions */
   SizeT usmblks;  /* unused -- always zero */
   SizeT fsmblks;  /* unused -- always zero */
   SizeT uordblks; /* total allocated space */
   SizeT fordblks; /* total non-inuse space */
   SizeT keepcost; /* top-most, releasable (via malloc_trim) space */
};
#elif defined(VGO_solaris)

struct vg_mallinfo  {
        unsigned long arena;    /* total space in arena */
        unsigned long ordblks;  /* number of ordinary blocks */
        unsigned long smblks;   /* number of small blocks */
        unsigned long hblks;    /* number of holding blocks */
        unsigned long hblkhd;   /* space in holding block headers */
        unsigned long usmblks;  /* space in small blocks in use */
        unsigned long fsmblks;  /* space in free small blocks */
        unsigned long uordblks; /* space in ordinary blocks in use */
        unsigned long fordblks; /* space in free ordinary blocks */
        unsigned long keepcost; /* cost of enabling keep option */
};

#endif

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

#if defined(VGO_linux) || defined(VGO_solaris)
extern void  VG_(mallinfo) ( ThreadId tid, struct vg_mallinfo* mi );
#endif
#if defined(VGO_linux)
extern void  VG_(mallinfo2) ( ThreadId tid, struct vg_mallinfo2* mi );
#endif

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

#if defined(VGP_arm64_darwin)
#include "pub_core_vki.h" // For VKI_PROT_*
#include "pub_core_vkiscnums.h" // system call numbers
#include "pub_core_syscall.h" // VG_(do_syscall3)
#include "pub_core_libcassert.h" // vg_assert
#include "pub_core_debuglog.h" // VG_(debugLog)

__attribute__((always_inline))
__inline__
void enable_thread_to_jit_write(Addr ptr, SizeT size, Bool enable) {
  VG_(debugLog)(6, "mmapjit", "enable_thread_to_jit_write(%#lx, %lu, %d)\n", ptr, size, enable);

  SysRes sres = VG_(do_syscall3)(__NR_mprotect, ptr, size,
    enable ? VKI_PROT_READ | VKI_PROT_WRITE
           : VKI_PROT_READ | VKI_PROT_EXEC
  );
  vg_assert2(!sr_isError(sres), "mprotect failed: %s (%d)", VG_(strerror)(sr_Res(sres)), sr_Res(sres));
}
#define ALLOW_RWX_WRITE(ptr, size) enable_thread_to_jit_write((ptr), (size), 1)
#define ALLOW_RWX_EXECUTE(ptr, size) enable_thread_to_jit_write((ptr), (size), 0)
#else
#define ALLOW_RWX_WRITE(...)
#define ALLOW_RWX_EXECUTE(...)
#endif

#endif   // __PUB_CORE_MALLOCFREE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
