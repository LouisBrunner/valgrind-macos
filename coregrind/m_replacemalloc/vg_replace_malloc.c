
/*--------------------------------------------------------------------*/
/*--- Replacements for malloc() et al, which run on the simulated  ---*/
/*--- CPU.                                     vg_replace_malloc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Julian Seward 
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

/* ---------------------------------------------------------------------
   ALL THE CODE IN THIS FILE RUNS ON THE SIMULATED CPU.  
   
   These functions are drop-in replacements for malloc() and friends.
   They have global scope, but are not intended to be called directly.
   See pub_core_redir.h for the gory details.

   This file can be linked into the vg_preload_<tool>.so file for any tool
   that wishes to know about calls to malloc().  The tool must define all
   the functions that will be called via 'info'.

   It is called vg_replace_malloc.c because this filename appears in stack
   traces, so we want the name to be (hopefully!) meaningful to users.
   ------------------------------------------------------------------ */

#include "pub_core_basics.h"
#include "pub_core_vki.h"           // VKI_EINVAL, VKI_ENOMEM
#include "pub_core_clreq.h"         // for VALGRIND_INTERNAL_PRINTF,
                                    //   VALGRIND_NON_SIMD_CALL[12]
#include "pub_core_debuginfo.h"     // needed for pub_core_redir.h :(
#include "pub_core_mallocfree.h"    // for VG_MIN_MALLOC_SZB, VG_AR_CLIENT
#include "pub_core_redir.h"         // for VG_REPLACE_FUNCTION_*
#include "pub_core_replacemalloc.h"

/* --------- Some handy Z-encoded names. --------- */

/* --- Soname of the standard C library. --- */

#if defined(VGO_linux)
#  define  m_libc_soname     libcZdsoZa              // libc.so*
#elif defined(VGP_ppc32_aix5)
   /* AIX has both /usr/lib/libc.a and /usr/lib/libc_r.a. */
#  define  m_libc_soname     libcZaZdaZLshrZdoZR     // libc*.a(shr.o)
#elif defined(VGP_ppc64_aix5)
#  define  m_libc_soname     libcZaZdaZLshrZu64ZdoZR // libc*.a(shr_64.o)
#else
#  error "Unknown platform"
#endif

/* --- Soname of the GNU C++ library. --- */

#define  m_libstdcxx_soname  libstdcZpZpZa           // libstdc++*

/* --- Soname of XLC's C++ library. --- */

/* AIX: xlC's C++ runtime library is called libC.a, and the
   interesting symbols appear to be in ansicore_32.o or ansicore_64.o
   respectively. */
#if defined(VGP_ppc32_aix5)
#  define  m_libC_dot_a   libCZdaZLansicoreZu32ZdoZR // libC.a(ansicore_32.o)
#elif defined(VGP_ppc64_aix5)
#  define  m_libC_dot_a   libCZdaZLansicoreZu64ZdoZR // libC.a(ansicore_64.o)
#endif


/* 2 Apr 05: the Portland Group compiler, which uses cfront/ARM style
   mangling, could be supported properly by the redirects in this
   module.  Except we can't because it doesn't put its allocation
   functions in libpgc.so but instead hardwires them into the
   compilation unit holding main(), which makes them impossible to
   intercept directly.  Fortunately those fns seem to route everything
   through to malloc/free.

   mid-06: could be improved, since we can now intercept in the main
   executable too.
*/

extern void _exit(int);

/* Apparently it is necessary to make ourselves free of any dependency
   on memcpy() on ppc32-aix5; else programs linked with -brtl fail.
   memcpy() is used by gcc for a struct assignment in mallinfo()
   below.  Add the following conservative implementation (memmove,
   really). */
#if defined(VGO_aix5)
__attribute__((weak))
void *memcpy(void *destV, const void *srcV, unsigned long n)
{
   unsigned char* src = (unsigned char*)srcV;
   unsigned char* dest = (unsigned char*)destV;
   unsigned long  i;
   if (dest < src) {
      for (i = 0; i < n; i++)
         dest[i] = src[i];
   }
   if (dest > src) {
      for (i = n; i > 0; i--)
         dest[i-1] = src[i-1];
   }
   return dest;
}
#endif


/*------------------------------------------------------------*/
/*--- Replacing malloc() et al                             ---*/
/*------------------------------------------------------------*/

/* This struct is initially empty.  Before the first use of any of
   these functions, we make a client request which fills in the
   fields. 
*/
static struct vg_mallocfunc_info info;
static int init_done;

/* Startup hook - called as init section */
static void init(void) __attribute__((constructor));

#define MALLOC_TRACE(format, args...)  \
   if (info.clo_trace_malloc)          \
      VALGRIND_INTERNAL_PRINTF(format, ## args )

/* Below are new versions of malloc, __builtin_new, free, 
   __builtin_delete, calloc, realloc, memalign, and friends.

   None of these functions are called directly - they are not meant to
   be found by the dynamic linker.  But ALL client calls to malloc()
   and friends wind up here eventually.  They get called because
   vg_replace_malloc installs a bunch of code redirects which causes
   Valgrind to use these functions rather than the ones they're
   replacing.
*/


/*---------------------- malloc ----------------------*/

/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to allocate memory.  If that fails, return NULL.
*/
#define ALLOC_or_NULL(soname, fnname, vg_replacement) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (SizeT n); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (SizeT n)  \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE(#fnname "(%llu)", (ULong)n ); \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, n ); \
      MALLOC_TRACE(" = %p", v ); \
      return v; \
   }


/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to allocate memory.  If that fails, it bombs the
   system.
*/
#define ALLOC_or_BOMB(soname, fnname, vg_replacement)  \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (SizeT n); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (SizeT n)  \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE(#fnname "(%llu)", (ULong)n );        \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, n ); \
      MALLOC_TRACE(" = %p", v ); \
      if (NULL == v) { \
         VALGRIND_PRINTF_BACKTRACE( \
            "new/new[] failed and should throw an exception, but Valgrind\n" \
            "   cannot throw exceptions and so is aborting instead.  Sorry."); \
            _exit(1); \
      } \
      return v; \
   }

// Each of these lines generates a replacement function:
//     (from_so, from_fn,  v's replacement)

// malloc
ALLOC_or_NULL(m_libstdcxx_soname, malloc,      malloc);
ALLOC_or_NULL(m_libc_soname,      malloc,      malloc);


/*---------------------- new ----------------------*/

// operator new(unsigned int), not mangled (for gcc 2.96)
ALLOC_or_BOMB(m_libcstdcxx_soname, builtin_new,    __builtin_new);
ALLOC_or_BOMB(m_libc_soname,       builtin_new,    __builtin_new);

ALLOC_or_BOMB(m_libcstdcxx_soname, __builtin_new,  __builtin_new);
ALLOC_or_BOMB(m_libc_soname,       __builtin_new,  __builtin_new);

// operator new(unsigned int), GNU mangling
#if VG_WORDSIZE == 4
 ALLOC_or_BOMB(m_libstdcxx_soname, _Znwj,          __builtin_new);
 ALLOC_or_BOMB(m_libc_soname,      _Znwj,          __builtin_new);
#endif

// operator new(unsigned long), GNU mangling
#if VG_WORDSIZE == 8 || defined(VGP_ppc32_aix5)
 ALLOC_or_BOMB(m_libstdcxx_soname, _Znwm,          __builtin_new);
 ALLOC_or_BOMB(m_libc_soname,      _Znwm,          __builtin_new);
#endif

// operator new(unsigned long), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_BOMB(m_libC_dot_a,       __nw__FUl,      __builtin_new);
#endif


/*---------------------- new nothrow ----------------------*/

// operator new(unsigned, std::nothrow_t const&), GNU mangling
#if VG_WORDSIZE == 4
 ALLOC_or_NULL(m_libstdcxx_soname, _ZnwjRKSt9nothrow_t,  __builtin_new);
 ALLOC_or_NULL(m_libc_soname,      _ZnwjRKSt9nothrow_t,  __builtin_new);
#endif

// operator new(unsigned long, std::nothrow_t const&), GNU mangling
#if VG_WORDSIZE == 8 || defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_NULL(m_libstdcxx_soname, _ZnwmRKSt9nothrow_t,  __builtin_new);
 ALLOC_or_NULL(m_libc_soname,      _ZnwmRKSt9nothrow_t,  __builtin_new);
#endif

// operator new(unsigned long, std::nothrow_t const&), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_NULL(m_libC_dot_a,    __nw__FUlRCQ2_3std9nothrow_t, __builtin_new);
#endif


/*---------------------- new [] ----------------------*/

// operator new[](unsigned int), not mangled (for gcc 2.96)
ALLOC_or_BOMB(m_libcstdcxx_soname, __builtin_vec_new, __builtin_vec_new );
ALLOC_or_BOMB(m_libc_soname,       __builtin_vec_new, __builtin_vec_new );

// operator new[](unsigned int), GNU mangling
#if VG_WORDSIZE == 4
 ALLOC_or_BOMB(m_libstdcxx_soname, _Znaj,             __builtin_vec_new );
 ALLOC_or_BOMB(m_libc_soname,      _Znaj,             __builtin_vec_new );
#endif

// operator new[](unsigned long), GNU mangling
#if VG_WORDSIZE == 8 || defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_BOMB(m_libstdcxx_soname, _Znam,             __builtin_vec_new );
 ALLOC_or_BOMB(m_libc_soname,      _Znam,             __builtin_vec_new );
#endif

// operator new[](unsigned long), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_BOMB(m_libC_dot_a,       __vn__FUl,         __builtin_vec_new);
#endif


/*---------------------- new [] nothrow ----------------------*/

// operator new[](unsigned, std::nothrow_t const&), GNU mangling
#if VG_WORDSIZE == 4
 ALLOC_or_NULL(m_libstdcxx_soname, _ZnajRKSt9nothrow_t, __builtin_vec_new );
 ALLOC_or_NULL(m_libc_soname,      _ZnajRKSt9nothrow_t, __builtin_vec_new );
#endif

// operator new[](unsigned long, std::nothrow_t const&), GNU mangling
#if VG_WORDSIZE == 8 || defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_NULL(m_libstdcxx_soname, _ZnamRKSt9nothrow_t, __builtin_vec_new );
 ALLOC_or_NULL(m_libc_soname,      _ZnamRKSt9nothrow_t, __builtin_vec_new );
#endif

// operator new [](unsigned long, std::nothrow_t const&), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_BOMB(m_libC_dot_a,   __vn__FUlRCQ2_3std9nothrow_t, __builtin_vec_new );
#endif


/*---------------------- free ----------------------*/

/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to free previously allocated memory.
*/
#define FREE(soname, fnname, vg_replacement) \
   \
   void VG_REPLACE_FUNCTION_ZU(soname,fnname) (void *p); \
   void VG_REPLACE_FUNCTION_ZU(soname,fnname) (void *p)  \
   { \
      if (!init_done) init(); \
      MALLOC_TRACE(#vg_replacement "(%p)", p ); \
      if (p == NULL)  \
         return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }

// free
FREE(m_libstdcxx_soname,  free,                 free );
FREE(m_libc_soname,       free,                 free );


/*---------------------- cfree ----------------------*/

// cfree
FREE(m_libstdcxx_soname,  cfree,                free );
FREE(m_libc_soname,       cfree,                free );


/*---------------------- delete ----------------------*/
// operator delete(void*), not mangled (for gcc 2.96)
FREE(m_libcstdcxx_soname,  __builtin_delete,     __builtin_delete );
FREE(m_libc_soname,        __builtin_delete,     __builtin_delete );

// operator delete(void*), GNU mangling
FREE(m_libstdcxx_soname,  _ZdlPv,               __builtin_delete );
FREE(m_libc_soname,       _ZdlPv,               __builtin_delete );

// operator delete(void*), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
FREE(m_libC_dot_a,        __dl__FPv,            __builtin_delete );
#endif


/*---------------------- delete nothrow ----------------------*/

// operator delete(void*, std::nothrow_t const&), GNU mangling
FREE(m_libstdcxx_soname, _ZdlPvRKSt9nothrow_t,  __builtin_delete );
FREE(m_libc_soname,      _ZdlPvRKSt9nothrow_t,  __builtin_delete );


/*---------------------- delete [] ----------------------*/
// operator delete[](void*), not mangled (for gcc 2.96)
FREE(m_libcstdcxx_soname,  __builtin_vec_delete, __builtin_vec_delete );
FREE(m_libc_soname,        __builtin_vec_delete, __builtin_vec_delete );

// operator delete[](void*), GNU mangling
FREE(m_libstdcxx_soname,  _ZdaPv,               __builtin_vec_delete );
FREE(m_libc_soname,       _ZdaPv,               __builtin_vec_delete );

// operator delete[](void*), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
FREE(m_libC_dot_a,        __vd__FPv,            __builtin_vec_delete );
#endif


/*---------------------- delete [] nothrow ----------------------*/

// operator delete[](void*, std::nothrow_t const&), GNU mangling
FREE(m_libstdcxx_soname,  _ZdaPvRKSt9nothrow_t, __builtin_vec_delete );
FREE(m_libc_soname,       _ZdaPvRKSt9nothrow_t, __builtin_vec_delete );


/*---------------------- calloc ----------------------*/

#define CALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( SizeT nmemb, SizeT size ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( SizeT nmemb, SizeT size )  \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE("calloc(%llu,%llu)", (ULong)nmemb, (ULong)size ); \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_calloc, nmemb, size ); \
      MALLOC_TRACE(" = %p", v ); \
      return v; \
   }

CALLOC(m_libc_soname, calloc);


/*---------------------- realloc ----------------------*/

#define REALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void* ptrV, SizeT new_size );\
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void* ptrV, SizeT new_size ) \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE("realloc(%p,%llu)", ptrV, (ULong)new_size ); \
      \
      if (ptrV == NULL) \
         /* We need to call a malloc-like function; so let's use \
            one which we know exists. */ \
         return VG_REPLACE_FUNCTION_ZU(m_libc_soname,malloc) (new_size); \
      if (new_size <= 0) { \
         VG_REPLACE_FUNCTION_ZU(m_libc_soname,free)(ptrV); \
         MALLOC_TRACE(" = 0"); \
         return NULL; \
      } \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_realloc, ptrV, new_size ); \
      MALLOC_TRACE(" = %p", v ); \
      return v; \
   }

REALLOC(m_libc_soname, realloc);


/*---------------------- memalign ----------------------*/

#define MEMALIGN(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( SizeT alignment, SizeT n ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( SizeT alignment, SizeT n )  \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE("memalign(al %llu, size %llu)", \
                   (ULong)alignment, (ULong)n ); \
      \
      /* Round up to minimum alignment if necessary. */ \
      if (alignment < VG_MIN_MALLOC_SZB) \
         alignment = VG_MIN_MALLOC_SZB; \
      \
      /* Round up to nearest power-of-two if necessary (like glibc). */ \
      while (0 != (alignment & (alignment - 1))) alignment++; \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_memalign, alignment, n ); \
      MALLOC_TRACE(" = %p", v ); \
      return v; \
   }

MEMALIGN(m_libc_soname, memalign);


/*---------------------- valloc ----------------------*/

#define VALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( SizeT size ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( SizeT size )  \
   { \
      static int pszB = 0; \
      extern int getpagesize (void); \
      if (pszB == 0) \
         pszB = getpagesize(); \
      return VG_REPLACE_FUNCTION_ZU(m_libc_soname,memalign) \
                ((SizeT)pszB, size); \
   }

VALLOC(m_libc_soname, valloc);


/*---------------------- mallopt ----------------------*/

/* Various compatibility wrapper functions, for glibc and libstdc++. */

#define MALLOPT(soname, fnname) \
   \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname) ( int cmd, int value ); \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname) ( int cmd, int value )  \
   { \
      /* In glibc-2.2.4, 1 denotes a successful return value for \
         mallopt */ \
      return 1; \
   }

MALLOPT(m_libc_soname, mallopt);


/*---------------------- malloc_trim ----------------------*/
// Documentation says:
//   malloc_trim(size_t pad);
// 
//   If possible, gives memory back to the system (via negative arguments to
//   sbrk) if there is unused memory at the `high' end of the malloc pool.
//   You can call this after freeing large blocks of memory to potentially
//   reduce the system-level memory requirements of a program. However, it
//   cannot guarantee to reduce memory.  Under some allocation patterns,
//   some large free blocks of memory will be locked between two used
//   chunks, so they cannot be given back to the system.
// 
//   The `pad' argument to malloc_trim represents the amount of free
//   trailing space to leave untrimmed. If this argument is zero, only the
//   minimum amount of memory to maintain internal data structures will be
//   left (one page or less). Non-zero arguments can be supplied to maintain
//   enough trailing space to service future expected allocations without
//   having to re-obtain memory from the system.
// 
//   Malloc_trim returns 1 if it actually released any memory, else 0. On
//   systems that do not support "negative sbrks", it will always return 0. 
//
// For simplicity, we always return 0.
#define MALLOC_TRIM(soname, fnname) \
   \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname) ( SizeT pad ); \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname) ( SizeT pad )  \
   { \
      /* 0 denotes that malloc_trim() either wasn't able \
         to do anything, or was not implemented */ \
      return 0; \
   }

MALLOC_TRIM(m_libc_soname, malloc_trim);


/*---------------------- posix_memalign ----------------------*/

#define POSIX_MEMALIGN(soname, fnname) \
   \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void **memptr, \
                                                 SizeT alignment, SizeT size ); \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void **memptr, \
                                                 SizeT alignment, SizeT size )  \
   { \
      void *mem; \
      \
      /* Test whether the alignment argument is valid.  It must be \
         a power of two multiple of sizeof (void *).  */ \
      if (alignment % sizeof (void *) != 0 \
          || (alignment & (alignment - 1)) != 0) \
         return VKI_EINVAL; \
      \
      mem = VG_REPLACE_FUNCTION_ZU(m_libc_soname,memalign)(alignment, size); \
      \
      if (mem != NULL) { \
        *memptr = mem; \
        return 0; \
      } \
      \
      return VKI_ENOMEM; \
   }

POSIX_MEMALIGN(m_libc_soname, posix_memalign);


/*---------------------- malloc_usable_size ----------------------*/

#define MALLOC_USABLE_SIZE(soname, fnname) \
   \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void* p ); \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void* p )  \
   {  \
      SizeT pszB; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE("malloc_usable_size(%p)", p ); \
      if (NULL == p) \
         return 0; \
      \
      pszB = (SizeT)VALGRIND_NON_SIMD_CALL2( info.arena_payload_szB, \
                                             VG_AR_CLIENT, p ); \
      MALLOC_TRACE(" = %llu", (ULong)pszB ); \
      \
      return pszB; \
   }

MALLOC_USABLE_SIZE(m_libc_soname, malloc_usable_size);


/*---------------------- (unimplemented) ----------------------*/

/* Bomb out if we get any of these. */

static void panic(const char *str)
{
   VALGRIND_PRINTF_BACKTRACE("Program aborting because of call to %s", str);
   _exit(99);
   *(int *)0 = 'x';
}

#define PANIC(soname, fnname) \
   \
   void VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void ); \
   void VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void )  \
   { \
      panic(#fnname); \
   }

PANIC(m_libc_soname, pvalloc);
PANIC(m_libc_soname, malloc_stats);
PANIC(m_libc_soname, malloc_get_state);
PANIC(m_libc_soname, malloc_set_state);


/*---------------------- mallinfo ----------------------*/

// mi must be static;  if it is auto then Memcheck thinks it is
// uninitialised when used by the caller of this function, because Memcheck
// doesn't know that the call to mallinfo fills in mi.
#define MALLINFO(soname, fnname) \
   \
   struct vg_mallinfo VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void ); \
   struct vg_mallinfo VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void )  \
   { \
      static struct vg_mallinfo mi; \
      if (!init_done) init(); \
      MALLOC_TRACE("mallinfo()"); \
      (void)VALGRIND_NON_SIMD_CALL1( info.mallinfo, &mi ); \
      return mi; \
   }

MALLINFO(m_libc_soname, mallinfo);


/* All the code in here is unused until this function is called */

static void init(void)
{
   int res;

   if (init_done)
      return;

   init_done = 1;

   VALGRIND_DO_CLIENT_REQUEST(res, -1, VG_USERREQ__GET_MALLOCFUNCS, &info,
                              0, 0, 0, 0);
}

/*--------------------------------------------------------------------*/
/*--- end                                      vg_replace_malloc.c ---*/
/*--------------------------------------------------------------------*/
