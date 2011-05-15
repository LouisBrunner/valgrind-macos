
/*--------------------------------------------------------------------*/
/*--- Replacements for malloc() et al, which run on the simulated  ---*/
/*--- CPU.                                     vg_replace_malloc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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

   IMPORTANT: this file must not contain any floating point code, nor
   any integer division.  This is because on ARM these can cause calls
   to helper functions, which will be unresolved within this .so.
   Although it is usually the case that the client's ld.so instance
   can bind them at runtime to the relevant functions in the client
   executable, there is no guarantee of this; and so the client may
   die via a runtime link failure.  Hence the only safe approach is to
   avoid such function calls in the first place.  See "#define CALLOC"
   below for a specific example.

   A useful command is
      for f in `find . -name "*preload*.so*"` ; \
          do nm -A $f | grep " U " ; \
      done

   to see all the undefined symbols in all the preload shared objects.
   ------------------------------------------------------------------ */

#include "pub_core_basics.h"
#include "pub_core_vki.h"           // VKI_EINVAL, VKI_ENOMEM
#include "pub_core_clreq.h"         // for VALGRIND_INTERNAL_PRINTF,
                                    //   VALGRIND_NON_SIMD_CALL[12]
#include "pub_core_debuginfo.h"     // needed for pub_core_redir.h :(
#include "pub_core_mallocfree.h"    // for VG_MIN_MALLOC_SZB, VG_AR_CLIENT
#include "pub_core_redir.h"         // for VG_REPLACE_FUNCTION_*
#include "pub_core_replacemalloc.h"


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

__attribute__ ((__noreturn__))
extern void _exit(int);

/* Apparently it is necessary to make ourselves free of any dependency
   on memcpy() on ppc32-aix5; else programs linked with -brtl fail.
   memcpy() is used by gcc for a struct assignment in mallinfo()
   below.  Add the following conservative implementation (memmove,
   really). */
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
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


/* Compute the high word of the double-length unsigned product of U
   and V.  This is for calloc argument overflow checking; see comments
   below.  Algorithm as described in Hacker's Delight, chapter 8. */
static UWord umulHW ( UWord u, UWord v )
{
   UWord u0, v0, w0, rHi;
   UWord u1, v1, w1,w2,t;
   UWord halfMask  = sizeof(UWord)==4 ? (UWord)0xFFFF
                                      : (UWord)0xFFFFFFFFULL;
   UWord halfShift = sizeof(UWord)==4 ? 16 : 32;
   u0  = u & halfMask;
   u1  = u >> halfShift;
   v0  = v & halfMask;
   v1  = v >> halfShift;
   w0  = u0 * v0;
   t   = u1 * v0 + (w0 >> halfShift);
   w1  = t & halfMask;
   w2  = t >> halfShift;
   w1  = u0 * v1 + w1;
   rHi = u1 * v1 + w2 + (w1 >> halfShift);
   return rHi;
}


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
__attribute__((constructor))
static void init(void);

#define MALLOC_TRACE(format, args...)  \
   if (info.clo_trace_malloc) {        \
      VALGRIND_INTERNAL_PRINTF(format, ## args ); }

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
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

#define ZONEALLOC_or_NULL(soname, fnname, vg_replacement) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (void *zone, SizeT n); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (void *zone, SizeT n)  \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE(#fnname "(%p, %llu)", zone, (ULong)n ); \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, n ); \
      MALLOC_TRACE(" = %p\n", v ); \
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
      MALLOC_TRACE(" = %p\n", v ); \
      if (NULL == v) { \
         VALGRIND_PRINTF( \
            "new/new[] failed and should throw an exception, but Valgrind\n"); \
         VALGRIND_PRINTF_BACKTRACE( \
            "   cannot throw exceptions and so is aborting instead.  Sorry.\n"); \
            _exit(1); \
      } \
      return v; \
   }

// Each of these lines generates a replacement function:
//     (from_so, from_fn,  v's replacement)

// malloc
ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, malloc,      malloc);
ALLOC_or_NULL(VG_Z_LIBC_SONAME,      malloc,      malloc);
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
ALLOC_or_NULL(VG_Z_LIBC_SONAME,      malloc_common, malloc);
#elif defined(VGO_darwin)
ZONEALLOC_or_NULL(VG_Z_LIBC_SONAME, malloc_zone_malloc, malloc);
#endif


/*---------------------- new ----------------------*/

// operator new(unsigned int), not mangled (for gcc 2.96)
ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME,  builtin_new,    __builtin_new);
ALLOC_or_BOMB(VG_Z_LIBC_SONAME,       builtin_new,    __builtin_new);

ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME,  __builtin_new,  __builtin_new);
ALLOC_or_BOMB(VG_Z_LIBC_SONAME,       __builtin_new,  __builtin_new);

// operator new(unsigned int), GNU mangling
#if VG_WORDSIZE == 4
 ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwj,          __builtin_new);
 ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znwj,          __builtin_new);
#endif

// operator new(unsigned long), GNU mangling
#if VG_WORDSIZE == 8 || defined(VGP_ppc32_aix5) || defined(VGO_darwin)
 ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwm,          __builtin_new);
 ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znwm,          __builtin_new);
#endif

// operator new(unsigned long), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_BOMB(VG_Z_LIBC_DOT_A,       __nw__FUl,      __builtin_new);
#endif


/*---------------------- new nothrow ----------------------*/

// operator new(unsigned, std::nothrow_t const&), GNU mangling
#if VG_WORDSIZE == 4
 ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwjRKSt9nothrow_t,  __builtin_new);
 ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnwjRKSt9nothrow_t,  __builtin_new);
#endif

// operator new(unsigned long, std::nothrow_t const&), GNU mangling
#if VG_WORDSIZE == 8 || defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5) || defined(VGO_darwin)
 ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwmRKSt9nothrow_t,  __builtin_new);
 ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnwmRKSt9nothrow_t,  __builtin_new);
#endif

// operator new(unsigned long, std::nothrow_t const&), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_NULL(VG_Z_LIBC_DOT_A,    __nw__FUlRCQ2_3std9nothrow_t, __builtin_new);
#endif


/*---------------------- new [] ----------------------*/

// operator new[](unsigned int), not mangled (for gcc 2.96)
ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME,  __builtin_vec_new, __builtin_vec_new );
ALLOC_or_BOMB(VG_Z_LIBC_SONAME,       __builtin_vec_new, __builtin_vec_new );

// operator new[](unsigned int), GNU mangling
#if VG_WORDSIZE == 4
 ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znaj,             __builtin_vec_new );
 ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znaj,             __builtin_vec_new );
#endif

// operator new[](unsigned long), GNU mangling
#if VG_WORDSIZE == 8 || defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5) || defined(VGO_darwin)
 ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znam,             __builtin_vec_new );
 ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znam,             __builtin_vec_new );
#endif

// operator new[](unsigned long), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_BOMB(VG_Z_LIBC_DOT_A,       __vn__FUl,         __builtin_vec_new);
#endif


/*---------------------- new [] nothrow ----------------------*/

// operator new[](unsigned, std::nothrow_t const&), GNU mangling
#if VG_WORDSIZE == 4
 ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnajRKSt9nothrow_t, __builtin_vec_new );
 ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnajRKSt9nothrow_t, __builtin_vec_new );
#endif

// operator new[](unsigned long, std::nothrow_t const&), GNU mangling
#if VG_WORDSIZE == 8 || defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5) || defined(VGO_darwin)
 ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnamRKSt9nothrow_t, __builtin_vec_new );
 ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnamRKSt9nothrow_t, __builtin_vec_new );
#endif

// operator new [](unsigned long, std::nothrow_t const&), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
 ALLOC_or_BOMB(VG_Z_LIBC_DOT_A,   __vn__FUlRCQ2_3std9nothrow_t, __builtin_vec_new );
#endif


/*---------------------- free ----------------------*/

/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to free previously allocated memory.
*/
#define ZONEFREE(soname, fnname, vg_replacement) \
   \
   void VG_REPLACE_FUNCTION_ZU(soname,fnname) (void *zone, void *p); \
   void VG_REPLACE_FUNCTION_ZU(soname,fnname) (void *zone, void *p)  \
   { \
      if (!init_done) init(); \
      MALLOC_TRACE(#vg_replacement "(%p, %p)\n", zone, p ); \
      if (p == NULL)  \
         return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }

#define FREE(soname, fnname, vg_replacement) \
   \
   void VG_REPLACE_FUNCTION_ZU(soname,fnname) (void *p); \
   void VG_REPLACE_FUNCTION_ZU(soname,fnname) (void *p)  \
   { \
      if (!init_done) init(); \
      MALLOC_TRACE(#vg_replacement "(%p)\n", p ); \
      if (p == NULL)  \
         return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }

// free
FREE(VG_Z_LIBSTDCXX_SONAME,  free,                 free );
FREE(VG_Z_LIBC_SONAME,       free,                 free );
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
FREE(VG_Z_LIBC_SONAME,       free_common,          free );
#elif defined(VGO_darwin)
ZONEFREE(VG_Z_LIBC_SONAME,   malloc_zone_free,     free );
#endif


/*---------------------- cfree ----------------------*/

// cfree
FREE(VG_Z_LIBSTDCXX_SONAME,  cfree,                free );
FREE(VG_Z_LIBC_SONAME,       cfree,                free );


/*---------------------- delete ----------------------*/
// operator delete(void*), not mangled (for gcc 2.96)
FREE(VG_Z_LIBSTDCXX_SONAME,   __builtin_delete,     __builtin_delete );
FREE(VG_Z_LIBC_SONAME,        __builtin_delete,     __builtin_delete );

// operator delete(void*), GNU mangling
FREE(VG_Z_LIBSTDCXX_SONAME,  _ZdlPv,               __builtin_delete );
FREE(VG_Z_LIBC_SONAME,       _ZdlPv,               __builtin_delete );

// operator delete(void*), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
FREE(VG_Z_LIBC_DOT_A,        __dl__FPv,            __builtin_delete );
#endif


/*---------------------- delete nothrow ----------------------*/

// operator delete(void*, std::nothrow_t const&), GNU mangling
FREE(VG_Z_LIBSTDCXX_SONAME, _ZdlPvRKSt9nothrow_t,  __builtin_delete );
FREE(VG_Z_LIBC_SONAME,      _ZdlPvRKSt9nothrow_t,  __builtin_delete );


/*---------------------- delete [] ----------------------*/
// operator delete[](void*), not mangled (for gcc 2.96)
FREE(VG_Z_LIBSTDCXX_SONAME,   __builtin_vec_delete, __builtin_vec_delete );
FREE(VG_Z_LIBC_SONAME,        __builtin_vec_delete, __builtin_vec_delete );

// operator delete[](void*), GNU mangling
FREE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPv,               __builtin_vec_delete );
FREE(VG_Z_LIBC_SONAME,       _ZdaPv,               __builtin_vec_delete );

// operator delete[](void*), ARM/cfront mangling
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
FREE(VG_Z_LIBC_DOT_A,        __vd__FPv,            __builtin_vec_delete );
#endif


/*---------------------- delete [] nothrow ----------------------*/

// operator delete[](void*, std::nothrow_t const&), GNU mangling
FREE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvRKSt9nothrow_t, __builtin_vec_delete );
FREE(VG_Z_LIBC_SONAME,       _ZdaPvRKSt9nothrow_t, __builtin_vec_delete );


/*---------------------- calloc ----------------------*/

#define ZONECALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void *zone, SizeT nmemb, SizeT size ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void *zone, SizeT nmemb, SizeT size )  \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE("calloc(%p, %llu,%llu)", zone, (ULong)nmemb, (ULong)size ); \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_calloc, nmemb, size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

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
      /* Protect against overflow.  See bug 24078. (that bug number is
         invalid.  Which one really?) */ \
      /* But don't use division, since that produces an external symbol
         reference on ARM, in the form of a call to __aeabi_uidiv.  It's
         normally OK, because ld.so manages to resolve it to something in the
         executable, or one of its shared objects.  But that isn't guaranteed
         to be the case, and it has been observed to fail in rare cases, eg:
            echo x | valgrind /bin/sed -n "s/.*-\>\ //p"
         So instead compute the high word of the product and check it is zero. */ \
      if (umulHW(size, nmemb) != 0) return NULL; \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_calloc, nmemb, size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

CALLOC(VG_Z_LIBC_SONAME, calloc);
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
CALLOC(VG_Z_LIBC_SONAME, calloc_common);
#elif defined(VGO_darwin)
ZONECALLOC(VG_Z_LIBC_SONAME, malloc_zone_calloc);
#endif


/*---------------------- realloc ----------------------*/

#define ZONEREALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void *zone, void* ptrV, SizeT new_size );\
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void *zone, void* ptrV, SizeT new_size ) \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE("realloc(%p,%p,%llu)", zone, ptrV, (ULong)new_size ); \
      \
      if (ptrV == NULL) \
         /* We need to call a malloc-like function; so let's use \
            one which we know exists. GrP fixme use zonemalloc instead? */ \
         return VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME,malloc) (new_size); \
      if (new_size <= 0) { \
         VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME,free)(ptrV); \
         MALLOC_TRACE(" = 0\n"); \
         return NULL; \
      } \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_realloc, ptrV, new_size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

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
         return VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME,malloc) (new_size); \
      if (new_size <= 0) { \
         VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME,free)(ptrV); \
         MALLOC_TRACE(" = 0\n"); \
         return NULL; \
      } \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_realloc, ptrV, new_size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

REALLOC(VG_Z_LIBC_SONAME, realloc);
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
REALLOC(VG_Z_LIBC_SONAME, realloc_common);
#elif defined(VGO_darwin)
ZONEREALLOC(VG_Z_LIBC_SONAME, malloc_zone_realloc);
#endif


/*---------------------- memalign ----------------------*/

#define ZONEMEMALIGN(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void *zone, SizeT alignment, SizeT n ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void *zone, SizeT alignment, SizeT n ) \
   { \
      void* v; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE("memalign(%p, al %llu, size %llu)", \
                   zone, (ULong)alignment, (ULong)n );  \
      \
      /* Round up to minimum alignment if necessary. */ \
      if (alignment < VG_MIN_MALLOC_SZB) \
         alignment = VG_MIN_MALLOC_SZB; \
      \
      /* Round up to nearest power-of-two if necessary (like glibc). */ \
      while (0 != (alignment & (alignment - 1))) alignment++; \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_memalign, alignment, n ); \
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

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
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

MEMALIGN(VG_Z_LIBC_SONAME, memalign);
#if defined(VGO_darwin)
ZONEMEMALIGN(VG_Z_LIBC_SONAME, malloc_zone_memalign);
#endif


/*---------------------- valloc ----------------------*/

static int local__getpagesize ( void ) {
#  if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   return 4096; /* kludge - toc problems prevent calling getpagesize() */
#  else
   extern int getpagesize (void);
   return getpagesize();
#  endif
}

#define VALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( SizeT size ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( SizeT size )  \
   { \
      static int pszB = 0; \
      if (pszB == 0) \
         pszB = local__getpagesize(); \
      return VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME,memalign) \
                ((SizeT)pszB, size); \
   }

#define ZONEVALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void *zone, SizeT size ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( void *zone, SizeT size )  \
   { \
      static int pszB = 0; \
      extern int getpagesize (void); \
      if (pszB == 0) \
         pszB = getpagesize(); \
      return VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME,memalign) \
                ((SizeT)pszB, size); \
   }

VALLOC(VG_Z_LIBC_SONAME, valloc);
#if defined(VGO_darwin)
ZONEVALLOC(VG_Z_LIBC_SONAME, malloc_zone_valloc);
#endif


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

MALLOPT(VG_Z_LIBC_SONAME, mallopt);


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

MALLOC_TRIM(VG_Z_LIBC_SONAME, malloc_trim);


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
      mem = VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME,memalign)(alignment, size); \
      \
      if (mem != NULL) { \
        *memptr = mem; \
        return 0; \
      } \
      \
      return VKI_ENOMEM; \
   }

POSIX_MEMALIGN(VG_Z_LIBC_SONAME, posix_memalign);
#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
/* 27 Nov 07: it appears that xlc links into executables, a
   posix_memalign, which calls onwards to memalign_common, with the
   same args. */
POSIX_MEMALIGN(VG_Z_LIBC_SONAME, memalign_common);
#endif


/*---------------------- malloc_usable_size ----------------------*/

#define MALLOC_USABLE_SIZE(soname, fnname) \
   \
   SizeT VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void* p ); \
   SizeT VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void* p )  \
   {  \
      SizeT pszB; \
      \
      if (!init_done) init(); \
      MALLOC_TRACE("malloc_usable_size(%p)", p ); \
      if (NULL == p) \
         return 0; \
      \
      pszB = (SizeT)VALGRIND_NON_SIMD_CALL1( info.tl_malloc_usable_size, p ); \
      MALLOC_TRACE(" = %llu\n", (ULong)pszB ); \
      \
      return pszB; \
   }

MALLOC_USABLE_SIZE(VG_Z_LIBC_SONAME, malloc_usable_size);
MALLOC_USABLE_SIZE(VG_Z_LIBC_SONAME, malloc_size);


/*---------------------- (unimplemented) ----------------------*/

/* Bomb out if we get any of these. */

static void panic(const char *str)
{
   VALGRIND_PRINTF_BACKTRACE("Program aborting because of call to %s\n", str);
   _exit(99);
   *(volatile int *)0 = 'x';
}

#define PANIC(soname, fnname) \
   \
   void VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void ); \
   void VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void )  \
   { \
      panic(#fnname); \
   }

PANIC(VG_Z_LIBC_SONAME, pvalloc);
PANIC(VG_Z_LIBC_SONAME, malloc_get_state);
PANIC(VG_Z_LIBC_SONAME, malloc_set_state);

#define MALLOC_STATS(soname, fnname) \
   \
   void VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void ); \
   void VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void )  \
   { \
      /* Valgrind's malloc_stats implementation does nothing. */ \
   } 

MALLOC_STATS(VG_Z_LIBC_SONAME, malloc_stats);


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
      MALLOC_TRACE("mallinfo()\n"); \
      (void)VALGRIND_NON_SIMD_CALL1( info.mallinfo, &mi ); \
      return mi; \
   }

MALLINFO(VG_Z_LIBC_SONAME, mallinfo);


#if defined(VGO_darwin)

static vki_malloc_zone_t vg_default_zone = {
    NULL, // reserved
    NULL, // reserved
    NULL, // GrP fixme malloc_size
    (void*)VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, malloc_zone_malloc), 
    (void*)VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, malloc_zone_calloc), 
    (void*)VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, malloc_zone_valloc), 
    (void*)VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, malloc_zone_free), 
    (void*)VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, malloc_zone_realloc), 
    NULL, // GrP fixme destroy
    "ValgrindMallocZone", 
    NULL, // batch_malloc
    NULL, // batch_free
    NULL, // GrP fixme introspect
    2,  // version (GrP fixme 3?)
    // DDD: this field exists in Mac OS 10.6, but not 10.5.
    #if 0
    (void*)VG_REPLACE_FUNCTION_ZU(VG_Z_LIBC_SONAME, malloc_zone_memalign)
    #endif
};

#define DEFAULT_ZONE(soname, fnname) \
   \
   void *VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void ); \
   void *VG_REPLACE_FUNCTION_ZU(soname, fnname) ( void )  \
   { \
      return &vg_default_zone; \
   }

#if defined(VGO_darwin)
DEFAULT_ZONE(VG_Z_LIBC_SONAME, malloc_zone_from_ptr);
DEFAULT_ZONE(VG_Z_LIBC_SONAME, malloc_default_zone);
#endif

// GrP fixme bypass libc's use of zone->introspect->check
#define ZONE_CHECK(soname, fnname) \
                                   \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname)(void* zone); \
   int VG_REPLACE_FUNCTION_ZU(soname, fnname)(void* zone)  \
   { \
      return 1; \
   }

#if defined(VGO_darwin)
ZONE_CHECK(VG_Z_LIBC_SONAME, malloc_zone_check);    
#endif

#endif


/* All the code in here is unused until this function is called */

__attribute__((constructor))
static void init(void)
{
   // This doesn't look thread-safe, but it should be ok... Bart says:
   //   
   //   Every program I know of calls malloc() at least once before calling
   //   pthread_create().  So init_done gets initialized before any thread is
   //   created, and is only read when multiple threads are active
   //   simultaneously.  Such an access pattern is safe.
   //
   //   If the assignment to the variable init_done would be triggering a race
   //   condition, both DRD and Helgrind would report this race.
   // 
   //   By the way, although the init() function in
   //   coregrind/m_replacemalloc/vg_replace_malloc.c has been declared
   //   __attribute__((constructor)), it is not safe to remove the variable
   //   init_done. This is because it is possible that malloc() and hence
   //   init() gets called before shared library initialization finished.
   //
   if (init_done)
      return;

   init_done = 1;

   VALGRIND_DO_CLIENT_REQUEST_EXPR(-1, VG_USERREQ__GET_MALLOCFUNCS, &info,
                                   0, 0, 0, 0);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
