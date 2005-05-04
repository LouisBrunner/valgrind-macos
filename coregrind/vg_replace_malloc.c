
/*--------------------------------------------------------------------*/
/*--- Replacements for malloc() et al, which run on the simulated  ---*/
/*--- CPU.                                     vg_replace_malloc.c ---*/
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

/* ---------------------------------------------------------------------
   All the code in this file runs on the SIMULATED CPU.  It is intended
   for various reasons as drop-in replacements for malloc() and friends.
   These functions have global scope, but are not intended to be called
   directly.  See the comments in coregrind/vg_intercept.c for the
   gory details.

   This file can be linked into the injected so file for any tool that
   wishes to know about calls to malloc().  It should define functions
   TL_(malloc) et al that will be called.
   ------------------------------------------------------------------ */

#include "valgrind.h"            /* for VALGRIND_NON_SIMD_CALL[12] */
#include "core.h"

/* The general idea is: you can write a function like this:

      retty ENCODE(foo, bar) ( ... args ... )
      {
         ... body ...
      }

   If foo is a Z-encoded soname and bar is an unencoded fn name, then
   the core's symbol-table reading machinery and redirection machinery
   will conspire to cause calls to the function 'bar' in object with
   soname 'foo' to actually be routed to the function written here.
   We use this below to define dozens of replacements of malloc, free,
   etc.

   The soname must be a Z-encoded bit of text because sonames can
   contain dots etc which are not valid symbol names.  However, it is
   better not to Z-encode the function name, because C++ function names
   are already mangled, and we don't want our own Z encoding to interact
   badly with them; furthermore there's no point, because mangled C++
   function names are by definition already valid symbol names.
   
   It is important that the Z-encoded soname contains no unencoded 
   underscores, since the intercept-handlers in vg_symtab2.c detect
   the end of the soname by looking for the first trailing underscore.
*/

/* We use a Z-encoding scheme here, a la GHC.  This is just about
   readable enough to make a preprocessor unnecessary.

   The intercept-me function names are encoded as

      _vgi_zEncodedSoname_fnname

   where soname is Z-encoded but fnname isn't.

   The Z-encoding scheme is as follows:

   *         -->  Za    (asterisk)
   +         -->  Zp
   :         -->  Zc
   .         -->  Zd
   _         -->  Zu
   (space)   -->  Zs
   Z         -->  ZZ
*/


/* It would be nice to be able to write VG_INTERCEPT_PREFIX instead of
   "_vgi_" here, but I can't figure out how to get cpp to cooperate.
   If you change this "_vgi_" you should also change the definitiion
   of VG_INTERCEPT_PREFIX in core.h accordingly.
*/
#define ENCODE(libname,fnname) _vgi_##libname##_##fnname


/* Some handy mangled names */
#define  m_libstc_plus_plus_star  libstdcZpZpZa   // libstdc++*
#define  m_libc_dot_so_dot_6      libcZdsoZd6     // libc.so.6
//#define  m_libpgc_dot_so          libpgcZdso      // libpgc.so

/* 2 Apr 05: the Portland Group compiler, which uses cfront/ARM style
   mangling, could be supported properly by the redirects in this
   module.  Except we can't because it doesn't put its allocation
   functions in libpgc.so but instead hardwires them into the
   compilation unit holding main(), which makes them impossible to
   intercept directly.  Fortunately those fns seem to route everything
   through to malloc/free.
*/

extern void _exit(int);

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

// Functions for printing from code within Valgrind, but which runs on the
// sim'd CPU.  They must be functions rather than macros so that va_list can
// be used.
// Nb: at one point, these were used by multiple files that run on the sim'd
// CPU, and so were *defined* in core.h with the 'weak' attribute.  That was
// pretty ugly.  It's much better if this is the only file that needs them.

__attribute__((format(__printf__, 1, 2)))
static int
internal_printf(char *format, ...)
{
   UWord _qzz_res = 0;
   va_list vargs;
   va_start(vargs, format);
   VALGRIND_MAGIC_SEQUENCE(_qzz_res, 0, VG_USERREQ__INTERNAL_PRINTF,
                           (UWord)format, (UWord)vargs, 0, 0);
   va_end(vargs);
   return _qzz_res;
}

#define MALLOC_TRACE(format, args...)  \
   if (info.clo_trace_malloc)          \
      internal_printf(format, ## args )

/* Below are new versions of malloc, __builtin_new, free, 
   __builtin_delete, calloc, realloc, memalign, and friends.

   None of these functions are called directly - they are not meant to
   be found by the dynamic linker.  But ALL client calls to malloc() and
   friends wind up here eventually.  They get called because vg_replace_malloc
   installs a bunch of code redirects which causes Valgrind to use these
   functions rather than the ones they're replacing.
*/

/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to allocate memory.  If that fails, return NULL.
*/
#define ALLOC_or_NULL(soname, fnname, vg_replacement) \
   \
   void* ENCODE(soname,fnname) (SizeT n); \
   void* ENCODE(soname,fnname) (SizeT n)  \
   { \
      void* v; \
      \
      MALLOC_TRACE(#fnname "(%llu)", (ULong)n ); \
      if (!init_done) init(); \
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
   void* ENCODE(soname,fnname) (SizeT n); \
   void* ENCODE(soname,fnname) (SizeT n)  \
   { \
      void* v; \
      \
      MALLOC_TRACE(#fnname "(%llu)", (ULong)n ); \
      if (!init_done) init(); \
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
ALLOC_or_NULL(m_libstc_plus_plus_star, malloc,      malloc);
ALLOC_or_NULL(m_libc_dot_so_dot_6,     malloc,      malloc);
//ALLOC_or_NULL(m_libpgc_dot_so,         malloc,      malloc);

// operator new(unsigned int), GNU mangling, 32-bit platforms
ALLOC_or_BOMB(m_libstc_plus_plus_star, builtin_new,    __builtin_new);
ALLOC_or_BOMB(m_libc_dot_so_dot_6,     builtin_new,    __builtin_new);

ALLOC_or_BOMB(m_libstc_plus_plus_star, __builtin_new,  __builtin_new);
ALLOC_or_BOMB(m_libc_dot_so_dot_6,     __builtin_new,  __builtin_new);

// TODO: these should only exist on 32-bit platforms
ALLOC_or_BOMB(m_libstc_plus_plus_star, _Znwj,          __builtin_new);
ALLOC_or_BOMB(m_libc_dot_so_dot_6,     _Znwj,          __builtin_new);

// TODO: these should only exist on 64-bit platforms
// operator new(unsigned long), GNU mangling, 64-bit platforms
ALLOC_or_BOMB(m_libstc_plus_plus_star, _Znwm,          __builtin_new);
ALLOC_or_BOMB(m_libc_dot_so_dot_6,     _Znwm,          __builtin_new);


// operator new(unsigned int), ARM/cfront mangling
//ALLOC_or_BOMB(m_libpgc_dot_so,         __nw__FUi,      __builtin_new);

// TODO: these should only exist on 32-bit platforms
// operator new(unsigned, std::nothrow_t const&), GNU mangling
ALLOC_or_NULL(m_libstc_plus_plus_star, _ZnwjRKSt9nothrow_t,  __builtin_new);
ALLOC_or_NULL(m_libc_dot_so_dot_6,     _ZnwjRKSt9nothrow_t,  __builtin_new);

// TODO: these should only exist on 64-bit platforms
// operator new(unsigned long, std::nothrow_t const&), GNU mangling
ALLOC_or_NULL(m_libstc_plus_plus_star, _ZnwmRKSt9nothrow_t,  __builtin_new);
ALLOC_or_NULL(m_libc_dot_so_dot_6,     _ZnwmRKSt9nothrow_t,  __builtin_new);

// operator new[](unsigned int), GNU mangling
ALLOC_or_BOMB(m_libstc_plus_plus_star, __builtin_vec_new, __builtin_vec_new );
ALLOC_or_BOMB(m_libc_dot_so_dot_6,     __builtin_vec_new, __builtin_vec_new );

// TODO: these should only exist on 32-bit platforms
ALLOC_or_BOMB(m_libstc_plus_plus_star, _Znaj,             __builtin_vec_new );
ALLOC_or_BOMB(m_libc_dot_so_dot_6,     _Znaj,             __builtin_vec_new );

// TODO: these should only exist on 64-bit platforms
// operator new[](unsigned long), GNU mangling, 64-bit platforms
ALLOC_or_BOMB(m_libstc_plus_plus_star, _Znam,             __builtin_vec_new );
ALLOC_or_BOMB(m_libc_dot_so_dot_6,     _Znam,             __builtin_vec_new );

// TODO: these should only exist on 32-bit platforms
// operator new[](unsigned, std::nothrow_t const&), GNU mangling
ALLOC_or_NULL(m_libstc_plus_plus_star, _ZnajRKSt9nothrow_t, __builtin_vec_new );
ALLOC_or_NULL(m_libc_dot_so_dot_6,     _ZnajRKSt9nothrow_t, __builtin_vec_new );

// TODO: these should only exist on 64-bit platforms
// operator new[](unsigned long, std::nothrow_t const&), GNU mangling
ALLOC_or_NULL(m_libstc_plus_plus_star, _ZnamRKSt9nothrow_t, __builtin_vec_new );
ALLOC_or_NULL(m_libc_dot_so_dot_6,     _ZnamRKSt9nothrow_t, __builtin_vec_new );


/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to free previously allocated memory.
*/
#define FREE(soname, fnname, vg_replacement) \
   \
   void ENCODE(soname,fnname) (void *p); \
   void ENCODE(soname,fnname) (void *p)  \
   { \
      MALLOC_TRACE(#vg_replacement "(%p)", p ); \
      if (p == NULL)  \
         return; \
      if (!init_done) init(); \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }

// free
FREE(m_libstc_plus_plus_star,  free,                 free );
FREE(m_libc_dot_so_dot_6,      free,                 free );

// cfree
FREE(m_libstc_plus_plus_star,  cfree,                free );
FREE(m_libc_dot_so_dot_6,      cfree,                free );

// do we really need these?
FREE(m_libstc_plus_plus_star,  __builtin_delete,     __builtin_delete );
FREE(m_libc_dot_so_dot_6,      __builtin_delete,     __builtin_delete );

// operator delete(void*), GNU mangling
FREE(m_libstc_plus_plus_star,  _ZdlPv,               __builtin_delete );
FREE(m_libc_dot_so_dot_6,      _ZdlPv,               __builtin_delete );

// operator delete(void*, std::nothrow_t const&), GNU mangling
FREE(m_libstc_plus_plus_star, _ZdlPvRKSt9nothrow_t,  __builtin_delete );
FREE(m_libc_dot_so_dot_6,     _ZdlPvRKSt9nothrow_t,  __builtin_delete );

// operator delete[](void*), GNU mangling
FREE(m_libstc_plus_plus_star,  __builtin_vec_delete, __builtin_vec_delete );
FREE(m_libc_dot_so_dot_6,      __builtin_vec_delete, __builtin_vec_delete );
FREE(m_libstc_plus_plus_star,  _ZdaPv,               __builtin_vec_delete );
FREE(m_libc_dot_so_dot_6,      _ZdaPv,               __builtin_vec_delete );

// operator delete[](void*, std::nothrow_t const&), GNU mangling
FREE(m_libstc_plus_plus_star,  _ZdaPvRKSt9nothrow_t, __builtin_vec_delete );
FREE(m_libc_dot_so_dot_6,      _ZdaPvRKSt9nothrow_t, __builtin_vec_delete );


#define CALLOC(soname, fnname) \
   \
   void* ENCODE(soname,fnname) ( SizeT nmemb, SizeT size ); \
   void* ENCODE(soname,fnname) ( SizeT nmemb, SizeT size )  \
   { \
      void* v; \
      \
      MALLOC_TRACE("calloc(%llu,%llu)", (ULong)nmemb, (ULong)size ); \
      \
      if (!init_done) init(); \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_calloc, nmemb, size ); \
      MALLOC_TRACE(" = %p", v ); \
      return v; \
   }

CALLOC(m_libc_dot_so_dot_6, calloc);


#define REALLOC(soname, fnname) \
   \
   void* ENCODE(soname,fnname) ( void* ptrV, SizeT new_size ); \
   void* ENCODE(soname,fnname) ( void* ptrV, SizeT new_size )  \
   { \
      void* v; \
      \
      MALLOC_TRACE("realloc(%p,%llu)", ptrV, (ULong)new_size ); \
      \
      if (ptrV == NULL) \
         /* We need to call a malloc-like function; so let's use \
            one which we know exists. */ \
         return ENCODE(libcZdsoZd6,malloc) (new_size); \
      if (new_size <= 0) { \
         ENCODE(libcZdsoZd6,free)(ptrV); \
         if (info.clo_trace_malloc) \
            internal_printf(" = 0" ); \
         return NULL; \
      } \
      if (!init_done) init(); \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_realloc, ptrV, new_size ); \
      MALLOC_TRACE(" = %p", v ); \
      return v; \
   }

REALLOC(m_libc_dot_so_dot_6, realloc);


#define MEMALIGN(soname, fnname) \
   \
   void* ENCODE(soname,fnname) ( SizeT alignment, SizeT n ); \
   void* ENCODE(soname,fnname) ( SizeT alignment, SizeT n )  \
   { \
      void* v; \
      \
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
      if (!init_done) init(); \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_memalign, alignment, n ); \
      MALLOC_TRACE(" = %p", v ); \
      return v; \
   }

MEMALIGN(m_libc_dot_so_dot_6, memalign);


#define VALLOC(soname, fnname) \
   \
   void* ENCODE(soname,fnname) ( SizeT size ); \
   void* ENCODE(soname,fnname) ( SizeT size )  \
   { \
      return ENCODE(libcZdsoZd6,memalign)(VKI_PAGE_SIZE, size); \
   }

VALLOC(m_libc_dot_so_dot_6, valloc);


/* Various compatibility wrapper functions, for glibc and libstdc++. */

#define MALLOPT(soname, fnname) \
   \
   int ENCODE(soname, fnname) ( int cmd, int value ); \
   int ENCODE(soname, fnname) ( int cmd, int value )  \
   { \
      /* In glibc-2.2.4, 1 denotes a successful return value for \
         mallopt */ \
      return 1; \
   }

MALLOPT(m_libc_dot_so_dot_6, mallopt);


#define POSIX_MEMALIGN(soname, fnname) \
   \
   int ENCODE(soname, fnname) ( void **memptr, SizeT alignment, SizeT size ); \
   int ENCODE(soname, fnname) ( void **memptr, SizeT alignment, SizeT size )  \
   { \
      void *mem; \
      \
      /* Test whether the alignment argument is valid.  It must be \
         a power of two multiple of sizeof (void *).  */ \
      if (alignment % sizeof (void *) != 0 \
          || (alignment & (alignment - 1)) != 0) \
         return VKI_EINVAL; \
      \
      mem = ENCODE(libcZdsoZd6,memalign)(alignment, size); \
      \
      if (mem != NULL) { \
        *memptr = mem; \
        return 0; \
      } \
      \
      return VKI_ENOMEM; \
   }

POSIX_MEMALIGN(m_libc_dot_so_dot_6, posix_memalign);


#define MALLOC_USABLE_SIZE(soname, fnname) \
   \
   int ENCODE(soname, fnname) ( void* p ); \
   int ENCODE(soname, fnname) ( void* p )  \
   {  \
      SizeT pszB; \
      \
      MALLOC_TRACE("malloc_usable_size(%p)", p ); \
      if (NULL == p) \
         return 0; \
      \
      if (!init_done) init(); \
      pszB = (SizeT)VALGRIND_NON_SIMD_CALL2( info.arena_payload_szB, \
                                             VG_AR_CLIENT, p ); \
      MALLOC_TRACE(" = %llu", (ULong)pszB ); \
      \
      return pszB; \
   }

MALLOC_USABLE_SIZE(m_libc_dot_so_dot_6, malloc_usable_size);


/* Bomb out if we get any of these. */

static void panic(const char *str)
{
   VALGRIND_PRINTF_BACKTRACE("Program aborting because of call to %s", str);
   _exit(99);
   *(int *)0 = 'x';
}

#define PANIC(soname, fnname) \
   \
   void ENCODE(soname, fnname) ( void ); \
   void ENCODE(soname, fnname) ( void )  \
   { \
      panic(#fnname); \
   }

PANIC(m_libc_dot_so_dot_6, pvalloc);
PANIC(m_libc_dot_so_dot_6, malloc_stats);
PANIC(m_libc_dot_so_dot_6, malloc_trim);
PANIC(m_libc_dot_so_dot_6, malloc_get_state);
PANIC(m_libc_dot_so_dot_6, malloc_set_state);


/* Yet another ugly hack.  Cannot include <malloc.h> because we
   implement functions implemented there with different signatures.
   This struct definition MUST match the system one. */

/* SVID2/XPG mallinfo structure */
struct mallinfo {
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

#define MALLINFO(soname, fnname) \
   \
   struct mallinfo ENCODE(soname, fnname) ( void ); \
   struct mallinfo ENCODE(soname, fnname) ( void )  \
   { \
      /* Should really try to return something a bit more meaningful */ \
      UInt            i; \
      struct mallinfo mi; \
      UChar*          pmi = (UChar*)(&mi); \
      for (i = 0; i < sizeof(mi); i++) \
         pmi[i] = 0; \
      return mi; \
   }

MALLINFO(m_libc_dot_so_dot_6, mallinfo);


/* All the code in here is unused until this function is called */

static void init(void)
{
   int res;

   if (init_done)
      return;

   init_done = 1;

   VALGRIND_MAGIC_SEQUENCE(res, -1, VG_USERREQ__GET_MALLOCFUNCS, &info,
                           0, 0, 0);
}

/*--------------------------------------------------------------------*/
/*--- end                                      vg_replace_malloc.c ---*/
/*--------------------------------------------------------------------*/
