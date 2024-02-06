
/*--------------------------------------------------------------------*/
/*--- Replacements for malloc() et al, which run on the simulated  ---*/
/*--- CPU.                                     vg_replace_malloc.c ---*/
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
#include "../../memcheck/memcheck.h"

#define VG_ALIGN_ROUNDUP(size, alignment)   (((size) + (alignment) - 1) & ~((alignment) - 1))

#define VERIFY_ALIGNMENT(aligned_alloc_info)                                   \
   VALGRIND_DO_CLIENT_REQUEST_STMT(_VG_USERREQ__MEMCHECK_VERIFY_ALIGNMENT,     \
                                   aligned_alloc_info, 0, 0, 0, 0)

/* Assignment of behavioural equivalence class tags: 1NNNP is intended
   to be reserved for the Valgrind core.  Current usage:

   10010 ALLOC_or_NULL
   10020 ZONEALLOC_or_NULL
   10030 ALLOC_or_BOMB
   10040 ZONEFREE
   10050 FREE
   10060 ZONECALLOC
   10070 CALLOC
   10080 ZONEREALLOC
   10090 REALLOC
   10091 REALLOCF
   10092 REALLOCARRAY
   10100 ZONEMEMALIGN
   10110 MEMALIGN
   10120 VALLOC
   10130 ZONEVALLOC
   10140 MALLOPT
   10150 MALLOC_TRIM
   10160 POSIX_MEMALIGN
   10170 ALIGNED_ALL0C
   10180 MALLOC_USABLE_SIZE
   10190 PANIC
   10200 MALLOC_STATS
   10210 MALLINFO
   10220 DEFAULT_ZONE
   10230 CREATE_ZONE
   10240 ZONE_FROM_PTR
   10250 ZONE_CHECK
   10260 ZONE_REGISTER
   10270 ZONE_UNREGISTER
   10280 ZONE_SET_NAME
   10290 ZONE_GET_NAME
*/

/* 2 Apr 05: the Portland Group compiler, which uses cfront/ARM style
   mangling, could be supported properly by the redirects in this
   module.  Except we can't because it doesn't put its allocation
   functions in libpgc.so but instead hardwires them into the
   compilation unit holding main(), which makes them impossible to
   intercept directly.  Fortunately those fns seem to route everything
   through to malloc/free.

   mid-06: could be improved, since we can now intercept in the main
   executable too.

   2023-03:

   There seem to be an ever increasing number of C++ new and delete
   oveloads.

   See
   https://en.cppreference.com/w/cpp/memory/new/operator_new
   https://en.cppreference.com/w/cpp/memory/new/operator_delete

   We need to redirect the "replaceable" versions.

   Anything "user-defined" or "class-specific" we can't know
   about and the user needs to use memory pool annotation.

   "non-alocating placement" as the name implies does not
   allocate. Placement deletes are no-ops.
*/



/* Call here to exit if we can't continue.  On Android we can't call
   _exit for some reason, so we have to blunt-instrument it. */
__attribute__ ((__noreturn__))
static inline void my_exit ( int x )
{
#  if defined(VGPV_arm_linux_android) || defined(VGPV_mips32_linux_android) \
      || defined(VGPV_arm64_linux_android)
   __asm__ __volatile__(".word 0xFFFFFFFF");
   while (1) {}
#  elif defined(VGPV_x86_linux_android)
   __asm__ __volatile__("ud2");
   while (1) {}
#  else
   extern __attribute__ ((__noreturn__)) void _exit(int status);
   _exit(x);
#  endif
}

/* Same problem with getpagesize. */
static inline int my_getpagesize ( void )
{
#  if defined(VGPV_arm_linux_android) \
      || defined(VGPV_x86_linux_android) \
      || defined(VGPV_mips32_linux_android) \
      || defined(VGPV_arm64_linux_android)
   return 4096; /* kludge - link failure on Android, for some reason */
#  else
   extern int getpagesize (void);
   return getpagesize();
#  endif
}


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
#define DO_INIT if (UNLIKELY(!init_done)) init()

/* Startup hook - called as init section */
__attribute__((constructor))
static void init(void);

#define MALLOC_TRACE(format, args...)  \
   if (info.clo_trace_malloc) {        \
      VALGRIND_INTERNAL_PRINTF(format, ## args ); }

// @todo PJF this mechanism doesn't work for MUSL C
// not sure why
// source here https://elixir.bootlin.com/musl/latest/source/src/errno/__errno_location.c#L4

/* Tries to set ERRNO to ENOMEM/EINVAL if possible. */
#if defined(VGO_linux)
extern int *__errno_location (void) __attribute__((weak));
#define SET_ERRNO_ENOMEM if (__errno_location)        \
      (*__errno_location ()) = VKI_ENOMEM;
#define SET_ERRNO_EINVAL {}
#elif defined(VGO_freebsd)
extern int *__error (void) __attribute__((weak));
#define SET_ERRNO_ENOMEM if (__error)        \
      (*__error ()) = VKI_ENOMEM;
#define SET_ERRNO_EINVAL if (__error)        \
      (*__error ()) = VKI_EINVAL;
#elif defined(VGO_solaris)
extern int *___errno (void) __attribute__((weak));
#define SET_ERRNO_ENOMEM if (___errno)        \
      (*___errno ()) = VKI_ENOMEM;
#define SET_ERRNO_EINVAL if (___errno)        \
      (*___errno ()) = VKI_EINVAL;
#elif defined(VGO_darwin)
extern int * __error(void) __attribute__((weak));
#define SET_ERRNO_ENOMEM if (__error)        \
      (*__error ()) = VKI_ENOMEM;
#define SET_ERRNO_EINVAL if (__error)        \
      (*__error ()) = VKI_EINVAL;

#else
#define SET_ERRNO_ENOMEM {}
#define SET_ERRNO_EINVAL {}
#endif

/* Below are new versions of malloc, __builtin_new, free,
   __builtin_delete, calloc, realloc, memalign, and friends.

   None of these functions are called directly - they are not meant to
   be found by the dynamic linker.  But ALL client calls to malloc()
   and friends wind up here eventually.  They get called because
   vg_replace_malloc installs a bunch of code redirects which causes
   Valgrind to use these functions rather than the ones they're
   replacing.
*/

/* The replacement functions are running on the simulated CPU.
   The code on the simulated CPU does not necessarily use
   all arguments. E.g. args can be ignored and/or only given
   to a NON SIMD call.
   The definedness of such 'unused' arguments will not be verified
   by memcheck.
   The macro 'TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED' allows
   memcheck to detect such errors for the otherwise unused args.
   Apart of allowing memcheck to detect an error, the macro
   TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED has no effect and
   has a minimal cost for other tools replacing malloc functions.

   Creating an "artificial" use of _x that works reliably is not entirely
   straightforward.  Simply comparing it against zero often produces no
   warning if _x contains at least one nonzero bit is defined, because
   Memcheck knows that the result of the comparison will be defined (cf
   expensiveCmpEQorNE).

   Really we want to PCast _x, so as to create a value which is entirely
   undefined if any bit of _x is undefined.  But there's no portable way to do
   that.
*/
#define TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(_x) \
   if ((UWord)(_x) == 0) __asm__ __volatile__( "" ::: "memory" )

/*---------------------- malloc ----------------------*/

/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to allocate memory.  If that fails, return NULL.
*/
#define ALLOC_or_NULL(soname, fnname, vg_replacement) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10010,soname,fnname) (SizeT n); \
   void* VG_REPLACE_FUNCTION_EZU(10010,soname,fnname) (SizeT n)  \
   { \
      void* v; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(n); \
      MALLOC_TRACE(#fnname "(%llu)", (ULong)n ); \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, n ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (!v) SET_ERRNO_ENOMEM; \
      return v; \
   }

/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to allocate aligned memory.  If that fails, return NULL.
*/
#define ALLOC_or_NULL_ALIGNED(soname, fnname, vg_replacement, tag) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10010,soname,fnname) (SizeT n, SizeT alignment); \
   void* VG_REPLACE_FUNCTION_EZU(10010,soname,fnname) (SizeT n, SizeT alignment)  \
   { \
      void* v; \
      SizeT orig_alignment = alignment; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(n); \
      struct AlignedAllocInfo aligned_alloc_info = { .orig_alignment=alignment, .size=n, .alloc_kind=AllocKind##tag}; \
      VERIFY_ALIGNMENT(&aligned_alloc_info); \
      MALLOC_TRACE(#fnname "(size %llu, al %llu)", (ULong)n, (ULong)alignment ); \
      \
      if ((alignment == 0) \
       || ((alignment & (alignment - 1)) != 0)) { \
         return 0; \
      } \
      \
      /* Round up to minimum alignment if necessary. */ \
      if (alignment < VG_MIN_MALLOC_SZB) \
         alignment = VG_MIN_MALLOC_SZB; \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL3( info.tl_##vg_replacement, n, alignment, orig_alignment ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (!v) SET_ERRNO_ENOMEM; \
      return v; \
   }

#define ZONEALLOC_or_NULL(soname, fnname, vg_replacement) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10020,soname,fnname) (void *zone, SizeT n); \
   void* VG_REPLACE_FUNCTION_EZU(10020,soname,fnname) (void *zone, SizeT n)  \
   { \
      void* v; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord) zone);	\
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(n);                   \
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
   void* VG_REPLACE_FUNCTION_EZU(10030,soname,fnname) (SizeT n); \
   void* VG_REPLACE_FUNCTION_EZU(10030,soname,fnname) (SizeT n)  \
   { \
      void* v; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(n);           \
      MALLOC_TRACE(#fnname "(%llu)", (ULong)n );        \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, n ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (NULL == v) { \
         VALGRIND_PRINTF( \
            "new/new[] failed and should throw an exception, but Valgrind\n"); \
         VALGRIND_PRINTF_BACKTRACE( \
            "   cannot throw exceptions and so is aborting instead.  Sorry.\n"); \
            my_exit(1); \
      } \
      return v; \
   }

/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to allocate aligned memory.  If that fails, it bombs the
   system.
*/
#define ALLOC_or_BOMB_ALIGNED(soname, fnname, vg_replacement, tag)  \
   \
   void* VG_REPLACE_FUNCTION_EZU(10030,soname,fnname) (SizeT n, SizeT alignment); \
   void* VG_REPLACE_FUNCTION_EZU(10030,soname,fnname) (SizeT n, SizeT alignment)  \
   { \
      void* v; \
      SizeT orig_alignment = alignment; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(n);           \
      struct AlignedAllocInfo aligned_alloc_info = { .orig_alignment=alignment, .size=n, .alloc_kind=AllocKind##tag }; \
      VERIFY_ALIGNMENT(&aligned_alloc_info); \
      MALLOC_TRACE(#fnname "(size %llu, al %llu)", (ULong)n, (ULong)alignment ); \
      \
      if ((alignment == 0) \
       || ((alignment & (alignment - 1)) != 0)) { \
         VALGRIND_PRINTF( \
            "new/new[] aligned failed and should throw an exception, but Valgrind\n"); \
         VALGRIND_PRINTF_BACKTRACE( \
            "   cannot throw exceptions and so is aborting instead.  Sorry.\n"); \
         my_exit(1); \
      } \
      \
      /* Round up to minimum alignment if necessary. */ \
      if (alignment < VG_MIN_MALLOC_SZB) \
         alignment = VG_MIN_MALLOC_SZB; \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL3( info.tl_##vg_replacement, n, alignment, orig_alignment ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (NULL == v) { \
         VALGRIND_PRINTF( \
            "new/new[] aligned failed and should throw an exception, but Valgrind\n"); \
         VALGRIND_PRINTF_BACKTRACE( \
            "   cannot throw exceptions and so is aborting instead.  Sorry.\n"); \
            my_exit(1); \
      } \
      return v; \
   }

// Each of these lines generates a replacement function:
//     (from_so, from_fn,  v's replacement)
// For some lines, we will also define a replacement function
// whose only purpose is to be a soname synonym place holder
// that can be replaced using --soname-synonyms.

// malloc
#if defined(VGO_linux)
 ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, malloc,      malloc);
 ALLOC_or_NULL(VG_Z_LIBC_SONAME,      malloc,      malloc);
 ALLOC_or_NULL(SO_SYN_MALLOC,         malloc,      malloc);

#elif defined(VGO_freebsd)
 ALLOC_or_NULL(VG_Z_LIBC_SONAME,      malloc,      malloc);
 ALLOC_or_NULL(SO_SYN_MALLOC,         malloc,      malloc);

#elif defined(VGO_darwin)
 ALLOC_or_NULL(VG_Z_LIBC_SONAME,      malloc,      malloc);
 ALLOC_or_NULL(SO_SYN_MALLOC,         malloc,      malloc);
 ZONEALLOC_or_NULL(VG_Z_LIBC_SONAME,  malloc_zone_malloc, malloc);
 ZONEALLOC_or_NULL(SO_SYN_MALLOC,     malloc_zone_malloc, malloc);

#elif defined(VGO_solaris)
 ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, malloc,      malloc);
 ALLOC_or_NULL(VG_Z_LIBC_SONAME,      malloc,      malloc);
 ALLOC_or_NULL(VG_Z_LIBUMEM_SO_1,     malloc,      malloc);
 ALLOC_or_NULL(SO_SYN_MALLOC,         malloc,      malloc);

#endif


/*---------------------- new ----------------------*/

#if defined(VGO_linux)
 // operator new(unsigned int), not mangled (for gcc 2.96)
 ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME,  builtin_new,    __builtin_new);
 ALLOC_or_BOMB(VG_Z_LIBC_SONAME,       builtin_new,    __builtin_new);
 ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME,  __builtin_new,  __builtin_new);
 ALLOC_or_BOMB(VG_Z_LIBC_SONAME,       __builtin_new,  __builtin_new);
 // operator new(unsigned int)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwj,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znwj,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znwj,          __builtin_new);
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znwj,          __builtin_new);
 #endif
 // operator new(unsigned long)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwm,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znwm,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znwm,          __builtin_new);
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znwm,          __builtin_new);
 #endif

#elif defined(VGO_freebsd)
 // operator new(unsigned int)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwj,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znwj,          __builtin_new);
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znwj,          __builtin_new);
 #endif
 // operator new(unsigned long)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwm,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znwm,          __builtin_new);
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znwm,          __builtin_new);
 #endif

#elif defined(VGO_darwin)
 // operator new(unsigned int)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwj,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znwj,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znwj,          __builtin_new);
 #endif
 // operator new(unsigned long)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwm,          __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znwm,          __builtin_new);
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znwm,          __builtin_new);
 #endif

#elif defined(VGO_solaris)
 // operator new(unsigned int)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwj,          __builtin_new);
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znwj,          __builtin_new);
 #endif
 // operator new(unsigned long)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znwm,          __builtin_new);
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znwm,          __builtin_new);
 #endif

#endif

/*------------------- C++17 new aligned -------------------*/

 #if defined(VGO_linux)
 // operator new(unsigned int, std::align_val_t)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBC_SONAME,      _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
 #endif
 // operator new(unsigned long, std::align_val_t)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBC_SONAME,      _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
 #endif

#elif defined(VGO_freebsd)
 // operator new(unsigned int, std::align_val_t)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
 #endif
 // operator new(unsigned long, std::align_val_t)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
 #endif

#elif defined(VGO_darwin)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
 #endif
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
 #endif

#elif defined(VGO_solaris)
 // operator new(unsigned int, std::align_val_t)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnwjSt11align_val_t, __builtin_new_aligned, NewAligned);
 #endif
 // operator new(unsigned long, std::align_val_t)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnwmSt11align_val_t, __builtin_new_aligned, NewAligned);

 #endif

#endif


/*---------------------- new nothrow ----------------------*/

#if defined(VGO_linux)
 // operator new(unsigned, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwjRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnwjRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnwjRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnwjRKSt9nothrow_t,  __builtin_new);
 #endif
 // operator new(unsigned long, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwmRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnwmRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnwmRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnwmRKSt9nothrow_t,  __builtin_new);
 #endif

#elif defined(VGO_freebsd)
 // operator new(unsigned, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwjRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _ZnwjRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnwjRKSt9nothrow_t,  __builtin_new);
 #endif
 // operator new(unsigned long, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwmRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnwmRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnwmRKSt9nothrow_t,  __builtin_new);
 #endif

#elif defined(VGO_darwin)
 // operator new(unsigned, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwjRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnwjRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnwjRKSt9nothrow_t,  __builtin_new);
 #endif
 // operator new(unsigned long, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwmRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnwmRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnwmRKSt9nothrow_t,  __builtin_new);
 #endif

#elif defined(VGO_solaris)
 // operator new(unsigned, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwjRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnwjRKSt9nothrow_t,  __builtin_new);
 #endif
 // operator new(unsigned long, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnwmRKSt9nothrow_t,  __builtin_new);
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnwmRKSt9nothrow_t,  __builtin_new);
 #endif

#endif

/*----------------- C++17 new aligned nothrow -----------------*/

#if defined(VGO_linux)
 // operator new(unsigned int, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBC_SONAME,      _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
 #endif
 // operator new(unsigned long, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBC_SONAME,      _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
 #endif

#elif defined(VGO_freebsd)
 // operator new(unsigned int, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
 #endif
 // operator new(unsigned long, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
 #endif

#elif defined(VGO_darwin)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
 #endif
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
 #endif

#elif defined(VGO_solaris)
 // operator new(unsigned, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, __ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         __ZnwjSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
 #endif
 // operator new(unsigned long, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnwmSt11align_val_tRKSt9nothrow_t,  __builtin_new_aligned, NewAligned);
 #endif

#endif


/*---------------------- new [] ----------------------*/

#if defined(VGO_linux)
 // operator new[](unsigned int), not mangled (for gcc 2.96)
 ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME,  __builtin_vec_new, __builtin_vec_new );
 ALLOC_or_BOMB(VG_Z_LIBC_SONAME,       __builtin_vec_new, __builtin_vec_new );
 // operator new[](unsigned int)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znaj,             __builtin_vec_new );
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znaj,             __builtin_vec_new );
  ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znaj,             __builtin_vec_new );
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znaj,             __builtin_vec_new );
 #endif
 // operator new[](unsigned long),
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znam,             __builtin_vec_new );
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znam,             __builtin_vec_new );
  ALLOC_or_BOMB(VG_Z_LIBC_SONAME,      _Znam,             __builtin_vec_new );
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znam,             __builtin_vec_new );
 #endif

#elif defined(VGO_freebsd)
 // operator new[](unsigned int)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znaj,             __builtin_vec_new );
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znaj,             __builtin_vec_new );
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znaj,             __builtin_vec_new );
 #endif
 // operator new[](unsigned long)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znam,             __builtin_vec_new );
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znam,             __builtin_vec_new );
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znam,             __builtin_vec_new );
 #endif

#elif defined(VGO_darwin)
 // operator new[](unsigned int)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znaj,             __builtin_vec_new );
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znaj,             __builtin_vec_new );
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znaj,             __builtin_vec_new );
 #endif
 // operator new[](unsigned long)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znam,             __builtin_vec_new );
  ALLOC_or_BOMB(VG_Z_LIBCXX_SONAME,    _Znam,             __builtin_vec_new );
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znam,             __builtin_vec_new );
 #endif

#elif defined(VGO_solaris)
 // operator new[](unsigned int)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znaj,             __builtin_vec_new );
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znaj,             __builtin_vec_new );
 #endif
 // operator new[](unsigned long)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB(VG_Z_LIBSTDCXX_SONAME, _Znam,             __builtin_vec_new );
  ALLOC_or_BOMB(SO_SYN_MALLOC,         _Znam,             __builtin_vec_new );
 #endif

#endif

/*------------------ C++ 17 new aligned [] ------------------*/

#if defined(VGO_linux)
 // operator new[](unsigned int, std::align_val_t)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBC_SONAME,      _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif
 // operator new[](unsigned long, std::align_val_t)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBC_SONAME,      _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif

#elif defined(VGO_freebsd)
 // operator new[](unsigned int, std::align_val_t)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif
 // operator new[](unsigned long, std::align_val_t)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif

#elif defined(VGO_darwin)

 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif
 // operator new[](unsigned long, std::align_val_t)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif

#elif defined(VGO_solaris)
 // operator new[](unsigned int, std::align_val_t)
 #if VG_WORDSIZE == 4
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnajSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif
 // operator new[](unsigned long, std::align_val_t)
 #if VG_WORDSIZE == 8
  ALLOC_or_BOMB_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_BOMB_ALIGNED(SO_SYN_MALLOC,         _ZnamSt11align_val_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif

#endif


/*---------------------- new [] nothrow ----------------------*/

#if defined(VGO_linux)
 // operator new[](unsigned, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnajRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnajRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnajRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnajRKSt9nothrow_t, __builtin_vec_new );
 #endif
 // operator new[](unsigned long, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnamRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnamRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnamRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnamRKSt9nothrow_t, __builtin_vec_new );
 #endif

#elif defined(VGO_freebsd)
 // operator new[](unsigned, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnajRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnajRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnajRKSt9nothrow_t, __builtin_vec_new );
 #endif
 // operator new[](unsigned long, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnamRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnamRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnamRKSt9nothrow_t, __builtin_vec_new );
 #endif

#elif defined(VGO_darwin)
 // operator new[](unsigned, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnajRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnajRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBC_SONAME,      _ZnajRKSt9nothrow_t, __builtin_vec_new );
 #endif
 // operator new[](unsigned long, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnamRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(VG_Z_LIBCXX_SONAME,    _ZnamRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnamRKSt9nothrow_t, __builtin_vec_new );
 #endif

#elif defined(VGO_solaris)
 // operator new[](unsigned, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnajRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnajRKSt9nothrow_t, __builtin_vec_new );
 #endif
 // operator new[](unsigned long, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL(VG_Z_LIBSTDCXX_SONAME, _ZnamRKSt9nothrow_t, __builtin_vec_new );
  ALLOC_or_NULL(SO_SYN_MALLOC,         _ZnamRKSt9nothrow_t, __builtin_vec_new );
 #endif

#endif

/*----------------- C++17 new aligned [] nothrow -----------------*/

#if defined(VGO_linux)
 // operator new[](unsigned int, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBC_SONAME,      _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif
 // operator new[](unsigned long, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBC_SONAME,      _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif

#elif defined(VGO_freebsd)
 // operator new[](unsigned int, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif
 // operator new[](unsigned long, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif

#elif defined(VGO_darwin)

 #if VG_WORDSIZE == 4
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif
 // operator new[](unsigned long, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif

#elif defined(VGO_solaris)
 // operator new[](unsigned int, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 4
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnajSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif
 // operator new[](unsigned long, std::align_val_t, std::nothrow_t const&)
 #if VG_WORDSIZE == 8
  ALLOC_or_NULL_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
  ALLOC_or_NULL_ALIGNED(SO_SYN_MALLOC,         _ZnamSt11align_val_tRKSt9nothrow_t, __builtin_vec_new_aligned, VecNewAligned );
 #endif

#endif

/*---------------------- free ----------------------*/

/* Generate a replacement for 'fnname' in object 'soname', which calls
   'vg_replacement' to free previously allocated memory.
*/
#define ZONEFREE(soname, fnname, vg_replacement) \
   \
   void VG_REPLACE_FUNCTION_EZU(10040,soname,fnname) (void *zone, void *p); \
   void VG_REPLACE_FUNCTION_EZU(10040,soname,fnname) (void *zone, void *p)  \
   { \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord)zone ^ (UWord)p); \
      MALLOC_TRACE(#fnname "(%p, %p)\n", zone, p ); \
      if (p == NULL)  \
         return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }

#define FREE(soname, fnname, vg_replacement) \
   \
   void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p); \
   void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p)  \
   { \
      DO_INIT; \
      MALLOC_TRACE(#fnname "(%p)\n", p ); \
      if (p == NULL)  \
         return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }


#if defined(VGO_linux)
 FREE(VG_Z_LIBSTDCXX_SONAME,  free,                 free );
 FREE(VG_Z_LIBC_SONAME,       free,                 free );
 FREE(SO_SYN_MALLOC,          free,                 free );

#elif defined(VGO_freebsd)
 FREE(VG_Z_LIBC_SONAME,       free,                 free );
 FREE(SO_SYN_MALLOC,          free,                 free );

#elif defined(VGO_darwin)
 FREE(VG_Z_LIBC_SONAME,       free,                 free );
 FREE(SO_SYN_MALLOC,          free,                 free );
 ZONEFREE(VG_Z_LIBC_SONAME,   malloc_zone_free,     free );
 ZONEFREE(SO_SYN_MALLOC,      malloc_zone_free,     free );

#elif defined(VGO_solaris)
 FREE(VG_Z_LIBC_SONAME,       free,                 free );
 FREE(VG_Z_LIBUMEM_SO_1,      free,                 free );
 FREE(SO_SYN_MALLOC,          free,                 free );

#endif


/*---------------------- cfree ----------------------*/

// cfree
#if defined(VGO_linux)
 FREE(VG_Z_LIBSTDCXX_SONAME,  cfree,                free );
 FREE(VG_Z_LIBC_SONAME,       cfree,                free );
 FREE(SO_SYN_MALLOC,          cfree,                free );

#elif defined(VGO_darwin)
 //FREE(VG_Z_LIBSTDCXX_SONAME,  cfree,                free );
 //FREE(VG_Z_LIBC_SONAME,       cfree,                free );

#elif defined(VGO_solaris)
 FREE(VG_Z_LIBC_SONAME,       cfree,                free );
 /* libumem does not implement cfree(). */
 //FREE(VG_Z_LIBUMEM_SO_1,      cfree,                free );
 FREE(SO_SYN_MALLOC,          cfree,                free );

#endif


/*---------------------- delete ----------------------*/

#define DELETE(soname, fnname, vg_replacement, tag) \
 \
    void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p); \
    void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p)  \
 { \
 struct AlignedAllocInfo aligned_alloc_info = { .mem=p, .alloc_kind=AllocKind##tag }; \
      \
      DO_INIT; \
      VERIFY_ALIGNMENT(&aligned_alloc_info); \
      MALLOC_TRACE(#fnname "(%p)\n", p ); \
      if (p == NULL)  \
      return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
 }

#if defined(VGO_linux)
 // operator delete(void*), not mangled (for gcc 2.96)
 DELETE(VG_Z_LIBSTDCXX_SONAME,   __builtin_delete,     __builtin_delete, DeleteDefault  );
 DELETE(VG_Z_LIBC_SONAME,        __builtin_delete,     __builtin_delete, DeleteDefault  );
 // operator delete(void*)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdlPv,               __builtin_delete, DeleteDefault  );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdlPv,               __builtin_delete, DeleteDefault  );
 DELETE(VG_Z_LIBC_SONAME,       _ZdlPv,               __builtin_delete, DeleteDefault  );
 DELETE(SO_SYN_MALLOC,          _ZdlPv,               __builtin_delete, DeleteDefault  );

#elif defined(VGO_freebsd)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdlPv,               __builtin_delete, DeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdlPv,               __builtin_delete, DeleteDefault );
 DELETE(SO_SYN_MALLOC,          _ZdlPv,               __builtin_delete, DeleteDefault );

#elif defined(VGO_darwin)
 // operator delete(void*)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdlPv,               __builtin_delete, DeleteDefault  );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdlPv,               __builtin_delete, DeleteDefault  );
 DELETE(SO_SYN_MALLOC,          _ZdlPv,               __builtin_delete, DeleteDefault  );

#elif defined(VGO_solaris)
 // operator delete(void*)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdlPv,               __builtin_delete, DeleteDefault  );
 DELETE(SO_SYN_MALLOC,          _ZdlPv,               __builtin_delete, DeleteDefault  );

#endif

 /*------------------- C++14 delete sized -------------------*/

#define DELETE_SIZED(soname, fnname, vg_replacement, tag) \
   \
   void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p, SizeT size); \
   void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p, SizeT size)  \
   { \
      struct AlignedAllocInfo aligned_alloc_info = { .size=size, .mem=p, .alloc_kind=AllocKind##tag }; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord)size); \
      VERIFY_ALIGNMENT(&aligned_alloc_info); \
      MALLOC_TRACE(#fnname "(%p)\n", p ); \
      if (p == NULL)  \
         return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }

#if defined(VGO_linux)
 // operator delete(void*, unsigned int)
#if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvj,               __builtin_delete, DeleteSized );
 DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdlPvj,               __builtin_delete, DeleteSized );
 DELETE_SIZED(VG_Z_LIBC_SONAME,       _ZdlPvj,               __builtin_delete, DeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdlPvj,               __builtin_delete, DeleteSized );
 // operator delete(void*, unsigned long)
#elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvm,               __builtin_delete, DeleteSized );
 DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdlPvm,               __builtin_delete, DeleteSized );
 DELETE_SIZED(VG_Z_LIBC_SONAME,       _ZdlPvm,               __builtin_delete, DeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdlPvm,               __builtin_delete, DeleteSized );
#endif

#elif defined(VGO_freebsd)
 // operator delete(void*, unsigned int)
#if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvj,               __builtin_delete, DeleteSized );
 DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdlPvj,               __builtin_delete, DeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdlPvj,               __builtin_delete, DeleteSized );
#elif __SIZEOF_SIZE_T__ == 8
 // operator delete(void*, unsigned long)
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvm,               __builtin_delete, DeleteSized );
 DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdlPvm,               __builtin_delete, DeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdlPvm,               __builtin_delete, DeleteSized );
#endif

#elif defined(VGO_darwin)
 // operator delete(void*, unsigned int)
#if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvj,               __builtin_delete, DeleteSized );
 DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdlPvj,               __builtin_delete, DeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdlPvj,               __builtin_delete, DeleteSized );
#elif __SIZEOF_SIZE_T__ == 8
 // operator delete(void*, unsigned long)
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvm,               __builtin_delete, DeleteSized );
 DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdlPvm,               __builtin_delete, DeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdlPvm,               __builtin_delete, DeleteSized );
#endif

#elif defined(VGO_solaris)
 // operator delete(void*, unsigned long)
 #if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvj,               __builtin_delete, DeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdlPvj,               __builtin_delete, DeleteSized );
 #elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvm,               __builtin_delete, DeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdlPvm,               __builtin_delete, DeleteSized );
#endif

#endif

 /*------------------- C++17 delete aligned -------------------*/

/* No need to check the alignment
 * either the alignment matches the alloc
 * or the alloc would have failed */

#define DELETE_ALIGNED(soname, fnname, vg_replacement, tag ) \
   \
   void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p, SizeT alignment); \
   void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p, SizeT alignment)  \
   { \
      struct AlignedAllocInfo aligned_alloc_info = { .orig_alignment=alignment, .mem=p, .alloc_kind=AllocKind##tag }; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord)alignment); \
      VERIFY_ALIGNMENT(&aligned_alloc_info); \
      MALLOC_TRACE(#fnname "(%p)\n", p ); \
      if (p == NULL)  \
         return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }

#define DELETE_SIZED_ALIGNED(soname, fnname, vg_replacement, tag ) \
   \
   void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p, SizeT size, SizeT alignment); \
   void VG_REPLACE_FUNCTION_EZU(10050,soname,fnname) (void *p, SizeT size, SizeT alignment)  \
   { \
      struct AlignedAllocInfo aligned_alloc_info = { .orig_alignment=alignment, .size=size, .mem=p, .alloc_kind=AllocKind##tag }; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord)size); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord)alignment); \
      VERIFY_ALIGNMENT(&aligned_alloc_info); \
      MALLOC_TRACE(#fnname "(%p)\n", p ); \
      if (p == NULL)  \
         return; \
      (void)VALGRIND_NON_SIMD_CALL1( info.tl_##vg_replacement, p ); \
   }

#if defined(VGO_linux)
 // operator delete(void*, std::align_val_t)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvSt11align_val_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvSt11align_val_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBC_SONAME,       _ZdlPvSt11align_val_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdlPvSt11align_val_t,  __builtin_delete_aligned, DeleteAligned );

 // operator delete(void*, unsigned int, std::align_val_t)
#if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBC_SONAME,       _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 // operator delete(void*, unsigned long, std::align_val_t)
#elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBC_SONAME,       _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
#endif

#elif defined(VGO_freebsd)
 // operator delete(void*, std::align_val_t)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvSt11align_val_t, __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvSt11align_val_t, __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdlPvSt11align_val_t, __builtin_delete_aligned, DeleteAligned );

 // operator delete(void*, unsigned int, std::align_val_t)
#if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 // operator delete(void*, unsigned long, std::align_val_t)
#elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
#endif

#elif defined(VGO_darwin)

 // operator delete(void*, std::align_val_t)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvSt11align_val_t,               __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvSt11align_val_t,               __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdlPvSt11align_val_t,               __builtin_delete_aligned, DeleteAligned );

 // operator delete(void*, unsigned int, std::align_val_t)
#if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdlPvjSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 // operator delete(void*, unsigned long, std::align_val_t)
#elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdlPvmSt11align_val_t,               __builtin_delete_aligned, DeleteSizedAligned );
#endif

#elif defined(VGO_solaris)

 // operator delete(void*, std::align_val_t)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvSt11align_val_t, __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdlPvSt11align_val_t, __builtin_delete_aligned, DeleteAligned );

 // operator delete(void*, unsigned int, std::align_val_t)
#if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvjSt11align_val_t, __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdlPvjSt11align_val_t, __builtin_delete_aligned, DeleteSizedAligned );
 // operator delete(void*, unsigned long, std::align_val_t)
 #elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdlPvmSt11align_val_t, __builtin_delete_aligned, DeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdlPvmSt11align_val_t, __builtin_delete_aligned, DeleteSizedAligned );
#endif

#endif

/*---------------------- delete nothrow ----------------------*/

#if defined(VGO_linux)
 // operator delete(void*, std::nothrow_t const&)
 DELETE(VG_Z_LIBSTDCXX_SONAME, _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,    _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );
 DELETE(VG_Z_LIBC_SONAME,      _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );
 DELETE(SO_SYN_MALLOC,         _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );

#elif defined(VGO_freebsd)
 // operator delete(void*, std::nothrow_t const&)
 DELETE(VG_Z_LIBSTDCXX_SONAME, _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,    _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );
 DELETE(SO_SYN_MALLOC,         _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );

#elif defined(VGO_darwin)
 // operator delete(void*, std::nothrow_t const&)
 DELETE(VG_Z_LIBSTDCXX_SONAME, _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,    _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );
 DELETE(SO_SYN_MALLOC,         _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );

#elif defined(VGO_solaris)
 // operator delete(void*, std::nothrow_t const&)
 DELETE(VG_Z_LIBSTDCXX_SONAME, _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );
 DELETE(SO_SYN_MALLOC,         _ZdlPvRKSt9nothrow_t,  __builtin_delete, DeleteDefault );

#endif

 /*---------------------- C++17 delete aligned nothrow ----------------------*/

#if defined(VGO_linux)
 // operator delete(void*, std::align_val_t, std::nothrow_t const&)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBC_SONAME,      _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,         _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );

 // no sized version of this operator

#elif defined(VGO_freebsd)
 // operator delete(void*, std::align_val_t, std::nothrow_t const&)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,         _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );

#elif defined(VGO_darwin)

 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,    _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,         _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );

#elif defined(VGO_solaris)
 // operator delete(void*, std::align_val_t, std::nothrow_t const&)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME, _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,         _ZdlPvSt11align_val_tRKSt9nothrow_t,  __builtin_delete_aligned, DeleteAligned );

 // no sized version of this operator

#endif


/*---------------------- delete [] ----------------------*/



#if defined(VGO_linux)
 // operator delete[](void*), not mangled (for gcc 2.96)
 DELETE(VG_Z_LIBSTDCXX_SONAME,   __builtin_vec_delete, __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBC_SONAME,        __builtin_vec_delete, __builtin_vec_delete, VecDeleteDefault );
 // operator delete[](void*)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBC_SONAME,       _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );
 DELETE(SO_SYN_MALLOC,          _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );

#elif defined(VGO_freebsd)
 // operator delete[](void*)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );
 DELETE(SO_SYN_MALLOC,          _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );

#elif defined(VGO_darwin)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );
 DELETE(SO_SYN_MALLOC,          _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );

#elif defined(VGO_solaris)
 // operator delete[](void*)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );
 DELETE(SO_SYN_MALLOC,          _ZdaPv,               __builtin_vec_delete, VecDeleteDefault );

#endif

/*---------------------- C++14 delete sized [] ----------------------*/

#if defined(VGO_linux)
// operator delete[](void*, unsigned int)
 #if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
 DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
 DELETE_SIZED(VG_Z_LIBC_SONAME,       _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );

 #elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
 DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
 DELETE_SIZED(VG_Z_LIBC_SONAME,       _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
#endif

#elif defined(VGO_freebsd)
 // operator delete[](void*, unsigned int)
  #if __SIZEOF_SIZE_T__ == 4
  DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
  DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
  DELETE_SIZED(SO_SYN_MALLOC,          _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
 #elif __SIZEOF_SIZE_T__ == 8
  DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
  DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
  DELETE_SIZED(SO_SYN_MALLOC,          _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
 #endif

#elif defined(VGO_darwin)

  #if __SIZEOF_SIZE_T__ == 4
  DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
  DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
  DELETE_SIZED(SO_SYN_MALLOC,          _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
 #elif __SIZEOF_SIZE_T__ == 8
  DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
  DELETE_SIZED(VG_Z_LIBCXX_SONAME,     _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
  DELETE_SIZED(SO_SYN_MALLOC,          _ZdaPvm,              __builtin_vec_delete, VecDeleteSized );
 #endif

#elif defined(VGO_solaris)
 // operator delete[](void*, unsigned int)
 #if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdaPvj,              __builtin_vec_delete, VecDeleteSized );
  // operator delete[](void*, unsigned long)
 #elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvm,               __builtin_vec_delete, VecDeleteSized );
 DELETE_SIZED(SO_SYN_MALLOC,          _ZdaPvm,               __builtin_vec_delete, VecDeleteSized );
#endif

#endif

/*---------------------- C++17 delete aligned [] ----------------------*/

#if defined(VGO_linux)
 // operator delete[](void*, std::align_val_t)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBC_SONAME,       _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );

 // operator delete[](void*, unsigned int, std::align_val_t)
 #if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBC_SONAME,       _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 // operator delete[](void*, unsigned long, std::align_val_t)
 #elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBC_SONAME,       _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
#endif

#elif defined(VGO_freebsd)
 // operator delete[](void*, std::align_val_t)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );

 // operator delete[](void*, unsigned int, std::align_val_t)
 #if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 // operator delete[](void*, unsigned long, std::align_val_t)
 #elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
#endif

#elif defined(VGO_darwin)

 // operator delete[](void*, std::align_val_t)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );

 // operator delete[](void*, unsigned int, std::align_val_t)
 #if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 // operator delete[](void*, unsigned long, std::align_val_t)
 #elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
#endif


#elif defined(VGO_solaris)
 // operator delete[](void*, std::align_val_t), GNU mangling
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdaPvSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteAligned );

 // operator delete[](void*, unsigned int, std::align_val_t)
 #if __SIZEOF_SIZE_T__ == 4
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdaPvjSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 // operator delete[](void*, unsigned long)
 #elif __SIZEOF_SIZE_T__ == 8
 DELETE_SIZED_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
 DELETE_SIZED_ALIGNED(SO_SYN_MALLOC,          _ZdaPvmSt11align_val_t, __builtin_vec_delete_aligned, VecDeleteSizedAligned );
#endif

#endif

/*---------------------- delete [] nothrow ----------------------*/

#if defined(VGO_linux)
 // operator delete[](void*, std::nothrow_t const&)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBC_SONAME,       _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );
 DELETE(SO_SYN_MALLOC,          _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );

#elif defined(VGO_freebsd)
 // operator delete[](void*, std::nothrow_t const&)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );
 DELETE(SO_SYN_MALLOC,          _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );

#elif defined(VGO_darwin)
 // operator delete[](void*, std::nothrow_t const&)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBCXX_SONAME,     _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );
 DELETE(VG_Z_LIBC_SONAME,       _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );

#elif defined(VGO_solaris)
 // operator delete[](void*, std::nothrow_t const&)
 DELETE(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );
 DELETE(SO_SYN_MALLOC,          _ZdaPvRKSt9nothrow_t, __builtin_vec_delete, VecDeleteDefault );

#endif

 /*---------------------- C+17 delete aligned [] nothrow ----------------------*/

#if defined(VGO_linux)
 // operator delete[](void*, std::align_val_t, std::nothrow_t const&)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBC_SONAME,       _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );

 // no sized version of this operator

#elif defined(VGO_freebsd)
 // operator delete[](void*, std::align_val_t, std::nothrow_t const&)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );

#elif defined(VGO_darwin)

  DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );
  DELETE_ALIGNED(VG_Z_LIBCXX_SONAME,     _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );
  DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );

#elif defined(VGO_solaris)
 // operator delete[](void*, std::align_val_t, std::nothrow_t const&)
 DELETE_ALIGNED(VG_Z_LIBSTDCXX_SONAME,  _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );
 DELETE_ALIGNED(SO_SYN_MALLOC,          _ZdaPvSt11align_val_tRKSt9nothrow_t, __builtin_vec_delete_aligned, VecDeleteAligned );

 // no sized version of this operator

#endif


/*---------------------- calloc ----------------------*/

#define ZONECALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10060,soname,fnname) \
            ( void *zone, SizeT nmemb, SizeT size ); \
   void* VG_REPLACE_FUNCTION_EZU(10060,soname,fnname) \
            ( void *zone, SizeT nmemb, SizeT size )  \
   { \
      void* v; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord) zone); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(nmemb); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(size); \
      MALLOC_TRACE("zone_calloc(%p, %llu,%llu)", zone, (ULong)nmemb, (ULong)size ); \
      \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_calloc, nmemb, size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

#define CALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10070,soname,fnname) \
            ( SizeT nmemb, SizeT size ); \
   void* VG_REPLACE_FUNCTION_EZU(10070,soname,fnname) \
            ( SizeT nmemb, SizeT size )  \
   { \
      void* v; \
      \
      DO_INIT; \
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
      if (!v) SET_ERRNO_ENOMEM; \
      return v; \
   }

#if defined(VGO_linux)
 CALLOC(VG_Z_LIBC_SONAME, calloc);
 CALLOC(SO_SYN_MALLOC,    calloc);

#elif defined(VGO_freebsd)
 CALLOC(VG_Z_LIBC_SONAME, calloc);
 CALLOC(SO_SYN_MALLOC,    calloc);

#elif defined(VGO_darwin)
 CALLOC(VG_Z_LIBC_SONAME, calloc);
 CALLOC(SO_SYN_MALLOC,    calloc);
 ZONECALLOC(VG_Z_LIBC_SONAME, malloc_zone_calloc);
 ZONECALLOC(SO_SYN_MALLOC,    malloc_zone_calloc);

#elif defined(VGO_solaris)
 CALLOC(VG_Z_LIBC_SONAME,      calloc);
 CALLOC(VG_Z_LIBUMEM_SO_1,     calloc);
 CALLOC(SO_SYN_MALLOC,         calloc);

#endif


/*---------------------- realloc ----------------------*/

#define ZONEREALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10080,soname,fnname) \
            ( void *zone, void* ptrV, SizeT new_size ); \
   void* VG_REPLACE_FUNCTION_EZU(10080,soname,fnname) \
            ( void *zone, void* ptrV, SizeT new_size ) \
   { \
      void* v; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(ptrV); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(new_size); \
      MALLOC_TRACE("zone_realloc(%p,%p,%llu)", zone, ptrV, (ULong)new_size ); \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_realloc, ptrV, new_size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (v == NULL) { \
         if (!(new_size == 0U && info.clo_realloc_zero_bytes_frees == True)) {\
            SET_ERRNO_ENOMEM; \
         } \
      } \
      return v; \
   }

#define REALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10090,soname,fnname) \
            ( void* ptrV, SizeT new_size );\
   void* VG_REPLACE_FUNCTION_EZU(10090,soname,fnname) \
            ( void* ptrV, SizeT new_size ) \
   { \
      void* v; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(ptrV); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(new_size); \
      MALLOC_TRACE("realloc(%p,%llu)", ptrV, (ULong)new_size ); \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_realloc, ptrV, new_size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (v == NULL) { \
         if (!(new_size == 0U && info.clo_realloc_zero_bytes_frees == True)) {\
            SET_ERRNO_ENOMEM; \
         } \
      } \
      return v; \
   }

#define REALLOCF(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10091,soname,fnname) \
            ( void* ptrV, SizeT new_size );\
   void* VG_REPLACE_FUNCTION_EZU(10091,soname,fnname) \
            ( void* ptrV, SizeT new_size ) \
   { \
      void* v; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(ptrV); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(new_size); \
      MALLOC_TRACE("reallocf(%p,%llu)", ptrV, (ULong)new_size ); \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_realloc, ptrV, new_size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (v == NULL) { \
         if (!(new_size == 0U && info.clo_realloc_zero_bytes_frees == True)) {\
            VG_REPLACE_FUNCTION_EZU(10050,VG_Z_LIBC_SONAME,free)(ptrV); \
            SET_ERRNO_ENOMEM; \
         } \
      } \
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
   }

#define REALLOCARRAY(soname, fnname) \
 \
    void* VG_REPLACE_FUNCTION_EZU(10092,soname,fnname) \
    ( void* ptrV, SizeT nmemb, SizeT size );\
    void* VG_REPLACE_FUNCTION_EZU(10092,soname,fnname) \
    ( void* ptrV, SizeT nmemb, SizeT size ) \
 { \
      void* v; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(ptrV); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(nmemb); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(size); \
      MALLOC_TRACE("reallocarray(%p,%llu,%llu)", ptrV, (ULong)nmemb, (ULong)size ); \
      if (nmemb > 0 && (SizeT)-1 / nmemb < size) { \
         SET_ERRNO_ENOMEM; \
         return NULL; \
      } \
      v = (void*)VALGRIND_NON_SIMD_CALL2( info.tl_realloc, ptrV, nmemb*size ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (v == NULL) { \
         if (!(size*nmemb == 0U && info.clo_realloc_zero_bytes_frees == True)) {\
            VG_REPLACE_FUNCTION_EZU(10050,VG_Z_LIBC_SONAME,free)(ptrV); \
            SET_ERRNO_ENOMEM; \
      } \
   } \
      MALLOC_TRACE(" = %p\n", v ); \
      return v; \
 }

#if defined(VGO_linux)
 REALLOC(VG_Z_LIBC_SONAME, realloc);
 REALLOC(SO_SYN_MALLOC,    realloc);
 REALLOCARRAY(VG_Z_LIBC_SONAME, reallocarray);
 REALLOCARRAY(SO_SYN_MALLOC, reallocarray);

#elif defined(VGO_freebsd)
 REALLOC(VG_Z_LIBC_SONAME, realloc);
 REALLOC(SO_SYN_MALLOC,    realloc);
 REALLOCF(VG_Z_LIBC_SONAME, reallocf);
 REALLOCF(SO_SYN_MALLOC, reallocf);
 REALLOCARRAY(VG_Z_LIBC_SONAME, reallocarray);
 REALLOCARRAY(SO_SYN_MALLOC, reallocarray);

#elif defined(VGO_darwin)
 REALLOC(VG_Z_LIBC_SONAME, realloc);
 REALLOC(SO_SYN_MALLOC,    realloc);
 REALLOCF(VG_Z_LIBC_SONAME, reallocf);
 REALLOCF(SO_SYN_MALLOC, reallocf);
 ZONEREALLOC(VG_Z_LIBC_SONAME, malloc_zone_realloc);
 ZONEREALLOC(SO_SYN_MALLOC,    malloc_zone_realloc);

#elif defined(VGO_solaris)
 REALLOC(VG_Z_LIBC_SONAME,      realloc);
 REALLOC(VG_Z_LIBUMEM_SO_1,     realloc);
 REALLOC(SO_SYN_MALLOC,         realloc);
 REALLOCARRAY(VG_Z_LIBC_SONAME, reallocarray);
 REALLOCARRAY(SO_SYN_MALLOC, reallocarray);
#endif


/*---------------------- memalign ----------------------*/

 /*
  * memalign is rather old and deprecated
  * Linux glibc will fixup the alignment
  * (unless it is greater than SIZE_MAX / 2 + 1
  * in which case it returns EINVAL)
  *
  * musl libc just calls aligned_alloc
  *
  * FreeBSD, undocumented,  just calls aligned_alloc
  * with size rounded up to a multiple
  * of aligment
  *
  * jemalloc mininum alignment is 1, must be a power of 2
  * it looks like excessively large alignment causes ENOMEM
  *
  * Illumos does not allow an alignment of zero
  * Nor a size of zero
  * And the alignment must be a multiple of 4
  * (though the man page says that the alignment
  * must be a power of 2 at least the size of a word)
  *
  * Does not exist on Darwin
  *
  * tcmalloc seems to behave like glibc and we have
  * no way to switch at runtime
  *
  */

 /* Probably in the wrong place, this is the function
 called by posix_memalign, at least on macOS 10.13 */
#define ZONEMEMALIGN(soname, fnname)                                           \
                                                                               \
   void* VG_REPLACE_FUNCTION_EZU(10100, soname, fnname)(                       \
      void* zone, SizeT alignment, SizeT n);                                   \
   void* VG_REPLACE_FUNCTION_EZU(10100, soname,                                \
                                 fnname)(void* zone, SizeT alignment, SizeT n) \
   {                                                                           \
      void*                   v;                                               \
      SizeT                   orig_alignment     = alignment;                  \
      struct AlignedAllocInfo aligned_alloc_info = {                           \
         .orig_alignment = alignment,                                          \
         .size           = n,                                                  \
         .alloc_kind     = AllocKindPosixMemalign};                            \
                                                                               \
      DO_INIT;                                                                 \
      VERIFY_ALIGNMENT(&aligned_alloc_info);                                   \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord)zone);                        \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(n);                                  \
      MALLOC_TRACE("zone_memalign(%p, al %llu, size %llu)", zone,              \
                   (ULong)alignment, (ULong)n);                                \
                                                                               \
      if (alignment == 0 || alignment % sizeof(void*) != 0 ||                  \
          (alignment & (alignment - 1)) != 0) {                                \
         SET_ERRNO_EINVAL;                                                     \
         return NULL;                                                          \
      }                                                                        \
      /* Round up to minimum alignment if necessary. */                        \
      if (alignment < VG_MIN_MALLOC_SZB)                                       \
         alignment = VG_MIN_MALLOC_SZB;                                        \
                                                                               \
      /* Round up to nearest power-of-two if necessary (like glibc). */        \
      while (0 != (alignment & (alignment - 1)))                               \
         alignment++;                                                          \
                                                                               \
      v = (void*)VALGRIND_NON_SIMD_CALL3(info.tl_memalign, alignment,          \
                                         orig_alignment, n);                   \
      MALLOC_TRACE(" = %p\n", v);                                              \
      if (!v)                                                                  \
         SET_ERRNO_ENOMEM;                                                     \
      return v;                                                                \
   }

#if defined(VGO_freebsd)
#define VG_MEMALIGN_MAKE_SIZE_MULTIPLE_ALIGN 1
#else
#define VG_MEMALIGN_MAKE_SIZE_MULTIPLE_ALIGN 0
#endif

#if defined(VGO_solaris)
#define VG_MEMALIGN_ALIGN_POWER_TWO 0
#define VG_MEMALIGN_NO_ALIGN_ZERO 1
#else
#define VG_MEMALIGN_ALIGN_POWER_TWO 1
#define VG_MEMALIGN_NO_ALIGN_ZERO 0
#endif

#if defined(VGO_solaris)
#define VG_MEMALIGN_ALIGN_FACTOR_FOUR 1
#define VG_MEMALIGN_NO_ALIGN_ZERO 1
#else
#define VG_MEMALIGN_ALIGN_FACTOR_FOUR 0
#define VG_MEMALIGN_NO_ALIGN_ZERO 0
#endif

#if defined(MUSL_LIBC)
#define VG_MEMALIGN_NO_SIZE_ZERO 0
#else
#define VG_MEMALIGN_NO_SIZE_ZERO 1
#endif


#if defined(VGO_linux) && !defined(MUSL_LIBC)

#define MEMALIGN(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10110,soname,fnname) \
            ( SizeT alignment, SizeT n ); \
   void* VG_REPLACE_FUNCTION_EZU(10110,soname,fnname) \
            ( SizeT alignment, SizeT n )  \
   { \
      void* v; \
      SizeT orig_alignment = alignment; \
      struct AlignedAllocInfo aligned_alloc_info = { .orig_alignment=alignment, .size=n, .alloc_kind=AllocKindMemalign}; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(n); \
      VERIFY_ALIGNMENT(&aligned_alloc_info); \
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
      v = (void*)VALGRIND_NON_SIMD_CALL3( info.tl_memalign, alignment, orig_alignment, n ); \
      MALLOC_TRACE(" = %p\n", v ); \
      if (!v) SET_ERRNO_ENOMEM; \
      return v; \
   }

#else

#define MEMALIGN(soname, fnname)                                               \
                                                                               \
   void* VG_REPLACE_FUNCTION_EZU(10110, soname, fnname)(SizeT alignment,       \
                                                        SizeT size);           \
   void* VG_REPLACE_FUNCTION_EZU(10110, soname, fnname)(SizeT alignment,       \
                                                        SizeT size)            \
   {                                                                           \
      void*                   mem;                                             \
      SizeT                   orig_alignment     = alignment;                  \
      struct AlignedAllocInfo aligned_alloc_info = {                           \
         .orig_alignment = alignment,                                          \
         .size           = size,                                               \
         .alloc_kind     = AllocKindMemalign};                                 \
                                                                               \
      DO_INIT;                                                                 \
      VERIFY_ALIGNMENT(&aligned_alloc_info);                                   \
      MALLOC_TRACE("memalign(alignment %llu, size %llu)", (ULong)alignment,    \
                   (ULong)size);                                               \
      if ((VG_MEMALIGN_NO_SIZE_ZERO && (size == 0)) ||                         \
          (VG_MEMALIGN_NO_ALIGN_ZERO && (alignment == 0)) ||                   \
          (VG_MEMALIGN_ALIGN_POWER_TWO &&                                      \
           (alignment & (alignment - 1)) != 0) ||                              \
          (VG_MEMALIGN_ALIGN_FACTOR_FOUR && (alignment % 4 != 0))) {           \
         SET_ERRNO_EINVAL;                                                     \
         return 0;                                                             \
      }                                                                        \
      /* Round up to minimum alignment if necessary. */                        \
      if (alignment < VG_MIN_MALLOC_SZB)                                       \
         alignment = VG_MIN_MALLOC_SZB;                                        \
      /* Solaris allows non-power of 2 alignment but not Valgrind. */          \
      while (0 != (alignment & (alignment - 1)))                               \
         alignment++;                                                          \
                                                                               \
      if (VG_MEMALIGN_MAKE_SIZE_MULTIPLE_ALIGN) {                              \
         size = ((size + alignment - 1) / alignment) * alignment;              \
      }                                                                        \
                                                                               \
      mem = (void*)VALGRIND_NON_SIMD_CALL3(info.tl_memalign, alignment,        \
                                           orig_alignment, size);              \
                                                                               \
      if (!mem)                                                                \
         SET_ERRNO_ENOMEM;                                                     \
                                                                               \
      return mem;                                                              \
   }

#endif

#if defined(VGO_linux)
 MEMALIGN(VG_Z_LIBC_SONAME, memalign);
 MEMALIGN(SO_SYN_MALLOC,    memalign);

#elif defined(VGO_freebsd)
 MEMALIGN(VG_Z_LIBC_SONAME, memalign);
 MEMALIGN(SO_SYN_MALLOC,    memalign);

#elif defined(VGO_darwin)
 ZONEMEMALIGN(VG_Z_LIBC_SONAME, malloc_zone_memalign);
 ZONEMEMALIGN(SO_SYN_MALLOC,    malloc_zone_memalign);

#elif defined(VGO_solaris)
 MEMALIGN(VG_Z_LIBC_SONAME,      memalign);
 MEMALIGN(VG_Z_LIBUMEM_SO_1,     memalign);
 MEMALIGN(SO_SYN_MALLOC,         memalign);

#endif


/*---------------------- valloc ----------------------*/

#define VALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10120,soname,fnname) ( SizeT size ); \
   void* VG_REPLACE_FUNCTION_EZU(10120,soname,fnname) ( SizeT size ) \
   { \
      void *mem; \
      static int pszB = 0; \
      if (pszB == 0) \
         pszB = my_getpagesize(); \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(size); \
      mem = (void*)VALGRIND_NON_SIMD_CALL3( info.tl_memalign, \
               pszB, pszB, size ); \
      \
      if (!mem) SET_ERRNO_ENOMEM; \
      \
      return mem; \
   }

#define ZONEVALLOC(soname, fnname) \
   \
   void* VG_REPLACE_FUNCTION_EZU(10130,soname,fnname) \
            ( void *zone, SizeT size ); \
   void* VG_REPLACE_FUNCTION_EZU(10130,soname,fnname) \
            ( void *zone, SizeT size )  \
   { \
      static int pszB = 0; \
      if (pszB == 0) \
         pszB = my_getpagesize(); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord) zone);	      \
      return (void*)VALGRIND_NON_SIMD_CALL3( info.tl_memalign, \
         pszB, pszB, size); \
   }

#if defined(VGO_linux)
 VALLOC(VG_Z_LIBC_SONAME, valloc);
 VALLOC(SO_SYN_MALLOC, valloc);

#elif defined(VGO_freebsd)
 VALLOC(VG_Z_LIBC_SONAME, valloc);
 VALLOC(SO_SYN_MALLOC, valloc);

#elif defined(VGO_darwin)
 VALLOC(VG_Z_LIBC_SONAME, valloc);
 VALLOC(SO_SYN_MALLOC, valloc);
 ZONEVALLOC(VG_Z_LIBC_SONAME, malloc_zone_valloc);
 ZONEVALLOC(SO_SYN_MALLOC,    malloc_zone_valloc);

#elif defined(VGO_solaris)
 VALLOC(VG_Z_LIBC_SONAME,      valloc);
 VALLOC(VG_Z_LIBUMEM_SO_1,     valloc);
 VALLOC(SO_SYN_MALLOC,         valloc);

#endif


/*---------------------- mallopt ----------------------*/

/* Various compatibility wrapper functions, for glibc and libstdc++. */

#define MALLOPT(soname, fnname) \
   \
   int VG_REPLACE_FUNCTION_EZU(10140,soname,fnname) ( int cmd, int value ); \
   int VG_REPLACE_FUNCTION_EZU(10140,soname,fnname) ( int cmd, int value ) \
   { \
      /* In glibc-2.2.4, 1 denotes a successful return value for \
         mallopt */ \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(cmd); \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(value); \
      return 1; \
   }

#if defined(VGO_linux)
 MALLOPT(VG_Z_LIBC_SONAME, mallopt);
 MALLOPT(SO_SYN_MALLOC,    mallopt);

#elif defined(VGO_darwin)
 //MALLOPT(VG_Z_LIBC_SONAME, mallopt);

#endif


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
   int VG_REPLACE_FUNCTION_EZU(10150,soname,fnname) ( SizeT pad ); \
   int VG_REPLACE_FUNCTION_EZU(10150,soname,fnname) ( SizeT pad ) \
   { \
      /* 0 denotes that malloc_trim() either wasn't able \
         to do anything, or was not implemented */ \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(pad); \
      return 0; \
   }

#if defined(VGO_linux)
 MALLOC_TRIM(VG_Z_LIBC_SONAME, malloc_trim);
 MALLOC_TRIM(SO_SYN_MALLOC,    malloc_trim);

#elif defined(VGO_darwin)
 //MALLOC_TRIM(VG_Z_LIBC_SONAME, malloc_trim);

#endif


/*---------------------- posix_memalign ----------------------*/

#if defined(VGO_solaris)
#define VG_POSIX_MEMALIGN_SIZE_0_RETURN_NULL 1
#else
#define VG_POSIX_MEMALIGN_SIZE_0_RETURN_NULL 0
#endif

#define POSIX_MEMALIGN(soname, fnname) \
   \
   int VG_REPLACE_FUNCTION_EZU(10160,soname,fnname) \
          ( void **memptr, SizeT alignment, SizeT size ); \
   int VG_REPLACE_FUNCTION_EZU(10160,soname,fnname) \
          ( void **memptr, SizeT alignment, SizeT size ) \
   { \
      void *mem; \
      SizeT orig_alignment = alignment; \
      struct AlignedAllocInfo aligned_alloc_info = { .orig_alignment=alignment, .size=size, .alloc_kind=AllocKindPosixMemalign}; \
      \
      DO_INIT; \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(size); \
      VERIFY_ALIGNMENT(&aligned_alloc_info); \
      MALLOC_TRACE("posix_memalign(al %llu, size %llu)\n", \
            (ULong)alignment, (ULong)size ); \
      /* Test whether the alignment argument is valid.  It must be \
         a power of two multiple of sizeof (void *).  */ \
      if (alignment == 0 \
          || alignment % sizeof (void *) != 0 \
          || (alignment & (alignment - 1)) != 0) { \
         return VKI_EINVAL; \
      } \
      if (VG_POSIX_MEMALIGN_SIZE_0_RETURN_NULL && \
          size == 0U) { \
         /* no allocation for zero size on Solaris/Illumos */ \
         *memptr = NULL; \
         return 0; \
      } \
      /* Round up to minimum alignment if necessary. */ \
      if (alignment < VG_MIN_MALLOC_SZB) \
          alignment = VG_MIN_MALLOC_SZB; \
      \
      mem = (void*)VALGRIND_NON_SIMD_CALL3( info.tl_memalign, \
               alignment, orig_alignment, size ); \
      \
      if (mem != NULL) { \
        *memptr = mem; \
        return 0; \
      } \
      \
      return VKI_ENOMEM; \
   }

#if defined(VGO_linux)
 POSIX_MEMALIGN(VG_Z_LIBC_SONAME, posix_memalign);
 POSIX_MEMALIGN(SO_SYN_MALLOC,    posix_memalign);

#elif defined(VGO_freebsd)
 POSIX_MEMALIGN(VG_Z_LIBC_SONAME, posix_memalign);
 POSIX_MEMALIGN(SO_SYN_MALLOC,    posix_memalign);

#elif defined(VGO_darwin)
#if (DARWIN_VERSIO >= DARWIN_10_6)
 POSIX_MEMALIGN(VG_Z_LIBC_SONAME, posix_memalign);
#endif

#elif defined(VGO_solaris)
 POSIX_MEMALIGN(VG_Z_LIBC_SONAME, posix_memalign);
 POSIX_MEMALIGN(SO_SYN_MALLOC,    posix_memalign);

#endif

 /*---------------------- aligned_alloc ----------------------*/

 /*
  * No OS does things the same way.
  *
  * The C standard says "If the value of _alignment_ is not a valid
  * alignment supported by the implementation the function shall
  * fail by returning a null pointer".
  *
  * Linux glibc. The man page claims that the alignment must be
  * a power of two and that size should be a multiple of alignment.
  * However the only case that returns EINVAL (glibc 2.34)
  * is if the alignement is  > SIZE_MAX / 2 + 1
  * Also this is just a weak alias for memalign so this wrapper
  * has no effect on Linux glibc.
  *
  * Linux musl. The alignment must be a power of 2 else
  * returns einval. The value of the alignment is clamped
  * to a minumum of UNIT (16).
  *
  * FreeBSD. the man page claims alignment must be a power of 2.
  * UB if size is not an integral multiple of alignment.
  * The code checks that the alignment is a power of
  * 2 and not less than the minumum alignment (1)
  *
  * Solaris. Doesn't seem to exist on 11.3
  * Illumos. Invalid if the size is 0, the alignment is 0, the
  * alignment is not a multiple of 4 (no power of 2
  * requirement even though the manpage claims is) or the
  * alignment is greater than MAX_ALIGN (whatever that is).
  * Wrapper function that just calls memalign
  *
  * Darwin. Does enforce size being an integer multiple of
  * alignment.
  *
  */

#if defined(VGO_darwin)
#define VG_ALIGNED_ALLOC_SIZE_MULTIPLE_ALIGN 1
#else
#define VG_ALIGNED_ALLOC_SIZE_MULTIPLE_ALIGN 0
#endif

#if defined(VGO_solaris)
#define VG_ALIGNED_ALLOC_ALIGN_POWER_TWO 0
#else
#define VG_ALIGNED_ALLOC_ALIGN_POWER_TWO 1
#endif

#if defined(VGO_solaris)
#define VG_ALIGNED_ALLOC_ALIGN_FACTOR_FOUR 1
#else
#define VG_ALIGNED_ALLOC_ALIGN_FACTOR_FOUR 0
#endif

#if defined(MUSL_LIBC)
#define VG_ALIGNED_ALLOC_NO_SIZE_ZERO 0
#else
#define VG_ALIGNED_ALLOC_NO_SIZE_ZERO 1
#endif

#if defined (VGO_linux) && !defined(MUSL_LIBC) && !defined(HAVE_GNU_LIBC_C17_ALIGNED_ALLOC)

/*
 * Normally for GNU libc <= 2.37 aligned_alloc is a weak alias for memalign
 * so this redir is not used.
 * For libc 2.38 and later it is a separate function but then HAVE_GNU_LIBC_C17_ALIGNED_ALLOC
 * should be true and this version doesn't get compiled.
 * Leaving it here to be on the safe side.
 */

 #define ALIGNED_ALLOC(soname, fnname) \
    \
    void* VG_REPLACE_FUNCTION_EZU(10170,soname,fnname) \
           ( SizeT alignment, SizeT size ); \
    void* VG_REPLACE_FUNCTION_EZU(10170,soname,fnname) \
           ( SizeT alignment, SizeT size ) \
    { \
       void *mem; \
       SizeT orig_alignment = alignment; \
       struct AlignedAllocInfo aligned_alloc_info = { .orig_alignment=alignment, .size=size, .alloc_kind=AllocKindAlignedAlloc}; \
       \
       DO_INIT; \
       TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(size); \
       VERIFY_ALIGNMENT(&aligned_alloc_info); \
       MALLOC_TRACE("aligned_alloc(al %llu, size %llu)", \
                (ULong)alignment, (ULong)size ); \
       \
       /* Round up to minimum alignment if necessary. */ \
       if (alignment < VG_MIN_MALLOC_SZB) \
          alignment = VG_MIN_MALLOC_SZB; \
       \
       /* Round up to nearest power-of-two if necessary (like glibc). */ \
       while (0 != (alignment & (alignment - 1))) alignment++; \
       \
       mem = (void*)VALGRIND_NON_SIMD_CALL3( info.tl_memalign, \
                 alignment, orig_alignment, size ); \
       \
       return mem; \
    }

#else

 #define ALIGNED_ALLOC(soname, fnname) \
    \
    void* VG_REPLACE_FUNCTION_EZU(10170,soname,fnname) \
           ( SizeT alignment, SizeT size ); \
    void* VG_REPLACE_FUNCTION_EZU(10170,soname,fnname) \
           ( SizeT alignment, SizeT size ) \
    { \
       void *mem; \
       SizeT orig_alignment = alignment; \
       struct AlignedAllocInfo aligned_alloc_info = { .orig_alignment=alignment, .size=size, .alloc_kind=AllocKindAlignedAlloc}; \
       \
       DO_INIT; \
       VERIFY_ALIGNMENT(&aligned_alloc_info); \
       MALLOC_TRACE("aligned_alloc(al %llu, size %llu)", \
                (ULong)alignment, (ULong)size ); \
       if ((VG_ALIGNED_ALLOC_NO_SIZE_ZERO && (alignment == 0)) \
           || (VG_ALIGNED_ALLOC_SIZE_MULTIPLE_ALIGN && (size % alignment != 0)) \
           || (VG_ALIGNED_ALLOC_ALIGN_POWER_TWO && (alignment & (alignment - 1)) != 0) \
           || (VG_ALIGNED_ALLOC_ALIGN_FACTOR_FOUR && (alignment % 4 != 0))) { \
          SET_ERRNO_EINVAL; \
          return 0; \
       } \
       \
       /* Round up to minimum alignment if necessary. */ \
       if (alignment < VG_MIN_MALLOC_SZB) \
          alignment = VG_MIN_MALLOC_SZB; \
       /* Solaris allows non-power of 2 alignment but not Valgrind. */ \
       while (0 != (alignment & (alignment - 1))) alignment++; \
       \
       mem = (void*)VALGRIND_NON_SIMD_CALL3( info.tl_memalign, \
                 alignment, orig_alignment, size ); \
       \
       if (!mem) SET_ERRNO_ENOMEM; \
       \
       return mem; \
    }
#endif

 #if defined(VGO_linux)
  ALIGNED_ALLOC(VG_Z_LIBC_SONAME, aligned_alloc);
  ALIGNED_ALLOC(SO_SYN_MALLOC,    aligned_alloc);

#elif defined(VGO_freebsd)
 ALIGNED_ALLOC(G_Z_LIBC_SONAME, aligned_alloc);
 ALIGNED_ALLOC(SO_SYN_MALLOC,   aligned_alloc);

 #elif defined(VGO_darwin)
  //ALIGNED_ALLOC(VG_Z_LIBC_SONAME, aligned_alloc);

 #elif defined(VGO_solaris)
  ALIGNED_ALLOC(VG_Z_LIBC_SONAME, aligned_alloc);
  ALIGNED_ALLOC(SO_SYN_MALLOC,    aligned_alloc);

 #endif

/*---------------------- malloc_usable_size ----------------------*/

#define MALLOC_USABLE_SIZE(soname, fnname) \
   \
   SizeT VG_REPLACE_FUNCTION_EZU(10180,soname,fnname) ( void* p ); \
   SizeT VG_REPLACE_FUNCTION_EZU(10180,soname,fnname) ( void* p ) \
   {  \
      SizeT pszB; \
      \
      DO_INIT; \
      MALLOC_TRACE("malloc_usable_size(%p)", p ); \
      if (NULL == p) \
         return 0; \
      \
      pszB = (SizeT)VALGRIND_NON_SIMD_CALL1( info.tl_malloc_usable_size, p ); \
      MALLOC_TRACE(" = %llu\n", (ULong)pszB ); \
      \
      return pszB; \
   }

#if defined(VGO_linux)
 MALLOC_USABLE_SIZE(VG_Z_LIBC_SONAME, malloc_usable_size);
 MALLOC_USABLE_SIZE(SO_SYN_MALLOC,    malloc_usable_size);
 MALLOC_USABLE_SIZE(VG_Z_LIBC_SONAME, malloc_size);
 MALLOC_USABLE_SIZE(SO_SYN_MALLOC,    malloc_size);
# if defined(VGPV_arm_linux_android) || defined(VGPV_x86_linux_android) \
     || defined(VGPV_mips32_linux_android)
  MALLOC_USABLE_SIZE(VG_Z_LIBC_SONAME, dlmalloc_usable_size);
  MALLOC_USABLE_SIZE(SO_SYN_MALLOC,    dlmalloc_usable_size);
# endif

#elif defined(VGO_freebsd)
 MALLOC_USABLE_SIZE(VG_Z_LIBC_SONAME, malloc_usable_size);
 MALLOC_USABLE_SIZE(SO_SYN_MALLOC,    malloc_usable_size);

#elif defined(VGO_darwin)
 //MALLOC_USABLE_SIZE(VG_Z_LIBC_SONAME, malloc_usable_size);
 MALLOC_USABLE_SIZE(VG_Z_LIBC_SONAME, malloc_size);
 MALLOC_USABLE_SIZE(SO_SYN_MALLOC,    malloc_size);

#endif


/*---------------------- (unimplemented) ----------------------*/

/* Bomb out if we get any of these. */

static void panic(const char *str) __attribute__((unused));
static void panic(const char *str)
{
   VALGRIND_PRINTF_BACKTRACE("Program aborting because of call to %s\n", str);
   my_exit(1);
}

#define PANIC(soname, fnname) \
   \
   void VG_REPLACE_FUNCTION_EZU(10190,soname,fnname) ( void ); \
   void VG_REPLACE_FUNCTION_EZU(10190,soname,fnname) ( void )  \
   { \
      panic(#fnname); \
   }

#if defined(VGO_linux)
 PANIC(VG_Z_LIBC_SONAME, pvalloc);
 PANIC(VG_Z_LIBC_SONAME, malloc_get_state);
 PANIC(VG_Z_LIBC_SONAME, malloc_set_state);

#elif defined(VGO_darwin)
 PANIC(VG_Z_LIBC_SONAME, pvalloc);
 PANIC(VG_Z_LIBC_SONAME, malloc_get_state);
 PANIC(VG_Z_LIBC_SONAME, malloc_set_state);

#endif


#define MALLOC_STATS(soname, fnname) \
   \
   void VG_REPLACE_FUNCTION_EZU(10200,soname,fnname) ( void ); \
   void VG_REPLACE_FUNCTION_EZU(10200,soname,fnname) ( void )  \
   { \
      /* Valgrind's malloc_stats implementation does nothing. */ \
   }

#if defined(VGO_linux)
 MALLOC_STATS(VG_Z_LIBC_SONAME, malloc_stats);
 MALLOC_STATS(SO_SYN_MALLOC,    malloc_stats);

#elif defined(VGO_darwin)
 //MALLOC_STATS(VG_Z_LIBC_SONAME, malloc_stats);

#endif


/*---------------------- mallinfo ----------------------*/

// mi must be static;  if it is auto then Memcheck thinks it is
// uninitialised when used by the caller of this function, because Memcheck
// doesn't know that the call to mallinfo fills in mi.
#define MALLINFO(soname, fnname) \
   \
   struct vg_mallinfo VG_REPLACE_FUNCTION_EZU(10210,soname,fnname) ( void ); \
   struct vg_mallinfo VG_REPLACE_FUNCTION_EZU(10210,soname,fnname) ( void ) \
   { \
      static struct vg_mallinfo mi; \
      DO_INIT; \
      MALLOC_TRACE("mallinfo()\n"); \
      (void)VALGRIND_NON_SIMD_CALL1( info.mallinfo, &mi ); \
      return mi; \
   }

#if defined(VGO_linux)
 MALLINFO(VG_Z_LIBC_SONAME, mallinfo);
 MALLINFO(SO_SYN_MALLOC,    mallinfo);

#elif defined(VGO_darwin)
 //MALLINFO(VG_Z_LIBC_SONAME, mallinfo);

#endif


/*------------------ Darwin zone stuff ------------------*/

#if defined(VGO_darwin)

static size_t my_malloc_size ( void* zone, void* ptr )
{
   /* Implement "malloc_size" by handing the request through to the
      tool's .tl_usable_size method. */
   DO_INIT;
   TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord) zone);
   TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED((UWord) ptr);
   size_t res = (size_t)VALGRIND_NON_SIMD_CALL1(
                           info.tl_malloc_usable_size, ptr);
   return res;
}

/* Note that the (void*) casts below are a kludge which stops
   compilers complaining about the fact that the replacement
   functions aren't really of the right type. */
static vki_malloc_zone_t vg_default_zone = {
    NULL, // reserved1
    NULL, // reserved2
    (void*)my_malloc_size, // JRS fixme: is this right?
    (void*)VG_REPLACE_FUNCTION_EZU(10020,VG_Z_LIBC_SONAME,malloc_zone_malloc),
    (void*)VG_REPLACE_FUNCTION_EZU(10060,VG_Z_LIBC_SONAME,malloc_zone_calloc),
    (void*)VG_REPLACE_FUNCTION_EZU(10130,VG_Z_LIBC_SONAME,malloc_zone_valloc),
    (void*)VG_REPLACE_FUNCTION_EZU(10040,VG_Z_LIBC_SONAME,malloc_zone_free),
    (void*)VG_REPLACE_FUNCTION_EZU(10080,VG_Z_LIBC_SONAME,malloc_zone_realloc),
    NULL, // GrP fixme: destroy
    "ValgrindMallocZone",
    NULL, // batch_malloc
    NULL, // batch_free
    NULL, // GrP fixme: introspect
    2,  // version (GrP fixme 3?)
    (void*)VG_REPLACE_FUNCTION_EZU(10100,VG_Z_LIBC_SONAME,malloc_zone_memalign), // DDD: this field exists in Mac OS 10.6+
    NULL, /* free_definite_size */
    NULL, /* pressure_relief */
};


#define DEFAULT_ZONE(soname, fnname) \
   \
   void *VG_REPLACE_FUNCTION_EZU(10220,soname,fnname) ( void ); \
   void *VG_REPLACE_FUNCTION_EZU(10220,soname,fnname) ( void )  \
   { \
      return &vg_default_zone; \
   }

DEFAULT_ZONE(VG_Z_LIBC_SONAME, malloc_default_zone);
DEFAULT_ZONE(SO_SYN_MALLOC,    malloc_default_zone);
DEFAULT_ZONE(VG_Z_LIBC_SONAME, malloc_default_purgeable_zone);
DEFAULT_ZONE(SO_SYN_MALLOC,    malloc_default_purgeable_zone);


#define CREATE_ZONE(soname, fnname) \
   \
   void *VG_REPLACE_FUNCTION_EZU(10230,soname,fnname)(size_t sz, unsigned fl); \
   void *VG_REPLACE_FUNCTION_EZU(10230,soname,fnname)(size_t sz, unsigned fl)  \
   { \
      return &vg_default_zone; \
   }
CREATE_ZONE(VG_Z_LIBC_SONAME, malloc_create_zone);


#define ZONE_FROM_PTR(soname, fnname) \
   \
   void *VG_REPLACE_FUNCTION_EZU(10240,soname,fnname) ( void* ptr ); \
   void *VG_REPLACE_FUNCTION_EZU(10240,soname,fnname) ( void* ptr )  \
   { \
      return &vg_default_zone; \
   }

ZONE_FROM_PTR(VG_Z_LIBC_SONAME, malloc_zone_from_ptr);
ZONE_FROM_PTR(SO_SYN_MALLOC,    malloc_zone_from_ptr);


// GrP fixme bypass libc's use of zone->introspect->check
#define ZONE_CHECK(soname, fnname) \
   \
   int VG_REPLACE_FUNCTION_EZU(10250,soname,fnname)(void* zone); \
   int VG_REPLACE_FUNCTION_EZU(10250,soname,fnname)(void* zone)  \
   { \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(zone); \
      panic(#fnname); \
      return 1; \
   }

ZONE_CHECK(VG_Z_LIBC_SONAME, malloc_zone_check);
ZONE_CHECK(SO_SYN_MALLOC,    malloc_zone_check);


#define ZONE_REGISTER(soname, fnname) \
   \
   void VG_REPLACE_FUNCTION_EZU(10260,soname,fnname)(void* zone); \
   void VG_REPLACE_FUNCTION_EZU(10260,soname,fnname)(void* zone)  \
   { \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(zone); \
   }

ZONE_REGISTER(VG_Z_LIBC_SONAME, malloc_zone_register);
ZONE_REGISTER(SO_SYN_MALLOC,    malloc_zone_register);


#define ZONE_UNREGISTER(soname, fnname) \
   \
   void VG_REPLACE_FUNCTION_EZU(10270,soname,fnname)(void* zone); \
   void VG_REPLACE_FUNCTION_EZU(10270,soname,fnname)(void* zone)  \
   { \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(zone); \
   }

ZONE_UNREGISTER(VG_Z_LIBC_SONAME, malloc_zone_unregister);
ZONE_UNREGISTER(SO_SYN_MALLOC,    malloc_zone_unregister);


#define ZONE_SET_NAME(soname, fnname) \
   \
   void VG_REPLACE_FUNCTION_EZU(10280,soname,fnname)(void* zone, char* nm); \
   void VG_REPLACE_FUNCTION_EZU(10280,soname,fnname)(void* zone, char* nm)  \
   { \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(zone); \
   }

ZONE_SET_NAME(VG_Z_LIBC_SONAME, malloc_set_zone_name);
ZONE_SET_NAME(SO_SYN_MALLOC,    malloc_set_zone_name);


#define ZONE_GET_NAME(soname, fnname) \
   \
   const char* VG_REPLACE_FUNCTION_EZU(10290,soname,fnname)(void* zone); \
   const char* VG_REPLACE_FUNCTION_EZU(10290,soname,fnname)(void* zone)  \
   { \
      TRIGGER_MEMCHECK_ERROR_IF_UNDEFINED(zone); \
      return vg_default_zone.zone_name; \
   }

ZONE_GET_NAME(VG_Z_LIBC_SONAME, malloc_get_zone_name);
ZONE_GET_NAME(SO_SYN_MALLOC,    malloc_get_zone_name);

#endif /* defined(VGO_darwin) */


/*------------------ (startup related) ------------------*/

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

   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__GET_MALLOCFUNCS, &info,
                                   0, 0, 0, 0);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
