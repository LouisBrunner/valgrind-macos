
/*--------------------------------------------------------------------*/
/*--- Replacements for malloc() et al, which run on the simulated  ---*/
/*--- CPU.                                     vg_replace_malloc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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
   All the code in this file runs on the SIMULATED CPU.  It is
   intended for various reasons as drop-in replacements for malloc()
   and friends.  These functions have global visibility (obviously) and
   have no prototypes in vg_include.h, since they are not intended to
   be called from within Valgrind.

   This file can be #included into a tool that wishes to know about
   calls to malloc().  It should define functions SK_(malloc) et al
   that will be called.
   ------------------------------------------------------------------ */

#include "valgrind.h"            /* for VALGRIND_NON_SIMD_CALL[12] */
#include "vg_include.h"
#include "vg_skin.h"

/* Create an alias */
#define ALIAS(ret, name, args, toname)					\
   ret name args __attribute__((alias(#toname), visibility("protected")))

/* Declare a function, along with libc's various aliases */
#define LIBALIAS(ret, name, args)		\
	ALIAS(ret, __##name, args, name);	\
	ALIAS(ret, __libc_##name, args, name);	\
	ret name args

/*------------------------------------------------------------*/
/*--- Replacing malloc() et al                             ---*/
/*------------------------------------------------------------*/

static struct vg_mallocfunc_info info;
static int init_done;

/* Startup hook - called as init section */
static void init(void) __attribute__((constructor));

/* Below are new versions of malloc, __builtin_new, free, 
   __builtin_delete, calloc, realloc, memalign, and friends.

   None of these functions are called directly - they are not meant to
   be found by the dynamic linker.  They get called because
   vg_replace_malloc installs a bunch of code redirects which causes
   Valgrind to use these functions rather than the ones they're
   replacing.  That said, we certainly don't mind if the linker finds
   them, because it makes our life easier with respect to startup
   initialization order (we can't guarantee that our init routine will
   necessarily be called early enough to do the redirects before
   someone wants to allocate).
*/

#define MALLOC_TRACE(format, args...)  \
   if (info.clo_trace_malloc)          \
      VALGRIND_INTERNAL_PRINTF(format, ## args )

#define MAYBE_SLOPPIFY(n)           \
   if (info.clo_sloppy_malloc) {    \
      n = (n+3) & ~3;		    \
   }

/* ALL calls to malloc() and friends wind up here. */
#define ALLOC(fff, vgfff) \
LIBALIAS(void *, fff, (Int n))			\
{ \
   void* v; \
 \
   MALLOC_TRACE(#fff "(%d)", n ); \
   MAYBE_SLOPPIFY(n); \
   if (!init_done) init(); \
 \
   v = (void*)VALGRIND_NON_SIMD_CALL1( info.sk_##vgfff, n ); \
   MALLOC_TRACE(" = %p", v ); \
   return v; \
}
ALLOC( malloc,              malloc            );
ALLOC( __builtin_new,       __builtin_new     );
ALLOC( _Znwj,               __builtin_new     );

// operator new(unsigned, std::nothrow_t const&)
ALLOC( _ZnwjRKSt9nothrow_t, __builtin_new     );

ALLOC( __builtin_vec_new,   __builtin_vec_new );
ALLOC( _Znaj,               __builtin_vec_new );

// operator new[](unsigned, std::nothrow_t const&
ALLOC( _ZnajRKSt9nothrow_t, __builtin_vec_new );

#define FREE(fff, vgfff) \
LIBALIAS(void, fff, (void *p))			\
{ \
   MALLOC_TRACE(#fff "(%p)", p ); \
   if (p == NULL)  \
      return; \
   if (!init_done) init(); \
   (void)VALGRIND_NON_SIMD_CALL1( info.sk_##vgfff, p ); \
}
FREE( free,                 free                 );
FREE( __builtin_delete,     __builtin_delete     );
FREE( _ZdlPv,               __builtin_delete     );
FREE( __builtin_vec_delete, __builtin_vec_delete );
FREE( _ZdaPv,               __builtin_vec_delete );

LIBALIAS(void*, calloc, ( Int nmemb, Int size ))
{
   void* v;

   MALLOC_TRACE("calloc(%d,%d)", nmemb, size );
   MAYBE_SLOPPIFY(size);

   if (!init_done) init();
   v = (void*)VALGRIND_NON_SIMD_CALL2( info.sk_calloc, nmemb, size );
   MALLOC_TRACE(" = %p", v );
   return v;
}

LIBALIAS(void*, realloc, ( void* ptrV, Int new_size ))
{
   void* v;

   MALLOC_TRACE("realloc(%p,%d)", ptrV, new_size );
   MAYBE_SLOPPIFY(new_size);

   if (ptrV == NULL)
      return malloc(new_size);
   if (new_size <= 0) {
      free(ptrV);
      if (info.clo_trace_malloc) 
         VALGRIND_INTERNAL_PRINTF(" = 0" );
      return NULL;
   }   
   if (!init_done) init();
   v = (void*)VALGRIND_NON_SIMD_CALL2( info.sk_realloc, ptrV, new_size );
   MALLOC_TRACE(" = %p", v );
   return v;
}


LIBALIAS(void*, memalign, ( Int alignment, Int n ))
{
   void* v;

   MALLOC_TRACE("memalign(al %d, size %d)", alignment, n );
   MAYBE_SLOPPIFY(n);

   if (!init_done) init();
   v = (void*)VALGRIND_NON_SIMD_CALL2( info.sk_memalign, alignment, n );
   MALLOC_TRACE(" = %p", v );
   return v;
}


LIBALIAS(void*, valloc, ( Int size ))
{
   return memalign(VKI_BYTES_PER_PAGE, size);
}


/* Various compatibility wrapper functions, for glibc and libstdc++. */

/* Don't just alias free, otherwise people could get confused seeing
   cfree rather than free in error output */
LIBALIAS(void, cfree, ( void* p ) )
{
   free(p);
}

LIBALIAS(int, mallopt, ( int cmd, int value ))
{
   /* In glibc-2.2.4, 1 denotes a successful return value for mallopt */
   return 1;
}


LIBALIAS(int, posix_memalign, ( void **memptr, UInt alignment, UInt size ))
{
    void *mem;

    /* Test whether the alignment argument is valid.  It must be a power of
       two multiple of sizeof (void *).  */
    if (alignment % sizeof (void *) != 0 || (alignment & (alignment - 1)) != 0)
       return VKI_EINVAL /*22*/ /*EINVAL*/;

    mem = memalign (alignment, size);

    if (mem != NULL) {
       *memptr = mem;
       return 0;
    }

    return VKI_ENOMEM /*12*/ /*ENOMEM*/;
}

LIBALIAS(int, malloc_usable_size, ( void* p ))
{ 
   Int pszB;
   
   MALLOC_TRACE("malloc_usable_size(%p)", p );
   if (NULL == p)
      return 0;

   if (!init_done) init();
   pszB = (Int)VALGRIND_NON_SIMD_CALL2( info.arena_payload_szB, 
					VG_AR_CLIENT, p );
   MALLOC_TRACE(" = %d", pszB );

   return pszB;
}


/* Bomb out if we get any of these. */

extern void _exit(int);

static void panic(const char *str)
{
   VALGRIND_PRINTF_BACKTRACE("Program aborting because of call to %s", str);
   
   _exit(99);
   *(int *)0 = 'x';
}

#define PANIC(x)				\
   void x(void)					\
   {						\
      panic(#x);				\
   }

PANIC(pvalloc);
PANIC(malloc_stats);
PANIC(malloc_trim);
PANIC(malloc_get_state);
PANIC(malloc_set_state);


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

LIBALIAS(struct mallinfo, mallinfo, ( void ))
{
   /* Should really try to return something a bit more meaningful */
   UInt            i;
   struct mallinfo mi;
   UChar*          pmi = (UChar*)(&mi);
   for (i = 0; i < sizeof(mi); i++)
      pmi[i] = 0;
   return mi;
}

static const struct {
   const Char *libname;
   Addr		func;
} replacements[] =
{
#define E(pfx, x)	{ pfx #x, (Addr)x }
#define R(x)		E("", x), E("__libc_", x), E("__", x)

   /* alloc */
   R(malloc),
   R(__builtin_new),
   R(_Znwj),
   R(_ZnwjRKSt9nothrow_t),	/* operator new(unsigned, std::nothrow_t const&) */
   R(__builtin_vec_new),
   R(_Znaj),
   R(_ZnajRKSt9nothrow_t),	/* operator new[](unsigned, std::nothrow_t const& */
   R(calloc),
   R(realloc),
   R(memalign),
   R(valloc),
   R(cfree),
   R(posix_memalign),

   /* free */
   R(free),
   R(__builtin_delete),
   R(_ZdlPv),
   R(__builtin_vec_delete),
   R(_ZdaPv),

   /* misc */
   R(mallopt),
   R(malloc_usable_size),
   R(mallinfo),

   /* bad */
   R(pvalloc),
   R(malloc_stats),
   R(malloc_trim),
   R(malloc_get_state),
   R(malloc_set_state),   
#undef R
#undef S
#undef E
};

/* All the code in here is unused until this function is called */

static void init(void)
{
   int i;
   int res;

   if (init_done)
      return;

   init_done = 1;

   VALGRIND_MAGIC_SEQUENCE(res, -1, VG_USERREQ__GET_MALLOCFUNCS, &info, 0, 0, 0);

   for(i = 0; i < sizeof(replacements)/sizeof(*replacements); i++) {
#if 0
      /* doesn't seem much point - ld-linux.so will have already used
	 malloc/free before we run */
      VALGRIND_MAGIC_SEQUENCE(res, 0, VG_USERREQ__REGISTER_REDIRECT_ADDR, 
			      "soname:ld-linux.so.2", replacements[i].libname,
			      replacements[i].func, 0);
#endif
      VALGRIND_MAGIC_SEQUENCE(res, 0, VG_USERREQ__REGISTER_REDIRECT_ADDR, 
			      "soname:libc.so.6", replacements[i].libname,
			      replacements[i].func, 0);
      VALGRIND_MAGIC_SEQUENCE(res, 0, VG_USERREQ__REGISTER_REDIRECT_ADDR, 
			      "soname:libstdc++*", replacements[i].libname,
			      replacements[i].func, 0);
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                      vg_replace_malloc.c ---*/
/*--------------------------------------------------------------------*/
