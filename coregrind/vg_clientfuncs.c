
/*--------------------------------------------------------------------*/
/*--- Code which runs on the simulated CPU.                        ---*/
/*---                                             vg_clientfuncs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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

#include "vg_include.h"

#include "valgrind.h"   /* for VALGRIND_MAGIC_SEQUENCE */


/* ---------------------------------------------------------------------
   All the code in this file runs on the SIMULATED CPU.  It is
   intended for various reasons as drop-in replacements for libc
   functions.  These functions have global visibility (obviously) and
   have no prototypes in vg_include.h, since they are not intended to
   be called from within Valgrind.
   ------------------------------------------------------------------ */

/* ---------------------------------------------------------------------
   Intercepts for the GNU malloc interface.
   ------------------------------------------------------------------ */

#define SIMPLE_REQUEST1(_qyy_request, _qyy_arg1)                 \
   ({unsigned int _qyy_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qyy_res, 0 /* default return */,    \
                            _qyy_request,                        \
                            _qyy_arg1, 0, 0, 0);                 \
    _qyy_res;                                                    \
   })

#define SIMPLE_REQUEST2(_qyy_request, _qyy_arg1, _qyy_arg2)      \
   ({unsigned int _qyy_res;                                      \
    VALGRIND_MAGIC_SEQUENCE(_qyy_res, 0 /* default return */,    \
                            _qyy_request,                        \
                            _qyy_arg1, _qyy_arg2, 0, 0);         \
    _qyy_res;                                                    \
   })


/* Below are new versions of malloc, __builtin_new, free, 
   __builtin_delete, calloc and realloc.

   malloc, __builtin_new, free, __builtin_delete, calloc and realloc
   can be entered either on the real CPU or the simulated one.  If on
   the real one, this is because the dynamic linker is running the
   static initialisers for C++, before starting up Valgrind itself.
   In this case it is safe to route calls through to
   VG_(arena_malloc)/VG_(arena_free), since they are self-initialising.

   Once Valgrind is initialised, vg_running_on_simd_CPU becomes True.
   The call needs to be transferred from the simulated CPU back to the
   real one and routed to the vg_client_* functions.  To do that, the
   client-request mechanism (in valgrind.h) is used to convey requests
   to the scheduler.
*/

/* ALL calls to malloc wind up here. */
void* malloc ( Int n )
{
   void* v;

   if (VG_(clo_trace_malloc))
      VG_(printf)("malloc[simd=%d](%d)", 
                  (UInt)VG_(running_on_simd_CPU), n );
   if (n < 0) {
      v = NULL;
      if (VG_(needs).core_errors)
         VG_(message)(Vg_UserMsg, 
                      "Warning: silly arg (%d) to malloc()", n );
   } else {
      if (VG_(clo_sloppy_malloc)) { while ((n % 4) > 0) n++; }

      if (VG_(running_on_simd_CPU)) {
         v = (void*)SIMPLE_REQUEST1(VG_USERREQ__MALLOC, n);
      } else {
         v = VG_(arena_malloc)(VG_AR_CLIENT, n);
      }
   }
   if (VG_(clo_trace_malloc)) 
      VG_(printf)(" = %p\n", v );
   return (void*)v;
}

void* __builtin_new ( Int n )
{
   void* v;

   if (VG_(clo_trace_malloc))
      VG_(printf)("__builtin_new[simd=%d](%d)", 
                  (UInt)VG_(running_on_simd_CPU), n );
   if (n < 0) {
      v = NULL;
      if (VG_(needs).core_errors)
         VG_(message)(Vg_UserMsg, 
                      "Warning: silly arg (%d) to __builtin_new()", n );
   } else {
      if (VG_(clo_sloppy_malloc)) { while ((n % 4) > 0) n++; }

      if (VG_(running_on_simd_CPU)) {
         v = (void*)SIMPLE_REQUEST1(VG_USERREQ__BUILTIN_NEW, n);
      } else {
         v = VG_(arena_malloc)(VG_AR_CLIENT, n);
      }
   }
   if (VG_(clo_trace_malloc)) 
      VG_(printf)(" = %p\n", v );
   return v;
}

/* gcc 3.X.X mangles them differently. */
void* _Znwj ( Int n )
{
  return __builtin_new(n);
}

void* __builtin_vec_new ( Int n )
{
   void* v;

   if (VG_(clo_trace_malloc))
      VG_(printf)("__builtin_vec_new[simd=%d](%d)", 
                  (UInt)VG_(running_on_simd_CPU), n );
   if (n < 0) {
      v = NULL;
      if (VG_(needs).core_errors)
         VG_(message)(Vg_UserMsg, 
                      "Warning: silly arg (%d) to __builtin_vec_new()", n );
   } else {
      if (VG_(clo_sloppy_malloc)) { while ((n % 4) > 0) n++; }

      if (VG_(running_on_simd_CPU)) {
         v = (void*)SIMPLE_REQUEST1(VG_USERREQ__BUILTIN_VEC_NEW, n);
      } else {
         v = VG_(arena_malloc)(VG_AR_CLIENT, n);
      }
   }
   if (VG_(clo_trace_malloc)) 
      VG_(printf)(" = %p\n", v );
   return v;
}

/* gcc 3.X.X mangles them differently. */
void* _Znaj ( Int n )
{
  return __builtin_vec_new(n);
}

void free ( void* p )
{
   if (VG_(clo_trace_malloc))
      VG_(printf)("free[simd=%d](%p)\n", 
                  (UInt)VG_(running_on_simd_CPU), p );
   if (p == NULL) 
      return;
   if (VG_(running_on_simd_CPU)) {
      (void)SIMPLE_REQUEST1(VG_USERREQ__FREE, p);
   } else {
      VG_(arena_free)(VG_AR_CLIENT, p);      
   }
}

void __builtin_delete ( void* p )
{
   if (VG_(clo_trace_malloc))
      VG_(printf)("__builtin_delete[simd=%d](%p)\n", 
                  (UInt)VG_(running_on_simd_CPU), p );
   if (p == NULL) 
      return;
   if (VG_(running_on_simd_CPU)) {
      (void)SIMPLE_REQUEST1(VG_USERREQ__BUILTIN_DELETE, p);
   } else {
      VG_(arena_free)(VG_AR_CLIENT, p);
   }
}

/* gcc 3.X.X mangles them differently. */
void _ZdlPv ( void* p )
{
  __builtin_delete(p);
}

void __builtin_vec_delete ( void* p )
{
   if (VG_(clo_trace_malloc))
       VG_(printf)("__builtin_vec_delete[simd=%d](%p)\n", 
                   (UInt)VG_(running_on_simd_CPU), p );
   if (p == NULL) 
      return;
   if (VG_(running_on_simd_CPU)) {
      (void)SIMPLE_REQUEST1(VG_USERREQ__BUILTIN_VEC_DELETE, p);
   } else {
      VG_(arena_free)(VG_AR_CLIENT, p);
   }
}

/* gcc 3.X.X mangles them differently. */
void _ZdaPv ( void* p )
{
  __builtin_vec_delete(p);
}

void* calloc ( Int nmemb, Int size )
{
   void* v;

   if (VG_(clo_trace_malloc))
      VG_(printf)("calloc[simd=%d](%d,%d)", 
                  (UInt)VG_(running_on_simd_CPU), nmemb, size );
   if (nmemb < 0 || size < 0) {
      v = NULL;
      if (VG_(needs).core_errors)
         VG_(message)(Vg_UserMsg, "Warning: silly args (%d,%d) to calloc()", 
                                  nmemb, size );
   } else {
      if (VG_(running_on_simd_CPU)) {
         v = (void*)SIMPLE_REQUEST2(VG_USERREQ__CALLOC, nmemb, size);
      } else {
         v = VG_(arena_calloc)(VG_AR_CLIENT, nmemb, size);
      }
   }
   if (VG_(clo_trace_malloc)) 
      VG_(printf)(" = %p\n", v );
   return v;
}


void* realloc ( void* ptrV, Int new_size )
{
   void* v;

   if (VG_(clo_trace_malloc))
      VG_(printf)("realloc[simd=%d](%p,%d)", 
                  (UInt)VG_(running_on_simd_CPU), ptrV, new_size );

   if (VG_(clo_sloppy_malloc)) 
      { while ((new_size % 4) > 0) new_size++; }

   if (ptrV == NULL)
      return malloc(new_size);
   if (new_size <= 0) {
      free(ptrV);
      if (VG_(clo_trace_malloc)) 
         VG_(printf)(" = 0\n" );
      return NULL;
   }   
   if (VG_(running_on_simd_CPU)) {
      v = (void*)SIMPLE_REQUEST2(VG_USERREQ__REALLOC, ptrV, new_size);
   } else {
      v = VG_(arena_realloc)(VG_AR_CLIENT, ptrV, /*alignment*/4, new_size);
   }
   if (VG_(clo_trace_malloc)) 
      VG_(printf)(" = %p\n", v );
   return v;
}


void* memalign ( Int alignment, Int n )
{
   void* v;

   if (VG_(clo_trace_malloc))
      VG_(printf)("memalign[simd=%d](al %d, size %d)", 
                  (UInt)VG_(running_on_simd_CPU), alignment, n );
   if (n < 0) {
      v = NULL;
   } else {
      if (VG_(clo_sloppy_malloc)) { while ((n % 4) > 0) n++; }

      if (VG_(running_on_simd_CPU)) {
         v = (void*)SIMPLE_REQUEST2(VG_USERREQ__MEMALIGN, alignment, n);
      } else {
         v = VG_(arena_malloc_aligned)(VG_AR_CLIENT, alignment, n);
      }
   }
   if (VG_(clo_trace_malloc)) 
      VG_(printf)(" = %p\n", v );
   return (void*)v;
}


void* valloc ( Int size )
{
   return memalign(VKI_BYTES_PER_PAGE, size);
}


/* Various compatibility wrapper functions, for glibc and libstdc++. */
void cfree ( void* p )
{
   free ( p );
}


int mallopt ( int cmd, int value )
{
   /* In glibc-2.2.4, 1 denotes a successful return value for mallopt */
   return 1;
}


int __posix_memalign ( void **memptr, UInt alignment, UInt size )
{
    void *mem;

    /* Test whether the SIZE argument is valid.  It must be a power of
       two multiple of sizeof (void *).  */
    if (size % sizeof (void *) != 0 || (size & (size - 1)) != 0)
       return VKI_EINVAL /*22*/ /*EINVAL*/;

    mem = memalign (alignment, size);

    if (mem != NULL) {
       *memptr = mem;
       return 0;
    }

    return VKI_ENOMEM /*12*/ /*ENOMEM*/;
}


/* Bomb out if we get any of these. */
/* HACK: We shouldn't call VG_(core_panic) or VG_(message) on the simulated
   CPU.  Really we should pass the request in the usual way, and
   Valgrind itself can do the panic.  Too tedious, however.  
*/
void pvalloc ( void )
{ VG_(core_panic)("call to pvalloc\n"); }
void malloc_stats ( void )
{ VG_(core_panic)("call to malloc_stats\n"); }
void malloc_usable_size ( void )
{ VG_(core_panic)("call to malloc_usable_size\n"); }
void malloc_trim ( void )
{ VG_(core_panic)("call to malloc_trim\n"); }
void malloc_get_state ( void )
{ VG_(core_panic)("call to malloc_get_state\n"); }
void malloc_set_state ( void )
{ VG_(core_panic)("call to malloc_set_state\n"); }


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

struct mallinfo mallinfo ( void )
{
   /* Should really try to return something a bit more meaningful */
   Int             i;
   struct mallinfo mi;
   UChar*          pmi = (UChar*)(&mi);
   for (i = 0; i < sizeof(mi); i++)
      pmi[i] = 0;
   return mi;
}


/* ---------------------------------------------------------------------
   Replace some C lib things with equivs which don't get
   spurious value warnings.  THEY RUN ON SIMD CPU!
   ------------------------------------------------------------------ */

char* strrchr ( const char* s, int c )
{
   UChar  ch   = (UChar)((UInt)c);
   UChar* p    = (UChar*)s;
   UChar* last = NULL;
   while (True) {
      if (*p == ch) last = p;
      if (*p == 0) return last;
      p++;
   }
}

char* strchr ( const char* s, int c )
{
   UChar  ch = (UChar)((UInt)c);
   UChar* p  = (UChar*)s;
   while (True) {
      if (*p == ch) return p;
      if (*p == 0) return NULL;
      p++;
   }
}

char* strcat ( char* dest, const char* src )
{
   Char* dest_orig = dest;
   while (*dest) dest++;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

unsigned int strlen ( const char* str )
{
   UInt i = 0;
   while (str[i] != 0) i++;
   return i;
}

char* strcpy ( char* dest, const char* src )
{
   Char* dest_orig = dest;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

int strncmp ( const unsigned char* s1, const unsigned char* s2, 
              unsigned int nmax )
{
   unsigned int n = 0;
   while (True) {
      if (n >= nmax) return 0;
      if (*s1 == 0 && *s2 == 0) return 0;
      if (*s1 == 0) return -1;
      if (*s2 == 0) return 1;

      if (*(unsigned char*)s1 < *(unsigned char*)s2) return -1;
      if (*(unsigned char*)s1 > *(unsigned char*)s2) return 1;

      s1++; s2++; n++;
   }
}

int strcmp ( const char* s1, const char* s2 )
{
   register unsigned char c1;
   register unsigned char c2;
   while (True) {
      c1 = *(unsigned char *)s1;
      c2 = *(unsigned char *)s2;
      if (c1 != c2) break;
      if (c1 == 0) break;
      s1++; s2++;
   }
   if ((unsigned char)c1 < (unsigned char)c2) return -1;
   if ((unsigned char)c1 > (unsigned char)c2) return 1;
   return 0;
}

void* memchr(const void *s, int c, unsigned int n)
{
   unsigned int i;
   UChar c0 = (UChar)c;
   UChar* p = (UChar*)s;
   for (i = 0; i < n; i++)
      if (p[i] == c0) return (void*)(&p[i]);
   return NULL;
}

void* memcpy( void *dst, const void *src, unsigned int len )
{
    register char *d;
    register char *s;
    if ( dst > src ) {
        d = (char *)dst + len - 1;
        s = (char *)src + len - 1;
        while ( len >= 4 ) {
            *d-- = *s--;
            *d-- = *s--;
            *d-- = *s--;
            *d-- = *s--;
            len -= 4;
	}
        while ( len-- ) {
            *d-- = *s--;
        }
    } else if ( dst < src ) {
        d = (char *)dst;
        s = (char *)src;
	while ( len >= 4 ) {
            *d++ = *s++;
            *d++ = *s++;
            *d++ = *s++;
            *d++ = *s++;
            len -= 4;
	}
        while ( len-- ) {
            *d++ = *s++;
	}
    }
    return dst;
}


/* ---------------------------------------------------------------------
   Horrible hack to make sigsuspend() sort-of work OK.  Same trick as
   for pause() in vg_libpthread.so.
   ------------------------------------------------------------------ */

/* Horrible because

   -- uses VG_(ksigprocmask), VG_(nanosleep) and vg_assert, which are 
      valgrind-native (not intended for client use).

   -- This is here so single-threaded progs (not linking libpthread.so)
      can see it.  But pause() should also be here.  ???
*/

/* Either libc supplies this (weak) or our libpthread.so supplies it
   (strong) in a threaded setting. 
*/
extern int* __errno_location ( void );


int sigsuspend ( /* const sigset_t * */ void* mask)
{
   unsigned int n_orig, n_now;
   struct vki_timespec nanosleep_interval;

   VALGRIND_MAGIC_SEQUENCE(n_orig, 0xFFFFFFFF /* default */,
                           VG_USERREQ__GET_N_SIGS_RETURNED, 
                           0, 0, 0, 0);
   vg_assert(n_orig != 0xFFFFFFFF);

   VG_(ksigprocmask)(VKI_SIG_SETMASK, mask, NULL);

   while (1) {
      VALGRIND_MAGIC_SEQUENCE(n_now, 0xFFFFFFFF /* default */,
                              VG_USERREQ__GET_N_SIGS_RETURNED, 
                              0, 0, 0, 0);
      vg_assert(n_now != 0xFFFFFFFF);
      vg_assert(n_now >= n_orig);
      if (n_now != n_orig) break;

      nanosleep_interval.tv_sec  = 0;
      nanosleep_interval.tv_nsec = 53 * 1000 * 1000; /* 53 milliseconds */
      /* It's critical here that valgrind's nanosleep implementation
         is nonblocking. */
      VG_(nanosleep)( &nanosleep_interval, NULL);
   }

   /* Maybe this is OK both in single and multithreaded setting. */
   * (__errno_location()) = -VKI_EINTR; /* == EINTR; */ 
   return -1;
}


/* ---------------------------------------------------------------------
   Hook for running __libc_freeres once the program exits.
   ------------------------------------------------------------------ */

void VG_(__libc_freeres_wrapper)( void )
{
   int res;
   extern void __libc_freeres(void);
   //__libc_freeres();
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__LIBC_FREERES_DONE, 0, 0, 0, 0);
   /*NOTREACHED*/
   vg_assert(12345+54321 == 999999);
}


/*--------------------------------------------------------------------*/
/*--- end                                         vg_clientfuncs.c ---*/
/*--------------------------------------------------------------------*/
