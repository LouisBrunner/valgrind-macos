
/*--------------------------------------------------------------------*/
/*--- Replacements for malloc() et al, which run on the simulated  ---*/
/*--- CPU.                                     vg_replace_malloc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
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

   This file can be #included into a skin that wishes to know about
   calls to malloc().  It should define functions SK_(malloc) et al
   that will be called.
   ------------------------------------------------------------------ */

#include "vg_include.h"

/* Sidestep the normal check which disallows using valgrind.h directly. */
#define __VALGRIND_SOMESKIN_H
#include "valgrind.h"            /* for VALGRIND_NON_SIMD_tstCALL[12] */

/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

/* Round malloc sizes upwards to integral number of words? default: NO */
Bool VG_(clo_sloppy_malloc)  = False;

/* DEBUG: print malloc details?  default: NO */
Bool VG_(clo_trace_malloc)   = False;

/* Minimum alignment in functions that don't specify alignment explicitly.
   default: 0, i.e. use default of the machine (== 4) */
Int  VG_(clo_alignment) = 4;


Bool VG_(replacement_malloc_process_cmd_line_option)(Char* arg)
{
   if      (VG_CLO_STREQN(12, arg, "--alignment=")) {
      VG_(clo_alignment) = (Int)VG_(atoll)(&arg[12]);

      if (VG_(clo_alignment) < 4 
          || VG_(clo_alignment) > 4096
          || VG_(log2)( VG_(clo_alignment) ) == -1 /* not a power of 2 */) {
         VG_(message)(Vg_UserMsg, "");
         VG_(message)(Vg_UserMsg, 
            "Invalid --alignment= setting.  "
            "Should be a power of 2, >= 4, <= 4096.");
         VG_(bad_option)("--alignment");
      }
   }

   else if (VG_CLO_STREQ(arg, "--sloppy-malloc=yes"))
      VG_(clo_sloppy_malloc) = True;
   else if (VG_CLO_STREQ(arg, "--sloppy-malloc=no"))
      VG_(clo_sloppy_malloc) = False;

   else if (VG_CLO_STREQ(arg, "--trace-malloc=yes"))
      VG_(clo_trace_malloc) = True;
   else if (VG_CLO_STREQ(arg, "--trace-malloc=no"))
      VG_(clo_trace_malloc) = False;

   else 
      return False;

   return True;
}

void VG_(replacement_malloc_print_usage)(void)
{
   VG_(printf)(
"    --sloppy-malloc=no|yes    round malloc sizes to next word? [no]\n"
"    --alignment=<number>      set minimum alignment of allocations [4]\n"
   );
}

void VG_(replacement_malloc_print_debug_usage)(void)
{
   VG_(printf)(
"    --trace-malloc=no|yes     show client malloc details? [no]\n"
   );
}


/*------------------------------------------------------------*/
/*--- Replacing malloc() et al                             ---*/
/*------------------------------------------------------------*/

/* Below are new versions of malloc, __builtin_new, free, 
   __builtin_delete, calloc, realloc, memalign, and friends.

   malloc, __builtin_new, free, __builtin_delete, calloc and realloc
   can be entered either on the real CPU or the simulated one.  If on
   the real one, this is because the dynamic linker is running the
   static initialisers for C++, before starting up Valgrind itself.
   In this case it is safe to route calls through to
   VG_(arena_malloc)/VG_(arena_free), since they are self-initialising.

   Once Valgrind is initialised, vg_running_on_simd_CPU becomes True.
   The call needs to be transferred from the simulated CPU back to the
   real one and routed to the VG_(cli_malloc)() or VG_(cli_free)().  To do
   that, the client-request mechanism (in valgrind.h) is used to convey
   requests to the scheduler.
*/

#define MALLOC_TRACE(format, args...)  \
   if (VG_(clo_trace_malloc))          \
      VG_(printf)(format, ## args )

#define MAYBE_SLOPPIFY(n)           \
   if (VG_(clo_sloppy_malloc)) {    \
      while ((n % 4) > 0) n++;      \
   }

/* ALL calls to malloc wind up here. */
void* malloc ( Int n )
{
   void* v;

   MALLOC_TRACE("malloc[simd=%d](%d)", 
                (UInt)VG_(is_running_on_simd_CPU)(), n );
   MAYBE_SLOPPIFY(n);

   if (VG_(is_running_on_simd_CPU)()) {
      v = (void*)VALGRIND_NON_SIMD_tstCALL1( SK_(malloc), n );
   } else if (VG_(clo_alignment) != 4) {
      v = VG_(arena_malloc_aligned)(VG_AR_CLIENT, VG_(clo_alignment), n);
   } else {
      v = VG_(arena_malloc)(VG_AR_CLIENT, n);
   }
   MALLOC_TRACE(" = %p\n", v );
   return v;
}

void* __builtin_new ( Int n )
{
   void* v;

   MALLOC_TRACE("__builtin_new[simd=%d](%d)", 
                (UInt)VG_(is_running_on_simd_CPU)(), n );
   MAYBE_SLOPPIFY(n);

   if (VG_(is_running_on_simd_CPU)()) {
      v = (void*)VALGRIND_NON_SIMD_tstCALL1( SK_(__builtin_new), n );
   } else if (VG_(clo_alignment) != 4) {
      v = VG_(arena_malloc_aligned)(VG_AR_CLIENT, VG_(clo_alignment), n);
   } else {
      v = VG_(arena_malloc)(VG_AR_CLIENT, n);
   }
   MALLOC_TRACE(" = %p\n", v );
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

   MALLOC_TRACE("__builtin_vec_new[simd=%d](%d)", 
                (UInt)VG_(is_running_on_simd_CPU)(), n );
   MAYBE_SLOPPIFY(n);

   if (VG_(is_running_on_simd_CPU)()) {
      v = (void*)VALGRIND_NON_SIMD_tstCALL1( SK_(__builtin_vec_new), n );
   } else if (VG_(clo_alignment) != 4) {
      v = VG_(arena_malloc_aligned)(VG_AR_CLIENT, VG_(clo_alignment), n);
   } else {
      v = VG_(arena_malloc)(VG_AR_CLIENT, n);
   }
   MALLOC_TRACE(" = %p\n", v );
   return v;
}

/* gcc 3.X.X mangles them differently. */
void* _Znaj ( Int n )
{
  return __builtin_vec_new(n);
}

void free ( void* p )
{
   MALLOC_TRACE("free[simd=%d](%p)\n", 
                (UInt)VG_(is_running_on_simd_CPU)(), p );
   if (p == NULL) 
      return;
   if (VG_(is_running_on_simd_CPU)()) {
      (void)VALGRIND_NON_SIMD_tstCALL1( SK_(free), p );
   } else {
      VG_(arena_free)(VG_AR_CLIENT, p);      
   }
}

void __builtin_delete ( void* p )
{
   MALLOC_TRACE("__builtin_delete[simd=%d](%p)\n", 
                (UInt)VG_(is_running_on_simd_CPU)(), p );
   if (p == NULL) 
      return;
   if (VG_(is_running_on_simd_CPU)()) {
      (void)VALGRIND_NON_SIMD_tstCALL1( SK_(__builtin_delete), p );
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
   MALLOC_TRACE("__builtin_vec_delete[simd=%d](%p)\n", 
                (UInt)VG_(is_running_on_simd_CPU)(), p );
   if (p == NULL) 
      return;
   if (VG_(is_running_on_simd_CPU)()) {
      (void)VALGRIND_NON_SIMD_tstCALL1( SK_(__builtin_vec_delete), p );
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

   MALLOC_TRACE("calloc[simd=%d](%d,%d)", 
                (UInt)VG_(is_running_on_simd_CPU)(), nmemb, size );
   MAYBE_SLOPPIFY(size);

   if (VG_(is_running_on_simd_CPU)()) {
      v = (void*)VALGRIND_NON_SIMD_tstCALL2( SK_(calloc), nmemb, size );
   } else {
      v = VG_(arena_calloc)(VG_AR_CLIENT, VG_(clo_alignment), nmemb, size);
   }
   MALLOC_TRACE(" = %p\n", v );
   return v;
}


void* realloc ( void* ptrV, Int new_size )
{
   void* v;

   MALLOC_TRACE("realloc[simd=%d](%p,%d)", 
                (UInt)VG_(is_running_on_simd_CPU)(), ptrV, new_size );
   MAYBE_SLOPPIFY(new_size);

   if (ptrV == NULL)
      return malloc(new_size);
   if (new_size <= 0) {
      free(ptrV);
      if (VG_(clo_trace_malloc)) 
         VG_(printf)(" = 0\n" );
      return NULL;
   }   
   if (VG_(is_running_on_simd_CPU)()) {
      v = (void*)VALGRIND_NON_SIMD_tstCALL2( SK_(realloc), ptrV, new_size );
   } else {
      v = VG_(arena_realloc)(VG_AR_CLIENT, ptrV, VG_(clo_alignment), new_size);
   }
   MALLOC_TRACE(" = %p\n", v );
   return v;
}


void* memalign ( Int alignment, Int n )
{
   void* v;

   MALLOC_TRACE("memalign[simd=%d](al %d, size %d)", 
                (UInt)VG_(is_running_on_simd_CPU)(), alignment, n );
   MAYBE_SLOPPIFY(n);

   if (VG_(is_running_on_simd_CPU)()) {
      v = (void*)VALGRIND_NON_SIMD_tstCALL2( SK_(memalign), alignment, n );
   } else {
      v = VG_(arena_malloc_aligned)(VG_AR_CLIENT, alignment, n);
   }
   MALLOC_TRACE(" = %p\n", v );
   return v;
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

Int malloc_usable_size ( void* p )
{ 
   Int pszB;
   
   MALLOC_TRACE("malloc_usable_size[simd=%d](%p)", 
                (UInt)VG_(is_running_on_simd_CPU)(), p );
   if (NULL == p)
      return 0;

   if (VG_(is_running_on_simd_CPU)()) {
      pszB = (Int)VALGRIND_NON_SIMD_CALL2( VG_(arena_payload_szB), 
                                           VG_AR_CLIENT, p );
   } else {
      pszB = VG_(arena_payload_szB)(VG_AR_CLIENT, p);
   }
   MALLOC_TRACE(" = %d\n", pszB );

   return pszB;
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
   UInt            i;
   struct mallinfo mi;
   UChar*          pmi = (UChar*)(&mi);
   for (i = 0; i < sizeof(mi); i++)
      pmi[i] = 0;
   return mi;
}

/*--------------------------------------------------------------------*/
/*--- end                                      vg_replace_malloc.c ---*/
/*--------------------------------------------------------------------*/
