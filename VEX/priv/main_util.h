
/*---------------------------------------------------------------*/
/*--- begin                                       main_util.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
      info@open-works.net

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __VEX_MAIN_UTIL_H
#define __VEX_MAIN_UTIL_H

#include "libvex_basictypes.h"

#include "libvex_inner.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
/* Including here memcheck include file is kind of a hack, but this is needed
   to have self-hosting checking VEX. Note however that this is included only
   when Valgrind and VEX are configured using --enable-inner. */
#include "memcheck/memcheck.h"
#endif

/* Misc. */

#define NULL ((void*)0)

#if defined(_MSC_VER) // building with MSVC
# define LIKELY(x)          (x)
# define UNLIKELY(x)        (x)
# define CAST_TO_TYPEOF(x)  /**/
#else
# define LIKELY(x)          __builtin_expect(!!(x), 1)
# define UNLIKELY(x)        __builtin_expect(!!(x), 0)
# define CAST_TO_TYPEOF(x)  (__typeof__(x))
#endif // defined(_MSC_VER)

#if !defined(offsetof)
#   define offsetof(type,memb) ((SizeT)(HWord)&((type*)0)->memb)
#endif

// Poor man's static assert
#define STATIC_ASSERT(x)  extern int vex__unused_array[(x) ? 1 : -1] \
                                     __attribute__((unused))

/* Stuff for panicking and assertion. */

#define vassert(expr)                                           \
  ((void) (LIKELY(expr) ? 0 :                                   \
           (vex_assert_fail (#expr,                             \
                             __FILE__, __LINE__,                \
                             __PRETTY_FUNCTION__), 0)))

__attribute__ ((__noreturn__))
extern void vex_assert_fail ( const HChar* expr, const HChar* file,
                              Int line, const HChar* fn );
__attribute__ ((__noreturn__))
extern void vpanic ( const HChar* str );

__attribute__ ((__noreturn__)) __attribute__ ((format (printf, 1, 2)))
extern void vfatal ( const HChar* format, ... );


/* Printing */

__attribute__ ((format (printf, 1, 2)))
extern UInt vex_printf ( const HChar *format, ... );

__attribute__ ((format (printf, 2, 3)))
extern UInt vex_sprintf ( HChar* buf, const HChar *format, ... );


/* String ops */

extern Bool vex_streq ( const HChar* s1, const HChar* s2 );
extern SizeT vex_strlen ( const HChar* str );
extern void vex_bzero ( void* s, SizeT n );


/* Math ops */

/* Sign extend an N-bit value up to 64 bits, by copying bit N-1 into all higher
   positions. */
static inline ULong vex_sx_to_64( ULong x, UInt n )
{
   vassert(n >= 1 && n < 64);
   return (ULong)((Long)(x << (64 - n)) >> (64 - n));
}


/* Storage management: clear the area, and allocate from it. */

/* By default allocation occurs in the temporary area.  However, it is
   possible to switch to permanent area allocation if that's what you
   want.  Permanent area allocation is very limited, tho. */

typedef
   enum {
      VexAllocModeTEMP, 
      VexAllocModePERM 
   }
   VexAllocMode;

extern void         vexSetAllocMode ( VexAllocMode );
extern VexAllocMode vexGetAllocMode ( void );
extern void         vexAllocSanityCheck ( void );

extern void vexSetAllocModeTEMP_and_clear ( void );

/* Allocate in Vex's temporary allocation area.  Be careful with this.
   You can only call it inside an instrumentation or optimisation
   callback that you have previously specified in a call to
   LibVEX_Translate.  The storage allocated will only stay alive until
   translation of the current basic block is complete.
 */
extern HChar* private_LibVEX_alloc_first;
extern HChar* private_LibVEX_alloc_curr;
extern HChar* private_LibVEX_alloc_last;
extern void   private_LibVEX_alloc_OOM(void) __attribute__((noreturn));

/* Allocated memory as returned by LibVEX_Alloc will be aligned on this
   boundary. */
#define REQ_ALIGN 8

#if defined(ENABLE_INNER)
#define VEX_REDZONE_SIZEB (2*REQ_ALIGN)
#endif

static inline void* LibVEX_Alloc_inline ( SizeT nbytes )
{
   struct align {
      char c;
      union {
         char c;
         short s;
         int i;
         long l;
         long long ll;
         float f;
         double d;
         /* long double is currently not used and would increase alignment
            unnecessarily. */
         /* long double ld; */
         void *pto;
         void (*ptf)(void);
      } x;
   };

   /* Make sure the compiler does no surprise us */
   vassert(offsetof(struct align,x) <= REQ_ALIGN);

#if 0
  /* Nasty debugging hack, do not use. */
  return malloc(nbytes);
#else
   HChar* curr;
   HChar* next;
   SizeT  ALIGN;
   ALIGN  = offsetof(struct align,x) - 1;
   curr   = private_LibVEX_alloc_curr;
   next   = curr + ((nbytes + ALIGN) & ~ALIGN);
   INNER_REQUEST(next += 2 * VEX_REDZONE_SIZEB);
   if (next >= private_LibVEX_alloc_last)
      private_LibVEX_alloc_OOM();
   private_LibVEX_alloc_curr = next;
   INNER_REQUEST(curr += VEX_REDZONE_SIZEB);
   INNER_REQUEST(VALGRIND_MEMPOOL_ALLOC(private_LibVEX_alloc_first,
                                        curr, nbytes));
   return curr;
#endif
}

/* Misaligned memory access support. */

extern UInt  read_misaligned_UInt_LE  ( void* addr );
extern ULong read_misaligned_ULong_LE ( void* addr );

extern void  write_misaligned_UInt_LE  ( void* addr, UInt  w );
extern void  write_misaligned_ULong_LE ( void* addr, ULong w );

#endif /* ndef __VEX_MAIN_UTIL_H */

/*---------------------------------------------------------------*/
/*---                                             main_util.h ---*/
/*---------------------------------------------------------------*/
