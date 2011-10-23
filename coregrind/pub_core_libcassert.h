
/*--------------------------------------------------------------------*/
/*--- Assertions, etc.                       pub_core_libcassert.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2011 Julian Seward
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

#ifndef __PUB_CORE_LIBCASSERT_H
#define __PUB_CORE_LIBCASSERT_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the libc code related to assertions,
// panics and aborting.
//--------------------------------------------------------------------

#include "pub_tool_libcassert.h"

// Useful for making failing stubs, when certain things haven't yet been
// implemented.
#define I_die_here                                             \
   VG_(assert_fail) (/*isCore*//*BOGUS*/True,                  \
                     "Unimplemented functionality",            \
                     __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                     "valgrind", VG_BUGS_TO, "")

#define vg_assert(expr)                                                 \
  ((void) (LIKELY(expr) ? 0 :                                           \
           (VG_(assert_fail) (/*isCore*/True, #expr,                    \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              ""),                                      \
                              0)))

#define vg_assert2(expr, format, args...)                               \
  ((void) (LIKELY(expr) ? 0 :                                           \
           (VG_(assert_fail) (/*isCore*/True, #expr,                    \
                              __FILE__, __LINE__, __PRETTY_FUNCTION__,  \
                              format, ##args),                          \
                              0)))

__attribute__ ((__noreturn__))
extern void  VG_(core_panic)      ( Char* str );
__attribute__ ((__noreturn__))
extern void  VG_(core_panic_at)   ( Char* str, UnwindStartRegs* );

/* Called when some unhandleable client behaviour is detected.
   Prints a msg and aborts. */
extern void VG_(unimplemented) ( Char* msg )
            __attribute__((__noreturn__));

/* Show the state of all threads.  Mostly for debugging V. */
extern void VG_(show_sched_status) ( void );

#endif   // __PUB_CORE_LIBCASSERT_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
