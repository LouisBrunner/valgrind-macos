
/*---------------------------------------------------------------*/
/*--- begin                                       main_util.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2012 OpenWorks LLP
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __VEX_MAIN_UTIL_H
#define __VEX_MAIN_UTIL_H

#include "libvex_basictypes.h"


/* Misc. */

#define NULL ((void*)0)

#define LIKELY(x)       __builtin_expect(!!(x), 1)
#define UNLIKELY(x)     __builtin_expect(!!(x), 0)

/* Stuff for panicking and assertion. */

#define VG__STRING(__str)  #__str

#define vassert(expr)                                           \
  ((void) (LIKELY(expr) ? 0 :                                   \
           (vex_assert_fail (VG__STRING(expr),                  \
                             __FILE__, __LINE__,                \
                             __PRETTY_FUNCTION__), 0)))

__attribute__ ((__noreturn__))
extern void vex_assert_fail ( const HChar* expr, const HChar* file,
                              Int line, const HChar* fn );
__attribute__ ((__noreturn__))
extern void vpanic ( HChar* str );


/* Printing */

__attribute__ ((format (printf, 1, 2)))
extern UInt vex_printf ( HChar *format, ... );

__attribute__ ((format (printf, 2, 3)))
extern UInt vex_sprintf ( HChar* buf, HChar *format, ... );


/* String ops */

extern Bool vex_streq ( const HChar* s1, const HChar* s2 );
extern Int  vex_strlen ( const HChar* str );
extern void vex_bzero ( void* s, UInt n );


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

#endif /* ndef __VEX_MAIN_UTIL_H */

/*---------------------------------------------------------------*/
/*---                                             main_util.h ---*/
/*---------------------------------------------------------------*/
