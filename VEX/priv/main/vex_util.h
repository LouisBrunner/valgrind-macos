
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (vex_util.h) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __VEX_UTIL_H
#define __VEX_UTIL_H

#include "libvex_basictypes.h"


/* Misc. */

#define NULL ((void*)0)


/* Stuff for panicking and assertion. */

#define VG__STRING(__str)  #__str

#define vassert(expr)                                           \
  ((void) ((expr) ? 0 :                                         \
           (vex_assert_fail (VG__STRING(expr),                  \
                             __FILE__, __LINE__,                \
                             __PRETTY_FUNCTION__), 0)))

__attribute__ ((__noreturn__))
extern void vex_assert_fail ( const Char* expr, const Char* file,
                              Int line, const Char* fn );
__attribute__ ((__noreturn__))
extern void vpanic ( Char* str );


/* Printing */

__attribute__ ((format (printf, 1, 2)))
extern UInt vex_printf ( const Char *format, ... );

__attribute__ ((format (printf, 2, 3)))
extern UInt vex_sprintf ( Char* buf, const Char *format, ... );

#endif /* ndef __VEX_UTIL_H */

/*---------------------------------------------------------------*/
/*---                                              vex_util.h ---*/
/*---------------------------------------------------------------*/
