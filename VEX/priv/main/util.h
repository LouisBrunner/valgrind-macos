
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (util.h) is                                   ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBJIT_UTIL_H
#define __LIBJIT_UTIL_H

#include "libjit_basictypes.h"


/* Stuff for panicking and assertion. */

#define VG__STRING(__str)  #__str

#define assert(expr)                                            \
  ((void) ((expr) ? 0 :                                         \
           (vex_assert_fail (VG__STRING(expr),                  \
                             __FILE__, __LINE__,                \
                             __PRETTY_FUNCTION__), 0)))

__attribute__ ((__noreturn__))
extern void vex_assert_fail ( const Char* expr, const Char* file,
                              Int line, const Char* fn );
__attribute__ ((__noreturn__))
extern void panic ( Char* str );


#endif /* ndef __LIBJIT_UTIL_H */

/*---------------------------------------------------------------*/
/*---                                                  util.h ---*/
/*---------------------------------------------------------------*/
