
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (basictypes.h) is                             ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __BASICTYPES_H
#define __BASICTYPES_H

typedef  unsigned char   UChar;
typedef           char   Char;        /* platform-dependent signfulness */

typedef  unsigned short  UShort;
typedef    signed short  Short;

typedef  unsigned int    UInt;
typedef    signed int    Int;

typedef  unsigned long long int   ULong;
typedef    signed long long int   Long;

typedef  unsigned char  Bool;
#define  True   ((Bool)1)
#define  False  ((Bool)0)


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


#endif /* ndef __BASICTYPES_H */
