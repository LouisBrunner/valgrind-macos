
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_basictypes.h) is                      ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBVEX_BASICTYPES_H
#define __LIBVEX_BASICTYPES_H

typedef  unsigned char   UChar;
typedef           char   Char;        /* platform-dependent signfulness */

typedef  unsigned short  UShort;
typedef    signed short  Short;

typedef  unsigned int    UInt;
typedef    signed int    Int;

typedef  unsigned long long int   ULong;
typedef    signed long long int   Long;

typedef  float   Float;    /* IEEE754 single-precision (32-bit) value */
typedef  double  Double;   /* IEEE754 double-precision (64-bit) value */

typedef  unsigned char  Bool;
#define  True   ((Bool)1)
#define  False  ((Bool)0)


/* 32/64 bit addresses. */
typedef  UInt      Addr32;
typedef  ULong     Addr64;


#endif /* ndef __LIBVEX_BASICTYPES_H */


/*---------------------------------------------------------------*/
/*---                                     libvex_basictypes.h ---*/
/*---------------------------------------------------------------*/
