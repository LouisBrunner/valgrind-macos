
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_basictypes.h) is                      ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBVEX_BASICTYPES_H
#define __LIBVEX_BASICTYPES_H

/* It is important that the sizes of the following data types (on the
   host) are as stated.  LibVEX_Init therefore checks these at
   startup. */

/* Always 8 bits. */
typedef  unsigned char   UChar;
typedef           char   Char;        /* platform-dependent signfulness */

/* Always 16 bits. */
typedef  unsigned short  UShort;
typedef    signed short  Short;

/* Always 32 bits. */
typedef  unsigned int    UInt;
typedef    signed int    Int;

/* Always 64 bits. */
typedef  unsigned long long int   ULong;
typedef    signed long long int   Long;


typedef  float   Float;    /* IEEE754 single-precision (32-bit) value */
typedef  double  Double;   /* IEEE754 double-precision (64-bit) value */

/* Bool is always 8 bits. */
typedef  unsigned char  Bool;
#define  True   ((Bool)1)
#define  False  ((Bool)0)

/* 32/64 bit addresses. */
typedef  UInt      Addr32;
typedef  ULong     Addr64;

/* Something which has the same size as void* on the host.  That is,
   it is 32 bits on a 32-bit host and 64 bits on a 64-bit host, and so
   it can safely be coerced to and from a pointer type on the host
   machine. */
typedef  unsigned long HWord;


/* This is so useful it should be visible absolutely everywhere. */
#define offsetof(type,memb) ((Int)&((type*)0)->memb)


#endif /* ndef __LIBVEX_BASICTYPES_H */


/*---------------------------------------------------------------*/
/*---                                     libvex_basictypes.h ---*/
/*---------------------------------------------------------------*/
