
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libjit_basictypes.h) is                      ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBJIT_BASICTYPES_H
#define __LIBJIT_BASICTYPES_H

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


/* 32/64 bit addresses. */
typedef  UInt      Addr32;
typedef  ULong     Addr64;


#endif /* ndef __LIBJIT_BASICTYPES_H */


/*---------------------------------------------------------------*/
/*---                                     libjit_basictypes.h ---*/
/*---------------------------------------------------------------*/
