
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

#endif /* ndef __BASICTYPES_H */
