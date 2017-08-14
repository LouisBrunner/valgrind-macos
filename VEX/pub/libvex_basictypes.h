
/*---------------------------------------------------------------*/
/*--- begin                               libvex_basictypes.h ---*/
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

#ifndef __LIBVEX_BASICTYPES_H
#define __LIBVEX_BASICTYPES_H

/* It is important that the sizes of the following data types (on the
   host) are as stated.  LibVEX_Init therefore checks these at
   startup. */

/* Always 8 bits. */
typedef  unsigned char   UChar;
typedef    signed char   Char;
typedef           char   HChar; /* signfulness depends on host */
                                /* Only to be used for printf etc 
                                   format strings */

/* Always 16 bits. */
typedef  unsigned short  UShort;
typedef    signed short  Short;

/* Always 32 bits. */
typedef  unsigned int    UInt;
typedef    signed int    Int;

/* Always 64 bits. */
typedef  unsigned long long int   ULong;
typedef    signed long long int   Long;

/* Always 128 bits. */
typedef  UInt  U128[4];

/* Always 256 bits. */
typedef  UInt  U256[8];

/* A union for doing 128-bit vector primitives conveniently. */
typedef
   union {
      UChar  w8[16];
      UShort w16[8];
      UInt   w32[4];
      ULong  w64[2];
   }
   V128;

/* Floating point. */
typedef  float   Float;    /* IEEE754 single-precision (32-bit) value */
typedef  double  Double;   /* IEEE754 double-precision (64-bit) value */

/* Bool is always 8 bits. */
typedef  unsigned char  Bool;
#define  True   ((Bool)1)
#define  False  ((Bool)0)

/* Use this to coerce the result of a C comparison to a Bool.  This is
   useful when compiling with Intel icc with ultra-paranoid
   compilation flags (-Wall). */
static inline Bool toBool ( Int x ) {
   Int r = (x == 0) ? False : True;
   return (Bool)r;
}
static inline UChar toUChar ( Int x ) {
   x &= 0xFF;
   return (UChar)x;
}
static inline HChar toHChar ( Int x ) {
   x &= 0xFF;
   return (HChar)x;
}
static inline UShort toUShort ( Int x ) {
   x &= 0xFFFF;
   return (UShort)x;
}
static inline Short toShort ( Int x ) {
   x &= 0xFFFF;
   return (Short)x;
}
static inline UInt toUInt ( Long x ) {
   x &= 0xFFFFFFFFLL;
   return (UInt)x;
}

/* 32/64 bit addresses. */
typedef  UInt      Addr32;
typedef  ULong     Addr64;

/* Something which has the same size as void* on the host.  That is,
   it is 32 bits on a 32-bit host and 64 bits on a 64-bit host, and so
   it can safely be coerced to and from a pointer type on the host
   machine. */
typedef  unsigned long HWord;


/* This is so useful it should be visible absolutely everywhere. */
#if !defined(offsetof)
#   define offsetof(type,memb) ((Int)(HWord)&((type*)0)->memb)
#endif


/* We need to know the host word size in order to write Ptr_to_ULong
   and ULong_to_Ptr in a way that doesn't cause compilers to complain.
   These functions allow us to cast pointers to and from 64-bit
   integers without complaints from compilers, regardless of the host
   word size.

   Also set up VEX_REGPARM.
*/

#undef VEX_HOST_WORDSIZE
#undef VEX_REGPARM

/* The following 4 work OK for Linux. */
#if defined(__x86_64__)
#   define VEX_HOST_WORDSIZE 8
#   define VEX_REGPARM(_n) /* */

#elif defined(__i386__)
#   define VEX_HOST_WORDSIZE 4
#   define VEX_REGPARM(_n) __attribute__((regparm(_n)))

#elif defined(__powerpc__) && defined(__powerpc64__)
#   define VEX_HOST_WORDSIZE 8
#   define VEX_REGPARM(_n) /* */

#elif defined(__powerpc__) && !defined(__powerpc64__)
#   define VEX_HOST_WORDSIZE 4
#   define VEX_REGPARM(_n) /* */

#elif defined(__arm__)
#   define VEX_HOST_WORDSIZE 4
#   define VEX_REGPARM(_n) /* */

#elif defined(_AIX) && !defined(__64BIT__)
#   define VEX_HOST_WORDSIZE 4
#   define VEX_REGPARM(_n) /* */

#elif defined(_AIX) && defined(__64BIT__)
#   define VEX_HOST_WORDSIZE 8
#   define VEX_REGPARM(_n) /* */

#elif defined(__s390x__)
#   define VEX_HOST_WORDSIZE 8
#   define VEX_REGPARM(_n) /* */

#elif defined(__mips__)
#   define VEX_HOST_WORDSIZE 4
#   define VEX_REGPARM(_n) /* */

#else
#   error "Vex: Fatal: Can't establish the host architecture"
#endif


#if VEX_HOST_WORDSIZE == 8
   static inline ULong Ptr_to_ULong ( void* p ) {
      return (ULong)p;
   }
   static inline void* ULong_to_Ptr ( ULong n ) {
      return (void*)n;
   }
#elif VEX_HOST_WORDSIZE == 4
   static inline ULong Ptr_to_ULong ( void* p ) {
      UInt w = (UInt)p;
      return (ULong)w;
   }
   static inline void* ULong_to_Ptr ( ULong n ) {
      UInt w = (UInt)n;
      return (void*)w;
   }
#else
#   error "Vex: Fatal: Can't define  Ptr_to_ULong / ULong_to_Ptr"
#endif


#endif /* ndef __LIBVEX_BASICTYPES_H */

/*---------------------------------------------------------------*/
/*---                                     libvex_basictypes.h ---*/
/*---------------------------------------------------------------*/
