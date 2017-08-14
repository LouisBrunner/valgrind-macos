
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_basictypes.h) is                      ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

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
   word size. */

#undef VEX_HOST_WORDSIZE

/* The following 4 work OK for Linux. */
#if defined(__x86_64__)
#   define VEX_HOST_WORDSIZE 8
#elif defined(__i386__)
#   define VEX_HOST_WORDSIZE 4
#elif defined(__powerpc__) && defined(__powerpc64__)
#   define VEX_HOST_WORDSIZE 8
#elif defined(__powerpc__) && !defined(__powerpc64__)
#   define VEX_HOST_WORDSIZE 4

#elif defined(_AIX) && !defined(__64BIT__)
#   define VEX_HOST_WORDSIZE 4
#elif defined(_AIX) && defined(__64BIT__)
#   define VEX_HOST_WORDSIZE 8

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
