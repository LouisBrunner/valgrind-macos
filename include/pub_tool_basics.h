
/*--------------------------------------------------------------------*/
/*--- Header included by every tool C file.      pub_tool_basics.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2011 Julian Seward 
      jseward@acm.org

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_TOOL_BASICS_H
#define __PUB_TOOL_BASICS_H

//--------------------------------------------------------------------
// PURPOSE: This header should be imported by every single C file in
// tools.  It contains the basic types and other things needed everywhere.
// There is no corresponding C file because this isn't a module
// containing executable code, it's all just declarations.
//--------------------------------------------------------------------

/* ---------------------------------------------------------------------
   Other headers to include
   ------------------------------------------------------------------ */

// VEX defines Char, UChar, Short, UShort, Int, UInt, Long, ULong,
// Addr32, Addr64, HWord, HChar, Bool, False and True.
#include "libvex_basictypes.h"

// For varargs types
#include <stdarg.h>


/* ---------------------------------------------------------------------
   symbol prefixing
   ------------------------------------------------------------------ */
 
// All symbols externally visible from Valgrind are prefixed
// as specified here to avoid namespace conflict problems.
//
// VG_ is for symbols exported from modules.  ML_ (module-local) is
// for symbols which are not intended to be visible outside modules,
// but which cannot be declared as C 'static's since they need to be
// visible across C files within a given module.  It is a mistake for
// a ML_ name to appear in a pub_core_*.h or pub_tool_*.h file.
// Likewise it is a mistake for a VG_ name to appear in a priv_*.h
// file. 

#define VGAPPEND(str1,str2) str1##str2

#define VG_(str)    VGAPPEND(vgPlain_,          str)
#define ML_(str)    VGAPPEND(vgModuleLocal_,    str)


/* ---------------------------------------------------------------------
   builtin types
   ------------------------------------------------------------------ */

// By choosing the right types, we can get these right for 32-bit and 64-bit
// platforms without having to do any conditional compilation or anything.
// POSIX references:
// - http://www.opengroup.org/onlinepubs/009695399/basedefs/sys/types.h.html
// - http://www.opengroup.org/onlinepubs/009695399/basedefs/stddef.h.html
// 
// Size in bits on:                          32-bit archs   64-bit archs
//                                           ------------   ------------
typedef unsigned long          UWord;     // 32             64
typedef   signed long           Word;     // 32             64

// Addr is for holding an address.  AddrH was intended to be "Addr on the
// host", for the notional case where host word size != guest word size.
// But since the assumption that host arch == guest arch has become so
// deeply wired in, it's a pretty pointless distinction now.
typedef UWord                  Addr;      // 32             64
typedef UWord                  AddrH;     // 32             64

// Our equivalents of POSIX 'size_t' and 'ssize_t':
// - size_t is an "unsigned integer type of the result of the sizeof operator".
// - ssize_t is "used for a count of bytes or an error indication".
typedef UWord                  SizeT;     // 32             64
typedef  Word                 SSizeT;     // 32             64

// Our equivalent of POSIX 'ptrdiff_t':
// - ptrdiff_t is a "signed integer type of the result of subtracting two
//   pointers".
// We use it for memory offsets, eg. the offset into a memory block.
typedef  Word                 PtrdiffT;   // 32             64

// Our equivalent of POSIX 'off_t':
// - off_t is "used for file sizes".
// At one point we were using it for memory offsets, but PtrdiffT should be
// used in those cases.
// Nb: on Linux, off_t is a signed word-sized int.  On Darwin it's
// always a signed 64-bit int.  So we defined our own Off64T as well.
#if defined(VGO_linux)
typedef Word                   OffT;      // 32             64
#elif defined(VGO_darwin)
typedef Long                   OffT;      // 64             64
#else
#  error Unknown OS
#endif
typedef Long                 Off64T;      // 64             64

#if !defined(NULL)
#  define NULL ((void*)0)
#endif

/* This is just too useful to not have around the place somewhere. */
typedef  struct { UWord uw1; UWord uw2; }  UWordPair;


/* ---------------------------------------------------------------------
   non-builtin types
   ------------------------------------------------------------------ */

// These probably shouldn't be here, but moving them to their logical
// modules results in a lot more #includes...

/* ThreadIds are simply indices into the VG_(threads)[] array. */
typedef UInt ThreadId;

/* An abstraction of syscall return values.
   Linux:
      When _isError == False, 
         _val holds the return value.
      When _isError == True,  
         _err holds the error code.

   Darwin:
      Interpretation depends on _mode:
      MACH, MDEP:
         these can never 'fail' (apparently).  The result of the
         syscall is a single host word, _wLO.
      UNIX:
         Can record a double-word error or a double-word result:
         When _mode is SysRes_UNIX_OK,  _wHI:_wLO holds the result.
         When _mode is SysRes_UNIX_ERR, _wHI:_wLO holds the error code.
         Probably the high word of an error is always ignored by
         userspace, but we have to record it, so that we can correctly
         update both {R,E}DX and {R,E}AX (in guest state) given a SysRes,
         if we're required to.
*/
#if defined(VGO_linux)
typedef
   struct {
      UWord _val;
      Bool  _isError;
   }
   SysRes;
#elif defined(VGO_darwin)
typedef
   enum { 
      SysRes_MACH=40,  // MACH, result is _wLO
      SysRes_MDEP,     // MDEP, result is _wLO
      SysRes_UNIX_OK,  // UNIX, success, result is _wHI:_wLO
      SysRes_UNIX_ERR  // UNIX, error,   error  is _wHI:_wLO
   }
   SysResMode;
typedef
   struct {
      UWord _wLO;
      UWord _wHI;
      SysResMode _mode;
   }
   SysRes;
#else
#  error "Unknown OS"
#endif


/* ---- And now some basic accessor functions for it. ---- */

#if defined(VGO_linux)

static inline Bool sr_isError ( SysRes sr ) {
   return sr._isError;
}
static inline UWord sr_Res ( SysRes sr ) {
   return sr._isError ? 0 : sr._val;
}
static inline UWord sr_ResHI ( SysRes sr ) {
   return 0;
}
static inline UWord sr_Err ( SysRes sr ) {
   return sr._isError ? sr._val : 0;
}
static inline Bool sr_EQ ( SysRes sr1, SysRes sr2 ) {
   return sr1._val == sr2._val 
          && ((sr1._isError && sr2._isError) 
              || (!sr1._isError && !sr2._isError));
}

#elif defined(VGO_darwin)

static inline Bool sr_isError ( SysRes sr ) {
   switch (sr._mode) {
      case SysRes_UNIX_ERR: return True;
      default:              return False;
      /* should check tags properly and assert here, but we can't here */
   }
}

static inline UWord sr_Res ( SysRes sr ) {
   switch (sr._mode) {
      case SysRes_MACH:
      case SysRes_MDEP:
      case SysRes_UNIX_OK: return sr._wLO;
      default: return 0; /* should assert, but we can't here */
   }
}

static inline UWord sr_ResHI ( SysRes sr ) {
   switch (sr._mode) {
      case SysRes_UNIX_OK: return sr._wHI;
      default: return 0; /* should assert, but we can't here */
   }
}

static inline UWord sr_Err ( SysRes sr ) {
   switch (sr._mode) {
      case SysRes_UNIX_ERR: return sr._wLO;
      default: return 0; /* should assert, but we can't here */
   }
}

static inline Bool sr_EQ ( SysRes sr1, SysRes sr2 ) {
   return sr1._mode == sr2._mode
          && sr1._wLO == sr2._wLO && sr1._wHI == sr2._wHI;
}

#else
#  error "Unknown OS"
#endif


/* ---------------------------------------------------------------------
   Miscellaneous (word size, endianness, regparmness, stringification)
   ------------------------------------------------------------------ */

/* Word size: this is going to be either 4 or 8. */
// It should probably be in m_machine.
#define VG_WORDSIZE VEX_HOST_WORDSIZE

/* Endianness */
#undef VG_BIGENDIAN
#undef VG_LITTLEENDIAN

#if defined(VGA_x86) || defined(VGA_amd64) || defined (VGA_arm)
#  define VG_LITTLEENDIAN 1
#elif defined(VGA_ppc32) || defined(VGA_ppc64) || defined(VGA_s390x)
#  define VG_BIGENDIAN 1
#else
#  error Unknown arch
#endif

/* Regparmness */
#if defined(VGA_x86)
#  define VG_REGPARM(n)            __attribute__((regparm(n)))
#elif defined(VGA_amd64) || defined(VGA_ppc32) \
      || defined(VGA_ppc64) || defined(VGA_arm) || defined(VGA_s390x)
#  define VG_REGPARM(n)            /* */
#else
#  error Unknown arch
#endif

/* Macro games */
#define VG_STRINGIFZ(__str)  #__str
#define VG_STRINGIFY(__str)  VG_STRINGIFZ(__str)

// Where to send bug reports to.
#define VG_BUGS_TO "www.valgrind.org"

/* Branch prediction hints. */
#if defined(__GNUC__)
#  define LIKELY(x)   __builtin_expect(!!(x), 1)
#  define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#  define LIKELY(x)   (x)
#  define UNLIKELY(x) (x)
#endif

// printf format string checking for gcc.
// This feature has been supported since at least gcc version 2.95.
// For more information about the format attribute, see
// http://gcc.gnu.org/onlinedocs/gcc-4.3.0/gcc/Function-Attributes.html.
#if defined(__GNUC__)
#define PRINTF_CHECK(x, y) __attribute__((format(__printf__, x, y)))
#else
#define PRINTF_CHECK(x, y)
#endif


#endif /* __PUB_TOOL_BASICS_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
