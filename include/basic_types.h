
/*--------------------------------------------------------------------*/
/*--- Basic types                                    basic_types.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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

#ifndef __BASIC_TYPES_H
#define __BASIC_TYPES_H

/* ---------------------------------------------------------------------
   Basic types
   ------------------------------------------------------------------ */

#include "libvex_basictypes.h"

/* VEX defines Char, UChar, Short, UShort, Int, UInt, Long, ULong,
   Addr32, Addr64, HWord, HChar, Bool, False and True. */

// By choosing the right types, we can get these right for 32-bit and 64-bit
// platforms without having to do any conditional compilation or anything.
// 
// Size in bits on:                          32-bit archs   64-bit archs
//                                           ------------   ------------
typedef unsigned long          UWord;     // 32             64

typedef signed long            Word;      // 32             64

typedef UWord                  Addr;      // 32             64

typedef UWord                  SizeT;     // 32             64
typedef  Word                 SSizeT;     // 32             64

typedef  Word                   OffT;     // 32             64


/* ---------------------------------------------------------------------
   Where to send bug reports to.
   ------------------------------------------------------------------ */

#define VG_BUGS_TO "valgrind.kde.org"


#endif /* __BASIC_TYPES_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
