
/*--------------------------------------------------------------------*/
/*--- Tool-specific, asm-specific includes.             tool_asm.h ---*/
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

#ifndef __TOOL_ASM_H
#define __TOOL_ASM_H


/* All symbols externally visible from valgrind.so are prefixed
   as specified here.  The prefix can be changed, so as to avoid
   namespace conflict problems.
*/
#define VGAPPEND(str1,str2) str1##str2

/* These macros should add different prefixes so the same base
   name can safely be used across different macros. */
#define VG_(str)    VGAPPEND(vgPlain_,str)
#define VGP_(str)   VGAPPEND(vgProf_,str)
#define VGOFF_(str) VGAPPEND(vgOff_,str)
#define VGA_(str)   VGAPPEND(vgArch_,str)

/* Tool-specific ones.  Note that final name still starts with "vg". */
#define TL_(str)    VGAPPEND(vgTool_,str)

/* This is specifically for stringifying VG_(x) function names.  We
   need to do two macroexpansions to get the VG_ macro expanded before
   stringifying */
#define _STR(x)	#x
#define STR(x)	_STR(x)

#endif /* ndef __TOOL_ASM_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
