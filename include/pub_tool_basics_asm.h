
/*--------------------------------------------------------------------*/
/*--- Header imported directly by every asm file, and indirectly   ---*/
/*--- (via pub_tool_basics.h) by every C file.                     ---*/
/*---                                        pub_tool_basics_asm.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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

#ifndef __PUB_TOOL_BASICS_ASM_H
#define __PUB_TOOL_BASICS_ASM_H

/* All symbols externally visible from Valgrind are prefixed
   as specified here to avoid namespace conflict problems.  */

#define VGAPPEND(str1,str2) str1##str2

/* VG_ is for symbols exported from modules.  ML_ (module-local) is
   for symbols which are not intended to be visible outside modules,
   but which cannot be declared as C 'static's since they need to be
   visible across C files within a given module.  It is a mistake for
   a ML_ name to appear in a pub_core_*.h or pub_tool_*.h file.
   Likewise it is a mistake for a VG_ name to appear in a priv_*.h
   file. 
*/
#define VG_(str)    VGAPPEND(vgPlain_,          str)
#define ML_(str)    VGAPPEND(vgModuleLocal_,    str)

#define VGA_(str)   VGAPPEND(vgArch_,     str)
#define VGO_(str)   VGAPPEND(vgOS_,       str)
#define VGP_(str)   VGAPPEND(vgPlatform_, str)

#endif /* __PUB_TOOL_BASICS_ASM_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
