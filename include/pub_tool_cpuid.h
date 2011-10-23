
/*--------------------------------------------------------------------*/
/*--- Interface to CPUID.                         pub_tool_cpuid.h ---*/
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

#ifndef __PUB_TOOL_CPUID_H
#define __PUB_TOOL_CPUID_H

#if defined(VGA_x86) || defined(VGA_amd64)
extern Bool VG_(has_cpuid) ( void );

extern void VG_(cpuid) ( UInt eax, UInt ecx,
                         UInt* eax_ret, UInt* ebx_ret,
                         UInt* ecx_ret, UInt* edx_ret );
#endif

#endif   // __PUB_TOOL_CPUID_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
