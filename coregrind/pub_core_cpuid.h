
/*--------------------------------------------------------------------*/
/*--- Interface to CPUID.                         pub_core_cpuid.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_CPUID_H
#define __PUB_CORE_CPUID_H

#include "pub_core_basics.h"      // VG_ macro

//--------------------------------------------------------------------
// PURPOSE: This module provides Valgrind's interface to the x86/amd64
// CPUID instruction.
//--------------------------------------------------------------------

#if defined(VGA_x86) || defined(VGA_amd64)
extern Bool VG_(has_cpuid) ( void );

extern void VG_(cpuid) ( UInt eax, UInt ecx,
                         UInt* eax_ret, UInt* ebx_ret,
                         UInt* ecx_ret, UInt* edx_ret );
#endif

#endif   // __PUB_CORE_CPUID_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
