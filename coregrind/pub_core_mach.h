
/*--------------------------------------------------------------------*/
/*--- Mach kernel interface module.                pub_core_mach.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2017 Apple Inc.
      Greg Parker  gparker@apple.com

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

#if defined(VGO_darwin)

#ifndef __PUB_CORE_MACH_H
#define __PUB_CORE_MACH_H

//--------------------------------------------------------------------
// PURPOSE: This module contains the Mach kernel interface, 
// for operating systems like Darwin / Mac OS X that use it.
//--------------------------------------------------------------------

// Call this early in Valgrind's main(). It depends on nothing.
extern void VG_(mach_init)(void);

#endif // __PUB_CORE_MACH_H

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
