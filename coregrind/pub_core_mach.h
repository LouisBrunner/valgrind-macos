
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

#if defined(VGP_amd64_darwin)
#define MACH_DSC_END 0x7ffffffff000
#elif defined(VGP_x86_darwin)
#define MACH_DSC_END 0xfffff000
#elif defined(VGP_arm64_darwin)
#define MACH_DSC_END 0x280000000
#else
#error "Unsupported platform"
#endif

#if DARWIN_VERS >= DARWIN_11_00
// Dyld shared cache (DSC) parsing, which is required as system libraries are not provided on disk
// starting with macOS 11.0 (Big Sur)
extern void VG_(dyld_cache_init)(void);
extern int VG_(dyld_cache_might_be_in)(const HChar*);
extern int VG_(dyld_cache_load_library)(const HChar*);
extern Addr VG_(dyld_cache_get_slide)(void);
#endif

#if defined(VGA_arm64)
extern void VG_(mach_invalidate_icache)( void *start, SizeT len);
extern void* VG_(dyld_dlsym)( const HChar * library, const HChar * symbol );
#endif

#endif // __PUB_CORE_MACH_H

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
