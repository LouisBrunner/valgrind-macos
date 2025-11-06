
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
   published by the Free Software Foundation; either version 3 of the
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

#define DARWIN_FAKE_MEMORY_PATH "/dev/macos/internals/"

//--------------------------------------------------------------------
// PURPOSE: This module contains the Mach kernel interface,
// for operating systems like Darwin / Mac OS X that use it.
//--------------------------------------------------------------------

// Call this early in Valgrind's main(). It depends on nothing.
extern void VG_(mach_init)(void);

// Record system memory after aspace has been init'd
extern void VG_(mach_record_system_memory)(void);

// MACH_DSC_END represent a guess to where the dyld shared cache ends
// there is no way to know for sure and even if we could,
// it would be a pain to carry that value around all of V
// so we just use a guess and hope for the best.
// Why is this value needed?
//  We only use it in a single context: when reading the dyld shared cache directly from memory
//  we use it as a bound when reading symbols with DebugInfo.
// How is this value calculated?
//  - amd64: end of user addressable space, which makes sense as the DSC is mapped at the end of the address space
//  - x86: end of user addressable space, note that there should not be any x86 valgrind with DSC support so this is just a placeholder
//  - arm64:
//    - the DSC is mapped at 0x180000000 + some ASLR slider
//    - the size depends on the macOS version but is usually less than 0xFFFFFFFF
//    - thus it should be around 0x280000000 but I have seen it reach above
//    - thus we use 0x300000000 which is also where we tell V that client memory starts
#if defined(VGP_amd64_darwin)
#define MACH_DSC_END 0x7ffffffff000
#elif defined(VGP_x86_darwin)
#define MACH_DSC_END 0xfffff000
#elif defined(VGP_arm64_darwin)
#define MACH_DSC_END 0x300000000
#else
#error "Unsupported platform"
#endif

#if DARWIN_VERS >= DARWIN_11_00
// Dyld shared cache (DSC) parsing, which is required as system libraries are not provided on disk
// starting with macOS 11.0 (Big Sur)
extern void VG_(dyld_cache_init)(const HChar*);
extern int VG_(dyld_cache_might_be_in)(const HChar*);
extern int VG_(dyld_cache_load_library)(const HChar*);
extern Addr VG_(dyld_cache_get_slide)(void);
#endif

#endif // __PUB_CORE_MACH_H

#endif // defined(VGO_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
