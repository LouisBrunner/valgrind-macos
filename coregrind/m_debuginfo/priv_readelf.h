
/*--------------------------------------------------------------------*/
/*--- Reading of syms & debug info from ELF .so/executable files.  ---*/
/*---                                               priv_readelf.h ---*/
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

#ifndef __PRIV_READELF_H
#define __PRIV_READELF_H

#include "pub_core_basics.h"     // SizeT
#include "pub_core_debuginfo.h"  // DebugInfo

/*
   Stabs reader greatly improved by Nick Nethercote, Apr 02.
   This module was also extensively hacked on by Jeremy Fitzhardinge
   and Tom Hughes.
*/

/* Identify an ELF object file by peering at the first few bytes of
   it. */
extern Bool ML_(is_elf_object_file)( const void* image, SizeT n_image,
                                     Bool rel_ok );

/* Read the ELF binary specified by DI.  For the object/exe specified
   by the SegInfo, find ELF sections, then read the symbols, line number
   info, file name info, CFA (stack-unwind info) and anything else we
   want, into the tables within the supplied SegInfo.

   .debug_* sections as well as any separate debuginfo files are not
   loaded by this function but instead by ML_(read_elf_debug).  This
   separation facilitates lazy loading of debuginfo.  */
extern Bool ML_(read_elf_object) ( DebugInfo* di );

/* Read .debug_* sections from the ELF binary specified by DI.  Also
   attempt to load any separate debuginfo files associated with the
   object.

   ML_(read_elf_object) should be called on DI before calling this
   function.  */
extern Bool ML_(read_elf_debug) ( DebugInfo* di );

extern Bool ML_(check_elf_and_get_rw_loads) ( Int fd, const HChar* filename, Int * rw_load_count );


#endif /* ndef __PRIV_READELF_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
