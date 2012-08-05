
/*--------------------------------------------------------------------*/
/*--- PPC32-specific definitions.                       cg-ppc32.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2005-2012 Nicholas Nethercote
      njn@valgrind.org

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

#if defined(VGA_ppc32)

#include "pub_tool_basics.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"

#include "cg_arch.h"

void VG_(configure_caches)(cache_t* I1c, cache_t* D1c, cache_t* LLc,
                           Bool all_caches_clo_defined)
{
   // Set caches to default.
   *I1c = (cache_t) {  65536, 2, 64 };
   *D1c = (cache_t) {  65536, 2, 64 };
   *LLc = (cache_t) { 262144, 8, 64 };

   // Warn if config not completely specified from cmd line.  Note that
   // this message is slightly different from the one we give on x86/AMD64
   // when auto-detection fails;  this lets us filter out this one (which is
   // not important) in the regression test suite without filtering the
   // x86/AMD64 one (which we want to see if it ever occurs in the
   // regression test suite).
   //
   // If you change this message, please update
   // cachegrind/tests/filter_stderr!
   //
   if (!all_caches_clo_defined) {
      VG_(dmsg)("Warning: Cannot auto-detect cache config on PPC32, using one "
                "or more defaults\n");
   }
}

#endif // defined(VGA_ppc32)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
