
/*--------------------------------------------------------------------*/
/*--- ARM-specific definitions.                           cg-arm.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2002-2005 Nicholas Nethercote
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

#include "cg_arch.h"

void VGA_(configure_caches)(cache_t* I1c, cache_t* D1c, cache_t* L2c,
                            Bool all_caches_clo_defined)
{
   // XXX: I1 and D1 are vaguely plausible, although they could really be
   // anything.  However, most (all?) ARMs don't have an L2 cache.  But
   // Cachegrind assumes the presence of an L2 cache... so we just copy the
   // x86 defaults.  Urk.
   *I1c = (cache_t) {   4096, 2, 32 };
   *D1c = (cache_t) {   4096, 2, 32 };
   *L2c = (cache_t) { 262144, 8, 64 };
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

