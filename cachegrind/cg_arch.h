
/*--------------------------------------------------------------------*/
/*--- Arch-specific declarations, cache configuration.   cg_arch.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Cachegrind, a Valgrind tool for cache
   profiling programs.

   Copyright (C) 2002-2012 Nicholas Nethercote
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

#ifndef __CG_ARCH_H
#define __CG_ARCH_H

// For cache simulation
typedef struct {
   Int size;       // bytes
   Int assoc;
   Int line_size;  // bytes
} cache_t;

#define MIN_LINE_SIZE         16

// clo_*c used in the call to VG_(str_clo_cache_opt) should be statically
// initialized to UNDEFINED_CACHE.
#define UNDEFINED_CACHE     { -1, -1, -1 }

// Gives the auto-detected configuration of I1, D1 and LL caches.  They get
// overridden by any cache configurations specified on the command line.
void VG_(configure_caches)(cache_t* I1c, cache_t* D1c, cache_t* LLc,
                           Bool all_caches_clo_defined);

// If arg is a command line option configuring I1 or D1 or LL cache,
// then parses arg to set the relevant cache_t elements.
// Returns True if arg is a cache command line option, False otherwise.
Bool VG_(str_clo_cache_opt)(Char *arg,
                            cache_t* clo_I1c,
                            cache_t* clo_D1c,
                            cache_t* clo_LLc);

// Checks the correctness of the auto-detected caches.
// If a cache has been configured by command line options, it
// replaces the equivalent auto-detected cache.
// Note that an invalid auto-detected cache will make Valgrind exit
// with an fatal error, except if the invalid auto-detected cache
// will be replaced by a command line defined cache.
void VG_(post_clo_init_configure_caches)(cache_t* I1c,
                                         cache_t* D1c,
                                         cache_t* LLc,
                                         cache_t* clo_I1c,
                                         cache_t* clo_D1c,
                                         cache_t* clo_LLc);

void VG_(print_cache_clo_opts)(void);

#endif   // __CG_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
