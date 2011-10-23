
/*--------------------------------------------------------------------*/
/*--- The translation table and cache.                             ---*/
/*---                                          pub_core_transtab.h ---*/
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

#ifndef __PUB_CORE_TRANSTAB_H
#define __PUB_CORE_TRANSTAB_H

//--------------------------------------------------------------------
// PURPOSE: This module is responsible for caching translations, and
// enabling fast look-ups of them.
//--------------------------------------------------------------------

#include "pub_core_transtab_asm.h"

/* The fast-cache for tt-lookup, and for finding counters.  Unused
   entries are denoted by .guest == 1, which is assumed to be a bogus
   address for all guest code. */
typedef
   struct { 
      Addr guest;
      Addr host;
   }
   FastCacheEntry;

extern __attribute__((aligned(16)))
       FastCacheEntry VG_(tt_fast) [VG_TT_FAST_SIZE];

#define TRANSTAB_BOGUS_GUEST_ADDR ((Addr)1)

extern UInt*          VG_(tt_fastN)[VG_TT_FAST_SIZE];

extern void VG_(init_tt_tc)       ( void );

extern
void VG_(add_to_transtab)( VexGuestExtents* vge,
                           Addr64           entry,
                           AddrH            code,
                           UInt             code_len,
                           Bool             is_self_checking );

extern Bool VG_(search_transtab) ( /*OUT*/AddrH* result,
                                   Addr64        guest_addr, 
                                   Bool          upd_cache );

extern void VG_(discard_translations) ( Addr64 start, ULong range,
                                        HChar* who );

extern void VG_(print_tt_tc_stats) ( void );

extern UInt VG_(get_bbs_translated) ( void );

/* Add to / search the auxiliary, small, unredirected translation
   table. */

extern
void VG_(add_to_unredir_transtab)( VexGuestExtents* vge,
                                   Addr64           entry,
                                   AddrH            code,
                                   UInt             code_len );
extern 
Bool VG_(search_unredir_transtab) ( /*OUT*/AddrH* result,
                                    Addr64        guest_addr );

// BB profiling stuff

typedef struct _BBProfEntry {
   Addr64 addr;
   ULong  score;
} BBProfEntry;

extern ULong VG_(get_BB_profile) ( BBProfEntry tops[], UInt n_tops );

#endif   // __PUB_CORE_TRANSTAB_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
