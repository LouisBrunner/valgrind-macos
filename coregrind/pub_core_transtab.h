
/*--------------------------------------------------------------------*/
/*--- The translation table and cache.                             ---*/
/*---                                          pub_core_transtab.h ---*/
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

#ifndef __PUB_CORE_TRANSTAB_H
#define __PUB_CORE_TRANSTAB_H

//--------------------------------------------------------------------
// PURPOSE: This module is responsible for caching translations, and
// enabling fast look-ups of them.
//--------------------------------------------------------------------

#include "pub_core_transtab_asm.h"
#include "pub_tool_transtab.h"
#include "libvex.h"                   // VexGuestExtents

/* The fast-cache for tt-lookup.  Unused entries are denoted by
   .guest == TRANSTAB_BOGUS_GUEST_ADDR (viz, 1), which is assumed
   to be a bogus address for all guest code.  See pub_core_transtab_asm.h
   for further description. */
typedef
   struct { 
      Addr guest0;
      Addr host0;
      Addr guest1;
      Addr host1;
      Addr guest2;
      Addr host2;
      Addr guest3;
      Addr host3;
   }
   FastCacheSet;

STATIC_ASSERT(sizeof(Addr) == sizeof(UWord));
STATIC_ASSERT(sizeof(FastCacheSet) == sizeof(Addr) * 8);

extern __attribute__((aligned(64)))
       FastCacheSet VG_(tt_fast) [VG_TT_FAST_SETS];

#define TRANSTAB_BOGUS_GUEST_ADDR ((Addr)1)

#if defined(VGA_x86) || defined(VGA_amd64)
static inline UWord VG_TT_FAST_HASH ( Addr guest ) {
   // There's no minimum insn alignment on these targets.
   UWord merged = ((UWord)guest) >> 0;
   merged = (merged >> VG_TT_FAST_BITS) ^ merged;
   return merged & VG_TT_FAST_MASK;
}

#elif defined(VGA_s390x) || defined(VGA_arm) || defined(VGA_nanomips)
static inline UWord VG_TT_FAST_HASH ( Addr guest ) {
   // Instructions are 2-byte aligned.
   UWord merged = ((UWord)guest) >> 1;
   merged = (merged >> VG_TT_FAST_BITS) ^ merged;
   return merged & VG_TT_FAST_MASK;
}

#elif defined(VGA_ppc32) || defined(VGA_ppc64be) || defined(VGA_ppc64le) \
      || defined(VGA_mips32) || defined(VGA_mips64) || defined(VGA_arm64)
static inline UWord VG_TT_FAST_HASH ( Addr guest ) {
   // Instructions are 4-byte aligned.
   UWord merged = ((UWord)guest) >> 2;
   merged = (merged >> VG_TT_FAST_BITS) ^ merged;
   return merged & VG_TT_FAST_MASK;
}

#else
#  error "VG_TT_FAST_HASH: unknown platform"
#endif

static inline Bool VG_(lookupInFastCache)( /*MB_OUT*/Addr* host, Addr guest )
{
   UWord setNo = (UInt)VG_TT_FAST_HASH(guest);
   FastCacheSet* set = &VG_(tt_fast)[setNo];
   if (LIKELY(set->guest0 == guest)) {
      // hit at way 0
      *host = set->host0;
      return True;
   }
   if (LIKELY(set->guest1 == guest)) {
      // hit at way 1; swap upwards
      Addr tG = guest;
      Addr tH = set->host1;
      set->guest1 = set->guest0;
      set->host1  = set->host0;
      set->guest0 = tG;
      set->host0  = tH;
      *host = tH;
      return True;
   }
   if (LIKELY(set->guest2 == guest)) {
      // hit at way 2; swap upwards
      Addr tG = guest;
      Addr tH = set->host2;
      set->guest2 = set->guest1;
      set->host2  = set->host1;
      set->guest1 = tG;
      set->host1  = tH;
      *host = tH;
      return True;
   }
   if (LIKELY(set->guest3 == guest)) {
      // hit at way 3; swap upwards
      Addr tG = guest;
      Addr tH = set->host3;
      set->guest3 = set->guest2;
      set->host3  = set->host2;
      set->guest2 = tG;
      set->host2  = tH;
      *host = tH;
      return True;
   }
   // Not found
   *host = 0;
   return False;
}


/* Initialises the TC, using VG_(clo_num_transtab_sectors)
   and VG_(clo_avg_transtab_entry_size).
   VG_(clo_num_transtab_sectors) must be >= MIN_N_SECTORS
   and <= MAX_N_SECTORS. */
extern void VG_(init_tt_tc)       ( void );


/* Limits for number of sectors the TC is divided into.  If you need a larger
   overall translation cache, increase MAX_N_SECTORS. */ 
#define MIN_N_SECTORS 2
#define MAX_N_SECTORS 48

/* Default for the nr of sectors, if not overridden by command line.
   On Android, space is limited, so try to get by with fewer sectors.
   On other platforms we can go to town.  32 sectors gives theoretical
   capacity of about 880MB of JITted code in 2.1 million translations
   (realistically, about 2/3 of that) for Memcheck. */
#if defined(VGPV_arm_linux_android) \
    || defined(VGPV_x86_linux_android) \
    || defined(VGPV_mips32_linux_android) \
    || defined(VGPV_arm64_linux_android)
# define N_SECTORS_DEFAULT 12
#else
# define N_SECTORS_DEFAULT 32
#endif

extern
void VG_(add_to_transtab)( const VexGuestExtents* vge,
                           Addr             entry,
                           Addr             code,
                           UInt             code_len,
                           Bool             is_self_checking,
                           Int              offs_profInc,
                           UInt             n_guest_instrs );

typedef UShort SECno; // SECno type identifies a sector
typedef UShort TTEno; // TTEno type identifies a TT entry in a sector.

// 2 constants that indicates Invalid entries.
#define INV_SNO ((SECno)0xFFFF)
#define INV_TTE ((TTEno)0xFFFF)

extern
void VG_(tt_tc_do_chaining) ( void* from__patch_addr,
                              SECno to_sNo,
                              TTEno to_tteNo,
                              Bool  to_fastEP );

extern Bool VG_(search_transtab) ( /*OUT*/Addr*  res_hcode,
                                   /*OUT*/SECno* res_sNo,
                                   /*OUT*/TTEno* res_tteNo,
                                   Addr          guest_addr, 
                                   Bool          upd_cache );

extern void VG_(discard_translations) ( Addr  start, ULong range,
                                        const HChar* who );

extern void VG_(print_tt_tc_stats) ( void );

extern ULong VG_(get_bbs_translated) ( void );
extern ULong VG_(get_bbs_discarded_or_dumped) ( void );

/* Add to / search the auxiliary, small, unredirected translation
   table. */

extern
void VG_(add_to_unredir_transtab)( const VexGuestExtents* vge,
                                   Addr             entry,
                                   Addr             code,
                                   UInt             code_len );
extern 
Bool VG_(search_unredir_transtab) ( /*OUT*/Addr*  result,
                                    Addr          guest_addr );

// SB profiling stuff

typedef struct _SBProfEntry {
   Addr   addr;
   ULong  score;
} SBProfEntry;

extern ULong VG_(get_SB_profile) ( SBProfEntry tops[], UInt n_tops );

//  Exported variables
extern Bool  VG_(ok_to_discard_translations);

#endif   // __PUB_CORE_TRANSTAB_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
