
/*--------------------------------------------------------------------*/
/*--- Management of the translation table and cache.               ---*/
/*---                                                vg_transtab.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
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

#include "vg_include.h"
#include <stddef.h>

/* #define DEBUG_TRANSTAB */


/*-------------------------------------------------------------*/
/*--- Management of the FIFO-based translation table+cache. ---*/
/*-------------------------------------------------------------*/

/*------------------ CONSTANTS ------------------*/

/* Number of sectors the TC is divided into. */
#define VG_TC_N_SECTORS 8

/* Calculated once at startup and never changed. */
static /* const */ Int vg_tc_sector_szB = 0;

/* Number of entries in the translation table.  This must be a prime
   number in order to make the hashing work properly. */
#define VG_TT_SIZE /*5281*/ /*100129*/ /*200191*/ /*250829*/ 300007

/* Do an LRU pass when the translation table becomes this full. */
#define VG_TT_LIMIT_PERCENT /*67*/ 80

#define VG_TT_LIMIT ((VG_TT_SIZE * VG_TT_LIMIT_PERCENT) / 100)


/*------------------ TYPES ------------------*/

#define CODE_ALIGNMENT	4	/* alignment of TCEntries */
#define CODE_ALIGN(a)	(((a)+CODE_ALIGNMENT-1) & ~(CODE_ALIGNMENT-1))
#define IS_ALIGNED(a)	(((a) & (CODE_ALIGNMENT-1)) == 0)

/* An entry in TC.  Payload always is always padded out to a 4-aligned
   quantity so that these structs are always word-aligned. */
typedef
   struct { 
      /* +0 */ Addr   orig_addr;
      /* +4 */ UShort orig_size;
      /* +6 */ UShort trans_size;
      /* +8 */ UShort jump_sites[VG_MAX_JUMPS];
      /* +VG_CODE_OFFSET */ UChar  payload[0];
   }
   TCEntry;

/* An entry in TT. */
typedef
   struct {
      Addr     orig_addr;
      TCEntry* tcentry;
   }
   TTEntry;

/* Denotes an empty TT slot, when TTEntry.orig_addr holds this
   value. */
#define VG_TTE_EMPTY ((Addr)1)

/* Denotes an empty TT slot, when TTEntry.orig_addr holds this
   value. */
#define VG_TTE_DELETED ((Addr)3)

/* A bogus TCEntry which hopefully does not match code from any valid
   address.  This is what all VG_(tt_fast) entries are made to point
   at when we want to invalidate it. */
static const TCEntry vg_tc_bogus_TCEntry = { ((Addr)5), 0, 0 };


/*------------------ DECLS ------------------*/

/* The translation cache sectors.  These are NULL until allocated
   dynamically. */
static UChar* vg_tc[VG_TC_N_SECTORS];

/* Count of bytes used in each sector of the TC. */
static Int vg_tc_used[VG_TC_N_SECTORS];

/* The age of each sector, so we can find the oldest.  We just use the
   global count of translations made when the sector was brought into
   use.  Doesn't matter if this mechanism gets confused (wraps around
   4G) once in a while. */
static Int vg_tc_age[VG_TC_N_SECTORS];

/* The number of the sector currently being allocated in. */
static Int vg_tc_current;

/* Count of number of translations, orig and new bytes in each sector.
   For stats purposes only. */
static Int vg_tc_stats_count[VG_TC_N_SECTORS];
static Int vg_tc_stats_osize[VG_TC_N_SECTORS];
static Int vg_tc_stats_tsize[VG_TC_N_SECTORS];


/*------------------ TRANSLATION TABLE ------------------*/

/* The translation table.  An array of VG_TT_SIZE TTEntrys. */
static TTEntry* vg_tt = NULL;

/* Count of non-empty TT entries.  This includes deleted ones. */
static Int vg_tt_used = 0;

/* Fast helper for the TT.  A direct-mapped cache which holds a
   pointer to a TT entry which may or may not be the correct one, but
   which we hope usually is.  This array is referred to directly from
   vg_dispatch.S. */
Addr /* TCEntry*, really */ VG_(tt_fast)[VG_TT_FAST_SIZE];

static void for_each_tc(Int sector, void (*fn)(TCEntry *));


/*------------------ T-CHAINING HELPERS ------------------*/

static
void for_each_jumpsite(TCEntry *tce, void (*fn)(Addr))
{
   Int i;
   for(i = 0; i < VG_MAX_JUMPS; i++) {
      Addr a;
      UShort idx = tce->jump_sites[i];

      if (idx == (UShort)-1)
	 continue;
      
      a = (Addr)&tce->payload[idx];

      (*fn)(a);
   }
}

static inline
void unchain_tce(TCEntry *tce)
{
   for_each_jumpsite(tce, VG_(unchain_jumpsite));
}

/* Unchain any jumps pointing to a sector we're about to free */
static
void unchain_sector(Int s, Addr base, UInt len)
{
   void unchain_site(Addr a) {
      Addr jmp = VG_(get_jmp_dest)(a);
      if (jmp >= base && jmp < (base+len))
	 VG_(unchain_jumpsite)(a);
   }
   void _unchain_tce(TCEntry *tce) {
      for_each_jumpsite(tce, unchain_site);
   }

   for_each_tc(s, _unchain_tce);
}


/*------------------ TT HELPERS ------------------*/

static
void pp_tt_tc_status ( Char* submsg )
{
   Int tc_used, s;
   if (VG_(clo_verbosity) <= 2)
      return;
   tc_used = 0;
   for (s = 0; s < VG_TC_N_SECTORS; s++)
      tc_used += vg_tc_used[s];

   VG_(message)(Vg_DebugMsg, 
       "%lluk bbs: tt %d, tc %d, %s",
       VG_(bbs_done) / 1000,
       vg_tt_used, tc_used, submsg );
}

/* Invalidate the tt_fast cache, for whatever reason, by pointing all
   entries at vg_tc_bogus_TCEntry.  */
static
void vg_invalidate_tt_fast( void )
{
   Int j;
   for (j = 0; j < VG_TT_FAST_SIZE; j++)
      VG_(tt_fast)[j] = (Addr)&vg_tc_bogus_TCEntry;
}


static
void add_tt_entry ( TCEntry* tce )
{
   UInt i;
   /* VG_(printf)("add_TT_entry orig_addr %p\n", tce->orig_addr); */
   /* Hash to get initial probe point. */
   i = ((UInt)(tce->orig_addr)) % VG_TT_SIZE;
   while (True) {
      if (vg_tt[i].orig_addr == tce->orig_addr)
         VG_(core_panic)("add_TT_entry: duplicate");
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY)
         break;
      i++;
      if (i == VG_TT_SIZE) 
         i = 0;
   }

   vg_tt[i].orig_addr = tce->orig_addr;
   vg_tt[i].tcentry = tce;
   vg_tt_used++;
   /* sanity ... */
   vg_assert(vg_tt_used < VG_TT_SIZE-1000);
}


/* Search TT to find the translated address of the supplied original,
   or NULL if not found.  This routine is used when we miss in
   VG_(tt_fast). 
*/
static __inline__
TTEntry* search_tt ( Addr orig_addr )
{
   Int i;
   /* Hash to get initial probe point. */
   i = ((UInt)orig_addr) % VG_TT_SIZE;
   while (True) {
      if (vg_tt[i].orig_addr == orig_addr)
         return &vg_tt[i];
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY)
         return NULL;
      i++;
      if (i == VG_TT_SIZE) i = 0;
   }
}


static
void initialise_tt ( void )
{
   Int i;
   vg_tt_used = 0;
   for (i = 0; i < VG_TT_SIZE; i++) {
      vg_tt[i].orig_addr = VG_TTE_EMPTY;
   }
   vg_invalidate_tt_fast();
}


static 
void rebuild_TT ( void )
{
   Int      s;

   /* Throw away TT. */
   initialise_tt();
   
   /* Rebuild TT from the remaining quarters. */
   for (s = 0; s < VG_TC_N_SECTORS; s++) {
      for_each_tc(s, add_tt_entry);
   }
   pp_tt_tc_status ( "after  rebuild of TC" );
}


/*------------------ TC HELPERS ------------------*/

static
void for_each_tc(Int s, void (*fn)(TCEntry *))
{
   UChar *pc;
   UChar *pc_lim;
   TCEntry *tce;

   pc     = &(vg_tc[s][0]);
   pc_lim = &(vg_tc[s][vg_tc_used[s]]);
   while (True) {
      if (pc >= pc_lim) break;
      tce = (TCEntry*)pc;
      pc += sizeof(TCEntry) + tce->trans_size;
      if (tce->orig_addr != VG_TTE_DELETED)
	 (*fn)(tce);
   }
}

/* Find the oldest non-NULL, non-empty sector, or -1 if none such. */
static 
Int find_oldest_sector ( void ) 
{
   Int oldest_age, oldest, i;
   oldest_age = 1000 * 1000 * 1000;
   oldest = -1;
   for (i = 0; i < VG_TC_N_SECTORS; i++) {
      if (vg_tc[i] == NULL) 
         continue;
      if (vg_tc_used[i] == 0)
         continue;
      if (vg_tc_age[i] < oldest_age) {
         oldest = i;
         oldest_age = vg_tc_age[i];
      }
   }
   return oldest;
}


/* Discard the oldest sector, if any such exists. */
static
void discard_oldest_sector ( void )
{
   Char msg[100];
   Int s = find_oldest_sector();
   if (s != -1) {
      Int i;

      vg_assert(s >= 0 && s < VG_TC_N_SECTORS);
      VG_(sprintf)(msg, "before discard of sector %d (%d bytes)", 
                        s, vg_tc_used[s]);

      for(i = 0; i < VG_TC_N_SECTORS; i++) {
	 if (i != s && vg_tc[i] != NULL)
	    unchain_sector(i, (Addr)vg_tc[s], vg_tc_used[s]);
      }

      pp_tt_tc_status ( msg );
      VG_(overall_out_count) += vg_tc_stats_count[s];
      VG_(overall_out_osize) += vg_tc_stats_osize[s];
      VG_(overall_out_tsize) += vg_tc_stats_tsize[s]; 
      vg_tc_used[s] = 0;
      vg_tc_stats_count[s] = 0;
      vg_tc_stats_osize[s] = 0;
      vg_tc_stats_tsize[s] = 0;
      VG_(number_of_tc_discards) ++;
   }
}


/* Find an empty sector and bring it into use.  If there isn't one,
   try and allocate one.  If that fails, return -1. */
static
Int maybe_commission_sector ( void )
{
   Char msg[100];
   Int  s;
   for (s = 0; s < VG_TC_N_SECTORS; s++) {
      if (vg_tc[s] != NULL && vg_tc_used[s] == 0) {
         vg_tc_age[s] = VG_(overall_in_count);
         VG_(sprintf)(msg, "after  commission of sector %d "
                           "at time %d", 
                           s, vg_tc_age[s]);
         pp_tt_tc_status ( msg );
#        ifdef DEBUG_TRANSTAB
         VG_(sanity_check_tc_tt)();
#        endif
         return s;
      }
   }
   for (s = 0; s < VG_TC_N_SECTORS; s++) {
      if (vg_tc[s] == NULL) {
#if 1
         vg_tc[s] = VG_(get_memory_from_mmap) 
                       ( vg_tc_sector_szB, "trans-cache(sector)" );
#else
	 Char buf[20];
	 static Int count = 0;
	 Int fd;
	 
	 VG_(sprintf)(buf, ".transtab.%d", count++);

	 fd = VG_(open)(buf, VKI_O_RDWR|VKI_O_CREAT|VKI_O_TRUNC, 0700);
	 //VG_(unlink)(buf);
	 VG_(do_syscall)(__NR_ftruncate, fd, PGROUNDUP(vg_tc_sector_szB));
	 vg_tc[s] = VG_(mmap)(0, PGROUNDUP(vg_tc_sector_szB), VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC, VKI_MAP_SHARED, fd, 0);
	 VG_(close)(fd);
#endif
         vg_tc_used[s] = 0;
         VG_(sprintf)(msg, "after  allocation of sector %d "
                           "(size %d)", 
                           s, vg_tc_sector_szB );
         pp_tt_tc_status ( msg );
         return maybe_commission_sector();
      }
   }
   return -1;
}


static
UChar* allocate ( Int nBytes )
{
   Int i;

   vg_assert(IS_ALIGNED(nBytes));

   /* Ensure the TT is still OK. */
   while (vg_tt_used >= VG_TT_LIMIT) {
      (void)discard_oldest_sector();
      rebuild_TT();
      vg_assert(vg_tt_used < VG_TT_LIMIT);
   }

   /* Can we get it into the current sector? */
   if (vg_tc_current >= 0 
       && vg_tc_current < VG_TC_N_SECTORS
       && vg_tc[vg_tc_current] != NULL
       && vg_tc_used[vg_tc_current] + nBytes <= vg_tc_sector_szB) {
      /* Yes. */
      UChar* p = &(vg_tc[vg_tc_current][ vg_tc_used[vg_tc_current] ]);
      vg_tc_used[vg_tc_current] += nBytes;
      return p;
   }

   /* Perhaps we can bring a new sector into use, for the first
      time. */
   vg_tc_current = maybe_commission_sector();
   if (vg_tc_current >= 0 && vg_tc_current < VG_TC_N_SECTORS)
      return allocate(nBytes);

   /* That didn't work.  We'll have to dump the oldest.  We take the
      opportunity to dump the N oldest at once. */
   for (i = 0; i < 1; i++)
      (void)discard_oldest_sector();

   rebuild_TT();
   vg_tc_current = maybe_commission_sector();
   vg_assert(vg_tc_current >= 0 && vg_tc_current < VG_TC_N_SECTORS);
#  ifdef DEBUG_TRANSTAB
   VG_(sanity_check_tc_tt)();
#  endif

   return allocate(nBytes);
}


/* Just so these counts can be queried without making them globally
   visible. */
void VG_(get_tt_tc_used) ( UInt* tt_used, UInt* tc_used )
{
   Int s;
   *tt_used = vg_tt_used;
   *tc_used = 0;
   for (s = 0; s < VG_TC_N_SECTORS; s++)
      *tc_used += vg_tc_used[s];
}


/* Do a sanity check on TT/TC.
*/
void VG_(sanity_check_tc_tt) ( void )
{
   Int i, s;
   TTEntry* tte;
   TCEntry* tce;
   /* Checks: 
      - Each TT entry points to a valid and corresponding TC entry.
   */
   for (i = 0; i < VG_TT_SIZE; i++) {
      tte = &vg_tt[i];
      /* empty slots are harmless. */
      if (tte->orig_addr == VG_TTE_EMPTY) continue;
      /* all others should agree with the TC entry. */
      tce = tte->tcentry;
      vg_assert(IS_ALIGNED4_ADDR(tce));
      /* does this point into a valid TC sector? */
      for (s = 0; s < VG_TC_N_SECTORS; s++)
	if (vg_tc[s] != NULL
            && ((Addr)tce) >= (Addr)&vg_tc[s][0]
            && ((Addr)tce) <  (Addr)&vg_tc[s][ vg_tc_used[s] ])
	  break; 
      vg_assert(s < VG_TC_N_SECTORS);
      /* It should agree with the TC entry on the orig_addr.  This may
         be VG_TTE_DELETED, or a real orig addr. */
      vg_assert(tte->orig_addr == tce->orig_addr);
   }
}


/* Add this already-filled-in entry to the TT.  Assumes that the
   relevant code chunk has been placed in TC, along with a dummy back
   pointer, which is inserted here.
*/
void VG_(add_to_trans_tab) ( Addr orig_addr,  Int orig_size,
                             Addr trans_addr, Int trans_size,
			     UShort jumps[VG_MAX_JUMPS])
{
   Int i, nBytes, trans_size_aligned;
   TCEntry* tce;
   /*
   VG_(printf)("add_to_trans_tab(%d) %x %d %x %d\n",
               vg_tt_used, tte->orig_addr, tte->orig_size, 
               tte->trans_addr, tte->trans_size);
   */

   vg_assert(offsetof(TCEntry, payload) == VG_CODE_OFFSET);

   /* figure out how many bytes we require. */
   nBytes = CODE_ALIGN(trans_size + sizeof(TCEntry));
   trans_size_aligned = nBytes-sizeof(TCEntry);
   vg_assert(IS_ALIGNED(nBytes));

   tce = (TCEntry*)allocate(nBytes);
   /*
   VG_(printf)("allocate returned %p (code start %p)\n", 
               tce, &tce->payload[0]);
   */
   vg_assert(vg_tc_current >= 0 && vg_tc_current < VG_TC_N_SECTORS);

   tce->orig_addr  = orig_addr;
   tce->orig_size  = (UShort)orig_size;  /* what's the point of storing this? */
   tce->trans_size = (UShort)trans_size_aligned;
   for (i = 0; i < VG_MAX_JUMPS; i++) {
      tce->jump_sites[i] = jumps[i];
   }
   for (i = 0; i < trans_size; i++) {
      tce->payload[i] = ((UChar*)trans_addr)[i];
   }
   
   unchain_tce(tce);
   add_tt_entry(tce);

   /* Update stats. */
   VG_(overall_in_count) ++;
   VG_(overall_in_osize) += orig_size;
   VG_(overall_in_tsize) += trans_size;

   vg_tc_stats_count[vg_tc_current] ++;
   vg_tc_stats_osize[vg_tc_current] += orig_size;
   vg_tc_stats_tsize[vg_tc_current] += trans_size;
}


/* Find the translation address for a given (original) code address.
   If found, update VG_(tt_fast) so subsequent lookups are fast.  If
   no translation can be found, return zero.  This routine is (the
   only one) called from vg_run_innerloop.  */
Addr VG_(search_transtab) ( Addr original_addr )
{
   TTEntry* tte;
   VGP_PUSHCC(VgpSlowFindT);
   tte = search_tt ( original_addr );
   if (tte == NULL) {
      /* We didn't find it.  vg_run_innerloop will have to request a
         translation. */
      VGP_POPCC(VgpSlowFindT);
      return (Addr)0;
   } else {
      /* Found it.  Put the search result into the fast cache now. */
      UInt cno = (UInt)original_addr & VG_TT_FAST_MASK;
      VG_(tt_fast)[cno] = (Addr)(tte->tcentry);
      VG_(tt_fast_misses)++;
      VGP_POPCC(VgpSlowFindT);
      return (Addr)&(tte->tcentry->payload[0]);
   }
}


/* Invalidate translations of original code [start .. start + range - 1].
   This is slow, so you *really* don't want to call it very often.
   Set 'unchain_blocks' if the translation being invalidated may be chained
   to by other local blocks (which are NOT being discarded).
*/
void VG_(invalidate_translations) ( Addr start, UInt range, Bool unchain_blocks )
{
   Addr     i_start, i_end, o_start, o_end;
   UInt     out_count, out_osize, out_tsize;
   Int      i, j;
   TCEntry* tce;
#  ifdef DEBUG_TRANSTAB
   VG_(sanity_check_tc_tt)();
#  endif
   i_start = start;
   i_end   = start + range - 1;
   out_count = out_osize = out_tsize = 0;

   for (i = 0; i < VG_TT_SIZE; i++) {
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY
          || vg_tt[i].orig_addr == VG_TTE_DELETED) continue;
      tce = vg_tt[i].tcentry;
      o_start = tce->orig_addr;
      o_end   = o_start + tce->trans_size - 1;
      if (o_end < i_start || o_start > i_end)
         continue;

      if (VG_(needs).basic_block_discards)
         SK_(discard_basic_block_info)( tce->orig_addr, 
                                        tce->orig_size );

      vg_tt[i].orig_addr = VG_TTE_DELETED;
      tce->orig_addr = VG_TTE_DELETED;

      if (unchain_blocks) {
         /* make sure no other blocks chain to the one we just discarded */
         for(j = 0; j < VG_TC_N_SECTORS; j++) {
            if (vg_tc[j] != NULL)
               unchain_sector(j, (Addr)tce->payload, tce->trans_size);
         }
      }

      VG_(overall_out_count) ++;
      VG_(overall_out_osize) += tce->orig_size;
      VG_(overall_out_tsize) += tce->trans_size;
      out_count ++;
      out_osize += tce->orig_size;
      out_tsize += tce->trans_size;
   }

   if (out_count > 0) {
      vg_invalidate_tt_fast();
      VG_(sanity_check_tc_tt)();
#     ifdef DEBUG_TRANSTAB
      { Addr aa;
        for (aa = i_start; aa <= i_end; aa++)
           vg_assert(search_tt ( aa ) == NULL);
      }
#     endif
   }

   if (VG_(clo_verbosity) > 2)
      VG_(message)(Vg_UserMsg,   
         "discard %d (%d -> %d) translations in range %p .. %p",
         out_count, out_osize, out_tsize, i_start, i_end );
}


/*------------------------------------------------------------*/
/*--- Initialisation.                                      ---*/
/*------------------------------------------------------------*/

void VG_(init_tt_tc) ( void )
{
   Int s;

   /* Otherwise we wind up with non-32-bit-aligned code in
      TCEntries. */
   vg_assert((VG_MAX_JUMPS % 2) == 0);

   /* Figure out how big each sector should be.  */
   vg_tc_sector_szB 
      = (VG_TT_LIMIT /* max TT entries we expect */
         * (VG_(details).avg_translation_sizeB 
            + sizeof(TCEntry)
            + (CODE_ALIGNMENT/2) /* avg alignment loss */)
        )
        / VG_TC_N_SECTORS;
   /* Ensure the calculated value is not way crazy. */
   vg_assert(vg_tc_sector_szB >= 200000);
   vg_assert(vg_tc_sector_szB <= 8000000);

   for (s = 0; s < VG_TC_N_SECTORS; s++) {
      vg_tc[s] = NULL;
      vg_tc_used[s] = 0;
      vg_tc_age[s] = 0;
      vg_tc_stats_count[s] = 0;
      vg_tc_stats_osize[s] = 0;
      vg_tc_stats_tsize[s] = 0;
   }
   vg_tc_current = 0;

   vg_tt = VG_(get_memory_from_mmap) ( VG_TT_SIZE * sizeof(TTEntry),
                                       "trans-table" );
   /* The main translation table is empty. */
   initialise_tt();

   if (VG_(clo_verbosity) > 2) {
      VG_(message)(Vg_DebugMsg,
         "Translation Cache: using %d sectors of %d bytes each", 
          VG_TC_N_SECTORS, vg_tc_sector_szB );
      VG_(message)(Vg_DebugMsg,
         "Translation Table: %d total entries, max occupancy %d (%d%%)",
         VG_TT_SIZE, VG_TT_LIMIT, VG_TT_LIMIT_PERCENT );
   }

#  ifdef DEBUG_TRANSTAB
   VG_(sanity_check_tc_tt)();
#  endif
}

/*--------------------------------------------------------------------*/
/*--- end                                            vg_transtab.c ---*/
/*--------------------------------------------------------------------*/
