
/*--------------------------------------------------------------------*/
/*--- Management of the translation table and cache.               ---*/
/*---                                                vg_transtab.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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

#include "core.h"
#include "pub_core_tooliface.h"

/* #define DEBUG_TRANSTAB */


/*-------------------------------------------------------------*/
/*--- Management of the FIFO-based translation table+cache. ---*/
/*-------------------------------------------------------------*/

/*------------------ CONSTANTS ------------------*/

/* Number of sectors the TC is divided into.  If you need a larger
   overall translation cache, increase this value. */
#define N_SECTORS 8

/* Number of TC entries in each sector.  This needs to be a prime
   number to work properly, and it is strongly recommended not to
   change this. */
#define N_TTES_PER_SECTOR /*30011*/ 40009

/* Because each sector contains a hash table of TTEntries, we need to
   specify the maximum allowable loading, after which the sector is
   deemed full. */
#define SECTOR_TT_LIMIT_PERCENT 60

/* The sector is deemed full when this many entries are in it. */
#define N_TTES_PER_SECTOR_USABLE \
           ((N_TTES_PER_SECTOR * SECTOR_TT_LIMIT_PERCENT) / 100)


/*------------------ TYPES ------------------*/

/* A translation-cache entry is two parts:
   - The guest address of the first (entry) bb in the translation,
     as a 64-bit word.
   - One or more 64-bit words containing the code.
   It is supposed to be 64-bit aligned.
*/
/*
typedef
   struct {
      Addr64 orig_addr;
      ULong  code[0];
   }
   TCEntry;
*/

/* A translation-table entry.  This indicates precisely which areas of
   guest code are included in the translation, and contains all other
   auxiliary info too.  */
typedef
   struct {
      /* Profiling only: the count and weight (arbitrary meaning) for
         this translation.  Weight is a property of the translation
         itself and computed once when the translation is created.
         Count is an entry count for the translation and is
         incremented by 1 every time the translation is used, if we
         are profiling. */
      UInt     count;
      UShort   weight;

      /* Status of the slot.  Note, we need to be able to do lazy
         deletion, hence the Deleted state. */
      enum { InUse, Deleted, Empty } status;

      /* Pointer to the corresponding TCEntry (must be in the same
         sector!) */
      ULong* tce;

      /* This is the original guest address that purportedly is the
         entry point of the translation.  You might think that .entry
         should be the same as .vge->base[0], and most of the time it
         is.  However, when doing redirections, that is not the case.
         .vge must always correctly describe the guest code sections
         from which this translation was made.  However, .entry may or
         may not be a lie, depending on whether or not we're doing
         redirection. */
      Addr64 entry;

      /* This structure describes precisely what ranges of guest code
         the translation covers, so we can decide whether or not to
         delete it when translations of a given address range are
         invalidated. */
      VexGuestExtents vge;
   }
   TTEntry;


/* Finally, a sector itself.  Each sector contains an array of
   TCEntries, which hold code, and an array of TTEntries, containing
   all required administrative info.  Profiling is supported using the
   TTEntry .count and .weight fields, if required.  Each sector is
   independent in that no cross-sector references are allowed.

   If the sector is not in use, all three pointers are NULL and
   tt_n_inuse is zero.  
*/
typedef
   struct {
      /* The TCEntry area.  Size of this depends on the average
         translation size.  We try and size it so it becomes full
         precisely when this sector's translation table (tt) reaches
         its load limit (SECTOR_TT_LIMIT_PERCENT). */
      ULong* tc;

      /* The TTEntry array.  This is a fixed size, always containing
         exactly N_TTES_PER_SECTOR entries. */
      TTEntry* tt;

      /* This points to the current allocation point in tc. */
      ULong* tc_next;

      /* The count of tt entries with state InUse. */
      Int tt_n_inuse;
   }
   Sector;


/*------------------ DECLS ------------------*/

/* The root data structure is an array of sectors.  The index of the
   youngest sector is recorded, and new translations are put into that
   sector.  When it fills up, we move along to the next sector and
   start to fill that up, wrapping around at the end of the array.
   That way, once all N_TC_SECTORS have been bought into use for the
   first time, and are full, we then re-use the oldest sector,
   endlessly. 

   When running, youngest sector should be between >= 0 and <
   N_TC_SECTORS.  The initial -1 value indicates the TT/TC system is
   not yet initialised. 
*/
static Sector sectors[N_SECTORS];
static Int    youngest_sector = -1;

/* The number of ULongs in each TCEntry area.  This is computed once
   at startup and does not change. */
static Int    tc_sector_szQ;


/* Fast helper for the TC.  A direct-mapped cache which holds a
   pointer to a TC entry which may or may not be the correct one, but
   which we hope usually is.  This array is referred to directly from
   <arch>/dispatch.S.

   Entries in tt_fast may point to any valid TC entry, regardless of
   which sector it's in.  Consequently we must be very careful to
   invalidate this cache when TC entries are changed or disappear.

   A special TCEntry -- bogus_tc_entry -- must be pointed at to cause
   that cache entry to miss.  This relies on the assumption that no
   guest code actually has an address of 0x1.
*/
/*global*/ ULong* VG_(tt_fast)[VG_TT_FAST_SIZE];

static ULong bogus_tc_entry = (Addr64)1;


/* For profiling, we have a parallel array of pointers to .count
   fields in TT entries.  Again, these pointers must be invalidated
   when translations disappear.  A NULL pointer suffices to indicate
   an unused slot.

   tt_fast and tt_fastN change together: if tt_fast[i] points to
   bogus_tc_entry then the corresponding tt_fastN[i] must be null.  If
   tt_fast[i] points to some TC entry somewhere, then tt_fastN[i]
   *must* point to the .count field of the corresponding TT entry.

   tt_fast and tt_fastN are referred to from assembly code
   (dispatch.S).
*/
/*global*/ UInt* VG_(tt_fastN)[VG_TT_FAST_SIZE];


/* Make sure we're not used before initialisation. */
static Bool init_done = False;


/*------------------ STATS DECLS ------------------*/

/* Number of fast-cache updates and flushes done. */
ULong n_fast_flushes = 0;
ULong n_fast_updates = 0;

/* Number of full lookups done. */
ULong n_full_lookups = 0;
ULong n_lookup_probes = 0;

/* Number/osize/tsize of translations entered. */
ULong n_in_count = 0;
ULong n_in_osize = 0;
ULong n_in_tsize = 0;

/* Number/osize of translations discarded due to lack of space. */
ULong n_dump_count = 0;
ULong n_dump_osize = 0;

/* Number/osize of translations discarded due to requests to do so. */
ULong n_disc_count = 0;
ULong n_disc_osize = 0;



/*-------------------------------------------------------------*/
/*--- Add/delete/find translations                          ---*/
/*-------------------------------------------------------------*/

static UInt vge_osize ( VexGuestExtents* vge )
{
   UInt i, n = 0;
   for (i = 0; i < vge->n_used; i++)
      n += (UInt)vge->len[i];
   return n;
}

static Bool isValidSector ( Int sector )
{
   if (sector < 0 || sector >= N_SECTORS)
      return False;
   return True;
}

static inline UInt HASH_TT ( Addr64 key )
{
   UInt kHi = (UInt)(key >> 32);
   UInt kLo = (UInt)key;
   return (kHi ^ kLo) % N_TTES_PER_SECTOR;
}

static void setFastCacheEntry ( Addr64 key, ULong* tce, UInt* count )
{
   UInt cno = ((UInt)key) & VG_TT_FAST_MASK;
   VG_(tt_fast)[cno]  = tce;
   VG_(tt_fastN)[cno] = count;
   n_fast_updates++;
}

static void invalidateFastCache ( void )
{
   UInt j;
   for (j = 0; j < VG_TT_FAST_SIZE; j++) {
      VG_(tt_fast)[j]  = &bogus_tc_entry;
      VG_(tt_fastN)[j] = NULL;
   }
   n_fast_flushes++;
}

static void initialiseSector ( Int sno )
{
   Int i;
   vg_assert(isValidSector(sno));

   if (sectors[sno].tc == NULL) {
      /* Sector has never been used before.  Need to allocate tt and
         tc. */
      vg_assert(sectors[sno].tt == NULL);
      vg_assert(sectors[sno].tc_next == NULL);
      vg_assert(sectors[sno].tt_n_inuse == 0);
      sectors[sno].tc 
         = VG_(get_memory_from_mmap)
              ( 8 * tc_sector_szQ, "sectors[sno].tc" );
      sectors[sno].tt 
         = VG_(get_memory_from_mmap) 
              ( N_TTES_PER_SECTOR * sizeof(TTEntry), "sectors[sno].tt" );
      if (VG_(clo_verbosity) > 2)
         VG_(message)(Vg_DebugMsg, "TT/TC: initialise sector %d", sno);
   } else {
      /* Sector has been used before. */
      vg_assert(sectors[sno].tt != NULL);
      vg_assert(sectors[sno].tc_next != NULL);
      n_dump_count += sectors[sno].tt_n_inuse;
      for (i = 0; i < N_TTES_PER_SECTOR; i++) {
         if (sectors[sno].tt[i].status == InUse) {
            n_dump_osize += vge_osize(&sectors[sno].tt[i].vge);
         }
      }
      if (VG_(clo_verbosity) > 2)
         VG_(message)(Vg_DebugMsg, "TT/TC: recycle sector %d", sno);
   }

   sectors[sno].tc_next = sectors[sno].tc;
   sectors[sno].tt_n_inuse = 0;
   for (i = 0; i < N_TTES_PER_SECTOR; i++)
      sectors[sno].tt[i].status = Empty;

   invalidateFastCache();
}


/* Add a translation of vge to TT/TC.  The translation is temporarily
   in code[0 .. code_len-1].

   pre: youngest_sector points to a valid (although possibly full)
   sector.
*/
void VG_(add_to_trans_tab)( VexGuestExtents* vge,
                            Addr64           entry,
                            AddrH            code,
                            UInt             code_len )
{
   Int    tcAvailQ, reqdQ, y, i;
   ULong  *tce, *tce2;
   UChar* srcP;
   UChar* dstP;

   vg_assert(init_done);
   vg_assert(vge->n_used >= 1 && vge->n_used <= 3);
   vg_assert(code_len > 0 && code_len < 20000);

   if (0)
      VG_(printf)("add_to_trans_tab(entry = 0x%llx, len = %d)\n",
                  entry, code_len);

   n_in_count++;
   n_in_tsize += code_len;
   n_in_osize += vge_osize(vge);

   y = youngest_sector;
   vg_assert(isValidSector(y));

   if (sectors[y].tc == NULL)
      initialiseSector(y);

   /* Try putting the translation in this sector. */
   reqdQ = 1 + ((code_len + 7) >> 3);

   /* Will it fit in tc? */
   tcAvailQ = ((ULong*)(&sectors[y].tc[tc_sector_szQ]))
              - ((ULong*)(sectors[y].tc_next));
   vg_assert(tcAvailQ >= 0);
   vg_assert(tcAvailQ <= tc_sector_szQ);

   if (tcAvailQ < reqdQ 
       || sectors[y].tt_n_inuse >= N_TTES_PER_SECTOR_USABLE) {
      /* No.  So move on to the next sector.  Either it's never been
         used before, in which case it will get its tt/tc allocated
         now, or it has been used before, in which case it is set to be
         empty, hence throwing out the oldest sector. */
      youngest_sector++;
      if (youngest_sector >= N_SECTORS)
         youngest_sector = 0;
      y = youngest_sector;
      initialiseSector(y);
   }

   /* Be sure ... */
   tcAvailQ = ((ULong*)(&sectors[y].tc[tc_sector_szQ]))
              - ((ULong*)(sectors[y].tc_next));
   vg_assert(tcAvailQ >= 0);
   vg_assert(tcAvailQ <= tc_sector_szQ);
   vg_assert(tcAvailQ >= reqdQ);
   vg_assert(sectors[y].tt_n_inuse < N_TTES_PER_SECTOR_USABLE);
   vg_assert(sectors[y].tt_n_inuse >= 0);
 
   /* Copy into tc. */
   tce = sectors[y].tc_next;
   vg_assert(tce >= &sectors[y].tc[0]);
   vg_assert(tce <= &sectors[y].tc[tc_sector_szQ]);

   tce[0] = entry;
   dstP = (UChar*)(&tce[1]);
   srcP = (UChar*)code;
   for (i = 0; i < code_len; i++)
      dstP[i] = srcP[i];
   sectors[y].tc_next += reqdQ;
   sectors[y].tt_n_inuse++;

   /* more paranoia */
   tce2 = sectors[y].tc_next;
   vg_assert(tce2 >= &sectors[y].tc[0]);
   vg_assert(tce2 <= &sectors[y].tc[tc_sector_szQ]);

   /* Find an empty tt slot, and use it.  There must be such a slot
      since tt is never allowed to get completely full. */
   i = HASH_TT(entry);
   vg_assert(i >= 0 && i < N_TTES_PER_SECTOR);
   while (True) {
      if (sectors[y].tt[i].status == Empty
          || sectors[y].tt[i].status == Deleted)
         break;
      i++;
      if (i >= N_TTES_PER_SECTOR)
         i = 0;
   }

   sectors[y].tt[i].status = InUse;
   sectors[y].tt[i].tce    = tce;
   sectors[y].tt[i].count  = 0;
   sectors[y].tt[i].weight = 1;
   sectors[y].tt[i].vge    = *vge;
   sectors[y].tt[i].entry  = entry;

   setFastCacheEntry( entry, tce, &sectors[y].tt[i].count );
}


/* Search for the translation of the given guest address.  If
   requested, a successful search can also cause the fast-caches to be
   updated.  
*/
Bool VG_(search_transtab) ( /*OUT*/AddrH* result,
                            Addr64        guest_addr, 
                            Bool          upd_cache )
{
   Int i, j, k, kstart, sno;

   vg_assert(init_done);
   /* Find the initial probe point just once.  It will be the same in
      all sectors and avoids multiple expensive % operations. */
   n_full_lookups++;
   k      = -1;
   kstart = HASH_TT(guest_addr);
   vg_assert(kstart >= 0 && kstart < N_TTES_PER_SECTOR);

   /* Search in all the sectors.  Although the order should not matter,
      it might be most efficient to search in the order youngest to
      oldest. */
   sno = youngest_sector;
   for (i = 0; i < N_SECTORS; i++) {

      if (sectors[sno].tc == NULL)
         goto notfound; /* sector not in use. */

      k = kstart;
      for (j = 0; j < N_TTES_PER_SECTOR; j++) {
         n_lookup_probes++;
         if (sectors[sno].tt[k].status == InUse
             && sectors[sno].tt[k].entry == guest_addr) {
            /* found it */
            if (upd_cache)
               setFastCacheEntry( 
                  guest_addr, sectors[sno].tt[k].tce, 
                              &sectors[sno].tt[k].count );
            if (result)
               *result = sizeof(Addr64) + (AddrH)sectors[sno].tt[k].tce;
            return True;
         }
         if (sectors[sno].tt[k].status == Empty)
            break; /* not found in this sector */
         k++;
         if (k == N_TTES_PER_SECTOR)
            k = 0;
      }

      /* If we fall off the end, all entries are InUse and not
         matching, or Deleted.  In any case we did not find it in this
         sector. */

     notfound:
      /* move to the next oldest sector */
      sno = sno==0 ? (N_SECTORS-1) : (sno-1);
   }

   /* Not found in any sector. */
   return False;
}


/* Delete all translations which intersect with any part of the
   specified guest address range.  Note, this is SLOW. 
*/

static inline
Bool overlap1 ( Addr64 s1, UInt r1, Addr64 s2, UInt r2 )
{
   Addr64 e1 = s1 + (ULong)r1 - 1ULL;
   Addr64 e2 = s2 + (ULong)r1 - 1ULL;
   if (e1 < s2 || e2 < s1) 
      return False;
   return True;
}

static inline
Bool overlaps ( Addr64 start, UInt range, VexGuestExtents* vge )
{
   if (overlap1(start, range, vge->base[0], (UInt)vge->len[0]))
      return True;
   if (vge->n_used < 2)
      return False;
   if (overlap1(start, range, vge->base[1], (UInt)vge->len[1]))
      return True;
   if (vge->n_used < 3)
      return False;
   if (overlap1(start, range, vge->base[2], (UInt)vge->len[2]))
      return True;
   return False;
}


void VG_(discard_translations) ( Addr64 guest_start, UInt range )
{
   Int sno, i;
   Bool anyDeleted = False;

   vg_assert(init_done);

   for (sno = 0; sno < N_SECTORS; sno++) {
      if (sectors[sno].tc == NULL)
         continue;
      for (i = 0; i < N_TTES_PER_SECTOR; i++) {
         if (sectors[sno].tt[i].status == InUse
             && overlaps( guest_start, range, &sectors[sno].tt[i].vge )) {
            sectors[sno].tt[i].status = Deleted;
            sectors[sno].tt_n_inuse--;
              anyDeleted = True;
            n_disc_count++;
            n_disc_osize += vge_osize(&sectors[sno].tt[i].vge);
         }
      }    
   }

   if (anyDeleted)
      invalidateFastCache();
}


/*------------------------------------------------------------*/
/*--- Sanity checking                                      ---*/
/*------------------------------------------------------------*/

void VG_(sanity_check_tt_tc) ( Char* who )
{
}


/*------------------------------------------------------------*/
/*--- Initialisation.                                      ---*/
/*------------------------------------------------------------*/

void VG_(init_tt_tc) ( void )
{
   Int i, avg_codeszQ;

   vg_assert(!init_done);
   init_done = True;

   /* Otherwise lots of things go wrong... */
   vg_assert(sizeof(ULong) == 8);
   vg_assert(sizeof(Addr64) == 8);

   if (VG_(clo_verbosity) > 2)
      VG_(message)(Vg_DebugMsg, 
                   "TT/TC: VG_(init_tt_tc) "
                   "(startup of code management)");

   /* Figure out how big each tc area should be.  */
   avg_codeszQ   = (VG_(details).avg_translation_sizeB + 7) / 8;
   tc_sector_szQ = N_TTES_PER_SECTOR_USABLE * (1 + avg_codeszQ);

   /* Ensure the calculated value is not way crazy. */
   vg_assert(tc_sector_szQ >= 2 * N_TTES_PER_SECTOR_USABLE);
   vg_assert(tc_sector_szQ <= 50 * N_TTES_PER_SECTOR_USABLE);

   /* Initialise the sectors */
   youngest_sector = 0;
   for (i = 0; i < N_SECTORS; i++) {
      sectors[i].tc = NULL;
      sectors[i].tt = NULL;
      sectors[i].tc_next = NULL;
      sectors[i].tt_n_inuse = 0;
   }

   /* and the fast caches. */
   invalidateFastCache();

   if (VG_(clo_verbosity) > 2) {
      VG_(message)(Vg_DebugMsg,
         "TT/TC: cache: %d sectors of %d bytes each = %d total", 
          N_SECTORS, 8 * tc_sector_szQ,
          N_SECTORS * 8 * tc_sector_szQ );
      VG_(message)(Vg_DebugMsg,
         "TT/TC: table: %d total entries, max occupancy %d (%d%%)",
         N_SECTORS * N_TTES_PER_SECTOR,
         N_SECTORS * N_TTES_PER_SECTOR_USABLE, 
         SECTOR_TT_LIMIT_PERCENT );
   }
}


/*------------------------------------------------------------*/
/*--- Printing out statistics.                             ---*/
/*------------------------------------------------------------*/

static ULong safe_idiv( ULong a, ULong b )
{
   return (b == 0 ? 0 : a / b);
}

UInt VG_(get_bbs_translated) ( void )
{
   return n_in_count;
}

void VG_(print_tt_tc_stats) ( void )
{
   VG_(message)(Vg_DebugMsg,
      "    tt/tc: %llu tt lookups requiring %llu probes", 
      n_full_lookups, n_lookup_probes );
   VG_(message)(Vg_DebugMsg,
      "    tt/tc: %llu fast-cache updates, %llu flushes", 
      n_fast_updates, n_fast_flushes );

   VG_(message)(Vg_DebugMsg,
                "translate: new        %lld (%lld -> %lld; ratio %lld:10)",
                n_in_count, n_in_osize, n_in_tsize,
                safe_idiv(10*n_in_tsize, n_in_osize));
   VG_(message)(Vg_DebugMsg,
                "translate: dumped     %lld (%lld -> ?" "?)",
                n_dump_count, n_dump_osize );
   VG_(message)(Vg_DebugMsg,
                "translate: discarded  %lld (%lld -> ?" "?)",
                n_disc_count, n_disc_osize );
}

/*------------------------------------------------------------*/
/*--- Printing out of profiling results.                   ---*/
/*------------------------------------------------------------*/

/* Only the top N_MAX bbs will be displayed. */
#define N_MAX 200

static TTEntry* tops[N_MAX];

static ULong score ( TTEntry* tte )
{
   return ((ULong)tte->weight) * ((ULong)tte->count);
}

static Bool heavier ( TTEntry* t1, TTEntry* t2 )
{
   return score(t1) > score(t2);
}

/* Print n/m in form xx.yy% */
static
void percentify ( ULong n, ULong m, Int field_width, Char* buf)
{
   Int i, len, space;
   ULong lo, hi;
   if (m == 0) m = 1; /* stay sane */
   hi = (n * 100) / m;
   lo = (((n * 100) - hi * m) * 100) / m;
   vg_assert(lo < 100);
   if (lo < 10)
      VG_(sprintf)(buf, "%lld.0%lld%%", hi, lo);
   else
      VG_(sprintf)(buf, "%lld.%lld%%", hi, lo);

   len = VG_(strlen)(buf);
   space = field_width - len;
   if (space < 0) space = 0;     /* Allow for v. small field_width */
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  buf[i + space] = buf[i];
   for (i = 0; i < space; i++)  buf[i] = ' ';
}


void VG_(show_BB_profile) ( void )
{
   Char  name[64];
   Int   sno, i, r, s;
   ULong score_total, score_cumul, score_here;
   Char  buf_cumul[10];
   Char  buf_here[10];

   /* First, compute the total weighted count, and find the top N
      ttes.  tops contains pointers to the most-used N_MAX blocks, in
      descending order (viz, tops[0] is the highest scorer). */
   for (i = 0; i < N_MAX; i++)
      tops[i] = NULL;

   score_total = 0;

   for (sno = 0; sno < N_SECTORS; sno++) {
      if (sectors[sno].tc == NULL)
         continue;
      for (i = 0; i < N_TTES_PER_SECTOR; i++) {
         if (sectors[sno].tt[i].status != InUse)
            continue;
         score_total += score(&sectors[sno].tt[i]);
         /* Find the rank for sectors[sno].tt[i]. */
         r = N_MAX-1;
         while (True) {
            if (r == -1)
               break;
             if (tops[r] == NULL) {
               r--; 
               continue;
             }
             if (heavier(&sectors[sno].tt[i], tops[r])) {
                r--;
                continue;
             }
             break;
         }
         r++;
         vg_assert(r >= 0 && r <= N_MAX);
         /* This bb should be placed at r, and bbs above it shifted
            upwards one slot. */
         if (r < N_MAX) {
            for (s = N_MAX-1; s > r; s--)
               tops[s] = tops[s-1];
            tops[r] = &sectors[sno].tt[i];
         }
      }
   }

   VG_(printf)("\n");
   VG_(printf)("------------------------------------------------------------\n");
   VG_(printf)("--- BEGIN BB Profile (summary of scores)                 ---\n");
   VG_(printf)("------------------------------------------------------------\n");
   VG_(printf)("\n");

   VG_(printf)("Total score = %lld\n\n", score_total);

   score_cumul = 0;
   for (r = 0; r < N_MAX; r++) {
      if (tops[r] == NULL)
         continue;
      name[0] = 0;
      VG_(get_fnname_w_offset)(tops[r]->entry, name, 64);
      name[63] = 0;
      score_here = score(tops[r]);
      score_cumul += score_here;
      percentify(score_cumul, score_total, 6, buf_cumul);
      percentify(score_here,  score_total, 6, buf_here);
      VG_(printf)("%3d: (%9lld %s)   %9lld %s      0x%llx %s\n",
                  r,
                  score_cumul, buf_cumul,
                  score_here,  buf_here, tops[r]->entry, name );
   }

   VG_(printf)("\n");
   VG_(printf)("------------------------------------------------------------\n");
   VG_(printf)("--- BB Profile (BB details)                              ---\n");
   VG_(printf)("------------------------------------------------------------\n");
   VG_(printf)("\n");

   score_cumul = 0;
   for (r = 0; r < N_MAX; r++) {
      if (tops[r] == NULL)
         continue;
      name[0] = 0;
      VG_(get_fnname_w_offset)(tops[r]->entry, name, 64);
      name[63] = 0;
      score_here = score(tops[r]);
      score_cumul += score_here;
      percentify(score_cumul, score_total, 6, buf_cumul);
      percentify(score_here,  score_total, 6, buf_here);
      VG_(printf)("\n");
      VG_(printf)("=-=-=-=-=-=-=-=-=-=-=-=-=-= begin BB rank %d "
                  "=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n", r);
      VG_(printf)("%3d: (%9lld %s)   %9lld %s      0x%llx %s\n",
                  r,
                  score_cumul, buf_cumul,
                  score_here,  buf_here, tops[r]->entry, name );
      VG_(printf)("\n");
      VG_(translate)(0, tops[r]->entry, True, VG_(clo_profile_flags));
      VG_(printf)("=-=-=-=-=-=-=-=-=-=-=-=-=-=  end BB rank %d  "
                  "=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n", r);
   }

   VG_(printf)("\n");
   VG_(printf)("------------------------------------------------------------\n");
   VG_(printf)("--- END BB Profile                                       ---\n");
   VG_(printf)("------------------------------------------------------------\n");
   VG_(printf)("\n");
}


/*--------------------------------------------------------------------*/
/*--- end                                            vg_transtab.c ---*/
/*--------------------------------------------------------------------*/
