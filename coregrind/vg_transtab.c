
/*--------------------------------------------------------------------*/
/*--- Management of the translation table and cache.               ---*/
/*---                                                vg_transtab.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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

/* #define DEBUG_TRANSTAB */


/*------------------------------------------------------------*/
/*--- Management of the LRU-based translation table+cache. ---*/
/*------------------------------------------------------------*/

/* These sizes were set up so as to be able to debug large KDE 3
   applications (are there any small ones?) without excessive amounts
   of code retranslation.  */

/* Size of the translation cache, in bytes. */
#define VG_TC_SIZE /*1000000*/ /*16000000*/ 32000000 /*40000000*/

/* Do a LRU pass when the translation cache becomes this full. */
#define VG_TC_LIMIT_PERCENT 98

/* When doing an LRU pass, reduce TC fullness to this level. */
#define VG_TC_TARGET_PERCENT 85

/* Number of entries in the translation table.  This must be a prime
   number in order to make the hashing work properly. */
#define VG_TT_SIZE /*5281*/ /*100129*/ 200191 /*250829*/

/* Do an LRU pass when the translation table becomes this full. */
#define VG_TT_LIMIT_PERCENT /*67*/ 80

/* When doing an LRU pass, reduce TT fullness to this level. */
#define VG_TT_TARGET_PERCENT /*60*/ 70

/* The number of age steps we track.  0 means the current epoch,
   N_EPOCHS-1 means used the epoch N_EPOCHS-1 or more ago.  */
#define VG_N_EPOCHS /*2000*/ /*4000*/ 20000

/* This TT entry is empty.  There is no associated TC storage. */
#define VG_TTE_EMPTY   ((Addr)1)
/* This TT entry has been deleted, in the sense that it does not
   contribute to the orig->trans mapping.  However, the ex-translation
   it points at still occupies space in TC.  This slot cannot be
   re-used without doing an LRU pass. */
#define VG_TTE_DELETED ((Addr)3)

/* The TC.  This used to be statically allocated, but that forces many
   SecMap arrays to be pointlessly allocated at startup, bloating the
   process size by about 22M and making startup slow.  So now we
   dynamically allocate it at startup time.
   was: static UChar vg_tc[VG_TC_SIZE];
*/
static UChar* vg_tc = NULL;

/* Count of bytes used in the TC.  This includes those pointed to from
   VG_TTE_DELETED entries. */
static Int vg_tc_used = 0;

/* The TT.  Like TC, for the same reason, is dynamically allocated at
   startup. 
   was: static TTEntry vg_tt[VG_TT_SIZE];
*/
static TTEntry* vg_tt = NULL;

/* Count of non-empty TT entries.  This includes deleted ones. */
static Int vg_tt_used = 0;

/* Fast helper for the TT.  A direct-mapped cache which holds a
   pointer to a TT entry which may or may not be the correct one, but
   which we hope usually is.  This array is referred to directly from
   vg_dispatch.S. */
Addr VG_(tt_fast)[VG_TT_FAST_SIZE];

/* For reading/writing the misaligned TT-index word at immediately
   preceding every translation in TC. */
#if 0
   /* Big sigh.  However reasonable this seems, there are those who
      set AC in %EFLAGS (Alignment Check) to 1, causing bus errors.  A
      proper solution is for valgrind to properly virtualise AC, like
      the other flags (DOSZACP).  The current cheap hack simply avoids
      all misaligned accesses, so valgrind doesn't fault even if AC is
      set. */
#  define VG_READ_MISALIGNED_WORD(aaa) (*((UInt*)(aaa)))
#  define VG_WRITE_MISALIGNED_WORD(aaa,vvv) *((UInt*)(aaa)) = ((UInt)(vvv))
#else
  static __inline__
  UInt VG_READ_MISALIGNED_WORD ( Addr aaa )
  {
     UInt w = 0;
     UChar* p = (UChar*)aaa;
     w = 0xFF & ((UInt)(p[3]));
     w = (w << 8) | (0xFF & ((UInt)(p[2])));
     w = (w << 8) | (0xFF & ((UInt)(p[1])));
     w = (w << 8) | (0xFF & ((UInt)(p[0])));
     return w;
  }

  static __inline__
  void VG_WRITE_MISALIGNED_WORD ( Addr aaa, UInt vvv )
  {
     UChar* p = (UChar*)aaa;
     p[0] = vvv & 0xFF;
     p[1] = (vvv >> 8) & 0xFF;
     p[2] = (vvv >> 16) & 0xFF;
     p[3] = (vvv >> 24) & 0xFF;
  }
#endif


/* Used for figuring out an age threshold for translations. */
static Int vg_bytes_in_epoch[VG_N_EPOCHS];
static Int vg_entries_in_epoch[VG_N_EPOCHS];


/* Just so these counts can be queried without making them globally
   visible. */
void VG_(get_tt_tc_used) ( UInt* tt_used, UInt* tc_used )
{
   *tt_used = vg_tt_used;
   *tc_used = vg_tc_used;
}


/* Do the LRU thing on TT/TC, clearing them back to the target limits
   if they are over the threshold limits. 
*/
void VG_(maybe_do_lru_pass) ( void )
{
   Int i, j, r, w, thresh, ttno;
   TTEntry* tte;

   const Int tc_limit  = (Int)(((double)VG_TC_SIZE * (double)VG_TC_LIMIT_PERCENT)
                                / (double)100.0);
   const Int tt_limit  = (Int)(((double)VG_TT_SIZE * (double)VG_TT_LIMIT_PERCENT)
                                / (double)100.0);
   const Int tc_target = (Int)(((double)VG_TC_SIZE * (double)VG_TC_TARGET_PERCENT)
                                / (double)100.0);
   const Int tt_target = (Int)(((double)VG_TT_SIZE * (double)VG_TT_TARGET_PERCENT)
                                / (double)100.0);

   /* Decide quickly if we need to do an LRU pass ? */
   if (vg_tc_used <= tc_limit && vg_tt_used <= tt_limit)
      return;

#  ifdef DEBUG_TRANSTAB
   VG_(sanity_check_tc_tt)();
#  endif

   VGP_PUSHCC(VgpDoLRU);
   /*   
   VG_(printf)(
      "limits: tc_limit %d, tt_limit %d, tc_target %d, tt_target %d\n",
      tc_limit, tt_limit, tc_target, tt_target);
   */

   if (VG_(clo_verbosity) > 2)
      VG_(printf)(" pre-LRU: tc %d (target %d),  tt %d (target %d)\n",
	          vg_tc_used, tc_target, vg_tt_used, tt_target);

   /* Yes we do.  Figure out what threshold age is required in order to
      shrink both the TC and TT occupancy below TC_TARGET_PERCENT and
      TT_TARGET_PERCENT respectively. */

   VG_(number_of_lrus)++;

   /* Count the number of TC bytes and TT entries in each epoch. */
   for (i = 0; i < VG_N_EPOCHS; i++)
      vg_bytes_in_epoch[i] = vg_entries_in_epoch[i] = 0;

   for (i = 0; i < VG_TT_SIZE; i++) {
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY 
          || vg_tt[i].orig_addr == VG_TTE_DELETED) 
            continue;
      j = vg_tt[i].mru_epoch;
      vg_assert(j <= VG_(current_epoch));
      j = VG_(current_epoch) - j;
      if (j >= VG_N_EPOCHS) j = VG_N_EPOCHS-1;
      vg_assert(0 <= j && j < VG_N_EPOCHS);
      /* Greater j now means older. */
      vg_entries_in_epoch[j]++;
      vg_bytes_in_epoch[j] += 4+vg_tt[i].trans_size;
   }

   /*
   for (i = 0; i < VG_N_EPOCHS; i++)
      VG_(printf)("epoch %d: ents %d, bytes %d\n", 
                  i, vg_entries_in_epoch[i], vg_bytes_in_epoch[i]);
   */

   /* Cumulatise.  Make vg_{bytes,entries}_in_epoch[n] contain the
      counts for itself and all younger epochs. */
   for (i = 1; i < VG_N_EPOCHS; i++) {
      vg_entries_in_epoch[i] += vg_entries_in_epoch[i-1];
      vg_bytes_in_epoch[i] += vg_bytes_in_epoch[i-1];
   }

   for (thresh = 0; thresh < VG_N_EPOCHS; thresh++) {
      if (vg_entries_in_epoch[thresh] > tt_target 
          || vg_bytes_in_epoch[thresh] >= tc_target)
         break;
   }

   if (VG_(clo_verbosity) > 2)
      VG_(printf)(
         "     LRU: discard translations %d or more epochs since last use\n",
         thresh
      );

   thresh = VG_(current_epoch) - thresh;

   /* Ok, so we will hit our targets if we retain all entries most
      recently used at most thresh epochs ago.  Traverse the TT and
      mark such entries as deleted. */
   for (i = 0; i < VG_TT_SIZE; i++) {
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY 
          || vg_tt[i].orig_addr == VG_TTE_DELETED) 
         continue;
      if (vg_tt[i].mru_epoch <= thresh) {
         vg_tt[i].orig_addr = VG_TTE_DELETED;
	 VG_(this_epoch_out_count) ++;
	 VG_(this_epoch_out_osize) += vg_tt[i].orig_size;
	 VG_(this_epoch_out_tsize) += vg_tt[i].trans_size;
	 VG_(overall_out_count) ++;
	 VG_(overall_out_osize) += vg_tt[i].orig_size;
	 VG_(overall_out_tsize) += vg_tt[i].trans_size;
      }
   }

   /* Now compact the TC, sliding live entries downwards to fill spaces
      left by deleted entries.  In this loop, r is the offset in TC of
      the current translation under consideration, and w is the next
      allocation point. */
   r = w = 0;
   while (True) {
      if (r >= vg_tc_used) break;
      /* The first four bytes of every translation contain the index
         of its TT entry.  The TT entry's .trans_addr field points at
         the start of the code proper, not at this 4-byte index, so
         that we don't constantly have to keep adding 4 in the main
         lookup/dispatch loop. */

      ttno = VG_READ_MISALIGNED_WORD((Addr)(&vg_tc[r]));
      vg_assert(ttno >= 0 && ttno < VG_TT_SIZE);
      tte = & vg_tt[ ttno ];
      vg_assert(tte->orig_addr != VG_TTE_EMPTY);
      if (tte->orig_addr != VG_TTE_DELETED) {
         /* We want to keep this one alive. */
         /* Sanity check the pointer back to TC. */
         vg_assert(tte->trans_addr == (Addr)&vg_tc[r+4]);
         for (i = 0; i < 4+tte->trans_size; i++)
            vg_tc[w+i] = vg_tc[r+i];
         tte->trans_addr = (Addr)&vg_tc[w+4];
         w += 4+tte->trans_size;
      } else {
         tte->orig_addr = VG_TTE_EMPTY;
         vg_tt_used--;
      }
      r += 4+tte->trans_size;
   }
   /* should have traversed an exact number of translations, with no
      slop at the end. */
   vg_assert(w <= r);
   vg_assert(r == vg_tc_used);
   vg_assert(w <= r);
   vg_assert(w <= tc_target);
   vg_tc_used = w;

   vg_assert(vg_tt_used >= 0);
   vg_assert(vg_tt_used <= tt_target);

   /* Invalidate the fast cache, since it is now out of date.  It will get
      reconstructed incrementally when the client resumes. */
   VG_(invalidate_tt_fast)();

   if (VG_(clo_verbosity) > 2)
      VG_(printf)("post-LRU: tc %d (target %d),  tt %d (target %d)\n",
	          vg_tc_used, tc_target, vg_tt_used, tt_target);

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg,   
         "epoch %d (bb %luk): thresh %d, "
         "out %d (%dk -> %dk), new TT %d, TC %dk",
         VG_(current_epoch), 
         VG_(bbs_done) / 1000,
         VG_(current_epoch) - thresh, 
         VG_(this_epoch_out_count),
         VG_(this_epoch_out_osize) / 1000,
         VG_(this_epoch_out_tsize) / 1000,
         vg_tt_used, vg_tc_used / 1000
      );

#  ifdef DEBUG_TRANSTAB
   for (i = 0; i < VG_TT_SIZE; i++)
      vg_assert(vg_tt[i].orig_addr != VG_TTE_DELETED);
#  endif
   VG_(sanity_check_tc_tt)();

   VGP_POPCC(VgpDoLRU);
}


/* Do a sanity check on TT/TC.
*/
void VG_(sanity_check_tc_tt) ( void )
{
   Int      i, counted_entries, counted_bytes;
   TTEntry* tte;
   counted_entries = 0;
   counted_bytes   = 0;
   for (i = 0; i < VG_TT_SIZE; i++) {
      tte = &vg_tt[i];
      if (tte->orig_addr == VG_TTE_EMPTY) continue;
      vg_assert(tte->mru_epoch >= 0);
      vg_assert(tte->mru_epoch <= VG_(current_epoch));
      counted_entries++;
      counted_bytes += 4+tte->trans_size;
      vg_assert(tte->trans_addr >= (Addr)&vg_tc[4]);
      vg_assert(tte->trans_addr < (Addr)&vg_tc[vg_tc_used]);
      vg_assert(VG_READ_MISALIGNED_WORD(tte->trans_addr-4) == i);
   }
   vg_assert(counted_entries == vg_tt_used);
   vg_assert(counted_bytes == vg_tc_used);
}


/* Add this already-filled-in entry to the TT.  Assumes that the
   relevant code chunk has been placed in TC, along with a dummy back
   pointer, which is inserted here.  
*/
extern void VG_(add_to_trans_tab) ( TTEntry* tte )
{
   Int i;
   /*
   VG_(printf)("add_to_trans_tab(%d) %x %d %x %d\n",
               vg_tt_used, tte->orig_addr, tte->orig_size, 
               tte->trans_addr, tte->trans_size);
   */
   vg_assert(tte->orig_addr != VG_TTE_DELETED 
             && tte->orig_addr != VG_TTE_EMPTY);
   /* Hash to get initial probe point. */
   i = ((UInt)(tte->orig_addr)) % VG_TT_SIZE;
   while (True) {
      if (vg_tt[i].orig_addr == tte->orig_addr)
         VG_(core_panic)("add_to_trans_tab: duplicate");
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY) {
         /* Put it here, and set the back pointer. */
         vg_tt[i] = *tte;
         VG_WRITE_MISALIGNED_WORD(tte->trans_addr-4, i);
         vg_tt_used++;
         return;
      }
      i++;
      if (i == VG_TT_SIZE) i = 0;
   }
}


/* Copy a new translation's code into TC, leaving a 4-byte hole for
   the back pointer, and returning a pointer to the code proper (not
   the hole) in TC. 
*/
Addr VG_(copy_to_transcache) ( Addr trans_addr, Int trans_size )
{
   Int i;
   Addr ret_addr;
   if (4+trans_size > VG_TC_SIZE-vg_tc_used)
      VG_(core_panic)("copy_to_transcache: not enough free space?!");
   /* Leave a hole for the back pointer to the TT entry. */
   vg_tc_used += 4;
   ret_addr = (Addr)&vg_tc[vg_tc_used];
   for (i = 0; i < trans_size; i++)
      vg_tc[vg_tc_used+i] = ((UChar*)trans_addr)[i];
   vg_tc_used += trans_size;
   return ret_addr;
}


/* Invalidate the tt_fast cache, for whatever reason.  Tricky.  We
   have to find a TTE_EMPTY slot to point all entries at. */
void VG_(invalidate_tt_fast)( void )
{
   Int i, j;
   for (i = 0; i < VG_TT_SIZE && vg_tt[i].orig_addr != VG_TTE_EMPTY; i++)
      ;
   vg_assert(i < VG_TT_SIZE 
             && vg_tt[i].orig_addr == VG_TTE_EMPTY);
   for (j = 0; j < VG_TT_FAST_SIZE; j++)
      VG_(tt_fast)[j] = (Addr)&vg_tt[i];
}


/* Search TT to find the translated address of the supplied original,
   or NULL if not found.  This routine is used when we miss in
   VG_(tt_fast). 
*/
static __inline__ TTEntry* search_trans_table ( Addr orig_addr )
{
   //static Int queries = 0;
   //static Int probes = 0;
   Int i;
   /* Hash to get initial probe point. */
   //   if (queries == 10000) {
   //  VG_(printf)("%d queries, %d probes\n", queries, probes);
   //  queries = probes = 0;
   //}
   //queries++;
   i = ((UInt)orig_addr) % VG_TT_SIZE;
   while (True) {
      //probes++;
      if (vg_tt[i].orig_addr == orig_addr)
         return &vg_tt[i];
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY)
         return NULL;
      i++;
      if (i == VG_TT_SIZE) i = 0;
   }
}


/* Find the translation address for a given (original) code address.
   If found, update VG_(tt_fast) so subsequent lookups are fast.  If
   no translation can be found, return zero.  This routine is (the
   only one) called from vg_run_innerloop.  */
Addr VG_(search_transtab) ( Addr original_addr )
{
   TTEntry* tte;
   VGP_PUSHCC(VgpSlowFindT);
   tte = search_trans_table ( original_addr );
   if (tte == NULL) {
      /* We didn't find it.  vg_run_innerloop will have to request a
         translation. */
      VGP_POPCC(VgpSlowFindT);
      return (Addr)0;
   } else {
      /* Found it.  Put the search result into the fast cache now.
         Also set the mru_epoch to mark this translation as used. */
      UInt cno = (UInt)original_addr & VG_TT_FAST_MASK;
      VG_(tt_fast)[cno] = (Addr)tte;
      VG_(tt_fast_misses)++;
      tte->mru_epoch = VG_(current_epoch);
      VGP_POPCC(VgpSlowFindT);
      return tte->trans_addr;
   }
}


/* Invalidate translations of original code [start .. start + range - 1].
   This is slow, so you *really* don't want to call it very often. 
*/
void VG_(invalidate_translations) ( Addr start, UInt range )
{
   Addr  i_start, i_end, o_start, o_end;
   UInt  out_count, out_osize, out_tsize;
   Int   i;

#  ifdef DEBUG_TRANSTAB
   VG_(sanity_check_tc_tt)();
#  endif
   i_start = start;
   i_end   = start + range - 1;
   out_count = out_osize = out_tsize = 0;

   for (i = 0; i < VG_TT_SIZE; i++) {
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY
          || vg_tt[i].orig_addr == VG_TTE_DELETED) continue;
      o_start = vg_tt[i].orig_addr;
      o_end = o_start + vg_tt[i].orig_size - 1;
      if (o_end < i_start || o_start > i_end)
         continue;

      if (VG_(needs).basic_block_discards)
         SK_(discard_basic_block_info)( vg_tt[i].orig_addr, 
                                         vg_tt[i].orig_size );

      vg_tt[i].orig_addr = VG_TTE_DELETED;
      VG_(this_epoch_out_count) ++;
      VG_(this_epoch_out_osize) += vg_tt[i].orig_size;
      VG_(this_epoch_out_tsize) += vg_tt[i].trans_size;
      VG_(overall_out_count) ++;
      VG_(overall_out_osize) += vg_tt[i].orig_size;
      VG_(overall_out_tsize) += vg_tt[i].trans_size;
      out_count ++;
      out_osize += vg_tt[i].orig_size;
      out_tsize += vg_tt[i].trans_size;
   }

   if (out_count > 0) {
      VG_(invalidate_tt_fast)();
      VG_(sanity_check_tc_tt)();
#     ifdef DEBUG_TRANSTAB
      { Addr aa;
        for (aa = i_start; aa <= i_end; aa++)
           vg_assert(search_trans_table ( aa ) == NULL);
      }
#     endif
   }

   if (1|| VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg,   
         "discard %d (%d -> %d) translations in range %p .. %p",
         out_count, out_osize, out_tsize, i_start, i_end );
}


/*------------------------------------------------------------*/
/*--- Initialisation.                                      ---*/
/*------------------------------------------------------------*/

void VG_(init_tt_tc) ( void )
{
   Int i;

   /* Allocate the translation table and translation cache. */
   vg_assert(vg_tc == NULL);
   vg_tc = VG_(get_memory_from_mmap) ( VG_TC_SIZE * sizeof(UChar), 
                                       "trans-cache" );
   vg_assert(vg_tc != NULL);

   vg_assert(vg_tt == NULL);
   vg_tt = VG_(get_memory_from_mmap) ( VG_TT_SIZE * sizeof(TTEntry),
                                       "trans-table" );
   vg_assert(vg_tt != NULL);

   /* The main translation table is empty. */
   vg_tt_used = 0;
   for (i = 0; i < VG_TT_SIZE; i++) {
      vg_tt[i].orig_addr = VG_TTE_EMPTY;
   }

   /* The translation table's fast cache is empty.  Point all entries
      at the first TT entry, which is, of course, empty. */
   for (i = 0; i < VG_TT_FAST_SIZE; i++)
      VG_(tt_fast)[i] = (Addr)(&vg_tt[0]);
}

/*--------------------------------------------------------------------*/
/*--- end                                            vg_transtab.c ---*/
/*--------------------------------------------------------------------*/
