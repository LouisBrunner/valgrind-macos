
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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"
#include "vg_constants.h"


/*------------------------------------------------------------*/
/*--- Management of the LRU-based translation table+cache. ---*/
/*------------------------------------------------------------*/

/* These sizes were set up so as to be able to debug large KDE 3
   applications (are there any small ones?) without excessive amounts
   of code retranslation.  */

/* Size of the translation cache, in bytes. */
#define VG_TC_SIZE /*16000000*/ 32000000 /*40000000*/

/* Do a LRU pass when the translation cache becomes this full. */
#define VG_TC_LIMIT_PERCENT 98

/* When doing an LRU pass, reduce TC fullness to this level. */
#define VG_TC_TARGET_PERCENT 85

/* Number of entries in the translation table.  This must be a prime
   number in order to make the hashing work properly. */
#define VG_TT_SIZE /*100129*/ 200191 /*250829*/

/* Do an LRU pass when the translation table becomes this full. */
#define VG_TT_LIMIT_PERCENT /*67*/ 80

/* When doing an LRU pass, reduce TT fullness to this level. */
#define VG_TT_TARGET_PERCENT /*60*/ 70

/* The number of age steps we track.  0 means the current epoch,
   N_EPOCHS-1 means used the epoch N_EPOCHS-1 or more ago.  */
#define VG_N_EPOCHS /*2000*/ /*4000*/ 20000

/* This TT entry is empty. */
#define VG_TTE_EMPTY   ((Addr)1)
/* This TT entry has been deleted. */
#define VG_TTE_DELETED ((Addr)3)

/* The TC.  This used to be statically allocated, but that forces many
   SecMap arrays to be pointlessly allocated at startup, bloating the
   process size by about 22M and making startup slow.  So now we
   dynamically allocate it at startup time.
   was: static UChar vg_tc[VG_TC_SIZE];
*/
static UChar* vg_tc = NULL;

/* Count of bytes used in the TC. */
static Int vg_tc_used = 0;

/* The TT.  Like TC, for the same reason, is dynamically allocated at
   startup. 
   was: static TTEntry vg_tt[VG_TT_SIZE];
*/
static TTEntry* vg_tt = NULL;

/* Count of non-empty, non-deleted TT entries. */
static Int vg_tt_used = 0;

/* Fast helper for the TT.  A direct-mapped cache which holds a
   pointer to a TT entry which may or may not be the correct one, but
   which we hope usually is.  This array is referred to directly from
   vg_dispatch.S. */
Addr VG_(tt_fast)[VG_TT_FAST_SIZE];

/* For reading/writing the misaligned TT-index word at immediately
   preceding every translation in TC. */
#define VG_READ_MISALIGNED_WORD(aaa) (*((UInt*)(aaa)))
#define VG_WRITE_MISALIGNED_WORD(aaa,vvv) *((UInt*)(aaa)) = ((UInt)(vvv))

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
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY || 
          vg_tt[i].orig_addr == VG_TTE_DELETED) continue;
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
      if (vg_tt[i].orig_addr == VG_TTE_EMPTY || 
         vg_tt[i].orig_addr == VG_TTE_DELETED) continue;
      if (vg_tt[i].mru_epoch <= thresh) {
         vg_tt[i].orig_addr = VG_TTE_DELETED;
         vg_tt_used--;
	 VG_(this_epoch_out_count) ++;
	 VG_(this_epoch_out_osize) += vg_tt[i].orig_size;
	 VG_(this_epoch_out_tsize) += vg_tt[i].trans_size;
	 VG_(overall_out_count) ++;
	 VG_(overall_out_osize) += vg_tt[i].orig_size;
	 VG_(overall_out_tsize) += vg_tt[i].trans_size;
      }
   }

   vg_assert(vg_tt_used >= 0);
   vg_assert(vg_tt_used <= tt_target);

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
      ttno = VG_READ_MISALIGNED_WORD(&vg_tc[r]);
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

   /* Reconstruct the SMC detection structures. */

   VGP_POPCC;
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
      if (tte->orig_addr == VG_TTE_DELETED) continue;
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
         VG_(panic)("add_to_trans_tab: duplicate");
      if (vg_tt[i].orig_addr == VG_TTE_DELETED ||
          vg_tt[i].orig_addr == VG_TTE_EMPTY) {
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
      VG_(panic)("copy_to_transcache: not enough free space?!");
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
      VGP_POPCC;
      return (Addr)0;
   } else {
      /* Found it.  Put the search result into the fast cache now.
         Also set the mru_epoch to mark this translation as used. */
      UInt cno = (UInt)original_addr & VG_TT_FAST_MASK;
      VG_(tt_fast)[cno] = (Addr)tte;
      VG_(tt_fast_misses)++;
      tte->mru_epoch = VG_(current_epoch);
      VGP_POPCC;
      return tte->trans_addr;
   }
}


/*------------------------------------------------------------*/
/*--- Detecting and handling self-modifying code.          ---*/
/*------------------------------------------------------------*/

/* This mechanism uses two data structures:

   vg_oldmap -- array[64k] of Bool, which approximately records
   parts of the address space corresponding to code for which
   a translation exists in the translation table.  vg_oldmap is
   consulted at each write, to determine whether that write might
   be writing a code address; if so, the program is stopped at 
   the next jump, and the corresponding translations are invalidated.

   Precise semantics: vg_oldmap[(a >> 8) & 0xFFFF] is true for all
   addresses a containing a code byte which has been translated.  So
   it acts kind-of like a direct-mapped cache with 64k entries.

   The second structure is vg_CAW, a small array of addresses at which
   vg_oldmap indicates a code write may have happened.  This is
   (effectively) checked at each control transfer (jump), so that
   translations can be discarded before going on.  An array is
   somewhat overkill, since it strikes me as very unlikely that a
   single basic block will do more than one code write.  Nevertheless
   ...  

   ToDo: make this comment up-to-date.
*/


/* Definitions for the self-modifying-code detection cache, intended
   as a fast check which clears the vast majority of writes.  */

#define VG_SMC_CACHE_HASH(aaa) \
   ((((UInt)a) >> VG_SMC_CACHE_SHIFT) & VG_SMC_CACHE_MASK)

Bool VG_(smc_cache)[VG_SMC_CACHE_SIZE];


/* Definitions for the fallback mechanism, which, more slowly,
   provides a precise record of which words in the address space
   belong to original code. */

typedef struct { UChar chars[2048]; } VgSmcSecondary;

static VgSmcSecondary* vg_smc_primary[65536];

static VgSmcSecondary* vg_smc_new_secondary ( void )
{
   Int i;
   VgSmcSecondary* sec 
      = VG_(malloc) ( VG_AR_PRIVATE, sizeof(VgSmcSecondary) );
   for (i = 0; i < 2048; i++)
      sec->chars[i] = 0;
   return sec;
}

#define GET_BIT_ARRAY(arr,indx)                      \
   (1 & (  ((UChar*)arr)[((UInt)indx) / 8]           \
           >> ( ((UInt)indx) % 8) ) )

#define SET_BIT_ARRAY(arr,indx)                      \
   ((UChar*)arr)[((UInt)indx) / 8] |= (1 << ((UInt)indx) % 8)


/* Finally, a place to record the original-code-write addresses
   detected in a basic block. */

#define VG_ORIGWRITES_SIZE 10

static Addr vg_origwrites[VG_ORIGWRITES_SIZE];
static Int  vg_origwrites_used;


/* Call here to check a written address. */

void VG_(smc_check4) ( Addr a )
{
   UInt bit_index;
   VgSmcSecondary* smc_secondary;

#  if VG_SMC_FASTCHECK_IN_C
   VG_(smc_total_check4s)++;

   /* Try the fast check first. */
   if (VG_(smc_cache)[VG_SMC_CACHE_HASH(a)] == False) return;
#  endif

   VG_(smc_cache_passed)++;

   /* Need to do a slow check. */
   smc_secondary = vg_smc_primary[a >> 16];
   if (smc_secondary == NULL) return;

   bit_index = (a & 0xFFFF) >> 2;
   if (GET_BIT_ARRAY(smc_secondary->chars, bit_index) == 0) return;

   VG_(smc_fancy_passed)++;

   /* Detected a Real Live write to code which has been translated.
      Note it. */
   if (vg_origwrites_used == VG_ORIGWRITES_SIZE)
      VG_(panic)("VG_ORIGWRITES_SIZE is too small; "
                 "increase and recompile.");
   vg_origwrites[vg_origwrites_used] = a;
   vg_origwrites_used++;

   VG_(message)(Vg_DebugMsg, "self-modifying-code write at %p", a);

   /* Force an exit before the next basic block, so the translation
      cache can be flushed appropriately. */
   //   VG_(dispatch_ctr_SAVED) = VG_(dispatch_ctr);
   //VG_(dispatch_ctr)       = 1;
   //VG_(interrupt_reason)   = VG_Y_SMC;
}


/* Mark an address range as containing an original translation,
   updating both the fast-check cache and the slow-but-correct data
   structure.  
*/
void VG_(smc_mark_original) ( Addr orig_addr, Int orig_size )
{
   Addr a;
   VgSmcSecondary* smc_secondary;
   UInt bit_index;

   for (a = orig_addr; a < orig_addr+orig_size; a++) {

      VG_(smc_cache)[VG_SMC_CACHE_HASH(a)] = True;

      smc_secondary = vg_smc_primary[a >> 16];
      if (smc_secondary == NULL)
         smc_secondary = 
         vg_smc_primary[a >> 16] = vg_smc_new_secondary();

      bit_index = (a & 0xFFFF) >> 2;
      SET_BIT_ARRAY(smc_secondary->chars, bit_index);      
   }
}


/* Discard any translations whose original code overlaps with the
   range w_addr .. w_addr+3 inclusive. 
*/
__attribute__ ((unused))
static void discard_translations_bracketing ( Addr w_addr )
{
#  if 0
   Int      i, rd, wr;
   Addr     o_start, o_end;
   TTEntry* tt;

   for (i = 0; i < VG_TRANSTAB_SLOW_SIZE; i++) {
      tt = vg_transtab[i];
      wr = 0;
      for (rd = 0; rd < vg_transtab_used[i]; rd++) {
         o_start = tt[rd].orig_addr;
         o_end   = o_start + tt[rd].orig_size;
         if (w_addr > o_end || (w_addr+3) < o_start) {
            /* No collision possible; keep this translation */
            VG_(smc_mark_original) ( tt[rd].orig_addr, tt[rd].orig_size );
            if (wr < rd) vg_transtab[wr] = vg_transtab[rd];
            wr++;
	 } else {
            /* Possible collision; discard. */
            vg_smc_discards++;
            VG_(message) (Vg_DebugMsg, 
                             "discarding translation of %p .. %p",
                             tt[rd].orig_addr, 
                             tt[rd].orig_addr + tt[rd].orig_size - 1);
            VG_(free)((void*)tt[rd].trans_addr);
         }         
      }
      vg_transtab_used[i] = wr;
   }
#  endif   
}


/* Top-level function in charge of discarding out-of-date translations
   following the discovery of a (potential) original-code-write. 
*/
void VG_(flush_transtab) ( void )
{
#  if 0
   Addr w_addr;
   Int  i, j;

   /* We shouldn't be here unless a code write was detected. */
   vg_assert(vg_origwrites_used > 0);

   /* Instead of incrementally fixing up the translation table cache,
      just invalidate the whole darn thing.  Pray this doesn't happen
      very often :) */
   for (i = 0; i < VG_TRANSTAB_CACHE_SIZE; i++)
      VG_(transtab_cache_orig)[i] = 
      VG_(transtab_cache_trans)[i] = (Addr)0;

   /* Clear out the fast cache; discard_translations_bracketing
      reconstructs it. */
   for (i = 0; i < VG_SMC_CACHE_SIZE; i++) 
      VG_(smc_cache)[i] = False;

   /* And also clear the slow-but-correct table. */
   for (i = 0; i < 65536; i++) {
      VgSmcSecondary* sec = vg_smc_primary[i];
      if (sec)
         for (j = 0; j < 2048; j++)
            sec->chars[j] = 0;         
   }

   /* This doesn't need to be particularly fast, since we (presumably)
      don't have to handle particularly frequent writes to code
      addresses. */
   while (vg_origwrites_used > 0) {
      vg_origwrites_used--;
      w_addr = vg_origwrites[vg_origwrites_used];
      discard_translations_bracketing ( w_addr );
   }

   vg_assert(vg_origwrites_used == 0);
#  endif
}


/*------------------------------------------------------------*/
/*--- Initialisation.                                      ---*/
/*------------------------------------------------------------*/

void VG_(init_transtab_and_SMC) ( void )
{
   Int i;

   /* Allocate the translation table and translation cache. */
   vg_assert(vg_tc == NULL);
   vg_tc = VG_(get_memory_from_mmap) ( VG_TC_SIZE * sizeof(UChar) );
   vg_assert(vg_tc != NULL);

   vg_assert(vg_tt == NULL);
   vg_tt = VG_(get_memory_from_mmap) ( VG_TT_SIZE * sizeof(TTEntry) );
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

   /* No part of the address space has any translations. */
   for (i = 0; i < 65536; i++)
      vg_smc_primary[i] = NULL;

   /* ... and the associated fast-check cache reflects this. */
   for (i = 0; i < VG_SMC_CACHE_SIZE; i++) 
      VG_(smc_cache)[i] = False;

   /* Finally, no original-code-writes have been recorded. */
   vg_origwrites_used = 0;
}

/*--------------------------------------------------------------------*/
/*--- end                                            vg_transtab.c ---*/
/*--------------------------------------------------------------------*/
