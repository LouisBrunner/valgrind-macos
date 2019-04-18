
//--------------------------------------------------------------------//
//--- DHAT: a Dynamic Heap Analysis Tool                 dh_main.c ---//
//--------------------------------------------------------------------//

/*
   This file is part of DHAT, a Valgrind tool for profiling the
   heap usage of programs.

   Copyright (C) 2010-2018 Mozilla Foundation

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

/* Contributed by Julian Seward <jseward@acm.org> */


#include "pub_tool_basics.h"
#include "pub_tool_clientstate.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcfile.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcproc.h"
#include "pub_tool_machine.h"      // VG_(fnptr_to_fnentry)
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_wordfm.h"

#define HISTOGRAM_SIZE_LIMIT 1024

//------------------------------------------------------------//
//--- Globals                                              ---//
//------------------------------------------------------------//

// Values for the entire run.
static ULong g_total_blocks = 0;
static ULong g_total_bytes  = 0;

// Current values.
static ULong g_curr_blocks = 0;
static ULong g_curr_bytes  = 0;
static ULong g_curr_instrs = 0;  // incremented from generated code

// Values at the global max, i.e. when g_curr_bytes peaks.
static ULong g_max_blocks = 0;
static ULong g_max_bytes  = 0;
static ULong g_max_instrs = 0;

// Values for the entire run. Computed at the end.
static ULong g_reads_bytes = 0;
static ULong g_writes_bytes = 0;

//------------------------------------------------------------//
//--- an Interval Tree of live blocks                      ---//
//------------------------------------------------------------//

/* Tracks information about live blocks. */
typedef
   struct {
      Addr        payload;
      SizeT       req_szB;
      ExeContext* ap;  /* allocation ec */
      ULong       allocd_at; /* instruction number */
      ULong       reads_bytes;
      ULong       writes_bytes;
      /* Approx histogram, one byte per payload byte.  Counts latch up
         therefore at 0xFFFF.  Can be NULL if the block is resized or if
         the block is larger than HISTOGRAM_SIZE_LIMIT. */
      UShort*     histoW; /* [0 .. req_szB-1] */
   }
   Block;

/* May not contain zero-sized blocks.  May not contain
   overlapping blocks. */
static WordFM* interval_tree = NULL;  /* WordFM* Block* void */

/* Here's the comparison function.  Since the tree is required
to contain non-zero sized, non-overlapping blocks, it's good
enough to consider any overlap as a match. */
static Word interval_tree_Cmp ( UWord k1, UWord k2 )
{
   Block* b1 = (Block*)k1;
   Block* b2 = (Block*)k2;
   tl_assert(b1->req_szB > 0);
   tl_assert(b2->req_szB > 0);
   if (b1->payload + b1->req_szB <= b2->payload) return -1;
   if (b2->payload + b2->req_szB <= b1->payload) return  1;
   return 0;
}

// 2-entry cache for find_Block_containing
static Block* fbc_cache0 = NULL;
static Block* fbc_cache1 = NULL;

static UWord stats__n_fBc_cached = 0;
static UWord stats__n_fBc_uncached = 0;
static UWord stats__n_fBc_notfound = 0;

static Block* find_Block_containing ( Addr a )
{
   if (LIKELY(fbc_cache0
              && fbc_cache0->payload <= a
              && a < fbc_cache0->payload + fbc_cache0->req_szB)) {
      // found at 0
      stats__n_fBc_cached++;
      return fbc_cache0;
   }
   if (LIKELY(fbc_cache1
              && fbc_cache1->payload <= a
              && a < fbc_cache1->payload + fbc_cache1->req_szB)) {
      // found at 1; swap 0 and 1
      Block* tmp = fbc_cache0;
      fbc_cache0 = fbc_cache1;
      fbc_cache1 = tmp;
      stats__n_fBc_cached++;
      return fbc_cache0;
   }
   Block fake;
   fake.payload = a;
   fake.req_szB = 1;
   UWord foundkey = 1;
   UWord foundval = 1;
   Bool found = VG_(lookupFM)( interval_tree,
                               &foundkey, &foundval, (UWord)&fake );
   if (!found) {
      stats__n_fBc_notfound++;
      return NULL;
   }
   tl_assert(foundval == 0); // we don't store vals in the interval tree
   tl_assert(foundkey != 1);
   Block* res = (Block*)foundkey;
   tl_assert(res != &fake);
   // put at the top position
   fbc_cache1 = fbc_cache0;
   fbc_cache0 = res;
   stats__n_fBc_uncached++;
   return res;
}

// delete a block; asserts if not found.  (viz, 'a' must be
// known to be present.)
static void delete_Block_starting_at ( Addr a )
{
   Block fake;
   fake.payload = a;
   fake.req_szB = 1;
   Bool found = VG_(delFromFM)( interval_tree,
                                NULL, NULL, (Addr)&fake );
   tl_assert(found);
   fbc_cache0 = fbc_cache1 = NULL;
}


//------------------------------------------------------------//
//--- a FM of allocation points (APs)                      ---//
//------------------------------------------------------------//

typedef
   struct {
      // The allocation point that we're summarising stats for.
      ExeContext* ap;

      // Total number of blocks and bytes allocated by this AP.
      ULong total_blocks;
      ULong total_bytes;

      // The current number of blocks and bytes live for this AP.
      ULong curr_blocks;
      ULong curr_bytes;

      // Values at the AP max, i.e. when this AP's curr_bytes peaks.
      ULong max_blocks;     // Blocks at the AP max.
      ULong max_bytes;      // The AP max, measured in bytes.

      // Values at the global max.
      ULong at_tgmax_blocks;
      ULong at_tgmax_bytes;

      // Total lifetimes of all blocks allocated by this AP. Includes blocks
      // explicitly freed and blocks implicitly freed at termination.
      ULong total_lifetimes_instrs;

      // Number of blocks freed by this AP. (Only used in assertions.)
      ULong freed_blocks;

      // Total number of reads and writes in all blocks allocated
      // by this AP.
      ULong reads_bytes;
      ULong writes_bytes;

      /* Histogram information.  We maintain a histogram aggregated for
         all retiring Blocks allocated by this AP, but only if:
         - this AP has only ever allocated objects of one size
         - that size is <= HISTOGRAM_SIZE_LIMIT
         What we need therefore is a mechanism to see if this AP
         has only ever allocated blocks of one size.

         3 states:
            Unknown          because no retirement yet
            Exactly xsize    all retiring blocks are of this size
            Mixed            multiple different sizes seen
      */
      enum { Unknown=999, Exactly, Mixed } xsize_tag;
      SizeT xsize;
      UInt* histo; /* [0 .. xsize-1] */
   }
   APInfo;

/* maps ExeContext*'s to APInfo*'s.  Note that the keys must match the
   .ap field in the values. */
static WordFM* apinfo = NULL;  /* WordFM* ExeContext* APInfo* */


// Are we at peak memory? If so, update at_tgmax_blocks and at_tgmax_bytes in
// all APInfos. Note that this is moderately expensive so we avoid calling it
// on every allocation.
static void check_for_peak(void)
{
   if (g_curr_bytes == g_max_bytes) {
      // It's a peak. (If there are multiple equal peaks we record the latest
      // one.)
      UWord keyW, valW;
      VG_(initIterFM)(apinfo);
      while (VG_(nextIterFM)(apinfo, &keyW, &valW)) {
         APInfo* api = (APInfo*)valW;
         tl_assert(api && api->ap == (ExeContext*)keyW);
         api->at_tgmax_blocks = api->curr_blocks;
         api->at_tgmax_bytes = api->curr_bytes;
      }
      VG_(doneIterFM)(apinfo);
   }
}

/* 'bk' is being introduced (has just been allocated).  Find the
   relevant APInfo entry for it, or create one, based on the block's
   allocation EC.  Then, update the APInfo to the extent that we
   actually can, to reflect the allocation. */
static void intro_Block ( Block* bk )
{
   tl_assert(bk);
   tl_assert(bk->ap);

   APInfo* api   = NULL;
   UWord   keyW  = 0;
   UWord   valW  = 0;
   Bool    found = VG_(lookupFM)( apinfo,
                                  &keyW, &valW, (UWord)bk->ap );
   if (found) {
      api = (APInfo*)valW;
      tl_assert(keyW == (UWord)bk->ap);
   } else {
      api = VG_(malloc)( "dh.intro_Block.1", sizeof(APInfo) );
      VG_(memset)(api, 0, sizeof(*api));
      api->ap = bk->ap;
      Bool present = VG_(addToFM)( apinfo,
                                   (UWord)bk->ap, (UWord)api );
      tl_assert(!present);
      // histo stuff
      tl_assert(api->freed_blocks == 0);
      api->xsize_tag = Unknown;
      api->xsize = 0;
      if (0) VG_(printf)("api %p   -->  Unknown\n", api);
   }

   tl_assert(api->ap == bk->ap);

   // Update global stats first.

   g_total_blocks++;
   g_total_bytes += bk->req_szB;

   g_curr_blocks++;
   g_curr_bytes += bk->req_szB;
   if (g_curr_bytes > g_max_bytes) {
      g_max_blocks = g_curr_blocks;
      g_max_bytes = g_curr_bytes;
      g_max_instrs = g_curr_instrs;
   }

   // Now update APInfo stats.

   api->total_blocks++;
   api->total_bytes += bk->req_szB;

   api->curr_blocks++;
   api->curr_bytes += bk->req_szB;
   if (api->curr_bytes > api->max_bytes) {
      api->max_blocks = api->curr_blocks;
      api->max_bytes  = api->curr_bytes;
   }
}

/* 'bk' is retiring (being freed).  Find the relevant APInfo entry for
   it, which must already exist.  Then, fold info from 'bk' into that
   entry.  'because_freed' is True if the block is retiring because
   the client has freed it.  If it is False then the block is retiring
   because the program has finished, in which case we want to skip the
   updates of the total blocks live etc for this AP, but still fold in
   the access counts and histo data that have so far accumulated for
   the block. */
static void retire_Block ( Block* bk, Bool because_freed )
{
   tl_assert(bk);
   tl_assert(bk->ap);

   APInfo* api   = NULL;
   UWord   keyW  = 0;
   UWord   valW  = 0;
   Bool    found = VG_(lookupFM)( apinfo,
                                  &keyW, &valW, (UWord)bk->ap );

   tl_assert(found);
   api = (APInfo*)valW;
   tl_assert(api->ap == bk->ap);

   // update stats following this free.
   if (0)
      VG_(printf)("ec %p  api->c_by_l %llu  bk->rszB %llu\n",
                  bk->ap, api->curr_bytes, (ULong)bk->req_szB);

   if (because_freed) {
      // Total bytes is coming down from a possible peak.
      check_for_peak();

      // Then update global stats.
      tl_assert(g_curr_blocks >= 1);
      tl_assert(g_curr_bytes >= bk->req_szB);
      g_curr_blocks--;
      g_curr_bytes -= bk->req_szB;

      // Then update APInfo stats.
      tl_assert(api->curr_blocks >= 1);
      tl_assert(api->curr_bytes >= bk->req_szB);
      api->curr_blocks--;
      api->curr_bytes -= bk->req_szB;

      api->freed_blocks++;
   }

   tl_assert(bk->allocd_at <= g_curr_instrs);
   api->total_lifetimes_instrs += (g_curr_instrs - bk->allocd_at);

   // access counts
   api->reads_bytes += bk->reads_bytes;
   api->writes_bytes += bk->writes_bytes;
   g_reads_bytes += bk->reads_bytes;
   g_writes_bytes += bk->writes_bytes;

   // histo stuff.  First, do state transitions for xsize/xsize_tag.
   switch (api->xsize_tag) {

      case Unknown:
         tl_assert(api->xsize == 0);
         tl_assert(api->freed_blocks == 1 || api->freed_blocks == 0);
         tl_assert(!api->histo);
         api->xsize_tag = Exactly;
         api->xsize = bk->req_szB;
         if (0) VG_(printf)("api %p   -->  Exactly(%lu)\n", api, api->xsize);
         // and allocate the histo
         if (bk->histoW) {
            api->histo = VG_(malloc)("dh.retire_Block.1",
                                     api->xsize * sizeof(UInt));
            VG_(memset)(api->histo, 0, api->xsize * sizeof(UInt));
         }
         break;

      case Exactly:
         //tl_assert(api->freed_blocks > 1);
         if (bk->req_szB != api->xsize) {
            if (0) VG_(printf)("api %p   -->  Mixed(%lu -> %lu)\n",
                               api, api->xsize, bk->req_szB);
            api->xsize_tag = Mixed;
            api->xsize = 0;
            // deallocate the histo, if any
            if (api->histo) {
               VG_(free)(api->histo);
               api->histo = NULL;
            }
         }
         break;

      case Mixed:
         //tl_assert(api->freed_blocks > 1);
         break;

      default:
        tl_assert(0);
   }

   // See if we can fold the histo data from this block into
   // the data for the AP
   if (api->xsize_tag == Exactly && api->histo && bk->histoW) {
      tl_assert(api->xsize == bk->req_szB);
      UWord i;
      for (i = 0; i < api->xsize; i++) {
         // FIXME: do something better in case of overflow of api->histo[..]
         // Right now, at least don't let it overflow/wrap around
         if (api->histo[i] <= 0xFFFE0000)
            api->histo[i] += (UInt)bk->histoW[i];
      }
      if (0) VG_(printf)("fold in, AP = %p\n", api);
   }

#if 0
   if (bk->histoB) {
      VG_(printf)("block retiring, histo %lu: ", bk->req_szB);
      UWord i;
      for (i = 0; i < bk->req_szB; i++)
        VG_(printf)("%u ", (UInt)bk->histoB[i]);
      VG_(printf)("\n");
   } else {
      VG_(printf)("block retiring, no histo %lu\n", bk->req_szB);
   }
#endif
}

/* This handles block resizing.  When a block with AP 'ec' has a
   size change of 'delta', call here to update the APInfo. */
static void resize_Block(ExeContext* ec, SizeT old_req_szB, SizeT new_req_szB)
{
   Long    delta = (Long)new_req_szB - (Long)old_req_szB;
   APInfo* api   = NULL;
   UWord   keyW  = 0;
   UWord   valW  = 0;
   Bool    found = VG_(lookupFM)( apinfo,
                                  &keyW, &valW, (UWord)ec );

   tl_assert(found);
   api = (APInfo*)valW;
   tl_assert(api->ap == ec);

   if (delta < 0) {
      tl_assert(api->curr_bytes >= -delta);
      tl_assert(g_curr_bytes >= -delta);
   }

   // Total bytes might be coming down from a possible peak.
   if (delta < 0)
      check_for_peak();

   // Note: we treat realloc() like malloc() + free() for total counts, i.e. we
   // increment total_blocks by 1 and increment total_bytes by new_req_szB.
   //
   // A reasonable alternative would be to leave total_blocks unchanged and
   // increment total_bytes by delta (but only if delta is positive). But then
   // calls to realloc wouldn't be counted towards the total_blocks count,
   // which is undesirable.

   // Update global stats first.

   g_total_blocks++;
   g_total_bytes += new_req_szB;

   g_curr_blocks += 0;  // unchanged
   g_curr_bytes += delta;
   if (g_curr_bytes > g_max_bytes) {
      g_max_blocks = g_curr_blocks;
      g_max_bytes = g_curr_bytes;
      g_max_instrs = g_curr_instrs;
   }

   // Now update APInfo stats.

   api->total_blocks++;
   api->total_bytes += new_req_szB;

   api->curr_blocks += 0;  // unchanged
   api->curr_bytes += delta;
   if (api->curr_bytes > api->max_bytes) {
      api->max_blocks = api->curr_blocks;
      api->max_bytes  = api->curr_bytes;
   }
}


//------------------------------------------------------------//
//--- update both Block and APInfos after {m,re}alloc/free ---//
//------------------------------------------------------------//

static
void* new_block ( ThreadId tid, void* p, SizeT req_szB, SizeT req_alignB,
                  Bool is_zeroed )
{
   tl_assert(p == NULL); // don't handle custom allocators right now
   SizeT actual_szB /*, slop_szB*/;

   if ((SSizeT)req_szB < 0) return NULL;

   if (req_szB == 0)
      req_szB = 1;  /* can't allow zero-sized blocks in the interval tree */

   // Allocate and zero if necessary
   if (!p) {
      p = VG_(cli_malloc)( req_alignB, req_szB );
      if (!p) {
         return NULL;
      }
      if (is_zeroed) VG_(memset)(p, 0, req_szB);
      actual_szB = VG_(cli_malloc_usable_size)(p);
      tl_assert(actual_szB >= req_szB);
      /* slop_szB = actual_szB - req_szB; */
   } else {
      /* slop_szB = 0; */
   }

   // Make new Block, add to interval_tree.
   Block* bk = VG_(malloc)("dh.new_block.1", sizeof(Block));
   bk->payload      = (Addr)p;
   bk->req_szB      = req_szB;
   bk->ap           = VG_(record_ExeContext)(tid, 0/*first word delta*/);
   bk->allocd_at    = g_curr_instrs;
   bk->reads_bytes  = 0;
   bk->writes_bytes = 0;
   // set up histogram array, if the block isn't too large
   bk->histoW = NULL;
   if (req_szB <= HISTOGRAM_SIZE_LIMIT) {
      bk->histoW = VG_(malloc)("dh.new_block.2", req_szB * sizeof(UShort));
      VG_(memset)(bk->histoW, 0, req_szB * sizeof(UShort));
   }

   Bool present = VG_(addToFM)( interval_tree, (UWord)bk, (UWord)0/*no val*/);
   tl_assert(!present);
   fbc_cache0 = fbc_cache1 = NULL;

   intro_Block(bk);

   if (0) VG_(printf)("ALLOC %lu -> %p\n", req_szB, p);

   return p;
}

static
void die_block ( void* p, Bool custom_free )
{
   tl_assert(!custom_free);  // at least for now

   Block* bk = find_Block_containing( (Addr)p );

   if (!bk) {
     return; // bogus free
   }

   tl_assert(bk->req_szB > 0);
   // assert the block finder is behaving sanely
   tl_assert(bk->payload <= (Addr)p);
   tl_assert( (Addr)p < bk->payload + bk->req_szB );

   if (bk->payload != (Addr)p) {
      return; // bogus free
   }

   if (0) VG_(printf)(" FREE %p %llu\n",
                      p, g_curr_instrs - bk->allocd_at);

   retire_Block(bk, True/*because_freed*/);

   VG_(cli_free)( (void*)bk->payload );
   delete_Block_starting_at( bk->payload );
   if (bk->histoW) {
      VG_(free)( bk->histoW );
      bk->histoW = NULL;
   }
   VG_(free)( bk );
}


static
void* renew_block ( ThreadId tid, void* p_old, SizeT new_req_szB )
{
   if (0) VG_(printf)("REALL %p %lu\n", p_old, new_req_szB);
   void* p_new = NULL;

   tl_assert(new_req_szB > 0); // map 0 to 1

   // Find the old block.
   Block* bk = find_Block_containing( (Addr)p_old );
   if (!bk) {
      return NULL;   // bogus realloc
   }

   tl_assert(bk->req_szB > 0);
   // assert the block finder is behaving sanely
   tl_assert(bk->payload <= (Addr)p_old);
   tl_assert( (Addr)p_old < bk->payload + bk->req_szB );

   if (bk->payload != (Addr)p_old) {
      return NULL; // bogus realloc
   }

   // Keeping the histogram alive in any meaningful way across
   // block resizing is too darn complicated.  Just throw it away.
   if (bk->histoW) {
      VG_(free)(bk->histoW);
      bk->histoW = NULL;
   }

   // Actually do the allocation, if necessary.
   if (new_req_szB <= bk->req_szB) {

      // New size is smaller or same; block not moved.
      resize_Block(bk->ap, bk->req_szB, new_req_szB);
      bk->req_szB = new_req_szB;
      return p_old;

   } else {

      // New size is bigger;  make new block, copy shared contents, free old.
      p_new = VG_(cli_malloc)(VG_(clo_alignment), new_req_szB);
      if (!p_new) {
         // Nb: if realloc fails, NULL is returned but the old block is not
         // touched.  What an awful function.
         return NULL;
      }
      tl_assert(p_new != p_old);

      VG_(memcpy)(p_new, p_old, bk->req_szB);
      VG_(cli_free)(p_old);

      // Since the block has moved, we need to re-insert it into the
      // interval tree at the new place.  Do this by removing
      // and re-adding it.
      delete_Block_starting_at( (Addr)p_old );
      // now 'bk' is no longer in the tree, but the Block itself
      // is still alive

      // Update the metadata.
      resize_Block(bk->ap, bk->req_szB, new_req_szB);
      bk->payload = (Addr)p_new;
      bk->req_szB = new_req_szB;

      // and re-add
      Bool present
         = VG_(addToFM)( interval_tree, (UWord)bk, (UWord)0/*no val*/);
      tl_assert(!present);
      fbc_cache0 = fbc_cache1 = NULL;

      return p_new;
   }
   /*NOTREACHED*/
   tl_assert(0);
}


//------------------------------------------------------------//
//--- malloc() et al replacement wrappers                  ---//
//------------------------------------------------------------//

static void* dh_malloc ( ThreadId tid, SizeT szB )
{
   return new_block( tid, NULL, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* dh___builtin_new ( ThreadId tid, SizeT szB )
{
   return new_block( tid, NULL, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* dh___builtin_vec_new ( ThreadId tid, SizeT szB )
{
   return new_block( tid, NULL, szB, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* dh_calloc ( ThreadId tid, SizeT m, SizeT szB )
{
   return new_block( tid, NULL, m*szB, VG_(clo_alignment), /*is_zeroed*/True );
}

static void *dh_memalign ( ThreadId tid, SizeT alignB, SizeT szB )
{
   return new_block( tid, NULL, szB, alignB, False );
}

static void dh_free ( ThreadId tid __attribute__((unused)), void* p )
{
   die_block( p, /*custom_free*/False );
}

static void dh___builtin_delete ( ThreadId tid, void* p )
{
   die_block( p, /*custom_free*/False);
}

static void dh___builtin_vec_delete ( ThreadId tid, void* p )
{
   die_block( p, /*custom_free*/False );
}

static void* dh_realloc ( ThreadId tid, void* p_old, SizeT new_szB )
{
   if (p_old == NULL) {
      return dh_malloc(tid, new_szB);
   }
   if (new_szB == 0) {
      dh_free(tid, p_old);
      return NULL;
   }
   return renew_block(tid, p_old, new_szB);
}

static SizeT dh_malloc_usable_size ( ThreadId tid, void* p )
{
   Block* bk = find_Block_containing( (Addr)p );
   return bk ? bk->req_szB : 0;
}


//------------------------------------------------------------//
//--- memory references                                    ---//
//------------------------------------------------------------//

static
void inc_histo_for_block ( Block* bk, Addr addr, UWord szB )
{
   UWord i, offMin, offMax1;
   offMin = addr - bk->payload;
   tl_assert(offMin < bk->req_szB);
   offMax1 = offMin + szB;
   if (offMax1 > bk->req_szB)
      offMax1 = bk->req_szB;
   //VG_(printf)("%lu %lu   (size of block %lu)\n", offMin, offMax1, bk->req_szB);
   for (i = offMin; i < offMax1; i++) {
      UShort n = bk->histoW[i];
      if (n < 0xFFFF) n++;
      bk->histoW[i] = n;
   }
}

static VG_REGPARM(2)
void dh_handle_write ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      bk->writes_bytes += szB;
      if (bk->histoW)
         inc_histo_for_block(bk, addr, szB);
   }
}

static VG_REGPARM(2)
void dh_handle_read ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      bk->reads_bytes += szB;
      if (bk->histoW)
         inc_histo_for_block(bk, addr, szB);
   }
}


// Handle reads and writes by syscalls (read == kernel
// reads user space, write == kernel writes user space).
// Assumes no such read or write spans a heap block
// boundary and so we can treat it just as one giant
// read or write.
static
void dh_handle_noninsn_read ( CorePart part, ThreadId tid, const HChar* s,
                              Addr base, SizeT size )
{
   switch (part) {
      case Vg_CoreSysCall:
         dh_handle_read(base, size);
         break;
      case Vg_CoreSysCallArgInMem:
         break;
      case Vg_CoreTranslate:
         break;
      default:
         tl_assert(0);
   }
}

static
void dh_handle_noninsn_write ( CorePart part, ThreadId tid,
                               Addr base, SizeT size )
{
   switch (part) {
      case Vg_CoreSysCall:
      case Vg_CoreClientReq:
         dh_handle_write(base, size);
         break;
      case Vg_CoreSignal:
         break;
      default:
         tl_assert(0);
   }
}


//------------------------------------------------------------//
//--- Instrumentation                                      ---//
//------------------------------------------------------------//

#define binop(_op, _arg1, _arg2) IRExpr_Binop((_op),(_arg1),(_arg2))
#define mkexpr(_tmp)             IRExpr_RdTmp((_tmp))
#define mkU32(_n)                IRExpr_Const(IRConst_U32(_n))
#define mkU64(_n)                IRExpr_Const(IRConst_U64(_n))
#define assign(_t, _e)           IRStmt_WrTmp((_t), (_e))

static
void add_counter_update(IRSB* sbOut, Int n)
{
   #if defined(VG_BIGENDIAN)
   # define END Iend_BE
   #elif defined(VG_LITTLEENDIAN)
   # define END Iend_LE
   #else
   # error "Unknown endianness"
   #endif
   // Add code to increment 'g_curr_instrs' by 'n', like this:
   //   WrTmp(t1, Load64(&g_curr_instrs))
   //   WrTmp(t2, Add64(RdTmp(t1), Const(n)))
   //   Store(&g_curr_instrs, t2)
   IRTemp t1 = newIRTemp(sbOut->tyenv, Ity_I64);
   IRTemp t2 = newIRTemp(sbOut->tyenv, Ity_I64);
   IRExpr* counter_addr = mkIRExpr_HWord( (HWord)&g_curr_instrs );

   IRStmt* st1 = assign(t1, IRExpr_Load(END, Ity_I64, counter_addr));
   IRStmt* st2 = assign(t2, binop(Iop_Add64, mkexpr(t1), mkU64(n)));
   IRStmt* st3 = IRStmt_Store(END, counter_addr, mkexpr(t2));

   addStmtToIRSB( sbOut, st1 );
   addStmtToIRSB( sbOut, st2 );
   addStmtToIRSB( sbOut, st3 );
}

static
void addMemEvent(IRSB* sbOut, Bool isWrite, Int szB, IRExpr* addr,
                 Int goff_sp)
{
   IRType   tyAddr   = Ity_INVALID;
   const HChar* hName= NULL;
   void*    hAddr    = NULL;
   IRExpr** argv     = NULL;
   IRDirty* di       = NULL;

   const Int THRESH = 4096 * 4; // somewhat arbitrary
   const Int rz_szB = VG_STACK_REDZONE_SZB;

   tyAddr = typeOfIRExpr( sbOut->tyenv, addr );
   tl_assert(tyAddr == Ity_I32 || tyAddr == Ity_I64);

   if (isWrite) {
      hName = "dh_handle_write";
      hAddr = &dh_handle_write;
   } else {
      hName = "dh_handle_read";
      hAddr = &dh_handle_read;
   }

   argv = mkIRExprVec_2( addr, mkIRExpr_HWord(szB) );

   /* Add the helper. */
   tl_assert(hName);
   tl_assert(hAddr);
   tl_assert(argv);
   di = unsafeIRDirty_0_N( 2/*regparms*/,
                           hName, VG_(fnptr_to_fnentry)( hAddr ),
                           argv );

   /* Generate the guard condition: "(addr - (SP - RZ)) >u N", for
      some arbitrary N.  If that fails then addr is in the range (SP -
      RZ .. SP + N - RZ).  If N is smallish (a page?) then we can say
      addr is within a page of SP and so can't possibly be a heap
      access, and so can be skipped. */
   IRTemp sp = newIRTemp(sbOut->tyenv, tyAddr);
   addStmtToIRSB( sbOut, assign(sp, IRExpr_Get(goff_sp, tyAddr)));

   IRTemp sp_minus_rz = newIRTemp(sbOut->tyenv, tyAddr);
   addStmtToIRSB(
      sbOut,
      assign(sp_minus_rz,
             tyAddr == Ity_I32
                ? binop(Iop_Sub32, mkexpr(sp), mkU32(rz_szB))
                : binop(Iop_Sub64, mkexpr(sp), mkU64(rz_szB)))
   );

   IRTemp diff = newIRTemp(sbOut->tyenv, tyAddr);
   addStmtToIRSB(
      sbOut,
      assign(diff,
             tyAddr == Ity_I32
                ? binop(Iop_Sub32, addr, mkexpr(sp_minus_rz))
                : binop(Iop_Sub64, addr, mkexpr(sp_minus_rz)))
   );

   IRTemp guard = newIRTemp(sbOut->tyenv, Ity_I1);
   addStmtToIRSB(
      sbOut,
      assign(guard,
             tyAddr == Ity_I32
                ? binop(Iop_CmpLT32U, mkU32(THRESH), mkexpr(diff))
                : binop(Iop_CmpLT64U, mkU64(THRESH), mkexpr(diff)))
   );
   di->guard = mkexpr(guard);

   addStmtToIRSB( sbOut, IRStmt_Dirty(di) );
}

static
IRSB* dh_instrument ( VgCallbackClosure* closure,
                      IRSB* sbIn,
                      const VexGuestLayout* layout,
                      const VexGuestExtents* vge,
                      const VexArchInfo* archinfo_host,
                      IRType gWordTy, IRType hWordTy )
{
   Int   i, n = 0;
   IRSB* sbOut;
   IRTypeEnv* tyenv = sbIn->tyenv;

   const Int goff_sp = layout->offset_SP;

   // We increment the instruction count in two places:
   // - just before any Ist_Exit statements;
   // - just before the IRSB's end.
   // In the former case, we zero 'n' and then continue instrumenting.

   sbOut = deepCopyIRSBExceptStmts(sbIn);

   // Copy verbatim any IR preamble preceding the first IMark
   i = 0;
   while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
      addStmtToIRSB( sbOut, sbIn->stmts[i] );
      i++;
   }

   for (/*use current i*/; i < sbIn->stmts_used; i++) {
      IRStmt* st = sbIn->stmts[i];

      if (!st || st->tag == Ist_NoOp) continue;

      switch (st->tag) {

         case Ist_IMark: {
            n++;
            break;
         }

         case Ist_Exit: {
            if (n > 0) {
               // Add an increment before the Exit statement, then reset 'n'.
               add_counter_update(sbOut, n);
               n = 0;
            }
            break;
         }

         case Ist_WrTmp: {
            IRExpr* data = st->Ist.WrTmp.data;
            if (data->tag == Iex_Load) {
               IRExpr* aexpr = data->Iex.Load.addr;
               // Note also, endianness info is ignored.  I guess
               // that's not interesting.
               addMemEvent( sbOut, False/*!isWrite*/,
                            sizeofIRType(data->Iex.Load.ty),
                            aexpr, goff_sp );
            }
            break;
         }

         case Ist_Store: {
            IRExpr* data  = st->Ist.Store.data;
            IRExpr* aexpr = st->Ist.Store.addr;
            addMemEvent( sbOut, True/*isWrite*/,
                         sizeofIRType(typeOfIRExpr(tyenv, data)),
                         aexpr, goff_sp );
            break;
         }

         case Ist_Dirty: {
            Int      dataSize;
            IRDirty* d = st->Ist.Dirty.details;
            if (d->mFx != Ifx_None) {
               /* This dirty helper accesses memory.  Collect the details. */
               tl_assert(d->mAddr != NULL);
               tl_assert(d->mSize != 0);
               dataSize = d->mSize;
               // Large (eg. 28B, 108B, 512B on x86) data-sized
               // instructions will be done inaccurately, but they're
               // very rare and this avoids errors from hitting more
               // than two cache lines in the simulation.
               if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify)
                  addMemEvent( sbOut, False/*!isWrite*/,
                               dataSize, d->mAddr, goff_sp );
               if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify)
                  addMemEvent( sbOut, True/*isWrite*/,
                               dataSize, d->mAddr, goff_sp );
            } else {
               tl_assert(d->mAddr == NULL);
               tl_assert(d->mSize == 0);
            }
            break;
         }

         case Ist_CAS: {
            /* We treat it as a read and a write of the location.  I
               think that is the same behaviour as it was before IRCAS
               was introduced, since prior to that point, the Vex
               front ends would translate a lock-prefixed instruction
               into a (normal) read followed by a (normal) write. */
            Int    dataSize;
            IRCAS* cas = st->Ist.CAS.details;
            tl_assert(cas->addr != NULL);
            tl_assert(cas->dataLo != NULL);
            dataSize = sizeofIRType(typeOfIRExpr(tyenv, cas->dataLo));
            if (cas->dataHi != NULL)
               dataSize *= 2; /* since it's a doubleword-CAS */
            addMemEvent( sbOut, False/*!isWrite*/,
                         dataSize, cas->addr, goff_sp );
            addMemEvent( sbOut, True/*isWrite*/,
                         dataSize, cas->addr, goff_sp );
            break;
         }

         case Ist_LLSC: {
            IRType dataTy;
            if (st->Ist.LLSC.storedata == NULL) {
               /* LL */
               dataTy = typeOfIRTemp(tyenv, st->Ist.LLSC.result);
               addMemEvent( sbOut, False/*!isWrite*/,
                            sizeofIRType(dataTy),
                            st->Ist.LLSC.addr, goff_sp );
            } else {
               /* SC */
               dataTy = typeOfIRExpr(tyenv, st->Ist.LLSC.storedata);
               addMemEvent( sbOut, True/*isWrite*/,
                            sizeofIRType(dataTy),
                            st->Ist.LLSC.addr, goff_sp );
            }
            break;
         }

         default:
            break;
      }

      addStmtToIRSB( sbOut, st );
   }

   if (n > 0) {
      // Add an increment before the SB end.
      add_counter_update(sbOut, n);
   }
   return sbOut;
}

#undef binop
#undef mkexpr
#undef mkU32
#undef mkU64
#undef assign


//------------------------------------------------------------//
//--- Command line args                                    ---//
//------------------------------------------------------------//

static const HChar* clo_dhat_out_file = "dhat.out.%p";

static Bool dh_process_cmd_line_option(const HChar* arg)
{
   if VG_STR_CLO(arg, "--dhat-out-file", clo_dhat_out_file) {}

   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;
}

static void dh_print_usage(void)
{
   VG_(printf)(
"    --dhat-out-file=<file>  output file name [dhat.out.%%p]\n"
   );
}

static void dh_print_debug_usage(void)
{
   VG_(printf)(
"    (none)\n"
   );
}


//------------------------------------------------------------//
//--- Finalisation                                         ---//
//------------------------------------------------------------//

// File format notes.
//
// - The files are JSON, because it's a widely-used format and saves us having
//   to write a parser in dh_view.js.
//
// - We use a comma-first style for the generated JSON. Comma-first style
//   moves the special case for arrays/objects from the last item to the
//   first. This helps in cases where you can't easily tell in advance the
//   size of arrays/objects, such as iterating over a WordFM (because
//   VG_(sizeFM) is O(n) rather than O(1)), and iterating over stack frames
//   using VG_(apply_ExeContext) in combination with an InlIpCursor.
//
// - We use short field names and minimal whitespace to minimize file sizes.

static VgFile* fp;

#define FP(format, args...) ({ VG_(fprintf)(fp, format, ##args); })

// The frame table holds unique frames.
static WordFM* frame_tbl = NULL;
static UWord next_frame_n = 0;

static Word frame_cmp(UWord a, UWord b)
{
   return VG_(strcmp)((const HChar*)a, (const HChar*)b);
}

static HChar hex_digit_to_ascii_char(UChar d)
{
   d = d & 0xf;
   return (d < 10) ? ('0' + d) : ('a' + (d - 10));
}

// For JSON, we must escape double quote, backslash, and 0x00..0x1f.
//
// Returns the original string if no escaping was required. Returns a pointer
// to a static buffer if escaping was required. Therefore, the return value is
// only valid until the next call to this function.
static const HChar* json_escape(const HChar* s)
{
   static HChar* buf = NULL;
   static SizeT bufcap = 0;

   // Do we need any escaping?
   SizeT extra = 0;
   const HChar* p = s;
   while (*p) {
      UChar c = *p;
      if (c == '"' || c == '\\') {
         extra += 1;
      } else if (c <= 0x1f) {
         extra += 5;
      }
      p++;
   }
   SizeT len = p - s;

   if (extra == 0) {
      // No escaping needed.
      return s;
   }

   // Escaping needed. (The +1 is for the NUL terminator.) Enlarge buf if
   // necessary.
   SizeT newcap = len + extra + 1;
   if (bufcap < newcap) {
      buf = VG_(realloc)("dh.json", buf, newcap);
      bufcap = newcap;
   }

   p = s;
   HChar* q = buf;
   while (*p) {
      UChar c = *p;
      if (c == '"') {
         *q++ = '\\';
         *q++ = '"';
      } else if (c == '\\') {
         *q++ = '\\';
         *q++ = '\\';
      } else if (c <= 0x1f) {
         *q++ = '\\';
         *q++ = 'u';
         *q++ = '0';
         *q++ = '0';
         *q++ = hex_digit_to_ascii_char((c & 0x00f0) >> 4);
         *q++ = hex_digit_to_ascii_char(c & 0x000f);
      } else {
         *q++ = c;
      }
      p++;
   }
   *q = '\0';

   return buf;
}

static void write_APInfo_frame(UInt n, DiEpoch ep, Addr ip, void* opaque)
{
   Bool* is_first = (Bool*)opaque;
   InlIPCursor* iipc = VG_(new_IIPC)(ep, ip);

   do {
      const HChar* buf = VG_(describe_IP)(ep, ip, iipc);

      // Skip entries in vg_replace_malloc.c (e.g. `malloc`, `calloc`,
      // `realloc`, `operator new`) because they're boring and clog up the
      // output.
      if (VG_(strstr)(buf, "vg_replace_malloc.c")) {
         continue;
      }

      // If this description has been seen before, get its number. Otherwise,
      // give it a new number and put it in the table.
      UWord keyW = 0, valW = 0;
      UWord frame_n = 0;
      Bool found = VG_(lookupFM)(frame_tbl, &keyW, &valW, (UWord)buf);
      if (found) {
         //const HChar* str = (const HChar*)keyW;
         //tl_assert(0 == VG_(strcmp)(buf, str));
         frame_n = valW;
      } else {
         // `buf` is a static buffer, we must copy it.
         const HChar* str = VG_(strdup)("dh.frame_tbl.3", buf);
         frame_n = next_frame_n++;
         Bool present = VG_(addToFM)(frame_tbl, (UWord)str, frame_n);
         tl_assert(!present);
      }

      FP("%c%lu", *is_first ? '[' : ',', frame_n);
      *is_first = False;

   } while (VG_(next_IIPC)(iipc));

   VG_(delete_IIPC)(iipc);
};

static void write_APInfo(APInfo* api, Bool is_first)
{
   tl_assert(api->total_blocks >= api->max_blocks);
   tl_assert(api->total_bytes >= api->max_bytes);

   FP(" %c{\"tb\":%llu,\"tbk\":%llu,\"tli\":%llu\n",
      is_first ? '[' : ',',
      api->total_bytes, api->total_blocks, api->total_lifetimes_instrs);
   FP("  ,\"mb\":%llu,\"mbk\":%llu\n",
      api->max_bytes, api->max_blocks);
   FP("  ,\"gb\":%llu,\"gbk\":%llu\n",
      api->at_tgmax_bytes, api->at_tgmax_blocks);
   FP("  ,\"fb\":%llu,\"fbk\":%llu\n",
      api->curr_bytes, api->curr_blocks);
   FP("  ,\"rb\":%llu,\"wb\":%llu\n",
      api->reads_bytes, api->writes_bytes);

   if (api->histo && api->xsize_tag == Exactly) {
      FP("  ,\"acc\":[");

      // Simple run-length encoding: when N entries in a row have the same
      // value M, we print "-N,M". If there is just one in a row, we just
      // print "M". This reduces file size significantly.
      UShort repval = 0;
      Int reps = 0;
      for (UWord i = 0; i < api->xsize; i++) {
         UShort h = api->histo[i];
         if (repval == h) {
            // Continue current run.
            reps++;
         } else {
            // End of run; print it.
            if (reps == 1) {
               FP("%u,", repval);
            } else if (reps > 1) {
               FP("-%d,%u,", reps, repval);
            }
            reps = 1;
            repval = h;
         }
      }
      // Print the final run.
      if (reps == 1) {
         FP("%u", repval);
      } else if (reps > 1) {
         FP("-%d,%u", reps, repval);
      }

      FP("]\n");
   }

   FP("  ,\"fs\":");
   Bool is_first_frame = True;
   VG_(apply_ExeContext)(write_APInfo_frame, &is_first_frame, api->ap);
   FP("]\n");

   FP("  }\n");
}

static void write_APInfos(void)
{
   UWord keyW, valW;

   FP(",\"aps\":\n");

   VG_(initIterFM)(apinfo);
   Bool is_first = True;
   while (VG_(nextIterFM)(apinfo, &keyW, &valW)) {
      APInfo* api = (APInfo*)valW;
      tl_assert(api && api->ap == (ExeContext*)keyW);
      write_APInfo(api, is_first);
      is_first = False;
   }
   VG_(doneIterFM)(apinfo);

   if (is_first) {
      // We didn't print any elements. This happens if apinfo is empty.
      FP(" [\n");
   }

   FP(" ]\n");
}

static void dh_fini(Int exit_status)
{
   // This function does lots of allocations that it doesn't bother to free,
   // because execution is almost over anyway.

   // Total bytes might be at a possible peak.
   check_for_peak();

   // Before printing statistics, we must harvest various stats (such as
   // lifetimes and accesses) for all the blocks that are still alive.
   UWord keyW, valW;
   VG_(initIterFM)( interval_tree );
   while (VG_(nextIterFM)( interval_tree, &keyW, &valW )) {
      Block* bk = (Block*)keyW;
      tl_assert(valW == 0);
      tl_assert(bk);
      retire_Block(bk, False/*!because_freed*/);
   }
   VG_(doneIterFM)( interval_tree );

   // Stats.
   if (VG_(clo_stats)) {
      VG_(dmsg)(" dhat: find_Block_containing:\n");
      VG_(dmsg)("             found: %'lu (%'lu cached + %'lu uncached)\n",
                stats__n_fBc_cached + stats__n_fBc_uncached,
                stats__n_fBc_cached,
                stats__n_fBc_uncached);
      VG_(dmsg)("          notfound: %'lu\n", stats__n_fBc_notfound);
      VG_(dmsg)("\n");
   }

   // Create the frame table, and insert the special "[root]" node at index 0.
   frame_tbl = VG_(newFM)(VG_(malloc),
                          "dh.frame_tbl.1",
                          VG_(free),
                          frame_cmp);
   const HChar* root = VG_(strdup)("dh.frame_tbl.2", "[root]");
   Bool present = VG_(addToFM)(frame_tbl, (UWord)root, 0);
   tl_assert(!present);
   next_frame_n = 1;

   // Setup output filename. Nb: it's important to do this now, i.e. as late
   // as possible. If we do it at start-up and the program forks and the
   // output file format string contains a %p (pid) specifier, both the parent
   // and child will incorrectly write to the same file; this happened in
   // 3.3.0.
   HChar* dhat_out_file =
      VG_(expand_file_name)("--dhat-out-file", clo_dhat_out_file);

   fp = VG_(fopen)(dhat_out_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                   VKI_S_IRUSR|VKI_S_IWUSR);
   if (!fp) {
      VG_(umsg)("error: can't open DHAT output file '%s'\n", dhat_out_file);
      VG_(free)(dhat_out_file);
      return;
   }

   // Write to data file.
   FP("{\"dhatFileVersion\":1\n");

   // The command.
   const HChar* exe = VG_(args_the_exename);
   FP(",\"cmd\":\"%s", json_escape(exe));
   for (Word i = 0; i < VG_(sizeXA)(VG_(args_for_client)); i++) {
      const HChar* arg = *(HChar**)VG_(indexXA)(VG_(args_for_client), i);
      FP(" %s", json_escape(arg));
   }
   FP("\"\n");

   // The PID.
   FP(",\"pid\":%d\n", VG_(getpid)());

   // Times.
   FP(",\"mi\":%llu,\"ei\":%llu\n", g_max_instrs, g_curr_instrs);

   // APs.
   write_APInfos();

   // Frame table.
   FP(",\"ftbl\":\n");

   // The frame table maps strings to numbers. We want to print it ordered by
   // numbers. So we create an array and fill it in from the frame table, then
   // print that.
   UWord n_frames = next_frame_n;
   const HChar** frames =
      VG_(malloc)("dh.frames", n_frames * sizeof(const HChar*));
   VG_(initIterFM)(frame_tbl);
   while (VG_(nextIterFM)(frame_tbl, &keyW, &valW)) {
      const HChar* str = (const HChar*)keyW;
      UWord n = valW;
      frames[n] = str;
   }
   VG_(doneIterFM)(frame_tbl);

   for (UWord i = 0; i < n_frames; i++) {
      FP(" %c\"%s\"\n", i == 0 ? '[' : ',', json_escape(frames[i]));
   }
   FP(" ]\n");
   VG_(free)(frames);

   FP("}\n");

   VG_(fclose)(fp);
   fp = NULL;

   if (VG_(clo_verbosity) == 0) {
      return;
   }

   // Print brief global stats.
   VG_(umsg)("Total:     %'llu bytes in %'llu blocks\n",
             g_total_bytes, g_total_blocks);
   VG_(umsg)("At t-gmax: %'llu bytes in %'llu blocks\n",
             g_max_bytes, g_max_blocks);
   VG_(umsg)("At t-end:  %'llu bytes in %'llu blocks\n",
             g_curr_bytes, g_curr_blocks);
   VG_(umsg)("Reads:     %'llu bytes\n", g_reads_bytes);
   VG_(umsg)("Writes:    %'llu bytes\n", g_writes_bytes);

   // Print a how-to-view-the-profile hint.
   VG_(umsg)("\n");
   VG_(umsg)("To view the resulting profile, open\n");
   VG_(umsg)("  file://%s/%s\n", DHAT_VIEW_DIR, "dh_view.html");
   VG_(umsg)("in a web browser, click on \"Load...\" "
             "and then select the file\n");
   VG_(umsg)("  %s\n", dhat_out_file);
   VG_(umsg)("Scroll to the end the displayed page to see a short\n");
   VG_(umsg)("explanation of some of the abbreviations used in the page.\n");

   VG_(free)(dhat_out_file);
}

//------------------------------------------------------------//
//--- Initialisation                                       ---//
//------------------------------------------------------------//

static void dh_post_clo_init(void)
{
}

static void dh_pre_clo_init(void)
{
   VG_(details_name)            ("DHAT");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a dynamic heap analysis tool");
   VG_(details_copyright_author)(
      "Copyright (C) 2010-2018, and GNU GPL'd, by Mozilla Foundation");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   // Basic functions.
   VG_(basic_tool_funcs)          (dh_post_clo_init,
                                   dh_instrument,
                                   dh_fini);
//zz
   // Needs.
   VG_(needs_libc_freeres)();
   VG_(needs_cxx_freeres)();
   VG_(needs_command_line_options)(dh_process_cmd_line_option,
                                   dh_print_usage,
                                   dh_print_debug_usage);
//zz   VG_(needs_client_requests)     (dh_handle_client_request);
//zz   VG_(needs_sanity_checks)       (dh_cheap_sanity_check,
//zz                                   dh_expensive_sanity_check);
   VG_(needs_malloc_replacement)  (dh_malloc,
                                   dh___builtin_new,
                                   dh___builtin_vec_new,
                                   dh_memalign,
                                   dh_calloc,
                                   dh_free,
                                   dh___builtin_delete,
                                   dh___builtin_vec_delete,
                                   dh_realloc,
                                   dh_malloc_usable_size,
                                   0 );

   VG_(track_pre_mem_read)        ( dh_handle_noninsn_read );
   //VG_(track_pre_mem_read_asciiz) ( check_mem_is_defined_asciiz );
   VG_(track_post_mem_write)      ( dh_handle_noninsn_write );

   tl_assert(!interval_tree);
   tl_assert(!fbc_cache0);
   tl_assert(!fbc_cache1);

   interval_tree = VG_(newFM)( VG_(malloc),
                               "dh.interval_tree.1",
                               VG_(free),
                               interval_tree_Cmp );

   apinfo = VG_(newFM)( VG_(malloc),
                        "dh.apinfo.1",
                        VG_(free),
                        NULL/*unboxedcmp*/ );
}

VG_DETERMINE_INTERFACE_VERSION(dh_pre_clo_init)

//--------------------------------------------------------------------//
//--- end                                                dh_main.c ---//
//--------------------------------------------------------------------//
