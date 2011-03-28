
//--------------------------------------------------------------------*/
//--- DHAT: a Dynamic Heap Analysis Tool                 dh_main.c ---*/
//--------------------------------------------------------------------*/

/*
   This file is part of DHAT, a Valgrind tool for profiling the
   heap usage of programs.

   Copyright (C) 2010-2010 Mozilla Inc

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
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
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

// Number of guest instructions executed so far.  This is 
// incremented directly from the generated code.
static ULong g_guest_instrs_executed = 0;

// Summary statistics for the entire run.
static ULong g_tot_blocks = 0;   // total blocks allocated
static ULong g_tot_bytes  = 0;   // total bytes allocated

static ULong g_cur_blocks_live = 0; // curr # blocks live
static ULong g_cur_bytes_live  = 0; // curr # bytes live

static ULong g_max_blocks_live = 0; // bytes and blocks at
static ULong g_max_bytes_live  = 0; // the max residency point


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
      ULong       n_reads;
      ULong       n_writes;
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
      // the allocation point that we're summarising stats for
      ExeContext* ap;
      // used when printing results
      Bool shown;
      // The current number of blocks and bytes live for this AP
      ULong cur_blocks_live;
      ULong cur_bytes_live;
      // The number of blocks and bytes live at the max-liveness
      // point.  Note this is a bit subtle.  max_blocks_live is not
      // the maximum number of live blocks, but rather the number of
      // blocks live at the point of maximum byte liveness.  These are
      // not necessarily the same thing.
      ULong max_blocks_live;
      ULong max_bytes_live;
      // Total number of blocks and bytes allocated by this AP.
      ULong tot_blocks;
      ULong tot_bytes;
      // Sum of death ages for all blocks allocated by this AP,
      // that have subsequently been freed.
      ULong death_ages_sum;
      ULong deaths;
      // Total number of reads and writes in all blocks allocated
      // by this AP.
      ULong n_reads;
      ULong n_writes;
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
      api = VG_(malloc)( "dh.main.intro_Block.1", sizeof(APInfo) );
      VG_(memset)(api, 0, sizeof(*api));
      api->ap = bk->ap;
      Bool present = VG_(addToFM)( apinfo,
                                   (UWord)bk->ap, (UWord)api );
      tl_assert(!present);
      // histo stuff
      tl_assert(api->deaths == 0);
      api->xsize_tag = Unknown;
      api->xsize = 0;
      if (0) VG_(printf)("api %p   -->  Unknown\n", api);
   }

   tl_assert(api->ap == bk->ap);

   /* So: update stats to reflect an allocation */

   // # live blocks
   api->cur_blocks_live++;

   // # live bytes
   api->cur_bytes_live += bk->req_szB;
   if (api->cur_bytes_live > api->max_bytes_live) {
      api->max_bytes_live  = api->cur_bytes_live;
      api->max_blocks_live = api->cur_blocks_live;
   }

   // total blocks and bytes allocated here
   api->tot_blocks++;
   api->tot_bytes += bk->req_szB;

   // update summary globals
   g_tot_blocks++;
   g_tot_bytes += bk->req_szB;

   g_cur_blocks_live++;
   g_cur_bytes_live += bk->req_szB;
   if (g_cur_bytes_live > g_max_bytes_live) {
      g_max_bytes_live = g_cur_bytes_live;
      g_max_blocks_live = g_cur_blocks_live;
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
               bk->ap, api->cur_bytes_live, (ULong)bk->req_szB);

   // update total blocks live etc for this AP
   if (because_freed) {
      tl_assert(api->cur_blocks_live >= 1);
      tl_assert(api->cur_bytes_live >= bk->req_szB);
      api->cur_blocks_live--;
      api->cur_bytes_live -= bk->req_szB;

      api->deaths++;

      tl_assert(bk->allocd_at <= g_guest_instrs_executed);
      api->death_ages_sum += (g_guest_instrs_executed - bk->allocd_at);

      // update global summary stats
      tl_assert(g_cur_blocks_live > 0);
      g_cur_blocks_live--;
      tl_assert(g_cur_bytes_live >= bk->req_szB);
      g_cur_bytes_live -= bk->req_szB;
   }

   // access counts
   api->n_reads  += bk->n_reads;
   api->n_writes += bk->n_writes;

   // histo stuff.  First, do state transitions for xsize/xsize_tag.
   switch (api->xsize_tag) {

      case Unknown:
         tl_assert(api->xsize == 0);
         tl_assert(api->deaths == 1 || api->deaths == 0);
         tl_assert(!api->histo);
         api->xsize_tag = Exactly;
         api->xsize = bk->req_szB;
         if (0) VG_(printf)("api %p   -->  Exactly(%lu)\n", api, api->xsize);
         // and allocate the histo
         if (bk->histoW) {
            api->histo = VG_(malloc)("dh.main.retire_Block.1",
                                     api->xsize * sizeof(UInt));
            VG_(memset)(api->histo, 0, api->xsize * sizeof(UInt));
         }
         break;

      case Exactly:
         //tl_assert(api->deaths > 1);
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
         //tl_assert(api->deaths > 1);
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
static void apinfo_change_cur_bytes_live( ExeContext* ec, Long delta )
{
   APInfo* api   = NULL;
   UWord   keyW  = 0;
   UWord   valW  = 0;
   Bool    found = VG_(lookupFM)( apinfo,
                                  &keyW, &valW, (UWord)ec );

   tl_assert(found);
   api = (APInfo*)valW;
   tl_assert(api->ap == ec);

   if (delta < 0) {
      tl_assert(api->cur_bytes_live >= -delta);
      tl_assert(g_cur_bytes_live >= -delta);
   }

   // adjust current live size
   api->cur_bytes_live += delta;
   g_cur_bytes_live += delta;

   if (delta > 0 && api->cur_bytes_live > api->max_bytes_live) {
      api->max_bytes_live  = api->cur_bytes_live;
      api->max_blocks_live = api->cur_blocks_live;
   }

   // update global summary stats
   if (delta > 0 && g_cur_bytes_live > g_max_bytes_live) {
      g_max_bytes_live = g_cur_bytes_live;
      g_max_blocks_live = g_cur_blocks_live;
   }
   if (delta > 0)
      g_tot_bytes += delta;

   // adjust total allocation size
   if (delta > 0)
      api->tot_bytes += delta;
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
      actual_szB = VG_(malloc_usable_size)(p);
      tl_assert(actual_szB >= req_szB);
      /* slop_szB = actual_szB - req_szB; */
   } else {
      /* slop_szB = 0; */
   }

   // Make new HP_Chunk node, add to malloc_list
   Block* bk = VG_(malloc)("dh.new_block.1", sizeof(Block));
   bk->payload   = (Addr)p;
   bk->req_szB   = req_szB;
   bk->ap        = VG_(record_ExeContext)(tid, 0/*first word delta*/);
   bk->allocd_at = g_guest_instrs_executed;
   bk->n_reads   = 0;
   bk->n_writes  = 0;
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

   if (0) VG_(printf)("ALLOC %ld -> %p\n", req_szB, p);

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
                      p, g_guest_instrs_executed - bk->allocd_at);

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
   if (0) VG_(printf)("REALL %p %ld\n", p_old, new_req_szB);
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
      apinfo_change_cur_bytes_live(bk->ap,
                                   (Long)new_req_szB - (Long)bk->req_szB);
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
      apinfo_change_cur_bytes_live(bk->ap,
                                   (Long)new_req_szB - (Long)bk->req_szB);
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
   tl_assert(0);
//zz   HP_Chunk* hc = VG_(HT_lookup)( malloc_list, (UWord)p );
//zz
//zz   return ( hc ? hc->req_szB + hc->slop_szB : 0 );
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
      bk->n_writes += szB;
      if (bk->histoW)
         inc_histo_for_block(bk, addr, szB);
   }
}

static VG_REGPARM(2)
void dh_handle_read ( Addr addr, UWord szB )
{
   Block* bk = find_Block_containing(addr);
   if (bk) {
      bk->n_reads += szB;
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
void dh_handle_noninsn_read ( CorePart part, ThreadId tid, Char* s,
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
   // Add code to increment 'g_guest_instrs_executed' by 'n', like this:
   //   WrTmp(t1, Load64(&g_guest_instrs_executed))
   //   WrTmp(t2, Add64(RdTmp(t1), Const(n)))
   //   Store(&g_guest_instrs_executed, t2)
   IRTemp t1 = newIRTemp(sbOut->tyenv, Ity_I64);
   IRTemp t2 = newIRTemp(sbOut->tyenv, Ity_I64);
   IRExpr* counter_addr = mkIRExpr_HWord( (HWord)&g_guest_instrs_executed );

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
   HChar*   hName    = NULL;
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
                      VexGuestLayout* layout,
                      VexGuestExtents* vge,
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

// FORWARDS
static Bool identify_metric ( /*OUT*/ULong(**get_metricP)(APInfo*),
                              /*OUT*/Bool* increasingP,
                              Char* metric_name );

static Int    clo_show_top_n = 10;
static HChar* clo_sort_by    = "max-bytes-live";

static Bool dh_process_cmd_line_option(Char* arg)
{
   if VG_BINT_CLO(arg, "--show-top-n", clo_show_top_n, 1, 100000) {}

   else if VG_STR_CLO(arg, "--sort-by", clo_sort_by) {
       ULong (*dummyFn)(APInfo*);
       Bool dummyB;
       Bool ok = identify_metric( &dummyFn, &dummyB, clo_sort_by);
       if (!ok)
          return False;
       // otherwise it's OK, in which case leave it alone.
       // show_top_n_apinfos will later convert the string by a
       // second call to identify_metric.
   }

   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;
}


static void dh_print_usage(void)
{
   VG_(printf)(
"    --show-top-n=number       show the top <number> alloc points [10]\n"
"    --sort-by=string\n"
"            sort the allocation points by the metric\n"
"            defined by <string>, thusly:\n"
"                max-bytes-live    maximum live bytes [default]\n"
"                tot-bytes-allocd  total allocation (turnover)\n"
"                max-blocks-live   maximum live blocks\n"
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

static void show_N_div_100( /*OUT*/HChar* buf, ULong n )
{
   ULong nK = n / 100;
   ULong nR = n % 100;
   VG_(sprintf)(buf, "%llu.%s%llu", nK,
                nR < 10 ? "0" : "",
                nR);
}

static void show_APInfo ( APInfo* api )
{
   HChar bufA[80];
   VG_(memset)(bufA, 0, sizeof(bufA));
   if (api->tot_blocks > 0) {
      show_N_div_100( bufA, ((ULong)api->tot_bytes * 100ULL)
                              / (ULong)api->tot_blocks );
   } else {
      bufA[0] = 'N'; bufA[1] = 'a'; bufA[2] = 'N';
   }

   VG_(umsg)("max-live:    %'llu in %'llu blocks\n",
             api->max_bytes_live, api->max_blocks_live);
   VG_(umsg)("tot-alloc:   %'llu in %'llu blocks (avg size %s)\n",
             api->tot_bytes, api->tot_blocks, bufA);

   tl_assert(api->tot_blocks >= api->max_blocks_live);
   tl_assert(api->tot_bytes >= api->max_bytes_live);

   if (api->deaths > 0) {
      // Average Age at Death
      ULong aad = api->deaths == 0
                  ? 0 : (api->death_ages_sum / api->deaths);
      // AAD as a fraction of the total program lifetime (so far)
      // measured in ten-thousand-ths (aad_frac_10k == 10000 means the
      // complete lifetime of the program.
      ULong aad_frac_10k
         = g_guest_instrs_executed == 0
           ? 0 : (10000ULL * aad) / g_guest_instrs_executed;
      HChar buf[16];
      show_N_div_100(buf, aad_frac_10k);
      VG_(umsg)("deaths:      %'llu, at avg age %'llu "
                "(%s%% of prog lifetime)\n",
                api->deaths, aad, buf );
   } else {
      VG_(umsg)("deaths:      none (none of these blocks were freed)\n");
   }

   HChar bufR[80], bufW[80];
   VG_(memset)(bufR, 0, sizeof(bufR));
   VG_(memset)(bufW, 0, sizeof(bufW));
   if (api->tot_bytes > 0) {
      show_N_div_100(bufR, (100ULL * api->n_reads) / api->tot_bytes);
      show_N_div_100(bufW, (100ULL * api->n_writes) / api->tot_bytes);
   } else {
      VG_(strcat)(bufR, "Inf");
      VG_(strcat)(bufW, "Inf");
   }

   VG_(umsg)("acc-ratios:  %s rd, %s wr "
             " (%'llu b-read, %'llu b-written)\n",
             bufR, bufW,
             api->n_reads, api->n_writes);

   VG_(pp_ExeContext)(api->ap);

   if (api->histo && api->xsize_tag == Exactly) {
      VG_(umsg)("\nAggregated access counts by offset:\n");
      VG_(umsg)("\n");
      UWord i;
      if (api->xsize > 0)
         VG_(umsg)("[   0]  ");
      for (i = 0; i < api->xsize; i++) {
         if (i > 0 && (i % 16) == 0 && i != api->xsize-1) {
            VG_(umsg)("\n");
            VG_(umsg)("[%4lu]  ", i);
         }
         VG_(umsg)("%u ", api->histo[i]);
      }
      VG_(umsg)("\n");
   }
}


/* Metric-access functions for APInfos. */
static ULong get_metric__max_bytes_live ( APInfo* api ) {
   return api->max_bytes_live;
}
static ULong get_metric__tot_bytes ( APInfo* api ) {
   return api->tot_bytes;
}
static ULong get_metric__max_blocks_live ( APInfo* api ) {
   return api->max_blocks_live;
}

/* Given a string, return the metric-access function and also a Bool
   indicating whether we want increasing or decreasing values of the
   metric.  This is used twice, once in command line processing, and
   then again in show_top_n_apinfos.  Returns False if the given
   string could not be identified.*/
static Bool identify_metric ( /*OUT*/ULong(**get_metricP)(APInfo*),
                              /*OUT*/Bool* increasingP,
                              Char* metric_name )
{
   if (0 == VG_(strcmp)(metric_name, "max-bytes-live")) {
      *get_metricP = get_metric__max_bytes_live;
      *increasingP = False;
      return True;
   }
   if (0 == VG_(strcmp)(metric_name, "tot-bytes-allocd")) {
      *get_metricP = get_metric__tot_bytes;
      *increasingP = False;
      return True;
   }
   if (0 == VG_(strcmp)(metric_name, "max-blocks-live")) {
      *get_metricP = get_metric__max_blocks_live;
      *increasingP = False;
      return True;
   }
   return False;
}


static void show_top_n_apinfos ( void )
{
   Int   i;
   UWord keyW, valW;
   ULong (*get_metric)(APInfo*);
   Bool  increasing;

   HChar* metric_name = clo_sort_by;
   tl_assert(metric_name); // ensured by clo processing

   Bool ok = identify_metric( &get_metric, &increasing, metric_name );
   tl_assert(ok); // ensured by clo processing

   VG_(umsg)("\n");
   VG_(umsg)("======== ORDERED BY %s \"%s\": "
             "top %d allocators ========\n", 
             increasing ? "increasing" : "decreasing",
             metric_name, clo_show_top_n );

   // Clear all .shown bits
   VG_(initIterFM)( apinfo );
   while (VG_(nextIterFM)( apinfo, &keyW, &valW )) {
      APInfo* api = (APInfo*)valW;
      tl_assert(api && api->ap == (ExeContext*)keyW);
      api->shown = False;
   }
   VG_(doneIterFM)( apinfo );

   // Now print the top N entries.  Each one requires a 
   // complete scan of the set.  Duh.
   for (i = 0; i < clo_show_top_n; i++) {
      ULong   best_metric = increasing ? ~0ULL : 0ULL;
      APInfo* best_api    = NULL;

      VG_(initIterFM)( apinfo );
      while (VG_(nextIterFM)( apinfo, &keyW, &valW )) {
         APInfo* api = (APInfo*)valW;
         if (api->shown)
            continue;
         ULong metric = get_metric(api);
         if (increasing ? (metric < best_metric) : (metric > best_metric)) {
            best_metric = metric;
            best_api = api;
         }
      }
      VG_(doneIterFM)( apinfo );

      if (!best_api)
         break; // all APIs have been shown.  Stop.

      VG_(umsg)("\n");
      VG_(umsg)("-------------------- %d of %d --------------------\n",
                i+1, clo_show_top_n );
      show_APInfo(best_api);
      best_api->shown = True;
   }

   VG_(umsg)("\n");
}


static void dh_fini(Int exit_status)
{
   // Before printing statistics, we must harvest access counts for
   // all the blocks that are still alive.  Not doing so gives
   // access ratios which are too low (zero, in the worst case)
   // for such blocks, since the accesses that do get made will
   // (if we skip this step) not get folded into the AP summaries.
   UWord keyW, valW;
   VG_(initIterFM)( interval_tree );
   while (VG_(nextIterFM)( interval_tree, &keyW, &valW )) {
      Block* bk = (Block*)keyW;
      tl_assert(valW == 0);
      tl_assert(bk);
      retire_Block(bk, False/*!because_freed*/);
   }
   VG_(doneIterFM)( interval_tree );

   // show results
   VG_(umsg)("======== SUMMARY STATISTICS ========\n");
   VG_(umsg)("\n");
   VG_(umsg)("guest_insns:  %'llu\n", g_guest_instrs_executed);
   VG_(umsg)("\n");
   VG_(umsg)("max_live:     %'llu in %'llu blocks\n",
             g_max_bytes_live, g_max_blocks_live);
   VG_(umsg)("\n");
   VG_(umsg)("tot_alloc:    %'llu in %'llu blocks\n",
             g_tot_bytes, g_tot_blocks);
   VG_(umsg)("\n");
   if (g_tot_bytes > 0) {
      VG_(umsg)("insns per allocated byte: %'llu\n",
                g_guest_instrs_executed / g_tot_bytes);
      VG_(umsg)("\n");
   }

   show_top_n_apinfos();

   VG_(umsg)("\n");
   VG_(umsg)("\n");
   VG_(umsg)("==============================================================\n");
   VG_(umsg)("\n");
   VG_(umsg)("Some hints: (see --help for command line option details):\n");
   VG_(umsg)("\n");
   VG_(umsg)("* summary stats for whole program are at the top of this output\n");
   VG_(umsg)("\n");
   VG_(umsg)("* --show-top-n=  controls how many alloc points are shown.\n");
   VG_(umsg)("                 You probably want to set it much higher than\n");
   VG_(umsg)("                 the default value (10)\n");
   VG_(umsg)("\n");
   VG_(umsg)("* --sort-by=     specifies the sort key for output.\n");
   VG_(umsg)("                 See --help for details.\n");
   VG_(umsg)("\n");
   VG_(umsg)("* Each allocation stack, by default 12 frames, counts as\n");
   VG_(umsg)("  a separate alloc point.  This causes the data to be spread out\n");
   VG_(umsg)("  over far too many alloc points.  I strongly suggest using\n");
   VG_(umsg)("  --num-callers=4 or some such, to reduce the spreading.\n");
   VG_(umsg)("\n");

   if (VG_(clo_stats)) {
      VG_(dmsg)(" dhat: find_Block_containing:\n");
      VG_(dmsg)("             found: %'lu (%'lu cached + %'lu uncached)\n",
                stats__n_fBc_cached + stats__n_fBc_uncached,
                stats__n_fBc_cached,
                stats__n_fBc_uncached);
      VG_(dmsg)("          notfound: %'lu\n", stats__n_fBc_notfound);
      VG_(dmsg)("\n");
   }
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
      "Copyright (C) 2010-2010, and GNU GPL'd, by Mozilla Inc");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   // Basic functions.
   VG_(basic_tool_funcs)          (dh_post_clo_init,
                                   dh_instrument,
                                   dh_fini);
//zz
   // Needs.
   VG_(needs_libc_freeres)();
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
                               "dh.main.interval_tree.1",
                               VG_(free),
                               interval_tree_Cmp );

   apinfo = VG_(newFM)( VG_(malloc),
                        "dh.main.apinfo.1",
                        VG_(free),
                        NULL/*unboxedcmp*/ );
}

VG_DETERMINE_INTERFACE_VERSION(dh_pre_clo_init)

//--------------------------------------------------------------------//
//--- end                                                dh_main.c ---//
//--------------------------------------------------------------------//
