
/*--------------------------------------------------------------------*/
/*--- The leak checker.                             mc_leakcheck.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

#include "pub_tool_basics.h"
#include "pub_tool_vki.h"
#include "pub_tool_aspacehl.h"
#include "pub_tool_aspacemgr.h"
#include "pub_tool_execontext.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcsignal.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_oset.h"
#include "pub_tool_poolalloc.h"     
#include "pub_tool_signals.h"       // Needed for mc_include.h
#include "pub_tool_libcsetjmp.h"    // setjmp facilities
#include "pub_tool_tooliface.h"     // Needed for mc_include.h
#include "pub_tool_xarray.h"
#include "pub_tool_xtree.h"

#include "mc_include.h"

/*------------------------------------------------------------*/
/*--- An overview of leak checking.                        ---*/
/*------------------------------------------------------------*/

// Leak-checking is a directed-graph traversal problem.  The graph has
// two kinds of nodes:
// - root-set nodes:
//   - GP registers of all threads;
//   - valid, aligned, pointer-sized data words in valid client memory,
//     including stacks, but excluding words within client heap-allocated
//     blocks (they are excluded so that later on we can differentiate
//     between heap blocks that are indirectly leaked vs. directly leaked).
// - heap-allocated blocks.  A block is a mempool chunk or a malloc chunk
//   that doesn't contain a mempool chunk.  Nb: the terms "blocks" and
//   "chunks" are used interchangeably below.
//
// There are two kinds of edges:
// - start-pointers, i.e. pointers to the start of a block;
// - interior-pointers, i.e. pointers to the interior of a block.
//
// We use "pointers" rather than "edges" below.
//
// Root set nodes only point to blocks.  Blocks only point to blocks;
// a block can point to itself.
//
// The aim is to traverse the graph and determine the status of each block.
//
// There are 9 distinct cases.  See memcheck/docs/mc-manual.xml for details.
// Presenting all nine categories to the user is probably too much.
// Currently we do this:
// - definitely lost:  case 3
// - indirectly lost:  case 4, 9
// - possibly lost:    cases 5..8
// - still reachable:  cases 1, 2
// 
// It's far from clear that this is the best possible categorisation;  it's
// accreted over time without any central guiding principle.

/*------------------------------------------------------------*/
/*--- XXX: Thoughts for improvement.                       ---*/
/*------------------------------------------------------------*/

// From the user's point of view:
// - If they aren't using interior-pointers, they just have to fix the
//   directly lost blocks, and the indirectly lost ones will be fixed as
//   part of that.  Any possibly lost blocks will just be due to random
//   pointer garbage and can be ignored.
// 
// - If they are using interior-pointers, the fact that they currently are not
//   being told which ones might be directly lost vs. indirectly lost makes
//   it hard to know where to begin.
// 
// All this makes me wonder if new option is warranted:
// --follow-interior-pointers.  By default it would be off, the leak checker
// wouldn't follow interior-pointers and there would only be 3 categories:
// R, DL, IL.
// 
// If turned on, then it would show 7 categories (R, DL, IL, DR/DL, IR/IL,
// IR/IL/DL, IL/DL).  That output is harder to understand but it's your own
// damn fault for using interior-pointers...
//
// ----
//
// Also, why are two blank lines printed between each loss record?
// [bug 197930]
//
// ----
//
// Also, --show-reachable is a bad name because it also turns on the showing
// of indirectly leaked blocks(!)  It would be better named --show-all or
// --show-all-heap-blocks, because that's the end result.
// We now have the option --show-leak-kinds=... which allows to specify =all.
//
// ----
//
// Also, the VALGRIND_LEAK_CHECK and VALGRIND_QUICK_LEAK_CHECK aren't great
// names.  VALGRIND_FULL_LEAK_CHECK and VALGRIND_SUMMARY_LEAK_CHECK would be
// better.
//
// ----
//
// Also, VALGRIND_COUNT_LEAKS and VALGRIND_COUNT_LEAK_BLOCKS aren't great as
// they combine direct leaks and indirect leaks into one.  New, more precise
// ones (they'll need new names) would be good.  If more categories are
// used, as per the --follow-interior-pointers option, they should be
// updated accordingly.  And they should use a struct to return the values.
//
// ----
//
// Also, for this case:
//
//  (4)  p4      BBB ---> AAA
// 
// BBB is definitely directly lost.  AAA is definitely indirectly lost.
// Here's the relevant loss records printed for a full check (each block is
// 16 bytes):
// 
// ==20397== 16 bytes in 1 blocks are indirectly lost in loss record 9 of 15
// ==20397==    at 0x4C2694E: malloc (vg_replace_malloc.c:177)
// ==20397==    by 0x400521: mk (leak-cases.c:49)
// ==20397==    by 0x400578: main (leak-cases.c:72)
// 
// ==20397== 32 (16 direct, 16 indirect) bytes in 1 blocks are definitely
// lost in loss record 14 of 15
// ==20397==    at 0x4C2694E: malloc (vg_replace_malloc.c:177)
// ==20397==    by 0x400521: mk (leak-cases.c:49)
// ==20397==    by 0x400580: main (leak-cases.c:72)
// 
// The first one is fine -- it describes AAA.
// 
// The second one is for BBB.  It's correct in that 16 bytes in 1 block are
// directly lost. It's also correct that 16 are indirectly lost as a result,
// but it means that AAA is being counted twice in the loss records.  (It's
// not, thankfully, counted twice in the summary counts).  Argh.
// 
// This would be less confusing for the second one:
// 
// ==20397== 16 bytes in 1 blocks are definitely lost in loss record 14
// of 15 (and 16 bytes in 1 block are indirectly lost as a result;  they
// are mentioned elsewhere (if --show-reachable=yes or indirect is given
// in --show-leak-kinds=... !))
// ==20397==    at 0x4C2694E: malloc (vg_replace_malloc.c:177)
// ==20397==    by 0x400521: mk (leak-cases.c:49)
// ==20397==    by 0x400580: main (leak-cases.c:72)
// 
// But ideally we'd present the loss record for the directly lost block and
// then the resultant indirectly lost blocks and make it clear the
// dependence.  Double argh.

/*------------------------------------------------------------*/
/*--- The actual algorithm.                                ---*/
/*------------------------------------------------------------*/

// - Find all the blocks (a.k.a. chunks) to check.  Mempool chunks require
//   some special treatment because they can be within malloc'd blocks.
// - Scan every word in the root set (GP registers and valid
//   non-heap memory words).
//   - First, we skip if it doesn't point to valid memory.
//   - Then, we see if it points to the start or interior of a block.  If
//     so, we push the block onto the mark stack and mark it as having been
//     reached.
// - Then, we process the mark stack, repeating the scanning for each block;
//   this can push more blocks onto the mark stack.  We repeat until the
//   mark stack is empty.  Each block is marked as definitely or possibly
//   reachable, depending on whether interior-pointers were required to
//   reach it.
// - At this point we know for every block if it's reachable or not.
// - We then push each unreached block onto the mark stack, using the block
//   number as the "clique" number.
// - We process the mark stack again, this time grouping blocks into cliques
//   in order to facilitate the directly/indirectly lost categorisation.
// - We group blocks by their ExeContexts and categorisation, and print them
//   if --leak-check=full.  We also print summary numbers.
//
// A note on "cliques":
// - A directly lost block is one with no pointers to it.  An indirectly
//   lost block is one that is pointed to by a directly or indirectly lost
//   block.
// - Each directly lost block has zero or more indirectly lost blocks
//   hanging off it.  All these blocks together form a "clique".  The
//   directly lost block is called the "clique leader".  The clique number
//   is the number (in lc_chunks[]) of the clique leader.
// - Actually, a directly lost block may be pointed to if it's part of a
//   cycle.  In that case, there may be more than one choice for the clique
//   leader, and the choice is arbitrary.  Eg. if you have A-->B and B-->A
//   either A or B could be the clique leader.
// - Cliques cannot overlap, and will be truncated to avoid this.  Eg. if we
//   have A-->C and B-->C, the two cliques will be {A,C} and {B}, or {A} and
//   {B,C} (again the choice is arbitrary).  This is because we don't want
//   to count a block as indirectly lost more than once.
//
// A note on 'is_prior_definite': 
// - This is a boolean used in various places that indicates if the chain
//   up to the prior node (prior to the one being considered) is definite.
// - In the clique == -1 case: 
//   - if True it means that the prior node is a root-set node, or that the
//     prior node is a block which is reachable from the root-set via
//     start-pointers.
//   - if False it means that the prior node is a block that is only
//     reachable from the root-set via a path including at least one
//     interior-pointer.
// - In the clique != -1 case, currently it's always True because we treat
//   start-pointers and interior-pointers the same for direct/indirect leak
//   checking.  If we added a PossibleIndirectLeak state then this would
//   change.


// Define to debug the memory-leak-detector.
#define VG_DEBUG_FIND_CHUNK 0
#define VG_DEBUG_LEAKCHECK 0
#define VG_DEBUG_CLIQUE    0
 

/*------------------------------------------------------------*/
/*--- Getting the initial chunks, and searching them.      ---*/
/*------------------------------------------------------------*/

// Compare the MC_Chunks by 'data' (i.e. the address of the block).
static Int compare_MC_Chunks(const void* n1, const void* n2)
{
   const MC_Chunk* mc1 = *(const MC_Chunk *const *)n1;
   const MC_Chunk* mc2 = *(const MC_Chunk *const *)n2;
   if (mc1->data < mc2->data) return -1;
   if (mc1->data > mc2->data) return  1;
   return 0;
}

#if VG_DEBUG_FIND_CHUNK
// Used to sanity-check the fast binary-search mechanism.
static 
Int find_chunk_for_OLD ( Addr       ptr, 
                         MC_Chunk** chunks,
                         Int        n_chunks )

{
   Int  i;
   Addr a_lo, a_hi;
   PROF_EVENT(MCPE_FIND_CHUNK_FOR_OLD);
   for (i = 0; i < n_chunks; i++) {
      PROF_EVENT(MCPE_FIND_CHUNK_FOR_OLD_LOOP);
      a_lo = chunks[i]->data;
      a_hi = ((Addr)chunks[i]->data) + chunks[i]->szB;
      if (a_lo == a_hi)
         a_hi++; // Special case for szB 0. See find_chunk_for.
      if (a_lo <= ptr && ptr < a_hi)
         return i;
   }
   return -1;
}
#endif

// Find the i such that ptr points at or inside the block described by
// chunks[i].  Return -1 if none found.  This assumes that chunks[]
// has been sorted on the 'data' field.
static 
Int find_chunk_for ( Addr       ptr, 
                     MC_Chunk** chunks,
                     Int        n_chunks )
{
   Addr a_mid_lo, a_mid_hi;
   Int lo, mid, hi, retVal;
   // VG_(printf)("find chunk for %p = ", ptr);
   retVal = -1;
   lo = 0;
   hi = n_chunks-1;
   while (True) {
      // Invariant: current unsearched space is from lo to hi, inclusive.
      if (lo > hi) break; // not found

      mid      = (lo + hi) / 2;
      a_mid_lo = chunks[mid]->data;
      a_mid_hi = chunks[mid]->data + chunks[mid]->szB;
      // Extent of block 'mid' is [a_mid_lo .. a_mid_hi).
      // Special-case zero-sized blocks - treat them as if they had
      // size 1.  Not doing so causes them to not cover any address
      // range at all and so will never be identified as the target of
      // any pointer, which causes them to be incorrectly reported as
      // definitely leaked.
      if (chunks[mid]->szB == 0)
         a_mid_hi++;

      if (ptr < a_mid_lo) {
         hi = mid-1;
         continue;
      } 
      if (ptr >= a_mid_hi) {
         lo = mid+1;
         continue;
      }
      tl_assert(ptr >= a_mid_lo && ptr < a_mid_hi);
      retVal = mid;
      break;
   }

#  if VG_DEBUG_FIND_CHUNK
   tl_assert(retVal == find_chunk_for_OLD ( ptr, chunks, n_chunks ));
#  endif
   // VG_(printf)("%d\n", retVal);
   return retVal;
}


static MC_Chunk**
get_sorted_array_of_active_chunks(Int* pn_chunks)
{
   UInt n_mallocs;
   MC_Chunk **mallocs;

   // First we collect all the malloc chunks into an array and sort it.
   // We do this because we want to query the chunks by interior
   // pointers, requiring binary search.
   mallocs = (MC_Chunk**) VG_(HT_to_array)( MC_(malloc_list), &n_mallocs );
   if (n_mallocs == 0) {
      tl_assert(mallocs == NULL);
      *pn_chunks = 0;
      return NULL;
   }
   VG_(ssort)(mallocs, n_mallocs, sizeof(VgHashNode*), compare_MC_Chunks);

   // If there are no mempools (for most users, this is the case),
   //    n_mallocs and mallocs is the final result
   // otherwise we need to do special handling for mempools.

   if (VG_(HT_count_nodes)(MC_(mempool_list)) > 0) {
      // Our goal is to construct a set of chunks that includes every
      // mempool chunk, and every malloc region that *doesn't* contain a
      // mempool chunk.
      MC_Mempool *mp;
      MC_Chunk **chunks, *mc;
      UInt n_chunks, m, s;
      Bool *malloc_chunk_holds_a_pool_chunk;

      // We build an array containing a Bool for each malloc chunk,
      // indicating whether it contains any mempools.
      malloc_chunk_holds_a_pool_chunk = VG_(calloc)( "mc.fas.1",
                                                     n_mallocs, sizeof(Bool) );
      n_chunks = n_mallocs;

      // Then we loop over the mempool tables. For each chunk in each
      // pool, we set the entry in the Bool array corresponding to the
      // malloc chunk containing the mempool chunk.
      VG_(HT_ResetIter)(MC_(mempool_list));
      while ( (mp = VG_(HT_Next)(MC_(mempool_list))) ) {
         VG_(HT_ResetIter)(mp->chunks);
         while ( (mc = VG_(HT_Next)(mp->chunks)) ) {

            // We'll need to record this chunk.
            n_chunks++;

            // Possibly invalidate the malloc holding the beginning of this chunk.
            m = find_chunk_for(mc->data, mallocs, n_mallocs);
            if (m != -1 && malloc_chunk_holds_a_pool_chunk[m] == False) {
               tl_assert(n_chunks > 0);
               n_chunks--;
               malloc_chunk_holds_a_pool_chunk[m] = True;
            }

            // Possibly invalidate the malloc holding the end of this chunk.
            if (mc->szB > 1) {
               m = find_chunk_for(mc->data + (mc->szB - 1), mallocs, n_mallocs);
               if (m != -1 && malloc_chunk_holds_a_pool_chunk[m] == False) {
                  tl_assert(n_chunks > 0);
                  n_chunks--;
                  malloc_chunk_holds_a_pool_chunk[m] = True;
               }
            }
         }
      }
      tl_assert(n_chunks > 0);

      // Create final chunk array.
      chunks = VG_(malloc)("mc.fas.2", sizeof(VgHashNode*) * (n_chunks));
      s = 0;

      // Copy the mempool chunks and the non-marked malloc chunks into a
      // combined array of chunks.
      VG_(HT_ResetIter)(MC_(mempool_list));
      while ( (mp = VG_(HT_Next)(MC_(mempool_list))) ) {
         VG_(HT_ResetIter)(mp->chunks);
         while ( (mc = VG_(HT_Next)(mp->chunks)) ) {
            tl_assert(s < n_chunks);
            chunks[s++] = mc;
         }
      }
      for (m = 0; m < n_mallocs; ++m) {
         if (!malloc_chunk_holds_a_pool_chunk[m]) {
            tl_assert(s < n_chunks);
            chunks[s++] = mallocs[m];
         }
      }
      tl_assert(s == n_chunks);

      // Free temporaries.
      VG_(free)(mallocs);
      VG_(free)(malloc_chunk_holds_a_pool_chunk);

      *pn_chunks = n_chunks;

      // Sort the array so blocks are in ascending order in memory.
      VG_(ssort)(chunks, n_chunks, sizeof(VgHashNode*), compare_MC_Chunks);

      // Sanity check -- make sure they're in order.
      for (int i = 0; i < n_chunks-1; i++) {
         tl_assert( chunks[i]->data <= chunks[i+1]->data);
      }

      return chunks;
   } else {
      *pn_chunks = n_mallocs;
      return mallocs;
   }
}

/*------------------------------------------------------------*/
/*--- The leak detector proper.                            ---*/
/*------------------------------------------------------------*/

// Holds extra info about each block during leak checking.
typedef 
   struct {
      UInt  state:2;    // Reachedness.
      UInt  pending:1;  // Scan pending.
      UInt  heuristic: (sizeof(UInt)*8)-3;
      // Heuristic with which this block was considered reachable.
      // LchNone if state != Reachable or no heuristic needed to
      // consider it reachable.
   
      union {
         SizeT indirect_szB;
         // If Unreached, how many bytes are unreachable from here.
         SizeT  clique;
         // if IndirectLeak, clique leader to which it belongs.
      } IorC;
   } 
   LC_Extra;

// An array holding pointers to every chunk we're checking.  Sorted by address.
// lc_chunks is initialised during leak search. It is kept after leak search
// to support printing the list of blocks belonging to a loss record.
// lc_chunk array can only be used validly till the next "free" operation
// (as a free operation potentially destroys one or more chunks).
// To detect lc_chunk is valid, we store the nr of frees operations done
// when lc_chunk was build : lc_chunks (and lc_extras) stays valid as
// long as no free operations has been done since lc_chunks building.
static MC_Chunk** lc_chunks;
// How many chunks we're dealing with.
static Int        lc_n_chunks;
static SizeT lc_chunks_n_frees_marker;
// This has the same number of entries as lc_chunks, and each entry
// in lc_chunks corresponds with the entry here (ie. lc_chunks[i] and
// lc_extras[i] describe the same block).
static LC_Extra* lc_extras;

// chunks will be converted and merged in loss record, maintained in lr_table
// lr_table elements are kept from one leak_search to another to implement
// the "print new/changed leaks" client request
static OSet*        lr_table;
// Array of sorted loss record (produced during last leak search).
static LossRecord** lr_array;

// Value of the heuristics parameter used in the current (or last) leak check.
static UInt detect_memory_leaks_last_heuristics;

// DeltaMode used the last time we called detect_memory_leaks.
// The recorded leak errors are output using a logic based on this delta_mode.
// The below avoids replicating the delta_mode in each LossRecord.
LeakCheckDeltaMode MC_(detect_memory_leaks_last_delta_mode);

// Each leak search run increments the below generation counter.
// A used suppression during a leak search will contain this
// generation number.
UInt MC_(leak_search_gen);

// Records chunks that are currently being processed.  Each element in the
// stack is an index into lc_chunks and lc_extras.  Its size is
// 'lc_n_chunks' because in the worst case that's how many chunks could be
// pushed onto it (actually I think the maximum is lc_n_chunks-1 but let's
// be conservative).
static Int* lc_markstack;
// The index of the top element of the stack; -1 if the stack is empty, 0 if
// the stack has one element, 1 if it has two, etc.
static Int  lc_markstack_top;    

// Keeps track of how many bytes of memory we've scanned, for printing.
// (Nb: We don't keep track of how many register bytes we've scanned.)
static SizeT lc_scanned_szB;
// Keeps track of how many bytes we have not scanned due to read errors that
// caused a signal such as SIGSEGV.
static SizeT lc_sig_skipped_szB;


SizeT MC_(bytes_leaked)     = 0;
SizeT MC_(bytes_indirect)   = 0;
SizeT MC_(bytes_dubious)    = 0;
SizeT MC_(bytes_reachable)  = 0;
SizeT MC_(bytes_suppressed) = 0;

SizeT MC_(blocks_leaked)     = 0;
SizeT MC_(blocks_indirect)   = 0;
SizeT MC_(blocks_dubious)    = 0;
SizeT MC_(blocks_reachable)  = 0;
SizeT MC_(blocks_suppressed) = 0;

// Subset of MC_(bytes_reachable) and MC_(blocks_reachable) which
// are considered reachable due to the corresponding heuristic.
static SizeT MC_(bytes_heuristically_reachable)[N_LEAK_CHECK_HEURISTICS]
                                               = {0,0,0,0};
static SizeT MC_(blocks_heuristically_reachable)[N_LEAK_CHECK_HEURISTICS]
                                                = {0,0,0,0};

// Determines if a pointer is to a chunk.  Returns the chunk number et al
// via call-by-reference.
static Bool
lc_is_a_chunk_ptr(Addr ptr, Int* pch_no, MC_Chunk** pch, LC_Extra** pex)
{
   Int ch_no;
   MC_Chunk* ch;
   LC_Extra* ex;

   // Quick filter. Note: implemented with am, not with get_vabits2
   // as ptr might be random data pointing anywhere. On 64 bit
   // platforms, getting va bits for random data can be quite costly
   // due to the secondary map.
   if (!VG_(am_is_valid_for_client)(ptr, 1, VKI_PROT_READ)) {
      return False;
   } else {
      ch_no = find_chunk_for(ptr, lc_chunks, lc_n_chunks);
      tl_assert(ch_no >= -1 && ch_no < lc_n_chunks);

      if (ch_no == -1) {
         return False;
      } else {
         // Ok, we've found a pointer to a chunk.  Get the MC_Chunk and its
         // LC_Extra.
         ch = lc_chunks[ch_no];
         ex = &(lc_extras[ch_no]);

         tl_assert(ptr >= ch->data);
         tl_assert(ptr < ch->data + ch->szB + (ch->szB==0  ? 1  : 0));

         if (VG_DEBUG_LEAKCHECK)
            VG_(printf)("ptr=%#lx -> block %d\n", ptr, ch_no);

         *pch_no = ch_no;
         *pch    = ch;
         *pex    = ex;

         return True;
      }
   }
}

// Push a chunk (well, just its index) onto the mark stack.
static void lc_push(Int ch_no, MC_Chunk* ch)
{
   if (!lc_extras[ch_no].pending) {
      if (0) {
         VG_(printf)("pushing %#lx-%#lx\n", ch->data, ch->data + ch->szB);
      }
      lc_markstack_top++;
      tl_assert(lc_markstack_top < lc_n_chunks);
      lc_markstack[lc_markstack_top] = ch_no;
      tl_assert(!lc_extras[ch_no].pending);
      lc_extras[ch_no].pending = True;
   }
}

// Return the index of the chunk on the top of the mark stack, or -1 if
// there isn't one.
static Bool lc_pop(Int* ret)
{
   if (-1 == lc_markstack_top) {
      return False;
   } else {
      tl_assert(0 <= lc_markstack_top && lc_markstack_top < lc_n_chunks);
      *ret = lc_markstack[lc_markstack_top];
      lc_markstack_top--;
      tl_assert(lc_extras[*ret].pending);
      lc_extras[*ret].pending = False;
      return True;
   }
}

static const HChar* pp_heuristic(LeakCheckHeuristic h)
{
   switch(h) {
   case LchNone:                return "none";
   case LchStdString:           return "stdstring";
   case LchLength64:            return "length64";
   case LchNewArray:            return "newarray";
   case LchMultipleInheritance: return "multipleinheritance";
   default:                     return "???invalid heuristic???";
   }
}

// True if ptr looks like the address of a vtable, i.e. if ptr
// points to an array of pointers to functions.
// It is assumed the only caller of this function is heuristic_reachedness
// which must check that ptr is aligned and above page 0.
// Checking that ptr is above page 0 is an optimisation : it is assumed
// that no vtable is located in the page 0. So, all small integer values
// encountered during the scan will not incur the cost of calling this
// function.
static Bool aligned_ptr_above_page0_is_vtable_addr(Addr ptr)
{
   // ??? If performance problem:
   // ??? maybe implement a cache (array indexed by ptr % primenr)
   // ??? of "I am a vtable ptr" ???

   // ??? Maybe the debug info could (efficiently?) be used to detect vtables ?
   
   // We consider ptr as a vtable ptr if it points to a table
   // where we find only NULL pointers or pointers pointing at an
   // executable region. We must find at least 2 non NULL pointers
   // before considering ptr as a vtable pointer.
   // We scan a maximum of VTABLE_MAX_CHECK words for these 2 non NULL
   // pointers.
#define VTABLE_MAX_CHECK 20 

   NSegment const *seg;
   UInt nr_fn_ptrs = 0;
   Addr scan;
   Addr scan_max;

   // First verify ptr points inside a client mapped file section.
   // ??? is a vtable always in a file mapped readable section ?
   seg = VG_(am_find_nsegment) (ptr);
   if (seg == NULL
       || seg->kind != SkFileC
       || !seg->hasR)
      return False;

   // Check potential function pointers, up to a maximum of VTABLE_MAX_CHECK.
   scan_max = ptr + VTABLE_MAX_CHECK*sizeof(Addr);
   // If ptr is near the end of seg, avoid scan_max exceeding the end of seg:
   if (scan_max > seg->end - sizeof(Addr))
      scan_max = seg->end - sizeof(Addr);
   for (scan = ptr; scan <= scan_max; scan+=sizeof(Addr)) {
      Addr pot_fn = *((Addr *)scan);
      if (pot_fn == 0)
         continue; // NULL fn pointer. Seems it can happen in vtable.
      seg = VG_(am_find_nsegment) (pot_fn);
#if defined(VGA_ppc64be)
      // ppc64BE uses a thunk table (function descriptors), so we have one
      // more level of indirection to follow.
      if (seg == NULL
          || seg->kind != SkFileC
          || !seg->hasR
          || !seg->hasW)
         return False; // ptr to nowhere, or not a ptr to thunks.
      pot_fn = *((Addr *)pot_fn);
      if (pot_fn == 0)
         continue; // NULL fn pointer. Seems it can happen in vtable.
      seg = VG_(am_find_nsegment) (pot_fn);
#endif
      if (seg == NULL
          || seg->kind != SkFileC
          || !seg->hasT)
         return False; // ptr to nowhere, or not a fn ptr.
      nr_fn_ptrs++;
      if (nr_fn_ptrs == 2)
         return True;
   }

   return False;
}

// true if a is properly aligned and points to 64bits of valid memory
static Bool is_valid_aligned_ULong ( Addr a )
{
   if (sizeof(Word) == 8)
      return MC_(is_valid_aligned_word)(a);

   return MC_(is_valid_aligned_word)(a)
      && MC_(is_valid_aligned_word)(a + 4);
}

/* The below leak_search_fault_catcher is used to catch memory access
   errors happening during leak_search.  During the scan, we check
   with aspacemgr and/or VA bits that each page or dereferenced location is
   readable and belongs to the client.  However, we still protect
   against SIGSEGV and SIGBUS e.g. in case aspacemgr is desynchronised
   with the real page mappings.  Such a desynchronisation could happen
   due to an aspacemgr bug.  Note that if the application is using
   mprotect(NONE), then a page can be unreadable but have addressable
   and defined VA bits (see mc_main.c function mc_new_mem_mprotect).
   Currently, 2 functions are dereferencing client memory during leak search:
   heuristic_reachedness and lc_scan_memory.
   Each such function has its own fault catcher, that will call
   leak_search_fault_catcher with the proper 'who' and jmpbuf parameters. */
static volatile Addr bad_scanned_addr;
static
void leak_search_fault_catcher ( Int sigNo, Addr addr,
                                 const HChar *who, VG_MINIMAL_JMP_BUF(jmpbuf) )
{
   vki_sigset_t sigmask;

   if (0)
      VG_(printf)("OUCH! sig=%d addr=%#lx who=%s\n", sigNo, addr, who);

   /* Signal handler runs with the signal masked.
      Unmask the handled signal before longjmp-ing or return-ing.
      Note that during leak search, we expect only SIGSEGV or SIGBUS
      and we do not expect another occurrence until we longjmp-ed!return-ed
      to resume the leak search. So, it is safe to unmask the signal
      here. */
   /* First get current mask (by passing NULL as first arg) */
   VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &sigmask);
   /* Then set a new sigmask, with this signal removed from the mask. */
   VG_(sigdelset)(&sigmask, sigNo);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &sigmask, NULL);

   if (sigNo == VKI_SIGSEGV || sigNo == VKI_SIGBUS) {
      bad_scanned_addr = addr;
      VG_MINIMAL_LONGJMP(jmpbuf);
   } else {
      /* ??? During leak search, we are not supposed to receive any
         other sync signal that these 2.
         In theory, we should not call VG_(umsg) in a signal handler,
         but better (try to) report this unexpected behaviour. */
      VG_(umsg)("leak_search_fault_catcher:"
                " unexpected signal %d, catcher %s ???\n",
                sigNo, who);
   }
}

// jmpbuf and fault_catcher used during heuristic_reachedness
static VG_MINIMAL_JMP_BUF(heuristic_reachedness_jmpbuf);
static
void heuristic_reachedness_fault_catcher ( Int sigNo, Addr addr )
{
   leak_search_fault_catcher (sigNo, addr, 
                              "heuristic_reachedness_fault_catcher",
                              heuristic_reachedness_jmpbuf);
}

// If ch is heuristically reachable via an heuristic member of heur_set,
// returns this heuristic.
// If ch cannot be considered reachable using one of these heuristics,
// return LchNone.
// This should only be called when ptr is an interior ptr to ch.
// The StdString/NewArray/MultipleInheritance heuristics are directly
// inspired from DrMemory:
//  see http://www.burningcutlery.com/derek/docs/drmem-CGO11.pdf [section VI,C]
//  and bug 280271.
static LeakCheckHeuristic heuristic_reachedness (Addr ptr,
                                                 MC_Chunk *ch, LC_Extra *ex,
                                                 UInt heur_set)
{

   fault_catcher_t prev_catcher;

   prev_catcher = VG_(set_fault_catcher)(heuristic_reachedness_fault_catcher);

   // See leak_search_fault_catcher
   if (VG_MINIMAL_SETJMP(heuristic_reachedness_jmpbuf) != 0) {
      VG_(set_fault_catcher) (prev_catcher);
      return LchNone;
   }

   if (HiS(LchStdString, heur_set)) {
      // Detects inner pointers to Std::String for layout being
      //     length capacity refcount char_array[] \0
      // where ptr points to the beginning of the char_array.
      // Note: we check definedness for length and capacity but
      // not for refcount, as refcount size might be smaller than
      // a SizeT, giving a uninitialised hole in the first 3 SizeT.
      if ( ptr == ch->data + 3 * sizeof(SizeT)
           && MC_(is_valid_aligned_word)(ch->data + sizeof(SizeT))) {
         const SizeT capacity = *((SizeT*)(ch->data + sizeof(SizeT)));
         if (3 * sizeof(SizeT) + capacity + 1 == ch->szB
            && MC_(is_valid_aligned_word)(ch->data)) {
            const SizeT length = *((SizeT*)ch->data);
            if (length <= capacity) {
               // ??? could check there is no null byte from ptr to ptr+length-1
               // ???    and that there is a null byte at ptr+length.
               // ???
               // ??? could check that ch->allockind is MC_AllocNew ???
               // ??? probably not a good idea, as I guess stdstring
               // ??? allocator can be done via custom allocator
               // ??? or even a call to malloc ????
               VG_(set_fault_catcher) (prev_catcher);
               return LchStdString;
            }
         }
      }
   }

   if (HiS(LchLength64, heur_set)) {
      // Detects inner pointers that point at 64bit offset (8 bytes) into a
      // block following the length of the remaining as 64bit number 
      // (=total block size - 8).
      // This is used e.g. by sqlite for tracking the total size of allocated
      // memory.
      // Note that on 64bit platforms, a block matching LchLength64 will
      // also be matched by LchNewArray.
      if ( ptr == ch->data + sizeof(ULong)
          && is_valid_aligned_ULong(ch->data)) {
         const ULong size = *((ULong*)ch->data);
         if (size > 0 && (ch->szB - sizeof(ULong)) == size) {
            VG_(set_fault_catcher) (prev_catcher);
            return LchLength64;
         }
      }
   }

   if (HiS(LchNewArray, heur_set)) {
      // Detects inner pointers at second word of new[] array, following
      // a plausible nr of elements.
      // Such inner pointers are used for arrays of elements
      // having a destructor, as the delete[] of the array must know
      // how many elements to destroy.
      //
      // We have a strange/wrong case for 'ptr = new MyClass[0];' :
      // For such a case, the returned ptr points just outside the
      // allocated chunk. This chunk is then seen as a definite
      // leak by Valgrind, as it is not considered an interior pointer.
      // It is the c++ equivalent of bug 99923 (malloc(0) wrongly considered
      // as definitely leaked). See the trick in find_chunk_for handling
      // 0-sized block. This trick does not work for 'new MyClass[0]'
      // because a chunk "word-sized" is allocated to store the (0) nr
      // of elements.
      if ( ptr == ch->data + sizeof(SizeT)
           && MC_(is_valid_aligned_word)(ch->data)) {
         const SizeT nr_elts = *((SizeT*)ch->data);
         if (nr_elts > 0 && (ch->szB - sizeof(SizeT)) % nr_elts == 0) {
            // ??? could check that ch->allockind is MC_AllocNewVec ???
            VG_(set_fault_catcher) (prev_catcher);
            return LchNewArray;
         }
      }
   }

   if (HiS(LchMultipleInheritance, heur_set)) {
      // Detect inner pointer used for multiple inheritance.
      // Assumption is that the vtable pointers are before the object.
      if (VG_IS_WORD_ALIGNED(ptr)
          && MC_(is_valid_aligned_word)(ptr)) {
         Addr first_addr;
         Addr inner_addr;

         // Avoid the call to is_vtable_addr when the addr is not
         // aligned or points in the page0, as it is unlikely
         // a vtable is located in this page. This last optimisation
         // avoids to call aligned_ptr_above_page0_is_vtable_addr
         // for all small integers.
         // Note: we could possibly also avoid calling this function
         // for small negative integers, as no vtable should be located
         // in the last page.
         inner_addr = *((Addr*)ptr);
         if (VG_IS_WORD_ALIGNED(inner_addr) 
             && inner_addr >= (Addr)VKI_PAGE_SIZE
             && MC_(is_valid_aligned_word)(ch->data)) {
            first_addr = *((Addr*)ch->data);
            if (VG_IS_WORD_ALIGNED(first_addr)
                && first_addr >= (Addr)VKI_PAGE_SIZE
                && aligned_ptr_above_page0_is_vtable_addr(inner_addr)
                && aligned_ptr_above_page0_is_vtable_addr(first_addr)) {
               // ??? could check that ch->allockind is MC_AllocNew ???
               VG_(set_fault_catcher) (prev_catcher);
               return LchMultipleInheritance;
            }
         }
      }
   }

   VG_(set_fault_catcher) (prev_catcher);
   return LchNone;
}


// If 'ptr' is pointing to a heap-allocated block which hasn't been seen
// before, push it onto the mark stack.
static void
lc_push_without_clique_if_a_chunk_ptr(Addr ptr, Bool is_prior_definite)
{
   Int ch_no;
   MC_Chunk* ch;
   LC_Extra* ex;
   Reachedness ch_via_ptr; // Is ch reachable via ptr, and how ?

   if ( ! lc_is_a_chunk_ptr(ptr, &ch_no, &ch, &ex) )
      return;

   if (ex->state == Reachable) {
      if (ex->heuristic && ptr == ch->data)
         // If block was considered reachable via an heuristic, and it is now
         // directly reachable via ptr, clear the heuristic field.
         ex->heuristic = LchNone;
      return;
   }
   
   // Possibly upgrade the state, ie. one of:
   // - Unreached --> Possible
   // - Unreached --> Reachable 
   // - Possible  --> Reachable

   if (ptr == ch->data)
      ch_via_ptr = Reachable;
   else if (detect_memory_leaks_last_heuristics) {
      ex->heuristic 
         = heuristic_reachedness (ptr, ch, ex,
                                  detect_memory_leaks_last_heuristics);
      if (ex->heuristic)
         ch_via_ptr = Reachable;
      else
         ch_via_ptr = Possible;
   } else
      ch_via_ptr = Possible;
         
   if (ch_via_ptr == Reachable && is_prior_definite) {
      // 'ptr' points to the start of the block or is to be considered as
      // pointing to the start of the block, and the prior node is
      // definite, which means that this block is definitely reachable.
      ex->state = Reachable;

      // State has changed to Reachable so (re)scan the block to make
      // sure any blocks it points to are correctly marked.
      lc_push(ch_no, ch);

   } else if (ex->state == Unreached) {
      // Either 'ptr' is a interior-pointer, or the prior node isn't definite,
      // which means that we can only mark this block as possibly reachable.
      ex->state = Possible;

      // State has changed to Possible so (re)scan the block to make
      // sure any blocks it points to are correctly marked.
      lc_push(ch_no, ch);
   }
}

static void
lc_push_if_a_chunk_ptr_register(ThreadId tid, const HChar* regname, Addr ptr)
{
   lc_push_without_clique_if_a_chunk_ptr(ptr, /*is_prior_definite*/True);
}

// If ptr is pointing to a heap-allocated block which hasn't been seen
// before, push it onto the mark stack.  Clique is the index of the
// clique leader.
static void
lc_push_with_clique_if_a_chunk_ptr(Addr ptr, Int clique, Int cur_clique)
{
   Int ch_no;
   MC_Chunk* ch;
   LC_Extra* ex;

   tl_assert(0 <= clique && clique < lc_n_chunks);

   if ( ! lc_is_a_chunk_ptr(ptr, &ch_no, &ch, &ex) )
      return;

   // If it's not Unreached, it's already been handled so ignore it.
   // If ch_no==clique, it's the clique leader, which means this is a cyclic
   // structure;  again ignore it because it's already been handled.
   if (ex->state == Unreached && ch_no != clique) {
      // Note that, unlike reachable blocks, we currently don't distinguish
      // between start-pointers and interior-pointers here.  We probably
      // should, though.
      lc_push(ch_no, ch);

      // Add the block to the clique, and add its size to the
      // clique-leader's indirect size.  Also, if the new block was
      // itself a clique leader, it isn't any more, so add its
      // indirect_szB to the new clique leader.
      if (VG_DEBUG_CLIQUE) {
         if (ex->IorC.indirect_szB > 0)
            VG_(printf)("  clique %d joining clique %d adding %lu+%lu\n", 
                        ch_no, clique, (SizeT)ch->szB, ex->IorC.indirect_szB);
         else
            VG_(printf)("  block %d joining clique %d adding %lu\n", 
                        ch_no, clique, (SizeT)ch->szB);
      }

      lc_extras[clique].IorC.indirect_szB += ch->szB;
      lc_extras[clique].IorC.indirect_szB += ex->IorC.indirect_szB;
      ex->state = IndirectLeak;
      ex->IorC.clique = (SizeT) cur_clique;
   }
}

static void
lc_push_if_a_chunk_ptr(Addr ptr,
                       Int clique, Int cur_clique, Bool is_prior_definite)
{
   if (-1 == clique) 
      lc_push_without_clique_if_a_chunk_ptr(ptr, is_prior_definite);
   else
      lc_push_with_clique_if_a_chunk_ptr(ptr, clique, cur_clique);
}


static VG_MINIMAL_JMP_BUF(lc_scan_memory_jmpbuf);
static
void lc_scan_memory_fault_catcher ( Int sigNo, Addr addr )
{
   leak_search_fault_catcher (sigNo, addr, 
                              "lc_scan_memory_fault_catcher",
                              lc_scan_memory_jmpbuf);
}

// lc_scan_memory has 2 modes:
//
// 1. Leak check mode (searched == 0).
// -----------------------------------
// Scan a block of memory between [start, start+len).  This range may
// be bogus, inaccessible, or otherwise strange; we deal with it.  For each
// valid aligned word we assume it's a pointer to a chunk a push the chunk
// onto the mark stack if so.
// clique is the "highest level clique" in which indirectly leaked blocks have
// to be collected. cur_clique is the current "lower" level clique through which
// the memory to be scanned has been found.
// Example: in the below tree if A is leaked, the top level clique will
//   be A, while lower level cliques will be B and C. 
/*
           A
         /   \
        B     C
       / \   / \
      D   E F   G
*/
// Proper handling of top and lowest level clique allows block_list of a loss
// record to describe the hierarchy of indirectly leaked blocks.
//
// 2. Search ptr mode (searched != 0).
// -----------------------------------
// In this mode, searches for pointers to a specific address range 
// In such a case, lc_scan_memory just scans [start..start+len[ for pointers
// to searched and outputs the places where searched is found.
// It does not recursively scans the found memory.
static void
lc_scan_memory(Addr start, SizeT len, Bool is_prior_definite,
               Int clique, Int cur_clique,
               Addr searched, SizeT szB)
{
   /* memory scan is based on the assumption that valid pointers are aligned
      on a multiple of sizeof(Addr). So, we can (and must) skip the begin and
      end portions of the block if they are not aligned on sizeof(Addr):
      These cannot be a valid pointer, and calls to MC_(is_valid_aligned_word)
      will assert for a non aligned address. */
#if defined(VGA_s390x)
   // Define ptr as volatile, as on this platform, the value of ptr
   // is read in code executed via a longjmp.
   volatile
#endif
   Addr ptr = VG_ROUNDUP(start, sizeof(Addr));
   const Addr end = VG_ROUNDDN(start+len, sizeof(Addr));
   fault_catcher_t prev_catcher;

   if (VG_DEBUG_LEAKCHECK)
      VG_(printf)("scan %#lx-%#lx (%lu)\n", start, end, len);

   prev_catcher = VG_(set_fault_catcher)(lc_scan_memory_fault_catcher);

   /* Optimisation: the loop below will check for each begin
      of SM chunk if the chunk is fully unaddressable. The idea is to
      skip efficiently such fully unaddressable SM chunks.
      So, we preferably start the loop on a chunk boundary.
      If the chunk is not fully unaddressable, we might be in
      an unaddressable page. Again, the idea is to skip efficiently
      such unaddressable page : this is the "else" part.
      We use an "else" so that two consecutive fully unaddressable
      SM chunks will be skipped efficiently: first one is skipped
      by this piece of code. The next SM chunk will be skipped inside
      the loop. */
   if ( ! MC_(is_within_valid_secondary)(ptr) ) {
      // Skip an invalid SM chunk till the beginning of the next SM Chunk.
      ptr = VG_ROUNDUP(ptr+1, SM_SIZE);
   } else if (!VG_(am_is_valid_for_client)(ptr, sizeof(Addr), VKI_PROT_READ)) {
      // else we are in a (at least partially) valid SM chunk.
      // We might be in the middle of an unreadable page.
      // Do a cheap check to see if it's valid;
      // if not, skip onto the next page.
      ptr = VG_PGROUNDUP(ptr+1);        // First page is bad - skip it.
   }
   /* The above optimisation and below loop is based on some relationships
      between VKI_PAGE_SIZE, SM_SIZE and sizeof(Addr) which are asserted in
      MC_(detect_memory_leaks). */

   // See leak_search_fault_catcher
   if (VG_MINIMAL_SETJMP(lc_scan_memory_jmpbuf) != 0) {
      // Catch read error ...
#     if defined(VGA_s390x)
      // For a SIGSEGV, s390 delivers the page address of the bad address.
      // For a SIGBUS, old s390 kernels deliver a NULL address.
      // bad_scanned_addr can thus not be used.
      // So, on this platform, we always skip a full page from ptr.
      // The below implies to mark ptr as volatile, as we read the value
      // after a longjmp to here.
      lc_sig_skipped_szB += VKI_PAGE_SIZE;
      ptr = ptr + VKI_PAGE_SIZE; // Unaddressable, - skip it.
#     else
      // On other platforms, just skip one Addr.
      lc_sig_skipped_szB += sizeof(Addr);
      // PJF asserts are always on
      // coverity[ASSERT_SIDE_EFFECT:FALSE]
      tl_assert(bad_scanned_addr >= VG_ROUNDUP(start, sizeof(Addr)));
      // coverity[ASSERT_SIDE_EFFECT:FALSE]
      tl_assert(bad_scanned_addr < VG_ROUNDDN(start+len, sizeof(Addr)));
      ptr = bad_scanned_addr + sizeof(Addr); // Unaddressable, - skip it.
#endif
   }
   while (ptr < end) {
      Addr addr;

      // Skip invalid chunks.
      if (UNLIKELY((ptr % SM_SIZE) == 0)) {
         if (! MC_(is_within_valid_secondary)(ptr) ) {
            ptr = VG_ROUNDUP(ptr+1, SM_SIZE);
            continue;
         }
      }

      // Look to see if this page seems reasonable.
      if (UNLIKELY((ptr % VKI_PAGE_SIZE) == 0)) {
         if (!VG_(am_is_valid_for_client)(ptr, sizeof(Addr), VKI_PROT_READ)) {
            ptr += VKI_PAGE_SIZE;      // Bad page - skip it.
            continue;
         }
      }

      if ( MC_(is_valid_aligned_word)(ptr) ) {
         lc_scanned_szB += sizeof(Addr);
         // If the below read fails, we will longjmp to the loop begin.
         addr = *(Addr *)ptr;
         // If we get here, the scanned word is in valid memory.  Now
         // let's see if its contents point to a chunk.
         if (UNLIKELY(searched)) {
            if (addr >= searched && addr < searched + szB) {
               const DiEpoch cur_ep = VG_(current_DiEpoch)();
               // The found addr is 'live', so use cur_ep to describe it.
               if (addr == searched) {
                  VG_(umsg)("*%#lx points at %#lx\n", ptr, searched);
                  MC_(pp_describe_addr) (cur_ep, ptr);
               } else {
                  Int ch_no;
                  MC_Chunk *ch;
                  LC_Extra *ex;
                  VG_(umsg)("*%#lx interior points at %lu bytes inside %#lx\n",
                            ptr, (long unsigned) addr - searched, searched);
                  MC_(pp_describe_addr) (cur_ep, ptr);
                  if (lc_is_a_chunk_ptr(addr, &ch_no, &ch, &ex) ) {
                     Int h;
                     for (h = LchStdString; h < N_LEAK_CHECK_HEURISTICS; h++) {
                        if (heuristic_reachedness(addr, ch, ex, H2S(h)) == h) {
                           VG_(umsg)("block at %#lx considered reachable "
                                     "by ptr %#lx using %s heuristic\n",
                                     ch->data, addr, pp_heuristic(h));
                        }
                     }
                     // Verify the loop above has properly scanned all
                     // heuristics. If the below fails, it probably means the
                     // LeakCheckHeuristic enum is not in sync anymore with the
                     // above loop and/or with N_LEAK_CHECK_HEURISTICS.
                     tl_assert (h == N_LEAK_CHECK_HEURISTICS);
                  }
               }
            }
         } else {
            lc_push_if_a_chunk_ptr(addr, clique, cur_clique, is_prior_definite);
         }
      } else if (0 && VG_DEBUG_LEAKCHECK) {
         VG_(printf)("%#lx not valid\n", ptr);
      }
      ptr += sizeof(Addr);
   }

   VG_(set_fault_catcher)(prev_catcher);
}


// Process the mark stack until empty.
static void lc_process_markstack(Int clique)
{
   Int  top = -1;    // shut gcc up
   Bool is_prior_definite;

   while (lc_pop(&top)) {
      tl_assert(top >= 0 && top < lc_n_chunks);

      // See comment about 'is_prior_definite' at the top to understand this.
      is_prior_definite = ( Possible != lc_extras[top].state );

      lc_scan_memory(lc_chunks[top]->data, lc_chunks[top]->szB,
                     is_prior_definite, clique, (clique == -1 ? -1 : top),
                     /*searched*/ 0, 0);
   }
}

static Word cmp_LossRecordKey_LossRecord(const void* key, const void* elem)
{
   const LossRecordKey* a = key;
   const LossRecordKey* b = &(((const LossRecord*)elem)->key);

   // Compare on states first because that's fast.
   if (a->state < b->state) return -1;
   if (a->state > b->state) return  1;
   // Ok, the states are equal.  Now compare the locations, which is slower.
   if (VG_(eq_ExeContext)(
            MC_(clo_leak_resolution), a->allocated_at, b->allocated_at))
      return 0;
   // Different locations.  Ordering is arbitrary, just use the ec pointer.
   if (a->allocated_at < b->allocated_at) return -1;
   if (a->allocated_at > b->allocated_at) return  1;
   VG_(tool_panic)("bad LossRecord comparison");
}

static Int cmp_LossRecords(const void* va, const void* vb)
{
   const LossRecord* lr_a = *(const LossRecord *const *)va;
   const LossRecord* lr_b = *(const LossRecord *const *)vb;
   SizeT total_szB_a = lr_a->szB + lr_a->indirect_szB;
   SizeT total_szB_b = lr_b->szB + lr_b->indirect_szB;

   // First compare by sizes.
   if (total_szB_a < total_szB_b) return -1;
   if (total_szB_a > total_szB_b) return  1;
   // If size are equal, compare by states.
   if (lr_a->key.state < lr_b->key.state) return -1;
   if (lr_a->key.state > lr_b->key.state) return  1;
   // If they're still equal here, it doesn't matter that much, but we keep
   // comparing other things so that regtests are as deterministic as
   // possible.  So:  compare num_blocks.
   if (lr_a->num_blocks < lr_b->num_blocks) return -1;
   if (lr_a->num_blocks > lr_b->num_blocks) return  1;
   // Finally, compare ExeContext addresses... older ones are likely to have
   // lower addresses.
   if (lr_a->key.allocated_at < lr_b->key.allocated_at) return -1;
   if (lr_a->key.allocated_at > lr_b->key.allocated_at) return  1;
   return 0;
}

// allocates or reallocates lr_array, and set its elements to the loss records
// contains in lr_table.
static UInt get_lr_array_from_lr_table(void) {
   UInt         i, n_lossrecords;
   LossRecord*  lr;

   n_lossrecords = VG_(OSetGen_Size)(lr_table);

   // (re-)create the array of pointers to the loss records.
   // lr_array is kept to allow producing the block list from gdbserver.
   if (lr_array != NULL)
      VG_(free)(lr_array);
   lr_array = VG_(malloc)("mc.pr.2", n_lossrecords * sizeof(LossRecord*));
   i = 0;
   VG_(OSetGen_ResetIter)(lr_table);
   while ( (lr = VG_(OSetGen_Next)(lr_table)) ) {
      lr_array[i++] = lr;
   }
   tl_assert(i == n_lossrecords);
   return n_lossrecords;
}


static void get_printing_rules(LeakCheckParams* lcp,
                               LossRecord*  lr,
                               Bool* count_as_error,
                               Bool* print_record)
{
   // Rules for printing:
   // - We don't show suppressed loss records ever (and that's controlled
   //   within the error manager).
   // - We show non-suppressed loss records that are specified in
   //   --show-leak-kinds=... if --leak-check=yes.

   Bool delta_considered;

   switch (lcp->deltamode) {
   case LCD_Any:
      delta_considered = lr->num_blocks > 0;
      break;
   case LCD_Increased:
      delta_considered
         = lr->szB > lr->old_szB
         || lr->indirect_szB > lr->old_indirect_szB
         || lr->num_blocks > lr->old_num_blocks;
      break;
   case LCD_Changed:
      delta_considered = lr->szB != lr->old_szB
         || lr->indirect_szB != lr->old_indirect_szB
         || lr->num_blocks != lr->old_num_blocks;
      break;
   case LCD_New:
      delta_considered
         = lr->num_blocks > 0 && lr->old_num_blocks == 0;
      break;
   default:
      tl_assert(0);
   }

   *print_record = lcp->mode == LC_Full && delta_considered
      && RiS(lr->key.state,lcp->show_leak_kinds);
   // We don't count a leaks as errors with lcp->mode==LC_Summary.
   // Otherwise you can get high error counts with few or no error
   // messages, which can be confusing.  Otherwise, we count as errors
   // the leak kinds requested by --errors-for-leak-kinds=...
   *count_as_error = lcp->mode == LC_Full && delta_considered 
      && RiS(lr->key.state,lcp->errors_for_leak_kinds);
}

//
// Types and functions for xtree leak report.
//

static XTree* leak_xt;

/* Sizes and delta sizes for a loss record output in an xtree.
   As the output format can only show positive values, we need values for
   the increase and decrease cases. */
typedef
   struct _XT_BIBK {
      ULong szB;           // Current values
      ULong indirect_szB;
      ULong num_blocks;
   } XT_BIBK; // Bytes, Indirect bytes, BlocKs

typedef 
   enum { 
      XT_Value    =0,
      XT_Increase =1,
      XT_Decrease =2
  }
  XT_VID; // Value or Increase or Decrease

typedef
   struct _XT_lr {
      XT_BIBK vid[3]; // indexed by XT_VID
   } XT_lr;

typedef
   struct _XT_Leak {
      XT_lr xt_lr[4]; // indexed by Reachedness
   } XT_Leak;

static void MC_(XT_Leak_init)(void* xtl)
{
   VG_(memset) (xtl, 0, sizeof(XT_Leak));
}
static void MC_(XT_Leak_add) (void* to, const void* xtleak)
{
   XT_Leak* xto = to;
   const XT_Leak* xtl = xtleak;

   for (int r = Reachable; r <= Unreached; r++)
      for (int d = 0; d < 3; d++) {
         xto->xt_lr[r].vid[d].szB += xtl->xt_lr[r].vid[d].szB;
         xto->xt_lr[r].vid[d].indirect_szB += xtl->xt_lr[r].vid[d].indirect_szB;
         xto->xt_lr[r].vid[d].num_blocks += xtl->xt_lr[r].vid[d].num_blocks;
      }
}
static void XT_insert_lr (LossRecord* lr)
{
   XT_Leak xtl;
   Reachedness i = lr->key.state;

   MC_(XT_Leak_init)(&xtl);
   
   xtl.xt_lr[i].vid[XT_Value].szB = lr->szB;
   xtl.xt_lr[i].vid[XT_Value].indirect_szB = lr->indirect_szB;
   xtl.xt_lr[i].vid[XT_Value].num_blocks = lr->num_blocks;

   if (lr->szB > lr->old_szB)
      xtl.xt_lr[i].vid[XT_Increase].szB = lr->szB - lr->old_szB;
   else
      xtl.xt_lr[i].vid[XT_Decrease].szB = lr->old_szB - lr->szB;
   if (lr->indirect_szB > lr->old_indirect_szB)
      xtl.xt_lr[i].vid[XT_Increase].indirect_szB 
         = lr->indirect_szB - lr->old_indirect_szB;
   else
      xtl.xt_lr[i].vid[XT_Decrease].indirect_szB 
         = lr->old_indirect_szB - lr->indirect_szB;
   if (lr->num_blocks > lr->old_num_blocks)
      xtl.xt_lr[i].vid[XT_Increase].num_blocks 
         = lr->num_blocks - lr->old_num_blocks;
   else
      xtl.xt_lr[i].vid[XT_Decrease].num_blocks 
         = lr->old_num_blocks - lr->num_blocks;

   VG_(XT_add_to_ec)(leak_xt, lr->key.allocated_at, &xtl);
}

static void MC_(XT_Leak_sub) (void* from, const void* xtleak)
{
   tl_assert(0); // Should not be called.
}
static const HChar* MC_(XT_Leak_img) (const void* xtleak)
{
   static XT_Leak zero;
   static HChar buf[600];
   UInt off = 0;

   const XT_Leak* xtl = xtleak;

   if (VG_(memcmp)(xtl, &zero, sizeof(XT_Leak)) != 0) {
      for (UInt d = XT_Value; d <= XT_Decrease; d++) {
         // print szB. We add indirect_szB to have the Unreachable showing
         // the total bytes loss, including indirect loss. This is similar
         // to the textual and xml reports.
         for (UInt r = Reachable; r <= Unreached; r++)
            off += VG_(sprintf) (buf + off, " %llu",
                                 xtl->xt_lr[r].vid[d].szB
                                   + xtl->xt_lr[r].vid[d].indirect_szB);
         // print indirect_szB, only for reachedness having such values)
         for (UInt r = Reachable; r <= Unreached; r++)
            if (r == Unreached)
               off += VG_(sprintf) (buf + off, " %llu",
                                    xtl->xt_lr[r].vid[d].indirect_szB);
         // print num_blocks
         for (UInt r = Reachable; r <= Unreached; r++)
            off += VG_(sprintf) (buf + off, " %llu",
                                 xtl->xt_lr[r].vid[d].num_blocks);
      }
      return buf + 1; // + 1 to skip the useless first space
   } else {
      return NULL;
   }
}

/* The short event name is made of 2 or 3 or 4 letters:
     an optional delta indication:  i = increase  d = decrease
     a loss kind: R = Reachable P = Possibly I = Indirectly D = Definitely
     an optional i to indicate this loss record has indirectly lost bytes
     B = Bytes or Bk = Blocks.
   Note that indirectly lost bytes/blocks can thus be counted in 2
   loss records: the loss records for their "own" allocation stack trace,
   and the loss record of the 'main' Definitely or Possibly loss record
   in the indirectly lost count for these loss records. */
static const HChar* XT_Leak_events = 
   ////// XT_Value szB
   "RB : Reachable Bytes"                                  ","
   "PB : Possibly lost Bytes"                              ","
   "IB : Indirectly lost Bytes"                            ","
   "DB : Definitely lost Bytes (direct plus indirect)"     ","

   ////// XT_Value indirect_szB
   // no RIB
   // no PIB
   // no IIB 
   "DIB : Definitely Indirectly lost Bytes (subset of DB)" ","

   ////// XT_Value num_blocks 
   "RBk : reachable Blocks"                                ","
   "PBk : Possibly lost Blocks"                            ","
   "IBk : Indirectly lost Blocks"                          ","
   "DBk : Definitely lost Blocks"                          ","

   ////// XT_Increase szB
   "iRB : increase Reachable Bytes"                        ","
   "iPB : increase Possibly lost Bytes"                    ","
   "iIB : increase Indirectly lost Bytes"                  ","
   "iDB : increase Definitely lost Bytes"                  ","

   ////// XT_Increase indirect_szB
   // no iRIB
   // no iPIB
   // no iIIB
   "iDIB : increase Definitely Indirectly lost Bytes"      ","

   ////// XT_Increase num_blocks
   "iRBk : increase reachable Blocks"                      ","
   "iPBk : increase Possibly lost Blocks"                  ","
   "iIBk : increase Indirectly lost Blocks"                ","
   "iDBk : increase Definitely lost Blocks"                ","


   ////// XT_Decrease szB
   "dRB : decrease Reachable Bytes"                        ","
   "dPB : decrease Possibly lost Bytes"                    ","
   "dIB : decrease Indirectly lost Bytes"                  ","
   "dDB : decrease Definitely lost Bytes"                  ","

   ////// XT_Decrease indirect_szB
   // no dRIB
   // no dPIB
   // no dIIB
   "dDIB : decrease Definitely Indirectly lost Bytes"      ","

   ////// XT_Decrease num_blocks
   "dRBk : decrease reachable Blocks"                      ","
   "dPBk : decrease Possibly lost Blocks"                  ","
   "dIBk : decrease Indirectly lost Blocks"                ","
   "dDBk : decrease Definitely lost Blocks";

static void print_results(ThreadId tid, LeakCheckParams* lcp)
{
   Int          i, n_lossrecords, start_lr_output_scan;
   LossRecord*  lr;
   Bool         is_suppressed;
   /* old_* variables are used to report delta in summary.  */
   SizeT        old_bytes_leaked      = MC_(bytes_leaked);
   SizeT        old_bytes_indirect    = MC_(bytes_indirect); 
   SizeT        old_bytes_dubious     = MC_(bytes_dubious); 
   SizeT        old_bytes_reachable   = MC_(bytes_reachable); 
   SizeT        old_bytes_suppressed  = MC_(bytes_suppressed); 
   SizeT        old_blocks_leaked     = MC_(blocks_leaked);
   SizeT        old_blocks_indirect   = MC_(blocks_indirect);
   SizeT        old_blocks_dubious    = MC_(blocks_dubious);
   SizeT        old_blocks_reachable  = MC_(blocks_reachable);
   SizeT        old_blocks_suppressed = MC_(blocks_suppressed);

   SizeT old_bytes_heuristically_reachable[N_LEAK_CHECK_HEURISTICS];
   SizeT old_blocks_heuristically_reachable[N_LEAK_CHECK_HEURISTICS];

   for (i = 0; i < N_LEAK_CHECK_HEURISTICS; i++) {
      old_bytes_heuristically_reachable[i]   
         =  MC_(bytes_heuristically_reachable)[i];
      MC_(bytes_heuristically_reachable)[i] = 0;
      old_blocks_heuristically_reachable[i]  
         =  MC_(blocks_heuristically_reachable)[i];
      MC_(blocks_heuristically_reachable)[i] = 0;
   }

   if (lr_table == NULL)
      // Create the lr_table, which holds the loss records.
      // If the lr_table already exists, it means it contains
      // loss_records from the previous leak search. The old_*
      // values in these records are used to implement the
      // leak check delta mode
      lr_table =
         VG_(OSetGen_Create)(offsetof(LossRecord, key),
                             cmp_LossRecordKey_LossRecord,
                             VG_(malloc), "mc.pr.1",
                             VG_(free));

   // If we have loss records from a previous search, reset values to have
   // proper printing of the deltas between previous search and this search.
   n_lossrecords = get_lr_array_from_lr_table();
   for (i = 0; i < n_lossrecords; i++) {
      if (lr_array[i]->num_blocks == 0) {
         // remove from lr_table the old loss_records with 0 bytes found
         VG_(OSetGen_Remove) (lr_table, &lr_array[i]->key);
         VG_(OSetGen_FreeNode)(lr_table, lr_array[i]);
      } else {
         // move the leak sizes to old_* and zero the current sizes
         // for next leak search
         lr_array[i]->old_szB          = lr_array[i]->szB;
         lr_array[i]->old_indirect_szB = lr_array[i]->indirect_szB;
         lr_array[i]->old_num_blocks   = lr_array[i]->num_blocks;
         lr_array[i]->szB              = 0;
         lr_array[i]->indirect_szB     = 0;
         lr_array[i]->num_blocks       = 0;
      }
   }
   // lr_array now contains "invalid" loss records => free it.
   // lr_array will be re-created below with the kept and new loss records.
   VG_(free) (lr_array);
   lr_array = NULL;

   // Convert the chunks into loss records, merging them where appropriate.
   for (i = 0; i < lc_n_chunks; i++) {
      MC_Chunk*     ch = lc_chunks[i];
      LC_Extra*     ex = &(lc_extras)[i];
      LossRecord*   old_lr;
      LossRecordKey lrkey;
      lrkey.state        = ex->state;
      lrkey.allocated_at = MC_(allocated_at)(ch);

     if (ex->heuristic) {
        MC_(bytes_heuristically_reachable)[ex->heuristic] += ch->szB;
        MC_(blocks_heuristically_reachable)[ex->heuristic]++;
        if (VG_DEBUG_LEAKCHECK)
           VG_(printf)("heuristic %s %#lx len %lu\n",
                       pp_heuristic(ex->heuristic),
                       ch->data, (SizeT)ch->szB);
     }

      old_lr = VG_(OSetGen_Lookup)(lr_table, &lrkey);
      if (old_lr) {
         // We found an existing loss record matching this chunk.  Update the
         // loss record's details in-situ.  This is safe because we don't
         // change the elements used as the OSet key.
         old_lr->szB          += ch->szB;
         if (ex->state == Unreached)
            old_lr->indirect_szB += ex->IorC.indirect_szB;
         old_lr->num_blocks++;
      } else {
         // No existing loss record matches this chunk.  Create a new loss
         // record, initialise it from the chunk, and insert it into lr_table.
         lr = VG_(OSetGen_AllocNode)(lr_table, sizeof(LossRecord));
         lr->key              = lrkey;
         lr->szB              = ch->szB;
         if (ex->state == Unreached)
            lr->indirect_szB     = ex->IorC.indirect_szB;
         else
            lr->indirect_szB     = 0;
         lr->num_blocks       = 1;
         lr->old_szB          = 0;
         lr->old_indirect_szB = 0;
         lr->old_num_blocks   = 0;
         VG_(OSetGen_Insert)(lr_table, lr);
      }
   }

   // (re-)create the array of pointers to the (new) loss records.
   n_lossrecords = get_lr_array_from_lr_table ();
   tl_assert(VG_(OSetGen_Size)(lr_table) == n_lossrecords);

   // Sort the array by loss record sizes.
   VG_(ssort)(lr_array, n_lossrecords, sizeof(LossRecord*),
              cmp_LossRecords);

   // Zero totals.
   MC_(blocks_leaked)     = MC_(bytes_leaked)     = 0;
   MC_(blocks_indirect)   = MC_(bytes_indirect)   = 0;
   MC_(blocks_dubious)    = MC_(bytes_dubious)    = 0;
   MC_(blocks_reachable)  = MC_(bytes_reachable)  = 0;
   MC_(blocks_suppressed) = MC_(bytes_suppressed) = 0;

   // If there is a maximum nr of loss records we can output, then first
   // compute from where the output scan has to start.
   // By default, start from the first loss record. Compute a higher
   // value if there is a maximum to respect. We need to print the last
   // records, as the one with the biggest sizes are more interesting.
   start_lr_output_scan = 0;
   if (lcp->mode == LC_Full && lcp->max_loss_records_output < n_lossrecords) {
      Int nr_printable_records = 0;
      for (i = n_lossrecords - 1; i >= 0 && start_lr_output_scan == 0; i--) {
         Bool count_as_error, print_record;
         lr = lr_array[i];
         get_printing_rules (lcp, lr, &count_as_error, &print_record);
         // Do not use get_printing_rules results for is_suppressed, as we
         // only want to check if the record would be suppressed.
         is_suppressed = 
            MC_(record_leak_error) ( tid, i+1, n_lossrecords, lr, 
                                     False /* print_record */,
                                     False /* count_as_error */);
         if (print_record && !is_suppressed) {
            nr_printable_records++;
            if (nr_printable_records == lcp->max_loss_records_output)
               start_lr_output_scan = i;
         }
      }
   }

   if (lcp->xt_filename != NULL)
      leak_xt = VG_(XT_create) (VG_(malloc),
                                "mc_leakcheck.leak_xt",
                                VG_(free),
                                sizeof(XT_Leak),
                                MC_(XT_Leak_init),
                                MC_(XT_Leak_add),
                                MC_(XT_Leak_sub),
                                VG_(XT_filter_maybe_below_main));

   // Print the loss records (in size order) and collect summary stats.
   for (i = start_lr_output_scan; i < n_lossrecords; i++) {
      Bool count_as_error, print_record;
      lr = lr_array[i];
      get_printing_rules(lcp, lr, &count_as_error, &print_record);
      is_suppressed = 
         MC_(record_leak_error) 
           ( tid, i+1, n_lossrecords, lr, 
             lcp->xt_filename == NULL ? print_record : False,
             count_as_error );
      if (lcp->xt_filename != NULL && !is_suppressed && print_record)
         XT_insert_lr (lr);

      if (is_suppressed) {
         MC_(blocks_suppressed) += lr->num_blocks;
         MC_(bytes_suppressed)  += lr->szB;

      } else if (Unreached == lr->key.state) {
         MC_(blocks_leaked)     += lr->num_blocks;
         MC_(bytes_leaked)      += lr->szB;

      } else if (IndirectLeak == lr->key.state) {
         MC_(blocks_indirect)   += lr->num_blocks;
         MC_(bytes_indirect)    += lr->szB;

      } else if (Possible == lr->key.state) {
         MC_(blocks_dubious)    += lr->num_blocks;
         MC_(bytes_dubious)     += lr->szB;

      } else if (Reachable == lr->key.state) {
         MC_(blocks_reachable)  += lr->num_blocks;
         MC_(bytes_reachable)   += lr->szB;

      } else {
         VG_(tool_panic)("unknown loss mode");
      }
   }

   if (lcp->xt_filename != NULL) {
      VG_(XT_callgrind_print)(leak_xt,
                              lcp->xt_filename,
                              XT_Leak_events,
                              MC_(XT_Leak_img));
      if (VG_(clo_verbosity) >= 1 || lcp->requested_by_monitor_command)
         VG_(umsg)("xtree leak report: %s\n", lcp->xt_filename);
      VG_(XT_delete)(leak_xt);
   }

   if (VG_(clo_verbosity) > 0 && !VG_(clo_xml)) {
      HChar d_bytes[31];
      HChar d_blocks[31];
#     define DBY(new,old) \
      MC_(snprintf_delta) (d_bytes, sizeof(d_bytes), (new), (old), \
                           lcp->deltamode)
#     define DBL(new,old) \
      MC_(snprintf_delta) (d_blocks, sizeof(d_blocks), (new), (old), \
                           lcp->deltamode)

      VG_(umsg)("LEAK SUMMARY:\n");
      VG_(umsg)("   definitely lost: %'lu%s bytes in %'lu%s blocks\n",
                MC_(bytes_leaked), 
                DBY (MC_(bytes_leaked), old_bytes_leaked),
                MC_(blocks_leaked),
                DBL (MC_(blocks_leaked), old_blocks_leaked));
      VG_(umsg)("   indirectly lost: %'lu%s bytes in %'lu%s blocks\n",
                MC_(bytes_indirect), 
                DBY (MC_(bytes_indirect), old_bytes_indirect),
                MC_(blocks_indirect),
                DBL (MC_(blocks_indirect), old_blocks_indirect));
      VG_(umsg)("     possibly lost: %'lu%s bytes in %'lu%s blocks\n",
                MC_(bytes_dubious), 
                DBY (MC_(bytes_dubious), old_bytes_dubious), 
                MC_(blocks_dubious),
                DBL (MC_(blocks_dubious), old_blocks_dubious));
      VG_(umsg)("   still reachable: %'lu%s bytes in %'lu%s blocks\n",
                MC_(bytes_reachable), 
                DBY (MC_(bytes_reachable), old_bytes_reachable), 
                MC_(blocks_reachable),
                DBL (MC_(blocks_reachable), old_blocks_reachable));
      for (i = 0; i < N_LEAK_CHECK_HEURISTICS; i++)
         if (old_blocks_heuristically_reachable[i] > 0 
             || MC_(blocks_heuristically_reachable)[i] > 0) {
            VG_(umsg)("                      of which "
                      "reachable via heuristic:\n");
            break;
         }
      for (i = 0; i < N_LEAK_CHECK_HEURISTICS; i++)
         if (old_blocks_heuristically_reachable[i] > 0 
             || MC_(blocks_heuristically_reachable)[i] > 0)
            VG_(umsg)("                        %-19s: "
                      "%'lu%s bytes in %'lu%s blocks\n",
                      pp_heuristic(i),
                      MC_(bytes_heuristically_reachable)[i], 
                      DBY (MC_(bytes_heuristically_reachable)[i],
                           old_bytes_heuristically_reachable[i]), 
                      MC_(blocks_heuristically_reachable)[i],
                      DBL (MC_(blocks_heuristically_reachable)[i],
                           old_blocks_heuristically_reachable[i]));
      VG_(umsg)("        suppressed: %'lu%s bytes in %'lu%s blocks\n",
                MC_(bytes_suppressed), 
                DBY (MC_(bytes_suppressed), old_bytes_suppressed), 
                MC_(blocks_suppressed),
                DBL (MC_(blocks_suppressed), old_blocks_suppressed));
      if (lcp->mode != LC_Full &&
          (MC_(blocks_leaked) + MC_(blocks_indirect) +
           MC_(blocks_dubious) + MC_(blocks_reachable)) > 0) {
         if (lcp->requested_by_monitor_command)
            VG_(umsg)("To see details of leaked memory, "
                      "give 'full' arg to leak_check\n");
         else
            VG_(umsg)("Rerun with --leak-check=full to see details "
                      "of leaked memory\n");
      }
      if (lcp->mode == LC_Full &&
          MC_(blocks_reachable) > 0 && !RiS(Reachable,lcp->show_leak_kinds)) {
         VG_(umsg)("Reachable blocks (those to which a pointer "
                   "was found) are not shown.\n");
         if (lcp->requested_by_monitor_command)
            VG_(umsg)("To see them, add 'reachable any' args to leak_check\n");
         else
            VG_(umsg)("To see them, rerun with: --leak-check=full "
                      "--show-leak-kinds=all\n");
      }
      VG_(umsg)("\n");
      #undef DBL
      #undef DBY
   }
}

// print recursively all indirectly leaked blocks collected in clique.
// Printing stops when *remaining reaches 0.
static void print_clique (Int clique, UInt level, UInt *remaining)
{
   Int ind;
   UInt i,  n_lossrecords;

   n_lossrecords = VG_(OSetGen_Size)(lr_table);

   for (ind = 0; ind < lc_n_chunks && *remaining > 0; ind++) {
      LC_Extra*     ind_ex = &(lc_extras)[ind];
      if (ind_ex->state == IndirectLeak 
          && ind_ex->IorC.clique == (SizeT) clique) {
         MC_Chunk*    ind_ch = lc_chunks[ind];
         LossRecord*  ind_lr;
         LossRecordKey ind_lrkey;
         UInt lr_i;
         ind_lrkey.state = ind_ex->state;
         ind_lrkey.allocated_at = MC_(allocated_at)(ind_ch);
         ind_lr = VG_(OSetGen_Lookup)(lr_table, &ind_lrkey);
         for (lr_i = 0; lr_i < n_lossrecords; lr_i++)
            if (ind_lr == lr_array[lr_i])
               break;
         for (i = 0; i < level; i++)
            VG_(umsg)("  ");
         VG_(umsg)("%p[%lu] indirect loss record %u\n",
                   (void *)ind_ch->data, (SizeT)ind_ch->szB,
                   lr_i+1); // lr_i+1 for user numbering.
         (*remaining)--;
         if (lr_i >= n_lossrecords)
            VG_(umsg)
               ("error: no indirect loss record found for %p[%lu]?????\n",
                (void *)ind_ch->data, (SizeT)ind_ch->szB);
         print_clique(ind, level+1, remaining);
      }
   }
 }

Bool MC_(print_block_list) ( UInt loss_record_nr_from,
                             UInt loss_record_nr_to,
                             UInt max_blocks,
                             UInt heuristics)
{
   UInt loss_record_nr;
   UInt         i,  n_lossrecords;
   LossRecord*  lr;
   Bool lr_printed;
   UInt remaining = max_blocks;

   if (lr_table == NULL || lc_chunks == NULL || lc_extras == NULL) {
      VG_(umsg)("Can't print block list : no valid leak search result\n");
      return False;
   }

   if (lc_chunks_n_frees_marker != MC_(get_cmalloc_n_frees)()) {
      VG_(umsg)("Can't print obsolete block list : redo a leak search first\n");
      return False;
   }

   n_lossrecords = VG_(OSetGen_Size)(lr_table);
   if (loss_record_nr_from >= n_lossrecords)
      return False; // Invalid starting loss record nr.

   if (loss_record_nr_to >= n_lossrecords)
      loss_record_nr_to = n_lossrecords - 1;

   tl_assert (lr_array);

   for (loss_record_nr = loss_record_nr_from;
        loss_record_nr <= loss_record_nr_to && remaining > 0;
        loss_record_nr++) {
      lr = lr_array[loss_record_nr];
      lr_printed = False;

      /* If user asks to print a specific loss record, we print
         the block details, even if no block will be shown for this lr.
         If user asks to print a range of lr, we only print lr details
         when at least one block is shown. */
      if (loss_record_nr_from == loss_record_nr_to) {
         /* (+1 on loss_record_nr as user numbering for loss records
            starts at 1). */
         MC_(pp_LossRecord)(loss_record_nr+1, n_lossrecords, lr);
         lr_printed = True;
      }
   
      // Match the chunks with loss records.
      for (i = 0; i < lc_n_chunks && remaining > 0; i++) {
         MC_Chunk*     ch = lc_chunks[i];
         LC_Extra*     ex = &(lc_extras)[i];
         LossRecord*   old_lr;
         LossRecordKey lrkey;
         lrkey.state        = ex->state;
         lrkey.allocated_at = MC_(allocated_at)(ch);

         old_lr = VG_(OSetGen_Lookup)(lr_table, &lrkey);
         if (old_lr) {
            // We found an existing loss record matching this chunk.
            // If this is the loss record we are looking for, output the
            // pointer.
            if (old_lr == lr_array[loss_record_nr]
                && (heuristics == 0 || HiS(ex->heuristic, heuristics))) {
               if (!lr_printed) {
                  MC_(pp_LossRecord)(loss_record_nr+1, n_lossrecords, lr);
                  lr_printed = True;
               }

               if (ex->heuristic)
                  VG_(umsg)("%p[%lu] (found via heuristic %s)\n",
                            (void *)ch->data, (SizeT)ch->szB,
                            pp_heuristic (ex->heuristic));
               else
                  VG_(umsg)("%p[%lu]\n",
                            (void *)ch->data, (SizeT)ch->szB);
               remaining--;
               if (ex->state != Reachable) {
                  // We can print the clique in all states, except Reachable.
                  // In Unreached state, lc_chunk[i] is the clique leader.
                  // In IndirectLeak, lc_chunk[i] might have been a clique
                  // leader which was later collected in another clique.
                  // For Possible, lc_chunk[i] might be the top of a clique
                  // or an intermediate clique.
                  print_clique(i, 1, &remaining);
               }
            }
         } else {
            // No existing loss record matches this chunk ???
            VG_(umsg)("error: no loss record found for %p[%lu]?????\n",
                      (void *)ch->data, (SizeT)ch->szB);
         }
      }
   }
   return True;
}

// If searched = 0, scan memory root set, pushing onto the mark stack the blocks
// encountered.
// Otherwise (searched != 0), scan the memory root set searching for ptr
// pointing inside [searched, searched+szB[.
static void scan_memory_root_set(Addr searched, SizeT szB)
{
   Int   i;
   Int   n_seg_starts;
   Addr* seg_starts = VG_(get_segment_starts)( SkFileC | SkAnonC | SkShmC,
                                               &n_seg_starts );

   tl_assert(seg_starts && n_seg_starts > 0);

   lc_scanned_szB = 0;
   lc_sig_skipped_szB = 0;

   // VG_(am_show_nsegments)( 0, "leakcheck");
   for (i = 0; i < n_seg_starts; i++) {
      SizeT seg_size;
      NSegment const* seg = VG_(am_find_nsegment)( seg_starts[i] );
      tl_assert(seg);
      tl_assert(seg->kind == SkFileC || seg->kind == SkAnonC ||
                seg->kind == SkShmC);

      if (!(seg->hasR && seg->hasW))                    continue;
      if (seg->isCH)                                    continue;

      // Don't poke around in device segments as this may cause
      // hangs.  Include /dev/zero just in case someone allocated
      // memory by explicitly mapping /dev/zero.
      if (seg->kind == SkFileC 
          && (VKI_S_ISCHR(seg->mode) || VKI_S_ISBLK(seg->mode))) {
         const HChar* dev_name = VG_(am_get_filename)( seg );
         if (dev_name && 0 == VG_(strcmp)(dev_name, "/dev/zero")) {
            // Don't skip /dev/zero.
         } else {
            // Skip this device mapping.
            continue;
         }
      }

      if (0)
         VG_(printf)("ACCEPT %2d  %#lx %#lx\n", i, seg->start, seg->end);

      // Scan the segment.  We use -1 for the clique number, because this
      // is a root-set.
      seg_size = seg->end - seg->start + 1;
      if (VG_(clo_verbosity) > 2) {
         VG_(message)(Vg_DebugMsg,
                      "  Scanning root segment: %#lx..%#lx (%lu)\n",
                      seg->start, seg->end, seg_size);
      }
      lc_scan_memory(seg->start, seg_size, /*is_prior_definite*/True,
                     /*clique*/-1, /*cur_clique*/-1,
                     searched, szB);
   }
   VG_(free)(seg_starts);
}

static MC_Mempool *find_mp_of_chunk (MC_Chunk* mc_search)
{
   MC_Mempool* mp;

   tl_assert( MC_(mempool_list) );

   VG_(HT_ResetIter)( MC_(mempool_list) );
   while ( (mp = VG_(HT_Next)(MC_(mempool_list))) ) {
         MC_Chunk* mc;
         VG_(HT_ResetIter)(mp->chunks);
         while ( (mc = VG_(HT_Next)(mp->chunks)) ) {
            if (mc == mc_search)
               return mp;
         }
   }

   return NULL;
}

/*------------------------------------------------------------*/
/*--- Top-level entry point.                               ---*/
/*------------------------------------------------------------*/

void MC_(detect_memory_leaks) ( ThreadId tid, LeakCheckParams* lcp)
{
   Int i, j;
   
   tl_assert(lcp->mode != LC_Off);

   // Verify some assertions which are used in lc_scan_memory.
   tl_assert((VKI_PAGE_SIZE % sizeof(Addr)) == 0);
   tl_assert((SM_SIZE % sizeof(Addr)) == 0);
   // Above two assertions are critical, while below assertion
   // ensures that the optimisation in the loop is done in the
   // correct order : the loop checks for (big) SM chunk skipping
   // before checking for (smaller) page skipping.
   tl_assert((SM_SIZE % VKI_PAGE_SIZE) == 0);

   MC_(leak_search_gen)++;
   MC_(detect_memory_leaks_last_delta_mode) = lcp->deltamode;
   detect_memory_leaks_last_heuristics = lcp->heuristics;

   // Get the chunks, stop if there were none.
   if (lc_chunks) {
      VG_(free)(lc_chunks);
      lc_chunks = NULL;
   }
   lc_chunks = get_sorted_array_of_active_chunks(&lc_n_chunks);
   lc_chunks_n_frees_marker = MC_(get_cmalloc_n_frees)();
   if (lc_n_chunks == 0) {
      tl_assert(lc_chunks == NULL);
      if (lr_table != NULL) {
         // forget the previous recorded LossRecords as next leak search
         // can in any case just create new leaks.
         // Maybe it would be better to rather call print_result ?
         // (at least when leak decreases are requested)
         // This will then output all LossRecords with a size decreasing to 0
         VG_(OSetGen_Destroy) (lr_table);
         lr_table = NULL;
      }
      if (VG_(clo_verbosity) >= 1 && !VG_(clo_xml)) {
         VG_(umsg)("All heap blocks were freed -- no leaks are possible\n");
         VG_(umsg)("\n");
      }
      return;
   }

   // Sanity check -- make sure they don't overlap.  One exception is that
   // we allow a MALLOCLIKE block to sit entirely within a malloc() block.
   // This is for bug 100628.  If this occurs, we ignore the malloc() block
   // for leak-checking purposes.  This is a hack and probably should be done
   // better, but at least it's consistent with mempools (which are treated
   // like this in find_active_chunks).  Mempools have a separate VgHashTable
   // for mempool chunks, but if custom-allocated blocks are put in a separate
   // table from normal heap blocks it makes free-mismatch checking more
   // difficult.
   // Another exception: Metapool memory blocks overlap by definition. The meta-
   // block is allocated (by a custom allocator), and chunks of that block are
   // allocated again for use by the application: Not an error.
   //
   // If this check fails, it probably means that the application
   // has done something stupid with VALGRIND_MALLOCLIKE_BLOCK client
   // requests, eg. has made overlapping requests (which are
   // nonsensical), or used VALGRIND_MALLOCLIKE_BLOCK for stack locations;
   // again nonsensical.
   //
   for (i = 0; i < lc_n_chunks-1; i++) {
      MC_Chunk* ch1 = lc_chunks[i];
      MC_Chunk* ch2 = lc_chunks[i+1];

      Addr start1    = ch1->data;
      Addr start2    = ch2->data;
      Addr end1      = ch1->data + ch1->szB - 1;
      Addr end2      = ch2->data + ch2->szB - 1;
      Bool isCustom1 = ch1->allockind == MC_AllocCustom;
      Bool isCustom2 = ch2->allockind == MC_AllocCustom;

      if (end1 < start2) {
         // Normal case - no overlap.

      // We used to allow exact duplicates, I'm not sure why.  --njn
      //} else if (start1 == start2 && end1 == end2) {
         // Degenerate case: exact duplicates.

      } else if (start1 >= start2 && end1 <= end2 && isCustom1 && !isCustom2) {
         // Block i is MALLOCLIKE and entirely within block i+1.
         // Remove block i+1.
         for (j = i+1; j < lc_n_chunks-1; j++) {
            lc_chunks[j] = lc_chunks[j+1];
         }
         lc_n_chunks--;

      } else if (start2 >= start1 && end2 <= end1 && isCustom2 && !isCustom1) {
         // Block i+1 is MALLOCLIKE and entirely within block i.
         // Remove block i.
         for (j = i; j < lc_n_chunks-1; j++) {
            lc_chunks[j] = lc_chunks[j+1];
         }
         lc_n_chunks--;

      } else {
         // Overlap is allowed ONLY when one of the two candicates is a block
         // from a memory pool that has the metapool attribute set.
         // All other mixtures trigger the error + assert.
         MC_Mempool* mp;
         Bool ch1_is_meta = False, ch2_is_meta = False;
         Bool Inappropriate = False;

         if (MC_(is_mempool_block)(ch1)) {
            mp = find_mp_of_chunk(ch1);
            if (mp && mp->metapool) {
               ch1_is_meta = True;
            }
         }

         if (MC_(is_mempool_block)(ch2)) {
            mp = find_mp_of_chunk(ch2);
            if (mp && mp->metapool) {
               ch2_is_meta = True;
            }
         }
         
         // If one of the blocks is a meta block, the other must be entirely
         // within that meta block, or something is really wrong with the custom
         // allocator.
         if (ch1_is_meta != ch2_is_meta) {
            if ( (ch1_is_meta && (start2 < start1 || end2 > end1)) ||
                 (ch2_is_meta && (start1 < start2 || end1 > end2)) ) {
               Inappropriate = True;
            }
         }

         if (ch1_is_meta == ch2_is_meta || Inappropriate) {
            VG_(umsg)("Block 0x%lx..0x%lx overlaps with block 0x%lx..0x%lx\n",
                      start1, end1, start2, end2);
            VG_(umsg)("Blocks allocation contexts:\n"),
            VG_(pp_ExeContext)( MC_(allocated_at)(ch1));
            VG_(umsg)("\n"),
            VG_(pp_ExeContext)(  MC_(allocated_at)(ch2));
            VG_(umsg)("This is usually caused by using ");
            VG_(umsg)("VALGRIND_MALLOCLIKE_BLOCK in an inappropriate way.\n");
            tl_assert (0);
         }
      }
   }

   // Initialise lc_extras.
   if (lc_extras) {
      VG_(free)(lc_extras);
      lc_extras = NULL;
   }
   lc_extras = VG_(malloc)( "mc.dml.2", lc_n_chunks * sizeof(LC_Extra) );
   for (i = 0; i < lc_n_chunks; i++) {
      lc_extras[i].state        = Unreached;
      lc_extras[i].pending      = False;
      lc_extras[i].heuristic = LchNone;
      lc_extras[i].IorC.indirect_szB = 0;
   }

   // Initialise lc_markstack.
   lc_markstack = VG_(malloc)( "mc.dml.2", lc_n_chunks * sizeof(Int) );
   for (i = 0; i < lc_n_chunks; i++) {
      lc_markstack[i] = -1;
   }
   lc_markstack_top = -1;

   // Verbosity.
   if (VG_(clo_verbosity) > 1 && !VG_(clo_xml)) {
      VG_(umsg)( "Searching for pointers to %'d not-freed blocks\n",
                 lc_n_chunks );
   }

   // Scan the memory root-set, pushing onto the mark stack any blocks
   // pointed to.
   scan_memory_root_set(/*searched*/0, 0);

   // Scan GP registers for chunk pointers.
   VG_(apply_to_GP_regs)(lc_push_if_a_chunk_ptr_register);

   // Process the pushed blocks.  After this, every block that is reachable
   // from the root-set has been traced.
   lc_process_markstack(/*clique*/-1);

   if (VG_(clo_verbosity) > 1 && !VG_(clo_xml)) {
      VG_(umsg)("Checked %'lu bytes\n", lc_scanned_szB);
      if (lc_sig_skipped_szB > 0)
         VG_(umsg)("Skipped %'lu bytes due to read errors\n",
                   lc_sig_skipped_szB);
      VG_(umsg)( "\n" );
   }

   // Trace all the leaked blocks to determine which are directly leaked and
   // which are indirectly leaked.  For each Unreached block, push it onto
   // the mark stack, and find all the as-yet-Unreached blocks reachable
   // from it.  These form a clique and are marked IndirectLeak, and their
   // size is added to the clique leader's indirect size.  If one of the
   // found blocks was itself a clique leader (from a previous clique), then
   // the cliques are merged.
   for (i = 0; i < lc_n_chunks; i++) {
      MC_Chunk* ch = lc_chunks[i];
      LC_Extra* ex = &(lc_extras[i]);

      if (VG_DEBUG_CLIQUE)
         VG_(printf)("cliques: %d at %#lx -> Loss state %d\n",
                     i, ch->data, ex->state);

      tl_assert(lc_markstack_top == -1);

      if (ex->state == Unreached) {
         if (VG_DEBUG_CLIQUE)
            VG_(printf)("%d: gathering clique %#lx\n", i, ch->data);
         
         // Push this Unreached block onto the stack and process it.
         lc_push(i, ch);
         lc_process_markstack(/*clique*/i);

         tl_assert(lc_markstack_top == -1);
         tl_assert(ex->state == Unreached);
      }
   }

   print_results( tid, lcp);

   VG_(free) ( lc_markstack );
   lc_markstack = NULL;
   // lc_chunks, lc_extras, lr_array and lr_table are kept (needed if user
   // calls MC_(print_block_list)). lr_table also used for delta leak reporting
   // between this leak search and the next leak search.
}

static Addr searched_wpa;
static SizeT searched_szB;
static void
search_address_in_GP_reg(ThreadId tid, const HChar* regname, Addr addr_in_reg)
{
   if (addr_in_reg >= searched_wpa 
       && addr_in_reg < searched_wpa + searched_szB) {
      if (addr_in_reg == searched_wpa)
         VG_(umsg)
            ("tid %u register %s pointing at %#lx\n",
             tid, regname, searched_wpa);  
      else
         VG_(umsg)
            ("tid %u register %s interior pointing %lu bytes inside %#lx\n",
             tid, regname, (long unsigned) addr_in_reg - searched_wpa,
             searched_wpa);
   }
}

void MC_(who_points_at) ( Addr address, SizeT szB)
{
   MC_Chunk** chunks;
   Int        n_chunks;
   Int        i;

   if (szB == 1)
      VG_(umsg) ("Searching for pointers to %#lx\n", address);
   else
      VG_(umsg) ("Searching for pointers pointing in %lu bytes from %#lx\n",
                 szB, address);

   chunks = get_sorted_array_of_active_chunks(&n_chunks);

   // Scan memory root-set, searching for ptr pointing in address[szB]
   scan_memory_root_set(address, szB);

   // Scan active malloc-ed chunks
   for (i = 0; i < n_chunks; i++) {
      lc_scan_memory(chunks[i]->data, chunks[i]->szB,
                     /*is_prior_definite*/True,
                     /*clique*/-1, /*cur_clique*/-1,
                     address, szB);
   }
   VG_(free) ( chunks );

   // Scan GP registers for pointers to address range.
   searched_wpa = address;
   searched_szB = szB;
   VG_(apply_to_GP_regs)(search_address_in_GP_reg);

}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

