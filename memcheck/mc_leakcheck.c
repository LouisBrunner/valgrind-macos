
/*--------------------------------------------------------------------*/
/*--- The leak checker.                             mc_leakcheck.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2010 Julian Seward 
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
#include "pub_tool_signals.h"
#include "pub_tool_tooliface.h"     // Needed for mc_include.h

#include "mc_include.h"

#include <setjmp.h>                 // For jmp_buf

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
// are mentioned elsewhere (if --show-reachable=yes is given!))
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
#define VG_DEBUG_LEAKCHECK 0
#define VG_DEBUG_CLIQUE    0
 

/*------------------------------------------------------------*/
/*--- Getting the initial chunks, and searching them.      ---*/
/*------------------------------------------------------------*/

// Compare the MC_Chunks by 'data' (i.e. the address of the block).
static Int compare_MC_Chunks(void* n1, void* n2)
{
   MC_Chunk* mc1 = *(MC_Chunk**)n1;
   MC_Chunk* mc2 = *(MC_Chunk**)n2;
   if (mc1->data < mc2->data) return -1;
   if (mc1->data > mc2->data) return  1;
   return 0;
}

#if VG_DEBUG_LEAKCHECK
// Used to sanity-check the fast binary-search mechanism.
static 
Int find_chunk_for_OLD ( Addr       ptr, 
                         MC_Chunk** chunks,
                         Int        n_chunks )

{
   Int  i;
   Addr a_lo, a_hi;
   PROF_EVENT(70, "find_chunk_for_OLD");
   for (i = 0; i < n_chunks; i++) {
      PROF_EVENT(71, "find_chunk_for_OLD(loop)");
      a_lo = chunks[i]->data;
      a_hi = ((Addr)chunks[i]->data) + chunks[i]->szB;
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

#  if VG_DEBUG_LEAKCHECK
   tl_assert(retVal == find_chunk_for_OLD ( ptr, chunks, n_chunks ));
#  endif
   // VG_(printf)("%d\n", retVal);
   return retVal;
}


static MC_Chunk**
find_active_chunks(UInt* pn_chunks)
{
   // Our goal is to construct a set of chunks that includes every
   // mempool chunk, and every malloc region that *doesn't* contain a
   // mempool chunk.
   MC_Mempool *mp;
   MC_Chunk **mallocs, **chunks, *mc;
   UInt n_mallocs, n_chunks, m, s;
   Bool *malloc_chunk_holds_a_pool_chunk;

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

   // Then we build an array containing a Bool for each malloc chunk,
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

   return chunks;
}

/*------------------------------------------------------------*/
/*--- The leak detector proper.                            ---*/
/*------------------------------------------------------------*/

// Holds extra info about each block during leak checking.
typedef 
   struct {
      UInt  state:2;    // Reachedness.
      UInt  pending:1;  // Scan pending.  
      SizeT indirect_szB : (sizeof(SizeT)*8)-3; // If Unreached, how many bytes
                                                //   are unreachable from here.
   } 
   LC_Extra;

// An array holding pointers to every chunk we're checking.  Sorted by address.
static MC_Chunk** lc_chunks;
// How many chunks we're dealing with.
static Int        lc_n_chunks;

// This has the same number of entries as lc_chunks, and each entry
// in lc_chunks corresponds with the entry here (ie. lc_chunks[i] and
// lc_extras[i] describe the same block).
static LC_Extra* lc_extras;

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


// Determines if a pointer is to a chunk.  Returns the chunk number et al
// via call-by-reference.
static Bool
lc_is_a_chunk_ptr(Addr ptr, Int* pch_no, MC_Chunk** pch, LC_Extra** pex)
{
   Int ch_no;
   MC_Chunk* ch;
   LC_Extra* ex;

   // Quick filter.
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


// If 'ptr' is pointing to a heap-allocated block which hasn't been seen
// before, push it onto the mark stack.
static void
lc_push_without_clique_if_a_chunk_ptr(Addr ptr, Bool is_prior_definite)
{
   Int ch_no;
   MC_Chunk* ch;
   LC_Extra* ex;

   if ( ! lc_is_a_chunk_ptr(ptr, &ch_no, &ch, &ex) )
      return;
   
   // Possibly upgrade the state, ie. one of:
   // - Unreached --> Possible
   // - Unreached --> Reachable 
   // - Possible  --> Reachable
   if (ptr == ch->data && is_prior_definite && ex->state != Reachable) {
      // 'ptr' points to the start of the block, and the prior node is
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
lc_push_if_a_chunk_ptr_register(Addr ptr)
{
   lc_push_without_clique_if_a_chunk_ptr(ptr, /*is_prior_definite*/True);
}

// If ptr is pointing to a heap-allocated block which hasn't been seen
// before, push it onto the mark stack.  Clique is the index of the
// clique leader.
static void
lc_push_with_clique_if_a_chunk_ptr(Addr ptr, Int clique)
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
      ex->state = IndirectLeak;
      lc_push(ch_no, ch);

      // Add the block to the clique, and add its size to the
      // clique-leader's indirect size.  Also, if the new block was
      // itself a clique leader, it isn't any more, so add its
      // indirect_szB to the new clique leader.
      if (VG_DEBUG_CLIQUE) {
         if (ex->indirect_szB > 0)
            VG_(printf)("  clique %d joining clique %d adding %lu+%lu\n", 
                        ch_no, clique, (SizeT)ch->szB, (SizeT)ex->indirect_szB);
         else
            VG_(printf)("  block %d joining clique %d adding %lu\n", 
                        ch_no, clique, (SizeT)ch->szB);
      }

      lc_extras[clique].indirect_szB += ch->szB;
      lc_extras[clique].indirect_szB += ex->indirect_szB;
      ex->indirect_szB = 0;    // Shouldn't matter.
   }
}

static void
lc_push_if_a_chunk_ptr(Addr ptr, Int clique, Bool is_prior_definite)
{
   if (-1 == clique) 
      lc_push_without_clique_if_a_chunk_ptr(ptr, is_prior_definite);
   else
      lc_push_with_clique_if_a_chunk_ptr(ptr, clique);
}


static jmp_buf memscan_jmpbuf;

static
void scan_all_valid_memory_catcher ( Int sigNo, Addr addr )
{
   if (0)
      VG_(printf)("OUCH! sig=%d addr=%#lx\n", sigNo, addr);
   if (sigNo == VKI_SIGSEGV || sigNo == VKI_SIGBUS)
      __builtin_longjmp(memscan_jmpbuf, 1);
}

// Scan a block of memory between [start, start+len).  This range may
// be bogus, inaccessable, or otherwise strange; we deal with it.  For each
// valid aligned word we assume it's a pointer to a chunk a push the chunk
// onto the mark stack if so.
static void
lc_scan_memory(Addr start, SizeT len, Bool is_prior_definite, Int clique)
{
   Addr ptr = VG_ROUNDUP(start,     sizeof(Addr));
   Addr end = VG_ROUNDDN(start+len, sizeof(Addr));
   vki_sigset_t sigmask;

   if (VG_DEBUG_LEAKCHECK)
      VG_(printf)("scan %#lx-%#lx (%lu)\n", start, end, len);

   VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &sigmask);
   VG_(set_fault_catcher)(scan_all_valid_memory_catcher);

   // We might be in the middle of a page.  Do a cheap check to see if
   // it's valid;  if not, skip onto the next page.
   if (!VG_(am_is_valid_for_client)(ptr, sizeof(Addr), VKI_PROT_READ))
      ptr = VG_PGROUNDUP(ptr+1);        // First page is bad - skip it.

   while (ptr < end) {
      Addr addr;

      // Skip invalid chunks.
      if ( ! MC_(is_within_valid_secondary)(ptr) ) {
         ptr = VG_ROUNDUP(ptr+1, SM_SIZE);
         continue;
      }

      // Look to see if this page seems reasonable.
      if ((ptr % VKI_PAGE_SIZE) == 0) {
         if (!VG_(am_is_valid_for_client)(ptr, sizeof(Addr), VKI_PROT_READ)) {
            ptr += VKI_PAGE_SIZE;      // Bad page - skip it.
            continue;
         }
      }

      if (__builtin_setjmp(memscan_jmpbuf) == 0) {
         if ( MC_(is_valid_aligned_word)(ptr) ) {
            lc_scanned_szB += sizeof(Addr);
            addr = *(Addr *)ptr;
            // If we get here, the scanned word is in valid memory.  Now
            // let's see if its contents point to a chunk.
            lc_push_if_a_chunk_ptr(addr, clique, is_prior_definite);
         } else if (0 && VG_DEBUG_LEAKCHECK) {
            VG_(printf)("%#lx not valid\n", ptr);
         }
         ptr += sizeof(Addr);
      } else {
         // We need to restore the signal mask, because we were
         // longjmped out of a signal handler.
         VG_(sigprocmask)(VKI_SIG_SETMASK, &sigmask, NULL);

         ptr = VG_PGROUNDUP(ptr+1);     // Bad page - skip it.
      }
   }

   VG_(sigprocmask)(VKI_SIG_SETMASK, &sigmask, NULL);
   VG_(set_fault_catcher)(NULL);
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
                     is_prior_definite, clique);
   }
}

static Word cmp_LossRecordKey_LossRecord(const void* key, const void* elem)
{
   LossRecordKey* a = (LossRecordKey*)key;
   LossRecordKey* b = &(((LossRecord*)elem)->key);

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

static Int cmp_LossRecords(void* va, void* vb)
{
   LossRecord* lr_a = *(LossRecord**)va;
   LossRecord* lr_b = *(LossRecord**)vb;
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

static void print_results(ThreadId tid, Bool is_full_check)
{
   Int          i, n_lossrecords;
   OSet*        lr_table;
   LossRecord** lr_array;
   LossRecord*  lr;
   Bool         is_suppressed;

   // Create the lr_table, which holds the loss records.
   lr_table =
      VG_(OSetGen_Create)(offsetof(LossRecord, key),
                          cmp_LossRecordKey_LossRecord,
                          VG_(malloc), "mc.pr.1",
                          VG_(free)); 

   // Convert the chunks into loss records, merging them where appropriate.
   for (i = 0; i < lc_n_chunks; i++) {
      MC_Chunk*     ch = lc_chunks[i];
      LC_Extra*     ex = &(lc_extras)[i];
      LossRecord*   old_lr;
      LossRecordKey lrkey;
      lrkey.state        = ex->state;
      lrkey.allocated_at = ch->where;

      old_lr = VG_(OSetGen_Lookup)(lr_table, &lrkey);
      if (old_lr) {
         // We found an existing loss record matching this chunk.  Update the
         // loss record's details in-situ.  This is safe because we don't
         // change the elements used as the OSet key.
         old_lr->szB          += ch->szB;
         old_lr->indirect_szB += ex->indirect_szB;
         old_lr->num_blocks++;
      } else {
         // No existing loss record matches this chunk.  Create a new loss
         // record, initialise it from the chunk, and insert it into lr_table.
         lr = VG_(OSetGen_AllocNode)(lr_table, sizeof(LossRecord));
         lr->key              = lrkey;
         lr->szB              = ch->szB;
         lr->indirect_szB     = ex->indirect_szB;
         lr->num_blocks       = 1;
         VG_(OSetGen_Insert)(lr_table, lr);
      }
   }
   n_lossrecords = VG_(OSetGen_Size)(lr_table);

   // Create an array of pointers to the loss records.
   lr_array = VG_(malloc)("mc.pr.2", n_lossrecords * sizeof(LossRecord*));
   i = 0;
   VG_(OSetGen_ResetIter)(lr_table);
   while ( (lr = VG_(OSetGen_Next)(lr_table)) ) {
      lr_array[i++] = lr;
   }
   tl_assert(i == n_lossrecords);

   // Sort the array by loss record sizes.
   VG_(ssort)(lr_array, n_lossrecords, sizeof(LossRecord*),
              cmp_LossRecords);

   // Zero totals.
   MC_(blocks_leaked)     = MC_(bytes_leaked)     = 0;
   MC_(blocks_indirect)   = MC_(bytes_indirect)   = 0;
   MC_(blocks_dubious)    = MC_(bytes_dubious)    = 0;
   MC_(blocks_reachable)  = MC_(bytes_reachable)  = 0;
   MC_(blocks_suppressed) = MC_(bytes_suppressed) = 0;

   // Print the loss records (in size order) and collect summary stats.
   for (i = 0; i < n_lossrecords; i++) {
      Bool count_as_error, print_record;
      // Rules for printing:
      // - We don't show suppressed loss records ever (and that's controlled
      //   within the error manager).
      // - We show non-suppressed loss records that are not "reachable" if 
      //   --leak-check=yes.
      // - We show all non-suppressed loss records if --leak-check=yes and
      //   --show-reachable=yes.
      //
      // Nb: here "reachable" means Reachable *or* IndirectLeak;  note that
      // this is different to "still reachable" used elsewhere because it
      // includes indirectly lost blocks!
      //
      lr = lr_array[i];
      print_record = is_full_check &&
                     ( MC_(clo_show_reachable) ||
                       Unreached == lr->key.state || 
                       ( MC_(clo_show_possibly_lost) && 
                         Possible  == lr->key.state ) );
      // We don't count a leaks as errors with --leak-check=summary.
      // Otherwise you can get high error counts with few or no error
      // messages, which can be confusing.  Also, you could argue that
      // indirect leaks should be counted as errors, but it seems better to
      // make the counting criteria similar to the printing criteria.  So we
      // don't count them.
      count_as_error = is_full_check && 
                       ( Unreached == lr->key.state || 
                         Possible  == lr->key.state );
      is_suppressed = 
         MC_(record_leak_error) ( tid, i+1, n_lossrecords, lr, print_record,
                                  count_as_error );

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

   if (VG_(clo_verbosity) > 0 && !VG_(clo_xml)) {
      VG_(umsg)("LEAK SUMMARY:\n");
      VG_(umsg)("   definitely lost: %'lu bytes in %'lu blocks\n",
                MC_(bytes_leaked), MC_(blocks_leaked) );
      VG_(umsg)("   indirectly lost: %'lu bytes in %'lu blocks\n",
                MC_(bytes_indirect), MC_(blocks_indirect) );
      VG_(umsg)("     possibly lost: %'lu bytes in %'lu blocks\n",
                MC_(bytes_dubious), MC_(blocks_dubious) );
      VG_(umsg)("   still reachable: %'lu bytes in %'lu blocks\n",
                MC_(bytes_reachable), MC_(blocks_reachable) );
      VG_(umsg)("        suppressed: %'lu bytes in %'lu blocks\n",
                MC_(bytes_suppressed), MC_(blocks_suppressed) );
      if (!is_full_check &&
          (MC_(blocks_leaked) + MC_(blocks_indirect) +
           MC_(blocks_dubious) + MC_(blocks_reachable)) > 0) {
         VG_(umsg)("Rerun with --leak-check=full to see details "
                   "of leaked memory\n");
      }
      if (is_full_check &&
          MC_(blocks_reachable) > 0 && !MC_(clo_show_reachable))
      {
         VG_(umsg)("Reachable blocks (those to which a pointer "
                   "was found) are not shown.\n");
         VG_(umsg)("To see them, rerun with: --leak-check=full "
                   "--show-reachable=yes\n");
      }
      VG_(umsg)("\n");
   }
}

/*------------------------------------------------------------*/
/*--- Top-level entry point.                               ---*/
/*------------------------------------------------------------*/

void MC_(detect_memory_leaks) ( ThreadId tid, LeakCheckMode mode )
{
   Int i, j;
   
   tl_assert(mode != LC_Off);

   // Get the chunks, stop if there were none.
   lc_chunks = find_active_chunks(&lc_n_chunks);
   if (lc_n_chunks == 0) {
      tl_assert(lc_chunks == NULL);
      if (VG_(clo_verbosity) >= 1 && !VG_(clo_xml)) {
         VG_(umsg)("All heap blocks were freed -- no leaks are possible\n");
         VG_(umsg)("\n");
      }
      return;
   }

   // Sort the array so blocks are in ascending order in memory.
   VG_(ssort)(lc_chunks, lc_n_chunks, sizeof(VgHashNode*), compare_MC_Chunks);

   // Sanity check -- make sure they're in order.
   for (i = 0; i < lc_n_chunks-1; i++) {
      tl_assert( lc_chunks[i]->data <= lc_chunks[i+1]->data);
   }

   // Sanity check -- make sure they don't overlap.  The one exception is that
   // we allow a MALLOCLIKE block to sit entirely within a malloc() block.
   // This is for bug 100628.  If this occurs, we ignore the malloc() block
   // for leak-checking purposes.  This is a hack and probably should be done
   // better, but at least it's consistent with mempools (which are treated
   // like this in find_active_chunks).  Mempools have a separate VgHashTable
   // for mempool chunks, but if custom-allocated blocks are put in a separate
   // table from normal heap blocks it makes free-mismatch checking more
   // difficult.
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
         VG_(umsg)("Block 0x%lx..0x%lx overlaps with block 0x%lx..0x%lx",
                   start1, end1, start1, end2);
         VG_(umsg)("This is usually caused by using VALGRIND_MALLOCLIKE_BLOCK");
         VG_(umsg)("in an inappropriate way.");
         tl_assert (0);
      }
   }

   // Initialise lc_extras.
   lc_extras = VG_(malloc)( "mc.dml.2", lc_n_chunks * sizeof(LC_Extra) );
   for (i = 0; i < lc_n_chunks; i++) {
      lc_extras[i].state        = Unreached;
      lc_extras[i].pending      = False;
      lc_extras[i].indirect_szB = 0;
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
   {
      Int   n_seg_starts;
      Addr* seg_starts = VG_(get_segment_starts)( &n_seg_starts );

      tl_assert(seg_starts && n_seg_starts > 0);

      lc_scanned_szB = 0;

      // VG_(am_show_nsegments)( 0, "leakcheck");
      for (i = 0; i < n_seg_starts; i++) {
         SizeT seg_size;
         NSegment const* seg = VG_(am_find_nsegment)( seg_starts[i] );
         tl_assert(seg);

         if (seg->kind != SkFileC && seg->kind != SkAnonC) continue;
         if (!(seg->hasR && seg->hasW))                    continue;
         if (seg->isCH)                                    continue;

         // Don't poke around in device segments as this may cause
         // hangs.  Exclude /dev/zero just in case someone allocated
         // memory by explicitly mapping /dev/zero.
         if (seg->kind == SkFileC 
             && (VKI_S_ISCHR(seg->mode) || VKI_S_ISBLK(seg->mode))) {
            HChar* dev_name = VG_(am_get_filename)( (NSegment*)seg );
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
         lc_scan_memory(seg->start, seg_size, /*is_prior_definite*/True, -1);
      }
   }

   // Scan GP registers for chunk pointers.
   VG_(apply_to_GP_regs)(lc_push_if_a_chunk_ptr_register);

   // Process the pushed blocks.  After this, every block that is reachable
   // from the root-set has been traced.
   lc_process_markstack(/*clique*/-1);

   if (VG_(clo_verbosity) > 1 && !VG_(clo_xml)) {
      VG_(umsg)("Checked %'lu bytes\n", lc_scanned_szB);
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
         lc_process_markstack(i);

         tl_assert(lc_markstack_top == -1);
         tl_assert(ex->state == Unreached);
      }
   }
      
   print_results( tid, ( mode == LC_Full ? True : False ) );

   VG_(free) ( lc_chunks );
   VG_(free) ( lc_extras );
   VG_(free) ( lc_markstack );
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

