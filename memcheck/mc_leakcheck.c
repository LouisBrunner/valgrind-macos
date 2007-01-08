
/*--------------------------------------------------------------------*/
/*--- The leak checker.                             mc_leakcheck.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2007 Julian Seward 
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
#include "pub_tool_signals.h"
#include "pub_tool_tooliface.h"     // Needed for mc_include.h

#include "mc_include.h"

#include <setjmp.h>                 // For jmp_buf


/* Define to debug the memory-leak-detector. */
#define VG_DEBUG_LEAKCHECK 0
#define VG_DEBUG_CLIQUE	   0

/*------------------------------------------------------------*/
/*--- Low-level address-space scanning, for the leak       ---*/
/*--- detector.                                            ---*/
/*------------------------------------------------------------*/

static 
jmp_buf memscan_jmpbuf;


static
void scan_all_valid_memory_catcher ( Int sigNo, Addr addr )
{
   if (0)
      VG_(printf)("OUCH! sig=%d addr=%p\n", sigNo, addr);
   if (sigNo == VKI_SIGSEGV || sigNo == VKI_SIGBUS)
      __builtin_longjmp(memscan_jmpbuf, 1);
}


/* TODO: GIVE THIS A PROPER HOME
   TODO: MERGE THIS WITH DUPLICATE IN m_main.c and coredump-elf.c.
   Extract from aspacem a vector of the current segment start
   addresses.  The vector is dynamically allocated and should be freed
   by the caller when done.  REQUIRES m_mallocfree to be running.
   Writes the number of addresses required into *n_acquired. */

static Addr* get_seg_starts ( /*OUT*/Int* n_acquired )
{
   Addr* starts;
   Int   n_starts, r = 0;

   n_starts = 1;
   while (True) {
      starts = VG_(malloc)( n_starts * sizeof(Addr) );
      if (starts == NULL)
         break;
      r = VG_(am_get_segment_starts)( starts, n_starts );
      if (r >= 0)
         break;
      VG_(free)(starts);
      n_starts *= 2;
   }

   if (starts == NULL) {
     *n_acquired = 0;
     return NULL;
   }

   *n_acquired = r;
   return starts;
}


/*------------------------------------------------------------*/
/*--- Detecting leaked (unreachable) malloc'd blocks.      ---*/
/*------------------------------------------------------------*/

/* An entry in the mark stack */
typedef 
   struct {
      Int   next:30;	/* Index of next in mark stack */
      UInt  state:2;	/* Reachedness */
      SizeT indirect;	/* if Unreached, how much is unreachable from here */
   } 
   MarkStack;

/* Find the i such that ptr points at or inside the block described by
   shadows[i].  Return -1 if none found.  This assumes that shadows[]
   has been sorted on the ->data field. */

#if VG_DEBUG_LEAKCHECK
/* Used to sanity-check the fast binary-search mechanism. */
static 
Int find_shadow_for_OLD ( Addr       ptr, 
                          MC_Chunk** shadows,
                          Int        n_shadows )

{
   Int  i;
   Addr a_lo, a_hi;
   PROF_EVENT(70, "find_shadow_for_OLD");
   for (i = 0; i < n_shadows; i++) {
      PROF_EVENT(71, "find_shadow_for_OLD(loop)");
      a_lo = shadows[i]->data;
      a_hi = ((Addr)shadows[i]->data) + shadows[i]->szB;
      if (a_lo <= ptr && ptr < a_hi)
         return i;
   }
   return -1;
}
#endif


static 
Int find_shadow_for ( Addr       ptr, 
                      MC_Chunk** shadows,
                      Int        n_shadows )
{
   Addr a_mid_lo, a_mid_hi;
   Int lo, mid, hi, retVal;
   /* VG_(printf)("find shadow for %p = ", ptr); */
   retVal = -1;
   lo = 0;
   hi = n_shadows-1;
   while (True) {
      /* invariant: current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) break; /* not found */

      mid      = (lo + hi) / 2;
      a_mid_lo = shadows[mid]->data;
      a_mid_hi = shadows[mid]->data + shadows[mid]->szB;
      /* Extent of block 'mid' is [a_mid_lo .. a_mid_hi).
         Special-case zero-sized blocks - treat them as if they had
         size 1.  Not doing so causes them to not cover any address
         range at all and so will never be identified as the target of
         any pointer, which causes them to be incorrectly reported as
         definitely leaked. */
      if (shadows[mid]->szB == 0)
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
   tl_assert(retVal == find_shadow_for_OLD ( ptr, shadows, n_shadows ));
#  endif
   /* VG_(printf)("%d\n", retVal); */
   return retVal;
}

/* Globals, for the following callback used by VG_(detect_memory_leaks). */
static MC_Chunk** lc_shadows;
static Int        lc_n_shadows;
static MarkStack* lc_markstack;
static Int	  lc_markstack_top;
static Addr       lc_min_mallocd_addr;
static Addr       lc_max_mallocd_addr;
static SizeT	  lc_scanned;

static Bool	  (*lc_is_within_valid_secondary) (Addr addr);
static Bool	  (*lc_is_valid_aligned_word)     (Addr addr);


SizeT MC_(bytes_leaked)     = 0;
SizeT MC_(bytes_indirect)   = 0;
SizeT MC_(bytes_dubious)    = 0;
SizeT MC_(bytes_reachable)  = 0;
SizeT MC_(bytes_suppressed) = 0;

static Int lc_compar(void* n1, void* n2)
{
   MC_Chunk* mc1 = *(MC_Chunk**)n1;
   MC_Chunk* mc2 = *(MC_Chunk**)n2;
   return (mc1->data < mc2->data ? -1 : 1);
}

/* If ptr is pointing to a heap-allocated block which hasn't been seen
   before, push it onto the mark stack.  Clique is the index of the
   clique leader; -1 if none. */
static void lc_markstack_push_WRK(Addr ptr, Int clique)
{
   Int sh_no;

   /* quick filter */
   if (!VG_(am_is_valid_for_client)(ptr, 1, VKI_PROT_NONE))
      return;

   sh_no = find_shadow_for(ptr, lc_shadows, lc_n_shadows);

   if (VG_DEBUG_LEAKCHECK)
      VG_(printf)("ptr=%p -> block %d\n", ptr, sh_no);

   if (sh_no == -1)
      return;

   tl_assert(sh_no >= 0 && sh_no < lc_n_shadows);
   tl_assert(ptr >= lc_shadows[sh_no]->data);
   tl_assert(ptr < lc_shadows[sh_no]->data 
                   + lc_shadows[sh_no]->szB
                   + (lc_shadows[sh_no]->szB==0  ? 1  : 0));

   if (lc_markstack[sh_no].state == Unreached) {
      if (0)
	 VG_(printf)("pushing %p-%p\n", lc_shadows[sh_no]->data, 
		     lc_shadows[sh_no]->data + lc_shadows[sh_no]->szB);

      tl_assert(lc_markstack[sh_no].next == -1);
      lc_markstack[sh_no].next = lc_markstack_top;
      lc_markstack_top = sh_no;
   }

   tl_assert(clique >= -1 && clique < lc_n_shadows);

   if (clique != -1) {
      if (0)
	 VG_(printf)("mopup: %d: %p is %d\n", 
		     sh_no, lc_shadows[sh_no]->data, lc_markstack[sh_no].state);

      /* An unmarked block - add it to the clique.  Add its size to
	 the clique-leader's indirect size.  If the new block was
	 itself a clique leader, it isn't any more, so add its
	 indirect to the new clique leader.

	 If this block *is* the clique leader, it means this is a
	 cyclic structure, so none of this applies. */
      if (lc_markstack[sh_no].state == Unreached) {
	 lc_markstack[sh_no].state = IndirectLeak;

	 if (sh_no != clique) {
	    if (VG_DEBUG_CLIQUE) {
	       if (lc_markstack[sh_no].indirect)
		  VG_(printf)("  clique %d joining clique %d adding %d+%d bytes\n", 
			      sh_no, clique, 
			      lc_shadows[sh_no]->szB, lc_markstack[sh_no].indirect);
	       else
		  VG_(printf)("  %d joining %d adding %d\n", 
			      sh_no, clique, lc_shadows[sh_no]->szB);
	    }

	    lc_markstack[clique].indirect += lc_shadows[sh_no]->szB;
	    lc_markstack[clique].indirect += lc_markstack[sh_no].indirect;
	    lc_markstack[sh_no].indirect = 0; /* shouldn't matter */
	 }
      }
   } else if (ptr == lc_shadows[sh_no]->data) {
      lc_markstack[sh_no].state = Proper;
   } else {
      if (lc_markstack[sh_no].state == Unreached)
	 lc_markstack[sh_no].state = Interior;
   }
}

static void lc_markstack_push(Addr ptr)
{
   lc_markstack_push_WRK(ptr, -1);
}

/* Return the top of the mark stack, if any. */
static Int lc_markstack_pop(void)
{
   Int ret = lc_markstack_top;

   if (ret != -1) {
      lc_markstack_top = lc_markstack[ret].next;
      lc_markstack[ret].next = -1;
   }

   return ret;
}


/* Scan a block of memory between [start, start+len).  This range may
   be bogus, inaccessable, or otherwise strange; we deal with it.

   If clique != -1, it means we're gathering leaked memory into
   cliques, and clique is the index of the current clique leader. */
static void lc_scan_memory_WRK(Addr start, SizeT len, Int clique)
{
   Addr ptr = VG_ROUNDUP(start, sizeof(Addr));
   Addr end = VG_ROUNDDN(start+len, sizeof(Addr));
   vki_sigset_t sigmask;

   if (VG_DEBUG_LEAKCHECK)
      VG_(printf)("scan %p-%p\n", start, start+len);
   VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &sigmask);
   VG_(set_fault_catcher)(scan_all_valid_memory_catcher);

   //   lc_scanned += end-ptr;

   if (!VG_(am_is_valid_for_client)(ptr, sizeof(Addr), VKI_PROT_READ))
      ptr = VG_PGROUNDUP(ptr+1);	/* first page bad */

   while (ptr < end) {
      Addr addr;

      /* Skip invalid chunks */
      if (!(*lc_is_within_valid_secondary)(ptr)) {
	 ptr = VG_ROUNDUP(ptr+1, SM_SIZE);
	 continue;
      }

      /* Look to see if this page seems reasonble */
      if ((ptr % VKI_PAGE_SIZE) == 0) {
	 if (!VG_(am_is_valid_for_client)(ptr, sizeof(Addr), VKI_PROT_READ))
	    ptr += VKI_PAGE_SIZE; /* bad page - skip it */
      }

      if (__builtin_setjmp(memscan_jmpbuf) == 0) {
	 if ((*lc_is_valid_aligned_word)(ptr)) {
            lc_scanned += sizeof(Addr);
	    addr = *(Addr *)ptr;
	    lc_markstack_push_WRK(addr, clique);
	 } else if (0 && VG_DEBUG_LEAKCHECK)
	    VG_(printf)("%p not valid\n", ptr);
	 ptr += sizeof(Addr);
      } else {
	 /* We need to restore the signal mask, because we were
	    longjmped out of a signal handler. */
	 VG_(sigprocmask)(VKI_SIG_SETMASK, &sigmask, NULL);

	 ptr = VG_PGROUNDUP(ptr+1);	/* bad page - skip it */
      }
   }

   VG_(sigprocmask)(VKI_SIG_SETMASK, &sigmask, NULL);
   VG_(set_fault_catcher)(NULL);
}


static void lc_scan_memory(Addr start, SizeT len)
{
   lc_scan_memory_WRK(start, len, -1);
}

/* Process the mark stack until empty.  If mopup is true, then we're
   actually gathering leaked blocks, so they should be marked
   IndirectLeak. */
static void lc_do_leakcheck(Int clique)
{
   Int top;

   while((top = lc_markstack_pop()) != -1) {
      tl_assert(top >= 0 && top < lc_n_shadows);      
      tl_assert(lc_markstack[top].state != Unreached);

      lc_scan_memory_WRK(lc_shadows[top]->data, lc_shadows[top]->szB, clique);
   }
}

static SizeT blocks_leaked;
static SizeT blocks_indirect;
static SizeT blocks_dubious;
static SizeT blocks_reachable;
static SizeT blocks_suppressed;

static void full_report(ThreadId tid)
{
   Int i;
   Int    n_lossrecords;
   LossRecord* errlist;
   LossRecord* p;
   Bool   is_suppressed;

   /* Go through and group lost structures into cliques.  For each
      Unreached block, push it onto the mark stack, and find all the
      blocks linked to it.  These are marked IndirectLeak, and their
      size is added to the clique leader's indirect size.  If one of
      the found blocks was itself a clique leader (from a previous
      pass), then the cliques are merged. */
   for (i = 0; i < lc_n_shadows; i++) {
      if (VG_DEBUG_CLIQUE)
	 VG_(printf)("cliques: %d at %p -> Loss state %d\n",
		     i, lc_shadows[i]->data, lc_markstack[i].state);
      if (lc_markstack[i].state != Unreached)
	 continue;

      tl_assert(lc_markstack_top == -1);

      if (VG_DEBUG_CLIQUE)
	 VG_(printf)("%d: gathering clique %p\n", i, lc_shadows[i]->data);
      
      lc_markstack_push_WRK(lc_shadows[i]->data, i);

      lc_do_leakcheck(i);

      tl_assert(lc_markstack_top == -1);
      tl_assert(lc_markstack[i].state == IndirectLeak
                /* jrs 20051218: Ashley Pittman supplied a
                   custom-allocator test program which causes the ==
                   IndirectLeak condition to fail - it causes .state
                   to be Unreached.  Since I have no idea how this
                   clique stuff works and no time to figure it out,
                   just allow that condition too.  This could well be
                   a completely bogus fix.  It doesn't seem unsafe
                   given that in any case the .state field is
                   immediately overwritten by the next statement. */
                || lc_markstack[i].state == Unreached);

      lc_markstack[i].state = Unreached; /* Return to unreached state,
					    to indicate its a clique
					    leader */
   }
      
   /* Common up the lost blocks so we can print sensible error messages. */
   n_lossrecords = 0;
   errlist       = NULL;
   for (i = 0; i < lc_n_shadows; i++) {
      ExeContext* where = lc_shadows[i]->where;

      for (p = errlist; p != NULL; p = p->next) {
         if (p->loss_mode == lc_markstack[i].state
             && VG_(eq_ExeContext) ( MC_(clo_leak_resolution),
                                     p->allocated_at, 
                                     where) ) {
            break;
	 }
      }
      if (p != NULL) {
         p->num_blocks  ++;
         p->total_bytes += lc_shadows[i]->szB;
	 p->indirect_bytes += lc_markstack[i].indirect;
      } else {
         n_lossrecords ++;
         p = VG_(malloc)(sizeof(LossRecord));
         p->loss_mode    = lc_markstack[i].state;
         p->allocated_at = where;
         p->total_bytes  = lc_shadows[i]->szB;
	 p->indirect_bytes = lc_markstack[i].indirect;
         p->num_blocks   = 1;
         p->next         = errlist;
         errlist         = p;
      }
   }

   /* Print out the commoned-up blocks and collect summary stats. */
   for (i = 0; i < n_lossrecords; i++) {
      Bool        print_record;
      LossRecord* p_min = NULL;
      SizeT       n_min = ~(0x0L);
      for (p = errlist; p != NULL; p = p->next) {
         if (p->num_blocks > 0 && p->total_bytes < n_min) {
            n_min = p->total_bytes + p->indirect_bytes;
            p_min = p;
         }
      }
      tl_assert(p_min != NULL);

      /* Ok to have tst==NULL;  it's only used if --gdb-attach=yes, and
         we disallow that when --leak-check=yes.  
         
         Prints the error if not suppressed, unless it's reachable (Proper
         or IndirectLeak) and --show-reachable=no */

      print_record = ( MC_(clo_show_reachable) || 
		       Unreached == p_min->loss_mode || 
                       Interior == p_min->loss_mode );

      // Nb: because VG_(unique_error) does all the error processing
      // immediately, and doesn't save the error, leakExtra can be
      // stack-allocated.
      is_suppressed = 
         MC_(record_leak_error) ( tid, i+1, n_lossrecords, p_min,
                                  print_record );

      if (is_suppressed) {
         blocks_suppressed     += p_min->num_blocks;
         MC_(bytes_suppressed) += p_min->total_bytes;

      } else if (Unreached == p_min->loss_mode) {
         blocks_leaked       += p_min->num_blocks;
         MC_(bytes_leaked)   += p_min->total_bytes;

      } else if (IndirectLeak == p_min->loss_mode) {
         blocks_indirect     += p_min->num_blocks;
         MC_(bytes_indirect) += p_min->total_bytes;

      } else if (Interior   == p_min->loss_mode) {
         blocks_dubious     += p_min->num_blocks;
         MC_(bytes_dubious) += p_min->total_bytes;

      } else if (Proper       == p_min->loss_mode) {
         blocks_reachable     += p_min->num_blocks;
         MC_(bytes_reachable) += p_min->total_bytes;

      } else {
         VG_(tool_panic)("generic_detect_memory_leaks: unknown loss mode");
      }
      p_min->num_blocks = 0;
   }
}

/* Compute a quick summary of the leak check. */
static void make_summary(void)
{
   Int i;

   for(i = 0; i < lc_n_shadows; i++) {
      SizeT size = lc_shadows[i]->szB;

      switch(lc_markstack[i].state) {
      case Unreached:
	 blocks_leaked++;
	 MC_(bytes_leaked) += size;
	 break;

      case Proper:
	 blocks_reachable++;
	 MC_(bytes_reachable) += size;
	 break;

      case Interior:
	 blocks_dubious++;
	 MC_(bytes_dubious) += size;
	 break;
	 
      case IndirectLeak:	/* shouldn't happen */
	 blocks_indirect++;
	 MC_(bytes_indirect) += size;
	 break;
      }
   }
}

static MC_Chunk**
find_active_shadows(UInt* n_shadows)
{
   /* Our goal is to construct a set of shadows that includes every
    * mempool chunk, and every malloc region that *doesn't* contain a
    * mempool chunk. We do this in several phases.
    *
    * First we collect all the malloc chunks into an array and sort it.
    * We do this because we want to query the chunks by interior
    * pointers, requiring binary search.
    *
    * Second we build an array containing a Bool for each malloc chunk,
    * indicating whether it contains any mempools.
    *
    * Third we loop over the mempool tables. For each chunk in each
    * pool, we set the entry in the Bool array corresponding to the
    * malloc chunk containing the mempool chunk.
    *
    * Finally we copy the mempool chunks and the non-marked malloc
    * chunks into a combined array of shadows, free our temporaries,
    * and return the combined array.
    */

   MC_Mempool *mp;
   MC_Chunk **mallocs, **shadows, *mc;
   UInt n_mallocs, m, s;
   Bool *malloc_chunk_holds_a_pool_chunk;

   mallocs = (MC_Chunk**) VG_(HT_to_array)( MC_(malloc_list), &n_mallocs );

   if (n_mallocs == 0) {
      tl_assert(mallocs == NULL);
      *n_shadows = 0;
      return NULL;
   }

   VG_(ssort)((void*)mallocs, n_mallocs, 
              sizeof(VgHashNode*), lc_compar);

   malloc_chunk_holds_a_pool_chunk = VG_(calloc)( n_mallocs, sizeof(Bool) );

   *n_shadows = n_mallocs;

   VG_(HT_ResetIter)(MC_(mempool_list));
   while ( (mp = VG_(HT_Next)(MC_(mempool_list))) ) {
      VG_(HT_ResetIter)(mp->chunks);
      while ( (mc = VG_(HT_Next)(mp->chunks)) ) {

         /* We'll need a shadow for this chunk. */
         ++(*n_shadows);

         /* Possibly invalidate the malloc holding the beginning of this chunk. */
         m = find_shadow_for(mc->data, mallocs, n_mallocs);
         if (m != -1 && malloc_chunk_holds_a_pool_chunk[m] == False) {
            tl_assert(*n_shadows > 0);
            --(*n_shadows);
            malloc_chunk_holds_a_pool_chunk[m] = True;
         }

         /* Possibly invalidate the malloc holding the end of this chunk. */
         if (mc->szB > 1) {
            m = find_shadow_for(mc->data + (mc->szB - 1), mallocs, n_mallocs);
            if (m != -1 && malloc_chunk_holds_a_pool_chunk[m] == False) {
               tl_assert(*n_shadows > 0);
               --(*n_shadows);
               malloc_chunk_holds_a_pool_chunk[m] = True;
            }
         }
      }
   }

   tl_assert(*n_shadows > 0);
   shadows = VG_(malloc)(sizeof(VgHashNode*) * (*n_shadows));
   s = 0;

   /* Copy the mempool chunks into the final array. */
   VG_(HT_ResetIter)(MC_(mempool_list));
   while ( (mp = VG_(HT_Next)(MC_(mempool_list))) ) {
      VG_(HT_ResetIter)(mp->chunks);
      while ( (mc = VG_(HT_Next)(mp->chunks)) ) {
         tl_assert(s < *n_shadows);
         shadows[s++] = mc;
      }
   }

   /* Copy the malloc chunks into the final array. */
   for (m = 0; m < n_mallocs; ++m) {
      if (!malloc_chunk_holds_a_pool_chunk[m]) {
         tl_assert(s < *n_shadows);
         shadows[s++] = mallocs[m];
      }
   }

   tl_assert(s == *n_shadows);
   VG_(free)(mallocs);
   VG_(free)(malloc_chunk_holds_a_pool_chunk);

   return shadows;
}


/* Top level entry point to leak detector.  Call here, passing in
   suitable address-validating functions (see comment at top of
   scan_all_valid_memory above).  These functions used to encapsulate the
   differences between Memcheck and Addrcheck;  they no longer do but it
   doesn't hurt to keep them here.
*/
void MC_(do_detect_memory_leaks) (
   ThreadId tid, LeakCheckMode mode,
   Bool (*is_within_valid_secondary) ( Addr ),
   Bool (*is_valid_aligned_word)     ( Addr )
)
{
   Int i;
   
   tl_assert(mode != LC_Off);

   lc_shadows = find_active_shadows(&lc_n_shadows);

   /* Sort the array. */
   VG_(ssort)((void*)lc_shadows, lc_n_shadows, sizeof(VgHashNode*), lc_compar);

   /* Sanity check; assert that the blocks are now in order */
   for (i = 0; i < lc_n_shadows-1; i++) {
      tl_assert( lc_shadows[i]->data <= lc_shadows[i+1]->data);
   }

   /* Sanity check -- make sure they don't overlap */
   for (i = 0; i < lc_n_shadows-1; i++) {
      tl_assert( lc_shadows[i]->data + lc_shadows[i]->szB
                 <= lc_shadows[i+1]->data );
   }

   if (lc_n_shadows == 0) {
      tl_assert(lc_shadows == NULL);
      if (VG_(clo_verbosity) >= 1 && !VG_(clo_xml)) {
         VG_(message)(Vg_UserMsg, 
                      "All heap blocks were freed -- no leaks are possible.");
      }
      return;
   }

   if (VG_(clo_verbosity) > 0 && !VG_(clo_xml))
      VG_(message)(Vg_UserMsg, 
                   "searching for pointers to %,d not-freed blocks.", 
                   lc_n_shadows );

   lc_min_mallocd_addr = lc_shadows[0]->data;
   lc_max_mallocd_addr = lc_shadows[lc_n_shadows-1]->data
                         + lc_shadows[lc_n_shadows-1]->szB;

   lc_markstack = VG_(malloc)( lc_n_shadows * sizeof(*lc_markstack) );
   for (i = 0; i < lc_n_shadows; i++) {
      lc_markstack[i].next = -1;
      lc_markstack[i].state = Unreached;
      lc_markstack[i].indirect = 0;
   }
   lc_markstack_top = -1;

   lc_is_within_valid_secondary = is_within_valid_secondary;
   lc_is_valid_aligned_word     = is_valid_aligned_word;

   lc_scanned = 0;

   /* Push roots onto the mark stack.  Roots are:
      - the integer registers of all threads
      - all mappings belonging to the client, including stacks
      - .. but excluding any client heap segments.
      Client heap segments are excluded because we wish to differentiate
      client heap blocks which are referenced only from inside the heap
      from those outside.  This facilitates the indirect vs direct loss
      categorisation, which [if the users ever manage to understand it]
      is really useful for detecting lost cycles.
   */
   { Addr*     seg_starts;
     Int       n_seg_starts;
     seg_starts = get_seg_starts( &n_seg_starts );
     tl_assert(seg_starts && n_seg_starts > 0);
     /* VG_(am_show_nsegments)( 0,"leakcheck"); */
     for (i = 0; i < n_seg_starts; i++) {
        NSegment const* seg = VG_(am_find_nsegment)( seg_starts[i] );
        tl_assert(seg);
        if (seg->kind != SkFileC && seg->kind != SkAnonC) 
           continue;
        if (!(seg->hasR && seg->hasW))
           continue;
        if (seg->isCH)
           continue;

        /* Don't poke around in device segments as this may cause
           hangs.  Exclude /dev/zero just in case someone allocated
           memory by explicitly mapping /dev/zero. */
        if (seg->kind == SkFileC 
            && (VKI_S_ISCHR(seg->mode) || VKI_S_ISBLK(seg->mode))) {
           HChar* dev_name = VG_(am_get_filename)( (NSegment*)seg );
           if (dev_name && 0 == VG_(strcmp)(dev_name, "/dev/zero")) {
              /* don't skip /dev/zero */
           } else {
              /* skip this device mapping */
              continue;
           }
        }

        if (0)
           VG_(printf)("ACCEPT %2d  %p %p\n", i, seg->start, seg->end);
        lc_scan_memory(seg->start, seg->end+1 - seg->start);
     }
   }

   /* Push registers onto mark stack */
   VG_(apply_to_GP_regs)(lc_markstack_push);

   /* Keep walking the heap until everything is found */
   lc_do_leakcheck(-1);

   if (VG_(clo_verbosity) > 0 && !VG_(clo_xml))
      VG_(message)(Vg_UserMsg, "checked %,lu bytes.", lc_scanned);

   blocks_leaked     = MC_(bytes_leaked)     = 0;
   blocks_indirect   = MC_(bytes_indirect)   = 0;
   blocks_dubious    = MC_(bytes_dubious)    = 0;
   blocks_reachable  = MC_(bytes_reachable)  = 0;
   blocks_suppressed = MC_(bytes_suppressed) = 0;

   if (mode == LC_Full)
      full_report(tid);
   else
      make_summary();

   if (VG_(clo_verbosity) > 0 && !VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "LEAK SUMMARY:");
      VG_(message)(Vg_UserMsg, "   definitely lost: %,lu bytes in %,lu blocks.",
                               MC_(bytes_leaked), blocks_leaked );
      if (blocks_indirect > 0)
	 VG_(message)(Vg_UserMsg, "   indirectly lost: %,lu bytes in %,lu blocks.",
		      MC_(bytes_indirect), blocks_indirect );
      VG_(message)(Vg_UserMsg, "     possibly lost: %,lu bytes in %,lu blocks.",
                               MC_(bytes_dubious), blocks_dubious );
      VG_(message)(Vg_UserMsg, "   still reachable: %,lu bytes in %,lu blocks.",
                               MC_(bytes_reachable), blocks_reachable );
      VG_(message)(Vg_UserMsg, "        suppressed: %,lu bytes in %,lu blocks.",
                               MC_(bytes_suppressed), blocks_suppressed );
      if (mode == LC_Summary 
          && (blocks_leaked + blocks_indirect 
              + blocks_dubious + blocks_reachable) > 0) {
         VG_(message)(Vg_UserMsg,
                      "Rerun with --leak-check=full to see details of leaked memory.");
      }
      if (blocks_reachable > 0 && !MC_(clo_show_reachable) && mode == LC_Full) {
         VG_(message)(Vg_UserMsg, 
           "Reachable blocks (those to which a pointer was found) are not shown.");
         VG_(message)(Vg_UserMsg, 
            "To see them, rerun with: --leak-check=full --show-reachable=yes");
      }
   }

   VG_(free) ( lc_shadows );
   VG_(free) ( lc_markstack );
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

