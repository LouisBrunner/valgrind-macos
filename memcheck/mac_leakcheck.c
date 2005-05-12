
/*--------------------------------------------------------------------*/
/*--- The leak checker, shared between Memcheck and Addrcheck.     ---*/
/*---                                              mac_leakcheck.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors, and AddrCheck, a lightweight Valgrind tool 
   for detecting memory errors.

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

#include <setjmp.h>
#include "mac_shared.h"

/* Define to debug the memory-leak-detector. */
#define VG_DEBUG_LEAKCHECK 0
#define VG_DEBUG_CLIQUE	   0

#define ROUNDDN(p, a)	((Addr)(p) & ~((a)-1))
#define ROUNDUP(p, a)	ROUNDDN((p)+(a)-1, (a))
#define PGROUNDDN(p)	ROUNDDN(p, VKI_PAGE_SIZE)
#define PGROUNDUP(p)	ROUNDUP(p, VKI_PAGE_SIZE)

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

/*------------------------------------------------------------*/
/*--- Detecting leaked (unreachable) malloc'd blocks.      ---*/
/*------------------------------------------------------------*/

/* A block is either 
   -- Proper-ly reached; a pointer to its start has been found
   -- Interior-ly reached; only an interior pointer to it has been found
   -- Unreached; so far, no pointers to any part of it have been found. 
   -- IndirectLeak; leaked, but referred to by another leaked block
*/
typedef enum { 
   Unreached, 
   IndirectLeak,
   Interior, 
   Proper
 } Reachedness;

/* An entry in the mark stack */
typedef struct {
   Int	next:30;		/* Index of next in mark stack */
   UInt	state:2;		/* Reachedness */
   SizeT indirect;		/* if Unreached, how much is unreachable from here */
} MarkStack;

/* A block record, used for generating err msgs. */
typedef
   struct _LossRecord {
      struct _LossRecord* next;
      /* Where these lost blocks were allocated. */
      ExeContext*  allocated_at;
      /* Their reachability. */
      Reachedness  loss_mode;
      /* Number of blocks and total # bytes involved. */
      UInt         total_bytes;
      UInt	   indirect_bytes;
      UInt         num_blocks;
   }
   LossRecord;


/* Find the i such that ptr points at or inside the block described by
   shadows[i].  Return -1 if none found.  This assumes that shadows[]
   has been sorted on the ->data field. */

#if VG_DEBUG_LEAKCHECK
/* Used to sanity-check the fast binary-search mechanism. */
static 
Int find_shadow_for_OLD ( Addr        ptr, 
                          MAC_Chunk** shadows,
                          Int         n_shadows )

{
   Int  i;
   Addr a_lo, a_hi;
   PROF_EVENT(70);
   for (i = 0; i < n_shadows; i++) {
      PROF_EVENT(71);
      a_lo = shadows[i]->data;
      a_hi = ((Addr)shadows[i]->data) + shadows[i]->size;
      if (a_lo <= ptr && ptr <= a_hi)
         return i;
   }
   return -1;
}
#endif


static 
Int find_shadow_for ( Addr        ptr, 
                      MAC_Chunk** shadows,
                      Int         n_shadows )
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
      a_mid_hi = shadows[mid]->data + shadows[mid]->size;

      if (ptr < a_mid_lo) {
         hi = mid-1;
         continue;
      } 
      if (ptr > a_mid_hi) {
         lo = mid+1;
         continue;
      }
      tl_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
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
static MAC_Chunk**  lc_shadows;
static Int          lc_n_shadows;
static MarkStack*   lc_markstack;
static Int	    lc_markstack_top;
static Addr         lc_min_mallocd_addr;
static Addr         lc_max_mallocd_addr;
static SizeT	    lc_scanned;

static Bool	  (*lc_is_within_valid_secondary) (Addr addr);
static Bool	  (*lc_is_valid_aligned_word)     (Addr addr);

static const Char *pp_lossmode(Reachedness lossmode)
{
   const Char *loss = "?";

   switch(lossmode) {
   case Unreached:	loss = "definitely lost"; break;
   case IndirectLeak:	loss = "indirectly lost"; break;
   case Interior:	loss = "possibly lost"; break;
   case Proper:		loss = "still reachable"; break;
   }

   return loss;
}

/* Used for printing leak errors, avoids exposing the LossRecord type (which
   comes in as void*, requiring a cast. */
void MAC_(pp_LeakError)(void* vl, UInt n_this_record, UInt n_total_records)
{
   LossRecord* l = (LossRecord*)vl;
   const Char *loss = pp_lossmode(l->loss_mode);

   VG_(message)(Vg_UserMsg, "");
   if (l->indirect_bytes) {
      VG_(message)(Vg_UserMsg, 
		   "%d (%d direct, %d indirect) bytes in %d blocks are %s in loss record %d of %d",
		   l->total_bytes + l->indirect_bytes, 
		   l->total_bytes, l->indirect_bytes, l->num_blocks,
		   loss, n_this_record, n_total_records);
   } else {
      VG_(message)(Vg_UserMsg, 
		   "%d bytes in %d blocks are %s in loss record %d of %d",
		   l->total_bytes, l->num_blocks,
		   loss, n_this_record, n_total_records);
   }
   VG_(pp_ExeContext)(l->allocated_at);
}

Int MAC_(bytes_leaked)     = 0;
Int MAC_(bytes_indirect)   = 0;
Int MAC_(bytes_dubious)    = 0;
Int MAC_(bytes_reachable)  = 0;
Int MAC_(bytes_suppressed) = 0;

static Int lc_compar(void* n1, void* n2)
{
   MAC_Chunk* mc1 = *(MAC_Chunk**)n1;
   MAC_Chunk* mc2 = *(MAC_Chunk**)n2;
   return (mc1->data < mc2->data ? -1 : 1);
}

/* If ptr is pointing to a heap-allocated block which hasn't been seen
   before, push it onto the mark stack.  Clique is the index of the
   clique leader; -1 if none. */
static void _lc_markstack_push(Addr ptr, Int clique)
{
   Int sh_no;

   if (!VG_(is_client_addr)(ptr)) /* quick filter */
      return;

   sh_no = find_shadow_for(ptr, lc_shadows, lc_n_shadows);

   if (VG_DEBUG_LEAKCHECK)
      VG_(printf)("ptr=%p -> block %d\n", ptr, sh_no);

   if (sh_no == -1)
      return;

   tl_assert(sh_no >= 0 && sh_no < lc_n_shadows);
   tl_assert(ptr <= lc_shadows[sh_no]->data + lc_shadows[sh_no]->size);

   if (lc_markstack[sh_no].state == Unreached) {
      if (0)
	 VG_(printf)("pushing %p-%p\n", lc_shadows[sh_no]->data, 
		     lc_shadows[sh_no]->data + lc_shadows[sh_no]->size);

      tl_assert(lc_markstack[sh_no].next == -1);
      lc_markstack[sh_no].next = lc_markstack_top;
      lc_markstack_top = sh_no;
   }

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
			      lc_shadows[sh_no]->size, lc_markstack[sh_no].indirect);
	       else
		  VG_(printf)("  %d joining %d adding %d\n", 
			      sh_no, clique, lc_shadows[sh_no]->size);
	    }

	    lc_markstack[clique].indirect += lc_shadows[sh_no]->size;
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
   _lc_markstack_push(ptr, -1);
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
static void _lc_scan_memory(Addr start, SizeT len, Int clique)
{
   Addr ptr = ROUNDUP(start, sizeof(Addr));
   Addr end = ROUNDDN(start+len, sizeof(Addr));
   vki_sigset_t sigmask;

   if (VG_DEBUG_LEAKCHECK)
      VG_(printf)("scan %p-%p\n", start, len);
   VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &sigmask);
   VG_(set_fault_catcher)(scan_all_valid_memory_catcher);

   lc_scanned += end-ptr;

   if (!VG_(is_client_addr)(ptr) ||
       !VG_(is_addressable)(ptr, sizeof(Addr), VKI_PROT_READ))
      ptr = PGROUNDUP(ptr+1);	/* first page bad */

   while (ptr < end) {
      Addr addr;

      /* Skip invalid chunks */
      if (!(*lc_is_within_valid_secondary)(ptr)) {
	 ptr = ROUNDUP(ptr+1, SECONDARY_SIZE);
	 continue;
      }

      /* Look to see if this page seems reasonble */
      if ((ptr % VKI_PAGE_SIZE) == 0) {
	 if (!VG_(is_client_addr)(ptr) ||
	     !VG_(is_addressable)(ptr, sizeof(Addr), VKI_PROT_READ))
	    ptr += VKI_PAGE_SIZE; /* bad page - skip it */
      }

      if (__builtin_setjmp(memscan_jmpbuf) == 0) {
	 if ((*lc_is_valid_aligned_word)(ptr)) {
	    addr = *(Addr *)ptr;
	    _lc_markstack_push(addr, clique);
	 } else if (0 && VG_DEBUG_LEAKCHECK)
	    VG_(printf)("%p not valid\n", ptr);
	 ptr += sizeof(Addr);
      } else {
	 /* We need to restore the signal mask, because we were
	    longjmped out of a signal handler. */
	 VG_(sigprocmask)(VKI_SIG_SETMASK, &sigmask, NULL);

	 ptr = PGROUNDUP(ptr+1);	/* bad page - skip it */
      }
   }

   VG_(sigprocmask)(VKI_SIG_SETMASK, &sigmask, NULL);
   VG_(set_fault_catcher)(NULL);
}


static void lc_scan_memory(Addr start, SizeT len)
{
   _lc_scan_memory(start, len, -1);
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

      _lc_scan_memory(lc_shadows[top]->data, lc_shadows[top]->size, clique);
   }
}

static Int    blocks_leaked;
static Int    blocks_indirect;
static Int    blocks_dubious;
static Int    blocks_reachable;
static Int    blocks_suppressed;

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
	 VG_(printf)("cliques: %d at %p -> %s\n",
		     i, lc_shadows[i]->data, pp_lossmode(lc_markstack[i].state));
      if (lc_markstack[i].state != Unreached)
	 continue;

      tl_assert(lc_markstack_top == -1);

      if (VG_DEBUG_CLIQUE)
	 VG_(printf)("%d: gathering clique %p\n", i, lc_shadows[i]->data);
      
      _lc_markstack_push(lc_shadows[i]->data, i);

      lc_do_leakcheck(i);

      tl_assert(lc_markstack_top == -1);
      tl_assert(lc_markstack[i].state == IndirectLeak);

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
             && VG_(eq_ExeContext) ( MAC_(clo_leak_resolution),
                                     p->allocated_at, 
                                     where) ) {
            break;
	 }
      }
      if (p != NULL) {
         p->num_blocks  ++;
         p->total_bytes += lc_shadows[i]->size;
	 p->indirect_bytes += lc_markstack[i].indirect;
      } else {
         n_lossrecords ++;
         p = VG_(malloc)(sizeof(LossRecord));
         p->loss_mode    = lc_markstack[i].state;
         p->allocated_at = where;
         p->total_bytes  = lc_shadows[i]->size;
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
      UInt        n_min = 0xFFFFFFFF;
      for (p = errlist; p != NULL; p = p->next) {
         if (p->num_blocks > 0 && p->total_bytes < n_min) {
            n_min = p->total_bytes + p->indirect_bytes;
            p_min = p;
         }
      }
      tl_assert(p_min != NULL);

      /* Ok to have tst==NULL;  it's only used if --gdb-attach=yes, and
         we disallow that when --leak-check=yes.  
         
         Prints the error if not suppressed, unless it's reachable (Proper or IndirectLeak)
         and --show-reachable=no */

      print_record = ( MAC_(clo_show_reachable) || 
		       Unreached == p_min->loss_mode || Interior == p_min->loss_mode );
      is_suppressed = 
         VG_(unique_error) ( tid, LeakErr, (UInt)i+1,
                             /* HACK ALERT */
                             ULong_to_Ptr((ULong)(UInt)n_lossrecords), 
                             /* end HACK ALERT */
                             (void*) p_min,
                             p_min->allocated_at, print_record,
                             /*allow_GDB_attach*/False, /*count_error*/False );

      if (is_suppressed) {
         blocks_suppressed      += p_min->num_blocks;
         MAC_(bytes_suppressed) += p_min->total_bytes;

      } else if (Unreached  == p_min->loss_mode) {
         blocks_leaked      += p_min->num_blocks;
         MAC_(bytes_leaked) += p_min->total_bytes;

      } else if (IndirectLeak  == p_min->loss_mode) {
         blocks_indirect    += p_min->num_blocks;
         MAC_(bytes_indirect)+= p_min->total_bytes;

      } else if (Interior    == p_min->loss_mode) {
         blocks_dubious      += p_min->num_blocks;
         MAC_(bytes_dubious) += p_min->total_bytes;

      } else if (Proper        == p_min->loss_mode) {
         blocks_reachable      += p_min->num_blocks;
         MAC_(bytes_reachable) += p_min->total_bytes;

      } else {
         VG_(tool_panic)("generic_detect_memory_leaks: unknown loss mode");
      }
      p_min->num_blocks = 0;
   }
}

/* Compute a quick summary of the leak check. */
static void make_summary()
{
   Int i;

   for(i = 0; i < lc_n_shadows; i++) {
      SizeT size = lc_shadows[i]->size;

      switch(lc_markstack[i].state) {
      case Unreached:
	 blocks_leaked++;
	 MAC_(bytes_leaked) += size;
	 break;

      case Proper:
	 blocks_reachable++;
	 MAC_(bytes_reachable) += size;
	 break;

      case Interior:
	 blocks_dubious++;
	 MAC_(bytes_dubious) += size;
	 break;
	 
      case IndirectLeak:	/* shouldn't happen */
	 blocks_indirect++;
	 MAC_(bytes_indirect) += size;
	 break;
      }
   }
}

/* Top level entry point to leak detector.  Call here, passing in
   suitable address-validating functions (see comment at top of
   scan_all_valid_memory above).  All this is to avoid duplication
   of the leak-detection code for Memcheck and Addrcheck.
   Also pass in a tool-specific function to extract the .where field
   for allocated blocks, an indication of the resolution wanted for
   distinguishing different allocation points, and whether or not
   reachable blocks should be shown.
*/
void MAC_(do_detect_memory_leaks) (
   ThreadId tid, LeakCheckMode mode,
   Bool (*is_within_valid_secondary) ( Addr ),
   Bool (*is_valid_aligned_word)     ( Addr )
)
{
   Int i;
   
   tl_assert(mode != LC_Off);

   /* VG_(HT_to_array) allocates storage for shadows */
   lc_shadows = (MAC_Chunk**)VG_(HT_to_array)( MAC_(malloc_list),
                                               &lc_n_shadows );

   /* Sort the array. */
   VG_(ssort)((void*)lc_shadows, lc_n_shadows, sizeof(VgHashNode*), lc_compar);

   /* Sanity check; assert that the blocks are now in order */
   for (i = 0; i < lc_n_shadows-1; i++) {
      tl_assert( lc_shadows[i]->data <= lc_shadows[i+1]->data);
   }

   /* Sanity check -- make sure they don't overlap */
   for (i = 0; i < lc_n_shadows-1; i++) {
      tl_assert( lc_shadows[i]->data + lc_shadows[i]->size
                 < lc_shadows[i+1]->data );
   }

   if (lc_n_shadows == 0) {
      tl_assert(lc_shadows == NULL);
      if (VG_(clo_verbosity) >= 1) {
         VG_(message)(Vg_UserMsg, 
                      "No malloc'd blocks -- no leaks are possible.");
      }
      return;
   }

   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, 
                   "searching for pointers to %d not-freed blocks.", 
                   lc_n_shadows );

   lc_min_mallocd_addr = lc_shadows[0]->data;
   lc_max_mallocd_addr = lc_shadows[lc_n_shadows-1]->data
                         + lc_shadows[lc_n_shadows-1]->size;

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

   /* Do the scan of memory, pushing any pointers onto the mark stack */
   VG_(find_root_memory)(lc_scan_memory);

   /* Push registers onto mark stack */
   VG_(mark_from_registers)(lc_markstack_push);

   /* Keep walking the heap until everything is found */
   lc_do_leakcheck(-1);

   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "checked %d bytes.", lc_scanned);

   blocks_leaked     = MAC_(bytes_leaked)     = 0;
   blocks_indirect   = MAC_(bytes_indirect)   = 0;
   blocks_dubious    = MAC_(bytes_dubious)    = 0;
   blocks_reachable  = MAC_(bytes_reachable)  = 0;
   blocks_suppressed = MAC_(bytes_suppressed) = 0;

   if (mode == LC_Full)
      full_report(tid);
   else
      make_summary();

   if (VG_(clo_verbosity) > 0) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "LEAK SUMMARY:");
      VG_(message)(Vg_UserMsg, "   definitely lost: %d bytes in %d blocks.", 
                               MAC_(bytes_leaked), blocks_leaked );
      if (blocks_indirect > 0)
	 VG_(message)(Vg_UserMsg, "   indirectly lost: %d bytes in %d blocks.", 
		      MAC_(bytes_indirect), blocks_indirect );
      VG_(message)(Vg_UserMsg, "     possibly lost: %d bytes in %d blocks.", 
                               MAC_(bytes_dubious), blocks_dubious );
      VG_(message)(Vg_UserMsg, "   still reachable: %d bytes in %d blocks.", 
                               MAC_(bytes_reachable), blocks_reachable );
      VG_(message)(Vg_UserMsg, "        suppressed: %d bytes in %d blocks.", 
                               MAC_(bytes_suppressed), blocks_suppressed );
      if (mode == LC_Summary && blocks_leaked > 0)
	 VG_(message)(Vg_UserMsg,
		      "Use --leak-check=full to see details of leaked memory.");
      else if (!MAC_(clo_show_reachable)) {
         VG_(message)(Vg_UserMsg, 
           "Reachable blocks (those to which a pointer was found) are not shown.");
         VG_(message)(Vg_UserMsg, 
            "To see them, rerun with: --show-reachable=yes");
      }
   }

   VG_(free) ( lc_shadows );
   VG_(free) ( lc_markstack );
}

/*--------------------------------------------------------------------*/
/*--- end                                          mac_leakcheck.c ---*/
/*--------------------------------------------------------------------*/

