
/*--------------------------------------------------------------------*/
/*--- The leak checker, shared between Memcheck and Addrcheck.     ---*/
/*---                                              mac_leakcheck.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind skin for
   detecting memory errors, and AddrCheck, a lightweight Valgrind skin 
   for detecting memory errors.

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

#include "mac_shared.h"

/* Define to debug the memory-leak-detector. */
/* #define VG_DEBUG_LEAKCHECK */

/*------------------------------------------------------------*/
/*--- Low-level address-space scanning, for the leak       ---*/
/*--- detector.                                            ---*/
/*------------------------------------------------------------*/

static 
jmp_buf memscan_jmpbuf;


static
void vg_scan_all_valid_memory_sighandler ( Int sigNo )
{
   __builtin_longjmp(memscan_jmpbuf, 1);
}


/* Safely (avoiding SIGSEGV / SIGBUS) scan the entire valid address
   space and pass the addresses and values of all addressible,
   defined, aligned words to notify_word.  This is the basis for the
   leak detector.  Returns the number of calls made to notify_word.

   Addresses are validated 3 ways.  First we enquire whether (addr >>
   16) denotes a 64k chunk in use, by asking is_valid_64k_chunk().  If
   so, we decide for ourselves whether each x86-level (4 K) page in
   the chunk is safe to inspect.  If yes, we enquire with
   is_valid_address() whether or not each of the 1024 word-locations
   on the page is valid.  Only if so are that address and its contents
   passed to notify_word.

   This is all to avoid duplication of this machinery between the
   memcheck and addrcheck skins.  
*/
static
UInt vg_scan_all_valid_memory ( Bool is_valid_64k_chunk ( UInt ),
                                Bool is_valid_address ( Addr ),
                                void (*notify_word)( Addr, UInt ) )
{
   /* All volatile, because some gccs seem paranoid about longjmp(). */
   volatile Bool anyValid;
   volatile Addr pageBase, addr;
   volatile UInt res, numPages, page, primaryMapNo;
   volatile UInt page_first_word, nWordsNotified;

   vki_ksigaction sigbus_saved;
   vki_ksigaction sigbus_new;
   vki_ksigaction sigsegv_saved;
   vki_ksigaction sigsegv_new;
   vki_ksigset_t  blockmask_saved;
   vki_ksigset_t  unblockmask_new;

   /* Temporarily install a new sigsegv and sigbus handler, and make
      sure SIGBUS, SIGSEGV and SIGTERM are unblocked.  (Perhaps the
      first two can never be blocked anyway?)  */

   sigbus_new.ksa_handler = vg_scan_all_valid_memory_sighandler;
   sigbus_new.ksa_flags = VKI_SA_ONSTACK | VKI_SA_RESTART;
   sigbus_new.ksa_restorer = NULL;
   res = VG_(ksigemptyset)( &sigbus_new.ksa_mask );
   sk_assert(res == 0);

   sigsegv_new.ksa_handler = vg_scan_all_valid_memory_sighandler;
   sigsegv_new.ksa_flags = VKI_SA_ONSTACK | VKI_SA_RESTART;
   sigsegv_new.ksa_restorer = NULL;
   res = VG_(ksigemptyset)( &sigsegv_new.ksa_mask );
   sk_assert(res == 0+0);

   res =  VG_(ksigemptyset)( &unblockmask_new );
   res |= VG_(ksigaddset)( &unblockmask_new, VKI_SIGBUS );
   res |= VG_(ksigaddset)( &unblockmask_new, VKI_SIGSEGV );
   res |= VG_(ksigaddset)( &unblockmask_new, VKI_SIGTERM );
   sk_assert(res == 0+0+0);

   res = VG_(ksigaction)( VKI_SIGBUS, &sigbus_new, &sigbus_saved );
   sk_assert(res == 0+0+0+0);

   res = VG_(ksigaction)( VKI_SIGSEGV, &sigsegv_new, &sigsegv_saved );
   sk_assert(res == 0+0+0+0+0);

   res = VG_(ksigprocmask)( VKI_SIG_UNBLOCK, &unblockmask_new, &blockmask_saved );
   sk_assert(res == 0+0+0+0+0+0);

   /* The signal handlers are installed.  Actually do the memory scan. */
   numPages = 1 << (32-VKI_BYTES_PER_PAGE_BITS);
   sk_assert(numPages == 1048576);
   sk_assert(4096 == (1 << VKI_BYTES_PER_PAGE_BITS));

   nWordsNotified = 0;

   for (page = 0; page < numPages; page++) {

      /* Base address of this 4k page. */
      pageBase = page << VKI_BYTES_PER_PAGE_BITS;

      /* Skip if this page is in an unused 64k chunk. */
      primaryMapNo = pageBase >> 16;
      if (!is_valid_64k_chunk(primaryMapNo))
         continue;

      /* Next, establish whether or not we want to consider any
         locations on this page.  We need to do so before actually
         prodding it, because prodding it when in fact it is not
         needed can cause a page fault which under some rare
         circumstances can cause the kernel to extend the stack
         segment all the way down to here, which is seriously bad.
         Hence: */
      anyValid = False;
      for (addr = pageBase; addr < pageBase+VKI_BYTES_PER_PAGE; addr += 4) {
         if (is_valid_address(addr)) {
            anyValid = True;
            break;
         }
      }

      if (!anyValid)
         continue;  /* nothing interesting here .. move to the next page */

      /* Ok, we have to prod cautiously at the page and see if it
         explodes or not. */
      if (__builtin_setjmp(memscan_jmpbuf) == 0) {
         /* try this ... */
         page_first_word = * (volatile UInt*)pageBase;
         /* we get here if we didn't get a fault */
         /* Scan the page */
         for (addr = pageBase; addr < pageBase+VKI_BYTES_PER_PAGE; addr += 4) {
            if (is_valid_address(addr)) {
               nWordsNotified++;
               notify_word ( addr, *(UInt*)addr );
	    }
         }
      } else {
         /* We get here if reading the first word of the page caused a
            fault, which in turn caused the signal handler to longjmp.
            Ignore this page. */
         if (0)
         VG_(printf)(
            "vg_scan_all_valid_memory_sighandler: ignoring page at %p\n",
            (void*)pageBase 
         );
      }
   }

   /* Restore signal state to whatever it was before. */
   res = VG_(ksigaction)( VKI_SIGBUS, &sigbus_saved, NULL );
   sk_assert(res == 0 +0);

   res = VG_(ksigaction)( VKI_SIGSEGV, &sigsegv_saved, NULL );
   sk_assert(res == 0 +0 +0);

   res = VG_(ksigprocmask)( VKI_SIG_SETMASK, &blockmask_saved, NULL );
   sk_assert(res == 0 +0 +0 +0);

   return nWordsNotified;
}

/*------------------------------------------------------------*/
/*--- Detecting leaked (unreachable) malloc'd blocks.      ---*/
/*------------------------------------------------------------*/

/* A block is either 
   -- Proper-ly reached; a pointer to its start has been found
   -- Interior-ly reached; only an interior pointer to it has been found
   -- Unreached; so far, no pointers to any part of it have been found. 
*/
typedef 
   enum { Unreached, Interior, Proper } 
   Reachedness;

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
      UInt         num_blocks;
   }
   LossRecord;


/* Find the i such that ptr points at or inside the block described by
   shadows[i].  Return -1 if none found.  This assumes that shadows[]
   has been sorted on the ->data field. */

#ifdef VG_DEBUG_LEAKCHECK
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
      a_hi = ((Addr)shadows[i]->data) + shadows[i]->size - 1;
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
      a_mid_hi = shadows[mid]->data + shadows[mid]->size - 1;

      if (ptr < a_mid_lo) {
         hi = mid-1;
         continue;
      } 
      if (ptr > a_mid_hi) {
         lo = mid+1;
         continue;
      }
      sk_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      retVal = mid;
      break;
   }

#  ifdef VG_DEBUG_LEAKCHECK
   sk_assert(retVal == find_shadow_for_OLD ( ptr, shadows, n_shadows ));
#  endif
   /* VG_(printf)("%d\n", retVal); */
   return retVal;
}

/* Globals, for the following callback used by VG_(detect_memory_leaks). */
static MAC_Chunk**  lc_shadows;
static Int          lc_n_shadows;
static Reachedness* lc_reachedness;
static Addr         lc_min_mallocd_addr;
static Addr         lc_max_mallocd_addr;

static 
void vg_detect_memory_leaks_notify_addr ( Addr a, UInt word_at_a )
{
   Int  sh_no;
   Addr ptr;

   /* Rule out some known causes of bogus pointers.  Mostly these do
      not cause much trouble because only a few false pointers can
      ever lurk in these places.  This mainly stops it reporting that
      blocks are still reachable in stupid test programs like this

         int main (void) { char* a = malloc(100); return 0; }

      which people seem inordinately fond of writing, for some reason.  

      Note that this is a complete kludge.  It would be better to
      ignore any addresses corresponding to valgrind.so's .bss and
      .data segments, but I cannot think of a reliable way to identify
      where the .bss segment has been put.  If you can, drop me a
      line.  
   */
   if (VG_(within_stack)(a))                      return;
   if (VG_(within_m_state_static_OR_threads)(a))  return;
   if (a == (Addr)(&lc_min_mallocd_addr))         return;
   if (a == (Addr)(&lc_max_mallocd_addr))         return;

   /* OK, let's get on and do something Useful for a change. */

   ptr = (Addr)word_at_a;
   if (ptr >= lc_min_mallocd_addr && ptr <= lc_max_mallocd_addr) {
      /* Might be legitimate; we'll have to investigate further. */
      sh_no = find_shadow_for ( ptr, lc_shadows, lc_n_shadows );
      if (sh_no != -1) {
         /* Found a block at/into which ptr points. */
         sk_assert(sh_no >= 0 && sh_no < lc_n_shadows);
         sk_assert(ptr < lc_shadows[sh_no]->data + lc_shadows[sh_no]->size);
         /* Decide whether Proper-ly or Interior-ly reached. */
         if (ptr == lc_shadows[sh_no]->data) {
            if (0) VG_(printf)("pointer at %p to %p\n", a, word_at_a );
            lc_reachedness[sh_no] = Proper;
         } else {
            if (lc_reachedness[sh_no] == Unreached)
               lc_reachedness[sh_no] = Interior;
         }
      }
   }
}

/* Used for printing leak errors, avoids exposing the LossRecord type (which
   comes in as void*, requiring a cast. */
void MAC_(pp_LeakError)(void* vl, UInt n_this_record, UInt n_total_records)
{
   LossRecord* l = (LossRecord*)vl;

   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, 
                "%d bytes in %d blocks are %s in loss record %d of %d",
                l->total_bytes, l->num_blocks,
                l->loss_mode==Unreached ?    "definitely lost"
                 : (l->loss_mode==Interior ? "possibly lost"
                                           : "still reachable"),
                n_this_record, n_total_records
   );
   VG_(pp_ExeContext)(l->allocated_at);
}

Int MAC_(bytes_leaked)     = 0;
Int MAC_(bytes_dubious)    = 0;
Int MAC_(bytes_reachable)  = 0;
Int MAC_(bytes_suppressed) = 0;

/* Top level entry point to leak detector.  Call here, passing in
   suitable address-validating functions (see comment at top of
   vg_scan_all_valid_memory above).  All this is to avoid duplication
   of the leak-detection code for the Memcheck and Addrcheck skins.
   Also pass in a skin-specific function to extract the .where field
   for allocated blocks, an indication of the resolution wanted for
   distinguishing different allocation points, and whether or not
   reachable blocks should be shown.
*/
void MAC_(do_detect_memory_leaks) (
   Bool is_valid_64k_chunk ( UInt ),
   Bool is_valid_address ( Addr )
)
{
   Int    i;
   Int    blocks_leaked;
   Int    blocks_dubious;
   Int    blocks_reachable;
   Int    blocks_suppressed;
   Int    n_lossrecords;
   UInt   bytes_notified;
   Bool   is_suppressed;
   
   LossRecord* errlist;
   LossRecord* p;

   /* VG_(HashTable_to_array) allocates storage for shadows */
   lc_shadows = (MAC_Chunk**)VG_(HT_to_sorted_array)( MAC_(malloc_list),
                                                        &lc_n_shadows );

   /* Sanity check -- make sure they don't overlap */
   for (i = 0; i < lc_n_shadows-1; i++) {
      sk_assert( lc_shadows[i]->data + lc_shadows[i]->size
                 < lc_shadows[i+1]->data );
   }

   if (lc_n_shadows == 0) {
      sk_assert(lc_shadows == NULL);
      if (VG_(clo_verbosity) >= 1) {
         VG_(message)(Vg_UserMsg, 
                      "No malloc'd blocks -- no leaks are possible.");
      }
      return;
   }

   VG_(message)(Vg_UserMsg, "searching for pointers to %d not-freed blocks.", 
                lc_n_shadows );

   lc_min_mallocd_addr = lc_shadows[0]->data;
   lc_max_mallocd_addr = lc_shadows[lc_n_shadows-1]->data
                         + lc_shadows[lc_n_shadows-1]->size - 1;

   lc_reachedness = VG_(malloc)( lc_n_shadows * sizeof(Reachedness) );
   for (i = 0; i < lc_n_shadows; i++)
      lc_reachedness[i] = Unreached;

   /* Do the scan of memory. */
   bytes_notified
       = VKI_BYTES_PER_WORD
         * vg_scan_all_valid_memory (
              is_valid_64k_chunk,
              is_valid_address,
              &vg_detect_memory_leaks_notify_addr
           );

   VG_(message)(Vg_UserMsg, "checked %d bytes.", bytes_notified);

   /* Common up the lost blocks so we can print sensible error messages. */
   n_lossrecords = 0;
   errlist       = NULL;
   for (i = 0; i < lc_n_shadows; i++) {
     
      ExeContext* where = lc_shadows[i]->where;
      
      for (p = errlist; p != NULL; p = p->next) {
         if (p->loss_mode == lc_reachedness[i]
             && VG_(eq_ExeContext) ( MAC_(clo_leak_resolution),
                                     p->allocated_at, 
                                     where) ) {
            break;
	 }
      }
      if (p != NULL) {
         p->num_blocks  ++;
         p->total_bytes += lc_shadows[i]->size;
      } else {
         n_lossrecords ++;
         p = VG_(malloc)(sizeof(LossRecord));
         p->loss_mode    = lc_reachedness[i];
         p->allocated_at = where;
         p->total_bytes  = lc_shadows[i]->size;
         p->num_blocks   = 1;
         p->next         = errlist;
         errlist         = p;
      }
   }

   /* Print out the commoned-up blocks and collect summary stats. */
   blocks_leaked     = MAC_(bytes_leaked)     = 0;
   blocks_dubious    = MAC_(bytes_dubious)    = 0;
   blocks_reachable  = MAC_(bytes_reachable)  = 0;
   blocks_suppressed = MAC_(bytes_suppressed) = 0;

   for (i = 0; i < n_lossrecords; i++) {
      Bool        print_record;
      LossRecord* p_min = NULL;
      UInt        n_min = 0xFFFFFFFF;
      for (p = errlist; p != NULL; p = p->next) {
         if (p->num_blocks > 0 && p->total_bytes < n_min) {
            n_min = p->total_bytes;
            p_min = p;
         }
      }
      sk_assert(p_min != NULL);

      /* Ok to have tst==NULL;  it's only used if --gdb-attach=yes, and
         we disallow that when --leak-check=yes.  
         
         Prints the error if not suppressed, unless it's reachable (Proper)
         and --show-reachable=no */

      print_record = ( MAC_(clo_show_reachable) || Proper != p_min->loss_mode );
      is_suppressed = 
         VG_(unique_error) ( VG_(get_current_tid)(), LeakErr, (UInt)i+1,
                             (Char*)n_lossrecords, (void*) p_min,
                             p_min->allocated_at, print_record,
                             /*allow_GDB_attach*/False, /*count_error*/False );

      if (is_suppressed) {
         blocks_suppressed      += p_min->num_blocks;
         MAC_(bytes_suppressed) += p_min->total_bytes;

      } else if (Unreached  == p_min->loss_mode) {
         blocks_leaked      += p_min->num_blocks;
         MAC_(bytes_leaked) += p_min->total_bytes;

      } else if (Interior    == p_min->loss_mode) {
         blocks_dubious      += p_min->num_blocks;
         MAC_(bytes_dubious) += p_min->total_bytes;

      } else if (Proper        == p_min->loss_mode) {
         blocks_reachable      += p_min->num_blocks;
         MAC_(bytes_reachable) += p_min->total_bytes;

      } else {
         VG_(skin_panic)("generic_detect_memory_leaks: unknown loss mode");
      }
      p_min->num_blocks = 0;
   }

   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "LEAK SUMMARY:");
   VG_(message)(Vg_UserMsg, "   definitely lost: %d bytes in %d blocks.", 
                            MAC_(bytes_leaked), blocks_leaked );
   VG_(message)(Vg_UserMsg, "   possibly lost:   %d bytes in %d blocks.", 
                            MAC_(bytes_dubious), blocks_dubious );
   VG_(message)(Vg_UserMsg, "   still reachable: %d bytes in %d blocks.", 
                            MAC_(bytes_reachable), blocks_reachable );
   VG_(message)(Vg_UserMsg, "        suppressed: %d bytes in %d blocks.", 
                            MAC_(bytes_suppressed), blocks_suppressed );
   if (!MAC_(clo_show_reachable)) {
      VG_(message)(Vg_UserMsg, 
        "Reachable blocks (those to which a pointer was found) are not shown.");
      VG_(message)(Vg_UserMsg, 
         "To see them, rerun with: --show-reachable=yes");
   }
   VG_(message)(Vg_UserMsg, "");

   VG_(free) ( lc_shadows );
   VG_(free) ( lc_reachedness );
}

/*--------------------------------------------------------------------*/
/*--- end                                          mac_leakcheck.c ---*/
/*--------------------------------------------------------------------*/

