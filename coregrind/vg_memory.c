
/*--------------------------------------------------------------------*/
/*--- Memory-related stuff: segment initialisation and tracking,   ---*/
/*--- stack operations                                             ---*/
/*---                                                  vg_memory.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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

/* Define to debug the memory-leak-detector. */
/* #define VG_DEBUG_LEAKCHECK */


/*--------------------------------------------------------------*/
/*--- Initialise program data/text etc on program startup.   ---*/
/*--------------------------------------------------------------*/

typedef
   struct _ExeSeg {
      Addr start;
      UInt size;
      struct _ExeSeg* next;
   }
   ExeSeg;

/* The list of current executable segments loaded.  Required so that when a
   segment is munmap'd, if it's executable we can recognise it as such and
   invalidate translations for it, and drop any basic-block specific
   information being stored.  If symbols are being used, this list will have
   the same segments recorded in it as the SegInfo symbols list (but much
   less information about each segment).
*/
static ExeSeg* exeSegsHead = NULL;

/* Prepend it -- mmaps/munmaps likely to follow a stack pattern(?) so this
   is good.
   Also check no segments overlap, which would be very bad.  Check is linear
   for each seg added (quadratic overall) but the total number should be
   small (konqueror has around 50 --njn). */
static void add_exe_segment_to_list( a, len ) 
{
   Addr lo = a;
   Addr hi = a + len - 1;
   ExeSeg* es;
   ExeSeg* es2;
   
   /* Prepend it */
   es        = (ExeSeg*)VG_(arena_malloc)(VG_AR_CORE, sizeof(ExeSeg));
   es->start = a;
   es->size  = len;
   es->next  = exeSegsHead;
   exeSegsHead = es;

   /* Check there's no overlap with the rest of the list */
   for (es2 = es->next; es2 != NULL; es2 = es2->next) {
      Addr lo2 = es2->start;
      Addr hi2 = es2->start + es2->size - 1;
      Bool overlap;
      vg_assert(lo < hi);
      vg_assert(lo2 < hi2);
      /* the main assertion */
      overlap = (lo <= lo2 && lo2 <= hi)
                 || (lo <= hi2 && hi2 <= hi);
      if (overlap) {
         VG_(printf)("\n\nOVERLAPPING EXE SEGMENTS\n"
                     "  new: start %p, size %d\n"
                     "  old: start %p, size %d\n\n",
                     es->start, es->size, es2->start, es2->size );
         vg_assert(! overlap);
      }
   }
}

static Bool remove_if_exe_segment_from_list( Addr a, UInt len )
{
   ExeSeg **prev_next_ptr = & exeSegsHead, 
          *curr = exeSegsHead;

   while (True) {
      if (curr == NULL) break;
      if (a == curr->start) break;
      prev_next_ptr = &curr->next;
      curr = curr->next;
   }
   if (curr == NULL)
      return False;

   vg_assert(*prev_next_ptr == curr);

   *prev_next_ptr = curr->next;

   VG_(arena_free)(VG_AR_CORE, curr);
   return True;
}

/* Records the exe segment in the ExeSeg list (checking for overlaps), and
   reads debug info if required.  Note the entire /proc/pid/maps file is 
   read for the debug info, but it just reads symbols for newly added exe
   segments.  This is required to find out their names if they have one.  So
   we don't use this at startup because it's overkill and can screw reading
   of /proc/pid/maps.
 */
void VG_(new_exe_segment) ( Addr a, UInt len )
{
   add_exe_segment_to_list( a, len );
   VG_(maybe_read_symbols)();
}

/* Invalidate translations as necessary (also discarding any basic
   block-specific info retained by the skin) and unload any debug
   symbols. */
// Nb: remove_if_exe_segment_from_list() and VG_(maybe_unload_symbols)()
// both ignore 'len', but that seems that's ok for most programs...  see
// comment above vg_syscalls.c:mmap_segment() et al for more details.
void VG_(remove_if_exe_segment) ( Addr a, UInt len )
{
   if (remove_if_exe_segment_from_list( a, len )) {
      VG_(invalidate_translations) ( a, len );
      VG_(maybe_unload_symbols)    ( a, len );
   }
}


static
void startup_segment_callback ( Addr start, UInt size, 
                                Char rr, Char ww, Char xx, 
                                UInt foffset, UChar* filename )
{
   UInt r_esp;
   Bool is_stack_segment;

   /* Sanity check ... if this is the executable's text segment,
      ensure it is loaded where we think it ought to be.  Any file
      name which doesn't contain ".so" is assumed to be the
      executable. */
   if (filename != NULL
       && xx == 'x'
       && VG_(strstr(filename, ".so")) == NULL
      ) {
      /* We assume this is the executable. */
      if (start != VG_ASSUMED_EXE_BASE) {
         VG_(message)(Vg_UserMsg,
                      "FATAL: executable base addr not as assumed.");
         VG_(message)(Vg_UserMsg, "name %s, actual %p, assumed %p.",
                      filename, start, VG_ASSUMED_EXE_BASE);
         VG_(message)(Vg_UserMsg,
            "One reason this could happen is that you have a shared object");
         VG_(message)(Vg_UserMsg,
            " whose name doesn't contain the characters \".so\", so Valgrind ");
         VG_(message)(Vg_UserMsg,
            "naively assumes it is the executable.  ");
         VG_(message)(Vg_UserMsg,
            "In that case, rename it appropriately.");
         VG_(core_panic)("VG_ASSUMED_EXE_BASE doesn't match reality");
      }
   }

   if (0)
      VG_(message)(Vg_DebugMsg,
                   "initial map %8x-%8x %c%c%c? %8x (%d) (%s)",
                   start,start+size,rr,ww,xx,foffset,
                   size, filename?filename:(UChar*)"NULL");

   if (rr != 'r' && xx != 'x' && ww != 'w') {
      /* Implausible as it seems, R H 6.2 generates such segments:
      40067000-400ac000 r-xp 00000000 08:05 320686 /usr/X11R6/lib/libXt.so.6.0
      400ac000-400ad000 ---p 00045000 08:05 320686 /usr/X11R6/lib/libXt.so.6.0
      400ad000-400b0000 rw-p 00045000 08:05 320686 /usr/X11R6/lib/libXt.so.6.0
      when running xedit. So just ignore them. */
      if (0)
         VG_(printf)("No permissions on a segment mapped from %s\n", 
                     filename?filename:(UChar*)"NULL");
      return;
   }

   /* This parallels what happens when we mmap some new memory */
   if (filename != NULL && xx == 'x') {
      VG_(new_exe_segment)( start, size );
   }
   VG_TRACK( new_mem_startup, start, size, rr=='r', ww=='w', xx=='x' );

   /* If this is the stack segment mark all below %esp as noaccess. */
   r_esp = VG_(baseBlock)[VGOFF_(m_esp)];
   is_stack_segment = start <= r_esp && r_esp < start+size;
   if (is_stack_segment) {
      if (0)
         VG_(message)(Vg_DebugMsg, "invalidating stack area: %x .. %x",
                      start,r_esp);
      VG_TRACK( die_mem_stack, start, r_esp-start );
   }
}


/* 1. Records exe segments from /proc/pid/maps -- always necessary, because 
      if they're munmap()ed we need to know if they were executable in order
      to discard translations.  Also checks there's no exe segment overlaps.

   2. Marks global variables that might be accessed from generated code;

   3. Sets up the end of the data segment so that vg_syscalls.c can make
      sense of calls to brk().
 */
void VG_(init_memory) ( void )
{
   /* 1 and 2 */
   VG_(read_procselfmaps) ( startup_segment_callback );

   /* 3 */
   VG_TRACK( post_mem_write, (Addr) & VG_(running_on_simd_CPU), 1 );
   VG_TRACK( post_mem_write, (Addr) & VG_(clo_trace_malloc),    1 );
   VG_TRACK( post_mem_write, (Addr) & VG_(clo_sloppy_malloc),   1 );

   /* 4 */
   VG_(init_dataseg_end_for_brk)();
}


/*------------------------------------------------------------*/
/*--- Tracking permissions around %esp changes.            ---*/
/*------------------------------------------------------------*/

/*
   The stack
   ~~~~~~~~~
   The stack's segment seems to be dynamically extended downwards
   by the kernel as the stack pointer moves down.  Initially, a
   1-page (4k) stack is allocated.  When %esp moves below that for
   the first time, presumably a page fault occurs.  The kernel
   detects that the faulting address is in the range from %esp upwards
   to the current valid stack.  It then extends the stack segment
   downwards for enough to cover the faulting address, and resumes
   the process (invisibly).  The process is unaware of any of this.

   That means that Valgrind can't spot when the stack segment is
   being extended.  Fortunately, we want to precisely and continuously
   update stack permissions around %esp, so we need to spot all
   writes to %esp anyway.

   The deal is: when %esp is assigned a lower value, the stack is
   being extended.  Create a secondary maps to fill in any holes
   between the old stack ptr and this one, if necessary.  Then 
   mark all bytes in the area just "uncovered" by this %esp change
   as write-only.

   When %esp goes back up, mark the area receded over as unreadable
   and unwritable.

   Just to record the %esp boundary conditions somewhere convenient:
   %esp always points to the lowest live byte in the stack.  All
   addresses below %esp are not live; those at and above it are.  
*/

/* Does this address look like something in or vaguely near the
   current thread's stack? */
static __attribute__((unused))
Bool is_plausible_stack_addr ( ThreadState* tst, Addr aa )
{
   UInt a = (UInt)aa;
   //PROF_EVENT(100);   PPP
   if (a <= tst->stack_highest_word && 
       a > tst->stack_highest_word - VG_PLAUSIBLE_STACK_SIZE)
      return True;
   else
      return False;
}


/* Kludgey ... how much does %esp have to change before we reckon that
   the application is switching stacks ? */
#define VG_HUGE_DELTA (VG_PLAUSIBLE_STACK_SIZE / 4)

static  __attribute__((unused))
Addr get_page_base ( Addr a )
{
   return a & ~(VKI_BYTES_PER_PAGE-1);
}

static void vg_handle_esp_assignment_SLOWLY ( Addr old_esp, Addr new_esp );

__attribute__ ((regparm (1)))
void VG_(handle_esp_assignment) ( Addr new_esp )
{
   UInt old_esp;
   Int  delta;

   VGP_MAYBE_PUSHCC(VgpStack);

   old_esp = VG_(baseBlock)[VGOFF_(m_esp)];
   delta = ((Int)new_esp) - ((Int)old_esp);

   /* Update R_ESP */
   VG_(baseBlock)[VGOFF_(m_esp)] = new_esp;

   //PROF_EVENT(101);   PPP

#  ifndef VG_DEBUG_MEMORY

   /* if (IS_ALIGNED4_ADDR(old_esp) && IS_ALIGNED4_ADDR(new_esp)) { */
   if (IS_ALIGNED4_ADDR((old_esp|new_esp))) {

      /* Deal with the most common cases fast.  These are ordered in
         the sequence most common first. */

#     ifdef VG_PROFILE_MEMORY
      // PPP
      if      (delta == - 4) PROF_EVENT(102);
      else if (delta ==   4) PROF_EVENT(103);
      else if (delta == -12) PROF_EVENT(104);
      else if (delta == - 8) PROF_EVENT(105);
      else if (delta ==  16) PROF_EVENT(106);
      else if (delta ==  12) PROF_EVENT(107);
      else if (delta ==   0) PROF_EVENT(108);
      else if (delta ==   8) PROF_EVENT(109);
      else if (delta == -16) PROF_EVENT(110);
      else if (delta ==  20) PROF_EVENT(111);
      else if (delta == -20) PROF_EVENT(112);
      else if (delta ==  24) PROF_EVENT(113);
      else if (delta == -24) PROF_EVENT(114);
      else if (delta > 0)    PROF_EVENT(115); // PPP: new: aligned_big_pos
      else                   PROF_EVENT(116); // PPP: new: aligned_big_neg
#     endif
      
      if (delta < 0 && delta > -2000) {
         VG_TRACK(new_mem_stack_aligned, new_esp, -delta);
         VGP_MAYBE_POPCC(VgpStack);
         return;
      } 
      else 
      if (delta > 0 && delta < 2000) {
         VG_TRACK(die_mem_stack_aligned, old_esp, delta);
         VGP_MAYBE_POPCC(VgpStack);
         return;
      }
      if (delta == 0) {
         VGP_MAYBE_POPCC(VgpStack);
         return;
      }
      /* otherwise fall onto the slow-but-general case */
   }

#  endif

   /* The above special cases handle 90% to 95% of all the stack
      adjustments.  The rest we give to the slow-but-general
      mechanism. */
   /* VG_(printf)("big delta %d\n", delta); */
   vg_handle_esp_assignment_SLOWLY ( old_esp, new_esp );
   VGP_MAYBE_POPCC(VgpStack);
}


static void vg_handle_esp_assignment_SLOWLY ( Addr old_esp, Addr new_esp )
{
   Int  delta;
   
   delta = ((Int)new_esp) - ((Int)old_esp);
   //VG_(printf)("delta %d (%x) %x --> %x\n", delta, delta, old_esp, new_esp);
   //PROF_EVENT(120);   PPP
   if (-(VG_HUGE_DELTA) < delta && delta < VG_HUGE_DELTA) {
      /* "Ordinary" stack change. */
      if (new_esp < old_esp) {
         /* Moving down; the stack is growing. */
         //PROF_EVENT(121); PPP
         VG_TRACK( new_mem_stack, new_esp, -delta );
      
      } else if (new_esp > old_esp) {
         /* Moving up; the stack is shrinking. */
         //PROF_EVENT(122); PPP
         VG_TRACK( die_mem_stack, old_esp, delta );

      } else {
         /* when old_esp == new_esp */
         //PROF_EVENT(123);    PPP
      }
      return;
   }

   /* %esp has changed by more than HUGE_DELTA.  We take this to mean
      that the application is switching to a new stack, for whatever
      reason, and we attempt to initialise the permissions around the
      new stack in some plausible way.  All pretty kludgey; needed to
      make netscape-4.07 run without generating thousands of error
      contexts.

      If we appear to be switching back to the main stack, don't mess
      with the permissions in the area at and above the stack ptr.
      Otherwise, we're switching to an alternative stack; make the
      area above %esp readable -- this doesn't seem right -- the right
      thing to do would be to make it writable -- but is needed to
      avoid huge numbers of errs in netscape.  To be investigated. */

   { 
#    if 0
     Addr invalid_down_to = get_page_base(new_esp) 
                            - 0 * VKI_BYTES_PER_PAGE;
     Addr valid_up_to     = get_page_base(new_esp) + VKI_BYTES_PER_PAGE
                            + 0 * VKI_BYTES_PER_PAGE;
     ThreadState* tst     = VG_(get_current_thread_state)();
#    endif
     //PROF_EVENT(124); PPP
     if (VG_(clo_verbosity) > 1)
        VG_(message)(Vg_UserMsg, "Warning: client switching stacks?  "
                                 "%%esp: %p --> %p", old_esp, new_esp);
     /* VG_(printf)("na %p,   %%esp %p,   wr %p\n",
                    invalid_down_to, new_esp, valid_up_to ); */
#    if 0
     /* JRS 20021001: following discussions with John Regehr, just
        remove this.  If a stack switch happens, it seems best not to
        mess at all with memory permissions.  Seems to work well with
        Netscape 4.X.  Really the only remaining difficulty is knowing
        exactly when a stack switch is happening. */
     VG_TRACK( die_mem_stack, invalid_down_to, new_esp - invalid_down_to );
     if (!is_plausible_stack_addr(tst, new_esp)) {
        VG_TRACK( post_mem_write, new_esp, valid_up_to - new_esp );
     }
#    endif
   }
}


/*--------------------------------------------------------------------*/
/*--- Support for memory leak detectors                            ---*/
/*--------------------------------------------------------------------*/

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
Int find_shadow_for_OLD ( Addr          ptr, 
                          ShadowChunk** shadows,
                          Int           n_shadows )

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
Int find_shadow_for ( Addr          ptr, 
                      ShadowChunk** shadows,
                      Int           n_shadows )
{
   Addr a_mid_lo, a_mid_hi;
   Int lo, mid, hi, retVal;
   /* VG_(printf)("find shadow for %p = ", ptr); */
   retVal = -1;
   lo = 0;
   hi = n_shadows-1;
   while (True) {
      /* invariant: current unsearched space is from lo to hi,
         inclusive. */
      if (lo > hi) break; /* not found */

      mid      = (lo + hi) / 2;
      a_mid_lo = shadows[mid]->data;
      a_mid_hi = ((Addr)shadows[mid]->data) + shadows[mid]->size - 1;

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
   vg_assert(retVal == find_shadow_for_OLD ( ptr, shadows, n_shadows ));
#  endif
   /* VG_(printf)("%d\n", retVal); */
   return retVal;
}



static 
void sort_malloc_shadows ( ShadowChunk** shadows, UInt n_shadows )
{
   Int   incs[14] = { 1, 4, 13, 40, 121, 364, 1093, 3280,
                      9841, 29524, 88573, 265720,
                      797161, 2391484 };
   Int          lo = 0;
   Int          hi = n_shadows-1;
   Int          i, j, h, bigN, hp;
   ShadowChunk* v;

   bigN = hi - lo + 1; if (bigN < 2) return;
   hp = 0; while (hp < 14 && incs[hp] < bigN) hp++; hp--;
   vg_assert(0 <= hp && hp < 14);

   for (; hp >= 0; hp--) {
      h = incs[hp];
      i = lo + h;
      while (1) {
         if (i > hi) break;
         v = shadows[i];
         j = i;
         while (shadows[j-h]->data > v->data) {
            shadows[j] = shadows[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         shadows[j] = v;
         i++;
      }
   }
}


/* Globals, for the callback used by VG_(detect_memory_leaks). */

static ShadowChunk** vglc_shadows;
static Int           vglc_n_shadows;
static Reachedness*  vglc_reachedness;
static Addr          vglc_min_mallocd_addr;
static Addr          vglc_max_mallocd_addr;

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
   if (VG_(within_stack)(a))                return;
   if (VG_(within_m_state_static)(a))       return;
   if (a == (Addr)(&vglc_min_mallocd_addr)) return;
   if (a == (Addr)(&vglc_max_mallocd_addr)) return;

   /* OK, let's get on and do something Useful for a change. */

   ptr = (Addr)word_at_a;
   if (ptr >= vglc_min_mallocd_addr && ptr <= vglc_max_mallocd_addr) {
      /* Might be legitimate; we'll have to investigate further. */
      sh_no = find_shadow_for ( ptr, vglc_shadows, vglc_n_shadows );
      if (sh_no != -1) {
         /* Found a block at/into which ptr points. */
         sk_assert(sh_no >= 0 && sh_no < vglc_n_shadows);
         sk_assert(ptr < vglc_shadows[sh_no]->data 
                         + vglc_shadows[sh_no]->size);
         /* Decide whether Proper-ly or Interior-ly reached. */
         if (ptr == vglc_shadows[sh_no]->data) {
            if (0) VG_(printf)("pointer at %p to %p\n", a, word_at_a );
            vglc_reachedness[sh_no] = Proper;
         } else {
            if (vglc_reachedness[sh_no] == Unreached)
               vglc_reachedness[sh_no] = Interior;
         }
      }
   }
}


/* Top level entry point to leak detector.  Call here, passing in
   suitable address-validating functions (see comment at top of
   vg_scan_all_valid_memory above).  All this is to avoid duplication
   of the leak-detection code for the Memcheck and Addrcheck skins.
   Also pass in a skin-specific function to extract the .where field
   for allocated blocks, an indication of the resolution wanted for
   distinguishing different allocation points, and whether or not
   reachable blocks should be shown.
*/
void VG_(generic_detect_memory_leaks) (
   Bool is_valid_64k_chunk ( UInt ),
   Bool is_valid_address ( Addr ),
   ExeContext* get_where ( ShadowChunk* ),
   VgRes leak_resolution,
   Bool  show_reachable
)
{
   Int    i;
   Int    blocks_leaked, bytes_leaked;
   Int    blocks_dubious, bytes_dubious;
   Int    blocks_reachable, bytes_reachable;
   Int    n_lossrecords;
   UInt   bytes_notified;
   
   LossRecord*  errlist;
   LossRecord*  p;

   /* VG_(get_malloc_shadows) allocates storage for shadows */
   vglc_shadows = VG_(get_malloc_shadows)( &vglc_n_shadows );
   if (vglc_n_shadows == 0) {
      sk_assert(vglc_shadows == NULL);
      VG_(message)(Vg_UserMsg, 
                   "No malloc'd blocks -- no leaks are possible.");
      return;
   }

   VG_(message)(Vg_UserMsg, 
                "searching for pointers to %d not-freed blocks.", 
                vglc_n_shadows );
   sort_malloc_shadows ( vglc_shadows, vglc_n_shadows );

   /* Sanity check; assert that the blocks are now in order and that
      they don't overlap. */
   for (i = 0; i < vglc_n_shadows-1; i++) {
      sk_assert( ((Addr)vglc_shadows[i]->data)
                 < ((Addr)vglc_shadows[i+1]->data) );
      sk_assert( ((Addr)vglc_shadows[i]->data) + vglc_shadows[i]->size
                 < ((Addr)vglc_shadows[i+1]->data) );
   }

   vglc_min_mallocd_addr = ((Addr)vglc_shadows[0]->data);
   vglc_max_mallocd_addr = ((Addr)vglc_shadows[vglc_n_shadows-1]->data)
                         + vglc_shadows[vglc_n_shadows-1]->size - 1;

   vglc_reachedness 
      = VG_(malloc)( vglc_n_shadows * sizeof(Reachedness) );
   for (i = 0; i < vglc_n_shadows; i++)
      vglc_reachedness[i] = Unreached;

   /* Do the scan of memory. */
   bytes_notified
       = VKI_BYTES_PER_WORD
         * vg_scan_all_valid_memory (
              is_valid_64k_chunk,
              is_valid_address,
              &vg_detect_memory_leaks_notify_addr
           );

   VG_(message)(Vg_UserMsg, "checked %d bytes.", bytes_notified);

   blocks_leaked    = bytes_leaked    = 0;
   blocks_dubious   = bytes_dubious   = 0;
   blocks_reachable = bytes_reachable = 0;

   for (i = 0; i < vglc_n_shadows; i++) {
      if (vglc_reachedness[i] == Unreached) {
         blocks_leaked++;
         bytes_leaked += vglc_shadows[i]->size;
      }
      else if (vglc_reachedness[i] == Interior) {
         blocks_dubious++;
         bytes_dubious += vglc_shadows[i]->size;
      }
      else if (vglc_reachedness[i] == Proper) {
         blocks_reachable++;
         bytes_reachable += vglc_shadows[i]->size;
      }
   }

   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "definitely lost: %d bytes in %d blocks.", 
                            bytes_leaked, blocks_leaked );
   VG_(message)(Vg_UserMsg, "possibly lost:   %d bytes in %d blocks.", 
                            bytes_dubious, blocks_dubious );
   VG_(message)(Vg_UserMsg, "still reachable: %d bytes in %d blocks.", 
                            bytes_reachable, blocks_reachable );


   /* Common up the lost blocks so we can print sensible error
      messages. */

   n_lossrecords = 0;
   errlist       = NULL;
   for (i = 0; i < vglc_n_shadows; i++) {
     
      /* 'where' stored in 'skin_extra' field; extract using function
         supplied by the calling skin. */
      ExeContext* where = get_where ( vglc_shadows[i] );

      for (p = errlist; p != NULL; p = p->next) {
         if (p->loss_mode == vglc_reachedness[i]
             && VG_(eq_ExeContext) ( leak_resolution,
                                     p->allocated_at, 
                                     where) ) {
            break;
	 }
      }
      if (p != NULL) {
         p->num_blocks  ++;
         p->total_bytes += vglc_shadows[i]->size;
      } else {
         n_lossrecords ++;
         p = VG_(malloc)(sizeof(LossRecord));
         p->loss_mode    = vglc_reachedness[i];
         p->allocated_at = where;
         p->total_bytes  = vglc_shadows[i]->size;
         p->num_blocks   = 1;
         p->next         = errlist;
         errlist         = p;
      }
   }
   
   for (i = 0; i < n_lossrecords; i++) {
      LossRecord* p_min = NULL;
      UInt        n_min = 0xFFFFFFFF;
      for (p = errlist; p != NULL; p = p->next) {
         if (p->num_blocks > 0 && p->total_bytes < n_min) {
            n_min = p->total_bytes;
            p_min = p;
         }
      }
      sk_assert(p_min != NULL);

      if ( (!show_reachable) && (p_min->loss_mode == Proper)) {
         p_min->num_blocks = 0;
         continue;
      }

      VG_(message)(Vg_UserMsg, "");
      VG_(message)(
         Vg_UserMsg,
         "%d bytes in %d blocks are %s in loss record %d of %d",
         p_min->total_bytes, p_min->num_blocks,
         p_min->loss_mode==Unreached ? "definitely lost" :
            (p_min->loss_mode==Interior ? "possibly lost"
                                        : "still reachable"),
         i+1, n_lossrecords
      );
      VG_(pp_ExeContext)(p_min->allocated_at);
      p_min->num_blocks = 0;
   }

   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, "LEAK SUMMARY:");
   VG_(message)(Vg_UserMsg, "   definitely lost: %d bytes in %d blocks.", 
                            bytes_leaked, blocks_leaked );
   VG_(message)(Vg_UserMsg, "   possibly lost:   %d bytes in %d blocks.", 
                            bytes_dubious, blocks_dubious );
   VG_(message)(Vg_UserMsg, "   still reachable: %d bytes in %d blocks.", 
                            bytes_reachable, blocks_reachable );
   if (!show_reachable) {
      VG_(message)(Vg_UserMsg, 
         "Reachable blocks (those to which a pointer was found) are not shown.");
      VG_(message)(Vg_UserMsg, 
         "To see them, rerun with: --show-reachable=yes");
   }
   VG_(message)(Vg_UserMsg, "");

   VG_(free) ( vglc_shadows );
   VG_(free) ( vglc_reachedness );
}


/*--------------------------------------------------------------------*/
/*--- end                                              vg_memory.c ---*/
/*--------------------------------------------------------------------*/

