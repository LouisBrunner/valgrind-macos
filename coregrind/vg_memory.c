
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
      VG_(printf)("No permissions on the segment named %s\n", filename);
      VG_(core_panic)("Non-readable, writable, executable segment at startup");
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
static
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

static Addr get_page_base ( Addr a )
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

   if (IS_ALIGNED4_ADDR(old_esp) && IS_ALIGNED4_ADDR(new_esp)) {

      /* Deal with the most common cases fast.  These are ordered in
         the sequence most common first. */

#     ifdef VG_PROFILE_MEMORY
      // PPP
      if      (delta = - 4) PROF_EVENT(102);
      else if (delta =   4) PROF_EVENT(103);
      else if (delta = -12) PROF_EVENT(104);
      else if (delta = - 8) PROF_EVENT(105);
      else if (delta =  16) PROF_EVENT(106);
      else if (delta =  12) PROF_EVENT(107);
      else if (delta =   0) PROF_EVENT(108);
      else if (delta =   8) PROF_EVENT(109);
      else if (delta = -16) PROF_EVENT(110);
      else if (delta =  20) PROF_EVENT(111);
      else if (delta = -20) PROF_EVENT(112);
      else if (delta =  24) PROF_EVENT(113);
      else if (delta = -24) PROF_EVENT(114);
      else if (delta > 0)   PROF_EVENT(115); // PPP: new: aligned_big_pos
      else                  PROF_EVENT(116); // PPP: new: aligned_big_neg
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

   { Addr invalid_down_to = get_page_base(new_esp) 
                            - 0 * VKI_BYTES_PER_PAGE;
     Addr valid_up_to     = get_page_base(new_esp) + VKI_BYTES_PER_PAGE
                            + 0 * VKI_BYTES_PER_PAGE;
     ThreadState* tst     = VG_(get_current_thread_state)();
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
/*--- end                                              vg_memory.c ---*/
/*--------------------------------------------------------------------*/

