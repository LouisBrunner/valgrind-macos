
/*--------------------------------------------------------------------*/
/*--- Memory-related stuff: segment initialisation and tracking,   ---*/
/*--- stack operations                                             ---*/
/*---                                                  vg_memory.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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
      VG_(invalidate_translations) ( a, len, False );
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


/* 1. Records startup segments from /proc/pid/maps.  Takes special note
      of the executable ones, because if they're munmap()ed we need to
      discard translations.  Also checks there's no exe segment overlaps.

      Note that `read_from_file' is false;  we read /proc/self/maps into a
      buffer at the start of VG_(main) so that any superblocks mmap'd by
      calls to VG_(malloc)() by SK_({pre,post}_clo_init) aren't erroneously
      thought of as being owned by the client.

   2. Sets up the end of the data segment so that vg_syscalls.c can make
      sense of calls to brk().
 */
void VG_(init_memory) ( void )
{
   /* 1 */
   VG_(read_procselfmaps) ( startup_segment_callback, /*read_from_file*/False );

   /* 2 */
   VG_(init_dataseg_end_for_brk)();

   /* kludge: some newer kernels place a "sysinfo" page up high, with
      vsyscalls in it, and possibly some other stuff in the future. */
   if (VG_(sysinfo_page_exists)) {
      VG_(new_exe_segment)( VG_(sysinfo_page_addr), 4096 );
      VG_TRACK( new_mem_startup, VG_(sysinfo_page_addr), 4096, 
                True, True, True );
     }
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

/* Kludgey ... how much does %esp have to change before we reckon that
   the application is switching stacks ? */
#define VG_PLAUSIBLE_STACK_SIZE  8000000
#define VG_HUGE_DELTA            (VG_PLAUSIBLE_STACK_SIZE / 4)

/* This function gets called if new_mem_stack and/or die_mem_stack are
   tracked by the skin, and one of the specialised cases (eg. new_mem_stack_4)
   isn't used in preference */
__attribute__((regparm(1)))
void VG_(unknown_esp_update)(Addr new_ESP)
{
   Addr old_ESP = VG_(get_archreg)(R_ESP);
   Int  delta   = (Int)new_ESP - (Int)old_ESP;

   if (delta < -(VG_HUGE_DELTA) || VG_HUGE_DELTA < delta) {
      /* %esp has changed by more than HUGE_DELTA.  We take this to mean
         that the application is switching to a new stack, for whatever
         reason. 
       
         JRS 20021001: following discussions with John Regehr, if a stack
         switch happens, it seems best not to mess at all with memory
         permissions.  Seems to work well with Netscape 4.X.  Really the
         only remaining difficulty is knowing exactly when a stack switch is
         happening. */
      if (VG_(clo_verbosity) > 1)
           VG_(message)(Vg_UserMsg, "Warning: client switching stacks?  "
                                    "%%esp: %p --> %p", old_ESP, new_ESP);
   } else if (delta < 0) {
      VG_TRACK( new_mem_stack, new_ESP, -delta );

   } else if (delta > 0) {
      VG_TRACK( die_mem_stack, old_ESP,  delta );
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                              vg_memory.c ---*/
/*--------------------------------------------------------------------*/

