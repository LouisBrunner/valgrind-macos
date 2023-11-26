/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- The address space manager: segment initialisation and        ---*/
/*--- tracking, stack operations                                   ---*/
/*---                                                              ---*/
/*--- Implementation for Linux, Darwin, Solaris and FreeBSD        ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)

/* *************************************************************
   DO NOT INCLUDE ANY OTHER FILES HERE.
   ADD NEW INCLUDES ONLY TO priv_aspacemgr.h
   AND THEN ONLY AFTER READING DIRE WARNINGS THERE TOO.
   ************************************************************* */

#include "priv_aspacemgr.h"
#include "config.h"


/* Note: many of the exported functions implemented below are
   described more fully in comments in pub_core_aspacemgr.h.
*/


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Overview.                                                 ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Purpose
   ~~~~~~~
   The purpose of the address space manager (aspacem) is:

   (1) to record the disposition of all parts of the process' address
       space at all times.

   (2) to the extent that it can, influence layout in ways favourable
       to our purposes.

   It is important to appreciate that whilst it can and does attempt
   to influence layout, and usually succeeds, it isn't possible to
   impose absolute control: in the end, the kernel is the final
   arbiter, and can always bounce our requests.

   Strategy
   ~~~~~~~~
   The strategy is therefore as follows: 

   * Track ownership of mappings.  Each one can belong either to
     Valgrind or to the client.

   * Try to place the client's fixed and hinted mappings at the
     requested addresses.  Fixed mappings are allowed anywhere except
     in areas reserved by Valgrind; the client can trash its own
     mappings if it wants.  Hinted mappings are allowed providing they
     fall entirely in free areas; if not, they will be placed by
     aspacem in a free area.

   * Anonymous mappings are allocated so as to keep Valgrind and
     client areas widely separated when possible.  If address space
     runs low, then they may become intermingled: aspacem will attempt
     to use all possible space.  But under most circumstances lack of
     address space is not a problem and so the areas will remain far
     apart.

     Searches for client space start at aspacem_cStart and will wrap
     around the end of the available space if needed.  Searches for
     Valgrind space start at aspacem_vStart and will also wrap around.
     Because aspacem_cStart is approximately at the start of the
     available space and aspacem_vStart is approximately in the
     middle, for the most part the client anonymous mappings will be
     clustered towards the start of available space, and Valgrind ones
     in the middle.

     On Solaris, searches for client space start at (aspacem_vStart - 1)
     and for Valgrind space start at (aspacem_maxAddr - 1) and go backwards.
     This simulates what kernel does - brk limit grows from bottom and mmap'ed
     objects from top. It is in contrary with Linux where data segment
     and mmap'ed objects grow from bottom (leading to early data segment
     exhaustion for tools which do not use m_replacemalloc). While Linux glibc
     can cope with this problem by employing mmap, Solaris libc treats inability
     to grow brk limit as a hard failure.

     The available space is delimited by aspacem_minAddr and
     aspacem_maxAddr.  aspacem is flexible and can operate with these
     at any (sane) setting.  For 32-bit Linux, aspacem_minAddr is set
     to some low-ish value at startup (64M) and aspacem_maxAddr is
     derived from the stack pointer at system startup.  This seems a
     reliable way to establish the initial boundaries.
     A command line option allows to change the value of aspacem_minAddr,
     so as to allow memory hungry applications to use the lowest
     part of the memory.

     64-bit Linux is similar except for the important detail that the
     upper boundary is set to 64G.  The reason is so that all
     anonymous mappings (basically all client data areas) are kept
     below 64G, since that is the maximum range that memcheck can
     track shadow memory using a fast 2-level sparse array.  It can go
     beyond that but runs much more slowly.  The 64G limit is
     arbitrary and is trivially changed.  So, with the current
     settings, programs on 64-bit Linux will appear to run out of
     address space and presumably fail at the 64G limit.  Given the
     considerable space overhead of Memcheck, that means you should be
     able to memcheckify programs that use up to about 32G natively.

   Note that the aspacem_minAddr/aspacem_maxAddr limits apply only to
   anonymous mappings.  The client can still do fixed and hinted maps
   at any addresses provided they do not overlap Valgrind's segments.
   This makes Valgrind able to load prelinked .so's at their requested
   addresses on 64-bit platforms, even if they are very high (eg,
   112TB).

   At startup, aspacem establishes the usable limits, and advises
   m_main to place the client stack at the top of the range, which on
   a 32-bit machine will be just below the real initial stack.  One
   effect of this is that self-hosting sort-of works, because an inner
   valgrind will then place its client's stack just below its own
   initial stack.
     
   The segment array and segment kinds
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   The central data structure is the segment array (segments[0
   .. nsegments_used-1]).  This covers the entire address space in
   order, giving account of every byte of it.  Free spaces are
   represented explicitly as this makes many operations simpler.
   Mergeable adjacent segments are aggressively merged so as to create
   a "normalised" representation (preen_nsegments).

   There are 7 (mutually-exclusive) segment kinds, the meaning of
   which is important:

   SkFree: a free space, which may be allocated either to Valgrind (V)
      or the client (C).

   SkAnonC: an anonymous mapping belonging to C.  For these, aspacem
      tracks a boolean indicating whether or not is is part of the
      client's heap area (can't remember why).

   SkFileC: a file mapping belonging to C.

   SkShmC: a shared memory segment belonging to C.

   SkAnonV: an anonymous mapping belonging to V.  These cover all V's
      dynamic memory needs, including non-client malloc/free areas,
      shadow memory, and the translation cache.

   SkFileV: a file mapping belonging to V.  As far as I know these are
      only created transiently for the purposes of reading debug info.

   SkResvn: a reservation segment.

   These are mostly straightforward.  Reservation segments have some
   subtlety, however.

   A reservation segment is unmapped from the kernel's point of view,
   but is an area in which aspacem will not create anonymous maps
   (either Vs or Cs).  The idea is that we will try to keep it clear
   when the choice to do so is ours.  Reservation segments are
   'invisible' from the client's point of view: it may choose to park
   a fixed mapping in the middle of one, and that's just tough -- we
   can't do anything about that.  From the client's perspective
   reservations are semantically equivalent to (although
   distinguishable from, if it makes enquiries) free areas.

   Reservations are a primitive mechanism provided for whatever
   purposes the rest of the system wants.  Currently they are used to
   reserve the expansion space into which a growdown stack is
   expanded, and into which the data segment is extended.  Note,
   though, those uses are entirely external to this module, which only
   supplies the primitives.

   Reservations may be shrunk in order that an adjoining anonymous
   mapping may be extended.  This makes dataseg/stack expansion work.
   A reservation may not be shrunk below one page.

   The advise/notify concept
   ~~~~~~~~~~~~~~~~~~~~~~~~~
   All mmap-related calls must be routed via aspacem.  Calling
   sys_mmap directly from the rest of the system is very dangerous
   because aspacem's data structures will become out of date.

   The fundamental mode of operation of aspacem is to support client
   mmaps.  Here's what happens (in ML_(generic_PRE_sys_mmap)):

   * m_syswrap intercepts the mmap call.  It examines the parameters
     and identifies the requested placement constraints.  There are
     three possibilities: no constraint (MAny), hinted (MHint, "I
     prefer X but will accept anything"), and fixed (MFixed, "X or
     nothing").

   * This request is passed to VG_(am_get_advisory).  This decides on
     a placement as described in detail in Strategy above.  It may
     also indicate that the map should fail, because it would trash
     one of Valgrind's areas, which would probably kill the system.

   * Control returns to the wrapper.  If VG_(am_get_advisory) has
     declared that the map should fail, then it must be made to do so.
     Usually, though, the request is considered acceptable, in which
     case an "advised" address is supplied.  The advised address
     replaces the original address supplied by the client, and
     MAP_FIXED is set.

     Note at this point that although aspacem has been asked for
     advice on where to place the mapping, no commitment has yet been
     made by either it or the kernel.

   * The adjusted request is handed off to the kernel.

   * The kernel's result is examined.  If the map succeeded, aspacem
     is told of the outcome (VG_(am_notify_client_mmap)), so it can
     update its records accordingly.

  This then is the central advise-notify idiom for handling client
  mmap/munmap/mprotect/shmat:

  * ask aspacem for an advised placement (or a veto)

  * if not vetoed, hand request to kernel, using the advised placement

  * examine result, and if successful, notify aspacem of the result.

  There are also many convenience functions, eg
  VG_(am_mmap_anon_fixed_client), which do both phases entirely within
  aspacem.

  To debug all this, a sync-checker is provided.  It reads
  /proc/self/maps, compares what it sees with aspacem's records, and
  complains if there is a difference.  --sanity-level=3 runs it before
  and after each syscall, which is a powerful, if slow way of finding
  buggy syscall wrappers.

  Loss of pointercheck
  ~~~~~~~~~~~~~~~~~~~~
  Up to and including Valgrind 2.4.1, x86 segmentation was used to
  enforce separation of V and C, so that wild writes by C could not
  trash V.  This got called "pointercheck".  Unfortunately, the new
  more flexible memory layout, plus the need to be portable across
  different architectures, means doing this in hardware is no longer
  viable, and doing it in software is expensive.  So at the moment we
  don't do it at all.
*/


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- The Address Space Manager's state.                        ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* ------ start of STATE for the address-space manager ------ */

/* Max number of segments we can track.  On Android, virtual address
   space is limited, so keep a low limit -- 5000 x sizef(NSegment) is
   360KB. */
#if defined(VGPV_arm_linux_android) \
    || defined(VGPV_x86_linux_android) \
    || defined(VGPV_mips32_linux_android) \
    || defined(VGPV_arm64_linux_android)
# define VG_N_SEGMENTS 5000
#else
# define VG_N_SEGMENTS 30000
#endif

/* Array [0 .. nsegments_used-1] of all mappings. */
/* Sorted by .addr field. */
/* I: len may not be zero. */
/* I: overlapping segments are not allowed. */
/* I: the segments cover the entire address space precisely. */
/* Each segment can optionally hold an index into the filename table. */

static NSegment nsegments[VG_N_SEGMENTS];
static Int      nsegments_used = 0;

#define Addr_MIN ((Addr)0)
#define Addr_MAX ((Addr)(-1ULL))

/* Limits etc */


Addr VG_(clo_aspacem_minAddr)
#if defined(VGO_linux)
   = (Addr) 0x04000000; // 64M
#elif defined(VGO_darwin)
# if VG_WORDSIZE == 4
   = (Addr) 0x00001000;
# else
   = (Addr) 0x100000000;  // 4GB page zero
# endif
#elif defined(VGO_solaris)
   = (Addr) 0x00100000; // 1MB
#elif defined(VGO_freebsd)
   = (Addr) 0x04000000; // 64M
#else
#endif


// The smallest address that aspacem will try to allocate
static Addr aspacem_minAddr = 0;

// The largest address that aspacem will try to allocate
static Addr aspacem_maxAddr = 0;

// Where aspacem will start looking for client space
static Addr aspacem_cStart = 0;

// Where aspacem will start looking for Valgrind space
static Addr aspacem_vStart = 0;


#define AM_SANITY_CHECK                                      \
   do {                                                      \
      if (VG_(clo_sanity_level) >= 3)                        \
         aspacem_assert(VG_(am_do_sync_check)                \
            (__PRETTY_FUNCTION__,__FILE__,__LINE__));        \
   } while (0) 

/* ------ end of STATE for the address-space manager ------ */

/* ------ Forwards decls ------ */
inline
static Int  find_nsegment_idx ( Addr a );

static void parse_procselfmaps (
      void (*record_mapping)( Addr addr, SizeT len, UInt prot,
                              ULong dev, ULong ino, Off64T offset,
                              const HChar* filename, Bool ignore_offset ),
      void (*record_gap)( Addr addr, SizeT len )
   );

/* ----- Hacks to do with the "commpage" on arm-linux ----- */
/* Not that I have anything against the commpage per se.  It's just
   that it's not listed in /proc/self/maps, which is a royal PITA --
   we have to fake it up, in parse_procselfmaps.

   But note also bug 254556 comment #2: this is now fixed in newer
   kernels -- it is listed as a "[vectors]" entry.  Presumably the
   fake entry made here duplicates the [vectors] entry, and so, if at
   some point in the future, we can stop supporting buggy kernels,
   then this kludge can be removed entirely, since the procmap parser
   below will read that entry in the normal way. */
#if defined(VGP_arm_linux)
#  define ARM_LINUX_FAKE_COMMPAGE_START 0xFFFF0000
#  define ARM_LINUX_FAKE_COMMPAGE_END1  0xFFFF1000
#endif

#if !defined(VKI_MAP_STACK)
/* this is only defined for FreeBSD
 * for readability, define it to 0
 * for other platforms */
#define VKI_MAP_STACK 0
#endif

/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Displaying the segment array.                             ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

static const HChar* show_SegKind ( SegKind sk )
{
   switch (sk) {
      case SkFree:  return "    ";
      case SkAnonC: return "anon";
      case SkAnonV: return "ANON";
      case SkFileC: return "file";
      case SkFileV: return "FILE";
      case SkShmC:  return "shm ";
      case SkResvn: return "RSVN";
      default:      return "????";
   }
}

static const HChar* show_ShrinkMode ( ShrinkMode sm )
{
   switch (sm) {
      case SmLower: return "SmLower";
      case SmUpper: return "SmUpper";
      case SmFixed: return "SmFixed";
      default: return "Sm?????";
   }
}

static void show_len_concisely ( /*OUT*/HChar* buf, Addr start, Addr end )
{
   const HChar* fmt;
   ULong len = ((ULong)end) - ((ULong)start) + 1;

   if (len < 10*1000*1000ULL) {
      fmt = "%7llu";
   } 
   else if (len < 999999ULL * (1ULL<<20)) {
      fmt = "%6llum";
      len >>= 20;
   }
   else if (len < 999999ULL * (1ULL<<30)) {
      fmt = "%6llug";
      len >>= 30;
   }
   else if (len < 999999ULL * (1ULL<<40)) {
      fmt = "%6llut";
      len >>= 40;
   }
   else {
      fmt = "%6llue";
      len >>= 50;
   }
   ML_(am_sprintf)(buf, fmt, len);
}

/* Show full details of an NSegment */

static void show_nsegment_full ( Int logLevel, Int segNo, const NSegment* seg )
{
   HChar len_buf[20];
   const HChar* name = ML_(am_get_segname)( seg->fnIdx );

   if (name == NULL)
      name = "(none)";

   show_len_concisely(len_buf, seg->start, seg->end);

   VG_(debugLog)(
      logLevel, "aspacem",
      "%3d: %s %010lx-%010lx %s %c%c%c%c%c %s "
      "d=0x%03llx i=%-7llu o=%-7lld (%d,%d) %s\n",
      segNo, show_SegKind(seg->kind),
      seg->start, seg->end, len_buf,
      seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
      seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-',
      seg->isCH ? 'H' : '-',
      show_ShrinkMode(seg->smode),
      seg->dev, seg->ino, seg->offset,
      ML_(am_segname_get_seqnr)(seg->fnIdx), seg->fnIdx,
      name
   );
}


/* Show an NSegment in a user-friendly-ish way. */

static void show_nsegment ( Int logLevel, Int segNo, const NSegment* seg )
{
   HChar len_buf[20];
   show_len_concisely(len_buf, seg->start, seg->end);

   switch (seg->kind) {

      case SkFree:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: %s %010lx-%010lx %s\n",
            segNo, show_SegKind(seg->kind),
            seg->start, seg->end, len_buf
         );
         break;

      case SkAnonC: case SkAnonV: case SkShmC:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: %s %010lx-%010lx %s %c%c%c%c%c\n",
            segNo, show_SegKind(seg->kind),
            seg->start, seg->end, len_buf,
            seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
            seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-',
            seg->isCH ? 'H' : '-'
         );
         break;

      case SkFileC: case SkFileV:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: %s %010lx-%010lx %s %c%c%c%c%c d=0x%03llx "
            "i=%-7llu o=%-7lld (%d,%d)\n",
            segNo, show_SegKind(seg->kind),
            seg->start, seg->end, len_buf,
            seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
            seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-', 
            seg->isCH ? 'H' : '-',
            seg->dev, seg->ino, seg->offset,
            ML_(am_segname_get_seqnr)(seg->fnIdx), seg->fnIdx
         );
         break;

      case SkResvn:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: %s %010lx-%010lx %s %c%c%c%c%c %s\n",
            segNo, show_SegKind(seg->kind),
            seg->start, seg->end, len_buf,
            seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
            seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-', 
            seg->isCH ? 'H' : '-',
            show_ShrinkMode(seg->smode)
         );
         break;

      default:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: ???? UNKNOWN SEGMENT KIND\n", 
            segNo 
         );
         break;
   }
}

/* Print out the segment array (debugging only!). */
void VG_(am_show_nsegments) ( Int logLevel, const HChar* who )
{
   Int i;
   VG_(debugLog)(logLevel, "aspacem",
                 "<<< SHOW_SEGMENTS: %s (%d segments)\n", 
                 who, nsegments_used);
   ML_(am_show_segnames)( logLevel, who);
   for (i = 0; i < nsegments_used; i++)
     show_nsegment( logLevel, i, &nsegments[i] );
   VG_(debugLog)(logLevel, "aspacem",
                 ">>>\n");
}


/* Get the filename corresponding to this segment, if known and if it
   has one. */
const HChar* VG_(am_get_filename)( NSegment const * seg )
{
   aspacem_assert(seg);
   return ML_(am_get_segname)( seg->fnIdx );
}

/* Collect up the start addresses of segments whose kind matches one of
   the kinds specified in kind_mask.
   The interface is a bit strange in order to avoid potential
   segment-creation races caused by dynamic allocation of the result
   buffer *starts.

   The function first computes how many entries in the result
   buffer *starts will be needed.  If this number <= nStarts,
   they are placed in starts[0..], and the number is returned.
   If nStarts is not large enough, nothing is written to
   starts[0..], and the negation of the size is returned.

   Correct use of this function may mean calling it multiple times in
   order to establish a suitably-sized buffer. */

Int VG_(am_get_segment_starts)( UInt kind_mask, Addr* starts, Int nStarts )
{
   Int i, j, nSegs;

   /* don't pass dumbass arguments */
   aspacem_assert(nStarts > 0);

   nSegs = 0;
   for (i = 0; i < nsegments_used; i++) {
      if ((nsegments[i].kind & kind_mask) != 0)
         nSegs++;
   }

   if (nSegs > nStarts) {
      /* The buffer isn't big enough.  Tell the caller how big it needs
         to be. */
      return -nSegs;
   }

   /* There's enough space.  So write into the result buffer. */
   aspacem_assert(nSegs <= nStarts);

   j = 0;
   for (i = 0; i < nsegments_used; i++) {
      if ((nsegments[i].kind & kind_mask) != 0)
         starts[j++] = nsegments[i].start;
   }

   aspacem_assert(j == nSegs); /* this should not fail */
   return nSegs;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Sanity checking and preening of the segment array.        ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Check representational invariants for NSegments. */

static Bool sane_NSegment ( const NSegment* s )
{
   if (s == NULL) return False;

   /* No zero sized segments and no wraparounds. */
   if (s->start > s->end) return False;

   /* require page alignment */
   if (!VG_IS_PAGE_ALIGNED(s->start)) return False;
   if (!VG_IS_PAGE_ALIGNED(s->end+1)) return False;

   switch (s->kind) {

      case SkFree:
         return 
            s->smode == SmFixed
            && s->dev == 0 && s->ino == 0 && s->offset == 0 && s->fnIdx == -1 
            && !s->hasR && !s->hasW && !s->hasX && !s->hasT
            && !s->isCH;

      case SkAnonC: case SkAnonV: case SkShmC:
         return 
            s->smode == SmFixed 
            && s->dev == 0 && s->ino == 0 && s->offset == 0 && s->fnIdx == -1
            && (s->kind==SkAnonC ? True : !s->isCH);

      case SkFileC: case SkFileV:
         return 
            s->smode == SmFixed
            && ML_(am_sane_segname)(s->fnIdx)
            && !s->isCH;

      case SkResvn: 
         return 
            s->dev == 0 && s->ino == 0 && s->offset == 0 && s->fnIdx == -1 
            && !s->hasR && !s->hasW && !s->hasX && !s->hasT
            && !s->isCH;

      default:
         return False;
   }
}


/* Try merging s2 into s1, if possible.  If successful, s1 is
   modified, and True is returned.  Otherwise s1 is unchanged and
   False is returned. */

static Bool maybe_merge_nsegments ( NSegment* s1, const NSegment* s2 )
{
   if (s1->kind != s2->kind) 
      return False;

   if (s1->end+1 != s2->start)
      return False;

   /* reject cases which would cause wraparound */
   if (s1->start > s2->end)
      return False;

   switch (s1->kind) {

      case SkFree:
         s1->end = s2->end;
         return True;

      case SkAnonC: case SkAnonV:
         if (s1->hasR == s2->hasR && s1->hasW == s2->hasW 
             && s1->hasX == s2->hasX && s1->isCH == s2->isCH) {
            s1->end = s2->end;
            s1->hasT |= s2->hasT;
            return True;
         }
         break;

      case SkFileC: case SkFileV:
         if (s1->hasR == s2->hasR 
             && s1->hasW == s2->hasW && s1->hasX == s2->hasX
             && s1->dev == s2->dev && s1->ino == s2->ino
             && s2->offset == s1->offset
                              + ((ULong)s2->start) - ((ULong)s1->start) ) {
            s1->end = s2->end;
            s1->hasT |= s2->hasT;
            ML_(am_dec_refcount)(s1->fnIdx);
            return True;
         }
         break;

      case SkShmC:
         return False;

      case SkResvn:
         if (s1->smode == SmFixed && s2->smode == SmFixed) {
            s1->end = s2->end;
            return True;
         }

      default:
         break;
   
   }

   return False;
}


/* Sanity-check and canonicalise the segment array (merge mergable
   segments).  Returns True if any segments were merged. */

static Bool preen_nsegments ( void )
{
   Int i, r, w, nsegments_used_old = nsegments_used;

   /* Pass 1: check the segment array covers the entire address space
      exactly once, and also that each segment is sane. */
   aspacem_assert(nsegments_used > 0);
   aspacem_assert(nsegments[0].start == Addr_MIN);
   aspacem_assert(nsegments[nsegments_used-1].end == Addr_MAX);

   aspacem_assert(sane_NSegment(&nsegments[0]));
   for (i = 1; i < nsegments_used; i++) {
      aspacem_assert(sane_NSegment(&nsegments[i]));
      aspacem_assert(nsegments[i-1].end+1 == nsegments[i].start);
   }

   /* Pass 2: merge as much as possible, using
      maybe_merge_segments. */
   w = 0;
   for (r = 1; r < nsegments_used; r++) {
      if (maybe_merge_nsegments(&nsegments[w], &nsegments[r])) {
         /* nothing */
      } else {
         w++;
         if (w != r) 
            nsegments[w] = nsegments[r];
      }
   }
   w++;
   aspacem_assert(w > 0 && w <= nsegments_used);
   nsegments_used = w;

   return nsegments_used != nsegments_used_old;
}


/* Check the segment array corresponds with the kernel's view of
   memory layout.  sync_check_ok returns True if no anomalies were
   found, else False.  In the latter case the mismatching segments are
   displayed. 

   The general idea is: we get the kernel to show us all its segments
   and also the gaps in between.  For each such interval, try and find
   a sequence of appropriate intervals in our segment array which
   cover or more than cover the kernel's interval, and which all have
   suitable kinds/permissions etc. 

   Although any specific kernel interval is not matched exactly to a
   valgrind interval or sequence thereof, eventually any disagreement
   on mapping boundaries will be detected.  This is because, if for
   example valgrind's intervals cover a greater range than the current
   kernel interval, it must be the case that a neighbouring free-space
   interval belonging to valgrind cannot cover the neighbouring
   free-space interval belonging to the kernel.  So the disagreement
   is detected.

   In other words, we examine each kernel interval in turn, and check
   we do not disagree over the range of that interval.  Because all of
   the address space is examined, any disagreements must eventually be
   detected.
*/

static Bool sync_check_ok = False;

static void sync_check_mapping_callback ( Addr addr, SizeT len, UInt prot,
                                          ULong dev, ULong ino, Off64T offset,
                                          const HChar* filename, Bool ignore_offset )
{
   Int  iLo, iHi, i;
   Bool sloppyXcheck, sloppyRcheck;

   /* If a problem has already been detected, don't continue comparing
      segments, so as to avoid flooding the output with error
      messages. */
#if !defined(VGO_darwin)
   /* GrP fixme not */
   if (!sync_check_ok)
      return;
#endif
   if (len == 0)
      return;

   /* The kernel should not give us wraparounds. */
   aspacem_assert(addr <= addr + len - 1); 

   iLo = find_nsegment_idx( addr );
   iHi = find_nsegment_idx( addr + len - 1 );

   /* These 5 should be guaranteed by find_nsegment_idx. */
   aspacem_assert(0 <= iLo && iLo < nsegments_used);
   aspacem_assert(0 <= iHi && iHi < nsegments_used);
   aspacem_assert(iLo <= iHi);
   aspacem_assert(nsegments[iLo].start <= addr );
   aspacem_assert(nsegments[iHi].end   >= addr + len - 1 );

   /* x86 doesn't differentiate 'x' and 'r' (at least, all except the
      most recent NX-bit enabled CPUs) and so recent kernels attempt
      to provide execute protection by placing all executable mappings
      low down in the address space and then reducing the size of the
      code segment to prevent code at higher addresses being executed.

      These kernels report which mappings are really executable in
      the /proc/self/maps output rather than mirroring what was asked
      for when each mapping was created. In order to cope with this we
      have a sloppyXcheck mode which we enable on x86 and s390 - in this
      mode we allow the kernel to report execute permission when we weren't
      expecting it but not vice versa. */
#  if defined(VGA_x86) || defined (VGA_s390x) || \
      defined(VGA_mips32) || defined(VGA_mips64)
   sloppyXcheck = True;
#  else
   sloppyXcheck = False;
#  endif

   /* Some kernels on s390 provide 'r' permission even when it was not
      explicitly requested. It seems that 'x' permission implies 'r'. 
      This behaviour also occurs on OS X. */
#  if defined(VGA_s390x) || defined(VGO_darwin)
   sloppyRcheck = True;
#  else
   sloppyRcheck = False;
#  endif

   /* NSegments iLo .. iHi inclusive should agree with the presented
      data. */
   for (i = iLo; i <= iHi; i++) {

      Bool same, cmp_offsets, cmp_devino;
      UInt seg_prot;
   
      /* compare the kernel's offering against ours. */
      same = nsegments[i].kind == SkAnonC
             || nsegments[i].kind == SkAnonV
             || nsegments[i].kind == SkFileC
             || nsegments[i].kind == SkFileV
             || nsegments[i].kind == SkShmC;

      seg_prot = 0;
      if (nsegments[i].hasR) seg_prot |= VKI_PROT_READ;
      if (nsegments[i].hasW) seg_prot |= VKI_PROT_WRITE;
      if (nsegments[i].hasX) seg_prot |= VKI_PROT_EXEC;

#if defined(VGO_darwin) || defined(VGO_freebsd)
      // GrP fixme kernel info doesn't have dev/inode
      cmp_devino = False;

      // GrP fixme V and kernel don't agree on offsets
      cmp_offsets = False;
#else
      cmp_offsets
         = nsegments[i].kind == SkFileC || nsegments[i].kind == SkFileV;

      cmp_devino
         = nsegments[i].dev != 0 || nsegments[i].ino != 0;
#endif

      /* Consider other reasons to not compare dev/inode */
#if defined(VGO_linux)
      /* bproc does some godawful hack on /dev/zero at process
         migration, which changes the name of it, and its dev & ino */
      if (filename && 0==VG_(strcmp)(filename, "/dev/zero (deleted)"))
         cmp_devino = False;

      /* hack apparently needed on MontaVista Linux */
      if (filename && VG_(strstr)(filename, "/.lib-ro/"))
         cmp_devino = False;
#endif
      
      /* If we are doing sloppy execute permission checks then we
         allow segment to have X permission when we weren't expecting
         it (but not vice versa) so if the kernel reported execute
         permission then pretend that this segment has it regardless
         of what we were expecting. */
      if (sloppyXcheck && (prot & VKI_PROT_EXEC) != 0) {
         seg_prot |= VKI_PROT_EXEC;
      }

      if (sloppyRcheck && (prot & (VKI_PROT_EXEC | VKI_PROT_READ)) ==
          (VKI_PROT_EXEC | VKI_PROT_READ)) {
         seg_prot |= VKI_PROT_READ;
      }

      same = same
             && seg_prot == prot
             && (cmp_devino
                   ? (nsegments[i].dev == dev && nsegments[i].ino == ino)
                   : True)
             && (cmp_offsets 
                   ? nsegments[i].start-nsegments[i].offset == addr-offset
                   : True);
      if (!same) {
         Addr start = addr;
         Addr end = start + len - 1;
         HChar len_buf[20];
         show_len_concisely(len_buf, start, end);

         sync_check_ok = False;

         VG_(debugLog)(
            0,"aspacem",
              "segment mismatch: V's seg 1st, kernel's 2nd:\n");
         show_nsegment_full( 0, i, &nsegments[i] );
         VG_(debugLog)(0,"aspacem", 
            "...: .... %010lx-%010lx %s %c%c%c.. ....... "
            "d=0x%03llx i=%-7llu o=%-7lld (.) m=. %s\n",
            start, end, len_buf,
            prot & VKI_PROT_READ  ? 'r' : '-',
            prot & VKI_PROT_WRITE ? 'w' : '-',
            prot & VKI_PROT_EXEC  ? 'x' : '-',
            dev, ino, offset, filename ? filename : "(none)" );

         return;
      }
   }

   /* Looks harmless.  Keep going. */
   return;
}

static void sync_check_gap_callback ( Addr addr, SizeT len )
{
   Int iLo, iHi, i;

   /* If a problem has already been detected, don't continue comparing
      segments, so as to avoid flooding the output with error
      messages. */
#if !defined(VGO_darwin)
   /* GrP fixme not */
   if (!sync_check_ok)
      return;
#endif 
   if (len == 0)
      return;

   /* The kernel should not give us wraparounds. */
   aspacem_assert(addr <= addr + len - 1); 

   iLo = find_nsegment_idx( addr );
   iHi = find_nsegment_idx( addr + len - 1 );

   /* These 5 should be guaranteed by find_nsegment_idx. */
   aspacem_assert(0 <= iLo && iLo < nsegments_used);
   aspacem_assert(0 <= iHi && iHi < nsegments_used);
   aspacem_assert(iLo <= iHi);
   aspacem_assert(nsegments[iLo].start <= addr );
   aspacem_assert(nsegments[iHi].end   >= addr + len - 1 );

   /* NSegments iLo .. iHi inclusive should agree with the presented
      data. */
   for (i = iLo; i <= iHi; i++) {

      Bool same;
   
      /* compare the kernel's offering against ours. */
      same = nsegments[i].kind == SkFree
             || nsegments[i].kind == SkResvn;

      if (!same) {
         Addr start = addr;
         Addr end = start + len - 1;
         HChar len_buf[20];
         show_len_concisely(len_buf, start, end);

         sync_check_ok = False;

         VG_(debugLog)(
            0,"aspacem",
              "segment mismatch: V's gap 1st, kernel's 2nd:\n");
         show_nsegment_full( 0, i, &nsegments[i] );
         VG_(debugLog)(0,"aspacem", 
            "   : .... %010lx-%010lx %s\n",
            start, end, len_buf);
         return;
      }
   }

   /* Looks harmless.  Keep going. */
   return;
}


/* Sanity check: check that Valgrind and the kernel agree on the
   address space layout.  Prints offending segments and call point if
   a discrepancy is detected, but does not abort the system.  Returned
   Bool is False if a discrepancy was found. */

Bool VG_(am_do_sync_check) ( const HChar* fn, 
                             const HChar* file, Int line )
{
   sync_check_ok = True;
   if (0)
      VG_(debugLog)(0,"aspacem", "do_sync_check %s:%d\n", file,line);
   parse_procselfmaps( sync_check_mapping_callback,
                       sync_check_gap_callback );
   if (!sync_check_ok) {
      VG_(debugLog)(0,"aspacem", 
                      "sync check at %s:%d (%s): FAILED\n",
                      file, line, fn);
      VG_(debugLog)(0,"aspacem", "\n");

#     if 0
      {
         HChar buf[100];   // large enough
         VG_(am_show_nsegments)(0,"post syncheck failure");
         VG_(sprintf)(buf, "/bin/cat /proc/%d/maps", VG_(getpid)());
         VG_(system)(buf);
      }
#     endif

   }
   return sync_check_ok;
}

/* Hook to allow sanity checks to be done from aspacemgr-common.c. */
void ML_(am_do_sanity_check)( void )
{
   AM_SANITY_CHECK;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Low level access / modification of the segment array.     ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Binary search the interval array for a given address.  Since the
   array covers the entire address space the search cannot fail.  The
   _WRK function does the real work.  Its caller (just below) caches
   the results thereof, to save time.  With N_CACHE of 63 we get a hit
   rate exceeding 90% when running OpenOffice.

   Re ">> 12", it doesn't matter that the page size of some targets
   might be different from 12.  Really "(a >> 12) % N_CACHE" is merely
   a hash function, and the actual cache entry is always validated
   correctly against the selected cache entry before use.
*/
/* Don't call find_nsegment_idx_WRK; use find_nsegment_idx instead. */
__attribute__((noinline))
static Int find_nsegment_idx_WRK ( Addr a )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid,
        lo = 0,
        hi = nsegments_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) {
         /* Not found.  This can't happen. */
         ML_(am_barf)("find_nsegment_idx: not found");
      }
      mid      = (lo + hi) / 2;
      a_mid_lo = nsegments[mid].start;
      a_mid_hi = nsegments[mid].end;

      if (a < a_mid_lo) { hi = mid-1; continue; }
      if (a > a_mid_hi) { lo = mid+1; continue; }
      aspacem_assert(a >= a_mid_lo && a <= a_mid_hi);
      aspacem_assert(0 <= mid && mid < nsegments_used);
      return mid;
   }
}

inline static Int find_nsegment_idx ( Addr a )
{
#  define N_CACHE 131 /*prime*/
   static Addr cache_pageno[N_CACHE];
   static Int  cache_segidx[N_CACHE];
   static Bool cache_inited = False;

#  ifdef N_Q_M_STATS
   static UWord n_q = 0;
   static UWord n_m = 0;
   n_q++;
   if (0 == (n_q & 0xFFFF))
      VG_(debugLog)(0,"xxx","find_nsegment_idx: %lu %lu\n", n_q, n_m);
#  endif

   UWord ix;

   if (LIKELY(cache_inited)) {
      /* do nothing */
   } else {
      for (ix = 0; ix < N_CACHE; ix++) {
         cache_pageno[ix] = 0;
         cache_segidx[ix] = -1;
      }
      cache_inited = True;
   }

   ix = (a >> 12) % N_CACHE;

   if ((a >> 12) == cache_pageno[ix]
       && cache_segidx[ix] >= 0
       && cache_segidx[ix] < nsegments_used
       && nsegments[cache_segidx[ix]].start <= a
       && a <= nsegments[cache_segidx[ix]].end) {
      /* hit */
      /* aspacem_assert( cache_segidx[ix] == find_nsegment_idx_WRK(a) ); */
      return cache_segidx[ix];
   }
   /* miss */
#  ifdef N_Q_M_STATS
   n_m++;
#  endif
   cache_segidx[ix] = find_nsegment_idx_WRK(a);
   cache_pageno[ix] = a >> 12;
   return cache_segidx[ix];
#  undef N_CACHE
}


/* Finds the segment containing 'a'.  Only returns non-SkFree segments. */
NSegment const * VG_(am_find_nsegment) ( Addr a )
{
   Int i = find_nsegment_idx(a);
   aspacem_assert(i >= 0 && i < nsegments_used);
   aspacem_assert(nsegments[i].start <= a);
   aspacem_assert(a <= nsegments[i].end);
   if (nsegments[i].kind == SkFree) 
      return NULL;
   else
      return &nsegments[i];
}

/* Finds an anonymous segment containing 'a'. Returned pointer is read only. */
NSegment const *VG_(am_find_anon_segment) ( Addr a )
{
   Int i = find_nsegment_idx(a);
   aspacem_assert(i >= 0 && i < nsegments_used);
   aspacem_assert(nsegments[i].start <= a);
   aspacem_assert(a <= nsegments[i].end);
   if (nsegments[i].kind == SkAnonC || nsegments[i].kind == SkAnonV)
      return &nsegments[i];
   else
      return NULL;
}

/* Map segment pointer to segment index. */
static Int segAddr_to_index ( const NSegment* seg )
{
   aspacem_assert(seg >= &nsegments[0] && seg < &nsegments[nsegments_used]);

   return seg - &nsegments[0];
}


/* Find the next segment along from 'here', if it is a non-SkFree segment. */
NSegment const * VG_(am_next_nsegment) ( const NSegment* here, Bool fwds )
{
   Int i = segAddr_to_index(here);

   if (fwds) {
      i++;
      if (i >= nsegments_used)
         return NULL;
   } else {
      i--;
      if (i < 0)
         return NULL;
   }
   if (nsegments[i].kind == SkFree) 
      return NULL;
   else
      return &nsegments[i];
}


/* Trivial fn: return the total amount of space in anonymous mappings,
   both for V and the client.  Is used for printing stats in
   out-of-memory messages. */
ULong VG_(am_get_anonsize_total)( void )
{
   Int   i;
   ULong total = 0;
   for (i = 0; i < nsegments_used; i++) {
      if (nsegments[i].kind == SkAnonC || nsegments[i].kind == SkAnonV) {
         total += (ULong)nsegments[i].end 
                  - (ULong)nsegments[i].start + 1ULL;
      }
   }
   return total;
}


/* Test if a piece of memory is addressable by client or by valgrind with at
   least the "prot" protection permissions by examining the underlying
   segments. The KINDS argument specifies the allowed segments ADDR may
   belong to in order to be considered "valid".
*/
static
Bool is_valid_for( UInt kinds, Addr start, SizeT len, UInt prot )
{
   Int  i, iLo, iHi;
   Bool needR, needW, needX;

   if (len == 0)
      return True; /* somewhat dubious case */
   if (start + len < start)
      return False; /* reject wraparounds */

   needR = toBool(prot & VKI_PROT_READ);
   needW = toBool(prot & VKI_PROT_WRITE);
   needX = toBool(prot & VKI_PROT_EXEC);

   iLo = find_nsegment_idx(start);
   aspacem_assert(start >= nsegments[iLo].start);

   if (start+len-1 <= nsegments[iLo].end) {
      /* This is a speedup hack which avoids calling find_nsegment_idx
         a second time when possible.  It is always correct to just
         use the "else" clause below, but is_valid_for_client is
         called a lot by the leak checker, so avoiding pointless calls
         to find_nsegment_idx, which can be expensive, is helpful. */
      iHi = iLo;
   } else {
      iHi = find_nsegment_idx(start + len - 1);
   }

   for (i = iLo; i <= iHi; i++) {
      if ( (nsegments[i].kind & kinds) != 0
           && (needR ? nsegments[i].hasR : True)
           && (needW ? nsegments[i].hasW : True)
           && (needX ? nsegments[i].hasX : True) ) {
         /* ok */
      } else {
         return False;
      }
   }

   return True;
}

/* Test if a piece of memory is addressable by the client with at
   least the "prot" protection permissions by examining the underlying
   segments. */
Bool VG_(am_is_valid_for_client)( Addr start, SizeT len, 
                                  UInt prot )
{
   const UInt kinds = SkFileC | SkAnonC | SkShmC;

   return is_valid_for(kinds, start, len, prot);
}

/* Variant of VG_(am_is_valid_for_client) which allows free areas to
   be consider part of the client's addressable space.  It also
   considers reservations to be allowable, since from the client's
   point of view they don't exist. */
Bool VG_(am_is_valid_for_client_or_free_or_resvn)
   ( Addr start, SizeT len, UInt prot )
{
   const UInt kinds = SkFileC | SkAnonC | SkShmC | SkFree | SkResvn;

   return is_valid_for(kinds, start, len, prot);
}

/* Checks if a piece of memory consists of either free or reservation
   segments. */
Bool VG_(am_is_free_or_resvn)( Addr start, SizeT len )
{
   const UInt kinds = SkFree | SkResvn;

   return is_valid_for(kinds, start, len, 0);
}


Bool VG_(am_is_valid_for_valgrind) ( Addr start, SizeT len, UInt prot )
{
   const UInt kinds = SkFileV | SkAnonV;

   return is_valid_for(kinds, start, len, prot);
}


/* Returns True if any part of the address range is marked as having
   translations made from it.  This is used to determine when to
   discard code, so if in doubt return True. */

static Bool any_Ts_in_range ( Addr start, SizeT len )
{
   Int iLo, iHi, i;
   aspacem_assert(len > 0);
   aspacem_assert(start + len > start);
   iLo = find_nsegment_idx(start);
   iHi = find_nsegment_idx(start + len - 1);
   for (i = iLo; i <= iHi; i++) {
      if (nsegments[i].hasT)
         return True;
   }
   return False;
}


/* Check whether ADDR looks like an address or address-to-be located in an
   extensible client stack segment. Return true if 
   (1) ADDR is located in an already mapped stack segment, OR
   (2) ADDR is located in a reservation segment into which an abutting SkAnonC
       segment can be extended. */
Bool VG_(am_addr_is_in_extensible_client_stack)( Addr addr )
{
   const NSegment *seg = nsegments + find_nsegment_idx(addr);

   switch (seg->kind) {
   case SkFree:
   case SkAnonV:
   case SkFileV:
   case SkFileC:
   case SkShmC:
      return False;

   case SkResvn: {
      if (seg->smode != SmUpper) return False;
      /* If the abutting segment towards higher addresses is an SkAnonC
         segment, then ADDR is a future stack pointer. */
      const NSegment *next = VG_(am_next_nsegment)(seg, /*forward*/ True);
      if (next == NULL || next->kind != SkAnonC) return False;

      /* OK; looks like a stack segment */
      return True;
   }

   case SkAnonC: {
      /* If the abutting segment towards lower addresses is an SkResvn
         segment, then ADDR is a stack pointer into mapped memory. */
      const NSegment *next = VG_(am_next_nsegment)(seg, /*forward*/ False);
      if (next == NULL || next->kind != SkResvn || next->smode != SmUpper)
         return False;

      /* OK; looks like a stack segment */
      return True;
   }

   default:
      aspacem_assert(0);   // should never happen
   }
}

/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Modifying the segment array, and constructing segments.   ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Split the segment containing 'a' into two, so that 'a' is
   guaranteed to be the start of a new segment.  If 'a' is already the
   start of a segment, do nothing. */

static void split_nsegment_at ( Addr a )
{
   Int i, j;

   aspacem_assert(a > 0);
   aspacem_assert(VG_IS_PAGE_ALIGNED(a));
 
   i = find_nsegment_idx(a);
   aspacem_assert(i >= 0 && i < nsegments_used);

   if (nsegments[i].start == a)
      /* 'a' is already the start point of a segment, so nothing to be
         done. */
      return;

   /* else we have to slide the segments upwards to make a hole */
   if (nsegments_used >= VG_N_SEGMENTS)
      ML_(am_barf_toolow)("VG_N_SEGMENTS");
   for (j = nsegments_used-1; j > i; j--)
      nsegments[j+1] = nsegments[j];
   nsegments_used++;

   nsegments[i+1]       = nsegments[i];
   nsegments[i+1].start = a;
   nsegments[i].end     = a-1;

   if (nsegments[i].kind == SkFileV || nsegments[i].kind == SkFileC)
      nsegments[i+1].offset 
         += ((ULong)nsegments[i+1].start) - ((ULong)nsegments[i].start);

   ML_(am_inc_refcount)(nsegments[i].fnIdx);

   aspacem_assert(sane_NSegment(&nsegments[i]));
   aspacem_assert(sane_NSegment(&nsegments[i+1]));
}


/* Do the minimum amount of segment splitting necessary to ensure that
   sLo is the first address denoted by some segment and sHi is the
   highest address denoted by some other segment.  Returns the indices
   of the lowest and highest segments in the range. */

static 
void split_nsegments_lo_and_hi ( Addr sLo, Addr sHi,
                                 /*OUT*/Int* iLo,
                                 /*OUT*/Int* iHi )
{
   aspacem_assert(sLo < sHi);
   aspacem_assert(VG_IS_PAGE_ALIGNED(sLo));
   aspacem_assert(VG_IS_PAGE_ALIGNED(sHi+1));

   if (sLo > 0)
      split_nsegment_at(sLo);
   if (sHi < sHi+1)
      split_nsegment_at(sHi+1);

   *iLo = find_nsegment_idx(sLo);
   *iHi = find_nsegment_idx(sHi);
   aspacem_assert(0 <= *iLo && *iLo < nsegments_used);
   aspacem_assert(0 <= *iHi && *iHi < nsegments_used);
   aspacem_assert(*iLo <= *iHi);
   aspacem_assert(nsegments[*iLo].start == sLo);
   aspacem_assert(nsegments[*iHi].end == sHi);
   /* Not that I'm overly paranoid or anything, definitely not :-) */
}


/* Add SEG to the collection, deleting/truncating any it overlaps.
   This deals with all the tricky cases of splitting up segments as
   needed. */

static void add_segment ( const NSegment* seg )
{
   Int  i, iLo, iHi, delta;
   Bool segment_is_sane;

   Addr sStart = seg->start;
   Addr sEnd   = seg->end;

   aspacem_assert(sStart <= sEnd);
   aspacem_assert(VG_IS_PAGE_ALIGNED(sStart));
   aspacem_assert(VG_IS_PAGE_ALIGNED(sEnd+1));

   segment_is_sane = sane_NSegment(seg);
   if (!segment_is_sane) show_nsegment_full(0,-1,seg);
   aspacem_assert(segment_is_sane);

   split_nsegments_lo_and_hi( sStart, sEnd, &iLo, &iHi );

   /* Increase the reference count of SEG's name. We need to do this
      *before* decreasing the reference count of the names of the replaced
      segments. Consider the case where the segment name of SEG and one of
      the replaced segments are the same. If the refcount of that name is 1,
      then decrementing first would put the slot for that name on the free
      list. Attempting to increment the refcount later would then fail
      because the slot is no longer allocated. */
   ML_(am_inc_refcount)(seg->fnIdx);

   /* Now iLo .. iHi inclusive is the range of segment indices which
      seg will replace.  If we're replacing more than one segment,
      slide those above the range down to fill the hole. Before doing
      that decrement the reference counters for the segments names of
      the replaced segments. */
   for (i = iLo; i <= iHi; ++i)
      ML_(am_dec_refcount)(nsegments[i].fnIdx);
   delta = iHi - iLo;
   aspacem_assert(delta >= 0);
   if (delta > 0) {
      for (i = iLo; i < nsegments_used-delta; i++)
         nsegments[i] = nsegments[i+delta];
      nsegments_used -= delta;
   }

   nsegments[iLo] = *seg;

   (void)preen_nsegments();
   if (0) VG_(am_show_nsegments)(0,"AFTER preen (add_segment)");
}


/* Clear out an NSegment record. */

static void init_nsegment ( /*OUT*/NSegment* seg )
{
   seg->kind     = SkFree;
   seg->start    = 0;
   seg->end      = 0;
   seg->smode    = SmFixed;
   seg->dev      = 0;
   seg->ino      = 0;
   seg->mode     = 0;
   seg->offset   = 0;
   seg->fnIdx    = -1;

   seg->hasR     = seg->hasW = seg->hasX = seg->hasT
                 = seg->isCH = False;
#if defined(VGO_freebsd)
   seg->isFF     = False;
   seg->ignore_offset = False;
#endif

}

/* Make an NSegment which holds a reservation. */

static void init_resvn ( /*OUT*/NSegment* seg, Addr start, Addr end )
{
   aspacem_assert(start < end);
   aspacem_assert(VG_IS_PAGE_ALIGNED(start));
   aspacem_assert(VG_IS_PAGE_ALIGNED(end+1));
   init_nsegment(seg);
   seg->kind  = SkResvn;
   seg->start = start;
   seg->end   = end;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Startup, including reading /proc/self/maps.               ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

static void read_maps_callback ( Addr addr, SizeT len, UInt prot,
                                 ULong dev, ULong ino, Off64T offset, 
                                 const HChar* filename, Bool ignore_offset )
{
   NSegment seg;
   init_nsegment( &seg );
   seg.start  = addr;
   seg.end    = addr+len-1;
   seg.dev    = dev;
   seg.ino    = ino;
   seg.offset = offset;
#if defined(VGO_freebsd)
   seg.ignore_offset = ignore_offset;
#endif
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);
   seg.hasT   = False;

   /* A segment in the initial /proc/self/maps is considered a FileV
      segment if either it has a file name associated with it or both its
      device and inode numbers are != 0. See bug #124528. */
   seg.kind = SkAnonV;
   if (filename || (dev != 0 && ino != 0)) 
      seg.kind = SkFileV;

#  if defined(VGO_darwin)
   // GrP fixme no dev/ino on darwin
   if (offset != 0) 
      seg.kind = SkFileV;
#  endif // defined(VGO_darwin)

#  if defined(VGP_arm_linux)
   /* The standard handling of entries read from /proc/self/maps will
      cause the faked up commpage segment to have type SkAnonV, which
      is a problem because it contains code we want the client to
      execute, and so later m_translate will segfault the client when
      it tries to go in there.  Hence change the ownership of it here
      to the client (SkAnonC).  The least-worst kludge I could think
      of. */
   if (addr == ARM_LINUX_FAKE_COMMPAGE_START
       && addr + len == ARM_LINUX_FAKE_COMMPAGE_END1
       && seg.kind == SkAnonV)
      seg.kind = SkAnonC;
#  endif // defined(VGP_arm_linux)

   if (filename)
      seg.fnIdx = ML_(am_allocate_segname)( filename );

   if (0) show_nsegment( 2,0, &seg );
   add_segment( &seg );
}

Bool
VG_(am_is_valid_for_aspacem_minAddr)( Addr addr, const HChar **errmsg )
{
   const Addr min = VKI_PAGE_SIZE;
#if VG_WORDSIZE == 4
   const Addr max = 0x40000000;  // 1Gb
#else
   const Addr max = 0x200000000; // 8Gb
#endif
   Bool ok = VG_IS_PAGE_ALIGNED(addr) && addr >= min && addr <= max;

   if (errmsg) {
      *errmsg = "";
      if (! ok) {
         const HChar fmt[] = "Must be a page aligned address between "
                             "0x%lx and 0x%lx";
         static HChar buf[sizeof fmt + 2 * 16];   // large enough
         ML_(am_sprintf)(buf, fmt, min, max);
         *errmsg = buf;
      }
   }
   return ok;
}

/* See description in pub_core_aspacemgr.h */
Addr VG_(am_startup) ( Addr sp_at_startup )
{
   NSegment seg;
   Addr     suggested_clstack_end;

   aspacem_assert(sizeof(Word)   == sizeof(void*));
   aspacem_assert(sizeof(Addr)   == sizeof(void*));
   aspacem_assert(sizeof(SizeT)  == sizeof(void*));
   aspacem_assert(sizeof(SSizeT) == sizeof(void*));

   /* Initialise the string table for segment names. */
   ML_(am_segnames_init)();

   /* Check that we can store the largest imaginable dev, ino and
      offset numbers in an NSegment. */
   aspacem_assert(sizeof(seg.dev)    == 8);
   aspacem_assert(sizeof(seg.ino)    == 8);
   aspacem_assert(sizeof(seg.offset) == 8);
   aspacem_assert(sizeof(seg.mode)   == 4);

   /* Add a single interval covering the entire address space. */
   init_nsegment(&seg);
   seg.kind        = SkFree;
   seg.start       = Addr_MIN;
   seg.end         = Addr_MAX;
   nsegments[0]    = seg;
   nsegments_used  = 1;

   aspacem_minAddr = VG_(clo_aspacem_minAddr);

   // --- Darwin -------------------------------------------
#if defined(VGO_darwin)

# if VG_WORDSIZE == 4
   aspacem_maxAddr = (Addr) 0xffffffff;

   aspacem_cStart = aspacem_minAddr;
   aspacem_vStart = 0xf0000000;  // 0xc0000000..0xf0000000 available
# else
   aspacem_maxAddr = (Addr) 0x7fffffffffff;

   aspacem_cStart = aspacem_minAddr;
   aspacem_vStart = 0x700000000000; // 0x7000:00000000..0x7fff:5c000000 avail
   // 0x7fff:5c000000..0x7fff:ffe00000? is stack, dyld, shared cache
# endif

   suggested_clstack_end = -1; // ignored; Mach-O specifies its stack

   // --- Freebsd ------------------------------------------
#elif defined(VGO_freebsd)


   VG_(debugLog)(2, "aspacem",
                    "        sp_at_startup = 0x%010lx (supplied)\n",
                    sp_at_startup );

# if VG_WORDSIZE == 4

   aspacem_maxAddr = VG_PGROUNDDN( sp_at_startup ) - 1;
# else
   aspacem_maxAddr = (Addr)0x2000000000ULL - 1; // 128G
#  ifdef ENABLE_INNER
   { Addr cse = VG_PGROUNDDN( sp_at_startup ) - 1;
     if (aspacem_maxAddr > cse)
        aspacem_maxAddr = cse;
   }
#    endif // ENABLE_INNER
# endif

   aspacem_cStart = aspacem_minAddr;
   aspacem_vStart = VG_PGROUNDUP((aspacem_minAddr + aspacem_maxAddr + 1) / 2);

#  ifdef ENABLE_INNER
   aspacem_vStart -= 0x10000000UL; // 512M
#  endif // ENABLE_INNER

   // starting with FreeBSD 10.4, the stack is created with a zone
   // that is marked MAP_GUARD. This zone is reserved but unmapped,
   // and fills the space up to the end of the segment
   // see man mmap

   // Version number from
   // https://www.freebsd.org/doc/en_US.ISO8859-1/books/porters-handbook/versions-10.html

   // On x86 this is 0x3FE0000
   // And on amd64 it is 0x1FFE0000 (536739840)
   // There is less of an issue on amd64 as we just choose some arbitrary address rather then trying
   // to squeeze in just below the host stack

   // Some of this is in sys/vm/vm_map.c, for instance vm_map_stack and vm_map_stack_locked
   // These refer to the kernel global sgrowsiz, which seems to be the initial size
   // of the user stack, 128k on my system
   //
   // This seems to be in the sysctl kern.sgrowsiz
   // Then there is kern.maxssiz which is the total stack size (grow size + guard area)
   // In other words guard area = maxssiz - sgrowsiz

#if (__FreeBSD_version >= 1003516)

#if 0
   // this block implements what is described above
   // this makes no changes to the regression tests
   // I'm keeping it for a rainy day.
   // note this needs
   // #include "pub_core_libcproc.h"
   SizeT kern_maxssiz;
   SizeT kern_sgrowsiz;
   SizeT sysctl_size = sizeof(SizeT);
   VG_(sysctlbyname)("kern.maxssiz", &kern_maxssiz, &sysctl_size, NULL, 0);
   VG_(sysctlbyname)("kern.sgrowsiz", &kern_sgrowsiz, &sysctl_size, NULL, 0);

   suggested_clstack_end = aspacem_maxAddr - (kern_maxssiz - kern_sgrowsiz) + VKI_PAGE_SIZE;
#endif

   suggested_clstack_end = aspacem_maxAddr - 64*1024*1024UL
                                           + VKI_PAGE_SIZE;

#else
   suggested_clstack_end = aspacem_maxAddr - 16*1024*1024UL
                                           + VKI_PAGE_SIZE;

#endif

   // --- Solaris ------------------------------------------
#elif defined(VGO_solaris)
#  if VG_WORDSIZE == 4
   /*
      Intended address space partitioning:

      ,--------------------------------, 0x00000000
      |                                |
      |--------------------------------|
      | initial stack given to V by OS |
      |--------------------------------| 0x08000000
      |          client text           |
      |--------------------------------|
      |                                |
      |                                |
      |--------------------------------|
      |          client stack          |
      |--------------------------------| 0x58000000
      |            V's text            |
      |--------------------------------|
      |                                |
      |                                |
      |--------------------------------|
      |     dynamic shared objects     |
      '--------------------------------' 0xffffffff

      */

   /* Anonymous pages need to fit under user limit (USERLIMIT32)
      which is 4KB + 16MB below the top of the 32-bit range. */
#    ifdef ENABLE_INNER
     aspacem_maxAddr = (Addr)0x4fffffff; // 1.25GB
     aspacem_vStart  = (Addr)0x40000000; // 1GB
#    else
     aspacem_maxAddr = (Addr)0xfefff000 - 1; // 4GB - 16MB - 4KB
     aspacem_vStart  = (Addr)0x50000000; // 1.25GB
#    endif
#  elif VG_WORDSIZE == 8
   /*
      Intended address space partitioning:

      ,--------------------------------, 0x00000000_00000000
      |                                |
      |--------------------------------| 0x00000000_00400000
      |          client text           |
      |--------------------------------|
      |                                |
      |                                |
      |--------------------------------|
      |          client stack          |
      |--------------------------------| 0x00000000_58000000
      |            V's text            |
      |--------------------------------|
      |                                |
      |--------------------------------|
      |     dynamic shared objects     |
      |--------------------------------| 0x0000001f_ffffffff
      |                                |
      |                                |
      |--------------------------------|
      | initial stack given to V by OS |
      '--------------------------------' 0xffffffff_ffffffff

      */

   /* Kernel likes to place objects at the end of the address space.
      However accessing memory beyond 128GB makes memcheck slow
      (see memcheck/mc_main.c, internal representation). Therefore:
      - mmapobj() syscall is emulated so that libraries are subject to
        Valgrind's aspacemgr control
      - Kernel shared pages (such as schedctl and hrt) are left as they are
        because kernel cannot be told where they should be put */
#    ifdef ENABLE_INNER
     aspacem_maxAddr = (Addr) 0x0000000fffffffff; // 64GB
     aspacem_vStart  = (Addr) 0x0000000800000000; // 32GB
#    else
     aspacem_maxAddr = (Addr) 0x0000001fffffffff; // 128GB
     aspacem_vStart  = (Addr) 0x0000001000000000; // 64GB
#    endif
#  else
#    error "Unknown word size"
#  endif

   aspacem_cStart = aspacem_minAddr;
#  ifdef ENABLE_INNER
   suggested_clstack_end = (Addr) 0x37ff0000 - 1; // 64kB below V's text
#  else
   suggested_clstack_end = (Addr) 0x57ff0000 - 1; // 64kB below V's text
#  endif

   // --- Linux --------------------------------------------
#else

   /* Establish address limits and block out unusable parts
      accordingly. */

   VG_(debugLog)(2, "aspacem", 
                    "        sp_at_startup = 0x%010lx (supplied)\n", 
                    sp_at_startup );

#  if VG_WORDSIZE == 8
     aspacem_maxAddr = (Addr)0x2000000000ULL - 1; // 128G
#    ifdef ENABLE_INNER
     { Addr cse = VG_PGROUNDDN( sp_at_startup ) - 1;
       if (aspacem_maxAddr > cse)
          aspacem_maxAddr = cse;
     }
#    endif
#  else
     aspacem_maxAddr = VG_PGROUNDDN( sp_at_startup ) - 1;
#  endif

   aspacem_cStart = aspacem_minAddr;
   aspacem_vStart = VG_PGROUNDUP(aspacem_minAddr 
                                 + (aspacem_maxAddr - aspacem_minAddr + 1) / 2);
#  ifdef ENABLE_INNER
   aspacem_vStart -= 0x20000000; // 512M
#  endif

   suggested_clstack_end = aspacem_maxAddr - 16*1024*1024ULL
                                           + VKI_PAGE_SIZE;

#endif /* #else of 'defined(VGO_solaris)' */
   // --- (end) --------------------------------------------

   aspacem_assert(VG_IS_PAGE_ALIGNED(aspacem_minAddr));
   aspacem_assert(VG_IS_PAGE_ALIGNED(aspacem_maxAddr + 1));
   aspacem_assert(VG_IS_PAGE_ALIGNED(aspacem_cStart));
   aspacem_assert(VG_IS_PAGE_ALIGNED(aspacem_vStart));
   aspacem_assert(VG_IS_PAGE_ALIGNED(suggested_clstack_end + 1));

   VG_(debugLog)(2, "aspacem", 
                    "              minAddr = 0x%010lx (computed)\n", 
                    aspacem_minAddr);
   VG_(debugLog)(2, "aspacem", 
                    "              maxAddr = 0x%010lx (computed)\n", 
                    aspacem_maxAddr);
   VG_(debugLog)(2, "aspacem", 
                    "               cStart = 0x%010lx (computed)\n", 
                    aspacem_cStart);
   VG_(debugLog)(2, "aspacem", 
                    "               vStart = 0x%010lx (computed)\n", 
                    aspacem_vStart);
   VG_(debugLog)(2, "aspacem", 
                    "suggested_clstack_end = 0x%010lx (computed)\n", 
                    suggested_clstack_end);

   if (aspacem_cStart > Addr_MIN) {
      init_resvn(&seg, Addr_MIN, aspacem_cStart-1);
      add_segment(&seg);
   }
   if (aspacem_maxAddr < Addr_MAX) {
      init_resvn(&seg, aspacem_maxAddr+1, Addr_MAX);
      add_segment(&seg);
   }

   /* Create a 1-page reservation at the notional initial
      client/valgrind boundary.  This isn't strictly necessary, but
      because the advisor does first-fit and starts searches for
      valgrind allocations at the boundary, this is kind of necessary
      in order to get it to start allocating in the right place. */
   init_resvn(&seg, aspacem_vStart,  aspacem_vStart + VKI_PAGE_SIZE - 1);
   add_segment(&seg);

   VG_(am_show_nsegments)(2, "Initial layout");

   VG_(debugLog)(2, "aspacem", "Reading /proc/self/maps\n");
   parse_procselfmaps( read_maps_callback, NULL );
   /* NB: on arm-linux, parse_procselfmaps automagically kludges up
      (iow, hands to its callbacks) a description of the ARM Commpage,
      since that's not listed in /proc/self/maps (kernel bug IMO).  We
      have to fake up its existence in parse_procselfmaps and not
      merely add it here as an extra segment, because doing the latter
      causes sync checking to fail: we see we have an extra segment in
      the segments array, which isn't listed in /proc/self/maps.
      Hence we must make it appear that /proc/self/maps contained this
      segment all along.  Sigh. */

   VG_(am_show_nsegments)(2, "With contents of /proc/self/maps");

   AM_SANITY_CHECK;
   return suggested_clstack_end;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- The core query-notify mechanism.                          ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Query aspacem to ask where a mapping should go. */

Addr VG_(am_get_advisory) ( const MapRequest*  req, 
                            Bool  forClient, 
                            /*OUT*/Bool* ok )
{
   /* This function implements allocation policy.

      The nature of the allocation request is determined by req, which
      specifies the start and length of the request and indicates
      whether the start address is mandatory, a hint, or irrelevant,
      and by forClient, which says whether this is for the client or
      for V. 

      Return values: the request can be vetoed (*ok is set to False),
      in which case the caller should not attempt to proceed with
      making the mapping.  Otherwise, *ok is set to True, the caller
      may proceed, and the preferred address at which the mapping
      should happen is returned.

      Note that this is an advisory system only: the kernel can in
      fact do whatever it likes as far as placement goes, and we have
      no absolute control over it.

      Allocations will never be granted in a reserved area.

      The Default Policy is:

        Search the address space for two free intervals: one of them
        big enough to contain the request without regard to the
        specified address (viz, as if it was a floating request) and
        the other being able to contain the request at the specified
        address (viz, as if were a fixed request).  Then, depending on
        the outcome of the search and the kind of request made, decide
        whether the request is allowable and what address to advise.

      The Default Policy is overridden by Policy Exception #1:

        If the request is for a fixed client map, we are prepared to
        grant it providing all areas inside the request are either
        free, reservations, or mappings belonging to the client.  In
        other words we are prepared to let the client trash its own
        mappings if it wants to.

      The Default Policy is overridden by Policy Exception #2:

        If the request is for a hinted client map, we are prepared to
        grant it providing all areas inside the request are either
        free or reservations.  In other words we are prepared to let 
        the client have a hinted mapping anywhere it likes provided
        it does not trash either any of its own mappings or any of 
        valgrind's mappings.
   */
   Int  i, j;
   Addr holeStart, holeEnd, holeLen;
   Bool fixed_not_required;

#if defined(VGO_solaris)
   Addr startPoint = forClient ? aspacem_vStart - 1 : aspacem_maxAddr - 1;
#else
   Addr startPoint = forClient ? aspacem_cStart : aspacem_vStart;
#endif /* VGO_solaris */

   Addr reqStart = req->rkind==MFixed || req->rkind==MHint ? req->start : 0;
   Addr reqEnd   = reqStart + req->len - 1;
   Addr reqLen   = req->len;

   /* These hold indices for segments found during search, or -1 if not
      found. */
   Int floatIdx = -1;
   Int fixedIdx = -1;

   aspacem_assert(nsegments_used > 0);

   if (0) {
      VG_(am_show_nsegments)(0,"getAdvisory");
      VG_(debugLog)(0,"aspacem", "getAdvisory 0x%lx %lu\n",
                    req->start, req->len);
   }

   /* Reject zero-length requests */
   if (req->len == 0) {
      *ok = False;
      return 0;
   }

   /* Reject wraparounds */
   if (req->start + req->len < req->start) {
      *ok = False;
      return 0;
   }

   /* ------ Implement Policy Exception #1 ------ */

   if (forClient && req->rkind == MFixed) {
      Int  iLo   = find_nsegment_idx(reqStart);
      Int  iHi   = find_nsegment_idx(reqEnd);
      Bool allow = True;
      for (i = iLo; i <= iHi; i++) {
         if (nsegments[i].kind == SkFree
             || nsegments[i].kind == SkFileC
             || nsegments[i].kind == SkAnonC
             || nsegments[i].kind == SkShmC
             || nsegments[i].kind == SkResvn) {
            /* ok */
         } else {
            allow = False;
            break;
         }
      }
      if (allow) {
         /* Acceptable.  Granted. */
         *ok = True;
         return reqStart;
      }
      /* Not acceptable.  Fail. */
      *ok = False;
      return 0;
   }

   /* ------ Implement Policy Exception #2 ------ */

   if (forClient && req->rkind == MHint) {
      Int  iLo   = find_nsegment_idx(reqStart);
      Int  iHi   = find_nsegment_idx(reqEnd);
      Bool allow = True;
      for (i = iLo; i <= iHi; i++) {
         if (nsegments[i].kind == SkFree
             || nsegments[i].kind == SkResvn) {
            /* ok */
         } else {
            allow = False;
            break;
         }
      }
      if (allow) {
         /* Acceptable.  Granted. */
         *ok = True;
         return reqStart;
      }
      /* Not acceptable.  Fall through to the default policy. */
   }

   /* ------ Implement the Default Policy ------ */

   /* Don't waste time looking for a fixed match if not requested to. */
   fixed_not_required = req->rkind == MAny || req->rkind == MAlign;

   i = find_nsegment_idx(startPoint);

#if defined(VGO_solaris)
#  define UPDATE_INDEX(index)                               \
      (index)--;                                            \
      if ((index) <= 0)                                     \
         (index) = nsegments_used - 1;
#  define ADVISE_ADDRESS(segment)                           \
       VG_PGROUNDDN((segment)->end + 1 - reqLen)
#  define ADVISE_ADDRESS_ALIGNED(segment)                   \
        VG_ROUNDDN((segment)->end + 1 - reqLen, req->start)

#else

#  define UPDATE_INDEX(index)                               \
      (index)++;                                            \
      if ((index) >= nsegments_used)                        \
         (index) = 0;
#  define ADVISE_ADDRESS(segment)                           \
      (segment)->start
#  define ADVISE_ADDRESS_ALIGNED(segment)                   \
      VG_ROUNDUP((segment)->start, req->start)
#endif /* VGO_solaris */

   /* Examine holes from index i back round to i-1.  Record the
      index first fixed hole and the first floating hole which would
      satisfy the request. */
   for (j = 0; j < nsegments_used; j++) {

      if (nsegments[i].kind != SkFree) {
         UPDATE_INDEX(i);
         continue;
      }

      holeStart = nsegments[i].start;
      holeEnd   = nsegments[i].end;

      /* Stay sane .. */
      aspacem_assert(holeStart <= holeEnd);
      aspacem_assert(aspacem_minAddr <= holeStart);
      aspacem_assert(holeEnd <= aspacem_maxAddr);

      if (req->rkind == MAlign) {
         holeStart = VG_ROUNDUP(holeStart, req->start);
         if (holeStart >= holeEnd) {
            /* This hole can't be used. */
            UPDATE_INDEX(i);
            continue;
         }
      }

      /* See if it's any use to us. */
      holeLen = holeEnd - holeStart + 1;

      if (fixedIdx == -1 && holeStart <= reqStart && reqEnd <= holeEnd)
         fixedIdx = i;

      if (floatIdx == -1 && holeLen >= reqLen)
         floatIdx = i;
  
      /* Don't waste time searching once we've found what we wanted. */
      if ((fixed_not_required || fixedIdx >= 0) && floatIdx >= 0)
         break;

      UPDATE_INDEX(i);
   }

   aspacem_assert(fixedIdx >= -1 && fixedIdx < nsegments_used);
   if (fixedIdx >= 0) 
      aspacem_assert(nsegments[fixedIdx].kind == SkFree);

   aspacem_assert(floatIdx >= -1 && floatIdx < nsegments_used);
   if (floatIdx >= 0) 
      aspacem_assert(nsegments[floatIdx].kind == SkFree);

   AM_SANITY_CHECK;

   /* Now see if we found anything which can satisfy the request. */
   switch (req->rkind) {
      case MFixed:
         if (fixedIdx >= 0) {
            *ok = True;
            return req->start;
         } else {
            *ok = False;
            return 0;
         }
         break;
      case MHint:
         if (fixedIdx >= 0) {
            *ok = True;
            return req->start;
         }
         if (floatIdx >= 0) {
            *ok = True;
            return ADVISE_ADDRESS(&nsegments[floatIdx]);
         }
         *ok = False;
         return 0;
      case MAny:
         if (floatIdx >= 0) {
            *ok = True;
            return ADVISE_ADDRESS(&nsegments[floatIdx]);
         }
         *ok = False;
         return 0;
      case MAlign:
         if (floatIdx >= 0) {
            *ok = True;
            return ADVISE_ADDRESS_ALIGNED(&nsegments[floatIdx]);
         }
         *ok = False;
         return 0;
      default: 
         break;
   }

   /*NOTREACHED*/
   ML_(am_barf)("getAdvisory: unknown request kind");
   *ok = False;
   return 0;

#undef UPDATE_INDEX
#undef ADVISE_ADDRESS
#undef ADVISE_ADDRESS_ALIGNED
}

/* Convenience wrapper for VG_(am_get_advisory) for client floating or
   fixed requests.  If start is zero, a floating request is issued; if
   nonzero, a fixed request at that address is issued.  Same comments
   about return values apply. */

Addr VG_(am_get_advisory_client_simple) ( Addr start, SizeT len, 
                                          /*OUT*/Bool* ok )
{
   MapRequest mreq;
   mreq.rkind = start==0 ? MAny : MFixed;
   mreq.start = start;
   mreq.len   = len;
   return VG_(am_get_advisory)( &mreq, True/*forClient*/, ok );
}

/* Similar to VG_(am_find_nsegment) but only returns free segments. */
static NSegment const * VG_(am_find_free_nsegment) ( Addr a )
{
   Int i = find_nsegment_idx(a);
   aspacem_assert(i >= 0 && i < nsegments_used);
   aspacem_assert(nsegments[i].start <= a);
   aspacem_assert(a <= nsegments[i].end);
   if (nsegments[i].kind == SkFree) 
      return &nsegments[i];
   else
      return NULL;
}

Bool VG_(am_covered_by_single_free_segment)
   ( Addr start, SizeT len)
{
   NSegment const* segLo = VG_(am_find_free_nsegment)( start );
   NSegment const* segHi = VG_(am_find_free_nsegment)( start + len - 1 );

   return segLo != NULL && segHi != NULL && segLo == segHi;
}


/* Notifies aspacem that the client completed an mmap successfully.
   The segment array is updated accordingly.  If the returned Bool is
   True, the caller should immediately discard translations from the
   specified address range. */

Bool
VG_(am_notify_client_mmap)( Addr a, SizeT len, UInt prot, UInt flags,
                            Int fd, Off64T offset )
{
   HChar    buf[VKI_PATH_MAX];
   ULong    dev, ino;
   UInt     mode;
   NSegment seg;
   Bool     needDiscard;

   aspacem_assert(len > 0);
   aspacem_assert(VG_IS_PAGE_ALIGNED(a));
   aspacem_assert(VG_IS_PAGE_ALIGNED(len));
   aspacem_assert(VG_IS_PAGE_ALIGNED(offset));

   /* Discard is needed if any of the just-trashed range had T. */
   needDiscard = any_Ts_in_range( a, len );

   init_nsegment( &seg );
   seg.kind   = (flags & (VKI_MAP_ANONYMOUS | VKI_MAP_STACK)) ? SkAnonC : SkFileC;
   seg.start  = a;
   seg.end    = a + len - 1;
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);
   if (!(flags & (VKI_MAP_ANONYMOUS | VKI_MAP_STACK))) {
      // Nb: We ignore offset requests in anonymous mmaps (see bug #126722)
      seg.offset = offset;
      if (ML_(am_get_fd_d_i_m)(fd, &dev, &ino, &mode)) {
         seg.dev = dev;
         seg.ino = ino;
         seg.mode = mode;
      }
      if (ML_(am_resolve_filename)(fd, buf, VKI_PATH_MAX)) {
         seg.fnIdx = ML_(am_allocate_segname)( buf );
      }
#if defined(VGO_freebsd)
      seg.isFF = (flags & VKI_MAP_FIXED);
#endif
   }
   add_segment( &seg );
   AM_SANITY_CHECK;
   return needDiscard;
}

/* Notifies aspacem that the client completed a shmat successfully.
   The segment array is updated accordingly.  If the returned Bool is
   True, the caller should immediately discard translations from the
   specified address range. */

Bool
VG_(am_notify_client_shmat)( Addr a, SizeT len, UInt prot )
{
   NSegment seg;
   Bool     needDiscard;

   aspacem_assert(len > 0);
   aspacem_assert(VG_IS_PAGE_ALIGNED(a));
   aspacem_assert(VG_IS_PAGE_ALIGNED(len));

   /* Discard is needed if any of the just-trashed range had T. */
   needDiscard = any_Ts_in_range( a, len );

   init_nsegment( &seg );
   seg.kind   = SkShmC;
   seg.start  = a;
   seg.end    = a + len - 1;
   seg.offset = 0;
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);
   add_segment( &seg );
   AM_SANITY_CHECK;
   return needDiscard;
}

/* Notifies aspacem that an mprotect was completed successfully.  The
   segment array is updated accordingly.  Note, as with
   VG_(am_notify_munmap), it is not the job of this function to reject
   stupid mprotects, for example the client doing mprotect of
   non-client areas.  Such requests should be intercepted earlier, by
   the syscall wrapper for mprotect.  This function merely records
   whatever it is told.  If the returned Bool is True, the caller
   should immediately discard translations from the specified address
   range. */

Bool VG_(am_notify_mprotect)( Addr start, SizeT len, UInt prot )
{
   Int  i, iLo, iHi;
   Bool newR, newW, newX, needDiscard;

   aspacem_assert(VG_IS_PAGE_ALIGNED(start));
   aspacem_assert(VG_IS_PAGE_ALIGNED(len));

   if (len == 0)
      return False;

   newR = toBool(prot & VKI_PROT_READ);
   newW = toBool(prot & VKI_PROT_WRITE);
   newX = toBool(prot & VKI_PROT_EXEC);

   /* Discard is needed if we're dumping X permission */
   needDiscard = any_Ts_in_range( start, len ) && !newX;

   split_nsegments_lo_and_hi( start, start+len-1, &iLo, &iHi );

   iLo = find_nsegment_idx(start);
   iHi = find_nsegment_idx(start + len - 1);

   for (i = iLo; i <= iHi; i++) {
      /* Apply the permissions to all relevant segments. */
      switch (nsegments[i].kind) {
         case SkAnonC: case SkAnonV: case SkFileC: case SkFileV: case SkShmC:
            nsegments[i].hasR = newR;
            nsegments[i].hasW = newW;
            nsegments[i].hasX = newX;
            aspacem_assert(sane_NSegment(&nsegments[i]));
            break;
         default:
            break;
      }
   }

   /* Changing permissions could have made previously un-mergable
      segments mergeable.  Therefore have to re-preen them. */
   (void)preen_nsegments();
   AM_SANITY_CHECK;
   return needDiscard;
}


/* Notifies aspacem that an munmap completed successfully.  The
   segment array is updated accordingly.  As with
   VG_(am_notify_mprotect), we merely record the given info, and don't
   check it for sensibleness.  If the returned Bool is True, the
   caller should immediately discard translations from the specified
   address range. */

Bool VG_(am_notify_munmap)( Addr start, SizeT len )
{
   NSegment seg;
   Bool     needDiscard;
   aspacem_assert(VG_IS_PAGE_ALIGNED(start));
   aspacem_assert(VG_IS_PAGE_ALIGNED(len));

   if (len == 0)
      return False;

   needDiscard = any_Ts_in_range( start, len );

   init_nsegment( &seg );
   seg.start = start;
   seg.end   = start + len - 1;

   /* The segment becomes unused (free).  Segments from above
      aspacem_maxAddr were originally SkResvn and so we make them so
      again.  Note, this isn't really right when the segment straddles
      the aspacem_maxAddr boundary - then really it should be split in
      two, the lower part marked as SkFree and the upper part as
      SkResvn.  Ah well. */
   if (start > aspacem_maxAddr 
       && /* check previous comparison is meaningful */
          aspacem_maxAddr < Addr_MAX)
      seg.kind = SkResvn;
   else 
   /* Ditto for segments from below aspacem_minAddr. */
   if (seg.end < aspacem_minAddr && aspacem_minAddr > 0)
      seg.kind = SkResvn;
   else
      seg.kind = SkFree;

   add_segment( &seg );

   /* Unmapping could create two adjacent free segments, so a preen is
      needed.  add_segment() will do that, so no need to here. */
   AM_SANITY_CHECK;
   return needDiscard;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Handling mappings which do not arise directly from the    ---*/
/*--- simulation of the client.                                 ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* --- --- --- map, unmap, protect  --- --- --- */

/* Map a file at a fixed address for the client, and update the
   segment array accordingly. */

SysRes VG_(am_mmap_file_fixed_client)
     ( Addr start, SizeT length, UInt prot, Int fd, Off64T offset )
{
   UInt flags = VKI_MAP_FIXED | VKI_MAP_PRIVATE;
   return VG_(am_mmap_named_file_fixed_client_flags)(start, length, prot, flags,
                                                     fd, offset, NULL);
}

SysRes VG_(am_mmap_file_fixed_client_flags)
     ( Addr start, SizeT length, UInt prot, UInt flags, Int fd, Off64T offset )
{
   return VG_(am_mmap_named_file_fixed_client_flags)(start, length, prot, flags,
                                                     fd, offset, NULL);
}

SysRes VG_(am_mmap_named_file_fixed_client)
     ( Addr start, SizeT length, UInt prot, Int fd, Off64T offset, const HChar *name )
{
   UInt flags = VKI_MAP_FIXED | VKI_MAP_PRIVATE;
   return VG_(am_mmap_named_file_fixed_client_flags)(start, length, prot, flags,
                                                     fd, offset, name);
}

SysRes VG_(am_mmap_named_file_fixed_client_flags)
     ( Addr start, SizeT length, UInt prot, UInt flags,
       Int fd, Off64T offset, const HChar *name )
{
   SysRes     sres;
   NSegment   seg;
   Addr       advised;
   Bool       ok;
   MapRequest req;
   ULong      dev, ino;
   UInt       mode;
   HChar      buf[VKI_PATH_MAX];

   /* Not allowable. */
   if (length == 0 
       || !VG_IS_PAGE_ALIGNED(start)
       || !VG_IS_PAGE_ALIGNED(offset))
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* Ask for an advisory.  If it's negative, fail immediately. */
   req.rkind = MFixed;
   req.start = start;
   req.len   = length;
   advised = VG_(am_get_advisory)( &req, True/*forClient*/, &ok );
   if (!ok || advised != start)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* We have been advised that the mapping is allowable at the
      specified address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   // DDD: #warning GrP fixme MAP_FIXED can clobber memory!
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             start, length, prot, flags,
             fd, offset 
          );
   if (sr_isError(sres))
      return sres;

   if (sr_Res(sres) != start) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)ML_(am_do_munmap_NO_NOTIFY)( sr_Res(sres), length );
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Ok, the mapping succeeded.  Now notify the interval map. */
   init_nsegment( &seg );
   seg.kind   = SkFileC;
   seg.start  = start;
   seg.end    = seg.start + VG_PGROUNDUP(length) - 1;
   seg.offset = offset;
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);
   if (ML_(am_get_fd_d_i_m)(fd, &dev, &ino, &mode)) {
      seg.dev = dev;
      seg.ino = ino;
      seg.mode = mode;
   }
   if (name) {
      seg.fnIdx = ML_(am_allocate_segname)( name );
   } else if (ML_(am_resolve_filename)(fd, buf, VKI_PATH_MAX)) {
      seg.fnIdx = ML_(am_allocate_segname)( buf );
   }
#if defined(VGO_freebsd)
   seg.isFF = (flags & VKI_MAP_FIXED);
#endif
   add_segment( &seg );

   AM_SANITY_CHECK;
   return sres;
}


/* Map anonymously at a fixed address for the client, and update
   the segment array accordingly. */

SysRes VG_(am_mmap_anon_fixed_client) ( Addr start, SizeT length, UInt prot )
{
   SysRes     sres;
   NSegment   seg;
   Addr       advised;
   Bool       ok;
   MapRequest req;
 
   /* Not allowable. */
   if (length == 0 || !VG_IS_PAGE_ALIGNED(start))
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* Ask for an advisory.  If it's negative, fail immediately. */
   req.rkind = MFixed;
   req.start = start;
   req.len   = length;
   advised = VG_(am_get_advisory)( &req, True/*forClient*/, &ok );
   if (!ok || advised != start)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* We have been advised that the mapping is allowable at the
      specified address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   // DDD: #warning GrP fixme MAP_FIXED can clobber memory!
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             start, length, prot, 
             VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
             0, 0 
          );
   if (sr_isError(sres))
      return sres;

   if (sr_Res(sres) != start) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)ML_(am_do_munmap_NO_NOTIFY)( sr_Res(sres), length );
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Ok, the mapping succeeded.  Now notify the interval map. */
   init_nsegment( &seg );
   seg.kind  = SkAnonC;
   seg.start = start;
   seg.end   = seg.start + VG_PGROUNDUP(length) - 1;
   seg.hasR  = toBool(prot & VKI_PROT_READ);
   seg.hasW  = toBool(prot & VKI_PROT_WRITE);
   seg.hasX  = toBool(prot & VKI_PROT_EXEC);
   add_segment( &seg );

   AM_SANITY_CHECK;
   return sres;
}


/* Map anonymously at an unconstrained address for the client, and
   update the segment array accordingly.  */

static SysRes am_mmap_anon_float_client ( SizeT length, Int prot, Bool isCH )
{
   SysRes     sres;
   NSegment   seg;
   Addr       advised;
   Bool       ok;
   MapRequest req;
 
   /* Not allowable. */
   if (length == 0)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* Ask for an advisory.  If it's negative, fail immediately. */
   req.rkind = MAny;
   req.start = 0;
   req.len   = length;
   advised = VG_(am_get_advisory)( &req, True/*forClient*/, &ok );
   if (!ok)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* We have been advised that the mapping is allowable at the
      advised address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   // DDD: #warning GrP fixme MAP_FIXED can clobber memory!
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             advised, length, prot, 
             VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
             0, 0 
          );
   if (sr_isError(sres))
      return sres;

   if (sr_Res(sres) != advised) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)ML_(am_do_munmap_NO_NOTIFY)( sr_Res(sres), length );
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Ok, the mapping succeeded.  Now notify the interval map. */
   init_nsegment( &seg );
   seg.kind  = SkAnonC;
   seg.start = advised;
   seg.end   = seg.start + VG_PGROUNDUP(length) - 1;
   seg.hasR  = toBool(prot & VKI_PROT_READ);
   seg.hasW  = toBool(prot & VKI_PROT_WRITE);
   seg.hasX  = toBool(prot & VKI_PROT_EXEC);
   seg.isCH  = isCH;
   add_segment( &seg );

   AM_SANITY_CHECK;
   return sres;
}

SysRes VG_(am_mmap_anon_float_client) ( SizeT length, Int prot )
{
   return am_mmap_anon_float_client (length, prot, False /* isCH */);
}

/* Map anonymously at an unconstrained address for V, and update the
   segment array accordingly.  This is fundamentally how V allocates
   itself more address space when needed. */

SysRes VG_(am_mmap_anon_float_valgrind)( SizeT length )
{
   SysRes     sres;
   NSegment   seg;
   Addr       advised;
   Bool       ok;
   MapRequest req;
 
   /* Not allowable. */
   if (length == 0)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* Ask for an advisory.  If it's negative, fail immediately. */
   req.rkind = MAny;
   req.start = 0;
   req.len   = length;
   advised = VG_(am_get_advisory)( &req, False/*forClient*/, &ok );
   if (!ok)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

// On Darwin, for anonymous maps you can pass in a tag which is used by
// programs like vmmap for statistical purposes.
#ifndef VM_TAG_VALGRIND
#  define VM_TAG_VALGRIND 0
#endif

   /* We have been advised that the mapping is allowable at the
      specified address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   /* GrP fixme darwin: use advisory as a hint only, otherwise syscall in 
      another thread can pre-empt our spot.  [At one point on the DARWIN
      branch the VKI_MAP_FIXED was commented out;  unclear if this is
      necessary or not given the second Darwin-only call that immediately
      follows if this one fails.  --njn]
      Also, an inner valgrind cannot observe the mmap syscalls done by
      the outer valgrind. The outer Valgrind might make the mmap
      fail here, as the inner valgrind believes that a segment is free,
      while it is in fact used by the outer valgrind.
      So, for an inner valgrind, similarly to DARWIN, if the fixed mmap
      fails, retry the mmap without map fixed.
      This is a kludge which on linux is only activated for the inner.
      The state of the inner aspacemgr is not made correct by this kludge
      and so a.o. VG_(am_do_sync_check) could fail.
      A proper solution implies a better collaboration between the
      inner and the outer (e.g. inner VG_(am_get_advisory) should do
      a client request to call the outer VG_(am_get_advisory). */
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             advised, length, 
             VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC, 
             VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
             VM_TAG_VALGRIND, 0
          );
#if defined(VGO_darwin) || defined(ENABLE_INNER)
   /* Kludge on Darwin and inner linux if the fixed mmap failed. */
   if (sr_isError(sres)) {
       /* try again, ignoring the advisory */
       sres = VG_(am_do_mmap_NO_NOTIFY)( 
             0, length, 
             VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC, 
             /*VKI_MAP_FIXED|*/VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
             VM_TAG_VALGRIND, 0
          );
   }
#endif
   if (sr_isError(sres))
      return sres;

#if defined(VGO_linux) && !defined(ENABLE_INNER)
   /* Doing the check only in linux not inner, as the below
      check can fail when the kludge above has been used. */
   if (sr_Res(sres) != advised) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)ML_(am_do_munmap_NO_NOTIFY)( sr_Res(sres), length );
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }
#endif

   /* Ok, the mapping succeeded.  Now notify the interval map. */
   init_nsegment( &seg );
   seg.kind  = SkAnonV;
   seg.start = sr_Res(sres);
   seg.end   = seg.start + VG_PGROUNDUP(length) - 1;
   seg.hasR  = True;
   seg.hasW  = True;
   seg.hasX  = True;
   add_segment( &seg );

   AM_SANITY_CHECK;
   return sres;
}

/* Really just a wrapper around VG_(am_mmap_anon_float_valgrind). */

SysRes VG_(am_shadow_alloc)(SizeT size)
{
   return VG_(am_mmap_anon_float_valgrind)( size );
}

/* Map a file at an unconstrained address for V, and update the
   segment array accordingly. Use the provided flags */

static SysRes VG_(am_mmap_file_float_valgrind_flags) ( SizeT length, UInt prot,
                                                       UInt flags,
                                                       Int fd, Off64T offset )
{
   SysRes     sres;
   NSegment   seg;
   Addr       advised;
   Bool       ok;
   MapRequest req;
   ULong      dev, ino;
   UInt       mode;
   HChar      buf[VKI_PATH_MAX];
 
   /* Not allowable. */
   if (length == 0 || !VG_IS_PAGE_ALIGNED(offset))
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* Ask for an advisory.  If it's negative, fail immediately. */
   req.rkind = MAny;
   req.start = 0;
   #if defined(VGA_arm) || defined(VGA_arm64) \
      || defined(VGA_mips32) || defined(VGA_mips64) || defined(VGA_nanomips)
   aspacem_assert(VKI_SHMLBA >= VKI_PAGE_SIZE);
   #else
   aspacem_assert(VKI_SHMLBA == VKI_PAGE_SIZE);
   #endif
   if ((VKI_SHMLBA > VKI_PAGE_SIZE) && (VKI_MAP_SHARED & flags)) {
      /* arm-linux only. See ML_(generic_PRE_sys_shmat) and bug 290974 */
      req.len = length + VKI_SHMLBA - VKI_PAGE_SIZE;
   } else {
      req.len = length;
   }
   advised = VG_(am_get_advisory)( &req, False/*forClient*/, &ok );
   if (!ok)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   if ((VKI_SHMLBA > VKI_PAGE_SIZE) && (VKI_MAP_SHARED & flags))
      advised = VG_ROUNDUP(advised, VKI_SHMLBA);

   /* We have been advised that the mapping is allowable at the
      specified address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             advised, length, prot, 
             flags,
             fd, offset 
          );
   if (sr_isError(sres))
      return sres;

   if (sr_Res(sres) != advised) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)ML_(am_do_munmap_NO_NOTIFY)( sr_Res(sres), length );
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Ok, the mapping succeeded.  Now notify the interval map. */
   init_nsegment( &seg );
   seg.kind   = SkFileV;
   seg.start  = sr_Res(sres);
   seg.end    = seg.start + VG_PGROUNDUP(length) - 1;
   seg.offset = offset;
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);
   if (ML_(am_get_fd_d_i_m)(fd, &dev, &ino, &mode)) {
      seg.dev  = dev;
      seg.ino  = ino;
      seg.mode = mode;
   }
   if (ML_(am_resolve_filename)(fd, buf, VKI_PATH_MAX)) {
      seg.fnIdx = ML_(am_allocate_segname)( buf );
   }
#if defined(VGO_freebsd)
   seg.isFF = (flags & VKI_MAP_FIXED);
#endif
   add_segment( &seg );

   AM_SANITY_CHECK;
   return sres;
}
/* Map privately a file at an unconstrained address for V, and update the
   segment array accordingly.  This is used by V for transiently
   mapping in object files to read their debug info.  */

SysRes VG_(am_mmap_file_float_valgrind) ( SizeT length, UInt prot, 
                                          Int fd, Off64T offset )
{
   return VG_(am_mmap_file_float_valgrind_flags) (length, prot,
                                                  VKI_MAP_FIXED|VKI_MAP_PRIVATE,
                                                  fd, offset );
}

SysRes VG_(am_shared_mmap_file_float_valgrind)
   ( SizeT length, UInt prot, Int fd, Off64T offset )
{
   return VG_(am_mmap_file_float_valgrind_flags) (length, prot,
                                                  VKI_MAP_FIXED|VKI_MAP_SHARED,
                                                  fd, offset );
}

/* Similar to VG_(am_mmap_anon_float_client) but also
   marks the segment as containing the client heap. This is for the benefit
   of the leak checker which needs to be able to identify such segments
   so as not to use them as sources of roots during leak checks. */
SysRes VG_(am_mmap_client_heap) ( SizeT length, Int prot )
{
   return am_mmap_anon_float_client (length, prot, True /* isCH */);
}

/* --- --- munmap helper --- --- */

static 
SysRes am_munmap_both_wrk ( /*OUT*/Bool* need_discard,
                            Addr start, SizeT len, Bool forClient )
{
   Bool   d;
   SysRes sres;

   /* Be safe with this regardless of return path. */
   *need_discard = False;

   if (!VG_IS_PAGE_ALIGNED(start))
      goto eINVAL;

   if (len == 0) {
      *need_discard = False;
      return VG_(mk_SysRes_Success)( 0 );
   }

   if (start + len < len)
      goto eINVAL;

   len = VG_PGROUNDUP(len);
   aspacem_assert(VG_IS_PAGE_ALIGNED(start));
   aspacem_assert(VG_IS_PAGE_ALIGNED(len));

   if (forClient) {
      if (!VG_(am_is_valid_for_client_or_free_or_resvn)
            ( start, len, VKI_PROT_NONE ))
         goto eINVAL;
   } else {
      if (!VG_(am_is_valid_for_valgrind)
            ( start, len, VKI_PROT_NONE ))
         goto eINVAL;
   }

   d = any_Ts_in_range( start, len );

   sres = ML_(am_do_munmap_NO_NOTIFY)( start, len );
   if (sr_isError(sres))
      return sres;

   VG_(am_notify_munmap)( start, len );
   AM_SANITY_CHECK;
   *need_discard = d;
   return sres;

  eINVAL:
   return VG_(mk_SysRes_Error)( VKI_EINVAL );
}

/* Unmap the given address range and update the segment array
   accordingly.  This fails if the range isn't valid for the client.
   If *need_discard is True after a successful return, the caller
   should immediately discard translations from the specified address
   range. */

SysRes VG_(am_munmap_client)( /*OUT*/Bool* need_discard,
                              Addr start, SizeT len )
{
   return am_munmap_both_wrk( need_discard, start, len, True/*client*/ );
}

/* Unmap the given address range and update the segment array
   accordingly.  This fails if the range isn't valid for valgrind. */

SysRes VG_(am_munmap_valgrind)( Addr start, SizeT len )
{
   Bool need_discard;
   SysRes r = am_munmap_both_wrk( &need_discard, 
                                  start, len, False/*valgrind*/ );
   /* If this assertion fails, it means we allowed translations to be
      made from a V-owned section.  Which shouldn't happen. */
   if (!sr_isError(r))
      aspacem_assert(!need_discard);
   return r;
}

/* Let (start,len) denote an area within a single Valgrind-owned
  segment (anon or file).  Change the ownership of [start, start+len)
  to the client instead.  Fails if (start,len) does not denote a
  suitable segment. */

Bool VG_(am_change_ownership_v_to_c)( Addr start, SizeT len )
{
   Int i, iLo, iHi;

   if (len == 0)
      return True;
   if (start + len < start)
      return False;
   if (!VG_IS_PAGE_ALIGNED(start) || !VG_IS_PAGE_ALIGNED(len))
      return False;

   i = find_nsegment_idx(start);
   if (nsegments[i].kind != SkFileV && nsegments[i].kind != SkAnonV)
      return False;
   if (start+len-1 > nsegments[i].end)
      return False;

   aspacem_assert(start >= nsegments[i].start);
   aspacem_assert(start+len-1 <= nsegments[i].end);

   /* This scheme is like how mprotect works: split the to-be-changed
      range into its own segment(s), then mess with them (it).  There
      should be only one. */
   split_nsegments_lo_and_hi( start, start+len-1, &iLo, &iHi );
   aspacem_assert(iLo == iHi);
   switch (nsegments[iLo].kind) {
      case SkFileV: nsegments[iLo].kind = SkFileC; break;
      case SkAnonV: nsegments[iLo].kind = SkAnonC; break;
      default: aspacem_assert(0); /* can't happen - guarded above */
   }

   preen_nsegments();
   return True;
}

/* Set the 'hasT' bit on the segment containing ADDR indicating that
   translations have or may have been taken from this segment. ADDR is
   expected to belong to a client segment. */
void VG_(am_set_segment_hasT)( Addr addr )
{
   Int i = find_nsegment_idx(addr);
   SegKind kind = nsegments[i].kind;
   aspacem_assert(kind == SkAnonC || kind == SkFileC || kind == SkShmC);
   nsegments[i].hasT = True;
}


/* --- --- --- reservations --- --- --- */

/* Create a reservation from START .. START+LENGTH-1, with the given
   ShrinkMode.  When checking whether the reservation can be created,
   also ensure that at least abs(EXTRA) extra free bytes will remain
   above (> 0) or below (< 0) the reservation.

   The reservation will only be created if it, plus the extra-zone,
   falls entirely within a single free segment.  The returned Bool
   indicates whether the creation succeeded. */

Bool VG_(am_create_reservation) ( Addr start, SizeT length, 
                                  ShrinkMode smode, SSizeT extra )
{
   Int      startI, endI;
   NSegment seg;

   /* start and end, not taking into account the extra space. */
   Addr start1 = start;
   Addr end1   = start + length - 1;

   /* start and end, taking into account the extra space. */
   Addr start2 = start1;
   Addr end2   = end1;

   if (extra < 0) start2 += extra; // this moves it down :-)
   if (extra > 0) end2 += extra;

   aspacem_assert(VG_IS_PAGE_ALIGNED(start));
   aspacem_assert(VG_IS_PAGE_ALIGNED(start+length));
   aspacem_assert(VG_IS_PAGE_ALIGNED(start2));
   aspacem_assert(VG_IS_PAGE_ALIGNED(end2+1));

   startI = find_nsegment_idx( start2 );
   endI = find_nsegment_idx( end2 );

   /* If the start and end points don't fall within the same (free)
      segment, we're hosed.  This does rely on the assumption that all
      mergeable adjacent segments can be merged, but add_segment()
      should ensure that. */
   if (startI != endI)
      return False;

   if (nsegments[startI].kind != SkFree)
      return False;

   /* Looks good - make the reservation. */
   aspacem_assert(nsegments[startI].start <= start2);
   aspacem_assert(end2 <= nsegments[startI].end);

   init_nsegment( &seg );
   seg.kind  = SkResvn;
   seg.start = start1;  /* NB: extra space is not included in the
                           reservation. */
   seg.end   = end1;
   seg.smode = smode;
   add_segment( &seg );

   AM_SANITY_CHECK;
   return True;
}


/* ADDR is the start address of an anonymous client mapping.  This fn extends
   the mapping by DELTA bytes, taking the space from a reservation section
   which must be adjacent.  If DELTA is positive, the segment is
   extended forwards in the address space, and the reservation must be
   the next one along.  If DELTA is negative, the segment is extended
   backwards in the address space and the reservation must be the
   previous one.  DELTA must be page aligned.  abs(DELTA) must not
   exceed the size of the reservation segment minus one page, that is,
   the reservation segment after the operation must be at least one
   page long. The function returns a pointer to the resized segment. */

const NSegment *VG_(am_extend_into_adjacent_reservation_client)( Addr addr, 
                                                                 SSizeT delta,
                                                                 Bool *overflow)
{
   Int    segA, segR;
   UInt   prot;
   SysRes sres;

   *overflow = False;

   segA = find_nsegment_idx(addr);
   aspacem_assert(nsegments[segA].kind == SkAnonC);

   if (delta == 0)
      return nsegments + segA;

   prot =   (nsegments[segA].hasR ? VKI_PROT_READ : 0)
          | (nsegments[segA].hasW ? VKI_PROT_WRITE : 0)
          | (nsegments[segA].hasX ? VKI_PROT_EXEC : 0);

   aspacem_assert(VG_IS_PAGE_ALIGNED(delta<0 ? -delta : delta));

   if (delta > 0) {

      /* Extending the segment forwards. */
      segR = segA+1;
      if (segR >= nsegments_used
          || nsegments[segR].kind != SkResvn
          || nsegments[segR].smode != SmLower)
         return NULL;

      if (delta + VKI_PAGE_SIZE 
                > (nsegments[segR].end - nsegments[segR].start + 1)) {
         *overflow = True;
         return NULL;
      }
        
      /* Extend the kernel's mapping. */
      // DDD: #warning GrP fixme MAP_FIXED can clobber memory!
      sres = VG_(am_do_mmap_NO_NOTIFY)( 
                nsegments[segR].start, delta,
                prot,
                VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
                0, 0 
             );
      if (sr_isError(sres))
         return NULL; /* kernel bug if this happens? */
      if (sr_Res(sres) != nsegments[segR].start) {
         /* kernel bug if this happens? */
        (void)ML_(am_do_munmap_NO_NOTIFY)( sr_Res(sres), delta );
        return NULL;
      }

      /* Ok, success with the kernel.  Update our structures. */
      nsegments[segR].start += delta;
      nsegments[segA].end += delta;
      aspacem_assert(nsegments[segR].start <= nsegments[segR].end);

   } else {

      /* Extending the segment backwards. */
      delta = -delta;
      aspacem_assert(delta > 0);

      segR = segA-1;
      if (segR < 0
          || nsegments[segR].kind != SkResvn
          || nsegments[segR].smode != SmUpper)
         return NULL;

      if (delta + VKI_PAGE_SIZE 
                > (nsegments[segR].end - nsegments[segR].start + 1)) {
         *overflow = True;
         return NULL;
      }
        
      /* Extend the kernel's mapping. */
      // DDD: #warning GrP fixme MAP_FIXED can clobber memory!
      sres = VG_(am_do_mmap_NO_NOTIFY)( 
                nsegments[segA].start-delta, delta,
                prot,
                VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
                0, 0 
             );
      if (sr_isError(sres))
         return NULL; /* kernel bug if this happens? */
      if (sr_Res(sres) != nsegments[segA].start-delta) {
         /* kernel bug if this happens? */
        (void)ML_(am_do_munmap_NO_NOTIFY)( sr_Res(sres), delta );
        return NULL;
      }

      /* Ok, success with the kernel.  Update our structures. */
      nsegments[segR].end -= delta;
      nsegments[segA].start -= delta;
      aspacem_assert(nsegments[segR].start <= nsegments[segR].end);
   }

   AM_SANITY_CHECK;
   return nsegments + segA;
}


/* --- --- --- resizing/move a mapping --- --- --- */

#if HAVE_MREMAP

/* This function grows a client mapping in place into an adjacent free segment.
   ADDR is the client mapping's start address and DELTA, which must be page
   aligned, is the growth amount. The function returns a pointer to the
   resized segment. The function is used in support of mremap. */
const NSegment *VG_(am_extend_map_client)( Addr addr, SizeT delta )
{
   Addr     xStart;
   SysRes   sres;

   if (0)
      VG_(am_show_nsegments)(0, "VG_(am_extend_map_client) BEFORE");

   /* Get the client segment */
   Int ix = find_nsegment_idx(addr);
   aspacem_assert(ix >= 0 && ix < nsegments_used);

   NSegment *seg = nsegments + ix;

   aspacem_assert(seg->kind == SkFileC || seg->kind == SkAnonC ||
                  seg->kind == SkShmC);
   aspacem_assert(delta > 0 && VG_IS_PAGE_ALIGNED(delta)) ;

   xStart = seg->end+1;
   aspacem_assert(xStart + delta >= delta);   // no wrap-around

   /* The segment following the client segment must be a free segment and
      it must be large enough to cover the additional memory. */
   NSegment *segf = seg + 1;
   aspacem_assert(segf->kind == SkFree);
   aspacem_assert(segf->start == xStart);
   aspacem_assert(xStart + delta - 1 <= segf->end);

   SizeT seg_old_len = seg->end + 1 - seg->start;

   AM_SANITY_CHECK;
   sres = ML_(am_do_extend_mapping_NO_NOTIFY)( seg->start, 
                                               seg_old_len,
                                               seg_old_len + delta );
   if (sr_isError(sres)) {
      AM_SANITY_CHECK;
      return NULL;
   } else {
      /* the area must not have moved */
      aspacem_assert(sr_Res(sres) == seg->start);
   }

   NSegment seg_copy = *seg;
   seg_copy.end += delta;
   add_segment( &seg_copy );

   if (0)
      VG_(am_show_nsegments)(0, "VG_(am_extend_map_client) AFTER");

   AM_SANITY_CHECK;
   return nsegments + find_nsegment_idx(addr);
}


/* Remap the old address range to the new address range.  Fails if any
   parameter is not page aligned, if the either size is zero, if any
   wraparound is implied, if the old address range does not fall
   entirely within a single segment, if the new address range overlaps
   with the old one, or if the old address range is not a valid client
   mapping.  If *need_discard is True after a successful return, the
   caller should immediately discard translations from both specified
   address ranges.  */

Bool VG_(am_relocate_nooverlap_client)( /*OUT*/Bool* need_discard,
                                        Addr old_addr, SizeT old_len,
                                        Addr new_addr, SizeT new_len )
{
   Int      iLo, iHi;
   SysRes   sres;
   NSegment seg;

   if (old_len == 0 || new_len == 0)
      return False;

   if (!VG_IS_PAGE_ALIGNED(old_addr) || !VG_IS_PAGE_ALIGNED(old_len)
       || !VG_IS_PAGE_ALIGNED(new_addr) || !VG_IS_PAGE_ALIGNED(new_len))
      return False;

   if (old_addr + old_len < old_addr
       || new_addr + new_len < new_addr)
      return False;

   if (old_addr + old_len - 1 < new_addr
       || new_addr + new_len - 1 < old_addr) {
      /* no overlap */
   } else
      return False;

   iLo = find_nsegment_idx( old_addr );
   iHi = find_nsegment_idx( old_addr + old_len - 1 );
   if (iLo != iHi)
      return False;

   if (nsegments[iLo].kind != SkFileC && nsegments[iLo].kind != SkAnonC &&
       nsegments[iLo].kind != SkShmC)
      return False;

   sres = ML_(am_do_relocate_nooverlap_mapping_NO_NOTIFY)
             ( old_addr, old_len, new_addr, new_len );
   if (sr_isError(sres)) {
      AM_SANITY_CHECK;
      return False;
   } else {
      aspacem_assert(sr_Res(sres) == new_addr);
   }

   *need_discard = any_Ts_in_range( old_addr, old_len )
                   || any_Ts_in_range( new_addr, new_len );

   seg = nsegments[iLo];

   /* Mark the new area based on the old seg. */
   if (seg.kind == SkFileC) {
      seg.offset += ((ULong)old_addr) - ((ULong)seg.start);
   }
   seg.start = new_addr;
   seg.end   = new_addr + new_len - 1;
   add_segment( &seg );

   /* Create a free hole in the old location. */
   init_nsegment( &seg );
   seg.start = old_addr;
   seg.end   = old_addr + old_len - 1;
   /* See comments in VG_(am_notify_munmap) about this SkResvn vs
      SkFree thing. */
   if (old_addr > aspacem_maxAddr 
       && /* check previous comparison is meaningful */
          aspacem_maxAddr < Addr_MAX)
      seg.kind = SkResvn;
   else
      seg.kind = SkFree;

   add_segment( &seg );

   AM_SANITY_CHECK;
   return True;
}

#endif // HAVE_MREMAP


#if defined(VGO_linux)

/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- A simple parser for /proc/self/maps on Linux 2.4.X/2.6.X. ---*/
/*--- Almost completely independent of the stuff above.  The    ---*/
/*--- only function it 'exports' to the code above this comment ---*/
/*--- is parse_procselfmaps.                                    ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/*------BEGIN-procmaps-parser-for-Linux--------------------------*/

/* Size of a smallish table used to read /proc/self/map entries. */
#define M_PROCMAP_BUF 100000

/* static ... to keep it out of the stack frame. */
static HChar procmap_buf[M_PROCMAP_BUF];

/* Records length of /proc/self/maps read into procmap_buf. */
static Int  buf_n_tot;

/* Helper fns. */

static Int hexdigit ( HChar c )
{
   if (c >= '0' && c <= '9') return (Int)(c - '0');
   if (c >= 'a' && c <= 'f') return 10 + (Int)(c - 'a');
   if (c >= 'A' && c <= 'F') return 10 + (Int)(c - 'A');
   return -1;
}

static Int decdigit ( HChar c )
{
   if (c >= '0' && c <= '9') return (Int)(c - '0');
   return -1;
}

static Int readchar ( const HChar* buf, HChar* ch )
{
   if (*buf == 0) return 0;
   *ch = *buf;
   return 1;
}

static Int readhex ( const HChar* buf, UWord* val )
{
   /* Read a word-sized hex number. */
   Int n = 0;
   *val = 0;
   while (hexdigit(*buf) >= 0) {
      *val = (*val << 4) + hexdigit(*buf);
      n++; buf++;
   }
   return n;
}

static Int readhex64 ( const HChar* buf, ULong* val )
{
   /* Read a potentially 64-bit hex number. */
   Int n = 0;
   *val = 0;
   while (hexdigit(*buf) >= 0) {
      *val = (*val << 4) + hexdigit(*buf);
      n++; buf++;
   }
   return n;
}

static Int readdec64 ( const HChar* buf, ULong* val )
{
   Int n = 0;
   *val = 0;
   while (decdigit(*buf) >= 0) {
      *val = (*val * 10) + decdigit(*buf);
      n++; buf++;
   }
   return n;
}


/* Get the contents of /proc/self/maps into a static buffer.  If
   there's a syntax error, it won't fit, or other failure, just
   abort. */

static void read_procselfmaps_into_buf ( void )
{
   Int    n_chunk;
   SysRes fd;
   
   /* Read the initial memory mapping from the /proc filesystem. */
   fd = ML_(am_open)( "/proc/self/maps", VKI_O_RDONLY, 0 );
   if (sr_isError(fd))
      ML_(am_barf)("can't open /proc/self/maps");

   buf_n_tot = 0;
   do {
      n_chunk = ML_(am_read)( sr_Res(fd), &procmap_buf[buf_n_tot],
                              M_PROCMAP_BUF - buf_n_tot );
      if (n_chunk >= 0)
         buf_n_tot += n_chunk;
   } while ( n_chunk > 0 && buf_n_tot < M_PROCMAP_BUF );

   ML_(am_close)(sr_Res(fd));

   if (buf_n_tot >= M_PROCMAP_BUF-5)
      ML_(am_barf_toolow)("M_PROCMAP_BUF");
   if (buf_n_tot == 0)
      ML_(am_barf)("I/O error on /proc/self/maps");

   procmap_buf[buf_n_tot] = 0;
}

/* Parse /proc/self/maps.  For each map entry, call
   record_mapping, passing it, in this order:

      start address in memory
      length
      page protections (using the VKI_PROT_* flags)
      mapped file device and inode
      offset in file, or zero if no file
      filename, zero terminated, or NULL if no file
      ignore_offset, when the exact offset cannot be
                     obtained

   So the sig of the called fn might be

      void (*record_mapping)( Addr start, SizeT size, UInt prot,
			      UInt dev, UInt info,
                              ULong foffset, UChar* filename,
                              Bool ignore_offset )

   Note that the supplied filename is transiently stored; record_mapping 
   should make a copy if it wants to keep it.

   Nb: it is important that this function does not alter the contents of
       procmap_buf!
*/
static void parse_procselfmaps (
      void (*record_mapping)( Addr addr, SizeT len, UInt prot,
                              ULong dev, ULong ino, Off64T offset, 
                              const HChar* filename, Bool ignore_offset ),
      void (*record_gap)( Addr addr, SizeT len )
   )
{
   Int    i, j, i_eol;
   Addr   start, endPlusOne, gapStart;
   HChar* filename;
   HChar  rr, ww, xx, pp, ch, tmp;
   UInt	  prot;
   UWord  maj, min;
   ULong  foffset, dev, ino;

   foffset = ino = 0; /* keep gcc-4.1.0 happy */

   read_procselfmaps_into_buf();

   aspacem_assert('\0' != procmap_buf[0] && 0 != buf_n_tot);

   if (0)
      VG_(debugLog)(0, "procselfmaps", "raw:\n%s\n", procmap_buf);

   /* Ok, it's safely aboard.  Parse the entries. */
   i = 0;
   gapStart = Addr_MIN;
   while (True) {
      if (i >= buf_n_tot) break;

      /* Read (without fscanf :) the pattern %16x-%16x %c%c%c%c %16x %2x:%2x %d */
      j = readhex(&procmap_buf[i], &start);
      if (j > 0) i += j; else goto syntaxerror;
      j = readchar(&procmap_buf[i], &ch);
      if (j == 1 && ch == '-') i += j; else goto syntaxerror;
      j = readhex(&procmap_buf[i], &endPlusOne);
      if (j > 0) i += j; else goto syntaxerror;

      j = readchar(&procmap_buf[i], &ch);
      if (j == 1 && ch == ' ') i += j; else goto syntaxerror;

      j = readchar(&procmap_buf[i], &rr);
      if (j == 1 && (rr == 'r' || rr == '-')) i += j; else goto syntaxerror;
      j = readchar(&procmap_buf[i], &ww);
      if (j == 1 && (ww == 'w' || ww == '-')) i += j; else goto syntaxerror;
      j = readchar(&procmap_buf[i], &xx);
      if (j == 1 && (xx == 'x' || xx == '-')) i += j; else goto syntaxerror;
      /* This field is the shared/private flag */
      j = readchar(&procmap_buf[i], &pp);
      if (j == 1 && (pp == 'p' || pp == '-' || pp == 's')) 
                                              i += j; else goto syntaxerror;

      j = readchar(&procmap_buf[i], &ch);
      if (j == 1 && ch == ' ') i += j; else goto syntaxerror;

      j = readhex64(&procmap_buf[i], &foffset);
      if (j > 0) i += j; else goto syntaxerror;

      j = readchar(&procmap_buf[i], &ch);
      if (j == 1 && ch == ' ') i += j; else goto syntaxerror;

      j = readhex(&procmap_buf[i], &maj);
      if (j > 0) i += j; else goto syntaxerror;
      j = readchar(&procmap_buf[i], &ch);
      if (j == 1 && ch == ':') i += j; else goto syntaxerror;
      j = readhex(&procmap_buf[i], &min);
      if (j > 0) i += j; else goto syntaxerror;

      j = readchar(&procmap_buf[i], &ch);
      if (j == 1 && ch == ' ') i += j; else goto syntaxerror;

      j = readdec64(&procmap_buf[i], &ino);
      if (j > 0) i += j; else goto syntaxerror;
 
      goto read_line_ok;

    syntaxerror:
      VG_(debugLog)(0, "Valgrind:", 
                       "FATAL: syntax error reading /proc/self/maps\n");
      { Int k, m;
        HChar buf50[51];
        m = 0;
        buf50[m] = 0;
        k = i - 50;
        if (k < 0) k = 0;
        for (; k <= i; k++) {
           buf50[m] = procmap_buf[k];
           buf50[m+1] = 0;
           if (m < 50-1) m++;
        }
        VG_(debugLog)(0, "procselfmaps", "Last 50 chars: '%s'\n", buf50);
      }
      ML_(am_exit)(1);

    read_line_ok:

      aspacem_assert(i < buf_n_tot);

      /* Try and find the name of the file mapped to this segment, if
         it exists.  Note that file names can contain spaces. */

      // Move i to the next non-space char, which should be either a '/',
      // a '[', or a newline.
      while (procmap_buf[i] == ' ') i++;
      
      // Move i_eol to the end of the line.
      i_eol = i;
      while (procmap_buf[i_eol] != '\n') i_eol++;

      // If there's a filename...
      if (procmap_buf[i] == '/') {
         /* Minor hack: put a '\0' at the filename end for the call to
            'record_mapping', then restore the old char with 'tmp'. */
         filename = &procmap_buf[i];
         tmp = filename[i_eol - i];
         filename[i_eol - i] = '\0';
      } else {
	 tmp = 0;
         filename = NULL;
         foffset = 0;
      }

      prot = 0;
      if (rr == 'r') prot |= VKI_PROT_READ;
      if (ww == 'w') prot |= VKI_PROT_WRITE;
      if (xx == 'x') prot |= VKI_PROT_EXEC;

      /* Linux has two ways to encode a device number when it
         is exposed to user space (via fstat etc). The old way
         is the traditional unix scheme that produces a 16 bit
         device number with the top 8 being the major number and
         the bottom 8 the minor number.
         
         The new scheme allows for a 12 bit major number and
         a 20 bit minor number by using a 32 bit device number
         and putting the top 12 bits of the minor number into
         the top 12 bits of the device number thus leaving an
         extra 4 bits for the major number.
         
         If the minor and major number are both single byte
         values then both schemes give the same result so we
         use the new scheme here in case either number is
         outside the 0-255 range and then use fstat64 when
         available (or fstat on 64 bit systems) so that we
         should always have a new style device number and
         everything should match. */
      dev = (min & 0xff) | (maj << 8) | ((min & ~0xff) << 12);

      if (record_gap && gapStart < start)
         (*record_gap) ( gapStart, start-gapStart );

      if (record_mapping && start < endPlusOne)
         (*record_mapping) ( start, endPlusOne-start,
                             prot, dev, ino,
                             foffset, filename, False );

      if ('\0' != tmp) {
         filename[i_eol - i] = tmp;
      }

      i = i_eol + 1;
      gapStart = endPlusOne;
   }

#  if defined(VGP_arm_linux)
   /* ARM puts code at the end of memory that contains processor
      specific stuff (cmpxchg, getting the thread local storage, etc.)
      This isn't specified in /proc/self/maps, so do it here.  This
      kludgery causes the view of memory, as presented to
      record_gap/record_mapping, to actually reflect reality.  IMO
      (JRS, 2010-Jan-03) the fact that /proc/.../maps does not list
      the commpage should be regarded as a bug in the kernel. */
   { const Addr commpage_start = ARM_LINUX_FAKE_COMMPAGE_START;
     const Addr commpage_end1  = ARM_LINUX_FAKE_COMMPAGE_END1;
     if (gapStart < commpage_start) {
        if (record_gap)
           (*record_gap)( gapStart, commpage_start - gapStart );
        if (record_mapping)
           (*record_mapping)( commpage_start, commpage_end1 - commpage_start,
                              VKI_PROT_READ|VKI_PROT_EXEC,
                              0/*dev*/, 0/*ino*/, 0/*foffset*/,
                              NULL, False);
        gapStart = commpage_end1;
     }
   }
#  endif

   if (record_gap && gapStart < Addr_MAX)
      (*record_gap) ( gapStart, Addr_MAX - gapStart + 1 );
}

/*------END-procmaps-parser-for-Linux----------------------------*/

/*------BEGIN-procmaps-parser-for-Darwin-------------------------*/

#elif defined(VGO_darwin)
#include <mach/mach.h>
#include <mach/mach_vm.h>

static unsigned int mach2vki(unsigned int vm_prot)
{
   return
      ((vm_prot & VM_PROT_READ)    ? VKI_PROT_READ    : 0) |
      ((vm_prot & VM_PROT_WRITE)   ? VKI_PROT_WRITE   : 0) |
      ((vm_prot & VM_PROT_EXECUTE) ? VKI_PROT_EXEC    : 0) ;
}

static UInt stats_machcalls = 0;

static void parse_procselfmaps (
      void (*record_mapping)( Addr addr, SizeT len, UInt prot,
                              ULong dev, ULong ino, Off64T offset, 
                              const HChar* filename, Bool ignore_offset ),
      void (*record_gap)( Addr addr, SizeT len )
   )
{
   vm_address_t iter;
   unsigned int depth;
   vm_address_t last;

   iter = 0;
   depth = 0;
   last = 0;
   while (1) {
      mach_vm_address_t addr = iter;
      mach_vm_size_t size;
      vm_region_submap_short_info_data_64_t info;
      kern_return_t kr;

      while (1) {
         mach_msg_type_number_t info_count
            = VM_REGION_SUBMAP_SHORT_INFO_COUNT_64;
         stats_machcalls++;
         kr = mach_vm_region_recurse(mach_task_self(), &addr, &size, &depth,
                                     (vm_region_info_t)&info, &info_count);
         if (kr) 
            return;
         if (info.is_submap) {
            depth++;
            continue;
         }
         break;
      }
      iter = addr + size;

      if (addr > last  &&  record_gap) {
         (*record_gap)(last, addr - last);
      }
      if (record_mapping) {
         (*record_mapping)(addr, size, mach2vki(info.protection),
                           0, 0, info.offset, NULL, False);
      }
      last = addr + size;
   }

   if ((Addr)-1 > last  &&  record_gap)
      (*record_gap)(last, (Addr)-1 - last);
}

// Urr.  So much for thread safety.
static Bool        css_overflowed;
static ChangedSeg* css_local;
static Int         css_size_local;
static Int         css_used_local;

static Addr Addr__max ( Addr a, Addr b ) { return a > b ? a : b; }
static Addr Addr__min ( Addr a, Addr b ) { return a < b ? a : b; }

static void add_mapping_callback(Addr addr, SizeT len, UInt prot, 
                                 ULong dev, ULong ino, Off64T offset, 
                                 const HChar *filename, Bool ignore_offset)
{
   // derived from sync_check_mapping_callback()

   /* JRS 2012-Mar-07: this all seems very dubious to me.  It would be
      safer to see if we can find, in V's segment collection, one
      single segment that completely covers the range [addr, +len)
      (and possibly more), and that has the exact same other
      properties (prot, dev, ino, offset, etc) as the data presented
      here.  If found, we just skip.  Otherwise add the data presented
      here into css_local[]. */

   Int iLo, iHi, i;

   if (len == 0) return;

   /* The kernel should not give us wraparounds. */
   aspacem_assert(addr <= addr + len - 1); 

   iLo = find_nsegment_idx( addr );
   iHi = find_nsegment_idx( addr + len - 1 );

   /* NSegments iLo .. iHi inclusive should agree with the presented
      data. */
   for (i = iLo; i <= iHi; i++) {

      UInt seg_prot;

      if (nsegments[i].kind == SkAnonV  ||  nsegments[i].kind == SkFileV) {
         /* Ignore V regions */
         continue;
      } 
      else if (nsegments[i].kind == SkFree || nsegments[i].kind == SkResvn) {
         /* Add mapping for SkResvn regions */
         ChangedSeg* cs = &css_local[css_used_local];
         if (css_used_local < css_size_local) {
            cs->is_added = True;
            cs->start    = addr;
            cs->end      = addr + len - 1;
            cs->prot     = prot;
            cs->offset   = offset;
            css_used_local++;
         } else {
            css_overflowed = True;
         }
         return;

      }
      else if (nsegments[i].kind == SkAnonC ||
               nsegments[i].kind == SkFileC ||
               nsegments[i].kind == SkShmC)
      {
         /* Check permissions on client regions */
         // GrP fixme
         seg_prot = 0;
         if (nsegments[i].hasR) seg_prot |= VKI_PROT_READ;
         if (nsegments[i].hasW) seg_prot |= VKI_PROT_WRITE;
#        if defined(VGA_x86)
         // GrP fixme sloppyXcheck 
         // darwin: kernel X ignored and spuriously changes? (vm_copy)
         seg_prot |= (prot & VKI_PROT_EXEC);
#        else
         if (nsegments[i].hasX) seg_prot |= VKI_PROT_EXEC;
#        endif
         if (seg_prot != prot) {
             if (VG_(clo_trace_syscalls)) 
                 VG_(debugLog)(0,"aspacem","region %p..%p permission "
                                 "mismatch (kernel %x, V %x)\n", 
                                 (void*)nsegments[i].start,
                                 (void*)(nsegments[i].end+1), prot, seg_prot);
            /* Add mapping for regions with protection changes */
            ChangedSeg* cs = &css_local[css_used_local];
            if (css_used_local < css_size_local) {
               cs->is_added = True;
               cs->start    = addr;
               cs->end      = addr + len - 1;
               cs->prot     = prot;
               cs->offset   = offset;
               css_used_local++;
            } else {
               css_overflowed = True;
            }
	    return;

         }

      } else {
         aspacem_assert(0);
      }
   }
}

static void remove_mapping_callback(Addr addr, SizeT len)
{
   // derived from sync_check_gap_callback()

   Int iLo, iHi, i;

   if (len == 0)
      return;

   /* The kernel should not give us wraparounds. */
   aspacem_assert(addr <= addr + len - 1); 

   iLo = find_nsegment_idx( addr );
   iHi = find_nsegment_idx( addr + len - 1 );

   /* NSegments iLo .. iHi inclusive should agree with the presented data. */
   for (i = iLo; i <= iHi; i++) {
      if (nsegments[i].kind != SkFree && nsegments[i].kind != SkResvn) {
         /* V has a mapping, kernel doesn't.  Add to css_local[],
            directives to chop off the part of the V mapping that
            falls within the gap that the kernel tells us is
            present. */
         ChangedSeg* cs = &css_local[css_used_local];
         if (css_used_local < css_size_local) {
            cs->is_added = False;
            cs->start    = Addr__max(nsegments[i].start, addr);
            cs->end      = Addr__min(nsegments[i].end,   addr + len - 1);
            aspacem_assert(VG_IS_PAGE_ALIGNED(cs->start));
            aspacem_assert(VG_IS_PAGE_ALIGNED(cs->end+1));
            /* I don't think the following should fail.  But if it
               does, just omit the css_used_local++ in the cases where
               it doesn't hold. */
            aspacem_assert(cs->start < cs->end);
            cs->prot     = 0;
            cs->offset   = 0;
            css_used_local++;
         } else {
            css_overflowed = True;
         }
      }
   }
}


// Returns False if 'css' wasn't big enough.
Bool VG_(get_changed_segments)(
      const HChar* when, const HChar* where, /*OUT*/ChangedSeg* css,
      Int css_size, /*OUT*/Int* css_used)
{
   static UInt stats_synccalls = 1;
   aspacem_assert(when && where);

   if (0)
      VG_(debugLog)(0,"aspacem",
         "[%u,%u] VG_(get_changed_segments)(%s, %s)\n",
         stats_synccalls++, stats_machcalls, when, where
      );

   css_overflowed = False;
   css_local = css;
   css_size_local = css_size;
   css_used_local = 0;

   // Get the list of segs that need to be added/removed.
   parse_procselfmaps(&add_mapping_callback, &remove_mapping_callback);

   *css_used = css_used_local;

   if (css_overflowed) {
      aspacem_assert(css_used_local == css_size_local);
   }

   return !css_overflowed;
}


/*------END-procmaps-parser-for-Darwin---------------------------*/

/*------BEGIN-procmaps-parser-for-Freebsd------------------------*/
#elif defined(VGO_freebsd)

/*
 * PJF 2023-09-23
 *
 * This function is somewhat badly named for FreeBSD, where the
 * /proc filesystem is optional so we can't count on users
 * having it. Instead we use the KERN_PROC_VMMAP syscall.
 * So far so good.
 *
 * This function is used in two contexts. The heaviest use is from
 * VG_(am_do_sync_check) as a sanity check that the contents of the
 * global nsegments array is consistent with what the OS reports
 * as being memory maps. No known problems with that.
 *
 * The other use is at startup in order to get the mapping for the
 * tool itself. In this case we have a fairly big problem. There is
 * a difference in the mapping used when the kernel loads an exe
 * and when the link loader ldrt (or Valgrind which does the same
 * job for the guest exe. In the case of ldrt, all ELF PT_LOAD
 * sections get mmap'd. The kernel, however, does _not_ mmap
 * the RW PT_LOAD.
 *
 * For instance, objdump -p for memcheck-amd64-freebsd contains
 *     LOAD off    0x0000000000000000 vaddr 0x0000000038000000 paddr 0x0000000038000000 align 2**12
 *         filesz 0x00000000000c5124 memsz 0x00000000000c5124 flags r--
 *    LOAD off    0x00000000000c5130 vaddr 0x00000000380c6130 paddr 0x00000000380c6130 align 2**12
 *         filesz 0x00000000001b10df memsz 0x00000000001b10df flags r-x
 *    LOAD off    0x0000000000276210 vaddr 0x0000000038278210 paddr 0x0000000038278210 align 2**12
 *         filesz 0x0000000000000a90 memsz 0x00000000025dd000 flags rw-
 *
 * Running procstat -v on a running instance gives
 * 44814         0x38000000         0x380c6000 r--  198 2558   2   0 CN--- vn /usr/home/paulf/scratch/valgrind/memcheck/memcheck-amd64-freebsd
 * 44814         0x380c6000         0x38278000 r-x  434 2558   2   0 CN--- vn /usr/home/paulf/scratch/valgrind/memcheck/memcheck-amd64-freebsd
 * 44814         0x38278000         0x3a856000 rw- 4590 4590   1   0 ----- df
 *
 * Instead of mmap'ing the RW PT_LOAD the kernel has mmap'd anonymous swap and copied from the exe file.
 * See https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=273956
 *
 * So what can we do? We can reuse most of the info from the previous 'r-x' mapping.
 * The filename, dev and ino are all the same. That leaves the offset. We can
 * make a rough estimate of the value as being previous offset + previous size.
 * Since the addresses in memory will be page aligned it's not possible to
 * obtain the original offset. It isn't good enough for ML_(read_elf_object)
 * in readelf.c
 *
 * As a hack of last resort we force ML_(read_elf_object) to accept this
 * mapping by adding an "ignore offset" flag. We can't be wrong that
 * there is something mapped roughly there - it's the global data of the
 * code that is executing on the CPU! Furthermore, this is not frequently
 * used. The main benefit is for Valgrind developers. Without this hack,
 * if Valgrind crashes or asserts it will print its own stack without
 * debuginfo, which is mostly useless. See the above FreeBSD bugzilla item
 * for an example.
 */

/* Size of a smallish table used to read /proc/self/map entries. */
#define M_PROCMAP_BUF 10485760	/* 10M */

/* static ... to keep it out of the stack frame. */
static char procmap_buf[M_PROCMAP_BUF];

static void parse_procselfmaps (
      void (*record_mapping)( Addr addr, SizeT len, UInt prot,
                              ULong dev, ULong ino, Off64T offset,
                              const HChar* filename, Bool ignore_offset ),
      void (*record_gap)( Addr addr, SizeT len )
   )
{
   Addr   start, endPlusOne, gapStart;
   char* filename;
   char   *p;
   UInt   prot;
   ULong  foffset, dev, ino;
   struct vki_kinfo_vmentry *kve;
   vki_size_t len;
   Int    oid[4];
   SysRes sres;
   Int map_count = 0;
   // could copy the whole kinfo_vmentry but it is 1160 bytes
   char   *rx_filename = NULL;
   ULong  rx_dev = 0U;
   ULong  rx_ino = 0U;
   ULong  rx_foffset = 0U;
   Bool   tool_read_maps = (record_mapping == read_maps_callback);

   foffset = ino = 0; /* keep gcc-4.1.0 happy */

   oid[0] = VKI_CTL_KERN;
   oid[1] = VKI_KERN_PROC;
   oid[2] = VKI_KERN_PROC_VMMAP;
   oid[3] = sr_Res(VG_(do_syscall0)(__NR_getpid));
   len = sizeof(procmap_buf);

   sres = VG_(do_syscall6)(__NR___sysctl, (UWord)oid, 4, (UWord)procmap_buf,
      (UWord)&len, 0, 0);
   if (sr_isError(sres)) {
      VG_(debugLog)(0, "procselfmaps", "sysctl %lu\n", sr_Err(sres));
      ML_(am_exit)(1);
   }
   gapStart = Addr_MIN;
   p = procmap_buf;
   while (p < (char *)procmap_buf + len) {
      kve = (struct vki_kinfo_vmentry *)p;
      start      = (UWord)kve->kve_start;
      endPlusOne = (UWord)kve->kve_end;
      foffset    = kve->kve_offset;
      filename   = kve->kve_path;
      dev        = kve->kve_vn_fsid_freebsd11;
      ino        = kve->kve_fileid;
      if (filename[0] != '/') {
         filename = NULL;
         foffset = 0;
      }
 
      prot = 0;
      if (kve->kve_protection & VKI_KVME_PROT_READ)  prot |= VKI_PROT_READ;
      if (kve->kve_protection & VKI_KVME_PROT_WRITE) prot |= VKI_PROT_WRITE;
      if (kve->kve_protection & VKI_KVME_PROT_EXEC)  prot |= VKI_PROT_EXEC;

      map_count = (p - (char *)procmap_buf)/kve->kve_structsize;

      if (tool_read_maps && map_count == 2) {
         aspacem_assert((prot & (VKI_PROT_READ | VKI_PROT_WRITE)) == (VKI_PROT_READ | VKI_PROT_WRITE));
         filename = rx_filename;
         dev = rx_dev;
         ino = rx_ino;
         foffset = rx_foffset;
      }
 
      if (record_gap && gapStart < start)
         (*record_gap) ( gapStart, start-gapStart );

      if (record_mapping && start < endPlusOne) {
         (*record_mapping) ( start, endPlusOne-start,
                             prot, dev, ino,
                           foffset, filename, tool_read_maps && map_count == 2 );
      }

      if (tool_read_maps && map_count == 1) {
         aspacem_assert((prot & (VKI_PROT_READ | VKI_PROT_EXEC)) == (VKI_PROT_READ | VKI_PROT_EXEC));
         rx_filename = filename;
         rx_dev = dev;
         rx_ino = ino;
         /* this is only accurate to the page alignment */
         rx_foffset = foffset + endPlusOne - start;
      }

      gapStart = endPlusOne;
      p += kve->kve_structsize;
   }
 
   if (record_gap && gapStart < Addr_MAX)
      (*record_gap) ( gapStart, Addr_MAX - gapStart + 1 );
}

/*------END-procmaps-parser-for-Freebsd--------------------------*/

/*------BEGIN-procmaps-parser-for-Solaris------------------------*/

#elif defined(VGO_solaris)

/* Note: /proc/self/xmap contains extended information about already
   materialized mappings whereas /proc/self/rmap contains information about
   all mappings including reserved but yet-to-materialize mappings (mmap'ed
   with MAP_NORESERVE flag, such as thread stacks). But /proc/self/rmap does
   not contain extended information found in /proc/self/xmap. Therefore
   information from both sources need to be combined.
 */

typedef struct
{
   Addr   addr;
   SizeT  size;
   UInt   prot;
   ULong  dev;
   ULong  ino;
   Off64T foffset;
   HChar  filename[VKI_PATH_MAX];
} Mapping;

static SizeT read_proc_file(const HChar *filename, HChar *buf,
                            SizeT buf_size, const HChar *buf_size_name,
                            SizeT entry_size)
{
   SysRes res = ML_(am_open)(filename, VKI_O_RDONLY, 0);
   if (sr_isError(res)) {
      HChar message[100];
      ML_(am_sprintf)(message, "Cannot open %s.", filename);
      ML_(am_barf)(message);
   }
   Int fd = sr_Res(res);

   Int r = ML_(am_read)(fd, buf, buf_size);
   ML_(am_close)(fd);
   if (r < 0) {
      HChar message[100];
      ML_(am_sprintf)(message, "I/O error on %s.", filename);
      ML_(am_barf)(message);
   }

   if (r >= buf_size)
      ML_(am_barf_toolow)(buf_size_name);

   if (r % entry_size != 0) {
      HChar message[100];
      ML_(am_sprintf)(message, "Bogus values read from %s.", filename);
      ML_(am_barf)(message);
   }

   return r / entry_size;
}

static Mapping *next_xmap(const HChar *buffer, SizeT entries, SizeT *idx,
                          Mapping *mapping)
{
   aspacem_assert(idx);
   aspacem_assert(mapping);

   if (*idx >= entries)
      return NULL; /* No more entries */

   const vki_prxmap_t *map = (const vki_prxmap_t *)buffer + *idx;

   mapping->addr = map->pr_vaddr;
   mapping->size = map->pr_size;

   mapping->prot = 0;
   if (map->pr_mflags & VKI_MA_READ)
      mapping->prot |= VKI_PROT_READ;
   if (map->pr_mflags & VKI_MA_WRITE)
      mapping->prot |= VKI_PROT_WRITE;
   if (map->pr_mflags & VKI_MA_EXEC)
      mapping->prot |= VKI_PROT_EXEC;

   if (map->pr_dev != VKI_PRNODEV) {
      mapping->dev = map->pr_dev;
      mapping->ino = map->pr_ino;
      mapping->foffset = map->pr_offset;
   }
   else {
      mapping->dev = 0;
      mapping->ino = 0;
      mapping->foffset = 0;
   }

   /* Try to get the filename. */
   mapping->filename[0] = '\0';
   if (map->pr_mapname[0] != '\0') {
      ML_(am_sprintf)(mapping->filename, "/proc/self/path/%s",
                      map->pr_mapname);
      Int r = ML_(am_readlink)(mapping->filename, mapping->filename,
                               sizeof(mapping->filename) - 1);
      if (r == -1) {
         /* If Valgrind is executed in a non-global zone and the link in
            /proc/self/path/ represents a file that is available through lofs
            from a global zone then the kernel may not be able to resolve the
            link.

            In such a case, return a corresponding /proc/self/object/ file to
            allow Valgrind to read the file if it is necessary.

            This can create some discrepancy for the sanity check. For
            instance, if a client program mmaps some file then the address
            space manager will have a correct zone-local name of that file,
            but the sanity check will receive a different file name from this
            code. This currently does not represent a problem because the
            sanity check ignores the file names (it uses device and inode
            numbers for the comparison).
          */
         ML_(am_sprintf)(mapping->filename, "/proc/self/object/%s",
                         map->pr_mapname);
      }
      else {
         aspacem_assert(r >= 0);
         mapping->filename[r] = '\0';
      }
   }

   *idx += 1;
   return mapping;
}

static Mapping *next_rmap(const HChar *buffer, SizeT entries, SizeT *idx,
                          Mapping *mapping)
{
   aspacem_assert(idx);
   aspacem_assert(mapping);

   if (*idx >= entries)
      return NULL; /* No more entries */

   const vki_prmap_t *map = (const vki_prmap_t *)buffer + *idx;

   mapping->addr = map->pr_vaddr;
   mapping->size = map->pr_size;

   mapping->prot = 0;
   if (map->pr_mflags & VKI_MA_READ)
      mapping->prot |= VKI_PROT_READ;
   if (map->pr_mflags & VKI_MA_WRITE)
      mapping->prot |= VKI_PROT_WRITE;
   if (map->pr_mflags & VKI_MA_EXEC)
      mapping->prot |= VKI_PROT_EXEC;

   mapping->dev = 0;
   mapping->ino = 0;
   mapping->foffset = 0;
   mapping->filename[0] = '\0';

   *idx += 1;
   return mapping;
}

/* Used for two purposes:
   1. Establish initial mappings upon the process startup
   2. Check mappings during aspacemgr sanity check
 */
static void parse_procselfmaps (
      void (*record_mapping)( Addr addr, SizeT len, UInt prot,
                              ULong dev, ULong ino, Off64T offset,
                              const HChar *filename, Bool ignore_offset ),
      void (*record_gap)( Addr addr, SizeT len )
   )
{
   Addr start = Addr_MIN;
   Addr gap_start = Addr_MIN;

#define M_XMAP_BUF (VG_N_SEGMENTS * sizeof(vki_prxmap_t))
   /* Static to keep it out of stack frame... */
   static HChar xmap_buf[M_XMAP_BUF];
   const Mapping *xmap = NULL;
   SizeT xmap_index = 0; /* Current entry */
   SizeT xmap_entries;
   Mapping xmap_mapping;
   Bool advance_xmap;

#define M_RMAP_BUF (VG_N_SEGMENTS * sizeof(vki_prmap_t))
   static HChar rmap_buf[M_RMAP_BUF];
   const Mapping *rmap = NULL;
   SizeT rmap_index = 0; /* Current entry */
   SizeT rmap_entries;
   Mapping rmap_mapping;
   Bool advance_rmap;

   /* Read fully /proc/self/xmap and /proc/self/rmap. */
   xmap_entries = read_proc_file("/proc/self/xmap", xmap_buf, M_XMAP_BUF,
                                 "M_XMAP_BUF", sizeof(vki_prxmap_t));

   rmap_entries = read_proc_file("/proc/self/rmap", rmap_buf, M_RMAP_BUF,
                                 "M_RMAP_BUF", sizeof(vki_prmap_t));

   /* Get the first xmap and rmap. */
   advance_xmap = True;
   advance_rmap = True;

   while (1) {
      /* Get next xmap or rmap if necessary. */
      if (advance_xmap) {
         xmap = next_xmap(xmap_buf, xmap_entries, &xmap_index, &xmap_mapping);
         advance_xmap = False;
      }
      if (advance_rmap) {
         rmap = next_rmap(rmap_buf, rmap_entries, &rmap_index, &rmap_mapping);
         advance_rmap = False;
      }

      /* Check if the end has been reached. */
      if (rmap == NULL)
         break;

      /* Invariants */
      if (xmap != NULL) {
         aspacem_assert(start <= xmap->addr);
         aspacem_assert(rmap->addr <= xmap->addr);
      }

      if (xmap != NULL && start == xmap->addr) {
         /* xmap mapping reached. */
         aspacem_assert(xmap->addr >= rmap->addr &&
                        xmap->addr + xmap->size <= rmap->addr + rmap->size);
         aspacem_assert(xmap->prot == rmap->prot);

         if (record_mapping != NULL)
            (*record_mapping)(xmap->addr, xmap->size, xmap->prot, xmap->dev,
                              xmap->ino, xmap->foffset,
                              (xmap->filename[0] != '\0') ?
                               xmap->filename : NULL, False);

         start = xmap->addr + xmap->size;
         advance_xmap = True;
      }
      else if (start >= rmap->addr) {
         /* Reserved-only part. */
         /* First calculate size until the end of this reserved mapping... */
         SizeT size = rmap->addr + rmap->size - start;
         /* ... but shrink it if some xmap is in a way. */
         if (xmap != NULL && size > xmap->addr - start)
            size = xmap->addr - start;

         if (record_mapping != NULL)
            (*record_mapping)(start, size, rmap->prot, 0, 0, 0, NULL, False);
         start += size;
      }
      else {
         /* Gap. */
         if (record_gap != NULL && gap_start < start)
            (*record_gap)(gap_start, start - gap_start);
         start = rmap->addr;
      }

      if (rmap->addr + rmap->size <= start)
         advance_rmap = True;

      gap_start = start;
   }

   if (record_gap != NULL && gap_start < Addr_MAX)
      (*record_gap)(gap_start, Addr_MAX - gap_start + 1);
}

/* parse_procselfmaps() callbacks do not allow for easy thread safety. */
static Addr found_addr;
static SizeT found_size;
static UInt found_prot;

/* Reports a new mapping into variables above. */
static void new_segment_found_callback(Addr addr, SizeT len, UInt prot,
   ULong dev, ULong ino, Off64T offset, const HChar *filename, Bool ignore_offset)
{
   aspacem_assert(addr <= addr + len - 1); 

   Int iLo = find_nsegment_idx(addr);
   Int iHi = find_nsegment_idx(addr + len - 1);
   aspacem_assert(iLo <= iHi);
   aspacem_assert(nsegments[iLo].start <= addr);
   aspacem_assert(nsegments[iHi].end   >= addr + len - 1);

   /* Do not perform any sanity checks. That is done in other places.
      Just find if a reported mapping is found in aspacemgr's book keeping. */
   for (Int i = iLo; i <= iHi; i++) {
      if ((nsegments[i].kind == SkFree) || (nsegments[i].kind == SkResvn)) {
         found_addr = addr;
         found_size = len;
         found_prot = prot;
         break;
      }
   }
}

/* Returns True if a new segment was found. */
Bool VG_(am_search_for_new_segment)(Addr *addr, SizeT *size, UInt *prot)
{
   found_addr = 0;
   parse_procselfmaps(new_segment_found_callback, NULL);

   if (found_addr != 0) {
      *addr = found_addr;
      *size = found_size;
      *prot = found_prot;
      return True;
   } else {
      return False;
   }
}

#endif // defined(VGO_solaris)

/*------END-procmaps-parser-for-Solaris--------------------------*/

#endif // defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
