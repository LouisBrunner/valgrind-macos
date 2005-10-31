
/*--------------------------------------------------------------------*/
/*--- The address space manager: segment initialisation and        ---*/
/*--- tracking, stack operations                                   ---*/
/*---                                                m_aspacemgr.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

/* One of the important design goals of the address space manager is
   to minimise dependence on other modules.  Hence the following
   minimal set of imports. */

#include "pub_core_basics.h"     // types

#include "pub_core_debuglog.h"   // VG_(debugLog)

#include "pub_core_libcbase.h"   // VG_(strlen), VG_(strcmp)
                                 // VG_IS_PAGE_ALIGNED
                                 // VG_PGROUNDDN, VG_PGROUNDUP

#include "pub_core_syscall.h"    // VG_(do_syscallN)
                                 // VG_(mk_SysRes_Error)
                                 // VG_(mk_SysRes_Success)

#include "pub_core_options.h"    // VG_(clo_sanity_level)

#include "vki_unistd.h"          // __NR_* constants

#include "pub_core_aspacemgr.h"  // self


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

     The available space is delimited by aspacem_minAddr and
     aspacem_maxAddr.  aspacem is flexible and can operate with these
     at any (sane) setting.  For 32-bit Linux, aspacem_minAddr is set
     to some low-ish value at startup (64M) and aspacem_maxAddr is
     derived from the stack pointer at system startup.  This seems a
     reliable way to establish the initial boundaries.

     64-bit Linux is similar except for the important detail that the
     upper boundary is set to 32G.  The reason is so that all
     anonymous mappings (basically all client data areas) are kept
     below 32G, since that is the maximum range that memcheck can
     track shadow memory using a fast 2-level sparse array.  It can go
     beyond that but runs much more slowly.  The 32G limit is
     arbitrary and is trivially changed.  So, with the current
     settings, programs on 64-bit Linux will appear to run out of
     address space and presumably fail at the 32G limit.  Given the
     9/8 space overhead of Memcheck, that means you should be able to
     memcheckify programs that use up to about 14G natively.

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
  enforce seperation of V and C, so that wild writes by C could not
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

/* Max number of segments we can track. */
#define VG_N_SEGMENTS 2000

/* Max number of segment file names we can track. */
#define VG_N_SEGNAMES 400

/* Max length of a segment file name. */
#define VG_MAX_SEGNAMELEN 1000


typedef
   struct {
      Bool  inUse;
      Bool  mark;
      HChar fname[VG_MAX_SEGNAMELEN];
   }
   SegName;

/* Filename table.  _used is the high water mark; an entry is only
   valid if its index >= 0, < _used, and its .inUse field == True.
   The .mark field is used to garbage-collect dead entries.
*/
static SegName segnames[VG_N_SEGNAMES];
static Int     segnames_used = 0;


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
      if (VG_(clo_sanity_level >= 3))                        \
         aspacem_assert(VG_(am_do_sync_check)                \
            (__PRETTY_FUNCTION__,__FILE__,__LINE__));        \
   } while (0) 

/* ------ end of STATE for the address-space manager ------ */

/* ------ Forwards decls ------ */
static void aspacem_exit      ( Int );
static Int  find_nsegment_idx ( Addr a );

static void parse_procselfmaps (
      void (*record_mapping)( Addr addr, SizeT len, UInt prot,
                              UInt dev, UInt ino, ULong offset, 
                              const UChar* filename ),
      void (*record_gap)( Addr addr, SizeT len )
   );


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Stuff to make aspacem almost completely independent of    ---*/
/*--- the rest of Valgrind.                                     ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

//--------------------------------------------------------------
// Simple assert and assert-like fns, which avoid dependence on
// m_libcassert, and hence on the entire debug-info reader swamp

static void aspacem_barf ( HChar* what )
{
  VG_(debugLog)(0, "aspacem", "Valgrind: FATAL: %s\n", what);
  VG_(debugLog)(0, "aspacem", "Exiting now.\n");
  aspacem_exit(1);
}

static void aspacem_barf_toolow ( HChar* what )
{
  VG_(debugLog)(0, "aspacem", "Valgrind: FATAL: %s is too low.\n", what);
  VG_(debugLog)(0, "aspacem", "  Increase it and rebuild.  "
                              "Exiting now.\n");
  aspacem_exit(1);
}

static void aspacem_assert_fail( const HChar* expr,
                                 const Char* file,
                                 Int line, 
                                 const Char* fn )
{
  VG_(debugLog)(0, "aspacem", "Valgrind: FATAL: aspacem assertion failed:\n");
  VG_(debugLog)(0, "aspacem", "  %s\n", expr);
  VG_(debugLog)(0, "aspacem", "  at %s:%d (%s)\n", file,line,fn);
  VG_(debugLog)(0, "aspacem", "Exiting now.\n");
  aspacem_exit(1);
}

#define aspacem_assert(expr)                             \
  ((void) ((expr) ? 0 :                                  \
           (aspacem_assert_fail(#expr,                   \
                                __FILE__, __LINE__,      \
                                __PRETTY_FUNCTION__))))


//--------------------------------------------------------------
// A simple sprintf implementation, so as to avoid dependence on
// m_libcprint.

static void add_to_aspacem_sprintf_buf ( HChar c, void *p )
{
   HChar** aspacem_sprintf_ptr = p;
   *(*aspacem_sprintf_ptr)++ = c;
}

static
UInt aspacem_vsprintf ( HChar* buf, const HChar *format, va_list vargs )
{
   Int ret;
   Char *aspacem_sprintf_ptr = buf;

   ret = VG_(debugLog_vprintf)
            ( add_to_aspacem_sprintf_buf, 
              &aspacem_sprintf_ptr, format, vargs );
   add_to_aspacem_sprintf_buf('\0', &aspacem_sprintf_ptr);

   return ret;
}

static
UInt aspacem_sprintf ( HChar* buf, const HChar *format, ... )
{
   UInt ret;
   va_list vargs;

   va_start(vargs,format);
   ret = aspacem_vsprintf(buf, format, vargs);
   va_end(vargs);

   return ret;
}


//--------------------------------------------------------------
// Direct access to a handful of syscalls.  This avoids dependence on
// m_libc*.  THESE DO NOT UPDATE THE SEGMENT LIST.  DO NOT USE THEM
// UNLESS YOU KNOW WHAT YOU ARE DOING.

SysRes VG_(am_do_mmap_NO_NOTIFY)( Addr start, SizeT length, UInt prot, 
                                  UInt flags, UInt fd, Off64T offset)
{
   SysRes res;
   aspacem_assert(VG_IS_PAGE_ALIGNED(offset));
#  if defined(VGP_x86_linux) || defined(VGP_ppc32_linux)
   res = VG_(do_syscall6)(__NR_mmap2, (UWord)start, length,
                          prot, flags, fd, offset / VKI_PAGE_SIZE);
#  elif defined(VGP_amd64_linux)
   res = VG_(do_syscall6)(__NR_mmap, (UWord)start, length, 
                         prot, flags, fd, offset);
#  else
#    error Unknown platform
#  endif
   return res;
}

static SysRes do_mprotect_NO_NOTIFY(Addr start, SizeT length, UInt prot)
{
   return VG_(do_syscall3)(__NR_mprotect, (UWord)start, length, prot );
}

static SysRes do_munmap_NO_NOTIFY(Addr start, SizeT length)
{
   return VG_(do_syscall2)(__NR_munmap, (UWord)start, length );
}

static SysRes do_extend_mapping_NO_NOTIFY( Addr  old_addr, SizeT old_len,
                                           SizeT new_len )
{
   /* Extend the mapping old_addr .. old_addr+old_len-1 to have length
      new_len, WITHOUT moving it.  If it can't be extended in place,
      fail. */
   return VG_(do_syscall5)(
             __NR_mremap, 
             old_addr, old_len, new_len, 
             0/*flags, meaning: must be at old_addr, else FAIL */,
             0/*new_addr, is ignored*/
          );
}

static SysRes do_relocate_nooverlap_mapping_NO_NOTIFY( 
                 Addr old_addr, Addr old_len, 
                 Addr new_addr, Addr new_len 
              )
{
   /* Move the mapping old_addr .. old_addr+old_len-1 to the new
      location and with the new length.  Only needs to handle the case
      where the two areas do not overlap, neither length is zero, and
      all args are page aligned. */
   return VG_(do_syscall5)(
             __NR_mremap, 
             old_addr, old_len, new_len, 
             VKI_MREMAP_MAYMOVE|VKI_MREMAP_FIXED/*move-or-fail*/,
             new_addr
          );
}

static Int aspacem_readlink(HChar* path, HChar* buf, UInt bufsiz)
{
   SysRes res;
   res = VG_(do_syscall3)(__NR_readlink, (UWord)path, (UWord)buf, bufsiz);
   return res.isError ? -1 : res.val;
}

static Int aspacem_fstat( Int fd, struct vki_stat* buf )
{
   SysRes res = VG_(do_syscall2)(__NR_fstat, fd, (UWord)buf);
   return res.isError ? (-1) : 0;
}

static void aspacem_exit( Int status )
{
   (void)VG_(do_syscall1)(__NR_exit_group, status );
   (void)VG_(do_syscall1)(__NR_exit, status );
   /* Why are we still alive here? */
   /*NOTREACHED*/
   *(volatile Int *)0 = 'x';
   aspacem_assert(2+2 == 5);
}

static SysRes aspacem_open ( const Char* pathname, Int flags, Int mode )
{  
   SysRes res = VG_(do_syscall3)(__NR_open, (UWord)pathname, flags, mode);
   return res;
}

static void aspacem_close ( Int fd )
{
   (void)VG_(do_syscall1)(__NR_close, fd);
}

static Int aspacem_read ( Int fd, void* buf, Int count)
{
   SysRes res = VG_(do_syscall3)(__NR_read, fd, (UWord)buf, count);
   return res.isError ? -1 : res.val;
}


//--------------------------------------------------------------
// Functions for extracting information about file descriptors.

/* Extract the device and inode numbers for a fd. */
static 
Bool get_inode_for_fd ( Int fd, /*OUT*/UWord* dev, /*OUT*/UWord* ino, /*OUT*/UInt* mode )
{
   struct vki_stat buf;
   Int r = aspacem_fstat(fd, &buf);
   if (r == 0) {
      *dev = buf.st_dev;
      *ino = buf.st_ino;
      *mode = buf.st_mode;
      return True;
   }
   return False;
}

/* Given a file descriptor, attempt to deduce its filename.  To do
   this, we use /proc/self/fd/<FD>.  If this doesn't point to a file,
   or if it doesn't exist, we return False. */
static
Bool get_name_for_fd ( Int fd, /*OUT*/HChar* buf, Int nbuf )
{
   Int   i;
   HChar tmp[64];

   aspacem_sprintf(tmp, "/proc/self/fd/%d", fd);
   for (i = 0; i < nbuf; i++) buf[i] = 0;
   
   if (aspacem_readlink(tmp, buf, nbuf) > 0 && buf[0] == '/')
      return True;
   else
      return False;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- SegName array management.                                 ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Searches the filename table to find an index for the given name.
   If none is found, an index is allocated and the name stored.  If no
   space is available we just give up.  If the string is too long to
   store, return -1.
*/
static Int allocate_segname ( const HChar* name )
{
   Int i, j, len;

   aspacem_assert(name);

   if (0) VG_(debugLog)(0,"aspacem","allocate_segname %s\n", name);

   len = VG_(strlen)(name);
   if (len >= VG_MAX_SEGNAMELEN-1) {
      return -1;
   }

   /* first see if we already have the name. */
   for (i = 0; i < segnames_used; i++) {
      if (!segnames[i].inUse)
         continue;
      if (0 == VG_(strcmp)(name, &segnames[i].fname[0])) {
         return i;
      }
   }

   /* no we don't.  So look for a free slot. */
   for (i = 0; i < segnames_used; i++)
      if (!segnames[i].inUse)
         break;

   if (i == segnames_used) {
      /* no free slots .. advance the high-water mark. */
      if (segnames_used+1 < VG_N_SEGNAMES) {
         i = segnames_used;
         segnames_used++;
      } else {
         aspacem_barf_toolow("VG_N_SEGNAMES");
      }
   }

   /* copy it in */
   segnames[i].inUse = True;
   for (j = 0; j < len; j++)
      segnames[i].fname[j] = name[j];
   aspacem_assert(len < VG_MAX_SEGNAMELEN);
   segnames[i].fname[len] = 0;
   return i;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Displaying the segment array.                             ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

static HChar* show_SegKind ( SegKind sk )
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

static HChar* show_ShrinkMode ( ShrinkMode sm )
{
   switch (sm) {
      case SmLower: return "SmLower";
      case SmUpper: return "SmUpper";
      case SmFixed: return "SmFixed";
      default: return "Sm?????";
   }
}

static void show_Addr_concisely ( /*OUT*/HChar* buf, Addr aA )
{
   HChar* fmt;
   ULong a = (ULong)aA;

   if (a < 10*1000*1000ULL) {
      fmt = "%7llu";
   } 
   else if (a < 999999ULL * (1ULL<<20)) {
      fmt = "%6llum";
      a >>= 20;
   }
   else if (a < 999999ULL * (1ULL<<30)) {
      fmt = "%6llug";
      a >>= 30;
   }
   else if (a < 999999ULL * (1ULL<<40)) {
      fmt = "%6llut";
      a >>= 40;
   }
   else {
      fmt = "%6llue";
      a >>= 50;
   }
   aspacem_sprintf(buf, fmt, a);
}


/* Show full details of an NSegment */

static void __attribute__ ((unused))
            show_nsegment_full ( Int logLevel, NSegment* seg )
{
   HChar* name = "(none)";
   if (seg->fnIdx >= 0 && seg->fnIdx < segnames_used
                       && segnames[seg->fnIdx].inUse
                       && segnames[seg->fnIdx].fname[0] != 0)
      name = segnames[seg->fnIdx].fname;

   VG_(debugLog)(logLevel, "aspacem",
      "NSegment{%s, start=0x%llx, end=0x%llx, smode=%s, dev=%llu, "
      "ino=%llu, offset=%llu, fnIdx=%d, hasR=%d, hasW=%d, hasX=%d, "
      "hasT=%d, mark=%d, name=\"%s\"}\n",
      show_SegKind(seg->kind),
      (ULong)seg->start,
      (ULong)seg->end,
      show_ShrinkMode(seg->smode),
      (ULong)seg->dev, (ULong)seg->ino, (ULong)seg->offset, seg->fnIdx,
      (Int)seg->hasR, (Int)seg->hasW, (Int)seg->hasX, (Int)seg->hasT,
      (Int)seg->mark, 
      name
   );
}


/* Show an NSegment in a user-friendly-ish way. */

static void show_nsegment ( Int logLevel, Int segNo, NSegment* seg )
{
   HChar len_buf[20];
   ULong len = ((ULong)seg->end) - ((ULong)seg->start) + 1;
   show_Addr_concisely(len_buf, len);

   switch (seg->kind) {

      case SkFree:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %s\n",
            segNo, show_SegKind(seg->kind),
            (ULong)seg->start, (ULong)seg->end, len_buf
         );
         break;

      case SkAnonC: case SkAnonV: case SkShmC:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %s %c%c%c%c%c\n",
            segNo, show_SegKind(seg->kind),
            (ULong)seg->start, (ULong)seg->end, len_buf,
            seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
            seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-',
            seg->isCH ? 'H' : '-'
         );
         break;

      case SkFileC: case SkFileV:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %s %c%c%c%c%c d=0x%03llx "
            "i=%-7lld o=%-7lld (%d)\n",
            segNo, show_SegKind(seg->kind),
            (ULong)seg->start, (ULong)seg->end, len_buf,
            seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
            seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-', 
            seg->isCH ? 'H' : '-',
            (ULong)seg->dev, (ULong)seg->ino, (Long)seg->offset, seg->fnIdx
         );
         break;

      case SkResvn:
         VG_(debugLog)(
            logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %s %c%c%c%c%c %s\n",
            segNo, show_SegKind(seg->kind),
            (ULong)seg->start, (ULong)seg->end, len_buf,
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
void VG_(am_show_nsegments) ( Int logLevel, HChar* who )
{
   Int i;
   VG_(debugLog)(logLevel, "aspacem",
                 "<<< SHOW_SEGMENTS: %s (%d segments, %d segnames)\n", 
                 who, nsegments_used, segnames_used);
   for (i = 0; i < segnames_used; i++) {
      if (!segnames[i].inUse)
         continue;
      VG_(debugLog)(logLevel, "aspacem",
                    "(%2d) %s\n", i, segnames[i].fname);
   }
   for (i = 0; i < nsegments_used; i++)
     show_nsegment( logLevel, i, &nsegments[i] );
   VG_(debugLog)(logLevel, "aspacem",
                 ">>>\n");
}


/* Get the filename corresponding to this segment, if known and if it
   has one.  The returned name's storage cannot be assumed to be
   persistent, so the caller should immediately copy the name
   elsewhere. */
HChar* VG_(am_get_filename)( NSegment* seg )
{
   Int i;
   aspacem_assert(seg);
   i = seg->fnIdx;
   if (i < 0 || i >= segnames_used || !segnames[i].inUse)
      return NULL;
   else
      return &segnames[i].fname[0];
}

/* Collect up the start addresses of all non-free, non-resvn segments.
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

Int VG_(am_get_segment_starts)( Addr* starts, Int nStarts )
{
   Int i, j, nSegs;

   /* don't pass dumbass arguments */
   aspacem_assert(nStarts >= 0);

   nSegs = 0;
   for (i = 0; i < nsegments_used; i++) {
      if (nsegments[i].kind == SkFree || nsegments[i].kind == SkResvn)
         continue;
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
      if (nsegments[i].kind == SkFree || nsegments[i].kind == SkResvn)
         continue;
      starts[j] = nsegments[i].start;
      j++;
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

static Bool sane_NSegment ( NSegment* s )
{
   if (s == NULL) return False;

   /* No zero sized segments and no wraparounds. */
   if (s->start >= s->end) return False;

   /* .mark is used for admin purposes only. */
   if (s->mark) return False;

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

static Bool maybe_merge_nsegments ( NSegment* s1, NSegment* s2 )
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
            return True;
         }
         break;

      case SkShmC:
         return False;

      default:
         break;
   
   }

   return False;
}


/* Sanity-check and canonicalise the segment array (merge mergable
   segments).  Returns True if any segments were merged. */

static Bool preen_nsegments ( void )
{
   Int i, j, r, w, nsegments_used_old = nsegments_used;

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

   /* Pass 3: free up unused string table slots */
   /* clear mark bits */
   for (i = 0; i < segnames_used; i++)
      segnames[i].mark = False;
   /* mark */
   for (i = 0; i < nsegments_used; i++) {
     j = nsegments[i].fnIdx;
      aspacem_assert(j >= -1 && j < segnames_used);
      if (j >= 0) {
         aspacem_assert(segnames[j].inUse);
         segnames[j].mark = True;
      }
   }
   /* release */
   for (i = 0; i < segnames_used; i++) {
      if (segnames[i].mark == False) {
         segnames[i].inUse = False;
         segnames[i].fname[0] = 0;
      }
   }

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
                                          UInt dev, UInt ino, ULong offset, 
                                          const UChar* filename )
{
   Int  iLo, iHi, i;
   Bool sloppyRXcheck;

   /* If a problem has already been detected, don't continue comparing
      segments, so as to avoid flooding the output with error
      messages. */
   if (!sync_check_ok)
      return;

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
      most recent NX-bit enabled CPUs) and so recent kernels mark most
      readable sections also as executable (in the /proc/self/maps
      they give out), which makes checking fail.  When sloppyRXcheck
      is True, the checker therefore regards R and X as
      interchangeable. */
   sloppyRXcheck = False;
#  if defined(VGA_x86)
   sloppyRXcheck = True;
#  endif

   if (sloppyRXcheck) {
      /* If either bit is set, ensure both are set. */
      if (prot & (VKI_PROT_READ|VKI_PROT_EXEC))
         prot |= (VKI_PROT_READ|VKI_PROT_EXEC);
   }

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

      cmp_offsets
         = nsegments[i].kind == SkFileC || nsegments[i].kind == SkFileV;

      cmp_devino
         = nsegments[i].dev != 0 || nsegments[i].ino != 0;

      /* Consider other reasons to not compare dev/inode */
      /* bproc does some godawful hack on /dev/zero at process
         migration, which changes the name of it, and its dev & ino */
      if (filename && 0==VG_(strcmp)(filename, "/dev/zero (deleted)"))
         cmp_devino = False;

      if (sloppyRXcheck) {
         /* If either bit is set, ensure both are set. */
         if (seg_prot & (VKI_PROT_READ|VKI_PROT_EXEC))
            seg_prot |= (VKI_PROT_READ|VKI_PROT_EXEC);
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
         sync_check_ok = False;
         VG_(debugLog)(
            0,"aspacem",
              "sync_check_mapping_callback: segment mismatch: V's seg:\n");
         show_nsegment_full( 0, &nsegments[i] );
         goto show_kern_seg;
      }
   }

   /* Looks harmless.  Keep going. */
   return;

  show_kern_seg:
   VG_(debugLog)(0,"aspacem",
                   "sync_check_mapping_callback: "
                   "segment mismatch: kernel's seg:\n");
   VG_(debugLog)(0,"aspacem", 
                   "start=0x%llx end=0x%llx prot=%u "
                   "dev=%u ino=%u offset=%lld name=\"%s\"\n",
                   (ULong)addr, ((ULong)addr) + ((ULong)len) - 1,
                   prot, dev, ino, offset, 
                   filename ? (HChar*)filename : "(none)" );
   return;
}

static void sync_check_gap_callback ( Addr addr, SizeT len )
{
   Int iLo, iHi, i;

   /* If a problem has already been detected, don't continue comparing
      segments, so as to avoid flooding the output with error
      messages. */
   if (!sync_check_ok)
      return;

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
         sync_check_ok = False;
         VG_(debugLog)(
            0,"aspacem",
              "sync_check_mapping_callback: segment mismatch: V's gap:\n");
         show_nsegment_full( 0, &nsegments[i] );
         goto show_kern_gap;
      }
   }

   /* Looks harmless.  Keep going. */
   return;

  show_kern_gap:
   VG_(debugLog)(0,"aspacem",
                   "sync_check_gap_callback: segment mismatch: kernel's gap:\n");
   VG_(debugLog)(0,"aspacem", 
                   "start=0x%llx end=0x%llx\n",
                   (ULong)addr, ((ULong)addr) + ((ULong)len) - 1 );
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
         HChar buf[100];
         VG_(am_show_nsegments)(0,"post syncheck failure");
         VG_(sprintf)(buf, "/bin/cat /proc/%d/maps", VG_(getpid)());
         VG_(system)(buf);
      }
#     endif

   }
   return sync_check_ok;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Low level access / modification of the segment array.     ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Binary search the interval array for a given address.  Since the
   array covers the entire address space the search cannot fail. */
static Int find_nsegment_idx ( Addr a )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid,
        lo = 0,
        hi = nsegments_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) {
         /* Not found.  This can't happen. */
         aspacem_barf("find_nsegment_idx: not found");
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


/* Finds the segment containing 'a'.  Only returns file/anon/resvn
   segments. */
NSegment* VG_(am_find_nsegment) ( Addr a )
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


/* Given a pointer to a seg, tries to figure out which one it is in
   nsegments[..].  Very paranoid. */
static Int segAddr_to_index ( NSegment* seg )
{
   Int i;
   if (seg < &nsegments[0] || seg >= &nsegments[nsegments_used])
      return -1;
   i = ((UChar*)seg - (UChar*)(&nsegments[0])) / sizeof(NSegment);
   if (i < 0 || i >= nsegments_used)
      return -1;
   if (seg == &nsegments[i])
      return i;
   return -1;
}


/* Find the next segment along from 'here', if it is a file/anon/resvn
   segment. */
NSegment* VG_(am_next_nsegment) ( NSegment* here, Bool fwds )
{
   Int i = segAddr_to_index(here);
   if (i < 0 || i >= nsegments_used)
      return NULL;
   if (fwds) {
      i++;
      if (i >= nsegments_used)
         return NULL;
   } else {
      i--;
      if (i < 0)
         return NULL;
   }
   switch (nsegments[i].kind) {
      case SkFileC: case SkFileV: case SkShmC:
      case SkAnonC: case SkAnonV: case SkResvn:
         return &nsegments[i];
      default:
         break;
   }
   return NULL;
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


/* Test if a piece of memory is addressable by the client with at
   least the "prot" protection permissions by examining the underlying
   segments.  If freeOk is True then SkFree areas are also allowed.
*/
static
Bool is_valid_for_client( Addr start, SizeT len, UInt prot, Bool freeOk )
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
      if ( (nsegments[i].kind == SkFileC 
            || nsegments[i].kind == SkAnonC
            || nsegments[i].kind == SkShmC
            || (nsegments[i].kind == SkFree  && freeOk)
            || (nsegments[i].kind == SkResvn && freeOk))
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
   return is_valid_for_client( start, len, prot, False/*free not OK*/ );
}

/* Variant of VG_(am_is_valid_for_client) which allows free areas to
   be consider part of the client's addressable space.  It also
   considers reservations to be allowable, since from the client's
   point of view they don't exist. */
Bool VG_(am_is_valid_for_client_or_free_or_resvn)
   ( Addr start, SizeT len, UInt prot )
{
   return is_valid_for_client( start, len, prot, True/*free is OK*/ );
}


/* Test if a piece of memory is addressable by valgrind with at least
   PROT_NONE protection permissions by examining the underlying
   segments. */
static Bool is_valid_for_valgrind( Addr start, SizeT len )
{
   Int  i, iLo, iHi;

   if (len == 0)
      return True; /* somewhat dubious case */
   if (start + len < start)
      return False; /* reject wraparounds */

   iLo = find_nsegment_idx(start);
   iHi = find_nsegment_idx(start + len - 1);
   for (i = iLo; i <= iHi; i++) {
      if (nsegments[i].kind == SkFileV || nsegments[i].kind == SkAnonV) {
         /* ok */
      } else {
         return False;
      }
   }
   return True;
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
      aspacem_barf_toolow("VG_N_SEGMENTS");
   for (j = nsegments_used-1; j > i; j--)
      nsegments[j+1] = nsegments[j];
   nsegments_used++;

   nsegments[i+1]       = nsegments[i];
   nsegments[i+1].start = a;
   nsegments[i].end     = a-1;

   if (nsegments[i].kind == SkFileV || nsegments[i].kind == SkFileC)
      nsegments[i+1].offset 
         += ((ULong)nsegments[i+1].start) - ((ULong)nsegments[i].start);

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

static void add_segment ( NSegment* seg )
{
   Int  i, iLo, iHi, delta;
   Bool segment_is_sane;

   Addr sStart = seg->start;
   Addr sEnd   = seg->end;

   aspacem_assert(sStart <= sEnd);
   aspacem_assert(VG_IS_PAGE_ALIGNED(sStart));
   aspacem_assert(VG_IS_PAGE_ALIGNED(sEnd+1));

   segment_is_sane = sane_NSegment(seg);
   if (!segment_is_sane) show_nsegment_full(0,seg);
   aspacem_assert(segment_is_sane);

   split_nsegments_lo_and_hi( sStart, sEnd, &iLo, &iHi );

   /* Now iLo .. iHi inclusive is the range of segment indices which
      seg will replace.  If we're replacing more than one segment,
      slide those above the range down to fill the hole. */
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
   seg->hasR = seg->hasW = seg->hasX = seg->hasT = seg->isCH = False;
   seg->mark = False;
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
                                 UInt dev, UInt ino, ULong offset, 
                                 const UChar* filename )
{
   NSegment seg;
   init_nsegment( &seg );
   seg.start  = addr;
   seg.end    = addr+len-1;
   seg.dev    = dev;
   seg.ino    = ino;
   seg.offset = offset;
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);
   seg.hasT   = False;

   seg.kind = SkAnonV;
   if (filename) { 
      seg.kind  = SkFileV;
      seg.fnIdx = allocate_segname( filename );
   }

   if (0) show_nsegment( 2,0, &seg );
   add_segment( &seg );
}

/* Initialise the address space manager, setting up the initial
   segment list, and reading /proc/self/maps into it.  This must
   be called before any other function.

   Takes a pointer to the SP at the time V gained control.  This is
   taken to be the highest usable address (more or less).  Based on
   that (and general consultation of tea leaves, etc) return a
   suggested end address for the client's stack. */

Addr VG_(am_startup) ( Addr sp_at_startup )
{
   NSegment seg;
   Addr     suggested_clstack_top;

   aspacem_assert(sizeof(Word)   == sizeof(void*));
   aspacem_assert(sizeof(Addr)   == sizeof(void*));
   aspacem_assert(sizeof(SizeT)  == sizeof(void*));
   aspacem_assert(sizeof(SSizeT) == sizeof(void*));

   { 
      /* If these fail, we'd better change the type of dev and ino in
         NSegment accordingly. */
      struct vki_stat buf;
      aspacem_assert(sizeof(buf.st_dev) == sizeof(seg.dev));
      aspacem_assert(sizeof(buf.st_ino) == sizeof(seg.ino));
   }

   /* Add a single interval covering the entire address space. */
   init_nsegment(&seg);
   seg.kind        = SkFree;
   seg.start       = Addr_MIN;
   seg.end         = Addr_MAX;
   nsegments[0]    = seg;
   nsegments_used  = 1;

   /* Establish address limits and block out unusable parts
      accordingly. */

   VG_(debugLog)(2, "aspacem", 
                    "        sp_at_startup = 0x%010llx (supplied)\n", 
                    (ULong)sp_at_startup );

   aspacem_minAddr = (Addr) 0x04000000; // 64M

#  if VG_WORDSIZE == 8
   aspacem_maxAddr = (Addr)0x800000000 - 1; // 32G
#  else
   aspacem_maxAddr = VG_PGROUNDDN( sp_at_startup ) - 1;
#  endif

   aspacem_cStart = aspacem_minAddr; // 64M
   aspacem_vStart = VG_PGROUNDUP((aspacem_minAddr + aspacem_maxAddr + 1) / 2);
#  ifdef ENABLE_INNER
   aspacem_vStart -= 0x10000000; // 256M
#  endif

   suggested_clstack_top = aspacem_maxAddr - 16*1024*1024ULL
                                           + VKI_PAGE_SIZE;

   aspacem_assert(VG_IS_PAGE_ALIGNED(aspacem_minAddr));
   aspacem_assert(VG_IS_PAGE_ALIGNED(aspacem_maxAddr + 1));
   aspacem_assert(VG_IS_PAGE_ALIGNED(aspacem_cStart));
   aspacem_assert(VG_IS_PAGE_ALIGNED(aspacem_vStart));
   aspacem_assert(VG_IS_PAGE_ALIGNED(suggested_clstack_top + 1));

   VG_(debugLog)(2, "aspacem", 
                    "              minAddr = 0x%010llx (computed)\n", 
                    (ULong)aspacem_minAddr);
   VG_(debugLog)(2, "aspacem", 
                    "              maxAddr = 0x%010llx (computed)\n", 
                    (ULong)aspacem_maxAddr);
   VG_(debugLog)(2, "aspacem", 
                    "               cStart = 0x%010llx (computed)\n", 
                    (ULong)aspacem_cStart);
   VG_(debugLog)(2, "aspacem", 
                    "               vStart = 0x%010llx (computed)\n", 
                    (ULong)aspacem_vStart);
   VG_(debugLog)(2, "aspacem", 
                    "suggested_clstack_top = 0x%010llx (computed)\n", 
                    (ULong)suggested_clstack_top);

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

   VG_(am_show_nsegments)(2, "With contents of /proc/self/maps");

   AM_SANITY_CHECK;
   return suggested_clstack_top;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- The core query-notify mechanism.                          ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Query aspacem to ask where a mapping should go. */

Addr VG_(am_get_advisory) ( MapRequest*  req, 
                            Bool         forClient, 
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

      The Default Policy is overriden by Policy Exception #1:

        If the request is for a fixed client map, we are prepared to
        grant it providing all areas inside the request are either
        free, reservations, or mappings belonging to the client.  In
        other words we are prepared to let the client trash its own
        mappings if it wants to.

      The Default Policy is overriden by Policy Exception #2:

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

   Addr startPoint = forClient ? aspacem_cStart : aspacem_vStart;

   Addr reqStart = req->rkind==MAny ? 0 : req->start;
   Addr reqEnd   = reqStart + req->len - 1;
   Addr reqLen   = req->len;

   /* These hold indices for segments found during search, or -1 if not
      found. */
   Int floatIdx = -1;
   Int fixedIdx = -1;

   aspacem_assert(nsegments_used > 0);

   if (0) {
      VG_(am_show_nsegments)(0,"getAdvisory");
      VG_(debugLog)(0,"aspacem", "getAdvisory 0x%llx %lld\n", 
                      (ULong)req->start, (ULong)req->len);
   }

   /* Reject zero-length requests */
   if (req->len == 0) {
      *ok = False;
      return 0;
   }

   /* Reject wraparounds */
   if ((req->rkind==MFixed || req->rkind==MHint)
       && req->start + req->len < req->start) {
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
   fixed_not_required = req->rkind == MAny;

   i = find_nsegment_idx(startPoint);

   /* Examine holes from index i back round to i-1.  Record the
      index first fixed hole and the first floating hole which would
      satisfy the request. */
   for (j = 0; j < nsegments_used; j++) {

      if (nsegments[i].kind != SkFree) {
         i++;
         if (i >= nsegments_used) i = 0;
         continue;
      }

      holeStart = nsegments[i].start;
      holeEnd   = nsegments[i].end;

      /* Stay sane .. */
      aspacem_assert(holeStart <= holeEnd);
      aspacem_assert(aspacem_minAddr <= holeStart);
      aspacem_assert(holeEnd <= aspacem_maxAddr);

      /* See if it's any use to us. */
      holeLen = holeEnd - holeStart + 1;

      if (fixedIdx == -1 && holeStart <= reqStart && reqEnd <= holeEnd)
         fixedIdx = i;

      if (floatIdx == -1 && holeLen >= reqLen)
         floatIdx = i;
  
      /* Don't waste time searching once we've found what we wanted. */
      if ((fixed_not_required || fixedIdx >= 0) && floatIdx >= 0)
         break;

      i++;
      if (i >= nsegments_used) i = 0;
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
            return nsegments[floatIdx].start;
         }
         *ok = False;
         return 0;
      case MAny:
         if (floatIdx >= 0) {
            *ok = True;
            return nsegments[floatIdx].start;
         }
         *ok = False;
         return 0;
      default: 
         break;
   }

   /*NOTREACHED*/
   aspacem_barf("getAdvisory: unknown request kind");
   *ok = False;
   return 0;
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
   return VG_(am_get_advisory)( &mreq, True/*client*/, ok );
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
   UWord    dev, ino;
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
   seg.kind   = (flags & VKI_MAP_ANONYMOUS) ? SkAnonC : SkFileC;
   seg.start  = a;
   seg.end    = a + len - 1;
   seg.offset = offset;
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);
   if (!(flags & VKI_MAP_ANONYMOUS)) {
      if (get_inode_for_fd(fd, &dev, &ino, &mode)) {
         seg.dev = dev;
         seg.ino = ino;
         seg.mode = mode;
      }
      if (get_name_for_fd(fd, buf, VKI_PATH_MAX)) {
         seg.fnIdx = allocate_segname( buf );
      }
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
   VG_(am_notify_munmap), we merely record the given info, and don't
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
   seg.kind  = SkFree;
   seg.start = start;
   seg.end   = start + len - 1;
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
   SysRes     sres;
   NSegment   seg;
   Addr       advised;
   Bool       ok;
   MapRequest req;
   UWord      dev, ino;
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
   advised = VG_(am_get_advisory)( &req, True/*client*/, &ok );
   if (!ok || advised != start)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* We have been advised that the mapping is allowable at the
      specified address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             start, length, prot, 
             VKI_MAP_FIXED|VKI_MAP_PRIVATE, 
             fd, offset 
          );
   if (sres.isError)
      return sres;

   if (sres.val != start) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)do_munmap_NO_NOTIFY( sres.val, length );
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
   if (get_inode_for_fd(fd, &dev, &ino, &mode)) {
      seg.dev = dev;
      seg.ino = ino;
      seg.mode = mode;
   }
   if (get_name_for_fd(fd, buf, VKI_PATH_MAX)) {
      seg.fnIdx = allocate_segname( buf );
   }
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
   advised = VG_(am_get_advisory)( &req, True/*client*/, &ok );
   if (!ok || advised != start)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* We have been advised that the mapping is allowable at the
      specified address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             start, length, prot, 
             VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
             0, 0 
          );
   if (sres.isError)
      return sres;

   if (sres.val != start) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)do_munmap_NO_NOTIFY( sres.val, length );
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

SysRes VG_(am_mmap_anon_float_client) ( SizeT length, Int prot )
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
   advised = VG_(am_get_advisory)( &req, True/*client*/, &ok );
   if (!ok)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* We have been advised that the mapping is allowable at the
      advised address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             advised, length, prot, 
             VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
             0, 0 
          );
   if (sres.isError)
      return sres;

   if (sres.val != advised) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)do_munmap_NO_NOTIFY( sres.val, length );
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
   add_segment( &seg );

   AM_SANITY_CHECK;
   return sres;
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
   advised = VG_(am_get_advisory)( &req, False/*valgrind*/, &ok );
   if (!ok)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* We have been advised that the mapping is allowable at the
      specified address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             advised, length, 
             VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC, 
             VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
             0, 0 
          );
   if (sres.isError)
      return sres;

   if (sres.val != advised) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)do_munmap_NO_NOTIFY( sres.val, length );
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Ok, the mapping succeeded.  Now notify the interval map. */
   init_nsegment( &seg );
   seg.kind  = SkAnonV;
   seg.start = advised;
   seg.end   = seg.start + VG_PGROUNDUP(length) - 1;
   seg.hasR  = True;
   seg.hasW  = True;
   seg.hasX  = True;
   add_segment( &seg );

   AM_SANITY_CHECK;
   return sres;
}

/* Really just a wrapper around VG_(am_mmap_anon_float_valgrind). */

void* VG_(am_shadow_alloc)(SizeT size)
{
   SysRes sres = VG_(am_mmap_anon_float_valgrind)( size );
   return sres.isError ? NULL : (void*)sres.val;
}



/* Map a file at an unconstrained address for V, and update the
   segment array accordingly.  This is used by V for transiently
   mapping in object files to read their debug info.  */

SysRes VG_(am_mmap_file_float_valgrind) ( SizeT length, UInt prot, 
                                          Int fd, Off64T offset )
{
   SysRes     sres;
   NSegment   seg;
   Addr       advised;
   Bool       ok;
   MapRequest req;
   UWord      dev, ino;
   UInt       mode;
   HChar      buf[VKI_PATH_MAX];
 
   /* Not allowable. */
   if (length == 0 || !VG_IS_PAGE_ALIGNED(offset))
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* Ask for an advisory.  If it's negative, fail immediately. */
   req.rkind = MAny;
   req.start = 0;
   req.len   = length;
   advised = VG_(am_get_advisory)( &req, True/*client*/, &ok );
   if (!ok)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* We have been advised that the mapping is allowable at the
      specified address.  So hand it off to the kernel, and propagate
      any resulting failure immediately. */
   sres = VG_(am_do_mmap_NO_NOTIFY)( 
             advised, length, prot, 
             VKI_MAP_FIXED|VKI_MAP_PRIVATE, 
             fd, offset 
          );
   if (sres.isError)
      return sres;

   if (sres.val != advised) {
      /* I don't think this can happen.  It means the kernel made a
         fixed map succeed but not at the requested location.  Try to
         repair the damage, then return saying the mapping failed. */
      (void)do_munmap_NO_NOTIFY( sres.val, length );
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Ok, the mapping succeeded.  Now notify the interval map. */
   init_nsegment( &seg );
   seg.kind   = SkFileV;
   seg.start  = sres.val;
   seg.end    = seg.start + VG_PGROUNDUP(length) - 1;
   seg.offset = offset;
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);
   if (get_inode_for_fd(fd, &dev, &ino, &mode)) {
      seg.dev = dev;
      seg.ino = ino;
      seg.mode - mode;
   }
   if (get_name_for_fd(fd, buf, VKI_PATH_MAX)) {
      seg.fnIdx = allocate_segname( buf );
   }
   add_segment( &seg );

   AM_SANITY_CHECK;
   return sres;
}


/* --- --- munmap helper --- --- */

static 
SysRes am_munmap_both_wrk ( /*OUT*/Bool* need_discard,
                            Addr start, SizeT len, Bool forClient )
{
   Bool   d;
   SysRes sres;

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
      if (!is_valid_for_valgrind( start, len ))
         goto eINVAL;
   }

   d = any_Ts_in_range( start, len );

   sres = do_munmap_NO_NOTIFY( start, len );
   if (sres.isError)
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
   if (!r.isError)
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


/* Let SEG be an anonymous client mapping.  This fn extends the
   mapping by DELTA bytes, taking the space from a reservation section
   which must be adjacent.  If DELTA is positive, the segment is
   extended forwards in the address space, and the reservation must be
   the next one along.  If DELTA is negative, the segment is extended
   backwards in the address space and the reservation must be the
   previous one.  DELTA must be page aligned.  abs(DELTA) must not
   exceed the size of the reservation segment minus one page, that is,
   the reservation segment after the operation must be at least one
   page long. */

Bool VG_(am_extend_into_adjacent_reservation_client) ( NSegment* seg, 
                                                       SSizeT    delta )
{
   Int    segA, segR;
   UInt   prot;
   SysRes sres;

   /* Find the segment array index for SEG.  If the assertion fails it
      probably means you passed in a bogus SEG. */
   segA = segAddr_to_index( seg );
   aspacem_assert(segA >= 0 && segA < nsegments_used);

   if (nsegments[segA].kind != SkAnonC)
      return False;

   if (delta == 0)
      return True;

   prot =   (nsegments[segA].hasR ? VKI_PROT_READ : 0)
          | (nsegments[segA].hasW ? VKI_PROT_WRITE : 0)
          | (nsegments[segA].hasX ? VKI_PROT_EXEC : 0);

   aspacem_assert(VG_IS_PAGE_ALIGNED(delta<0 ? -delta : delta));

   if (delta > 0) {

      /* Extending the segment forwards. */
      segR = segA+1;
      if (segR >= nsegments_used
          || nsegments[segR].kind != SkResvn
          || nsegments[segR].smode != SmLower
          || nsegments[segR].start != nsegments[segA].end + 1
          || delta + VKI_PAGE_SIZE 
                > (nsegments[segR].end - nsegments[segR].start + 1))
        return False;
        
      /* Extend the kernel's mapping. */
      sres = VG_(am_do_mmap_NO_NOTIFY)( 
                nsegments[segR].start, delta,
                prot,
                VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
                0, 0 
             );
      if (sres.isError)
         return False; /* kernel bug if this happens? */
      if (sres.val != nsegments[segR].start) {
         /* kernel bug if this happens? */
        (void)do_munmap_NO_NOTIFY( sres.val, delta );
        return False;
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
          || nsegments[segR].smode != SmUpper
          || nsegments[segR].end + 1 != nsegments[segA].start
          || delta + VKI_PAGE_SIZE 
                > (nsegments[segR].end - nsegments[segR].start + 1))
        return False;
        
      /* Extend the kernel's mapping. */
      sres = VG_(am_do_mmap_NO_NOTIFY)( 
                nsegments[segA].start-delta, delta,
                prot,
                VKI_MAP_FIXED|VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS, 
                0, 0 
             );
      if (sres.isError)
         return False; /* kernel bug if this happens? */
      if (sres.val != nsegments[segA].start-delta) {
         /* kernel bug if this happens? */
        (void)do_munmap_NO_NOTIFY( sres.val, delta );
        return False;
      }

      /* Ok, success with the kernel.  Update our structures. */
      nsegments[segR].end -= delta;
      nsegments[segA].start -= delta;
      aspacem_assert(nsegments[segR].start <= nsegments[segR].end);

   }

   AM_SANITY_CHECK;
   return True;
}


/* --- --- --- resizing/move a mapping --- --- --- */

/* Let SEG be a client mapping (anonymous or file).  This fn extends
   the mapping forwards only by DELTA bytes, and trashes whatever was
   in the new area.  Fails if SEG is not a single client mapping or if
   the new area is not accessible to the client.  Fails if DELTA is
   not page aligned.  *seg is invalid after a successful return.  If
   *need_discard is True after a successful return, the caller should
   immediately discard translations from the new area. */

Bool VG_(am_extend_map_client)( /*OUT*/Bool* need_discard,
                                NSegment* seg, SizeT delta )
{
   Addr     xStart;
   SysRes   sres;
   NSegment seg_copy = *seg;
   SizeT    seg_old_len = seg->end + 1 - seg->start;

   if (seg->kind != SkFileC && seg->kind != SkAnonC)
      return False;

   if (delta == 0 || !VG_IS_PAGE_ALIGNED(delta)) 
      return False;

   xStart = seg->end+1;
   if (xStart + delta < delta)
      return False;

   if (!VG_(am_is_valid_for_client_or_free_or_resvn)( xStart, delta, 
                                                      VKI_PROT_NONE ))
      return False;

   AM_SANITY_CHECK;
   sres = do_extend_mapping_NO_NOTIFY( seg->start, 
                                       seg_old_len,
                                       seg_old_len + delta );
   if (sres.isError) {
      AM_SANITY_CHECK;
      return False;
   }

   *need_discard = any_Ts_in_range( seg_copy.end+1, delta );

   seg_copy.end += delta;
   add_segment( &seg_copy );

   AM_SANITY_CHECK;
   return True;
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
   NSegment seg, oldseg;

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

   if (nsegments[iLo].kind != SkFileC && nsegments[iLo].kind != SkAnonC)
      return False;

   sres = do_relocate_nooverlap_mapping_NO_NOTIFY( old_addr, old_len, 
                                                   new_addr, new_len );
   if (sres.isError) {
      AM_SANITY_CHECK;
      return False;
   }

   *need_discard = any_Ts_in_range( old_addr, old_len )
                   || any_Ts_in_range( new_addr, new_len );

   oldseg = nsegments[iLo];

   /* Create a free hole in the old location. */
   init_nsegment( &seg );
   seg.kind  = SkFree;
   seg.start = old_addr;
   seg.end   = old_addr + old_len - 1;
   add_segment( &seg );

   /* Mark the new area based on the old seg. */
   if (oldseg.kind == SkFileC) {
      oldseg.offset += ((ULong)old_addr) - ((ULong)oldseg.start);
   } else {
      aspacem_assert(oldseg.kind == SkAnonC);
      aspacem_assert(oldseg.offset == 0);
   }
   oldseg.start = new_addr;
   oldseg.end   = new_addr + new_len - 1;
   add_segment( &oldseg );

   AM_SANITY_CHECK;
   return True;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Manage stacks for Valgrind itself.                        ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Allocate and initialise a VgStack (anonymous client space).
   Protect the stack active area and the guard areas appropriately.
   Returns NULL on failure, else the address of the bottom of the
   stack.  On success, also sets *initial_sp to what the stack pointer
   should be set to. */

VgStack* VG_(am_alloc_VgStack)( /*OUT*/Addr* initial_sp )
{
   Int      szB;
   SysRes   sres;
   VgStack* stack;
   UInt*    p;
   Int      i;

   /* Allocate the stack. */
   szB = VG_STACK_GUARD_SZB 
         + VG_STACK_ACTIVE_SZB + VG_STACK_GUARD_SZB;

   sres = VG_(am_mmap_anon_float_valgrind)( szB );
   if (sres.isError)
      return NULL;

   stack = (VgStack*)sres.val;

   aspacem_assert(VG_IS_PAGE_ALIGNED(szB));
   aspacem_assert(VG_IS_PAGE_ALIGNED(stack));
   
   /* Protect the guard areas. */
   sres = do_mprotect_NO_NOTIFY( 
             (Addr) &stack[0], 
             VG_STACK_GUARD_SZB, VKI_PROT_NONE 
          );
   if (sres.isError) goto protect_failed;
   VG_(am_notify_mprotect)( 
      (Addr) &stack->bytes[0], 
      VG_STACK_GUARD_SZB, VKI_PROT_NONE 
   );

   sres = do_mprotect_NO_NOTIFY( 
             (Addr) &stack->bytes[VG_STACK_GUARD_SZB + VG_STACK_ACTIVE_SZB], 
             VG_STACK_GUARD_SZB, VKI_PROT_NONE 
          );
   if (sres.isError) goto protect_failed;
   VG_(am_notify_mprotect)( 
      (Addr) &stack->bytes[VG_STACK_GUARD_SZB + VG_STACK_ACTIVE_SZB],
      VG_STACK_GUARD_SZB, VKI_PROT_NONE 
   );

   /* Looks good.  Fill the active area with junk so we can later
      tell how much got used. */

   p = (UInt*)&stack->bytes[VG_STACK_GUARD_SZB];
   for (i = 0; i < VG_STACK_ACTIVE_SZB/sizeof(UInt); i++)
      p[i] = 0xDEADBEEF;

   *initial_sp = (Addr)&stack->bytes[VG_STACK_GUARD_SZB + VG_STACK_ACTIVE_SZB];
   *initial_sp -= 8;
   *initial_sp &= ~((Addr)0xF);

   VG_(debugLog)( 1,"aspacem","allocated thread stack at 0x%llx size %d\n",
                  (ULong)(Addr)stack, szB);
   AM_SANITY_CHECK;
   return stack;

  protect_failed:
   /* The stack was allocated, but we can't protect it.  Unmap it and
      return NULL (failure). */
   (void)do_munmap_NO_NOTIFY( (Addr)stack, szB );
   AM_SANITY_CHECK;
   return NULL;
}


/* Figure out how many bytes of the stack's active area have not
   been used.  Used for estimating if we are close to overflowing it. */

Int VG_(am_get_VgStack_unused_szB)( VgStack* stack )
{
   Int i;
   UInt* p;

   p = (UInt*)&stack->bytes[VG_STACK_GUARD_SZB];
   for (i = 0; i < VG_STACK_ACTIVE_SZB/sizeof(UInt); i++)
      if (p[i] != 0xDEADBEEF)
         break;

   return i * sizeof(UInt);
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- A simple parser for /proc/self/maps on Linux 2.4.X/2.6.X. ---*/
/*--- Almost completely independent of the stuff above.  The    ---*/
/*--- only function it 'exports' to the code above this comment ---*/
/*--- is parse_procselfmaps.                                    ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Size of a smallish table used to read /proc/self/map entries. */
#define M_PROCMAP_BUF 100000

/* static ... to keep it out of the stack frame. */
static Char procmap_buf[M_PROCMAP_BUF];

/* Records length of /proc/self/maps read into procmap_buf. */
static Int  buf_n_tot;

/* Helper fns. */

static Int hexdigit ( Char c )
{
   if (c >= '0' && c <= '9') return (Int)(c - '0');
   if (c >= 'a' && c <= 'f') return 10 + (Int)(c - 'a');
   if (c >= 'A' && c <= 'F') return 10 + (Int)(c - 'A');
   return -1;
}

static Int decdigit ( Char c )
{
   if (c >= '0' && c <= '9') return (Int)(c - '0');
   return -1;
}

static Int readchar ( const Char* buf, Char* ch )
{
   if (*buf == 0) return 0;
   *ch = *buf;
   return 1;
}

static Int readhex ( const Char* buf, UWord* val )
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

static Int readhex64 ( const Char* buf, ULong* val )
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

static Int readdec ( const Char* buf, UInt* val )
{
   Int n = 0;
   *val = 0;
   while (hexdigit(*buf) >= 0) {
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
   fd = aspacem_open( "/proc/self/maps", VKI_O_RDONLY, 0 );
   if (fd.isError)
      aspacem_barf("can't open /proc/self/maps");

   buf_n_tot = 0;
   do {
      n_chunk = aspacem_read( fd.val, &procmap_buf[buf_n_tot],
                              M_PROCMAP_BUF - buf_n_tot );
      buf_n_tot += n_chunk;
   } while ( n_chunk > 0 && buf_n_tot < M_PROCMAP_BUF );

   aspacem_close(fd.val);

   if (buf_n_tot >= M_PROCMAP_BUF-5)
      aspacem_barf_toolow("M_PROCMAP_BUF");
   if (buf_n_tot == 0)
      aspacem_barf("I/O error on /proc/self/maps");

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

   So the sig of the called fn might be

      void (*record_mapping)( Addr start, SizeT size, UInt prot,
			      UInt dev, UInt info,
                              ULong foffset, UChar* filename )

   Note that the supplied filename is transiently stored; record_mapping 
   should make a copy if it wants to keep it.

   Nb: it is important that this function does not alter the contents of
       procmap_buf!
*/
static void parse_procselfmaps (
      void (*record_mapping)( Addr addr, SizeT len, UInt prot,
                              UInt dev, UInt ino, ULong offset, 
                              const UChar* filename ),
      void (*record_gap)( Addr addr, SizeT len )
   )
{
   Int    i, j, i_eol;
   Addr   start, endPlusOne, gapStart;
   UChar* filename;
   UChar  rr, ww, xx, pp, ch, tmp;
   UInt	  ino, prot;
   UWord  maj, min;
   ULong  foffset;

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

      j = readdec(&procmap_buf[i], &ino);
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
      aspacem_exit(1);

    read_line_ok:

      /* Try and find the name of the file mapped to this segment, if
         it exists.  Note that files can contains spaces. */

      // Move i to the next non-space char, which should be either a '/' or
      // a newline.
      while (procmap_buf[i] == ' ' && i < buf_n_tot-1) i++;
      
      // Move i_eol to the end of the line.
      i_eol = i;
      while (procmap_buf[i_eol] != '\n' && i_eol < buf_n_tot-1) i_eol++;

      // If there's a filename...
      if (i < i_eol-1 && procmap_buf[i] == '/') {
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

      if (record_gap && gapStart < start)
         (*record_gap) ( gapStart, start-gapStart );

      (*record_mapping) ( start, endPlusOne-start, 
                          prot, maj * 256 + min, ino,
                          foffset, filename );

      if ('\0' != tmp) {
         filename[i_eol - i] = tmp;
      }

      i = i_eol + 1;
      gapStart = endPlusOne;
   }

   if (record_gap && gapStart < Addr_MAX)
      (*record_gap) ( gapStart, Addr_MAX - gapStart + 1 );
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
