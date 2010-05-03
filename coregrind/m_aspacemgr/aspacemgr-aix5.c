
/*--------------------------------------------------------------------*/
/*--- The address space manager: segment initialisation and        ---*/
/*--- tracking, stack operations                                   ---*/
/*---                                                              ---*/
/*--- Implementation for AIX5                   m_aspacemgr-aix5.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2010 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#if defined(VGO_aix5)

/* *************************************************************
   DO NOT INCLUDE ANY OTHER FILES HERE.
   ADD NEW INCLUDES ONLY TO priv_aspacemgr.h
   AND THEN ONLY AFTER READING DIRE WARNINGS THERE TOO.
   ************************************************************* */

#include "priv_aspacemgr.h"


/* Note: many of the exported functions implemented below are
   described more fully in comments in pub_core_aspacemgr.h.
*/

/* This provides a minimal address space management facility for AIX5.
   It is not as comprehensive, robust or efficient as its Linux
   counterpart.

   It does implement the advise/notify concept described in
   aspacemgr-linux.c, but minimally.  It only keeps track of the
   mappings belonging to Valgrind; the client can do what it likes so
   long as it doesn't trash Valgrind's mappings.

   This is unfortunate, but the root problem is that it is impossible
   to find out on AIX what the complete set of mappings for a process
   is.  Sure, AIX does have /proc/pid/map, but it's weak compared to
   Linux's: it just shows some small subset of the mappings, not all
   of them.  So it is not very useful: it can't be used to discover
   the true initial process mapping state, and it can't be used to
   cross-check Valgrind's internal mapping table, as is done at
   --sanity-level=3 and above on Linux.
*/


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- The Address Space Manager's state.                        ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Describes AIX5-specific segment kinds */
typedef
   enum {
      ASkFree=1,  // free space
      ASkMText,   // module text (code) mapping
      ASkMData,   // module data (& bss) mapping
      ASkFileV,   // file mapping belonging to valgrind
      ASkAnonC,   // anonymous mapping belonging to the client
      ASkAnonV,   // anonymous mapping belonging to valgrind
      ASkShmemC,  // shm mapping belonging to the client
      ASkPreAlloc // area preallocated from sbrk
   }
   AixSegKind;

/* Segment table entries, in summary:

   ASkFree   start end
   ASkMText  start end r w x sibling ismainexe fname mname
   ASkMData  start end r w x sibling
   FileV     start end r w x fname offset
   AnonC     start end r w x fromP isCH
   AnonV     start end r w x fromP
   ShmemC    start end r w x
   PreAlloc  start end

   Entries are non-overlapping and cover the entire address space
   exactly (as in the Linux aspacem).  Unlike Linux there are no
   alignment constraints, since we're just recording what's going on,
   rather than controlling it.

   MText/MData are XCOFF mapped modules, as determined by looking at
   /proc/../map.  MText is the primary entry and contains the text
   range.  MData contains the data range, if the module has a data
   mapping (usually but not always).  MText also holds the avma of the
   corresponding data segment start, if any, (sibling field) so it can
   be found and the two added/removed together.  Similarly MData
   contains the address of the corresponding MText (also sibling).

   fname/mname only apply to MText.  To find the fname/mname for MData
   you have to look at the corresponding MText entry, which is
   guaranteed to exist.  MText may exist without a corresponding MData
   but not vice versa.  Kludge: in fact fname/mname have to be
   allowed in MData, else read_procselfmap doesn't work.

   MText may have a zero sibling pointer, indicating that there is no
   corresponding MData.  But MData must have a nonzero sibling pointer
   since MData without MText is not allowed.  Implication is that
   neither MText nor MData may be mapped at zero as this would mess up
   the representation, but I don't think that will ever happen since
   AIX uses page zero as a readonly const-zero area.

   For MData entries, the data section size acquired from /proc/../map
   appears to also include the bss, so there is no need for any
   further handling of that.

   isCH indicates whether an AnonC area is part of the client heap
   or not.  May not be set for any other kind of area.

   File and member names are entries into the string table.

   fromP, for AnonC/AnonV, if True, indicates that the segment was
   allocated from a PreAlloc area, and so should be returned to that
   state upon deallocation.  If False, indicates that the segment
   should be unmapped on deallocation.
*/
typedef
   struct {
      AixSegKind kind;

      /* ALL: extent */
      /* Note: zero-length segments are not allowed.  That guarantees
         that start <= end. */
      Addr start;  // lowest addr in range (ALL)
      Addr end;    // highest addr in range (ALL)

      /* ALL except Free */
      Bool hasR;
      Bool hasW;
      Bool hasX;

      /* misc */
      Addr   sibling;   // MText, MData only: addr of MData/MText
      Bool   isMainExe; // MText only: is this the main executable?
      Bool   isCH;      // AnonC only: is this part of the client's heap?
      Bool   fromP;     // AnonC, AnonV only: originated from PreAlloc?
      UChar* fname;     // MText, FileV only: filename
      UChar* mname;     // MText only: member name if present
      Off64T offset;    // FileV only: file offset
   }
   AixSegment;


#define VG_N_ASEGMENTS 5000

typedef
   struct {
      AixSegment seg[VG_N_ASEGMENTS];
      Int        used;
   }
   AixSegments;


/* ------ start of STATE for the address-space manager ------ */

/* A table of zero-terminated strings (file names etc).  This
   is only ever added to. */

#define VG_N_ASTRTAB 200000
static Int strtab_used = 0;
static UChar strtab[VG_N_ASTRTAB];

#define Addr_MIN ((Addr)0)
#define Addr_MAX ((Addr)(-1ULL))

/* The main array of AixSegments, in order as required. */

static AixSegments asegs_pri;

/* and two auxiliary arrays. */

static AixSegments asegs_tnew;
static AixSegments asegs_told;

/* The assumed size of the main thread's stack, so that we can add a
   segment for it at startup. */

#define N_FAKE_STACK_PAGES_MIN 4096  /* 16M fake stack */ /* default size */
#define N_FAKE_STACK_PAGES_MAX 32768 /* 128M fake stack */ /* max size? */


/* Hacks which are probably for AIX 'millicode'.  Note: ensure
   these stay page aligned. */

#define MAGIC_PAGES_1_BASE  0x3000
#define MAGIC_PAGES_1_SIZE  (2*0x1000)

#define MAGIC_PAGES_2_BASE  0xC000
#define MAGIC_PAGES_2_SIZE  (4*0x1000)


#define AM_SANITY_CHECK(_who)                                  \
   do {                                                        \
      if (VG_(clo_sanity_level >= 3)) {                        \
         Bool ok = sane_AixSegments(&asegs_pri);               \
         if (!ok)                                              \
            VG_(debugLog)(0,"aspace", "sanity check failed, "  \
                                      "who = %s\n", _who);     \
            aspacem_assert(ok);                                \
      }                                                        \
   } while (0)

/* When preallocating a block from sbrk-world, how much extra
   should we pre-emptively acquire? */

//#define AM_PREALLOC_EXTRA (512 * 1024)
//#define AM_PREALLOC_EXTRA 0x0800000  /* 8 M */
#define AM_PREALLOC_EXTRA 0x4000000  /* 64 M */

/* The AIX5 aspacem implementation needs to be told when it is and
   isn't allowed to use sbrk to allocate memory.  Hence: */
Bool VG_(am_aix5_sbrk_allowed) = True;

/* ------ end of STATE for the address-space manager ------ */

/* ------ Forwards decls ------ */
static void parse_procselfmap ( /*OUT*/ AixSegments* );


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Stuff for 4K (small-page-size) rounding.                  ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

#define AM_4K_PAGESZ 4096

static Bool AM_IS_4K_ALIGNED ( UWord w )
{
   UWord m = AM_4K_PAGESZ-1;
   return toBool( (w & m) == 0 );
}

static UWord AM_4K_ROUNDUP ( UWord w )
{
   UWord m = AM_4K_PAGESZ-1;
   return (w+m) & (~m);
}

static UWord AM_64K_ROUNDUP ( UWord w )
{
   UWord m = 0x10000-1;
   return (w+m) & (~m);
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- String table management.                                  ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Add the given string into the string table (or find an existing
   copy of it) and return a pointer to the in-table version.  The
   pointer will be valid for the entire rest of the run. */

static UChar* add_to_strtab ( UChar* str )
{
   Int off, len;
   /* First, look for the string. */
   off = 0;
   while (off < strtab_used) {
      if (0 == VG_(strcmp)(str, &strtab[off]))
         return &strtab[off];
      off += VG_(strlen)(&strtab[off]) + 1;
   }
   /* not present?  we'll have to copy it then. */
   len = VG_(strlen)(str);
   if (len + 1 + strtab_used > VG_N_ASTRTAB)
      ML_(am_barf_toolow)("VG_N_ASTRTAB");
   off = strtab_used;
   for (; *str; str++)
      strtab[strtab_used++] = *str;
   strtab[strtab_used++] = 0;
   aspacem_assert(strtab_used <= VG_N_ASTRTAB);
   return &strtab[off];
}


static Bool is_in_strtab ( UChar* str )
{
   if (str < &strtab[0]) 
      return False;
   if (str >= &strtab[strtab_used])
      return False;
   if (str > &strtab[0] && str[-1] != 0)
      return False;
   return True;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Low level AixSegment stuff.                               ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

static void init_AixSegment ( AixSegment* s )
{
   s->kind      = 0; /* invalid */
   s->start     = 0;
   s->end       = 0;
   s->hasR      = False;
   s->hasW      = False;
   s->hasX      = False;
   s->sibling   = 0;
   s->isMainExe = False;
   s->isCH      = False;
   s->fromP     = False;
   s->fname     = NULL;
   s->mname     = NULL;
   s->offset    = 0;
}


static HChar* name_of_AixSegKind ( AixSegKind sk )
{
   switch (sk) {
      case ASkFree:     return "Free ";
      case ASkMText:    return "MText";
      case ASkMData:    return "MData";
      case ASkAnonV:    return "AnonV";
      case ASkAnonC:    return "AnonC";
      case ASkFileV:    return "FileV";
      case ASkShmemC:   return "ShmC ";
      case ASkPreAlloc: return "PreAl";
      default:        ML_(am_barf)("name_of_AixSegKind");
                      /*NOTREACHED*/
                      return NULL;
   }
}


static 
void show_AixSegment ( Int logLevel, Int segNo, AixSegment* seg )
{
   HChar* segName = name_of_AixSegKind( seg->kind );
   switch (seg->kind) {
      case ASkFree:
         VG_(debugLog)(logLevel, "aspacem",
            "%3d: %s %010llx-%010llx\n",
            segNo, /*segName*/"     ",
            (ULong)seg->start, (ULong)seg->end
         );
         break;
      case ASkMText:
         VG_(debugLog)(logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %c%c%c-- (d %010llx) %s%s%s%s\n",
            segNo, seg->isMainExe ? "MTEXT" : "MText",
            (ULong)seg->start, (ULong)seg->end,
            seg->hasR ? 'r' : '-',
            seg->hasW ? 'w' : '-',
            seg->hasX ? 'x' : '-',
            (ULong)seg->sibling,
            seg->fname,
            seg->mname ? "(" : "",
            seg->mname ? (HChar*)seg->mname : "",
            seg->mname ? ")" : ""
         );
         break;
      case ASkMData:
         VG_(debugLog)(logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %c%c%c-- (t %010llx)\n",
            segNo, "MData",
            (ULong)seg->start, (ULong)seg->end,
            seg->hasR ? 'r' : '-',
            seg->hasW ? 'w' : '-',
            seg->hasX ? 'x' : '-',
            (ULong)seg->sibling
         );
         break;
      case ASkFileV: 
         VG_(debugLog)(logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %c%c%c-- %6lld %s\n",
            segNo, segName,
            (ULong)seg->start, (ULong)seg->end,
            seg->hasR ? 'r' : '-',
            seg->hasW ? 'w' : '-',
            seg->hasX ? 'x' : '-',
            seg->offset,
            seg->fname
         );
         break;
      case ASkAnonV: 
      case ASkAnonC:
      case ASkShmemC:
         VG_(debugLog)(logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %c%c%c%c%c\n",
            segNo, segName,
            (ULong)seg->start, (ULong)seg->end,
            seg->hasR ? 'r' : '-',
            seg->hasW ? 'w' : '-',
            seg->hasX ? 'x' : '-',
            seg->kind==ASkAnonC && seg->isCH ? 'H' : '-',
            seg->fromP ? 'P' : '-'
         );
         break;
      case ASkPreAlloc:
         VG_(debugLog)(logLevel, "aspacem",
            "%3d: %s %010llx-%010llx %c%c%c-- (size %llu)\n",
            segNo, segName,
            (ULong)seg->start, (ULong)seg->end,
            seg->hasR ? 'r' : '-',
            seg->hasW ? 'w' : '-',
            seg->hasX ? 'x' : '-',
            (ULong)seg->end - (ULong)seg->start + 1
         );
         break;
      default:
         VG_(debugLog)(logLevel, "aspacem",
                       "%3d: show_AixSegment: unknown segment\n", 
                       segNo);
         break;
   }
}


static void init_AixSegments ( AixSegments* segs )
{
   segs->used = 1;
   init_AixSegment( &segs->seg[0] );
   segs->seg[0].kind  = ASkFree;
   segs->seg[0].start = Addr_MIN;
   segs->seg[0].end   = Addr_MAX;
}


static 
void show_AixSegments ( Int logLevel, HChar* who, AixSegments* segs )
{
   Int i;
   VG_(debugLog)(logLevel, "aspacem", "<<< %s\n", who);
   for (i = 0; i < segs->used; i++)
      show_AixSegment( logLevel, i, &segs->seg[i] );
   VG_(debugLog)(logLevel, "aspacem", ">>>\n");
}


static Bool sane_AixSegment ( AixSegment* seg )
{
   /* disallow zero and negative length segments */
   if (seg->end < seg->start)
      return False;

   switch (seg->kind) {
      case ASkFree:
         if (seg->hasR || seg->hasW || seg->hasX)
            return False;
         if (seg->isMainExe || seg->sibling != 0 || seg->offset != 0)
            return False;
         if (seg->fname || seg->mname)
            return False;
         if (seg->isCH || seg->fromP)
            return False;
         break;
      case ASkMText:
         if (!is_in_strtab(seg->fname))
            return False;
         if (seg->mname && !is_in_strtab(seg->mname))
            return False;
         if (seg->offset != 0)
            return False;
         if (seg->isCH || seg->fromP)
            return False;
         break;
      case ASkMData:
         if (seg->isMainExe || seg->sibling == 0 || seg->offset != 0)
            return False;
         /* fname/mname have to be allowed in MData, else
            read_procselfmap doesn't work.  Unfortunately. */
         /*
         if (seg->fname || seg->mname)
            return False;
         */
         if (seg->isCH || seg->fromP)
            return False;
         break;
      case ASkFileV:
         if (!is_in_strtab(seg->fname))
            return False;
         if (seg->mname != NULL)
            return False;
         if (seg->isMainExe || seg->sibling != 0)
            return False;
         if (seg->isCH || seg->fromP)
            return False;
         break;
      case ASkShmemC:
      case ASkAnonV:
      case ASkAnonC:
         if (seg->fname || seg->mname)
            return False;
         if (seg->isMainExe || seg->sibling != 0)
            return False;
         if (seg->offset != 0)
            return False;
         if (seg->kind != ASkAnonC && seg->isCH)
            return False;
         if ( (!(seg->kind == ASkAnonV || seg->kind == ASkAnonC))
              && seg->fromP)
            return False;
         break;
      case ASkPreAlloc:
         if (seg->fname || seg->mname)
            return False;
         if (seg->isMainExe || seg->sibling != 0)
            return False;
         if (seg->offset != 0)
            return False;
         if (seg->kind != ASkAnonC && seg->isCH)
            return False;
         if (seg->fromP)
            return False;
         if (!AM_IS_4K_ALIGNED(seg->start))
            return False;
         if (!AM_IS_4K_ALIGNED(seg->end + 1))
            return False;
         if (!(seg->hasR && seg->hasW && seg->hasX))
            return False;
         break;
      default:
         return False;
   }
   return True;
}


/* Binary search the interval array for a given address.  Since the
   array covers the entire address space the search cannot fail. */
static Int find_asegment_idx ( AixSegments* segs, Addr a )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid,
        lo = 0,
        hi = segs->used-1;
   aspacem_assert(lo <= hi);
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) {
         /* Not found.  This can't happen. */
         ML_(am_barf)("find_nsegment_idx: not found");
      }
      mid      = (lo + hi) / 2;
      a_mid_lo = segs->seg[mid].start;
      a_mid_hi = segs->seg[mid].end;

      if (a < a_mid_lo) { hi = mid-1; continue; }
      if (a > a_mid_hi) { lo = mid+1; continue; }
      aspacem_assert(a >= a_mid_lo && a <= a_mid_hi);
      aspacem_assert(0 <= mid && mid < segs->used);
      return mid;
   }
}


static Bool sane_AixSegments ( AixSegments* segs )
{
   Int i;

   /* Check endpoints */
   if (segs->used < 1 || segs->used > VG_N_ASEGMENTS) {
      VG_(debugLog)(0, "aspacem", "sane_AixSegments: bad ->used");
      return False;
   }
   if (segs->seg[0].start != Addr_MIN
       || segs->seg[segs->used-1].end != Addr_MAX) {
      VG_(debugLog)(0, "aspacem", "sane_AixSegments: bad endpoints");
      return False;
   }

   /* Check each segment, and check entire range is covered. */
   for (i = 0; i < segs->used; i++) {
      if (!sane_AixSegment( &segs->seg[i] )) {
         VG_(debugLog)(0, "aspacem", 
                          "sane_AixSegments: bad segment %d\n", i);
         return False;
      }
   }
   for (i = 1; i < segs->used; i++) {
      if (segs->seg[i-1].end + 1 != segs->seg[i].start) {
         VG_(debugLog)(0, "aspacem", 
                          "sane_AixSegments: bad transition at %d/%d\n", i-1,i);
         return False;
      }
   }

   /* Now we know 'seg' is safe for use in find_asegment_idx().
      Check the sibling pointers for MText/MData.

      Also check that the segment starting at address zero is neither
      MText nor MData (since this would mess up the sibling pointer
      representation; see comments above.)  Failure of this is not per
      se a logic failure, but it does indicate that the kernel
      unexpectedly placed MText or MData at zero, and our
      representation is therefore inadequate.
   */
   if (segs->seg[0].kind == ASkMText || segs->seg[0].kind == ASkMData) {
      VG_(debugLog)(0, "aspacem", 
                       "sane_AixSegments: ASkMText/ASkMData at address zero\n");
      return False;
   }

   for (i = 0; i < segs->used-1; i++) {

      AixSegment *s1, *s2;

      s1 = &segs->seg[i];

      if (s1->kind == ASkMData) {
         s2 = &segs->seg[ find_asegment_idx(segs, s1->sibling) ];
         if (s2->kind != ASkMText
             || find_asegment_idx(segs, s2->sibling) != i) {
            VG_(debugLog)(0, "aspacem", "sane_AixSegments: bad sibling "
                                        "link(s) for ASkData\n");
            return False;
         }
      }

      if (s1->kind == ASkMText && s1->sibling != 0) {
         s2 = &segs->seg[ find_asegment_idx(segs, s1->sibling) ];
         if (s2->kind != ASkMData
             || find_asegment_idx(segs, s2->sibling) != i) {
            VG_(debugLog)(0, "aspacem", "sane_AixSegments: bad sibling "
                                        "link(s) for ASkText\n");
            return False;
         }
      }

   }

   return True;
}


/* Try merging s2 into s1, if possible.  If successful, s1 is
   modified, and True is returned.  Otherwise s1 is unchanged and
   False is returned. */

static Bool maybe_merge_asegments ( AixSegment* s1, AixSegment* s2 )
{
   if (s1->kind != s2->kind) 
      return False;

   if (s1->end+1 != s2->start)
      return False;

   switch (s1->kind) {

      case ASkFree:
         s1->end = s2->end;
         return True;

      case ASkAnonC:
      case ASkAnonV:
         if (s1->hasR == s2->hasR && s1->hasW == s2->hasW 
             && s1->hasX == s2->hasX && s1->isCH == s2->isCH
             && s1->fromP == s2->fromP) {
            s1->end = s2->end;
            return True;
         }
         break;

      /* not really necessary, but .. */
      case SkFileV:
         if (s1->hasR == s2->hasR
             && s1->hasW == s2->hasW && s1->hasX == s2->hasX
             && s1->fname == s2->fname
             && s2->offset == s1->offset
                + ((ULong)s2->start) - ((ULong)s1->start) ) {
            s1->end = s2->end;
            return True;
         }
         break;

      /* it's important to merge PreAlloc's back together to avoid
         fragmenting PreAlloc'd space unnecessarily */
      case ASkPreAlloc:
         s1->end = s2->end;
         return True;

      default:
         break;
   }

   return False;
}


/* Merge mergable segments in SEGS. */

static void preen_asegments ( AixSegments* segs )
{
   Int r, w;

   aspacem_assert(segs->used >= 1);
   if (segs->used == 1)
      return;

   w = 0;
   for (r = 1; r < segs->used; r++) {
      if (maybe_merge_asegments(&segs->seg[w], &segs->seg[r])) {
         /* nothing */
      } else {
         w++;
         if (w != r) 
            segs->seg[w] = segs->seg[r];
      }
   }
   w++;
   aspacem_assert(w > 0 && w <= segs->used);
   segs->used = w;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Modifying a segment array, and constructing segments.     ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Split the segment containing 'a' into two, so that 'a' is
   guaranteed to be the start of a new segment.  If 'a' is already the
   start of a segment, do nothing. */

static void split_asegment_at ( AixSegments* segs, Addr a )
{
   Int i, j;

   aspacem_assert(a > 0);
   aspacem_assert(segs->used >= 1);
 
   i = find_asegment_idx(segs, a);
   aspacem_assert(i >= 0 && i < segs->used);

   if (segs->seg[i].start == a)
      /* 'a' is already the start point of a segment, so nothing to be
         done. */
      return;

   /* else we have to slide the segments upwards to make a hole */
   if (segs->used >= VG_N_ASEGMENTS)
      ML_(am_barf_toolow)("VG_N_ASEGMENTS");
   for (j = segs->used-1; j > i; j--)
      segs->seg[j+1] = segs->seg[j];
   segs->used++;

   segs->seg[i+1]       = segs->seg[i];
   segs->seg[i+1].start = a;
   segs->seg[i].end     = a-1;

   if (segs->seg[i].kind == ASkFileV /* || segs->seg[i].kind == ASkFileC*/)
      segs->seg[i+1].offset 
         += ((ULong)segs->seg[i+1].start) - ((ULong)segs->seg[i].start);

   aspacem_assert(sane_AixSegment(&segs->seg[i]));
   aspacem_assert(sane_AixSegment(&segs->seg[i+1]));
}


/* Do the minimum amount of segment splitting necessary to ensure that
   sLo is the first address denoted by some segment and sHi is the
   highest address denoted by some other segment.  Returns the indices
   of the lowest and highest segments in the range. */

static 
void split_asegments_lo_and_hi ( AixSegments* segs,
                                 Addr sLo, Addr sHi,
                                 /*OUT*/Int* iLo,
                                 /*OUT*/Int* iHi )
{
   aspacem_assert(sLo < sHi);

   if (sLo > 0)
      split_asegment_at(segs, sLo);
   if (sHi < Addr_MAX)
      split_asegment_at(segs, sHi+1);

   *iLo = find_asegment_idx(segs,sLo);
   *iHi = find_asegment_idx(segs,sHi);
   aspacem_assert(0 <= *iLo && *iLo < segs->used);
   aspacem_assert(0 <= *iHi && *iHi < segs->used);
   aspacem_assert(*iLo <= *iHi);
   aspacem_assert(segs->seg[*iLo].start == sLo);
   aspacem_assert(segs->seg[*iHi].end == sHi);
   /* Not that I'm overly paranoid or anything, definitely not :-) */
}


/* Add SEG to the collection, deleting/truncating any it overlaps.
   This deals with all the tricky cases of splitting up segments as
   needed.  Contents of SEG are copied. */

static void add_asegment ( AixSegments* segs, AixSegment* seg )
{
   Int  i, iLo, iHi, delta;
   Bool segment_is_sane;

   Addr sStart = seg->start;
   Addr sEnd   = seg->end;

   aspacem_assert(sStart <= sEnd);

   segment_is_sane = sane_AixSegment(seg);
   if (!segment_is_sane) show_AixSegment(0,0,seg);
   aspacem_assert(segment_is_sane);

   split_asegments_lo_and_hi( segs, sStart, sEnd, &iLo, &iHi );

   /* Now iLo .. iHi inclusive is the range of segment indices which
      seg will replace.  If we're replacing more than one segment,
      slide those above the range down to fill the hole. */
   delta = iHi - iLo;
   aspacem_assert(delta >= 0);
   if (delta > 0) {
      for (i = iLo; i < segs->used-delta; i++)
         segs->seg[i] = segs->seg[i+delta];
      segs->used -= delta;
   }
   aspacem_assert(segs->used >= 1);

   segs->seg[iLo] = *seg;

   preen_asegments(segs);
   if (0) VG_(am_show_nsegments)(0,"AFTER preen (add_segment)");
}


/* Convert everything in SEG except MData and MText into Free,
   then preen, so as to retain normalised form. */

static void knockout_non_module_segs ( AixSegments* segs )
{
   Int i;
   Addr s, e;
   for (i = 0; i < segs->used; i++) {
      if (segs->seg[i].kind == ASkFree
          || segs->seg[i].kind == ASkMText
          || segs->seg[i].kind == ASkMData)
         continue;
      s = segs->seg[i].start;
      e = segs->seg[i].end;
      init_AixSegment( &segs->seg[i] );
      segs->seg[i].start = s;
      segs->seg[i].end = e;
      segs->seg[i].kind = ASkFree;
   }
   preen_asegments(segs);
   aspacem_assert( sane_AixSegments(segs) );
}


/* Copy a segment array. */

static void copy_asegments_d_s ( AixSegments* dst, AixSegments* src )
{
   Int i;
   aspacem_assert(src->used >= 1 && src->used < VG_N_ASEGMENTS);
   dst->used = src->used;
   for (i = 0; i < src->used; i++)
      dst->seg[i] = src->seg[i];
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Re-reading /proc/../map and updating MText/MData segments ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Find out the size of the AixCodeSegChange that must be
   presented to VG_(am_aix5_reread_procmap). */

Int VG_(am_aix5_reread_procmap_howmany_directives)(void)
{
   /* In the worst imaginable case, all the tracked modules could have
      disappeared and been replaced with different ones.  Hence: */
   return 2 * VG_N_ASEGMENTS;
}


static 
void add_pri_text_and_data_segs ( AixSegment* tnew, AixSegment* dnew )
{
   Bool dExists = (dnew->end - dnew->start + 1) != 0;
   aspacem_assert(tnew->kind == ASkMText);
   aspacem_assert(dnew->kind == ASkMData);
   if (dExists) {
      aspacem_assert(tnew->sibling == dnew->start);
      aspacem_assert(dnew->sibling == tnew->start);
      add_asegment(&asegs_pri, tnew);
      add_asegment(&asegs_pri, dnew);
   } else {
      aspacem_assert(tnew->sibling == 0);
      add_asegment(&asegs_pri, tnew);
   }
}

static 
void del_pri_text_and_data_segs ( AixSegment* told, AixSegment* dold )
{
   AixSegment fre;
   Bool       dExists = (dold->end - dold->start + 1) != 0;
   aspacem_assert(told->kind == ASkMText);
   aspacem_assert(dold->kind == ASkMData);
   init_AixSegment( &fre );
   fre.kind = ASkFree;
   if (dExists) {
      aspacem_assert(told->sibling == dold->start);
      aspacem_assert(dold->sibling == told->start);
      fre.start = told->start;
      fre.end   = told->end;
      add_asegment(&asegs_pri, &fre);
      fre.start = dold->start;
      fre.end   = dold->end;
      add_asegment(&asegs_pri, &fre);
   } else {
      aspacem_assert(told->sibling == 0);
      fre.start = told->start;
      fre.end   = told->end;
      add_asegment(&asegs_pri, &fre);
   }
}


/* Tell aspacem that /proc/<pid>/map may have changed (eg following
   __loadx) and so it should be re-read, and the code/data segment
   list updated accordingly.  The resulting array of AixCodeChangeSeg
   directives are written to 'directives', and the number of entries
   to *ndirectives. */

void VG_(am_aix5_reread_procmap)
     ( /*OUT*/AixCodeSegChange* directives, /*OUT*/Int* ndirectives )
{
   Int        ixold, ixnew;
   Bool       done_old, done_new;
   AixSegment *olds, *news;

   /* First, read /proc/../map into asegs_tnew.  Copy asegs_pri into
      asegs_told, and remove everything except MData and MText, so as
      to generate something we can sanely compare with asegs_tnew.
      Walk asegs_told and asegs_tnew together, writing the differences
      to 'directives', and modifying asegs_pri accordingly. */
   parse_procselfmap( &asegs_tnew );
   copy_asegments_d_s( &asegs_told, &asegs_pri );
   knockout_non_module_segs( &asegs_told );

   *ndirectives = 0;

#  define MODIFY_PRI(_dir, _asegs, _ixt, _acquire) \
      do { \
         Int        _ixd; \
         AixSegment *_segt, *_segd; \
         AixSegment _segd_dummy; \
         aspacem_assert(_ixt >= 0 && _ixt < _asegs.used); \
         _segt = &_asegs.seg[_ixt]; \
         aspacem_assert(_segt->kind == ASkMText); \
         if (_segt->sibling) { \
            _ixd = find_asegment_idx( &_asegs, _segt->sibling ); \
            _segd = &_asegs.seg[_ixd]; \
            aspacem_assert(_segd->kind == ASkMData); \
            aspacem_assert(_segt->sibling == _segd->start); \
         } else { \
            init_AixSegment( &_segd_dummy ); \
            _segd_dummy.kind = ASkMData; \
            _segd_dummy.start = 1; \
            _segd_dummy.end   = 0; \
            _segd = &_segd_dummy; \
         } \
         if (_segd != &_segd_dummy) \
            aspacem_assert(_segd->sibling == _segt->start); \
         \
         (_dir).code_start = (_segt)->start; \
         (_dir).code_len   = (_segt)->end - (_segt)->start + 1; \
         (_dir).data_start = (_segd)->start; \
         (_dir).data_len   = (_segd)->end - (_segd)->start + 1; \
         (_dir).file_name  = (_segt)->fname; \
         (_dir).mem_name   = (_segt)->mname; \
         (_dir).is_mainexe = (_acquire) ? (_segt)->isMainExe : False; \
         (_dir).acquire    = (_acquire); \
         \
         if (_acquire) { \
            add_pri_text_and_data_segs( _segt, _segd ); \
         } else { \
            del_pri_text_and_data_segs( _segt, _segd ); \
         } \
      } while (0)

   ixold = 0; /* indexes asegs_told */
   ixnew = 0; /* indexes asegs_tnew */

   while (True) {

      aspacem_assert(ixold >= 0 && ixold < asegs_told.used);
      aspacem_assert(ixnew >= 0 && ixnew < asegs_tnew.used);

      /* Advance ixold and ixnew to the next MText in their
         respective arrays. */
      while (ixold < asegs_told.used 
             && asegs_told.seg[ixold].kind != ASkMText) {
         aspacem_assert(asegs_told.seg[ixold].kind == ASkFree
                        || asegs_told.seg[ixold].kind == ASkMData);
         ixold++;
      }
      while (ixnew < asegs_tnew.used 
             && asegs_tnew.seg[ixnew].kind != ASkMText) {
         aspacem_assert(asegs_tnew.seg[ixnew].kind == ASkFree
                        || asegs_tnew.seg[ixnew].kind == ASkMData);
         ixnew++;
      }

      aspacem_assert(ixold >= 0 && ixold <= asegs_told.used);
      aspacem_assert(ixnew >= 0 && ixnew <= asegs_tnew.used);

      done_old = ixold == asegs_told.used;
      done_new = ixnew == asegs_tnew.used;

      if (done_old && done_new)
         goto both_done;
      if (done_old && !done_new)
         goto finishup_new;
      if (done_new && !done_old)
         goto finishup_old;

      olds = &asegs_told.seg[ixold];
      news = &asegs_tnew.seg[ixnew];

      aspacem_assert(olds->kind == ASkMText);
      aspacem_assert(news->kind == ASkMText);

      if (0) {
         show_AixSegment(0,ixold,&asegs_told.seg[ixold]); 
         show_AixSegment(0,ixnew,&asegs_tnew.seg[ixnew]); 
         VG_(debugLog)(0, "aspacem", "\n");
      }

      /* Here, if olds->start < news->start, then the old sequence has
         an entry which the new one doesn't, so a module has been
         unloaded.  If news->start < olds->start then the new sequence
         has a module the old one doesn't, so a module has been
         loaded.  If news->start ==olds->start then the module is
         unchanged.  Except, we should check a bit more carefully in
         the zero case. */
      if (olds->start == news->start) {
         if (olds->start == news->start
             && olds->end == news->end
             && olds->fname == news->fname
             && olds->mname == news->mname
             && olds->sibling == news->sibling
             && olds->isMainExe == news->isMainExe) {
            /* really identical, do nothing */
         } else {
            /* Dubious; mark it as an unload of old and load of
               new. */
            MODIFY_PRI(directives[*ndirectives], asegs_told, ixold, False);
            (*ndirectives)++;
            aspacem_assert(*ndirectives <= 2 * VG_N_ASEGMENTS);
            MODIFY_PRI(directives[*ndirectives], asegs_tnew, ixnew, True);
            (*ndirectives)++;
            aspacem_assert(*ndirectives <= 2 * VG_N_ASEGMENTS);
         }
         ixold++;
         ixnew++;
         continue;
      }

      if (olds->start < news->start) {
         /* discard olds */
         MODIFY_PRI(directives[*ndirectives], asegs_told, ixold, False);
         (*ndirectives)++;
         aspacem_assert(*ndirectives <= 2 * VG_N_ASEGMENTS);
         ixold++;
         continue;
      }

      if (news->start < olds->start) {
         /* acquire news */
         MODIFY_PRI(directives[*ndirectives], asegs_tnew, ixnew, True);
         (*ndirectives)++;
         aspacem_assert(*ndirectives <= 2 * VG_N_ASEGMENTS);
         ixnew++;
         continue;
      }
      /* NOTREACHED */
      aspacem_assert(0);
   }

  finishup_new:
   olds = NULL;
   aspacem_assert(ixold == asegs_told.used);
   aspacem_assert(ixnew < asegs_tnew.used);
   while (ixnew < asegs_tnew.used) {
      news = &asegs_tnew.seg[ixnew];
      aspacem_assert(news->kind == ASkMText || news->kind == ASkMData
                     || news->kind == ASkFree);
      if (news->kind == ASkMText) {
         MODIFY_PRI(directives[*ndirectives], asegs_tnew, ixnew, True);
         (*ndirectives)++;
         aspacem_assert(*ndirectives <= 2 * VG_N_ASEGMENTS);
      }
      ixnew++;
   }
   goto both_done;

  finishup_old:
   news = NULL;
   aspacem_assert(ixnew == asegs_tnew.used);
   aspacem_assert(ixold < asegs_told.used);
   while (ixold < asegs_told.used) {
      olds = &asegs_told.seg[ixold];
      aspacem_assert(olds->kind == ASkMText || olds->kind == ASkMData
                     || olds->kind == ASkFree);
      if (olds->kind == ASkMText) {
         MODIFY_PRI(directives[*ndirectives], asegs_told, ixold, False);
         (*ndirectives)++;
         aspacem_assert(*ndirectives <= 2 * VG_N_ASEGMENTS);
      }
      ixold++;
   }
   goto both_done;

  both_done:
   aspacem_assert(ixold == asegs_told.used);
   aspacem_assert(ixnew == asegs_tnew.used);

   asegs_tnew.used = 0;
   asegs_told.used = 0;

   aspacem_assert( sane_AixSegments(&asegs_pri) );

#  undef MODIFY_PRI
}


/* Set the initial stack segment.  Contains kludgery.  Also take the
   opportunity to create fake segs for the millicode areas. */

void VG_(am_aix5_set_initial_client_sp)( Addr sp )
{
   static Bool done = False;
   AixSegment  seg;
   Word n_fake_stack_pages;
   Word m1 = 1048576;

   aspacem_assert(!done);
   done = True;

   /* We are given the initial client SP (that of the root thread).
      Already on the stack are argv and env.  How far up does it
      extend?  We assume to the next 64k boundary.  How far down does
      it extend?  We assume N_FAKE_STACK_PAGES small pages - by
      default 16M.  Establish those limits and add an AnonC rwx
      segment. */

   /* The 64k boundary is "justified" as follows.  On 32-bit AIX 5.3,
      a typical initial SP is 0x2FF22xxx, but the accessible (rw) area
      beyond that extends up to 0x2FF2FFFF - the next 64k boundary.
      In 64-bit mode, a typical initial SP might be
      0xFFF'FFFF'FFFF'E920, and the accessible area extends to
      0xFFF'FFFF'FFFF'FFFF.  So in both cases, (64k roundup of sp) - 1
      gives the end of the accessible area. */
   VG_(debugLog)(1,"aspacem", "aix5_set_initial_client_sp( %p )\n",
                   (void*)sp);

   init_AixSegment( &seg );
   seg.kind  = ASkAnonC;
   seg.hasR  = seg.hasW = seg.hasX = True;

   if (sizeof(void*) == 4
       && ((sp & 0xFFFF0000) == 0x2FF20000
           || (sp & 0xFFFF0000) == 0x2FF10000)) {
      /* Gaaah.  Special-case 32-bit mode. */
      seg.end = 0x2FF2FFFF;
   } else {
      seg.end = AM_64K_ROUNDUP(sp) - 1;
   }

   n_fake_stack_pages = N_FAKE_STACK_PAGES_MIN;
   if (VG_(clo_main_stacksize) > 0 
       && ((m1+VG_(clo_main_stacksize)) / VKI_PAGE_SIZE) > n_fake_stack_pages) {
      n_fake_stack_pages = (m1+VG_(clo_main_stacksize)) / VKI_PAGE_SIZE;
   }
   if (n_fake_stack_pages > N_FAKE_STACK_PAGES_MAX) {
      /* Allocation of the stack failed.  We have to stop. */
      VG_(debugLog)(
         0, "aspacem",
            "valgrind: "
            "I failed to allocate space for the application's stack.\n");
      VG_(debugLog)(
         0, "aspacem",
            "valgrind: "
            "This may be the result of a very large --max-stackframe=\n");
      VG_(debugLog)(
         0, "aspacem",
            "valgrind: "
            "setting.  Cannot continue.  Sorry.\n\n");
      ML_(am_exit)(0);
   }

   seg.start = seg.end+1 - n_fake_stack_pages * VKI_PAGE_SIZE;

   VG_(debugLog)(1,"aspacem", "aix5_set_initial_client_sp: stack seg:\n");
   show_AixSegment(1,0, &seg);
   add_asegment( &asegs_pri, &seg );

   init_AixSegment( &seg );
   seg.kind  = ASkAnonC;
   seg.hasR  = seg.hasX = True;
   seg.start = MAGIC_PAGES_1_BASE;
   seg.end   = MAGIC_PAGES_1_BASE + MAGIC_PAGES_1_SIZE - 1;
   VG_(debugLog)(1,"aspacem", "am_aix5_set_initial_client_sp: FAKE1 seg:\n");
   show_AixSegment(1,0, &seg);
   add_asegment( &asegs_pri, &seg );

   init_AixSegment( &seg );
   seg.kind  = ASkAnonC;
   seg.hasR  = seg.hasX = True;
   seg.start = MAGIC_PAGES_2_BASE;
   seg.end   = MAGIC_PAGES_2_BASE + MAGIC_PAGES_2_SIZE - 1;
   VG_(debugLog)(1,"aspacem", "am_aix5_set_initial_client_sp: FAKE2 seg:\n");
   show_AixSegment(1,0, &seg);
   add_asegment( &asegs_pri, &seg );
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Getting segment-starts.                                   ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Print out the segment array (debugging only!). */
void VG_(am_show_nsegments) ( Int logLevel, HChar* who )
{
   show_AixSegments( logLevel, who, &asegs_pri );
}

/* Get the filename corresponding to this segment, if known and if it
   has one.  The returned name's storage cannot be assumed to be
   persistent, so the caller should immediately copy the name
   elsewhere.  On AIX5, we don't know what this is (in general)
   so just return NULL. */
HChar* VG_(am_get_filename)( NSegment const* seg )
{
   return NULL;
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
   for (i = 0; i < asegs_pri.used; i++) {
      if (asegs_pri.seg[i].kind == ASkFree
          || asegs_pri.seg[i].kind == ASkPreAlloc)
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
   for (i = 0; i < asegs_pri.used; i++) {
      if (asegs_pri.seg[i].kind == ASkFree
          || asegs_pri.seg[i].kind == ASkPreAlloc)
         continue;
      starts[j++] = asegs_pri.seg[i].start;
   }

   aspacem_assert(j == nSegs); /* this should not fail */
   return nSegs;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Sanity checking and preening of the segment array.        ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

Bool VG_(am_do_sync_check) ( const HChar* fn, 
                             const HChar* file, Int line )
{
   /* There's nothing we can do here; just return a dummy value. */
   return False; /* placate gcc */
}

/* Hook to allow sanity checks to be done from aspacemgr-common.c. */
void ML_(am_do_sanity_check)( void )
{
   Bool ok = sane_AixSegments( &asegs_pri );
   aspacem_assert(ok);
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Finding segments.                                         ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Finds the segment containing 'a'.  Only returns file/anon/resvn
   segments.  On AIX5 this is pretty bogus; we fake up an entry as
   best we can by snooping round for useful information in
   asegs_pri. */

NSegment const* VG_(am_find_nsegment) ( Addr a )
{
   Int             i;
   AixSegment*     aseg;
   static NSegment bogus;

   /* Fill in default info. */
   bogus.kind   = SkAnonC;
   bogus.start  = 0;
   bogus.end    = 0;
   bogus.smode  = SmFixed;
   bogus.dev    = 0;
   bogus.ino    = 0;
   bogus.mode   = 0;
   bogus.offset = 0;
   bogus.fnIdx  = -1;
   bogus.hasR   = bogus.hasW = bogus.hasX = False;
   bogus.hasT   = False;
   bogus.isCH   = False;
   bogus.mark   = False;

   /* Go look for it in the segment table. */
   i = find_asegment_idx( &asegs_pri, a );
   aspacem_assert(i >= 0 && i <= asegs_pri.used);

   aseg = &asegs_pri.seg[i];
   if (aseg->kind == ASkFree || aseg->kind == ASkPreAlloc)
      return NULL;

   bogus.start  = aseg->start;
   bogus.end    = aseg->end;

   /* Refine */
   switch (aseg->kind) {
      case ASkMText:
         bogus.kind = SkAnonC; /* hmm, pretty darn bogus */
         bogus.hasR = bogus.hasX = True;
         break;
      case ASkMData:
         bogus.kind = SkAnonC; /* hmm, pretty darn bogus */
         bogus.hasR = bogus.hasW = True;
         break;
      case ASkShmemC:
         bogus.kind = SkShmC;
         bogus.hasR = aseg->hasR;
         bogus.hasW = aseg->hasW;
         bogus.hasX = aseg->hasX;
         break;
      case ASkAnonC:
         bogus.kind = SkAnonC;
         bogus.hasR = aseg->hasR;
         bogus.hasW = aseg->hasW;
         bogus.hasX = aseg->hasX;
         bogus.isCH = aseg->isCH;
         break;
      case ASkAnonV:
         bogus.kind = SkAnonV;
         bogus.hasR = aseg->hasR;
         bogus.hasW = aseg->hasW;
         bogus.hasX = aseg->hasX;
         break;
      case ASkFileV:
         bogus.kind = SkFileV;
         bogus.hasR = aseg->hasR;
         bogus.hasW = aseg->hasW;
         bogus.hasX = aseg->hasX;
         bogus.offset = aseg->offset;
         break;
      default:
         aspacem_assert(0);
   }

   return &bogus;
}


/* Find the next segment along from 'here', if it is a file/anon/resvn
   segment. */
NSegment const* VG_(am_next_nsegment) ( NSegment* here, Bool fwds )
{
   ML_(am_barf)("unimplemented: VG_(am_next_nsegment)");
   return NULL; /* placate gcc */
}


/* Trivial fn: return the total amount of space in anonymous mappings,
   both for V and the client.  Is used for printing stats in
   out-of-memory messages. */
ULong VG_(am_get_anonsize_total)( void )
{
   Int   i;
   ULong total = 0;
   for (i = 0; i < asegs_pri.used; i++) {
      if (asegs_pri.seg[i].kind == ASkAnonC 
          || asegs_pri.seg[i].kind == ASkAnonV) {
         total += (ULong)asegs_pri.seg[i].end
                  - (ULong)asegs_pri.seg[i].start + 1ULL;
      }
   }
   return total;
}


/* Test if a piece of memory is addressable by the client with at
   least the "prot" protection permissions by examining the underlying
   segments. */
Bool VG_(am_is_valid_for_client)( Addr start, SizeT len, 
                                  UInt prot )
{
   NSegment const * const fake = VG_(am_find_nsegment)(start);
   if (!fake)
      return False;
   aspacem_assert(fake->start <= start);
   aspacem_assert(start + len - 1 <= fake->end);
   if (fake->kind == SkAnonV || fake->kind == SkFileV)
      return False;
   if ((prot & VKI_PROT_READ) && !fake->hasR)
      return False;
   if ((prot & VKI_PROT_WRITE) && !fake->hasW)
      return False;
   if ((prot & VKI_PROT_EXEC) && !fake->hasX)
      return False;
   return True;
}

/* Variant of VG_(am_is_valid_for_client) which allows free areas to
   be considered part of the client's addressable space.  It also
   considers reservations to be allowable, since from the client's
   point of view they don't exist. */
Bool VG_(am_is_valid_for_client_or_free_or_resvn)
   ( Addr start, SizeT len, UInt prot )
{
   ML_(am_barf)("unimplemented: "
                "VG_(am_is_valid_for_client_or_free_or_resvn)");
   /*NOTREACHED*/
   return False;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Startup, including reading /proc/self/maps.               ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Initialise the address space manager, setting up the initial
   segment list, and reading /proc/self/maps into it.  This must
   be called before any other function.

   Takes a pointer to the SP at the time V gained control.  This is
   taken to be the highest usable address (more or less).  Based on
   that (and general consultation of tea leaves, etc) return a
   suggested end address for the client's stack. */

Addr VG_(am_startup) ( Addr sp_at_startup )
{
   aspacem_assert(sizeof(Word)   == sizeof(void*));
   aspacem_assert(sizeof(Addr)   == sizeof(void*));
   aspacem_assert(sizeof(SizeT)  == sizeof(void*));
   aspacem_assert(sizeof(SSizeT) == sizeof(void*));

   asegs_tnew.used = 0;
   asegs_told.used = 0;

   asegs_pri.used  = 1;
   init_AixSegments( &asegs_pri );
   aspacem_assert( sane_AixSegments(&asegs_pri) );

   if (0)
      VG_(am_show_nsegments)(0,"AFTER VG_(am_startup)");

   /* We do not make an initial read of /proc/../map since doing so
      would leave us without a way to communicate the results to a
      caller.  Hence we expect that the caller (m_main) will call
      VG_(am_aix5_reread_procmap) soon after this call so as to get
      the initial code/data segments recorded. */

   /* Return value is irrelevant since we don't lay out the
      client's stack; it is already done. */
   return 0; 
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Preallocation (acquiring space from sbrk).                ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

static
SysRes local_do_sbrk_NO_NOTIFY( Word delta )
{
   SysRes res;
   aspacem_assert(__NR_AIX5_sbrk != __NR_AIX5_UNKNOWN);
   res = VG_(do_syscall1)(__NR_AIX5_sbrk, (UWord)delta);
   /* kernel produces (-1, VKI_ENOMEM) on failure.  I think that's
      ok. */
   return res;
}


/* Find the ix of a prealloc section containing at least req_sz bytes,
   or -1 if not found.  Uses best-fit. */

static Int find_prealloc_idx ( SizeT req_sz )
{
   SizeT best_sz, this_sz;
   Int   best_ix, i;
   aspacem_assert(sizeof(SizeT) == sizeof(Addr));
   aspacem_assert(req_sz > 0);
   aspacem_assert(AM_IS_4K_ALIGNED(req_sz));

   best_sz = Addr_MAX;
   best_ix = -1;

   for (i = 0; i < asegs_pri.used; i++) {
      AixSegment* s = &asegs_pri.seg[i];
      if (s->kind != ASkPreAlloc)
         continue;
      this_sz
        = s->end + 1 - s->start;
      aspacem_assert(this_sz > 0);
      aspacem_assert(AM_IS_4K_ALIGNED(this_sz));
      if (this_sz >= req_sz && this_sz < best_sz) {
         best_sz = this_sz;
         best_ix = i;
      }
   }

   return best_ix;
}


/* Create a new prealloc section containing req_sz bytes.  Returns
   False if failed, True on success. */

static Bool new_prealloc ( SizeT req_sz )
{
   SysRes     sres;
   AixSegment seg;
   Addr       start;
   SSizeT     delta;
   HChar*     why = NULL;

   aspacem_assert(req_sz > 0);
   aspacem_assert(AM_IS_4K_ALIGNED(req_sz));

   /* m_syswrap may have decided that it's not currently safe to allow
      allocations from sbrk-world.  If so, we have to fail. */
   if (0 && !VG_(am_aix5_sbrk_allowed)) {
      why = "sbrk disallowed";
      goto fail;
   }

   /* Get the current limit. */
   sres = local_do_sbrk_NO_NOTIFY(0);
   if (sres.isError) {
      why = "initial sbrk failed";
      goto fail;
   }

   /* Get it page aligned */
   delta = AM_4K_ROUNDUP(sres.res) - sres.res;
   aspacem_assert(delta >= 0 && delta < AM_4K_PAGESZ);
   if (delta > 0) {
      sres = local_do_sbrk_NO_NOTIFY(delta);
      if (sres.isError) {
         why = "aligning sbrk failed";
         goto fail;
      }
   }

   /* Now the brk is aligned.  Try to acquire the block. */
   sres = local_do_sbrk_NO_NOTIFY(0);
   if (sres.isError)
      return False;
   start = sres.res;
   aspacem_assert( AM_IS_4K_ALIGNED( start ));

   sres = local_do_sbrk_NO_NOTIFY( req_sz );
   if (sres.isError) {
      why = "main sbrk failed";
      goto fail;
   }

   /* If this fails, the kernel is acting strange. */
   aspacem_assert( sres.res == start );

   init_AixSegment( &seg );
   seg.start = start;
   seg.end   = start + req_sz - 1;
   seg.kind  = ASkPreAlloc;
   seg.hasR  = seg.hasW = seg.hasX = True; /* presumably */
   add_asegment( &asegs_pri, &seg );

   VG_(debugLog)(
      1, "aspacem", "new_prealloc: SUCCESS at 0x%llx size %lld\n", 
         (ULong)start, (ULong)req_sz
   );
   return True;

  fail:
   VG_(debugLog)(1, "aspacem", "new_prealloc: FAILED: %s\n", why);
   return False;
}


/* Find the ix of a prealloc section capable of holding a block of
   size req_sz.  If none exists, try to create one first.  Returns -1
   on failure. */

static Int find_or_create_prealloc_idx ( SizeT req_sz )
{
   Int   ix;
   SizeT req_szX;
   Bool  alloc_ok;

   if (0)
      VG_(debugLog)(0, "zz", " find_or_create_prealloc_idx ( %lu )\n", 
                       req_sz);

   aspacem_assert(sizeof(SizeT) == sizeof(Addr));
   aspacem_assert(req_sz > 0);
   aspacem_assert(AM_IS_4K_ALIGNED(req_sz));

   ix = find_prealloc_idx ( req_sz );
   if (ix >= 0 && ix < asegs_pri.used)
      return ix;

   /* Not found.  We'll have to allocate one.  Allocate some extra at
      the same time, so as to give a reservoir from which to satisfy
      future requests. */
   aspacem_assert(ix == -1);

   req_szX = req_sz + AM_PREALLOC_EXTRA;
   aspacem_assert(req_szX > 0);
   aspacem_assert(AM_IS_4K_ALIGNED(req_szX));

   alloc_ok = new_prealloc( req_szX );
   if (!alloc_ok)
      return -1; /* failed */

   /* We should now be able to find it in the segment table. */
   ix = find_prealloc_idx( req_sz );
   aspacem_assert(ix >= 0 && ix < asegs_pri.used);
   return ix;
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
   ML_(am_barf)("unimplemented: VG_(am_get_advisory)");
   /*NOTREACHED*/
   return 0; /* placate gcc -Wall */
}


/* Convenience wrapper for VG_(am_get_advisory) for client floating or
   fixed requests.  If start is zero, a floating request is issued; if
   nonzero, a fixed request at that address is issued.  Same comments
   about return values apply. */

Addr VG_(am_get_advisory_client_simple) ( Addr start, SizeT len, 
                                          /*OUT*/Bool* ok )
{
   ML_(am_barf)("unimplemented: VG_(am_get_advisory_client_simple)");
   /*NOTREACHED*/
   return 0; /* placate gcc -Wall */
}


/* Notifies aspacem that the client completed an mmap successfully.
   The segment array is updated accordingly.  If the returned Bool is
   True, the caller should immediately discard translations from the
   specified address range. */

Bool
VG_(am_notify_client_mmap)( Addr a, SizeT len, UInt prot, UInt flags,
                            Int fd, Off64T offset )
{
   AixSegment seg;
   Bool       needDiscard;

   if (len == 0)
      return False;

   /* Discard is needed if any of the just-trashed range had T. */
   needDiscard = True; /* conservative but safe */

   init_AixSegment( &seg );
   seg.kind   = ASkAnonC; /* XXX bogus: could be a file */
   seg.start  = a;
   seg.end    = a + len - 1;
   seg.hasR   = toBool(prot & VKI_PROT_READ);
   seg.hasW   = toBool(prot & VKI_PROT_WRITE);
   seg.hasX   = toBool(prot & VKI_PROT_EXEC);

   if (0)
   VG_(debugLog)(0,"aspacem","notify mmap ( %p, %ld, %ld, %ld )\n", 
                             (void*)a, len, (UWord)prot, (UWord)flags);

   add_asegment( &asegs_pri, &seg );
   AM_SANITY_CHECK("am_notify_client_mmap");
   return needDiscard;
}


/* Notifies aspacem that the client completed a shmat successfully.
   The segment array is updated accordingly.  If the returned Bool is
   True, the caller should immediately discard translations from the
   specified address range. */

Bool
VG_(am_notify_client_shmat)( Addr a, SizeT len, UInt prot )
{
   AixSegment seg;
   init_AixSegment( &seg );
   seg.kind  = ASkShmemC;
   seg.start = a;
   seg.end   = seg.start + len - 1;
   seg.hasR  = (prot & VKI_PROT_READ)  ? True : False;
   seg.hasW  = (prot & VKI_PROT_WRITE) ? True : False;
   seg.hasX  = (prot & VKI_PROT_EXEC)  ? True : False;
   add_asegment( &asegs_pri, &seg );
   AM_SANITY_CHECK("am_notify_client_shmat");
   if (0) VG_(am_show_nsegments)(0, "after shmat");
   return True; /* be paranoid */
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

   if (len == 0)
      return False;

   newR = toBool(prot & VKI_PROT_READ);
   newW = toBool(prot & VKI_PROT_WRITE);
   newX = toBool(prot & VKI_PROT_EXEC);

   /* Discard is needed if we're dumping X permission */
   needDiscard = True; /* conservative but correct */

   split_asegments_lo_and_hi( &asegs_pri, start, start+len-1, &iLo, &iHi );

   iLo = find_asegment_idx(&asegs_pri, start);
   iHi = find_asegment_idx(&asegs_pri, start + len - 1);

   for (i = iLo; i <= iHi; i++) {
      aspacem_assert(i >= 0 && i < asegs_pri.used);
      /* Apply the permissions to all relevant segments. */
      if (asegs_pri.seg[i].kind != ASkFree) {
         asegs_pri.seg[i].hasR = newR;
         asegs_pri.seg[i].hasW = newW;
         asegs_pri.seg[i].hasX = newX;
         aspacem_assert(sane_AixSegment(&asegs_pri.seg[i]));
      }
   }
   if (0)
   VG_(debugLog)(0,"aspacem","notify mprotect ( %p, %ld, %ld )\n", 
                             (void*)start, len, (UWord)prot);
   /* Changing permissions could have made previously un-mergable
      segments mergeable.  Therefore have to re-preen them. */
   preen_asegments(&asegs_pri);
   AM_SANITY_CHECK("am_notify_mprotect");
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
   Bool       needDiscard = True; /* conservative but safe */
   AixSegment seg;

   if (len == 0)
      return False;

   init_AixSegment( &seg );
   seg.kind  = ASkFree;
   seg.start = start;
   seg.end   = start + len - 1;
   add_asegment( &asegs_pri, &seg );
   AM_SANITY_CHECK("am_notify_munmap");

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
   SysRes r = {0,0};
   ML_(am_barf)("unimplemented: VG_(am_mmap_file_fixed_client)");
   /*NOTREACHED*/
   return r;
}


/* Map anonymously at a fixed address for the client, and update
   the segment array accordingly. */

SysRes VG_(am_mmap_anon_fixed_client) ( Addr start, SizeT length, UInt prot )
{
   SysRes r = {0,0};
   ML_(am_barf)("unimplemented: VG_(am_mmap_anon_fixed_client)");
   /*NOTREACHED*/
   return r;
}


/* Map anonymously at an unconstrained address for the client, and
   update the segment array accordingly.  */

SysRes VG_(am_mmap_anon_float_client) ( SizeT length, Int prot )
{
   SysRes     sres;
   AixSegment seg;

   /* Not allowable. */
   if (length == 0)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* AIX seems to demand fd == -1 in anonymous mappings. hence: */
   sres = VG_(am_do_mmap_NO_NOTIFY)(
             0, length,
             prot,
             VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS,
             -1, 0
          );

   if (!sres.isError) {
      init_AixSegment( &seg );
      seg.kind  = ASkAnonC;
      seg.start = sres.res;
      seg.end   = seg.start + length - 1;
      seg.hasR  = toBool((prot & VKI_PROT_READ) > 0);
      seg.hasW  = toBool((prot & VKI_PROT_WRITE) > 0);
      seg.hasX  = toBool((prot & VKI_PROT_EXEC) > 0);
      seg.fromP = False;
      add_asegment( &asegs_pri, &seg );
      VG_(debugLog)(2, "aspacem", "new AnonC from mmap, size %lu\n", 
                       length );
   }

   return sres;
}


/* Similarly, acquire new address space for the client but with
   considerable restrictions on what can be done with it: (1) the
   actual protections may exceed those stated in 'prot', (2) the
   area's protections cannot be later changed using any form of
   mprotect, and (3) the area cannot be freed using any form of
   munmap.  On Linux this behaves the same as
   VG_(am_mmap_anon_float_client).  On AIX5 this *may* allocate memory
   by using sbrk, so as to make use of large pages on AIX. */

SysRes VG_(am_sbrk_anon_float_client) ( SizeT length, Int prot )
{
   Int        ix;
   SysRes     sres;
   AixSegment seg;
   SizeT      lenX = AM_4K_ROUNDUP(length);

   /* Not allowable. */
   if (length == 0)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* First see if we can get space from sbrk-world. */
   ix = find_or_create_prealloc_idx ( lenX );
   if (ix >= 0 && ix < asegs_pri.used) {
      init_AixSegment( &seg );
      seg.kind  = ASkAnonC;
      seg.start = asegs_pri.seg[ix].start;
      seg.end   = seg.start + lenX - 1;
      seg.hasR  = toBool((prot & VKI_PROT_READ) > 0);
      seg.hasW  = toBool((prot & VKI_PROT_WRITE) > 0);
      seg.hasX  = toBool((prot & VKI_PROT_EXEC) > 0);
      seg.fromP = True;
      add_asegment( &asegs_pri, &seg );
      sres = VG_(mk_SysRes_Success)( seg.start );
      VG_(debugLog)(2, "aspacem", "new AnonC from prealloc, size %lu\n", 
                       length );
      return sres;
   }

   /* That didn't work out.  Try mmap-world instead. */
   aspacem_assert(ix == -1);
   return VG_(am_mmap_anon_float_client)( length, prot );
}


/* Map anonymously at an unconstrained address for V, and update the
   segment array accordingly.  This is fundamentally how V allocates
   itself more address space when needed. */

SysRes VG_(am_mmap_anon_float_valgrind)( SizeT length )
{
   SysRes     sres;
   AixSegment seg;

   /* Not allowable. */
   if (length == 0)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* AIX seems to demand fd == -1 in anonymous mappings. hence: */
   sres = VG_(am_do_mmap_NO_NOTIFY)(
             0, length,
             VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC,
             VKI_MAP_PRIVATE|VKI_MAP_ANONYMOUS,
             -1, 0
          );

   if (!sres.isError) {
      init_AixSegment( &seg );
      seg.kind  = ASkAnonV;
      seg.start = sres.res;
      seg.end   = seg.start + length - 1;
      seg.hasR  = seg.hasW = seg.hasX = True;
      seg.fromP = False;
      add_asegment( &asegs_pri, &seg );
      VG_(debugLog)(2, "aspacem", "new AnonV from mmap, size %lu\n", 
                       length );
   }

   return sres;
}


/* Same comments apply as per VG_(am_sbrk_anon_float_client).  On
   Linux this behaves the same as VG_(am_mmap_anon_float_valgrind). */
SysRes VG_(am_sbrk_anon_float_valgrind)( SizeT length )
{
   Int        ix;
   SysRes     sres;
   AixSegment seg;
   SizeT      lenX = AM_4K_ROUNDUP(length);

   /* Not allowable. */
   if (length == 0)
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* First see if we can get space from sbrk-world. */
   ix = find_or_create_prealloc_idx ( lenX );
   if (ix >= 0 && ix < asegs_pri.used) {
      init_AixSegment( &seg );
      seg.kind  = ASkAnonV;
      seg.start = asegs_pri.seg[ix].start;
      seg.end   = seg.start + lenX - 1;
      seg.hasR  = True;
      seg.hasW  = True;
      seg.hasX  = True;
      seg.fromP = True;
      add_asegment( &asegs_pri, &seg );
      sres = VG_(mk_SysRes_Success)( seg.start );
      VG_(debugLog)(2, "aspacem", "new AnonV from prealloc, size %lu\n", 
                       length );
      return sres;
   }

   /* That didn't work out.  Try mmap-world instead. */
   aspacem_assert(ix == -1);
   return VG_(am_mmap_anon_float_valgrind)( length );
}


/* Really just a wrapper around VG_(am_sbrk_anon_float_valgrind). */

void* VG_(am_shadow_alloc)(SizeT size)
{
   SysRes sres = VG_(am_sbrk_anon_float_valgrind)( size );
   return sres.isError ? NULL : (void*)sres.res;
}


/* Map a file at an unconstrained address for V, and update the
   segment array accordingly.  This is used by V for transiently
   mapping in object files to read their debug info. */

SysRes VG_(am_mmap_file_float_valgrind) ( SizeT length, UInt prot, 
                                          Int fd, Off64T offset )
{
   SysRes sres;

   /* Not allowable. */
   if (length == 0 || !VG_IS_PAGE_ALIGNED(offset))
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   sres = VG_(am_do_mmap_NO_NOTIFY)(
             0, length,
             prot, VKI_MAP_PRIVATE,
             fd, offset
          );
   if (!sres.isError) {
      AixSegment seg;
      init_AixSegment( &seg );
      seg.kind = SkFileV;
      seg.start = sres.res;
      seg.end = seg.start + length - 1;
      seg.hasR   = toBool(prot & VKI_PROT_READ);
      seg.hasW   = toBool(prot & VKI_PROT_WRITE);
      seg.hasX   = toBool(prot & VKI_PROT_EXEC);
      seg.fname  = add_to_strtab("(FileV-float, unknown name)");
      add_asegment( &asegs_pri, &seg );
      aspacem_assert( sane_AixSegments( &asegs_pri ));
   }
   return sres;
}


/* Unmap the given address range and update the segment array
   accordingly.  This fails if the range isn't valid for the client.
   If *need_discard is True after a successful return, the caller
   should immediately discard translations from the specified address
   range. */

SysRes VG_(am_munmap_client)( /*OUT*/Bool* need_discard,
                              Addr start, SizeT len )
{
   SysRes r = {0,0}; 
   ML_(am_barf)("unimplemented: VG_(am_munmap_client)");
   /*NOTREACHED*/
   return r;
}


/* Unmap the given address range and update the segment array
   accordingly.  This fails if the range isn't valid for valgrind. */
/* Also, if the specified range doesn't fall within a single segment,
   it barfs.  This simplifies the implementation; we shouldn't need to
   deal with anything but the simplest cases. */

SysRes VG_(am_munmap_valgrind)( Addr start, SizeT len )
{
   AixSegment* seg;
   AixSegment  seg2;
   Addr        end;
   SysRes      sres;
   Int         ixS, ixE;
   Bool        debug = False;

   if (debug)
      VG_(debugLog)(0,"aspacem", 
                      "am_munmap_valgrind(%p, %lu)\n", (void*)start, len);

   if (len == 0)
      return VG_(mk_SysRes_Success)(0);

   /* We have to be a bit careful here.  If the area being unmapped is
      AnonV which originated from a preallocated area (hence from
      sbrk-land) then we will have to return it to the preallocated
      state, rather than unmapping it.  */
   end = start + len - 1;
   aspacem_assert(start <= end); // else have wraparound?!

   ixS = find_asegment_idx( &asegs_pri, start );
   ixE = find_asegment_idx( &asegs_pri, end );

   aspacem_assert(ixS >= 0 && ixS < asegs_pri.used);
   aspacem_assert(ixE >= 0 && ixE < asegs_pri.used);

   /* Preconditions: See comment at start of fn */
   aspacem_assert(ixS == ixE);

   /* For the segment S denoted by ixS:

      - if S is AnonV from prealloc and S entirely within start .. end,
        return it to prealloc

      - if S is AnonV not from prealloc and S entirely within start .. end,
        munmap it

      - if S is FileV and S entirely within start .. end, munmap it

      Otherwise, leave it alone (too complex to handle).  In theory
      this could cause a leak; in practice I don't think it will.
   */
   seg = &asegs_pri.seg[ixS];

   if (debug)
      show_AixSegment( 0, ixS, seg );

   /* Invariants */
   aspacem_assert(seg->start <= start);
   aspacem_assert(end <= seg->end);

   if (seg->kind == ASkFileV
       || (seg->kind == ASkAnonV && (!seg->fromP))) {
      if (debug)
         VG_(debugLog)(0,"aspacem", "am_munmap_valgrind: !fromP: %p-%p\n",
                         (void*)start, (void*)end);
      sres = ML_(am_do_munmap_NO_NOTIFY)( start, len );
      if (sres.isError)
         goto bad;
      init_AixSegment( &seg2 );
      seg2.start = start;
      seg2.end   = end;
      seg2.kind  = ASkFree;
      add_asegment( &asegs_pri, &seg2 );
   }
   else
   if (seg->kind == ASkAnonV && seg->fromP) {
      if (debug)
         VG_(debugLog)(0,"aspacem", "am_munmap_valgrind:  fromP: %p-%p\n",
                         (void*)start, (void*)end);
      init_AixSegment( &seg2 );
      seg2.start = start;
      seg2.end   = end;
      seg2.kind  = ASkPreAlloc;
      seg2.hasR  = seg2.hasW = seg2.hasX = True;
      add_asegment( &asegs_pri, &seg2 );
   }
   else {
      /* shouldn't be asked to handle any other cases */
      aspacem_assert(0);
   }

   aspacem_assert( sane_AixSegments( &asegs_pri ));
   return VG_(mk_SysRes_Success)(0);

  bad:
   aspacem_assert( sane_AixSegments( &asegs_pri ));
   return VG_(mk_SysRes_Error)(VKI_EINVAL);
}


/* Let (start,len) denote an area within a single Valgrind-owned
  segment (anon or file).  Change the ownership of [start, start+len)
  to the client instead.  Fails if (start,len) does not denote a
  suitable segment. */

Bool VG_(am_change_ownership_v_to_c)( Addr start, SizeT len )
{
   return True;
}


/* 'seg' must be NULL or have been obtained from
   VG_(am_find_nsegment), and still valid.  If non-NULL, and if it
   denotes a SkAnonC (anonymous client mapping) area, set the .isCH
   (is-client-heap) flag for that area.  Otherwise do nothing.
   (Bizarre interface so that the same code works for both Linux and
   AIX and does not impose inefficiencies on the Linux version.) */
/* AIX: presumably this is a faked-up segment our VG_(am_find_segment)
   came up with.  So we have to find the corresponding AixSegment. */

void VG_(am_set_segment_isCH_if_SkAnonC)( NSegment* seg )
{
   Int i;
   if (seg == NULL)
      return;
   i = find_asegment_idx( &asegs_pri, seg->start );
   aspacem_assert(i >= 0 && i < asegs_pri.used );
   if (asegs_pri.seg[i].kind == ASkAnonC) {
      asegs_pri.seg[i].isCH = True;
      if (0)
         VG_(debugLog)(0,"aspacem","set isCH for %p\n", (void*)seg->start );
   } else {
      aspacem_assert(asegs_pri.seg[i].isCH == False);
   }
}


/* Same idea as VG_(am_set_segment_isCH_if_SkAnonC), except set the
   segment's hasT bit (has-cached-code) if this is SkFileC or SkAnonC
   segment. */
/* AIX: we ignore these complexities by conservatively assuming that
   all segments had translations taken from them.  Hence we can safely
   ignore this. */
void VG_(am_set_segment_hasT_if_SkFileC_or_SkAnonC)( NSegment* seg )
{
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
   ML_(am_barf)("unimplemented: VG_(am_create_reservation)");
   /*NOTREACHED*/
   return False;
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
   ML_(am_barf)("unimplemented: "
                "VG_(am_extend_into_adjacent_reservation_client)");
   /*NOTREACHED*/
   return False;
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
   ML_(am_barf)("unimplemented: VG_(am_extend_map_client)");
   /*NOTREACHED*/
   return False;
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
   ML_(am_barf)("unimplemented: VG_(am_relocate_nooverlap_client)");
   /*NOTREACHED*/
   return False;
}



/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- A simple parser for /proc/<pid>/map on AIX5.              ---*/
/*--- Almost completely independent of the stuff above.  The    ---*/
/*--- only function it 'exports' to the code above this comment ---*/
/*--- is parse_procselfmaps.                                    ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <sys/procfs.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */


/* Size of a smallish table used to read /proc/<pid>/map entries. */
#define M_APROCMAP_BUF 100000

/* static ... to keep it out of the stack frame. */
static HChar procmap_buf[M_APROCMAP_BUF];

/* Records length of /proc/<pid>/map read into procmap_buf. */
static Int buf_n_tot;

/* Helper fns. */

/* Get the contents of /proc/<pid>/map into a static buffer.  If
   there's a syntax error, it won't fit, or other failure, just
   abort. */

static void read_procselfmap_into_buf ( void )
{
   Char   fname[50];
   Int    n_chunk;
   SysRes fd;

   ML_(am_sprintf)( fname, "/proc/%d/map", ML_(am_getpid)() );

   /* Read the initial memory mapping from the /proc filesystem. */
   fd = ML_(am_open)( fname, VKI_O_RDONLY, 0 );
   if (fd.isError)
      ML_(am_barf)("can't open /proc/<pid>/map");

   buf_n_tot = 0;
   do {
      n_chunk = ML_(am_read)( fd.res, &procmap_buf[buf_n_tot],
                              M_APROCMAP_BUF - buf_n_tot );
      buf_n_tot += n_chunk;
   } while ( n_chunk > 0 && buf_n_tot < M_APROCMAP_BUF );

   ML_(am_close)(fd.res);

   if (buf_n_tot >= M_APROCMAP_BUF-5)
      ML_(am_barf_toolow)("M_APROCMAP_BUF");
   if (buf_n_tot == 0)
      ML_(am_barf)("I/O error on /proc/<pid>/map");

   procmap_buf[buf_n_tot] = 0;
}


/* /proc/<pid>/map appears to give out a non-absolute path name for
   the main executable.  Fortunately we can reliably identify the main
   executable via the MA_MAINEXEC bit, and if we find the path is
   non-absolute, replace it with /proc/<pid>/object/a.out instead.
   AIX guarantees the latter is another name for the main
   executable. */

static HChar* kludge_exe_file_name ( HChar* file_name, prmap_t* map )
{
   static Int   my_pid = -1;
   static HChar a_out_name[64];
   if (file_name == NULL)
      return NULL;
   if (file_name[0] != '/' && (map->pr_mflags & MA_MAINEXEC)) {
      if (my_pid == -1)
         my_pid = ML_(am_getpid)();
      ML_(am_sprintf)(a_out_name, "/proc/%d/object/a.out", my_pid);
      file_name = a_out_name;
   }
   return file_name;
}



/* Parse /proc/<pid>/map, copying the entries in it into an
   AixSegments structure.  Returns a properly formed AixSegments, with
   ASkMText/ASkMData entries, with sibling pointers set up, and
   ASkFree everywhere else.
*/
static void parse_procselfmap ( /*OUT*/AixSegments* segs )
{
   UChar      rr, ww, xx, mm, ss;
   prmap_t*   map;
   UChar*     file_name;
   UChar*     member_name;
   Bool       show_map;
   Int        off, i, j;
   AixSegment s;

   const UInt valid_pr_mflags
      = MA_MAINEXEC | MA_KERNTEXT | MA_READ | MA_WRITE 
        | MA_EXEC | MA_SHARED | MA_BREAK | MA_STACK;

   segs->used = 1;
   init_AixSegments(segs);
   aspacem_assert( sane_AixSegments(segs) );

   read_procselfmap_into_buf();

   if (0)
      VG_(debugLog)(0, "procselfmaps", "got %d bytes\n", buf_n_tot);

   off = 0;
   while (True) {

      /* stay sane .. */
      if (off + sizeof(prmap_t) > buf_n_tot)
         break;
 
      map = (prmap_t*)&procmap_buf[off];
      off += sizeof(prmap_t);

      /* When should we stop reading the array?
         /usr/include/sys/procfs.h says that "Array entries continue
         until an entry with a pr_size field of 0 and invalid
         pr_mflags occurs."  It unhelpfully fails to define what
         "invalid" means here.  However, the following test _seems_ to
         work. */
      if (map->pr_size == 0 
          && (map->pr_mflags & valid_pr_mflags) == 0)
         break;

      /* Ok, keep going, but ignore any zero-sized mappings: */
      if (map->pr_size == 0)
         continue;

      mm = (map->pr_mflags & MA_MAINEXEC) > 0;
      rr = (map->pr_mflags & MA_READ) > 0;
      ww = (map->pr_mflags & MA_WRITE) > 0;
      xx = (map->pr_mflags & MA_EXEC) > 0;
      ss = (map->pr_mflags & MA_SHARED) > 0;

      if (map->pr_pathoff > 0) {
         file_name   = &procmap_buf[map->pr_pathoff];
         member_name = file_name + VG_(strlen)(file_name) + 1;
         if (*member_name == 0)
            member_name = NULL;
      } else {
         file_name = member_name = NULL;
      }
      file_name = kludge_exe_file_name( file_name, map );

      /* Now file_name and member_name are NULL or ordinary strings.
         Convert them to string-table resident strings. */
      if (file_name)
         file_name = add_to_strtab(file_name);
      if (member_name)
         member_name = add_to_strtab(member_name);

      /* Create a suitable kind of segment.  Initially we will start
         with bogus sibling pointers, and allow ASkMData entries to
         have file names, since we cannot assume anything about the
         ordering of entries in the procmap file.  In a second pass,
         we will set up the sibling pointers based on those file
         names, then remove the MData file names. */
      init_AixSegment(&s);
      show_map = False;
      if (rr && (!ww) && xx) {
         if (map->pr_size > 0) {
            /* r-x segment; add bounds for a text area. */
            s.kind    = ASkMText;
            s.start   = (Addr)map->pr_vaddr;
            s.end     = (Addr)map->pr_vaddr + (Addr)map->pr_size - 1;
            s.isMainExe = mm;
            s.sibling = 0;
            s.fname   = file_name;
            s.mname   = member_name;
            s.hasR = rr;
            s.hasW = ww;
            s.hasX = xx;
            add_asegment(segs, &s);
         }
      }
      else
      if (rr && ww && (!xx)) {
         if (map->pr_size > 0) {
            /* rw- segment; add bounds for a data area. */
            s.kind    = ASkMData;
            s.start   = (Addr)map->pr_vaddr;
            s.end     = (Addr)map->pr_vaddr + (Addr)map->pr_size - 1;
            /* Set a bogus non-zero sibling pointer, since sanity
               checking will reject zero sibling pointers on MData.
               It doesn't matter since the loops following this one
               below fix up the sibling pointers. */
            s.sibling = 1;
            s.fname   = file_name;
            s.mname   = member_name;
            s.hasR = rr;
            s.hasW = ww;
            s.hasX = xx;
            add_asegment(segs, &s);
         }
      }
      else {
         /* unclassifiable; we better complain. */
         show_map = True;
         VG_(debugLog)(0, "aspacem", "parse_procselfmap: unclassifiable:\n");
      }

      if (show_map)
         VG_(debugLog)(1,"aspacem",
                       "  %010llx-%010llx %c%c%c%c%c %s%s%s%s\n",
                       (ULong)map->pr_vaddr,
                       (ULong)map->pr_vaddr + (ULong)map->pr_size,
                       mm ? 'M' : '-',
                       rr ? 'r' : '-',
                       ww ? 'w' : '-',
                       xx ? 'x' : '-',
                       ss ? 'S' : '-',
                       file_name ? file_name : (UChar*)"(none)",
                       member_name ? "(" : "",
                       member_name ? member_name : (UChar*)"",
                       member_name ? ")" : ""
         );

   }

   /* Set up sibling pointers.  For each MData, find an MText with the
      same file/member names, or complain.  This is really ugly in
      that it makes the process quadratic in the number of modules
      mapped in, but I can't think of a (simple) better way.  */

   for (i = 0; i < segs->used; i++) {
      if (segs->seg[i].kind != ASkMData)
         continue;
      for (j = 0; j < segs->used; j++) {
         if (segs->seg[j].kind == ASkMText 
             && segs->seg[j].fname == segs->seg[i].fname
             && segs->seg[j].mname == segs->seg[i].mname)
            break;
      }
      if (j == segs->used) {
         VG_(debugLog)(0, "aspacem", "parse_procselfmap: "
            "data segment with no associated text segment:\n");
         VG_(debugLog)(0, "aspacem", "module = %s(%s)\n",
                          segs->seg[i].fname,
                          segs->seg[i].mname ? segs->seg[i].mname 
                                             : (UChar*)"(none)");
         aspacem_assert(0);
      }
      aspacem_assert(j >= 0 && j < segs->used && j != i);
      segs->seg[i].sibling = segs->seg[j].start;
   }

   /* (Almost) dually, for each MText, find an MData with same
      file/member names, but don't complain if not present. */

   for (i = 0; i < segs->used; i++) {
      if (segs->seg[i].kind != ASkMText)
         continue;
      for (j = 0; j < segs->used; j++) {
         if (segs->seg[j].kind == ASkMData 
             && segs->seg[j].fname == segs->seg[i].fname
             && segs->seg[j].mname == segs->seg[i].mname)
            break;
      }
      if (j == segs->used) {
         /* no corresponding MData found; harmless. */
      } else {
         aspacem_assert(j >= 0 && j < segs->used && j != i);
         segs->seg[i].sibling = segs->seg[j].start;
      }
   }

   /* Finally, get rid of fname/mname pointers on MDatas, so as to
      adhere to the necessary representational invariants. */
   for (i = 0; i < segs->used; i++) {
      if (segs->seg[i].kind == ASkMData){
         segs->seg[i].fname = segs->seg[i].mname = NULL;
      }
   }

   aspacem_assert( sane_AixSegments(segs) );
   if (0)
      show_AixSegments(0, "as read from procmap", segs);
}

#endif // defined(VGO_aix5)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
