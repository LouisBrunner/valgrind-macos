
/*--------------------------------------------------------------------*/
/*--- The address space manager: segment initialisation and        ---*/
/*--- tracking, stack operations                                   ---*/
/*---                                                  aspacemgr.c ---*/
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

#include "core.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_options.h"
#include "pub_core_syscalls.h"
#include "pub_core_tooliface.h"
#include "pub_core_transtab.h"
#include "vki_unistd.h"


/* Define to debug the memory-leak-detector. */
/* #define VG_DEBUG_LEAKCHECK */

static const Bool mem_debug = False;

/*--------------------------------------------------------------*/
/*--- Basic globals about the address space.                 ---*/
/*--------------------------------------------------------------*/

/* Client address space, lowest to highest (see top of ume.c) */
Addr VG_(client_base);           /* client address space limits */
Addr VG_(client_end);
Addr VG_(client_mapbase);
Addr VG_(client_trampoline_code);
Addr VG_(clstk_base);
Addr VG_(clstk_end);

Addr VG_(brk_base);	         /* start of brk */
Addr VG_(brk_limit);	         /* current brk */

Addr VG_(shadow_base);	         /* tool's shadow memory */
Addr VG_(shadow_end);

Addr VG_(valgrind_base);	 /* valgrind's address range */

// Note that VG_(valgrind_last) names the last byte of the section, whereas
// the VG_(*_end) vars name the byte one past the end of the section.
Addr VG_(valgrind_last);

/*--------------------------------------------------------------*/
/*--- A simple, self-contained ordered array of segments.    ---*/
/*--------------------------------------------------------------*/

/* Max number of segments we can track. */
#define VG_N_SEGMENTS 1000

/* Max number of segment file names we can track. */
#define VG_N_SEGNAMES 200

/* Max length of a segment file name. */
#define VG_MAX_SEGNAMELEN 1000


/* ------ STATE for the address-space manager ------ */

/* Array [0 .. segments_used-1] of all mappings. */
/* Sorted by .addr field. */
/* I: len may not be zero. */
/* I: overlapping segments are not allowed. */
/* Each segment can optionally hold an index into the filename table. */

static Segment segments[VG_N_SEGMENTS];
static Int     segments_used = 0;

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


/* ------ end of STATE for the address-space manager ------ */


/* Searches the filename table to find an index for the given name.
   If none is found, an index is allocated and the name stored.  If no
   space is available we just give up.  If the string is too long to
   store, return -1.
*/
static Int allocate_segname ( const HChar* name )
{
   Int i, j, len;

   vg_assert(name);

   if (0) VG_(printf)("alloc_segname %s\n", name);

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
         VG_(printf)(
            "coregrind/m_aspacemgr/aspacemgr.c:\n"
            "   VG_N_SEGNAMES is too small: "
            "increase it and rebuild Valgrind.\n"
         );
         VG_(printf)(
            "coregrind/m_aspacemgr/aspacemgr.c:\n"
            "   giving up now.\n\n"
         );
         VG_(exit)(0);
      }
   }

   /* copy it in */
   segnames[i].inUse = True;
   for (j = 0; j < len; j++)
      segnames[i].fname[j] = name[j];
   vg_assert(len < VG_MAX_SEGNAMELEN);
   segnames[i].fname[len] = 0;
   return i;
}


/* Returns -1 if 'a' denotes an address prior to seg, 1 if it denotes
   an address after it, and 0 if it denotes an address covered by
   seg. 
*/
static Int compare_addr_with_seg ( Addr a, Segment* seg )
{
   if (a < seg->addr) 
      return -1;
   if (a >= seg->addr + seg->len) 
      return 1;
   return 0;
}


/* Find the (index of the) segment that contains 'a', or -1 if
   none. 
*/
static Int find_segment ( Addr a )
{
   Int i;
   for (i = 0; i < segments_used; i++) {
      if (compare_addr_with_seg(a, &segments[i]) == 0)
         return i;
   }
   return -1;
}


/* Assumes that 'a' is not in any segment.  Finds the index of the
   lowest-addressed segment above 'a', or -1 if none.  Passing 'a'
   which is in fact in a segment is a checked error. 
*/
static Int find_segment_above_unmapped ( Addr a )
{
   Int i, r;
   for (i = 0; i < segments_used; i++) {
      r = compare_addr_with_seg(a, &segments[i]);
      vg_assert(r != 0); /* 'a' should not be in any segment. */
      if (r == 1)
         continue;
      vg_assert(r == -1);
      break;
   }

   if (i == segments_used)
      return -1; /* not found */
   else
      return i;
}


/* Assumes that 'a' is in some segment.  Finds the next segment along,
   or NULL if none.  Passing 'a' which is in fact not in a segment is
   a checked error.
*/
static Int find_segment_above_mapped ( Addr a )
{
   Int i, r;
   for (i = 0; i < segments_used; i++) {
      r = compare_addr_with_seg(a, &segments[i]);
      if (r == 1)
         continue; /* not yet there */
      if (r == 0)
         break; /* found it */
      vg_assert(0);
      /* we shouldn't get here -- r == -1 and so it means we went past 
         'a' without seeing it -- it is therefore unmapped. */
      /*NOTREACHED*/
   }

   vg_assert(i < segments_used);
   if (i == segments_used-1)
      return -1; /* not found */
   else
      return i+1;
}


/* Shift segments[i .. segments_used-1] up by one. */
static void make_space_at ( Int i )
{
   Int j;
   vg_assert(i >= 0 && i <= segments_used);
   vg_assert(segments_used >= 0);
   if (segments_used+1 == VG_N_SEGMENTS) {
      VG_(printf)(
         "coregrind/vg_memory.c:\n"
         "   VG_N_SEGMENTS is too small: "
         "increase it and rebuild Valgrind.\n"
      );
      VG_(printf)(
         "coregrind/vg_memory.c:\n"
         "   giving up now.\n\n"
      );
      VG_(exit)(0);
   }
   vg_assert(segments_used+1 < VG_N_SEGMENTS);
   for (j = segments_used; j > i; j--)
      segments[j] = segments[j-1];
   segments_used++;
}


/* Shift segments [i+1 .. segments_used-1] down by one, and decrement
   segments_used. 
*/
static void delete_segment_at ( Int i )
{
   Int j;
   vg_assert(i >= 0 && i < segments_used);
   for (j = i+1; j < segments_used; j++)
     segments[j-1] = segments[j];
   segments_used--;
   vg_assert(segments_used >= 0 && segments_used < VG_N_SEGMENTS);
}


/* Fill the i'th record all with zeroes. */
static void zeroise_segment ( Int i )
{
   vg_assert(i >= 0 && i < segments_used);
   segments[i].prot     = 0;
   segments[i].flags    = 0;
   segments[i].addr     = 0;
   segments[i].len      = 0;
   segments[i].offset   = 0;
   segments[i].filename = NULL;
   segments[i].fnIdx    = -1;
   segments[i].dev      = 0;
   segments[i].ino      = 0;
   segments[i].seginfo  = NULL;
}


/* Create a segment to contain 'a', and return its index.  Or -1 if
   this failed because some other segment already contains 'a'.  If
   successful, fill in the segment's .addr field with 'a' but leave
   all other fields alone. 
*/
static Int create_segment ( Addr a )
{
   Int i, r;
   for (i = 0; i < segments_used; i++) {
      r = compare_addr_with_seg( a, &segments[i] );
      if (r == 1)
         continue; /* seg[i] precedes a */
      if (r == 0)
         return -1; /* seg[i] contains a.  Give up */
      vg_assert(r == -1);
      break;
   }
   /* a precedes seg[i].  Shift segs at i and above up one, and use
      this slot. */
   make_space_at(i);
   zeroise_segment(i);
   segments[i].addr = a;
   return i;
}


/* Print out the segment array (debugging only!).  Note, this calls
   VG_(printf), and I'm not 100% clear that that wouldn't require
   dynamic memory allocation and hence more segments to be allocated.
*/
static void show_segments ( HChar* who )
{
   Int i;
   VG_(printf)("<<< SHOW_SEGMENTS: %s (%d segments, %d segnames)\n", 
               who, segments_used, segnames_used);
   for (i = 0; i < segnames_used; i++) {
      if (!segnames[i].inUse)
         continue;
      VG_(printf)("(%2d) %s\n", i, segnames[i].fname);
   }
   for (i = 0; i < segments_used; i++) {
      VG_(printf)(
         "%3d: %08p-%08p %7llu pr=0x%x fl=0x%04x d=0x%03x i=%-7d o=%-7lld (%d)\n",
         i,
         segments[i].addr, segments[i].addr + segments[i].len,
         (ULong)segments[i].len, segments[i].prot, 
         segments[i].flags, segments[i].dev, segments[i].ino, 
         (Long)segments[i].offset, 
         segments[i].fnIdx);
   }
   VG_(printf)(">>>\n");
}


/* Find the segment containing 'a' and split it into two pieces at
   'a'.  Does nothing if no segment contains 'a', or if the split
   would cause either of the pieces to have zero size.

   If 'a' is not found, or if no splitting happens, -1 is returned.

   If a value 'r' other than -1 is returned, this is the index of the
   higher-addressed segment resulting from the split, and the index of
   the lower-addressed segment is r-1.
*/
static Int split_segment ( Addr a )
{
   Int r;
   HWord delta;
   vg_assert(VG_IS_PAGE_ALIGNED(a));
   r = find_segment(a);
   if (r == -1)
      /* not found */
      return -1;
   if (segments[r].addr == a)
      /* segment starts at 'a', so splitting it would create a
         zero-sized segment */
      return -1;

   /* copy original; make adjustments. */
   vg_assert(a > segments[r].addr);
   delta = a - segments[r].addr;
   make_space_at(r);
   segments[r] = segments[r+1];
   segments[r].len = delta;
   segments[r+1].len -= delta;
   segments[r+1].addr += delta;
   segments[r+1].offset += delta;
   return r+1;
}


/* Return true if two segments are adjacent and mergable (s1 is
   assumed to have a lower ->addr than s2) */
static inline Bool segments_are_mergeable(Segment *s1, Segment *s2)
{
   if (s1->addr+s1->len != s2->addr)
      return False;

   if (s1->flags != s2->flags)
      return False;

   if (s1->prot != s2->prot)
      return False;

   if (s1->seginfo != s2->seginfo)
      return False;

   if (s1->flags & SF_FILE){
      if ((s1->offset + s1->len) != s2->offset)
	 return False;
      if (s1->dev != s2->dev)
	 return False;
      if (s1->ino != s2->ino)
	 return False;
      if (s1->fnIdx != s2->fnIdx)
         return False;
   }
   
   return True;
}


/* Clean up and sanity check the segment array:
   - check segments are in ascending order
   - check segments do not overlap
   - check no segment has zero size
   - merge adjacent where possible
   - perform checks on the filename table, and reclaim dead entries
*/
static void preen_segments ( void )
{
   Int i, j, rd, wr;
   Segment *s, *s1;
   vg_assert(segments_used >= 0 && segments_used < VG_N_SEGMENTS);
   vg_assert(segnames_used >= 0 && segnames_used < VG_N_SEGNAMES);

   if (0) show_segments("before preen");

   /* clear string table mark bits */
   for (i = 0; i < segnames_used; i++)
      segnames[i].mark = False;

   /* check for non-zero size, and set mark bits for any used strings */
   for (i = 0; i < segments_used; i++) {
      vg_assert(segments[i].len > 0);
      j = segments[i].fnIdx;
      vg_assert(j >= -1 && j < segnames_used);
      if (j >= 0) {
         vg_assert(segnames[j].inUse);
         segnames[j].mark = True;
      }
   }

   /* check ascendingness and non-overlap */
   for (i = 0; i < segments_used-1; i++) {
      s = &segments[i];
      s1 = &segments[i+1];
      vg_assert(s->addr < s1->addr);
      vg_assert(s->addr + s->len <= s1->addr);
   }

   /* merge */
   if (segments_used < 1)
      return;

   wr = 1;
   for (rd = 1; rd < segments_used; rd++) {
      s = &segments[wr-1];
      s1 = &segments[rd];
      if (segments_are_mergeable(s,s1)) {
         if (0)
            VG_(printf)("merge %p-%p with %p-%p\n",
                        s->addr, s->addr+s->len,
                        s1->addr, s1->addr+s1->len);
         s->len += s1->len;
         continue;
      }
      if (wr < rd)
         segments[wr] = segments[rd];
      wr++;
   }
   vg_assert(wr >= 0 && wr <= segments_used);
   segments_used = wr;

   /* Free up any strings which are no longer referenced. */
   for (i = 0; i < segnames_used; i++) {
      if (segnames[i].mark == False) {
         segnames[i].inUse = False;
         segnames[i].fname[0] = 0;
      }
   }

   if (0) show_segments("after preen");
}


/*--------------------------------------------------------------*/
/*--- Maintain an ordered list of all the client's mappings  ---*/
/*--------------------------------------------------------------*/

Bool VG_(seg_contains)(const Segment *s, Addr p, SizeT len)
{
   Addr se = s->addr+s->len;
   Addr pe = p+len;
   vg_assert(pe >= p);

   return (p >= s->addr && pe <= se);
}

Bool VG_(seg_overlaps)(const Segment *s, Addr p, SizeT len)
{
   Addr se = s->addr+s->len;
   Addr pe = p+len;
   vg_assert(pe >= p);

   return (p < se && pe > s->addr);
}

#if 0
/* 20050228: apparently unused */
/* Prepare a Segment structure for recycling by freeing everything
   hanging off it. */
static void recycleseg(Segment *s)
{
   if (s->flags & SF_CODE)
      VG_(discard_translations)(s->addr, s->len);

   if (s->filename != NULL)
      VG_(arena_free)(VG_AR_CORE, (Char *)s->filename);

   /* keep the SegInfo, if any - it probably still applies */
}

/* When freeing a Segment, also clean up every one else's ideas of
   what was going on in that range of memory */
static void freeseg(Segment *s)
{
   recycleseg(s);
   if (s->seginfo != NULL) {
      VG_(seginfo_decref)(s->seginfo, s->addr);
      s->seginfo = NULL;
   }

   VG_(SkipNode_Free)(&sk_segments, s);
}
#endif


/* Get rid of any translations arising from s. */
/* Note, this is not really the job of the low level memory manager.
   When it comes time to rewrite this subsystem, clean this up. */
static void dump_translations_from ( Segment* s )
{
   if (s->flags & SF_CODE) {
      VG_(discard_translations)(s->addr, s->len);
      if (0)
         VG_(printf)("dumping translations in %p .. %p\n",
                     s->addr, s->addr+s->len);
   }
}


/* This unmaps all the segments in the range [addr, addr+len); any
   partial mappings at the ends are truncated. */
void VG_(unmap_range)(Addr addr, SizeT len)
{
   static const Bool debug = False || mem_debug;
   Segment* s;
   Addr     end, s_end;
   Int      i;
   Bool     deleted;

   if (len == 0)
      return;

   len = VG_PGROUNDUP(len);

   if (debug)
      VG_(printf)("unmap_range(%p, %lu)\n", addr, len);
   if (0) show_segments("unmap_range(BEFORE)");
   end = addr+len;

   /* Everything must be page-aligned */
   vg_assert(VG_IS_PAGE_ALIGNED(addr));
   vg_assert(VG_IS_PAGE_ALIGNED(len));

   for (i = 0; i < segments_used; i++) {

      /* do not delete .. even though it looks stupid */
      vg_assert(i >= 0);

      deleted = False;
      s = &segments[i];
      s_end = s->addr + s->len;

      if (0 && debug)
	 VG_(printf)("unmap: addr=%p-%p s=%p ->addr=%p-%p len=%d\n",
		     addr, end, s, s->addr, s_end, s->len);

      if (!VG_(seg_overlaps)(s, addr, len)) {
	 if (0 && debug)
	    VG_(printf)("   (no overlap)\n");
	 continue;
      }

      /* 4 cases: */
      if (addr > s->addr &&
	  addr < s_end &&
	  end >= s_end) {
	 /* this segment's tail is truncated by [addr, addr+len)
	    -> truncate tail
	 */
         dump_translations_from(s);
	 s->len = addr - s->addr;

	 if (debug)
	    VG_(printf)("  case 1: s->len=%lu\n", s->len);
      } else if (addr <= s->addr && end > s->addr && end < s_end) {
	 /* this segment's head is truncated by [addr, addr+len)
	    -> truncate head
	 */
	 Word delta = end - s->addr;

	 if (debug)
	    VG_(printf)("  case 2: s->addr=%p s->len=%lu delta=%d\n", 
                        s->addr, s->len, delta);

         dump_translations_from(s);
	 s->addr += delta;
	 s->offset += delta;
	 s->len -= delta;

	 vg_assert(s->len != 0);
      } else if (addr <= s->addr && end >= s_end) {
	 /* this segment is completely contained within [addr, addr+len)
	    -> delete segment
	 */
         dump_translations_from(s);
         delete_segment_at(i);
         deleted = True;

	 if (debug)
	    VG_(printf)("  case 3: seg %d deleted\n", i);
      } else if (addr > s->addr && end < s_end) {
	 /* [addr, addr+len) is contained within a single segment
	    -> split segment into 3, delete middle portion
	  */
         Int i_middle;
         dump_translations_from(s);
         i_middle = split_segment(addr);
	 vg_assert(i_middle != -1);
	 (void)split_segment(addr+len);
	 vg_assert(segments[i_middle].addr == addr);
	 delete_segment_at(i_middle);
	 deleted = True;

	 if (debug)
	    VG_(printf)("  case 4: subrange %p-%p deleted\n",
			addr, addr+len);
      }

      /* If we deleted this segment (or any above), those above will
         have been moved down to fill in the hole in the segment
         array.  In order that we don't miss them, we have to
         re-consider this slot number; hence the i--. */
      if (deleted)
         i--;
   }
   preen_segments();
   if (0) show_segments("unmap_range(AFTER)");
}


/* Add a binding of [addr,addr+len) to
   (prot,flags,dev,ino,off,filename) in the segment array.
   Delete/truncate any previous mapping(s) covering that range.
*/
void 
VG_(map_file_segment)( Addr addr, SizeT len, 
                       UInt prot, UInt flags, 
                       UInt dev, UInt ino, ULong off, 
                       const Char *filename)
{
   static const Bool debug = False || mem_debug;
   Segment* s;
   Int      idx;

   if (debug)
      VG_(printf)(
         "\n"
         "map_file_segment(addr=%p len=%lu prot=0x%x flags=0x%x\n"
         "                 dev=0x%4x ino=%d off=%ld\n"
         "                 filename='%s')\n",
         addr, (ULong)len, prot, flags, dev, ino, off, filename);

   /* Everything must be page-aligned */
   vg_assert(VG_IS_PAGE_ALIGNED(addr));
   len = VG_PGROUNDUP(len);

   /* Nuke/truncate any existing segment(s) covering [addr,addr+len) */
   VG_(unmap_range)(addr, len);

   /* and now install this one */
   idx = create_segment(addr);
   vg_assert(segments_used >= 0 && segments_used <= VG_N_SEGMENTS);
   vg_assert(idx != -1);
   vg_assert(idx >= 0 && idx < segments_used);

   s = &segments[idx];
   vg_assert(s->addr == addr);
   s->prot     = prot;
   s->flags    = flags;
   s->len      = len;
   s->offset   = off;
   s->fnIdx    = filename==NULL ? -1 : allocate_segname(filename);
   s->filename = s->fnIdx==-1 ? NULL : &segnames[s->fnIdx].fname[0];
   s->dev      = dev;
   s->ino      = ino;
   s->seginfo  = NULL;

   /* Clean up right now */
   preen_segments();
   if (0) show_segments("after map_file_segment");

   /* If this mapping is of the beginning of a file, isn't part of
      Valgrind, is at least readable and seems to contain an object
      file, then try reading symbols from it.
   */
   if (s->seginfo == NULL
       && (addr+len < VG_(valgrind_base) || addr > VG_(valgrind_last))
       && (flags & (SF_MMAP|SF_NOSYMS)) == SF_MMAP) {
      if (off == 0
	  && s->fnIdx != -1
	  && (prot & (VKI_PROT_READ|VKI_PROT_EXEC)) == (VKI_PROT_READ|VKI_PROT_EXEC)
	  && len >= VKI_PAGE_SIZE
          && VG_(is_object_file)((void *)addr)) {
         s->seginfo = VG_(read_seg_symbols)(s);
         if (s->seginfo != NULL) {
            s->flags |= SF_DYNLIB;
         }
      } else if (flags & SF_MMAP) {
#if 0
	 const SegInfo *info;

	 /* Otherwise see if an existing SegInfo applies to this Segment */
	 for(info = VG_(next_seginfo)(NULL);
	     info != NULL;
	     info = VG_(next_seginfo)(info)) {
	    if (VG_(seg_overlaps)(s, VG_(seg_start)(info), VG_(seg_size)(info)))
            {
	       s->seginfo = (SegInfo *)info;
	       VG_(seginfo_incref)((SegInfo *)info);
	    }
	 }
#endif
      }
   }

   /* clean up */
   preen_segments();
}

void VG_(map_fd_segment)(Addr addr, SizeT len, UInt prot, UInt flags, 
			 Int fd, ULong off, const Char *filename)
{
   struct vki_stat st;
   Char *name = NULL;

   st.st_dev = 0;
   st.st_ino = 0;

   if (fd != -1 && (flags & SF_FILE)) {
      vg_assert((off & (VKI_PAGE_SIZE-1)) == 0);

      if (VG_(fstat)(fd, &st) < 0)
	 flags &= ~SF_FILE;
   }

   if ((flags & SF_FILE) && filename == NULL && fd != -1)
      name = VG_(resolve_filename_nodup)(fd);

   if (filename == NULL)
      filename = name;

   VG_(map_file_segment)(addr, len, prot, flags, 
                         st.st_dev, st.st_ino, off, filename);
}

void VG_(map_segment)(Addr addr, SizeT len, UInt prot, UInt flags)
{
   flags &= ~SF_FILE;

   VG_(map_file_segment)(addr, len, prot, flags, 0, 0, 0, 0);
}

/* set new protection flags on an address range */
void VG_(mprotect_range)(Addr a, SizeT len, UInt prot)
{
   Int r;
   static const Bool debug = False || mem_debug;

   if (debug)
      VG_(printf)("\nmprotect_range(%p, %lu, %x)\n", a, len, prot);

   if (0) show_segments( "mprotect_range(before)" );

   /* Everything must be page-aligned */
   vg_assert(VG_IS_PAGE_ALIGNED(a));
   len = VG_PGROUNDUP(len);

   split_segment(a);
   split_segment(a+len);

   r = find_segment(a);
   vg_assert(r != -1);
   segments[r].prot = prot;

   preen_segments();

   if (0) show_segments( "mprotect_range(after)");
}


/* Try to find a map space for [addr,addr+len).  If addr==0, it means
   the caller is prepared to accept a space at any location; if not,
   we will try for addr, but fail if we can't get it.  This mimics
   mmap fixed vs mmap not-fixed.
*/
Addr VG_(find_map_space)(Addr addr, SizeT len, Bool for_client)
{
   static const Bool debug = False || mem_debug;
   Addr ret;
   Addr addrOrig = addr;
   Addr limit = (for_client ? VG_(client_end)-1   : VG_(valgrind_last));
   Addr base  = (for_client ? VG_(client_mapbase) : VG_(valgrind_base));
   Addr hole_start, hole_end, hstart_any, hstart_fixed, hstart_final;
   Int i, i_any, i_fixed, i_final;
   SizeT hole_len;

   Bool fixed;

   if (debug) {
      VG_(printf)("\n\n");
      VG_(printf)("find_map_space(%p, %lu, %d) ...\n",
                  addr, len, for_client);
   }

   if (0) show_segments("find_map_space: start");

   if (addr == 0) {
      fixed = False;
   } else {
      fixed = True;
      /* leave space for redzone and still try to get the exact
         address asked for */
      addr -= VKI_PAGE_SIZE;
   }

   /* Everything must be page-aligned */
   vg_assert((addr & (VKI_PAGE_SIZE-1)) == 0);
   len = VG_PGROUNDUP(len);

   len += VKI_PAGE_SIZE * 2; /* leave redzone gaps before and after mapping */

   /* Scan the segment list, looking for a hole which satisfies the
      requirements.  At each point i we ask the question "can we use
      the hole in between segments[i-1] and segments[i] ?" */
   i_any = i_fixed = -1;
   hstart_any = hstart_fixed = 0;

   hole_start = hole_end = 0;

   /* Iterate over all possible holes, generating them into
      hole_start/hole_end.  Filter out invalid ones.  Then see if any
      are usable; if so set i_fixed/i_any and hstart_fixed/hstart_any.  
   */
   for (i = 0; i <=/*yes,really*/ segments_used; i++) {
      if (i == 0) {
         hole_start = 0;
         hole_end = segments[0].addr-1;
      } 
      else {
         vg_assert(segments_used > 0);
         if (i == segments_used) {
            hole_start = segments[i-1].addr + segments[i-1].len;
            hole_end = ~(Addr)0;
         } else {
            hole_start = segments[i-1].addr + segments[i-1].len;
            hole_end = segments[i].addr - 1;
         }
      }

      vg_assert(hole_start <= hole_end || hole_start == hole_end+1);

      /* ignore zero-sized holes */
      if (hole_start == hole_end+1)
         continue;

      vg_assert(VG_IS_PAGE_ALIGNED(hole_start));
      vg_assert(VG_IS_PAGE_ALIGNED(hole_end+1));

      /* ignore holes which fall outside the allowable area */
      if (!(hole_start >= base && hole_end <= limit))
         continue;

      vg_assert(hole_end > hole_start);
      hole_len = hole_end - hole_start + 1;
      vg_assert(VG_IS_PAGE_ALIGNED(hole_len));

      if (hole_len >= len && i_any == -1) {
         /* It will at least fit in this hole. */
         i_any = i;
         hstart_any = hole_start;
      }

      if (fixed && hole_start <= addr 
                && hole_start+hole_len >= addr+len) {
         /* We were asked for a fixed mapping, and this hole works.
            Bag it -- and stop searching as further searching is
            pointless. */
         i_fixed = i;
         hstart_fixed = addr;
         break;
      }
   }

   /* Summarise the final decision into i_final/hstart_final. */
   i_final = -1;
   hstart_final = 0;

   if (fixed) {
      i_final = i_fixed;
      hstart_final = hstart_fixed + VKI_PAGE_SIZE;  /* skip leading redzone */
   } else {
      i_final = i_any;
      hstart_final = hstart_any;
   }


   if (i_final != -1)
      ret = hstart_final;
   else
      ret = 0; /* not found */

   if (debug)
      VG_(printf)("find_map_space(%p, %lu, %d) -> %p\n\n",
                  addr, len, for_client, ret);

   if (fixed) {
      vg_assert(ret == 0 || ret == addrOrig);
   }

   return ret;
}


/* Pad the entire process address space, from "start"
   to VG_(valgrind_last) by creating an anonymous and inaccessible
   mapping over any part of the address space which is not covered
   by an entry in the segment list.

   This is designed for use around system calls which allocate
   memory in the process address space without providing a way to
   control its location such as io_setup. By choosing a suitable
   address with VG_(find_map_space) and then adding a segment for
   it and padding the address space valgrind can ensure that the
   kernel has no choice but to put the memory where we want it. */
void VG_(pad_address_space)(Addr start)
{
   Addr     addr = (start == 0) ? VG_(client_base) : start;
   void*    ret;

   Int      i = 0;
   Segment* s = i >= segments_used ? NULL : &segments[i];
   
   while (s && addr <= VG_(valgrind_last)) {
      if (addr < s->addr) {
         ret = VG_(mmap_native)((void*)addr, s->addr - addr, 0,
                     VKI_MAP_FIXED | VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS,
                     -1, 0);
      }
      addr = s->addr + s->len;
      i++;
      s = i >= segments_used ? NULL : &segments[i];
   }

   if (addr <= VG_(valgrind_last)) {
      ret = VG_(mmap_native)((void*)addr, VG_(valgrind_last) - addr + 1, 0,
                  VKI_MAP_FIXED | VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS,
                  -1, 0);
   }
}

/* Remove the address space padding added by VG_(pad_address_space)
   by removing any mappings that it created. */
void VG_(unpad_address_space)(Addr start)
{
   Addr     addr = (start == 0) ? VG_(client_base) : start;
   Int      ret;

   Int      i = 0;
   Segment* s = i >= segments_used ? NULL : &segments[i];

   while (s && addr <= VG_(valgrind_last)) {
      if (addr < s->addr) {
         ret = VG_(do_syscall2)(__NR_munmap, addr, s->addr - addr);
      }
      addr = s->addr + s->len;
      i++;
      s = i >= segments_used ? NULL : &segments[i];
   }

   if (addr <= VG_(valgrind_last)) {
      ret = VG_(do_syscall2)(__NR_munmap, addr, 
                             (VG_(valgrind_last) - addr) + 1);
   }
}

/* Find the segment holding 'a', or NULL if none. */
Segment *VG_(find_segment)(Addr a)
{
  Int r = find_segment(a);
  if (0) show_segments("find_segment");
  if (r == -1) return NULL;
  return &segments[r];
}

/* Assumes that 'a' is not in any segment.  Finds the lowest-addressed
   segment above 'a', or NULL if none.  Passing 'a' which is in fact in
   a segment is a checked error.
*/
Segment *VG_(find_segment_above_unmapped)(Addr a)
{
  Int r = find_segment_above_unmapped(a);
  if (0) show_segments("find_segment_above_unmapped");
  if (r == -1) return NULL;
  return &segments[r];
}

/* Assumes that 'a' is in some segment.  Finds the next segment along,
   or NULL if none.  Passing 'a' which is in fact not in a segment is
   a checked error.
*/
Segment *VG_(find_segment_above_mapped)(Addr a)
{
  Int r = find_segment_above_mapped(a);
  if (0) show_segments("find_segment_above_mapped");
  if (r == -1) return NULL;
  return &segments[r];
}


/*------------------------------------------------------------*/
/*--- Tracking permissions around %esp changes.            ---*/
/*------------------------------------------------------------*/

/*
   The stack
   ~~~~~~~~~
   The stack's segment seems to be dynamically extended downwards by
   the kernel as the stack pointer moves down.  Initially, a 1-page
   (4k) stack is allocated.  When SP moves below that for the first
   time, presumably a page fault occurs.  The kernel detects that the
   faulting address is in the range from SP - VGA_STACK_REDZONE_SZB
   upwards to the current valid stack.  It then extends the stack
   segment downwards for enough to cover the faulting address, and
   resumes the process (invisibly).  The process is unaware of any of
   this.

   That means that Valgrind can't spot when the stack segment is being
   extended.  Fortunately, we want to precisely and continuously
   update stack permissions around SP, so we need to spot all writes
   to SP anyway.

   The deal is: when SP is assigned a lower value, the stack is being
   extended.  Create suitably-permissioned pages to fill in any holes
   between the old stack ptr and this one, if necessary.  Then mark
   all bytes in the area just "uncovered" by this SP change as
   write-only.

   When SP goes back up, mark the area receded over as unreadable and
   unwritable.

   Just to record the SP boundary conditions somewhere convenient: 
   SP - VGA_STACK_REDZONE_SZB always points to the lowest live byte in
   the stack.  All addresses below SP - VGA_STACK_REDZONE_SZB are not
   live; those at and above it are.

   We do not concern ourselves here with the VGA_STACK_REDZONE_SZB
   bias; that is handled by new_mem_stack/die_mem_stack.
*/

/* This function gets called if new_mem_stack and/or die_mem_stack are
   tracked by the tool, and one of the specialised cases
   (eg. new_mem_stack_4) isn't used in preference.  
*/
VGA_REGPARM(2)
void VG_(unknown_SP_update)( Addr old_SP, Addr new_SP )
{
   static Int moans = 3;
   Word delta  = (Word)new_SP - (Word)old_SP;

   if (delta < -VG_(clo_max_stackframe) || VG_(clo_max_stackframe) < delta) {
      /* SP has changed by more than some threshold amount (by
         default, 2MB).  We take this to mean that the application is
         switching to a new stack, for whatever reason.
       
         JRS 20021001: following discussions with John Regehr, if a stack
         switch happens, it seems best not to mess at all with memory
         permissions.  Seems to work well with Netscape 4.X.  Really the
         only remaining difficulty is knowing exactly when a stack switch is
         happening. */
      if (VG_(clo_verbosity) > 0 && moans > 0) {
         moans--;
         VG_(message)(Vg_UserMsg,
            "Warning: client switching stacks?  "
            "SP change: %p --> %p", old_SP, new_SP);
         VG_(message)(Vg_UserMsg,
            "         to suppress, use: --max-stackframe=%d or greater",
            (delta < 0 ? -delta : delta));
         if (moans == 0)
            VG_(message)(Vg_UserMsg,
                "         further instances of this message "
                "will not be shown.");
      }
   } else if (delta < 0) {
      VG_TRACK( new_mem_stack, new_SP, -delta );

   } else if (delta > 0) {
      VG_TRACK( die_mem_stack, old_SP,  delta );
   }
}

/* 
   Test if a piece of memory is addressable with at least the "prot"
   protection permissions by examining the underlying segments.

   Really this is a very stupid algorithm and we could do much
   better by iterating through the segment array instead of through
   the address space.
 */
Bool VG_(is_addressable)(Addr p, SizeT size, UInt prot)
{
   Segment *seg;

   if ((p + size) < p)
      return False; /* reject wraparounds */
   if (size == 0)
      return True; /* isn't this a bit of a strange case? */

   p    = VG_PGROUNDDN(p);
   size = VG_PGROUNDUP(size);
   vg_assert(VG_IS_PAGE_ALIGNED(p));
   vg_assert(VG_IS_PAGE_ALIGNED(size));

   for (; size > 0; size -= VKI_PAGE_SIZE) {
      seg = VG_(find_segment)(p);
      if (!seg)
         return False;
      if ((seg->prot & prot) != prot)
         return False;
      p += VKI_PAGE_SIZE;
   }

   return True;
}


/*--------------------------------------------------------------------*/
/*--- Manage allocation of memory on behalf of the client          ---*/
/*--------------------------------------------------------------------*/

// Returns 0 on failure.
Addr VG_(get_memory_from_mmap_for_client)
        (Addr addr, SizeT len, UInt prot, UInt sf_flags)
{
   len = VG_PGROUNDUP(len);

   tl_assert(!(sf_flags & SF_FIXED));
   tl_assert(0 == addr);

   addr = (Addr)VG_(mmap)((void *)addr, len, prot, 
                          VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS | VKI_MAP_CLIENT,
                          sf_flags | SF_CORE, -1, 0);
   if ((Addr)-1 != addr)
      return addr;
   else
      return 0;
}


/* We'll call any RW mmaped memory segment, within the client address
   range, which isn't SF_CORE, a root. 
*/
void VG_(find_root_memory)(void (*add_rootrange)(Addr a, SizeT sz))
{
   Int     i;
   UInt    flags;
   Segment *s;

   for (i = 0; i < segments_used; i++) {
      s = &segments[i];
      flags = s->flags & (SF_SHARED|SF_MMAP|SF_VALGRIND
                          |SF_CORE|SF_STACK|SF_DEVICE);
      if (flags != SF_MMAP && flags != SF_STACK)
         continue;
      if ((s->prot & (VKI_PROT_READ|VKI_PROT_WRITE)) 
          != (VKI_PROT_READ|VKI_PROT_WRITE))
         continue;
      if (!VG_(is_client_addr)(s->addr) ||
          !VG_(is_client_addr)(s->addr+s->len))
         continue;

      (*add_rootrange)(s->addr, s->len);
   }
}


/*--------------------------------------------------------------------*/
/*--- Querying memory layout                                       ---*/
/*--------------------------------------------------------------------*/

Bool VG_(is_client_addr)(Addr a)
{
   return a >= VG_(client_base) && a < VG_(client_end);
}

Bool VG_(is_shadow_addr)(Addr a)
{
   return a >= VG_(shadow_base) && a < VG_(shadow_end);
}

Addr VG_(get_shadow_size)(void)
{
   return VG_(shadow_end)-VG_(shadow_base);
}


/*--------------------------------------------------------------------*/
/*--- Handling shadow memory                                       ---*/
/*--------------------------------------------------------------------*/

void VG_(init_shadow_range)(Addr p, UInt sz, Bool call_init)
{
vg_assert(0);
   if (0)
      VG_(printf)("init_shadow_range(%p, %d)\n", p, sz);

   vg_assert(VG_(needs).shadow_memory);
   vg_assert(VG_(tdict).track_init_shadow_page);

   sz = VG_PGROUNDUP(p+sz) - VG_PGROUNDDN(p);
   p  = VG_PGROUNDDN(p);

   VG_(mprotect)((void *)p, sz, VKI_PROT_READ|VKI_PROT_WRITE);
   
   if (call_init) 
      while(sz) {
	 /* ask the tool to initialize each page */
	 VG_TRACK( init_shadow_page, VG_PGROUNDDN(p) );
	 
	 p  += VKI_PAGE_SIZE;
	 sz -= VKI_PAGE_SIZE;
      }
}

void *VG_(shadow_alloc)(UInt size)
{
   static Addr shadow_alloc = 0;
   Addr try_here;
   Int r;

   if (0) show_segments("shadow_alloc(before)");

   vg_assert(VG_(needs).shadow_memory);
   vg_assert(!VG_(tdict).track_init_shadow_page);

   size = VG_PGROUNDUP(size);

   if (shadow_alloc == 0)
      shadow_alloc = VG_(shadow_base);

   if (shadow_alloc >= VG_(shadow_end))
      goto failed;

   try_here = shadow_alloc;
   vg_assert(VG_IS_PAGE_ALIGNED(try_here));
   vg_assert(VG_IS_PAGE_ALIGNED(size));
   vg_assert(size > 0);

   if (0)
      VG_(printf)("shadow_alloc: size %d, trying at %p\n", size, (void*)try_here);

   /* this is big-bang allocated, so we don't expect to find a listed
      segment for it. */
   /* This is really an absolute disgrace.  Sometimes the big-bang
      mapping is in the list (due to re-reads of /proc/self/maps,
      presumably) and sometimes it isn't. */
#if 0
   r = find_segment(try_here);
   vg_assert(r == -1);
   r = find_segment(try_here+size-1);
   vg_assert(r == -1);
#endif

   r = VG_(mprotect_native)( (void*)try_here, 
                             size,  VKI_PROT_READ|VKI_PROT_WRITE );

   if (r != 0)
      goto failed;

   shadow_alloc += size;
   return (void*)try_here;

  failed:
   VG_(printf)(
       "valgrind: Could not allocate address space (0x%x bytes)\n"
       "valgrind:   for shadow memory chunk.\n",
       size
      ); 
   VG_(exit)(1);
}

/*------------------------------------------------------------*/
/*--- pointercheck                                         ---*/
/*------------------------------------------------------------*/

Bool VGA_(setup_pointercheck)(Addr client_base, Addr client_end)
{
   vg_assert(0 != client_end);
#if defined(VGP_x86_linux)
   /* Client address space segment limit descriptor entry */
   #define POINTERCHECK_SEGIDX  1

   vki_modify_ldt_t ldt = { 
      POINTERCHECK_SEGIDX,       // entry_number
      client_base,               // base_addr
      (client_end - client_base) / VKI_PAGE_SIZE, // limit
      1,                         // seg_32bit
      0,                         // contents: data, RW, non-expanding
      0,                         // ! read_exec_only
      1,                         // limit_in_pages
      0,                         // ! seg not present
      1,                         // useable
   };
   int ret = VG_(do_syscall3)(__NR_modify_ldt, 1, (UWord)&ldt, sizeof(ldt));
   if (ret < 0) {
      VG_(message)(Vg_UserMsg,
                   "Warning: ignoring --pointercheck=yes, "
                   "because modify_ldt failed (errno=%d)", -ret);
      return False;
   } else {
      return True;
   }
#elif defined(VGP_amd64_linux)
   if (0) 
      VG_(message)(Vg_DebugMsg, "ignoring --pointercheck (unimplemented)");
   return True;
#else
#  error Unknown architecture
#endif
}

/*--------------------------------------------------------------------*/
/*--- end                                              aspacemgr.c ---*/
/*--------------------------------------------------------------------*/
