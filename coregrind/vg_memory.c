
/*--------------------------------------------------------------------*/
/*--- Memory-related stuff: segment initialisation and tracking,   ---*/
/*--- stack operations                                             ---*/
/*---                                                  vg_memory.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2004 Julian Seward 
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

/* Define to debug the memory-leak-detector. */
/* #define VG_DEBUG_LEAKCHECK */

static const Bool mem_debug = False;

/*--------------------------------------------------------------*/
/*---                                                        ---*/
/*--------------------------------------------------------------*/

#define VG_N_SEGMENTS 400

/* Array [0 .. segments_used-1] of all mappings. */
/* Sorted by .addr field. */
/* I: len may not be zero. */
/* I: overlapping segments are not allowed. */

static Segment segments[VG_N_SEGMENTS];
static Int     segments_used = 0;


/* Returns -1 if 'a' denotes an address prior to seg, 1 if it denotes
   an address after it, and 0 if it denotes an address covered by
   seg. */
static Int compare_addr_with_seg ( Addr a, Segment* seg )
{
   if (a < seg->addr) 
      return -1;
   if (a >= seg->addr + seg->len) 
      return 1;
   return 0;
}

/* Find the (index of the) segment that contains 'a', or -1 if
   none. */
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
   vg_assert(segments_used+1 < VG_N_SEGMENTS);
   for (j = segments_used; j > i; j--)
      segments[j] = segments[j-1];
   segments_used++;
}

/* Shift segments [i+1 .. segments_used-1] down by one, and decrement
   segments_used. */
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
   segments[i].prot = 0;
   segments[i].flags = 0;
   segments[i].addr = 0;
   segments[i].len = 0;
   segments[i].offset = 0;
   segments[i].filename = NULL;
   segments[i].dev = 0;
   segments[i].ino = 0;
   segments[i].symtab = NULL;
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

//static 
void show_segments ( HChar* who )
{
  Int i;
  VG_(printf)("<<< SHOW_SEGMENTS: %s %d\n", who, segments_used);
  for (i = 0; i < segments_used; i++) {
      VG_(printf)(
         "%2d: %p %llu 0x%x 0x%x 0x%x %d %lld %s\n",
         i,
         segments[i].addr, (ULong)segments[i].len, segments[i].prot, segments[i].flags, segments[i].dev, segments[i].ino, (Long)segments[i].offset, "qqq" /*segments[i].filename*/);
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
   vg_assert((a & (VKI_PAGE_SIZE-1)) == 0);
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

/*--------------------------------------------------------------*/
/*---                                                        ---*/
/*--------------------------------------------------------------*/

#define IS_PAGE_ALIGNED(_aa)  (((_aa) & (VKI_PAGE_SIZE-1)) == 0)


static Int addrcmp(const void *ap, const void *bp)
{
   Addr a = *(Addr *)ap;
   Addr b = *(Addr *)bp;
   Int ret;

   if (a == b)
      ret = 0;
   else
      ret = (a < b) ? -1 : 1;

   return ret;
}

static Char *straddr(void *p)
{
   static Char buf[16];

   VG_(sprintf)(buf, "%p", *(Addr *)p);

   return buf;
}

static SkipList sk_segments = SKIPLIST_INIT(Segment, addr, addrcmp, straddr, VG_AR_CORE);

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
   if (s->symtab != NULL) {
      VG_(symtab_decref)(s->symtab, s->addr);
      s->symtab = NULL;
   }

   VG_(SkipNode_Free)(&sk_segments, s);
}

/* Split a segment at address a, returning the new segment */
Segment *VG_(split_segment)(Addr a)
{
   Segment *s = VG_(SkipList_Find)(&sk_segments, &a);
   Segment *ns;
   Int delta;
vg_assert(0);
   vg_assert((a & (VKI_PAGE_SIZE-1)) == 0);

   /* missed */
   if (s == NULL)
      return NULL;

   /* a at or beyond endpoint */
   if (s->addr == a || a >= (s->addr+s->len))
      return NULL;

   vg_assert(a > s->addr && a < (s->addr+s->len));

   ns = VG_(SkipNode_Alloc)(&sk_segments);

   *ns = *s;

   delta = a - s->addr;
   ns->addr += delta;
   ns->offset += delta;
   ns->len -= delta;
   s->len = delta;

   if (s->filename != NULL)
      ns->filename = VG_(arena_strdup)(VG_AR_CORE, s->filename);

   if (ns->symtab != NULL)
      VG_(symtab_incref)(ns->symtab);

   VG_(SkipList_Insert)(&sk_segments, ns);

   return ns;
}

/* This unmaps all the segments in the range [addr, addr+len); any
   partial mappings at the ends are truncated. */
void VG_(unmap_range)(Addr addr, SizeT len)
{
   Segment *s;
   Segment *next;
   static const Bool debug = False || mem_debug;
   Addr end, s_end;
   Int i;

   if (len == 0)
      return;

   len = PGROUNDUP(len);

   if (debug)
      VG_(printf)("unmap_range(%p, %d)\n", addr, len);
   if (0) show_segments("unmap_range");
   end = addr+len;

   /* Everything must be page-aligned */
   vg_assert(IS_PAGE_ALIGNED(addr));
   vg_assert(IS_PAGE_ALIGNED(len));

   for (i = 0; i < segments_used; i++) {
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
	 s->len = addr - s->addr;

	 if (debug)
	    VG_(printf)("  case 1: s->len=%d\n", s->len);
      } else if (addr <= s->addr && end > s->addr && end < s_end) {
	 /* this segment's head is truncated by [addr, addr+len)
	    -> truncate head
	 */
	 Word delta = end - s->addr;

	 if (debug)
	    VG_(printf)("  case 2: s->addr=%p s->len=%d delta=%d\n", s->addr, s->len, delta);

	 s->addr += delta;
	 s->offset += delta;
	 s->len -= delta;

	 vg_assert(s->len != 0);
      } else if (addr <= s->addr && end >= s_end) {
	 /* this segment is completely contained within [addr, addr+len)
	    -> delete segment
	 */
         delete_segment_at(i);

	 if (debug)
	    VG_(printf)("  case 3: seg %d deleted\n", i);
      } else if (addr > s->addr && end < s_end) {
	 /* [addr, addr+len) is contained within a single segment
	    -> split segment into 3, delete middle portion
	  */
         Int i_middle = split_segment(addr);
	 vg_assert(i_middle != -1);
	 (void)split_segment(addr+len);

	 vg_assert(segments[i_middle].addr == addr);
	 delete_segment_at(i_middle);

	 if (debug)
	    VG_(printf)("  case 4: subrange %p-%p deleted\n",
			addr, addr+len);
      }
   }
   if (0) show_segments("unmap_range(AFTER)");
}

/* Return true if two segments are adjacent and mergable (s1 is
   assumed to have a lower ->addr than s2) */
static inline Bool neighbours(Segment *s1, Segment *s2)
{
   if (s1->addr+s1->len != s2->addr)
      return False;

   if (s1->flags != s2->flags)
      return False;

   if (s1->prot != s2->prot)
      return False;

   if (s1->symtab != s2->symtab)
      return False;

   if (s1->flags & SF_FILE){
      if ((s1->offset + s1->len) != s2->offset)
	 return False;
      if (s1->dev != s2->dev)
	 return False;
      if (s1->ino != s2->ino)
	 return False;
   }
   
   return True;
}

/* If possible, merge segment with its neighbours - some segments,
   including s, may be destroyed in the process */
static void merge_segments(Addr a, SizeT len)
{
   Segment *s;
   Segment *next;

   vg_assert((a   & (VKI_PAGE_SIZE-1)) == 0);
   vg_assert((len & (VKI_PAGE_SIZE-1)) == 0);

   a   -= VKI_PAGE_SIZE;
   len += VKI_PAGE_SIZE;

   for(s = VG_(SkipList_Find)(&sk_segments, &a);
       s != NULL && s->addr < (a+len);) {
      next = VG_(SkipNode_Next)(&sk_segments, s);

      if (next && neighbours(s, next)) {
	 Segment *rs;

	 if (0)
	    VG_(printf)("merge %p-%p with %p-%p\n",
			s->addr, s->addr+s->len,
			next->addr, next->addr+next->len);
	 s->len += next->len;
	 s = VG_(SkipNode_Next)(&sk_segments, next);

	 rs = VG_(SkipList_Remove)(&sk_segments, &next->addr);
	 vg_assert(next == rs);
	 freeseg(next);
      } else
	 s = next;
   }
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
   Segment *s;
   static const Bool debug = False || mem_debug;
   Bool recycled;
   Int  idx;

   if (debug)
      VG_(printf)(
         "\n"
         "map_file_segment(addr=%p len=%llu prot=0x%x flags=0x%x\n"
         "                 dev=0x%4x ino=%d off=%ld\n"
         "                 filename='%s')\n",
         addr, (ULong)len, prot, flags, dev, ino, off, filename);

   /* Everything must be page-aligned */
   vg_assert(IS_PAGE_ALIGNED(addr));
   len = PGROUNDUP(len);

   /* Nuke/truncate any existing segment(s) covering [addr,addr+len) */
   VG_(unmap_range)(addr, len);

   /* and now install this one */
   idx = create_segment(addr);
   vg_assert(segments_used >= 0 && segments_used <= VG_N_SEGMENTS);
   vg_assert(idx != -1);
   vg_assert(idx >= 0 && idx < segments_used);
   vg_assert(segments[idx].addr == addr);
   segments[idx].prot     = prot;
   segments[idx].flags    = flags;
   segments[idx].len      = len;
   segments[idx].offset   = off;
   segments[idx].filename = filename;
   segments[idx].dev      = dev;
   segments[idx].ino      = ino;
   segments[idx].symtab   = NULL;
   return;

#if 0
   /* First look to see what already exists around here */
   s = VG_(SkipList_Find)(&sk_segments, &addr);

   if (s != NULL && s->addr == addr && s->len == len) {
      recycled = True;
      recycleseg(s);

      /* If we had a symtab, but the new mapping is incompatible, then
	 free up the old symtab in preparation for a new one. */
      if (s->symtab != NULL		&&
	  (!(s->flags & SF_FILE)	||
	   !(flags & SF_FILE)		||
	   s->dev != dev		||
	   s->ino != ino		||
	   s->offset != off)) {
	 VG_(symtab_decref)(s->symtab, s->addr);
	 s->symtab = NULL;
      }
   } else {
      recycled = False;
      VG_(unmap_range)(addr, len);

      s = VG_(SkipNode_Alloc)(&sk_segments);

      s->addr   = addr;
      s->len    = len;
      s->symtab = NULL;
   }

   s->flags  = flags;
   s->prot   = prot;
   s->dev    = dev;
   s->ino    = ino;
   s->offset = off;
   
   if (filename != NULL)
      s->filename = VG_(arena_strdup)(VG_AR_CORE, filename);
   else
      s->filename = NULL;

   if (debug) {
      Segment *ts;
      for(ts = VG_(SkipNode_First)(&sk_segments);
	  ts != NULL;
	  ts = VG_(SkipNode_Next)(&sk_segments, ts))
	 VG_(printf)("list: %8p->%8p ->%d (0x%x) prot=%x flags=%x\n",
		     ts, ts->addr, ts->len, ts->len, ts->prot, ts->flags);

      VG_(printf)("inserting s=%p addr=%p len=%d\n",
		  s, s->addr, s->len);
   }

   if (!recycled)
      VG_(SkipList_Insert)(&sk_segments, s);

   /* If this mapping is of the beginning of a file, isn't part of
      Valgrind, is at least readable and seems to contain an object
      file, then try reading symbols from it. */
   if ((flags & (SF_MMAP|SF_NOSYMS)) == SF_MMAP	&&
       s->symtab == NULL) {
      if (off == 0									&&
	  filename != NULL								&&
	  (prot & (VKI_PROT_READ|VKI_PROT_EXEC)) == (VKI_PROT_READ|VKI_PROT_EXEC)	&&
	  len >= VKI_PAGE_SIZE							&&
	  s->symtab == NULL								&&
	  VG_(is_object_file)((void *)addr)) 
      {
         s->symtab = VG_(read_seg_symbols)(s);

         if (s->symtab != NULL) {
            s->flags |= SF_DYNLIB;
         }
      } else if (flags & SF_MMAP) {
	 const SegInfo *info;

	 /* Otherwise see if an existing symtab applies to this Segment */
	 for(info = VG_(next_seginfo)(NULL);
	     info != NULL;
	     info = VG_(next_seginfo)(info)) {
	    if (VG_(seg_overlaps)(s, VG_(seg_start)(info), VG_(seg_size)(info)))
            {
	       s->symtab = (SegInfo *)info;
	       VG_(symtab_incref)((SegInfo *)info);
	    }
	 }
      }
   }

   /* clean up */
   merge_segments(addr, len);
#endif
}

void VG_(map_fd_segment)(Addr addr, SizeT len, UInt prot, UInt flags, 
			 Int fd, ULong off, const Char *filename)
{
   struct vki_stat st;
   Char *name = NULL;
   //vg_assert(0);

   st.st_dev = 0;
   st.st_ino = 0;

   if (fd != -1 && (flags & SF_FILE)) {
      vg_assert((off & (VKI_PAGE_SIZE-1)) == 0);

      if (VG_(fstat)(fd, &st) < 0)
	 flags &= ~SF_FILE;
   }

   //   if ((flags & SF_FILE) && filename == NULL && fd != -1)
   //   name = VG_(resolve_filename)(fd);

   if (filename == NULL)
      filename = name;

   VG_(map_file_segment)(addr, len, prot, flags, st.st_dev, st.st_ino, off, filename);

   if (name)
      VG_(arena_free)(VG_AR_CORE, name);
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
   Segment *s, *next;
   static const Bool debug = False || mem_debug;

   if (debug)
      VG_(printf)("\nmprotect_range(%p, %d, %x)\n", a, len, prot);

   if (0) show_segments( "mprotect_range(before)" );

   /* Everything must be page-aligned */
   vg_assert((a & (VKI_PAGE_SIZE-1)) == 0);
   len = PGROUNDUP(len);

   /*VG_*/ (split_segment)(a);
   /*VG_*/ (split_segment)(a+len);

   r = find_segment(a);
   vg_assert(r != -1);
   segments[r].prot = prot;
   if (0) show_segments( "mprotect_range(after)");

   return;

vg_assert(0);
   for(s = VG_(SkipList_Find)(&sk_segments, &a);
       s != NULL && s->addr < a+len;
       s = next)
   {
      next = VG_(SkipNode_Next)(&sk_segments, s);
      if (s->addr < a)
	 continue;

      s->prot = prot;
   }

   merge_segments(a, len);
}

/* Try to find a map space for [addr,addr+len).  If addr==0, it means
   the caller is prepared to accept a space at any location; if not,
   we will try for addr, but fail if we can't get it.  This mimics
   mmap fixed vs mmap not-fixed.
*/
Addr VG_(find_map_space)(Addr addr, SizeT len, Bool for_client)
{
   static const Bool debug = False || mem_debug;
   Segment *s;
   Addr ret;
   Addr limit = (for_client ? VG_(client_end)-1   : VG_(valgrind_last));
   Addr base  = (for_client ? VG_(client_mapbase) : VG_(valgrind_base));
   Addr hole_start, hole_end, hstart_any, hstart_fixed, hstart_final;
   Int i, i_any, i_fixed, i_final;
   SizeT hole_len;

   Bool fixed;

   if (debug) {
      VG_(printf)("\n\n");
      VG_(printf)("find_map_space(%p, %d, %d) ...\n",
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
   len = PGROUNDUP(len);

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

      vg_assert(IS_PAGE_ALIGNED(hole_start));
      vg_assert(IS_PAGE_ALIGNED(hole_end+1));

      /* ignore holes which fall outside the allowable area */
      if (!(hole_start >= base && hole_end <= limit))
         continue;

      vg_assert(hole_end > hole_start);
      hole_len = hole_end - hole_start + 1;

      if (hole_len >= len && i_any == -1) {
         /* It will at least fit in this hole. */
         i_any = i;
         hstart_any = hole_start;
      }

      if (fixed && hole_start <= addr && hole_len >= len) {
         /* We were asked for a fixed mapping, and this hole works.
            Bag it -- and stop searching as further searching is
            pointless. */
         i_fixed = i;
         hstart_fixed = hole_start;
         break;
      }
   }

   /* Summarise the final decision into i_final/hstart_final. */
   i_final = -1;
   hstart_final = 0;

   if (fixed) {
      i_final = i_fixed;
      hstart_final = hstart_fixed;
   } else {
      i_final = i_any;
      hstart_final = hstart_any;
   }


   if (i_final != -1)
      ret = hstart_final + VKI_PAGE_SIZE;  /* skip leading redzone */
   else
      ret = 0; /* not found */

   if (debug)
      VG_(printf)("find_map_space(%p, %d, %d) -> %p\n\n",
                  addr, len, for_client, ret);

   return ret;

   vg_assert(0);
vg_assert(0);
///////////
   if (debug)
      VG_(printf)("find_map_space: ret starts as %p-%p client=%d\n",
		  ret, ret+len, for_client);

   s = VG_(SkipList_Find)(&sk_segments, &ret);
   if (s == NULL)
      s = VG_(SkipNode_First)(&sk_segments);

   for( ;
       s != NULL && s->addr < (ret+len);
       s = VG_(SkipNode_Next)(&sk_segments, s))
   {
      if (debug)
	 VG_(printf)("s->addr=%p len=%d (%p) ret=%p\n",
		     s->addr, s->len, s->addr+s->len, ret);

      if (s->addr < (ret + len) && (s->addr + s->len) > ret)
	 ret = s->addr+s->len;
   }

   if (debug) {
      if (s)
	 VG_(printf)("  s->addr=%p ->len=%d\n", s->addr, s->len);
      else
	 VG_(printf)("  s == NULL\n");
   }

   if (((limit - len)+1) < ret)
      ret = 0;			/* no space */
   else
      ret += VKI_PAGE_SIZE; /* skip leading redzone */

   if (debug)
      VG_(printf)("find_map_space(%p, %d, %d) -> %p\n\n",
		  addr, len, for_client, ret);
   
   return ret;
}

/* Pad the entire process address space, from VG_(client_base)
   to VG_(valgrind_last) by creating an anonymous and inaccessible
   mapping over any part of the address space which is not covered
   by an entry in the segment list.

   This is designed for use around system calls which allocate
   memory in the process address space without providing a way to
   control its location such as io_setup. By choosing a suitable
   address with VG_(find_map_space) and then adding a segment for
   it and padding the address space valgrind can ensure that the
   kernel has no choice but to put the memory where we want it. */
void VG_(pad_address_space)(void)
{
   Addr addr = VG_(client_base);
   Segment *s = VG_(SkipNode_First)(&sk_segments);
   Addr ret;
vg_assert(0);
   
   while (s && addr <= VG_(valgrind_last)) {
      if (addr < s->addr) {
         PLATFORM_DO_MMAP(ret, addr, s->addr - addr, 0,
                          VKI_MAP_FIXED | VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS,
                          -1, 0);
      }
        
      addr = s->addr + s->len;
      s = VG_(SkipNode_Next)(&sk_segments, s);
   }

   if (addr <= VG_(valgrind_last)) {
      PLATFORM_DO_MMAP(ret, addr, VG_(valgrind_last) - addr + 1, 0,
                       VKI_MAP_FIXED | VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS,
                       -1, 0);
   }

   return;
}

/* Remove the address space padding added by VG_(pad_address_space)
   by removing any mappings that it created. */
void VG_(unpad_address_space)(void)
{
   Addr addr = VG_(client_base);
   Segment *s = VG_(SkipNode_First)(&sk_segments);
   Int ret;
vg_assert(0);

   while (s && addr <= VG_(valgrind_last)) {
      if (addr < s->addr) {
         ret = VG_(do_syscall2)(__NR_munmap, addr, s->addr - addr);
      }
         
      addr = s->addr + s->len;
      s = VG_(SkipNode_Next)(&sk_segments, s);
   }

   if (addr <= VG_(valgrind_last)) {
      ret = VG_(do_syscall2)(__NR_munmap, addr, (VG_(valgrind_last) - addr) + 1);
   }

   return;
}

/* Find the segment holding 'a', or NULL if none. */
Segment *VG_(find_segment)(Addr a)
{
  Int r = find_segment(a);
  if (0) show_segments("find_segment");
  if (r == -1) return NULL;
  return &segments[r];
vg_assert(0);
   return VG_(SkipList_Find)(&sk_segments, &a);
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



Segment *VG_(first_segment)(void)
{
vg_assert(0);
   return VG_(SkipNode_First)(&sk_segments);
}

Segment *VG_(next_segment)(Segment *s)
{
vg_assert(0);
   return VG_(SkipNode_Next)(&sk_segments, s);
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
   tracked by the tool, and one of the specialised cases (eg. new_mem_stack_4)
   isn't used in preference */
REGPARM(2)
void VG_(unknown_SP_update)( Addr old_SP, Addr new_SP )
{
   Word delta  = (Word)new_SP - (Word)old_SP;

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
                                    "%%esp: %p --> %p", old_SP, new_SP);
   } else if (delta < 0) {
      VG_TRACK( new_mem_stack, new_SP, -delta );

   } else if (delta > 0) {
      VG_TRACK( die_mem_stack, old_SP,  delta );
   }
}

static jmp_buf segv_jmpbuf;

static void segv_handler(Int seg)
{
   __builtin_longjmp(segv_jmpbuf, 1);
   VG_(core_panic)("longjmp failed");
}

/* 
   Test if a piece of memory is addressable by setting up a temporary
   SIGSEGV handler, then try to touch the memory.  No signal = good,
   signal = bad.
 */
Bool VG_(is_addressable)(Addr p, SizeT size)
{
   volatile Char * volatile cp = (volatile Char *)p;
   volatile Bool ret;
   struct vki_sigaction sa, origsa;
   vki_sigset_t mask;

   sa.ksa_handler = segv_handler;
   sa.sa_flags = 0;
   VG_(sigfillset)(&sa.sa_mask);
   VG_(sigaction)(VKI_SIGSEGV, &sa, &origsa);
   VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &mask);

   if (__builtin_setjmp(&segv_jmpbuf) == 0) {
      while(size--)
	 *cp++;
      ret = True;
    } else
      ret = False;

   VG_(sigaction)(VKI_SIGSEGV, &origsa, NULL);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, NULL);

   return ret;
}

/*--------------------------------------------------------------------*/
/*--- Manage allocation of memory on behalf of the client          ---*/
/*--------------------------------------------------------------------*/

// Returns 0 on failure.
Addr VG_(client_alloc)(Addr addr, SizeT len, UInt prot, UInt sf_flags)
{
   len = PGROUNDUP(len);
vg_assert(0);

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

void VG_(client_free)(Addr addr)
{
   Segment *s = VG_(find_segment)(addr);

vg_assert(0);
   if (s == NULL || s->addr != addr || !(s->flags & SF_CORE)) {
      VG_(message)(Vg_DebugMsg, "VG_(client_free)(%p) - no CORE memory found there", addr);
      return;
   }

   VG_(munmap)((void *)s->addr, s->len);
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
vg_assert(0);
   return a >= VG_(shadow_base) && a < VG_(shadow_end);
}

Bool VG_(is_valgrind_addr)(Addr a)
{
vg_assert(0);
   return a >= VG_(valgrind_base) && a <= VG_(valgrind_last);
}

Addr VG_(get_client_base)(void)
{
vg_assert(0);
   return VG_(client_base);
}

Addr VG_(get_client_end)(void)
{
vg_assert(0);
   return VG_(client_end);
}

Addr VG_(get_client_size)(void)
{
vg_assert(0);
   return VG_(client_end)-VG_(client_base);
}

Addr VG_(get_shadow_base)(void)
{
vg_assert(0);
   return VG_(shadow_base);
}

Addr VG_(get_shadow_end)(void)
{
vg_assert(0);
   return VG_(shadow_end);
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
   vg_assert(VG_(defined_init_shadow_page)());

   sz = PGROUNDUP(p+sz) - PGROUNDDN(p);
   p = PGROUNDDN(p);

   VG_(mprotect)((void *)p, sz, VKI_PROT_READ|VKI_PROT_WRITE);
   
   if (call_init) 
      while(sz) {
	 /* ask the tool to initialize each page */
	 VG_TRACK( init_shadow_page, PGROUNDDN(p) );
	 
	 p  += VKI_PAGE_SIZE;
	 sz -= VKI_PAGE_SIZE;
      }
}

void *VG_(shadow_alloc)(UInt size)
{
   static Addr shadow_alloc = 0;
   void *ret;
vg_assert(0);

   vg_assert(VG_(needs).shadow_memory);
   vg_assert(!VG_(defined_init_shadow_page)());

   size = PGROUNDUP(size);

   if (shadow_alloc == 0)
      shadow_alloc = VG_(shadow_base);

   if (shadow_alloc >= VG_(shadow_end))
       return 0;

   ret = (void *)shadow_alloc;
   VG_(mprotect)(ret, size, VKI_PROT_READ|VKI_PROT_WRITE);

   shadow_alloc += size;

   return ret;
}

/*--------------------------------------------------------------------*/
/*--- end                                              vg_memory.c ---*/
/*--------------------------------------------------------------------*/

