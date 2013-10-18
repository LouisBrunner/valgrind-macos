
/*--------------------------------------------------------------------*/
/*--- Address space manager.                  pub_tool_aspacemgr.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward
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

#ifndef __PUB_TOOL_ASPACEMGR_H
#define __PUB_TOOL_ASPACEMGR_H

#include "pub_tool_basics.h"   // VG_ macro

//--------------------------------------------------------------
// Definition of address-space segments

/* Describes segment kinds. */
typedef
   enum {
      SkFree,   // unmapped space
      SkAnonC,  // anonymous mapping belonging to the client
      SkAnonV,  // anonymous mapping belonging to valgrind
      SkFileC,  // file mapping belonging to the client
      SkFileV,  // file mapping belonging to valgrind
      SkShmC,   // shared memory segment belonging to the client
      SkResvn   // reservation
   }
   SegKind;

/* Describes how a reservation segment can be resized. */
typedef
   enum {
      SmLower,  // lower end can move up
      SmFixed,  // cannot be shrunk
      SmUpper   // upper end can move down
   }
   ShrinkMode;

/* Describes a segment.  Invariants:

     kind == SkFree:
        // the only meaningful fields are .start and .end

     kind == SkAnon{C,V}:
        // smode==SmFixed
        // there's no associated file:
        dev==ino==foff = 0, fnidx == -1
        // segment may have permissions

     kind == SkFile{C,V}:
        // smode==SmFixed
        moveLo == moveHi == NotMovable, maxlen == 0
        // there is an associated file
        // segment may have permissions

     kind == SkShmC:
        // smode==SmFixed
        // there's no associated file:
        dev==ino==foff = 0, fnidx == -1
        // segment may have permissions

     kind == SkResvn
        // the segment may be resized if required
        // there's no associated file:
        dev==ino==foff = 0, fnidx == -1
        // segment has no permissions
        hasR==hasW==hasX==anyTranslated == False

     Also: anyTranslated==True is only allowed in SkFileV and SkAnonV
           (viz, not allowed to make translations from non-client areas)
*/
typedef
   struct {
      SegKind kind;
      /* Extent (SkFree, SkAnon{C,V}, SkFile{C,V}, SkResvn) */
      Addr    start;    // lowest address in range
      Addr    end;      // highest address in range
      /* Shrinkable? (SkResvn only) */
      ShrinkMode smode;
      /* Associated file (SkFile{C,V} only) */
      ULong   dev;
      ULong   ino;
      Off64T  offset;
      UInt    mode;
      Int     fnIdx;    // file name table index, if name is known
      /* Permissions (SkAnon{C,V}, SkFile{C,V} only) */
      Bool    hasR;
      Bool    hasW;
      Bool    hasX;
      Bool    hasT;     // True --> translations have (or MAY have)
                        // been taken from this segment
      Bool    isCH;     // True --> is client heap (SkAnonC ONLY)
      /* Admin */
      Bool    mark;
   }
   NSegment;


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
extern Int VG_(am_get_segment_starts)( Addr* starts, Int nStarts );


// See pub_core_aspacemgr.h for description.
extern NSegment const * VG_(am_find_nsegment) ( Addr a ); 

// See pub_core_aspacemgr.h for description.
extern HChar* VG_(am_get_filename)( NSegment const * );

// See pub_core_aspacemgr.h for description.
extern Bool VG_(am_is_valid_for_client) ( Addr start, SizeT len, 
                                          UInt prot );

// See pub_core_aspacemgr.h for description.
/* Really just a wrapper around VG_(am_mmap_anon_float_valgrind). */
extern void* VG_(am_shadow_alloc)(SizeT size);

/* Unmap the given address range and update the segment array
   accordingly.  This fails if the range isn't valid for valgrind. */
extern SysRes VG_(am_munmap_valgrind)( Addr start, SizeT length );

#endif   // __PUB_TOOL_ASPACEMGR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
