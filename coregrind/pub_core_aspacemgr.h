
/*--------------------------------------------------------------------*/
/*--- The address space manager.                                   ---*/
/*---                                         pub_core_aspacemgr.h ---*/
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

#ifndef __PUB_CORE_ASPACEMGR_H
#define __PUB_CORE_ASPACEMGR_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with management of the entire process
// address space.  Almost everything depends upon it, including dynamic
// memory management.  Hence this module is almost completely
// standalone; the only module it uses is m_debuglog.  DO NOT CHANGE
// THIS.
//--------------------------------------------------------------------

/* #include "pub_tool_aspacemgr.h" */

/* A Segment is mapped piece of client memory.  This covers all kinds
   of mapped memory (exe, brk, mmap, .so, shm, stack, etc)

   We try to encode everything we know about a particular segment here.
*/
#define SF_FIXED    (1 <<  0) // client asked for MAP_FIXED
#define SF_SHARED   (1 <<  1) // shared
#define SF_SHM      (1 <<  2) // SYSV SHM (also SF_SHARED)
#define SF_MMAP     (1 <<  3) // mmap memory
#define SF_FILE     (1 <<  4) // mapping is backed by a file
#define SF_STACK    (1 <<  5) // is a stack
#define SF_GROWDOWN (1 <<  6) // segment grows down
#define SF_GROWUP   (1 <<  7) // segment grows up
#define SF_EXEC     (1 <<  8) // segment created by exec
#define SF_DYNLIB   (1 <<  9) // mapped from dynamic library
#define SF_NOSYMS   (1 << 10) // don't load syms, even if present
#define SF_BRK      (1 << 11) // brk segment
#define SF_CORE     (1 << 12) // allocated by core on behalf of the client
#define SF_VALGRIND (1 << 13) // a valgrind-internal mapping - not in client
#define SF_CODE     (1 << 14) // segment contains cached code
#define SF_DEVICE   (1 << 15) // device mapping; avoid careless touching

struct _Segment {
   UInt         prot;         // VKI_PROT_*
   UInt         flags;        // SF_*

   Addr         addr;         // mapped addr (page aligned)
   SizeT        len;          // size of mapping (page aligned)

   // These are valid if (flags & SF_FILE)
   OffT        offset;        // file offset
   const Char* filename;      // filename (NULL if unknown)
   Int         fnIdx;         // filename table index (-1 if unknown)
   UInt        dev;           // device
   UInt        ino;           // inode

   SegInfo*    symtab;        // symbol table
};

/* segment mapped from a file descriptor */
extern void VG_(map_fd_segment)  (Addr addr, SizeT len, UInt prot, UInt flags, 
				  Int fd, ULong off, const Char *filename);

/* segment mapped from a file */
extern void VG_(map_file_segment)(Addr addr, SizeT len, UInt prot, UInt flags, 
				  UInt dev, UInt ino, ULong off, const Char *filename);

/* simple segment */
extern void VG_(map_segment)     (Addr addr, SizeT len, UInt prot, UInt flags);

extern void VG_(unmap_range)   (Addr addr, SizeT len);
extern void VG_(mprotect_range)(Addr addr, SizeT len, UInt prot);
extern Addr VG_(find_map_space)(Addr base, SizeT len, Bool for_client);

/* Find the segment containing a, or NULL if none. */
extern Segment *VG_(find_segment)(Addr a);

/* a is an unmapped address (is checked).  Find the next segment 
   along in the address space, or NULL if none. */
extern Segment *VG_(find_segment_above_unmapped)(Addr a);

/* a is a mapped address (in a segment, is checked).  Find the
   next segment along. */
extern Segment *VG_(find_segment_above_mapped)(Addr a);

extern Bool VG_(seg_contains)(const Segment *s, Addr ptr, SizeT size);
extern Bool VG_(seg_overlaps)(const Segment *s, Addr ptr, SizeT size);

extern Segment *VG_(split_segment)(Addr a);

extern void VG_(pad_address_space)  (Addr start);
extern void VG_(unpad_address_space)(Addr start);

extern VGA_REGPARM(2)
       void VG_(unknown_SP_update) ( Addr old_SP, Addr new_SP );

///* Search /proc/self/maps for changes which aren't reflected in the
//   segment list */
//extern void VG_(sync_segments)(UInt flags);

/* Return string for prot */
extern const HChar *VG_(prot_str)(UInt prot);

extern Addr VG_(get_memory_from_mmap_for_client)
               (Addr base, SizeT len, UInt prot, UInt flags);

//extern void VG_(print_shadow_stats)();

/* Parses /proc/self/maps, calling `record_mapping' for each entry. */
extern 
void VG_(parse_procselfmaps) (
   void (*record_mapping)( Addr addr, SizeT len, UInt prot,
			   UInt dev, UInt ino, ULong foff,
                           const UChar *filename ) );

#endif   // __PUB_CORE_ASPACEMGR_H

/*--------------------------------------------------------------------*/
/*--- end                                     pub_core_aspacemgr.h ---*/
/*--------------------------------------------------------------------*/
