
/*--------------------------------------------------------------------*/
/*--- Obtaining information about an address.  pub_tool_addrinfo.h ---*/
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

#ifndef __PUB_TOOL_ADDRINFO_H
#define __PUB_TOOL_ADDRINFO_H

#include "pub_tool_basics.h"   // VG_ macro

/*====================================================================*/
/*=== Obtaining information about an address                       ===*/
/*====================================================================*/

// Different kinds of blocks.
// Block_Mallocd is used by tools that maintain detailed information about
//   Client allocated heap blocks.
// Block_Freed is used by tools such as memcheck that maintain a 'quarantine' 
//   list of blocks freed by the Client but not yet physically freed.
// Block_MempoolChunk and Block_UserG are used for mempool or user defined heap
//   blocks.
// Block_ClientArenaMallocd and Block_ClientArenaFree are used when the tool
//   replaces the malloc/free/... functions but does not maintain detailed
//   information about Client allocated heap blocks.
// Block_ValgrindArenaMallocd and Block_ValgrindArenaFree are used for heap
//   blocks of Valgrind internal heap.
typedef enum {
   Block_Mallocd = 111,
   Block_Freed,
   Block_MempoolChunk,
   Block_UserG,
   Block_ClientArenaMallocd,
   Block_ClientArenaFree,
   Block_ValgrindArenaMallocd,
   Block_ValgrindArenaFree,
} BlockKind;

/* ------------------ Addresses -------------------- */

/* The classification of a faulting address. */
typedef 
   enum { 
      Addr_Undescribed, // as-yet unclassified
      Addr_Unknown,     // classification yielded nothing useful
      Addr_Block,       // in malloc'd/free'd block
      Addr_Stack,       // on a thread's stack       
      Addr_DataSym,     // in a global data sym
      Addr_Variable,    // variable described by the debug info
      Addr_SectKind     // last-ditch classification attempt
   }
   AddrTag;

typedef
   struct _AddrInfo
   AddrInfo;

struct _AddrInfo {
   AddrTag tag;
   union {
      // As-yet unclassified.
      struct { } Undescribed;

      // On a stack.
      struct {
         ThreadId tid;        // Which thread's stack?
      } Stack;

      // This covers heap blocks (normal and from mempools), user-defined
      // blocks and Arena blocks.
      struct {
         BlockKind   block_kind;
         const HChar* block_desc;   // "block","mempool","user-defined",arena
         SizeT       block_szB;
         PtrdiffT    rwoffset;
         ExeContext* allocated_at;  // might be null_ExeContext.
         ExeContext* freed_at;      // might be null_ExeContext.
      } Block;

      // In a global .data symbol.  This holds the first 127 chars of
      // the variable's name (zero terminated), plus a (memory) offset.
      struct {
         HChar    name[128];
         PtrdiffT offset;
      } DataSym;

      // Is described by Dwarf debug info.  XArray*s of HChar.
      struct {
         XArray* /* of HChar */ descr1;
         XArray* /* of HChar */ descr2;
      } Variable;

      // Could only narrow it down to be the PLT/GOT/etc of a given
      // object.  Better than nothing, perhaps.
      struct {
         HChar      objname[128];
         VgSectKind kind;
      } SectKind;

      // Classification yielded nothing useful.
      struct { } Unknown;

   } Addr;
};


/* Describe an address as best you can, putting the result in ai.
   On entry, ai->tag must be equal to Addr_Undescribed.
   This might allocate some memory, that can be cleared with
   VG_(clear_addrinfo). */
extern void VG_(describe_addr) ( Addr a, /*OUT*/AddrInfo* ai );

extern void VG_(clear_addrinfo) ( AddrInfo* ai);

/* Prints the AddrInfo ai describing a. */
extern void VG_(pp_addrinfo) ( Addr a, AddrInfo* ai );

/* Same as VG_(pp_addrinfo) but provides some memcheck specific behaviour:
   * maybe_gcc indicates Addr a was just below the stack ptr when the error
     with a was encountered.
   * the message for Unknown tag is slightly different, as memcheck
     has a recently freed list. */
extern void VG_(pp_addrinfo_mc) ( Addr a, AddrInfo* ai, Bool maybe_gcc );

#endif   // __PUB_TOOL_ADDRINFO_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
