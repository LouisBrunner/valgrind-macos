
/*--------------------------------------------------------------------*/
/*--- Debug info.                             pub_core_debuginfo.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Julian Seward
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

#ifndef __PUB_CORE_DEBUGINFO_H
#define __PUB_CORE_DEBUGINFO_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with reading debug info and symbol tables
// to get file and function names, line numbers, variable types, and
// to help stack unwinding.
//--------------------------------------------------------------------

#include "pub_tool_debuginfo.h"

/* LINUX: Notify the debuginfo system about a new mapping, or the
   disappearance of such, or a permissions change on an existing
   mapping.  This is the way new debug information gets loaded.  If
   allow_SkFileV is True, it will try load debug info if the mapping
   at 'a' belongs to Valgrind; whereas normally (False) it will not do
   that.  This allows us to carefully control when the thing will read
   symbols from the Valgrind executable itself. */
#if defined(VGO_linux)
extern void VG_(di_notify_mmap)( Addr a, Bool allow_SkFileV );

extern void VG_(di_notify_munmap)( Addr a, SizeT len );

extern void VG_(di_notify_mprotect)( Addr a, SizeT len, UInt prot );
#endif

#if defined(VGO_aix5)
/* AIX5: Very similar, except packaged more neatly.  The supplied
   parameters describe a code segment and its associated data segment,
   that have recently been mapped in -- so we need to read debug info
   for it -- or conversely, have recently been dumped, in which case
   the relevant debug info has to be unloaded. */
extern void VG_(di_aix5_notify_segchange)( 
               Addr   code_start,
               Word   code_len,
               Addr   data_start,
               Word   data_len,
               UChar* file_name,
               UChar* mem_name,
               Bool   is_mainexe,
               Bool   acquire
            );
#endif

extern Bool VG_(get_fnname_nodemangle)( Addr a, 
                                        Char* fnname, Int n_fnname );

/* Use DWARF2/3 CFA information to do one step of stack unwinding. */
extern Bool VG_(use_CF_info) ( /*MOD*/Addr* ipP,
                               /*MOD*/Addr* spP,
                               /*MOD*/Addr* fpP,
                               Addr min_accessible,
                               Addr max_accessible );

/* ppc64-linux only: find the TOC pointer (R2 value) that should be in
   force at the entry point address of the function containing
   guest_code_addr.  Returns 0 if not known. */
extern Addr VG_(get_tocptr) ( Addr guest_code_addr );

/* This is only available to core... don't demangle C++ names, but do
   do Z-demangling, match anywhere in function, and don't show
   offsets. */
extern
Bool VG_(get_fnname_Z_demangle_only) ( Addr a, Char* buf, Int nbuf );

/* Map a function name to its entry point and toc pointer.  Is done by
   sequential search of all symbol tables, so is very slow.  To
   mitigate the worst performance effects, you may specify a soname
   pattern, and only objects matching that pattern are searched.
   Therefore specify "*" to search all the objects.  On TOC-afflicted
   platforms, a symbol is deemed to be found only if it has a nonzero
   TOC pointer.  */
extern
Bool VG_(lookup_symbol_SLOW)(UChar* sopatt, UChar* name, Addr* pEnt, Addr* pToc);

#endif   // __PUB_CORE_DEBUGINFO_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
