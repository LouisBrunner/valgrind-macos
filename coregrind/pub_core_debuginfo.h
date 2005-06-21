
/*--------------------------------------------------------------------*/
/*--- Debug info.                             pub_core_debuginfo.h ---*/
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

#ifndef __PUB_CORE_DEBUGINFO_H
#define __PUB_CORE_DEBUGINFO_H

//--------------------------------------------------------------------
// PURPOSE: This module deals with reading debug info and symbol tables
// to get file and function names, line numbers, variable types, and
// to help stack unwinding.
//
// And its internals are currently a mess.  Its interface is ugly, too.
//--------------------------------------------------------------------

#include "pub_tool_debuginfo.h"

typedef struct _Segment Segment;

extern Bool VG_(is_object_file)   ( const void *hdr );
extern SegInfo * VG_(read_seg_symbols) ( Segment *seg );
extern void VG_(seginfo_incref)   ( SegInfo * );
extern void VG_(seginfo_decref)   ( SegInfo *, Addr a );

extern Bool VG_(get_fnname_nodemangle)( Addr a, Char* fnname, Int n_fnname );

extern Addr VG_(reverse_search_one_symtab) ( const SegInfo* si, const Char* name );

extern Bool VG_(use_CFI_info) ( /*MOD*/Addr* ipP,
                                /*MOD*/Addr* spP,
                                /*MOD*/Addr* fpP,
                                Addr min_accessible,
                                Addr max_accessible );


#endif   // __PUB_CORE_DEBUGINFO_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
