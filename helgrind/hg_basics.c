
/*--------------------------------------------------------------------*/
/*--- Basic definitions for all of Helgrind.                       ---*/
/*---                                                  hg_basics.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2017 OpenWorks Ltd
      info@open-works.co.uk

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_tool_basics.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_threadstate.h"

#include "hg_basics.h"            /* self */


/*----------------------------------------------------------------*/
/*--- Very basic stuff                                         ---*/
/*----------------------------------------------------------------*/

void* HG_(zalloc) ( const HChar* cc, SizeT n )
{
   void* p;
   tl_assert(n > 0);
   p = VG_(malloc)( cc, n );
   VG_(memset)(p, 0, n);
   return p;
}

void HG_(free) ( void* p )
{
   tl_assert(p);
   VG_(free)(p);
}

HChar* HG_(strdup) ( const HChar* cc, const HChar* s )
{
   return VG_(strdup)( cc, s );
}


/*----------------------------------------------------------------*/
/*--- Command line options                                     ---*/
/*----------------------------------------------------------------*/

/* Description of these flags is in hg_basics.h. */

Bool  HG_(clo_track_lockorders) = True;

Bool  HG_(clo_cmp_race_err_addrs) = False;

UWord HG_(clo_history_level) = 2;

#if (defined(VGA_x86) || defined(VGA_amd64)) && defined(VGO_linux)
// Set to true on setup where it was (reasonably) validated.
Bool  HG_(clo_delta_stacktrace) = True;
#else
Bool  HG_(clo_delta_stacktrace) = False;
#endif

UWord HG_(clo_conflict_cache_size) = 2000000;

UWord HG_(clo_sanity_flags) = 0;

Bool  HG_(clo_free_is_write) = False;

UWord HG_(clo_vts_pruning) = 1;

Bool  HG_(clo_check_stack_refs) = True;

/*--------------------------------------------------------------------*/
/*--- end                                              hg_basics.c ---*/
/*--------------------------------------------------------------------*/
