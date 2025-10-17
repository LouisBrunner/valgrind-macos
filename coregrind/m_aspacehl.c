
/*--------------------------------------------------------------------*/
/*--- Services layered on top of m_aspacemgr.         m_aspacehl.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2017 Julian Seward
      jseward@acm.org

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

#include "pub_core_basics.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_mallocfree.h"

#include "pub_core_aspacehl.h"

// Extract from aspacem a vector of the current segment start
// addresses.  The vector is dynamically allocated and should be freed
// by the caller when done.  REQUIRES m_mallocfree to be running.
// Writes the number of addresses required into *n_acquired.
// Only those segments are considered whose kind matches any of the kinds
// given in KIND_MASK.
Addr* VG_(get_segment_starts) ( UInt kind_mask, /*OUT*/Int* n_acquired )
{
   Addr* starts;
   Int   n_starts, r = 0;

   n_starts = 1;
   while (True) {
      starts = VG_(malloc)( "main.gss.1", n_starts * sizeof(Addr) );
      r = VG_(am_get_segment_starts)( kind_mask, starts, n_starts );
      if (r >= 0)
         break;
      VG_(free)(starts);
      n_starts *= 2;
   }

   *n_acquired = r;
   return starts;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
