
/*--------------------------------------------------------------------*/
/*--- Services layered on top of m_aspacemgr.  pub_tool_aspacehl.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2009-2010 Julian Seward
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

#ifndef __PUB_TOOL_ASPACEHL_H
#define __PUB_TOOL_ASPACEHL_H

// Extract from aspacem a vector of the current segment start
// addresses.  The vector is dynamically allocated and should be freed
// by the caller when done.  REQUIRES m_mallocfree to be running.
// Writes the number of addresses required into *n_acquired.
extern Addr* VG_(get_segment_starts) ( /*OUT*/Int* n_acquired );

#endif   // __PUB_TOOL_ASPACEHL_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
