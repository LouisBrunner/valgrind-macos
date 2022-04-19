
/*--------------------------------------------------------------------*/
/*--- Read DWARF3 ".debug_info" sections (DIE trees).              ---*/
/*---                                            priv_readdwarf3.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2017 OpenWorks LLP
      info@open-works.co.uk

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __PRIV_READDWARF3_H
#define __PRIV_READDWARF3_H

#include "pub_core_debuginfo.h"   // DebugInfo
#include "priv_image.h"           // DiSlice

/* Read variables and types from DWARF3 ".debug_info" sections. */
void 
ML_(new_dwarf3_reader) (
   DebugInfo* di,
   DiSlice escn_debug_info,      DiSlice escn_debug_types,
   DiSlice escn_debug_abbv,      DiSlice escn_debug_line,
   DiSlice escn_debug_str,       DiSlice escn_debug_ranges,
   DiSlice escn_debug_rnglists,  DiSlice escn_debug_loclists,
   DiSlice escn_debug_loc,       DiSlice escn_debug_info_alt,
   DiSlice escn_debug_abbv_alt,  DiSlice escn_debug_line_alt,
   DiSlice escn_debug_str_alt,   DiSlice escn_debug_line_str,
   DiSlice escn_debug_addr,      DiSlice escn_debug_str_offsets
);

#endif /* ndef __PRIV_READDWARF3_H */

/*--------------------------------------------------------------------*/
/*--- end                                        priv_readdwarf3.h ---*/
/*--------------------------------------------------------------------*/
