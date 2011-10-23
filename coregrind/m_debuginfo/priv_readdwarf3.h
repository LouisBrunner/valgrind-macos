
/*--------------------------------------------------------------------*/
/*--- Read DWARF3 ".debug_info" sections (DIE trees).              ---*/
/*---                                            priv_readdwarf3.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2011 OpenWorks LLP
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __PRIV_READDWARF3_H
#define __PRIV_READDWARF3_H


/* Read variables and types from DWARF3 ".debug_info" sections. */
void 
ML_(new_dwarf3_reader) (
   struct _DebugInfo* di,
   UChar* debug_info_img,   SizeT debug_info_sz,
   UChar* debug_abbv_img,   SizeT debug_abbv_sz,
   UChar* debug_line_img,   SizeT debug_line_sz,
   UChar* debug_str_img,    SizeT debug_str_sz,
   UChar* debug_ranges_img, SizeT debug_ranges_sz,
   UChar* debug_loc_img,    SizeT debug_loc_sz
);

#endif /* ndef __PRIV_READDWARF3_H */

/*--------------------------------------------------------------------*/
/*--- end                                        priv_readdwarf3.h ---*/
/*--------------------------------------------------------------------*/
