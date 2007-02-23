
/*--------------------------------------------------------------------*/
/*--- Read DWARF1/2/3 debug info.                 priv_readdwarf.h ---*/
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

#ifndef __PRIV_READDWARF_H
#define __PRIV_READDWARF_H

/*
   Stabs reader greatly improved by Nick Nethercote, Apr 02.
   This module was also extensively hacked on by Jeremy Fitzhardinge
   and Tom Hughes.
*/


/* --------------------
   DWARF2 reader
   -------------------- */
extern
void ML_(read_debuginfo_dwarf2)
        ( struct _SegInfo* si, OffT debug_offset,
          UChar* debuginfo,   Int debug_info_sz,  /* .debug_info */
          UChar* debugabbrev,                     /* .debug_abbrev */
          UChar* debugline,   Int debug_line_sz,  /* .debug_line */
          UChar* debugstr );

/* --------------------
   DWARF1 reader
   -------------------- */
extern
void ML_(read_debuginfo_dwarf1) ( struct _SegInfo* si,
                                  UChar* dwarf1d, Int dwarf1d_sz,
                                  UChar* dwarf1l, Int dwarf1l_sz );

/* --------------------
   CFI reader
   -------------------- */
extern
void ML_(read_callframe_info_dwarf3)
    ( /*OUT*/struct _SegInfo* si, 
      UChar* ehframe, Int ehframe_sz, Addr ehframe_addr );


#endif /* ndef __PRIV_READDWARF_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
