
/*---------------------------------------------------------------------*/
/*--- Address Description, used e.g. to describe addresses involved ---*/
/*--- in race conditions, locks.                                    ---*/
/*---                                                hg_addrdescr.h ---*/
/*---------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2012 OpenWorks Ltd
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
*/

#ifndef __HG_ADDRDESCR_H
#define __HG_ADDRDESCR_H

/* Describe an address as best you can, for error messages or
   lock description, putting the result in ai.
   This might allocate some memory in ai, to be cleared with
   VG_(clear_addrinfo). */
extern void HG_(describe_addr) ( Addr a, /*OUT*/AddrInfo* ai );

/* Prints (using *print) the readable description of addr given in ai.
   "what" identifies the type pointed to by addr (e.g. a lock). */
extern void HG_(pp_addrdescr) (Bool xml, const HChar* what, Addr addr,
                               AddrInfo* ai,
                               void(*print)(const HChar *format, ...));

/* Get a readable description of addr, then print it using HG_(pp_addrdescr)
   using xml False and VG_(printf) to emit the characters.
   Returns True if a description was found/printed, False otherwise. */
extern Bool HG_(get_and_pp_addrdescr) (Addr a);

/* For error creation/address description:
   map 'data_addr' to a malloc'd chunk, if any.
   Slow linear search accelerated in some special cases normal hash
   search of the mallocmeta table. This is an abuse of the normal file
   structure since this is exported by hg_main.c, not hg_addrdesc.c.  Oh
   Well.  Returns True if found, False if not.  Zero-sized blocks are
   considered to contain the searched-for address if they equal that
   address. */
Bool HG_(mm_find_containing_block)( /*OUT*/ExeContext** where,
                                    /*OUT*/Addr*        payload,
                                    /*OUT*/SizeT*       szB,
                                    Addr                data_addr );


#endif /* ! __HG_ADDRDESCR_H */

/*--------------------------------------------------------------------*/
/*--- end                                           hg_addrdescr.h ---*/
/*--------------------------------------------------------------------*/
