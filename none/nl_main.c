
/*--------------------------------------------------------------------*/
/*--- Nulgrind: The null tool.                           nl_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Nulgrind, the simplest possible Valgrind tool,
   which does nothing.

   Copyright (C) 2002-2005 Nicholas Nethercote
      njn25@cam.ac.uk

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

#include "tool.h"

void TL_(pre_clo_init)(void)
{
   VG_(details_name)            ("Nulgrind");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a binary JIT-compiler");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2005, and GNU GPL'd, by Nicholas Nethercote.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   /* No needs, no core events to track */
}

void TL_(post_clo_init)(void)
{
}

IRBB* TL_(instrument)(IRBB* bb, VexGuestLayout* layout, 
                      IRType gWordTy, IRType hWordTy)
{
    return bb;
}

void TL_(fini)(Int exitcode)
{
}

VG_DETERMINE_INTERFACE_VERSION(TL_(pre_clo_init), 0)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
