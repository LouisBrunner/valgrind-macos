
/*--------------------------------------------------------------------*/
/*--- An example tool.                                   ex_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2002-2004 Nicholas Nethercote
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

#include "vg_skin.h"

static void SK_(pre_clo_init)()
{
   VG_(details_name)            ("Example");
   VG_(details_version)         ("0.0.1");
   VG_(details_description)     ("an example Valgrind tool");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2004, and put in the public domain, by Santa Claus.");
   VG_(details_bug_reports_to)  ("santa.claus@northpole.org");

   /* No needs, no core events to track */
}

void SK_(post_clo_init)(void)
{
}

UCodeBlock* SK_(instrument)(UCodeBlock* cb, Addr a)
{
    return cb;
}

void SK_(fini)(exitcode)
{
}

/* Does not use shadow memory */
VG_DETERMINE_INTERFACE_VERSION(SK_(pre_clo_init), 0)

/*--------------------------------------------------------------------*/
/*--- end                                                ex_main.c ---*/
/*--------------------------------------------------------------------*/
