
/*--------------------------------------------------------------------*/
/*--- Dummy profiling machinery -- overridden by tools when they   ---*/
/*--- want profiling.                                              ---*/
/*---                                           vg_dummy_profile.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2004 Julian Seward 
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

#include "core.h"

static void vgp_die(void)
{
   VG_(printf)(
      "\nProfiling error:\n"
      "  The --profile=yes option was specified, but the tool\n"
      "  wasn't built for profiling.  #include \"vg_profile.c\"\n"
      "  into the tool and rebuild to allow profiling.\n\n");
   VG_(exit)(1);
}

void VGP_(register_profile_event) ( Int n, Char* name )
{
}

void VGP_(init_profiling) ( void )
{
   vgp_die();
}

void VGP_(done_profiling) ( void )
{
   VG_(core_panic)("done_profiling(), but not compiled for profiling??");
}

void VGP_(pushcc) ( UInt cc )
{
   vgp_die();
}

void VGP_(popcc) ( UInt cc )
{
   vgp_die();
}

/*--------------------------------------------------------------------*/
/*--- end                                       vg_dummy_profile.c ---*/
/*--------------------------------------------------------------------*/
