
/*--------------------------------------------------------------------*/
/*--- For printing superblock profiles        pub_core_sbprofile.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2012-2017 Mozilla Foundation

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

/* Contributed by Julian Seward <jseward@acm.org> */

#ifndef __PUB_CORE_SBPROFILE_H
#define __PUB_CORE_SBPROFILE_H

#include "pub_core_basics.h"   // VG_ macro

/* Get and print a profile.  Also, zero out the counters so that if we
   call it again later, the second call will only show new work done
   since the first call.  ecs_done == 0 is taken to mean this is a
   run-end profile. */
void VG_(get_and_show_SB_profile) ( ULong ecs_done );

#endif   // __PUB_CORE_SBPROFILE_H

/*--------------------------------------------------------------------*/
/*--- end                                     pub_core_sbprofile.h ---*/
/*--------------------------------------------------------------------*/
