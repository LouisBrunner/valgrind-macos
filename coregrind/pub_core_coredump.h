
/*--------------------------------------------------------------------*/
/*--- Dumping core.                            pub_core_coredump.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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

#ifndef __PUB_CORE_COREDUMP_H
#define __PUB_CORE_COREDUMP_H

#include "pub_core_basics.h"      // ThreadId
#include "pub_core_vki.h"         // vki_siginfo_t

//--------------------------------------------------------------------
// PURPOSE: This module produces a core dump when asked.
//--------------------------------------------------------------------

extern void VG_(make_coredump) ( ThreadId tid, const vki_siginfo_t *si,
                                 ULong max_size );

#endif   // __PUB_CORE_COREDUMP_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
