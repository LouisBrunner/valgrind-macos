
/*--------------------------------------------------------------------*/
/*--- Dumping core.                            pub_core_coredump.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2011 Julian Seward
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

#ifndef __PUB_CORE_COREDUMP_H
#define __PUB_CORE_COREDUMP_H

//--------------------------------------------------------------------
// PURPOSE: This module produces a core dump when asked.
//--------------------------------------------------------------------

extern void VG_(make_coredump) ( ThreadId tid, const vki_siginfo_t *si,
                                 UInt max_size );

#endif   // __PUB_CORE_COREDUMP_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
