
/*--------------------------------------------------------------------*/
/*--- Demangling of C++ mangled names.                             ---*/
/*---                                                vg_demangle.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org
      Julian_Seward@muraroa.demon.co.uk

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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"
#include "demangle.h"

#define ADD_TO_RESULT(zzstr,zzn)                   \
{                                                  \
   Char* zz = (zzstr);                             \
   Int nn = (zzn);                                 \
   Int ii;                                         \
   for (ii = 0; ii < nn; ii++) {                   \
      result[n_result] = zz[ii];                   \
      if (n_result < result_size-1) n_result++;    \
      result[n_result] = 0;                        \
   }                                               \
}

void VG_(demangle) ( Char* orig, Char* result, Int result_size )
{
   Int   n_result  = 0;
   Char* demangled = NULL;

   if (VG_(clo_demangle))
      demangled = VG_(cplus_demangle) ( orig, DMGL_ANSI | DMGL_PARAMS );

   if (demangled) {
      ADD_TO_RESULT(demangled, VG_(strlen)(demangled));
      VG_(free) (VG_AR_DEMANGLE, demangled);
   } else {
      ADD_TO_RESULT(orig, VG_(strlen)(orig));
   }

   /* Check that the demangler isn't leaking. */
   /* 15 Feb 02: if this assertion fails, this is not a disaster.
      Comment it out, and let me know.  (jseward@acm.org). */
   vg_assert(VG_(is_empty_arena)(VG_AR_DEMANGLE));

   /* VG_(show_all_arena_stats)(); */
}


/*--------------------------------------------------------------------*/
/*--- end                                            vg_demangle.c ---*/
/*--------------------------------------------------------------------*/
