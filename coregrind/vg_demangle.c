
/*--------------------------------------------------------------------*/
/*--- Demangling of C++ mangled names.                             ---*/
/*---                                                vg_demangle.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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

   VGP_PUSHCC(VgpDemangle);

   if (VG_(clo_demangle))
      demangled = VG_(cplus_demangle) ( orig, DMGL_ANSI | DMGL_PARAMS );

   if (demangled) {
      ADD_TO_RESULT(demangled, VG_(strlen)(demangled));
      VG_(arena_free) (VG_AR_DEMANGLE, demangled);
   } else {
      ADD_TO_RESULT(orig, VG_(strlen)(orig));
   }

   // 13 Mar 2005: We used to check here that the demangler wasn't leaking
   // by calling the (now-removed) function VG_(is_empty_arena)().  But,
   // very rarely (ie. I've heard of it twice in 3 years), the demangler
   // does leak.  But, we can't do much about it, and it's not a disaster,
   // so we just let it slide without aborting or telling the user.

   VGP_POPCC(VgpDemangle);
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
