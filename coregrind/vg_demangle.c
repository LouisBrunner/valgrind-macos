
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

void VG_(demangle) ( Char* orig, Char* result, Int result_size )
{
   Char* demangled = NULL;

   VGP_PUSHCC(VgpDemangle);

   if (VG_(clo_demangle))
      demangled = VG_(cplus_demangle) ( orig, DMGL_ANSI | DMGL_PARAMS );

   if (demangled) {
      VG_(strncpy_safely)(result, demangled, result_size);
      VG_(arena_free) (VG_AR_DEMANGLE, demangled);
   } else {
      VG_(strncpy_safely)(result, orig, result_size);
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
