/*--------------------------------------------------------------------*/
/*--- Swift demangler                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2024-2024 Louis Brunner <louis.brunner.fr@gmail.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "vg_libciface.h"

#include "ansidecl.h"
#include "demangle.h"
#include "safe-ctype.h"

char* swift_demangle (const char *mangled, int options)
{
  UInt prefix_length = 0;
  Bool legacy = False;

  if (VG_(strstr)(mangled, "_T0") == mangled || VG_(strstr)(mangled, "_$S") == mangled || VG_(strstr)(mangled, "_$s") == mangled) {
    prefix_length = 3;
  }
  if (VG_(strstr)(mangled, "_T") == mangled) {
    prefix_length = 2;
    legacy = True;
  }
  if (VG_(strstr)(mangled, "$S") == mangled || VG_(strstr)(mangled, "$s") == mangled) {
    prefix_length = 2;
  }

  if (prefix_length == 0) {
    return NULL;
  }

  mangled += prefix_length;

  return NULL;
}
