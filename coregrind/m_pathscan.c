
/*--------------------------------------------------------------------*/
/*--- search PATH for an executable                   m_pathscan.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_pathscan.h"         /* self */


/*====================================================================*/
/*=== Find executable                                              ===*/
/*====================================================================*/

/* Scan a colon-separated list, and call a function on each element.
   The string must be mutable, because we insert a temporary '\0', but
   the string will end up unmodified.  (*func) should return True if it
   doesn't need to see any more.

   This routine will return True if (*func) returns True and False if
   it reaches the end of the list without that happening.
*/
static Bool scan_colsep(HChar *colsep, Bool (*func)(const HChar *))
{
   HChar *cp, *entry;
   int end;

   if (colsep == NULL ||
       *colsep == '\0')
      return False;

   entry = cp = colsep;

   do {
      end = (*cp == '\0');

      if (*cp == ':' || *cp == '\0') {
	 HChar save = *cp;

	 *cp = '\0';
	 if ((*func)(entry)) {
            *cp = save;
	    return True;
         }
	 *cp = save;
	 entry = cp+1;
      }
      cp++;
   } while(!end);

   return False;
}


static const HChar *executable_name_in;
static HChar *executable_name_out;

static Bool match_executable(const HChar *entry) 
{
   /* empty ENTRY element means '.' */
   if (*entry == '\0')
      entry = ".";

   HChar buf[VG_(strlen)(entry) + 1 + VG_(strlen)(executable_name_in) + 1];

   VG_(sprintf)(buf, "%s/%s", entry, executable_name_in);

   // Don't match directories
   if (VG_(is_dir)(buf))
      return False;

   // If we match an executable, we choose that immediately.  If we find a
   // matching non-executable we remember it but keep looking for an
   // matching executable later in the path.
   if (VG_(access)(buf, True/*r*/, False/*w*/, True/*x*/) == 0) {
      VG_(free)(executable_name_out);
      executable_name_out = VG_(strdup)("match_executable", buf);
      return True;      // Stop looking
   } else if (VG_(access)(buf, True/*r*/, False/*w*/, False/*x*/) == 0 
              && executable_name_out == NULL)
   {
      executable_name_out = VG_(strdup)("match_executable", buf);
      return False;     // Keep looking
   } else { 
      return False;     // Keep looking
   }
}

// Returns NULL if it wasn't found.
const HChar* VG_(find_executable) ( const HChar* exec )
{
   vg_assert(NULL != exec);

   if (VG_(strchr)(exec, '/')) {
      // Has a '/' - use the name as is even if exec is a directory.
      // The reason is that we get a better error message this way:
      //   valgrind  ./foo
      //   valgrind: ./foo: is a directory
      return exec;
   }

   // No '/' - we need to search the path
   HChar* path = VG_(getenv)("PATH");

   VG_(free)(executable_name_out);

   executable_name_in  = exec;
   executable_name_out = NULL;
   scan_colsep(path, match_executable);

   return executable_name_out;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
