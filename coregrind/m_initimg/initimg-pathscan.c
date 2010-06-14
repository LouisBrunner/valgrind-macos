
/*--------------------------------------------------------------------*/
/*--- Startup: search PATH for an executable    initimg-pathscan.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Julian Seward
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

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_mallocfree.h"
#include "pub_core_machine.h"
#include "pub_core_ume.h"
#include "pub_core_options.h"
#include "pub_core_tooliface.h"       /* VG_TRACK */
#include "pub_core_threadstate.h"     /* ThreadArchState */
#include "pub_core_initimg.h"         /* self */

#include "priv_initimg_pathscan.h"


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
static Bool scan_colsep(char *colsep, Bool (*func)(const char *))
{
   char *cp, *entry;
   int end;

   if (colsep == NULL ||
       *colsep == '\0')
      return False;

   entry = cp = colsep;

   do {
      end = (*cp == '\0');

      if (*cp == ':' || *cp == '\0') {
	 char save = *cp;

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

/* Need a static copy because can't use dynamic mem allocation yet */
static HChar executable_name_in [VKI_PATH_MAX];
static HChar executable_name_out[VKI_PATH_MAX];

static Bool match_executable(const char *entry) 
{
   HChar buf[VG_(strlen)(entry) + VG_(strlen)(executable_name_in) + 3];

   /* empty PATH element means '.' */
   if (*entry == '\0')
      entry = ".";

   VG_(snprintf)(buf, sizeof(buf), "%s/%s", entry, executable_name_in);

   // Don't match directories
   if (VG_(is_dir)(buf))
      return False;

   // If we match an executable, we choose that immediately.  If we find a
   // matching non-executable we remember it but keep looking for an
   // matching executable later in the path.
   if (VG_(access)(buf, True/*r*/, False/*w*/, True/*x*/) == 0) {
      VG_(strncpy)( executable_name_out, buf, VKI_PATH_MAX-1 );
      executable_name_out[VKI_PATH_MAX-1] = 0;
      return True;      // Stop looking
   } else if (VG_(access)(buf, True/*r*/, False/*w*/, False/*x*/) == 0 
           && VG_STREQ(executable_name_out, "")) 
   {
      VG_(strncpy)( executable_name_out, buf, VKI_PATH_MAX-1 );
      executable_name_out[VKI_PATH_MAX-1] = 0;
      return False;     // Keep looking
   } else { 
      return False;     // Keep looking
   }
}

// Returns NULL if it wasn't found.
HChar* ML_(find_executable) ( const HChar* exec )
{
   vg_assert(NULL != exec);
   if (VG_(strchr)(exec, '/')) {
      // Has a '/' - use the name as is
      VG_(strncpy)( executable_name_out, exec, VKI_PATH_MAX-1 );
   } else {
      // No '/' - we need to search the path
      HChar* path;
      VG_(strncpy)( executable_name_in,  exec, VKI_PATH_MAX-1 );
      VG_(memset) ( executable_name_out, 0,    VKI_PATH_MAX );
      path = VG_(getenv)("PATH");
      scan_colsep(path, match_executable);
   }
   return VG_STREQ(executable_name_out, "") ? NULL : executable_name_out;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
