
/*--------------------------------------------------------------------*/
/*--- User-mode execve() for #! scripts.            m_ume_script.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward 
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

#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"    // VG_(exit), vg_assert
#include "pub_core_libcfile.h"      // VG_(close) et al
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_mallocfree.h"    // VG_(strdup)
#include "pub_core_ume.h"           // self

#include "priv_ume.h"

Bool VG_(match_script)(const void *hdr, Int len)
{
   const HChar* script = hdr;
   const HChar* end    = script + len;
   const HChar* interp = script + 2;

   // len < 4: need '#', '!', plus at least a '/' and one more char
   if (len < 4) return False;    
   if (0 != VG_(memcmp)(hdr, "#!", 2)) return False;

   // Find interpreter name, make sure it's an absolute path (starts with
   // '/') and has at least one more char.  First, skip over any space
   // between the #! and the start of the interpreter name
   while (interp < end && VG_(isspace)(*interp)) interp++;

   // overrun?
   if (interp >= end)   return False;  // can't find start of interp name

   // interp should now point at the /
   if (*interp != '/')  return False;  // absolute path only for interpreter

   // check for something plausible after the /
   interp++;
   if (interp >= end)   return False;
   if (VG_(isspace)(*interp)) return False;

   // Here we should get the full interpreter name and check it with
   // check_executable().  See the "EXEC FAILED" failure when running shell
   // for an example.

   return True;   // looks like a #! script
}


/* returns: 0 = success, non-0 is failure */
Int VG_(load_script)(Int fd, const HChar* name, ExeInfo* info)
{
   HChar  hdr[4096];
   Int    len = 4096;
   Int    eol;
   HChar* interp;
   HChar* end;
   HChar* cp;
   HChar* arg = NULL;
   SysRes res;

   // Read the first part of the file.
   res = VG_(pread)(fd, hdr, len, 0);
   if (sr_isError(res)) {
      VG_(close)(fd);
      return VKI_EACCES;
   } else {
      len = sr_Res(res);
   }

   vg_assert('#' == hdr[0] && '!' == hdr[1]);

   end    = hdr + len;
   interp = hdr + 2;
   while (interp < end && VG_(isspace)(*interp))
      interp++;

   vg_assert(*interp == '/');   /* absolute path only for interpreter */

   /* skip over interpreter name */
   for (cp = interp; cp < end && !VG_(isspace)(*cp); cp++)
      ;

   eol = (*cp == '\n');

   *cp++ = '\0';

   if (!eol && cp < end) {
      /* skip space before arg */
      while (cp < end && VG_(isspace)(*cp) && *cp != '\n')
         cp++;

      /* arg is from here to eol */
      arg = cp;
      while (cp < end && *cp != '\n')
         cp++;
      *cp = '\0';
   }
   
   info->interp_name = VG_(strdup)("ume.ls.1", interp);
   vg_assert(NULL != info->interp_name);
   if (arg != NULL && *arg != '\0') {
      info->interp_args = VG_(strdup)("ume.ls.2", arg);
      vg_assert(NULL != info->interp_args);
   }

   if (info->argv && info->argv[0] != NULL)
     info->argv[0] = name;

   VG_(args_the_exename) = name;

   if (0)
      VG_(printf)("#! script: interp_name=\"%s\" interp_args=\"%s\"\n",
                  info->interp_name, info->interp_args);

   return VG_(do_exec_inner)(interp, info);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
