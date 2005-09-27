
/*--------------------------------------------------------------------*/
/*--- Command line handling.                       m_commandline.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_mallocfree.h"
#include "pub_core_clientstate.h"
#include "pub_core_commandline.h"


/* Add a string to an expandable array of strings. */

static void add_string ( XArrayStrings* xa, HChar* str )
{
   Int     i;
   HChar** strs2;
   vg_assert(xa->used >= 0);
   vg_assert(xa->size >= 0);
   vg_assert(xa->used <= xa->size);
   if (xa->strs == NULL) vg_assert(xa->size == 0);

   if (xa->used == xa->size) {
      xa->size = xa->size==0 ? 2 : 2*xa->size;
      strs2 = VG_(malloc)( xa->size * sizeof(HChar*) );
      for (i = 0; i < xa->used; i++)
         strs2[i] = xa->strs[i];
      if (xa->strs) 
         VG_(free)(xa->strs);
      xa->strs = strs2;
   }
   vg_assert(xa->used < xa->size);
   xa->strs[xa->used++] = str;
}


/* Read the contents of .valgrindrc in 'dir' into malloc'd memory. */
// Note that we deliberately don't free the malloc'd memory.  See
// comment at call site.

static HChar* read_dot_valgrindrc ( HChar* dir )
{
   Int    n;
   SysRes fd;
   Int    size;
   HChar* f_clo = NULL;
   HChar  filename[VKI_PATH_MAX];

   VG_(snprintf)(filename, VKI_PATH_MAX, "%s/.valgrindrc", 
                           ( NULL == dir ? "" : dir ) );
   fd = VG_(open)(filename, 0, VKI_S_IRUSR);
   if ( !fd.isError ) {
      size = VG_(fsize)(fd.val);
      if (size > 0) {
         f_clo = VG_(malloc)(size+1);
         vg_assert(f_clo);
         n = VG_(read)(fd.val, f_clo, size);
         if (n == -1) n = 0;
         vg_assert(n >= 0 && n <= size+1);
         f_clo[n] = '\0';
      }
      VG_(close)(fd.val);
   }
   return f_clo;
}


// Add args from a string into VG_(args_for_valgrind), splitting the
// string at whitespace and adding each component as a separate arg.

static void add_args_from_string ( HChar* s )
{
   HChar* tmp;
   HChar* cp = s;
   vg_assert(cp);
   while (True) {
      // We have alternating sequences: blanks, non-blanks, blanks...
      // copy the non-blanks sequences, and add terminating '\0'
      while (VG_(isspace)(*cp)) cp++;
      if (*cp == 0) break;
      tmp = cp;
      while ( !VG_(isspace)(*cp) && *cp != 0 ) cp++;
      if ( *cp != 0 ) *cp++ = '\0';       // terminate if not the last
      add_string( &VG_(args_for_valgrind), tmp );
   }
}


/* Split up the args presented by the launcher to m_main.main(), and
   park them in VG_(args_for_client) and VG_(args_for_valgrind).

   The resulting arg list is the concatenation of the following:
   - contents of ~/.valgrindrc
   - contents of $VALGRIND_OPTS
   - contents of ./.valgrindrc
   - args from the command line
   in the stated order.

   VG_(args_for_valgrind_noexecpass) is set to be the number of items
   in the first three categories.  They are not passed to child invokations
   at exec, whereas the last group is.

   If the last group contains --command-line-only=yes, then the 
   first three groups are left empty.

   Scheme: first examine the last group (the supplied argc/argv).
   It should look like this.

      args-for-v  exe_name  args-for-c

   args-for-v are taken until either they don't start with '-' or
   a "--" is seen.

   The exe name and args-for-c are recorded without further ado.
   Note that args-for-c[0] is the first real arg for the client, not
   its executable name.

   args-for-v are then copied into tmp_xarray.

   if args-for-v does not include --command-line-only=yes:
      contents of ~/.valgrindrc, $VALGRIND_OPTS and ./.valgrindrc
      are copied into VG_(args_for_valgrind).
   else
      VG_(args_for_valgrind) is made empty.

   Finally, tmp_xarray is copied onto the end of VG_(args_for_valgrind).
*/

void VG_(split_up_argv)( Int argc, HChar** argv )
{
          Int  i;
          Bool augment = True;
   static Bool already_called = False;

   XArrayStrings tmp_xarray = {0,0,NULL};

   /* This function should be called once, at startup, and then never
      again. */
   vg_assert(!already_called);
   already_called = True;

   /* Collect up the args-for-V. */
   i = 1; /* skip the exe (stage2) name. */
   for (; i < argc; i++) {
      vg_assert(argv[i]);
      if (0 == VG_(strcmp)(argv[i], "--")) {
         i++;
         break;
      }
      if (0 == VG_(strcmp)(argv[i], "--command-line-only=yes"))
         augment = False;
      if (argv[i][0] != '-')
	break;
      add_string( &tmp_xarray, argv[i] );
   }

   /* Should now be looking at the exe name. */
   if (i < argc) {
      vg_assert(argv[i]);
      VG_(args_the_exename) = argv[i];
      i++;
   }

   /* The rest are args for the client. */
   for (; i < argc; i++) {
      vg_assert(argv[i]);
      add_string( &VG_(args_for_client), argv[i] );
   }

   VG_(args_for_valgrind).size = 0;
   VG_(args_for_valgrind).used = 0;
   VG_(args_for_valgrind).strs = NULL;

   /* Get extra args from ~/.valgrindrc, $VALGRIND_OPTS and
      ./.valgrindrc into VG_(args_for_valgrind). */
   if (augment) {
      // read_dot_valgrindrc() allocates the return value with
      // VG_(malloc)().  We do not free f1_clo and f2_clo as they get
      // put into VG_(args_for_valgrind) and so must persist.
      HChar* f1_clo  = read_dot_valgrindrc( VG_(getenv)("HOME") );
      HChar* env_clo = VG_(strdup)( VG_(getenv)(VALGRIND_OPTS) );
      HChar* f2_clo  = read_dot_valgrindrc(".");

      if (f1_clo)  add_args_from_string( f1_clo );
      if (env_clo) add_args_from_string( env_clo );
      if (f2_clo)  add_args_from_string( f2_clo );
   }

   /* .. and record how many extras we got. */
   VG_(args_for_valgrind_noexecpass) = VG_(args_for_valgrind).used;

   /* Finally, copy tmp_xarray onto the end. */
   for (i = 0; i < tmp_xarray.used; i++)
      add_string( &VG_(args_for_valgrind), tmp_xarray.strs[i] );

   if (tmp_xarray.strs)
      VG_(free)(tmp_xarray.strs);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
