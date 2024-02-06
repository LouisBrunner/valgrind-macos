
/*--------------------------------------------------------------------*/
/*--- Command line handling.                       m_commandline.c ---*/
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
#include "pub_core_vki.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_mallocfree.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_commandline.h" /* self */


/* Add a string to an expandable array of strings. */

static void add_string ( XArray* /* of HChar* */xa, HChar* str )
{
   (void) VG_(addToXA)( xa, (void*)(&str) );
}


/* Read the contents of .valgrindrc in 'dir' into malloc'd memory. */
// Note that we deliberately don't free the malloc'd memory.  See
// comment at call site.

static HChar* read_dot_valgrindrc ( const HChar* dir )
{
   struct vg_stat stat_buf;
   HChar* f_clo = NULL;
   const  HChar dot_valgrindrc[] = ".valgrindrc";

   vg_assert(dir != NULL);

   HChar filename[VG_(strlen)(dir) + 1 + VG_(strlen)(dot_valgrindrc) + 1];
   VG_(sprintf)(filename, "%s/%s", dir, dot_valgrindrc);

   SysRes fd = VG_(open)(filename, 0, VKI_S_IRUSR);
   if ( !sr_isError(fd) ) {
      Int res = VG_(fstat)( sr_Res(fd), &stat_buf );
      /* Ignore if not owned by the current user, or is not a regular file,
         or is world writable (CVE-2008-4865). */
      if (res == 0
          && stat_buf.uid == VG_(geteuid)()
          && VKI_S_ISREG(stat_buf.mode)
          && !(stat_buf.mode & VKI_S_IWOTH)) {
         if ( stat_buf.size > 0 ) {
            f_clo = VG_(malloc)("commandline.rdv.1", stat_buf.size+1);
            Int n = VG_(read)(sr_Res(fd), f_clo, stat_buf.size);
            if (n == -1) n = 0;
            vg_assert(n >= 0 && n <= stat_buf.size+1);
            f_clo[n] = '\0';
         }
      }
      else
         VG_(message)(Vg_UserMsg,
            "%s was not read as it is either not a regular file,\n"
            "    or is world writeable, or is not owned by the current user.\n",
            filename);

      VG_(close)(sr_Res(fd));
   }
   return f_clo;
}


// Add args from a string into VG_(args_for_valgrind), splitting the
// string at whitespace and adding each component as a separate arg.

static void add_args_from_string ( HChar* s )
{
   HChar* tmp;
   HChar* cp = s;
   int quoted = '\0';
   vg_assert(cp);
   while (True) {
      HChar* out;
      // We have alternating sequences: blanks, non-blanks, blanks...
      // copy the non-blanks sequences, and add terminating '\0'
      // deal with " or '-quoted strings properly.
      while (VG_(isspace)(*cp)) cp++;
      if (*cp == 0) break;
      tmp = out = cp;
      while ( (quoted || !VG_(isspace)(*cp)) && *cp) {
          if (*cp == quoted) {
              quoted = '\0';
          } else if (*cp == '\'' || *cp == '"') {
              quoted = *cp;
          } else {
              *out++ = *cp;
          }
          cp++;
      }
      if (out < cp) *out++ = '\0';
      if ( *cp != 0 ) *cp++ = '\0';       // terminate if not the last
      add_string( VG_(args_for_valgrind), tmp );
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
   in the first three categories.  They are not passed to child invocations
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

   XArray* /* of HChar* */ tmp_xarray;

   /* This function should be called once, at startup, and then never
      again. */
   vg_assert(!already_called);
   already_called = True;

   tmp_xarray = VG_(newXA)( VG_(malloc), "commandline.sua.1",
                            VG_(free), sizeof(HChar*) );

   vg_assert( ! VG_(args_for_valgrind) );
   VG_(args_for_valgrind)
      = VG_(newXA)( VG_(malloc), "commandline.sua.2",
                    VG_(free), sizeof(HChar*) );

   vg_assert( ! VG_(args_for_client) );
   VG_(args_for_client)
      = VG_(newXA)( VG_(malloc), "commandline.sua.3",
                    VG_(free), sizeof(HChar*) );

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
      /* mainly to allow overriding the regtest default */
      if (0 == VG_(strcmp)(argv[i], "--command-line-only=no")) {
         augment = True;
      }
      if (argv[i][0] != '-')
	break;
      add_string( tmp_xarray, argv[i] );
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
      add_string( VG_(args_for_client), argv[i] );
   }

   /* Get extra args from ~/.valgrindrc, $VALGRIND_OPTS and
      ./.valgrindrc into VG_(args_for_valgrind). */
   if (augment) {
      // read_dot_valgrindrc() allocates the return value with
      // VG_(malloc)().  We do not free f1_clo and f2_clo as they get
      // put into VG_(args_for_valgrind) and so must persist.
      HChar* home    = VG_(getenv)("HOME");
      HChar* f1_clo  = home ? read_dot_valgrindrc( home ) : NULL;
      HChar* env_clo = VG_(strdup)( "commandline.sua.4",
                                    VG_(getenv)(VALGRIND_OPTS) );
      HChar* f2_clo  = NULL;

      // Don't read ./.valgrindrc if "." is the same as "$HOME", else its
      // contents will be applied twice. (bug #142488)
      // Also don't try to read it if there is no cwd.
      if (home) {
         const HChar *cwd = VG_(get_startup_wd)();
         f2_clo = ( (cwd == NULL || VG_STREQ(home, cwd))
                       ? NULL : read_dot_valgrindrc(".") );
      }

      if (f1_clo)  add_args_from_string( f1_clo );
      if (env_clo) add_args_from_string( env_clo );
      if (f2_clo)  add_args_from_string( f2_clo );
   }

   /* .. and record how many extras we got. */
   VG_(args_for_valgrind_noexecpass) 
      = VG_(sizeXA)( VG_(args_for_valgrind) );

   /* Finally, copy tmp_xarray onto the end. */
   for (i = 0; i < VG_(sizeXA)( tmp_xarray ); i++)
      add_string( VG_(args_for_valgrind), 
                  * (HChar**)VG_(indexXA)( tmp_xarray, i ) );

   VG_(deleteXA)( tmp_xarray );
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
