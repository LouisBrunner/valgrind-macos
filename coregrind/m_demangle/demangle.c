
/*--------------------------------------------------------------------*/
/*--- Demangling of C++ mangled names.                  demangle.c ---*/
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
#include "pub_core_demangle.h"
#include "pub_core_libcbase.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_libcassert.h"
#include "demangle.h"
#include "pub_core_libcprint.h"

/* The demangler's job is to take a raw symbol name and turn it into
   something a Human Bean can understand.  There are two levels of
   mangling.

   1. First, C++ names are mangled by the compiler.  So we'll have to
      undo that.

   2. Optionally, in relatively rare cases, the resulting name is then
      itself encoded using Z-escaping (see pub_core_redir.h) so as to
      become part of a redirect-specification.

   Therefore, VG_(demangle) first tries to undo (2).  If successful,
   the soname part is discarded (humans don't want to see that).
   Then, it tries to undo (1) (using demangling code from GNU/FSF).

   Finally, change the name of all symbols which are known to be
   functions below main() to "(below main)".  This helps reduce
   variability of stack traces, something which has been a problem for
   the testsuite for a long time.

   --------
   If do_cxx_demangle == True, does all the above stages:
   - undo (2) [Z-encoding]
   - undo (1) [C++ mangling]
   - do the below-main hack

   If do_cxx_demangle == False, the middle stage is skipped:
   - undo (2) [Z-encoding]
   - do the below-main hack
*/

/* This is the main, standard demangler entry point. */

void VG_(demangle) ( Bool do_cxx_demangle, 
                     Char* orig, Char* result, Int result_size )
{
#  define N_ZBUF 4096
   HChar* demangled = NULL;
   HChar z_demangled[N_ZBUF];

   if (!VG_(clo_demangle)) {
      VG_(strncpy_safely)(result, orig, result_size);
      return;
   }

   /* Undo (2) */
   /* Demangling was requested.  First see if it's a Z-mangled
      intercept specification.  The fastest way is just to attempt a
      Z-demangling (with NULL soname buffer, since we're not
      interested in that). */
   if (VG_(maybe_Z_demangle)( orig, NULL,0,/*soname*/
                              z_demangled, N_ZBUF, NULL)) {
      orig = z_demangled;
   }

   /* Possibly undo (1) */
   if (do_cxx_demangle)
      demangled = ML_(cplus_demangle) ( orig, DMGL_ANSI | DMGL_PARAMS );
   else
      demangled = NULL;

   if (demangled) {
      VG_(strncpy_safely)(result, demangled, result_size);
      VG_(arena_free) (VG_AR_DEMANGLE, demangled);
   } else {
      VG_(strncpy_safely)(result, orig, result_size);
   }

   /* Do the below-main hack */
   // 13 Mar 2005: We used to check here that the demangler wasn't leaking
   // by calling the (now-removed) function VG_(is_empty_arena)().  But,
   // very rarely (ie. I've heard of it twice in 3 years), the demangler
   // does leak.  But, we can't do much about it, and it's not a disaster,
   // so we just let it slide without aborting or telling the user.

   // Finally, to reduce the endless nuisance of multiple different names 
   // for "the frame below main()" screwing up the testsuite, change all
   // known incarnations of said into a single name, "(below main)".
   if (0==VG_(strcmp)("__libc_start_main", result)
       || 0==VG_(strcmp)("generic_start_main", result)
       || 0==VG_(strcmp)("__start", result)) /* on AIX */
      VG_(strncpy_safely)(result, "(below main)", 13);

#  undef N_ZBUF
}


/*------------------------------------------------------------*/
/*--- DEMANGLE Z-ENCODED NAMES                             ---*/
/*------------------------------------------------------------*/

/* Demangle a Z-encoded name as described in pub_tool_redir.h. 
   Z-encoded names are used by Valgrind for doing function 
   interception/wrapping.

   Demangle 'sym' into its soname and fnname parts, putting them in
   the specified buffers.  Returns a Bool indicating whether the
   demangled failed or not.  A failure can occur because the prefix
   isn't recognised, the internal Z-escaping is wrong, or because one
   or the other (or both) of the output buffers becomes full.  Passing
   'so' as NULL is acceptable if the caller is only interested in the
   function name part. */

Bool VG_(maybe_Z_demangle) ( const HChar* sym, 
                             /*OUT*/HChar* so, Int soLen,
                             /*OUT*/HChar* fn, Int fnLen,
                             /*OUT*/Bool* isWrap )
{
#  define EMITSO(ch)                           \
      do {                                     \
         if (so) {                             \
            if (soi >= soLen) {                \
               so[soLen-1] = 0; oflow = True;  \
            } else {                           \
               so[soi++] = ch; so[soi] = 0;    \
            }                                  \
         }                                     \
      } while (0)
#  define EMITFN(ch)                           \
      do {                                     \
         if (fni >= fnLen) {                   \
            fn[fnLen-1] = 0; oflow = True;     \
         } else {                              \
            fn[fni++] = ch; fn[fni] = 0;       \
         }                                     \
      } while (0)

   Bool error, oflow, valid, fn_is_encoded;
   Int  soi, fni, i;

   vg_assert(soLen > 0 || (soLen == 0 && so == NULL));
   vg_assert(fnLen > 0);
   error = False;
   oflow = False;
   soi = 0;
   fni = 0;

   valid =     sym[0] == '_'
           &&  sym[1] == 'v'
           &&  sym[2] == 'g'
           && (sym[3] == 'r' || sym[3] == 'w' || sym[3] == 'n')
           &&  sym[4] == 'Z'
           && (sym[5] == 'Z' || sym[5] == 'U')
           &&  sym[6] == '_';
   if (!valid)
      return False;

   fn_is_encoded = sym[5] == 'Z';

   if (isWrap)
      *isWrap = sym[3] == 'w';

   /* Now scan the Z-encoded soname. */
   i = 7;
   while (True) {

      if (sym[i] == '_')
      /* Found the delimiter.  Move on to the fnname loop. */
         break;

      if (sym[i] == 0) {
         error = True;
         goto out;
      }

      if (sym[i] != 'Z') {
         EMITSO(sym[i]);
         i++;
         continue;
      }

      /* We've got a Z-escape. */
      i++;
      switch (sym[i]) {
         case 'a': EMITSO('*'); break;
         case 'p': EMITSO('+'); break;
         case 'c': EMITSO(':'); break;
         case 'd': EMITSO('.'); break;
         case 'u': EMITSO('_'); break;
         case 'h': EMITSO('-'); break;
         case 's': EMITSO(' '); break;
         case 'Z': EMITSO('Z'); break;
         case 'A': EMITSO('@'); break;
         case 'L': EMITSO('('); break;
         case 'R': EMITSO(')'); break;
         default: error = True; goto out;
      }
      i++;
   }

   vg_assert(sym[i] == '_');
   i++;

   /* Now deal with the function name part. */
   if (!fn_is_encoded) {

      /* simple; just copy. */
      while (True) {
         if (sym[i] == 0)
            break;
         EMITFN(sym[i]);
         i++;
      }
      goto out;

   }

   /* else use a Z-decoding loop like with soname */
   while (True) {

      if (sym[i] == 0)
         break;

      if (sym[i] != 'Z') {
         EMITFN(sym[i]);
         i++;
         continue;
      }

      /* We've got a Z-escape. */
      i++;
      switch (sym[i]) {
         case 'a': EMITFN('*'); break;
         case 'p': EMITFN('+'); break;
         case 'c': EMITFN(':'); break;
         case 'd': EMITFN('.'); break;
         case 'u': EMITFN('_'); break;
         case 'h': EMITFN('-'); break;
         case 's': EMITFN(' '); break;
         case 'Z': EMITFN('Z'); break;
         case 'A': EMITFN('@'); break;
         case 'L': EMITSO('('); break;
         case 'R': EMITSO(')'); break;
         default: error = True; goto out;
      }
      i++;
   }

  out:
   EMITSO(0);
   EMITFN(0);

   if (error) {
      /* Something's wrong.  Give up. */
      VG_(message)(Vg_UserMsg, "m_demangle: error Z-demangling: %s", sym);
      return False;
   }
   if (oflow) {
      /* It didn't fit.  Give up. */
      VG_(message)(Vg_UserMsg, "m_demangle: oflow Z-demangling: %s", sym);
      return False;
   }

   return True;
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
