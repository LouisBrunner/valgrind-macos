
/*--------------------------------------------------------------------*/
/*--- Demangling of C++ mangled names.                  demangle.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
      jseward@acm.org

   Rust demangler components are
   Copyright (C) 2016-2016 David Tolnay
      dtolnay@gmail.com

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
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"

#include "vg_libciface.h"
#include "demangle.h"


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* The demangler's job is to take a raw symbol name and turn it into
   something a Human Bean can understand.  Our mangling model
   comprises a three stage pipeline.  Mangling pushes names forward
   through the pipeline (0, then 1, then 2) and demangling is
   obviously the reverse.  In practice it is highly unlikely that a
   name would require all stages, but it is not impossible either.

   0. If we're working with Rust, Rust names are lightly mangled by
      the Rust front end.

   1. Then the name is subject to standard C++ mangling.

   2. Optionally, in relatively rare cases, the resulting name is then
      itself encoded using Z-escaping (see pub_core_redir.h) so as to
      become part of a redirect-specification.

   Therefore, VG_(demangle) first tries to undo (2).  If successful,
   the soname part is discarded (humans don't want to see that).
   Then, it tries to undo (1) (using demangling code from GNU/FSF) and
   finally it tries to undo (0).

   Finally, it changes the name of all symbols which are known to be
   functions below main() to "(below main)".  This helps reduce
   variability of stack traces, something which has been a problem for
   the testsuite for a long time.

   --------
   If do_cxx_demangle == True, it does all the above stages:
   - undo (2) [Z-encoding]
   - undo (1) [C++ mangling]
   - if (1) succeeds, undo (0) [Rust mangling]
   - do the below-main hack

   Rust demangling (0) is only done if C++ demangling (1) succeeds
   because Rust demangling is performed in-place, and it is difficult
   to prove that we "own" the storage -- hence, that the in-place
   operation is safe -- unless it is clear that it has come from the
   C++ demangler, which returns its output in a heap-allocated buffer
   which we can be sure we own.  In practice (Nov 2016) this does not
   seem to be a problem, since the Rust compiler appears to apply C++
   mangling after Rust mangling, so we never encounter symbols that
   require Rust demangling but not C++ demangling.

   If do_cxx_demangle == False, the C++ and Rust stags are skipped:
   - undo (2) [Z-encoding]
   - do the below-main hack
*/

/* Note that the C++ demangler is from GNU libiberty and is almost
   completely unmodified.  We use vg_libciface.h as a way to
   impedance-match the libiberty code into our own framework.

   The libiberty code included here was taken from the GCC repository
   and is released under the LGPL 2.1 license, which AFAICT is compatible
   with "GPL 2 or later" and so is OK for inclusion in Valgrind.

   To update to a newer libiberty, use the "update-demangler" script
   which is included in the valgrind repository. */

/* This is the main, standard demangler entry point. */

/* Upon return, *RESULT will point to the demangled name.
   The memory buffer that holds the demangled name is allocated on the
   heap and will be deallocated in the next invocation. Conceptually,
   that buffer is owned by VG_(demangle). That means two things:
   (1) Users of VG_(demangle) must not free that buffer.
   (2) If the demangled name needs to be stashed away for later use,
       the contents of the buffer need to be copied. It is not sufficient
       to just store the pointer as it will point to deallocated memory
       after the next VG_(demangle) invocation. */
void VG_(demangle) ( Bool do_cxx_demangling, Bool do_z_demangling,
                     /* IN */  const HChar  *orig,
                     /* OUT */ const HChar **result )
{
   /* Possibly undo (2) */
   /* Z-Demangling was requested.  
      The fastest way to see if it's a Z-mangled name is just to attempt
      to Z-demangle it (with NULL for the soname buffer, since we're not
      interested in that). */
   if (do_z_demangling) {
      const HChar *z_demangled;

      if (VG_(maybe_Z_demangle)( orig, NULL, /*soname*/
                                 &z_demangled, NULL, NULL, NULL )) {
         orig = z_demangled;
      }
   }

   /* Possibly undo (1) */
   if (do_cxx_demangling && VG_(clo_demangle)
       && orig != NULL && orig[0] == '_' && orig[1] == 'Z') {
      /* !!! vvv STATIC vvv !!! */
      static HChar* demangled = NULL;
      /* !!! ^^^ STATIC ^^^ !!! */

      /* Free up previously demangled name */
      if (demangled) {
         VG_(arena_free) (VG_AR_DEMANGLE, demangled);
         demangled = NULL;
      }
      demangled = ML_(cplus_demangle) ( orig, DMGL_ANSI | DMGL_PARAMS );

      *result = (demangled == NULL) ? orig : demangled;

      if (demangled) {
         /* Possibly undo (0).  This is the only place where it is
            safe, from a storage management perspective, to
            Rust-demangle the symbol.  That's because Rust demangling
            happens in place, so we need to be sure that the storage
            it is happening in is actually owned by us, and non-const.
            In this case, the value returned by ML_(cplus_demangle)
            does have that property. */
         if (rust_is_mangled(demangled)) {
            rust_demangle_sym(demangled);
         }
         *result = demangled;
      } else {
         *result = orig;
      }

   } else {
      *result = orig;
   }

   // 13 Mar 2005: We used to check here that the demangler wasn't leaking
   // by calling the (now-removed) function VG_(is_empty_arena)().  But,
   // very rarely (ie. I've heard of it twice in 3 years), the demangler
   // does leak.  But, we can't do much about it, and it's not a disaster,
   // so we just let it slide without aborting or telling the user.
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
                             /*OUT*/const HChar** so,
                             /*OUT*/const HChar** fn,
                             /*OUT*/Bool* isWrap,
                             /*OUT*/Int*  eclassTag,
                             /*OUT*/Int*  eclassPrio )
{
   static HChar *sobuf;
   static HChar *fnbuf;
   static SizeT  buf_len = 0;

   /* The length of the name after undoing Z-encoding is always smaller
      than the mangled name. Making the soname and fnname buffers as large
      as the demangled name is therefore always safe and overflow can never
      occur. */
   SizeT len = VG_(strlen)(sym) + 1;

   if (buf_len < len) {
      sobuf = VG_(arena_realloc)(VG_AR_DEMANGLE, "Z-demangle", sobuf, len);
      fnbuf = VG_(arena_realloc)(VG_AR_DEMANGLE, "Z-demangle", fnbuf, len);
      buf_len = len;
   }
   sobuf[0] = fnbuf[0] = '\0';

   if (so) 
     *so = sobuf;
   *fn = fnbuf;

#  define EMITSO(ch)                           \
      do {                                     \
         if (so) {                             \
            sobuf[soi++] = ch; sobuf[soi] = 0; \
         }                                     \
      } while (0)
#  define EMITFN(ch)                           \
      do {                                     \
         fnbuf[fni++] = ch; fnbuf[fni] = 0;    \
      } while (0)

   Bool error, valid, fn_is_encoded, is_VG_Z_prefixed;
   Int  soi, fni, i;

   error = False;
   soi = 0;
   fni = 0;

   valid =     sym[0] == '_'
           &&  sym[1] == 'v'
           &&  sym[2] == 'g'
           && (sym[3] == 'r' || sym[3] == 'w')
           &&  VG_(isdigit)(sym[4])
           &&  VG_(isdigit)(sym[5])
           &&  VG_(isdigit)(sym[6])
           &&  VG_(isdigit)(sym[7])
           &&  VG_(isdigit)(sym[8])
           &&  sym[9] == 'Z'
           && (sym[10] == 'Z' || sym[10] == 'U')
           &&  sym[11] == '_';

   if (valid
       && sym[4] == '0' && sym[5] == '0' && sym[6] == '0' && sym[7] == '0'
       && sym[8] != '0') {
      /* If the eclass tag is 0000 (meaning "no eclass"), the priority
         must be 0 too. */
      valid = False;
   }

   if (!valid)
      return False;

   fn_is_encoded = sym[10] == 'Z';

   if (isWrap)
      *isWrap = sym[3] == 'w';

   if (eclassTag) {
      *eclassTag =    1000 * ((Int)sym[4] - '0')
                   +  100 * ((Int)sym[5] - '0')
                   +  10 * ((Int)sym[6] - '0')
                   +  1 * ((Int)sym[7] - '0');
      vg_assert(*eclassTag >= 0 && *eclassTag <= 9999);
   }

   if (eclassPrio) {
      *eclassPrio = ((Int)sym[8]) - '0';
      vg_assert(*eclassPrio >= 0 && *eclassPrio <= 9);
   }

   /* Now check the soname prefix isn't "VG_Z_", as described in
      pub_tool_redir.h. */
   is_VG_Z_prefixed =
      sym[12] == 'V' &&
      sym[13] == 'G' &&
      sym[14] == '_' &&
      sym[15] == 'Z' &&
      sym[16] == '_';
   if (is_VG_Z_prefixed) {
      vg_assert2(0, "symbol with a 'VG_Z_' prefix: %s.\n"
                    "see pub_tool_redir.h for an explanation.", sym);
   }

   /* Now scan the Z-encoded soname. */
   i = 12;
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
         case 'c': EMITSO(':'); break;
         case 'd': EMITSO('.'); break;
         case 'h': EMITSO('-'); break;
         case 'p': EMITSO('+'); break;
         case 's': EMITSO(' '); break;
         case 'u': EMITSO('_'); break;
         case 'A': EMITSO('@'); break;
         case 'D': EMITSO('$'); break;
         case 'L': EMITSO('('); break;
         case 'P': EMITSO('%'); break;
         case 'R': EMITSO(')'); break;
         case 'S': EMITSO('/'); break;
         case 'Z': EMITSO('Z'); break;
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
         case 'c': EMITFN(':'); break;
         case 'd': EMITFN('.'); break;
         case 'h': EMITFN('-'); break;
         case 'p': EMITFN('+'); break;
         case 's': EMITFN(' '); break;
         case 'u': EMITFN('_'); break;
         case 'A': EMITFN('@'); break;
         case 'D': EMITFN('$'); break;
         case 'L': EMITFN('('); break;
         case 'P': EMITFN('%'); break;
         case 'R': EMITFN(')'); break;
         case 'S': EMITFN('/'); break;
         case 'Z': EMITFN('Z'); break;
         default: error = True; goto out;
      }
      i++;
   }

  out:
   EMITSO(0);
   EMITFN(0);

   if (error) {
      /* Something's wrong.  Give up. */
      VG_(message)(Vg_UserMsg,
                   "m_demangle: error Z-demangling: %s\n", sym);
      return False;
   }

   return True;
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
