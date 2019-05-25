
/*--------------------------------------------------------------------*/
/*--- The C++ name demangler.                  pub_core_demangle.h ---*/
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

#ifndef __PUB_CORE_DEMANGLE_H
#define __PUB_CORE_DEMANGLE_H

#include "pub_core_basics.h"      // VG_ macro

//--------------------------------------------------------------------
// PURPOSE: This module exports functions for demangling C++ and 
// Z-encoded names.
//--------------------------------------------------------------------

/* This is the main, standard demangler entry point.  It does two things:
 * (1) undoes Z-encoding, if "do_z_demangle" is True;
 * (2) undoes C++ demangling, if 'do_cxx_demangle' is True.  */
extern 
void VG_(demangle) ( Bool do_cxx_demangling, Bool do_z_demangling,
                     const HChar* orig, const HChar** result );

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

extern 
Bool VG_(maybe_Z_demangle) ( const HChar* sym, 
                             /*OUT*/const HChar** so,    // may be NULL
                             /*OUT*/const HChar** fn,
                             /*OUT*/Bool* isWrap,        // may be NULL
                             /*OUT*/Int*  eclassTag,     // may be NULL
                             /*OUT*/Int*  eclassPrio );  // may be NULL

#endif   // __PUB_CORE_DEMANGLE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
