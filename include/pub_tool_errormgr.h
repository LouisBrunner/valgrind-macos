/*--------------------------------------------------------------------*/
/*--- ErrorMgr: management of errors and suppressions.             ---*/
/*---                                          pub_tool_errormgr.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward
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

#ifndef __PUB_TOOL_ERRORMGR_H
#define __PUB_TOOL_ERRORMGR_H

#include "pub_tool_execontext.h"

/* ------------------------------------------------------------------ */
/* Error records contain enough info to generate an error report.  The idea
   is that (typically) the same few points in the program generate thousands
   of errors, and we don't want to spew out a fresh error message for each
   one.  Instead, we use these structures to common up duplicates.
*/

typedef
   Int         /* Do not make this unsigned! */
   ErrorKind;

/* The tool-relevant parts of an Error are:
     kind:   what kind of error; must be in the range (0..)
     addr:   use is optional.  0 by default.
     string: use is optional.  NULL by default.
     extra:  use is optional.  NULL by default.  void* so it's extensible.
*/
typedef
   struct _Error
   Error;

/* Useful in VG_(tdict).tool_error_matches_suppression(),
 * VG_(tdict).tool_pp_Error(), etc */
ExeContext* VG_(get_error_where)   ( Error* err );
ErrorKind   VG_(get_error_kind)    ( Error* err );
Addr        VG_(get_error_address) ( Error* err );
Char*       VG_(get_error_string)  ( Error* err );
void*       VG_(get_error_extra)   ( Error* err );

/* Call this when an error occurs.  It will be recorded if it hasn't been
   seen before.  If it has, the existing error record will have its count
   incremented.

   'tid' can be found as for VG_(record_ExeContext)().  The `extra' field can
   be stack-allocated;  it will be copied by the core if needed (but it
   won't be copied if it's NULL).

   If no 'a', 's' or 'extra' of interest needs to be recorded, just use
   NULL for them.  */
extern void VG_(maybe_record_error) ( ThreadId tid, ErrorKind ekind,
                                      Addr a, Char* s, void* extra );

/* Similar to VG_(maybe_record_error)(), except this one doesn't record the
   error -- useful for errors that can only happen once.  The errors can be
   suppressed, though.  Return value is True if it was suppressed.
   'print_error' dictates whether to print the error, which is a bit of a
   hack that's useful sometimes if you just want to know if the error would
   be suppressed without possibly printing it.  'count_error' dictates
   whether to add the error in the error total count (another mild hack). */
extern Bool VG_(unique_error) ( ThreadId tid, ErrorKind ekind,
                                Addr a, Char* s, void* extra,
                                ExeContext* where, Bool print_error,
                                Bool allow_GDB_attach, Bool count_error );

/* Gets a non-blank, non-comment line from fd.  bufpp is a pointer to a
   pointer to a buffer that must be allocated with VG_(malloc);  nBufp is a
   pointer to size_t holding its size;  if the buffer is too small for the
   line, it will be realloc'd until big enough (updating *bufpp and *nBufp in
   the process).  (It will bomb out if the size gets ridiculous).  Skips
   leading spaces on the line.  Increments lineno with the number of lines
   read if lineno is non-NULL. Returns True if EOF was hit.  */
extern Bool VG_(get_line) ( Int fd, Char** bufpp, SizeT* nBufp, Int* lineno );


/* ------------------------------------------------------------------ */
/* Suppressions describe errors which we want to suppress, ie, not
   show the user, usually because it is caused by a problem in a library
   which we can't fix, replace or work around.  Suppressions are read from
   a file at startup time.  This gives flexibility so that new
   suppressions can be added to the file as and when needed.
*/
typedef
   Int         /* Do not make this unsigned! */
   SuppKind;

/* The tool-relevant parts of a suppression are:
     kind:   what kind of suppression; must be in the range (0..)
     string: use is optional.  NULL by default.
     extra:  use is optional.  NULL by default.  void* so it's extensible.
*/
typedef
   struct _Supp
   Supp;

/* Useful in VG_(tdict).tool_error_matches_suppression() */
SuppKind VG_(get_supp_kind)   ( Supp* su );
Char*    VG_(get_supp_string) ( Supp* su );
void*    VG_(get_supp_extra)  ( Supp* su );

/* Must be used in VG_(recognised_suppression)() */
void VG_(set_supp_kind)   ( Supp* su, SuppKind suppkind );
/* May be used in VG_(read_extra_suppression_info)() */
void VG_(set_supp_string) ( Supp* su, Char* string );
void VG_(set_supp_extra)  ( Supp* su, void* extra );


#endif   // __PUB_TOOL_ERRORMGR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
