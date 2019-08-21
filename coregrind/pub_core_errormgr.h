
/*--------------------------------------------------------------------*/
/*--- ErrorMgr: management of errors and suppressions.             ---*/
/*---                                          pub_core_errormgr.h ---*/
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

#ifndef __PUB_CORE_ERRORMGR_H
#define __PUB_CORE_ERRORMGR_H

//--------------------------------------------------------------------
// PURPOSE: This module manages errors recording and printing, 
// which includes suppression reading and writing.
//--------------------------------------------------------------------

#include "pub_tool_errormgr.h"

// These must be negative, so as to not overlap with tool error kinds.
typedef
   enum { 
      // Nb: thread errors are a relic of the time when Valgrind's core
      // could detect them.  This example is left as an example should new
      // core errors ever be added.
      ThreadErr = -1,
   }
   CoreErrorKind;

/* Add a new suppression file in the list of supp files.
   If VG_(load_suppressions) was already called, also load it. */
extern void VG_(add_suppression_file) (const HChar *filename);

extern void VG_(load_suppressions)        ( void );

// if verbosity == 0,           print nothing.
// else if xml                  print suppressions used (in xml format)
// else if verbosity == 1       print Error summary
// else                         print all errors and suppressions used.
extern void VG_(show_all_errors)          ( Int verbosity, Bool xml );

/* Print (in readable format) the last error that occurred. */
extern void VG_(show_last_error)          ( void );

extern void VG_(show_error_counts_as_XML) ( void );

extern Bool VG_(is_action_requested)      ( const HChar* action, Bool* clo );

extern Bool VG_(showing_core_errors)      ( void );

extern UInt VG_(get_n_errs_found)         ( void );
extern UInt VG_(get_n_errs_shown)         ( void );

extern void VG_(print_errormgr_stats)     ( void );

#endif   // __PUB_CORE_ERRORMGR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
