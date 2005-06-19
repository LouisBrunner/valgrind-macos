/*--------------------------------------------------------------------*/
/*--- ErrorMgr: management of errors and suppressions.             ---*/
/*---                                          pub_core_errormgr.h ---*/
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

#ifndef __PUB_CORE_ERRORMGR_H
#define __PUB_CORE_ERRORMGR_H

//--------------------------------------------------------------------
// PURPOSE: This module manages errors recording and printing, 
// which includes suppression reading and writing.
//--------------------------------------------------------------------

#include "pub_tool_errormgr.h"

// XXX: should this be in pthreadmodel.c?
// These must be negative, so as to not overlap with tool error kinds.
typedef
   enum { 
      ThreadErr      = -1,   // Thread error
      MutexErr       = -2,   // Mutex error
   }
   CoreErrorKind;

extern void VG_(load_suppressions)        ( void );

extern void VG_(show_all_errors)          ( void );

extern void VG_(show_error_counts_as_XML) ( void );

extern Bool VG_(is_action_requested)      ( Char* action, Bool* clo );

extern UInt VG_(get_n_errs_found)         ( void );

#endif   // __PUB_CORE_ERRORMGR_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
