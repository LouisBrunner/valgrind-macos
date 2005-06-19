
/*--------------------------------------------------------------------*/
/*--- The main module.                             pub_core_main.h ---*/
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

#ifndef __PUB_CORE_MAIN_H
#define __PUB_CORE_MAIN_H

//--------------------------------------------------------------------
// PURPOSE: This module is the main module, ie. the one holding main().
// It arguably shouldn't export anything to other modules, since it depends
// on almost every other module!  But currently it exports quite a few
// things.
//--------------------------------------------------------------------

/* Sanity checks which may be done at any time.  The scheduler decides when. */
extern void VG_(sanity_check_general) ( Bool force_expensive );

/* client executable file descriptor */
extern Int  VG_(clexecfd);

// Help set up the child used when doing execve() with --trace-children=yes
Char* VG_(build_child_VALGRINDCLO) ( Char* exename );
Char* VG_(build_child_exename)     ( void );

/* Something of a function looking for a home ... start up debugger. */
extern void VG_(start_debugger) ( ThreadId tid );

/* 64-bit counter for the number of basic blocks done. */
extern ULong VG_(bbs_done);

// Set up the libc freeres wrapper 
extern void VG_(set_libc_freeres_wrapper_addr)(Addr);

// Do everything which needs doing before the process finally ends,
// like printing reports, etc
extern void VG_(shutdown_actions_NORETURN) (
               ThreadId tid, 
               VgSchedReturnCode tids_schedretcode 
            );

#endif   // __PUB_CORE_MAIN_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
