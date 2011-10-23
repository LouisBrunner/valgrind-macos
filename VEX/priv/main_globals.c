
/*---------------------------------------------------------------*/
/*--- begin                                    main_globals.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2011 OpenWorks LLP
      info@open-works.net

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "libvex_basictypes.h"

#include "main_util.h"
#include "main_globals.h"


/* Global settings for the VEX library.  These are the
   only library-wide globals. */

/* Are we started yet? */
Bool vex_initdone = False;

/* failure exit function */
__attribute__ ((noreturn))
void (*vex_failure_exit) ( void ) = NULL;

/* logging output function */
void (*vex_log_bytes) ( HChar*, Int nbytes ) = NULL;

/* debug paranoia level */
Int vex_debuglevel = 0;

/* trace flags */
Int vex_traceflags = 0;

/* Are we supporting valgrind checking? */
Bool vex_valgrind_support = False;

/* Max # guest insns per bb */
VexControl vex_control = { 0,0,False,0,0,0 };



/*---------------------------------------------------------------*/
/*--- end                                      main_globals.c ---*/
/*---------------------------------------------------------------*/
