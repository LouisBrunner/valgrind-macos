
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (vex_globals.c) is                            ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004 OpenWorks, LLP.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; Version 2 dated June 1991 of the
   license.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, or liability
   for damages.  See the GNU General Public License for more details.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.
*/

#include "libvex_basictypes.h"

#include "vex_util.h"
#include "vex_globals.h"


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
/*--- end                                       vex_globals.c ---*/
/*---------------------------------------------------------------*/

