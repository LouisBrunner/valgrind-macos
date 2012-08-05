
/*---------------------------------------------------------------*/
/*--- begin                                    main_globals.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2012 OpenWorks LLP
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

#ifndef __VEX_MAIN_GLOBALS_H
#define __VEX_MAIN_GLOBALS_H

#include "libvex_basictypes.h"
#include "libvex.h"


/* Global settings for the VEX library.  These are the
   only library-wide globals. */

/* Are we started yet? */
extern Bool vex_initdone;

/* failure exit function */
__attribute__ ((noreturn))
extern void (*vex_failure_exit) ( void );

/* logging output function */
extern void (*vex_log_bytes) ( HChar*, Int nbytes );

/* debug paranoia level */
extern Int vex_debuglevel;

/* trace flags */
extern Int vex_traceflags;

/* Are we supporting valgrind checking? */
extern Bool vex_valgrind_support;

/* Optimiser/front-end control */
extern VexControl vex_control;


/* vex_traceflags values */
#define VEX_TRACE_FE     (1 << 7)  /* show conversion into IR */
#define VEX_TRACE_OPT1   (1 << 6)  /* show after initial opt */
#define VEX_TRACE_INST   (1 << 5)  /* show after instrumentation */
#define VEX_TRACE_OPT2   (1 << 4)  /* show after second opt */
#define VEX_TRACE_TREES  (1 << 3)  /* show after tree building */
#define VEX_TRACE_VCODE  (1 << 2)  /* show selected insns */
#define VEX_TRACE_RCODE  (1 << 1)  /* show after reg-alloc */
#define VEX_TRACE_ASM    (1 << 0)  /* show final assembly */


#endif /* ndef __VEX_MAIN_GLOBALS_H */

/*---------------------------------------------------------------*/
/*--- end                                      main_globals.h ---*/
/*---------------------------------------------------------------*/
