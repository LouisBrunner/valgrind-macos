
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (vex_globals.h) is                            ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __VEX_GLOBALS_H
#define __VEX_GLOBALS_H

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
extern void (*vex_log_bytes) ( Char*, Int nbytes );

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


#endif /* ndef __VEX_GLOBALS_H */

/*---------------------------------------------------------------*/
/*--- end                                       vex_globals.h ---*/
/*---------------------------------------------------------------*/
