
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (jit_globals.h) is                            ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __JIT_GLOBALS_H
#define __JIT_GLOBALS_H

#include "libjit_basictypes.h"


/* Global settings for the JIT library.  These are the
   only library-wide globals. */

/* Are we started yet? */
extern Bool vex_initdone;

/* failure exit function */
extern void (*vex_failure_exit) ( void );

/* logging output function */
extern void (*vex_log_bytes) ( Char*, Int nbytes );

/* debug paranoia level */
extern Int vex_debuglevel;

/* verbosity level */
extern Int vex_verbosity;

/* Are we supporting valgrind checking? */
extern Bool vex_valgrind_support;

/* Max # guest insns per bb */
extern Int vex_guest_insns_per_bb;


#endif /* ndef __JIT_GLOBALS_H */

/*---------------------------------------------------------------*/
/*--- end                                       jit_globals.h ---*/
/*---------------------------------------------------------------*/
