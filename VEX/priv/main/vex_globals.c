
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (vex_globals.c) is                            ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"

#include "vex_util.h"


/* Global settings for the VEX library.  These are the
   only library-wide globals. */

/* Are we started yet? */
Bool vex_initdone = False;

/* failure exit function */
void (*vex_failure_exit) ( void ) = NULL;

/* logging output function */
void (*vex_log_bytes) ( Char*, Int nbytes ) = NULL;

/* debug paranoia level */
Int vex_debuglevel = 0;

/* verbosity level */
Int vex_verbosity = 0;

/* Are we supporting valgrind checking? */
Bool vex_valgrind_support = False;

/* Max # guest insns per bb */
Int vex_guest_insns_per_bb = 0;



/*---------------------------------------------------------------*/
/*--- end                                       vex_globals.c ---*/
/*---------------------------------------------------------------*/
