
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_x86.h) is                       ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBVEX_PUB_GUEST_X86_H
#define __LIBVEX_PUB_GUEST_X86_H

#include "libvex_basictypes.h"

/*---------------------------------------------------------------*/
/*--- Utility functions for x86 guest stuff.                  ---*/
/*---------------------------------------------------------------*/

/* Convert a saved x87 FPU image (as created by fsave) into the
   equivalent vex representation. 
*/
extern void x87_to_vex ( /*IN*/UChar* x87_state, 
                         /*OUT*/UChar* vex_state );

/* Extract from the vex representation, an x87 FPU image. */
extern void vex_to_x87 ( /*IN*/UChar* vex_state, 
                         /*OUT*/UChar* x87_state );

#endif /* ndef __LIBVEX_PUB_GUEST_X86_H */

/*---------------------------------------------------------------*/
/*---                                      libvex_guest_x86.h ---*/
/*---------------------------------------------------------------*/
