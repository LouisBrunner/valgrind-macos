
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host_regs.h) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __HOST_REGS_H
#define __HOST_REGS_H


/* Host registers.  Stuff to represent:

   - The register number
   - The register class
   - Whether or not the register is a virtual reg.

   Registers are a 32-bit Int, thusly:

     bits 31-8  register number
     bits 7-4   are 0000b for real register, 0001b for virtual register
     bits 3-0   are the register class.

   There are currently 3 register classes:

     int
     floating
     vector.
*/

typedef UInt HReg;

typedef
   enum { HRcInt=0, HRcFloat=1, HRcVector=2 }
   HRegClass;


/* Print an HReg in a generic (non-target-specific) way. */
extern void ppHReg ( FILE*, HReg );

/* Construct/destruct. */
extern HReg mkHReg ( UInt regno, HRegClass rc, Bool virtual );

extern HRegClass hregClass     ( HReg );
extern Bool      hregIsVirtual ( HReg );
extern UInt      hregNumber    ( HReg );


#endif /* ndef __HOST_REGS_H */
