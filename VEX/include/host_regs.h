
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host_regs.h) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __HOST_REGS_H
#define __HOST_REGS_H


/*---------------------------------------------------------*/
/*--- Representing HOST REGISTERS                       ---*/
/*---------------------------------------------------------*/

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

#define INVALID_HREG ((HReg)0xFFFFFFFF)


/*---------------------------------------------------------*/
/*--- Recording register usage (for reg-alloc)          ---*/
/*---------------------------------------------------------*/

typedef
   enum { HRmRead, HRmWrite, HRmModify }
   HRegMode;


/* A struct for recording the usage of registers in instructions.
   This can get quite large, but we don't expect to allocate them
   dynamically, so there's no problem. 
*/
#define N_HREG_USAGE 4

typedef
   struct {
      HReg     hreg[N_HREG_USAGE];
      HRegMode mode[N_HREG_USAGE];
      Int      n_used;
   }
   HRegUsage;

extern void ppHRegUsage ( FILE*, HRegUsage* );

extern void initHRegUsage ( HRegUsage* );

/* Add a register to a usage table.  Combine incoming read uses with
   existing write uses into a modify use, and vice versa.  Do not
   create duplicate entries -- each reg should only be mentioned once.  
*/
extern void addToHRegUsage ( HRegUsage*, HReg, HRegMode );



/*---------------------------------------------------------*/
/*--- Indicating register remappings (for reg-alloc)    ---*/
/*---------------------------------------------------------*/

#define N_HREG_REMAP 4

typedef
   struct {
      HReg orig       [N_HREG_REMAP];
      HReg replacement[N_HREG_REMAP];
      Int  n_used;
   }
   HRegRemap;

extern void ppHRegRemap     ( FILE*, HRegRemap* );
extern void initHRegRemap   ( HRegRemap* );
extern void addToHRegRemap  ( HRegRemap*, HReg, HReg );
extern HReg lookupHRegRemap ( HRegRemap*, HReg );


#endif /* ndef __HOST_REGS_H */

/*---------------------------------------------------------------*/
/*---                                             host_regs.h ---*/
/*---------------------------------------------------------------*/
