
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (h_generic_regs.h) is                         ---*/
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

#ifndef __H_GENERIC_REGS_H
#define __H_GENERIC_REGS_H

#include "libvex_basictypes.h"


/*---------------------------------------------------------*/
/*--- Representing HOST REGISTERS                       ---*/
/*---------------------------------------------------------*/

/* Host registers.  Stuff to represent:

   - The register number
   - The register class
   - Whether or not the register is a virtual reg.

   Registers are a 32-bit Int, thusly:

     bits 31-28  are the register class.
     bits 27-23  are 0000b for real register, 0001b for virtual register
     bits 23-0   register number

   Note (importantly) that by arranging that the class field is never
   0000b, any valid register looks like an extremely large int -- at
   least 2^28 -- and so there is little chance of confusing it with an
   integer array index in the register allocator.

   Note further that since the class field is never 1111b, no valid
   register can have the value INVALID_HREG.

   There are currently 3 register classes:

     int
     floating
     vector 
*/

typedef UInt HReg;

/* When extending this, do not use any value > 14 or < 0. */
/* HRegClass describes host register classes which the instruction
   selectors can speak about.  We would not expect all of them to be
   available on any specific host.  For example on x86, the available
   classes are: Int32, Flt64, Vec128 only.

   IMPORTANT NOTE: Vec128 is the only >= 128-bit-sized class, and
   reg_alloc2.c handles it specially when assigning spill slots.  If
   you add another 128-bit or larger regclass, you must remember to
   update reg_alloc2.c accordingly.
*/
typedef
   enum { 
      HRcINVALID=1,   /* NOT A VALID REGISTER CLASS */
      HRcInt32=4,     /* 32-bit int */
      HRcInt64=5,     /* 64-bit int */
      HRcFlt64=6,     /* 64-bit float */
      HRcVec64=7,     /* 64-bit SIMD */
      HRcVec128=8     /* 128-bit SIMD */
   }
   HRegClass;

extern void ppHRegClass ( HRegClass );


/* Print an HReg in a generic (non-target-specific) way. */
extern void ppHReg ( HReg );

/* Construct/destruct. */
static inline HReg mkHReg ( UInt regno, HRegClass rc, Bool virtual ) {
   UInt r24 = regno & 0x00FFFFFF;
   /* This is critical.  The register number field may only
      occupy 24 bits. */
   if (r24 != regno)
      vpanic("mkHReg: regno exceeds 2^24");
   return regno | (((UInt)rc) << 28) | (virtual ? (1<<24) : 0);
}

static inline HRegClass hregClass ( HReg r ) {
   UInt rc = r;
   rc = (rc >> 28) & 0x0F;
   vassert(rc >= HRcInt32 && rc <= HRcVec128);
   return (HRegClass)rc;
}

static inline UInt hregNumber ( HReg r ) {
   return ((UInt)r) & 0x00FFFFFF;
}

static inline Bool hregIsVirtual ( HReg r ) {
   return toBool(((UInt)r) & (1<<24));
}




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
#define N_HREG_USAGE 25

typedef
   struct {
      HReg     hreg[N_HREG_USAGE];
      HRegMode mode[N_HREG_USAGE];
      Int      n_used;
   }
   HRegUsage;

extern void ppHRegUsage ( HRegUsage* );

static inline void initHRegUsage ( HRegUsage* tab ) {
   tab->n_used = 0;
}

/* Add a register to a usage table.  Combine incoming read uses with
   existing write uses into a modify use, and vice versa.  Do not
   create duplicate entries -- each reg should only be mentioned once.  
*/
extern void addHRegUse ( HRegUsage*, HRegMode, HReg );



/*---------------------------------------------------------*/
/*--- Indicating register remappings (for reg-alloc)    ---*/
/*---------------------------------------------------------*/

/* Note that such maps can only map virtual regs to real regs.
   addToHRegRenap will barf if given a pair not of that form.  As a
   result, no valid HRegRemap will bind a real reg to anything, and so
   if lookupHRegMap is given a real reg, it returns it unchanged.
   This is precisely the behaviour that the register allocator needs
   to impose its decisions on the instructions it processes.  */

#define N_HREG_REMAP 4

typedef
   struct {
      HReg orig       [N_HREG_REMAP];
      HReg replacement[N_HREG_REMAP];
      Int  n_used;
   }
   HRegRemap;

extern void ppHRegRemap     ( HRegRemap* );
extern void initHRegRemap   ( HRegRemap* );
extern void addToHRegRemap  ( HRegRemap*, HReg, HReg );
extern HReg lookupHRegRemap ( HRegRemap*, HReg );


/*---------------------------------------------------------*/
/*--- Abstract instructions                             ---*/
/*---------------------------------------------------------*/

/* A type is needed to refer to pointers to instructions of any
   target.  Defining it like this means that HInstr* can stand in for
   X86Instr*, ArmInstr*, etc. */

typedef  void  HInstr;


/* An expandable array of HInstr*'s.  Handy for insn selection and
   register allocation.  n_vregs indicates the number of virtual
   registers mentioned in the code, something that reg-alloc needs to
   know.  These are required to be numbered 0 .. n_vregs-1. 
*/
typedef
   struct {
      HInstr** arr;
      Int      arr_size;
      Int      arr_used;
      Int      n_vregs;
   }
   HInstrArray;

extern HInstrArray* newHInstrArray ( void );
extern void         addHInstr ( HInstrArray*, HInstr* );


/*---------------------------------------------------------*/
/*--- Reg alloc: TODO: move somewhere else              ---*/
/*---------------------------------------------------------*/

extern
HInstrArray* doRegisterAllocation (

   /* Incoming virtual-registerised code. */ 
   HInstrArray* instrs_in,

   /* An array listing all the real registers the allocator may use,
      in no particular order. */
   HReg* available_real_regs,
   Int   n_available_real_regs,

   /* Return True iff the given insn is a reg-reg move, in which
      case also return the src and dst regs. */
   Bool (*isMove) (HInstr*, HReg*, HReg*),

   /* Get info about register usage in this insn. */
   void (*getRegUsage) (HRegUsage*, HInstr*),

   /* Apply a reg-reg mapping to an insn. */
   void (*mapRegs) (HRegRemap*, HInstr*),

   /* Return an insn to spill/restore a real reg to a spill slot
      offset. */
   HInstr* (*genSpill) ( HReg, Int ),
   HInstr* (*genReload) ( HReg, Int ),
   Int     guest_sizeB,

   /* For debug printing only. */
   void (*ppInstr) ( HInstr* ),
   void (*ppReg) ( HReg )
);


#endif /* ndef __H_GENERIC_REGS_H */

/*---------------------------------------------------------------*/
/*---                                        h_generic_regs.h ---*/
/*---------------------------------------------------------------*/
