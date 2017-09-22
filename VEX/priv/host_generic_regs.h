
/*---------------------------------------------------------------*/
/*--- begin                               host_generic_regs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
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

#ifndef __VEX_HOST_GENERIC_REGS_H
#define __VEX_HOST_GENERIC_REGS_H

#include "libvex_basictypes.h"


/*---------------------------------------------------------*/
/*--- Representing HOST REGISTERS                       ---*/
/*---------------------------------------------------------*/

/* Host registers.  Stuff to represent:

   - The register index.  This is a zero-based, sequential index that
     facilitates indexing into arrays or virtual or real registers.
     Virtual and real registers both have indices starting at zero.
     Interpreting a real register index requires having the host's
     RRegUniverse to hand.

   - The register's hardware encoding.  This applies only for real
     registers and should be zero for virtual registers.  This is the
     number as used in a target architecture encoding.

   - The register class

   - Whether or not the register is a virtual reg.

   Registers are sized so as to fit into 32 bits.

   Note that since the class field is never 1111b, no valid register
   can have the value INVALID_HREG.

   There are currently 6 register classes:

     int32 int64 float32 float64 simd64 simd128
*/

/* Registers are represented as 32 bit integers, with the following layout:

   31     30..27  26..20  19..0
   isV:1  rc:4    enc:7   ix:20

   where
      UInt      ix:20;   // Zero based index
      UInt      enc:7;   // Hardware encoding number
      HRegClass rc:4;    // the register's HRegClass
      Bool      isV:1;   // is it a virtual register?

   The obvious thing to do here would be to use bitfields.  But gcc
   seems to have problems constant folding calls to mkHReg() with all
   4 parameters constant to a 32 bit number, when using bitfields.
   Hence the use of the traditional shift-and-mask by-hand bitfields
   instead.
*/
typedef  struct { UInt u32; }  HReg;

/* HRegClass describes host register classes which the instruction
   selectors can speak about.  We would not expect all of them to be
   available on any specific host.  For example on x86, the available
   classes are: Int32, Flt64, Vec128 only.

   IMPORTANT NOTE: host_generic_reg_alloc*.c needs to know how much space is
   needed to spill each class of register.  It allocates the following
   amount of space:

      HRcInt32     64 bits
      HRcInt64     64 bits
      HRcFlt32     64 bits
      HRcFlt64     128 bits (on x86 these are spilled by fstpt/fldt and
                             so won't fit in a 64-bit slot)
      HRcVec64     64 bits
      HRcVec128    128 bits

   If you add another regclass, you must remember to update
   host_generic_reg_alloc*.c and RRegUniverse accordingly.

   When adding entries to enum HRegClass, do not use any value > 14 or < 1.
*/
typedef
   enum { 
      HRcINVALID=1,   /* NOT A VALID REGISTER CLASS */
      HRcInt32=3,     /* 32-bit int */
      HRcInt64=4,     /* 64-bit int */
      HRcFlt32=5,     /* 32-bit float */
      HRcFlt64=6,     /* 64-bit float */
      HRcVec64=7,     /* 64-bit SIMD */
      HRcVec128=8,    /* 128-bit SIMD */
      HrcLAST=HRcVec128
   }
   HRegClass;

extern void ppHRegClass ( HRegClass );


/* Print an HReg in a generic (non-target-specific) way.
   Returns number of HChar's written. */
extern UInt ppHReg ( HReg );

/* Construct.  The goal here is that compiler can fold this down to a
   constant in the case where the four arguments are constants, which
   is often the case. */
static inline HReg mkHReg ( Bool virtual, HRegClass rc, UInt enc, UInt ix )
{
   vassert(ix <= 0xFFFFF);
   vassert(enc <= 0x7F);
   vassert(((UInt)rc) <= 0xF);
   vassert(((UInt)virtual) <= 1);
   if (virtual) vassert(enc == 0);
   HReg r;
   r.u32 = ((((UInt)virtual) & 1)       << 31)  |
           ((((UInt)rc)      & 0xF)     << 27)  |
           ((((UInt)enc)     & 0x7F)    << 20)  |
           ((((UInt)ix)      & 0xFFFFF) << 0);
   return r;
}

static inline HRegClass hregClass ( HReg r )
{
   HRegClass rc = (HRegClass)((r.u32 >> 27) & 0xF);
   vassert(rc >= HRcInt32 && rc <= HrcLAST);
   return rc;
}

static inline UInt hregIndex ( HReg r )
{
   return r.u32 & 0xFFFFF;
}

static inline UInt hregEncoding ( HReg r )
{
   return (r.u32 >> 20) & 0x7F;
}

static inline Bool hregIsVirtual ( HReg r )
{
   return toBool((r.u32 >> 31) & 1);
}

static inline Bool sameHReg ( HReg r1, HReg r2 )
{
   return toBool(r1.u32 == r2.u32);
}

static const HReg INVALID_HREG = { .u32 = 0xFFFFFFFF };

static inline Bool hregIsInvalid ( HReg r )
{
   return sameHReg(r, INVALID_HREG);
}


/*---------------------------------------------------------*/
/*--- Real register Universes.                          ---*/
/*---------------------------------------------------------*/

/* A "Real Register Universe" is a read-only structure that contains
   all information about real registers on a given host.  It serves
   several purposes:

   * defines the mapping from real register indices to the registers
     themselves

   * defines the size of the initial section of that mapping that is
     available to the register allocator for use, so that the register
     allocator can treat the registers under its control as a zero
     based, contiguous array.  This is important for its efficiency.

   * gives meaning to RRegSets, which otherwise would merely be a
     bunch of bits.

   This is a big structure, but it's readonly, and we expect to
   allocate only one instance for each run of Valgrind.  It is sized
   so as to be able to deal with up to 64 real registers.  AFAICS none
   of the back ends actually mention more than 64, despite the fact
   that many of the host architectures have more than 64 registers
   when all classes are taken into consideration.
*/

#define N_RREGUNIVERSE_REGS 64

typedef
   struct {
      /* Total number of registers in this universe .. */
      UInt size;
      /* .. of which the first |allocable| are available to regalloc. */
      UInt allocable;
      /* The registers themselves.  All must be real registers, and
         all must have their index number (.s.ix) equal to the array
         index here, since this is the only place where we map index
         numbers to actual registers. */
      HReg regs[N_RREGUNIVERSE_REGS];

      /* Ranges for groups of allocable registers. Used to quickly address only
         a group of allocable registers belonging to the same register class.
         Indexes into |allocable_{start,end}| are HRcClass entries, such as
         HRcInt64. Values in |allocable_{start,end}| give a valid range into
         |regs| where registers corresponding to the given register class are
         found.

         For example, let's say allocable_start[HRcInt64] == 10 and
         allocable_end[HRcInt64] == 14. Then regs[10], regs[11], regs[12],
         regs[13], and regs[14] give all registers of register class HRcInt64.

         If a register class is not present, then values of the corresponding
         |allocable_{start,end}| elements are equal to N_RREGUNIVERSE_REGS.

         Naturally registers in |regs| must form contiguous groups. This is
         checked by RRegUniverse__check_is_sane(). */
      UInt allocable_start[HrcLAST + 1];
      UInt allocable_end[HrcLAST + 1];
   }
   RRegUniverse;

/* Nominally initialise (zero out) an RRegUniverse. */
void RRegUniverse__init ( /*OUT*/RRegUniverse* );

/* Check an RRegUniverse is valid, and assert if not.*/
void RRegUniverse__check_is_sane ( const RRegUniverse* );

/* Print an RRegUniverse, for debugging. */
void RRegUniverse__show ( const RRegUniverse* );


/*---------------------------------------------------------*/
/*--- Real register sets.                               ---*/
/*---------------------------------------------------------*/

/* Represents sets of real registers.  |bitset| is interpreted in the
   context of |univ|.  That is, each bit index |i| in |bitset|
   corresponds to the register |univ->regs[i]|.  This relies
   entirely on the fact that N_RREGUNIVERSE_REGS <= 64. */
typedef
   struct {
      ULong         bitset;
      RRegUniverse* univ;
   }
   RRegSet;


/*---------------------------------------------------------*/
/*--- Recording register usage (for reg-alloc)          ---*/
/*---------------------------------------------------------*/

typedef
   enum { HRmRead, HRmWrite, HRmModify }
   HRegMode;


/* This isn't entirely general, and is specialised towards being fast,
   for the reg-alloc.  It represents real registers using a bitmask
   and can also represent up to four virtual registers, in an
   unordered array.  This is based on the observation that no
   instruction that we generate can mention more than four registers
   at once. 
*/
#define N_HREGUSAGE_VREGS 5

typedef
   struct {
      /* The real registers.  The associated universe is not stored
         here -- callers will have to pass it around separately, as
         needed. */
      ULong    rRead;     /* real regs that are read */
      ULong    rWritten;  /* real regs that are written */
      /* The virtual registers. */
      HReg     vRegs[N_HREGUSAGE_VREGS];
      HRegMode vMode[N_HREGUSAGE_VREGS];
      UInt     n_vRegs;

      /* Hint to the register allocator: this instruction is actually a move
         between two registers: regMoveSrc -> regMoveDst. */
      Bool     isRegRegMove;
      HReg     regMoveSrc;
      HReg     regMoveDst;

      /* Used internally by the register allocator. The reg-reg move is
         actually a vreg-vreg move. */
      Bool     isVregVregMove;
   }
   HRegUsage;

extern void ppHRegUsage ( const RRegUniverse*, HRegUsage* );

static inline void initHRegUsage ( HRegUsage* tab )
{
   tab->rRead        = 0;
   tab->rWritten     = 0;
   tab->n_vRegs      = 0;
   tab->isRegRegMove = False;
}

/* Add a register to a usage table.  Combine incoming read uses with
   existing write uses into a modify use, and vice versa.  Do not
   create duplicate entries -- each reg should only be mentioned once.  
*/
extern void addHRegUse ( HRegUsage*, HRegMode, HReg );

extern Bool HRegUsage__contains ( const HRegUsage*, HReg );


/*---------------------------------------------------------*/
/*--- Indicating register remappings (for reg-alloc)    ---*/
/*---------------------------------------------------------*/

/* Note that such maps can only map virtual regs to real regs.
   addToHRegRemap will barf if given a pair not of that form.  As a
   result, no valid HRegRemap will bind a real reg to anything, and so
   if lookupHRegMap is given a real reg, it returns it unchanged.
   This is precisely the behaviour that the register allocator needs
   to impose its decisions on the instructions it processes.  */

#define N_HREG_REMAP 6

typedef
   struct {
      HReg orig       [N_HREG_REMAP];
      HReg replacement[N_HREG_REMAP];
      Int  n_used;
   }
   HRegRemap;

extern void ppHRegRemap     ( HRegRemap* );
extern void addToHRegRemap  ( HRegRemap*, HReg, HReg );
extern HReg lookupHRegRemap ( HRegRemap*, HReg );

static inline void initHRegRemap ( HRegRemap* map )
{
   map->n_used = 0;
}


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

/* Never call this directly.  It's the slow and incomplete path for
   addHInstr. */
__attribute__((noinline))
extern void addHInstr_SLOW ( HInstrArray*, HInstr* );

static inline void addHInstr ( HInstrArray* ha, HInstr* instr )
{
   if (LIKELY(ha->arr_used < ha->arr_size)) {
      ha->arr[ha->arr_used] = instr;
      ha->arr_used++;
   } else {
      addHInstr_SLOW(ha, instr);
   }
}


/*---------------------------------------------------------*/
/*--- C-Call return-location descriptions               ---*/
/*---------------------------------------------------------*/

/* This is common to all back ends.  It describes where the return
   value from a C call is located.  This is important in the case that
   the call is conditional, since the return locations will need to be
   set to 0x555..555 in the case that the call does not happen. */

typedef
   enum {
      RLPri_INVALID,   /* INVALID */
      RLPri_None,      /* no return value (a.k.a C "void") */
      RLPri_Int,       /* in the primary int return reg */
      RLPri_2Int,      /* in both primary and secondary int ret regs */
      RLPri_V128SpRel, /* 128-bit value, on the stack */
      RLPri_V256SpRel  /* 256-bit value, on the stack */
   }
   RetLocPrimary;

typedef
   struct {
      /* Primary description */
      RetLocPrimary pri;
      /* For .pri == RLPri_V128SpRel or RLPri_V256SpRel only, gives
         the offset of the lowest addressed byte of the value,
         relative to the stack pointer.  For all other .how values,
         has no meaning and should be zero. */
      Int spOff;
   }
   RetLoc;

extern void ppRetLoc ( RetLoc rloc );

static inline RetLoc mk_RetLoc_simple ( RetLocPrimary pri ) {
   vassert(pri >= RLPri_INVALID && pri <= RLPri_2Int);
   return (RetLoc){pri, 0};
}

static inline RetLoc mk_RetLoc_spRel ( RetLocPrimary pri, Int off ) {
   vassert(pri >= RLPri_V128SpRel && pri <= RLPri_V256SpRel);
   return (RetLoc){pri, off};
}

static inline Bool is_sane_RetLoc ( RetLoc rloc ) {
   switch (rloc.pri) {
      case RLPri_None: case RLPri_Int: case RLPri_2Int:
         return rloc.spOff == 0;
      case RLPri_V128SpRel: case RLPri_V256SpRel:
         return True;
      default:
         return False;
   }
}

static inline RetLoc mk_RetLoc_INVALID ( void ) {
   return (RetLoc){RLPri_INVALID, 0};
}

static inline Bool is_RetLoc_INVALID ( RetLoc rl ) {
   return rl.pri == RLPri_INVALID && rl.spOff == 0;
}


/*---------------------------------------------------------*/
/*--- Reg alloc: TODO: move somewhere else              ---*/
/*---------------------------------------------------------*/

/* Control of the VEX register allocator. */
typedef
   struct {
      /* The real-register universe to use.  This contains facts about real
         registers, one of which is the set of registers available for
         allocation. */
      const RRegUniverse* univ;

      /* Get info about register usage in this insn. */
      void (*getRegUsage)(HRegUsage*, const HInstr*, Bool);

      /* Apply a reg-reg mapping to an insn. */
      void (*mapRegs)(HRegRemap*, HInstr*, Bool);

      /* Return insn(s) to spill/restore a real register to a spill slot offset.
         Also a function to move between registers.
         And optionally a function to do direct reloads. */
      void    (*genSpill)(HInstr**, HInstr**, HReg, Int, Bool);
      void    (*genReload)(HInstr**, HInstr**, HReg, Int, Bool);
      HInstr* (*genMove)(HReg from, HReg to, Bool);
      HInstr* (*directReload)(HInstr*, HReg, Short);
      UInt    guest_sizeB;

      /* For debug printing only. */
      void (*ppInstr)(const HInstr*, Bool);
      UInt (*ppReg)(HReg);

      /* 32/64bit mode */
      Bool mode64;
   }
   RegAllocControl;

extern HInstrArray* doRegisterAllocation_v2(
   HInstrArray* instrs_in,
   const RegAllocControl* con
);
extern HInstrArray* doRegisterAllocation_v3(
   HInstrArray* instrs_in,
   const RegAllocControl* con
);


#endif /* ndef __VEX_HOST_GENERIC_REGS_H */

/*---------------------------------------------------------------*/
/*---                                     host_generic_regs.h ---*/
/*---------------------------------------------------------------*/
