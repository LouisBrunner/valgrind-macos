/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                     s390_disasm.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2017

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
*/

#ifndef __VEX_S390_DISASM_H
#define __VEX_S390_DISASM_H

#include "libvex_basictypes.h"

/* Macros to encode a command for s390_disasm. */
#undef  P
#define P(a) (S390_ARG_##a)
#undef  ENC1
#define ENC1(a) ((P(DONE) << 4) | P(a))
#undef  ENC2
#define ENC2(a,b) ((P(DONE) << 8) | (P(b) << 4) | P(a))
#undef  ENC3
#define ENC3(a,b,c) ((P(DONE) << 12) | (P(c) << 8) | (P(b) << 4) | P(a))
#undef  ENC4
#define ENC4(a,b,c,d) ((P(DONE) << 16) | (P(d) << 12) | (P(c) << 8) | \
                       (P(b) << 4) | P(a))
#undef  ENC5
#define ENC5(a,b,c,d,e) ((P(DONE) << 20) | (P(e) << 16) | (P(d) << 12) | \
                         (P(c) << 8) | (P(b) << 4) | P(a))
#undef  ENC6
#define ENC6(a,b,c,d,e,f) ((P(DONE) << 24) | (P(f) << 20) | (P(e) << 16) | \
                           (P(d) << 12) | (P(c) << 8) | (P(b) << 4) | P(a))
#undef  ENC7
#define ENC7(a,b,c,d,e,f,g) ((P(DONE) << 28) | (P(g) << 24) | (P(f) << 20) | \
                             (P(e) << 16) | (P(d) << 12) | (P(c) << 8) | \
                             (P(b) << 4) | P(a))

/* The different kinds of operands in an asm insn */
enum {
   S390_ARG_DONE = 0,
   S390_ARG_GPR = 1,
   S390_ARG_FPR = 2,
   S390_ARG_AR = 3,
   S390_ARG_INT = 4,
   S390_ARG_UINT = 5,
   S390_ARG_PCREL = 6,
   S390_ARG_SDXB = 7,
   S390_ARG_UDXB = 8,
   S390_ARG_UDLB = 9,
   S390_ARG_CABM = 10,
   S390_ARG_MNM = 11,
   S390_ARG_XMNM = 12,
   S390_ARG_VR = 13,
   S390_ARG_UDVB = 14,
};

/* The different kinds of extended mnemonics */
enum {
   S390_XMNM_CAB = 0,
   S390_XMNM_BCR = 1,
   S390_XMNM_BC = 2,
   S390_XMNM_BRC = 3,
   S390_XMNM_BRCL = 4,
   S390_XMNM_LOCR = 5,
   S390_XMNM_LOCGR = 6,
   S390_XMNM_LOC = 7,
   S390_XMNM_LOCG = 8,
   S390_XMNM_STOC = 9,
   S390_XMNM_STOCG = 10,
   S390_XMNM_STOCFH = 11,
   S390_XMNM_LOCFH = 12,
   S390_XMNM_LOCFHR = 13,
   S390_XMNM_LOCHI = 14,
   S390_XMNM_LOCGHI = 15,
   S390_XMNM_LOCHHI = 16
};

void s390_disasm(UInt command, ...);

/*---------------------------------------------------------------*/
/*--- end                                       s390_disasm.h ---*/
/*---------------------------------------------------------------*/

#endif /* __VEX_S390_DISASM_H */
