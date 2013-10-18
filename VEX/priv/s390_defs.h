/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                       s390_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2013

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

#ifndef __VEX_S390_DEFS_H
#define __VEX_S390_DEFS_H


/* Condition code. The encoding of the enumerators matches the value of
   the mask field in the various branch opcodes. */
typedef enum {
   S390_CC_NEVER  =  0,
   S390_CC_OVFL   =  1,   /* overflow */
   S390_CC_H      =  2,   /* A > B ; high */
   S390_CC_NLE    =  3,   /* not low or equal */
   S390_CC_L      =  4,   /* A < B ; low */
   S390_CC_NHE    =  5,   /* not high or equal */
   S390_CC_LH     =  6,   /* low or high */
   S390_CC_NE     =  7,   /* A != B ; not zero */
   S390_CC_E      =  8,   /* A == B ; zero */
   S390_CC_NLH    =  9,   /* not low or high */
   S390_CC_HE     = 10,   /* A >= B ; high or equal*/
   S390_CC_NL     = 11,   /* not low */
   S390_CC_LE     = 12,   /* A <= B ; low or equal */
   S390_CC_NH     = 13,   /* not high */
   S390_CC_NO     = 14,   /* not overflow */
   S390_CC_ALWAYS = 15
} s390_cc_t;


/* Invert the condition code */
static __inline__ s390_cc_t
s390_cc_invert(s390_cc_t cond)
{
   return S390_CC_ALWAYS - cond;
}


/* BFP Rounding mode as it is encoded in the m3 field of certain
   instructions (e.g. CFEBR) */
typedef enum {
   S390_BFP_ROUND_PER_FPC       = 0,
   S390_BFP_ROUND_NEAREST_AWAY  = 1,
   /* 2 is not allowed */
   S390_BFP_ROUND_PREPARE_SHORT = 3,
   S390_BFP_ROUND_NEAREST_EVEN  = 4,
   S390_BFP_ROUND_ZERO          = 5,
   S390_BFP_ROUND_POSINF        = 6,
   S390_BFP_ROUND_NEGINF        = 7
} s390_bfp_round_t;


/* BFP Rounding mode as it is encoded in bits [29:31] of the FPC register.
   Only rounding modes 0..3 are universally supported. Others require
   additional hardware facilities. */
typedef enum {
   S390_FPC_BFP_ROUND_NEAREST_EVEN  = 0,
   S390_FPC_BFP_ROUND_ZERO          = 1,
   S390_FPC_BFP_ROUND_POSINF        = 2,
   S390_FPC_BFP_ROUND_NEGINF        = 3,
   /* 4,5,6 are not allowed */
   S390_FPC_BFP_ROUND_PREPARE_SHORT = 7 /* floating point extension facility */
} s390_fpc_bfp_round_t;


/* DFP Rounding mode as it is encoded in the m3 field of certain
   instructions (e.g. CGDTR) */
typedef enum {
   S390_DFP_ROUND_PER_FPC_0             = 0,
   S390_DFP_ROUND_NEAREST_TIE_AWAY_0_1  = 1,
   S390_DFP_ROUND_PER_FPC_2             = 2,
   S390_DFP_ROUND_PREPARE_SHORT_3       = 3,
   S390_DFP_ROUND_NEAREST_EVEN_4        = 4,
   S390_DFP_ROUND_ZERO_5                = 5,
   S390_DFP_ROUND_POSINF_6              = 6,
   S390_DFP_ROUND_NEGINF_7              = 7,
   S390_DFP_ROUND_NEAREST_EVEN_8        = 8,
   S390_DFP_ROUND_ZERO_9                = 9,
   S390_DFP_ROUND_POSINF_10             = 10,
   S390_DFP_ROUND_NEGINF_11             = 11,
   S390_DFP_ROUND_NEAREST_TIE_AWAY_0_12 = 12,
   S390_DFP_ROUND_NEAREST_TIE_TOWARD_0  = 13,
   S390_DFP_ROUND_AWAY_0                = 14,
   S390_DFP_ROUND_PREPARE_SHORT_15      = 15
} s390_dfp_round_t;


/* DFP Rounding mode as it is encoded in bits [25:27] of the FPC register. */
typedef enum {
   S390_FPC_DFP_ROUND_NEAREST_EVEN     = 0,
   S390_FPC_DFP_ROUND_ZERO             = 1,
   S390_FPC_DFP_ROUND_POSINF           = 2,
   S390_FPC_DFP_ROUND_NEGINF           = 3,
   S390_FPC_DFP_ROUND_NEAREST_AWAY_0   = 4,
   S390_FPC_DFP_ROUND_NEAREST_TOWARD_0 = 5,
   S390_FPC_DFP_ROUND_AWAY_ZERO        = 6,
   S390_FPC_DFP_ROUND_PREPARE_SHORT    = 7
} s390_fpc_dfp_round_t;

/* PFPO function code as it is encoded in bits [33:55] of GR0
   when PFPO insn is executed. */
typedef enum {
   S390_PFPO_F32_TO_D32   = 0x010805,
   S390_PFPO_F32_TO_D64   = 0x010905,
   S390_PFPO_F32_TO_D128  = 0x010A05,
   S390_PFPO_F64_TO_D32   = 0x010806,
   S390_PFPO_F64_TO_D64   = 0x010906,
   S390_PFPO_F64_TO_D128  = 0x010A06,
   S390_PFPO_F128_TO_D32  = 0x010807,
   S390_PFPO_F128_TO_D64  = 0x010907,
   S390_PFPO_F128_TO_D128 = 0x010A07,
   S390_PFPO_D32_TO_F32   = 0x010508,
   S390_PFPO_D32_TO_F64   = 0x010608,
   S390_PFPO_D32_TO_F128  = 0x010708,
   S390_PFPO_D64_TO_F32   = 0x010509,
   S390_PFPO_D64_TO_F64   = 0x010609,
   S390_PFPO_D64_TO_F128  = 0x010709,
   S390_PFPO_D128_TO_F32  = 0x01050A,
   S390_PFPO_D128_TO_F64  = 0x01060A,
   S390_PFPO_D128_TO_F128 = 0x01070A
} s390_pfpo_function_t;

/* The length of the longest mnemonic: locgrnhe */
#define S390_MAX_MNEMONIC_LEN  8


/*---------------------------------------------------------------*/
/*--- end                                         s390_defs.h ---*/
/*---------------------------------------------------------------*/

#endif /* __VEX_S390_DEFS_H */
