/*---------------------------------------------------------------*/
/*--- begin                      libvex_guest_arm64_sysregs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2026 Paul Floyd
      pjfloyd@wanadoo.fr

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __LIBVEX_PUB_GUEST_ARM64_SYSREGS_H
#define __LIBVEX_PUB_GUEST_ARM64_SYSREGS_H

/*---------------------------------------------------------------*/
/*--- arm64 system register field definitions                 ---*/
/*---------------------------------------------------------------*/

/* These definitions are used when reading system registers
 * to mask unsupported fields. They are also used by Valgrind
 * during startup when determining hardware capabilities. */

/* ID_AA64ISAR0_EL1 Instruction set attribute register 0 fields */
#define ID_AA64ISAR0_FHM_SHIFT            48
#define ID_AA64ISAR0_DP_SHIFT             44
#define ID_AA64ISAR0_SM4_SHIFT            40
#define ID_AA64ISAR0_SM3_SHIFT            36
#define ID_AA64ISAR0_SHA3_SHIFT           32
#define ID_AA64ISAR0_RDM_SHIFT            28
#define ID_AA64ISAR0_ATOMICS_SHIFT        20
#define ID_AA64ISAR0_CRC32_SHIFT          16
#define ID_AA64ISAR0_SHA2_SHIFT           12
#define ID_AA64ISAR0_SHA1_SHIFT           8
#define ID_AA64ISAR0_AES_SHIFT            4
#define ID_AA64ISAR0_RES0_SHIFT           0
/* Field values */
#define ID_AA64ISAR0_FHM_SUPPORTED        0x1
#define ID_AA64ISAR0_DP_SUPPORTED         0x1
#define ID_AA64ISAR0_SM4_SUPPORTED        0x1
#define ID_AA64ISAR0_SM3_SUPPORTED        0x1
#define ID_AA64ISAR0_SHA3_SUPPORTED       0x1
#define ID_AA64ISAR0_RDM_SUPPORTED        0x1
#define ID_AA64ISAR0_ATOMICS_SUPPORTED    0x2

/* ID_AA64ISAR1_EL1 Instruction set attribute register 1 fields */
#define ID_AA64ISAR1_I8MM_SHIFT           52
#define ID_AA64ISAR1_BF16_SHIFT           44
#define ID_AA64ISAR1_DPB_SHIFT             0
/* Field values */
#define ID_AA64ISAR1_I8MM_SUPPORTED       0x1
#define ID_AA64ISAR1_BF16_SUPPORTED       0x1
#define ID_AA64ISAR1_DPBCVAP_SUPPORTED    0x1
#define ID_AA64ISAR1_DPBCVADP_SUPPORTED   0x2

/* ID_AA64PFR0_EL1 Processor feature register 0 fields */
#define ID_AA64PFR0_CSV3_SHIFT            60
#define ID_AA64PFR0_CSV2_SHIFT            56
#define ID_AA64PFR0_RME_SHIFT             52
#define ID_AA64PFR0_DIT_SHIFT             48
#define ID_AA64PFR0_AMU_SHIFT             44
#define ID_AA64PFR0_MPAM_SHIFT            40
#define ID_AA64PFR0_SEL2_SHIFT            36
#define ID_AA64PFR0_SVE_SHIFT             32
#define ID_AA64PFR0_RAS_SHIFT             28
#define ID_AA64PFR0_GIC_SHIFT             24
#define ID_AA64PFR0_ADVSIMD_SHIFT         20
#define ID_AA64PFR0_FP_SHIFT              16
#define ID_AA64PFR0_EL3_SHIFT             12
#define ID_AA64PFR0_EL2_SHIFT             8
#define ID_AA64PFR0_EL1_SHIFT             4
#define ID_AA64PFR0_EL0_SHIFT             0
/* Field values */
#define ID_AA64PFR0_FP_NHP_SUPPORTED      0x0 /* FP but no half precision */
#define ID_AA64PFR0_FP_HP_SUPPORTED       0x1 /* FP and half precision */
#define ID_AA64PFR0_FP_NOT_PRESENT        0xf /* no FP present */
#define ID_AA64PFR0_ADVSIMD_NHP_SUPPORTED 0x0
#define ID_AA64PFR0_ADVSIMD_HP_SUPPORTED  0x1
#define ID_AA64PFR0_ADVSIMD_NOT_PRESENT   0xf

/* ID_AA64MMFR1_EL1 memory model feature register */
#define ID_AA64MMFR1_RES0_SHIFT           60
#define ID_AA64MMFR1_CMOW_SHIFT           56
#define ID_AA64MMFR1_TIDCP1_SHIFT         52
#define ID_AA64MMFR1_NTLBPA_SHIFT         48
#define ID_AA64MMFR1_AFP_SHIFT            44
#define ID_AA64MMFR1_HCX_SHIFT            40
#define ID_AA64MMFR1_ETS_SHIFT            36
#define ID_AA64MMFR1_TWED_SHIFT           32
#define ID_AA64MMFR1_XNX_SHIFT            28
#define ID_AA64MMFR1_SPECSEI_SHIFT        24
#define ID_AA64MMFR1_PAN_SHIFT            20
#define ID_AA64MMFR1_LO_SHIFT             16
#define ID_AA64MMFR1_HPDS_SHIFT           12
#define ID_AA64MMFR1_VH_SHIFT             8
#define ID_AA64MMFR1_VMIDBITS_SHIFT       4
#define ID_AA64MMFR1_HAFDBS_SHIFT         0

#define SYSTEM_REGISTER_FIELD(val, shift) ((((val) >> (shift)) & 0xfULL))

/* Feature support is specified in nibbles. That gives 16 possible
 * levels. Usually 0 means no support. After that each successive
 * level is a superset of the previous one. That means that if we do
 * not fully support a level then we need to clamp to the previous
 * level. */
#define CLAMP_REGISTER_FIELD_INPLACE(val, shift, limit) \
   do {                                                 \
      if (SYSTEM_REGISTER_FIELD(val, shift) > (ULong)(limit)) { \
         val &= ~(0xfULL << (shift)); \
         val |= ((ULong)(limit) << (shift)); \
      } \
   } while (0)

#define MAKE_SYSTEM_REGISTER_MASK_FIELD(shift) (0xfULL << (shift))
#define MASK_SYSTEM_REGISTER_FIELDS(val, mask) (val) &= (mask)

#endif /* ifndef __LIBVEX_PUB_GUEST_ARM64_SYSREGS_H */


/*---------------------------------------------------------------*/
/*---                            libvex_guest_arm64_sysregs.h ---*/
/*---------------------------------------------------------------*/
