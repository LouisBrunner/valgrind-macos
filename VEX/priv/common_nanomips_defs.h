
/*---------------------------------------------------------------*/
/*--- begin                            common_nanomips_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2018 RT-RK

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/
#ifndef __VEX_COMMON_NANOMIPS_DEFS_H
#define __VEX_COMMON_NANOMIPS_DEFS_H

typedef enum {
   P_ADDIURI = 0x00,
   ADDIUPC32 = 0x01,
   MOVE_BALC = 0x02,
   P16MV     = 0x04,
   LW16      = 0x05,
   BC16      = 0x06,
   P16SR     = 0x07,
   P32A      = 0x08,
   PBAL      = 0x0A,
   P16SHIFT  = 0x0C,
   LWSP      = 0x0D,
   BALC16    = 0x0E,
   P164X4    = 0x0F,
   PGPW      = 0x10,
   PGPBH     = 0x11,
   PJ        = 0x12,
   P16C      = 0x14,
   LWGP16    = 0x15,
   P16LB     = 0x17,
   P48I      = 0x18,
   P16A1     = 0x1C,
   LW4X4     = 0x1D,
   P16LH     = 0x1F,
   PU12      = 0x20,
   PLSU12    = 0x21,
   PBR1      = 0x22,
   P16A2     = 0x24,
   SW16      = 0x25,
   BEQZC16   = 0x26,
   PLSS9     = 0x29,
   PBR2      = 0x2A,
   P16ADDU   = 0x2C,
   SWSP      = 0x2D,
   BNEZC16   = 0x2E,
   MOVEP     = 0x2F,
   PBRI      = 0x32,
   LI16      = 0x34,
   SWGP16    = 0x35,
   P16BR     = 0x36,
   P_LUI     = 0x38,
   ANDI16    = 0x3C,
   SW4X4     = 0x3D,
   MOVEPREV  = 0x3F,
} nanoMIPSopcodes;

typedef enum {
   P48I_LI       = 0x00,
   P48I_ADDIU    = 0x01,
   P48I_ADDIU_GP = 0x02,
   P48I_ADDIUPC  = 0x03,
   P48I_LWPC     = 0x0B,
   P48I_SWPC     = 0x0F,
} nanoP48I;

typedef enum {
   JALRC32 = 0x00,
   JALRCHB = 0x01,
   PBALRSC = 0x08
} nano_PJ;

typedef enum {
   PLSS0   = 0x00,
   PLSS1   = 0x01,
   PLSE0   = 0x02,
   PLSWM   = 0x04,
   PLSUAWM = 0x05,
   PLSDM   = 0x06,
   PLSUADM = 0x07,
} nanoPLSS9;

typedef enum {
   PU12_ORI       = 0x00,
   PU12_XORI      = 0x01,
   PU12_ANDI      = 0x02,
   PU12_PSR       = 0x03,
   PU12_SLTI      = 0x04,
   PU12_SLTIU     = 0x05,
   PU12_SEQI      = 0x06,
   PU12_ADDIU_NEG = 0x08,
   PU12_PSHIFT    = 0x0C,
   PU12_PROTX     = 0x0D,
   PU12_PINS      = 0x0E,
   PU12_PEXT      = 0x0F
} nanoPU12;

typedef enum {
   RI_PSYSCALL  = 0x1,
   RI_BREAK     = 0x2,
   RI_SDBBP     = 0x3
} nanoP16RI;

typedef enum {
   PRI_SIGRIE    = 0x0,
   PRI_PSYSCALL  = 0x1,
   PRI_BREAK     = 0x2,
   PRI_SDBBP     = 0x3
} nanoPRI;

typedef enum {
   P32A_POOL32A0 = 0x00,
   P32A_POOL32A7 = 0x07
} nano_P32A;

typedef enum {
   _POOL32A0_PTRAP  = 0x00,
   _POOL32A0_SEB    = 0x01,
   _POOL32A0_SLLV   = 0x02,
   _POOL32A0_MUL32  = 0x03,
   _POOL32A0_MFC0   = 0x06,
   _POOL32A0_MFHC0  = 0x07,
   _POOL32A0_SEH    = 0x09,
   _POOL32A0_SRLV   = 0x0A,
   _POOL32A0_MUH    = 0x0B,
   _POOL32A0_MTC0   = 0x0E,
   _POOL32A0_MTHC0  = 0x0F,
   _POOL32A0_SRAV   = 0x12,
   _POOL32A0_MULU   = 0x13,
   _POOL32A0_MFGC0  = 0x16,
   _POOL32A0_MFHGC0 = 0x17,
   _POOL32A0_ROTRV  = 0x1A,
   _POOL32A0_MUHU   = 0x1B,
   _POOL32A0_MTGC0  = 0x1E,
   _POOL32A0_MTHGC0 = 0x1F,
   _POOL32A0_ADD    = 0x22,
   _POOL32A0_DIV    = 0x23,
   _POOL32A0_DMFC0  = 0x26,
   _POOL32A0_ADDU32 = 0x2A,
   _POOL32A0_MOD    = 0x2B,
   _POOL32A0_DMTC0  = 0x2E,
   _POOL32A0_SUB    = 0x32,
   _POOL32A0_DIVU   = 0x33,
   _POOL32A0_DMFGC0 = 0x36,
   _POOL32A0_RDHWR  = 0x38,
   _POOL32A0_SUBU32 = 0x3A,
   _POOL32A0_MODU   = 0x3B,
   _POOL32A0_DMTGC0 = 0x3E,
   _POOL32A0_PCMOVE = 0x42,
   _POOL32A0_FORK   = 0x45,
   _POOL32A0_MFTR   = 0x46,
   _POOL32A0_MFHTR  = 0x47,
   _POOL32A0_AND32  = 0x4A,
   _POOL32A0_YIELD  = 0x4D,
   _POOL32A0_MTTR   = 0x4E,
   _POOL32A0_MTHTR  = 0x4F,
   _POOL32A0_OR32   = 0x52,
   _POOL32A0_PMTVPE = 0x56,
   _POOL32A0_NOR    = 0x5A,
   _POOL32A0_XOR32  = 0x62,
   _POOL32A0_SLT    = 0x6A,
   _POOL32A0_PSLTU  = 0x72,
   _POOL32A0_SOV    = 0x7A,
} nano_POOL32A0;

typedef enum {
   _POOL32A7_PLSX    = 0x00,
   _POOL32A7_LSA     = 0x01,
   _POOL32A7_EXTW    = 0x03,
   _POOL32A7_P32Axf  = 0x07,
} nano_POOL32A7;

typedef enum {
   nano_POOL32Axf4_CLO = 0x25,
   nano_POOL32Axf4_CLZ = 0x2D,
} nano_POOL32Axf4;

typedef enum {
   PLSX_PPLSX  = 0x00,
   PLSX_PPLSXS = 0x01,
} nano_PLSX;

typedef enum {
   LBX   = 0x00,
   SBX   = 0x01,
   LBUX  = 0x02,
   LHX   = 0x04,
   SHX   = 0x05,
   LHUX  = 0x06,
   LWUX  = 0x07,
   LWX   = 0x08,
   SWX   = 0x09,
   LWC1X = 0x0A,
   SWC1X = 0x0B,
   LDX   = 0x0C,
   SDX   = 0x0D,
   LDC1X = 0x0E,
   SDC1X = 0x0F
} nano_PPLSX;

typedef enum {
   LHXS   = 0x04,
   SHXS   = 0x05,
   LHUXS  = 0x06,
   LWUXS  = 0x07,
   LWXS32 = 0x08,
   SWXS   = 0x09,
   LWC1XS = 0x0A,
   SWC1XS = 0x0B,
   LDXS   = 0x0C,
   SDXS   = 0x0D,
   LDC1XS = 0x0E,
   SDC1XS = 0x0F
} nano_PPLSXS;

typedef enum {
   PLSU12_LB    = 0x00,
   PLSU12_SB    = 0x01,
   PLSU12_LBU   = 0x02,
   PLSU12_PREF  = 0x03,
   PLSU12_LH    = 0x04,
   PLSU12_SH    = 0x05,
   PLSU12_LHU   = 0x06,
   PLSU12_LWU   = 0x07,
   PLSU12_LW    = 0x08,
   PLSU12_SW    = 0x09,
   PLSU12_LWC1  = 0x0A,
   PLSU12_SWC1  = 0x0B,
   PLSU12_LD    = 0x0C,
   PLSU12_SD    = 0x0D,
   PLSU12_LDC1  = 0x0E,
   PLSU12_SDC1  = 0x0F,

} nano_PLSU12;

typedef enum {
   PSLL      = 0x00,
   SRL32     = 0x02,
   SRA       = 0x04,
   ROTR      = 0x06,
   DSLL      = 0x08,
   DSLL32    = 0x09,
   DSRL      = 0x0A,
   DSRL32    = 0x0B,
   DSRA      = 0x0C,
   DSRA32    = 0x0D,
   DROTR     = 0x0E,
   DROTR32   = 0x0F,
} nano_PSHIFT;

typedef enum {
   LBS9     = 0x00,
   SBS9     = 0x01,
   LBUS9    = 0x02,
   PPREFS9  = 0x03,
   LHS9     = 0x04,
   SHS9     = 0x05,
   LHUS9    = 0x06,
   LWUS9    = 0x07,
   LWS9     = 0x08,
   SWS9     = 0x09,
   LWC1S9   = 0x0A,
   SWC1S9   = 0x0B,
   LDS9     = 0x0C,
   SDS9     = 0x0D,
   LDC1S9   = 0x0E,
   SDC1S9   = 0x0F,
} nano_PLSS0;

typedef enum {
   LBGP     = 0x00,
   SBGP     = 0x01,
   LBUGP    = 0x02,
   ADDIUGPB = 0x03,
   PGPLH    = 0x04,
   PGPSH    = 0x05,
   PGPCP1   = 0x06,
   PGPM64   = 0x07
} nano_PGPBH;

typedef enum {
   ASET_ACLER = 0x02,
   UALH       = 0x04,
   UASH       = 0x05,
   CACHE      = 0x07,
   LWC2       = 0x08,
   SWC2       = 0x09,
   PLL        = 0x0A,
   PSC        = 0x0B,
   LDC2       = 0x0C,
   SDC2       = 0x0D,
   PLLD       = 0x0E,
   PSCD       = 0x0F
} nano_PLSS1;

typedef enum {
   LL   = 0x00,
   LLWP = 0x01
} nano_LL;

typedef enum {
   SC   = 0x00,
   SCWP = 0x01
} nano_SC;

typedef enum {
   PBR1_BEQC32 = 0x00,
   PBR1_PBR3A  = 0x01,
   PBR1_BGEC   = 0x02,
   PBR1_BGEUC  = 0x03,
} nano_PBR1;

typedef enum {
   PBR2_BNEC32 = 0x00,
   PBR2_BLTC   = 0x02,
   PBR2_BLTUC  = 0x03,
} nano_PBR2;

typedef enum {
   PBRI_BEQIC  = 0x00,
   PBRI_BBEQZC = 0x01,
   PBRI_BGEIC  = 0x02,
   PBRI_BGEIUC = 0x03,
   PBRI_BNEIC  = 0x04,
   PBRI_BBNEZC = 0x05,
   PBRI_BLTIC  = 0x06,
   PBRI_BLTIUC = 0x07
} nano_PBRI;

typedef enum {
   PGPW_ADDIU = 0x00,
   PGPW_PGPD  = 0X01,
   PGPW_LW  = 0X02,
   PGPW_SW  = 0X03
} nano_PGPW;

typedef enum {
   POOL32aXF_4 = 0x04,
   POOL32aXF_5 = 0x05,
} nano_POOL32Axf;

typedef enum {
   POOL16C00_NOT = 0x00,
   POOL16C00_XOR = 0x04,
   POOL16C00_AND = 0x08,
   POOL16C00_OR  = 0x0C,
} nano_POOL16C_00;

#endif

/*---------------------------------------------------------------*/
/*--- end                              common_nanomips_defs.h ---*/
/*---------------------------------------------------------------*/
