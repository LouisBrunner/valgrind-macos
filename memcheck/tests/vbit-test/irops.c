
/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2012-2017  Florian Krohm

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include <stdio.h>    // fprintf
#include <stdlib.h>   // exit
#include "pub_tool_basics.h"   // STATIC_ASSERT
#include "vtest.h"

#define DEFOP(op,ukind) op, #op, ukind

/* The opcodes appear in the same order here as in libvex_ir.h
   That is not necessary but helpful when supporting a new architecture.
*/
static irop_t irops[] = {
  { DEFOP(Iop_Add8,    UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Add16,   UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Add32,   UNDEF_INT_ADD), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Add64,   UNDEF_INT_ADD), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_Sub8,    UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Sub16,   UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Sub32,   UNDEF_INT_SUB), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Sub64,   UNDEF_INT_SUB), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_Mul8,    UNDEF_LEFT), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_Mul16,   UNDEF_LEFT), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_Mul32,   UNDEF_LEFT), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Mul64,   UNDEF_LEFT), .s390x = 0, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_Or1,     UNDEF_OR),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Or8,     UNDEF_OR),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Or16,    UNDEF_OR),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Or32,    UNDEF_OR),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Or64,    UNDEF_OR),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_And1,    UNDEF_AND),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_And8,    UNDEF_AND),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_And16,   UNDEF_AND),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_And32,   UNDEF_AND),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_And64,   UNDEF_AND),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Xor8,    UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Xor16,   UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Xor32,   UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Xor64,   UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Shl8,    UNDEF_SHL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_Shl16,   UNDEF_SHL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_Shl32,   UNDEF_SHL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Shl64,   UNDEF_SHL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32 asserts
  { DEFOP(Iop_Shr8,    UNDEF_SHR),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // ppc32/64 assert
  { DEFOP(Iop_Shr16,   UNDEF_SHR),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 }, // ppc32/64 assert
  { DEFOP(Iop_Shr32,   UNDEF_SHR),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Shr64,   UNDEF_SHR),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32 asserts
  { DEFOP(Iop_Sar8,    UNDEF_SAR),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // ppc32/64 assert
  { DEFOP(Iop_Sar16,   UNDEF_SAR),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 }, // ppc32/64 assert
  { DEFOP(Iop_Sar32,   UNDEF_SAR),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Sar64,   UNDEF_SAR),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 1, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32 asserts
  { DEFOP(Iop_CmpEQ8,  UNDEF_CMP_EQ_NE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CmpEQ16, UNDEF_CMP_EQ_NE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_CmpEQ32, UNDEF_CMP_EQ_NE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_CmpEQ64, UNDEF_CMP_EQ_NE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_CmpNE8,  UNDEF_CMP_EQ_NE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CmpNE16, UNDEF_CMP_EQ_NE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CmpNE32, UNDEF_CMP_EQ_NE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_CmpNE64, UNDEF_CMP_EQ_NE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_Not1,       UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Not8,       UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Not16,      UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Not32,      UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_Not64,      UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CasCmpEQ8,  UNDEF_NONE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CasCmpEQ16, UNDEF_NONE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CasCmpEQ32, UNDEF_NONE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CasCmpEQ64, UNDEF_NONE), .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },

  { DEFOP(Iop_CasCmpNE8,  UNDEF_NONE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CasCmpNE16, UNDEF_NONE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CasCmpNE32, UNDEF_NONE), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CasCmpNE64, UNDEF_NONE), .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_ExpCmpNE8,  UNDEF_UNKNOWN), }, // exact (expensive) equality
  { DEFOP(Iop_ExpCmpNE16, UNDEF_UNKNOWN), }, // exact (expensive) equality
  { DEFOP(Iop_ExpCmpNE32, UNDEF_UNKNOWN), }, // exact (expensive) equality
  { DEFOP(Iop_ExpCmpNE64, UNDEF_UNKNOWN), }, // exact (expensive) equality
  { DEFOP(Iop_MullS8,     UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_MullS16,    UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_MullS32,    UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  // s390 has signed multiplication of 64-bit values but the result
  // is 64-bit (not 128-bit). So we cannot test this op standalone.
  { DEFOP(Iop_MullS64,    UNDEF_LEFT), .s390x = 0, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 =0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_MullU8,     UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_MullU16,    UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_MullU32,    UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 =0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_MullU64,    UNDEF_LEFT), .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 =0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_Clz64,      UNDEF_ALL),  .s390x = 0, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 =0, .mips64 = 1 }, // ppc32 asserts
  { DEFOP(Iop_Clz32,      UNDEF_ALL),  .s390x = 0, .amd64 = 0, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 =1, .mips64 = 1 },
  { DEFOP(Iop_Ctz64,      UNDEF_ALL),  .s390x = 0, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_Ctz32,      UNDEF_ALL),  .s390x = 0, .amd64 = 0, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_ClzNat64,   UNDEF_ALL),  .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 =0, .mips64 = 0 }, // ppc32 asserts
  { DEFOP(Iop_ClzNat32,   UNDEF_ALL),  .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_CtzNat64,   UNDEF_ALL),  .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_CtzNat32,   UNDEF_ALL),  .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 1, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_PopCount64, UNDEF_ALL),  .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_PopCount32, UNDEF_ALL),  .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 =0, .mips64 = 0 },
  { DEFOP(Iop_CmpLT32S,   UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 =1, .mips64 = 1 },
  { DEFOP(Iop_CmpLT64S,   UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 =0, .mips64 = 1 }, // ppc, mips assert
  { DEFOP(Iop_CmpLE32S,   UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 =1, .mips64 = 1 },
  { DEFOP(Iop_CmpLE64S,   UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 =0, .mips64 = 1 }, // ppc, mips assert
  { DEFOP(Iop_CmpLT32U,   UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 =1, .mips64 = 1 },
  { DEFOP(Iop_CmpLT64U,   UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 =0, .mips64 = 1}, // ppc32, mips assert
  { DEFOP(Iop_CmpLE32U,   UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 =1, .mips64 = 1 },
  { DEFOP(Iop_CmpLE64U,   UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 =0, .mips64 = 0 }, // ppc32 asserts
  { DEFOP(Iop_CmpNEZ8,    UNDEF_ALL), },   // not supported by mc_translate
  { DEFOP(Iop_CmpNEZ16,   UNDEF_ALL), },   // not supported by mc_translate
  { DEFOP(Iop_CmpNEZ32,   UNDEF_ALL), },   // not supported by mc_translate
  { DEFOP(Iop_CmpNEZ64,   UNDEF_ALL), },   // not supported by mc_translate
  { DEFOP(Iop_CmpwNEZ32,  UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_CmpwNEZ64,  UNDEF_ALL),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_Left8,      UNDEF_UNKNOWN), },  // not supported by mc_translate
  { DEFOP(Iop_Left16,     UNDEF_UNKNOWN), },  // not supported by mc_translate
  { DEFOP(Iop_Left32,     UNDEF_UNKNOWN), },  // not supported by mc_translate
  { DEFOP(Iop_Left64,     UNDEF_UNKNOWN), },  // not supported by mc_translate
  { DEFOP(Iop_Max32U,     UNDEF_UNKNOWN), },  // not supported by mc_translate
  { DEFOP(Iop_CmpORD32U,  UNDEF_ORD), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 }, // support added in vbit-test
  { DEFOP(Iop_CmpORD64U,  UNDEF_ORD), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // support added in vbit-test
  { DEFOP(Iop_CmpORD32S,  UNDEF_ORD), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 }, // support added in vbit-test
  { DEFOP(Iop_CmpORD64S,  UNDEF_ORD), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // support added in vbit-test
  { DEFOP(Iop_DivU32,     UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_DivS32,     UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_DivU64,     UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // ppc32 asserts
  { DEFOP(Iop_DivS64,     UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // ppc32 asserts
  { DEFOP(Iop_DivU64E,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // ppc32 asserts
  { DEFOP(Iop_DivS64E,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // ppc32 asserts
  { DEFOP(Iop_DivU32E,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_DivS32E,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  // On s390 the DivMod operations always appear in a certain context
  // So they cannot be tested in isolation on that platform.
  { DEFOP(Iop_DivModU64to32,  UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_DivModS64to32,  UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_DivModU32to32,  UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_DivModS32to32,  UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_DivModU128to64, UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // mips asserts
  { DEFOP(Iop_DivModS128to64, UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // mips asserts
  { DEFOP(Iop_DivModS64to64,  UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_DivModU64to64,  UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_8Uto16,    UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_8Uto32,    UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_8Uto64,    UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32 assert
  { DEFOP(Iop_16Uto32,   UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_16Uto64,   UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32 assert
  { DEFOP(Iop_32Uto64,   UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_8Sto16,    UNDEF_SEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_8Sto32,    UNDEF_SEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_8Sto64,    UNDEF_SEXT),   .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_16Sto32,   UNDEF_SEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_16Sto64,   UNDEF_SEXT),   .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_32Sto64,   UNDEF_SEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_64to8,     UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 1, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_32to8,     UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_64to16,    UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_16to8,     UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_16HIto8,   UNDEF_UPPER),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_8HLto16,   UNDEF_CONCAT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },  // ppc isel
  { DEFOP(Iop_32to16,    UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_32HIto16,  UNDEF_UPPER),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_16HLto32,  UNDEF_CONCAT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },  // ppc isel
  { DEFOP(Iop_64to32,    UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_64HIto32,  UNDEF_UPPER),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_32HLto64,  UNDEF_CONCAT), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_128to64,   UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_128HIto64, UNDEF_UPPER),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_64HLto128, UNDEF_CONCAT), .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_32to1,     UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_64to1,     UNDEF_TRUNC),  .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32, mips assert
  { DEFOP(Iop_1Uto8,     UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_1Uto32,    UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_1Uto64,    UNDEF_ZEXT),   .s390x = 1, .amd64 = 1, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // ppc32 assert
  { DEFOP(Iop_1Sto8,     UNDEF_ALL),    .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_1Sto16,    UNDEF_ALL), }, // not handled by mc_translate
  { DEFOP(Iop_1Sto32,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_1Sto64,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_AddF64,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_SubF64,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_MulF64,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_DivF64,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_AddF32,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_AddF16,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .arm64 = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_SubF32,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_SubF16,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .arm64 = 1,.ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_MulF32,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_DivF32,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_AddF64r32, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_SubF64r32, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_MulF64r32, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_DivF64r32, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_NegF64,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_AbsF64,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_NegF32,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_NegF16,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .arm64 = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_AbsF32,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_AbsF16,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .arm64 = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_SqrtF64,   UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_SqrtF32,   UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_SqrtF16,   UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .arm64 = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CmpF64,    UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_CmpF32,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 }, // mips asserts
  { DEFOP(Iop_CmpF16,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .arm64 = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CmpF128,   UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F64toI16S, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F64toI32S, UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_F64toI64S, UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F64toI64U, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F64toI32U, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_I32StoF64, UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_I64StoF64, UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_I64UtoF64, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 }, // mips asserts
  { DEFOP(Iop_I64UtoF32, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_I32UtoF32, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_I32UtoF64, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F32toI32S, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F32toI64S, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_F32toI32U, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F32toI64U, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_I32StoF32, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_I64StoF32, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_F32toF64,  UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_F64toF32,  UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_ReinterpF64asI64, UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_ReinterpI64asF64, UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_ReinterpF32asI32, UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 1, .ppc32 = 1, .mips32 = 1, .mips64 = 1 },
  // ppc requires this op to show up in a specific context. So it cannot be
  // tested standalone on that platform.
  { DEFOP(Iop_ReinterpI32asF32, UNDEF_SAME), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 1, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_F64HLtoF128, UNDEF_CONCAT),    .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128HItoF64, UNDEF_UPPER),     .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128LOtoF64, UNDEF_TRUNC), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_AddF128,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_SubF128,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_MulF128,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_DivF128,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_MAddF128,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_MSubF128,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_NegMAddF128, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_NegMSubF128, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_NegF128,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_AbsF128,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_SqrtF128,      UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_I32StoF128,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_I64StoF128,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_I32UtoF128,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_I64UtoF128,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F32toF128,     UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F64toF128,     UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128toI32S,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128toI64S,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128toI32U,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128toI64U,    UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128toI128S,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128toF64,     UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_F128toF32,     UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_RndF128,        UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_TruncF128toI32S,UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_TruncF128toI32U,UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_TruncF128toI64U,UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_TruncF128toI64S,UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_AtanF64,       UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_Yl2xF64,       UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_Yl2xp1F64,     UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_PRemF64,       UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_PRemC3210F64,  UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_PRem1F64,      UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_PRem1C3210F64, UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_ScaleF64,      UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_SinF64,        UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_CosF64,        UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_TanF64,        UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_2xm1F64,       UNDEF_ALL), .s390x = 0, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_RoundF128toInt, UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_RoundF64toInt, UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_RoundF32toInt, UNDEF_ALL), .s390x = 1, .amd64 = 1, .x86 = 1, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 1, .mips64 = 1 },
  { DEFOP(Iop_MAddF32,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_MSubF32,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 0, .ppc32 = 0, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_MAddF64,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_MSubF64,       UNDEF_ALL), .s390x = 1, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_MAddF64r32,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_MSubF64r32,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_RSqrtEst5GoodF64,      UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_RoundF64toF64_NEAREST, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_RoundF64toF64_NegINF,  UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_RoundF64toF64_PosINF,  UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_RoundF64toF64_ZERO,    UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 },
  { DEFOP(Iop_TruncF64asF32, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 1 }, // mips asserts
  { DEFOP(Iop_RoundF64toF32, UNDEF_ALL), .s390x = 0, .amd64 = 0, .x86 = 0, .arm = 0, .ppc64 = 1, .ppc32 = 1, .mips32 = 0, .mips64 = 0 },
  { DEFOP(Iop_RecpExpF64, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecpExpF32, UNDEF_UNKNOWN), },

  /* --------- Possibly required by IEEE 754-2008. --------- */
  { DEFOP(Iop_MaxNumF64, UNDEF_ALL), .arm = 1 },
  { DEFOP(Iop_MinNumF64, UNDEF_ALL), .arm = 1 },
  { DEFOP(Iop_MaxNumF32, UNDEF_ALL), .arm = 1 },
  { DEFOP(Iop_MinNumF32, UNDEF_ALL), .arm = 1 },

  /* ------------------ 16-bit scalar FP ------------------ */
  { DEFOP(Iop_F16toF64,  UNDEF_ALL), .arm64 = 1 },
  { DEFOP(Iop_F64toF16,  UNDEF_ALL), .arm64 = 1 },
  { DEFOP(Iop_F16toF32,  UNDEF_ALL), .arm64 = 1 },
  { DEFOP(Iop_F32toF16,  UNDEF_ALL), .arm64 = 1 },

  /* ------------------ 32-bit SIMD Integer ------------------ */
  { DEFOP(Iop_QAdd32S, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub32S, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add16x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub16x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd16Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd16Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub16Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub16Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_HAdd16Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_HAdd16Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_HSub16Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_HSub16Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add8x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub8x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd8Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd8Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub8Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub8Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_HAdd8Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_HAdd8Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_HSub8Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_HSub8Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sad8Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ16x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ8x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse8sIn32_x1, UNDEF_UNKNOWN) },
  /* ------------------ 64-bit SIMD FP ------------------------ */
  { DEFOP(Iop_I32UtoF32x2_DEP, UNDEF_UNKNOWN), },
  { DEFOP(Iop_I32StoF32x2_DEP, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toI32Ux2_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toI32Sx2_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32ToFixed32Ux2_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32ToFixed32Sx2_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Fixed32UToF32x2_RN, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Fixed32SToF32x2_RN, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMax32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMin32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGE32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipEst32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipStep32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtEst32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtStep32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Neg32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs32Fx2, UNDEF_UNKNOWN), },
  /* ------------------ 64-bit SIMD Integer. ------------------ */
  { DEFOP(Iop_CmpNEZ8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd64Ux1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd64Sx1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAdd8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAdd16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAdd32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMax8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMax16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMax32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMax8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMax16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMax32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMin8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMin16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMin32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMin8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMin16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMin32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub64Ux1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub64Sx1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PolynomialMul8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QDMulHi16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QDMulHi32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QRDMulHi16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QRDMulHi32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Cnt8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Clz8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Clz16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Clz32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Cls8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Cls16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Cls32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shl8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shl16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shl32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shr8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shr16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shr32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sar8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sar16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sar32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sal8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sal16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sal32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sal64x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShl8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShl16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShl32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShl64x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSal8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSal16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSal32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSal64x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSU8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSU16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSU32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSU64x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatUU8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatUU16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatUU32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatUU64x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSS8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSS16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSS32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSS64x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin16Sto8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin16Sto8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin32Sto16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_NarrowBin16to8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_NarrowBin32to16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveHI8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveHI16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveHI32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveLO8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveLO16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveLO32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveOddLanes8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveEvenLanes8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveOddLanes16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveEvenLanes16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatOddLanes8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatOddLanes16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatEvenLanes8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatEvenLanes16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetElem8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetElem16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetElem32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetElem8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetElem16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetElem32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Dup8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Dup16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Dup32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Slice64, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse8sIn16_x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse8sIn32_x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse16sIn32_x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse8sIn64_x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse16sIn64_x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse32sIn64_x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Perm8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PermOrZero8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetMSBs8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipEst32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtEst32Ux2, UNDEF_UNKNOWN), },
  /* ------------------ Decimal Floating Point ------------------ */
  { DEFOP(Iop_AddD64,                UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_SubD64,                UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_MulD64,                UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_DivD64,                UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_AddD128,               UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_SubD128,               UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_MulD128,               UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_DivD128,               UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ShlD64,                UNDEF_ALL),  .s390x = 0, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ShrD64,                UNDEF_ALL),  .s390x = 0, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ShlD128,               UNDEF_ALL),  .s390x = 0, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ShrD128,               UNDEF_ALL),  .s390x = 0, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D32toD64,              UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D64toD128,             UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_I32StoD128,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_I32UtoD128,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_I64StoD128,            UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_I64UtoD128,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D64toD32,              UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D128toD64,             UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_I32StoD64,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_I32UtoD64,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_I64StoD64,             UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_I64UtoD64,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D64toI32S,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D64toI32U,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D64toI64S,             UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D64toI64U,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D128toI64S,            UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D128toI64U,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D128toI32S,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D128toI32U,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F32toD32,              UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F32toD64,              UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F32toD128,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F64toD32,              UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F64toD64,              UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F64toD128,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F128toD32,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F128toD64,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_F128toD128,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D32toF32,              UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D32toF64,              UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D32toF128,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D64toF32,              UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D64toF64,              UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D64toF128,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D128toF32,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D128toF64,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_D128toF128,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_RoundD64toInt,         UNDEF_ALL),  .s390x = 0, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_RoundD128toInt,        UNDEF_ALL),  .s390x = 0, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_CmpD64,                UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_CmpD128,               UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_CmpExpD64,             UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_CmpExpD128,            UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_QuantizeD64,           UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_QuantizeD128,          UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_SignificanceRoundD64,  UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_SignificanceRoundD128, UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ExtractExpD64,         UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ExtractExpD128,        UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ExtractSigD64,         UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_ExtractSigD128,        UNDEF_ALL),  .s390x = 1, .ppc64 = 0, .ppc32 = 0 },
  { DEFOP(Iop_InsertExpD64,          UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_InsertExpD128,         UNDEF_ALL),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D64HLtoD128,           UNDEF_CONCAT), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D128HItoD64,           UNDEF_UPPER),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D128LOtoD64,           UNDEF_TRUNC),  .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_DPBtoBCD,              UNDEF_ALL),    .s390x = 0, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_BCDtoDPB,              UNDEF_ALL),    .s390x = 0, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ReinterpI64asD64,      UNDEF_SAME),   .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ReinterpD64asI64,      UNDEF_SAME),   .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  /* ------------------ 128-bit SIMD FP. ------------------ */
  { DEFOP(Iop_Add16Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub16Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Div32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLT32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLE32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpUN32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGE32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMax32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwMin32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs16Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sqrt32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sqrt16Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Scale2_32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Log2_32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Exp2_32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Neg32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Neg16Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipEst32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipStep32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtEst32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtStep32Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_I32UtoF32x4_DEP, UNDEF_UNKNOWN), },
  { DEFOP(Iop_I32StoF32x4_DEP, UNDEF_UNKNOWN), },
  { DEFOP(Iop_I32StoF32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toI32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toI32Ux4_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toI32Sx4_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QF32toI32Ux4_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QF32toI32Sx4_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RoundF32x4_RM, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RoundF32x4_RP, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RoundF32x4_RN, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RoundF32x4_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32ToFixed32Ux4_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32ToFixed32Sx4_RZ, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Fixed32UToF32x4_RN, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Fixed32SToF32x4_RN, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toF16x4_DEP, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toF16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F16toF32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F64toF16x2_DEP, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F16toF64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32x4_2toQ16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F64x2_2toQ32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Div32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLT32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLE32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpUN32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipEst32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sqrt32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtEst32F0x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Div64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLT64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLE64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLT16Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLE16Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ16Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpUN64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sqrt64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Scale2_64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Log2_64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Neg64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipEst64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipStep64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtEst64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtStep64Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Div64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLT64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpLE64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpUN64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sqrt64F0x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V128to64, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V128HIto64, UNDEF_UNKNOWN), },
  { DEFOP(Iop_64HLtoV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_64UtoV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetV128lo64, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ZeroHI64ofV128, UNDEF_UNKNOWN) },
  { DEFOP(Iop_ZeroHI96ofV128, UNDEF_UNKNOWN) },
  { DEFOP(Iop_ZeroHI112ofV128, UNDEF_UNKNOWN) },
  { DEFOP(Iop_ZeroHI120ofV128, UNDEF_UNKNOWN) },
  { DEFOP(Iop_32UtoV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V128to32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetV128lo32, UNDEF_UNKNOWN), },
  /* ------------------ 128-bit SIMD Integer. ------------------ */
  { DEFOP(Iop_NotV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_AndV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_OrV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_XorV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ128x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add128x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd64Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd64Sx2, UNDEF_UNKNOWN), },

  { DEFOP(Iop_QAddExtUSsatSS8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAddExtUSsatSS16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAddExtUSsatSS32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAddExtUSsatSS64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAddExtSUsatUU8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAddExtSUsatUU16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAddExtSUsatUU32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAddExtSUsatUU64x2, UNDEF_UNKNOWN), },

  { DEFOP(Iop_Sub8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub128x1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub64Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub64Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi32Sx4, UNDEF_UNKNOWN), },
  /* Result of the Iop_MullEvenBxE is 2*BxE/2 */
  { DEFOP(Iop_MullEven8Ux16, UNDEF_ALL_8x16_EVEN), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_MullEven16Ux8, UNDEF_ALL_16x8_EVEN), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_MullEven32Ux4, UNDEF_ALL_32x4_EVEN), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_MullEven8Sx16, UNDEF_ALL_8x16_EVEN), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_MullEven16Sx8, UNDEF_ALL_16x8_EVEN), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_MullEven32Sx4, UNDEF_ALL_32x4_EVEN), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_Mull8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mull8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mull16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mull16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mull32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mull32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QDMulHi16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QDMulHi32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QRDMulHi16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QRDMulHi32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QDMull16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QDMull32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PolynomialMul8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PolynomialMull8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAdd8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAdd16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAdd32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAdd32Fx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL64Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwAddL32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PwExtUSMulQAdd8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Abs64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg64Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg64Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max64Sx2, UNDEF_ALL_64x2), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_Max8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max64Ux2, UNDEF_ALL_64x2), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_Min8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min64Sx2, UNDEF_ALL_64x2), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_Min8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min64Ux2, UNDEF_ALL_64x2), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_CmpEQ8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT64Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT64Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Cnt8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Clz8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Clz16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Clz32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Clz64x2, UNDEF_ALL_64x2), .s390x = 1, .ppc64 = 1, .ppc32 = 1  },
  { DEFOP(Iop_Ctz8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Ctz16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Ctz32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Ctz64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Cls8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Cls16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Cls32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shl8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shl16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shl32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shl64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shr8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shr16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shr32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Shr64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sar8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sar16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sar32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sar64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sal8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sal16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sal32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sal64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rol8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rol16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rol32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rol64x2, UNDEF_64x2_ROTATE), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_QShl8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShl16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShl32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShl64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSal8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSal16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSal32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSal64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSU8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSU16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSU32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSU64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatUU8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatUU16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatUU32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatUU64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSS8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSS16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSS32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QShlNsatSS64x2, UNDEF_UNKNOWN), },

  { DEFOP(Iop_QandUQsh8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandUQsh16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandUQsh32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandUQsh64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandSQsh8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandSQsh16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandSQsh32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandSQsh64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandUQRsh8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandUQRsh16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandUQRsh32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandUQRsh64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandSQRsh8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandSQRsh16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandSQRsh32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandSQRsh64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sh8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sh16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sh32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sh64Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sh8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sh16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sh32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sh64Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rsh8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rsh16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rsh32Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rsh64Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rsh8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rsh16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rsh32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Rsh64Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQShrNnarrow16Uto8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQShrNnarrow32Uto16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQShrNnarrow64Uto32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQSarNnarrow16Sto8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQSarNnarrow32Sto16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQSarNnarrow64Sto32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQSarNnarrow16Sto8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQSarNnarrow32Sto16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQSarNnarrow64Sto32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRShrNnarrow16Uto8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRShrNnarrow32Uto16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRShrNnarrow64Uto32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRSarNnarrow16Sto8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRSarNnarrow32Sto16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRSarNnarrow64Sto32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRSarNnarrow16Sto8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRSarNnarrow32Sto16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QandQRSarNnarrow64Sto32Ux2, UNDEF_UNKNOWN), },

  { DEFOP(Iop_QNarrowBin16Sto8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin32Sto16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin16Sto8Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin32Sto16Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin16Uto8Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin32Uto16Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_NarrowBin16to8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_NarrowBin32to16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_NarrowBin64to32x4, UNDEF_NARROW256_AtoB), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_NarrowUn16to8x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_NarrowUn32to16x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_NarrowUn64to32x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowUn16Sto8Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowUn32Sto16Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowUn64Sto32Sx2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin64Sto32Sx4, UNDEF_NARROW256_AtoB), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_QNarrowUn16Sto8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowUn32Sto16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowUn64Sto32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowBin64Uto32Ux4, UNDEF_NARROW256_AtoB), .s390x = 1, .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_QNarrowUn16Uto8Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowUn32Uto16Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QNarrowUn64Uto32Ux2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Widen8Uto16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Widen16Uto32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Widen32Uto64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Widen8Sto16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Widen16Sto32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Widen32Sto64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveHI8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveHI16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveHI32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveHI64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveLO8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveLO16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveLO32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveLO64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveOddLanes8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveEvenLanes8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveOddLanes16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveEvenLanes16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveOddLanes32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_InterleaveEvenLanes32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PackOddLanes8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PackEvenLanes8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PackOddLanes16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PackEvenLanes16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PackOddLanes32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PackEvenLanes32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatOddLanes8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatOddLanes16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatOddLanes32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatEvenLanes8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatEvenLanes16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CatEvenLanes32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetElem8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetElem16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetElem32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetElem64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetElem8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetElem16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetElem32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SetElem64x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Dup8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Dup16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Dup32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SliceV128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse8sIn16_x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse8sIn32_x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse16sIn32_x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse8sIn64_x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse16sIn64_x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse32sIn64_x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Reverse1sIn8_x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Perm8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PermOrZero8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Perm32x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Perm8x16x2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_GetMSBs8x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipEst32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtEst32Ux4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulI128by10, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulI128by10Carry, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulI128by10E, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulI128by10ECarry, UNDEF_UNKNOWN), },

  /* ------------------ 256-bit SIMD Integer. ------------------ */
  { DEFOP(Iop_V256to64_0, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V256to64_1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V256to64_2, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V256to64_3, UNDEF_UNKNOWN), },
  { DEFOP(Iop_64x4toV256, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V256toV128_0, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V256toV128_1, UNDEF_UNKNOWN), },
  { DEFOP(Iop_V128HLtoV256, UNDEF_UNKNOWN), },
  { DEFOP(Iop_AndV256, UNDEF_UNKNOWN), },
  { DEFOP(Iop_OrV256,  UNDEF_UNKNOWN), },
  { DEFOP(Iop_XorV256, UNDEF_UNKNOWN), },
  { DEFOP(Iop_NotV256, UNDEF_UNKNOWN), },
  /* --------------- MISC (vector integer cmp != 0) -------------*/
  { DEFOP(Iop_CmpNEZ8x32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ16x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpNEZ64x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add8x32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add16x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add64x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub8x32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub16x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub64x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ8x32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ16x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpEQ64x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT8Sx32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT16Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT32Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_CmpGT64Sx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN16x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShlN64x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN16x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_ShrN64x4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN16x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_SarN32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max8Sx32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max16Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max8Ux32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max16Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min8Sx32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min16Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min8Ux32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min16Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Ux8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul16x16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi16Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_MulHi16Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd8Ux32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd16Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd8Sx32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QAdd16Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub8Ux32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub16Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub8Sx32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_QSub16Sx16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg8Ux32, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Avg16Ux16, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Perm32x8, UNDEF_UNKNOWN), },
  /* ------------------ 256-bit SIMD FP. ------------------ */
  { DEFOP(Iop_Add64Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub64Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul64Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Div64Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Add32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sub32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Mul32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Div32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_I32StoF32x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toI32Sx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F32toF16x8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_F16toF32x8, UNDEF_UNKNOWN) },
  { DEFOP(Iop_Sqrt32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Sqrt64Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RSqrtEst32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_RecipEst32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min32Fx8, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Max64Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_Min64Fx4, UNDEF_UNKNOWN), },
  { DEFOP(Iop_BCDAdd, UNDEF_ALL), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_BCDSub, UNDEF_ALL), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_I128StoBCD128, UNDEF_UNKNOWN), },
  { DEFOP(Iop_BCD128toI128S, UNDEF_UNKNOWN), },
  { DEFOP(Iop_PolynomialMulAdd8x16, UNDEF_ALL_8x16), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_PolynomialMulAdd16x8, UNDEF_ALL_16x8), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_PolynomialMulAdd32x4, UNDEF_ALL_32x4), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_PolynomialMulAdd64x2, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_CipherV128,   UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_CipherLV128,  UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_CipherSV128,  UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_NCipherV128,  UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_NCipherLV128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_SHA512, UNDEF_SOME), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_SHA256, UNDEF_SOME), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_Rotx32, UNDEF_ALL), },
  { DEFOP(Iop_Rotx64, UNDEF_ALL), },
  { DEFOP(Iop_PwBitMtxXpose64x2, UNDEF_64x2_TRANSPOSE), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_DivU128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_DivS128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_DivU128E, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_DivS128E, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ModU128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ModS128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_2xMultU64Add128CarryOut, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_TruncF128toI128U, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_TruncF128toI128S, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_I128UtoF128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_I128StoF128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_I128StoD128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_D128toI128S, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ReinterpF128asI128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ReinterpI128asF128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ReinterpV128asI128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
  { DEFOP(Iop_ReinterpI128asV128, UNDEF_ALL_64x2), .ppc64 = 1, .ppc32 = 1 },
};

/* Force compile time failure in case libvex_ir.h::IROp was updated
   and the irops array is out of synch */
STATIC_ASSERT \
      (sizeof irops / sizeof *irops == Iop_LAST - Iop_INVALID - 1);

/* Return a descriptor for OP, iff it exists and it is implemented
   for the current architecture. */
irop_t *
get_irop(IROp op)
{
   unsigned i;

   for (i = 0; i < sizeof irops / sizeof *irops; ++i) {
      irop_t *p = irops + i;
      if (p->op == op) {
#ifdef __s390x__
#define S390X_FEATURES "../../../tests/s390x_features"
        int rc;

         switch (op) {
         case Iop_I32StoD64:    // CDFTR
         case Iop_I32StoD128:   // CXFTR
         case Iop_I32UtoD64:    // CDLFTR
         case Iop_I32UtoD128:   // CXLFTR
         case Iop_I64UtoD64:    // CDLGTR
         case Iop_I64UtoD128:   // CXLGTR
         case Iop_D64toI32S:    // CFDTR
         case Iop_D128toI32S:   // CFXTR
         case Iop_D64toI64U:    // CLGDTR
         case Iop_D64toI32U:    // CLFDTR
         case Iop_D128toI64U:   // CLGXTR
         case Iop_D128toI32U:   // CLFXTR
         case Iop_I32UtoF32:
         case Iop_I32UtoF64:
         case Iop_I32UtoF128:
         case Iop_I64UtoF32:
         case Iop_I64UtoF64:
         case Iop_I64UtoF128:
         case Iop_F32toI32U:
         case Iop_F32toI64U:
         case Iop_F64toI32U:
         case Iop_F64toI64U:
         case Iop_F128toI32U:
         case Iop_F128toI64U: {
            /* These IROps require the floating point extension facility */
            rc = system(S390X_FEATURES " s390x-fpext");
            // s390x_features returns 1 if feature does not exist
            rc /= 256;
            if (rc != 0) return NULL;
         }
         break;
         /* PFPO Iops */
         case Iop_F32toD32:
         case Iop_F32toD64:
         case Iop_F32toD128:
         case Iop_F64toD32:
         case Iop_F64toD64:
         case Iop_F64toD128:
         case Iop_F128toD32:
         case Iop_F128toD64:
         case Iop_F128toD128:
         case Iop_D32toF32:
         case Iop_D32toF64:
         case Iop_D32toF128:
         case Iop_D64toF32:
         case Iop_D64toF64:
         case Iop_D64toF128:
         case Iop_D128toF32:
         case Iop_D128toF64:
         case Iop_D128toF128: {
            /* These IROps require the Perform Floating Point Operation
               facility */
            rc = system(S390X_FEATURES " s390x-pfpo");
            // s390x_features returns 1 if feature does not exist
            rc /= 256;
            if (rc != 0) return NULL;
         }
         break;
         /* Other */
         default:
            break;
         }
         return p->s390x ? p : NULL;
#endif
#ifdef __x86_64__
         return p->amd64 ? p : NULL;
#endif
#ifdef __powerpc__
#define  MIN_POWER_ISA  "../../../tests/min_power_isa"
         int rc;

         switch (op) {
         case Iop_DivS64E:
         case Iop_DivU64E:
         case Iop_DivU32E:
         case Iop_DivS32E:
         case Iop_F64toI64U:
         case Iop_F64toI32U:
         case Iop_I64UtoF64:
         case Iop_I64UtoF32:
         case Iop_I64StoD64: {
            /* IROps require a processor that supports ISA 2.06 (Power 7) or newer */
            rc = system(MIN_POWER_ISA " 2.06 ");
            rc /= 256;
            /* MIN_POWER_ISA returns 0 if underlying HW supports the
             * specified ISA or newer. Returns 1 if the HW does not support
             * the specified ISA.  Returns 2 on error.
             */
            if (rc == 1) return NULL;
            if (rc > 2) {
               panic(" ERROR, min_power_isa() return code is invalid.\n");
            }
         }
         break;

         case Iop_PwBitMtxXpose64x2:
         case Iop_Clz64x2:
         case Iop_BCDAdd:
         case Iop_BCDSub:
         case Iop_PolynomialMulAdd8x16:
         case Iop_PolynomialMulAdd16x8:
         case Iop_PolynomialMulAdd32x4:
         case Iop_PolynomialMulAdd64x2:
         case Iop_CipherV128:
         case Iop_CipherLV128:
         case Iop_CipherSV128:
         case Iop_NCipherV128:
         case Iop_NCipherLV128:
         case Iop_SHA512:
         case Iop_SHA256:
         case Iop_MullEven8Ux16:
         case Iop_MullEven16Ux8:
         case Iop_MullEven32Ux4:
         case Iop_MullEven8Sx16:
         case Iop_MullEven16Sx8:
         case Iop_MullEven32Sx4:
         case Iop_Max64Sx2:
         case Iop_Max64Ux2:
         case Iop_Min64Sx2:
         case Iop_Min64Ux2:
         case Iop_CmpGT64Ux2:
         case Iop_Rol64x2:
         case Iop_QNarrowBin64Sto32Sx4:
         case Iop_QNarrowBin64Uto32Ux4:
         case Iop_NarrowBin64to32x4: {
            /* IROps require a processor that supports ISA 2.07 (Power 8) or newer */
            rc = system(MIN_POWER_ISA " 2.07 ");
            rc /= 256;
            /* MIN_POWER_ISA returns 0 if underlying HW supports the
             * specified ISA or newer. Returns 1 if the HW does not support
             * the specified ISA.  Returns 2 on error.
             */
            if (rc == 1) return NULL;
            if (rc > 2) {
               panic(" ERROR, min_power_isa() return code is invalid.\n");
            }
         }
         break;

         case Iop_MAddF128:
         case Iop_MSubF128:
         case Iop_NegMAddF128:
         case Iop_NegMSubF128:
         case Iop_F128toI128S:
         case Iop_RndF128:
         case Iop_I64UtoF128:
         case Iop_I64StoF128:
         case Iop_F64toF128:
         case Iop_F128toF64:
         case Iop_F128toF32:
         case Iop_TruncF128toI32S:
         case Iop_TruncF128toI32U:
         case Iop_TruncF128toI64U:
         case Iop_TruncF128toI64S:
         case Iop_F16toF32x4:
         case Iop_F32toF16x4_DEP:
         case Iop_F64toF16x2_DEP:
         case Iop_F16toF64x2:
         case Iop_MulI128by10:
         case Iop_MulI128by10Carry:
         case Iop_MulI128by10E:
         case Iop_MulI128by10ECarry: {
            /* IROps require a processor that supports ISA 3.00 (Power 9) or newer */
            rc = system(MIN_POWER_ISA " 3.00 ");
            rc /= 256;
            /* MIN_POWER_ISA returns 0 if underlying HW supports the
             * specified ISA or newer. Returns 1 if the HW does not support
             * the specified ISA.  Returns 2 on error.
             */
            if (rc == 1) return NULL;
            if (rc > 2) {
               panic(" ERROR, min_power_isa() return code is invalid.\n");
            }
         }
         break;
         case Iop_DivU128:
         case Iop_DivS128:
         case Iop_DivU128E:
         case Iop_DivS128E:
         case Iop_ModU128:
         case Iop_ModS128:
         case Iop_ReinterpV128asI128:
         case Iop_ReinterpI128asV128:
         case Iop_ReinterpF128asI128:
         case Iop_ReinterpI128asF128:
         case Iop_I128UtoF128:
         case Iop_I128StoF128:
         case Iop_TruncF128toI128U:
         case Iop_TruncF128toI128S:
         case Iop_I128StoD128:
         case Iop_D128toI128S:
         case Iop_2xMultU64Add128CarryOut: {
            /* IROps require a processor that supports ISA 3.10 (Power 10)
               or newer */
            rc = system(MIN_POWER_ISA " 3.1 ");
            rc /= 256;
            /* MIN_POWER_ISA returns 0 if underlying HW supports the
             * specified ISA or newer. Returns 1 if the HW does not support
             * the specified ISA.  Returns 2 on error.
             */
            if (rc == 1) return NULL;
            if (rc > 2) {
               panic(" ERROR, min_power_isa() return code is invalid.\n");
            }
         }
         break;
         /* Other */
         default:
         break;
         }

#ifdef __powerpc64__
         return p->ppc64 ? p : NULL;
#else
         return p->ppc32 ? p : NULL;
#endif
#endif
#ifdef __mips__
#if (__mips==64)
         return p->mips64 ? p : NULL;
#else
         return p->mips32 ? p : NULL;
#endif
#endif
#ifdef __arm__
         return p->arm ? p : NULL;
#endif
#ifdef __i386__
         return p->x86 ? p : NULL;
#endif
         return NULL;
      }
   }

   fprintf(stderr, "unknown opcode %d\n", op);
   exit(1);
}
