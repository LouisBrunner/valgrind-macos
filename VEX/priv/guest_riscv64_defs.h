
/*--------------------------------------------------------------------*/
/*--- begin                                   guest_riscv64_defs.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2020-2023 Petr Pavlu
      petr.pavlu@dagobah.cz

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* Only to be used within the guest_riscv64_* files. */

#ifndef __VEX_GUEST_RISCV64_DEFS_H
#define __VEX_GUEST_RISCV64_DEFS_H

#include "libvex_basictypes.h"

#include "guest_generic_bb_to_IR.h"

/*------------------------------------------------------------*/
/*--- riscv64 to IR conversion                             ---*/
/*------------------------------------------------------------*/

/* Convert one riscv64 insn to IR. See the type DisOneInstrFn in
   guest_generic_bb_to_IR.h. */
DisResult disInstr_RISCV64(IRSB*              irbb,
                           const UChar*       guest_code,
                           Long               delta,
                           Addr               guest_IP,
                           VexArch            guest_arch,
                           const VexArchInfo* archinfo,
                           const VexAbiInfo*  abiinfo,
                           VexEndness         host_endness,
                           Bool               sigill_diag);

/* Used by the optimiser to specialise calls to helpers. */
IRExpr* guest_riscv64_spechelper(const HChar* function_name,
                                 IRExpr**     args,
                                 IRStmt**     precedingStmts,
                                 Int          n_precedingStmts);

/* Describes to the optimiser which part of the guest state require precise
   memory exceptions. This is logically part of the guest state description. */
Bool guest_riscv64_state_requires_precise_mem_exns(
   Int minoff, Int maxoff, VexRegisterUpdates pxControl);

extern VexGuestLayout riscv64guest_layout;

/*------------------------------------------------------------*/
/*--- riscv64 guest helpers                                ---*/
/*------------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

/* Calculate resulting flags of a specified floating-point operation. Returns
   a 32-bit value where bits 4:0 contain the fflags in the RISC-V native
   format (NV DZ OF UF NX) and remaining upper bits are zero. */
UInt riscv64g_calculate_fflags_fsqrt_s(Float a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_w_s(Float a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_wu_s(Float a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_s_w(UInt a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_s_wu(UInt a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_l_s(Float a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_lu_s(Float a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_s_l(ULong a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_s_lu(ULong a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fsqrt_d(Double a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_s_d(Double a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_w_d(Double a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_wu_d(Double a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_l_d(Double a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_lu_d(Double a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_d_l(ULong a1, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fcvt_d_lu(ULong a1, UInt rm_RISCV);

UInt riscv64g_calculate_fflags_fadd_s(Float a1, Float a2, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fmul_s(Float a1, Float a2, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fdiv_s(Float a1, Float a2, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fadd_d(Double a1, Double a2, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fmul_d(Double a1, Double a2, UInt rm_RISCV);
UInt riscv64g_calculate_fflags_fdiv_d(Double a1, Double a2, UInt rm_RISCV);

UInt riscv64g_calculate_fflags_fmin_s(Float a1, Float a2);
UInt riscv64g_calculate_fflags_fmax_s(Float a1, Float a2);
UInt riscv64g_calculate_fflags_feq_s(Float a1, Float a2);
UInt riscv64g_calculate_fflags_flt_s(Float a1, Float a2);
UInt riscv64g_calculate_fflags_fle_s(Float a1, Float a2);
UInt riscv64g_calculate_fflags_fmin_d(Double a1, Double a2);
UInt riscv64g_calculate_fflags_fmax_d(Double a1, Double a2);
UInt riscv64g_calculate_fflags_feq_d(Double a1, Double a2);
UInt riscv64g_calculate_fflags_flt_d(Double a1, Double a2);
UInt riscv64g_calculate_fflags_fle_d(Double a1, Double a2);

UInt riscv64g_calculate_fflags_fmadd_s(Float a1,
                                       Float a2,
                                       Float a3,
                                       UInt  rm_RISCV);
UInt riscv64g_calculate_fflags_fmadd_d(Double a1,
                                       Double a2,
                                       Double a3,
                                       UInt   rm_RISCV);

/* Calculate floating-point class. Returns a 64-bit value where bits 9:0
   contains the properties in the RISC-V FCLASS-instruction format and remaining
   upper bits are zero. */
ULong riscv64g_calculate_fclass_s(Float a1);
ULong riscv64g_calculate_fclass_d(Double a1);

#endif /* ndef __VEX_GUEST_RISCV64_DEFS_H */

/*--------------------------------------------------------------------*/
/*--- end                                     guest_riscv64_defs.h ---*/
/*--------------------------------------------------------------------*/
