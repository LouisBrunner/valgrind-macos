
/*---------------------------------------------------------------*/
/*--- begin                                 guest_mips_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2017 RT-RK
      mips-valgrind@rt-rk.com

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

/* Only to be used within the guest-mips directory. */

#ifndef __VEX_GUEST_MIPS_DEFS_H
#define __VEX_GUEST_MIPS_DEFS_H

#include "libvex_basictypes.h"
#include "guest_generic_bb_to_IR.h"  /* DisResult */

/*---------------------------------------------------------*/
/*---               mips to IR conversion               ---*/
/*---------------------------------------------------------*/

/* Convert one MIPS insn to IR. See the type DisOneInstrFn in 
   guest_generic_bb_to_IR.h. */
extern DisResult disInstr_MIPS ( IRSB*        irbb,
                                 const UChar* guest_code,
                                 Long         delta,
                                 Addr         guest_IP,
                                 VexArch      guest_arch,
                                 const VexArchInfo* archinfo,
                                 const VexAbiInfo*  abiinfo,
                                 VexEndness   host_endness,
                                 Bool         sigill_diag );

/* Used by the optimiser to specialise calls to helpers. */
extern IRExpr *guest_mips32_spechelper ( const HChar * function_name,
                                         IRExpr ** args,
                                         IRStmt ** precedingStmts,
                                         Int n_precedingStmts );

extern IRExpr *guest_mips64_spechelper ( const HChar * function_name,
                                         IRExpr ** args,
                                         IRStmt ** precedingStmts,
                                         Int n_precedingStmts);

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern
Bool guest_mips32_state_requires_precise_mem_exns ( Int, Int,
                                                    VexRegisterUpdates );

extern
Bool guest_mips64_state_requires_precise_mem_exns ( Int, Int,
                                                    VexRegisterUpdates );

extern VexGuestLayout mips32Guest_layout;
extern VexGuestLayout mips64Guest_layout;

/*---------------------------------------------------------*/
/*---                mips guest helpers                 ---*/
/*---------------------------------------------------------*/
typedef enum {
   CEILWS=0, CEILWD,  CEILLS,  CEILLD,
   FLOORWS,  FLOORWD, FLOORLS, FLOORLD,
   ROUNDWS,  ROUNDWD, ROUNDLS, ROUNDLD,
   TRUNCWS,  TRUNCWD, TRUNCLS, TRUNCLD,
   CVTDS,    CVTDW,   CVTSD,   CVTSW,
   CVTWS,    CVTWD,   CVTDL,   CVTLS,
   CVTLD,    CVTSL,   ADDS,    ADDD,
   SUBS,     SUBD,    DIVS,
   RINTS,    RINTD,
   MAXS,     MAXD,    MINS,    MIND,
   MAXAS,    MAXAD,   MINAS,   MINAD,
   CMPAFS,   CMPAFD,  CMPSAFS, CMPSAFD,
} flt_op;

typedef enum {
   FADDW=0, FADDD, FSUBW, FSUBD, FMULW, FMULD, FDIVW, FDIVD, FMADDW, FMADDD,
   FCAFD, FCAFW, FSAFD, FSAFW, FCEQD, FCEQW, FSEQD, FSEQW, FCLTD, FCLTW, FSLTD,
   FSLTW, FCLED, FCLEW, FSLED, FSLEW, FCNED, FCNEW, FSNED, FSNEW, FCUND, FCUNW,
   FSUND, FSUNW, FCORD, FCORW, FSORD, FSORW, FCUEQD, FCUEQW, FSUEQD, FSUEQW,
   FCUNED, FCUNEW, FSUNED, FSUNEW, FCULED, FCULEW, FSULED, FSULEW, FCULTD,
   FCULTW, FSULTD, FSULTW, FEXP2W, FEXP2D, FMINW, FMIND, FMINAW, FMINAD, FMAXW,
   FMAXD, FMAXAW, FMAXAD, FFINTSW, FFINTSD, FRCPW, FRCPD, FRSQRTW, FRSQRTD,
   FSQRTW, FSQRTD, FRINTW, FRINTD, FTRUNCUW, FTRUNCUD, FTRUNCSW, FTRUNCSD,
   FEXDOH, FEXDOW, FEXUPRD, FEXUPRW, FEXUPLD, FEXUPLW, FLOG2W, FLOG2D,
   FTQH, FTQW, FFQRW, FFQRD,FFQLW, FFQLD, FTINT_SW, FTINT_SD,
   FTINT_UW, FTINT_UD, FFINT_UW, FFINT_UD,
} msa_flt_op;

#if defined (_MIPSEL)
   #define MIPS_IEND Iend_LE
#else
   #define MIPS_IEND Iend_BE
#endif

extern HWord mips_dirtyhelper_rdhwr ( UInt rd );

/* Calculate FCSR in fp32 mode. */
extern UInt mips_dirtyhelper_calculate_FCSR_fp32 ( void* guest_state, UInt fs,
                                                   UInt ft, flt_op op );
/* Calculate FCSR in fp64 mode. */
extern UInt mips_dirtyhelper_calculate_FCSR_fp64 ( void* guest_state, UInt fs,
                                                   UInt ft, flt_op op );

extern UInt mips_dirtyhelper_calculate_MSACSR ( void* gs, UInt ws, UInt wt,
                                                msa_flt_op inst );
extern UInt mips_dirtyhelper_get_MSAIR ( void );


/*---------------------------------------------------------*/
/*---               Condition code stuff                ---*/
/*---------------------------------------------------------*/

typedef enum {
   MIPSCondEQ = 0,   /* equal                         : Z=1 */
   MIPSCondNE = 1,   /* not equal                     : Z=0 */

   MIPSCondHS = 2,   /* >=u (higher or same)          : C=1 */
   MIPSCondLO = 3,   /* <u  (lower)                   : C=0 */

   MIPSCondMI = 4,   /* minus (negative)              : N=1 */
   MIPSCondPL = 5,   /* plus (zero or +ve)            : N=0 */

   MIPSCondVS = 6,   /* overflow                      : V=1 */
   MIPSCondVC = 7,   /* no overflow                   : V=0 */

   MIPSCondHI = 8,   /* >u   (higher)                 : C=1 && Z=0 */
   MIPSCondLS = 9,   /* <=u  (lower or same)          : C=0 || Z=1 */

   MIPSCondGE = 10,  /* >=s (signed greater or equal) : N=V */
   MIPSCondLT = 11,  /* <s  (signed less than)        : N!=V */

   MIPSCondGT = 12,  /* >s  (signed greater)          : Z=0 && N=V */
   MIPSCondLE = 13,  /* <=s (signed less or equal)    : Z=1 || N!=V */

   MIPSCondAL = 14,  /* always (unconditional)        : 1 */
   MIPSCondNV = 15   /* never (unconditional):        : 0 */
} MIPSCondcode;

#endif            /* __VEX_GUEST_MIPS_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                   guest_mips_defs.h ---*/
/*---------------------------------------------------------------*/
