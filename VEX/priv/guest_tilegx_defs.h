/*---------------------------------------------------------------*/
/*--- begin                               guest_tilegx_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2015 Tilera Corp.

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

 /* Contributed by Zhi-Gang Liu <zliu at tilera dot com> */

#ifndef __VEX_GUEST_TILEGX_DEFS_H
#define __VEX_GUEST_TILEGX_DEFS_H

#ifdef __tilegx__
#include "tilegx_disasm.h"
#endif

/*---------------------------------------------------------*/
/*--- tilegx to IR conversion                           ---*/
/*---------------------------------------------------------*/

/* Convert one TILEGX insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern DisResult disInstr_TILEGX ( IRSB* irbb,
                                   Bool (*resteerOkFn) ( void *, Addr ),
                                   Bool resteerCisOk,
                                   void* callback_opaque,
                                   const UChar* guest_code,
                                   Long delta,
                                   Addr guest_IP,
                                   VexArch guest_arch,
                                   const VexArchInfo* archinfo,
                                   const VexAbiInfo* abiinfo,
                                   VexEndness host_endness_IN,
                                   Bool sigill_diag_IN );

/* Used by the optimiser to specialise calls to helpers. */
extern IRExpr *guest_tilegx_spechelper ( const HChar * function_name,
                                         IRExpr ** args,
                                         IRStmt ** precedingStmts,
                                         Int n_precedingStmts );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern Bool guest_tilegx_state_requires_precise_mem_exns (
  Int, Int, VexRegisterUpdates );

extern VexGuestLayout tilegxGuest_layout;

/*---------------------------------------------------------*/
/*--- tilegx guest helpers                              ---*/
/*---------------------------------------------------------*/

extern ULong tilegx_dirtyhelper_gen ( ULong opc,
                                      ULong rd0,
                                      ULong rd1,
                                      ULong rd2,
                                      ULong rd3 );

/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* Defines conditions which we can ask for TILEGX */

typedef enum {
  TILEGXCondEQ = 0,      /* equal                         : Z=1 */
  TILEGXCondNE = 1,      /* not equal                     : Z=0 */
  TILEGXCondHS = 2,      /* >=u (higher or same)          : C=1 */
  TILEGXCondLO = 3,      /* <u  (lower)                   : C=0 */
  TILEGXCondMI = 4,      /* minus (negative)              : N=1 */
  TILEGXCondPL = 5,      /* plus (zero or +ve)            : N=0 */
  TILEGXCondVS = 6,      /* overflow                      : V=1 */
  TILEGXCondVC = 7,      /* no overflow                   : V=0 */
  TILEGXCondHI = 8,      /* >u   (higher)                 : C=1 && Z=0 */
  TILEGXCondLS = 9,      /* <=u  (lower or same)          : C=0 || Z=1 */
  TILEGXCondGE = 10,     /* >=s (signed greater or equal) : N=V */
  TILEGXCondLT = 11,     /* <s  (signed less than)        : N!=V */
  TILEGXCondGT = 12,     /* >s  (signed greater)          : Z=0 && N=V */
  TILEGXCondLE = 13,     /* <=s (signed less or equal)    : Z=1 || N!=V */
  TILEGXCondAL = 14,     /* always (unconditional)        : 1 */
  TILEGXCondNV = 15      /* never (unconditional):        : 0 */
} TILEGXCondcode;

#endif            /* __VEX_GUEST_TILEGX_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                 guest_tilegx_defs.h ---*/
/*---------------------------------------------------------------*/
