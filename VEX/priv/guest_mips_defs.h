
/*---------------------------------------------------------------*/
/*--- begin                                 guest_mips_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2012 RT-RK
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

/* Only to be used within the guest-mips directory. */

#ifndef __VEX_GUEST_MIPS_DEFS_H
#define __VEX_GUEST_MIPS_DEFS_H

/*---------------------------------------------------------*/
/*--- mips to IR conversion                             ---*/
/*---------------------------------------------------------*/

/* Convert one MIPS insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern DisResult disInstr_MIPS ( IRSB*        irbb,
                                 Bool         (*resteerOkFn) (void *, Addr64),
                                 Bool         resteerCisOk,
                                 void*        callback_opaque,
                                 UChar*       guest_code,
                                 Long         delta,
                                 Addr64       guest_IP,
                                 VexArch      guest_arch,
                                 VexArchInfo* archinfo,
                                 VexAbiInfo*  abiinfo,
                                 Bool         host_bigendian );

/* Used by the optimiser to specialise calls to helpers. */
extern IRExpr *guest_mips32_spechelper(HChar * function_name, IRExpr ** args,
                                       IRStmt ** precedingStmts,
                                       Int n_precedingStmts);

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern Bool guest_mips32_state_requires_precise_mem_exns(Int, Int);

extern VexGuestLayout mips32Guest_layout;

/*---------------------------------------------------------*/
/*--- mips guest helpers                                 ---*/
/*---------------------------------------------------------*/

extern UInt mips32_dirtyhelper_mfc0(UInt rd, UInt sel);

extern void mips32_dirtyhelper_sync(UInt sync);

/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* Defines conditions which we can ask for (MIPS MIPS 2e page A3-6) */

typedef enum {
   MIPSCondEQ = 0,      /* equal                         : Z=1 */
   MIPSCondNE = 1,      /* not equal                     : Z=0 */

   MIPSCondHS = 2,      /* >=u (higher or same)          : C=1 */
   MIPSCondLO = 3,      /* <u  (lower)                   : C=0 */

   MIPSCondMI = 4,      /* minus (negative)              : N=1 */
   MIPSCondPL = 5,      /* plus (zero or +ve)            : N=0 */

   MIPSCondVS = 6,      /* overflow                      : V=1 */
   MIPSCondVC = 7,      /* no overflow                   : V=0 */

   MIPSCondHI = 8,      /* >u   (higher)                 : C=1 && Z=0 */
   MIPSCondLS = 9,      /* <=u  (lower or same)          : C=0 || Z=1 */

   MIPSCondGE = 10,  /* >=s (signed greater or equal) : N=V */
   MIPSCondLT = 11,  /* <s  (signed less than)        : N!=V */

   MIPSCondGT = 12,  /* >s  (signed greater)          : Z=0 && N=V */
   MIPSCondLE = 13,  /* <=s (signed less or equal)    : Z=1 || N!=V */

   MIPSCondAL = 14,  /* always (unconditional)        : 1 */
   MIPSCondNV = 15      /* never (unconditional):        : 0 */
       /* NB: MIPS have deprecated the use of the NV condition code.
          You are now supposed to use MOV R0,R0 as a noop rather than
          MOVNV R0,R0 as was previously recommended.  Future processors
          may have the NV condition code reused to do other things.  */
} MIPSCondcode;

#endif            /* __VEX_GUEST_MIPS_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                   guest_mips_defs.h ---*/
/*---------------------------------------------------------------*/
