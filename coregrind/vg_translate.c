
/*--------------------------------------------------------------------*/
/*--- The JITter proper: register allocation & code improvement    ---*/
/*---                                               vg_translate.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org

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

#include "vg_include.h"

/*------------------------------------------------------------*/
/*--- Renamings of frequently-used global functions.       ---*/
/*------------------------------------------------------------*/

#define dis       VG_(print_codegen)


/*------------------------------------------------------------*/
/*--- Basics                                               ---*/
/*------------------------------------------------------------*/

UCodeBlock* VG_(alloc_UCodeBlock) ( void )
{
   UCodeBlock* cb = VG_(arena_malloc)(VG_AR_CORE, sizeof(UCodeBlock));
   cb->used = cb->size = cb->nextTemp = 0;
   cb->instrs = NULL;
   return cb;
}


void VG_(free_UCodeBlock) ( UCodeBlock* cb )
{
   if (cb->instrs) VG_(arena_free)(VG_AR_CORE, cb->instrs);
   VG_(arena_free)(VG_AR_CORE, cb);
}


/* Ensure there's enough space in a block to add one uinstr. */
static __inline__
void ensureUInstr ( UCodeBlock* cb )
{
   if (cb->used == cb->size) {
      if (cb->instrs == NULL) {
         vg_assert(cb->size == 0);
         vg_assert(cb->used == 0);
         cb->size = 8;
         cb->instrs = VG_(arena_malloc)(VG_AR_CORE, 8 * sizeof(UInstr));
      } else {
         Int i;
         UInstr* instrs2 = VG_(arena_malloc)(VG_AR_CORE, 
                                       2 * sizeof(UInstr) * cb->size);
         for (i = 0; i < cb->used; i++)
            instrs2[i] = cb->instrs[i];
         cb->size *= 2;
         VG_(arena_free)(VG_AR_CORE, cb->instrs);
         cb->instrs = instrs2;
      }
   }

   vg_assert(cb->used < cb->size);
}


__inline__ 
void VG_(new_NOP) ( UInstr* u )
{
   u->val1 = u->val2 = u->val3 = 0;
   u->tag1 = u->tag2 = u->tag3 = NoValue;
   u->flags_r = u->flags_w = FlagsEmpty;
   u->jmpkind = JmpBoring;
   u->signed_widen = u->has_ret_val = False;
   u->regs_live_after = ALL_RREGS_LIVE;
   u->lit32    = 0;
   u->opcode   = NOP;
   u->size     = 0;
   u->cond     = 0;
   u->extra4b  = 0;
   u->argc = u->regparms_n = 0;
}


/* Add an instruction to a ucode block, and return the index of the
   instruction. */
__inline__
void VG_(new_UInstr3) ( UCodeBlock* cb, Opcode opcode, Int sz,
                       Tag tag1, UInt val1,
                       Tag tag2, UInt val2,
                       Tag tag3, UInt val3 )
{
   UInstr* ui;
   ensureUInstr(cb);
   ui = & cb->instrs[cb->used];
   cb->used++;
   VG_(new_NOP)(ui);
   ui->val1   = val1;
   ui->val2   = val2;
   ui->val3   = val3;
   ui->opcode = opcode;
   ui->tag1   = tag1;
   ui->tag2   = tag2;
   ui->tag3   = tag3;
   ui->size   = sz;
   if (tag1 == TempReg) vg_assert(val1 != INVALID_TEMPREG);
   if (tag2 == TempReg) vg_assert(val2 != INVALID_TEMPREG);
   if (tag3 == TempReg) vg_assert(val3 != INVALID_TEMPREG);
}


__inline__
void VG_(new_UInstr2) ( UCodeBlock* cb, Opcode opcode, Int sz,
                       Tag tag1, UInt val1,
                       Tag tag2, UInt val2 )
{
   UInstr* ui;
   ensureUInstr(cb);
   ui = & cb->instrs[cb->used];
   cb->used++;
   VG_(new_NOP)(ui);
   ui->val1   = val1;
   ui->val2   = val2;
   ui->opcode = opcode;
   ui->tag1   = tag1;
   ui->tag2   = tag2;
   ui->size   = sz;
   if (tag1 == TempReg) vg_assert(val1 != INVALID_TEMPREG);
   if (tag2 == TempReg) vg_assert(val2 != INVALID_TEMPREG);
}


__inline__
void VG_(new_UInstr1) ( UCodeBlock* cb, Opcode opcode, Int sz,
                       Tag tag1, UInt val1 )
{
   UInstr* ui;
   ensureUInstr(cb);
   ui = & cb->instrs[cb->used];
   cb->used++;
   VG_(new_NOP)(ui);
   ui->val1   = val1;
   ui->opcode = opcode;
   ui->tag1   = tag1;
   ui->size   = sz;
   if (tag1 == TempReg) vg_assert(val1 != INVALID_TEMPREG);
}


__inline__
void VG_(new_UInstr0) ( UCodeBlock* cb, Opcode opcode, Int sz )
{
   UInstr* ui;
   ensureUInstr(cb);
   ui = & cb->instrs[cb->used];
   cb->used++;
   VG_(new_NOP)(ui);
   ui->opcode = opcode;
   ui->size   = sz;
}

/* Copy an instruction into the given codeblock. */
__inline__ 
void VG_(copy_UInstr) ( UCodeBlock* cb, UInstr* instr )
{
   ensureUInstr(cb);
   cb->instrs[cb->used] = *instr;
   cb->used++;
}

/* Copy auxiliary info from one uinstr to another. */
static __inline__ 
void copyAuxInfoFromTo ( UInstr* src, UInstr* dst )
{
   dst->cond            = src->cond;
   dst->extra4b         = src->extra4b;
   dst->signed_widen    = src->signed_widen;
   dst->jmpkind         = src->jmpkind;
   dst->flags_r         = src->flags_r;
   dst->flags_w         = src->flags_w;
   dst->argc            = src->argc;
   dst->regparms_n      = src->regparms_n;
   dst->has_ret_val     = src->has_ret_val;
   dst->regs_live_after = src->regs_live_after;
}


/* Set the flag R/W sets on a uinstr. */
void VG_(set_flag_RW) ( UInstr* u, FlagSet fr, FlagSet fw )
{
   /* VG_(pp_UInstr)(-1,u); */
   vg_assert(fr == (fr & FlagsALL));
   vg_assert(fw == (fw & FlagsALL));
   u->flags_r = fr;
   u->flags_w = fw;
}


/* Set the lit32 field of the most recent uinsn. */
void VG_(set_lit_field) ( UCodeBlock* cb, UInt lit32 )
{
   LAST_UINSTR(cb).lit32 = lit32;
}


/* Set the C call info fields of the most recent uinsn. */
void  VG_(set_ccall_fields) ( UCodeBlock* cb, Addr fn, UChar argc, UChar
                              regparms_n, Bool has_ret_val )
{
   vg_assert(argc       <  4);
   vg_assert(regparms_n <= argc);
   LAST_UINSTR(cb).lit32       = fn;
   LAST_UINSTR(cb).argc        = argc;
   LAST_UINSTR(cb).regparms_n  = regparms_n;
   LAST_UINSTR(cb).has_ret_val = has_ret_val;
}

Bool VG_(any_flag_use) ( UInstr* u )
{
   return (u->flags_r != FlagsEmpty 
           || u->flags_w != FlagsEmpty);
}

#if 1
#  define BEST_ALLOC_ORDER
#endif

/* Convert a rank in the range 0 .. VG_MAX_REALREGS-1 into an Intel
   register number.  This effectively defines the order in which real
   registers are allocated.  %ebp is excluded since it is permanently
   reserved for pointing at VG_(baseBlock).

   Important!  This function must correspond with the value of
   VG_MAX_REALREGS (actually, VG_MAX_REALREGS can be reduced without
   a problem, except the generated code will obviously be worse).
*/
__inline__ 
Int VG_(rank_to_realreg) ( Int rank )
{
   switch (rank) {
#     ifdef BEST_ALLOC_ORDER
      /* Probably the best allocation ordering. */
      case 0: return R_EAX;
      case 1: return R_EBX;
      case 2: return R_ECX;
      case 3: return R_EDX;
      case 4: return R_ESI;
      case 5: return R_EDI;
#     else
      /* Contrary; probably the worst.  Helpful for debugging, tho. */
      case 5: return R_EAX;
      case 4: return R_EBX;
      case 3: return R_ECX;
      case 2: return R_EDX;
      case 1: return R_ESI;
      case 0: return R_EDI;
#     endif
      default: VG_(core_panic)("VG_(rank_to_realreg)");
   }
}

/* Convert an Intel register number into a rank in the range 0 ..
   VG_MAX_REALREGS-1.  See related comments for rank_to_realreg()
   above.  */
__inline__
Int VG_(realreg_to_rank) ( Int realReg )
{
   switch (realReg) {
#     ifdef BEST_ALLOC_ORDER
      case R_EAX: return 0;
      case R_EBX: return 1;
      case R_ECX: return 2;
      case R_EDX: return 3;
      case R_ESI: return 4;
      case R_EDI: return 5;
#     else
      case R_EAX: return 5;
      case R_EBX: return 4;
      case R_ECX: return 3;
      case R_EDX: return 2;
      case R_ESI: return 1;
      case R_EDI: return 0;
#     endif
      default: VG_(core_panic)("VG_(realreg_to_rank)");
   }
}


/*------------------------------------------------------------*/
/*--- Sanity checking uinstrs.                             ---*/
/*------------------------------------------------------------*/

/* This seems as good a place as any to record some important stuff
   about ucode semantics.

   * TempRegs are 32 bits wide.  LOADs of 8/16 bit values into a
     TempReg are defined to zero-extend the loaded value to 32 bits.
     This is needed to make the translation of movzbl et al work
     properly.

   * Similarly, GETs of a 8/16 bit ArchRegs are zero-extended.

   * Arithmetic on TempRegs is at the specified size.  For example,
     SUBW t1, t2 has to result in a real 16 bit x86 subtraction 
     being emitted -- not a 32 bit one.

   * On some insns we allow the cc bit to be set.  If so, the
     intention is that the simulated machine's %eflags register
     is copied into that of the real machine before the insn,
     and copied back again afterwards.  This means that the 
     code generated for that insn must be very careful only to
     update %eflags in the intended way.  This is particularly
     important for the routines referenced by CALL insns.
*/

/* Meaning of operand kinds is as follows:

   ArchReg  is a register of the simulated CPU, stored in memory,
            in vg_m_state.m_eax .. m_edi.  These values are stored
            using the Intel register encoding.

   RealReg  is a register of the real CPU.  There are VG_MAX_REALREGS
            available for allocation.  As with ArchRegs, these values
            are stored using the Intel register encoding.

   TempReg  is a temporary register used to express the results of
            disassembly.  There is an unlimited supply of them -- 
            register allocation and spilling eventually assigns them 
            to RealRegs.

   SpillNo  is a spill slot number.  The number of required spill
            slots is VG_MAX_PSEUDOS, in general.  Only allowed 
            as the ArchReg operand of GET and PUT.

   Lit16    is a signed 16-bit literal value.

   Literal  is a 32-bit literal value.  Each uinstr can only hold
            one of these.

   The disassembled code is expressed purely in terms of ArchReg,
   TempReg and Literal operands.  Eventually, register allocation
   removes all the TempRegs, giving a result using ArchRegs, RealRegs,
   and Literals.  New x86 code can easily be synthesised from this.
   There are carefully designed restrictions on which insns can have
   which operands, intended to make it possible to generate x86 code
   from the result of register allocation on the ucode efficiently and
   without need of any further RealRegs.

   Restrictions for the individual UInstrs are clear from the checks below.
   Abbreviations: A=ArchReg   S=SpillNo   T=TempReg   L=Literal
                  Ls=Lit16    R=RealReg   N=NoValue
                  As=ArchRegS
 
   Before register allocation, S operands should not appear anywhere.
   After register allocation, all T operands should have been
   converted into Rs, and S operands are allowed in GET and PUT --
   denoting spill saves/restores.  

   Before liveness analysis, save_e[acd]x fields should all be True.
   Afterwards, they may be False.

   The size field should be 0 for insns for which it is meaningless,
   ie those which do not directly move/operate on data.
*/
Bool VG_(saneUInstr) ( Bool beforeRA, Bool beforeLiveness, UInstr* u )
{
#  define LIT0 (u->lit32 == 0)
#  define LIT1 (!(LIT0))
#  define LITm (u->tag1 == Literal ? True : LIT0 )
#  define SZ4  (u->size == 4)
#  define SZ2  (u->size == 2)
#  define SZ1  (u->size == 1)
#  define SZ0  (u->size == 0)
#  define SZ42 (u->size == 4 || u->size == 2)
#  define SZi  (u->size == 4 || u->size == 2 || u->size == 1)
#  define SZf  (  u->size ==  4 || u->size ==  8 || u->size ==   2     \
               || u->size == 10 || u->size == 28 || u->size == 108)
#  define SZ4m ((u->tag1 == TempReg || u->tag1 == RealReg) \
                      ? (u->size == 4) : True)

/* For these ones, two cases:
 *
 * 1. They are transliterations of the corresponding x86 instruction, in
 *    which case they should have its flags (except that redundant write
 *    flags can be annulled by the optimisation pass).
 *
 * 2. They are being used generally for other purposes, eg. helping with a
 *    'rep'-prefixed instruction, in which case should have empty flags .
 */
#  define emptyR (u->flags_r == FlagsEmpty)
#  define emptyW (u->flags_w == FlagsEmpty)
#  define CC0 (emptyR && emptyW)
#  define CCr (u->flags_r == FlagsALL && emptyW)
#  define CCw (emptyR &&  u->flags_w == FlagsALL)
#  define CCa (emptyR && (u->flags_w == FlagsOSZACP  || emptyW))
#  define CCc (emptyR && (u->flags_w == FlagsOC      || emptyW))
#  define CCe (emptyR && (u->flags_w == FlagsOSZAP   || emptyW))
#  define CCb ((u->flags_r==FlagC       || emptyR) && \
               (u->flags_w==FlagsOSZACP || emptyW))
#  define CCd ((u->flags_r==FlagC   || emptyR) && \
               (u->flags_w==FlagsOC || emptyW))
#  define CCf (CC0 || CCr || CCw)
#  define CCg ((u->flags_r==FlagsOSZACP || emptyR) && emptyW)
#  define CCj (u->cond==CondAlways ? CC0 : CCg)

#  define TR1 (beforeRA ? (u->tag1 == TempReg) : (u->tag1 == RealReg))
#  define TR2 (beforeRA ? (u->tag2 == TempReg) : (u->tag2 == RealReg))
#  define TR3 (beforeRA ? (u->tag3 == TempReg) : (u->tag3 == RealReg))
#  define A1  (u->tag1 == ArchReg)
#  define A2  (u->tag2 == ArchReg)
#  define AS1 ((u->tag1 == ArchReg) || ((!beforeRA && (u->tag1 == SpillNo))))
#  define AS2 ((u->tag2 == ArchReg) || ((!beforeRA && (u->tag2 == SpillNo))))
#  define AS3 ((u->tag3 == ArchReg) || ((!beforeRA && (u->tag3 == SpillNo))))
#  define L1  (u->tag1 == Literal && u->val1 == 0)
#  define L2  (u->tag2 == Literal && u->val2 == 0)
#  define Ls1 (u->tag1 == Lit16)
#  define Ls3 (u->tag3 == Lit16)
#  define TRL1 (TR1 || L1)
#  define TRAL1 (TR1 || A1 || L1)
#  define N1  (u->tag1 == NoValue)
#  define N2  (u->tag2 == NoValue)
#  define N3  (u->tag3 == NoValue)
#  define Se1 (u->tag1 == ArchRegS)
#  define Se2 (u->tag2 == ArchRegS)

#  define COND0    (u->cond         == 0)
#  define EXTRA4b0 (u->extra4b      == 0)
#  define SG_WD0   (u->signed_widen == 0)
#  define JMPKIND0 (u->jmpkind      == 0)
#  define CCALL0   (u->argc==0 && u->regparms_n==0 && u->has_ret_val==0 && \
                    ( beforeLiveness                                       \
                    ? u->regs_live_after == ALL_RREGS_LIVE                 \
                    : True ))

#  define XCONDi   (         EXTRA4b0 && SG_WD0 && JMPKIND0 && CCALL0)
#  define Xextra4b (COND0             && SG_WD0 && JMPKIND0 && CCALL0)
#  define XWIDEN   (COND0                       && JMPKIND0 && CCALL0)
#  define XJMP     (                     SG_WD0             && CCALL0)
#  define XCCALL   (COND0 && EXTRA4b0 && SG_WD0 && JMPKIND0          )
#  define XOTHER   (COND0 && EXTRA4b0 && SG_WD0 && JMPKIND0 && CCALL0)

   /* 0 or 1 Literal args per UInstr */
   Int n_lits = 0;
   if (u->tag1 == Literal) n_lits++;
   if (u->tag2 == Literal) n_lits++;
   if (u->tag3 == Literal) n_lits++;
   if (n_lits > 1) 
      return False;

   /* Fields not checked: val1, val2, val3 */

   switch (u->opcode) {

   /* Fields checked: lit32   size  flags_r/w tag1   tag2   tag3    (rest) */
   case PUTSEG: return LIT0 && SZ2  && CC0 &&  TR1 && Se2 &&  N3 && XOTHER;
   case GETSEG: return LIT0 && SZ2  && CC0 &&  Se1 && TR2 &&  N3 && XOTHER;
   case USESEG: return LIT0 && SZ0  && CC0 &&  TR1 && TR2 &&  N3 && XOTHER;
   case NOP:    return LIT0 && SZ0  && CC0 &&   N1 &&  N2 &&  N3 && XOTHER;
   case GETF:   return LIT0 && SZ42 && CCr &&  TR1 &&  N2 &&  N3 && XOTHER;
   case PUTF:   return LIT0 && SZ42 && CCw &&  TR1 &&  N2 &&  N3 && XOTHER;
   case GET:    return LIT0 && SZi  && CC0 &&  AS1 && TR2 &&  N3 && XOTHER;
   case PUT:    return LIT0 && SZi  && CC0 &&  TR1 && AS2 &&  N3 && XOTHER;
   case LOAD: 
   case STORE:  return LIT0 && SZi  && CC0 &&  TR1 && TR2 &&  N3 && XOTHER;
   case MOV:    return LITm && SZ4m && CC0 && TRL1 && TR2 &&  N3 && XOTHER;
   case CMOV:   return LIT0 && SZ4  && CCg &&  TR1 && TR2 &&  N3 && XCONDi;
   case WIDEN:  return LIT0 && SZi  && CC0 &&  TR1 &&  N2 &&  N3 && XWIDEN;
   case JMP:    return LITm && SZ0  && CCj && TRL1 &&  N2 &&  N3 && XJMP;
   case CALLM:  return LIT0 && SZ0 /*any*/ &&  Ls1 &&  N2 &&  N3 && XOTHER;
   case CALLM_S: 
   case CALLM_E:return LIT0 && SZ0  && CC0 &&   N1 &&  N2 &&  N3 && XOTHER;
   case PUSH: 
   case POP:    return LIT0 && SZi  && CC0 &&  TR1 &&  N2 &&  N3 && XOTHER;
   case CLEAR:  return LIT0 && SZ0  && CC0 &&  Ls1 &&  N2 &&  N3 && XOTHER;
   case AND:
   case OR:     return LIT0 && SZi  && CCa &&  TR1 && TR2 &&  N3 && XOTHER;
   case ADD:
   case XOR:
   case SUB:    return LITm && SZi  && CCa &&TRAL1 && TR2 &&  N3 && XOTHER;
   case SBB:
   case ADC:    return LITm && SZi  && CCb &&TRAL1 && TR2 &&  N3 && XOTHER;
   case SHL:
   case SHR:
   case SAR:    return LITm && SZi  && CCa && TRL1 && TR2 &&  N3 && XOTHER;
   case ROL:
   case ROR:    return LITm && SZi  && CCc && TRL1 && TR2 &&  N3 && XOTHER;
   case RCL:
   case RCR:    return LITm && SZi  && CCd && TRL1 && TR2 &&  N3 && XOTHER;
   case NOT:    return LIT0 && SZi  && CC0 &&  TR1 &&  N2 &&  N3 && XOTHER;
   case NEG:    return LIT0 && SZi  && CCa &&  TR1 &&  N2 &&  N3 && XOTHER;
   case INC:
   case DEC:    return LIT0 && SZi  && CCe &&  TR1 &&  N2 &&  N3 && XOTHER;
   case CC2VAL: return LIT0 && SZ1  && CCg &&  TR1 &&  N2 &&  N3 && XCONDi;
   case BSWAP:  return LIT0 && SZ4  && CC0 &&  TR1 &&  N2 &&  N3 && XOTHER;
   case JIFZ:   return LIT1 && SZ4  && CC0 &&  TR1 &&  L2 &&  N3 && XOTHER;
   case FPU_R:  
   case FPU_W:  return LIT0 && SZf  && CC0 &&  Ls1 && TR2 &&  N3 && XOTHER;
   case FPU:    return LIT0 && SZ0  && CCf &&  Ls1 &&  N2 &&  N3 && XOTHER;
   case LEA1:   return /*any*/ SZ4  && CC0 &&  TR1 && TR2 &&  N3 && XOTHER;
   case LEA2:   return /*any*/ SZ4  && CC0 &&  TR1 && TR2 && TR3 && Xextra4b;
   case INCEIP: return LIT0 && SZ0  && CC0 &&  Ls1 &&  N2 &&  N3 && XOTHER;
   case CCALL:  return LIT1 && SZ0  && CC0 && 
                       (u->argc > 0                   ? TR1 : N1) && 
                       (u->argc > 1                   ? TR2 : N2) && 
                       (u->argc > 2 || u->has_ret_val ? TR3 : N3) &&
                       u->regparms_n <= u->argc && XCCALL;
   default: 
      if (VG_(needs).extended_UCode)
         return SK_(sane_XUInstr)(beforeRA, beforeLiveness, u);
      else {
         VG_(printf)("unhandled opcode: %u.  Perhaps " 
                     "VG_(needs).extended_UCode should be set?",
                     u->opcode);
         VG_(core_panic)("VG_(saneUInstr): unhandled opcode");
      }
   }
#  undef LIT0
#  undef LIT1
#  undef LITm
#  undef SZ4
#  undef SZ2
#  undef SZ1
#  undef SZ0
#  undef SZ42
#  undef SZi
#  undef SZf
#  undef SZ4m
#  undef emptyR
#  undef emptyW
#  undef CC0
#  undef CCr
#  undef CCw
#  undef CCa
#  undef CCb
#  undef CCc
#  undef CCd
#  undef CCe
#  undef CCf
#  undef CCg
#  undef CCj
#  undef TR1
#  undef TR2
#  undef TR3
#  undef A1
#  undef A2
#  undef AS1
#  undef AS2
#  undef AS3
#  undef L1
#  undef L2
#  undef Ls1
#  undef Ls3
#  undef TRL1
#  undef TRAL1
#  undef N1
#  undef N2
#  undef N3
#  undef Se2
#  undef Se1
#  undef COND0
#  undef EXTRA4b0
#  undef SG_WD0
#  undef JMPKIND0
#  undef CCALL0
#  undef Xextra4b
#  undef XWIDEN
#  undef XJMP
#  undef XCCALL
#  undef XOTHER
}

void VG_(saneUCodeBlock) ( UCodeBlock* cb )
{
   Int i;
        
   for (i = 0; i < cb->used; i++) {
      Bool sane = VG_(saneUInstr)(True, True, &cb->instrs[i]);
      if (!sane) {
         VG_(printf)("Instruction failed sanity check:\n");
         VG_(up_UInstr)(i, &cb->instrs[i]);
      }
      vg_assert(sane);
   }
}

/* Sanity checks to do with CALLMs in UCodeBlocks. */
Bool VG_(saneUCodeBlockCalls) ( UCodeBlock* cb )
{
   Int  callm = 0;
   Int  callm_s = 0;
   Int  callm_e = 0;
   Int  callm_ptr, calls_ptr;
   Int  i, j, t;
   Bool incall = False;

   /* Ensure the number of CALLM, CALLM_S and CALLM_E are the same. */

   for (i = 0; i < cb->used; i++) {
      switch (cb->instrs[i].opcode) {
         case CALLM:
            if (!incall) return False;
            callm++; 
            break;
         case CALLM_S: 
            if (incall) return False;
            incall = True;
            callm_s++; 
            break;
         case CALLM_E: 
            if (!incall) return False;
            incall = False;
            callm_e++; 
            break;
         case PUSH: case POP: case CLEAR:
            if (!incall) return False;
            break;
         default:
            break;
      }
   }
   if (incall) return False;
   if (callm != callm_s || callm != callm_e) return False;

   /* Check the sections between CALLM_S and CALLM's.  Ensure that no
      PUSH uinsn pushes any TempReg that any other PUSH in the same
      section pushes.  Ie, check that the TempReg args to PUSHes in
      the section are unique.  If not, the instrumenter generates
      incorrect code for CALLM insns. */

   callm_ptr = 0;

 find_next_CALLM:
   /* Search for the next interval, making calls_ptr .. callm_ptr
      bracket it. */
   while (callm_ptr < cb->used 
          && cb->instrs[callm_ptr].opcode != CALLM)
      callm_ptr++;
   if (callm_ptr == cb->used)
      return True;
   vg_assert(cb->instrs[callm_ptr].opcode == CALLM);

   calls_ptr = callm_ptr - 1;
   while (cb->instrs[calls_ptr].opcode != CALLM_S)
      calls_ptr--;
   vg_assert(cb->instrs[calls_ptr].opcode == CALLM_S);
   vg_assert(calls_ptr >= 0);

   /* VG_(printf)("interval from %d to %d\n", calls_ptr, callm_ptr ); */

   /* For each PUSH insn in the interval ... */
   for (i = calls_ptr + 1; i < callm_ptr; i++) {
      if (cb->instrs[i].opcode != PUSH) continue;
      t = cb->instrs[i].val1;
      /* Ensure no later PUSH insns up to callm_ptr push the same
         TempReg.  Return False if any such are found. */
      for (j = i+1; j < callm_ptr; j++) {
         if (cb->instrs[j].opcode == PUSH &&
             cb->instrs[j].val1 == t)
            return False;
      }
   }

   /* This interval is clean.  Keep going ... */
   callm_ptr++;
   goto find_next_CALLM;
}


/*------------------------------------------------------------*/
/*--- Printing uinstrs.                                    ---*/
/*------------------------------------------------------------*/

/* Global that dictates whether to print generated code at all stages */
Bool VG_(print_codegen);

Char* VG_(nameCondcode) ( Condcode cond )
{
   switch (cond) {
      case CondO:      return "o";
      case CondNO:     return "no";
      case CondB:      return "b";
      case CondNB:     return "nb";
      case CondZ:      return "z";
      case CondNZ:     return "nz";
      case CondBE:     return "be";
      case CondNBE:    return "nbe";
      case CondS:      return "s";
      case CondNS:     return "ns";
      case CondP:      return "p";
      case CondNP:     return "np";
      case CondL:      return "l";
      case CondNL:     return "nl";
      case CondLE:     return "le";
      case CondNLE:    return "nle";
      case CondAlways: return "MP"; /* hack! */
      default: VG_(core_panic)("nameCondcode");
   }
}


static void vg_ppFlagSet ( Char* prefix, FlagSet set )
{
   VG_(printf)("%s", prefix);
   if (set & FlagD) VG_(printf)("D");
   if (set & FlagO) VG_(printf)("O");
   if (set & FlagS) VG_(printf)("S");
   if (set & FlagZ) VG_(printf)("Z");
   if (set & FlagA) VG_(printf)("A");
   if (set & FlagC) VG_(printf)("C");
   if (set & FlagP) VG_(printf)("P");
}


static void ppTempReg ( Int tt )
{
   if ((tt & 1) == 0)
      VG_(printf)("t%d", tt);
   else
      VG_(printf)("q%d", tt-1);
}


void VG_(pp_UOperand) ( UInstr* u, Int operandNo, Int sz, Bool parens )
{
   UInt tag, val;
   switch (operandNo) {
      case 1: tag = u->tag1; val = u->val1; break;
      case 2: tag = u->tag2; val = u->val2; break;
      case 3: tag = u->tag3; val = u->val3; break;
      default: VG_(core_panic)("VG_(pp_UOperand)(1)");
   }
   if (tag == Literal) val = u->lit32;

   if (parens) VG_(printf)("(");
   switch (tag) {
      case TempReg:  ppTempReg(val); break;
      case RealReg:  VG_(printf)("%s",nameIReg(sz==0 ? 4 : sz,val)); break;
      case Literal:  VG_(printf)("$0x%x", val); break;
      case Lit16:    VG_(printf)("$0x%x", val); break;
      case NoValue:  VG_(printf)("NoValue"); break;
      case ArchReg:  VG_(printf)("%S",nameIReg(sz,val)); break;
      case ArchRegS: VG_(printf)("%S",nameSReg(val)); break;
      case SpillNo:  VG_(printf)("spill%d", val); break;
      default: VG_(core_panic)("VG_(ppUOperand)(2)");
   }
   if (parens) VG_(printf)(")");
}


Char* VG_(name_UOpcode) ( Bool upper, Opcode opc )
{
   switch (opc) {
      case ADD:   return (upper ? "ADD" : "add");
      case ADC:   return (upper ? "ADC" : "adc");
      case AND:   return (upper ? "AND" : "and");
      case OR:    return (upper ? "OR"  : "or");
      case XOR:   return (upper ? "XOR" : "xor");
      case SUB:   return (upper ? "SUB" : "sub");
      case SBB:   return (upper ? "SBB" : "sbb");
      case SHL:   return (upper ? "SHL" : "shl");
      case SHR:   return (upper ? "SHR" : "shr");
      case SAR:   return (upper ? "SAR" : "sar");
      case ROL:   return (upper ? "ROL" : "rol");
      case ROR:   return (upper ? "ROR" : "ror");
      case RCL:   return (upper ? "RCL" : "rcl");
      case RCR:   return (upper ? "RCR" : "rcr");
      case NOT:   return (upper ? "NOT" : "not");
      case NEG:   return (upper ? "NEG" : "neg");
      case INC:   return (upper ? "INC" : "inc");
      case DEC:   return (upper ? "DEC" : "dec");
      case BSWAP: return (upper ? "BSWAP" : "bswap");
      default:    break;
   }
   if (!upper) VG_(core_panic)("vg_name_UOpcode: invalid !upper");
   switch (opc) {
      case CALLM_S: return "CALLM_S";
      case CALLM_E: return "CALLM_E";
      case INCEIP:  return "INCEIP";
      case LEA1:    return "LEA1";
      case LEA2:    return "LEA2";
      case NOP:     return "NOP";
      case GET:     return "GET";
      case PUT:     return "PUT";
      case GETF:    return "GETF";
      case PUTF:    return "PUTF";
      case GETSEG:  return "GETSEG";
      case PUTSEG:  return "PUTSEG";
      case USESEG:  return "USESEG";
      case LOAD:    return "LD" ;
      case STORE:   return "ST" ;
      case MOV:     return "MOV";
      case CMOV:    return "CMOV";
      case WIDEN:   return "WIDEN";
      case JMP:     return "J"    ;
      case JIFZ:    return "JIFZ" ;
      case CALLM:   return "CALLM";
      case CCALL:   return "CCALL";
      case PUSH:    return "PUSH" ;
      case POP:     return "POP"  ;
      case CLEAR:   return "CLEAR";
      case CC2VAL:  return "CC2VAL";
      case FPU_R:   return "FPU_R";
      case FPU_W:   return "FPU_W";
      case FPU:     return "FPU"  ;
      default:
         if (VG_(needs).extended_UCode)
            return SK_(name_XUOpcode)(opc);
         else {
            VG_(printf)("unhandled opcode: %u.  Perhaps " 
                        "VG_(needs).extended_UCode should be set?",
                        opc);
            VG_(core_panic)("name_UOpcode: unhandled opcode");
         }
   }
}

static
void pp_realregs_liveness ( UInstr* u )
{
#  define PRINT_RREG_LIVENESS(realReg,s) \
     VG_(printf)( IS_RREG_LIVE(VG_(realreg_to_rank)(realReg), \
                               u->regs_live_after)             \
                     ? s : "-");

   VG_(printf)("[");
   PRINT_RREG_LIVENESS(R_EAX, "a");
   PRINT_RREG_LIVENESS(R_EBX, "b");
   PRINT_RREG_LIVENESS(R_ECX, "c");
   PRINT_RREG_LIVENESS(R_EDX, "d");
   PRINT_RREG_LIVENESS(R_ESI, "S");
   PRINT_RREG_LIVENESS(R_EDI, "D");
   VG_(printf)("]");

#  undef PRINT_RREG_LIVENESS
}

/* Ugly-print UInstr :) */
void VG_(up_UInstr) ( Int i, UInstr* u )
{
   VG_(pp_UInstr_regs)(i, u);
   
   VG_(printf)("opcode:          %d\n", u->opcode);
   VG_(printf)("lit32:           %x\n", u->lit32);
   VG_(printf)("size:            %d\n", u->size);
   VG_(printf)("val1,val2,val3:  %d, %d, %d\n", u->val1, u->val2, u->val3);
   VG_(printf)("tag1,tag2,tag3:  %d, %d, %d\n", u->tag1, u->tag2, u->tag3);
   VG_(printf)("flags_r:         %x\n", u->flags_r);
   VG_(printf)("flags_w:         %x\n", u->flags_w);
   VG_(printf)("extra4b:         %x\n", u->extra4b);
   VG_(printf)("cond:            %x\n", u->cond);
   VG_(printf)("signed_widen:    %d\n", u->signed_widen);
   VG_(printf)("jmpkind:         %d\n", u->jmpkind);
   VG_(printf)("argc,regparms_n: %d, %d\n", u->argc, u->regparms_n);
   VG_(printf)("has_ret_val:     %d\n", u->has_ret_val);
   VG_(printf)("regs_live_after: ");
   pp_realregs_liveness(u);
   VG_(printf)("\n");
}

static
void pp_UInstrWorker ( Int instrNo, UInstr* u, Bool ppRegsLiveness )
{
   VG_(printf)("\t%4d: %s", instrNo, 
                            VG_(name_UOpcode)(True, u->opcode));
   if (u->opcode == JMP || u->opcode == CC2VAL)
      VG_(printf)("%s", VG_(nameCondcode(u->cond)));

   switch (u->size) {
      case 0:  VG_(printf)("o"); break;
      case 1:  VG_(printf)("B"); break;
      case 2:  VG_(printf)("W"); break;
      case 4:  VG_(printf)("L"); break;
      case 8:  VG_(printf)("Q"); break;
      default: VG_(printf)("%d", (Int)u->size); break;
   }

   switch (u->opcode) {

      case CALLM_S: case CALLM_E:
         break;

      case INCEIP:
         VG_(printf)("\t$%d", u->val1);
         break;

      case LEA2:
         VG_(printf)("\t%d(" , u->lit32);
         VG_(pp_UOperand)(u, 1, 4, False);
         VG_(printf)(",");
         VG_(pp_UOperand)(u, 2, 4, False);
         VG_(printf)(",%d), ", (Int)u->extra4b);
         VG_(pp_UOperand)(u, 3, 4, False);
         break;

      case LEA1:
         VG_(printf)("\t%d" , u->lit32);
         VG_(pp_UOperand)(u, 1, 4, True);
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, 4, False);
         break;

      case NOP:
         break;

      case FPU_W:
         VG_(printf)("\t0x%x:0x%x, ",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         VG_(pp_UOperand)(u, 2, 4, True);
         break;

      case FPU_R:
         VG_(printf)("\t");
         VG_(pp_UOperand)(u, 2, 4, True);
         VG_(printf)(", 0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         break;

      case FPU:
         VG_(printf)("\t0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         break;

      case GET: case PUT: case MOV: case LOAD: case STORE: case CMOV:
      case GETSEG: case PUTSEG:
         VG_(printf)("\t");
         VG_(pp_UOperand)(u, 1, u->size, u->opcode==LOAD); 
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, u->size, u->opcode==STORE);
         break;

      case JMP:
         switch (u->jmpkind) {
            case JmpCall:      VG_(printf)("-c"); break;
            case JmpRet:       VG_(printf)("-r"); break;
            case JmpSyscall:   VG_(printf)("-sys"); break;
            case JmpClientReq: VG_(printf)("-cli"); break;
            default: break;
         }
         VG_(printf)("\t");
         VG_(pp_UOperand)(u, 1, u->size, False);
         if (CondAlways == u->cond) {
            /* Print x86 instruction size if filled in */
            if (0 != u->extra4b)
               VG_(printf)("  ($%u)", u->extra4b);
         }
         break;

      case GETF: case PUTF:
      case CC2VAL: case PUSH: case POP: case CLEAR: case CALLM:
      case NOT: case NEG: case INC: case DEC: case BSWAP:
         VG_(printf)("\t");
         VG_(pp_UOperand)(u, 1, u->size, False);
         break;

      /* Print a "(s)" after args passed on stack */
      case CCALL:
         VG_(printf)("\t");
         if (u->has_ret_val) {
            VG_(pp_UOperand)(u, 3, 0, False);
            VG_(printf)(" = ");
         }
         VG_(printf)("%p(", u->lit32);
         if (u->argc > 0) {
            VG_(pp_UOperand)(u, 1, 0, False);
            if (u->regparms_n < 1)
               VG_(printf)("(s)");
         }
         if (u->argc > 1) {
            VG_(printf)(", ");
            VG_(pp_UOperand)(u, 2, 0, False);
            if (u->regparms_n < 2)
               VG_(printf)("(s)");
         }
         if (u->argc > 2) {
            VG_(printf)(", ");
            VG_(pp_UOperand)(u, 3, 0, False);
            if (u->regparms_n < 3)
               VG_(printf)("(s)");
         }
         VG_(printf)(") ");
         break;

      case USESEG:
      case JIFZ:
      case ADD: case ADC: case AND: case OR:  
      case XOR: case SUB: case SBB:   
      case SHL: case SHR: case SAR: 
      case ROL: case ROR: case RCL: case RCR:   
         VG_(printf)("\t");
         VG_(pp_UOperand)(u, 1, u->size, False); 
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, u->size, False);
         break;

      case WIDEN:
         VG_(printf)("_%c%c", VG_(toupper)(nameISize(u->extra4b)),
                              u->signed_widen?'s':'z');
         VG_(printf)("\t");
         VG_(pp_UOperand)(u, 1, u->size, False);
         break;

      default: 
         if (VG_(needs).extended_UCode)
            SK_(pp_XUInstr)(u);
         else {
            VG_(printf)("unhandled opcode: %u.  Perhaps " 
                        "VG_(needs).extended_UCode should be set?",
                        u->opcode);
            VG_(core_panic)("pp_UInstr: unhandled opcode");
         }
   }
   if (u->flags_r != FlagsEmpty || u->flags_w != FlagsEmpty) {
      VG_(printf)("  (");
      if (u->flags_r != FlagsEmpty) 
         vg_ppFlagSet("-r", u->flags_r);
      if (u->flags_w != FlagsEmpty) 
         vg_ppFlagSet("-w", u->flags_w);
      VG_(printf)(")");
   }

   if (ppRegsLiveness) {
      VG_(printf)("\t\t");
      pp_realregs_liveness ( u );
   }

   VG_(printf)("\n");
}

void VG_(pp_UInstr) ( Int instrNo, UInstr* u )
{
   pp_UInstrWorker ( instrNo, u, /*ppRegsLiveness*/False );
}

void VG_(pp_UInstr_regs) ( Int instrNo, UInstr* u )
{
   pp_UInstrWorker ( instrNo, u, /*ppRegsLiveness*/True );
}

void VG_(pp_UCodeBlock) ( UCodeBlock* cb, Char* title )
{
   Int i;
   VG_(printf)("%s\n", title);
   for (i = 0; i < cb->used; i++)
      if (cb->instrs[i].opcode != NOP)
         VG_(pp_UInstr) ( i, &cb->instrs[i] );
   VG_(printf)("\n");
}


/*------------------------------------------------------------*/
/*--- uinstr helpers for register allocation               ---*/
/*--- and code improvement.                                ---*/
/*------------------------------------------------------------*/

/* Get the temp/reg use of a uinstr, parking them in an array supplied by
   the caller, which is assumed to be big enough.  Return the number
   of entries.  Insns which read _and_ write a register wind up
   mentioning it twice.  Entries are placed in the array in program
   order, so that if a reg is read-modified-written, it appears first
   as a read and then as a write.  'tag' indicates whether we are looking at
   TempRegs or RealRegs.
*/
__inline__
Int VG_(get_reg_usage) ( UInstr* u, Tag tag, RegUse* arr )
{
#  define RD(ono)    VG_UINSTR_READS_REG(ono)
#  define WR(ono)    VG_UINSTR_WRITES_REG(ono)

   Int n = 0;
   switch (u->opcode) {
      case LEA1: RD(1); WR(2); break;
      case LEA2: RD(1); RD(2); WR(3); break;

      case NOP:   case FPU:   case INCEIP: case CALLM_S: case CALLM_E:
      case CLEAR: case CALLM: break;

      case CCALL:
         if (u->argc > 0)    RD(1); 
         if (u->argc > 1)    RD(2); 
         if (u->argc > 2)    RD(3); 
         if (u->has_ret_val) WR(3);
         break;

      case FPU_R: case FPU_W: RD(2); break;

      case GETSEG: WR(2); break;
      case PUTSEG: RD(1); break;

      case GETF:  WR(1); break;
      case PUTF:  RD(1); break;

      case GET:   WR(2); break;
      case PUT:   RD(1); break;
      case LOAD:  RD(1); WR(2); break;
      case STORE: RD(1); RD(2); break;
      case MOV:   RD(1); WR(2); break;

      case JMP:   RD(1); break;

      case PUSH: RD(1); break;
      case POP:  WR(1); break;

      case USESEG:
      case CMOV:
      case ADD: case ADC: case AND: case OR:  
      case XOR: case SUB: case SBB:   
         RD(1); RD(2); WR(2); break;

      case SHL: case SHR: case SAR: 
      case ROL: case ROR: case RCL: case RCR:
         RD(1); RD(2); WR(2); break;

      case NOT: case NEG: case INC: case DEC: case BSWAP:
         RD(1); WR(1); break;

      case WIDEN: RD(1); WR(1); break;

      case CC2VAL: WR(1); break;
      case JIFZ: RD(1); break;

      default:
         if (VG_(needs).extended_UCode)
            return SK_(get_Xreg_usage)(u, tag, arr);
         else {
            VG_(printf)("unhandled opcode: %u.  Perhaps " 
                        "VG_(needs).extended_UCode should be set?",
                        u->opcode);
            VG_(core_panic)("VG_(get_reg_usage): unhandled opcode");
         }
   }
   return n;

#  undef RD
#  undef WR
}


/* Change temp regs in u into real regs, as directed by the
 * temps[i]-->reals[i] mapping. */
static __inline__
void patchUInstr ( UInstr* u, RegUse temps[], UInt reals[], Int n_tmap )
{
   Int i;
   if (u->tag1 == TempReg) {
      for (i = 0; i < n_tmap; i++)
         if (temps[i].num == u->val1) break;
      if (i == n_tmap) VG_(core_panic)("patchUInstr(1)");
      u->tag1 = RealReg;
      u->val1 = reals[i];
   }
   if (u->tag2 == TempReg) {
      for (i = 0; i < n_tmap; i++)
         if (temps[i].num == u->val2) break;
      if (i == n_tmap) VG_(core_panic)("patchUInstr(2)");
      u->tag2 = RealReg;
      u->val2 = reals[i];
   }
   if (u->tag3 == TempReg) {
      for (i = 0; i < n_tmap; i++)
         if (temps[i].num == u->val3) break;
      if (i == n_tmap) VG_(core_panic)("patchUInstr(3)");
      u->tag3 = RealReg;
      u->val3 = reals[i];
   }
}


/* Tedious x86-specific hack which compensates for the fact that the
   register numbers for %ah .. %dh do not correspond to those for %eax
   .. %edx.  It maps a (reg size, reg no) pair to the number of the
   containing 32-bit reg. */
static __inline__ 
Int containingArchRegOf ( Int sz, Int aregno )
{
   switch (sz) {
      case 4: return aregno;
      case 2: return aregno;
      case 1: return aregno >= 4 ? aregno-4 : aregno;
      default: VG_(core_panic)("containingArchRegOf");
   }
}


/* If u reads an ArchReg, return the number of the containing arch
   reg.  Otherwise return -1.  Used in redundant-PUT elimination.
   Note that this is not required for skins extending UCode because
   this happens before instrumentation. */
static __inline__ 
Int maybe_uinstrReadsArchReg ( UInstr* u )
{
   switch (u->opcode) {
      case GET:
      case ADD: case ADC: case AND: case OR:  
      case XOR: case SUB: case SBB:   
      case SHL: case SHR: case SAR: case ROL: 
      case ROR: case RCL: case RCR:
         if (u->tag1 == ArchReg) 
            return containingArchRegOf ( u->size, u->val1 ); 
         else
            return -1;

      case GETF: case PUTF:
      case CALLM_S: case CALLM_E:
      case INCEIP:
      case LEA1:
      case LEA2:
      case NOP:
      case PUT:
      case LOAD:
      case STORE:
      case MOV:
      case CMOV:
      case JMP:
      case CALLM: case CLEAR: case PUSH: case POP:
      case NOT: case NEG: case INC: case DEC: case BSWAP:
      case CC2VAL:
      case JIFZ:
      case FPU: case FPU_R: case FPU_W:
      case WIDEN:
      /* GETSEG and USESEG are to do with ArchRegS, not ArchReg */
      case GETSEG: case PUTSEG: 
      case USESEG:
         return -1;

      default: 
         VG_(pp_UInstr)(0,u);
         VG_(core_panic)("maybe_uinstrReadsArchReg: unhandled opcode");
   }
}

static __inline__
Bool uInstrMentionsTempReg ( UInstr* u, Int tempreg )
{
   Int i, k;
   RegUse tempUse[3];
   k = VG_(get_reg_usage) ( u, TempReg, &tempUse[0] );
   for (i = 0; i < k; i++)
      if (tempUse[i].num == tempreg)
         return True;
   return False;
}


/*------------------------------------------------------------*/
/*--- ucode improvement.                                   ---*/
/*------------------------------------------------------------*/

/* Improve the code in cb by doing
   -- Redundant ArchReg-fetch elimination
   -- Redundant PUT elimination
   -- Redundant cond-code restore/save elimination
   The overall effect of these is to allow target registers to be
   cached in host registers over multiple target insns.  
*/
static void vg_improve ( UCodeBlock* cb )
{
   Int     i, j, k, m, n, ar, tr, told, actual_areg;
   Int     areg_map[8];
   Bool    annul_put[8];
   RegUse  tempUse[3];
   UInstr* u;
   Bool    wr;
   Int*    last_live_before;
   FlagSet future_dead_flags;

   if (dis) 
      VG_(printf) ("Improvements:\n");

   if (cb->nextTemp > 0)
      last_live_before = VG_(arena_malloc) ( VG_AR_JITTER, 
                                             cb->nextTemp * sizeof(Int) );
   else
      last_live_before = NULL;

   
   /* PASS 1: redundant GET elimination.  (Actually, more general than
      that -- eliminates redundant fetches of ArchRegs). */

   /* Find the live-range-ends for all temporaries.  Duplicates code
      in the register allocator :-( */

   for (i = 0; i < cb->nextTemp; i++) last_live_before[i] = -1;

   for (i = cb->used-1; i >= 0; i--) {
      u = &cb->instrs[i];

      k = VG_(get_reg_usage)(u, TempReg, &tempUse[0]);

      /* For each temp usage ... bwds in program order. */
      for (j = k-1; j >= 0; j--) {
         tr = tempUse[j].num;
         wr = tempUse[j].isWrite;
         if (last_live_before[tr] == -1) {
            vg_assert(tr >= 0 && tr < cb->nextTemp);
            last_live_before[tr] = wr ? (i+1) : i;
         }
      }

   }

#  define BIND_ARCH_TO_TEMP(archreg,tempreg)\
   { Int q;                                           \
     /* Invalidate any old binding(s) to tempreg. */  \
     for (q = 0; q < 8; q++)                          \
        if (areg_map[q] == tempreg) areg_map[q] = -1; \
     /* Add the new binding. */                       \
     areg_map[archreg] = (tempreg);                   \
   }

   /* Set up the A-reg map. */
   for (i = 0; i < 8; i++) areg_map[i] = -1;

   /* Scan insns. */
   for (i = 0; i < cb->used; i++) {
      u = &cb->instrs[i];
      if (u->opcode == GET && u->size == 4) {
         /* GET; see if it can be annulled. */
         vg_assert(u->tag1 == ArchReg);
         vg_assert(u->tag2 == TempReg);
         ar   = u->val1;
         tr   = u->val2;
         told = areg_map[ar];
         if (told != -1 && last_live_before[told] <= i) {
            /* ar already has an old mapping to told, but that runs
               out here.  Annul this GET, rename tr to told for the
               rest of the block, and extend told's live range to that
               of tr.  */
            VG_(new_NOP)(u);
            n = last_live_before[tr] + 1;
            if (n > cb->used) n = cb->used;
            last_live_before[told] = last_live_before[tr];
            last_live_before[tr] = i-1;
            if (dis)
               VG_(printf)(
                  "   at %2d: delete GET, rename t%d to t%d in (%d .. %d)\n", 
                  i, tr, told,i+1, n-1);
            for (m = i+1; m < n; m++) {
               if (cb->instrs[m].tag1 == TempReg 
                   && cb->instrs[m].val1 == tr) 
                 cb->instrs[m].val1 = told;
               if (cb->instrs[m].tag2 == TempReg 
                   && cb->instrs[m].val2 == tr) 
                 cb->instrs[m].val2 = told;
            }
            BIND_ARCH_TO_TEMP(ar,told);
         }
         else
            BIND_ARCH_TO_TEMP(ar,tr);
      }
      else if (u->opcode == GET && u->size != 4) {
         /* Invalidate any mapping for this archreg.  */
         actual_areg = containingArchRegOf ( u->size, u->val1 );
         areg_map[actual_areg] = -1;
      } 
      else if (u->opcode == PUT && u->size == 4) {
         /* PUT; re-establish t -> a binding */
         vg_assert(u->tag1 == TempReg);
         vg_assert(u->tag2 == ArchReg);
         BIND_ARCH_TO_TEMP(u->val2, u->val1);
      }
      else if (u->opcode == PUT && u->size != 4) {
         /* Invalidate any mapping for this archreg. */
         actual_areg = containingArchRegOf ( u->size, u->val2 );
         areg_map[actual_areg] = -1;
      } else {

         /* see if insn has an archreg as a read operand; if so try to
            map it. */
         if (u->tag1 == ArchReg && u->size == 4 
                                && areg_map[u->val1] != -1) {
            switch (u->opcode) {
               case ADD: case SUB: case AND: case OR: case XOR:
               case ADC: case SBB:
               case SHL: case SHR: case SAR: case ROL: case ROR:
               case RCL: case RCR:
                  if (dis) 
                     VG_(printf)(
                        "   at %2d: change ArchReg %S to TempReg t%d\n", 
                        i, nameIReg(4,u->val1), areg_map[u->val1]);
                  u->tag1 = TempReg;
                  u->val1 = areg_map[u->val1];
                  /* Remember to extend the live range of the TempReg,
                     if necessary. */
                  if (last_live_before[u->val1] < i)
                     last_live_before[u->val1] = i;
                  break;
               default: 
                  break;
            }
         }

         /* boring insn; invalidate any mappings to temps it writes */
         k = VG_(get_reg_usage)(u, TempReg, &tempUse[0]);

         for (j = 0; j < k; j++) {
            wr  = tempUse[j].isWrite;
            if (!wr) continue;
            tr = tempUse[j].num;
            for (m = 0; m < 8; m++)
               if (areg_map[m] == tr) areg_map[m] = -1;
         }
      }
         
   }

#  undef BIND_ARCH_TO_TEMP

   /* PASS 2: redundant PUT elimination.  Don't annul (delay) puts of
      %ESP, since the memory check machinery always requires the
      in-memory value of %ESP to be up to date.  Although this isn't
      actually required by other analyses (cache simulation), it's
      simplest to be consistent for all end-uses. */
   for (j = 0; j < 8; j++)
      annul_put[j] = False;

   for (i = cb->used-1; i >= 0; i--) {
      u = &cb->instrs[i];
      if (u->opcode == NOP) continue;

      if (u->opcode == PUT && u->size == 4) {
         vg_assert(u->tag2 == ArchReg);
         actual_areg = containingArchRegOf ( 4, u->val2 );
         if (annul_put[actual_areg]) {
            vg_assert(actual_areg != R_ESP);
            VG_(new_NOP)(u);
            if (dis) 
               VG_(printf)("   at %2d: delete PUT\n", i );
         } else {
            if (actual_areg != R_ESP)
               annul_put[actual_areg] = True;
         }
      } 
      else if (u->opcode == PUT && u->size != 4) { 
         actual_areg = containingArchRegOf ( u->size, u->val2 );
         annul_put[actual_areg] = False;
      } 
      else if (u->opcode == JMP || u->opcode == JIFZ
               || u->opcode == CALLM) {
         for (j = 0; j < 8; j++)
            annul_put[j] = False;
      }
      else {
         /* If an instruction reads an ArchReg, the immediately
            preceding PUT cannot be annulled. */
         actual_areg = maybe_uinstrReadsArchReg ( u );
         if (actual_areg != -1)      
            annul_put[actual_areg] = False;
      }
   }

   /* PASS 2a: redundant-move elimination.  Given MOV t1, t2 and t1 is
      dead after this point, annul the MOV insn and rename t2 to t1.
      Further modifies the last_live_before map. */

#  if 0
   VG_(pp_UCodeBlock)(cb, "Before MOV elimination" );
   for (i = 0; i < cb->nextTemp; i++)
     VG_(printf)("llb[t%d]=%d   ", i, last_live_before[i]);
   VG_(printf)("\n");
#  endif

   for (i = 0; i < cb->used-1; i++) {
      u = &cb->instrs[i];
      if (u->opcode != MOV) continue;
      if (u->tag1 == Literal) continue;
      vg_assert(u->tag1 == TempReg);
      vg_assert(u->tag2 == TempReg);
      if (last_live_before[u->val1] == i) {
         if (dis)
            VG_(printf)(
               "   at %2d: delete MOV, rename t%d to t%d in (%d .. %d)\n",
               i, u->val2, u->val1, i+1, last_live_before[u->val2] );
         for (j = i+1; j <= last_live_before[u->val2]; j++) {
            if (cb->instrs[j].tag1 == TempReg 
                && cb->instrs[j].val1 == u->val2)
               cb->instrs[j].val1 = u->val1;
            if (cb->instrs[j].tag2 == TempReg 
                && cb->instrs[j].val2 == u->val2)
               cb->instrs[j].val2 = u->val1;
         }
         last_live_before[u->val1] = last_live_before[u->val2];
         last_live_before[u->val2] = i-1;
         VG_(new_NOP)(u);
      }
   }

   /* PASS 3: redundant condition-code restore/save elimination.
      Scan backwards from the end.  future_dead_flags records the set
      of flags which are dead at this point, that is, will be written
      before they are next read.  Earlier uinsns which write flags
      already in future_dead_flags can have their writes annulled.  
   */
   future_dead_flags = FlagsEmpty;

   for (i = cb->used-1; i >= 0; i--) {
      u = &cb->instrs[i];

      /* We might never make it to insns beyond this one, so be
         conservative. */
      if (u->opcode == JIFZ || u->opcode == JMP) {
         future_dead_flags = FlagsEmpty;
         continue;
      } 

      /* PUTF modifies the %EFLAGS in essentially unpredictable ways.
         For example people try to mess with bit 21 to see if CPUID
         works.  The setting may or may not actually take hold.  So we
         play safe here. */
      if (u->opcode == PUTF) {
         future_dead_flags = FlagsEmpty;
         continue;
      } 

      /* We can annul the flags written by this insn if it writes a
         subset (or eq) of the set of flags known to be dead after
         this insn.  If not, just record the flags also written by
         this insn.*/
      if (u->flags_w != FlagsEmpty
          && VG_IS_FLAG_SUBSET(u->flags_w, future_dead_flags)) {
         if (dis) {
            VG_(printf)("   at %2d: annul flag write ", i);
            vg_ppFlagSet("", u->flags_w);
            VG_(printf)(" due to later ");
            vg_ppFlagSet("", future_dead_flags);
            VG_(printf)("\n");
         }
         u->flags_w = FlagsEmpty;
      } else {
        future_dead_flags 
           = VG_UNION_FLAG_SETS ( u->flags_w, future_dead_flags );
      }

      /* If this insn also reads flags, empty out future_dead_flags so
         as to force preceding writes not to be annulled. */
      if (u->flags_r != FlagsEmpty)
         future_dead_flags = FlagsEmpty;
   }

   if (last_live_before) 
      VG_(arena_free) ( VG_AR_JITTER, last_live_before );

   if (dis) {
      VG_(printf)("\n");
      VG_(pp_UCodeBlock) ( cb, "Improved UCode:" );
   }
}


/*------------------------------------------------------------*/
/*--- The new register allocator.                          ---*/
/*------------------------------------------------------------*/

typedef
   struct {
      /* Becomes live for the first time after this insn ... */
      Int live_after;
      /* Becomes dead for the last time after this insn ... */
      Int dead_before;
      /* The "home" spill slot, if needed.  Never changes. */
      Int spill_no;
      /* Where is it?  VG_NOVALUE==in a spill slot; else in reg. */
      Int real_no;
   }
   TempInfo;


/* Take a ucode block and allocate its TempRegs to RealRegs, or put
   them in spill locations, and add spill code, if there are not
   enough real regs.  The usual register allocation deal, in short.  

   Important redundancy of representation:

     real_to_temp maps real reg ranks (RRRs) to TempReg nos, or
     to VG_NOVALUE if the real reg has no currently assigned TempReg.

     The .real_no field of a TempInfo gives the current RRR for
     this TempReg, or VG_NOVALUE if the TempReg is currently
     in memory, in which case it is in the SpillNo denoted by
     spillno.

   These pieces of information (a fwds-bwds mapping, really) must 
   be kept consistent!

   This allocator uses the so-called Second Chance Bin Packing
   algorithm, as described in "Quality and Speed in Linear-scan
   Register Allocation" (Traub, Holloway and Smith, ACM PLDI98,
   pp142-151).  It is simple and fast and remarkably good at
   minimising the amount of spill code introduced.
*/

static
UCodeBlock* vg_do_register_allocation ( UCodeBlock* c1 )
{
   TempInfo*    temp_info;
   Int          real_to_temp[VG_MAX_REALREGS];
   Bool         is_spill_cand[VG_MAX_REALREGS];
   Int          ss_busy_until_before[VG_MAX_SPILLSLOTS];
   Int          i, j, k, m, r, tno, max_ss_no;
   Bool         wr, defer, isRead, spill_reqd;
   UInt         realUse[3];
   RegUse       tempUse[3];
   UCodeBlock*  c2;

   /* Used to denote ... well, "no value" in this fn. */
#  define VG_NOTHING (-2)

   /* Initialise the TempReg info.  */
   if (c1->nextTemp > 0)
      temp_info = VG_(arena_malloc)(VG_AR_JITTER,
                                    c1->nextTemp * sizeof(TempInfo) );
   else
      temp_info = NULL;

   for (i = 0; i < c1->nextTemp; i++) {
      temp_info[i].live_after  = VG_NOTHING;
      temp_info[i].dead_before = VG_NOTHING;
      temp_info[i].spill_no    = VG_NOTHING;
      /* temp_info[i].real_no is not yet relevant. */
   }

   spill_reqd = False;

   /* Scan fwds to establish live ranges. */

   for (i = 0; i < c1->used; i++) {
      k = VG_(get_reg_usage)(&c1->instrs[i], TempReg, &tempUse[0]);
      vg_assert(k >= 0 && k <= 3);

      /* For each temp usage ... fwds in program order */
      for (j = 0; j < k; j++) {
         tno = tempUse[j].num;
         wr  = tempUse[j].isWrite;
         if (wr) {
            /* Writes hold a reg live until after this insn. */
            if (temp_info[tno].live_after == VG_NOTHING)
               temp_info[tno].live_after = i;
            if (temp_info[tno].dead_before < i + 1)
               temp_info[tno].dead_before = i + 1;
         } else {
            /* First use of a tmp should be a write. */
            vg_assert(temp_info[tno].live_after != VG_NOTHING);
            /* Reads only hold it live until before this insn. */
            if (temp_info[tno].dead_before < i)
               temp_info[tno].dead_before = i;
         }
      }
   }

#  if 0
   /* Sanity check on live ranges.  Expensive but correct. */
   for (i = 0; i < c1->nextTemp; i++) {
      vg_assert( (temp_info[i].live_after == VG_NOTHING 
                  && temp_info[i].dead_before == VG_NOTHING)
                 || (temp_info[i].live_after != VG_NOTHING 
                     && temp_info[i].dead_before != VG_NOTHING) );
   }
#  endif

   /* Do a rank-based allocation of TempRegs to spill slot numbers.
      We put as few as possible values in spill slots, but
      nevertheless need to have an assignment to them just in case. */

   max_ss_no = -1;

   for (i = 0; i < VG_MAX_SPILLSLOTS; i++)
      ss_busy_until_before[i] = 0;
  
   for (i = 0; i < c1->nextTemp; i++) {

      /* True iff this temp is unused. */
      if (temp_info[i].live_after == VG_NOTHING) 
         continue;

      /* Find the lowest-numbered spill slot which is available at the
         start point of this interval, and assign the interval to
         it. */
      for (j = 0; j < VG_MAX_SPILLSLOTS; j++)
         if (ss_busy_until_before[j] <= temp_info[i].live_after)
            break;
      if (j == VG_MAX_SPILLSLOTS) {
         VG_(printf)("VG_MAX_SPILLSLOTS is too low; increase and recompile.\n");
         VG_(core_panic)("register allocation failed -- out of spill slots");
      }
      ss_busy_until_before[j] = temp_info[i].dead_before;
      temp_info[i].spill_no = j;
      if (j > max_ss_no)
         max_ss_no = j;
   }

   VG_(total_reg_rank) += (max_ss_no+1);

   /* Show live ranges and assigned spill slot nos. */

   if (dis) {
      VG_(printf)("Live range assignments:\n");

      for (i = 0; i < c1->nextTemp; i++) {
         if (temp_info[i].live_after == VG_NOTHING) 
            continue;
         VG_(printf)(
            "   LR %d is  after %d to before %d\tspillno %d\n",
            i,
            temp_info[i].live_after,
            temp_info[i].dead_before,
            temp_info[i].spill_no
         );
      }
      VG_(printf)("\n");
   }

   /* Now that we've established a spill slot number for each used
      temporary, we can go ahead and do the core of the "Second-chance
      binpacking" allocation algorithm. */

   if (dis) VG_(printf)("Register allocated UCode:\n");
      

   /* Resulting code goes here.  We generate it all in a forwards
      pass. */
   c2 = VG_(alloc_UCodeBlock)();

   /* At the start, no TempRegs are assigned to any real register.
      Correspondingly, all temps claim to be currently resident in
      their spill slots, as computed by the previous two passes. */
   for (i = 0; i < VG_MAX_REALREGS; i++)
      real_to_temp[i] = VG_NOTHING;
   for (i = 0; i < c1->nextTemp; i++)
      temp_info[i].real_no = VG_NOTHING;

   /* Process each insn in turn. */
   for (i = 0; i < c1->used; i++) {

      if (c1->instrs[i].opcode == NOP) continue;
      VG_(uinstrs_prealloc)++;

#     if 0
      /* Check map consistency.  Expensive but correct. */
      for (r = 0; r < VG_MAX_REALREGS; r++) {
         if (real_to_temp[r] != VG_NOTHING) {
            tno = real_to_temp[r];
            vg_assert(tno >= 0 && tno < c1->nextTemp);
            vg_assert(temp_info[tno].real_no == r);
         }
      }
      for (tno = 0; tno < c1->nextTemp; tno++) {
         if (temp_info[tno].real_no != VG_NOTHING) {
            r = temp_info[tno].real_no;
            vg_assert(r >= 0 && r < VG_MAX_REALREGS);
            vg_assert(real_to_temp[r] == tno);
         }
      }
#     endif

      if (dis)
         VG_(pp_UInstr)(i, &c1->instrs[i]);

      /* First, free up enough real regs for this insn.  This may
         generate spill stores since we may have to evict some TempRegs
         currently in real regs.  Also generates spill loads. */

      k = VG_(get_reg_usage)(&c1->instrs[i], TempReg, &tempUse[0]);
      vg_assert(k >= 0 && k <= 3);

      /* For each ***different*** temp mentioned in the insn .... */
      for (j = 0; j < k; j++) {

         /* First check if the temp is mentioned again later; if so,
            ignore this mention.  We only want to process each temp
            used by the insn once, even if it is mentioned more than
            once. */
         defer = False;
         tno = tempUse[j].num;
         for (m = j+1; m < k; m++)
            if (tempUse[m].num == tno) 
               defer = True;
         if (defer) 
            continue;

         /* Now we're trying to find a register for tempUse[j].num.
            First of all, if it already has a register assigned, we
            don't need to do anything more. */
         if (temp_info[tno].real_no != VG_NOTHING)
            continue;

         /* No luck.  The next thing to do is see if there is a
            currently unassigned register available.  If so, bag it. */
         for (r = 0; r < VG_MAX_REALREGS; r++) {
            if (real_to_temp[r] == VG_NOTHING)
               break;
         }
         if (r < VG_MAX_REALREGS) {
            real_to_temp[r]        = tno;
            temp_info[tno].real_no = r;
            continue;
         }

         /* Unfortunately, that didn't pan out either.  So we'll have
            to eject some other unfortunate TempReg into a spill slot
            in order to free up a register.  Of course, we need to be
            careful not to eject some other TempReg needed by this
            insn.

            Select r in 0 .. VG_MAX_REALREGS-1 such that
            real_to_temp[r] is not mentioned in 
            tempUse[0 .. k-1].num, since it would be just plain 
            wrong to eject some other TempReg which we need to use in 
            this insn.

            It is here that it is important to make a good choice of
            register to spill.  */

         /* First, mark those regs which are not spill candidates. */
         for (r = 0; r < VG_MAX_REALREGS; r++) {
            is_spill_cand[r] = True;
            for (m = 0; m < k; m++) {
               if (real_to_temp[r] == tempUse[m].num) {
                  is_spill_cand[r] = False;
                  break;
               }
            }
         }

         /* We can choose any r satisfying is_spill_cand[r].  However,
            try to make a good choice.  First, try and find r such
            that the associated TempReg is already dead. */
         for (r = 0; r < VG_MAX_REALREGS; r++) {
            if (is_spill_cand[r] && 
                temp_info[real_to_temp[r]].dead_before <= i)
               goto have_spill_cand;
         }

         /* No spill cand is mapped to a dead TempReg.  Now we really
           _do_ have to generate spill code.  Choose r so that the
           next use of its associated TempReg is as far ahead as
           possible, in the hope that this will minimise the number of
           consequent reloads required.  This is a bit expensive, but
           we don't have to do it very often. */
         {
            Int furthest_r = VG_MAX_REALREGS;
            Int furthest = 0;
            for (r = 0; r < VG_MAX_REALREGS; r++) {
               if (!is_spill_cand[r]) continue;
               for (m = i+1; m < c1->used; m++)
                  if (uInstrMentionsTempReg(&c1->instrs[m], 
                                            real_to_temp[r]))
                     break;
               if (m > furthest) {
                  furthest   = m;
                  furthest_r = r;
               }
            }
            r = furthest_r;
            goto have_spill_cand;
         }

         have_spill_cand:
         if (r == VG_MAX_REALREGS)
            VG_(core_panic)("new reg alloc: out of registers ?!");

         /* Eject r.  Important refinement: don't bother if the
            associated TempReg is now dead. */
         vg_assert(real_to_temp[r] != VG_NOTHING);
         vg_assert(real_to_temp[r] != tno);
         temp_info[real_to_temp[r]].real_no = VG_NOTHING;
         if (temp_info[real_to_temp[r]].dead_before > i) {
            uInstr2(c2, PUT, 4, 
                        RealReg, VG_(rank_to_realreg)(r), 
                        SpillNo, temp_info[real_to_temp[r]].spill_no);
            VG_(uinstrs_spill)++;
            spill_reqd = True;
            if (dis)
               VG_(pp_UInstr)(c2->used-1, &LAST_UINSTR(c2));
         }

         /* Decide if tno is read. */
         isRead = False;
         for (m = 0; m < k; m++)
            if (tempUse[m].num == tno && !tempUse[m].isWrite) 
               isRead = True;

         /* If so, generate a spill load. */
         if (isRead) {
            uInstr2(c2, GET, 4, 
                        SpillNo, temp_info[tno].spill_no, 
                        RealReg, VG_(rank_to_realreg)(r) );
            VG_(uinstrs_spill)++;
            spill_reqd = True;
            if (dis)
               VG_(pp_UInstr)(c2->used-1, &LAST_UINSTR(c2));
         }

         /* Update the forwards and backwards maps. */
         real_to_temp[r]        = tno;
         temp_info[tno].real_no = r;
      }

      /* By this point, all TempRegs mentioned by the insn have been
         bought into real regs.  We now copy the insn to the output
         and use patchUInstr to convert its rTempRegs into
         realregs. */
      for (j = 0; j < k; j++)
         realUse[j] = VG_(rank_to_realreg)(temp_info[tempUse[j].num].real_no);
      VG_(copy_UInstr)(c2, &c1->instrs[i]);
      patchUInstr(&LAST_UINSTR(c2), &tempUse[0], &realUse[0], k);

      if (dis) {
         VG_(pp_UInstr)(c2->used-1, &LAST_UINSTR(c2));
         VG_(printf)("\n");
      }
   }

   if (temp_info != NULL)
      VG_(arena_free)(VG_AR_JITTER, temp_info);

   VG_(free_UCodeBlock)(c1);

   if (spill_reqd) 
      VG_(translations_needing_spill)++;

   return c2;

#  undef VG_NOTHING

}
extern void fooble(int);
/* Analysis records liveness of all general-use RealRegs in the UCode. */
static void vg_realreg_liveness_analysis ( UCodeBlock* cb )
{        
   Int      i, j, k;
   RRegSet  rregs_live;
   RegUse   regUse[3];
   UInstr*  u;

   /* All regs are dead at the end of the block */
   rregs_live = ALL_RREGS_DEAD;
            
   for (i = cb->used-1; i >= 0; i--) {
      u = &cb->instrs[i];

      u->regs_live_after = rregs_live;

      k = VG_(get_reg_usage)(u, RealReg, regUse);

      /* For each reg usage ... bwds in program order.  Variable is live
         before this UInstr if it is read by this UInstr.
         Note that regUse[j].num holds the Intel reg number, so we must
         convert it to our rank number.  */
      for (j = k-1; j >= 0; j--) {
         SET_RREG_LIVENESS ( VG_(realreg_to_rank)(regUse[j].num),
                             rregs_live,
                             !regUse[j].isWrite );
      }
   }
}

/*------------------------------------------------------------*/
/*--- Main entry point for the JITter.                     ---*/
/*------------------------------------------------------------*/

/* Translate the basic block beginning at orig_addr, placing the
   translation in a vg_malloc'd block, the address and size of which
   are returned in trans_addr and trans_size.  Length of the original
   block is also returned in orig_size.  If the latter three are NULL,
   this call is being done for debugging purposes, in which case (a)
   throw away the translation once it is made, and (b) produce a load
   of debugging output. 

   'tst' is the identity of the thread needing this block.
*/
void VG_(translate) ( /*IN*/  ThreadState* tst, 
		      /*IN*/  Addr  orig_addr,  
                      /*OUT*/ UInt* orig_size,
                      /*OUT*/ Addr* trans_addr, 
                      /*OUT*/ UInt* trans_size )
{
   Int         n_disassembled_bytes, final_code_size;
   Bool        debugging_translation;
   UChar*      final_code;
   UCodeBlock* cb;

   VGP_PUSHCC(VgpTranslate);
   debugging_translation
      = orig_size == NULL || trans_addr == NULL || trans_size == NULL;

   if (!debugging_translation)
      VG_TRACK( pre_mem_read, Vg_CoreTranslate, tst, "", orig_addr, 1 );

   cb = VG_(alloc_UCodeBlock)();

   /* If doing any code printing, print a basic block start marker */
   if (VG_(clo_trace_codegen)) {
      Char fnname[64] = "";
      VG_(get_fnname_if_entry)(orig_addr, fnname, 64);
      VG_(printf)(
              "==== BB %d %s(%p) in %dB, out %dB, BBs exec'd %lu ====\n\n",
              VG_(overall_in_count), fnname, orig_addr, 
              VG_(overall_in_osize), VG_(overall_in_tsize),
              VG_(bbs_done));
   }

   /* True if a debug trans., or if bit N set in VG_(clo_trace_codegen). */
#  define DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE(n) \
      ( debugging_translation || (VG_(clo_trace_codegen) & (1 << (n-1))) )

   /* Disassemble this basic block into cb. */
   VG_(print_codegen) = DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE(1);
   VGP_PUSHCC(VgpToUCode);
   n_disassembled_bytes = VG_(disBB) ( cb, orig_addr );
   VGP_POPCC(VgpToUCode);

   /* Try and improve the code a bit. */
   if (VG_(clo_optimise)) {
      VG_(print_codegen) = DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE(2);
      VGP_PUSHCC(VgpImprove);
      vg_improve ( cb );
      VGP_POPCC(VgpImprove);
   }

   /* Skin's instrumentation (Nb: must set VG_(print_codegen) in case
      SK_(instrument) looks at it. */
   VG_(print_codegen) = DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE(3);
   VGP_PUSHCC(VgpInstrument);
   cb = SK_(instrument) ( cb, orig_addr );
   if (VG_(print_codegen))
      VG_(pp_UCodeBlock) ( cb, "Instrumented UCode:" );
   VG_(saneUCodeBlock)( cb );
   VGP_POPCC(VgpInstrument);

   /* Allocate registers. */
   VG_(print_codegen) = DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE(4);
   VGP_PUSHCC(VgpRegAlloc);
   cb = vg_do_register_allocation ( cb );
   VGP_POPCC(VgpRegAlloc);

   /* Do post reg-alloc %e[acd]x liveness analysis (too boring to print
    * anything;  results can be seen when emitting final code). */
   VGP_PUSHCC(VgpLiveness);
   vg_realreg_liveness_analysis ( cb );
   VGP_POPCC(VgpLiveness);

   /* Emit final code */
   VG_(print_codegen) = DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE(5);

   VGP_PUSHCC(VgpFromUcode);
   final_code = VG_(emit_code)(cb, &final_code_size );
   VGP_POPCC(VgpFromUcode);
   VG_(free_UCodeBlock)(cb);

#undef DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE

   if (debugging_translation) {
      /* Only done for debugging -- throw away final result. */
      VG_(arena_free)(VG_AR_JITTER, final_code);
   } else {
      /* Doing it for real -- return values to caller. */
      *orig_size = n_disassembled_bytes;
      *trans_addr = (Addr)final_code;
      *trans_size = final_code_size;
   }
   VGP_POPCC(VgpTranslate);
}

/*--------------------------------------------------------------------*/
/*--- end                                           vg_translate.c ---*/
/*--------------------------------------------------------------------*/
