
/*--------------------------------------------------------------------*/
/*--- The JITter proper: register allocation & code improvement    ---*/
/*---                                               vg_translate.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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

#include "core.h"
#include "../../pub/libvex.h"

/*------------------------------------------------------------*/
/*--- Renamings of frequently-used global functions.       ---*/
/*------------------------------------------------------------*/

#define dis       VG_(print_codegen)

/*------------------------------------------------------------*/
/*--- Reg-alloc stats                                      ---*/
/*------------------------------------------------------------*/

static UInt n_uinstrs_prealloc;           // # uinstrs input to reg-alloc
static UInt n_uinstrs_spill;              // # uinstrs added due to spill code
static UInt n_translations_needing_spill; // # bbs requiring spill code
static UInt n_total_reg_rank; // total of register ranks over all translations

void VG_(print_reg_alloc_stats)(void)
{
   VG_(message)(Vg_DebugMsg, 
                "reg-alloc: %d t-req-spill, %d+%d orig+spill uis,",
                n_translations_needing_spill, 
                n_uinstrs_prealloc, n_uinstrs_spill );
   VG_(message)(Vg_DebugMsg, 
                "           %d total-reg-rank",
                n_total_reg_rank );
}

/*------------------------------------------------------------*/
/*--- Basics                                               ---*/
/*------------------------------------------------------------*/

#define VG_IS_FLAG_SUBSET(set1,set2) \
   (( ((FlagSet)set1) & ((FlagSet)set2) ) == ((FlagSet)set1) )

#define VG_UNION_FLAG_SETS(set1,set2) \
   ( ((FlagSet)set1) | ((FlagSet)set2) )

// This one is local.
static UCodeBlock* alloc_UCodeBlock ( Addr orig_eip )
{
   UCodeBlock* cb = VG_(arena_malloc)(VG_AR_CORE, sizeof(UCodeBlock));
   cb->orig_eip = orig_eip;
   cb->used     = 0;
   cb->size     = 0;
   cb->instrs   = NULL;
   cb->nextTemp = 0;
   return cb;
}

// This one is called by tools.
UCodeBlock* VG_(setup_UCodeBlock) ( UCodeBlock* cb_in )
{
   UCodeBlock* cb = alloc_UCodeBlock( cb_in->orig_eip );
   cb->nextTemp = cb_in->nextTemp;
   return cb;
}

void VG_(free_UCodeBlock) ( UCodeBlock* cb )
{
   if (cb->instrs) VG_(arena_free)(VG_AR_CORE, cb->instrs);
   VG_(arena_free)(VG_AR_CORE, cb);
}


/* Ensure there's enough space in a block to add one uinstr. */
static
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

/* For the last uinsn inserted into cb, set the read, written and
   undefined flags.  Undefined flags are counted as written, but it
   seems worthwhile to distinguish them. 
*/
__inline__
void VG_(set_flag_fields) ( UCodeBlock* cb,
                             FlagSet rr, FlagSet ww, FlagSet uu )
{
   FlagSet uw = VG_UNION_FLAG_SETS(ww,uu);
   
   vg_assert(rr == (rr & FlagsALL));
   vg_assert(uw == (uw & FlagsALL));
   LAST_UINSTR(cb).flags_r = rr;
   LAST_UINSTR(cb).flags_w = uw;
}

void VG_(set_cond_field) ( UCodeBlock* cb, Condcode cond )
{
   LAST_UINSTR(cb).cond = cond;
}

void VG_(set_widen_fields) ( UCodeBlock* cb, UInt szs, Bool is_signed )
{
   LAST_UINSTR(cb).extra4b = szs;
   LAST_UINSTR(cb).signed_widen = is_signed;
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

Int VG_(get_num_instrs) ( UCodeBlock* cb )
{
   return cb->used;
}

Int VG_(get_num_temps) ( UCodeBlock* cb )
{
   return cb->nextTemp;
}

UInstr* VG_(get_instr) ( UCodeBlock* cb, Int i )
{
   return & cb->instrs[i];
}

UInstr* VG_(get_last_instr) ( UCodeBlock* cb )
{
   return & cb->instrs[cb->used-1];
}
   

/*------------------------------------------------------------*/
/*--- Sanity checking uinstrs.                             ---*/
/*------------------------------------------------------------*/

// Global variables that indicate where we are in the translation of a basic
// block, and affect exactly how UInstrs are sanity-checked.
static Bool beforeRA = True;
static Bool beforeLiveness  = True;

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
static Bool is_sane_UInstr ( UInstr* u )
{
#  define LIT0 (u->lit32 == 0)
#  define LIT8 (((u->lit32) & 0xFFFFFF00) == 0)
#  define LIT1 (!(LIT0))
#  define LITm (u->tag1 == Literal ? True : LIT0 )
#  define SZ16 (u->size == 16)
#  define SZ8  (u->size == 8)
#  define SZ4  (u->size == 4)
#  define SZ2  (u->size == 2)
#  define SZ1  (u->size == 1)
#  define SZ0  (u->size == 0)
#  define SZ42 (u->size == 4 || u->size == 2)
#  define SZ48 (u->size == 4 || u->size == 8)
#  define SZ416 (u->size == 4 || u->size == 16)
#  define SZ816 (u->size == 8 || u->size == 16)
#  define SZsse2 (u->size == 4 || u->size == 8 || u->size == 16 || u->size == 512)
#  define SZsse3 (u->size == 4 || u->size == 8 || u->size == 16)
#  define SZi  (u->size == 4 || u->size == 2 || u->size == 1)
#  define SZf  (  u->size ==  4 || u->size ==  8 || u->size ==   2     \
               || u->size == 10 || u->size == 28 || u->size == 108)
#  define SZ4m ((u->tag1 == TempReg || u->tag1 == RealReg) \
                      ? (u->size == 4) : SZi)

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
#  define CCf (CC0 || (emptyR && u->flags_w==FlagsZCP) \
                   || (u->flags_r==FlagsZCP && emptyW))
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
#  define Ls2 (u->tag2 == Lit16)
#  define Ls3 (u->tag3 == Lit16)
#  define TRL1 (TR1 || L1)
#  define TRAL1 (TR1 || A1 || L1)
#  define TRA1 (TR1 || A1)
#  define TRA2 (TR2 || A2)
#  define N1  (u->tag1 == NoValue)
#  define N2  (u->tag2 == NoValue)
#  define N3  (u->tag3 == NoValue)
#  define Se1 (u->tag1 == ArchRegS)
#  define Se2 (u->tag2 == ArchRegS)

#  define COND0    (u->cond         == 0)
#  define EXTRA4b0 (u->extra4b      == 0)
#  define EXTRA4b12 (u->extra4b     == 1 || u->extra4b == 2)
#  define SG_WD0   (u->signed_widen == 0)
#  define JMPKIND0 (u->jmpkind      == 0)
#  define CCALL0   (u->argc==0 && u->regparms_n==0 && u->has_ret_val==0 && \
                    ( beforeLiveness                                       \
                    ? u->regs_live_after == ALL_RREGS_LIVE                 \
                    : True ))

#  define XCONDi   (         EXTRA4b0 && SG_WD0 && JMPKIND0 && CCALL0)
#  define XLEA2    (COND0             && SG_WD0 && JMPKIND0 && CCALL0)
#  define XWIDEN   (COND0 && EXTRA4b12          && JMPKIND0 && CCALL0)
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
   case LOCK:   return LIT0 && SZ0  && CC0 &&   N1 &&  N2 &&  N3 && XOTHER;
   case GETF:   return LIT0 && SZ42 && CCr &&  TR1 &&  N2 &&  N3 && XOTHER;
   case PUTF:   return LIT0 && SZ42 && CCw &&  TR1 &&  N2 &&  N3 && XOTHER;
   case GET:    return LIT0 && SZi  && CC0 &&  AS1 && TR2 &&  N3 && XOTHER;
   case PUT:    return LIT0 && SZi  && CC0 &&  TR1 && AS2 &&  N3 && XOTHER;
   case LOAD: 
   case STORE:  return LIT0 && SZi  && CC0 &&  TR1 && TR2 &&  N3 && XOTHER;
   case MOV:    return LITm && SZ4m && CC0 && TRL1 && TR2 &&  N3 && XOTHER;
   case CMOV:   return LIT0 && SZ4  && CCg &&  TR1 && TR2 &&  N3 && XCONDi;
   case WIDEN:  return LIT0 && SZ42 && CC0 &&  TR1 &&  N2 &&  N3 && XWIDEN;
   case JMP:    return LITm && SZ0  && CCj && TRL1 &&  N2 &&  N3 && XJMP;
   case CALLM:  return LIT0 && SZ0 /*any*/ &&  Ls1 &&  N2 &&  N3 && XOTHER;
   case CALLM_S: 
   case CALLM_E:return LIT0 && SZ0  && CC0 &&   N1 &&  N2 &&  N3 && XOTHER;
   case PUSH: 
   case POP:    return LIT0 && SZi  && CC0 &&  TR1 &&  N2 &&  N3 && XOTHER;
   case CLEAR:  return LIT0 && SZ0  && CC0 &&  Ls1 &&  N2 &&  N3 && XOTHER;
   case AND:
   case OR:     return LIT0 && SZi  && CCa &&  TR1 && TR2 &&  N3 && XOTHER;
   case MUL:    return LIT0 && SZ42 && CCa && TRA1 &&TRA2 &&  N3 && XOTHER;
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
   case LEA2:   return /*any*/ SZ4  && CC0 &&  TR1 && TR2 && TR3 && XLEA2;
   case INCEIP: return LIT0 && SZ0  && CC0 &&  Ls1 &&  N2 &&  N3 && XOTHER;
   case CCALL:  return LIT1 && SZ0  && CC0 && 
                       (u->argc > 0                   ? TR1 : N1) && 
                       (u->argc > 1                   ? TR2 : N2) && 
                       (u->argc > 2 || u->has_ret_val ? TR3 : N3) &&
                       u->regparms_n <= u->argc && XCCALL;
   /* Fields checked:       lit32   size  flags_r/w tag1   tag2   tag3    (rest) */
   case MMX1:
   case MMX2:         return LIT0 && SZ0  && CC0 &&  Ls1 &&  N2 &&  N3 && XOTHER;
   case MMX3:         return LIT0 && SZ0  && CC0 &&  Ls1 && Ls2 &&  N3 && XOTHER;
   case MMX2_MemRd:   return LIT0 && SZ48 && CC0 &&  Ls1 && TR2 &&  N3 && XOTHER;
   case MMX2_MemWr:   return LIT0 && SZ48 && CC0 &&  Ls1 && TR2 &&  N3 && XOTHER;
   case MMX2a1_MemRd: return LIT0 && SZ8  && CC0 &&  Ls1 && Ls2 && TR3 && XOTHER;
   case MMX2_ERegRd:  return LIT0 && SZ4  && CC0 &&  Ls1 && TR2 &&  N3 && XOTHER;
   case MMX2_ERegWr:  return LIT0 && SZ4  && CC0 &&  Ls1 && TR2 &&  N3 && XOTHER;

   /* Fields checked:        lit32   size  flags_r/w tag1   tag2   tag3    (rest) */
   case SSE2a_MemWr:  return LIT0 && SZsse2 && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE2a_MemRd:  return LIT0 && SZsse2 && CCa  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE2a1_MemRd: return LIT0 && SZsse3 && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE2g_RegWr:  return LIT0 && SZ4    && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE2g1_RegWr: return LIT8 && SZ4    && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE2e1_RegRd: return LIT8 && SZ2    && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3a_MemWr:  return LIT0 && SZsse3 && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3a_MemRd:  return LIT0 && SZsse3 && CCa  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3e_RegRd:  return LIT0 && SZ4    && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3e_RegWr:  return LIT0 && SZ4    && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3a1_MemRd: return LIT8 && SZ816  && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3g_RegWr:  return LIT0 && SZ4    && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3g1_RegWr: return LIT8 && SZ4    && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3e1_RegRd: return LIT8 && SZ2    && CC0  && Ls1 && Ls2 && TR3 && XOTHER;
   case SSE3:         return LIT0 && SZ0    && CCa  && Ls1 && Ls2 && N3  && XOTHER;
   case SSE4:         return LIT0 && SZ0    && CCa  && Ls1 && Ls2 && N3  && XOTHER;
   case SSE5:         return LIT0 && SZ0    && CC0  && Ls1 && Ls2 && Ls3 && XOTHER;
   case SSE3ag_MemRd_RegWr:
                      return         SZ48   && CC0  && TR1 && TR2 && N3  && XOTHER;
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
#  undef LIT8
#  undef LITm
#  undef SZ16
#  undef SZ8
#  undef SZ4
#  undef SZ2
#  undef SZ1
#  undef SZ0
#  undef SZ42
#  undef SZ48
#  undef SZ416
#  undef SZ816
#  undef SZsse2
#  undef SZsse3
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
#  undef Ls2
#  undef Ls3
#  undef TRL1
#  undef TRAL1
#  undef TRA1
#  undef TRA2
#  undef N1
#  undef N2
#  undef N3
#  undef Se2
#  undef Se1
#  undef COND0
#  undef EXTRA4b0
#  undef EXTRA4b12
#  undef SG_WD0
#  undef JMPKIND0
#  undef CCALL0
#  undef XCONDi
#  undef XLEA2
#  undef XWIDEN
#  undef XJMP
#  undef XCCALL
#  undef XOTHER
}

void VG_(sanity_check_UInstr)( UInt n, UInstr* u )
{
   Bool sane = is_sane_UInstr(u);
   if (!sane) {
      VG_(printf)("\nInsane instruction:\n");
      VG_(pp_UInstr)(n, u);
      VG_(up_UInstr)(n, u);
      vg_assert(sane);
   }
}

/*------------------------------------------------------------*/
/*--- Printing uinstrs.                                    ---*/
/*------------------------------------------------------------*/

/* Global that dictates whether to print generated code at all stages */
Bool VG_(print_codegen);

Char* VG_(name_UCondcode) ( Condcode cond )
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
      default: VG_(core_panic)("name_UCondcode");
   }
}


void VG_(pp_UOperand) ( UInstr* u, Int operandNo, Int sz, Bool parens )
{
   VG_(core_panic)("pp_UInstrWorker");
#if 0
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
#endif
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
      case MUL:   return (upper ? "MUL" : "mul");
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
      case LOCK:    return "LOCK";
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
      case MMX1:       return "MMX1" ;
      case MMX2:       return "MMX2" ;
      case MMX3:       return "MMX3" ;
      case MMX2_MemRd: return "MMX2_MRd" ;
      case MMX2_MemWr: return "MMX2_MWr" ;
      case MMX2a1_MemRd: return "MMX2a1_MRd" ;
      case MMX2_ERegRd: return "MMX2_eRRd" ;
      case MMX2_ERegWr: return "MMX2_eRWr" ;
      case SSE2a_MemWr: return "SSE2a_MWr";
      case SSE2a_MemRd: return "SSE2a_MRd";
      case SSE2g_RegWr: return "SSE2g_RWr";
      case SSE2a1_MemRd: return "SSE2a1_MRd";
      case SSE2g1_RegWr: return "SSE2g1_RWr";
      case SSE2e1_RegRd: return "SSE2e1_RRd";
      case SSE3e_RegRd: return "SSE3e_RRd";
      case SSE3e_RegWr: return "SSE3e_RWr";
      case SSE3g_RegWr: return "SSE3g_RWr";
      case SSE3a1_MemRd: return "SSE3a1_MRd";
      case SSE3g1_RegWr: return "SSE3g1_RWr";
      case SSE3e1_RegRd: return "SSE3e1_RRd";
      case SSE3:        return "SSE3";
      case SSE4:        return "SSE4";
      case SSE5:        return "SSE5";
      case SSE3a_MemWr: return "SSE3a_MWr";
      case SSE3a_MemRd: return "SSE3a_MRd";
      case SSE3ag_MemRd_RegWr: return "SSE3ag_MemRd_RegWr";
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
   VG_(printf)("lit32:           0x%x\n", u->lit32);
   VG_(printf)("size:            %d\n", u->size);
   VG_(printf)("val1,val2,val3:  %d, %d, %d\n", u->val1, u->val2, u->val3);
   VG_(printf)("tag1,tag2,tag3:  %d, %d, %d\n", u->tag1, u->tag2, u->tag3);
   VG_(printf)("flags_r:         0x%x\n", u->flags_r);
   VG_(printf)("flags_w:         0x%x\n", u->flags_w);
   VG_(printf)("extra4b:         0x%x\n", u->extra4b);
   VG_(printf)("cond:            0x%x\n", u->cond);
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
   VG_(core_panic)("pp_UInstrWorker");
#if 0
   VG_(printf)("\t%4d: %s", instrNo, 
                            VG_(name_UOpcode)(True, u->opcode));
   // For JMP, the condition goes before the size
   if (u->opcode == JMP)
      VG_(printf)("%s", VG_(name_UCondcode)(u->cond));

   switch (u->size) {
      case 0:  VG_(printf)("o"); break;
      case 1:  VG_(printf)("B"); break;
      case 2:  VG_(printf)("W"); break;
      case 4:  VG_(printf)("L"); break;
      case 8:  VG_(printf)("Q"); break;
      case 16: VG_(printf)("QQ"); break;
      default: VG_(printf)("%d", (Int)u->size); break;
   }

   // For CC2VAL and CMOV, the condition goes after the size
   if (u->opcode == CC2VAL || u->opcode == CMOV)
      VG_(printf)("%s", VG_(name_UCondcode)(u->cond));

   // Append extra bits
   switch (u->opcode) {
   case JMP:
      switch (u->jmpkind) {
         case JmpCall:      VG_(printf)("-c"); break;
         case JmpRet:       VG_(printf)("-r"); break;
         case JmpSyscall:   VG_(printf)("-sys"); break;
         case JmpClientReq: VG_(printf)("-cli"); break;
         case JmpYield:     VG_(printf)("-yld"); break;
         default: break;
      }
      break;

   case WIDEN:
      VG_(printf)("_%c%c", VG_(toupper)(nameISize(u->extra4b)),
                           u->signed_widen?'s':'z');
   } 
   VG_(printf)("       \t");

   switch (u->opcode) {

      case CALLM_S: case CALLM_E:
         break;

      case INCEIP:
         VG_(printf)("$%d", u->val1);
         break;

      case LEA2:
         VG_(printf)("%d(" , u->lit32);
         VG_(pp_UOperand)(u, 1, 4, False);
         VG_(printf)(",");
         VG_(pp_UOperand)(u, 2, 4, False);
         VG_(printf)(",%d), ", (Int)u->extra4b);
         VG_(pp_UOperand)(u, 3, 4, False);
         break;

      case LEA1:
         VG_(printf)("%d" , u->lit32);
         VG_(pp_UOperand)(u, 1, 4, True);
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, 4, False);
         break;

      case NOP: case LOCK:
         break;

      case FPU_W:
         VG_(printf)("0x%x:0x%x, ",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         VG_(pp_UOperand)(u, 2, 4, True);
         break;

      case FPU_R:
         VG_(printf)("");
         VG_(pp_UOperand)(u, 2, 4, True);
         VG_(printf)(", 0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         break;

      case FPU:
         VG_(printf)("0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         break;

      case MMX1:
         VG_(printf)("0x%x",
                     u->val1 & 0xFF );
         break;

      case MMX2:
         VG_(printf)("0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         break;

      case MMX3:
         VG_(printf)("0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, u->val2 & 0xFF );
         break;

      case MMX2_ERegWr:
      case MMX2_ERegRd:
         VG_(printf)("0x%x:0x%x, ",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         VG_(pp_UOperand)(u, 2, 4, False);
         break;
 
      case MMX2_MemWr:
      case MMX2_MemRd:
          VG_(printf)("0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         VG_(pp_UOperand)(u, 2, 4, True);
         break;

      case MMX2a1_MemRd:
          VG_(printf)("0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, u->val2 & 0xFF );
         VG_(pp_UOperand)(u, 3, 4, True);
         break;

      case SSE2a_MemWr:
      case SSE2a_MemRd:
      case SSE2g_RegWr:
      case SSE2g1_RegWr:
      case SSE2e1_RegRd:
         VG_(printf)("0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, u->val2 & 0xFF );
         VG_(pp_UOperand)(u, 3, 4, True);
         break;

      case SSE2a1_MemRd:
      case SSE3a_MemWr:
      case SSE3a_MemRd:
         VG_(printf)("0x%x:0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, 
                     (u->val2 >> 8) & 0xFF, u->val2 & 0xFF );
         VG_(pp_UOperand)(u, 3, 4, True);
         break;

      case SSE3e_RegWr:
      case SSE3e_RegRd:
      case SSE3g_RegWr:
         VG_(printf)("0x%x:0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, 
                     (u->val2 >> 8) & 0xFF, u->val2 & 0xFF );
         VG_(pp_UOperand)(u, 3, 4, True);
         break;

      case SSE3g1_RegWr:
      case SSE3e1_RegRd:
      case SSE3a1_MemRd:
         VG_(printf)("0x%x:0x%x:0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, 
                     (u->val2 >> 8) & 0xFF, u->val2 & 0xFF,
                     u->lit32 );
         VG_(pp_UOperand)(u, 3, 4, True);
         break;

      case SSE3:
         VG_(printf)("0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, 
                     u->val2 & 0xFF );
         break;

      case SSE4:
         VG_(printf)("0x%x:0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, 
                     (u->val2 >> 8) & 0xFF, u->val2 & 0xFF );
         break;

      case SSE5:
         VG_(printf)("0x%x:0x%x:0x%x:0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF, 
                     (u->val2 >> 8) & 0xFF, u->val2 & 0xFF,
                     u->val3 & 0xFF );
         break;

      case SSE3ag_MemRd_RegWr:
	 VG_(printf)("0x%x(addr=", u->lit32 );
	 VG_(pp_UOperand)(u, 1, 4, False);
	 VG_(printf)(", dst=");
	 VG_(pp_UOperand)(u, 2, 4, False);
         VG_(printf)(")");
         break;

      case GET: case PUT: case MOV: case LOAD: case STORE: case CMOV:
      case GETSEG: case PUTSEG:
         VG_(pp_UOperand)(u, 1, u->size, u->opcode==LOAD); 
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, u->size, u->opcode==STORE);
         break;

      case JMP:
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
         VG_(pp_UOperand)(u, 1, u->size, False);
         break;

      /* Print a "(s)" after args passed on stack */
      case CCALL:
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
      case MUL:   
         VG_(pp_UOperand)(u, 1, u->size, False); 
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, u->size, False);
         break;

      case WIDEN:
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
#endif
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
   the caller (regs), which is assumed to be big enough.  Return the number
   of entries.  Written regs are indicated in parallel array isWrites.
   Insns which read _and_ write a register wind up mentioning it twice.
   Entries are placed in the array in program order, so that if a reg is
   read-modified-written, it appears first as a read and then as a write.
   'tag' indicates whether we are looking at TempRegs or RealRegs.
*/
Int VG_(get_reg_usage) ( UInstr* u, Tag tag, Int* regs, Bool* isWrites )
{
#  define RD(ono)    VG_UINSTR_READS_REG(ono, regs, isWrites)
#  define WR(ono)    VG_UINSTR_WRITES_REG(ono, regs, isWrites)

   Int n = 0;
   switch (u->opcode) {
      case LEA1: RD(1); WR(2); break;
      case LEA2: RD(1); RD(2); WR(3); break;

      case SSE3a1_MemRd:
      case SSE2a1_MemRd:
      case SSE2e1_RegRd:
      case SSE3e_RegRd:
      case SSE3a_MemWr:
      case SSE3a_MemRd:
      case SSE2a_MemWr: 
      case SSE3e1_RegRd:
      case SSE2a_MemRd: RD(3); break;

      case SSE2g_RegWr:
      case SSE2g1_RegWr:
      case SSE3e_RegWr:
      case SSE3g1_RegWr:
      case SSE3g_RegWr: WR(3); break;

      case SSE3ag_MemRd_RegWr: RD(1); WR(2); break;

      case MMX2a1_MemRd: RD(3); break;
      case MMX2_ERegRd: RD(2); break;
      case MMX2_ERegWr: WR(2); break;

      case SSE4: case SSE3: case SSE5:
      case MMX1: case MMX2: case MMX3:
      case NOP:   case FPU:   case INCEIP: case CALLM_S: case CALLM_E:
      case CLEAR: case CALLM: case LOCK: break;

      case CCALL:
         if (u->argc > 0)    RD(1); 
         if (u->argc > 1)    RD(2); 
         if (u->argc > 2)    RD(3); 
         if (u->has_ret_val) WR(3);
         break;

      case MMX2_MemRd: case MMX2_MemWr:
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
      case MUL:
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
            return SK_(get_Xreg_usage)(u, tag, regs, isWrites);
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


static __inline__
Bool uInstrMentionsTempReg ( UInstr* u, Int tempreg )
{
   Int i, k;
   Int tempUse[VG_MAX_REGS_USED];
   Bool notUsed[VG_MAX_REGS_USED];

   k = VG_(get_reg_usage) ( u, TempReg, &tempUse[0], &notUsed[0] );
   for (i = 0; i < k; i++)
      if (tempUse[i] == tempreg)
         return True;
   return False;
}


/*------------------------------------------------------------*/
/*--- ucode improvement.                                   ---*/
/*------------------------------------------------------------*/

/*------------------------------------------------------------*/
/*--- %SP-update pass                                      ---*/
/*------------------------------------------------------------*/

static
IRBB* vg_SP_update_pass ( IRBB* bb_in, VexGuestLayoutInfo* layout )
{
   Int      i, j, minoff_ST, maxoff_ST;
   IRDirty  *dcall, *d;
   IRStmt*  st;
   IRExpr*  e;
   IRArray* descr;

   /* Set up BB */
   IRBB* bb     = emptyIRBB();
   bb->tyenv    = dopyIRTypeEnv(bb_in->tyenv);
   bb->next     = dopyIRExpr(bb_in->next);
   bb->jumpkind = bb_in->jumpkind;

   IRTemp curr  = INVALID_IRTEMP;
   Long   delta = 0;

   Int    sizeof_SP = layout->sizeof_SP;
   Int    offset_SP = layout->offset_SP;
   IRType typeof_SP = sizeof_SP==4 ? Ity_I32 : Ity_I64;
   vg_assert(sizeof_SP == 4 || sizeof_SP == 8);

#  define IS_ADD(op) (sizeof_SP==4 ? ((op)==Iop_Add32) : ((op)==Iop_Add64))
#  define IS_SUB(op) (sizeof_SP==4 ? ((op)==Iop_Sub32) : ((op)==Iop_Sub64))

#  define IS_ADD_OR_SUB(op) (IS_ADD(op) || IS_SUB(op))

#  define GET_CONST(con)                                                \
       (sizeof_SP==4 ? (Long)(Int)(con->Ico.U32)                        \
                     : (Long)(con->Ico.U64))

#  define DO(kind, syze)                                                \
      do {                                                              \
         if (!VG_(defined_##kind##_mem_stack_##syze)())                 \
            goto generic;                                               \
                                                                        \
         /* I don't know if it's really necessary to say that the */    \
         /* call reads the stack pointer.  But anyway, we do. */        \
         dcall = unsafeIRDirty_0_N(                                     \
                    1/*regparms*/,                                      \
                    "track_" #kind "_mem_stack_" #syze,                 \
                    VG_(tool_interface)                                 \
                               .track_##kind##_mem_stack_##syze,        \
                    mkIRExprVec_1(IRExpr_Tmp(curr))                     \
                 );                                                     \
         dcall->nFxState = 1;                                           \
         dcall->fxState[0].fx     = Ifx_Read;                           \
         dcall->fxState[0].offset = layout->offset_SP;                  \
         dcall->fxState[0].size   = layout->sizeof_SP;                  \
                                                                        \
         addStmtToIRBB( bb, IRStmt_Dirty(dcall) );                      \
      } while (0)

   for (i = 0; i <  bb_in->stmts_used; i++) {

      st = bb_in->stmts[i];
      if (!st)
         continue;

      /* t = Get(sp):   curr = t, delta = 0 */
      if (st->tag != Ist_Tmp) goto case2;
      e = st->Ist.Tmp.data;
      if (e->tag != Iex_Get)              goto case2;
      if (e->Iex.Get.offset != offset_SP) goto case2;
      if (e->Iex.Get.ty != typeof_SP)     goto case2;
      curr = st->Ist.Tmp.tmp;
      delta = 0;
      addStmtToIRBB( bb, st );
      continue;

     case2:
      /* t' = curr +/- const:   curr = t',  delta +=/-= const */
      if (st->tag != Ist_Tmp) goto case3;
      e = st->Ist.Tmp.data;
      if (e->tag != Iex_Binop) goto case3;
      if (e->Iex.Binop.arg1->tag != Iex_Tmp) goto case3;
      if (e->Iex.Binop.arg1->Iex.Tmp.tmp != curr) goto case3;
      if (e->Iex.Binop.arg2->tag != Iex_Const) goto case3;
      if (!IS_ADD_OR_SUB(e->Iex.Binop.op)) goto case3;
      curr = st->Ist.Tmp.tmp;
      if (IS_ADD(e->Iex.Binop.op))
         delta += GET_CONST(e->Iex.Binop.arg2->Iex.Const.con);
      else
         delta -= GET_CONST(e->Iex.Binop.arg2->Iex.Const.con);
      addStmtToIRBB( bb, st );
      continue;

     case3:
      /* t' = curr:   curr = t' */
      if (st->tag != Ist_Tmp) goto case4;
      e = st->Ist.Tmp.data;
      if (e->tag != Iex_Tmp) goto case4;
      if (e->Iex.Tmp.tmp != curr) goto case4;
      curr = st->Ist.Tmp.tmp;
      addStmtToIRBB( bb, st );
      continue;

     case4:
      /* Put(sp) = curr */
      if (st->tag != Ist_Put) goto case5;
      if (st->Ist.Put.offset != offset_SP) goto case5;
      if (st->Ist.Put.data->tag != Iex_Tmp) goto case5;
      if (st->Ist.Put.data->Iex.Tmp.tmp == curr) {
         switch (delta) {
#if 1
            case   0:              addStmtToIRBB(bb,st); delta = 0; continue;
            case   4: DO(die, 4);  addStmtToIRBB(bb,st); delta = 0; continue;
            case  -4: DO(new, 4);  addStmtToIRBB(bb,st); delta = 0; continue;
            case   8: DO(die, 8);  addStmtToIRBB(bb,st); delta = 0; continue;
            case  -8: DO(new, 8);  addStmtToIRBB(bb,st); delta = 0; continue;
            case  12: DO(die, 12); addStmtToIRBB(bb,st); delta = 0; continue;
            case -12: DO(new, 12); addStmtToIRBB(bb,st); delta = 0; continue;
            case  16: DO(die, 16); addStmtToIRBB(bb,st); delta = 0; continue;
            case -16: DO(new, 16); addStmtToIRBB(bb,st); delta = 0; continue;
            case  32: DO(die, 32); addStmtToIRBB(bb,st); delta = 0; continue;
            case -32: DO(new, 32); addStmtToIRBB(bb,st); delta = 0; continue;
#endif
            default:  goto generic;
         }
      } else {
        generic:
         /* I don't know if it's really necessary to say that the call
            reads the stack pointer.  But anyway, we do. */
         dcall = unsafeIRDirty_0_N( 
                    1/*regparms*/, 
                    "VG_(unknown_esp_update)", &VG_(unknown_esp_update),
                    mkIRExprVec_1(st->Ist.Put.data) 
                 );
         dcall->nFxState = 1;
         dcall->fxState[0].fx     = Ifx_Read;
         dcall->fxState[0].offset = layout->offset_SP;
         dcall->fxState[0].size   = layout->sizeof_SP;

         addStmtToIRBB( bb, IRStmt_Dirty(dcall) );
         addStmtToIRBB(bb,st);

         curr = st->Ist.Put.data->Iex.Tmp.tmp;
         delta = 0;
         continue;
      }

     case5:
      /* PutI or Dirty call which overlaps SP: complain.  We can't
         deal with SP changing in weird ways (well, we can, but not at
         this time of night).  */
      if (st->tag == Ist_PutI) {
         descr = st->Ist.PutI.descr;
         minoff_ST = descr->base;
         maxoff_ST = descr->base + descr->nElems * sizeofIRType(descr->elemTy) - 1;
         if (!(offset_SP > maxoff_ST || (offset_SP + sizeof_SP - 1) < minoff_ST))
            goto complain;
      }
      if (st->tag == Ist_Dirty) {
         d = st->Ist.Dirty.details;
         for (j = 0; j < d->nFxState; j++) {
            minoff_ST = d->fxState[j].offset;
            maxoff_ST = d->fxState[j].offset + d->fxState[j].size - 1;
            if (d->fxState[j].fx == Ifx_Read || d->fxState[j].fx == Ifx_None)
               continue;
            if (!(offset_SP > maxoff_ST || (offset_SP + sizeof_SP - 1) < minoff_ST))
               goto complain;
         }
      }

      /* well, not interesting.  Just copy and keep going. */
      addStmtToIRBB( bb, st );

   } /* for (i = 0; i <  bb_in->stmts_used; i++) */

   return bb;

  complain:
   VG_(core_panic)("vg_SP_update_pass: PutI or Dirty which overlaps SP");

}



#if 0
   for (i = 0; i <  bb_in->stmts_used; i++) {
      st = bb_in->stmts[i];
      if (!st)
         continue;
      if (st->tag != Ist_Put) 
         goto boring;
      offP = st->Ist.Put.offset;
      if (offP != layout->offset_SP) 
         goto boring;
      szP = sizeofIRType(typeOfIRExpr(bb_in->tyenv, st->Ist.Put.data));
      if (szP != layout->sizeof_SP)
         goto boring;
      vg_assert(isAtom(st->Ist.Put.data));

      /* I don't know if it's really necessary to say that the call reads
         the stack pointer.  But anyway, we do. */      
      dcall = unsafeIRDirty_0_N( 
                 mkIRCallee(1, "VG_(unknown_esp_update)", 
                            (HWord)&VG_(unknown_esp_update)),
                 mkIRExprVec_1(st->Ist.Put.data) 
              );
      dcall->nFxState = 1;
      dcall->fxState[0].fx     = Ifx_Read;
      dcall->fxState[0].offset = layout->offset_SP;
      dcall->fxState[0].size   = layout->sizeof_SP;

      addStmtToIRBB( bb, IRStmt_Dirty(dcall) );

     boring:
      addStmtToIRBB( bb, st );
   }
#endif


/*------------------------------------------------------------*/
/*--- Main entry point for the JITter.                     ---*/
/*------------------------------------------------------------*/

/* Vex dumps the final code in here.  Then we can copy it off
   wherever we like. */
#define N_TMPBUF 10000
static UChar tmpbuf[N_TMPBUF];

/* Function pointers we must supply to LibVEX in order that it
   can bomb out and emit messages under Valgrind's control. */
__attribute__ ((noreturn))
static
void failure_exit ( void )
{
   LibVEX_ClearTemporary(True);
   VG_(printf)("VEX did failure_exit.  Bye.\n");
   vg_assert(0); //VG_(exit)(1);
}

static
void log_bytes ( Char* bytes, Int nbytes )
{
  Int i;
  for (i = 0; i < nbytes-3; i += 4)
     VG_(printf)("%c%c%c%c", bytes[i], bytes[i+1], bytes[i+2], bytes[i+3]);
  for (; i < nbytes; i++) 
     VG_(printf)("%c", bytes[i]);
}

/* Translate the basic block beginning at orig_addr, and add it to
   the translation cache & translation table.  Unless 'debugging' is true,
   in which case the call is being done for debugging purposes, so
   (a) throw away the translation once it is made, and (b) produce a
   load of debugging output. 

   'tid' is the identity of the thread needing this block.
*/

Bool VG_(translate) ( ThreadId tid, Addr orig_addr,
                      Bool debugging_translation )
{
   Addr        redir, orig_addr0 = orig_addr;
   Int         orig_size, tmpbuf_used;
   Bool        notrace_until_done;
   UInt        notrace_until_limit = 0;
   UInt        FULLTRACE_LIMIT = 1; //21068;
   Segment     *seg;

   /* Make sure Vex is initialised right. */
   TranslateResult tres;
   static Bool vex_init_done = False;

   if (!vex_init_done) {
      LibVEX_Init ( &failure_exit, &log_bytes, 
                    1,     /* debug_paranoia */ 
                    False, /* valgrind support */
                    &VG_(clo_vex_control) );
      vex_init_done = True;
   }

   /* profiling ... */
   VGP_PUSHCC(VgpTranslate);

   /* Look in the code redirect table to see if we should
      translate an alternative address for orig_addr. */
   redir = VG_(code_redirect)(orig_addr);

   if (redir != orig_addr && VG_(clo_verbosity) >= 2) {
      VG_(message)(Vg_UserMsg, 
                   "TRANSLATE: %p redirected to %p",
                   orig_addr, 
                   redir );
   }
   orig_addr = redir;

   /* If codegen tracing, don't start tracing until
      notrace_until_limit blocks have gone by.  This avoids printing
      huge amounts of useless junk when all we want to see is the last
      few blocks translated prior to a failure.  Set
      notrace_until_limit to be the number of translations to be made
      before --trace-codegen= style printing takes effect. */
   notrace_until_done = VG_(get_bbs_translated)() >= notrace_until_limit;

   seg = VG_(find_segment)(orig_addr);

   if (!debugging_translation)
      VG_TRACK( pre_mem_read, Vg_CoreTranslate, tid, "", orig_addr, 1 );

   if (seg == NULL ||
       !VG_(seg_contains)(seg, orig_addr, 1) || 
       (seg->prot & (VKI_PROT_READ|VKI_PROT_EXEC)) == 0) {
      /* Code address is bad - deliver a signal instead */
      vg_assert(!VG_(is_addressable)(orig_addr, 1));

      if (seg != NULL && VG_(seg_contains)(seg, orig_addr, 1)) {
         vg_assert((seg->prot & VKI_PROT_EXEC) == 0);
         VG_(synth_fault_perms)(tid, orig_addr);
      } else
         VG_(synth_fault_mapping)(tid, orig_addr);

      return False;
   } else
      seg->flags |= SF_CODE;        /* contains cached code */

   /* If doing any code printing, print a basic block start marker */
   if (VG_(clo_trace_codegen)) {
      Char fnname[64] = "";
      VG_(get_fnname_if_entry)(orig_addr, fnname, 64);
      VG_(printf)(
              "==== BB %d %s(%p) approx BBs exec'd %llu ====\n",
              VG_(get_bbs_translated)(), fnname, orig_addr, 
              VG_(bbs_done));
   }

   /* True if a debug trans., or if bit N set in VG_(clo_trace_codegen). */
#if 0
#  define DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE(n)               \
      ( debugging_translation                                   \
        || (notrace_until_done                                  \
            && (VG_(clo_trace_codegen) & (1 << (n-1))) ))
#else
#  define DECIDE_IF_PRINTING_CODEGEN                            \
      ( debugging_translation                                   \
        || (VG_(clo_trace_codegen) > 0                          \
            && VG_(get_bbs_translated)() >= FULLTRACE_LIMIT))
#endif

   /* Actually do the translation. */
   tres = LibVEX_Translate ( 
             InsnSetX86, InsnSetX86,
             (Char*)orig_addr, (Addr64)orig_addr, &orig_size,
             tmpbuf, N_TMPBUF, &tmpbuf_used,
             SK_(instrument),
             VG_(need_to_handle_esp_assignment)()
                ? vg_SP_update_pass
                : NULL,
             NULL,
             VG_(clo_trace_codegen)
          );

   vg_assert(tres == TransOK);
   vg_assert(tmpbuf_used <= N_TMPBUF);
   vg_assert(tmpbuf_used > 0);

#undef DECIDE_IF_PRINTING_CODEGEN_FOR_PHASE

   /* Copy data at trans_addr into the translation cache. */
   /* Since the .orig_size and .trans_size fields are UShort, be paranoid. */
   vg_assert(orig_size > 0 && orig_size < 65536);
   vg_assert(tmpbuf_used > 0 && tmpbuf_used < 65536);

   // If debugging, don't do anything with the translated block;  we
   // only did this for the debugging output produced along the way.
   if (!debugging_translation) {
      // Note that we use orig_addr0, not orig_addr, which might have been
      // changed by the redirection
      VG_(add_to_trans_tab)( orig_addr0, orig_size, 
                             (Addr)(&tmpbuf[0]), tmpbuf_used );
   }

   VGP_POPCC(VgpTranslate);

   return True;
}


/*--------------------------------------------------------------------*/
/*--- end                                           vg_translate.c ---*/
/*--------------------------------------------------------------------*/

