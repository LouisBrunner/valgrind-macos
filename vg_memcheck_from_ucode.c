
/*--------------------------------------------------------------------*/
/*--- Part of the MemCheck skin: Generate code for skin-specific   ---*/
/*--- UInstrs.                                                     ---*/
/*---                                     vg_memcheck_from_ucode.c ---*/
/*--------------------------------------------------------------------*/
/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

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

#include "vg_memcheck_include.h"

/*------------------------------------------------------------*/
/*--- Renamings of frequently-used global functions.       ---*/
/*------------------------------------------------------------*/

#define nameIReg  VG_(nameOfIntReg)
#define nameISize VG_(nameOfIntSize)

#define dis       VG_(print_codegen)

/*------------------------------------------------------------*/
/*--- Instruction emission -- turning final uinstrs back   ---*/
/*--- into x86 code.                                       ---*/
/*------------------------------------------------------------*/

/* See the corresponding comment at the top of vg_from_ucode.c to find out
 * how all this works */

/*----------------------------------------------------*/
/*--- v-size (4, or 2 with OSO) insn emitters      ---*/
/*----------------------------------------------------*/

static void emit_testv_lit_reg ( Int sz, UInt lit, Int reg )
{
   VG_(newEmit)();
   if (sz == 2) {
      VG_(emitB) ( 0x66 );
   } else {
      vg_assert(sz == 4);
   }
   VG_(emitB) ( 0xF7 ); /* Grp3 Ev */
   VG_(emit_amode_ereg_greg) ( reg, 0 /* Grp3 subopcode for TEST */ );
   if (sz == 2) VG_(emitW) ( lit ); else VG_(emitL) ( lit );
   if (dis)
      VG_(printf)("\n\t\ttest%c $0x%x, %s\n", nameISize(sz), 
                                            lit, nameIReg(sz,reg));
}

static void emit_testv_lit_offregmem ( Int sz, UInt lit, Int off, Int reg )
{
   VG_(newEmit)();
   if (sz == 2) {
      VG_(emitB) ( 0x66 );
   } else {
      vg_assert(sz == 4);
   }
   VG_(emitB) ( 0xF7 ); /* Grp3 Ev */
   VG_(emit_amode_offregmem_reg) ( off, reg, 0 /* Grp3 subopcode for TEST */ );
   if (sz == 2) VG_(emitW) ( lit ); else VG_(emitL) ( lit );
   if (dis)
      VG_(printf)("\n\t\ttest%c $%d, 0x%x(%s)\n", 
                  nameISize(sz), lit, off, nameIReg(4,reg) );
}

/*----------------------------------------------------*/
/*--- Instruction synthesisers                     ---*/
/*----------------------------------------------------*/

/* Synthesise a minimal test (and which discards result) of reg32
   against lit.  It's always safe do simply
      emit_testv_lit_reg ( 4, lit, reg32 )
   but we try to do better when possible.
*/
static void synth_minimal_test_lit_reg ( UInt lit, Int reg32 )
{
   if ((lit & 0xFFFFFF00) == 0 && reg32 < 4) {
      /* We can get away with a byte insn. */
      VG_(emit_testb_lit_reg) ( lit, reg32 );
   }
   else 
   if ((lit & 0xFFFF0000) == 0) {
      /* Literal fits in 16 bits; do a word insn. */
      emit_testv_lit_reg ( 2, lit, reg32 );
   }
   else {
      /* Totally general ... */
      emit_testv_lit_reg ( 4, lit, reg32 );
   }
}

/*----------------------------------------------------*/
/*--- Top level of the uinstr -> x86 translation.  ---*/
/*----------------------------------------------------*/

static void synth_LOADV ( Int sz, Int a_reg, Int tv_reg,
                          RRegSet regs_live_before,
                          RRegSet regs_live_after )
{
   Addr helper;
   UInt argv[] = { a_reg };
   UInt tagv[] = { RealReg };

   switch (sz) {
      case 4: helper = (Addr) & SK_(helperc_LOADV4); break;
      case 2: helper = (Addr) & SK_(helperc_LOADV2); break;
      case 1: helper = (Addr) & SK_(helperc_LOADV1); break;
      default: VG_(panic)("synth_LOADV");
   }
   VG_(synth_ccall) ( helper, 1, 1, argv, tagv, tv_reg,
                      regs_live_before, regs_live_after );
}


static void synth_STOREV ( Int sz, Int tv_tag, Int tv_val, Int a_reg,
                           RRegSet regs_live_before,
                           RRegSet regs_live_after )
{
   Addr helper;
   UInt argv[] = { a_reg,   tv_val };
   Tag  tagv[] = { RealReg, tv_tag };

   vg_assert(tv_tag == RealReg || tv_tag == Literal);
   switch (sz) {
      case 4: helper = (Addr) SK_(helperc_STOREV4); break;
      case 2: helper = (Addr) SK_(helperc_STOREV2); break;
      case 1: helper = (Addr) SK_(helperc_STOREV1); break;
      default: VG_(panic)("synth_STOREV");
   }
   VG_(synth_ccall) ( helper, 2, 2, argv, tagv, INVALID_REALREG,
                      regs_live_before, regs_live_after );
}


static void synth_SETV ( Int sz, Int reg )
{
   UInt val;
   switch (sz) {
      case 4: val = 0x00000000; break;
      case 2: val = 0xFFFF0000; break;
      case 1: val = 0xFFFFFF00; break;
      case 0: val = 0xFFFFFFFE; break;
      default: VG_(panic)("synth_SETV");
   }
   VG_(emit_movv_lit_reg) ( 4, val, reg );
}


static void synth_TESTV ( Int sz, Int tag, Int val )
{
   vg_assert(tag == ArchReg || tag == RealReg);
   if (tag == ArchReg) {
      switch (sz) {
         case 4: 
            emit_testv_lit_offregmem ( 
               4, 0xFFFFFFFF, VG_(shadowRegOffset)(val), R_EBP );
            break;
         case 2: 
            emit_testv_lit_offregmem ( 
               4, 0x0000FFFF, VG_(shadowRegOffset)(val), R_EBP );
            break;
         case 1:
            if (val < 4) {
               emit_testv_lit_offregmem ( 
                  4, 0x000000FF, VG_(shadowRegOffset)(val), R_EBP );
            } else {
               emit_testv_lit_offregmem ( 
                  4, 0x0000FF00, VG_(shadowRegOffset)(val-4), R_EBP );
            }
            break;
         case 0: 
            /* should never happen */
         default: 
            VG_(panic)("synth_TESTV(ArchReg)");
      }
   } else {
      switch (sz) {
         case 4:
            /* Works, but holds the entire 32-bit literal, hence
               generating a 6-byte insn.  We want to know if any bits
               in the reg are set, but since this is for the full reg,
               we might as well compare it against zero, which can be
               done with a shorter insn. */
            /* synth_minimal_test_lit_reg ( 0xFFFFFFFF, val ); */
            VG_(emit_cmpl_zero_reg) ( val );
            break;
         case 2:
            synth_minimal_test_lit_reg ( 0x0000FFFF, val );
            break;
         case 1:
            synth_minimal_test_lit_reg ( 0x000000FF, val );
            break;
         case 0:
            synth_minimal_test_lit_reg ( 0x00000001, val );
            break;
         default: 
            VG_(panic)("synth_TESTV(RealReg)");
      }
   }
   VG_(emit_jcondshort_delta) ( CondZ, 3 );
   VG_(synth_call) (
      True, /* needed to guarantee that this insn is indeed 3 bytes long */
      ( sz==4 
      ? VG_(helper_offset)((Addr) & SK_(helper_value_check4_fail))
      : ( sz==2 
        ? VG_(helper_offset)((Addr) & SK_(helper_value_check2_fail))
        : ( sz==1 
          ? VG_(helper_offset)((Addr) & SK_(helper_value_check1_fail))
          : VG_(helper_offset)((Addr) & SK_(helper_value_check0_fail)))))
   );
}


static void synth_GETV ( Int sz, Int arch, Int reg )
{
   /* VG_(printf)("synth_GETV %d of Arch %s\n", sz, nameIReg(sz, arch)); */
   switch (sz) {
      case 4: 
         VG_(emit_movv_offregmem_reg) ( 4, VG_(shadowRegOffset)(arch),
                                        R_EBP, reg );
         break;
      case 2: 
         VG_(emit_movzwl_offregmem_reg) ( VG_(shadowRegOffset)(arch),
                                          R_EBP, reg );
         VG_(emit_nonshiftopv_lit_reg) ( 4, OR, 0xFFFF0000, reg );
         break;
      case 1: 
         if (arch < 4) {
            VG_(emit_movzbl_offregmem_reg) ( VG_(shadowRegOffset)(arch),
                                             R_EBP, reg );
         } else {
            VG_(emit_movzbl_offregmem_reg) ( VG_(shadowRegOffset)(arch-4)+1,
                                             R_EBP, reg );
         }
         VG_(emit_nonshiftopv_lit_reg) ( 4, OR, 0xFFFFFF00, reg );
         break;
      default: 
         VG_(panic)("synth_GETV");
   }
}


static void synth_PUTV ( Int sz, Int srcTag, UInt lit_or_reg, Int arch )
{
   if (srcTag == Literal) {
     /* PUTV with a Literal is only ever used to set the corresponding
        ArchReg to `all valid'.  Should really be a kind of SETV. */
      UInt lit = lit_or_reg;
      switch (sz) {
         case 4:
            vg_assert(lit == 0x00000000);
            VG_(emit_movv_lit_offregmem) ( 4, 0x00000000, 
                                      VG_(shadowRegOffset)(arch), R_EBP );
            break;
         case 2:
            vg_assert(lit == 0xFFFF0000);
            VG_(emit_movv_lit_offregmem) ( 2, 0x0000, 
                                      VG_(shadowRegOffset)(arch), R_EBP );
            break;
         case 1:
            vg_assert(lit == 0xFFFFFF00);
            if (arch < 4) {
               VG_(emit_movb_lit_offregmem) ( 0x00, 
                                         VG_(shadowRegOffset)(arch), R_EBP );
            } else {
               VG_(emit_movb_lit_offregmem) ( 0x00, 
                                              VG_(shadowRegOffset)(arch-4)+1,
                                              R_EBP );
            }
            break;
         default: 
            VG_(panic)("synth_PUTV(lit)");
      }

   } else {

      UInt reg;
      vg_assert(srcTag == RealReg);

      if (sz == 1 && lit_or_reg >= 4) {
         VG_(emit_swapl_reg_EAX) ( lit_or_reg );
         reg = R_EAX;
      } else {
         reg = lit_or_reg;
      }

      if (sz == 1) vg_assert(reg < 4);

      switch (sz) {
         case 4:
            VG_(emit_movv_reg_offregmem) ( 4, reg,
                                      VG_(shadowRegOffset)(arch), R_EBP );
            break;
         case 2:
            VG_(emit_movv_reg_offregmem) ( 2, reg,
                                      VG_(shadowRegOffset)(arch), R_EBP );
            break;
         case 1:
            if (arch < 4) {
               VG_(emit_movb_reg_offregmem) ( reg,
                                         VG_(shadowRegOffset)(arch), R_EBP );
	    } else {
               VG_(emit_movb_reg_offregmem) ( reg,
                                        VG_(shadowRegOffset)(arch-4)+1, R_EBP );
            }
            break;
         default: 
            VG_(panic)("synth_PUTV(reg)");
      }

      if (sz == 1 && lit_or_reg >= 4) {
         VG_(emit_swapl_reg_EAX) ( lit_or_reg );
      }
   }
}


static void synth_GETVF ( Int reg )
{
   VG_(emit_movv_offregmem_reg) ( 4, VG_(shadowFlagsOffset)(), R_EBP, reg );
   /* paranoia only; should be unnecessary ... */
   /* VG_(emit_nonshiftopv_lit_reg) ( 4, OR, 0xFFFFFFFE, reg ); */
}


static void synth_PUTVF ( UInt reg )
{
   VG_(emit_movv_reg_offregmem) ( 4, reg, VG_(shadowFlagsOffset)(), R_EBP );
}


static void synth_TAG1_op ( TagOp op, Int reg, RRegSet regs_live_after )
{
   switch (op) {

      /* Scheme is
            neg<sz> %reg          -- CF = %reg==0 ? 0 : 1
            sbbl %reg, %reg       -- %reg = -CF
            or 0xFFFFFFFE, %reg   -- invalidate all bits except lowest
      */
      case Tag_PCast40:
         VG_(emit_unaryopv_reg)(4, NEG, reg);
         VG_(emit_nonshiftopv_reg_reg)(4, SBB, reg, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFFFFFE, reg);
         break;
      case Tag_PCast20:
         VG_(emit_unaryopv_reg)(2, NEG, reg);
         VG_(emit_nonshiftopv_reg_reg)(4, SBB, reg, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFFFFFE, reg);
         break;
      case Tag_PCast10:
         if (reg >= 4) {
            VG_(emit_swapl_reg_EAX)(reg);
            VG_(emit_unaryopb_reg)(NEG, R_EAX);
            VG_(emit_swapl_reg_EAX)(reg);
         } else {
            VG_(emit_unaryopb_reg)(NEG, reg);
         }
         VG_(emit_nonshiftopv_reg_reg)(4, SBB, reg, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFFFFFE, reg);
         break;

      /* Scheme is
            andl $1, %reg -- %reg is 0 or 1
            negl %reg -- %reg is 0 or 0xFFFFFFFF
            and possibly an OR to invalidate unused bits.
      */
      case Tag_PCast04:
         VG_(emit_nonshiftopv_lit_reg)(4, AND, 0x00000001, reg);
         VG_(emit_unaryopv_reg)(4, NEG, reg);
         break;
      case Tag_PCast02:
         VG_(emit_nonshiftopv_lit_reg)(4, AND, 0x00000001, reg);
         VG_(emit_unaryopv_reg)(4, NEG, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFF0000, reg);
         break;
      case Tag_PCast01:
         VG_(emit_nonshiftopv_lit_reg)(4, AND, 0x00000001, reg);
         VG_(emit_unaryopv_reg)(4, NEG, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFFFF00, reg);
         break;

      /* Scheme is
            shl $24, %reg -- make irrelevant bits disappear
            negl %reg             -- CF = %reg==0 ? 0 : 1
            sbbl %reg, %reg       -- %reg = -CF
            and possibly an OR to invalidate unused bits.
      */
      case Tag_PCast14:
         VG_(emit_shiftopv_lit_reg)(4, SHL, 24, reg);
         VG_(emit_unaryopv_reg)(4, NEG, reg);
         VG_(emit_nonshiftopv_reg_reg)(4, SBB, reg, reg);
         break;
      case Tag_PCast12:
         VG_(emit_shiftopv_lit_reg)(4, SHL, 24, reg);
         VG_(emit_unaryopv_reg)(4, NEG, reg);
         VG_(emit_nonshiftopv_reg_reg)(4, SBB, reg, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFF0000, reg);
         break;
      case Tag_PCast11:
         VG_(emit_shiftopv_lit_reg)(4, SHL, 24, reg);
         VG_(emit_unaryopv_reg)(4, NEG, reg);
         VG_(emit_nonshiftopv_reg_reg)(4, SBB, reg, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFFFF00, reg);
         break;

      /* We use any non-live reg (except %reg) as a temporary,
         or push/pop %ebp if none available:
            (%dead_reg = any dead reg, else %ebp)
            (pushl %ebp if all regs live)
            movl %reg, %dead_reg
            negl %dead_reg
            orl %dead_reg, %reg
            (popl %ebp if all regs live)
         This sequence turns out to be correct regardless of the 
         operation width.
      */
      case Tag_Left4:
      case Tag_Left2:
      case Tag_Left1: {
         UInt dead_reg = R_EBP;
         Int  i, reg_of_i;

         for (i = 0; i < VG_MAX_REALREGS; i++) {
            if (! IS_RREG_LIVE(i, regs_live_after)) {
               reg_of_i = VG_(rankToRealRegNum)(i);
               if (reg != reg_of_i) {
                  dead_reg = reg_of_i;
                  break;
               }
            }
         }

         if (R_EBP == dead_reg)
            VG_(emit_pushv_reg)(4, dead_reg);
         VG_(emit_movv_reg_reg)(4, reg, dead_reg);
         VG_(emit_unaryopv_reg)(4, NEG, dead_reg);
         VG_(emit_nonshiftopv_reg_reg)(4, OR, dead_reg, reg);
         if (R_EBP == dead_reg)
            VG_(emit_popv_reg)(4, dead_reg);
         break;
      }

      /* These are all fairly obvious; do the op and then, if
         necessary, invalidate unused bits. */
      case Tag_SWiden14:
         VG_(emit_shiftopv_lit_reg)(4, SHL, 24, reg);
         VG_(emit_shiftopv_lit_reg)(4, SAR, 24, reg);
         break;
      case Tag_SWiden24:
         VG_(emit_shiftopv_lit_reg)(4, SHL, 16, reg);
         VG_(emit_shiftopv_lit_reg)(4, SAR, 16, reg);
         break;
      case Tag_SWiden12:
         VG_(emit_shiftopv_lit_reg)(4, SHL, 24, reg);
         VG_(emit_shiftopv_lit_reg)(4, SAR, 24, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFF0000, reg);
         break;
      case Tag_ZWiden14:
         VG_(emit_nonshiftopv_lit_reg)(4, AND, 0x000000FF, reg);
         break;
      case Tag_ZWiden24:
         VG_(emit_nonshiftopv_lit_reg)(4, AND, 0x0000FFFF, reg);
         break;
      case Tag_ZWiden12:
         VG_(emit_nonshiftopv_lit_reg)(4, AND, 0x000000FF, reg);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFF0000, reg);
         break;

      default:
         VG_(panic)("synth_TAG1_op");
   }
}


static void synth_TAG2_op ( TagOp op, Int regs, Int regd )
{
   switch (op) {

      /* UifU is implemented by OR, since 1 means Undefined. */
      case Tag_UifU4:
      case Tag_UifU2:
      case Tag_UifU1:
      case Tag_UifU0:
         VG_(emit_nonshiftopv_reg_reg)(4, OR, regs, regd);
         break;

      /* DifD is implemented by AND, since 0 means Defined. */
      case Tag_DifD4:
      case Tag_DifD2:
      case Tag_DifD1:
         VG_(emit_nonshiftopv_reg_reg)(4, AND, regs, regd);
         break;

      /* ImproveAND(value, tags) = value OR tags.
	 Defined (0) value 0s give defined (0); all other -> undefined (1).
         value is in regs; tags is in regd. 
         Be paranoid and invalidate unused bits; I don't know whether 
         or not this is actually necessary. */
      case Tag_ImproveAND4_TQ:
         VG_(emit_nonshiftopv_reg_reg)(4, OR, regs, regd);
         break;
      case Tag_ImproveAND2_TQ:
         VG_(emit_nonshiftopv_reg_reg)(4, OR, regs, regd);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFF0000, regd);
         break;
      case Tag_ImproveAND1_TQ:
         VG_(emit_nonshiftopv_reg_reg)(4, OR, regs, regd);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFFFF00, regd);
         break;

      /* ImproveOR(value, tags) = (not value) OR tags.
	 Defined (0) value 1s give defined (0); all other -> undefined (1).
         value is in regs; tags is in regd. 
         To avoid trashing value, this is implemented (re de Morgan) as
               not (value AND (not tags))
         Be paranoid and invalidate unused bits; I don't know whether 
         or not this is actually necessary. */
      case Tag_ImproveOR4_TQ:
         VG_(emit_unaryopv_reg)(4, NOT, regd);
         VG_(emit_nonshiftopv_reg_reg)(4, AND, regs, regd);
         VG_(emit_unaryopv_reg)(4, NOT, regd);
         break;
      case Tag_ImproveOR2_TQ:
         VG_(emit_unaryopv_reg)(4, NOT, regd);
         VG_(emit_nonshiftopv_reg_reg)(4, AND, regs, regd);
         VG_(emit_unaryopv_reg)(4, NOT, regd);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFF0000, regd);
         break;
      case Tag_ImproveOR1_TQ:
         VG_(emit_unaryopv_reg)(4, NOT, regd);
         VG_(emit_nonshiftopv_reg_reg)(4, AND, regs, regd);
         VG_(emit_unaryopv_reg)(4, NOT, regd);
         VG_(emit_nonshiftopv_lit_reg)(4, OR, 0xFFFFFF00, regd);
         break;

      default:
         VG_(panic)("synth_TAG2_op");
   }
}

/*----------------------------------------------------*/
/*--- Generate code for a single UInstr.           ---*/
/*----------------------------------------------------*/

void SK_(emitExtUInstr) ( UInstr* u, RRegSet regs_live_before )
{
   switch (u->opcode) {

      case SETV:
         vg_assert(u->tag1 == RealReg);
         synth_SETV ( u->size, u->val1 );
         break;

      case STOREV:
         vg_assert(u->tag1 == RealReg || u->tag1 == Literal);
         vg_assert(u->tag2 == RealReg);
         synth_STOREV ( u->size, u->tag1, 
                                 u->tag1==Literal ? u->lit32 : u->val1, 
                                 u->val2,
                        regs_live_before, u->regs_live_after );
         break;

      case LOADV:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         if (0)
            VG_(emit_AMD_prefetch_reg) ( u->val1 );
         synth_LOADV ( u->size, u->val1, u->val2,
                       regs_live_before, u->regs_live_after );
         break;

      case TESTV:
         vg_assert(u->tag1 == RealReg || u->tag1 == ArchReg);
         synth_TESTV(u->size, u->tag1, u->val1);
         break;

      case GETV:
         vg_assert(u->tag1 == ArchReg);
         vg_assert(u->tag2 == RealReg);
         synth_GETV(u->size, u->val1, u->val2);
         break;

      case GETVF:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->size == 0);
         synth_GETVF(u->val1);
         break;

      case PUTV:
         vg_assert(u->tag1 == RealReg || u->tag1 == Literal);
         vg_assert(u->tag2 == ArchReg);
         synth_PUTV(u->size, u->tag1, 
                             u->tag1==Literal ? u->lit32 : u->val1, 
                             u->val2 );
         break;

      case PUTVF:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->size == 0);
         synth_PUTVF(u->val1);
         break;

      case TAG1:
         synth_TAG1_op ( u->val3, u->val1, u->regs_live_after );
         break;

      case TAG2:
         synth_TAG2_op ( u->val3, u->val1, u->val2 );
         break;

      default: 
         VG_(printf)("emitExtUInstr: unhandled extension insn:\n");
         VG_(ppUInstr)(0,u);
         VG_(panic)("emitExtUInstr: unhandled extension opcode");
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                 vg_memcheck_from_ucode.c ---*/
/*--------------------------------------------------------------------*/
