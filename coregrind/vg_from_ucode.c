
/*--------------------------------------------------------------------*/
/*--- The JITter: translate ucode back to x86 code.                ---*/
/*---                                              vg_from_ucode.c ---*/
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
/*--- Instruction emission -- turning final uinstrs back   ---*/
/*--- into x86 code.                                       ---*/
/*------------------------------------------------------------*/

/* [2001-07-08 This comment is now somewhat out of date.]

   This is straightforward but for one thing: to facilitate generating
   code in a single pass, we generate position-independent code.  To
   do this, calls and jmps to fixed addresses must specify the address
   by first loading it into a register, and jump to/call that
   register.  Fortunately, the only jump to a literal is the jump back
   to vg_dispatch, and only %eax is live then, conveniently.  UCode
   call insns may only have a register as target anyway, so there's no
   need to do anything fancy for them.

   The emit_* routines constitute the lowest level of instruction
   emission.  They simply emit the sequence of bytes corresponding to
   the relevant instruction, with no further ado.  In particular there
   is no checking about whether uses of byte registers makes sense,
   nor whether shift insns have their first operand in %cl, etc.

   These issues are taken care of by the level above, the synth_*
   routines.  These detect impossible operand combinations and turn
   them into sequences of legal instructions.  Finally, emitUInstr is
   phrased in terms of the synth_* abstraction layer.  */

/* Static state for the current basic block */
static UChar* emitted_code;
static Int    emitted_code_used;
static Int    emitted_code_size;

/* offset (in bytes into the basic block)  */
static UShort jumps[VG_MAX_JUMPS];
static Int    jumpidx;

static enum eflags_state {
	UPD_Simd,		/* baseblock copy is up to date */
	UPD_Real,		/* CPU copy is up to date */
	UPD_Both,		/* both are current */
} eflags_state;

/* single site for resetting state */
static void reset_state(void)
{
   emitted_code_used = 0;
   emitted_code_size = 500; /* reasonable initial size */
   emitted_code = VG_(arena_malloc)(VG_AR_JITTER, emitted_code_size);
   jumpidx = 0;
   eflags_state = UPD_Simd;
}


/* Statistics about C functions called from generated code. */
static UInt ccalls                 = 0;
static UInt ccall_reg_saves        = 0;
static UInt ccall_args             = 0;
static UInt ccall_arg_setup_instrs = 0;
static UInt ccall_stack_clears     = 0;
static UInt ccall_retvals          = 0;
static UInt ccall_retval_movs      = 0;

/* Statistics about frequency of each UInstr */
typedef
   struct {
      UInt counts;
      UInt size;
   } Histogram;

/* Automatically zeroed because it's static. */
static Histogram histogram[100];     

void VG_(print_ccall_stats)(void)
{
   VG_(message)(Vg_DebugMsg,
                "   ccalls: %u C calls, %u%% saves+restores avoided"
                " (%d bytes)",
                ccalls, 
                100-(UInt)(ccall_reg_saves/(double)(ccalls*3)*100),
                ((ccalls*3) - ccall_reg_saves)*2);
   VG_(message)(Vg_DebugMsg,
                "           %u args, avg 0.%d setup instrs each (%d bytes)", 
                ccall_args, 
               (UInt)(ccall_arg_setup_instrs/(double)ccall_args*100),
               (ccall_args - ccall_arg_setup_instrs)*2);
   VG_(message)(Vg_DebugMsg,
                "           %d%% clear the stack (%d bytes)", 
               (UInt)(ccall_stack_clears/(double)ccalls*100),
               (ccalls - ccall_stack_clears)*3);
   VG_(message)(Vg_DebugMsg,
                "           %u retvals, %u%% of reg-reg movs avoided (%d bytes)",
                ccall_retvals,
                ( ccall_retvals == 0 
                ? 100
                : 100-(UInt)(ccall_retval_movs / 
                             (double)ccall_retvals*100)),
                (ccall_retvals-ccall_retval_movs)*2);
}

void VG_(print_UInstr_histogram)(void)
{
   Int i, j;
   UInt total_counts = 0;
   UInt total_size   = 0;

   for (i = 0; i < 100; i++) {
      total_counts += histogram[i].counts;
      total_size   += histogram[i].size;
   }

   VG_(printf)("-- UInstr frequencies -----------\n");
   for (i = 0; i < 100; i++) {
      if (0 != histogram[i].counts) {

         UInt count_pc = 
            (UInt)(histogram[i].counts/(double)total_counts*100 + 0.5);
         UInt size_pc  = 
            (UInt)(histogram[i].size  /(double)total_size  *100 + 0.5);
         UInt avg_size =
            (UInt)(histogram[i].size / (double)histogram[i].counts + 0.5);

         VG_(printf)("%-7s:%8u (%2u%%), avg %2dB (%2u%%) |", 
                     VG_(name_UOpcode)(True, i), 
                     histogram[i].counts, count_pc, 
                     avg_size, size_pc);

         for (j = 0; j < size_pc; j++) VG_(printf)("O");
         VG_(printf)("\n");

      } else {
         vg_assert(0 == histogram[i].size);
      }
   }

   VG_(printf)("total UInstrs %u, total size %u\n", total_counts, total_size);
}

static void expandEmittedCode ( void )
{
   Int    i;
   UChar *tmp = VG_(arena_malloc)(VG_AR_JITTER, 2 * emitted_code_size);
   /* VG_(printf)("expand to %d\n", 2 * emitted_code_size); */
   for (i = 0; i < emitted_code_size; i++)
      tmp[i] = emitted_code[i];
   VG_(arena_free)(VG_AR_JITTER, emitted_code);
   emitted_code = tmp;
   emitted_code_size *= 2;
}

/* Local calls will be inlined, cross-module ones not */
__inline__ void VG_(emitB) ( UInt b )
{
   if (dis) {
      if (b < 16) VG_(printf)("0%x ", b); else VG_(printf)("%2x ", b);
   }
   if (emitted_code_used == emitted_code_size)
      expandEmittedCode();

   emitted_code[emitted_code_used] = (UChar)b;
   emitted_code_used++;
}

__inline__ void VG_(emitW) ( UInt l )
{
   VG_(emitB) ( (l) & 0x000000FF );
   VG_(emitB) ( (l >> 8) & 0x000000FF );
}

__inline__ void VG_(emitL) ( UInt l )
{
   VG_(emitB) ( (l) & 0x000000FF );
   VG_(emitB) ( (l >> 8) & 0x000000FF );
   VG_(emitB) ( (l >> 16) & 0x000000FF );
   VG_(emitB) ( (l >> 24) & 0x000000FF );
}

static void emit_get_eflags ( void )
{
   Int off = 4 * VGOFF_(m_eflags);
   vg_assert(off >= 0 && off < 128);

   if (dis)
      VG_(printf)("\t       %4d: ", emitted_code_used );

   VG_(emitB) ( 0xFF ); /* PUSHL off(%ebp) */
   VG_(emitB) ( 0x75 );
   VG_(emitB) ( off );
   VG_(emitB) ( 0x9D ); /* POPFL */
   if (dis)
      VG_(printf)( "\n\t\tpushl %d(%%ebp) ; popfl\n", off );
}

static void emit_put_eflags ( void )
{
   Int off = 4 * VGOFF_(m_eflags);
   vg_assert(off >= 0 && off < 128);

   if (dis)
      VG_(printf)("\t       %4d: ", emitted_code_used );

   VG_(emitB) ( 0x9C ); /* PUSHFL */
   VG_(emitB) ( 0x8F ); /* POPL vg_m_state.m_eflags */
   VG_(emitB) ( 0x45 );
   VG_(emitB) ( off );
   if (dis)
      VG_(printf)( "\n\t\tpushfl ; popl %d(%%ebp)\n", off );
}

static void maybe_emit_put_eflags( void )
{
   if (eflags_state == UPD_Real) {
      eflags_state = UPD_Both;
      emit_put_eflags();
   }
}

static void maybe_emit_get_eflags( void )
{
   if (eflags_state == UPD_Simd) {
      eflags_state = UPD_Both;
      emit_get_eflags();
   }
}

/* Call this before emitting each instruction.

   Arguments are:
   upds_simd_flags: 
      if true, this instruction updates the simulated %EFLAGS state,
      otherwise it doesn't
   use_flags: set of (real) flags the instruction uses
   set_flags: set of (real) flags the instruction sets
 */
__inline__ 
void VG_(new_emit) ( Bool upds_simd_flags, 
                     FlagSet use_flags, FlagSet set_flags )
{
   Bool use, set;

   use = use_flags != FlagsEmpty 
         || (set_flags != FlagsEmpty && set_flags != FlagsOSZACP);
   set = set_flags != FlagsEmpty;

   if (0)
      VG_(printf)(
         "new_emit: state=%d upds_simd_flags=%d use_flags=%x set_flags=%x\n",
         eflags_state, upds_simd_flags, use_flags, set_flags);

   if (upds_simd_flags) {
      if (use && eflags_state == UPD_Simd) {
	 /* we need the CPU flags set, but they're not already */
	 eflags_state = UPD_Both;
	 emit_get_eflags();
      }
      if (set) {
	 /* if we're setting the flags, then the CPU will have the
	    only good copy */
	 eflags_state = UPD_Real;
      }
   } else {
      /* presume that if non-simd code is using flags, it knows what
	 it's doing (ie, it just set up the flags). */
      if (set) {
	 /* This instruction is going to trash the flags, so we'd
	    better save them away and say that they're only in the
	    simulated state. */
	 maybe_emit_put_eflags();
	 eflags_state = UPD_Simd;
      }
   }

   if (dis)
      VG_(printf)("\t       %4d: ", emitted_code_used );
}


/*----------------------------------------------------*/
/*--- Addressing modes                             ---*/
/*----------------------------------------------------*/

static __inline__ UChar mkModRegRM ( UChar mod, UChar reg, UChar regmem )
{
   return ((mod & 3) << 6) | ((reg & 7) << 3) | (regmem & 7);
}

static __inline__ UChar mkSIB ( Int scale, Int regindex, Int regbase )
{
   Int shift;
   switch (scale) {
      case 1: shift = 0; break;
      case 2: shift = 1; break;
      case 4: shift = 2; break;
      case 8: shift = 3; break;
      default: VG_(core_panic)( "mkSIB" );
   }
   return ((shift & 3) << 6) | ((regindex & 7) << 3) | (regbase & 7);
}

static __inline__ void emit_amode_litmem_reg ( Addr addr, Int reg )
{
   /* ($ADDR), reg */
   VG_(emitB) ( mkModRegRM(0, reg, 5) );
   VG_(emitL) ( addr );
}

static __inline__ void emit_amode_regmem_reg ( Int regmem, Int reg )
{
   /* (regmem), reg */
   if (regmem == R_ESP) 
      VG_(core_panic)("emit_amode_regmem_reg");
   if (regmem == R_EBP) {
      VG_(emitB) ( mkModRegRM(1, reg, 5) );
      VG_(emitB) ( 0x00 );
   } else {
      VG_(emitB)( mkModRegRM(0, reg, regmem) );
   }
}

void VG_(emit_amode_offregmem_reg) ( Int off, Int regmem, Int reg )
{
   if (regmem == R_ESP)
      VG_(core_panic)("emit_amode_offregmem_reg(ESP)");
   if (off < -128 || off > 127) {
      /* Use a large offset */
      /* d32(regmem), reg */
      VG_(emitB) ( mkModRegRM(2, reg, regmem) );
      VG_(emitL) ( off );
   } else {
      /* d8(regmem), reg */
      VG_(emitB) ( mkModRegRM(1, reg, regmem) );
      VG_(emitB) ( off & 0xFF );
   }
}

static __inline__ void emit_amode_sib_reg ( Int off, Int scale, Int regbase, 
                                            Int regindex, Int reg )
{
   if (regindex == R_ESP)
      VG_(core_panic)("emit_amode_sib_reg(ESP)");
   if (off < -128 || off > 127) {
      /* Use a 32-bit offset */
      VG_(emitB) ( mkModRegRM(2, reg, 4) ); /* SIB with 32-bit displacement */
      VG_(emitB) ( mkSIB( scale, regindex, regbase ) );
      VG_(emitL) ( off );
   } else {
      /* Use an 8-bit offset */
      VG_(emitB) ( mkModRegRM(1, reg, 4) ); /* SIB with 8-bit displacement */
      VG_(emitB) ( mkSIB( scale, regindex, regbase ) );
      VG_(emitB) ( off & 0xFF );
   }
}

void VG_(emit_amode_ereg_greg) ( Int e_reg, Int g_reg )
{
   /* other_reg, reg */
   VG_(emitB) ( mkModRegRM(3, g_reg, e_reg) );
}

static __inline__ void emit_amode_greg_ereg ( Int g_reg, Int e_reg )
{
   /* other_reg, reg */
   VG_(emitB) ( mkModRegRM(3, g_reg, e_reg) );
}


/*----------------------------------------------------*/
/*--- Opcode translation                           ---*/
/*----------------------------------------------------*/

static __inline__ Int mkGrp1opcode ( Opcode opc )
{
   switch (opc) {
      case ADD: return 0;
      case OR:  return 1;
      case ADC: return 2;
      case SBB: return 3;
      case AND: return 4;
      case SUB: return 5;
      case XOR: return 6;
      default: VG_(core_panic)("mkGrp1opcode");
   }
}

static __inline__ FlagSet nonshiftop_use(Opcode opc)
{
   switch(opc) {
   case ADC:
   case SBB:
      return FlagC;

   case ADD:
   case OR:
   case AND:
   case SUB:
   case XOR:
      return FlagsEmpty;

   default:
      VG_(core_panic)("nonshiftop_use");
   }
}

static __inline__ FlagSet nonshiftop_set(Opcode opc)
{
   switch(opc) {
   case ADC:
   case SBB:
   case ADD:
   case OR:
   case AND:
   case SUB:
   case XOR:
      return FlagsOSZACP;

   default:
      VG_(core_panic)("nonshiftop_set");
   }
}

static __inline__ Int mkGrp2opcode ( Opcode opc )
{
   switch (opc) {
      case ROL: return 0;
      case ROR: return 1;
      case RCL: return 2;
      case RCR: return 3;
      case SHL: return 4;
      case SHR: return 5;
      case SAR: return 7;
      default: VG_(core_panic)("mkGrp2opcode");
   }
}

static __inline__ FlagSet shiftop_use(Opcode opc)
{
   switch(opc) {
   case ROR:
   case ROL:
   case SHL:
   case SHR:
   case SAR:
      return FlagsEmpty;

   case RCL:
   case RCR:
      return FlagC;

   default:
      VG_(core_panic)("shiftop_use");
   }
}

static __inline__ FlagSet shiftop_set(Opcode opc)
{
   switch(opc) {
   case ROR:
   case ROL:
   case RCL:
   case RCR:
      return FlagsOC;

   case SHL:
   case SHR:
   case SAR:
      return FlagsOSZACP;

   default:
      VG_(core_panic)("shiftop_set");
   }
}

static __inline__ Int mkGrp3opcode ( Opcode opc )
{
   switch (opc) {
      case NOT: return 2;
      case NEG: return 3;
      default: VG_(core_panic)("mkGrp3opcode");
   }
}

static __inline__ Int mkGrp4opcode ( Opcode opc )
{
   switch (opc) {
      case INC: return 0;
      case DEC: return 1;
      default: VG_(core_panic)("mkGrp4opcode");
   }
}

static __inline__ Int mkGrp5opcode ( Opcode opc )
{
   switch (opc) {
      case CALLM: return 2;
      case JMP:   return 4;
      default: VG_(core_panic)("mkGrp5opcode");
   }
}

static __inline__ UChar mkPrimaryOpcode ( Opcode opc )
{
   switch (opc) {
      case ADD: return 0x00;
      case ADC: return 0x10;
      case AND: return 0x20;
      case XOR: return 0x30;
      case OR:  return 0x08;
      case SBB: return 0x18;
      case SUB: return 0x28;
      default: VG_(core_panic)("mkPrimaryOpcode");
  }
}

/*----------------------------------------------------*/
/*--- v-size (4, or 2 with OSO) insn emitters      ---*/
/*----------------------------------------------------*/

void VG_(emit_movv_offregmem_reg) ( Int sz, Int off, Int areg, Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x8B ); /* MOV Ev, Gv */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t0x%x(%s), %s\n", 
                   nameISize(sz), off, nameIReg(4,areg), nameIReg(sz,reg));
}

void VG_(emit_movv_reg_offregmem) ( Int sz, Int reg, Int off, Int areg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x89 ); /* MOV Gv, Ev */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, 0x%x(%s)\n", 
                   nameISize(sz), nameIReg(sz,reg), off, nameIReg(4,areg));
}

static void emit_movv_regmem_reg ( Int sz, Int reg1, Int reg2 )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x8B ); /* MOV Ev, Gv */
   emit_amode_regmem_reg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t(%s), %s\n",
                   nameISize(sz),  nameIReg(4,reg1), nameIReg(sz,reg2));
}

static void emit_movv_reg_regmem ( Int sz, Int reg1, Int reg2 )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x89 ); /* MOV Gv, Ev */
   emit_amode_regmem_reg ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, (%s)\n", 
                   nameISize(sz), nameIReg(sz,reg1), nameIReg(4,reg2));
}

void VG_(emit_movv_reg_reg) ( Int sz, Int reg1, Int reg2 )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x89 ); /* MOV Gv, Ev */
   VG_(emit_amode_ereg_greg) ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, %s\n", 
                   nameISize(sz), nameIReg(sz,reg1), nameIReg(sz,reg2));
}

void VG_(emit_nonshiftopv_lit_reg) ( Bool upd_cc, Int sz, Opcode opc, UInt lit, Int reg )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));

   if (sz == 2) VG_(emitB) ( 0x66 );
   if (lit == VG_(extend_s_8to32)(lit & 0x000000FF)) {
      /* short form OK */
      VG_(emitB) ( 0x83 ); /* Grp1 Ib,Ev */
      VG_(emit_amode_ereg_greg) ( reg, mkGrp1opcode(opc) );
      VG_(emitB) ( lit & 0x000000FF );
   } else {
      VG_(emitB) ( 0x81 ); /* Grp1 Iv,Ev */
      VG_(emit_amode_ereg_greg) ( reg, mkGrp1opcode(opc) );
      if (sz == 2) VG_(emitW) ( lit ); else VG_(emitL) ( lit );
   }
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t$0x%x, %s\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz), 
                   lit, nameIReg(sz,reg));
}

void VG_(emit_nonshiftopv_lit_offregmem) ( Bool upd_cc, Int sz, Opcode opc, UInt lit, 
					   Int off, Int regmem )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));
   if (sz == 2) VG_(emitB) ( 0x66 );
   if (lit == VG_(extend_s_8to32)(lit & 0x000000FF)) {
      /* short form OK */
      VG_(emitB) ( 0x83 ); /* Grp1 Ib,Ev */
      VG_(emit_amode_offregmem_reg) ( off, regmem, mkGrp1opcode(opc) );
      VG_(emitB) ( lit & 0x000000FF );
   } else {
      VG_(emitB) ( 0x81 ); /* Grp1 Iv,Ev */
      VG_(emit_amode_offregmem_reg) ( off, regmem, mkGrp1opcode(opc) );
      if (sz == 2) VG_(emitW) ( lit ); else VG_(emitL) ( lit );
   }
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t$0x%x, 0x%x(%s)\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz), 
                   lit, off, nameIReg(sz,regmem));
}

void VG_(emit_shiftopv_lit_reg) ( Bool upd_cc, Int sz, Opcode opc, UInt lit, Int reg )
{
   VG_(new_emit)(upd_cc, shiftop_use(opc), shiftop_set(opc));

   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0xC1 ); /* Grp2 Ib,Ev */
   VG_(emit_amode_ereg_greg) ( reg, mkGrp2opcode(opc) );
   VG_(emitB) ( lit );
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t$%d, %s\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz), 
                   lit, nameIReg(sz,reg));
}

static void emit_shiftopv_cl_stack0 ( Bool upd_cc, Int sz, Opcode opc )
{
   VG_(new_emit)(upd_cc, shiftop_use(opc), shiftop_set(opc));
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0xD3 ); /* Grp2 CL,Ev */
   VG_(emitB) ( mkModRegRM ( 1, mkGrp2opcode(opc), 4 ) );
   VG_(emitB) ( 0x24 ); /* a SIB, I think `d8(%esp)' */
   VG_(emitB) ( 0x00 ); /* the d8 displacement */
   if (dis)
      VG_(printf)("\n\t\t%s%c %%cl, 0(%%esp)\n",
                  VG_(name_UOpcode)(False,opc), nameISize(sz) );
}

static void emit_shiftopb_cl_stack0 ( Bool upd_cc, Opcode opc )
{
   VG_(new_emit)(upd_cc, shiftop_use(opc), shiftop_set(opc));
   VG_(emitB) ( 0xD2 ); /* Grp2 CL,Eb */
   VG_(emitB) ( mkModRegRM ( 1, mkGrp2opcode(opc), 4 ) );
   VG_(emitB) ( 0x24 ); /* a SIB, I think `d8(%esp)' */
   VG_(emitB) ( 0x00 ); /* the d8 displacement */
   if (dis)
      VG_(printf)("\n\t\t%s%c %%cl, 0(%%esp)\n",
                  VG_(name_UOpcode)(False,opc), nameISize(1) );
}

static void emit_nonshiftopv_offregmem_reg ( Bool upd_cc, Int sz, Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 3 + mkPrimaryOpcode(opc) ); /* op Ev, Gv */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t0x%x(%s), %s\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz),
                   off, nameIReg(4,areg), nameIReg(sz,reg));
}

static void emit_nonshiftopv_reg_offregmem ( Bool upd_cc, Int sz, Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 1 + mkPrimaryOpcode(opc) ); /* op Gv, Ev */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t0x%s, %x(%s),\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz),
                   nameIReg(sz,reg), off, nameIReg(4,areg));
}

void VG_(emit_nonshiftopv_reg_reg) ( Bool upd_cc, Int sz, Opcode opc, 
				     Int reg1, Int reg2 )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));
   if (sz == 2) VG_(emitB) ( 0x66 );
#  if 0
   /* Perfectly correct, but the GNU assembler uses the other form.
      Therefore we too use the other form, to aid verification. */
   VG_(emitB) ( 3 + mkPrimaryOpcode(opc) ); /* op Ev, Gv */
   VG_(emit_amode_ereg_greg) ( reg1, reg2 );
#  else
   VG_(emitB) ( 1 + mkPrimaryOpcode(opc) ); /* op Gv, Ev */
   emit_amode_greg_ereg ( reg1, reg2 );
#  endif
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t%s, %s\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz), 
                   nameIReg(sz,reg1), nameIReg(sz,reg2));
}

void VG_(emit_movv_lit_reg) ( Int sz, UInt lit, Int reg )
{
   if (lit == 0 && eflags_state != UPD_Real) {
      /* Only emit this for zeroing if it won't stomp flags */
      VG_(emit_nonshiftopv_reg_reg) ( False, sz, XOR, reg, reg );
      return;
   }
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0xB8+reg ); /* MOV imm, Gv */
   if (sz == 2) VG_(emitW) ( lit ); else VG_(emitL) ( lit );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t$0x%x, %s\n", 
                   nameISize(sz), lit, nameIReg(sz,reg));
}

void VG_(emit_unaryopv_reg) ( Bool upd_cc, Int sz, Opcode opc, Int reg )
{
   switch (opc) {
      case NEG:
	 VG_(new_emit)(upd_cc, FlagsEmpty, FlagsOSZACP);
	 if (sz == 2) VG_(emitB) ( 0x66 );
         VG_(emitB) ( 0xF7 );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp3opcode(NEG) );
         if (dis)
            VG_(printf)( "\n\t\tneg%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case NOT:
	 VG_(new_emit)(upd_cc, FlagsEmpty, FlagsEmpty);
	 if (sz == 2) VG_(emitB) ( 0x66 );
         VG_(emitB) ( 0xF7 );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp3opcode(NOT) );
         if (dis)
            VG_(printf)( "\n\t\tnot%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case DEC:
	 VG_(new_emit)(upd_cc, FlagsEmpty, FlagsOSZAP);
	 if (sz == 2) VG_(emitB) ( 0x66 );
         VG_(emitB) ( 0x48 + reg );
         if (dis)
            VG_(printf)( "\n\t\tdec%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case INC:
	 VG_(new_emit)(upd_cc, FlagsEmpty, FlagsOSZAP);
	 if (sz == 2) VG_(emitB) ( 0x66 );
         VG_(emitB) ( 0x40 + reg );
         if (dis)
            VG_(printf)( "\n\t\tinc%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      default: 
         VG_(core_panic)("VG_(emit_unaryopv_reg)");
   }
}

void VG_(emit_pushv_reg) ( Int sz, Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) {
      VG_(emitB) ( 0x66 ); 
   } else {
      vg_assert(sz == 4);
   }
   VG_(emitB) ( 0x50 + reg );
   if (dis)
      VG_(printf)("\n\t\tpush%c %s\n", nameISize(sz), nameIReg(sz,reg));
}

void VG_(emit_popv_reg) ( Int sz, Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) {
      VG_(emitB) ( 0x66 ); 
   } else {
      vg_assert(sz == 4);
   }
   VG_(emitB) ( 0x58 + reg );
   if (dis)
      VG_(printf)("\n\t\tpop%c %s\n", nameISize(sz), nameIReg(sz,reg));
}

void VG_(emit_pushl_lit32) ( UInt int32 )
{  
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x68 );
   VG_(emitL) ( int32 );
   if (dis)
      VG_(printf)("\n\t\tpushl $0x%x\n", int32 );
}  

void VG_(emit_pushl_lit8) ( Int lit8 )
{
   vg_assert(lit8 >= -128 && lit8 < 128);
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x6A );
   VG_(emitB) ( (UChar)((UInt)lit8) );
   if (dis)
      VG_(printf)("\n\t\tpushl $%d\n", lit8 );
}

void VG_(emit_cmpl_zero_reg) ( Bool upd_cc, Int reg )
{
   VG_(new_emit)(upd_cc, False, FlagsOSZACP);
   VG_(emitB) ( 0x83 );
   VG_(emit_amode_ereg_greg) ( reg, 7 /* Grp 3 opcode for CMP */ );
   VG_(emitB) ( 0x00 );
   if (dis)
      VG_(printf)("\n\t\tcmpl $0, %s\n", nameIReg(4,reg));
}

static void emit_swapl_reg_ECX ( Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x87 ); /* XCHG Gv,Ev */
   VG_(emit_amode_ereg_greg) ( reg, R_ECX );
   if (dis) 
      VG_(printf)("\n\t\txchgl %%ecx, %s\n", nameIReg(4,reg));
}

void VG_(emit_swapl_reg_EAX) ( Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x90 + reg ); /* XCHG Gv,eAX */
   if (dis) 
      VG_(printf)("\n\t\txchgl %%eax, %s\n", nameIReg(4,reg));
}

static void emit_swapl_reg_reg ( Int reg1, Int reg2 )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x87 ); /* XCHG Gv,Ev */
   VG_(emit_amode_ereg_greg) ( reg1, reg2 );
   if (dis) 
      VG_(printf)("\n\t\txchgl %s, %s\n", nameIReg(4,reg1), 
                  nameIReg(4,reg2));
}

static void emit_bswapl_reg ( Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( 0xC8 + reg ); /* BSWAP r32 */
   if (dis) 
      VG_(printf)("\n\t\tbswapl %s\n", nameIReg(4,reg));
}

static void emit_movl_reg_reg ( Int regs, Int regd )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x89 ); /* MOV Gv,Ev */
   VG_(emit_amode_ereg_greg) ( regd, regs );
   if (dis) 
      VG_(printf)("\n\t\tmovl %s, %s\n", nameIReg(4,regs), nameIReg(4,regd));
}

void VG_(emit_movv_lit_offregmem) ( Int sz, UInt lit, Int off, Int memreg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   if (sz == 2) {
      VG_(emitB) ( 0x66 );
   } else {
      vg_assert(sz == 4);
   }
   VG_(emitB) ( 0xC7 ); /* Grp11 Ev */
   VG_(emit_amode_offregmem_reg) ( off, memreg, 0 /* Grp11 subopcode for MOV */ );
   if (sz == 2) VG_(emitW) ( lit ); else VG_(emitL) ( lit );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t$0x%x, 0x%x(%s)\n", 
                   nameISize(sz), lit, off, nameIReg(4,memreg) );
}


/*----------------------------------------------------*/
/*--- b-size (1 byte) instruction emitters         ---*/
/*----------------------------------------------------*/

/* There is some doubt as to whether C6 (Grp 11) is in the
   486 insn set.  ToDo: investigate. */
void VG_(emit_movb_lit_offregmem) ( UInt lit, Int off, Int memreg )
{                                     
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0xC6 ); /* Grp11 Eb */
   VG_(emit_amode_offregmem_reg) ( off, memreg, 0 /* Grp11 subopcode for MOV */ );
   VG_(emitB) ( lit ); 
   if (dis)
      VG_(printf)( "\n\t\tmovb\t$0x%x, 0x%x(%s)\n", 
                   lit, off, nameIReg(4,memreg) );
}              
              
static void emit_nonshiftopb_offregmem_reg ( Bool upd_cc, Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   VG_(new_emit)(upd_cc, (opc == ADC || opc == SBB) ? FlagC : FlagsEmpty, True);
   VG_(emitB) ( 2 + mkPrimaryOpcode(opc) ); /* op Eb, Gb */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t0x%x(%s), %s\n", 
                   VG_(name_UOpcode)(False,opc), off, nameIReg(4,areg), 
                   nameIReg(1,reg));
}

static void emit_nonshiftopb_lit_offregmem ( Bool upd_cc, Opcode opc, 
                                             UInt lit, Int off, Int areg )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 0x80 );
   VG_(emit_amode_offregmem_reg) ( off, areg, mkGrp1opcode(opc) );
   VG_(emitB) ( lit );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t$0x%x, 0x%x(%s)\n", 
                   VG_(name_UOpcode)(False,opc), lit, off, nameIReg(4,areg));
}

static void emit_nonshiftopb_reg_offregmem ( Bool upd_cc, Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 0 + mkPrimaryOpcode(opc) ); /* op Gb, Eb */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t0x%s , %x(%s)\n", 
                   VG_(name_UOpcode)(False,opc), 
		   nameIReg(1,reg),
		   off, nameIReg(4,areg));
}

void VG_(emit_movb_reg_offregmem) ( Int reg, Int off, Int areg )
{
   /* Could do better when reg == %al. */
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x88 ); /* MOV G1, E1 */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovb\t%s, 0x%x(%s)\n", 
                   nameIReg(1,reg), off, nameIReg(4,areg));
}

static void emit_nonshiftopb_reg_reg ( Bool upd_cc, Opcode opc, Int reg1, Int reg2 )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 2 + mkPrimaryOpcode(opc) ); /* op Eb, Gb */
   VG_(emit_amode_ereg_greg) ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t%s, %s\n", 
                   VG_(name_UOpcode)(False,opc),
                   nameIReg(1,reg1), nameIReg(1,reg2));
}

static void emit_movb_reg_regmem ( Int reg1, Int reg2 )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x88 ); /* MOV G1, E1 */
   emit_amode_regmem_reg ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmovb\t%s, (%s)\n", nameIReg(1,reg1), 
                                             nameIReg(4,reg2));
}

static void emit_nonshiftopb_lit_reg ( Bool upd_cc, Opcode opc, UInt lit, Int reg )
{
   VG_(new_emit)(upd_cc, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 0x80 ); /* Grp1 Ib,Eb */
   VG_(emit_amode_ereg_greg) ( reg, mkGrp1opcode(opc) );
   VG_(emitB) ( lit & 0x000000FF );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t$0x%x, %s\n", VG_(name_UOpcode)(False,opc),
                                             lit, nameIReg(1,reg));
}

static void emit_shiftopb_lit_reg ( Bool upd_cc, Opcode opc, UInt lit, Int reg )
{
   VG_(new_emit)(upd_cc, shiftop_use(opc), shiftop_set(opc));
   VG_(emitB) ( 0xC0 ); /* Grp2 Ib,Eb */
   VG_(emit_amode_ereg_greg) ( reg, mkGrp2opcode(opc) );
   VG_(emitB) ( lit );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t$%d, %s\n", 
                   VG_(name_UOpcode)(False,opc),
                   lit, nameIReg(1,reg));
}

void VG_(emit_unaryopb_reg) ( Bool upd_cc, Opcode opc, Int reg )
{
   switch (opc) {
      case INC:
	 VG_(new_emit)(upd_cc, FlagsEmpty, FlagsOSZAP);
         VG_(emitB) ( 0xFE );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp4opcode(INC) );
         if (dis)
            VG_(printf)( "\n\t\tincb\t%s\n", nameIReg(1,reg));
         break;
      case DEC:
	 VG_(new_emit)(upd_cc, FlagsEmpty, FlagsOSZAP);
         VG_(emitB) ( 0xFE );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp4opcode(DEC) );
         if (dis)
            VG_(printf)( "\n\t\tdecb\t%s\n", nameIReg(1,reg));
         break;
      case NOT:
	 VG_(new_emit)(upd_cc, FlagsEmpty, FlagsEmpty);
         VG_(emitB) ( 0xF6 );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp3opcode(NOT) );
         if (dis)
            VG_(printf)( "\n\t\tnotb\t%s\n", nameIReg(1,reg));
         break;
      case NEG:
	 VG_(new_emit)(upd_cc, FlagsEmpty, FlagsOSZACP);
         VG_(emitB) ( 0xF6 );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp3opcode(NEG) );
         if (dis)
            VG_(printf)( "\n\t\tnegb\t%s\n", nameIReg(1,reg));
         break;
      default: 
         VG_(core_panic)("VG_(emit_unaryopb_reg)");
   }
}

void VG_(emit_testb_lit_reg) ( Bool upd_cc, UInt lit, Int reg )
{
   VG_(new_emit)(upd_cc, FlagsEmpty, FlagsOSZACP);
   VG_(emitB) ( 0xF6 ); /* Grp3 Eb */
   VG_(emit_amode_ereg_greg) ( reg, 0 /* Grp3 subopcode for TEST */ );
   VG_(emitB) ( lit );
   if (dis)
      VG_(printf)("\n\t\ttestb $0x%x, %s\n", lit, nameIReg(1,reg));
}

/*----------------------------------------------------*/
/*--- zero-extended load emitters                  ---*/
/*----------------------------------------------------*/

void VG_(emit_movzbl_offregmem_reg) ( Int off, Int regmem, Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0xB6 ); /* MOVZBL */
   VG_(emit_amode_offregmem_reg) ( off, regmem, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovzbl\t0x%x(%s), %s\n", 
                   off, nameIReg(4,regmem), nameIReg(4,reg));
}

static void emit_movzbl_regmem_reg ( Int reg1, Int reg2 )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0xB6 ); /* MOVZBL */
   emit_amode_regmem_reg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\tmovzbl\t(%s), %s\n", nameIReg(4,reg1), 
                                               nameIReg(4,reg2));
}

void VG_(emit_movzwl_offregmem_reg) ( Int off, Int areg, Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0xB7 ); /* MOVZWL */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovzwl\t0x%x(%s), %s\n",
                   off, nameIReg(4,areg), nameIReg(4,reg));
}

static void emit_movzwl_regmem_reg ( Int reg1, Int reg2 )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0xB7 ); /* MOVZWL */
   emit_amode_regmem_reg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\tmovzwl\t(%s), %s\n", nameIReg(4,reg1), 
                                             nameIReg(4,reg2));
}

/*----------------------------------------------------*/
/*--- FPU instruction emitters                     ---*/
/*----------------------------------------------------*/

static void emit_get_fpu_state ( void )
{
   Int off = 4 * VGOFF_(m_fpustate);
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0xDD ); VG_(emitB) ( 0xA5 ); /* frstor d32(%ebp) */
   VG_(emitL) ( off );
   if (dis)
      VG_(printf)("\n\t\tfrstor\t%d(%%ebp)\n", off );
}

static void emit_put_fpu_state ( void )
{
   Int off = 4 * VGOFF_(m_fpustate);
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0xDD ); VG_(emitB) ( 0xB5 ); /* fnsave d32(%ebp) */
   VG_(emitL) ( off );
   if (dis)
      VG_(printf)("\n\t\tfnsave\t%d(%%ebp)\n", off );
}

static void emit_fpu_no_mem ( FlagSet uses_flags, FlagSet sets_flags,
			      UChar first_byte, 
                              UChar second_byte )
{
   VG_(new_emit)(True, uses_flags, sets_flags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   if (dis)
      VG_(printf)("\n\t\tfpu-0x%x:0x%x\n", 
                  (UInt)first_byte, (UInt)second_byte );
}

static void emit_fpu_regmem ( FlagSet uses_flags, FlagSet sets_flags,
			      UChar first_byte, 
                              UChar second_byte_masked, 
                              Int reg )
{
   VG_(new_emit)(True, uses_flags, sets_flags);
   VG_(emitB) ( first_byte );
   emit_amode_regmem_reg ( reg, second_byte_masked >> 3 );
   if (dis)
      VG_(printf)("\n\t\tfpu-0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte_masked,
                  nameIReg(4,reg) );
}


/*----------------------------------------------------*/
/*--- misc instruction emitters                    ---*/
/*----------------------------------------------------*/

void VG_(emit_call_reg) ( Int reg )
{           
   VG_(new_emit)(False, FlagsEmpty, FlagsOSZACP); /* XXX */
   VG_(emitB) ( 0xFF ); /* Grp5 */
   VG_(emit_amode_ereg_greg) ( reg, mkGrp5opcode(CALLM) );
   if (dis) 
      VG_(printf)( "\n\t\tcall\t*%s\n", nameIReg(4,reg) );
}              
         
static void emit_call_star_EBP_off ( Bool upd_cc, Int byte_off, FlagSet use_flag, FlagSet set_flag )
{
   /* Used for helpers which expect to see Simd flags in Real flags */
   VG_(new_emit)(upd_cc, use_flag, set_flag);

   if (byte_off < -128 || byte_off > 127) {
      VG_(emitB) ( 0xFF );
      VG_(emitB) ( 0x95 );
      VG_(emitL) ( byte_off );
   } else {
      VG_(emitB) ( 0xFF );
      VG_(emitB) ( 0x55 );
      VG_(emitB) ( byte_off );
   }
   if (dis)
      VG_(printf)( "\n\t\tcall * %d(%%ebp)\n", byte_off );
}


static void emit_addlit8_offregmem ( Int lit8, Int regmem, Int off )
{
   vg_assert(lit8 >= -128 && lit8 < 128);
   VG_(new_emit)(True, FlagsEmpty, FlagsOSZACP);
   VG_(emitB) ( 0x83 ); /* Grp1 Ib,Ev */
   VG_(emit_amode_offregmem_reg) ( off, regmem, 
                              0 /* Grp1 subopcode for ADD */ );
   VG_(emitB) ( lit8 & 0xFF );
   if (dis)
      VG_(printf)( "\n\t\taddl $%d, %d(%s)\n", lit8, off, 
                                               nameIReg(4,regmem));
}


void VG_(emit_add_lit_to_esp) ( Int lit )
{
   if (lit < -128 || lit > 127) VG_(core_panic)("VG_(emit_add_lit_to_esp)");
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x8D );
   VG_(emitB) ( 0x64 );
   VG_(emitB) ( 0x24 );
   VG_(emitB) ( lit & 0xFF );
   if (dis)
      VG_(printf)( "\n\t\tlea\t%d(%%esp), %%esp\n", lit );
}


static void emit_movb_AL_zeroESPmem ( void )
{
   /* movb %al, 0(%esp) */
   /* 88442400              movb    %al, 0(%esp) */
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x88 );
   VG_(emitB) ( 0x44 );
   VG_(emitB) ( 0x24 );
   VG_(emitB) ( 0x00 );
   if (dis)
      VG_(printf)( "\n\t\tmovb %%al, 0(%%esp)\n" );
}

static void emit_movb_zeroESPmem_AL ( void )
{
   /* movb 0(%esp), %al */
   /* 8A442400              movb    0(%esp), %al */
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x8A );
   VG_(emitB) ( 0x44 );
   VG_(emitB) ( 0x24 );
   VG_(emitB) ( 0x00 );
   if (dis)
      VG_(printf)( "\n\t\tmovb 0(%%esp), %%al\n" );
}


/* Emit a jump short with an 8-bit signed offset.  Note that the
   offset is that which should be added to %eip once %eip has been
   advanced over this insn.  */
void VG_(emit_jcondshort_delta) ( Bool simd, Condcode cond, Int delta )
{
   vg_assert(delta >= -128 && delta <= 127);
   VG_(new_emit)(simd, FlagsOSZCP, False);
   VG_(emitB) ( 0x70 + (UInt)cond );
   VG_(emitB) ( (UChar)delta );
   if (dis)
      VG_(printf)( "\n\t\tj%s-8\t%%eip+%d\n", 
                   VG_(nameCondcode)(cond), delta );
}

static void emit_setb_reg ( Int reg, Condcode cond )
{
   VG_(new_emit)(True, FlagsOSZCP, False);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0x90 + (UChar)cond );
   VG_(emit_amode_ereg_greg) ( reg, 0 );
   if (dis)
      VG_(printf)("\n\t\tset%s %s\n", 
                  VG_(nameCondcode)(cond), nameIReg(1,reg));
}

static void emit_ret ( void )
{
   maybe_emit_put_eflags(); /* make sure flags are stored */
   VG_(new_emit)(False, False, False);
   VG_(emitB) ( 0xC3 ); /* RET */
   if (dis)
      VG_(printf)("\n\t\tret\n");
}

/* Predicate used in sanity checks elsewhere - returns true if any
   jump-site is an actual chained jump */
Bool VG_(is_chained_jumpsite)(Addr a)
{
   UChar *cp = (UChar *)a;

   return (*cp == 0xE9);		/* 0xE9 -- jmp */
}

static 
Bool is_fresh_jumpsite(UChar *cp)
{
   return
      cp[0] == 0x0F &&		/* UD2 */
      cp[1] == 0x0B &&
      cp[2] == 0x0F &&		/* UD2 */
      cp[3] == 0x0B &&
      cp[4] == 0x90;		/* NOP */
}

/* Predicate used in sanity checks elsewhere - returns true if all
   jump-sites are calls to VG_(patch_me) */
Bool VG_(is_unchained_jumpsite)(Addr a)
{
   UChar *cp = (UChar *)a;
   Int delta = ((Addr)&VG_(patch_me)) - (a + VG_PATCHME_CALLSZ);
   Int idelta;

   if (*cp++ != 0xE8)	/* 0xE8 == call */
      return False;

   idelta  = (*cp++) <<  0;
   idelta |= (*cp++) <<  8;
   idelta |= (*cp++) << 16;
   idelta |= (*cp++) << 24;
      
   return idelta == delta;
}

/* Return target address for a direct jmp */
Addr VG_(get_jmp_dest)(Addr a)
{
   Int delta;
   UChar *cp = (UChar *)a;

   if (*cp++ != 0xE9)	/* 0xE9 == jmp */
      return 0;

   delta  = (*cp++) <<  0;
   delta |= (*cp++) <<  8;
   delta |= (*cp++) << 16;
   delta |= (*cp++) << 24;

   return a + VG_PATCHME_JMPSZ + delta;
}

/* unchain a BB by generating a call to VG_(patch_me) */
void VG_(unchain_jumpsite)(Addr a)
{
   Int delta = ((Addr)&VG_(patch_me)) - (a + VG_PATCHME_CALLSZ);
   UChar *cp = (UChar *)a;

   if (VG_(is_unchained_jumpsite)(a))
      return;			/* don't write unnecessarily */

   if (!is_fresh_jumpsite(cp))
      VG_(bb_dechain_count)++;     /* update stats */

   *cp++ = 0xE8;		/* call */
   *cp++ = (delta >>  0) & 0xff;
   *cp++ = (delta >>  8) & 0xff;
   *cp++ = (delta >> 16) & 0xff;
   *cp++ = (delta >> 24) & 0xff;
}

/* This doesn't actually generate a call to VG_(patch_me), but
   reserves enough space in the instruction stream for it to happen
   and records the offset into the jump table.  This is because call
   is a relative jump, and so will be affected when this code gets
   moved about.  The translation table will "unchain" this basic block
   on insertion (with VG_(unchain_BB)()), and thereby generate a
   proper call instruction. */
static void emit_call_patchme( void )
{
   vg_assert(VG_PATCHME_CALLSZ == 5);

   maybe_emit_put_eflags();		/* save flags before end of BB */
   VG_(new_emit)(False, False, False);

   if (jumpidx >= VG_MAX_JUMPS) {
      /* If there too many jumps in this basic block, fall back to
	 dispatch loop.  We still need to keep it the same size as the
	 call sequence. */
      VG_(emitB) ( 0xC3 );	/* ret */
      VG_(emitB) ( 0x90 );	/* nop */
      VG_(emitB) ( 0x90 );	/* nop */
      VG_(emitB) ( 0x90 );	/* nop */
      VG_(emitB) ( 0x90 );	/* nop */

      if (dis)
	 VG_(printf)("\n\t\tret; nop; nop; nop; nop\n");

      if (0 && VG_(clo_verbosity))
	 VG_(message)(Vg_DebugMsg, "too many chained jumps in basic-block");
   } else {
      jumps[jumpidx++] = emitted_code_used;
      
      VG_(emitB) ( 0x0F );		/* UD2 - undefined instruction */
      VG_(emitB) ( 0x0B );
      VG_(emitB) ( 0x0F );		/* UD2 - undefined instruction */
      VG_(emitB) ( 0x0B );
      VG_(emitB) ( 0x90 );		/* NOP */

      if (dis)
	 VG_(printf)("\n\t\tud2; ud2; nop\n");
   }
}   

void VG_(emit_pushal) ( void )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x60 ); /* PUSHAL */
   if (dis)
      VG_(printf)("\n\t\tpushal\n");
}

void VG_(emit_popal) ( void )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x61 ); /* POPAL */
   if (dis)
      VG_(printf)("\n\t\tpopal\n");
}

static void emit_lea_litreg_reg ( UInt lit, Int regmem, Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x8D ); /* LEA M,Gv */
   VG_(emit_amode_offregmem_reg) ( (Int)lit, regmem, reg );
   if (dis)
      VG_(printf)("\n\t\tleal 0x%x(%s), %s\n",
                  lit, nameIReg(4,regmem), nameIReg(4,reg) );
}

static void emit_lea_sib_reg ( UInt lit, Int scale,
			       Int regbase, Int regindex, Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x8D ); /* LEA M,Gv */
   emit_amode_sib_reg ( (Int)lit, scale, regbase, regindex, reg );
   if (dis)
      VG_(printf)("\n\t\tleal 0x%x(%s,%s,%d), %s\n",
                  lit, nameIReg(4,regbase), 
                       nameIReg(4,regindex), scale,
                       nameIReg(4,reg) );
}

void VG_(emit_AMD_prefetch_reg) ( Int reg )
{
   VG_(new_emit)(True, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( 0x0D );
   emit_amode_regmem_reg ( reg, 1 /* 0 is prefetch; 1 is prefetchw */ );
   if (dis)
      VG_(printf)("\n\t\tamd-prefetch (%s)\n", nameIReg(4,reg) );
}

/*----------------------------------------------------*/
/*--- Helper offset -> addr translation            ---*/
/*----------------------------------------------------*/

/* Finds the baseBlock offset of a skin-specified helper.
 * Searches through compacts first, then non-compacts. */
Int VG_(helper_offset)(Addr a)
{
   Int i;

   for (i = 0; i < VG_(n_compact_helpers); i++)
      if (VG_(compact_helper_addrs)[i] == a)
         return VG_(compact_helper_offsets)[i];
   for (i = 0; i < VG_(n_noncompact_helpers); i++)
      if (VG_(noncompact_helper_addrs)[i] == a)
         return VG_(noncompact_helper_offsets)[i];

   /* Shouldn't get here */
   VG_(printf)(
      "\nCouldn't find offset of helper from its address (%p).\n"
      "A helper function probably used hasn't been registered?\n\n", a);

   VG_(printf)("      compact helpers: ");
   for (i = 0; i < VG_(n_compact_helpers); i++)
      VG_(printf)("%p ", VG_(compact_helper_addrs)[i]);

   VG_(printf)("\n  non-compact helpers: ");
   for (i = 0; i < VG_(n_noncompact_helpers); i++)
      VG_(printf)("%p ", VG_(noncompact_helper_addrs)[i]);

   VG_(printf)("\n");
   VG_(skin_panic)("Unfound helper");
}

/*----------------------------------------------------*/
/*--- Instruction synthesisers                     ---*/
/*----------------------------------------------------*/

static Condcode invertCondition ( Condcode cond )
{
   return (Condcode)(1 ^ (UInt)cond);
}


/* Synthesise a call to *baseBlock[offset], ie,
   call * (4 x offset)(%ebp).
*/
void VG_(synth_call) ( Bool ensure_shortform, Int word_offset, 
		       Bool upd_cc, FlagSet use_flags, FlagSet set_flags )
{
   vg_assert(word_offset >= 0);
   vg_assert(word_offset < VG_BASEBLOCK_WORDS);
   if (ensure_shortform) {
      vg_assert(word_offset < 32);
   }
   emit_call_star_EBP_off ( upd_cc, 4 * word_offset, use_flags, set_flags );
}

static void maybe_emit_movl_reg_reg ( UInt src, UInt dst )
{
   if (src != dst) {
      VG_(emit_movv_reg_reg) ( 4, src, dst );
      ccall_arg_setup_instrs++;
   }
}

/* 'maybe' because it is sometimes skipped eg. for "movl %eax,%eax" */
static void maybe_emit_movl_litOrReg_reg ( UInt litOrReg, Tag tag, UInt reg )
{
   if (RealReg == tag) {
      maybe_emit_movl_reg_reg ( litOrReg, reg );
   } else if (Literal == tag) {
      VG_(emit_movv_lit_reg) ( 4, litOrReg, reg );
      ccall_arg_setup_instrs++;
   }
   else
      VG_(core_panic)("emit_movl_litOrReg_reg: unexpected tag");
}

static
void emit_swapl_arg_regs ( UInt reg1, UInt reg2 )
{
   if        (R_EAX == reg1) {
      VG_(emit_swapl_reg_EAX) ( reg2 );
   } else if (R_EAX == reg2) {
      VG_(emit_swapl_reg_EAX) ( reg1 );
   } else {
      emit_swapl_reg_reg ( reg1, reg2 );
   }
   ccall_arg_setup_instrs++;
}

static
void emit_two_regs_args_setup ( UInt src1, UInt src2, UInt dst1, UInt dst2)
{
   if        (dst1 != src2) {
      maybe_emit_movl_reg_reg ( src1, dst1 );
      maybe_emit_movl_reg_reg ( src2, dst2 );

   } else if (dst2 != src1) {
      maybe_emit_movl_reg_reg ( src2, dst2 );
      maybe_emit_movl_reg_reg ( src1, dst1 );

   } else {
      /* swap to break cycle */
      emit_swapl_arg_regs ( dst1, dst2 );
   }
}

static
void emit_three_regs_args_setup ( UInt src1, UInt src2, UInt src3,
                                  UInt dst1, UInt dst2, UInt dst3)
{
   if        (dst1 != src2 && dst1 != src3) {
      maybe_emit_movl_reg_reg ( src1, dst1 );
      emit_two_regs_args_setup ( src2, src3, dst2, dst3 );

   } else if (dst2 != src1 && dst2 != src3) {
      maybe_emit_movl_reg_reg ( src2, dst2 );
      emit_two_regs_args_setup ( src1, src3, dst1, dst3 );

   } else if (dst3 != src1 && dst3 != src2) {
      maybe_emit_movl_reg_reg ( src3, dst3 );
      emit_two_regs_args_setup ( src1, src2, dst1, dst2 );
      
   } else {
      /* break cycle */
      if        (dst1 == src2 && dst2 == src3 && dst3 == src1) {
         emit_swapl_arg_regs ( dst1, dst2 );
         emit_swapl_arg_regs ( dst1, dst3 );

      } else if (dst1 == src3 && dst2 == src1 && dst3 == src2) {
         emit_swapl_arg_regs ( dst1, dst3 );
         emit_swapl_arg_regs ( dst1, dst2 );

      } else {
         VG_(core_panic)("impossible 3-cycle");
      }
   }
}

static
void emit_two_regs_or_lits_args_setup ( UInt argv[], Tag tagv[],
                                        UInt src1, UInt src2,
                                        UInt dst1, UInt dst2)
{
   /* If either are lits, order doesn't matter */
   if (Literal == tagv[src1] || Literal == tagv[src2]) {
      maybe_emit_movl_litOrReg_reg ( argv[src1], tagv[src1], dst1 );
      maybe_emit_movl_litOrReg_reg ( argv[src2], tagv[src2], dst2 );

   } else {
      emit_two_regs_args_setup ( argv[src1], argv[src2], dst1, dst2 );
   }
}

static
void emit_three_regs_or_lits_args_setup ( UInt argv[], Tag tagv[],
                                          UInt src1, UInt src2, UInt src3,
                                          UInt dst1, UInt dst2, UInt dst3)
{
   // SSS: fix this eventually -- make STOREV use two RealRegs?
   /* Not supporting literals for 3-arg C functions -- they're only used
      by STOREV which has 2 args */
   vg_assert(RealReg == tagv[src1] &&
             RealReg == tagv[src2] &&
             RealReg == tagv[src3]);
   emit_three_regs_args_setup ( argv[src1], argv[src2], argv[src3],
                                dst1, dst2, dst3 );
}

/* Synthesise a call to a C function `fn' (which must be registered in
   baseBlock) doing all the reg saving and arg handling work.
 
   WARNING:  a UInstr should *not* be translated with synth_ccall followed
   by some other x86 assembly code;  vg_liveness_analysis() doesn't expect
   such behaviour and everything will fall over.
 */
void VG_(synth_ccall) ( Addr fn, Int argc, Int regparms_n, UInt argv[],
                        Tag tagv[], Int ret_reg,
                        RRegSet regs_live_before, RRegSet regs_live_after )
{
   Int  i;
   Int  stack_used = 0;
   Bool preserve_eax, preserve_ecx, preserve_edx;

   vg_assert(0 <= regparms_n && regparms_n <= 3);

   ccalls++;

   /* If %e[acd]x is live before and after the C call, save/restore it.
      Unless the return values clobbers the reg;  in this case we must not
      save/restore the reg, because the restore would clobber the return
      value.  (Before and after the UInstr really constitute separate live
      ranges, but you miss this if you don't consider what happens during
      the UInstr.) */
#  define PRESERVE_REG(realReg)   \
   (IS_RREG_LIVE(VG_(realreg_to_rank)(realReg), regs_live_before) &&   \
    IS_RREG_LIVE(VG_(realreg_to_rank)(realReg), regs_live_after)  &&   \
    ret_reg != realReg)

   preserve_eax = PRESERVE_REG(R_EAX);
   preserve_ecx = PRESERVE_REG(R_ECX);
   preserve_edx = PRESERVE_REG(R_EDX);

#  undef PRESERVE_REG

   /* Save caller-save regs as required */
   if (preserve_eax) { VG_(emit_pushv_reg) ( 4, R_EAX ); ccall_reg_saves++; }
   if (preserve_ecx) { VG_(emit_pushv_reg) ( 4, R_ECX ); ccall_reg_saves++; }
   if (preserve_edx) { VG_(emit_pushv_reg) ( 4, R_EDX ); ccall_reg_saves++; }

   /* Args are passed in two groups: (a) via stack (b) via regs.  regparms_n
      is the number of args passed in regs (maximum 3 for GCC on x86). */

   ccall_args += argc;
   
   /* First push stack args (RealRegs or Literals) in reverse order. */
   for (i = argc-1; i >= regparms_n; i--) {
      switch (tagv[i]) {
      case RealReg:
         VG_(emit_pushv_reg) ( 4, argv[i] );
         break;
      case Literal:
         /* Use short form of pushl if possible. */
         if (argv[i] == VG_(extend_s_8to32) ( argv[i] ))
            VG_(emit_pushl_lit8) ( VG_(extend_s_8to32)(argv[i]) );
         else
            VG_(emit_pushl_lit32)( argv[i] );
         break;
      default:
         VG_(printf)("tag=%d\n", tagv[i]);
         VG_(core_panic)("VG_(synth_ccall): bad tag");
      }
      stack_used += 4;
      ccall_arg_setup_instrs++;
   }

   /* Then setup args in registers (arg[123] --> %e[adc]x;  note order!).
      If moving values between registers, be careful not to clobber any on
      the way.  Happily we can use xchgl to swap registers.
   */
   switch (regparms_n) {

   /* Trickiest.  Args passed in %eax, %edx, and %ecx. */
   case 3:
      emit_three_regs_or_lits_args_setup ( argv, tagv, 0, 1, 2,
                                           R_EAX, R_EDX, R_ECX );
      break;

   /* Less-tricky.  Args passed in %eax and %edx. */
   case 2:
      emit_two_regs_or_lits_args_setup ( argv, tagv, 0, 1, R_EAX, R_EDX );
      break;
      
   /* Easy.  Just move arg1 into %eax (if not already in there). */
   case 1:  
      maybe_emit_movl_litOrReg_reg ( argv[0], tagv[0], R_EAX );
      break;

   case 0:
      break;

   default:
      VG_(core_panic)("VG_(synth_call): regparms_n value not in range 0..3");
   }
   
   /* Call the function - may trash all flags */
   VG_(synth_call) ( False, VG_(helper_offset) ( fn ), False, FlagsEmpty, FlagsOSZACP );

   /* Clear any args from stack */
   if (0 != stack_used) {
      VG_(emit_add_lit_to_esp) ( stack_used );
      ccall_stack_clears++;
   }

   /* Move return value into ret_reg if necessary and not already there */
   if (INVALID_REALREG != ret_reg) {
      ccall_retvals++;
      if (R_EAX != ret_reg) {
         VG_(emit_movv_reg_reg) ( 4, R_EAX, ret_reg );
         ccall_retval_movs++;
      }
   }

   /* Restore live caller-save regs as required */
   if (preserve_edx) VG_(emit_popv_reg) ( 4, R_EDX ); 
   if (preserve_ecx) VG_(emit_popv_reg) ( 4, R_ECX ); 
   if (preserve_eax) VG_(emit_popv_reg) ( 4, R_EAX ); 
}

static void load_ebp_from_JmpKind ( JmpKind jmpkind )
{
   switch (jmpkind) {
      case JmpBoring: 
         break;
      case JmpRet: 
         break;
      case JmpCall:
         break;
      case JmpSyscall: 
         VG_(emit_movv_lit_reg) ( 4, VG_TRC_EBP_JMP_SYSCALL, R_EBP );
         break;
      case JmpClientReq: 
         VG_(emit_movv_lit_reg) ( 4, VG_TRC_EBP_JMP_CLIENTREQ, R_EBP );
         break;
      default: 
         VG_(core_panic)("load_ebp_from_JmpKind");
   }
}

/* Jump to the next translation, by loading its original addr into
   %eax and returning to the scheduler.  Signal special requirements
   by loading a special value into %ebp first.  
*/
static void synth_jmp_reg ( Int reg, JmpKind jmpkind )
{
   maybe_emit_put_eflags();	/* save flags here */
   load_ebp_from_JmpKind ( jmpkind );
   if (reg != R_EAX)
      VG_(emit_movv_reg_reg) ( 4, reg, R_EAX );
   emit_ret();
}

static void synth_mov_reg_offregmem ( Int size, Int reg, Int off, Int areg );

/* Same deal as synth_jmp_reg. */
static void synth_jmp_lit ( Addr addr, JmpKind jmpkind )
{
   maybe_emit_put_eflags();	/* save flags here */

   VG_(emit_movv_lit_reg) ( 4, addr, R_EAX );

   if (VG_(clo_chain_bb) && (jmpkind == JmpBoring || jmpkind == JmpCall)) {
      synth_mov_reg_offregmem(4, R_EAX, 4*VGOFF_(m_eip), R_EBP); /* update EIP */
      emit_call_patchme();
   } else {
      load_ebp_from_JmpKind ( jmpkind );
      emit_ret();
   }
}


static void synth_mov_offregmem_reg ( Int size, Int off, Int areg, Int reg );
static void synth_nonshiftop_lit_reg ( Bool upd_cc,
                                       Opcode opcode, Int size, 
                                       UInt lit, Int reg );

static void synth_jcond_lit ( Condcode cond, 
                              Addr addr,
                              Bool eax_trashable )
{
   UInt mask;
   Int  delta;
   Bool simd;

   if (VG_(clo_chain_bb)) {
      /* When using BB chaining, the jump sequence is:
         ensure that simulated eflags are up-to-date
         jmp short if not cond to xyxyxy, using the real
            machine eflags if we can, synthesising a suitable sequence 
            to examine the simulated ones otherwise
         addr -> eax
         call VG_(patch_me)/jmp target
         xyxyxy
         
                 <possibly sequence to compute some condition>
                 j<cond>    xyxyxy
                 mov    $0x4000d190,%eax                     // 5
                 mov    %eax, VGOFF_(m_eip)(%ebp)            // 3
                 call   0x40050f9a <vgPlain_patch_me>        // 5
         xyxyxy: mov    $0x4000d042,%eax
                 call   0x40050f9a <vgPlain_patch_me>
      */
      delta = 5+3+5;
   } else {
      /* When not using BB chaining:
         ensure that simulated eflags are up-to-date
         jmp short if not cond to xyxyxy, using the real
            machine eflags if we can, synthesising a suitable sequence 
            to examine the simulated ones otherwise
         addr -> eax
         ret
         xyxyxy

                 <possibly sequence to compute some condition>
                 j<cond>     xyxyxy
                 movl    $0x44556677, %eax    // 5
                 ret                          // 1
         xyxyxy:
      */
      delta = 5+1;
   }
   
   /* Ensure simulated %EFLAGS are up-to-date, by copying back %eflags
      if need be */
   maybe_emit_put_eflags();
   vg_assert(eflags_state == UPD_Both || eflags_state == UPD_Simd);

   if (eflags_state == UPD_Both) {
      /* The flags are already set up, so we just use them as is. */
      simd = True;
      cond = invertCondition(cond);
   } else {
      Bool parity = False;	/* test Z or P */

      /* The simd state contains the most recent version, so we emit a
         sequence to calculate the relevant condition directly out of
         the simd flags.  This is much cheaper (on P3/P4/Athlon) than
         copying them back to the real flags via popf.  Notice that
         some of these sequences trash %eax, but that should be free
         now since this is the end of a bb and therefore all regs are
         dead. */
      simd = False;

      switch (cond) {

         case CondLE: 
         case CondNLE:
            vg_assert(eax_trashable);

            VG_(emit_movv_offregmem_reg)
               ( 4, VGOFF_(m_eflags) * 4, R_EBP, R_EAX );
            /* eax == %EFLAGS */

            VG_(emit_shiftopv_lit_reg)( False, 4, SHR, 11-7, R_EAX );
            /* eax has OF in SF's place */

            emit_nonshiftopv_offregmem_reg 
               ( False, 4, XOR, VGOFF_(m_eflags) * 4, R_EBP, R_EAX );
            /* eax has (OF xor SF) in SF's place */

            VG_(emit_shiftopv_lit_reg)( False, 4, SHR, 7-6, R_EAX );
            /* eax has (OF xor SF) in ZF's place */

            emit_nonshiftopv_offregmem_reg 
               ( False, 4, OR, VGOFF_(m_eflags) * 4, R_EBP, R_EAX );
            /* eax has ((OF xor SF) or ZF) in SF's place */
        
            VG_(emit_nonshiftopv_lit_reg)( False, 4, AND, 1 << 6, R_EAX );
            /* Z is now set iff ((OF xor SF) or ZF) == 1 */

            if (cond == CondLE) cond = CondZ; else cond = CondNZ;
            break;

         case CondL: 
         case CondNL:
            parity = True; 
            mask = EFlagO | EFlagS;    /* O != S     */
            goto simple;

         case CondB: 
         case CondNB: 
            mask = EFlagC; goto simple; /* C=1        */

         case CondZ: 
         case CondNZ: 
            mask = EFlagZ; goto simple; /* Z=1        */

         case CondBE: 
         case CondNBE: 
            mask = EFlagC | EFlagZ; goto simple; /* C=1 || Z=1 */

         case CondS:  
         case CondNS:
            mask = EFlagS; goto simple; /* S=1        */

         case CondP:
         case CondNP: 
            mask = EFlagP; goto simple; /* P=1        */

         default: 
            VG_(printf)("synth_jcond_lit: unhandled simd case %d (%s)\n", 
                        (Int)cond, VG_(nameCondcode)(cond) );
            VG_(core_panic)("synth_jcond_lit: unhandled simd case");

          simple:
            VG_(new_emit)(False, False, FlagsOSZACP);
            if ((mask & 0xff) == mask) {
               VG_(emitB) ( 0xF6 );   /* Grp3 */
               VG_(emit_amode_offregmem_reg)(
                  VGOFF_(m_eflags) * 4, R_EBP, 0 /* subcode for TEST */);
               VG_(emitB) (mask);
               if (dis)
                  VG_(printf)("\n\t\ttestb $0x%x, %d(%%ebp)\n", 
                              mask, VGOFF_(m_eflags) * 4);
            } else {
               /* all cond codes are in lower 16 bits */
               vg_assert((mask & 0xffff) == mask);

               VG_(emitB) ( 0x66 );
               VG_(emitB) ( 0xF7 );
               VG_(emit_amode_offregmem_reg)(
                  VGOFF_(m_eflags) * 4, R_EBP, 0 /* subcode for TEST */);
               VG_(emitW) (mask);
               if (dis)
                  VG_(printf)("\n\t\ttestl $0x%x, %d(%%ebp)\n", 
                              mask, VGOFF_(m_eflags) * 4);
            }

            cond = (parity ? CondP : CondZ) | (cond & 1);
            break;
      }
   }

   VG_(emit_jcondshort_delta) ( simd, cond, delta );
   synth_jmp_lit ( addr, JmpBoring );
}



static void synth_jmp_ifzero_reg_lit ( Int reg, Addr addr )
{
   /* 0000 83FF00                cmpl    $0, %edi
      0003 750A                  jnz     next
      0005 B844332211            movl    $0x11223344, %eax
      000a C3                    ret
      next:
   */
   VG_(emit_cmpl_zero_reg) ( False, reg );
   if (VG_(clo_chain_bb))
      VG_(emit_jcondshort_delta) ( False, CondNZ, 5+3+5 );
   else
      VG_(emit_jcondshort_delta) ( False, CondNZ, 5+1 );
   synth_jmp_lit ( addr, JmpBoring );
}


static void synth_mov_lit_reg ( Int size, UInt lit, Int reg ) 
{
   /* Load the zero-extended literal into reg, at size l,
      regardless of the request size. */
   VG_(emit_movv_lit_reg) ( 4, lit, reg );
}


static void synth_mov_regmem_reg ( Int size, Int reg1, Int reg2 ) 
{
   switch (size) {
      case 4: emit_movv_regmem_reg ( 4, reg1, reg2 ); break;
      case 2: emit_movzwl_regmem_reg ( reg1, reg2 ); break;
      case 1: emit_movzbl_regmem_reg ( reg1, reg2 ); break;
      default: VG_(core_panic)("synth_mov_regmem_reg");
   }  
}


static void synth_mov_offregmem_reg ( Int size, Int off, Int areg, Int reg ) 
{
   switch (size) {
      case 4: VG_(emit_movv_offregmem_reg) ( 4, off, areg, reg ); break;
      case 2: VG_(emit_movzwl_offregmem_reg) ( off, areg, reg ); break;
      case 1: VG_(emit_movzbl_offregmem_reg) ( off, areg, reg ); break;
      default: VG_(core_panic)("synth_mov_offregmem_reg");
   }  
}


static void synth_mov_reg_offregmem ( Int size, Int reg, 
                                      Int off, Int areg )
{
   switch (size) {
      case 4: VG_(emit_movv_reg_offregmem) ( 4, reg, off, areg ); break;
      case 2: VG_(emit_movv_reg_offregmem) ( 2, reg, off, areg ); break;
      case 1: if (reg < 4) {
                 VG_(emit_movb_reg_offregmem) ( reg, off, areg ); 
              }
              else {
                 VG_(emit_swapl_reg_EAX) ( reg );
                 VG_(emit_movb_reg_offregmem) ( R_AL, off, areg );
                 VG_(emit_swapl_reg_EAX) ( reg );
              }
              break;
      default: VG_(core_panic)("synth_mov_reg_offregmem");
   }
}


static void synth_mov_reg_memreg ( Int size, Int reg1, Int reg2 )
{
   Int s1;
   switch (size) {
      case 4: emit_movv_reg_regmem ( 4, reg1, reg2 ); break;
      case 2: emit_movv_reg_regmem ( 2, reg1, reg2 ); break;
      case 1: if (reg1 < 4) {
                 emit_movb_reg_regmem ( reg1, reg2 ); 
              }
              else {
                 /* Choose a swap reg which is < 4 and not reg1 or reg2. */
                 for (s1 = 0; s1 == reg1 || s1 == reg2; s1++) ;
                 emit_swapl_reg_reg ( s1, reg1 );
                 emit_movb_reg_regmem ( s1, reg2 );
                 emit_swapl_reg_reg ( s1, reg1 );
              }
              break;
      default: VG_(core_panic)("synth_mov_reg_litmem");
   }
}


static void synth_unaryop_reg ( Bool upd_cc,
                                Opcode opcode, Int size,
                                Int reg )
{
   /* NB! opcode is a uinstr opcode, not an x86 one! */
   switch (size) {
      case 4: VG_(emit_unaryopv_reg) ( upd_cc, 4, opcode, reg );
              break;
      case 2: VG_(emit_unaryopv_reg) ( upd_cc, 2, opcode, reg );
              break;
      case 1: if (reg < 4) {
                 VG_(emit_unaryopb_reg) ( upd_cc, opcode, reg );
              } else {
                 VG_(emit_swapl_reg_EAX) ( reg );
                 VG_(emit_unaryopb_reg) ( upd_cc, opcode, R_AL );
                 VG_(emit_swapl_reg_EAX) ( reg );
              }
              break;
      default: VG_(core_panic)("synth_unaryop_reg");
   }
}



static void synth_nonshiftop_reg_reg ( Bool upd_cc, 
                                       Opcode opcode, Int size, 
                                       Int reg1, Int reg2 )
{
   /* NB! opcode is a uinstr opcode, not an x86 one! */
   switch (size) {
      case 4: VG_(emit_nonshiftopv_reg_reg) ( upd_cc, 4, opcode, reg1, reg2 );
              break;
      case 2: VG_(emit_nonshiftopv_reg_reg) ( upd_cc, 2, opcode, reg1, reg2 );
              break;
      case 1: { /* Horrible ... */
         Int s1, s2;
         /* Choose s1 and s2 to be x86 regs which we can talk about the
            lowest 8 bits, ie either %eax, %ebx, %ecx or %edx.  Make
            sure s1 != s2 and that neither of them equal either reg1 or
            reg2. Then use them as temporaries to make things work. */
         if (reg1 < 4 && reg2 < 4) {
            emit_nonshiftopb_reg_reg(upd_cc, opcode, reg1, reg2); 
            break;
         }
         for (s1 = 0; s1 == reg1 || s1 == reg2; s1++) ;
         if (reg1 >= 4 && reg2 < 4) {
            emit_swapl_reg_reg ( reg1, s1 );
            emit_nonshiftopb_reg_reg(upd_cc, opcode, s1, reg2);
            emit_swapl_reg_reg ( reg1, s1 );
            break;
         }
         for (s2 = 0; s2 == reg1 || s2 == reg2 || s2 == s1; s2++) ;
         if (reg1 < 4 && reg2 >= 4) {
            emit_swapl_reg_reg ( reg2, s2 );
            emit_nonshiftopb_reg_reg(upd_cc, opcode, reg1, s2);
            emit_swapl_reg_reg ( reg2, s2 );
            break;
         }
         if (reg1 >= 4 && reg2 >= 4 && reg1 != reg2) {
            emit_swapl_reg_reg ( reg1, s1 );
            emit_swapl_reg_reg ( reg2, s2 );
            emit_nonshiftopb_reg_reg(upd_cc, opcode, s1, s2);
            emit_swapl_reg_reg ( reg1, s1 );
            emit_swapl_reg_reg ( reg2, s2 );
            break;
         }
         if (reg1 >= 4 && reg2 >= 4 && reg1 == reg2) {
            emit_swapl_reg_reg ( reg1, s1 );
            emit_nonshiftopb_reg_reg(upd_cc, opcode, s1, s1);
            emit_swapl_reg_reg ( reg1, s1 );
            break;
         }
         VG_(core_panic)("synth_nonshiftopb_reg_reg");
      }
      default: VG_(core_panic)("synth_nonshiftop_reg_reg");
   }
}


static void synth_nonshiftop_reg_offregmem (
   Bool upd_cc,
   Opcode opcode, Int size, 
   Int off, Int areg, Int reg )
{
   switch (size) {
      case 4: 
         emit_nonshiftopv_reg_offregmem ( upd_cc, 4, opcode, off, areg, reg ); 
         break;
      case 2: 
         emit_nonshiftopv_reg_offregmem ( upd_cc, 2, opcode, off, areg, reg ); 
         break;
      case 1: 
         if (reg < 4) {
            emit_nonshiftopb_reg_offregmem ( upd_cc, opcode, off, areg, reg );
         } else {
            VG_(emit_swapl_reg_EAX) ( reg );
            emit_nonshiftopb_reg_offregmem ( upd_cc, opcode, off, areg, R_AL );
            VG_(emit_swapl_reg_EAX) ( reg );
         }
         break;
      default: 
         VG_(core_panic)("synth_nonshiftop_reg_offregmem");
   }
}

static void synth_nonshiftop_offregmem_reg ( 
   Bool upd_cc,
   Opcode opcode, Int size, 
   Int off, Int areg, Int reg )
{
   switch (size) {
      case 4: 
         emit_nonshiftopv_offregmem_reg ( upd_cc, 4, opcode, off, areg, reg ); 
         break;
      case 2: 
         emit_nonshiftopv_offregmem_reg ( upd_cc, 2, opcode, off, areg, reg ); 
         break;
      case 1: 
         if (reg < 4) {
            emit_nonshiftopb_offregmem_reg ( upd_cc, opcode, off, areg, reg );
         } else {
            VG_(emit_swapl_reg_EAX) ( reg );
            emit_nonshiftopb_offregmem_reg ( upd_cc, opcode, off, areg, R_AL );
            VG_(emit_swapl_reg_EAX) ( reg );
         }
         break;
      default: 
         VG_(core_panic)("synth_nonshiftop_offregmem_reg");
   }
}


static void synth_nonshiftop_lit_reg ( Bool upd_cc,
                                       Opcode opcode, Int size, 
                                       UInt lit, Int reg )
{
   switch (size) {
      case 4: VG_(emit_nonshiftopv_lit_reg) ( upd_cc, 4, opcode, lit, reg );
              break;
      case 2: VG_(emit_nonshiftopv_lit_reg) ( upd_cc, 2, opcode, lit, reg );
              break;
      case 1: if (reg < 4) {
                 emit_nonshiftopb_lit_reg ( upd_cc, opcode, lit, reg );
              } else {
                 VG_(emit_swapl_reg_EAX) ( reg );
                 emit_nonshiftopb_lit_reg ( upd_cc, opcode, lit, R_AL );
                 VG_(emit_swapl_reg_EAX) ( reg );
              }
              break;
      default: VG_(core_panic)("synth_nonshiftop_lit_reg");
   }
}

static void synth_nonshiftop_lit_offregmem ( Bool upd_cc,
					     Opcode opcode, Int size, 
					     UInt lit, Int off, Int regmem )
{
   switch (size) {
      case 4: VG_(emit_nonshiftopv_lit_offregmem) ( upd_cc, 4, opcode, lit, off, regmem );
              break;
      case 2: VG_(emit_nonshiftopv_lit_offregmem) ( upd_cc, 2, opcode, lit, off, regmem );
              break;
      case 1: emit_nonshiftopb_lit_offregmem ( upd_cc, opcode, lit, off, regmem );
              break;
      default: VG_(core_panic)("synth_nonshiftop_lit_offregmem");
   }
}


static void synth_push_reg ( Int size, Int reg )
{
   switch (size) {
      case 4: 
         VG_(emit_pushv_reg) ( 4, reg ); 
         break;
      case 2: 
         VG_(emit_pushv_reg) ( 2, reg ); 
         break;
      /* Pray that we don't have to generate this really cruddy bit of
         code very often.  Could do better, but can I be bothered? */
      case 1: 
         vg_assert(reg != R_ESP); /* duh */
         VG_(emit_add_lit_to_esp)(-1);
         if (reg != R_EAX) VG_(emit_swapl_reg_EAX) ( reg );
         emit_movb_AL_zeroESPmem();
         if (reg != R_EAX) VG_(emit_swapl_reg_EAX) ( reg );
         break;
     default: 
         VG_(core_panic)("synth_push_reg");
   }
}


static void synth_pop_reg ( Int size, Int reg )
{
   switch (size) {
      case 4: 
         VG_(emit_popv_reg) ( 4, reg ); 
         break;
      case 2: 
         VG_(emit_popv_reg) ( 2, reg ); 
         break;
      case 1:
         /* Same comment as above applies. */
         vg_assert(reg != R_ESP); /* duh */
         if (reg != R_EAX) VG_(emit_swapl_reg_EAX) ( reg );
         emit_movb_zeroESPmem_AL();
         if (reg != R_EAX) VG_(emit_swapl_reg_EAX) ( reg );
         VG_(emit_add_lit_to_esp)(1);
         break;
      default: VG_(core_panic)("synth_pop_reg");
   }
}


static void synth_shiftop_reg_reg ( Bool upd_cc,
                                    Opcode opcode, Int size, 
                                    Int regs, Int regd )
{
   synth_push_reg ( size, regd );
   if (regs != R_ECX) emit_swapl_reg_ECX ( regs );
   switch (size) {
      case 4: emit_shiftopv_cl_stack0 ( upd_cc, 4, opcode ); break;
      case 2: emit_shiftopv_cl_stack0 ( upd_cc, 2, opcode ); break;
      case 1: emit_shiftopb_cl_stack0 ( upd_cc, opcode ); break;
      default: VG_(core_panic)("synth_shiftop_reg_reg");
   }
   if (regs != R_ECX) emit_swapl_reg_ECX ( regs );
   synth_pop_reg ( size, regd );
}


static void synth_shiftop_lit_reg ( Bool upd_cc,
                                    Opcode opcode, Int size, 
                                    UInt lit, Int reg )
{
   switch (size) {
      case 4: VG_(emit_shiftopv_lit_reg) ( upd_cc, 4, opcode, lit, reg );
              break;
      case 2: VG_(emit_shiftopv_lit_reg) ( upd_cc, 2, opcode, lit, reg );
              break;
      case 1: if (reg < 4) {
                 emit_shiftopb_lit_reg ( upd_cc, opcode, lit, reg );
              } else {
                 VG_(emit_swapl_reg_EAX) ( reg );
                 emit_shiftopb_lit_reg ( upd_cc, opcode, lit, R_AL );
                 VG_(emit_swapl_reg_EAX) ( reg );
              }
              break;
      default: VG_(core_panic)("synth_shiftop_lit_reg");
   }
}


static void synth_setb_reg ( Int reg, Condcode cond )
{
   if (reg < 4) {
      emit_setb_reg ( reg, cond );
   } else {
      VG_(emit_swapl_reg_EAX) ( reg );
      emit_setb_reg ( R_AL, cond );
      VG_(emit_swapl_reg_EAX) ( reg );
   }
}


static void synth_fpu_regmem ( Bool uses_flags, Bool sets_flags,
			       UChar first_byte,
                               UChar second_byte_masked, 
                               Int reg )
{
   emit_fpu_regmem ( uses_flags, sets_flags, first_byte, second_byte_masked, reg );
}


static void synth_fpu_no_mem ( Bool uses_flags, Bool sets_flags,
			       UChar first_byte,
                               UChar second_byte )
{
   emit_fpu_no_mem ( uses_flags, sets_flags, first_byte, second_byte );
}


static void synth_movl_reg_reg ( Int src, Int dst )
{
   emit_movl_reg_reg ( src, dst );
}

static void synth_cmovl_reg_reg ( Condcode cond, Int src, Int dst )
{
   VG_(emit_jcondshort_delta) ( True, invertCondition(cond), 
                           2 /* length of the next insn */ );
   emit_movl_reg_reg ( src, dst );
}


/*----------------------------------------------------*/
/*--- Top level of the uinstr -> x86 translation.  ---*/
/*----------------------------------------------------*/

/* Return the byte offset from %ebp (ie, into baseBlock)
   for the specified ArchReg or SpillNo. */
static Int spillOrArchOffset ( Int size, Tag tag, UInt value )
{
   if (tag == SpillNo) {
      vg_assert(size == 4);
      vg_assert(value >= 0 && value < VG_MAX_SPILLSLOTS);
      return 4 * (value + VGOFF_(spillslots));
   }
   if (tag == ArchReg) {
      switch (value) {
         case R_EAX: return 4 * VGOFF_(m_eax);
         case R_ECX: return 4 * VGOFF_(m_ecx);
         case R_EDX: return 4 * VGOFF_(m_edx);
         case R_EBX: return 4 * VGOFF_(m_ebx);
         case R_ESP:
           if (size == 1) return 4 * VGOFF_(m_eax) + 1;
                     else return 4 * VGOFF_(m_esp);
         case R_EBP:
           if (size == 1) return 4 * VGOFF_(m_ecx) + 1;
                     else return 4 * VGOFF_(m_ebp);
         case R_ESI:
           if (size == 1) return 4 * VGOFF_(m_edx) + 1;
                     else return 4 * VGOFF_(m_esi);
         case R_EDI:
           if (size == 1) return 4 * VGOFF_(m_ebx) + 1;
                     else return 4 * VGOFF_(m_edi);
      }
   }
   VG_(core_panic)("spillOrArchOffset");
}

static Int eflagsOffset ( void )
{
   return 4 * VGOFF_(m_eflags);
}

static Int segRegOffset ( UInt archregs )
{
   switch (archregs) {
      case R_CS: return 4 * VGOFF_(m_cs);
      case R_SS: return 4 * VGOFF_(m_ss);
      case R_DS: return 4 * VGOFF_(m_ds);
      case R_ES: return 4 * VGOFF_(m_es);
      case R_FS: return 4 * VGOFF_(m_fs);
      case R_GS: return 4 * VGOFF_(m_gs);
      default: VG_(core_panic)("segRegOffset");
   }
}


/* Return the byte offset from %ebp (ie, into baseBlock)
   for the specified shadow register */
Int VG_(shadow_reg_offset) ( Int arch )
{
   switch (arch) {
      case R_EAX: return 4 * VGOFF_(sh_eax);
      case R_ECX: return 4 * VGOFF_(sh_ecx);
      case R_EDX: return 4 * VGOFF_(sh_edx);
      case R_EBX: return 4 * VGOFF_(sh_ebx);
      case R_ESP: return 4 * VGOFF_(sh_esp);
      case R_EBP: return 4 * VGOFF_(sh_ebp);
      case R_ESI: return 4 * VGOFF_(sh_esi);
      case R_EDI: return 4 * VGOFF_(sh_edi);
      default:    VG_(core_panic)( "shadowOffset");
   }
}

Int VG_(shadow_flags_offset) ( void )
{
   return 4 * VGOFF_(sh_eflags);
}



static void synth_WIDEN_signed ( Int sz_src, Int sz_dst, Int reg )
{
   if (sz_src == 1 && sz_dst == 4) {
      VG_(emit_shiftopv_lit_reg) ( False, 4, SHL, 24, reg );
      VG_(emit_shiftopv_lit_reg) ( False, 4, SAR, 24, reg );
   }
   else if (sz_src == 2 && sz_dst == 4) {
      VG_(emit_shiftopv_lit_reg) ( False, 4, SHL, 16, reg );
      VG_(emit_shiftopv_lit_reg) ( False, 4, SAR, 16, reg );
   }
   else if (sz_src == 1 && sz_dst == 2) {
      VG_(emit_shiftopv_lit_reg) ( False, 2, SHL, 8, reg );
      VG_(emit_shiftopv_lit_reg) ( False, 2, SAR, 8, reg );
   }
   else
      VG_(core_panic)("synth_WIDEN");
}


static void synth_handle_esp_assignment ( Int i, Int reg,
                                          RRegSet regs_live_before,
                                          RRegSet regs_live_after )
{
   UInt argv[] = { reg };
   Tag  tagv[] = { RealReg };

   VG_(synth_ccall) ( (Addr) VG_(handle_esp_assignment), 1, 1, argv, tagv, 
                      INVALID_REALREG, regs_live_before, regs_live_after);
}


/*----------------------------------------------------*/
/*--- Generate code for a single UInstr.           ---*/
/*----------------------------------------------------*/

static __inline__
Bool writeFlagUse ( UInstr* u )
{
   return (u->flags_w != FlagsEmpty); 
}

static __inline__
Bool readFlagUse ( UInstr* u )
{
   /* If the UInstr writes some flags but not all, then we still need
      to consider it as reading flags so that the unchanged values are
      passed through properly. (D is special) */
   return
      (u->flags_r != FlagsEmpty) || 
      (u->flags_w != FlagsEmpty && u->flags_w != FlagsOSZACP) ; 
}

static __inline__ 
Bool anyFlagUse ( UInstr* u )
{
   return readFlagUse(u) || writeFlagUse(u);
}


/* *fplive==True indicates that the simulated machine's FPU state is in
   the real FPU.  If so we need to be very careful not to trash it.
   If FPU state is live and we deem it necessary to copy it back to
   the simulated machine's FPU state, we do so.  The final state of
   fpliveness is returned.  In short we _must_ do put_fpu_state if
   there is any chance at all that the code generated for a UInstr
   will change the real FPU state.  
*/
static void emitUInstr ( UCodeBlock* cb, Int i, 
                         RRegSet regs_live_before, 
			 /* Running state, which we update. */
                         Bool* fplive,   /* True<==>FPU state in real FPU */
                         Addr* orig_eip, /* previous curr_eip, or zero */
                         Addr* curr_eip ) /* current eip */
{
   Int     old_emitted_code_used;
   UInstr* u = &cb->instrs[i];

   if (dis)
      VG_(pp_UInstr_regs)(i, u);

   old_emitted_code_used = emitted_code_used;
   
   switch (u->opcode) {
      case NOP: case LOCK: case CALLM_S: case CALLM_E: break;

      case INCEIP:
         /* Advance %EIP some small amount. */
         *curr_eip += (UInt)(u->val1);

         if (*orig_eip == 0 /* we don't know what the old value was */
             || ((*orig_eip & ~0xFF) != (*curr_eip & ~0xFF))) {
            /* We have to update all 32 bits of the value. */
            VG_(emit_movv_lit_offregmem)(
               4, *curr_eip, 4*VGOFF_(m_eip), R_EBP);
         } else {
            /* Cool! we only need to update lowest 8 bits */
            VG_(emit_movb_lit_offregmem)(
               *curr_eip & 0xFF, 4*VGOFF_(m_eip)+0, R_EBP);
         }

         *orig_eip = *curr_eip;
         break;

      case LEA1: {
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         emit_lea_litreg_reg ( u->lit32, u->val1, u->val2 );
         break;
      }

      case LEA2: {
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->tag3 == RealReg);
         emit_lea_sib_reg ( u->lit32, u->extra4b, 
                            u->val1, u->val2, u->val3 );
         break;
      }

      case WIDEN: {
         vg_assert(u->tag1 == RealReg);
         if (u->signed_widen) {
            synth_WIDEN_signed ( u->extra4b, u->size, u->val1 );
         } else {
            /* no need to generate any code. */
         }
         break;
      }

      case STORE: {
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         synth_mov_reg_memreg ( u->size, u->val1, u->val2 );
         break;
      }

      case LOAD: {
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         synth_mov_regmem_reg ( u->size, u->val1, u->val2 );
         break;
      }

      case GET: {
         vg_assert(u->tag1 == ArchReg || u->tag1 == SpillNo);
         vg_assert(u->tag2 == RealReg);
         synth_mov_offregmem_reg ( 
            u->size, 
            spillOrArchOffset( u->size, u->tag1, u->val1 ),
            R_EBP,
            u->val2 
         );
         break;
      }
            
      case PUT: {
         vg_assert(u->tag2 == ArchReg || u->tag2 == SpillNo);
         vg_assert(u->tag1 == RealReg);
         if (u->tag2 == ArchReg 
             && u->val2 == R_ESP
             && u->size == 4
             && (VG_(track_events).new_mem_stack         || 
                 VG_(track_events).new_mem_stack_aligned ||
                 VG_(track_events).die_mem_stack         ||
                 VG_(track_events).die_mem_stack_aligned ||
                 VG_(track_events).post_mem_write))
         {
            synth_handle_esp_assignment ( i, u->val1, regs_live_before,
                                          u->regs_live_after );
	 }
         else {
            synth_mov_reg_offregmem ( 
               u->size, 
               u->val1, 
               spillOrArchOffset( u->size, u->tag2, u->val2 ),
               R_EBP
            );
         }
         break;
      }

      case GETSEG: {
         vg_assert(u->tag1 == ArchRegS);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->size == 2);
         synth_mov_offregmem_reg (
            4,
            segRegOffset( u->val1 ),
            R_EBP,
            u->val2
         );
         break;
      }

      case PUTSEG: {
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == ArchRegS);
         vg_assert(u->size == 2);
         synth_mov_reg_offregmem (
            4,
            u->val1,
            segRegOffset( u->val2 ),
            R_EBP 
         );
         break;
      }

      case GETF: {
         vg_assert(u->size == 2 || u->size == 4);
         vg_assert(u->tag1 == RealReg);
	 
	 /* This complexity is because the D(irection) flag is stored
	    separately from the rest of EFLAGS.  */

	 /* We're only fetching from the Simd state, so make sure it's
	    up to date. */
	 maybe_emit_put_eflags();

	 /* get D in u->val1 (== 1 or -1) */
         synth_mov_offregmem_reg (u->size, 4*VGOFF_(m_dflag), R_EBP, u->val1);

	 /* u->val1 &= EFlagD (== 0 or EFlagD) */
	 synth_nonshiftop_lit_reg(False, AND, u->size, EFlagD, u->val1);

	 /* EFLAGS &= ~EFlagD (make sure there's no surprises) */
	 synth_nonshiftop_lit_offregmem(False, AND, u->size, ~EFlagD,
					eflagsOffset(), R_EBP);

	 /* EFLAGS &= ~EFlagD (make sure there's no surprises) */
	 synth_nonshiftop_lit_offregmem(False, AND, u->size, ~EFlagD,
					eflagsOffset(), R_EBP);

	 /* u->val1 |= EFLAGS (EFLAGS & EflagD == 0) */
	 synth_nonshiftop_offregmem_reg(False, OR, u->size, 
					eflagsOffset(), R_EBP, u->val1);
         break;
      }
            
      case PUTF: {
         vg_assert(u->size == 2 || u->size == 4);
         vg_assert(u->tag1 == RealReg);

	 /* When putting a value into EFLAGS, this generates the
	    correct value for m_dflag (-1 or 1), and clears the D bit
	    in EFLAGS. */

	 /* We're updating the whole flag state, so the old state
	    doesn't matter; make sure that the new simulated state
	    will be fetched when needed. */
	 eflags_state = UPD_Simd;

	 /* store EFLAGS (with D) */
         synth_mov_reg_offregmem (u->size, u->val1, eflagsOffset(), R_EBP);
	 
	 /* u->val1 &= EFlagD */
	 synth_nonshiftop_lit_reg(False, AND, u->size, EFlagD, u->val1);

	 /* computes: u->val1 = (u->val1 == 0) ? 1 : -1 */
	 synth_unaryop_reg(False, NEG, u->size, u->val1);
	 synth_nonshiftop_reg_reg(False, SBB, u->size, u->val1, u->val1);
	 synth_nonshiftop_lit_reg(False, SBB, u->size, -1, u->val1);

	 /* save D */
	 synth_mov_reg_offregmem(u->size, u->val1, 4*VGOFF_(m_dflag), R_EBP);
	 
	 /* EFLAGS &= ~EFlagD */
	 synth_nonshiftop_lit_offregmem(False, AND, u->size, ~EFlagD,
					eflagsOffset(), R_EBP);
         break;
      }
            
      case MOV: {
         vg_assert(u->tag1 == RealReg || u->tag1 == Literal);
         vg_assert(u->tag2 == RealReg);
         switch (u->tag1) {
            case RealReg: vg_assert(u->size == 4);
                          if (u->val1 != u->val2)
                             synth_movl_reg_reg ( u->val1, u->val2 ); 
                          break;
            case Literal: synth_mov_lit_reg ( u->size, u->lit32, u->val2 ); 
                          break;
            default: VG_(core_panic)("emitUInstr:mov");
	 }
         break;
      }

      case USESEG: {
         /* Lazy: copy all three vals;  synth_ccall ignores any unnecessary
            ones. */
         UInt argv[]  = { u->val1, u->val2 };
         UInt tagv[]  = { RealReg, RealReg };
         UInt ret_reg = u->val2;

         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->size == 0);

	 if (*fplive) {
	    emit_put_fpu_state();
	    *fplive = False;
	 }

         VG_(synth_ccall) ( (Addr) & VG_(do_useseg), 
                            2, /* args */
                            0, /* regparms_n */
                            argv, tagv,
                            ret_reg, regs_live_before, u->regs_live_after );
         break;
      }

      case SBB:
      case ADC:
      case XOR:
      case OR:
      case AND:
      case SUB:
      case ADD: {
         vg_assert(u->tag2 == RealReg);
         switch (u->tag1) {
            case Literal: synth_nonshiftop_lit_reg (
                             anyFlagUse(u), 
                             u->opcode, u->size, u->lit32, u->val2 );
                          break;
            case RealReg: synth_nonshiftop_reg_reg (
                             anyFlagUse(u), 
                             u->opcode, u->size, u->val1, u->val2 );
                          break;
            case ArchReg: synth_nonshiftop_offregmem_reg (
                             anyFlagUse(u), 
                             u->opcode, u->size, 
                             spillOrArchOffset( u->size, u->tag1, u->val1 ), 
                             R_EBP,
                             u->val2 );
                          break;
            default: VG_(core_panic)("emitUInstr:non-shift-op");
         }
         break;
      }

      case RCR:
      case RCL:
      case ROR:
      case ROL:
      case SAR:
      case SHR:
      case SHL: {
         vg_assert(u->tag2 == RealReg);
         switch (u->tag1) {
            case Literal: synth_shiftop_lit_reg (
                             anyFlagUse(u), 
                             u->opcode, u->size, u->lit32, u->val2 );
                          break;
            case RealReg: synth_shiftop_reg_reg (
                             anyFlagUse(u), 
                             u->opcode, u->size, u->val1, u->val2 );
                          break;
            default: VG_(core_panic)("emitUInstr:non-shift-op");
         }
         break;
      }

      case INC:
      case DEC:
      case NEG:
      case NOT:
         vg_assert(u->tag1 == RealReg);
         synth_unaryop_reg ( 
            anyFlagUse(u), u->opcode, u->size, u->val1 );
         break;

      case BSWAP:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->size == 4);
	 vg_assert(!VG_(any_flag_use)(u));
         emit_bswapl_reg ( u->val1 );
         break;

      case CMOV: 
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->cond != CondAlways);
         vg_assert(u->size == 4);
         synth_cmovl_reg_reg ( u->cond, u->val1, u->val2 );
         break;

      case JMP: {
         vg_assert(u->tag2 == NoValue);
         vg_assert(u->tag1 == RealReg || u->tag1 == Literal);
	 if (*fplive) {
	    emit_put_fpu_state();
	    *fplive = False;
	 }
         if (u->cond == CondAlways) {
            switch (u->tag1) {
               case RealReg:
                  synth_jmp_reg ( u->val1, u->jmpkind );
                  break;
               case Literal:
                  synth_jmp_lit ( u->lit32, u->jmpkind );
                  break;
               default: 
                  VG_(core_panic)("emitUInstr(JMP, unconditional, default)");
                  break;
            }
         } else {
            switch (u->tag1) {
               case RealReg:
                  VG_(core_panic)("emitUInstr(JMP, conditional, RealReg)");
                  break;
               case Literal:
                  vg_assert(u->jmpkind == JmpBoring);
                  /* %eax had better not be live since synth_jcond_lit
                     trashes it in some circumstances.  If that turns
                     out to be a problem we can get synth_jcond_lit to
                     push/pop it when it is live. */
                  vg_assert(! IS_RREG_LIVE(VG_(realreg_to_rank)(R_EAX), 
                                           u->regs_live_after));
                  synth_jcond_lit ( u->cond, u->lit32, True );
                  break;
               default: 
                  VG_(core_panic)("emitUInstr(JMP, conditional, default)");
                  break;
            }
         }
         break;
      }

      case JIFZ:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == Literal);
         vg_assert(u->size == 4);
	 if (*fplive) {
	    emit_put_fpu_state();
	    *fplive = False;
	 }
         synth_jmp_ifzero_reg_lit ( u->val1, u->lit32 );
         break;

      case PUSH:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == NoValue);
         VG_(emit_pushv_reg) ( 4, u->val1 );
         break;

      case POP:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == NoValue);
         VG_(emit_popv_reg) ( 4, u->val1 );
         break;

      case CALLM:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
         vg_assert(u->size == 0);
	 if (*fplive) {
	    emit_put_fpu_state();
	    *fplive = False;
	 }
	 /* Call to a helper which is pretending to be a real CPU
	    instruction (and therefore operates on Real flags and
	    registers) */
         VG_(synth_call) ( False, u->val1, 
			   True, u->flags_r, u->flags_w );
         break;

      case CCALL: {
         /* If you change this, remember to change USESEG above, since
            that's just a copy of this, slightly simplified. */
         /* Lazy: copy all three vals;  synth_ccall ignores any unnecessary
            ones. */
         UInt argv[]  = { u->val1, u->val2, u->val3 };
         UInt tagv[]  = { RealReg, RealReg, RealReg };
         UInt ret_reg = ( u->has_ret_val ? u->val3 : INVALID_REALREG );

         if (u->argc >= 1)                   vg_assert(u->tag1 == RealReg);
         else                                vg_assert(u->tag1 == NoValue);
         if (u->argc >= 2)                   vg_assert(u->tag2 == RealReg);
         else                                vg_assert(u->tag2 == NoValue);
         if (u->argc == 3 || u->has_ret_val) vg_assert(u->tag3 == RealReg);
         else                                vg_assert(u->tag3 == NoValue);
         vg_assert(u->size == 0);

	 if (*fplive) {
	    emit_put_fpu_state();
	    *fplive = False;
	 }
         VG_(synth_ccall) ( u->lit32, u->argc, u->regparms_n, argv, tagv,
                            ret_reg, regs_live_before, u->regs_live_after );
         break;
      }

      case CLEAR:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
         VG_(emit_add_lit_to_esp) ( u->val1 );
         break;

      case CC2VAL:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == NoValue);
         vg_assert(VG_(any_flag_use)(u));
         synth_setb_reg ( u->val1, u->cond );
         break;

      case FPU_R: 
      case FPU_W:         
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == RealReg);
	 if (!(*fplive)) {
	    emit_get_fpu_state();
	    *fplive = True;
	 }
         synth_fpu_regmem ( u->flags_r, u->flags_w,
			    (u->val1 >> 8) & 0xFF,
                            u->val1 & 0xFF,
                            u->val2 );
         break;

      case FPU:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
         if (anyFlagUse ( u )) 
            emit_get_eflags();
	 if (!(*fplive)) {
	    emit_get_fpu_state();
	    *fplive = True;
	 }
         synth_fpu_no_mem ( u->flags_r, u->flags_w,
			    (u->val1 >> 8) & 0xFF,
                            u->val1 & 0xFF );
         break;

      default: 
         if (VG_(needs).extended_UCode) {
	    if (*fplive) {
	       emit_put_fpu_state();
	       *fplive = False;
	    }
            SK_(emit_XUInstr)(u, regs_live_before);
         } else {
            VG_(printf)("\nError:\n"
                        "  unhandled opcode: %u.  Perhaps "
                        " VG_(needs).extended_UCode should be set?\n",
                        u->opcode);
            VG_(pp_UInstr)(0,u);
            VG_(core_panic)("emitUInstr: unimplemented opcode");
         }
   }

   if (0 && (*fplive)) {
      emit_put_fpu_state();
      *fplive = False;
   }

   /* Update UInstr histogram */
   vg_assert(u->opcode < 100);
   histogram[u->opcode].counts++;
   histogram[u->opcode].size += (emitted_code_used - old_emitted_code_used);
}


/* Emit x86 for the ucode in cb, returning the address of the
   generated code and setting *nbytes to its size. */
UChar* VG_(emit_code) ( UCodeBlock* cb, 
                        Int*        nbytes, 
                        UShort      j[VG_MAX_JUMPS] )
{
   Int i;
   UChar regs_live_before = 0;   /* No regs live at BB start */
   Bool fplive;
   Addr orig_eip, curr_eip;
  
   reset_state();

   if (dis) VG_(printf)("Generated x86 code:\n");

   /* Generate decl VG_(dispatch_ctr) and drop into dispatch if we hit
      zero.  We have to do this regardless of whether we're t-chaining
      or not. */
   VG_(new_emit)(False, FlagsEmpty, FlagsOSZAP);
   VG_(emitB) (0xFF);	/* decl */
   emit_amode_litmem_reg((Addr)&VG_(dispatch_ctr), 1);
   if (dis)
      VG_(printf)("\n\t\tdecl (%p)\n", &VG_(dispatch_ctr));
   VG_(emit_jcondshort_delta)(False, CondNZ, 5+1);
   VG_(emit_movv_lit_reg) ( 4, VG_TRC_INNER_COUNTERZERO, R_EBP );
   emit_ret();

   /* Set up running state. */
   fplive   = False;
   orig_eip = cb->orig_eip;	/* we know EIP is up to date on BB entry */
   curr_eip = cb->orig_eip;
   vg_assert(curr_eip != 0); /* otherwise the incremental updating
                                algorithm gets messed up. */
   /* for each uinstr ... */
   for (i = 0; i < cb->used; i++) {
      UInstr* u = &cb->instrs[i];
      if (cb->instrs[i].opcode != NOP) {

         /* Check on the sanity of this insn. */
         Bool sane = VG_(saneUInstr)( False, False, u );
         if (!sane) {
            VG_(printf)("\ninsane instruction\n");
            VG_(up_UInstr)( i, u );
	 }
         vg_assert(sane);
         emitUInstr( cb, i, regs_live_before, 
                         &fplive, &orig_eip, &curr_eip );
      }
      regs_live_before = u->regs_live_after;
   }
   if (dis) VG_(printf)("\n");
   vg_assert(!fplive);			/* FPU state must be saved by end of BB */
   vg_assert(eflags_state != UPD_Real);	/* flags can't just be in CPU */

   if (j != NULL) {
      vg_assert(jumpidx <= VG_MAX_JUMPS);
      for(i = 0; i < jumpidx; i++)
	 j[i] = jumps[i];
   }

   /* Returns a pointer to the emitted code.  This will have to be
      copied by the caller into the translation cache, and then freed */
   *nbytes = emitted_code_used;
   return emitted_code;
}

#undef dis

/*--------------------------------------------------------------------*/
/*--- end                                          vg_from_ucode.c ---*/
/*--------------------------------------------------------------------*/
