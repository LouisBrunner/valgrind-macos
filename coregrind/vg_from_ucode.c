
/*--------------------------------------------------------------------*/
/*--- The JITter: translate ucode back to x86 code.                ---*/
/*---                                              vg_from_ucode.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
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

static enum _eflags_state {
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

         for (j = 0; j < (Int)size_pc; j++) VG_(printf)("O");
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


/* evidently unused */
#if 0
static void maybe_emit_get_eflags( void )
{
   if (eflags_state == UPD_Simd) {
      eflags_state = UPD_Both;
      emit_get_eflags();
   }
}
#endif


#if 0
/* begin UNUSED UNUSED UNUSED UNUSED UNUSED UNUSED UNUSED */
/* An alternative implementation of new_emit in which the
   state space is explicitly enumerated. */
__inline__ 
void VG_(new_emit) ( Bool upds_simd_flags, 
                     FlagSet use_flags, FlagSet set_flags )
{
  Bool simd = upds_simd_flags;
  enum _eflags_state where = eflags_state;

  enum { WNone, WSome, WAll } ww;
  Bool rr;

#define DIS_HEADER  \
   if (dis)         \
      VG_(printf)("\t       %4d: ", emitted_code_used );

  if (use_flags == FlagsEmpty) {
     rr = False;
  } else {
     rr = True;
  }

  if (set_flags == FlagsEmpty) {
     ww = WNone;
  } else
  if (set_flags == FlagsOSZACP) {
     ww = WAll;
  } else {
     ww = WSome;
  }

  /* If we're not wanting to interact with simd flags, and the simd
     flags are not in the real flags, then do nothing. */
  if (simd == False && where == UPD_Simd)
     goto noaction;

  if (simd == True && where == UPD_Simd && rr == False && ww == WAll) {
     /* We're going to generate a complete new simd flag state without
        consulting the old one first, so just deem this insn to create
        the state in the real flags. */
     eflags_state = UPD_Real;
     DIS_HEADER;
     return;
  }

  if (simd == True && where == UPD_Simd && rr == False && ww == WSome) {
     /* Want to partially update the flags state, but is in simd.  So
        fetch it first, then declare that the real state is the most
        recent. */
     emit_get_eflags();
     eflags_state = UPD_Real;
     DIS_HEADER;
     return;
  }

  if (simd == True && where == UPD_Simd && rr == True && ww == WNone) {
     /* want to read simd flags, but not in real -> copy to real. */
     emit_get_eflags();
     eflags_state = UPD_Both;
     DIS_HEADER;
     return;
  }

  if (simd == True && where == UPD_Simd && rr == True && ww == WAll) {
     /* want to read and write simd flags, but not in real -> copy to
	real.  State is then Real since they get updated. */
     emit_get_eflags();
     eflags_state = UPD_Real;
     DIS_HEADER;
     return;
  }

  if (simd == True && where == UPD_Simd && rr == False && ww == WNone) {
     /* Doesn't really make sense.  Want to interact with simd flags,
        but insn doesn't modify them.  So don't do anything. ??? */
     goto noaction;
  }

  if (simd == True && where == UPD_Real && rr == False && ww == WNone) {
     /* Doesn't really make sense.  Want to interact with simd flags,
        but insn doesn't modify them.  So don't do anything. ??? */
     goto noaction;
  }

  if (simd == True && where == UPD_Real && rr == True && ww == WNone) {
     /* simd is in real.  Insn reads real but does not change. --> do
        nothing. */
     goto noaction;
  }

  if (simd == True && where == UPD_Real && rr == True && ww == WAll) {
     /* simd is in real.  we want to capture changes made by it.  -->
        do nothing */
     goto noaction;
  }

  if (simd == True && where == UPD_Real && rr == False && ww == WAll) {
     /* simd is in real.  Insn creates new simd state.  --> leave in
        real */
     goto noaction;
  }

  if (simd == True && where == UPD_Both && rr == False && ww == WAll) {
     /* simd is in both.  Insn creates new simd state.  --> change
        state to Real. */
    narrow_Both_to_Real:
     eflags_state = UPD_Real;
     DIS_HEADER;
     return;    
  }

  if (simd == True && where == UPD_Both && rr == False && ww == WSome) {
     /* simd is in both.  Insn creates partial new simd state.  -->
        change state to Real.  No need to get, since Both holds. */
     goto narrow_Both_to_Real;
  }

  if (simd == True && where == UPD_Real && rr == False && ww == WSome) {
     /* simd is in real.  Insn creates new simd state.  --> leave in
        real */
     goto noaction;
  }

  if (simd == True && where == UPD_Both && rr == True && ww == WNone)
     /* want to read the simd flags, but already have a copy in real,
        and not planning to modify it --> do nothing. */
     goto noaction;

  ////////////////

  if (simd == False && where == UPD_Real && rr == False && ww == WNone)
     /* simd state is in real, but insn doesn't touch it --> do nothing */
     goto noaction;

  if (simd == False && where == UPD_Both && rr == False && ww == WNone)
     /* simd state is in both, insn doesn't touch it --> do nothing */
     goto noaction;

  if (simd == False && where == UPD_Both && rr == False && ww == WAll) {
     /* simd state is in both.  insn trashes real, therefore declare
        simd state only in simd. */
    narrow_Both_to_Simd:
     eflags_state = UPD_Simd;
     DIS_HEADER;
     return;    
  }

  if (simd == False && where == UPD_Both && rr == False && ww == WSome) {
     /* simd state is in both.  insn trashes real, therefore declare
        simd state only in simd. */
     goto narrow_Both_to_Simd;
  }

  if (simd == False && where == UPD_Real && rr == False && ww == WAll) {
     /* simd state is in real; we don't want simd state changed, but
        insn writes the flags.  Therefore have to copy back first. */
    put_flags_and_continue:
     emit_put_eflags();
     eflags_state = UPD_Simd;
     DIS_HEADER;
     return;
  }

  if (simd == False && where == UPD_Real && rr == False && ww == WSome) {
     /* simd state is in real; we don't want simd state changed, but
        insn writes the flags.  Therefore have to copy back first. */
     goto put_flags_and_continue;
  }

  goto unhandled;

 noaction:
   DIS_HEADER;
   return;

  //  if (simd == False && where == UPD_Simd && FL_NONE(rrr) && FL_SOME(www)) {
  //   return;
  //}

 unhandled:
  VG_(printf)("simd %s,  where %s,  read %s,  write %s\n",
	      simd ? "True " : "False",
	      (eflags_state == UPD_Simd ? "Simd" : (eflags_state == UPD_Real 
                                                      ? "Real" : "Both")),
	      rr ? "True " : "False",
              ww == WNone ? "None" : ww == WSome ? "Some" : "All "
	     );

  VG_(core_panic)("new_emit");
}
/* end UNUSED UNUSED UNUSED UNUSED UNUSED UNUSED UNUSED */
#endif


/* Call this before emitting each instruction.

   Arguments are:
   interacts_with_simd_flags: 
      if true, this instruction wants to interact (read and/or write)
         the simulated %EFLAGS state,
      otherwise it doesn't want to.
   use_flags: set of (real) flags the instruction uses
   set_flags: set of (real) flags the instruction sets
*/
__inline__ 
void VG_(new_emit) ( Bool interacts_with_simd_flags, 
                     FlagSet use_flags, FlagSet set_flags )
{
   Bool use, set;

   use = use_flags != FlagsEmpty 
         || (set_flags != FlagsEmpty && set_flags != FlagsOSZACP);
   set = set_flags != FlagsEmpty;

   if (0)
      VG_(printf)(
         "new_emit: state=%d interacts_with_simd_flags=%d "
         "use_flags=%x set_flags=%x\n",
         eflags_state, interacts_with_simd_flags, use_flags, set_flags);

   if (interacts_with_simd_flags) {
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x8B ); /* MOV Ev, Gv */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t0x%x(%s), %s\n", 
                   nameISize(sz), off, nameIReg(4,areg), nameIReg(sz,reg));
}

void VG_(emit_movv_reg_offregmem) ( Int sz, Int reg, Int off, Int areg )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x89 ); /* MOV Gv, Ev */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, 0x%x(%s)\n", 
                   nameISize(sz), nameIReg(sz,reg), off, nameIReg(4,areg));
}

static void emit_movv_regmem_reg ( Int sz, Int reg1, Int reg2 )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x8B ); /* MOV Ev, Gv */
   emit_amode_regmem_reg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t(%s), %s\n",
                   nameISize(sz),  nameIReg(4,reg1), nameIReg(sz,reg2));
}

static void emit_movv_reg_regmem ( Int sz, Int reg1, Int reg2 )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x89 ); /* MOV Gv, Ev */
   emit_amode_regmem_reg ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, (%s)\n", 
                   nameISize(sz), nameIReg(sz,reg1), nameIReg(4,reg2));
}

void VG_(emit_movv_reg_reg) ( Int sz, Int reg1, Int reg2 )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0x89 ); /* MOV Gv, Ev */
   VG_(emit_amode_ereg_greg) ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, %s\n", 
                   nameISize(sz), nameIReg(sz,reg1), nameIReg(sz,reg2));
}

void VG_(emit_nonshiftopv_lit_reg) ( Bool simd_flags, 
                                     Int sz, Opcode opc, 
                                     UInt lit, Int reg )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));

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

void VG_(emit_nonshiftopv_lit_offregmem) ( Bool simd_flags, Int sz, 
                                           Opcode opc, UInt lit, 
					   Int off, Int regmem )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
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

void VG_(emit_shiftopv_lit_reg) ( Bool simd_flags, 
                                  Int sz, Opcode opc, 
                                  UInt lit, Int reg )
{
   VG_(new_emit)(simd_flags, shiftop_use(opc), shiftop_set(opc));

   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0xC1 ); /* Grp2 Ib,Ev */
   VG_(emit_amode_ereg_greg) ( reg, mkGrp2opcode(opc) );
   VG_(emitB) ( lit );
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t$%d, %s\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz), 
                   lit, nameIReg(sz,reg));
}

static void emit_shiftopv_cl_stack0 ( Bool simd_flags, Int sz, Opcode opc )
{
   VG_(new_emit)(simd_flags, shiftop_use(opc), shiftop_set(opc));
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0xD3 ); /* Grp2 CL,Ev */
   VG_(emitB) ( mkModRegRM ( 1, mkGrp2opcode(opc), 4 ) );
   VG_(emitB) ( 0x24 ); /* a SIB, I think `d8(%esp)' */
   VG_(emitB) ( 0x00 ); /* the d8 displacement */
   if (dis)
      VG_(printf)("\n\t\t%s%c %%cl, 0(%%esp)\n",
                  VG_(name_UOpcode)(False,opc), nameISize(sz) );
}

static void emit_shiftopb_cl_stack0 ( Bool simd_flags, Opcode opc )
{
   VG_(new_emit)(simd_flags, shiftop_use(opc), shiftop_set(opc));
   VG_(emitB) ( 0xD2 ); /* Grp2 CL,Eb */
   VG_(emitB) ( mkModRegRM ( 1, mkGrp2opcode(opc), 4 ) );
   VG_(emitB) ( 0x24 ); /* a SIB, I think `d8(%esp)' */
   VG_(emitB) ( 0x00 ); /* the d8 displacement */
   if (dis)
      VG_(printf)("\n\t\t%s%c %%cl, 0(%%esp)\n",
                  VG_(name_UOpcode)(False,opc), nameISize(1) );
}

static void emit_nonshiftopv_offregmem_reg ( Bool simd_flags, Int sz, 
                                             Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 3 + mkPrimaryOpcode(opc) ); /* op Ev, Gv */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t0x%x(%s), %s\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz),
                   off, nameIReg(4,areg), nameIReg(sz,reg));
}

#if 0
/* evidently unused */
static void emit_nonshiftopv_reg_offregmem ( Bool simd_flags, Int sz, Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 1 + mkPrimaryOpcode(opc) ); /* op Gv, Ev */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t0x%s, %x(%s),\n", 
                   VG_(name_UOpcode)(False,opc), nameISize(sz),
                   nameIReg(sz,reg), off, nameIReg(4,areg));
}
#endif

void VG_(emit_nonshiftopv_reg_reg) ( Bool simd_flags, Int sz, Opcode opc, 
				     Int reg1, Int reg2 )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
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
#if 0
   if (lit == 0 && eflags_state != UPD_Real) {
      /* Only emit this for zeroing if it won't stomp flags */
      VG_(emit_nonshiftopv_reg_reg) ( False, sz, XOR, reg, reg );
      return;
   }
#endif
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   if (sz == 2) VG_(emitB) ( 0x66 );
   VG_(emitB) ( 0xB8+reg ); /* MOV imm, Gv */
   if (sz == 2) VG_(emitW) ( lit ); else VG_(emitL) ( lit );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t$0x%x, %s\n", 
                   nameISize(sz), lit, nameIReg(sz,reg));
}

void VG_(emit_unaryopv_reg) ( Bool simd_flags, Int sz, Opcode opc, Int reg )
{
   switch (opc) {
      case NEG:
	 VG_(new_emit)(simd_flags, FlagsEmpty, FlagsOSZACP);
	 if (sz == 2) VG_(emitB) ( 0x66 );
         VG_(emitB) ( 0xF7 );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp3opcode(NEG) );
         if (dis)
            VG_(printf)( "\n\t\tneg%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case NOT:
	 VG_(new_emit)(simd_flags, FlagsEmpty, FlagsEmpty);
	 if (sz == 2) VG_(emitB) ( 0x66 );
         VG_(emitB) ( 0xF7 );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp3opcode(NOT) );
         if (dis)
            VG_(printf)( "\n\t\tnot%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case DEC:
	 VG_(new_emit)(simd_flags, FlagsEmpty, FlagsOSZAP);
	 if (sz == 2) VG_(emitB) ( 0x66 );
         VG_(emitB) ( 0x48 + reg );
         if (dis)
            VG_(printf)( "\n\t\tdec%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case INC:
	 VG_(new_emit)(simd_flags, FlagsEmpty, FlagsOSZAP);
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x68 );
   VG_(emitL) ( int32 );
   if (dis)
      VG_(printf)("\n\t\tpushl $0x%x\n", int32 );
}  

void VG_(emit_pushl_lit8) ( Int lit8 )
{
   vg_assert(lit8 >= -128 && lit8 < 128);
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x6A );
   VG_(emitB) ( (UChar)((UInt)lit8) );
   if (dis)
      VG_(printf)("\n\t\tpushl $%d\n", lit8 );
}

void VG_(emit_cmpl_zero_reg) ( Bool simd_flags, Int reg )
{
   VG_(new_emit)(simd_flags, False, FlagsOSZACP);
   VG_(emitB) ( 0x83 );
   VG_(emit_amode_ereg_greg) ( reg, 7 /* Grp 3 opcode for CMP */ );
   VG_(emitB) ( 0x00 );
   if (dis)
      VG_(printf)("\n\t\tcmpl $0, %s\n", nameIReg(4,reg));
}

static void emit_swapl_reg_ECX ( Int reg )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x87 ); /* XCHG Gv,Ev */
   VG_(emit_amode_ereg_greg) ( reg, R_ECX );
   if (dis) 
      VG_(printf)("\n\t\txchgl %%ecx, %s\n", nameIReg(4,reg));
}

void VG_(emit_swapl_reg_EAX) ( Int reg )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x90 + reg ); /* XCHG Gv,eAX */
   if (dis) 
      VG_(printf)("\n\t\txchgl %%eax, %s\n", nameIReg(4,reg));
}

static void emit_swapl_reg_reg ( Int reg1, Int reg2 )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x87 ); /* XCHG Gv,Ev */
   VG_(emit_amode_ereg_greg) ( reg1, reg2 );
   if (dis) 
      VG_(printf)("\n\t\txchgl %s, %s\n", nameIReg(4,reg1), 
                  nameIReg(4,reg2));
}

static void emit_bswapl_reg ( Int reg )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( 0xC8 + reg ); /* BSWAP r32 */
   if (dis) 
      VG_(printf)("\n\t\tbswapl %s\n", nameIReg(4,reg));
}

static void emit_movl_reg_reg ( Int regs, Int regd )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x89 ); /* MOV Gv,Ev */
   VG_(emit_amode_ereg_greg) ( regd, regs );
   if (dis) 
      VG_(printf)("\n\t\tmovl %s, %s\n", nameIReg(4,regs), nameIReg(4,regd));
}

void VG_(emit_movv_lit_offregmem) ( Int sz, UInt lit, Int off, Int memreg )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0xC6 ); /* Grp11 Eb */
   VG_(emit_amode_offregmem_reg) ( off, memreg, 0 /* Grp11 subopcode for MOV */ );
   VG_(emitB) ( lit ); 
   if (dis)
      VG_(printf)( "\n\t\tmovb\t$0x%x, 0x%x(%s)\n", 
                   lit, off, nameIReg(4,memreg) );
}              
              
static void emit_nonshiftopb_offregmem_reg ( Bool simd_flags, Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 2 + mkPrimaryOpcode(opc) ); /* op Eb, Gb */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t0x%x(%s), %s\n", 
                   VG_(name_UOpcode)(False,opc), off, nameIReg(4,areg), 
                   nameIReg(1,reg));
}

static void emit_nonshiftopb_lit_offregmem ( Bool simd_flags, Opcode opc, 
                                             UInt lit, Int off, Int areg )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 0x80 );
   VG_(emit_amode_offregmem_reg) ( off, areg, mkGrp1opcode(opc) );
   VG_(emitB) ( lit );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t$0x%x, 0x%x(%s)\n", 
                   VG_(name_UOpcode)(False,opc), lit, off, nameIReg(4,areg));
}

#if 0
/* evidently unused */
static void emit_nonshiftopb_reg_offregmem ( Bool simd_flags, Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 0 + mkPrimaryOpcode(opc) ); /* op Gb, Eb */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t0x%s , %x(%s)\n", 
                   VG_(name_UOpcode)(False,opc), 
		   nameIReg(1,reg),
		   off, nameIReg(4,areg));
}
#endif

void VG_(emit_movb_reg_offregmem) ( Int reg, Int off, Int areg )
{
   /* Could do better when reg == %al. */
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x88 ); /* MOV G1, E1 */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovb\t%s, 0x%x(%s)\n", 
                   nameIReg(1,reg), off, nameIReg(4,areg));
}

static void emit_nonshiftopb_reg_reg ( Bool simd_flags, Opcode opc, 
                                       Int reg1, Int reg2 )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 2 + mkPrimaryOpcode(opc) ); /* op Eb, Gb */
   VG_(emit_amode_ereg_greg) ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t%s, %s\n", 
                   VG_(name_UOpcode)(False,opc),
                   nameIReg(1,reg1), nameIReg(1,reg2));
}

static void emit_movb_reg_regmem ( Int reg1, Int reg2 )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x88 ); /* MOV G1, E1 */
   emit_amode_regmem_reg ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmovb\t%s, (%s)\n", nameIReg(1,reg1), 
                                             nameIReg(4,reg2));
}

static void emit_nonshiftopb_lit_reg ( Bool simd_flags, Opcode opc, 
                                       UInt lit, Int reg )
{
   VG_(new_emit)(simd_flags, nonshiftop_use(opc), nonshiftop_set(opc));
   VG_(emitB) ( 0x80 ); /* Grp1 Ib,Eb */
   VG_(emit_amode_ereg_greg) ( reg, mkGrp1opcode(opc) );
   VG_(emitB) ( lit & 0x000000FF );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t$0x%x, %s\n", VG_(name_UOpcode)(False,opc),
                                             lit, nameIReg(1,reg));
}

static void emit_shiftopb_lit_reg ( Bool simd_flags, Opcode opc, 
                                    UInt lit, Int reg )
{
   VG_(new_emit)(simd_flags, shiftop_use(opc), shiftop_set(opc));
   VG_(emitB) ( 0xC0 ); /* Grp2 Ib,Eb */
   VG_(emit_amode_ereg_greg) ( reg, mkGrp2opcode(opc) );
   VG_(emitB) ( lit );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t$%d, %s\n", 
                   VG_(name_UOpcode)(False,opc),
                   lit, nameIReg(1,reg));
}

void VG_(emit_unaryopb_reg) ( Bool simd_flags, Opcode opc, Int reg )
{
   switch (opc) {
      case INC:
	 VG_(new_emit)(simd_flags, FlagsEmpty, FlagsOSZAP);
         VG_(emitB) ( 0xFE );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp4opcode(INC) );
         if (dis)
            VG_(printf)( "\n\t\tincb\t%s\n", nameIReg(1,reg));
         break;
      case DEC:
	 VG_(new_emit)(simd_flags, FlagsEmpty, FlagsOSZAP);
         VG_(emitB) ( 0xFE );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp4opcode(DEC) );
         if (dis)
            VG_(printf)( "\n\t\tdecb\t%s\n", nameIReg(1,reg));
         break;
      case NOT:
	 VG_(new_emit)(simd_flags, FlagsEmpty, FlagsEmpty);
         VG_(emitB) ( 0xF6 );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp3opcode(NOT) );
         if (dis)
            VG_(printf)( "\n\t\tnotb\t%s\n", nameIReg(1,reg));
         break;
      case NEG:
	 VG_(new_emit)(simd_flags, FlagsEmpty, FlagsOSZACP);
         VG_(emitB) ( 0xF6 );
         VG_(emit_amode_ereg_greg) ( reg, mkGrp3opcode(NEG) );
         if (dis)
            VG_(printf)( "\n\t\tnegb\t%s\n", nameIReg(1,reg));
         break;
      default: 
         VG_(core_panic)("VG_(emit_unaryopb_reg)");
   }
}

void VG_(emit_testb_lit_reg) ( Bool simd_flags, UInt lit, Int reg )
{
   VG_(new_emit)(simd_flags, FlagsEmpty, FlagsOSZACP);
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0xB6 ); /* MOVZBL */
   VG_(emit_amode_offregmem_reg) ( off, regmem, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovzbl\t0x%x(%s), %s\n", 
                   off, nameIReg(4,regmem), nameIReg(4,reg));
}

static void emit_movzbl_regmem_reg ( Int reg1, Int reg2 )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0xB6 ); /* MOVZBL */
   emit_amode_regmem_reg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\tmovzbl\t(%s), %s\n", nameIReg(4,reg1), 
                                               nameIReg(4,reg2));
}

void VG_(emit_movzwl_offregmem_reg) ( Int off, Int areg, Int reg )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0xB7 ); /* MOVZWL */
   VG_(emit_amode_offregmem_reg) ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovzwl\t0x%x(%s), %s\n",
                   off, nameIReg(4,areg), nameIReg(4,reg));
}

static void emit_movzwl_regmem_reg ( Int reg1, Int reg2 )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0xB7 ); /* MOVZWL */
   emit_amode_regmem_reg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\tmovzwl\t(%s), %s\n", nameIReg(4,reg1), 
                                             nameIReg(4,reg2));
}

/*----------------------------------------------------*/
/*--- FPU instruction emitters                     ---*/
/*----------------------------------------------------*/

static void emit_get_sse_state ( void )
{
   Int off = 4 * VGOFF_(m_ssestate);
   if (VG_(have_ssestate)) {
      VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
      VG_(emitB) ( 0x0F );
      VG_(emitB) ( 0xAE ); VG_(emitB) ( 0x8D ); /* fxrstor d32(%ebp) */
      VG_(emitL) ( off );
      if (dis)
         VG_(printf)("\n\t\tfxrstor\t%d(%%ebp)\n", off );
   } else {
      /* Not a SSE-capable CPU.  Just do frstor. */
      VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
      VG_(emitB) ( 0xDD ); VG_(emitB) ( 0xA5 ); /* frstor d32(%ebp) */
      VG_(emitL) ( off );
      if (dis)
         VG_(printf)("\n\t\tfrstor\t%d(%%ebp)\n", off );
   }
}

static void emit_put_sse_state ( void )
{
   Int off = 4 * VGOFF_(m_ssestate);
   if (VG_(have_ssestate)) {
      VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
      VG_(emitB) ( 0x0F );
      VG_(emitB) ( 0xAE ); VG_(emitB) ( 0x85 ); /* fxsave d32(%ebp) */
      VG_(emitL) ( off );
      if (dis)
         VG_(printf)("\n\t\tfxsave\t%d(%%ebp)\n", off );
   } else {
      /* Not a SSE-capable CPU.  Just do fnsave. */
      VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
      VG_(emitB) ( 0xDD ); VG_(emitB) ( 0xB5 ); /* fnsave d32(%ebp) */
      VG_(emitL) ( off );
      if (dis)
         VG_(printf)("\n\t\tfnsave\t%d(%%ebp)\n", off );
   }
}

static void emit_fpu_no_mem ( FlagSet uses_sflags, 
                              FlagSet sets_sflags,
			      UChar first_byte, 
                              UChar second_byte )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   if (dis)
      VG_(printf)("\n\t\tfpu-0x%x:0x%x\n", 
                  (UInt)first_byte, (UInt)second_byte );
}

static void emit_fpu_regmem ( FlagSet uses_sflags, 
                              FlagSet sets_sflags,
			      UChar first_byte, 
                              UChar second_byte_masked, 
                              Int reg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   emit_amode_regmem_reg ( reg, second_byte_masked >> 3 );
   if (dis)
      VG_(printf)("\n\t\tfpu-0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte_masked,
                  nameIReg(4,reg) );
}

static void emit_MMX2_regmem ( FlagSet uses_sflags, 
                               FlagSet sets_sflags,
			       UChar first_byte, 
                               UChar second_byte, 
                               Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( first_byte );
   second_byte &= 0x38; /* mask out mod and rm fields */
   emit_amode_regmem_reg ( ireg, second_byte >> 3 );
   if (dis)
      VG_(printf)("\n\t\tmmx2-0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte,
                  nameIReg(4,ireg) );
}

static void emit_SSE2a ( FlagSet uses_sflags, 
                         FlagSet sets_sflags,
                         UChar first_byte, 
                         UChar second_byte, 
			 UChar third_byte,
                         Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   third_byte &= 0x38; /* mask out mod and rm fields */
   emit_amode_regmem_reg ( ireg, third_byte >> 3 );
   if (dis)
      VG_(printf)("\n\t\tsse2a-0x%x:0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte, (UInt)third_byte,
                  nameIReg(4,ireg) );
}

static void emit_SSE2a1 ( FlagSet uses_sflags, 
                          FlagSet sets_sflags,
                          UChar first_byte, 
                          UChar second_byte, 
 			  UChar third_byte,
 			  UChar fourth_byte,
                          Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   third_byte &= 0x38; /* mask out mod and rm fields */
   emit_amode_regmem_reg ( ireg, third_byte >> 3 );
   VG_(emitB) ( fourth_byte );
   if (dis)
      VG_(printf)("\n\t\tsse2a1-0x%x:0x%x:0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte, 
                  (UInt)third_byte, (UInt)fourth_byte,
                  nameIReg(4,ireg) );
}

static void emit_SSE3a ( FlagSet uses_sflags, 
                         FlagSet sets_sflags,
                         UChar first_byte, 
                         UChar second_byte, 
			 UChar third_byte,
			 UChar fourth_byte,
                         Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   fourth_byte &= 0x38; /* mask out mod and rm fields */
   emit_amode_regmem_reg ( ireg, fourth_byte >> 3 );
   if (dis)
      VG_(printf)("\n\t\tsse3a-0x%x:0x%x:0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte, 
                  (UInt)third_byte, (UInt)fourth_byte,
                  nameIReg(4,ireg) );
}

static void emit_SSE3e ( FlagSet uses_sflags, 
                         FlagSet sets_sflags,
                         UChar first_byte, 
                         UChar second_byte, 
                         UChar third_byte,
			 UChar fourth_byte,
                         Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   fourth_byte &= 0x38; /* mask out mod and rm fields */
   fourth_byte |= 0xC0; /* set top two bits: mod = 11b */
   fourth_byte |= (ireg & 7); /* patch in our ireg */
   VG_(emitB) ( fourth_byte );
   if (dis)
      VG_(printf)(
         "\n\t\tsse3e--0x%x:0x%x:0x%x:0x%x-(%s)\n",
         (UInt)first_byte, (UInt)second_byte, 
         (UInt)third_byte, (UInt)fourth_byte,
         nameIReg(4,ireg) 
      );
}

static void emit_SSE3e1 ( FlagSet uses_sflags, 
                          FlagSet sets_sflags,
                          UChar first_byte, 
                          UChar second_byte, 
 			  UChar third_byte,
                          UChar fourth_byte,
			  UChar fifth_byte,
                          Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   fourth_byte &= 0x38; /* mask out mod and rm fields */
   fourth_byte |= 0xC0; /* set top two bits: mod = 11b */
   fourth_byte |= (ireg & 7); /* patch in our ireg */
   VG_(emitB) ( fourth_byte );
   VG_(emitB) ( fifth_byte );
   if (dis)
      VG_(printf)(
         "\n\t\tsse3e1--0x%x:0x%x:0x%x:0x%x:0x%x-(%s)\n", 
         (UInt)first_byte, (UInt)second_byte, 
         (UInt)third_byte, (UInt)fourth_byte, (UInt)fifth_byte,
         nameIReg(4,ireg) 
      );
}

static void emit_SSE3g1 ( FlagSet uses_sflags, 
                          FlagSet sets_sflags,
                          UChar first_byte, 
                          UChar second_byte, 
                          UChar third_byte,
                          UChar fourth_byte,
			  UChar fifth_byte,
                          Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   fourth_byte &= 0xC7; /* mask out reg field */
   fourth_byte |= 0xC0; /* set top two bits: mod = 11b */
   fourth_byte |= ((ireg & 7) << 3); /* patch in our ireg */
   VG_(emitB) ( fourth_byte );
   VG_(emitB) ( fifth_byte );
   if (dis)
      VG_(printf)(
         "\n\t\tsse3g1_reg_wr--0x%x:0x%x:0x%x:0x%x:0x%x-(%s)\n", 
         (UInt)first_byte, (UInt)second_byte, 
         (UInt)third_byte, (UInt)fourth_byte, (UInt)fifth_byte,
         nameIReg(4,ireg) 
      );
}

static void emit_SSE3g ( FlagSet uses_sflags, 
                         FlagSet sets_sflags,
                         UChar first_byte, 
                         UChar second_byte, 
 			 UChar third_byte,
                         UChar fourth_byte,
                         Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   fourth_byte &= 0xC7; /* mask out reg field */
   fourth_byte |= 0xC0; /* set top two bits: mod = 11b */
   fourth_byte |= ((ireg & 7) << 3); /* patch in our ireg */
   VG_(emitB) ( fourth_byte );
   if (dis)
      VG_(printf)(
         "\n\t\tsse3g--0x%x:0x%x:0x%x:0x%x-(%s)\n", 
         (UInt)first_byte, (UInt)second_byte, 
         (UInt)third_byte, (UInt)fourth_byte,
         nameIReg(4,ireg) 
      );
}

static void emit_SSE4 ( FlagSet uses_sflags, 
                        FlagSet sets_sflags,
                        UChar first_byte, 
                        UChar second_byte, 
		        UChar third_byte,
                        UChar fourth_byte )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   VG_(emitB) ( fourth_byte );
   if (dis)
      VG_(printf)("\n\t\tsse4-0x%x:0x%x:0x%x:0x%x\n", 
                  (UInt)first_byte, (UInt)second_byte, 
                  (UInt)third_byte, (UInt)fourth_byte );
}

static void emit_SSE5 ( FlagSet uses_sflags, 
                        FlagSet sets_sflags,
                        UChar first_byte, 
                        UChar second_byte, 
			UChar third_byte,
			UChar fourth_byte,
			UChar fifth_byte )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   VG_(emitB) ( fourth_byte );
   VG_(emitB) ( fifth_byte );
   if (dis)
      VG_(printf)("\n\t\tsse5-0x%x:0x%x:0x%x:0x%x:0x%x\n", 
                  (UInt)first_byte, (UInt)second_byte, 
                  (UInt)third_byte, (UInt)fourth_byte,
                  (UInt)fifth_byte );
}

static void emit_SSE3 ( FlagSet uses_sflags, 
                        FlagSet sets_sflags,
                        UChar first_byte, 
                        UChar second_byte, 
                        UChar third_byte )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   if (dis)
      VG_(printf)("\n\t\tsse3-0x%x:0x%x:0x%x\n", 
                  (UInt)first_byte, (UInt)second_byte, 
                  (UInt)third_byte );
}

static void emit_SSE3ag_MemRd_RegWr ( FlagSet uses_sflags, 
                                      FlagSet sets_sflags,
                                      UChar first_byte, 
                                      UChar second_byte, 
                                      UChar third_byte,
				      Int addr_reg,
				      Int dest_reg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   /* 4th byte can be completely synthesised from addr_reg and
      dest_reg. */
   emit_amode_regmem_reg ( addr_reg, dest_reg );
   if (dis)
      VG_(printf)("\n\t\tsse3ag_mem_rd_reg_wr-0x%x:0x%x:0x%x(addr=%s, dest=%s)\n", 
                  (UInt)first_byte, (UInt)second_byte, 
            	  (UInt)third_byte, nameIReg(4, addr_reg), 
                                    nameIReg(4, dest_reg));
}

static void emit_MMX2_reg_to_mmxreg ( FlagSet uses_sflags, 
                                      FlagSet sets_sflags,
			              UChar first_byte, 
                                      UChar second_byte, 
                                      Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( first_byte );
   second_byte &= 0x38; /* mask out mod and rm fields */
   second_byte |= 0xC0; /* set top two bits: mod = 11b */
   second_byte |= (ireg & 7); /* patch in our ireg */
   VG_(emitB) ( second_byte );
   if (dis)
      VG_(printf)("\n\t\tmmx2:reg-to-mmxreg--0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte,
                  nameIReg(4,ireg) );
}

static void emit_MMX2_mmxreg_to_reg ( FlagSet uses_sflags, 
                                      FlagSet sets_sflags,
			              UChar first_byte, 
                                      UChar second_byte, 
                                      Int ireg )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( first_byte );
   second_byte &= 0x38; /* mask out mod and rm fields */
   second_byte |= 0xC0; /* set top two bits: mod = 11b */
   second_byte |= (ireg & 7); /* patch in our ireg */
   VG_(emitB) ( second_byte );
   if (dis)
      VG_(printf)("\n\t\tmmx2:mmxreg-to-reg--0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte,
                  nameIReg(4,ireg) );
}

static void emit_MMX3_no_mem ( FlagSet uses_sflags, 
                               FlagSet sets_sflags,
			       UChar first_byte, 
                               UChar second_byte,
                               UChar third_byte )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   VG_(emitB) ( third_byte );
   if (dis)
      VG_(printf)("\n\t\tmmx3-0x%x:0x%x:0x%x\n", 
                  (UInt)first_byte, (UInt)second_byte, (UInt)third_byte );
}

static void emit_MMX2_no_mem ( FlagSet uses_sflags, 
                               FlagSet sets_sflags,
			       UChar first_byte, 
                               UChar second_byte )
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( first_byte );
   VG_(emitB) ( second_byte );
   if (dis)
      VG_(printf)("\n\t\tmmx2-0x%x:0x%x\n", 
                  (UInt)first_byte, (UInt)second_byte );
}

static void emit_MMX1_no_mem ( FlagSet uses_sflags, 
                               FlagSet sets_sflags,
			       UChar first_byte ) 
{
   VG_(new_emit)(True, uses_sflags, sets_sflags);
   VG_(emitB) ( 0x0F );
   VG_(emitB) ( first_byte );
   if (dis)
      VG_(printf)("\n\t\tmmx1-0x%x\n", 
                  (UInt)first_byte );
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
         
static 
void emit_call_star_EBP_off ( Bool simd_flags, Int byte_off, 
                              FlagSet use_flag, FlagSet set_flag )
{
   /* Used for helpers which expect to see Simd flags in Real flags */
   VG_(new_emit)(simd_flags, use_flag, set_flag);

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

#if 0
/* evidently unused */
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
#endif

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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x8A );
   VG_(emitB) ( 0x44 );
   VG_(emitB) ( 0x24 );
   VG_(emitB) ( 0x00 );
   if (dis)
      VG_(printf)( "\n\t\tmovb 0(%%esp), %%al\n" );
}

/* Jump target states */
#define TGT_UNDEF	(1 << 16)
#define TGT_FORWARD	(2 << 16)
#define TGT_BACKWARD	(3 << 16)

static inline Int tgt_state(Int tgt)
{
   return tgt & 0xffff0000;
}

static inline Int tgt_addr(Int tgt)
{
   return tgt & 0x0000ffff;
}

static inline Int mk_tgt(Int state, Int addr)
{
   vg_assert(state == TGT_UNDEF 
             || state == TGT_FORWARD || state == TGT_BACKWARD);
   vg_assert((addr & 0xffff0000) == 0);

   return state | addr;
}
   
void VG_(init_target) ( Int *tgt )
{
   *tgt = TGT_UNDEF;
}

void VG_(target_back) ( Int *tgt )
{
   vg_assert(tgt_state(*tgt) == TGT_UNDEF);

   *tgt = mk_tgt(TGT_BACKWARD, emitted_code_used);
}

void VG_(target_forward)  ( Int *tgt )
{
   Int delta;

   vg_assert(tgt_state(*tgt) == TGT_FORWARD ||
	     tgt_state(*tgt) == TGT_UNDEF);
   
   if (tgt_state(*tgt) == TGT_UNDEF)
      return;			/* target not used */
   
   delta = emitted_code_used - (tgt_addr(*tgt) + 1);
   vg_assert(delta >= -128 && delta <= 127);
   vg_assert(tgt_addr(*tgt) >= 0);
   vg_assert(tgt_addr(*tgt) < emitted_code_used);
   emitted_code[tgt_addr(*tgt)] = delta;
   if (dis)
      VG_(printf)("(target to jump site %d; delta: %d)\n", 
                  tgt_addr(*tgt), delta);
}

void VG_(emit_target_delta) ( Int *tgt )
{
   vg_assert(tgt_state(*tgt) == TGT_UNDEF ||
	     tgt_state(*tgt) == TGT_BACKWARD);

   if (tgt_state(*tgt) == TGT_UNDEF) {
      /* forward jump */
      *tgt = mk_tgt(TGT_FORWARD, emitted_code_used);
      VG_(emitB) (0x00);
   } else {
      /* backward jump */
      Int delta = emitted_code_used - (tgt_addr(*tgt) + 1);
      vg_assert(delta >= -128 && delta <= 127);
      VG_(emitB) (delta);
   }
}


/* Emit a jump short with an 8-bit signed offset.  Note that the
   offset is that which should be added to %eip once %eip has been
   advanced over this insn.  */
void VG_(emit_jcondshort_delta) ( Bool simd_flags, Condcode cond, Int delta )
{
   vg_assert(delta >= -128 && delta <= 127);
   VG_(new_emit)(simd_flags, FlagsOSZCP, FlagsEmpty);
   VG_(emitB) ( 0x70 + (UInt)cond );
   VG_(emitB) ( (UChar)delta );
   if (dis)
      VG_(printf)( "\n\t\tj%s-8\t%%eip+%d\n", 
                   VG_(name_UCondcode)(cond), delta );
}

/* Same as above, but defers emitting the delta  */
void VG_(emit_jcondshort_target) ( Bool simd, Condcode cond, Int *tgt )
{
   VG_(new_emit)(simd, FlagsOSZCP, FlagsEmpty);
   VG_(emitB) ( 0x70 + (UInt)cond );
   VG_(emit_target_delta) (tgt);
   if (dis)
      VG_(printf)( "\n\t\tj%s-8\t%%eip+(%d)\n", 
                   VG_(name_UCondcode)(cond), tgt_addr(*tgt) );
}



static void emit_setb_reg ( Bool simd, Int reg, Condcode cond )
{
   VG_(new_emit)(simd, FlagsOSZCP, FlagsEmpty);
   VG_(emitB) ( 0x0F ); VG_(emitB) ( 0x90 + (UChar)cond );
   VG_(emit_amode_ereg_greg) ( reg, 0 );
   if (dis)
      VG_(printf)("\n\t\tset%s %s\n", 
                  VG_(name_UCondcode)(cond), nameIReg(1,reg));
}

static void emit_ret ( void )
{
   maybe_emit_put_eflags(); /* make sure flags are stored */
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);

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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x60 ); /* PUSHAL */
   if (dis)
      VG_(printf)("\n\t\tpushal\n");
}

void VG_(emit_popal) ( void )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x61 ); /* POPAL */
   if (dis)
      VG_(printf)("\n\t\tpopal\n");
}

static void emit_lea_litreg_reg ( UInt lit, Int regmem, Int reg )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
   VG_(emitB) ( 0x8D ); /* LEA M,Gv */
   VG_(emit_amode_offregmem_reg) ( (Int)lit, regmem, reg );
   if (dis)
      VG_(printf)("\n\t\tleal 0x%x(%s), %s\n",
                  lit, nameIReg(4,regmem), nameIReg(4,reg) );
}

static void emit_lea_sib_reg ( UInt lit, Int scale,
			       Int regbase, Int regindex, Int reg )
{
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
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
   VG_(new_emit)(False, FlagsEmpty, FlagsEmpty);
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
   UInt i;
   Char buf[100];

   for (i = 0; i < VG_(n_compact_helpers); i++)
      if (VG_(compact_helper_addrs)[i] == a)
         return VG_(compact_helper_offsets)[i];
   for (i = 0; i < VG_(n_noncompact_helpers); i++)
      if (VG_(noncompact_helper_addrs)[i] == a)
         return VG_(noncompact_helper_offsets)[i];

   /* Shouldn't get here */
   VG_(get_fnname)   ( a, buf, 100 );

   VG_(printf)(
      "\nCouldn't find offset of helper from its address (%p: %s).\n"
      "A helper function probably used hasn't been registered?\n\n", a, buf);

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
		       Bool simd_flags, FlagSet use_flags, FlagSet set_flags )
{
   vg_assert(word_offset >= 0);
   vg_assert(word_offset < VG_BASEBLOCK_WORDS);
   if (ensure_shortform) {
      vg_assert(word_offset < 32);
   }
   emit_call_star_EBP_off ( simd_flags, 4 * word_offset, use_flags, set_flags );
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
static void synth_nonshiftop_lit_reg ( Bool simd_flags,
                                       Opcode opcode, Int size, 
                                       UInt lit, Int reg );

static void synth_jcond_lit ( Condcode cond, 
                              Addr addr,
                              Bool eax_trashable )
{
   UInt mask;
   Bool simd;
   Int  tgt, tgt2, tgt_jump;

   VG_(init_target)(&tgt);
   VG_(init_target)(&tgt2);
   VG_(init_target)(&tgt_jump);

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

         case CondLE:		/*  Z || S != O ->  S || !P */
         case CondNLE:		/* !Z && S == O -> !S &&  P */
            vg_assert(eax_trashable);

            VG_(emit_movv_offregmem_reg)
               ( 4, VGOFF_(m_eflags) * 4, R_EBP, R_EAX );
            /* eax == %EFLAGS */

	    VG_(emit_nonshiftopv_lit_reg)
               ( False, 4, AND, EFlagO|EFlagS|EFlagZ, R_EAX );
	    /* eax just contains OF, SF and ZF */

            VG_(emit_shiftopv_lit_reg)( False, 4, ROR, 7, R_EAX );
            /* eax has OF and SF in lower 8 bits, and ZF in MSB */

	    /* actually set the real cpu flags, since ROR changes
	       neither P nor Z */
	    VG_(emit_nonshiftopv_reg_reg)( False, 4, OR, R_EAX, R_EAX );

	    if (cond == CondLE) {
	       /* test Z */
	       VG_(emit_jcondshort_target)(False, CondS, &tgt_jump);
	       /* test OF != SF */
	       cond = CondP;
	    } else {
	       /* test Z */
	       VG_(emit_jcondshort_target)(False, CondS, &tgt2);
	       /* test OF == SF */
	       cond = CondNP;
	    }
            break;

         case CondL: 
         case CondNL:
            vg_assert(eax_trashable);

            VG_(emit_movv_offregmem_reg)
               ( 4, VGOFF_(m_eflags) * 4, R_EBP, R_EAX );
            /* eax == %EFLAGS */

            VG_(emit_shiftopv_lit_reg)( False, 4, SHR, 7, R_EAX );
            /* eax has OF and SF in lower byte */

	    VG_(emit_testb_lit_reg) ( False, 0x11, R_EAX);
	    /* PF = OF == SF */

	    /* Testing P now is OK since SHR sets it */
            if (cond == CondL) cond = CondP; else cond = CondNP;
	    break;

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

         case CondO:
         case CondNO:
            mask = EFlagO; goto simple; /* O=1        */

         default: 
            VG_(printf)("synth_jcond_lit: unhandled simd case %d (%s)\n", 
                        (Int)cond, VG_(name_UCondcode)(cond) );
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

   VG_(emit_jcondshort_target) ( simd, cond, &tgt );

   VG_(target_forward)(&tgt_jump);
   synth_jmp_lit ( addr, JmpBoring );

   VG_(target_forward)(&tgt);
   VG_(target_forward)(&tgt2);
}



static void synth_jmp_ifzero_reg_lit ( Int reg, Addr addr )
{
   Int tgt;
 
   VG_(init_target)(&tgt);
 
   VG_(emit_cmpl_zero_reg) ( False, reg );

   VG_(emit_jcondshort_target) ( False, CondNZ, &tgt );
   synth_jmp_lit ( addr, JmpBoring );
 
   VG_(target_forward)(&tgt);
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


static void synth_unaryop_reg ( Bool simd_flags,
                                Opcode opcode, Int size,
                                Int reg )
{
   /* NB! opcode is a uinstr opcode, not an x86 one! */
   switch (size) {
      case 4: VG_(emit_unaryopv_reg) ( simd_flags, 4, opcode, reg );
              break;
      case 2: VG_(emit_unaryopv_reg) ( simd_flags, 2, opcode, reg );
              break;
      case 1: if (reg < 4) {
                 VG_(emit_unaryopb_reg) ( simd_flags, opcode, reg );
              } else {
                 VG_(emit_swapl_reg_EAX) ( reg );
                 VG_(emit_unaryopb_reg) ( simd_flags, opcode, R_AL );
                 VG_(emit_swapl_reg_EAX) ( reg );
              }
              break;
      default: VG_(core_panic)("synth_unaryop_reg");
   }
}



static void synth_nonshiftop_reg_reg ( Bool simd_flags, 
                                       Opcode opcode, Int size, 
                                       Int reg1, Int reg2 )
{
   /* NB! opcode is a uinstr opcode, not an x86 one! */
   switch (size) {
      case 4: VG_(emit_nonshiftopv_reg_reg) ( simd_flags, 4, opcode, reg1, reg2 );
              break;
      case 2: VG_(emit_nonshiftopv_reg_reg) ( simd_flags, 2, opcode, reg1, reg2 );
              break;
      case 1: { /* Horrible ... */
         Int s1, s2;
         /* Choose s1 and s2 to be x86 regs which we can talk about the
            lowest 8 bits, ie either %eax, %ebx, %ecx or %edx.  Make
            sure s1 != s2 and that neither of them equal either reg1 or
            reg2. Then use them as temporaries to make things work. */
         if (reg1 < 4 && reg2 < 4) {
            emit_nonshiftopb_reg_reg(simd_flags, opcode, reg1, reg2); 
            break;
         }
         for (s1 = 0; s1 == reg1 || s1 == reg2; s1++) ;
         if (reg1 >= 4 && reg2 < 4) {
            emit_swapl_reg_reg ( reg1, s1 );
            emit_nonshiftopb_reg_reg(simd_flags, opcode, s1, reg2);
            emit_swapl_reg_reg ( reg1, s1 );
            break;
         }
         for (s2 = 0; s2 == reg1 || s2 == reg2 || s2 == s1; s2++) ;
         if (reg1 < 4 && reg2 >= 4) {
            emit_swapl_reg_reg ( reg2, s2 );
            emit_nonshiftopb_reg_reg(simd_flags, opcode, reg1, s2);
            emit_swapl_reg_reg ( reg2, s2 );
            break;
         }
         if (reg1 >= 4 && reg2 >= 4 && reg1 != reg2) {
            emit_swapl_reg_reg ( reg1, s1 );
            emit_swapl_reg_reg ( reg2, s2 );
            emit_nonshiftopb_reg_reg(simd_flags, opcode, s1, s2);
            emit_swapl_reg_reg ( reg1, s1 );
            emit_swapl_reg_reg ( reg2, s2 );
            break;
         }
         if (reg1 >= 4 && reg2 >= 4 && reg1 == reg2) {
            emit_swapl_reg_reg ( reg1, s1 );
            emit_nonshiftopb_reg_reg(simd_flags, opcode, s1, s1);
            emit_swapl_reg_reg ( reg1, s1 );
            break;
         }
         VG_(core_panic)("synth_nonshiftopb_reg_reg");
      }
      default: VG_(core_panic)("synth_nonshiftop_reg_reg");
   }
}

#if 0
/* evidently unused */
static void synth_nonshiftop_reg_offregmem (
   Bool simd_flags,
   Opcode opcode, Int size, 
   Int off, Int areg, Int reg )
{
   switch (size) {
      case 4: 
         emit_nonshiftopv_reg_offregmem ( simd_flags, 4, opcode, off, areg, reg ); 
         break;
      case 2: 
         emit_nonshiftopv_reg_offregmem ( simd_flags, 2, opcode, off, areg, reg ); 
         break;
      case 1: 
         if (reg < 4) {
            emit_nonshiftopb_reg_offregmem ( simd_flags, opcode, off, areg, reg );
         } else {
            VG_(emit_swapl_reg_EAX) ( reg );
            emit_nonshiftopb_reg_offregmem ( simd_flags, opcode, off, areg, R_AL );
            VG_(emit_swapl_reg_EAX) ( reg );
         }
         break;
      default: 
         VG_(core_panic)("synth_nonshiftop_reg_offregmem");
   }
}
#endif

static void synth_nonshiftop_offregmem_reg ( 
   Bool simd_flags,
   Opcode opcode, Int size, 
   Int off, Int areg, Int reg )
{
   switch (size) {
      case 4: 
         emit_nonshiftopv_offregmem_reg ( simd_flags, 4, opcode, off, areg, reg ); 
         break;
      case 2: 
         emit_nonshiftopv_offregmem_reg ( simd_flags, 2, opcode, off, areg, reg ); 
         break;
      case 1: 
         if (reg < 4) {
            emit_nonshiftopb_offregmem_reg ( simd_flags, opcode, off, areg, reg );
         } else {
            VG_(emit_swapl_reg_EAX) ( reg );
            emit_nonshiftopb_offregmem_reg ( simd_flags, opcode, off, areg, R_AL );
            VG_(emit_swapl_reg_EAX) ( reg );
         }
         break;
      default: 
         VG_(core_panic)("synth_nonshiftop_offregmem_reg");
   }
}


static void synth_nonshiftop_lit_reg ( Bool simd_flags,
                                       Opcode opcode, Int size, 
                                       UInt lit, Int reg )
{
   switch (size) {
      case 4: VG_(emit_nonshiftopv_lit_reg) ( simd_flags, 4, opcode, lit, reg );
              break;
      case 2: VG_(emit_nonshiftopv_lit_reg) ( simd_flags, 2, opcode, lit, reg );
              break;
      case 1: if (reg < 4) {
                 emit_nonshiftopb_lit_reg ( simd_flags, opcode, lit, reg );
              } else {
                 VG_(emit_swapl_reg_EAX) ( reg );
                 emit_nonshiftopb_lit_reg ( simd_flags, opcode, lit, R_AL );
                 VG_(emit_swapl_reg_EAX) ( reg );
              }
              break;
      default: VG_(core_panic)("synth_nonshiftop_lit_reg");
   }
}

static void synth_nonshiftop_lit_offregmem ( Bool simd_flags,
					     Opcode opcode, Int size, 
					     UInt lit, Int off, Int regmem )
{
   switch (size) {
      case 4: VG_(emit_nonshiftopv_lit_offregmem) ( simd_flags, 4, opcode, lit, off, regmem );
              break;
      case 2: VG_(emit_nonshiftopv_lit_offregmem) ( simd_flags, 2, opcode, lit, off, regmem );
              break;
      case 1: emit_nonshiftopb_lit_offregmem ( simd_flags, opcode, lit, off, regmem );
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


static void synth_shiftop_reg_reg ( Bool simd_flags,
                                    Opcode opcode, Int size, 
                                    Int regs, Int regd )
{
   synth_push_reg ( size, regd );
   if (regs != R_ECX) emit_swapl_reg_ECX ( regs );
   switch (size) {
      case 4: emit_shiftopv_cl_stack0 ( simd_flags, 4, opcode ); break;
      case 2: emit_shiftopv_cl_stack0 ( simd_flags, 2, opcode ); break;
      case 1: emit_shiftopb_cl_stack0 ( simd_flags, opcode ); break;
      default: VG_(core_panic)("synth_shiftop_reg_reg");
   }
   if (regs != R_ECX) emit_swapl_reg_ECX ( regs );
   synth_pop_reg ( size, regd );
}


static void synth_shiftop_lit_reg ( Bool simd_flags,
                                    Opcode opcode, Int size, 
                                    UInt lit, Int reg )
{
   switch (size) {
      case 4: VG_(emit_shiftopv_lit_reg) ( simd_flags, 4, opcode, lit, reg );
              break;
      case 2: VG_(emit_shiftopv_lit_reg) ( simd_flags, 2, opcode, lit, reg );
              break;
      case 1: if (reg < 4) {
                 emit_shiftopb_lit_reg ( simd_flags, opcode, lit, reg );
              } else {
                 VG_(emit_swapl_reg_EAX) ( reg );
                 emit_shiftopb_lit_reg ( simd_flags, opcode, lit, R_AL );
                 VG_(emit_swapl_reg_EAX) ( reg );
              }
              break;
      default: VG_(core_panic)("synth_shiftop_lit_reg");
   }
}


static void synth_setb_reg ( Bool simd, Int reg, Condcode cond )
{
   if (reg < 4) {
      emit_setb_reg ( simd, reg, cond );
   } else {
      VG_(emit_swapl_reg_EAX) ( reg );
      emit_setb_reg ( simd, R_AL, cond );
      VG_(emit_swapl_reg_EAX) ( reg );
   }
}


static void synth_MMX2_regmem ( Bool uses_flags, Bool sets_flags,
 			        UChar first_byte,
                                UChar second_byte, 
                                Int ireg )
{
   emit_MMX2_regmem ( uses_flags, sets_flags, 
                      first_byte, second_byte, ireg );
}


static void synth_MMX2_reg_to_mmxreg ( Bool uses_flags, Bool sets_flags,
                                       UChar first_byte,
                                       UChar second_byte, 
                                       Int ireg )
{
   emit_MMX2_reg_to_mmxreg ( uses_flags, sets_flags,
                             first_byte, second_byte, ireg );
}

static void synth_MMX2_mmxreg_to_reg ( Bool uses_flags, Bool sets_flags,
                                       UChar first_byte,
                                       UChar second_byte, 
                                       Int ireg )
{
   emit_MMX2_mmxreg_to_reg ( uses_flags, sets_flags,
                             first_byte, second_byte, ireg );
}

static void synth_MMX2_no_mem ( Bool uses_flags, Bool sets_flags,
			        UChar first_byte,
                                UChar second_byte )
{
   emit_MMX2_no_mem ( uses_flags, sets_flags, first_byte, second_byte );
}


static void synth_MMX3_no_mem ( Bool uses_flags, Bool sets_flags,
			        UChar first_byte,
                                UChar second_byte,
                                UChar third_byte )
{
   emit_MMX3_no_mem ( uses_flags, sets_flags, 
                      first_byte, second_byte, third_byte );
}


static void synth_MMX1_no_mem ( Bool uses_flags, Bool sets_flags,
			        UChar first_byte )
{
   emit_MMX1_no_mem ( uses_flags, sets_flags, first_byte );
}


static void synth_fpu_regmem ( Bool uses_flags, Bool sets_flags,
			       UChar first_byte,
                               UChar second_byte_masked, 
                               Int reg )
{
   emit_fpu_regmem ( uses_flags, sets_flags, 
                     first_byte, second_byte_masked, reg );
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
   Int tgt;

   VG_(init_target)(&tgt);

   VG_(emit_jcondshort_target) ( True, invertCondition(cond), &tgt);
   emit_movl_reg_reg ( src, dst );

   VG_(target_forward)(&tgt);
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

UInt VG_(get_archreg) ( UInt arch )
{
   switch (arch) {
      case R_EAX: return VG_(baseBlock)[ VGOFF_(m_eax) ];
      case R_ECX: return VG_(baseBlock)[ VGOFF_(m_ecx) ];
      case R_EDX: return VG_(baseBlock)[ VGOFF_(m_edx) ];
      case R_EBX: return VG_(baseBlock)[ VGOFF_(m_ebx) ];
      case R_ESP: return VG_(baseBlock)[ VGOFF_(m_esp) ];
      case R_EBP: return VG_(baseBlock)[ VGOFF_(m_ebp) ];
      case R_ESI: return VG_(baseBlock)[ VGOFF_(m_esi) ];
      case R_EDI: return VG_(baseBlock)[ VGOFF_(m_edi) ];
      default:    VG_(core_panic)( "get_archreg");
   }
}

UInt VG_(get_thread_archreg) ( ThreadId tid, UInt arch )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   switch (arch) {
      case R_EAX: return tst->m_eax;
      case R_ECX: return tst->m_ecx;
      case R_EDX: return tst->m_edx;
      case R_EBX: return tst->m_ebx;
      case R_ESP: return tst->m_esp;
      case R_EBP: return tst->m_ebp;
      case R_ESI: return tst->m_esi;
      case R_EDI: return tst->m_edi;
      default:    VG_(core_panic)( "get_thread_archreg");
   }
}

/* Return the baseBlock index for the specified shadow register */
static Int shadow_reg_index ( Int arch )
{
   switch (arch) {
      case R_EAX: return VGOFF_(sh_eax);
      case R_ECX: return VGOFF_(sh_ecx);
      case R_EDX: return VGOFF_(sh_edx);
      case R_EBX: return VGOFF_(sh_ebx);
      case R_ESP: return VGOFF_(sh_esp);
      case R_EBP: return VGOFF_(sh_ebp);
      case R_ESI: return VGOFF_(sh_esi);
      case R_EDI: return VGOFF_(sh_edi);
      default:    VG_(core_panic)( "shadow_reg_index");
   }
}

/* Return the byte offset from %ebp (ie, into baseBlock)
   for the specified shadow register */
Int VG_(shadow_reg_offset) ( Int arch )
{
   return 4 * shadow_reg_index ( arch );
}

Int VG_(shadow_flags_offset) ( void )
{
   return 4 * VGOFF_(sh_eflags);
}

/* Accessing shadow arch. registers */
UInt VG_(get_shadow_archreg) ( UInt archreg )
{
   return VG_(baseBlock)[ shadow_reg_index(archreg) ];
}

void VG_(set_shadow_archreg) ( UInt archreg, UInt val )
{
   VG_(baseBlock)[ shadow_reg_index(archreg) ] = val;
}

void VG_(set_shadow_eflags) ( UInt val )
{
   VG_(baseBlock)[ VGOFF_(sh_eflags) ] = val;
}

UInt VG_(get_thread_shadow_archreg) ( ThreadId tid, UInt archreg )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   switch (archreg) {
      case R_EAX: return tst->sh_eax;
      case R_ECX: return tst->sh_ecx;
      case R_EDX: return tst->sh_edx; 
      case R_EBX: return tst->sh_ebx; 
      case R_ESP: return tst->sh_esp; 
      case R_EBP: return tst->sh_ebp; 
      case R_ESI: return tst->sh_esi; 
      case R_EDI: return tst->sh_edi; 
      default:    VG_(core_panic)( "get_thread_shadow_archreg");
   }
}

void VG_(set_thread_shadow_archreg) ( ThreadId tid, UInt archreg, UInt val )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   switch (archreg) {
      case R_EAX: tst->sh_eax = val; break;
      case R_ECX: tst->sh_ecx = val; break;
      case R_EDX: tst->sh_edx = val; break;
      case R_EBX: tst->sh_ebx = val; break;
      case R_ESP: tst->sh_esp = val; break;
      case R_EBP: tst->sh_ebp = val; break;
      case R_ESI: tst->sh_esi = val; break;
      case R_EDI: tst->sh_edi = val; break;
      default:    VG_(core_panic)( "set_thread_shadow_archreg");
   }
}

Addr VG_(shadow_archreg_address) ( UInt archreg )
{
   return (Addr) & VG_(baseBlock)[ shadow_reg_index(archreg) ];
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


/* *fplive==True indicates that the simulated machine's FPU/SSE state is in
   the real machine's cpu.  If so we need to be very careful not to trash it.
   If FPU/SSE state is live and we deem it necessary to copy it back to
   the simulated machine's FPU/SSE state, we do so.  The final state of
   fpliveness is returned.  In short we _must_ do put_sse_state if
   there is any chance at all that the code generated for a UInstr
   will change the real FPU/MMX/SSE/SSE2 state.  
*/
static void emitUInstr ( UCodeBlock* cb, Int i, 
                         RRegSet regs_live_before, 
			 /* Running state, which we update. */
                         Bool* sselive,   /* True<==>FPU/SSE 
                                            state in real FPU */
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
         synth_mov_reg_offregmem ( 
            u->size, 
            u->val1, 
            spillOrArchOffset( u->size, u->tag2, u->val2 ),
            R_EBP
         );
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

	 if (*sselive) {
	    emit_put_sse_state();
	    *sselive = False;
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
	 if (*sselive) {
	    emit_put_sse_state();
	    *sselive = False;
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
	 if (*sselive) {
	    emit_put_sse_state();
	    *sselive = False;
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
	 if (*sselive) {
	    emit_put_sse_state();
	    *sselive = False;
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

	 if (*sselive) {
	    emit_put_sse_state();
	    *sselive = False;
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
         synth_setb_reg ( True, u->val1, u->cond );
         break;

      case FPU_R: 
      case FPU_W:         
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == RealReg);
	 if (!(*sselive)) {
	    emit_get_sse_state();
	    *sselive = True;
	 }
         synth_fpu_regmem ( u->flags_r, u->flags_w,
			    (u->val1 >> 8) & 0xFF,
                            u->val1 & 0xFF,
                            u->val2 );
         break;

      case FPU:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
	 if (!(*sselive)) {
	    emit_get_sse_state();
	    *sselive = True;
	 }
         synth_fpu_no_mem ( u->flags_r, u->flags_w,
			    (u->val1 >> 8) & 0xFF,
                            u->val1 & 0xFF );
         break;

      case MMX2_MemWr:
      case MMX2_MemRd:
         vg_assert(u->size == 4 || u->size == 8);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->tag3 == NoValue);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         synth_MMX2_regmem ( u->flags_r, u->flags_w,
                             (u->val1 >> 8) & 0xFF,
                             u->val1 & 0xFF,
                             u->val2 );
         break;

      case MMX2_ERegRd:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->tag3 == NoValue);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         synth_MMX2_reg_to_mmxreg ( u->flags_r, u->flags_w,
                                    (u->val1 >> 8) & 0xFF,
                                    u->val1 & 0xFF,
                                    u->val2 );
         break;

      case MMX2_ERegWr:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->tag3 == NoValue);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         synth_MMX2_mmxreg_to_reg ( u->flags_r, u->flags_w,
                                    (u->val1 >> 8) & 0xFF,
                                    u->val1 & 0xFF,
                                    u->val2 );
         break;

      case MMX1:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
         vg_assert(u->tag3 == NoValue);
	 if (!(*sselive)) {
	    emit_get_sse_state();
	    *sselive = True;
	 }
         synth_MMX1_no_mem ( u->flags_r, u->flags_w,
                             u->val1 & 0xFF );
         break;

      case MMX2:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
         vg_assert(u->tag3 == NoValue);
	 if (!(*sselive)) {
	    emit_get_sse_state();
	    *sselive = True;
	 }
         synth_MMX2_no_mem ( u->flags_r, u->flags_w,
			     (u->val1 >> 8) & 0xFF,
                             u->val1 & 0xFF );
         break;

      case MMX3:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == NoValue);
	 if (!(*sselive)) {
	    emit_get_sse_state();
	    *sselive = True;
	 }
         synth_MMX3_no_mem ( u->flags_r, u->flags_w,
			     (u->val1 >> 8) & 0xFF,
                             u->val1 & 0xFF,
                             u->val2 & 0xFF );
         break;

      case SSE2a_MemWr:
      case SSE2a_MemRd:
         vg_assert(u->size == 4 || u->size == 16);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == RealReg);
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE2a ( u->flags_r, u->flags_w,
                      (u->val1 >> 8) & 0xFF,
                      u->val1 & 0xFF,
                      u->val2 & 0xFF,
                      u->val3 );
         break;

      case SSE2a1_MemRd:
         vg_assert(u->size == 4 || u->size == 16);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == RealReg);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE2a1 ( u->flags_r, u->flags_w,
                      (u->val1 >> 8) & 0xFF,
                      u->val1 & 0xFF,
                      (u->val2 >> 8) & 0xFF,
                      u->val2 & 0xFF,
                      u->val3 );
         break;

      case SSE3a_MemWr:
      case SSE3a_MemRd:
         vg_assert(u->size == 4 || u->size == 8 || u->size == 16);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == RealReg);
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE3a ( u->flags_r, u->flags_w,
                      (u->val1 >> 8) & 0xFF,
                      u->val1 & 0xFF,
                      (u->val2 >> 8) & 0xFF,
                      u->val2 & 0xFF,
                      u->val3 );
         break;

      case SSE3e_RegWr:
      case SSE3e_RegRd:
      case SSE3g_RegWr:
         vg_assert(u->size == 4);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == RealReg);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
	 if (u->opcode==SSE3e_RegRd || u->opcode==SSE3e_RegWr) {
            emit_SSE3e ( u->flags_r, u->flags_w,
                         (u->val1 >> 8) & 0xFF,
                         u->val1 & 0xFF,
                         (u->val2 >> 8) & 0xFF,
                         u->val2 & 0xFF,
                         u->val3 );
	 } else {
            emit_SSE3g ( u->flags_r, u->flags_w,
                         (u->val1 >> 8) & 0xFF,
                         u->val1 & 0xFF,
                         (u->val2 >> 8) & 0xFF,
                         u->val2 & 0xFF,
                         u->val3 );
	 }
         break;

      case SSE3g1_RegWr:
         vg_assert(u->size == 4);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == RealReg);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE3g1 ( u->flags_r, u->flags_w,
                       (u->val1 >> 8) & 0xFF,
                       u->val1 & 0xFF,
                       (u->val2 >> 8) & 0xFF,
                       u->val2 & 0xFF,
                       u->lit32 & 0xFF,
                       u->val3 );
         break;

      case SSE3e1_RegRd:
         vg_assert(u->size == 2);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == RealReg);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE3e1 ( u->flags_r, u->flags_w,
                       (u->val1 >> 8) & 0xFF,
                       u->val1 & 0xFF,
                       (u->val2 >> 8) & 0xFF,
                       u->val2 & 0xFF,
                       u->lit32 & 0xFF,
                       u->val3 );
         break;

      case SSE5:
         vg_assert(u->size == 0);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == Lit16);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE5 ( u->flags_r, u->flags_w,
                     (u->val1 >> 8) & 0xFF,
                     u->val1 & 0xFF,
                     (u->val2 >> 8) & 0xFF,
                     u->val2 & 0xFF,
                     u->val3 & 0xFF );
         break;

      case SSE4:
         vg_assert(u->size == 0);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == NoValue);
         vg_assert(u->flags_r == FlagsEmpty); 
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE4 ( u->flags_r, u->flags_w,
                     (u->val1 >> 8) & 0xFF,
                     u->val1 & 0xFF,
                     (u->val2 >> 8) & 0xFF,
                     u->val2 & 0xFF );
         break;

      case SSE3:
         vg_assert(u->size == 0);
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == Lit16);
         vg_assert(u->tag3 == NoValue);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE3 ( u->flags_r, u->flags_w,
                     (u->val1 >> 8) & 0xFF,
                     u->val1 & 0xFF,
                     u->val2 & 0xFF );
         break;

      case SSE3ag_MemRd_RegWr:
         vg_assert(u->size == 4 || u->size == 8);
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->tag3 == NoValue);
         vg_assert(!anyFlagUse(u));
         if (!(*sselive)) {
            emit_get_sse_state();
            *sselive = True;
         }
         emit_SSE3ag_MemRd_RegWr ( u->flags_r, u->flags_w,
                                   (u->lit32 >> 24) & 0xFF,
                                   (u->lit32 >> 16) & 0xFF,
                                   (u->lit32 >> 8) & 0xFF,
				   u->val1, u->val2 );
	 break;

      default: 
         if (VG_(needs).extended_UCode) {
	    if (*sselive) {
	       emit_put_sse_state();
	       *sselive = False;
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

   if (0 && (*sselive)) {
      emit_put_sse_state();
      *sselive = False;
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
   Bool sselive;
   Addr orig_eip, curr_eip;
   Int tgt;

   reset_state();

   if (dis) VG_(printf)("Generated x86 code:\n");

   /* Generate decl VG_(dispatch_ctr) and drop into dispatch if we hit
      zero.  We have to do this regardless of whether we're t-chaining
      or not. */
   VG_(init_target)(&tgt);
   VG_(new_emit)(False, FlagsEmpty, FlagsOSZAP);
   VG_(emitB) (0xFF);	/* decl */
   emit_amode_litmem_reg((Addr)&VG_(dispatch_ctr), 1);
   if (dis)
      VG_(printf)("\n\t\tdecl (%p)\n", &VG_(dispatch_ctr));
   VG_(emit_jcondshort_target)(False, CondNZ, &tgt);
   VG_(emit_movv_lit_reg) ( 4, VG_TRC_INNER_COUNTERZERO, R_EBP );
   emit_ret();
   VG_(target_forward)(&tgt);

   /* Set up running state. */
   sselive  = False;
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
                         &sselive, &orig_eip, &curr_eip );
      }
      regs_live_before = u->regs_live_after;
   }
   if (dis) VG_(printf)("\n");
   vg_assert(!sselive);		  /* SSE state must be saved by end of BB */
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
