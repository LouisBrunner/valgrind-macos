
/*--------------------------------------------------------------------*/
/*--- The JITter: translate ucode back to x86 code.                ---*/
/*---                                              vg_from_ucode.c ---*/
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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"


/*------------------------------------------------------------*/
/*--- Renamings of frequently-used global functions.       ---*/
/*------------------------------------------------------------*/

#define dis       VG_(disassemble)
#define nameIReg  VG_(nameOfIntReg)
#define nameISize VG_(nameOfIntSize)


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
   to vg_dispatch, and only %eax is live then, conveniently.  Ucode
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

static UChar* emitted_code;
static Int    emitted_code_used;
static Int    emitted_code_size;

static void expandEmittedCode ( void )
{
   Int    i;
   UChar* tmp = VG_(jitmalloc)(2 * emitted_code_size);
   /* VG_(printf)("expand to %d\n", 2 * emitted_code_size); */
   for (i = 0; i < emitted_code_size; i++)
      tmp[i] = emitted_code[i];
   VG_(jitfree)(emitted_code);
   emitted_code = tmp;
   emitted_code_size *= 2;
}

static __inline__ void emitB ( UInt b )
{
   if (dis) {
      if (b < 16) VG_(printf)("0%x ", b); else VG_(printf)("%2x ", b);
   }
   if (emitted_code_used == emitted_code_size)
      expandEmittedCode();

   emitted_code[emitted_code_used] = (UChar)b;
   emitted_code_used++;
}

static __inline__ void emitW ( UInt l )
{
   emitB ( (l) & 0x000000FF );
   emitB ( (l >> 8) & 0x000000FF );
}

static __inline__ void emitL ( UInt l )
{
   emitB ( (l) & 0x000000FF );
   emitB ( (l >> 8) & 0x000000FF );
   emitB ( (l >> 16) & 0x000000FF );
   emitB ( (l >> 24) & 0x000000FF );
}

static __inline__ void newEmit ( void )
{
   if (dis)
      VG_(printf)("\t       %4d: ", emitted_code_used );
}

/* Is this a callee-save register, in the normal C calling convention?  */
#define VG_CALLEE_SAVED(reg) (reg == R_EBX || reg == R_ESI || reg == R_EDI)


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
      default: VG_(panic)( "mkSIB" );
   }
   return ((shift & 3) << 6) | ((regindex & 7) << 3) | (regbase & 7);
}

static __inline__ void emit_amode_litmem_reg ( Addr addr, Int reg )
{
   /* ($ADDR), reg */
   emitB ( mkModRegRM(0, reg, 5) );
   emitL ( addr );
}

static __inline__ void emit_amode_regmem_reg ( Int regmem, Int reg )
{
   /* (regmem), reg */
   if (regmem == R_ESP) 
      VG_(panic)("emit_amode_regmem_reg");
   if (regmem == R_EBP) {
      emitB ( mkModRegRM(1, reg, 5) );
      emitB ( 0x00 );
   } else {
      emitB( mkModRegRM(0, reg, regmem) );
   }
}

static __inline__ void emit_amode_offregmem_reg ( Int off, Int regmem, Int reg )
{
   if (regmem == R_ESP)
      VG_(panic)("emit_amode_offregmem_reg(ESP)");
   if (off < -128 || off > 127) {
      /* Use a large offset */
      /* d32(regmem), reg */
      emitB ( mkModRegRM(2, reg, regmem) );
      emitL ( off );
   } else {
      /* d8(regmem), reg */
      emitB ( mkModRegRM(1, reg, regmem) );
      emitB ( off & 0xFF );
   }
}

static __inline__ void emit_amode_sib_reg ( Int off, Int scale, Int regbase, 
                                            Int regindex, Int reg )
{
   if (regindex == R_ESP)
      VG_(panic)("emit_amode_sib_reg(ESP)");
   if (off < -128 || off > 127) {
      /* Use a 32-bit offset */
      emitB ( mkModRegRM(2, reg, 4) ); /* SIB with 32-bit displacement */
      emitB ( mkSIB( scale, regindex, regbase ) );
      emitL ( off );
   } else {
      /* Use an 8-bit offset */
      emitB ( mkModRegRM(1, reg, 4) ); /* SIB with 8-bit displacement */
      emitB ( mkSIB( scale, regindex, regbase ) );
      emitB ( off & 0xFF );
   }
}

static __inline__ void emit_amode_ereg_greg ( Int e_reg, Int g_reg )
{
   /* other_reg, reg */
   emitB ( mkModRegRM(3, g_reg, e_reg) );
}

static __inline__ void emit_amode_greg_ereg ( Int g_reg, Int e_reg )
{
   /* other_reg, reg */
   emitB ( mkModRegRM(3, g_reg, e_reg) );
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
      default: VG_(panic)("mkGrp1opcode");
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
      default: VG_(panic)("mkGrp2opcode");
   }
}

static __inline__ Int mkGrp3opcode ( Opcode opc )
{
   switch (opc) {
      case NOT: return 2;
      case NEG: return 3;
      default: VG_(panic)("mkGrp3opcode");
   }
}

static __inline__ Int mkGrp4opcode ( Opcode opc )
{
   switch (opc) {
      case INC: return 0;
      case DEC: return 1;
      default: VG_(panic)("mkGrp4opcode");
   }
}

static __inline__ Int mkGrp5opcode ( Opcode opc )
{
   switch (opc) {
      case CALLM: return 2;
      case JMP:   return 4;
      default: VG_(panic)("mkGrp5opcode");
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
      default: VG_(panic)("mkPrimaryOpcode");
  }
}

/*----------------------------------------------------*/
/*--- v-size (4, or 2 with OSO) insn emitters      ---*/
/*----------------------------------------------------*/

static void emit_movv_offregmem_reg ( Int sz, Int off, Int areg, Int reg )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 0x8B ); /* MOV Ev, Gv */
   emit_amode_offregmem_reg ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t0x%x(%s), %s\n", 
                   nameISize(sz), off, nameIReg(4,areg), nameIReg(sz,reg));
}

static void emit_movv_reg_offregmem ( Int sz, Int reg, Int off, Int areg )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 0x89 ); /* MOV Gv, Ev */
   emit_amode_offregmem_reg ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, 0x%x(%s)\n", 
                   nameISize(sz), nameIReg(sz,reg), off, nameIReg(4,areg));
}

static void emit_movv_regmem_reg ( Int sz, Int reg1, Int reg2 )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 0x8B ); /* MOV Ev, Gv */
   emit_amode_regmem_reg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t(%s), %s\n",
                   nameISize(sz),  nameIReg(4,reg1), nameIReg(sz,reg2));
}

static void emit_movv_reg_regmem ( Int sz, Int reg1, Int reg2 )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 0x89 ); /* MOV Gv, Ev */
   emit_amode_regmem_reg ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, (%s)\n", 
                   nameISize(sz), nameIReg(sz,reg1), nameIReg(4,reg2));
}

static void emit_movv_reg_reg ( Int sz, Int reg1, Int reg2 )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 0x89 ); /* MOV Gv, Ev */
   emit_amode_ereg_greg ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t%s, %s\n", 
                   nameISize(sz), nameIReg(sz,reg1), nameIReg(sz,reg2));
}

static void emit_nonshiftopv_lit_reg ( Int sz, Opcode opc, 
                                       UInt lit, Int reg )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   if (lit == VG_(extend_s_8to32)(lit & 0x000000FF)) {
      /* short form OK */
      emitB ( 0x83 ); /* Grp1 Ib,Ev */
      emit_amode_ereg_greg ( reg, mkGrp1opcode(opc) );
      emitB ( lit & 0x000000FF );
   } else {
      emitB ( 0x81 ); /* Grp1 Iv,Ev */
      emit_amode_ereg_greg ( reg, mkGrp1opcode(opc) );
      if (sz == 2) emitW ( lit ); else emitL ( lit );
   }
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t$0x%x, %s\n", 
                   VG_(nameUOpcode)(False,opc), nameISize(sz), 
                   lit, nameIReg(sz,reg));
}

static void emit_shiftopv_lit_reg ( Int sz, Opcode opc, UInt lit, Int reg )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 0xC1 ); /* Grp2 Ib,Ev */
   emit_amode_ereg_greg ( reg, mkGrp2opcode(opc) );
   emitB ( lit );
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t$%d, %s\n", 
                   VG_(nameUOpcode)(False,opc), nameISize(sz), 
                   lit, nameIReg(sz,reg));
}

static void emit_shiftopv_cl_stack0 ( Int sz, Opcode opc )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 0xD3 ); /* Grp2 CL,Ev */
   emitB ( mkModRegRM ( 1, mkGrp2opcode(opc), 4 ) );
   emitB ( 0x24 ); /* a SIB, I think `d8(%esp)' */
   emitB ( 0x00 ); /* the d8 displacement */
   if (dis)
      VG_(printf)("\n\t\t%s%c %%cl, 0(%%esp)\n",
                  VG_(nameUOpcode)(False,opc), nameISize(sz) );
}

static void emit_shiftopb_cl_stack0 ( Opcode opc )
{
   newEmit();
   emitB ( 0xD2 ); /* Grp2 CL,Eb */
   emitB ( mkModRegRM ( 1, mkGrp2opcode(opc), 4 ) );
   emitB ( 0x24 ); /* a SIB, I think `d8(%esp)' */
   emitB ( 0x00 ); /* the d8 displacement */
   if (dis)
      VG_(printf)("\n\t\t%s%c %%cl, 0(%%esp)\n",
                  VG_(nameUOpcode)(False,opc), nameISize(1) );
}

static void emit_nonshiftopv_offregmem_reg ( Int sz, Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 3 + mkPrimaryOpcode(opc) ); /* op Ev, Gv */
   emit_amode_offregmem_reg ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t0x%x(%s), %s\n", 
                   VG_(nameUOpcode)(False,opc), nameISize(sz),
                   off, nameIReg(4,areg), nameIReg(sz,reg));
}

static void emit_nonshiftopv_reg_reg ( Int sz, Opcode opc, 
                                       Int reg1, Int reg2 )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
#  if 0
   /* Perfectly correct, but the GNU assembler uses the other form.
      Therefore we too use the other form, to aid verification. */
   emitB ( 3 + mkPrimaryOpcode(opc) ); /* op Ev, Gv */
   emit_amode_ereg_greg ( reg1, reg2 );
#  else
   emitB ( 1 + mkPrimaryOpcode(opc) ); /* op Gv, Ev */
   emit_amode_greg_ereg ( reg1, reg2 );
#  endif
   if (dis)
      VG_(printf)( "\n\t\t%s%c\t%s, %s\n", 
                   VG_(nameUOpcode)(False,opc), nameISize(sz), 
                   nameIReg(sz,reg1), nameIReg(sz,reg2));
}

static void emit_movv_lit_reg ( Int sz, UInt lit, Int reg )
{
   if (lit == 0) {
      emit_nonshiftopv_reg_reg ( sz, XOR, reg, reg );
      return;
   }
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   emitB ( 0xB8+reg ); /* MOV imm, Gv */
   if (sz == 2) emitW ( lit ); else emitL ( lit );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t$0x%x, %s\n", 
                   nameISize(sz), lit, nameIReg(sz,reg));
}

static void emit_unaryopv_reg ( Int sz, Opcode opc, Int reg )
{
   newEmit();
   if (sz == 2) emitB ( 0x66 );
   switch (opc) {
      case NEG:
         emitB ( 0xF7 );
         emit_amode_ereg_greg ( reg, mkGrp3opcode(NEG) );
         if (dis)
            VG_(printf)( "\n\t\tneg%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case NOT:
         emitB ( 0xF7 );
         emit_amode_ereg_greg ( reg, mkGrp3opcode(NOT) );
         if (dis)
            VG_(printf)( "\n\t\tnot%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case DEC:
         emitB ( 0x48 + reg );
         if (dis)
            VG_(printf)( "\n\t\tdec%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      case INC:
         emitB ( 0x40 + reg );
         if (dis)
            VG_(printf)( "\n\t\tinc%c\t%s\n", 
                         nameISize(sz), nameIReg(sz,reg));
         break;
      default: 
         VG_(panic)("emit_unaryopv_reg");
   }
}

static void emit_pushv_reg ( Int sz, Int reg )
{
   newEmit();
   if (sz == 2) {
      emitB ( 0x66 ); 
   } else {
      vg_assert(sz == 4);
   }
   emitB ( 0x50 + reg );
   if (dis)
      VG_(printf)("\n\t\tpush%c %s\n", nameISize(sz), nameIReg(sz,reg));
}

static void emit_popv_reg ( Int sz, Int reg )
{
   newEmit();
   if (sz == 2) {
      emitB ( 0x66 ); 
   } else {
      vg_assert(sz == 4);
   }
   emitB ( 0x58 + reg );
   if (dis)
      VG_(printf)("\n\t\tpop%c %s\n", nameISize(sz), nameIReg(sz,reg));
}

static void emit_pushl_lit8 ( Int lit8 )
{
   vg_assert(lit8 >= -128 && lit8 < 128);
   newEmit();
   emitB ( 0x6A );
   emitB ( (UChar)((UInt)lit8) );
   if (dis)
      VG_(printf)("\n\t\tpushl $%d\n", lit8 );
}

static void emit_pushl_lit32 ( UInt int32 )
{
   newEmit();
   emitB ( 0x68 );
   emitL ( int32 );
   if (dis)
      VG_(printf)("\n\t\tpushl $0x%x\n", int32 );
}

static void emit_cmpl_zero_reg ( Int reg )
{
   newEmit();
   emitB ( 0x83 );
   emit_amode_ereg_greg ( reg, 7 /* Grp 3 opcode for CMP */ );
   emitB ( 0x00 );
   if (dis)
      VG_(printf)("\n\t\tcmpl $0, %s\n", nameIReg(4,reg));
}

static void emit_swapl_reg_ECX ( Int reg )
{
   newEmit();
   emitB ( 0x87 ); /* XCHG Gv,Ev */
   emit_amode_ereg_greg ( reg, R_ECX );
   if (dis) 
      VG_(printf)("\n\t\txchgl %%ecx, %s\n", nameIReg(4,reg));
}

static void emit_swapl_reg_EAX ( Int reg )
{
   newEmit();
   emitB ( 0x90 + reg ); /* XCHG Gv,eAX */
   if (dis) 
      VG_(printf)("\n\t\txchgl %%eax, %s\n", nameIReg(4,reg));
}

static void emit_swapl_reg_reg ( Int reg1, Int reg2 )
{
   newEmit();
   emitB ( 0x87 ); /* XCHG Gv,Ev */
   emit_amode_ereg_greg ( reg1, reg2 );
   if (dis) 
      VG_(printf)("\n\t\txchgl %s, %s\n", nameIReg(4,reg1), 
                  nameIReg(4,reg2));
}

static void emit_bswapl_reg ( Int reg )
{
   newEmit();
   emitB ( 0x0F );
   emitB ( 0xC8 + reg ); /* BSWAP r32 */
   if (dis) 
      VG_(printf)("\n\t\tbswapl %s\n", nameIReg(4,reg));
}

static void emit_movl_reg_reg ( Int regs, Int regd )
{
   newEmit();
   emitB ( 0x89 ); /* MOV Gv,Ev */
   emit_amode_ereg_greg ( regd, regs );
   if (dis) 
      VG_(printf)("\n\t\tmovl %s, %s\n", nameIReg(4,regs), nameIReg(4,regd));
}

static void emit_testv_lit_reg ( Int sz, UInt lit, Int reg )
{
   newEmit();
   if (sz == 2) {
      emitB ( 0x66 );
   } else {
      vg_assert(sz == 4);
   }
   emitB ( 0xF7 ); /* Grp3 Ev */
   emit_amode_ereg_greg ( reg, 0 /* Grp3 subopcode for TEST */ );
   if (sz == 2) emitW ( lit ); else emitL ( lit );
   if (dis)
      VG_(printf)("\n\t\ttest%c $0x%x, %s\n", nameISize(sz), 
                                            lit, nameIReg(sz,reg));
}

static void emit_testv_lit_offregmem ( Int sz, UInt lit, Int off, Int reg )
{
   newEmit();
   if (sz == 2) {
      emitB ( 0x66 );
   } else {
      vg_assert(sz == 4);
   }
   emitB ( 0xF7 ); /* Grp3 Ev */
   emit_amode_offregmem_reg ( off, reg, 0 /* Grp3 subopcode for TEST */ );
   if (sz == 2) emitW ( lit ); else emitL ( lit );
   if (dis)
      VG_(printf)("\n\t\ttest%c $%d, 0x%x(%s)\n", 
                  nameISize(sz), lit, off, nameIReg(4,reg) );
}

static void emit_movv_lit_offregmem ( Int sz, UInt lit, Int off, Int memreg )
{
   newEmit();
   if (sz == 2) {
      emitB ( 0x66 );
   } else {
      vg_assert(sz == 4);
   }
   emitB ( 0xC7 ); /* Grp11 Ev */
   emit_amode_offregmem_reg ( off, memreg, 0 /* Grp11 subopcode for MOV */ );
   if (sz == 2) emitW ( lit ); else emitL ( lit );
   if (dis)
      VG_(printf)( "\n\t\tmov%c\t$0x%x, 0x%x(%s)\n", 
                   nameISize(sz), lit, off, nameIReg(4,memreg) );
}


/*----------------------------------------------------*/
/*--- b-size (1 byte) instruction emitters         ---*/
/*----------------------------------------------------*/

/* There is some doubt as to whether C6 (Grp 11) is in the
   486 insn set.  ToDo: investigate. */
static void emit_movb_lit_offregmem ( UInt lit, Int off, Int memreg )
{
   newEmit();
   emitB ( 0xC6 ); /* Grp11 Eb */
   emit_amode_offregmem_reg ( off, memreg, 0 /* Grp11 subopcode for MOV */ );
   emitB ( lit );
   if (dis)
      VG_(printf)( "\n\t\tmovb\t$0x%x, 0x%x(%s)\n", 
                   lit, off, nameIReg(4,memreg) );
}

static void emit_nonshiftopb_offregmem_reg ( Opcode opc, 
                                             Int off, Int areg, Int reg )
{
   newEmit();
   emitB ( 2 + mkPrimaryOpcode(opc) ); /* op Eb, Gb */
   emit_amode_offregmem_reg ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t0x%x(%s), %s\n", 
                   VG_(nameUOpcode)(False,opc), off, nameIReg(4,areg), 
                   nameIReg(1,reg));
}

static void emit_movb_reg_offregmem ( Int reg, Int off, Int areg )
{
   /* Could do better when reg == %al. */
   newEmit();
   emitB ( 0x88 ); /* MOV G1, E1 */
   emit_amode_offregmem_reg ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovb\t%s, 0x%x(%s)\n", 
                   nameIReg(1,reg), off, nameIReg(4,areg));
}

static void emit_nonshiftopb_reg_reg ( Opcode opc, Int reg1, Int reg2 )
{
   newEmit();
   emitB ( 2 + mkPrimaryOpcode(opc) ); /* op Eb, Gb */
   emit_amode_ereg_greg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t%s, %s\n", 
                   VG_(nameUOpcode)(False,opc),
                   nameIReg(1,reg1), nameIReg(1,reg2));
}

static void emit_movb_reg_regmem ( Int reg1, Int reg2 )
{
   newEmit();
   emitB ( 0x88 ); /* MOV G1, E1 */
   emit_amode_regmem_reg ( reg2, reg1 );
   if (dis)
      VG_(printf)( "\n\t\tmovb\t%s, (%s)\n", nameIReg(1,reg1), 
                                             nameIReg(4,reg2));
}

static void emit_nonshiftopb_lit_reg ( Opcode opc, UInt lit, Int reg )
{
   newEmit();
   emitB ( 0x80 ); /* Grp1 Ib,Eb */
   emit_amode_ereg_greg ( reg, mkGrp1opcode(opc) );
   emitB ( lit & 0x000000FF );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t$0x%x, %s\n", VG_(nameUOpcode)(False,opc),
                                             lit, nameIReg(1,reg));
}

static void emit_shiftopb_lit_reg ( Opcode opc, UInt lit, Int reg )
{
   newEmit();
   emitB ( 0xC0 ); /* Grp2 Ib,Eb */
   emit_amode_ereg_greg ( reg, mkGrp2opcode(opc) );
   emitB ( lit );
   if (dis)
      VG_(printf)( "\n\t\t%sb\t$%d, %s\n", 
                   VG_(nameUOpcode)(False,opc),
                   lit, nameIReg(1,reg));
}

static void emit_unaryopb_reg ( Opcode opc, Int reg )
{
   newEmit();
   switch (opc) {
      case INC:
         emitB ( 0xFE );
         emit_amode_ereg_greg ( reg, mkGrp4opcode(INC) );
         if (dis)
            VG_(printf)( "\n\t\tincb\t%s\n", nameIReg(1,reg));
         break;
      case DEC:
         emitB ( 0xFE );
         emit_amode_ereg_greg ( reg, mkGrp4opcode(DEC) );
         if (dis)
            VG_(printf)( "\n\t\tdecb\t%s\n", nameIReg(1,reg));
         break;
      case NOT:
         emitB ( 0xF6 );
         emit_amode_ereg_greg ( reg, mkGrp3opcode(NOT) );
         if (dis)
            VG_(printf)( "\n\t\tnotb\t%s\n", nameIReg(1,reg));
         break;
      case NEG:
         emitB ( 0xF6 );
         emit_amode_ereg_greg ( reg, mkGrp3opcode(NEG) );
         if (dis)
            VG_(printf)( "\n\t\tnegb\t%s\n", nameIReg(1,reg));
         break;
      default: 
         VG_(panic)("emit_unaryopb_reg");
   }
}

static void emit_testb_lit_reg ( UInt lit, Int reg )
{
   newEmit();
   emitB ( 0xF6 ); /* Grp3 Eb */
   emit_amode_ereg_greg ( reg, 0 /* Grp3 subopcode for TEST */ );
   emitB ( lit );
   if (dis)
      VG_(printf)("\n\t\ttestb $0x%x, %s\n", lit, nameIReg(1,reg));
}


/*----------------------------------------------------*/
/*--- zero-extended load emitters                  ---*/
/*----------------------------------------------------*/

static void emit_movzbl_offregmem_reg ( Int off, Int regmem, Int reg )
{
   newEmit();
   emitB ( 0x0F ); emitB ( 0xB6 ); /* MOVZBL */
   emit_amode_offregmem_reg ( off, regmem, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovzbl\t0x%x(%s), %s\n", 
                   off, nameIReg(4,regmem), nameIReg(4,reg));
}

static void emit_movzbl_regmem_reg ( Int reg1, Int reg2 )
{
   newEmit();
   emitB ( 0x0F ); emitB ( 0xB6 ); /* MOVZBL */
   emit_amode_regmem_reg ( reg1, reg2 );
   if (dis)
      VG_(printf)( "\n\t\tmovzbl\t(%s), %s\n", nameIReg(4,reg1), 
                                               nameIReg(4,reg2));
}

static void emit_movzwl_offregmem_reg ( Int off, Int areg, Int reg )
{
   newEmit();
   emitB ( 0x0F ); emitB ( 0xB7 ); /* MOVZWL */
   emit_amode_offregmem_reg ( off, areg, reg );
   if (dis)
      VG_(printf)( "\n\t\tmovzwl\t0x%x(%s), %s\n",
                   off, nameIReg(4,areg), nameIReg(4,reg));
}

static void emit_movzwl_regmem_reg ( Int reg1, Int reg2 )
{
   newEmit();
   emitB ( 0x0F ); emitB ( 0xB7 ); /* MOVZWL */
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
   newEmit();
   emitB ( 0xDD ); emitB ( 0xA5 ); /* frstor d32(%ebp) */
   emitL ( off );
   if (dis)
      VG_(printf)("\n\t\tfrstor\t%d(%%ebp)\n", off );
}

static void emit_put_fpu_state ( void )
{
   Int off = 4 * VGOFF_(m_fpustate);
   newEmit();
   emitB ( 0xDD ); emitB ( 0xB5 ); /* fnsave d32(%ebp) */
   emitL ( off );
   if (dis)
      VG_(printf)("\n\t\tfnsave\t%d(%%ebp)\n", off );
}

static void emit_fpu_no_mem ( UChar first_byte, 
                              UChar second_byte )
{
   newEmit();
   emitB ( first_byte );
   emitB ( second_byte );
   if (dis)
      VG_(printf)("\n\t\tfpu-0x%x:0x%x\n", 
                  (UInt)first_byte, (UInt)second_byte );
}

static void emit_fpu_regmem ( UChar first_byte, 
                              UChar second_byte_masked, 
                              Int reg )
{
   newEmit();
   emitB ( first_byte );
   emit_amode_regmem_reg ( reg, second_byte_masked >> 3 );
   if (dis)
      VG_(printf)("\n\t\tfpu-0x%x:0x%x-(%s)\n", 
                  (UInt)first_byte, (UInt)second_byte_masked,
                  nameIReg(4,reg) );
}


/*----------------------------------------------------*/
/*--- misc instruction emitters                    ---*/
/*----------------------------------------------------*/

static void emit_call_reg ( Int reg )
{
   newEmit();
   emitB ( 0xFF ); /* Grp5 */
   emit_amode_ereg_greg ( reg, mkGrp5opcode(CALLM) );
   if (dis)
      VG_(printf)( "\n\t\tcall\t*%s\n", nameIReg(4,reg) );
}


static void emit_call_star_EBP_off ( Int byte_off )
{
  newEmit();
  if (byte_off < -128 || byte_off > 127) {
     emitB ( 0xFF );
     emitB ( 0x95 );
     emitL ( byte_off );
  } else {
     emitB ( 0xFF );
     emitB ( 0x55 );
     emitB ( byte_off );
  }
  if (dis)
     VG_(printf)( "\n\t\tcall * %d(%%ebp)\n", byte_off );
}


static void emit_addlit8_offregmem ( Int lit8, Int regmem, Int off )
{
   vg_assert(lit8 >= -128 && lit8 < 128);
   newEmit();
   emitB ( 0x83 ); /* Grp1 Ib,Ev */
   emit_amode_offregmem_reg ( off, regmem, 
                              0 /* Grp1 subopcode for ADD */ );
   emitB ( lit8 & 0xFF );
   if (dis)
      VG_(printf)( "\n\t\taddl $%d, %d(%s)\n", lit8, off, 
                                               nameIReg(4,regmem));
}


static void emit_add_lit_to_esp ( Int lit )
{
   if (lit < -128 || lit > 127) VG_(panic)("emit_add_lit_to_esp");
   newEmit();
   emitB ( 0x83 );
   emitB ( 0xC4 );
   emitB ( lit & 0xFF );
   if (dis)
      VG_(printf)( "\n\t\taddl $%d, %%esp\n", lit );
}


static void emit_movb_AL_zeroESPmem ( void )
{
   /* movb %al, 0(%esp) */
   /* 88442400              movb    %al, 0(%esp) */
   newEmit();
   emitB ( 0x88 );
   emitB ( 0x44 );
   emitB ( 0x24 );
   emitB ( 0x00 );
   if (dis)
      VG_(printf)( "\n\t\tmovb %%al, 0(%%esp)\n" );
}

static void emit_movb_zeroESPmem_AL ( void )
{
   /* movb 0(%esp), %al */
   /* 8A442400              movb    0(%esp), %al */
   newEmit();
   emitB ( 0x8A );
   emitB ( 0x44 );
   emitB ( 0x24 );
   emitB ( 0x00 );
   if (dis)
      VG_(printf)( "\n\t\tmovb 0(%%esp), %%al\n" );
}


/* Emit a jump short with an 8-bit signed offset.  Note that the
   offset is that which should be added to %eip once %eip has been
   advanced over this insn.  */
static void emit_jcondshort_delta ( Condcode cond, Int delta )
{
   vg_assert(delta >= -128 && delta <= 127);
   newEmit();
   emitB ( 0x70 + (UInt)cond );
   emitB ( (UChar)delta );
   if (dis)
      VG_(printf)( "\n\t\tj%s-8\t%%eip+%d\n", 
                   VG_(nameCondcode)(cond), delta );
}

static void emit_get_eflags ( void )
{
   Int off = 4 * VGOFF_(m_eflags);
   vg_assert(off >= 0 && off < 128);
   newEmit();
   emitB ( 0xFF ); /* PUSHL off(%ebp) */
   emitB ( 0x75 );
   emitB ( off );
   emitB ( 0x9D ); /* POPFL */
   if (dis)
      VG_(printf)( "\n\t\tpushl %d(%%ebp) ; popfl\n", off );
}

static void emit_put_eflags ( void )
{
   Int off = 4 * VGOFF_(m_eflags);
   vg_assert(off >= 0 && off < 128);
   newEmit();
   emitB ( 0x9C ); /* PUSHFL */
   emitB ( 0x8F ); /* POPL vg_m_state.m_eflags */
   emitB ( 0x45 );
   emitB ( off );
   if (dis)
      VG_(printf)( "\n\t\tpushfl ; popl %d(%%ebp)\n", off );
}

static void emit_setb_reg ( Int reg, Condcode cond )
{
   newEmit();
   emitB ( 0x0F ); emitB ( 0x90 + (UChar)cond );
   emit_amode_ereg_greg ( reg, 0 );
   if (dis)
      VG_(printf)("\n\t\tset%s %s\n", 
                  VG_(nameCondcode)(cond), nameIReg(1,reg));
}

static void emit_ret ( void )
{
   newEmit();
   emitB ( 0xC3 ); /* RET */
   if (dis)
      VG_(printf)("\n\t\tret\n");
}

static void emit_pushal ( void )
{
   newEmit();
   emitB ( 0x60 ); /* PUSHAL */
   if (dis)
      VG_(printf)("\n\t\tpushal\n");
}

static void emit_popal ( void )
{
   newEmit();
   emitB ( 0x61 ); /* POPAL */
   if (dis)
      VG_(printf)("\n\t\tpopal\n");
}

static void emit_lea_litreg_reg ( UInt lit, Int regmem, Int reg )
{
   newEmit();
   emitB ( 0x8D ); /* LEA M,Gv */
   emit_amode_offregmem_reg ( (Int)lit, regmem, reg );
   if (dis)
      VG_(printf)("\n\t\tleal 0x%x(%s), %s\n",
                  lit, nameIReg(4,regmem), nameIReg(4,reg) );
}

static void emit_lea_sib_reg ( UInt lit, Int scale,
			       Int regbase, Int regindex, Int reg )
{
   newEmit();
   emitB ( 0x8D ); /* LEA M,Gv */
   emit_amode_sib_reg ( (Int)lit, scale, regbase, regindex, reg );
   if (dis)
      VG_(printf)("\n\t\tleal 0x%x(%s,%s,%d), %s\n",
                  lit, nameIReg(4,regbase), 
                       nameIReg(4,regindex), scale,
                       nameIReg(4,reg) );
}

static void emit_AMD_prefetch_reg ( Int reg )
{
   newEmit();
   emitB ( 0x0F );
   emitB ( 0x0D );
   emit_amode_regmem_reg ( reg, 1 /* 0 is prefetch; 1 is prefetchw */ );
   if (dis)
      VG_(printf)("\n\t\tamd-prefetch (%s)\n", nameIReg(4,reg) );
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
static void synth_call_baseBlock_method ( Bool ensure_shortform, 
                                          Int word_offset )
{
   vg_assert(word_offset >= 0);
   vg_assert(word_offset < VG_BASEBLOCK_WORDS);
   if (ensure_shortform)
      vg_assert(word_offset < 32);
   emit_call_star_EBP_off ( 4 * word_offset );
}

static void synth_ccall_saveRegs ( void )
{
   emit_pushv_reg ( 4, R_EAX ); 
   emit_pushv_reg ( 4, R_ECX ); 
   emit_pushv_reg ( 4, R_EDX ); 
}
   
static void synth_ccall_pushOneArg ( Int r1 )
{
   emit_pushv_reg ( 4, r1 );
}

static void synth_ccall_pushTwoArgs ( Int r1, Int r2 )
{
   /* must push in reverse order */
   emit_pushv_reg ( 4, r2 );
   emit_pushv_reg ( 4, r1 );
}

/* Synthesise a call to *baseBlock[offset], ie,
   call * (4 x offset)(%ebp) with arguments
*/
static void synth_ccall_call_clearStack_restoreRegs ( Int word_offset, 
                                                      UInt n_args_bytes )
{
   vg_assert(word_offset >= 0);
   vg_assert(word_offset < VG_BASEBLOCK_WORDS);
   vg_assert(n_args_bytes <= 12);           /* Max 3 word-sized args */
   vg_assert(0 == (n_args_bytes & 0x3));    /* Divisible by four */

   emit_call_star_EBP_off ( 4 * word_offset );
   if ( 0 != n_args_bytes )
      emit_add_lit_to_esp ( n_args_bytes );
   emit_popv_reg ( 4, R_EDX ); 
   emit_popv_reg ( 4, R_ECX ); 
   emit_popv_reg ( 4, R_EAX ); 
}

static void load_ebp_from_JmpKind ( JmpKind jmpkind )
{
   switch (jmpkind) {
      case JmpBoring: 
         break;
      case JmpCall:
      case JmpRet: 
         emit_movv_lit_reg ( 4, VG_TRC_EBP_JMP_STKADJ, R_EBP );
         break;
      case JmpSyscall: 
         emit_movv_lit_reg ( 4, VG_TRC_EBP_JMP_SYSCALL, R_EBP );
         break;
      case JmpClientReq: 
         emit_movv_lit_reg ( 4, VG_TRC_EBP_JMP_CLIENTREQ, R_EBP );
         break;
      default: 
         VG_(panic)("load_ebp_from_JmpKind");
   }
}

/* Jump to the next translation, by loading its original addr into
   %eax and returning to the scheduler.  Signal special requirements
   by loading a special value into %ebp first.  
*/
static void synth_jmp_reg ( Int reg, JmpKind jmpkind )
{
   load_ebp_from_JmpKind ( jmpkind );
   if (reg != R_EAX)
      emit_movv_reg_reg ( 4, reg, R_EAX );
   emit_ret();
}


/* Same deal as synth_jmp_reg. */
static void synth_jmp_lit ( Addr addr, JmpKind jmpkind )
{
   load_ebp_from_JmpKind ( jmpkind );
   emit_movv_lit_reg ( 4, addr, R_EAX );
   emit_ret();
}


static void synth_jcond_lit ( Condcode cond, Addr addr )
{
  /* Do the following:
        get eflags
        jmp short if not cond to xyxyxy
        addr -> eax
        ret
        xyxyxy

   2 0000 750C                  jnz     xyxyxy
   3 0002 B877665544            movl    $0x44556677, %eax
   4 0007 C3                    ret
   5 0008 FFE3                  jmp     *%ebx
   6                    xyxyxy:
  */
   emit_get_eflags();
   emit_jcondshort_delta ( invertCondition(cond), 5+1 );
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
   emit_cmpl_zero_reg ( reg );
   emit_jcondshort_delta ( CondNZ, 5+1 );
   synth_jmp_lit ( addr, JmpBoring );
}


static void synth_mov_lit_reg ( Int size, UInt lit, Int reg ) 
{
   /* Load the zero-extended literal into reg, at size l,
      regardless of the request size. */
   emit_movv_lit_reg ( 4, lit, reg );
}


static void synth_mov_regmem_reg ( Int size, Int reg1, Int reg2 ) 
{
   switch (size) {
      case 4: emit_movv_regmem_reg ( 4, reg1, reg2 ); break;
      case 2: emit_movzwl_regmem_reg ( reg1, reg2 ); break;
      case 1: emit_movzbl_regmem_reg ( reg1, reg2 ); break;
      default: VG_(panic)("synth_mov_regmem_reg");
   }  
}


static void synth_mov_offregmem_reg ( Int size, Int off, Int areg, Int reg ) 
{
   switch (size) {
      case 4: emit_movv_offregmem_reg ( 4, off, areg, reg ); break;
      case 2: emit_movzwl_offregmem_reg ( off, areg, reg ); break;
      case 1: emit_movzbl_offregmem_reg ( off, areg, reg ); break;
      default: VG_(panic)("synth_mov_offregmem_reg");
   }  
}


static void synth_mov_reg_offregmem ( Int size, Int reg, 
                                      Int off, Int areg )
{
   switch (size) {
      case 4: emit_movv_reg_offregmem ( 4, reg, off, areg ); break;
      case 2: emit_movv_reg_offregmem ( 2, reg, off, areg ); break;
      case 1: if (reg < 4) {
                 emit_movb_reg_offregmem ( reg, off, areg ); 
              }
              else {
                 emit_swapl_reg_EAX ( reg );
                 emit_movb_reg_offregmem ( R_AL, off, areg );
                 emit_swapl_reg_EAX ( reg );
              }
              break;
      default: VG_(panic)("synth_mov_reg_offregmem");
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
      default: VG_(panic)("synth_mov_reg_litmem");
   }
}


static void synth_unaryop_reg ( Bool upd_cc,
                                Opcode opcode, Int size,
                                Int reg )
{
   /* NB! opcode is a uinstr opcode, not an x86 one! */
   switch (size) {
      case 4: if (upd_cc) emit_get_eflags();
              emit_unaryopv_reg ( 4, opcode, reg );
              if (upd_cc) emit_put_eflags();
              break;
      case 2: if (upd_cc) emit_get_eflags();
              emit_unaryopv_reg ( 2, opcode, reg );
              if (upd_cc) emit_put_eflags();
              break;
      case 1: if (reg < 4) {
                 if (upd_cc) emit_get_eflags();
                 emit_unaryopb_reg ( opcode, reg );
                 if (upd_cc) emit_put_eflags();
              } else {
                 emit_swapl_reg_EAX ( reg );
                 if (upd_cc) emit_get_eflags();
                 emit_unaryopb_reg ( opcode, R_AL );
                 if (upd_cc) emit_put_eflags();
                 emit_swapl_reg_EAX ( reg );
              }
              break;
      default: VG_(panic)("synth_unaryop_reg");
   }
}



static void synth_nonshiftop_reg_reg ( Bool upd_cc, 
                                       Opcode opcode, Int size, 
                                       Int reg1, Int reg2 )
{
   /* NB! opcode is a uinstr opcode, not an x86 one! */
   switch (size) {
      case 4: if (upd_cc) emit_get_eflags();
              emit_nonshiftopv_reg_reg ( 4, opcode, reg1, reg2 );
              if (upd_cc) emit_put_eflags();
              break;
      case 2: if (upd_cc) emit_get_eflags();
              emit_nonshiftopv_reg_reg ( 2, opcode, reg1, reg2 );
              if (upd_cc) emit_put_eflags();
              break;
      case 1: { /* Horrible ... */
         Int s1, s2;
         /* Choose s1 and s2 to be x86 regs which we can talk about the
            lowest 8 bits, ie either %eax, %ebx, %ecx or %edx.  Make
            sure s1 != s2 and that neither of them equal either reg1 or
            reg2. Then use them as temporaries to make things work. */
         if (reg1 < 4 && reg2 < 4) {
            if (upd_cc) emit_get_eflags();
            emit_nonshiftopb_reg_reg(opcode, reg1, reg2); 
            if (upd_cc) emit_put_eflags();
            break;
         }
         for (s1 = 0; s1 == reg1 || s1 == reg2; s1++) ;
         if (reg1 >= 4 && reg2 < 4) {
            emit_swapl_reg_reg ( reg1, s1 );
            if (upd_cc) emit_get_eflags();
            emit_nonshiftopb_reg_reg(opcode, s1, reg2);
            if (upd_cc) emit_put_eflags();
            emit_swapl_reg_reg ( reg1, s1 );
            break;
         }
         for (s2 = 0; s2 == reg1 || s2 == reg2 || s2 == s1; s2++) ;
         if (reg1 < 4 && reg2 >= 4) {
            emit_swapl_reg_reg ( reg2, s2 );
            if (upd_cc) emit_get_eflags();
            emit_nonshiftopb_reg_reg(opcode, reg1, s2);
            if (upd_cc) emit_put_eflags();
            emit_swapl_reg_reg ( reg2, s2 );
            break;
         }
         if (reg1 >= 4 && reg2 >= 4 && reg1 != reg2) {
            emit_swapl_reg_reg ( reg1, s1 );
            emit_swapl_reg_reg ( reg2, s2 );
            if (upd_cc) emit_get_eflags();
            emit_nonshiftopb_reg_reg(opcode, s1, s2);
            if (upd_cc) emit_put_eflags();
            emit_swapl_reg_reg ( reg1, s1 );
            emit_swapl_reg_reg ( reg2, s2 );
            break;
         }
         if (reg1 >= 4 && reg2 >= 4 && reg1 == reg2) {
            emit_swapl_reg_reg ( reg1, s1 );
            if (upd_cc) emit_get_eflags();
            emit_nonshiftopb_reg_reg(opcode, s1, s1);
            if (upd_cc) emit_put_eflags();
            emit_swapl_reg_reg ( reg1, s1 );
            break;
         }
         VG_(panic)("synth_nonshiftopb_reg_reg");
      }
      default: VG_(panic)("synth_nonshiftop_reg_reg");
   }
}


static void synth_nonshiftop_offregmem_reg ( 
   Bool upd_cc,
   Opcode opcode, Int size, 
   Int off, Int areg, Int reg )
{
   switch (size) {
      case 4: 
         if (upd_cc) emit_get_eflags();
         emit_nonshiftopv_offregmem_reg ( 4, opcode, off, areg, reg ); 
         if (upd_cc) emit_put_eflags();
         break;
      case 2: 
         if (upd_cc) emit_get_eflags();
         emit_nonshiftopv_offregmem_reg ( 2, opcode, off, areg, reg ); 
         if (upd_cc) emit_put_eflags();
         break;
      case 1: 
         if (reg < 4) {
            if (upd_cc) emit_get_eflags();
            emit_nonshiftopb_offregmem_reg ( opcode, off, areg, reg );
            if (upd_cc) emit_put_eflags();
         } else {
            emit_swapl_reg_EAX ( reg );
            if (upd_cc) emit_get_eflags();
            emit_nonshiftopb_offregmem_reg ( opcode, off, areg, R_AL );
            if (upd_cc) emit_put_eflags();
            emit_swapl_reg_EAX ( reg );
         }
         break;
      default: 
         VG_(panic)("synth_nonshiftop_litmem_reg");
   }
}


static void synth_nonshiftop_lit_reg ( Bool upd_cc,
                                       Opcode opcode, Int size, 
                                       UInt lit, Int reg )
{
   switch (size) {
      case 4: if (upd_cc) emit_get_eflags();
              emit_nonshiftopv_lit_reg ( 4, opcode, lit, reg );
              if (upd_cc) emit_put_eflags();
              break;
      case 2: if (upd_cc) emit_get_eflags();
              emit_nonshiftopv_lit_reg ( 2, opcode, lit, reg );
              if (upd_cc) emit_put_eflags();
              break;
      case 1: if (reg < 4) {
                 if (upd_cc) emit_get_eflags();
                 emit_nonshiftopb_lit_reg ( opcode, lit, reg );
                 if (upd_cc) emit_put_eflags();
              } else {
                 emit_swapl_reg_EAX ( reg );
                 if (upd_cc) emit_get_eflags();
                 emit_nonshiftopb_lit_reg ( opcode, lit, R_AL );
                 if (upd_cc) emit_put_eflags();
                 emit_swapl_reg_EAX ( reg );
              }
              break;
      default: VG_(panic)("synth_nonshiftop_lit_reg");
   }
}


static void synth_push_reg ( Int size, Int reg )
{
   switch (size) {
      case 4: 
         emit_pushv_reg ( 4, reg ); 
         break;
      case 2: 
         emit_pushv_reg ( 2, reg ); 
         break;
      /* Pray that we don't have to generate this really cruddy bit of
         code very often.  Could do better, but can I be bothered? */
      case 1: 
         vg_assert(reg != R_ESP); /* duh */
         emit_add_lit_to_esp(-1);
         if (reg != R_EAX) emit_swapl_reg_EAX ( reg );
         emit_movb_AL_zeroESPmem();
         if (reg != R_EAX) emit_swapl_reg_EAX ( reg );
         break;
     default: 
         VG_(panic)("synth_push_reg");
   }
}


static void synth_pop_reg ( Int size, Int reg )
{
   switch (size) {
      case 4: 
         emit_popv_reg ( 4, reg ); 
         break;
      case 2: 
         emit_popv_reg ( 2, reg ); 
         break;
      case 1:
         /* Same comment as above applies. */
         vg_assert(reg != R_ESP); /* duh */
         if (reg != R_EAX) emit_swapl_reg_EAX ( reg );
         emit_movb_zeroESPmem_AL();
         if (reg != R_EAX) emit_swapl_reg_EAX ( reg );
         emit_add_lit_to_esp(1);
         break;
      default: VG_(panic)("synth_pop_reg");
   }
}


static void synth_shiftop_reg_reg ( Bool upd_cc,
                                    Opcode opcode, Int size, 
                                    Int regs, Int regd )
{
   synth_push_reg ( size, regd );
   if (regs != R_ECX) emit_swapl_reg_ECX ( regs );
   if (upd_cc) emit_get_eflags();
   switch (size) {
      case 4: emit_shiftopv_cl_stack0 ( 4, opcode ); break;
      case 2: emit_shiftopv_cl_stack0 ( 2, opcode ); break;
      case 1: emit_shiftopb_cl_stack0 ( opcode ); break;
      default: VG_(panic)("synth_shiftop_reg_reg");
   }
   if (upd_cc) emit_put_eflags();
   if (regs != R_ECX) emit_swapl_reg_ECX ( regs );
   synth_pop_reg ( size, regd );
}


static void synth_shiftop_lit_reg ( Bool upd_cc,
                                    Opcode opcode, Int size, 
                                    UInt lit, Int reg )
{
   switch (size) {
      case 4: if (upd_cc) emit_get_eflags();
              emit_shiftopv_lit_reg ( 4, opcode, lit, reg );
              if (upd_cc) emit_put_eflags();
              break;
      case 2: if (upd_cc) emit_get_eflags();
              emit_shiftopv_lit_reg ( 2, opcode, lit, reg );
              if (upd_cc) emit_put_eflags();
              break;
      case 1: if (reg < 4) {
                 if (upd_cc) emit_get_eflags();
                 emit_shiftopb_lit_reg ( opcode, lit, reg );
                 if (upd_cc) emit_put_eflags();
              } else {
                 emit_swapl_reg_EAX ( reg );
                 if (upd_cc) emit_get_eflags();
                 emit_shiftopb_lit_reg ( opcode, lit, R_AL );
                 if (upd_cc) emit_put_eflags();
                 emit_swapl_reg_EAX ( reg );
              }
              break;
      default: VG_(panic)("synth_nonshiftop_lit_reg");
   }
}


static void synth_setb_reg ( Int reg, Condcode cond )
{
   emit_get_eflags();
   if (reg < 4) {
      emit_setb_reg ( reg, cond );
   } else {
      emit_swapl_reg_EAX ( reg );
      emit_setb_reg ( R_AL, cond );
      emit_swapl_reg_EAX ( reg );
   }
}


static void synth_fpu_regmem ( UChar first_byte,
                               UChar second_byte_masked, 
                               Int reg )
{
   emit_get_fpu_state();
   emit_fpu_regmem ( first_byte, second_byte_masked, reg );
   emit_put_fpu_state();
}


static void synth_fpu_no_mem ( UChar first_byte,
                               UChar second_byte )
{
   emit_get_fpu_state();
   emit_fpu_no_mem ( first_byte, second_byte );
   emit_put_fpu_state();
}


static void synth_movl_reg_reg ( Int src, Int dst )
{
   emit_movl_reg_reg ( src, dst );
}

static void synth_cmovl_reg_reg ( Condcode cond, Int src, Int dst )
{
   emit_get_eflags();
   emit_jcondshort_delta ( invertCondition(cond), 
                           2 /* length of the next insn */ );
   emit_movl_reg_reg ( src, dst );
}


/* Synthesise a minimal test (and which discards result) of reg32
   against lit.  It's always safe do simply
      emit_testv_lit_reg ( 4, lit, reg32 )
   but we try to do better when possible.
*/
static void synth_minimal_test_lit_reg ( UInt lit, Int reg32 )
{
   if ((lit & 0xFFFFFF00) == 0 && reg32 < 4) {
      /* We can get away with a byte insn. */
      emit_testb_lit_reg ( lit, reg32 );
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
   VG_(panic)("spillOrArchOffset");
}


static Int eflagsOffset ( void )
{
   return 4 * VGOFF_(m_eflags);
}


static Int shadowOffset ( Int arch )
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
      default:    VG_(panic)( "shadowOffset");
   }
}


static Int shadowFlagsOffset ( void )
{
   return 4 * VGOFF_(sh_eflags);
}


static void synth_LOADV ( Int sz, Int a_reg, Int tv_reg )
{
   Int i, j, helper_offw;
   Int pushed[VG_MAX_REALREGS+2];
   Int n_pushed;
   switch (sz) {
      case 4: helper_offw = VGOFF_(helperc_LOADV4); break;
      case 2: helper_offw = VGOFF_(helperc_LOADV2); break;
      case 1: helper_offw = VGOFF_(helperc_LOADV1); break;
      default: VG_(panic)("synth_LOADV");
   }
   n_pushed = 0;
   for (i = 0; i < VG_MAX_REALREGS; i++) {
      j = VG_(rankToRealRegNo) ( i );
      if (VG_CALLEE_SAVED(j)) continue;
      if (j == tv_reg || j == a_reg) continue;
      emit_pushv_reg ( 4, j );
      pushed[n_pushed++] = j;
   }
   emit_pushv_reg ( 4, a_reg );
   pushed[n_pushed++] = a_reg;
   vg_assert(n_pushed <= VG_MAX_REALREGS+1);

   synth_call_baseBlock_method ( False, helper_offw );
   /* Result is in %eax; we need to get it to tv_reg. */
   if (tv_reg != R_EAX)
      emit_movv_reg_reg ( 4, R_EAX, tv_reg );

   while (n_pushed > 0) {
      n_pushed--;
      if (pushed[n_pushed] == tv_reg) {
         emit_add_lit_to_esp ( 4 );
      } else {
         emit_popv_reg ( 4, pushed[n_pushed] );
      }
   }
}


static void synth_STOREV ( Int sz,
                           Int tv_tag, Int tv_val,
                           Int a_reg )
{
   Int i, j, helper_offw;
   vg_assert(tv_tag == RealReg || tv_tag == Literal);
   switch (sz) {
      case 4: helper_offw = VGOFF_(helperc_STOREV4); break;
      case 2: helper_offw = VGOFF_(helperc_STOREV2); break;
      case 1: helper_offw = VGOFF_(helperc_STOREV1); break;
      default: VG_(panic)("synth_STOREV");
   }
   for (i = 0; i < VG_MAX_REALREGS; i++) {
      j = VG_(rankToRealRegNo) ( i );
      if (VG_CALLEE_SAVED(j)) continue;
      if ((tv_tag == RealReg && j == tv_val) || j == a_reg) continue;
      emit_pushv_reg ( 4, j );
   }
   if (tv_tag == RealReg) {
      emit_pushv_reg ( 4, tv_val );
   } else {
     if (tv_val == VG_(extend_s_8to32)(tv_val))
        emit_pushl_lit8 ( VG_(extend_s_8to32)(tv_val) );
     else
        emit_pushl_lit32(tv_val);
   }
   emit_pushv_reg ( 4, a_reg );
   synth_call_baseBlock_method ( False, helper_offw );
   emit_popv_reg ( 4, a_reg );
   if (tv_tag == RealReg) {
      emit_popv_reg ( 4, tv_val );
   } else {
      emit_add_lit_to_esp ( 4 );
   }
   for (i = VG_MAX_REALREGS-1; i >= 0; i--) {
      j = VG_(rankToRealRegNo) ( i );
      if (VG_CALLEE_SAVED(j)) continue;
      if ((tv_tag == RealReg && j == tv_val) || j == a_reg) continue;
      emit_popv_reg ( 4, j );
   }
}


static void synth_WIDEN_signed ( Int sz_src, Int sz_dst, Int reg )
{
   if (sz_src == 1 && sz_dst == 4) {
      emit_shiftopv_lit_reg ( 4, SHL, 24, reg );
      emit_shiftopv_lit_reg ( 4, SAR, 24, reg );
   }
   else if (sz_src == 2 && sz_dst == 4) {
      emit_shiftopv_lit_reg ( 4, SHL, 16, reg );
      emit_shiftopv_lit_reg ( 4, SAR, 16, reg );
   }
   else if (sz_src == 1 && sz_dst == 2) {
      emit_shiftopv_lit_reg ( 2, SHL, 8, reg );
      emit_shiftopv_lit_reg ( 2, SAR, 8, reg );
   }
   else
      VG_(panic)("synth_WIDEN");
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
   emit_movv_lit_reg ( 4, val, reg );
}


static void synth_TESTV ( Int sz, Int tag, Int val )
{
   vg_assert(tag == ArchReg || tag == RealReg);
   if (tag == ArchReg) {
      switch (sz) {
         case 4: 
            emit_testv_lit_offregmem ( 
               4, 0xFFFFFFFF, shadowOffset(val), R_EBP );
            break;
         case 2: 
            emit_testv_lit_offregmem ( 
               4, 0x0000FFFF, shadowOffset(val), R_EBP );
            break;
         case 1:
            if (val < 4) {
               emit_testv_lit_offregmem ( 
                  4, 0x000000FF, shadowOffset(val), R_EBP );
            } else {
               emit_testv_lit_offregmem ( 
                  4, 0x0000FF00, shadowOffset(val-4), R_EBP );
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
            emit_cmpl_zero_reg ( val );
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
   emit_jcondshort_delta ( CondZ, 3 );
   synth_call_baseBlock_method (
      True, /* needed to guarantee that this insn is indeed 3 bytes long */
      (sz==4 ? VGOFF_(helper_value_check4_fail)
             : (sz==2 ? VGOFF_(helper_value_check2_fail)
                      : sz == 1 ? VGOFF_(helper_value_check1_fail)
                                : VGOFF_(helper_value_check0_fail)))
   );
}


static void synth_GETV ( Int sz, Int arch, Int reg )
{
   /* VG_(printf)("synth_GETV %d of Arch %s\n", sz, nameIReg(sz, arch)); */
   switch (sz) {
      case 4: 
         emit_movv_offregmem_reg ( 4, shadowOffset(arch), R_EBP, reg );
         break;
      case 2: 
         emit_movzwl_offregmem_reg ( shadowOffset(arch), R_EBP, reg );
         emit_nonshiftopv_lit_reg ( 4, OR, 0xFFFF0000, reg );
         break;
      case 1: 
         if (arch < 4) {
            emit_movzbl_offregmem_reg ( shadowOffset(arch), R_EBP, reg );
         } else {
            emit_movzbl_offregmem_reg ( shadowOffset(arch-4)+1, R_EBP, reg );
         }
         emit_nonshiftopv_lit_reg ( 4, OR, 0xFFFFFF00, reg );
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
            emit_movv_lit_offregmem ( 4, 0x00000000, 
                                      shadowOffset(arch), R_EBP );
            break;
         case 2:
            vg_assert(lit == 0xFFFF0000);
            emit_movv_lit_offregmem ( 2, 0x0000, 
                                      shadowOffset(arch), R_EBP );
            break;
         case 1:
            vg_assert(lit == 0xFFFFFF00);
            if (arch < 4) {
               emit_movb_lit_offregmem ( 0x00, 
                                         shadowOffset(arch), R_EBP );
            } else {
               emit_movb_lit_offregmem ( 0x00, 
                                         shadowOffset(arch-4)+1, R_EBP );
            }
            break;
         default: 
            VG_(panic)("synth_PUTV(lit)");
      }

   } else {

      UInt reg;
      vg_assert(srcTag == RealReg);

      if (sz == 1 && lit_or_reg >= 4) {
         emit_swapl_reg_EAX ( lit_or_reg );
         reg = R_EAX;
      } else {
         reg = lit_or_reg;
      }

      if (sz == 1) vg_assert(reg < 4);

      switch (sz) {
         case 4:
            emit_movv_reg_offregmem ( 4, reg,
                                      shadowOffset(arch), R_EBP );
            break;
         case 2:
            emit_movv_reg_offregmem ( 2, reg,
                                      shadowOffset(arch), R_EBP );
            break;
         case 1:
            if (arch < 4) {
               emit_movb_reg_offregmem ( reg,
                                         shadowOffset(arch), R_EBP );
	    } else {
               emit_movb_reg_offregmem ( reg,
                                         shadowOffset(arch-4)+1, R_EBP );
            }
            break;
         default: 
            VG_(panic)("synth_PUTV(reg)");
      }

      if (sz == 1 && lit_or_reg >= 4) {
         emit_swapl_reg_EAX ( lit_or_reg );
      }
   }
}


static void synth_GETVF ( Int reg )
{
   emit_movv_offregmem_reg ( 4, shadowFlagsOffset(), R_EBP, reg );
   /* paranoia only; should be unnecessary ... */
   /* emit_nonshiftopv_lit_reg ( 4, OR, 0xFFFFFFFE, reg ); */
}


static void synth_PUTVF ( UInt reg )
{
   emit_movv_reg_offregmem ( 4, reg, shadowFlagsOffset(), R_EBP );
}


static void synth_handle_esp_assignment ( Int reg )
{
   emit_pushal();
   emit_pushv_reg ( 4, reg );
   synth_call_baseBlock_method ( False, VGOFF_(handle_esp_assignment) );
   emit_add_lit_to_esp ( 4 );
   emit_popal();
}


static void synth_fpu_mem_check_actions ( Bool isWrite, 
                                          Int size, Int a_reg )
{
   Int helper_offw
     = isWrite ? VGOFF_(fpu_write_check)
               : VGOFF_(fpu_read_check);
   emit_pushal();
   emit_pushl_lit8 ( size );
   emit_pushv_reg ( 4, a_reg );
   synth_call_baseBlock_method ( False, helper_offw );
   emit_add_lit_to_esp ( 8 );   
   emit_popal();
}


#if 0
/* FixMe.  Useful for debugging. */
void VG_(oink) ( Int n )
{
   VG_(printf)("OiNk(%d): ", n );
   VG_(show_reg_tags)( &VG_(m_shadow) );
}

static void synth_OINK ( Int n )
{
   emit_pushal();
   emit_movv_lit_reg ( 4, n, R_EBP );
   emit_pushl_reg ( R_EBP );
   emit_movv_lit_reg ( 4, (Addr)&VG_(oink), R_EBP );
   emit_call_reg ( R_EBP );
   emit_add_lit_to_esp ( 4 );
   emit_popal();
}
#endif

static void synth_TAG1_op ( VgTagOp op, Int reg )
{
   switch (op) {

      /* Scheme is
            neg<sz> %reg          -- CF = %reg==0 ? 0 : 1
            sbbl %reg, %reg       -- %reg = -CF
            or 0xFFFFFFFE, %reg   -- invalidate all bits except lowest
      */
      case VgT_PCast40:
         emit_unaryopv_reg(4, NEG, reg);
         emit_nonshiftopv_reg_reg(4, SBB, reg, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFFFFFE, reg);
         break;
      case VgT_PCast20:
         emit_unaryopv_reg(2, NEG, reg);
         emit_nonshiftopv_reg_reg(4, SBB, reg, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFFFFFE, reg);
         break;
      case VgT_PCast10:
         if (reg >= 4) {
            emit_swapl_reg_EAX(reg);
            emit_unaryopb_reg(NEG, R_EAX);
            emit_swapl_reg_EAX(reg);
         } else {
            emit_unaryopb_reg(NEG, reg);
         }
         emit_nonshiftopv_reg_reg(4, SBB, reg, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFFFFFE, reg);
         break;

      /* Scheme is
            andl $1, %reg -- %reg is 0 or 1
            negl %reg -- %reg is 0 or 0xFFFFFFFF
            and possibly an OR to invalidate unused bits.
      */
      case VgT_PCast04:
         emit_nonshiftopv_lit_reg(4, AND, 0x00000001, reg);
         emit_unaryopv_reg(4, NEG, reg);
         break;
      case VgT_PCast02:
         emit_nonshiftopv_lit_reg(4, AND, 0x00000001, reg);
         emit_unaryopv_reg(4, NEG, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFF0000, reg);
         break;
      case VgT_PCast01:
         emit_nonshiftopv_lit_reg(4, AND, 0x00000001, reg);
         emit_unaryopv_reg(4, NEG, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFFFF00, reg);
         break;

      /* Scheme is
            shl $24, %reg -- make irrelevant bits disappear
            negl %reg             -- CF = %reg==0 ? 0 : 1
            sbbl %reg, %reg       -- %reg = -CF
            and possibly an OR to invalidate unused bits.
      */
      case VgT_PCast14:
         emit_shiftopv_lit_reg(4, SHL, 24, reg);
         emit_unaryopv_reg(4, NEG, reg);
         emit_nonshiftopv_reg_reg(4, SBB, reg, reg);
         break;
      case VgT_PCast12:
         emit_shiftopv_lit_reg(4, SHL, 24, reg);
         emit_unaryopv_reg(4, NEG, reg);
         emit_nonshiftopv_reg_reg(4, SBB, reg, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFF0000, reg);
         break;
      case VgT_PCast11:
         emit_shiftopv_lit_reg(4, SHL, 24, reg);
         emit_unaryopv_reg(4, NEG, reg);
         emit_nonshiftopv_reg_reg(4, SBB, reg, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFFFF00, reg);
         break;

      /* We steal %ebp (a non-allocable reg) as a temporary:
            pushl %ebp
            movl %reg, %ebp
            negl %ebp
            orl %ebp, %reg
            popl %ebp
         This sequence turns out to be correct regardless of the 
         operation width.
      */
      case VgT_Left4:
      case VgT_Left2:
      case VgT_Left1:
         vg_assert(reg != R_EDI);
         emit_movv_reg_reg(4, reg, R_EDI);
         emit_unaryopv_reg(4, NEG, R_EDI);
         emit_nonshiftopv_reg_reg(4, OR, R_EDI, reg);
         break;

      /* These are all fairly obvious; do the op and then, if
         necessary, invalidate unused bits. */
      case VgT_SWiden14:
         emit_shiftopv_lit_reg(4, SHL, 24, reg);
         emit_shiftopv_lit_reg(4, SAR, 24, reg);
         break;
      case VgT_SWiden24:
         emit_shiftopv_lit_reg(4, SHL, 16, reg);
         emit_shiftopv_lit_reg(4, SAR, 16, reg);
         break;
      case VgT_SWiden12:
         emit_shiftopv_lit_reg(4, SHL, 24, reg);
         emit_shiftopv_lit_reg(4, SAR, 24, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFF0000, reg);
         break;
      case VgT_ZWiden14:
         emit_nonshiftopv_lit_reg(4, AND, 0x000000FF, reg);
         break;
      case VgT_ZWiden24:
         emit_nonshiftopv_lit_reg(4, AND, 0x0000FFFF, reg);
         break;
      case VgT_ZWiden12:
         emit_nonshiftopv_lit_reg(4, AND, 0x000000FF, reg);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFF0000, reg);
         break;

      default:
         VG_(panic)("synth_TAG1_op");
   }
}


static void synth_TAG2_op ( VgTagOp op, Int regs, Int regd )
{
   switch (op) {

      /* UifU is implemented by OR, since 1 means Undefined. */
      case VgT_UifU4:
      case VgT_UifU2:
      case VgT_UifU1:
      case VgT_UifU0:
         emit_nonshiftopv_reg_reg(4, OR, regs, regd);
         break;

      /* DifD is implemented by AND, since 0 means Defined. */
      case VgT_DifD4:
      case VgT_DifD2:
      case VgT_DifD1:
         emit_nonshiftopv_reg_reg(4, AND, regs, regd);
         break;

      /* ImproveAND(value, tags) = value OR tags.
	 Defined (0) value 0s give defined (0); all other -> undefined (1).
         value is in regs; tags is in regd. 
         Be paranoid and invalidate unused bits; I don't know whether 
         or not this is actually necessary. */
      case VgT_ImproveAND4_TQ:
         emit_nonshiftopv_reg_reg(4, OR, regs, regd);
         break;
      case VgT_ImproveAND2_TQ:
         emit_nonshiftopv_reg_reg(4, OR, regs, regd);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFF0000, regd);
         break;
      case VgT_ImproveAND1_TQ:
         emit_nonshiftopv_reg_reg(4, OR, regs, regd);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFFFF00, regd);
         break;

      /* ImproveOR(value, tags) = (not value) OR tags.
	 Defined (0) value 1s give defined (0); all other -> undefined (1).
         value is in regs; tags is in regd. 
         To avoid trashing value, this is implemented (re de Morgan) as
               not (value AND (not tags))
         Be paranoid and invalidate unused bits; I don't know whether 
         or not this is actually necessary. */
      case VgT_ImproveOR4_TQ:
         emit_unaryopv_reg(4, NOT, regd);
         emit_nonshiftopv_reg_reg(4, AND, regs, regd);
         emit_unaryopv_reg(4, NOT, regd);
         break;
      case VgT_ImproveOR2_TQ:
         emit_unaryopv_reg(4, NOT, regd);
         emit_nonshiftopv_reg_reg(4, AND, regs, regd);
         emit_unaryopv_reg(4, NOT, regd);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFF0000, regd);
         break;
      case VgT_ImproveOR1_TQ:
         emit_unaryopv_reg(4, NOT, regd);
         emit_nonshiftopv_reg_reg(4, AND, regs, regd);
         emit_unaryopv_reg(4, NOT, regd);
         emit_nonshiftopv_lit_reg(4, OR, 0xFFFFFF00, regd);
         break;

      default:
         VG_(panic)("synth_TAG2_op");
   }
}

/*----------------------------------------------------*/
/*--- Generate code for a single UInstr.           ---*/
/*----------------------------------------------------*/

static void emitUInstr ( Int i, UInstr* u )
{
   if (dis)
      VG_(ppUInstr)(i, u);

#  if 0
   if (0&& VG_(translations_done) >= 600) {
      Bool old_dis = dis;
      dis = False; 
      synth_OINK(i);
      dis = old_dis;
   }
#  endif

   switch (u->opcode) {

      case NOP: case CALLM_S: case CALLM_E: break;

      case INCEIP: {
         vg_assert(u->tag1 == Lit16);
         emit_addlit8_offregmem ( u->val1, R_EBP, 4 * VGOFF_(m_eip) );
         break;
      }

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

      case SETV: {
         vg_assert(VG_(clo_instrument));
         vg_assert(u->tag1 == RealReg);
         synth_SETV ( u->size, u->val1 );
         break;
      }

      case STOREV: {
         vg_assert(VG_(clo_instrument));
         vg_assert(u->tag1 == RealReg || u->tag1 == Literal);
         vg_assert(u->tag2 == RealReg);
         synth_STOREV ( u->size, u->tag1, 
                                 u->tag1==Literal ? u->lit32 : u->val1, 
                                 u->val2 );
         break;
      }

      case STORE: {
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         synth_mov_reg_memreg ( u->size, u->val1, u->val2 );
	 /* No longer possible, but retained for illustrative purposes.
         if (u->smc_check) 
            synth_orig_code_write_check ( u->size, u->val2 );
	 */
         break;
      }

      case LOADV: {
         vg_assert(VG_(clo_instrument));
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         if (0 && VG_(clo_instrument))
            emit_AMD_prefetch_reg ( u->val1 );
         synth_LOADV ( u->size, u->val1, u->val2 );
         break;
      }

      case LOAD: {
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         synth_mov_regmem_reg ( u->size, u->val1, u->val2 );
         break;
      }

      case TESTV: {
         vg_assert(VG_(clo_instrument));
         vg_assert(u->tag1 == RealReg || u->tag1 == ArchReg);
         synth_TESTV(u->size, u->tag1, u->val1);
         break;
      }

      case GETV: {
         vg_assert(VG_(clo_instrument));
         vg_assert(u->tag1 == ArchReg);
         vg_assert(u->tag2 == RealReg);
         synth_GETV(u->size, u->val1, u->val2);
         break;
      }

      case GETVF: {
         vg_assert(VG_(clo_instrument));
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->size == 0);
         synth_GETVF(u->val1);
         break;
      }

      case PUTV: {
         vg_assert(VG_(clo_instrument));
         vg_assert(u->tag1 == RealReg || u->tag1 == Literal);
         vg_assert(u->tag2 == ArchReg);
         synth_PUTV(u->size, u->tag1, 
                             u->tag1==Literal ? u->lit32 : u->val1, 
                             u->val2 );
         break;
      }

      case PUTVF: {
         vg_assert(VG_(clo_instrument));
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->size == 0);
         synth_PUTVF(u->val1);
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
             && VG_(clo_instrument)) {
            synth_handle_esp_assignment ( u->val1 );
	 }
         synth_mov_reg_offregmem ( 
            u->size, 
            u->val1, 
            spillOrArchOffset( u->size, u->tag2, u->val2 ),
            R_EBP
         );
         break;
      }

      case GETF: {
         vg_assert(u->size == 2 || u->size == 4);
         vg_assert(u->tag1 == RealReg);
         synth_mov_offregmem_reg ( 
            u->size, 
            eflagsOffset(),
            R_EBP,
            u->val1
         );
         break;
      }
            
      case PUTF: {
         vg_assert(u->size == 2 || u->size == 4);
         vg_assert(u->tag1 == RealReg);
         synth_mov_reg_offregmem ( 
            u->size, 
            u->val1,
            eflagsOffset(),
            R_EBP
         );
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
            default: VG_(panic)("emitUInstr:mov");
	 }
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
                             VG_(anyFlagUse)(u), 
                             u->opcode, u->size, u->lit32, u->val2 );
                          break;
            case RealReg: synth_nonshiftop_reg_reg (
                             VG_(anyFlagUse)(u), 
                             u->opcode, u->size, u->val1, u->val2 );
                          break;
            case ArchReg: synth_nonshiftop_offregmem_reg (
                             VG_(anyFlagUse)(u), 
                             u->opcode, u->size, 
                             spillOrArchOffset( u->size, u->tag1, u->val1 ), 
                             R_EBP,
                             u->val2 );
                          break;
            default: VG_(panic)("emitUInstr:non-shift-op");
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
                             VG_(anyFlagUse)(u), 
                             u->opcode, u->size, u->lit32, u->val2 );
                          break;
            case RealReg: synth_shiftop_reg_reg (
                             VG_(anyFlagUse)(u), 
                             u->opcode, u->size, u->val1, u->val2 );
                          break;
            default: VG_(panic)("emitUInstr:non-shift-op");
         }
         break;
      }

      case INC:
      case DEC:
      case NEG:
      case NOT:
         vg_assert(u->tag1 == RealReg);
         synth_unaryop_reg ( 
            VG_(anyFlagUse)(u), u->opcode, u->size, u->val1 );
         break;

      case BSWAP:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->size == 4);
	 vg_assert(!VG_(anyFlagUse)(u));
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
         if (u->cond == CondAlways) {
            switch (u->tag1) {
               case RealReg:
                  synth_jmp_reg ( u->val1, u->jmpkind );
                  break;
               case Literal:
                  synth_jmp_lit ( u->lit32, u->jmpkind );
                  break;
               default: 
                  VG_(panic)("emitUInstr(JMP, unconditional, default)");
                  break;
            }
         } else {
            switch (u->tag1) {
               case RealReg:
                  VG_(panic)("emitUInstr(JMP, conditional, RealReg)");
                  break;
               case Literal:
                  vg_assert(u->jmpkind == JmpBoring);
                  synth_jcond_lit ( u->cond, u->lit32 );
                  break;
               default: 
                  VG_(panic)("emitUInstr(JMP, conditional, default)");
                  break;
            }
         }
         break;
      }

      case JIFZ:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == Literal);
         vg_assert(u->size == 4);
         synth_jmp_ifzero_reg_lit ( u->val1, u->lit32 );
         break;

      case TAG1:
         synth_TAG1_op ( u->val3, u->val1 );
         break;

      case TAG2:
         if (u->val3 != VgT_DebugFn) {
            synth_TAG2_op ( u->val3, u->val1, u->val2 );
         } else {
            /* Assume a call to VgT_DebugFn passing both args
               and placing the result back in the second. */
            Int j, k;
            /* u->val2 is the reg into which the result is written.  So
               don't save/restore it.  And it can be used at a temp for
               the call target, too.  Since %eax is used for the return
               value from the C procedure, it is preserved only by
               virtue of not being mentioned as a VG_CALLEE_SAVED reg. */
            for (k = 0; k < VG_MAX_REALREGS; k++) {
               j = VG_(rankToRealRegNo) ( k );
               if (VG_CALLEE_SAVED(j)) continue;
               if (j == u->val2) continue;
               emit_pushv_reg ( 4, j );
            }
            emit_pushv_reg(4, u->val2);
            emit_pushv_reg(4, u->val1);
            emit_movv_lit_reg ( 4, (UInt)(&VG_(DebugFn)), u->val2 );
            emit_call_reg ( u->val2 );
            if (u->val2 != R_EAX)
               emit_movv_reg_reg ( 4, R_EAX, u->val2 );
            /* nuke args */
            emit_add_lit_to_esp(8);
            for (k = VG_MAX_REALREGS-1; k >= 0; k--) {
               j = VG_(rankToRealRegNo) ( k );
               if (VG_CALLEE_SAVED(j)) continue;
               if (j == u->val2) continue;
               emit_popv_reg ( 4, j );
            }
         }
         break;

      case PUSH:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == NoValue);
         emit_pushv_reg ( 4, u->val1 );
         break;

      case POP:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == NoValue);
         emit_popv_reg ( 4, u->val1 );
         break;

      case CALLM:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
         vg_assert(u->size == 0);
         if (u->flags_r != FlagsEmpty || u->flags_w != FlagsEmpty) 
            emit_get_eflags();
         synth_call_baseBlock_method ( False, u->val1 );
         if (u->flags_w != FlagsEmpty) 
            emit_put_eflags();
         break;

      case CCALL_1_0:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == NoValue);
         vg_assert(u->size == 0);

         synth_ccall_saveRegs();
         synth_ccall_pushOneArg ( u->val1 );
         synth_ccall_call_clearStack_restoreRegs ( u->lit32, 4 );
         break;

      case CCALL_2_0:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == RealReg);
         vg_assert(u->size == 0);

         synth_ccall_saveRegs();
         synth_ccall_pushTwoArgs ( u->val1, u->val2 );
         synth_ccall_call_clearStack_restoreRegs ( u->lit32, 8 );
         break;

      case CLEAR:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
         emit_add_lit_to_esp ( u->val1 );
         break;

      case CC2VAL:
         vg_assert(u->tag1 == RealReg);
         vg_assert(u->tag2 == NoValue);
         vg_assert(VG_(anyFlagUse)(u));
         synth_setb_reg ( u->val1, u->cond );
         break;

      /* We assume that writes to memory done by FPU_Ws are not going
         to be used to create new code, so there's no orig-code-write
         checks done by default. */
      case FPU_R: 
      case FPU_W:         
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == RealReg);
         if (VG_(clo_instrument))
            synth_fpu_mem_check_actions ( 
               u->opcode==FPU_W, u->size, u->val2 );
         synth_fpu_regmem ( (u->val1 >> 8) & 0xFF,
                            u->val1 & 0xFF,
                            u->val2 );
         /* No longer possible, but retained for illustrative purposes.
         if (u->opcode == FPU_W && u->smc_check) 
            synth_orig_code_write_check ( u->size, u->val2 );
         */
         break;

      case FPU:
         vg_assert(u->tag1 == Lit16);
         vg_assert(u->tag2 == NoValue);
         if (u->flags_r != FlagsEmpty || u->flags_w != FlagsEmpty) 
            emit_get_eflags();
         synth_fpu_no_mem ( (u->val1 >> 8) & 0xFF,
                            u->val1 & 0xFF );
         if (u->flags_w != FlagsEmpty) 
            emit_put_eflags();
         break;

      default: 
         VG_(printf)("emitUInstr: unhandled insn:\n");
         VG_(ppUInstr)(0,u);
         VG_(panic)("emitUInstr: unimplemented opcode");
   }

}


/* Emit x86 for the ucode in cb, returning the address of the
   generated code and setting *nbytes to its size. */
UChar* VG_(emit_code) ( UCodeBlock* cb, Int* nbytes )
{
   Int i;
   emitted_code_used = 0;
   emitted_code_size = 500; /* reasonable initial size */
   emitted_code = VG_(jitmalloc)(emitted_code_size);

   if (dis) VG_(printf)("Generated code:\n");

   for (i = 0; i < cb->used; i++) {
      if (cb->instrs[i].opcode != NOP) {
         UInstr* u = &cb->instrs[i];
#        if 1
         /* Check on the sanity of this insn. */
         Bool sane = VG_(saneUInstr)( False, u );
         if (!sane) {
            VG_(printf)("\ninsane instruction\n");
            VG_(ppUInstr)( i, u );
	 }
         vg_assert(sane);
#        endif
#        if 0
         /* Pass args to TAG1/TAG2 to vg_DebugFn for sanity checking.
            Requires a suitable definition of vg_DebugFn. */
	 if (u->opcode == TAG1) {
            UInstr t1;
            vg_assert(u->tag1 == RealReg);
            VG_(emptyUInstr)( &t1 );
            t1.opcode = TAG2;
            t1.tag1 = t1.tag2 = RealReg;
            t1.val1 = t1.val2 = u->val1;
            t1.tag3 = Lit16;
            t1.val3 = VgT_DebugFn;
            emitUInstr( i, &t1 );
	 }
	 if (u->opcode == TAG2) {
            UInstr t1;
            vg_assert(u->tag1 == RealReg);
            vg_assert(u->tag2 == RealReg);
            VG_(emptyUInstr)( &t1 );
            t1.opcode = TAG2;
            t1.tag1 = t1.tag2 = RealReg;
            t1.val1 = t1.val2 = u->val1;
            t1.tag3 = Lit16;
            t1.val3 = VgT_DebugFn;
            if (u->val3 == VgT_UifU1 || u->val3 == VgT_UifU2 
                || u->val3 == VgT_UifU4 || u->val3 == VgT_DifD1 
                || u->val3 == VgT_DifD2 || u->val3 == VgT_DifD4)
               emitUInstr( i, &t1 );
            t1.val1 = t1.val2 = u->val2;
            emitUInstr( i, &t1 );
	 }
#        endif
         emitUInstr( i, u );
      }
   }

   /* Returns a pointer to the emitted code.  This will have to be
      copied by the caller into the translation cache, and then freed
      using VG_(jitfree). */
   *nbytes = emitted_code_used;
   return emitted_code;
}

/*--------------------------------------------------------------------*/
/*--- end                                          vg_from_ucode.c ---*/
/*--------------------------------------------------------------------*/
