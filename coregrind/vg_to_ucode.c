
/*--------------------------------------------------------------------*/
/*--- The JITter: translate x86 code to ucode.                     ---*/
/*---                                                vg_to_ucode.c ---*/
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
/*--- Here so it can be inlined everywhere.                ---*/
/*------------------------------------------------------------*/

/* Allocate a new temp reg number. */
__inline__ Int VG_(get_new_temp) ( UCodeBlock* cb )
{
   Int t = cb->nextTemp;
   cb->nextTemp += 2;
   return t;
}

Int VG_(get_new_shadow) ( UCodeBlock* cb )
{
   Int t = cb->nextTemp;
   cb->nextTemp += 2;
   return SHADOW(t);
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- x86 insn stream.                                     ---*/
/*------------------------------------------------------------*/

static Char* nameGrp1 ( Int opc_aux )
{
   static Char* grp1_names[8] 
     = { "add", "or", "adc", "sbb", "and", "sub", "xor", "cmp" };
   if (opc_aux < 0 || opc_aux > 7) VG_(core_panic)("nameGrp1");
   return grp1_names[opc_aux];
}

static Char* nameGrp2 ( Int opc_aux )
{
   static Char* grp2_names[8] 
     = { "rol", "ror", "rcl", "rcr", "shl", "shr", "shl", "sar" };
   if (opc_aux < 0 || opc_aux > 7) VG_(core_panic)("nameGrp2");
   return grp2_names[opc_aux];
}

static Char* nameGrp4 ( Int opc_aux )
{
   static Char* grp4_names[8] 
     = { "inc", "dec", "???", "???", "???", "???", "???", "???" };
   if (opc_aux < 0 || opc_aux > 1) VG_(core_panic)("nameGrp4");
   return grp4_names[opc_aux];
}

static Char* nameGrp5 ( Int opc_aux )
{
   static Char* grp5_names[8] 
     = { "inc", "dec", "call*", "call*", "jmp*", "jmp*", "push", "???" };
   if (opc_aux < 0 || opc_aux > 6) VG_(core_panic)("nameGrp5");
   return grp5_names[opc_aux];
}

static Char* nameGrp8 ( Int opc_aux )
{
   static Char* grp8_names[8] 
     = { "???", "???", "???", "???", "bt", "bts", "btr", "btc" };
   if (opc_aux < 4 || opc_aux > 7) VG_(core_panic)("nameGrp8");
   return grp8_names[opc_aux];
}

Char* VG_(name_of_int_reg) ( Int size, Int reg )
{
   static Char* ireg32_names[8] 
     = { "%eax", "%ecx", "%edx", "%ebx", 
         "%esp", "%ebp", "%esi", "%edi" };
   static Char* ireg16_names[8] 
     = { "%ax", "%cx", "%dx", "%bx", "%sp", "%bp", "%si", "%di" };
   static Char* ireg8_names[8] 
     = { "%al", "%cl", "%dl", "%bl", 
         "%ah{sp}", "%ch{bp}", "%dh{si}", "%bh{di}" };
   if (reg < 0 || reg > 7) goto bad;
   switch (size) {
      case 4: return ireg32_names[reg];
      case 2: return ireg16_names[reg];
      case 1: return ireg8_names[reg];
   }
  bad:
   VG_(core_panic)("name_of_int_reg");
   return NULL; /*notreached*/
}

Char* VG_(name_of_seg_reg) ( Int sreg )
{
   switch (sreg) {
      case R_ES: return "%es";
      case R_CS: return "%cs";
      case R_SS: return "%ss";
      case R_DS: return "%ds";
      case R_FS: return "%fs";
      case R_GS: return "%gs";
      default: VG_(core_panic)("nameOfSegReg");
   }
}

Char* VG_(name_of_mmx_reg) ( Int mmxreg )
{
   static Char* mmx_names[8] 
     = { "%mm0", "%mm1", "%mm2", "%mm3", "%mm4", "%mm5", "%mm6", "%mm7" };
   if (mmxreg < 0 || mmxreg > 7) VG_(core_panic)("name_of_mmx_reg");
   return mmx_names[mmxreg];
}

Char* VG_(name_of_xmm_reg) ( Int xmmreg )
{
   static Char* xmm_names[8] 
     = { "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7" };
   if (xmmreg < 0 || xmmreg > 7) VG_(core_panic)("name_of_xmm_reg");
   return xmm_names[xmmreg];
}

Char* VG_(name_of_mmx_gran) ( UChar gran )
{
   switch (gran) {
      case 0: return "b";
      case 1: return "w";
      case 2: return "d";
      case 3: return "q";
      default: VG_(core_panic)("name_of_mmx_gran");
   }
}

Char VG_(name_of_int_size) ( Int size )
{
   switch (size) {
      case 4: return 'l';
      case 2: return 'w';
      case 1: return 'b';
      default: VG_(core_panic)("name_of_int_size");
   }
}

__inline__ UInt VG_(extend_s_8to32) ( UInt x )
{
   return (UInt)((((Int)x) << 24) >> 24);
}

__inline__ static UInt extend_s_16to32 ( UInt x )
{
   return (UInt)((((Int)x) << 16) >> 16);
}


/* Get a byte value out of the insn stream and sign-extend to 32
   bits. */
__inline__ static UInt getSDisp8 ( Addr eip0 )
{
   UChar* eip = (UChar*)eip0;
   return VG_(extend_s_8to32)( (UInt) (eip[0]) );
}

__inline__ static UInt getSDisp16 ( Addr eip0 )
{
   UChar* eip = (UChar*)eip0;
   UInt d = *eip++;
   d |= ((*eip++) << 8);
   return extend_s_16to32(d);
}

/* Get a 32-bit value out of the insn stream. */
__inline__ static UInt getUDisp32 ( Addr eip0 )
{
   UChar* eip = (UChar*)eip0;
   UInt v = eip[3]; v <<= 8;
   v |= eip[2]; v <<= 8;
   v |= eip[1]; v <<= 8;
   v |= eip[0];
   return v;
}

__inline__ static UInt getUDisp16 ( Addr eip0 )
{
   UChar* eip = (UChar*)eip0;
   UInt v = eip[1]; v <<= 8;
   v |= eip[0];
   return v;
}

__inline__ static UChar getUChar ( Addr eip0 )
{
   UChar* eip = (UChar*)eip0;
   return eip[0];
}

__inline__ static UInt LOW24 ( UInt x )
{
   return x & 0x00FFFFFF;
}

__inline__ static UInt HI8 ( UInt x )
{
   return x >> 24;
}

__inline__ static UInt getUDisp ( Int size, Addr eip )
{
   switch (size) {
      case 4: return getUDisp32(eip);
      case 2: return getUDisp16(eip);
      case 1: return getUChar(eip);
      default: VG_(core_panic)("getUDisp");
  }
  return 0; /*notreached*/
}

__inline__ static UInt getSDisp ( Int size, Addr eip )
{
   switch (size) {
      case 4: return getUDisp32(eip);
      case 2: return getSDisp16(eip);
      case 1: return getSDisp8(eip);
      default: VG_(core_panic)("getUDisp");
  }
  return 0; /*notreached*/
}


/*------------------------------------------------------------*/
/*--- Flag-related helpers.                                ---*/
/*------------------------------------------------------------*/

static void setFlagsFromUOpcode ( UCodeBlock* cb, Int uopc )
{
   switch (uopc) {
      case XOR: case OR: case AND:
         uFlagsRWU(cb, FlagsEmpty, FlagsOSZCP,  FlagA); break;
      case ADC: case SBB: 
         uFlagsRWU(cb, FlagC,      FlagsOSZACP, FlagsEmpty); break;
      case ADD: case SUB: case NEG: 
         uFlagsRWU(cb, FlagsEmpty, FlagsOSZACP, FlagsEmpty); break;
      case INC: case DEC:
         uFlagsRWU(cb, FlagsEmpty, FlagsOSZAP,  FlagsEmpty); break;
      case SHR: case SAR: case SHL:
         uFlagsRWU(cb, FlagsEmpty, FlagsOSZCP,  FlagA); break;
      case ROL: case ROR:
         uFlagsRWU(cb, FlagsEmpty, FlagsOC,     FlagsEmpty); break;
      case RCR: case RCL: 
         uFlagsRWU(cb, FlagC,      FlagsOC,     FlagsEmpty); break;
      case NOT:
         uFlagsRWU(cb, FlagsEmpty, FlagsEmpty,  FlagsEmpty); break;
      default: 
         VG_(printf)("unhandled case is %s\n", 
                     VG_(name_UOpcode)(True, uopc));
         VG_(core_panic)("setFlagsFromUOpcode: unhandled case");
   }
}

__inline__
void VG_(set_cond_field) ( UCodeBlock* cb, Condcode cond )
{
   LAST_UINSTR(cb).cond = cond;
}


/*------------------------------------------------------------*/
/*--- Disassembling addressing modes                       ---*/
/*------------------------------------------------------------*/

static 
UChar* sorbTxt ( UChar sorb )
{
   switch (sorb) {
      case 0:    return ""; /* no override */
      case 0x3E: return "%ds";
      case 0x26: return "%es:";
      case 0x64: return "%fs:";
      case 0x65: return "%gs:";
      default: VG_(core_panic)("sorbTxt");
   }
}


/* Tmp is a TempReg holding a virtual address.  Convert it to a linear
   address by adding any required segment override as indicated by
   sorb. */
static
void handleSegOverride ( UCodeBlock* cb, UChar sorb, Int tmp )
{
   Int sreg, tsreg;

   if (sorb == 0)
      /* the common case - no override */
      return;

   switch (sorb) {
      case 0x3E: sreg = R_DS; break;
      case 0x26: sreg = R_ES; break;
      case 0x64: sreg = R_FS; break;
      case 0x65: sreg = R_GS; break;
      default: VG_(core_panic)("handleSegOverride");
   }

   tsreg = newTemp(cb);

   /* sreg -> tsreg */
   uInstr2(cb, GETSEG, 2, ArchRegS, sreg, TempReg, tsreg );

   /* tmp += segment_base(ldt[tsreg]); also do limit check */
   uInstr2(cb, USESEG, 0, TempReg, tsreg, TempReg, tmp );
}


/* Generate ucode to calculate an address indicated by a ModRM and
   following SIB bytes, getting the value in a new temporary.  The
   temporary, and the number of bytes in the address mode, are
   returned, as a pair (length << 24) | temp.  Note that this fn should
   not be called if the R/M part of the address denotes a register
   instead of memory.  If buf is non-NULL, text of the addressing mode
   is placed therein. */

static 
UInt disAMode ( UCodeBlock* cb, UChar sorb, Addr eip0, UChar* buf )
{
   UChar* eip        = (UChar*)eip0;
   UChar  mod_reg_rm = *eip++;
   Int    tmp        = newTemp(cb);

   /* squeeze out the reg field from mod_reg_rm, since a 256-entry
      jump table seems a bit excessive. 
   */
   mod_reg_rm &= 0xC7;               /* is now XX000YYY */
   mod_reg_rm |= (mod_reg_rm >> 3);  /* is now XX0XXYYY */
   mod_reg_rm &= 0x1F;               /* is now 000XXYYY */
   switch (mod_reg_rm) {

      /* (%eax) .. (%edi), not including (%esp) or (%ebp).
         --> GET %reg, t 
      */
      case 0x00: case 0x01: case 0x02: case 0x03: 
      /* ! 04 */ /* ! 05 */ case 0x06: case 0x07:
         { UChar rm  = mod_reg_rm;
           uInstr2(cb, GET, 4, ArchReg, rm,  TempReg, tmp);
           handleSegOverride(cb, sorb, tmp);
           if (buf) VG_(sprintf)(buf,"%s(%s)", sorbTxt(sorb), 
                                               nameIReg(4,rm));
           return (1<<24 | tmp);
         }

      /* d8(%eax) ... d8(%edi), not including d8(%esp) 
         --> GET %reg, t ; ADDL d8, t
      */
      case 0x08: case 0x09: case 0x0A: case 0x0B: 
      /* ! 0C */ case 0x0D: case 0x0E: case 0x0F:
         { UChar rm  = mod_reg_rm & 7;
           Int   tmq = newTemp(cb);
           UInt  d   = getSDisp8((Addr)eip); eip++;
           uInstr2(cb, GET,  4, ArchReg, rm,  TempReg, tmq);
           uInstr2(cb, LEA1, 4, TempReg, tmq, TempReg, tmp);
           LAST_UINSTR(cb).lit32 = d;
           handleSegOverride(cb, sorb, tmp);
           if (buf) VG_(sprintf)(buf,"%s%d(%s)", sorbTxt(sorb), 
                                                 d, nameIReg(4,rm));
           return (2<<24 | tmp);
         }

      /* d32(%eax) ... d32(%edi), not including d32(%esp)
         --> GET %reg, t ; ADDL d8, t
      */
      case 0x10: case 0x11: case 0x12: case 0x13: 
      /* ! 14 */ case 0x15: case 0x16: case 0x17:
         { UChar rm  = mod_reg_rm & 7;
           Int   tmq = newTemp(cb);
           UInt  d   = getUDisp32((Addr)eip); eip += 4;
           uInstr2(cb, GET,  4, ArchReg, rm,  TempReg, tmq);
           uInstr2(cb, LEA1, 4, TempReg, tmq, TempReg, tmp);
           LAST_UINSTR(cb).lit32 = d;
           handleSegOverride(cb, sorb, tmp);
           if (buf) VG_(sprintf)(buf,"%s0x%x(%s)", sorbTxt(sorb), 
                                                   d, nameIReg(4,rm));
           return (5<<24 | tmp);
         }

      /* a register, %eax .. %edi.  This shouldn't happen. */
      case 0x18: case 0x19: case 0x1A: case 0x1B:
      case 0x1C: case 0x1D: case 0x1E: case 0x1F:
         VG_(core_panic)("disAMode: not an addr!");

      /* a 32-bit literal address
         --> MOV d32, tmp 
      */
      case 0x05: 
         { UInt d = getUDisp32((Addr)eip); eip += 4;
           uInstr2(cb, MOV, 4, Literal, 0, TempReg, tmp);
           uLiteral(cb, d);
           handleSegOverride(cb, sorb, tmp);
           if (buf) VG_(sprintf)(buf,"%s(0x%x)", sorbTxt(sorb), d);
           return (5<<24 | tmp);
         }

      case 0x04: {
         /* SIB, with no displacement.  Special cases:
            -- %esp cannot act as an index value.  
               If index_r indicates %esp, zero is used for the index.
            -- when mod is zero and base indicates EBP, base is instead
               a 32-bit literal.
            It's all madness, I tell you.  Extract %index, %base and 
            scale from the SIB byte.  The value denoted is then:
               | %index == %ESP && %base == %EBP
               = d32 following SIB byte
               | %index == %ESP && %base != %EBP
               = %base
               | %index != %ESP && %base == %EBP
               = d32 following SIB byte + (%index << scale)
               | %index != %ESP && %base != %ESP
               = %base + (%index << scale)

            What happens to the souls of CPU architects who dream up such
            horrendous schemes, do you suppose?  
         */
         UChar sib     = *eip++;
         UChar scale   = (sib >> 6) & 3;
         UChar index_r = (sib >> 3) & 7;
         UChar base_r  = sib & 7;

         if (index_r != R_ESP && base_r != R_EBP) {
            Int index_tmp = newTemp(cb);
            Int base_tmp  = newTemp(cb);
            uInstr2(cb, GET,  4, ArchReg, index_r,  TempReg, index_tmp);
            uInstr2(cb, GET,  4, ArchReg, base_r,   TempReg, base_tmp);
            uInstr3(cb, LEA2, 4, TempReg, base_tmp, TempReg, index_tmp, 
                                 TempReg, tmp);
            LAST_UINSTR(cb).lit32   = 0;
            LAST_UINSTR(cb).extra4b = 1 << scale;
            handleSegOverride(cb, sorb, tmp);
            if (buf) VG_(sprintf)(buf,"%s(%s,%s,%d)", sorbTxt(sorb), 
                                  nameIReg(4,base_r),
                                  nameIReg(4,index_r),1<<scale);
            return (2<<24 | tmp);
         }

         if (index_r != R_ESP && base_r == R_EBP) {
            Int index_tmp = newTemp(cb);
            UInt d = getUDisp32((Addr)eip); eip += 4;
            uInstr2(cb, GET,  4, ArchReg, index_r,  TempReg, index_tmp);
            uInstr2(cb, MOV,  4, Literal, 0,        TempReg, tmp);
            uLiteral(cb, 0);
            uInstr3(cb, LEA2, 4, TempReg, tmp,      TempReg, index_tmp, 
                                 TempReg, tmp);
            LAST_UINSTR(cb).lit32   = d;
            LAST_UINSTR(cb).extra4b = 1 << scale;
            handleSegOverride(cb, sorb, tmp);
            if (buf) VG_(sprintf)(buf,"%s0x%x(,%s,%d)", sorbTxt(sorb), d, 
                                  nameIReg(4,index_r),1<<scale);
            return (6<<24 | tmp);
         }

         if (index_r == R_ESP && base_r != R_EBP) {
            uInstr2(cb, GET, 4, ArchReg, base_r, TempReg, tmp);
            handleSegOverride(cb, sorb, tmp);
            if (buf) VG_(sprintf)(buf,"%s(%s,,)", 
                                      sorbTxt(sorb), nameIReg(4,base_r));
            return (2<<24 | tmp);
         }

         if (index_r == R_ESP && base_r == R_EBP) {
            UInt d = getUDisp32((Addr)eip); eip += 4;
            uInstr2(cb, MOV, 4, Literal, 0, TempReg, tmp);
	    uLiteral(cb, d);
            handleSegOverride(cb, sorb, tmp);
            if (buf) VG_(sprintf)(buf,"%s0x%x()", sorbTxt(sorb), d);
            return (6<<24 | tmp);
         }

         vg_assert(0);
      }

      /* SIB, with 8-bit displacement.  Special cases:
         -- %esp cannot act as an index value.  
            If index_r indicates %esp, zero is used for the index.
         Denoted value is:
            | %index == %ESP
            = d8 + %base
            | %index != %ESP
            = d8 + %base + (%index << scale)
      */
      case 0x0C: {
         UChar sib     = *eip++;
         UChar scale   = (sib >> 6) & 3;
         UChar index_r = (sib >> 3) & 7;
         UChar base_r  = sib & 7;
         UInt d        = getSDisp8((Addr)eip); eip++;

         if (index_r == R_ESP) {
            Int tmq = newTemp(cb);
            uInstr2(cb, GET,  4, ArchReg, base_r,  TempReg, tmq);
            uInstr2(cb, LEA1, 4, TempReg, tmq, TempReg, tmp);
            LAST_UINSTR(cb).lit32 = d;
            handleSegOverride(cb, sorb, tmp);
            if (buf) VG_(sprintf)(buf,"%s%d(%s,,)", sorbTxt(sorb), 
                                  d, nameIReg(4,base_r));
            return (3<<24 | tmp);
         } else {
            Int index_tmp = newTemp(cb);
            Int base_tmp  = newTemp(cb);
            uInstr2(cb, GET, 4,  ArchReg, index_r,  TempReg, index_tmp);
            uInstr2(cb, GET, 4,  ArchReg, base_r,   TempReg, base_tmp);
            uInstr3(cb, LEA2, 4, TempReg, base_tmp, TempReg, index_tmp, 
                                 TempReg, tmp);
            LAST_UINSTR(cb).lit32   = d;
            LAST_UINSTR(cb).extra4b = 1 << scale;
            handleSegOverride(cb, sorb, tmp);
            if (buf) VG_(sprintf)(buf,"%s%d(%s,%s,%d)", 
                                  sorbTxt(sorb), d, nameIReg(4,base_r), 
                                  nameIReg(4,index_r), 1<<scale);
            return (3<<24 | tmp);
         }
         vg_assert(0);
      }

      /* SIB, with 32-bit displacement.  Special cases:
         -- %esp cannot act as an index value.  
            If index_r indicates %esp, zero is used for the index.
         Denoted value is:
            | %index == %ESP
            = d32 + %base
            | %index != %ESP
            = d32 + %base + (%index << scale)
      */
      case 0x14: {
         UChar sib     = *eip++;
         UChar scale   = (sib >> 6) & 3;
         UChar index_r = (sib >> 3) & 7;
         UChar base_r  = sib & 7;
         UInt d        = getUDisp32((Addr)eip); eip += 4;

         if (index_r == R_ESP) {
            Int tmq = newTemp(cb);
            uInstr2(cb, GET,  4, ArchReg, base_r,  TempReg, tmq);
            uInstr2(cb, LEA1, 4, TempReg, tmq, TempReg, tmp);
            LAST_UINSTR(cb).lit32 = d;
            handleSegOverride(cb, sorb, tmp);
            if (buf) VG_(sprintf)(buf,"%s%d(%s,,)", 
                                  sorbTxt(sorb), d, nameIReg(4,base_r));
            return (6<<24 | tmp);
         } else {
            Int index_tmp = newTemp(cb);
            Int base_tmp = newTemp(cb);
            uInstr2(cb, GET,  4, ArchReg, index_r, TempReg, index_tmp);
            uInstr2(cb, GET,  4, ArchReg, base_r, TempReg, base_tmp);
            uInstr3(cb, LEA2, 4, TempReg, base_tmp, TempReg, index_tmp, 
                                 TempReg, tmp);
            LAST_UINSTR(cb).lit32   = d;
            LAST_UINSTR(cb).extra4b = 1 << scale;
            handleSegOverride(cb, sorb, tmp);
            if (buf) VG_(sprintf)(buf,"%s%d(%s,%s,%d)", 
                                  sorbTxt(sorb), d, nameIReg(4,base_r), 
                                  nameIReg(4,index_r), 1<<scale);
            return (6<<24 | tmp);
         }
         vg_assert(0);
      }

      default:
         VG_(core_panic)("disAMode");
         return 0; /*notreached*/
   }
}


/* Figure out the number of (insn-stream) bytes constituting the amode
   beginning at eip0.  Is useful for getting hold of literals beyond
   the end of the amode before it has been disassembled.  */

static UInt lengthAMode ( Addr eip0 )
{
   UChar* eip        = (UChar*)eip0;
   UChar  mod_reg_rm = *eip++;

   /* squeeze out the reg field from mod_reg_rm, since a 256-entry
      jump table seems a bit excessive. 
   */
   mod_reg_rm &= 0xC7;               /* is now XX000YYY */
   mod_reg_rm |= (mod_reg_rm >> 3);  /* is now XX0XXYYY */
   mod_reg_rm &= 0x1F;               /* is now 000XXYYY */
   switch (mod_reg_rm) {

      /* (%eax) .. (%edi), not including (%esp) or (%ebp). */
      case 0x00: case 0x01: case 0x02: case 0x03: 
      /* ! 04 */ /* ! 05 */ case 0x06: case 0x07:
         return 1;

      /* d8(%eax) ... d8(%edi), not including d8(%esp). */ 
      case 0x08: case 0x09: case 0x0A: case 0x0B: 
      /* ! 0C */ case 0x0D: case 0x0E: case 0x0F:
         return 2;

      /* d32(%eax) ... d32(%edi), not including d32(%esp). */
      case 0x10: case 0x11: case 0x12: case 0x13: 
      /* ! 14 */ case 0x15: case 0x16: case 0x17:
         return 5;

      /* a register, %eax .. %edi.  (Not an addr, but still handled.) */
      case 0x18: case 0x19: case 0x1A: case 0x1B:
      case 0x1C: case 0x1D: case 0x1E: case 0x1F:
         return 1;

      /* a 32-bit literal address. */
      case 0x05: return 5;

      /* SIB, no displacement.  */
      case 0x04: {
         UChar sib     = *eip++;
         UChar base_r  = sib & 7;
         if (base_r == R_EBP) return 6; else return 2;
      }
      /* SIB, with 8-bit displacement.  */
      case 0x0C: return 3;

      /* SIB, with 32-bit displacement.  */
      case 0x14: return 6;

      default:
         VG_(core_panic)("amode_from_RM");
         return 0; /*notreached*/
   }
}


/* Extract the reg field from a modRM byte. */
static __inline__ Int gregOfRM ( UChar mod_reg_rm )
{
   return (Int)( (mod_reg_rm >> 3) & 7 );
}

/* Figure out whether the mod and rm parts of a modRM byte refer to a
   register or memory.  If so, the byte will have the form 11XXXYYY,
   where YYY is the register number. */
static __inline__ Bool epartIsReg ( UChar mod_reg_rm )
{
   return (0xC0 == (mod_reg_rm & 0xC0));
}

/* ... and extract the register number ... */
static __inline__ Int eregOfRM ( UChar mod_reg_rm )
{
   return (Int)(mod_reg_rm & 0x7);
}


/*------------------------------------------------------------*/
/*--- Disassembling common idioms                          ---*/
/*------------------------------------------------------------*/

static
void codegen_XOR_reg_with_itself ( UCodeBlock* cb, Int size, 
                                   Int ge_reg, Int tmp )
{
   if (dis) 
      VG_(printf)("xor%c %s, %s\n", nameISize(size),
                  nameIReg(size,ge_reg), nameIReg(size,ge_reg) );
   uInstr2(cb, MOV, size, Literal, 0, TempReg, tmp);
   uLiteral(cb, 0);
   uInstr2(cb, XOR, size, TempReg, tmp, TempReg, tmp);
   setFlagsFromUOpcode(cb, XOR);
   uInstr2(cb, PUT, size, TempReg, tmp, ArchReg, ge_reg);
}


/* Handle binary integer instructions of the form
      op E, G  meaning
      op reg-or-mem, reg
   Is passed the a ptr to the modRM byte, the actual operation, and the
   data size.  Returns the address advanced completely over this
   instruction.

   E(src) is reg-or-mem
   G(dst) is reg.

   If E is reg, -->    GET %G,  tmp
                       OP %E,   tmp
                       PUT tmp, %G
 
   If E is mem and OP is not reversible, 
                -->    (getAddr E) -> tmpa
                       LD (tmpa), tmpa
                       GET %G, tmp2
                       OP tmpa, tmp2
                       PUT tmp2, %G

   If E is mem and OP is reversible
                -->    (getAddr E) -> tmpa
                       LD (tmpa), tmpa
                       OP %G, tmpa
                       PUT tmpa, %G
*/
static
Addr dis_op2_E_G ( UCodeBlock* cb,
                   UChar       sorb,
                   Opcode      opc, 
                   Bool        keep,
                   Int         size, 
                   Addr        eip0,
                   Char*       t_x86opc )
{
   Bool  reversible;
   UChar rm = getUChar(eip0);
   UChar dis_buf[50];

   if (epartIsReg(rm)) {
      Int tmp = newTemp(cb);

      /* Specially handle XOR reg,reg, because that doesn't really
         depend on reg, and doing the obvious thing potentially
         generates a spurious value check failure due to the bogus
         dependency. */
      if (opc == XOR && gregOfRM(rm) == eregOfRM(rm)) {
         codegen_XOR_reg_with_itself ( cb, size, gregOfRM(rm), tmp );
         return 1+eip0;
      }

      uInstr2(cb, GET, size, ArchReg, gregOfRM(rm), TempReg, tmp);
      if (opc == AND || opc == OR) {
         Int tao = newTemp(cb);
         uInstr2(cb, GET, size, ArchReg, eregOfRM(rm), TempReg, tao); 
         uInstr2(cb, opc, size, TempReg, tao, TempReg, tmp);
         setFlagsFromUOpcode(cb, opc);
      } else {
         uInstr2(cb, opc, size, ArchReg, eregOfRM(rm), TempReg, tmp);
         setFlagsFromUOpcode(cb, opc);
      }
      if (keep)
         uInstr2(cb, PUT, size, TempReg, tmp, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                           nameIReg(size,eregOfRM(rm)),
                           nameIReg(size,gregOfRM(rm)));
      return 1+eip0;
   }

   /* E refers to memory */    
   reversible
      = (opc == ADD || opc == OR || opc == AND || opc == XOR || opc == ADC)
           ? True : False;
   if (reversible) {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      uInstr2(cb, LOAD, size, TempReg, tmpa, TempReg, tmpa);

      if (opc == AND || opc == OR) {
         Int tao = newTemp(cb);
         uInstr2(cb, GET, size, ArchReg, gregOfRM(rm), TempReg, tao); 
         uInstr2(cb, opc, size, TempReg, tao, TempReg, tmpa);
         setFlagsFromUOpcode(cb, opc);
      } else {
         uInstr2(cb, opc,  size, ArchReg, gregOfRM(rm), TempReg, tmpa);
         setFlagsFromUOpcode(cb, opc);
      }
      if (keep)
         uInstr2(cb, PUT,  size, TempReg, tmpa, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                           dis_buf,nameIReg(size,gregOfRM(rm)));
      return HI8(pair)+eip0;
   } else {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      Int  tmp2 = newTemp(cb);
      uInstr2(cb, LOAD, size, TempReg, tmpa, TempReg, tmpa);
      uInstr2(cb, GET,  size, ArchReg, gregOfRM(rm), TempReg, tmp2);
      uInstr2(cb, opc,  size, TempReg, tmpa, TempReg, tmp2);
      setFlagsFromUOpcode(cb, opc);
      if (keep)
         uInstr2(cb, PUT,  size, TempReg, tmp2, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                           dis_buf,nameIReg(size,gregOfRM(rm)));
      return HI8(pair)+eip0;
   }
}



/* Handle binary integer instructions of the form
      op G, E  meaning
      op reg, reg-or-mem
   Is passed the a ptr to the modRM byte, the actual operation, and the
   data size.  Returns the address advanced completely over this
   instruction.

   G(src) is reg.
   E(dst) is reg-or-mem

   If E is reg, -->    GET %E,  tmp
                       OP %G,   tmp
                       PUT tmp, %E
 
   If E is mem, -->    (getAddr E) -> tmpa
                       LD (tmpa), tmpv
                       OP %G, tmpv
                       ST tmpv, (tmpa)
*/
static
Addr dis_op2_G_E ( UCodeBlock* cb, 
                   UChar       sorb,
                   Opcode      opc, 
                   Bool        keep,
                   Int         size, 
                   Addr        eip0,
                   Char*       t_x86opc )
{
   UChar rm = getUChar(eip0);
   UChar dis_buf[50];

   if (epartIsReg(rm)) {
      Int tmp = newTemp(cb);

      /* Specially handle XOR reg,reg, because that doesn't really
         depend on reg, and doing the obvious thing potentially
         generates a spurious value check failure due to the bogus
         dependency. */
      if (opc == XOR && gregOfRM(rm) == eregOfRM(rm)) {
         codegen_XOR_reg_with_itself ( cb, size, gregOfRM(rm), tmp );
         return 1+eip0;
      }

      uInstr2(cb, GET, size, ArchReg, eregOfRM(rm), TempReg, tmp);

      if (opc == AND || opc == OR) {
         Int tao = newTemp(cb);
         uInstr2(cb, GET, size, ArchReg, gregOfRM(rm), TempReg, tao); 
         uInstr2(cb, opc, size, TempReg, tao, TempReg, tmp);
         setFlagsFromUOpcode(cb, opc);
      } else {
         uInstr2(cb, opc, size, ArchReg, gregOfRM(rm), TempReg, tmp);
         setFlagsFromUOpcode(cb, opc);
      }
      if (keep)
         uInstr2(cb, PUT, size, TempReg, tmp, ArchReg, eregOfRM(rm));
      if (dis) VG_(printf)("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                           nameIReg(size,gregOfRM(rm)),
                           nameIReg(size,eregOfRM(rm)));
      return 1+eip0;
   }

   /* E refers to memory */    
   {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      Int  tmpv = newTemp(cb);
      uInstr2(cb, LOAD,  size, TempReg, tmpa, TempReg, tmpv);

      if (opc == AND || opc == OR) {
         Int tao = newTemp(cb);
         uInstr2(cb, GET, size, ArchReg, gregOfRM(rm), TempReg, tao); 
         uInstr2(cb, opc, size, TempReg, tao, TempReg, tmpv);
         setFlagsFromUOpcode(cb, opc);
      } else {
         uInstr2(cb, opc, size, ArchReg, gregOfRM(rm), TempReg, tmpv);
         setFlagsFromUOpcode(cb, opc);
      }
      if (keep) {
         uInstr2(cb, STORE, size, TempReg, tmpv, TempReg, tmpa);
      }
      if (dis) VG_(printf)("%s%c %s,%s\n", t_x86opc, nameISize(size), 
                           nameIReg(size,gregOfRM(rm)), dis_buf);
      return HI8(pair)+eip0;
   }
}


/* Handle move instructions of the form
      mov E, G  meaning
      mov reg-or-mem, reg
   Is passed the a ptr to the modRM byte, and the data size.  Returns
   the address advanced completely over this instruction.

   E(src) is reg-or-mem
   G(dst) is reg.

   If E is reg, -->    GET %E,  tmpv
                       PUT tmpv, %G
 
   If E is mem  -->    (getAddr E) -> tmpa
                       LD (tmpa), tmpb
                       PUT tmpb, %G
*/
static
Addr dis_mov_E_G ( UCodeBlock* cb, 
                   UChar       sorb,
                   Int         size, 
                   Addr        eip0 )
{
   UChar rm  = getUChar(eip0);
   UChar dis_buf[50];

   if (epartIsReg(rm)) {
      Int tmpv = newTemp(cb);
      uInstr2(cb, GET, size, ArchReg, eregOfRM(rm), TempReg, tmpv);
      uInstr2(cb, PUT, size, TempReg, tmpv, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("mov%c %s,%s\n", nameISize(size), 
                           nameIReg(size,eregOfRM(rm)),
                           nameIReg(size,gregOfRM(rm)));
      return 1+eip0;
   }

   /* E refers to memory */    
   {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      Int  tmpb = newTemp(cb);
      uInstr2(cb, LOAD, size, TempReg, tmpa, TempReg, tmpb);
      uInstr2(cb, PUT,  size, TempReg, tmpb, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("mov%c %s,%s\n", nameISize(size), 
                           dis_buf,nameIReg(size,gregOfRM(rm)));
      return HI8(pair)+eip0;
   }
}


/* Handle move instructions of the form
      mov G, E  meaning
      mov reg, reg-or-mem
   Is passed the a ptr to the modRM byte, and the data size.  Returns
   the address advanced completely over this instruction.

   G(src) is reg.
   E(dst) is reg-or-mem

   If E is reg, -->    GET %G,  tmp
                       PUT tmp, %E
 
   If E is mem, -->    (getAddr E) -> tmpa
                       GET %G, tmpv
                       ST tmpv, (tmpa) 
*/
static
Addr dis_mov_G_E ( UCodeBlock* cb, 
                   UChar       sorb,
                   Int         size, 
                   Addr        eip0 )
{
   UChar rm = getUChar(eip0);
   UChar dis_buf[50];

   if (epartIsReg(rm)) {
      Int tmpv = newTemp(cb);
      uInstr2(cb, GET, size, ArchReg, gregOfRM(rm), TempReg, tmpv);
      uInstr2(cb, PUT, size, TempReg, tmpv, ArchReg, eregOfRM(rm));
      if (dis) VG_(printf)("mov%c %s,%s\n", nameISize(size), 
                           nameIReg(size,gregOfRM(rm)),
                           nameIReg(size,eregOfRM(rm)));
      return 1+eip0;
   }

   /* E refers to memory */    
   {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      Int  tmpv = newTemp(cb);
      uInstr2(cb, GET,   size, ArchReg, gregOfRM(rm), TempReg, tmpv);
      uInstr2(cb, STORE, size, TempReg, tmpv, TempReg, tmpa);
      if (dis) VG_(printf)("mov%c %s,%s\n", nameISize(size), 
                           nameIReg(size,gregOfRM(rm)), dis_buf);
      return HI8(pair)+eip0;
   }
}


/* op $immediate, AL/AX/EAX. */
static
Addr dis_op_imm_A ( UCodeBlock* cb, 
                    Int         size,
                    Opcode      opc,
                    Bool        keep,
                    Addr        eip,
                    Char*       t_x86opc )
{
   Int  tmp = newTemp(cb);
   UInt lit = getUDisp(size,eip);
   uInstr2(cb, GET, size, ArchReg, R_EAX, TempReg, tmp);
   if (opc == AND || opc == OR) {
      Int tao = newTemp(cb);
      uInstr2(cb, MOV, size, Literal, 0, TempReg, tao);
      uLiteral(cb, lit);
      uInstr2(cb, opc, size, TempReg, tao, TempReg, tmp);
      setFlagsFromUOpcode(cb, opc);
   } else {
      uInstr2(cb, opc, size, Literal, 0, TempReg, tmp);
      uLiteral(cb, lit);
      setFlagsFromUOpcode(cb, opc);
   }
   if (keep)
      uInstr2(cb, PUT, size, TempReg, tmp, ArchReg, R_EAX);
   if (dis) VG_(printf)("%s%c $0x%x, %s\n", t_x86opc, nameISize(size), 
                        lit, nameIReg(size,R_EAX));
   return eip+size;
}


/* Sign- and Zero-extending moves. */
static
Addr dis_movx_E_G ( UCodeBlock* cb, 
                    UChar       sorb,
                    Addr eip, Int szs, Int szd, Bool sign_extend )
{
   UChar dis_buf[50];
   UChar rm = getUChar(eip);
   if (epartIsReg(rm)) {
      Int tmpv = newTemp(cb);
      uInstr2(cb, GET, szs, ArchReg, eregOfRM(rm), TempReg, tmpv);
      uInstr1(cb, WIDEN, szd, TempReg, tmpv);
      LAST_UINSTR(cb).extra4b = szs;
      LAST_UINSTR(cb).signed_widen = sign_extend;
      uInstr2(cb, PUT, szd, TempReg, tmpv, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("mov%c%c%c %s,%s\n", 
                           sign_extend ? 's' : 'z',
                           nameISize(szs), nameISize(szd),
                           nameIReg(szs,eregOfRM(rm)),
                           nameIReg(szd,gregOfRM(rm)));
      return 1+eip;
   }

   /* E refers to memory */    
   {
      UInt pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      uInstr2(cb, LOAD, szs, TempReg, tmpa, TempReg, tmpa);
      uInstr1(cb, WIDEN, szd, TempReg, tmpa);
      LAST_UINSTR(cb).extra4b = szs;
      LAST_UINSTR(cb).signed_widen = sign_extend;
      uInstr2(cb, PUT, szd, TempReg, tmpa, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("mov%c%c%c %s,%s\n", 
                           sign_extend ? 's' : 'z',
                           nameISize(szs), nameISize(szd),
                           dis_buf,
                           nameIReg(szd,gregOfRM(rm)));
      return HI8(pair)+eip;
   }
}


/* Generate code to divide ArchRegs EDX:EAX / DX:AX / AX by the 32 /
   16 / 8 bit quantity in the given TempReg.  */
static
void codegen_div ( UCodeBlock* cb, Int sz, Int t, Bool signed_divide )
{
   Int  helper;
   Int  ta = newTemp(cb);
   Int  td = newTemp(cb);

   switch (sz) {
      case 4: helper = (signed_divide ? VGOFF_(helper_idiv_64_32) 
                                      : VGOFF_(helper_div_64_32));
              break;
      case 2: helper = (signed_divide ? VGOFF_(helper_idiv_32_16) 
                                      : VGOFF_(helper_div_32_16));
              break;
      case 1: helper = (signed_divide ? VGOFF_(helper_idiv_16_8)
                                      : VGOFF_(helper_div_16_8));
              break;
      default: VG_(core_panic)("codegen_div");
   }
   uInstr0(cb, CALLM_S, 0);
   if (sz == 4 || sz == 2) {
      uInstr1(cb, PUSH,  sz, TempReg, t);
      uInstr2(cb, GET,   sz, ArchReg, R_EAX,  TempReg, ta);
      uInstr1(cb, PUSH,  sz, TempReg, ta);
      uInstr2(cb, GET,   sz, ArchReg, R_EDX,  TempReg, td);
      uInstr1(cb, PUSH,  sz, TempReg, td);
      uInstr1(cb, CALLM,  0, Lit16,   helper);
      uFlagsRWU(cb, FlagsEmpty, FlagsEmpty, FlagsOSZACP);
      uInstr1(cb, POP,   sz, TempReg, t);
      uInstr2(cb, PUT,   sz, TempReg, t,      ArchReg, R_EDX);
      uInstr1(cb, POP,   sz, TempReg, t);
      uInstr2(cb, PUT,   sz, TempReg, t,      ArchReg, R_EAX);
      uInstr1(cb, CLEAR,  0, Lit16,   4);
   } else {
      uInstr1(cb, PUSH,  1, TempReg, t);
      uInstr2(cb, GET,   2, ArchReg, R_EAX,  TempReg, ta);
      uInstr1(cb, PUSH,  2, TempReg, ta);
      uInstr2(cb, MOV,   1, Literal, 0,      TempReg, td);
      uLiteral(cb, 0);
      uInstr1(cb, PUSH,  1, TempReg, td);
      uInstr1(cb, CALLM, 0, Lit16,   helper);
      uFlagsRWU(cb, FlagsEmpty, FlagsEmpty, FlagsOSZACP);
      uInstr1(cb, POP,   1, TempReg, t);
      uInstr2(cb, PUT,   1, TempReg, t,      ArchReg, R_AL);
      uInstr1(cb, POP,   1, TempReg, t);
      uInstr2(cb, PUT,   1, TempReg, t,      ArchReg, R_AH);
      uInstr1(cb, CLEAR, 0, Lit16,   4);
   }
   uInstr0(cb, CALLM_E, 0);
}


static 
Addr dis_Grp1 ( UCodeBlock* cb, 
                UChar       sorb,
                Addr eip, UChar modrm, 
                Int am_sz, Int d_sz, Int sz, UInt d32 )
{
   Int   t1, t2, uopc;
   UInt  pair;
   UChar dis_buf[50];
   if (epartIsReg(modrm)) {
      vg_assert(am_sz == 1);
      t1  = newTemp(cb);
      uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
      switch (gregOfRM(modrm)) {
         case 0: uopc = ADD; break;  case 1: uopc = OR;  break;
         case 2: uopc = ADC; break;  case 3: uopc = SBB; break;
         case 4: uopc = AND; break;  case 5: uopc = SUB; break;
         case 6: uopc = XOR; break;  case 7: uopc = SUB; break;
         default: VG_(core_panic)("dis_Grp1(Reg): unhandled case");
      }
      if (uopc == AND || uopc == OR) {
         Int tao = newTemp(cb);
         uInstr2(cb, MOV, sz, Literal, 0, TempReg, tao);
         uLiteral(cb, d32);
         uInstr2(cb, uopc, sz, TempReg, tao, TempReg, t1);
         setFlagsFromUOpcode(cb, uopc);
      } else {
         uInstr2(cb, uopc, sz, Literal, 0, TempReg, t1);
         uLiteral(cb, d32);
         setFlagsFromUOpcode(cb, uopc);
      }
      if (gregOfRM(modrm) < 7)
         uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, eregOfRM(modrm));
      eip += (am_sz + d_sz);
      if (dis)
         VG_(printf)("%s%c $0x%x, %s\n",
                     nameGrp1(gregOfRM(modrm)), nameISize(sz), d32, 
                     nameIReg(sz,eregOfRM(modrm)));
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL);
      t1   = LOW24(pair);
      t2   = newTemp(cb);
      eip  += HI8(pair);
      eip  += d_sz;
      uInstr2(cb, LOAD, sz, TempReg, t1, TempReg, t2);
      switch (gregOfRM(modrm)) {
         case 0: uopc = ADD; break;  case 1: uopc = OR;  break;
         case 2: uopc = ADC; break;  case 3: uopc = SBB; break;
         case 4: uopc = AND; break;  case 5: uopc = SUB; break;
         case 6: uopc = XOR; break;  case 7: uopc = SUB; break;
         default: VG_(core_panic)("dis_Grp1(Mem): unhandled case");
      }
      if (uopc == AND || uopc == OR) {
         Int tao = newTemp(cb);
         uInstr2(cb, MOV, sz, Literal, 0, TempReg, tao);
         uLiteral(cb, d32);
         uInstr2(cb, uopc, sz, TempReg, tao, TempReg, t2);
         setFlagsFromUOpcode(cb, uopc);
      } else {
         uInstr2(cb, uopc, sz, Literal, 0, TempReg, t2);
         uLiteral(cb, d32);
         setFlagsFromUOpcode(cb, uopc);
      }
      if (gregOfRM(modrm) < 7) {
         uInstr2(cb, STORE, sz, TempReg, t2, TempReg, t1);
      }
      if (dis)
         VG_(printf)("%s%c $0x%x, %s\n",
                     nameGrp1(gregOfRM(modrm)), nameISize(sz), d32, 
                     dis_buf);
   }
   return eip;
}


/* Group 2 extended opcodes. */
static
Addr dis_Grp2 ( UCodeBlock* cb, 
                UChar       sorb,
                Addr eip, UChar modrm,
                Int am_sz, Int d_sz, Int sz, 
                Tag orig_src_tag, UInt orig_src_val )
{
   /* orig_src_tag and orig_src_val denote either ArchReg(%CL) or a
      Literal.  And eip on entry points at the modrm byte. */
   Int   t1, t2, uopc;
   UInt  pair;
   UChar dis_buf[50];
   UInt  src_val;
   Tag   src_tag;

   /* Get the amount to be shifted by into src_tag/src_val. */
   if (orig_src_tag == ArchReg) {
      src_val = newTemp(cb);
      src_tag = TempReg;
      uInstr2(cb, GET, 1, orig_src_tag, orig_src_val, TempReg, src_val);
   } else {
      src_val = orig_src_val;
      src_tag = Literal;
   }

   if (epartIsReg(modrm)) {
      vg_assert(am_sz == 1);
      t1  = newTemp(cb);
      uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
      switch (gregOfRM(modrm)) {
         case 0: uopc = ROL; break;  case 1: uopc = ROR; break;
         case 2: uopc = RCL; break;  case 3: uopc = RCR; break;
         case 4: uopc = SHL; break;  case 5: uopc = SHR; break;
         case 7: uopc = SAR; break;
         default: VG_(core_panic)("dis_Grp2(Reg): unhandled case");
      }
      if (src_tag == Literal) {
          uInstr2(cb, uopc, sz, Literal, 0, TempReg, t1);
	  uLiteral(cb, src_val);
      } else {
          uInstr2(cb, uopc, sz, src_tag, src_val, TempReg, t1);
      }
      setFlagsFromUOpcode(cb, uopc);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, eregOfRM(modrm));
      eip += (am_sz + d_sz);
      if (dis) {
         if (orig_src_tag == Literal)
            VG_(printf)("%s%c $0x%x, %s\n",
                        nameGrp2(gregOfRM(modrm)), nameISize(sz), 
                        orig_src_val, nameIReg(sz,eregOfRM(modrm)));
         else
            VG_(printf)("%s%c %s, %s\n",
                        nameGrp2(gregOfRM(modrm)), nameISize(sz),
                        nameIReg(1,orig_src_val),
                        nameIReg(sz,eregOfRM(modrm)));
      }
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL);
      t1   = LOW24(pair);
      t2   = newTemp(cb);
      eip  += HI8(pair);
      eip  += d_sz;
      uInstr2(cb, LOAD, sz, TempReg, t1, TempReg, t2);
      switch (gregOfRM(modrm)) {
         case 0: uopc = ROL; break;  case 1: uopc = ROR; break;
         case 2: uopc = RCL; break;  case 3: uopc = RCR; break;
         case 4: uopc = SHL; break;  case 5: uopc = SHR; break;
         case 7: uopc = SAR; break;
         default: VG_(core_panic)("dis_Grp2(Reg): unhandled case");
      }
      if (src_tag == Literal) {
         uInstr2(cb, uopc, sz, Literal, 0, TempReg, t2);
	 uLiteral(cb, src_val);
      } else {
         uInstr2(cb, uopc, sz, src_tag, src_val, TempReg, t2);
      }
      setFlagsFromUOpcode(cb, uopc);
      uInstr2(cb, STORE, sz, TempReg, t2, TempReg, t1);
      if (dis) {
         if (orig_src_tag == Literal)
            VG_(printf)("%s%c $0x%x, %s\n",
                        nameGrp2(gregOfRM(modrm)), nameISize(sz), 
                        orig_src_val, dis_buf);
         else 
            VG_(printf)("%s%c %s, %s\n",
                        nameGrp2(gregOfRM(modrm)), nameISize(sz), 
                        nameIReg(1,orig_src_val),
                        dis_buf);
      }
   }
   return eip;
}



/* Group 8 extended opcodes (but BT/BTS/BTC/BTR only). */
static
Addr dis_Grp8_BT ( UCodeBlock* cb, 
                   UChar       sorb,
                   Addr eip, UChar modrm,
                   Int am_sz, Int sz, UInt src_val )
{
#  define MODIFY_t2_AND_SET_CARRY_FLAG					\
      /* t2 is the value to be op'd on.  Copy to t_fetched, then	\
         modify t2, if non-BT. */					\
      uInstr2(cb, MOV,   4,  TempReg, t2, TempReg, t_fetched);		\
      uInstr2(cb, MOV,  sz,  Literal, 0,  TempReg, t_mask);		\
      uLiteral(cb, v_mask);						\
      switch (gregOfRM(modrm)) {					\
         case 4: /* BT */  break;					\
         case 5: /* BTS */ 						\
            uInstr2(cb, OR, sz, TempReg, t_mask, TempReg, t2); break;	\
         case 6: /* BTR */						\
            uInstr2(cb, AND, sz, TempReg, t_mask, TempReg, t2); break;	\
         case 7: /* BTC */ 						\
            uInstr2(cb, XOR, sz, TempReg, t_mask, TempReg, t2); break;	\
      }									\
      /* Copy relevant bit from t_fetched into carry flag. */		\
      uInstr2(cb, SHR, sz, Literal, 0, TempReg, t_fetched);		\
      uLiteral(cb, src_val);						\
      uInstr2(cb, MOV, sz, Literal, 0, TempReg, t_mask);		\
      uLiteral(cb, 1);							\
      uInstr2(cb, AND, sz, TempReg, t_mask, TempReg, t_fetched);	\
      uInstr1(cb, NEG, sz, TempReg, t_fetched);				\
      setFlagsFromUOpcode(cb, NEG);


   /* src_val denotes a d8.
      And eip on entry points at the modrm byte. */
   Int   t1, t2, t_fetched, t_mask;
   UInt  pair;
   UChar dis_buf[50];
   UInt  v_mask;

   /* There is no 1-byte form of this instruction, AFAICS. */
   vg_assert(sz == 2 || sz == 4);

   /* Limit src_val -- the bit offset -- to something within a word.
      The Intel docs say that literal offsets larger than a word are
      masked in this way. */
   switch (sz) {
      case 2: src_val &= 15; break;
      case 4: src_val &= 31; break;
      default: VG_(core_panic)("dis_Grp8_BT: invalid size");
   }

   /* Invent a mask suitable for the operation. */

   switch (gregOfRM(modrm)) {
      case 4: /* BT */  v_mask = 0; break;
      case 5: /* BTS */ v_mask = 1 << src_val; break;
      case 6: /* BTR */ v_mask = ~(1 << src_val); break;
      case 7: /* BTC */ v_mask = 1 << src_val; break;
         /* If this needs to be extended, probably simplest to make a
            new function to handle the other cases (0 .. 3).  The
            Intel docs do however not indicate any use for 0 .. 3, so
            we don't expect this to happen. */
      default: VG_(core_panic)("dis_Grp8_BT");
   }
   /* Probably excessively paranoid. */
   if (sz == 2)
      v_mask &= 0x0000FFFF;

   t1        = INVALID_TEMPREG;
   t_fetched = newTemp(cb);
   t_mask    = newTemp(cb);

   if (epartIsReg(modrm)) {
      vg_assert(am_sz == 1);
      t2 = newTemp(cb);

      /* Fetch the value to be tested and modified. */
      uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t2);
      /* Do it! */
      MODIFY_t2_AND_SET_CARRY_FLAG;
      /* Dump the result back, if non-BT. */
      if (gregOfRM(modrm) != 4 /* BT */)
         uInstr2(cb, PUT, sz, TempReg, t2, ArchReg, eregOfRM(modrm));

      eip += (am_sz + 1);
      if (dis)
         VG_(printf)("%s%c $0x%x, %s\n",
                     nameGrp8(gregOfRM(modrm)), nameISize(sz),
                     src_val,
                     nameIReg(sz,eregOfRM(modrm)));
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL);
      t1   = LOW24(pair);
      t2   = newTemp(cb);
      eip  += HI8(pair);
      eip  += 1;

      /* Fetch the value to be tested and modified. */
      uInstr2(cb, LOAD,  sz, TempReg, t1, TempReg, t2);
      /* Do it! */
      MODIFY_t2_AND_SET_CARRY_FLAG;
      /* Dump the result back, if non-BT. */
      if (gregOfRM(modrm) != 4 /* BT */) {
         uInstr2(cb, STORE, sz, TempReg, t2, TempReg, t1);
      }
      if (dis)
            VG_(printf)("%s%c $0x%x, %s\n",
                        nameGrp8(gregOfRM(modrm)), nameISize(sz), src_val, 
                        dis_buf);
   }
   return eip;

#  undef MODIFY_t2_AND_SET_CARRY_FLAG
}



/* Generate ucode to multiply the value in EAX/AX/AL by the register
   specified by the ereg of modrm, and park the result in
   EDX:EAX/DX:AX/AX. */
static void codegen_mul_A_D_Reg ( UCodeBlock* cb, Int sz, 
                                  UChar modrm, Bool signed_multiply )
{
   Int helper = signed_multiply 
                ?
                   (sz==1 ? VGOFF_(helper_imul_8_16) 
                          : (sz==2 ? VGOFF_(helper_imul_16_32) 
                                   : VGOFF_(helper_imul_32_64)))
                :
                   (sz==1 ? VGOFF_(helper_mul_8_16)
                          : (sz==2 ? VGOFF_(helper_mul_16_32) 
                                   : VGOFF_(helper_mul_32_64)));
   Int t1 = newTemp(cb);
   Int ta = newTemp(cb);
   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, GET,   sz, ArchReg, eregOfRM(modrm), TempReg, t1);
   uInstr1(cb, PUSH,  sz, TempReg, t1);
   uInstr2(cb, GET,   sz, ArchReg, R_EAX,  TempReg, ta);
   uInstr1(cb, PUSH,  sz, TempReg, ta);
   uInstr1(cb, CALLM, 0,  Lit16,   helper);
   uFlagsRWU(cb, FlagsEmpty, FlagsOC, FlagsSZAP);
   if (sz > 1) {
      uInstr1(cb, POP, sz, TempReg, t1);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, R_EDX);
      uInstr1(cb, POP, sz, TempReg, t1);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, R_EAX);
   } else {
      uInstr1(cb, CLEAR, 0, Lit16,   4);
      uInstr1(cb, POP,   2, TempReg, t1);
      uInstr2(cb, PUT,   2, TempReg, t1, ArchReg, R_EAX);
   }
	uInstr0(cb, CALLM_E, 0);
   if (dis) VG_(printf)("%s%c %s\n", signed_multiply ? "imul" : "mul",
                        nameISize(sz), nameIReg(sz, eregOfRM(modrm)));

}


/* Generate ucode to multiply the value in EAX/AX/AL by the value in
   TempReg temp, and park the result in EDX:EAX/DX:AX/AX. */
static void codegen_mul_A_D_Temp ( UCodeBlock* cb, Int sz, 
                                   Int temp, Bool signed_multiply,
                                   UChar* dis_buf )
{
   Int helper = signed_multiply 
                ?
                   (sz==1 ? VGOFF_(helper_imul_8_16) 
                          : (sz==2 ? VGOFF_(helper_imul_16_32) 
                                   : VGOFF_(helper_imul_32_64)))
                :
                   (sz==1 ? VGOFF_(helper_mul_8_16) 
                          : (sz==2 ? VGOFF_(helper_mul_16_32)
                                   : VGOFF_(helper_mul_32_64)));
   Int t1 = newTemp(cb);
   uInstr0(cb, CALLM_S, 0);
   uInstr1(cb, PUSH,  sz, TempReg, temp);
   uInstr2(cb, GET,   sz, ArchReg, R_EAX,  TempReg, t1);
   uInstr1(cb, PUSH,  sz, TempReg, t1);
   uInstr1(cb, CALLM, 0,  Lit16,   helper);
   uFlagsRWU(cb, FlagsEmpty, FlagsOC, FlagsSZAP);
   if (sz > 1) {
      uInstr1(cb, POP, sz, TempReg, t1);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, R_EDX);
      uInstr1(cb, POP, sz, TempReg, t1);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, R_EAX);
   } else {
      uInstr1(cb, CLEAR, 0, Lit16,   4);
      uInstr1(cb, POP,   2, TempReg, t1);
      uInstr2(cb, PUT,   2, TempReg, t1, ArchReg, R_EAX);
   }
   uInstr0(cb, CALLM_E, 0);
   if (dis) VG_(printf)("%s%c %s\n", signed_multiply ? "imul" : "mul",
                        nameISize(sz), dis_buf);
}


/* Group 3 extended opcodes. */
static 
Addr dis_Grp3 ( UCodeBlock* cb, 
                UChar       sorb,
                Int sz, Addr eip )
{
   Int   t1, t2;
   UInt  pair, d32;
   UChar modrm;
   UChar dis_buf[50];
   t1 = t2 = INVALID_TEMPREG;
   modrm = getUChar(eip);
   if (epartIsReg(modrm)) {
      t1 = newTemp(cb);
      switch (gregOfRM(modrm)) {
         case 0: { /* TEST */
            Int tao = newTemp(cb);
            eip++; d32 = getUDisp(sz, eip); eip += sz;
            uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
	    uInstr2(cb, MOV, sz, Literal, 0, TempReg, tao);
	    uLiteral(cb, d32);
            uInstr2(cb, AND, sz, TempReg, tao, TempReg, t1);
            setFlagsFromUOpcode(cb, AND);
            if (dis)
               VG_(printf)("test%c $0x%x, %s\n",
                   nameISize(sz), d32, nameIReg(sz, eregOfRM(modrm)));
            break;
         }
         case 2: /* NOT */
            eip++;
            uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
            uInstr1(cb, NOT, sz, TempReg, t1);
            setFlagsFromUOpcode(cb, NOT);
            uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, eregOfRM(modrm));
            if (dis)
               VG_(printf)("not%c %s\n",
                   nameISize(sz), nameIReg(sz, eregOfRM(modrm)));
            break;
         case 3: /* NEG */
            eip++;
            uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
            uInstr1(cb, NEG, sz, TempReg, t1);
            setFlagsFromUOpcode(cb, NEG);
            uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, eregOfRM(modrm));
            if (dis)
               VG_(printf)("neg%c %s\n",
                   nameISize(sz), nameIReg(sz, eregOfRM(modrm)));
            break;
         case 4: /* MUL */
            eip++;
            codegen_mul_A_D_Reg ( cb, sz, modrm, False );
            break;
         case 5: /* IMUL */
            eip++;
            codegen_mul_A_D_Reg ( cb, sz, modrm, True );
            break;
         case 6: /* DIV */
            eip++;
            uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
            codegen_div ( cb, sz, t1, False );
            if (dis)
               VG_(printf)("div%c %s\n", nameISize(sz), 
                           nameIReg(sz, eregOfRM(modrm)));
            break;
         case 7: /* IDIV */
            eip++;
            uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
            codegen_div ( cb, sz, t1, True );
            if (dis)
               VG_(printf)("idiv%c %s\n", nameISize(sz), 
                           nameIReg(sz, eregOfRM(modrm)));
            break;
         default: 
            VG_(printf)(
               "unhandled Grp3(R) case %d\n", (UInt)gregOfRM(modrm));
            VG_(core_panic)("Grp3");
      }
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      t2   = LOW24(pair);
      t1   = newTemp(cb);
      eip  += HI8(pair);
      uInstr2(cb, LOAD, sz, TempReg, t2, TempReg, t1);
      switch (gregOfRM(modrm)) {
         case 0: { /* TEST */
            Int tao = newTemp(cb);
            d32 = getUDisp(sz, eip); eip += sz;
            uInstr2(cb, MOV, sz, Literal, 0, TempReg, tao);
            uLiteral(cb, d32);
            uInstr2(cb, AND, sz, TempReg, tao, TempReg, t1);
            setFlagsFromUOpcode(cb, AND);
            if (dis)
               VG_(printf)("test%c $0x%x, %s\n", 
                           nameISize(sz), d32, dis_buf);
            break;
         }
         case 2: /* NOT */
            uInstr1(cb, NOT, sz, TempReg, t1);
            setFlagsFromUOpcode(cb, NOT);
            uInstr2(cb, STORE, sz, TempReg, t1, TempReg, t2);
            if (dis)
               VG_(printf)("not%c %s\n", nameISize(sz), dis_buf);
            break;
         case 3: /* NEG */
            uInstr1(cb, NEG, sz, TempReg, t1);
            setFlagsFromUOpcode(cb, NEG);
            uInstr2(cb, STORE, sz, TempReg, t1, TempReg, t2);
            if (dis)
               VG_(printf)("neg%c %s\n", nameISize(sz), dis_buf);
            break;
         case 4: /* MUL */
            codegen_mul_A_D_Temp ( cb, sz, t1, False, 
                                   dis?dis_buf:NULL );
            break;
         case 5: /* IMUL */
            codegen_mul_A_D_Temp ( cb, sz, t1, True, dis?dis_buf:NULL );
            break;
         case 6: /* DIV */
            codegen_div ( cb, sz, t1, False );
            if (dis)
               VG_(printf)("div%c %s\n", nameISize(sz), dis_buf);
            break;
         case 7: /* IDIV */
            codegen_div ( cb, sz, t1, True );
            if (dis)
               VG_(printf)("idiv%c %s\n", nameISize(sz), dis_buf);
            break;
         default: 
            VG_(printf)(
               "unhandled Grp3(M) case %d\n", (UInt)gregOfRM(modrm));
            VG_(core_panic)("Grp3");
      }
   }
   return eip;
}


/* Group 4 extended opcodes. */
static
Addr dis_Grp4 ( UCodeBlock* cb, 
                UChar       sorb,
                Addr eip )
{
   Int   t1, t2;
   UInt  pair;
   UChar modrm;
   UChar dis_buf[50];
   t1 = t2 = INVALID_TEMPREG;

   modrm = getUChar(eip);
   if (epartIsReg(modrm)) {
      t1 = newTemp(cb);
      uInstr2(cb, GET, 1, ArchReg, eregOfRM(modrm), TempReg, t1);
      switch (gregOfRM(modrm)) {
         case 0: /* INC */
            uInstr1(cb, INC, 1, TempReg, t1);
            setFlagsFromUOpcode(cb, INC);
            uInstr2(cb, PUT, 1, TempReg, t1, ArchReg, eregOfRM(modrm));
            break;
         case 1: /* DEC */
            uInstr1(cb, DEC, 1, TempReg, t1);
            setFlagsFromUOpcode(cb, DEC);
            uInstr2(cb, PUT, 1, TempReg, t1, ArchReg, eregOfRM(modrm));
            break;
         default: 
            VG_(printf)(
               "unhandled Grp4(R) case %d\n", (UInt)gregOfRM(modrm));
            VG_(core_panic)("Grp4");
      }
      eip++;
      if (dis)
         VG_(printf)("%sb %s\n", nameGrp4(gregOfRM(modrm)),
                     nameIReg(1, eregOfRM(modrm)));
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      t2   = LOW24(pair);
      t1   = newTemp(cb);
      uInstr2(cb, LOAD, 1, TempReg, t2, TempReg, t1);
      switch (gregOfRM(modrm)) {
         case 0: /* INC */ 
            uInstr1(cb, INC, 1, TempReg, t1);
            setFlagsFromUOpcode(cb, INC);
            uInstr2(cb, STORE, 1, TempReg, t1, TempReg, t2);
            break;
         case 1: /* DEC */
            uInstr1(cb, DEC, 1, TempReg, t1);
            setFlagsFromUOpcode(cb, DEC);
            uInstr2(cb, STORE, 1, TempReg, t1, TempReg, t2);
            break;
         default: 
            VG_(printf)(
               "unhandled Grp4(M) case %d\n", (UInt)gregOfRM(modrm));
            VG_(core_panic)("Grp4");
      }
      eip += HI8(pair);
      if (dis)
         VG_(printf)("%sb %s\n", nameGrp4(gregOfRM(modrm)), dis_buf);
   }
   return eip;
}


/* Group 5 extended opcodes. */
static
Addr dis_Grp5 ( UCodeBlock* cb, 
                UChar       sorb,
                Int sz, Addr eip, Bool* isEnd )
{
   Int   t1, t2, t3, t4;
   UInt  pair;
   UChar modrm;
   UChar dis_buf[50];
   t1 = t2 = t3 = t4 = INVALID_TEMPREG;

   modrm = getUChar(eip);
   if (epartIsReg(modrm)) {
      t1 = newTemp(cb);
      uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
      switch (gregOfRM(modrm)) {
         case 0: /* INC */
            uInstr1(cb, INC, sz, TempReg, t1);
            setFlagsFromUOpcode(cb, INC);
            uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, eregOfRM(modrm));
            break;
         case 1: /* DEC */
            uInstr1(cb, DEC, sz, TempReg, t1);
            setFlagsFromUOpcode(cb, DEC);
            uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, eregOfRM(modrm));
            break;
         case 2: /* call Ev */
            t3 = newTemp(cb); t4 = newTemp(cb);
            uInstr2(cb, GET,   4, ArchReg, R_ESP, TempReg, t3);
            uInstr2(cb, SUB,   4, Literal, 0,     TempReg, t3);
	    uLiteral(cb, 4);
            uInstr2(cb, PUT,   4, TempReg, t3,    ArchReg, R_ESP);
            uInstr2(cb, MOV,   4, Literal, 0,     TempReg, t4);
	    uLiteral(cb, eip+1);
            uInstr2(cb, STORE, 4, TempReg, t4,    TempReg, t3);
            uInstr1(cb, JMP,   0, TempReg, t1);
            uCond(cb, CondAlways);
            LAST_UINSTR(cb).jmpkind = JmpCall;
            *isEnd = True;
            break;
         case 4: /* jmp Ev */
            uInstr1(cb, JMP, 0, TempReg, t1);
            uCond(cb, CondAlways);
            *isEnd = True;
            break;
         default: 
            VG_(printf)(
               "unhandled Grp5(R) case %d\n", (UInt)gregOfRM(modrm));
            VG_(core_panic)("Grp5");
      }
      eip++;
      if (dis)
         VG_(printf)("%s%c %s\n", nameGrp5(gregOfRM(modrm)),
                     nameISize(sz), nameIReg(sz, eregOfRM(modrm)));
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      t2   = LOW24(pair);
      t1   = newTemp(cb);
      uInstr2(cb, LOAD, sz, TempReg, t2, TempReg, t1);
      switch (gregOfRM(modrm)) {
         case 0: /* INC */ 
            uInstr1(cb, INC, sz, TempReg, t1);
            setFlagsFromUOpcode(cb, INC);
            uInstr2(cb, STORE, sz, TempReg, t1, TempReg, t2);
            break;
         case 1: /* DEC */
            uInstr1(cb, DEC, sz, TempReg, t1);
            setFlagsFromUOpcode(cb, DEC);
            uInstr2(cb, STORE, sz, TempReg, t1, TempReg, t2);
            break;
         case 2: /* call Ev */
            t3 = newTemp(cb); t4 = newTemp(cb);
            uInstr2(cb, GET,   4, ArchReg, R_ESP, TempReg, t3);
            uInstr2(cb, SUB,   4, Literal, 0,     TempReg, t3);
            uLiteral(cb, 4);
            uInstr2(cb, PUT,   4, TempReg, t3,    ArchReg, R_ESP);
            uInstr2(cb, MOV,   4, Literal, 0,     TempReg, t4);
	         uLiteral(cb, eip+HI8(pair));
            uInstr2(cb, STORE, 4, TempReg, t4,    TempReg, t3);
            uInstr1(cb, JMP,   0, TempReg, t1);
            uCond(cb, CondAlways);
            LAST_UINSTR(cb).jmpkind = JmpCall;
            *isEnd = True;
            break;
         case 4: /* JMP Ev */
            uInstr1(cb, JMP, 0, TempReg, t1);
            uCond(cb, CondAlways);
            *isEnd = True;
            break;
         case 6: /* PUSH Ev */
            t3 = newTemp(cb);
            uInstr2(cb, GET,    4, ArchReg, R_ESP, TempReg, t3);
            uInstr2(cb, SUB,    4, Literal, 0,     TempReg, t3);
	    uLiteral(cb, sz);
            uInstr2(cb, PUT,    4, TempReg, t3,    ArchReg, R_ESP);
            uInstr2(cb, STORE, sz, TempReg, t1,    TempReg, t3);
            break;
         default: 
            VG_(printf)(
               "unhandled Grp5(M) case %d\n", (UInt)gregOfRM(modrm));
            VG_(core_panic)("Grp5");
      }
      eip += HI8(pair);
      if (dis)
         VG_(printf)("%s%c %s\n", nameGrp5(gregOfRM(modrm)),
                     nameISize(sz), dis_buf);
   }
   return eip;
}


/* Template for REPE CMPS<sz>.  Assumes this insn is the last one in
   the basic block, and so emits a jump to the next insn. */
static 
void codegen_REPE_CMPS ( UCodeBlock* cb, Int sz, Addr eip, Addr eip_next )
{
   Int tc,  /* ECX */
       td,  /* EDI */   ts, /* ESI */
       tdv, /* (EDI) */ tsv /* (ESI) */;

   tdv = newTemp(cb);
   tsv = newTemp(cb);
   td = newTemp(cb);
   ts = newTemp(cb);
   tc = newTemp(cb);

   uInstr2(cb, GET,   4, ArchReg, R_ECX, TempReg, tc);
   uInstr2(cb, JIFZ,  4, TempReg, tc,    Literal, 0);
   uLiteral(cb, eip_next);
   uInstr1(cb, DEC,   4, TempReg, tc);
   uInstr2(cb, PUT,   4, TempReg, tc,    ArchReg, R_ECX);

   uInstr2(cb, GET,   4, ArchReg, R_EDI, TempReg, td);
   uInstr2(cb, GET,   4, ArchReg, R_ESI, TempReg, ts);

   uInstr2(cb, LOAD, sz, TempReg, td,    TempReg, tdv);
   uInstr2(cb, LOAD, sz, TempReg, ts,    TempReg, tsv);

   uInstr2(cb, SUB,  sz, TempReg, tdv,   TempReg, tsv);
   setFlagsFromUOpcode(cb, SUB);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, tdv);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, tdv);

   uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, tdv);
	uInstr0(cb, CALLM_E, 0);
   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, tdv);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, tdv,    TempReg, td);
   uInstr2(cb, ADD,   4, TempReg, tdv,    TempReg, ts);

   uInstr2(cb, PUT,   4, TempReg, td,     ArchReg, R_EDI);
   uInstr2(cb, PUT,   4, TempReg, ts,     ArchReg, R_ESI);

   uInstr1(cb, JMP,   0, Literal, 0);
   uLiteral(cb, eip);
   uCond(cb, CondZ);
   uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
   uInstr1(cb, JMP,   0, Literal, 0);
   uLiteral(cb, eip_next);
   uCond(cb, CondAlways);
}


/* Template for REPNE SCAS<sz>.  Assumes this insn is the last one in
   the basic block, and so emits a jump to the next insn. */
static 
void codegen_REPNE_SCAS ( UCodeBlock* cb, Int sz, Addr eip, Addr eip_next )
{
   Int ta /* EAX */, tc /* ECX */, td /* EDI */, tv;
   ta = newTemp(cb);
   tc = newTemp(cb);
   tv = newTemp(cb);
   td = newTemp(cb);

   uInstr2(cb, GET,   4, ArchReg, R_ECX, TempReg, tc);
   uInstr2(cb, JIFZ,  4, TempReg, tc,    Literal, 0);
   uLiteral(cb, eip_next);
   uInstr1(cb, DEC,   4, TempReg, tc);
   uInstr2(cb, PUT,   4, TempReg, tc,    ArchReg, R_ECX);

   uInstr2(cb, GET,  sz, ArchReg, R_EAX, TempReg, ta);
   uInstr2(cb, GET,   4, ArchReg, R_EDI, TempReg, td);
   uInstr2(cb, LOAD, sz, TempReg, td,    TempReg, tv);
   /* next uinstr kills ta, but that's ok -- don't need it again */
   uInstr2(cb, SUB,  sz, TempReg, tv,    TempReg, ta);
   setFlagsFromUOpcode(cb, SUB);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, tv);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, tv);

   uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, tv);
	uInstr0(cb, CALLM_E, 0);

   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, tv);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, tv,    TempReg, td);
   uInstr2(cb, PUT,   4, TempReg, td,    ArchReg, R_EDI);
   uInstr1(cb, JMP,   0, Literal, 0);
   uLiteral(cb, eip);
   uCond(cb, CondNZ);
   uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
   uInstr1(cb, JMP,   0, Literal, 0);
   uLiteral(cb, eip_next);
   uCond(cb, CondAlways);
}


/* Template for REPE MOVS<sz>.  Assumes this insn is the last one in
   the basic block, and so emits a jump to the next insn. */
static 
void codegen_REPE_MOVS ( UCodeBlock* cb, Int sz, Addr eip, Addr eip_next )
{
   Int ts /* ESI */, tc /* ECX */, td /* EDI */, tv;
   tc = newTemp(cb);
   td = newTemp(cb);
   ts = newTemp(cb);
   tv = newTemp(cb);

   uInstr2(cb, GET,   4, ArchReg, R_ECX, TempReg, tc);
   uInstr2(cb, JIFZ,  4, TempReg, tc,    Literal, 0);
   uLiteral(cb, eip_next);
   uInstr1(cb, DEC,   4, TempReg, tc);
   uInstr2(cb, PUT,   4, TempReg, tc,    ArchReg, R_ECX);

   uInstr2(cb, GET,   4, ArchReg, R_EDI, TempReg, td);
   uInstr2(cb, GET,   4, ArchReg, R_ESI, TempReg, ts);

   uInstr2(cb, LOAD,  sz, TempReg, ts,    TempReg, tv);
   uInstr2(cb, STORE, sz, TempReg, tv,    TempReg, td);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, tv);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, tv);

   uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, tv);
	uInstr0(cb, CALLM_E, 0);

   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, tv);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, tv,    TempReg, td);
   uInstr2(cb, ADD,   4, TempReg, tv,    TempReg, ts);

   uInstr2(cb, PUT,   4, TempReg, td,    ArchReg, R_EDI);
   uInstr2(cb, PUT,   4, TempReg, ts,    ArchReg, R_ESI);

   uInstr1(cb, JMP,   0, Literal, 0);
   uLiteral(cb, eip);
   uCond(cb, CondAlways);
}


/* Template for REPE STOS<sz>.  Assumes this insn is the last one in
   the basic block, and so emits a jump to the next insn. */
static 
void codegen_REPE_STOS ( UCodeBlock* cb, Int sz, Addr eip, Addr eip_next )
{
   Int ta /* EAX */, tc /* ECX */, td /* EDI */;
   ta = newTemp(cb);
   tc = newTemp(cb);
   td = newTemp(cb);

   uInstr2(cb, GET,    4, ArchReg, R_ECX, TempReg, tc);
   uInstr2(cb, JIFZ,   4, TempReg, tc,    Literal, 0);
   uLiteral(cb, eip_next);
   uInstr1(cb, DEC,    4, TempReg, tc);
   uInstr2(cb, PUT,    4, TempReg, tc,    ArchReg, R_ECX);

   uInstr2(cb, GET,   sz, ArchReg, R_EAX, TempReg, ta);
   uInstr2(cb, GET,    4, ArchReg, R_EDI, TempReg, td);
   uInstr2(cb, STORE, sz, TempReg, ta,    TempReg, td);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, ta);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, ta);

   uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, ta);
	uInstr0(cb, CALLM_E, 0);

   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, ta);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, ta,    TempReg, td);
   uInstr2(cb, PUT,   4, TempReg, td,    ArchReg, R_EDI);

   uInstr1(cb, JMP,   0, Literal, 0);
   uLiteral(cb, eip);
   uCond(cb, CondAlways);
}


/* Template for CMPS<sz>, _not_ preceded by a REP prefix. */
static 
void codegen_CMPS ( UCodeBlock* cb, Int sz )
{
   Int td,  /* EDI */   ts, /* ESI */
       tdv, /* (EDI) */ tsv /* (ESI) */;
   tdv = newTemp(cb);
   tsv = newTemp(cb);
   td  = newTemp(cb);
   ts  = newTemp(cb);

   uInstr2(cb, GET,   4, ArchReg, R_EDI, TempReg, td);
   uInstr2(cb, GET,   4, ArchReg, R_ESI, TempReg, ts);

   uInstr2(cb, LOAD, sz, TempReg, td,    TempReg, tdv);
   uInstr2(cb, LOAD, sz, TempReg, ts,    TempReg, tsv);

   uInstr2(cb, SUB,  sz, TempReg, tdv,   TempReg, tsv); 
   setFlagsFromUOpcode(cb, SUB);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, tdv);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, tdv);

   uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, tdv);
	uInstr0(cb, CALLM_E, 0);

   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, tdv);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, tdv,    TempReg, td);
   uInstr2(cb, ADD,   4, TempReg, tdv,    TempReg, ts);

   uInstr2(cb, PUT,   4, TempReg, td,    ArchReg, R_EDI);
   uInstr2(cb, PUT,   4, TempReg, ts,    ArchReg, R_ESI);
}


/* Template for MOVS<sz>, _not_ preceded by a REP prefix. */
static 
void codegen_MOVS ( UCodeBlock* cb, Int sz )
{
   Int tv, /* the value being copied */
       td, /* EDI */ ts /* ESI */;
   tv = newTemp(cb);
   td = newTemp(cb);
   ts = newTemp(cb);

   uInstr2(cb, GET,   4, ArchReg, R_EDI, TempReg, td);
   uInstr2(cb, GET,   4, ArchReg, R_ESI, TempReg, ts);

   uInstr2(cb, LOAD,  sz, TempReg, ts,    TempReg, tv);
   uInstr2(cb, STORE, sz, TempReg, tv,    TempReg, td);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, tv);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, tv);

   uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, tv);
	uInstr0(cb, CALLM_E, 0);

   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, tv);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, tv,    TempReg, td);
   uInstr2(cb, ADD,   4, TempReg, tv,    TempReg, ts);

   uInstr2(cb, PUT,   4, TempReg, td,    ArchReg, R_EDI);
   uInstr2(cb, PUT,   4, TempReg, ts,    ArchReg, R_ESI);
}


/* Template for STOS<sz>, _not_ preceded by a REP prefix. */
static 
void codegen_STOS ( UCodeBlock* cb, Int sz )
{
   Int ta /* EAX */, td /* EDI */;
   ta = newTemp(cb);
   td = newTemp(cb);

   uInstr2(cb, GET,   sz, ArchReg, R_EAX, TempReg, ta);
   uInstr2(cb, GET,    4, ArchReg, R_EDI, TempReg, td);
   uInstr2(cb, STORE, sz, TempReg, ta,    TempReg, td);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, ta);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, ta);

   uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, ta);
	uInstr0(cb, CALLM_E, 0);

   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, ta);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, ta,    TempReg, td);
   uInstr2(cb, PUT,   4, TempReg, td,    ArchReg, R_EDI);
}


/* Template for LODS<sz>, _not_ preceded by a REP prefix. */
static 
void codegen_LODS ( UCodeBlock* cb, Int sz )
{
   Int ta /* EAX */, ts /* ESI */;
   ta = newTemp(cb);
   ts = newTemp(cb);

   uInstr2(cb, GET,    4, ArchReg, R_ESI, TempReg, ts);
   uInstr2(cb, LOAD,  sz, TempReg, ts,    TempReg, ta);
   uInstr2(cb, PUT,   sz, TempReg, ta,    ArchReg, R_EAX);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, ta);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, ta);

   uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, ta);
	uInstr0(cb, CALLM_E, 0);

   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, ta);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, ta,    TempReg, ts);
   uInstr2(cb, PUT,   4, TempReg, ts,    ArchReg, R_ESI);
}


/* Template for REPNE SCAS<sz>, _not_ preceded by a REP prefix. */
static 
void codegen_SCAS ( UCodeBlock* cb, Int sz )
{
   Int ta /* EAX */, td /* EDI */, tv;
   ta = newTemp(cb);
   tv = newTemp(cb);
   td = newTemp(cb);

   uInstr2(cb, GET,  sz, ArchReg, R_EAX, TempReg, ta);
   uInstr2(cb, GET,   4, ArchReg, R_EDI, TempReg, td);
   uInstr2(cb, LOAD, sz, TempReg, td,    TempReg, tv);
   /* next uinstr kills ta, but that's ok -- don't need it again */
   uInstr2(cb, SUB,  sz, TempReg, tv,    TempReg, ta);
   setFlagsFromUOpcode(cb, SUB);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, MOV,   4, Literal, 0,     TempReg, tv);
   uLiteral(cb, 0);
   uInstr1(cb, PUSH,  4, TempReg, tv);

   uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_get_dirflag));
   uFlagsRWU(cb, FlagD, FlagsEmpty, FlagsEmpty);

   uInstr1(cb, POP,   4, TempReg, tv);
	uInstr0(cb, CALLM_E, 0);

   if (sz == 4 || sz == 2) {
      uInstr2(cb, SHL, 4, Literal, 0, TempReg, tv);
      uLiteral(cb, sz/2);
   }
   uInstr2(cb, ADD,   4, TempReg, tv,    TempReg, td);
   uInstr2(cb, PUT,   4, TempReg, td,    ArchReg, R_EDI);
}


/* (I)MUL E, G.  Supplied eip points to the modR/M byte. */
static
Addr dis_mul_E_G ( UCodeBlock* cb, 
                   UChar       sorb,
                   Int         size, 
                   Addr        eip0,
                   Bool        signed_multiply )
{
   Int ta, tg, te, helper;
   UChar dis_buf[50];
   UChar rm = getUChar(eip0);
   ta = INVALID_TEMPREG;
   te = newTemp(cb);
   tg = newTemp(cb);

   switch (size) {
      case 4: helper = signed_multiply ? VGOFF_(helper_imul_32_64) 
                                       : VGOFF_(helper_mul_32_64);
              break;
      case 2: helper = signed_multiply ? VGOFF_(helper_imul_16_32) 
                                       : VGOFF_(helper_mul_16_32);
              break;
      case 1: helper = signed_multiply ? VGOFF_(helper_imul_8_16)
                                       : VGOFF_(helper_mul_8_16);
              break;
      default: VG_(core_panic)("dis_mul_E_G");
   }

   uInstr0(cb, CALLM_S, 0);
   if (epartIsReg(rm)) {
      uInstr2(cb, GET,   size, ArchReg, eregOfRM(rm), TempReg, te);
      uInstr2(cb, GET,   size, ArchReg, gregOfRM(rm), TempReg, tg);
      uInstr1(cb, PUSH,  size, TempReg, te);
      uInstr1(cb, PUSH,  size, TempReg, tg);
      uInstr1(cb, CALLM, 0,    Lit16,   helper);
      uFlagsRWU(cb, FlagsEmpty, FlagsOC, FlagsSZAP);
      uInstr1(cb, CLEAR, 0,    Lit16,   4);
      uInstr1(cb, POP,   size, TempReg, tg);
      uInstr2(cb, PUT,   size, TempReg, tg,   ArchReg, gregOfRM(rm));
      uInstr0(cb, CALLM_E, 0);
      if (dis) VG_(printf)("%smul%c %s, %s\n",
                           signed_multiply ? "i" : "",
                           nameISize(size), 
                           nameIReg(size,eregOfRM(rm)),
                           nameIReg(size,gregOfRM(rm)));
      return 1+eip0;
   } else {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      ta = LOW24(pair);
      uInstr2(cb, LOAD,  size, TempReg, ta, TempReg, te);
      uInstr2(cb, GET,   size, ArchReg, gregOfRM(rm), TempReg, tg);
      uInstr1(cb, PUSH,  size, TempReg, te);
      uInstr1(cb, PUSH,  size, TempReg, tg);
      uInstr1(cb, CALLM, 0,    Lit16, helper);
      uFlagsRWU(cb, FlagsEmpty, FlagsOC, FlagsSZAP);
      uInstr1(cb, CLEAR, 0,    Lit16,   4);
      uInstr1(cb, POP,   size, TempReg, tg);
      uInstr2(cb, PUT,   size, TempReg, tg,   ArchReg, gregOfRM(rm));
      uInstr0(cb, CALLM_E, 0);
      if (dis) VG_(printf)("%smul%c %s, %s\n",
                           signed_multiply ? "i" : "",
                           nameISize(size), 
                           dis_buf,nameIReg(size,gregOfRM(rm)));
      return HI8(pair)+eip0;
   }
}


/* IMUL I * E -> G.  Supplied eip points to the modR/M byte. */
static
Addr dis_imul_I_E_G ( UCodeBlock* cb, 
                      UChar       sorb,
                      Int         size, 
                      Addr        eip,
                      Int         litsize )
{
   Int ta, te, tl, helper, d32;
   UChar dis_buf[50];
   UChar rm = getUChar(eip);
   ta = INVALID_TEMPREG;
   te = newTemp(cb);
   tl = newTemp(cb);

   switch (size) {
      case 4: helper = VGOFF_(helper_imul_32_64); break;
      case 2: helper = VGOFF_(helper_imul_16_32); break;
      case 1: helper = VGOFF_(helper_imul_8_16); break;
      default: VG_(core_panic)("dis_imul_I_E_G");
   }

   uInstr0(cb, CALLM_S, 0);
   if (epartIsReg(rm)) {
      uInstr2(cb, GET,   size, ArchReg, eregOfRM(rm), TempReg, te);
      uInstr1(cb, PUSH,  size, TempReg, te);
      eip++;
   } else {
      UInt pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL);
      ta = LOW24(pair);
      uInstr2(cb, LOAD,  size, TempReg, ta, TempReg, te);
      uInstr1(cb, PUSH,  size, TempReg, te);
      eip += HI8(pair);
   }

   d32 = getSDisp(litsize,eip);
   eip += litsize;

   uInstr2(cb, MOV,   size, Literal, 0,   TempReg, tl);
   uLiteral(cb, d32);
   uInstr1(cb, PUSH,  size, TempReg, tl);
   uInstr1(cb, CALLM, 0,    Lit16, helper);
   uFlagsRWU(cb, FlagsEmpty, FlagsOC, FlagsSZAP);
   uInstr1(cb, CLEAR, 0,    Lit16,   4);
   uInstr1(cb, POP,   size, TempReg, te);
   uInstr2(cb, PUT,   size, TempReg, te,   ArchReg, gregOfRM(rm));
   uInstr0(cb, CALLM_E, 0);

   if (dis) {
      if (epartIsReg(rm)) {
         VG_(printf)("imul %d, %s, %s\n", d32, nameIReg(size,eregOfRM(rm)),
                                          nameIReg(size,gregOfRM(rm)));
      } else {
         VG_(printf)("imul %d, %s, %s\n", d32, dis_buf,
                                          nameIReg(size,gregOfRM(rm)));
      }
   }

   return eip;
}   


/* Handle FPU insns which read/write memory.  On entry, eip points to
   the second byte of the insn (the one following D8 .. DF). */
static 
Addr dis_fpu_mem ( UCodeBlock* cb, 
                   UChar       sorb,
                   Int size, Bool is_write, 
                   Addr eip, UChar first_byte )
{
   Int   ta;
   UInt  pair;
   UChar dis_buf[50];
   UChar second_byte = getUChar(eip);
   vg_assert(second_byte < 0xC0);
   second_byte &= 0x38;
   pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
   ta   = LOW24(pair);
   eip  += HI8(pair);
   uInstr2(cb, is_write ? FPU_W : FPU_R, size,
               Lit16, 
               (((UShort)first_byte) << 8) | ((UShort)second_byte),
               TempReg, ta);
   if (dis) {
      if (is_write)
         VG_(printf)("fpu_w_%d 0x%x:0x%x, %s\n",
                     size, (UInt)first_byte, 
                           (UInt)second_byte, dis_buf );
      else
         VG_(printf)("fpu_r_%d %s, 0x%x:0x%x\n",
                     size, dis_buf,
                     (UInt)first_byte, 
                     (UInt)second_byte );
   }
   return eip;
}


/* Handle FPU insns which don't reference memory.  On entry, eip points to
   the second byte of the insn (the one following D8 .. DF). */
static 
Addr dis_fpu_no_mem ( UCodeBlock* cb, Addr eip, UChar first_byte )
{
   Bool  sets_ZCP    = False;
   Bool  uses_ZCP    = False;
   UChar second_byte = getUChar(eip); eip++;
   vg_assert(second_byte >= 0xC0);

   /* Does the insn write any integer condition codes (%EIP) ? */

   if (first_byte == 0xDB && second_byte >= 0xF0 && second_byte <= 0xF7) {
      /* FCOMI */
      sets_ZCP = True;
   } else
   if (first_byte == 0xDF && second_byte >= 0xF0 && second_byte <= 0xF7) {
      /* FCOMIP */
      sets_ZCP = True;
   } else
   if (first_byte == 0xDB && second_byte >= 0xE8 && second_byte <= 0xEF) {
      /* FUCOMI */
      sets_ZCP = True;
   } else
   if (first_byte == 0xDF && second_byte >= 0xE8 && second_byte <= 0xEF) {
      /* FUCOMIP */
      sets_ZCP = True;
   } 

   /* Dually, does the insn read any integer condition codes (%EIP) ? */

   if (first_byte == 0xDA && second_byte >= 0xC0 && second_byte <= 0xDF) {
      /* FCMOVB  %st(n), %st(0)
         FCMOVE  %st(n), %st(0)
         FCMOVBE %st(n), %st(0)
         FCMOVU  %st(n), %st(0)
      */
      uses_ZCP = True;
   } else
   if (first_byte == 0xDB && second_byte >= 0xC0 && second_byte <= 0xDF) {
      /* FCMOVNB  %st(n), %st(0)
         FCMOVNE  %st(n), %st(0)
         FCMOVNBE %st(n), %st(0)
         FCMOVNU  %st(n), %st(0)
      */
      uses_ZCP = True;
   }

   uInstr1(cb, FPU, 0,
               Lit16,
               (((UShort)first_byte) << 8) | ((UShort)second_byte)
          );
   if (uses_ZCP) {
      /* VG_(printf)("!!! --- FPU insn which reads %EFLAGS\n"); */
      uFlagsRWU(cb, FlagsZCP, FlagsEmpty, FlagsEmpty);
      vg_assert(!sets_ZCP);
   }
   if (sets_ZCP) {
      /* VG_(printf)("!!! --- FPU insn which writes %EFLAGS\n"); */
      uFlagsRWU(cb, FlagsEmpty, FlagsZCP, FlagsEmpty);
      vg_assert(!uses_ZCP);
   }

   if (dis) VG_(printf)("fpu 0x%x:0x%x%s%s\n",
                        (UInt)first_byte, (UInt)second_byte,
                        uses_ZCP ? " -rZCP" : "",
                        sets_ZCP ? " -wZCP" : "" );
   return eip;
}


/* Top-level handler for all FPU insns.  On entry, eip points to the
   second byte of the insn. */
static
Addr dis_fpu ( UCodeBlock* cb, 
               UChar       sorb,
               UChar first_byte, Addr eip )
{
   const Bool rd = False; 
   const Bool wr = True;
   UChar second_byte = getUChar(eip);

   /* Handle FSTSW %ax specially. */
   if (first_byte == 0xDF && second_byte == 0xE0) {
      Int t1 = newTemp(cb);
      uInstr0(cb, CALLM_S, 0);
      uInstr2(cb, MOV,   4, Literal, 0,  TempReg, t1);
      uLiteral(cb, 0);
      uInstr1(cb, PUSH,  4, TempReg, t1);
      uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_fstsw_AX) );
      uFlagsRWU(cb, FlagsEmpty, FlagsEmpty, FlagsEmpty);
      uInstr1(cb, POP,   2,  TempReg, t1);
      uInstr2(cb, PUT,   2,  TempReg, t1, ArchReg, R_EAX);
      uInstr0(cb, CALLM_E, 0);
      if (dis) VG_(printf)("fstsw %%ax\n");
      eip++;
      return eip;
   }

   /* Handle all non-memory FPU ops simply. */
   if (second_byte >= 0xC0)
      return dis_fpu_no_mem ( cb, eip, first_byte );

   /* The insn references memory; need to determine 
      whether it reads or writes, and at what size. */
   switch (first_byte) {

      case 0xD8:
         switch ((second_byte >> 3) & 7) {
            case 0: /* FADDs */
            case 1: /* FMULs */
            case 2: /* FCOMs */
            case 3: /* FCOMPs */
            case 4: /* FSUBs */
            case 5: /* FSUBRs */
            case 6: /* FDIVs */
            case 7: /* FDIVRs */
               return dis_fpu_mem(cb, sorb, 4, rd, eip, first_byte); 
            default: 
               goto unhandled;
         }
         break;

      case 0xD9:
         switch ((second_byte >> 3) & 7) {
            case 0: /* FLDs */
               return dis_fpu_mem(cb, sorb, 4, rd, eip, first_byte); 
            case 2: /* FSTs */
            case 3: /* FSTPs */
               return dis_fpu_mem(cb, sorb, 4, wr, eip, first_byte); 
            case 4: /* FLDENV */
               return dis_fpu_mem(cb, sorb, 28, rd, eip, first_byte);
            case 5: /* FLDCW */
               return dis_fpu_mem(cb, sorb, 2, rd, eip, first_byte); 
            case 6: /* FNSTENV */
               return dis_fpu_mem(cb, sorb, 28, wr, eip, first_byte);
            case 7: /* FSTCW */
               /* HACK!  FSTCW actually writes 2 bytes, not 4.  glibc
                  gets lots of moaning in __floor() if we do the right
                  thing here. */
               /* Later ... hack disabled .. we do do the Right Thing. */
               return dis_fpu_mem(cb, sorb, /*4*/ 2, wr, eip, first_byte); 
            default: 
               goto unhandled;
         }
         break;

      case 0xDA:
         switch ((second_byte >> 3) & 7) {
            case 0: /* FIADD */
            case 1: /* FIMUL */
            case 2: /* FICOM */
            case 3: /* FICOMP */
            case 4: /* FISUB */
            case 5: /* FISUBR */
            case 6: /* FIDIV */
            case 7: /* FIDIVR */
               return dis_fpu_mem(cb, sorb, 4, rd, eip, first_byte); 
            default: 
               goto unhandled;
         }
         break;

      case 0xDB:
         switch ((second_byte >> 3) & 7) {
            case 0: /* FILD dword-integer */
               return dis_fpu_mem(cb, sorb, 4, rd, eip, first_byte); 
            case 2: /* FIST dword-integer */
               return dis_fpu_mem(cb, sorb, 4, wr, eip, first_byte); 
            case 3: /* FISTPl */
               return dis_fpu_mem(cb, sorb, 4, wr, eip, first_byte); 
            case 5: /* FLD extended-real */
               return dis_fpu_mem(cb, sorb, 10, rd, eip, first_byte); 
            case 7: /* FSTP extended-real */
               return dis_fpu_mem(cb, sorb, 10, wr, eip, first_byte); 
            default: 
               goto unhandled;
         }
         break;

      case 0xDC:
         switch ((second_byte >> 3) & 7) {
            case 0: /* FADD double-real */
            case 1: /* FMUL double-real */
            case 2: /* FCOM double-real */
            case 3: /* FCOMP double-real */
            case 4: /* FSUB double-real */
            case 5: /* FSUBR double-real */
            case 6: /* FDIV double-real */
            case 7: /* FDIVR double-real */
               return dis_fpu_mem(cb, sorb, 8, rd, eip, first_byte); 
            default: 
               goto unhandled;
         }
         break;

      case 0xDD:
         switch ((second_byte >> 3) & 7) {
            case 0: /* FLD double-real */
               return dis_fpu_mem(cb, sorb, 8, rd, eip, first_byte); 
            case 2: /* FST double-real */
            case 3: /* FSTP double-real */
               return dis_fpu_mem(cb, sorb, 8, wr, eip, first_byte);
            case 4: /* FRSTOR */
               return dis_fpu_mem(cb, sorb, 108, rd, eip, first_byte);
            case 6: /* FSAVE */
               return dis_fpu_mem(cb, sorb, 108, wr, eip, first_byte);
            case 7: /* FSTSW */
               return dis_fpu_mem(cb, sorb, 2, wr, eip, first_byte);
            default: 
               goto unhandled;
         }
         break;

      case 0xDF:
         switch ((second_byte >> 3) & 7) {
            case 0: /* FILD word-integer */
               return dis_fpu_mem(cb, sorb, 2, rd, eip, first_byte); 
            case 2: /* FIST word-integer */
               return dis_fpu_mem(cb, sorb, 2, wr, eip, first_byte); 
            case 3: /* FISTP word-integer */
               return dis_fpu_mem(cb, sorb, 2, wr, eip, first_byte); 
            case 5: /* FILD qword-integer */
               return dis_fpu_mem(cb, sorb, 8, rd, eip, first_byte); 
            case 7: /* FISTP qword-integer */
               return dis_fpu_mem(cb, sorb, 8, wr, eip, first_byte); 
            default: 
               goto unhandled;
         }
         break;

      default: goto unhandled;
   }

  unhandled: 
   VG_(printf)("dis_fpu: unhandled memory case 0x%2x:0x%2x(%d)\n",
               (UInt)first_byte, (UInt)second_byte, 
               (UInt)((second_byte >> 3) & 7) );
   VG_(core_panic)("dis_fpu: unhandled opcodes");
}


/* Double length left shifts.  Apparently only required in v-size (no
   b- variant). */
static
Addr dis_SHLRD_Gv_Ev ( UCodeBlock* cb,
                       UChar sorb,
                       Addr eip, UChar modrm,
                       Int sz, 
                       Tag amt_tag, UInt amt_val,
                       Bool left_shift )
{
   /* amt_tag and amt_val denote either ArchReg(%CL) or a Literal.
      And eip on entry points at the modrm byte. */
   Int   t, t1, t2, ta, helper;
   UInt  pair;
   UChar dis_buf[50];

   vg_assert(sz == 2 || sz == 4);

   helper = left_shift 
               ? (sz==4 ? VGOFF_(helper_shldl) 
                        : VGOFF_(helper_shldw))
               : (sz==4 ? VGOFF_(helper_shrdl) 
                        : VGOFF_(helper_shrdw));

   /* Get the amount to be shifted by onto the stack. */
   t = newTemp(cb);
   t1 = newTemp(cb);
   t2 = newTemp(cb);
   if (amt_tag == ArchReg) {
      vg_assert(amt_val == R_CL);
      uInstr2(cb, GET, 1, ArchReg, amt_val, TempReg, t);
   } else {
      uInstr2(cb, MOV, 1, Literal, 0, TempReg, t);
      uLiteral(cb, amt_val);
   }

   uInstr0(cb, CALLM_S, 0);
   uInstr1(cb, PUSH, 1, TempReg, t);

   /* The E-part is the destination; this is shifted.  The G-part
      supplies bits to be shifted into the E-part, but is not
      changed. */

   uInstr2(cb, GET,  sz, ArchReg, gregOfRM(modrm), TempReg, t1);
   uInstr1(cb, PUSH, sz, TempReg, t1);

   if (epartIsReg(modrm)) {
      eip++;
      uInstr2(cb, GET,   sz, ArchReg, eregOfRM(modrm), TempReg, t2);
      uInstr1(cb, PUSH,  sz, TempReg, t2);
      uInstr1(cb, CALLM, 0,  Lit16,   helper);
      uFlagsRWU(cb, FlagsEmpty, FlagsOSZACP, FlagsEmpty);
      uInstr1(cb, POP,   sz, TempReg, t);
      uInstr2(cb, PUT,   sz, TempReg, t, ArchReg, eregOfRM(modrm));
      if (dis)
         VG_(printf)("sh%cd%c %%cl, %s, %s\n",
                     ( left_shift ? 'l' : 'r' ),
                     nameISize(sz), nameIReg(sz, gregOfRM(modrm)), 
                     nameIReg(sz, eregOfRM(modrm)));
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      ta   = LOW24(pair);
      eip  += HI8(pair);
      uInstr2(cb, LOAD,  sz, TempReg, ta,     TempReg, t2);
      uInstr1(cb, PUSH,  sz, TempReg, t2);
      uInstr1(cb, CALLM, 0,  Lit16,   helper);
      uFlagsRWU(cb, FlagsEmpty, FlagsOSZACP, FlagsEmpty);
      uInstr1(cb, POP,   sz, TempReg, t);
      uInstr2(cb, STORE, sz, TempReg, t,      TempReg, ta);
      if (dis)
         VG_(printf)("sh%cd%c %%cl, %s, %s\n",
                     ( left_shift ? 'l' : 'r' ),
                     nameISize(sz), nameIReg(sz, gregOfRM(modrm)), 
                     dis_buf);
   }
  
   if (amt_tag == Literal) eip++;
   uInstr1(cb, CLEAR, 0, Lit16, 8);

   uInstr0(cb, CALLM_E, 0);
   return eip;
}


/* Handle BT/BTS/BTR/BTC Gv, Ev.  Apparently b-size is not
   required. */

typedef enum { BtOpNone, BtOpSet, BtOpReset, BtOpComp } BtOp;

static Char* nameBtOp ( BtOp op )
{
   switch (op) {
      case BtOpNone:  return "";
      case BtOpSet:   return "s";
      case BtOpReset: return "r";
      case BtOpComp:  return "c";
      default: VG_(core_panic)("nameBtOp");
   }
}


static
Addr dis_bt_G_E ( UCodeBlock* cb, 
                  UChar       sorb,
                  Int sz, Addr eip, BtOp op )
{
   UInt  pair;
   UChar dis_buf[50];
   UChar modrm;

   Int t_addr, t_bitno, t_mask, t_fetched, t_esp, temp, lit;

   /* 2 and 4 are actually possible. */
   vg_assert(sz == 2 || sz == 4);
   /* We only handle 4. */
   vg_assert(sz == 4);

   t_addr = t_bitno = t_mask 
          = t_fetched = t_esp = temp = INVALID_TEMPREG;

   t_fetched = newTemp(cb);
   t_bitno   = newTemp(cb);
   temp      = newTemp(cb);
   lit       = newTemp(cb);

   modrm = getUChar(eip);

   uInstr2(cb, GET,  sz, ArchReg, gregOfRM(modrm), TempReg, t_bitno);

   if (epartIsReg(modrm)) {
      eip++;
      /* Get it onto the client's stack. */
      t_esp = newTemp(cb);
      t_addr = newTemp(cb);
      uInstr2(cb, GET,   4, ArchReg,  R_ESP, TempReg, t_esp);
      uInstr2(cb, SUB,  sz, Literal,  0,     TempReg, t_esp);
      uLiteral(cb, sz);
      uInstr2(cb, PUT,   4, TempReg,  t_esp, ArchReg, R_ESP);
      uInstr2(cb, GET,   sz, ArchReg, eregOfRM(modrm), TempReg, temp);
      uInstr2(cb, STORE, sz, TempReg, temp, TempReg, t_esp);
      /* Make ta point at it. */
      uInstr2(cb, MOV,   4,  TempReg, t_esp, TempReg, t_addr);
      /* Mask out upper bits of the shift amount, since we're doing a
         reg. */
      uInstr2(cb, MOV, 4, Literal, 0, TempReg, lit);
      uLiteral(cb, sz == 4 ? 31 : 15);
      uInstr2(cb, AND, 4, TempReg, lit, TempReg, t_bitno);
   } else {
      pair   = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      t_addr = LOW24(pair);
      eip   += HI8(pair);
   }
  
   /* At this point: ta points to the address being operated on.  If
      it was a reg, we will have pushed it onto the client's stack.
      t_bitno is the bit number, suitable masked in the case of a reg.  */
   
   /* Now the main sequence. */

   uInstr2(cb, MOV, 4, TempReg, t_bitno, TempReg, temp);
   uInstr2(cb, SAR, 4, Literal, 0, TempReg, temp);
   uLiteral(cb, 3);
   uInstr2(cb, ADD, 4, TempReg, temp, TempReg, t_addr);
   /* ta now holds effective address */

   uInstr2(cb, MOV, 4, Literal, 0, TempReg, lit);
   uLiteral(cb, 7);
   uInstr2(cb, AND, 4, TempReg, lit, TempReg, t_bitno);
   /* bitno contains offset of bit within byte */

   if (op != BtOpNone) {
      t_mask = newTemp(cb);
      uInstr2(cb, MOV, 4, Literal, 0, TempReg, t_mask);
      uLiteral(cb, 1);
      uInstr2(cb, SHL, 4, TempReg, t_bitno, TempReg, t_mask);
   }
   /* mask is now a suitable byte mask */

   uInstr2(cb, LOAD, 1, TempReg, t_addr, TempReg, t_fetched);
   if (op != BtOpNone) {
      uInstr2(cb, MOV, 4, TempReg, t_fetched, TempReg, temp);
      switch (op) {
         case BtOpSet: 
            uInstr2(cb, OR, 4, TempReg, t_mask, TempReg, temp); 
            break;
         case BtOpComp: 
            uInstr2(cb, XOR, 4, TempReg, t_mask, TempReg, temp); 
            break;
         case BtOpReset: 
            uInstr1(cb, NOT, 4, TempReg, t_mask);
            uInstr2(cb, AND, 4, TempReg, t_mask, TempReg, temp); 
            break;
         default: 
            VG_(core_panic)("dis_bt_G_E");
      }
      uInstr2(cb, STORE, 1, TempReg, temp, TempReg, t_addr);
   }

   /* Side effect done; now get selected bit into Carry flag */

   uInstr2(cb, SHR, 4, TempReg, t_bitno, TempReg, t_fetched);
   /* at bit 0 of fetched */

   uInstr2(cb, MOV, 4, Literal, 0, TempReg, lit);
   uLiteral(cb, 1);
   uInstr2(cb, AND, 4, TempReg, lit, TempReg, t_fetched);
   /* fetched is now 1 or 0 */

   /* NEG is a handy way to convert zero/nonzero into the carry
      flag. */
   uInstr1(cb, NEG, 4, TempReg, t_fetched);
   setFlagsFromUOpcode(cb, NEG);
   /* fetched is now in carry flag */

   /* Move reg operand from stack back to reg */
   if (epartIsReg(modrm)) {
      /* t_esp still points at it. */
      uInstr2(cb, LOAD, sz, TempReg, t_esp, TempReg, temp);
      uInstr2(cb, PUT,  sz, TempReg, temp, ArchReg, eregOfRM(modrm));
      uInstr2(cb, ADD,  sz, Literal, 0, TempReg, t_esp);
      uLiteral(cb, sz);
      uInstr2(cb, PUT,  4,  TempReg, t_esp, ArchReg, R_ESP);
   }

   if (epartIsReg(modrm)) {
      if (dis)
         VG_(printf)("bt%s%c %s, %s\n",
                     nameBtOp(op),
                     nameISize(sz), nameIReg(sz, gregOfRM(modrm)), 
                     nameIReg(sz, eregOfRM(modrm)));
   } else {
      if (dis)
         VG_(printf)("bt%s%c %s, %s\n",
                     nameBtOp(op),
                     nameISize(sz), nameIReg(sz, gregOfRM(modrm)), 
                     dis_buf);
   }
 
   return eip;
}




/* Handle BSF/BSR.  Only v-size seems necessary. */
static
Addr dis_bs_E_G ( UCodeBlock* cb, 
                  UChar       sorb,
                  Int sz, Addr eip, Bool fwds )
{
   Int   t, t1, ta, helper;
   UInt  pair;
   UChar dis_buf[50];
   UChar modrm;

   vg_assert(sz == 2 || sz == 4);
   vg_assert(sz==4);

   helper = fwds ? VGOFF_(helper_bsf) : VGOFF_(helper_bsr);
   modrm  = getUChar(eip);
   t1     = newTemp(cb);
   t      = newTemp(cb);

   uInstr0(cb, CALLM_S, 0);
   uInstr2(cb, GET,  sz, ArchReg, gregOfRM(modrm), TempReg, t1);
   uInstr1(cb, PUSH, sz, TempReg, t1);

   if (epartIsReg(modrm)) {
      eip++;
      uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t);
      if (dis)
         VG_(printf)("bs%c%c %s, %s\n",
                     fwds ? 'f' : 'r',
                     nameISize(sz), nameIReg(sz, eregOfRM(modrm)), 
                     nameIReg(sz, gregOfRM(modrm)));
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      ta   = LOW24(pair);
      eip  += HI8(pair);
      uInstr2(cb, LOAD, sz, TempReg, ta, TempReg, t);
      if (dis)
         VG_(printf)("bs%c%c %s, %s\n",
                     fwds ? 'f' : 'r',
                     nameISize(sz), dis_buf,
                     nameIReg(sz, gregOfRM(modrm)));
   }

   uInstr1(cb, PUSH,  sz,  TempReg, t);
   uInstr1(cb, CALLM, 0,   Lit16, helper);
   uFlagsRWU(cb, FlagsEmpty, FlagZ, FlagsOSACP);
   uInstr1(cb, POP,   sz,  TempReg, t);
   uInstr1(cb, POP,   sz,  TempReg, t);
   uInstr2(cb, PUT,   sz,  TempReg, t, ArchReg, gregOfRM(modrm));
   uInstr0(cb, CALLM_E, 0);

   return eip;
}


static 
void codegen_xchg_eAX_Reg ( UCodeBlock* cb, Int sz, Int reg )
{
   Int t1, t2;
   vg_assert(sz == 2 || sz == 4);
   t1 = newTemp(cb);
   t2 = newTemp(cb);
   uInstr2(cb, GET, sz, ArchReg, R_EAX, TempReg, t1);
   uInstr2(cb, GET, sz, ArchReg, reg,   TempReg, t2);
   uInstr2(cb, PUT, sz, TempReg, t2,    ArchReg, R_EAX);
   uInstr2(cb, PUT, sz, TempReg, t1,    ArchReg, reg);
   if (dis)
      VG_(printf)("xchg%c %s, %s\n", nameISize(sz),
                  nameIReg(sz, R_EAX), nameIReg(sz, reg));
}


static 
void codegen_SAHF ( UCodeBlock* cb )
{
   Int t   = newTemp(cb);
   Int t2  = newTemp(cb);
   uInstr2(cb, GET,   4, ArchReg, R_EAX, TempReg, t);

   /* Mask out parts of t not corresponding to %AH.  This stops the
      instrumenter complaining if they are undefined.  Otherwise, the
      instrumenter would check all 32 bits of t at the PUSH, which
      could be the cause of incorrect warnings.  Discovered by Daniel
      Veillard <veillard@redhat.com>. 
   */
   uInstr2(cb, MOV, 4, Literal, 0, TempReg, t2);
   uLiteral(cb, 0x0000FF00);
   uInstr2(cb, AND, 4, TempReg, t2, TempReg, t);
   /* We deliberately don't set the condition codes here, since this
      AND is purely internal to Valgrind and nothing to do with the
      client's state. */

   uInstr0(cb, CALLM_S, 0);
   uInstr1(cb, PUSH,  4, TempReg, t);
   uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_SAHF));
   uFlagsRWU(cb, FlagsEmpty, FlagsSZACP, FlagsEmpty);
   uInstr1(cb, CLEAR, 0, Lit16, 4);
   uInstr0(cb, CALLM_E, 0);
}

static 
void codegen_LAHF ( UCodeBlock* cb )
{
   Int t = newTemp(cb);

   /* Pushed arg is ignored, it just provides somewhere to put the
      return value. */
   uInstr2(cb, GET,   4, ArchReg, R_EAX, TempReg, t);
   uInstr0(cb, CALLM_S, 0);
   uInstr1(cb, PUSH,  4, TempReg, t);
   uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_LAHF));
   uFlagsRWU(cb, FlagsSZACP, FlagsEmpty, FlagsEmpty);
   uInstr1(cb, POP,   4, TempReg, t);
   uInstr0(cb, CALLM_E, 0);

   /* At this point, the %ah sub-register in %eax has been updated,
      the rest is the same, so do a PUT of the whole thing. */
   uInstr2(cb, PUT,   4,  TempReg, t,   ArchReg, R_EAX);
}


static
Addr dis_cmpxchg_G_E ( UCodeBlock* cb, 
                       UChar       sorb,
                       Int         size, 
                       Addr        eip0 )
{
   Int   ta, junk, dest, src, acc;
   UChar dis_buf[50];
   UChar rm;

   rm   = getUChar(eip0);
   acc  = newTemp(cb);
   src  = newTemp(cb);
   dest = newTemp(cb);
   junk = newTemp(cb);
   /* Only needed to get gcc's dataflow analyser off my back. */
   ta   = INVALID_TEMPREG;

   if (epartIsReg(rm)) {
     uInstr2(cb, GET, size, ArchReg, eregOfRM(rm), TempReg, dest);
     eip0++;
     if (dis) VG_(printf)("cmpxchg%c %s,%s\n", 
                          nameISize(size),
                          nameIReg(size,gregOfRM(rm)),
                          nameIReg(size,eregOfRM(rm)) );
   } else {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL );
      ta        = LOW24(pair);
      uInstr2(cb, LOAD, size, TempReg, ta, TempReg, dest);
      eip0 += HI8(pair);
      if (dis) VG_(printf)("cmpxchg%c %s,%s\n",  nameISize(size), 
                           nameIReg(size,gregOfRM(rm)), dis_buf);
   }

   uInstr2(cb, GET, size, ArchReg, gregOfRM(rm), TempReg, src);
   uInstr2(cb, GET, size, ArchReg, R_EAX,        TempReg, acc);
   uInstr2(cb, MOV, size, TempReg, acc,          TempReg, junk);
   uInstr2(cb, SUB, size, TempReg, dest,         TempReg, junk);
   setFlagsFromUOpcode(cb, SUB);

   uInstr2(cb, CMOV, 4, TempReg, src,  TempReg, dest);
   uCond(cb, CondZ);
   uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
   uInstr2(cb, CMOV, 4, TempReg, dest, TempReg, acc);
   uCond(cb, CondNZ);
   uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);

   uInstr2(cb, PUT, size, TempReg, acc, ArchReg, R_EAX);
   if (epartIsReg(rm)) {
     uInstr2(cb, PUT,   size, TempReg, dest, ArchReg, eregOfRM(rm));
   } else {
     uInstr2(cb, STORE, size, TempReg, dest, TempReg, ta);
   }

   return eip0;
}


/* Handle conditional move instructions of the form
      cmovcc E(reg-or-mem), G(reg)

   E(src) is reg-or-mem
   G(dst) is reg.

   If E is reg, -->    GET %E, tmps
                       GET %G, tmpd
                       CMOVcc tmps, tmpd
                       PUT tmpd, %G
 
   If E is mem  -->    (getAddr E) -> tmpa
                       LD (tmpa), tmps
                       GET %G, tmpd
                       CMOVcc tmps, tmpd
                       PUT tmpd, %G
*/
static
Addr dis_cmov_E_G ( UCodeBlock* cb, 
                    UChar       sorb,
                    Int         size, 
                    Condcode    cond,
                    Addr        eip0 )
{
   UChar rm  = getUChar(eip0);
   UChar dis_buf[50];

   Int tmps = newTemp(cb);
   Int tmpd = newTemp(cb);   

   if (epartIsReg(rm)) {
      uInstr2(cb, GET,  size, ArchReg, eregOfRM(rm), TempReg, tmps);
      uInstr2(cb, GET,  size, ArchReg, gregOfRM(rm), TempReg, tmpd);
      uInstr2(cb, CMOV,    4, TempReg, tmps, TempReg, tmpd);
      uCond(cb, cond);
      uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
      uInstr2(cb, PUT, size, TempReg, tmpd, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("cmov%c%s %s,%s\n", 
                           nameISize(size), 
                           VG_(name_UCondcode)(cond),
                           nameIReg(size,eregOfRM(rm)),
                           nameIReg(size,gregOfRM(rm)));
      return 1+eip0;
   }

   /* E refers to memory */    
   {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      uInstr2(cb, LOAD, size, TempReg, tmpa, TempReg, tmps);
      uInstr2(cb, GET,  size, ArchReg, gregOfRM(rm), TempReg, tmpd);
      uInstr2(cb, CMOV,    4, TempReg, tmps, TempReg, tmpd);
      uCond(cb, cond);
      uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
      uInstr2(cb, PUT, size, TempReg, tmpd, ArchReg, gregOfRM(rm));
      if (dis) VG_(printf)("cmov%c%s %s,%s\n", 
                           nameISize(size), 
                           VG_(name_UCondcode)(cond),
                           dis_buf,
                           nameIReg(size,gregOfRM(rm)));
      return HI8(pair)+eip0;
   }
}


static
Addr dis_xadd_G_E ( UCodeBlock* cb, 
                    UChar       sorb,
                    Int         sz, 
                    Addr        eip0 )
{
   UChar rm  = getUChar(eip0);
   UChar dis_buf[50];

   Int tmpd = newTemp(cb);   
   Int tmpt = newTemp(cb);

   if (epartIsReg(rm)) {
      uInstr2(cb, GET, sz, ArchReg, eregOfRM(rm), TempReg, tmpd);
      uInstr2(cb, GET, sz, ArchReg, gregOfRM(rm), TempReg, tmpt);
      uInstr2(cb, ADD, sz, TempReg, tmpd, TempReg, tmpt);
      setFlagsFromUOpcode(cb, ADD);
      uInstr2(cb, PUT, sz, TempReg, tmpt, ArchReg, eregOfRM(rm));
      uInstr2(cb, PUT, sz, TempReg, tmpd, ArchReg, gregOfRM(rm));
      if (dis)
         VG_(printf)("xadd%c %s, %s\n", nameISize(sz), 
                     nameIReg(sz,gregOfRM(rm)), 
                     nameIReg(sz,eregOfRM(rm)));
      return 1+eip0;
   } else {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa  = LOW24(pair);
      uInstr2(cb, LOAD, sz, TempReg, tmpa,          TempReg, tmpd);
      uInstr2(cb, GET,  sz, ArchReg, gregOfRM(rm),  TempReg, tmpt);
      uInstr2(cb,  ADD, sz, TempReg, tmpd, TempReg, tmpt);
      setFlagsFromUOpcode(cb, ADD);
      uInstr2(cb, STORE, sz, TempReg, tmpt, TempReg, tmpa);
      uInstr2(cb, PUT, sz, TempReg, tmpd, ArchReg, gregOfRM(rm));
      if (dis)
         VG_(printf)("xadd%c %s, %s\n", nameISize(sz), 
                     nameIReg(sz,gregOfRM(rm)), 
                     dis_buf);
      return HI8(pair)+eip0;
   }
}


/* Moves of Ew into a segment register.
      mov Ew, Sw  meaning
      mov reg-or-mem, reg
   Is passed the a ptr to the modRM byte, and the data size.  Returns
   the address advanced completely over this instruction.

   Ew(src) is reg-or-mem
   Sw(dst) is seg reg.

   If E is reg, -->    GETw   %Ew,  tmpv
                       PUTSEG tmpv, %Sw
 
   If E is mem  -->    (getAddr E) -> tmpa
                       LDw (tmpa), tmpb
                       PUTSEG tmpb, %Sw
*/
static
Addr dis_mov_Ew_Sw ( UCodeBlock* cb, 
                     UChar       sorb,
                     Addr        eip0 )
{
   UChar rm  = getUChar(eip0);
   UChar dis_buf[50];

   if (epartIsReg(rm)) {
      Int tmpv = newTemp(cb);
      uInstr2(cb, GET,    2, ArchReg, eregOfRM(rm), TempReg, tmpv);
      uInstr2(cb, PUTSEG, 2, TempReg, tmpv, ArchRegS, gregOfRM(rm));
      if (dis) VG_(printf)("movw %s,%s\n",
                           nameIReg(2,eregOfRM(rm)),
                           nameSReg(gregOfRM(rm)));
      return 1+eip0;
   }

   /* E refers to memory */    
   {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      Int  tmpb = newTemp(cb);
      uInstr2(cb, LOAD,   2, TempReg, tmpa, TempReg, tmpb);
      uInstr2(cb, PUTSEG, 2, TempReg, tmpb, ArchRegS, gregOfRM(rm));
      if (dis) VG_(printf)("movw %s,%s\n",
                           dis_buf,nameSReg(gregOfRM(rm)));
      return HI8(pair)+eip0;
   }
}


/* Moves of a segment register to Ew.
      mov Sw, Ew  meaning
      mov reg, reg-or-mem
   Is passed the a ptr to the modRM byte, and the data size.  Returns
   the address advanced completely over this instruction.

   Sw(src) is seg reg.
   Ew(dst) is reg-or-mem

   If E is reg, -->    GETSEG %Sw,  tmp
                       PUTW tmp, %Ew
 
   If E is mem, -->    (getAddr E) -> tmpa
                       GETSEG %Sw, tmpv
                       STW tmpv, (tmpa) 
*/
static
Addr dis_mov_Sw_Ew ( UCodeBlock* cb, 
                     UChar       sorb,
                     Addr        eip0 )
{
   UChar rm = getUChar(eip0);
   UChar dis_buf[50];

   if (epartIsReg(rm)) {
      Int tmpv = newTemp(cb);
      uInstr2(cb, GETSEG, 2, ArchRegS, gregOfRM(rm), TempReg, tmpv);
      uInstr2(cb, PUT,    2, TempReg, tmpv, ArchReg, eregOfRM(rm));
      if (dis) VG_(printf)("movw %s,%s\n",
                           nameSReg(gregOfRM(rm)),
                           nameIReg(2,eregOfRM(rm)));
      return 1+eip0;
   }

   /* E refers to memory */    
   {
      UInt pair = disAMode ( cb, sorb, eip0, dis?dis_buf:NULL);
      Int  tmpa = LOW24(pair);
      Int  tmpv = newTemp(cb);
      uInstr2(cb, GETSEG, 2, ArchRegS, gregOfRM(rm), TempReg, tmpv);
      uInstr2(cb, STORE,  2, TempReg, tmpv, TempReg, tmpa);
      if (dis) VG_(printf)("mov %s,%s\n",
                           nameSReg(gregOfRM(rm)), dis_buf);
      return HI8(pair)+eip0;
   }
}



/* Simple MMX operations, either 
       op   (src)mmxreg, (dst)mmxreg
   or
       op   (src)address, (dst)mmxreg
   opc is the byte following the 0x0F prefix.
*/
static 
Addr dis_MMXop_regmem_to_reg ( UCodeBlock* cb, 
                               UChar sorb,
                               Addr eip,
                               UChar opc,
                               Char* name,
                               Bool show_granularity )
{
   UChar dis_buf[50];
   UChar modrm;
   modrm = getUChar(eip);
   if (epartIsReg(modrm)) {
      eip++;
      uInstr1(cb, MMX2, 0, 
                  Lit16, 
                  (((UShort)(opc)) << 8) | ((UShort)modrm) );
      if (dis)
         VG_(printf)("%s%s %s, %s\n", 
                     name,
                     show_granularity ? nameMMXGran(opc & 3) : (Char*)"",
                     nameMMXReg(eregOfRM(modrm)),
                     nameMMXReg(gregOfRM(modrm)));
   } else {
      UInt pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      Int  tmpa = LOW24(pair);
      eip += HI8(pair);
      uInstr2(cb, MMX2_MemRd, 8, 
                  Lit16, 
                  (((UShort)(opc)) << 8) | ((UShort)modrm),
                  TempReg, tmpa);
      if (dis)
         VG_(printf)("%s%s %s, %s\n", 
                     name,
                     show_granularity ? nameMMXGran(opc & 3) : (Char*)"",
                     dis_buf,
                     nameMMXReg(gregOfRM(modrm)));
   }
   return eip;
}



/* Simple SSE operations, either 
       op   (src)xmmreg, (dst)xmmreg
   or
       op   (src)address, (dst)xmmreg
   3 opcode bytes.
   Supplied eip points to the first address mode byte.
*/
static
Addr dis_SSE3_reg_or_mem ( UCodeBlock* cb, 
                           UChar sorb, 
			   Addr eip,
			   Int sz,
                           Char* name, 
                           UChar opc1, 
                           UChar opc2, 
                           UChar opc3 )
{
   UChar dis_buf[50];
   UChar modrm = getUChar(eip);
   if (epartIsReg(modrm)) {
      /* Completely internal SSE insn. */
      uInstr2(cb, SSE4, 0,  /* ignore sz for internal ops */
                  Lit16, (((UShort)opc1) << 8) | (UShort)opc2,
                  Lit16, (((UShort)opc3) << 8) | (UShort)modrm );
      if (dis)
         VG_(printf)("%s %s, %s\n", name, 
                     nameXMMReg(eregOfRM(modrm)), 
                     nameXMMReg(gregOfRM(modrm)) );
      eip++;
   } else {
      UInt pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      Int  tmpa = LOW24(pair);
      eip += HI8(pair);
      uInstr3(cb, SSE3a_MemRd, sz,
                  Lit16, (((UShort)(opc1)) << 8) | ((UShort)opc2),
                  Lit16, (((UShort)(opc3)) << 8) | ((UShort)modrm),
                  TempReg, tmpa);
      if (dis)
         VG_(printf)("%s %s, %s\n", 
                     name,
                     dis_buf,
                     nameXMMReg(gregOfRM(modrm)));
   }
   return eip;
}


/* Simple SSE operations, either 
       op   (src)xmmreg, (dst)xmmreg
   or
       op   (src)address, (dst)xmmreg
   2 opcode bytes.
   Supplied eip points to the first address mode byte.
*/
static
Addr dis_SSE2_reg_or_mem ( UCodeBlock* cb, 
                           UChar sorb, 
                           Addr eip,
                           Int sz, 
                           Char* name,
                           UChar opc1, 
                           UChar opc2 )
{
   UChar modrm = getUChar(eip);
   if (epartIsReg(modrm)) {
      /* Completely internal SSE insn. */
      uInstr2(cb, SSE3, 0,  /* ignore sz for internal ops */
                  Lit16, (((UShort)opc1) << 8) | (UShort)opc2,
                  Lit16, (UShort)modrm );
      if (dis)
         VG_(printf)("%s %s, %s\n", name, 
                     nameXMMReg(eregOfRM(modrm)), 
                     nameXMMReg(gregOfRM(modrm)) );
      eip++;
   } else {
      VG_(core_panic)("dis_SSE2_reg_or_mem: mem");
   }
   return eip;
}


/* Simple SSE operations, either 
       op   (src)xmmreg, (dst)xmmreg
   or
       op   (src)address, (dst)xmmreg
   2 opcode bytes and an 8-bit immediate after the amode.
   Supplied eip points to the first address mode byte.
*/
static
Addr dis_SSE2_reg_or_mem_Imm8 ( UCodeBlock* cb, 
                                UChar sorb, 
				Addr eip,
                                Int sz, 
                                Char* name,
				UChar opc1, 
				UChar opc2 )
{
   UChar modrm = getUChar(eip);
   UChar imm8;
   if (epartIsReg(modrm)) {
      /* Completely internal SSE insn. */
      eip++;
      imm8 = getUChar(eip);
      uInstr2(cb, SSE4, 0,  /* ignore sz for internal ops */
                  Lit16, (((UShort)opc1) << 8) | (UShort)opc2,
                  Lit16, (((UShort)modrm) << 8) | (UShort)imm8 );
      if (dis)
         VG_(printf)("%s %s, %s\n", name, 
                     nameXMMReg(eregOfRM(modrm)), 
                     nameXMMReg(gregOfRM(modrm)) );
      eip++;
   } else {
      VG_(core_panic)("dis_SSE2_reg_or_mem_Imm8: mem");
   }
   return eip;
}


/* Simple SSE operations, either 
       op   (src)xmmreg, (dst)xmmreg
   or
       op   (src)address, (dst)xmmreg
   3 opcode bytes and an 8-bit immediate after the amode.
   Supplied eip points to the first address mode byte.
*/
static
Addr dis_SSE3_reg_or_mem_Imm8 ( UCodeBlock* cb, 
                                UChar sorb, 
				Addr eip,
                                Int sz, 
                                Char* name,
				UChar opc1, 
				UChar opc2,
                                UChar opc3 )
{
   UChar modrm = getUChar(eip);
   UChar imm8;
   if (epartIsReg(modrm)) {
      /* Completely internal SSE insn. */
      eip++;
      imm8 = getUChar(eip);
      uInstr3(cb, SSE5, 0,  /* ignore sz for internal ops */
                  Lit16, (((UShort)opc1) << 8) | (UShort)opc2,
                  Lit16, (((UShort)opc3) << 8) | (UShort)modrm,
                  Lit16, (UShort)imm8 );
      if (dis)
         VG_(printf)("%s %s, %s\n", name, 
                     nameXMMReg(eregOfRM(modrm)), 
                     nameXMMReg(gregOfRM(modrm)) );
      eip++;
   } else {
      VG_(core_panic)("dis_SSE3_reg_or_mem_Imm8: mem");
   }
   return eip;
}


/* Disassemble an SSE insn which is either a simple reg-reg move or a
   move between registers and memory.  Supplied eip points to the
   first address mode byte.
*/
static
Addr dis_SSE3_load_store_or_mov ( UCodeBlock* cb, 
                                  UChar sorb,
                                  Addr eip, 
                                  Int sz,
				  Bool is_store, 
                                  Char* name,
                                  UChar insn0, 
                                  UChar insn1, 
                                  UChar insn2 )
{
   UChar dis_buf[50];
   UChar modrm; 
   UInt  pair;
   Int   t1;
   modrm = getUChar(eip);
   if (epartIsReg(modrm)) {
      /* Completely internal; we can issue SSE4. */
      eip++;
      uInstr2(cb, SSE4, 0, /* ignore sz for internal ops */
                  Lit16, (((UShort)insn0) << 8) | (UShort)insn1,
                  Lit16, (((UShort)insn2) << 8) | (UShort)modrm );
      if (dis && is_store)
         VG_(printf)("%s %s, %s\n", name,
                     nameXMMReg(gregOfRM(modrm)),
                     nameIReg(4,eregOfRM(modrm)) );
      if (dis && !is_store)
         VG_(printf)("%s %s, %s\n", name,
                      nameIReg(4,eregOfRM(modrm)), 
                      nameXMMReg(gregOfRM(modrm)) );
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      t1   = LOW24(pair);
      eip += HI8(pair);
      uInstr3(cb, is_store ? SSE3a_MemWr : SSE3a_MemRd, sz,
                  Lit16, (((UShort)insn0) << 8) | (UShort)insn1,
                  Lit16, (((UShort)insn2) << 8) | (UShort)modrm,
                  TempReg, t1 );
      if (dis && is_store)
         VG_(printf)("%s %s, %s\n", name,
                     nameXMMReg(gregOfRM(modrm)), dis_buf );
      if (dis && !is_store)
         VG_(printf)("%s %s, %s\n", name,
                     dis_buf, nameXMMReg(gregOfRM(modrm)) );
   }
   return eip;
}


/* Disassemble an SSE insn which is either a simple reg-reg move or a
   move between registers and memory.  Supplied eip points to the
   first address mode byte. */
static
Addr dis_SSE2_load_store_or_mov ( UCodeBlock* cb, 
                                  UChar sorb,
                                  Addr eip, 
                                  Int sz,
				  Bool is_store, 
                                  Char* name,
                                  UChar insn0, 
                                  UChar insn1 )
{
   UChar dis_buf[50];
   UChar modrm; 
   UInt  pair;
   Int   t1;
   modrm = getUChar(eip);
   if (epartIsReg(modrm)) {
      /* Completely internal; we can issue SSE3. */
      eip++;
      uInstr2(cb, SSE3, 0, /* ignore sz for internal ops */
                  Lit16, (((UShort)insn0) << 8) | (UShort)insn1,
                  Lit16, (UShort)modrm );
      if (dis && is_store)
         VG_(printf)("%s %s, %s\n", name,
                     nameXMMReg(gregOfRM(modrm)),
                     nameIReg(4,eregOfRM(modrm)) );
      if (dis && !is_store)
         VG_(printf)("%s %s, %s\n", name,
                      nameIReg(4,eregOfRM(modrm)), 
                      nameXMMReg(gregOfRM(modrm)) );
   } else {
      pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
      t1   = LOW24(pair);
      eip += HI8(pair);
      uInstr3(cb, is_store ? SSE2a_MemWr : SSE2a_MemRd, sz,
                  Lit16, (((UShort)insn0) << 8) | (UShort)insn1,
                  Lit16, (UShort)modrm,
                  TempReg, t1 );
      if (dis && is_store)
         VG_(printf)("%s %s, %s\n", name,
                     nameXMMReg(gregOfRM(modrm)), dis_buf );
      if (dis && !is_store)
         VG_(printf)("%s %s, %s\n", name,
                     dis_buf, nameXMMReg(gregOfRM(modrm)) );
   }
   return eip;
}


/*------------------------------------------------------------*/
/*--- Disassembling entire basic blocks                    ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into ucode, returning the updated
   eip, and setting *isEnd to True if this is the last insn in a basic
   block.  Also do debug printing if (dis). */

static Addr disInstr ( UCodeBlock* cb, Addr eip, Bool* isEnd )
{
   UChar opc, modrm, abyte;
   UInt  d32, pair;
   Int   t1, t2, t3, t4;
   UChar dis_buf[50];
   Int   am_sz, d_sz;

   /* Holds eip at the start of the insn, so that we can print
      consistent error messages for unimplemented insns. */
   UChar* eip_start = (UChar*)eip;

   /* sz denotes the nominal data-op size of the insn; we change it to
      2 if an 0x66 prefix is seen */
   Int sz = 4;

   /* sorb holds the segment-override-prefix byte, if any.  Zero if no
      prefix has been seen, else one of {0x26, 0x3E, 0x64, 0x65}
      indicating the prefix.  */
   UChar sorb = 0;

   Int first_uinstr = cb->used;
   *isEnd = False;
   t1 = t2 = t3 = t4 = INVALID_TEMPREG;

   if (dis) VG_(printf)("\t0x%x:  ", eip);

   /* Spot the client-request magic sequence. */
   {
      UChar* myeip = (UChar*)eip;
      /* Spot this:
         C1C01D                roll $29, %eax
         C1C003                roll $3,  %eax
         C1C81B                rorl $27, %eax
         C1C805                rorl $5,  %eax
         C1C00D                roll $13, %eax
         C1C013                roll $19, %eax      
      */
      if (myeip[ 0] == 0xC1 && myeip[ 1] == 0xC0 && myeip[ 2] == 0x1D &&
          myeip[ 3] == 0xC1 && myeip[ 4] == 0xC0 && myeip[ 5] == 0x03 &&
          myeip[ 6] == 0xC1 && myeip[ 7] == 0xC8 && myeip[ 8] == 0x1B &&
          myeip[ 9] == 0xC1 && myeip[10] == 0xC8 && myeip[11] == 0x05 &&
          myeip[12] == 0xC1 && myeip[13] == 0xC0 && myeip[14] == 0x0D &&
          myeip[15] == 0xC1 && myeip[16] == 0xC0 && myeip[17] == 0x13
         ) {
         eip += 18;
         uInstr1(cb, JMP,  0, Literal, 0);
         uLiteral(cb, eip);
         uCond(cb, CondAlways);
         LAST_UINSTR(cb).jmpkind = JmpClientReq;
         *isEnd = True;
         if (dis) 
            VG_(printf)("%%edx = client_request ( %%eax )\n");
         return eip;
      }
   }

   /* Skip a LOCK prefix. */
   if (getUChar(eip) == 0xF0) { 
      /* VG_(printf)("LOCK LOCK LOCK LOCK LOCK \n"); */
      uInstr0(cb, LOCK, 0);
      eip++;
   }

   /* Detect operand-size overrides. */
   if (getUChar(eip) == 0x66) { sz = 2; eip++; };

   /* segment override prefixes come after the operand-size override,
      it seems */
   switch (getUChar(eip)) {
      case 0x3E: /* %DS: */
      case 0x26: /* %ES: */
      case 0x64: /* %FS: */
      case 0x65: /* %GS: */
         sorb = getUChar(eip); eip++; 
         break;
      case 0x2E: /* %CS: */
         VG_(unimplemented)("x86 segment override (SEG=CS) prefix");
         /*NOTREACHED*/
         break;
      case 0x36: /* %SS: */
         VG_(unimplemented)("x86 segment override (SEG=SS) prefix");
         /*NOTREACHED*/
         break;
      default:
         break;
   }

   /* ---------------------------------------------------- */
   /* --- The SSE/SSE2 decoder.                        --- */
   /* ---------------------------------------------------- */

   /* If it looks like this CPU might support SSE, try decoding SSE
      insns.  */
   if (VG_(have_ssestate)) {
   UChar* insn = (UChar*)eip;

   /* STMXCSR/LDMXCSR m32 -- load/store the MXCSR register. */
   if (insn[0] == 0x0F && insn[1] == 0xAE 
       && (gregOfRM(insn[2]) == 3 || gregOfRM(insn[2]) == 2) ) {
      Bool store = gregOfRM(insn[2]) == 3;
      vg_assert(sz == 4);
      pair = disAMode ( cb, sorb, eip+2, dis?dis_buf:NULL );
      t1   = LOW24(pair);
      eip += 2+HI8(pair);
      uInstr3(cb, store ? SSE2a_MemWr : SSE2a_MemRd, 4,
                  Lit16, (((UShort)insn[0]) << 8) | (UShort)insn[1],
                  Lit16, (UShort)insn[2],
                  TempReg, t1 );
      if (dis)
         VG_(printf)("%smxcsr %s\n", store ? "st" : "ld", dis_buf );
      goto decode_success;
   }

   /* CVTTSD2SI (0xF2,0x0F,0x2C) -- convert a double-precision float value in
      memory or xmm reg to int and put it in an ireg.  Truncate. */
   /* CVTTSS2SI (0xF3,0x0F,0x2C) -- convert a single-precision float value in
      memory or xmm reg to int and put it in an ireg.  Truncate. */
   /* CVTSD2SI (0xF2,0x0F,0x2D) -- convert a double-precision float value in
      memory or xmm reg to int and put it in an ireg.  Round as per MXCSR. */
   /* CVTSS2SI (0xF3,0x0F,0x2D) -- convert a single-precision float value in
      memory or xmm reg to int and put it in an ireg.  Round as per MXCSR. */
   if ((insn[0] == 0xF2 || insn[0] == 0xF3)
       && insn[1] == 0x0F 
       && (insn[2] == 0x2C || insn[2] == 0x2D)) {
      vg_assert(sz == 4);
      modrm = insn[3];
      if (epartIsReg(modrm)) {
         /* We're moving a value in an xmm reg to an ireg. */
         eip += 4;
	 t1 = newTemp(cb);
         /* sz is 4 for all 4 insns. */
         vg_assert(epartIsReg(modrm));
         uInstr3(cb, SSE3g_RegWr, 4,
                     Lit16, (((UShort)insn[0]) << 8) | (UShort)insn[1],
                     Lit16, (((UShort)insn[2]) << 8) | (UShort)modrm,
                     TempReg, t1 );
         uInstr2(cb, PUT, 4, TempReg, t1, ArchReg, gregOfRM(modrm));
	 if (dis)
            VG_(printf)("cvt{t}s{s,d}2si %s, %s\n", 
                        nameXMMReg(eregOfRM(modrm)),
                        nameIReg(4,gregOfRM(modrm)) );
      } else {
         /* So, we're reading memory and writing an ireg.  This calls
            for the ultra-horrible SSE3ag_MemRd_RegWr uinstr.  We
            can't do it in a roundabout route because it does some
            kind of conversion on the way, which we need to have
            happen too.  So our only choice is to re-emit a suitably
            rehashed version of the instruction. */
 	 /* Destination ireg is GREG.  Address goes as EREG as
	    usual. */
         t1 = newTemp(cb); /* t1 holds value on its way to ireg */
         pair = disAMode ( cb, sorb, eip+3, dis?dis_buf:NULL );
         t2   = LOW24(pair); /* t2 holds addr */
         eip += 3+HI8(pair);
         uInstr2(cb, SSE3ag_MemRd_RegWr, insn[0]==0xF2 ? 8 : 4,
                     TempReg, t2, /* address */
                     TempReg, t1 /* dest */);
         uLiteral(cb  , (((UInt)insn[0]) << 24)
                      | (((UInt)insn[1]) << 16)
                      | (((UInt)insn[2]) << 8) 
                      | ((UInt)modrm) );
         uInstr2(cb, PUT, 4, TempReg, t1, ArchReg, gregOfRM(modrm));
	 if (dis)
            VG_(printf)("cvt{t}s{s,d}2si %s, %s\n", 
                        dis_buf,
                        nameIReg(4,gregOfRM(modrm)) );
      }
      goto decode_success;
   }

   /* CVTSI2SS -- convert int reg, or int value in memory, to low 4
      bytes of XMM reg. */
   /* CVTSI2SD -- convert int reg, or int value in memory, to low 8
      bytes of XMM reg. */
   if ((insn[0] == 0xF3 /*CVTSI2SS*/ || insn[0] == 0xF2 /* CVTSI2SD*/)
       && insn[1] == 0x0F && insn[2] == 0x2A) {
      Char* s_or_d = insn[0]==0xF3 ? "s" : "d";
      vg_assert(sz == 4);
      modrm = insn[3];
      t1 = newTemp(cb);
      if (epartIsReg(modrm)) { 
         uInstr2(cb, GET, 4, ArchReg, eregOfRM(modrm), TempReg, t1);
         vg_assert(epartIsReg(modrm));
         uInstr3(cb, SSE3e_RegRd, 4,
                     Lit16, (((UShort)insn[0]) << 8) | (UShort)insn[1],
                     Lit16, (((UShort)insn[2]) << 8) | (UShort)modrm,
                     TempReg, t1 );
         eip += 4;
	 if (dis)
            VG_(printf)("cvtsi2s%s %s, %s\n", s_or_d,
                        nameIReg(4,eregOfRM(modrm)), 
                        nameXMMReg(gregOfRM(modrm)));
      } else {
         pair = disAMode ( cb, sorb, eip+3, dis?dis_buf:NULL );
         t2   = LOW24(pair);
         eip += 3+HI8(pair);
	 uInstr3(cb, SSE3a_MemRd, 4,
                     Lit16, (((UShort)insn[0]) << 8) | (UShort)insn[1],
                     Lit16, (((UShort)insn[2]) << 8) | (UShort)modrm,
                     TempReg, t2 );
	 if (dis)
            VG_(printf)("cvtsi2s%s %s, %s\n", s_or_d,
                        dis_buf,
                        nameXMMReg(gregOfRM(modrm)));
      }
      goto decode_success;
   }

   /* DIVSS -- divide low 4 bytes of XMM reg. */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x5E) {
      vg_assert(sz == 4);
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+3, 4, "divss",
                                      insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* MULSS -- multiply low 4 bytes of XMM reg. */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x59) {
      vg_assert(sz == 4);
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+3, 4, "mulss",
                                      insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* CVTSS2SD -- convert one single float to double. */
   if (insn[0] == 0xF3 && insn[1] == 0x0F && insn[2] == 0x5A) {
      vg_assert(sz == 4);
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+3, 4, "cvtss2sd",
                                      insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* CVTSD2SS -- convert one single double. to float. */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x5A) {
      vg_assert(sz == 4);
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+3, 8, "cvtsd2ss",
                                      insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* PSHUFD */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x70) {
      eip = dis_SSE3_reg_or_mem_Imm8 ( cb, sorb, eip+2, 16, 
                                           "pshufd",
                                           0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* SHUFPS */
   if (insn[0] == 0x0F && insn[1] == 0xC6) {
      vg_assert(sz == 4);
      eip = dis_SSE2_reg_or_mem_Imm8 ( cb, sorb, eip+2, 16, "shufps",
                                           insn[0], insn[1] );
      goto decode_success;
   }

   /* MULPS */
   if (insn[0] == 0x0F && insn[1] == 0x59) {
      vg_assert(sz == 4);
      eip = dis_SSE2_reg_or_mem ( cb, sorb, eip+2, 16, "mulps",
                                      insn[0], insn[1] );
      goto decode_success;
   }

   /* MULSD */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x59) {
      vg_assert(sz == 4);
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+3, 8, "mulsd",
                                      insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* DIVSD */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x5E) {
      vg_assert(sz == 4);
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+3, 8, "divsd",
                                      insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* SUBSD */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x5C) {
      vg_assert(sz == 4);
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+3, 8, "subsd",
                                      insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* ADDSD */
   if (insn[0] == 0xF2 && insn[1] == 0x0F && insn[2] == 0x58) {
      vg_assert(sz == 4);
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+3, 8, "addsd",
                                      insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* ADDPS */
   if (insn[0] == 0x0F && insn[1] == 0x58) {
      vg_assert(sz == 4);
      eip = dis_SSE2_reg_or_mem ( cb, sorb, eip+2, 16, "addps",
                                      insn[0], insn[1] );
      goto decode_success;
   }

   /* XORPD (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x57) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "xorpd",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* PXOR (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0xEF) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pxor",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* PAND (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0xDB) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pand",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* PANDN (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0xDF) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pandn",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* POR (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0xEB) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "por",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xDA: PMINUB(src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xEA: PMINSW(src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xDA || insn[1] == 0xEA)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pmin{ub,sw}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xDE: PMAXUB(src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xEE: PMAXSW(src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xDE || insn[1] == 0xEE)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pmax{ub,sw}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xE0: PAVGB(src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xE3: PAVGW(src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xE0 || insn[1] == 0xE3)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pavg{b,w}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0x60: PUNPCKLBW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x61: PUNPCKLWD (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x62: PUNPCKLDQ (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0x60 || insn[1] == 0x61 || insn[1] == 0x62)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, 
                                      "punpckl{bw,wd,dq}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0x68: PUNPCKHBW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x69: PUNPCKHWD (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x6A: PUNPCKHDQ (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0x68 || insn[1] == 0x69 || insn[1] == 0x6A)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, 
                                      "punpckh{bw,wd,dq}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xFC: PADDB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xFD: PADDW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xFE: PADDD (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xD4: PADDQ (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xFC || insn[1] == 0xFD 
           || insn[1] == 0xFE || insn[1] == 0xD4)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "padd{b,w,d,q}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xEC: PADDSB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xED: PADDSW (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xEC || insn[1] == 0xED)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "padds{b,w}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xDC: PADDUSB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xDD: PADDUSW (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xDC || insn[1] == 0xDD)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "paddus{b,w}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xF8: PSUBB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xF9: PSUBW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xFA: PSUBD (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xFB: PSUBQ (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xF8 || insn[1] == 0xF9 
           || insn[1] == 0xFA || insn[1] == 0xFB)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "psub{b,w,d,q}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xE8: PSUBSB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xE9: PSUBSW (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xE8 || insn[1] == 0xE9)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "psubs{b,w}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xD8: PSUBUSB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xD9: PSUBUSW (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xD8 || insn[1] == 0xD9)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "psubus{b,w}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xE5: PMULHW(src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xD5: PMULLW(src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xE5 || insn[1] == 0xD5)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pmul{h,l}w",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xF5: PMADDWD(src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && insn[1] == 0xF5) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pmaddwd",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0x74: PCMPEQB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x75: PCMPEQW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x76: PCMPEQD (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0x74 || insn[1] == 0x75 || insn[1] == 0x76)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pcmpeq{b,w,d}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0x64: PCMPGTB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x65: PCMPGTW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x66: PCMPGTD (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0x64 || insn[1] == 0x65 || insn[1] == 0x66)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "pcmpgt{b,w,d}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0x63: PACKSSWB (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0x6B: PACKSSDW (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0x63 || insn[1] == 0x6B)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "packss{wb,dw}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0x67: PACKUSWB (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && insn[1] == 0x67) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "packuswb",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xF1: PSLLW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xF2: PSLLD (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xF3: PSLLQ (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xF1 || insn[1] == 0xF2 || insn[1] == 0xF3)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "psll{b,w,d}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xD1: PSRLW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xD2: PSRLD (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xD3: PSRLQ (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xD1 || insn[1] == 0xD2 || insn[1] == 0xD3)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "psrl{b,w,d}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* 0xE1: PSRAW (src)xmmreg-or-mem, (dst)xmmreg */
   /* 0xE2: PSRAD (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F 
       && (insn[1] == 0xE1 || insn[1] == 0xE2)) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 16, "psra{w,d}",
                                      0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* COMISD (src)xmmreg-or-mem, (dst)xmmreg */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x2F) {
      eip = dis_SSE3_reg_or_mem ( cb, sorb, eip+2, 8, "comisd",
                                      0x66, insn[0], insn[1] );
      vg_assert(LAST_UINSTR(cb).opcode == SSE3a_MemRd 
                || LAST_UINSTR(cb).opcode == SSE4);
      uFlagsRWU(cb, FlagsEmpty, FlagsZCP, FlagsEmpty);
      goto decode_success;
   }

   /* MOVSD -- move 8 bytes of XMM reg to/from XMM reg or mem. */
   if (insn[0] == 0xF2
       && insn[1] == 0x0F 
       && (insn[2] == 0x11 || insn[2] == 0x10)) {
      vg_assert(sz == 4);
      eip = dis_SSE3_load_store_or_mov 
               ( cb, sorb, eip+3, 8, insn[2]==0x11, "movsd",
		 insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* MOVQ -- move 8 bytes of XMM reg to XMM reg or mem.  How
      does this differ from MOVSD ?? */
   if (sz == 2
       && insn[0] == 0x0F
       && insn[1] == 0xD6) {
      eip = dis_SSE3_load_store_or_mov 
               ( cb, sorb, eip+2, 8, True /*store*/, "movq",
                     0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* MOVSS -- move 4 bytes of XMM reg to/from XMM reg or mem. */
   if (insn[0] == 0xF3
       && insn[1] == 0x0F 
       && (insn[2] == 0x11 || insn[2] == 0x10)) {
      vg_assert(sz == 4);
      eip = dis_SSE3_load_store_or_mov 
               ( cb, sorb, eip+3, 4, insn[2]==0x11, "movss",
                     insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* I don't understand how MOVAPD differs from MOVAPS. */
   /* MOVAPD (28,29) -- aligned load/store of xmm reg, or xmm-xmm reg
      move */
   if (sz == 2
       && insn[0] == 0x0F && insn[1] == 0x28) {
      UChar* name = "movapd";
                    //(insn[1] == 0x10 || insn[1] == 0x11)
                    // ? "movups" : "movaps";
      Bool store = False; //insn[1] == 0x29 || insn[1] == 11;
      eip = dis_SSE3_load_store_or_mov
               ( cb, sorb, eip+2, 16, store, name,
                     0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* MOVAPS (28,29) -- aligned load/store of xmm reg, or xmm-xmm reg
      move */
   /* MOVUPS (10,11) -- unaligned load/store of xmm reg, or xmm-xmm
      reg move */
   if (insn[0] == 0x0F && (insn[1] == 0x28
                           || insn[1] == 0x29
                           || insn[1] == 0x10
                           || insn[1] == 0x11)) {
      UChar* name = (insn[1] == 0x10 || insn[1] == 0x11)
                    ? "movups" : "movaps";
      Bool store = insn[1] == 0x29 || insn[1] == 11;
      vg_assert(sz == 4);
      eip = dis_SSE2_load_store_or_mov
               ( cb, sorb, eip+2, 16, store, name,
                     insn[0], insn[1] );
      goto decode_success;
   }

   /* MOVDQA -- aligned 16-byte load/store. */
   if (sz == 2 
       && insn[0] == 0x0F 
       && (insn[1] == 0x6F || insn[1] == 0x7F)) {
      Bool is_store = insn[1]==0x7F;
      eip = dis_SSE3_load_store_or_mov
               (cb, sorb, eip+2, 16, is_store, "movdqa", 
                    0x66, insn[0], insn[1] );
      goto decode_success;
   }

   /* MOVDQU -- unaligned 16-byte load/store. */
   if (insn[0] == 0xF3
       && insn[1] == 0x0F 
       && (insn[2] == 0x6F || insn[2] == 0x7F)) {
      Bool is_store = insn[2]==0x7F;
      eip = dis_SSE3_load_store_or_mov
               (cb, sorb, eip+3, 16, is_store, "movdqu", 
                    insn[0], insn[1], insn[2] );
      goto decode_success;
   }

   /* MOVD -- 4-byte move between xmmregs and (ireg or memory). */
   if (sz == 2 
       && insn[0] == 0x0F 
       && (insn[1] == 0x6E || insn[1] == 0x7E)) {
      Bool is_store = insn[1]==0x7E;
      modrm = insn[2];
      if (epartIsReg(modrm) && is_store) {
         t1 = newTemp(cb);
         uInstr3(cb, SSE3e_RegWr, 4,
                     Lit16, (((UShort)0x66) << 8) | (UShort)insn[0],
                     Lit16, (((UShort)insn[1]) << 8) | (UShort)modrm,
                     TempReg, t1 );
	 uInstr2(cb, PUT, 4, TempReg, t1, ArchReg, eregOfRM(modrm));
	 if (dis)
	    VG_(printf)("movd %s, %s\n", 
		        nameXMMReg(gregOfRM(modrm)),
		        nameIReg(4,eregOfRM(modrm)));
      } else
      if (epartIsReg(modrm) && !is_store) {
         t1 = newTemp(cb);
	 uInstr2(cb, GET, 4, ArchReg, eregOfRM(modrm), TempReg, t1);
         uInstr3(cb, SSE3e_RegRd, 4,
                     Lit16, (((UShort)0x66) << 8) | (UShort)insn[0],
                     Lit16, (((UShort)insn[1]) << 8) | (UShort)modrm,
                     TempReg, t1 );
	 if (dis)
	    VG_(printf)("movd %s, %s\n", 
		        nameIReg(4,eregOfRM(modrm)),
		        nameXMMReg(gregOfRM(modrm)));
      } else {
         eip = dis_SSE3_load_store_or_mov
                  (cb, sorb, eip+2, 4, is_store, "movd", 
                       0x66, insn[0], insn[1] );
      }
      goto decode_success;
   }

   /* PEXTRW from SSE register; writes ireg */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xC5) {
      t1 = newTemp(cb);
      modrm = insn[2];
      vg_assert(epartIsReg(modrm));
      vg_assert((modrm & 0xC0) == 0xC0);
      uInstr3(cb, SSE3g1_RegWr, 4,
                  Lit16, (((UShort)0x66) << 8) | (UShort)insn[0],
                  Lit16, (((UShort)insn[1]) << 8) | (UShort)modrm,
                  TempReg, t1 );
      uLiteral(cb, insn[3]);
      uInstr2(cb, PUT, 4, TempReg, t1, ArchReg, gregOfRM(modrm));
      if (dis)
         VG_(printf)("pextrw %s, %d, %s\n",
                     nameXMMReg(eregOfRM(modrm)), (Int)insn[3], 
                                nameIReg(4, gregOfRM(modrm)));
      eip += 4;
      goto decode_success;
   }

   /* PINSRW to SSE register; reads mem or ireg */
   if (sz == 2 && insn[0] == 0x0F && insn[1] == 0xC4) {
      t1 = newTemp(cb);
      modrm = insn[2];
      if (epartIsReg(modrm)) {
         uInstr2(cb, GET, 2, ArchReg, eregOfRM(modrm), TempReg, t1);
         vg_assert(epartIsReg(modrm));
         uInstr3(cb, SSE3e1_RegRd, 2,
                     Lit16, (((UShort)0x66) << 8) | (UShort)insn[0],
                     Lit16, (((UShort)insn[1]) << 8) | (UShort)modrm,
                     TempReg, t1 );
         uLiteral(cb, insn[3]);
         if (dis)
            VG_(printf)("pinsrw %s, %d, %s\n",
                        nameIReg(2, eregOfRM(modrm)),
                        (Int)insn[3], 
                        nameXMMReg(gregOfRM(modrm)));
         eip += 4;
      } else {
	 VG_(core_panic)("PINSRW mem");
      }
      goto decode_success;
   }

   /* Fall through into the non-SSE decoder. */

   } /* if (VG_(have_ssestate)) */


   /* ---------------------------------------------------- */
   /* --- end of the SSE/SSE2 decoder.                 --- */
   /* ---------------------------------------------------- */

   /* Get the primary opcode. */
   opc = getUChar(eip); eip++;

   /* We get here if the current insn isn't SSE, or this CPU doesn't
      support SSE. */

   switch (opc) {

   /* ------------------------ Control flow --------------- */

   case 0xC2: /* RET imm16 */
      d32 = getUDisp16(eip); eip += 2;
      goto do_Ret;
   case 0xC3: /* RET */
      d32 = 0;
      goto do_Ret;
   do_Ret:
      t1 = newTemp(cb); t2 = newTemp(cb);
      uInstr2(cb, GET,  4, ArchReg, R_ESP, TempReg, t1);
      uInstr2(cb, LOAD, 4, TempReg, t1,    TempReg, t2);
      uInstr2(cb, ADD,  4, Literal, 0,     TempReg, t1);
      uLiteral(cb, 4+d32);
      uInstr2(cb, PUT,  4, TempReg, t1,    ArchReg, R_ESP);
      uInstr1(cb, JMP,  0, TempReg, t2);
      uCond(cb, CondAlways);
      LAST_UINSTR(cb).jmpkind = JmpRet;

      *isEnd = True;
      if (dis) {
         if (d32 == 0) VG_(printf)("ret\n"); 
                  else VG_(printf)("ret %d\n", d32);
      }
      break;
      
   case 0xE8: /* CALL J4 */
      d32 = getUDisp32(eip); eip += 4;
      d32 += eip; /* eip now holds return-to addr, d32 is call-to addr */
      if (d32 == eip && getUChar(eip) >= 0x58 
                     && getUChar(eip) <= 0x5F) {
         /* Specially treat the position-independent-code idiom 
                 call X
              X: popl %reg
            as 
                 movl %eip, %reg.
            since this generates better code, but for no other reason. */
         Int archReg = getUChar(eip) - 0x58;
         /* VG_(printf)("-- fPIC thingy\n"); */
         t1 = newTemp(cb);
         uInstr2(cb, MOV, 4, Literal, 0, TempReg, t1);
         uLiteral(cb, eip);
         uInstr2(cb, PUT, 4, TempReg, t1,  ArchReg, archReg);
         eip++; /* Step over the POP */
         if (dis) 
            VG_(printf)("call 0x%x ; popl %s\n",d32,nameIReg(4,archReg));
      } else {
         /* The normal sequence for a call. */
         t1 = newTemp(cb); t2 = newTemp(cb); t3 = newTemp(cb);
         uInstr2(cb, GET,   4, ArchReg, R_ESP, TempReg, t3);
         uInstr2(cb, MOV,   4, TempReg, t3,    TempReg, t1);
         uInstr2(cb, SUB,   4, Literal, 0,     TempReg, t1);
	 uLiteral(cb, 4);
         uInstr2(cb, PUT,   4, TempReg, t1,    ArchReg, R_ESP);
         uInstr2(cb, MOV,   4, Literal, 0,     TempReg, t2);
	 uLiteral(cb, eip);
         uInstr2(cb, STORE, 4, TempReg, t2,    TempReg, t1);
         uInstr1(cb, JMP,   0, Literal, 0);
	 uLiteral(cb, d32);
         uCond(cb, CondAlways);
         LAST_UINSTR(cb).jmpkind = JmpCall;
         *isEnd = True;
         if (dis) VG_(printf)("call 0x%x\n",d32);
      }
      break;

   case 0xC9: /* LEAVE */
      t1 = newTemp(cb); t2 = newTemp(cb);
      uInstr2(cb, GET,  4, ArchReg, R_EBP, TempReg, t1);
      uInstr2(cb, PUT,  4, TempReg, t1, ArchReg, R_ESP);
      uInstr2(cb, LOAD, 4, TempReg, t1, TempReg, t2);
      uInstr2(cb, PUT,  4, TempReg, t2, ArchReg, R_EBP);
      uInstr2(cb, ADD,  4, Literal, 0, TempReg, t1);
      uLiteral(cb, 4);
      /* This 2nd PUT looks redundant, but Julian thinks it's not.
       * --njn 03-feb-2003 */
      uInstr2(cb, PUT,  4, TempReg, t1, ArchReg, R_ESP);
      if (dis) VG_(printf)("leave");
      break;

   /* ---------------- Misc wierd-ass insns --------------- */

   case 0x27: /* DAA */
   case 0x2F: /* DAS */
      t1 = newTemp(cb);
      uInstr2(cb, GET, 1, ArchReg, R_AL, TempReg, t1);
      /* Widen %AL to 32 bits, so it's all defined when we push it. */
      uInstr1(cb, WIDEN, 4, TempReg, t1);
      LAST_UINSTR(cb).extra4b = 1;
      LAST_UINSTR(cb).signed_widen = False;
      uInstr0(cb, CALLM_S, 0);
      uInstr1(cb, PUSH, 4, TempReg, t1);
      uInstr1(cb, CALLM, 0, Lit16, 
                  opc == 0x27 ? VGOFF_(helper_DAA) : VGOFF_(helper_DAS) );
      uFlagsRWU(cb, FlagsAC, FlagsOSZACP, FlagsEmpty);
      uInstr1(cb, POP, 4, TempReg, t1);
      uInstr0(cb, CALLM_E, 0);
      uInstr2(cb, PUT, 1, TempReg, t1, ArchReg, R_AL);
      if (dis) VG_(printf)(opc == 0x27 ? "daa\n" : "das\n");
      break;

   /* ------------------------ CWD/CDQ -------------------- */

   case 0x98: /* CBW */
      t1 = newTemp(cb);
      if (sz == 4) {
         uInstr2(cb, GET,   2, ArchReg, R_EAX, TempReg, t1);
         uInstr1(cb, WIDEN, 4, TempReg, t1); /* 4 == dst size */
         LAST_UINSTR(cb).extra4b = 2; /* the source size */
         LAST_UINSTR(cb).signed_widen = True;
         uInstr2(cb, PUT, 4, TempReg, t1, ArchReg, R_EAX);
         if (dis) VG_(printf)("cwd\n");
      } else {
         vg_assert(sz == 2);
         uInstr2(cb, GET,   1, ArchReg, R_EAX, TempReg, t1);
         uInstr1(cb, WIDEN, 2, TempReg, t1); /* 2 == dst size */
         LAST_UINSTR(cb).extra4b = 1; /* the source size */
         LAST_UINSTR(cb).signed_widen = True;
         uInstr2(cb, PUT, 2, TempReg, t1, ArchReg, R_EAX);
         if (dis) VG_(printf)("cbw\n");
      }
      break;

   case 0x99: /* CWD/CDQ */
      t1 = newTemp(cb);
      uInstr2(cb, GET, sz, ArchReg, R_EAX, TempReg, t1);
      uInstr2(cb, SAR, sz, Literal, 0,     TempReg, t1);
      uLiteral(cb, sz == 2 ? 15  : 31);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, R_EDX);
      if (dis) VG_(printf)(sz == 2 ? "cwdq\n" : "cdqq\n");
      break;

   /* ------------------------ FPU ops -------------------- */

   case 0x9E: /* SAHF */
      codegen_SAHF ( cb );
      if (dis) VG_(printf)("sahf\n");
      break;

   case 0x9F: /* LAHF */
      codegen_LAHF ( cb );
      if (dis) VG_(printf)("lahf\n");
      break;

   case 0x9B: /* FWAIT */
      /* ignore? */
      if (dis) VG_(printf)("fwait\n");
      break;

   case 0xD8:
   case 0xD9:
   case 0xDA:
   case 0xDB:
   case 0xDC:
   case 0xDD:
   case 0xDE:
   case 0xDF:
      eip = dis_fpu ( cb, sorb, opc, eip );
      break;

   /* ------------------------ INC & DEC ------------------ */

   case 0x40: /* INC eAX */
   case 0x41: /* INC eCX */
   case 0x42: /* INC eDX */
   case 0x43: /* INC eBX */
   case 0x45: /* INC eBP */
   case 0x46: /* INC eSI */
   case 0x47: /* INC eDI */
      t1 = newTemp(cb);
      uInstr2(cb, GET, sz, ArchReg, (UInt)(opc - 0x40),
                             TempReg, t1);
      uInstr1(cb, INC, sz, TempReg, t1);
      setFlagsFromUOpcode(cb, INC);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg,
                             (UInt)(opc - 0x40));
      if (dis)
         VG_(printf)("inc%c %s\n", nameISize(sz), nameIReg(sz,opc-0x40));
      break;

   case 0x48: /* DEC eAX */
   case 0x49: /* DEC eCX */
   case 0x4A: /* DEC eDX */
   case 0x4B: /* DEC eBX */
   case 0x4D: /* DEC eBP */
   case 0x4E: /* DEC eSI */
   case 0x4F: /* DEC eDI */
      t1 = newTemp(cb);
      uInstr2(cb, GET, sz, ArchReg, (UInt)(opc - 0x48),
                             TempReg, t1);
      uInstr1(cb, DEC, sz, TempReg, t1);
      setFlagsFromUOpcode(cb, DEC);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg,
                             (UInt)(opc - 0x48));
      if (dis)
         VG_(printf)("dec%c %s\n", nameISize(sz), nameIReg(sz,opc-0x48));
      break;

   /* ------------------------ INT ------------------------ */

   case 0xCD: /* INT imm8 */
      d32 = getUChar(eip); eip++;
      if (d32 != 0x80) VG_(core_panic)("disInstr: INT but not 0x80 !");
      /* It's important that all ArchRegs carry their up-to-date value
         at this point.  So we declare an end-of-block here, which
         forces any TempRegs caching ArchRegs to be flushed. */
      uInstr1(cb, JMP,  0, Literal, 0);
      uLiteral(cb, eip);
      uCond(cb, CondAlways);
      LAST_UINSTR(cb).jmpkind = JmpSyscall;
      *isEnd = True;
      if (dis) VG_(printf)("int $0x80\n");
      break;

   /* ------------------------ Jcond, byte offset --------- */

   case 0xEB: /* Jb (jump, byte offset) */
      d32 = (eip+1) + getSDisp8(eip); eip++;
      uInstr1(cb, JMP, 0, Literal, 0);
      uLiteral(cb, d32);
      uCond(cb, CondAlways);
      *isEnd = True;
      if (dis)
         VG_(printf)("jmp-8 0x%x\n", d32);
      break;

   case 0xE9: /* Jv (jump, 16/32 offset) */
      d32 = (eip+sz) + getSDisp(sz,eip); eip += sz;
      uInstr1(cb, JMP, 0, Literal, 0);
      uLiteral(cb, d32);
      uCond(cb, CondAlways);
      *isEnd = True;
      if (dis)
        VG_(printf)("jmp 0x%x\n", d32);
      break;

   case 0x70:
   case 0x71:
   case 0x72: /* JBb/JNAEb (jump below) */
   case 0x73: /* JNBb/JAEb (jump not below) */
   case 0x74: /* JZb/JEb (jump zero) */
   case 0x75: /* JNZb/JNEb (jump not zero) */
   case 0x76: /* JBEb/JNAb (jump below or equal) */
   case 0x77: /* JNBEb/JAb (jump not below or equal) */
   case 0x78: /* JSb (jump negative) */
   case 0x79: /* JSb (jump not negative) */
   case 0x7A: /* JP (jump parity even) */
   case 0x7B: /* JNP/JPO (jump parity odd) */
   case 0x7C: /* JLb/JNGEb (jump less) */
   case 0x7D: /* JGEb/JNLb (jump greater or equal) */
   case 0x7E: /* JLEb/JNGb (jump less or equal) */
   case 0x7F: /* JGb/JNLEb (jump greater) */
      d32 = (eip+1) + getSDisp8(eip); eip++;
      uInstr1(cb, JMP, 0, Literal, 0);
      uLiteral(cb, d32);
      uCond(cb, (Condcode)(opc - 0x70));
      uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
      /* It's actually acceptable not to end this basic block at a
         control transfer, reducing the number of jumps through
         vg_dispatch, at the expense of possibly translating the insns
         following this jump twice.  This does give faster code, but
         on the whole I don't think the effort is worth it. */
      uInstr1(cb, JMP, 0, Literal, 0);
      uLiteral(cb, eip);
      uCond(cb, CondAlways);
      *isEnd = True;
      /* The above 3 lines would be removed if the bb was not to end
         here. */
      if (dis)
         VG_(printf)("j%s-8 0x%x\n", VG_(name_UCondcode)(opc - 0x70), d32);
      break;

   case 0xE3: /* JECXZ or perhaps JCXZ, depending on OSO ?  Intel
                 manual says it depends on address size override,
                 which doesn't sound right to me. */
      d32 = (eip+1) + getSDisp8(eip); eip++;
      t1 = newTemp(cb);
      uInstr2(cb, GET,  4,  ArchReg, R_ECX, TempReg, t1);
      uInstr2(cb, JIFZ, 4,  TempReg, t1,    Literal, 0);
      uLiteral(cb, d32);
      if (dis)
         VG_(printf)("j%sz 0x%x\n", nameIReg(sz, R_ECX), d32);
      break;

   case 0xE2: /* LOOP disp8 */
      /* Again, the docs say this uses ECX/CX as a count depending on
         the address size override, not the operand one.  Since we
         don't handle address size overrides, I guess that means
         ECX. */
      d32 = (eip+1) + getSDisp8(eip); eip++;
      t1 = newTemp(cb);
      uInstr2(cb, GET,  4, ArchReg, R_ECX, TempReg, t1);
      uInstr1(cb, DEC,  4, TempReg, t1);
      uInstr2(cb, PUT,  4, TempReg, t1,    ArchReg, R_ECX);
      uInstr2(cb, JIFZ, 4, TempReg, t1,    Literal, 0);
      uLiteral(cb, eip);
      uInstr1(cb, JMP,  0, Literal, 0);
      uLiteral(cb, d32);
      uCond(cb, CondAlways);
      *isEnd = True;
      if (dis)
         VG_(printf)("loop 0x%x\n", d32);
      break;

   /* ------------------------ IMUL ----------------------- */

   case 0x69: /* IMUL Iv, Ev, Gv */
      eip = dis_imul_I_E_G ( cb, sorb, sz, eip, sz );
      break;
   case 0x6B: /* IMUL Ib, Ev, Gv */
      eip = dis_imul_I_E_G ( cb, sorb, sz, eip, 1 );
      break;

   /* ------------------------ MOV ------------------------ */

   case 0x88: /* MOV Gb,Eb */
      eip = dis_mov_G_E(cb, sorb, 1, eip);
      break;

   case 0x89: /* MOV Gv,Ev */
      eip = dis_mov_G_E(cb, sorb, sz, eip);
      break;

   case 0x8A: /* MOV Eb,Gb */
      eip = dis_mov_E_G(cb, sorb, 1, eip);
      break;
 
   case 0x8B: /* MOV Ev,Gv */
      eip = dis_mov_E_G(cb, sorb, sz, eip);
      break;
 
   case 0x8D: /* LEA M,Gv */
      modrm = getUChar(eip);
      if (epartIsReg(modrm)) 
         VG_(core_panic)("LEA M,Gv: modRM refers to register");
      /* NOTE!  this is the one place where a segment override prefix
         has no effect on the address calculation.  Therefore we pass
         zero instead of sorb here. */
      pair = disAMode ( cb, /*sorb*/ 0, eip, dis?dis_buf:NULL );
      eip  += HI8(pair);
      t1   = LOW24(pair);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, gregOfRM(modrm));
      if (dis)
         VG_(printf)("lea%c %s, %s\n", nameISize(sz), dis_buf, 
                                       nameIReg(sz,gregOfRM(modrm)));
      break;

   case 0x8C: /* MOV Sw,Ew -- MOV from a SEGMENT REGISTER */
      eip = dis_mov_Sw_Ew(cb, sorb, eip);
      break;

   case 0x8E: /* MOV Ew,Sw -- MOV to a SEGMENT REGISTER */
      eip = dis_mov_Ew_Sw(cb, sorb, eip);
      break;

   case 0xA0: /* MOV Ob,AL */
      sz = 1;
      /* Fall through ... */
   case 0xA1: /* MOV Ov,eAX */
      d32 = getUDisp32(eip); eip += 4;
      t1 = newTemp(cb); t2 = newTemp(cb);
      uInstr2(cb, MOV,   4, Literal, 0,   TempReg, t2);
      uLiteral(cb, d32);
      handleSegOverride(cb, sorb, t2);
      uInstr2(cb, LOAD, sz, TempReg, t2,  TempReg, t1);
      uInstr2(cb, PUT,  sz, TempReg, t1,  ArchReg, R_EAX);
      if (dis) VG_(printf)("mov%c %s0x%x, %s\n", nameISize(sz), 
                           sorbTxt(sorb),
                           d32, nameIReg(sz,R_EAX));
      break;

   case 0xA2: /* MOV AL,Ob */
      sz = 1;
      /* Fall through ... */
   case 0xA3: /* MOV eAX,Ov */
      d32 = getUDisp32(eip); eip += 4;
      t1 = newTemp(cb); t2 = newTemp(cb);
      uInstr2(cb, GET,   sz, ArchReg, R_EAX, TempReg, t1);
      uInstr2(cb, MOV,    4, Literal, 0,     TempReg, t2);
      uLiteral(cb, d32);
      handleSegOverride(cb, sorb, t2);
      uInstr2(cb, STORE, sz, TempReg, t1,    TempReg, t2);
      if (dis) VG_(printf)("mov%c %s, %s0x%x\n", nameISize(sz), 
                           nameIReg(sz,R_EAX),
                           sorbTxt(sorb), d32);
      break;

   case 0xB0: /* MOV imm,AL */
   case 0xB1: /* MOV imm,CL */
   case 0xB2: /* MOV imm,DL */
   case 0xB3: /* MOV imm,BL */
   case 0xB4: /* MOV imm,AH */
   case 0xB5: /* MOV imm,CH */
   case 0xB6: /* MOV imm,DH */
   case 0xB7: /* MOV imm,BH */
      d32 = getUChar(eip); eip += 1;
      t1 = newTemp(cb);
      uInstr2(cb, MOV, 1, Literal, 0,  TempReg, t1);
      uLiteral(cb, d32);
      uInstr2(cb, PUT, 1, TempReg, t1, ArchReg, opc-0xB0);
      if (dis) VG_(printf)("movb $0x%x,%s\n", d32,
                           nameIReg(1,opc-0xB0));
      break;

   case 0xB8: /* MOV imm,eAX */
   case 0xB9: /* MOV imm,eCX */
   case 0xBA: /* MOV imm,eDX */
   case 0xBB: /* MOV imm,eBX */
   case 0xBD: /* MOV imm,eBP */
   case 0xBE: /* MOV imm,eSI */
   case 0xBF: /* MOV imm,eDI */
      d32 = getUDisp(sz,eip); eip += sz;
      t1 = newTemp(cb);
      uInstr2(cb, MOV, sz, Literal, 0,  TempReg, t1);
      uLiteral(cb, d32);
      uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, opc-0xB8);
      if (dis) VG_(printf)("mov%c $0x%x,%s\n", nameISize(sz), d32,
                           nameIReg(sz,opc-0xB8));
      break;

   case 0xC6: /* MOV Ib,Eb */
      sz = 1;
      goto do_Mov_I_E;
   case 0xC7: /* MOV Iv,Ev */
      goto do_Mov_I_E;

   do_Mov_I_E:
      modrm = getUChar(eip);
      if (epartIsReg(modrm)) {
         eip++; /* mod/rm byte */
         d32 = getUDisp(sz,eip); eip += sz;
         t1 = newTemp(cb);
         uInstr2(cb, MOV, sz, Literal, 0,  TempReg, t1);
	 uLiteral(cb, d32);
         uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, eregOfRM(modrm));
         if (dis) VG_(printf)("mov%c $0x%x, %s\n", nameISize(sz), d32, 
                              nameIReg(sz,eregOfRM(modrm)));
      } else {
         pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
         eip += HI8(pair);
         d32 = getUDisp(sz,eip); eip += sz;
         t1 = newTemp(cb);
         t2 = LOW24(pair);
         uInstr2(cb, MOV, sz, Literal, 0, TempReg, t1);
	 uLiteral(cb, d32);
         uInstr2(cb, STORE, sz, TempReg, t1, TempReg, t2);
         if (dis) VG_(printf)("mov%c $0x%x, %s\n", nameISize(sz), d32, dis_buf);
      }
      break;

   /* ------------------------ opl imm, A ----------------- */

   case 0x04: /* ADD Ib, AL */
      eip = dis_op_imm_A(cb, 1, ADD, True, eip, "add" );
      break;
   case 0x05: /* ADD Iv, eAX */
      eip = dis_op_imm_A(cb, sz, ADD, True, eip, "add" );
      break;

   case 0x0C: /* OR Ib, AL */
      eip = dis_op_imm_A(cb, 1, OR, True, eip, "or" );
      break;
   case 0x0D: /* OR Iv, eAX */
      eip = dis_op_imm_A(cb, sz, OR, True, eip, "or" );
      break;

   case 0x14: /* ADC Ib, AL */
      eip = dis_op_imm_A(cb, 1, ADC, True, eip, "adc" );
      break;
   case 0x15: /* ADC Iv, eAX */
      eip = dis_op_imm_A(cb, sz, ADC, True, eip, "adc" );
      break;

   case 0x1C: /* SBB Ib, AL */
      eip = dis_op_imm_A(cb, 1, SBB, True, eip, "sbb" );
      break;

   case 0x24: /* AND Ib, AL */
      eip = dis_op_imm_A(cb, 1, AND, True, eip, "and" );
      break;
   case 0x25: /* AND Iv, eAX */
      eip = dis_op_imm_A(cb, sz, AND, True, eip, "and" );
      break;

   case 0x2C: /* SUB Ib, AL */
      eip = dis_op_imm_A(cb, 1, SUB, True, eip, "sub" );
      break;
   case 0x2D: /* SUB Iv, eAX */
      eip = dis_op_imm_A(cb, sz, SUB, True, eip, "sub" );
      break;

   case 0x34: /* XOR Ib, AL */
      eip = dis_op_imm_A(cb, 1, XOR, True, eip, "xor" );
      break;
   case 0x35: /* XOR Iv, eAX */
      eip = dis_op_imm_A(cb, sz, XOR, True, eip, "xor" );
      break;

   case 0x3C: /* CMP Ib, AL */
      eip = dis_op_imm_A(cb, 1, SUB, False, eip, "cmp" );
      break;
   case 0x3D: /* CMP Iv, eAX */
      eip = dis_op_imm_A(cb, sz, SUB, False, eip, "cmp" );
      break;

   case 0xA8: /* TEST Ib, AL */
      eip = dis_op_imm_A(cb, 1, AND, False, eip, "test" );
      break;
   case 0xA9: /* TEST Iv, eAX */
      eip = dis_op_imm_A(cb, sz, AND, False, eip, "test" );
      break;

   /* ------------------------ opl Ev, Gv ----------------- */

   case 0x02: /* ADD Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, ADD, True, 1, eip, "add" );
      break;
   case 0x03: /* ADD Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, ADD, True, sz, eip, "add" );
      break;

   case 0x0A: /* OR Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, OR, True, 1, eip, "or" );
      break;
   case 0x0B: /* OR Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, OR, True, sz, eip, "or" );
      break;

   case 0x12: /* ADC Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, ADC, True, 1, eip, "adc" );
      break;
   case 0x13: /* ADC Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, ADC, True, sz, eip, "adc" );
      break;

   case 0x1A: /* SBB Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, SBB, True, 1, eip, "sbb" );
      break;
   case 0x1B: /* SBB Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, SBB, True, sz, eip, "sbb" );
      break;

   case 0x22: /* AND Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, AND, True, 1, eip, "and" );
      break;
   case 0x23: /* AND Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, AND, True, sz, eip, "and" );
      break;

   case 0x2A: /* SUB Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, SUB, True, 1, eip, "sub" );
      break;
   case 0x2B: /* SUB Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, SUB, True, sz, eip, "sub" );
      break;

   case 0x32: /* XOR Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, XOR, True, 1, eip, "xor" );
      break;
   case 0x33: /* XOR Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, XOR, True, sz, eip, "xor" );
      break;

   case 0x3A: /* CMP Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, SUB, False, 1, eip, "cmp" );
      break;
   case 0x3B: /* CMP Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, SUB, False, sz, eip, "cmp" );
      break;

   case 0x84: /* TEST Eb,Gb */
      eip = dis_op2_E_G ( cb, sorb, AND, False, 1, eip, "test" );
      break;
   case 0x85: /* TEST Ev,Gv */
      eip = dis_op2_E_G ( cb, sorb, AND, False, sz, eip, "test" );
      break;

   /* ------------------------ opl Gv, Ev ----------------- */

   case 0x00: /* ADD Gb,Eb */
      eip = dis_op2_G_E ( cb, sorb, ADD, True, 1, eip, "add" );
      break;
   case 0x01: /* ADD Gv,Ev */
      eip = dis_op2_G_E ( cb, sorb, ADD, True, sz, eip, "add" );
      break;

   case 0x08: /* OR Gb,Eb */
      eip = dis_op2_G_E ( cb, sorb, OR, True, 1, eip, "or" );
      break;
   case 0x09: /* OR Gv,Ev */
      eip = dis_op2_G_E ( cb, sorb, OR, True, sz, eip, "or" );
      break;

   case 0x10: /* ADC Gb,Eb */
      eip = dis_op2_G_E ( cb, sorb, ADC, True, 1, eip, "adc" );
      break;
   case 0x11: /* ADC Gv,Ev */
      eip = dis_op2_G_E ( cb, sorb, ADC, True, sz, eip, "adc" );
      break;

   case 0x19: /* SBB Gv,Ev */
      eip = dis_op2_G_E ( cb, sorb, SBB, True, sz, eip, "sbb" );
      break;

   case 0x20: /* AND Gb,Eb */
      eip = dis_op2_G_E ( cb, sorb, AND, True, 1, eip, "and" );
      break;
   case 0x21: /* AND Gv,Ev */
      eip = dis_op2_G_E ( cb, sorb, AND, True, sz, eip, "and" );
      break;

   case 0x28: /* SUB Gb,Eb */
      eip = dis_op2_G_E ( cb, sorb, SUB, True, 1, eip, "sub" );
      break;
   case 0x29: /* SUB Gv,Ev */
      eip = dis_op2_G_E ( cb, sorb, SUB, True, sz, eip, "sub" );
      break;

   case 0x30: /* XOR Gb,Eb */
      eip = dis_op2_G_E ( cb, sorb, XOR, True, 1, eip, "xor" );
      break;
   case 0x31: /* XOR Gv,Ev */
      eip = dis_op2_G_E ( cb, sorb, XOR, True, sz, eip, "xor" );
      break;

   case 0x38: /* CMP Gb,Eb */
      eip = dis_op2_G_E ( cb, sorb, SUB, False, 1, eip, "cmp" );
      break;
   case 0x39: /* CMP Gv,Ev */
      eip = dis_op2_G_E ( cb, sorb, SUB, False, sz, eip, "cmp" );
      break;

   /* ------------------------ POP ------------------------ */

   case 0x58: /* POP eAX */
   case 0x59: /* POP eCX */
   case 0x5A: /* POP eDX */
   case 0x5B: /* POP eBX */
   case 0x5D: /* POP eBP */
   case 0x5E: /* POP eSI */
   case 0x5F: /* POP eDI */
   case 0x5C: /* POP eSP */
      t1 = newTemp(cb); t2 = newTemp(cb);
      uInstr2(cb, GET,    4, ArchReg, R_ESP,    TempReg, t2);
      uInstr2(cb, LOAD,  sz, TempReg, t2,       TempReg, t1);
      uInstr2(cb, ADD,    4, Literal, 0,        TempReg, t2);
      uLiteral(cb, sz);
      uInstr2(cb, PUT,    4, TempReg, t2,       ArchReg, R_ESP);
      uInstr2(cb, PUT,   sz, TempReg, t1,       ArchReg, opc-0x58);
      if (dis) 
         VG_(printf)("pop%c %s\n", nameISize(sz), nameIReg(sz,opc-0x58));
      break;

   case 0x9D: /* POPF */
      vg_assert(sz == 2 || sz == 4);
      t1 = newTemp(cb); t2 = newTemp(cb);
      uInstr2(cb, GET,    4, ArchReg, R_ESP,    TempReg, t2);
      uInstr2(cb, LOAD,  sz, TempReg, t2,       TempReg, t1);
      uInstr2(cb, ADD,    4, Literal, 0,        TempReg, t2);
      uLiteral(cb, sz);
      uInstr2(cb, PUT,    4, TempReg, t2,       ArchReg, R_ESP);
      uInstr1(cb, PUTF,  sz, TempReg, t1);
      /* PUTF writes all the flags we are interested in */
      uFlagsRWU(cb, FlagsEmpty, FlagsALL, FlagsEmpty);
      if (dis) 
         VG_(printf)("popf%c\n", nameISize(sz));
      break;

   case 0x61: /* POPA */
    { Int reg;
      /* Just to keep things sane, we assert for a size 4.  It's
         probably OK for size 2 as well, but I'd like to find a test
         case; ie, have the assertion fail, before committing to it.
         If it fails for you, uncomment the sz == 2 bit, try again,
         and let me know whether or not it works.  (jseward@acm.org).  */
      vg_assert(sz == 4 /* || sz == 2 */);

      /* Eight values are popped, one per register, but the value of
         %esp on the stack is ignored and instead incremented (in one
         hit at the end) for each of the values. */
      t1 = newTemp(cb); t2 = newTemp(cb); t3 = newTemp(cb);
      uInstr2(cb, GET,    4, ArchReg, R_ESP, TempReg, t2);
      uInstr2(cb, MOV,    4, TempReg, t2,    TempReg, t3);

      /* Do %edi, %esi, %ebp */
      for (reg = 7; reg >= 5; reg--) {
          uInstr2(cb, LOAD,  sz, TempReg, t2, TempReg, t1);
          uInstr2(cb, ADD,    4, Literal, 0,  TempReg, t2);
          uLiteral(cb, sz);
          uInstr2(cb, PUT,   sz, TempReg, t1, ArchReg, reg);
      }
      /* Ignore (skip) value of %esp on stack. */
      uInstr2(cb, ADD,    4, Literal, 0,  TempReg, t2);
      uLiteral(cb, sz);
      /* Do %ebx, %edx, %ecx, %eax */
      for (reg = 3; reg >= 0; reg--) {
          uInstr2(cb, LOAD,  sz, TempReg, t2, TempReg, t1);
          uInstr2(cb, ADD,    4, Literal, 0,  TempReg, t2);
          uLiteral(cb, sz);
          uInstr2(cb, PUT,   sz, TempReg, t1, ArchReg, reg);
      }
      uInstr2(cb, ADD,    4, Literal, 0,  TempReg, t3);
      uLiteral(cb, sz * 8);             /* One 'sz' per register */
      uInstr2(cb, PUT,    4, TempReg, t3, ArchReg, R_ESP);
      if (dis)
         VG_(printf)("popa%c\n", nameISize(sz));
      break;
    }

   case 0x8F: /* POPL/POPW m32 */
     { UInt pair1;
       Int  tmpa;
       UChar rm = getUChar(eip);

       /* make sure this instruction is correct POP */
       vg_assert(!epartIsReg(rm) && (gregOfRM(rm) == 0));
       /* and has correct size */
       vg_assert(sz == 4);      
       
       t1 = newTemp(cb); t3 = newTemp(cb);
       /* set t1 to ESP: t1 = ESP */
       uInstr2(cb, GET,  4, ArchReg, R_ESP,    TempReg, t1);
       /* load M[ESP] to virtual register t3: t3 = M[t1] */
       uInstr2(cb, LOAD, 4, TempReg, t1, TempReg, t3);
       /* resolve MODR/M */
       pair1 = disAMode ( cb, sorb, eip, dis?dis_buf:NULL);              
       
       tmpa = LOW24(pair1);
       /*  uInstr2(cb, LOAD, sz, TempReg, tmpa, TempReg, tmpa); */
       /* store value from stack in memory, M[m32] = t3 */       
       uInstr2(cb, STORE, 4, TempReg, t3, TempReg, tmpa);

       /* increase ESP */
       uInstr2(cb, ADD,    4, Literal, 0,        TempReg, t1);
       uLiteral(cb, sz);
       uInstr2(cb, PUT,    4, TempReg, t1,       ArchReg, R_ESP);

       if (dis) 
          VG_(printf)("popl %s\n", dis_buf);

       eip += HI8(pair1);
       break;
     }

   /* ------------------------ PUSH ----------------------- */

   case 0x50: /* PUSH eAX */
   case 0x51: /* PUSH eCX */
   case 0x52: /* PUSH eDX */
   case 0x53: /* PUSH eBX */
   case 0x55: /* PUSH eBP */
   case 0x56: /* PUSH eSI */
   case 0x57: /* PUSH eDI */
   case 0x54: /* PUSH eSP */
      /* This is the Right Way, in that the value to be pushed is
         established before %esp is changed, so that pushl %esp
         correctly pushes the old value. */
      t1 = newTemp(cb); t2 = newTemp(cb); t3 = newTemp(cb);
      uInstr2(cb, GET,   sz, ArchReg, opc-0x50, TempReg, t1);
      uInstr2(cb, GET,    4, ArchReg, R_ESP,    TempReg, t3);
      uInstr2(cb, MOV,    4, TempReg, t3,       TempReg, t2);
      uInstr2(cb, SUB,    4, Literal, 0,        TempReg, t2);
      uLiteral(cb, sz);
      uInstr2(cb, PUT,    4, TempReg, t2,       ArchReg, R_ESP);
      uInstr2(cb, STORE, sz, TempReg, t1,       TempReg, t2);
      if (dis) 
         VG_(printf)("push%c %s\n", nameISize(sz), nameIReg(sz,opc-0x50));
      break;

   case 0x68: /* PUSH Iv */
      d32 = getUDisp(sz,eip); eip += sz;
      goto do_push_I;
   case 0x6A: /* PUSH Ib, sign-extended to sz */
      d32 = getSDisp8(eip); eip += 1;
      goto do_push_I;
   do_push_I:
      t1 = newTemp(cb); t2 = newTemp(cb);
      uInstr2(cb, GET,    4, ArchReg, R_ESP, TempReg, t1);
      uInstr2(cb, SUB,    4, Literal, 0,     TempReg, t1);
      uLiteral(cb, sz);
      uInstr2(cb, PUT,    4, TempReg, t1,    ArchReg, R_ESP);
      uInstr2(cb, MOV,   sz, Literal, 0,     TempReg, t2);
      uLiteral(cb, d32);
      uInstr2(cb, STORE, sz, TempReg, t2,    TempReg, t1);
      if (dis) 
         VG_(printf)("push%c $0x%x\n", nameISize(sz), d32);
      break;

   case 0x9C: /* PUSHF */
      vg_assert(sz == 2 || sz == 4);
      t1 = newTemp(cb); t2 = newTemp(cb); t3 = newTemp(cb);
      uInstr1(cb, GETF,  sz, TempReg, t1);
      /* GETF reads all the flags we are interested in */
      uFlagsRWU(cb, FlagsALL, FlagsEmpty, FlagsEmpty);
      uInstr2(cb, GET,    4, ArchReg, R_ESP,    TempReg, t3);
      uInstr2(cb, MOV,    4, TempReg, t3,       TempReg, t2);
      uInstr2(cb, SUB,    4, Literal, 0,        TempReg, t2);
      uLiteral(cb, sz);
      uInstr2(cb, PUT,    4, TempReg, t2,       ArchReg, R_ESP);
      uInstr2(cb, STORE, sz, TempReg, t1,       TempReg, t2);
      if (dis) 
         VG_(printf)("pushf%c\n", nameISize(sz));
      break;

   case 0x60: /* PUSHA */
    { Int reg;
      /* Just to keep things sane, we assert for a size 4.  It's
         probably OK for size 2 as well, but I'd like to find a test
         case; ie, have the assertion fail, before committing to it.
         If it fails for you, uncomment the sz == 2 bit, try again,
         and let me know whether or not it works.  (jseward@acm.org).  */
      vg_assert(sz == 4 /* || sz == 2 */);

      /* This is the Right Way, in that the value to be pushed is
         established before %esp is changed, so that pusha
         correctly pushes the old %esp value.  New value of %esp is
         pushed at start. */
      t1 = newTemp(cb); t2 = newTemp(cb); t3 = newTemp(cb);
      t4 = newTemp(cb);
      uInstr2(cb, GET,    4, ArchReg, R_ESP, TempReg, t3);
      uInstr2(cb, MOV,    4, TempReg, t3,    TempReg, t2);
      uInstr2(cb, MOV,    4, TempReg, t3,    TempReg, t4);
      uInstr2(cb, SUB,    4, Literal, 0,     TempReg, t4);
      uLiteral(cb, sz * 8);             /* One 'sz' per register. */
      uInstr2(cb, PUT,    4, TempReg, t4,  ArchReg, R_ESP);
      /* Do %eax, %ecx, %edx, %ebx */
      for (reg = 0; reg <= 3; reg++) {
         uInstr2(cb, GET,   sz, ArchReg, reg, TempReg, t1);
         uInstr2(cb, SUB,    4, Literal,   0, TempReg, t2);
         uLiteral(cb, sz);
         uInstr2(cb, STORE, sz, TempReg,  t1, TempReg, t2);
      }
      /* Push old value of %esp */
      uInstr2(cb, SUB,    4, Literal,   0, TempReg, t2);
      uLiteral(cb, sz);
      uInstr2(cb, STORE, sz, TempReg,  t3, TempReg, t2);
      /* Do %ebp, %esi, %edi */
      for (reg = 5; reg <= 7; reg++) {
         uInstr2(cb, GET,   sz, ArchReg, reg, TempReg, t1);
         uInstr2(cb, SUB,    4, Literal,   0, TempReg, t2);
         uLiteral(cb, sz);
         uInstr2(cb, STORE, sz, TempReg,  t1, TempReg, t2);
      }
      if (dis)
         VG_(printf)("pusha%c\n", nameISize(sz));
      break;
    }

   /* ------------------------ SCAS et al ----------------- */

   case 0xA4: /* MOVSb, no REP prefix */
      vg_assert(sorb == 0);
      codegen_MOVS ( cb, 1 );
      if (dis) VG_(printf)("movsb\n");
      break;
   case 0xA5: /* MOVSv, no REP prefix */
      vg_assert(sorb == 0);
      codegen_MOVS ( cb, sz );
      if (dis) VG_(printf)("movs%c\n", nameISize(sz));
      break;

   case 0xA6: /* CMPSb, no REP prefix */
      vg_assert(sorb == 0);
      codegen_CMPS ( cb, 1 );
      if (dis) VG_(printf)("cmpsb\n");
      break;

   case 0xAA: /* STOSb, no REP prefix */
      vg_assert(sorb == 0);
      codegen_STOS ( cb, 1 );
      if (dis) VG_(printf)("stosb\n");
      break;
   case 0xAB: /* STOSv, no REP prefix */
      vg_assert(sorb == 0);
      codegen_STOS ( cb, sz );
      if (dis) VG_(printf)("stos%c\n", nameISize(sz));
      break;

   case 0xAC: /* LODSb, no REP prefix */
      vg_assert(sorb == 0);
      codegen_LODS ( cb, 1 );
      if (dis) VG_(printf)("lodsb\n");
      break;
   case 0xAD: /* LODSv, no REP prefix */
      vg_assert(sorb == 0);
      codegen_LODS ( cb, sz );
      if (dis) VG_(printf)("lods%c\n", nameISize(sz));
      break;

   case 0xAE: /* SCASb, no REP prefix */
      vg_assert(sorb == 0);
      codegen_SCAS ( cb, 1 );
      if (dis) VG_(printf)("scasb\n");
      break;

   case 0xFC: /* CLD */
      uInstr0(cb, CALLM_S, 0);
      uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_CLD));
      uFlagsRWU(cb, FlagsEmpty, FlagD, FlagsEmpty);
      uInstr0(cb, CALLM_E, 0);
      if (dis) VG_(printf)("cld\n");
      break;

   case 0xFD: /* STD */
      uInstr0(cb, CALLM_S, 0);
      uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_STD));
      uFlagsRWU(cb, FlagsEmpty, FlagD, FlagsEmpty);
      uInstr0(cb, CALLM_E, 0);
      if (dis) VG_(printf)("std\n");
      break;

   case 0xF8: /* CLC */
      uInstr0(cb, CALLM_S, 0);
      uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_CLC));
      uFlagsRWU(cb, FlagsEmpty, FlagC, FlagsOSZAP);
      uInstr0(cb, CALLM_E, 0);
      if (dis) VG_(printf)("clc\n");
      break;

   case 0xF9: /* STC */
      uInstr0(cb, CALLM_S, 0);
      uInstr1(cb, CALLM, 0, Lit16, VGOFF_(helper_STC));
      uFlagsRWU(cb, FlagsEmpty, FlagC, FlagsOSZCP);
      uInstr0(cb, CALLM_E, 0);
      if (dis) VG_(printf)("stc\n");
      break;

   case 0xF2: { /* REPNE prefix insn */
      Addr eip_orig = eip - 1;
      vg_assert(sorb == 0);
      abyte = getUChar(eip); eip++;
      if (abyte == 0x66) { sz = 2; abyte = getUChar(eip); eip++; }

      if (abyte == 0xAE || abyte == 0xAF) { /* REPNE SCAS<sz> */
         if (abyte == 0xAE) sz = 1;
         codegen_REPNE_SCAS ( cb, sz, eip_orig, eip );
         *isEnd = True;         
         if (dis) VG_(printf)("repne scas%c\n", nameISize(sz));
      }
      else {
         goto decode_failure;
      }
      break;
   }

   case 0xF3: { /* REPE prefix insn */
      Addr eip_orig = eip - 1;
      vg_assert(sorb == 0);
      abyte = getUChar(eip); eip++;
      if (abyte == 0x66) { sz = 2; abyte = getUChar(eip); eip++; }

      if (abyte == 0xA4 || abyte == 0xA5) { /* REPE MOV<sz> */
         if (abyte == 0xA4) sz = 1;
         codegen_REPE_MOVS ( cb, sz, eip_orig, eip );
         *isEnd = True;
         if (dis) VG_(printf)("repe mov%c\n", nameISize(sz));
      }
      else 
      if (abyte == 0xA6 || abyte == 0xA7) { /* REPE CMP<sz> */
         if (abyte == 0xA6) sz = 1;
         codegen_REPE_CMPS ( cb, sz, eip_orig, eip );
         *isEnd = True;
         if (dis) VG_(printf)("repe cmps%c\n", nameISize(sz));
      } 
      else
      if (abyte == 0xAA || abyte == 0xAB) { /* REPE STOS<sz> */
         if (abyte == 0xAA) sz = 1;
         codegen_REPE_STOS ( cb, sz, eip_orig, eip );
         *isEnd = True;
         if (dis) VG_(printf)("repe stos%c\n", nameISize(sz));
      }
      else
      if (abyte == 0x90) { /* REPE NOP (PAUSE) */
         if (dis) VG_(printf)("repe nop (P4 pause)\n");
         /* do nothing; apparently a hint to the P4 re spin-wait loop */
      } else {
         goto decode_failure;
      }
      break;
   }

   /* ------------------------ XCHG ----------------------- */

   case 0x86: /* XCHG Gb,Eb */
      sz = 1;
      /* Fall through ... */
   case 0x87: /* XCHG Gv,Ev */
      modrm = getUChar(eip);
      t1 = newTemp(cb); t2 = newTemp(cb);
      if (epartIsReg(modrm)) {
         uInstr2(cb, GET, sz, ArchReg, eregOfRM(modrm), TempReg, t1);
         uInstr2(cb, GET, sz, ArchReg, gregOfRM(modrm), TempReg, t2);
         uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, gregOfRM(modrm));
         uInstr2(cb, PUT, sz, TempReg, t2, ArchReg, eregOfRM(modrm));
         eip++;
         if (dis)
            VG_(printf)("xchg%c %s, %s\n", nameISize(sz), 
                        nameIReg(sz,gregOfRM(modrm)), 
                        nameIReg(sz,eregOfRM(modrm)));
      } else {
         pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL);
         t3   = LOW24(pair);
         uInstr2(cb, LOAD, sz, TempReg, t3, TempReg, t1);
         uInstr2(cb, GET, sz, ArchReg, gregOfRM(modrm), TempReg, t2);
         uInstr2(cb, STORE, sz, TempReg, t2, TempReg, t3);
         uInstr2(cb, PUT, sz, TempReg, t1, ArchReg, gregOfRM(modrm));
         eip += HI8(pair);
         if (dis)
            VG_(printf)("xchg%c %s, %s\n", nameISize(sz), 
                        nameIReg(sz,gregOfRM(modrm)), 
                        dis_buf);
      }
      break;

   case 0x90: /* XCHG eAX,eAX */
      if (dis) VG_(printf)("nop\n");
      break;
   case 0x91: /* XCHG eAX,eCX */
   case 0x92: /* XCHG eAX,eDX */
   case 0x93: /* XCHG eAX,eBX */
   case 0x96: /* XCHG eAX,eSI */
   case 0x97: /* XCHG eAX,eDI */
      codegen_xchg_eAX_Reg ( cb, sz, opc - 0x90 );
      break;

   /* ------------------------ XLAT ----------------------- */

   case 0xD7: /* XLAT */
      t1 = newTemp(cb); t2 = newTemp(cb);
      uInstr2(cb, GET, sz, ArchReg, R_EBX, TempReg, t1); /* get eBX */
      handleSegOverride( cb, sorb, t1 );               /* make t1 DS:eBX */
      uInstr2(cb, GET, 1, ArchReg, R_AL, TempReg, t2); /* get AL */
      /* Widen %AL to 32 bits, so it's all defined when we add it. */
      uInstr1(cb, WIDEN, 4, TempReg, t2);
      LAST_UINSTR(cb).extra4b = 1;
      LAST_UINSTR(cb).signed_widen = False;
      uInstr2(cb, ADD, sz, TempReg, t2, TempReg, t1);  /* add AL to eBX */
      uInstr2(cb, LOAD, 1, TempReg, t1,  TempReg, t2); /* get byte at t1 into t2 */
      uInstr2(cb, PUT, 1, TempReg, t2, ArchReg, R_AL); /* put byte into AL */

      if (dis)
         VG_(printf)("xlat%c [ebx]\n", nameISize(sz));
      break;

   /* ------------------------ (Grp1 extensions) ---------- */

   case 0x80: /* Grp1 Ib,Eb */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      sz    = 1;
      d_sz  = 1;
      d32   = getSDisp8(eip + am_sz);
      eip   = dis_Grp1 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, d32 );
      break;

   case 0x81: /* Grp1 Iv,Ev */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      d_sz  = sz;
      d32   = getUDisp(d_sz, eip + am_sz);
      eip   = dis_Grp1 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, d32 );
      break;

   case 0x83: /* Grp1 Ib,Ev */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      d_sz  = 1;
      d32   = getSDisp8(eip + am_sz);
      eip   = dis_Grp1 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, d32 );
      break;

   /* ------------------------ (Grp2 extensions) ---------- */

   case 0xC0: /* Grp2 Ib,Eb */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      d_sz  = 1;
      d32   = getSDisp8(eip + am_sz);
      sz    = 1;
      eip   = dis_Grp2 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, Literal, d32 );
      break;

   case 0xC1: /* Grp2 Ib,Ev */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      d_sz  = 1;
      d32   = getSDisp8(eip + am_sz);
      eip   = dis_Grp2 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, Literal, d32 );
      break;

   case 0xD0: /* Grp2 1,Eb */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      d_sz  = 0;
      d32   = 1;
      sz    = 1;
      eip   = dis_Grp2 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, Literal, d32 );
      break;

   case 0xD1: /* Grp2 1,Ev */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      d_sz  = 0;
      d32   = 1;
      eip   = dis_Grp2 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, Literal, d32 );
      break;

   case 0xD2: /* Grp2 CL,Eb */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      d_sz  = 0;
      sz    = 1;
      eip   = dis_Grp2 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, ArchReg, R_ECX );
      break;

   case 0xD3: /* Grp2 CL,Ev */
      modrm = getUChar(eip);
      am_sz = lengthAMode(eip);
      d_sz  = 0;
      eip   = dis_Grp2 ( cb, sorb, eip, modrm, am_sz, d_sz, sz, ArchReg, R_ECX );
      break;

   /* ------------------------ (Grp3 extensions) ---------- */

   case 0xF6: /* Grp3 Eb */
      eip = dis_Grp3 ( cb, sorb, 1, eip );
      break;
   case 0xF7: /* Grp3 Ev */
      eip = dis_Grp3 ( cb, sorb, sz, eip );
      break;

   /* ------------------------ (Grp4 extensions) ---------- */

   case 0xFE: /* Grp4 Eb */
      eip = dis_Grp4 ( cb, sorb, eip );
      break;

   /* ------------------------ (Grp5 extensions) ---------- */

   case 0xFF: /* Grp5 Ev */
      eip = dis_Grp5 ( cb, sorb, sz, eip, isEnd );
      break;

   /* ------------------------ Escapes to 2-byte opcodes -- */

   case 0x0F: {
      opc = getUChar(eip); eip++;
      switch (opc) {

      /* =-=-=-=-=-=-=-=-=- Grp8 =-=-=-=-=-=-=-=-=-=-=-= */

      case 0xBA: /* Grp8 Ib,Ev */
         modrm = getUChar(eip);
         am_sz = lengthAMode(eip);
         d32   = getSDisp8(eip + am_sz);
         eip = dis_Grp8_BT ( cb, sorb, eip, modrm, am_sz, sz, d32 );
         break;

      /* =-=-=-=-=-=-=-=-=- BSF/BSR -=-=-=-=-=-=-=-=-=-= */

      case 0xBC: /* BSF Gv,Ev */
         eip = dis_bs_E_G ( cb, sorb, sz, eip, True );
         break;
      case 0xBD: /* BSR Gv,Ev */
         eip = dis_bs_E_G ( cb, sorb, sz, eip, False );
         break;

      /* =-=-=-=-=-=-=-=-=- BSWAP -=-=-=-=-=-=-=-=-=-=-= */

      case 0xC8: /* BSWAP %eax */
      case 0xC9:
      case 0xCA:
      case 0xCB:
      case 0xCC:
      case 0xCD:
      case 0xCE:
      case 0xCF: /* BSWAP %edi */
         /* AFAICS from the Intel docs, this only exists at size 4. */
         vg_assert(sz == 4);
         t1 = newTemp(cb);
         uInstr2(cb, GET,   4, ArchReg, opc-0xC8, TempReg, t1);
	 uInstr1(cb, BSWAP, 4, TempReg, t1);
         uInstr2(cb, PUT,   4, TempReg, t1, ArchReg, opc-0xC8);
         if (dis) VG_(printf)("bswapl %s\n", nameIReg(4, opc-0xC8));
         break;

      /* =-=-=-=-=-=-=-=-=- BT/BTS/BTR/BTC =-=-=-=-=-=-= */

      case 0xA3: /* BT Gv,Ev */
         eip = dis_bt_G_E ( cb, sorb, sz, eip, BtOpNone );
         break;
      case 0xB3: /* BTR Gv,Ev */
         eip = dis_bt_G_E ( cb, sorb, sz, eip, BtOpReset );
         break;
      case 0xAB: /* BTS Gv,Ev */
         eip = dis_bt_G_E ( cb, sorb, sz, eip, BtOpSet );
         break;
      case 0xBB: /* BTC Gv,Ev */
         eip = dis_bt_G_E ( cb, sorb, sz, eip, BtOpComp );
         break;

      /* =-=-=-=-=-=-=-=-=- CMOV =-=-=-=-=-=-=-=-=-=-=-= */

      case 0x40:
      case 0x41:
      case 0x42: /* CMOVBb/CMOVNAEb (cmov below) */
      case 0x43: /* CMOVNBb/CMOVAEb (cmov not below) */
      case 0x44: /* CMOVZb/CMOVEb (cmov zero) */
      case 0x45: /* CMOVNZb/CMOVNEb (cmov not zero) */
      case 0x46: /* CMOVBEb/CMOVNAb (cmov below or equal) */
      case 0x47: /* CMOVNBEb/CMOVAb (cmov not below or equal) */
      case 0x48: /* CMOVSb (cmov negative) */
      case 0x49: /* CMOVSb (cmov not negative) */
      case 0x4A: /* CMOVP (cmov parity even) */
      case 0x4B: /* CMOVNP (cmov parity odd) */
      case 0x4C: /* CMOVLb/CMOVNGEb (cmov less) */
      case 0x4D: /* CMOVGEb/CMOVNLb (cmov greater or equal) */
      case 0x4E: /* CMOVLEb/CMOVNGb (cmov less or equal) */
      case 0x4F: /* CMOVGb/CMOVNLEb (cmov greater) */
         eip = dis_cmov_E_G(cb, sorb, sz, (Condcode)(opc - 0x40), eip);
         break;

      /* =-=-=-=-=-=-=-=-=- CMPXCHG -=-=-=-=-=-=-=-=-=-= */

      case 0xB1: /* CMPXCHG Gv,Ev */
         eip = dis_cmpxchg_G_E ( cb, sorb, sz, eip );
         break;

      /* =-=-=-=-=-=-=-=-=- CPUID -=-=-=-=-=-=-=-=-=-=-= */

      case 0xA2: /* CPUID */
         t1 = newTemp(cb);
         t2 = newTemp(cb);
         t3 = newTemp(cb);
         t4 = newTemp(cb);
         uInstr0(cb, CALLM_S, 0);

         uInstr2(cb, GET,   4, ArchReg, R_EAX, TempReg, t1);
         uInstr1(cb, PUSH,  4, TempReg, t1);

         uInstr2(cb, MOV,   4, Literal, 0, TempReg, t2);
         uLiteral(cb, 0);
         uInstr1(cb, PUSH,  4, TempReg, t2);

         uInstr2(cb, MOV,   4, Literal, 0, TempReg, t3);
         uLiteral(cb, 0);
         uInstr1(cb, PUSH,  4, TempReg, t3);

         uInstr2(cb, MOV,   4, Literal, 0, TempReg, t4);
         uLiteral(cb, 0);
         uInstr1(cb, PUSH,  4, TempReg, t4);

         uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_CPUID));
         uFlagsRWU(cb, FlagsEmpty, FlagsEmpty, FlagsEmpty);

         uInstr1(cb, POP,   4, TempReg, t4);
         uInstr2(cb, PUT,   4, TempReg, t4, ArchReg, R_EDX);

         uInstr1(cb, POP,   4, TempReg, t3);
         uInstr2(cb, PUT,   4, TempReg, t3, ArchReg, R_ECX);

         uInstr1(cb, POP,   4, TempReg, t2);
         uInstr2(cb, PUT,   4, TempReg, t2, ArchReg, R_EBX);

         uInstr1(cb, POP,   4, TempReg, t1);
         uInstr2(cb, PUT,   4, TempReg, t1, ArchReg, R_EAX);

         uInstr0(cb, CALLM_E, 0);
         if (dis) VG_(printf)("cpuid\n");
         break;

      /* =-=-=-=-=-=-=-=-=- MOVZX, MOVSX =-=-=-=-=-=-=-= */

      case 0xB6: /* MOVZXb Eb,Gv */
         eip = dis_movx_E_G ( cb, sorb, eip, 1, 4, False );
         break;
      case 0xB7: /* MOVZXw Ew,Gv */
         eip = dis_movx_E_G ( cb, sorb, eip, 2, 4, False );
         break;

      case 0xBE: /* MOVSXb Eb,Gv */
         eip = dis_movx_E_G ( cb, sorb, eip, 1, 4, True );
         break;
      case 0xBF: /* MOVSXw Ew,Gv */
         eip = dis_movx_E_G ( cb, sorb, eip, 2, 4, True );
         break;

      /* =-=-=-=-=-=-=-=-=- MUL/IMUL =-=-=-=-=-=-=-=-=-= */

      case 0xAF: /* IMUL Ev, Gv */
         eip = dis_mul_E_G ( cb, sorb, sz, eip, True );
         break;

      /* =-=-=-=-=-=-=-=-=- Jcond d32 -=-=-=-=-=-=-=-=-= */
      case 0x80:
      case 0x81:
      case 0x82: /* JBb/JNAEb (jump below) */
      case 0x83: /* JNBb/JAEb (jump not below) */
      case 0x84: /* JZb/JEb (jump zero) */
      case 0x85: /* JNZb/JNEb (jump not zero) */
      case 0x86: /* JBEb/JNAb (jump below or equal) */
      case 0x87: /* JNBEb/JAb (jump not below or equal) */
      case 0x88: /* JSb (jump negative) */
      case 0x89: /* JSb (jump not negative) */
      case 0x8A: /* JP (jump parity even) */
      case 0x8B: /* JNP/JPO (jump parity odd) */
      case 0x8C: /* JLb/JNGEb (jump less) */
      case 0x8D: /* JGEb/JNLb (jump greater or equal) */
      case 0x8E: /* JLEb/JNGb (jump less or equal) */
      case 0x8F: /* JGb/JNLEb (jump greater) */
         d32 = (eip+4) + getUDisp32(eip); eip += 4;
         uInstr1(cb, JMP, 0, Literal, 0);
	 uLiteral(cb, d32);
         uCond(cb, (Condcode)(opc - 0x80));
         uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
         uInstr1(cb, JMP, 0, Literal, 0);
	 uLiteral(cb, eip);
         uCond(cb, CondAlways);
         *isEnd = True;
         if (dis)
            VG_(printf)("j%s-32 0x%x\n", 
                        VG_(name_UCondcode)(opc - 0x80), d32);
         break;

      /* =-=-=-=-=-=-=-=-=- RDTSC -=-=-=-=-=-=-=-=-=-=-= */

      case 0x31: /* RDTSC */
         t1 = newTemp(cb);
         t2 = newTemp(cb);
         t3 = newTemp(cb);
         uInstr0(cb, CALLM_S, 0);
         uInstr2(cb, MOV,   4, Literal, 0, TempReg, t1);
         uLiteral(cb, 0);
         uInstr1(cb, PUSH,  4, TempReg, t1);
         uInstr2(cb, MOV,   4, Literal, 0, TempReg, t2);
         uLiteral(cb, 0);
         uInstr1(cb, PUSH,  4, TempReg, t2);
         uInstr1(cb, CALLM, 0, Lit16,   VGOFF_(helper_RDTSC));
         uFlagsRWU(cb, FlagsEmpty, FlagsEmpty, FlagsEmpty);
         uInstr1(cb, POP,   4, TempReg, t3);
         uInstr2(cb, PUT,   4, TempReg, t3, ArchReg, R_EDX);
         uInstr1(cb, POP,   4, TempReg, t3);
         uInstr2(cb, PUT,   4, TempReg, t3, ArchReg, R_EAX);
         uInstr0(cb, CALLM_E, 0);
         if (dis) VG_(printf)("rdtsc\n");
         break;

      /* =-=-=-=-=-=-=-=-=- SETcc Eb =-=-=-=-=-=-=-=-=-= */
      case 0x90:
      case 0x91:
      case 0x92: /* set-Bb/set-NAEb (jump below) */
      case 0x93: /* set-NBb/set-AEb (jump not below) */
      case 0x94: /* set-Zb/set-Eb (jump zero) */
      case 0x95: /* set-NZb/set-NEb (jump not zero) */
      case 0x96: /* set-BEb/set-NAb (jump below or equal) */
      case 0x97: /* set-NBEb/set-Ab (jump not below or equal) */
      case 0x98: /* set-Sb (jump negative) */
      case 0x99: /* set-Sb (jump not negative) */
      case 0x9A: /* set-P (jump parity even) */
      case 0x9B: /* set-NP (jump parity odd) */
      case 0x9C: /* set-Lb/set-NGEb (jump less) */
      case 0x9D: /* set-GEb/set-NLb (jump greater or equal) */
      case 0x9E: /* set-LEb/set-NGb (jump less or equal) */
      case 0x9F: /* set-Gb/set-NLEb (jump greater) */
         modrm = getUChar(eip);
         t1 = newTemp(cb);
         if (epartIsReg(modrm)) {
            eip++;
            uInstr1(cb, CC2VAL, 1, TempReg, t1);
            uCond(cb, (Condcode)(opc-0x90));
            uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
            uInstr2(cb, PUT, 1, TempReg, t1, ArchReg, eregOfRM(modrm));
            if (dis) VG_(printf)("set%s %s\n", 
                                 VG_(name_UCondcode)(opc-0x90), 
                                 nameIReg(1,eregOfRM(modrm)));
         } else {
            pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
            t2 = LOW24(pair);
            eip += HI8(pair);
            uInstr1(cb, CC2VAL, 1, TempReg, t1);
            uCond(cb, (Condcode)(opc-0x90));
            uFlagsRWU(cb, FlagsOSZACP, FlagsEmpty, FlagsEmpty);
            uInstr2(cb, STORE, 1, TempReg, t1, TempReg, t2);
            if (dis) VG_(printf)("set%s %s\n", 
                                 VG_(name_UCondcode)(opc-0x90), 
                                 dis_buf);
         }
         break;

      /* =-=-=-=-=-=-=-=-=- SHLD/SHRD -=-=-=-=-=-=-=-=-= */

      case 0xA4: /* SHLDv imm8,Gv,Ev */
         modrm = getUChar(eip);
         eip = dis_SHLRD_Gv_Ev ( 
                  cb, sorb, eip, modrm, sz, 
                  Literal, getUChar(eip + lengthAMode(eip)),
                  True );
         break;
      case 0xA5: /* SHLDv %cl,Gv,Ev */
         modrm = getUChar(eip);
         eip = dis_SHLRD_Gv_Ev ( 
                  cb, sorb, eip, modrm, sz, ArchReg, R_CL, True );
         break;

      case 0xAC: /* SHRDv imm8,Gv,Ev */
         modrm = getUChar(eip);
         eip = dis_SHLRD_Gv_Ev ( 
                  cb, sorb, eip, modrm, sz, 
                  Literal, getUChar(eip + lengthAMode(eip)),
                  False );
         break;
      case 0xAD: /* SHRDv %cl,Gv,Ev */
         modrm = getUChar(eip);
         eip = dis_SHLRD_Gv_Ev ( 
                  cb, sorb, eip, modrm, sz, ArchReg, R_CL, False );
         break;

      /* =-=-=-=-=-=-=-=-=- CMPXCHG -=-=-=-=-=-=-=-=-=-= */

      case 0xC1: /* XADD Gv,Ev */
         eip = dis_xadd_G_E ( cb, sorb, sz, eip );
         break;

      /* =-=-=-=-=-=-=-=-=- MMXery =-=-=-=-=-=-=-=-=-=-= */

      case 0x18: /* PREFETCHT0/PREFETCHT1/PREFETCHT2/PREFETCHNTA */
         vg_assert(sz == 4);
         modrm = getUChar(eip);
         if (epartIsReg(modrm)) {
            goto decode_failure;
         }
         if (gregOfRM(modrm) > 3) {
            goto decode_failure;
         }
         eip += lengthAMode(eip);
         if (dis) {
            UChar* hintstr;
            switch (gregOfRM(modrm)) {
               case 0: hintstr = "nta"; break;
               case 1: hintstr = "t0"; break;
               case 2: hintstr = "t1"; break;
               case 3: hintstr = "t2"; break;
               default: goto decode_failure;
            }
            VG_(printf)("prefetch%s ...\n", hintstr);
         }
         break;

      case 0x71: case 0x72: case 0x73: 
         /* (sz==4): PSLL/PSRA/PSRL mmxreg by imm8 */
         /* (sz==2): PSLL/PSRA/PSRL xmmreg by imm8 */
         {
         UChar byte1, byte2, byte3, subopc, mmreg;
         vg_assert(sz == 4 || sz == 2);
         byte1 = opc;                   /* 0x71/72/73 */
         byte2 = getUChar(eip); eip++;  /* amode / sub-opcode */
         byte3 = getUChar(eip); eip++;  /* imm8 */
         mmreg = byte2 & 7;
         subopc = (byte2 >> 3) & 7;
         if (subopc == 2 || subopc == 6 || subopc == 4) {  
            /* 2 == 010 == SRL, 6 == 110 == SLL, 4 == 100 == SRA */
            /* ok */
         } else
         if (sz == 2 && opc == 0x73 && (subopc == 7 || subopc == 3)) {
            /* 3 == PSRLDQ, 7 == PSLLDQ */
            /* This is allowable in SSE.  Because sz==2 we fall thru to
               SSE5 below. */
         } else {
            eip -= (sz==2 ? 3 : 2);
            goto decode_failure;
         }
         if (sz == 4) {
            /* The leading 0x0F is implied for MMX*, so we don't
	       include it. */
            uInstr2(cb, MMX3, 0, 
                        Lit16, (((UShort)byte1) << 8) | ((UShort)byte2),
                        Lit16, ((UShort)byte3) );
            if (dis)
               VG_(printf)("ps%s%s $%d, %s\n",
                           (subopc == 2 ? "rl" 
                            : subopc == 6 ? "ll" 
                            : subopc == 4 ? "ra"
                            : "??"),
                           nameMMXGran(opc & 3),
                           (Int)byte3,
                           nameMMXReg(mmreg) );
	 } else {
            /* Whereas we have to include it for SSE. */
            uInstr3(cb, SSE5, 0, 
                        Lit16, (((UShort)0x66) << 8) | ((UShort)0x0F),
                        Lit16, (((UShort)byte1) << 8) | ((UShort)byte2),
                        Lit16, ((UShort)byte3) );
            if (dis)
               VG_(printf)("ps%s%s $%d, %s\n",
                           (subopc == 2 ? "rl" 
                            : subopc == 6 ? "ll" 
                            : subopc == 4 ? "ra"
                            : subopc == 3 ? "(PSRLDQ)"
                            : subopc == 7 ? "(PSLLDQ)"
                            : "??"),
                           nameMMXGran(opc & 3),
                           (Int)byte3,
                           nameXMMReg(mmreg) );
	 }
	 }
         break;

      case 0x77: /* EMMS */
         vg_assert(sz == 4);
         uInstr1(cb, MMX1, 0, Lit16, ((UShort)(opc)) );
         if (dis)
            VG_(printf)("emms\n");
         break;

      case 0x7E: /* MOVD (src)mmxreg, (dst)ireg-or-mem */
         vg_assert(sz == 4);
         modrm = getUChar(eip);
         if (epartIsReg(modrm)) {
            eip++;
            t1 = newTemp(cb);
            uInstr2(cb, MMX2_ERegWr, 4, 
                        Lit16, 
                        (((UShort)(opc)) << 8) | ((UShort)modrm),
                        TempReg, t1 );
            uInstr2(cb, PUT, 4, TempReg, t1, ArchReg, eregOfRM(modrm));
            if (dis)
               VG_(printf)("movd %s, %s\n", 
                           nameMMXReg(gregOfRM(modrm)),
                           nameIReg(4,eregOfRM(modrm)));
         } else {
            Int tmpa;
            pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
            tmpa = LOW24(pair);
            eip += HI8(pair);
            uInstr2(cb, MMX2_MemWr, 4, 
                        Lit16, 
                        (((UShort)(opc)) << 8) | ((UShort)modrm),
                        TempReg, tmpa);
            if (dis)
               VG_(printf)("movd %s, %s\n", 
                           nameMMXReg(gregOfRM(modrm)),
                           dis_buf);
         }
         break;

      case 0x6E: /* MOVD (src)ireg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         modrm = getUChar(eip);
         if (epartIsReg(modrm)) {
            eip++;
            t1 = newTemp(cb);
            uInstr2(cb, GET, 4, ArchReg, eregOfRM(modrm), TempReg, t1);
            uInstr2(cb, MMX2_ERegRd, 4, 
                        Lit16, 
                        (((UShort)(opc)) << 8) | ((UShort)modrm),
                        TempReg, t1 );
            if (dis)
               VG_(printf)("movd %s, %s\n", 
                           nameIReg(4,eregOfRM(modrm)),
                           nameMMXReg(gregOfRM(modrm)));
         } else {
            Int tmpa;
            pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
            tmpa = LOW24(pair);
            eip += HI8(pair);
            uInstr2(cb, MMX2_MemRd, 4, 
                        Lit16, 
                        (((UShort)(opc)) << 8) | ((UShort)modrm),
                        TempReg, tmpa);
            if (dis)
               VG_(printf)("movd %s, %s\n", 
                           dis_buf,
                           nameMMXReg(gregOfRM(modrm)));
         }
         break;

      case 0x6F: /* MOVQ (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         modrm = getUChar(eip);
         if (epartIsReg(modrm)) {
            eip++;
            uInstr1(cb, MMX2, 0, 
                        Lit16, 
                        (((UShort)(opc)) << 8) | ((UShort)modrm) );
            if (dis)
               VG_(printf)("movq %s, %s\n", 
                           nameMMXReg(eregOfRM(modrm)),
                           nameMMXReg(gregOfRM(modrm)));
         } else {
            Int tmpa;
            pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
            tmpa = LOW24(pair);
            eip += HI8(pair);
            uInstr2(cb, MMX2_MemRd, 8, 
                        Lit16, 
                        (((UShort)(opc)) << 8) | ((UShort)modrm),
                        TempReg, tmpa);
            if (dis)
               VG_(printf)("movq %s, %s\n", 
                           dis_buf,
                           nameMMXReg(gregOfRM(modrm)));
         }
         break;

      case 0x7F: /* MOVQ (src)mmxreg, (dst)mmxreg-or-mem */
         vg_assert(sz == 4);
         modrm = getUChar(eip);
         if (epartIsReg(modrm)) {
            goto decode_failure;
         } else {
            Int tmpa;
            pair = disAMode ( cb, sorb, eip, dis?dis_buf:NULL );
            tmpa = LOW24(pair);
            eip += HI8(pair);
            uInstr2(cb, MMX2_MemWr, 8, 
                        Lit16, 
                        (((UShort)(opc)) << 8) | ((UShort)modrm),
                        TempReg, tmpa);
            if (dis)
               VG_(printf)("movq %s, %s\n", 
                           nameMMXReg(gregOfRM(modrm)),
                           dis_buf);
         }
         break;

      case 0xFC: case 0xFD: case 0xFE: 
         /* PADDgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "padd", True );
         break;

      case 0xEC: case 0xED:
         /* PADDSgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "padds", True );
         break;

      case 0xDC: case 0xDD:
         /* PADDUSgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "paddus", True );
         break;

      case 0xF8: case 0xF9: case 0xFA:
         /* PSUBgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "psub", True );
         break;

      case 0xE8: case 0xE9:
         /* PSUBSgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "psubs", True );
         break;

      case 0xD8: case 0xD9:
         /* PSUBUSgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "psubus", True );
         break;

      case 0xE5: /* PMULHW (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "pmulhw", False );
         break;

      case 0xD5: /* PMULLW (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "pmullw", False );
         break;

      case 0xF5: /* PMADDWD (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "pmaddwd", False );
         break;

      case 0x74: case 0x75: case 0x76: 
         /* PCMPEQgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "pcmpeq", True );
         break;

      case 0x64: case 0x65: case 0x66: 
         /* PCMPGTgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "pcmpgt", True );
         break;

      case 0x6B: /* PACKSSDW (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "packssdw", False );
         break;

      case 0x63: /* PACKSSWB (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "packsswb", False );
         break;

      case 0x67: /* PACKUSWB (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "packuswb", False );
         break;

      case 0x68: case 0x69: case 0x6A: 
         /* PUNPCKHgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "punpckh", True );
         break;

      case 0x60: case 0x61: case 0x62:
         /* PUNPCKLgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "punpckl", True );
         break;

      case 0xDB: /* PAND (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "pand", False );
         break;

      case 0xDF: /* PANDN (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "pandn", False );
         break;

      case 0xEB: /* POR (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "por", False );
         break;

      case 0xEF: /* PXOR (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "pxor", False );
         break;

      case 0xF1: case 0xF2: case 0xF3:
         /* PSLLgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "psll", True );
         break;

      case 0xD1: case 0xD2: case 0xD3:
         /* PSRLgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "psrl", True );
         break;

      case 0xE1: case 0xE2:
         /* PSRAgg (src)mmxreg-or-mem, (dst)mmxreg */
         vg_assert(sz == 4);
         eip = dis_MMXop_regmem_to_reg ( cb, sorb, eip, opc, "psra", True );
         break;

      /* =-=-=-=-=-=-=-=-=- unimp2 =-=-=-=-=-=-=-=-=-=-= */

      default:
         goto decode_failure;
   } /* switch (opc) for the 2-byte opcodes */
   goto decode_success;
   } /* case 0x0F: of primary opcode */

   /* ------------------------ ??? ------------------------ */
  
  default:
  decode_failure:
   /* All decode failures end up here. */
   VG_(printf)("disInstr: unhandled instruction bytes: "
               "0x%x 0x%x 0x%x 0x%x\n",
               (Int)eip_start[0],
               (Int)eip_start[1],
               (Int)eip_start[2],
               (Int)eip_start[3] );

   uInstr0(cb, CALLM_S, 0);
   uInstr1(cb, CALLM,   0, Lit16, 
               VGOFF_(helper_undefined_instruction));
   uInstr0(cb, CALLM_E, 0);

   /* just because everything else insists the last instruction of
      a BB is a jmp */
   uInstr1(cb, JMP,     0, Literal, 0);
   uCond(cb, CondAlways);
   uLiteral(cb, eip);
   *isEnd = True;
   break;
   return eip;

   } /* switch (opc) for the main (primary) opcode switch. */

  decode_success:
   /* All decode successes end up here. */
   if (dis)
      VG_(printf)("\n");
   for (; first_uinstr < cb->used; first_uinstr++) {
      Bool sane = VG_(saneUInstr)(True, True, &cb->instrs[first_uinstr]);
      if (dis) 
         VG_(pp_UInstr)(first_uinstr, &cb->instrs[first_uinstr]);
      else if (!sane)
         VG_(up_UInstr)(-1, &cb->instrs[first_uinstr]);
      vg_assert(sane);
   }
   return eip;
}


/* Disassemble a complete basic block, starting at eip, and dumping
   the ucode into cb.  Returns the size, in bytes, of the basic
   block. */

Int VG_(disBB) ( UCodeBlock* cb, Addr eip0 )
{
   Addr eip   = eip0;
   Bool isEnd = False;
   Bool block_sane;
   Int delta = 0;

   if (dis) VG_(printf)("Original x86 code to UCode:\n\n");

   /* After every x86 instruction do an INCEIP, except for the final one
    * in the basic block.  For them we patch in the x86 instruction size 
    * into the `extra4b' field of the basic-block-ending JMP. 
    *
    * The INCEIPs and JMP.extra4b fields allows a skin to track x86
    * instruction sizes, important for some skins (eg. cache simulation).
    */
   if (VG_(clo_single_step)) {
      eip = disInstr ( cb, eip, &isEnd );

      /* Add a JMP to the next (single x86 instruction) BB if it doesn't
       * already end with a JMP instr. We also need to check for no UCode,
       * which occurs if the x86 instr was a nop */
      if (cb->used == 0 || LAST_UINSTR(cb).opcode != JMP) {
         uInstr1(cb, JMP, 0, Literal, 0);
         uLiteral(cb, eip);
         uCond(cb, CondAlways);
         /* Print added JMP */
         if (dis) VG_(pp_UInstr)(cb->used-1, &cb->instrs[cb->used-1]);
      }
      if (dis) VG_(printf)("\n");
      delta = eip - eip0;

   } else {
      Addr eip2;
      while (!isEnd) {
         eip2 = disInstr ( cb, eip, &isEnd );
         delta = (eip2 - eip);
         eip = eip2;
         /* Split up giant basic blocks into pieces, so the
            translations fall within 64k. */
         if (eip - eip0 > 2000 && !isEnd) {
            if (VG_(clo_verbosity) > 2)
               VG_(message)(Vg_DebugMsg,
                  "Warning: splitting giant basic block into pieces");
            uInstr1(cb, JMP, 0, Literal, 0);
            uLiteral(cb, eip);
            uCond(cb, CondAlways);
            /* Print added JMP */
            if (dis) VG_(pp_UInstr)(cb->used-1, &cb->instrs[cb->used-1]);
            isEnd = True;

         } else if (!isEnd) {
            uInstr1(cb, INCEIP, 0, Lit16, delta);
            /* Print added INCEIP */
            if (dis) VG_(pp_UInstr)(cb->used-1, &cb->instrs[cb->used-1]);
         }
         if (dis) VG_(printf)("\n");
      }
   }

   /* Patch instruction size into final JMP. */
   LAST_UINSTR(cb).extra4b = delta;

   block_sane = VG_(saneUCodeBlockCalls)(cb);
   if (!block_sane) {
      VG_(pp_UCodeBlock)(cb, "block failing sanity check");
      vg_assert(block_sane);
   }

   return eip - eip0;
}

#undef dis

/*--------------------------------------------------------------------*/
/*--- end                                            vg_to_ucode.c ---*/
/*--------------------------------------------------------------------*/
