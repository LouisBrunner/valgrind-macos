
/*--------------------------------------------------------------------*/
/*--- The JITter proper: register allocation & code improvement    ---*/
/*---                                               vg_translate.c ---*/
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

#define uInstr1   VG_(newUInstr1)
#define uInstr2   VG_(newUInstr2)
#define uInstr3   VG_(newUInstr3)
#define dis       VG_(disassemble)
#define nameIReg  VG_(nameOfIntReg)
#define nameISize VG_(nameOfIntSize)
#define uLiteral  VG_(setLiteralField)
#define newTemp   VG_(getNewTemp)
#define newShadow VG_(getNewShadow)


/*------------------------------------------------------------*/
/*--- Memory management for the translater.                ---*/
/*------------------------------------------------------------*/

#define N_JITBLOCKS    4
#define N_JITBLOCK_SZ  5000

static UChar jitstorage[N_JITBLOCKS][N_JITBLOCK_SZ];
static Bool  jitstorage_inuse[N_JITBLOCKS];
static Bool  jitstorage_initdone = False;

static __inline__ void jitstorage_initialise ( void )
{
   Int i;
   if (jitstorage_initdone) return;
   jitstorage_initdone = True;
   for (i = 0; i < N_JITBLOCKS; i++)
      jitstorage_inuse[i] = False; 
}

void* VG_(jitmalloc) ( Int nbytes )
{
   Int i;
   jitstorage_initialise();
   if (nbytes > N_JITBLOCK_SZ) {
      /* VG_(printf)("too large: %d\n", nbytes); */
      return VG_(malloc)(VG_AR_PRIVATE, nbytes);
   }
   for (i = 0; i < N_JITBLOCKS; i++) {
      if (!jitstorage_inuse[i]) {
         jitstorage_inuse[i] = True;
         /* VG_(printf)("alloc %d -> %d\n", nbytes, i ); */
         return & jitstorage[i][0];
      }
   }
   VG_(panic)("out of slots in vg_jitmalloc\n");
   return VG_(malloc)(VG_AR_PRIVATE, nbytes);
}

void VG_(jitfree) ( void* ptr )
{
   Int i;
   jitstorage_initialise();
   for (i = 0; i < N_JITBLOCKS; i++) {
      if (ptr == & jitstorage[i][0]) {
         vg_assert(jitstorage_inuse[i]);
         jitstorage_inuse[i] = False;
         return;
      }
   }
   VG_(free)(VG_AR_PRIVATE, ptr);
}

/*------------------------------------------------------------*/
/*--- Basics                                               ---*/
/*------------------------------------------------------------*/

UCodeBlock* VG_(allocCodeBlock) ( void )
{
   UCodeBlock* cb = VG_(malloc)(VG_AR_PRIVATE, sizeof(UCodeBlock));
   cb->used = cb->size = cb->nextTemp = 0;
   cb->instrs = NULL;
   return cb;
}


void VG_(freeCodeBlock) ( UCodeBlock* cb )
{
   if (cb->instrs) VG_(free)(VG_AR_PRIVATE, cb->instrs);
   VG_(free)(VG_AR_PRIVATE, cb);
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
         cb->instrs = VG_(malloc)(VG_AR_PRIVATE, 8 * sizeof(UInstr));
      } else {
         Int i;
         UInstr* instrs2 = VG_(malloc)(VG_AR_PRIVATE, 
                                       2 * sizeof(UInstr) * cb->size);
         for (i = 0; i < cb->used; i++)
            instrs2[i] = cb->instrs[i];
         cb->size *= 2;
         VG_(free)(VG_AR_PRIVATE, cb->instrs);
         cb->instrs = instrs2;
      }
   }

   vg_assert(cb->used < cb->size);
}


__inline__ 
void VG_(emptyUInstr) ( UInstr* u )
{
   u->val1 = u->val2 = u->val3 = 0;
   u->tag1 = u->tag2 = u->tag3 = NoValue;
   u->flags_r = u->flags_w = FlagsEmpty;
   u->jmpkind = JmpBoring;
   u->smc_check = u->signed_widen = False;
   u->lit32    = 0;
   u->opcode   = 0;
   u->size     = 0;
   u->cond     = 0;
   u->extra4b  = 0;
}


/* Add an instruction to a ucode block, and return the index of the
   instruction. */
__inline__
void VG_(newUInstr3) ( UCodeBlock* cb, Opcode opcode, Int sz,
                       Tag tag1, UInt val1,
                       Tag tag2, UInt val2,
                       Tag tag3, UInt val3 )
{
   UInstr* ui;
   ensureUInstr(cb);
   ui = & cb->instrs[cb->used];
   cb->used++;
   VG_(emptyUInstr)(ui);
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
void VG_(newUInstr2) ( UCodeBlock* cb, Opcode opcode, Int sz,
                       Tag tag1, UInt val1,
                       Tag tag2, UInt val2 )
{
   UInstr* ui;
   ensureUInstr(cb);
   ui = & cb->instrs[cb->used];
   cb->used++;
   VG_(emptyUInstr)(ui);
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
void VG_(newUInstr1) ( UCodeBlock* cb, Opcode opcode, Int sz,
                       Tag tag1, UInt val1 )
{
   UInstr* ui;
   ensureUInstr(cb);
   ui = & cb->instrs[cb->used];
   cb->used++;
   VG_(emptyUInstr)(ui);
   ui->val1   = val1;
   ui->opcode = opcode;
   ui->tag1   = tag1;
   ui->size   = sz;
   if (tag1 == TempReg) vg_assert(val1 != INVALID_TEMPREG);
}


__inline__
void VG_(newUInstr0) ( UCodeBlock* cb, Opcode opcode, Int sz )
{
   UInstr* ui;
   ensureUInstr(cb);
   ui = & cb->instrs[cb->used];
   cb->used++;
   VG_(emptyUInstr)(ui);
   ui->opcode = opcode;
   ui->size   = sz;
}

/* Copy an instruction into the given codeblock. */
__inline__ 
void VG_(copyUInstr) ( UCodeBlock* cb, UInstr* instr )
{
   ensureUInstr(cb);
   cb->instrs[cb->used] = *instr;
   cb->used++;
}

/* Copy auxiliary info from one uinstr to another. */
static __inline__ 
void copyAuxInfoFromTo ( UInstr* src, UInstr* dst )
{
   dst->cond          = src->cond;
   dst->extra4b       = src->extra4b;
   dst->smc_check     = src->smc_check;
   dst->signed_widen  = src->signed_widen;
   dst->jmpkind       = src->jmpkind;
   dst->flags_r       = src->flags_r;
   dst->flags_w       = src->flags_w;
}


/* Set the flag R/W sets on a uinstr. */
void VG_(setFlagRW) ( UInstr* u, FlagSet fr, FlagSet fw )
{
   /* VG_(ppUInstr)(-1,u); */
   vg_assert(fr == (fr & FlagsALL));
   vg_assert(fw == (fw & FlagsALL));
   u->flags_r = fr;
   u->flags_w = fw;
}


/* Set the lit32 field of the most recent uinsn. */
void VG_(setLiteralField) ( UCodeBlock* cb, UInt lit32 )
{
   LAST_UINSTR(cb).lit32 = lit32;
}


Bool VG_(anyFlagUse) ( UInstr* u )
{
   return (u->flags_r != FlagsEmpty 
           || u->flags_w != FlagsEmpty);
}




/* Convert a rank in the range 0 .. VG_MAX_REALREGS-1 into an Intel
   register number.  This effectively defines the order in which real
   registers are allocated.  %ebp is excluded since it is permanently
   reserved for pointing at VG_(baseBlock).  %edi is a general spare
   temp used for Left4 and various misc tag ops.

   Important!  If you change the set of allocatable registers from
   %eax, %ebx, %ecx, %edx, %esi you must change the
   save/restore sequences in various places to match!  
*/
__inline__ Int VG_(rankToRealRegNo) ( Int rank )
{
   switch (rank) {
#     if 1
      /* Probably the best allocation ordering. */
      case 0: return R_EAX;
      case 1: return R_EBX;
      case 2: return R_ECX;
      case 3: return R_EDX;
      case 4: return R_ESI;
#     else
      /* Contrary; probably the worst.  Helpful for debugging, tho. */
      case 4: return R_EAX;
      case 3: return R_EBX;
      case 2: return R_ECX;
      case 1: return R_EDX;
      case 0: return R_ESI;
#     endif
      default: VG_(panic)("rankToRealRegNo");
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

   Restrictions on insns (as generated by the disassembler) are as
   follows:

      A=ArchReg   S=SpillNo   T=TempReg   L=Literal   R=RealReg
      N=NoValue

         GETF       T       N       N
         PUTF       T       N       N

         GET        A,S     T       N
         PUT        T       A,S     N
         LOAD       T       T       N
         STORE      T       T       N
         MOV        T,L     T       N
         CMOV       T       T       N
         WIDEN      T       N       N
         JMP        T,L     N       N
         CALLM      L       N       N
         CALLM_S    N       N       N
         CALLM_E    N       N       N
         PUSH,POP   T       N       N
         CLEAR      L       N       N

         AND, OR
                    T       T       N

         ADD, ADC, XOR, SUB, SBB
                    A,L,T   T       N

         SHL, SHR, SAR, ROL, ROR, RCL, RCR
                    L,T     T       N

         NOT, NEG, INC, DEC, CC2VAL, BSWAP
                    T       N       N

         JIFZ       T       L       N

         FPU_R      L       T       N
         FPU_W      L       T       N
         FPU        L       T       N

         LEA1       T       T   (const in a seperate field)
         LEA2       T       T       T   (const & shift ditto)

         INCEIP     L       N       N
 
   and for instrumentation insns:

         LOADV      T       T       N
         STOREV     T,L     T       N
         GETV       A       T       N
         PUTV       T,L     A       N
         GETVF      T       N       N
         PUTVF      T       N       N
         WIDENV     T       N       N
         TESTV      A,T     N       N
         SETV       A,T     N       N
         TAG1       T       N       N
         TAG2       T       T       N

   Before register allocation, S operands should not appear anywhere.
   After register allocation, all T operands should have been
   converted into Rs, and S operands are allowed in GET and PUT --
   denoting spill saves/restores.  

   The size field should be 0 for insns for which it is meaningless,
   ie those which do not directly move/operate on data.
*/
Bool VG_(saneUInstr) ( Bool beforeRA, UInstr* u )
{
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
#  define N1  (u->tag1 == NoValue)
#  define N2  (u->tag2 == NoValue)
#  define N3  (u->tag3 == NoValue)
#  define SZ4 (u->size == 4)
#  define SZ2 (u->size == 2)
#  define SZ1 (u->size == 1)
#  define SZ0 (u->size == 0)
#  define CC0 (u->flags_r == FlagsEmpty && u->flags_w == FlagsEmpty)
#  define FLG_RD (u->flags_r == FlagsALL && u->flags_w == FlagsEmpty)
#  define FLG_WR (u->flags_r == FlagsEmpty && u->flags_w == FlagsALL)
#  define FLG_RD_WR_MAYBE                                         \
       ((u->flags_r == FlagsEmpty && u->flags_w == FlagsEmpty)    \
        || (u->flags_r == FlagsEmpty && u->flags_w == FlagsZCP)   \
        || (u->flags_r == FlagsZCP && u->flags_w == FlagsEmpty))
#  define CC1 (!(CC0))
#  define SZ4_IF_TR1 ((u->tag1 == TempReg || u->tag1 == RealReg) \
                      ? (u->size == 4) : True)

   Int n_lits = 0;
   if (u->tag1 == Literal) n_lits++;
   if (u->tag2 == Literal) n_lits++;
   if (u->tag3 == Literal) n_lits++;
   if (n_lits > 1) 
      return False;

   switch (u->opcode) {
      case GETF:
         return (SZ2 || SZ4) && TR1 && N2 && N3 && FLG_RD;
      case PUTF:
         return (SZ2 || SZ4) && TR1 && N2 && N3 && FLG_WR;
      case CALLM_S: case CALLM_E:
         return SZ0 && N1 && N2 && N3;
      case INCEIP:
         return SZ0 && CC0 && Ls1 && N2 && N3;
      case LEA1:
         return CC0 && TR1 && TR2 && N3 && SZ4;
      case LEA2:
         return CC0 && TR1 && TR2 && TR3 && SZ4;
      case NOP: 
         return SZ0 && CC0 && N1 && N2 && N3;
      case GET: 
         return CC0 && AS1 && TR2 && N3;
      case PUT: 
         return CC0 && TR1 && AS2 && N3;
      case LOAD: case STORE: 
         return CC0 && TR1 && TR2 && N3;
      case MOV:
         return CC0 && (TR1 || L1) && TR2 && N3 && SZ4_IF_TR1;
      case CMOV:
         return CC1 && TR1 && TR2 && N3 && SZ4;
      case JMP: 
         return (u->cond==CondAlways ? CC0 : CC1)
                && (TR1 || L1) && N2 && SZ0 && N3;
      case CLEAR:
         return CC0 && Ls1 && N2 && SZ0 && N3;
      case CALLM:
         return SZ0 && Ls1 && N2 && N3;
      case PUSH: case POP:
         return CC0 && TR1 && N2 && N3;
      case AND: case OR:
         return TR1 && TR2 && N3;
      case ADD: case ADC: case XOR: case SUB: case SBB:
         return (A1 || TR1 || L1) && TR2 && N3;
      case SHL: case SHR: case SAR: case ROL: case ROR: case RCL: case RCR:
         return       (TR1 || L1) && TR2 && N3;
      case NOT: case NEG: case INC: case DEC:
         return        TR1 && N2 && N3;
      case BSWAP:
         return TR1 && N2 && N3 && CC0 && SZ4;
      case CC2VAL: 
         return CC1 && SZ1 && TR1 && N2 && N3;
      case JIFZ:
         return CC0 && SZ4 && TR1 && L2 && N3;
      case FPU_R:  case FPU_W: 
         return CC0 && Ls1 && TR2 && N3;
      case FPU: 
         return SZ0 && FLG_RD_WR_MAYBE && Ls1 && N2 && N3;
      case LOADV:
         return CC0 && TR1 && TR2 && N3;
      case STOREV:
         return CC0 && (TR1 || L1) && TR2 && N3;
      case GETV: 
         return CC0 && A1 && TR2 && N3;
      case PUTV: 
         return CC0 && (TR1 || L1) && A2 && N3;
      case GETVF: 
         return CC0 && TR1 && N2 && N3 && SZ0;
      case PUTVF: 
         return CC0 && TR1 && N2 && N3 && SZ0;
      case WIDEN:
         return CC0 && TR1 && N2 && N3;
      case TESTV: 
         return CC0 && (A1 || TR1) && N2 && N3;
      case SETV:
         return CC0 && (A1 || TR1) && N2 && N3;
      case TAG1:
         return CC0 && TR1 && N2 && Ls3 && SZ0;
      case TAG2:
         return CC0 && TR1 && TR2 && Ls3 && SZ0;
      default: 
         VG_(panic)("vg_saneUInstr: unhandled opcode");
   }
#  undef SZ4_IF_TR1
#  undef CC0
#  undef CC1
#  undef SZ4
#  undef SZ2
#  undef SZ1
#  undef SZ0
#  undef TR1
#  undef TR2
#  undef TR3
#  undef A1
#  undef A2
#  undef AS1
#  undef AS2
#  undef AS3
#  undef L1
#  undef Ls1
#  undef L2
#  undef Ls3
#  undef N1
#  undef N2
#  undef N3
#  undef FLG_RD
#  undef FLG_WR
#  undef FLG_RD_WR_MAYBE 
}


/* Sanity checks to do with CALLMs in UCodeBlocks. */
Bool VG_(saneUCodeBlock) ( UCodeBlock* cb )
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
      case ConsNS:     return "ns";
      case CondP:      return "p";
      case CondNP:     return "np";
      case CondL:      return "l";
      case CondNL:     return "nl";
      case CondLE:     return "le";
      case CondNLE:    return "nle";
      case CondAlways: return "MP"; /* hack! */
      default: VG_(panic)("nameCondcode");
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


static void ppUOperand ( UInstr* u, Int operandNo, Int sz, Bool parens )
{
   UInt tag, val;
   switch (operandNo) {
      case 1: tag = u->tag1; val = u->val1; break;
      case 2: tag = u->tag2; val = u->val2; break;
      case 3: tag = u->tag3; val = u->val3; break;
      default: VG_(panic)("ppUOperand(1)");
   }
   if (tag == Literal) val = u->lit32;

   if (parens) VG_(printf)("(");
   switch (tag) {
      case TempReg: ppTempReg(val); break;
      case RealReg: VG_(printf)("%s",nameIReg(sz==0 ? 4 : sz,val)); break;
      case Literal: VG_(printf)("$0x%x", val); break;
      case Lit16:   VG_(printf)("$0x%x", val); break;
      case NoValue: VG_(printf)("NoValue"); break;
      case ArchReg: VG_(printf)("%S",nameIReg(sz,val)); break;
      case SpillNo: VG_(printf)("spill%d", val); break;
      default: VG_(panic)("ppUOperand(2)");
   }
   if (parens) VG_(printf)(")");
}


Char* VG_(nameUOpcode) ( Bool upper, Opcode opc )
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
   if (!upper) VG_(panic)("vg_nameUOpcode: invalid !upper");
   switch (opc) {
      case GETVF:   return "GETVF";
      case PUTVF:   return "PUTVF";
      case TAG1:    return "TAG1";
      case TAG2:    return "TAG2";
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
      case LOAD:    return "LD" ;
      case STORE:   return "ST" ;
      case MOV:     return "MOV";
      case CMOV:    return "CMOV";
      case WIDEN:   return "WIDEN";
      case JMP:     return "J"    ;
      case JIFZ:    return "JIFZ" ;
      case CALLM:   return "CALLM";
      case PUSH:    return "PUSH" ;
      case POP:     return "POP"  ;
      case CLEAR:   return "CLEAR";
      case CC2VAL:  return "CC2VAL";
      case FPU_R:   return "FPU_R";
      case FPU_W:   return "FPU_W";
      case FPU:     return "FPU"  ;
      case LOADV:   return "LOADV";
      case STOREV:  return "STOREV";
      case GETV:    return "GETV";
      case PUTV:    return "PUTV";
      case TESTV:   return "TESTV";
      case SETV:    return "SETV";
      default:      VG_(panic)("nameUOpcode: unhandled case");
   }
}


void VG_(ppUInstr) ( Int instrNo, UInstr* u )
{
   VG_(printf)("\t%4d: %s", instrNo, 
                            VG_(nameUOpcode)(True, u->opcode));
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

      case TAG1:
         VG_(printf)("\t");
         ppUOperand(u, 1, 4, False);
         VG_(printf)(" = %s ( ", VG_(nameOfTagOp)( u->val3 ));
         ppUOperand(u, 1, 4, False);
         VG_(printf)(" )");
         break;

      case TAG2:
         VG_(printf)("\t");
         ppUOperand(u, 2, 4, False);
         VG_(printf)(" = %s ( ", VG_(nameOfTagOp)( u->val3 ));
         ppUOperand(u, 1, 4, False);
         VG_(printf)(", ");
         ppUOperand(u, 2, 4, False);
         VG_(printf)(" )");
         break;

      case CALLM_S: case CALLM_E:
         break;

      case INCEIP:
         VG_(printf)("\t$%d", u->val1);
         break;

      case LEA2:
         VG_(printf)("\t%d(" , u->lit32);
         ppUOperand(u, 1, 4, False);
         VG_(printf)(",");
         ppUOperand(u, 2, 4, False);
         VG_(printf)(",%d), ", (Int)u->extra4b);
         ppUOperand(u, 3, 4, False);
         break;

      case LEA1:
         VG_(printf)("\t%d" , u->lit32);
         ppUOperand(u, 1, 4, True);
         VG_(printf)(", ");
         ppUOperand(u, 2, 4, False);
         break;

      case NOP:
         break;

      case FPU_W:
         VG_(printf)("\t0x%x:0x%x, ",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         ppUOperand(u, 2, 4, True);
         break;

      case FPU_R:
         VG_(printf)("\t");
         ppUOperand(u, 2, 4, True);
         VG_(printf)(", 0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         break;

      case FPU:
         VG_(printf)("\t0x%x:0x%x",
                     (u->val1 >> 8) & 0xFF, u->val1 & 0xFF );
         break;

      case STOREV: case LOADV:
      case GET: case PUT: case MOV: case LOAD: case STORE: case CMOV:
         VG_(printf)("\t");
         ppUOperand(u, 1, u->size, u->opcode==LOAD || u->opcode==LOADV); 
         VG_(printf)(", ");
         ppUOperand(u, 2, u->size, u->opcode==STORE || u->opcode==STOREV);
         break;

      case GETF: case PUTF:
         VG_(printf)("\t");
         ppUOperand(u, 1, u->size, False);
         break;

      case JMP: case CC2VAL:
      case PUSH: case POP: case CLEAR: case CALLM:
         if (u->opcode == JMP) {
            switch (u->jmpkind) {
               case JmpCall:      VG_(printf)("-c"); break;
               case JmpRet:       VG_(printf)("-r"); break;
               case JmpSyscall:   VG_(printf)("-sys"); break;
               case JmpClientReq: VG_(printf)("-cli"); break;
               default: break;
            }
         }
         VG_(printf)("\t");
         ppUOperand(u, 1, u->size, False);
         break;

      case JIFZ:
         VG_(printf)("\t");
         ppUOperand(u, 1, u->size, False);
         VG_(printf)(", ");
         ppUOperand(u, 2, u->size, False);
         break;

      case PUTVF: case GETVF:
         VG_(printf)("\t");
         ppUOperand(u, 1, 0, False); 
         break;

      case NOT: case NEG: case INC: case DEC: case BSWAP:
         VG_(printf)("\t");
         ppUOperand(u, 1, u->size, False); 
         break;

      case ADD: case ADC: case AND: case OR:  
      case XOR: case SUB: case SBB:   
      case SHL: case SHR: case SAR: 
      case ROL: case ROR: case RCL: case RCR:   
         VG_(printf)("\t");
         ppUOperand(u, 1, u->size, False); 
         VG_(printf)(", ");
         ppUOperand(u, 2, u->size, False);
         break;

      case GETV: case PUTV:
         VG_(printf)("\t");
         ppUOperand(u, 1, u->opcode==PUTV ? 4 : u->size, False);
         VG_(printf)(", ");
         ppUOperand(u, 2, u->opcode==GETV ? 4 : u->size, False);
         break;

      case WIDEN:
         VG_(printf)("_%c%c", VG_(toupper)(nameISize(u->extra4b)),
                              u->signed_widen?'s':'z');
         VG_(printf)("\t");
         ppUOperand(u, 1, u->size, False);
         break;

      case TESTV: case SETV:
         VG_(printf)("\t");
         ppUOperand(u, 1, u->size, False);
         break;

      default: VG_(panic)("ppUInstr: unhandled opcode");
   }

   if (u->flags_r != FlagsEmpty || u->flags_w != FlagsEmpty) {
      VG_(printf)("  (");
      if (u->flags_r != FlagsEmpty) 
         vg_ppFlagSet("-r", u->flags_r);
      if (u->flags_w != FlagsEmpty) 
         vg_ppFlagSet("-w", u->flags_w);
      VG_(printf)(")");
   }
   VG_(printf)("\n");
}


void VG_(ppUCodeBlock) ( UCodeBlock* cb, Char* title )
{
   Int i;
   VG_(printf)("\n%s\n", title);
   for (i = 0; i < cb->used; i++)
      if (0 || cb->instrs[i].opcode != NOP)
         VG_(ppUInstr) ( i, &cb->instrs[i] );
   VG_(printf)("\n");
}


/*------------------------------------------------------------*/
/*--- uinstr helpers for register allocation               ---*/
/*--- and code improvement.                                ---*/
/*------------------------------------------------------------*/

/* A structure for communicating temp uses, and for indicating
   temp->real register mappings for patchUInstr. */
typedef
   struct {
      Int   realNo;
      Int   tempNo;
      Bool  isWrite;
   }
   TempUse;


/* Get the temp use of a uinstr, parking them in an array supplied by
   the caller, which is assumed to be big enough.  Return the number
   of entries.  Insns which read _and_ write a register wind up
   mentioning it twice.  Entries are placed in the array in program
   order, so that if a reg is read-modified-written, it appears first
   as a read and then as a write.  
*/
static __inline__ 
Int getTempUsage ( UInstr* u, TempUse* arr )
{

#  define RD(ono)                                  \
      if (mycat(u->tag,ono) == TempReg)            \
         { arr[n].tempNo  = mycat(u->val,ono);     \
           arr[n].isWrite = False; n++; }
#  define WR(ono)                                  \
      if (mycat(u->tag,ono) == TempReg)            \
         { arr[n].tempNo  = mycat(u->val,ono);     \
           arr[n].isWrite = True; n++; }

   Int n = 0;
   switch (u->opcode) {
      case LEA1: RD(1); WR(2); break;
      case LEA2: RD(1); RD(2); WR(3); break;

      case NOP: case FPU: case INCEIP: case CALLM_S: case CALLM_E: break;
      case FPU_R: case FPU_W: RD(2); break;

      case GETF:  WR(1); break;
      case PUTF:  RD(1); break;

      case GET:   WR(2); break;
      case PUT:   RD(1); break;
      case LOAD:  RD(1); WR(2); break;
      case STORE: RD(1); RD(2); break;
      case MOV:   RD(1); WR(2); break;

      case JMP:   RD(1); break;
      case CLEAR: case CALLM: break;

      case PUSH: RD(1); break;
      case POP:  WR(1); break;

      case TAG2:
      case CMOV:
      case ADD: case ADC: case AND: case OR:  
      case XOR: case SUB: case SBB:   
         RD(1); RD(2); WR(2); break;

      case SHL: case SHR: case SAR: 
      case ROL: case ROR: case RCL: case RCR:
         RD(1); RD(2); WR(2); break;

      case NOT: case NEG: case INC: case DEC: case TAG1: case BSWAP:
         RD(1); WR(1); break;

      case WIDEN: RD(1); WR(1); break;

      case CC2VAL: WR(1); break;
      case JIFZ: RD(1); break;

      /* These sizes are only ever consulted when the instrumentation
         code is being added, so the following can return
         manifestly-bogus sizes. */
      case LOADV:   RD(1); WR(2); break;
      case STOREV:  RD(1); RD(2); break;
      case GETV:    WR(2); break;
      case PUTV:    RD(1); break;
      case TESTV:   RD(1); break;
      case SETV:    WR(1); break;
      case PUTVF:   RD(1); break;
      case GETVF:   WR(1); break;

      default: VG_(panic)("getTempUsage: unhandled opcode");
   }
   return n;

#  undef RD
#  undef WR
}


/* Change temp regs in u into real regs, as directed by tmap. */
static __inline__ 
void patchUInstr ( UInstr* u, TempUse* tmap, Int n_tmap )
{
   Int i;
   if (u->tag1 == TempReg) {
      for (i = 0; i < n_tmap; i++)
         if (tmap[i].tempNo == u->val1) break;
      if (i == n_tmap) VG_(panic)("patchUInstr(1)");
      u->tag1 = RealReg;
      u->val1 = tmap[i].realNo;
   }
   if (u->tag2 == TempReg) {
      for (i = 0; i < n_tmap; i++)
         if (tmap[i].tempNo == u->val2) break;
      if (i == n_tmap) VG_(panic)("patchUInstr(2)");
      u->tag2 = RealReg;
      u->val2 = tmap[i].realNo;
   }
   if (u->tag3 == TempReg) {
      for (i = 0; i < n_tmap; i++)
         if (tmap[i].tempNo == u->val3) break;
      if (i == n_tmap) VG_(panic)("patchUInstr(3)");
      u->tag3 = RealReg;
      u->val3 = tmap[i].realNo;
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
      default: VG_(panic)("containingArchRegOf");
   }
}


/* If u reads an ArchReg, return the number of the containing arch
   reg.  Otherwise return -1.  Used in redundant-PUT elimination. */
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
         return -1;

      default: 
         VG_(ppUInstr)(0,u);
         VG_(panic)("maybe_uinstrReadsArchReg: unhandled opcode");
   }
}

static __inline__
Bool uInstrMentionsTempReg ( UInstr* u, Int tempreg )
{
   Int i, k;
   TempUse tempUse[3];
   k = getTempUsage ( u, &tempUse[0] );
   for (i = 0; i < k; i++)
      if (tempUse[i].tempNo == tempreg)
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
   TempUse tempUse[3];
   UInstr* u;
   Bool    wr;
   Int*    last_live_before;
   FlagSet future_dead_flags;

   if (cb->nextTemp > 0)
      last_live_before = VG_(jitmalloc) ( cb->nextTemp * sizeof(Int) );
   else
      last_live_before = NULL;

   
   /* PASS 1: redundant GET elimination.  (Actually, more general than
      that -- eliminates redundant fetches of ArchRegs). */

   /* Find the live-range-ends for all temporaries.  Duplicates code
      in the register allocator :-( */

   for (i = 0; i < cb->nextTemp; i++) last_live_before[i] = -1;

   for (i = cb->used-1; i >= 0; i--) {
      u = &cb->instrs[i];

      k = getTempUsage(u, &tempUse[0]);

      /* For each temp usage ... bwds in program order. */
      for (j = k-1; j >= 0; j--) {
         tr = tempUse[j].tempNo;
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
            u->opcode = NOP;
            u->tag1 = u->tag2 = NoValue;
            n = last_live_before[tr] + 1;
            if (n > cb->used) n = cb->used;
            last_live_before[told] = last_live_before[tr];
            last_live_before[tr] = i-1;
            if (VG_(disassemble))
               VG_(printf)(
                  "at %d: delete GET, rename t%d to t%d in (%d .. %d)\n", 
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
                  if (VG_(disassemble)) 
                     VG_(printf)(
                        "at %d: change ArchReg %S to TempReg t%d\n", 
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
         k = getTempUsage(u, &tempUse[0]);

         for (j = 0; j < k; j++) {
            wr  = tempUse[j].isWrite;
            if (!wr) continue;
            tr = tempUse[j].tempNo;
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
            u->opcode = NOP;
            u->tag1 = u->tag2 = NoValue;
            if (VG_(disassemble)) 
               VG_(printf)("at %d: delete PUT\n", i );
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
   VG_(ppUCodeBlock)(cb, "Before MOV elimination" );
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
         if (VG_(disassemble))
            VG_(printf)(
               "at %d: delete MOV, rename t%d to t%d in (%d .. %d)\n",
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
         u->opcode = NOP;
         u->tag1 = u->tag2 = NoValue;
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
         if (VG_(disassemble)) {
            VG_(printf)("at %d: annul flag write ", i);
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
      VG_(jitfree) ( last_live_before );
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
   TempUse      tempUse[3];
   UCodeBlock*  c2;

   /* Used to denote ... well, "no value" in this fn. */
#  define VG_NOTHING (-2)

   /* Initialise the TempReg info.  */
   if (c1->nextTemp > 0)
      temp_info = VG_(jitmalloc)(c1->nextTemp * sizeof(TempInfo) );
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
      k = getTempUsage(&c1->instrs[i], &tempUse[0]);
      vg_assert(k >= 0 && k <= 3);

      /* For each temp usage ... fwds in program order */
      for (j = 0; j < k; j++) {
         tno = tempUse[j].tempNo;
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
         VG_(panic)("register allocation failed -- out of spill slots");
      }
      ss_busy_until_before[j] = temp_info[i].dead_before;
      temp_info[i].spill_no = j;
      if (j > max_ss_no)
         max_ss_no = j;
   }

   VG_(total_reg_rank) += (max_ss_no+1);

   /* Show live ranges and assigned spill slot nos. */

   if (VG_(disassemble)) {
      VG_(printf)("Live Range Assignments\n");

      for (i = 0; i < c1->nextTemp; i++) {
         if (temp_info[i].live_after == VG_NOTHING) 
            continue;
         VG_(printf)(
            "   LR %d is   after %d to before %d   spillno %d\n",
            i,
            temp_info[i].live_after,
            temp_info[i].dead_before,
            temp_info[i].spill_no
         );
      }
   }

   /* Now that we've established a spill slot number for each used
      temporary, we can go ahead and do the core of the "Second-chance
      binpacking" allocation algorithm. */

   /* Resulting code goes here.  We generate it all in a forwards
      pass. */
   c2 = VG_(allocCodeBlock)();

   /* At the start, no TempRegs are assigned to any real register.
      Correspondingly, all temps claim to be currently resident in
      their spill slots, as computed by the previous two passes. */
   for (i = 0; i < VG_MAX_REALREGS; i++)
      real_to_temp[i] = VG_NOTHING;
   for (i = 0; i < c1->nextTemp; i++)
      temp_info[i].real_no = VG_NOTHING;

   if (VG_(disassemble))
      VG_(printf)("\n");

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

      if (VG_(disassemble))
         VG_(ppUInstr)(i, &c1->instrs[i]);

      /* First, free up enough real regs for this insn.  This may
         generate spill stores since we may have to evict some TempRegs
         currently in real regs.  Also generates spill loads. */

      k = getTempUsage(&c1->instrs[i], &tempUse[0]);
      vg_assert(k >= 0 && k <= 3);

      /* For each ***different*** temp mentioned in the insn .... */
      for (j = 0; j < k; j++) {

         /* First check if the temp is mentioned again later; if so,
            ignore this mention.  We only want to process each temp
            used by the insn once, even if it is mentioned more than
            once. */
         defer = False;
         tno = tempUse[j].tempNo;
         for (m = j+1; m < k; m++)
            if (tempUse[m].tempNo == tno) 
               defer = True;
         if (defer) 
            continue;

         /* Now we're trying to find a register for tempUse[j].tempNo.
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
            tempUse[0 .. k-1].tempNo, since it would be just plain 
            wrong to eject some other TempReg which we need to use in 
            this insn.

            It is here that it is important to make a good choice of
            register to spill.  */

         /* First, mark those regs which are not spill candidates. */
         for (r = 0; r < VG_MAX_REALREGS; r++) {
            is_spill_cand[r] = True;
            for (m = 0; m < k; m++) {
               if (real_to_temp[r] == tempUse[m].tempNo) {
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
            VG_(panic)("new reg alloc: out of registers ?!");

         /* Eject r.  Important refinement: don't bother if the
            associated TempReg is now dead. */
         vg_assert(real_to_temp[r] != VG_NOTHING);
         vg_assert(real_to_temp[r] != tno);
         temp_info[real_to_temp[r]].real_no = VG_NOTHING;
         if (temp_info[real_to_temp[r]].dead_before > i) {
            uInstr2(c2, PUT, 4, 
                        RealReg, VG_(rankToRealRegNo)(r), 
                        SpillNo, temp_info[real_to_temp[r]].spill_no);
            VG_(uinstrs_spill)++;
            spill_reqd = True;
            if (VG_(disassemble))
               VG_(ppUInstr)(c2->used-1, &LAST_UINSTR(c2));
         }

         /* Decide if tno is read. */
         isRead = False;
         for (m = 0; m < k; m++)
            if (tempUse[m].tempNo == tno && !tempUse[m].isWrite) 
               isRead = True;

         /* If so, generate a spill load. */
         if (isRead) {
            uInstr2(c2, GET, 4, 
                        SpillNo, temp_info[tno].spill_no, 
                        RealReg, VG_(rankToRealRegNo)(r) );
            VG_(uinstrs_spill)++;
            spill_reqd = True;
            if (VG_(disassemble))
               VG_(ppUInstr)(c2->used-1, &LAST_UINSTR(c2));
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
         tempUse[j].realNo 
            = VG_(rankToRealRegNo)(temp_info[tempUse[j].tempNo].real_no);
      VG_(copyUInstr)(c2, &c1->instrs[i]);
      patchUInstr(&LAST_UINSTR(c2), &tempUse[0], k);

      if (VG_(disassemble)) {
         VG_(ppUInstr)(c2->used-1, &LAST_UINSTR(c2));
         VG_(printf)("\n");
      }
   }

   if (temp_info != NULL)
      VG_(jitfree)(temp_info);

   VG_(freeCodeBlock)(c1);

   if (spill_reqd) 
      VG_(translations_needing_spill)++;

   return c2;

#  undef VG_NOTHING

}


/*------------------------------------------------------------*/
/*--- New instrumentation machinery.                       ---*/
/*------------------------------------------------------------*/

static
VgTagOp get_VgT_ImproveOR_TQ ( Int sz )
{
   switch (sz) {
      case 4: return VgT_ImproveOR4_TQ;
      case 2: return VgT_ImproveOR2_TQ;
      case 1: return VgT_ImproveOR1_TQ;
      default: VG_(panic)("get_VgT_ImproveOR_TQ");
   }
}


static
VgTagOp get_VgT_ImproveAND_TQ ( Int sz )
{
   switch (sz) {
      case 4: return VgT_ImproveAND4_TQ;
      case 2: return VgT_ImproveAND2_TQ;
      case 1: return VgT_ImproveAND1_TQ;
      default: VG_(panic)("get_VgT_ImproveAND_TQ");
   }
}


static
VgTagOp get_VgT_Left ( Int sz )
{
   switch (sz) {
      case 4: return VgT_Left4;
      case 2: return VgT_Left2;
      case 1: return VgT_Left1;
      default: VG_(panic)("get_VgT_Left");
   }
}


static
VgTagOp get_VgT_UifU ( Int sz )
{
   switch (sz) {
      case 4: return VgT_UifU4;
      case 2: return VgT_UifU2;
      case 1: return VgT_UifU1;
      case 0: return VgT_UifU0;
      default: VG_(panic)("get_VgT_UifU");
   }
}


static
VgTagOp get_VgT_DifD ( Int sz )
{
   switch (sz) {
      case 4: return VgT_DifD4;
      case 2: return VgT_DifD2;
      case 1: return VgT_DifD1;
      default: VG_(panic)("get_VgT_DifD");
   }
}


static 
VgTagOp get_VgT_PCast ( Int szs, Int szd )
{
   if (szs == 4 && szd == 0) return VgT_PCast40;
   if (szs == 2 && szd == 0) return VgT_PCast20;
   if (szs == 1 && szd == 0) return VgT_PCast10;
   if (szs == 0 && szd == 1) return VgT_PCast01;
   if (szs == 0 && szd == 2) return VgT_PCast02;
   if (szs == 0 && szd == 4) return VgT_PCast04;
   if (szs == 1 && szd == 4) return VgT_PCast14;
   if (szs == 1 && szd == 2) return VgT_PCast12;
   if (szs == 1 && szd == 1) return VgT_PCast11;
   VG_(printf)("get_VgT_PCast(%d,%d)\n", szs, szd);
   VG_(panic)("get_VgT_PCast");
}


static 
VgTagOp get_VgT_Widen ( Bool syned, Int szs, Int szd )
{
   if (szs == 1 && szd == 2 && syned)  return VgT_SWiden12;
   if (szs == 1 && szd == 2 && !syned) return VgT_ZWiden12;

   if (szs == 1 && szd == 4 && syned)  return VgT_SWiden14;
   if (szs == 1 && szd == 4 && !syned) return VgT_ZWiden14;

   if (szs == 2 && szd == 4 && syned)  return VgT_SWiden24;
   if (szs == 2 && szd == 4 && !syned) return VgT_ZWiden24;

   VG_(printf)("get_VgT_Widen(%d,%d,%d)\n", (Int)syned, szs, szd);
   VG_(panic)("get_VgT_Widen");
}

/* Pessimally cast the spec'd shadow from one size to another. */
static 
void create_PCast ( UCodeBlock* cb, Int szs, Int szd, Int tempreg )
{
   if (szs == 0 && szd == 0)
      return;
   uInstr3(cb, TAG1, 0, TempReg, tempreg, 
                        NoValue, 0, 
                        Lit16,   get_VgT_PCast(szs,szd));
}


/* Create a signed or unsigned widen of the spec'd shadow from one
   size to another.  The only allowed size transitions are 1->2, 1->4
   and 2->4. */
static 
void create_Widen ( UCodeBlock* cb, Bool signed_widen,
                    Int szs, Int szd, Int tempreg )
{
   if (szs == szd) return;
   uInstr3(cb, TAG1, 0, TempReg, tempreg, 
                        NoValue, 0, 
                        Lit16,   get_VgT_Widen(signed_widen,szs,szd));
}


/* Get the condition codes into a new shadow, at the given size. */
static
Int create_GETVF ( UCodeBlock* cb, Int sz )
{
   Int tt = newShadow(cb);
   uInstr1(cb, GETVF, 0, TempReg, tt);
   create_PCast(cb, 0, sz, tt);
   return tt;
}


/* Save the condition codes from the spec'd shadow. */
static
void create_PUTVF ( UCodeBlock* cb, Int sz, Int tempreg )
{
   if (sz == 0) {
      uInstr1(cb, PUTVF, 0, TempReg, tempreg);
   } else { 
      Int tt = newShadow(cb);
      uInstr2(cb, MOV, 4, TempReg, tempreg, TempReg, tt);
      create_PCast(cb, sz, 0, tt);
      uInstr1(cb, PUTVF, 0, TempReg, tt);
   }
}


/* Do Left on the spec'd shadow. */
static 
void create_Left ( UCodeBlock* cb, Int sz, Int tempreg )
{
   uInstr3(cb, TAG1, 0, 
               TempReg, tempreg,
               NoValue, 0, 
               Lit16, get_VgT_Left(sz));
}


/* Do UifU on ts and td, putting the result in td. */
static 
void create_UifU ( UCodeBlock* cb, Int sz, Int ts, Int td )
{
   uInstr3(cb, TAG2, 0, TempReg, ts, TempReg, td,
               Lit16, get_VgT_UifU(sz));
}


/* Do DifD on ts and td, putting the result in td. */
static 
void create_DifD ( UCodeBlock* cb, Int sz, Int ts, Int td )
{
   uInstr3(cb, TAG2, 0, TempReg, ts, TempReg, td,
               Lit16, get_VgT_DifD(sz));
}


/* Do HelpAND on value tval and tag tqqq, putting the result in
   tqqq. */
static 
void create_ImproveAND_TQ ( UCodeBlock* cb, Int sz, Int tval, Int tqqq )
{
   uInstr3(cb, TAG2, 0, TempReg, tval, TempReg, tqqq,
               Lit16, get_VgT_ImproveAND_TQ(sz));
}


/* Do HelpOR on value tval and tag tqqq, putting the result in
   tqqq. */
static 
void create_ImproveOR_TQ ( UCodeBlock* cb, Int sz, Int tval, Int tqqq )
{
   uInstr3(cb, TAG2, 0, TempReg, tval, TempReg, tqqq,
               Lit16, get_VgT_ImproveOR_TQ(sz));
}


/* Get the shadow for an operand described by (tag, val).  Emit code
   to do this and return the identity of the shadow holding the
   result.  The result tag is always copied into a new shadow, so it
   can be modified without trashing the original.*/
static
Int /* TempReg */ getOperandShadow ( UCodeBlock* cb, 
                                     Int sz, Int tag, Int val )
{
   Int sh;
   sh = newShadow(cb);
   if (tag == TempReg) {
      uInstr2(cb, MOV, 4, TempReg, SHADOW(val), TempReg, sh);
      return sh;
   }
   if (tag == Literal) {
      uInstr1(cb, SETV, sz, TempReg, sh);
      return sh;
   }
   if (tag == ArchReg) {
      uInstr2(cb, GETV, sz, ArchReg, val, TempReg, sh);
      return sh;
   }
   VG_(panic)("getOperandShadow");
}



/* Create and return an instrumented version of cb_in.  Free cb_in
   before returning. */
static UCodeBlock* vg_instrument ( UCodeBlock* cb_in )
{
   UCodeBlock* cb;
   Int         i, j;
   UInstr*     u_in;
   Int         qs, qd, qt, qtt;
   cb = VG_(allocCodeBlock)();
   cb->nextTemp = cb_in->nextTemp;

   for (i = 0; i < cb_in->used; i++) {
      qs = qd = qt = qtt = INVALID_TEMPREG;
      u_in = &cb_in->instrs[i];

      /* if (i > 0) uInstr1(cb, NOP, 0, NoValue, 0); */

      /* VG_(ppUInstr)(0, u_in); */
      switch (u_in->opcode) {

         case NOP:
            break;

         case INCEIP:
            VG_(copyUInstr)(cb, u_in);
            break;

         /* Loads and stores.  Test the V bits for the address.  24
            Mar 02: since the address is A-checked anyway, there's not
            really much point in doing the V-check too, unless you
            think that you might use addresses which are undefined but
            still addressible.  Hence the optionalisation of the V
            check.

            The LOADV/STOREV does an addressibility check for the
            address. */

         case LOAD: 
            if (VG_(clo_check_addrVs)) {
               uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val1));
               uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val1));
            }
            uInstr2(cb, LOADV, u_in->size, 
                        TempReg, u_in->val1,
                        TempReg, SHADOW(u_in->val2));
            VG_(copyUInstr)(cb, u_in);
            break;
         case STORE:
            if (VG_(clo_check_addrVs)) {
               uInstr1(cb, TESTV,  4, TempReg, SHADOW(u_in->val2));
               uInstr1(cb, SETV,   4, TempReg, SHADOW(u_in->val2));
            }
            uInstr2(cb, STOREV, u_in->size,
                        TempReg, SHADOW(u_in->val1), 
                        TempReg, u_in->val2);
            VG_(copyUInstr)(cb, u_in);
            break;

         /* Moving stuff around.  Make the V bits follow accordingly,
            but don't do anything else.  */

         case GET:
            uInstr2(cb, GETV, u_in->size,
                        ArchReg, u_in->val1,
                        TempReg, SHADOW(u_in->val2));
            VG_(copyUInstr)(cb, u_in);
            break;
         case PUT:
            uInstr2(cb, PUTV, u_in->size, 
                        TempReg, SHADOW(u_in->val1),
                        ArchReg, u_in->val2);
            VG_(copyUInstr)(cb, u_in);
            break;

         case GETF:
            /* This is not the smartest way to do it, but should work. */
            qd = create_GETVF(cb, u_in->size);
            uInstr2(cb, MOV, 4, TempReg, qd, TempReg, SHADOW(u_in->val1));
            VG_(copyUInstr)(cb, u_in);
            break;
         case PUTF:
            create_PUTVF(cb, u_in->size, SHADOW(u_in->val1));
            VG_(copyUInstr)(cb, u_in);
            break;

         case MOV:
            switch (u_in->tag1) {
               case TempReg: 
                  uInstr2(cb, MOV, 4,
                              TempReg, SHADOW(u_in->val1),
                              TempReg, SHADOW(u_in->val2));
                  break;
               case Literal: 
                  uInstr1(cb, SETV, u_in->size, 
                              TempReg, SHADOW(u_in->val2));
                  break;
               default: 
                  VG_(panic)("vg_instrument: MOV");
            }
            VG_(copyUInstr)(cb, u_in);
            break;

         /* Special case of add, where one of the operands is a literal.
            lea1(t) = t + some literal.
            Therefore: lea1#(qa) = left(qa) 
         */
         case LEA1:
            vg_assert(u_in->size == 4 && !VG_(anyFlagUse)(u_in));
            qs = SHADOW(u_in->val1);
            qd = SHADOW(u_in->val2);
            uInstr2(cb, MOV, 4, TempReg, qs, TempReg, qd);
            create_Left(cb, u_in->size, qd);
            VG_(copyUInstr)(cb, u_in);
            break;

         /* Another form of add.  
            lea2(ts,tt,shift) = ts + (tt << shift); shift is a literal
                                and is 0,1,2 or 3.
            lea2#(qs,qt) = left(qs `UifU` (qt << shift)).
            Note, subtly, that the shift puts zeroes at the bottom of qt,
            meaning Valid, since the corresponding shift of tt puts 
            zeroes at the bottom of tb.
         */
         case LEA2: {
            Int shift;
            vg_assert(u_in->size == 4 && !VG_(anyFlagUse)(u_in));
            switch (u_in->extra4b) {
               case 1: shift = 0; break;
               case 2: shift = 1; break;
               case 4: shift = 2; break;
               case 8: shift = 3; break;
               default: VG_(panic)( "vg_instrument(LEA2)" );
            }
            qs = SHADOW(u_in->val1);
            qt = SHADOW(u_in->val2);
            qd = SHADOW(u_in->val3);
            uInstr2(cb, MOV, 4, TempReg, qt, TempReg, qd);
            if (shift > 0) {
               uInstr2(cb, SHL, 4, Literal, 0, TempReg, qd);
               uLiteral(cb, shift);
            }
            create_UifU(cb, 4, qs, qd);
            create_Left(cb, u_in->size, qd);
            VG_(copyUInstr)(cb, u_in);
            break;
         }

         /* inc#/dec#(qd) = q `UifU` left(qd) = left(qd) */
         case INC: case DEC:
            qd = SHADOW(u_in->val1);
            create_Left(cb, u_in->size, qd);
            if (u_in->flags_w != FlagsEmpty)
               create_PUTVF(cb, u_in->size, qd);
            VG_(copyUInstr)(cb, u_in);
            break;

         /* This is a HACK (approximation :-) */
         /* rcl#/rcr#(qs,qd) 
               = let q0 = pcast-sz-0(qd) `UifU` pcast-sz-0(qs) `UifU` eflags#
                 eflags# = q0
                 qd =pcast-0-sz(q0)
            Ie, cast everything down to a single bit, then back up.
            This assumes that any bad bits infect the whole word and 
            the eflags.
         */
         case RCL: case RCR:
	    vg_assert(u_in->flags_r != FlagsEmpty);
            /* The following assertion looks like it makes sense, but is
               actually wrong.  Consider this:
                  rcll    %eax
                  imull   %eax, %eax
               The rcll writes O and C but so does the imull, so the O and C 
               write of the rcll is annulled by the prior improvement pass.
               Noticed by Kevin Ryde <user42@zip.com.au>
            */
	    /* vg_assert(u_in->flags_w != FlagsEmpty); */
            qs = getOperandShadow(cb, u_in->size, u_in->tag1, u_in->val1);
            /* We can safely modify qs; cast it to 0-size. */
            create_PCast(cb, u_in->size, 0, qs);
            qd = SHADOW(u_in->val2);
            create_PCast(cb, u_in->size, 0, qd);
            /* qs is cast-to-0(shift count#), and qd is cast-to-0(value#). */
            create_UifU(cb, 0, qs, qd);
            /* qs is now free; reuse it for the flag definedness. */
            qs = create_GETVF(cb, 0);
            create_UifU(cb, 0, qs, qd);
            create_PUTVF(cb, 0, qd);
            create_PCast(cb, 0, u_in->size, qd);
            VG_(copyUInstr)(cb, u_in);
            break;

         /* for OP in shl shr sar rol ror
            (qs is shift count#, qd is value to be OP#d)
            OP(ts,td)
            OP#(qs,qd)
               = pcast-1-sz(qs) `UifU` OP(ts,qd)
            So we apply OP to the tag bits too, and then UifU with
            the shift count# to take account of the possibility of it
            being undefined.
            
            A bit subtle:
               ROL/ROR rearrange the tag bits as per the value bits.
               SHL/SHR shifts zeroes into the value, and corresponding 
                  zeroes indicating Definedness into the tag.
               SAR copies the top bit of the value downwards, and therefore
                  SAR also copies the definedness of the top bit too.
            So in all five cases, we just apply the same op to the tag 
            bits as is applied to the value bits.  Neat!
         */
         case SHL:
         case SHR: case SAR:
         case ROL: case ROR: {
            Int t_amount = INVALID_TEMPREG;
            vg_assert(u_in->tag1 == TempReg || u_in->tag1 == Literal);
            vg_assert(u_in->tag2 == TempReg);
            qd = SHADOW(u_in->val2);

            /* Make qs hold shift-count# and make
               t_amount be a TempReg holding the shift count. */
            if (u_in->tag1 == Literal) {
               t_amount = newTemp(cb);
               uInstr2(cb, MOV, 4, Literal, 0, TempReg, t_amount);
               uLiteral(cb, u_in->lit32);
               qs = SHADOW(t_amount);
               uInstr1(cb, SETV, 1, TempReg, qs);
            } else {
               t_amount = u_in->val1;
               qs = SHADOW(u_in->val1);
            }

            uInstr2(cb, u_in->opcode, 
                        u_in->size, 
                        TempReg, t_amount, 
                        TempReg, qd);
            qt = newShadow(cb);
            uInstr2(cb, MOV, 4, TempReg, qs, TempReg, qt);
            create_PCast(cb, 1, u_in->size, qt);
            create_UifU(cb, u_in->size, qt, qd);
            VG_(copyUInstr)(cb, u_in);
            break;
         }

         /* One simple tag operation. */
         case WIDEN:
            vg_assert(u_in->tag1 == TempReg);
            create_Widen(cb, u_in->signed_widen, u_in->extra4b, u_in->size, 
                             SHADOW(u_in->val1));
            VG_(copyUInstr)(cb, u_in);
            break;

         /* not#(x) = x (since bitwise independent) */
         case NOT:
            vg_assert(u_in->tag1 == TempReg);
            VG_(copyUInstr)(cb, u_in);
            break;

         /* neg#(x) = left(x) (derivable from case for SUB) */
         case NEG:
            vg_assert(u_in->tag1 == TempReg);
            create_Left(cb, u_in->size, SHADOW(u_in->val1));
            VG_(copyUInstr)(cb, u_in);
            break;

         /* bswap#(x) = bswap(x) */
         case BSWAP:
            vg_assert(u_in->tag1 == TempReg);
            vg_assert(u_in->size == 4);
            qd = SHADOW(u_in->val1);
            uInstr1(cb, BSWAP, 4, TempReg, qd);
            VG_(copyUInstr)(cb, u_in);
            break;

         /* cc2val#(qd) = pcast-0-to-size(eflags#) */
         case CC2VAL:
            vg_assert(u_in->tag1 == TempReg);
            vg_assert(u_in->flags_r != FlagsEmpty);
            qt = create_GETVF(cb, u_in->size);
            uInstr2(cb, MOV, 4, TempReg, qt, TempReg, SHADOW(u_in->val1));
            VG_(copyUInstr)(cb, u_in);
            break;

         /* cmov#(qs,qd) = cmov(qs,qd)
            That is, do the cmov of tags using the same flags as for
            the data (obviously).  However, first do a test on the 
            validity of the flags.
         */
         case CMOV:
            vg_assert(u_in->size == 4);
            vg_assert(u_in->tag1 == TempReg);
            vg_assert(u_in->tag2 == TempReg);
            vg_assert(u_in->flags_r != FlagsEmpty);
            vg_assert(u_in->flags_w == FlagsEmpty);
            qs = SHADOW(u_in->val1);
            qd = SHADOW(u_in->val2);
            qt = create_GETVF(cb, 0);
            uInstr1(cb, TESTV, 0, TempReg, qt);
            /* qt should never be referred to again.  Nevertheless
               ... */
            uInstr1(cb, SETV, 0, TempReg, qt);

            uInstr2(cb, CMOV, 4, TempReg, qs, TempReg, qd);
            LAST_UINSTR(cb).cond    = u_in->cond;
            LAST_UINSTR(cb).flags_r = u_in->flags_r;

            VG_(copyUInstr)(cb, u_in);
            break;

         /* add#/sub#(qs,qd) 
               = qs `UifU` qd `UifU` left(qs) `UifU` left(qd)
               = left(qs) `UifU` left(qd)
               = left(qs `UifU` qd)
            adc#/sbb#(qs,qd)
               = left(qs `UifU` qd) `UifU` pcast(eflags#)
            Second arg (dest) is TempReg.
            First arg (src) is Literal or TempReg or ArchReg. 
         */
         case ADD: case SUB:
         case ADC: case SBB:
            qd = SHADOW(u_in->val2);
            qs = getOperandShadow(cb, u_in->size, u_in->tag1, u_in->val1);
            create_UifU(cb, u_in->size, qs, qd);
            create_Left(cb, u_in->size, qd);
            if (u_in->opcode == ADC || u_in->opcode == SBB) {
               vg_assert(u_in->flags_r != FlagsEmpty);
               qt = create_GETVF(cb, u_in->size);
               create_UifU(cb, u_in->size, qt, qd);
            }
            if (u_in->flags_w != FlagsEmpty) {
               create_PUTVF(cb, u_in->size, qd);
            }
            VG_(copyUInstr)(cb, u_in);
            break;

         /* xor#(qs,qd) = qs `UifU` qd */
         case XOR:
            qd = SHADOW(u_in->val2);
            qs = getOperandShadow(cb, u_in->size, u_in->tag1, u_in->val1);
            create_UifU(cb, u_in->size, qs, qd);
            if (u_in->flags_w != FlagsEmpty) {
               create_PUTVF(cb, u_in->size, qd);
            }
            VG_(copyUInstr)(cb, u_in);
            break;

         /* and#/or#(qs,qd) 
               = (qs `UifU` qd) `DifD` improve(vs,qs) 
                                `DifD` improve(vd,qd)
            where improve is the relevant one of
                Improve{AND,OR}_TQ
            Use the following steps, with qt as a temp:
               qt = improve(vd,qd)
               qd = qs `UifU` qd
               qd = qt `DifD` qd
               qt = improve(vs,qs)
               qd = qt `DifD` qd
         */
         case AND: case OR:
            vg_assert(u_in->tag1 == TempReg);
            vg_assert(u_in->tag2 == TempReg);
            qd = SHADOW(u_in->val2);
            qs = SHADOW(u_in->val1);
            qt = newShadow(cb);

            /* qt = improve(vd,qd) */
            uInstr2(cb, MOV, 4, TempReg, qd, TempReg, qt);
            if (u_in->opcode == AND)
               create_ImproveAND_TQ(cb, u_in->size, u_in->val2, qt);
            else
               create_ImproveOR_TQ(cb, u_in->size, u_in->val2, qt);
            /* qd = qs `UifU` qd */
            create_UifU(cb, u_in->size, qs, qd);
            /* qd = qt `DifD` qd */
            create_DifD(cb, u_in->size, qt, qd);
            /* qt = improve(vs,qs) */
            uInstr2(cb, MOV, 4, TempReg, qs, TempReg, qt);
            if (u_in->opcode == AND)
               create_ImproveAND_TQ(cb, u_in->size, u_in->val1, qt);
            else
               create_ImproveOR_TQ(cb, u_in->size, u_in->val1, qt);
            /* qd = qt `DifD` qd */
               create_DifD(cb, u_in->size, qt, qd);
            /* So, finally qd is the result tag. */
            if (u_in->flags_w != FlagsEmpty) {
               create_PUTVF(cb, u_in->size, qd);
            }
            VG_(copyUInstr)(cb, u_in);
            break;

         /* Machinery to do with supporting CALLM.  Copy the start and
            end markers only to make the result easier to read
            (debug); they generate no code and have no effect. 
         */
         case CALLM_S: case CALLM_E:
            VG_(copyUInstr)(cb, u_in);
            break;

         /* Copy PUSH and POP verbatim.  Arg/result absval
            calculations are done when the associated CALL is
            processed.  CLEAR has no effect on absval calculations but
            needs to be copied.  
         */
         case PUSH: case POP: case CLEAR:
            VG_(copyUInstr)(cb, u_in);
            break;

         /* In short:
               callm#(a1# ... an#) = (a1# `UifU` ... `UifU` an#)
            We have to decide on a size to do the computation at,
            although the choice doesn't affect correctness.  We will
            do a pcast to the final size anyway, so the only important
            factor is to choose a size which minimises the total
            number of casts needed.  Valgrind: just use size 0,
            regardless.  It may not be very good for performance
            but does simplify matters, mainly by reducing the number
            of different pessimising casts which have to be implemented.
         */
         case CALLM: {
            UInstr* uu;
            Bool res_used;

            /* Now generate the code.  Get the final result absval
               into qt. */
            qt  = newShadow(cb);
            qtt = newShadow(cb);
            uInstr1(cb, SETV, 0, TempReg, qt);
            for (j = i-1; cb_in->instrs[j].opcode != CALLM_S; j--) {
               uu = & cb_in->instrs[j];
               if (uu->opcode != PUSH) continue;
               /* cast via a temporary */
               uInstr2(cb, MOV, 4, TempReg, SHADOW(uu->val1),
                                   TempReg, qtt);
               create_PCast(cb, uu->size, 0, qtt);
               create_UifU(cb, 0, qtt, qt);
            }
            /* Remembering also that flags read count as inputs. */
            if (u_in->flags_r != FlagsEmpty) {
               qtt = create_GETVF(cb, 0);
               create_UifU(cb, 0, qtt, qt);
            }

            /* qt now holds the result tag.  If any results from the
               call are used, either by fetching with POP or
               implicitly by writing the flags, we copy the result
               absval to the relevant location.  If not used, the call
               must have been for its side effects, so we test qt here
               and now.  Note that this assumes that all values
               removed by POP continue to be live.  So dead args
               *must* be removed with CLEAR, not by POPping them into
               a dummy tempreg. 
            */
            res_used = False;
            for (j = i+1; cb_in->instrs[j].opcode != CALLM_E; j++) {
               uu = & cb_in->instrs[j];
               if (uu->opcode != POP) continue;
               /* Cast via a temp. */
               uInstr2(cb, MOV, 4, TempReg, qt, TempReg, qtt);
               create_PCast(cb, 0, uu->size, qtt);
               uInstr2(cb, MOV, 4, TempReg, qtt, 
                                   TempReg, SHADOW(uu->val1));
               res_used = True;
            }
            if (u_in->flags_w != FlagsEmpty) {
               create_PUTVF(cb, 0, qt);
               res_used = True;
            }
            if (!res_used) {
               uInstr1(cb, TESTV, 0, TempReg, qt);
               /* qt should never be referred to again.  Nevertheless
                  ... */
               uInstr1(cb, SETV, 0, TempReg, qt);
            }
            VG_(copyUInstr)(cb, u_in);
            break;
         }
         /* Whew ... */

         case JMP:
            if (u_in->tag1 == TempReg) {
               uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val1));
               uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val1));
            } else {
               vg_assert(u_in->tag1 == Literal);
            }
            if (u_in->cond != CondAlways) {
               vg_assert(u_in->flags_r != FlagsEmpty);
               qt = create_GETVF(cb, 0);
               uInstr1(cb, TESTV, 0, TempReg, qt);
               /* qt should never be referred to again.  Nevertheless
                  ... */
               uInstr1(cb, SETV, 0, TempReg, qt);
            }
            VG_(copyUInstr)(cb, u_in);
            break;

         case JIFZ:
            uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val1));
            uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val1));
            VG_(copyUInstr)(cb, u_in);
            break;

         /* Emit a check on the address used.  For FPU_R, the value
            loaded into the FPU is checked at the time it is read from
            memory (see synth_fpu_mem_check_actions).  */
         case FPU_R: case FPU_W:
            vg_assert(u_in->tag2 == TempReg);
            uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val2));
            uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val2));
            VG_(copyUInstr)(cb, u_in);
            break;

         /* For FPU insns not referencing memory, just copy thru. */
         case FPU: 
            VG_(copyUInstr)(cb, u_in);
            break;

         default:
            VG_(ppUInstr)(0, u_in);
            VG_(panic)( "vg_instrument: unhandled case");

      } /* end of switch (u_in->opcode) */

   } /* end of for loop */

   VG_(freeCodeBlock)(cb_in);
   return cb;
}

/*------------------------------------------------------------*/
/*--- Clean up mem check instrumentation.                  ---*/
/*------------------------------------------------------------*/

#define VGC_IS_SHADOW(tempreg) ((tempreg % 2) == 1)
#define VGC_UNDEF ((UChar)100)
#define VGC_VALUE ((UChar)101)

#define NOP_no_msg(uu)                                         \
   do { uu->opcode = NOP; } while (False)

#define NOP_tag1_op(uu)                                        \
   do { uu->opcode = NOP;                                      \
        if (VG_(disassemble))                                  \
           VG_(printf)("at %d: delete %s due to defd arg\n",   \
                       i, VG_(nameOfTagOp(u->val3)));          \
   } while (False)

#define SETV_tag1_op(uu,newsz)                                 \
   do { uu->opcode = SETV;                                     \
        uu->size = newsz;                                      \
        uu->tag2 = uu->tag3 = NoValue;                         \
        if (VG_(disassemble))                                  \
           VG_(printf)("at %d: convert %s to SETV%d "          \
                       "due to defd arg\n",                    \
                       i, VG_(nameOfTagOp(u->val3)), newsz);   \
   } while (False)



/* Run backwards and delete SETVs on shadow temps for which the next
   action is a write.  Needs an env saying whether or not the next
   action is a write.  The supplied UCodeBlock is destructively
   modified.
*/
static void vg_delete_redundant_SETVs ( UCodeBlock* cb )
{
   Bool*   next_is_write;
   Int     i, j, k, n_temps;
   UInstr* u;
   TempUse tempUse[3];

   n_temps = cb->nextTemp;
   if (n_temps == 0) return;

   next_is_write = VG_(jitmalloc)(n_temps * sizeof(Bool));

   for (i = 0; i < n_temps; i++) next_is_write[i] = True;

   for (i = cb->used-1; i >= 0; i--) {
      u = &cb->instrs[i];

      /* If we're not checking address V bits, there will be a lot of
         GETVs, TAG1s and TAG2s calculating values which are never
         used.  These first three cases get rid of them. */

      if (u->opcode == GETV && VGC_IS_SHADOW(u->val2) 
                            && next_is_write[u->val2]
                            && !VG_(clo_check_addrVs)) {
         u->opcode = NOP;
         u->size = 0;
         if (VG_(disassemble)) 
            VG_(printf)("at %d: delete GETV\n", i);
      } else

      if (u->opcode == TAG1 && VGC_IS_SHADOW(u->val1) 
                            && next_is_write[u->val1]
                            && !VG_(clo_check_addrVs)) {
         u->opcode = NOP;
         u->size = 0;
         if (VG_(disassemble)) 
            VG_(printf)("at %d: delete TAG1\n", i);
      } else

      if (u->opcode == TAG2 && VGC_IS_SHADOW(u->val2) 
                            && next_is_write[u->val2]
                            && !VG_(clo_check_addrVs)) {
         u->opcode = NOP;
         u->size = 0;
         if (VG_(disassemble)) 
            VG_(printf)("at %d: delete TAG2\n", i);
      } else

      /* We do the rest of these regardless of whether or not
         addresses are V-checked. */

      if (u->opcode == MOV && VGC_IS_SHADOW(u->val2) 
                           && next_is_write[u->val2]) {
         /* This MOV is pointless because the target is dead at this
            point.  Delete it. */
         u->opcode = NOP;
         u->size = 0;
         if (VG_(disassemble)) 
            VG_(printf)("at %d: delete MOV\n", i);
      } else

      if (u->opcode == SETV) {
         if (u->tag1 == TempReg) {
            vg_assert(VGC_IS_SHADOW(u->val1));
            if (next_is_write[u->val1]) {
               /* This write is pointless, so annul it. */
               u->opcode = NOP;
               u->size = 0;
               if (VG_(disassemble)) 
                  VG_(printf)("at %d: delete SETV\n", i);
            } else {
               /* This write has a purpose; don't annul it, but do
                  notice that we did it. */
               next_is_write[u->val1] = True;
            }
              
         }

      } else {
         /* Find out what this insn does to the temps. */
         k = getTempUsage(u, &tempUse[0]);
         vg_assert(k <= 3);
         for (j = k-1; j >= 0; j--) {
            next_is_write[ tempUse[j].tempNo ]
                         = tempUse[j].isWrite;
         }
      }

   }

   VG_(jitfree)(next_is_write);
}


/* Run forwards, propagating and using the is-completely-defined
   property.  This removes a lot of redundant tag-munging code.
   Unfortunately it requires intimate knowledge of how each uinstr and
   tagop modifies its arguments.  This duplicates knowledge of uinstr
   tempreg uses embodied in getTempUsage(), which is unfortunate. 
   The supplied UCodeBlock* is modified in-place.

   For each value temp, def[] should hold VGC_VALUE.

   For each shadow temp, def[] may hold 4,2,1 or 0 iff that shadow is
   definitely known to be fully defined at that size.  In all other
   circumstances a shadow's def[] entry is VGC_UNDEF, meaning possibly
   undefined.  In cases of doubt, VGC_UNDEF is always safe.
*/
static void vg_propagate_definedness ( UCodeBlock* cb )
{
   UChar*  def;
   Int     i, j, k, t, n_temps;
   UInstr* u;
   TempUse tempUse[3];

   n_temps = cb->nextTemp;
   if (n_temps == 0) return;

   def = VG_(jitmalloc)(n_temps * sizeof(UChar));
   for (i = 0; i < n_temps; i++) 
      def[i] = VGC_IS_SHADOW(i) ? VGC_UNDEF : VGC_VALUE;

   /* Run forwards, detecting and using the all-defined property. */

   for (i = 0; i < cb->used; i++) {
      u = &cb->instrs[i];
      switch (u->opcode) {

      /* Tag-handling uinstrs. */

         /* Deal with these quickly. */
         case NOP:
         case INCEIP:
            break;

         /* Make a tag defined. */
         case SETV:
            vg_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            def[u->val1] = u->size;
            break;

         /* Check definedness of a tag. */
         case TESTV:
            vg_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            if (def[u->val1] <= 4) { 
               vg_assert(def[u->val1] == u->size); 
               NOP_no_msg(u);
               if (VG_(disassemble)) 
                  VG_(printf)("at %d: delete TESTV on defd arg\n", i);
            }
            break;

         /* Applies to both values and tags.  Propagate Definedness
            property through copies.  Note that this isn't optional;
            we *have* to do this to keep def[] correct. */
         case MOV:
            vg_assert(u->tag2 == TempReg);
            if (u->tag1 == TempReg) {
               if (VGC_IS_SHADOW(u->val1)) {
                  vg_assert(VGC_IS_SHADOW(u->val2));
                  def[u->val2] = def[u->val1];
               }
            }
            break;

         case PUTV:
            vg_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            if (def[u->val1] <= 4) {
               vg_assert(def[u->val1] == u->size);
               u->tag1 = Literal;
               u->val1 = 0;
               switch (u->size) {
                  case 4: u->lit32 = 0x00000000; break;
                  case 2: u->lit32 = 0xFFFF0000; break;
                  case 1: u->lit32 = 0xFFFFFF00; break;
                  default: VG_(panic)("vg_cleanup(PUTV)");
               }
               if (VG_(disassemble)) 
                  VG_(printf)(
                     "at %d: propagate definedness into PUTV\n", i);
            }
            break;

         case STOREV:
            vg_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            if (def[u->val1] <= 4) {
               vg_assert(def[u->val1] == u->size);
               u->tag1 = Literal;
               u->val1 = 0;
               switch (u->size) {
                  case 4: u->lit32 = 0x00000000; break;
                  case 2: u->lit32 = 0xFFFF0000; break;
                  case 1: u->lit32 = 0xFFFFFF00; break;
                  default: VG_(panic)("vg_cleanup(STOREV)");
               }
               if (VG_(disassemble)) 
                  VG_(printf)(
                     "at %d: propagate definedness into STandV\n", i);
            }
            break;

         /* Nothing interesting we can do with this, I think. */
         case PUTVF:
            break;

         /* Tag handling operations. */
         case TAG2:
            vg_assert(u->tag2 == TempReg && VGC_IS_SHADOW(u->val2));
            vg_assert(u->tag3 == Lit16);
            /* Ultra-paranoid "type" checking. */
            switch (u->val3) {
               case VgT_ImproveAND4_TQ: case VgT_ImproveAND2_TQ:
               case VgT_ImproveAND1_TQ: case VgT_ImproveOR4_TQ:
               case VgT_ImproveOR2_TQ: case VgT_ImproveOR1_TQ:
                  vg_assert(u->tag1 == TempReg && !VGC_IS_SHADOW(u->val1));
                  break;
               default:
                  vg_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
                  break;
            }
            switch (u->val3) {
               Int sz;
               case VgT_UifU4: 
                  sz = 4; goto do_UifU;
               case VgT_UifU2: 
                  sz = 2; goto do_UifU;
               case VgT_UifU1:
                  sz = 1; goto do_UifU;
               case VgT_UifU0:
                  sz = 0; goto do_UifU;
               do_UifU:
                  vg_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
                  vg_assert(u->tag2 == TempReg && VGC_IS_SHADOW(u->val2));
                  if (def[u->val1] <= 4) {
                     /* UifU.  The first arg is defined, so result is
                        simply second arg.  Delete this operation. */
                     vg_assert(def[u->val1] == sz);
                     NOP_no_msg(u);
                     if (VG_(disassemble)) 
                        VG_(printf)(
                           "at %d: delete UifU%d due to defd arg1\n", 
                           i, sz);
                  }
                  else 
                  if (def[u->val2] <= 4) {
                     /* UifU.  The second arg is defined, so result is
                        simply first arg.  Copy to second. */
                     vg_assert(def[u->val2] == sz);
                     u->opcode = MOV; 
                     u->size = 4;
                     u->tag3 = NoValue;
                     def[u->val2] = def[u->val1];
                     if (VG_(disassemble)) 
                        VG_(printf)(
                           "at %d: change UifU%d to MOV due to defd"
                           " arg2\n", 
                           i, sz);
                  }
                  break;
               case VgT_ImproveAND4_TQ:
                  sz = 4; goto do_ImproveAND;
               case VgT_ImproveAND1_TQ:
                  sz = 1; goto do_ImproveAND;
               do_ImproveAND:
                  /* Implements Q = T OR Q.  So if Q is entirely defined,
                     ie all 0s, we get MOV T, Q. */
		  if (def[u->val2] <= 4) {
                     vg_assert(def[u->val2] == sz);
                     u->size = 4; /* Regardless of sz */
                     u->opcode = MOV;
                     u->tag3 = NoValue;
                     def[u->val2] = VGC_UNDEF;
                     if (VG_(disassemble)) 
                        VG_(printf)(
                            "at %d: change ImproveAND%d_TQ to MOV due "
                            "to defd arg2\n", 
                            i, sz);
                  }
                  break;
               default: 
                  goto unhandled;
            }
            break;

         case TAG1:
            vg_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            if (def[u->val1] > 4) break;
            /* We now know that the arg to the op is entirely defined.
               If the op changes the size of the arg, we must replace
               it with a SETV at the new size.  If it doesn't change
               the size, we can delete it completely. */
            switch (u->val3) {
               /* Maintain the same size ... */
               case VgT_Left4: 
                  vg_assert(def[u->val1] == 4);
                  NOP_tag1_op(u);
                  break;
               case VgT_PCast11: 
                  vg_assert(def[u->val1] == 1);
                  NOP_tag1_op(u);
                  break;
               /* Change size ... */
               case VgT_PCast40: 
                  vg_assert(def[u->val1] == 4);
                  SETV_tag1_op(u,0);
                  def[u->val1] = 0;
                  break;
               case VgT_PCast14: 
                  vg_assert(def[u->val1] == 1);
                  SETV_tag1_op(u,4);
                  def[u->val1] = 4;
                  break;
               case VgT_PCast12: 
                  vg_assert(def[u->val1] == 1);
                  SETV_tag1_op(u,2);
                  def[u->val1] = 2;
                  break;
               case VgT_PCast10: 
                  vg_assert(def[u->val1] == 1);
                  SETV_tag1_op(u,0);
                  def[u->val1] = 0;
                  break;
               case VgT_PCast02: 
                  vg_assert(def[u->val1] == 0);
                  SETV_tag1_op(u,2);
                  def[u->val1] = 2;
                  break;
               default: 
                  goto unhandled;
            }
            if (VG_(disassemble)) 
               VG_(printf)(
                  "at %d: delete TAG1 %s due to defd arg\n",
                  i, VG_(nameOfTagOp(u->val3)));
            break;

         default:
         unhandled:
            /* We don't know how to handle this uinstr.  Be safe, and 
               set to VGC_VALUE or VGC_UNDEF all temps written by it. */
            k = getTempUsage(u, &tempUse[0]);
            vg_assert(k <= 3);
            for (j = 0; j < k; j++) {
               t = tempUse[j].tempNo;
               vg_assert(t >= 0 && t < n_temps);
               if (!tempUse[j].isWrite) {
                  /* t is read; ignore it. */
                  if (0&& VGC_IS_SHADOW(t) && def[t] <= 4)
                     VG_(printf)("ignoring def %d at %s %s\n", 
                                 def[t], 
                                 VG_(nameUOpcode)(True, u->opcode),
                                 (u->opcode == TAG1 || u->opcode == TAG2)
                                    ? VG_(nameOfTagOp)(u->val3) 
                                    : (Char*)"");
               } else {
                  /* t is written; better nullify it. */
                  def[t] = VGC_IS_SHADOW(t) ? VGC_UNDEF : VGC_VALUE;
               }
            }
      }
   }

   VG_(jitfree)(def);
}


/* Top level post-instrumentation cleanup function. */
static void vg_cleanup ( UCodeBlock* cb )
{
   vg_propagate_definedness ( cb );
   vg_delete_redundant_SETVs ( cb );
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
*/
void VG_(translate) ( ThreadState* tst, 
                         /* Identity of thread needing this block */
                      Addr  orig_addr,
                      UInt* orig_size,
                      Addr* trans_addr,
                      UInt* trans_size )
{
   Int         n_disassembled_bytes, final_code_size;
   Bool        debugging_translation;
   UChar*      final_code;
   UCodeBlock* cb;

   VGP_PUSHCC(VgpTranslate);
   debugging_translation
      = orig_size == NULL || trans_addr == NULL || trans_size == NULL;

   dis = True;
   dis = debugging_translation;

   /* Check if we're being asked to jump to a silly address, and if so
      record an error message before potentially crashing the entire
      system. */
   if (VG_(clo_instrument) && !debugging_translation && !dis) {
      Addr bad_addr;
      Bool ok = VGM_(check_readable) ( orig_addr, 1, &bad_addr );
      if (!ok) {
         VG_(record_jump_error)(tst, bad_addr);
      }
   }

   /* if (VG_(overall_in_count) >= 4800) dis=True; */
   if (VG_(disassemble))
      VG_(printf)("\n");
   if (0 || dis 
       || (VG_(overall_in_count) > 0 &&
           (VG_(overall_in_count) % 1000 == 0))) {
      if (0&& (VG_(clo_verbosity) > 1 || dis))
         VG_(message)(Vg_UserMsg,
              "trans# %d, bb# %lu, in %d, out %d",
              VG_(overall_in_count), 
              VG_(bbs_done),
              VG_(overall_in_osize), VG_(overall_in_tsize),
              orig_addr );
   }
   cb = VG_(allocCodeBlock)();

   /* Disassemble this basic block into cb. */
   /* VGP_PUSHCC(VgpToUCode); */
   n_disassembled_bytes = VG_(disBB) ( cb, orig_addr );
   /* VGP_POPCC; */
   /* dis=True; */
   /* if (0&& VG_(translations_done) < 617)  */
   /*    dis=False; */
   /* Try and improve the code a bit. */
   if (VG_(clo_optimise)) {
      /* VGP_PUSHCC(VgpImprove); */
      vg_improve ( cb );
      if (VG_(disassemble)) 
         VG_(ppUCodeBlock) ( cb, "Improved code:" );
      /* VGP_POPCC; */
   }
   /* dis=False; */
   /* Add instrumentation code. */
   if (VG_(clo_instrument)) {
      /* VGP_PUSHCC(VgpInstrument); */
      cb = vg_instrument(cb);
      /* VGP_POPCC; */
      if (VG_(disassemble)) 
         VG_(ppUCodeBlock) ( cb, "Instrumented code:" );
      if (VG_(clo_cleanup)) {
         /* VGP_PUSHCC(VgpCleanup); */
         vg_cleanup(cb);
         /* VGP_POPCC; */
         if (VG_(disassemble)) 
            VG_(ppUCodeBlock) ( cb, "Cleaned-up instrumented code:" );
      }
   }

   //VG_(disassemble) = True;

   /* Add cache simulation code. */
   if (VG_(clo_cachesim)) {
      /* VGP_PUSHCC(VgpCacheInstrument); */
      cb = VG_(cachesim_instrument)(cb, orig_addr);
      /* VGP_POPCC; */
      if (VG_(disassemble)) 
         VG_(ppUCodeBlock) ( cb, "Cachesim instrumented code:" );
   }
   
   //VG_(disassemble) = False;
   
   /* Allocate registers. */
   /* VGP_PUSHCC(VgpRegAlloc); */
   cb = vg_do_register_allocation ( cb );
   /* VGP_POPCC; */
   /* dis=False; */
   /* 
   if (VG_(disassemble))
      VG_(ppUCodeBlock) ( cb, "After Register Allocation:");
   */

   /* VGP_PUSHCC(VgpFromUcode); */
   /* NB final_code is allocated with VG_(jitmalloc), not VG_(malloc)
      and so must be VG_(jitfree)'d. */
   final_code = VG_(emit_code)(cb, &final_code_size );
   /* VGP_POPCC; */
   VG_(freeCodeBlock)(cb);

   if (debugging_translation) {
      /* Only done for debugging -- throw away final result. */
      VG_(jitfree)(final_code);
   } else {
      /* Doing it for real -- return values to caller. */
      *orig_size = n_disassembled_bytes;
      *trans_addr = (Addr)final_code;
      *trans_size = final_code_size;
   }
   VGP_POPCC;
}

/*--------------------------------------------------------------------*/
/*--- end                                           vg_translate.c ---*/
/*--------------------------------------------------------------------*/
