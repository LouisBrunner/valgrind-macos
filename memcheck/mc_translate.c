/* -*- c-basic-offset: 3 -*- */
/*--------------------------------------------------------------------*/
/*--- Instrument UCode to perform memory checking operations.      ---*/
/*---                                               mc_translate.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

#include "mc_include.h"

/* ---------------------------------------------------------------------
   Template functions for extending UCode
   ------------------------------------------------------------------ */

/* Compare this with the restrictions on core instructions in
   vg_translate.c:VG_(saneUInstr)().  Everything general said there applies
   here too.
*/
Bool SK_(sane_XUInstr)(Bool beforeRA, Bool beforeLiveness, UInstr* u)
{
// SSS: duplicating these macros really sucks
#  define LIT0 (u->lit32 == 0)
#  define LIT1 (!(LIT0))
#  define LITm (u->tag1 == Literal ? True : LIT0 )
#  define SZ0 (u->size == 0)
#  define SZi (u->size == 4 || u->size == 2 || u->size == 1)
#  define SZj (u->size == 4 || u->size == 2 || u->size == 1 || u->size == 0)
#  define CC0 (u->flags_r == FlagsEmpty && u->flags_w == FlagsEmpty)
#  define TR1 (beforeRA ? (u->tag1 == TempReg) : (u->tag1 == RealReg))
#  define TR2 (beforeRA ? (u->tag2 == TempReg) : (u->tag2 == RealReg))
#  define A1  (u->tag1 == ArchReg)
#  define A2  (u->tag2 == ArchReg)
#  define L1  (u->tag1 == Literal && u->val1 == 0)
#  define Ls1 (u->tag1 == Lit16)
#  define Ls3 (u->tag3 == Lit16)
#  define TRL1 (TR1 || L1)
#  define TRA1 (TR1 || A1)
#  define N2  (u->tag2 == NoValue)
#  define N3  (u->tag3 == NoValue)
#  define COND0    (u->cond         == 0)
#  define EXTRA4b0 (u->extra4b      == 0)
#  define SG_WD0   (u->signed_widen == 0)
#  define JMPKIND0 (u->jmpkind      == 0)
#  define CCALL0   (u->argc==0 && u->regparms_n==0 && u->has_ret_val==0 && \
                    ( beforeLiveness                                       \
                    ? u->regs_live_after == ALL_RREGS_LIVE                 \
                    : True ))
#  define XOTHER   (COND0 && EXTRA4b0 && SG_WD0 && JMPKIND0 && CCALL0)

   Int n_lits = 0;
   if (u->tag1 == Literal) n_lits++;
   if (u->tag2 == Literal) n_lits++;
   if (u->tag3 == Literal) n_lits++;
   if (n_lits > 1) 
      return False;

   /* Fields not checked: val1, val2, val3 */

   switch (u->opcode) {

   /* Fields checked: lit32   size flags_r/w tag1   tag2   tag3    (rest) */
   case LOADV:  return LIT0 && SZi && CC0 &&  TR1 && TR2 &&  N3 && XOTHER;
   case STOREV: return LITm && SZi && CC0 && TRL1 && TR2 &&  N3 && XOTHER;
   case GETV:   return LIT0 && SZi && CC0 &&   A1 && TR2 &&  N3 && XOTHER;
   case PUTV:   return LITm && SZi && CC0 && TRL1 &&  A2 &&  N3 && XOTHER;
   case GETVF: 
   case PUTVF:  return LIT0 && SZ0 && CC0 &&  TR1 &&  N2 &&  N3 && XOTHER;
   case TESTV: 
   case SETV:   return LIT0 && SZj && CC0 && TRA1 &&  N2 &&  N3 && XOTHER;
   case TAG1:   return LIT0 && SZ0 && CC0 &&  TR1 &&  N2 && Ls3 && XOTHER;
   case TAG2:   return LIT0 && SZ0 && CC0 &&  TR1 && TR2 && Ls3 && XOTHER;
   default:
      VG_(printf)("unhandled opcode: %u\n", u->opcode);
      VG_(skin_panic)("SK_(sane_XUInstr): unhandled opcode");
   }
#  undef LIT0
#  undef LIT1
#  undef LITm
#  undef SZ0
#  undef SZi
#  undef SZj
#  undef CC0
#  undef TR1
#  undef TR2
#  undef A1
#  undef A2
#  undef L1
#  undef Ls1
#  undef Ls3
#  undef TRL1
#  undef TRA1
#  undef N2
#  undef N3
#  undef COND0
#  undef EXTRA4b0
#  undef JMPKIND0
#  undef CCALL0
#  undef XOTHER
}

static Char* nameOfTagOp ( TagOp h )
{
   switch (h) {
      case Tag_PCast40:        return "PCast40";
      case Tag_PCast20:        return "PCast20";
      case Tag_PCast10:        return "PCast10";
      case Tag_PCast01:        return "PCast01";
      case Tag_PCast02:        return "PCast02";
      case Tag_PCast04:        return "PCast04";
      case Tag_PCast14:        return "PCast14";
      case Tag_PCast12:        return "PCast12";
      case Tag_PCast11:        return "PCast11";
      case Tag_Left4:          return "Left4";
      case Tag_Left2:          return "Left2";
      case Tag_Left1:          return "Left1";
      case Tag_SWiden14:       return "SWiden14";
      case Tag_SWiden24:       return "SWiden24";
      case Tag_SWiden12:       return "SWiden12";
      case Tag_ZWiden14:       return "ZWiden14";
      case Tag_ZWiden24:       return "ZWiden24";
      case Tag_ZWiden12:       return "ZWiden12";
      case Tag_UifU4:          return "UifU4";
      case Tag_UifU2:          return "UifU2";
      case Tag_UifU1:          return "UifU1";
      case Tag_UifU0:          return "UifU0";
      case Tag_DifD4:          return "DifD4";
      case Tag_DifD2:          return "DifD2";
      case Tag_DifD1:          return "DifD1";
      case Tag_ImproveAND4_TQ: return "ImproveAND4_TQ";
      case Tag_ImproveAND2_TQ: return "ImproveAND2_TQ";
      case Tag_ImproveAND1_TQ: return "ImproveAND1_TQ";
      case Tag_ImproveOR4_TQ:  return "ImproveOR4_TQ";
      case Tag_ImproveOR2_TQ:  return "ImproveOR2_TQ";
      case Tag_ImproveOR1_TQ:  return "ImproveOR1_TQ";
      case Tag_DebugFn:        return "DebugFn";
      default: VG_(skin_panic)("vg_nameOfTagOp");
   }
}


Char* SK_(name_XUOpcode)(Opcode opc)
{
   switch (opc) {
      case GETVF:   return "GETVF";
      case PUTVF:   return "PUTVF";
      case TAG1:    return "TAG1";
      case TAG2:    return "TAG2";
      case LOADV:   return "LOADV";
      case STOREV:  return "STOREV";
      case GETV:    return "GETV";
      case PUTV:    return "PUTV";
      case TESTV:   return "TESTV";
      case SETV:    return "SETV";
      default:      
         VG_(printf)("unhandled opcode: %u\n", opc);
         VG_(skin_panic)("SK_(name_XUOpcode): unhandled case");
   }
}

/* ---------------------------------------------------------------------
   Debugging stuff.
   ------------------------------------------------------------------ */

void SK_(pp_XUInstr)(UInstr* u)
{
   switch (u->opcode) {
      case TAG1:
         VG_(pp_UOperand)(u, 1, 4, False);
         VG_(printf)(" = %s ( ", nameOfTagOp( u->val3 ));
         VG_(pp_UOperand)(u, 1, 4, False);
         VG_(printf)(" )");
         break;

      case TAG2:
         VG_(pp_UOperand)(u, 2, 4, False);
         VG_(printf)(" = %s ( ", nameOfTagOp( u->val3 ));
         VG_(pp_UOperand)(u, 1, 4, False);
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, 4, False);
         VG_(printf)(" )");
         break;

      case STOREV: case LOADV:
         VG_(pp_UOperand)(u, 1, u->size, u->opcode==LOADV);
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, u->size, u->opcode==STOREV);
         break;

      case PUTVF: case GETVF:
         VG_(pp_UOperand)(u, 1, 0, False);
         break;

      case GETV: case PUTV:
         VG_(pp_UOperand)(u, 1, u->opcode==PUTV ? 4 : u->size, False);
         VG_(printf)(", ");
         VG_(pp_UOperand)(u, 2, u->opcode==GETV ? 4 : u->size, False);
         break;

      case TESTV: case SETV:
         VG_(pp_UOperand)(u, 1, u->size, False);
         break;

      default:
         VG_(printf)("unhandled opcode: %u\n", u->opcode);
         VG_(skin_panic)("SK_(pp_XUInstr): unhandled opcode");
   }

}

Int SK_(get_Xreg_usage)(UInstr* u, Tag tag, Int* regs, Bool* isWrites)
{
#  define RD(ono)    VG_UINSTR_READS_REG(ono, regs, isWrites)
#  define WR(ono)    VG_UINSTR_WRITES_REG(ono, regs, isWrites)

   Int n = 0;
   switch (u->opcode) {        
      case TAG1:    RD(1); WR(1);        break;
      case TAG2:    RD(1); RD(2); WR(2); break;
      case LOADV:   RD(1); WR(2);        break;
      case STOREV:  RD(1); RD(2);        break;
      case GETV:    WR(2);               break;
      case PUTV:    RD(1);               break;
      case TESTV:   RD(1);               break;
      case SETV:    WR(1);               break;
      case PUTVF:   RD(1);               break;
      case GETVF:   WR(1);               break;

      default: 
         VG_(printf)("unhandled opcode: %u\n", u->opcode);
         VG_(skin_panic)("SK_(get_Xreg_usage): unhandled opcode");
   }
   return n;

#  undef RD
#  undef WR
}

/*------------------------------------------------------------*/
/*--- New instrumentation machinery.                       ---*/
/*------------------------------------------------------------*/

static
TagOp get_Tag_ImproveOR_TQ ( Int sz )
{
   switch (sz) {
      case 4: return Tag_ImproveOR4_TQ;
      case 2: return Tag_ImproveOR2_TQ;
      case 1: return Tag_ImproveOR1_TQ;
      default: VG_(skin_panic)("get_Tag_ImproveOR_TQ");
   }
}


static
TagOp get_Tag_ImproveAND_TQ ( Int sz )
{
   switch (sz) {
      case 4: return Tag_ImproveAND4_TQ;
      case 2: return Tag_ImproveAND2_TQ;
      case 1: return Tag_ImproveAND1_TQ;
      default: VG_(skin_panic)("get_Tag_ImproveAND_TQ");
   }
}


static
TagOp get_Tag_Left ( Int sz )
{
   switch (sz) {
      case 4: return Tag_Left4;
      case 2: return Tag_Left2;
      case 1: return Tag_Left1;
      default: VG_(skin_panic)("get_Tag_Left");
   }
}


static
TagOp get_Tag_UifU ( Int sz )
{
   switch (sz) {
      case 4: return Tag_UifU4;
      case 2: return Tag_UifU2;
      case 1: return Tag_UifU1;
      case 0: return Tag_UifU0;
      default: VG_(skin_panic)("get_Tag_UifU");
   }
}


static
TagOp get_Tag_DifD ( Int sz )
{
   switch (sz) {
      case 4: return Tag_DifD4;
      case 2: return Tag_DifD2;
      case 1: return Tag_DifD1;
      default: VG_(skin_panic)("get_Tag_DifD");
   }
}


static 
TagOp get_Tag_PCast ( Int szs, Int szd )
{
   if (szs == 4 && szd == 0) return Tag_PCast40;
   if (szs == 2 && szd == 0) return Tag_PCast20;
   if (szs == 1 && szd == 0) return Tag_PCast10;
   if (szs == 0 && szd == 1) return Tag_PCast01;
   if (szs == 0 && szd == 2) return Tag_PCast02;
   if (szs == 0 && szd == 4) return Tag_PCast04;
   if (szs == 1 && szd == 4) return Tag_PCast14;
   if (szs == 1 && szd == 2) return Tag_PCast12;
   if (szs == 1 && szd == 1) return Tag_PCast11;
   VG_(printf)("get_Tag_PCast(%d,%d)\n", szs, szd);
   VG_(skin_panic)("get_Tag_PCast");
}


static 
TagOp get_Tag_Widen ( Bool syned, Int szs, Int szd )
{
   if (szs == 1 && szd == 2 && syned)  return Tag_SWiden12;
   if (szs == 1 && szd == 2 && !syned) return Tag_ZWiden12;

   if (szs == 1 && szd == 4 && syned)  return Tag_SWiden14;
   if (szs == 1 && szd == 4 && !syned) return Tag_ZWiden14;

   if (szs == 2 && szd == 4 && syned)  return Tag_SWiden24;
   if (szs == 2 && szd == 4 && !syned) return Tag_ZWiden24;

   VG_(printf)("get_Tag_Widen(%d,%d,%d)\n", (Int)syned, szs, szd);
   VG_(skin_panic)("get_Tag_Widen");
}

/* Pessimally cast the spec'd shadow from one size to another. */
static 
void create_PCast ( UCodeBlock* cb, Int szs, Int szd, Int tempreg )
{
   if (szs == 0 && szd == 0)
      return;
   uInstr3(cb, TAG1, 0, TempReg, tempreg, 
                        NoValue, 0, 
                        Lit16,   get_Tag_PCast(szs,szd));
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
                        Lit16,   get_Tag_Widen(signed_widen,szs,szd));
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
               Lit16, get_Tag_Left(sz));
}


/* Do UifU on ts and td, putting the result in td. */
static 
void create_UifU ( UCodeBlock* cb, Int sz, Int ts, Int td )
{
   uInstr3(cb, TAG2, 0, TempReg, ts, TempReg, td,
               Lit16, get_Tag_UifU(sz));
}


/* Do DifD on ts and td, putting the result in td. */
static 
void create_DifD ( UCodeBlock* cb, Int sz, Int ts, Int td )
{
   uInstr3(cb, TAG2, 0, TempReg, ts, TempReg, td,
               Lit16, get_Tag_DifD(sz));
}


/* Do HelpAND on value tval and tag tqqq, putting the result in
   tqqq. */
static 
void create_ImproveAND_TQ ( UCodeBlock* cb, Int sz, Int tval, Int tqqq )
{
   uInstr3(cb, TAG2, 0, TempReg, tval, TempReg, tqqq,
               Lit16, get_Tag_ImproveAND_TQ(sz));
}


/* Do HelpOR on value tval and tag tqqq, putting the result in
   tqqq. */
static 
void create_ImproveOR_TQ ( UCodeBlock* cb, Int sz, Int tval, Int tqqq )
{
   uInstr3(cb, TAG2, 0, TempReg, tval, TempReg, tqqq,
               Lit16, get_Tag_ImproveOR_TQ(sz));
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
   VG_(skin_panic)("getOperandShadow");
}

/* Create and return an instrumented version of cb_in.  Free cb_in
   before returning. */
static UCodeBlock* memcheck_instrument ( UCodeBlock* cb_in )
{
   UCodeBlock* cb;
   Int         i, j;
   UInstr*     u_in;
   Int         qs, qd, qt, qtt;
   Bool        bogusLiterals;

   cb = VG_(setup_UCodeBlock)(cb_in);

   /* Scan the block to look for bogus literals.  These are magic
      numbers which particularly appear in hand-optimised / inlined
      implementations of strlen() et al which cause so much trouble
      (spurious reports of uninit-var uses).  Purpose of this horrible
      hack is to disable some checks any such literals are present in
      this basic block. */
   bogusLiterals = False;

   if (MC_(clo_avoid_strlen_errors)) {
      for (i = 0; i < VG_(get_num_instrs)(cb_in); i++) {
         u_in = VG_(get_instr)(cb_in, i);
         switch (u_in->opcode) {
            case ADD: case SUB: case MOV: 
               if (u_in->size == 4 && u_in->tag1 == Literal)
                  goto literal;
               break;
            case LEA1: 
               sk_assert(u_in->size == 4);
               goto literal;
            default: 
               break;
         }     
         continue;
        literal:
         if (u_in->lit32 == 0xFEFEFEFF ||
             u_in->lit32 == 0x80808080 ||
             u_in->lit32 == 0x00008080) {
            bogusLiterals = True;
            break;
         }
      }
   }

   for (i = 0; i < VG_(get_num_instrs)(cb_in); i++) {
      u_in = VG_(get_instr)(cb_in, i);
      qs = qd = qt = qtt = INVALID_TEMPREG;

      switch (u_in->opcode) {

         case LOCK:
         case NOP:
            break;

         case INCEIP:
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* The segment registers do not have their definedness
            tracked.  We therefore make fake shadows on GETSEG and
            test them on PUTSEG.  This will catch writing garbage to a
            segment register; therefore we can assume it to be defined
            when read (GETSEGd).  Since the first arg of USESEG is
            fetched by GETSEG, we can assume it to be defined, and so
            the definedness of the result is simply the definedness of
            the second (virtual_address) arg of USESEG.  The upshot of
            all this is that instrumentation of USESEG is a no-op! */

         case PUTSEG:
            sk_assert(u_in->tag1 == TempReg);
            uInstr1(cb, TESTV, 2, TempReg, SHADOW(u_in->val1));
            uInstr1(cb, SETV,  2, TempReg, SHADOW(u_in->val1));
            VG_(copy_UInstr)(cb, u_in);
            break;

         case GETSEG:
            sk_assert(u_in->tag2 == TempReg);
            uInstr1(cb, SETV,  2, TempReg, SHADOW(u_in->val2));
            VG_(copy_UInstr)(cb, u_in);
            break;

         case USESEG:
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* Loads and stores.  Test the V bits for the address.  24
            Mar 02: since the address is A-checked anyway, there's not
            really much point in doing the V-check too, unless you
            think that you might use addresses which are undefined but
            still addressible.  Hence the optionalisation of the V
            check.  15 Dec 02: optionalisation removed, since it no
            longer makes much sense given we also have an addrcheck
            skin.

            The LOADV/STOREV does an addressibility check for the
            address. */

         case LOAD: 
            uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val1));
            uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val1));
            uInstr2(cb, LOADV, u_in->size, 
                        TempReg, u_in->val1,
                        TempReg, SHADOW(u_in->val2));
            VG_(copy_UInstr)(cb, u_in);
            break;

         case STORE:
            uInstr1(cb, TESTV,  4, TempReg, SHADOW(u_in->val2));
            uInstr1(cb, SETV,   4, TempReg, SHADOW(u_in->val2));
            uInstr2(cb, STOREV, u_in->size,
                        TempReg, SHADOW(u_in->val1), 
                        TempReg, u_in->val2);
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* Moving stuff around.  Make the V bits follow accordingly,
            but don't do anything else.  */

         case GET:
            uInstr2(cb, GETV, u_in->size,
                        ArchReg, u_in->val1,
                        TempReg, SHADOW(u_in->val2));
            VG_(copy_UInstr)(cb, u_in);
            break;

         case PUT:
            uInstr2(cb, PUTV, u_in->size, 
                        TempReg, SHADOW(u_in->val1),
                        ArchReg, u_in->val2);
            VG_(copy_UInstr)(cb, u_in);
            break;

         case GETF:
            /* This is not the smartest way to do it, but should work. */
            qd = create_GETVF(cb, u_in->size);
            uInstr2(cb, MOV, 4, TempReg, qd, TempReg, SHADOW(u_in->val1));
            VG_(copy_UInstr)(cb, u_in);
            break;

         case PUTF:
            create_PUTVF(cb, u_in->size, SHADOW(u_in->val1));
            VG_(copy_UInstr)(cb, u_in);
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
                  VG_(skin_panic)("memcheck_instrument: MOV");
            }
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* Special case of add, where one of the operands is a literal.
            lea1(t) = t + some literal.
            Therefore: lea1#(qa) = left(qa) 
         */
         case LEA1:
            sk_assert(u_in->size == 4 && !VG_(any_flag_use)(u_in));
            qs = SHADOW(u_in->val1);
            qd = SHADOW(u_in->val2);
            uInstr2(cb, MOV, 4, TempReg, qs, TempReg, qd);
            create_Left(cb, u_in->size, qd);
            VG_(copy_UInstr)(cb, u_in);
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
            sk_assert(u_in->size == 4 && !VG_(any_flag_use)(u_in));
            switch (u_in->extra4b) {
               case 1: shift = 0; break;
               case 2: shift = 1; break;
               case 4: shift = 2; break;
               case 8: shift = 3; break;
               default: VG_(skin_panic)( "memcheck_instrument(LEA2)" );
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
            VG_(copy_UInstr)(cb, u_in);
            break;
         }

         /* inc#/dec#(qd) = q `UifU` left(qd) = left(qd) */
         case INC: case DEC:
            qd = SHADOW(u_in->val1);
            create_Left(cb, u_in->size, qd);
            if (u_in->flags_w != FlagsEmpty)
               create_PUTVF(cb, u_in->size, qd);
            VG_(copy_UInstr)(cb, u_in);
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
	    sk_assert(u_in->flags_r != FlagsEmpty);
            /* The following assertion looks like it makes sense, but is
               actually wrong.  Consider this:
                  rcll    %eax
                  imull   %eax, %eax
               The rcll writes O and C but so does the imull, so the O and C 
               write of the rcll is annulled by the prior improvement pass.
               Noticed by Kevin Ryde <user42@zip.com.au>
            */
	    /* sk_assert(u_in->flags_w != FlagsEmpty); */
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
            VG_(copy_UInstr)(cb, u_in);
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
            sk_assert(u_in->tag1 == TempReg || u_in->tag1 == Literal);
            sk_assert(u_in->tag2 == TempReg);
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
            VG_(copy_UInstr)(cb, u_in);
            break;
         }

         /* One simple tag operation. */
         case WIDEN:
            sk_assert(u_in->tag1 == TempReg);
            create_Widen(cb, u_in->signed_widen, u_in->extra4b, u_in->size, 
                             SHADOW(u_in->val1));
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* not#(x) = x (since bitwise independent) */
         case NOT:
            sk_assert(u_in->tag1 == TempReg);
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* neg#(x) = left(x) (derivable from case for SUB) */
         case NEG:
            sk_assert(u_in->tag1 == TempReg);
            create_Left(cb, u_in->size, SHADOW(u_in->val1));
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* bswap#(x) = bswap(x) */
         case BSWAP:
            sk_assert(u_in->tag1 == TempReg);
            sk_assert(u_in->size == 4);
            qd = SHADOW(u_in->val1);
            uInstr1(cb, BSWAP, 4, TempReg, qd);
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* cc2val#(qd) = pcast-0-to-size(eflags#) */
         case CC2VAL:
            sk_assert(u_in->tag1 == TempReg);
            sk_assert(u_in->flags_r != FlagsEmpty);
            qt = create_GETVF(cb, u_in->size);
            uInstr2(cb, MOV, 4, TempReg, qt, TempReg, SHADOW(u_in->val1));
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* cmov#(qs,qd) = cmov(qs,qd)
            That is, do the cmov of tags using the same flags as for
            the data (obviously).  However, first do a test on the 
            validity of the flags.
         */
         case CMOV:
            sk_assert(u_in->size == 4);
            sk_assert(u_in->tag1 == TempReg);
            sk_assert(u_in->tag2 == TempReg);
            sk_assert(u_in->flags_r != FlagsEmpty);
            sk_assert(u_in->flags_w == FlagsEmpty);
            qs = SHADOW(u_in->val1);
            qd = SHADOW(u_in->val2);
            qt = create_GETVF(cb, 0);
            uInstr1(cb, TESTV, 0, TempReg, qt);
            /* qt should never be referred to again.  Nevertheless
               ... */
            uInstr1(cb, SETV, 0, TempReg, qt);

            uInstr2(cb, CMOV, 4, TempReg, qs, TempReg, qd);
            uCond(cb, u_in->cond);         
            uFlagsRWU(cb, u_in->flags_r, u_in->flags_w, FlagsEmpty);

            VG_(copy_UInstr)(cb, u_in);
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
         case MUL:
            qd = SHADOW(u_in->val2);
            qs = getOperandShadow(cb, u_in->size, u_in->tag1, u_in->val1);
            create_UifU(cb, u_in->size, qs, qd);
            create_Left(cb, u_in->size, qd);
            if (u_in->opcode == ADC || u_in->opcode == SBB) {
               sk_assert(u_in->flags_r != FlagsEmpty);
               qt = create_GETVF(cb, u_in->size);
               create_UifU(cb, u_in->size, qt, qd);
            }
            if (u_in->flags_w != FlagsEmpty) {
               create_PUTVF(cb, u_in->size, qd);
            }
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* xor#(qs,qd) = qs `UifU` qd */
         case XOR:
            qd = SHADOW(u_in->val2);
            qs = getOperandShadow(cb, u_in->size, u_in->tag1, u_in->val1);
            create_UifU(cb, u_in->size, qs, qd);
            if (u_in->flags_w != FlagsEmpty) {
               create_PUTVF(cb, u_in->size, qd);
            }
            VG_(copy_UInstr)(cb, u_in);
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
            sk_assert(u_in->tag1 == TempReg);
            sk_assert(u_in->tag2 == TempReg);
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
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* Machinery to do with supporting CALLM.  Copy the start and
            end markers only to make the result easier to read
            (debug); they generate no code and have no effect. 
         */
         case CALLM_S: case CALLM_E:
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* Copy PUSH and POP verbatim.  Arg/result absval
            calculations are done when the associated CALL is
            processed.  CLEAR has no effect on absval calculations but
            needs to be copied.  
         */
         case PUSH: case POP: case CLEAR:
            VG_(copy_UInstr)(cb, u_in);
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
            for (j = i-1; VG_(get_instr)(cb_in, j)->opcode != CALLM_S; j--) {
               uu = VG_(get_instr)(cb_in, j);
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
            for (j = i+1; VG_(get_instr)(cb_in, j)->opcode != CALLM_E; j++) {
               uu = VG_(get_instr)(cb_in, j);
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
            VG_(copy_UInstr)(cb, u_in);
            break;
         }
         /* Whew ... */

         case JMP:
            if (u_in->tag1 == TempReg) {
               uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val1));
               uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val1));
            } else {
               sk_assert(u_in->tag1 == Literal);
            }
            if (u_in->cond != CondAlways) {
               sk_assert(u_in->flags_r != FlagsEmpty);
               qt = create_GETVF(cb, 0);
               if (/* HACK */ bogusLiterals) {
                  if (0) 
                     VG_(printf)("ignore TESTV due to bogus literal\n");
               } else {
                  uInstr1(cb, TESTV, 0, TempReg, qt);
               }
               /* qt should never be referred to again.  Nevertheless
                  ... */
               uInstr1(cb, SETV, 0, TempReg, qt);
            }
            VG_(copy_UInstr)(cb, u_in);
            break;

         case JIFZ:
            uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val1));
            uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val1));
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* Emit a check on the address used.  The value loaded into the 
            FPU is checked by the call to fpu_{read/write}_check().  */
         case MMX2_MemRd: case MMX2_MemWr:
         case FPU_R: case FPU_W: {
            Int t_size = INVALID_TEMPREG;
            Bool is_load;

            if (u_in->opcode == MMX2_MemRd || u_in->opcode == MMX2_MemWr)
               sk_assert(u_in->size == 4 || u_in->size == 8);

            is_load = u_in->opcode==FPU_R || u_in->opcode==MMX2_MemRd;
            sk_assert(u_in->tag2 == TempReg);
            uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val2));
            uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val2));

            t_size = newTemp(cb);
            uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
            uLiteral(cb, u_in->size);
            uInstr2(cb, CCALL, 0, TempReg, u_in->val2, TempReg, t_size);
            uCCall(cb, is_load ? (Addr) & MC_(fpu_read_check) 
                               : (Addr) & MC_(fpu_write_check),
                   2, 2, False);

            VG_(copy_UInstr)(cb, u_in);
            break;
         }

	 /* SSE ins referencing scalar integer registers */
	 case SSE3g_RegWr:
         case SSE3e_RegRd:
         case SSE3e_RegWr: 
         case SSE3g1_RegWr:
         case SSE3e1_RegRd:
            sk_assert(u_in->tag3 == TempReg);

            if (u_in->opcode == SSE3e1_RegRd) {
               sk_assert(u_in->size == 2);
            } else {
               sk_assert(u_in->size == 4);
            }

            /* Is it a read ?  Better check the V bits right now. */
            if ( u_in->opcode == SSE3e_RegRd
                 || u_in->opcode == SSE3e1_RegRd )
	       uInstr1(cb, TESTV, u_in->size, 
                           TempReg, SHADOW(u_in->val3));

	    /* And for both read and write, set the register to be
	       defined. */
	    uInstr1(cb, SETV, u_in->size, 
                        TempReg, SHADOW(u_in->val3));

            VG_(copy_UInstr)(cb, u_in);
            break;

         /* ... and the same deal for SSE insns referencing memory */
         case SSE3a_MemRd:
         case SSE3a_MemWr:
         case SSE2a_MemWr:
         case SSE2a_MemRd:
         case SSE3a1_MemRd: { 
            Bool is_load;
            Int t_size;

            sk_assert(u_in->size == 4 
                      || u_in->size == 8 || u_in->size == 16);

            t_size = INVALID_TEMPREG;
            is_load = u_in->opcode==SSE2a_MemRd 
                      || u_in->opcode==SSE3a_MemRd
                      || u_in->opcode==SSE3a1_MemRd;

            sk_assert(u_in->tag3 == TempReg);

	    uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val3));
	    uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val3));
	    t_size = newTemp(cb);
	    uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, u_in->size);
	    uInstr2(cb, CCALL, 0, TempReg, u_in->val3, TempReg, t_size);
	    uCCall(cb, is_load ? (Addr) & MC_(fpu_read_check) 
		   : (Addr) & MC_(fpu_write_check),
		   2, 2, False);

            VG_(copy_UInstr)(cb, u_in);
            break;
         }

	 case SSE3ag_MemRd_RegWr:
	 {
	    Int t_size;

            sk_assert(u_in->size == 4 || u_in->size == 8);
	    sk_assert(u_in->tag1 == TempReg);
	    uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val1));
	    uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val1));
            t_size = newTemp(cb);
	    uInstr2(cb, MOV, 4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, u_in->size);
            uInstr2(cb, CCALL, 0, TempReg, u_in->val1, TempReg, t_size);
            uCCall(cb, (Addr) MC_(fpu_read_check), 2, 2, False );
	    uInstr1(cb, SETV, 4, TempReg, SHADOW(u_in->val2));
            VG_(copy_UInstr)(cb, u_in);
	    break;
         }

         /* For FPU, MMX and SSE insns not referencing memory, just
            copy thru. */
         case SSE5: case SSE4: case SSE3:
         case MMX1: case MMX2: case MMX3:
         case FPU: 
            VG_(copy_UInstr)(cb, u_in);
            break;

         /* Since we don't track definedness of values inside the
            MMX state, we'd better check that the (int) reg being
            read here is defined. */
         case MMX2_ERegRd: 
            sk_assert(u_in->tag2 == TempReg);
            sk_assert(u_in->size == 4);
            uInstr1(cb, TESTV, 4, TempReg, SHADOW(u_in->val2));
            uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val2));
            VG_(copy_UInstr)(cb, u_in);
            break;

	 /* The MMX register is assumed to be fully defined, so
	    that's what this register becomes. */
         case MMX2_ERegWr:
            sk_assert(u_in->tag2 == TempReg);
            sk_assert(u_in->size == 4);
            uInstr1(cb, SETV,  4, TempReg, SHADOW(u_in->val2));
            VG_(copy_UInstr)(cb, u_in);	 
            break;

         default:
            VG_(pp_UInstr)(0, u_in);
            VG_(skin_panic)( "memcheck_instrument: unhandled case");

      } /* end of switch (u_in->opcode) */

   } /* end of for loop */

   VG_(free_UCodeBlock)(cb_in);
   return cb;
}

/*------------------------------------------------------------*/
/*--- Clean up mem check instrumentation.                  ---*/
/*------------------------------------------------------------*/

#define dis    VG_(print_codegen)


#define VGC_IS_SHADOW(tempreg) ((tempreg % 2) == 1)
#define VGC_UNDEF ((UChar)100)
#define VGC_VALUE ((UChar)101)

#define NOP_no_msg(uu)                                            \
   do { VG_(new_NOP)(uu); } while (False)

#define NOP_tag1_op(uu)                                           \
   do { VG_(new_NOP)(uu);                                         \
        if (dis)                                                  \
           VG_(printf)("   at %2d: delete %s due to defd arg\n",  \
                       i, nameOfTagOp(u->val3));                  \
   } while (False)

#define SETV_tag1_op(uu,newsz)                                    \
   do { uu->opcode = SETV;                                        \
        uu->size = newsz;                                         \
        uu->tag2 = uu->tag3 = NoValue;                            \
        if (dis)                                                  \
           VG_(printf)("   at %2d: convert %s to SETV%d "         \
                       "due to defd arg\n",                       \
                       i, nameOfTagOp(u->val3), newsz);           \
   } while (False)



/* Run backwards and delete SETVs on shadow temps for which the next
   action is a write.  Needs an env saying whether or not the next
   action is a write.  The supplied UCodeBlock is destructively
   modified.
*/
static void vg_delete_redundant_SETVs ( UCodeBlock* cb )
{
   Int     i, j, k;
   Int     n_temps = VG_(get_num_temps)(cb);
   Bool*   next_is_write;
   UInstr* u;
   Int     tempUse[VG_MAX_REGS_USED];
   Bool    isWrites[VG_MAX_REGS_USED];

   if (n_temps == 0) return;

   next_is_write = VG_(malloc)(n_temps * sizeof(Bool));

   for (i = 0; i < n_temps; i++) next_is_write[i] = True;

   for (i = VG_(get_num_instrs)(cb) - 1; i >= 0; i--) {
      u = VG_(get_instr)(cb, i);

      /* Occasionally there will be GETVs, TAG1s and TAG2s calculating
         values which are never used.  These first three cases get rid
         of them. */

      if (u->opcode == GETV && VGC_IS_SHADOW(u->val2) 
                            && next_is_write[u->val2]) {
         sk_assert(u->val2 < n_temps);
         VG_(new_NOP)(u);
         if (dis) 
            VG_(printf)("   at %2d: delete GETV\n", i);
      } else

      if (u->opcode == TAG1 && VGC_IS_SHADOW(u->val1) 
                            && next_is_write[u->val1]) {
         sk_assert(u->val1 < n_temps);
         VG_(new_NOP)(u);
         if (dis) 
            VG_(printf)("   at %2d: delete TAG1\n", i);
      } else

      if (u->opcode == TAG2 && VGC_IS_SHADOW(u->val2) 
                            && next_is_write[u->val2]) {
         sk_assert(u->val2 < n_temps);
         VG_(new_NOP)(u);
         if (dis) 
            VG_(printf)("   at %2d: delete TAG2\n", i);
      } else

      /* The bulk of the cleanup work of this function is done by
         the code from here downwards. */

      if (u->opcode == MOV && VGC_IS_SHADOW(u->val2) 
                           && next_is_write[u->val2]) {
         /* This MOV is pointless because the target is dead at this
            point.  Delete it. */
         VG_(new_NOP)(u);
         if (dis) 
            VG_(printf)("   at %2d: delete MOV\n", i);
      } else

      if (u->opcode == SETV) {
         if (u->tag1 == TempReg) {
            sk_assert(VGC_IS_SHADOW(u->val1));
            if (next_is_write[u->val1]) {
               /* This write is pointless, so annul it. */
               VG_(new_NOP)(u);
               if (dis) 
                  VG_(printf)("   at %2d: delete SETV\n", i);
            } else {
               /* This write has a purpose; don't annul it, but do
                  notice that we did it. */
               next_is_write[u->val1] = True;
            }
              
         }

      } else {
         /* Find out what this insn does to the temps. */
         k = VG_(get_reg_usage)(u, TempReg, &tempUse[0], &isWrites[0]);
         sk_assert(0 <= k && k <= VG_MAX_REGS_USED);
         for (j = k-1; j >= 0; j--) {
            next_is_write[ tempUse[j] ] = isWrites[j];
         }
      }
   }
}


/* Run forwards, propagating and using the is-completely-defined
   property.  This removes a lot of redundant tag-munging code.
   Unfortunately it requires intimate knowledge of how each uinstr and
   tagop modifies its arguments.  This duplicates knowledge of uinstr
   tempreg uses embodied in VG_(get_reg_usage)(), which is unfortunate. 
   The supplied UCodeBlock* is modified in-place.

   For each value temp, def[] should hold VGC_VALUE.

   For each shadow temp, def[] may hold 4,2,1 or 0 iff that shadow is
   definitely known to be fully defined at that size.  In all other
   circumstances a shadow's def[] entry is VGC_UNDEF, meaning possibly
   undefined.  In cases of doubt, VGC_UNDEF is always safe.
*/
static void vg_propagate_definedness ( UCodeBlock* cb )
{
   Int     i, j, k, t;
   Int     n_temps = VG_(get_num_temps)(cb);
   UChar*  def;
   UInstr* u;
   Int     tempUse[VG_MAX_REGS_USED];
   Bool    isWrites[VG_MAX_REGS_USED];

   if (n_temps == 0) return;

   def = VG_(malloc)(n_temps * sizeof(UChar));

   for (i = 0; i < n_temps; i++) 
      def[i] = VGC_IS_SHADOW(i) ? VGC_UNDEF : VGC_VALUE;

   /* Run forwards, detecting and using the all-defined property. */

   for (i = 0; i < VG_(get_num_instrs)(cb); i++) {
      u = VG_(get_instr)(cb, i);
      switch (u->opcode) {

      /* Tag-handling uinstrs. */

         /* Deal with these quickly. */
         case NOP:
         case LOCK:
         case INCEIP:
            break;

         /* Make a tag defined. */
         case SETV:
            sk_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            def[u->val1] = u->size;
            break;

         /* Check definedness of a tag. */
         case TESTV:
            sk_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            if (def[u->val1] <= 4) { 
               sk_assert(def[u->val1] == u->size); 
               NOP_no_msg(u);
               if (dis) 
                  VG_(printf)("   at %2d: delete TESTV on defd arg\n", i);
            }
            break;

         /* Applies to both values and tags.  Propagate Definedness
            property through copies.  Note that this isn't optional;
            we *have* to do this to keep def[] correct. */
         case MOV:
            sk_assert(u->tag2 == TempReg);
            if (u->tag1 == TempReg) {
               if (VGC_IS_SHADOW(u->val1)) {
                  sk_assert(VGC_IS_SHADOW(u->val2));
                  def[u->val2] = def[u->val1];
               }
            }
            break;

         case PUTV:
            sk_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            if (def[u->val1] <= 4) {
               sk_assert(def[u->val1] == u->size);
               u->tag1 = Literal;
               u->val1 = 0;
               switch (u->size) {
                  case 4: u->lit32 = 0x00000000; break;
                  case 2: u->lit32 = 0xFFFF0000; break;
                  case 1: u->lit32 = 0xFFFFFF00; break;
                  default: VG_(skin_panic)("vg_cleanup(PUTV)");
               }
               if (dis) 
                  VG_(printf)(
                     "   at %2d: propagate definedness into PUTV\n", i);
            }
            break;

         case STOREV:
            sk_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            if (def[u->val1] <= 4) {
               sk_assert(def[u->val1] == u->size);
               u->tag1 = Literal;
               u->val1 = 0;
               switch (u->size) {
                  case 4: u->lit32 = 0x00000000; break;
                  case 2: u->lit32 = 0xFFFF0000; break;
                  case 1: u->lit32 = 0xFFFFFF00; break;
                  default: VG_(skin_panic)("vg_cleanup(STOREV)");
               }
               if (dis) 
                  VG_(printf)(
                     "   at %2d: propagate definedness into STandV\n", i);
            }
            break;

         /* Nothing interesting we can do with this, I think. */
         case PUTVF:
            break;

         /* Tag handling operations. */
         case TAG2:
            sk_assert(u->tag2 == TempReg && VGC_IS_SHADOW(u->val2));
            sk_assert(u->tag3 == Lit16);
            /* Ultra-paranoid "type" checking. */
            switch (u->val3) {
               case Tag_ImproveAND4_TQ: case Tag_ImproveAND2_TQ:
               case Tag_ImproveAND1_TQ: case Tag_ImproveOR4_TQ:
               case Tag_ImproveOR2_TQ: case Tag_ImproveOR1_TQ:
                  sk_assert(u->tag1 == TempReg && !VGC_IS_SHADOW(u->val1));
                  break;
               default:
                  sk_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
                  break;
            }
            switch (u->val3) {
               Int sz;
               case Tag_UifU4: 
                  sz = 4; goto do_UifU;
               case Tag_UifU2: 
                  sz = 2; goto do_UifU;
               case Tag_UifU1:
                  sz = 1; goto do_UifU;
               case Tag_UifU0:
                  sz = 0; goto do_UifU;
               do_UifU:
                  sk_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
                  sk_assert(u->tag2 == TempReg && VGC_IS_SHADOW(u->val2));
                  if (def[u->val1] <= 4) {
                     /* UifU.  The first arg is defined, so result is
                        simply second arg.  Delete this operation. */
                     sk_assert(def[u->val1] == sz);
                     NOP_no_msg(u);
                     if (dis) 
                        VG_(printf)(
                           "   at %2d: delete UifU%d due to defd arg1\n", 
                           i, sz);
                  }
                  else 
                  if (def[u->val2] <= 4) {
                     /* UifU.  The second arg is defined, so result is
                        simply first arg.  Copy to second. */
                     sk_assert(def[u->val2] == sz);
                     u->opcode = MOV; 
                     u->size = 4;
                     u->tag3 = NoValue;
                     def[u->val2] = def[u->val1];
                     if (dis) 
                        VG_(printf)(
                           "   at %2d: change UifU%d to MOV due to defd"
                           " arg2\n", 
                           i, sz);
                  }
                  break;
               case Tag_ImproveAND4_TQ:
                  sz = 4; goto do_ImproveAND;
               case Tag_ImproveAND1_TQ:
                  sz = 1; goto do_ImproveAND;
               do_ImproveAND:
                  /* Implements Q = T AND Q.  So if Q is entirely defined,
                     ie all 0s, we get MOV T, Q. */
		  if (def[u->val2] <= 4) {
                     sk_assert(def[u->val2] == sz);
                     u->size = 4; /* Regardless of sz */
                     u->opcode = MOV;
                     u->tag3 = NoValue;
                     def[u->val2] = VGC_UNDEF;
                     if (dis) 
                        VG_(printf)(
                            "   at %2d: change ImproveAND%d_TQ to MOV due "
                            "to defd arg2\n", 
                            i, sz);
                  }
                  break;
               default: 
                  goto unhandled;
            }
            break;

         case TAG1:
            sk_assert(u->tag1 == TempReg && VGC_IS_SHADOW(u->val1));
            if (def[u->val1] > 4) break;
            /* We now know that the arg to the op is entirely defined.
               If the op changes the size of the arg, we must replace
               it with a SETV at the new size.  If it doesn't change
               the size, we can delete it completely. */
            switch (u->val3) {
               /* Maintain the same size ... */
               case Tag_Left4: 
                  sk_assert(def[u->val1] == 4);
                  NOP_tag1_op(u);
                  break;
               case Tag_PCast11: 
                  sk_assert(def[u->val1] == 1);
                  NOP_tag1_op(u);
                  break;
               /* Change size ... */
               case Tag_PCast40: 
                  sk_assert(def[u->val1] == 4);
                  SETV_tag1_op(u,0);
                  def[u->val1] = 0;
                  break;
               case Tag_PCast14: 
                  sk_assert(def[u->val1] == 1);
                  SETV_tag1_op(u,4);
                  def[u->val1] = 4;
                  break;
               case Tag_PCast12: 
                  sk_assert(def[u->val1] == 1);
                  SETV_tag1_op(u,2);
                  def[u->val1] = 2;
                  break;
               case Tag_PCast10: 
                  sk_assert(def[u->val1] == 1);
                  SETV_tag1_op(u,0);
                  def[u->val1] = 0;
                  break;
               case Tag_PCast02: 
                  sk_assert(def[u->val1] == 0);
                  SETV_tag1_op(u,2);
                  def[u->val1] = 2;
                  break;
               default: 
                  goto unhandled;
            }
            if (dis) 
               VG_(printf)(
                  "   at %2d: delete TAG1 %s due to defd arg\n",
                  i, nameOfTagOp(u->val3));
            break;

         default:
         unhandled:
            /* We don't know how to handle this uinstr.  Be safe, and 
               set to VGC_VALUE or VGC_UNDEF all temps written by it. */
            k = VG_(get_reg_usage)(u, TempReg, &tempUse[0], &isWrites[0]);
            sk_assert(0 <= k && k <= VG_MAX_REGS_USED);
            for (j = 0; j < k; j++) {
               t = tempUse[j];
               sk_assert(t >= 0 && t < n_temps);
               if (!isWrites[j]) {
                  /* t is read; ignore it. */
                  if (0&& VGC_IS_SHADOW(t) && def[t] <= 4)
                     VG_(printf)("ignoring def %d at %s %s\n", 
                                 def[t], 
                                 VG_(name_UOpcode)(True, u->opcode),
                                 (u->opcode == TAG1 || u->opcode == TAG2)
                                    ? nameOfTagOp(u->val3) 
                                    : (Char*)"");
               } else {
                  /* t is written; better nullify it. */
                  def[t] = VGC_IS_SHADOW(t) ? VGC_UNDEF : VGC_VALUE;
               }
            }
      }
   }
}


/* Top level post-MemCheck-instrumentation cleanup function. */
static void vg_cleanup ( UCodeBlock* cb )
{
   vg_propagate_definedness ( cb );
   vg_delete_redundant_SETVs ( cb );
}


/* Caller will print out final instrumented code if necessary;  we
   print out intermediate instrumented code here if necessary. */
UCodeBlock* SK_(instrument) ( UCodeBlock* cb, Addr not_used )
{
   cb = memcheck_instrument ( cb );
   if (MC_(clo_cleanup)) {
      if (dis) {
         VG_(pp_UCodeBlock) ( cb, "Unimproved instrumented UCode:" );
         VG_(printf)("Instrumentation improvements:\n");
      }
      vg_cleanup(cb);
      if (dis) VG_(printf)("\n");
   }
   return cb;
}

#undef dis

/*--------------------------------------------------------------------*/
/*--- end                                           mc_translate.c ---*/
/*--------------------------------------------------------------------*/
