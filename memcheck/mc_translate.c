
/*--------------------------------------------------------------------*/
/*--- Instrument IR to perform memory checking operations.         ---*/
/*---                                               mc_translate.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

#include "mc_include.h"


/*------------------------------------------------------------*/
/*--- Forward decls                                        ---*/
/*------------------------------------------------------------*/

struct _MCEnv;

static IRType  shadowType ( IRType ty );
static IRExpr* expr2vbits ( struct _MCEnv* mce, IRExpr* e );


/*------------------------------------------------------------*/
/*--- Memcheck running state, and tmp management.          ---*/
/*------------------------------------------------------------*/

/* Carries around state during memcheck instrumentation. */
typedef
   struct _MCEnv {
      /* MODIFIED: the bb being constructed.  IRStmts are added. */
      IRBB* bb;

      /* MODIFIED: a table [0 .. #temps_in_original_bb-1] which maps
         original temps to their current their current shadow temp.
         Initially all entries are IRTemp_INVALID.  Entries are added
         lazily since many original temps are not used due to
         optimisation prior to instrumentation.  Note that floating
         point original tmps are shadowed by integer tmps of the same
         size, and Bit-typed original tmps are shadowed by the type
         Ity_I8.  See comment below. */
      IRTemp* tmpMap;
      Int     n_originalTmps; /* for range checking */

      /* READONLY: the guest layout.  This indicates which parts of
         the guest state should be regarded as 'always defined'. */
      VexGuestLayout* layout;
      /* READONLY: the host word type.  Needed for constructing
         arguments of type 'HWord' to be passed to helper functions.
         Ity_I32 or Ity_I64 only. */
      IRType hWordTy;
   }
   MCEnv;

/* SHADOW TMP MANAGEMENT.  Shadow tmps are allocated lazily (on
   demand), as they are encountered.  This is for two reasons.

   (1) (less important reason): Many original tmps are unused due to
   initial IR optimisation, and we do not want to spaces in tables
   tracking them.

   Shadow IRTemps are therefore allocated on demand.  mce.tmpMap is a
   table indexed [0 .. n_types-1], which gives the current shadow for
   each original tmp, or INVALID_IRTEMP if none is so far assigned.
   It is necessary to support making multiple assignments to a shadow
   -- specifically, after testing a shadow for definedness, it needs
   to be made defined.  But IR's SSA property disallows this.  

   (2) (more important reason): Therefore, when a shadow needs to get
   a new value, a new temporary is created, the value is assigned to
   that, and the tmpMap is updated to reflect the new binding.

   A corollary is that if the tmpMap maps a given tmp to
   INVALID_IRTEMP and we are hoping to read that shadow tmp, it means
   there's a read-before-write error in the original tmps.  The IR
   sanity checker should catch all such anomalies, however.  
*/

/* Find the tmp currently shadowing the given original tmp.  If none
   so far exists, allocate one.  */
static IRTemp findShadowTmp ( MCEnv* mce, IRTemp orig )
{
   tl_assert(orig < mce->n_originalTmps);
   if (mce->tmpMap[orig] == IRTemp_INVALID) {
      mce->tmpMap[orig] 
         = newIRTemp(mce->bb->tyenv, 
                     shadowType(mce->bb->tyenv->types[orig]));
   }
   return mce->tmpMap[orig];
}

/* Allocate a new shadow for the given original tmp.  This means any
   previous shadow is abandoned.  This is needed because it is
   necessary to give a new value to a shadow once it has been tested
   for undefinedness, but unfortunately IR's SSA property disallows
   this.  Instead we must abandon the old shadow, allocate a new one
   and use that instead. */
static void newShadowTmp ( MCEnv* mce, IRTemp orig )
{
   tl_assert(orig < mce->n_originalTmps);
   mce->tmpMap[orig] 
      = newIRTemp(mce->bb->tyenv, 
                  shadowType(mce->bb->tyenv->types[orig]));
}


/*------------------------------------------------------------*/
/*--- IRAtoms -- a subset of IRExprs                       ---*/
/*------------------------------------------------------------*/

/* An atom is either an IRExpr_Const or an IRExpr_Tmp, as defined by
   isAtom() in libvex_ir.h.  Because this instrumenter expects flat
   input, most of this code deals in atoms.  Usefully, a value atom
   always has a V-value which is also an atom: constants are shadowed
   by constants, and temps are shadowed by the corresponding shadow
   temporary. */

typedef  IRExpr  IRAtom;

/* (used for sanity checks only): is this an atom which looks
   like it's from original code? */
static Bool isOriginalAtom ( MCEnv* mce, IRAtom* a1 )
{
   if (a1->tag == Iex_Const)
      return True;
   if (a1->tag == Iex_Tmp && a1->Iex.Tmp.tmp < mce->n_originalTmps)
      return True;
   return False;
}

/* (used for sanity checks only): is this an atom which looks
   like it's from shadow code? */
static Bool isShadowAtom ( MCEnv* mce, IRAtom* a1 )
{
   if (a1->tag == Iex_Const)
      return True;
   if (a1->tag == Iex_Tmp && a1->Iex.Tmp.tmp >= mce->n_originalTmps)
      return True;
   return False;
}

/* (used for sanity checks only): check that both args are atoms and
   are identically-kinded. */
static Bool sameKindedAtoms ( IRAtom* a1, IRAtom* a2 )
{
   if (a1->tag == Iex_Tmp && a1->tag == Iex_Tmp)
      return True;
   if (a1->tag == Iex_Const && a1->tag == Iex_Const)
      return True;
   return False;
}


/*------------------------------------------------------------*/
/*--- Type management                                      ---*/
/*------------------------------------------------------------*/

/* Shadow state is always accessed using integer types.  This returns
   an integer type with the same size (as per sizeofIRType) as the
   given type.  The only valid shadow types are Bit, I8, I16, I32,
   I64. */

static IRType shadowType ( IRType ty )
{
   switch (ty) {
      case Ity_I1:
      case Ity_I8:
      case Ity_I16:
      case Ity_I32: 
      case Ity_I64: return ty;
      case Ity_F32: return Ity_I32;
      case Ity_F64: return Ity_I64;
      default: ppIRType(ty); 
               VG_(tool_panic)("memcheck:shadowType");
   }
}

/* Produce a 'defined' value of the given shadow type.  Should only be
   supplied shadow types (Bit/I8/I16/I32/UI64). */
static IRExpr* definedOfType ( IRType ty ) {
   switch (ty) {
      case Ity_I1:  return IRExpr_Const(IRConst_U1(False));
      case Ity_I8:  return IRExpr_Const(IRConst_U8(0));
      case Ity_I16: return IRExpr_Const(IRConst_U16(0));
      case Ity_I32: return IRExpr_Const(IRConst_U32(0));
      case Ity_I64: return IRExpr_Const(IRConst_U64(0));
      default:      VG_(tool_panic)("memcheck:definedOfType");
   }
}


/*------------------------------------------------------------*/
/*--- Constructing IR fragments                            ---*/
/*------------------------------------------------------------*/

/* assign value to tmp */
#define assign(_bb,_tmp,_expr)   \
   addStmtToIRBB((_bb), IRStmt_Tmp((_tmp),(_expr)))

/* add stmt to a bb */
#define stmt(_bb,_stmt)    \
   addStmtToIRBB((_bb), (_stmt))

/* build various kinds of expressions */
#define binop(_op, _arg1, _arg2) IRExpr_Binop((_op),(_arg1),(_arg2))
#define unop(_op, _arg)          IRExpr_Unop((_op),(_arg))
#define mkU8(_n)                 IRExpr_Const(IRConst_U8(_n))
#define mkU16(_n)                IRExpr_Const(IRConst_U16(_n))
#define mkU32(_n)                IRExpr_Const(IRConst_U32(_n))
#define mkU64(_n)                IRExpr_Const(IRConst_U64(_n))
#define mkexpr(_tmp)             IRExpr_Tmp((_tmp))

/* bind the given expression to a new temporary, and return the
   temporary.  This effectively converts an arbitrary expression into
   an atom. */
static IRAtom* assignNew ( MCEnv* mce, IRType ty, IRExpr* e ) {
   IRTemp t = newIRTemp(mce->bb->tyenv, ty);
   assign(mce->bb, t, e);
   return mkexpr(t);
}


/*------------------------------------------------------------*/
/*--- Constructing definedness primitive ops               ---*/
/*------------------------------------------------------------*/

/* --------- Defined-if-either-defined --------- */

static IRAtom* mkDifD8 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew(mce, Ity_I8, binop(Iop_And8, a1, a2));
}

static IRAtom* mkDifD16 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew(mce, Ity_I16, binop(Iop_And16, a1, a2));
}

static IRAtom* mkDifD32 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew(mce, Ity_I32, binop(Iop_And32, a1, a2));
}

/* --------- Undefined-if-either-undefined --------- */

static IRAtom* mkUifU8 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew(mce, Ity_I8, binop(Iop_Or8, a1, a2));
}

static IRAtom* mkUifU16 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew(mce, Ity_I16, binop(Iop_Or16, a1, a2));
}

static IRAtom* mkUifU32 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew(mce, Ity_I32, binop(Iop_Or32, a1, a2));
}

static IRAtom* mkUifU64 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew(mce, Ity_I64, binop(Iop_Or64, a1, a2));
}

static IRAtom* mkUifU ( MCEnv* mce, IRType vty,  IRAtom* a1, IRAtom* a2 ) {
   switch (vty) {
      case Ity_I16: return mkUifU16(mce, a1, a2);
      case Ity_I32: return mkUifU32(mce, a1, a2);
      case Ity_I64: return mkUifU64(mce, a1, a2);
      default:
         VG_(printf)("\n"); ppIRType(vty); VG_(printf)("\n");
         VG_(tool_panic)("memcheck:mkUifU");
   }
}

/* --------- The Left-family of operations. --------- */

static IRAtom* mkLeft8 ( MCEnv* mce, IRAtom* a1 ) {
   tl_assert(isShadowAtom(mce,a1));
   /* It's safe to duplicate a1 since it's only an atom */
   return assignNew(mce, Ity_I8, 
                    binop(Iop_Or8, a1, 
                          assignNew(mce, Ity_I8,
                                    /* unop(Iop_Neg8, a1)))); */
                                    binop(Iop_Sub8, mkU8(0), a1) )));
}

static IRAtom* mkLeft16 ( MCEnv* mce, IRAtom* a1 ) {
   tl_assert(isShadowAtom(mce,a1));
   /* It's safe to duplicate a1 since it's only an atom */
   return assignNew(mce, Ity_I16, 
                    binop(Iop_Or16, a1, 
                          assignNew(mce, Ity_I16,
                                    /* unop(Iop_Neg16, a1)))); */
                                    binop(Iop_Sub16, mkU16(0), a1) )));
}

static IRAtom* mkLeft32 ( MCEnv* mce, IRAtom* a1 ) {
   tl_assert(isShadowAtom(mce,a1));
   /* It's safe to duplicate a1 since it's only an atom */
   return assignNew(mce, Ity_I32, 
                    binop(Iop_Or32, a1, 
                          assignNew(mce, Ity_I32,
                                    /* unop(Iop_Neg32, a1)))); */
                                    binop(Iop_Sub32, mkU32(0), a1) )));
}

/* --------- 'Improvement' functions for AND/OR. --------- */

/* ImproveAND(data, vbits) = data OR vbits.  Defined (0) data 0s give
   defined (0); all other -> undefined (1).
*/
static IRAtom* mkImproveAND8 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(mce, Ity_I8, binop(Iop_Or8, data, vbits));
}

static IRAtom* mkImproveAND16 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(mce, Ity_I16, binop(Iop_Or16, data, vbits));
}

static IRAtom* mkImproveAND32 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(mce, Ity_I32, binop(Iop_Or32, data, vbits));
}

/* ImproveOR(data, vbits) = ~data OR vbits.  Defined (0) data 1s give
   defined (0); all other -> undefined (1).
*/
static IRAtom* mkImproveOR8 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             mce, Ity_I8, 
             binop(Iop_Or8, 
                   assignNew(mce, Ity_I8, unop(Iop_Not8, data)), 
                   vbits) );
}

static IRAtom* mkImproveOR16 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             mce, Ity_I16, 
             binop(Iop_Or16, 
                   assignNew(mce, Ity_I16, unop(Iop_Not16, data)), 
                   vbits) );
}

static IRAtom* mkImproveOR32 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             mce, Ity_I32, 
             binop(Iop_Or32, 
                   assignNew(mce, Ity_I32, unop(Iop_Not32, data)), 
                   vbits) );
}

/* --------- Pessimising casts. --------- */

static IRAtom* mkPCastTo( MCEnv* mce, IRType dst_ty, IRAtom* vbits ) 
{
   IRType  ty;
   IRAtom* tmp1;
   /* Note, dst_ty is a shadow type, not an original type. */
   /* First of all, collapse vbits down to a single bit. */
   tl_assert(isShadowAtom(mce,vbits));
   ty   = typeOfIRExpr(mce->bb->tyenv, vbits);
   tmp1 = NULL;
   switch (ty) {
      case Ity_I1:
         tmp1 = vbits;
         break;
      case Ity_I8: 
         tmp1 = assignNew(mce, Ity_I1, binop(Iop_CmpNE8, vbits, mkU8(0)));
         break;
      case Ity_I16: 
         tmp1 = assignNew(mce, Ity_I1, binop(Iop_CmpNE16, vbits, mkU16(0)));
         break;
      case Ity_I32: 
         tmp1 = assignNew(mce, Ity_I1, binop(Iop_CmpNE32, vbits, mkU32(0)));
         break;
      case Ity_I64: 
         tmp1 = assignNew(mce, Ity_I1, binop(Iop_CmpNE64, vbits, mkU64(0)));
         break;
      default:
         VG_(tool_panic)("mkPCastTo(1)");
   }
   tl_assert(tmp1);
   /* Now widen up to the dst type. */
   switch (dst_ty) {
      case Ity_I1:
         return tmp1;
      case Ity_I8: 
         return assignNew(mce, Ity_I8, unop(Iop_1Sto8, tmp1));
      case Ity_I16: 
         return assignNew(mce, Ity_I16, unop(Iop_1Sto16, tmp1));
      case Ity_I32: 
         return assignNew(mce, Ity_I32, unop(Iop_1Sto32, tmp1));
      case Ity_I64: 
         return assignNew(mce, Ity_I64, unop(Iop_1Sto64, tmp1));
      default: 
         ppIRType(dst_ty);
         VG_(tool_panic)("mkPCastTo(2)");
   }
}


/*------------------------------------------------------------*/
/*--- Emit a test and complaint if something is undefined. ---*/
/*------------------------------------------------------------*/

/* Set the annotations on a dirty helper to indicate that the stack
   pointer and instruction pointers might be read.  This is the
   behaviour of all 'emit-a-complaint' style functions we might
   call. */

static void setHelperAnns ( MCEnv* mce, IRDirty* di ) {
   di->nFxState = 2;
   di->fxState[0].fx     = Ifx_Read;
   di->fxState[0].offset = mce->layout->offset_SP;
   di->fxState[0].size   = mce->layout->sizeof_SP;
   di->fxState[1].fx     = Ifx_Read;
   di->fxState[1].offset = mce->layout->offset_IP;
   di->fxState[1].size   = mce->layout->sizeof_IP;
}


/* Check the supplied **original** atom for undefinedness, and emit a
   complaint if so.  Once that happens, mark it as defined.  This is
   possible because the atom is either a tmp or literal.  If it's a
   tmp, it will be shadowed by a tmp, and so we can set the shadow to
   be defined.  In fact as mentioned above, we will have to allocate a
   new tmp to carry the new 'defined' shadow value, and update the
   original->tmp mapping accordingly; we cannot simply assign a new
   value to an existing shadow tmp as this breaks SSAness -- resulting
   in the post-instrumentation sanity checker spluttering in disapproval. 
*/
static void complainIfUndefined ( MCEnv* mce, IRAtom* atom )
{
   IRAtom*  vatom;
   IRType   ty;
   Int      sz;
   IRDirty* di;
   IRAtom*  cond;

   /* Since the original expression is atomic, there's no duplicated
      work generated by making multiple V-expressions for it.  So we
      don't really care about the possibility that someone else may
      also create a V-interpretion for it. */
   tl_assert(isOriginalAtom(mce, atom));
   vatom = expr2vbits( mce, atom );
   tl_assert(isShadowAtom(mce, vatom));
   tl_assert(sameKindedAtoms(atom, vatom));

   ty = typeOfIRExpr(mce->bb->tyenv, vatom);

   /* sz is only used for constructing the error message */
   sz = ty==Ity_I1 ? 0 : sizeofIRType(ty);

   cond = mkPCastTo( mce, Ity_I1, vatom );
   /* cond will be 0 if all defined, and 1 if any not defined. */

   switch (sz) {
      case 0:
         di = unsafeIRDirty_0_N( 0/*regparms*/, 
                                 "MC_(helperc_value_check0_fail)",
                                 &MC_(helperc_value_check0_fail),
                                 mkIRExprVec_0() 
                               );
         break;
      case 1:
         di = unsafeIRDirty_0_N( 0/*regparms*/, 
                                 "MC_(helperc_value_check1_fail)",
                                 &MC_(helperc_value_check1_fail),
                                 mkIRExprVec_0() 
                               );
         break;
      case 4:
         di = unsafeIRDirty_0_N( 0/*regparms*/, 
                                 "MC_(helperc_value_check4_fail)",
                                 &MC_(helperc_value_check4_fail),
                                 mkIRExprVec_0() 
                               );
         break;
      default:
         di = unsafeIRDirty_0_N( 1/*regparms*/, 
                                 "MC_(helperc_complain_undef)",
                                 &MC_(helperc_complain_undef),
                                 mkIRExprVec_1( mkIRExpr_HWord( sz ))
                               );
         break;
   }
   di->guard = cond;
   setHelperAnns( mce, di );
   stmt( mce->bb, IRStmt_Dirty(di));

   /* Set the shadow tmp to be defined.  First, update the
      orig->shadow tmp mapping to reflect the fact that this shadow is
      getting a new value. */
   tl_assert(isAtom(vatom));
   /* sameKindedAtoms ... */
   if (vatom->tag == Iex_Tmp) {
      tl_assert(atom->tag == Iex_Tmp);
      newShadowTmp(mce, atom->Iex.Tmp.tmp);
      assign(mce->bb, findShadowTmp(mce, atom->Iex.Tmp.tmp), 
                      definedOfType(ty));
   }
}


/*------------------------------------------------------------*/
/*--- Shadowing PUTs/GETs, and indexed variants thereof    ---*/
/*------------------------------------------------------------*/

/* Examine the always-defined sections declared in layout to see if
   the (offset,size) section is within one.  Note, is is an error to
   partially fall into such a region: (offset,size) should either be
   completely in such a region or completely not-in such a region.  
*/
static Bool isAlwaysDefd ( MCEnv* mce, Int offset, Int size )
{
   Int minoffD, maxoffD, i;
   Int minoff = offset;
   Int maxoff = minoff + size - 1;
   tl_assert((minoff & ~0xFFFF) == 0);
   tl_assert((maxoff & ~0xFFFF) == 0);

   for (i = 0; i < mce->layout->n_alwaysDefd; i++) {
      minoffD = mce->layout->alwaysDefd[i].offset;
      maxoffD = minoffD + mce->layout->alwaysDefd[i].size - 1;
      tl_assert((minoffD & ~0xFFFF) == 0);
      tl_assert((maxoffD & ~0xFFFF) == 0);

      if (maxoff < minoffD || maxoffD < minoff)
         continue; /* no overlap */
      if (minoff >= minoffD && maxoff <= maxoffD)
         return True; /* completely contained in an always-defd section */

      VG_(tool_panic)("memcheck:isAlwaysDefd:partial overlap");
   }
   return False; /* could not find any containing section */
}


/* Generate into bb suitable actions to shadow this Put.  If the state
   slice is marked 'always defined', do nothing.  Otherwise, write the
   supplied V bits to the shadow state.  We can pass in either an
   original atom or a V-atom, but not both.  In the former case the
   relevant V-bits are then generated from the original.
*/
static
void do_shadow_PUT ( MCEnv* mce,  Int offset, 
                     IRAtom* atom, IRAtom* vatom )
{
   IRType ty;
   if (atom) {
      tl_assert(!vatom);
      tl_assert(isOriginalAtom(mce, atom));
      vatom = expr2vbits( mce, atom );
   } else {
      tl_assert(vatom);
      tl_assert(isShadowAtom(mce, vatom));
   }

   ty = typeOfIRExpr(mce->bb->tyenv, vatom);
   tl_assert(ty != Ity_I1);
   if (isAlwaysDefd(mce, offset, sizeofIRType(ty))) {
      /* later: no ... */
      /* emit code to emit a complaint if any of the vbits are 1. */
      /* complainIfUndefined(mce, atom); */
   } else {
      /* Do a plain shadow Put. */
      stmt( mce->bb, IRStmt_Put( offset + mce->layout->total_sizeB, vatom ) );
   }
}


/* Return an expression which contains the V bits corresponding to the
   given GETI (passed in in pieces). 
*/
static
void do_shadow_PUTI ( MCEnv* mce, 
                      IRArray* descr, IRAtom* ix, Int bias, IRAtom* atom )
{
   IRAtom* vatom;
   IRType  ty, tyS;
   Int     arrSize;;

   tl_assert(isOriginalAtom(mce,atom));
   vatom = expr2vbits( mce, atom );
   tl_assert(sameKindedAtoms(atom, vatom));
   ty   = descr->elemTy;
   tyS  = shadowType(ty);
   arrSize = descr->nElems * sizeofIRType(ty);
   tl_assert(ty != Ity_I1);
   tl_assert(isOriginalAtom(mce,ix));
   complainIfUndefined(mce,ix);
   if (isAlwaysDefd(mce, descr->base, arrSize)) {
      /* later: no ... */
      /* emit code to emit a complaint if any of the vbits are 1. */
      /* complainIfUndefined(mce, atom); */
   } else {
      /* Do a cloned version of the Put that refers to the shadow
         area. */
      IRArray* new_descr 
         = mkIRArray( descr->base + mce->layout->total_sizeB, 
                      tyS, descr->nElems);
      stmt( mce->bb, IRStmt_PutI( new_descr, ix, bias, vatom ));
   }
}


/* Return an expression which contains the V bits corresponding to the
   given GET (passed in in pieces). 
*/
static 
IRExpr* shadow_GET ( MCEnv* mce, Int offset, IRType ty )
{
   IRType tyS = shadowType(ty);
   tl_assert(ty != Ity_I1);
   if (isAlwaysDefd(mce, offset, sizeofIRType(ty))) {
      /* Always defined, return all zeroes of the relevant type */
      return definedOfType(tyS);
   } else {
      /* return a cloned version of the Get that refers to the shadow
         area. */
      return IRExpr_Get( offset + mce->layout->total_sizeB, tyS );
   }
}


/* Return an expression which contains the V bits corresponding to the
   given GETI (passed in in pieces). 
*/
static
IRExpr* shadow_GETI ( MCEnv* mce, IRArray* descr, IRAtom* ix, Int bias )
{
   IRType ty   = descr->elemTy;
   IRType tyS  = shadowType(ty);
   Int arrSize = descr->nElems * sizeofIRType(ty);
   tl_assert(ty != Ity_I1);
   tl_assert(isOriginalAtom(mce,ix));
   complainIfUndefined(mce,ix);
   if (isAlwaysDefd(mce, descr->base, arrSize)) {
      /* Always defined, return all zeroes of the relevant type */
      return definedOfType(tyS);
   } else {
      /* return a cloned version of the Get that refers to the shadow
         area. */
      IRArray* new_descr 
         = mkIRArray( descr->base + mce->layout->total_sizeB, 
                      tyS, descr->nElems);
      return IRExpr_GetI( new_descr, ix, bias );
   }
}


/*------------------------------------------------------------*/
/*--- Generating approximations for unknown operations,    ---*/
/*--- using lazy-propagate semantics                       ---*/
/*------------------------------------------------------------*/

/* Lazy propagation of undefinedness from two values, resulting in the
   specified shadow type. 
*/
static
IRAtom* mkLazy2 ( MCEnv* mce, IRType finalVty, IRAtom* va1, IRAtom* va2 )
{
   /* force everything via 32-bit intermediaries. */
   IRAtom* at;
   tl_assert(isShadowAtom(mce,va1));
   tl_assert(isShadowAtom(mce,va2));
   at = mkPCastTo(mce, Ity_I32, va1);
   at = mkUifU(mce, Ity_I32, at, mkPCastTo(mce, Ity_I32, va2));
   at = mkPCastTo(mce, finalVty, at);
   return at;
}


/* Do the lazy propagation game from a null-terminated vector of
   atoms.  This is presumably the arguments to a helper call, so the
   IRCallee info is also supplied in order that we can know which
   arguments should be ignored (via the .mcx_mask field). 
*/
static
IRAtom* mkLazyN ( MCEnv* mce, 
                  IRAtom** exprvec, IRType finalVtype, IRCallee* cee )
{
   Int i;
   IRAtom* here;
   IRAtom* curr = definedOfType(Ity_I32);
   for (i = 0; exprvec[i]; i++) {
      tl_assert(i < 32);
      tl_assert(isOriginalAtom(mce, exprvec[i]));
      /* Only take notice of this arg if the callee's mc-exclusion
         mask does not say it is to be excluded. */
      if (cee->mcx_mask & (1<<i)) {
         /* the arg is to be excluded from definedness checking.  Do
            nothing. */
         if (0) VG_(printf)("excluding %s(%d)\n", cee->name, i);
      } else {
         /* calculate the arg's definedness, and pessimistically merge
            it in. */
         here = mkPCastTo( mce, Ity_I32, expr2vbits(mce, exprvec[i]) );
         curr = mkUifU32(mce, here, curr);
      }
   }
   return mkPCastTo(mce, finalVtype, curr );
}


/*------------------------------------------------------------*/
/*--- Generating expensive sequences for exact carry-chain ---*/
/*--- propagation in add/sub and related operations.       ---*/
/*------------------------------------------------------------*/

static
IRAtom* expensiveAdd32 ( MCEnv* mce, IRAtom* qaa, IRAtom* qbb, 
                                     IRAtom* aa,  IRAtom* bb )
{
   IRAtom *a_min, *b_min, *a_max, *b_max;
   IRType ty;
   IROp   opAND, opOR, opXOR, opNOT, opADD;

   tl_assert(isShadowAtom(mce,qaa));
   tl_assert(isShadowAtom(mce,qbb));
   tl_assert(isOriginalAtom(mce,aa));
   tl_assert(isOriginalAtom(mce,bb));
   tl_assert(sameKindedAtoms(qaa,aa));
   tl_assert(sameKindedAtoms(qbb,bb));

   ty    = Ity_I32;
   opAND = Iop_And32;
   opOR  = Iop_Or32;
   opXOR = Iop_Xor32;
   opNOT = Iop_Not32;
   opADD = Iop_Add32;

   // a_min = aa & ~qaa
   a_min = assignNew(mce,ty, 
                     binop(opAND, aa,
                                  assignNew(mce,ty, unop(opNOT, qaa))));

   // b_min = bb & ~qbb
   b_min = assignNew(mce,ty, 
                     binop(opAND, bb,
                                  assignNew(mce,ty, unop(opNOT, qbb))));

   // a_max = aa | qaa
   a_max = assignNew(mce,ty, binop(opOR, aa, qaa));

   // b_max = bb | qbb
   b_max = assignNew(mce,ty, binop(opOR, bb, qbb));

   // result = (qaa | qbb) | ((a_min + b_min) ^ (a_max + b_max))
   return
   assignNew(mce,ty,
      binop( opOR,
             assignNew(mce,ty, binop(opOR, qaa, qbb)),
             assignNew(mce,ty, 
                binop(opXOR, assignNew(mce,ty, binop(opADD, a_min, b_min)),
                             assignNew(mce,ty, binop(opADD, a_max, b_max))
                )
             )
      )
   );
}


/*------------------------------------------------------------*/
/*--- Generate shadow values from all kinds of IRExprs.    ---*/
/*------------------------------------------------------------*/

static 
IRAtom* expr2vbits_Binop ( MCEnv* mce,
                           IROp op,
                           IRAtom* atom1, IRAtom* atom2 )
{
   IRType  and_or_ty;
   IRAtom* (*uifu)    (MCEnv*, IRAtom*, IRAtom*);
   IRAtom* (*difd)    (MCEnv*, IRAtom*, IRAtom*);
   IRAtom* (*improve) (MCEnv*, IRAtom*, IRAtom*);

   IRAtom* vatom1 = expr2vbits( mce, atom1 );
   IRAtom* vatom2 = expr2vbits( mce, atom2 );

   tl_assert(isOriginalAtom(mce,atom1));
   tl_assert(isOriginalAtom(mce,atom2));
   tl_assert(isShadowAtom(mce,vatom1));
   tl_assert(isShadowAtom(mce,vatom2));
   tl_assert(sameKindedAtoms(atom1,vatom1));
   tl_assert(sameKindedAtoms(atom2,vatom2));
   switch (op) {

      case Iop_RoundF64:
      case Iop_F64toI64:
         /* First arg is I32 (rounding mode), second is F64 (data). */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_PRemC3210F64: case Iop_PRem1C3210F64:
         /* Takes two F64 args. */
      case Iop_F64toI32:
         /* First arg is I32 (rounding mode), second is F64 (data). */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_F64toI16:
         /* First arg is I32 (rounding mode), second is F64 (data). */
         return mkLazy2(mce, Ity_I16, vatom1, vatom2);

      case Iop_ScaleF64:
      case Iop_Yl2xF64:
      case Iop_Yl2xp1F64:
      case Iop_PRemF64:
      case Iop_AtanF64:
      case Iop_AddF64:
      case Iop_DivF64:
      case Iop_SubF64:
      case Iop_MulF64:
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_CmpF64:
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      /* non-FP after here */

      case Iop_DivModU64to32:
      case Iop_DivModS64to32:
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_16HLto32:
         return assignNew(mce, Ity_I32,
                          binop(Iop_16HLto32, vatom1, vatom2));
      case Iop_32HLto64:
         return assignNew(mce, Ity_I64,
                          binop(Iop_32HLto64, vatom1, vatom2));

      case Iop_MullS32:
      case Iop_MullU32: {
         IRAtom* vLo32 = mkLeft32(mce, mkUifU32(mce, vatom1,vatom2));
         IRAtom* vHi32 = mkPCastTo(mce, Ity_I32, vLo32);
         return assignNew(mce, Ity_I64, binop(Iop_32HLto64, vHi32, vLo32));
      }

      case Iop_MullS16:
      case Iop_MullU16: {
         IRAtom* vLo16 = mkLeft16(mce, mkUifU16(mce, vatom1,vatom2));
         IRAtom* vHi16 = mkPCastTo(mce, Ity_I16, vLo16);
         return assignNew(mce, Ity_I32, binop(Iop_16HLto32, vHi16, vLo16));
      }

      case Iop_MullS8:
      case Iop_MullU8: {
         IRAtom* vLo8 = mkLeft8(mce, mkUifU8(mce, vatom1,vatom2));
         IRAtom* vHi8 = mkPCastTo(mce, Ity_I8, vLo8);
         return assignNew(mce, Ity_I16, binop(Iop_8HLto16, vHi8, vLo8));
      }

      case Iop_Add32:
#        if 0
         return expensiveAdd32(mce, vatom1,vatom2, atom1,atom2);
#        endif
      case Iop_Sub32:
      case Iop_Mul32:
         return mkLeft32(mce, mkUifU32(mce, vatom1,vatom2));

      case Iop_Mul16:
      case Iop_Add16:
      case Iop_Sub16:
         return mkLeft16(mce, mkUifU16(mce, vatom1,vatom2));

      case Iop_Sub8:
      case Iop_Add8:
         return mkLeft8(mce, mkUifU8(mce, vatom1,vatom2));

      case Iop_CmpLE32S: case Iop_CmpLE32U: 
      case Iop_CmpLT32U: case Iop_CmpLT32S:
      case Iop_CmpEQ32: case Iop_CmpNE32:
         return mkPCastTo(mce, Ity_I1, mkUifU32(mce, vatom1,vatom2));

      case Iop_CmpEQ16: case Iop_CmpNE16:
         return mkPCastTo(mce, Ity_I1, mkUifU16(mce, vatom1,vatom2));

      case Iop_CmpEQ8: case Iop_CmpNE8:
         return mkPCastTo(mce, Ity_I1, mkUifU8(mce, vatom1,vatom2));

      case Iop_Shl32: case Iop_Shr32: case Iop_Sar32:
         /* Complain if the shift amount is undefined.  Then simply
            shift the first arg's V bits by the real shift amount. */
         complainIfUndefined(mce, atom2);
         return assignNew(mce, Ity_I32, binop(op, vatom1, atom2));

      case Iop_Shl16: case Iop_Shr16:
         /* Same scheme as with 32-bit shifts. */
         complainIfUndefined(mce, atom2);
         return assignNew(mce, Ity_I16, binop(op, vatom1, atom2));

      case Iop_Shl8: case Iop_Shr8:
         /* Same scheme as with 32-bit shifts. */
         complainIfUndefined(mce, atom2);
         return assignNew(mce, Ity_I8, binop(op, vatom1, atom2));

      case Iop_Shl64: case Iop_Shr64: 
         /* Same scheme as with 32-bit shifts. */
         complainIfUndefined(mce, atom2);
         return assignNew(mce, Ity_I64, binop(op, vatom1, atom2));

      case Iop_And32:
         uifu = mkUifU32; difd = mkDifD32; 
         and_or_ty = Ity_I32; improve = mkImproveAND32; goto do_And_Or;
      case Iop_And16:
         uifu = mkUifU16; difd = mkDifD16; 
         and_or_ty = Ity_I16; improve = mkImproveAND16; goto do_And_Or;
      case Iop_And8:
         uifu = mkUifU8; difd = mkDifD8; 
         and_or_ty = Ity_I8; improve = mkImproveAND8; goto do_And_Or;

      case Iop_Or32:
         uifu = mkUifU32; difd = mkDifD32; 
         and_or_ty = Ity_I32; improve = mkImproveOR32; goto do_And_Or;
      case Iop_Or16:
         uifu = mkUifU16; difd = mkDifD16; 
         and_or_ty = Ity_I16; improve = mkImproveOR16; goto do_And_Or;
      case Iop_Or8:
         uifu = mkUifU8; difd = mkDifD8; 
         and_or_ty = Ity_I8; improve = mkImproveOR8; goto do_And_Or;

      do_And_Or:
         return
         assignNew(
            mce, 
            and_or_ty,
            difd(mce, uifu(mce, vatom1, vatom2),
                      difd(mce, improve(mce, atom1, vatom1),
                                improve(mce, atom2, vatom2) ) ) );

      case Iop_Xor8:
         return mkUifU8(mce, vatom1, vatom2);
      case Iop_Xor16:
         return mkUifU16(mce, vatom1, vatom2);
      case Iop_Xor32:
         return mkUifU32(mce, vatom1, vatom2);

      default:
         ppIROp(op);
         VG_(tool_panic)("memcheck:expr2vbits_Binop");
   }
}


static 
IRExpr* expr2vbits_Unop ( MCEnv* mce, IROp op, IRAtom* atom )
{
   IRAtom* vatom = expr2vbits( mce, atom );
   tl_assert(isOriginalAtom(mce,atom));
   switch (op) {

      case Iop_F32toF64: 
      case Iop_I32toF64:
      case Iop_I64toF64:
      case Iop_NegF64:
      case Iop_SinF64:
      case Iop_CosF64:
      case Iop_TanF64:
      case Iop_SqrtF64:
      case Iop_AbsF64:
      case Iop_2xm1F64:
         return mkPCastTo(mce, Ity_I64, vatom);

      case Iop_F64toF32:
      case Iop_Clz32:
      case Iop_Ctz32:
         return mkPCastTo(mce, Ity_I32, vatom);

      case Iop_32Sto64:
      case Iop_32Uto64:
         return assignNew(mce, Ity_I64, unop(op, vatom));

      case Iop_64to32:
      case Iop_64HIto32:
      case Iop_1Uto32:
      case Iop_8Uto32:
      case Iop_16Uto32:
      case Iop_16Sto32:
      case Iop_8Sto32:
         return assignNew(mce, Ity_I32, unop(op, vatom));

      case Iop_8Sto16:
      case Iop_8Uto16:
      case Iop_32to16:
      case Iop_32HIto16:
         return assignNew(mce, Ity_I16, unop(op, vatom));

      case Iop_1Uto8:
      case Iop_16to8:
      case Iop_32to8:
         return assignNew(mce, Ity_I8, unop(op, vatom));

      case Iop_32to1:
         return assignNew(mce, Ity_I1, unop(Iop_32to1, vatom));

      case Iop_ReinterpF64asI64:
      case Iop_ReinterpI64asF64:
      case Iop_Not32:
      case Iop_Not16:
      case Iop_Not8:
      case Iop_Not1:
         return vatom;
      default:
         ppIROp(op);
         VG_(tool_panic)("memcheck:expr2vbits_Unop");
   }
}


static
IRAtom* expr2vbits_LDle ( MCEnv* mce, IRType ty, IRAtom* addr, UInt bias )
{
   void*    helper;
   Char*    hname;
   IRDirty* di;
   IRTemp   datavbits;
   IRAtom*  addrAct;

   tl_assert(isOriginalAtom(mce,addr));

   /* First, emit a definedness test for the address.  This also sets
      the address (shadow) to 'defined' following the test. */
   complainIfUndefined( mce, addr );

   /* Now cook up a call to the relevant helper function, to read the
      data V bits from shadow memory. */
   ty = shadowType(ty);
   switch (ty) {
      case Ity_I64: helper = &MC_(helperc_LOADV8);
                    hname = "MC_(helperc_LOADV8)";
                    break;
      case Ity_I32: helper = &MC_(helperc_LOADV4);
                    hname = "MC_(helperc_LOADV4)";
                    break;
      case Ity_I16: helper = &MC_(helperc_LOADV2);
                    hname = "MC_(helperc_LOADV2)";
                    break;
      case Ity_I8:  helper = &MC_(helperc_LOADV1);
                    hname = "MC_(helperc_LOADV1)";
                    break;
      default:      ppIRType(ty);
                    VG_(tool_panic)("memcheck:do_shadow_LDle");
   }

   /* Generate the actual address into addrAct. */
   if (bias == 0) {
      addrAct = addr;
   } else {
      IROp    mkAdd;
      IRAtom* eBias;
      IRType  tyAddr  = mce->hWordTy;
      tl_assert( tyAddr == Ity_I32 || tyAddr == Ity_I64 );
      mkAdd   = tyAddr==Ity_I32 ? Iop_Add32 : Iop_Add64;
      eBias   = tyAddr==Ity_I32 ? mkU32(bias) : mkU64(bias);
      addrAct = assignNew(mce, tyAddr, binop(mkAdd, addr, eBias) );
   }

   /* We need to have a place to park the V bits we're just about to
      read. */
   datavbits = newIRTemp(mce->bb->tyenv, ty);
   di = unsafeIRDirty_1_N( datavbits, 
                           1/*regparms*/, hname, helper, 
                           mkIRExprVec_1( addrAct ));
   setHelperAnns( mce, di );
   stmt( mce->bb, IRStmt_Dirty(di) );

   return mkexpr(datavbits);
}


static
IRAtom* expr2vbits_Mux0X ( MCEnv* mce, 
                           IRAtom* cond, IRAtom* expr0, IRAtom* exprX )
{
   IRAtom *vbitsC, *vbits0, *vbitsX;
   IRType ty;
   /* Given Mux0X(cond,expr0,exprX), generate
         Mux0X(cond,expr0#,exprX#) `UifU` PCast(cond#)
      That is, steer the V bits like the originals, but trash the 
      result if the steering value is undefined.  This gives 
      lazy propagation. */
   tl_assert(isOriginalAtom(mce, cond));
   tl_assert(isOriginalAtom(mce, expr0));
   tl_assert(isOriginalAtom(mce, exprX));

   vbitsC = expr2vbits(mce, cond);
   vbits0 = expr2vbits(mce, expr0);
   vbitsX = expr2vbits(mce, exprX);
   ty = typeOfIRExpr(mce->bb->tyenv, vbits0);

   return
      mkUifU(mce, ty, assignNew(mce, ty, IRExpr_Mux0X(cond, vbits0, vbitsX)),
                      mkPCastTo(mce, ty, vbitsC) );
}      

/* --------- This is the main expression-handling function. --------- */

static
IRExpr* expr2vbits ( MCEnv* mce, IRExpr* e )
{
   switch (e->tag) {

      case Iex_Get:
         return shadow_GET( mce, e->Iex.Get.offset, e->Iex.Get.ty );

      case Iex_GetI:
         return shadow_GETI( mce, e->Iex.GetI.descr, 
                                  e->Iex.GetI.ix, e->Iex.GetI.bias );

      case Iex_Tmp:
         return IRExpr_Tmp( findShadowTmp(mce, e->Iex.Tmp.tmp) );

      case Iex_Const:
         return definedOfType(shadowType(typeOfIRExpr(mce->bb->tyenv, e)));

      case Iex_Binop:
         return expr2vbits_Binop(
                   mce,
                   e->Iex.Binop.op,
                   e->Iex.Binop.arg1, e->Iex.Binop.arg2
                );

      case Iex_Unop:
         return expr2vbits_Unop( mce, e->Iex.Unop.op, e->Iex.Unop.arg );

      case Iex_LDle:
         return expr2vbits_LDle( mce, e->Iex.LDle.ty, 
                                      e->Iex.LDle.addr, 0/*addr bias*/ );

      case Iex_CCall:
         return mkLazyN( mce, e->Iex.CCall.args, 
                              e->Iex.CCall.retty,
                              e->Iex.CCall.cee );

      case Iex_Mux0X:
         return expr2vbits_Mux0X( mce, e->Iex.Mux0X.cond, e->Iex.Mux0X.expr0, 
                                       e->Iex.Mux0X.exprX);

      default: 
         VG_(printf)("\n");
         ppIRExpr(e);
         VG_(printf)("\n");
         VG_(tool_panic)("memcheck: expr2vbits");
   }
}

/*------------------------------------------------------------*/
/*--- Generate shadow stmts from all kinds of IRStmts.     ---*/
/*------------------------------------------------------------*/

/* Widen a value to the host word size. */

static
IRExpr* zwidenToHostWord ( MCEnv* mce, IRAtom* vatom )
{
   IRType ty, tyH;

   /* vatom is vbits-value and as such can only have a shadow type. */
   tl_assert(isShadowAtom(mce,vatom));

   ty  = typeOfIRExpr(mce->bb->tyenv, vatom);
   tyH = mce->hWordTy;

   if (tyH == Ity_I32) {
      switch (ty) {
         case Ity_I32: return vatom;
         case Ity_I16: return assignNew(mce, tyH, unop(Iop_16Uto32, vatom));
         case Ity_I8:  return assignNew(mce, tyH, unop(Iop_8Uto32, vatom));
         default:      goto unhandled;
      }
   } else {
      goto unhandled;
   }
  unhandled:
   VG_(printf)("\nty = "); ppIRType(ty); VG_(printf)("\n");
   VG_(tool_panic)("zwidenToHostWord");
}


/* Generate a shadow store.  addr is always the original address atom.
   You can pass in either originals or V-bits for the data atom, but
   obviously not both.  */

static 
void do_shadow_STle ( MCEnv* mce, 
                      IRAtom* addr, UInt bias,
                      IRAtom* data, IRAtom* vdata )
{
   IRType   ty;
   IRDirty* di;
   void*    helper = NULL;
   Char*    hname = NULL;
   IRAtom*  addrAct;

   if (data) {
      tl_assert(!vdata);
      tl_assert(isOriginalAtom(mce, data));
      tl_assert(bias == 0);
      vdata = expr2vbits( mce, data );
   } else {
      tl_assert(vdata);
   }

   tl_assert(isOriginalAtom(mce,addr));
   tl_assert(isShadowAtom(mce,vdata));

   ty = typeOfIRExpr(mce->bb->tyenv, vdata);

   /* First, emit a definedness test for the address.  This also sets
      the address (shadow) to 'defined' following the test. */
   complainIfUndefined( mce, addr );

   /* Now cook up a call to the relevant helper function, to write the
      data V bits into shadow memory. */
   switch (ty) {
      case Ity_I64: helper = &MC_(helperc_STOREV8);
                    hname = "MC_(helperc_STOREV8)";
                    break;
      case Ity_I32: helper = &MC_(helperc_STOREV4);
                    hname = "MC_(helperc_STOREV4)";
                    break;
      case Ity_I16: helper = &MC_(helperc_STOREV2);
                    hname = "MC_(helperc_STOREV2)";
                    break;
      case Ity_I8:  helper = &MC_(helperc_STOREV1);
                    hname = "MC_(helperc_STOREV1)";
                    break;
      default:      VG_(tool_panic)("memcheck:do_shadow_STle");
   }

   /* Generate the actual address into addrAct. */
   if (bias == 0) {
      addrAct = addr;
   } else {
      IROp    mkAdd;
      IRAtom* eBias;
      IRType  tyAddr  = mce->hWordTy;
      tl_assert( tyAddr == Ity_I32 || tyAddr == Ity_I64 );
      mkAdd   = tyAddr==Ity_I32 ? Iop_Add32 : Iop_Add64;
      eBias   = tyAddr==Ity_I32 ? mkU32(bias) : mkU64(bias);
      addrAct = assignNew(mce, tyAddr, binop(mkAdd, addr, eBias) );
   }

   if (ty == Ity_I64) {
      /* We can't do this with regparm 2 on 32-bit platforms, since
         the back ends aren't clever enough to handle 64-bit regparm
         args.  Therefore be different. */
      di = unsafeIRDirty_0_N( 
              1/*regparms*/, hname, helper, 
              mkIRExprVec_2( addrAct, vdata ));
   } else {
      di = unsafeIRDirty_0_N( 
              2/*regparms*/, hname, helper, 
              mkIRExprVec_2( addrAct,
                             zwidenToHostWord( mce, vdata )));
   }
   setHelperAnns( mce, di );
   stmt( mce->bb, IRStmt_Dirty(di) );
}


/* Do lazy pessimistic propagation through a dirty helper call, by
   looking at the annotations on it.  This is the most complex part of
   Memcheck. */

static IRType szToITy ( Int n )
{
   switch (n) {
      case 1: return Ity_I8;
      case 2: return Ity_I16;
      case 4: return Ity_I32;
      case 8: return Ity_I64;
      default: VG_(tool_panic)("szToITy(memcheck)");
   }
}

static
void do_shadow_Dirty ( MCEnv* mce, IRDirty* d )
{
   Int     i, offset, toDo;
   IRAtom  *src, *here, *curr;
   IRType  tyAddr, tySrc, tyDst;
   IRTemp  dst;

   /* First check the guard. */
   complainIfUndefined(mce, d->guard);

   /* Now round up all inputs and PCast over them. */
   curr = definedOfType(Ity_I32);

   /* Inputs: unmasked args */
   for (i = 0; d->args[i]; i++) {
      if (d->cee->mcx_mask & (1<<i)) {
         /* ignore this arg */
      } else {
         here = mkPCastTo( mce, Ity_I32, expr2vbits(mce, d->args[i]) );
         curr = mkUifU32(mce, here, curr);
      }
   }

   /* Inputs: guest state that we read. */
   for (i = 0; i < d->nFxState; i++) {
      tl_assert(d->fxState[i].fx != Ifx_None);
      if (d->fxState[i].fx == Ifx_Write)
         continue;

      /* Ignore any sections marked as 'always defined'. */
      if (isAlwaysDefd(mce, d->fxState[i].offset, d->fxState[i].size )) {
         VG_(printf)("memcheck: Dirty gst: ignored off %d, sz %d\n",
                     d->fxState[i].offset, d->fxState[i].size );
         continue;
      }

      /* This state element is read or modified.  So we need to
         consider it. */
      tySrc = szToITy( d->fxState[i].size );
      src   = assignNew( mce, tySrc, 
                         shadow_GET(mce, d->fxState[i].offset, tySrc ) );
      here = mkPCastTo( mce, Ity_I32, src );
      curr = mkUifU32(mce, here, curr);
   }

   /* Inputs: memory.  First set up some info needed regardless of
      whether we're doing reads or writes. */
   tyAddr = Ity_INVALID;

   if (d->mFx != Ifx_None) {
      /* Because we may do multiple shadow loads/stores from the same
         base address, it's best to do a single test of its
         definedness right now.  Post-instrumentation optimisation
         should remove all but this test. */
      tl_assert(d->mAddr);
      complainIfUndefined(mce, d->mAddr);

      tyAddr = typeOfIRExpr(mce->bb->tyenv, d->mAddr);
      tl_assert(tyAddr == Ity_I32 || tyAddr == Ity_I64);
      tl_assert(tyAddr == mce->hWordTy); /* not really right */
   }

   /* Deal with memory inputs (reads or modifies) */
   if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify) {
      offset = 0;
      toDo   = d->mSize;
      /* chew off 32-bit chunks */
      while (toDo >= 4) {
         here = mkPCastTo( 
                   mce, Ity_I32,
                   expr2vbits_LDle ( mce, Ity_I32, 
                                     d->mAddr, d->mSize - toDo )
                );
         curr = mkUifU32(mce, here, curr);
         toDo -= 4;
      }
      /* chew off 16-bit chunks */
      while (toDo >= 2) {
         here = mkPCastTo( 
                   mce, Ity_I32,
                   expr2vbits_LDle ( mce, Ity_I16, 
                                     d->mAddr, d->mSize - toDo )
                );
         curr = mkUifU32(mce, here, curr);
         toDo -= 2;
      }
      tl_assert(toDo == 0); /* also need to handle 1-byte excess */
   }

   /* Whew!  So curr is a 32-bit V-value summarising pessimistically
      all the inputs to the helper.  Now we need to re-distribute the
      results to all destinations. */

   /* Outputs: the destination temporary, if there is one. */
   if (d->tmp != IRTemp_INVALID) {
      dst   = findShadowTmp(mce, d->tmp);
      tyDst = typeOfIRTemp(mce->bb->tyenv, d->tmp);
      assign( mce->bb, dst, mkPCastTo( mce, tyDst, curr) );
   }

   /* Outputs: guest state that we write or modify. */
   for (i = 0; i < d->nFxState; i++) {
      tl_assert(d->fxState[i].fx != Ifx_None);
      if (d->fxState[i].fx == Ifx_Read)
         continue;
      /* Ignore any sections marked as 'always defined'. */
      if (isAlwaysDefd(mce, d->fxState[i].offset, d->fxState[i].size ))
         continue;
      /* this state element is written or modified.  So we need to
         consider it. */
      tyDst = szToITy( d->fxState[i].size );
      do_shadow_PUT( mce, d->fxState[i].offset,
                          NULL, /* original atom */
                          mkPCastTo( mce, tyDst, curr ) );
   }

   /* Outputs: memory that we write or modify. */
   if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify) {
      offset = 0;
      toDo   = d->mSize;
      /* chew off 32-bit chunks */
      while (toDo >= 4) {
         do_shadow_STle( mce, d->mAddr, d->mSize - toDo,
                         NULL, /* original data */
                         mkPCastTo( mce, Ity_I32, curr ) );
         toDo -= 4;
      }
      /* chew off 16-bit chunks */
      while (toDo >= 2) {
         do_shadow_STle( mce, d->mAddr, d->mSize - toDo,
                         NULL, /* original data */
                         mkPCastTo( mce, Ity_I16, curr ) );
         toDo -= 2;
      }
      tl_assert(toDo == 0); /* also need to handle 1-byte excess */
   }

}


/*------------------------------------------------------------*/
/*--- Memcheck main                                        ---*/
/*------------------------------------------------------------*/

#if 0 /* UNUSED */
static Bool isBogusAtom ( IRAtom* at )
{
   ULong n = 0;
   IRConst* con;
   tl_assert(isAtom(at));
   if (at->tag == Iex_Tmp)
      return False;
   tl_assert(at->tag == Iex_Const);
   con = at->Iex.Const.con;
   switch (con->tag) {
      case Ico_U8:  n = (ULong)con->Ico.U8; break;
      case Ico_U16: n = (ULong)con->Ico.U16; break;
      case Ico_U32: n = (ULong)con->Ico.U32; break;
      case Ico_U64: n = (ULong)con->Ico.U64; break;
      default: ppIRExpr(at); tl_assert(0);
   }
   /* VG_(printf)("%llx\n", n); */
   return (n == 0xFEFEFEFF
           || n == 0x80808080
           || n == 0x1010101
           || n == 1010100);
}

static Bool checkForBogusLiterals ( /*FLAT*/ IRStmt* st )
{
   Int     i;
   IRExpr* e;
   switch (st->tag) {
      case Ist_Tmp:
         e = st->Ist.Tmp.data;
         switch (e->tag) {
            case Iex_Get:
            case Iex_Tmp:
               return False;
            case Iex_Unop: 
               return isBogusAtom(e->Iex.Unop.arg);
            case Iex_Binop: 
               return isBogusAtom(e->Iex.Binop.arg1)
                      || isBogusAtom(e->Iex.Binop.arg2);
            case Iex_Mux0X:
               return isBogusAtom(e->Iex.Mux0X.cond)
                      || isBogusAtom(e->Iex.Mux0X.expr0)
                      || isBogusAtom(e->Iex.Mux0X.exprX);
            case Iex_LDle: 
               return isBogusAtom(e->Iex.LDle.addr);
            case Iex_CCall:
               for (i = 0; e->Iex.CCall.args[i]; i++)
                  if (isBogusAtom(e->Iex.CCall.args[i]))
                     return True;
               return False;
            default: 
               goto unhandled;
         }
      case Ist_Put:
         return isBogusAtom(st->Ist.Put.data);
      case Ist_STle:
         return isBogusAtom(st->Ist.STle.addr) 
                || isBogusAtom(st->Ist.STle.data);
      case Ist_Exit:
         return isBogusAtom(st->Ist.Exit.cond);
      default: 
      unhandled:
         ppIRStmt(st);
         VG_(tool_panic)("hasBogusLiterals");
   }
}
#endif /* UNUSED */


IRBB* TL_(instrument) ( IRBB* bb_in, VexGuestLayout* layout, IRType hWordTy )
{
   Bool verboze = False; //True; 

   /* Bool hasBogusLiterals = False; */

   Int i, j, first_stmt;
   IRStmt* st;
   MCEnv mce;

   /* Set up BB */
   IRBB* bb     = emptyIRBB();
   bb->tyenv    = dopyIRTypeEnv(bb_in->tyenv);
   bb->next     = dopyIRExpr(bb_in->next);
   bb->jumpkind = bb_in->jumpkind;

   /* Set up the running environment.  Only .bb is modified as we go
      along. */
   mce.bb             = bb;
   mce.layout         = layout;
   mce.n_originalTmps = bb->tyenv->types_used;
   mce.hWordTy        = hWordTy;
   mce.tmpMap         = LibVEX_Alloc(mce.n_originalTmps * sizeof(IRTemp));
   for (i = 0; i < mce.n_originalTmps; i++)
      mce.tmpMap[i] = IRTemp_INVALID;

   /* Iterate over the stmts. */

   for (i = 0; i <  bb_in->stmts_used; i++) {
      st = bb_in->stmts[i];
      if (!st) continue;

      tl_assert(isFlatIRStmt(st));

      /*
      if (!hasBogusLiterals) {
         hasBogusLiterals = checkForBogusLiterals(st);
         if (hasBogusLiterals) {
            VG_(printf)("bogus: ");
            ppIRStmt(st);
            VG_(printf)("\n");
         }
      }
      */
      first_stmt = bb->stmts_used;

      if (verboze) {
         ppIRStmt(st);
         VG_(printf)("\n\n");
      }

      switch (st->tag) {

         case Ist_Tmp:
            assign( bb, findShadowTmp(&mce, st->Ist.Tmp.tmp), 
                        expr2vbits( &mce, st->Ist.Tmp.data) );
            break;

         case Ist_Put:
            do_shadow_PUT( &mce, 
                           st->Ist.Put.offset,
                           st->Ist.Put.data,
                           NULL /* shadow atom */ );
            break;

         case Ist_PutI:
            do_shadow_PUTI( &mce, 
                            st->Ist.PutI.descr,
                            st->Ist.PutI.ix,
                            st->Ist.PutI.bias,
                            st->Ist.PutI.data );
            break;

         case Ist_STle:
            do_shadow_STle( &mce, st->Ist.STle.addr, 0/* addr bias */,
                                  st->Ist.STle.data,
                                  NULL /* shadow data */ );
            break;

         case Ist_Exit:
            /* if (!hasBogusLiterals) */
               complainIfUndefined( &mce, st->Ist.Exit.guard );
            break;

         case Ist_Dirty:
            do_shadow_Dirty( &mce, st->Ist.Dirty.details );
            break;

         default:
            VG_(printf)("\n");
            ppIRStmt(st);
            VG_(printf)("\n");
            VG_(tool_panic)("memcheck: unhandled IRStmt");

      } /* switch (st->tag) */

      if (verboze) {
         for (j = first_stmt; j < bb->stmts_used; j++) {
            VG_(printf)("   ");
            ppIRStmt(bb->stmts[j]);
            VG_(printf)("\n");
         }
         VG_(printf)("\n");
      }

      addStmtToIRBB(bb, st);

   }

   /* Now we need to complain if the jump target is undefined. */
   first_stmt = bb->stmts_used;

   if (verboze) {
      VG_(printf)("bb->next = ");
      ppIRExpr(bb->next);
      VG_(printf)("\n\n");
   }

   complainIfUndefined( &mce, bb->next );

   if (verboze) {
      for (j = first_stmt; j < bb->stmts_used; j++) {
         VG_(printf)("   ");
         ppIRStmt(bb->stmts[j]);
         VG_(printf)("\n");
      }
      VG_(printf)("\n");
   }

   return bb;
}

/*--------------------------------------------------------------------*/
/*--- end                                           mc_translate.c ---*/
/*--------------------------------------------------------------------*/
