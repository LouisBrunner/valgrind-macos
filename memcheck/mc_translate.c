
/*--------------------------------------------------------------------*/
/*--- Instrument IR to perform memory checking operations.         ---*/
/*---                                               mc_translate.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_tool_basics.h"
#include "pub_tool_poolalloc.h"     // For mc_include.h
#include "pub_tool_hashtable.h"     // For mc_include.h
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_machine.h"     // VG_(fnptr_to_fnentry)
#include "pub_tool_xarray.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_libcbase.h"

#include "mc_include.h"


/* FIXMEs JRS 2011-June-16.

   Check the interpretation for vector narrowing and widening ops,
   particularly the saturating ones.  I suspect they are either overly
   pessimistic and/or wrong.

   Iop_QandSQsh64x2 and friends (vector-by-vector bidirectional
   saturating shifts): the interpretation is overly pessimistic.
   See comments on the relevant cases below for details.

   Iop_Sh64Sx2 and friends (vector-by-vector bidirectional shifts,
   both rounding and non-rounding variants): ditto
*/

/* This file implements the Memcheck instrumentation, and in
   particular contains the core of its undefined value detection
   machinery.  For a comprehensive background of the terminology,
   algorithms and rationale used herein, read:

     Using Valgrind to detect undefined value errors with
     bit-precision

     Julian Seward and Nicholas Nethercote

     2005 USENIX Annual Technical Conference (General Track),
     Anaheim, CA, USA, April 10-15, 2005.

   ----

   Here is as good a place as any to record exactly when V bits are and
   should be checked, why, and what function is responsible.

   
   Memcheck complains when an undefined value is used:

   1. In the condition of a conditional branch.  Because it could cause
      incorrect control flow, and thus cause incorrect externally-visible
      behaviour.  [mc_translate.c:complainIfUndefined]

   2. As an argument to a system call, or as the value that specifies
      the system call number.  Because it could cause an incorrect
      externally-visible side effect.  [mc_translate.c:mc_pre_reg_read]

   3. As the address in a load or store.  Because it could cause an
      incorrect value to be used later, which could cause externally-visible
      behaviour (eg. via incorrect control flow or an incorrect system call
      argument)  [complainIfUndefined]

   4. As the target address of a branch.  Because it could cause incorrect
      control flow.  [complainIfUndefined]

   5. As an argument to setenv, unsetenv, or putenv.  Because it could put
      an incorrect value into the external environment.
      [mc_replace_strmem.c:VG_WRAP_FUNCTION_ZU(*, *env)]

   6. As the index in a GETI or PUTI operation.  I'm not sure why... (njn).
      [complainIfUndefined]

   7. As an argument to the VALGRIND_CHECK_MEM_IS_DEFINED and
      VALGRIND_CHECK_VALUE_IS_DEFINED client requests.  Because the user
      requested it.  [in memcheck.h]


   Memcheck also complains, but should not, when an undefined value is used:

   8. As the shift value in certain SIMD shift operations (but not in the
      standard integer shift operations).  This inconsistency is due to
      historical reasons.)  [complainIfUndefined]


   Memcheck does not complain, but should, when an undefined value is used:

   9. As an input to a client request.  Because the client request may
      affect the visible behaviour -- see bug #144362 for an example
      involving the malloc replacements in vg_replace_malloc.c and
      VALGRIND_NON_SIMD_CALL* requests, where an uninitialised argument
      isn't identified.  That bug report also has some info on how to solve
      the problem.  [valgrind.h:VALGRIND_DO_CLIENT_REQUEST]


   In practice, 1 and 2 account for the vast majority of cases.
*/

/* Generation of addr-definedness, addr-validity and
   guard-definedness checks pertaining to loads and stores (Iex_Load,
   Ist_Store, IRLoadG, IRStoreG, LLSC, CAS and Dirty memory
   loads/stores) was re-checked 11 May 2013. */


/*------------------------------------------------------------*/
/*--- Forward decls                                        ---*/
/*------------------------------------------------------------*/

struct _MCEnv;

// See below for comments explaining what this is for.
typedef
   enum __attribute__((packed)) { HuUnU=0, HuPCa=1, HuOth=2 }
   HowUsed;

static IRType  shadowTypeV ( IRType ty );
static IRExpr* expr2vbits ( struct _MCEnv* mce, IRExpr* e,
                            HowUsed hu/*use HuOth if unknown*/ );
static IRTemp  findShadowTmpB ( struct _MCEnv* mce, IRTemp orig );

static IRExpr *i128_const_zero(void);


/*------------------------------------------------------------*/
/*--- Memcheck running state, and tmp management.          ---*/
/*------------------------------------------------------------*/

/* For a few (maybe 1%) IROps, we have both a cheaper, less exact vbit
   propagation scheme, and a more expensive, more precise vbit propagation
   scheme.  This enum describes, for such an IROp, which scheme to use. */
typedef
   enum {
      // Use the cheaper, less-exact variant.
      DLcheap=4,
      // Choose between cheap and expensive based on analysis of the block
      // to be instrumented.  Note that the choice may be done on a
      // per-instance basis of the IROp that this DetailLevel describes.
      DLauto,
      // Use the more expensive, more-exact variant.
      DLexpensive
   }
   DetailLevel;


/* A readonly part of the running state.  For IROps that have both a
   less-exact and more-exact interpretation, records which interpretation is
   to be used.  */
typedef
   struct {
      // For Add32/64 and Sub32/64, all 3 settings are allowed.  For the
      // DLauto case, a per-instance decision is to be made by inspecting
      // the associated tmp's entry in MCEnv.tmpHowUsed.
      DetailLevel dl_Add32;
      DetailLevel dl_Add64;
      DetailLevel dl_Sub32;
      DetailLevel dl_Sub64;
      // For Cmp{EQ,NE}{64,32,16,8}, only DLcheap and DLexpensive are
      // allowed.
      DetailLevel dl_CmpEQ64_CmpNE64;
      DetailLevel dl_CmpEQ32_CmpNE32;
      DetailLevel dl_CmpEQ16_CmpNE16;
      DetailLevel dl_CmpEQ8_CmpNE8;
   }
   DetailLevelByOp;

static void DetailLevelByOp__set_all ( /*OUT*/DetailLevelByOp* dlbo,
                                       DetailLevel dl )
{
   dlbo->dl_Add32           = dl;
   dlbo->dl_Add64           = dl;
   dlbo->dl_Sub32           = dl;
   dlbo->dl_Sub64           = dl;
   dlbo->dl_CmpEQ64_CmpNE64 = dl;
   dlbo->dl_CmpEQ32_CmpNE32 = dl;
   dlbo->dl_CmpEQ16_CmpNE16 = dl;
   dlbo->dl_CmpEQ8_CmpNE8   = dl;
}

static void DetailLevelByOp__check_sanity ( const DetailLevelByOp* dlbo )
{
   tl_assert(dlbo->dl_Add32 >= DLcheap && dlbo->dl_Add32 <= DLexpensive);
   tl_assert(dlbo->dl_Add64 >= DLcheap && dlbo->dl_Add64 <= DLexpensive);
   tl_assert(dlbo->dl_Sub32 >= DLcheap && dlbo->dl_Sub32 <= DLexpensive);
   tl_assert(dlbo->dl_Sub64 >= DLcheap && dlbo->dl_Sub64 <= DLexpensive);
   tl_assert(dlbo->dl_CmpEQ64_CmpNE64 == DLcheap
             || dlbo->dl_CmpEQ64_CmpNE64 == DLexpensive);
   tl_assert(dlbo->dl_CmpEQ32_CmpNE32 == DLcheap
             || dlbo->dl_CmpEQ32_CmpNE32 == DLexpensive);
   tl_assert(dlbo->dl_CmpEQ16_CmpNE16 == DLcheap
             || dlbo->dl_CmpEQ16_CmpNE16 == DLexpensive);
   tl_assert(dlbo->dl_CmpEQ8_CmpNE8 == DLcheap
             || dlbo->dl_CmpEQ8_CmpNE8 == DLexpensive);
}

static UInt DetailLevelByOp__count ( const DetailLevelByOp* dlbo,
                                     DetailLevel dl )
{
   UInt n = 0;
   n += (dlbo->dl_Add32 == dl            ? 1 : 0);
   n += (dlbo->dl_Add64 == dl            ? 1 : 0);
   n += (dlbo->dl_Sub32 == dl            ? 1 : 0);
   n += (dlbo->dl_Sub64 == dl            ? 1 : 0);
   n += (dlbo->dl_CmpEQ64_CmpNE64 == dl  ? 1 : 0);
   n += (dlbo->dl_CmpEQ32_CmpNE32 == dl  ? 1 : 0);
   n += (dlbo->dl_CmpEQ16_CmpNE16 == dl  ? 1 : 0);
   n += (dlbo->dl_CmpEQ8_CmpNE8 == dl    ? 1 : 0);
   return n;
}


/* Carries info about a particular tmp.  The tmp's number is not
   recorded, as this is implied by (equal to) its index in the tmpMap
   in MCEnv.  The tmp's type is also not recorded, as this is present
   in MCEnv.sb->tyenv.

   When .kind is Orig, .shadowV and .shadowB may give the identities
   of the temps currently holding the associated definedness (shadowV)
   and origin (shadowB) values, or these may be IRTemp_INVALID if code
   to compute such values has not yet been emitted.

   When .kind is VSh or BSh then the tmp is holds a V- or B- value,
   and so .shadowV and .shadowB must be IRTemp_INVALID, since it is
   illogical for a shadow tmp itself to be shadowed.
*/
typedef
   enum { Orig=1, VSh=2, BSh=3 }
   TempKind;

typedef
   struct {
      TempKind kind;
      IRTemp   shadowV;
      IRTemp   shadowB;
   }
   TempMapEnt;


/* A |HowUsed| value carries analysis results about how values are used,
   pertaining to whether we need to instrument integer adds expensively or
   not.  The running state carries a (readonly) mapping from original tmp to
   a HowUsed value for it.  A usage value can be one of three values,
   forming a 3-point chain lattice.

      HuOth   ("Other") used in some arbitrary way
       |
      HuPCa   ("PCast") used *only* in effectively a PCast, in which all
       |      we care about is the all-defined vs not-all-defined distinction
       |
      HuUnU   ("Unused") not used at all.

   The "safe" (don't-know) end of the lattice is "HuOth".  See comments
   below in |preInstrumentationAnalysis| for further details.
*/
/* DECLARED ABOVE:
typedef
   enum __attribute__((packed)) { HuUnU=0, HuPCa=1, HuOth=2 }
   HowUsed;
*/

// Not actually necessary, but we don't want to waste D1 space.
STATIC_ASSERT(sizeof(HowUsed) == 1);


/* Carries around state during memcheck instrumentation. */
typedef
   struct _MCEnv {
      /* MODIFIED: the superblock being constructed.  IRStmts are
         added. */
      IRSB* sb;
      Bool  trace;

      /* MODIFIED: a table [0 .. #temps_in_sb-1] which gives the
         current kind and possibly shadow temps for each temp in the
         IRSB being constructed.  Note that it does not contain the
         type of each tmp.  If you want to know the type, look at the
         relevant entry in sb->tyenv.  It follows that at all times
         during the instrumentation process, the valid indices for
         tmpMap and sb->tyenv are identical, being 0 .. N-1 where N is
         total number of Orig, V- and B- temps allocated so far.

         The reason for this strange split (types in one place, all
         other info in another) is that we need the types to be
         attached to sb so as to make it possible to do
         "typeOfIRExpr(mce->bb->tyenv, ...)" at various places in the
         instrumentation process. */
      XArray* /* of TempMapEnt */ tmpMap;

      /* READONLY: contains details of which ops should be expensively
         instrumented. */
      DetailLevelByOp dlbo;

      /* READONLY: for each original tmp, how the tmp is used.  This is
         computed by |preInstrumentationAnalysis|.  Valid indices are
         0 .. #temps_in_sb-1 (same as for tmpMap). */
      HowUsed* tmpHowUsed;

      /* READONLY: the guest layout.  This indicates which parts of
         the guest state should be regarded as 'always defined'. */
      const VexGuestLayout* layout;

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
   IRTemp_INVALID and we are hoping to read that shadow tmp, it means
   there's a read-before-write error in the original tmps.  The IR
   sanity checker should catch all such anomalies, however.  
*/

/* Create a new IRTemp of type 'ty' and kind 'kind', and add it to
   both the table in mce->sb and to our auxiliary mapping.  Note that
   newTemp may cause mce->tmpMap to resize, hence previous results
   from VG_(indexXA)(mce->tmpMap) are invalidated. */
static IRTemp newTemp ( MCEnv* mce, IRType ty, TempKind kind )
{
   Word       newIx;
   TempMapEnt ent;
   IRTemp     tmp = newIRTemp(mce->sb->tyenv, ty);
   ent.kind    = kind;
   ent.shadowV = IRTemp_INVALID;
   ent.shadowB = IRTemp_INVALID;
   newIx = VG_(addToXA)( mce->tmpMap, &ent );
   tl_assert(newIx == (Word)tmp);
   return tmp;
}


/* Find the tmp currently shadowing the given original tmp.  If none
   so far exists, allocate one.  */
static IRTemp findShadowTmpV ( MCEnv* mce, IRTemp orig )
{
   TempMapEnt* ent;
   /* VG_(indexXA) range-checks 'orig', hence no need to check
      here. */
   ent = (TempMapEnt*)VG_(indexXA)( mce->tmpMap, (Word)orig );
   tl_assert(ent->kind == Orig);
   if (ent->shadowV == IRTemp_INVALID) {
      IRTemp tmpV
        = newTemp( mce, shadowTypeV(mce->sb->tyenv->types[orig]), VSh );
      /* newTemp may cause mce->tmpMap to resize, hence previous results
         from VG_(indexXA) are invalid. */
      ent = (TempMapEnt*)VG_(indexXA)( mce->tmpMap, (Word)orig );
      tl_assert(ent->kind == Orig);
      tl_assert(ent->shadowV == IRTemp_INVALID);
      ent->shadowV = tmpV;
   }
   return ent->shadowV;
}

/* Allocate a new shadow for the given original tmp.  This means any
   previous shadow is abandoned.  This is needed because it is
   necessary to give a new value to a shadow once it has been tested
   for undefinedness, but unfortunately IR's SSA property disallows
   this.  Instead we must abandon the old shadow, allocate a new one
   and use that instead.

   This is the same as findShadowTmpV, except we don't bother to see
   if a shadow temp already existed -- we simply allocate a new one
   regardless. */
static void newShadowTmpV ( MCEnv* mce, IRTemp orig )
{
   TempMapEnt* ent;
   /* VG_(indexXA) range-checks 'orig', hence no need to check
      here. */
   ent = (TempMapEnt*)VG_(indexXA)( mce->tmpMap, (Word)orig );
   tl_assert(ent->kind == Orig);
   if (1) {
      IRTemp tmpV
        = newTemp( mce, shadowTypeV(mce->sb->tyenv->types[orig]), VSh );
      /* newTemp may cause mce->tmpMap to resize, hence previous results
         from VG_(indexXA) are invalid. */
      ent = (TempMapEnt*)VG_(indexXA)( mce->tmpMap, (Word)orig );
      tl_assert(ent->kind == Orig);
      ent->shadowV = tmpV;
   }
}


/*------------------------------------------------------------*/
/*--- IRAtoms -- a subset of IRExprs                       ---*/
/*------------------------------------------------------------*/

/* An atom is either an IRExpr_Const or an IRExpr_Tmp, as defined by
   isIRAtom() in libvex_ir.h.  Because this instrumenter expects flat
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
   if (a1->tag == Iex_RdTmp) {
      TempMapEnt* ent = VG_(indexXA)( mce->tmpMap, a1->Iex.RdTmp.tmp );
      return ent->kind == Orig;
   }
   return False;
}

/* (used for sanity checks only): is this an atom which looks
   like it's from shadow code? */
static Bool isShadowAtom ( MCEnv* mce, IRAtom* a1 )
{
   if (a1->tag == Iex_Const)
      return True;
   if (a1->tag == Iex_RdTmp) {
      TempMapEnt* ent = VG_(indexXA)( mce->tmpMap, a1->Iex.RdTmp.tmp );
      return ent->kind == VSh || ent->kind == BSh;
   }
   return False;
}

/* (used for sanity checks only): check that both args are atoms and
   are identically-kinded. */
static Bool sameKindedAtoms ( IRAtom* a1, IRAtom* a2 )
{
   if (a1->tag == Iex_RdTmp && a2->tag == Iex_RdTmp)
      return True;
   if (a1->tag == Iex_Const && a2->tag == Iex_Const)
      return True;
   return False;
}


/*------------------------------------------------------------*/
/*--- Type management                                      ---*/
/*------------------------------------------------------------*/

/* Shadow state is always accessed using integer types.  This returns
   an integer type with the same size (as per sizeofIRType) as the
   given type.  The only valid shadow types are Bit, I8, I16, I32,
   I64, I128, V128, V256. */

static IRType shadowTypeV ( IRType ty )
{
   switch (ty) {
      case Ity_I1:
      case Ity_I8:
      case Ity_I16:
      case Ity_I32: 
      case Ity_I64: 
      case Ity_I128: return ty;
      case Ity_F16:  return Ity_I16;
      case Ity_F32:  return Ity_I32;
      case Ity_D32:  return Ity_I32;
      case Ity_F64:  return Ity_I64;
      case Ity_D64:  return Ity_I64;
      case Ity_F128: return Ity_I128;
      case Ity_D128: return Ity_I128;
      case Ity_V128: return Ity_V128;
      case Ity_V256: return Ity_V256;
      default: ppIRType(ty); 
               VG_(tool_panic)("memcheck:shadowTypeV");
   }
}

/* Produce a 'defined' value of the given shadow type.  Should only be
   supplied shadow types (Bit/I8/I16/I32/UI64). */
static IRExpr* definedOfType ( IRType ty ) {
   switch (ty) {
      case Ity_I1:   return IRExpr_Const(IRConst_U1(False));
      case Ity_I8:   return IRExpr_Const(IRConst_U8(0));
      case Ity_I16:  return IRExpr_Const(IRConst_U16(0));
      case Ity_I32:  return IRExpr_Const(IRConst_U32(0));
      case Ity_I64:  return IRExpr_Const(IRConst_U64(0));
      case Ity_I128: return i128_const_zero();
      case Ity_V128: return IRExpr_Const(IRConst_V128(0x0000));
      case Ity_V256: return IRExpr_Const(IRConst_V256(0x00000000));
      default:       VG_(tool_panic)("memcheck:definedOfType");
   }
}


/*------------------------------------------------------------*/
/*--- Constructing IR fragments                            ---*/
/*------------------------------------------------------------*/

/* add stmt to a bb */
static inline void stmt ( HChar cat, MCEnv* mce, IRStmt* st ) {
   if (mce->trace) {
      VG_(printf)("  %c: ", cat);
      ppIRStmt(st);
      VG_(printf)("\n");
   }
   addStmtToIRSB(mce->sb, st);
}

/* assign value to tmp */
static inline 
void assign ( HChar cat, MCEnv* mce, IRTemp tmp, IRExpr* expr ) {
   stmt(cat, mce, IRStmt_WrTmp(tmp,expr));
}

/* build various kinds of expressions */
#define triop(_op, _arg1, _arg2, _arg3) \
                                 IRExpr_Triop((_op),(_arg1),(_arg2),(_arg3))
#define binop(_op, _arg1, _arg2) IRExpr_Binop((_op),(_arg1),(_arg2))
#define unop(_op, _arg)          IRExpr_Unop((_op),(_arg))
#define mkU1(_n)                 IRExpr_Const(IRConst_U1(_n))
#define mkU8(_n)                 IRExpr_Const(IRConst_U8(_n))
#define mkU16(_n)                IRExpr_Const(IRConst_U16(_n))
#define mkU32(_n)                IRExpr_Const(IRConst_U32(_n))
#define mkU64(_n)                IRExpr_Const(IRConst_U64(_n))
#define mkV128(_n)               IRExpr_Const(IRConst_V128(_n))
#define mkexpr(_tmp)             IRExpr_RdTmp((_tmp))

/* Bind the given expression to a new temporary, and return the
   temporary.  This effectively converts an arbitrary expression into
   an atom.

   'ty' is the type of 'e' and hence the type that the new temporary
   needs to be.  But passing it in is redundant, since we can deduce
   the type merely by inspecting 'e'.  So at least use that fact to
   assert that the two types agree. */
static IRAtom* assignNew ( HChar cat, MCEnv* mce, IRType ty, IRExpr* e )
{
   TempKind k;
   IRTemp   t;
   IRType   tyE = typeOfIRExpr(mce->sb->tyenv, e);

   tl_assert(tyE == ty); /* so 'ty' is redundant (!) */
   switch (cat) {
      case 'V': k = VSh;  break;
      case 'B': k = BSh;  break;
      case 'C': k = Orig; break; 
                /* happens when we are making up new "orig"
                   expressions, for IRCAS handling */
      default: tl_assert(0);
   }
   t = newTemp(mce, ty, k);
   assign(cat, mce, t, e);
   return mkexpr(t);
}


/*------------------------------------------------------------*/
/*--- Helper functions for 128-bit ops                     ---*/
/*------------------------------------------------------------*/

static IRExpr *i128_const_zero(void)
{
   IRAtom* z64 = IRExpr_Const(IRConst_U64(0));
   return binop(Iop_64HLto128, z64, z64);
}

/* There are no I128-bit loads and/or stores [as generated by any
   current front ends].  So we do not need to worry about that in
   expr2vbits_Load */


/*------------------------------------------------------------*/
/*--- Constructing definedness primitive ops               ---*/
/*------------------------------------------------------------*/

/* --------- Defined-if-either-defined --------- */

static IRAtom* mkDifD1 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I1, binop(Iop_And1, a1, a2));
}

static IRAtom* mkDifD8 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I8, binop(Iop_And8, a1, a2));
}

static IRAtom* mkDifD16 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I16, binop(Iop_And16, a1, a2));
}

static IRAtom* mkDifD32 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I32, binop(Iop_And32, a1, a2));
}

static IRAtom* mkDifD64 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I64, binop(Iop_And64, a1, a2));
}

static IRAtom* mkDifDV128 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_V128, binop(Iop_AndV128, a1, a2));
}

static IRAtom* mkDifDV256 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_V256, binop(Iop_AndV256, a1, a2));
}

/* --------- Undefined-if-either-undefined --------- */

static IRAtom* mkUifU1 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I1, binop(Iop_Or1, a1, a2));
}

static IRAtom* mkUifU8 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I8, binop(Iop_Or8, a1, a2));
}

static IRAtom* mkUifU16 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I16, binop(Iop_Or16, a1, a2));
}

static IRAtom* mkUifU32 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I32, binop(Iop_Or32, a1, a2));
}

static IRAtom* mkUifU64 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_I64, binop(Iop_Or64, a1, a2));
}

static IRAtom* mkUifU128 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   IRAtom *tmp1, *tmp2, *tmp3, *tmp4, *tmp5, *tmp6;
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   tmp1 = assignNew('V', mce, Ity_I64, unop(Iop_128to64, a1));
   tmp2 = assignNew('V', mce, Ity_I64, unop(Iop_128HIto64, a1));
   tmp3 = assignNew('V', mce, Ity_I64, unop(Iop_128to64, a2));
   tmp4 = assignNew('V', mce, Ity_I64, unop(Iop_128HIto64, a2));
   tmp5 = assignNew('V', mce, Ity_I64, binop(Iop_Or64, tmp1, tmp3));
   tmp6 = assignNew('V', mce, Ity_I64, binop(Iop_Or64, tmp2, tmp4));

   return assignNew('V', mce, Ity_I128, binop(Iop_64HLto128, tmp6, tmp5));
}

static IRAtom* mkUifUV128 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_V128, binop(Iop_OrV128, a1, a2));
}

static IRAtom* mkUifUV256 ( MCEnv* mce, IRAtom* a1, IRAtom* a2 ) {
   tl_assert(isShadowAtom(mce,a1));
   tl_assert(isShadowAtom(mce,a2));
   return assignNew('V', mce, Ity_V256, binop(Iop_OrV256, a1, a2));
}

static IRAtom* mkUifU ( MCEnv* mce, IRType vty, IRAtom* a1, IRAtom* a2 ) {
   switch (vty) {
      case Ity_I8:   return mkUifU8(mce, a1, a2);
      case Ity_I16:  return mkUifU16(mce, a1, a2);
      case Ity_I32:  return mkUifU32(mce, a1, a2);
      case Ity_I64:  return mkUifU64(mce, a1, a2);
      case Ity_I128: return mkUifU128(mce, a1, a2);
      case Ity_V128: return mkUifUV128(mce, a1, a2);
      case Ity_V256: return mkUifUV256(mce, a1, a2);
      default:
         VG_(printf)("\n"); ppIRType(vty); VG_(printf)("\n");
         VG_(tool_panic)("memcheck:mkUifU");
   }
}

/* --------- The Left-family of operations. --------- */

static IRAtom* mkLeft8 ( MCEnv* mce, IRAtom* a1 ) {
   tl_assert(isShadowAtom(mce,a1));
   return assignNew('V', mce, Ity_I8, unop(Iop_Left8, a1));
}

static IRAtom* mkLeft16 ( MCEnv* mce, IRAtom* a1 ) {
   tl_assert(isShadowAtom(mce,a1));
   return assignNew('V', mce, Ity_I16, unop(Iop_Left16, a1));
}

static IRAtom* mkLeft32 ( MCEnv* mce, IRAtom* a1 ) {
   tl_assert(isShadowAtom(mce,a1));
   return assignNew('V', mce, Ity_I32, unop(Iop_Left32, a1));
}

static IRAtom* mkLeft64 ( MCEnv* mce, IRAtom* a1 ) {
   tl_assert(isShadowAtom(mce,a1));
   return assignNew('V', mce, Ity_I64, unop(Iop_Left64, a1));
}

/* --------- The Right-family of operations. --------- */

/* Unfortunately these are a lot more expensive then their Left
   counterparts.  Fortunately they are only very rarely used -- only for
   count-leading-zeroes instrumentation. */

static IRAtom* mkRight32 ( MCEnv* mce, IRAtom* a1 )
{
   for (Int i = 1; i <= 16; i *= 2) {
      // a1 |= (a1 >>u i)
      IRAtom* tmp
         = assignNew('V', mce, Ity_I32, binop(Iop_Shr32, a1, mkU8(i)));
      a1 = assignNew('V', mce, Ity_I32, binop(Iop_Or32, a1, tmp));
   }
   return a1;
}

static IRAtom* mkRight64 ( MCEnv* mce, IRAtom* a1 )
{
   for (Int i = 1; i <= 32; i *= 2) {
      // a1 |= (a1 >>u i)
      IRAtom* tmp
         = assignNew('V', mce, Ity_I64, binop(Iop_Shr64, a1, mkU8(i)));
      a1 = assignNew('V', mce, Ity_I64, binop(Iop_Or64, a1, tmp));
   }
   return a1;
}

/* --------- 'Improvement' functions for AND/OR. --------- */

/* ImproveAND(data, vbits) = data OR vbits.  Defined (0) data 0s give
   defined (0); all other -> undefined (1).
*/
static IRAtom* mkImproveAND1 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew('V', mce, Ity_I1, binop(Iop_Or1, data, vbits));
}

static IRAtom* mkImproveAND8 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew('V', mce, Ity_I8, binop(Iop_Or8, data, vbits));
}

static IRAtom* mkImproveAND16 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew('V', mce, Ity_I16, binop(Iop_Or16, data, vbits));
}

static IRAtom* mkImproveAND32 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew('V', mce, Ity_I32, binop(Iop_Or32, data, vbits));
}

static IRAtom* mkImproveAND64 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew('V', mce, Ity_I64, binop(Iop_Or64, data, vbits));
}

static IRAtom* mkImproveANDV128 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew('V', mce, Ity_V128, binop(Iop_OrV128, data, vbits));
}

static IRAtom* mkImproveANDV256 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew('V', mce, Ity_V256, binop(Iop_OrV256, data, vbits));
}

/* ImproveOR(data, vbits) = ~data OR vbits.  Defined (0) data 1s give
   defined (0); all other -> undefined (1).
*/
static IRAtom* mkImproveOR1 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             'V', mce, Ity_I1, 
             binop(Iop_Or1, 
                   assignNew('V', mce, Ity_I1, unop(Iop_Not1, data)), 
                   vbits) );
}

static IRAtom* mkImproveOR8 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             'V', mce, Ity_I8, 
             binop(Iop_Or8, 
                   assignNew('V', mce, Ity_I8, unop(Iop_Not8, data)), 
                   vbits) );
}

static IRAtom* mkImproveOR16 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             'V', mce, Ity_I16, 
             binop(Iop_Or16, 
                   assignNew('V', mce, Ity_I16, unop(Iop_Not16, data)), 
                   vbits) );
}

static IRAtom* mkImproveOR32 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             'V', mce, Ity_I32, 
             binop(Iop_Or32, 
                   assignNew('V', mce, Ity_I32, unop(Iop_Not32, data)), 
                   vbits) );
}

static IRAtom* mkImproveOR64 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             'V', mce, Ity_I64, 
             binop(Iop_Or64, 
                   assignNew('V', mce, Ity_I64, unop(Iop_Not64, data)), 
                   vbits) );
}

static IRAtom* mkImproveORV128 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             'V', mce, Ity_V128, 
             binop(Iop_OrV128, 
                   assignNew('V', mce, Ity_V128, unop(Iop_NotV128, data)), 
                   vbits) );
}

static IRAtom* mkImproveORV256 ( MCEnv* mce, IRAtom* data, IRAtom* vbits )
{
   tl_assert(isOriginalAtom(mce, data));
   tl_assert(isShadowAtom(mce, vbits));
   tl_assert(sameKindedAtoms(data, vbits));
   return assignNew(
             'V', mce, Ity_V256, 
             binop(Iop_OrV256, 
                   assignNew('V', mce, Ity_V256, unop(Iop_NotV256, data)), 
                   vbits) );
}

/* --------- Pessimising casts. --------- */

/* The function returns an expression of type DST_TY. If any of the VBITS
   is undefined (value == 1) the resulting expression has all bits set to
   1. Otherwise, all bits are 0. */

static IRAtom* mkPCastTo( MCEnv* mce, IRType dst_ty, IRAtom* vbits ) 
{
   IRType  src_ty;
   IRAtom* tmp1;

   /* Note, dst_ty is a shadow type, not an original type. */
   tl_assert(isShadowAtom(mce,vbits));
   src_ty = typeOfIRExpr(mce->sb->tyenv, vbits);

   /* Fast-track some common cases */
   if (src_ty == Ity_I32 && dst_ty == Ity_I32)
      return assignNew('V', mce, Ity_I32, unop(Iop_CmpwNEZ32, vbits));

   if (src_ty == Ity_I64 && dst_ty == Ity_I64)
      return assignNew('V', mce, Ity_I64, unop(Iop_CmpwNEZ64, vbits));

   if (src_ty == Ity_I32 && dst_ty == Ity_I64) {
      /* PCast the arg, then clone it. */
      IRAtom* tmp = assignNew('V', mce, Ity_I32, unop(Iop_CmpwNEZ32, vbits));
      return assignNew('V', mce, Ity_I64, binop(Iop_32HLto64, tmp, tmp));
   }

   if (src_ty == Ity_I32 && dst_ty == Ity_V128) {
      /* PCast the arg, then clone it 4 times. */
      IRAtom* tmp = assignNew('V', mce, Ity_I32, unop(Iop_CmpwNEZ32, vbits));
      tmp = assignNew('V', mce, Ity_I64, binop(Iop_32HLto64, tmp, tmp));
      return assignNew('V', mce, Ity_V128, binop(Iop_64HLtoV128, tmp, tmp));
   }

   if (src_ty == Ity_I32 && dst_ty == Ity_V256) {
      /* PCast the arg, then clone it 8 times. */
      IRAtom* tmp = assignNew('V', mce, Ity_I32, unop(Iop_CmpwNEZ32, vbits));
      tmp = assignNew('V', mce, Ity_I64, binop(Iop_32HLto64, tmp, tmp));
      tmp = assignNew('V', mce, Ity_V128, binop(Iop_64HLtoV128, tmp, tmp));
      return assignNew('V', mce, Ity_V256, binop(Iop_V128HLtoV256, tmp, tmp));
   }

   if (src_ty == Ity_I64 && dst_ty == Ity_I32) {
      /* PCast the arg.  This gives all 0s or all 1s.  Then throw away
         the top half. */
      IRAtom* tmp = assignNew('V', mce, Ity_I64, unop(Iop_CmpwNEZ64, vbits));
      return assignNew('V', mce, Ity_I32, unop(Iop_64to32, tmp));
   }

   if (src_ty == Ity_V128 && dst_ty == Ity_I64) {
      /* Use InterleaveHI64x2 to copy the top half of the vector into
         the bottom half.  Then we can UifU it with the original, throw
         away the upper half of the result, and PCast-I64-to-I64
         the lower half. */
      // Generates vbits[127:64] : vbits[127:64]
      IRAtom* hi64hi64
         = assignNew('V', mce, Ity_V128,
                     binop(Iop_InterleaveHI64x2, vbits, vbits));
      // Generates
      //   UifU(vbits[127:64],vbits[127:64]) : UifU(vbits[127:64],vbits[63:0])
      //   == vbits[127:64] : UifU(vbits[127:64],vbits[63:0])
      IRAtom* lohi64 
         = mkUifUV128(mce, hi64hi64, vbits);
      // Generates UifU(vbits[127:64],vbits[63:0])
      IRAtom* lo64
         = assignNew('V', mce, Ity_I64, unop(Iop_V128to64, lohi64));
      // Generates
      //   PCast-to-I64( UifU(vbits[127:64], vbits[63:0] )
      //   == PCast-to-I64( vbits[127:0] )
      IRAtom* res
         = assignNew('V', mce, Ity_I64, unop(Iop_CmpwNEZ64, lo64));
      return res;
   }

   /* Else do it the slow way .. */
   /* First of all, collapse vbits down to a single bit. */
   tmp1   = NULL;
   switch (src_ty) {
      case Ity_I1:
         tmp1 = vbits;
         break;
      case Ity_I8: 
         tmp1 = assignNew('V', mce, Ity_I1, unop(Iop_CmpNEZ8, vbits));
         break;
      case Ity_I16: 
         tmp1 = assignNew('V', mce, Ity_I1, unop(Iop_CmpNEZ16, vbits));
         break;
      case Ity_I32: 
         tmp1 = assignNew('V', mce, Ity_I1, unop(Iop_CmpNEZ32, vbits));
         break;
      case Ity_I64: 
         tmp1 = assignNew('V', mce, Ity_I1, unop(Iop_CmpNEZ64, vbits));
         break;
      case Ity_I128: {
         /* Gah.  Chop it in half, OR the halves together, and compare
            that with zero. */
         IRAtom* tmp2 = assignNew('V', mce, Ity_I64, unop(Iop_128HIto64, vbits));
         IRAtom* tmp3 = assignNew('V', mce, Ity_I64, unop(Iop_128to64, vbits));
         IRAtom* tmp4 = assignNew('V', mce, Ity_I64, binop(Iop_Or64, tmp2, tmp3));
         tmp1         = assignNew('V', mce, Ity_I1, 
                                       unop(Iop_CmpNEZ64, tmp4));
         break;
      }
      case Ity_V128: {
         /* Chop it in half, OR the halves together, and compare that
          * with zero.
          */
         IRAtom* tmp2 = assignNew('V', mce, Ity_I64, unop(Iop_V128HIto64, vbits));
         IRAtom* tmp3 = assignNew('V', mce, Ity_I64, unop(Iop_V128to64, vbits));
         IRAtom* tmp4 = assignNew('V', mce, Ity_I64, binop(Iop_Or64, tmp2, tmp3));
         tmp1         = assignNew('V', mce, Ity_I1,
                                       unop(Iop_CmpNEZ64, tmp4));
         break;
      }
      default:
         ppIRType(src_ty);
         VG_(tool_panic)("mkPCastTo(1)");
   }
   tl_assert(tmp1);
   /* Now widen up to the dst type. */
   switch (dst_ty) {
      case Ity_I1:
         return tmp1;
      case Ity_I8: 
         return assignNew('V', mce, Ity_I8, unop(Iop_1Sto8, tmp1));
      case Ity_I16: 
         return assignNew('V', mce, Ity_I16, unop(Iop_1Sto16, tmp1));
      case Ity_I32: 
         return assignNew('V', mce, Ity_I32, unop(Iop_1Sto32, tmp1));
      case Ity_I64: 
         return assignNew('V', mce, Ity_I64, unop(Iop_1Sto64, tmp1));
      case Ity_V128:
         tmp1 = assignNew('V', mce, Ity_I64,  unop(Iop_1Sto64, tmp1));
         tmp1 = assignNew('V', mce, Ity_V128, binop(Iop_64HLtoV128, tmp1, tmp1));
         return tmp1;
      case Ity_I128:
         tmp1 = assignNew('V', mce, Ity_I64,  unop(Iop_1Sto64, tmp1));
         tmp1 = assignNew('V', mce, Ity_I128, binop(Iop_64HLto128, tmp1, tmp1));
         return tmp1;
      case Ity_V256:
         tmp1 = assignNew('V', mce, Ity_I64,  unop(Iop_1Sto64, tmp1));
         tmp1 = assignNew('V', mce, Ity_V128, binop(Iop_64HLtoV128,
                                                    tmp1, tmp1));
         tmp1 = assignNew('V', mce, Ity_V256, binop(Iop_V128HLtoV256,
                                                    tmp1, tmp1));
         return tmp1;
      default: 
         ppIRType(dst_ty);
         VG_(tool_panic)("mkPCastTo(2)");
   }
}

/* This is a minor variant.  It takes an arg of some type and returns
   a value of the same type.  The result consists entirely of Defined
   (zero) bits except its least significant bit, which is a PCast of
   the entire argument down to a single bit. */
static IRAtom* mkPCastXXtoXXlsb ( MCEnv* mce, IRAtom* varg, IRType ty )
{
   if (ty == Ity_V128) {
      /* --- Case for V128 --- */
      IRAtom* varg128 = varg;
      // generates: PCast-to-I64(varg128)
      IRAtom* pcdTo64 = mkPCastTo(mce, Ity_I64, varg128);
      // Now introduce zeros (defined bits) in the top 63 places
      // generates: Def--(63)--Def PCast-to-I1(varg128)
      IRAtom* d63pc 
         = assignNew('V', mce, Ity_I64, binop(Iop_And64, pcdTo64, mkU64(1)));
      // generates: Def--(64)--Def
      IRAtom* d64
         = definedOfType(Ity_I64);
      // generates: Def--(127)--Def PCast-to-I1(varg128)
      IRAtom* res
         = assignNew('V', mce, Ity_V128, binop(Iop_64HLtoV128, d64, d63pc));
      return res;
   }
   if (ty == Ity_I64) {
      /* --- Case for I64 --- */
      // PCast to 64
      IRAtom* pcd = mkPCastTo(mce, Ity_I64, varg);
      // Zero (Def) out the top 63 bits
      IRAtom* res 
         = assignNew('V', mce, Ity_I64, binop(Iop_And64, pcd, mkU64(1)));   
      return res;
   }
   /*NOTREACHED*/
   tl_assert(0);
}

/* --------- Optimistic casts. --------- */

/* The function takes and returns an expression of type TY. If any of the
   VBITS indicate defined (value == 0) the resulting expression has all bits
   set to 0. Otherwise, all bits are 1.  In words, if any bits are defined
   then all bits are made to be defined.

   In short we compute (vbits - (vbits >>u 1)) >>s (bitsize(vbits)-1).
*/
static IRAtom* mkOCastAt( MCEnv* mce, IRType ty, IRAtom* vbits )
{
   IROp opSUB, opSHR, opSAR;
   UInt sh;

   switch (ty) {
      case Ity_I64:
         opSUB = Iop_Sub64; opSHR = Iop_Shr64; opSAR = Iop_Sar64; sh = 63;
         break;
      case Ity_I32:
         opSUB = Iop_Sub32; opSHR = Iop_Shr32; opSAR = Iop_Sar32; sh = 31;
         break;
      case Ity_I16:
         opSUB = Iop_Sub16; opSHR = Iop_Shr16; opSAR = Iop_Sar16; sh = 15;
         break;
      case Ity_I8:
         opSUB = Iop_Sub8; opSHR = Iop_Shr8; opSAR = Iop_Sar8; sh = 7;
         break;
      default:
         ppIRType(ty);
         VG_(tool_panic)("mkOCastTo");
   }

   IRAtom *shr1, *at;
   shr1 = assignNew('V', mce,ty, binop(opSHR, vbits, mkU8(1)));
   at   = assignNew('V', mce,ty, binop(opSUB, vbits, shr1));
   at   = assignNew('V', mce,ty, binop(opSAR, at, mkU8(sh)));
   return at;
}


/* --------- Accurate interpretation of CmpEQ/CmpNE. --------- */
/* 
   Normally, we can do CmpEQ/CmpNE by doing UifU on the arguments, and
   PCasting to Ity_U1.  However, sometimes it is necessary to be more
   accurate.  The insight is that the result is defined if two
   corresponding bits can be found, one from each argument, so that
   both bits are defined but are different -- that makes EQ say "No"
   and NE say "Yes".  Hence, we compute an improvement term and DifD
   it onto the "normal" (UifU) result.

   The result is:

   PCastTo<1> (
      -- naive version
      UifU<sz>(vxx, vyy)

      `DifD<sz>`

      -- improvement term
      OCast<sz>(vec)
   )

   where
     vec contains 0 (defined) bits where the corresponding arg bits 
     are defined but different, and 1 bits otherwise.

     vec = Or<sz>( vxx,   // 0 iff bit defined
                   vyy,   // 0 iff bit defined
                   Not<sz>(Xor<sz>( xx, yy )) // 0 iff bits different
                 )

     If any bit of vec is 0, the result is defined and so the 
     improvement term should produce 0...0, else it should produce
     1...1.

     Hence require for the improvement term:

        OCast(vec) = if vec == 1...1 then 1...1 else 0...0

     which you can think of as an "optimistic cast" (OCast, the opposite of
     the normal "pessimistic cast" (PCast) family.  An OCast says all bits
     are defined if any bit is defined.

     It is possible to show that

         if vec == 1...1 then 1...1 else 0...0

     can be implemented in straight-line code as

         (vec - (vec >>u 1)) >>s (word-size-in-bits - 1)

   We note that vec contains the sub-term Or<sz>(vxx, vyy).  Since UifU is
   implemented with Or (since 1 signifies undefinedness), this is a
   duplicate of the UifU<sz>(vxx, vyy) term and so we can CSE it out, giving
   a final version of:

   let naive = UifU<sz>(vxx, vyy)
       vec   = Or<sz>(naive, Not<sz>(Xor<sz)(xx, yy))
   in
       PCastTo<1>( DifD<sz>(naive, OCast<sz>(vec)) )

   This was extensively re-analysed and checked on 6 July 05 and again
   in July 2017.
*/
static IRAtom* expensiveCmpEQorNE ( MCEnv*  mce,
                                    IRType  ty,
                                    IRAtom* vxx, IRAtom* vyy, 
                                    IRAtom* xx,  IRAtom* yy )
{
   IRAtom *naive, *vec, *improved, *final_cast;
   IROp   opDIFD, opUIFU, opOR, opXOR, opNOT;

   tl_assert(isShadowAtom(mce,vxx));
   tl_assert(isShadowAtom(mce,vyy));
   tl_assert(isOriginalAtom(mce,xx));
   tl_assert(isOriginalAtom(mce,yy));
   tl_assert(sameKindedAtoms(vxx,xx));
   tl_assert(sameKindedAtoms(vyy,yy));
 
   switch (ty) {
      case Ity_I8:
         opDIFD = Iop_And8;
         opUIFU = Iop_Or8;
         opOR   = Iop_Or8;
         opXOR  = Iop_Xor8;
         opNOT  = Iop_Not8;
         break;
      case Ity_I16:
         opDIFD = Iop_And16;
         opUIFU = Iop_Or16;
         opOR   = Iop_Or16;
         opXOR  = Iop_Xor16;
         opNOT  = Iop_Not16;
         break;
      case Ity_I32:
         opDIFD = Iop_And32;
         opUIFU = Iop_Or32;
         opOR   = Iop_Or32;
         opXOR  = Iop_Xor32;
         opNOT  = Iop_Not32;
         break;
      case Ity_I64:
         opDIFD = Iop_And64;
         opUIFU = Iop_Or64;
         opOR   = Iop_Or64;
         opXOR  = Iop_Xor64;
         opNOT  = Iop_Not64;
         break;
      default:
         VG_(tool_panic)("expensiveCmpEQorNE");
   }

   naive 
      = assignNew('V', mce, ty, binop(opUIFU, vxx, vyy));

   vec 
      = assignNew(
           'V', mce,ty, 
           binop( opOR,
                  naive,
                  assignNew(
                     'V', mce,ty,
                     unop(opNOT,
                          assignNew('V', mce,ty, binop(opXOR, xx, yy))))));

   improved
      = assignNew( 'V', mce,ty, 
                   binop(opDIFD, naive, mkOCastAt(mce, ty, vec)));

   final_cast
      = mkPCastTo( mce, Ity_I1, improved );

   return final_cast;
}


/* --------- Semi-accurate interpretation of CmpORD. --------- */

/* CmpORD32{S,U} does PowerPC-style 3-way comparisons:

      CmpORD32S(x,y) = 1<<3   if  x <s y
                     = 1<<2   if  x >s y
                     = 1<<1   if  x == y

   and similarly the unsigned variant.  The default interpretation is:

      CmpORD32{S,U}#(x,y,x#,y#) = PCast(x# `UifU` y#)  
                                  & (7<<1)

   The "& (7<<1)" reflects the fact that all result bits except 3,2,1
   are zero and therefore defined (viz, zero).

   Also deal with a special case better:

      CmpORD32S(x,0)

   Here, bit 3 (LT) of the result is a copy of the top bit of x and
   will be defined even if the rest of x isn't.  In which case we do:

      CmpORD32S#(x,x#,0,{impliedly 0}#)
         = PCast(x#) & (3<<1)      -- standard interp for GT#,EQ#
           | (x# >>u 31) << 3      -- LT# = x#[31]

   Analogous handling for CmpORD64{S,U}.
*/
static Bool isZeroU32 ( IRAtom* e )
{
   return
      toBool( e->tag == Iex_Const
              && e->Iex.Const.con->tag == Ico_U32
              && e->Iex.Const.con->Ico.U32 == 0 );
}

static Bool isZeroU64 ( IRAtom* e )
{
   return
      toBool( e->tag == Iex_Const
              && e->Iex.Const.con->tag == Ico_U64
              && e->Iex.Const.con->Ico.U64 == 0 );
}

static IRAtom* doCmpORD ( MCEnv*  mce,
                          IROp    cmp_op,
                          IRAtom* xxhash, IRAtom* yyhash, 
                          IRAtom* xx,     IRAtom* yy )
{
   Bool   m64      = cmp_op == Iop_CmpORD64S || cmp_op == Iop_CmpORD64U;
   Bool   syned    = cmp_op == Iop_CmpORD64S || cmp_op == Iop_CmpORD32S;
   IROp   opOR     = m64 ? Iop_Or64   : Iop_Or32;
   IROp   opAND    = m64 ? Iop_And64  : Iop_And32;
   IROp   opSHL    = m64 ? Iop_Shl64  : Iop_Shl32;
   IROp   opSHR    = m64 ? Iop_Shr64  : Iop_Shr32;
   IROp   op1UtoWS = m64 ? Iop_1Uto64 : Iop_1Uto32;
   IRType ty       = m64 ? Ity_I64    : Ity_I32;
   Int    width    = m64 ? 64         : 32;

   Bool (*isZero)(IRAtom*) = m64 ? isZeroU64 : isZeroU32;

   tl_assert(isShadowAtom(mce,xxhash));
   tl_assert(isShadowAtom(mce,yyhash));
   tl_assert(isOriginalAtom(mce,xx));
   tl_assert(isOriginalAtom(mce,yy));
   tl_assert(sameKindedAtoms(xxhash,xx));
   tl_assert(sameKindedAtoms(yyhash,yy));
   tl_assert(cmp_op == Iop_CmpORD32S || cmp_op == Iop_CmpORD32U
             || cmp_op == Iop_CmpORD64S || cmp_op == Iop_CmpORD64U);

   if (0) {
      ppIROp(cmp_op); VG_(printf)(" "); 
      ppIRExpr(xx); VG_(printf)(" "); ppIRExpr( yy ); VG_(printf)("\n");
   }

   if (syned && isZero(yy)) {
      /* fancy interpretation */
      /* if yy is zero, then it must be fully defined (zero#). */
      tl_assert(isZero(yyhash));
      // This is still inaccurate, but I don't think it matters, since
      // nobody writes code of the form
      // "is <partially-undefined-value> signedly greater than zero?".
      // We therefore simply declare "x >s 0" to be undefined if any bit in
      // x is undefined.  That's clearly suboptimal in some cases.  Eg, if
      // the highest order bit is a defined 1 then x is negative so it
      // doesn't matter whether the remaining bits are defined or not.
      IRAtom* t_0_gt_0_0
         = assignNew(
              'V', mce,ty,
              binop(
                 opAND,
                 mkPCastTo(mce,ty, xxhash),
                 m64 ? mkU64(1<<2) : mkU32(1<<2)
              ));
      // For "x <s 0", we can just copy the definedness of the top bit of x
      // and we have a precise result.
      IRAtom* t_lt_0_0_0
         = assignNew(
              'V', mce,ty,
              binop(
                 opSHL,
                 assignNew(
                    'V', mce,ty,
                    binop(opSHR, xxhash, mkU8(width-1))),
                 mkU8(3)
              ));
      // For "x == 0" we can hand the problem off to expensiveCmpEQorNE.
      IRAtom* t_0_0_eq_0
         = assignNew(
              'V', mce,ty,
              binop(
                 opSHL,
                 assignNew('V', mce,ty,
                    unop(
                    op1UtoWS,
                    expensiveCmpEQorNE(mce, ty, xxhash, yyhash, xx, yy))
                 ),
                 mkU8(1)
              ));
      return
         binop(
            opOR,
            assignNew('V', mce,ty, binop(opOR, t_lt_0_0_0, t_0_gt_0_0)),
            t_0_0_eq_0
         );
   } else {
      /* standard interpretation */
      IRAtom* sevenLeft1 = m64 ? mkU64(7<<1) : mkU32(7<<1);
      return 
         binop( 
            opAND, 
            mkPCastTo( mce,ty,
                       mkUifU(mce,ty, xxhash,yyhash)),
            sevenLeft1
         );
   }
}


/*------------------------------------------------------------*/
/*--- Emit a test and complaint if something is undefined. ---*/
/*------------------------------------------------------------*/

static IRAtom* schemeE ( MCEnv* mce, IRExpr* e ); /* fwds */


/* Set the annotations on a dirty helper to indicate that the stack
   pointer and instruction pointers might be read.  This is the
   behaviour of all 'emit-a-complaint' style functions we might
   call. */

static void setHelperAnns ( MCEnv* mce, IRDirty* di ) {
   di->nFxState = 2;
   di->fxState[0].fx        = Ifx_Read;
   di->fxState[0].offset    = mce->layout->offset_SP;
   di->fxState[0].size      = mce->layout->sizeof_SP;
   di->fxState[0].nRepeats  = 0;
   di->fxState[0].repeatLen = 0;
   di->fxState[1].fx        = Ifx_Read;
   di->fxState[1].offset    = mce->layout->offset_IP;
   di->fxState[1].size      = mce->layout->sizeof_IP;
   di->fxState[1].nRepeats  = 0;
   di->fxState[1].repeatLen = 0;
}


/* Check the supplied *original* |atom| for undefinedness, and emit a
   complaint if so.  Once that happens, mark it as defined.  This is
   possible because the atom is either a tmp or literal.  If it's a
   tmp, it will be shadowed by a tmp, and so we can set the shadow to
   be defined.  In fact as mentioned above, we will have to allocate a
   new tmp to carry the new 'defined' shadow value, and update the
   original->tmp mapping accordingly; we cannot simply assign a new
   value to an existing shadow tmp as this breaks SSAness.

   The checks are performed, any resulting complaint emitted, and
   |atom|'s shadow temp set to 'defined', ONLY in the case that
   |guard| evaluates to True at run-time.  If it evaluates to False
   then no action is performed.  If |guard| is NULL (the usual case)
   then it is assumed to be always-true, and hence these actions are
   performed unconditionally.

   This routine does not generate code to check the definedness of
   |guard|.  The caller is assumed to have taken care of that already.
*/
static void complainIfUndefined ( MCEnv* mce, IRAtom* atom, IRExpr *guard )
{
   IRAtom*  vatom;
   IRType   ty;
   Int      sz;
   IRDirty* di;
   IRAtom*  cond;
   IRAtom*  origin;
   void*    fn;
   const HChar* nm;
   IRExpr** args;
   Int      nargs;

   // Don't do V bit tests if we're not reporting undefined value errors.
   if (MC_(clo_mc_level) == 1)
      return;

   if (guard)
      tl_assert(isOriginalAtom(mce, guard));

   /* Since the original expression is atomic, there's no duplicated
      work generated by making multiple V-expressions for it.  So we
      don't really care about the possibility that someone else may
      also create a V-interpretion for it. */
   tl_assert(isOriginalAtom(mce, atom));
   vatom = expr2vbits( mce, atom, HuOth );
   tl_assert(isShadowAtom(mce, vatom));
   tl_assert(sameKindedAtoms(atom, vatom));

   ty = typeOfIRExpr(mce->sb->tyenv, vatom);

   /* sz is only used for constructing the error message */
   sz = ty==Ity_I1 ? 0 : sizeofIRType(ty);

   cond = mkPCastTo( mce, Ity_I1, vatom );
   /* cond will be 0 if all defined, and 1 if any not defined. */

   /* Get the origin info for the value we are about to check.  At
      least, if we are doing origin tracking.  If not, use a dummy
      zero origin. */
   if (MC_(clo_mc_level) == 3) {
      origin = schemeE( mce, atom );
      if (mce->hWordTy == Ity_I64) {
         origin = assignNew( 'B', mce, Ity_I64, unop(Iop_32Uto64, origin) );
      }
   } else {
      origin = NULL;
   }

   fn    = NULL;
   nm    = NULL;
   args  = NULL;
   nargs = -1;

   switch (sz) {
      case 0:
         if (origin) {
            fn    = &MC_(helperc_value_check0_fail_w_o);
            nm    = "MC_(helperc_value_check0_fail_w_o)";
            args  = mkIRExprVec_1(origin);
            nargs = 1;
         } else {
            fn    = &MC_(helperc_value_check0_fail_no_o);
            nm    = "MC_(helperc_value_check0_fail_no_o)";
            args  = mkIRExprVec_0();
            nargs = 0;
         }
         break;
      case 1:
         if (origin) {
            fn    = &MC_(helperc_value_check1_fail_w_o);
            nm    = "MC_(helperc_value_check1_fail_w_o)";
            args  = mkIRExprVec_1(origin);
            nargs = 1;
         } else {
            fn    = &MC_(helperc_value_check1_fail_no_o);
            nm    = "MC_(helperc_value_check1_fail_no_o)";
            args  = mkIRExprVec_0();
            nargs = 0;
         }
         break;
      case 4:
         if (origin) {
            fn    = &MC_(helperc_value_check4_fail_w_o);
            nm    = "MC_(helperc_value_check4_fail_w_o)";
            args  = mkIRExprVec_1(origin);
            nargs = 1;
         } else {
            fn    = &MC_(helperc_value_check4_fail_no_o);
            nm    = "MC_(helperc_value_check4_fail_no_o)";
            args  = mkIRExprVec_0();
            nargs = 0;
         }
         break;
      case 8:
         if (origin) {
            fn    = &MC_(helperc_value_check8_fail_w_o);
            nm    = "MC_(helperc_value_check8_fail_w_o)";
            args  = mkIRExprVec_1(origin);
            nargs = 1;
         } else {
            fn    = &MC_(helperc_value_check8_fail_no_o);
            nm    = "MC_(helperc_value_check8_fail_no_o)";
            args  = mkIRExprVec_0();
            nargs = 0;
         }
         break;
      case 2:
      case 16:
         if (origin) {
            fn    = &MC_(helperc_value_checkN_fail_w_o);
            nm    = "MC_(helperc_value_checkN_fail_w_o)";
            args  = mkIRExprVec_2( mkIRExpr_HWord( sz ), origin);
            nargs = 2;
         } else {
            fn    = &MC_(helperc_value_checkN_fail_no_o);
            nm    = "MC_(helperc_value_checkN_fail_no_o)";
            args  = mkIRExprVec_1( mkIRExpr_HWord( sz ) );
            nargs = 1;
         }
         break;
      default:
         VG_(tool_panic)("unexpected szB");
   }

   tl_assert(fn);
   tl_assert(nm);
   tl_assert(args);
   tl_assert(nargs >= 0 && nargs <= 2);
   tl_assert( (MC_(clo_mc_level) == 3 && origin != NULL)
              || (MC_(clo_mc_level) == 2 && origin == NULL) );

   di = unsafeIRDirty_0_N( nargs/*regparms*/, nm, 
                           VG_(fnptr_to_fnentry)( fn ), args );
   di->guard = cond; // and cond is PCast-to-1(atom#)

   /* If the complaint is to be issued under a guard condition, AND
      that into the guard condition for the helper call. */
   if (guard) {
      IRAtom *g1 = assignNew('V', mce, Ity_I32, unop(Iop_1Uto32, di->guard));
      IRAtom *g2 = assignNew('V', mce, Ity_I32, unop(Iop_1Uto32, guard));
      IRAtom *e  = assignNew('V', mce, Ity_I32, binop(Iop_And32, g1, g2));
      di->guard  = assignNew('V', mce, Ity_I1,  unop(Iop_32to1, e));
   }

   setHelperAnns( mce, di );
   stmt( 'V', mce, IRStmt_Dirty(di));

   /* If |atom| is shadowed by an IRTemp, set the shadow tmp to be
      defined -- but only in the case where the guard evaluates to
      True at run-time.  Do the update by setting the orig->shadow
      mapping for tmp to reflect the fact that this shadow is getting
      a new value. */
   tl_assert(isIRAtom(vatom));
   /* sameKindedAtoms ... */
   if (vatom->tag == Iex_RdTmp) {
      tl_assert(atom->tag == Iex_RdTmp);
      if (guard == NULL) {
         // guard is 'always True', hence update unconditionally
         newShadowTmpV(mce, atom->Iex.RdTmp.tmp);
         assign('V', mce, findShadowTmpV(mce, atom->Iex.RdTmp.tmp), 
                          definedOfType(ty));
      } else {
         // update the temp only conditionally.  Do this by copying
         // its old value when the guard is False.
         // The old value ..
         IRTemp old_tmpV = findShadowTmpV(mce, atom->Iex.RdTmp.tmp);
         newShadowTmpV(mce, atom->Iex.RdTmp.tmp);
         IRAtom* new_tmpV
            = assignNew('V', mce, shadowTypeV(ty),
                        IRExpr_ITE(guard, definedOfType(ty),
                                          mkexpr(old_tmpV)));
         assign('V', mce, findShadowTmpV(mce, atom->Iex.RdTmp.tmp), new_tmpV);
      }
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
   We assume here, that the definedness of GUARD has already been checked.
*/
static
void do_shadow_PUT ( MCEnv* mce,  Int offset, 
                     IRAtom* atom, IRAtom* vatom, IRExpr *guard )
{
   IRType ty;

   // Don't do shadow PUTs if we're not doing undefined value checking.
   // Their absence lets Vex's optimiser remove all the shadow computation
   // that they depend on, which includes GETs of the shadow registers.
   if (MC_(clo_mc_level) == 1)
      return;
   
   if (atom) {
      tl_assert(!vatom);
      tl_assert(isOriginalAtom(mce, atom));
      vatom = expr2vbits( mce, atom, HuOth );
   } else {
      tl_assert(vatom);
      tl_assert(isShadowAtom(mce, vatom));
   }

   ty = typeOfIRExpr(mce->sb->tyenv, vatom);
   tl_assert(ty != Ity_I1);
   if (isAlwaysDefd(mce, offset, sizeofIRType(ty))) {
      /* later: no ... */
      /* emit code to emit a complaint if any of the vbits are 1. */
      /* complainIfUndefined(mce, atom); */
   } else {
      /* Do a plain shadow Put. */
      if (guard) {
         /* If the guard expression evaluates to false we simply Put the value
            that is already stored in the guest state slot */
         IRAtom *cond, *iffalse;

         cond    = assignNew('V', mce, Ity_I1, guard);
         iffalse = assignNew('V', mce, ty,
                             IRExpr_Get(offset + mce->layout->total_sizeB, ty));
         vatom   = assignNew('V', mce, ty, IRExpr_ITE(cond, vatom, iffalse));
      }
      stmt( 'V', mce, IRStmt_Put( offset + mce->layout->total_sizeB, vatom ));
   }
}


/* Return an expression which contains the V bits corresponding to the
   given GETI (passed in in pieces). 
*/
static
void do_shadow_PUTI ( MCEnv* mce, IRPutI *puti)
{
   IRAtom* vatom;
   IRType  ty, tyS;
   Int     arrSize;;
   IRRegArray* descr = puti->descr;
   IRAtom*     ix    = puti->ix;
   Int         bias  = puti->bias;
   IRAtom*     atom  = puti->data;

   // Don't do shadow PUTIs if we're not doing undefined value checking.
   // Their absence lets Vex's optimiser remove all the shadow computation
   // that they depend on, which includes GETIs of the shadow registers.
   if (MC_(clo_mc_level) == 1)
      return;
   
   tl_assert(isOriginalAtom(mce,atom));
   vatom = expr2vbits( mce, atom, HuOth );
   tl_assert(sameKindedAtoms(atom, vatom));
   ty   = descr->elemTy;
   tyS  = shadowTypeV(ty);
   arrSize = descr->nElems * sizeofIRType(ty);
   tl_assert(ty != Ity_I1);
   tl_assert(isOriginalAtom(mce,ix));
   complainIfUndefined(mce, ix, NULL);
   if (isAlwaysDefd(mce, descr->base, arrSize)) {
      /* later: no ... */
      /* emit code to emit a complaint if any of the vbits are 1. */
      /* complainIfUndefined(mce, atom); */
   } else {
      /* Do a cloned version of the Put that refers to the shadow
         area. */
      IRRegArray* new_descr 
         = mkIRRegArray( descr->base + mce->layout->total_sizeB, 
                         tyS, descr->nElems);
      stmt( 'V', mce, IRStmt_PutI( mkIRPutI(new_descr, ix, bias, vatom) ));
   }
}


/* Return an expression which contains the V bits corresponding to the
   given GET (passed in in pieces). 
*/
static 
IRExpr* shadow_GET ( MCEnv* mce, Int offset, IRType ty )
{
   IRType tyS = shadowTypeV(ty);
   tl_assert(ty != Ity_I1);
   tl_assert(ty != Ity_I128);
   if (isAlwaysDefd(mce, offset, sizeofIRType(ty))) {
      /* Always defined, return all zeroes of the relevant type */
      return definedOfType(tyS);
   } else {
      /* return a cloned version of the Get that refers to the shadow
         area. */
      /* FIXME: this isn't an atom! */
      return IRExpr_Get( offset + mce->layout->total_sizeB, tyS );
   }
}


/* Return an expression which contains the V bits corresponding to the
   given GETI (passed in in pieces). 
*/
static
IRExpr* shadow_GETI ( MCEnv* mce, 
                      IRRegArray* descr, IRAtom* ix, Int bias )
{
   IRType ty   = descr->elemTy;
   IRType tyS  = shadowTypeV(ty);
   Int arrSize = descr->nElems * sizeofIRType(ty);
   tl_assert(ty != Ity_I1);
   tl_assert(isOriginalAtom(mce,ix));
   complainIfUndefined(mce, ix, NULL);
   if (isAlwaysDefd(mce, descr->base, arrSize)) {
      /* Always defined, return all zeroes of the relevant type */
      return definedOfType(tyS);
   } else {
      /* return a cloned version of the Get that refers to the shadow
         area. */
      IRRegArray* new_descr 
         = mkIRRegArray( descr->base + mce->layout->total_sizeB, 
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
   IRAtom* at;
   IRType t1 = typeOfIRExpr(mce->sb->tyenv, va1);
   IRType t2 = typeOfIRExpr(mce->sb->tyenv, va2);
   tl_assert(isShadowAtom(mce,va1));
   tl_assert(isShadowAtom(mce,va2));

   /* The general case is inefficient because PCast is an expensive
      operation.  Here are some special cases which use PCast only
      once rather than twice. */

   /* I64 x I64 -> I64 */
   if (t1 == Ity_I64 && t2 == Ity_I64 && finalVty == Ity_I64) {
      if (0) VG_(printf)("mkLazy2: I64 x I64 -> I64\n");
      at = mkUifU(mce, Ity_I64, va1, va2);
      at = mkPCastTo(mce, Ity_I64, at);
      return at;
   }

   /* I64 x I64 -> I32 */
   if (t1 == Ity_I64 && t2 == Ity_I64 && finalVty == Ity_I32) {
      if (0) VG_(printf)("mkLazy2: I64 x I64 -> I32\n");
      at = mkUifU(mce, Ity_I64, va1, va2);
      at = mkPCastTo(mce, Ity_I32, at);
      return at;
   }

   /* I32 x I32 -> I32 */
   if (t1 == Ity_I32 && t2 == Ity_I32 && finalVty == Ity_I32) {
      if (0) VG_(printf)("mkLazy2: I32 x I32 -> I32\n");
      at = mkUifU(mce, Ity_I32, va1, va2);
      at = mkPCastTo(mce, Ity_I32, at);
      return at;
   }

   if (0) {
      VG_(printf)("mkLazy2 ");
      ppIRType(t1);
      VG_(printf)("_");
      ppIRType(t2);
      VG_(printf)("_");
      ppIRType(finalVty);
      VG_(printf)("\n");
   }

   /* General case: force everything via 32-bit intermediaries. */
   at = mkPCastTo(mce, Ity_I32, va1);
   at = mkUifU(mce, Ity_I32, at, mkPCastTo(mce, Ity_I32, va2));
   at = mkPCastTo(mce, finalVty, at);
   return at;
}


/* 3-arg version of the above. */
static
IRAtom* mkLazy3 ( MCEnv* mce, IRType finalVty, 
                  IRAtom* va1, IRAtom* va2, IRAtom* va3 )
{
   IRAtom* at;
   IRType t1 = typeOfIRExpr(mce->sb->tyenv, va1);
   IRType t2 = typeOfIRExpr(mce->sb->tyenv, va2);
   IRType t3 = typeOfIRExpr(mce->sb->tyenv, va3);
   tl_assert(isShadowAtom(mce,va1));
   tl_assert(isShadowAtom(mce,va2));
   tl_assert(isShadowAtom(mce,va3));

   /* The general case is inefficient because PCast is an expensive
      operation.  Here are some special cases which use PCast only
      twice rather than three times. */

   /* I32 x I64 x I64 -> I64 */
   /* Standard FP idiom: rm x FParg1 x FParg2 -> FPresult */
   if (t1 == Ity_I32 && t2 == Ity_I64 && t3 == Ity_I64 
       && finalVty == Ity_I64) {
      if (0) VG_(printf)("mkLazy3: I32 x I64 x I64 -> I64\n");
      /* Widen 1st arg to I64.  Since 1st arg is typically a rounding
         mode indication which is fully defined, this should get
         folded out later. */
      at = mkPCastTo(mce, Ity_I64, va1);
      /* Now fold in 2nd and 3rd args. */
      at = mkUifU(mce, Ity_I64, at, va2);
      at = mkUifU(mce, Ity_I64, at, va3);
      /* and PCast once again. */
      at = mkPCastTo(mce, Ity_I64, at);
      return at;
   }

   /* I32 x I8 x I64 -> I64 */
   if (t1 == Ity_I32 && t2 == Ity_I8 && t3 == Ity_I64
       && finalVty == Ity_I64) {
      if (0) VG_(printf)("mkLazy3: I32 x I8 x I64 -> I64\n");
      /* Widen 1st and 2nd args to I64.  Since 1st arg is typically a
       * rounding mode indication which is fully defined, this should
       * get folded out later.
      */
      IRAtom* at1 = mkPCastTo(mce, Ity_I64, va1);
      IRAtom* at2 = mkPCastTo(mce, Ity_I64, va2);
      at = mkUifU(mce, Ity_I64, at1, at2);  // UifU(PCast(va1), PCast(va2))
      at = mkUifU(mce, Ity_I64, at, va3);
      /* and PCast once again. */
      at = mkPCastTo(mce, Ity_I64, at);
      return at;
   }

   /* I32 x I64 x I64 -> I32 */
   if (t1 == Ity_I32 && t2 == Ity_I64 && t3 == Ity_I64 
       && finalVty == Ity_I32) {
      if (0) VG_(printf)("mkLazy3: I32 x I64 x I64 -> I32\n");
      at = mkPCastTo(mce, Ity_I64, va1);
      at = mkUifU(mce, Ity_I64, at, va2);
      at = mkUifU(mce, Ity_I64, at, va3);
      at = mkPCastTo(mce, Ity_I32, at);
      return at;
   }

   /* I32 x I32 x I32 -> I32 */
   /* 32-bit FP idiom, as (eg) happens on ARM */
   if (t1 == Ity_I32 && t2 == Ity_I32 && t3 == Ity_I32 
       && finalVty == Ity_I32) {
      if (0) VG_(printf)("mkLazy3: I32 x I32 x I32 -> I32\n");
      at = va1;
      at = mkUifU(mce, Ity_I32, at, va2);
      at = mkUifU(mce, Ity_I32, at, va3);
      at = mkPCastTo(mce, Ity_I32, at);
      return at;
   }

   /* I32 x I128 x I128 -> I128 */
   /* Standard FP idiom: rm x FParg1 x FParg2 -> FPresult */
   if (t1 == Ity_I32 && t2 == Ity_I128 && t3 == Ity_I128
       && finalVty == Ity_I128) {
      if (0) VG_(printf)("mkLazy3: I32 x I128 x I128 -> I128\n");
      /* Widen 1st arg to I128.  Since 1st arg is typically a rounding
         mode indication which is fully defined, this should get
         folded out later. */
      at = mkPCastTo(mce, Ity_I128, va1);
      /* Now fold in 2nd and 3rd args. */
      at = mkUifU(mce, Ity_I128, at, va2);
      at = mkUifU(mce, Ity_I128, at, va3);
      /* and PCast once again. */
      at = mkPCastTo(mce, Ity_I128, at);
      return at;
   }

   /* I32 x I8 x I128 -> I128 */
   /* Standard FP idiom: rm x FParg1 x FParg2 -> FPresult */
   if (t1 == Ity_I32 && t2 == Ity_I8 && t3 == Ity_I128
       && finalVty == Ity_I128) {
      if (0) VG_(printf)("mkLazy3: I32 x I8 x I128 -> I128\n");
      /* Use I64 as an intermediate type, which means PCasting all 3
         args to I64 to start with. 1st arg is typically a rounding
         mode indication which is fully defined, so we hope that it
         will get folded out later. */
      IRAtom* at1 = mkPCastTo(mce, Ity_I64, va1);
      IRAtom* at2 = mkPCastTo(mce, Ity_I64, va2);
      IRAtom* at3 = mkPCastTo(mce, Ity_I64, va3);
      /* Now UifU all three together. */
      at = mkUifU(mce, Ity_I64, at1, at2);  // UifU(PCast(va1), PCast(va2))
      at = mkUifU(mce, Ity_I64, at, at3);   // ... `UifU` PCast(va3)
      /* and PCast once again. */
      at = mkPCastTo(mce, Ity_I128, at);
      return at;
   }
   if (1) {
      VG_(printf)("mkLazy3: ");
      ppIRType(t1);
      VG_(printf)(" x ");
      ppIRType(t2);
      VG_(printf)(" x ");
      ppIRType(t3);
      VG_(printf)(" -> ");
      ppIRType(finalVty);
      VG_(printf)("\n");
   }

   tl_assert(0);
   /* General case: force everything via 32-bit intermediaries. */
   /*
   at = mkPCastTo(mce, Ity_I32, va1);
   at = mkUifU(mce, Ity_I32, at, mkPCastTo(mce, Ity_I32, va2));
   at = mkUifU(mce, Ity_I32, at, mkPCastTo(mce, Ity_I32, va3));
   at = mkPCastTo(mce, finalVty, at);
   return at;
   */
}


/* 4-arg version of the above. */
static
IRAtom* mkLazy4 ( MCEnv* mce, IRType finalVty, 
                  IRAtom* va1, IRAtom* va2, IRAtom* va3, IRAtom* va4 )
{
   IRAtom* at;
   IRType t1 = typeOfIRExpr(mce->sb->tyenv, va1);
   IRType t2 = typeOfIRExpr(mce->sb->tyenv, va2);
   IRType t3 = typeOfIRExpr(mce->sb->tyenv, va3);
   IRType t4 = typeOfIRExpr(mce->sb->tyenv, va4);
   tl_assert(isShadowAtom(mce,va1));
   tl_assert(isShadowAtom(mce,va2));
   tl_assert(isShadowAtom(mce,va3));
   tl_assert(isShadowAtom(mce,va4));

   /* The general case is inefficient because PCast is an expensive
      operation.  Here are some special cases which use PCast only
      twice rather than three times. */

   /* Standard FP idiom: rm x FParg1 x FParg2 x FParg3 -> FPresult */

   if (t1 == Ity_I32 && t2 == Ity_I128 && t3 == Ity_I128 && t4 == Ity_I128
       && finalVty == Ity_I128) {
      if (0) VG_(printf)("mkLazy4: I32 x I128 x I128 x I128 -> I128\n");
      /* Widen 1st arg to I128.  Since 1st arg is typically a rounding
         mode indication which is fully defined, this should get
         folded out later. */
      at = mkPCastTo(mce, Ity_I128, va1);
      /* Now fold in 2nd, 3rd, 4th args. */
      at = mkUifU(mce, Ity_I128, at, va2);
      at = mkUifU(mce, Ity_I128, at, va3);
      at = mkUifU(mce, Ity_I128, at, va4);
      /* and PCast once again. */
      at = mkPCastTo(mce, Ity_I128, at);
      return at;
   }

   /* I32 x I64 x I64 x I64 -> I64 */
   if (t1 == Ity_I32 && t2 == Ity_I64 && t3 == Ity_I64 && t4 == Ity_I64
       && finalVty == Ity_I64) {
      if (0) VG_(printf)("mkLazy4: I32 x I64 x I64 x I64 -> I64\n");
      /* Widen 1st arg to I64.  Since 1st arg is typically a rounding
         mode indication which is fully defined, this should get
         folded out later. */
      at = mkPCastTo(mce, Ity_I64, va1);
      /* Now fold in 2nd, 3rd, 4th args. */
      at = mkUifU(mce, Ity_I64, at, va2);
      at = mkUifU(mce, Ity_I64, at, va3);
      at = mkUifU(mce, Ity_I64, at, va4);
      /* and PCast once again. */
      at = mkPCastTo(mce, Ity_I64, at);
      return at;
   }
   /* I32 x I32 x I32 x I32 -> I32 */
   /* Standard FP idiom: rm x FParg1 x FParg2 x FParg3 -> FPresult */
   if (t1 == Ity_I32 && t2 == Ity_I32 && t3 == Ity_I32 && t4 == Ity_I32
       && finalVty == Ity_I32) {
      if (0) VG_(printf)("mkLazy4: I32 x I32 x I32 x I32 -> I32\n");
      at = va1;
      /* Now fold in 2nd, 3rd, 4th args. */
      at = mkUifU(mce, Ity_I32, at, va2);
      at = mkUifU(mce, Ity_I32, at, va3);
      at = mkUifU(mce, Ity_I32, at, va4);
      at = mkPCastTo(mce, Ity_I32, at);
      return at;
   }

   if (t1 == Ity_I32 && t2 == Ity_I8 && t3 == Ity_I8 && t4 == Ity_I8
       && finalVty == Ity_I32) {
      if (0) VG_(printf)("mkLazy4: I32 x I8 x I8 x I8 -> I32\n");
      at = mkPCastTo(mce, Ity_I8, va1);
      /* Now fold in 2nd, 3rd, 4th args. */
      at = mkUifU(mce, Ity_I8, at, va2);
      at = mkUifU(mce, Ity_I8, at, va3);
      at = mkUifU(mce, Ity_I8, at, va4);
      at = mkPCastTo(mce, Ity_I32, at);
      return at;
   }

   if (t1 == Ity_I64 && t2 == Ity_I8 && t3 == Ity_I8 && t4 == Ity_I8
       && finalVty == Ity_I64) {
      if (0) VG_(printf)("mkLazy4: I64 x I8 x I8 x I8 -> I64\n");
      at = mkPCastTo(mce, Ity_I8, va1);
      /* Now fold in 2nd, 3rd, 4th args. */
      at = mkUifU(mce, Ity_I8, at, va2);
      at = mkUifU(mce, Ity_I8, at, va3);
      at = mkUifU(mce, Ity_I8, at, va4);
      at = mkPCastTo(mce, Ity_I64, at);
      return at;
   }

   if (1) {
      VG_(printf)("mkLazy4: ");
      ppIRType(t1);
      VG_(printf)(" x ");
      ppIRType(t2);
      VG_(printf)(" x ");
      ppIRType(t3);
      VG_(printf)(" x ");
      ppIRType(t4);
      VG_(printf)(" -> ");
      ppIRType(finalVty);
      VG_(printf)("\n");
   }

   tl_assert(0);
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
   Int     i;
   IRAtom* here;
   IRAtom* curr;
   IRType  mergeTy;
   Bool    mergeTy64 = True;

   /* Decide on the type of the merge intermediary.  If all relevant
      args are I64, then it's I64.  In all other circumstances, use
      I32. */
   for (i = 0; exprvec[i]; i++) {
      tl_assert(i < 32);
      tl_assert(isOriginalAtom(mce, exprvec[i]));
      if (cee->mcx_mask & (1<<i))
         continue;
      if (typeOfIRExpr(mce->sb->tyenv, exprvec[i]) != Ity_I64)
         mergeTy64 = False;
   }

   mergeTy = mergeTy64  ? Ity_I64  : Ity_I32;
   curr    = definedOfType(mergeTy);

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
         here = mkPCastTo( mce, mergeTy, expr2vbits(mce, exprvec[i], HuOth) );
         curr = mergeTy64 
                   ? mkUifU64(mce, here, curr)
                   : mkUifU32(mce, here, curr);
      }
   }
   return mkPCastTo(mce, finalVtype, curr );
}


/*------------------------------------------------------------*/
/*--- Generating expensive sequences for exact carry-chain ---*/
/*--- propagation in add/sub and related operations.       ---*/
/*------------------------------------------------------------*/

static
IRAtom* expensiveAddSub ( MCEnv*  mce,
                          Bool    add,
                          IRType  ty,
                          IRAtom* qaa, IRAtom* qbb, 
                          IRAtom* aa,  IRAtom* bb )
{
   IRAtom *a_min, *b_min, *a_max, *b_max;
   IROp   opAND, opOR, opXOR, opNOT, opADD, opSUB;

   tl_assert(isShadowAtom(mce,qaa));
   tl_assert(isShadowAtom(mce,qbb));
   tl_assert(isOriginalAtom(mce,aa));
   tl_assert(isOriginalAtom(mce,bb));
   tl_assert(sameKindedAtoms(qaa,aa));
   tl_assert(sameKindedAtoms(qbb,bb));

   switch (ty) {
      case Ity_I32:
         opAND = Iop_And32;
         opOR  = Iop_Or32;
         opXOR = Iop_Xor32;
         opNOT = Iop_Not32;
         opADD = Iop_Add32;
         opSUB = Iop_Sub32;
         break;
      case Ity_I64:
         opAND = Iop_And64;
         opOR  = Iop_Or64;
         opXOR = Iop_Xor64;
         opNOT = Iop_Not64;
         opADD = Iop_Add64;
         opSUB = Iop_Sub64;
         break;
      default:
         VG_(tool_panic)("expensiveAddSub");
   }

   // a_min = aa & ~qaa
   a_min = assignNew('V', mce,ty, 
                     binop(opAND, aa,
                                  assignNew('V', mce,ty, unop(opNOT, qaa))));

   // b_min = bb & ~qbb
   b_min = assignNew('V', mce,ty, 
                     binop(opAND, bb,
                                  assignNew('V', mce,ty, unop(opNOT, qbb))));

   // a_max = aa | qaa
   a_max = assignNew('V', mce,ty, binop(opOR, aa, qaa));

   // b_max = bb | qbb
   b_max = assignNew('V', mce,ty, binop(opOR, bb, qbb));

   if (add) {
      // result = (qaa | qbb) | ((a_min + b_min) ^ (a_max + b_max))
      return
      assignNew('V', mce,ty,
         binop( opOR,
                assignNew('V', mce,ty, binop(opOR, qaa, qbb)),
                assignNew('V', mce,ty, 
                   binop( opXOR, 
                          assignNew('V', mce,ty, binop(opADD, a_min, b_min)),
                          assignNew('V', mce,ty, binop(opADD, a_max, b_max))
                   )
                )
         )
      );
   } else {
      // result = (qaa | qbb) | ((a_min - b_max) ^ (a_max - b_min))
      return
      assignNew('V', mce,ty,
         binop( opOR,
                assignNew('V', mce,ty, binop(opOR, qaa, qbb)),
                assignNew('V', mce,ty, 
                   binop( opXOR, 
                          assignNew('V', mce,ty, binop(opSUB, a_min, b_max)),
                          assignNew('V', mce,ty, binop(opSUB, a_max, b_min))
                   )
                )
         )
      );
   }

}


static
IRAtom* expensiveCountTrailingZeroes ( MCEnv* mce, IROp czop,
                                       IRAtom* atom, IRAtom* vatom )
{
   IRType ty;
   IROp xorOp, subOp, andOp;
   IRExpr *one;
   IRAtom *improver, *improved;
   tl_assert(isShadowAtom(mce,vatom));
   tl_assert(isOriginalAtom(mce,atom));
   tl_assert(sameKindedAtoms(atom,vatom));

   switch (czop) {
      case Iop_Ctz32: case Iop_CtzNat32:
         ty = Ity_I32;
         xorOp = Iop_Xor32;
         subOp = Iop_Sub32;
         andOp = Iop_And32;
         one = mkU32(1);
         break;
      case Iop_Ctz64: case Iop_CtzNat64:
         ty = Ity_I64;
         xorOp = Iop_Xor64;
         subOp = Iop_Sub64;
         andOp = Iop_And64;
         one = mkU64(1);
         break;
      default:
         ppIROp(czop);
         VG_(tool_panic)("memcheck:expensiveCountTrailingZeroes");
   }

   // improver = atom ^ (atom - 1)
   //
   // That is, improver has its low ctz(atom)+1 bits equal to one;
   // higher bits (if any) equal to zero.  So it's exactly the right
   // mask to use to remove the irrelevant undefined input bits.
   /* Here are some examples:
         atom   = U...U 1 0...0
         atom-1 = U...U 0 1...1
         ^ed    = 0...0 1 11111, which correctly describes which bits of |atom|
                                 actually influence the result
      A boundary case
         atom   = 0...0
         atom-1 = 1...1
         ^ed    = 11111, also a correct mask for the input: all input bits
                         are relevant
      Another boundary case
         atom   = 1..1 1
         atom-1 = 1..1 0
         ^ed    = 0..0 1, also a correct mask: only the rightmost input bit
                          is relevant
      Now with misc U bits interspersed:
         atom   = U...U 1 0 U...U 0 1 0...0
         atom-1 = U...U 1 0 U...U 0 0 1...1
         ^ed    = 0...0 0 0 0...0 0 1 1...1, also correct
      (Per re-check/analysis of 14 Nov 2018)
   */
   improver = assignNew('V', mce,ty,
                        binop(xorOp,
                              atom,
                              assignNew('V', mce, ty,
                                        binop(subOp, atom, one))));

   // improved = vatom & improver
   //
   // That is, treat any V bits to the left of the rightmost ctz(atom)+1
   // bits as "defined".
   improved = assignNew('V', mce, ty,
                        binop(andOp, vatom, improver));

   // Return pessimizing cast of improved.
   return mkPCastTo(mce, ty, improved);
}

static
IRAtom* expensiveCountLeadingZeroes ( MCEnv* mce, IROp czop,
                                      IRAtom* atom, IRAtom* vatom )
{
   IRType ty;
   IROp shrOp, notOp, andOp;
   IRAtom* (*mkRight)(MCEnv*, IRAtom*);
   IRAtom *improver, *improved;
   tl_assert(isShadowAtom(mce,vatom));
   tl_assert(isOriginalAtom(mce,atom));
   tl_assert(sameKindedAtoms(atom,vatom));

   switch (czop) {
      case Iop_Clz32: case Iop_ClzNat32:
         ty = Ity_I32;
         shrOp = Iop_Shr32;
         notOp = Iop_Not32;
         andOp = Iop_And32;
         mkRight = mkRight32;
         break;
      case Iop_Clz64: case Iop_ClzNat64:
         ty = Ity_I64;
         shrOp = Iop_Shr64;
         notOp = Iop_Not64;
         andOp = Iop_And64;
         mkRight = mkRight64;
         break;
      default:
         ppIROp(czop);
         VG_(tool_panic)("memcheck:expensiveCountLeadingZeroes");
   }

   // This is in principle very similar to how expensiveCountTrailingZeroes
   // works.  That function computed an "improver", which it used to mask
   // off all but the rightmost 1-bit and the zeroes to the right of it,
   // hence removing irrelevant bits from the input.  Here, we play the
   // exact same game but with the left-vs-right roles interchanged.
   // Unfortunately calculation of the improver in this case is
   // significantly more expensive.
   //
   // improver = ~(RIGHT(atom) >>u 1)
   //
   // That is, improver has its upper clz(atom)+1 bits equal to one;
   // lower bits (if any) equal to zero.  So it's exactly the right
   // mask to use to remove the irrelevant undefined input bits.
   /* Here are some examples:
         atom             = 0...0 1 U...U
         R(atom)          = 0...0 1 1...1
         R(atom) >>u 1    = 0...0 0 1...1
         ~(R(atom) >>u 1) = 1...1 1 0...0
                            which correctly describes which bits of |atom|
                            actually influence the result
      A boundary case
         atom             = 0...0
         R(atom)          = 0...0
         R(atom) >>u 1    = 0...0
         ~(R(atom) >>u 1) = 1...1
                            also a correct mask for the input: all input bits
                            are relevant
      Another boundary case
         atom             = 1 1..1
         R(atom)          = 1 1..1
         R(atom) >>u 1    = 0 1..1
         ~(R(atom) >>u 1) = 1 0..0
                            also a correct mask: only the leftmost input bit
                            is relevant
      Now with misc U bits interspersed:
         atom             = 0...0 1 U...U 0 1 U...U
         R(atom)          = 0...0 1 1...1 1 1 1...1
         R(atom) >>u 1    = 0...0 0 1...1 1 1 1...1
         ~(R(atom) >>u 1) = 1...1 1 0...0 0 0 0...0, also correct
      (Per initial implementation of 15 Nov 2018)
   */
   improver = mkRight(mce, atom);
   improver = assignNew('V', mce, ty, binop(shrOp, improver, mkU8(1)));
   improver = assignNew('V', mce, ty, unop(notOp, improver));

   // improved = vatom & improver
   //
   // That is, treat any V bits to the right of the leftmost clz(atom)+1
   // bits as "defined".
   improved = assignNew('V', mce, ty,
                        binop(andOp, vatom, improver));

   // Return pessimizing cast of improved.
   return mkPCastTo(mce, ty, improved);
}


/*------------------------------------------------------------*/
/*--- Scalar shifts.                                       ---*/
/*------------------------------------------------------------*/

/* Produce an interpretation for (aa << bb) (or >>s, >>u).  The basic
   idea is to shift the definedness bits by the original shift amount.
   This introduces 0s ("defined") in new positions for left shifts and
   unsigned right shifts, and copies the top definedness bit for
   signed right shifts.  So, conveniently, applying the original shift
   operator to the definedness bits for the left arg is exactly the
   right thing to do:

      (qaa << bb)

   However if the shift amount is undefined then the whole result
   is undefined.  Hence need:

      (qaa << bb) `UifU` PCast(qbb)

   If the shift amount bb is a literal than qbb will say 'all defined'
   and the UifU and PCast will get folded out by post-instrumentation
   optimisation.
*/
static IRAtom* scalarShift ( MCEnv*  mce,
                             IRType  ty,
                             IROp    original_op,
                             IRAtom* qaa, IRAtom* qbb, 
                             IRAtom* aa,  IRAtom* bb )
{
   tl_assert(isShadowAtom(mce,qaa));
   tl_assert(isShadowAtom(mce,qbb));
   tl_assert(isOriginalAtom(mce,aa));
   tl_assert(isOriginalAtom(mce,bb));
   tl_assert(sameKindedAtoms(qaa,aa));
   tl_assert(sameKindedAtoms(qbb,bb));
   return 
      assignNew(
         'V', mce, ty,
         mkUifU( mce, ty,
                 assignNew('V', mce, ty, binop(original_op, qaa, bb)),
                 mkPCastTo(mce, ty, qbb)
         )
   );
}


/*------------------------------------------------------------*/
/*--- Helpers for dealing with vector primops.             ---*/
/*------------------------------------------------------------*/

/* Vector pessimisation -- pessimise within each lane individually. */

static IRAtom* mkPCast8x16 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V128, unop(Iop_CmpNEZ8x16, at));
}

static IRAtom* mkPCast16x8 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V128, unop(Iop_CmpNEZ16x8, at));
}

static IRAtom* mkPCast32x4 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V128, unop(Iop_CmpNEZ32x4, at));
}

static IRAtom* mkPCast64x2 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V128, unop(Iop_CmpNEZ64x2, at));
}

static IRAtom* mkPCast128x1 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V128, unop(Iop_CmpNEZ128x1, at));
}

static IRAtom* mkPCast64x4 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V256, unop(Iop_CmpNEZ64x4, at));
}

static IRAtom* mkPCast32x8 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V256, unop(Iop_CmpNEZ32x8, at));
}

static IRAtom* mkPCast32x2 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_I64, unop(Iop_CmpNEZ32x2, at));
}

static IRAtom* mkPCast16x16 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V256, unop(Iop_CmpNEZ16x16, at));
}

static IRAtom* mkPCast16x4 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_I64, unop(Iop_CmpNEZ16x4, at));
}

static IRAtom* mkPCast8x32 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_V256, unop(Iop_CmpNEZ8x32, at));
}

static IRAtom* mkPCast8x8 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_I64, unop(Iop_CmpNEZ8x8, at));
}

static IRAtom* mkPCast16x2 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_I32, unop(Iop_CmpNEZ16x2, at));
}

static IRAtom* mkPCast8x4 ( MCEnv* mce, IRAtom* at )
{
   return assignNew('V', mce, Ity_I32, unop(Iop_CmpNEZ8x4, at));
}


/* Here's a simple scheme capable of handling ops derived from SSE1
   code and while only generating ops that can be efficiently
   implemented in SSE1. */

/* All-lanes versions are straightforward:

   binary32Fx4(x,y)   ==> PCast32x4(UifUV128(x#,y#))

   unary32Fx4(x,y)    ==> PCast32x4(x#)

   Lowest-lane-only versions are more complex:

   binary32F0x4(x,y)  ==> SetV128lo32(
                             x#, 
                             PCast32(V128to32(UifUV128(x#,y#))) 
                          )

   This is perhaps not so obvious.  In particular, it's faster to
   do a V128-bit UifU and then take the bottom 32 bits than the more
   obvious scheme of taking the bottom 32 bits of each operand
   and doing a 32-bit UifU.  Basically since UifU is fast and 
   chopping lanes off vector values is slow.

   Finally:

   unary32F0x4(x)     ==> SetV128lo32(
                             x#, 
                             PCast32(V128to32(x#)) 
                          )

   Where:

   PCast32(v#)   = 1Sto32(CmpNE32(v#,0))
   PCast32x4(v#) = CmpNEZ32x4(v#)
*/

static
IRAtom* binary32Fx4 ( MCEnv* mce, IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   tl_assert(isShadowAtom(mce, vatomY));
   at = mkUifUV128(mce, vatomX, vatomY);
   at = assignNew('V', mce, Ity_V128, mkPCast32x4(mce, at));
   return at;
}

static
IRAtom* unary32Fx4 ( MCEnv* mce, IRAtom* vatomX )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   at = assignNew('V', mce, Ity_V128, mkPCast32x4(mce, vatomX));
   return at;
}

static
IRAtom* binary32F0x4 ( MCEnv* mce, IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   tl_assert(isShadowAtom(mce, vatomY));
   at = mkUifUV128(mce, vatomX, vatomY);
   at = assignNew('V', mce, Ity_I32, unop(Iop_V128to32, at));
   at = mkPCastTo(mce, Ity_I32, at);
   at = assignNew('V', mce, Ity_V128, binop(Iop_SetV128lo32, vatomX, at));
   return at;
}

static
IRAtom* unary32F0x4 ( MCEnv* mce, IRAtom* vatomX )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   at = assignNew('V', mce, Ity_I32, unop(Iop_V128to32, vatomX));
   at = mkPCastTo(mce, Ity_I32, at);
   at = assignNew('V', mce, Ity_V128, binop(Iop_SetV128lo32, vatomX, at));
   return at;
}

/* --- ... and ... 64Fx2 versions of the same ... --- */

static
IRAtom* binary64Fx2 ( MCEnv* mce, IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   tl_assert(isShadowAtom(mce, vatomY));
   at = mkUifUV128(mce, vatomX, vatomY);
   at = assignNew('V', mce, Ity_V128, mkPCast64x2(mce, at));
   return at;
}

static
IRAtom* unary64Fx2 ( MCEnv* mce, IRAtom* vatomX )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   at = assignNew('V', mce, Ity_V128, mkPCast64x2(mce, vatomX));
   return at;
}

static
IRAtom* binary64F0x2 ( MCEnv* mce, IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   tl_assert(isShadowAtom(mce, vatomY));
   at = mkUifUV128(mce, vatomX, vatomY);
   at = assignNew('V', mce, Ity_I64, unop(Iop_V128to64, at));
   at = mkPCastTo(mce, Ity_I64, at);
   at = assignNew('V', mce, Ity_V128, binop(Iop_SetV128lo64, vatomX, at));
   return at;
}

static
IRAtom* unary64F0x2 ( MCEnv* mce, IRAtom* vatomX )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   at = assignNew('V', mce, Ity_I64, unop(Iop_V128to64, vatomX));
   at = mkPCastTo(mce, Ity_I64, at);
   at = assignNew('V', mce, Ity_V128, binop(Iop_SetV128lo64, vatomX, at));
   return at;
}

/* --- --- ... and ... 32Fx2 versions of the same --- --- */

static
IRAtom* binary32Fx2 ( MCEnv* mce, IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   tl_assert(isShadowAtom(mce, vatomY));
   at = mkUifU64(mce, vatomX, vatomY);
   at = assignNew('V', mce, Ity_I64, mkPCast32x2(mce, at));
   return at;
}

static
IRAtom* unary32Fx2 ( MCEnv* mce, IRAtom* vatomX )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   at = assignNew('V', mce, Ity_I64, mkPCast32x2(mce, vatomX));
   return at;
}

/* --- ... and ... 64Fx4 versions of the same ... --- */

static
IRAtom* binary64Fx4 ( MCEnv* mce, IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   tl_assert(isShadowAtom(mce, vatomY));
   at = mkUifUV256(mce, vatomX, vatomY);
   at = assignNew('V', mce, Ity_V256, mkPCast64x4(mce, at));
   return at;
}

static
IRAtom* unary64Fx4 ( MCEnv* mce, IRAtom* vatomX )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   at = assignNew('V', mce, Ity_V256, mkPCast64x4(mce, vatomX));
   return at;
}

/* --- ... and ... 32Fx8 versions of the same ... --- */

static
IRAtom* binary32Fx8 ( MCEnv* mce, IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   tl_assert(isShadowAtom(mce, vatomY));
   at = mkUifUV256(mce, vatomX, vatomY);
   at = assignNew('V', mce, Ity_V256, mkPCast32x8(mce, at));
   return at;
}

static
IRAtom* unary32Fx8 ( MCEnv* mce, IRAtom* vatomX )
{
   IRAtom* at;
   tl_assert(isShadowAtom(mce, vatomX));
   at = assignNew('V', mce, Ity_V256, mkPCast32x8(mce, vatomX));
   return at;
}

/* --- 64Fx2 binary FP ops, with rounding mode --- */

static
IRAtom* binary64Fx2_w_rm ( MCEnv* mce, IRAtom* vRM,
                                       IRAtom* vatomX, IRAtom* vatomY )
{
   /* This is the same as binary64Fx2, except that we subsequently
      pessimise vRM (definedness of the rounding mode), widen to 128
      bits and UifU it into the result.  As with the scalar cases, if
      the RM is a constant then it is defined and so this extra bit
      will get constant-folded out later. */
   // "do" the vector args
   IRAtom* t1 = binary64Fx2(mce, vatomX, vatomY);
   // PCast the RM, and widen it to 128 bits
   IRAtom* t2 = mkPCastTo(mce, Ity_V128, vRM);
   // Roll it into the result
   t1 = mkUifUV128(mce, t1, t2);
   return t1;
}

/* --- ... and ... 32Fx4 versions of the same --- */

static
IRAtom* binary32Fx4_w_rm ( MCEnv* mce, IRAtom* vRM,
                                       IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* t1 = binary32Fx4(mce, vatomX, vatomY);
   // PCast the RM, and widen it to 128 bits
   IRAtom* t2 = mkPCastTo(mce, Ity_V128, vRM);
   // Roll it into the result
   t1 = mkUifUV128(mce, t1, t2);
   return t1;
}

/* --- ... and ... 64Fx4 versions of the same --- */

static
IRAtom* binary64Fx4_w_rm ( MCEnv* mce, IRAtom* vRM,
                                       IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* t1 = binary64Fx4(mce, vatomX, vatomY);
   // PCast the RM, and widen it to 256 bits
   IRAtom* t2 = mkPCastTo(mce, Ity_V256, vRM);
   // Roll it into the result
   t1 = mkUifUV256(mce, t1, t2);
   return t1;
}

/* --- ... and ... 32Fx8 versions of the same --- */

static
IRAtom* binary32Fx8_w_rm ( MCEnv* mce, IRAtom* vRM,
                                       IRAtom* vatomX, IRAtom* vatomY )
{
   IRAtom* t1 = binary32Fx8(mce, vatomX, vatomY);
   // PCast the RM, and widen it to 256 bits
   IRAtom* t2 = mkPCastTo(mce, Ity_V256, vRM);
   // Roll it into the result
   t1 = mkUifUV256(mce, t1, t2);
   return t1;
}

/* --- 64Fx2 unary FP ops, with rounding mode --- */

static
IRAtom* unary64Fx2_w_rm ( MCEnv* mce, IRAtom* vRM, IRAtom* vatomX )
{
   /* Same scheme as binary64Fx2_w_rm. */
   // "do" the vector arg
   IRAtom* t1 = unary64Fx2(mce, vatomX);
   // PCast the RM, and widen it to 128 bits
   IRAtom* t2 = mkPCastTo(mce, Ity_V128, vRM);
   // Roll it into the result
   t1 = mkUifUV128(mce, t1, t2);
   return t1;
}

/* --- ... and ... 32Fx4 versions of the same --- */

static
IRAtom* unary32Fx4_w_rm ( MCEnv* mce, IRAtom* vRM, IRAtom* vatomX )
{
   /* Same scheme as binaryFx4_w_rm. */
   IRAtom* t1 = unary32Fx4(mce, vatomX);
   // PCast the RM, and widen it to 128 bits
   IRAtom* t2 = mkPCastTo(mce, Ity_V128, vRM);
   // Roll it into the result
   t1 = mkUifUV128(mce, t1, t2);
   return t1;
}

/* --- ... and ... 32Fx8 versions of the same --- */

static
IRAtom* unary32Fx8_w_rm ( MCEnv* mce, IRAtom* vRM, IRAtom* vatomX )
{
   /* Same scheme as unary32Fx8_w_rm. */
   IRAtom* t1 = unary32Fx8(mce, vatomX);
   // PCast the RM, and widen it to 256 bits
   IRAtom* t2 = mkPCastTo(mce, Ity_V256, vRM);
   // Roll it into the result
   t1 = mkUifUV256(mce, t1, t2);
   return t1;
}


/* --- --- Vector saturated narrowing --- --- */

/* We used to do something very clever here, but on closer inspection
   (2011-Jun-15), and in particular bug #279698, it turns out to be
   wrong.  Part of the problem came from the fact that for a long
   time, the IR primops to do with saturated narrowing were
   underspecified and managed to confuse multiple cases which needed
   to be separate: the op names had a signedness qualifier, but in
   fact the source and destination signednesses needed to be specified
   independently, so the op names really need two independent
   signedness specifiers.

   As of 2011-Jun-15 (ish) the underspecification was sorted out
   properly.  The incorrect instrumentation remained, though.  That
   has now (2011-Oct-22) been fixed.

   What we now do is simple:

   Let the original narrowing op be QNarrowBinXtoYxZ, where Z is a
   number of lanes, X is the source lane width and signedness, and Y
   is the destination lane width and signedness.  In all cases the
   destination lane width is half the source lane width, so the names
   have a bit of redundancy, but are at least easy to read.

   For example, Iop_QNarrowBin32Sto16Ux8 narrows 8 lanes of signed 32s
   to unsigned 16s.

   Let Vanilla(OP) be a function that takes OP, one of these
   saturating narrowing ops, and produces the same "shaped" narrowing
   op which is not saturating, but merely dumps the most significant
   bits.  "same shape" means that the lane numbers and widths are the
   same as with OP.

   For example, Vanilla(Iop_QNarrowBin32Sto16Ux8) 
                  = Iop_NarrowBin32to16x8,
   that is, narrow 8 lanes of 32 bits to 8 lanes of 16 bits, by
   dumping the top half of each lane.

   So, with that in place, the scheme is simple, and it is simple to
   pessimise each lane individually and then apply Vanilla(OP) so as
   to get the result in the right "shape".  If the original OP is
   QNarrowBinXtoYxZ then we produce

   Vanilla(OP)( PCast-X-to-X-x-Z(vatom1), PCast-X-to-X-x-Z(vatom2) )

   or for the case when OP is unary (Iop_QNarrowUn*)

   Vanilla(OP)( PCast-X-to-X-x-Z(vatom) )
*/
static
IROp vanillaNarrowingOpOfShape ( IROp qnarrowOp )
{
   switch (qnarrowOp) {
      /* Binary: (128, 128) -> 128 */
      case Iop_QNarrowBin16Sto8Ux16:
      case Iop_QNarrowBin16Sto8Sx16:
      case Iop_QNarrowBin16Uto8Ux16:
      case Iop_QNarrowBin64Sto32Sx4:
      case Iop_QNarrowBin64Uto32Ux4:
         return Iop_NarrowBin16to8x16;
      case Iop_QNarrowBin32Sto16Ux8:
      case Iop_QNarrowBin32Sto16Sx8:
      case Iop_QNarrowBin32Uto16Ux8:
         return Iop_NarrowBin32to16x8;
      /* Binary: (64, 64) -> 64 */
      case Iop_QNarrowBin32Sto16Sx4:
         return Iop_NarrowBin32to16x4;
      case Iop_QNarrowBin16Sto8Ux8:
      case Iop_QNarrowBin16Sto8Sx8:
         return Iop_NarrowBin16to8x8;
      /* Unary: 128 -> 64 */
      case Iop_QNarrowUn64Uto32Ux2:
      case Iop_QNarrowUn64Sto32Sx2:
      case Iop_QNarrowUn64Sto32Ux2:
         return Iop_NarrowUn64to32x2;
      case Iop_QNarrowUn32Uto16Ux4:
      case Iop_QNarrowUn32Sto16Sx4:
      case Iop_QNarrowUn32Sto16Ux4:
      case Iop_F32toF16x4_DEP:
         return Iop_NarrowUn32to16x4;
      case Iop_QNarrowUn16Uto8Ux8:
      case Iop_QNarrowUn16Sto8Sx8:
      case Iop_QNarrowUn16Sto8Ux8:
         return Iop_NarrowUn16to8x8;
      default: 
         ppIROp(qnarrowOp);
         VG_(tool_panic)("vanillaNarrowOpOfShape");
   }
}

static
IRAtom* vectorNarrowBinV128 ( MCEnv* mce, IROp narrow_op, 
                              IRAtom* vatom1, IRAtom* vatom2)
{
   IRAtom *at1, *at2, *at3;
   IRAtom* (*pcast)( MCEnv*, IRAtom* );
   switch (narrow_op) {
      case Iop_QNarrowBin64Sto32Sx4: pcast = mkPCast32x4; break;
      case Iop_QNarrowBin64Uto32Ux4: pcast = mkPCast32x4; break;
      case Iop_QNarrowBin32Sto16Sx8: pcast = mkPCast32x4; break;
      case Iop_QNarrowBin32Uto16Ux8: pcast = mkPCast32x4; break;
      case Iop_QNarrowBin32Sto16Ux8: pcast = mkPCast32x4; break;
      case Iop_QNarrowBin16Sto8Sx16: pcast = mkPCast16x8; break;
      case Iop_QNarrowBin16Uto8Ux16: pcast = mkPCast16x8; break;
      case Iop_QNarrowBin16Sto8Ux16: pcast = mkPCast16x8; break;
      default: VG_(tool_panic)("vectorNarrowBinV128");
   }
   IROp vanilla_narrow = vanillaNarrowingOpOfShape(narrow_op);
   tl_assert(isShadowAtom(mce,vatom1));
   tl_assert(isShadowAtom(mce,vatom2));
   at1 = assignNew('V', mce, Ity_V128, pcast(mce, vatom1));
   at2 = assignNew('V', mce, Ity_V128, pcast(mce, vatom2));
   at3 = assignNew('V', mce, Ity_V128, binop(vanilla_narrow, at1, at2));
   return at3;
}

static
IRAtom* vectorNarrowBin64 ( MCEnv* mce, IROp narrow_op, 
                            IRAtom* vatom1, IRAtom* vatom2)
{
   IRAtom *at1, *at2, *at3;
   IRAtom* (*pcast)( MCEnv*, IRAtom* );
   switch (narrow_op) {
      case Iop_QNarrowBin32Sto16Sx4: pcast = mkPCast32x2; break;
      case Iop_QNarrowBin16Sto8Sx8:  pcast = mkPCast16x4; break;
      case Iop_QNarrowBin16Sto8Ux8:  pcast = mkPCast16x4; break;
      default: VG_(tool_panic)("vectorNarrowBin64");
   }
   IROp vanilla_narrow = vanillaNarrowingOpOfShape(narrow_op);
   tl_assert(isShadowAtom(mce,vatom1));
   tl_assert(isShadowAtom(mce,vatom2));
   at1 = assignNew('V', mce, Ity_I64, pcast(mce, vatom1));
   at2 = assignNew('V', mce, Ity_I64, pcast(mce, vatom2));
   at3 = assignNew('V', mce, Ity_I64, binop(vanilla_narrow, at1, at2));
   return at3;
}

static
IRAtom* vectorNarrowUnV128 ( MCEnv* mce, IROp narrow_op,
                             IRAtom* vatom1)
{
   IRAtom *at1, *at2;
   IRAtom* (*pcast)( MCEnv*, IRAtom* );
   tl_assert(isShadowAtom(mce,vatom1));
   /* For vanilla narrowing (non-saturating), we can just apply
      the op directly to the V bits. */
   switch (narrow_op) {
      case Iop_NarrowUn16to8x8:
      case Iop_NarrowUn32to16x4:
      case Iop_NarrowUn64to32x2:
      case Iop_F32toF16x4_DEP:
         at1 = assignNew('V', mce, Ity_I64, unop(narrow_op, vatom1));
         return at1;
      default:
         break; /* Do Plan B */
   }
   /* Plan B: for ops that involve a saturation operation on the args,
      we must PCast before the vanilla narrow. */
   switch (narrow_op) {
      case Iop_QNarrowUn16Sto8Sx8:  pcast = mkPCast16x8; break;
      case Iop_QNarrowUn16Sto8Ux8:  pcast = mkPCast16x8; break;
      case Iop_QNarrowUn16Uto8Ux8:  pcast = mkPCast16x8; break;
      case Iop_QNarrowUn32Sto16Sx4: pcast = mkPCast32x4; break;
      case Iop_QNarrowUn32Sto16Ux4: pcast = mkPCast32x4; break;
      case Iop_QNarrowUn32Uto16Ux4: pcast = mkPCast32x4; break;
      case Iop_QNarrowUn64Sto32Sx2: pcast = mkPCast64x2; break;
      case Iop_QNarrowUn64Sto32Ux2: pcast = mkPCast64x2; break;
      case Iop_QNarrowUn64Uto32Ux2: pcast = mkPCast64x2; break;
      default: VG_(tool_panic)("vectorNarrowUnV128");
   }
   IROp vanilla_narrow = vanillaNarrowingOpOfShape(narrow_op);
   at1 = assignNew('V', mce, Ity_V128, pcast(mce, vatom1));
   at2 = assignNew('V', mce, Ity_I64, unop(vanilla_narrow, at1));
   return at2;
}

static
IRAtom* vectorWidenI64 ( MCEnv* mce, IROp longen_op,
                         IRAtom* vatom1)
{
   IRAtom *at1, *at2;
   IRAtom* (*pcast)( MCEnv*, IRAtom* );
   switch (longen_op) {
      case Iop_Widen8Uto16x8:  pcast = mkPCast16x8; break;
      case Iop_Widen8Sto16x8:  pcast = mkPCast16x8; break;
      case Iop_Widen16Uto32x4: pcast = mkPCast32x4; break;
      case Iop_Widen16Sto32x4: pcast = mkPCast32x4; break;
      case Iop_Widen32Uto64x2: pcast = mkPCast64x2; break;
      case Iop_Widen32Sto64x2: pcast = mkPCast64x2; break;
      case Iop_F16toF32x4:     pcast = mkPCast32x4; break;
      default: VG_(tool_panic)("vectorWidenI64");
   }
   tl_assert(isShadowAtom(mce,vatom1));
   at1 = assignNew('V', mce, Ity_V128, unop(longen_op, vatom1));
   at2 = assignNew('V', mce, Ity_V128, pcast(mce, at1));
   return at2;
}


/* --- --- Vector integer arithmetic --- --- */

/* Simple ... UifU the args and per-lane pessimise the results. */

/* --- V256-bit versions --- */

static
IRAtom* binary8Ix32 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV256(mce, vatom1, vatom2);
   at = mkPCast8x32(mce, at);
   return at;   
}

static
IRAtom* binary16Ix16 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV256(mce, vatom1, vatom2);
   at = mkPCast16x16(mce, at);
   return at;   
}

static
IRAtom* binary32Ix8 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV256(mce, vatom1, vatom2);
   at = mkPCast32x8(mce, at);
   return at;   
}

static
IRAtom* binary64Ix4 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV256(mce, vatom1, vatom2);
   at = mkPCast64x4(mce, at);
   return at;   
}

/* --- V128-bit versions --- */

static
IRAtom* binary8Ix16 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV128(mce, vatom1, vatom2);
   at = mkPCast8x16(mce, at);
   return at;   
}

static
IRAtom* binary16Ix8 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV128(mce, vatom1, vatom2);
   at = mkPCast16x8(mce, at);
   return at;   
}

static
IRAtom* binary32Ix4 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV128(mce, vatom1, vatom2);
   at = mkPCast32x4(mce, at);
   return at;   
}

static
IRAtom* binary64Ix2 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV128(mce, vatom1, vatom2);
   at = mkPCast64x2(mce, at);
   return at;   
}

static
IRAtom* binary128Ix1 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifUV128(mce, vatom1, vatom2);
   at = mkPCast128x1(mce, at);
   return at;
}

/* --- 64-bit versions --- */

static
IRAtom* binary8Ix8 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifU64(mce, vatom1, vatom2);
   at = mkPCast8x8(mce, at);
   return at;   
}

static
IRAtom* binary16Ix4 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifU64(mce, vatom1, vatom2);
   at = mkPCast16x4(mce, at);
   return at;   
}

static
IRAtom* binary32Ix2 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifU64(mce, vatom1, vatom2);
   at = mkPCast32x2(mce, at);
   return at;   
}

static
IRAtom* binary64Ix1 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifU64(mce, vatom1, vatom2);
   at = mkPCastTo(mce, Ity_I64, at);
   return at;
}

/* --- 32-bit versions --- */

static
IRAtom* binary8Ix4 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifU32(mce, vatom1, vatom2);
   at = mkPCast8x4(mce, at);
   return at;   
}

static
IRAtom* binary16Ix2 ( MCEnv* mce, IRAtom* vatom1, IRAtom* vatom2 )
{
   IRAtom* at;
   at = mkUifU32(mce, vatom1, vatom2);
   at = mkPCast16x2(mce, at);
   return at;   
}


/*------------------------------------------------------------*/
/*--- Generate shadow values from all kinds of IRExprs.    ---*/
/*------------------------------------------------------------*/

static 
IRAtom* expr2vbits_Qop ( MCEnv* mce,
                         IROp op,
                         IRAtom* atom1, IRAtom* atom2, 
                         IRAtom* atom3, IRAtom* atom4 )
{
   IRAtom* vatom1 = expr2vbits( mce, atom1, HuOth );
   IRAtom* vatom2 = expr2vbits( mce, atom2, HuOth );
   IRAtom* vatom3 = expr2vbits( mce, atom3, HuOth );
   IRAtom* vatom4 = expr2vbits( mce, atom4, HuOth );

   tl_assert(isOriginalAtom(mce,atom1));
   tl_assert(isOriginalAtom(mce,atom2));
   tl_assert(isOriginalAtom(mce,atom3));
   tl_assert(isOriginalAtom(mce,atom4));
   tl_assert(isShadowAtom(mce,vatom1));
   tl_assert(isShadowAtom(mce,vatom2));
   tl_assert(isShadowAtom(mce,vatom3));
   tl_assert(isShadowAtom(mce,vatom4));
   tl_assert(sameKindedAtoms(atom1,vatom1));
   tl_assert(sameKindedAtoms(atom2,vatom2));
   tl_assert(sameKindedAtoms(atom3,vatom3));
   tl_assert(sameKindedAtoms(atom4,vatom4));
   switch (op) {
      case Iop_MAddF64:
      case Iop_MAddF64r32:
      case Iop_MSubF64:
      case Iop_MSubF64r32:
         /* I32(rm) x F64 x F64 x F64 -> F64 */
         return mkLazy4(mce, Ity_I64, vatom1, vatom2, vatom3, vatom4);

      case Iop_MAddF32:
      case Iop_MSubF32:
         /* I32(rm) x F32 x F32 x F32 -> F32 */
         return mkLazy4(mce, Ity_I32, vatom1, vatom2, vatom3, vatom4);

      case Iop_MAddF128:
      case Iop_MSubF128:
      case Iop_NegMAddF128:
      case Iop_NegMSubF128:
         /* I32(rm) x F128 x F128 x F128 -> F128 */
         return mkLazy4(mce, Ity_I128, vatom1, vatom2, vatom3, vatom4);

      /* V256-bit data-steering */
      case Iop_64x4toV256:
         return assignNew('V', mce, Ity_V256,
                          IRExpr_Qop(op, vatom1, vatom2, vatom3, vatom4));

      /* I32/I64 x I8 x I8 x I8 -> I32/I64 */
      case Iop_Rotx32:
         return mkLazy4(mce, Ity_I32, vatom1, vatom2, vatom3, vatom4);
      case Iop_Rotx64:
         return mkLazy4(mce, Ity_I64, vatom1, vatom2, vatom3, vatom4);
      default:
         ppIROp(op);
         VG_(tool_panic)("memcheck:expr2vbits_Qop");
   }
}


static 
IRAtom* expr2vbits_Triop ( MCEnv* mce,
                           IROp op,
                           IRAtom* atom1, IRAtom* atom2, IRAtom* atom3 )
{
   IRAtom* vatom1 = expr2vbits( mce, atom1, HuOth );
   IRAtom* vatom2 = expr2vbits( mce, atom2, HuOth );
   IRAtom* vatom3 = expr2vbits( mce, atom3, HuOth );

   tl_assert(isOriginalAtom(mce,atom1));
   tl_assert(isOriginalAtom(mce,atom2));
   tl_assert(isOriginalAtom(mce,atom3));
   tl_assert(isShadowAtom(mce,vatom1));
   tl_assert(isShadowAtom(mce,vatom2));
   tl_assert(isShadowAtom(mce,vatom3));
   tl_assert(sameKindedAtoms(atom1,vatom1));
   tl_assert(sameKindedAtoms(atom2,vatom2));
   tl_assert(sameKindedAtoms(atom3,vatom3));
   switch (op) {
      case Iop_AddF128:
      case Iop_SubF128:
      case Iop_MulF128:
      case Iop_DivF128:
      case Iop_AddD128:
      case Iop_SubD128:
      case Iop_MulD128:
      case Iop_DivD128:
      case Iop_QuantizeD128:
         /* I32(rm) x F128/D128 x F128/D128 -> F128/D128 */
         return mkLazy3(mce, Ity_I128, vatom1, vatom2, vatom3);
      case Iop_AddF64:
      case Iop_AddD64:
      case Iop_AddF64r32:
      case Iop_SubF64:
      case Iop_SubD64:
      case Iop_SubF64r32:
      case Iop_MulF64:
      case Iop_MulD64:
      case Iop_MulF64r32:
      case Iop_DivF64:
      case Iop_DivD64:
      case Iop_DivF64r32:
      case Iop_ScaleF64:
      case Iop_Yl2xF64:
      case Iop_Yl2xp1F64:
      case Iop_AtanF64:
      case Iop_PRemF64:
      case Iop_PRem1F64:
      case Iop_QuantizeD64:
         /* I32(rm) x F64/D64 x F64/D64 -> F64/D64 */
         return mkLazy3(mce, Ity_I64, vatom1, vatom2, vatom3);
      case Iop_PRemC3210F64:
      case Iop_PRem1C3210F64:
         /* I32(rm) x F64 x F64 -> I32 */
         return mkLazy3(mce, Ity_I32, vatom1, vatom2, vatom3);
      case Iop_AddF32:
      case Iop_SubF32:
      case Iop_MulF32:
      case Iop_DivF32:
         /* I32(rm) x F32 x F32 -> I32 */
         return mkLazy3(mce, Ity_I32, vatom1, vatom2, vatom3);
      case Iop_SignificanceRoundD64:
         /* IRRoundingMode(I32) x I8 x D64 -> D64 */
         return mkLazy3(mce, Ity_I64, vatom1, vatom2, vatom3);
      case Iop_SignificanceRoundD128:
         /* IRRoundingMode(I32) x I8 x D128 -> D128 */
         return mkLazy3(mce, Ity_I128, vatom1, vatom2, vatom3);
      case Iop_SliceV128:
         /* (V128, V128, I8) -> V128 */
         complainIfUndefined(mce, atom3, NULL);
         return assignNew('V', mce, Ity_V128, triop(op, vatom1, vatom2, atom3));
      case Iop_Slice64:
         /* (I64, I64, I8) -> I64 */
         complainIfUndefined(mce, atom3, NULL);
         return assignNew('V', mce, Ity_I64, triop(op, vatom1, vatom2, atom3));
      case Iop_SetElem8x8:
      case Iop_SetElem16x4:
      case Iop_SetElem32x2:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I64, triop(op, vatom1, atom2, vatom3));

      case Iop_SetElem8x16:
      case Iop_SetElem16x8:
      case Iop_SetElem32x4:
      case Iop_SetElem64x2:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_V128, triop(op, vatom1, atom2, vatom3));

      case Iop_Perm8x16x2:
         /* (V128, V128, V128) -> V128 */
            complainIfUndefined(mce, atom3, NULL);
            return mkUifUV128(
                   mce,
                   assignNew('V', mce, Ity_V128, triop(op, vatom1, vatom2, atom3)),
                   mkPCast8x16(mce, vatom3)
                );

      /* Vector FP with rounding mode as the first arg */
      case Iop_Add64Fx2:
      case Iop_Sub64Fx2:
      case Iop_Mul64Fx2:
      case Iop_Div64Fx2:
      case Iop_Scale2_64Fx2:
         return binary64Fx2_w_rm(mce, vatom1, vatom2, vatom3);

      case Iop_Add32Fx4:
      case Iop_Sub32Fx4:
      case Iop_Mul32Fx4:
      case Iop_Div32Fx4:
      case Iop_Scale2_32Fx4:
        return binary32Fx4_w_rm(mce, vatom1, vatom2, vatom3);

      case Iop_Add64Fx4:
      case Iop_Sub64Fx4:
      case Iop_Mul64Fx4:
      case Iop_Div64Fx4:
         return binary64Fx4_w_rm(mce, vatom1, vatom2, vatom3);

      case Iop_Add32Fx8:
      case Iop_Sub32Fx8:
      case Iop_Mul32Fx8:
      case Iop_Div32Fx8:
         return binary32Fx8_w_rm(mce, vatom1, vatom2, vatom3);

      case Iop_F32x4_2toQ16x8:
         return assignNew('V', mce, Ity_V128,
                          binop(Iop_PackEvenLanes16x8,
                                unary32Fx4_w_rm(mce, vatom1, vatom2),
                                unary32Fx4_w_rm(mce, vatom1, vatom3)));
      case Iop_F64x2_2toQ32x4:
         return assignNew('V', mce, Ity_V128,
                          binop(Iop_PackEvenLanes32x4,
                                unary64Fx2_w_rm(mce, vatom1, vatom2),
                                unary64Fx2_w_rm(mce, vatom1, vatom3)));


      default:
         ppIROp(op);
         VG_(tool_panic)("memcheck:expr2vbits_Triop");
   }
}


static 
IRAtom* expr2vbits_Binop ( MCEnv* mce,
                           IROp op,
                           IRAtom* atom1, IRAtom* atom2,
                           HowUsed hu/*use HuOth if unknown*/ )
{
   IRType  and_or_ty = Ity_INVALID;
   IRAtom* (*uifu)    (MCEnv*, IRAtom*, IRAtom*) = NULL;
   IRAtom* (*difd)    (MCEnv*, IRAtom*, IRAtom*) = NULL;
   IRAtom* (*improve) (MCEnv*, IRAtom*, IRAtom*) = NULL;

   IRAtom* vatom1 = expr2vbits( mce, atom1, HuOth );
   IRAtom* vatom2 = expr2vbits( mce, atom2, HuOth );

   tl_assert(isOriginalAtom(mce,atom1));
   tl_assert(isOriginalAtom(mce,atom2));
   tl_assert(isShadowAtom(mce,vatom1));
   tl_assert(isShadowAtom(mce,vatom2));
   tl_assert(sameKindedAtoms(atom1,vatom1));
   tl_assert(sameKindedAtoms(atom2,vatom2));
   switch (op) {

      /* 32-bit SIMD */

      case Iop_Add16x2:
      case Iop_HAdd16Ux2:
      case Iop_HAdd16Sx2:
      case Iop_Sub16x2:
      case Iop_HSub16Ux2:
      case Iop_HSub16Sx2:
      case Iop_QAdd16Sx2:
      case Iop_QSub16Sx2:
      case Iop_QSub16Ux2:
      case Iop_QAdd16Ux2:
         return binary16Ix2(mce, vatom1, vatom2);

      case Iop_Add8x4:
      case Iop_HAdd8Ux4:
      case Iop_HAdd8Sx4:
      case Iop_Sub8x4:
      case Iop_HSub8Ux4:
      case Iop_HSub8Sx4:
      case Iop_QSub8Ux4:
      case Iop_QAdd8Ux4:
      case Iop_QSub8Sx4:
      case Iop_QAdd8Sx4:
         return binary8Ix4(mce, vatom1, vatom2);

      /* 64-bit SIMD */

      case Iop_ShrN8x8:
      case Iop_ShrN16x4:
      case Iop_ShrN32x2:
      case Iop_SarN8x8:
      case Iop_SarN16x4:
      case Iop_SarN32x2:
      case Iop_ShlN16x4:
      case Iop_ShlN32x2:
      case Iop_ShlN8x8:
         /* Same scheme as with all other shifts. */
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I64, binop(op, vatom1, atom2));

      case Iop_QNarrowBin32Sto16Sx4:
      case Iop_QNarrowBin16Sto8Sx8:
      case Iop_QNarrowBin16Sto8Ux8:
         return vectorNarrowBin64(mce, op, vatom1, vatom2);

      case Iop_Min8Ux8:
      case Iop_Min8Sx8:
      case Iop_Max8Ux8:
      case Iop_Max8Sx8:
      case Iop_Avg8Ux8:
      case Iop_QSub8Sx8:
      case Iop_QSub8Ux8:
      case Iop_Sub8x8:
      case Iop_CmpGT8Sx8:
      case Iop_CmpGT8Ux8:
      case Iop_CmpEQ8x8:
      case Iop_QAdd8Sx8:
      case Iop_QAdd8Ux8:
      case Iop_QSal8x8:
      case Iop_QShl8x8:
      case Iop_Add8x8:
      case Iop_Mul8x8:
      case Iop_PolynomialMul8x8:
         return binary8Ix8(mce, vatom1, vatom2);

      case Iop_Min16Sx4:
      case Iop_Min16Ux4:
      case Iop_Max16Sx4:
      case Iop_Max16Ux4:
      case Iop_Avg16Ux4:
      case Iop_QSub16Ux4:
      case Iop_QSub16Sx4:
      case Iop_Sub16x4:
      case Iop_Mul16x4:
      case Iop_MulHi16Sx4:
      case Iop_MulHi16Ux4:
      case Iop_CmpGT16Sx4:
      case Iop_CmpGT16Ux4:
      case Iop_CmpEQ16x4:
      case Iop_QAdd16Sx4:
      case Iop_QAdd16Ux4:
      case Iop_QSal16x4:
      case Iop_QShl16x4:
      case Iop_Add16x4:
      case Iop_QDMulHi16Sx4:
      case Iop_QRDMulHi16Sx4:
         return binary16Ix4(mce, vatom1, vatom2);

      case Iop_Sub32x2:
      case Iop_Mul32x2:
      case Iop_Max32Sx2:
      case Iop_Max32Ux2:
      case Iop_Min32Sx2:
      case Iop_Min32Ux2:
      case Iop_CmpGT32Sx2:
      case Iop_CmpGT32Ux2:
      case Iop_CmpEQ32x2:
      case Iop_Add32x2:
      case Iop_QAdd32Ux2:
      case Iop_QAdd32Sx2:
      case Iop_QSub32Ux2:
      case Iop_QSub32Sx2:
      case Iop_QSal32x2:
      case Iop_QShl32x2:
      case Iop_QDMulHi32Sx2:
      case Iop_QRDMulHi32Sx2:
         return binary32Ix2(mce, vatom1, vatom2);

      case Iop_QSub64Ux1:
      case Iop_QSub64Sx1:
      case Iop_QAdd64Ux1:
      case Iop_QAdd64Sx1:
      case Iop_QSal64x1:
      case Iop_QShl64x1:
      case Iop_Sal64x1:
         return binary64Ix1(mce, vatom1, vatom2);

      case Iop_QShlNsatSU8x8:
      case Iop_QShlNsatUU8x8:
      case Iop_QShlNsatSS8x8:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast8x8(mce, vatom1);

      case Iop_QShlNsatSU16x4:
      case Iop_QShlNsatUU16x4:
      case Iop_QShlNsatSS16x4:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast16x4(mce, vatom1);

      case Iop_QShlNsatSU32x2:
      case Iop_QShlNsatUU32x2:
      case Iop_QShlNsatSS32x2:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast32x2(mce, vatom1);

      case Iop_QShlNsatSU64x1:
      case Iop_QShlNsatUU64x1:
      case Iop_QShlNsatSS64x1:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast32x2(mce, vatom1);

      case Iop_PwMax32Sx2:
      case Iop_PwMax32Ux2:
      case Iop_PwMin32Sx2:
      case Iop_PwMin32Ux2:
      case Iop_PwMax32Fx2:
      case Iop_PwMin32Fx2:
         return assignNew('V', mce, Ity_I64,
                          binop(Iop_PwMax32Ux2, 
                                mkPCast32x2(mce, vatom1),
                                mkPCast32x2(mce, vatom2)));

      case Iop_PwMax16Sx4:
      case Iop_PwMax16Ux4:
      case Iop_PwMin16Sx4:
      case Iop_PwMin16Ux4:
         return assignNew('V', mce, Ity_I64,
                          binop(Iop_PwMax16Ux4,
                                mkPCast16x4(mce, vatom1),
                                mkPCast16x4(mce, vatom2)));

      case Iop_PwMax8Sx8:
      case Iop_PwMax8Ux8:
      case Iop_PwMin8Sx8:
      case Iop_PwMin8Ux8:
         return assignNew('V', mce, Ity_I64,
                          binop(Iop_PwMax8Ux8,
                                mkPCast8x8(mce, vatom1),
                                mkPCast8x8(mce, vatom2)));

      case Iop_PwAdd32x2:
      case Iop_PwAdd32Fx2:
         return mkPCast32x2(mce,
               assignNew('V', mce, Ity_I64,
                         binop(Iop_PwAdd32x2,
                               mkPCast32x2(mce, vatom1),
                               mkPCast32x2(mce, vatom2))));

      case Iop_PwAdd16x4:
         return mkPCast16x4(mce,
               assignNew('V', mce, Ity_I64,
                         binop(op, mkPCast16x4(mce, vatom1),
                                   mkPCast16x4(mce, vatom2))));

      case Iop_PwAdd8x8:
         return mkPCast8x8(mce,
               assignNew('V', mce, Ity_I64,
                         binop(op, mkPCast8x8(mce, vatom1),
                                   mkPCast8x8(mce, vatom2))));

      case Iop_Shl8x8:
      case Iop_Shr8x8:
      case Iop_Sar8x8:
      case Iop_Sal8x8:
         return mkUifU64(mce,
                   assignNew('V', mce, Ity_I64, binop(op, vatom1, atom2)),
                   mkPCast8x8(mce,vatom2)
                );

      case Iop_Shl16x4:
      case Iop_Shr16x4:
      case Iop_Sar16x4:
      case Iop_Sal16x4:
         return mkUifU64(mce,
                   assignNew('V', mce, Ity_I64, binop(op, vatom1, atom2)),
                   mkPCast16x4(mce,vatom2)
                );

      case Iop_Shl32x2:
      case Iop_Shr32x2:
      case Iop_Sar32x2:
      case Iop_Sal32x2:
         return mkUifU64(mce,
                   assignNew('V', mce, Ity_I64, binop(op, vatom1, atom2)),
                   mkPCast32x2(mce,vatom2)
                );

      /* 64-bit data-steering */
      case Iop_InterleaveLO32x2:
      case Iop_InterleaveLO16x4:
      case Iop_InterleaveLO8x8:
      case Iop_InterleaveHI32x2:
      case Iop_InterleaveHI16x4:
      case Iop_InterleaveHI8x8:
      case Iop_CatOddLanes8x8:
      case Iop_CatEvenLanes8x8:
      case Iop_CatOddLanes16x4:
      case Iop_CatEvenLanes16x4:
      case Iop_InterleaveOddLanes8x8:
      case Iop_InterleaveEvenLanes8x8:
      case Iop_InterleaveOddLanes16x4:
      case Iop_InterleaveEvenLanes16x4:
         return assignNew('V', mce, Ity_I64, binop(op, vatom1, vatom2));

      case Iop_GetElem8x8:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I8, binop(op, vatom1, atom2));
      case Iop_GetElem16x4:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I16, binop(op, vatom1, atom2));
      case Iop_GetElem32x2:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I32, binop(op, vatom1, atom2));

      /* Perm8x8: rearrange values in left arg using steering values from
         right arg.  So rearrange the vbits in the same way but pessimise wrt
         steering values.  We assume that unused bits in the steering value
         are defined zeros, so we can safely PCast within each lane of the the
         steering value without having to take precautions to avoid a
         dependency on those unused bits.

         This is also correct for PermOrZero8x8, but it is a bit subtle.  For
         each lane, if bit 7 of the steering value is zero, then we'll steer
         the shadow value exactly as per Perm8x8.  If that bit is one, then
         the operation will set the resulting (concrete) value to zero.  That
         means it is defined, and should have a shadow value of zero.  Hence
         in both cases (bit 7 is 0 or 1) we can self-shadow (in the same way
         as Perm8x8) and then pessimise against the steering values.  */
      case Iop_Perm8x8:
      case Iop_PermOrZero8x8:
         return mkUifU64(
                   mce,
                   assignNew('V', mce, Ity_I64, binop(op, vatom1, atom2)),
                   mkPCast8x8(mce, vatom2)
                );

      /* V128-bit SIMD */

      case Iop_I32StoF32x4:
      case Iop_F32toI32Sx4:
      case Iop_Sqrt32Fx4:
         return unary32Fx4_w_rm(mce, vatom1, vatom2);
      case Iop_Sqrt64Fx2:
         return unary64Fx2_w_rm(mce, vatom1, vatom2);

      case Iop_ShrN8x16:
      case Iop_ShrN16x8:
      case Iop_ShrN32x4:
      case Iop_ShrN64x2:
      case Iop_SarN8x16:
      case Iop_SarN16x8:
      case Iop_SarN32x4:
      case Iop_SarN64x2:
      case Iop_ShlN8x16:
      case Iop_ShlN16x8:
      case Iop_ShlN32x4:
      case Iop_ShlN64x2:
         /* Same scheme as with all other shifts.  Note: 22 Oct 05:
            this is wrong now, scalar shifts are done properly lazily.
            Vector shifts should be fixed too. */
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2));

      /* V x V shifts/rotates are done using the standard lazy scheme. */
      /* For the non-rounding variants of bi-di vector x vector
         shifts (the Iop_Sh.. ops, that is) we use the lazy scheme.
         But note that this is overly pessimistic, because in fact only
         the bottom 8 bits of each lane of the second argument are taken
         into account when shifting.  So really we ought to ignore
         undefinedness in bits 8 and above of each lane in the
         second argument. */
      case Iop_Shl8x16:
      case Iop_Shr8x16:
      case Iop_Sar8x16:
      case Iop_Sal8x16:
      case Iop_Rol8x16:
      case Iop_Sh8Sx16:
      case Iop_Sh8Ux16:
         return mkUifUV128(mce,
                   assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2)),
                   mkPCast8x16(mce,vatom2)
                );

      case Iop_Shl16x8:
      case Iop_Shr16x8:
      case Iop_Sar16x8:
      case Iop_Sal16x8:
      case Iop_Rol16x8:
      case Iop_Sh16Sx8:
      case Iop_Sh16Ux8:
         return mkUifUV128(mce,
                   assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2)),
                   mkPCast16x8(mce,vatom2)
                );

      case Iop_Shl32x4:
      case Iop_Shr32x4:
      case Iop_Sar32x4:
      case Iop_Sal32x4:
      case Iop_Rol32x4:
      case Iop_Sh32Sx4:
      case Iop_Sh32Ux4:
         return mkUifUV128(mce,
                   assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2)),
                   mkPCast32x4(mce,vatom2)
                );

      case Iop_Shl64x2:
      case Iop_Shr64x2:
      case Iop_Sar64x2:
      case Iop_Sal64x2:
      case Iop_Rol64x2:
      case Iop_Sh64Sx2:
      case Iop_Sh64Ux2:
         return mkUifUV128(mce,
                   assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2)),
                   mkPCast64x2(mce,vatom2)
                );

      /* For the rounding variants of bi-di vector x vector shifts, the
         rounding adjustment can cause undefinedness to propagate through
         the entire lane, in the worst case.  Too complex to handle 
         properly .. just UifU the arguments and then PCast them.
         Suboptimal but safe. */
      case Iop_Rsh8Sx16:
      case Iop_Rsh8Ux16:
         return binary8Ix16(mce, vatom1, vatom2);
      case Iop_Rsh16Sx8:
      case Iop_Rsh16Ux8:
         return binary16Ix8(mce, vatom1, vatom2);
      case Iop_Rsh32Sx4:
      case Iop_Rsh32Ux4:
         return binary32Ix4(mce, vatom1, vatom2);
      case Iop_Rsh64Sx2:
      case Iop_Rsh64Ux2:
         return binary64Ix2(mce, vatom1, vatom2);

      case Iop_F32ToFixed32Ux4_RZ:
      case Iop_F32ToFixed32Sx4_RZ:
      case Iop_Fixed32UToF32x4_RN:
      case Iop_Fixed32SToF32x4_RN:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast32x4(mce, vatom1);

      case Iop_F32ToFixed32Ux2_RZ:
      case Iop_F32ToFixed32Sx2_RZ:
      case Iop_Fixed32UToF32x2_RN:
      case Iop_Fixed32SToF32x2_RN:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast32x2(mce, vatom1);

      case Iop_QSub8Ux16:
      case Iop_QSub8Sx16:
      case Iop_Sub8x16:
      case Iop_Min8Ux16:
      case Iop_Min8Sx16:
      case Iop_Max8Ux16:
      case Iop_Max8Sx16:
      case Iop_CmpGT8Sx16:
      case Iop_CmpGT8Ux16:
      case Iop_CmpEQ8x16:
      case Iop_Avg8Ux16:
      case Iop_Avg8Sx16:
      case Iop_QAdd8Ux16:
      case Iop_QAdd8Sx16:
      case Iop_QAddExtUSsatSS8x16:
      case Iop_QAddExtSUsatUU8x16:
      case Iop_QSal8x16:
      case Iop_QShl8x16:
      case Iop_Add8x16:
      case Iop_Mul8x16:
      case Iop_MulHi8Sx16:
      case Iop_MulHi8Ux16:
      case Iop_PolynomialMul8x16:
      case Iop_PolynomialMulAdd8x16:
         return binary8Ix16(mce, vatom1, vatom2);

      case Iop_QSub16Ux8:
      case Iop_QSub16Sx8:
      case Iop_Sub16x8:
      case Iop_Mul16x8:
      case Iop_MulHi16Sx8:
      case Iop_MulHi16Ux8:
      case Iop_Min16Sx8:
      case Iop_Min16Ux8:
      case Iop_Max16Sx8:
      case Iop_Max16Ux8:
      case Iop_CmpGT16Sx8:
      case Iop_CmpGT16Ux8:
      case Iop_CmpEQ16x8:
      case Iop_Avg16Ux8:
      case Iop_Avg16Sx8:
      case Iop_QAdd16Ux8:
      case Iop_QAdd16Sx8:
      case Iop_QAddExtUSsatSS16x8:
      case Iop_QAddExtSUsatUU16x8:
      case Iop_QSal16x8:
      case Iop_QShl16x8:
      case Iop_Add16x8:
      case Iop_QDMulHi16Sx8:
      case Iop_QRDMulHi16Sx8:
      case Iop_PolynomialMulAdd16x8:
      /* PwExtUSMulQAdd8x16 is a bit subtle.  The effect of it is that each
         16-bit chunk of the output is formed from corresponding 16-bit chunks
         of the input args, so we can treat it like an other binary 16x8
         operation.  That's despite it having '8x16' in its name. */
      case Iop_PwExtUSMulQAdd8x16:
         return binary16Ix8(mce, vatom1, vatom2);

      case Iop_Sub32x4:
      case Iop_CmpGT32Sx4:
      case Iop_CmpGT32Ux4:
      case Iop_CmpEQ32x4:
      case Iop_QAdd32Sx4:
      case Iop_QAdd32Ux4:
      case Iop_QSub32Sx4:
      case Iop_QSub32Ux4:
      case Iop_QAddExtUSsatSS32x4:
      case Iop_QAddExtSUsatUU32x4:
      case Iop_QSal32x4:
      case Iop_QShl32x4:
      case Iop_Avg32Ux4:
      case Iop_Avg32Sx4:
      case Iop_Add32x4:
      case Iop_Max32Ux4:
      case Iop_Max32Sx4:
      case Iop_Min32Ux4:
      case Iop_Min32Sx4:
      case Iop_Mul32x4:
      case Iop_MulHi32Sx4:
      case Iop_MulHi32Ux4:
      case Iop_QDMulHi32Sx4:
      case Iop_QRDMulHi32Sx4:
      case Iop_PolynomialMulAdd32x4:
         return binary32Ix4(mce, vatom1, vatom2);

      case Iop_Sub64x2:
      case Iop_Add64x2:
      case Iop_Avg64Ux2:
      case Iop_Avg64Sx2:
      case Iop_Max64Sx2:
      case Iop_Max64Ux2:
      case Iop_Min64Sx2:
      case Iop_Min64Ux2:
      case Iop_CmpEQ64x2:
      case Iop_CmpGT64Sx2:
      case Iop_CmpGT64Ux2:
      case Iop_QSal64x2:
      case Iop_QShl64x2:
      case Iop_QAdd64Ux2:
      case Iop_QAdd64Sx2:
      case Iop_QSub64Ux2:
      case Iop_QSub64Sx2:
      case Iop_QAddExtUSsatSS64x2:
      case Iop_QAddExtSUsatUU64x2:
      case Iop_PolynomialMulAdd64x2:
      case Iop_CipherV128:
      case Iop_CipherLV128:
      case Iop_NCipherV128:
      case Iop_NCipherLV128:
      case Iop_MulI128by10E:
      case Iop_MulI128by10ECarry:
        return binary64Ix2(mce, vatom1, vatom2);

      case Iop_Add128x1:
      case Iop_Sub128x1:
      case Iop_CmpNEZ128x1:
         return binary128Ix1(mce, vatom1, vatom2);

      case Iop_QNarrowBin64Sto32Sx4:
      case Iop_QNarrowBin64Uto32Ux4:
      case Iop_QNarrowBin32Sto16Sx8:
      case Iop_QNarrowBin32Uto16Ux8:
      case Iop_QNarrowBin32Sto16Ux8:
      case Iop_QNarrowBin16Sto8Sx16:
      case Iop_QNarrowBin16Uto8Ux16:
      case Iop_QNarrowBin16Sto8Ux16:
         return vectorNarrowBinV128(mce, op, vatom1, vatom2);

      case Iop_Min64Fx2:
      case Iop_Max64Fx2:
      case Iop_CmpLT64Fx2:
      case Iop_CmpLE64Fx2:
      case Iop_CmpEQ64Fx2:
      case Iop_CmpUN64Fx2:
      case Iop_RecipStep64Fx2:
      case Iop_RSqrtStep64Fx2:
         return binary64Fx2(mce, vatom1, vatom2);      

      case Iop_Sub64F0x2:
      case Iop_Mul64F0x2:
      case Iop_Min64F0x2:
      case Iop_Max64F0x2:
      case Iop_Div64F0x2:
      case Iop_CmpLT64F0x2:
      case Iop_CmpLE64F0x2:
      case Iop_CmpEQ64F0x2:
      case Iop_CmpUN64F0x2:
      case Iop_Add64F0x2:
         return binary64F0x2(mce, vatom1, vatom2);      

      case Iop_Min32Fx4:
      case Iop_Max32Fx4:
      case Iop_CmpLT32Fx4:
      case Iop_CmpLE32Fx4:
      case Iop_CmpEQ32Fx4:
      case Iop_CmpUN32Fx4:
      case Iop_CmpGT32Fx4:
      case Iop_CmpGE32Fx4:
      case Iop_RecipStep32Fx4:
      case Iop_RSqrtStep32Fx4:
         return binary32Fx4(mce, vatom1, vatom2);      

      case Iop_Sub32Fx2:
      case Iop_Mul32Fx2:
      case Iop_Min32Fx2:
      case Iop_Max32Fx2:
      case Iop_CmpEQ32Fx2:
      case Iop_CmpGT32Fx2:
      case Iop_CmpGE32Fx2:
      case Iop_Add32Fx2:
      case Iop_RecipStep32Fx2:
      case Iop_RSqrtStep32Fx2:
         return binary32Fx2(mce, vatom1, vatom2);      

      case Iop_Sub32F0x4:
      case Iop_Mul32F0x4:
      case Iop_Min32F0x4:
      case Iop_Max32F0x4:
      case Iop_Div32F0x4:
      case Iop_CmpLT32F0x4:
      case Iop_CmpLE32F0x4:
      case Iop_CmpEQ32F0x4:
      case Iop_CmpUN32F0x4:
      case Iop_Add32F0x4:
         return binary32F0x4(mce, vatom1, vatom2);      

      case Iop_QShlNsatSU8x16:
      case Iop_QShlNsatUU8x16:
      case Iop_QShlNsatSS8x16:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast8x16(mce, vatom1);

      case Iop_QShlNsatSU16x8:
      case Iop_QShlNsatUU16x8:
      case Iop_QShlNsatSS16x8:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast16x8(mce, vatom1);

      case Iop_QShlNsatSU32x4:
      case Iop_QShlNsatUU32x4:
      case Iop_QShlNsatSS32x4:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast32x4(mce, vatom1);

      case Iop_QShlNsatSU64x2:
      case Iop_QShlNsatUU64x2:
      case Iop_QShlNsatSS64x2:
         complainIfUndefined(mce, atom2, NULL);
         return mkPCast32x4(mce, vatom1);

      /* Q-and-Qshift-by-imm-and-narrow of the form (V128, I8) -> V128.
         To make this simpler, do the following:
         * complain if the shift amount (the I8) is undefined
         * pcast each lane at the wide width
         * truncate each lane to half width
         * pcast the resulting 64-bit value to a single bit and use
           that as the least significant bit of the upper half of the
           result. */
      case Iop_QandQShrNnarrow64Uto32Ux2:
      case Iop_QandQSarNnarrow64Sto32Sx2:
      case Iop_QandQSarNnarrow64Sto32Ux2:
      case Iop_QandQRShrNnarrow64Uto32Ux2:
      case Iop_QandQRSarNnarrow64Sto32Sx2:
      case Iop_QandQRSarNnarrow64Sto32Ux2:
      case Iop_QandQShrNnarrow32Uto16Ux4:
      case Iop_QandQSarNnarrow32Sto16Sx4:
      case Iop_QandQSarNnarrow32Sto16Ux4:
      case Iop_QandQRShrNnarrow32Uto16Ux4:
      case Iop_QandQRSarNnarrow32Sto16Sx4:
      case Iop_QandQRSarNnarrow32Sto16Ux4:
      case Iop_QandQShrNnarrow16Uto8Ux8:
      case Iop_QandQSarNnarrow16Sto8Sx8:
      case Iop_QandQSarNnarrow16Sto8Ux8:
      case Iop_QandQRShrNnarrow16Uto8Ux8:
      case Iop_QandQRSarNnarrow16Sto8Sx8:
      case Iop_QandQRSarNnarrow16Sto8Ux8:
      {
         IRAtom* (*fnPessim) (MCEnv*, IRAtom*) = NULL;
         IROp opNarrow = Iop_INVALID;
         switch (op) {
            case Iop_QandQShrNnarrow64Uto32Ux2:
            case Iop_QandQSarNnarrow64Sto32Sx2:
            case Iop_QandQSarNnarrow64Sto32Ux2:
            case Iop_QandQRShrNnarrow64Uto32Ux2:
            case Iop_QandQRSarNnarrow64Sto32Sx2:
            case Iop_QandQRSarNnarrow64Sto32Ux2:
               fnPessim = mkPCast64x2;
               opNarrow = Iop_NarrowUn64to32x2;
               break;
            case Iop_QandQShrNnarrow32Uto16Ux4:
            case Iop_QandQSarNnarrow32Sto16Sx4:
            case Iop_QandQSarNnarrow32Sto16Ux4:
            case Iop_QandQRShrNnarrow32Uto16Ux4:
            case Iop_QandQRSarNnarrow32Sto16Sx4:
            case Iop_QandQRSarNnarrow32Sto16Ux4:
               fnPessim = mkPCast32x4;
               opNarrow = Iop_NarrowUn32to16x4;
               break;
            case Iop_QandQShrNnarrow16Uto8Ux8:
            case Iop_QandQSarNnarrow16Sto8Sx8:
            case Iop_QandQSarNnarrow16Sto8Ux8:
            case Iop_QandQRShrNnarrow16Uto8Ux8:
            case Iop_QandQRSarNnarrow16Sto8Sx8:
            case Iop_QandQRSarNnarrow16Sto8Ux8:
               fnPessim = mkPCast16x8;
               opNarrow = Iop_NarrowUn16to8x8;
               break;
            default:
               tl_assert(0);
         }
         complainIfUndefined(mce, atom2, NULL);
         // Pessimised shift result
         IRAtom* shV
            = fnPessim(mce, vatom1);
         // Narrowed, pessimised shift result
         IRAtom* shVnarrowed
            = assignNew('V', mce, Ity_I64, unop(opNarrow, shV));
         // Generates: Def--(63)--Def PCast-to-I1(narrowed)
         IRAtom* qV = mkPCastXXtoXXlsb(mce, shVnarrowed, Ity_I64);
         // and assemble the result
         return assignNew('V', mce, Ity_V128, 
                          binop(Iop_64HLtoV128, qV, shVnarrowed));
      }

      case Iop_Mull32Sx2:
      case Iop_Mull32Ux2:
      case Iop_QDMull32Sx2:
         return vectorWidenI64(mce, Iop_Widen32Sto64x2,
                                    mkUifU64(mce, vatom1, vatom2));

      case Iop_Mull16Sx4:
      case Iop_Mull16Ux4:
      case Iop_QDMull16Sx4:
         return vectorWidenI64(mce, Iop_Widen16Sto32x4,
                                    mkUifU64(mce, vatom1, vatom2));

      case Iop_Mull8Sx8:
      case Iop_Mull8Ux8:
      case Iop_PolynomialMull8x8:
         return vectorWidenI64(mce, Iop_Widen8Sto16x8,
                                    mkUifU64(mce, vatom1, vatom2));

      case Iop_PwAdd32x4:
         return mkPCast32x4(mce,
               assignNew('V', mce, Ity_V128, binop(op, mkPCast32x4(mce, vatom1),
                     mkPCast32x4(mce, vatom2))));

      case Iop_PwAdd16x8:
         return mkPCast16x8(mce,
               assignNew('V', mce, Ity_V128, binop(op, mkPCast16x8(mce, vatom1),
                     mkPCast16x8(mce, vatom2))));

      case Iop_PwAdd8x16:
         return mkPCast8x16(mce,
               assignNew('V', mce, Ity_V128, binop(op, mkPCast8x16(mce, vatom1),
                     mkPCast8x16(mce, vatom2))));

      /* V128-bit data-steering */
      case Iop_SetV128lo32:
      case Iop_SetV128lo64:
      case Iop_64HLtoV128:
      case Iop_InterleaveLO64x2:
      case Iop_InterleaveLO32x4:
      case Iop_InterleaveLO16x8:
      case Iop_InterleaveLO8x16:
      case Iop_InterleaveHI64x2:
      case Iop_InterleaveHI32x4:
      case Iop_InterleaveHI16x8:
      case Iop_InterleaveHI8x16:
      case Iop_CatOddLanes8x16:
      case Iop_CatOddLanes16x8:
      case Iop_CatOddLanes32x4:
      case Iop_CatEvenLanes8x16:
      case Iop_CatEvenLanes16x8:
      case Iop_CatEvenLanes32x4:
      case Iop_InterleaveOddLanes8x16:
      case Iop_InterleaveOddLanes16x8:
      case Iop_InterleaveOddLanes32x4:
      case Iop_InterleaveEvenLanes8x16:
      case Iop_InterleaveEvenLanes16x8:
      case Iop_InterleaveEvenLanes32x4:
      case Iop_PackOddLanes8x16:
      case Iop_PackOddLanes16x8:
      case Iop_PackOddLanes32x4:
      case Iop_PackEvenLanes8x16:
      case Iop_PackEvenLanes16x8:
      case Iop_PackEvenLanes32x4:
         return assignNew('V', mce, Ity_V128, binop(op, vatom1, vatom2));

      case Iop_GetElem8x16:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I8, binop(op, vatom1, atom2));
      case Iop_GetElem16x8:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I16, binop(op, vatom1, atom2));
      case Iop_GetElem32x4:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I32, binop(op, vatom1, atom2));
      case Iop_GetElem64x2:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_I64, binop(op, vatom1, atom2));

      /* Perm8x16: rearrange values in left arg using steering values
         from right arg.  So rearrange the vbits in the same way but
         pessimise wrt steering values.  Perm32x4 ditto. */
      /* PermOrZero8x16: see comments above for PermOrZero8x8. */
      case Iop_Perm8x16:
      case Iop_PermOrZero8x16:
         return mkUifUV128(
                   mce,
                   assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2)),
                   mkPCast8x16(mce, vatom2)
                );
      case Iop_Perm32x4:
         return mkUifUV128(
                   mce,
                   assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2)),
                   mkPCast32x4(mce, vatom2)
                );

     /* These two take the lower half of each 16-bit lane, sign/zero
        extend it to 32, and multiply together, producing a 32x4
        result (and implicitly ignoring half the operand bits).  So
        treat it as a bunch of independent 16x8 operations, but then
        do 32-bit shifts left-right to copy the lower half results
        (which are all 0s or all 1s due to PCasting in binary16Ix8)
        into the upper half of each result lane. */
      case Iop_MullEven16Ux8:
      case Iop_MullEven16Sx8: {
         IRAtom* at;
         at = binary16Ix8(mce,vatom1,vatom2);
         at = assignNew('V', mce, Ity_V128, binop(Iop_ShlN32x4, at, mkU8(16)));
         at = assignNew('V', mce, Ity_V128, binop(Iop_SarN32x4, at, mkU8(16)));
	 return at;
      }

      /* Same deal as Iop_MullEven16{S,U}x8 */
      case Iop_MullEven8Ux16:
      case Iop_MullEven8Sx16: {
         IRAtom* at;
         at = binary8Ix16(mce,vatom1,vatom2);
         at = assignNew('V', mce, Ity_V128, binop(Iop_ShlN16x8, at, mkU8(8)));
         at = assignNew('V', mce, Ity_V128, binop(Iop_SarN16x8, at, mkU8(8)));
	 return at;
      }

      /* Same deal as Iop_MullEven16{S,U}x8 */
      case Iop_MullEven32Ux4:
      case Iop_MullEven32Sx4: {
         IRAtom* at;
         at = binary32Ix4(mce,vatom1,vatom2);
         at = assignNew('V', mce, Ity_V128, binop(Iop_ShlN64x2, at, mkU8(32)));
         at = assignNew('V', mce, Ity_V128, binop(Iop_SarN64x2, at, mkU8(32)));
         return at;
      }

      /* narrow 2xV128 into 1xV128, hi half from left arg, in a 2 x
         32x4 -> 16x8 laneage, discarding the upper half of each lane.
         Simply apply same op to the V bits, since this really no more
         than a data steering operation. */
      case Iop_NarrowBin32to16x8: 
      case Iop_NarrowBin16to8x16: 
      case Iop_NarrowBin64to32x4:
         return assignNew('V', mce, Ity_V128, 
                                    binop(op, vatom1, vatom2));

      case Iop_ShrV128:
      case Iop_SarV128:
      case Iop_ShlV128:
      case Iop_I128StoBCD128:
         /* Same scheme as with all other shifts.  Note: 10 Nov 05:
            this is wrong now, scalar shifts are done properly lazily.
            Vector shifts should be fixed too. */
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2));

      case Iop_BCDAdd:
      case Iop_BCDSub:
         return mkLazy2(mce, Ity_V128, vatom1, vatom2);

      /* SHA Iops */
      case Iop_SHA256:
      case Iop_SHA512:
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_V128, binop(op, vatom1, atom2));

      /* I128-bit data-steering */
      case Iop_64HLto128:
         return assignNew('V', mce, Ity_I128, binop(op, vatom1, vatom2));

      /* V256-bit SIMD */

      case Iop_Max64Fx4:
      case Iop_Min64Fx4:
         return binary64Fx4(mce, vatom1, vatom2);

      case Iop_Max32Fx8:
      case Iop_Min32Fx8:
         return binary32Fx8(mce, vatom1, vatom2);

      /* V256-bit data-steering */
      case Iop_V128HLtoV256:
         return assignNew('V', mce, Ity_V256, binop(op, vatom1, vatom2));

      /* Scalar floating point */

      case Iop_F32toI64S:
      case Iop_F32toI64U:
         /* I32(rm) x F32 -> I64 */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_I64StoF32:
         /* I32(rm) x I64 -> F32 */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_RoundF64toInt:
      case Iop_RoundF64toF32:
      case Iop_F64toI64S:
      case Iop_F64toI64U:
      case Iop_I64StoF64:
      case Iop_I64UtoF64:
      case Iop_SinF64:
      case Iop_CosF64:
      case Iop_TanF64:
      case Iop_2xm1F64:
      case Iop_SqrtF64:
      case Iop_RecpExpF64:
         /* I32(rm) x I64/F64 -> I64/F64 */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_ShlD64:
      case Iop_ShrD64:
      case Iop_RoundD64toInt:
         /* I32(rm) x D64 -> D64 */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_ShlD128:
      case Iop_ShrD128:
      case Iop_RoundD128toInt:
         /* I32(rm) x D128 -> D128 */
         return mkLazy2(mce, Ity_I128, vatom1, vatom2);

      case Iop_RoundF128toInt:
         /* I32(rm) x F128 -> F128 */
         return mkLazy2(mce, Ity_I128, vatom1, vatom2);

      case Iop_D64toI64S:
      case Iop_D64toI64U:
      case Iop_I64StoD64:
      case Iop_I64UtoD64:
         /* I32(rm) x I64/D64 -> D64/I64 */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_F32toD32:
      case Iop_F64toD32:
      case Iop_F128toD32:
      case Iop_D32toF32:
      case Iop_D64toF32:
      case Iop_D128toF32:
         /* I32(rm) x F32/F64/F128/D32/D64/D128 -> D32/F32 */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_F32toD64:
      case Iop_F64toD64:
      case Iop_F128toD64:
      case Iop_D32toF64:
      case Iop_D64toF64:
      case Iop_D128toF64:
         /* I32(rm) x F32/F64/F128/D32/D64/D128 -> D64/F64 */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_F32toD128:
      case Iop_F64toD128:
      case Iop_F128toD128:
      case Iop_D32toF128:
      case Iop_D64toF128:
      case Iop_D128toF128:
         /* I32(rm) x F32/F64/F128/D32/D64/D128 -> D128/F128 */
         return mkLazy2(mce, Ity_I128, vatom1, vatom2);

      case Iop_RoundF32toInt:
      case Iop_SqrtF32:
      case Iop_RecpExpF32:
         /* I32(rm) x I32/F32 -> I32/F32 */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_SqrtF128:
         /* I32(rm) x F128 -> F128 */
         return mkLazy2(mce, Ity_I128, vatom1, vatom2);

      case Iop_I32StoF32:
      case Iop_I32UtoF32:
      case Iop_F32toI32S:
      case Iop_F32toI32U:
         /* First arg is I32 (rounding mode), second is F32/I32 (data). */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_F64toF16:
      case Iop_F32toF16:
         /* First arg is I32 (rounding mode), second is F64/F32 (data). */
         return mkLazy2(mce, Ity_I16, vatom1, vatom2);

      case Iop_F128toI32S: /* IRRoundingMode(I32) x F128 -> signed I32  */
      case Iop_F128toI32U: /* IRRoundingMode(I32) x F128 -> unsigned I32  */
      case Iop_F128toF32:  /* IRRoundingMode(I32) x F128 -> F32         */
      case Iop_D128toI32S: /* IRRoundingMode(I32) x D128 -> signed I32  */
      case Iop_D128toI32U: /* IRRoundingMode(I32) x D128 -> unsigned I32  */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_F128toI128S:   /* IRRoundingMode(I32) x F128 -> signed I128 */
      case Iop_RndF128:       /* IRRoundingMode(I32) x F128 -> F128 */
         return mkLazy2(mce, Ity_I128, vatom1, vatom2);

      case Iop_F128toI64S: /* IRRoundingMode(I32) x F128 -> signed I64  */
      case Iop_F128toI64U: /* IRRoundingMode(I32) x F128 -> unsigned I64  */
      case Iop_F128toF64:  /* IRRoundingMode(I32) x F128 -> F64         */
      case Iop_D128toD64:  /* IRRoundingMode(I64) x D128 -> D64 */
      case Iop_D128toI64S: /* IRRoundingMode(I64) x D128 -> signed I64  */
      case Iop_D128toI64U: /* IRRoundingMode(I32) x D128 -> unsigned I64  */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_F64HLtoF128:
      case Iop_D64HLtoD128:
         return assignNew('V', mce, Ity_I128,
                          binop(Iop_64HLto128, vatom1, vatom2));

      case Iop_F64toI32U:
      case Iop_F64toI32S:
      case Iop_F64toF32:
      case Iop_I64UtoF32:
      case Iop_D64toI32U:
      case Iop_D64toI32S:
         /* First arg is I32 (rounding mode), second is F64/D64 (data). */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_D64toD32:
         /* First arg is I32 (rounding mode), second is D64 (data). */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_F64toI16S:
         /* First arg is I32 (rounding mode), second is F64 (data). */
         return mkLazy2(mce, Ity_I16, vatom1, vatom2);

      case Iop_InsertExpD64:
         /*  I64 x I64 -> D64 */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_InsertExpD128:
         /*  I64 x I128 -> D128 */
         return mkLazy2(mce, Ity_I128, vatom1, vatom2);

      case Iop_CmpF32:
      case Iop_CmpF64:
      case Iop_CmpF128:
      case Iop_CmpD64:
      case Iop_CmpD128:
      case Iop_CmpExpD64:
      case Iop_CmpExpD128:
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_MaxNumF32:
      case Iop_MinNumF32:
         /* F32 x F32 -> F32 */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_MaxNumF64:
      case Iop_MinNumF64:
         /* F64 x F64 -> F64 */
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      /* non-FP after here */

      case Iop_DivModU64to32:
      case Iop_DivModS64to32:
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_DivModU128to64:
      case Iop_DivModS128to64:
         return mkLazy2(mce, Ity_I128, vatom1, vatom2);

      case Iop_8HLto16:
         return assignNew('V', mce, Ity_I16, binop(op, vatom1, vatom2));
      case Iop_16HLto32:
         return assignNew('V', mce, Ity_I32, binop(op, vatom1, vatom2));
      case Iop_32HLto64:
         return assignNew('V', mce, Ity_I64, binop(op, vatom1, vatom2));

      case Iop_DivModU64to64:
      case Iop_DivModS64to64: {
         IRAtom* vTmp64 = mkLazy2(mce, Ity_I64, vatom1, vatom2);
         return assignNew('V', mce, Ity_I128,
                          binop(Iop_64HLto128, vTmp64, vTmp64));
      }

      case Iop_MullS64:
      case Iop_MullU64: {
         IRAtom* vLo64 = mkLeft64(mce, mkUifU64(mce, vatom1,vatom2));
         IRAtom* vHi64 = mkPCastTo(mce, Ity_I64, vLo64);
         return assignNew('V', mce, Ity_I128,
                          binop(Iop_64HLto128, vHi64, vLo64));
      }

      case Iop_DivModU32to32:
      case Iop_DivModS32to32: {
         IRAtom* vTmp32 = mkLazy2(mce, Ity_I32, vatom1, vatom2);
         return assignNew('V', mce, Ity_I64,
                          binop(Iop_32HLto64, vTmp32, vTmp32));
      }

      case Iop_MullS32:
      case Iop_MullU32: {
         IRAtom* vLo32 = mkLeft32(mce, mkUifU32(mce, vatom1,vatom2));
         IRAtom* vHi32 = mkPCastTo(mce, Ity_I32, vLo32);
         return assignNew('V', mce, Ity_I64,
                          binop(Iop_32HLto64, vHi32, vLo32));
      }

      case Iop_MullS16:
      case Iop_MullU16: {
         IRAtom* vLo16 = mkLeft16(mce, mkUifU16(mce, vatom1,vatom2));
         IRAtom* vHi16 = mkPCastTo(mce, Ity_I16, vLo16);
         return assignNew('V', mce, Ity_I32,
                          binop(Iop_16HLto32, vHi16, vLo16));
      }

      case Iop_MullS8:
      case Iop_MullU8: {
         IRAtom* vLo8 = mkLeft8(mce, mkUifU8(mce, vatom1,vatom2));
         IRAtom* vHi8 = mkPCastTo(mce, Ity_I8, vLo8);
         return assignNew('V', mce, Ity_I16, binop(Iop_8HLto16, vHi8, vLo8));
      }

      case Iop_Sad8Ux4: /* maybe we could do better?  ftm, do mkLazy2. */
      case Iop_DivS32:
      case Iop_DivU32:
      case Iop_DivU32E:
      case Iop_DivS32E:
      case Iop_QAdd32S: /* could probably do better */
      case Iop_QSub32S: /* could probably do better */
         return mkLazy2(mce, Ity_I32, vatom1, vatom2);

      case Iop_DivS64:
      case Iop_DivU64:
      case Iop_DivS64E:
      case Iop_DivU64E:
         return mkLazy2(mce, Ity_I64, vatom1, vatom2);

      case Iop_Add32:
         if (mce->dlbo.dl_Add32 == DLexpensive
             || (mce->dlbo.dl_Add32 == DLauto && hu == HuOth)) {
             return expensiveAddSub(mce,True,Ity_I32,
                                    vatom1,vatom2, atom1,atom2);
         } else {
             goto cheap_AddSub32;
         }
      case Iop_Sub32:
         if (mce->dlbo.dl_Sub32 == DLexpensive
             || (mce->dlbo.dl_Sub32 == DLauto && hu == HuOth)) {
             return expensiveAddSub(mce,False,Ity_I32,
                                    vatom1,vatom2, atom1,atom2);
         } else {
             goto cheap_AddSub32;
         }

      cheap_AddSub32:
      case Iop_Mul32:
         return mkLeft32(mce, mkUifU32(mce, vatom1,vatom2));

      case Iop_CmpORD32S:
      case Iop_CmpORD32U:
      case Iop_CmpORD64S:
      case Iop_CmpORD64U:
         return doCmpORD(mce, op, vatom1,vatom2, atom1,atom2);

      case Iop_Add64:
         if (mce->dlbo.dl_Add64 == DLexpensive
             || (mce->dlbo.dl_Add64 == DLauto && hu == HuOth)) {
             return expensiveAddSub(mce,True,Ity_I64,
                                    vatom1,vatom2, atom1,atom2);
         } else {
             goto cheap_AddSub64;
         }
      case Iop_Sub64:
         if (mce->dlbo.dl_Sub64 == DLexpensive
             || (mce->dlbo.dl_Sub64 == DLauto && hu == HuOth)) {
             return expensiveAddSub(mce,False,Ity_I64,
                                    vatom1,vatom2, atom1,atom2);
         } else {
             goto cheap_AddSub64;
         }

      cheap_AddSub64:
      case Iop_Mul64:
         return mkLeft64(mce, mkUifU64(mce, vatom1,vatom2));

      case Iop_Mul16:
      case Iop_Add16:
      case Iop_Sub16:
         return mkLeft16(mce, mkUifU16(mce, vatom1,vatom2));

      case Iop_Mul8:
      case Iop_Sub8:
      case Iop_Add8:
         return mkLeft8(mce, mkUifU8(mce, vatom1,vatom2));

      ////---- CmpXX64
      case Iop_CmpEQ64: case Iop_CmpNE64:
         if (mce->dlbo.dl_CmpEQ64_CmpNE64 == DLexpensive)
            goto expensive_cmp64;
         else
            goto cheap_cmp64;

      expensive_cmp64:
      case Iop_ExpCmpNE64:
         return expensiveCmpEQorNE(mce,Ity_I64, vatom1,vatom2, atom1,atom2 );

      cheap_cmp64:
      case Iop_CmpLE64S: case Iop_CmpLE64U: 
      case Iop_CmpLT64U: case Iop_CmpLT64S:
         return mkPCastTo(mce, Ity_I1, mkUifU64(mce, vatom1,vatom2));

      ////---- CmpXX32
      case Iop_CmpEQ32: case Iop_CmpNE32:
         if (mce->dlbo.dl_CmpEQ32_CmpNE32 == DLexpensive)
            goto expensive_cmp32;
         else
            goto cheap_cmp32;

      expensive_cmp32:
      case Iop_ExpCmpNE32:
         return expensiveCmpEQorNE(mce,Ity_I32, vatom1,vatom2, atom1,atom2 );

      cheap_cmp32:
      case Iop_CmpLE32S: case Iop_CmpLE32U: 
      case Iop_CmpLT32U: case Iop_CmpLT32S:
         return mkPCastTo(mce, Ity_I1, mkUifU32(mce, vatom1,vatom2));

      ////---- CmpXX16
      case Iop_CmpEQ16: case Iop_CmpNE16:
         if (mce->dlbo.dl_CmpEQ16_CmpNE16 == DLexpensive)
            goto expensive_cmp16;
         else
            goto cheap_cmp16;

      expensive_cmp16:
      case Iop_ExpCmpNE16:
         return expensiveCmpEQorNE(mce,Ity_I16, vatom1,vatom2, atom1,atom2 );

      cheap_cmp16:
         return mkPCastTo(mce, Ity_I1, mkUifU16(mce, vatom1,vatom2));

      ////---- CmpXX8
      case Iop_CmpEQ8: case Iop_CmpNE8:
         if (mce->dlbo.dl_CmpEQ8_CmpNE8 == DLexpensive)
            goto expensive_cmp8;
         else
            goto cheap_cmp8;

      expensive_cmp8:
         return expensiveCmpEQorNE(mce,Ity_I8, vatom1,vatom2, atom1,atom2 );

      cheap_cmp8:
         return mkPCastTo(mce, Ity_I1, mkUifU8(mce, vatom1,vatom2));

      ////---- end CmpXX{64,32,16,8}

      case Iop_CasCmpEQ8:  case Iop_CasCmpNE8:
      case Iop_CasCmpEQ16: case Iop_CasCmpNE16:
      case Iop_CasCmpEQ32: case Iop_CasCmpNE32:
      case Iop_CasCmpEQ64: case Iop_CasCmpNE64:
         /* Just say these all produce a defined result, regardless
            of their arguments.  See COMMENT_ON_CasCmpEQ in this file. */
         return assignNew('V', mce, Ity_I1, definedOfType(Ity_I1));

      case Iop_Shl64: case Iop_Shr64: case Iop_Sar64:
         return scalarShift( mce, Ity_I64, op, vatom1,vatom2, atom1,atom2 );

      case Iop_Shl32: case Iop_Shr32: case Iop_Sar32:
         return scalarShift( mce, Ity_I32, op, vatom1,vatom2, atom1,atom2 );

      case Iop_Shl16: case Iop_Shr16: case Iop_Sar16:
         return scalarShift( mce, Ity_I16, op, vatom1,vatom2, atom1,atom2 );

      case Iop_Shl8: case Iop_Shr8: case Iop_Sar8:
         return scalarShift( mce, Ity_I8, op, vatom1,vatom2, atom1,atom2 );

      case Iop_AndV256:
         uifu = mkUifUV256; difd = mkDifDV256; 
         and_or_ty = Ity_V256; improve = mkImproveANDV256; goto do_And_Or;
      case Iop_AndV128:
         uifu = mkUifUV128; difd = mkDifDV128; 
         and_or_ty = Ity_V128; improve = mkImproveANDV128; goto do_And_Or;
      case Iop_And64:
         uifu = mkUifU64; difd = mkDifD64; 
         and_or_ty = Ity_I64; improve = mkImproveAND64; goto do_And_Or;
      case Iop_And32:
         uifu = mkUifU32; difd = mkDifD32; 
         and_or_ty = Ity_I32; improve = mkImproveAND32; goto do_And_Or;
      case Iop_And16:
         uifu = mkUifU16; difd = mkDifD16; 
         and_or_ty = Ity_I16; improve = mkImproveAND16; goto do_And_Or;
      case Iop_And8:
         uifu = mkUifU8; difd = mkDifD8; 
         and_or_ty = Ity_I8; improve = mkImproveAND8; goto do_And_Or;
      case Iop_And1:
         uifu = mkUifU1; difd = mkDifD1; 
         and_or_ty = Ity_I1; improve = mkImproveAND1; goto do_And_Or;

      case Iop_OrV256:
         uifu = mkUifUV256; difd = mkDifDV256; 
         and_or_ty = Ity_V256; improve = mkImproveORV256; goto do_And_Or;
      case Iop_OrV128:
         uifu = mkUifUV128; difd = mkDifDV128; 
         and_or_ty = Ity_V128; improve = mkImproveORV128; goto do_And_Or;
      case Iop_Or64:
         uifu = mkUifU64; difd = mkDifD64; 
         and_or_ty = Ity_I64; improve = mkImproveOR64; goto do_And_Or;
      case Iop_Or32:
         uifu = mkUifU32; difd = mkDifD32; 
         and_or_ty = Ity_I32; improve = mkImproveOR32; goto do_And_Or;
      case Iop_Or16:
         uifu = mkUifU16; difd = mkDifD16; 
         and_or_ty = Ity_I16; improve = mkImproveOR16; goto do_And_Or;
      case Iop_Or8:
         uifu = mkUifU8; difd = mkDifD8; 
         and_or_ty = Ity_I8; improve = mkImproveOR8; goto do_And_Or;
      case Iop_Or1:
         uifu = mkUifU1; difd = mkDifD1; 
         and_or_ty = Ity_I1; improve = mkImproveOR1; goto do_And_Or;

      do_And_Or:
         return
         assignNew(
            'V', mce, 
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
      case Iop_Xor64:
         return mkUifU64(mce, vatom1, vatom2);
      case Iop_XorV128:
         return mkUifUV128(mce, vatom1, vatom2);
      case Iop_XorV256:
         return mkUifUV256(mce, vatom1, vatom2);

      /* V256-bit SIMD */

      case Iop_ShrN16x16:
      case Iop_ShrN32x8:
      case Iop_ShrN64x4:
      case Iop_SarN16x16:
      case Iop_SarN32x8:
      case Iop_ShlN16x16:
      case Iop_ShlN32x8:
      case Iop_ShlN64x4:
         /* Same scheme as with all other shifts.  Note: 22 Oct 05:
            this is wrong now, scalar shifts are done properly lazily.
            Vector shifts should be fixed too. */
         complainIfUndefined(mce, atom2, NULL);
         return assignNew('V', mce, Ity_V256, binop(op, vatom1, atom2));

      case Iop_QSub8Ux32:
      case Iop_QSub8Sx32:
      case Iop_Sub8x32:
      case Iop_Min8Ux32:
      case Iop_Min8Sx32:
      case Iop_Max8Ux32:
      case Iop_Max8Sx32:
      case Iop_CmpGT8Sx32:
      case Iop_CmpEQ8x32:
      case Iop_Avg8Ux32:
      case Iop_QAdd8Ux32:
      case Iop_QAdd8Sx32:
      case Iop_Add8x32:
         return binary8Ix32(mce, vatom1, vatom2);

      case Iop_QSub16Ux16:
      case Iop_QSub16Sx16:
      case Iop_Sub16x16:
      case Iop_Mul16x16:
      case Iop_MulHi16Sx16:
      case Iop_MulHi16Ux16:
      case Iop_Min16Sx16:
      case Iop_Min16Ux16:
      case Iop_Max16Sx16:
      case Iop_Max16Ux16:
      case Iop_CmpGT16Sx16:
      case Iop_CmpEQ16x16:
      case Iop_Avg16Ux16:
      case Iop_QAdd16Ux16:
      case Iop_QAdd16Sx16:
      case Iop_Add16x16:
         return binary16Ix16(mce, vatom1, vatom2);

      case Iop_Sub32x8:
      case Iop_CmpGT32Sx8:
      case Iop_CmpEQ32x8:
      case Iop_Add32x8:
      case Iop_Max32Ux8:
      case Iop_Max32Sx8:
      case Iop_Min32Ux8:
      case Iop_Min32Sx8:
      case Iop_Mul32x8:
         return binary32Ix8(mce, vatom1, vatom2);

      case Iop_Sub64x4:
      case Iop_Add64x4:
      case Iop_CmpEQ64x4:
      case Iop_CmpGT64Sx4:
         return binary64Ix4(mce, vatom1, vatom2);

      case Iop_I32StoF32x8:
      case Iop_F32toI32Sx8:
         return unary32Fx8_w_rm(mce, vatom1, vatom2);

      /* Perm32x8: rearrange values in left arg using steering values
         from right arg.  So rearrange the vbits in the same way but
         pessimise wrt steering values. */
      case Iop_Perm32x8:
         return mkUifUV256(
                   mce,
                   assignNew('V', mce, Ity_V256, binop(op, vatom1, atom2)),
                   mkPCast32x8(mce, vatom2)
                );

      /* Q-and-Qshift-by-vector of the form (V128, V128) -> V256.
         Handle the shifted results in the same way that other
         binary Q ops are handled, eg QSub: UifU the two args,
         then pessimise -- which is binaryNIxM.  But for the upper
         V128, we require to generate just 1 bit which is the
         pessimised shift result, with 127 defined zeroes above it.

         Note that this overly pessimistic in that in fact only the
         bottom 8 bits of each lane of the second arg determine the shift
         amount.  Really we ought to ignore any undefinedness in the
         rest of the lanes of the second arg. */
      case Iop_QandSQsh64x2:  case Iop_QandUQsh64x2:
      case Iop_QandSQRsh64x2: case Iop_QandUQRsh64x2:
      case Iop_QandSQsh32x4:  case Iop_QandUQsh32x4:
      case Iop_QandSQRsh32x4: case Iop_QandUQRsh32x4:
      case Iop_QandSQsh16x8:  case Iop_QandUQsh16x8:
      case Iop_QandSQRsh16x8: case Iop_QandUQRsh16x8:
      case Iop_QandSQsh8x16:  case Iop_QandUQsh8x16:
      case Iop_QandSQRsh8x16: case Iop_QandUQRsh8x16:
      {
         // The function to generate the pessimised shift result
         IRAtom* (*binaryNIxM)(MCEnv*,IRAtom*,IRAtom*) = NULL;
         switch (op) {
            case Iop_QandSQsh64x2:
            case Iop_QandUQsh64x2:
            case Iop_QandSQRsh64x2:
            case Iop_QandUQRsh64x2:
               binaryNIxM = binary64Ix2;
               break;
            case Iop_QandSQsh32x4:
            case Iop_QandUQsh32x4:
            case Iop_QandSQRsh32x4:
            case Iop_QandUQRsh32x4:
               binaryNIxM = binary32Ix4;
               break;
            case Iop_QandSQsh16x8:
            case Iop_QandUQsh16x8:
            case Iop_QandSQRsh16x8:
            case Iop_QandUQRsh16x8:
               binaryNIxM = binary16Ix8;
               break;
            case Iop_QandSQsh8x16:
            case Iop_QandUQsh8x16:
            case Iop_QandSQRsh8x16:
            case Iop_QandUQRsh8x16:
               binaryNIxM = binary8Ix16;
               break;
            default:
               tl_assert(0);
         }
         tl_assert(binaryNIxM);
         // Pessimised shift result, shV[127:0]
         IRAtom* shV = binaryNIxM(mce, vatom1, vatom2);
         // Generates: Def--(127)--Def PCast-to-I1(shV)
         IRAtom* qV = mkPCastXXtoXXlsb(mce, shV, Ity_V128);
         // and assemble the result
         return assignNew('V', mce, Ity_V256,
                          binop(Iop_V128HLtoV256, qV, shV));
      }

      case Iop_F32toF16x4: {
         // First, PCast the input vector, retaining the 32x4 format.
         IRAtom* pcasted = mkPCast32x4(mce, vatom2); // :: 32x4
         // Now truncate each 32 bit lane to 16 bits.  Since we already PCasted
         // the input, we're not going to lose any information.
         IRAtom* pcHI64
            = assignNew('V', mce, Ity_I64, unop(Iop_V128HIto64, pcasted));//32x2
         IRAtom* pcLO64
            = assignNew('V', mce, Ity_I64, unop(Iop_V128to64, pcasted)); // 32x2
         IRAtom* narrowed
            = assignNew('V', mce, Ity_I64, binop(Iop_NarrowBin32to16x4,
                                                 pcHI64, pcLO64)); // 16x4
         // Finally, roll in any badness from the rounding mode.
         IRAtom* rmPCasted = mkPCastTo(mce, Ity_I64, vatom1);
         return mkUifU64(mce, narrowed, rmPCasted);
      }

      case Iop_F32toF16x8: {
         // Same scheme as for Iop_F32toF16x4.
         IRAtom* pcasted = mkPCast32x8(mce, vatom2); // :: 32x8
         IRAtom* pcHI128
            = assignNew('V', mce, Ity_V128, unop(Iop_V256toV128_1,
                                                 pcasted)); // 32x4
         IRAtom* pcLO128
            = assignNew('V', mce, Ity_V128, unop(Iop_V256toV128_0,
                                                 pcasted)); // 32x4
         IRAtom* narrowed
            = assignNew('V', mce, Ity_V128, binop(Iop_NarrowBin32to16x8,
                                                  pcHI128, pcLO128)); // 16x8
         // Finally, roll in any badness from the rounding mode.
         IRAtom* rmPCasted = mkPCastTo(mce, Ity_V128, vatom1);
         return mkUifUV128(mce, narrowed, rmPCasted);
      }

      default:
         ppIROp(op);
         VG_(tool_panic)("memcheck:expr2vbits_Binop");
   }
}


static 
IRExpr* expr2vbits_Unop ( MCEnv* mce, IROp op, IRAtom* atom )
{
   /* For the widening operations {8,16,32}{U,S}to{16,32,64}, the
      selection of shadow operation implicitly duplicates the logic in
      do_shadow_LoadG and should be kept in sync (in the very unlikely
      event that the interpretation of such widening ops changes in
      future).  See comment in do_shadow_LoadG. */
   IRAtom* vatom = expr2vbits( mce, atom, HuOth );
   tl_assert(isOriginalAtom(mce,atom));
   switch (op) {

      case Iop_Abs64Fx2:
      case Iop_Neg64Fx2:
      case Iop_RSqrtEst64Fx2:
      case Iop_RecipEst64Fx2:
      case Iop_Log2_64Fx2:
         return unary64Fx2(mce, vatom);

      case Iop_Sqrt64F0x2:
         return unary64F0x2(mce, vatom);

      case Iop_Sqrt32Fx8:
      case Iop_RSqrtEst32Fx8:
      case Iop_RecipEst32Fx8:
         return unary32Fx8(mce, vatom);

      case Iop_Sqrt64Fx4:
         return unary64Fx4(mce, vatom);

      case Iop_RecipEst32Fx4:
      case Iop_I32UtoF32x4_DEP:
      case Iop_I32StoF32x4_DEP:
      case Iop_QF32toI32Ux4_RZ:
      case Iop_QF32toI32Sx4_RZ:
      case Iop_RoundF32x4_RM:
      case Iop_RoundF32x4_RP:
      case Iop_RoundF32x4_RN:
      case Iop_RoundF32x4_RZ:
      case Iop_RecipEst32Ux4:
      case Iop_Abs32Fx4:
      case Iop_Neg32Fx4:
      case Iop_RSqrtEst32Fx4:
      case Iop_Log2_32Fx4:
      case Iop_Exp2_32Fx4:
         return unary32Fx4(mce, vatom);

      case Iop_I32UtoF32x2_DEP:
      case Iop_I32StoF32x2_DEP:
      case Iop_RecipEst32Fx2:
      case Iop_RecipEst32Ux2:
      case Iop_Abs32Fx2:
      case Iop_Neg32Fx2:
      case Iop_RSqrtEst32Fx2:
         return unary32Fx2(mce, vatom);

      case Iop_Sqrt32F0x4:
      case Iop_RSqrtEst32F0x4:
      case Iop_RecipEst32F0x4:
         return unary32F0x4(mce, vatom);

      // These are self-shadowing.
      case Iop_32UtoV128:
      case Iop_64UtoV128:
      case Iop_Dup8x16:
      case Iop_Dup16x8:
      case Iop_Dup32x4:
      case Iop_Reverse1sIn8_x16:
      case Iop_Reverse8sIn16_x8:
      case Iop_Reverse8sIn32_x4:
      case Iop_Reverse16sIn32_x4:
      case Iop_Reverse8sIn64_x2:
      case Iop_Reverse16sIn64_x2:
      case Iop_Reverse32sIn64_x2:
      case Iop_V256toV128_1: case Iop_V256toV128_0:
      case Iop_ZeroHI64ofV128:
      case Iop_ZeroHI96ofV128:
      case Iop_ZeroHI112ofV128:
      case Iop_ZeroHI120ofV128:
         return assignNew('V', mce, Ity_V128, unop(op, vatom));

      case Iop_F128HItoF64:  /* F128 -> high half of F128 */
      case Iop_D128HItoD64:  /* D128 -> high half of D128 */
         return assignNew('V', mce, Ity_I64, unop(Iop_128HIto64, vatom));
      case Iop_F128LOtoF64:  /* F128 -> low  half of F128 */
      case Iop_D128LOtoD64:  /* D128 -> low  half of D128 */
         return assignNew('V', mce, Ity_I64, unop(Iop_128to64, vatom));

      case Iop_NegF128:
      case Iop_AbsF128:
      case Iop_RndF128:
      case Iop_TruncF128toI64S: /* F128 -> I64S */
      case Iop_TruncF128toI32S: /* F128 -> I32S (result stored in 64-bits) */
      case Iop_TruncF128toI64U: /* F128 -> I64U */
      case Iop_TruncF128toI32U: /* F128 -> I32U (result stored in 64-bits) */
         return mkPCastTo(mce, Ity_I128, vatom);

      case Iop_BCD128toI128S:
      case Iop_MulI128by10:
      case Iop_MulI128by10Carry:
      case Iop_F16toF64x2:
      case Iop_F64toF16x2_DEP:
         // FIXME JRS 2018-Nov-15.  This is surely not correct!
         return vatom;

      case Iop_I32StoF128: /* signed I32 -> F128 */
      case Iop_I64StoF128: /* signed I64 -> F128 */
      case Iop_I32UtoF128: /* unsigned I32 -> F128 */
      case Iop_I64UtoF128: /* unsigned I64 -> F128 */
      case Iop_F32toF128:  /* F32 -> F128 */
      case Iop_F64toF128:  /* F64 -> F128 */
      case Iop_I32StoD128: /* signed I64 -> D128 */
      case Iop_I64StoD128: /* signed I64 -> D128 */
      case Iop_I32UtoD128: /* unsigned I32 -> D128 */
      case Iop_I64UtoD128: /* unsigned I64 -> D128 */
         return mkPCastTo(mce, Ity_I128, vatom);

      case Iop_F16toF64:
      case Iop_F32toF64: 
      case Iop_I32StoF64:
      case Iop_I32UtoF64:
      case Iop_NegF64:
      case Iop_AbsF64:
      case Iop_RSqrtEst5GoodF64:
      case Iop_RoundF64toF64_NEAREST:
      case Iop_RoundF64toF64_NegINF:
      case Iop_RoundF64toF64_PosINF:
      case Iop_RoundF64toF64_ZERO:
      case Iop_D32toD64:
      case Iop_I32StoD64:
      case Iop_I32UtoD64:
      case Iop_ExtractExpD64:    /* D64  -> I64 */
      case Iop_ExtractExpD128:   /* D128 -> I64 */
      case Iop_ExtractSigD64:    /* D64  -> I64 */
      case Iop_ExtractSigD128:   /* D128 -> I64 */
      case Iop_DPBtoBCD:
      case Iop_BCDtoDPB:
         return mkPCastTo(mce, Ity_I64, vatom);

      case Iop_D64toD128:
         return mkPCastTo(mce, Ity_I128, vatom);

      case Iop_TruncF64asF32:
      case Iop_NegF32:
      case Iop_AbsF32:
      case Iop_F16toF32: 
         return mkPCastTo(mce, Ity_I32, vatom);

      case Iop_Ctz32: case Iop_CtzNat32:
      case Iop_Ctz64: case Iop_CtzNat64:
         return expensiveCountTrailingZeroes(mce, op, atom, vatom);

      case Iop_Clz32: case Iop_ClzNat32:
      case Iop_Clz64: case Iop_ClzNat64:
         return expensiveCountLeadingZeroes(mce, op, atom, vatom);

      // PopCount32: this is slightly pessimistic.  It is true that the
      // result depends on all input bits, so that aspect of the PCast is
      // correct.  However, regardless of the input, only the lowest 5 bits
      // out of the output can ever be undefined.  So we could actually
      // "improve" the results here by marking the top 27 bits of output as
      // defined.  A similar comment applies for PopCount64.
      case Iop_PopCount32:
         return mkPCastTo(mce, Ity_I32, vatom);
      case Iop_PopCount64:
         return mkPCastTo(mce, Ity_I64, vatom);

      // These are self-shadowing.
      case Iop_1Uto64:
      case Iop_1Sto64:
      case Iop_8Uto64:
      case Iop_8Sto64:
      case Iop_16Uto64:
      case Iop_16Sto64:
      case Iop_32Sto64:
      case Iop_32Uto64:
      case Iop_V128to64:
      case Iop_V128HIto64:
      case Iop_128HIto64:
      case Iop_128to64:
      case Iop_Dup8x8:
      case Iop_Dup16x4:
      case Iop_Dup32x2:
      case Iop_Reverse8sIn16_x4:
      case Iop_Reverse8sIn32_x2:
      case Iop_Reverse16sIn32_x2:
      case Iop_Reverse8sIn64_x1:
      case Iop_Reverse16sIn64_x1:
      case Iop_Reverse32sIn64_x1:
      case Iop_V256to64_0: case Iop_V256to64_1:
      case Iop_V256to64_2: case Iop_V256to64_3:
         return assignNew('V', mce, Ity_I64, unop(op, vatom));

      // These are self-shadowing.
      case Iop_64to32:
      case Iop_64HIto32:
      case Iop_1Uto32:
      case Iop_1Sto32:
      case Iop_8Uto32:
      case Iop_16Uto32:
      case Iop_16Sto32:
      case Iop_8Sto32:
      case Iop_V128to32:
      case Iop_Reverse8sIn32_x1:
         return assignNew('V', mce, Ity_I32, unop(op, vatom));

      // These are self-shadowing.
      case Iop_8Sto16:
      case Iop_8Uto16:
      case Iop_32to16:
      case Iop_32HIto16:
      case Iop_64to16:
      case Iop_GetMSBs8x16:
         return assignNew('V', mce, Ity_I16, unop(op, vatom));

      // These are self-shadowing.
      case Iop_1Uto8:
      case Iop_1Sto8:
      case Iop_16to8:
      case Iop_16HIto8:
      case Iop_32to8:
      case Iop_64to8:
      case Iop_GetMSBs8x8:
         return assignNew('V', mce, Ity_I8, unop(op, vatom));

      case Iop_32to1:
         return assignNew('V', mce, Ity_I1, unop(Iop_32to1, vatom));

      case Iop_64to1:
         return assignNew('V', mce, Ity_I1, unop(Iop_64to1, vatom));

      case Iop_ReinterpF64asI64:
      case Iop_ReinterpI64asF64:
      case Iop_ReinterpI32asF32:
      case Iop_ReinterpF32asI32:
      case Iop_ReinterpI64asD64:
      case Iop_ReinterpD64asI64:
      case Iop_NotV256:
      case Iop_NotV128:
      case Iop_Not64:
      case Iop_Not32:
      case Iop_Not16:
      case Iop_Not8:
      case Iop_Not1:
         // FIXME JRS 2018-Nov-15.  This is surely not correct!
         return vatom;

      case Iop_CmpNEZ8x8:
      case Iop_Cnt8x8:
      case Iop_Clz8x8:
      case Iop_Cls8x8:
      case Iop_Abs8x8:
         return mkPCast8x8(mce, vatom);

      case Iop_CmpNEZ8x16:
      case Iop_Cnt8x16:
      case Iop_Clz8x16:
      case Iop_Cls8x16:
      case Iop_Abs8x16:
      case Iop_Ctz8x16:
         return mkPCast8x16(mce, vatom);

      case Iop_CmpNEZ16x4:
      case Iop_Clz16x4:
      case Iop_Cls16x4:
      case Iop_Abs16x4:
         return mkPCast16x4(mce, vatom);

      case Iop_CmpNEZ16x8:
      case Iop_Clz16x8:
      case Iop_Cls16x8:
      case Iop_Abs16x8:
      case Iop_Ctz16x8:
         return mkPCast16x8(mce, vatom);

      case Iop_CmpNEZ32x2:
      case Iop_Clz32x2:
      case Iop_Cls32x2:
      case Iop_F32toI32Ux2_RZ:
      case Iop_F32toI32Sx2_RZ:
      case Iop_Abs32x2:
         return mkPCast32x2(mce, vatom);

      case Iop_CmpNEZ32x4:
      case Iop_Clz32x4:
      case Iop_Cls32x4:
      case Iop_F32toI32Ux4_RZ:
      case Iop_F32toI32Sx4_RZ:
      case Iop_Abs32x4:
      case Iop_RSqrtEst32Ux4:
      case Iop_Ctz32x4:
         return mkPCast32x4(mce, vatom);

      case Iop_CmpwNEZ32:
         return mkPCastTo(mce, Ity_I32, vatom);

      case Iop_CmpwNEZ64:
         return mkPCastTo(mce, Ity_I64, vatom);

      case Iop_CmpNEZ64x2:
      case Iop_CipherSV128:
      case Iop_Clz64x2:
      case Iop_Abs64x2:
      case Iop_Ctz64x2:
         return mkPCast64x2(mce, vatom);

      // This is self-shadowing.
      case Iop_PwBitMtxXpose64x2:
         return assignNew('V', mce, Ity_V128, unop(op, vatom));

      case Iop_NarrowUn16to8x8:
      case Iop_NarrowUn32to16x4:
      case Iop_NarrowUn64to32x2:
      case Iop_QNarrowUn16Sto8Sx8:
      case Iop_QNarrowUn16Sto8Ux8:
      case Iop_QNarrowUn16Uto8Ux8:
      case Iop_QNarrowUn32Sto16Sx4:
      case Iop_QNarrowUn32Sto16Ux4:
      case Iop_QNarrowUn32Uto16Ux4:
      case Iop_QNarrowUn64Sto32Sx2:
      case Iop_QNarrowUn64Sto32Ux2:
      case Iop_QNarrowUn64Uto32Ux2:
         return vectorNarrowUnV128(mce, op, vatom);

      // JRS FIXME 2019 Mar 17: per comments on F16toF32x4, this is probably not
      // right.
      case Iop_F32toF16x4_DEP:
         return vectorNarrowUnV128(mce, op, vatom);

      case Iop_Widen8Sto16x8:
      case Iop_Widen8Uto16x8:
      case Iop_Widen16Sto32x4:
      case Iop_Widen16Uto32x4:
      case Iop_Widen32Sto64x2:
      case Iop_Widen32Uto64x2:
         return vectorWidenI64(mce, op, vatom);

      case Iop_F16toF32x4:
         // JRS 2019 Mar 17: this definitely isn't right, but it probably works
         // OK by accident if -- as seems likely -- the F16 to F32 conversion
         // preserves will generate an output 32 bits with at least one 1 bit
         // set if there's one or more 1 bits set in the input 16 bits.  More
         // correct code for this is just below, but commented out, so as to
         // avoid short-term backend failures on targets that can't do
         // Iop_Interleave{LO,HI}16x4.
         return vectorWidenI64(mce, op, vatom);

      case Iop_F16toF32x8: {
         // PCast the input at 16x8.  This makes each lane hold either all
         // zeroes or all ones.
         IRAtom* pcasted = mkPCast16x8(mce, vatom); // :: I16x8
         // Now double the width of each lane to 32 bits.  Because the lanes are
         // all zeroes or all ones, we can just copy the each lane twice into
         // the result.  Here's the low half:
         IRAtom* widenedLO // :: I32x4
            = assignNew('V', mce, Ity_V128, binop(Iop_InterleaveLO16x8,
                                                  pcasted, pcasted));
         // And the high half:
         IRAtom* widenedHI // :: I32x4
            = assignNew('V', mce, Ity_V128, binop(Iop_InterleaveHI16x8,
                                                  pcasted, pcasted));
         // Glue them back together:
         return assignNew('V', mce, Ity_V256, binop(Iop_V128HLtoV256,
                                                    widenedHI, widenedLO));
      }

      // See comment just above, for Iop_F16toF32x4
      //case Iop_F16toF32x4: {
      //   // Same scheme as F16toF32x4
      //   IRAtom* pcasted = mkPCast16x4(mce, vatom); // :: I16x4
      //   IRAtom* widenedLO // :: I32x2
      //      = assignNew('V', mce, Ity_I64, binop(Iop_InterleaveLO16x4,
      //                                            pcasted, pcasted));
      //   IRAtom* widenedHI // :: I32x4
      //      = assignNew('V', mce, Ity_I64, binop(Iop_InterleaveHI16x4,
      //                                            pcasted, pcasted));
      //   // Glue them back together:
      //   return assignNew('V', mce, Ity_V128, binop(Iop_64HLtoV128,
      //                                              widenedHI, widenedLO));
      //}

      case Iop_PwAddL32Ux2:
      case Iop_PwAddL32Sx2:
         return mkPCastTo(mce, Ity_I64,
               assignNew('V', mce, Ity_I64, unop(op, mkPCast32x2(mce, vatom))));

      case Iop_PwAddL16Ux4:
      case Iop_PwAddL16Sx4:
         return mkPCast32x2(mce,
               assignNew('V', mce, Ity_I64, unop(op, mkPCast16x4(mce, vatom))));

      case Iop_PwAddL8Ux8:
      case Iop_PwAddL8Sx8:
         return mkPCast16x4(mce,
               assignNew('V', mce, Ity_I64, unop(op, mkPCast8x8(mce, vatom))));

      case Iop_PwAddL32Ux4:
      case Iop_PwAddL32Sx4:
         return mkPCast64x2(mce,
               assignNew('V', mce, Ity_V128, unop(op, mkPCast32x4(mce, vatom))));

      case Iop_PwAddL64Ux2:
         return mkPCast128x1(mce,
               assignNew('V', mce, Ity_V128, unop(op, mkPCast64x2(mce, vatom))));

      case Iop_PwAddL16Ux8:
      case Iop_PwAddL16Sx8:
         return mkPCast32x4(mce,
               assignNew('V', mce, Ity_V128, unop(op, mkPCast16x8(mce, vatom))));

      case Iop_PwAddL8Ux16:
      case Iop_PwAddL8Sx16:
         return mkPCast16x8(mce,
               assignNew('V', mce, Ity_V128, unop(op, mkPCast8x16(mce, vatom))));

      case Iop_I64UtoF32:
      default:
         ppIROp(op);
         VG_(tool_panic)("memcheck:expr2vbits_Unop");
   }
}


/* Worker function -- do not call directly.  See comments on
   expr2vbits_Load for the meaning of |guard|.

   Generates IR to (1) perform a definedness test of |addr|, (2)
   perform a validity test of |addr|, and (3) return the Vbits for the
   location indicated by |addr|.  All of this only happens when
   |guard| is NULL or |guard| evaluates to True at run time.

   If |guard| evaluates to False at run time, the returned value is
   the IR-mandated 0x55..55 value, and no checks nor shadow loads are
   performed.

   The definedness of |guard| itself is not checked.  That is assumed
   to have been done before this point, by the caller. */
static
IRAtom* expr2vbits_Load_WRK ( MCEnv* mce,
                              IREndness end, IRType ty,
                              IRAtom* addr, UInt bias, IRAtom* guard )
{
   tl_assert(isOriginalAtom(mce,addr));
   tl_assert(end == Iend_LE || end == Iend_BE);

   /* First, emit a definedness test for the address.  This also sets
      the address (shadow) to 'defined' following the test. */
   complainIfUndefined( mce, addr, guard );

   /* Now cook up a call to the relevant helper function, to read the
      data V bits from shadow memory. */
   ty = shadowTypeV(ty);

   void*        helper           = NULL;
   const HChar* hname            = NULL;
   Bool         ret_via_outparam = False;

   if (end == Iend_LE) {
      switch (ty) {
         case Ity_V256: helper = &MC_(helperc_LOADV256le);
                        hname = "MC_(helperc_LOADV256le)";
                        ret_via_outparam = True;
                        break;
         case Ity_V128: helper = &MC_(helperc_LOADV128le);
                        hname = "MC_(helperc_LOADV128le)";
                        ret_via_outparam = True;
                        break;
         case Ity_I64:  helper = &MC_(helperc_LOADV64le);
                        hname = "MC_(helperc_LOADV64le)";
                        break;
         case Ity_I32:  helper = &MC_(helperc_LOADV32le);
                        hname = "MC_(helperc_LOADV32le)";
                        break;
         case Ity_I16:  helper = &MC_(helperc_LOADV16le);
                        hname = "MC_(helperc_LOADV16le)";
                        break;
         case Ity_I8:   helper = &MC_(helperc_LOADV8);
                        hname = "MC_(helperc_LOADV8)";
                        break;
         default:       ppIRType(ty);
                        VG_(tool_panic)("memcheck:expr2vbits_Load_WRK(LE)");
      }
   } else {
      switch (ty) {
         case Ity_V256: helper = &MC_(helperc_LOADV256be);
                        hname = "MC_(helperc_LOADV256be)";
                        ret_via_outparam = True;
                        break;
         case Ity_V128: helper = &MC_(helperc_LOADV128be);
                        hname = "MC_(helperc_LOADV128be)";
                        ret_via_outparam = True;
                        break;
         case Ity_I64:  helper = &MC_(helperc_LOADV64be);
                        hname = "MC_(helperc_LOADV64be)";
                        break;
         case Ity_I32:  helper = &MC_(helperc_LOADV32be);
                        hname = "MC_(helperc_LOADV32be)";
                        break;
         case Ity_I16:  helper = &MC_(helperc_LOADV16be);
                        hname = "MC_(helperc_LOADV16be)";
                        break;
         case Ity_I8:   helper = &MC_(helperc_LOADV8);
                        hname = "MC_(helperc_LOADV8)";
                        break;
         default:       ppIRType(ty);
                        VG_(tool_panic)("memcheck:expr2vbits_Load_WRK(BE)");
      }
   }

   tl_assert(helper);
   tl_assert(hname);

   /* Generate the actual address into addrAct. */
   IRAtom* addrAct;
   if (bias == 0) {
      addrAct = addr;
   } else {
      IROp    mkAdd;
      IRAtom* eBias;
      IRType  tyAddr  = mce->hWordTy;
      tl_assert( tyAddr == Ity_I32 || tyAddr == Ity_I64 );
      mkAdd   = tyAddr==Ity_I32 ? Iop_Add32 : Iop_Add64;
      eBias   = tyAddr==Ity_I32 ? mkU32(bias) : mkU64(bias);
      addrAct = assignNew('V', mce, tyAddr, binop(mkAdd, addr, eBias) );
   }

   /* We need to have a place to park the V bits we're just about to
      read. */
   IRTemp datavbits = newTemp(mce, ty, VSh);

   /* Here's the call. */
   IRDirty* di;
   if (ret_via_outparam) {
      di = unsafeIRDirty_1_N( datavbits, 
                              2/*regparms*/, 
                              hname, VG_(fnptr_to_fnentry)( helper ), 
                              mkIRExprVec_2( IRExpr_VECRET(), addrAct ) );
   } else {
      di = unsafeIRDirty_1_N( datavbits, 
                              1/*regparms*/, 
                              hname, VG_(fnptr_to_fnentry)( helper ), 
                              mkIRExprVec_1( addrAct ) );
   }

   setHelperAnns( mce, di );
   if (guard) {
      di->guard = guard;
      /* Ideally the didn't-happen return value here would be all-ones
         (all-undefined), so it'd be obvious if it got used
         inadvertently.  We can get by with the IR-mandated default
         value (0b01 repeating, 0x55 etc) as that'll still look pretty
         undefined if it ever leaks out. */
   }
   stmt( 'V', mce, IRStmt_Dirty(di) );

   return mkexpr(datavbits);
}


/* Generate IR to do a shadow load.  The helper is expected to check
   the validity of the address and return the V bits for that address.
   This can optionally be controlled by a guard, which is assumed to
   be True if NULL.  In the case where the guard is False at runtime,
   the helper will return the didn't-do-the-call value of 0x55..55.
   Since that means "completely undefined result", the caller of
   this function will need to fix up the result somehow in that
   case.

   Caller of this function is also expected to have checked the
   definedness of |guard| before this point.
*/
static
IRAtom* expr2vbits_Load ( MCEnv* mce,
                          IREndness end, IRType ty,
                          IRAtom* addr, UInt bias,
                          IRAtom* guard )
{
   tl_assert(end == Iend_LE || end == Iend_BE);
   switch (shadowTypeV(ty)) {
      case Ity_I8:
      case Ity_I16:
      case Ity_I32:
      case Ity_I64:
      case Ity_V128:
      case Ity_V256:
         return expr2vbits_Load_WRK(mce, end, ty, addr, bias, guard);
      default:
         VG_(tool_panic)("expr2vbits_Load");
   }
}


/* The most general handler for guarded loads.  Assumes the
   definedness of GUARD has already been checked by the caller.  A
   GUARD of NULL is assumed to mean "always True".  Generates code to
   check the definedness and validity of ADDR.

   Generate IR to do a shadow load from ADDR and return the V bits.
   The loaded type is TY.  The loaded data is then (shadow) widened by
   using VWIDEN, which can be Iop_INVALID to denote a no-op.  If GUARD
   evaluates to False at run time then the returned Vbits are simply
   VALT instead.  Note therefore that the argument type of VWIDEN must
   be TY and the result type of VWIDEN must equal the type of VALT.
*/
static
IRAtom* expr2vbits_Load_guarded_General ( MCEnv* mce,
                                          IREndness end, IRType ty,
                                          IRAtom* addr, UInt bias,
                                          IRAtom* guard,
                                          IROp vwiden, IRAtom* valt )
{
   /* Sanity check the conversion operation, and also set TYWIDE. */
   IRType tyWide = Ity_INVALID;
   switch (vwiden) {
      case Iop_INVALID:
         tyWide = ty;
         break;
      case Iop_16Uto32: case Iop_16Sto32: case Iop_8Uto32: case Iop_8Sto32:
         tyWide = Ity_I32; 
         break;
      default:
         VG_(tool_panic)("memcheck:expr2vbits_Load_guarded_General");
   }

   /* If the guard evaluates to True, this will hold the loaded V bits
      at TY.  If the guard evaluates to False, this will be all
      ones, meaning "all undefined", in which case we will have to
      replace it using an ITE below. */
   IRAtom* iftrue1
      = assignNew('V', mce, ty,
                  expr2vbits_Load(mce, end, ty, addr, bias, guard));
   /* Now (shadow-) widen the loaded V bits to the desired width.  In
      the guard-is-False case, the allowable widening operators will
      in the worst case (unsigned widening) at least leave the
      pre-widened part as being marked all-undefined, and in the best
      case (signed widening) mark the whole widened result as
      undefined.  Anyway, it doesn't matter really, since in this case
      we will replace said value with the default value |valt| using an
      ITE. */
   IRAtom* iftrue2
      = vwiden == Iop_INVALID
           ? iftrue1
           : assignNew('V', mce, tyWide, unop(vwiden, iftrue1));
   /* These are the V bits we will return if the load doesn't take
      place. */
   IRAtom* iffalse 
      = valt;
   /* Prepare the cond for the ITE.  Convert a NULL cond into
      something that iropt knows how to fold out later. */
   IRAtom* cond
      = guard == NULL  ? mkU1(1)  : guard;
   /* And assemble the final result. */
   return assignNew('V', mce, tyWide, IRExpr_ITE(cond, iftrue2, iffalse));
}


/* A simpler handler for guarded loads, in which there is no
   conversion operation, and the default V bit return (when the guard
   evaluates to False at runtime) is "all defined".  If there is no
   guard expression or the guard is always TRUE this function behaves
   like expr2vbits_Load.  It is assumed that definedness of GUARD has
   already been checked at the call site. */
static
IRAtom* expr2vbits_Load_guarded_Simple ( MCEnv* mce, 
                                         IREndness end, IRType ty, 
                                         IRAtom* addr, UInt bias,
                                         IRAtom *guard )
{
   return expr2vbits_Load_guarded_General(
             mce, end, ty, addr, bias, guard, Iop_INVALID, definedOfType(ty)
          );
}


static
IRAtom* expr2vbits_ITE ( MCEnv* mce, 
                         IRAtom* cond, IRAtom* iftrue, IRAtom* iffalse )
{
   IRAtom *vbitsC, *vbits0, *vbits1;
   IRType ty;
   /* Given ITE(cond, iftrue,  iffalse),  generate
            ITE(cond, iftrue#, iffalse#) `UifU` PCast(cond#)
      That is, steer the V bits like the originals, but trash the 
      result if the steering value is undefined.  This gives 
      lazy propagation. */
   tl_assert(isOriginalAtom(mce, cond));
   tl_assert(isOriginalAtom(mce, iftrue));
   tl_assert(isOriginalAtom(mce, iffalse));

   vbitsC = expr2vbits(mce, cond, HuOth); // could we use HuPCa here?
   vbits1 = expr2vbits(mce, iftrue, HuOth);
   vbits0 = expr2vbits(mce, iffalse, HuOth);
   ty = typeOfIRExpr(mce->sb->tyenv, vbits0);

   return
      mkUifU(mce, ty, assignNew('V', mce, ty, 
                                     IRExpr_ITE(cond, vbits1, vbits0)),
                      mkPCastTo(mce, ty, vbitsC) );
}      

/* --------- This is the main expression-handling function. --------- */

static
IRExpr* expr2vbits ( MCEnv* mce, IRExpr* e,
                     HowUsed hu/*use HuOth if unknown*/ )
{
   switch (e->tag) {

      case Iex_Get:
         return shadow_GET( mce, e->Iex.Get.offset, e->Iex.Get.ty );

      case Iex_GetI:
         return shadow_GETI( mce, e->Iex.GetI.descr, 
                                  e->Iex.GetI.ix, e->Iex.GetI.bias );

      case Iex_RdTmp:
         return IRExpr_RdTmp( findShadowTmpV(mce, e->Iex.RdTmp.tmp) );

      case Iex_Const:
         return definedOfType(shadowTypeV(typeOfIRExpr(mce->sb->tyenv, e)));

      case Iex_Qop:
         return expr2vbits_Qop(
                   mce,
                   e->Iex.Qop.details->op,
                   e->Iex.Qop.details->arg1, e->Iex.Qop.details->arg2,
                   e->Iex.Qop.details->arg3, e->Iex.Qop.details->arg4
                );

      case Iex_Triop:
         return expr2vbits_Triop(
                   mce,
                   e->Iex.Triop.details->op,
                   e->Iex.Triop.details->arg1, e->Iex.Triop.details->arg2,
                   e->Iex.Triop.details->arg3
                );

      case Iex_Binop:
         return expr2vbits_Binop(
                   mce,
                   e->Iex.Binop.op,
                   e->Iex.Binop.arg1, e->Iex.Binop.arg2,
                   hu
                );

      case Iex_Unop:
         return expr2vbits_Unop( mce, e->Iex.Unop.op, e->Iex.Unop.arg );

      case Iex_Load:
         return expr2vbits_Load( mce, e->Iex.Load.end,
                                      e->Iex.Load.ty, 
                                      e->Iex.Load.addr, 0/*addr bias*/, 
                                      NULL/* guard == "always True"*/ );

      case Iex_CCall:
         return mkLazyN( mce, e->Iex.CCall.args, 
                              e->Iex.CCall.retty,
                              e->Iex.CCall.cee );

      case Iex_ITE:
         return expr2vbits_ITE( mce, e->Iex.ITE.cond, e->Iex.ITE.iftrue, 
                                     e->Iex.ITE.iffalse);

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

   ty  = typeOfIRExpr(mce->sb->tyenv, vatom);
   tyH = mce->hWordTy;

   if (tyH == Ity_I32) {
      switch (ty) {
         case Ity_I32:
            return vatom;
         case Ity_I16:
            return assignNew('V', mce, tyH, unop(Iop_16Uto32, vatom));
         case Ity_I8:
            return assignNew('V', mce, tyH, unop(Iop_8Uto32, vatom));
         default:
            goto unhandled;
      }
   } else
   if (tyH == Ity_I64) {
      switch (ty) {
         case Ity_I32:
            return assignNew('V', mce, tyH, unop(Iop_32Uto64, vatom));
         case Ity_I16:
            return assignNew('V', mce, tyH, unop(Iop_32Uto64, 
                   assignNew('V', mce, Ity_I32, unop(Iop_16Uto32, vatom))));
         case Ity_I8:
            return assignNew('V', mce, tyH, unop(Iop_32Uto64, 
                   assignNew('V', mce, Ity_I32, unop(Iop_8Uto32, vatom))));
         default:
            goto unhandled;
      }
   } else {
      goto unhandled;
   }
  unhandled:
   VG_(printf)("\nty = "); ppIRType(ty); VG_(printf)("\n");
   VG_(tool_panic)("zwidenToHostWord");
}


/* Generate a shadow store.  |addr| is always the original address
   atom.  You can pass in either originals or V-bits for the data
   atom, but obviously not both.  This function generates a check for
   the definedness and (indirectly) the validity of |addr|, but only
   when |guard| evaluates to True at run time (or is NULL).

   |guard| :: Ity_I1 controls whether the store really happens; NULL
   means it unconditionally does.  Note that |guard| itself is not
   checked for definedness; the caller of this function must do that
   if necessary.
*/
static 
void do_shadow_Store ( MCEnv* mce, 
                       IREndness end,
                       IRAtom* addr, UInt bias,
                       IRAtom* data, IRAtom* vdata,
                       IRAtom* guard )
{
   IROp     mkAdd;
   IRType   ty, tyAddr;
   void*    helper = NULL;
   const HChar* hname = NULL;
   IRConst* c;

   tyAddr = mce->hWordTy;
   mkAdd  = tyAddr==Ity_I32 ? Iop_Add32 : Iop_Add64;
   tl_assert( tyAddr == Ity_I32 || tyAddr == Ity_I64 );
   tl_assert( end == Iend_LE || end == Iend_BE );

   if (data) {
      tl_assert(!vdata);
      tl_assert(isOriginalAtom(mce, data));
      tl_assert(bias == 0);
      vdata = expr2vbits( mce, data, HuOth );
   } else {
      tl_assert(vdata);
   }

   tl_assert(isOriginalAtom(mce,addr));
   tl_assert(isShadowAtom(mce,vdata));

   if (guard) {
      tl_assert(isOriginalAtom(mce, guard));
      tl_assert(typeOfIRExpr(mce->sb->tyenv, guard) == Ity_I1);
   }

   ty = typeOfIRExpr(mce->sb->tyenv, vdata);

   // If we're not doing undefined value checking, pretend that this value
   // is "all valid".  That lets Vex's optimiser remove some of the V bit
   // shadow computation ops that precede it.
   if (MC_(clo_mc_level) == 1) {
      switch (ty) {
         case Ity_V256: // V256 weirdness -- used four times
                        c = IRConst_V256(V_BITS32_DEFINED); break;
         case Ity_V128: // V128 weirdness -- used twice
                        c = IRConst_V128(V_BITS16_DEFINED); break;
         case Ity_I64:  c = IRConst_U64 (V_BITS64_DEFINED); break;
         case Ity_I32:  c = IRConst_U32 (V_BITS32_DEFINED); break;
         case Ity_I16:  c = IRConst_U16 (V_BITS16_DEFINED); break;
         case Ity_I8:   c = IRConst_U8  (V_BITS8_DEFINED);  break;
         default:       VG_(tool_panic)("memcheck:do_shadow_Store(LE)");
      }
      vdata = IRExpr_Const( c );
   }

   /* First, emit a definedness test for the address.  This also sets
      the address (shadow) to 'defined' following the test.  Both of
      those actions are gated on |guard|. */
   complainIfUndefined( mce, addr, guard );

   /* Now decide which helper function to call to write the data V
      bits into shadow memory. */
   if (end == Iend_LE) {
      switch (ty) {
         case Ity_V256: /* we'll use the helper four times */
         case Ity_V128: /* we'll use the helper twice */
         case Ity_I64: helper = &MC_(helperc_STOREV64le);
                       hname = "MC_(helperc_STOREV64le)";
                       break;
         case Ity_I32: helper = &MC_(helperc_STOREV32le);
                       hname = "MC_(helperc_STOREV32le)";
                       break;
         case Ity_I16: helper = &MC_(helperc_STOREV16le);
                       hname = "MC_(helperc_STOREV16le)";
                       break;
         case Ity_I8:  helper = &MC_(helperc_STOREV8);
                       hname = "MC_(helperc_STOREV8)";
                       break;
         default:      VG_(tool_panic)("memcheck:do_shadow_Store(LE)");
      }
   } else {
      switch (ty) {
         case Ity_V128: /* we'll use the helper twice */
         case Ity_I64: helper = &MC_(helperc_STOREV64be);
                       hname = "MC_(helperc_STOREV64be)";
                       break;
         case Ity_I32: helper = &MC_(helperc_STOREV32be);
                       hname = "MC_(helperc_STOREV32be)";
                       break;
         case Ity_I16: helper = &MC_(helperc_STOREV16be);
                       hname = "MC_(helperc_STOREV16be)";
                       break;
         case Ity_I8:  helper = &MC_(helperc_STOREV8);
                       hname = "MC_(helperc_STOREV8)";
                       break;
         /* Note, no V256 case here, because no big-endian target that
            we support, has 256 vectors. */
         default:      VG_(tool_panic)("memcheck:do_shadow_Store(BE)");
      }
   }

   if (UNLIKELY(ty == Ity_V256)) {

      /* V256-bit case -- phrased in terms of 64 bit units (Qs), with
         Q3 being the most significant lane. */
      /* These are the offsets of the Qs in memory. */
      Int     offQ0, offQ1, offQ2, offQ3;

      /* Various bits for constructing the 4 lane helper calls */
      IRDirty *diQ0,    *diQ1,    *diQ2,    *diQ3;
      IRAtom  *addrQ0,  *addrQ1,  *addrQ2,  *addrQ3;
      IRAtom  *vdataQ0, *vdataQ1, *vdataQ2, *vdataQ3;
      IRAtom  *eBiasQ0, *eBiasQ1, *eBiasQ2, *eBiasQ3;

      if (end == Iend_LE) {
         offQ0 = 0; offQ1 = 8; offQ2 = 16; offQ3 = 24;
      } else {
         offQ3 = 0; offQ2 = 8; offQ1 = 16; offQ0 = 24;
      }

      eBiasQ0 = tyAddr==Ity_I32 ? mkU32(bias+offQ0) : mkU64(bias+offQ0);
      addrQ0  = assignNew('V', mce, tyAddr, binop(mkAdd, addr, eBiasQ0) );
      vdataQ0 = assignNew('V', mce, Ity_I64, unop(Iop_V256to64_0, vdata));
      diQ0    = unsafeIRDirty_0_N( 
                   1/*regparms*/, 
                   hname, VG_(fnptr_to_fnentry)( helper ), 
                   mkIRExprVec_2( addrQ0, vdataQ0 )
                );

      eBiasQ1 = tyAddr==Ity_I32 ? mkU32(bias+offQ1) : mkU64(bias+offQ1);
      addrQ1  = assignNew('V', mce, tyAddr, binop(mkAdd, addr, eBiasQ1) );
      vdataQ1 = assignNew('V', mce, Ity_I64, unop(Iop_V256to64_1, vdata));
      diQ1    = unsafeIRDirty_0_N( 
                   1/*regparms*/, 
                   hname, VG_(fnptr_to_fnentry)( helper ), 
                   mkIRExprVec_2( addrQ1, vdataQ1 )
                );

      eBiasQ2 = tyAddr==Ity_I32 ? mkU32(bias+offQ2) : mkU64(bias+offQ2);
      addrQ2  = assignNew('V', mce, tyAddr, binop(mkAdd, addr, eBiasQ2) );
      vdataQ2 = assignNew('V', mce, Ity_I64, unop(Iop_V256to64_2, vdata));
      diQ2    = unsafeIRDirty_0_N( 
                   1/*regparms*/, 
                   hname, VG_(fnptr_to_fnentry)( helper ), 
                   mkIRExprVec_2( addrQ2, vdataQ2 )
                );

      eBiasQ3 = tyAddr==Ity_I32 ? mkU32(bias+offQ3) : mkU64(bias+offQ3);
      addrQ3  = assignNew('V', mce, tyAddr, binop(mkAdd, addr, eBiasQ3) );
      vdataQ3 = assignNew('V', mce, Ity_I64, unop(Iop_V256to64_3, vdata));
      diQ3    = unsafeIRDirty_0_N( 
                   1/*regparms*/, 
                   hname, VG_(fnptr_to_fnentry)( helper ), 
                   mkIRExprVec_2( addrQ3, vdataQ3 )
                );

      if (guard)
         diQ0->guard = diQ1->guard = diQ2->guard = diQ3->guard = guard;

      setHelperAnns( mce, diQ0 );
      setHelperAnns( mce, diQ1 );
      setHelperAnns( mce, diQ2 );
      setHelperAnns( mce, diQ3 );
      stmt( 'V', mce, IRStmt_Dirty(diQ0) );
      stmt( 'V', mce, IRStmt_Dirty(diQ1) );
      stmt( 'V', mce, IRStmt_Dirty(diQ2) );
      stmt( 'V', mce, IRStmt_Dirty(diQ3) );

   } 
   else if (UNLIKELY(ty == Ity_V128)) {

      /* V128-bit case */
      /* See comment in next clause re 64-bit regparms */
      /* also, need to be careful about endianness */

      Int     offLo64, offHi64;
      IRDirty *diLo64, *diHi64;
      IRAtom  *addrLo64, *addrHi64;
      IRAtom  *vdataLo64, *vdataHi64;
      IRAtom  *eBiasLo64, *eBiasHi64;

      if (end == Iend_LE) {
         offLo64 = 0;
         offHi64 = 8;
      } else {
         offLo64 = 8;
         offHi64 = 0;
      }

      eBiasLo64 = tyAddr==Ity_I32 ? mkU32(bias+offLo64) : mkU64(bias+offLo64);
      addrLo64  = assignNew('V', mce, tyAddr, binop(mkAdd, addr, eBiasLo64) );
      vdataLo64 = assignNew('V', mce, Ity_I64, unop(Iop_V128to64, vdata));
      diLo64    = unsafeIRDirty_0_N( 
                     1/*regparms*/, 
                     hname, VG_(fnptr_to_fnentry)( helper ), 
                     mkIRExprVec_2( addrLo64, vdataLo64 )
                  );
      eBiasHi64 = tyAddr==Ity_I32 ? mkU32(bias+offHi64) : mkU64(bias+offHi64);
      addrHi64  = assignNew('V', mce, tyAddr, binop(mkAdd, addr, eBiasHi64) );
      vdataHi64 = assignNew('V', mce, Ity_I64, unop(Iop_V128HIto64, vdata));
      diHi64    = unsafeIRDirty_0_N( 
                     1/*regparms*/, 
                     hname, VG_(fnptr_to_fnentry)( helper ), 
                     mkIRExprVec_2( addrHi64, vdataHi64 )
                  );
      if (guard) diLo64->guard = guard;
      if (guard) diHi64->guard = guard;
      setHelperAnns( mce, diLo64 );
      setHelperAnns( mce, diHi64 );
      stmt( 'V', mce, IRStmt_Dirty(diLo64) );
      stmt( 'V', mce, IRStmt_Dirty(diHi64) );

   } else {

      IRDirty *di;
      IRAtom  *addrAct;

      /* 8/16/32/64-bit cases */
      /* Generate the actual address into addrAct. */
      if (bias == 0) {
         addrAct = addr;
      } else {
         IRAtom* eBias   = tyAddr==Ity_I32 ? mkU32(bias) : mkU64(bias);
         addrAct = assignNew('V', mce, tyAddr, binop(mkAdd, addr, eBias));
      }

      if (ty == Ity_I64) {
         /* We can't do this with regparm 2 on 32-bit platforms, since
            the back ends aren't clever enough to handle 64-bit
            regparm args.  Therefore be different. */
         di = unsafeIRDirty_0_N( 
                 1/*regparms*/, 
                 hname, VG_(fnptr_to_fnentry)( helper ), 
                 mkIRExprVec_2( addrAct, vdata )
              );
      } else {
         di = unsafeIRDirty_0_N( 
                 2/*regparms*/, 
                 hname, VG_(fnptr_to_fnentry)( helper ), 
                 mkIRExprVec_2( addrAct,
                                zwidenToHostWord( mce, vdata ))
              );
      }
      if (guard) di->guard = guard;
      setHelperAnns( mce, di );
      stmt( 'V', mce, IRStmt_Dirty(di) );
   }

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
   Int       i, k, n, toDo, gSz, gOff;
   IRAtom    *src, *here, *curr;
   IRType    tySrc, tyDst;
   IRTemp    dst;
   IREndness end;

   /* What's the native endianness?  We need to know this. */
#  if defined(VG_BIGENDIAN)
   end = Iend_BE;
#  elif defined(VG_LITTLEENDIAN)
   end = Iend_LE;
#  else
#    error "Unknown endianness"
#  endif

   /* First check the guard. */
   complainIfUndefined(mce, d->guard, NULL);

   /* Now round up all inputs and PCast over them. */
   curr = definedOfType(Ity_I32);

   /* Inputs: unmasked args
      Note: arguments are evaluated REGARDLESS of the guard expression */
   for (i = 0; d->args[i]; i++) {
      IRAtom* arg = d->args[i];
      if ( (d->cee->mcx_mask & (1<<i))
           || UNLIKELY(is_IRExpr_VECRET_or_GSPTR(arg)) ) {
         /* ignore this arg */
      } else {
         here = mkPCastTo( mce, Ity_I32, expr2vbits(mce, arg, HuOth) );
         curr = mkUifU32(mce, here, curr);
      }
   }

   /* Inputs: guest state that we read. */
   for (i = 0; i < d->nFxState; i++) {
      tl_assert(d->fxState[i].fx != Ifx_None);
      if (d->fxState[i].fx == Ifx_Write)
         continue;

      /* Enumerate the described state segments */
      for (k = 0; k < 1 + d->fxState[i].nRepeats; k++) {
         gOff = d->fxState[i].offset + k * d->fxState[i].repeatLen;
         gSz  = d->fxState[i].size;

         /* Ignore any sections marked as 'always defined'. */
         if (isAlwaysDefd(mce, gOff, gSz)) {
            if (0)
            VG_(printf)("memcheck: Dirty gst: ignored off %d, sz %d\n",
                        gOff, gSz);
            continue;
         }

         /* This state element is read or modified.  So we need to
            consider it.  If larger than 8 bytes, deal with it in
            8-byte chunks. */
         while (True) {
            tl_assert(gSz >= 0);
            if (gSz == 0) break;
            n = gSz <= 8 ? gSz : 8;
            /* update 'curr' with UifU of the state slice 
               gOff .. gOff+n-1 */
            tySrc = szToITy( n );

            /* Observe the guard expression. If it is false use an
               all-bits-defined bit pattern */
            IRAtom *cond, *iffalse, *iftrue;

            cond    = assignNew('V', mce, Ity_I1, d->guard);
            iftrue  = assignNew('V', mce, tySrc, shadow_GET(mce, gOff, tySrc));
            iffalse = assignNew('V', mce, tySrc, definedOfType(tySrc));
            src     = assignNew('V', mce, tySrc,
                                IRExpr_ITE(cond, iftrue, iffalse));

            here = mkPCastTo( mce, Ity_I32, src );
            curr = mkUifU32(mce, here, curr);
            gSz -= n;
            gOff += n;
         }
      }
   }

   /* Inputs: memory.  First set up some info needed regardless of
      whether we're doing reads or writes. */

   if (d->mFx != Ifx_None) {
      /* Because we may do multiple shadow loads/stores from the same
         base address, it's best to do a single test of its
         definedness right now.  Post-instrumentation optimisation
         should remove all but this test. */
      IRType tyAddr;
      tl_assert(d->mAddr);
      complainIfUndefined(mce, d->mAddr, d->guard);

      tyAddr = typeOfIRExpr(mce->sb->tyenv, d->mAddr);
      tl_assert(tyAddr == Ity_I32 || tyAddr == Ity_I64);
      tl_assert(tyAddr == mce->hWordTy); /* not really right */
   }

   /* Deal with memory inputs (reads or modifies) */
   if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify) {
      toDo   = d->mSize;
      /* chew off 32-bit chunks.  We don't care about the endianness
         since it's all going to be condensed down to a single bit,
         but nevertheless choose an endianness which is hopefully
         native to the platform. */
      while (toDo >= 4) {
         here = mkPCastTo( 
                   mce, Ity_I32,
                   expr2vbits_Load_guarded_Simple(
                      mce, end, Ity_I32, d->mAddr, d->mSize - toDo, d->guard )
                );
         curr = mkUifU32(mce, here, curr);
         toDo -= 4;
      }
      /* chew off 16-bit chunks */
      while (toDo >= 2) {
         here = mkPCastTo( 
                   mce, Ity_I32,
                   expr2vbits_Load_guarded_Simple(
                      mce, end, Ity_I16, d->mAddr, d->mSize - toDo, d->guard )
                );
         curr = mkUifU32(mce, here, curr);
         toDo -= 2;
      }
      /* chew off the remaining 8-bit chunk, if any */
      if (toDo == 1) {
         here = mkPCastTo( 
                   mce, Ity_I32,
                   expr2vbits_Load_guarded_Simple(
                      mce, end, Ity_I8, d->mAddr, d->mSize - toDo, d->guard )
                );
         curr = mkUifU32(mce, here, curr);
         toDo -= 1;
      }
      tl_assert(toDo == 0);
   }

   /* Whew!  So curr is a 32-bit V-value summarising pessimistically
      all the inputs to the helper.  Now we need to re-distribute the
      results to all destinations. */

   /* Outputs: the destination temporary, if there is one. */
   if (d->tmp != IRTemp_INVALID) {
      dst   = findShadowTmpV(mce, d->tmp);
      tyDst = typeOfIRTemp(mce->sb->tyenv, d->tmp);
      assign( 'V', mce, dst, mkPCastTo( mce, tyDst, curr) );
   }

   /* Outputs: guest state that we write or modify. */
   for (i = 0; i < d->nFxState; i++) {
      tl_assert(d->fxState[i].fx != Ifx_None);
      if (d->fxState[i].fx == Ifx_Read)
         continue;

      /* Enumerate the described state segments */
      for (k = 0; k < 1 + d->fxState[i].nRepeats; k++) {
         gOff = d->fxState[i].offset + k * d->fxState[i].repeatLen;
         gSz  = d->fxState[i].size;

         /* Ignore any sections marked as 'always defined'. */
         if (isAlwaysDefd(mce, gOff, gSz))
            continue;

         /* This state element is written or modified.  So we need to
            consider it.  If larger than 8 bytes, deal with it in
            8-byte chunks. */
         while (True) {
            tl_assert(gSz >= 0);
            if (gSz == 0) break;
            n = gSz <= 8 ? gSz : 8;
            /* Write suitably-casted 'curr' to the state slice 
               gOff .. gOff+n-1 */
            tyDst = szToITy( n );
            do_shadow_PUT( mce, gOff,
                                NULL, /* original atom */
                                mkPCastTo( mce, tyDst, curr ), d->guard );
            gSz -= n;
            gOff += n;
         }
      }
   }

   /* Outputs: memory that we write or modify.  Same comments about
      endianness as above apply. */
   if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify) {
      toDo   = d->mSize;
      /* chew off 32-bit chunks */
      while (toDo >= 4) {
         do_shadow_Store( mce, end, d->mAddr, d->mSize - toDo,
                          NULL, /* original data */
                          mkPCastTo( mce, Ity_I32, curr ),
                          d->guard );
         toDo -= 4;
      }
      /* chew off 16-bit chunks */
      while (toDo >= 2) {
         do_shadow_Store( mce, end, d->mAddr, d->mSize - toDo,
                          NULL, /* original data */
                          mkPCastTo( mce, Ity_I16, curr ),
                          d->guard );
         toDo -= 2;
      }
      /* chew off the remaining 8-bit chunk, if any */
      if (toDo == 1) {
         do_shadow_Store( mce, end, d->mAddr, d->mSize - toDo,
                          NULL, /* original data */
                          mkPCastTo( mce, Ity_I8, curr ),
                          d->guard );
         toDo -= 1;
      }
      tl_assert(toDo == 0);
   }

}


/* We have an ABI hint telling us that [base .. base+len-1] is to
   become undefined ("writable").  Generate code to call a helper to
   notify the A/V bit machinery of this fact.

   We call 
   void MC_(helperc_MAKE_STACK_UNINIT) ( Addr base, UWord len,
                                                    Addr nia );
*/
static
void do_AbiHint ( MCEnv* mce, IRExpr* base, Int len, IRExpr* nia )
{
   IRDirty* di;

   if (MC_(clo_mc_level) == 3) {
      di = unsafeIRDirty_0_N(
              3/*regparms*/,
              "MC_(helperc_MAKE_STACK_UNINIT_w_o)",
              VG_(fnptr_to_fnentry)( &MC_(helperc_MAKE_STACK_UNINIT_w_o) ),
              mkIRExprVec_3( base, mkIRExpr_HWord( (UInt)len), nia )
           );
   } else {
      /* We ignore the supplied nia, since it is irrelevant. */
      tl_assert(MC_(clo_mc_level) == 2 || MC_(clo_mc_level) == 1);
      /* Special-case the len==128 case, since that is for amd64-ELF,
         which is a very common target. */
      if (len == 128) {
         di = unsafeIRDirty_0_N(
                 1/*regparms*/,
                 "MC_(helperc_MAKE_STACK_UNINIT_128_no_o)",
                 VG_(fnptr_to_fnentry)( &MC_(helperc_MAKE_STACK_UNINIT_128_no_o)),
                 mkIRExprVec_1( base )
              );
      } else {
         di = unsafeIRDirty_0_N(
                 2/*regparms*/,
                 "MC_(helperc_MAKE_STACK_UNINIT_no_o)",
                 VG_(fnptr_to_fnentry)( &MC_(helperc_MAKE_STACK_UNINIT_no_o) ),
                 mkIRExprVec_2( base, mkIRExpr_HWord( (UInt)len) )
              );
      }
   }

   stmt( 'V', mce, IRStmt_Dirty(di) );
}


/* ------ Dealing with IRCAS (big and complex) ------ */

/* FWDS */
static IRAtom* gen_load_b  ( MCEnv* mce, Int szB, 
                             IRAtom* baseaddr, Int offset );
static IRAtom* gen_maxU32  ( MCEnv* mce, IRAtom* b1, IRAtom* b2 );
static void    gen_store_b ( MCEnv* mce, Int szB,
                             IRAtom* baseaddr, Int offset, IRAtom* dataB,
                             IRAtom* guard );

static void do_shadow_CAS_single ( MCEnv* mce, IRCAS* cas );
static void do_shadow_CAS_double ( MCEnv* mce, IRCAS* cas );


/* Either ORIG and SHADOW are both IRExpr.RdTmps, or they are both
   IRExpr.Consts, else this asserts.  If they are both Consts, it
   doesn't do anything.  So that just leaves the RdTmp case.

   In which case: this assigns the shadow value SHADOW to the IR
   shadow temporary associated with ORIG.  That is, ORIG, being an
   original temporary, will have a shadow temporary associated with
   it.  However, in the case envisaged here, there will so far have
   been no IR emitted to actually write a shadow value into that
   temporary.  What this routine does is to (emit IR to) copy the
   value in SHADOW into said temporary, so that after this call,
   IRExpr.RdTmps of ORIG's shadow temp will correctly pick up the
   value in SHADOW.

   Point is to allow callers to compute "by hand" a shadow value for
   ORIG, and force it to be associated with ORIG.

   How do we know that that shadow associated with ORIG has not so far
   been assigned to?  Well, we don't per se know that, but supposing
   it had.  Then this routine would create a second assignment to it,
   and later the IR sanity checker would barf.  But that never
   happens.  QED.
*/
static void bind_shadow_tmp_to_orig ( UChar how,
                                      MCEnv* mce,
                                      IRAtom* orig, IRAtom* shadow )
{
   tl_assert(isOriginalAtom(mce, orig));
   tl_assert(isShadowAtom(mce, shadow));
   switch (orig->tag) {
      case Iex_Const:
         tl_assert(shadow->tag == Iex_Const);
         break;
      case Iex_RdTmp:
         tl_assert(shadow->tag == Iex_RdTmp);
         if (how == 'V') {
            assign('V', mce, findShadowTmpV(mce,orig->Iex.RdTmp.tmp),
                   shadow);
         } else {
            tl_assert(how == 'B');
            assign('B', mce, findShadowTmpB(mce,orig->Iex.RdTmp.tmp),
                   shadow);
         }
         break;
      default:
         tl_assert(0);
   }
}


static
void do_shadow_CAS ( MCEnv* mce, IRCAS* cas )
{
   /* Scheme is (both single- and double- cases):

      1. fetch data#,dataB (the proposed new value)

      2. fetch expd#,expdB (what we expect to see at the address)

      3. check definedness of address

      4. load old#,oldB from shadow memory; this also checks
         addressibility of the address

      5. the CAS itself

      6. compute "expected == old".  See COMMENT_ON_CasCmpEQ below.

      7. if "expected == old" (as computed by (6))
            store data#,dataB to shadow memory

      Note that 5 reads 'old' but 4 reads 'old#'.  Similarly, 5 stores
      'data' but 7 stores 'data#'.  Hence it is possible for the
      shadow data to be incorrectly checked and/or updated:

      * 7 is at least gated correctly, since the 'expected == old'
        condition is derived from outputs of 5.  However, the shadow
        write could happen too late: imagine after 5 we are
        descheduled, a different thread runs, writes a different
        (shadow) value at the address, and then we resume, hence
        overwriting the shadow value written by the other thread.

      Because the original memory access is atomic, there's no way to
      make both the original and shadow accesses into a single atomic
      thing, hence this is unavoidable.

      At least as Valgrind stands, I don't think it's a problem, since
      we're single threaded *and* we guarantee that there are no
      context switches during the execution of any specific superblock
      -- context switches can only happen at superblock boundaries.

      If Valgrind ever becomes MT in the future, then it might be more
      of a problem.  A possible kludge would be to artificially
      associate with the location, a lock, which we must acquire and
      release around the transaction as a whole.  Hmm, that probably
      would't work properly since it only guards us against other
      threads doing CASs on the same location, not against other
      threads doing normal reads and writes.

      ------------------------------------------------------------

      COMMENT_ON_CasCmpEQ:

      Note two things.  Firstly, in the sequence above, we compute
      "expected == old", but we don't check definedness of it.  Why
      not?  Also, the x86 and amd64 front ends use
      Iop_CasCmp{EQ,NE}{8,16,32,64} comparisons to make the equivalent
      determination (expected == old ?) for themselves, and we also
      don't check definedness for those primops; we just say that the
      result is defined.  Why?  Details follow.

      x86/amd64 contains various forms of locked insns:
      * lock prefix before all basic arithmetic insn; 
        eg lock xorl %reg1,(%reg2)
      * atomic exchange reg-mem
      * compare-and-swaps

      Rather than attempt to represent them all, which would be a
      royal PITA, I used a result from Maurice Herlihy
      (http://en.wikipedia.org/wiki/Maurice_Herlihy), in which he
      demonstrates that compare-and-swap is a primitive more general
      than the other two, and so can be used to represent all of them.
      So the translation scheme for (eg) lock incl (%reg) is as
      follows:

        again:
         old = * %reg
         new = old + 1
         atomically { if (* %reg == old) { * %reg = new } else { goto again } }

      The "atomically" is the CAS bit.  The scheme is always the same:
      get old value from memory, compute new value, atomically stuff
      new value back in memory iff the old value has not changed (iow,
      no other thread modified it in the meantime).  If it has changed
      then we've been out-raced and we have to start over.

      Now that's all very neat, but it has the bad side effect of
      introducing an explicit equality test into the translation.
      Consider the behaviour of said code on a memory location which
      is uninitialised.  We will wind up doing a comparison on
      uninitialised data, and mc duly complains.

      What's difficult about this is, the common case is that the
      location is uncontended, and so we're usually comparing the same
      value (* %reg) with itself.  So we shouldn't complain even if it
      is undefined.  But mc doesn't know that.

      My solution is to mark the == in the IR specially, so as to tell
      mc that it almost certainly compares a value with itself, and we
      should just regard the result as always defined.  Rather than
      add a bit to all IROps, I just cloned Iop_CmpEQ{8,16,32,64} into
      Iop_CasCmpEQ{8,16,32,64} so as not to disturb anything else.

      So there's always the question of, can this give a false
      negative?  eg, imagine that initially, * %reg is defined; and we
      read that; but then in the gap between the read and the CAS, a
      different thread writes an undefined (and different) value at
      the location.  Then the CAS in this thread will fail and we will
      go back to "again:", but without knowing that the trip back
      there was based on an undefined comparison.  No matter; at least
      the other thread won the race and the location is correctly
      marked as undefined.  What if it wrote an uninitialised version
      of the same value that was there originally, though?

      etc etc.  Seems like there's a small corner case in which we
      might lose the fact that something's defined -- we're out-raced
      in between the "old = * reg" and the "atomically {", _and_ the
      other thread is writing in an undefined version of what's
      already there.  Well, that seems pretty unlikely.

      ---

      If we ever need to reinstate it .. code which generates a
      definedness test for "expected == old" was removed at r10432 of
      this file.
   */
   if (cas->oldHi == IRTemp_INVALID) {
      do_shadow_CAS_single( mce, cas );
   } else {
      do_shadow_CAS_double( mce, cas );
   }
}


static void do_shadow_CAS_single ( MCEnv* mce, IRCAS* cas )
{
   IRAtom *vdataLo = NULL, *bdataLo = NULL;
   IRAtom *vexpdLo = NULL, *bexpdLo = NULL;
   IRAtom *voldLo  = NULL, *boldLo  = NULL;
   IRAtom *expd_eq_old = NULL;
   IROp   opCasCmpEQ;
   Int    elemSzB;
   IRType elemTy;
   Bool   otrak = MC_(clo_mc_level) >= 3; /* a shorthand */

   /* single CAS */
   tl_assert(cas->oldHi == IRTemp_INVALID);
   tl_assert(cas->expdHi == NULL);
   tl_assert(cas->dataHi == NULL);

   elemTy = typeOfIRExpr(mce->sb->tyenv, cas->expdLo);
   switch (elemTy) {
      case Ity_I8:  elemSzB = 1; opCasCmpEQ = Iop_CasCmpEQ8;  break;
      case Ity_I16: elemSzB = 2; opCasCmpEQ = Iop_CasCmpEQ16; break;
      case Ity_I32: elemSzB = 4; opCasCmpEQ = Iop_CasCmpEQ32; break;
      case Ity_I64: elemSzB = 8; opCasCmpEQ = Iop_CasCmpEQ64; break;
      default: tl_assert(0); /* IR defn disallows any other types */
   }

   /* 1. fetch data# (the proposed new value) */
   tl_assert(isOriginalAtom(mce, cas->dataLo));
   vdataLo
      = assignNew('V', mce, elemTy, expr2vbits(mce, cas->dataLo, HuOth));
   tl_assert(isShadowAtom(mce, vdataLo));
   if (otrak) {
      bdataLo
         = assignNew('B', mce, Ity_I32, schemeE(mce, cas->dataLo));
      tl_assert(isShadowAtom(mce, bdataLo));
   }

   /* 2. fetch expected# (what we expect to see at the address) */
   tl_assert(isOriginalAtom(mce, cas->expdLo));
   vexpdLo
      = assignNew('V', mce, elemTy, expr2vbits(mce, cas->expdLo, HuOth));
   tl_assert(isShadowAtom(mce, vexpdLo));
   if (otrak) {
      bexpdLo
         = assignNew('B', mce, Ity_I32, schemeE(mce, cas->expdLo));
      tl_assert(isShadowAtom(mce, bexpdLo));
   }

   /* 3. check definedness of address */
   /* 4. fetch old# from shadow memory; this also checks
         addressibility of the address */
   voldLo
      = assignNew(
           'V', mce, elemTy,
           expr2vbits_Load( 
              mce,
              cas->end, elemTy, cas->addr, 0/*Addr bias*/,
              NULL/*always happens*/
        ));
   bind_shadow_tmp_to_orig('V', mce, mkexpr(cas->oldLo), voldLo);
   if (otrak) {
      boldLo
         = assignNew('B', mce, Ity_I32,
                     gen_load_b(mce, elemSzB, cas->addr, 0/*addr bias*/));
      bind_shadow_tmp_to_orig('B', mce, mkexpr(cas->oldLo), boldLo);
   }

   /* 5. the CAS itself */
   stmt( 'C', mce, IRStmt_CAS(cas) );

   /* 6. compute "expected == old" */
   /* See COMMENT_ON_CasCmpEQ in this file background/rationale. */
   /* Note that 'C' is kinda faking it; it is indeed a non-shadow
      tree, but it's not copied from the input block. */
   expd_eq_old
      = assignNew('C', mce, Ity_I1,
                  binop(opCasCmpEQ, cas->expdLo, mkexpr(cas->oldLo)));

   /* 7. if "expected == old"
            store data# to shadow memory */
   do_shadow_Store( mce, cas->end, cas->addr, 0/*bias*/,
                    NULL/*data*/, vdataLo/*vdata*/,
                    expd_eq_old/*guard for store*/ );
   if (otrak) {
      gen_store_b( mce, elemSzB, cas->addr, 0/*offset*/,
                   bdataLo/*bdata*/,
                   expd_eq_old/*guard for store*/ );
   }
}


static void do_shadow_CAS_double ( MCEnv* mce, IRCAS* cas )
{
   IRAtom *vdataHi = NULL, *bdataHi = NULL;
   IRAtom *vdataLo = NULL, *bdataLo = NULL;
   IRAtom *vexpdHi = NULL, *bexpdHi = NULL;
   IRAtom *vexpdLo = NULL, *bexpdLo = NULL;
   IRAtom *voldHi  = NULL, *boldHi  = NULL;
   IRAtom *voldLo  = NULL, *boldLo  = NULL;
   IRAtom *xHi = NULL, *xLo = NULL, *xHL = NULL;
   IRAtom *expd_eq_old = NULL, *zero = NULL;
   IROp   opCasCmpEQ, opOr, opXor;
   Int    elemSzB, memOffsLo, memOffsHi;
   IRType elemTy;
   Bool   otrak = MC_(clo_mc_level) >= 3; /* a shorthand */

   /* double CAS */
   tl_assert(cas->oldHi != IRTemp_INVALID);
   tl_assert(cas->expdHi != NULL);
   tl_assert(cas->dataHi != NULL);

   elemTy = typeOfIRExpr(mce->sb->tyenv, cas->expdLo);
   switch (elemTy) {
      case Ity_I8:
         opCasCmpEQ = Iop_CasCmpEQ8; opOr = Iop_Or8; opXor = Iop_Xor8; 
         elemSzB = 1; zero = mkU8(0);
         break;
      case Ity_I16:
         opCasCmpEQ = Iop_CasCmpEQ16; opOr = Iop_Or16; opXor = Iop_Xor16;
         elemSzB = 2; zero = mkU16(0);
         break;
      case Ity_I32:
         opCasCmpEQ = Iop_CasCmpEQ32; opOr = Iop_Or32; opXor = Iop_Xor32;
         elemSzB = 4; zero = mkU32(0);
         break;
      case Ity_I64:
         opCasCmpEQ = Iop_CasCmpEQ64; opOr = Iop_Or64; opXor = Iop_Xor64;
         elemSzB = 8; zero = mkU64(0);
         break;
      default:
         tl_assert(0); /* IR defn disallows any other types */
   }

   /* 1. fetch data# (the proposed new value) */
   tl_assert(isOriginalAtom(mce, cas->dataHi));
   tl_assert(isOriginalAtom(mce, cas->dataLo));
   vdataHi
      = assignNew('V', mce, elemTy, expr2vbits(mce, cas->dataHi, HuOth));
   vdataLo
      = assignNew('V', mce, elemTy, expr2vbits(mce, cas->dataLo, HuOth));
   tl_assert(isShadowAtom(mce, vdataHi));
   tl_assert(isShadowAtom(mce, vdataLo));
   if (otrak) {
      bdataHi
         = assignNew('B', mce, Ity_I32, schemeE(mce, cas->dataHi));
      bdataLo
         = assignNew('B', mce, Ity_I32, schemeE(mce, cas->dataLo));
      tl_assert(isShadowAtom(mce, bdataHi));
      tl_assert(isShadowAtom(mce, bdataLo));
   }

   /* 2. fetch expected# (what we expect to see at the address) */
   tl_assert(isOriginalAtom(mce, cas->expdHi));
   tl_assert(isOriginalAtom(mce, cas->expdLo));
   vexpdHi
      = assignNew('V', mce, elemTy, expr2vbits(mce, cas->expdHi, HuOth));
   vexpdLo
      = assignNew('V', mce, elemTy, expr2vbits(mce, cas->expdLo, HuOth));
   tl_assert(isShadowAtom(mce, vexpdHi));
   tl_assert(isShadowAtom(mce, vexpdLo));
   if (otrak) {
      bexpdHi
         = assignNew('B', mce, Ity_I32, schemeE(mce, cas->expdHi));
      bexpdLo
         = assignNew('B', mce, Ity_I32, schemeE(mce, cas->expdLo));
      tl_assert(isShadowAtom(mce, bexpdHi));
      tl_assert(isShadowAtom(mce, bexpdLo));
   }

   /* 3. check definedness of address */
   /* 4. fetch old# from shadow memory; this also checks
         addressibility of the address */
   if (cas->end == Iend_LE) {
      memOffsLo = 0;
      memOffsHi = elemSzB;
   } else {
      tl_assert(cas->end == Iend_BE);
      memOffsLo = elemSzB;
      memOffsHi = 0;
   }
   voldHi
      = assignNew(
           'V', mce, elemTy,
           expr2vbits_Load( 
              mce,
              cas->end, elemTy, cas->addr, memOffsHi/*Addr bias*/,
              NULL/*always happens*/
        ));
   voldLo
      = assignNew(
           'V', mce, elemTy,
           expr2vbits_Load( 
              mce,
              cas->end, elemTy, cas->addr, memOffsLo/*Addr bias*/,
              NULL/*always happens*/
        ));
   bind_shadow_tmp_to_orig('V', mce, mkexpr(cas->oldHi), voldHi);
   bind_shadow_tmp_to_orig('V', mce, mkexpr(cas->oldLo), voldLo);
   if (otrak) {
      boldHi
         = assignNew('B', mce, Ity_I32,
                     gen_load_b(mce, elemSzB, cas->addr,
                                memOffsHi/*addr bias*/));
      boldLo
         = assignNew('B', mce, Ity_I32,
                     gen_load_b(mce, elemSzB, cas->addr,
                                memOffsLo/*addr bias*/));
      bind_shadow_tmp_to_orig('B', mce, mkexpr(cas->oldHi), boldHi);
      bind_shadow_tmp_to_orig('B', mce, mkexpr(cas->oldLo), boldLo);
   }

   /* 5. the CAS itself */
   stmt( 'C', mce, IRStmt_CAS(cas) );

   /* 6. compute "expected == old" */
   /* See COMMENT_ON_CasCmpEQ in this file background/rationale. */
   /* Note that 'C' is kinda faking it; it is indeed a non-shadow
      tree, but it's not copied from the input block. */
   /*
      xHi = oldHi ^ expdHi;
      xLo = oldLo ^ expdLo;
      xHL = xHi | xLo;
      expd_eq_old = xHL == 0;
   */
   xHi = assignNew('C', mce, elemTy,
                   binop(opXor, cas->expdHi, mkexpr(cas->oldHi))); 
   xLo = assignNew('C', mce, elemTy,
                   binop(opXor, cas->expdLo, mkexpr(cas->oldLo)));
   xHL = assignNew('C', mce, elemTy,
                   binop(opOr, xHi, xLo));
   expd_eq_old
      = assignNew('C', mce, Ity_I1,
                  binop(opCasCmpEQ, xHL, zero));

   /* 7. if "expected == old"
            store data# to shadow memory */
   do_shadow_Store( mce, cas->end, cas->addr, memOffsHi/*bias*/,
                    NULL/*data*/, vdataHi/*vdata*/,
                    expd_eq_old/*guard for store*/ );
   do_shadow_Store( mce, cas->end, cas->addr, memOffsLo/*bias*/,
                    NULL/*data*/, vdataLo/*vdata*/,
                    expd_eq_old/*guard for store*/ );
   if (otrak) {
      gen_store_b( mce, elemSzB, cas->addr, memOffsHi/*offset*/,
                   bdataHi/*bdata*/,
                   expd_eq_old/*guard for store*/ );
      gen_store_b( mce, elemSzB, cas->addr, memOffsLo/*offset*/,
                   bdataLo/*bdata*/,
                   expd_eq_old/*guard for store*/ );
   }
}


/* ------ Dealing with LL/SC (not difficult) ------ */

static void do_shadow_LLSC ( MCEnv*    mce,
                             IREndness stEnd,
                             IRTemp    stResult,
                             IRExpr*   stAddr,
                             IRExpr*   stStoredata )
{
   /* In short: treat a load-linked like a normal load followed by an
      assignment of the loaded (shadow) data to the result temporary.
      Treat a store-conditional like a normal store, and mark the
      result temporary as defined. */
   IRType resTy  = typeOfIRTemp(mce->sb->tyenv, stResult);
   IRTemp resTmp = findShadowTmpV(mce, stResult);

   tl_assert(isIRAtom(stAddr));
   if (stStoredata)
      tl_assert(isIRAtom(stStoredata));

   if (stStoredata == NULL) {
      /* Load Linked */
      /* Just treat this as a normal load, followed by an assignment of
         the value to .result. */
      /* Stay sane */
      tl_assert(resTy == Ity_I64 || resTy == Ity_I32
                || resTy == Ity_I16 || resTy == Ity_I8);
      assign( 'V', mce, resTmp,
                   expr2vbits_Load(
                      mce, stEnd, resTy, stAddr, 0/*addr bias*/,
                      NULL/*always happens*/) );
   } else {
      /* Store Conditional */
      /* Stay sane */
      IRType dataTy = typeOfIRExpr(mce->sb->tyenv,
                                   stStoredata);
      tl_assert(dataTy == Ity_I64 || dataTy == Ity_I32
                || dataTy == Ity_I16 || dataTy == Ity_I8);
      do_shadow_Store( mce, stEnd,
                            stAddr, 0/* addr bias */,
                            stStoredata,
                            NULL /* shadow data */,
                            NULL/*guard*/ );
      /* This is a store conditional, so it writes to .result a value
         indicating whether or not the store succeeded.  Just claim
         this value is always defined.  In the PowerPC interpretation
         of store-conditional, definedness of the success indication
         depends on whether the address of the store matches the
         reservation address.  But we can't tell that here (and
         anyway, we're not being PowerPC-specific).  At least we are
         guaranteed that the definedness of the store address, and its
         addressibility, will be checked as per normal.  So it seems
         pretty safe to just say that the success indication is always
         defined.

         In schemeS, for origin tracking, we must correspondingly set
         a no-origin value for the origin shadow of .result.
      */
      tl_assert(resTy == Ity_I1);
      assign( 'V', mce, resTmp, definedOfType(resTy) );
   }
}


/* ---- Dealing with LoadG/StoreG (not entirely simple) ---- */

static void do_shadow_StoreG ( MCEnv* mce, IRStoreG* sg )
{
   complainIfUndefined(mce, sg->guard, NULL);
   /* do_shadow_Store will generate code to check the definedness and
      validity of sg->addr, in the case where sg->guard evaluates to
      True at run-time. */
   do_shadow_Store( mce, sg->end,
                    sg->addr, 0/* addr bias */,
                    sg->data,
                    NULL /* shadow data */,
                    sg->guard );
}

static void do_shadow_LoadG ( MCEnv* mce, IRLoadG* lg )
{
   complainIfUndefined(mce, lg->guard, NULL);
   /* expr2vbits_Load_guarded_General will generate code to check the
      definedness and validity of lg->addr, in the case where
      lg->guard evaluates to True at run-time. */

   /* Look at the LoadG's built-in conversion operation, to determine
      the source (actual loaded data) type, and the equivalent IROp.
      NOTE that implicitly we are taking a widening operation to be
      applied to original atoms and producing one that applies to V
      bits.  Since signed and unsigned widening are self-shadowing,
      this is a straight copy of the op (modulo swapping from the
      IRLoadGOp form to the IROp form).  Note also therefore that this
      implicitly duplicates the logic to do with said widening ops in
      expr2vbits_Unop.  See comment at the start of expr2vbits_Unop. */
   IROp   vwiden   = Iop_INVALID;
   IRType loadedTy = Ity_INVALID;
   switch (lg->cvt) {
      case ILGop_IdentV128: loadedTy = Ity_V128; vwiden = Iop_INVALID; break;
      case ILGop_Ident64:   loadedTy = Ity_I64;  vwiden = Iop_INVALID; break;
      case ILGop_Ident32:   loadedTy = Ity_I32;  vwiden = Iop_INVALID; break;
      case ILGop_16Uto32:   loadedTy = Ity_I16;  vwiden = Iop_16Uto32; break;
      case ILGop_16Sto32:   loadedTy = Ity_I16;  vwiden = Iop_16Sto32; break;
      case ILGop_8Uto32:    loadedTy = Ity_I8;   vwiden = Iop_8Uto32;  break;
      case ILGop_8Sto32:    loadedTy = Ity_I8;   vwiden = Iop_8Sto32;  break;
      default: VG_(tool_panic)("do_shadow_LoadG");
   }

   IRAtom* vbits_alt
      = expr2vbits( mce, lg->alt, HuOth );
   IRAtom* vbits_final
      = expr2vbits_Load_guarded_General(mce, lg->end, loadedTy,
                                        lg->addr, 0/*addr bias*/,
                                        lg->guard, vwiden, vbits_alt );
   /* And finally, bind the V bits to the destination temporary. */
   assign( 'V', mce, findShadowTmpV(mce, lg->dst), vbits_final );
}


/*------------------------------------------------------------*/
/*--- Origin tracking stuff                                ---*/
/*------------------------------------------------------------*/

/* Almost identical to findShadowTmpV. */
static IRTemp findShadowTmpB ( MCEnv* mce, IRTemp orig )
{
   TempMapEnt* ent;
   /* VG_(indexXA) range-checks 'orig', hence no need to check
      here. */
   ent = (TempMapEnt*)VG_(indexXA)( mce->tmpMap, (Word)orig );
   tl_assert(ent->kind == Orig);
   if (ent->shadowB == IRTemp_INVALID) {
      IRTemp tmpB
        = newTemp( mce, Ity_I32, BSh );
      /* newTemp may cause mce->tmpMap to resize, hence previous results
         from VG_(indexXA) are invalid. */
      ent = (TempMapEnt*)VG_(indexXA)( mce->tmpMap, (Word)orig );
      tl_assert(ent->kind == Orig);
      tl_assert(ent->shadowB == IRTemp_INVALID);
      ent->shadowB = tmpB;
   }
   return ent->shadowB;
}

static IRAtom* gen_maxU32 ( MCEnv* mce, IRAtom* b1, IRAtom* b2 )
{
   return assignNew( 'B', mce, Ity_I32, binop(Iop_Max32U, b1, b2) );
}


/* Make a guarded origin load, with no special handling in the
   didn't-happen case.  A GUARD of NULL is assumed to mean "always
   True".

   Generate IR to do a shadow origins load from BASEADDR+OFFSET and
   return the otag.  The loaded size is SZB.  If GUARD evaluates to
   False at run time then the returned otag is zero.
*/
static IRAtom* gen_guarded_load_b ( MCEnv* mce, Int szB, 
                                    IRAtom* baseaddr, 
                                    Int offset, IRExpr* guard )
{
   void*    hFun;
   const HChar* hName;
   IRTemp   bTmp;
   IRDirty* di;
   IRType   aTy   = typeOfIRExpr( mce->sb->tyenv, baseaddr );
   IROp     opAdd = aTy == Ity_I32 ? Iop_Add32 : Iop_Add64;
   IRAtom*  ea    = baseaddr;
   if (offset != 0) {
      IRAtom* off = aTy == Ity_I32 ? mkU32( offset )
                                   : mkU64( (Long)(Int)offset );
      ea = assignNew( 'B', mce, aTy, binop(opAdd, ea, off));
   }
   bTmp = newTemp(mce, mce->hWordTy, BSh);

   switch (szB) {
      case 1: hFun  = (void*)&MC_(helperc_b_load1);
              hName = "MC_(helperc_b_load1)";
              break;
      case 2: hFun  = (void*)&MC_(helperc_b_load2);
              hName = "MC_(helperc_b_load2)";
              break;
      case 4: hFun  = (void*)&MC_(helperc_b_load4);
              hName = "MC_(helperc_b_load4)";
              break;
      case 8: hFun  = (void*)&MC_(helperc_b_load8);
              hName = "MC_(helperc_b_load8)";
              break;
      case 16: hFun  = (void*)&MC_(helperc_b_load16);
               hName = "MC_(helperc_b_load16)";
               break;
      case 32: hFun  = (void*)&MC_(helperc_b_load32);
               hName = "MC_(helperc_b_load32)";
               break;
      default:
         VG_(printf)("mc_translate.c: gen_load_b: unhandled szB == %d\n", szB);
         tl_assert(0);
   }
   di = unsafeIRDirty_1_N(
           bTmp, 1/*regparms*/, hName, VG_(fnptr_to_fnentry)( hFun ),
           mkIRExprVec_1( ea )
        );
   if (guard) {
      di->guard = guard;
      /* Ideally the didn't-happen return value here would be
         all-zeroes (unknown-origin), so it'd be harmless if it got
         used inadvertently.  We slum it out with the IR-mandated
         default value (0b01 repeating, 0x55 etc) as that'll probably
         trump all legitimate otags via Max32, and it's pretty
         obviously bogus. */
   }
   /* no need to mess with any annotations.  This call accesses
      neither guest state nor guest memory. */
   stmt( 'B', mce, IRStmt_Dirty(di) );
   if (mce->hWordTy == Ity_I64) {
      /* 64-bit host */
      IRTemp bTmp32 = newTemp(mce, Ity_I32, BSh);
      assign( 'B', mce, bTmp32, unop(Iop_64to32, mkexpr(bTmp)) );
      return mkexpr(bTmp32);
   } else {
      /* 32-bit host */
      return mkexpr(bTmp);
   }
}


/* Generate IR to do a shadow origins load from BASEADDR+OFFSET.  The
   loaded size is SZB.  The load is regarded as unconditional (always
   happens).
*/
static IRAtom* gen_load_b ( MCEnv* mce, Int szB, IRAtom* baseaddr,
                            Int offset )
{
   return gen_guarded_load_b(mce, szB, baseaddr, offset, NULL/*guard*/);
}


/* The most general handler for guarded origin loads.  A GUARD of NULL
   is assumed to mean "always True".

   Generate IR to do a shadow origin load from ADDR+BIAS and return
   the B bits.  The loaded type is TY.  If GUARD evaluates to False at
   run time then the returned B bits are simply BALT instead.
*/
static
IRAtom* expr2ori_Load_guarded_General ( MCEnv* mce,
                                        IRType ty,
                                        IRAtom* addr, UInt bias,
                                        IRAtom* guard, IRAtom* balt )
{
   /* If the guard evaluates to True, this will hold the loaded
      origin.  If the guard evaluates to False, this will be zero,
      meaning "unknown origin", in which case we will have to replace
      it using an ITE below. */
   IRAtom* iftrue
      = assignNew('B', mce, Ity_I32,
                  gen_guarded_load_b(mce, sizeofIRType(ty),
                                     addr, bias, guard));
   /* These are the bits we will return if the load doesn't take
      place. */
   IRAtom* iffalse 
      = balt;
   /* Prepare the cond for the ITE.  Convert a NULL cond into
      something that iropt knows how to fold out later. */
   IRAtom* cond
      = guard == NULL  ? mkU1(1)  : guard;
   /* And assemble the final result. */
   return assignNew('B', mce, Ity_I32, IRExpr_ITE(cond, iftrue, iffalse));
}


/* Generate a shadow origins store.  guard :: Ity_I1 controls whether
   the store really happens; NULL means it unconditionally does. */
static void gen_store_b ( MCEnv* mce, Int szB,
                          IRAtom* baseaddr, Int offset, IRAtom* dataB,
                          IRAtom* guard )
{
   void*    hFun;
   const HChar* hName;
   IRDirty* di;
   IRType   aTy   = typeOfIRExpr( mce->sb->tyenv, baseaddr );
   IROp     opAdd = aTy == Ity_I32 ? Iop_Add32 : Iop_Add64;
   IRAtom*  ea    = baseaddr;
   if (guard) {
      tl_assert(isOriginalAtom(mce, guard));
      tl_assert(typeOfIRExpr(mce->sb->tyenv, guard) == Ity_I1);
   }
   if (offset != 0) {
      IRAtom* off = aTy == Ity_I32 ? mkU32( offset )
                                   : mkU64( (Long)(Int)offset );
      ea = assignNew(  'B', mce, aTy, binop(opAdd, ea, off));
   }
   if (mce->hWordTy == Ity_I64)
      dataB = assignNew( 'B', mce, Ity_I64, unop(Iop_32Uto64, dataB));

   switch (szB) {
      case 1: hFun  = (void*)&MC_(helperc_b_store1);
              hName = "MC_(helperc_b_store1)";
              break;
      case 2: hFun  = (void*)&MC_(helperc_b_store2);
              hName = "MC_(helperc_b_store2)";
              break;
      case 4: hFun  = (void*)&MC_(helperc_b_store4);
              hName = "MC_(helperc_b_store4)";
              break;
      case 8: hFun  = (void*)&MC_(helperc_b_store8);
              hName = "MC_(helperc_b_store8)";
              break;
      case 16: hFun  = (void*)&MC_(helperc_b_store16);
               hName = "MC_(helperc_b_store16)";
               break;
      case 32: hFun  = (void*)&MC_(helperc_b_store32);
               hName = "MC_(helperc_b_store32)";
               break;
      default:
         tl_assert(0);
   }
   di = unsafeIRDirty_0_N( 2/*regparms*/,
           hName, VG_(fnptr_to_fnentry)( hFun ),
           mkIRExprVec_2( ea, dataB )
        );
   /* no need to mess with any annotations.  This call accesses
      neither guest state nor guest memory. */
   if (guard) di->guard = guard;
   stmt( 'B', mce, IRStmt_Dirty(di) );
}

static IRAtom* narrowTo32 ( MCEnv* mce, IRAtom* e ) {
   IRType eTy = typeOfIRExpr(mce->sb->tyenv, e);
   if (eTy == Ity_I64)
      return assignNew( 'B', mce, Ity_I32, unop(Iop_64to32, e) );
   if (eTy == Ity_I32)
      return e;
   tl_assert(0);
}

static IRAtom* zWidenFrom32 ( MCEnv* mce, IRType dstTy, IRAtom* e ) {
   IRType eTy = typeOfIRExpr(mce->sb->tyenv, e);
   tl_assert(eTy == Ity_I32);
   if (dstTy == Ity_I64)
      return assignNew( 'B', mce, Ity_I64, unop(Iop_32Uto64, e) );
   tl_assert(0);
}


static IRAtom* schemeE ( MCEnv* mce, IRExpr* e )
{
   tl_assert(MC_(clo_mc_level) == 3);

   switch (e->tag) {

      case Iex_GetI: {
         IRRegArray* descr_b;
         IRAtom      *t1, *t2, *t3, *t4;
         IRRegArray* descr      = e->Iex.GetI.descr;
         IRType equivIntTy 
            = MC_(get_otrack_reg_array_equiv_int_type)(descr);
         /* If this array is unshadowable for whatever reason, use the
            usual approximation. */
         if (equivIntTy == Ity_INVALID)
            return mkU32(0);
         tl_assert(sizeofIRType(equivIntTy) >= 4);
         tl_assert(sizeofIRType(equivIntTy) == sizeofIRType(descr->elemTy));
         descr_b = mkIRRegArray( descr->base + 2*mce->layout->total_sizeB,
                                 equivIntTy, descr->nElems );
         /* Do a shadow indexed get of the same size, giving t1.  Take
            the bottom 32 bits of it, giving t2.  Compute into t3 the
            origin for the index (almost certainly zero, but there's
            no harm in being completely general here, since iropt will
            remove any useless code), and fold it in, giving a final
            value t4. */
         t1 = assignNew( 'B', mce, equivIntTy, 
                          IRExpr_GetI( descr_b, e->Iex.GetI.ix, 
                                                e->Iex.GetI.bias ));
         t2 = narrowTo32( mce, t1 );
         t3 = schemeE( mce, e->Iex.GetI.ix );
         t4 = gen_maxU32( mce, t2, t3 );
         return t4;
      }
      case Iex_CCall: {
         Int i;
         IRAtom*  here;
         IRExpr** args = e->Iex.CCall.args;
         IRAtom*  curr = mkU32(0);
         for (i = 0; args[i]; i++) {
            tl_assert(i < 32);
            tl_assert(isOriginalAtom(mce, args[i]));
            /* Only take notice of this arg if the callee's
               mc-exclusion mask does not say it is to be excluded. */
            if (e->Iex.CCall.cee->mcx_mask & (1<<i)) {
               /* the arg is to be excluded from definedness checking.
                  Do nothing. */
               if (0) VG_(printf)("excluding %s(%d)\n",
                                  e->Iex.CCall.cee->name, i);
            } else {
               /* calculate the arg's definedness, and pessimistically
                  merge it in. */
               here = schemeE( mce, args[i] );
               curr = gen_maxU32( mce, curr, here );
            }
         }
         return curr;
      }
      case Iex_Load: {
         Int dszB;
         dszB = sizeofIRType(e->Iex.Load.ty);
         /* assert that the B value for the address is already
            available (somewhere) */
         tl_assert(isIRAtom(e->Iex.Load.addr));
         tl_assert(mce->hWordTy == Ity_I32 || mce->hWordTy == Ity_I64);
         return gen_load_b( mce, dszB, e->Iex.Load.addr, 0 );
      }
      case Iex_ITE: {
         IRAtom* b1 = schemeE( mce, e->Iex.ITE.cond );
         IRAtom* b3 = schemeE( mce, e->Iex.ITE.iftrue );
         IRAtom* b2 = schemeE( mce, e->Iex.ITE.iffalse );
         return gen_maxU32( mce, b1, gen_maxU32( mce, b2, b3 ));
      }
      case Iex_Qop: {
         IRAtom* b1 = schemeE( mce, e->Iex.Qop.details->arg1 );
         IRAtom* b2 = schemeE( mce, e->Iex.Qop.details->arg2 );
         IRAtom* b3 = schemeE( mce, e->Iex.Qop.details->arg3 );
         IRAtom* b4 = schemeE( mce, e->Iex.Qop.details->arg4 );
         return gen_maxU32( mce, gen_maxU32( mce, b1, b2 ),
                                 gen_maxU32( mce, b3, b4 ) );
      }
      case Iex_Triop: {
         IRAtom* b1 = schemeE( mce, e->Iex.Triop.details->arg1 );
         IRAtom* b2 = schemeE( mce, e->Iex.Triop.details->arg2 );
         IRAtom* b3 = schemeE( mce, e->Iex.Triop.details->arg3 );
         return gen_maxU32( mce, b1, gen_maxU32( mce, b2, b3 ) );
      }
      case Iex_Binop: {
         switch (e->Iex.Binop.op) {
            case Iop_CasCmpEQ8:  case Iop_CasCmpNE8:
            case Iop_CasCmpEQ16: case Iop_CasCmpNE16:
            case Iop_CasCmpEQ32: case Iop_CasCmpNE32:
            case Iop_CasCmpEQ64: case Iop_CasCmpNE64:
               /* Just say these all produce a defined result,
                  regardless of their arguments.  See
                  COMMENT_ON_CasCmpEQ in this file. */
               return mkU32(0);
            default: {
               IRAtom* b1 = schemeE( mce, e->Iex.Binop.arg1 );
               IRAtom* b2 = schemeE( mce, e->Iex.Binop.arg2 );
               return gen_maxU32( mce, b1, b2 );
            }
         }
         tl_assert(0);
         /*NOTREACHED*/
      }
      case Iex_Unop: {
         IRAtom* b1 = schemeE( mce, e->Iex.Unop.arg );
         return b1;
      }
      case Iex_Const:
         return mkU32(0);
      case Iex_RdTmp:
         return mkexpr( findShadowTmpB( mce, e->Iex.RdTmp.tmp ));
      case Iex_Get: {
         Int b_offset = MC_(get_otrack_shadow_offset)( 
                           e->Iex.Get.offset,
                           sizeofIRType(e->Iex.Get.ty) 
                        );
         tl_assert(b_offset >= -1
                   && b_offset <= mce->layout->total_sizeB -4);
         if (b_offset >= 0) {
            /* FIXME: this isn't an atom! */
            return IRExpr_Get( b_offset + 2*mce->layout->total_sizeB,
                               Ity_I32 );
         }
         return mkU32(0);
      }
      default:
         VG_(printf)("mc_translate.c: schemeE: unhandled: ");
         ppIRExpr(e); 
         VG_(tool_panic)("memcheck:schemeE");
   }
}


static void do_origins_Dirty ( MCEnv* mce, IRDirty* d )
{
   // This is a hacked version of do_shadow_Dirty
   Int       i, k, n, toDo, gSz, gOff;
   IRAtom    *here, *curr;
   IRTemp    dst;

   /* First check the guard. */
   curr = schemeE( mce, d->guard );

   /* Now round up all inputs and maxU32 over them. */

   /* Inputs: unmasked args
      Note: arguments are evaluated REGARDLESS of the guard expression */
   for (i = 0; d->args[i]; i++) {
      IRAtom* arg = d->args[i];
      if ( (d->cee->mcx_mask & (1<<i))
           || UNLIKELY(is_IRExpr_VECRET_or_GSPTR(arg)) ) {
         /* ignore this arg */
      } else {
         here = schemeE( mce, arg );
         curr = gen_maxU32( mce, curr, here );
      }
   }

   /* Inputs: guest state that we read. */
   for (i = 0; i < d->nFxState; i++) {
      tl_assert(d->fxState[i].fx != Ifx_None);
      if (d->fxState[i].fx == Ifx_Write)
         continue;

      /* Enumerate the described state segments */
      for (k = 0; k < 1 + d->fxState[i].nRepeats; k++) {
         gOff = d->fxState[i].offset + k * d->fxState[i].repeatLen;
         gSz  = d->fxState[i].size;

         /* Ignore any sections marked as 'always defined'. */
         if (isAlwaysDefd(mce, gOff, gSz)) {
            if (0)
            VG_(printf)("memcheck: Dirty gst: ignored off %d, sz %d\n",
                        gOff, gSz);
            continue;
         }

         /* This state element is read or modified.  So we need to
            consider it.  If larger than 4 bytes, deal with it in
            4-byte chunks. */
         while (True) {
            Int b_offset;
            tl_assert(gSz >= 0);
            if (gSz == 0) break;
            n = gSz <= 4 ? gSz : 4;
            /* update 'curr' with maxU32 of the state slice 
               gOff .. gOff+n-1 */
            b_offset = MC_(get_otrack_shadow_offset)(gOff, 4);
            if (b_offset != -1) {
               /* Observe the guard expression. If it is false use 0, i.e.
                  nothing is known about the origin */
               IRAtom *cond, *iffalse, *iftrue;

               cond = assignNew( 'B', mce, Ity_I1, d->guard);
               iffalse = mkU32(0);
               iftrue  = assignNew( 'B', mce, Ity_I32,
                                    IRExpr_Get(b_offset
                                                 + 2*mce->layout->total_sizeB,
                                               Ity_I32));
               here = assignNew( 'B', mce, Ity_I32,
                                 IRExpr_ITE(cond, iftrue, iffalse));
               curr = gen_maxU32( mce, curr, here );
            }
            gSz -= n;
            gOff += n;
         }
      }
   }

   /* Inputs: memory */

   if (d->mFx != Ifx_None) {
      /* Because we may do multiple shadow loads/stores from the same
         base address, it's best to do a single test of its
         definedness right now.  Post-instrumentation optimisation
         should remove all but this test. */
      tl_assert(d->mAddr);
      here = schemeE( mce, d->mAddr );
      curr = gen_maxU32( mce, curr, here );
   }

   /* Deal with memory inputs (reads or modifies) */
   if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify) {
      toDo   = d->mSize;
      /* chew off 32-bit chunks.  We don't care about the endianness
         since it's all going to be condensed down to a single bit,
         but nevertheless choose an endianness which is hopefully
         native to the platform. */
      while (toDo >= 4) {
         here = gen_guarded_load_b( mce, 4, d->mAddr, d->mSize - toDo,
                                    d->guard );
         curr = gen_maxU32( mce, curr, here );
         toDo -= 4;
      }
      /* handle possible 16-bit excess */
      while (toDo >= 2) {
         here = gen_guarded_load_b( mce, 2, d->mAddr, d->mSize - toDo,
                                    d->guard );
         curr = gen_maxU32( mce, curr, here );
         toDo -= 2;
      }
      /* chew off the remaining 8-bit chunk, if any */
      if (toDo == 1) {
         here = gen_guarded_load_b( mce, 1, d->mAddr, d->mSize - toDo,
                                    d->guard );
         curr = gen_maxU32( mce, curr, here );
         toDo -= 1;
      }
      tl_assert(toDo == 0);
   }

   /* Whew!  So curr is a 32-bit B-value which should give an origin
      of some use if any of the inputs to the helper are undefined.
      Now we need to re-distribute the results to all destinations. */

   /* Outputs: the destination temporary, if there is one. */
   if (d->tmp != IRTemp_INVALID) {
      dst   = findShadowTmpB(mce, d->tmp);
      assign( 'V', mce, dst, curr );
   }

   /* Outputs: guest state that we write or modify. */
   for (i = 0; i < d->nFxState; i++) {
      tl_assert(d->fxState[i].fx != Ifx_None);
      if (d->fxState[i].fx == Ifx_Read)
         continue;

      /* Enumerate the described state segments */
      for (k = 0; k < 1 + d->fxState[i].nRepeats; k++) {
         gOff = d->fxState[i].offset + k * d->fxState[i].repeatLen;
         gSz  = d->fxState[i].size;

         /* Ignore any sections marked as 'always defined'. */
         if (isAlwaysDefd(mce, gOff, gSz))
            continue;

         /* This state element is written or modified.  So we need to
            consider it.  If larger than 4 bytes, deal with it in
            4-byte chunks. */
         while (True) {
            Int b_offset;
            tl_assert(gSz >= 0);
            if (gSz == 0) break;
            n = gSz <= 4 ? gSz : 4;
            /* Write 'curr' to the state slice gOff .. gOff+n-1 */
            b_offset = MC_(get_otrack_shadow_offset)(gOff, 4);
            if (b_offset != -1) {

               /* If the guard expression evaluates to false we simply Put
                  the value that is already stored in the guest state slot */
               IRAtom *cond, *iffalse;

               cond    = assignNew('B', mce, Ity_I1,
                                   d->guard);
               iffalse = assignNew('B', mce, Ity_I32,
                                   IRExpr_Get(b_offset +
                                              2*mce->layout->total_sizeB,
                                              Ity_I32));
               curr = assignNew('V', mce, Ity_I32,
                                IRExpr_ITE(cond, curr, iffalse));

               stmt( 'B', mce, IRStmt_Put(b_offset
                                          + 2*mce->layout->total_sizeB,
                                          curr ));
            }
            gSz -= n;
            gOff += n;
         }
      }
   }

   /* Outputs: memory that we write or modify.  Same comments about
      endianness as above apply. */
   if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify) {
      toDo   = d->mSize;
      /* chew off 32-bit chunks */
      while (toDo >= 4) {
         gen_store_b( mce, 4, d->mAddr, d->mSize - toDo, curr,
                      d->guard );
         toDo -= 4;
      }
      /* handle possible 16-bit excess */
      while (toDo >= 2) {
         gen_store_b( mce, 2, d->mAddr, d->mSize - toDo, curr,
                      d->guard );
         toDo -= 2;
      }
      /* chew off the remaining 8-bit chunk, if any */
      if (toDo == 1) {
         gen_store_b( mce, 1, d->mAddr, d->mSize - toDo, curr,
                      d->guard );
         toDo -= 1;
      }
      tl_assert(toDo == 0);
   }
}


/* Generate IR for origin shadowing for a general guarded store. */
static void do_origins_Store_guarded ( MCEnv* mce,
                                       IREndness stEnd,
                                       IRExpr* stAddr,
                                       IRExpr* stData,
                                       IRExpr* guard )
{
   Int     dszB;
   IRAtom* dataB;
   /* assert that the B value for the address is already available
      (somewhere), since the call to schemeE will want to see it.
      XXXX how does this actually ensure that?? */
   tl_assert(isIRAtom(stAddr));
   tl_assert(isIRAtom(stData));
   dszB  = sizeofIRType( typeOfIRExpr(mce->sb->tyenv, stData ) );
   dataB = schemeE( mce, stData );
   gen_store_b( mce, dszB, stAddr, 0/*offset*/, dataB, guard );
}


/* Generate IR for origin shadowing for a plain store. */
static void do_origins_Store_plain ( MCEnv* mce,
                                     IREndness stEnd,
                                     IRExpr* stAddr,
                                     IRExpr* stData )
{
   do_origins_Store_guarded ( mce, stEnd, stAddr, stData,
                              NULL/*guard*/ );
}


/* ---- Dealing with LoadG/StoreG (not entirely simple) ---- */

static void do_origins_StoreG ( MCEnv* mce, IRStoreG* sg )
{
   do_origins_Store_guarded( mce, sg->end, sg->addr,
                             sg->data, sg->guard );
}

static void do_origins_LoadG ( MCEnv* mce, IRLoadG* lg )
{
   IRType loadedTy = Ity_INVALID;
   switch (lg->cvt) {
      case ILGop_IdentV128: loadedTy = Ity_V128; break;
      case ILGop_Ident64:   loadedTy = Ity_I64;  break;
      case ILGop_Ident32:   loadedTy = Ity_I32;  break;
      case ILGop_16Uto32:   loadedTy = Ity_I16;  break;
      case ILGop_16Sto32:   loadedTy = Ity_I16;  break;
      case ILGop_8Uto32:    loadedTy = Ity_I8;   break;
      case ILGop_8Sto32:    loadedTy = Ity_I8;   break;
      default: VG_(tool_panic)("schemeS.IRLoadG");
   }
   IRAtom* ori_alt
      = schemeE( mce,lg->alt );
   IRAtom* ori_final
      = expr2ori_Load_guarded_General(mce, loadedTy,
                                      lg->addr, 0/*addr bias*/,
                                      lg->guard, ori_alt );
   /* And finally, bind the origin to the destination temporary. */
   assign( 'B', mce, findShadowTmpB(mce, lg->dst), ori_final );
}


static void schemeS ( MCEnv* mce, IRStmt* st )
{
   tl_assert(MC_(clo_mc_level) == 3);

   switch (st->tag) {

      case Ist_AbiHint:
         /* The value-check instrumenter handles this - by arranging
            to pass the address of the next instruction to
            MC_(helperc_MAKE_STACK_UNINIT).  This is all that needs to
            happen for origin tracking w.r.t. AbiHints.  So there is
            nothing to do here. */
         break;

      case Ist_PutI: {
         IRPutI *puti = st->Ist.PutI.details;
         IRRegArray* descr_b;
         IRAtom      *t1, *t2, *t3, *t4;
         IRRegArray* descr = puti->descr;
         IRType equivIntTy
            = MC_(get_otrack_reg_array_equiv_int_type)(descr);
         /* If this array is unshadowable for whatever reason,
            generate no code. */
         if (equivIntTy == Ity_INVALID)
            break;
         tl_assert(sizeofIRType(equivIntTy) >= 4);
         tl_assert(sizeofIRType(equivIntTy) == sizeofIRType(descr->elemTy));
         descr_b
            = mkIRRegArray( descr->base + 2*mce->layout->total_sizeB,
                            equivIntTy, descr->nElems );
         /* Compute a value to Put - the conjoinment of the origin for
            the data to be Put-ted (obviously) and of the index value
            (not so obviously). */
         t1 = schemeE( mce, puti->data );
         t2 = schemeE( mce, puti->ix );
         t3 = gen_maxU32( mce, t1, t2 );
         t4 = zWidenFrom32( mce, equivIntTy, t3 );
         stmt( 'B', mce, IRStmt_PutI( mkIRPutI(descr_b, puti->ix,
                                               puti->bias, t4) ));
         break;
      }

      case Ist_Dirty:
         do_origins_Dirty( mce, st->Ist.Dirty.details );
         break;

      case Ist_Store:
         do_origins_Store_plain( mce, st->Ist.Store.end,
                                      st->Ist.Store.addr,
                                      st->Ist.Store.data );
         break;

      case Ist_StoreG:
         do_origins_StoreG( mce, st->Ist.StoreG.details );
         break;

      case Ist_LoadG:
         do_origins_LoadG( mce, st->Ist.LoadG.details );
         break;

      case Ist_LLSC: {
         /* In short: treat a load-linked like a normal load followed
            by an assignment of the loaded (shadow) data the result
            temporary.  Treat a store-conditional like a normal store,
            and mark the result temporary as defined. */
         if (st->Ist.LLSC.storedata == NULL) {
            /* Load Linked */
            IRType resTy 
               = typeOfIRTemp(mce->sb->tyenv, st->Ist.LLSC.result);
            IRExpr* vanillaLoad
               = IRExpr_Load(st->Ist.LLSC.end, resTy, st->Ist.LLSC.addr);
            tl_assert(resTy == Ity_I64 || resTy == Ity_I32
                      || resTy == Ity_I16 || resTy == Ity_I8);
            assign( 'B', mce, findShadowTmpB(mce, st->Ist.LLSC.result),
                              schemeE(mce, vanillaLoad));
         } else {
            /* Store conditional */
            do_origins_Store_plain( mce, st->Ist.LLSC.end,
                                    st->Ist.LLSC.addr,
                                    st->Ist.LLSC.storedata );
            /* For the rationale behind this, see comments at the
               place where the V-shadow for .result is constructed, in
               do_shadow_LLSC.  In short, we regard .result as
               always-defined. */
            assign( 'B', mce, findShadowTmpB(mce, st->Ist.LLSC.result),
                              mkU32(0) );
         }
         break;
      }

      case Ist_Put: {
         Int b_offset
            = MC_(get_otrack_shadow_offset)(
                 st->Ist.Put.offset,
                 sizeofIRType(typeOfIRExpr(mce->sb->tyenv, st->Ist.Put.data))
              );
         if (b_offset >= 0) {
            /* FIXME: this isn't an atom! */
            stmt( 'B', mce, IRStmt_Put(b_offset + 2*mce->layout->total_sizeB, 
                                       schemeE( mce, st->Ist.Put.data )) );
         }
         break;
      }

      case Ist_WrTmp:
         assign( 'B', mce, findShadowTmpB(mce, st->Ist.WrTmp.tmp),
                           schemeE(mce, st->Ist.WrTmp.data) );
         break;

      case Ist_MBE:
      case Ist_NoOp:
      case Ist_Exit:
      case Ist_IMark:
         break;

      default:
         VG_(printf)("mc_translate.c: schemeS: unhandled: ");
         ppIRStmt(st); 
         VG_(tool_panic)("memcheck:schemeS");
   }
}


/*------------------------------------------------------------*/
/*--- Post-tree-build final tidying                        ---*/
/*------------------------------------------------------------*/

/* This exploits the observation that Memcheck often produces
   repeated conditional calls of the form

   Dirty G MC_(helperc_value_check0/1/4/8_fail)(UInt otag)

   with the same guard expression G guarding the same helper call.
   The second and subsequent calls are redundant.  This usually
   results from instrumentation of guest code containing multiple
   memory references at different constant offsets from the same base
   register.  After optimisation of the instrumentation, you get a
   test for the definedness of the base register for each memory
   reference, which is kinda pointless.  MC_(final_tidy) therefore
   looks for such repeated calls and removes all but the first. */


/* With some testing on perf/bz2.c, on amd64 and x86, compiled with
   gcc-5.3.1 -O2, it appears that 16 entries in the array are enough to
   get almost all the benefits of this transformation whilst causing
   the slide-back case to just often enough to be verifiably
   correct.  For posterity, the numbers are:

   bz2-32

   1   4,336 (112,212 -> 1,709,473; ratio 15.2)
   2   4,336 (112,194 -> 1,669,895; ratio 14.9)
   3   4,336 (112,194 -> 1,660,713; ratio 14.8)
   4   4,336 (112,194 -> 1,658,555; ratio 14.8)
   5   4,336 (112,194 -> 1,655,447; ratio 14.8)
   6   4,336 (112,194 -> 1,655,101; ratio 14.8)
   7   4,336 (112,194 -> 1,654,858; ratio 14.7)
   8   4,336 (112,194 -> 1,654,810; ratio 14.7)
   10  4,336 (112,194 -> 1,654,621; ratio 14.7)
   12  4,336 (112,194 -> 1,654,678; ratio 14.7)
   16  4,336 (112,194 -> 1,654,494; ratio 14.7)
   32  4,336 (112,194 -> 1,654,602; ratio 14.7)
   inf 4,336 (112,194 -> 1,654,602; ratio 14.7)

   bz2-64

   1   4,113 (107,329 -> 1,822,171; ratio 17.0)
   2   4,113 (107,329 -> 1,806,443; ratio 16.8)
   3   4,113 (107,329 -> 1,803,967; ratio 16.8)
   4   4,113 (107,329 -> 1,802,785; ratio 16.8)
   5   4,113 (107,329 -> 1,802,412; ratio 16.8)
   6   4,113 (107,329 -> 1,802,062; ratio 16.8)
   7   4,113 (107,329 -> 1,801,976; ratio 16.8)
   8   4,113 (107,329 -> 1,801,886; ratio 16.8)
   10  4,113 (107,329 -> 1,801,653; ratio 16.8)
   12  4,113 (107,329 -> 1,801,526; ratio 16.8)
   16  4,113 (107,329 -> 1,801,298; ratio 16.8)
   32  4,113 (107,329 -> 1,800,827; ratio 16.8)
   inf 4,113 (107,329 -> 1,800,827; ratio 16.8)
*/

/* Structs for recording which (helper, guard) pairs we have already
   seen. */

#define N_TIDYING_PAIRS 16

typedef
   struct { void* entry; IRExpr* guard; }
   Pair;

typedef
   struct {
      Pair pairs[N_TIDYING_PAIRS +1/*for bounds checking*/];
      UInt pairsUsed;
   }
   Pairs;


/* Return True if e1 and e2 definitely denote the same value (used to
   compare guards).  Return False if unknown; False is the safe
   answer.  Since guest registers and guest memory do not have the
   SSA property we must return False if any Gets or Loads appear in
   the expression.  This implicitly assumes that e1 and e2 have the
   same IR type, which is always true here -- the type is Ity_I1. */

static Bool sameIRValue ( IRExpr* e1, IRExpr* e2 )
{
   if (e1->tag != e2->tag)
      return False;
   switch (e1->tag) {
      case Iex_Const:
         return eqIRConst( e1->Iex.Const.con, e2->Iex.Const.con );
      case Iex_Binop:
         return e1->Iex.Binop.op == e2->Iex.Binop.op 
                && sameIRValue(e1->Iex.Binop.arg1, e2->Iex.Binop.arg1)
                && sameIRValue(e1->Iex.Binop.arg2, e2->Iex.Binop.arg2);
      case Iex_Unop:
         return e1->Iex.Unop.op == e2->Iex.Unop.op 
                && sameIRValue(e1->Iex.Unop.arg, e2->Iex.Unop.arg);
      case Iex_RdTmp:
         return e1->Iex.RdTmp.tmp == e2->Iex.RdTmp.tmp;
      case Iex_ITE:
         return sameIRValue( e1->Iex.ITE.cond, e2->Iex.ITE.cond )
                && sameIRValue( e1->Iex.ITE.iftrue,  e2->Iex.ITE.iftrue )
                && sameIRValue( e1->Iex.ITE.iffalse, e2->Iex.ITE.iffalse );
      case Iex_Qop:
      case Iex_Triop:
      case Iex_CCall:
         /* be lazy.  Could define equality for these, but they never
            appear to be used. */
         return False;
      case Iex_Get:
      case Iex_GetI:
      case Iex_Load:
         /* be conservative - these may not give the same value each
            time */
         return False;
      case Iex_Binder:
         /* should never see this */
         /* fallthrough */
      default:
         VG_(printf)("mc_translate.c: sameIRValue: unhandled: ");
         ppIRExpr(e1); 
         VG_(tool_panic)("memcheck:sameIRValue");
         return False;
   }
}

/* See if 'pairs' already has an entry for (entry, guard).  Return
   True if so.  If not, add an entry. */

static 
Bool check_or_add ( Pairs* tidyingEnv, IRExpr* guard, void* entry )
{
   UInt i, n = tidyingEnv->pairsUsed;
   tl_assert(n <= N_TIDYING_PAIRS);
   for (i = 0; i < n; i++) {
      if (tidyingEnv->pairs[i].entry == entry
          && sameIRValue(tidyingEnv->pairs[i].guard, guard))
         return True;
   }
   /* (guard, entry) wasn't found in the array.  Add it at the end.
      If the array is already full, slide the entries one slot
      backwards.  This means we will lose to ability to detect
      duplicates from the pair in slot zero, but that happens so
      rarely that it's unlikely to have much effect on overall code
      quality.  Also, this strategy loses the check for the oldest
      tracked exit (memory reference, basically) and so that is (I'd
      guess) least likely to be re-used after this point. */
   tl_assert(i == n);
   if (n == N_TIDYING_PAIRS) {
      for (i = 1; i < N_TIDYING_PAIRS; i++) {
         tidyingEnv->pairs[i-1] = tidyingEnv->pairs[i];
      }
      tidyingEnv->pairs[N_TIDYING_PAIRS-1].entry = entry;
      tidyingEnv->pairs[N_TIDYING_PAIRS-1].guard = guard;
   } else {
      tl_assert(n < N_TIDYING_PAIRS);
      tidyingEnv->pairs[n].entry = entry;
      tidyingEnv->pairs[n].guard = guard;
      n++;
      tidyingEnv->pairsUsed = n;
   }
   return False;
}

static Bool is_helperc_value_checkN_fail ( const HChar* name )
{
   /* This is expensive because it happens a lot.  We are checking to
      see whether |name| is one of the following 8 strings:

         MC_(helperc_value_check8_fail_no_o)
         MC_(helperc_value_check4_fail_no_o)
         MC_(helperc_value_check0_fail_no_o)
         MC_(helperc_value_check1_fail_no_o)
         MC_(helperc_value_check8_fail_w_o)
         MC_(helperc_value_check0_fail_w_o)
         MC_(helperc_value_check1_fail_w_o)
         MC_(helperc_value_check4_fail_w_o)

      To speed it up, check the common prefix just once, rather than
      all 8 times.
   */
   const HChar* prefix = "MC_(helperc_value_check";

   HChar n, p;
   while (True) {
      n = *name;
      p = *prefix;
      if (p == 0) break; /* ran off the end of the prefix */
      /* We still have some prefix to use */
      if (n == 0) return False; /* have prefix, but name ran out */
      if (n != p) return False; /* have both pfx and name, but no match */
      name++;
      prefix++;
   }

   /* Check the part after the prefix. */
   tl_assert(*prefix == 0 && *name != 0);
   return    0==VG_(strcmp)(name, "8_fail_no_o)")
          || 0==VG_(strcmp)(name, "4_fail_no_o)")
          || 0==VG_(strcmp)(name, "0_fail_no_o)")
          || 0==VG_(strcmp)(name, "1_fail_no_o)")
          || 0==VG_(strcmp)(name, "8_fail_w_o)")
          || 0==VG_(strcmp)(name, "4_fail_w_o)")
          || 0==VG_(strcmp)(name, "0_fail_w_o)")
          || 0==VG_(strcmp)(name, "1_fail_w_o)");
}

IRSB* MC_(final_tidy) ( IRSB* sb_in )
{
   Int       i;
   IRStmt*   st;
   IRDirty*  di;
   IRExpr*   guard;
   IRCallee* cee;
   Bool      alreadyPresent;
   Pairs     pairs;

   pairs.pairsUsed = 0;

   pairs.pairs[N_TIDYING_PAIRS].entry = (void*)0x123;
   pairs.pairs[N_TIDYING_PAIRS].guard = (IRExpr*)0x456;

   /* Scan forwards through the statements.  Each time a call to one
      of the relevant helpers is seen, check if we have made a
      previous call to the same helper using the same guard
      expression, and if so, delete the call. */
   for (i = 0; i < sb_in->stmts_used; i++) {
      st = sb_in->stmts[i];
      tl_assert(st);
      if (st->tag != Ist_Dirty)
         continue;
      di = st->Ist.Dirty.details;
      guard = di->guard;
      tl_assert(guard);
      if (0) { ppIRExpr(guard); VG_(printf)("\n"); }
      cee = di->cee;
      if (!is_helperc_value_checkN_fail( cee->name )) 
         continue;
       /* Ok, we have a call to helperc_value_check0/1/4/8_fail with
          guard 'guard'.  Check if we have already seen a call to this
          function with the same guard.  If so, delete it.  If not,
          add it to the set of calls we do know about. */
      alreadyPresent = check_or_add( &pairs, guard, cee->addr );
      if (alreadyPresent) {
         sb_in->stmts[i] = IRStmt_NoOp();
         if (0) VG_(printf)("XX\n");
      }
   }

   tl_assert(pairs.pairs[N_TIDYING_PAIRS].entry == (void*)0x123);
   tl_assert(pairs.pairs[N_TIDYING_PAIRS].guard == (IRExpr*)0x456);

   return sb_in;
}

#undef N_TIDYING_PAIRS


/*------------------------------------------------------------*/
/*--- Startup assertion checking                           ---*/
/*------------------------------------------------------------*/

void MC_(do_instrumentation_startup_checks)( void )
{
   /* Make a best-effort check to see that is_helperc_value_checkN_fail
      is working as we expect. */

#  define CHECK(_expected, _string) \
      tl_assert((_expected) == is_helperc_value_checkN_fail(_string))

   /* It should identify these 8, and no others, as targets. */
   CHECK(True, "MC_(helperc_value_check8_fail_no_o)");
   CHECK(True, "MC_(helperc_value_check4_fail_no_o)");
   CHECK(True, "MC_(helperc_value_check0_fail_no_o)");
   CHECK(True, "MC_(helperc_value_check1_fail_no_o)");
   CHECK(True, "MC_(helperc_value_check8_fail_w_o)");
   CHECK(True, "MC_(helperc_value_check0_fail_w_o)");
   CHECK(True, "MC_(helperc_value_check1_fail_w_o)");
   CHECK(True, "MC_(helperc_value_check4_fail_w_o)");

   /* Ad-hoc selection of other strings gathered via a quick test. */
   CHECK(False, "amd64g_dirtyhelper_CPUID_avx2");
   CHECK(False, "amd64g_dirtyhelper_RDTSC");
   CHECK(False, "MC_(helperc_b_load1)");
   CHECK(False, "MC_(helperc_b_load2)");
   CHECK(False, "MC_(helperc_b_load4)");
   CHECK(False, "MC_(helperc_b_load8)");
   CHECK(False, "MC_(helperc_b_load16)");
   CHECK(False, "MC_(helperc_b_load32)");
   CHECK(False, "MC_(helperc_b_store1)");
   CHECK(False, "MC_(helperc_b_store2)");
   CHECK(False, "MC_(helperc_b_store4)");
   CHECK(False, "MC_(helperc_b_store8)");
   CHECK(False, "MC_(helperc_b_store16)");
   CHECK(False, "MC_(helperc_b_store32)");
   CHECK(False, "MC_(helperc_LOADV8)");
   CHECK(False, "MC_(helperc_LOADV16le)");
   CHECK(False, "MC_(helperc_LOADV32le)");
   CHECK(False, "MC_(helperc_LOADV64le)");
   CHECK(False, "MC_(helperc_LOADV128le)");
   CHECK(False, "MC_(helperc_LOADV256le)");
   CHECK(False, "MC_(helperc_STOREV16le)");
   CHECK(False, "MC_(helperc_STOREV32le)");
   CHECK(False, "MC_(helperc_STOREV64le)");
   CHECK(False, "MC_(helperc_STOREV8)");
   CHECK(False, "track_die_mem_stack_8");
   CHECK(False, "track_new_mem_stack_8_w_ECU");
   CHECK(False, "MC_(helperc_MAKE_STACK_UNINIT_w_o)");
   CHECK(False, "VG_(unknown_SP_update_w_ECU)");

#  undef CHECK
}


/*------------------------------------------------------------*/
/*--- Memcheck main                                        ---*/
/*------------------------------------------------------------*/

static Bool isBogusAtom ( IRAtom* at )
{
   if (at->tag == Iex_RdTmp)
      return False;
   tl_assert(at->tag == Iex_Const);

   ULong n = 0;
   IRConst* con = at->Iex.Const.con;
   switch (con->tag) {
      case Ico_U1:   return False;
      case Ico_U8:   n = (ULong)con->Ico.U8; break;
      case Ico_U16:  n = (ULong)con->Ico.U16; break;
      case Ico_U32:  n = (ULong)con->Ico.U32; break;
      case Ico_U64:  n = (ULong)con->Ico.U64; break;
      case Ico_F32:  return False;
      case Ico_F64:  return False;
      case Ico_F32i: return False;
      case Ico_F64i: return False;
      case Ico_V128: return False;
      case Ico_V256: return False;
      default: ppIRExpr(at); tl_assert(0);
   }
   /* VG_(printf)("%llx\n", n); */
   /* Shortcuts */
   if (LIKELY(n <= 0x0000000000001000ULL)) return False;
   if (LIKELY(n >= 0xFFFFFFFFFFFFF000ULL)) return False;
   /* The list of bogus atoms is: */
   return (/*32*/    n == 0xFEFEFEFFULL
           /*32*/ || n == 0x80808080ULL
           /*32*/ || n == 0x7F7F7F7FULL
           /*32*/ || n == 0x7EFEFEFFULL
           /*32*/ || n == 0x81010100ULL
           /*64*/ || n == 0xFFFFFFFFFEFEFEFFULL
           /*64*/ || n == 0xFEFEFEFEFEFEFEFFULL
           /*64*/ || n == 0x0000000000008080ULL
           /*64*/ || n == 0x8080808080808080ULL
           /*64*/ || n == 0x0101010101010101ULL
          );
}


/* Does 'st' mention any of the literals identified/listed in
   isBogusAtom()? */
static inline Bool containsBogusLiterals ( /*FLAT*/ IRStmt* st )
{
   Int      i;
   IRExpr*  e;
   IRDirty* d;
   IRCAS*   cas;
   switch (st->tag) {
      case Ist_WrTmp:
         e = st->Ist.WrTmp.data;
         switch (e->tag) {
            case Iex_Get:
            case Iex_RdTmp:
               return False;
            case Iex_Const:
               return isBogusAtom(e);
            case Iex_Unop: 
               return isBogusAtom(e->Iex.Unop.arg)
                      || e->Iex.Unop.op == Iop_GetMSBs8x16;
            case Iex_GetI:
               return isBogusAtom(e->Iex.GetI.ix);
            case Iex_Binop: 
               return isBogusAtom(e->Iex.Binop.arg1)
                      || isBogusAtom(e->Iex.Binop.arg2);
            case Iex_Triop: 
               return isBogusAtom(e->Iex.Triop.details->arg1)
                      || isBogusAtom(e->Iex.Triop.details->arg2)
                      || isBogusAtom(e->Iex.Triop.details->arg3);
            case Iex_Qop: 
               return isBogusAtom(e->Iex.Qop.details->arg1)
                      || isBogusAtom(e->Iex.Qop.details->arg2)
                      || isBogusAtom(e->Iex.Qop.details->arg3)
                      || isBogusAtom(e->Iex.Qop.details->arg4);
            case Iex_ITE:
               return isBogusAtom(e->Iex.ITE.cond)
                      || isBogusAtom(e->Iex.ITE.iftrue)
                      || isBogusAtom(e->Iex.ITE.iffalse);
            case Iex_Load: 
               return isBogusAtom(e->Iex.Load.addr);
            case Iex_CCall:
               for (i = 0; e->Iex.CCall.args[i]; i++)
                  if (isBogusAtom(e->Iex.CCall.args[i]))
                     return True;
               return False;
            default: 
               goto unhandled;
         }
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         for (i = 0; d->args[i]; i++) {
            IRAtom* atom = d->args[i];
            if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(atom))) {
               if (isBogusAtom(atom))
                  return True;
            }
         }
         if (isBogusAtom(d->guard))
            return True;
         if (d->mAddr && isBogusAtom(d->mAddr))
            return True;
         return False;
      case Ist_Put:
         return isBogusAtom(st->Ist.Put.data);
      case Ist_PutI:
         return isBogusAtom(st->Ist.PutI.details->ix) 
                || isBogusAtom(st->Ist.PutI.details->data);
      case Ist_Store:
         return isBogusAtom(st->Ist.Store.addr) 
                || isBogusAtom(st->Ist.Store.data);
      case Ist_StoreG: {
         IRStoreG* sg = st->Ist.StoreG.details;
         return isBogusAtom(sg->addr) || isBogusAtom(sg->data)
                || isBogusAtom(sg->guard);
      }
      case Ist_LoadG: {
         IRLoadG* lg = st->Ist.LoadG.details;
         return isBogusAtom(lg->addr) || isBogusAtom(lg->alt)
                || isBogusAtom(lg->guard);
      }
      case Ist_Exit:
         return isBogusAtom(st->Ist.Exit.guard);
      case Ist_AbiHint:
         return isBogusAtom(st->Ist.AbiHint.base)
                || isBogusAtom(st->Ist.AbiHint.nia);
      case Ist_NoOp:
      case Ist_IMark:
      case Ist_MBE:
         return False;
      case Ist_CAS:
         cas = st->Ist.CAS.details;
         return isBogusAtom(cas->addr)
                || (cas->expdHi ? isBogusAtom(cas->expdHi) : False)
                || isBogusAtom(cas->expdLo)
                || (cas->dataHi ? isBogusAtom(cas->dataHi) : False)
                || isBogusAtom(cas->dataLo);
      case Ist_LLSC:
         return isBogusAtom(st->Ist.LLSC.addr)
                || (st->Ist.LLSC.storedata
                       ? isBogusAtom(st->Ist.LLSC.storedata)
                       : False);
      default: 
      unhandled:
         ppIRStmt(st);
         VG_(tool_panic)("hasBogusLiterals");
   }
}


/* This is the pre-instrumentation analysis.  It does a backwards pass over
   the stmts in |sb_in| to determine a HowUsed value for each tmp defined in
   the block.

   Unrelatedly, it also checks all literals in the block with |isBogusAtom|,
   as a positive result from that is a strong indication that we need to
   expensively instrument add/sub in the block.  We do both analyses in one
   pass, even though they are independent, so as to avoid the overhead of
   having to traverse the whole block twice.

   The usage pass proceeds as follows.  Let max= be the max operation in the
   HowUsed lattice, hence

     X max= Y   means   X = max(X, Y)

   then

     for t in original tmps . useEnv[t] = HuUnU

     for t used in the block's . next field
        useEnv[t] max= HuPCa  // because jmp targets are PCast-tested

     for st iterating *backwards* in the block

        match st

           case "t1 = load(t2)"          // case 1
              useEnv[t2] max= HuPCa

           case "t1 = add(t2, t3)"       // case 2
              useEnv[t2] max= useEnv[t1]
              useEnv[t3] max= useEnv[t1]

           other
              for t in st.usedTmps       // case 3
                 useEnv[t] max= HuOth
                 // same as useEnv[t] = HuOth

   The general idea is that we accumulate, in useEnv[], information about
   how each tmp is used.  That can be updated as we work further back
   through the block and find more uses of it, but its HowUsed value can
   only ascend the lattice, not descend.

   Initially we mark all tmps as unused.  In case (1), if a tmp is seen to
   be used as a memory address, then its use is at least HuPCa.  The point
   is that for a memory address we will add instrumentation to check if any
   bit of the address is undefined, which means that we won't need expensive
   V-bit propagation through an add expression that computed the address --
   cheap add instrumentation will be equivalent.

   Note in case (1) that if we have previously seen a non-memory-address use
   of the tmp, then its use will already be HuOth and will be unchanged by
   the max= operation.  And if it turns out that the source of the tmp was
   an add, then we'll have to expensively instrument the add, because we
   can't prove that, for the previous non-memory-address use of the tmp,
   cheap and expensive instrumentation will be equivalent.

   In case 2, we propagate the usage-mode of the result of an add back
   through to its operands.  Again, we use max= so as to take account of the
   fact that t2 or t3 might later in the block (viz, earlier in the
   iteration) have been used in a way that requires expensive add
   instrumentation.

   In case 3, we deal with all other tmp uses.  We assume that we'll need a
   result that is as accurate as possible, so we max= HuOth into its use
   mode.  Since HuOth is the top of the lattice, that's equivalent to just
   setting its use to HuOth.

   The net result of all this is that:

     tmps that are used either
       - only as a memory address, or
       - only as part of a tree of adds that computes a memory address,
         and has no other use
     are marked as HuPCa, and so we can instrument their generating Add
     nodes cheaply, which is the whole point of this analysis

     tmps that are used any other way at all are marked as HuOth

     tmps that are unused are marked as HuUnU.  We don't expect to see any
     since we expect that the incoming IR has had all dead assignments
     removed by previous optimisation passes.  Nevertheless the analysis is
     correct even in the presence of dead tmps.

   A final comment on dead tmps.  In case 1 and case 2, we could actually
   conditionalise the updates thusly:

     if (useEnv[t1] > HuUnU) { useEnv[t2] max= HuPCa }  // case 1

     if (useEnv[t1] > HuUnU) { useEnv[t2] max= useEnv[t1] }  // case 2
     if (useEnv[t1] > HuUnU) { useEnv[t3] max= useEnv[t1] }  // case 2

   In other words, if the assigned-to tmp |t1| is never used, then there's
   no point in propagating any use through to its operands.  That won't
   change the final HuPCa-vs-HuOth results, which is what we care about.
   Given that we expect to get dead-code-free inputs, there's no point in
   adding this extra refinement.
*/

/* Helper for |preInstrumentationAnalysis|. */
static inline void noteTmpUsesIn ( /*MOD*/HowUsed* useEnv,
                                   UInt tyenvUsed,
                                   HowUsed newUse, IRAtom* at )
{
   /* For the atom |at|, declare that for any tmp |t| in |at|, we will have
      seen a use of |newUse|.  So, merge that info into |t|'s accumulated
      use info. */
   switch (at->tag) {
      case Iex_GSPTR:
      case Iex_VECRET:
      case Iex_Const:
         return;
      case Iex_RdTmp: {
         IRTemp t = at->Iex.RdTmp.tmp;
         tl_assert(t < tyenvUsed); // "is an original tmp"
         // The "max" operation in the lattice
         if (newUse > useEnv[t]) useEnv[t] = newUse;
         return;
      }
      default:
         // We should never get here -- it implies non-flat IR
         ppIRExpr(at);
         VG_(tool_panic)("noteTmpUsesIn");
   }
   /*NOTREACHED*/
   tl_assert(0);
}


static void preInstrumentationAnalysis ( /*OUT*/HowUsed** useEnvP,
                                         /*OUT*/Bool* hasBogusLiteralsP,
                                         const IRSB* sb_in )
{
   const UInt nOrigTmps = (UInt)sb_in->tyenv->types_used;

   // We've seen no bogus literals so far.
   Bool bogus = False;

   // This is calloc'd, so implicitly all entries are initialised to HuUnU.
   HowUsed* useEnv = VG_(calloc)("mc.preInstrumentationAnalysis.1",
                                 nOrigTmps, sizeof(HowUsed));

   // Firstly, roll in contributions from the final dst address.
   bogus = isBogusAtom(sb_in->next);
   noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, sb_in->next);

   // Now work backwards through the stmts.
   for (Int i = sb_in->stmts_used-1; i >= 0; i--) {
      IRStmt* st = sb_in->stmts[i];

      // Deal with literals.
      if (LIKELY(!bogus)) {
         bogus = containsBogusLiterals(st);
      }

      // Deal with tmp uses.
      switch (st->tag) {
         case Ist_WrTmp: {
            IRTemp  dst = st->Ist.WrTmp.tmp;
            IRExpr* rhs = st->Ist.WrTmp.data;
            // This is the one place where we have to consider all possible
            // tags for |rhs|, and can't just assume it is a tmp or a const.
            switch (rhs->tag) {
               case Iex_RdTmp:
                  // just propagate demand for |dst| into this tmp use.
                  noteTmpUsesIn(useEnv, nOrigTmps, useEnv[dst], rhs);
                  break;
               case Iex_Unop:
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, rhs->Iex.Unop.arg);
                  break;
               case Iex_Binop:
                  if (rhs->Iex.Binop.op == Iop_Add64
                      || rhs->Iex.Binop.op == Iop_Add32) {
                     // propagate demand for |dst| through to the operands.
                     noteTmpUsesIn(useEnv, nOrigTmps,
                                   useEnv[dst], rhs->Iex.Binop.arg1);
                     noteTmpUsesIn(useEnv, nOrigTmps,
                                   useEnv[dst], rhs->Iex.Binop.arg2);
                  } else {
                     // just say that the operands are used in some unknown way.
                     noteTmpUsesIn(useEnv, nOrigTmps,
                                   HuOth, rhs->Iex.Binop.arg1);
                     noteTmpUsesIn(useEnv, nOrigTmps,
                                   HuOth, rhs->Iex.Binop.arg2);
                  }
                  break;
               case Iex_Triop: {
                  // All operands are used in some unknown way.
                  IRTriop* tri = rhs->Iex.Triop.details;
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, tri->arg1);
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, tri->arg2);
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, tri->arg3);
                  break;
               }
               case Iex_Qop: {
                  // All operands are used in some unknown way.
                  IRQop* qop = rhs->Iex.Qop.details;
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, qop->arg1);
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, qop->arg2);
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, qop->arg3);
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, qop->arg4);
                  break;
               }
               case Iex_Load:
                  // The address will be checked (== PCasted).
                  noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, rhs->Iex.Load.addr);
                  break;
               case Iex_ITE:
                  // The condition is PCasted, the then- and else-values
                  // aren't.
                  noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, rhs->Iex.ITE.cond);
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, rhs->Iex.ITE.iftrue);
                  noteTmpUsesIn(useEnv, nOrigTmps, HuOth, rhs->Iex.ITE.iffalse);
                  break;
               case Iex_CCall:
                  // The args are used in unknown ways.
                  for (IRExpr** args = rhs->Iex.CCall.args; *args; args++) {
                     noteTmpUsesIn(useEnv, nOrigTmps, HuOth, *args);
                  }
                  break;
               case Iex_GetI: {
                  // The index will be checked/PCasted (see do_shadow_GETI)
                  noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, rhs->Iex.GetI.ix);
                  break;
               }
               case Iex_Const:
               case Iex_Get:
                  break;
               default:
                  ppIRExpr(rhs);
                  VG_(tool_panic)("preInstrumentationAnalysis:"
                                  " unhandled IRExpr");
            }
            break;
         }
         case Ist_Store:
            // The address will be checked (== PCasted).  The data will be
            // used in some unknown way.
            noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, st->Ist.Store.addr);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, st->Ist.Store.data);
            break;
         case Ist_Exit:
            // The guard will be checked (== PCasted)
            noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, st->Ist.Exit.guard);
            break;
         case Ist_Put:
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, st->Ist.Put.data);
            break;
         case Ist_PutI: {
            IRPutI* putI = st->Ist.PutI.details;
            // The index will be checked/PCasted (see do_shadow_PUTI).  The
            // data will be used in an unknown way.
            noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, putI->ix);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, putI->data);
            break;
         }
         case Ist_Dirty: {
            IRDirty* d = st->Ist.Dirty.details;
            // The guard will be checked (== PCasted)
            noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, d->guard);
            // The args will be used in unknown ways.
            for (IRExpr** args = d->args; *args; args++) {
               noteTmpUsesIn(useEnv, nOrigTmps, HuOth, *args);
            }
            break;
         }
         case Ist_CAS: {
            IRCAS* cas = st->Ist.CAS.details;
            // Address will be pcasted, everything else used as unknown
            noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, cas->addr);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, cas->expdLo);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, cas->dataLo);
            if (cas->expdHi)
               noteTmpUsesIn(useEnv, nOrigTmps, HuOth, cas->expdHi);
            if (cas->dataHi)
               noteTmpUsesIn(useEnv, nOrigTmps, HuOth, cas->dataHi);
            break;
         }
         case Ist_AbiHint:
            // Both exprs are used in unknown ways.  TODO: can we safely
            // just ignore AbiHints?
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, st->Ist.AbiHint.base);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, st->Ist.AbiHint.nia);
            break;
         case Ist_StoreG: {
            // We might be able to do better, and use HuPCa for the addr.
            // It's not immediately obvious that we can, because the address
            // is regarded as "used" only when the guard is true.
            IRStoreG* sg = st->Ist.StoreG.details;
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, sg->addr);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, sg->data);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, sg->guard);
            break;
         }
         case Ist_LoadG: {
            // Per similar comments to Ist_StoreG .. not sure whether this
            // is really optimal.
            IRLoadG* lg = st->Ist.LoadG.details;
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, lg->addr);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, lg->alt);
            noteTmpUsesIn(useEnv, nOrigTmps, HuOth, lg->guard);
            break;
         }
         case Ist_LLSC: {
            noteTmpUsesIn(useEnv, nOrigTmps, HuPCa, st->Ist.LLSC.addr);
            if (st->Ist.LLSC.storedata)
               noteTmpUsesIn(useEnv, nOrigTmps, HuOth, st->Ist.LLSC.storedata);
            break;
         }
         case Ist_MBE:
         case Ist_IMark:
         case Ist_NoOp:
            break;
         default: {
            ppIRStmt(st);
            VG_(tool_panic)("preInstrumentationAnalysis: unhandled IRStmt");
         }
      }
   } // Now work backwards through the stmts.

   // Return the computed use env and the bogus-atom flag.
   tl_assert(*useEnvP == NULL);
   *useEnvP = useEnv;

   tl_assert(*hasBogusLiteralsP == False);
   *hasBogusLiteralsP = bogus;
}


IRSB* MC_(instrument) ( VgCallbackClosure* closure,
                        IRSB* sb_in, 
                        const VexGuestLayout* layout, 
                        const VexGuestExtents* vge,
                        const VexArchInfo* archinfo_host,
                        IRType gWordTy, IRType hWordTy )
{
   Bool    verboze = 0||False;
   Int     i, j, first_stmt;
   IRStmt* st;
   MCEnv   mce;
   IRSB*   sb_out;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   /* Check we're not completely nuts */
   tl_assert(sizeof(UWord)  == sizeof(void*));
   tl_assert(sizeof(Word)   == sizeof(void*));
   tl_assert(sizeof(Addr)   == sizeof(void*));
   tl_assert(sizeof(ULong)  == 8);
   tl_assert(sizeof(Long)   == 8);
   tl_assert(sizeof(UInt)   == 4);
   tl_assert(sizeof(Int)    == 4);

   tl_assert(MC_(clo_mc_level) >= 1 && MC_(clo_mc_level) <= 3);

   /* Set up SB */
   sb_out = deepCopyIRSBExceptStmts(sb_in);

   /* Set up the running environment.  Both .sb and .tmpMap are
      modified as we go along.  Note that tmps are added to both
      .sb->tyenv and .tmpMap together, so the valid index-set for
      those two arrays should always be identical. */
   VG_(memset)(&mce, 0, sizeof(mce));
   mce.sb             = sb_out;
   mce.trace          = verboze;
   mce.layout         = layout;
   mce.hWordTy        = hWordTy;
   mce.tmpHowUsed     = NULL;

   /* BEGIN decide on expense levels for instrumentation. */

   /* Initially, select the cheap version of everything for which we have an
      option. */
   DetailLevelByOp__set_all( &mce.dlbo, DLcheap );

   /* Take account of the --expensive-definedness-checks= flag. */
   if (MC_(clo_expensive_definedness_checks) == EdcNO) {
      /* We just selected 'cheap for everything', so we don't need to do
         anything here.  mce.tmpHowUsed remains NULL. */
   }
   else if (MC_(clo_expensive_definedness_checks) == EdcYES) {
      /* Select 'expensive for everything'.  mce.tmpHowUsed remains NULL. */
      DetailLevelByOp__set_all( &mce.dlbo, DLexpensive );
   }
   else {
      tl_assert(MC_(clo_expensive_definedness_checks) == EdcAUTO);
      /* We'll make our own selection, based on known per-target constraints
         and also on analysis of the block to be instrumented.  First, set
         up default values for detail levels.

         On x86 and amd64, we'll routinely encounter code optimised by LLVM
         5 and above.  Enable accurate interpretation of the following.
         LLVM uses adds for some bitfield inserts, and we get a lot of false
         errors if the cheap interpretation is used, alas.  Could solve this
         much better if we knew which of such adds came from x86/amd64 LEA
         instructions, since these are the only ones really needing the
         expensive interpretation, but that would require some way to tag
         them in the _toIR.c front ends, which is a lot of faffing around.
         So for now we use preInstrumentationAnalysis() to detect adds which
         are used only to construct memory addresses, which is an
         approximation to the above, and is self-contained.*/
#     if defined(VGA_x86)
      mce.dlbo.dl_Add32           = DLauto;
      mce.dlbo.dl_CmpEQ32_CmpNE32 = DLexpensive;
#     elif defined(VGA_amd64)
      mce.dlbo.dl_Add64           = DLauto;
      mce.dlbo.dl_CmpEQ32_CmpNE32 = DLexpensive;
#     elif defined(VGA_ppc64le)
      // Needed by (at least) set_AV_CR6() in the front end.
      mce.dlbo.dl_CmpEQ64_CmpNE64 = DLexpensive;
#     endif

      /* preInstrumentationAnalysis() will allocate &mce.tmpHowUsed and then
         fill it in. */
      Bool hasBogusLiterals = False;
      preInstrumentationAnalysis( &mce.tmpHowUsed, &hasBogusLiterals, sb_in );

      if (hasBogusLiterals) {
         /* This happens very rarely.  In this case just select expensive
            for everything, and throw away the tmp-use analysis results. */
         DetailLevelByOp__set_all( &mce.dlbo, DLexpensive );
         VG_(free)( mce.tmpHowUsed );
         mce.tmpHowUsed = NULL;
      } else {
         /* Nothing.  mce.tmpHowUsed contains tmp-use analysis results,
            which will be used for some subset of Iop_{Add,Sub}{32,64},
            based on which ones are set to DLauto for this target. */
      }
   }

   DetailLevelByOp__check_sanity( &mce.dlbo );

   if (0) {
      // Debug printing: which tmps have been identified as PCast-only use
      if (mce.tmpHowUsed) {
         VG_(printf)("Cheapies: ");
         for (UInt q = 0; q < sb_in->tyenv->types_used; q++) {
            if (mce.tmpHowUsed[q] == HuPCa) {
               VG_(printf)("t%u ", q);
            }
         }
         VG_(printf)("\n");
      }

      // Debug printing: number of ops by detail level
      UChar nCheap     = DetailLevelByOp__count( &mce.dlbo, DLcheap     );
      UChar nAuto      = DetailLevelByOp__count( &mce.dlbo, DLauto      );
      UChar nExpensive = DetailLevelByOp__count( &mce.dlbo, DLexpensive );
      tl_assert(nCheap + nAuto + nExpensive == 8);

      VG_(printf)("%u,%u,%u ", nCheap, nAuto, nExpensive);
   }
   /* END decide on expense levels for instrumentation. */

   /* Initialise the running the tmp environment. */

   mce.tmpMap = VG_(newXA)( VG_(malloc), "mc.MC_(instrument).1", VG_(free),
                            sizeof(TempMapEnt));
   VG_(hintSizeXA) (mce.tmpMap, sb_in->tyenv->types_used);
   for (i = 0; i < sb_in->tyenv->types_used; i++) {
      TempMapEnt ent;
      ent.kind    = Orig;
      ent.shadowV = IRTemp_INVALID;
      ent.shadowB = IRTemp_INVALID;
      VG_(addToXA)( mce.tmpMap, &ent );
   }
   tl_assert( VG_(sizeXA)( mce.tmpMap ) == sb_in->tyenv->types_used );

   /* Finally, begin instrumentation. */
   /* Copy verbatim any IR preamble preceding the first IMark */

   tl_assert(mce.sb == sb_out);
   tl_assert(mce.sb != sb_in);

   i = 0;
   while (i < sb_in->stmts_used && sb_in->stmts[i]->tag != Ist_IMark) {

      st = sb_in->stmts[i];
      tl_assert(st);
      tl_assert(isFlatIRStmt(st));

      stmt( 'C', &mce, sb_in->stmts[i] );
      i++;
   }

   /* Nasty problem.  IR optimisation of the pre-instrumented IR may
      cause the IR following the preamble to contain references to IR
      temporaries defined in the preamble.  Because the preamble isn't
      instrumented, these temporaries don't have any shadows.
      Nevertheless uses of them following the preamble will cause
      memcheck to generate references to their shadows.  End effect is
      to cause IR sanity check failures, due to references to
      non-existent shadows.  This is only evident for the complex
      preambles used for function wrapping on TOC-afflicted platforms
      (ppc64-linux).

      The following loop therefore scans the preamble looking for
      assignments to temporaries.  For each one found it creates an
      assignment to the corresponding (V) shadow temp, marking it as
      'defined'.  This is the same resulting IR as if the main
      instrumentation loop before had been applied to the statement
      'tmp = CONSTANT'.

      Similarly, if origin tracking is enabled, we must generate an
      assignment for the corresponding origin (B) shadow, claiming
      no-origin, as appropriate for a defined value.
   */
   for (j = 0; j < i; j++) {
      if (sb_in->stmts[j]->tag == Ist_WrTmp) {
         /* findShadowTmpV checks its arg is an original tmp;
            no need to assert that here. */
         IRTemp tmp_o = sb_in->stmts[j]->Ist.WrTmp.tmp;
         IRTemp tmp_v = findShadowTmpV(&mce, tmp_o);
         IRType ty_v  = typeOfIRTemp(sb_out->tyenv, tmp_v);
         assign( 'V', &mce, tmp_v, definedOfType( ty_v ) );
         if (MC_(clo_mc_level) == 3) {
            IRTemp tmp_b = findShadowTmpB(&mce, tmp_o);
            tl_assert(typeOfIRTemp(sb_out->tyenv, tmp_b) == Ity_I32);
            assign( 'B', &mce, tmp_b, mkU32(0)/* UNKNOWN ORIGIN */);
         }
         if (0) {
            VG_(printf)("create shadow tmp(s) for preamble tmp [%d] ty ", j);
            ppIRType( ty_v );
            VG_(printf)("\n");
         }
      }
   }

   /* Iterate over the remaining stmts to generate instrumentation. */

   tl_assert(sb_in->stmts_used > 0);
   tl_assert(i >= 0);
   tl_assert(i < sb_in->stmts_used);
   tl_assert(sb_in->stmts[i]->tag == Ist_IMark);

   for (/* use current i*/; i < sb_in->stmts_used; i++) {

      st = sb_in->stmts[i];
      first_stmt = sb_out->stmts_used;

      if (verboze) {
         VG_(printf)("\n");
         ppIRStmt(st);
         VG_(printf)("\n");
      }

      if (MC_(clo_mc_level) == 3) {
         /* See comments on case Ist_CAS below. */
         if (st->tag != Ist_CAS) 
            schemeS( &mce, st );
      }

      /* Generate instrumentation code for each stmt ... */

      switch (st->tag) {

         case Ist_WrTmp: {
            IRTemp dst = st->Ist.WrTmp.tmp;
            tl_assert(dst < (UInt)sb_in->tyenv->types_used);
            HowUsed hu = mce.tmpHowUsed ? mce.tmpHowUsed[dst]
                                        : HuOth/*we don't know, so play safe*/;
            assign( 'V', &mce, findShadowTmpV(&mce, st->Ist.WrTmp.tmp), 
                               expr2vbits( &mce, st->Ist.WrTmp.data, hu ));
            break;
         }

         case Ist_Put:
            do_shadow_PUT( &mce, 
                           st->Ist.Put.offset,
                           st->Ist.Put.data,
                           NULL /* shadow atom */, NULL /* guard */ );
            break;

         case Ist_PutI:
            do_shadow_PUTI( &mce, st->Ist.PutI.details);
            break;

         case Ist_Store:
            do_shadow_Store( &mce, st->Ist.Store.end,
                                   st->Ist.Store.addr, 0/* addr bias */,
                                   st->Ist.Store.data,
                                   NULL /* shadow data */,
                                   NULL/*guard*/ );
            break;

         case Ist_StoreG:
            do_shadow_StoreG( &mce, st->Ist.StoreG.details );
            break;

         case Ist_LoadG:
            do_shadow_LoadG( &mce, st->Ist.LoadG.details );
            break;

         case Ist_Exit:
            complainIfUndefined( &mce, st->Ist.Exit.guard, NULL );
            break;

         case Ist_IMark:
            break;

         case Ist_NoOp:
         case Ist_MBE:
            break;

         case Ist_Dirty:
            do_shadow_Dirty( &mce, st->Ist.Dirty.details );
            break;

         case Ist_AbiHint:
            do_AbiHint( &mce, st->Ist.AbiHint.base,
                              st->Ist.AbiHint.len,
                              st->Ist.AbiHint.nia );
            break;

         case Ist_CAS:
            do_shadow_CAS( &mce, st->Ist.CAS.details );
            /* Note, do_shadow_CAS copies the CAS itself to the output
               block, because it needs to add instrumentation both
               before and after it.  Hence skip the copy below.  Also
               skip the origin-tracking stuff (call to schemeS) above,
               since that's all tangled up with it too; do_shadow_CAS
               does it all. */
            break;

         case Ist_LLSC:
            do_shadow_LLSC( &mce,
                            st->Ist.LLSC.end,
                            st->Ist.LLSC.result,
                            st->Ist.LLSC.addr,
                            st->Ist.LLSC.storedata );
            break;

         default:
            VG_(printf)("\n");
            ppIRStmt(st);
            VG_(printf)("\n");
            VG_(tool_panic)("memcheck: unhandled IRStmt");

      } /* switch (st->tag) */

      if (0 && verboze) {
         for (j = first_stmt; j < sb_out->stmts_used; j++) {
            VG_(printf)("   ");
            ppIRStmt(sb_out->stmts[j]);
            VG_(printf)("\n");
         }
         VG_(printf)("\n");
      }

      /* ... and finally copy the stmt itself to the output.  Except,
         skip the copy of IRCASs; see comments on case Ist_CAS
         above. */
      if (st->tag != Ist_CAS)
         stmt('C', &mce, st);
   }

   /* Now we need to complain if the jump target is undefined. */
   first_stmt = sb_out->stmts_used;

   if (verboze) {
      VG_(printf)("sb_in->next = ");
      ppIRExpr(sb_in->next);
      VG_(printf)("\n\n");
   }

   complainIfUndefined( &mce, sb_in->next, NULL );

   if (0 && verboze) {
      for (j = first_stmt; j < sb_out->stmts_used; j++) {
         VG_(printf)("   ");
         ppIRStmt(sb_out->stmts[j]);
         VG_(printf)("\n");
      }
      VG_(printf)("\n");
   }

   /* If this fails, there's been some serious snafu with tmp management,
      that should be investigated. */
   tl_assert( VG_(sizeXA)( mce.tmpMap ) == mce.sb->tyenv->types_used );
   VG_(deleteXA)( mce.tmpMap );

   if (mce.tmpHowUsed) {
      VG_(free)( mce.tmpHowUsed );
   }

   tl_assert(mce.sb == sb_out);
   return sb_out;
}


/*--------------------------------------------------------------------*/
/*--- end                                           mc_translate.c ---*/
/*--------------------------------------------------------------------*/
