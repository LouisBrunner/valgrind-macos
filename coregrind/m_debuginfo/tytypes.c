
/*--------------------------------------------------------------------*/
/*--- Representation of source level types.              tytypes.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2008 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"   /* to keep priv_tytypes.h happy */

#include "priv_misc.h"         /* dinfo_zalloc/free/strdup */
#include "priv_d3basics.h"     /* ML_(evaluate_Dwarf3_Expr) et al */
#include "priv_tytypes.h"      /* self */


TyAdmin* ML_(new_TyAdmin) ( UWord cuOff, TyAdmin* next ) {
   TyAdmin* admin = ML_(dinfo_zalloc)( sizeof(TyAdmin) );
   admin->cuOff = cuOff;
   admin->next  = next;
   return admin;
}
TyAtom* ML_(new_TyAtom) ( UChar* name, Long value ) {
   TyAtom* atom = ML_(dinfo_zalloc)( sizeof(TyAtom) );
   atom->name  = name;
   atom->value = value;
   return atom;
}
TyField* ML_(new_TyField) ( UChar* name,
                            Type* typeR, D3Expr* loc ) {
   TyField* field = ML_(dinfo_zalloc)( sizeof(TyField) );
   field->name  = name;
   field->typeR = typeR;
   field->loc   = loc;
   return field;
}
TyBounds* ML_(new_TyBounds) ( void ) {
   TyBounds* bounds = ML_(dinfo_zalloc)( sizeof(TyBounds) );
   bounds->magic = TyBounds_MAGIC;
   return bounds;
}
D3Expr* ML_(new_D3Expr) ( UChar* bytes, UWord nbytes ) {
   D3Expr* expr = ML_(dinfo_zalloc)( sizeof(D3Expr) );
   expr->bytes = bytes;
   expr->nbytes = nbytes;
   return expr;
}
Type* ML_(new_Type) ( void ) {
   Type* type = ML_(dinfo_zalloc)( sizeof(Type) );
   return type;
}

static void delete_TyAtom ( TyAtom* atom ) {
   /* .name is in DebugInfo.strchunks */
   ML_(dinfo_free)(atom);
}
static void delete_TyField ( TyField* field ) {
   /* .name is in DebugInfo.strchunks */
   /* typeR and loc will be on the admin list; no need to free */
   ML_(dinfo_free)(field);
}
static void delete_TyBounds ( TyBounds* bounds ) {
   ML_(dinfo_free)(bounds);
}
static void delete_D3Expr ( D3Expr* expr ) {
   /* .bytes is in DebugInfo.strchunks */
   ML_(dinfo_free)(expr);
}
static void delete_Type ( Type* ty ) {
   switch (ty->tag) {
      case Ty_Base:
         /* .name is in DebugInfo.strchunks */
         break;
      case Ty_PorR:
         /* typeR will be on the admin list */
         break;
      case Ty_TyDef:
         /* .name is in DebugInfo.strchunks */
         /* typeR will be on the admin list */
         break;
      case Ty_StOrUn:
         /* .name is in DebugInfo.strchunks */
         /* Just dump the containing XArray.  The fields themselves
            will be on the admin list. */
         if (ty->Ty.StOrUn.fields)
            VG_(deleteXA)(ty->Ty.StOrUn.fields);
         break;
      case Ty_Enum:
         /* .name is in DebugInfo.strchunks */
         if (ty->Ty.Enum.atomRs)
            VG_(deleteXA)( ty->Ty.Enum.atomRs);
         /* Just dump the containing XArray.  The atoms themselves
            will be on the admin list. */
         break;
      case Ty_Array:
         if (ty->Ty.Array.bounds)
            VG_(deleteXA)( ty->Ty.Array.bounds);
         /* Just dump the containing XArray.  The bounds themselves
            will be on the admin list. */
         break;
      case Ty_Fn:
         break;
      case Ty_Qual:
         /* typeR will be on the admin list */
         break;
      case Ty_Void:
         break;
      default:
         vg_assert(0);
   }
}

void ML_(delete_TyAdmin_and_payload) ( TyAdmin* ad ) {
   vg_assert(ad->payload);
   switch (ad->tag) {
      case TyA_Type:   delete_Type(ad->payload);     break;
      case TyA_Atom:   delete_TyAtom(ad->payload);   break;
      case TyA_Expr:   delete_D3Expr(ad->payload);   break;
      case TyA_Field:  delete_TyField(ad->payload);  break;
      case TyA_Bounds: delete_TyBounds(ad->payload); break;
      default:         vg_assert(0);
   }
   ML_(dinfo_free)(ad);
}



static void pp_XArray_of_pointersOrRefs ( XArray* xa ) {
   Word i;
   VG_(printf)("{");
   for (i = 0; i < VG_(sizeXA)(xa); i++) {
      void* ptr = *(void**) VG_(indexXA)(xa, i);
      VG_(printf)("0x%05lx", (unsigned long)(ptr));
      if (i+1 < VG_(sizeXA)(xa))
         VG_(printf)(",");
   }
   VG_(printf)("}");
}
void ML_(pp_TyAtom) ( TyAtom* atom ) {
   VG_(printf)("TyAtom(%lld,\"%s\")", atom->value, atom->name);
}
void ML_(pp_D3Expr) ( D3Expr* expr ) {
   VG_(printf)("D3Expr(%p,%lu)", expr->bytes, expr->nbytes);
}
void ML_(pp_TyField) ( TyField* field ) {
   VG_(printf)("TyField(0x%05lx,%p,\"%s\")",
               (unsigned long)(field->typeR), field->loc,
               field->name ? field->name : (UChar*)"");
}
void ML_(pp_TyBounds) ( TyBounds* bounds ) {
   vg_assert(bounds->magic == TyBounds_MAGIC);
   VG_(printf)("TyBounds[");
   if (bounds->knownL)
      VG_(printf)("%lld", bounds->boundL);
   else
      VG_(printf)("??");
   VG_(printf)(",");
   if (bounds->knownU)
      VG_(printf)("%lld", bounds->boundU);
   else
      VG_(printf)("??");
   VG_(printf)("]");
}

static void pp_TyBounds_C_ishly ( TyBounds* bounds ) {
   vg_assert(bounds->magic == TyBounds_MAGIC);
   if (bounds->knownL && bounds->knownU && bounds->boundL == 0) {
      VG_(printf)("[%lld]", 1 + bounds->boundU);
   }
   else
   if (bounds->knownL && (!bounds->knownU) && bounds->boundL == 0) {
      VG_(printf)("[]");
   }
   else
      ML_(pp_TyBounds)( bounds );
}


void ML_(pp_Type) ( Type* ty )
{
   if (!ty) {
      VG_(printf)("**type=NULL**");
      return;
   }
   switch (ty->tag) {
      case Ty_Base:
         VG_(printf)("Ty_Base(%d,%c,\"%s\")",
                     ty->Ty.Base.szB, ty->Ty.Base.enc,
                     ty->Ty.Base.name ? ty->Ty.Base.name
                                        : (UChar*)"(null)" );
         break;
      case Ty_PorR:
         VG_(printf)("Ty_PorR(%d,%c,0x%05lx)",
                     ty->Ty.PorR.szB,
                     ty->Ty.PorR.isPtr ? 'P' : 'R',
                     (unsigned long)(ty->Ty.PorR.typeR));
         break;
      case Ty_Enum:
         VG_(printf)("Ty_Enum(%d,%p,\"%s\")",
                     ty->Ty.Enum.szB, ty->Ty.Enum.atomRs,
                     ty->Ty.Enum.name ? ty->Ty.Enum.name
                                        : (UChar*)"" );
         if (ty->Ty.Enum.atomRs)
            pp_XArray_of_pointersOrRefs( ty->Ty.Enum.atomRs );
         break;
      case Ty_StOrUn:
         if (ty->Ty.StOrUn.complete) {
            VG_(printf)("Ty_StOrUn(%ld,%c,%p,\"%s\")",
                        ty->Ty.StOrUn.szB, 
                        ty->Ty.StOrUn.isStruct ? 'S' : 'U',
                        ty->Ty.StOrUn.fields,
                        ty->Ty.StOrUn.name ? ty->Ty.StOrUn.name
                                             : (UChar*)"" );
            if (ty->Ty.StOrUn.fields)
               pp_XArray_of_pointersOrRefs( ty->Ty.StOrUn.fields );
         } else {
            VG_(printf)("Ty_StOrUn(INCOMPLETE,\"%s\")",
                        ty->Ty.StOrUn.name);
         }
         break;
      case Ty_Array:
         VG_(printf)("Ty_Array(0x%05lx,%p)",
                     (unsigned long)(ty->Ty.Array.typeR), ty->Ty.Array.bounds);
         if (ty->Ty.Array.bounds)
            pp_XArray_of_pointersOrRefs( ty->Ty.Array.bounds );
         break;
      case Ty_TyDef:
         VG_(printf)("Ty_TyDef(0x%05lx,\"%s\")",
                     (unsigned long)(ty->Ty.TyDef.typeR),
                     ty->Ty.TyDef.name ? ty->Ty.TyDef.name
                                         : (UChar*)"" );
         break;
      case Ty_Fn:
         VG_(printf)("Ty_Fn");
         break;
      case Ty_Qual:
         VG_(printf)("Ty_Qual(%c,0x%05lx)", ty->Ty.Qual.qual,
                     (unsigned long)(ty->Ty.Qual.typeR));
         break;
      case Ty_Void:
         VG_(printf)("Ty_Void%s",
                     ty->Ty.Void.isFake ? "(fake)" : "");
         break;
      default: VG_(printf)("pp_Type:???");
         break;
   }
}
void ML_(pp_TyAdmin) ( TyAdmin* admin ) {
   if (admin->cuOff != -1UL) {
      VG_(printf)("<%05lx,%p> ", admin->cuOff, admin->payload);
   } else {
      VG_(printf)("<ff..f,%p> ", admin->payload);
   }
   switch (admin->tag) {
      case TyA_Type:   ML_(pp_Type)(admin->payload);     break;
      case TyA_Atom:   ML_(pp_TyAtom)(admin->payload);   break;
      case TyA_Expr:   ML_(pp_D3Expr)(admin->payload);   break;
      case TyA_Field:  ML_(pp_TyField)(admin->payload);  break;
      case TyA_Bounds: ML_(pp_TyBounds)(admin->payload); break;
      default:         VG_(printf)("pp_TyAdmin:???");    break;
   }
}

/* NOTE: this assumes that the types have all been 'resolved' (that
   is, inter-type references expressed as .debug_info offsets have
   been converted into pointers) */
void ML_(pp_Type_C_ishly) ( Type* ty )
{
   if (!ty) {
      VG_(printf)("**type=NULL**");
      return;
   }
   switch (ty->tag) {
      case Ty_Base:
         if (!ty->Ty.Base.name) goto unhandled;
         VG_(printf)("%s", ty->Ty.Base.name);
         break;
      case Ty_PorR:
         ML_(pp_Type_C_ishly)(ty->Ty.PorR.typeR);
         VG_(printf)("%s", ty->Ty.PorR.isPtr ? "*" : "&");
         break;
      case Ty_Enum:
         if (!ty->Ty.Enum.name) goto unhandled;
         VG_(printf)("enum %s", ty->Ty.Enum.name);
         break;
      case Ty_StOrUn:
         if (!ty->Ty.StOrUn.name) goto unhandled;
         VG_(printf)("%s %s",
                     ty->Ty.StOrUn.isStruct ? "struct" : "union",
                     ty->Ty.StOrUn.name);
         break;
      case Ty_Array:
         ML_(pp_Type_C_ishly)(ty->Ty.Array.typeR);
         if (ty->Ty.Array.bounds) {
            Word    w;
            XArray* xa = ty->Ty.Array.bounds;
            for (w = 0; w < VG_(sizeXA)(xa); w++) {
               pp_TyBounds_C_ishly( *(TyBounds**)VG_(indexXA)(xa, w) );
            }
         } else {
            VG_(printf)("%s", "[??]");
         }
         break;
      case Ty_TyDef:
         if (!ty->Ty.TyDef.name) goto unhandled;
         VG_(printf)("%s", ty->Ty.TyDef.name);
         break;
      case Ty_Fn:
         VG_(printf)("%s", "<function_type>");
         break;
      case Ty_Qual:
         switch (ty->Ty.Qual.qual) {
            case 'C': VG_(printf)("const "); break;
            case 'V': VG_(printf)("volatile "); break;
            default: goto unhandled;
         }
         ML_(pp_Type_C_ishly)(ty->Ty.Qual.typeR);
         break;
      case Ty_Void:
         VG_(printf)("%svoid",
                     ty->Ty.Void.isFake ? "fake" : "");
         break;
      default: VG_(printf)("pp_Type_C_ishly:???");
         break;
   }
   return;

  unhandled:
   ML_(pp_Type)(ty);
}


static MaybeUWord mk_MaybeUWord_Nothing ( void ) {
   MaybeUWord muw;
   muw.w = 0;
   muw.b = False;
   return muw;
}
static MaybeUWord mk_MaybeUWord_Just ( UWord w ) {
   MaybeUWord muw;
   muw.w = w;
   muw.b = True;
   return muw;
}
static MaybeUWord mul_MaybeUWord ( MaybeUWord muw1, MaybeUWord muw2 ) {
   if (!muw1.b) { vg_assert(muw1.w == 0); return muw1; }
   if (!muw2.b) { vg_assert(muw2.w == 0); return muw2; }
   muw1.w *= muw2.w;
   return muw1;
}

/* How big is this type?  (post-resolved only) */
/* FIXME: check all pointers before dereferencing */
MaybeUWord ML_(sizeOfType)( Type* ty )
{
   Word       i;
   MaybeUWord eszB;
   vg_assert(ty);
   switch (ty->tag) {
      case Ty_Base:
         vg_assert(ty->Ty.Base.szB > 0);
         return mk_MaybeUWord_Just( ty->Ty.Base.szB );
      case Ty_Qual:
         return ML_(sizeOfType)( ty->Ty.Qual.typeR );
      case Ty_TyDef:
         if (!ty->Ty.TyDef.typeR)
            return mk_MaybeUWord_Nothing(); /*UNKNOWN*/
         return ML_(sizeOfType)( ty->Ty.TyDef.typeR );
      case Ty_PorR:
         vg_assert(ty->Ty.PorR.szB == 4 || ty->Ty.PorR.szB == 8);
         return mk_MaybeUWord_Just( ty->Ty.PorR.szB );
      case Ty_StOrUn:
         return ty->Ty.StOrUn.complete
                   ? mk_MaybeUWord_Just( ty->Ty.StOrUn.szB )
                   : mk_MaybeUWord_Nothing();
      case Ty_Enum:
         return mk_MaybeUWord_Just( ty->Ty.Enum.szB );
      case Ty_Array:
         if (!ty->Ty.Array.typeR)
            return mk_MaybeUWord_Nothing(); /*UNKNOWN*/
         eszB = ML_(sizeOfType)( ty->Ty.Array.typeR );
         for (i = 0; i < VG_(sizeXA)( ty->Ty.Array.bounds ); i++) {
            TyBounds* bo
               = *(TyBounds**)VG_(indexXA)(ty->Ty.Array.bounds, i);
            vg_assert(bo);
            if (!(bo->knownL && bo->knownU))
               return mk_MaybeUWord_Nothing(); /*UNKNOWN*/
            eszB = mul_MaybeUWord( 
                      eszB,
                      mk_MaybeUWord_Just( bo->boundU - bo->boundL + 1 ));
         }
         return eszB;
      default:
         VG_(printf)("ML_(sizeOfType): unhandled: ");
         ML_(pp_Type)(ty);
         VG_(printf)("\n");
         vg_assert(0);
   }
}


static void copy_UWord_into_XA ( XArray* /* of UChar */ xa,
                                 UWord uw ) {
   UChar buf[32];
   VG_(memset)(buf, 0, sizeof(buf));
   VG_(sprintf)(buf, "%lu", uw);
   VG_(addBytesToXA)( xa, buf, VG_(strlen)(buf));
}

/* Describe where in the type 'offset' falls.  Caller must
   deallocate the resulting XArray. */
XArray* /*UChar*/ ML_(describe_type)( /*OUT*/OffT* residual_offset,
                                      Type* ty, OffT offset )
{
   XArray* xa = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                            sizeof(UChar) );
   vg_assert(xa);

   while (True) {
      vg_assert(ty);

      switch (ty->tag) {

         /* These are all atomic types; there is nothing useful we can
            do. */
         case Ty_Enum:
         case Ty_Fn:
         case Ty_Void:
         case Ty_PorR:
         case Ty_Base:
            goto done;

         case Ty_StOrUn: {
            Word       i;
            GXResult   res;
            MaybeUWord muw;
            TyField    *field = NULL, *fields;
            OffT       offMin = 0, offMax1 = 0;
            if (!ty->Ty.StOrUn.isStruct) goto done;
            fields = ty->Ty.StOrUn.fields;
            if ((!fields) || VG_(sizeXA)(fields) == 0) goto done;
            for (i = 0; i < VG_(sizeXA)( fields ); i++ ) {
               field = *(TyField**)VG_(indexXA)( fields, i );
               vg_assert(field);
               vg_assert(field->loc);
               /* Re data_bias in this call, we should really send in
                  a legitimate value.  But the expression is expected
                  to be a constant expression, evaluation of which
                  will not need to use DW_OP_addr and hence we can
                  avoid the trouble of plumbing the data bias through
                  to this point (if, indeed, it has any meaning; from
                  which DebugInfo would we take the data bias? */
               res = ML_(evaluate_Dwarf3_Expr)(
                       field->loc->bytes, field->loc->nbytes,
                       NULL/*fbGX*/, NULL/*RegSummary*/,
                       0/*data_bias*/,
                       True/*push_initial_zero*/);
               if (0) {
                  VG_(printf)("QQQ ");
                  ML_(pp_GXResult)(res);
                  VG_(printf)("\n");
               }
               if (res.kind != GXR_Value)
                  continue;
               muw = ML_(sizeOfType)( field->typeR );
               if (muw.b != True)
                  goto done; /* size of field is unknown (?!) */
               offMin  = res.word;
               offMax1 = offMin + muw.w;
               if (offMin == offMax1)
                  continue;
               vg_assert(offMin < offMax1);
               if (offset >= offMin && offset < offMax1)
                  break;
            }
            /* Did we find a suitable field? */
            vg_assert(i >= 0 && i <= VG_(sizeXA)( fields ));
            if (i == VG_(sizeXA)( fields ))
               goto done; /* No.  Give up. */
            /* Yes.  'field' is it. */
            if (!field->name) goto done;
            VG_(addBytesToXA)( xa, ".", 1 );
            VG_(addBytesToXA)( xa, field->name,
                               VG_(strlen)(field->name) );
            offset -= offMin;
            ty = field->typeR;
            if (!ty) goto done;
            /* keep going; look inside the field. */
            break;
         }

         case Ty_Array: {
            MaybeUWord muw;
            TyBounds*  bounds;
            UWord      size, eszB, ix;
            /* Just deal with the simple, common C-case: 1-D array,
               zero based, known size. */
            if (!(ty->Ty.Array.typeR && ty->Ty.Array.bounds))
               goto done;
            if (VG_(sizeXA)( ty->Ty.Array.bounds ) != 1) goto done;
            bounds = *(TyBounds**)VG_(indexXA)( ty->Ty.Array.bounds, 0 );
            vg_assert(bounds);
            vg_assert(bounds->magic == TyBounds_MAGIC);
            if (!(bounds->knownL && bounds->knownU && bounds->boundL == 0
                  && bounds->boundU >= bounds->boundL))
               goto done;
            size = bounds->boundU - bounds->boundL + 1;
            vg_assert(size >= 1);
            muw = ML_(sizeOfType)( ty->Ty.Array.typeR );
            if (muw.b != True)
               goto done; /* size of element type not known */
            eszB = muw.w;
            if (eszB == 0) goto done;
            ix = offset / eszB;
            VG_(addBytesToXA)( xa, "[", 1 );
            copy_UWord_into_XA( xa, ix );
            VG_(addBytesToXA)( xa, "]", 1 );
            ty = ty->Ty.Array.typeR;
            offset -= ix * eszB;
            /* keep going; look inside the array element. */
            break;
         }

         case Ty_Qual: {
            if (!ty->Ty.Qual.typeR) goto done;
            ty = ty->Ty.Qual.typeR;
            break;
         }

         case Ty_TyDef: {
            if (!ty->Ty.TyDef.typeR) goto done;
            ty = ty->Ty.TyDef.typeR;
            break;
         }

         default: {
            VG_(printf)("ML_(describe_type): unhandled: ");
            ML_(pp_Type)(ty);
            VG_(printf)("\n");
            vg_assert(0);
         }

      }
   }

  done:
   *residual_offset = offset;
   VG_(addBytesToXA)( xa, "\0", 1 );
   return xa;
}

/*--------------------------------------------------------------------*/
/*--- end                                                tytypes.c ---*/
/*--------------------------------------------------------------------*/
