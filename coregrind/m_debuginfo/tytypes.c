
/*--------------------------------------------------------------------*/
/*--- Representation of source level types.              tytypes.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2013 OpenWorks LLP
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
#include "pub_core_debuginfo.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"   /* to keep priv_tytypes.h happy */

#include "priv_misc.h"         /* dinfo_zalloc/free/strdup */
#include "priv_d3basics.h"     /* ML_(evaluate_Dwarf3_Expr) et al */
#include "priv_tytypes.h"      /* self */


/* Does this TyEnt denote a type, as opposed to some other kind of
   thing? */

Bool ML_(TyEnt__is_type)( TyEnt* te )
{
   switch (te->tag) {
      case Te_EMPTY: case Te_INDIR: case Te_UNKNOWN: 
      case Te_Atom:  case Te_Field: case Te_Bound:
         return False;
      case Te_TyBase:   case Te_TyPtr:     case Te_TyRef:
      case Te_TyPtrMbr: case Te_TyRvalRef: case Te_TyTyDef:
      case Te_TyStOrUn: case Te_TyEnum:    case Te_TyArray:
      case Te_TyFn:     case Te_TyQual:    case Te_TyVoid:
         return True;
      default:
         vg_assert(0);
   }
}


/* Print a TyEnt, debug-style. */

static void pp_XArray_of_cuOffs ( XArray* xa )
{
   Word i;
   VG_(printf)("{");
   for (i = 0; i < VG_(sizeXA)(xa); i++) {
      UWord cuOff = *(UWord*)VG_(indexXA)(xa, i);
      VG_(printf)("0x%05lx", cuOff);
      if (i+1 < VG_(sizeXA)(xa))
         VG_(printf)(",");
   }
   VG_(printf)("}");
}

void ML_(pp_TyEnt)( TyEnt* te )
{
   VG_(printf)("0x%05lx  ", te->cuOff);
   switch (te->tag) {
      case Te_EMPTY:
         VG_(printf)("EMPTY");
         break;
      case Te_INDIR:
         VG_(printf)("INDIR(0x%05lx)", te->Te.INDIR.indR);
         break;
      case Te_UNKNOWN:
         VG_(printf)("UNKNOWN");
         break;
      case Te_Atom:
         VG_(printf)("Te_Atom(%s%lld,\"%s\")",
                     te->Te.Atom.valueKnown ? "" : "unknown:",
                     te->Te.Atom.value, te->Te.Atom.name);
         break;
      case Te_Field:
         if (te->Te.Field.nLoc == -1)
            VG_(printf)("Te_Field(ty=0x%05lx,pos.offset=%ld,\"%s\")",
                        te->Te.Field.typeR, te->Te.Field.pos.offset,
                        te->Te.Field.name ? te->Te.Field.name : "");
         else
            VG_(printf)("Te_Field(ty=0x%05lx,nLoc=%lu,pos.loc=%p,\"%s\")",
                        te->Te.Field.typeR, te->Te.Field.nLoc,
                        te->Te.Field.pos.loc,
                        te->Te.Field.name ? te->Te.Field.name : "");
         break;
      case Te_Bound:
         VG_(printf)("Te_Bound[");
         if (te->Te.Bound.knownL)
            VG_(printf)("%lld", te->Te.Bound.boundL);
         else
            VG_(printf)("??");
         VG_(printf)(",");
         if (te->Te.Bound.knownU)
            VG_(printf)("%lld", te->Te.Bound.boundU);
         else
            VG_(printf)("??");
         VG_(printf)("]");
         break;
      case Te_TyBase:
         VG_(printf)("Te_TyBase(%d,%c,\"%s\")",
                     te->Te.TyBase.szB, te->Te.TyBase.enc,
                     te->Te.TyBase.name ? te->Te.TyBase.name
                                        : "(null)" );
         break;
      case Te_TyPtr:
         VG_(printf)("Te_TyPtr(%d,0x%05lx)", te->Te.TyPorR.szB,
                     te->Te.TyPorR.typeR);
         break;
      case Te_TyRef:
         VG_(printf)("Te_TyRef(%d,0x%05lx)", te->Te.TyPorR.szB,
                     te->Te.TyPorR.typeR);
         break;
      case Te_TyPtrMbr:
         VG_(printf)("Te_TyMbr(%d,0x%05lx)", te->Te.TyPorR.szB,
                     te->Te.TyPorR.typeR);
         break;
      case Te_TyRvalRef:
         VG_(printf)("Te_TyRvalRef(%d,0x%05lx)", te->Te.TyPorR.szB,
                     te->Te.TyPorR.typeR);
         break;
      case Te_TyTyDef:
         VG_(printf)("Te_TyTyDef(0x%05lx,\"%s\")",
                     te->Te.TyTyDef.typeR,
                     te->Te.TyTyDef.name ? te->Te.TyTyDef.name
                                         : "" );
         break;
      case Te_TyStOrUn:
         if (te->Te.TyStOrUn.complete) {
            VG_(printf)("Te_TyStOrUn(%ld,%c,%p,\"%s\")",
                        te->Te.TyStOrUn.szB, 
                        te->Te.TyStOrUn.isStruct ? 'S' : 'U',
                        te->Te.TyStOrUn.fieldRs,
                        te->Te.TyStOrUn.name ? te->Te.TyStOrUn.name
                                             : "" );
            if (te->Te.TyStOrUn.fieldRs)
               pp_XArray_of_cuOffs( te->Te.TyStOrUn.fieldRs );
         } else {
            VG_(printf)("Te_TyStOrUn(INCOMPLETE,\"%s\")",
                        te->Te.TyStOrUn.name);
         }
         break;
      case Te_TyEnum:
         VG_(printf)("Te_TyEnum(%d,%p,\"%s\")",
                     te->Te.TyEnum.szB, te->Te.TyEnum.atomRs,
                     te->Te.TyEnum.name ? te->Te.TyEnum.name
                                        : "" );
         if (te->Te.TyEnum.atomRs)
            pp_XArray_of_cuOffs( te->Te.TyEnum.atomRs );
         break;
      case Te_TyArray:
         VG_(printf)("Te_TyArray(0x%05lx,%p)",
                     te->Te.TyArray.typeR, te->Te.TyArray.boundRs);
         if (te->Te.TyArray.boundRs)
            pp_XArray_of_cuOffs( te->Te.TyArray.boundRs );
         break;
      case Te_TyFn:
         VG_(printf)("Te_TyFn");
         break;
      case Te_TyQual:
         VG_(printf)("Te_TyQual(%c,0x%05lx)", te->Te.TyQual.qual,
                     te->Te.TyQual.typeR);
         break;
      case Te_TyVoid:
         VG_(printf)("Te_TyVoid%s",
                     te->Te.TyVoid.isFake ? "(fake)" : "");
         break;
      default:
         vg_assert(0);
   }
}


/* Print a whole XArray of TyEnts, debug-style */

void ML_(pp_TyEnts)( XArray* tyents, const HChar* who )
{
   Word i, n;
   VG_(printf)("------ %s ------\n", who);
   n = VG_(sizeXA)( tyents );
   for (i = 0; i < n; i++) {
      TyEnt* tyent = (TyEnt*)VG_(indexXA)( tyents, i );
      VG_(printf)("   [%5ld]  ", i);
      ML_(pp_TyEnt)( tyent );
      VG_(printf)("\n");
   }
}


/* Print a TyEnt, C style, chasing stuff as necessary. */

static void pp_TyBound_C_ishly ( XArray* tyents, UWord cuOff )
{
   TyEnt* ent = ML_(TyEnts__index_by_cuOff)( tyents, NULL, cuOff );
   if (!ent) {
      VG_(printf)("**bounds-have-invalid-cuOff**");
      return;
   }
   vg_assert(ent->tag == Te_Bound);
   if (ent->Te.Bound.knownL && ent->Te.Bound.knownU
       && ent->Te.Bound.boundL == 0) {
      VG_(printf)("[%lld]", 1 + ent->Te.Bound.boundU);
   }
   else
   if (ent->Te.Bound.knownL && (!ent->Te.Bound.knownU) 
       && ent->Te.Bound.boundL == 0) {
      VG_(printf)("[]");
   }
   else
      ML_(pp_TyEnt)( ent );
}

void ML_(pp_TyEnt_C_ishly)( XArray* /* of TyEnt */ tyents,
                            UWord cuOff )
{
   TyEnt* ent = ML_(TyEnts__index_by_cuOff)( tyents, NULL, cuOff );
   if (!ent) {
      VG_(printf)("**type-has-invalid-cuOff**");
      return;
   }
   switch (ent->tag) {
      case Te_TyBase:
         if (!ent->Te.TyBase.name) goto unhandled;
         VG_(printf)("%s", ent->Te.TyBase.name);
         break;
      case Te_TyPtr:
         ML_(pp_TyEnt_C_ishly)(tyents, ent->Te.TyPorR.typeR);
         VG_(printf)("*");
         break;
      case Te_TyRef:
         ML_(pp_TyEnt_C_ishly)(tyents, ent->Te.TyPorR.typeR);
         VG_(printf)("&");
         break;
      case Te_TyPtrMbr:
         ML_(pp_TyEnt_C_ishly)(tyents, ent->Te.TyPorR.typeR);
         VG_(printf)("*");
         break;
      case Te_TyRvalRef:
         ML_(pp_TyEnt_C_ishly)(tyents, ent->Te.TyPorR.typeR);
         VG_(printf)("&&");
         break;
      case Te_TyEnum:
         VG_(printf)("enum %s", ent->Te.TyEnum.name ? ent->Te.TyEnum.name
                                                    : "<anonymous>" );
         break;
      case Te_TyStOrUn:
         VG_(printf)("%s %s",
                     ent->Te.TyStOrUn.isStruct ? "struct" : "union",
                     ent->Te.TyStOrUn.name ? ent->Te.TyStOrUn.name
                                           : "<anonymous>" );
         break;
      case Te_TyArray:
         ML_(pp_TyEnt_C_ishly)(tyents, ent->Te.TyArray.typeR);
         if (ent->Te.TyArray.boundRs) {
            Word    w;
            XArray* xa = ent->Te.TyArray.boundRs;
            for (w = 0; w < VG_(sizeXA)(xa); w++) {
               pp_TyBound_C_ishly( tyents, *(UWord*)VG_(indexXA)(xa, w) );
            }
         } else {
            VG_(printf)("%s", "[??]");
         }
         break;
      case Te_TyTyDef:
         VG_(printf)("%s", ent->Te.TyTyDef.name ? ent->Te.TyTyDef.name
                                                : "<anonymous>" );
         break;
      case Te_TyFn:
         VG_(printf)("%s", "<function_type>");
         break;
      case Te_TyQual:
         switch (ent->Te.TyQual.qual) {
            case 'C': VG_(printf)("const "); break;
            case 'V': VG_(printf)("volatile "); break;
            default: goto unhandled;
         }
         ML_(pp_TyEnt_C_ishly)(tyents, ent->Te.TyQual.typeR);
         break;
      case Te_TyVoid:
         VG_(printf)("%svoid",
                     ent->Te.TyVoid.isFake ? "fake" : "");
         break;
      case Te_UNKNOWN:
         ML_(pp_TyEnt)(ent);
         break;
      default:
         goto unhandled;
   }
   return;

  unhandled:
   VG_(printf)("pp_TyEnt_C_ishly:unhandled: ");
   ML_(pp_TyEnt)(ent);
   vg_assert(0);
}


/* 'ents' is an XArray of TyEnts, sorted by their .cuOff fields.  Find
   the entry which has .cuOff field as specified.  Returns NULL if not
   found.  Asserts if more than one entry has the specified .cuOff
   value. */

void ML_(TyEntIndexCache__invalidate) ( TyEntIndexCache* cache )
{
   Word i;
   for (i = 0; i < N_TYENT_INDEX_CACHE; i++) {
      cache->ce[i].cuOff0 = 0;    /* not actually necessary */
      cache->ce[i].ent0   = NULL; /* "invalid entry" */
      cache->ce[i].cuOff1 = 0;    /* not actually necessary */
      cache->ce[i].ent1   = NULL; /* "invalid entry" */
   }
}

TyEnt* ML_(TyEnts__index_by_cuOff) ( XArray* /* of TyEnt */ ents,
                                     TyEntIndexCache* cache,
                                     UWord cuOff_to_find )
{
   Bool  found;
   Word  first, last;
   TyEnt key, *res;

   /* crude stats, aggregated over all caches */
   static UWord cacheQs = 0 - 1;
   static UWord cacheHits = 0;

   if (0 && 0 == (cacheQs & 0xFFFF))
      VG_(printf)("cache: %'lu queries, %'lu misses\n", 
                  cacheQs, cacheQs - cacheHits);

   if (LIKELY(cache != NULL)) {
      UWord h = cuOff_to_find % (UWord)N_TYENT_INDEX_CACHE;
      cacheQs++;
      // dude, like, way 0, dude.
      if (cache->ce[h].cuOff0 == cuOff_to_find && cache->ce[h].ent0 != NULL) {
         // dude, way 0 is a total hit!
         cacheHits++;
         return cache->ce[h].ent0;
      }
      // dude, check out way 1, dude.
      if (cache->ce[h].cuOff1 == cuOff_to_find && cache->ce[h].ent1 != NULL) {
         // way 1 hit
         UWord  tc;
         TyEnt* te;
         cacheHits++;
         // dude, way 1 is the new way 0.  move with the times, dude.
         tc = cache->ce[h].cuOff0;
         te = cache->ce[h].ent0;
         cache->ce[h].cuOff0 = cache->ce[h].cuOff1;
         cache->ce[h].ent0   = cache->ce[h].ent1;
         cache->ce[h].cuOff1 = tc;
         cache->ce[h].ent1   = te;
         return cache->ce[h].ent0;
      }
   }

   /* We'll have to do it the hard way */
   key.cuOff = cuOff_to_find;
   key.tag   = Te_EMPTY;
   found = VG_(lookupXA)( ents, &key, &first, &last );
   //found = VG_(lookupXA_UNBOXED)( ents, cuOff_to_find, &first, &last, 
   //                               offsetof(TyEnt,cuOff) );
   if (!found)
      return NULL;
   /* If this fails, the array is invalid in the sense that there is
      more than one entry with .cuOff == cuOff_to_find. */
   vg_assert(first == last);
   res = (TyEnt*)VG_(indexXA)( ents, first );

   if (LIKELY(cache != NULL) && LIKELY(res != NULL)) {
      /* this is a bit stupid, computing this twice.  Oh well.
         Perhaps some magic gcc transformation will common them up.
         re "res != NULL", since .ent of NULL denotes 'invalid entry',
         we can't cache the result when res == NULL. */
      UWord h = cuOff_to_find % (UWord)N_TYENT_INDEX_CACHE;
      cache->ce[h].cuOff1 = cache->ce[h].cuOff0;
      cache->ce[h].ent1   = cache->ce[h].ent0;
      cache->ce[h].cuOff0 = cuOff_to_find;
      cache->ce[h].ent0   = res;
   }

   return res;
}


/* Generates a total ordering on TyEnts based only on their .cuOff
   fields. */

Word ML_(TyEnt__cmp_by_cuOff_only) ( const TyEnt* te1, const TyEnt* te2 )
{
   if (te1->cuOff < te2->cuOff) return -1;
   if (te1->cuOff > te2->cuOff) return 1;
   return 0;
}


/* Generates a total ordering on TyEnts based on everything except
   their .cuOff fields. */
static inline Word UWord__cmp ( UWord a, UWord b ) {
   if (a < b) return -1;
   if (a > b) return 1;
   return 0;
}
static inline Word Long__cmp ( Long a, Long b ) {
   if (a < b) return -1;
   if (a > b) return 1;
   return 0;
}
static inline Word Bool__cmp ( Bool a, Bool b ) {
   vg_assert( ((UWord)a) <= 1 );
   vg_assert( ((UWord)b) <= 1 );
   if (a < b) return -1;
   if (a > b) return 1;
   return 0;
}
static inline Word UChar__cmp ( UChar a, UChar b ) {
   if (a < b) return -1;
   if (a > b) return 1;
   return 0;
}
static inline Word Int__cmp ( Int a, Int b ) {
   if (a < b) return -1;
   if (a > b) return 1;
   return 0;
}
static Word XArray_of_UWord__cmp ( XArray* a, XArray* b ) {
   Word i, r;
   Word aN = VG_(sizeXA)( a );
   Word bN = VG_(sizeXA)( b );
   if (aN < bN) return -1;
   if (aN > bN) return 1;
   for (i = 0; i < aN; i++) {
      r = UWord__cmp( *(UWord*)VG_(indexXA)( a, i ),
                      *(UWord*)VG_(indexXA)( b, i ) );
      if (r != 0) return r;
   }
   return 0;
}
static Word Bytevector__cmp ( UChar* a, UChar* b, Word n ) {
   Word i, r;
   vg_assert(n >= 0);
   for (i = 0; i < n; i++) {
      r = UChar__cmp( a[i], b[i] );
      if (r != 0) return r;
   }
   return 0;
}
static Word Asciiz__cmp ( const HChar* a, const HChar* b ) {
   /* A wrapper around strcmp that handles NULL strings safely. */
   if (a == NULL && b == NULL) return 0;
   if (a == NULL && b != NULL) return -1;
   if (a != NULL && b == NULL) return 1;
   return VG_(strcmp)(a, b);
}

Word ML_(TyEnt__cmp_by_all_except_cuOff) ( const TyEnt* te1, const TyEnt* te2 )
{
   Word r;
   if (te1->tag < te2->tag) return -1;
   if (te1->tag > te2->tag) return 1;
   switch (te1->tag) {
   case Te_EMPTY:
      return 0;
   case Te_INDIR:
      r = UWord__cmp(te1->Te.INDIR.indR, te2->Te.INDIR.indR);
      return r;
   case Te_Atom:
      r = Bool__cmp(te1->Te.Atom.valueKnown, te2->Te.Atom.valueKnown);
      if (r != 0) return r;
      r = Long__cmp(te1->Te.Atom.value, te2->Te.Atom.value);
      if (r != 0) return r;
      r = Asciiz__cmp(te1->Te.Atom.name, te2->Te.Atom.name);
      return r;
   case Te_Field:
      r = Bool__cmp(te1->Te.Field.isStruct, te2->Te.Field.isStruct);
      if (r != 0) return r;
      r = UWord__cmp(te1->Te.Field.typeR, te2->Te.Field.typeR);
      if (r != 0) return r;
      r = Asciiz__cmp(te1->Te.Field.name, te2->Te.Field.name);
      if (r != 0) return r;
      r = UWord__cmp(te1->Te.Field.nLoc, te2->Te.Field.nLoc);
      if (r != 0) return r;
      if (te1->Te.Field.nLoc == -1)
         r = Long__cmp(te1->Te.Field.pos.offset, te2->Te.Field.pos.offset);
      else
         r = Bytevector__cmp(te1->Te.Field.pos.loc, te2->Te.Field.pos.loc,
                             te1->Te.Field.nLoc);
      return r;
   case Te_Bound:
      r = Bool__cmp(te1->Te.Bound.knownL, te2->Te.Bound.knownL);
      if (r != 0) return r;
      r = Bool__cmp(te1->Te.Bound.knownU, te2->Te.Bound.knownU);
      if (r != 0) return r;
      r = Long__cmp(te1->Te.Bound.boundL, te2->Te.Bound.boundL);
      if (r != 0) return r;
      r = Long__cmp(te1->Te.Bound.boundU, te2->Te.Bound.boundU);
      return r;
   case Te_TyBase:
      r = UChar__cmp(te1->Te.TyBase.enc, te2->Te.TyBase.enc);
      if (r != 0) return r;
      r = Int__cmp(te1->Te.TyBase.szB, te2->Te.TyBase.szB);
      if (r != 0) return r;
      r = Asciiz__cmp(te1->Te.TyBase.name, te2->Te.TyBase.name);
      return r;
   case Te_TyPtr:
   case Te_TyRef:
   case Te_TyPtrMbr:
   case Te_TyRvalRef:
      r = Int__cmp(te1->Te.TyPorR.szB, te2->Te.TyPorR.szB);
      if (r != 0) return r;
      r = UWord__cmp(te1->Te.TyPorR.typeR, te2->Te.TyPorR.typeR);
      return r;
   case Te_TyTyDef:
      r = UWord__cmp(te1->Te.TyTyDef.typeR, te2->Te.TyTyDef.typeR);
      if (r != 0) return r;
      r = Asciiz__cmp(te1->Te.TyTyDef.name, te2->Te.TyTyDef.name);
      return r;
   case Te_TyStOrUn:
      r = Bool__cmp(te1->Te.TyStOrUn.isStruct, te2->Te.TyStOrUn.isStruct);
      if (r != 0) return r;
      r = Bool__cmp(te1->Te.TyStOrUn.complete, te2->Te.TyStOrUn.complete);
      if (r != 0) return r;
      r = UWord__cmp(te1->Te.TyStOrUn.szB, te2->Te.TyStOrUn.szB);
      if (r != 0) return r;
      r = Asciiz__cmp(te1->Te.TyStOrUn.name, te2->Te.TyStOrUn.name);
      if (r != 0) return r;
      r = XArray_of_UWord__cmp(te1->Te.TyStOrUn.fieldRs,
                               te2->Te.TyStOrUn.fieldRs);
      return r;
   case Te_TyEnum:
      r = Int__cmp(te1->Te.TyEnum.szB, te2->Te.TyEnum.szB);
      if (r != 0) return r;
      r = Asciiz__cmp(te1->Te.TyEnum.name, te2->Te.TyEnum.name);
      if (r != 0) return r;
      r = XArray_of_UWord__cmp(te1->Te.TyEnum.atomRs, te2->Te.TyEnum.atomRs);
      return r;
   case Te_TyArray:
      r = UWord__cmp(te1->Te.TyArray.typeR, te2->Te.TyArray.typeR);
      if (r != 0) return r;
      r = XArray_of_UWord__cmp(te1->Te.TyArray.boundRs,
                               te2->Te.TyArray.boundRs);
      return r;
   case Te_TyFn:
      return 0;
   case Te_TyQual:
      r = UWord__cmp(te1->Te.TyQual.typeR, te2->Te.TyQual.typeR);
      if (r != 0) return r;
      r = UChar__cmp(te1->Te.TyQual.qual, te2->Te.TyQual.qual);
      return r;
   case Te_TyVoid:
      r = Bool__cmp(te1->Te.TyVoid.isFake, te2->Te.TyVoid.isFake);
      return r;
   default:
      vg_assert(0);
   }
}


/* Free up all directly or indirectly heap-allocated stuff attached to
   this TyEnt, and set its tag to Te_EMPTY.  The .cuOff field is
   unchanged. */

void ML_(TyEnt__make_EMPTY) ( TyEnt* te )
{
   UWord saved_cuOff;
   /* First, free up any fields in mallocville. */
   switch (te->tag) {
      case Te_EMPTY:
         break;
      case Te_INDIR:
         break;
      case Te_UNKNOWN:
         break;
      case Te_Atom:
         if (te->Te.Atom.name) ML_(dinfo_free)(te->Te.Atom.name);
         break;
      case Te_Field:
         if (te->Te.Field.name) ML_(dinfo_free)(te->Te.Field.name);
         if (te->Te.Field.nLoc > 0 && te->Te.Field.pos.loc)
            ML_(dinfo_free)(te->Te.Field.pos.loc);
         break;
      case Te_Bound:
         break;
      case Te_TyBase:
         if (te->Te.TyBase.name) ML_(dinfo_free)(te->Te.TyBase.name);
         break;
      case Te_TyPtr:
      case Te_TyRef:
      case Te_TyPtrMbr:
      case Te_TyRvalRef:
         break;
      case Te_TyTyDef:
         if (te->Te.TyTyDef.name) ML_(dinfo_free)(te->Te.TyTyDef.name);
         break;
      case Te_TyStOrUn:
         if (te->Te.TyStOrUn.name) ML_(dinfo_free)(te->Te.TyStOrUn.name);
         if (te->Te.TyStOrUn.fieldRs) VG_(deleteXA)(te->Te.TyStOrUn.fieldRs);
         break;
      case Te_TyEnum:
         if (te->Te.TyEnum.name) ML_(dinfo_free)(te->Te.TyEnum.name);
         if (te->Te.TyEnum.atomRs) VG_(deleteXA)(te->Te.TyEnum.atomRs);
         break;
      case Te_TyArray:
         if (te->Te.TyArray.boundRs) VG_(deleteXA)(te->Te.TyArray.boundRs);
         break;
      case Te_TyFn:
         break;
      case Te_TyQual:
         break;
      case Te_TyVoid:
         break;
      default:
         vg_assert(0);
   }
   /* Now clear it out and set to Te_EMPTY. */
   saved_cuOff = te->cuOff;
   VG_(memset)(te, 0, sizeof(*te));
   te->cuOff = saved_cuOff;
   te->tag = Te_EMPTY;
}


/* How big is this type?  If .b in the returned struct is False, the
   size is unknown. */

static MaybeULong mk_MaybeULong_Nothing ( void ) {
   MaybeULong mul;
   mul.ul = 0;
   mul.b  = False;
   return mul;
}
static MaybeULong mk_MaybeULong_Just ( ULong ul ) {
   MaybeULong mul;
   mul.ul = ul;
   mul.b  = True;
   return mul;
}
static MaybeULong mul_MaybeULong ( MaybeULong mul1, MaybeULong mul2 ) {
   if (!mul1.b) { vg_assert(mul1.ul == 0); return mul1; }
   if (!mul2.b) { vg_assert(mul2.ul == 0); return mul2; }
   mul1.ul *= mul2.ul;
   return mul1;
}

MaybeULong ML_(sizeOfType)( XArray* /* of TyEnt */ tyents,
                            UWord cuOff )
{
   Word       i;
   MaybeULong eszB;
   TyEnt*     ent = ML_(TyEnts__index_by_cuOff)(tyents, NULL, cuOff);
   TyEnt*     ent2;
   vg_assert(ent);
   vg_assert(ML_(TyEnt__is_type)(ent));
   switch (ent->tag) {
      case Te_TyBase:
         vg_assert(ent->Te.TyBase.szB > 0);
         return mk_MaybeULong_Just( ent->Te.TyBase.szB );
      case Te_TyQual:
         return ML_(sizeOfType)( tyents, ent->Te.TyQual.typeR );
      case Te_TyTyDef:
         ent2 = ML_(TyEnts__index_by_cuOff)(tyents, NULL,
                                            ent->Te.TyTyDef.typeR);
         vg_assert(ent2);
         if (ent2->tag == Te_UNKNOWN)
            return mk_MaybeULong_Nothing(); /*UNKNOWN*/
         return ML_(sizeOfType)( tyents, ent->Te.TyTyDef.typeR );
      case Te_TyPtr:
      case Te_TyRef:
      case Te_TyPtrMbr:
      case Te_TyRvalRef:
         vg_assert(ent->Te.TyPorR.szB == 4 || ent->Te.TyPorR.szB == 8);
         return mk_MaybeULong_Just( ent->Te.TyPorR.szB );
      case Te_TyStOrUn:
         return ent->Te.TyStOrUn.complete
                   ? mk_MaybeULong_Just( ent->Te.TyStOrUn.szB )
                   : mk_MaybeULong_Nothing();
      case Te_TyEnum:
         return mk_MaybeULong_Just( ent->Te.TyEnum.szB );
      case Te_TyArray:
         ent2 = ML_(TyEnts__index_by_cuOff)(tyents, NULL,
                                            ent->Te.TyArray.typeR);
         vg_assert(ent2);
         if (ent2->tag == Te_UNKNOWN)
            return mk_MaybeULong_Nothing(); /*UNKNOWN*/
         eszB = ML_(sizeOfType)( tyents, ent->Te.TyArray.typeR );
         for (i = 0; i < VG_(sizeXA)( ent->Te.TyArray.boundRs ); i++) {
            UWord bo_cuOff
               = *(UWord*)VG_(indexXA)(ent->Te.TyArray.boundRs, i);
            TyEnt* bo
              = ML_(TyEnts__index_by_cuOff)( tyents, NULL, bo_cuOff );
            vg_assert(bo);
            vg_assert(bo->tag == Te_Bound);
            if (!(bo->Te.Bound.knownL && bo->Te.Bound.knownU))
               return mk_MaybeULong_Nothing(); /*UNKNOWN*/
            eszB = mul_MaybeULong( 
                      eszB,
                      mk_MaybeULong_Just( (ULong)(bo->Te.Bound.boundU 
                                                  - bo->Te.Bound.boundL + 1) ));
         }
         return eszB;
      case Te_TyVoid:
         return mk_MaybeULong_Nothing(); /*UNKNOWN*/
      default:
         VG_(printf)("ML_(sizeOfType): unhandled: ");
         ML_(pp_TyEnt)(ent);
         VG_(printf)("\n");
         vg_assert(0);
   }
}


/* Describe where in the type 'offset' falls.  Caller must
   deallocate the resulting XArray. */

static void copy_UWord_into_XA ( XArray* /* of HChar */ xa,
                                 UWord uw ) {
   HChar buf[32];
   VG_(memset)(buf, 0, sizeof(buf));
   VG_(sprintf)(buf, "%lu", uw);
   VG_(addBytesToXA)( xa, buf, VG_(strlen)(buf));
}

XArray* /*HChar*/ ML_(describe_type)( /*OUT*/PtrdiffT* residual_offset,
                                      XArray* /* of TyEnt */ tyents,
                                      UWord ty_cuOff, 
                                      PtrdiffT offset )
{
   TyEnt*  ty;
   XArray* xa = VG_(newXA)( ML_(dinfo_zalloc), "di.tytypes.dt.1",
                            ML_(dinfo_free),
                            sizeof(HChar) );
   vg_assert(xa);

   ty = ML_(TyEnts__index_by_cuOff)(tyents, NULL, ty_cuOff);

   while (True) {
      vg_assert(ty);
      vg_assert(ML_(TyEnt__is_type)(ty));

      switch (ty->tag) {

         /* These are all atomic types; there is nothing useful we can
            do. */
         case Te_TyEnum:
         case Te_TyFn:
         case Te_TyVoid:
         case Te_TyPtr:
         case Te_TyRef:
         case Te_TyPtrMbr:
         case Te_TyRvalRef:
         case Te_TyBase:
            goto done;

         case Te_TyStOrUn: {
            Word       i;
            GXResult   res;
            MaybeULong mul;
            XArray*    fieldRs;
            UWord      fieldR;
            TyEnt*     field = NULL;
            PtrdiffT   offMin = 0, offMax1 = 0;
            if (!ty->Te.TyStOrUn.isStruct) goto done;
            fieldRs = ty->Te.TyStOrUn.fieldRs;
            if (((!fieldRs) || VG_(sizeXA)(fieldRs) == 0)
                && (ty->Te.TyStOrUn.typeR == 0)) goto done;
            for (i = 0; i < VG_(sizeXA)( fieldRs ); i++ ) {
               fieldR = *(UWord*)VG_(indexXA)( fieldRs, i );
               field = ML_(TyEnts__index_by_cuOff)(tyents, NULL, fieldR);
               vg_assert(field);
               vg_assert(field->tag == Te_Field);
               vg_assert(field->Te.Field.nLoc < 0
                         || (field->Te.Field.nLoc > 0
                             && field->Te.Field.pos.loc));
               if (field->Te.Field.nLoc == -1) {
                  res.kind = GXR_Addr;
                  res.word = field->Te.Field.pos.offset;
               } else {
                  /* Re data_bias in this call, we should really send in
                     a legitimate value.  But the expression is expected
                     to be a constant expression, evaluation of which
                     will not need to use DW_OP_addr and hence we can
                     avoid the trouble of plumbing the data bias through
                     to this point (if, indeed, it has any meaning; from
                     which DebugInfo would we take the data bias? */
                   res =  ML_(evaluate_Dwarf3_Expr)(
                          field->Te.Field.pos.loc, field->Te.Field.nLoc,
                          NULL/*fbGX*/, NULL/*RegSummary*/,
                          0/*data_bias*/,
                          True/*push_initial_zero*/);
                  if (0) {
                     VG_(printf)("QQQ ");
                     ML_(pp_GXResult)(res);
                     VG_(printf)("\n");
                  }
               }
               if (res.kind != GXR_Addr)
                  continue;
               mul = ML_(sizeOfType)( tyents, field->Te.Field.typeR );
               if (mul.b != True)
                  goto done; /* size of field is unknown (?!) */
               offMin  = res.word;
               offMax1 = offMin + (PtrdiffT)mul.ul;
               if (offMin == offMax1)
                  continue;
               vg_assert(offMin < offMax1);
               if (offset >= offMin && offset < offMax1)
                  break;
            }
            /* Did we find a suitable field? */
            vg_assert(i >= 0 && i <= VG_(sizeXA)( fieldRs ));
            if (i == VG_(sizeXA)( fieldRs )) {
               ty = ML_(TyEnts__index_by_cuOff)(tyents, NULL,
                                                   ty->Te.TyStOrUn.typeR);
               vg_assert(ty);
               if (ty->tag == Te_UNKNOWN) goto done;
               vg_assert(ML_(TyEnt__is_type)(ty));
               continue;
            }
            /* Yes.  'field' is it. */
            vg_assert(field);
            if (!field->Te.Field.name) goto done;
            VG_(addBytesToXA)( xa, ".", 1 );
            VG_(addBytesToXA)( xa, field->Te.Field.name,
                               VG_(strlen)(field->Te.Field.name) );
            offset -= offMin;
            ty = ML_(TyEnts__index_by_cuOff)(tyents, NULL,
                                             field->Te.Field.typeR );
            tl_assert(ty);
            if (ty->tag == Te_UNKNOWN) goto done;
            /* keep going; look inside the field. */
            break;
         }

         case Te_TyArray: {
            MaybeULong mul;
            UWord      size, eszB, ix;
            UWord      boundR;
            TyEnt*     elemTy;
            TyEnt*     bound;
            /* Just deal with the simple, common C-case: 1-D array,
               zero based, known size. */
            elemTy = ML_(TyEnts__index_by_cuOff)(tyents, NULL, 
                                                 ty->Te.TyArray.typeR);
            vg_assert(elemTy);
            if (elemTy->tag == Te_UNKNOWN) goto done;
            vg_assert(ML_(TyEnt__is_type)(elemTy));
            if (!ty->Te.TyArray.boundRs)
               goto done;
            if (VG_(sizeXA)( ty->Te.TyArray.boundRs ) != 1) goto done;
            boundR = *(UWord*)VG_(indexXA)( ty->Te.TyArray.boundRs, 0 );
            bound = ML_(TyEnts__index_by_cuOff)(tyents, NULL, boundR);
            vg_assert(bound);
            vg_assert(bound->tag == Te_Bound);
            if (!(bound->Te.Bound.knownL && bound->Te.Bound.knownU
                  && bound->Te.Bound.boundL == 0
                  && bound->Te.Bound.boundU >= bound->Te.Bound.boundL))
               goto done;
            size = bound->Te.Bound.boundU - bound->Te.Bound.boundL + 1;
            vg_assert(size >= 1);
            mul = ML_(sizeOfType)( tyents, ty->Te.TyArray.typeR );
            if (mul.b != True)
               goto done; /* size of element type not known */
            eszB = mul.ul;
            if (eszB == 0) goto done;
            ix = offset / eszB;
            VG_(addBytesToXA)( xa, "[", 1 );
            copy_UWord_into_XA( xa, ix );
            VG_(addBytesToXA)( xa, "]", 1 );
            ty = elemTy;
            offset -= ix * eszB;
            /* keep going; look inside the array element. */
            break;
         }

         case Te_TyQual: {
            ty = ML_(TyEnts__index_by_cuOff)(tyents, NULL,
                                             ty->Te.TyQual.typeR);
            tl_assert(ty);
            if (ty->tag == Te_UNKNOWN) goto done;
            break;
         }

         case Te_TyTyDef: {
            ty = ML_(TyEnts__index_by_cuOff)(tyents, NULL,
                                             ty->Te.TyTyDef.typeR);
            tl_assert(ty);
            if (ty->tag == Te_UNKNOWN) goto done;
            break;
         }

         default: {
            VG_(printf)("ML_(describe_type): unhandled: ");
            ML_(pp_TyEnt)(ty);
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
