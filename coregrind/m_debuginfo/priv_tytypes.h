
/*--------------------------------------------------------------------*/
/*--- Representation of source level types.         priv_tytypes.h ---*/
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

#ifndef __PRIV_TYTYPES_H
#define __PRIV_TYTYPES_H

#include "pub_core_basics.h"   // UWord
#include "pub_core_xarray.h"   // XArray
#include "priv_misc.h"         // MaybeULong

typedef
   enum {
      Te_EMPTY=10, /* empty (contains no info) */
      Te_INDIR,    /* indirection to some other TyEnt */
      Te_UNKNOWN,  /* denotes a unknown type/field/whatever */
      Te_Atom,     /* name & 64-bit const, iow, enumeration member */
      Te_Field,    /* struct/class field defn */
      Te_Bound,    /* array bounds indication, for one dimension */
      Te_TyBase,   /* base type */
      Te_TyPtr,    /* pointer type */
      Te_TyRef,    /* reference type */
      Te_TyPtrMbr, /* pointer to member type */
      Te_TyRvalRef,/* rvalue reference type */
      Te_TyTyDef,  /* a renaming of some other type */
      Te_TyStOrUn, /* structure or union type */
      Te_TyEnum,   /* an enum type */
      Te_TyArray,  /* an array type */
      Te_TyFn,     /* function type */
      Te_TyQual,   /* qualified type */
      Te_TyVoid    /* void type */
   }
   TyEntTag;

/* Fields ending in "R" are references to other TyEnts.  Fields ending
   in "Rs" are XArray*s of references to other TyEnts. */
typedef
   struct {
      UWord    cuOff;
      TyEntTag tag;
      union {
         struct {
         } EMPTY;
         struct {
            UWord indR;
         } INDIR;
         struct {
         } UNKNOWN;
         struct {
            HChar* name; /* in mallocville */
            Bool   valueKnown; /* atoms w/ unknown value are possible */
            Long   value;
         } Atom;
         struct {
            HChar* name;  /* in mallocville */
            UWord  typeR; /* should be Te_TyXXXX */
            union {
               UChar* loc;   /* location expr, in mallocville */
               Word offset;  /* or offset from the beginning of containing
                                entity */
            } pos;
            Word  nLoc;  /* number of bytes in .pos.loc if >= 0, or -1
                            if .pos.offset should be used instead */
            Bool   isStruct;
         } Field;
         struct {
            Bool knownL;
            Bool knownU;
            Long boundL;
            Long boundU;
         } Bound;
         struct {
            HChar* name; /* in mallocville */
            Int    szB;
            UChar  enc; /* S:signed U:unsigned F:floating C:complex float */
         } TyBase;
         struct {
            Int   szB;
            UWord typeR;
         } TyPorR;
         struct {
            HChar* name;  /* in mallocville */
            UWord  typeR; /* MAY BE D3_INVALID_CUOFF, denoting unknown */
         } TyTyDef;
         struct {
            HChar*  name; /* in mallocville */
            UWord   szB;
            UWord   typeR;
            XArray* /* of UWord */ fieldRs;
            Bool    complete;
            Bool    isStruct;
         } TyStOrUn;
         struct {
            HChar*  name; /* in mallocville */
            Int     szB;
            XArray* /* of UWord */ atomRs;
         } TyEnum;
         struct {
            UWord   typeR;
            XArray* /* of UWord */ boundRs;
         } TyArray;
         struct {
         } TyFn;
         struct {
            UChar qual; /* C:const V:volatile */
            UWord typeR;
         } TyQual;
         struct {
            Bool isFake; /* True == introduced by the reader */
         } TyVoid;
      } Te;
   }
   TyEnt;

/* Does this TyEnt denote a type, as opposed to some other kind of
   thing? */
Bool ML_(TyEnt__is_type)( TyEnt* );

/* Print a TyEnt, debug-style. */
void ML_(pp_TyEnt)( TyEnt* );

/* Print a whole XArray of TyEnts, debug-style */
void ML_(pp_TyEnts)( XArray* tyents, const HChar* who );

/* Print a TyEnt, C style, chasing stuff as necessary. */
void ML_(pp_TyEnt_C_ishly)( XArray* /* of TyEnt */ tyents,
                            UWord cuOff );

/* Generates a total ordering on TyEnts based only on their .cuOff
   fields. */
Word ML_(TyEnt__cmp_by_cuOff_only) ( const TyEnt* te1, const TyEnt* te2 );

/* Generates a total ordering on TyEnts based on everything except
   their .cuOff fields. */
Word ML_(TyEnt__cmp_by_all_except_cuOff) ( const TyEnt* te1, const TyEnt* te2 );

/* Free up all directly or indirectly heap-allocated stuff attached to
   this TyEnt, and set its tag to Te_EMPTY.  The .cuOff field is
   unchanged. */
void ML_(TyEnt__make_EMPTY) ( TyEnt* te );

/* How big is this type?  If .b in the returned struct is False, the
   size is unknown. */

MaybeULong ML_(sizeOfType)( XArray* /* of TyEnt */ tyents,
                            UWord cuOff );

/* Describe where in the type 'offset' falls.  Caller must
   deallocate the resulting XArray. */
XArray* /*UChar*/ ML_(describe_type)( /*OUT*/PtrdiffT* residual_offset,
                                      XArray* /* of TyEnt */ tyents,
                                      UWord ty_cuOff, 
                                      PtrdiffT offset );


/* A fast-lookup cache for ML_(TyEnts__index_by_cuOff).  Nothing
   particularly surprising here; it's 2 way set associative, with some
   number of ways, doesn't particularly have to be a power of 2.  In
   order to have a way to indicate an invalid entry, we set the second
   value of the pair to NULL, and keep checking for it, since
   unfortunately there's no obvious cuOff number that we could put in
   the first word of the pair that could indicate an invalid entry.

   4096 arrived at as the best value for an E6600 loading Qt-4.4.1
   Designer and all associated libraries, compiled by gcc-4.3.1, 
   -g -O, 64-bit, which is at least a moderately good stress test,
   with the largest library being about 150MB.*/

#define N_TYENT_INDEX_CACHE 4096

typedef
   struct {
      struct { UWord cuOff0; TyEnt* ent0; 
               UWord cuOff1; TyEnt* ent1; }
         ce[N_TYENT_INDEX_CACHE];
   }
   TyEntIndexCache;

void ML_(TyEntIndexCache__invalidate) ( TyEntIndexCache* cache );

/* 'ents' is an XArray of TyEnts, sorted by their .cuOff fields.  Find
   the entry which has .cuOff field as specified.  Returns NULL if not
   found.  Asserts if more than one entry has the specified .cuOff
   value. */
TyEnt* ML_(TyEnts__index_by_cuOff) ( XArray* /* of TyEnt */ ents,
                                     TyEntIndexCache* cache,
                                     UWord cuOff_to_find );

#endif /* ndef __PRIV_TYTYPES_H */

/*--------------------------------------------------------------------*/
/*--- end                                           priv_tytypes.h ---*/
/*--------------------------------------------------------------------*/
