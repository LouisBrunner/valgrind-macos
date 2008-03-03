
/*--------------------------------------------------------------------*/
/*--- Representation of source level types.         priv_tytypes.h ---*/
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

#ifndef __PRIV_TYTYPES_H
#define __PRIV_TYTYPES_H

typedef  struct _TyAdmin   TyAdmin;
typedef  struct _TyAtom    TyAtom;
typedef  struct _TyField   TyField;
typedef  struct _TyBounds  TyBounds;
typedef  struct _D3Expr    D3Expr;
typedef  struct _Type      Type;

#define TyBounds_MAGIC 0x0d573990UL

typedef
   enum { TyA_Atom=10, TyA_Field, TyA_Bounds, TyA_Expr, TyA_Type } 
   TyAdminTag;

struct _TyAdmin {
   UWord      cuOff;
   void*      payload;
   TyAdmin*   next;
   TyAdminTag tag;
};

/* an enumeration value */
struct _TyAtom {
   UChar* name; /* in DebugInfo.strchunks */
   Long   value;
};

struct _TyField {
   UChar*  name; /* in DebugInfo.strchunks */
   Type*   typeR;
   D3Expr* loc;
   Bool    isStruct;
};

struct _TyBounds {
   UInt magic;
   Bool knownL;
   Bool knownU;
   Long boundL;
   Long boundU;
};

struct _D3Expr {
   UChar* bytes; /* in DebugInfo.strchunks */
   UWord  nbytes;
};

struct _Type {
   enum { Ty_Base=30, Ty_PorR, Ty_TyDef, Ty_StOrUn, 
          Ty_Enum, Ty_Array, Ty_Fn, Ty_Qual, Ty_Void } tag;
   union {
      struct {
         UChar* name; /* in DebugInfo.strchunks */
         Int    szB;
         UChar  enc; /* S:signed U:unsigned F:floating C:complex float */
      } Base;
      struct {
         Int   szB;
         Type* typeR;
         Bool  isPtr;
      } PorR;
      struct {
         UChar* name;  /* in DebugInfo.strchunks */
         Type*  typeR; /* MAY BE NULL, denoting unknown */
      } TyDef;
      struct {
         UChar*  name; /* in DebugInfo.strchunks */
         UWord   szB;
         XArray* /* of TyField* */ fields;
         Bool    complete;
         Bool    isStruct;
      } StOrUn;
      struct {
         UChar*  name; /* in DebugInfo.strchunks */
         Int     szB;
         XArray* /* of TyAtom* */ atomRs;
      } Enum;
      struct {
         Type*   typeR;
         XArray* /* of TyBounds* */ bounds;
      } Array;
      struct {
      } Fn;
      struct {
         UChar qual; /* C:const V:volatile */
         Type* typeR;
      } Qual;
      struct {
         Bool isFake; /* True == introduced by the reader */
      } Void;
   } Ty;
};

TyAdmin*  ML_(new_TyAdmin)  ( UWord cuOff, TyAdmin* next );
TyAtom*   ML_(new_TyAtom)   ( UChar* name, Long value );
TyField*  ML_(new_TyField)  ( UChar* name, Type* typeR, D3Expr* loc );
TyBounds* ML_(new_TyBounds) ( void );
Type*     ML_(new_Type)     ( void );
D3Expr*   ML_(new_D3Expr)   ( UChar* bytes, UWord nbytes );

void ML_(delete_TyAdmin_and_payload) ( TyAdmin* ad );

void ML_(pp_TyAdmin)  ( TyAdmin* admin );
void ML_(pp_TyAtom)   ( TyAtom* atom );
void ML_(pp_TyField)  ( TyField* field );
void ML_(pp_TyBounds) ( TyBounds* bounds );
void ML_(pp_Type)     ( Type* ty );
void ML_(pp_D3Expr)   ( D3Expr* expr );

/* NOTE: this assumes that the types have all been 'resolved' (that
   is, inter-type references expressed as .debug_info offsets have
   been converted into pointers) */
void ML_(pp_Type_C_ishly) ( Type* ty );

/* How big is this type?  (post-resolved only)  If .b in the
   returned struct is False, the size is unknown. */
/* FIXME: check all pointers before dereferencing */

typedef struct { UWord w; Bool b; } MaybeUWord;

MaybeUWord ML_(sizeOfType)( Type* ty );

/* Describe where in the type 'offset' falls.  Caller must
   deallocate the resulting XArray. */
XArray* /*UChar*/ ML_(describe_type)( /*OUT*/OffT* residual_offset,
                                      Type* ty, OffT offset );


#endif /* ndef __PRIV_TYTYPES_H */

/*--------------------------------------------------------------------*/
/*--- end                                           priv_tytypes.h ---*/
/*--------------------------------------------------------------------*/
