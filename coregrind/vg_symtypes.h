/*--------------------------------------------------------------------*/
/*--- Intra-Valgrind interfaces for vg_symtypes.c.   vg_symtypes.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

#ifndef __VG_SYMTYPES_H
#define __VG_SYMTYPES_H

/* Lets try to make these opaque */
typedef struct _SymType SymType;

/* ------------------------------------------------------------
   Constructors for various SymType nodes
   ------------------------------------------------------------ */

/* Find the basetype for a given type: that is, if type is a typedef,
   return the typedef'd type.  If resolve is true, it will resolve
   unresolved symbols.  If type is not a typedef, then this is just
   returns type.
*/
SymType *VG_(st_basetype)(SymType *type, Bool resolve);

void VG_(st_setname)(SymType *ty, Char *name);

typedef void (SymResolver)(SymType *, void *);

/* Create an unresolved type */
SymType *VG_(st_mkunresolved)(SymType *, SymResolver *resolve, void *data);

/* update an unresolved type's data */
void VG_(st_unresolved_setdata)(SymType *, SymResolver *resolve, void *data);

Bool VG_(st_isresolved)(SymType *);
UInt VG_(st_sizeof)(SymType *);

/* Unknown type (unparsable) */
SymType *VG_(st_mkunknown)(SymType *);

SymType *VG_(st_mkvoid)(SymType *);

SymType *VG_(st_mkint)(SymType *, UInt size, Bool isSigned);
SymType *VG_(st_mkbool)(SymType *, UInt size);
SymType *VG_(st_mkchar)(SymType *, Bool isSigned);
SymType *VG_(st_mkfloat)(SymType *, UInt size);
SymType *VG_(st_mkdouble)(SymType *, UInt size);

SymType *VG_(st_mkpointer)(SymType *, SymType *);
SymType *VG_(st_mkrange)(SymType *, SymType *, Int min, Int max);

SymType *VG_(st_mkstruct)(SymType *, UInt size, UInt nfields);
SymType *VG_(st_mkunion)(SymType *, UInt size, UInt nfields);
void VG_(st_addfield)(SymType *, Char *name, SymType *, UInt off, UInt size);

SymType *VG_(st_mkenum)(SymType *, UInt ntags);
SymType *VG_(st_addtag)(SymType *, Char *name, Int val);

SymType *VG_(st_mkarray)(SymType *, SymType *idxtype, SymType *artype);

SymType *VG_(st_mktypedef)(SymType *, Char *name, SymType *type);

Bool VG_(st_isstruct)(SymType *);
Bool VG_(st_isunion)(SymType *);
Bool VG_(st_isenum)(SymType *);

/* ------------------------------------------------------------
   Interface with vg_symtab2.c
   ------------------------------------------------------------ */

/* Typed value */
typedef struct _Variable Variable;

struct _Variable {
   Char		*name;		/* name */
   SymType	*type;		/* type of value */
   Addr		valuep;		/* pointer to value */
   UInt		size;		/* size of value */
   UInt		distance;	/* "distance" from site of interest */
   Variable	*next;
   Variable	*container;
};

Variable *VG_(get_scope_variables)(ThreadId tid);

#endif /* VG_SYMTYPES_H */

/*--------------------------------------------------------------------*/
/*--- end                                            vg_symtypes.h ---*/
/*--------------------------------------------------------------------*/
