/*--------------------------------------------------------------------*/
/*--- Header for symbol table stuff.                  vg_symtab2.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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

#ifndef _VG_SYMTYPE_H
#define _VG_SYMTYPE_H

#include "vg_symtypes.h"

/* A structure to hold an ELF symbol (very crudely). */
typedef 
   struct { 
      Addr addr;   /* lowest address of entity */
      UInt size;   /* size in bytes */
      Char *name;  /* name */
   }
   RiSym;

/* Line count at which overflow happens, due to line numbers being stored as
 * shorts in `struct nlist' in a.out.h. */
#define LINENO_OVERFLOW (1 << (sizeof(short) * 8))

#define LINENO_BITS     20
#define LOC_SIZE_BITS  (32 - LINENO_BITS)
#define MAX_LINENO     ((1 << LINENO_BITS) - 1)

/* Unlikely to have any lines with instruction ranges > 4096 bytes */
#define MAX_LOC_SIZE   ((1 << LOC_SIZE_BITS) - 1)

/* Number used to detect line number overflows;  if one line is 60000-odd
 * smaller than the previous, is was probably an overflow.  
 */
#define OVERFLOW_DIFFERENCE     (LINENO_OVERFLOW - 5000)

/* A structure to hold addr-to-source info for a single line.  There can be a
 * lot of these, hence the dense packing. */
typedef
   struct {
      /* Word 1 */
      Addr   addr;                  /* lowest address for this line */
      /* Word 2 */
      UShort size:LOC_SIZE_BITS;    /* byte size; we catch overflows of this */
      UInt   lineno:LINENO_BITS;    /* source line number, or zero */
      /* Word 3 */
      Char*  filename;                /* source filename */
   }
   RiLoc;


/* A structure to hold a set of variables in a particular scope */
typedef struct _Scope Scope;	/* a set of symbols in one scope */
typedef struct _Sym Sym;	/* a single symbol */
typedef struct _ScopeRange ScopeRange; /* a range of code addreses a scope covers */

typedef enum {
      SyESPrel,			/* on the stack (relative to ESP) */
      SyEBPrel,			/* on the stack (relative to EBP) */
      SyReg,			/* in a register */
      SyType,			/* a type definition */
      SyStatic,			/* a static variable */
      SyGlobal,			/* a global variable (XXX any different to static
				   in an outer scope?) */
} SyKind;

struct _Sym {
   SymType	*type;		/* type */
   Char		*name;		/* name */
   SyKind	kind;		/* kind of symbol */

   /* a value, depending on kind */
   union {
      Int	offset;		/* offset on stack (-ve -> ebp; +ve -> esp) */
      Int	regno;		/* register number */
      Addr	addr;		/* static or global address */
   } u;
};

struct _Scope {
   Scope	*outer;		/* outer (containing) scope */
   UInt		nsyms;		/* number of symbols in this scope */
   UInt		depth;		/* depth of scope */
   Sym	        *syms;		/* the symbols */
};

/* A structure to map a scope to a range of code addresses; scopes may
   be broken into multiple ranges (before and after a nested scope) */
struct _ScopeRange {
   Addr		addr;			/* start address of this scope */
   Int		size;			/* length of scope */
   Scope       *scope;			/* symbols in scope */
};

#define STRCHUNKSIZE	(64*1024)

/* A structure which contains information pertaining to one mapped
   text segment. (typedef in vg_skin.h) */
struct _SegInfo {
   struct _SegInfo* next;	/* list of SegInfos */

   Segment	*seg;		/* first segment we're mapped out of */
   Int		ref;

   /* Description of the mapped segment. */
   Addr   start;
   UInt   size;
   Char*  filename; /* in mallocville */
   UInt   foffset;
   Char*  soname;

   /* An expandable array of symbols. */
   RiSym* symtab;
   UInt   symtab_used;
   UInt   symtab_size;
   /* An expandable array of locations. */
   RiLoc* loctab;
   UInt   loctab_used;
   UInt   loctab_size;
   /* An expandable array of scope ranges. */
   ScopeRange *scopetab;
   UInt        scopetab_used;
   UInt        scopetab_size;

   /* Expandable arrays of characters -- the string table.
      Pointers into this are stable (the arrays are not reallocated)
    */
   struct strchunk {
      UInt   strtab_used;
      struct strchunk *next;
      Char   strtab[STRCHUNKSIZE];
   }      *strchunks;

   /* offset    is what we need to add to symbol table entries
      to get the real location of that symbol in memory.
   */
   UInt   offset;

   /* Bounds of data, BSS, PLT and GOT, so that skins can see what
      section an address is in */
   Addr	  plt_start;
   UInt   plt_size;
   Addr   got_start;
   UInt   got_size;
   Addr   data_start;
   UInt   data_size;
   Addr   bss_start;
   UInt   bss_size;

   /* data used by stabs parser */
   struct _StabTypeTab	*stab_typetab;
};

Char *VG_(addStr) ( SegInfo* si, Char* str, Int len );
void VG_(addScopeInfo) ( SegInfo* si, Addr this, Addr next, Scope *scope);
void VG_(addLineInfo) ( SegInfo* si, Char* filename, Addr this, Addr next, Int lineno, Int entry);

/* Non-fatal -- use vg_panic if terminal. */
void VG_(symerr) ( Char* msg );

/* --------------------
   Stabs reader
   -------------------- */
void VG_(read_debuginfo_stabs) ( SegInfo* si,
				 UChar* stabC,   Int stab_sz, 
				 UChar* stabstr, Int stabstr_sz );

/* --------------------
   DWARF2 reader
   -------------------- */
void VG_(read_debuginfo_dwarf2) ( SegInfo* si, 
                                  UChar* dwarf2, Int dwarf2_sz );

/* --------------------
   DWARF1 reader
   -------------------- */
void VG_(read_debuginfo_dwarf1) ( SegInfo* si, 
                                  UChar* dwarf1d, Int dwarf1d_sz, 
                                  UChar* dwarf1l, Int dwarf1l_sz );


#endif /* _VG_SYMTYPE_H */

/*--------------------------------------------------------------------*/
/*--- end                                             vg_symtab2.h ---*/
/*--------------------------------------------------------------------*/
