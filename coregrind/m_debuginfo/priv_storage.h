
/*--------------------------------------------------------------------*/
/*--- Format-neutral storage of and querying of info acquired from ---*/
/*--- ELF/XCOFF stabs/dwarf1/dwarf2 debug info.                    ---*/
/*---                                               priv_storage.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Julian Seward 
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
/*
   Stabs reader greatly improved by Nick Nethercote, Apr 02.
   This module was also extensively hacked on by Jeremy Fitzhardinge
   and Tom Hughes.
*/
/* See comment at top of debuginfo.c for explanation of
   the _svma / _avma / _image / _bias naming scheme.
*/
/* Note this is not freestanding; needs pub_core_xarray.h to be
   included before it. */

#ifndef __PRIV_STORAGE_H
#define __PRIV_STORAGE_H

/* --------------------- SYMBOLS --------------------- */

/* A structure to hold an ELF/XCOFF symbol (very crudely). */
typedef 
   struct { 
      Addr  addr;   /* lowest address of entity */
      Addr  tocptr; /* ppc64-linux only: value that R2 should have */
      UInt  size;   /* size in bytes */
      UChar *name;  /* name */
   }
   DiSym;

/* --------------------- SRCLOCS --------------------- */

/* Line count at which overflow happens, due to line numbers being
   stored as shorts in `struct nlist' in a.out.h. */
#define LINENO_OVERFLOW (1 << (sizeof(short) * 8))

#define LINENO_BITS     20
#define LOC_SIZE_BITS  (32 - LINENO_BITS)
#define MAX_LINENO     ((1 << LINENO_BITS) - 1)

/* Unlikely to have any lines with instruction ranges > 4096 bytes */
#define MAX_LOC_SIZE   ((1 << LOC_SIZE_BITS) - 1)

/* Number used to detect line number overflows; if one line is
   60000-odd smaller than the previous, is was probably an overflow.
 */
#define OVERFLOW_DIFFERENCE     (LINENO_OVERFLOW - 5000)

/* A structure to hold addr-to-source info for a single line.  There
  can be a lot of these, hence the dense packing. */
typedef
   struct {
      /* Word 1 */
      Addr   addr;               /* lowest address for this line */
      /* Word 2 */
      UShort size:LOC_SIZE_BITS; /* # bytes; we catch overflows of this */
      UInt   lineno:LINENO_BITS; /* source line number, or zero */
      /* Word 3 */
      UChar*  filename;          /* source filename */
      /* Word 4 */
      UChar*  dirname;           /* source directory name */
   }
   DiLoc;

/* --------------------- CF INFO --------------------- */

/* A structure to summarise DWARF2/3 CFA info for the code address
   range [base .. base+len-1].  In short, if you know (sp,fp,ip) at
   some point and ip is in the range [base .. base+len-1], it tells
   you how to calculate (sp,fp) for the caller of the current frame
   and also ra, the return address of the current frame.

   First off, calculate CFA, the Canonical Frame Address, thusly:

     cfa = case cfa_how of
              CFIC_SPREL -> sp + cfa_off
              CFIC_FPREL -> fp + cfa_off
              CFIR_EXPR  -> expr whose index is in cfa_off

   Once that is done, the previous frame's sp/fp values and this
   frame's ra value can be calculated like this:

      old_sp/fp/ra
         = case sp/fp/ra_how of
              CFIR_UNKNOWN   -> we don't know, sorry
              CFIR_SAME      -> same as it was before (sp/fp only)
              CFIR_CFAREL    -> cfa + sp/fp/ra_off
              CFIR_MEMCFAREL -> *( cfa + sp/fp/ra_off )
              CFIR_EXPR      -> expr whose index is in sp/fp/ra_off
*/

#define CFIC_SPREL     ((UChar)1)
#define CFIC_FPREL     ((UChar)2)
#define CFIC_EXPR      ((UChar)3)

#define CFIR_UNKNOWN   ((UChar)4)
#define CFIR_SAME      ((UChar)5)
#define CFIR_CFAREL    ((UChar)6)
#define CFIR_MEMCFAREL ((UChar)7)
#define CFIR_EXPR      ((UChar)8)

typedef
   struct {
      Addr  base;
      UInt  len;
      UChar cfa_how; /* a CFIC_ value */
      UChar ra_how;  /* a CFIR_ value */
      UChar sp_how;  /* a CFIR_ value */
      UChar fp_how;  /* a CFIR_ value */
      Int   cfa_off;
      Int   ra_off;
      Int   sp_off;
      Int   fp_off;
   }
   DiCfSI;


typedef
   enum {
      Cop_Add=0x321,
      Cop_Sub,
      Cop_And,
      Cop_Mul
   }
   CfiOp;

typedef
   enum {
      Creg_SP=0x213,
      Creg_FP,
      Creg_IP
   }
   CfiReg;

typedef
   enum {
      Cex_Undef=0x123,
      Cex_Deref,
      Cex_Const,
      Cex_Binop,
      Cex_CfiReg,
      Cex_DwReg
   }
   CfiExprTag;

typedef 
   struct {
      CfiExprTag tag;
      union {
         struct {
         } Undef;
         struct {
            Int ixAddr;
         } Deref;
         struct {
            UWord con;
         } Const;
         struct {
            CfiOp op;
            Int ixL;
            Int ixR;
         } Binop;
         struct {
            CfiReg reg;
         } CfiReg;
         struct {
            Int reg;
         } DwReg;
      }
      Cex;
   }
   CfiExpr;

extern Int ML_(CfiExpr_Undef) ( XArray* dst );
extern Int ML_(CfiExpr_Deref) ( XArray* dst, Int ixAddr );
extern Int ML_(CfiExpr_Const) ( XArray* dst, UWord con );
extern Int ML_(CfiExpr_Binop) ( XArray* dst, CfiOp op, Int ixL, Int ixR );
extern Int ML_(CfiExpr_CfiReg)( XArray* dst, CfiReg reg );
extern Int ML_(CfiExpr_DwReg) ( XArray* dst, Int reg );

extern void ML_(ppCfiExpr)( XArray* src, Int ix );

/* --------------------- SEGINFO --------------------- */

/* This is the top-level data type.  It's a structure which contains
   information pertaining to one mapped text segment.  This type is
   exported only abstractly - in pub_tool_debuginfo.h. */

#define SEGINFO_STRCHUNKSIZE (64*1024)

struct _SegInfo {
   struct _SegInfo* next;	/* list of SegInfos */

   /* Description of the mapped segment. */
   Addr   text_start_avma;
   UInt   text_size;
   UChar* filename; /* in mallocville */
   UChar* memname;  /* malloc'd.  AIX5 only: .a member name */
   OffT   foffset;  /* file offset for mapped text section - UNUSED */
   UChar* soname;

   /* An expandable array of symbols. */
   DiSym*  symtab;
   UInt    symtab_used;
   UInt    symtab_size;
   /* An expandable array of locations. */
   DiLoc*  loctab;
   UInt    loctab_used;
   UInt    loctab_size;
   /* An expandable array of CFI summary info records.  Also includes
      summary address bounds, showing the min and max address covered
      by any of the records, as an aid to fast searching.  And, if the
      records require any expression nodes, they are stored in
      cfsi_exprs. */
   DiCfSI* cfsi;
   UInt    cfsi_used;
   UInt    cfsi_size;
   Addr    cfsi_minaddr;
   Addr    cfsi_maxaddr;
   XArray* cfsi_exprs; /* XArray of CfSiExpr */

   /* Expandable arrays of characters -- the string table.  Pointers
      into this are stable (the arrays are not reallocated). */
   struct strchunk {
      UInt   strtab_used;
      struct strchunk *next;
      UChar  strtab[SEGINFO_STRCHUNKSIZE];
   } *strchunks;

   /* 'text_bias' is what needs to be added to an address in the
      address space of the library as stored on disk [a so-called
      stated VMA] (which is not 0-based for executables or prelinked
      libraries) to get an address in memory for the object loaded at
      'text_start_avma'.  At least for text symbols. */
   OffT   text_bias;

   /* Bounds of data, BSS, PLT, GOT and OPD (for ppc64-linux) so that
      tools can see what section an address is in.  In the running
      image! */
   Addr	  plt_start_avma;
   UInt   plt_size;
   Addr   got_start_avma;
   UInt   got_size;
   Addr   opd_start_avma;
   UInt   opd_size;
   Addr   data_start_avma;
   UInt   data_size;
   Addr   bss_start_avma;
   UInt   bss_size;

   /* Used for debugging only - indicate what stuff to dump whilst
      reading stuff into the seginfo.  Are computed as early in the
      lifetime of the SegInfo as possible.  Use these when deciding
      what to spew out; do not use the global VG_(clo_blah) flags. */
   Bool trace_symtab; /* symbols, our style */
   Bool trace_cfi;    /* dwarf frame unwind, our style */
   Bool ddump_syms;   /* mimic /usr/bin/readelf --syms */
   Bool ddump_line;   /* mimic /usr/bin/readelf --debug-dump=line */
   Bool ddump_frames; /* mimic /usr/bin/readelf --debug-dump=frames */
};

/* --------------------- functions --------------------- */

/* ------ Adding ------ */

/* Add a symbol to si's symbol table. */
extern void ML_(addSym) ( struct _SegInfo* si, DiSym* sym );

/* Add a line-number record to a SegInfo. */
extern
void ML_(addLineInfo) ( struct _SegInfo* si, 
                        UChar*   filename, 
                        UChar*   dirname,  /* NULL is allowable */
                        Addr this, Addr next, Int lineno, Int entry);

/* Add a CFI summary record.  The supplied DiCfSI is copied. */
extern void ML_(addDiCfSI) ( struct _SegInfo* si, DiCfSI* cfsi );

/* Add a string to the string table of a SegInfo.  If len==-1,
   ML_(addStr) will itself measure the length of the string. */
extern UChar* ML_(addStr) ( struct _SegInfo* si, UChar* str, Int len );

/* Canonicalise the tables held by 'si', in preparation for use.  Call
   this after finishing adding entries to these tables. */
extern void ML_(canonicaliseTables) ( struct _SegInfo* si );

/* ------ Searching ------ */

/* Find a symbol-table index containing the specified pointer, or -1
   if not found.  Binary search.  */
extern Int ML_(search_one_symtab) ( struct _SegInfo* si, Addr ptr,
                                    Bool match_anywhere_in_fun );

/* Find a location-table index containing the specified pointer, or -1
   if not found.  Binary search.  */
extern Int ML_(search_one_loctab) ( struct _SegInfo* si, Addr ptr );

/* Find a CFI-table index containing the specified pointer, or -1 if
   not found.  Binary search.  */
extern Int ML_(search_one_cfitab) ( struct _SegInfo* si, Addr ptr );

/* ------ Misc ------ */

/* Show a non-fatal debug info reading error.  Use vg_panic if
   terminal. */
extern void ML_(symerr) ( HChar* msg );

/* Print a symbol. */
extern void ML_(ppSym) ( Int idx, DiSym* sym );

/* Print a call-frame-info summary. */
extern void ML_(ppDiCfSI) ( XArray* /* of CfiExpr */ exprs, DiCfSI* si );


#define TRACE_SYMTAB(format, args...) \
   if (si->trace_symtab) { VG_(printf)(format, ## args); }


#endif /* ndef __PRIV_STORAGE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
