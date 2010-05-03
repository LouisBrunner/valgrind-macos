
/*--------------------------------------------------------------------*/
/*--- Read XCOFF debug info.                           readxcoff.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2010 OpenWorks LLP
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

#if defined(VGO_aix5)

/* This file reads XCOFF symbol tables and debug info.
   Known limitations:

   * only one text section per object file is handled

   * C_BINCL/C_EINCL handling is wrong, so functions defined in files
     included from other files will end up with the wrong file name
     and possibly line numbers.  Fixable.

   * The line number reader leans heavily on the fact that the generic
     line number canonicaliser in storage.c truncates overlapping
     ranges.
*/

#include "pub_core_basics.h"
#include "pub_core_vki.h"          /* struct vki_stat et al */
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcfile.h"     /* stat, open, close */
#include "pub_core_aspacemgr.h"    /* for mmaping debuginfo files */
#include "pub_core_options.h"      /* VG_(clo_trace_symtab) */
#include "pub_core_xarray.h"
#include "priv_misc.h"
#include "priv_tytypes.h"
#include "pub_tool_debuginfo.h"
#include "priv_d3basics.h"
#include "priv_storage.h"
#include "priv_readxcoff.h"        /* self */

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#if defined(VGP_ppc32_aix5)
# define __XCOFF32__ 1
# undef  __XCOFF64__
#elif defined(VGP_ppc64_aix5)
# define __XCOFF64__ 1
# undef  __XCOFF32__
#else
# error "This file should only be compiled on AIX"
#endif
#include <xcoff.h>

#undef __AR_SMALL__
#define __AR_BIG__ 1
#include <ar.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

/* Debug stuff */
#define SHOW_LD_STRTAB 1   /* loader string tables */
#define SHOW_LD_SYMTAB 1   /* loader symbol table */
#define SHOW_LD_RELTAB 1   /* loader reloc table */
#define SHOW_STRTAB 1      /* main string table */
#define SHOW_SYMS_P1 1     /* P1: find text sym starts */
#define SHOW_SYMS_P2 1     /* P2: find text sym ends */
#define SHOW_SYMS_P3 1     /* P3: src filenames & fn start/end line #s */
#define SHOW_SYMS_P4 1     /* P4: line numbers */
#define SHOW_SYMS_P5 1     /* P5: find TOC pointers */
#define SHOW_SYMS_P6 1     /* P6: finalise symbol info */

#define SHOW_AR_DETAILS 0  /* show details of .a file internals */

#define SHOW  di->trace_symtab

/* A small stack of filenames is maintained for dealing
   with BINCL/EINCL symbol table entries. */

#define N_FILENAME_STACK 16

/* Phase 5 (find TOC pointers) has two implementations, the official
   version, which involves reading the data segment symbols, and the
   kludgey version, which basically scans the (actual loaded) data
   segment to find structs which look like function descriptors. */

#if 1
# undef OFFICIAL_PHASE5
#else
# define OFFICIAL_PHASE5 1
#endif

/*------------------------------------------------------------*/
/*--- Read XCOFF format debug info.                        ---*/
/*------------------------------------------------------------*/


/* COFF uses a strange way to represent symbol names.  A symbol is an
   eight-byte field.

   In 32-bit mode: if the first four bytes are zero, then the second
   four bytes give the offset into the string table where the string
   really is.  Otherwise, the whole 8-byte thing is itself the name.

   In 64-bit mode: a four-byte field at offset 8 is always interpreted
   as an offset into the string table.

   For a symbol of length 8, in 32-bit mode, there is no obvious way
   to zero-terminate it.  One solution is to copy the name into
   dynamically allocated memory, but that complicates storage
   management.

   An alternative solution, used here, is to represent a name as a
   (data, length) pair instead of the traditional zero-terminated
   string.  Such a pair can be constructed for any XCOFF symbol name,
   and has the advantages that (1) no dynamic memory is required, and
   (2) the name is guaranteed to be accessible as long as the object
   image is mapped in.

   What the .vec points at must not be modified; if you want to do
   that, copy it elsewhere first.
*/

typedef
   struct {
      UChar* vec; /* the text of the name */
      UInt   len; /* length of the text */
   }
   Name;

static Name maybeDerefStrTab( SYMENT* sym,
                              UChar* oi_strtab, UWord oi_n_strtab)
{
   Name res;
   static UChar* bogus 
      = (UChar*)"**_Error_Dereferencing_COFF_String_Table_**";
   UChar* bytes = (UChar*)sym;

#  if defined(VGP_ppc32_aix5)
   if (bytes[0]==0 && bytes[1]==0 && bytes[2]==0 && bytes[3]==0) {
      UInt off = *(UInt*)&bytes[4];
      if (oi_strtab && oi_n_strtab > 0 && off < oi_n_strtab) {
         res.vec = &oi_strtab[off];
         res.len = VG_(strlen)(res.vec);
         return res;
      } else
         goto bad;
   } else {
      Int i;
      res.vec = bytes;
      res.len = 8;
      for (i = 0; i < 8; i++)
         if (bytes[i] == 0)
            res.len--;
      return res;
   }

#  elif defined(VGP_ppc64_aix5)
   ULong off = (ULong)( *(UInt*)&bytes[8] );
   if (oi_strtab && oi_n_strtab > 0 && off < oi_n_strtab) {
      res.vec = &oi_strtab[off];
      res.len = VG_(strlen)(res.vec);
      return res;
   } else
      goto bad;

#  else
#    error "Unknown platform"
#  endif

  bad:
   res.vec = bogus;
   res.len = VG_(strlen)(bogus);
   return res;
}


/* Similar scheme for extracting names from C_FILE auxiliary entries,
   except that the 32-bit scheme appears to be always used, even for
   XCOFF64. */

static Name maybeDerefStrTab_fname ( UChar* bytes,
                                     UChar* oi_strtab, UWord oi_n_strtab)
{
   Name res;
   static UChar* bogus 
      = (UChar*)"**_Error_Dereferencing_COFF_String_Table_**";

   if (bytes[0]==0 && bytes[1]==0 && bytes[2]==0 && bytes[3]==0) {
      UInt off = *(UInt*)&bytes[4];
      if (oi_strtab && oi_n_strtab > 0 && off < oi_n_strtab) {
         res.vec = &oi_strtab[off];
         res.len = VG_(strlen)(res.vec);
         return res;
      } else
         goto bad;
   } else {
      Int i;
      res.vec = bytes;
      res.len = 8;
      for (i = 0; i < 8; i++)
         if (bytes[i] == 0)
            res.len--;
      return res;
   }

  bad:
   res.vec = bogus;
   res.len = VG_(strlen)(bogus);
   return res;
}


static Name mk_const_Name ( HChar* str )
{
   Name res;
   res.vec = str;
   res.len = VG_(strlen)(res.vec);
   return res;
}

static Name mk_empty_Name ( void )
{
   Name res;
   res.vec = "";
   res.len = 0;
   return res;
}

static Bool is_empty_Name ( Name name )
{
   return name.len == 0;
}

static Bool eq_string_Name ( Name name, UChar* str )
{
   UInt i;
   for (i = 0; i < name.len; i++) {
      if (str[i] == 0)
         return False;
      if (str[i] != name.vec[i])
         return False;
   }
   if (str[name.len] == 0)
      return True;
   else
      return False;
}

static Int cmp_Names ( Name n1, Name n2 )
{
   UInt i = 0;
   while (1) {
      vg_assert(i >= 0 && i <= n1.len);
      vg_assert(i >= 0 && i <= n2.len);
      if (i == n1.len && i == n2.len)
         return 0;
      if (i == n1.len && i < n2.len)
         return -1;
      if (i < n1.len && i == n2.len)
         return 1;
      if (n1.vec[i] < n2.vec[i])
         return -1;
      if (n1.vec[i] > n2.vec[i])
         return 1;
      i++;
   }
}

static void print_Name ( Name name )
{
   UInt i;
   for (i = 0; i < name.len; i++)
      VG_(printf)("%c", name.vec[i]);
}


static UChar sanitiseChar ( UChar c )
{
   if (c < 32 || c > 127)
      c = '?';
   return c;
}

static HChar* name_of_filhdr_f_magic ( Int magic )
{
   switch (magic) {
      case 0x01DF: return "xcoff32";
      case 0x01EF: return "xcoff64-upto-aix43";
      case 0x01F7: return "xcoff64-from-aix51";
      default:     return "unknown-xcoff-header-magic";
   }
}

static HChar* name_of_scnhdr_s_flags ( Int flags )
{
   switch (flags & 0xFFFF) {
      case STYP_REG:    return "\"regular\"";
      case STYP_PAD:    return "\"padding\"";
      case STYP_TEXT:   return "text only";
      case STYP_DATA:   return "data only";
      case STYP_BSS:    return "bss only";
      case STYP_EXCEPT: return "Exception";
      case STYP_INFO:   return "Comment";
      case STYP_LOADER: return "Loader";
      case STYP_DEBUG:  return "Debug";
      case STYP_TYPCHK: return "Typecheck";
      case STYP_OVRFLO: return "Overflow";
      default: return "unknown-section-header-name";
   }
}

static HChar* name_of_syment_n_sclass ( Int sclass )
{
   static HChar buf[10];
   switch (sclass) {
      /* dbx ones (>= 0x80) */
      case C_GSYM:    return "gsym";
      case C_LSYM:    return "lsym";
      case C_PSYM:    return "psym";
      case C_RSYM:    return "rsym";
      case C_RPSYM:   return "rpsym";
      case C_STSYM:   return "stsym";
      case C_DECL:    return "decl";
      case C_FUN:     return "fun";
      case C_BSTAT:   return "bstat";
      case C_ESTAT:   return "estat";
      /* non-dbx ones (< 0x80) */
      case C_STAT:    return "STAT";
      case C_FILE:    return "FILE";
      case C_HIDEXT:  return "HIDEXT";
      case C_EXT:     return "EXT";
      case C_FCN:     return "FCN";
      case C_BINCL:   return "BINCL";
      case C_EINCL:   return "EINCL";
      case C_BLOCK:   return "BLOCK";
      case C_WEAKEXT: return "WEAKEXT";
      default:
         VG_(sprintf)(buf, "??%d??", sclass);
         return buf;
   }
}

typedef 
   struct {
      Name   name;    /* symbol's name */
      Addr   first;   /* first address; always known */
      Addr   last;    /* last address; may be an overestimate */

      Name   fname;   /* source file name, if known */
      Int    slnno;   /* starting line #, or 0 if unknown */
      Int    elnno;   /* ending line #, or 0 if unknown */

      UWord  r2value; /* what r2 should be for this fn (tocptr) */
      Bool   r2known; /* do we have a r2 value? */
   } 
   XCoffSym;

static void init_XCoffSym( XCoffSym* sym )
{
   sym->name    = mk_empty_Name();
   sym->first   = 0;
   sym->last    = 0;
   sym->fname   = mk_empty_Name();
   sym->slnno   = 0;
   sym->elnno   = 0;
   sym->r2known = False;
   sym->r2value = False;
}

/* Compare XCoffSyms by their start address. */
static Int cmp_XCoffSym_by_start ( void* v1, void* v2 )
{
   XCoffSym* s1 = (XCoffSym*)v1;
   XCoffSym* s2 = (XCoffSym*)v2;
   if (s1->first < s2->first) return -1;
   if (s1->first > s2->first) return 1;
   return 0;
}

/* Compare XCoffSyms by a slightly weaker ordering, returning zero
   (equivalence) for any overlap, and -1 or 1 otherwise. */
static Int cmp_XCoffSym_by_overlap ( void* v1, void* v2 )
{
   XCoffSym* s1 = (XCoffSym*)v1;
   XCoffSym* s2 = (XCoffSym*)v2;
   if (s1->last < s2->first) return -1;
   if (s2->last < s1->first) return 1;
   return 0;
}

/* Compare XCoffSyms by their start address, and for equal addresses,
   use the name as a secondary sort key. */
static Int cmp_XCoffSym_by_start_then_name ( void* v1, void* v2 )
{
   XCoffSym* s1 = (XCoffSym*)v1;
   XCoffSym* s2 = (XCoffSym*)v2;
   if (s1->first < s2->first) return -1;
   if (s1->first > s2->first) return 1;
   return cmp_Names(s1->name, s2->name);
}


/* csect_idx is an index in the symbol table (start, n_entries) to a
   symbol defining a csect.  If possible, find the bounds of the csect
   and assign them to *first and *last, and return True; else return
   False.  sntext_1based_if_known is the 1-based number of the text
   section.  Note: computes stated VMAs, not actual VMAs. */

#if defined(VGP_ppc32_aix5)
# define SMTYP_SMTYP(x)  ((x) & 0x7)     /* symbol type */
# define CSECT(PP)       (((AUXENT*)(PP))->x_csect)
# define CSECT_LEN(PP)   (CSECT(PP).x_scnlen)
# define CSECT_ALIGN(PP) (SMTYP_ALIGN(CSECT(PP).x_smtyp))
# define CSECT_SMTYP(PP) (SMTYP_SMTYP(CSECT(PP).x_smtyp))
# define CSECT_SCLAS(PP) (CSECT(PP).x_smclas)

#elif defined(VGP_ppc64_aix5)
# define SMTYP_SMTYP(x)  ((x) & 0x7)     /* symbol type */
# define CSECT(PP)       (((AUXENT*)(PP))->x_csect)
# define CSECT_LEN(PP)   ((((ULong)(CSECT(PP).x_scnlen_hi)) << 32) \
                          | ((ULong)(CSECT(PP).x_scnlen_lo)))
# define CSECT_ALIGN(PP) (SMTYP_ALIGN(CSECT(PP).x_smtyp))
# define CSECT_SMTYP(PP) (SMTYP_SMTYP(CSECT(PP).x_smtyp))
# define CSECT_SCLAS(PP) (CSECT(PP).x_smclas)

#else
# error "Unknown platform"

#endif


#define SYM_IX(_tab,_n) ((SYMENT*)(((UChar*)(_tab)) + SYMESZ * (_n)))

static 
Bool get_csect_bounds ( UChar* start, UWord n_entries,
                        UWord csect_idx, 
                        Int sntext_1based_if_known,
                        /*OUT*/UChar** first, /*OUT*/UChar** last )
{
   Bool    is_text;
   SYMENT* cssym;
   AUXENT* csaux;

   vg_assert(SYMESZ == 18); /* both for XCOFF32 and XCOFF64 */

   if (n_entries < 2)
      return False;
   if (csect_idx+1 >= n_entries)
      return False;
   cssym = (SYMENT*)SYM_IX(start, csect_idx);
   csaux = (AUXENT*)SYM_IX(start, csect_idx+1);
   is_text = sntext_1based_if_known != -1
             && (Int)cssym->n_scnum == sntext_1based_if_known;

   if (!is_text)
      return False;

   if (cssym->n_sclass == C_EXT || cssym->n_sclass == C_HIDEXT) {
      if (cssym->n_numaux == 1) {
         if (CSECT_SMTYP(csaux) == XTY_SD) {
            if (0) VG_(printf)("GCB: SD: len is %lld\n", (Long)CSECT_LEN(csaux));
            *first = (UChar*)(cssym->n_value);
            *last = *first + CSECT_LEN(csaux)-1;
           return True;
         }
      } else {
         /* Possibly complain or take evasive action here.  In fact
            I've yet to see a case where a csect definition symbol has
            n_numaux != 1. */
      }
   }
   return False;
}

/* Read symbol and line number info for the given text section.  (This
   is the central routine for XCOFF reading.)  Returns NULL on
   success, or the text of an error message otherwise. */
static 
HChar* read_symbol_table ( 
          /*MOD*/struct _DebugInfo* di,

          /* location of symbol table */
          UChar* oi_symtab, UWord oi_nent_symtab,

          /* location of string table */
          UChar* oi_strtab, UWord oi_n_strtab,

          /* location of debug section (stabs strings, if any) */
          UChar* oi_debug, UWord oi_n_debug,

          /* location of line number info, if any */
          UChar* oi_lnos, UWord oi_nent_lnos,

          /* section indices */
          Int sntext_1based_if_known,
          Int sndata_1based_if_known,

          /* where the mapped data section is */
          /* Now in di->data_avma:   Addr data_avma, */
          /* Now in di->data_size:   UWord data_alen, */
          UWord data_alen_from_auxhdr,

	  /* where the mapped toc is (in the data section,
	     presumably), if known */
          Addr toc_avma,

          /* stated-to-actual VMA offsets */ 
          Word text_bias,
          Word data_bias 
       )
{
   SYMENT* sym;
   SYMENT* aux;
   UInt    i, j, nsyms, k, m;
   Name    name;
   Bool    is_text, is_data;
   XArray* syms = NULL; /* XArray of XCoffSyms */

   /* If the TOC avma is obviously bogus, get rid of it */
   { 
     UWord data_maxlen = di->data_size;
     if (data_maxlen < data_alen_from_auxhdr)
        data_maxlen = data_alen_from_auxhdr;

     //VG_(printf)(" toc_avma %p\n", toc_avma);
     //VG_(printf)("data_avma %p\n", data_avma);
     //VG_(printf)("dxxx_avma %p\n", data_avma + data_maxlen);

     if (toc_avma != 0
         && (toc_avma < di->data_avma 
             || toc_avma >= di->data_avma + data_maxlen))
        toc_avma = 0;
     //VG_(printf)("2toc_avma %p\n", toc_avma);
   }

   /* We can't just treat this as an array of SYMENTs, because C
      thinks they have size 20 whereas the spec says they have size 18
      (alignment padding) so doing the obvious thing screws up.  Hence
      we have to calculate the offset of each entry manually. */

   if (0) VG_(printf)("size of SYMENT = %ld\n", sizeof(SYMENT));

   /* ----------------------------------------------------------
      Phase 1: first make a pass through the symbols, looking for
      stuff in the text segment.  Calculate their actual VMAs,
      dump any outside the text segment actual VMA bounds, and 
      add the rest to 'syms'.
      ---------------------------------------------------------- */

   syms = VG_(newXA)( ML_(dinfo_zalloc), "di.readxcoff.rst.1", 
                      ML_(dinfo_free), sizeof(XCoffSym) );

   if (SHOW && SHOW_SYMS_P1) {
      VG_(printf)("--- BEGIN Phase1 (find text symbol starts) ---\n");
      VG_(printf)("--- note: shown addresses are STATED VMAs ---\n");
   }

   i = 0;
   while (1) {

      if (i >= oi_nent_symtab)
         break;

      sym = SYM_IX(oi_symtab, i);
      is_text = sntext_1based_if_known != -1
                && (Int)sym->n_scnum == sntext_1based_if_known;
      is_data = sndata_1based_if_known != -1
                && (Int)sym->n_scnum == sndata_1based_if_known;

      if (SHOW && SHOW_SYMS_P1)
         VG_(printf)("Phase1: %5d+%d  ", i, (Int)sym->n_numaux);

      name = mk_const_Name("(unknown)");
      if (sym->n_scnum == N_DEBUG && sym->n_sclass == C_FUN)
         name = maybeDerefStrTab( sym, oi_debug, oi_n_debug );
      else 
      if (sym->n_sclass & DBXMASK)
         name = mk_const_Name("(dbxstr)");
      else
         name = maybeDerefStrTab( sym, oi_strtab, oi_n_strtab);

      if (SHOW && SHOW_SYMS_P1) {
         VG_(printf)("%5s(%2d)  %6s  0x%016llx ", 
                     is_text ? "text" : is_data ? "data" : "other",
                     (Int)sym->n_scnum, 
                     name_of_syment_n_sclass(sym->n_sclass), 
                     (ULong)sym->n_value);
         print_Name(name);
         VG_(printf)("\n");
      }

      i++;
      i += sym->n_numaux;

      if (!is_text)
         continue;

      /* --- BEGIN regular(ish) symbol --- */
      if ((sym->n_sclass == C_EXT || sym->n_sclass == C_HIDEXT)
          && (sym->n_numaux == 1 || sym->n_numaux == 2)) {
         /* Dealing with a symbol with a csect entry.  By convention
            (according to IBM docs) the csect entry is the last
            auxiliary for this symbol, if there is more than one
            auxiliary present; hence "SYM_IX(oi_symtab, i-1)" below. */

         aux = SYM_IX(oi_symtab, i-1);
         if (0) VG_(printf)("symtype is %d\n", CSECT_SMTYP(aux));

         if (CSECT_SMTYP(aux) == XTY_SD) {
            /* Aux is a csect definition.  This is relatively rare,
               but at least it is simple: the CSECT_LEN(aux) field
               contains it's length, so we just heave that into the
               pot for phase 2. */
            XCoffSym cand;
            if (0) VG_(printf)("SD: len is %d\n", (Int)CSECT_LEN(aux));
            if (0) VG_(printf)("SD: proposed %#llx\n", (ULong)sym->n_value);
            init_XCoffSym(&cand);
            cand.first = sym->n_value;
            cand.last = cand.first + (UWord)CSECT_LEN(aux) - 1;

            cand.first += text_bias;
            cand.last += text_bias;
            cand.name = name;

            if (cand.last < di->text_avma 
                || cand.first >= di->text_avma + di->text_size)
               continue;
            if (cand.last < cand.first)
               continue;
            if (is_empty_Name(name))
               continue;
            (void)VG_(addToXA)(syms, &cand);
         }

         if (CSECT_SMTYP(aux) == XTY_LD) {
            /* Aux is a label definition.  This is the common case. */
            XCoffSym cand;
            Bool ok;
            UChar *csect_first, *csect_last;
            /* x_scnlen contains the symbol table entry of the
               containing csect.  Use the symbol's stated vma and csect
               end as the initial approximation of this symbol's start
               and length.  The length will get revised downwards in
               Phase 2. */
            init_XCoffSym(&cand);
            ok = get_csect_bounds( oi_symtab, oi_nent_symtab, 
                                   CSECT_LEN(aux), 
                                   sntext_1based_if_known,
                                   &csect_first, &csect_last );
            if (0 && ok)
               VG_(printf)("new csect svma %p %p\n", csect_first, csect_last);
            if (ok && ((UWord)csect_first) <= ((UWord)sym->n_value)
                   && ((UWord)sym->n_value) <= ((UWord)csect_last)) {
               if (0) {
                  VG_(printf)("LD: in a csect %p %p\n", 
                              csect_first, csect_last);
                  VG_(printf)("CAND: %p .. %p  %s\n", 
                              (void*)sym->n_value, (void*)csect_last, 
                              "fixme-Name-printing(1)" /*name*/);
               }
               cand.first = sym->n_value;
               cand.last = (Addr)csect_last;
            } else {
               if (0) {
                  VG_(printf)("LD: can't compute csect bounds?!\n");
                  VG_(printf)("CAND: %p .. %p  %s\n", 
                              (HChar*)sym->n_value,
                              (HChar*)sym->n_value+1,
                               "fixme-Name-printing(2)" /*name*/);
               }
               cand.first = sym->n_value;
               cand.last = cand.first + 1;
            }

            /* cand.first is a stated VMA; turn it into an actual VMA
               and ignore it if not in the actual text segment. */

            cand.first += text_bias;
            cand.last += text_bias;
            cand.name = name;

            if (cand.last < di->text_avma 
                || cand.first >= di->text_avma + di->text_size)
               continue;
            if (cand.last < cand.first)
               continue;
            if (is_empty_Name(name))
               continue;

            (void)VG_(addToXA)(syms, &cand);
         }
      }
      /* --- END regular(ish) symbol --- */

   }

   /* ----------------------------------------------------------
      Phase 2: suitable text symbols have been put into 'syms'.  Their
      start addresses are correct, but end addresses are those of the
      containing csect, which is in general way too long.  This phase
      clips the ends so that the ranges no longer overlap, and thereby
      constrains each symbol's range to something which, for the most
      part, is correct.
      ---------------------------------------------------------- */

   nsyms = VG_(sizeXA)(syms);

   if (SHOW && SHOW_SYMS_P1)
      VG_(printf)("Phase1 acquired %d text symbols\n", nsyms);

   if (SHOW && SHOW_SYMS_P2) {
      VG_(printf)("--- BEGIN Phase2 (find text symbol ends) ---\n");
      VG_(printf)("--- note: shown addresses are ACTUAL VMAs ---\n");
   }

   VG_(setCmpFnXA)(syms, cmp_XCoffSym_by_start_then_name);
   VG_(sortXA)(syms);

   /* We only know for sure the start addresses (actual VMAs) of
      symbols, and an overestimation of their end addresses.  So sort
      by start address, then clip each symbol so that its end address
      does not overlap with the next one along.

      There is a small refinement: if a group of symbols have the same
      address, treat them as a group: find the next symbol along that
      has a higher start address, and clip all of the group
      accordingly.  This clips the group as a whole so as not to
      overlap following symbols.  This leaves prefersym() in
      storage.c, which is not XCOFF-specific, to later decide which of
      the symbols in the group to keep. 

      Another refinement is that we need to get rid of symbols which,
      after clipping, have identical starts, ends, and names.  So the
      sorting uses the name as a secondary key.
   */

   for (i = 0; i < nsyms; i++) {
      for (k = i+1; 
           k < nsyms 
             && ((XCoffSym*)VG_(indexXA)(syms,i))->first 
                 == ((XCoffSym*)VG_(indexXA)(syms,k))->first; 
           k++)
         ;
      /* So now [i .. k-1] is a group all with the same start address.
         Clip their ending addresses so they don't overlap [k].  In
         the normal case (no overlaps), k == i+1. */
      if (k < nsyms) {
         XCoffSym* next = (XCoffSym*)VG_(indexXA)(syms,k);
         for (m = i; m < k; m++) {
            XCoffSym* here = (XCoffSym*)VG_(indexXA)(syms,m);
            vg_assert(here->first < next->first);
            if (here->last >= next->first)
               here->last = next->first-1;
         }
      }
      i = k-1;
      vg_assert(i <= nsyms);
   }

   j = 0;
   if (nsyms > 0) {
      j = 1;
      for (i = 1; i < nsyms; i++) {
         vg_assert(j <= i);
         XCoffSym* s_j1 = (XCoffSym*)VG_(indexXA)(syms, j-1);
         XCoffSym* s_j  = (XCoffSym*)VG_(indexXA)(syms, j);
         XCoffSym* s_i  = (XCoffSym*)VG_(indexXA)(syms, i);
         if (s_i->first != s_j1->first
             || s_i->last != s_j1->last
	     || 0 != cmp_Names(s_i->name, s_j1->name)) {
            *s_j = *s_i;
	    j++;
	 } else {
            if (SHOW && SHOW_SYMS_P2) {
               VG_(printf)("Phase2: dump duplicate "); 
               print_Name(s_i->name);
               VG_(printf)("\n");
	    }
	 }
      }
   }
   vg_assert(j >= 0 && j <= nsyms);
   VG_(dropTailXA)(syms, nsyms - j);
   nsyms = j;

   if (1) {
      for (i = 0; i < nsyms; i++) {
         XCoffSym* s = (XCoffSym*)VG_(indexXA)(syms, i);
         if (SHOW && SHOW_SYMS_P2) {
            VG_(printf)("Phase2: %d 0x%lx 0x%lx ", 
                        i, s->first, s->last);
            print_Name(s->name);
            VG_(printf)("\n");
         }
      }
   }

   /* ----------------------------------------------------------
      Phase 3: rescan the symbol table, looking for info on function
      start/end line numbers and source file names.  Generally
      this will be absent for sources compiled without -g.
      ---------------------------------------------------------- */

   if (SHOW && SHOW_SYMS_P3) {
      VG_(printf)("--- BEGIN Phase3 (find src filenames "
                  "& fn start/end line #s) ---\n");
      VG_(printf)("--- note: shown addresses are STATED VMAs ---\n");
   }

   /* The lookupXAs in the C_FUN(.bf) part have to operate by
      inclusion.  Hence: */
   VG_(setCmpFnXA)(syms, cmp_XCoffSym_by_overlap);
   VG_(sortXA)(syms);

   /* In this loop, p3currsym is maintained as a pointer to the most
      recent XCoffSym identified as FCN(.bf) (function start).
      Subsequent FCN(.ef) (function end) indications are compared
      against said symbol.  This assumes that function start/end
      indications are not nested. */

   XCoffSym* p3currsym = NULL;

   /* Maintain a stack of filenames.  We allow the stack pointer to go
      beyond the end, but obviously nothing is stored in this
      imaginary part of the stack. */
   Name filenames[N_FILENAME_STACK];
   Int  filenames_used = 1;

   Name name_unknown  = mk_empty_Name();
   Name name_overflow = mk_const_Name("(filename_stack_overflow)");

   for (i = 0; i < N_FILENAME_STACK; i++)
      filenames[i] = name_unknown;

#  define FNAME_PUSH(_fname) \
      do { \
         vg_assert(filenames_used >= 1);\
         if (filenames_used < N_FILENAME_STACK)\
            filenames[filenames_used] = (_fname);\
         filenames_used++;\
      } while (0)

#  define FNAME_POP \
      do {\
         vg_assert(filenames_used >= 1);\
         if (filenames_used > 1 && filenames_used <= N_FILENAME_STACK) \
            filenames[filenames_used-1] = name_unknown; \
         if (filenames_used > 1)\
            filenames_used--;\
      } while (0)

#  define FNAME_GET_TOP \
      (filenames_used > N_FILENAME_STACK  \
         ? name_overflow \
         : filenames[filenames_used-1])

#  define FNAME_SET_TOP(_fname) \
      do {\
         vg_assert(filenames_used >= 1);\
         filenames[filenames_used-1] = (_fname);\
      } while (0)


   i = 0;
   while (1) {

      if (i >= oi_nent_symtab)
         break;

      sym = SYM_IX(oi_symtab, i);
      is_text = sntext_1based_if_known != -1
                && (Int)sym->n_scnum == sntext_1based_if_known;
      is_data = sndata_1based_if_known != -1
                && (Int)sym->n_scnum == sndata_1based_if_known;

      if (0 && SHOW && SHOW_SYMS_P3)
         VG_(printf)("Phase3: %5d+%d  ", i, (Int)sym->n_numaux);

      name = mk_const_Name("(unknown)");
      if (sym->n_scnum == N_DEBUG && sym->n_sclass == C_FUN)
         name = maybeDerefStrTab( sym, oi_debug, oi_n_debug );
      else 
      if (sym->n_sclass & DBXMASK)
         name = mk_const_Name("(dbxstr)");
      else
         name = maybeDerefStrTab( sym, oi_strtab, oi_n_strtab);

      if (0 && SHOW && SHOW_SYMS_P3) {
         VG_(printf)("%5s(%2d)  %6s  0x%016llx ", 
                     is_text ? "text" : is_data ? "data" : "other",
                     (Int)sym->n_scnum, 
                     name_of_syment_n_sclass(sym->n_sclass), 
                     (ULong)sym->n_value);
         print_Name(name);
         VG_(printf)("\n");
      }

      i++;
      i += sym->n_numaux;

      /* --- BEGIN C_FILE [source file] --- */
      /* There are two variants of C_FILE: a simple one with n_numaux
         == 0, where the primary name is what we're after, and another
         variant with n_numaux == 3, in which we have to hunt around
         in the auxiliary entries to find the file name.  gcc produces
         exclusively the first kind, and xlc a mixture of both. */
      if (sym->n_sclass == C_FILE && sym->n_numaux == 0) {
         if (!is_empty_Name(name))
            FNAME_SET_TOP(name);
         if (SHOW && SHOW_SYMS_P3) {
            VG_(printf)("Phase3: %5d+%d  FILE      ",
                        i-1-sym->n_numaux, (Int)sym->n_numaux );
            print_Name(name);
            VG_(printf)("\n");
         }
         continue;
      }
      if (sym->n_sclass == C_FILE && sym->n_numaux > 1 
                                  && sym->n_numaux <= 5 /*stay sane*/) {
         for (k = 0; k < sym->n_numaux; k++) {
            aux = SYM_IX(oi_symtab, i - sym->n_numaux + k);
            Name fname
               = maybeDerefStrTab_fname( 
                    (UChar*)&((AUXENT*)aux)->x_file.x_fname,
                    oi_strtab, oi_n_strtab);
            if (((AUXENT*)aux)->x_file._x.x_ftype == XFT_FN) {
               if (!is_empty_Name(fname))
                  FNAME_SET_TOP(fname);
               if (SHOW && SHOW_SYMS_P3) {
                  VG_(printf)("Phase3: %5d+%d  FILE      ",
                              i-1-sym->n_numaux, (Int)sym->n_numaux );
                  print_Name(fname);
                  VG_(printf)("\n");
               }
               break;
            }
         }
         continue;
      }
      /* --- END C_FILE [source file] --- */

      /* --- BEGIN C_BINCL [beginning of include] --- */
      if (sym->n_sclass == C_BINCL && sym->n_numaux == 0) {
         FNAME_PUSH(name);
         if (SHOW && SHOW_SYMS_P3)
            VG_(printf)("Phase3: %5d+%d  BINCL     %s\n",
                         i-1-sym->n_numaux, (Int)sym->n_numaux, 
                         "fixme-Name-printing(3)" /*name*/ );
         continue;
      }
      /* --- END C_BINCL [beginning of include] --- */

      /* --- BEGIN C_EINCL [end of include] --- */
      if (sym->n_sclass == C_EINCL && sym->n_numaux == 0) {
         FNAME_POP;
         if (SHOW && SHOW_SYMS_P3)
            VG_(printf)("Phase3: %5d+%d  EINCL     %s\n",
                         i-1-sym->n_numaux, (Int)sym->n_numaux, 
                         "fixme-Name-printing(4)" /*name*/ );
         continue;
      }
      /* --- END C_EINCL [end of include] --- */

      /* everything else that is interesting is in the text
         section. */
      if (!is_text)
         continue;
 
      /* --- BEGIN C_FCN(.bf) [function begin mark] --- */
      if (sym->n_sclass == C_FCN 
          && sym->n_numaux == 1 
          && eq_string_Name(name, ".bf")) {
         /* aux is BLOCK */
         aux = SYM_IX(oi_symtab, i-1);
         Addr fn_start_avma = ((Addr)sym->n_value) + text_bias;
         Int  fn_start_lnno = ((AUXENT*)aux)->x_sym.x_misc.x_lnsz.x_lnno;
         /* Look in 'syms' to see if we have anything for address
            fn_avma. */
         XCoffSym key;
         VG_(memset)(&key, 0, sizeof(key));
         key.first = fn_start_avma;
         key.last  = fn_start_avma;
         Word ix_lo, ix_hi;

	 /* Search for all symbols intersecting fn_start_avma. */
         Bool found = VG_(lookupXA)(syms, &key, &ix_lo, &ix_hi);
         if (found) {
            /* All the 'syms' entries from ix_lo to ix_hi match. */

            for (k = ix_lo; k <= ix_hi; k++) {
               XCoffSym* tsym = (XCoffSym*)VG_(indexXA)(syms,k);

               /* note the start line number */
               if (tsym->slnno == 0 && fn_start_lnno > 0)
                  tsym->slnno = fn_start_lnno;

               /* also the current filename, if we know it */
               if (is_empty_Name(tsym->fname) 
                   && !is_empty_Name(FNAME_GET_TOP)) 
                  tsym->fname = FNAME_GET_TOP;

               /* remember the first in the range as the new current
                  (I've never seen a range with > 1) */
               if (k == ix_lo)
                  p3currsym = tsym;
               if (SHOW && SHOW_SYMS_P3) {
                   VG_(printf)("Phase3: %5d+%d  FCN(.bf)  0x%016llx  "
                               "lnno=%-4d  ", 
                               i-1-sym->n_numaux, (Int)sym->n_numaux, 
                               (ULong)sym->n_value,
                               fn_start_lnno );
                   print_Name(tsym->name);
                   VG_(printf)("\n");
                   if (!is_empty_Name(tsym->fname)) {
                      VG_(printf)("Phase3:                    ");
                      print_Name(tsym->fname);
                      VG_(printf)("\n");
                   }
               }
            }
         }
         continue;
      }
      /* --- END C_FCN(.bf) [function begin mark] --- */

      /* --- BEGIN C_FCN(.ef) [function end mark] --- */
      if (sym->n_sclass == C_FCN 
          && sym->n_numaux == 1 
          && eq_string_Name(name, ".ef")) {
         /* aux is BLOCK */
         aux = SYM_IX(oi_symtab, i-1);
         /* In this case the n_value field appears to give the address
            of the first insn following the end of the function.
            Hence the - 1. */
         Addr fn_end_avma = ((Addr)sym->n_value) + text_bias - 1;
         Int  fn_end_lnno = ((AUXENT*)aux)->x_sym.x_misc.x_lnsz.x_lnno;

         if (p3currsym
             && fn_end_avma >= p3currsym->first
             && fn_end_avma <= p3currsym->last) {
            if (p3currsym->elnno == 0 && fn_end_lnno > 0)
               p3currsym->elnno = fn_end_lnno;
            if (SHOW && SHOW_SYMS_P3) {
                VG_(printf)("Phase3: %5d+%d  FCN(.ef)  0x%016llx  "
                            "lnno=%-4d  ", 
                            i-1-sym->n_numaux, (Int)sym->n_numaux, 
                            (ULong)sym->n_value,
                            fn_end_lnno );
                print_Name(p3currsym->name);
                VG_(printf)("\n");
            }
            if (fn_end_avma < p3currsym->last) {
               /* also take the opportunity to trim the symbol's
                  length to something less than established by the
                  initial estimation done by Phases 1 and 2. */
               if (0) VG_(printf)("trim end from %#lx to %#lx\n", 
                                  p3currsym->last, fn_end_avma);
               p3currsym->last = fn_end_avma;
            }
         }
         continue;
      }
      /* --- END C_FCN(.ef) [function end mark] --- */

   }

   /* ----------------------------------------------------------
      Phase 4: read and enumerate the line number entries, if 
      there are any.  This depends on knowing the function start/end
      line numbers established in Phase 3.
      ---------------------------------------------------------- */

   if (SHOW && SHOW_SYMS_P4) {
      VG_(printf)("--- BEGIN Phase4 (read line number info) ---\n");
      VG_(printf)("--- note: shown addresses are ACTUAL VMAs ---\n");
   }

   /* Re-sort 'syms' using the compare-start-addresses ordering, so we
      can use that in subsequent searches. */
   VG_(setCmpFnXA)(syms, cmp_XCoffSym_by_start);
   VG_(sortXA)(syms);

   if (oi_lnos && oi_nent_lnos > 0) {

#     if defined(VGP_ppc32_aix5)
      vg_assert(LINESZ == 6); /* XCOFF32 */
#     elif defined(VGP_ppc64_aix5)
      vg_assert(LINESZ == 12); /* XCOFF64 */
#     else
#     error "Unknown plat"
#     endif

#     define LNO_IX(_tab,_n) \
         ((LINENO*)(((UChar*)(_tab)) + LINESZ * (_n)))

      /* Current fn that we are processing line numbers for */
      XCoffSym* p4currsym = NULL;

      /* SegInfo's string table pointer for p4currsym's file name.
         Allocated on demand, so as not to waste space in the
         SegInfo's string table. */
      UChar* si_fname_str = NULL;

      /* Ditto the directory name, if we can manage it. */
      UChar* si_dname_str = NULL;

      for (i = 0; i < oi_nent_lnos; i++) {
         LINENO* lno = LNO_IX(oi_lnos,i);

         if (lno->l_lnno == 0) {
            /* New fn.  We get given the index in the symbol table of
               the relevant function.  It should be a C_EXT, C_WEAKEXT
               or C_HIDEXT flavour, according to the IBM docs. */
            Int sym_ix = (Int)lno->l_addr.l_symndx;
            sym = SYM_IX(oi_symtab, sym_ix);
            if (!(sym->n_sclass == C_EXT 
                  || sym->n_sclass == C_WEAKEXT 
                  || sym->n_sclass == C_HIDEXT))
               return "readxcoff.c: invalid symbol reference"
                      " in line number info";
            /* For these 3 symbol kinds, the n_value field is the
               symbol's stated VMA.  Convert this to an actual VMA and
               use that to find the associated XCoffSym. */
            Addr sym_avma = ((Addr)sym->n_value) + text_bias;

            XCoffSym key;
            VG_(memset)(&key, 0, sizeof(key));
            key.first = sym_avma;
            Word ix_lo, ix_hi;

            Bool found = VG_(lookupXA)(syms, &key, &ix_lo, &ix_hi);
            if (found) {
               /* All the 'syms' entries from ix_lo to ix_hi match.
                  Just use the lowest (sigh ..) */
               p4currsym = (XCoffSym*)VG_(indexXA)(syms, ix_lo);
            } else {
               /* We can't find the relevant sym, but we still have to
                  wade through the line number info for this function
                  until we get to the starting record for the next
                  one. */
               p4currsym = NULL;
            }

            /* If we decide to add any line info for this fn to the
               SegInfo, we'll allocate this.  Otherwise don't
               bother. */
            si_fname_str = NULL;
            si_dname_str = NULL;

            if (SHOW && SHOW_SYMS_P4) {
               VG_(printf)("Phase4: new fn (%d found), avma 0x%016llx  ", 
                           (Int)(ix_hi-ix_lo+1),
                           (ULong)sym_avma );
               if (p4currsym)
                  print_Name(p4currsym->name);
               else
                  VG_(printf)("UNKNOWN");
               VG_(printf)("\n");
            }

         } else {
            /* Line number entry for the current fn. */
            if (!p4currsym)
               continue;
            Int line_no = (Int)(UInt)lno->l_lnno;
            line_no += (p4currsym->slnno - 1);
            Addr line_first_avma = ((Addr)lno->l_addr.l_paddr) + text_bias;
            if (line_first_avma < p4currsym->first
                || line_first_avma > p4currsym->last)
               continue;
            Addr line_last_avma = p4currsym->last;
            /* Try to refine the last_avma by looking at the next
               line's entry. */

            /* XXX: TODO.  What we have currently works only because
               the generic line number canonicaliser truncates
               overlapping address ranges in the way which we happen
               to need anyway. */
            if (SHOW && SHOW_SYMS_P4)
               VG_(printf)("Phase4:  line %d 0x%016llx - 0x%016llx\n", 
                           line_no, (ULong)line_first_avma, 
                                    (ULong)line_last_avma);

            /* This now has to be allocated.  Try and figure out the
               dir name at the same time.  This is a bit ugly in that
               it involves messing with the string after it's been
               copied into the SegInfo's string table, but seems
               harmless enough. */
            if ((!si_fname_str) && !is_empty_Name(p4currsym->fname)) {
               si_dname_str = NULL;
               si_fname_str = ML_(addStr)(di, p4currsym->fname.vec,
                                              p4currsym->fname.len);
               UChar* lastslash = VG_(strrchr)(si_fname_str, '/');
               if (lastslash)
                  vg_assert(lastslash[0] == '/');
               if (lastslash[1] != 0) {
                  si_dname_str = si_fname_str;
                  lastslash[0] = 0; /* replace the / with a NUL
                                       terminator */
                  si_fname_str = lastslash+1;
                  if (0) VG_(printf)("XXX %s %s\n", si_dname_str, 
                                                    si_fname_str);
               }
            }
            /* finally .. */
	    if (line_no >= 0)
               ML_(addLineInfo)(di, si_fname_str, si_dname_str,
                                line_first_avma, line_last_avma+1,
                                line_no, i/*debugging only*/);
         }
      }

#     undef LNO_IX
   }

#if defined(OFFICIAL_PHASE5)
   /* ----------------------------------------------------------
      Phase 5: Do another trawl of the XCOFF symbol table, looking
      for TOC entries for the entries we've already placed in 'syms'.
      ---------------------------------------------------------- */

   if (SHOW && SHOW_SYMS_P5)
      VG_(printf)("--- BEGIN official Phase5 (find TOC pointers) ---\n");

   Bool is_cfun;

   i = 0;
   while (1) {

      if (i >= oi_nent_symtab)
         break;

      sym = SYM_IX(oi_symtab, i);
      is_text = sntext_1based_if_known != -1
                && (Int)sym->n_scnum == sntext_1based_if_known;
      is_data = sndata_1based_if_known != -1
                && (Int)sym->n_scnum == sndata_1based_if_known;
      is_cfun = sym->n_scnum == N_DEBUG 
                && sym->n_sclass == C_FUN;

      i++;
      i += sym->n_numaux;

      if (!is_cfun && !is_data)
         continue;

      if (SHOW && SHOW_SYMS_P5)
         VG_(printf)("Phase5o: %5d+%d  ", i-1-sym->n_numaux, 
                                          (Int)sym->n_numaux);

      name = mk_const_Name("(unknown)");
      if (is_cfun)
         name = maybeDerefStrTab( sym, oi_debug, oi_n_debug );
      else 
      if (sym->n_sclass & DBXMASK)
         name = mk_const_Name("(dbxstr)");
      else
         name = maybeDerefStrTab( sym, oi_strtab, oi_n_strtab);

      if (SHOW && SHOW_SYMS_P5) {
         VG_(printf)("%5s(%2d)  %6s  svma 0x%016llx ", 
                     is_text ? "text" : is_data ? "data" : "other",
                     (Int)sym->n_scnum, 
                     name_of_syment_n_sclass(sym->n_sclass), 
                     (ULong)sym->n_value);
         print_Name(name);
         VG_(printf)("\n");
      }

      Addr avma = (Addr)sym->n_value + data_bias;
      if (0) VG_(printf)("data sym: avma %p, limits %p-%p\n", 
                         avma,  data_avma,data_avma + data_alen);

      /* Does avma point to 3 valid words inside the actual data
         segment?  iow, can it possibly be a valid function
         descriptor?  If not, move on. */
      if (! (avma >= data_avma 
             && avma + 3 * sizeof(Word) <= data_avma + data_alen) )
         continue;

      UWord* fndescr = (UWord*)avma;

      if (SHOW && SHOW_SYMS_P5) 
          VG_(printf)("                  fndescr = {0x%lx,0x%lx}\n", 
                      fndescr[0], fndescr[1]);

      /* Another check: fndescr[0], the entry point, must point inside
         the actual text segment.  Discard any that don't. */

      Addr fndescr_0 = (Addr)fndescr[0];
      if (fndescr_0 < si->text_avma 
          || fndescr_0 >= si->text_avma+si->text_size)
         continue;

      /* Let's suppose that fndescr is the descriptor for a
         function with name NAME.  If that's so, then 'syms'
         acquired by stage 2 should have an entry of name '.NAME'
         whose address is fndescr[0].  If so, then fndescr[1] must
         be the relevant r2 value for it. */
      /* Look in 'syms' to see if we have anything for address
         fndescr[0]. */
      XCoffSym key;
      VG_(memset)(&key, 0, sizeof(key));
      key.first = fndescr_0;
      Word ix_lo, ix_hi;
      Bool found = VG_(lookupXA)(syms, &key, &ix_lo, &ix_hi);
      if (found) {
         /* So all the 'syms' entries from ix_lo to ix_hi have an
            address which matches the entry point address stated in
            this descriptor.  For each one, as a final sanity
            check, see if the 'syms' entry has a name .NAME where
            NAME is that of the data symbol currently under
            consideration.  If so, it's a pretty good bet that this
            descriptor matches the text symbol we already have, and
            so we have a valid tocptr value from fndescr[1]. */
         for (k = ix_lo; k <= ix_hi; k++) {
            XCoffSym* tsym = (XCoffSym*)VG_(indexXA)(syms,k);
            vg_assert(!is_empty_Name(tsym->name));
            /* VG_(printf)("cmp %s %s\n", name, tsym->name); */
            /* VG_(printf)("found matching %d %s\n", k, tsym->name); */
            if (tsym->name.len == 1 + name.len
                && tsym->name.vec[0] == '.'
                && 0 == VG_(memcmp)(&tsym->name.vec[1],
                                    &name.vec[0], name.len)) {
               Addr r2val = fndescr[1];
               if (tsym->r2known) {
                  if (tsym->r2value != r2val)
                     /* COMPLAIN - conflicting r2 values*/ ;
               } else {
                  tsym->r2known = True;
                  tsym->r2value = r2val;
               }
            }
         }
      }

   }

#else /* !defined(OFFICIAL_PHASE5) */
   /* ----------------------------------------------------------
      Alternative kludgey Phase 5: find TOC entries for 'syms' by the
      blunt-instrument approach of scanning the actual data section
      and noting anything that looks like a function descriptor.
      This is dangerous in the sense that if there are any 3 word
      structs which are not real function descriptors but just happen
      to look like them, then those will be included too.  
      Seems unlikely though.
      ---------------------------------------------------------- */

   if (SHOW && SHOW_SYMS_P5)
      VG_(printf)("--- BEGIN kludged Phase5 (find TOC pointers) ---\n");

   if (SHOW)
      VG_(printf)("Phase5: actual data segment: %#lx %#lx\n",
                  di->data_avma, di->data_avma + di->data_size);

   /* Skip obviously-missing data sections. */
   if (di->data_avma != 0 && di->data_size >= sizeof(UWord)) {

      /* set up for inspecting all the aligned words in the actual
         data section. */

      Addr tmp = di->data_avma;
      while (tmp & (sizeof(UWord)-1))
         tmp++;

      UWord* first_data_word = (UWord*)tmp;
      tmp = di->data_avma + di->data_size - sizeof(UWord);
      while (tmp & (sizeof(UWord)-1))
         tmp--;
      UWord* last_data_word = (UWord*)tmp;

      if (SHOW) 
         VG_(printf)("Phase5: data segment conservatively aligned %p %p\n", 
                     first_data_word, last_data_word);

      UWord* wP = first_data_word;
      UWord  w;

      while (True) {

         XCoffSym key;
         Word     ix_lo, ix_hi;
         Bool     found;

         if (& wP[2] > last_data_word)
            break; /* no space left for a 3-word descriptor */

         w = wP[0];
         if (!(w >= di->text_avma 
               && w < di->text_avma + di->text_size)) {
            wP++;
            continue; /* entry pointer is not to text segment */
         }

         w = wP[1];
         if (!(w >= di->data_avma && w < di->data_avma + di->data_size)) {
            wP++;
            if (SHOW && SHOW_SYMS_P5) {
               VG_(memset)(&key, 0, sizeof(key));
               key.first = wP[0];
               found = VG_(lookupXA)(syms, &key, &ix_lo, &ix_hi);
               if (found) {
                  vg_assert(ix_lo <= ix_hi);
                  XCoffSym* tsym = (XCoffSym*)VG_(indexXA)(syms,ix_lo);
                  VG_(printf)("Phase5: bad tocptc at 0x%016llx={",
                              (ULong)(UWord)(wP-1));
                  print_Name(tsym->name);
                  VG_(printf)(",%p}\n", (void*)w);
               }
            }
            continue; /* r2 value does not point to data segment */
         }

         /* ok, so wP might be a valid fn descr.  But does it point to
            a text symbol we know about?  Look in 'syms' to see if we
            have anything for wP[0]. */
         VG_(memset)(&key, 0, sizeof(key));
         key.first = wP[0];
         found = VG_(lookupXA)(syms, &key, &ix_lo, &ix_hi);
         if (found) {
            for (k = ix_lo; k <= ix_hi; k++) {
               XCoffSym* tsym = (XCoffSym*)VG_(indexXA)(syms,k);
               Addr r2val = wP[1];
               if (tsym->r2known) {
                  if (tsym->r2value != r2val)
                     /* COMPLAIN - conflicting r2 values*/ ;
               } else {
                  tsym->r2known = True;
                  tsym->r2value = r2val;
                  if (SHOW && SHOW_SYMS_P5) {
                     VG_(printf)("Phase5: found tocptr 0x%016llx for ", 
                                 (ULong)r2val);
                     print_Name(tsym->name);
                     VG_(printf)("\n");
                  }
               }
            }
         }

         wP++;
      }
   }

#endif /* defined(OFFICIAL_PHASE5) */

   /* ----------------------------------------------------------
      Phase 6: trivial: copy the syms out of 'syms' into the 
      generic debuginfo tables, and free up 'syms'.
      ---------------------------------------------------------- */

   if (SHOW && SHOW_SYMS_P6) {
      VG_(printf)("--- BEGIN Phase6 (finalise symbol info) ---\n");
      VG_(printf)("--- note: shown addresses are ACTUAL VMAs ---\n");
   }

   for (i = 0; i < nsyms; i++) {
      DiSym     dis;
      XCoffSym* s = (XCoffSym*)VG_(indexXA)(syms, i);
      Addr  addr = s->first;
      UWord size = s->last + 1 - s->first;
      Bool  guessed_toc = False;

      /* If everything worked right, the symbol should fall within the
         mapped text segment.  Hence .. */
      Bool sane = addr >= di->text_avma 
                  && addr+size <= di->text_avma + di->text_size;

      if (SHOW && SHOW_SYMS_P6) {
         VG_(printf)("Phase6: %s %3d  0x%08lx-0x%08lx  0x%08lx  ", 
                     sane ? "   " : "BAD",
                     i, 
                     addr,
                     addr + size - 1,
                     s->r2known ? s->r2value : 0 );
         print_Name(s->name);
         VG_(printf)("\n");
      }

#     if defined(VGP_ppc64_aix5)
      /* 64-bit kludge: if we can't find a plausible toc ptr just use
         the one specified in the XCOFF auxiliary header. */
      if ((!s->r2known)
          && toc_avma != 0
          && s->name.len > 8
          && 0==VG_(strncmp)(&s->name.vec[0], "._vgwZU_", 8)) {
         s->r2known = True;
         s->r2value = toc_avma;
         guessed_toc = True;
         if (SHOW && SHOW_SYMS_P6)
            VG_(printf)("Phase6: assuming toc 0x%08lx for above sym\n", 
                        s->r2value);
      }
#     endif

      /* Actually add the symbol (finallyatlast) */
      if (sane) {
         UInt nlen;
         dis.addr    = addr;
         dis.size    = size;
         dis.tocptr  = s->r2known ? s->r2value : 0;
         dis.isText  = True;
         dis.isIFunc = False;
         vg_assert(!is_empty_Name(s->name));
         nlen = s->name.len;
         vg_assert(nlen > 0);
         if (s->name.vec[0] == '.')
            dis.name = ML_(addStr)(di, &s->name.vec[1], nlen-1 );
         else
            dis.name = ML_(addStr)(di, &s->name.vec[0], nlen-0 );
         ML_(addSym)( di, &dis );
         if (0 && s->r2known)
            VG_(printf)("r2 known for %s\n",
                        "fixme-Name-printing(5)" /*s->name*/ );

	 if (guessed_toc)
            VG_(message)(Vg_DebugMsg, "WARNING: assuming toc 0x%lx for %s\n", 
                                      s->r2value, dis.name);
      }
   }

   /* Free up the XA */
   VG_(deleteXA)(syms);

#  undef SYM_IX

   return NULL; /*success*/
}


static void show_loader_section ( struct _DebugInfo* di,
                                  UChar* oi_start, UWord size )
{
   Int i, j;
   LDHDR* hdr = (LDHDR*)oi_start;
   UChar* strtab_import = NULL;
   UChar* strtab_other  = NULL;
   if (SHOW) {
      VG_(printf)("   l_version           %llu\n", (ULong)hdr->l_version);
      VG_(printf)("   l_nsyms             %lld\n", (Long)hdr->l_nsyms);
      VG_(printf)("   l_nreloc            %lld\n", (Long)hdr->l_nreloc);
      VG_(printf)("   l_istlen (i st len) %lld\n", (Long)hdr->l_istlen);
      VG_(printf)("   l_impoff (i st off) %llu\n", (ULong)hdr->l_impoff);
      VG_(printf)("   l_nimpid (# imps)   %llu\n", (ULong)hdr->l_nimpid);
      VG_(printf)("   l_stlen  (st len)   %llu\n", (ULong)hdr->l_stlen);
      VG_(printf)("   l_stoff  (st off)   %llu\n", (ULong)hdr->l_stoff);
   }

   if (hdr->l_istlen > 0)
      strtab_import = oi_start + hdr->l_impoff;
   if (hdr->l_stlen > 0)
      strtab_other = oi_start + hdr->l_stoff;

   if (strtab_import) {
      if (SHOW)
         VG_(printf)("   Loader Import String Table: %llu bytes\n", 
                     (ULong)hdr->l_istlen);
      i = 0;
      j = 0;
      while (1) {
         if (i >= hdr->l_istlen)
            break;
         if (SHOW && SHOW_LD_STRTAB)
            VG_(printf)("     %3d%s ", i, (j%3)==0 ? "::" : "  ");
         j++;
         while (i < hdr->l_istlen && strtab_import[i]) {
            if (SHOW && SHOW_LD_STRTAB)
               VG_(printf)("%c", sanitiseChar(strtab_import[i]));
            i++;
         }
         i++;
         if (SHOW && SHOW_LD_STRTAB)
            VG_(printf)("\n");
      }
   }

   if (strtab_other) {
      if (SHOW)
         VG_(printf)("   Loader Other String Table: %llu bytes\n", 
                     (ULong)hdr->l_stlen);
      i = 0;
      while (1) {
         int len = 0;
         if (i+1 >= hdr->l_stlen)
            break;
         len = (unsigned char)strtab_other[i];
         len <<= 8;
         len |= (unsigned char)strtab_other[i+1];
         i += 2;
         if (i >= hdr->l_stlen)
            break;
         if (SHOW && SHOW_LD_STRTAB)
            VG_(printf)("      %2d len %2d  ", i, len);
         while (len >= 0 && i < hdr->l_stlen && strtab_other[i]) {
            if (SHOW && SHOW_LD_STRTAB)
               VG_(printf)("%c", sanitiseChar(strtab_other[i]));
            i++;
            len--;
         }
         i++;
         if (SHOW && SHOW_LD_STRTAB)
            VG_(printf)("\n");
      }
   }

   if (SHOW)
      VG_(printf)("   Loader Symbol Table: %lld entries\n", (Long)hdr->l_nsyms);
   LDSYM* sym = (LDSYM*)(oi_start + sizeof(LDHDR));
   for (i = 0; i < hdr->l_nsyms; i++) {
      Name name = maybeDerefStrTab( (SYMENT*)&sym[i],
                                    strtab_other, hdr->l_stlen );
      if (SHOW && SHOW_LD_SYMTAB) {
         VG_(printf)("      %2d:  %016llx  sec %d  ty 0x%02x  "
                     "scla 0x%02x  itab %d  ", 
                     i, (ULong)sym[i].l_value, (Int)sym[i].l_scnum, 
                     (Int)sym[i].l_smtype, (Int)sym[i].l_smclas,
                     (Int)sym[i].l_ifile);
         print_Name(name);
         VG_(printf)("\n");
      }
   }

#  if defined(VGP_ppc32_aix5)
   vg_assert(sizeof(LDREL) == 12);
#  elif defined(VGP_ppc64_aix5)
   vg_assert(sizeof(LDREL) == 16);
#  else
#    error Unknown platform
#  endif

   LDREL* rel = (LDREL*)(&sym[hdr->l_nsyms]);
   if (SHOW)
      VG_(printf)("   Loader Relocation Table: %lld entries\n", 
                  (Long)hdr->l_nreloc);
   for (i = 0; i < hdr->l_nreloc; i++) {
      if (SHOW && SHOW_LD_RELTAB)
         VG_(printf)("      %3d:  va %016llx  sym %2lld  rty 0x%4x  sec %2d\n",
                     i, (ULong)rel[i].l_vaddr, (Long)rel[i].l_symndx, 
                        (Int)rel[i].l_rtype, (Int)rel[i].l_rsecnm);
   }

   if (SHOW)
      VG_(printf)("\n");
}


/* Returns True on success, False on any kind of error. 

   The object file from which to read symbols is mapped temporarily at
   [oimage .. oimage + n_oimage).

   The VMA of where the relevant text section really got loaded (the
   "actual VMA", _avma) is [si->text_avma .. si->text_avma
   + si->text_size).

   The VMA of the associated data section really got loaded
   (the "actual VMA", _avma) is [data_avma .. data_avma + data_alen).

   We will need to peer at the loaded data section in order to make
   sense of TOC entries, hence we need to be assured it is mapped and
   readable.  m_aspacemgr should have given us that assurance, in the
   sense that data_avma/data_alen will be save to read in by the time
   we get here.
*/
static 
Bool read_xcoff_mapped_object ( struct _DebugInfo* di,
                                UChar* oimage, UWord n_oimage )
{
#define BAD(_msg)  do { ML_(symerr)(di, True/*serious*/,_msg); \
                        return False; } while (0)

   Int i, j;

   /* The first byte after the oimage - we can't go here */
   UChar* oimage_after = oimage + n_oimage;

   UChar* cursor = oimage;

   /* ------------ File Header ------------ */
#  if defined(VGP_ppc32_aix5)
   if (sizeof(FILHDR) != 20)
      BAD("readxcoff.c: invalid FILHDR size (32-bit)");
#  elif defined(VGP_ppc64_aix5)
   if (sizeof(FILHDR) != 24)
      BAD("readxcoff.c: invalid FILHDR size (64-bit)");
#  else
#  error "Invalid platform"
#  endif

   if (n_oimage < sizeof(FILHDR))
      BAD("readxcoff.c: XCOFF object file header is implausibly small (2)");

   FILHDR* t_filehdr = (FILHDR*)cursor;
   cursor += sizeof(FILHDR);

   if (SHOW) {
      VG_(printf)("\nFile Header:\n");
      VG_(printf)("   magic             0x%04x (%s)\n", 
                  (UInt)t_filehdr->f_magic,
                  name_of_filhdr_f_magic(t_filehdr->f_magic));
   }

#  if defined(VGP_ppc32_aix5)
   if (t_filehdr->f_magic != 0x01DF /* XCOFF32 */)
      BAD("readxcoff.c: XCOFF32 object file header has invalid magic");
#  elif defined(VGP_ppc64_aix5)
   if (t_filehdr->f_magic != 0x01F7 /* XCOFF64 */)
      BAD("readxcoff.c: XCOFF64 object file header has invalid magic");
#  else
#  error "Invalid platform"
#  endif

   if (SHOW) {
      VG_(printf)("   # of sections     %u\n",       (UInt)t_filehdr->f_nscns);
      VG_(printf)("   time/date         0x%08llx\n", (ULong)t_filehdr->f_timdat);
      VG_(printf)("   symtab foffset    %llu\n",     (ULong)t_filehdr->f_symptr);
      VG_(printf)("   # symtab entries  %llu\n",     (ULong)t_filehdr->f_nsyms);
      VG_(printf)("   size of aux hdr   %llu\n",     (ULong)t_filehdr->f_opthdr);
      VG_(printf)("   flags             0x%04x\n",   (UInt)t_filehdr->f_flags);
      if (t_filehdr->f_flags) {
         VG_(printf)("                     ");
         if (t_filehdr->f_flags & F_RELFLG)    VG_(printf)("NoRelocInfo ");
         if (t_filehdr->f_flags & F_EXEC)      VG_(printf)("IsExec ");
         if (t_filehdr->f_flags & F_LNNO)      VG_(printf)("NoLineInfo ");
         if (t_filehdr->f_flags & F_LSYMS)     VG_(printf)("LSYMS ");
         if (t_filehdr->f_flags & F_FDPR_PROF) VG_(printf)("FDPR_PROF ");
         if (t_filehdr->f_flags & F_FDPR_OPTI) VG_(printf)("FDPR_OPTI ");
         if (t_filehdr->f_flags & F_DSA)       VG_(printf)("LargeProc ");
#        if defined(F_DEP_1)
         if (t_filehdr->f_flags & F_DEP_1)     VG_(printf)("DEP_1 ");
#        endif
#        if defined(F_VARPG)
         if (t_filehdr->f_flags & F_VARPG)     VG_(printf)("VARPG ");
#        endif
         if (t_filehdr->f_flags & F_LPTEXT)    VG_(printf)("LPTEXT ");
         if (t_filehdr->f_flags & F_LPDATA)    VG_(printf)("LPDATA ");
         if (t_filehdr->f_flags & F_DYNLOAD)   VG_(printf)("Dynamic ");
         if (t_filehdr->f_flags & F_SHROBJ)    VG_(printf)("SharedObj ");
         if (t_filehdr->f_flags & F_LOADONLY)  VG_(printf)("LOADONLY ");
#        if defined(F_DEP_2)
         if (t_filehdr->f_flags & F_DEP_2)     VG_(printf)("DEP_2 ");
#        endif
         VG_(printf)("\n");
      }
   }

   /* ------------ Auxiliary Header ------------ */
#  if defined(VGP_ppc32_aix5)
   if (sizeof(AOUTHDR) != 72)
      BAD("readxcoff.c: invalid AOUTHDR size (32-bit)");
#  elif defined(VGP_ppc64_aix5)
   if (sizeof(AOUTHDR) != 120)
      BAD("readxcoff.c: invalid AOUTHDR size (64-bit)");
#  else
#  error "Invalid platform"
#  endif

   Int sntext_1based_if_known = -1;
   Int sndata_1based_if_known = -1;

   Addr  data_svma = 0; /* stated VMA of data section, if known */
   Bool  data_svma_known = False;
   Word  data_bias = 0;
   UWord data_alen_from_auxhdr = 0;

   Addr  text_svma = 0; /* stated VMA of text section, if known */
   Bool  text_svma_known = False;
   Word  text_bias = 0;

   Addr  toc_avma = 0; /* actual VMA of toc, if known */
   Addr  toc_svma = 0; /* stated VMA of toc, if known */
   Addr  toc_svma_known = False;

   AOUTHDR* t_auxhdr = NULL;
   if (t_filehdr->f_opthdr > 0) {
      t_auxhdr = (AOUTHDR*)cursor;
      cursor += sizeof(AOUTHDR);
      sntext_1based_if_known = (Int)t_auxhdr->o_sntext;
      sndata_1based_if_known = (Int)t_auxhdr->o_sndata;

      if (SHOW) {
         VG_(printf)("\nAuxiliary Header\n");
         VG_(printf)("   magic        0x%04x (should be 0x010b)\n", 
                     (UInt)t_auxhdr->magic);
         VG_(printf)("   vstamp       0x%04x\n", (UInt)t_auxhdr->vstamp);
         VG_(printf)("   tsize        %lld\n", (Long)t_auxhdr->tsize);
         VG_(printf)("   dsize        %lld\n", (Long)t_auxhdr->dsize);
         VG_(printf)("   bsize        %lld\n", (Long)t_auxhdr->bsize);
         VG_(printf)("   entry        0x%llx\n", (ULong)t_auxhdr->entry);
         VG_(printf)("   text_start   0x%llx (stated)\n",
                     (ULong)t_auxhdr->text_start);
         VG_(printf)("   data_start   0x%llx (stated)\n",
                     (ULong)t_auxhdr->data_start);
         VG_(printf)("   o_toc        0x%llx\n", (ULong)t_auxhdr->o_toc);
         VG_(printf)("   o_snentry    %d\n", (Int)t_auxhdr->o_snentry);
         VG_(printf)("   o_sntext     %d\n", (Int)t_auxhdr->o_sntext);
         VG_(printf)("   o_sndata     %d\n", (Int)t_auxhdr->o_sndata);
         VG_(printf)("   o_sntoc      %d\n", (Int)t_auxhdr->o_sntoc);
         VG_(printf)("   o_snloader   %d\n", (Int)t_auxhdr->o_snloader);
         VG_(printf)("   o_snbss      %d\n", (Int)t_auxhdr->o_snbss);
         VG_(printf)("   o_algntext   %d\n", (Int)t_auxhdr->o_algntext);
         VG_(printf)("   o_algndata   %d\n", (Int)t_auxhdr->o_algndata);
         VG_(printf)("   o_modtype    \"%c%c\"\n", 
                     (UChar)t_auxhdr->o_modtype[0],
                     (UChar)t_auxhdr->o_modtype[1] );
         VG_(printf)("   o_cpuflag    0x%02x\n", (UInt)t_auxhdr->o_cpuflag);
         VG_(printf)("   o_cputype    0x%02x\n", (UInt)t_auxhdr->o_cputype);
         VG_(printf)("   o_maxstack   %llu\n", (ULong)t_auxhdr->o_maxstack);
         VG_(printf)("   o_maxdata    %llu\n", (ULong)t_auxhdr->o_maxdata);
         VG_(printf)("   o_debugger   %u\n", t_auxhdr->o_debugger);
         /* printf("   o_textpsize  %u\n", (UInt)t_auxhdr->o_textpsize); */
         /* printf("   o_stackpsize %u\n", (UInt)t_auxhdr->o_stackpsize); */
      }

      text_svma       = t_auxhdr->text_start;
      text_svma_known = True;

      data_svma       = t_auxhdr->data_start;
      data_svma_known = True;

      /* The auxhdr may claim the data section is longer than
	 data_alen, so note the auxhdr-claimed size too. */
      data_alen_from_auxhdr = (UWord)t_auxhdr->dsize;

      if (t_auxhdr->o_sntoc == t_auxhdr->o_sndata) {
	 toc_svma       = (Addr)t_auxhdr->o_toc;
         toc_svma_known = True;
      }
   }

   /* ------------ Section Headers ------------ */
#  if defined(VGP_ppc32_aix5)
   if (sizeof(SCNHDR) != 40)
      BAD("readxcoff.c: invalid SCNHDR size (32-bit)");
#  elif defined(VGP_ppc64_aix5)
   if (sizeof(SCNHDR) != 72)
      BAD("readxcoff.c: invalid SCNHDR size (64-bit)");
#  else
#  error "Invalid platform"
#  endif

   SCNHDR* t_scnhdr = (SCNHDR*)cursor;

   if (SHOW)
      VG_(printf)("\nSection Headers: %d entries\n", t_filehdr->f_nscns);

   /* Where the stabs strings are in the oimage */
   UChar* oi_debug   = NULL;
   UWord  oi_n_debug = 0;

   /* Where the line number entries for the text section are
      in the oimage */
   UChar* oi_lnos      = NULL;
   UWord  oi_nent_lnos = 0; /* number of records */

   for (i = 0; i < t_filehdr->f_nscns; i++) {
      UChar sname_safe[9];
      for (j = 0; j < 8; j++) 
         sname_safe[j] = t_scnhdr[i].s_name[j];
      sname_safe[8] = 0;
      if (SHOW) {
         VG_(printf)("   --- #%d ---\n", i);
         VG_(printf)("   s_name    %s\n", sname_safe);
         VG_(printf)("   s_paddr   0x%llx\n", (ULong)t_scnhdr[i].s_paddr);
         VG_(printf)("   s_vaddr   0x%llx\n", (ULong)t_scnhdr[i].s_vaddr);
         VG_(printf)("   s_size    %lld\n",   (Long)t_scnhdr[i].s_size);
         VG_(printf)("   s_scnptr  %lld\n",   (Long)t_scnhdr[i].s_scnptr);
         VG_(printf)("   s_relptr  %lld\n",   (Long)t_scnhdr[i].s_relptr);
         VG_(printf)("   s_lnnoptr %lld\n",   (Long)t_scnhdr[i].s_lnnoptr);
         VG_(printf)("   s_nreloc  %llu\n",   (ULong)t_scnhdr[i].s_nreloc);
         VG_(printf)("   s_nlnno   %llu\n",   (ULong)t_scnhdr[i].s_nlnno);
         VG_(printf)("   s_flags   0x%llx (%s)\n", 
                     (ULong)t_scnhdr[i].s_flags,
                     name_of_scnhdr_s_flags(t_scnhdr[i].s_flags));
      }
      /* find the stabs strings */
      if (t_scnhdr[i].s_flags == STYP_DEBUG) {
         oi_debug = oimage;
         oi_debug += (UWord)t_scnhdr[i].s_scnptr;
         oi_n_debug = (UWord)t_scnhdr[i].s_size;
      }
      /* find the line number entries for the text section */
      if (t_scnhdr[i].s_flags == STYP_TEXT && t_scnhdr[i].s_lnnoptr > 0) {
         oi_lnos = oimage;
         oi_lnos += (UWord)t_scnhdr[i].s_lnnoptr;
         oi_nent_lnos = (UWord)t_scnhdr[i].s_nlnno;
         /* XCOFF is clearly the result of years of kludgery, and
            here's one place it shows.  .s_nlnno is a 16-bit field, so
            if there are 65535 or more entries, they can't be
            represented here.  In that case, the real number is stored
            in a 32-bit field of a an "overflow section header" - a
            dummy section header which has no purpose other than to
            hold the correct count.  And then this kludge applies to
            XCOFF32, not XCOFF64. */
         if (t_scnhdr[i].s_nlnno == 0xFFFF 
             || t_scnhdr[i].s_nreloc == 0xFFFF) {
            /* have to test both fields, according to the docs */
            /* find the relevant overflow header */
            for (j = 0; j < t_filehdr->f_nscns; j++)
               if (t_scnhdr[j].s_flags == STYP_OVRFLO 
                   && t_scnhdr[j].s_nlnno == i+1 /* ref to correct scn? */
                   && t_scnhdr[j].s_nreloc == i+1 /* also must check this */)
                  break;
            vg_assert(j >= 0 && j <= t_filehdr->f_nscns);
            if (j == t_filehdr->f_nscns)
               /* Hmm.  We're hosed.  Give up. */
               BAD("readxcoff.c: can't find a required "
                   "overflow section header");
            /* finally, we have the real count. */
            oi_nent_lnos = (UWord)t_scnhdr[j].s_vaddr;
         }
      }
      cursor += sizeof(SCNHDR);
   }
   if (SHOW) {
      VG_(printf)("\n   debug image (stabs strings) at %p size %ld bytes\n", 
                  oi_debug, oi_n_debug);
      VG_(printf)("   line number info at %p with %ld entries\n",
                  oi_lnos, oi_nent_lnos);
   }

   /* ------------ establish Text/data biases ------------ */

   /* Calculate, into text_bias, the offset that has to be added to
      symbol table values (stated VMAs) so as to convert them to correct 
      addresses in the running image (actual VMAs).  I can't find any 
      documentation for this, so the following is determined empirically.

      There appear to be two classes of loaded object:

      .o files.  These have a stated text VMA of zero, and so their
         symbols start from zero and work upwards.  In that case the
         bias is precisely the offset where the text section is 
         loaded (si->text_avma), that is, the actual text VMA.

         Except -- cryptically -- /usr/include/sys/ldr.h says that the
         ld_info.ldinfo_textorg field is "start of loaded program
         image (includes the XCOFF headers)".  And so to get the
         correct text bias it is necessary (determined empirically) to
         add on the file offset for the text section.  I guess this
         means that (1) it is assumed the text section is always the
         first in the file, and (2) in this case the stated text VMA
         is where the start of the file is mapped, not the start of
         the text section.

         Last verified 24 May 06.

      .so files, and executables.  These have a non-zero stated text 
         VMA, for example 0x10000150.  They appear to get loaded at some
         arbitrary address (actual VMA) which is always a whole number 
         of pages, eg 0x20002000, and in such a way that the offset is 
         a whole number of pages.  So in this example the offset (bias) 
         would be 0x20002000 - round_to_page_base(0x10000150).
   */
   if (text_svma_known) {
#if 0
      if (text_svma == 0) {
         text_bias = di->text_avma;
         if (sntext_1based_if_known >= 1 
             && sntext_1based_if_known <= t_filehdr->f_nscns)
            text_bias += t_scnhdr[sntext_1based_if_known - 1].s_scnptr;
      } else {
         text_bias = di->text_avma - VG_PGROUNDDN(text_svma);
      }
#else
      text_bias = di->text_avma - text_svma;
      if (sntext_1based_if_known >= 1 
          && sntext_1based_if_known <= t_filehdr->f_nscns)
         text_bias += t_scnhdr[sntext_1based_if_known - 1].s_scnptr;

#endif
      if (SHOW)
         VG_(printf)("   text section: stated vma 0x%lx, "
                     "actual vma 0x%lx, bias 0x%lx\n", 
                     text_svma, di->text_avma, text_bias);
   } else {
      text_bias = 0;
      if (SHOW)
         VG_(printf)("   text section: svma UNKNOWN, bias UNKNOWN\n");
   }

   if (data_svma_known) {
      data_bias = di->data_avma - data_svma;
      if (SHOW)
         VG_(printf)("   data section: stated vma 0x%lx, "
                     "actual vma 0x%lx, bias 0x%lx\n", 
                     data_svma, di->data_avma, data_bias);
   } else {
      data_bias = 0;
      if (SHOW)
         VG_(printf)("   data section: svma UNKNOWN, bias UNKNOWN\n");
   }

   if (toc_svma_known) {
      toc_avma = toc_svma + data_bias;
      if (SHOW)
         VG_(printf)("            toc: stated vma 0x%lx, actual vma 0x%lx\n",
                     toc_svma, toc_avma);
   } else {
      if (SHOW)
         VG_(printf)("            toc: svma UNKNOWN\n");
     toc_avma = 0;
   }

   /* ------------ Section Data ------------ */
   for (i = 0; i < t_filehdr->f_nscns; i++) {
      if (SHOW)
         VG_(printf)("\nSection Data (sec %d, \"%s\")\n", 
                     i, name_of_scnhdr_s_flags(t_scnhdr[i].s_flags) );
      switch (t_scnhdr[i].s_flags & 0xFFFF) {
         case STYP_LOADER:
            show_loader_section( di, oimage + t_scnhdr[i].s_scnptr, 
                                 t_scnhdr[i].s_size );
            break;
         default:
            if (SHOW)
               VG_(printf)("   Not handled yet\n");
            break;
      }
   }

   /* ------------ establish String Table ------------ */
   /* This is after the symbol table, if it exists at all. */
   /* This is a bit of a hack.  The easy way to find the string table
      is assume it immediately follows the symbol table.  That doesn't
      work if there is no symbol table; but on the other hand if there
      is no symbol table then there isn't much point in carrying on.
      Hence, if there is no symbol table we just give up here and
      claim to have successfully loaded zero symbols. */
   if (t_filehdr->f_nsyms == 0) {
      if (SHOW)
         VG_(printf)("Object contains no symbols.  Stopping here.\n");
      return True;
   }

   cursor = oimage;
   cursor += t_filehdr->f_symptr; /* symtab start */
   cursor += SYMESZ * t_filehdr->f_nsyms; /* strtab start */
   /* Does this fall inside the file image?  The first 4 bytes is the
      string table size, so we need to be able to see at least
      them. */
   UChar* oi_strtab   = NULL;
   UWord  oi_n_strtab = 0;
   if (cursor + 4 <= oimage_after) {
      oi_strtab = cursor;
      oi_n_strtab = (UWord)( *(UInt*)oi_strtab );
      if (0) {
         VG_(printf)("oimage       %p\n", oimage);
         VG_(printf)("oimage_after %p\n", oimage_after);
         VG_(printf)("cursor       %p\n", cursor);
      }
      if (oi_strtab + oi_n_strtab > oimage_after)
         BAD("readxcoff.c: string table exceeds image end");
   }

   /* ------------ Symbol Table ------------ */
   if (SHOW)
      VG_(printf)("\nSymbol Table: %llu entries\n", (ULong)t_filehdr->f_nsyms);
   cursor = oimage;
   cursor += t_filehdr->f_symptr;
   HChar* badness = read_symbol_table( 
                       di,
                       cursor, t_filehdr->f_nsyms, 
                       oi_strtab, oi_n_strtab,
                       oi_debug, oi_n_debug,
                       oi_lnos,  oi_nent_lnos,
                       sntext_1based_if_known, sndata_1based_if_known,
                       data_alen_from_auxhdr,
                       toc_avma,
                       text_bias, data_bias 
                    );
   if (badness)
      BAD(badness);
   /* cursor not used after this point */

   /* ------------ String Table ------------ */
   if (oi_strtab) {
      if (SHOW)
         VG_(printf)("\nString Table: %lu bytes\n", oi_n_strtab);
      i = 4;
      while (1) {
         if (i >= oi_n_strtab)
            break;
         if (SHOW && SHOW_STRTAB)
            VG_(printf)("  %5d  ", i);
         while (i < oi_n_strtab && oi_strtab[i]) {
            if (SHOW && SHOW_STRTAB)
               VG_(printf)("%c", sanitiseChar(oi_strtab[i]));
            i++;
         }
         i++;
         if (SHOW && SHOW_STRTAB)
            VG_(printf)("\n");
      }
   }

   if (SHOW)
      VG_(printf)("\n");
   return True;

#undef BAD
}


static ULong ascii_to_ULong ( void* vbuf, Int nbuf )
{
   Int    i;
   UChar  c;
   UChar* buf = (UChar*)vbuf;
   ULong  n = 0;
   for (i = 0; i < nbuf; i++) {
      c = buf[i];
      if (c >= '0' && c <= '9')
         n = 10ULL * n + (ULong)(c - '0');
   }
   return n;
}


/* Returns True on success, False if any kind of problem. */
static
Bool read_xcoff_o_or_a ( /*MOD*/struct _DebugInfo* di,
                         HChar* a_name, HChar* o_name )
{
   UChar* image   = NULL;
   Word   n_image = 0;
   Bool   ok;
   Int    i;
   SysRes sr, fd;

   struct vg_stat stat_buf;

   vg_assert(o_name);

   if (a_name == NULL) {
      /* This is just a plain XCOFF object file. */

      sr = VG_(stat)( o_name, &stat_buf );
      if (sr.isError) {
         ML_(symerr)(di, True, "can't stat XCOFF object file");
         return False;
      }

      n_image = stat_buf.st_size;
      if (SHOW && SHOW_AR_DETAILS)
         VG_(printf)("XCOFF object file size %ld\n", n_image);
      if (n_image <= 0) {
         ML_(symerr)(di, True, "implausible XCOFF object file size");
         return False;
      }

      fd = VG_(open)( o_name, VKI_O_RDONLY, 0 );
      if (fd.isError) {
         ML_(symerr)(di, True, "can't open XCOFF object file");
         return False;
      }

      sr = VG_(am_mmap_file_float_valgrind)(n_image, VKI_PROT_READ, 
                                                     fd.res, 0);
      VG_(close)(fd.res);

      if (sr.isError) {
         ML_(symerr)(di, True, "can't mmap XCOFF object file");
         return False;
      }

      image = (UChar*)sr.res;
      ok = read_xcoff_mapped_object( di, image, n_image );
      VG_(am_munmap_valgrind)( (Addr)image, n_image);

      /* assert OK */
      return ok;

   } else {

      /* It's an XCOFF .a file ("ar file format, large").  Map the
         whole thing in, find the member specified by O_NAME, and read
         symbols from that. */

      sr = VG_(stat)( a_name, &stat_buf );
      if (sr.isError) {
         ML_(symerr)(di, True, "can't stat XCOFF archive file");
         return False;
      }

      n_image = stat_buf.st_size;
      if (SHOW && SHOW_AR_DETAILS)
         VG_(printf)("XCOFF archive file size %ld\n", n_image);
      if (n_image <= 0) {
         ML_(symerr)(di, True, "implausible XCOFF archive file size");
         return False;
      }

      fd = VG_(open)( a_name, VKI_O_RDONLY, 0 );
      if (fd.isError) {
         ML_(symerr)(di, True, "can't open XCOFF archive file");
         return False;
      }

      sr = VG_(am_mmap_file_float_valgrind)(n_image, VKI_PROT_READ,
                                                     fd.res, 0);
      VG_(close)(fd.res);

      if (sr.isError) {
         ML_(symerr)(di, True, "can't mmap XCOFF archive file");
         return False;
      }

      image = (UChar*)sr.res;
      ok = False;

      /* Right.  Let's go looking for the requested object.  First, 
         peer at the archive's fixed header. */

      if (n_image < sizeof(FL_HDR)) {
         ML_(symerr)(di, True, "XCOFF archive too small for fixed header");
         goto done;
      }

      FL_HDR* fl_hdr = (FL_HDR*)image;
      if (SHOW && SHOW_AR_DETAILS) {
         VG_(printf)("magic:  %s\n", fl_hdr->fl_magic);
         VG_(printf)("memoff: %s\n", fl_hdr->fl_memoff);
         VG_(printf)("gstoff: %s\n", fl_hdr->fl_gstoff);
         VG_(printf)("gst64off: %s\n", fl_hdr->fl_gst64off);
      }

      { UChar* s = (UChar*)&fl_hdr->fl_magic;
        if (s[0] == '<' && s[1] == 'b' && s[2] == 'i' 
            && s[3] == 'g' && s[4] == 'a' && s[5] == 'f' 
            && s[6] == '>' && s[7] == '\n') {
           /* ok */
        } else {
           ML_(symerr)(di, True, 
                       "Is not XCOFF 'big'-variant .a format archive");
           goto done;
        }
      }

      /* Get a pointer to the member table entry. */
      UChar* mtabC = image + ascii_to_ULong(&fl_hdr->fl_memoff, 
                                            sizeof(fl_hdr->fl_memoff));
      AR_HDR* mt_hdr = (AR_HDR*)mtabC;

      if (mtabC < image || mtabC + sizeof(AR_HDR) > image + n_image) {
         ML_(symerr)(di, True, 
                     "XCOFF archive member table header exceeds image");
         goto done;
      }

      /* should be: backquote newline */
      if (mt_hdr->_ar_name.ar_name[0] != 0x60 /* backquote */
          || mt_hdr->_ar_name.ar_name[1] != 0x0A /* \n */) {
         ML_(symerr)(di, True, 
                     "XCOFF archive member table header is invalid");
         goto done;
      }

      if (SHOW) {
         VG_(printf)("member table ar_size = %lld\n", 
                     ascii_to_ULong(&mt_hdr->ar_size,20));
         VG_(printf)("member table ar_namlen = %lld\n", 
                     ascii_to_ULong(&mt_hdr->ar_namlen,4));
      }

      if (mtabC < image 
          || mtabC + sizeof(AR_HDR) 
                   + ascii_to_ULong(&mt_hdr->ar_size, 20) 
             > image + n_image) {
         ML_(symerr)(di, True, "XCOFF archive member table exceeds image");
         goto done;
      }

      UChar* data = mtabC + sizeof(AR_HDR) 
                          + ascii_to_ULong(&mt_hdr->ar_namlen,4);
      /* ALIGN */
      if ( ((UWord)data) & 1 ) data++;
      if (SHOW)
         VG_(printf)("member table data = %p\n", data);

      UInt nmembers = ascii_to_ULong(data, 20);
      if (SHOW)
          VG_(printf)("member table contains %d entries\n", nmembers);
      for (i = 0; i < nmembers; i++) {
         if (SHOW && SHOW_AR_DETAILS)
            VG_(printf)("   %d has off %d\n", 
                        i, (Int)ascii_to_ULong(data + 20 + 20*i, 20));
      }

      UChar* p = data + 20 + 20*nmembers;

      for (i = 0; i < nmembers; i++) {

         if (0 != VG_(strcmp)(p, o_name))
            goto move_on;

         UInt objoff = ascii_to_ULong(data + 20 + 20*i, 20);

         if (SHOW && SHOW_AR_DETAILS)
            VG_(printf)("got offset = %u\n", objoff);

         vg_assert(ok == False);

         /* Sanity check the selected member */
         UChar* o_hdrC = image + objoff;
         if (o_hdrC + sizeof(AR_HDR) >= image + n_image) {
            ML_(symerr)(di, True, 
                        "XCOFF archive member header exceeds image");
            goto done;
         }
         AR_HDR* o_hdr  = (AR_HDR*)o_hdrC;
         UWord   o_size = (UWord)ascii_to_ULong(&o_hdr->ar_size, 20);
         UChar*  o_data = o_hdrC + sizeof(AR_HDR)
                                 + (UWord)ascii_to_ULong(&o_hdr->ar_namlen,4);

         /* ALIGN */
         if ( ((UWord)o_data) & 1 ) o_data++;

         if (SHOW)
            VG_(printf)("member data = %p, size = %ld\n", o_data, o_size);

         if (!(o_data >= image && o_data + o_size <= image + n_image)) {
            ML_(symerr)(di, True, 
                        "XCOFF archive member exceeds image");
            goto done;
         }

         if (o_size < sizeof(FILHDR)) {
            ML_(symerr)(di, True, 
                        "XCOFF object file header is implausibly small (1)");
	    goto done;
	 }

         /* It's the right name, but need to also check the magic
            number, since some archives contain both a 32-bit and
            64-bit version of the same object. */
         FILHDR* t_filhdr = (FILHDR*)o_data;
#        if defined(VGP_ppc32_aix5)
         if (t_filhdr->f_magic == 0x01F7 /* XCOFF64 */) {
            if (0)
               VG_(printf)("Skipping 64-bit archive on 32-bit platform\n");
            goto move_on;
         }
#        elif defined(VGP_ppc64_aix5)
         if (t_filhdr->f_magic == 0x01DF /* XCOFF32 */) {
            if (0)
               VG_(printf)("Skipping 32-bit archive on 64-bit platform\n");
            goto move_on;
         }
#        endif

         if (SHOW && SHOW_AR_DETAILS)
            VG_(printf)("\nimage: %p-%p   object: %p-%p\n\n", 
                        image, image+n_image-1, o_data, o_data+o_size-1);
         ok = read_xcoff_mapped_object( di, o_data, o_size );
         goto done;

         vg_assert(0);
	 /* NOTREACHED */

        move_on:
         while (*p) {
            if (SHOW && SHOW_AR_DETAILS)
               VG_(printf)("%c", *p);
            p++;
         }
         if (SHOW && SHOW_AR_DETAILS)
            VG_(printf)("\n");
         p++;
      }

      vg_assert(i == nmembers);
      ML_(symerr)(di, True, "can't find object in XCOFF archive file");

     done:
      if (image) {
         VG_(am_munmap_valgrind)( (Addr)image, n_image );
         /* assert munmap succeeded */
      }
      return ok;

   }
}


/* Main entry point for XCOFF reading.  The following di fields must
   be filled in by the caller:

     filename
     memname (optional)
     text_avma, text_size
     data_avma, data_size

   and all other fields should be zeroed.
*/
Bool ML_(read_xcoff_debug_info) ( struct _DebugInfo* di,
                                  Bool is_mainexe )
{
   Bool ok;

   if (VG_(clo_verbosity) > 1 || VG_(clo_trace_redir)) {
      if (di->memname) {
         VG_(message)(Vg_DebugMsg, "Reading syms from %s(%s) (%#lx)\n",
                      di->filename, di->memname, di->text_avma);
      } else {
         VG_(message)(Vg_DebugMsg, "Reading syms from %s (%#lx)\n",
                      di->filename, di->text_avma);
      }
   }

   if (SHOW) {
      VG_(printf)("------------------- BEGIN read xcoff ------------------\n");
      VG_(printf)("---         file: %s\n",  di->filename);
      VG_(printf)("---          mem: %s\n",  di->memname ? di->memname  
                                                         : (UChar*)"(none)" );
      VG_(printf)("--- t actual vma: %#lx\n", di->text_avma);
      VG_(printf)("--- t actual len: %ld\n",  di->text_size);
      VG_(printf)("--- d actual vma: %#lx\n", di->data_avma);
      VG_(printf)("--- d actual len: %ld\n",  di->data_size);
   }

   if (di->memname) {
      /* XCOFF .a file.  di->filename is its name, di->memname is the
         name of the required .o within it. */
      ok = read_xcoff_o_or_a( di, di->filename, di->memname );
   } else {
      /* no archive member name, so di->filename is an XCOFF object */
      ok = read_xcoff_o_or_a( di, NULL, di->filename );
   }

   di->soname = NULL;
   if (ok) {
      if (is_mainexe) {
         di->soname = "NONE";
      } else {
         UChar* p = VG_(strrchr)(di->filename, '/');
         p = p  ? p+1  : di->filename;
         /* p points at the main filename */
         if (di->memname) {
            /* set the soname to "archive.a(member.o)" */
            Int nbytes = VG_(strlen)(p) + 1 + VG_(strlen)(di->memname) + 1 + 1;
            UChar* so = ML_(dinfo_zalloc)("di.readxcoff.rxdi.1", nbytes);
            vg_assert(so);
            VG_(sprintf)(so, "%s(%s)", p, di->memname);
            vg_assert(VG_(strlen)(so) == nbytes-1);
            di->soname = so;
         } else {
            /* no member name, hence soname = "archive.a" */
            di->soname = ML_(dinfo_strdup)("di.readxcoff.rxdi.2", p);
         }
      }
      if (SHOW)
         VG_(printf)("Setting soname to %s\n", di->soname);
   }

   if (SHOW)
      VG_(printf)("------------------- END read xcoff ------------------\n\n");

   return ok;
}

#endif // defined(VGO_aix5)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
