
/*--------------------------------------------------------------------*/
/*--- Format-neutral storage of and querying of info acquired from ---*/
/*--- ELF/XCOFF stabs/dwarf1/dwarf2/dwarf3 debug info.             ---*/
/*---                                                    storage.c ---*/
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

/* This file manages the data structures built by the debuginfo
   system.  These are: the top level SegInfo list.  For each SegInfo,
   there are tables for for address-to-symbol mappings,
   address-to-src-file/line mappings, and address-to-CFI-info
   mappings.
*/

#include "pub_core_basics.h"
#include "pub_core_options.h"      /* VG_(clo_verbosity) */
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_xarray.h"
#include "priv_storage.h"          /* self */


/*------------------------------------------------------------*/
/*--- Misc (printing, errors)                              ---*/
/*------------------------------------------------------------*/

/* Show a non-fatal debug info reading error.  Use vg_panic if
   terminal. */
void ML_(symerr) ( HChar* msg )
{
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, "%s", msg);
}

/* Print a symbol. */
void ML_(ppSym) ( Int idx, DiSym* sym )
{
  VG_(printf)( "%5d:  %8p .. %8p (%d)      %s\n",
               idx,
               sym->addr, 
               sym->addr + sym->size - 1, sym->size,
	       sym->name );
}

/* Print a call-frame-info summary. */
void ML_(ppDiCfSI) ( XArray* /* of CfiExpr */ exprs, DiCfSI* si )
{
#  define SHOW_HOW(_how, _off)                   \
      do {                                       \
         if (_how == CFIR_UNKNOWN) {             \
            VG_(printf)("Unknown");              \
         } else                                  \
         if (_how == CFIR_SAME) {                \
            VG_(printf)("Same");                 \
         } else                                  \
         if (_how == CFIR_CFAREL) {              \
            VG_(printf)("cfa+%d", _off);         \
         } else                                  \
         if (_how == CFIR_MEMCFAREL) {           \
            VG_(printf)("*(cfa+%d)", _off);      \
         } else                                  \
         if (_how == CFIR_EXPR) {                \
            VG_(printf)("{");                    \
            ML_(ppCfiExpr)(exprs, _off);         \
            VG_(printf)("}");                    \
         } else {                                \
            vg_assert(0+0);                      \
         }                                       \
      } while (0)

   VG_(printf)("[%p .. %p]: ", si->base, 
                               si->base + (UWord)si->len - 1);
   switch (si->cfa_how) {
      case CFIC_SPREL: 
         VG_(printf)("let cfa=oldSP+%d", si->cfa_off); 
         break;
      case CFIC_FPREL: 
         VG_(printf)("let cfa=oldFP+%d", si->cfa_off); 
         break;
      case CFIC_EXPR: 
         VG_(printf)("let cfa={"); 
         ML_(ppCfiExpr)(exprs, si->cfa_off);
         VG_(printf)("}"); 
         break;
      default: 
         vg_assert(0);
   }

   VG_(printf)(" in RA=");
   SHOW_HOW(si->ra_how, si->ra_off);
   VG_(printf)(" SP=");
   SHOW_HOW(si->sp_how, si->sp_off);
   VG_(printf)(" FP=");
   SHOW_HOW(si->fp_how, si->fp_off);
   VG_(printf)("\n");
#  undef SHOW_HOW
}


/*------------------------------------------------------------*/
/*--- Adding stuff                                         ---*/
/*------------------------------------------------------------*/

/* Add a str to the string table, including terminating zero, and
   return pointer to the string in vg_strtab.  Unless it's been seen
   recently, in which case we find the old pointer and return that.
   This avoids the most egregious duplications.

   JSGF: changed from returning an index to a pointer, and changed to
   a chunking memory allocator rather than reallocating, so the
   pointers are stable.
*/
UChar* ML_(addStr) ( struct _SegInfo* si, UChar* str, Int len )
{
   struct strchunk *chunk;
   Int    space_needed;
   UChar* p;

   if (len == -1)
      len = VG_(strlen)(str);

   space_needed = 1 + len;

   // Allocate a new strtab chunk if necessary
   if (si->strchunks == NULL || 
       (si->strchunks->strtab_used 
        + space_needed) > SEGINFO_STRCHUNKSIZE) {
      chunk = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*chunk));
      chunk->strtab_used = 0;
      chunk->next = si->strchunks;
      si->strchunks = chunk;
   }
   chunk = si->strchunks;

   p = &chunk->strtab[chunk->strtab_used];
   VG_(memcpy)(p, str, len);
   chunk->strtab[chunk->strtab_used+len] = '\0';
   chunk->strtab_used += space_needed;

   return p;
}


/* Add a symbol to the symbol table. 
*/
void ML_(addSym) ( struct _SegInfo* si, DiSym* sym )
{
   UInt   new_sz, i;
   DiSym* new_tab;

   /* Ignore zero-sized syms. */
   if (sym->size == 0) return;

   if (si->symtab_used == si->symtab_size) {
      new_sz = 2 * si->symtab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = VG_(arena_malloc)(VG_AR_SYMTAB, new_sz * sizeof(DiSym) );
      if (si->symtab != NULL) {
         for (i = 0; i < si->symtab_used; i++)
            new_tab[i] = si->symtab[i];
         VG_(arena_free)(VG_AR_SYMTAB, si->symtab);
      }
      si->symtab = new_tab;
      si->symtab_size = new_sz;
   }

   si->symtab[si->symtab_used] = *sym;
   si->symtab_used++;
   vg_assert(si->symtab_used <= si->symtab_size);
}


/* Add a location to the location table. 
*/
static void addLoc ( struct _SegInfo* si, DiLoc* loc )
{
   UInt   new_sz, i;
   DiLoc* new_tab;

   /* Zero-sized locs should have been ignored earlier */
   vg_assert(loc->size > 0);

   if (si->loctab_used == si->loctab_size) {
      new_sz = 2 * si->loctab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = VG_(arena_malloc)(VG_AR_SYMTAB, new_sz * sizeof(DiLoc) );
      if (si->loctab != NULL) {
         for (i = 0; i < si->loctab_used; i++)
            new_tab[i] = si->loctab[i];
         VG_(arena_free)(VG_AR_SYMTAB, si->loctab);
      }
      si->loctab = new_tab;
      si->loctab_size = new_sz;
   }

   si->loctab[si->loctab_used] = *loc;
   si->loctab_used++;
   vg_assert(si->loctab_used <= si->loctab_size);
}


/* Top-level place to call to add a source-location mapping entry.
*/
void ML_(addLineInfo) ( struct _SegInfo* si,
                        UChar*   filename,
                        UChar*   dirname, /* NULL == directory is unknown */
                        Addr     this,
                        Addr     next,
                        Int      lineno,
                        Int      entry /* only needed for debug printing */
     )
{
   static const Bool debug = False;
   DiLoc loc;
   Int size = next - this;

   /* Ignore zero-sized locs */
   if (this == next) return;

   if (debug)
      VG_(printf)( "  src %s %s line %d %p-%p\n",
                   dirname ? dirname : (UChar*)"(unknown)",
                   filename, lineno, this, next );

   /* Maximum sanity checking.  Some versions of GNU as do a shabby
    * job with stabs entries; if anything looks suspicious, revert to
    * a size of 1.  This should catch the instruction of interest
    * (since if using asm-level debug info, one instruction will
    * correspond to one line, unlike with C-level debug info where
    * multiple instructions can map to the one line), but avoid
    * catching any other instructions bogusly. */
   if (this > next) {
       if (VG_(clo_verbosity) > 2) {
           VG_(message)(Vg_DebugMsg, 
                        "warning: line info addresses out of order "
                        "at entry %d: 0x%x 0x%x", entry, this, next);
       }
       size = 1;
   }

   if (size > MAX_LOC_SIZE) {
       if (0)
       VG_(message)(Vg_DebugMsg, 
                    "warning: line info address range too large "
                    "at entry %d: %d", entry, size);
       size = 1;
   }

   /* vg_assert(this < si->text_start_avma + si->size 
                && next-1 >= si->text_start_avma); */
   if (this >= si->text_start_avma + si->text_size 
       || next-1 < si->text_start_avma) {
       if (0)
          VG_(message)(Vg_DebugMsg, 
                       "warning: ignoring line info entry falling "
                       "outside current SegInfo: %p %p %p %p",
                       si->text_start_avma, 
                       si->text_start_avma + si->text_size, 
                       this, next-1);
       return;
   }

   vg_assert(lineno >= 0);
   if (lineno > MAX_LINENO) {
      static Bool complained = False;
      if (!complained) {
         complained = True;
         VG_(message)(Vg_UserMsg, 
                      "warning: ignoring line info entry with "
                      "huge line number (%d)", lineno);
         VG_(message)(Vg_UserMsg, 
                      "         Can't handle line numbers "
                      "greater than %d, sorry", MAX_LINENO);
         VG_(message)(Vg_UserMsg, 
                      "(Nb: this message is only shown once)");
      }
      return;
   }

   loc.addr      = this;
   loc.size      = (UShort)size;
   loc.lineno    = lineno;
   loc.filename  = filename;
   loc.dirname   = dirname;

   if (0) VG_(message)(Vg_DebugMsg, 
		       "addLoc: addr %p, size %d, line %d, file %s",
		       this,size,lineno,filename);

   addLoc ( si, &loc );
}


/* Top-level place to call to add a CFI summary record.  The supplied
   DiCfSI is copied. */
void ML_(addDiCfSI) ( struct _SegInfo* si, DiCfSI* cfsi )
{
   static const Bool debug = False;
   UInt    new_sz, i;
   DiCfSI* new_tab;

   if (debug) {
      VG_(printf)("adding DiCfSI: ");
      ML_(ppDiCfSI)(si->cfsi_exprs, cfsi);
   }

   /* sanity */
   vg_assert(cfsi->len > 0);
   /* If this fails, the implication is you have a single procedure
      with more than 5 million bytes of code.  Which is pretty
      unlikely.  Either that, or the debuginfo reader is somehow
      broken. */
   vg_assert(cfsi->len < 5000000);

   /* Rule out ones which are completely outside the segment.  These
      probably indicate some kind of bug, but for the meantime ignore
      them. */
   if ( cfsi->base + cfsi->len - 1  <  si->text_start_avma
        || si->text_start_avma + si->text_size - 1  <  cfsi->base ) {
      static Int complaints = 3;
      if (VG_(clo_trace_cfi) || complaints > 0) {
         complaints--;
         if (VG_(clo_verbosity) > 1) {
            VG_(message)(
               Vg_DebugMsg,
               "warning: DiCfSI %p .. %p outside segment %p .. %p",
               cfsi->base, 
               cfsi->base + cfsi->len - 1,
               si->text_start_avma,
               si->text_start_avma + si->text_size - 1 
            );
         }
         if (VG_(clo_trace_cfi)) 
            ML_(ppDiCfSI)(si->cfsi_exprs, cfsi);
      }
      return;
   }

   if (si->cfsi_used == si->cfsi_size) {
      new_sz = 2 * si->cfsi_size;
      if (new_sz == 0) new_sz = 20;
      new_tab = VG_(arena_malloc)(VG_AR_SYMTAB, new_sz * sizeof(DiCfSI) );
      if (si->cfsi != NULL) {
         for (i = 0; i < si->cfsi_used; i++)
            new_tab[i] = si->cfsi[i];
         VG_(arena_free)(VG_AR_SYMTAB, si->cfsi);
      }
      si->cfsi = new_tab;
      si->cfsi_size = new_sz;
   }

   si->cfsi[si->cfsi_used] = *cfsi;
   si->cfsi_used++;
   vg_assert(si->cfsi_used <= si->cfsi_size);
}


Int ML_(CfiExpr_Undef)( XArray* dst )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_Undef;
   return VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_Deref)( XArray* dst, Int ixAddr )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_Deref;
   e.Cex.Deref.ixAddr = ixAddr;
   return VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_Const)( XArray* dst, UWord con )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_Const;
   e.Cex.Const.con = con;
   return VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_Binop)( XArray* dst, CfiOp op, Int ixL, Int ixR )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_Binop;
   e.Cex.Binop.op  = op;
   e.Cex.Binop.ixL = ixL;
   e.Cex.Binop.ixR = ixR;
   return VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_CfiReg)( XArray* dst, CfiReg reg )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_CfiReg;
   e.Cex.CfiReg.reg = reg;
   return VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_DwReg)( XArray* dst, Int reg )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_DwReg;
   e.Cex.DwReg.reg = reg;
   return VG_(addToXA)( dst, &e );
}

static void ppCfiOp ( CfiOp op ) 
{
   switch (op) {
      case Cop_Add: VG_(printf)("+"); break;
      case Cop_Sub: VG_(printf)("-"); break;
      case Cop_And: VG_(printf)("&"); break;
      case Cop_Mul: VG_(printf)("*"); break;
      default:      vg_assert(0);
   }
}

static void ppCfiReg ( CfiReg reg )
{
   switch (reg) {
      case Creg_SP: VG_(printf)("SP"); break;
      case Creg_FP: VG_(printf)("FP"); break;
      case Creg_IP: VG_(printf)("IP"); break;
      default: vg_assert(0);
   }
}

void ML_(ppCfiExpr)( XArray* src, Int ix )
{
   /* VG_(indexXA) checks for invalid src/ix values, so we can
      use it indiscriminately. */
   CfiExpr* e = (CfiExpr*) VG_(indexXA)( src, ix );
   switch (e->tag) {
      case Cex_Undef: 
         VG_(printf)("Undef"); 
         break;
      case Cex_Deref: 
         VG_(printf)("*("); 
         ML_(ppCfiExpr)(src, e->Cex.Deref.ixAddr);
         VG_(printf)(")"); 
         break;
      case Cex_Const: 
         VG_(printf)("0x%lx", e->Cex.Const.con); 
         break;
      case Cex_Binop: 
         VG_(printf)("(");
         ML_(ppCfiExpr)(src, e->Cex.Binop.ixL);
         VG_(printf)(")");
         ppCfiOp(e->Cex.Binop.op);
         VG_(printf)("(");
         ML_(ppCfiExpr)(src, e->Cex.Binop.ixR);
         VG_(printf)(")");
         break;
      case Cex_CfiReg:
         ppCfiReg(e->Cex.CfiReg.reg);
         break;
      case Cex_DwReg:
         VG_(printf)("dwr%d", e->Cex.DwReg.reg);
         break;
      default: 
         VG_(core_panic)("ML_(ppCfiExpr)"); 
         /*NOTREACHED*/
         break;
   }
}


/*------------------------------------------------------------*/
/*--- Canonicalisers                                       ---*/
/*------------------------------------------------------------*/

/* Sort the symtab by starting address, and emit warnings if any
   symbols have overlapping address ranges.  We use that old chestnut,
   shellsort.  Mash the table around so as to establish the property
   that addresses are in order and the ranges to not overlap.  This
   facilitates using binary search to map addresses to symbols when we
   come to query the table.
*/
static Int compare_DiSym ( void* va, void* vb ) 
{
   DiSym* a = (DiSym*)va;
   DiSym* b = (DiSym*)vb;
   if (a->addr < b->addr) return -1;
   if (a->addr > b->addr) return  1;
   return 0;
}


/* Two symbols have the same address.  Which name do we prefer?

   The general rule is to prefer the shorter symbol name.  If the
   symbol contains a '@', which means its versioned, then the length
   up to the '@' is used for length comparison purposes (so
   "foo@GLIBC_2.4.2" is considered shorter than "foobar"), but if two
   symbols have the same length, the one with the version string is
   preferred.  If all else fails, use alphabetical ordering.

   Very occasionally this goes wrong (eg. 'memcmp' and 'bcmp' are
   aliases in glibc, we choose the 'bcmp' symbol because it's shorter,
   so we can misdescribe memcmp() as bcmp()).  This is hard to avoid.
   It's mentioned in the FAQ file.
 */
static DiSym* prefersym ( struct _SegInfo* si, DiSym* a, DiSym* b )
{
   Int lena, lenb;		/* full length */
   Int vlena, vlenb;		/* length without version */
   const UChar *vpa, *vpb;

   vlena = lena = VG_(strlen)(a->name);
   vlenb = lenb = VG_(strlen)(b->name);

   vpa = VG_(strchr)(a->name, '@');
   vpb = VG_(strchr)(b->name, '@');

   if (vpa)
      vlena = vpa - a->name;
   if (vpb)
      vlenb = vpb - b->name;

   TRACE_SYMTAB("choosing between '%s' and '%s'\n", a->name, b->name);

   /* MPI hack: prefer PMPI_Foo over MPI_Foo */
   if (0==VG_(strncmp)(a->name, "MPI_", 4)
       && 0==VG_(strncmp)(b->name, "PMPI_", 5)
       && 0==VG_(strcmp)(a->name, 1+b->name))
      return b;
   else
   if (0==VG_(strncmp)(b->name, "MPI_", 4)
       && 0==VG_(strncmp)(a->name, "PMPI_", 5)
       && 0==VG_(strcmp)(b->name, 1+a->name))
      return a;

   /* Select the shortest unversioned name */
   if (vlena < vlenb)
      return a;
   else if (vlenb < vlena)
      return b;

   /* Equal lengths; select the versioned name */
   if (vpa && !vpb)
      return a;
   if (vpb && !vpa)
      return b;

   /* Either both versioned or neither is versioned; select them
      alphabetically */
   if (VG_(strcmp)(a->name, b->name) < 0)
      return a;
   else
      return b;
}

static void canonicaliseSymtab ( struct _SegInfo* si )
{
   Int   i, j, n_merged, n_truncated;
   Addr  s1, s2, e1, e2;

#  define SWAP(ty,aa,bb) \
      do { ty tt = (aa); (aa) = (bb); (bb) = tt; } while (0)

   if (si->symtab_used == 0)
      return;

   VG_(ssort)(si->symtab, si->symtab_used, 
                          sizeof(*si->symtab), compare_DiSym);

  cleanup_more:
 
   /* If two symbols have identical address ranges, we pick one
      using prefersym() (see it for details). */
   do {
      n_merged = 0;
      j = si->symtab_used;
      si->symtab_used = 0;
      for (i = 0; i < j; i++) {
         if (i < j-1
             && si->symtab[i].addr   == si->symtab[i+1].addr
             && si->symtab[i].size   == si->symtab[i+1].size) {
            n_merged++;
            /* merge the two into one */
	    si->symtab[si->symtab_used++] 
               = *prefersym(si, &si->symtab[i], &si->symtab[i+1]);
            i++;
         } else {
            si->symtab[si->symtab_used++] = si->symtab[i];
         }
      }
      TRACE_SYMTAB( "%d merged\n", n_merged);
   }
   while (n_merged > 0);

   /* Detect and "fix" overlapping address ranges. */
   n_truncated = 0;

   for (i = 0; i < ((Int)si->symtab_used) -1; i++) {

      vg_assert(si->symtab[i].addr <= si->symtab[i+1].addr);

      /* Check for common (no overlap) case. */ 
      if (si->symtab[i].addr + si->symtab[i].size 
          <= si->symtab[i+1].addr)
         continue;

      /* There's an overlap.  Truncate one or the other. */
      if (si->trace_symtab) {
         VG_(printf)("overlapping address ranges in symbol table\n\t");
         ML_(ppSym)( i, &si->symtab[i] );
         VG_(printf)("\t");
         ML_(ppSym)( i+1, &si->symtab[i+1] );
         VG_(printf)("\n");
      }

      /* Truncate one or the other. */
      s1 = si->symtab[i].addr;
      s2 = si->symtab[i+1].addr;
      e1 = s1 + si->symtab[i].size - 1;
      e2 = s2 + si->symtab[i+1].size - 1;
      if (s1 < s2) {
         e1 = s2-1;
      } else {
         vg_assert(s1 == s2);
         if (e1 > e2) { 
            s1 = e2+1; SWAP(Addr,s1,s2); SWAP(Addr,e1,e2); 
         } else 
         if (e1 < e2) {
            s2 = e1+1;
         } else {
	   /* e1 == e2.  Identical addr ranges.  We'll eventually wind
              up back at cleanup_more, which will take care of it. */
	 }
      }
      si->symtab[i].addr   = s1;
      si->symtab[i+1].addr = s2;
      si->symtab[i].size   = e1 - s1 + 1;
      si->symtab[i+1].size = e2 - s2 + 1;
      vg_assert(s1 <= s2);
      vg_assert(si->symtab[i].size > 0);
      vg_assert(si->symtab[i+1].size > 0);
      /* It may be that the i+1 entry now needs to be moved further
         along to maintain the address order requirement. */
      j = i+1;
      while (j < ((Int)si->symtab_used)-1 
             && si->symtab[j].addr > si->symtab[j+1].addr) {
         SWAP(DiSym,si->symtab[j],si->symtab[j+1]);
         j++;
      }
      n_truncated++;
   }

   if (n_truncated > 0) goto cleanup_more;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < ((Int)si->symtab_used)-1; i++) {
      /* No zero-sized symbols. */
      vg_assert(si->symtab[i].size > 0);
      /* In order. */
      vg_assert(si->symtab[i].addr < si->symtab[i+1].addr);
      /* No overlaps. */
      vg_assert(si->symtab[i].addr + si->symtab[i].size - 1
                < si->symtab[i+1].addr);
   }
#  undef SWAP
}


/* Sort the location table by starting address.  Mash the table around
   so as to establish the property that addresses are in order and the
   ranges do not overlap.  This facilitates using binary search to map
   addresses to locations when we come to query the table.
*/
static Int compare_DiLoc ( void* va, void* vb ) 
{
   DiLoc* a = (DiLoc*)va;
   DiLoc* b = (DiLoc*)vb;
   if (a->addr < b->addr) return -1;
   if (a->addr > b->addr) return  1;
   return 0;
}

static void canonicaliseLoctab ( struct _SegInfo* si )
{
   Int i, j;

#  define SWAP(ty,aa,bb) \
      do { ty tt = (aa); (aa) = (bb); (bb) = tt; } while (0);

   if (si->loctab_used == 0)
      return;

   /* Sort by start address. */
   VG_(ssort)(si->loctab, si->loctab_used, 
                          sizeof(*si->loctab), compare_DiLoc);

   /* If two adjacent entries overlap, truncate the first. */
   for (i = 0; i < ((Int)si->loctab_used)-1; i++) {
      vg_assert(si->loctab[i].size < 10000);
      if (si->loctab[i].addr + si->loctab[i].size > si->loctab[i+1].addr) {
         /* Do this in signed int32 because the actual .size fields
            are only 12 bits. */
         Int new_size = si->loctab[i+1].addr - si->loctab[i].addr;
         if (new_size < 0) {
            si->loctab[i].size = 0;
         } else
         if (new_size > MAX_LOC_SIZE) {
           si->loctab[i].size = MAX_LOC_SIZE;
         } else {
           si->loctab[i].size = (UShort)new_size;
         }
      }
   }

   /* Zap any zero-sized entries resulting from the truncation
      process. */
   j = 0;
   for (i = 0; i < (Int)si->loctab_used; i++) {
      if (si->loctab[i].size > 0) {
         if (j != i)
            si->loctab[j] = si->loctab[i];
         j++;
      }
   }
   si->loctab_used = j;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < ((Int)si->loctab_used)-1; i++) {
      /* 
      VG_(printf)("%d   (%d) %d 0x%x\n", 
                   i, si->loctab[i+1].confident, 
                   si->loctab[i+1].size, si->loctab[i+1].addr );
      */
      /* No zero-sized symbols. */
      vg_assert(si->loctab[i].size > 0);
      /* In order. */
      vg_assert(si->loctab[i].addr < si->loctab[i+1].addr);
      /* No overlaps. */
      vg_assert(si->loctab[i].addr + si->loctab[i].size - 1
                < si->loctab[i+1].addr);
   }
#  undef SWAP
}


/* Sort the call-frame-info table by starting address.  Mash the table
   around so as to establish the property that addresses are in order
   and the ranges do not overlap.  This facilitates using binary
   search to map addresses to locations when we come to query the
   table.  

   Also, set cfisi_minaddr and cfisi_maxaddr to be the min and max of
   any of the address ranges contained in cfisi[0 .. cfisi_used-1], so
   as to facilitate rapidly skipping this SegInfo when looking for an
   address which falls outside that range.
*/
static Int compare_DiCfSI ( void* va, void* vb )
{
   DiCfSI* a = (DiCfSI*)va;
   DiCfSI* b = (DiCfSI*)vb;
   if (a->base < b->base) return -1;
   if (a->base > b->base) return  1;
   return 0;
}

static void canonicaliseCFI ( struct _SegInfo* si )
{
   Int   i, j;
   const Addr minAddr = 0;
   const Addr maxAddr = ~minAddr;

   /* Note: take care in here.  si->cfsi can be NULL, in which
      case _used and _size fields will be zero. */
   if (si->cfsi == NULL) {
      vg_assert(si->cfsi_used == 0);
      vg_assert(si->cfsi_size == 0);
   }

   /* Set cfsi_minaddr and cfsi_maxaddr to summarise the entire
      address range contained in cfsi[0 .. cfsi_used-1]. */
   si->cfsi_minaddr = maxAddr; 
   si->cfsi_maxaddr = minAddr;
   for (i = 0; i < (Int)si->cfsi_used; i++) {
      Addr here_min = si->cfsi[i].base;
      Addr here_max = si->cfsi[i].base + si->cfsi[i].len - 1;
      if (here_min < si->cfsi_minaddr)
         si->cfsi_minaddr = here_min;
      if (here_max > si->cfsi_maxaddr)
         si->cfsi_maxaddr = here_max;
   }

   if (si->trace_cfi)
      VG_(printf)("canonicaliseCfiSI: %d entries, %p .. %p\n", 
                  si->cfsi_used,
	          si->cfsi_minaddr, si->cfsi_maxaddr);

   /* Sort the cfsi array by base address. */
   VG_(ssort)(si->cfsi, si->cfsi_used, sizeof(*si->cfsi), compare_DiCfSI);

   /* If two adjacent entries overlap, truncate the first. */
   for (i = 0; i < (Int)si->cfsi_used-1; i++) {
      if (si->cfsi[i].base + si->cfsi[i].len > si->cfsi[i+1].base) {
         Int new_len = si->cfsi[i+1].base - si->cfsi[i].base;
         /* how could it be otherwise?  The entries are sorted by the
            .base field. */         
         vg_assert(new_len >= 0);
	 vg_assert(new_len <= si->cfsi[i].len);
         si->cfsi[i].len = new_len;
      }
   }

   /* Zap any zero-sized entries resulting from the truncation
      process. */
   j = 0;
   for (i = 0; i < (Int)si->cfsi_used; i++) {
      if (si->cfsi[i].len > 0) {
         if (j != i)
            si->cfsi[j] = si->cfsi[i];
         j++;
      }
   }
   /* VG_(printf)("XXXXXXXXXXXXX %d %d\n", si->cfsi_used, j); */
   si->cfsi_used = j;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < (Int)si->cfsi_used; i++) {
      /* No zero-length ranges. */
      vg_assert(si->cfsi[i].len > 0);
      /* Makes sense w.r.t. summary address range */
      vg_assert(si->cfsi[i].base >= si->cfsi_minaddr);
      vg_assert(si->cfsi[i].base + si->cfsi[i].len - 1
                <= si->cfsi_maxaddr);

      if (i < si->cfsi_used - 1) {
         /*
         if (!(si->cfsi[i].base < si->cfsi[i+1].base)) {
            VG_(printf)("\nOOO cfsis:\n");
            ML_(ppCfiSI)(&si->cfsi[i]);
            ML_(ppCfiSI)(&si->cfsi[i+1]);
         }
         */
         /* In order. */
         vg_assert(si->cfsi[i].base < si->cfsi[i+1].base);
         /* No overlaps. */
         vg_assert(si->cfsi[i].base + si->cfsi[i].len - 1
                   < si->cfsi[i+1].base);
      }
   }

}


/* Canonicalise the tables held by 'si', in preparation for use.  Call
   this after finishing adding entries to these tables. */
void ML_(canonicaliseTables) ( struct _SegInfo* si )
{
   canonicaliseSymtab ( si );
   canonicaliseLoctab ( si );
   canonicaliseCFI ( si );
}


/*------------------------------------------------------------*/
/*--- Searching the tables                                 ---*/
/*------------------------------------------------------------*/

/* Find a symbol-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

Int ML_(search_one_symtab) ( struct _SegInfo* si, Addr ptr,
                             Bool match_anywhere_in_fun )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid, size, 
        lo = 0, 
        hi = si->symtab_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = si->symtab[mid].addr;
      size = ( match_anywhere_in_fun
             ? si->symtab[mid].size
             : 1);
      a_mid_hi = ((Addr)si->symtab[mid].addr) + size - 1;

      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      return mid;
   }
}


/* Find a location-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

Int ML_(search_one_loctab) ( struct _SegInfo* si, Addr ptr )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid, 
        lo = 0, 
        hi = si->loctab_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = si->loctab[mid].addr;
      a_mid_hi = ((Addr)si->loctab[mid].addr) + si->loctab[mid].size - 1;

      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      return mid;
   }
}


/* Find a CFI-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

Int ML_(search_one_cfitab) ( struct _SegInfo* si, Addr ptr )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid, size, 
        lo = 0, 
        hi = si->cfsi_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = si->cfsi[mid].base;
      size     = si->cfsi[mid].len;
      a_mid_hi = a_mid_lo + size - 1;
      vg_assert(a_mid_hi >= a_mid_lo);
      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      return mid;
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
