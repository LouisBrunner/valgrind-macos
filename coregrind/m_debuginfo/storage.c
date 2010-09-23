
/*--------------------------------------------------------------------*/
/*--- Format-neutral storage of and querying of info acquired from ---*/
/*--- ELF/XCOFF stabs/dwarf1/dwarf2/dwarf3 debug info.             ---*/
/*---                                                    storage.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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
#include "pub_core_debuginfo.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_xarray.h"
#include "pub_core_oset.h"

#include "priv_misc.h"         /* dinfo_zalloc/free/strdup */
#include "priv_d3basics.h"     /* ML_(pp_GX) */
#include "priv_tytypes.h"
#include "priv_storage.h"      /* self */


/*------------------------------------------------------------*/
/*--- Misc (printing, errors)                              ---*/
/*------------------------------------------------------------*/

/* Show a non-fatal debug info reading error.  Use vg_panic if
   terminal.  'serious' errors are shown regardless of the
   verbosity setting. */
void ML_(symerr) ( struct _DebugInfo* di, Bool serious, HChar* msg )
{
   /* XML mode hides everything :-( */
   if (VG_(clo_xml))
      return;

   if (serious) {

      VG_(message)(Vg_DebugMsg, "WARNING: Serious error when "
                                "reading debug info\n");
      if (True || VG_(clo_verbosity) < 2) {
         /* Need to show what the file name is, at verbosity levels 2
            or below, since that won't already have been shown */
         VG_(message)(Vg_DebugMsg, 
                      "When reading debug info from %s:\n",
                      (di && di->filename) ? di->filename : (UChar*)"???");
      }
      VG_(message)(Vg_DebugMsg, "%s\n", msg);

   } else { /* !serious */

      if (VG_(clo_verbosity) >= 2)
         VG_(message)(Vg_DebugMsg, "%s\n", msg);

   }
}


/* Print a symbol. */
void ML_(ppSym) ( Int idx, DiSym* sym )
{
   VG_(printf)( "%5d:  %#8lx .. %#8lx (%d)      %s\n",
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

   VG_(printf)("[%#lx .. %#lx]: ", si->base,
                               si->base + (UWord)si->len - 1);
   switch (si->cfa_how) {
      case CFIC_IA_SPREL: 
         VG_(printf)("let cfa=oldSP+%d", si->cfa_off); 
         break;
      case CFIC_IA_BPREL: 
         VG_(printf)("let cfa=oldBP+%d", si->cfa_off); 
         break;
      case CFIC_ARM_R13REL: 
         VG_(printf)("let cfa=oldR13+%d", si->cfa_off); 
         break;
      case CFIC_ARM_R12REL: 
         VG_(printf)("let cfa=oldR12+%d", si->cfa_off); 
         break;
      case CFIC_ARM_R11REL: 
         VG_(printf)("let cfa=oldR11+%d", si->cfa_off); 
         break;
      case CFIC_ARM_R7REL: 
         VG_(printf)("let cfa=oldR7+%d", si->cfa_off); 
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
#  if defined(VGA_x86) || defined(VGA_amd64)
   VG_(printf)(" SP=");
   SHOW_HOW(si->sp_how, si->sp_off);
   VG_(printf)(" BP=");
   SHOW_HOW(si->bp_how, si->bp_off);
#  elif defined(VGA_arm)
   VG_(printf)(" R14=");
   SHOW_HOW(si->r14_how, si->r14_off);
   VG_(printf)(" R13=");
   SHOW_HOW(si->r13_how, si->r13_off);
   VG_(printf)(" R12=");
   SHOW_HOW(si->r12_how, si->r12_off);
   VG_(printf)(" R11=");
   SHOW_HOW(si->r11_how, si->r11_off);
   VG_(printf)(" R7=");
   SHOW_HOW(si->r7_how, si->r7_off);
#  elif defined(VGA_ppc32) || defined(VGA_ppc64)
#  else
#    error "Unknown arch"
#  endif
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
UChar* ML_(addStr) ( struct _DebugInfo* di, UChar* str, Int len )
{
   struct strchunk *chunk;
   Int    space_needed;
   UChar* p;

   if (len == -1) {
      len = VG_(strlen)(str);
   } else {
      vg_assert(len >= 0);
   }

   space_needed = 1 + len;

   // Allocate a new strtab chunk if necessary
   if (di->strchunks == NULL || 
       (di->strchunks->strtab_used 
        + space_needed) > SEGINFO_STRCHUNKSIZE) {
      chunk = ML_(dinfo_zalloc)("di.storage.addStr.1", sizeof(*chunk));
      chunk->strtab_used = 0;
      chunk->next = di->strchunks;
      di->strchunks = chunk;
   }
   chunk = di->strchunks;

   p = &chunk->strtab[chunk->strtab_used];
   VG_(memcpy)(p, str, len);
   chunk->strtab[chunk->strtab_used+len] = '\0';
   chunk->strtab_used += space_needed;

   return p;
}


/* Add a symbol to the symbol table. 
*/
void ML_(addSym) ( struct _DebugInfo* di, DiSym* sym )
{
   UInt   new_sz, i;
   DiSym* new_tab;

   /* Ignore zero-sized syms. */
   if (sym->size == 0) return;

   if (di->symtab_used == di->symtab_size) {
      new_sz = 2 * di->symtab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = ML_(dinfo_zalloc)( "di.storage.addSym.1", 
                                   new_sz * sizeof(DiSym) );
      if (di->symtab != NULL) {
         for (i = 0; i < di->symtab_used; i++)
            new_tab[i] = di->symtab[i];
         ML_(dinfo_free)(di->symtab);
      }
      di->symtab = new_tab;
      di->symtab_size = new_sz;
   }

   di->symtab[di->symtab_used] = *sym;
   di->symtab_used++;
   vg_assert(di->symtab_used <= di->symtab_size);
}


/* Resize the symbol table to save memory.
*/
void ML_(shrinkSym)( struct _DebugInfo* di )
{
   DiSym* new_tab;
   UInt new_sz = di->symtab_used;
   if (new_sz == di->symtab_size) return;

   new_tab = ML_(dinfo_zalloc)( "di.storage.shrinkSym", 
                                new_sz * sizeof(DiSym) );
   VG_(memcpy)(new_tab, di->symtab, new_sz * sizeof(DiSym));

   ML_(dinfo_free)(di->symtab);
   di->symtab = new_tab;
   di->symtab_size = new_sz;
}


/* Add a location to the location table. 
*/
static void addLoc ( struct _DebugInfo* di, DiLoc* loc )
{
   UInt   new_sz, i;
   DiLoc* new_tab;

   /* Zero-sized locs should have been ignored earlier */
   vg_assert(loc->size > 0);

   if (di->loctab_used == di->loctab_size) {
      new_sz = 2 * di->loctab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = ML_(dinfo_zalloc)( "di.storage.addLoc.1",
                                   new_sz * sizeof(DiLoc) );
      if (di->loctab != NULL) {
         for (i = 0; i < di->loctab_used; i++)
            new_tab[i] = di->loctab[i];
         ML_(dinfo_free)(di->loctab);
      }
      di->loctab = new_tab;
      di->loctab_size = new_sz;
   }

   di->loctab[di->loctab_used] = *loc;
   di->loctab_used++;
   vg_assert(di->loctab_used <= di->loctab_size);
}


/* Resize the lineinfo table to save memory.
*/
void ML_(shrinkLineInfo)( struct _DebugInfo* di )
{
   DiLoc* new_tab;
   UInt new_sz = di->loctab_used;
   if (new_sz == di->loctab_size) return;

   new_tab = ML_(dinfo_zalloc)( "di.storage.shrinkLineInfo", 
                                new_sz * sizeof(DiLoc) );
   VG_(memcpy)(new_tab, di->loctab, new_sz * sizeof(DiLoc));

   ML_(dinfo_free)(di->loctab);
   di->loctab = new_tab;
   di->loctab_size = new_sz;
}


/* Top-level place to call to add a source-location mapping entry.
*/
void ML_(addLineInfo) ( struct _DebugInfo* di,
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
      VG_(printf)( "  src %s %s line %d %#lx-%#lx\n",
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
                        "at entry %d: 0x%lx 0x%lx\n", entry, this, next);
       }
       size = 1;
   }

   if (size > MAX_LOC_SIZE) {
       if (0)
       VG_(message)(Vg_DebugMsg, 
                    "warning: line info address range too large "
                    "at entry %d: %d\n", entry, size);
       size = 1;
   }

   /* Rule out ones which are completely outside the r-x mapped area.
      See "Comment_Regarding_Text_Range_Checks" elsewhere in this file
      for background and rationale. */
   vg_assert(di->have_rx_map && di->have_rw_map);
   if (next-1 < di->rx_map_avma
       || this >= di->rx_map_avma + di->rx_map_size ) {
       if (0)
          VG_(message)(Vg_DebugMsg, 
                       "warning: ignoring line info entry falling "
                       "outside current DebugInfo: %#lx %#lx %#lx %#lx\n",
                       di->text_avma, 
                       di->text_avma + di->text_size, 
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
                      "huge line number (%d)\n", lineno);
         VG_(message)(Vg_UserMsg, 
                      "         Can't handle line numbers "
                      "greater than %d, sorry\n", MAX_LINENO);
         VG_(message)(Vg_UserMsg, 
                      "(Nb: this message is only shown once)\n");
      }
      return;
   }

   loc.addr      = this;
   loc.size      = (UShort)size;
   loc.lineno    = lineno;
   loc.filename  = filename;
   loc.dirname   = dirname;

   if (0) VG_(message)(Vg_DebugMsg, 
		       "addLoc: addr %#lx, size %d, line %d, file %s\n",
		       this,size,lineno,filename);

   addLoc ( di, &loc );
}


/* Top-level place to call to add a CFI summary record.  The supplied
   DiCfSI is copied. */
void ML_(addDiCfSI) ( struct _DebugInfo* di, DiCfSI* cfsi_orig )
{
   static const Bool debug = False;
   UInt    new_sz, i;
   DiCfSI* new_tab;
   SSizeT  delta;

   /* copy the original, so we can mess with it */
   DiCfSI cfsi = *cfsi_orig;

   if (debug) {
      VG_(printf)("adding DiCfSI: ");
      ML_(ppDiCfSI)(di->cfsi_exprs, &cfsi);
   }

   /* sanity */
   vg_assert(cfsi.len > 0);
   /* If this fails, the implication is you have a single procedure
      with more than 5 million bytes of code.  Which is pretty
      unlikely.  Either that, or the debuginfo reader is somehow
      broken.  5 million is of course arbitrary; but it's big enough
      to be bigger than the size of any plausible piece of code that
      would fall within a single procedure. */
   vg_assert(cfsi.len < 5000000);

   vg_assert(di->have_rx_map && di->have_rw_map);
   /* If we have an empty r-x mapping (is that possible?) then the
      DiCfSI can't possibly fall inside it.  In which case skip. */
   if (di->rx_map_size == 0)
      return;

   /* Rule out ones which are completely outside the r-x mapped area.
      See "Comment_Regarding_Text_Range_Checks" elsewhere in this file
      for background and rationale. */
   if (cfsi.base + cfsi.len - 1 < di->rx_map_avma
       || cfsi.base >= di->rx_map_avma + di->rx_map_size) {
      static Int complaints = 10;
      if (VG_(clo_trace_cfi) || complaints > 0) {
         complaints--;
         if (VG_(clo_verbosity) > 1) {
            VG_(message)(
               Vg_DebugMsg,
               "warning: DiCfSI %#lx .. %#lx outside segment %#lx .. %#lx\n",
               cfsi.base, 
               cfsi.base + cfsi.len - 1,
               di->text_avma,
               di->text_avma + di->text_size - 1 
            );
         }
         if (VG_(clo_trace_cfi)) 
            ML_(ppDiCfSI)(di->cfsi_exprs, &cfsi);
      }
      return;
   }

   /* Now we know the range is at least partially inside the r-x
      mapped area.  That implies that at least one of the ends of the
      range falls inside the area.  If necessary, clip it so it is
      completely within the area.  If we don't do this,
      check_CFSI_related_invariants() in debuginfo.c (invariant #2)
      will fail.  See
      "Comment_on_IMPORTANT_CFSI_REPRESENTATIONAL_INVARIANTS" in
      priv_storage.h for background. */
   if (cfsi.base < di->rx_map_avma) {
      /* Lower end is outside the mapped area.  Hence upper end must
         be inside it. */
      if (0) VG_(printf)("XXX truncate lower\n");
      vg_assert(cfsi.base + cfsi.len - 1 >= di->rx_map_avma);
      delta = (SSizeT)(di->rx_map_avma - cfsi.base);
      vg_assert(delta > 0);
      vg_assert(delta < (SSizeT)cfsi.len);
      cfsi.base += delta;
      cfsi.len -= delta;
   }
   else
   if (cfsi.base + cfsi.len - 1 > di->rx_map_avma + di->rx_map_size - 1) {
      /* Upper end is outside the mapped area.  Hence lower end must be
         inside it. */
      if (0) VG_(printf)("XXX truncate upper\n");
      vg_assert(cfsi.base <= di->rx_map_avma + di->rx_map_size - 1);
      delta = (SSizeT)( (cfsi.base + cfsi.len - 1) 
                        - (di->rx_map_avma + di->rx_map_size - 1) );
      vg_assert(delta > 0); vg_assert(delta < (SSizeT)cfsi.len);
      cfsi.len -= delta;
   }

   /* Final checks */

   /* Because: either cfsi was entirely inside the range, in which
      case we asserted that len > 0 at the start, OR it fell partially
      inside the range, in which case we reduced it by some size
      (delta) which is < its original size. */
   vg_assert(cfsi.len > 0);

   /* Similar logic applies for the next two assertions. */
   vg_assert(cfsi.base >= di->rx_map_avma);
   vg_assert(cfsi.base + cfsi.len - 1
             <= di->rx_map_avma + di->rx_map_size - 1);

   if (di->cfsi_used == di->cfsi_size) {
      new_sz = 2 * di->cfsi_size;
      if (new_sz == 0) new_sz = 20;
      new_tab = ML_(dinfo_zalloc)( "di.storage.addDiCfSI.1",
                                   new_sz * sizeof(DiCfSI) );
      if (di->cfsi != NULL) {
         for (i = 0; i < di->cfsi_used; i++)
            new_tab[i] = di->cfsi[i];
         ML_(dinfo_free)(di->cfsi);
      }
      di->cfsi = new_tab;
      di->cfsi_size = new_sz;
   }

   di->cfsi[di->cfsi_used] = cfsi;
   di->cfsi_used++;
   vg_assert(di->cfsi_used <= di->cfsi_size);
}


Int ML_(CfiExpr_Undef)( XArray* dst )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_Undef;
   return (Int)VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_Deref)( XArray* dst, Int ixAddr )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_Deref;
   e.Cex.Deref.ixAddr = ixAddr;
   return (Int)VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_Const)( XArray* dst, UWord con )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_Const;
   e.Cex.Const.con = con;
   return (Int)VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_Binop)( XArray* dst, CfiOp op, Int ixL, Int ixR )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_Binop;
   e.Cex.Binop.op  = op;
   e.Cex.Binop.ixL = ixL;
   e.Cex.Binop.ixR = ixR;
   return (Int)VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_CfiReg)( XArray* dst, CfiReg reg )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_CfiReg;
   e.Cex.CfiReg.reg = reg;
   return (Int)VG_(addToXA)( dst, &e );
}
Int ML_(CfiExpr_DwReg)( XArray* dst, Int reg )
{
   CfiExpr e;
   VG_(memset)( &e, 0, sizeof(e) );
   e.tag = Cex_DwReg;
   e.Cex.DwReg.reg = reg;
   return (Int)VG_(addToXA)( dst, &e );
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
      case Creg_IA_SP:   VG_(printf)("xSP"); break;
      case Creg_IA_BP:   VG_(printf)("xBP"); break;
      case Creg_IA_IP:   VG_(printf)("xIP"); break;
      case Creg_ARM_R13: VG_(printf)("R13"); break;
      case Creg_ARM_R12: VG_(printf)("R12"); break;
      case Creg_ARM_R15: VG_(printf)("R15"); break;
      case Creg_ARM_R14: VG_(printf)("R14"); break;
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


Word ML_(cmp_for_DiAddrRange_range) ( const void* keyV,
                                      const void* elemV ) {
   const Addr* key = (const Addr*)keyV;
   const DiAddrRange* elem = (const DiAddrRange*)elemV;
   if (0)
      VG_(printf)("cmp_for_DiAddrRange_range: %#lx vs %#lx\n",
                  *key, elem->aMin);
   if ((*key) < elem->aMin) return -1;
   if ((*key) > elem->aMax) return 1;
   return 0;
}

static
void show_scope ( OSet* /* of DiAddrRange */ scope, HChar* who )
{
   DiAddrRange* range;
   VG_(printf)("Scope \"%s\" = {\n", who);
   VG_(OSetGen_ResetIter)( scope );
   while (True) {
      range = VG_(OSetGen_Next)( scope );
      if (!range) break;
      VG_(printf)("   %#lx .. %#lx: %lu vars\n", range->aMin, range->aMax,
                  range->vars ? VG_(sizeXA)(range->vars) : 0);
   }
   VG_(printf)("}\n");
}

/* Add the variable 'var' to 'scope' for the address range [aMin,aMax]
   (inclusive of aMin and aMax).  Split existing ranges as required if
   aMin or aMax or both don't match existing range boundaries, and add
   'var' to all required ranges.  Take great care to preserve the
   invariant that the ranges in 'scope' cover the entire address range
   exactly once, with no overlaps and no holes. */
static void add_var_to_arange ( 
               /*MOD*/OSet* /* of DiAddrRange */ scope,
               Addr aMin, 
               Addr aMax,
               DiVariable* var
            )
{
   DiAddrRange *first, *last, *range;
   /* These xx variables are for assertion checking only; they don't
      contribute anything to the actual work of this function. */
   DiAddrRange *xxRangep, *xxFirst, *xxLast;
   UWord       xxIters;

   vg_assert(aMin <= aMax);

   if (0) VG_(printf)("add_var_to_arange: %#lx .. %#lx\n", aMin, aMax);
   if (0) show_scope( scope, "add_var_to_arange(1)" );

   /* See if the lower end of the range (aMin) falls exactly on an
      existing range boundary.  If not, find the range it does fall
      into, and split it (copying the variables in the process), so
      that aMin does exactly fall on a range boundary. */
   first = VG_(OSetGen_Lookup)( scope, &aMin );
   /* It must be present, since the presented OSet must cover
      the entire address range. */
   vg_assert(first);
   vg_assert(first->aMin <= first->aMax);
   vg_assert(first->aMin <= aMin && aMin <= first->aMax);

   /* Fast track common case, which is that the range specified for
      the variable exactly coincides with one already-existing
      range. */
   if (first->aMin == aMin && first->aMax == aMax) {
      vg_assert(first->vars);
      VG_(addToXA)( first->vars, var );
      return;
   }

   /* We have to get into splitting ranges, which is complex
      and slow. */
   if (first->aMin < aMin) {
      DiAddrRange* nyu;
      /* Ok.  We'll have to split 'first'. */
      /* truncate the upper end of 'first' */
      Addr tmp = first->aMax;
      first->aMax = aMin-1;
      vg_assert(first->aMin <= first->aMax);
      /* create a new range */
      nyu = VG_(OSetGen_AllocNode)( scope, sizeof(DiAddrRange) );
      vg_assert(nyu);
      nyu->aMin = aMin;
      nyu->aMax = tmp;
      vg_assert(nyu->aMin <= nyu->aMax);
      /* copy vars into it */
      vg_assert(first->vars);
      nyu->vars = VG_(cloneXA)( "di.storage.avta.1", first->vars );
      vg_assert(nyu->vars);
      VG_(OSetGen_Insert)( scope, nyu );
      first = nyu;
   }

   vg_assert(first->aMin == aMin);

   /* Now do exactly the same for the upper end (aMax): if it doesn't
      fall on a boundary, cause it to do so by splitting the range it
      does currently fall into. */
   last = VG_(OSetGen_Lookup)( scope, &aMax );
   vg_assert(last->aMin <= last->aMax);
   vg_assert(last->aMin <= aMax && aMax <= last->aMax);

   if (aMax < last->aMax) {
      DiAddrRange* nyu;
      /* We have to split 'last'. */
      /* truncate the lower end of 'last' */
      Addr tmp = last->aMin;
      last->aMin = aMax+1;
      vg_assert(last->aMin <= last->aMax);
      /* create a new range */
      nyu = VG_(OSetGen_AllocNode)( scope, sizeof(DiAddrRange) );
      vg_assert(nyu);
      nyu->aMin = tmp;
      nyu->aMax = aMax;
      vg_assert(nyu->aMin <= nyu->aMax);
      /* copy vars into it */
      vg_assert(last->vars);
      nyu->vars = VG_(cloneXA)( "di.storage.avta.2", last->vars );
      vg_assert(nyu->vars);
      VG_(OSetGen_Insert)( scope, nyu );
      last = nyu;
   }

   vg_assert(aMax == last->aMax);

   xxFirst = (DiAddrRange*)VG_(OSetGen_Lookup)(scope, &aMin);
   xxLast  = (DiAddrRange*)VG_(OSetGen_Lookup)(scope, &aMax);
   vg_assert(xxFirst);
   vg_assert(xxLast);
   vg_assert(xxFirst->aMin == aMin);
   vg_assert(xxLast->aMax == aMax);
   if (xxFirst != xxLast)
      vg_assert(xxFirst->aMax < xxLast->aMin);

   /* Great.  Now we merely need to iterate over the segments from
      'first' to 'last' inclusive, and add 'var' to the variable set
      of each of them. */
   if (0) {
      static UWord ctr = 0;
      ctr++;
      VG_(printf)("ctr = %lu\n", ctr);
      if (ctr >= 33263) show_scope( scope, "add_var_to_arange(2)" );
   }

   xxIters = 0;
   range = xxRangep = NULL;
   VG_(OSetGen_ResetIterAt)( scope, &aMin );
   while (True) {
      xxRangep = range;
      range    = VG_(OSetGen_Next)( scope );
      if (!range) break;
      if (range->aMin > aMax) break;
      xxIters++;
      if (0) VG_(printf)("have range %#lx %#lx\n",
                         range->aMin, range->aMax);

      /* Sanity checks */
      if (!xxRangep) {
         /* This is the first in the range */
         vg_assert(range->aMin == aMin);
      } else {
         vg_assert(xxRangep->aMax + 1 == range->aMin);
      }

      vg_assert(range->vars);
      VG_(addToXA)( range->vars, var );
   }
   /* Done.  We should have seen at least one range. */
   vg_assert(xxIters >= 1);
   if (xxIters == 1) vg_assert(xxFirst == xxLast);
   if (xxFirst == xxLast) vg_assert(xxIters == 1);
   vg_assert(xxRangep);
   vg_assert(xxRangep->aMax == aMax);
   vg_assert(xxRangep == xxLast);
}


/* Top-level place to call to add a variable description (as extracted
   from a DWARF3 .debug_info section. */
void ML_(addVar)( struct _DebugInfo* di,
                  Int    level,
                  Addr   aMin,
                  Addr   aMax,
                  UChar* name, /* in di's .strchunks */
                  UWord  typeR, /* a cuOff */
                  GExpr* gexpr,
                  GExpr* fbGX,
                  UChar* fileName, /* where decl'd - may be NULL.
                                      in di's .strchunks */
                  Int    lineNo, /* where decl'd - may be zero */
                  Bool   show )
{
   OSet* /* of DiAddrRange */ scope;
   DiVariable var;
   Bool       all;
   TyEnt*     ent;
   MaybeULong mul;
   HChar*     badness;

   tl_assert(di && di->admin_tyents);

   if (0) {
      VG_(printf)("  ML_(addVar): level %d  %#lx-%#lx  %s :: ",
                  level, aMin, aMax, name );
      ML_(pp_TyEnt_C_ishly)( di->admin_tyents, typeR );
      VG_(printf)("\n  Var=");
      ML_(pp_GX)(gexpr);
      VG_(printf)("\n");
      if (fbGX) {
         VG_(printf)("  FrB=");
         ML_(pp_GX)( fbGX );
         VG_(printf)("\n");
      } else {
         VG_(printf)("  FrB=none\n");
      }
      VG_(printf)("\n");
   }

   vg_assert(level >= 0);
   vg_assert(aMin <= aMax);
   vg_assert(name);
   vg_assert(gexpr);

   ent = ML_(TyEnts__index_by_cuOff)( di->admin_tyents, NULL, typeR);
   tl_assert(ent);
   vg_assert(ML_(TyEnt__is_type)(ent));

   /* "Comment_Regarding_Text_Range_Checks" (is referred to elsewhere)
      ----------------------------------------------------------------
      Ignore any variables whose aMin .. aMax (that is, range of text
      addresses for which they actually exist) falls outside the text
      segment.  Is this indicative of a bug in the reader?  Maybe.
      (LATER): instead of restricting strictly to the .text segment,
      be a bit more relaxed, and accept any variable whose text range
      falls inside the r-x mapped area.  This is useful because .text
      is not always the only instruction-carrying segment: others are:
      .init .plt __libc_freeres_fn and .fini.  This implicitly assumes
      that those extra sections have the same bias as .text, but that
      seems a reasonable assumption to me. */
   /* This is assured us by top level steering logic in debuginfo.c,
      and it is re-checked at the start of
      ML_(read_elf_debug_info). */
   vg_assert(di->have_rx_map && di->have_rw_map);
   if (level > 0
       && (aMax < di->rx_map_avma
           || aMin >= di->rx_map_avma + di->rx_map_size)) {
      if (VG_(clo_verbosity) >= 0) {
         VG_(message)(Vg_DebugMsg, 
            "warning: addVar: in range %#lx .. %#lx outside "
            "segment %#lx .. %#lx (%s)\n",
            aMin, aMax,
            di->text_avma, di->text_avma + di->text_size -1,
            name
         );
      }
      return;
   }

   /* If the type's size is zero (which can mean unknown size), ignore
      it.  We will never be able to actually relate a data address to
      a data object with zero size, so there's no point in storing
      info on it.  On 32-bit platforms, also reject types whose size
      is 2^32 bytes or large.  (It's amazing what junk shows up ..) */
   mul = ML_(sizeOfType)(di->admin_tyents, typeR);

   badness = NULL;
   if (mul.b != True) 
      badness = "unknown size";
   else if (mul.ul == 0)
      badness = "zero size   ";
   else if (sizeof(void*) == 4 && mul.ul >= (1ULL<<32))
      badness = "implausibly large";

   if (badness) {
      static Int complaints = 10;
      if (VG_(clo_verbosity) >= 2 && complaints > 0) {
         VG_(message)(Vg_DebugMsg, "warning: addVar: %s (%s)\n",
                                   badness, name );
         complaints--;
      }
      return;
   }

   if (!di->varinfo) {
      di->varinfo = VG_(newXA)( ML_(dinfo_zalloc), 
                                "di.storage.addVar.1",
                                ML_(dinfo_free),
                                sizeof(OSet*) );
   }

   vg_assert(level < 256); /* arbitrary; stay sane */
   /* Expand the top level array enough to map this level */
   while ( VG_(sizeXA)(di->varinfo) <= level ) {
      DiAddrRange* nyu;
      scope = VG_(OSetGen_Create)( offsetof(DiAddrRange,aMin), 
                                   ML_(cmp_for_DiAddrRange_range),
                                   ML_(dinfo_zalloc), "di.storage.addVar.2",
                                   ML_(dinfo_free) );
      vg_assert(scope);
      if (0) VG_(printf)("create: scope = %p, adding at %ld\n",
                         scope, VG_(sizeXA)(di->varinfo));
      VG_(addToXA)( di->varinfo, &scope );
      /* Add a single range covering the entire address space.  At
         level 0 we require this doesn't get split.  At levels above 0
         we require that any additions to it cause it to get split.
         All of these invariants get checked both add_var_to_arange
         and after reading is complete, in canonicaliseVarInfo. */
      nyu = VG_(OSetGen_AllocNode)( scope, sizeof(DiAddrRange) );
      vg_assert(nyu);
      nyu->aMin = (Addr)0;
      nyu->aMax = ~(Addr)0;
      nyu->vars = VG_(newXA)( ML_(dinfo_zalloc), "di.storage.addVar.3",
                              ML_(dinfo_free),
                              sizeof(DiVariable) );
      vg_assert(nyu->vars);
      VG_(OSetGen_Insert)( scope, nyu );
   }

   vg_assert( VG_(sizeXA)(di->varinfo) > level );
   scope = *(OSet**)VG_(indexXA)( di->varinfo, level );
   vg_assert(scope);

   var.name     = name;
   var.typeR    = typeR;
   var.gexpr    = gexpr;
   var.fbGX     = fbGX;
   var.fileName = fileName;
   var.lineNo   = lineNo;

   all = aMin == (Addr)0 && aMax == ~(Addr)0;
   vg_assert(level == 0 ? all : !all);

   add_var_to_arange( /*MOD*/scope, aMin, aMax, &var );
}


/* This really just checks the constructed data structure, as there is
   no canonicalisation to do. */
static void canonicaliseVarInfo ( struct _DebugInfo* di )
{
   Word i, nInThisScope;

   if (!di->varinfo)
      return;

   for (i = 0; i < VG_(sizeXA)(di->varinfo); i++) {

      DiAddrRange *range, *rangep;
      OSet* scope = *(OSet**)VG_(indexXA)(di->varinfo, i);
      if (!scope) continue;

      /* Deal with the global-scope case. */
      if (i == 0) {
         Addr zero = 0;
         vg_assert(VG_(OSetGen_Size)( scope ) == 1);
         range = VG_(OSetGen_Lookup)( scope, &zero );
         vg_assert(range);
         vg_assert(range->aMin == (Addr)0);
         vg_assert(range->aMax == ~(Addr)0);
         continue;
      }

      /* All the rest of this is for the local-scope case. */
      /* iterate over all entries in 'scope' */
      nInThisScope = 0;
      rangep = NULL;
      VG_(OSetGen_ResetIter)(scope);
      while (True) {
         range = VG_(OSetGen_Next)(scope);
         if (!range) {
           /* We just saw the last one.  There must have been at
              least one entry in the range. */
           vg_assert(rangep);
           vg_assert(rangep->aMax == ~(Addr)0);
           break;
         }

         vg_assert(range->aMin <= range->aMax);
         vg_assert(range->vars);

         if (!rangep) {
           /* This is the first entry in the range. */
           vg_assert(range->aMin == 0);
         } else {
           vg_assert(rangep->aMax + 1 == range->aMin);
         }

         rangep = range;
         nInThisScope++;
      } /* iterating over ranges in a given scope */

      /* If there's only one entry in this (local) scope, it must
         cover the entire address space (obviously), but it must not
         contain any vars. */

      vg_assert(nInThisScope > 0);
      if (nInThisScope == 1) {
         Addr zero = 0;
         vg_assert(VG_(OSetGen_Size)( scope ) == 1);
         range = VG_(OSetGen_Lookup)( scope, &zero );
         vg_assert(range);
         vg_assert(range->aMin == (Addr)0);
         vg_assert(range->aMax == ~(Addr)0);
         vg_assert(range->vars);
         vg_assert(VG_(sizeXA)(range->vars) == 0);
      }

   } /* iterate over scopes */
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


/* Two symbols have the same address.  Which name do we prefer?  In order:

   - Prefer "PMPI_<foo>" over "MPI_<foo>".

   - Else, prefer a non-NULL name over a NULL one.

   - Else, prefer a non-whitespace name over an all-whitespace name.

   - Else, prefer the shorter symbol name.  If the symbol contains a
     version symbol ('@' on Linux, other platforms may differ), which means it
     is versioned, then the length up to the version symbol is used for length
     comparison purposes (so "foo@GLIBC_2.4.2" is considered shorter than
     "foobar"). 
     
   - Else, if two symbols have the same length, prefer a versioned symbol over
     a non-versioned symbol.
     
   - Else, use alphabetical ordering.

   - Otherwise, they must be the same;  use the symbol with the lower address.

   Very occasionally this goes wrong (eg. 'memcmp' and 'bcmp' are
   aliases in glibc, we choose the 'bcmp' symbol because it's shorter,
   so we can misdescribe memcmp() as bcmp()).  This is hard to avoid.
   It's mentioned in the FAQ file.
 */
static DiSym* prefersym ( struct _DebugInfo* di, DiSym* a, DiSym* b )
{
   Word cmp;
   Word vlena, vlenb;		/* length without version */
   const UChar *vpa, *vpb;

   Bool preferA = False;
   Bool preferB = False;

   vg_assert(a->addr == b->addr);

   vlena = VG_(strlen)(a->name);
   vlenb = VG_(strlen)(b->name);

#if defined(VGO_linux) || defined(VGO_aix5)
#  define VERSION_CHAR '@'
#elif defined(VGO_darwin)
#  define VERSION_CHAR '$'
#else
#  error Unknown OS
#endif

   vpa = VG_(strchr)(a->name, VERSION_CHAR);
   vpb = VG_(strchr)(b->name, VERSION_CHAR);

   if (vpa)
      vlena = vpa - a->name;
   if (vpb)
      vlenb = vpb - b->name;

   /* MPI hack: prefer PMPI_Foo over MPI_Foo */
   if (0==VG_(strncmp)(a->name, "MPI_", 4)
       && 0==VG_(strncmp)(b->name, "PMPI_", 5)
       && 0==VG_(strcmp)(a->name, 1+b->name)) {
      preferB = True; goto out;
   } 
   if (0==VG_(strncmp)(b->name, "MPI_", 4)
       && 0==VG_(strncmp)(a->name, "PMPI_", 5)
       && 0==VG_(strcmp)(b->name, 1+a->name)) {
      preferA = True; goto out;
   }

   /* Prefer non-empty name. */
   if (vlena  &&  !vlenb) {
      preferA = True; goto out;
   }
   if (vlenb  &&  !vlena) {
      preferB = True; goto out;
   }

   /* Prefer non-whitespace name. */
   {
      Bool blankA = True;
      Bool blankB = True;
      Char *s;
      s = a->name;
      while (*s) {
         if (!VG_(isspace)(*s++)) {
            blankA = False;
            break;
         }
      }
      s = b->name;
      while (*s) {
         if (!VG_(isspace)(*s++)) {
            blankB = False;
            break;
         }
      }

      if (!blankA  &&  blankB) {
         preferA = True; goto out;
      }
      if (!blankB  &&  blankA) {
         preferB = True; goto out;
      }
   }

   /* Select the shortest unversioned name */
   if (vlena < vlenb) {
      preferA = True; goto out;
   } 
   if (vlenb < vlena) {
      preferB = True; goto out;
   }

   /* Equal lengths; select the versioned name */
   if (vpa && !vpb) {
      preferA = True; goto out;
   }
   if (vpb && !vpa) {
      preferB = True; goto out;
   }

   /* Either both versioned or neither is versioned; select them
      alphabetically */
   cmp = VG_(strcmp)(a->name, b->name);
   if (cmp < 0) {
      preferA = True; goto out;
   }
   if (cmp > 0) {
      preferB = True; goto out;
   }

   /* If we get here, they are the same name. */

   /* In this case we could choose either (arbitrarily), but might as
      well choose the one with the lowest DiSym* address, so as to try
      and make the comparison mechanism more stable (a la sorting
      parlance).  Also, skip the diagnostic printing in this case. */
   return a <= b  ? a  : b;

   /*NOTREACHED*/
   vg_assert(0);
  out:
   if (preferA && !preferB) {
      TRACE_SYMTAB("sym at %#lx: prefer '%s' to '%s'\n",
                   a->addr, a->name, b->name );
      return a;
   }
   if (preferB && !preferA) {
      TRACE_SYMTAB("sym at %#lx: prefer '%s' to '%s'\n",
                   b->addr, b->name, a->name );
      return b;
   }
   /*NOTREACHED*/
   vg_assert(0);
}

static void canonicaliseSymtab ( struct _DebugInfo* di )
{
   Word  i, j, n_merged, n_truncated;
   Addr  s1, s2, e1, e2, p1, p2;
   UChar *n1, *n2;
   Bool t1, t2, f1, f2;

#  define SWAP(ty,aa,bb) \
      do { ty tt = (aa); (aa) = (bb); (bb) = tt; } while (0)

   if (di->symtab_used == 0)
      return;

   VG_(ssort)(di->symtab, di->symtab_used, 
                          sizeof(*di->symtab), compare_DiSym);

  cleanup_more:
 
   /* If two symbols have identical address ranges, we pick one
      using prefersym() (see it for details). */
   do {
      n_merged = 0;
      j = di->symtab_used;
      di->symtab_used = 0;
      for (i = 0; i < j; i++) {
         if (i < j-1
             && di->symtab[i].addr   == di->symtab[i+1].addr
             && di->symtab[i].size   == di->symtab[i+1].size
             ) {
            n_merged++;
            /* merge the two into one */
	    di->symtab[di->symtab_used++] 
               = *prefersym(di, &di->symtab[i], &di->symtab[i+1]);
            i++;
         } else {
            di->symtab[di->symtab_used++] = di->symtab[i];
         }
      }
      TRACE_SYMTAB( "canonicaliseSymtab: %ld symbols merged\n", n_merged);
   }
   while (n_merged > 0);

   /* Detect and "fix" overlapping address ranges. */
   n_truncated = 0;

   for (i = 0; i < ((Word)di->symtab_used) -1; i++) {

      vg_assert(di->symtab[i].addr <= di->symtab[i+1].addr);

      /* Check for common (no overlap) case. */ 
      if (di->symtab[i].addr + di->symtab[i].size 
          <= di->symtab[i+1].addr)
         continue;

      /* There's an overlap.  Truncate one or the other. */
      if (di->trace_symtab) {
         VG_(printf)("overlapping address ranges in symbol table\n\t");
         ML_(ppSym)( i, &di->symtab[i] );
         VG_(printf)("\t");
         ML_(ppSym)( i+1, &di->symtab[i+1] );
         VG_(printf)("\n");
      }

      /* Truncate one or the other. */
      s1 = di->symtab[i].addr;
      e1 = s1 + di->symtab[i].size - 1;
      p1 = di->symtab[i].tocptr;
      n1 = di->symtab[i].name;
      t1 = di->symtab[i].isText;
      f1 = di->symtab[i].isIFunc;
      s2 = di->symtab[i+1].addr;
      e2 = s2 + di->symtab[i+1].size - 1;
      p2 = di->symtab[i+1].tocptr;
      n2 = di->symtab[i+1].name;
      t2 = di->symtab[i+1].isText;
      f2 = di->symtab[i+1].isIFunc;
      if (s1 < s2) {
         e1 = s2-1;
      } else {
         vg_assert(s1 == s2);
         if (e1 > e2) { 
            s1 = e2+1; SWAP(Addr,s1,s2); SWAP(Addr,e1,e2); SWAP(Addr,p1,p2);
                       SWAP(UChar *,n1,n2); SWAP(Bool,t1,t2);
         } else 
         if (e1 < e2) {
            s2 = e1+1;
         } else {
	   /* e1 == e2.  Identical addr ranges.  We'll eventually wind
              up back at cleanup_more, which will take care of it. */
	 }
      }
      di->symtab[i].addr    = s1;
      di->symtab[i].size    = e1 - s1 + 1;
      di->symtab[i].tocptr  = p1;
      di->symtab[i].name    = n1;
      di->symtab[i].isText  = t1;
      di->symtab[i].isIFunc = f1;
      di->symtab[i+1].addr    = s2;
      di->symtab[i+1].size    = e2 - s2 + 1;
      di->symtab[i+1].tocptr  = p2;
      di->symtab[i+1].name    = n2;
      di->symtab[i+1].isText  = t2;
      di->symtab[i+1].isIFunc = f2;
      vg_assert(s1 <= s2);
      vg_assert(di->symtab[i].size > 0);
      vg_assert(di->symtab[i+1].size > 0);
      /* It may be that the i+1 entry now needs to be moved further
         along to maintain the address order requirement. */
      j = i+1;
      while (j < ((Word)di->symtab_used)-1 
             && di->symtab[j].addr > di->symtab[j+1].addr) {
         SWAP(DiSym,di->symtab[j],di->symtab[j+1]);
         j++;
      }
      n_truncated++;
   }

   if (n_truncated > 0) goto cleanup_more;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < ((Word)di->symtab_used)-1; i++) {
      /* No zero-sized symbols. */
      vg_assert(di->symtab[i].size > 0);
      /* In order. */
      vg_assert(di->symtab[i].addr < di->symtab[i+1].addr);
      /* No overlaps. */
      vg_assert(di->symtab[i].addr + di->symtab[i].size - 1
                < di->symtab[i+1].addr);
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

static void canonicaliseLoctab ( struct _DebugInfo* di )
{
   Word i, j;

#  define SWAP(ty,aa,bb) \
      do { ty tt = (aa); (aa) = (bb); (bb) = tt; } while (0);

   if (di->loctab_used == 0)
      return;

   /* Sort by start address. */
   VG_(ssort)(di->loctab, di->loctab_used, 
                          sizeof(*di->loctab), compare_DiLoc);

   /* If two adjacent entries overlap, truncate the first. */
   for (i = 0; i < ((Word)di->loctab_used)-1; i++) {
      vg_assert(di->loctab[i].size < 10000);
      if (di->loctab[i].addr + di->loctab[i].size > di->loctab[i+1].addr) {
         /* Do this in signed int32 because the actual .size fields
            are only 12 bits. */
         Int new_size = di->loctab[i+1].addr - di->loctab[i].addr;
         if (new_size < 0) {
            di->loctab[i].size = 0;
         } else
         if (new_size > MAX_LOC_SIZE) {
           di->loctab[i].size = MAX_LOC_SIZE;
         } else {
           di->loctab[i].size = (UShort)new_size;
         }
      }
   }

   /* Zap any zero-sized entries resulting from the truncation
      process. */
   j = 0;
   for (i = 0; i < (Word)di->loctab_used; i++) {
      if (di->loctab[i].size > 0) {
         if (j != i)
            di->loctab[j] = di->loctab[i];
         j++;
      }
   }
   di->loctab_used = j;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < ((Word)di->loctab_used)-1; i++) {
      /* 
      VG_(printf)("%d   (%d) %d 0x%x\n", 
                   i, di->loctab[i+1].confident, 
                   di->loctab[i+1].size, di->loctab[i+1].addr );
      */
      /* No zero-sized symbols. */
      vg_assert(di->loctab[i].size > 0);
      /* In order. */
      vg_assert(di->loctab[i].addr < di->loctab[i+1].addr);
      /* No overlaps. */
      vg_assert(di->loctab[i].addr + di->loctab[i].size - 1
                < di->loctab[i+1].addr);
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

void ML_(canonicaliseCFI) ( struct _DebugInfo* di )
{
   Word  i, j;
   const Addr minAvma = 0;
   const Addr maxAvma = ~minAvma;

   /* Note: take care in here.  di->cfsi can be NULL, in which
      case _used and _size fields will be zero. */
   if (di->cfsi == NULL) {
      vg_assert(di->cfsi_used == 0);
      vg_assert(di->cfsi_size == 0);
   }

   /* Set cfsi_minavma and cfsi_maxavma to summarise the entire
      address range contained in cfsi[0 .. cfsi_used-1]. */
   di->cfsi_minavma = maxAvma; 
   di->cfsi_maxavma = minAvma;
   for (i = 0; i < (Word)di->cfsi_used; i++) {
      Addr here_min = di->cfsi[i].base;
      Addr here_max = di->cfsi[i].base + di->cfsi[i].len - 1;
      if (here_min < di->cfsi_minavma)
         di->cfsi_minavma = here_min;
      if (here_max > di->cfsi_maxavma)
         di->cfsi_maxavma = here_max;
   }

   if (di->trace_cfi)
      VG_(printf)("canonicaliseCfiSI: %ld entries, %#lx .. %#lx\n",
                  di->cfsi_used,
	          di->cfsi_minavma, di->cfsi_maxavma);

   /* Sort the cfsi array by base address. */
   VG_(ssort)(di->cfsi, di->cfsi_used, sizeof(*di->cfsi), compare_DiCfSI);

   /* If two adjacent entries overlap, truncate the first. */
   for (i = 0; i < (Word)di->cfsi_used-1; i++) {
      if (di->cfsi[i].base + di->cfsi[i].len > di->cfsi[i+1].base) {
         Word new_len = di->cfsi[i+1].base - di->cfsi[i].base;
         /* how could it be otherwise?  The entries are sorted by the
            .base field. */         
         vg_assert(new_len >= 0);
	 vg_assert(new_len <= di->cfsi[i].len);
         di->cfsi[i].len = new_len;
      }
   }

   /* Zap any zero-sized entries resulting from the truncation
      process. */
   j = 0;
   for (i = 0; i < (Word)di->cfsi_used; i++) {
      if (di->cfsi[i].len > 0) {
         if (j != i)
            di->cfsi[j] = di->cfsi[i];
         j++;
      }
   }
   /* VG_(printf)("XXXXXXXXXXXXX %d %d\n", di->cfsi_used, j); */
   di->cfsi_used = j;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < (Word)di->cfsi_used; i++) {
      /* No zero-length ranges. */
      vg_assert(di->cfsi[i].len > 0);
      /* Makes sense w.r.t. summary address range */
      vg_assert(di->cfsi[i].base >= di->cfsi_minavma);
      vg_assert(di->cfsi[i].base + di->cfsi[i].len - 1
                <= di->cfsi_maxavma);

      if (i < di->cfsi_used - 1) {
         /*
         if (!(di->cfsi[i].base < di->cfsi[i+1].base)) {
            VG_(printf)("\nOOO cfsis:\n");
            ML_(ppCfiSI)(&di->cfsi[i]);
            ML_(ppCfiSI)(&di->cfsi[i+1]);
         }
         */
         /* In order. */
         vg_assert(di->cfsi[i].base < di->cfsi[i+1].base);
         /* No overlaps. */
         vg_assert(di->cfsi[i].base + di->cfsi[i].len - 1
                   < di->cfsi[i+1].base);
      }
   }

}


/* Canonicalise the tables held by 'di', in preparation for use.  Call
   this after finishing adding entries to these tables. */
void ML_(canonicaliseTables) ( struct _DebugInfo* di )
{
   canonicaliseSymtab ( di );
   canonicaliseLoctab ( di );
   ML_(canonicaliseCFI) ( di );
   canonicaliseVarInfo ( di );
}


/*------------------------------------------------------------*/
/*--- Searching the tables                                 ---*/
/*------------------------------------------------------------*/

/* Find a symbol-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

Word ML_(search_one_symtab) ( struct _DebugInfo* di, Addr ptr,
                              Bool match_anywhere_in_sym,
                              Bool findText )
{
   Addr a_mid_lo, a_mid_hi;
   Word mid, size, 
        lo = 0, 
        hi = di->symtab_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = di->symtab[mid].addr;
      size = ( match_anywhere_in_sym
             ? di->symtab[mid].size
             : 1);
      a_mid_hi = ((Addr)di->symtab[mid].addr) + size - 1;

      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      /* Found a symbol with the correct address range.  But is it
         of the right kind (text vs data) ? */
      if (  findText   &&   di->symtab[mid].isText  ) return mid;
      if ( (!findText) && (!di->symtab[mid].isText) ) return mid;
      return -1;
   }
}


/* Find a location-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

Word ML_(search_one_loctab) ( struct _DebugInfo* di, Addr ptr )
{
   Addr a_mid_lo, a_mid_hi;
   Word mid, 
        lo = 0, 
        hi = di->loctab_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = di->loctab[mid].addr;
      a_mid_hi = ((Addr)di->loctab[mid].addr) + di->loctab[mid].size - 1;

      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      return mid;
   }
}


/* Find a CFI-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

Word ML_(search_one_cfitab) ( struct _DebugInfo* di, Addr ptr )
{
   Addr a_mid_lo, a_mid_hi;
   Word mid, size, 
        lo = 0, 
        hi = di->cfsi_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = di->cfsi[mid].base;
      size     = di->cfsi[mid].len;
      a_mid_hi = a_mid_lo + size - 1;
      vg_assert(a_mid_hi >= a_mid_lo);
      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      return mid;
   }
}


/* Find a FPO-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

Word ML_(search_one_fpotab) ( struct _DebugInfo* di, Addr ptr )
{
   Addr const addr = ptr - di->rx_map_avma;
   Addr a_mid_lo, a_mid_hi;
   Word mid, size,
        lo = 0,
        hi = di->fpo_size-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = di->fpo[mid].ulOffStart;
      size     = di->fpo[mid].cbProcSize;
      a_mid_hi = a_mid_lo + size - 1;
      vg_assert(a_mid_hi >= a_mid_lo);
      if (addr < a_mid_lo) { hi = mid-1; continue; }
      if (addr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(addr >= a_mid_lo && addr <= a_mid_hi);
      return mid;
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
