
/*--------------------------------------------------------------------*/
/*--- Management of symbols and debugging information.             ---*/
/*---                                                     symtab.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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

#include "core.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_demangle.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_options.h"
#include "pub_core_profile.h"
#include "pub_core_redir.h"
#include "pub_core_tooliface.h"
#include "priv_symtab.h"

#include <elf.h>          /* ELF defns */

static SegInfo* segInfo = NULL;

/*------------------------------------------------------------*/
/*--- 32/64-bit parameterisation                           ---*/
/*------------------------------------------------------------*/

/* For all the ELF macros and types which specify '32' or '64',
   select the correct variant for this platform and give it
   an 'XX' name.  Then use the 'XX' variant consistently in
   the rest of this file. 
*/
#if VG_WORDSIZE == 4
#  define  ElfXX_Ehdr     Elf32_Ehdr
#  define  ElfXX_Shdr     Elf32_Shdr
#  define  ElfXX_Phdr     Elf32_Phdr
#  define  ElfXX_Sym      Elf32_Sym
#  define  ElfXX_Word     Elf32_Word
#  define  ElfXX_Addr     Elf32_Addr
#  define  ElfXX_Dyn      Elf32_Dyn
#  define  ELFXX_ST_BIND  ELF32_ST_BIND
#  define  ELFXX_ST_TYPE  ELF32_ST_TYPE

#elif VG_WORDSIZE == 8
#  define  ElfXX_Ehdr     Elf64_Ehdr
#  define  ElfXX_Shdr     Elf64_Shdr
#  define  ElfXX_Phdr     Elf64_Phdr
#  define  ElfXX_Sym      Elf64_Sym
#  define  ElfXX_Word     Elf64_Word
#  define  ElfXX_Addr     Elf64_Addr
#  define  ElfXX_Dyn      Elf64_Dyn
#  define  ELFXX_ST_BIND  ELF64_ST_BIND
#  define  ELFXX_ST_TYPE  ELF64_ST_TYPE

#else
# error "VG_WORDSIZE should be 4 or 8"
#endif


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

static Bool
intercept_demangle(const Char*, Char*, Int);

/* Majorly rewritten Sun 3 Feb 02 to enable loading symbols from
   dlopen()ed libraries, which is something that KDE3 does a lot.

   Stabs reader greatly improved by Nick Nethercote, Apr 02.
*/

static void freeSegInfo ( SegInfo* si )
{
   struct strchunk *chunk, *next;
   vg_assert(si != NULL);
   if (si->filename) VG_(arena_free)(VG_AR_SYMTAB, si->filename);
   if (si->symtab)   VG_(arena_free)(VG_AR_SYMTAB, si->symtab);
   if (si->loctab)   VG_(arena_free)(VG_AR_SYMTAB, si->loctab);
   if (si->scopetab) VG_(arena_free)(VG_AR_SYMTAB, si->scopetab);
   if (si->cfisi)    VG_(arena_free)(VG_AR_SYMTAB, si->cfisi);

   for(chunk = si->strchunks; chunk != NULL; chunk = next) {
      next = chunk->next;
      VG_(arena_free)(VG_AR_SYMTAB, chunk);
   }
   VG_(arena_free)(VG_AR_SYMTAB, si);
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

Char *VG_(addStr) ( SegInfo* si, Char* str, Int len )
{
#  define EMPTY    NULL
#  define NN       5
   
   /* prevN[0] has the most recent, prevN[NN-1] the least recent */
   static Char     *prevN[NN] = { EMPTY, EMPTY, EMPTY, EMPTY, EMPTY };
   static SegInfo* curr_si = NULL;
   struct strchunk *chunk;
   Int   i, space_needed;

   if (len == -1)
      len = VG_(strlen)(str);

   /* Avoid gratuitous duplication:  if we saw 'str' within the last NN,
    * within this segment, return that index.  Saves about 200KB in glibc,
    * extra time taken is too small to measure.  --NJN 2002-Aug-30 */
   if (curr_si == si) {
      for (i = NN-1; i >= 0; i--) {
         if (EMPTY != prevN[i] 
             && NULL != si->strchunks
	     && 0 == VG_(memcmp)(str, prevN[i], len+1)) {
            return prevN[i];
         }
      }
   } else {
      /* New segment */
      curr_si = si;
      for (i = 0; i < NN; i++) prevN[i] = EMPTY;
   }
   /* Shuffle prevous ones along, put new one in. */
   for (i = NN-1; i > 0; i--)
      prevN[i] = prevN[i-1];

#  undef EMPTY

   space_needed = 1 + len;

   if (si->strchunks == NULL || 
       (si->strchunks->strtab_used + space_needed) > STRCHUNKSIZE) {
      chunk = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*chunk));
      chunk->strtab_used = 0;
      chunk->next = si->strchunks;
      si->strchunks = chunk;
   }
   chunk = si->strchunks;

   prevN[0] = &chunk->strtab[chunk->strtab_used];
   VG_(memcpy)(prevN[0], str, len);
   chunk->strtab[chunk->strtab_used+len] = '\0';
   chunk->strtab_used += space_needed;

   return prevN[0];
}

/* Add a symbol to the symbol table. */

static __inline__
void addSym ( SegInfo* si, RiSym* sym )
{
   UInt   new_sz, i;
   RiSym* new_tab;

   /* Ignore zero-sized syms. */
   if (sym->size == 0) return;

   if (si->symtab_used == si->symtab_size) {
      new_sz = 2 * si->symtab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = VG_(arena_malloc)(VG_AR_SYMTAB, new_sz * sizeof(RiSym) );
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

/* Add a location to the location table. */

static __inline__
void addLoc ( SegInfo* si, RiLoc* loc )
{
   UInt   new_sz, i;
   RiLoc* new_tab;

   /* Zero-sized locs should have been ignored earlier */
   vg_assert(loc->size > 0);

   if (si->loctab_used == si->loctab_size) {
      new_sz = 2 * si->loctab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = VG_(arena_malloc)(VG_AR_SYMTAB, new_sz * sizeof(RiLoc) );
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


/* Top-level place to call to add a source-location mapping entry. */

void VG_(addLineInfo) ( SegInfo* si,
			Char*    filename,
			Addr     this,
			Addr     next,
			Int      lineno,
			Int      entry /* only needed for debug printing */
		       )
{
   static const Bool debug = False;
   RiLoc loc;
   Int size = next - this;

   /* Ignore zero-sized locs */
   if (this == next) return;

   if (debug)
      VG_(printf)("  src %s line %d %p-%p\n", filename, lineno, this, next);

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

   /* vg_assert(this < si->start + si->size && next-1 >= si->start); */
   if (this >= si->start + si->size || next-1 < si->start) {
       if (0)
       VG_(message)(Vg_DebugMsg, 
                    "warning: ignoring line info entry falling "
                    "outside current SegInfo: %p %p %p %p",
                    si->start, si->start + si->size, 
                    this, next-1);
       return;
   }

   vg_assert(lineno >= 0);
   if (lineno > MAX_LINENO) {
       VG_(message)(Vg_UserMsg, 
                    "warning: ignoring line info entry with "
                    "huge line number (%d)", lineno);
       VG_(message)(Vg_UserMsg, 
                    "         Can't handle line numbers "
                    "greater than %d, sorry", MAX_LINENO);
       return;
   }

   loc.addr      = this;
   loc.size      = (UShort)size;
   loc.lineno    = lineno;
   loc.filename  = filename;

   if (0) VG_(message)(Vg_DebugMsg, 
		       "addLoc: addr %p, size %d, line %d, file %s",
		       this,size,lineno,filename);

   addLoc ( si, &loc );
}

static __inline__
void addScopeRange ( SegInfo* si, ScopeRange *range )
{
   Int    new_sz, i;
   ScopeRange* new_tab;

   /* Zero-sized scopes should have been ignored earlier */
   vg_assert(range->size > 0);

   if (si->scopetab_used == si->scopetab_size) {
      new_sz = 2 * si->scopetab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = VG_(arena_malloc)(VG_AR_SYMTAB, new_sz * sizeof(*new_tab) );
      if (si->scopetab != NULL) {
         for (i = 0; i < si->scopetab_used; i++)
            new_tab[i] = si->scopetab[i];
         VG_(arena_free)(VG_AR_SYMTAB, si->scopetab);
      }
      si->scopetab = new_tab;
      si->scopetab_size = new_sz;
   }

   si->scopetab[si->scopetab_used] = *range;
   si->scopetab_used++;
   vg_assert(si->scopetab_used <= si->scopetab_size);
}


/* Top-level place to call to add a source-location mapping entry. */

void VG_(addScopeInfo) ( SegInfo* si,
			 Addr     this,
			 Addr     next,
			 Scope    *scope)
{
   static const Bool debug = False;
   Int size = next - this;
   ScopeRange range;

   /* Ignore zero-sized or negative scopes */
   if (size <= 0) {
      if (debug)
	 VG_(printf)("ignoring zero-sized range, scope %p at %p\n", scope, this);
      return;
   }

   if (debug)
      VG_(printf)("adding scope range %p-%p (size=%d)  scope %p (%d)\n",
		  this, next, next-this, scope, scope->depth);

   range.addr    = this;
   range.size    = size;
   range.scope   = scope;

   addScopeRange ( si, &range );
}


/* Top-level place to call to add a CFI summary record.  The supplied
   CfiSI is copied. */
void VG_(addCfiSI) ( SegInfo* si, CfiSI* cfisi )
{
   static const Bool debug = False;

   if (debug) {
      VG_(printf)("adding CfiSI: ");
      VG_(ppCfiSI)(cfisi);
   }

   vg_assert(cfisi->len > 0 && cfisi->len < 2000000);

   UInt   new_sz, i;
   CfiSI* new_tab;

   /* Rule out ones which are completely outside the segment.  These
      probably indicate some kind of bug, but for the meantime ignore
      them. */
   if ( cfisi->base + cfisi->len - 1  <  si->start
        || si->start + si->size - 1 < cfisi->base ) {
      static Int complaints = 3;
      if (VG_(clo_trace_cfi) || complaints > 0) {
         complaints--;
         if (VG_(clo_verbosity) > 1) {
            VG_(message)(
               Vg_DebugMsg,
               "warning: CfiSI %p .. %p outside segment %p .. %p",
               cfisi->base, 
               cfisi->base + cfisi->len - 1,
               si->start,
               si->start + si->size - 1 
            );
         }
         if (VG_(clo_trace_cfi)) 
            VG_(ppCfiSI)(cfisi);
      }
      return;
   }

   if (si->cfisi_used == si->cfisi_size) {
      new_sz = 2 * si->cfisi_size;
      if (new_sz == 0) new_sz = 20;
      new_tab = VG_(arena_malloc)(VG_AR_SYMTAB, new_sz * sizeof(CfiSI) );
      if (si->cfisi != NULL) {
         for (i = 0; i < si->cfisi_used; i++)
            new_tab[i] = si->cfisi[i];
         VG_(arena_free)(VG_AR_SYMTAB, si->cfisi);
      }
      si->cfisi = new_tab;
      si->cfisi_size = new_sz;
   }

   si->cfisi[si->cfisi_used] = *cfisi;
   si->cfisi_used++;
   vg_assert(si->cfisi_used <= si->cfisi_size);
}


/*------------------------------------------------------------*/
/*--- Helpers                                              ---*/
/*------------------------------------------------------------*/

/* Non-fatal -- use vg_panic if terminal. */
void VG_(symerr) ( Char* msg )
{
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg,"%s", msg );
}


/* Print a symbol. */
static
void printSym ( SegInfo* si, Int i )
{
  VG_(printf)( "%5d:  %8p .. %8p (%d)      %s\n",
               i,
               si->symtab[i].addr, 
               si->symtab[i].addr + si->symtab[i].size - 1, si->symtab[i].size,
	       si->symtab[i].name );
}

#define TRACE_SYMTAB(format, args...) \
   if (VG_(clo_trace_symtab)) { VG_(printf)(format, ## args); }


#if 0
/* Print the entire sym tab. */
static __attribute__ ((unused))
void printSymtab ( void )
{
   Int i;
   VG_(printf)("\n------ BEGIN vg_symtab ------\n");
   for (i = 0; i < vg_symtab_used; i++)
      printSym(i);
   VG_(printf)("------ BEGIN vg_symtab ------\n");
}
#endif

#if 0
/* Paranoid strcat. */
static
void safeCopy ( UChar* dst, UInt maxlen, UChar* src )
{
   UInt i = 0, j = 0;
   while (True) {
      if (i >= maxlen) return;
      if (dst[i] == 0) break;
      i++;
   }
   while (True) {
      if (i >= maxlen) return;
      dst[i] = src[j];
      if (src[j] == 0) return;
      i++; j++;
   }
}
#endif


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
static Int compare_RiSym(void *va, void *vb) {
   RiSym *a = (RiSym *)va;
   RiSym *b = (RiSym *)vb;
   
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
 */
static RiSym *prefersym(RiSym *a, RiSym *b)
{
   Int lena, lenb;		/* full length */
   Int vlena, vlenb;		/* length without version */
   const Char *vpa, *vpb;

   vlena = lena = VG_(strlen)(a->name);
   vlenb = lenb = VG_(strlen)(b->name);

   vpa = VG_(strchr)(a->name, '@');
   vpb = VG_(strchr)(b->name, '@');

   if (vpa)
      vlena = vpa - a->name;
   if (vpb)
      vlenb = vpb - b->name;

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

static 
void canonicaliseSymtab ( SegInfo* si )
{
   Int   i, j, n_merged, n_truncated;
   Addr  s1, s2, e1, e2;

#  define SWAP(ty,aa,bb) \
      do { ty tt = (aa); (aa) = (bb); (bb) = tt; } while (0)

   if (si->symtab_used == 0)
      return;

   VG_(ssort)(si->symtab, si->symtab_used, sizeof(*si->symtab), compare_RiSym);

   for (i = 0; i < si->symtab_used; i++) {
      if(VG_(strncmp)(si->symtab[i].name, VG_INTERCEPT_PREFIX,
                      VG_INTERCEPT_PREFIX_LEN) == 0) {
         int len = VG_(strlen)(si->symtab[i].name);
         char *buf = VG_(arena_malloc)(VG_AR_SYMTAB, len), *colon;
         intercept_demangle(si->symtab[i].name, buf, len);
	 colon = buf + VG_(strlen)(buf) - 1;
	 while(*colon != ':') colon--;
	 VG_(strncpy_safely)(si->symtab[i].name, colon+1, len);
	 VG_(arena_free)(VG_AR_SYMTAB, buf);
      }
   }

  cleanup_more:
 
   /* If two symbols have identical address ranges, favour the
      one with the longer name (unless the extra length is junk)
   */
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
	    si->symtab[si->symtab_used++] = *prefersym(&si->symtab[i], &si->symtab[i+1]);
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
      if (VG_(clo_trace_symtab)) {
         VG_(printf)("overlapping address ranges in symbol table\n\t");
         printSym(si,i);
         VG_(printf)("\t");
         printSym(si,i+1);
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
         SWAP(RiSym,si->symtab[j],si->symtab[j+1]);
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

/* Sort the scope range table by starting address.  Mash the table
   around so as to establish the property that addresses are in order
   and the ranges do not overlap.  This facilitates using binary
   search to map addresses to scopes when we come to query the
   table.
*/
static Int compare_ScopeRange(void *va, void *vb) {
   ScopeRange *a = (ScopeRange *)va;
   ScopeRange *b = (ScopeRange *)vb;
   
   if (a->addr < b->addr) return -1;
   if (a->addr > b->addr) return  1;
   return 0;
}

static 
void canonicaliseScopetab ( SegInfo* si )
{
   Int i,j;

   if (si->scopetab_used == 0)
      return;

   /* Sort by start address. */
   VG_(ssort)(si->scopetab, si->scopetab_used, sizeof(*si->scopetab), 
	      compare_ScopeRange);

   /* If two adjacent entries overlap, truncate the first. */
   for (i = 0; i < si->scopetab_used-1; i++) {
      if (si->scopetab[i].addr + si->scopetab[i].size > si->scopetab[i+1].addr) {
         Int new_size = si->scopetab[i+1].addr - si->scopetab[i].addr;

         if (new_size < 0)
            si->scopetab[i].size = 0;
         else
	    si->scopetab[i].size = new_size;
      }
   }

   /* Zap any zero-sized entries resulting from the truncation
      process. */
   j = 0;
   for (i = 0; i < si->scopetab_used; i++) {
      if (si->scopetab[i].size > 0) {
         si->scopetab[j] = si->scopetab[i];
         j++;
      }
   }
   si->scopetab_used = j;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < si->scopetab_used-1; i++) {
      /* 
      VG_(printf)("%d   (%d) %d 0x%x\n", 
                   i, si->scopetab[i+1].confident, 
                   si->scopetab[i+1].size, si->scopetab[i+1].addr );
      */
      /* No zero-sized symbols. */
      vg_assert(si->scopetab[i].size > 0);
      /* In order. */
      if (si->scopetab[i].addr >= si->scopetab[i+1].addr)
	 VG_(printf)("si->scopetab[%d] = %p,size=%d [%d] = %p,size=%d\n",
		     i, si->scopetab[i].addr, si->scopetab[i].size,
		     i+1, si->scopetab[i+1].addr, si->scopetab[i+1].size);
      vg_assert(si->scopetab[i].addr < si->scopetab[i+1].addr);
      /* No overlaps. */
      vg_assert(si->scopetab[i].addr + si->scopetab[i].size - 1
                < si->scopetab[i+1].addr);
   }
}


/* Sort the location table by starting address.  Mash the table around
   so as to establish the property that addresses are in order and the
   ranges do not overlap.  This facilitates using binary search to map
   addresses to locations when we come to query the table.  
*/
static Int compare_RiLoc(void *va, void *vb) {
   RiLoc *a = (RiLoc *)va;
   RiLoc *b = (RiLoc *)vb;

   if (a->addr < b->addr) return -1;
   if (a->addr > b->addr) return  1;
   return 0;
}

static 
void canonicaliseLoctab ( SegInfo* si )
{
   Int   i, j;

#  define SWAP(ty,aa,bb) \
      do { ty tt = (aa); (aa) = (bb); (bb) = tt; } while (0);

   if (si->loctab_used == 0)
      return;

   /* Sort by start address. */
   VG_(ssort)(si->loctab, si->loctab_used, sizeof(*si->loctab), compare_RiLoc);

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
static Int compare_CfiSI(void *va, void *vb) {
   CfiSI *a = (CfiSI*)va;
   CfiSI *b = (CfiSI*)vb;

   if (a->base < b->base) return -1;
   if (a->base > b->base) return  1;
   return 0;
}

static
void canonicaliseCfiSI ( SegInfo* si )
{
   Int   i;
   const Addr minAddr = 0;
   const Addr maxAddr = ~minAddr;

   /* Set cfisi_minaddr and cfisi_maxaddr to summarise the entire
      address range contained in cfisi[0 .. cfisi_used-1]. */
   si->cfisi_minaddr = maxAddr; 
   si->cfisi_maxaddr = minAddr;
   for (i = 0; i < si->cfisi_used; i++) {
      Addr here_min = si->cfisi[i].base;
      Addr here_max = si->cfisi[i].base + si->cfisi[i].len - 1;
      if (here_min < si->cfisi_minaddr)
         si->cfisi_minaddr = here_min;
      if (here_max > si->cfisi_maxaddr)
         si->cfisi_maxaddr = here_max;
   }

   if (VG_(clo_trace_cfi))
      VG_(printf)("canonicaliseCfiSI: %d entries, %p .. %p\n", 
                  si->cfisi_used,
	          si->cfisi_minaddr, si->cfisi_maxaddr);

   /* Sort the cfisi array by base address. */
   VG_(ssort)(si->cfisi, si->cfisi_used, sizeof(*si->cfisi), compare_CfiSI);

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < si->cfisi_used; i++) {
      /* No zero-length ranges. */
      vg_assert(si->cfisi[i].len > 0);
      /* Makes sense w.r.t. summary address range */
      vg_assert(si->cfisi[i].base >= si->cfisi_minaddr);
      vg_assert(si->cfisi[i].base + si->cfisi[i].len - 1
                <= si->cfisi_maxaddr);

      if (i < si->cfisi_used - 1) {
         /*
         if (!(si->cfisi[i].base < si->cfisi[i+1].base)) {
            VG_(printf)("\nOOO cfisis:\n");
            VG_(ppCfiSI)(&si->cfisi[i]);
            VG_(ppCfiSI)(&si->cfisi[i+1]);
         }
         */
         /* In order. */
         vg_assert(si->cfisi[i].base < si->cfisi[i+1].base);
         /* No overlaps. */
         vg_assert(si->cfisi[i].base + si->cfisi[i].len - 1
                   < si->cfisi[i+1].base);
      }
   }

}


/*------------------------------------------------------------*/
/*--- Read info from a .so/exe file.                       ---*/
/*------------------------------------------------------------*/

Bool VG_(is_object_file)(const void *buf)
{
   {
      ElfXX_Ehdr *ehdr = (ElfXX_Ehdr *)buf;
      Int ok = 1;

      ok &= (ehdr->e_ident[EI_MAG0] == 0x7F
             && ehdr->e_ident[EI_MAG1] == 'E'
             && ehdr->e_ident[EI_MAG2] == 'L'
             && ehdr->e_ident[EI_MAG3] == 'F');
      ok &= (ehdr->e_ident[EI_CLASS] == VGA_ELF_CLASS
             && ehdr->e_ident[EI_DATA] == VGA_ELF_ENDIANNESS
             && ehdr->e_ident[EI_VERSION] == EV_CURRENT);
      ok &= (ehdr->e_type == ET_EXEC || ehdr->e_type == ET_DYN);
      ok &= (ehdr->e_machine == VGA_ELF_MACHINE);
      ok &= (ehdr->e_version == EV_CURRENT);
      ok &= (ehdr->e_shstrndx != SHN_UNDEF);
      ok &= (ehdr->e_shoff != 0 && ehdr->e_shnum != 0);
      ok &= (ehdr->e_phoff != 0 && ehdr->e_phnum != 0);

      if (ok)
	 return True;
   }

   /* other file formats here? */

   return False;
}

/* Demangle an intercept symbol into library:func form
   eg "_vgi_libcZdsoZd6__ZdlPv"  -->  "libc.so.6:_ZdlPv"
   Uses the Z-encoding scheme described in vg_replace_malloc.c.
   Returns True if demangle OK, False otherwise.
 */

static Bool
intercept_demangle(const Char* symbol, Char* result, Int nbytes)
{
#  define EMIT(ch)                    \
      do {                            \
         if (j >= nbytes)             \
            result[j-1] = 0;          \
         else                         \
            result[j++] = ch;         \
      } while (0)

   Bool error = False;
   Int i, j = 0;
   Int len = VG_(strlen)(symbol);
   if (0) VG_(printf)("idm: %s\n", symbol);

   i = VG_INTERCEPT_PREFIX_LEN;

   /* Chew though the Z-encoded soname part. */
   while (True) {

      if (i >= len) 
         break;

      if (symbol[i] == '_')
         /* We found the underscore following the Z-encoded soname.
            Just copy the rest literally. */
         break;

      if (symbol[i] != 'Z') {
         EMIT(symbol[i]);
         i++;
         continue;
      }

      /* We've got a Z-escape.  Act accordingly. */
      i++;
      if (i >= len) {
         /* Hmm, Z right at the end.  Something's wrong. */
         error = True;
         EMIT('Z');
         break;
      }
      switch (symbol[i]) {
         case 'a': EMIT('*'); break;
         case 'p': EMIT('+'); break;
         case 'c': EMIT(':'); break;
         case 'd': EMIT('.'); break;
         case 'u': EMIT('_'); break;
         case 's': EMIT(' '); break;
         case 'Z': EMIT('Z'); break;
         default: error = True; EMIT('Z'); EMIT(symbol[i]); break;
      }
      i++;
   }

   if (error || i >= len || symbol[i] != '_') {
      /* Something's wrong.  Give up. */
      VG_(message)(Vg_UserMsg, "intercept: error demangling: %s", symbol);
      EMIT(0);
      return False;
   }

   /* Copy the rest of the string verbatim. */
   i++;
   EMIT(':');
   while (True) {
     if (i >= len)
        break;
     EMIT(symbol[i]);
     i++;
   }

   EMIT(0);
   if (0) VG_(printf)("%s\n", result);
   return True;

#  undef EMIT
}

static
void handle_intercept( SegInfo* si, Char* symbol, ElfXX_Sym* sym)
{
   Bool ok;
   Int  len  = VG_(strlen)(symbol) + 1 - VG_INTERCEPT_PREFIX_LEN;
   Char *lib = VG_(arena_malloc)(VG_AR_SYMTAB, len+8);
   Char *func;

   /* Put "soname:" at the start of lib. */
   lib[0] = 's';
   lib[1] = 'o';
   lib[2] = 'n';
   lib[3] = 'a';
   lib[4] = 'm';
   lib[5] = 'e';
   lib[6] = ':';
   lib[7] = 0;

   ok = intercept_demangle(symbol, lib+7, len);
   if (ok) {
      func = lib + VG_(strlen)(lib)-1;

      while(*func != ':') func--;
      *func = '\0';

      if (0) VG_(printf)("lib A%sZ, func A%sZ\n", lib, func+1);
      VG_(add_redirect_sym_to_addr)(lib, func+1, si->offset + sym->st_value);
   }

   VG_(arena_free)(VG_AR_SYMTAB, lib);
}

//static
//void handle_wrapper( SegInfo* si, Char* symbol, ElfXX_Sym* sym)
//{
//   if (VG_(strcmp)(symbol, STR(VG_WRAPPER(freeres))) == 0)
//      VGA_(intercept_libc_freeres_wrapper)((Addr)(si->offset + sym->st_value));
//   else if (VG_(strcmp)(symbol, STR(VG_WRAPPER(pthread_startfunc_wrapper))) == 0)
//      VG_(pthread_startfunc_wrapper)((Addr)(si->offset + sym->st_value));
//}

/* Read a symbol table (normal or dynamic) */
static
void read_symtab( SegInfo* si, Char* tab_name, Bool do_intercepts,
                  ElfXX_Sym* o_symtab, UInt o_symtab_sz,
                  UChar*     o_strtab, UInt o_strtab_sz )
{
   Int   i;
   Addr  sym_addr;
   RiSym risym;
   Char* t0;
   Char* name;

   if (o_strtab == NULL || o_symtab == NULL) {
      Char buf[80];
      vg_assert(VG_(strlen)(tab_name) < 40);
      VG_(sprintf)(buf, "   object doesn't have a %s", tab_name);
      VG_(symerr)(buf);
      return;
   }

   TRACE_SYMTAB("Reading %s (%d entries)\n", tab_name, 
                o_symtab_sz/sizeof(ElfXX_Sym) );

   /* Perhaps should start at i = 1; ELF docs suggest that entry
      0 always denotes 'unknown symbol'. */
   for (i = 1; i < (Int)(o_symtab_sz/sizeof(ElfXX_Sym)); i++) {
      ElfXX_Sym* sym = & o_symtab[i];
#     if 1
      sym_addr = si->offset + sym->st_value;

      if (VG_(clo_trace_symtab)) {
         VG_(printf)("raw symbol [%d]: ", i);
         switch (ELFXX_ST_BIND(sym->st_info)) {
         case STB_LOCAL:  VG_(printf)("LOC "); break;
         case STB_GLOBAL: VG_(printf)("GLO "); break;
         case STB_WEAK:   VG_(printf)("WEA "); break;
         case STB_LOPROC: VG_(printf)("lop "); break;
         case STB_HIPROC: VG_(printf)("hip "); break;
         default:         VG_(printf)("??? "); break;
         }
         switch (ELFXX_ST_TYPE(sym->st_info)) {
         case STT_NOTYPE:  VG_(printf)("NOT "); break;
         case STT_OBJECT:  VG_(printf)("OBJ "); break;
         case STT_FUNC:    VG_(printf)("FUN "); break;
         case STT_SECTION: VG_(printf)("SEC "); break;
         case STT_FILE:    VG_(printf)("FIL "); break;
         case STT_LOPROC:  VG_(printf)("lop "); break;
         case STT_HIPROC:  VG_(printf)("hip "); break;
         default:          VG_(printf)("??? "); break;
         }
         VG_(printf)(
            ": value %p, size %d, name %s\n",
            sym_addr, sym->st_size,
            ( sym->st_name 
            ? ((Char*)o_strtab+sym->st_name) 
            : (Char*)"NONAME" ) ); 
      }               
#     endif

      /*
       * Is this symbol a magic valgrind-intercept symbol?  If so,
       * hand this off to the interceptinator.
       */
      if (do_intercepts) {
         if (VG_(strncmp)((Char*)o_strtab+sym->st_name,
                          VG_INTERCEPT_PREFIX,
			  VG_INTERCEPT_PREFIX_LEN) == 0) {
           handle_intercept(si, (Char*)o_strtab+sym->st_name, sym);
         } 
         //else if (VG_(strncmp)((Char*)o_strtab+sym->st_name,
         //                        VG_WRAPPER_PREFIX,
	 //			 VG_WRAPPER_PREFIX_LEN) == 0) {
         //  handle_wrapper(si, (Char*)o_strtab+sym->st_name, sym);
         //}
      }

      /* Figure out if we're interested in the symbol.
         Firstly, is it of the right flavour?  */
      if ( ! ( (ELFXX_ST_BIND(sym->st_info) == STB_GLOBAL ||
                ELFXX_ST_BIND(sym->st_info) == STB_LOCAL ||
                ELFXX_ST_BIND(sym->st_info) == STB_WEAK)
             &&
               (ELFXX_ST_TYPE(sym->st_info) == STT_FUNC ||
                (VG_(needs).data_syms 
                 && ELFXX_ST_TYPE(sym->st_info) == STT_OBJECT))
             )
         )
         continue;

      /* Secondly, if it's apparently in a GOT or PLT, it's really
         a reference to a symbol defined elsewhere, so ignore it. */
      if (si->got_start != 0
          && sym_addr >= si->got_start 
          && sym_addr <  si->got_start + si->got_size) {
         TRACE_SYMTAB("in GOT: %s\n", o_strtab+sym->st_name);
         continue;
      }
      if (si->plt_start != 0
          && sym_addr >= si->plt_start
          && sym_addr <  si->plt_start + si->plt_size) {
         TRACE_SYMTAB("in PLT: %s\n", o_strtab+sym->st_name);
         continue;
      }

      /* Don't bother if nameless, or zero-sized. */
      if (sym->st_name == (ElfXX_Word)NULL
          || /* VG_(strlen)(o_strtab+sym->st_name) == 0 */
             /* equivalent but cheaper ... */
             * ((UChar*)(o_strtab+sym->st_name)) == 0
          || sym->st_size == 0) {
         TRACE_SYMTAB("size=0: %s\n", o_strtab+sym->st_name);
         continue;
      }

#     if 0
      /* Avoid _dl_ junk.  (Why?) */
      /* 01-02-24: disabled until I find out if it really helps. */
      if (VG_(strncmp)("_dl_", o_strtab+sym->st_name, 4) == 0
          || VG_(strncmp)("_r_debug", 
                          o_strtab+sym->st_name, 8) == 0) {
         TRACE_SYMTAB("_dl_ junk: %s\n", o_strtab+sym->st_name);
         continue;
      }
#     endif

      /* This seems to significantly reduce the number of junk
         symbols, and particularly reduces the number of
         overlapping address ranges.  Don't ask me why ... */
      if ((Int)sym->st_value == 0) {
         TRACE_SYMTAB( "valu=0: %s\n", o_strtab+sym->st_name);
         continue;
      }

      /* If no part of the symbol falls within the mapped range,
         ignore it. */
      if (sym_addr+sym->st_size <= si->start
          || sym_addr >= si->start+si->size) {
         TRACE_SYMTAB( "outside mapped range" );
         continue;
      }

      /* If we reach here, it's an interesting symbol; record it. */
      t0 = sym->st_name 
                    ? (Char*)(o_strtab+sym->st_name) 
                    : (Char*)"NONAME";
      name = VG_(addStr) ( si, t0, -1 );
      vg_assert(name != NULL
                /* && 0==VG_(strcmp)(t0,&vg_strtab[nmoff]) */ );
      /* VG_(printf)("%p + %d:   %p %s\n", si->start, 
                  (Int)sym->st_value, sym_addr,  t0 ); */
      risym.addr  = sym_addr;
      risym.size  = sym->st_size;
      risym.name  = name;
      addSym ( si, &risym );
   }
}

/*
 * This routine for calculating the CRC for a separate debug file
 * is GPLed code borrowed from binutils.
 */
static UInt
calc_gnu_debuglink_crc32(UInt crc, const UChar *buf, Int len)
{
  static const UInt crc32_table[256] =
    {
      0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419,
      0x706af48f, 0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4,
      0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07,
      0x90bf1d91, 0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
      0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7, 0x136c9856,
      0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9,
      0xfa0f3d63, 0x8d080df5, 0x3b6e20c8, 0x4c69105e, 0xd56041e4,
      0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
      0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3,
      0x45df5c75, 0xdcd60dcf, 0xabd13d59, 0x26d930ac, 0x51de003a,
      0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599,
      0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
      0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d, 0x76dc4190,
      0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f,
      0x9fbfe4a5, 0xe8b8d433, 0x7807c9a2, 0x0f00f934, 0x9609a88e,
      0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
      0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed,
      0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
      0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3,
      0xfbd44c65, 0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
      0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a,
      0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5,
      0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa, 0xbe0b1010,
      0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
      0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17,
      0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6,
      0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615,
      0x73dc1683, 0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
      0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1, 0xf00f9344,
      0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
      0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a,
      0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
      0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1,
      0xa6bc5767, 0x3fb506dd, 0x48b2364b, 0xd80d2bda, 0xaf0a1b4c,
      0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef,
      0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
      0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe,
      0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31,
      0x2cd99e8b, 0x5bdeae1d, 0x9b64c2b0, 0xec63f226, 0x756aa39c,
      0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
      0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b,
      0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
      0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1,
      0x18b74777, 0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
      0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45, 0xa00ae278,
      0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7,
      0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc, 0x40df0b66,
      0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
      0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605,
      0xcdd70693, 0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8,
      0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b,
      0x2d02ef8d
    };
  const UChar *end;

  crc = ~crc & 0xffffffff;
  for (end = buf + len; buf < end; ++ buf)
    crc = crc32_table[(crc ^ *buf) & 0xff] ^ (crc >> 8);
  return ~crc & 0xffffffff;;
}

/*
 * Try and open a separate debug file, ignoring any where the CRC does
 * not match the value from the main object file.
 */
static
Addr open_debug_file( Char* name, UInt crc, UInt* size )
{
   Int fd;
   struct vki_stat stat_buf;
   Addr addr;
   UInt calccrc;

   if ((fd = VG_(open)(name, VKI_O_RDONLY, 0)) < 0)
      return 0;

   if (VG_(fstat)(fd, &stat_buf) != 0) {
      VG_(close)(fd);
      return 0;
   }

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, "Reading debug info from %s...", name);

   *size = stat_buf.st_size;
   
   if ((addr = (Addr)VG_(mmap)(NULL, *size, VKI_PROT_READ,
                               VKI_MAP_PRIVATE|VKI_MAP_NOSYMS, 
                               0, fd, 0)) == (Addr)-1) 
   {
      VG_(close)(fd);
      return 0;
   }

   VG_(close)(fd);
   
   calccrc = calc_gnu_debuglink_crc32(0, (UChar*)addr, *size);
   if (calccrc != crc) {
      int res = VG_(munmap)((void*)addr, *size);
      vg_assert(0 == res);
      if (VG_(clo_verbosity) > 1)
	 VG_(message)(Vg_DebugMsg, "... CRC mismatch (computed %08x wanted %08x)", calccrc, crc);
      return 0;
   }
   
   return addr;
}

/*
 * Try to find a separate debug file for a given object file.
 */
static
Addr find_debug_file( Char* objpath, Char* debugname, UInt crc, UInt* size )
{
   Char *objdir = VG_(arena_strdup)(VG_AR_SYMTAB, objpath);
   Char *objdirptr;
   Char *debugpath;
   Addr addr = 0;
  
   if ((objdirptr = VG_(strrchr)(objdir, '/')) != NULL)
      *objdirptr = '\0';

   debugpath = VG_(arena_malloc)(VG_AR_SYMTAB, VG_(strlen)(objdir) + VG_(strlen)(debugname) + 16);
   
   VG_(sprintf)(debugpath, "%s/%s", objdir, debugname);

   if ((addr = open_debug_file(debugpath, crc, size)) == 0) {
      VG_(sprintf)(debugpath, "%s/.debug/%s", objdir, debugname);
      if ((addr = open_debug_file(debugpath, crc, size)) == 0) {
         VG_(sprintf)(debugpath, "/usr/lib/debug%s/%s", objdir, debugname);
         addr = open_debug_file(debugpath, crc, size);
      }
   }

   VG_(arena_free)(VG_AR_SYMTAB, debugpath);
   VG_(arena_free)(VG_AR_SYMTAB, objdir);
   
   return addr;
}

/* Read the symbols from the object/exe specified by the SegInfo into
   the tables within the supplied SegInfo.  */
static
Bool read_lib_symbols ( SegInfo* si )
{
   Bool          res;
   ElfXX_Ehdr*   ehdr;       /* The ELF header                          */
   ElfXX_Shdr*   shdr;       /* The section table                       */
   UChar*        sh_strtab;  /* The section table's string table        */
   Int           fd;
   Int           i;
   Bool          ok;
   Addr          oimage;
   UInt          n_oimage;
   Addr          dimage = 0;
   UInt          n_dimage = 0;
   struct vki_stat stat_buf;

   oimage = (Addr)NULL;
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, "Reading syms from %s (%p)", si->filename, si->start );

   /* mmap the object image aboard, so that we can read symbols and
      line number info out of it.  It will be munmapped immediately
      thereafter; it is only aboard transiently. */

   i = VG_(stat)(si->filename, &stat_buf);
   if (i != 0) {
      VG_(symerr)("Can't stat .so/.exe (to determine its size)?!");
      return False;
   }
   n_oimage = stat_buf.st_size;

   fd = VG_(open)(si->filename, VKI_O_RDONLY, 0);
   if (fd < 0) {
      VG_(symerr)("Can't open .so/.exe to read symbols?!");
      return False;
   }

   oimage = (Addr)VG_(mmap)( NULL, n_oimage, 
                             VKI_PROT_READ, VKI_MAP_PRIVATE|VKI_MAP_NOSYMS, 
                             0, fd, 0 );

   VG_(close)(fd);

   if (oimage == ((Addr)(-1))) {
      VG_(message)(Vg_UserMsg, "warning: mmap failed on %s", si->filename );
      VG_(message)(Vg_UserMsg, "         no symbols or debug info loaded" );
      return False;
   }

   /* Ok, the object image is safely in oimage[0 .. n_oimage-1]. 
      Now verify that it is a valid ELF .so or executable image.
   */
   res = False;
   ok = (n_oimage >= sizeof(ElfXX_Ehdr));
   ehdr = (ElfXX_Ehdr*)oimage;

   if (ok)
      ok &= VG_(is_object_file)(ehdr);

   if (!ok) {
      VG_(symerr)("Invalid ELF header, or missing stringtab/sectiontab.");
      goto out;
   }

   /* Walk the LOAD headers in the phdr and update the SegInfo to
      include them all, so that this segment also contains data and
      bss memory.  Also computes correct symbol offset value for this
      ELF file. */
   if (ehdr->e_phoff + ehdr->e_phnum*sizeof(ElfXX_Phdr) > n_oimage) {
      VG_(symerr)("ELF program header is beyond image end?!");
      goto out;
   }
   {
      Bool offset_set = False;
      ElfXX_Addr prev_addr = 0;
      Addr baseaddr = 0;

      si->offset = 0;

      for (i = 0; i < ehdr->e_phnum; i++) {
	 ElfXX_Phdr *o_phdr;
	 ElfXX_Addr mapped, mapped_end;

	 o_phdr = &((ElfXX_Phdr *)(oimage + ehdr->e_phoff))[i];

	 if (o_phdr->p_type == PT_DYNAMIC && si->soname == NULL) {
	    const ElfXX_Dyn *dyn = (const ElfXX_Dyn *)(oimage + o_phdr->p_offset);
	    Int stroff = -1;
	    Char *strtab = NULL;
	    Int j;
	    
	    for(j = 0; dyn[j].d_tag != DT_NULL; j++) {
	       switch(dyn[j].d_tag) {
	       case DT_SONAME:
		  stroff =  dyn[j].d_un.d_val;
		  break;

	       case DT_STRTAB:
		  strtab = (Char *)oimage + dyn[j].d_un.d_ptr - baseaddr;
		  break;
	       }
	    }

	    if (stroff != -1 && strtab != 0) {
	       TRACE_SYMTAB("soname=%s\n", strtab+stroff);
	       si->soname = VG_(arena_strdup)(VG_AR_SYMTAB, strtab+stroff);
	    }
	 }

	 if (o_phdr->p_type != PT_LOAD)
	    continue;

	 if (!offset_set) {
	    offset_set = True;
	    si->offset = si->start - o_phdr->p_vaddr;
	    baseaddr = o_phdr->p_vaddr;
	 }

	 if (o_phdr->p_vaddr < prev_addr) {
	    VG_(symerr)("ELF Phdrs are out of order!?");
            goto out;
	 }
	 prev_addr = o_phdr->p_vaddr;

	 mapped = o_phdr->p_vaddr + si->offset;
	 mapped_end = mapped + o_phdr->p_memsz;

	 if (si->data_start == 0 &&
	     (o_phdr->p_flags & (PF_R|PF_W|PF_X)) == (PF_R|PF_W)) {
	    si->data_start = mapped;
	    si->data_size = o_phdr->p_filesz;
	    si->bss_start = mapped + o_phdr->p_filesz;
	    if (o_phdr->p_memsz > o_phdr->p_filesz)
	       si->bss_size = o_phdr->p_memsz - o_phdr->p_filesz;
	    else
	       si->bss_size = 0;
	 }

	 mapped = mapped & ~(VKI_PAGE_SIZE-1);
	 mapped_end = (mapped_end + VKI_PAGE_SIZE - 1) & ~(VKI_PAGE_SIZE-1);

#if 0
	 /* 20050228: disabled this until VG_(next_segment) can be
	    reinstated in some clean incarnation of the low level
	    memory manager. */
	 if (VG_(needs).data_syms &&
	     (mapped >= si->start && mapped <= (si->start+si->size)) &&
	     (mapped_end > (si->start+si->size))) {
	    UInt newsz = mapped_end - si->start;
	    if (newsz > si->size) {
	       Segment *seg;

	       if (0)
		  VG_(printf)("extending mapping %p..%p %d -> ..%p %d\n", 
			      si->start, si->start+si->size, si->size,
			      si->start+newsz, newsz);

	       for(seg = VG_(find_segment_containing)(si->start);
		   seg != NULL && VG_(seg_overlaps)(seg, si->start, si->size); 
		   seg = VG_(next_segment)(seg)) {
		  if (seg->symtab == si)
		     continue;

		  if (seg->symtab != NULL)
		     VG_(seginfo_decref)(seg->symtab, seg->addr);

		  VG_(symtab_incref)(si);
		  seg->symtab = si;
		  
		  if (0)
		     VG_(printf)("adding symtab %p (%p-%p) to segment %p (%p-%p)\n",
				 si, si->start, si->start+newsz,
				 seg, seg->addr, seg->addr+seg->len);
	       }
	       
	       si->size = newsz;
	    }
	 }
#endif

      }
   }

   TRACE_SYMTAB("shoff = %d,  shnum = %d,  size = %d,  n_vg_oimage = %d\n",
                ehdr->e_shoff, ehdr->e_shnum, sizeof(ElfXX_Shdr), n_oimage );

   if (ehdr->e_shoff + ehdr->e_shnum*sizeof(ElfXX_Shdr) > n_oimage) {
      VG_(symerr)("ELF section header is beyond image end?!");
      goto out;
   }

   shdr = (ElfXX_Shdr*)(oimage + ehdr->e_shoff);
   sh_strtab = (UChar*)(oimage + shdr[ehdr->e_shstrndx].sh_offset);

   /* Find interesting sections, read the symbol table(s), read any debug
      information */
   {
      /* Pointers to start of sections */
      UChar*     o_strtab     = NULL; /* .strtab */
      ElfXX_Sym* o_symtab     = NULL; /* .symtab */
      UChar*     o_dynstr     = NULL; /* .dynstr */
      ElfXX_Sym* o_dynsym     = NULL; /* .dynsym */
      Char*      debuglink    = NULL; /* .gnu_debuglink */
      UChar*     stab         = NULL; /* .stab         (stabs)  */
      UChar*     stabstr      = NULL; /* .stabstr      (stabs)  */
      UChar*     debug_line   = NULL; /* .debug_line   (dwarf2) */
      UChar*     dwarf1d      = NULL; /* .debug        (dwarf1) */
      UChar*     dwarf1l      = NULL; /* .line         (dwarf1) */
      UChar*     ehframe      = NULL; /* .eh_frame     (dwarf2) */

      /* Section sizes, in bytes */
      UInt       o_strtab_sz     = 0;
      UInt       o_symtab_sz     = 0;
      UInt       o_dynstr_sz     = 0;
      UInt       o_dynsym_sz     = 0;
      UInt       debuglink_sz    = 0;
      UInt       stab_sz         = 0;
      UInt       stabstr_sz      = 0;
      UInt       debug_line_sz   = 0;
      UInt       dwarf1d_sz      = 0;
      UInt       dwarf1l_sz      = 0;
      UInt       ehframe_sz      = 0;

      /* Section virtual addresses */
      Addr       dummy_addr      = 0;
      Addr       ehframe_addr    = 0;

      Bool       has_debuginfo = False;

      /* Find all interesting sections */
      for (i = 0; i < ehdr->e_shnum; i++) {
#        define FIND(sec_name, sec_data, sec_size, sec_addr, in_exec, type) \
         if (0 == VG_(strcmp)(sec_name, sh_strtab + shdr[i].sh_name)) { \
            if (0 != sec_data) \
               VG_(core_panic)("repeated section!\n"); \
            if (in_exec) \
               sec_data = (type)(si->offset + shdr[i].sh_addr); \
            else \
               sec_data = (type)(oimage + shdr[i].sh_offset); \
            sec_size = shdr[i].sh_size; \
            sec_addr = si->offset + shdr[i].sh_addr; \
            TRACE_SYMTAB( "%18s: %p .. %p\n", \
                          sec_name, sec_data, sec_data + sec_size - 1); \
            if ( shdr[i].sh_offset + sec_size > n_oimage ) { \
               VG_(symerr)("   section beyond image end?!"); \
               goto out; \
            } \
         }

         /* Nb: must find where .got and .plt sections will be in the
          * executable image, not in the object image transiently loaded. */
              FIND(".dynsym",       o_dynsym,     o_dynsym_sz,   dummy_addr,   0, ElfXX_Sym*)
         else FIND(".dynstr",       o_dynstr,     o_dynstr_sz,   dummy_addr,   0, UChar*)
         else FIND(".symtab",       o_symtab,     o_symtab_sz,   dummy_addr,   0, ElfXX_Sym*)
         else FIND(".strtab",       o_strtab,     o_strtab_sz,   dummy_addr,   0, UChar*)

         else FIND(".gnu_debuglink", debuglink,   debuglink_sz,  dummy_addr,   0, Char*)

         else FIND(".stab",         stab,         stab_sz,       dummy_addr,   0, UChar*)
         else FIND(".stabstr",      stabstr,      stabstr_sz,    dummy_addr,   0, UChar*)
         else FIND(".debug_line",   debug_line,   debug_line_sz, dummy_addr,   0, UChar*)
         else FIND(".debug",        dwarf1d,      dwarf1d_sz,    dummy_addr,   0, UChar*)
         else FIND(".line",         dwarf1l,      dwarf1l_sz,    dummy_addr,   0, UChar*)
         else FIND(".eh_frame",     ehframe,      ehframe_sz,    ehframe_addr, 0, UChar*)

         else FIND(".got",         si->got_start, si->got_size,  dummy_addr,   1, Addr)
         else FIND(".plt",         si->plt_start, si->plt_size,  dummy_addr,   1, Addr)

#        undef FIND
      }
         
      /* Check some sizes */
      vg_assert((o_dynsym_sz % sizeof(ElfXX_Sym)) == 0);
      vg_assert((o_symtab_sz % sizeof(ElfXX_Sym)) == 0);

      /* Did we find a debuglink section? */
      if (debuglink != NULL) {
         UInt crc_offset = VG_ROUNDUP(VG_(strlen)(debuglink)+1, 4);
         UInt crc;

         vg_assert(crc_offset + sizeof(UInt) <= debuglink_sz);

         /* Extract the CRC from the debuglink section */
         crc = *(UInt *)(debuglink + crc_offset);

         /* See if we can find a matching debug file */
         if ((dimage = find_debug_file(si->filename, debuglink, crc, &n_dimage)) != 0) {
            ehdr = (ElfXX_Ehdr*)dimage;

            if (n_dimage >= sizeof(ElfXX_Ehdr) && VG_(is_object_file)(ehdr))
            {
               shdr = (ElfXX_Shdr*)(dimage + ehdr->e_shoff);
               sh_strtab = (UChar*)(dimage + shdr[ehdr->e_shstrndx].sh_offset);

               /* Find all interesting sections */
               for (i = 0; i < ehdr->e_shnum; i++) {
#                 define FIND(sec_name, sec_data, sec_size, type) \
                  if (0 == VG_(strcmp)(sec_name, sh_strtab + shdr[i].sh_name)) { \
                     if (0 != sec_data) \
                        VG_(core_panic)("repeated section!\n"); \
                     sec_data = (type)(dimage + shdr[i].sh_offset); \
                     sec_size = shdr[i].sh_size; \
                     TRACE_SYMTAB( "%18s: %p .. %p\n", \
                                   sec_name, sec_data, sec_data + sec_size - 1); \
                     if ( shdr[i].sh_offset + sec_size > n_dimage ) { \
                        VG_(symerr)("   section beyond image end?!"); \
                        goto out; \
                     } \
                  }

                  /* Nb: must find where .got and .plt sections will be in the
                   * executable image, not in the object image transiently loaded. */
                       FIND(".stab",         stab,         stab_sz,       UChar*)
                  else FIND(".stabstr",      stabstr,      stabstr_sz,    UChar*)
                  else FIND(".debug_line",   debug_line,   debug_line_sz, UChar*)
                  else FIND(".debug",        dwarf1d,      dwarf1d_sz,    UChar*)
                  else FIND(".line",         dwarf1l,      dwarf1l_sz,    UChar*)

#                 undef FIND
               }
            }
         }
      }

      /* Read symbols */
      read_symtab(si, "symbol table", False,
                  o_symtab, o_symtab_sz,
                  o_strtab, o_strtab_sz);

      read_symtab(si, "dynamic symbol table", True,
                  o_dynsym, o_dynsym_sz,
                  o_dynstr, o_dynstr_sz);

      /* Read .eh_frame (call-frame-info) if any */
      if (ehframe) {
         VG_(read_callframe_info_dwarf2) ( si, ehframe, ehframe_sz, ehframe_addr );
      }

      /* Read the stabs and/or dwarf2 debug information, if any.  It
         appears reading stabs stuff on amd64-linux doesn't work, so
         we ignore it. */
#     if !defined(VGP_amd64_linux)
      if (stab != NULL && stabstr != NULL) {
         has_debuginfo = True;
         VG_(read_debuginfo_stabs) ( si, stab, stab_sz, 
                                         stabstr, stabstr_sz );
      }
#     endif
      if (debug_line) {
         has_debuginfo = True;
         VG_(read_debuginfo_dwarf2) ( si, debug_line, debug_line_sz );
      }
      if (dwarf1d && dwarf1l) {
         has_debuginfo = True;
         VG_(read_debuginfo_dwarf1) ( si, dwarf1d, dwarf1d_sz, 
                                          dwarf1l, dwarf1l_sz );
      } 
      if (!has_debuginfo) {
         VG_(symerr)("   object doesn't have any line number info");
         goto out;
      }
   }
   res = True;

  out: {
   Int m_res;
   /* Last, but not least, heave the image(s) back overboard. */
   if (dimage) {
      m_res = VG_(munmap) ( (void*)dimage, n_dimage );
      vg_assert(0 == m_res);
   }
   m_res = VG_(munmap) ( (void*)oimage, n_oimage );
   vg_assert(0 == m_res);
   return res;
  } 
}

/*------------------------------------------------------------*/
/*--- Main entry point for symbols table reading.          ---*/
/*------------------------------------------------------------*/

/* The root structure for the entire symbol table system.  It is a
   linked list of SegInfos.  Note that this entire mechanism assumes
   that what we read from /proc/self/maps doesn't contain overlapping
   address ranges, and as a result the SegInfos in this list describe
   disjoint address ranges. 
*/
SegInfo *VG_(read_seg_symbols) ( Segment *seg )
{
   SegInfo* si;

   vg_assert(seg->seginfo == NULL);

   VGP_PUSHCC(VgpReadSyms);

   /* Get the record initialised right. */
   si = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(SegInfo));

   VG_(memset)(si, 0, sizeof(*si));
   si->start    = seg->addr;
   si->size     = seg->len;
   si->foffset  = seg->offset;
   si->filename = VG_(arena_strdup)(VG_AR_SYMTAB, seg->filename);

   si->ref = 1;

   si->symtab = NULL;
   si->symtab_size = si->symtab_used = 0;
   si->loctab = NULL;
   si->loctab_size = si->loctab_used = 0;
   si->strchunks = NULL;
   si->scopetab = NULL;
   si->scopetab_size = si->scopetab_used = 0;
   si->cfisi = NULL;
   si->cfisi_size = si->cfisi_used = 0;
   si->cfisi_minaddr = si->cfisi_maxaddr = 0;

   si->seg = seg;

   si->stab_typetab = NULL;

   si->plt_start  = si->plt_size  = 0;
   si->got_start  = si->got_size  = 0;
   si->data_start = si->data_size = 0;
   si->bss_start  = si->bss_size  = 0;

   /* And actually fill it up. */
   if (!read_lib_symbols ( si ) && 0) {
      /* XXX this interacts badly with the prevN optimization in
         addStr().  Since this frees the si, the si pointer value can
         be recycled, which confuses the curr_si == si test.  For now,
         this code is disabled, and everything is included in the
         segment list, even if it is a bad ELF file.  Ironically,
         running this under valgrind itself hides the problem, because
         it doesn't recycle pointers... */
      freeSegInfo( si );
   } else {
      si->next = segInfo;
      segInfo = si;

      canonicaliseSymtab ( si );
      canonicaliseLoctab ( si );
      canonicaliseScopetab ( si );
      canonicaliseCfiSI ( si );

      /* do redirects */
      VG_(resolve_seg_redirs)( si );
   }
   VGP_POPCC(VgpReadSyms);

   return si;
}


/* When an munmap() call happens, check to see whether it corresponds
   to a segment for a .so, and if so discard the relevant SegInfo.
   This might not be a very clever idea from the point of view of
   accuracy of error messages, but we need to do it in order to
   maintain the no-overlapping invariant.
*/
static void unload_symbols ( Addr start, SizeT length )
{
   SegInfo *prev, *curr;

   prev = NULL;
   curr = segInfo;
   while (True) {
      if (curr == NULL) break;
      if (start == curr->start) break;
      prev = curr;
      curr = curr->next;
   }
   if (curr == NULL) {
      VGP_POPCC(VgpReadSyms);
      return;
   }

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, 
                   "discard syms at %p-%p in %s due to munmap()", 
                   start, start+length, curr->filename ? curr->filename : (Char *)"???");

   vg_assert(prev == NULL || prev->next == curr);

   if (prev == NULL) {
      segInfo = curr->next;
   } else {
      prev->next = curr->next;
   }

   freeSegInfo(curr);
   return;
}

void VG_(seginfo_decref)(SegInfo *si, Addr start)
{
   vg_assert(si->ref >= 1);
   if (--si->ref == 0)
      unload_symbols(si->start, si->size);
}

void VG_(seginfo_incref)(SegInfo *si)
{
   vg_assert(si->ref > 0);
   si->ref++;
}

/*------------------------------------------------------------*/
/*--- Use of symbol table & location info to create        ---*/
/*--- plausible-looking stack dumps.                       ---*/
/*------------------------------------------------------------*/

/* Find a symbol-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

static Int search_one_symtab ( SegInfo* si, Addr ptr,
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


/* SLOW (Linear search).  Try and map a symbol name to an address.
   Since this is searching in the direction opposite to which the
   table is designed we have no option but to do a complete linear
   scan of the table.  Returns NULL if not found. */

Addr VG_(reverse_search_one_symtab) ( const SegInfo* si, const Char* name )
{
   UInt i;
   for (i = 0; i < si->symtab_used; i++) {
      if (0) 
         VG_(printf)("%p %s\n",  si->symtab[i].addr, si->symtab[i].name);
      if (0 == VG_(strcmp)(name, si->symtab[i].name))
         return si->symtab[i].addr;
   }
   return (Addr)NULL;
}


/* Search all symtabs that we know about to locate ptr.  If found, set
   *psi to the relevant SegInfo, and *symno to the symtab entry number
   within that.  If not found, *psi is set to NULL.  */

static void search_all_symtabs ( Addr ptr, /*OUT*/SegInfo** psi, 
                                           /*OUT*/Int* symno,
                                 Bool match_anywhere_in_fun )
{
   Int      sno;
   SegInfo* si;
   Segment *s;

   VGP_PUSHCC(VgpSearchSyms);

   s = VG_(find_segment)(ptr);

   if (s == NULL || s->seginfo == NULL)
      goto not_found;
   
   si = s->seginfo;

   sno = search_one_symtab ( si, ptr, match_anywhere_in_fun );
   if (sno == -1) goto not_found;
   
   *symno = sno;
   *psi = si;
   VGP_POPCC(VgpSearchSyms);
   return;

  not_found:
   *psi = NULL;
   VGP_POPCC(VgpSearchSyms);
}


/* Find a location-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

static Int search_one_loctab ( SegInfo* si, Addr ptr )
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


/* Search all loctabs that we know about to locate ptr.  If found, set
   *psi to the relevant SegInfo, and *locno to the loctab entry number
   within that.  If not found, *psi is set to NULL.
*/
static void search_all_loctabs ( Addr ptr, /*OUT*/SegInfo** psi,
                                           /*OUT*/Int* locno )
{
   Int      lno;
   SegInfo* si;

   VGP_PUSHCC(VgpSearchSyms);

   for (si = segInfo; si != NULL; si = si->next) {
      if (si->start <= ptr && ptr < si->start+si->size) {
         lno = search_one_loctab ( si, ptr );
         if (lno == -1) goto not_found;
         *locno = lno;
         *psi = si;
         VGP_POPCC(VgpSearchSyms);
         return;
      }
   }
  not_found:
   *psi = NULL;
   VGP_POPCC(VgpSearchSyms);
}


/* Find a scope-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

static Int search_one_scopetab ( SegInfo* si, Addr ptr )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid, 
        lo = 0, 
        hi = si->scopetab_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = si->scopetab[mid].addr;
      a_mid_hi = ((Addr)si->scopetab[mid].addr) + si->scopetab[mid].size - 1;

      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      return mid;
   }
}


/* Search all scopetabs that we know about to locate ptr.  If found, set
   *psi to the relevant SegInfo, and *locno to the scopetab entry number
   within that.  If not found, *psi is set to NULL.
*/
static void search_all_scopetabs ( Addr ptr,
				   /*OUT*/SegInfo** psi,
				   /*OUT*/Int* scopeno )
{
   Int      scno;
   SegInfo* si;

   VGP_PUSHCC(VgpSearchSyms);

   for (si = segInfo; si != NULL; si = si->next) {
      if (si->start <= ptr && ptr < si->start+si->size) {
         scno = search_one_scopetab ( si, ptr );
         if (scno == -1) goto not_found;
         *scopeno = scno;
         *psi = si;
         VGP_POPCC(VgpSearchSyms);
         return;
      }
   }
  not_found:
   *psi = NULL;
   VGP_POPCC(VgpSearchSyms);
}


/* Find a CFI-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

static Int search_one_cfitab ( SegInfo* si, Addr ptr )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid, size, 
        lo = 0, 
        hi = si->cfisi_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = si->cfisi[mid].base;
      size     = si->cfisi[mid].len;
      a_mid_hi = a_mid_lo + size - 1;
      vg_assert(a_mid_hi >= a_mid_lo);
      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      return mid;
   }
}


/* The whole point of this whole big deal: map a code address to a
   plausible symbol name.  Returns False if no idea; otherwise True.
   Caller supplies buf and nbuf.  If demangle is False, don't do
   demangling, regardless of VG_(clo_demangle) -- probably because the
   call has come from VG_(get_fnname_nodemangle)(). */
static
Bool get_fnname ( Bool demangle, Addr a, Char* buf, Int nbuf,
                  Bool match_anywhere_in_fun, Bool show_offset)
{
   SegInfo* si;
   Int      sno;
   Int      offset;

   search_all_symtabs ( a, &si, &sno, match_anywhere_in_fun );
   if (si == NULL) 
      return False;
   if (demangle) {
      VG_(demangle) ( si->symtab[sno].name, buf, nbuf );
   } else {
      VG_(strncpy_safely) ( buf, si->symtab[sno].name, nbuf );
   }

   offset = a - si->symtab[sno].addr;
   if (show_offset && offset != 0) {
      Char     buf2[12];
      Char*    symend = buf + VG_(strlen)(buf);
      Char*    end = buf + nbuf;
      Int      len;

      len = VG_(sprintf)(buf2, "%c%d",
			 offset < 0 ? '-' : '+',
			 offset < 0 ? -offset : offset);
      vg_assert(len < (Int)sizeof(buf2));

      if (len < (end - symend)) {
	 Char *cp = buf2;
	 VG_(memcpy)(symend, cp, len+1);
      }
   }

   return True;
}

/* This is available to tools... always demangle C++ names,
   match anywhere in function, but don't show offsets. */
Bool VG_(get_fnname) ( Addr a, Char* buf, Int nbuf )
{
   return get_fnname ( /*demangle*/True, a, buf, nbuf,
                       /*match_anywhere_in_fun*/True, 
                       /*show offset?*/False );
}

/* This is available to tools... always demangle C++ names,
   match anywhere in function, and show offset if nonzero. */
Bool VG_(get_fnname_w_offset) ( Addr a, Char* buf, Int nbuf )
{
   return get_fnname ( /*demangle*/True, a, buf, nbuf,
                       /*match_anywhere_in_fun*/True, 
                       /*show offset?*/True );
}

/* This is available to tools... always demangle C++ names,
   only succeed if 'a' matches first instruction of function,
   and don't show offsets. */
Bool VG_(get_fnname_if_entry) ( Addr a, Char* buf, Int nbuf )
{
   return get_fnname ( /*demangle*/True, a, buf, nbuf,
                       /*match_anywhere_in_fun*/False, 
                       /*show offset?*/False );
}

/* This is only available to core... don't demangle C++ names,
   match anywhere in function, and don't show offsets. */
Bool VG_(get_fnname_nodemangle) ( Addr a, Char* buf, Int nbuf )
{
   return get_fnname ( /*demangle*/False, a, buf, nbuf,
                       /*match_anywhere_in_fun*/True, 
                       /*show offset?*/False );
}

/* Map a code address to the name of a shared object file or the executable.
   Returns False if no idea; otherwise True.  Doesn't require debug info.
   Caller supplies buf and nbuf. */
Bool VG_(get_objname) ( Addr a, Char* buf, Int nbuf )
{
   SegInfo* si;

   for (si = segInfo; si != NULL; si = si->next) {
      if (si->start <= a && a < si->start+si->size) {
         VG_(strncpy_safely)(buf, si->filename, nbuf);
         return True;
      }
   }
   return False;
}

/* Map a code address to its SegInfo.  Returns NULL if not found.  Doesn't
   require debug info. */
SegInfo* VG_(get_obj) ( Addr a )
{
   SegInfo* si;

   for (si = segInfo; si != NULL; si = si->next) {
      if (si->start <= a && a < si->start+si->size) {
         return si;
      }
   }
   return NULL;
}


/* Map a code address to a filename.  Returns True if successful.  */
Bool VG_(get_filename)( Addr a, Char* filename, Int n_filename )
{
   SegInfo* si;
   Int      locno;
   search_all_loctabs ( a, &si, &locno );
   if (si == NULL) 
      return False;
   VG_(strncpy_safely)(filename, si->loctab[locno].filename, n_filename);
   return True;
}

/* Map a code address to a line number.  Returns True if successful. */
Bool VG_(get_linenum)( Addr a, UInt* lineno )
{
   SegInfo* si;
   Int      locno;
   search_all_loctabs ( a, &si, &locno );
   if (si == NULL) 
      return False;
   *lineno = si->loctab[locno].lineno;

   return True;
}

/* Map a code address to a (filename, line number) pair.  
   Returns True if successful.
*/
Bool VG_(get_filename_linenum)( Addr a, 
                                Char* filename, Int n_filename, 
                                UInt* lineno )
{
   SegInfo* si;
   Int      locno;
   search_all_loctabs ( a, &si, &locno );
   if (si == NULL) 
      return False;
   VG_(strncpy_safely)(filename, si->loctab[locno].filename, n_filename);
   *lineno = si->loctab[locno].lineno;

   return True;
}

#ifndef TEST

// Note that R_STACK_PTR and R_FRAME_PTR are used again further below,
// which is why they get a named constant.
static Addr regaddr_from_tst(Int regno, ThreadArchState *arch)
{
#if defined(VGA_x86)
/* This is the Intel register encoding -- integer regs. */
#  define R_STACK_PTR   4
#  define R_FRAME_PTR   5
   switch (regno) {
   case 0:           return (Addr) & arch->vex.guest_EAX;
   case 1:           return (Addr) & arch->vex.guest_ECX;
   case 2:           return (Addr) & arch->vex.guest_EDX;
   case 3:           return (Addr) & arch->vex.guest_EBX;
   case R_STACK_PTR: return (Addr) & arch->vex.guest_ESP;
   case R_FRAME_PTR: return (Addr) & arch->vex.guest_EBP;
   case 6:           return (Addr) & arch->vex.guest_ESI;
   case 7:           return (Addr) & arch->vex.guest_EDI;
   default:          return 0;
   }
#elif defined(VGA_amd64)
/* This is the Intel register encoding -- integer regs. */
#  define R_STACK_PTR   7
#  define R_FRAME_PTR   6
   switch (regno) {
   case 0:           return (Addr) & arch->vex.guest_RAX;
   case 1:           return (Addr) & arch->vex.guest_RDX;
   case 2:           return (Addr) & arch->vex.guest_RCX;
   case 3:           return (Addr) & arch->vex.guest_RBX;
   case 4:           return (Addr) & arch->vex.guest_RSI;
   case 5:           return (Addr) & arch->vex.guest_RDI;
   case R_FRAME_PTR: return (Addr) & arch->vex.guest_RBP;
   case R_STACK_PTR: return (Addr) & arch->vex.guest_RSP;
   case 8:           return (Addr) & arch->vex.guest_R8;
   case 9:           return (Addr) & arch->vex.guest_R9;
   case 10:          return (Addr) & arch->vex.guest_R10;
   case 11:          return (Addr) & arch->vex.guest_R11;
   case 12:          return (Addr) & arch->vex.guest_R12;
   case 13:          return (Addr) & arch->vex.guest_R13;
   case 14:          return (Addr) & arch->vex.guest_R14;
   case 15:          return (Addr) & arch->vex.guest_R15;
   default:          return 0;
   }
#else
#  error Unknown platform
#endif
}


/* return a pointer to a register (now for 5 other impossible things
   before breakfast) */
static Addr regaddr(ThreadId tid, Int regno)
{
   Addr ret = regaddr_from_tst(regno, &VG_(threads)[tid].arch);

   if (ret == 0) {
      Char buf[100];
      VG_(describe_IP)( VG_(get_IP)(tid), buf, 100 );
      VG_(printf)("mysterious register %d used at %s\n", regno, buf);
   }

   return ret;
}

/* Get a list of all variables in scope, working out from the directly
   current one */
Variable *VG_(get_scope_variables)(ThreadId tid)
{
   static const Bool debug = False;
   Variable *list, *end;
   Addr eip;
   SegInfo *si;
   Int scopeidx;
   Scope *scope;
   Int distance;
   static const Int maxsyms = 1000;
   Int nsyms = maxsyms;

   list = end = NULL;

   eip = VG_(get_IP)(tid);
   
   search_all_scopetabs(eip, &si, &scopeidx);

   if (debug)
      VG_(printf)("eip=%p si=%p (%s; offset=%p) scopeidx=%d\n", 
		  eip, si, si ? si->filename : (Char *)"???",
		  si ? si->offset : 0x99999, scopeidx);

   if (si == NULL)
      return NULL;		/* nothing in scope (should use global scope at least) */

   if (debug) {
      ScopeRange *sr = &si->scopetab[scopeidx];
      Char file[100];
      Int line;

      if (!VG_(get_filename_linenum)(sr->addr, file, sizeof(file), &line))
	 file[0] = 0;

      VG_(printf)("found scope range %p: eip=%p (%s:%d) size=%d scope=%p\n",
		  sr, sr->addr, file, line, sr->size, sr->scope);
   }

   distance = 0;
   for(scope = si->scopetab[scopeidx].scope; scope != NULL; scope = scope->outer, distance++) {
      UInt i;

      for(i = 0; i < scope->nsyms; i++) {
	 Sym *sym = &scope->syms[i];
	 Variable *v;

	 if (nsyms-- == 0) {
	    VG_(printf)("max %d syms reached\n", maxsyms);
	    return list;
	 }
	    
	 v = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(*v));

	 v->next = NULL;
	 v->distance = distance;
	 v->type = VG_(st_basetype)(sym->type, False);
	 v->name = VG_(arena_strdup)(VG_AR_SYMTAB, sym->name);
	 v->container = NULL;
	 v->size = VG_(st_sizeof)(sym->type);

	 if (debug && 0)
	    VG_(printf)("sym->name=%s sym->kind=%d offset=%d\n", sym->name, sym->kind, sym->u.offset);
	 switch(sym->kind) {

	 case SyGlobal:
	 case SyStatic:
	    if (sym->u.addr == 0) {
	       /* XXX lookup value */
	    }
	    v->valuep = sym->u.addr;
	    break;

	 case SyReg:
	    v->valuep = regaddr(tid, sym->u.regno);
	    break;

	 case SyEBPrel:
	 case SyESPrel: {
	    Addr reg = *(Addr*)regaddr(tid, sym->kind == SyESPrel
                                            ? R_STACK_PTR : R_FRAME_PTR);
	    if (debug)
	       VG_(printf)("reg=%p+%d=%p\n", reg, sym->u.offset, reg+sym->u.offset);
	    v->valuep = reg + sym->u.offset;
	    break;
         }

	 case SyType:
	    VG_(core_panic)("unexpected typedef in scope");
	 }

	 if (v->valuep == 0) {
	    /* not interesting or useful */
	    VG_(arena_free)(VG_AR_SYMTAB, v);
	    continue;
	 }

	 /* append to end of list */
	 if (list == NULL)
	    list = end = v;
	 else {
	    end->next = v;
	    end = v;
	 }
      }  
   }

   return list;
}
#endif /* TEST */

/* Print into buf info on code address, function name and filename */
Char* VG_(describe_IP)(Addr eip, Char* buf, Int n_buf)
{
#define APPEND(str)                                         \
   { UChar* sss;                                            \
     for (sss = str; n < n_buf-1 && *sss != 0; n++,sss++)   \
        buf[n] = *sss;                                      \
     buf[n] = '\0';                                         \
   }
   UInt  lineno; 
   UChar ibuf[50];
   UInt  n = 0;
   static UChar buf_fn[VG_ERRTXT_LEN];
   static UChar buf_obj[VG_ERRTXT_LEN];
   static UChar buf_srcloc[VG_ERRTXT_LEN];
   Bool  know_fnname  = VG_(get_fnname) (eip, buf_fn,  VG_ERRTXT_LEN);
   Bool  know_objname = VG_(get_objname)(eip, buf_obj, VG_ERRTXT_LEN);
   Bool  know_srcloc  = VG_(get_filename_linenum)(eip, buf_srcloc,
                                                  VG_ERRTXT_LEN, &lineno);

   if (VG_(clo_xml)) {

      Bool   human_readable = True;
      HChar* maybe_newline  = human_readable ? "\n      " : "";
      HChar* maybe_newline2 = human_readable ? "\n    "   : "";

      /* Print in XML format, dumping in as much info as we know. */
      APPEND("<frame>");
      VG_(sprintf)(ibuf,"<ip>0x%llx</ip>", (ULong)eip);
      APPEND(maybe_newline);
      APPEND(ibuf);
      if (know_objname) {
         APPEND(maybe_newline);
         APPEND("<obj>");
         APPEND(buf_obj);
         APPEND("</obj>");
      }
      if (know_fnname) {
         APPEND(maybe_newline);
         APPEND("<fn>");
         APPEND(buf_fn);
         APPEND("</fn>");
      }
      if (know_srcloc) {
         APPEND(maybe_newline);
         APPEND("<file>");
         APPEND(buf_srcloc);
         APPEND("</file>");
         APPEND(maybe_newline);
         APPEND("<line>");
         VG_(sprintf)(ibuf,"%d",lineno);
         APPEND(ibuf);
         APPEND("</line>");
      }
      APPEND(maybe_newline2);
      APPEND("</frame>");

   } else {

      /* Print for humans to read */
      VG_(sprintf)(ibuf,"0x%llx: ", (ULong)eip);
      APPEND(ibuf);
      if (know_fnname) { 
         APPEND(buf_fn);
         if (!know_srcloc && know_objname) {
            APPEND(" (in ");
            APPEND(buf_obj);
            APPEND(")");
         }
      } else if (know_objname && !know_srcloc) {
         APPEND("(within ");
         APPEND(buf_obj);
         APPEND(")");
      } else {
         APPEND("???");
      }
      if (know_srcloc) {
         APPEND(" (");
         APPEND(buf_srcloc);
         APPEND(":");
         VG_(sprintf)(ibuf,"%d",lineno);
         APPEND(ibuf);
         APPEND(")");
      }

   }
   return buf;

#undef APPEND
}

/* Returns True if OK.  If not OK, *{ip,sp,fp}P are not changed. */

Bool VG_(use_CFI_info) ( /*MOD*/Addr* ipP,
                         /*MOD*/Addr* spP,
                         /*MOD*/Addr* fpP,
                         Addr min_accessible,
                         Addr max_accessible )
{
   Int      i;
   SegInfo* si;
   CfiSI*   cfisi = NULL;
   Addr     cfa, ipHere, spHere, fpHere, ipPrev, spPrev, fpPrev;


   if (0) VG_(printf)("search for %p\n", *ipP);

   for (si = segInfo; si != NULL; si = si->next) {
      /* Use the per-SegInfo summary address ranges to skip
	 inapplicable SegInfos quickly. */
      if (si->cfisi_used == 0)
         continue;
      if (*ipP < si->cfisi_minaddr || *ipP > si->cfisi_maxaddr)
         continue;

      i = search_one_cfitab( si, *ipP );
      if (i != -1) {
         vg_assert(i >= 0 && i < si->cfisi_used);
         cfisi = &si->cfisi[i];
         break;
      }
   }

   if (cfisi == NULL)
      return False;

   if (0) {
      VG_(printf)("found cfisi: "); 
      VG_(ppCfiSI)(cfisi);
   }

   ipPrev = spPrev = fpPrev = 0;

   ipHere = *ipP;
   spHere = *spP;
   fpHere = *fpP;

   cfa = cfisi->cfa_off + (cfisi->cfa_sprel ? spHere : fpHere);

#  define COMPUTE(_prev, _here, _how, _off)             \
      do {                                              \
         switch (_how) {                                \
            case CFIR_UNKNOWN:                          \
               return False;                            \
            case CFIR_SAME:                             \
               _prev = _here; break;                    \
            case CFIR_MEMCFAREL: {                      \
               Addr a = cfa + (Word)_off;               \
               if (a < min_accessible                   \
                   || a+sizeof(Addr) > max_accessible)  \
                  return False;                         \
               _prev = *(Addr*)a;                       \
               break;                                   \
            }                                           \
            case CFIR_CFAREL:                           \
               _prev = cfa + (Word)_off;                \
               break;                                   \
         }                                              \
      } while (0)

   COMPUTE(ipPrev, ipHere, cfisi->ra_how, cfisi->ra_off);
   COMPUTE(spPrev, spHere, cfisi->sp_how, cfisi->sp_off);
   COMPUTE(fpPrev, fpHere, cfisi->fp_how, cfisi->fp_off);

#  undef COMPUTE

   *ipP = ipPrev;
   *spP = spPrev;
   *fpP = fpPrev;
   return True;
}


/*------------------------------------------------------------*/
/*--- SegInfo accessor functions                           ---*/
/*------------------------------------------------------------*/

const SegInfo* VG_(next_seginfo)(const SegInfo* si)
{
   if (si == NULL)
      return segInfo;
   return si->next;
}

Addr VG_(seg_start)(const SegInfo* si)
{
   return si->start;
}

SizeT VG_(seg_size)(const SegInfo* si)
{
   return si->size;
}

const UChar* VG_(seg_filename)(const SegInfo* si)
{
   return si->filename;
}

ULong VG_(seg_sym_offset)(const SegInfo* si)
{
   return si->offset;
}

VgSectKind VG_(seg_sect_kind)(Addr a)
{
   SegInfo* si;
   VgSectKind ret = Vg_SectUnknown;

   for(si = segInfo; si != NULL; si = si->next) {
      if (a >= si->start && a < (si->start + si->size)) {
	 if (0)
	    VG_(printf)("addr=%p si=%p %s got=%p %d  plt=%p %d data=%p %d bss=%p %d\n",
			a, si, si->filename, 
			si->got_start, si->got_size,
			si->plt_start, si->plt_size,
			si->data_start, si->data_size,
			si->bss_start, si->bss_size);
	 ret = Vg_SectText;

	 if (a >= si->data_start && a < (si->data_start + si->data_size))
	    ret = Vg_SectData;
	 else if (a >= si->bss_start && a < (si->bss_start + si->bss_size))
	    ret = Vg_SectBSS;
	 else if (a >= si->plt_start && a < (si->plt_start + si->plt_size))
	    ret = Vg_SectPLT;
	 else if (a >= si->got_start && a < (si->got_start + si->got_size))
	    ret = Vg_SectGOT;
      }
   }

   return ret;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
