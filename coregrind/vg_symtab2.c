/*--------------------------------------------------------------------*/
/*--- Management of symbols and debugging information.             ---*/
/*---                                                 vg_symtab2.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
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

#include "vg_include.h"
#include "vg_symtypes.h"
#include "vg_symtab2.h"

#include <elf.h>          /* ELF defns                      */


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

   /* Avoid gratuitous duplication:  if we saw `str' within the last NN,
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
       VG_(message)(Vg_DebugMsg, 
                    "warning: line info addresses out of order "
                    "at entry %d: 0x%x 0x%x", entry, this, next);
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

   /* Ignore zero-sized scopes */
   if (this == next) {
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

/*------------------------------------------------------------*/
/*--- Helpers                                              ---*/
/*------------------------------------------------------------*/

/* Non-fatal -- use vg_panic if terminal. */
void VG_(symerr) ( Char* msg )
{
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg,"%s", msg );
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
   
   return a->addr - b->addr;
}

/* Two symbols have the same address.  Which name do we prefer?

   In general we prefer the longer name, but if the choice is between
   __libc_X and X, then choose X (similarly with __GI__ and __
   prefixes).
 */
static RiSym *prefersym(RiSym *a, RiSym *b)
{
   Int pfx;
   Int lena, lenb;
   Int i;
   static const struct {
      const Char *prefix;
      Int len;
   } prefixes[] = {
#define PFX(x)	{ x, sizeof(x)-1 }
      /* order from longest to shortest */
      PFX("__GI___libc_"),
      PFX("__GI___"),
      PFX("__libc_"),
      PFX("__GI__"),
      PFX("__GI_"),
      PFX("__"),
#undef PFX
   };

   lena = VG_(strlen)(a->name);
   lenb = VG_(strlen)(b->name);

   /* rearrange so that a is the long one */
   if (lena < lenb) {
      RiSym *t;
      Int lt;

      t = a;
      a = b;
      b = t;

      lt = lena;
      lena = lenb;
      lenb = lt;
   }

   for(i = pfx = 0; i < sizeof(prefixes)/sizeof(*prefixes); i++) {
      Int pfxlen = prefixes[i].len;

      if (pfxlen < lena &&
	  VG_(memcmp)(a->name, prefixes[i].prefix, pfxlen) == 0) {
	 pfx = pfxlen;
	 break;
      }
   }

   if (pfx != 0 && VG_(strcmp)(a->name + pfx, b->name) == 0)
      return b;

   return a;
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
   
   return a->addr - b->addr;
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

   return a->addr - b->addr;
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
            are unsigned 16s. */
         Int new_size = si->loctab[i+1].addr - si->loctab[i].addr;
         if (new_size < 0) {
            si->loctab[i].size = 0;
         } else
         if (new_size >= 65536) {
           si->loctab[i].size = 65535;
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


/*------------------------------------------------------------*/
/*--- Read info from a .so/exe file.                       ---*/
/*------------------------------------------------------------*/

Bool VG_(is_object_file)(const void *buf)
{
   {
      Elf32_Ehdr *ehdr = (Elf32_Ehdr *)buf;
      Int ok = 1;

      ok &= (ehdr->e_ident[EI_MAG0] == 0x7F
             && ehdr->e_ident[EI_MAG1] == 'E'
             && ehdr->e_ident[EI_MAG2] == 'L'
             && ehdr->e_ident[EI_MAG3] == 'F');
      ok &= (ehdr->e_ident[EI_CLASS] == ELFCLASS32
             && ehdr->e_ident[EI_DATA] == ELFDATA2LSB
             && ehdr->e_ident[EI_VERSION] == EV_CURRENT);
      ok &= (ehdr->e_type == ET_EXEC || ehdr->e_type == ET_DYN);
      ok &= (ehdr->e_machine == EM_386);
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

/* Read a symbol table (normal or dynamic) */
static
void read_symtab( SegInfo* si, Char* tab_name,
                  Elf32_Sym* o_symtab, UInt o_symtab_sz,
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
                o_symtab_sz/sizeof(Elf32_Sym) );

   /* Perhaps should start at i = 1; ELF docs suggest that entry
      0 always denotes `unknown symbol'. */
   for (i = 1; i < (Int)(o_symtab_sz/sizeof(Elf32_Sym)); i++) {
      Elf32_Sym* sym = & o_symtab[i];
#     if 1
      sym_addr = si->offset + (UInt)sym->st_value;

      if (VG_(clo_trace_symtab)) {
         VG_(printf)("raw symbol [%d]: ", i);
         switch (ELF32_ST_BIND(sym->st_info)) {
         case STB_LOCAL:  VG_(printf)("LOC "); break;
         case STB_GLOBAL: VG_(printf)("GLO "); break;
         case STB_WEAK:   VG_(printf)("WEA "); break;
         case STB_LOPROC: VG_(printf)("lop "); break;
         case STB_HIPROC: VG_(printf)("hip "); break;
         default:         VG_(printf)("??? "); break;
         }
         switch (ELF32_ST_TYPE(sym->st_info)) {
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

      /* Figure out if we're interested in the symbol.
         Firstly, is it of the right flavour?  */
      if ( ! ( (ELF32_ST_BIND(sym->st_info) == STB_GLOBAL ||
                ELF32_ST_BIND(sym->st_info) == STB_LOCAL ||
                ELF32_ST_BIND(sym->st_info) == STB_WEAK)
             &&
               (ELF32_ST_TYPE(sym->st_info) == STT_FUNC ||
                (VG_(needs).data_syms 
                 && ELF32_ST_TYPE(sym->st_info) == STT_OBJECT))
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
      if (sym->st_name == (Elf32_Word)NULL
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


/* Read the symbols from the object/exe specified by the SegInfo into
   the tables within the supplied SegInfo.  */
static
Bool vg_read_lib_symbols ( SegInfo* si )
{
   Bool          res;
   Elf32_Ehdr*   ehdr;       /* The ELF header                          */
   Elf32_Shdr*   shdr;       /* The section table                       */
   UChar*        sh_strtab;  /* The section table's string table        */
   Int           fd;
   Int           i;
   Bool          ok;
   Addr          oimage;
   UInt          n_oimage;
   struct vki_stat stat_buf;

   oimage = (Addr)NULL;
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg, "Reading syms from %s (%p)", si->filename, si->start );

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
                             VKI_PROT_READ, VKI_MAP_PRIVATE|VKI_MAP_NOSYMS, fd, 0 );

   VG_(close)(fd);

   if (oimage == ((Addr)(-1))) {
      VG_(message)(Vg_UserMsg,
                   "mmap failed on %s", si->filename );
      return False;
   }

   /* Ok, the object image is safely in oimage[0 .. n_oimage-1]. 
      Now verify that it is a valid ELF .so or executable image.
   */
   res = False;
   ok = (n_oimage >= sizeof(Elf32_Ehdr));
   ehdr = (Elf32_Ehdr*)oimage;

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
   if (ehdr->e_phoff + ehdr->e_phnum*sizeof(Elf32_Phdr) > n_oimage) {
      VG_(symerr)("ELF program header is beyond image end?!");
      goto out;
   }
   {
      Bool offset_set = False;
      Elf32_Addr prev_addr = 0;
      Addr baseaddr = 0;

      si->offset = 0;

      for (i = 0; i < ehdr->e_phnum; i++) {
	 Elf32_Phdr *o_phdr;
	 Elf32_Addr mapped, mapped_end;

	 o_phdr = &((Elf32_Phdr *)(oimage + ehdr->e_phoff))[i];

	 if (o_phdr->p_type == PT_DYNAMIC && si->soname == NULL) {
	    const Elf32_Dyn *dyn = (const Elf32_Dyn *)(oimage + o_phdr->p_offset);
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

	 mapped = mapped & ~(VKI_BYTES_PER_PAGE-1);
	 mapped_end = (mapped_end + VKI_BYTES_PER_PAGE - 1) & ~(VKI_BYTES_PER_PAGE-1);

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

	       for(seg = VG_(find_segment)(si->start);
		   seg != NULL && VG_(seg_overlaps)(seg, si->start, si->size); 
		   seg = VG_(next_segment)(seg)) {
		  if (seg->symtab == si)
		     continue;

		  if (seg->symtab != NULL)
		     VG_(symtab_decref)(seg->symtab, seg->addr, seg->len);

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
      }
   }

   TRACE_SYMTAB("shoff = %d,  shnum = %d,  size = %d,  n_vg_oimage = %d\n",
                ehdr->e_shoff, ehdr->e_shnum, sizeof(Elf32_Shdr), n_oimage );

   if (ehdr->e_shoff + ehdr->e_shnum*sizeof(Elf32_Shdr) > n_oimage) {
      VG_(symerr)("ELF section header is beyond image end?!");
      goto out;
   }

   shdr = (Elf32_Shdr*)(oimage + ehdr->e_shoff);
   sh_strtab = (UChar*)(oimage + shdr[ehdr->e_shstrndx].sh_offset);

   /* Find interesting sections, read the symbol table(s), read any debug
      information */
   {
      /* Pointers to start of sections */
      UChar*     o_strtab     = NULL; /* .strtab */
      Elf32_Sym* o_symtab     = NULL; /* .symtab */
      UChar*     o_dynstr     = NULL; /* .dynstr */
      Elf32_Sym* o_dynsym     = NULL; /* .dynsym */
      UChar*     stab         = NULL; /* .stab         (stabs)  */
      UChar*     stabstr      = NULL; /* .stabstr      (stabs)  */
      UChar*     debug_line   = NULL; /* .debug_line   (dwarf2) */
      UChar*     dwarf1d      = NULL; /* .debug        (dwarf1) */
      UChar*     dwarf1l      = NULL; /* .line         (dwarf1) */

      /* Section sizes, in bytes */
      UInt       o_strtab_sz     = 0;
      UInt       o_symtab_sz     = 0;
      UInt       o_dynstr_sz     = 0;
      UInt       o_dynsym_sz     = 0;
      UInt       stab_sz         = 0;
      UInt       stabstr_sz      = 0;
      UInt       debug_line_sz   = 0;
      UInt       dwarf1d_sz      = 0;
      UInt       dwarf1l_sz      = 0;

      Bool       has_debuginfo = False;

      /* Find all interesting sections */
      for (i = 0; i < ehdr->e_shnum; i++) {
#        define FIND(sec_name, sec_data, sec_size, in_exec, type) \
         if (0 == VG_(strcmp)(sec_name, sh_strtab + shdr[i].sh_name)) { \
            if (0 != sec_data) \
               VG_(core_panic)("repeated section!\n"); \
            if (in_exec) \
               sec_data = (type)(si->offset + shdr[i].sh_addr); \
            else \
               sec_data = (type)(oimage + shdr[i].sh_offset); \
            sec_size = shdr[i].sh_size; \
            TRACE_SYMTAB( "%18s: %p .. %p\n", \
                          sec_name, sec_data, sec_data + sec_size - 1); \
            if ( shdr[i].sh_offset + sec_size > n_oimage ) { \
               VG_(symerr)("   section beyond image end?!"); \
               goto out; \
            } \
         }

         /* Nb: must find where .got and .plt sections will be in the
          * executable image, not in the object image transiently loaded. */
              FIND(".dynsym",       o_dynsym,     o_dynsym_sz,   0, Elf32_Sym*)
         else FIND(".dynstr",       o_dynstr,     o_dynstr_sz,   0, UChar*)
         else FIND(".symtab",       o_symtab,     o_symtab_sz,   0, Elf32_Sym*)
         else FIND(".strtab",       o_strtab,     o_strtab_sz,   0, UChar*)

         else FIND(".stab",         stab,         stab_sz,       0, UChar*)
         else FIND(".stabstr",      stabstr,      stabstr_sz,    0, UChar*)
         else FIND(".debug_line",   debug_line,   debug_line_sz, 0, UChar*)
         else FIND(".debug",        dwarf1d,      dwarf1d_sz,    0, UChar*)
         else FIND(".line",         dwarf1l,      dwarf1l_sz,    0, UChar*)

         else FIND(".got",         si->got_start, si->got_size,  1, Addr)
         else FIND(".plt",         si->plt_start, si->plt_size,  1, Addr)

#        undef FIND
         
         /* Check some sizes */
         vg_assert((o_dynsym_sz % sizeof(Elf32_Sym)) == 0);
         vg_assert((o_symtab_sz % sizeof(Elf32_Sym)) == 0);
      }

      read_symtab(si, "symbol table", 
                  o_symtab, o_symtab_sz,
                  o_strtab, o_strtab_sz);

      read_symtab(si, "dynamic symbol table",
                  o_dynsym, o_dynsym_sz,
                  o_dynstr, o_dynstr_sz);

      /* Read the stabs and/or dwarf2 debug information, if any. */
      if (stab != NULL && stabstr != NULL) {
         has_debuginfo = True;
         VG_(read_debuginfo_stabs) ( si, stab, stab_sz, 
                                         stabstr, stabstr_sz );
      }
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
         VG_(symerr)("   object doesn't have any debug info");
         goto out;
      }
   }
   res = True;

  out:
   /* Last, but not least, heave the oimage back overboard. */
   VG_(munmap) ( (void*)oimage, n_oimage );
   return res;
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
static SegInfo* segInfo = NULL;

static void resolve_seg_redirs(SegInfo *si);

SegInfo *VG_(read_seg_symbols) ( Segment *seg )
{
   SegInfo* si;

   vg_assert(seg->symtab == NULL);

   VGP_PUSHCC(VgpReadSyms);

   /* Get the record initialised right. */
   si = VG_(arena_malloc)(VG_AR_SYMTAB, sizeof(SegInfo));

   VG_(memset)(si, 0, sizeof(*si));
   si->start    = seg->addr;
   si->size     = seg->len;
   si->foffset  = seg->offset;
   si->filename = VG_(arena_malloc)(VG_AR_SYMTAB, 1 + VG_(strlen)(seg->filename));
   VG_(strcpy)(si->filename, seg->filename);

   si->ref = 1;

   si->symtab = NULL;
   si->symtab_size = si->symtab_used = 0;
   si->loctab = NULL;
   si->loctab_size = si->loctab_used = 0;
   si->strchunks = NULL;
   si->scopetab = NULL;
   si->scopetab_size = si->scopetab_used = 0;

   si->seg = seg;

   si->stab_typetab = NULL;

   si->plt_start  = si->plt_size  = 0;
   si->got_start  = si->got_size  = 0;
   si->data_start = si->data_size = 0;
   si->bss_start  = si->bss_size  = 0;

   /* And actually fill it up. */
   if (!vg_read_lib_symbols ( si ) && 0) {
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

      /* do redirects */
      resolve_seg_redirs( si );
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
void VG_(unload_symbols) ( Addr start, UInt length )
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
      VG_(message)(Vg_UserMsg, 
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

void VG_(symtab_decref)(SegInfo *si, Addr start, UInt len)
{
   vg_assert(si->ref >= 1);
   if (--si->ref == 0)
      VG_(unload_symbols)(si->start, si->size);
}

void VG_(symtab_incref)(SegInfo *si)
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

static Addr reverse_search_one_symtab ( const SegInfo* si,
                                        const Char* name )
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

   if (s == NULL || !VG_(seg_overlaps)(s, ptr, 0) || s->symtab == NULL)
      goto not_found;
   
   si = s->symtab;

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

/* The whole point of this whole big deal: map a code address to a
   plausible symbol name.  Returns False if no idea; otherwise True.
   Caller supplies buf and nbuf.  If demangle is False, don't do
   demangling, regardless of vg_clo_demangle -- probably because the
   call has come from vg_what_fn_or_object_is_this. */
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
      VG_(strncpy_safely) 
         ( buf, si->symtab[sno].name, nbuf );
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

/* This is available to skins... always demangle C++ names,
   match anywhere in function, but don't show offsets. */
Bool VG_(get_fnname) ( Addr a, Char* buf, Int nbuf )
{
   return get_fnname ( /*demangle*/True, a, buf, nbuf,
                       /*match_anywhere_in_fun*/True, 
                       /*show offset?*/False );
}

/* This is available to skins... always demangle C++ names,
   match anywhere in function, and show offset if nonzero. */
Bool VG_(get_fnname_w_offset) ( Addr a, Char* buf, Int nbuf )
{
   return get_fnname ( /*demangle*/True, a, buf, nbuf,
                       /*match_anywhere_in_fun*/True, 
                       /*show offset?*/True );
}

/* This is available to skins... always demangle C++ names,
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
   VG_(strncpy_safely)(filename, si->loctab[locno].filename, 
                       n_filename);
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
   VG_(strncpy_safely)(filename, si->loctab[locno].filename, 
                       n_filename);
   *lineno = si->loctab[locno].lineno;

   return True;
}

#ifndef TEST

/* return a pointer to a register (now for 5 other impossible things
   before breakfast) */
static UInt *regaddr(ThreadId tid, Int regno)
{
   UInt *ret = 0;

   if (VG_(is_running_thread)(tid)) {
      Int idx;

      switch(regno) {
      case R_EAX:	idx = VGOFF_(m_eax); break;
      case R_ECX:	idx = VGOFF_(m_ecx); break;
      case R_EDX:	idx = VGOFF_(m_edx); break;
      case R_EBX:	idx = VGOFF_(m_ebx); break;
      case R_ESP:	idx = VGOFF_(m_esp); break;
      case R_EBP:	idx = VGOFF_(m_ebp); break;
      case R_ESI:	idx = VGOFF_(m_esi); break;
      case R_EDI:	idx = VGOFF_(m_edi); break;
      default:		
	 idx = -1;
	 break;
      }
      if (idx != -1)
	 ret = &VG_(baseBlock)[idx];
   } else {
      ThreadState *tst = &VG_(threads)[tid];

      switch(regno) {
      case R_EAX:	ret = &tst->m_eax; break;
      case R_ECX:	ret = &tst->m_ecx; break;
      case R_EDX:	ret = &tst->m_edx; break;
      case R_EBX:	ret = &tst->m_ebx; break;
      case R_ESP:	ret = &tst->m_esp; break;
      case R_EBP:	ret = &tst->m_ebp; break;
      case R_ESI:	ret = &tst->m_esi; break;
      case R_EDI:	ret = &tst->m_edi; break;
      default:	
	 break;
      }
   }	       

   if (ret == 0) {
      Char file[100];
      Int line;
      Addr eip = VG_(get_EIP)(tid);

      if (!VG_(get_filename_linenum)(eip, file, sizeof(file), &line))
	 file[0] = 0;
      VG_(printf)("mysterious register %d used at %p %s:%d\n",
		  regno, eip, file, line);
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

   eip = VG_(get_EIP)(tid);
   
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
	    VG_(printf)("sym->name=%s sym->kind=%d offset=%d\n", sym->name, sym->kind, sym->offset);
	 switch(sym->kind) {
	    UInt reg;

	 case SyGlobal:
	 case SyStatic:
	    if (sym->addr == 0) {
	       /* XXX lookup value */
	    }
	    v->valuep = sym->addr;
	    break;

	 case SyReg:
	    v->valuep = (Addr)regaddr(tid, sym->regno);
	    break;

	 case SyEBPrel:
	 case SyESPrel:
	    reg = *regaddr(tid, sym->kind == SyESPrel ? R_ESP : R_EBP);
	    if (debug)
	       VG_(printf)("reg=%p+%d=%p\n", reg, sym->offset, reg+sym->offset);
	    v->valuep = (Addr)(reg + sym->offset);
	    break;

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
Char* VG_(describe_eip)(Addr eip, Char* buf, Int n_buf)
{
#define APPEND(str)                                         \
   { UChar* sss;                                            \
     for (sss = str; n < n_buf-1 && *sss != 0; n++,sss++)   \
        buf[n] = *sss;                                      \
     buf[n] = '\0';                                         \
   }
   UInt  lineno; 
   UChar ibuf[20];
   UInt  n = 0;
   UChar buf_fn[M_VG_ERRTXT];
   UChar buf_obj[M_VG_ERRTXT];
   UChar buf_srcloc[M_VG_ERRTXT];
   Bool  know_fnname  = VG_(get_fnname) (eip, buf_fn,  M_VG_ERRTXT);
   Bool  know_objname = VG_(get_objname)(eip, buf_obj, M_VG_ERRTXT);
   Bool  know_srcloc  = VG_(get_filename_linenum)(eip, buf_srcloc, M_VG_ERRTXT, 
                                                  &lineno);
   VG_(sprintf)(ibuf,"0x%x: ", eip);
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
   return buf;

#undef APPEND
}

/* Print a mini stack dump, showing the current location. */
void VG_(mini_stack_dump) ( Addr eips[], UInt n_eips )
{
   UInt  i;
   UChar buf[M_VG_ERRTXT];
   Bool  main_done = False;

   Int stop_at = n_eips;

   vg_assert(stop_at > 0);

   /* This loop is the basis for the one in VG_(gen_suppressions)();  if you
      change this, change it too! */
   i = 0;
   do {
      Addr eip = eips[i];
      if (i  > 0) eip--;            /* point to calling line */
      VG_(describe_eip)(eip, buf, M_VG_ERRTXT);

      if ( ! VG_(clo_show_below_main)) {
         // Stop after "main";  if main() is recursive, stop after last main().
         if (VG_(strstr)(buf, " main ("))
            main_done = True;
         else if (main_done)
            break;
      }
      VG_(message)(Vg_UserMsg, "   %s %s", ( i == 0 ? "at" : "by" ), buf);
      i++;

   } while (i < (UInt)stop_at && eips[i] != 0);
}


/*------------------------------------------------------------*/
/*--- General purpose redirection.                         ---*/
/*------------------------------------------------------------*/

/* Set to True for debug printing. */
static const Bool verbose_redir = False;


/* resolved redirections, indexed by from_addr */
typedef struct _CodeRedirect {
   const Char	*from_lib;	/* library qualifier pattern */
   const Char	*from_sym;	/* symbol */
   Addr		from_addr;	/* old addr */

   const Char	*to_lib;	/* library qualifier pattern */
   const Char	*to_sym;	/* symbol */
   Addr		to_addr;	/* new addr */

   struct _CodeRedirect *next;	/* next pointer on unresolved list */
} CodeRedirect;

static Int addrcmp(const void *ap, const void *bp)
{
   Addr a = *(Addr *)ap;
   Addr b = *(Addr *)bp;
   Int ret;

   if (a == b)
      ret = 0;
   else
      ret = (a < b) ? -1 : 1;

   return ret;
}

static Char *straddr(void *p)
{
   static Char buf[16];

   VG_(sprintf)(buf, "%p", *(Addr *)p);

   return buf;
}

static SkipList sk_resolved_redir = SKIPLIST_INIT(CodeRedirect, from_addr, 
						  addrcmp, straddr, VG_AR_SYMTAB);
static CodeRedirect *unresolved_redir = NULL;

static Bool match_lib(const Char *pattern, const SegInfo *si)
{
   /* pattern == NULL matches everything, otherwise use globbing

      If the pattern starts with:
	file:, then match filename
	soname:, then match soname
	something else, match filename
   */
   const Char *name = si->filename;

   if (pattern == NULL)
      return True;

   if (VG_(strncmp)(pattern, "file:", 5) == 0) {
      pattern += 5;
      name = si->filename;
   }
   if (VG_(strncmp)(pattern, "soname:", 7) == 0) {
      pattern += 7;
      name = si->soname;
   }

   if (name == NULL)
      return False;
   
   return VG_(string_match)(pattern, name);
}

/* Resolve a redir using si if possible, and add it to the resolved
   list */
static Bool resolve_redir(CodeRedirect *redir, const SegInfo *si)
{
   Bool resolved;

   vg_assert(si != NULL);
   vg_assert(si->seg != NULL);

   /* no redirection from Valgrind segments */
   if (si->seg->flags & SF_VALGRIND)
      return False;

   resolved = (redir->from_addr != 0) && (redir->to_addr != 0);

   if (0 && verbose_redir)
      VG_(printf)("   consider FROM binding %s:%s -> %s:%s in %s(%s)\n",
		  redir->from_lib, redir->from_sym,
		  redir->to_lib, redir->to_sym,
		  si->filename, si->soname);

   vg_assert(!resolved);

   if (redir->from_addr == 0) {
      vg_assert(redir->from_sym != NULL);

      if (match_lib(redir->from_lib, si)) {
	 redir->from_addr = reverse_search_one_symtab(si, redir->from_sym);
	 if (verbose_redir && redir->from_addr != 0)
	    VG_(printf)("   bind FROM: %p = %s:%s\n", 
                        redir->from_addr,redir->from_lib, redir->from_sym );
      }
   }

   if (redir->to_addr == 0) {
      vg_assert(redir->to_sym != NULL);

      if (match_lib(redir->to_lib, si)) {
	 redir->to_addr = reverse_search_one_symtab(si, redir->to_sym);
	 if (verbose_redir && redir->to_addr != 0)
	    VG_(printf)("   bind   TO: %p = %s:%s\n", 
                        redir->to_addr,redir->to_lib, redir->to_sym );

      }
   }

   resolved = (redir->from_addr != 0) && (redir->to_addr != 0);

   if (0 && verbose_redir)
      VG_(printf)("resolve_redir: %s:%s from=%p %s:%s to=%p\n",
		  redir->from_lib, redir->from_sym, redir->from_addr, 
		  redir->to_lib, redir->to_sym, redir->to_addr);

   if (resolved) {
      if (VG_(clo_verbosity) > 2 || verbose_redir) {
	 VG_(message)(Vg_DebugMsg, "  redir resolved (%s:%s=%p -> ",
		      redir->from_lib, redir->from_sym, redir->from_addr);
	 VG_(message)(Vg_DebugMsg, "                  %s:%s=%p)",
		      redir->to_lib, redir->to_sym, redir->to_addr);
      }
      
      if (VG_(search_transtab)(redir->from_addr) != 0)
	 VG_(message)(Vg_DebugMsg, "!!!! adding redirect to already called function %s (%p -> %p)!!!",
		      redir->from_sym, redir->from_addr, redir->to_addr);

      VG_(SkipList_Insert)(&sk_resolved_redir, redir);
   }

   return resolved;
}

/* Go through the complete redir list, resolving as much as possible with this SegInfo.

    This should be called when a new SegInfo symtab is loaded.
 */
static void resolve_seg_redirs(SegInfo *si)
{
   CodeRedirect **prevp = &unresolved_redir;
   CodeRedirect *redir, *next;

   if (verbose_redir)
      VG_(printf)("Considering redirs to/from %s(soname=%s)\n",
                  si->filename, si->soname);

   /* visit each unresolved redir - if it becomes resolved, then
      remove it from the unresolved list */
   for(redir = unresolved_redir; redir != NULL; redir = next) {
      next = redir->next;

      if (resolve_redir(redir, si)) {
	 *prevp = next;
	 redir->next = NULL;
      } else
	 prevp = &redir->next;
   }
}

static Bool resolve_redir_allsegs(CodeRedirect *redir)
{
   SegInfo *si;

   for(si = segInfo; si != NULL; si = si->next)
      if (resolve_redir(redir, si))
	 return True;

   return False;
}

/* Redirect a lib/symbol reference to a function at lib/symbol */
void VG_(add_redirect_sym)(const Char *from_lib, const Char *from_sym,
			   const Char *to_lib, const Char *to_sym)
{
   CodeRedirect *redir = VG_(SkipNode_Alloc)(&sk_resolved_redir);

   redir->from_lib = VG_(arena_strdup)(VG_AR_SYMTAB, from_lib);
   redir->from_sym = VG_(arena_strdup)(VG_AR_SYMTAB, from_sym);
   redir->from_addr = 0;

   redir->to_lib = VG_(arena_strdup)(VG_AR_SYMTAB, to_lib);
   redir->to_sym = VG_(arena_strdup)(VG_AR_SYMTAB, to_sym);
   redir->to_addr = 0;

   if (VG_(clo_verbosity) >= 2)
      VG_(message)(Vg_UserMsg, 
                   "REDIRECT %s(%s) to %s(%s)",
                   from_lib, from_sym, to_lib, to_sym);

   if (!resolve_redir_allsegs(redir)) {
      /* can't resolve immediately; add to list */
      redir->next = unresolved_redir;
      unresolved_redir = redir;
   }
}

/* Redirect a lib/symbol reference to a function at lib/symbol */
void VG_(add_redirect_addr)(const Char *from_lib, const Char *from_sym,
			    Addr to_addr)
{
   CodeRedirect *redir = VG_(SkipNode_Alloc)(&sk_resolved_redir);

   redir->from_lib = VG_(arena_strdup)(VG_AR_SYMTAB, from_lib);
   redir->from_sym = VG_(arena_strdup)(VG_AR_SYMTAB, from_sym);
   redir->from_addr = 0;

   redir->to_lib = NULL;
   redir->to_sym = NULL;
   redir->to_addr = to_addr;

   if (!resolve_redir_allsegs(redir)) {
      /* can't resolve immediately; add to list */
      redir->next = unresolved_redir;
      unresolved_redir = redir;
   }
}

Addr VG_(code_redirect)(Addr a)
{
   CodeRedirect *r = VG_(SkipList_Find)(&sk_resolved_redir, &a);

   if (r == NULL || r->from_addr != a)
      return a;

   vg_assert(r->to_addr != 0);

   return r->to_addr;
}

void VG_(setup_code_redirect_table) ( void )
{
   static const struct {
      const Char *from, *to;
   } redirects[] = {
      { "__GI___errno_location",	"__errno_location"	},
      { "__errno_location",		"__errno_location"	},
      { "__GI___h_errno_location",	"__h_errno_location"	},
      { "__h_errno_location",		"__h_errno_location"	},
      { "__GI___res_state",		"__res_state"		},
      { "__res_state",			"__res_state"		},
   };
   Int i;

   for(i = 0; i < sizeof(redirects)/sizeof(*redirects); i++) {
      VG_(add_redirect_sym)("soname:libc.so.6",		redirects[i].from,
			    "soname:libpthread.so.0",	redirects[i].to);
   }

   /* Overenthusiastic use of PLT bypassing by the glibc people also
      means we need to patch the following functions to our own
      implementations of said, in mac_replace_strmem.c.
    */
   VG_(add_redirect_sym)("soname:libc.so.6", "stpcpy",
			 "*vgpreload_memcheck.so*", "stpcpy");
}

/*------------------------------------------------------------*/
/*--- SegInfo accessor functions                           ---*/
/*------------------------------------------------------------*/

const SegInfo* VG_(next_seginfo)(const SegInfo* seg)
{
   if (seg == NULL)
      return segInfo;
   return seg->next;
}

Addr VG_(seg_start)(const SegInfo* seg)
{
   return seg->start;
}

UInt VG_(seg_size)(const SegInfo* seg)
{
   return seg->size;
}

const UChar* VG_(seg_filename)(const SegInfo* seg)
{
   return seg->filename;
}

UInt VG_(seg_sym_offset)(const SegInfo* seg)
{
   return seg->offset;
}

VgSectKind VG_(seg_sect_kind)(Addr a)
{
   SegInfo* seg;
   VgSectKind ret = Vg_SectUnknown;

   for(seg = segInfo; seg != NULL; seg = seg->next) {
      if (a >= seg->start && a < (seg->start + seg->size)) {
	 if (0)
	    VG_(printf)("addr=%p seg=%p %s got=%p %d  plt=%p %d data=%p %d bss=%p %d\n",
			a, seg, seg->filename, 
			seg->got_start, seg->got_size,
			seg->plt_start, seg->plt_size,
			seg->data_start, seg->data_size,
			seg->bss_start, seg->bss_size);
	 ret = Vg_SectText;

	 if (a >= seg->data_start && a < (seg->data_start + seg->data_size))
	    ret = Vg_SectData;
	 else if (a >= seg->bss_start && a < (seg->bss_start + seg->bss_size))
	    ret = Vg_SectBSS;
	 else if (a >= seg->plt_start && a < (seg->plt_start + seg->plt_size))
	    ret = Vg_SectPLT;
	 else if (a >= seg->got_start && a < (seg->got_start + seg->got_size))
	    ret = Vg_SectGOT;
      }
   }

   return ret;
}

/*--------------------------------------------------------------------*/
/*--- end                                             vg_symtab2.c ---*/
/*--------------------------------------------------------------------*/
