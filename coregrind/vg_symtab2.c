/*--------------------------------------------------------------------*/
/*--- Management of symbols and debugging information.             ---*/
/*---                                                 vg_symtab2.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"

#include <elf.h>          /* ELF defns                      */
#include <a.out.h>        /* stabs defns                    */


/* Majorly rewritten Sun 3 Feb 02 to enable loading symbols from
   dlopen()ed libraries, which is something that KDE3 does a lot.

   Stabs reader greatly improved by Nick Nethercode, Apr 02.

   16 May 02: when notified about munmap, return a Bool indicating
   whether or not the area being munmapped had executable permissions.
   This is then used to determine whether or not
   VG_(invalid_translations) should be called for that area.  In order
   that this work even if --instrument=no, in this case we still keep
   track of the mapped executable segments, but do not load any debug
   info or symbols.
*/

/*------------------------------------------------------------*/
/*--- Structs n stuff                                      ---*/
/*------------------------------------------------------------*/

/* A structure to hold an ELF symbol (very crudely). */
typedef 
   struct { 
      Addr addr;   /* lowest address of entity */
      UInt size;   /* size in bytes */
      Int  nmoff;  /* offset of name in this SegInfo's str tab */
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
      UInt   fnmoff;                /* source filename; offset in this 
                                       SegInfo's str tab */
   }
   RiLoc;


/* A structure which contains information pertaining to one mapped
   text segment. */
typedef
   struct _SegInfo {
      struct _SegInfo* next;
      /* Description of the mapped segment. */
      Addr   start;
      UInt   size;
      UChar* filename; /* in mallocville */
      UInt   foffset;
      /* An expandable array of symbols. */
      RiSym* symtab;
      UInt   symtab_used;
      UInt   symtab_size;
      /* An expandable array of locations. */
      RiLoc* loctab;
      UInt   loctab_used;
      UInt   loctab_size;
      /* An expandable array of characters -- the string table. */
      Char*  strtab;
      UInt   strtab_used;
      UInt   strtab_size;
      /* offset    is what we need to add to symbol table entries
                   to get the real location of that symbol in memory.
                   For executables, offset is zero.  
                   For .so's, offset == base_addr.
                   This seems like a giant kludge to me.
      */
      UInt   offset;
   } 
   SegInfo;


/* -- debug helper -- */
static void ppSegInfo ( SegInfo* si )
{
   VG_(printf)("name: %s\n"
               "start %p, size %d, foffset %d\n",
               si->filename?si->filename : (UChar*)"NULL",
               si->start, si->size, si->foffset );
}

static void freeSegInfo ( SegInfo* si )
{
   vg_assert(si != NULL);
   if (si->filename) VG_(free)(VG_AR_SYMTAB, si->filename);
   if (si->symtab) VG_(free)(VG_AR_SYMTAB, si->symtab);
   if (si->loctab) VG_(free)(VG_AR_SYMTAB, si->loctab);
   if (si->strtab) VG_(free)(VG_AR_SYMTAB, si->strtab);
   VG_(free)(VG_AR_SYMTAB, si);
}


/*------------------------------------------------------------*/
/*--- Adding stuff                                         ---*/
/*------------------------------------------------------------*/

/* Add a str to the string table, including terminating zero, and
   return offset of the string in vg_strtab. */

static __inline__
Int addStr ( SegInfo* si, Char* str )
{
   Char* new_tab;
   Int   new_sz, i, space_needed;
   
   space_needed = 1 + VG_(strlen)(str);
   if (si->strtab_used + space_needed > si->strtab_size) {
      new_sz = 2 * si->strtab_size;
      if (new_sz == 0) new_sz = 5000;
      new_tab = VG_(malloc)(VG_AR_SYMTAB, new_sz);
      if (si->strtab != NULL) {
         for (i = 0; i < si->strtab_used; i++)
            new_tab[i] = si->strtab[i];
         VG_(free)(VG_AR_SYMTAB, si->strtab);
      }
      si->strtab      = new_tab;
      si->strtab_size = new_sz;
   }

   for (i = 0; i < space_needed; i++)
      si->strtab[si->strtab_used+i] = str[i];

   si->strtab_used += space_needed;
   vg_assert(si->strtab_used <= si->strtab_size);
   return si->strtab_used - space_needed;
}

/* Add a symbol to the symbol table. */

static __inline__
void addSym ( SegInfo* si, RiSym* sym )
{
   Int    new_sz, i;
   RiSym* new_tab;

   /* Ignore zero-sized syms. */
   if (sym->size == 0) return;

   if (si->symtab_used == si->symtab_size) {
      new_sz = 2 * si->symtab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = VG_(malloc)(VG_AR_SYMTAB, new_sz * sizeof(RiSym) );
      if (si->symtab != NULL) {
         for (i = 0; i < si->symtab_used; i++)
            new_tab[i] = si->symtab[i];
         VG_(free)(VG_AR_SYMTAB, si->symtab);
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
   Int    new_sz, i;
   RiLoc* new_tab;

   /* Zero-sized locs should have been ignored earlier */
   vg_assert(loc->size > 0);

   if (si->loctab_used == si->loctab_size) {
      new_sz = 2 * si->loctab_size;
      if (new_sz == 0) new_sz = 500;
      new_tab = VG_(malloc)(VG_AR_SYMTAB, new_sz * sizeof(RiLoc) );
      if (si->loctab != NULL) {
         for (i = 0; i < si->loctab_used; i++)
            new_tab[i] = si->loctab[i];
         VG_(free)(VG_AR_SYMTAB, si->loctab);
      }
      si->loctab = new_tab;
      si->loctab_size = new_sz;
   }

   si->loctab[si->loctab_used] = *loc;
   si->loctab_used++;
   vg_assert(si->loctab_used <= si->loctab_size);
}


/* Top-level place to call to add a source-location mapping entry. */

static __inline__
void addLineInfo ( SegInfo* si,
                   Int      fnmoff,
                   Addr     this,
                   Addr     next,
                   Int      lineno,
                   Int      entry )
{
   RiLoc loc;
   Int size = next - this;

   /* Ignore zero-sized locs */
   if (this == next) return;

   /* Maximum sanity checking.  Some versions of GNU as do a shabby
    * job with stabs entries; if anything looks suspicious, revert to
    * a size of 1.  This should catch the instruction of interest
    * (since if using asm-level debug info, one instruction will
    * correspond to one line, unlike with C-level debug info where
    * multiple instructions can map to the one line), but avoid
    * catching any other instructions bogusly. */
   if (this > next) {
       VG_(message)(Vg_DebugMsg, 
                    "warning: stabs addresses out of order "
                    "at entry %d: 0x%x 0x%x", entry, this, next);
       size = 1;
   }

   if (size > MAX_LOC_SIZE) {
       VG_(message)(Vg_DebugMsg, 
                    "warning: stabs line address range too large "
                    "at entry %d: %d", entry, size);
       size = 1;
   }

   /* hack! */
   if (this >= 0 && this <= 100) {
       VG_(message)(Vg_DebugMsg, 
                    "warning: ignoring bogus stabs entry "
                    "%p %p %d: %d", this, next, entry, size);
       return;
   }

   //vg_assert(this < si->start + si->size && next-1 >= si->start);
   if (this >= si->start + si->size || next-1 < si->start) {
       VG_(message)(Vg_DebugMsg, "warning: ignoring stabs entry falling "
                                 "outside current SegInfo: %x %x %x %x\n",
                                 si->start, si->start + si->size, 
                                 this, next-1);
       return;
   }

   vg_assert(lineno >= 0);
   if (lineno > MAX_LINENO) {
       VG_(message)(Vg_UserMsg, 
                    "warning: ignoring stabs entry with "
                    "huge line number (%d)\n", lineno);
       VG_(message)(Vg_UserMsg, 
                    "         Can't handle line numbers "
                    "greater than %d, sorry\n", MAX_LINENO);
       return;
   }

   loc.addr      = this;
   loc.size      = (UShort)size;
   loc.lineno    = lineno;
   loc.fnmoff    = fnmoff;
   addLoc ( si, &loc );
}


/*------------------------------------------------------------*/
/*--- Helpers                                              ---*/
/*------------------------------------------------------------*/

/* Non-fatal -- use vg_panic if terminal. */
static 
void vg_symerr ( Char* msg )
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
                &si->strtab[si->symtab[i].nmoff] );
}


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
static 
void canonicaliseSymtab ( SegInfo* si )
{
   /* Magic numbers due to Janet Incerpi and Robert Sedgewick. */
   Int   incs[16] = { 1, 3, 7, 21, 48, 112, 336, 861, 1968,
                      4592, 13776, 33936, 86961, 198768, 
                      463792, 1391376 };
   Int   lo = 0;
   Int   hi = si->symtab_used-1;
   Int   i, j, h, bigN, hp, n_merged, n_truncated;
   RiSym v;
   Addr  s1, s2, e1, e2;

#  define SWAP(ty,aa,bb) \
      do { ty tt = (aa); (aa) = (bb); (bb) = tt; } while (0)

   bigN = hi - lo + 1; if (bigN < 2) return;
   hp = 0; while (hp < 16 && incs[hp] < bigN) hp++; hp--;
   vg_assert(0 <= hp && hp < 16);

   for (; hp >= 0; hp--) {
      h = incs[hp];
      i = lo + h;
      while (1) {
         if (i > hi) break;
         v = si->symtab[i];
         j = i;
         while (si->symtab[j-h].addr > v.addr) {
            si->symtab[j] = si->symtab[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         si->symtab[j] = v;
         i++;
      }
   }

  cleanup_more:
 
   /* If two symbols have identical address ranges, favour the
      one with the longer name. 
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
            if (VG_(strlen)(&si->strtab[si->symtab[i].nmoff]) 
                > VG_(strlen)(&si->strtab[si->symtab[i+1].nmoff])) {
               si->symtab[si->symtab_used++] = si->symtab[i];
            } else {
               si->symtab[si->symtab_used++] = si->symtab[i+1];
            }
            i++;
         } else {
            si->symtab[si->symtab_used++] = si->symtab[i];
         }
      }
      if (VG_(clo_trace_symtab))
         VG_(printf)( "%d merged\n", n_merged);
   }
   while (n_merged > 0);

   /* Detect and "fix" overlapping address ranges. */
   n_truncated = 0;

   for (i = 0; i < si->symtab_used-1; i++) {

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
      while (j < si->symtab_used-1 
             && si->symtab[j].addr > si->symtab[j+1].addr) {
         SWAP(RiSym,si->symtab[j],si->symtab[j+1]);
         j++;
      }
      n_truncated++;
   }

   if (n_truncated > 0) goto cleanup_more;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < si->symtab_used-1; i++) {
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
static 
void canonicaliseLoctab ( SegInfo* si )
{
   /* Magic numbers due to Janet Incerpi and Robert Sedgewick. */
   Int   incs[16] = { 1, 3, 7, 21, 48, 112, 336, 861, 1968,
                      4592, 13776, 33936, 86961, 198768, 
                      463792, 1391376 };
   Int   lo = 0;
   Int   hi = si->loctab_used-1;
   Int   i, j, h, bigN, hp;
   RiLoc v;

#  define SWAP(ty,aa,bb) \
      do { ty tt = (aa); (aa) = (bb); (bb) = tt; } while (0);

   /* Sort by start address. */

   bigN = hi - lo + 1; if (bigN < 2) return;
   hp = 0; while (hp < 16 && incs[hp] < bigN) hp++; hp--;
   vg_assert(0 <= hp && hp < 16);

   for (; hp >= 0; hp--) {
      h = incs[hp];
      i = lo + h;
      while (1) {
         if (i > hi) break;
         v = si->loctab[i];
         j = i;
         while (si->loctab[j-h].addr > v.addr) {
            si->loctab[j] = si->loctab[j-h];
            j = j - h;
            if (j <= (lo + h - 1)) break;
         }
         si->loctab[j] = v;
         i++;
      }
   }

   /* If two adjacent entries overlap, truncate the first. */
   for (i = 0; i < si->loctab_used-1; i++) {
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
   for (i = 0; i < si->loctab_used; i++) {
      if (si->loctab[i].size > 0) {
         si->loctab[j] = si->loctab[i];
         j++;
      }
   }
   si->loctab_used = j;

   /* Ensure relevant postconditions hold. */
   for (i = 0; i < si->loctab_used-1; i++) {
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
/*--- Read STABS format debug info.                        ---*/
/*------------------------------------------------------------*/

/* Stabs entry types, from:
 *   The "stabs" debug format
 *   Menapace, Kingdon and MacKenzie
 *   Cygnus Support
 */
typedef enum { N_GSYM  = 32,    /* Global symbol                    */
               N_FUN   = 36,    /* Function start or end            */
               N_STSYM = 38,    /* Data segment file-scope variable */
               N_LCSYM = 40,    /* BSS segment file-scope variable  */
               N_RSYM  = 64,    /* Register variable                */
               N_SLINE = 68,    /* Source line number               */
               N_SO    = 100,   /* Source file path and name        */
               N_LSYM  = 128,   /* Stack variable or type           */
               N_SOL   = 132,   /* Include file name                */
               N_LBRAC = 192,   /* Start of lexical block           */
               N_RBRAC = 224    /* End   of lexical block           */
             } stab_types;
      

/* Read stabs-format debug info.  This is all rather horrible because
   stabs is a underspecified, kludgy hack.
*/
static
void read_debuginfo_stabs ( SegInfo* si,
                            UChar* stabC,   Int stab_sz, 
                            UChar* stabstr, Int stabstr_sz )
{
   Int    i;
   Int    curr_filenmoff;
   Addr   curr_fn_stabs_addr = (Addr)NULL;
   Addr   curr_fnbaseaddr    = (Addr)NULL;
   Char  *curr_file_name, *curr_fn_name;
   Int    n_stab_entries;
   Int    prev_lineno = 0, lineno = 0;
   Int    lineno_overflows = 0;
   Bool   same_file = True;
   struct nlist* stab = (struct nlist*)stabC;

   /* Ok.  It all looks plausible.  Go on and read debug data. 
         stab kinds: 100   N_SO     a source file name
                      68   N_SLINE  a source line number
                      36   N_FUN    start of a function

      In this loop, we maintain a current file name, updated as 
      N_SO/N_SOLs appear, and a current function base address, 
      updated as N_FUNs appear.  Based on that, address ranges for 
      N_SLINEs are calculated, and stuffed into the line info table.

      Finding the instruction address range covered by an N_SLINE is
      complicated;  see the N_SLINE case below.
   */
   curr_filenmoff     = addStr(si,"???");
   curr_file_name     = curr_fn_name = (Char*)NULL;

   n_stab_entries = stab_sz/(int)sizeof(struct nlist);

   for (i = 0; i < n_stab_entries; i++) {
#     if 0
      VG_(printf) ( "   %2d  ", i );
      VG_(printf) ( "type=0x%x   othr=%d   desc=%d   value=0x%x   strx=%d  %s",
                    stab[i].n_type, stab[i].n_other, stab[i].n_desc, 
                    (int)stab[i].n_value,
                    (int)stab[i].n_un.n_strx, 
                    stabstr + stab[i].n_un.n_strx );
      VG_(printf)("\n");
#     endif

      Char *no_fn_name = "???";

      switch (stab[i].n_type) {
         UInt next_addr;

         /* Two complicated things here:
	  *
          * 1. the n_desc field in 'struct n_list' in a.out.h is only
          *    16-bits, which gives a maximum of 65535 lines.  We handle
          *    files bigger than this by detecting heuristically
          *    overflows -- if the line count goes from 65000-odd to
          *    0-odd within the same file, we assume it's an overflow.
          *    Once we switch files, we zero the overflow count.
          *
          * 2. To compute the instr address range covered by a single
          *    line, find the address of the next thing and compute the
          *    difference.  The approach used depends on what kind of
          *    entry/entries follow...
          */
         case N_SLINE: {
            Int this_addr = (UInt)stab[i].n_value;

            /* Although stored as a short, neg values really are >
             * 32768, hence the UShort cast.  Then we use an Int to
             * handle overflows. */
            prev_lineno = lineno;
            lineno      = (Int)((UShort)stab[i].n_desc);

            if (prev_lineno > lineno + OVERFLOW_DIFFERENCE && same_file) {
               VG_(message)(Vg_DebugMsg, 
                  "Line number overflow detected (%d --> %d) in %s", 
                  prev_lineno, lineno, curr_file_name);
               lineno_overflows++;
            }
            same_file = True;

            LOOP:
            if (i+1 >= n_stab_entries) {
               /* If it's the last entry, just guess the range is
                * four; can't do any better */
               next_addr = this_addr + 4;
            } else {    
               switch (stab[i+1].n_type) {
                  /* Easy, common case: use address of next entry */
                  case N_SLINE: case N_SO:
                     next_addr = (UInt)stab[i+1].n_value;
                     break;

                  /* Boring one: skip, look for something more
                     useful. */
                  case N_RSYM: case N_LSYM: case N_LBRAC: case N_RBRAC: 
                  case N_STSYM: case N_LCSYM: case N_GSYM:
                     i++;
                     goto LOOP;
                     
                  /* If end-of-this-fun entry, use its address.
                   * If start-of-next-fun entry, find difference between start
                   *   of current function and start of next function to work
                   *   it out.
                   */
                  case N_FUN: 
                     if ('\0' == * (stabstr + stab[i+1].n_un.n_strx) ) {
                        next_addr = (UInt)stab[i+1].n_value;
                     } else {
                        next_addr = 
                            (UInt)stab[i+1].n_value - curr_fn_stabs_addr;
                     }
                     break;

                  /* N_SOL should be followed by an N_SLINE which can
                     be used */
                  case N_SOL:
                     if (i+2 < n_stab_entries && N_SLINE == stab[i+2].n_type) {
                        next_addr = (UInt)stab[i+2].n_value;
                        break;
                     } else {
                        VG_(printf)("unhandled N_SOL stabs case: %d %d %d", 
                                    stab[i+1].n_type, i, n_stab_entries);
                        VG_(panic)("unhandled N_SOL stabs case");
                     }

                  default:
                     VG_(printf)("unhandled (other) stabs case: %d %d", 
                                 stab[i+1].n_type,i);
                     /* VG_(panic)("unhandled (other) stabs case"); */
                     next_addr = this_addr + 4;
                     break;
               }
            }
            
            addLineInfo ( si, curr_filenmoff, curr_fnbaseaddr + this_addr, 
                          curr_fnbaseaddr + next_addr,
                          lineno + lineno_overflows * LINENO_OVERFLOW, i);
            break;
         }

         case N_FUN: {
            if ('\0' != (stabstr + stab[i].n_un.n_strx)[0] ) {
               /* N_FUN with a name -- indicates the start of a fn.  */
               curr_fn_stabs_addr = (Addr)stab[i].n_value;
               curr_fnbaseaddr = si->offset + curr_fn_stabs_addr;
               curr_fn_name = stabstr + stab[i].n_un.n_strx;
            } else {
               curr_fn_name = no_fn_name;
            }
            break;
         }

         case N_SOL:
            if (lineno_overflows != 0) {
               VG_(message)(Vg_UserMsg, 
                            "Warning: file %s is very big (> 65535 lines) "
                            "Line numbers and annotation for this file might "
                            "be wrong.  Sorry",
                            curr_file_name);
            }
            /* fall through! */
         case N_SO: 
            lineno_overflows = 0;

         /* seems to give lots of locations in header files */
         /* case 130: */ /* BINCL */
         { 
            UChar* nm = stabstr + stab[i].n_un.n_strx;
            UInt len = VG_(strlen)(nm);
            
            if (len > 0 && nm[len-1] != '/') {
               curr_filenmoff = addStr ( si, nm );
               curr_file_name = stabstr + stab[i].n_un.n_strx;
            }
            else
               if (len == 0)
                  curr_filenmoff = addStr ( si, "?1\0" );

            break;
         }

#        if 0
         case 162: /* EINCL */
            curr_filenmoff = addStr ( si, "?2\0" );
            break;
#        endif

         default:
            break;
      }
   } /* for (i = 0; i < stab_sz/(int)sizeof(struct nlist); i++) */
}


/*------------------------------------------------------------*/
/*--- Read DWARF2 format debug info.                       ---*/
/*------------------------------------------------------------*/

/* Structure found in the .debug_line section.  */
typedef struct
{
  UChar li_length          [4];
  UChar li_version         [2];
  UChar li_prologue_length [4];
  UChar li_min_insn_length [1];
  UChar li_default_is_stmt [1];
  UChar li_line_base       [1];
  UChar li_line_range      [1];
  UChar li_opcode_base     [1];
}
DWARF2_External_LineInfo;
typedef struct
{
  UInt  li_length;
  UShort li_version;
  UInt   li_prologue_length;
  UChar  li_min_insn_length;
  UChar  li_default_is_stmt;
  Int            li_line_base;
  UChar  li_line_range;
  UChar  li_opcode_base;
}
DWARF2_Internal_LineInfo;
/* Line number opcodes.  */
enum dwarf_line_number_ops
  {
    DW_LNS_extended_op = 0,
    DW_LNS_copy = 1,
    DW_LNS_advance_pc = 2,
    DW_LNS_advance_line = 3,
    DW_LNS_set_file = 4,
    DW_LNS_set_column = 5,
    DW_LNS_negate_stmt = 6,
    DW_LNS_set_basic_block = 7,
    DW_LNS_const_add_pc = 8,
    DW_LNS_fixed_advance_pc = 9,
    /* DWARF 3.  */
    DW_LNS_set_prologue_end = 10,
    DW_LNS_set_epilogue_begin = 11,
    DW_LNS_set_isa = 12
  };

/* Line number extended opcodes.  */
enum dwarf_line_number_x_ops
  {
    DW_LNE_end_sequence = 1,
    DW_LNE_set_address = 2,
    DW_LNE_define_file = 3
  };

typedef struct State_Machine_Registers
{
  Addr address;
  UInt  file;
  UInt  line;
  UInt  column;
  Int   is_stmt;
  Int   basic_block;
  Int end_sequence;
/* This variable hold the number of the last entry seen
   in the File Table.  */
  UInt  last_file_entry;
} SMR;


static 
UInt read_leb128 ( UChar* data, Int* length_return, Int sign )
{
  UInt result = 0;
  UInt num_read = 0;
  Int  shift = 0;
  UChar     byte;

  do
    {
      byte = * data ++;
      num_read ++;

      result |= (byte & 0x7f) << shift;

      shift += 7;

    }
  while (byte & 0x80);

  if (length_return != NULL)
    * length_return = num_read;

  if (sign && (shift < 32) && (byte & 0x40))
    result |= -1 << shift;

  return result;
}


static SMR state_machine_regs;

static void
reset_state_machine (is_stmt)
     Int is_stmt;
{
  state_machine_regs.address = 0;
  state_machine_regs.file = 1;
  state_machine_regs.line = 1;
  state_machine_regs.column = 0;
  state_machine_regs.is_stmt = is_stmt;
  state_machine_regs.basic_block = 0;
  state_machine_regs.end_sequence = 0;
  state_machine_regs.last_file_entry = 0;
}

/* Handled an extend line op.  Returns true if this is the end
   of sequence.  */
static int
process_extended_line_op (si, fnames, data, is_stmt, pointer_size)
     SegInfo *si;
     UInt **fnames;
     UChar * data;
     Int is_stmt;
     Int pointer_size;
{
  UChar   op_code;
  Int             bytes_read;
  UInt    len;
  UChar * name;
  Addr   adr;

  len = read_leb128 (data, & bytes_read, 0);
  data += bytes_read;

  if (len == 0)
    {
      VG_(message) (Vg_UserMsg,"badly formed extended line op encountered!\n");
      return bytes_read;
    }

  len += bytes_read;
  op_code = * data ++;


  switch (op_code)
    {
    case DW_LNE_end_sequence:
      addLineInfo (si, (*fnames)[state_machine_regs.file], si->offset + (state_machine_regs.address - 1), si->offset + (state_machine_regs.address), 0, 0);
      reset_state_machine (is_stmt);
      break;

    case DW_LNE_set_address:
      /* XXX: Pointer size could be 8 */
      adr = *((Addr *)data);
      state_machine_regs.address = adr;
      break;

    case DW_LNE_define_file:

      ++ state_machine_regs.last_file_entry;
      name = data;
      if (*fnames == NULL)
        *fnames = VG_(malloc)(VG_AR_SYMTAB, sizeof (UInt) * 2);
      else
        *fnames = VG_(realloc)(VG_AR_SYMTAB, *fnames, sizeof (UInt) * (state_machine_regs.last_file_entry + 1));
      (*fnames)[state_machine_regs.last_file_entry] = addStr (si,name);
      data += VG_(strlen) ((char *) data) + 1;
      read_leb128 (data, & bytes_read, 0);
      data += bytes_read;
      read_leb128 (data, & bytes_read, 0);
      data += bytes_read;
       read_leb128 (data, & bytes_read, 0);
      break;

    default:
      break;
    }

  return len;
}


static
void read_debuginfo_dwarf2 ( SegInfo* si, UChar* dwarf2, Int dwarf2_sz )
{
  DWARF2_External_LineInfo * external;
  DWARF2_Internal_LineInfo   info;
  UChar *            standard_opcodes;
  UChar *            data = dwarf2;
  UChar *            end  = dwarf2 + dwarf2_sz;
  UChar *            end_of_sequence;
  UInt *fnames = NULL;


  while (data < end)
    {
      external = (DWARF2_External_LineInfo *) data;

      /* Check the length of the block.  */
      info.li_length =*((UInt *)(external->li_length));

      if (info.li_length == 0xffffffff)
       {
         vg_symerr("64-bit DWARF line info is not supported yet.");
         break;
       }

      if (info.li_length + sizeof (external->li_length) > dwarf2_sz)
       {
        vg_symerr("DWARF line info appears to be corrupt - the section is too small");
         return;
       }

      /* Check its version number.  */
      info.li_version =*((UShort *) (external->li_version));
      if (info.li_version != 2)
       {
         vg_symerr("Only DWARF version 2 line info is currently supported.");
         return;
       }

      info.li_prologue_length = *((UInt *)(external->li_prologue_length));
      info.li_min_insn_length = *((UChar *) (external->li_min_insn_length));
      info.li_default_is_stmt = *((UChar *) (external->li_default_is_stmt));
      info.li_line_base       = *((Int *) (external->li_line_base));
      info.li_line_range      = *((UChar *) (external->li_line_range));
      info.li_opcode_base     = *((UChar *) (external->li_opcode_base)); 

      /* Sign extend the line base field.  */
      info.li_line_base <<= 24;
      info.li_line_base >>= 24;

         end_of_sequence = data + info.li_length + sizeof (external->li_length);

      reset_state_machine (info.li_default_is_stmt);

      /* Read the contents of the Opcodes table.  */
      standard_opcodes = data + sizeof (* external);



      /* Read the contents of the Directory table.  */
      data = standard_opcodes + info.li_opcode_base - 1;

      if (* data == 0);
      else
       {
               /* We ignore the directory table, since gcc gives the entire path as part of the filename */
         while (* data != 0)
           {
             data += VG_(strlen) ((char *) data) + 1;
           }
       }

      /* Skip the NUL at the end of the table.  */
      data ++;

      /* Read the contents of the File Name table.  */
      if (* data == 0);
      else
       {

         while (* data != 0)
           {
             UChar * name;
             Int bytes_read;

            ++ state_machine_regs.last_file_entry;
             name = data;
             /* Since we don't have realloc (0, ....) == malloc (...) semantics, we need to malloc the first time. */

             if (fnames == NULL)
               fnames = VG_(malloc)(VG_AR_SYMTAB, sizeof (UInt) * 2);
             else
               fnames = VG_(realloc)(VG_AR_SYMTAB, fnames, sizeof (UInt) * (state_machine_regs.last_file_entry + 1));
             data += VG_(strlen) ((char *) data) + 1;
             fnames[state_machine_regs.last_file_entry] = addStr (si,name);

             read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
           }
       }

      /* Skip the NUL at the end of the table.  */
      data ++;

      /* Now display the statements.  */

      while (data < end_of_sequence)
       {
         UChar op_code;
         Int           adv;
         Int           bytes_read;

         op_code = * data ++;

         if (op_code >= info.li_opcode_base)
           {
             Int advAddr;
             op_code -= info.li_opcode_base;
             adv      = (op_code / info.li_line_range) * info.li_min_insn_length;
             advAddr = adv;
             state_machine_regs.address += adv;
             adv = (op_code % info.li_line_range) + info.li_line_base;
             addLineInfo (si, fnames[state_machine_regs.file], si->offset + (state_machine_regs.address - advAddr), si->offset + (state_machine_regs.address), state_machine_regs.line, 0);
             state_machine_regs.line += adv;
           }
         else switch (op_code)
           {
           case DW_LNS_extended_op:
             data += process_extended_line_op (si, &fnames, data, info.li_default_is_stmt,
                                               sizeof (Addr));
             break;

           case DW_LNS_copy:
             addLineInfo (si, fnames[state_machine_regs.file], si->offset + state_machine_regs.address, si->offset + (state_machine_regs.address + 1), state_machine_regs.line , 0);
             break;

           case DW_LNS_advance_pc:
             adv = info.li_min_insn_length * read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             state_machine_regs.address += adv;
             break;

           case DW_LNS_advance_line:
             adv = read_leb128 (data, & bytes_read, 1);
             data += bytes_read;
             state_machine_regs.line += adv;
             break;

           case DW_LNS_set_file:
             adv = read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             state_machine_regs.file = adv;
             break;

           case DW_LNS_set_column:
             adv = read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             state_machine_regs.column = adv;
             break;

           case DW_LNS_negate_stmt:
             adv = state_machine_regs.is_stmt;
             adv = ! adv;
             state_machine_regs.is_stmt = adv;
             break;

           case DW_LNS_set_basic_block:
             state_machine_regs.basic_block = 1;
             break;

           case DW_LNS_const_add_pc:
             adv = (((255 - info.li_opcode_base) / info.li_line_range)
                    * info.li_min_insn_length);
             state_machine_regs.address += adv;
             break;

           case DW_LNS_fixed_advance_pc:
             /* XXX: Need something to get 2 bytes */
             adv = *((UShort *)data);
             data += 2;
             state_machine_regs.address += adv;
             break;

           case DW_LNS_set_prologue_end:
             break;

           case DW_LNS_set_epilogue_begin:
             break;

           case DW_LNS_set_isa:
             adv = read_leb128 (data, & bytes_read, 0);
             data += bytes_read;
             break;

           default:
             {
               int j;
               for (j = standard_opcodes[op_code - 1]; j > 0 ; --j)
                 {
                   read_leb128 (data, &bytes_read, 0);
                   data += bytes_read;
                 }
             }
             break;
           }
       }
      VG_(free)(VG_AR_SYMTAB, fnames);
      fnames = NULL;
    }
}


/*------------------------------------------------------------*/
/*--- Read info from a .so/exe file.                       ---*/
/*------------------------------------------------------------*/

/* Read the symbols from the object/exe specified by the SegInfo into
   the tables within the supplied SegInfo.  */
static
void vg_read_lib_symbols ( SegInfo* si )
{
   Elf32_Ehdr*   ehdr;       /* The ELF header                          */
   Elf32_Shdr*   shdr;       /* The section table                       */
   UChar*        sh_strtab;  /* The section table's string table        */
   UChar*        stab;       /* The .stab table                         */
   UChar*        stabstr;    /* The .stab string table                  */
   UChar*        dwarf2;     /* The DWARF2 location info table          */
   Int           stab_sz;    /* Size in bytes of the .stab table        */
   Int           stabstr_sz; /* Size in bytes of the .stab string table */
   Int           dwarf2_sz;  /* Size in bytes of the DWARF2 srcloc table*/
   Int           fd;
   Int           i;
   Bool          ok;
   Addr          oimage;
   Int           n_oimage;
   struct vki_stat stat_buf;

   oimage = (Addr)NULL;
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg, "Reading syms from %s", si->filename );

   /* mmap the object image aboard, so that we can read symbols and
      line number info out of it.  It will be munmapped immediately
      thereafter; it is only aboard transiently. */

   i = VG_(stat)(si->filename, &stat_buf);
   if (i != 0) {
      vg_symerr("Can't stat .so/.exe (to determine its size)?!");
      return;
   }
   n_oimage = stat_buf.st_size;

   fd = VG_(open_read)(si->filename);
   if (fd == -1) {
      vg_symerr("Can't open .so/.exe to read symbols?!");
      return;
   }

   oimage = (Addr)VG_(mmap)( NULL, n_oimage, 
                             VKI_PROT_READ, VKI_MAP_PRIVATE, fd, 0 );
   if (oimage == ((Addr)(-1))) {
      VG_(message)(Vg_UserMsg,
                   "mmap failed on %s", si->filename );
      VG_(close)(fd);
      return;
   }

   VG_(close)(fd);

   /* Ok, the object image is safely in oimage[0 .. n_oimage-1]. 
      Now verify that it is a valid ELF .so or executable image.
   */
   ok = (n_oimage >= sizeof(Elf32_Ehdr));
   ehdr = (Elf32_Ehdr*)oimage;

   if (ok) {
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
   }

   if (!ok) {
      vg_symerr("Invalid ELF header, or missing stringtab/sectiontab.");
      VG_(munmap) ( (void*)oimage, n_oimage );
      return;
   }

   if (VG_(clo_trace_symtab))
      VG_(printf)( 
          "shoff = %d,  shnum = %d,  size = %d,  n_vg_oimage = %d\n",
          ehdr->e_shoff, ehdr->e_shnum, sizeof(Elf32_Shdr), n_oimage );

   if (ehdr->e_shoff + ehdr->e_shnum*sizeof(Elf32_Shdr) > n_oimage) {
      vg_symerr("ELF section header is beyond image end?!");
      VG_(munmap) ( (void*)oimage, n_oimage );
      return;
   }

   shdr = (Elf32_Shdr*)(oimage + ehdr->e_shoff);
   sh_strtab = (UChar*)(oimage + shdr[ehdr->e_shstrndx].sh_offset);

   /* try and read the object's symbol table */
   {
      UChar*     o_strtab    = NULL;
      Elf32_Sym* o_symtab    = NULL;
      UInt       o_strtab_sz = 0;
      UInt       o_symtab_sz = 0;

      UChar*     o_got = NULL;
      UChar*     o_plt = NULL;
      UInt       o_got_sz = 0;
      UInt       o_plt_sz = 0;

      Bool       snaffle_it;
      Addr       sym_addr;

      /* find the .stabstr and .stab sections */
      for (i = 0; i < ehdr->e_shnum; i++) {
         if (0 == VG_(strcmp)(".symtab",sh_strtab + shdr[i].sh_name)) {
            o_symtab    = (Elf32_Sym*)(oimage + shdr[i].sh_offset);
            o_symtab_sz = shdr[i].sh_size;
            vg_assert((o_symtab_sz % sizeof(Elf32_Sym)) == 0);
            /* check image overrun here */
         }
         if (0 == VG_(strcmp)(".strtab",sh_strtab + shdr[i].sh_name)) {
            o_strtab    = (UChar*)(oimage + shdr[i].sh_offset);
            o_strtab_sz = shdr[i].sh_size;
            /* check image overrun here */
         }

         /* find out where the .got and .plt sections will be in the
            executable image, not in the object image transiently loaded.
         */
         if (0 == VG_(strcmp)(".got",sh_strtab + shdr[i].sh_name)) {
            o_got    = (UChar*)(si->offset
                                + shdr[i].sh_offset);
            o_got_sz = shdr[i].sh_size;
            /* check image overrun here */
         }
         if (0 == VG_(strcmp)(".plt",sh_strtab + shdr[i].sh_name)) {
            o_plt    = (UChar*)(si->offset
                                + shdr[i].sh_offset);
            o_plt_sz = shdr[i].sh_size;
            /* check image overrun here */
         }

      }

      if (VG_(clo_trace_symtab)) {
         if (o_plt) VG_(printf)( "PLT: %p .. %p\n",
                                 o_plt, o_plt + o_plt_sz - 1 );
         if (o_got) VG_(printf)( "GOT: %p .. %p\n",
                                 o_got, o_got + o_got_sz - 1 );
      }

      if (o_strtab == NULL || o_symtab == NULL) {
         vg_symerr("   object doesn't have a symbol table");
      } else {
         /* Perhaps should start at i = 1; ELF docs suggest that entry
            0 always denotes `unknown symbol'. */
         for (i = 1; i < o_symtab_sz/sizeof(Elf32_Sym); i++){
#           if 0
            VG_(printf)("raw symbol: ");
            switch (ELF32_ST_BIND(o_symtab[i].st_info)) {
               case STB_LOCAL:  VG_(printf)("LOC "); break;
               case STB_GLOBAL: VG_(printf)("GLO "); break;
               case STB_WEAK:   VG_(printf)("WEA "); break;
               case STB_LOPROC: VG_(printf)("lop "); break;
               case STB_HIPROC: VG_(printf)("hip "); break;
               default:         VG_(printf)("??? "); break;
            }
            switch (ELF32_ST_TYPE(o_symtab[i].st_info)) {
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
                si->offset+(UChar*)o_symtab[i].st_value,
                o_symtab[i].st_size,
                o_symtab[i].st_name 
                   ? ((Char*)o_strtab+o_symtab[i].st_name) 
                   : (Char*)"NONAME");                
#           endif

            /* Figure out if we're interested in the symbol.
               Firstly, is it of the right flavour? 
            */
            snaffle_it
               =  ( (ELF32_ST_BIND(o_symtab[i].st_info) == STB_GLOBAL ||
                     ELF32_ST_BIND(o_symtab[i].st_info) == STB_LOCAL /* ||
		     ELF32_ST_BIND(o_symtab[i].st_info) == STB_WEAK */)
                    &&
                    (ELF32_ST_TYPE(o_symtab[i].st_info) == STT_FUNC /*||
                     ELF32_ST_TYPE(o_symtab[i].st_info) == STT_OBJECT*/)
                  );

            /* Secondly, if it's apparently in a GOT or PLT, it's really
               a reference to a symbol defined elsewhere, so ignore it. 
            */
            sym_addr = si->offset
                       + (UInt)o_symtab[i].st_value;
            if (o_got != NULL
                && sym_addr >= (Addr)o_got 
                && sym_addr < (Addr)(o_got+o_got_sz)) {
               snaffle_it = False;
               if (VG_(clo_trace_symtab)) {
	          VG_(printf)( "in GOT: %s\n", 
                               o_strtab+o_symtab[i].st_name);
               }
            }
            if (o_plt != NULL
                && sym_addr >= (Addr)o_plt 
                && sym_addr < (Addr)(o_plt+o_plt_sz)) {
               snaffle_it = False;
               if (VG_(clo_trace_symtab)) {
	          VG_(printf)( "in PLT: %s\n", 
                               o_strtab+o_symtab[i].st_name);
               }
            }

            /* Don't bother if nameless, or zero-sized. */
            if (snaffle_it
                && (o_symtab[i].st_name == (Elf32_Word)NULL
                    || /* VG_(strlen)(o_strtab+o_symtab[i].st_name) == 0 */
                       /* equivalent but cheaper ... */
                       * ((UChar*)(o_strtab+o_symtab[i].st_name)) == 0
                    || o_symtab[i].st_size == 0)) {
               snaffle_it = False;
               if (VG_(clo_trace_symtab)) {
	          VG_(printf)( "size=0: %s\n", 
                               o_strtab+o_symtab[i].st_name);
               }
            }

#           if 0
            /* Avoid _dl_ junk.  (Why?) */
            /* 01-02-24: disabled until I find out if it really helps. */
            if (snaffle_it
                && (VG_(strncmp)("_dl_", o_strtab+o_symtab[i].st_name, 4) == 0
                    || VG_(strncmp)("_r_debug", 
                                   o_strtab+o_symtab[i].st_name, 8) == 0)) {
               snaffle_it = False;
               if (VG_(clo_trace_symtab)) {
                  VG_(printf)( "_dl_ junk: %s\n", 
                               o_strtab+o_symtab[i].st_name);
               }
            }
#           endif

            /* This seems to significantly reduce the number of junk
               symbols, and particularly reduces the number of
               overlapping address ranges.  Don't ask me why ... */
	    if (snaffle_it && (Int)o_symtab[i].st_value == 0) {
               snaffle_it = False;
               if (VG_(clo_trace_symtab)) {
                  VG_(printf)( "valu=0: %s\n", 
                               o_strtab+o_symtab[i].st_name);
               }
            }

	    /* If no part of the symbol falls within the mapped range,
               ignore it. */
            if (sym_addr+o_symtab[i].st_size <= si->start
                || sym_addr >= si->start+si->size) {
               snaffle_it = False;
	    }

            if (snaffle_it) {
               /* it's an interesting symbol; record ("snaffle") it. */
               RiSym sym;
               Char* t0 = o_symtab[i].st_name 
                             ? (Char*)(o_strtab+o_symtab[i].st_name) 
                             : (Char*)"NONAME";
               Int nmoff = addStr ( si, t0 );
               vg_assert(nmoff >= 0 
                         /* && 0==VG_(strcmp)(t0,&vg_strtab[nmoff]) */ );
               vg_assert( (Int)o_symtab[i].st_value >= 0);
               /* VG_(printf)("%p + %d:   %s\n", si->addr, 
                              (Int)o_symtab[i].st_value, t0 ); */
               sym.addr  = sym_addr;
               sym.size  = o_symtab[i].st_size;
               sym.nmoff = nmoff;
               addSym ( si, &sym );
	    }
         }
      }
   }

   /* Reading of the stabs and/or dwarf2 debug format information, if
      any. */
   stabstr    = NULL;
   stab       = NULL;
   dwarf2     = NULL;
   stabstr_sz = 0;
   stab_sz    = 0;
   dwarf2_sz  = 0;

   /* find the .stabstr / .stab / .debug_line sections */
   for (i = 0; i < ehdr->e_shnum; i++) {
      if (0 == VG_(strcmp)(".stab",sh_strtab + shdr[i].sh_name)) {
         stab = (UChar*)(oimage + shdr[i].sh_offset);
         stab_sz = shdr[i].sh_size;
      }
      if (0 == VG_(strcmp)(".stabstr",sh_strtab + shdr[i].sh_name)) {
         stabstr = (UChar*)(oimage + shdr[i].sh_offset);
         stabstr_sz = shdr[i].sh_size;
      }
      if (0 == VG_(strcmp)(".debug_line",sh_strtab + shdr[i].sh_name)) {
         dwarf2 = (UChar *)(oimage + shdr[i].sh_offset);
	 dwarf2_sz = shdr[i].sh_size;
      }
   }

   if ((stab == NULL || stabstr == NULL) && dwarf2 == NULL) {
      vg_symerr("   object doesn't have any debug info");
      VG_(munmap) ( (void*)oimage, n_oimage );
      return;
   }

   if ( stab_sz + (UChar*)stab > n_oimage + (UChar*)oimage
        || stabstr_sz + (UChar*)stabstr 
           > n_oimage + (UChar*)oimage ) {
      vg_symerr("   ELF (stabs) debug data is beyond image end?!");
      VG_(munmap) ( (void*)oimage, n_oimage );
      return;
   }

   if ( dwarf2_sz + (UChar*)dwarf2 > n_oimage + (UChar*)oimage ) {
      vg_symerr("   ELF (dwarf2) debug data is beyond image end?!");
      VG_(munmap) ( (void*)oimage, n_oimage );
      return;
   }

   /* Looks plausible.  Go on and read debug data. */
   if (stab != NULL && stabstr != NULL) {
      read_debuginfo_stabs ( si, stab, stab_sz, stabstr, stabstr_sz );
   }

   if (dwarf2 != NULL) {
      read_debuginfo_dwarf2 ( si, dwarf2, dwarf2_sz );
   }

   /* Last, but not least, heave the oimage back overboard. */
   VG_(munmap) ( (void*)oimage, n_oimage );
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


static
void read_symtab_callback ( 
        Addr start, UInt size, 
        Char rr, Char ww, Char xx, 
        UInt foffset, UChar* filename )
{
   SegInfo* si;

   /* Stay sane ... */
   if (size == 0)
      return;

   /* We're only interested in collecting symbols in executable
      segments which are associated with a real file.  Hence: */
   if (filename == NULL || xx != 'x')
      return;
   if (0 == VG_(strcmp)(filename, "/dev/zero"))
      return;

   /* Perhaps we already have this one?  If so, skip. */
   for (si = segInfo; si != NULL; si = si->next) {
      /*
      if (0==VG_(strcmp)(si->filename, filename)) 
         VG_(printf)("same fnames: %c%c%c (%p, %d) (%p, %d) %s\n", 
                     rr,ww,xx,si->start,si->size,start,size,filename);
      */
      /* For some reason the observed size of a mapping can change, so
         we don't use that to determine uniqueness. */
      if (si->start == start
          /* && si->size == size */
          && 0==VG_(strcmp)(si->filename, filename)) {
         return;
      }
   }

   /* Get the record initialised right. */
   si = VG_(malloc)(VG_AR_SYMTAB, sizeof(SegInfo));
   si->next = segInfo;
   segInfo = si;

   si->start    = start;
   si->size     = size;
   si->foffset  = foffset;
   si->filename = VG_(malloc)(VG_AR_SYMTAB, 1 + VG_(strlen)(filename));
   VG_(strcpy)(si->filename, filename);

   si->symtab = NULL;
   si->symtab_size = si->symtab_used = 0;
   si->loctab = NULL;
   si->loctab_size = si->loctab_used = 0;
   si->strtab = NULL;
   si->strtab_size = si->strtab_used = 0;

   /* Kludge ... */
   si->offset 
      = si->start==VG_ASSUMED_EXE_BASE ? 0 : si->start;

   /* And actually fill it up. */
   if (VG_(clo_instrument) || VG_(clo_cachesim)) {
      vg_read_lib_symbols ( si );
      canonicaliseSymtab ( si );
      canonicaliseLoctab ( si );
   }
}


/* This one really is the Head Honcho.  Update the symbol tables to
   reflect the current state of /proc/self/maps.  Rather than re-read
   everything, just read the entries which are not already in segInfo.
   So we can call here repeatedly, after every mmap of a non-anonymous
   segment with execute permissions, for example, to pick up new
   libraries as they are dlopen'd.  Conversely, when the client does
   munmap(), vg_symtab_notify_munmap() throws away any symbol tables
   which happen to correspond to the munmap()d area.  */
void VG_(read_symbols) ( void )
{
   VG_(read_procselfmaps) ( read_symtab_callback );

   /* Do a sanity check on the symbol tables: ensure that the address
      space pieces they cover do not overlap (otherwise we are severely
      hosed).  This is a quadratic algorithm, but there shouldn't be
      many of them.  
   */
   { SegInfo *si, *si2;
     for (si = segInfo; si != NULL; si = si->next) {
        /* Check no overlap between *si and those in the rest of the
           list. */
        for (si2 = si->next; si2 != NULL; si2 = si2->next) {
           Addr lo = si->start;
           Addr hi = si->start + si->size - 1;
           Addr lo2 = si2->start;
           Addr hi2 = si2->start + si2->size - 1;
           Bool overlap;
           vg_assert(lo < hi);
	   vg_assert(lo2 < hi2);
           /* the main assertion */
           overlap = (lo <= lo2 && lo2 <= hi)
                      || (lo <= hi2 && hi2 <= hi);
	   if (overlap) {
              VG_(printf)("\n\nOVERLAPPING SEGMENTS\n" );
              ppSegInfo ( si );
              ppSegInfo ( si2 );
              VG_(printf)("\n\n"); 
              vg_assert(! overlap);
	   }
        }
     }
   }    
}


/* When an munmap() call happens, check to see whether it corresponds
   to a segment for a .so, and if so discard the relevant SegInfo.
   This might not be a very clever idea from the point of view of
   accuracy of error messages, but we need to do it in order to
   maintain the no-overlapping invariant.

   16 May 02: Returns a Bool indicating whether or not the discarded
   range falls inside a known executable segment.  See comment at top
   of file for why.
*/
Bool VG_(symtab_notify_munmap) ( Addr start, UInt length )
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
   if (curr == NULL) 
      return False;

   VG_(message)(Vg_UserMsg, 
                "discard syms in %s due to munmap()", 
                curr->filename ? curr->filename : (UChar*)"???");

   vg_assert(prev == NULL || prev->next == curr);

   if (prev == NULL) {
      segInfo = curr->next;
   } else {
      prev->next = curr->next;
   }

   freeSegInfo(curr);
   return True;
}


/*------------------------------------------------------------*/
/*--- Use of symbol table & location info to create        ---*/
/*--- plausible-looking stack dumps.                       ---*/
/*------------------------------------------------------------*/

/* Find a symbol-table index containing the specified pointer, or -1
   if not found.  Binary search.  */

static Int search_one_symtab ( SegInfo* si, Addr ptr )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid, 
        lo = 0, 
        hi = si->symtab_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) return -1; /* not found */
      mid      = (lo + hi) / 2;
      a_mid_lo = si->symtab[mid].addr;
      a_mid_hi = ((Addr)si->symtab[mid].addr) + si->symtab[mid].size - 1;

      if (ptr < a_mid_lo) { hi = mid-1; continue; } 
      if (ptr > a_mid_hi) { lo = mid+1; continue; }
      vg_assert(ptr >= a_mid_lo && ptr <= a_mid_hi);
      return mid;
   }
}


/* Search all symtabs that we know about to locate ptr.  If found, set
   *psi to the relevant SegInfo, and *symno to the symtab entry number
   within that.  If not found, *psi is set to NULL.  */

static void search_all_symtabs ( Addr ptr, SegInfo** psi, Int* symno )
{
   Int      sno;
   SegInfo* si;
   for (si = segInfo; si != NULL; si = si->next) {
      if (si->start <= ptr && ptr < si->start+si->size) {
         sno = search_one_symtab ( si, ptr );
         if (sno == -1) goto not_found;
         *symno = sno;
         *psi = si;
         return;
      }
   }
  not_found:
   *psi = NULL;
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
static void search_all_loctabs ( Addr ptr, SegInfo** psi, Int* locno )
{
   Int      lno;
   SegInfo* si;
   for (si = segInfo; si != NULL; si = si->next) {
      if (si->start <= ptr && ptr < si->start+si->size) {
         lno = search_one_loctab ( si, ptr );
         if (lno == -1) goto not_found;
         *locno = lno;
         *psi = si;
         return;
      }
   }
  not_found:
   *psi = NULL;
}


/* The whole point of this whole big deal: map a code address to a
   plausible symbol name.  Returns False if no idea; otherwise True.
   Caller supplies buf and nbuf.  If no_demangle is True, don't do
   demangling, regardless of vg_clo_demangle -- probably because the
   call has come from vg_what_fn_or_object_is_this. */
Bool VG_(what_fn_is_this) ( Bool no_demangle, Addr a, 
                            Char* buf, Int nbuf )
{
   SegInfo* si;
   Int      sno;
   search_all_symtabs ( a, &si, &sno );
   if (si == NULL) 
      return False;
   if (no_demangle) {
      VG_(strncpy_safely) 
         ( buf, & si->strtab[si->symtab[sno].nmoff], nbuf );
   } else {
      VG_(demangle) ( & si->strtab[si->symtab[sno].nmoff], buf, nbuf );
   }
   return True;
}


/* Map a code address to the name of a shared object file.  Returns
   False if no idea; otherwise False.  Caller supplies buf and
   nbuf. */
static
Bool vg_what_object_is_this ( Addr a, Char* buf, Int nbuf )
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

/* Return the name of an erring fn in a way which is useful
   for comparing against the contents of a suppressions file. 
   Always writes something to buf.  Also, doesn't demangle the
   name, because we want to refer to mangled names in the 
   suppressions file.
*/
void VG_(what_obj_and_fun_is_this) ( Addr a,
                                     Char* obj_buf, Int n_obj_buf,
                                     Char* fun_buf, Int n_fun_buf )
{
   (void)vg_what_object_is_this ( a, obj_buf, n_obj_buf );
   (void)VG_(what_fn_is_this) ( True, a, fun_buf, n_fun_buf );
}


/* Map a code address to a (filename, line number) pair.  
   Returns True if successful.
*/
Bool VG_(what_line_is_this)( Addr a, 
                             UChar* filename, Int n_filename, 
                             UInt* lineno )
{
   SegInfo* si;
   Int      locno;
   search_all_loctabs ( a, &si, &locno );
   if (si == NULL) 
      return False;
   VG_(strncpy_safely)(filename, & si->strtab[si->loctab[locno].fnmoff], 
                       n_filename);
   *lineno = si->loctab[locno].lineno;

   return True;
}


/* Print a mini stack dump, showing the current location. */
void VG_(mini_stack_dump) ( ExeContext* ec )
{

#define APPEND(str)                                              \
   { UChar* sss;                                                 \
     for (sss = str; n < M_VG_ERRTXT-1 && *sss != 0; n++,sss++)  \
        buf[n] = *sss;                                           \
     buf[n] = 0;                                                 \
   }

   Bool   know_fnname;
   Bool   know_objname;
   Bool   know_srcloc;
   UInt   lineno; 
   UChar  ibuf[20];
   UInt   i, n;

   UChar  buf[M_VG_ERRTXT];
   UChar  buf_fn[M_VG_ERRTXT];
   UChar  buf_obj[M_VG_ERRTXT];
   UChar  buf_srcloc[M_VG_ERRTXT];

   Int stop_at = VG_(clo_backtrace_size);

   n = 0;

   know_fnname  = VG_(what_fn_is_this)(False,ec->eips[0], buf_fn, M_VG_ERRTXT);
   know_objname = vg_what_object_is_this(ec->eips[0], buf_obj, M_VG_ERRTXT);
   know_srcloc  = VG_(what_line_is_this)(ec->eips[0], 
                                         buf_srcloc, M_VG_ERRTXT, 
                                         &lineno);

   APPEND("   at ");
   VG_(sprintf)(ibuf,"0x%x: ", ec->eips[0]);
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
   VG_(message)(Vg_UserMsg, "%s", buf);

   for (i = 1; i < stop_at && ec->eips[i] != 0; i++) {
      know_fnname  = VG_(what_fn_is_this)(False,ec->eips[i], buf_fn, M_VG_ERRTXT);
      know_objname = vg_what_object_is_this(ec->eips[i],buf_obj, M_VG_ERRTXT);
      know_srcloc  = VG_(what_line_is_this)(ec->eips[i], 
                                          buf_srcloc, M_VG_ERRTXT, 
                                          &lineno);
      n = 0;
      APPEND("   by ");
      VG_(sprintf)(ibuf,"0x%x: ",ec->eips[i]);
      APPEND(ibuf);
      if (know_fnname) { 
         APPEND(buf_fn) 
         if (!know_srcloc && know_objname) {
            APPEND(" (in ");
            APPEND(buf_obj);
            APPEND(")");
         }
      } else {
         if (know_objname && !know_srcloc) {
            APPEND("(within ");
            APPEND(buf_obj);
            APPEND(")"); 
         } else {
            APPEND("???");
         }
      };
      if (know_srcloc) {
         APPEND(" (");
         APPEND(buf_srcloc);
         APPEND(":");
         VG_(sprintf)(ibuf,"%d",lineno);
         APPEND(ibuf);
         APPEND(")");
      }
      VG_(message)(Vg_UserMsg, "%s", buf);
   }   
}

#undef APPEND

/*--------------------------------------------------------------------*/
/*--- end                                             vg_symtab2.c ---*/
/*--------------------------------------------------------------------*/
