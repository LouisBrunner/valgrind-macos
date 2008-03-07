
/*--------------------------------------------------------------------*/
/*--- Read DWARF3 ".debug_info" sections (DIE trees).              ---*/
/*---                                                 readdwarf3.c ---*/
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

/* REFERENCE (without which this code will not make much sense):

   DWARF Debugging Information Format, Version 3, 
   dated 20 December 2005 (the "D3 spec").

   Available at http://www.dwarfstd.org/Dwarf3.pdf.  There's also a
   .doc (MS Word) version, but for some reason the section numbers
   between the Word and PDF versions differ by 1 in the first digit.
   All section references in this code are to the PDF version.

   CURRENT HACKS:

   DW_TAG_{const,volatile}_type no DW_AT_type is allowed; it is
      assumed to mean "const void" or "volatile void" respectively.
      GDB appears to interpret them like this, anyway.

   In many cases it is important to know the svma of a CU (the "base
   address of the CU", as the D3 spec calls it).  There are some
   situations in which the spec implies this value is unknown, but the
   Dwarf3 produced by gcc-4.1 seems to assume is not unknown but
   merely zero when not explicitly stated.  So we too have to make
   that assumption.

   TODO, 2008 Feb 17:

   get rid of cu_svma_known and document the assumed-zero svma hack.

   ML_(sizeOfType): differentiate between zero sized types and types
   for which the size is unknown.  Is this important?  I don't know.

   DW_AT_array_types: deal with explicit sizes (currently we compute
   the size from the bounds and the element size, although that's
   fragile, if the bounds incompletely specified, or completely
   absent)

   Document reason for difference (by 1) of stack preening depth in
   parse_var_DIE vs parse_type_DIE.

   Don't hand to ML_(addVars), vars whose locations are entirely in
   registers (DW_OP_reg*).  This is merely a space-saving
   optimisation, as ML_(evaluate_Dwarf3_Expr) should handle these
   expressions correctly, by failing to evaluate them and hence
   effectively ignoring the variable with which they are associated.

   Deal with DW_AT_array_types which have element size != stride

   In some cases, the info for a variable is split between two
   different DIEs (generally a declarer and a definer).  We punt on
   these.  Could do better here.

   The 'data_bias' argument passed to the expression evaluator
   (ML_(evaluate_Dwarf3_Expr)) should really be changed to a
   MaybeUWord, to make it clear when we do vs don't know what it is
   for the evaluation of an expression.  At the moment zero is passed
   for this parameter in the don't know case.  That's a bit fragile
   and obscure; using a MaybeUWord would be clearer.

   POTENTIAL PERFORMANCE IMPROVEMENTS:

   The number of type entities that end up in the list of TyAdmins
   rapidly becomes huge (eg, for libQtGui.so.4.3.2 (amd64-linux, size
   80729047 bytes), there are 786860 entries in the list).  Mostly
   this seems to be caused by g++ adding type DIEs for all the basic
   types once for each source file contributing to the compilation
   unit, and for a large library they add up quickly.  That causes
   both a lot of work for this reader module, and also wastes vast
   amounts of memory storing this duplicated information.  We could
   surely do a lot better here.

   Handle interaction between read_DIE and parse_{var,type}_DIE
   better.  Currently read_DIE reads the entire DIE just to find where
   the end is (and for debug printing), so that it can later reliably
   move the cursor to the end regardless of what parse_{var,type}_DIE
   do.  This means many DIEs (most, even?) are read twice.  It would
   be smarter to make parse_{var,type}_DIE return a Bool indicating
   whether or not they advanced the DIE cursor, and only if they
   didn't should read_DIE itself read through the DIE.

   ML_(addVar) and add_var_to_arange: quite a lot of DiAddrRanges have
   zero variables in their .vars XArray.  Rather than have an XArray
   with zero elements (which uses 2 malloc'd blocks), allow the .vars
   pointer to be NULL in this case.

   More generally, reduce the amount of memory allocated and freed
   while reading Dwarf3 type/variable information.  Even modest (20MB)
   objects cause this module to allocate and free hundreds of
   thousands of small blocks, and ML_(arena_malloc) and its various
   groupies always show up at the top of performance profiles. */

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_options.h"
#include "pub_core_xarray.h"
#include "priv_misc.h"             /* dinfo_zalloc/free */
#include "priv_tytypes.h"
#include "priv_d3basics.h"
#include "priv_storage.h"
#include "priv_readdwarf3.h"       /* self */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Basic machinery for parsing DIEs.                    ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

#define TRACE_D3(format, args...) \
   if (td3) { VG_(printf)(format, ## args); }

#define D3_INVALID_CUOFF  ((void*)(-1UL))
#define D3_FAKEVOID_CUOFF ((void*)(-2UL))

typedef
   struct {
      UChar* region_start_img;
      UWord  region_szB;
      UWord  region_next;
      void (*barf)( HChar* ) __attribute__((noreturn));
      HChar* barfstr;
   }
   Cursor;

static inline Bool is_sane_Cursor ( Cursor* c ) {
   if (!c)                return False;
   if (!c->barf)          return False;
   if (!c->barfstr)       return False;
   return True;
}

static void init_Cursor ( Cursor* c,
                          UChar*  region_start_img,
                          UWord   region_szB,
                          UWord   region_next,
                          __attribute__((noreturn)) void (*barf)( HChar* ),
                          HChar*  barfstr )
{
   vg_assert(c);
   VG_(memset)(c, 0, sizeof(*c));
   c->region_start_img = region_start_img;
   c->region_szB       = region_szB;
   c->region_next      = region_next;
   c->barf             = barf;
   c->barfstr          = barfstr;
   vg_assert(is_sane_Cursor(c));
}

static Bool is_at_end_Cursor ( Cursor* c ) {
   vg_assert(is_sane_Cursor(c));
   return c->region_next >= c->region_szB;
}

static inline UWord get_position_of_Cursor ( Cursor* c ) {
   vg_assert(is_sane_Cursor(c));
   return c->region_next;
}
static inline void set_position_of_Cursor ( Cursor* c, UWord pos ) {
   c->region_next = pos;
   vg_assert(is_sane_Cursor(c));
}

static /*signed*/Word get_remaining_length_Cursor ( Cursor* c ) {
   vg_assert(is_sane_Cursor(c));
   return c->region_szB - c->region_next;
}

static UChar* get_address_of_Cursor ( Cursor* c ) {
   vg_assert(is_sane_Cursor(c));
   return &c->region_start_img[ c->region_next ];
}

__attribute__((noreturn)) 
static void failWith ( Cursor* c, HChar* str ) {
   vg_assert(c);
   vg_assert(c->barf);
   c->barf(str);
   /*NOTREACHED*/
   vg_assert(0);
}

/* FIXME: document assumptions on endianness for
   get_UShort/UInt/ULong. */
static inline UChar get_UChar ( Cursor* c ) {
   UChar r;
   /* vg_assert(is_sane_Cursor(c)); */
   if (c->region_next + sizeof(UChar) > c->region_szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   r = * (UChar*) &c->region_start_img[ c->region_next ];
   c->region_next += sizeof(UChar);
   return r;
}
static UShort get_UShort ( Cursor* c ) {
   UShort r;
   vg_assert(is_sane_Cursor(c));
   if (c->region_next + sizeof(UShort) > c->region_szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   r = * (UShort*) &c->region_start_img[ c->region_next ];
   c->region_next += sizeof(UShort);
   return r;
}
static UInt get_UInt ( Cursor* c ) {
   UInt r;
   vg_assert(is_sane_Cursor(c));
   if (c->region_next + sizeof(UInt) > c->region_szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   r = * (UInt*) &c->region_start_img[ c->region_next ];
   c->region_next += sizeof(UInt);
   return r;
}
static ULong get_ULong ( Cursor* c ) {
   ULong r;
   vg_assert(is_sane_Cursor(c));
   if (c->region_next + sizeof(ULong) > c->region_szB) {
      c->barf(c->barfstr);
      /*NOTREACHED*/
      vg_assert(0);
   }
   r = * (ULong*) &c->region_start_img[ c->region_next ];
   c->region_next += sizeof(ULong);
   return r;
}
static inline ULong get_ULEB128 ( Cursor* c ) {
   ULong result;
   Int   shift;
   UChar byte;
   /* unroll first iteration */
   byte = get_UChar( c );
   result = (ULong)(byte & 0x7f);
   if (LIKELY(!(byte & 0x80))) return result;
   shift = 7;
   /* end unroll first iteration */
   do {
      byte = get_UChar( c );
      result |= ((ULong)(byte & 0x7f)) << shift;
      shift += 7;
   } while (byte & 0x80);
   return result;
}
static Long get_SLEB128 ( Cursor* c ) {
   ULong  result = 0;
   Int    shift = 0;
   UChar  byte;
   do {
      byte = get_UChar(c);
      result |= ((ULong)(byte & 0x7f)) << shift;
      shift += 7;
   } while (byte & 0x80);
   if (shift < 64 && (byte & 0x40))
      result |= -(1ULL << shift);
   return result;
}

/* Assume 'c' points to the start of a string.  Return the absolute
   address of whatever it points at, and advance it past the
   terminating zero.  This makes it safe for the caller to then copy
   the string with ML_(addStr), since (w.r.t. image overruns) the
   process of advancing past the terminating zero will already have
   "vetted" the string. */
static UChar* get_AsciiZ ( Cursor* c ) {
   UChar  uc;
   UChar* res = get_address_of_Cursor(c);
   do { uc = get_UChar(c); } while (uc != 0);
   return res;
}

static ULong peek_ULEB128 ( Cursor* c ) {
   Word here = c->region_next;
   ULong r = get_ULEB128( c );
   c->region_next = here;
   return r;
}
static UChar peek_UChar ( Cursor* c ) {
   Word here = c->region_next;
   UChar r = get_UChar( c );
   c->region_next = here;
   return r;
}

static ULong get_Dwarfish_UWord ( Cursor* c, Bool is_dw64 ) {
   return is_dw64 ? get_ULong(c) : (ULong) get_UInt(c);
}

static UWord get_UWord ( Cursor* c ) {
   vg_assert(sizeof(UWord) == sizeof(void*));
   if (sizeof(UWord) == 4) return get_UInt(c);
   if (sizeof(UWord) == 8) return get_ULong(c);
   vg_assert(0);
}


/* Read a DWARF3 'Initial Length' field */
static ULong get_Initial_Length ( /*OUT*/Bool* is64,
                                  Cursor* c, 
                                  HChar* barfMsg )
{
   ULong w64;
   UInt  w32;
   *is64 = False;
   w32 = get_UInt( c );
   if (w32 >= 0xFFFFFFF0 && w32 < 0xFFFFFFFF) {
      c->barf( barfMsg );
   }
   else if (w32 == 0xFFFFFFFF) {
      *is64 = True;
      w64   = get_ULong( c );
   } else {
      *is64 = False;
      w64 = (ULong)w32;
   }
   return w64;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- "CUConst" structure                                  ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

#define N_ABBV_CACHE 32

/* Holds information that is constant through the parsing of a
   Compilation Unit.  This is basically plumbed through to
   everywhere. */
typedef
   struct {
      /* Call here if anything goes wrong */
      void (*barf)( HChar* ) __attribute__((noreturn));
      /* Is this 64-bit DWARF ? */
      Bool   is_dw64;
      /* Which DWARF version ?  (2 or 3) */
      UShort version;
      /* Length of this Compilation Unit, excluding its Header */
      ULong  unit_length;
      /* Offset of start of this unit in .debug_info */
      UWord  cu_start_offset;
      /* SVMA for this CU.  In the D3 spec, is known as the "base
         address of the compilation unit (last para sec 3.1.1).
         Needed for (amongst things) interpretation of location-list
         values. */
      Addr   cu_svma;
      Bool   cu_svma_known;
      /* The debug_abbreviations table to be used for this Unit */
      UChar* debug_abbv;
      /* Upper bound on size thereof (an overestimate, in general) */
      UWord  debug_abbv_maxszB;
      /* Where is .debug_str ? */
      UChar* debug_str_img;
      UWord  debug_str_sz;
      /* Where is .debug_ranges ? */
      UChar* debug_ranges_img;
      UWord  debug_ranges_sz;
      /* Where is .debug_loc ? */
      UChar* debug_loc_img;
      UWord  debug_loc_sz;
      /* Where is .debug_line? */
      UChar* debug_line_img;
      UWord  debug_line_sz;
      /* --- Needed so we can add stuff to the string table. --- */
      struct _DebugInfo* di;
      /* --- a cache for set_abbv_Cursor --- */
      /* abbv_code == (ULong)-1 for an unused entry. */
      struct { ULong abbv_code; UWord posn; } saC_cache[N_ABBV_CACHE];
      UWord saC_cache_queries;
      UWord saC_cache_misses;
   }
   CUConst;


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Helper functions for Guarded Expressions             ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Parse the location list starting at img-offset 'debug_loc_offset'
   in .debug_loc.  Results are biased with 'svma_of_referencing_CU'
   and so I believe are correct SVMAs for the object as a whole.  This
   function allocates the UChar*, and the caller must deallocate it.
   The resulting block is in so-called Guarded-Expression format.

   Guarded-Expression format is similar but not identical to the DWARF3
   location-list format.  The format of each returned block is:

      UChar biasMe;
      UChar isEnd;
      followed by zero or more of

      (Addr aMin;  Addr aMax;  UShort nbytes;  ..bytes..;  UChar isEnd)

   '..bytes..' is an standard DWARF3 location expression which is
   valid when aMin <= pc <= aMax (possibly after suitable biasing).

   The number of bytes in '..bytes..' is nbytes.

   The end of the sequence is marked by an isEnd == 1 value.  All
   previous isEnd values must be zero.

   biasMe is 1 if the aMin/aMax fields need this DebugInfo's
   text_bias added before use, and 0 if the GX is this is not
   necessary (is ready to go).

   Hence the block can be quickly parsed and is self-describing.  Note
   that aMax is 1 less than the corresponding value in a DWARF3
   location list.  Zero length ranges, with aMax == aMin-1, are not
   allowed.
*/
void ML_(pp_GX) ( GExpr* gx ) {
   Addr   aMin, aMax;
   UChar  uc;
   UShort nbytes;
   UChar* p = &gx->payload[0];
   uc = *p++;
   VG_(printf)("GX(%s){", uc == 0 ? "final" : "Breqd" );
   vg_assert(uc == 0 || uc == 1);
   while (True) {
      uc = *p++;
      if (uc == 1)
         break; /*isEnd*/
      vg_assert(uc == 0);
      aMin   = * (Addr*)p;  p += sizeof(Addr);
      aMax   = * (Addr*)p;  p += sizeof(Addr);
      nbytes = * (UShort*)p; p += sizeof(UShort);
      VG_(printf)("[%p,%p]=", aMin, aMax);
      while (nbytes > 0) {
         VG_(printf)("%02x", (UInt)*p++);
         nbytes--;
      }
      if (*p == 0)
         VG_(printf)(",");
   }
   VG_(printf)("}");
}

/* "Comment_Regarding_DWARF3_Text_Biasing" (is referred to elsewhere)
    -----------------------------------------------------------------
    apply_kludgey_text_bias() is our mechanism for biasing text
    addresses found in DWARF3 .debug_info, .debug_ranges, .debug_loc
    sections.  This is a nasty and unprincipled hack.

    Biasing the text svmas, so as to obtain text avmas, should be
    straightforward, right?  We just add on di->text_bias, as
    carefully computed by readelf.c.

    That works OK most of the time.  But in the following case it fails:
    1. The object is made in the usual way (gcc -g, etc)
    2. The DWARF3 stuff removed from it and parked in a .debuginfo object
    3. The remaining (base) object is then prelinked.

    Prelinking changes the text svmas throughout an object by some
    constant amount, including the DWARF3 stuff.  So if the DWARF3
    stuff remains attached to the original object, then there is no
    problem.  However, if the DWARF3 stuff is detached, and the
    remaining object is prelinked and the debuginfo object isn't, then
    we have a problem: the text bias computed for the main object
    isn't correct for the debuginfo object.

    So the following kludged is used to bias text svmas.

    1. First, try with the text bias computed for the main object.  If
       that gives an avma inside the area in which the text segment is
       known to have been mapped, then all well and good.

    2. If not, try using the avma of the text mapped area as a bias.
       Again, if that works out, fine.  This is the heart of the
       kludge.  It implicitly treats the svma-s to be biased as if
       they had been prelinked to zero.

    3. If even that doesn't work, just return the avma unchanged.

    For each object/object-pair, we count the number of times each
    case occurs.  We flag an error (which the user gets to see) if (3)
    ever occurs, or if a mixture of (1) and (2) occurs.  That should
    at least catch the most obvious snafus.

    Caveats: the main remaining worry is whether this problem somehow
    also affects the data-biasing done for case DW_OP_addr in
    ML_(evaluate_Dwarf3_Expr) in d3basics.c.  This is currently
    unknown.

    Possible sources of info: canonical description seems to be:

       http://people.redhat.com/jakub/prelink.pdf

    See para at line 337 starting "DWARF 2 debugging information ..."

    This thread looks like the gdb people hitting the same issue:

       http://sourceware.org/ml/gdb-patches/2007-01/msg00278.html
*/
typedef
   struct {
      /* FIXED */
      Addr  rx_map_avma;
      SizeT rx_map_size;
      OffT  text_bias;
      /* VARIABLE -- count stats */
      UWord n_straightforward_biasings;
      UWord n_kludgey_biasings;
      UWord n_failed_biasings;
   }
   KludgeyTextBiaser;

static Addr apply_kludgey_text_bias ( KludgeyTextBiaser* ktb,
                                      Addr allegedly_text_svma ) {
   Addr res;
   res = allegedly_text_svma + ktb->text_bias;
   if (res >= ktb->rx_map_avma 
       && res < ktb->rx_map_avma + ktb->rx_map_size) {
      ktb->n_straightforward_biasings++;
      return res;
   }
   res = allegedly_text_svma + ktb->rx_map_avma;
   if (res >= ktb->rx_map_avma 
       && res < ktb->rx_map_avma + ktb->rx_map_size) {
      ktb->n_kludgey_biasings++;
      return res;
   }
   ktb->n_failed_biasings++;
   return allegedly_text_svma; /* this svma is a luzer */
}


/* Apply a text bias to a GX.  Kludgily :-( */
static void bias_GX ( /*MOD*/GExpr* gx, KludgeyTextBiaser* ktb )
{
   UShort nbytes;
   Addr*  pA;
   UChar* p = &gx->payload[0];
   UChar  uc;
   uc = *p++; /*biasMe*/
   if (uc == 0)
      return;
   vg_assert(uc == 1);
   p[-1] = 0; /* mark it as done */
   while (True) {
      uc = *p++;
      if (uc == 1)
         break; /*isEnd*/
      vg_assert(uc == 0);
      /* t-bias aMin */
      pA = (Addr*)p;
      *pA = apply_kludgey_text_bias( ktb, *pA );
      p += sizeof(Addr);
      /* t-bias aMax */
      pA = (Addr*)p;
      *pA = apply_kludgey_text_bias( ktb, *pA );
      p += sizeof(Addr);
      /* nbytes, and actual expression */
      nbytes = * (UShort*)p; p += sizeof(UShort);
      p += nbytes;
   }
}

__attribute__((noinline))
static GExpr* make_singleton_GX ( UChar* block, UWord nbytes )
{
   SizeT  bytesReqd;
   GExpr* gx;
   UChar *p, *pstart;

   vg_assert(sizeof(UWord) == sizeof(Addr));
   vg_assert(nbytes <= 0xFFFF); /* else we overflow the nbytes field */
   bytesReqd
      =   sizeof(UChar)  /*biasMe*/    + sizeof(UChar) /*!isEnd*/
        + sizeof(UWord)  /*aMin*/      + sizeof(UWord) /*aMax*/
        + sizeof(UShort) /*nbytes*/    + nbytes
        + sizeof(UChar); /*isEnd*/

   gx = ML_(dinfo_zalloc)( sizeof(GExpr) + bytesReqd );
   vg_assert(gx);

   p = pstart = &gx->payload[0];

   * ((UChar*)p)  = 0;          /*biasMe*/ p += sizeof(UChar);
   * ((UChar*)p)  = 0;          /*!isEnd*/ p += sizeof(UChar);
   * ((Addr*)p)   = 0;          /*aMin*/   p += sizeof(Addr);
   * ((Addr*)p)   = ~((Addr)0); /*aMax */  p += sizeof(Addr);
   * ((UShort*)p) = (UShort)nbytes; /*nbytes*/ p += sizeof(UShort);
   VG_(memcpy)(p, block, nbytes); p += nbytes;
   * ((UChar*)p)  = 1;          /*isEnd*/  p += sizeof(UChar);

   vg_assert( (SizeT)(p - pstart) == bytesReqd);
   vg_assert( &gx->payload[bytesReqd] 
              == ((UChar*)gx) + sizeof(GExpr) + bytesReqd );

   gx->next = NULL;
   return gx;
}

__attribute__((noinline))
static GExpr* make_general_GX ( CUConst* cc,
                                Bool     td3,
                                UWord    debug_loc_offset,
                                Addr     svma_of_referencing_CU )
{
   Addr      base;
   Cursor    loc;
   XArray*   xa; /* XArray of UChar */
   GExpr*    gx;
   Word      nbytes;

   vg_assert(sizeof(UWord) == sizeof(Addr));
   if (cc->debug_loc_sz == 0)
      cc->barf("make_general_GX: .debug_loc is empty/missing");

   init_Cursor( &loc, cc->debug_loc_img, 
                cc->debug_loc_sz, 0, cc->barf,
                "Overrun whilst reading .debug_loc section(2)" );
   set_position_of_Cursor( &loc, debug_loc_offset );

   TRACE_D3("make_general_GX (.debug_loc_offset = %lu, img = %p) {\n",
            debug_loc_offset, get_address_of_Cursor( &loc ) );

   /* Who frees this xa?  It is freed before this fn exits. */
   xa = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                    sizeof(UChar) );

   { UChar c = 1; /*biasMe*/ VG_(addBytesToXA)( xa, &c, sizeof(c) ); }

   base = 0;
   while (True) {
      Bool  acquire;
      UWord len;
      /* Read a (host-)word pair.  This is something of a hack since
         the word size to read is really dictated by the ELF file;
         however, we assume we're reading a file with the same
         word-sizeness as the host.  Reasonably enough. */
      UWord w1 = get_UWord( &loc );
      UWord w2 = get_UWord( &loc );

      TRACE_D3("   %08lx %08lx\n", w1, w2);
      if (w1 == 0 && w2 == 0)
         break; /* end of list */

      if (w1 == -1UL) {
         /* new value for 'base' */
         base = w2;
         continue;
      }

      /* else a location expression follows */
      /* else enumerate [w1+base, w2+base) */
      /* w2 is 1 past end of range, as per D3 defn for "DW_AT_high_pc"
         (sec 2.17.2) */
      if (w1 > w2) {
         TRACE_D3("negative range is for .debug_loc expr at "
                  "file offset %lu\n", 
                  debug_loc_offset);
         cc->barf( "negative range in .debug_loc section" );
      }

      /* ignore zero length ranges */
      acquire = w1 < w2;
      len     = (UWord)get_UShort( &loc );

      if (acquire) {
         UWord  w;
         UShort s;
         UChar  c;
         c = 0; /* !isEnd*/
         VG_(addBytesToXA)( xa, &c, sizeof(c) );
         w = w1    + base + svma_of_referencing_CU;
         VG_(addBytesToXA)( xa, &w, sizeof(w) );
         w = w2 -1 + base + svma_of_referencing_CU;
         VG_(addBytesToXA)( xa, &w, sizeof(w) );
         s = (UShort)len;
         VG_(addBytesToXA)( xa, &s, sizeof(s) );
      }

      while (len > 0) {
         UChar byte = get_UChar( &loc );
         TRACE_D3("%02x", (UInt)byte);
         if (acquire)
            VG_(addBytesToXA)( xa, &byte, 1 );
         len--;
      }
      TRACE_D3("\n");
   }

   { UChar c = 1; /*isEnd*/ VG_(addBytesToXA)( xa, &c, sizeof(c) ); }

   nbytes = VG_(sizeXA)( xa );
   vg_assert(nbytes >= 1);

   gx = ML_(dinfo_zalloc)( sizeof(GExpr) + nbytes );
   vg_assert(gx);
   VG_(memcpy)( &gx->payload[0], (UChar*)VG_(indexXA)(xa,0), nbytes );
   vg_assert( &gx->payload[nbytes] 
              == ((UChar*)gx) + sizeof(GExpr) + nbytes );

   VG_(deleteXA)( xa );

   gx->next = NULL;

   TRACE_D3("}\n");

   return gx;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Helper functions for range lists and CU headers      ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* Denotes an address range.  Both aMin and aMax are included in the
   range; hence a complete range is (0, ~0) and an empty range is any
   (X, X-1) for X > 0.*/
typedef 
   struct { Addr aMin; Addr aMax; }
   AddrRange;


__attribute__((noinline))
static XArray* /* of AddrRange */ empty_range_list ( void )
{
   XArray* xa; /* XArray of AddrRange */
   /* Who frees this xa?  varstack_preen() does. */
   xa = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                    sizeof(AddrRange) );
   return xa;
}


static XArray* unitary_range_list ( Addr aMin, Addr aMax )
{
   XArray*   xa;
   AddrRange pair;
   vg_assert(aMin <= aMax);
   /* Who frees this xa?  varstack_preen() does. */
   xa = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                    sizeof(AddrRange) );
   pair.aMin = aMin;
   pair.aMax = aMax;
   VG_(addToXA)( xa, &pair );
   return xa;
}


/* Enumerate the address ranges starting at img-offset
   'debug_ranges_offset' in .debug_ranges.  Results are biased with
   'svma_of_referencing_CU' and so I believe are correct SVMAs for the
   object as a whole.  This function allocates the XArray, and the
   caller must deallocate it. */
__attribute__((noinline))
static XArray* /* of AddrRange */
       get_range_list ( CUConst* cc,
                        Bool     td3,
                        UWord    debug_ranges_offset,
                        Addr     svma_of_referencing_CU )
{
   Addr      base;
   Cursor    ranges;
   XArray*   xa; /* XArray of AddrRange */
   AddrRange pair;

   if (cc->debug_ranges_sz == 0)
      cc->barf("get_range_list: .debug_ranges is empty/missing");

   init_Cursor( &ranges, cc->debug_ranges_img, 
                cc->debug_ranges_sz, 0, cc->barf,
                "Overrun whilst reading .debug_ranges section(2)" );
   set_position_of_Cursor( &ranges, debug_ranges_offset );

   /* Who frees this xa?  varstack_preen() does. */
   xa = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                    sizeof(AddrRange) );
   base = 0;
   while (True) {
      /* Read a (host-)word pair.  This is something of a hack since
         the word size to read is really dictated by the ELF file;
         however, we assume we're reading a file with the same
         word-sizeness as the host.  Reasonably enough. */
      UWord w1 = get_UWord( &ranges );
      UWord w2 = get_UWord( &ranges );

      if (w1 == 0 && w2 == 0)
         break; /* end of list. */

      if (w1 == -1UL) {
         /* new value for 'base' */
         base = w2;
         continue;
      }

      /* else enumerate [w1+base, w2+base) */
      /* w2 is 1 past end of range, as per D3 defn for "DW_AT_high_pc"
         (sec 2.17.2) */
      if (w1 > w2)
         cc->barf( "negative range in .debug_ranges section" );
      if (w1 < w2) {
         pair.aMin = w1     + base + svma_of_referencing_CU;
         pair.aMax = w2 - 1 + base + svma_of_referencing_CU;
         vg_assert(pair.aMin <= pair.aMax);
         VG_(addToXA)( xa, &pair );
      }
   }
   return xa;
}


/* Parse the Compilation Unit header indicated at 'c' and 
   initialise 'cc' accordingly. */
static __attribute__((noinline))
void parse_CU_Header ( /*OUT*/CUConst* cc,
                       Bool td3,
                       Cursor* c, 
                       UChar* debug_abbv_img, UWord debug_abbv_sz )
{
   UChar  address_size;
   UWord  debug_abbrev_offset;
   Int    i;

   VG_(memset)(cc, 0, sizeof(*cc));
   vg_assert(c && c->barf);
   cc->barf = c->barf;

   /* initial_length field */
   cc->unit_length 
      = get_Initial_Length( &cc->is_dw64, c, 
           "parse_CU_Header: invalid initial-length field" );

   TRACE_D3("   Length:        %lld\n", cc->unit_length );

   /* version */
   cc->version = get_UShort( c );
   if (cc->version != 2 && cc->version != 3)
      cc->barf( "parse_CU_Header: is neither DWARF2 nor DWARF3" );
   TRACE_D3("   Version:       %d\n", (Int)cc->version );

   /* debug_abbrev_offset */
   debug_abbrev_offset = get_Dwarfish_UWord( c, cc->is_dw64 );
   if (debug_abbrev_offset >= debug_abbv_sz)
      cc->barf( "parse_CU_Header: invalid debug_abbrev_offset" );
   TRACE_D3("   Abbrev Offset: %ld\n", debug_abbrev_offset );

   /* address size.  If this isn't equal to the host word size, just
      give up.  This makes it safe to assume elsewhere that
      DW_FORM_addr can be treated as a host word. */
   address_size = get_UChar( c );
   if (address_size != sizeof(void*))
      cc->barf( "parse_CU_Header: invalid address_size" );
   TRACE_D3("   Pointer Size:  %d\n", (Int)address_size );

   /* Set up so that cc->debug_abbv points to the relevant table for
      this CU.  Set the szB so that at least we can't read off the end
      of the debug_abbrev section -- potentially (and quite likely)
      too big, if this isn't the last table in the section, but at
      least it's safe. */
   cc->debug_abbv        = debug_abbv_img + debug_abbrev_offset;
   cc->debug_abbv_maxszB = debug_abbv_sz  - debug_abbrev_offset;
   /* and empty out the set_abbv_Cursor cache */
   if (0) VG_(printf)("XXXXXX initialise set_abbv_Cursor cache\n");
   for (i = 0; i < N_ABBV_CACHE; i++) {
      cc->saC_cache[i].abbv_code = (ULong)-1; /* unused */
      cc->saC_cache[i].posn = 0;
   }
   cc->saC_cache_queries = 0;
   cc->saC_cache_misses = 0;
}


/* Set up 'c' so it is ready to parse the abbv table entry code
   'abbv_code' for this compilation unit.  */
static __attribute__((noinline))
void set_abbv_Cursor ( /*OUT*/Cursor* c, Bool td3,
                       CUConst* cc, ULong abbv_code )
{
   Int   i;
   ULong acode;

   if (abbv_code == 0)
      cc->barf("set_abbv_Cursor: abbv_code == 0" );

   /* (ULong)-1 is used to represent an empty cache slot.  So we can't
      allow it.  In any case no valid DWARF3 should make a reference
      to a negative abbreviation code.  [at least, they always seem to
      be numbered upwards from zero as far as I have seen] */
   vg_assert(abbv_code != (ULong)-1);

   /* First search the cache. */
   if (0) VG_(printf)("XXXXXX search set_abbv_Cursor cache\n");
   cc->saC_cache_queries++;
   for (i = 0; i < N_ABBV_CACHE; i++) {
      /* No need to test the cached abbv_codes for -1 (empty), since
         we just asserted that abbv_code is not -1. */
     if (cc->saC_cache[i].abbv_code == abbv_code) {
        /* Found it.  Cool.  Set up the parser using the cached
           position, and move this cache entry 1 step closer to the
           front. */
        if (0) VG_(printf)("XXXXXX found in set_abbv_Cursor cache\n");
        init_Cursor( c, cc->debug_abbv,
                     cc->debug_abbv_maxszB, cc->saC_cache[i].posn, 
                     cc->barf,
                     "Overrun whilst parsing .debug_abbrev section(1)" );
        if (i > 0) {
           ULong t_abbv_code = cc->saC_cache[i].abbv_code;
           UWord t_posn = cc->saC_cache[i].posn;
           while (i > 0) {
              cc->saC_cache[i] = cc->saC_cache[i-1];
              cc->saC_cache[0].abbv_code = t_abbv_code;
              cc->saC_cache[0].posn = t_posn;
              i--;
           }
        }
        return;
     }
   }

   /* No.  It's not in the cache.  We have to search through
      .debug_abbrev, of course taking care to update the cache
      when done. */

   cc->saC_cache_misses++;
   init_Cursor( c, cc->debug_abbv, cc->debug_abbv_maxszB, 0, cc->barf,
               "Overrun whilst parsing .debug_abbrev section(2)" );

   /* Now iterate though the table until we find the requested
      entry. */
   while (True) {
      ULong atag;
      UInt  has_children;
      acode = get_ULEB128( c );
      if (acode == 0) break; /* end of the table */
      if (acode == abbv_code) break; /* found it */
      atag         = get_ULEB128( c );
      has_children = get_UChar( c );
      //TRACE_D3("   %llu      %s    [%s]\n", 
      //         acode, pp_DW_TAG(atag), pp_DW_children(has_children));
      while (True) {
         ULong at_name = get_ULEB128( c );
         ULong at_form = get_ULEB128( c );
         if (at_name == 0 && at_form == 0) break;
         //TRACE_D3("    %18s %s\n", 
         //         pp_DW_AT(at_name), pp_DW_FORM(at_form));
      }
   }

   if (acode == 0) {
      /* Not found.  This is fatal. */
      cc->barf("set_abbv_Cursor: abbv_code not found");
   }

   /* Otherwise, 'c' is now set correctly to parse the relevant entry,
      starting from the abbreviation entry's tag.  So just cache
      the result, and return. */
   for (i = N_ABBV_CACHE-1; i > N_ABBV_CACHE/2; i--) {
      cc->saC_cache[i] = cc->saC_cache[i-1];
   }
   if (0) VG_(printf)("XXXXXX update set_abbv_Cursor cache\n");
   cc->saC_cache[N_ABBV_CACHE/2].abbv_code = abbv_code;
   cc->saC_cache[N_ABBV_CACHE/2].posn = get_position_of_Cursor(c);
}


/* From 'c', get the Form data into the lowest 1/2/4/8 bytes of *cts.

   If *cts itself contains the entire result, then *ctsSzB is set to
   1,2,4 or 8 accordingly and *ctsMemSzB is set to zero.

   Alternatively, the result can be a block of data (in the
   transiently mapped-in object, so-called "image" space).  If so then
   the lowest sizeof(void*)/8 bytes of *cts hold a pointer to said
   image, *ctsSzB is zero, and *ctsMemSzB is the size of the block.

   Unfortunately this means it is impossible to represent a zero-size
   image block since that would have *ctsSzB == 0 and *ctsMemSzB == 0
   and so is ambiguous (which case it is?)

   Invariant on successful return: 
      (*ctsSzB > 0 && *ctsMemSzB == 0)
      || (*ctsSzB == 0 && *ctsMemSzB > 0)
*/
static
void get_Form_contents ( /*OUT*/ULong* cts,
                         /*OUT*/Int*   ctsSzB,
                         /*OUT*/UWord* ctsMemSzB,
                         CUConst* cc, Cursor* c,
                         Bool td3, DW_FORM form )
{
   *cts       = 0;
   *ctsSzB    = 0;
   *ctsMemSzB = 0;
   switch (form) {
      case DW_FORM_data1:
         *cts = (ULong)(UChar)get_UChar(c);
         *ctsSzB = 1;
         TRACE_D3("%u", (UInt)*cts);
         break;
      case DW_FORM_data2:
         *cts = (ULong)(UShort)get_UShort(c);
         *ctsSzB = 2;
         TRACE_D3("%u", (UInt)*cts);
         break;
      case DW_FORM_data4:
         *cts = (ULong)(UInt)get_UInt(c);
         *ctsSzB = 4;
         TRACE_D3("%u", (UInt)*cts);
         break;
      case DW_FORM_data8:
         *cts = get_ULong(c);
         *ctsSzB = 8;
         TRACE_D3("%llu", *cts);
         break;
      case DW_FORM_sdata:
         *cts = (ULong)(Long)get_SLEB128(c);
         *ctsSzB = 8;
         TRACE_D3("%lld", (Long)*cts);
         break;
      case DW_FORM_addr:
         /* note, this is a hack.  DW_FORM_addr is defined as getting
            a word the size of the target machine as defined by the
            address_size field in the CU Header.  However,
            parse_CU_Header() rejects all inputs except those for
            which address_size == sizeof(Word), hence we can just
            treat it as a (host) Word.  */
         *cts = (ULong)(UWord)get_UWord(c);
         *ctsSzB = sizeof(UWord);
         TRACE_D3("0x%lx", (UWord)*cts);
         break;
      case DW_FORM_strp: {
         /* this is an offset into .debug_str */
         UChar* str;
         UWord uw = (UWord)get_Dwarfish_UWord( c, cc->is_dw64 );
         if (cc->debug_str_img == NULL || uw >= cc->debug_str_sz)
            cc->barf("read_and_show_Form: DW_FORM_strp "
                     "points outside .debug_str");
         /* FIXME: check the entire string lies inside debug_str,
            not just the first byte of it. */
         str = (UChar*)cc->debug_str_img + uw;
         TRACE_D3("(indirect string, offset: 0x%lx): %s", uw, str);
         *cts = (ULong)(UWord)str;
         *ctsMemSzB = 1 + (ULong)VG_(strlen)(str);
         break;
      }
      case DW_FORM_string: {
         UChar* str = get_AsciiZ(c);
         TRACE_D3("%s", str);
         *cts = (ULong)(UWord)str;
         /* strlen is safe because get_AsciiZ already 'vetted' the
            entire string */
         *ctsMemSzB = 1 + (ULong)VG_(strlen)(str);
         break;
      }
      case DW_FORM_ref4: {
         UInt  u32 = get_UInt(c);
         UWord res = cc->cu_start_offset + (UWord)u32;
         *cts = (ULong)res;
         *ctsSzB = sizeof(UWord);
         TRACE_D3("<%lx>", res);
         break;
      }
      case DW_FORM_flag: {
         UChar u8 = get_UChar(c);
         TRACE_D3("%u", (UInt)u8);
         *cts = (ULong)u8;
         *ctsSzB = 1;
         break;
      }
      case DW_FORM_block1: {
         ULong  u64b;
         ULong  u64 = (ULong)get_UChar(c);
         UChar* block = get_address_of_Cursor(c);
         TRACE_D3("%llu byte block: ", u64);
         for (u64b = u64; u64b > 0; u64b--) {
            UChar u8 = get_UChar(c);
            TRACE_D3("%x ", (UInt)u8);
         }
         *cts = (ULong)(UWord)block;
         *ctsMemSzB = (UWord)u64;
         break;
      }
      default:
         VG_(printf)("get_Form_contents: unhandled %lld (%s)\n",
                     form, ML_(pp_DW_FORM)(form));
         c->barf("get_Form_contents: unhandled DW_FORM");
   }
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Parsing of variable-related DIEs                     ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

typedef
   struct _TempVar {
      struct _TempVar* next;
      UChar*  name; /* in DebugInfo's .strchunks */
      /* Represent ranges economically.  nRanges is the number of
         ranges.  Cases:
         0: .rngOneMin .rngOneMax .manyRanges are all zero
         1: .rngOneMin .rngOneMax hold the range; .rngMany is NULL
         2: .rngOneMin .rngOneMax are zero; .rngMany holds the ranges.
         This is merely an optimisation to avoid having to allocate
         and free the XArray in the common (98%) of cases where there
         is zero or one address ranges. */
      UWord   nRanges;
      Addr    rngOneMin;
      Addr    rngOneMax;
      XArray* rngMany; /* of AddrRange.  UNIQUE PTR in AR_DINFO. */
      /* --- */
      Int     level;
      Type*   typeR;
      GExpr*  gexpr; /* for this variable */
      GExpr*  fbGX;  /* to find the frame base of the enclosing fn, if
                        any */
      UChar*  fName; /* declaring file name, or NULL */
      Int     fLine; /* declaring file line number, or zero */
      /* offset in .debug_info, so that abstract instances can be
         found to satisfy references from concrete instances. */
      UWord   dioff;
      UWord   absOri; /* so the absOri fields refer to dioff fields
                         in some other, related TempVar. */
   }
   TempVar;

#define N_D3_VAR_STACK 24

typedef
   struct {
      /* Contains the range stack: a stack of address ranges, one
         stack entry for each nested scope.  

         Some scope entries are created by function definitions
         (DW_AT_subprogram), and for those, we also note the GExpr
         derived from its DW_AT_frame_base attribute, if any.
         Consequently it should be possible to find, for any
         variable's DIE, the GExpr for the the containing function's
         DW_AT_frame_base by scanning back through the stack to find
         the nearest entry associated with a function.  This somewhat
         elaborate scheme is provided so as to make it possible to
         obtain the correct DW_AT_frame_base expression even in the
         presence of nested functions (or to be more precise, in the
         presence of nested DW_AT_subprogram DIEs). 
      */
      Int     sp; /* [sp] is innermost active entry; sp==-1 for empty
                     stack */
      XArray* ranges[N_D3_VAR_STACK]; /* XArray of AddrRange */
      Int     level[N_D3_VAR_STACK];  /* D3 DIE levels */
      Bool    isFunc[N_D3_VAR_STACK]; /* from DW_AT_subprogram? */
      GExpr*  fbGX[N_D3_VAR_STACK];   /* if isFunc, contains the FB
                                         expr, else NULL */
      /* The file name table.  Is a mapping from integer index to the
         (permanent) copy of the string, iow a non-img area. */
      XArray* /* of UChar* */ filenameTable;
   }
   D3VarParser;

static void varstack_show ( D3VarParser* parser, HChar* str ) {
   Word i, j;
   VG_(printf)("  varstack (%s) {\n", str);
   for (i = 0; i <= parser->sp; i++) {
      XArray* xa = parser->ranges[i];
      vg_assert(xa);
      VG_(printf)("    [%ld] (level %d)", i, parser->level[i]);
      if (parser->isFunc[i]) {
         VG_(printf)(" (fbGX=%p)", parser->fbGX[i]);
      } else {
         vg_assert(parser->fbGX[i] == NULL);
      }
      VG_(printf)(": ");
      if (VG_(sizeXA)( xa ) == 0) {
         VG_(printf)("** empty PC range array **");
      } else {
         for (j = 0; j < VG_(sizeXA)( xa ); j++) {
            AddrRange* range = (AddrRange*) VG_(indexXA)( xa, j );
            vg_assert(range);
            VG_(printf)("[%p,%p] ", range->aMin, range->aMax);
         }
      }
      VG_(printf)("\n");
   }
   VG_(printf)("  }\n");
}

/* Remove from the stack, all entries with .level > 'level' */
static 
void varstack_preen ( D3VarParser* parser, Bool td3, Int level )
{
   Bool changed = False;
   vg_assert(parser->sp < N_D3_VAR_STACK);
   while (True) {
      vg_assert(parser->sp >= -1);
      if (parser->sp == -1) break;
      if (parser->level[parser->sp] <= level) break;
      if (0) 
         TRACE_D3("BBBBAAAA varstack_pop [newsp=%d]\n", parser->sp-1);
      vg_assert(parser->ranges[parser->sp]);
      /* Who allocated this xa?  get_range_list() or
         unitary_range_list(). */
      VG_(deleteXA)( parser->ranges[parser->sp] );
      parser->ranges[parser->sp] = NULL;
      parser->level[parser->sp]  = 0;
      parser->isFunc[parser->sp] = False;
      parser->fbGX[parser->sp]   = NULL;
      parser->sp--;
      changed = True;
   }
   if (changed && td3)
      varstack_show( parser, "after preen" );
}

static void varstack_push ( CUConst* cc,
                            D3VarParser* parser,
                            Bool td3,
                            XArray* ranges, Int level,
                            Bool    isFunc, GExpr* fbGX ) {
   if (0)
   TRACE_D3("BBBBAAAA varstack_push[newsp=%d]: %d  %p\n",
            parser->sp+1, level, ranges);

   /* First we need to zap everything >= 'level', as we are about to
      replace any previous entry at 'level', so .. */
   varstack_preen(parser, /*td3*/False, level-1);

   vg_assert(parser->sp >= -1);
   vg_assert(parser->sp < N_D3_VAR_STACK);
   if (parser->sp == N_D3_VAR_STACK-1)
      cc->barf("varstack_push: N_D3_VAR_STACK is too low; "
               "increase and recompile");
   if (parser->sp >= 0)
      vg_assert(parser->level[parser->sp] < level);
   parser->sp++;
   vg_assert(parser->ranges[parser->sp] == NULL);
   vg_assert(parser->level[parser->sp]  == 0);
   vg_assert(parser->isFunc[parser->sp] == False);
   vg_assert(parser->fbGX[parser->sp]   == NULL);
   vg_assert(ranges != NULL);
   if (!isFunc) vg_assert(fbGX == NULL);
   parser->ranges[parser->sp] = ranges;
   parser->level[parser->sp]  = level;
   parser->isFunc[parser->sp] = isFunc;
   parser->fbGX[parser->sp]   = fbGX;
   if (td3)
      varstack_show( parser, "after push" );
}


/* cts, ctsSzB, ctsMemSzB are derived from a DW_AT_location and so
   refer either to a location expression or to a location list.
   Figure out which, and in both cases bundle the expression or
   location list into a so-called GExpr (guarded expression). */
__attribute__((noinline))
static GExpr* get_GX ( CUConst* cc, Bool td3, 
                       ULong cts, Int ctsSzB, UWord ctsMemSzB )
{
   GExpr* gexpr = NULL;
   if (ctsMemSzB > 0 && ctsSzB == 0) {
      /* represents an in-line location expression, and cts points
         right at it */
      gexpr = make_singleton_GX( (UChar*)(UWord)cts, ctsMemSzB );
   }
   else 
   if (ctsMemSzB == 0 && ctsSzB > 0) {
      /* represents location list.  cts is the offset of it in
         .debug_loc. */
      if (!cc->cu_svma_known)
         cc->barf("get_GX: location list, but CU svma is unknown");
      gexpr = make_general_GX( cc, td3, (UWord)cts, cc->cu_svma );
   }
   else {
      vg_assert(0); /* else caller is bogus */
   }
   return gexpr;
}


static 
void read_filename_table( /*MOD*/D3VarParser* parser,
                          CUConst* cc, UWord debug_line_offset,
                          Bool td3 )
{
   Bool   is_dw64;
   Cursor c;
   Word   i;
   ULong  unit_length;
   UShort version;
   ULong  header_length;
   UChar  minimum_instruction_length;
   UChar  default_is_stmt;
   Char   line_base;
   UChar  line_range;
   UChar  opcode_base;
   UChar* str;

   vg_assert(parser && cc && cc->barf);
   if ((!cc->debug_line_img) 
       || cc->debug_line_sz <= debug_line_offset)
      cc->barf("read_filename_table: .debug_line is missing?");

   init_Cursor( &c, cc->debug_line_img, 
                cc->debug_line_sz, debug_line_offset, cc->barf, 
                "Overrun whilst reading .debug_line section(1)" );

   unit_length 
      = get_Initial_Length( &is_dw64, &c,
           "read_filename_table: invalid initial-length field" );
   version = get_UShort( &c );
   if (version != 2)
     cc->barf("read_filename_table: Only DWARF version 2 line info "
              "is currently supported.");
   header_length = (ULong)get_Dwarfish_UWord( &c, is_dw64 );
   minimum_instruction_length = get_UChar( &c );
   default_is_stmt            = get_UChar( &c );
   line_base                  = (Char)get_UChar( &c );
   line_range                 = get_UChar( &c );
   opcode_base                = get_UChar( &c );
   /* skip over "standard_opcode_lengths" */
   for (i = 1; i < (Word)opcode_base; i++)
     (void)get_UChar( &c );

   /* skip over the directory names table */
   while (peek_UChar(&c) != 0) {
     (void)get_AsciiZ(&c);
   }
   (void)get_UChar(&c); /* skip terminating zero */

   /* Read and record the file names table */
   vg_assert(parser->filenameTable);
   vg_assert( VG_(sizeXA)( parser->filenameTable ) == 0 );
   /* Add a dummy index-zero entry.  DWARF3 numbers its files
      from 1, for some reason. */
   str = ML_(addStr)( cc->di, "<unknown_file>", -1 );
   VG_(addToXA)( parser->filenameTable, &str );
   while (peek_UChar(&c) != 0) {
      str = get_AsciiZ(&c);
      TRACE_D3("  read_filename_table: %ld %s\n",
               VG_(sizeXA)(parser->filenameTable), str);
      str = ML_(addStr)( cc->di, str, -1 );
      VG_(addToXA)( parser->filenameTable, &str );
      (void)get_ULEB128( &c ); /* skip directory index # */
      (void)get_ULEB128( &c ); /* skip last mod time */
      (void)get_ULEB128( &c ); /* file size */
   }
   /* We're done!  The rest of it is not interesting. */
}


__attribute__((noinline))
static void parse_var_DIE ( /*OUT*/TempVar** tempvars,
                            /*OUT*/GExpr** gexprs,
                            /*MOD*/D3VarParser* parser,
                            DW_TAG dtag,
                            UWord posn,
                            Int level,
                            Cursor* c_die,
                            Cursor* c_abbv,
                            CUConst* cc,
                            Bool td3 )
{
   ULong       cts;
   Int         ctsSzB;
   UWord       ctsMemSzB;

   UWord saved_die_c_offset  = get_position_of_Cursor( c_die );
   UWord saved_abbv_c_offset = get_position_of_Cursor( c_abbv );

   varstack_preen( parser, td3, level-1 );

   if (dtag == DW_TAG_compile_unit) {
      Bool have_lo    = False;
      Bool have_hi1   = False;
      Bool have_range = False;
      Addr ip_lo    = 0;
      Addr ip_hi1   = 0;
      Addr rangeoff = 0;
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_low_pc && ctsSzB > 0) {
            ip_lo   = cts;
            have_lo = True;
         }
         if (attr == DW_AT_high_pc && ctsSzB > 0) {
            ip_hi1   = cts;
            have_hi1 = True;
         }
         if (attr == DW_AT_ranges && ctsSzB > 0) {
            rangeoff = cts;
            have_range = True;
         }
         if (attr == DW_AT_stmt_list && ctsSzB > 0) {
            read_filename_table( parser, cc, (UWord)cts, td3 );
         }
      }
      /* Now, does this give us an opportunity to find this
         CU's svma? */
#if 0
      if (level == 0 && have_lo) {
         vg_assert(!cc->cu_svma_known); /* if this fails, it must be
         because we've already seen a DW_TAG_compile_unit DIE at level
         0.  But that can't happen, because DWARF3 only allows exactly
         one top level DIE per CU. */
         cc->cu_svma_known = True;
         cc->cu_svma = ip_lo;
         if (1)
            TRACE_D3("BBBBAAAA acquire CU_SVMA of %p\n", cc->cu_svma);
         /* Now, it may be that this DIE doesn't tell us the CU's
            SVMA, by way of not having a DW_AT_low_pc.  That's OK --
            the CU doesn't *have* to have its SVMA specified.

            But as per last para D3 spec sec 3.1.1 ("Normal and
            Partial Compilation Unit Entries", "If the base address
            (viz, the SVMA) is undefined, then any DWARF entry of
            structure defined interms of the base address of that
            compilation unit is not valid.".  So that means, if whilst
            processing the children of this top level DIE (or their
            children, etc) we see a DW_AT_range, and cu_svma_known is
            False, then the DIE that contains it is (per the spec)
            invalid, and we can legitimately stop and complain. */
      }
#else
      /* .. whereas The Reality is, simply assume the SVMA is zero
         if it isn't specified. */
      if (level == 0) {
         vg_assert(!cc->cu_svma_known);
         cc->cu_svma_known = True;
         if (have_lo)
            cc->cu_svma = ip_lo;
         else
            cc->cu_svma = 0;
      }
#endif
      /* Do we have something that looks sane? */
      if (have_lo && have_hi1 && (!have_range)) {
         if (ip_lo < ip_hi1)
            varstack_push( cc, parser, td3, 
                           unitary_range_list(ip_lo, ip_hi1 - 1),
                           level,
                           False/*isFunc*/, NULL/*fbGX*/ );
      } else
      if ((!have_lo) && (!have_hi1) && have_range) {
         varstack_push( cc, parser, td3, 
                        get_range_list( cc, td3,
                                        rangeoff, cc->cu_svma ),
                        level,
                        False/*isFunc*/, NULL/*fbGX*/ );
      } else
      if ((!have_lo) && (!have_hi1) && (!have_range)) {
         /* CU has no code, presumably? */
         varstack_push( cc, parser, td3, 
                        empty_range_list(),
                        level,
                        False/*isFunc*/, NULL/*fbGX*/ );
      } else
         goto bad_DIE;
   }

   if (dtag == DW_TAG_lexical_block || dtag == DW_TAG_subprogram) {
      Bool   have_lo    = False;
      Bool   have_hi1   = False;
      Bool   have_range = False;
      Addr   ip_lo      = 0;
      Addr   ip_hi1     = 0;
      Addr   rangeoff   = 0;
      Bool   isFunc     = dtag == DW_TAG_subprogram;
      GExpr* fbGX       = NULL;
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_low_pc && ctsSzB > 0) {
            ip_lo   = cts;
            have_lo = True;
         }
         if (attr == DW_AT_high_pc && ctsSzB > 0) {
            ip_hi1   = cts;
            have_hi1 = True;
         }
         if (attr == DW_AT_ranges && ctsSzB > 0) {
            rangeoff = cts;
            have_range = True;
         }
         if (isFunc
             && attr == DW_AT_frame_base
             && ((ctsMemSzB > 0 && ctsSzB == 0)
                 || (ctsMemSzB == 0 && ctsSzB > 0))) {
            fbGX = get_GX( cc, False/*td3*/, cts, ctsSzB, ctsMemSzB );
            vg_assert(fbGX);
            vg_assert(!fbGX->next);
            fbGX->next = *gexprs;
            *gexprs = fbGX;
         }
      }
      /* Do we have something that looks sane? */
      if (dtag == DW_TAG_subprogram 
          && (!have_lo) && (!have_hi1) && (!have_range)) {
         /* This is legit - ignore it. Sec 3.3.3: "A subroutine entry
            representing a subroutine declaration that is not also a
            definition does not have code address or range
            attributes." */
      } else
      if (dtag == DW_TAG_lexical_block
          && (!have_lo) && (!have_hi1) && (!have_range)) {
         /* I believe this is legit, and means the lexical block
            contains no insns (whatever that might mean).  Ignore. */
      } else
      if (have_lo && have_hi1 && (!have_range)) {
         /* This scope supplies just a single address range. */
         if (ip_lo < ip_hi1)
            varstack_push( cc, parser, td3, 
                           unitary_range_list(ip_lo, ip_hi1 - 1),
                           level, isFunc, fbGX );
      } else
      if ((!have_lo) && (!have_hi1) && have_range) {
         /* This scope supplies multiple address ranges via the use of
            a range list. */
         varstack_push( cc, parser, td3, 
                        get_range_list( cc, td3,
                                        rangeoff, cc->cu_svma ),
                        level, isFunc, fbGX );
      } else
      if (have_lo && (!have_hi1) && (!have_range)) {
         /* This scope is bogus.  The D3 spec sec 3.4 (Lexical Block
            Entries) says fairly clearly that a scope must have either
            _range or (_low_pc and _high_pc). */
         /* The spec is a bit ambiguous though.  Perhaps a single byte
            range is intended?  See sec 2.17 (Code Addresses And Ranges) */
         /* This case is here because icc9 produced this:
         <2><13bd>: DW_TAG_lexical_block
            DW_AT_decl_line   : 5229
            DW_AT_decl_column : 37
            DW_AT_decl_file   : 1
            DW_AT_low_pc      : 0x401b03
         */
         /* Ignore (seems safe than pushing a single byte range) */
      } else
         goto bad_DIE;
   }

   if (dtag == DW_TAG_variable || dtag == DW_TAG_formal_parameter) {
      UChar* name        = NULL;
      Type*  typeR       = D3_INVALID_CUOFF;
      Bool   external    = False;
      GExpr* gexpr       = NULL;
      Int    n_attrs     = 0;
      UWord  abs_ori     = (UWord)D3_INVALID_CUOFF;
      Bool   declaration = False;
      Int    lineNo      = 0;
      UChar* fileName    = NULL;
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         n_attrs++;
         if (attr == DW_AT_name && ctsMemSzB > 0) {
            name = ML_(addStr)( cc->di, (UChar*)(UWord)cts, -1 );
         }
         if (attr == DW_AT_location
             && ((ctsMemSzB > 0 && ctsSzB == 0)
                 || (ctsMemSzB == 0 && ctsSzB > 0))) {
            gexpr = get_GX( cc, False/*td3*/, cts, ctsSzB, ctsMemSzB );
            vg_assert(gexpr);
            vg_assert(!gexpr->next);
            gexpr->next = *gexprs;
            *gexprs = gexpr;
         }
         if (attr == DW_AT_type && ctsSzB > 0) {
            typeR = (Type*)(UWord)cts;
         }
         if (attr == DW_AT_external && ctsSzB > 0 && cts > 0) {
            external = True;
         }
         if (attr == DW_AT_abstract_origin && ctsSzB > 0) {
            abs_ori = (UWord)cts;
         }
         if (attr == DW_AT_declaration && ctsSzB > 0 && cts > 0) {
            declaration = True;
         }
         if (attr == DW_AT_decl_line && ctsSzB > 0) {
            lineNo = (Int)cts;
         }
         if (attr == DW_AT_decl_file && ctsSzB > 0) {
            Int ftabIx = (Int)cts;
            if (ftabIx >= 1
                && ftabIx < VG_(sizeXA)( parser->filenameTable )) {
               fileName = *(UChar**)
                          VG_(indexXA)( parser->filenameTable, ftabIx );
               vg_assert(fileName);
            }
            if (0) VG_(printf)("XXX filename = %s\n", fileName);
         }
      }
      /* We'll collect it under if one of the following three
         conditions holds:
         (1) has location and type    -> completed
         (2) has type only            -> is an abstract instance
         (3) has location and abs_ori -> is a concrete instance
         Name, filename and line number are all option frills.
      */
      if ( /* 1 */ (gexpr && typeR != D3_INVALID_CUOFF) 
           /* 2 */ || (typeR != D3_INVALID_CUOFF)
           /* 3 */ || (gexpr && abs_ori != (UWord)D3_INVALID_CUOFF) ) {

         /* Add this variable to the list of interesting looking
            variables.  Crucially, note along with it the address
            range(s) associated with the variable, which for locals
            will be the address ranges at the top of the varparser's
            stack. */
         GExpr*   fbGX = NULL;
         Word     i, nRanges;
         XArray*  /* of AddrRange */ xa;
         TempVar* tv;
         /* Stack can't be empty; we put a dummy entry on it for the
            entire address range before starting with the DIEs for
            this CU. */
         vg_assert(parser->sp >= 0);

         /* If this is a local variable (non-external), try to find
            the GExpr for the DW_AT_frame_base of the containing
            function.  It should have been pushed on the stack at the
            time we encountered its DW_TAG_subprogram DIE, so the way
            to find it is to scan back down the stack looking for it.
            If there isn't an enclosing stack entry marked 'isFunc'
            then we must be seeing variable or formal param DIEs
            outside of a function, so we deem the Dwarf to be
            malformed if that happens.  Note that the fbGX may be NULL
            if the containing DT_TAG_subprogram didn't supply a
            DW_AT_frame_base -- that's OK, but there must actually be
            a containing DW_TAG_subprogram. */
         if (!external) {
            Bool found = False;
            for (i = parser->sp; i >= 0; i--) {
               if (parser->isFunc[i]) {
                  fbGX = parser->fbGX[i];
                  found = True;
                  break;
               }
            }
            if (!found) {
               if (0 && VG_(clo_verbosity) >= 0) {
                  VG_(message)(Vg_DebugMsg, 
                     "warning: parse_var_DIE: non-external variable "
                     "outside DW_TAG_subprogram");
               }
               /* goto bad_DIE; */
               /* This seems to happen a lot.  Just ignore it -- if,
                  when we come to evaluation of the location (guarded)
                  expression, it requires a frame base value, and
                  there's no expression for that, then evaluation as a
                  whole will fail.  Harmless - a bit of a waste of
                  cycles but nothing more. */
            }
         }

         /* re "external ? 0 : parser->sp" (twice), if the var is
            marked 'external' then we must put it at the global scope,
            as only the global scope (level 0) covers the entire PC
            address space.  It is asserted elsewhere that level 0 
            always covers the entire address space. */
         xa = parser->ranges[external ? 0 : parser->sp];
         nRanges = VG_(sizeXA)(xa);
         vg_assert(nRanges >= 0);

         tv = ML_(dinfo_zalloc)( sizeof(TempVar) );
         tv->name   = name;
         tv->level  = external ? 0 : parser->sp;
         tv->typeR  = typeR;
         tv->gexpr  = gexpr;
         tv->fbGX   = fbGX;
         tv->fName  = fileName;
         tv->fLine  = lineNo;
         tv->dioff  = posn;
         tv->absOri = abs_ori;

         /* See explanation on definition of type TempVar for the
            reason for this elaboration. */
         tv->nRanges = nRanges;
         tv->rngOneMin = 0;
         tv->rngOneMax = 0;
         tv->rngMany = NULL;
         if (nRanges == 1) {
            AddrRange* range = VG_(indexXA)(xa, 0);
            tv->rngOneMin = range->aMin;
            tv->rngOneMax = range->aMax;
         }
         else if (nRanges > 1) {
            tv->rngMany = VG_(cloneXA)( xa ); /* free when 'tv' freed */
         }

         tv->next  = *tempvars;
         *tempvars = tv;

         TRACE_D3("  Recording this variable, with %ld PC range(s)\n",
                  VG_(sizeXA)(xa) );
         /* collect stats on how effective the ->ranges special
            casing is */
         if (0) {
           static Int ntot=0, ngt=0;
           ntot++;
           if (tv->rngMany) ngt++;
           if (0 == (ntot % 100000))
              VG_(printf)("XXXX %d tot, %d cloned\n", ntot, ngt);
         }

      }

      /* Here are some other weird cases seen in the wild:

            We have a variable with a name and a type, but no
            location.  I guess that's a sign that it has been
            optimised away.  Ignore it.  Here's an example:

            static Int lc_compar(void* n1, void* n2) {
               MC_Chunk* mc1 = *(MC_Chunk**)n1;
               MC_Chunk* mc2 = *(MC_Chunk**)n2;
               return (mc1->data < mc2->data ? -1 : 1);
            }

            Both mc1 and mc2 are like this
            <2><5bc>: Abbrev Number: 21 (DW_TAG_variable)
                DW_AT_name        : mc1
                DW_AT_decl_file   : 1
                DW_AT_decl_line   : 216
                DW_AT_type        : <5d3>

            whereas n1 and n2 do have locations specified.

            ---------------------------------------------

            We see a DW_TAG_formal_parameter with a type, but
            no name and no location.  It's probably part of a function type
            construction, thusly, hence ignore it:
         <1><2b4>: Abbrev Number: 12 (DW_TAG_subroutine_type)
             DW_AT_sibling     : <2c9>
             DW_AT_prototyped  : 1
             DW_AT_type        : <114>
         <2><2be>: Abbrev Number: 13 (DW_TAG_formal_parameter)
             DW_AT_type        : <13e>
         <2><2c3>: Abbrev Number: 13 (DW_TAG_formal_parameter)
             DW_AT_type        : <133>

            ---------------------------------------------

            Is very minimal, like this:
            <4><81d>: Abbrev Number: 44 (DW_TAG_variable)
                DW_AT_abstract_origin: <7ba>
            What that signifies I have no idea.  Ignore.

            ----------------------------------------------

            Is very minimal, like this:
            <200f>: DW_TAG_formal_parameter
                DW_AT_abstract_ori: <1f4c>
                DW_AT_location    : 13440
            What that signifies I have no idea.  Ignore. 
            It might be significant, though: the variable at least
            has a location and so might exist somewhere.
            Maybe we should handle this.

            ---------------------------------------------

            <22407>: DW_TAG_variable
              DW_AT_name        : (indirect string, offset: 0x6579):
                                  vgPlain_trampoline_stuff_start
              DW_AT_decl_file   : 29
              DW_AT_decl_line   : 56
              DW_AT_external    : 1
              DW_AT_declaration : 1

            Nameless and typeless variable that has a location?  Who
            knows.  Not me.
            <2><3d178>: Abbrev Number: 22 (DW_TAG_variable)
                 DW_AT_location    : 9 byte block: 3 c0 c7 13 38 0 0 0 0
                                     (DW_OP_addr: 3813c7c0)

            No, really.  Check it out.  gcc is quite simply borked.
            <3><168cc>: Abbrev Number: 141 (DW_TAG_variable)
            // followed by no attributes, and the next DIE is a sibling,
            // not a child
            */
   }
   return;

  bad_DIE:
   set_position_of_Cursor( c_die,  saved_die_c_offset );
   set_position_of_Cursor( c_abbv, saved_abbv_c_offset );
   VG_(printf)("\nparse_var_DIE: confused by:\n");
   VG_(printf)(" <%d><%lx>: %s\n", level, posn, ML_(pp_DW_TAG)( dtag ) );
   while (True) {
      DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
      DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
      if (attr == 0 && form == 0) break;
      VG_(printf)("     %18s: ", ML_(pp_DW_AT)(attr));
      /* Get the form contents, so as to print them */
      get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                         cc, c_die, True, form );
      VG_(printf)("\t\n");
   }
   VG_(printf)("\n");
   cc->barf("parse_var_DIE: confused by the above DIE");
   /*NOTREACHED*/
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Parsing of type-related DIEs                         ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

#define N_D3_TYPE_STACK 16

typedef
   struct {
      /* What source language?  'C'=C/C++, 'F'=Fortran, '?'=other
         Established once per compilation unit. */
      UChar language;
      /* A stack of types which are currently under construction */
      Int   sp; /* [sp] is innermost active entry; sp==-1 for empty
                   stack */
      Type* qparent[N_D3_TYPE_STACK];
      Int   qlevel[N_D3_TYPE_STACK];

   }
   D3TypeParser;

static void typestack_show ( D3TypeParser* parser, HChar* str ) {
   Word i;
   VG_(printf)("  typestack (%s) {\n", str);
   for (i = 0; i <= parser->sp; i++) {
      VG_(printf)("    [%ld] (level %d): ", i, parser->qlevel[i]);
      ML_(pp_Type)( parser->qparent[i] );
      VG_(printf)("\n");
   }
   VG_(printf)("  }\n");
}

/* Remove from the stack, all entries with .level > 'level' */
static 
void typestack_preen ( D3TypeParser* parser, Bool td3, Int level )
{
   Bool changed = False;
   vg_assert(parser->sp < N_D3_TYPE_STACK);
   while (True) {
      vg_assert(parser->sp >= -1);
      if (parser->sp == -1) break;
      if (parser->qlevel[parser->sp] <= level) break;
      if (0) 
         TRACE_D3("BBBBAAAA typestack_pop [newsp=%d]\n", parser->sp-1);
      vg_assert(parser->qparent[parser->sp]);
      parser->qparent[parser->sp] = NULL;
      parser->qlevel[parser->sp]  = 0;
      parser->sp--;
      changed = True;
   }
   if (changed && td3)
      typestack_show( parser, "after preen" );
}

static Bool typestack_is_empty ( D3TypeParser* parser ) {
   vg_assert(parser->sp >= -1 && parser->sp < N_D3_TYPE_STACK);
   return parser->sp == -1;
}

static void typestack_push ( CUConst* cc,
                             D3TypeParser* parser,
                             Bool td3,
                             Type* parent, Int level ) {
   if (0)
   TRACE_D3("BBBBAAAA typestack_push[newsp=%d]: %d  %p\n",
            parser->sp+1, level, parent);

   /* First we need to zap everything >= 'level', as we are about to
      replace any previous entry at 'level', so .. */
   typestack_preen(parser, /*td3*/False, level-1);

   vg_assert(parser->sp >= -1);
   vg_assert(parser->sp < N_D3_TYPE_STACK);
   if (parser->sp == N_D3_TYPE_STACK-1)
      cc->barf("typestack_push: N_D3_TYPE_STACK is too low; "
               "increase and recompile");
   if (parser->sp >= 0)
      vg_assert(parser->qlevel[parser->sp] < level);
   parser->sp++;
   vg_assert(parser->qparent[parser->sp] == NULL);
   vg_assert(parser->qlevel[parser->sp]  == 0);
   vg_assert(parent != NULL);
   parser->qparent[parser->sp] = parent;
   parser->qlevel[parser->sp]  = level;
   if (td3)
      typestack_show( parser, "after push" );
}


/* Parse a type-related DIE.  'parser' holds the current parser state.
   'admin' is where the completed types are dumped.  'dtag' is the tag
   for this DIE.  'c_die' points to the start of the data fields (FORM
   stuff) for the DIE.  c_abbv points to the start of the (name,form)
   pairs which describe the DIE.

   We may find the DIE uninteresting, in which case we should ignore
   it.
*/
__attribute__((noinline))
static void parse_type_DIE ( /*OUT*/TyAdmin** admin,
                             /*MOD*/D3TypeParser* parser,
                             DW_TAG dtag,
                             UWord posn,
                             Int level,
                             Cursor* c_die,
                             Cursor* c_abbv,
                             CUConst* cc,
                             Bool td3 )
{
   ULong     cts;
   Int       ctsSzB;
   UWord     ctsMemSzB;
   Type*     type   = NULL;
   TyAtom*   atom   = NULL;
   TyField*  field  = NULL;
   D3Expr*   expr   = NULL;
   TyBounds* bounds = NULL;

   UWord saved_die_c_offset  = get_position_of_Cursor( c_die );
   UWord saved_abbv_c_offset = get_position_of_Cursor( c_abbv );

   /* If we've returned to a level at or above any previously noted
      parent, un-note it, so we don't believe we're still collecting
      its children. */
   typestack_preen( parser, td3, level-1 );

   if (dtag == DW_TAG_compile_unit) {
      /* See if we can find DW_AT_language, since it is important for
         establishing array bounds (see DW_TAG_subrange_type below in
         this fn) */
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr != DW_AT_language)
            continue;
         if (ctsSzB == 0)
           goto bad_DIE;
         switch (cts) {
            case DW_LANG_C89: case DW_LANG_C:
            case DW_LANG_C_plus_plus: case DW_LANG_ObjC:
            case DW_LANG_ObjC_plus_plus: case DW_LANG_UPC:
            case DW_LANG_Upc:
               parser->language = 'C'; break;
            case DW_LANG_Fortran77: case DW_LANG_Fortran90:
            case DW_LANG_Fortran95:
               parser->language = 'F'; break;
            case DW_LANG_Ada83: case DW_LANG_Cobol74:
            case DW_LANG_Cobol85: case DW_LANG_Pascal83:
            case DW_LANG_Modula2: case DW_LANG_Java:
            case DW_LANG_C99: case DW_LANG_Ada95:
            case DW_LANG_PLI: case DW_LANG_D:
            case DW_LANG_Mips_Assembler:
               parser->language = '?'; break;
            default:
               goto bad_DIE;
         }
      }
   }

   if (dtag == DW_TAG_base_type) {
      /* We can pick up a new base type any time. */
      type = ML_(new_Type)();
      type->tag = Ty_Base;
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_name && ctsMemSzB > 0) {
            type->Ty.Base.name
               = ML_(addStr)( cc->di, (UChar*)(UWord)cts, -1 );
         }
         if (attr == DW_AT_byte_size && ctsSzB > 0) {
            type->Ty.Base.szB = cts;
         }
         if (attr == DW_AT_encoding && ctsSzB > 0) {
            switch (cts) {
               case DW_ATE_unsigned: case DW_ATE_unsigned_char:
               case DW_ATE_boolean:/* FIXME - is this correct? */
                  type->Ty.Base.enc = 'U'; break;
               case DW_ATE_signed: case DW_ATE_signed_char:
                  type->Ty.Base.enc = 'S'; break;
               case DW_ATE_float:
                  type->Ty.Base.enc = 'F'; break;
               case DW_ATE_complex_float:
                  type->Ty.Base.enc = 'C'; break;
               default:
                  goto bad_DIE;
            }
         }
      }

      /* Invent a name if it doesn't have one.  gcc-4.3
         -ftree-vectorize is observed to emit nameless base types. */
      if (!type->Ty.Base.name)
         type->Ty.Base.name 
            = ML_(addStr)( cc->di, "<anon_base_type>", -1 );

      /* Do we have something that looks sane? */
      if (/* must have a name */
          type->Ty.Base.name == NULL
          /* and a plausible size.  Yes, really 32: "complex long
             double" apparently has size=32 */
          || type->Ty.Base.szB < 0 || type->Ty.Base.szB > 32
          /* and a plausible encoding */
          || (type->Ty.Base.enc != 'U'
              && type->Ty.Base.enc != 'S' 
              && type->Ty.Base.enc != 'F'
              && type->Ty.Base.enc != 'C'))
         goto bad_DIE;
      /* Last minute hack: if we see this
         <1><515>: DW_TAG_base_type
             DW_AT_byte_size   : 0
             DW_AT_encoding    : 5
             DW_AT_name        : void
         convert it into a real Void type. */
      if (type->Ty.Base.szB == 0
          && 0 == VG_(strcmp)("void", type->Ty.Base.name)) {
         VG_(memset)(type, 0, sizeof(*type));
         type->tag = Ty_Void;
         type->Ty.Void.isFake = False; /* it's a real one! */
      }
      goto acquire_Type;
   }

   if (dtag == DW_TAG_pointer_type || dtag == DW_TAG_reference_type
       || dtag == DW_TAG_ptr_to_member_type) {
      /* This seems legit for _pointer_type and _reference_type.  I
         don't know if rolling _ptr_to_member_type in here really is
         legit, but it's better than not handling it at all. */
      type = ML_(new_Type)();
      type->tag = Ty_PorR;
      /* target type defaults to void */
      type->Ty.PorR.typeR = D3_FAKEVOID_CUOFF;
      type->Ty.PorR.isPtr = dtag == DW_TAG_pointer_type
                            || dtag == DW_TAG_ptr_to_member_type;
      /* Pointer types don't *have* to specify their size, in which
         case we assume it's a machine word.  But if they do specify
         it, it must be a machine word :-) This probably assumes that
         the word size of the Dwarf3 we're reading is the same size as
         that on the machine.  gcc appears to give a size whereas icc9
         doesn't. */
      if (type->Ty.PorR.isPtr)
         type->Ty.PorR.szB = sizeof(Word);
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_byte_size && ctsSzB > 0) {
            type->Ty.PorR.szB = cts;
         }
         if (attr == DW_AT_type && ctsSzB > 0) {
            type->Ty.PorR.typeR = (Type*)(UWord)cts;
         }
      }
      /* Do we have something that looks sane? */
      if (type->Ty.PorR.szB != sizeof(Word))
         goto bad_DIE;
      else
         goto acquire_Type;
   }

   if (dtag == DW_TAG_enumeration_type) {
      /* Create a new Type to hold the results. */
      type = ML_(new_Type)();
      type->tag = Ty_Enum;
      type->Ty.Enum.name = NULL;
      type->Ty.Enum.atomRs
         = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                       sizeof(TyAtom*) );
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_name && ctsMemSzB > 0) {
            type->Ty.Enum.name
               = ML_(addStr)( cc->di, (UChar*)(UWord)cts, -1 );
         }
         if (attr == DW_AT_byte_size && ctsSzB > 0) {
            type->Ty.Enum.szB = cts;
         }
      }
      /* Do we have something that looks sane? */
      if (type->Ty.Enum.szB == 0 /* we must know the size */
          /* But the name can be present, or not */)
         goto bad_DIE;
      /* On't stack! */
      typestack_push( cc, parser, td3, type, level );
      goto acquire_Type;
   }

   if (dtag == DW_TAG_enumerator) {
      Bool have_value = False;
      atom = ML_(new_TyAtom)( NULL, 0 );
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_name && ctsMemSzB > 0) {
            atom->name = ML_(addStr)( cc->di, (UChar*)(UWord)cts, -1 );
         }
         if (attr == DW_AT_const_value && ctsSzB > 0) {
            atom->value = cts;
            have_value = True;
         }
      }
      /* Do we have something that looks sane? */
      if ((!have_value) || atom->name == NULL)
         goto bad_DIE;
      /* Do we have a plausible parent? */
      if (typestack_is_empty(parser)) goto bad_DIE;
      vg_assert(parser->qparent[parser->sp]);
      if (level != parser->qlevel[parser->sp]+1) goto bad_DIE;
      if (parser->qparent[parser->sp]->tag != Ty_Enum) goto bad_DIE;
      /* Record this child in the parent */
      vg_assert(parser->qparent[parser->sp]->Ty.Enum.atomRs);
      VG_(addToXA)( parser->qparent[parser->sp]->Ty.Enum.atomRs, &atom );
      /* And record the child itself */
      goto acquire_Atom;
   }

   if (dtag == DW_TAG_structure_type || dtag == DW_TAG_union_type) {
      Bool have_szB = False;
      Bool is_decl  = False;
      Bool is_spec  = False;
      /* Create a new Type to hold the results. */
      type = ML_(new_Type)();
      type->tag = Ty_StOrUn;
      type->Ty.StOrUn.name = NULL;
      type->Ty.StOrUn.fields
         = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                       sizeof(TyAtom*) );
      type->Ty.StOrUn.complete = True;
      type->Ty.StOrUn.isStruct = dtag == DW_TAG_structure_type;
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_name && ctsMemSzB > 0) {
            type->Ty.StOrUn.name
               = ML_(addStr)( cc->di, (UChar*)(UWord)cts, -1 );
         }
         if (attr == DW_AT_byte_size && ctsSzB >= 0) {
            type->Ty.StOrUn.szB = cts;
            have_szB = True;
         }
         if (attr == DW_AT_declaration && ctsSzB > 0 && cts > 0) {
            is_decl = True;
         }
         if (attr == DW_AT_specification && ctsSzB > 0 && cts > 0) {
            is_spec = True;
         }
      }
      /* Do we have something that looks sane? */
      if (is_decl && (!is_spec)) {
         /* It's a DW_AT_declaration.  We require the name but
            nothing else. */
         if (type->Ty.StOrUn.name == NULL)
            goto bad_DIE;
         type->Ty.StOrUn.complete = False;
         goto acquire_Type;
      }
      if ((!is_decl) /* && (!is_spec) */) {
         /* this is the common, ordinary case */
         if ((!have_szB) /* we must know the size */
             /* But the name can be present, or not */)
            goto bad_DIE;
         /* On't stack! */
         typestack_push( cc, parser, td3, type, level );
         goto acquire_Type;
      }
      else {
         /* don't know how to handle any other variants just now */
         goto bad_DIE;
      }
   }

   if (dtag == DW_TAG_member) {
      /* Acquire member entries for both DW_TAG_structure_type and
         DW_TAG_union_type.  They differ minorly, in that struct
         members must have a DW_AT_data_member_location expression
         whereas union members must not. */
      Bool parent_is_struct;
      field = ML_(new_TyField)( NULL, NULL, NULL );
      field->typeR = D3_INVALID_CUOFF;
      expr  = NULL;
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_name && ctsMemSzB > 0) {
            field->name = ML_(addStr)( cc->di, (UChar*)(UWord)cts, -1 );
         }
         if (attr == DW_AT_type && ctsSzB > 0) {
            field->typeR = (Type*)(UWord)cts;
         }
         if (attr == DW_AT_data_member_location && ctsMemSzB > 0) {
            UChar* copy = ML_(addStr)( cc->di, (UChar*)(UWord)cts, 
                                               (Int)ctsMemSzB );
            expr = ML_(new_D3Expr)( copy, (UWord)ctsMemSzB );
         }
      }
      /* Do we have a plausible parent? */
      if (typestack_is_empty(parser)) goto bad_DIE;
      vg_assert(parser->qparent[parser->sp]);
      if (level != parser->qlevel[parser->sp]+1) goto bad_DIE;
      if (parser->qparent[parser->sp]->tag != Ty_StOrUn) goto bad_DIE;
      /* Do we have something that looks sane?  If this a member of a
         struct, we must have a location expression; but if a member
         of a union that is irrelevant (D3 spec sec 5.6.6).  We ought
         to reject in the latter case, but some compilers have been
         observed to emit constant-zero expressions.  So just ignore
         them. */
      parent_is_struct
         = parser->qparent[parser->sp]->Ty.StOrUn.isStruct;
      if (!field->name)
         field->name = ML_(addStr)(cc->di, "<anon_field>", -1);
      if ((!field->name) || (field->typeR == D3_INVALID_CUOFF))
         goto bad_DIE;
      if (parent_is_struct && (!expr))
         goto bad_DIE;
      if ((!parent_is_struct) && expr) {
         /* If this is a union type, pretend we haven't seen the data
            member location expression, as it is by definition
            redundant (it must be zero). */
         expr = NULL;
      }
      /* Record this child in the parent */
      field->isStruct = parent_is_struct;
      if (expr)
         field->loc = expr;
      vg_assert(parser->qparent[parser->sp]->Ty.StOrUn.fields);
      VG_(addToXA)( parser->qparent[parser->sp]->Ty.StOrUn.fields,
                    &field );
      /* And record the child itself */
      goto acquire_Field_and_Expr;
   }

   if (dtag == DW_TAG_array_type) {
      type = ML_(new_Type)();
      type->tag = Ty_Array;
      type->Ty.Array.typeR = D3_INVALID_CUOFF;
      type->Ty.Array.bounds
         = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                       sizeof(TyBounds*) );
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_type && ctsSzB > 0) {
            type->Ty.Array.typeR = (Type*)(UWord)cts;
         }
      }
      if (type->Ty.Array.typeR == D3_INVALID_CUOFF)
         goto bad_DIE;
      /* On't stack! */
      typestack_push( cc, parser, td3, type, level );
      goto acquire_Type;
   }

   if (dtag == DW_TAG_subrange_type) {
      Bool have_lower = False;
      Bool have_upper = False;
      Bool have_count = False;
      Long lower = 0;
      Long upper = 0;
      Long count = 0;

      switch (parser->language) {
         case 'C': have_lower = True;  lower = 0; break;
         case 'F': have_lower = True;  lower = 1; break;
         case '?': have_lower = False; break;
         default:  vg_assert(0); /* assured us by handling of
                                    DW_TAG_compile_unit in this fn */
      }
      bounds = ML_(new_TyBounds)();
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_lower_bound && ctsSzB > 0) {
            lower      = (Long)cts;
            have_lower = True;
         }
         if (attr == DW_AT_upper_bound && ctsSzB > 0) {
            upper      = (Long)cts;
            have_upper = True;
         }
         if (attr == DW_AT_count && ctsSzB > 0) {
            count      = cts;
            have_count = True;
         }
      }
      /* FIXME: potentially skip the rest if no parent present, since
         it could be the case that this subrange type is free-standing
         (not being used to describe the bounds of a containing array
         type) */
      /* Do we have a plausible parent? */
      if (typestack_is_empty(parser)) goto bad_DIE;
      vg_assert(parser->qparent[parser->sp]);
      if (level != parser->qlevel[parser->sp]+1) goto bad_DIE;
      if (parser->qparent[parser->sp]->tag != Ty_Array) goto bad_DIE;

      /* Figure out if we have a definite range or not */
      if (have_lower && have_upper && (!have_count)) {
         bounds->knownL = True;
         bounds->knownU = True;
         bounds->boundL = lower;
         bounds->boundU = upper;
      } 
      else if (have_lower && (!have_upper) && (!have_count)) {
         bounds->knownL = True;
         bounds->knownU = False;
         bounds->boundL = lower;
         bounds->boundU = 0;
      } else {
         /* FIXME: handle more cases */
         goto bad_DIE;
      }

      /* Record this bound in the parent */
      vg_assert(parser->qparent[parser->sp]->Ty.Array.bounds);
      VG_(addToXA)( parser->qparent[parser->sp]->Ty.Array.bounds,
                    &bounds );
      /* And record the child itself */
      goto acquire_Bounds;
   }

   if (dtag == DW_TAG_typedef) {
      /* We can pick up a new base type any time. */
      type = ML_(new_Type)();
      type->tag = Ty_TyDef;
      type->Ty.TyDef.name = NULL;
      type->Ty.TyDef.typeR = D3_INVALID_CUOFF;
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_name && ctsMemSzB > 0) {
            type->Ty.TyDef.name
               = ML_(addStr)( cc->di, (UChar*)(UWord)cts, -1 );
         }
         if (attr == DW_AT_type && ctsSzB > 0) {
            type->Ty.TyDef.typeR = (Type*)(UWord)cts;
         }
      }
      /* Do we have something that looks sane? */
      if (/* must have a name */
          type->Ty.TyDef.name == NULL
          /* but the referred-to type can be absent */)
         goto bad_DIE;
      else
         goto acquire_Type;
   }

   if (dtag == DW_TAG_subroutine_type) {
      /* function type? just record that one fact and ask no
         further questions. */
      type = ML_(new_Type)();
      type->tag = Ty_Fn;
      goto acquire_Type;
   }

   if (dtag == DW_TAG_volatile_type || dtag == DW_TAG_const_type) {
      Int have_ty = 0;
      type = ML_(new_Type)();
      type->tag = Ty_Qual;
      type->Ty.Qual.qual
         = dtag == DW_TAG_volatile_type ? 'V' : 'C';
      /* target type defaults to 'void' */
      type->Ty.Qual.typeR = D3_FAKEVOID_CUOFF;
      while (True) {
         DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
         DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
         if (attr == 0 && form == 0) break;
         get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                            cc, c_die, False/*td3*/, form );
         if (attr == DW_AT_type && ctsSzB > 0) {
            type->Ty.Qual.typeR = (Type*)(UWord)cts;
            have_ty++;
         }
      }
      /* gcc sometimes generates DW_TAG_const/volatile_type without
         DW_AT_type and GDB appears to interpret the type as 'const
         void' (resp. 'volatile void').  So just allow it .. */
      if (have_ty == 1 || have_ty == 0)
         goto acquire_Type;
      else
         goto bad_DIE;
   }

   /* else ignore this DIE */
   return;
   /*NOTREACHED*/

  acquire_Type:
   if (0) VG_(printf)("YYYY Acquire Type\n");
   vg_assert(type); vg_assert(!atom); vg_assert(!field);
   vg_assert(!expr); vg_assert(!bounds);
   *admin            = ML_(new_TyAdmin)( posn, *admin );
   (*admin)->payload = type;
   (*admin)->tag     = TyA_Type;
   return;
   /*NOTREACHED*/

  acquire_Atom:
   if (0) VG_(printf)("YYYY Acquire Atom\n");
   vg_assert(!type); vg_assert(atom); vg_assert(!field);
   vg_assert(!expr); vg_assert(!bounds);
   *admin            = ML_(new_TyAdmin)( posn, *admin );
   (*admin)->payload = atom;
   (*admin)->tag     = TyA_Atom;
   return;
   /*NOTREACHED*/

  acquire_Field_and_Expr:
   /* For union members, Expr should be absent */
   if (0) VG_(printf)("YYYY Acquire Field and Expr\n");
   vg_assert(!type); vg_assert(!atom); vg_assert(field); 
   /*vg_assert(expr);*/ vg_assert(!bounds);
   if (expr) {
      *admin            = ML_(new_TyAdmin)( (UWord)D3_INVALID_CUOFF,
                                            *admin );
      (*admin)->payload = expr;
      (*admin)->tag     = TyA_Expr;
   }
   *admin            = ML_(new_TyAdmin)( posn, *admin );
   (*admin)->payload = field;
   (*admin)->tag     = TyA_Field;
   return;
   /*NOTREACHED*/

  acquire_Bounds:
   if (0) VG_(printf)("YYYY Acquire Bounds\n");
   vg_assert(!type); vg_assert(!atom); vg_assert(!field);
   vg_assert(!expr); vg_assert(bounds);
   *admin            = ML_(new_TyAdmin)( posn, *admin );
   (*admin)->payload = bounds;
   (*admin)->tag     = TyA_Bounds;
   return;
   /*NOTREACHED*/

  bad_DIE:
   set_position_of_Cursor( c_die,  saved_die_c_offset );
   set_position_of_Cursor( c_abbv, saved_abbv_c_offset );
   VG_(printf)("\nparse_type_DIE: confused by:\n");
   VG_(printf)(" <%d><%lx>: %s\n", level, posn, ML_(pp_DW_TAG)( dtag ) );
   while (True) {
      DW_AT   attr = (DW_AT)  get_ULEB128( c_abbv );
      DW_FORM form = (DW_FORM)get_ULEB128( c_abbv );
      if (attr == 0 && form == 0) break;
      VG_(printf)("     %18s: ", ML_(pp_DW_AT)(attr));
      /* Get the form contents, so as to print them */
      get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                         cc, c_die, True, form );
      VG_(printf)("\t\n");
   }
   VG_(printf)("\n");
   cc->barf("parse_type_DIE: confused by the above DIE");
   /*NOTREACHED*/
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Resolution of references to type DIEs                ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

static Int cmp_D3TyAdmin_by_cuOff ( void* v1, void* v2 ) {
   TyAdmin* a1 = *(TyAdmin**)v1;
   TyAdmin* a2 = *(TyAdmin**)v2;
   if (a1->cuOff < a2->cuOff) return -1;
   if (a1->cuOff > a2->cuOff) return 1;
   return 0;
}

/* Look up 'cuOff' in 'map', to find the associated D3TyAdmin*.  Check
   that the found D3TyAdmin has tag 'adtag'.  Sets *payload to be the
   resulting payload pointer and returns True on success.

   Also, if 'allow_invalid' is True, then if cuOff is
   D3_INVALID_CUOFF, return NULL in *payload.

   Otherwise (conceptually fails) and returns False. */
__attribute__((noinline))
static Bool resolve_binding ( /*OUT*/void** payload,
                              XArray* map, void* cuOff,
                              TyAdminTag tag, 
                              Bool allow_invalid ) {
   Bool    found;
   Word    ixLo, ixHi;
   TyAdmin dummy, *dummyP, *admin;

   if (cuOff == D3_INVALID_CUOFF && allow_invalid) {
      *payload = NULL;
      return True;
   }

   VG_(memset)(&dummy, 0, sizeof(dummy));
   dummy.cuOff = (UWord)cuOff;
   dummyP = &dummy;
   found = VG_(lookupXA)( map, &dummyP, &ixLo, &ixHi );
   if (!found)
      return False;
   /* If this doesn't hold, we must have seen more than one DIE with
      the same cuOff(set).  Which isn't possible. */
   vg_assert(ixLo == ixHi);
   admin = *(TyAdmin**)VG_(indexXA)( map, ixLo );
   /* All payload pointers should be non-NULL.  Ensured by assertion in
      loop in resolve_type_entities that creates 'map'.  Hence it is
      safe to return NULL to indicate 'not found'. */
   vg_assert(admin->payload);
   vg_assert(admin->cuOff == (UWord)cuOff); /* stay sane */

   if (admin->tag != tag)
      return False;

   *payload = admin->payload;
   return True;
}

__attribute__((noinline))
static void resolve_type_entities ( /*MOD*/TyAdmin* admin,
                                    /*MOD*/TempVar* vars )
{
   Bool     ok;
   void*    payload;
   TyAdmin* adp;
   XArray* /* of D3TyAdmin* */ map;

   map = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                     sizeof(TyAdmin*) );
   for (adp = admin; adp; adp = adp->next) {
      vg_assert(adp);
      vg_assert(adp->payload != NULL);
      if (adp->cuOff != (UWord)D3_INVALID_CUOFF) {
         VG_(addToXA)( map, &adp );
      }
   }

   VG_(setCmpFnXA)( map, cmp_D3TyAdmin_by_cuOff );
   if (0) 
      VG_(printf)("XXXXXX sorting map with %d entries\n",
                  (Int)VG_(sizeXA)(map));
   VG_(sortXA)( map );

   for (adp = admin; adp; adp = adp->next) {
      vg_assert(adp->payload);
      switch (adp->tag) {
      case TyA_Bounds: {
         TyBounds* bounds = (TyBounds*)adp->payload;
         if (bounds->knownL && bounds->knownU 
             && bounds->knownL > bounds->knownU) goto baaad;
         break;
      }
      case TyA_Atom: {
         TyAtom* atom = (TyAtom*)adp->payload;
         if (!atom->name) goto baaad;
         break;
      }
      case TyA_Expr: {
         D3Expr* expr = (D3Expr*)adp->payload;
         if (!expr->bytes) goto baaad;
         break;
      }
      case TyA_Field: {
         TyField* field = (TyField*)adp->payload;
         if (!field->name) goto baaad;
         if ( (field->isStruct && (!field->loc)) 
              || ((!field->isStruct) && field->loc))
            goto baaad;
         ok = resolve_binding( &payload, map, field->typeR,
                               TyA_Type, False/*!allow_invalid*/ );
         if (!ok) goto baaad;
         field->typeR = payload;
         break;
      }
      case TyA_Type: {
         UChar   enc;
         XArray* xa;
         Type* ty = (Type*)adp->payload;
         switch (ty->tag) {
            case Ty_Base:
               enc = ty->Ty.Base.enc;
               if ((!ty->Ty.Base.name) 
                   || ty->Ty.Base.szB < 1 || ty->Ty.Base.szB > 32
                   || (enc != 'S' && enc != 'U' && enc != 'F' && enc != 'C'))
                  goto baaad;
               break;
            case Ty_TyDef:
               if (!ty->Ty.TyDef.name) goto baaad;
               ok = resolve_binding( &payload, map,
                                     ty->Ty.TyDef.typeR, 
                                     TyA_Type,
                                     True/*allow_invalid*/ );
               if (!ok) goto baaad;
               ty->Ty.TyDef.typeR = payload;
               break;
            case Ty_PorR:
               if (ty->Ty.PorR.szB != sizeof(Word)) goto baaad;
               ok = resolve_binding( &payload, map,
                                     ty->Ty.PorR.typeR, 
                                     TyA_Type,
                                     False/*!allow_invalid*/ );
               if (!ok) goto baaad;
               ty->Ty.PorR.typeR = payload;
               break;
            case Ty_Array:
               if (!ty->Ty.Array.bounds) goto baaad;
               ok = resolve_binding( &payload, map,
                                     ty->Ty.Array.typeR, 
                                     TyA_Type,
                                     False/*!allow_invalid*/ );
               if (!ok) goto baaad;
               ty->Ty.Array.typeR = payload;
               break;
            case Ty_Enum:
               if ((!ty->Ty.Enum.atomRs)
                   || ty->Ty.Enum.szB < 1 
                   || ty->Ty.Enum.szB > 8) goto baaad;
               xa = ty->Ty.Enum.atomRs;
               break;
            case Ty_StOrUn:
               xa = ty->Ty.StOrUn.fields;
               if (!xa) goto baaad;
               break;
            case Ty_Fn:
               break;
            case Ty_Qual:
               if (ty->Ty.Qual.qual != 'C' 
                   && ty->Ty.Qual.qual != 'V') goto baaad;
               ok = resolve_binding( &payload, map,
                                     ty->Ty.Qual.typeR, 
                                     TyA_Type,
                                     False/*!allow_invalid*/ );
               if (!ok) goto baaad;
               ty->Ty.Qual.typeR = payload;
               break;
            case Ty_Void:
               if (ty->Ty.Void.isFake != False 
                   && ty->Ty.Void.isFake != True) goto baaad;
               break;
            default:
               goto baaad;
         }
         break;
      }
      baaad:
      default:
         VG_(printf)("valgrind: bad D3TyAdmin: ");
         ML_(pp_TyAdmin)(adp);
         VG_(printf)("\n");
      }
   }

   /* Now resolve the variables list */
   for (; vars; vars = vars->next) {
      payload = NULL;
      ok = resolve_binding( &payload, map, vars->typeR,
                            TyA_Type, True/*allow_invalid*/ );

      if (0 && !ok)
         VG_(printf)("Can't resolve type reference 0x%lx\n",
                     (UWord)vars->typeR);
      //vg_assert(ok);
      vars->typeR = payload;
   }

   VG_(deleteXA)( map );
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- Parsing of Compilation Units                         ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

static Int cmp_TempVar_by_dioff ( void* v1, void* v2 ) {
   TempVar* t1 = *(TempVar**)v1;
   TempVar* t2 = *(TempVar**)v2;
   if (t1->dioff < t2->dioff) return -1;
   if (t1->dioff > t2->dioff) return 1;
   return 0;
}

static void read_DIE ( /*OUT*/TyAdmin** admin,
                       /*OUT*/TempVar** tempvars,
                       /*OUT*/GExpr** gexprs,
                       /*MOD*/D3TypeParser* typarser,
                       /*MOD*/D3VarParser* varparser,
                       Cursor* c, Bool td3, CUConst* cc, Int level )
{
   Cursor abbv;
   ULong  atag, abbv_code;
   UWord  posn;
   UInt   has_children;
   UWord  start_die_c_offset, start_abbv_c_offset;
   UWord  after_die_c_offset, after_abbv_c_offset;

   /* --- Deal with this DIE --- */
   posn      = get_position_of_Cursor( c );
   abbv_code = get_ULEB128( c );
   set_abbv_Cursor( &abbv, td3, cc, abbv_code );
   atag      = get_ULEB128( &abbv );
   TRACE_D3("\n");
   TRACE_D3(" <%d><%lx>: Abbrev Number: %llu (%s)\n",
            level, posn, abbv_code, ML_(pp_DW_TAG)( atag ) );

   if (atag == 0)
      cc->barf("read_DIE: invalid zero tag on DIE");

   has_children = get_UChar( &abbv );
   if (has_children != DW_children_no && has_children != DW_children_yes)
      cc->barf("read_DIE: invalid has_children value");

   /* We're set up to look at the fields of this DIE.  Hand it off to
      any parser(s) that want to see it.  Since they will in general
      advance both the DIE and abbrev cursors, remember their current
      settings so that we can then back up and do one final pass over
      the DIE, to print out its contents. */

   start_die_c_offset  = get_position_of_Cursor( c );
   start_abbv_c_offset = get_position_of_Cursor( &abbv );

   while (True) {
      ULong cts;
      Int   ctsSzB;
      UWord ctsMemSzB;
      ULong at_name = get_ULEB128( &abbv );
      ULong at_form = get_ULEB128( &abbv );
      if (at_name == 0 && at_form == 0) break;
      TRACE_D3("     %18s: ", ML_(pp_DW_AT)(at_name));
      /* Get the form contents, but ignore them; the only purpose is
         to print them, if td3 is True */
      get_Form_contents( &cts, &ctsSzB, &ctsMemSzB,
                         cc, c, td3, (DW_FORM)at_form );
      TRACE_D3("\t");
      TRACE_D3("\n");
   }

   after_die_c_offset  = get_position_of_Cursor( c );
   after_abbv_c_offset = get_position_of_Cursor( &abbv );

   set_position_of_Cursor( c,     start_die_c_offset );
   set_position_of_Cursor( &abbv, start_abbv_c_offset );

   parse_type_DIE( admin,
                   typarser,
                   (DW_TAG)atag,
                   posn,
                   level,
                   c,     /* DIE cursor */
                   &abbv, /* abbrev cursor */
                   cc,
                   td3 );

   set_position_of_Cursor( c,     start_die_c_offset );
   set_position_of_Cursor( &abbv, start_abbv_c_offset );

   parse_var_DIE( tempvars,
                  gexprs,
                  varparser,
                  (DW_TAG)atag,
                  posn,
                  level,
                  c,     /* DIE cursor */
                  &abbv, /* abbrev cursor */
                  cc,
                  td3 );

   set_position_of_Cursor( c,     after_die_c_offset );
   set_position_of_Cursor( &abbv, after_abbv_c_offset );

   /* --- Now recurse into its children, if any --- */
   if (has_children == DW_children_yes) {
      if (0) TRACE_D3("BEGIN children of level %d\n", level);
      while (True) {
         atag = peek_ULEB128( c );
         if (atag == 0) break;
         read_DIE( admin, tempvars, gexprs, typarser, varparser,
                   c, td3, cc, level+1 );
      }
      /* Now we need to eat the terminating zero */
      atag = get_ULEB128( c );
      vg_assert(atag == 0);
      if (0) TRACE_D3("END children of level %d\n", level);
   }

}


static
void new_dwarf3_reader_wrk ( 
   struct _DebugInfo* di,
   __attribute__((noreturn))
   void (*barf)( HChar* ),
   UChar* debug_info_img,   SizeT debug_info_sz,
   UChar* debug_abbv_img,   SizeT debug_abbv_sz,
   UChar* debug_line_img,   SizeT debug_line_sz,
   UChar* debug_str_img,    SizeT debug_str_sz,
   UChar* debug_ranges_img, SizeT debug_ranges_sz,
   UChar* debug_loc_img,    SizeT debug_loc_sz
)
{
   TyAdmin *admin, *adminp;
   TempVar *tempvars, *varp, *varp2;
   GExpr *gexprs, *gexpr;
   Cursor abbv; /* for showing .debug_abbrev */
   Cursor info; /* primary cursor for parsing .debug_info */
   Cursor ranges; /* for showing .debug_ranges */
   D3TypeParser typarser;
   D3VarParser varparser;
   Addr  dr_base;
   UWord dr_offset;
   Word  i;
   Bool td3 = di->trace_symtab;
   XArray* /* of TempVar* */ dioff_lookup_tab;
   Bool text_biasing_borked;
   KludgeyTextBiaser ktb;

#if 0
   /* This doesn't work properly because it assumes all entries are
      packed end to end, with no holes.  But that doesn't always
      appear to be the case, so it loses sync.  And the D3 spec
      doesn't appear to require a no-hole situation either. */
   /* Display .debug_loc */
   Addr  dl_base;
   UWord dl_offset;
   Cursor loc; /* for showing .debug_loc */
   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("\n------ The contents of .debug_loc ------\n");
   TRACE_SYMTAB("    Offset   Begin    End      Expression\n");
   init_Cursor( &loc, debug_loc_img, 
                debug_loc_sz, 0, barf, 
                "Overrun whilst reading .debug_loc section(1)" );
   dl_base = 0;
   dl_offset = 0;
   while (True) {
      UWord  w1, w2;
      UWord  len;
      if (is_at_end_Cursor( &loc ))
         break;

      /* Read a (host-)word pair.  This is something of a hack since
         the word size to read is really dictated by the ELF file;
         however, we assume we're reading a file with the same
         word-sizeness as the host.  Reasonably enough. */
      w1 = get_UWord( &loc );
      w2 = get_UWord( &loc );

      if (w1 == 0 && w2 == 0) {
         /* end of list.  reset 'base' */
         TRACE_D3("    %08lx <End of list>\n", dl_offset);
         dl_base = 0;
         dl_offset = get_position_of_Cursor( &loc );
         continue;
      }

      if (w1 == -1UL) {
         /* new value for 'base' */
         TRACE_D3("    %08lx %16lx %08lx (base address)\n",
                  dl_offset, w1, w2);
         dl_base = w2;
         continue;
      }

      /* else a location expression follows */
      TRACE_D3("    %08lx %08lx %08lx ",
               dl_offset, w1 + dl_base, w2 + dl_base);
      len = (UWord)get_UShort( &loc );
      while (len > 0) {
         UChar byte = get_UChar( &loc );
         TRACE_D3("%02x", (UInt)byte);
         len--;
      }
      TRACE_SYMTAB("\n");
   }
#endif

   /* Display .debug_ranges */
   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("\n------ The contents of .debug_ranges ------\n");
   TRACE_SYMTAB("    Offset   Begin    End\n");
   init_Cursor( &ranges, debug_ranges_img, 
                debug_ranges_sz, 0, barf, 
                "Overrun whilst reading .debug_ranges section(1)" );
   dr_base = 0;
   dr_offset = 0;
   while (True) {
      UWord  w1, w2;

      if (is_at_end_Cursor( &ranges ))
         break;

      /* Read a (host-)word pair.  This is something of a hack since
         the word size to read is really dictated by the ELF file;
         however, we assume we're reading a file with the same
         word-sizeness as the host.  Reasonably enough. */
      w1 = get_UWord( &ranges );
      w2 = get_UWord( &ranges );

      if (w1 == 0 && w2 == 0) {
         /* end of list.  reset 'base' */
         TRACE_D3("    %08lx <End of list>\n", dr_offset);
         dr_base = 0;
         dr_offset = get_position_of_Cursor( &ranges );
         continue;
      }

      if (w1 == -1UL) {
         /* new value for 'base' */
         TRACE_D3("    %08lx %16lx %08lx (base address)\n",
                  dr_offset, w1, w2);
         dr_base = w2;
         continue;
      }

      /* else a range [w1+base, w2+base) is denoted */
      TRACE_D3("    %08lx %08lx %08lx\n",
               dr_offset, w1 + dr_base, w2 + dr_base);
   }


   /* Display .debug_abbrev */
   init_Cursor( &abbv, debug_abbv_img, debug_abbv_sz, 0, barf, 
                "Overrun whilst reading .debug_abbrev section" );
   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("\n------ The contents of .debug_abbrev ------\n");
   while (True) {
      if (is_at_end_Cursor( &abbv ))
         break;
      /* Read one abbreviation table */
      TRACE_D3("  Number TAG\n");
      while (True) {
         ULong atag;
         UInt  has_children;
         ULong acode = get_ULEB128( &abbv );
         if (acode == 0) break; /* end of the table */
         atag = get_ULEB128( &abbv );
         has_children = get_UChar( &abbv );
         TRACE_D3("   %llu      %s    [%s]\n", 
                  acode, ML_(pp_DW_TAG)(atag),
                         ML_(pp_DW_children)(has_children));
         while (True) {
            ULong at_name = get_ULEB128( &abbv );
            ULong at_form = get_ULEB128( &abbv );
            if (at_name == 0 && at_form == 0) break;
            TRACE_D3("    %18s %s\n", 
                     ML_(pp_DW_AT)(at_name), ML_(pp_DW_FORM)(at_form));
         }
      }
   }
   TRACE_SYMTAB("\n");

   /* Now loop over the Compilation Units listed in the .debug_info
      section (see D3SPEC sec 7.5) paras 1 and 2.  Each compilation
      unit contains a Compilation Unit Header followed by precisely
      one DW_TAG_compile_unit or DW_TAG_partial_unit DIE. */
   init_Cursor( &info, debug_info_img, debug_info_sz, 0, barf,
                "Overrun whilst reading .debug_info section" );

   /* We'll park the harvested type information in here.  Also create
      a fake "void" entry with offset D3_FAKEVOID_CUOFF, so we always
      have at least one type entry to refer to.  D3_FAKEVOID_CUOFF is
      huge and presumably will not occur in any valid DWARF3 file --
      it would need to have a .debug_info section 4GB long for that to
      happen.  These type entries end up in the DebugInfo. */
   admin = NULL;
   { Type* tVoid = ML_(new_Type)();
     tVoid->tag = Ty_Void;
     tVoid->Ty.Void.isFake = True;
     admin = ML_(new_TyAdmin)( (UWord)D3_FAKEVOID_CUOFF, admin );
     admin->payload = tVoid;
     admin->tag     = TyA_Type;
   }

   /* List of variables we're accumulating.  These don't end up in the
      DebugInfo; instead their contents are handed to ML_(addVar) and
      the list elements are then deleted. */
   tempvars = NULL;

   /* List of GExprs we're accumulating.  These wind up in the
      DebugInfo. */
   gexprs = NULL;

   /* We need a D3TypeParser to keep track of partially constructed
      types.  It'll be discarded as soon as we've completed the CU,
      since the resulting information is tipped in to 'admin' as it is
      generated. */
   VG_(memset)( &typarser, 0, sizeof(typarser) );
   typarser.sp = -1;
   typarser.language = '?';

   VG_(memset)( &varparser, 0, sizeof(varparser) );
   varparser.sp = -1;

   TRACE_D3("\n------ Parsing .debug_info section ------\n");
   while (True) {
      UWord   cu_start_offset, cu_offset_now;
      CUConst cc;

      /* It seems icc9 finishes the DIE info before debug_info_sz
         bytes have been used up.  So be flexible, and declare the
         sequence complete if there is not enough remaining bytes to
         hold even the smallest conceivable CU header.  (11 bytes I
         reckon). */
      Word avail = get_remaining_length_Cursor( &info );
      if (avail < 11) {
         if (avail > 0)
            TRACE_D3("new_dwarf3_reader_wrk: warning: "
                     "%ld unused bytes after end of DIEs\n", avail);
         break;
      }

      /* Check the varparser's stack is in a sane state. */
      vg_assert(varparser.sp == -1);
      for (i = 0; i < N_D3_VAR_STACK; i++) {
         vg_assert(varparser.ranges[i] == NULL);
         vg_assert(varparser.level[i] == 0);
      }
      for (i = 0; i < N_D3_TYPE_STACK; i++) {
         vg_assert(typarser.qparent[i] == NULL);
         vg_assert(typarser.qlevel[i] == 0);
      }

      cu_start_offset = get_position_of_Cursor( &info );
      TRACE_D3("\n");
      TRACE_D3("  Compilation Unit @ offset 0x%lx:\n", cu_start_offset);
      /* parse_CU_header initialises the CU's set_abbv_Cursor cache
         (saC_cache) */
      parse_CU_Header( &cc, td3, &info,
                       (UChar*)debug_abbv_img, debug_abbv_sz );
      cc.debug_str_img    = debug_str_img;
      cc.debug_str_sz     = debug_str_sz;
      cc.debug_ranges_img = debug_ranges_img;
      cc.debug_ranges_sz  = debug_ranges_sz;
      cc.debug_loc_img    = debug_loc_img;
      cc.debug_loc_sz     = debug_loc_sz;
      cc.debug_line_img   = debug_line_img;
      cc.debug_line_sz    = debug_line_sz;
      cc.cu_start_offset  = cu_start_offset;
      cc.di = di;
      /* The CU's svma can be deduced by looking at the AT_low_pc
         value in the top level TAG_compile_unit, which is the topmost
         DIE.  We'll leave it for the 'varparser' to acquire that info
         and fill it in -- since it is the only party to want to know
         it. */
      cc.cu_svma_known = False;
      cc.cu_svma       = 0;

      /* Create a fake outermost-level range covering the entire
         address range.  So we always have *something* to catch all
         variable declarations. */
      varstack_push( &cc, &varparser, td3, 
                     unitary_range_list(0UL, ~0UL),
                     -1, False/*isFunc*/, NULL/*fbGX*/ );

      /* And set up the file name table.  When we come across the top
         level DIE for this CU (which is what the next call to
         read_DIE should process) we will copy all the file names out
         of the .debug_line img area and use this table to look up the
         copies when we later see filename numbers in DW_TAG_variables
         etc. */
      vg_assert(!varparser.filenameTable );
      varparser.filenameTable 
         = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free),
                       sizeof(UChar*) );
      vg_assert(varparser.filenameTable );

      /* Now read the one-and-only top-level DIE for this CU. */
      vg_assert(varparser.sp == 0);
      read_DIE( &admin, &tempvars, &gexprs, &typarser, &varparser,
                &info, td3, &cc, 0 );

      cu_offset_now = get_position_of_Cursor( &info );
      if (1) TRACE_D3("offset now %ld, d-i-size %ld\n",
                      cu_offset_now, debug_info_sz);
      if (cu_offset_now > debug_info_sz)
         barf("toplevel DIEs beyond end of CU");
      if (cu_offset_now == debug_info_sz)
         break;

      /* Preen to level -2.  DIEs have level >= 0 so -2 cannot occur
         anywhere else at all.  Our fake the-entire-address-space
         range is at level -1, so preening to -2 should completely
         empty the stack out. */
      TRACE_D3("\n");
      varstack_preen( &varparser, td3, -2 );
      /* Similarly, empty the type stack out. */
      typestack_preen( &typarser, td3, -2 );
      /* else keep going */

      TRACE_D3("set_abbv_Cursor cache: %lu queries, %lu misses\n",
               cc.saC_cache_queries, cc.saC_cache_misses);

      vg_assert(varparser.filenameTable );
      VG_(deleteXA)( varparser.filenameTable );
      varparser.filenameTable = NULL;
   }

   /* Put the type entry list the right way round.  Not strictly
      necessary, but makes it easier to read. */
   vg_assert(admin);
   if (admin) { 
      TyAdmin *next, *prev = NULL;
      for (adminp = admin; adminp; adminp = next) {
         next = adminp->next;
         adminp->next = prev;
         prev = adminp;
      }
      admin = prev;
   }

   /* Put the variable list the right way round.  Not strictly
      necessary, but makes it easier to read. */
   if (tempvars) { 
      TempVar *next, *prev = NULL;
      for (varp = tempvars; varp; varp = next) {
         next = varp->next;
         varp->next = prev;
         prev = varp;
      }
      tempvars = prev;
   }

   TRACE_D3("\n");
   TRACE_D3("------ Acquired the following type entities: ------\n");
   for (adminp = admin; adminp; adminp = adminp->next) {
      TRACE_D3("   ");
      if (td3) ML_(pp_TyAdmin)( adminp );
      TRACE_D3("\n");
   }
   TRACE_D3("\n");
   TRACE_D3("------ Resolving type entries ------\n");

   /* See "Comment_Regarding_DWARF3_Text_Biasing" above. */
   VG_(memset)( &ktb, 0, sizeof(ktb ));
   ktb.rx_map_avma = di->rx_map_avma;
   ktb.rx_map_size = di->rx_map_size;
   ktb.text_bias   = di->text_bias;

   resolve_type_entities( admin, tempvars );
   for (gexpr = gexprs; gexpr; gexpr = gexpr->next) {
      bias_GX( gexpr, &ktb );
   }

   TRACE_D3("\n");
   TRACE_D3("------ Acquired the following variables: ------\n\n");

   /* Park (pointers to) all the vars in an XArray, so we can look up
      abstract origins quickly.  The array is sorted (hence, looked-up
      by) the .dioff fields.  Since the .dioffs should be instrictly
      ascending order, there is no need to sort the array after
      construction.  The ascendingness is however asserted for. */
   dioff_lookup_tab
      = VG_(newXA)( ML_(dinfo_zalloc), ML_(dinfo_free), 
                    sizeof(TempVar*) );
   vg_assert(dioff_lookup_tab);
   varp2 = NULL;
   for (varp = tempvars; varp; varp = varp->next) {
      if (varp2)
         vg_assert(varp2->dioff < varp->dioff);
      VG_(addToXA)( dioff_lookup_tab, &varp );
      varp2 = varp;
   }
   VG_(setCmpFnXA)( dioff_lookup_tab, cmp_TempVar_by_dioff );
   VG_(sortXA)( dioff_lookup_tab ); /* POINTLESS; FIXME: rm */

   /* Now visit each var.  Collect up as much info as possible for
      each var and hand it to ML_(addVar). */
   for (varp = tempvars; varp; varp = varp->next) {

      /* Possibly show .. */
      if (td3) {
         VG_(printf)("<%lx> addVar: level %d: %s :: ",
                     varp->dioff,
                     varp->level,
                     varp->name ? varp->name : (UChar*)"<anon_var>" );
         if (varp->typeR) {
            ML_(pp_Type_C_ishly)( varp->typeR );
         } else {
            VG_(printf)("NULL");
         }
         VG_(printf)("\n  Loc=");
         if (varp->gexpr) {
            ML_(pp_GX)(varp->gexpr);
         } else {
            VG_(printf)("NULL");
         }
         VG_(printf)("\n");
         if (varp->fbGX) {
            VG_(printf)("  FrB=");
            ML_(pp_GX)( varp->fbGX );
            VG_(printf)("\n");
         } else {
            VG_(printf)("  FrB=none\n");
         }
         VG_(printf)("  declared at: %s:%d\n",
                     varp->fName ? varp->fName : (UChar*)"NULL",
                     varp->fLine );
         if (varp->absOri != (UWord)D3_INVALID_CUOFF)
            VG_(printf)("  abstract origin: <%lx>\n", varp->absOri);
      }

      /* Skip variables which have no location.  These must be
         abstract instances; they are useless as-is since with no
         location they have no specified memory location.  They will
         presumably be referred to via the absOri fields of other
         variables. */
      if (!varp->gexpr) {
         TRACE_D3("  SKIP (no location)\n\n");
         continue;
      }

      /* So it has a location, at least.  If it refers to some other
         entry through its absOri field, pull in further info through
         that. */
      if (varp->absOri != (UWord)D3_INVALID_CUOFF) {
         Bool found;
         Word ixFirst, ixLast;
         TempVar key;
         TempVar* keyp = &key;
         TempVar *varAI;
         VG_(memset)(&key, 0, sizeof(key)); /* not necessary */
         key.dioff = varp->absOri; /* this is what we want to find */
         found = VG_(lookupXA)( dioff_lookup_tab, &keyp,
                                &ixFirst, &ixLast );
         if (!found)
            barf("DW_AT_abstract_origin can't be resolved");
         /* If the following fails, there is more than one entry with
            the same dioff.  Which can't happen. */
         vg_assert(ixFirst == ixLast);
         varAI = *(TempVar**)VG_(indexXA)( dioff_lookup_tab, ixFirst );
         /* stay sane */
         vg_assert(varAI);
         vg_assert(varAI->dioff == varp->absOri);

         /* Copy what useful info we can. */
         if (varAI->typeR && !varp->typeR)
            varp->typeR = varAI->typeR;
         if (varAI->name && !varp->name)
            varp->name = varAI->name;
         if (varAI->fName && !varp->fName)
            varp->fName = varAI->fName;
         if (varAI->fLine > 0 && varp->fLine == 0)
            varp->fLine = varAI->fLine;
      }

      /* Give it a name if it doesn't have one. */
      if (!varp->name)
         varp->name = ML_(addStr)( di, "<anon_var>", -1 );

      /* So now does it have enough info to be useful? */
      /* NOTE: re typeR: this is a hack.  If typeR is NULL then the
         type didn't get resolved.  Really, in that case something's
         broken earlier on, and should be fixed, rather than just
         skipping the variable. */
      if (!varp->typeR) continue;
      vg_assert(varp->gexpr);
      vg_assert(varp->name);
      vg_assert(varp->typeR);
      vg_assert(varp->level >= 0);

      /* Ok.  So we're going to keep it.  Call ML_(addVar) once for
         each address range in which the variable exists. */
      TRACE_D3("  ACQUIRE for range(s) ");
      { AddrRange  oneRange;
        AddrRange* varPcRanges;
        Word       nVarPcRanges;
        /* Set up to iterate over address ranges, however
           represented. */
        if (varp->nRanges == 0 || varp->nRanges == 1) {
           vg_assert(!varp->rngMany);
           if (varp->nRanges == 0) {
              vg_assert(varp->rngOneMin == 0);
              vg_assert(varp->rngOneMax == 0);
           }
           nVarPcRanges = varp->nRanges;
           oneRange.aMin = varp->rngOneMin;
           oneRange.aMax = varp->rngOneMax;
           varPcRanges = &oneRange;
        } else {
           vg_assert(varp->rngMany);
           vg_assert(varp->rngOneMin == 0);
           vg_assert(varp->rngOneMax == 0);
           nVarPcRanges = VG_(sizeXA)(varp->rngMany);
           vg_assert(nVarPcRanges >= 2);
           vg_assert(nVarPcRanges == (Word)varp->nRanges);
           varPcRanges = VG_(indexXA)(varp->rngMany, 0);
        }
        if (varp->level == 0)
           vg_assert( nVarPcRanges == 1 );
        /* and iterate */
        for (i = 0; i < nVarPcRanges; i++) {
           Addr pcMin = varPcRanges[i].aMin;
           Addr pcMax = varPcRanges[i].aMax;
           vg_assert(pcMin <= pcMax);
           /* Level 0 is the global address range.  So at level 0 we
              don't want to bias pcMin/pcMax; but at all other levels
              we do since those are derived from svmas in the Dwarf
              we're reading.  Be paranoid ... */
           if (varp->level == 0) {
              vg_assert(pcMin == (Addr)0);
              vg_assert(pcMax == ~(Addr)0);
           } else {
              /* vg_assert(pcMin > (Addr)0);
                 No .. we can legitimately expect to see ranges like 
                 0x0-0x11D (pre-biasing, of course). */
              vg_assert(pcMax < ~(Addr)0);
           }

           /* Apply text biasing, for non-global variables. */
           if (varp->level > 0) {
              pcMin = apply_kludgey_text_bias( &ktb, pcMin );
              pcMax = apply_kludgey_text_bias( &ktb, pcMax );
           } 

           if (i > 0 && (i%2) == 0) 
              TRACE_D3("\n                       ");
           TRACE_D3("[%p,%p] ", pcMin, pcMax );

           ML_(addVar)(
              di, varp->level, 
                  pcMin, pcMax,
                  varp->name, (void*)varp->typeR,
                  varp->gexpr, varp->fbGX,
                  varp->fName, varp->fLine, td3 
           );
        }
      }

      TRACE_D3("\n\n");
      /* and move on to the next var */
   }

   /* For the text biasing to work out, we expect that:
      - there were no failures, and
      - either all were done straightforwardly, or all kludgily,
        but not with a mixture
   */ 
   text_biasing_borked 
      = ktb.n_failed_biasings > 0 
        || (ktb.n_straightforward_biasings > 0 && ktb.n_kludgey_biasings > 0);

   if (td3 || text_biasing_borked) {
      VG_(printf)("TEXT SVMA BIASING STATISTICS:\n");
      VG_(printf)("   straightforward biasings: %lu\n",
                  ktb.n_straightforward_biasings );
      VG_(printf)("           kludgey biasings: %lu\n",
                  ktb.n_kludgey_biasings );
      VG_(printf)("            failed biasings: %lu\n\n",
                  ktb.n_failed_biasings );
   }
   if (text_biasing_borked)
      barf("couldn't make sense of DWARF3 text-svma biasing; details above");

   /* Now free all the TempVars */
   for (varp = tempvars; varp; varp = varp2) {
      varp2 = varp->next;
      if (varp->rngMany)
         VG_(deleteXA)(varp->rngMany);
      ML_(dinfo_free)(varp);
   }
   tempvars = NULL;

   /* And get rid of the temporary mapping table. */
   VG_(deleteXA)( dioff_lookup_tab );

   /* record the TyAdmins and the GExprs in di so they can be freed
      later */
   vg_assert(!di->admin_tyadmins);
   di->admin_tyadmins = admin;
   vg_assert(!di->admin_gexprs);
   di->admin_gexprs = gexprs;
}


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The "new" DWARF3 reader -- top level control logic   ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <setjmp.h>   /* For jmp_buf */
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */

static Bool    d3rd_jmpbuf_valid  = False;
static HChar*  d3rd_jmpbuf_reason = NULL;
static jmp_buf d3rd_jmpbuf;

static __attribute__((noreturn)) void barf ( HChar* reason ) {
   vg_assert(d3rd_jmpbuf_valid);
   d3rd_jmpbuf_reason = reason;
   __builtin_longjmp(&d3rd_jmpbuf, 1);
   /*NOTREACHED*/
   vg_assert(0);
}


void 
ML_(new_dwarf3_reader) (
   struct _DebugInfo* di,
   UChar* debug_info_img,   SizeT debug_info_sz,
   UChar* debug_abbv_img,   SizeT debug_abbv_sz,
   UChar* debug_line_img,   SizeT debug_line_sz,
   UChar* debug_str_img,    SizeT debug_str_sz,
   UChar* debug_ranges_img, SizeT debug_ranges_sz,
   UChar* debug_loc_img,    SizeT debug_loc_sz
)
{
   volatile Int  jumped;
   volatile Bool td3 = di->trace_symtab;

   /* Run the _wrk function to read the dwarf3.  If it succeeds, it
      just returns normally.  If there is any failure, it longjmp's
      back here, having first set d3rd_jmpbuf_reason to something
      useful. */
   vg_assert(d3rd_jmpbuf_valid  == False);
   vg_assert(d3rd_jmpbuf_reason == NULL);

   d3rd_jmpbuf_valid = True;
   jumped = __builtin_setjmp(&d3rd_jmpbuf);
   if (jumped == 0) {
      /* try this ... */
      new_dwarf3_reader_wrk( di, barf,
                             debug_info_img,   debug_info_sz,
                             debug_abbv_img,   debug_abbv_sz,
                             debug_line_img,   debug_line_sz,
                             debug_str_img,    debug_str_sz,
                             debug_ranges_img, debug_ranges_sz,
                             debug_loc_img,    debug_loc_sz );
      d3rd_jmpbuf_valid = False;
      TRACE_D3("\n------ .debug_info reading was successful ------\n");
   } else {
      /* It longjmp'd. */
      d3rd_jmpbuf_valid = False;
      /* Can't longjump without giving some sort of reason. */
      vg_assert(d3rd_jmpbuf_reason != NULL);

      TRACE_D3("\n------ .debug_info reading failed ------\n");

      ML_(symerr)(di, True, d3rd_jmpbuf_reason);
   }

   d3rd_jmpbuf_valid  = False;
   d3rd_jmpbuf_reason = NULL;
}



/* --- Unused code fragments which might be useful one day. --- */

#if 0
   /* Read the arange tables */
   TRACE_SYMTAB("\n");
   TRACE_SYMTAB("\n------ The contents of .debug_arange ------\n");
   init_Cursor( &aranges, debug_aranges_img, 
                debug_aranges_sz, 0, barf, 
                "Overrun whilst reading .debug_aranges section" );
   while (True) {
      ULong  len, d_i_offset;
      Bool   is64;
      UShort version;
      UChar  asize, segsize;

      if (is_at_end_Cursor( &aranges ))
         break;
      /* Read one arange thingy */
      /* initial_length field */
      len = get_Initial_Length( &is64, &aranges, 
               "in .debug_aranges: invalid initial-length field" );
      version    = get_UShort( &aranges );
      d_i_offset = get_Dwarfish_UWord( &aranges, is64 );
      asize      = get_UChar( &aranges );
      segsize    = get_UChar( &aranges );
      TRACE_D3("  Length:                   %llu\n", len);
      TRACE_D3("  Version:                  %d\n", (Int)version);
      TRACE_D3("  Offset into .debug_info:  %llx\n", d_i_offset);
      TRACE_D3("  Pointer Size:             %d\n", (Int)asize);
      TRACE_D3("  Segment Size:             %d\n", (Int)segsize);
      TRACE_D3("\n");
      TRACE_D3("    Address            Length\n");

      while ((get_position_of_Cursor( &aranges ) % (2 * asize)) > 0) {
         (void)get_UChar( & aranges );
      }
      while (True) {
         ULong address = get_Dwarfish_UWord( &aranges, asize==8 );
         ULong length = get_Dwarfish_UWord( &aranges, asize==8 );
         TRACE_D3("    0x%016llx 0x%llx\n", address, length);
         if (address == 0 && length == 0) break;
      }
   }
   TRACE_SYMTAB("\n");
#endif

/*--------------------------------------------------------------------*/
/*--- end                                             readdwarf3.c ---*/
/*--------------------------------------------------------------------*/
